UNIT valueStore;
INTERFACE
USES sysutils,
     basicTypes,
     mnh_constants,
     out_adapters,
     litVar
     {$ifdef fullVersion},
     debuggingVar
     {$endif};
TYPE
  P_namedVariable=^T_namedVariable;
  T_namedVariable=object
    private
      id:T_idString;
      value:P_literal;
      readonly:boolean;
    public
      varCs:TRTLCriticalSection;
      CONSTRUCTOR create(CONST initialId:T_idString; CONST initialValue:P_literal; CONST isReadOnly:boolean);
      PROCEDURE cleanup(VAR literalRecycler:T_literalRecycler);
      DESTRUCTOR destroy;
      PROCEDURE setValue(VAR literalRecycler:T_literalRecycler; CONST newValue:P_literal);
      FUNCTION mutate(VAR literalRecycler:T_literalRecycler;CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
      FUNCTION getId:T_idString;
      FUNCTION getValue:P_literal;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):ansistring;
      FUNCTION clone:P_namedVariable;
  end;

  P_valueScope=^T_valueScope;
  T_valueScope=object
    parentScope :P_valueScope;
    refCount    :longint;
    variables   :array of P_namedVariable;
    varFill     :longint;
    CONSTRUCTOR create(CONST asChildOf:P_valueScope);
    PROCEDURE insteadOfCreate(CONST asChildOf:P_valueScope);
    PROCEDURE cleanup(VAR literalRecycler:T_literalRecycler);
    DESTRUCTOR destroy;
    PROCEDURE insteadOfDestroy;

    PROCEDURE createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean=false);
    PROCEDURE createVariable(VAR literalRecycler:T_literalRecycler; CONST id:T_idString; CONST value:int64    ; CONST readonly:boolean=true);
    FUNCTION  getVariableValue(CONST id: T_idString): P_literal;
    FUNCTION  setVariableValue(VAR literalRecycler:T_literalRecycler; CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext):boolean;
    FUNCTION  mutateVariableValue(VAR literalRecycler:T_literalRecycler; CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
    {$ifdef fullVersion}
    //For debugging:
    PROCEDURE reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
    {$endif}
    PROCEDURE attachParent(CONST parent:P_valueScope);
    PROCEDURE detachParent;
    FUNCTION checkVariablesOnPop(VAR literalRecycler:T_literalRecycler; CONST location:T_searchTokenLocation; CONST context:P_abstractContext):boolean;
  end;

IMPLEMENTATION
USES mnh_messages,myGenerics;
CONSTRUCTOR T_namedVariable.create(CONST initialId:T_idString; CONST initialValue:P_literal; CONST isReadOnly:boolean);
  begin
    initCriticalSection(varCs);
    id:=initialId;
    value:=initialValue;
    readonly:=isReadOnly;
    if value<>nil then value^.rereference;
  end;

PROCEDURE T_namedVariable.cleanup(VAR literalRecycler:T_literalRecycler);
  begin
    enterCriticalSection(varCs);
    if value<>nil then literalRecycler.disposeLiteral(value);
    leaveCriticalSection(varCs);
  end;

DESTRUCTOR T_namedVariable.destroy;
  begin
    assert(value=nil);
    doneCriticalSection(varCs);
  end;

PROCEDURE T_namedVariable.setValue(VAR literalRecycler:T_literalRecycler; CONST newValue:P_literal);
  begin
    if readonly then raise Exception.create('Mutation of constant "'+id+'" is not allowed.');
    enterCriticalSection(varCs);
    try
      literalRecycler.disposeLiteral(value);
      value:=newValue;
      value^.rereference;
    finally
      leaveCriticalSection(varCs);
    end;
  end;

FUNCTION T_namedVariable.mutate(VAR literalRecycler:T_literalRecycler; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
  begin
    if readonly then begin
      context^.raiseError('Mutation of constant "'+id+'" is not allowed.',location);
      exit(newVoidLiteral);
    end;
    enterCriticalSection(varCs);
    try
      result:=mutateVariable(literalRecycler,value,mutation,RHS,location,context,recycler);
    finally
      leaveCriticalSection(varCs);
    end;
    if result=nil then result:=newVoidLiteral;
  end;

FUNCTION T_namedVariable.getId:T_idString;
  begin
    result:=id;
  end;

FUNCTION T_namedVariable.getValue:P_literal;
  begin
    enterCriticalSection(varCs);
    try
      result:=value;
      result^.rereference;
    finally
      leaveCriticalSection(varCs);
    end;
  end;

FUNCTION T_namedVariable.toString(CONST lengthLimit: longint): ansistring;
  begin
    enterCriticalSection(varCs);
    try
      result:=id+'='+value^.toString(lengthLimit-1-length(id));
    finally
      leaveCriticalSection(varCs);
    end;
  end;

FUNCTION T_namedVariable.clone: P_namedVariable;
  begin
    enterCriticalSection(varCs);
    try
      new(result,create(id,value,readonly));
    finally
      leaveCriticalSection(varCs);
    end;
  end;

CONSTRUCTOR T_valueScope.create(CONST asChildOf:P_valueScope);
  begin
    if asChildOf<>nil then begin
      parentScope:=asChildOf;
      interLockedIncrement(parentScope^.refCount);
    end else parentScope:=nil;
    refCount        :=1;
    setLength(variables,0);
    varFill:=0;
  end;

PROCEDURE T_valueScope.insteadOfCreate(CONST asChildOf:P_valueScope);
  begin
    if asChildOf<>nil then begin
      parentScope:=asChildOf;
      interLockedIncrement(parentScope^.refCount);
    end else parentScope:=nil;
    refCount        :=1;
  end;

PROCEDURE T_valueScope.cleanup(VAR literalRecycler:T_literalRecycler);
  VAR k:longint;
  begin
    for k:=0 to varFill-1 do begin
      variables[k]^.cleanup(literalRecycler);
      dispose(variables[k],destroy);
    end;
    varFill:=0;
    setLength(variables,0);
  end;

DESTRUCTOR T_valueScope.destroy;
  begin
    assert(length(variables)=0);
    assert(varFill=0);
  end;

PROCEDURE T_valueScope.insteadOfDestroy;
  begin
    assert(varFill=0);
  end;

PROCEDURE T_valueScope.createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean=false);
  begin
    if varFill>=length(variables) then setLength(variables,varFill+1);
    new(variables[varFill],create(id,value,readonly));
    inc(varFill);
  end;

PROCEDURE T_valueScope.createVariable(VAR literalRecycler:T_literalRecycler; CONST id:T_idString; CONST value:int64;     CONST readonly:boolean=true);
  VAR lit:P_abstractIntLiteral;
  begin
    lit:=literalRecycler.newIntLiteral(value);
    createVariable(id,lit,readonly);
    lit^.unreference;
  end;

FUNCTION T_valueScope.getVariableValue(CONST id: T_idString): P_literal;
  VAR k:longint;
  begin
    result:=nil;
    for k:=0 to varFill-1 do if variables[k]^.id=id then begin
      result:=variables[k]^.getValue;
      exit(result);
    end;
    if parentScope<>nil then result:=parentScope^.getVariableValue(id);
  end;

FUNCTION T_valueScope.setVariableValue(VAR literalRecycler:T_literalRecycler; CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext):boolean;
  VAR k:longint;
  begin
    result:=false;
    for k:=0 to varFill-1 do if variables[k]^.id=id then begin
      variables[k]^.setValue(literalRecycler,value);
      exit(true);
    end;
    if parentScope<>nil
    then parentScope^.setVariableValue(literalRecycler,id,value,location,context)
    else context^.raiseError('Cannot assign value to unknown local variable '+id,location);
  end;

FUNCTION T_valueScope.mutateVariableValue(VAR literalRecycler:T_literalRecycler; CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
  VAR k:longint;
  begin
    result:=nil;
    for k:=0 to varFill-1 do if variables[k]^.id=id then begin
      result:=variables[k]^.mutate(literalRecycler,mutation,RHS,location,context,recycler);
      exit(result);
    end;
    if parentScope<>nil
    then result:=parentScope^.mutateVariableValue(literalRecycler,id,mutation,RHS,location,context,recycler)
    else context^.raiseError('Cannot assign value to unknown local variable '+id,location);
  end;

PROCEDURE T_valueScope.attachParent(CONST parent:P_valueScope);
  begin
    assert(parentScope=nil);
    if parent=nil then exit;
    interLockedIncrement(parent^.refCount);
    parentScope:=parent;
  end;

PROCEDURE T_valueScope.detachParent;
  begin
    if parentScope=nil then exit;
    if interlockedDecrement(parentScope^.refCount)=0 then dispose(parentScope,destroy);
    parentScope:=nil;
  end;

FUNCTION T_valueScope.checkVariablesOnPop(VAR literalRecycler:T_literalRecycler; CONST location:T_searchTokenLocation; CONST context:P_abstractContext):boolean;
  VAR i:longint;
  begin
    result:=true;
    for i:=0 to varFill-1 do
      if (variables[i]^.value^.literalType=lt_expression) and
         (P_expressionLiteral(variables[i]^.value)^.mustBeDroppedBeforePop)
      then begin
        result:=false;
        context^.messages^.postTextMessage(mt_el2_warning,location,'Invalid entry in value scope on pop: '+variables[i]^.id+'='+variables[i]^.getValue^.toString(20)+' - set value to void before end!');
        variables[i]^.readonly:=false;
        variables[i]^.setValue(literalRecycler,newVoidLiteral);
      end;
  end;

{$ifdef fullVersion}
PROCEDURE T_valueScope.reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
  VAR k:longint;
  begin
    if parentScope<>nil then parentScope^.reportVariables(variableReport);
    for k:=0 to varFill-1 do variableReport.addEntry(variables[k]^.id,variables[k]^.value,false);
  end;
{$endif}

end.

