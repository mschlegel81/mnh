UNIT valueStore;
INTERFACE
USES sysutils,
     mySys,
     basicTypes,
     mnh_constants,
     mnh_messages,
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
      CONSTRUCTOR create(CONST initialId:T_idString; CONST initialValue:P_literal; CONST isReadOnly:boolean);
      DESTRUCTOR destroy;
      PROCEDURE setValue(CONST newValue:P_literal);
      FUNCTION mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
      FUNCTION getId:T_idString;
      FUNCTION getValue:P_literal;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):ansistring;
      FUNCTION clone:P_namedVariable;
  end;

CONST
  ACCESS_BLOCKED  =0;
  ACCESS_READONLY =1;
  ACCESS_READWRITE=2;

TYPE
  AccessLevel=0..2;

  P_valueScope=^T_valueScope;
  T_valueScope=object
      parentScope :P_valueScope;
      parentAccess:AccessLevel;
      refCount    :longint;
      variables   :array of P_namedVariable;
      varFill     :longint;
      CONSTRUCTOR create(CONST asChildOf:P_valueScope; CONST accessToParent:AccessLevel);
      PROCEDURE insteadOfCreate(CONST asChildOf:P_valueScope; CONST accessToParent:AccessLevel);
      DESTRUCTOR destroy;
      PROCEDURE insteadOfDestroy;

      PROCEDURE createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean=false);
      PROCEDURE createVariable(CONST id:T_idString; CONST value:int64    ; CONST readonly:boolean=true);
      FUNCTION  getVariableValue(CONST id: T_idString): P_literal;
      FUNCTION  setVariableValue(CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext):boolean;
      FUNCTION  mutateVariableValue(CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
      {$ifdef fullVersion}
      //For debugging:
      PROCEDURE reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
      {$endif}
  end;

IMPLEMENTATION
CONSTRUCTOR T_namedVariable.create(CONST initialId:T_idString; CONST initialValue:P_literal; CONST isReadOnly:boolean);
  begin
    id:=initialId;
    value:=initialValue;
    readonly:=isReadOnly;
    if value<>nil then value^.rereference;
  end;

DESTRUCTOR T_namedVariable.destroy;
  begin
    if value<>nil then disposeLiteral(value);
  end;

PROCEDURE T_namedVariable.setValue(CONST newValue:P_literal);
  begin
    if readonly then raise Exception.create('Mutation of constant "'+id+'" is not allowed.');
    disposeLiteral(value);
    value:=newValue;
    value^.rereference;
  end;

FUNCTION T_namedVariable.mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
  begin
    if readonly then begin
      context^.raiseError('Mutation of constant "'+id+'" is not allowed.',location);
      exit(newVoidLiteral);
    end;
    result:=mutateVariable(value,mutation,RHS,location,context,recycler);
    if result=nil then result:=newVoidLiteral;
  end;

FUNCTION T_namedVariable.getId:T_idString;
  begin
    result:=id;
  end;

FUNCTION T_namedVariable.getValue:P_literal;
  begin
    result:=value;
    result^.rereference;
  end;

FUNCTION T_namedVariable.toString(CONST lengthLimit: longint): ansistring;
  begin
    result:=id+'='+value^.toString(lengthLimit-1-length(id));
  end;

FUNCTION T_namedVariable.clone: P_namedVariable;
  begin
    new(result,create(id,value,readonly));
  end;

CONSTRUCTOR T_valueScope.create(CONST asChildOf:P_valueScope; CONST accessToParent:AccessLevel);
  begin
    if asChildOf<>nil then begin
      parentScope:=asChildOf;
      interLockedIncrement(parentScope^.refCount);
    end else parentScope:=nil;
    if parentScope=nil
    then parentAccess:=ACCESS_BLOCKED
    else parentAccess:=accessToParent;
    refCount        :=1;
    setLength(variables,0);
    varFill:=0;
  end;

PROCEDURE T_valueScope.insteadOfCreate(CONST asChildOf:P_valueScope; CONST accessToParent:AccessLevel);
  begin
    if asChildOf<>nil then begin
      parentScope:=asChildOf;
      interLockedIncrement(parentScope^.refCount);
    end else parentScope:=nil;
    if parentScope=nil
    then parentAccess:=ACCESS_BLOCKED
    else parentAccess:=accessToParent;
    refCount        :=1;
  end;

DESTRUCTOR T_valueScope.destroy;
  VAR k:longint;
  begin
    for k:=0 to varFill-1 do dispose(variables[k],destroy);
    varFill:=0;
    setLength(variables,0);
  end;

PROCEDURE T_valueScope.insteadOfDestroy;
  VAR k:longint;
  begin
    for k:=0 to varFill-1 do dispose(variables[k],destroy);
    varFill:=0;
  end;

PROCEDURE T_valueScope.createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean=false);
  begin
    if varFill>=length(variables) then setLength(variables,varFill+1);
    new(variables[varFill],create(id,value,readonly));
    inc(varFill);
  end;

PROCEDURE T_valueScope.createVariable(CONST id:T_idString; CONST value:int64;     CONST readonly:boolean=true);
  VAR lit:P_abstractIntLiteral;
  begin
    lit:=newIntLiteral(value);
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

FUNCTION T_valueScope.setVariableValue(CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext):boolean;
  VAR k:longint;
  begin
    result:=false;
    for k:=0 to varFill-1 do if variables[k]^.id=id then begin
      variables[k]^.setValue(value);
      exit(true);
    end;
    if (parentAccess>=ACCESS_READWRITE)
    then parentScope^.setVariableValue(id,value,location,context)
    else context^.raiseError('Cannot assign value to unknown local variable '+id,location);
  end;

FUNCTION T_valueScope.mutateVariableValue(CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
  VAR k:longint;
  begin
    result:=nil;
    for k:=0 to varFill-1 do if variables[k]^.id=id then begin
      result:=variables[k]^.mutate(mutation,RHS,location,context,recycler);
      exit(result);
    end;
    if (parentAccess>=ACCESS_READWRITE)
    then result:=parentScope^.mutateVariableValue(id,mutation,RHS,location,context,recycler)
    else context^.raiseError('Cannot assign value to unknown local variable '+id,location);
  end;

{$ifdef fullVersion}
PROCEDURE T_valueScope.reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
  VAR k:longint;
  begin
    if parentAccess>=ACCESS_READONLY then parentScope^.reportVariables(variableReport);
    for k:=0 to varFill-1 do variableReport.addEntry(variables[k]^.id,variables[k]^.value,false);
  end;
{$endif}

end.

