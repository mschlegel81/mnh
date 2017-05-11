UNIT valueStore;
INTERFACE
USES sysutils,
     myGenerics,
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_litVar;
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
      FUNCTION mutate(CONST mutation:T_cStyleOperator; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
      FUNCTION getId:T_idString;
      FUNCTION getValue:P_literal;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):ansistring;
      FUNCTION readOnlyClone:P_namedVariable;
  end;

 {$ifdef fullVersion}
  T_variableReport=object
    dat:array of record
          id:ansistring;
          value:P_literal;
          location:string;
        end;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addVariable(CONST id:ansistring; CONST value:P_literal; CONST location:string; CONST retainExistent:boolean=false);
    PROCEDURE addVariable(CONST namedVar:P_namedVariable; CONST location:string);
  end;
  {$endif}

TYPE
  T_scope=record
    blockingScope:boolean;
    v:array of P_namedVariable;
  end;

  P_valueStore=^T_valueStore;
  T_valueStore=object
    private
      cs:TRTLCriticalSection;
      scopeStack:array of T_scope;
      PROCEDURE copyDataAsReadOnly(VAR original:T_valueStore);
      FUNCTION getVariable(CONST id:T_idString; OUT blockEncountered:boolean):P_namedVariable;
    public
      parentStore:P_valueStore;
      CONSTRUCTOR create;
      FUNCTION readOnlyClone:P_valueStore;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      FUNCTION isEmpty:boolean;

      PROCEDURE createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean);
      PROCEDURE createVariable(CONST id:T_idString; CONST value:int64;     CONST readonly:boolean);
      FUNCTION  getVariableValue(CONST id: T_idString): P_literal;
      PROCEDURE setVariableValue(CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters);
      FUNCTION  mutateVariableValue(CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
      PROCEDURE scopePush(CONST blocking:boolean);
      PROCEDURE scopePop;
      {$ifdef fullVersion}
      //For debugging:
      PROCEDURE reportVariables(VAR variableReport:T_variableReport);
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

FUNCTION T_namedVariable.mutate(CONST mutation:T_cStyleOperator; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  CONST MAPPED_OP:array[tt_cso_assignPlus..tt_cso_assignDiv] of T_tokenType=(tt_operatorPlus,tt_operatorMinus,tt_operatorMult,tt_operatorDivReal);
  VAR oldValue:P_literal;
  begin
    if readonly then begin
      adapters.raiseError('Mutation of constant "'+id+'" is not allowed.',location);
      exit(newVoidLiteral);
    end;
    oldValue:=value;
    case mutation of
      tt_cso_assignPlus..tt_cso_assignDiv: begin
        value:=resolveOperator(oldValue, MAPPED_OP[mutation], RHS, location,adapters);
        disposeLiteral(oldValue);
        exit(value^.rereferenced);
      end;
      tt_cso_assignStrConcat: begin
        if (oldValue^.literalType=lt_string) and (oldValue^.getReferenceCount=1) and (RHS^.literalType in [lt_boolean..lt_string]) then begin
          P_stringLiteral(oldValue)^.append(P_scalarLiteral(RHS)^.stringForm);
          exit(oldValue^.rereferenced);
        end else begin
          value:=resolveOperator(oldValue, tt_operatorStrConcat, RHS, location,adapters);
          disposeLiteral(oldValue);
          exit(value^.rereferenced);
        end;
      end;
      tt_cso_assignAppend: begin
        if (oldValue^.literalType in C_setTypes+C_listTypes) and (oldValue^.getReferenceCount=1) then begin
          if (RHS^.literalType in C_scalarTypes)
          then P_collectionLiteral(oldValue)^.append(RHS, true)
          else P_collectionLiteral(oldValue)^.appendAll(P_compoundLiteral(RHS));
          exit(oldValue^.rereferenced);
        end else begin
          value:=resolveOperator(oldValue, tt_operatorConcat   , RHS, location,adapters);
          disposeLiteral(oldValue);
          exit(value^.rereferenced);
        end;
      end;
      tt_cso_mapPut, tt_cso_mapDrop: begin
        if value^.literalType=lt_void then begin
          value:=newMapLiteral;
          disposeLiteral(oldValue);
        end;
        if value^.literalType in C_scalarTypes then begin
          adapters.raiseError('Operators << and >> expect a map variable (local or mutable) on the left-hand-side - cannot cast scalar to map',location);
          exit(newVoidLiteral);
        end;
        if (value^.literalType in C_compoundTypes-C_mapTypes) then begin
          value:=P_compoundLiteral(oldValue)^.toMap(location,adapters);
          disposeLiteral(oldValue);
          if P_compoundLiteral(value)^.containsError then begin
            adapters.raiseError('Operators << and >> expect a map variable (local or mutable) on the left-hand-side - colt not cast collection to map',location);
            exit(newVoidLiteral);
          end;
        end;
        if (mutation=tt_cso_mapPut) and not((RHS^.literalType in C_mapTypes) or (RHS^.literalType in C_listTypes) and (P_listLiteral(RHS)^.isKeyValuePair)) then begin
          adapters.raiseError('Operator << expect a map or key-value-pair on the right-hand-side',location);
          exit(newVoidLiteral);
        end;
        if value^.getReferenceCount>1 then begin
          value:=P_mapLiteral(oldValue)^.clone;
          disposeLiteral(oldValue);
        end;
        if mutation=tt_cso_mapPut then begin
          if RHS^.literalType in C_mapTypes
          then P_mapLiteral(value)^.putAll(P_mapLiteral(RHS))
          else P_mapLiteral(value)^.put(P_listLiteral(RHS)^[0],P_listLiteral(RHS)^[1],true);
        end else if mutation=tt_cso_mapDrop then begin
          P_mapLiteral(value)^.drop(P_scalarLiteral(RHS));
        end;
        exit(newVoidLiteral);
      end;
    end;
    result:=newVoidLiteral;
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

FUNCTION T_namedVariable.toString(CONST lengthLimit:longint=maxLongint):ansistring;
  begin
    result:=id+'='+value^.toString(lengthLimit-1-length(id));
  end;

FUNCTION T_namedVariable.readOnlyClone:P_namedVariable;
  begin
    new(result,create(id,value,true));
  end;

CONSTRUCTOR T_valueStore.create;
  begin
    parentStore:=nil;
    system.initCriticalSection(cs);
    setLength(scopeStack,0);
  end;

PROCEDURE T_valueStore.copyDataAsReadOnly(VAR original:T_valueStore);
  VAR i,i0:longint;
  PROCEDURE copyScope(CONST source:T_scope; OUT dest:T_scope);
    VAR k:longint;
    begin
      dest.blockingScope:=source.blockingScope;
      setLength(dest.v,length(source.v));
      for k:=0 to length(dest.v)-1 do dest.v[k]:=source.v[k]^.readOnlyClone;
    end;

  begin
    clear;
    //find first entry to be copied
    i0:=length(original.scopeStack)-1;
    if i0<0 then i0:=0;
    while (i0>0) and not(original.scopeStack[i0].blockingScope) do dec(i0);
    //copy entries
    setLength(scopeStack,length(original.scopeStack)-i0);
    for i:=i0 to length(original.scopeStack)-1 do copyScope(original.scopeStack[i],scopeStack[i-i0]);
  end;

FUNCTION T_valueStore.readOnlyClone:P_valueStore;
  begin
    new(result,create);
    result^.copyDataAsReadOnly(self);
  end;

DESTRUCTOR T_valueStore.destroy;
  begin
    clear;
    system.doneCriticalSection(cs);
  end;

PROCEDURE clearScope(VAR s:T_scope);
  VAR k:longint;
  begin
    with s do begin
      for k:=0 to length(v)-1 do dispose(v[k],destroy);
      setLength(v,0);
    end;
  end;

PROCEDURE T_valueStore.clear;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(scopeStack)-1 do clearScope(scopeStack[i]);
    setLength(scopeStack,0);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_valueStore.isEmpty:boolean;
  begin
    system.enterCriticalSection(cs);
    result:=length(scopeStack)=0;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePush(CONST blocking:boolean);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(scopeStack);
    setLength(scopeStack,i+1);
    scopeStack[i].blockingScope:=blocking;
    setLength(scopeStack[i].v,0);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePop;
  VAR i:longint;
      {$ifdef debugMode}
      named:P_namedVariable;
      {$endif}
  begin
    system.enterCriticalSection(cs);
    i:=length(scopeStack)-1;
    {$ifdef debugMode}
    if gui_started then begin
      writeln(stdErr,'        DEBUG: T_valueStore.scopePop (blocking=',scopeStack[i].blockingScope,')');
      for named in scopeStack[i].v do writeln(stdErr,'        DEBUG: ',named^.toString(100));
    end;
    {$endif}
    clearScope(scopeStack[i]);
    setLength(scopeStack,i);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_valueStore.getVariable(CONST id:T_idString; OUT blockEncountered:boolean):P_namedVariable;
  VAR i,k:longint;
  begin
    blockEncountered:=false;
    result:=nil;
    for i:=length(scopeStack)-1 downto 0 do with scopeStack[i] do begin
      for k:=length(v)-1 downto 0 do if v[k]^.getId=id then exit(v[k]);
      if blockingScope then begin
        blockEncountered:=true;
        exit(nil);
      end;
    end;
    if parentStore<>nil then result:=parentStore^.getVariable(id,blockEncountered);
  end;

PROCEDURE T_valueStore.createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean);
  VAR k:longint;
  begin
    system.enterCriticalSection(cs);
    with scopeStack[length(scopeStack)-1] do begin
      k:=length(v);
      setLength(v,k+1);
      new(v[k],create(id,value,readonly));
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.createVariable(CONST id:T_idString; CONST value:int64;     CONST readonly:boolean);
  VAR lit:P_intLiteral;
  begin
    lit:=newIntLiteral(value);
    createVariable(id,lit,readonly);
    lit^.unreference;
  end;

FUNCTION T_valueStore.getVariableValue(CONST id: T_idString): P_literal;
  VAR named:P_namedVariable;
      blocked:boolean;
  begin
    system.enterCriticalSection(cs);
    named:=getVariable(id,blocked);
    if named<>nil then result:=named^.getValue
    else result:=nil;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.setVariableValue(CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR named:P_namedVariable;
      blocked:boolean;
  begin
    system.enterCriticalSection(cs);
    named:=getVariable(id,blocked);
    if named<>nil then named^.setValue(value)
    else adapters^.raiseError('Cannot assign value to unknown local variable '+id,location);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_valueStore.mutateVariableValue(CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
  VAR named:P_namedVariable;
      blocked:boolean;
  begin
    system.enterCriticalSection(cs);
    named:=getVariable(id,blocked);
    if named<>nil then result:=named^.mutate(mutation,RHS,location,adapters^)
    else begin
      adapters^.raiseError('Cannot mutate unknown local variable '+id,location);
      result:=nil;
    end;
    system.leaveCriticalSection(cs);
  end;

{$ifdef fullVersion}
PROCEDURE T_valueStore.reportVariables(VAR variableReport:T_variableReport);
  VAR i :longint;
      i0:longint=0;
      up:longint=0;
      named:P_namedVariable;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(scopeStack)-1 do
    if scopeStack[i].blockingScope then begin up:=1; i0:=i; end
                                   else   inc(up);

    if (i0>0) and (parentStore<>nil) then parentStore^.reportVariables(variableReport);
    for i:=i0 to length(scopeStack)-1 do with scopeStack[i] do begin
      dec(up);
      for named in v do begin
        if up=0 then variableReport.addVariable(named,'local')
                else variableReport.addVariable(named,'local (+'+intToStr(up)+')');
      end;
    end;
    system.leaveCriticalSection(cs);
  end;
{$endif}

{$ifdef fullVersion}
CONSTRUCTOR T_variableReport.create;
  begin
    setLength(dat,0);
  end;

DESTRUCTOR T_variableReport.destroy;
  begin
    setLength(dat,0);
  end;

PROCEDURE T_variableReport.addVariable(CONST id: ansistring; CONST value: P_literal; CONST location: string; CONST retainExistent:boolean=false);
  VAR i,j:longint;
  begin
    if not(retainExistent) then begin
      j:=0;
      for i:=0 to length(dat)-1 do if dat[i].id<>id then begin
        dat[j]:=dat[i];
        inc(j);
      end;
      setLength(dat,j);
    end;
    setLength(dat,length(dat)+1);
    dat[length(dat)-1].id:=id;
    dat[length(dat)-1].value:=value;
    dat[length(dat)-1].location:=location;
  end;

PROCEDURE T_variableReport.addVariable(CONST namedVar: P_namedVariable; CONST location: string);
  begin
    addVariable(namedVar^.id,namedVar^.value,location);
  end;
{$endif}

end.
