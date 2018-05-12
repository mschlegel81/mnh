UNIT valueStore;
INTERFACE
USES sysutils,
     myGenerics,
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_litVar
     {$ifdef fullVersion},
     mnh_debuggingVar
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
      FUNCTION mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer):P_literal;
      FUNCTION getId:T_idString;
      FUNCTION getValue:P_literal;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):ansistring;
      FUNCTION readOnlyClone:P_namedVariable;
  end;

TYPE
  T_scope=record
    blockingScope:boolean;
    v:array of P_namedVariable;
  end;

  P_valueStore=^T_valueStore;
  T_valueStore=object
    private
      cs:TRTLCriticalSection;
      scopeStack     :array of T_scope;
      scopeTopIndex  :longint;
      PROCEDURE copyData(VAR original:T_valueStore; CONST readonly:boolean);
      FUNCTION getVariable(CONST id:T_idString; CONST ignoreBlocks:boolean; OUT blockEncountered:boolean):P_namedVariable;
    public
      parentStore:P_valueStore;
      CONSTRUCTOR create;
      FUNCTION readOnlyClone:P_valueStore;
      FUNCTION clone:P_valueStore;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      FUNCTION isEmpty:boolean;

      PROCEDURE createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean);
      PROCEDURE createVariable(CONST id:T_idString; CONST value:int64;     CONST readonly:boolean);
      FUNCTION  getVariableValue(CONST id: T_idString): P_literal;
      PROCEDURE setVariableValue(CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters);
      FUNCTION  mutateVariableValue(CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST threadContext:pointer):P_literal;
      PROCEDURE scopePush(CONST blocking:boolean);
      PROCEDURE scopePop;
      {$ifdef fullVersion}
      //For debugging:
      PROCEDURE reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
      PROCEDURE writeScopeList;
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

FUNCTION T_namedVariable.mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer):P_literal;
  begin
    if readonly then begin
      adapters.raiseError('Mutation of constant "'+id+'" is not allowed.',location);
      exit(newVoidLiteral);
    end;
    result:=mutateVariable(value,mutation,RHS,location,adapters,threadContext);
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
    scopeTopIndex:=-1;
  end;

PROCEDURE T_valueStore.copyData(VAR original:T_valueStore; CONST readonly:boolean);
  VAR i,i0:longint;
  PROCEDURE copyScope(CONST source:T_scope; OUT dest:T_scope);
    VAR k:longint;
    begin
      dest.blockingScope:=source.blockingScope;
      setLength(dest.v,length(source.v));
      if readonly
      then for k:=0 to length(dest.v)-1 do dest.v[k]:=source.v[k]^.readOnlyClone
      else for k:=0 to length(dest.v)-1 do new(dest.v[k],create(source.v[k]^.id,source.v[k]^.value,source.v[k]^.readonly));
    end;

  begin
    clear;
    //find first entry to be copied
    i0:=original.scopeTopIndex;
    if i0<0 then i0:=0;
    while (i0>0) and not(original.scopeStack[i0].blockingScope) do dec(i0);
    i0:=0;
    //copy entries
    scopeTopIndex:=original.scopeTopIndex-i0;
    setLength(scopeStack,scopeTopIndex+1);
    for i:=i0 to original.scopeTopIndex do copyScope(original.scopeStack[i],scopeStack[i-i0]);
  end;

FUNCTION T_valueStore.readOnlyClone:P_valueStore;
  begin
    new(result,create);
    result^.parentStore:=parentStore;
    result^.copyData(self,true);
  end;

FUNCTION T_valueStore.clone:P_valueStore;
  begin
    new(result,create);
    result^.parentStore:=parentStore;
    result^.copyData(self,false);
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
    for i:=0 to scopeTopIndex do clearScope(scopeStack[i]);
    scopeTopIndex:=-1;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_valueStore.isEmpty:boolean;
  begin
    system.enterCriticalSection(cs);
    result:=scopeTopIndex<=0;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePush(CONST blocking:boolean);
  begin
    system.enterCriticalSection(cs);
    inc(scopeTopIndex);
    if scopeTopIndex>=length(scopeStack) then setLength(scopeStack,scopeTopIndex+1);
    scopeStack[scopeTopIndex].blockingScope:=blocking;
    setLength(scopeStack[scopeTopIndex].v,0);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePop;
  begin
    system.enterCriticalSection(cs);
    clearScope(scopeStack[scopeTopIndex]);
    dec(scopeTopIndex);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_valueStore.getVariable(CONST id:T_idString; CONST ignoreBlocks:boolean; OUT blockEncountered:boolean):P_namedVariable;
  VAR i,k:longint;
  begin
    blockEncountered:=false;
    result:=nil;
    for i:=scopeTopIndex downto 0 do with scopeStack[i] do begin
      //for k:=length(v)-1 downto 0 do if v[k]^.getId=id then exit(v[k]);
      for k:=0 to length(v)-1 do if v[k]^.getId=id then begin
        if k<>0 then begin

        end;
        exit(v[k]);

      end;
      if blockingScope and not(ignoreBlocks) then begin
        blockEncountered:=true;
        exit(nil);
      end;
    end;
    if parentStore<>nil then begin
      enterCriticalSection(parentStore^.cs);
      result:=parentStore^.getVariable(id,ignoreBlocks,blockEncountered);
      leaveCriticalSection(parentStore^.cs);
    end;
  end;

PROCEDURE T_valueStore.createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean);
  VAR k:longint;
  begin
    system.enterCriticalSection(cs);
    with scopeStack[scopeTopIndex] do begin
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
    named:=getVariable(id,true,blocked);
    if named<>nil then result:=named^.getValue
    else result:=nil;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.setVariableValue(CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR named:P_namedVariable;
      blocked:boolean;
  begin
    system.enterCriticalSection(cs);
    named:=getVariable(id,false,blocked);
    if named<>nil then named^.setValue(value)
    else adapters^.raiseError('Cannot assign value to unknown local variable '+id,location);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_valueStore.mutateVariableValue(CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST threadContext:pointer):P_literal;
  VAR named:P_namedVariable;
      blocked:boolean;
  begin
    system.enterCriticalSection(cs);
    named:=getVariable(id,false,blocked);
    if named<>nil then result:=named^.mutate(mutation,RHS,location,adapters^,threadContext)
    else begin
      adapters^.raiseError('Cannot mutate unknown local variable '+id,location);
      result:=nil;
    end;
    system.leaveCriticalSection(cs);
  end;

{$ifdef fullVersion}
PROCEDURE T_valueStore.reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
  VAR i :longint;
      i0:longint=0;
      up:longint=0;
      named:P_namedVariable;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to scopeTopIndex do
    if scopeStack[i].blockingScope then begin up:=1; i0:=i; end
                                   else   inc(up);

    if (i0>0) and (parentStore<>nil) then parentStore^.reportVariables(variableReport);
    for i:=i0 to scopeTopIndex do with scopeStack[i] do begin
      dec(up);
      for named in v do variableReport.addEntry(named^.id,named^.value,false)
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.writeScopeList;
  VAR i,j:longint;
  begin
    if parentStore<>nil then begin
      parentStore^.writeScopeList;
      writeln('<       parent end        >')
    end;

    for i:=0 to scopeTopIndex do begin
      if scopeStack[i].blockingScope then writeln('===========================')
                                     else writeln('---------------------------');
      for j:=0 to length(scopeStack[i].v)-1 do write(scopeStack[i].v[j]^.getId,', ');
      writeln;
    end;
  end;
{$endif}

end.
