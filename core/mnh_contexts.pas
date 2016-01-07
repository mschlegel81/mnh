UNIT mnh_contexts;
INTERFACE
USES mnh_constants,mnh_tokens,mnh_tokLoc, mnh_out_adapters,mnh_litVar;
TYPE
  T_valueStoreMarker=(vsm_none,vsm_void,vsm_first);

TYPE
  T_valueStore=object
    cs:TRTLCriticalSection;
    data:array of record
      marker:T_valueStoreMarker;
      v:P_namedVariable;
    end;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE scopePush;
    PROCEDURE scopePop;
    FUNCTION getVariable(CONST id:ansistring):P_namedVariable;
    PROCEDURE createVariable(CONST id:ansistring; CONST value:P_literal);
    //For debugging:
    PROCEDURE reportVariables(VAR adapters:T_adapters);
  end;

  P_evaluationContext=^T_evaluationContext;
  T_evaluationContext=object
    dat:array[0..2047] of P_token;
    fill:longint;
    parentContext:P_evaluationContext;
    valueStore:T_valueStore;
    adapters:P_adapters;
    allowDelegation:boolean;
    CONSTRUCTOR createNormalContext(CONST outAdapters:P_adapters);
    CONSTRUCTOR createSanboxContext(CONST outAdapters:P_adapters);
    PROCEDURE adopt(CONST parent:P_evaluationContext);
    DESTRUCTOR destroy;
    //Recycler routines:
    FUNCTION disposeToken(p:P_token):P_token; inline;
    PROCEDURE cascadeDisposeToken(VAR p:P_token);
    FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer):P_token; inline;
    FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType):P_token; inline;
    FUNCTION newToken(CONST original:T_token):P_token; inline;
    FUNCTION newToken(CONST original:P_token):P_token; inline;
    //Local scope routines:
    FUNCTION getVariable(CONST id:ansistring):P_namedVariable;
    FUNCTION getVariableValue(CONST id:ansistring):P_literal;
    //For debugging:
    PROCEDURE reportVariables;
  end;

IMPLEMENTATION
CONSTRUCTOR T_valueStore.create;
  begin
    system.initCriticalSection(cs);
    setLength(data,0);
  end;

DESTRUCTOR T_valueStore.destroy;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(data)-1 do if data[i].v<>nil then dispose(data[i].v,destroy);
    setLength(data,0);
    system.leaveCriticalSection(cs);
    system.doneCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePush;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(data);
    setLength(data,i+1);
    with data[i] do begin
      v:=nil;
      marker:=vsm_void;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePop;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(data);
    repeat
      dec(i);
      with data[i] do if v<>nil then dispose(v,destroy);
    until data[i].marker<>vsm_none;
    setLength(data,i);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_valueStore.getVariable(CONST id:ansistring):P_namedVariable;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    result:=nil;
    for i:=length(data)-1 downto 0 do with data[i] do if (v<>nil) and (v^.getId=id) then begin
      system.leaveCriticalSection(cs);
      exit(v);
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.createVariable(CONST id:ansistring; CONST value:P_literal);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(data);
    with data[i-1] do if marker=vsm_void then begin
      marker:=vsm_first;
      new(v,create(id,value));
      system.leaveCriticalSection(cs);
      exit;
    end;
    setLength(data,i+1);
    with data[i] do begin
      marker:=vsm_none;
      new(v,create(id,value));
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.reportVariables(VAR adapters:T_adapters);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(data)-1 do with data[i] do begin
      if marker<>vsm_none then adapters.raiseCustomMessage(mt_debug_varInfo,'---------------',C_nilTokenLocation);
      if v<>nil           then adapters.raiseCustomMessage(mt_debug_varInfo,v^.toString,C_nilTokenLocation);
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluationContext.adopt(CONST parent:P_evaluationContext);
  begin
    parentContext:=parent;
    if parent<>nil then adapters:=parent^.adapters;
  end;

CONSTRUCTOR T_evaluationContext.createNormalContext(CONST outAdapters:P_adapters);
  VAR i:longint;
  begin
    parentContext:=nil;
    adapters:=outAdapters;
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
    valueStore.create;
    allowDelegation:=true;
  end;

CONSTRUCTOR T_evaluationContext.createSanboxContext(CONST outAdapters:P_adapters);
  VAR i:longint;
  begin
    parentContext:=nil;
    adapters:=outAdapters;
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
    valueStore.create;
    allowDelegation:=false;
  end;


DESTRUCTOR T_evaluationContext.destroy;
  begin
    while fill>0 do begin
      dec(fill);
      try
        dispose(dat[fill],destroy);
      except
        dat[fill]:=nil;
      end;
    end;
    valueStore.destroy;
  end;

FUNCTION T_evaluationContext.disposeToken(p:P_token):P_token;
  begin
    if p=nil then exit(nil);
    result:=p^.next;
    if (fill>=length(dat))
    then dispose(p,destroy)
    else begin
      p^.undefine;
      dat[fill]:=p;
      inc(fill);
    end;
  end;

PROCEDURE T_evaluationContext.cascadeDisposeToken(VAR p:P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_evaluationContext.newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION T_evaluationContext.newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType);
    result^.next:=nil;
  end;

FUNCTION T_evaluationContext.newToken(CONST original:T_token):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end;

FUNCTION T_evaluationContext.newToken(CONST original:P_token):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original^);
    result^.next:=nil;
  end;

FUNCTION T_evaluationContext.getVariable(CONST id:ansistring):P_namedVariable;
  begin
    result:=valueStore.getVariable(id);
    if (result=nil) and (parentContext<>nil) then result:=parentContext^.getVariable(id);
  end;

FUNCTION T_evaluationContext.getVariableValue(CONST id:ansistring):P_literal;
  VAR named:P_namedVariable;
  begin
    named:=getVariable(id);
    if named=nil then result:=nil
                 else result:=named^.getValue;
  end;

PROCEDURE T_evaluationContext.reportVariables;
  begin
    if parentContext<>nil then parentContext^.reportVariables;
    valueStore.reportVariables(adapters^);
  end;

end.
