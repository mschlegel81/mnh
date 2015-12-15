UNIT mnh_contexts;
INTERFACE
USES mnh_constants,mnh_tokens,mnh_tokLoc, mnh_out_adapters,mnh_litVar;
TYPE
    T_valueStoreElement=object
    data:array of T_namedVariable;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION indexOfId(id:ansistring):longint;
    PROCEDURE declareValue(CONST id:ansistring; CONST value:P_literal; CONST guaranteedToBeNew:boolean);
    PROCEDURE setValue(CONST id:ansistring; CONST value:P_literal);
    PROCEDURE setValue(CONST idx:longint; CONST value:P_literal);
    FUNCTION mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
    FUNCTION mutateInline(CONST idx:longint; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
    FUNCTION getValueOrNull(CONST id:ansistring):P_literal;
    FUNCTION getValueOrNull(CONST idx:longint):P_literal;
  end;

  T_valueStore=array of T_valueStoreElement;

  P_evaluationContext=^T_evaluationContext;
  T_evaluationContext=object
    dat:array[0..2047] of P_token;
    fill:longint;
    parentContext:P_evaluationContext;
    scopeStack:T_valueStore;
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
    PROCEDURE scopePush;
    PROCEDURE scopePop;
    FUNCTION scopeBottom:boolean;
    PROCEDURE declareLocalValue(CONST id:ansistring; CONST value:P_literal);
    PROCEDURE setLocalValue(CONST id:ansistring; CONST value:P_literal);
    FUNCTION mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
    FUNCTION getLocalValueOrNull(CONST id:ansistring):P_literal;
    FUNCTION hasId(CONST id:ansistring):boolean;
    //For debugging:
    PROCEDURE reportVariables;
  end;

IMPLEMENTATION
CONSTRUCTOR T_valueStoreElement.create;
  begin
    setLength(data,0);
  end;

DESTRUCTOR T_valueStoreElement.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(data)-1 do data[i].destroy;
    setLength(data,0);
  end;

FUNCTION T_valueStoreElement.indexOfId(id:ansistring):longint;
  VAR i:longint;
  begin
    for i:=0 to length(data)-1 do if data[i].getId=id then exit(i);
    result:=-1;
  end;

PROCEDURE T_valueStoreElement.declareValue(CONST id:ansistring; CONST value:P_literal; CONST guaranteedToBeNew:boolean);
  VAR i:longint;
  begin
    if guaranteedToBeNew then i:=length(data) else begin
      i:=0;
      while (i<length(data)) and (data[i].getId<>id) do inc(i);
    end;
    if i>=length(data) then begin
      setLength(data,i+1);
      data[i].create(id,value);
    end else data[i].setValue(value);
  end;

PROCEDURE T_valueStoreElement.setValue(CONST id:ansistring; CONST value:P_literal);
  VAR idx:longint;
  begin
    idx:=indexOfId(id);
    if idx<0 then declareValue(id,value,true)
             else setValue(idx,value);
  end;

PROCEDURE T_valueStoreElement.setValue(CONST idx:longint; CONST value:P_literal);
  begin
    data[idx].setValue(value);
  end;

FUNCTION T_valueStoreElement.mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR idx:longint;
  begin
    idx:=indexOfId(id);
    if idx<0 then begin
      adapters.raiseError('Value with ID="'+id+'" is not contained in value store! Cannot apply mutation.',location);
      result:=nil;
    end else mutateInline(idx,mutation,RHS,location,adapters);
  end;

FUNCTION T_valueStoreElement.mutateInline(CONST idx:longint; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    result:=data[idx].mutate(mutation,RHS,location,adapters);
  end;

FUNCTION T_valueStoreElement.getValueOrNull(CONST id:ansistring):P_literal;
  VAR idx:longint;
  begin
    idx:=indexOfId(id);
    if idx<0 then result:=nil else begin
      result:=data[idx].getValue;
    end;
  end;

FUNCTION T_valueStoreElement.getValueOrNull(CONST idx:longint):P_literal;
  begin
    if idx<0 then result:=nil else begin
      result:=data[idx].getValue;
    end;
  end;

PROCEDURE T_evaluationContext.adopt(CONST parent:P_evaluationContext);
  begin
    parentContext:=parent;
    adapters:=parent^.adapters;
  end;

CONSTRUCTOR T_evaluationContext.createNormalContext(CONST outAdapters:P_adapters);
  VAR i:longint;
  begin
    parentContext:=nil;
    adapters:=outAdapters;
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
    setLength(scopeStack,0);
    allowDelegation:=true;
  end;

CONSTRUCTOR T_evaluationContext.createSanboxContext(CONST outAdapters:P_adapters);
  VAR i:longint;
  begin
    parentContext:=nil;
    adapters:=outAdapters;
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
    setLength(scopeStack,0);
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
    while not(scopeBottom) do scopePop;
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

PROCEDURE T_evaluationContext.scopePush;
  begin
    setLength(scopeStack,length(scopeStack)+1);
  end;

PROCEDURE T_evaluationContext.scopePop;
  VAR topIdx:longint;
  begin
    topIdx:=length(scopeStack)-1;
    scopeStack[topIdx].destroy;
    setLength(scopeStack,topIdx);
  end;

FUNCTION T_evaluationContext.scopeBottom:boolean;
  begin
    result:=length(scopeStack)=0;
  end;

PROCEDURE T_evaluationContext.declareLocalValue(CONST id:ansistring; CONST value:P_literal);
  VAR topIdx:longint;
  begin
    topIdx:=length(scopeStack)-1;
    scopeStack[topIdx].declareValue(id,value,false);
  end;

PROCEDURE T_evaluationContext.setLocalValue(CONST id:ansistring; CONST value:P_literal);
  VAR i,j:longint;
  begin
    for i:=length(scopeStack)-1 downto 0 do begin
      j:=scopeStack[i].indexOfId(id);
      if j>=0 then begin
        scopeStack[i].setValue(j,value);
        exit;
      end;
    end;
    if parentContext<>nil then parentContext^.setLocalValue(id,value)
                          else scopeStack[length(scopeStack)-1].declareValue(id,value,true);
  end;

FUNCTION T_evaluationContext.mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  VAR i,j:longint;
  begin
    for i:=length(scopeStack)-1 downto 0 do begin
      j:=scopeStack[i].indexOfId(id);
      if j>=0 then exit(scopeStack[i].mutateInline(j,mutation,RHS,location,adapters^));
    end;
    if parentContext<>nil then exit(parentContext^.mutateInline(id,mutation,RHS,location));
    result:=nil;
  end;

FUNCTION T_evaluationContext.getLocalValueOrNull(CONST id:ansistring):P_literal;
  VAR i,j:longint;
  begin
    result:=nil;
    for i:=length(scopeStack)-1 downto 0 do begin
      j:=scopeStack[i].indexOfId(id);
      if j>=0 then exit(scopeStack[i].getValueOrNull(j));
    end;
    if parentContext<>nil then result:=parentContext^.getLocalValueOrNull(id);
  end;

FUNCTION T_evaluationContext.hasId(CONST id:ansistring):boolean;
  VAR i:longint;
  begin
    for i:=length(scopeStack)-1 downto 0 do if scopeStack[i].indexOfId(id)>=0 then exit(true);
    if parentContext<>nil then result:=parentContext^.hasId(id)
                          else result:=false;
  end;

PROCEDURE T_evaluationContext.reportVariables;
  VAR i,j:longint;
  begin
    if parentContext<>nil then parentContext^.reportVariables;
    for i:=0 to length(scopeStack)-1 do begin
      adapters^.raiseCustomMessage(mt_debug_varInfo,'---------------',C_nilTokenLocation);
      for j:=0 to length(scopeStack[i].data)-1 do
        adapters^.raiseCustomMessage(mt_debug_varInfo,scopeStack[i].data[j].toString,C_nilTokenLocation);
    end;
  end;

end.
