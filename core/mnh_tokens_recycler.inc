{$ifdef include_interface}
  T_valueStore=object
    data:array of record
      id:ansistring;
      value:P_literal;
    end;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION indexOfId(id:ansistring):longint;
    PROCEDURE declareValue(CONST id:ansistring; CONST value:P_literal; CONST guaranteedToBeNew:boolean);
    PROCEDURE setValue(CONST id:ansistring; CONST value:P_literal);
    PROCEDURE setValue(CONST idx:longint; CONST value:P_literal);
    FUNCTION mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
    FUNCTION mutateInline(CONST idx:longint; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
    FUNCTION getValueOrNull(CONST id:ansistring):P_literal;
    FUNCTION getValueOrNull(CONST idx:longint):P_literal;
  end;

  T_threadsafeValueStore=object(T_valueStore)
    cs:TRTLCriticalSection;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION indexOfId(id:ansistring):longint;
    PROCEDURE declareValue(CONST id:ansistring; CONST value:P_literal; CONST guaranteedToBeNew:boolean);
    PROCEDURE setValue(CONST id:ansistring; CONST value:P_literal);
    PROCEDURE setValue(CONST idx:longint; CONST value:P_literal);
    FUNCTION mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
    FUNCTION mutateInline(CONST idx:longint; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
    FUNCTION getValueOrNull(CONST id:ansistring):P_literal;
    FUNCTION getValueOrNull(CONST idx:longint):P_literal;
  end;

  T_tokenRecycler=object
    dat:array[0..1023] of P_token;
    fill:longint;

    scopeStack:array of T_valueStore;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION disposeToken(p:P_token):P_token; inline;
    PROCEDURE cascadeDisposeToken(VAR p:P_token);
    FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer):P_token; inline;
    FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType):P_token; inline;
    FUNCTION newToken(CONST original:T_token):P_token; inline;
    FUNCTION newToken(CONST original:P_token):P_token; inline;

    PROCEDURE scopePush;
    PROCEDURE scopePop;
    FUNCTION scopeBottom:boolean;
    PROCEDURE declareLocalValue(CONST id:ansistring; CONST value:P_literal);
    PROCEDURE setLocalValue(CONST id:ansistring; CONST value:P_literal);
    FUNCTION mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
    FUNCTION getLocalValueOrNull(CONST id:ansistring):P_literal;
  end;

{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_valueStore.create;
  begin
    setLength(data,0);
  end;

DESTRUCTOR T_valueStore.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(data)-1 do disposeLiteral(data[i].value);
    setLength(data,0);
  end;

FUNCTION T_valueStore.indexOfId(id:ansistring):longint;
  VAR i:longint;
  begin
    for i:=0 to length(data)-1 do if data[i].id=id then exit(i);
    result:=-1;
  end;

PROCEDURE T_valueStore.declareValue(CONST id:ansistring; CONST value:P_literal; CONST guaranteedToBeNew:boolean);
  VAR i:longint;
  begin
    if guaranteedToBeNew then i:=length(data) else begin
      i:=0;
      while (i<length(data)) and (data[i].id<>id) do inc(i);
    end;
    if i>=length(data) then begin
      setLength(data,i+1);
      data[i].id:=id;
    end else disposeLiteral(data[i].value);
    data[i].value:=value;
    value^.rereference;
  end;

PROCEDURE T_valueStore.setValue(CONST id:ansistring; CONST value:P_literal);
  VAR idx:longint;
  begin
    idx:=indexOfId(id);
    if idx<0 then declareValue(id,value,true)
             else setValue(idx,value);
  end;

PROCEDURE T_valueStore.setValue(CONST idx:longint; CONST value:P_literal);
  begin
    disposeLiteral(data[idx].value);
    data[idx].value:=value;
    value^.rereference;
  end;

FUNCTION T_valueStore.mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  VAR idx:longint;
  begin
    idx:=indexOfId(id);
    if idx<0 then begin
      raiseError(el3_evalError,'Value with ID="'+id+'" is not contained in value store! Cannot apply mutation.',location);
      result:=nil;
    end else mutateInline(idx,mutation,RHS,location);
  end;

FUNCTION T_valueStore.mutateInline(CONST idx:longint; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  CONST MAPPED_OP:array[tt_cso_assignPlus..tt_cso_assignDiv] of T_tokenType=(tt_operatorPlus,tt_operatorMinus,tt_operatorMult,tt_operatorDivReal);
  VAR value:P_literal;
  begin
    value:=data[idx].value;
    case mutation of
      tt_cso_assignPlus..tt_cso_assignDiv: begin
        result:=resolveOperator(value, MAPPED_OP[mutation], RHS, location);
        disposeLiteral(value);
        data[idx].value:=result;
        result^.rereference;
      end;
      tt_cso_assignStrConcat: begin
        if (value^.literalType=lt_string) and (value^.getReferenceCount=1) and (RHS^.literalType in [lt_boolean..lt_string]) then begin
          P_stringLiteral(value)^.append(P_scalarLiteral(RHS)^.stringForm);
          result:=value;
          result^.rereference;
        end else begin
          result:=resolveOperator(value, tt_operatorStrConcat, RHS, location);
          disposeLiteral(value);
          data[idx].value:=result;
          result^.rereference;
        end;
      end;
      tt_cso_assignAppend: begin
        if (value^.literalType in C_validListTypes) and (value^.getReferenceCount=1) then begin
          if (RHS^.literalType in [lt_boolean..lt_expression])
          then P_listLiteral(value)^.append(RHS, true)
          else P_listLiteral(value)^.appendAll(P_listLiteral(RHS));
          result:=value;
          result^.rereference;
        end else begin
          result:=resolveOperator(value, tt_operatorConcat   , RHS, location);
          result^.rereference;
          disposeLiteral(value);
          data[idx].value:=result;
        end;
      end;
    end;
  end;

FUNCTION T_valueStore.getValueOrNull(CONST id:ansistring):P_literal;
  VAR idx:longint;
  begin
    idx:=indexOfId(id);
    if idx<0 then result:=nil else begin
      result:=data[idx].value;
      result^.rereference;
    end;
  end;

FUNCTION T_valueStore.getValueOrNull(CONST idx:longint):P_literal;
  begin
    if idx<0 then result:=nil else begin
      result:=data[idx].value;
      result^.rereference;
    end;
  end;

CONSTRUCTOR T_threadsafeValueStore.create;
  begin
    system.InitCriticalSection(cs);
    system.EnterCriticalsection(cs);
    inherited create;
    system.LeaveCriticalsection(cs);
  end;

DESTRUCTOR T_threadsafeValueStore.destroy;
  begin
    system.EnterCriticalsection(cs);
    inherited destroy;
    system.LeaveCriticalsection(cs);
    system.DoneCriticalsection(cs);
  end;

FUNCTION T_threadsafeValueStore.indexOfId(id:ansistring):longint;
  begin
    system.EnterCriticalsection(cs);
    result:=inherited indexOfId(id);
    system.LeaveCriticalsection(cs);
  end;

PROCEDURE T_threadsafeValueStore.declareValue(CONST id:ansistring; CONST value:P_literal; CONST guaranteedToBeNew:boolean);
  begin
    system.EnterCriticalsection(cs);
    inherited declareValue(id,value,guaranteedToBeNew);
    system.LeaveCriticalsection(cs);
  end;

PROCEDURE T_threadsafeValueStore.setValue(CONST id:ansistring; CONST value:P_literal);
  begin
    system.EnterCriticalsection(cs);
    inherited setValue(id,value);
    system.LeaveCriticalsection(cs);
  end;

PROCEDURE T_threadsafeValueStore.setValue(CONST idx:longint; CONST value:P_literal);
  begin
    system.EnterCriticalsection(cs);
    inherited setValue(idx,value);
    system.LeaveCriticalsection(cs);
  end;

FUNCTION T_threadsafeValueStore.mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  begin
    system.EnterCriticalsection(cs);
    result:=inherited mutateInline(id,mutation,RHS,location);
    system.LeaveCriticalsection(cs);
  end;

FUNCTION T_threadsafeValueStore.mutateInline(CONST idx:longint; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  begin
    system.EnterCriticalsection(cs);
    result:=inherited mutateInline(idx,mutation,RHS,location);
    system.LeaveCriticalsection(cs);
  end;

FUNCTION T_threadsafeValueStore.getValueOrNull(CONST id:ansistring):P_literal;
  begin
    system.EnterCriticalsection(cs);
    result:=inherited getValueOrNull(id);
    system.LeaveCriticalsection(cs);
  end;

FUNCTION T_threadsafeValueStore.getValueOrNull(CONST idx:longint):P_literal;
  begin
    system.EnterCriticalsection(cs);
    result:=inherited getValueOrNull(idx);
    system.LeaveCriticalsection(cs);
  end;

CONSTRUCTOR T_tokenRecycler.create;
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
    setLength(scopeStack,0);
  end;

DESTRUCTOR T_tokenRecycler.destroy;
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

FUNCTION T_tokenRecycler.disposeToken(p:P_token):P_token;
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

PROCEDURE T_tokenRecycler.cascadeDisposeToken(VAR p:P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_tokenRecycler.newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST original:T_token):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST original:P_token):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original^);
    result^.next:=nil;
  end;

PROCEDURE T_tokenRecycler.scopePush;
  begin
    setLength(scopeStack,length(scopeStack)+1);
  end;

PROCEDURE T_tokenRecycler.scopePop;
  VAR topIdx:longint;
  begin
    topIdx:=length(scopeStack)-1;
    scopeStack[topIdx].destroy;
    setLength(scopeStack,topIdx);
  end;

FUNCTION T_tokenRecycler.scopeBottom:boolean;
  begin
    result:=length(scopeStack)=0;
  end;

PROCEDURE T_tokenRecycler.declareLocalValue(CONST id:ansistring; CONST value:P_literal);
  VAR topIdx:longint;
  begin
    topIdx:=length(scopeStack)-1;
    scopeStack[topIdx].declareValue(id,value,false);
  end;

PROCEDURE T_tokenRecycler.setLocalValue(CONST id:ansistring; CONST value:P_literal);
  VAR i,j:longint;
  begin
    for i:=length(scopeStack)-1 downto 0 do begin
      j:=scopeStack[i].indexOfId(id);
      if j>=0 then begin
        scopeStack[i].setValue(j,value);
        exit;
      end;
    end;
    scopeStack[length(scopeStack)-1].declareValue(id,value,true);
  end;

FUNCTION T_tokenRecycler.mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  VAR i,j:longint;
  begin
    for i:=length(scopeStack)-1 downto 0 do begin
      j:=scopeStack[i].indexOfId(id);
      if j>=0 then exit(scopeStack[i].mutateInline(j,mutation,RHS,location));
    end;
    result:=nil;
  end;

FUNCTION T_tokenRecycler.getLocalValueOrNull(CONST id:ansistring):P_literal;
  VAR i,j:longint;
  begin
    result:=nil;
    for i:=length(scopeStack)-1 downto 0 do begin
      j:=scopeStack[i].indexOfId(id);
      if j>=0 then exit(scopeStack[i].getValueOrNull(j));
    end;
  end;

{$endif}
