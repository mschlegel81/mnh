{$ifdef include_interface}
  T_tokenRecycler=object
    dat:array[0..1023] of P_token;
    fill:longint;

    scopeStack:array of array of record
      id:ansistring;
      value:P_literal;
    end;
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
  VAR topIdx,i:longint;
  begin
    topIdx:=length(scopeStack)-1;
    for i:=0 to length(scopeStack[topIdx])-1 do begin
      disposeLiteral(scopeStack[topIdx,i].value);
      scopeStack[topIdx,i].id:='';
    end;
    setLength(scopeStack[topIdx],0);
    setLength(scopeStack,topIdx);
  end;

FUNCTION T_tokenRecycler.scopeBottom:boolean;
  begin
    result:=length(scopeStack)=0;
  end;

PROCEDURE T_tokenRecycler.declareLocalValue(CONST id:ansistring; CONST value:P_literal);
  VAR i,j:longint;
  begin
    i:=length(scopeStack)-1;
    j:=0;
    while (j<length(scopeStack[i])) and (scopeStack[i,j].id<>id) do inc(j);
    if j>=length(scopeStack[i]) then begin
      setLength(scopeStack[i],j+1);
      scopeStack[i,j].id:=id;
    end else disposeLiteral(scopeStack[i,j].value);
    scopeStack[i,j].value:=value;
    value^.rereference;
  end;

PROCEDURE T_tokenRecycler.setLocalValue(CONST id:ansistring; CONST value:P_literal);
  VAR i,j:longint;
  begin
    for i:=length(scopeStack)-1 downto 0 do
    for j:=0 to length(scopeStack[i])-1 do if scopeStack[i,j].id=id then begin
      scopeStack[i,j].value:=value;
      value^.rereference;
      exit;
    end;
    declareLocalValue(id,value);
  end;

FUNCTION T_tokenRecycler.mutateInline(CONST id:ansistring; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  VAR i,j:longint;
      value:P_literal;
  CONST MAPPED_OP:array[tt_cso_assignPlus..tt_cso_assignDiv] of T_tokenType=(tt_operatorPlus,tt_operatorMinus,tt_operatorMult,tt_operatorDivReal);
  begin
    for i:=length(scopeStack)-1 downto 0 do
    for j:=0 to length(scopeStack[i])-1 do if scopeStack[i,j].id=id then begin
      value:=scopeStack[i,j].value;
      case mutation of
        tt_cso_assignPlus..tt_cso_assignDiv: begin
          result:=resolveOperator(value, MAPPED_OP[mutation], RHS, location);
          disposeLiteral(value);
          scopeStack[i,j].value:=result;
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
            scopeStack[i,j].value:=result;
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
            scopeStack[i,j].value:=result;
          end;
        end;
      end;
      Exit(result);
    end;
  end;

FUNCTION T_tokenRecycler.getLocalValueOrNull(CONST id:ansistring):P_literal;
  VAR i,j:longint;
  begin
    result:=nil;
    for i:=length(scopeStack)-1 downto 0 do
    for j:=0 to length(scopeStack[i])-1 do if scopeStack[i,j].id=id then begin
      result:=scopeStack[i,j].value;
      result^.rereference;
      exit(result);
    end;
  end;

{$endif}
