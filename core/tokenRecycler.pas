UNIT tokenRecycler;
INTERFACE
USES myGenerics,
     mnh_basicTypes,mnh_constants,
     mnh_litVar,
     mnh_tokens,mnh_out_adapters;
TYPE
  P_tokenRecycler=^T_tokenRecycler;
  T_tokenRecycler=object
    private
      dat:array[0..2047] of P_token;
      fill:longint;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;

      FUNCTION disposeToken(p:P_token):P_token; inline;
      PROCEDURE cascadeDisposeToken(VAR p:P_token);
      FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer=nil):P_token; inline;
      FUNCTION newToken(CONST original:T_token):P_token; inline;
      FUNCTION newToken(CONST original:P_token):P_token; inline;
      FUNCTION cascaseDisposeToLiteral(p:P_token; CONST errorBefore:boolean):P_literal;
  end;

FUNCTION getBodyParts(CONST first:P_token; CONST initialBracketLevel:longint; VAR recycler:T_tokenRecycler; CONST adapters:P_adapters; OUT closingBracket:P_token):T_bodyParts;
IMPLEMENTATION
FUNCTION getBodyParts(CONST first:P_token; CONST initialBracketLevel:longint; VAR recycler:T_tokenRecycler; CONST adapters:P_adapters; OUT closingBracket:P_token):T_bodyParts;
  VAR t,p:P_token;
      bracketLevel,i:longint;
      partLength:longint=-1;
  begin
    closingBracket:=nil;
    bracketLevel:=initialBracketLevel;
    t:=first; p:=nil;
    if (first^.next<>nil) and (first^.next^.tokType<>tt_separatorComma) then begin
      setLength(result,1);
      result[0].first:=first^.next;
    end else begin
      adapters^.raiseError('Invalid special construct; Cannot find closing bracket.',first^.location);
      setLength(result,0);
      exit;
    end;
    while (t<>nil) and not((t^.tokType=tt_braceClose) and (bracketLevel=1)) do begin
      if t^.tokType in C_openingBrackets then inc(bracketLevel)
      else if t^.tokType in C_closingBrackets then dec(bracketLevel)
      else if (t^.tokType=tt_separatorComma) and (bracketLevel=1) then begin
        if partLength=0 then adapters^.raiseError('Empty body part.',result[length(result)-1].first^.location);
        result[length(result)-1].last:=p; //end of body part is token before comma
        setLength(result,length(result)+1);
        result[length(result)-1].first:=t^.next; //start of next body part is token after comma
        partLength:=-1; //excluding delimiting separators
      end;
      p:=t; t:=t^.next; inc(partLength);
    end;
    result[length(result)-1].last:=p; //end of body part is token before comma
    if (t=nil) or (t^.tokType<>tt_braceClose) or (bracketLevel<>1) then begin
      adapters^.raiseError('Invalid special construct; Cannot find closing bracket.',first^.location);
      setLength(result,0);
      exit;
    end;
    closingBracket:=t;
    for i:=0 to length(result)-1 do begin
      if result[i].last^.next<>closingBracket then recycler.disposeToken(result[i].last^.next);
      result[i].last^.next:=nil;
    end;
  end;

CONSTRUCTOR T_tokenRecycler.create;
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
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
  end;

FUNCTION T_tokenRecycler.disposeToken(p: P_token): P_token;
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

PROCEDURE T_tokenRecycler.cascadeDisposeToken(VAR p: P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_tokenRecycler.newToken(CONST tokenLocation: T_tokenLocation; CONST tokenText: ansistring; CONST tokenType: T_tokenType; CONST ptr: pointer): P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST original: T_token): P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST original: P_token): P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original^);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.cascaseDisposeToLiteral(p:P_token; CONST errorBefore:boolean):P_literal;
  begin
    if not(errorBefore) and (p<>nil) and (p^.tokType=tt_literal) and (p^.next=nil) then begin
      result:=P_literal(p^.data)^.rereferenced;
      disposeToken(p);
    end else begin
      result:=nil;
      cascadeDisposeToken(p);
    end;
  end;

end.
