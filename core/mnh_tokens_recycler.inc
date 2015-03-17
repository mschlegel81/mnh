{$ifdef include_interface}
  T_tokenRecycler=object
    dat:array[0..1023] of P_token;
    fill:longint;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION disposeToken(p:P_token):P_token; inline;
    PROCEDURE cascadeDisposeToken(VAR p:P_token);
    FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer):P_token; inline;
    FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType):P_token; inline;
    FUNCTION newToken(CONST original:T_token):P_token; inline;
    FUNCTION newToken(CONST original:P_token):P_token; inline;
  end;

{$endif}
{$ifdef include_implementation}

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
{$endif}
