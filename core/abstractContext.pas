UNIT abstractContext;
INTERFACE
USES mnh_basicTypes,mnh_constants, mnh_tokens,mnh_out_adapters;
TYPE
  P_abstractContext=^T_abstractContext;
  T_abstractContext=object
    private
      dat:array[0..2047] of P_token;
      fill:longint;
    protected
      myAdapters:P_adapters;
    public
      PROPERTY adapters:P_adapters read myAdapters;

      CONSTRUCTOR create(CONST outAdapters:P_adapters);
      DESTRUCTOR destroy;

      FUNCTION disposeToken(p:P_token):P_token; inline;
      PROCEDURE cascadeDisposeToken(VAR p:P_token);
      FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer=nil):P_token; inline;
      FUNCTION newToken(CONST original:T_token):P_token; inline;
      FUNCTION newToken(CONST original:P_token):P_token; inline;

      FUNCTION enterTryStatementReturningPreviousAdapters: P_adapters;
      PROCEDURE leaveTryStatementReassumingPreviousAdapters(CONST previousAdapters: P_adapters; CONST tryBodyFailed: boolean);
  end;

IMPLEMENTATION
CONSTRUCTOR T_abstractContext.create(CONST outAdapters:P_adapters);
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
    myAdapters:=outAdapters;
  end;

DESTRUCTOR T_abstractContext.destroy;
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

FUNCTION T_abstractContext.disposeToken(p: P_token): P_token;
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

PROCEDURE T_abstractContext.cascadeDisposeToken(VAR p: P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_abstractContext.newToken(CONST tokenLocation: T_tokenLocation; CONST tokenText: ansistring; CONST tokenType: T_tokenType; CONST ptr: pointer): P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION T_abstractContext.newToken(CONST original: T_token): P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end;

FUNCTION T_abstractContext.newToken(CONST original: P_token): P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original^);
    result^.next:=nil;
  end;

FUNCTION T_abstractContext.enterTryStatementReturningPreviousAdapters: P_adapters;
  begin
    result:=myAdapters;
    myAdapters:=result^.collectingClone;
  end;

PROCEDURE T_abstractContext.leaveTryStatementReassumingPreviousAdapters(CONST previousAdapters: P_adapters; CONST tryBodyFailed: boolean);
  begin
    previousAdapters^.copyDataFromCollectingCloneDisposing(myAdapters,tryBodyFailed);
    myAdapters:=previousAdapters;
  end;

end.
