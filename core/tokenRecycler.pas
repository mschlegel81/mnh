UNIT tokenRecycler;
INTERFACE
USES mnh_basicTypes,mnh_constants,
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

IMPLEMENTATION
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

//FUNCTION T_tokenRecycler.enterTryStatementReturningPreviousAdapters: P_adapters;
//  begin
//    result:=myAdapters;
//    myAdapters:=result^.collectingClone;
//  end;
//
//PROCEDURE T_tokenRecycler.leaveTryStatementReassumingPreviousAdapters(CONST previousAdapters: P_adapters; CONST tryBodyFailed: boolean);
//  begin
//    previousAdapters^.copyDataFromCollectingCloneDisposing(myAdapters,tryBodyFailed);
//    myAdapters:=previousAdapters;
//  end;

end.
