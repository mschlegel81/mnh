UNIT tokenStack;
INTERFACE
USES mnh_tokens,abstractContext;
TYPE
  P_tokenStack=^T_TokenStack;
  T_TokenStack=object
    topIndex:longint;
    dat:array of P_token;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE popDestroy(CONST recycler:P_abstractContext);
    PROCEDURE popLink(VAR first:P_token);
    PROCEDURE push(VAR first:P_token);
    PROCEDURE quietPush(CONST first:P_token);
    PROCEDURE quietPop;
    FUNCTION toString(CONST first:P_token; CONST lengthLimit:longint=maxLongint):ansistring;
  end;

IMPLEMENTATION
CONSTRUCTOR T_TokenStack.create;
  begin
    setLength(dat,0);
    topIndex:=-1;
  end;

DESTRUCTOR T_TokenStack.destroy;
  begin
    setLength(dat,0);
  end;

PROCEDURE T_TokenStack.popDestroy(CONST recycler:P_abstractContext);
  begin
    recycler^.disposeToken(dat[topIndex]);
    dec(topIndex);
    if topIndex<length(dat)-100 then setLength(dat,topIndex+1);
  end;

PROCEDURE T_TokenStack.popLink(VAR first: P_token);
  begin
    dat[topIndex]^.next:=first;
    first:=dat[topIndex];
    dec(topIndex);
    if topIndex<length(dat)-100 then setLength(dat,topIndex+1);
  end;

PROCEDURE T_TokenStack.push(VAR first: P_token);
  begin
    inc(topIndex);
    if topIndex>=length(dat) then setLength(dat,round(length(dat)*1.1)+10);
    dat[topIndex]:=first;
    first:=first^.next;
  end;

PROCEDURE T_TokenStack.quietPush(CONST first:P_token);
  begin
    inc(topIndex);
    if topIndex>=length(dat) then setLength(dat,round(length(dat)*1.1)+10);
    dat[topIndex]:=first;
  end;

PROCEDURE T_TokenStack.quietPop;
  begin
    dec(topIndex);
  end;

FUNCTION T_TokenStack.toString(CONST first: P_token; CONST lengthLimit: longint): ansistring;
  VAR i0,i:longint;
      prevWasIdLike:boolean=false;
  begin
    if topIndex>=0 then begin
      i0:=topIndex;
      result:='';
      while (i0>0) and (length(result)<lengthLimit) do begin
        result:=dat[i0]^.toString(prevWasIdLike,prevWasIdLike,lengthLimit-length(result))+result;
        dec(i0);
      end;
      if i0>0 then result:='... '
              else result:='';
      prevWasIdLike:=false;
      for i:=i0 to topIndex do result:=result+dat[i]^.toString(prevWasIdLike,prevWasIdLike);
    end else result:='';
    result:=result+' # '+tokensToString(first,lengthLimit);
  end;

end.
