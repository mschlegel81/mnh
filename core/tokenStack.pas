UNIT tokenStack;
INTERFACE
USES //MNH:
     mnh_basicTypes,
     mnh_out_adapters,
     mnh_litVar,
     mnh_tokens,tokenRecycler,mnh_profiling;
TYPE
  P_tokenStack=^T_TokenStack;
  T_TokenStack=object
    topIndex:longint;
    dat:array of P_token;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE popDestroy(VAR recycler:T_tokenRecycler);
    PROCEDURE popLink(VAR first:P_token);
    PROCEDURE push(VAR first:P_token);
    PROCEDURE quietPush(CONST first:P_token);
    PROCEDURE quietPop;
    FUNCTION toString(CONST first:P_token; CONST lengthLimit:longint=maxLongint):ansistring;
  end;

  T_callStackEntry=record
    callerLocation:T_tokenLocation;
    callee:P_objectWithIdAndLocation;
    callParameters:P_listLiteral;
    calleeLiteral:P_expressionLiteral;
    {$ifdef fullVersion}
    timeForProfiling_inclusive,
    timeForProfiling_exclusive:double;
    {$endif}
  end;

  P_callStack=^T_callStack;
  T_callStack=object
    private
      dat:array of T_callStackEntry;
      fill:longint;
      FUNCTION getEntry(CONST index:longint):T_callStackEntry;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROPERTY size:longint read fill;
      PROCEDURE clear;
      PROCEDURE push(CONST wallclockTime:double;
                     CONST callerLocation: T_tokenLocation;
                     CONST callee: P_objectWithIdAndLocation;
                     CONST callParameters: P_listLiteral;
                     CONST expressionLiteral:P_expressionLiteral);
      PROCEDURE pop({$ifdef fullVersion}CONST wallclockTime:double;CONST profiler:P_profiler{$endif});
      PROCEDURE print(VAR adapters:T_adapters);
      PROPERTY entry[index:longint]:T_callStackEntry read getEntry; default;
  end;

IMPLEMENTATION

{ T_callStack }

CONSTRUCTOR T_callStack.create;
  begin
    setLength(dat,0);
    fill:=0;
  end;

DESTRUCTOR T_callStack.destroy;
  begin

  end;

PROCEDURE T_callStack.clear;
  begin
    while fill>0 do pop({$ifdef fullVersion}0,nil{$endif});
  end;

PROCEDURE T_callStack.push(CONST wallclockTime: double;
  CONST callerLocation: T_tokenLocation;
  CONST callee: P_objectWithIdAndLocation;
  CONST callParameters: P_listLiteral;
  CONST expressionLiteral: P_expressionLiteral);
  begin
    if length(dat)<=fill then setLength(dat,length(dat)+16);
    dat[fill].callerLocation:=callerLocation;
    dat[fill].callee        :=callee;
    dat[fill].callParameters:=callParameters;
    dat[fill].calleeLiteral :=expressionLiteral;
    if callParameters   <>nil then callParameters   ^.rereference;
    if expressionLiteral<>nil then expressionLiteral^.rereference;
    {$ifdef fullVersion}
    if fill>0 then with dat[fill-1] do timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
    dat[fill].timeForProfiling_exclusive:=wallclockTime;
    dat[fill].timeForProfiling_inclusive:=wallclockTime;
    {$endif}
    inc(fill);
  end;

PROCEDURE T_callStack.pop({$ifdef fullVersion}CONST wallclockTime: double;CONST profiler:P_profiler{$endif});
  begin
    if fill<1 then exit;
    {$ifdef fullVersion}
    if profiler<>nil then begin
      with dat[fill-1] do begin
        timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
        timeForProfiling_inclusive:=wallclockTime-timeForProfiling_inclusive;
        profiler^.add(callee^.getId,callee^.getLocation,timeForProfiling_inclusive, timeForProfiling_exclusive);
       end;
      if fill>1 then with dat[fill] do timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
    end;
    {$endif}
    with dat[fill-1] do begin
      if callParameters<>nil then disposeLiteral(callParameters);
      if calleeLiteral <>nil then disposeLiteral(calleeLiteral);
    end;
    dec(fill);
  end;

PROCEDURE T_callStack.print(VAR adapters:T_adapters);
  VAR i:longint;
  begin
    for i:=fill-1 downto 0 do with dat[i] do
    adapters.logCallStackInfo(callee^.getId+' '+toParameterListString(callParameters,true,50),callerLocation);
  end;

FUNCTION T_callStack.getEntry(CONST index:longint):T_callStackEntry;
  begin
    result:=dat[index];
  end;

CONSTRUCTOR T_TokenStack.create;
  begin
    setLength(dat,0);
    topIndex:=-1;
  end;

DESTRUCTOR T_TokenStack.destroy;
  begin
    setLength(dat,0);
  end;

PROCEDURE T_TokenStack.popDestroy(VAR recycler:T_tokenRecycler);
  begin
    recycler.disposeToken(dat[topIndex]);
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
