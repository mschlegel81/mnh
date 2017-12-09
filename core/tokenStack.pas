UNIT tokenStack;
INTERFACE
USES //MNH:
     mnh_constants,
     mnh_basicTypes,
     mnh_out_adapters,
     mnh_litVar,
     mnh_tokens
     {$ifdef fullVersion},mnh_profiling{$endif};
TYPE
  P_tokenStack=^T_TokenStack;
  T_TokenStack=object
    alloc,
    topIndex:longint;
    dat:PP_token;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE popDestroy(VAR recycler:T_tokenRecycler);
    PROCEDURE popLink(VAR first:P_token);
    PROCEDURE push(VAR first:P_token);
    PROCEDURE quietPush(CONST first:P_token);
    PROCEDURE quietPop;
    FUNCTION topType:T_tokenType;
    FUNCTION hasTokenTypeAnywhere(CONST t:T_tokenType):boolean;
    FUNCTION toString(CONST first:P_token; CONST lengthLimit:longint=maxLongint):ansistring;
  end;

  T_callStackEntry=record
    callLocation:T_tokenLocation;
    calleeId:ansistring;
    calleeLocation:T_tokenLocation;
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
      PROCEDURE push({$ifdef fullVersion}CONST wallclockTime:double;{$endif}
                     CONST callerLocation: T_tokenLocation;
                     CONST callee: P_objectWithIdAndLocation);
      PROCEDURE pop({$ifdef fullVersion}CONST wallclockTime:double;CONST profiler:P_profiler{$endif});
      PROCEDURE print(VAR adapters:T_adapters);
      PROPERTY entry[index:longint]:T_callStackEntry read getEntry; default;
  end;

  T_idStack=object
    ids:array of array of ansistring;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clear;
    PROCEDURE scopePush;
    PROCEDURE scopePop;
    FUNCTION oneAboveBottom:boolean;
    FUNCTION scopeBottom:boolean;
    FUNCTION addId(CONST id:ansistring):boolean;
    FUNCTION hasId(CONST id:ansistring):boolean;
  end;

IMPLEMENTATION
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

PROCEDURE T_callStack.push({$ifdef fullVersion}CONST wallclockTime:double;{$endif}
  CONST callerLocation: T_tokenLocation;
  CONST callee: P_objectWithIdAndLocation);
  begin
    if length(dat)<=fill then setLength(dat,length(dat)+16);
    dat[fill].callLocation:=callerLocation;
    dat[fill].calleeId:=callee^.getId;
    dat[fill].calleeLocation:=callee^.getLocation;
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
        profiler^.add(calleeId,calleeLocation,timeForProfiling_inclusive, timeForProfiling_exclusive);
      end;
      if fill>1 then with dat[fill-2] do timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
    end;
    {$endif}
    dec(fill);
  end;

PROCEDURE T_callStack.print(VAR adapters:T_adapters);
  VAR i:longint;
  begin
    for i:=fill-1 downto 0 do with dat[i] do
    adapters.logCallStackInfo(calleeId,callLocation);
  end;

FUNCTION T_callStack.getEntry(CONST index:longint):T_callStackEntry;
  begin
    result:=dat[index];
  end;

CONSTRUCTOR T_TokenStack.create;
  begin
    alloc:=8;
    getMem(dat,alloc*sizeOf(P_token));
    topIndex:=-1;
  end;

DESTRUCTOR T_TokenStack.destroy;
  begin
    freeMem(dat,alloc*sizeOf(P_token));
  end;

PROCEDURE T_TokenStack.popDestroy(VAR recycler:T_tokenRecycler);
  begin
    recycler.disposeToken(dat[topIndex]);
    dec(topIndex);
  end;

PROCEDURE T_TokenStack.popLink(VAR first: P_token);
  begin
    dat[topIndex]^.next:=first;
    first:=dat[topIndex];
    dec(topIndex);
  end;

PROCEDURE T_TokenStack.push(VAR first: P_token);
  begin
    inc(topIndex);
    if topIndex>=alloc then begin
      inc(alloc,alloc);
      ReAllocMem(dat,alloc*sizeOf(P_token));
    end;
    dat[topIndex]:=first;
    first:=first^.next;
  end;

PROCEDURE T_TokenStack.quietPush(CONST first:P_token);
  begin
    inc(topIndex);
    if topIndex>=alloc then begin
      inc(alloc,alloc);
      ReAllocMem(dat,alloc*sizeOf(P_token));
    end;
    dat[topIndex]:=first;
  end;

PROCEDURE T_TokenStack.quietPop;
  begin
    dec(topIndex);
  end;

FUNCTION T_TokenStack.topType:T_tokenType;
  begin
    if topIndex>=0 then result:=dat[topIndex]^.tokType
                   else result:=tt_EOL;
  end;

FUNCTION T_TokenStack.hasTokenTypeAnywhere(CONST t:T_tokenType):boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=topIndex downto 0 do if dat[i]^.tokType=t then exit(true);
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

CONSTRUCTOR T_idStack.create;
  begin
    setLength(ids,0);
  end;

DESTRUCTOR T_idStack.destroy;
  begin clear;  end;

PROCEDURE T_idStack.clear;
  begin
    while not(scopeBottom) do scopePop;
  end;

PROCEDURE T_idStack.scopePush;
  begin
    setLength(ids,length(ids)+1);
  end;

PROCEDURE T_idStack.scopePop;
  begin
    setLength(ids,length(ids)-1);
  end;

FUNCTION T_idStack.oneAboveBottom:boolean;
  begin
    result:=length(ids)=1;
  end;

FUNCTION T_idStack.scopeBottom:boolean;
  begin
    result:=length(ids)=0;
  end;

FUNCTION T_idStack.addId(CONST id:ansistring):boolean;
  VAR i,j:longint;
  begin
    i:=length(ids)-1;
    for j:=0 to length(ids[i])-1 do if ids[i,j]=id then exit(false);
    j:=length(ids[i]);
    setLength(ids[i],j+1);
    ids[i,j]:=id;
    result:=true;
  end;

FUNCTION T_idStack.hasId(CONST id:ansistring):boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=length(ids)-1 downto 0 do for j:=0 to length(ids[i])-1 do if ids[i,j]=id then exit(true);
  end;

end.
