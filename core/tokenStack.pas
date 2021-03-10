UNIT tokenStack;
INTERFACE
USES sysutils,
     myGenerics,
     //MNH:
     mnh_constants,
     basicTypes,
     litVar,
     tokens,
     out_adapters,
     mnh_messages,
     recyclers
     {$ifdef fullVersion},
     profiling,
     debuggingVar
     {$endif};
TYPE
  P_tokenStack=^T_TokenStack;
  T_TokenStack=object
    alloc,
    topIndex:longint;
    dat:PP_token;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE popDestroy(CONST recycler:P_recycler);
    PROCEDURE popLink(VAR first:P_token);
    PROCEDURE push(VAR first:P_token);
    PROCEDURE quietPush(CONST first:P_token);
    PROCEDURE quietPop;
    FUNCTION topType:T_tokenType;
    FUNCTION hasTokenTypeAnywhere(CONST t:T_tokenType):boolean;
    FUNCTION toString(CONST first:P_token; CONST lengthLimit:longint=maxLongint):ansistring;
    {$ifdef fullVersion}
    FUNCTION toDebuggerString(CONST first:P_token; CONST lengthLimit:longint;
                              CONST parameterVar,localVar,globalVar,inlineVar:P_variableTreeEntryCategoryNode):T_arrayOfString;
    {$endif}
  end;

  {$ifdef fullVersion}
  T_callStackEntry=record
    callerLocation:T_tokenLocation;
    calleeId:ansistring;
    calleeLocation:T_tokenLocation;
    callerFuncLocation:T_tokenLocation;
    timeForProfiling_inclusive,
    timeForProfiling_exclusive:double;
    parameters:P_variableTreeEntryCategoryNode;
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
                     CONST parameters:P_variableTreeEntryCategoryNode;
                     CONST callerLocation: T_tokenLocation;
                     CONST callee: P_objectWithIdAndLocation);
      FUNCTION pop(CONST wallclockTime:double;CONST profiler:P_profiler):T_tokenLocation;
      PROCEDURE ensureTraceInError(VAR error:T_errorMessage);
      PROPERTY entry[index:longint]:T_callStackEntry read getEntry; default;
  end;
  {$endif}

IMPLEMENTATION
{$ifdef fullVersion}
CONSTRUCTOR T_callStack.create;
  begin
    setLength(dat,0);
    fill:=0;
  end;

DESTRUCTOR T_callStack.destroy;
  begin
    clear;
  end;

PROCEDURE T_callStack.clear;
  begin
    while fill>0 do pop(0,nil);
  end;

PROCEDURE T_callStack.push(CONST wallclockTime:double; CONST parameters:P_variableTreeEntryCategoryNode;
  CONST callerLocation: T_tokenLocation;
  CONST callee: P_objectWithIdAndLocation);
  begin
    if length(dat)<=fill then setLength(dat,length(dat)+16);
    dat[fill].callerLocation:=callerLocation;
    dat[fill].calleeId:=callee^.getId;
    dat[fill].calleeLocation:=callee^.getLocation;

    if fill>0 then begin
      with dat[fill-1] do timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
             dat[fill].callerFuncLocation:=dat[fill-1].calleeLocation;
    end else dat[fill].callerFuncLocation.package:=nil;
    dat[fill].timeForProfiling_exclusive:=wallclockTime;
    dat[fill].timeForProfiling_inclusive:=wallclockTime;
    dat[fill].parameters:=parameters;

    inc(fill);
  end;

FUNCTION T_callStack.pop(CONST wallclockTime: double;CONST profiler:P_profiler):T_tokenLocation;
  begin
    initialize(result);
    if fill<1 then begin
      result.package:=nil;
      result.column:=-1;
      result.line:=-1;
      exit(result);
    end;
    if profiler<>nil then begin
      with dat[fill-1] do begin
        timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
        timeForProfiling_inclusive:=wallclockTime-timeForProfiling_inclusive;
        profiler^.add(calleeId,callerFuncLocation,callerLocation,calleeLocation,timeForProfiling_inclusive, timeForProfiling_exclusive);
      end;
      if fill>1 then with dat[fill-2] do timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
    end;
    if dat[fill-1].parameters<>nil then begin
      dispose(dat[fill-1].parameters,destroy);
      dat[fill-1].parameters:=nil;
    end;
    result:=dat[fill-1].callerLocation;
    dec(fill);
  end;

PROCEDURE T_callStack.ensureTraceInError(VAR error:T_errorMessage);
  VAR i,k:longint;
  begin
    if length(error.stacktrace)=0 then begin
      setLength(error.stacktrace,fill);
      k:=0;
      for i:=fill-1 downto 0 do begin
        error.stacktrace[k].callee    :=dat[i].calleeId;
        error.stacktrace[k].location  :=dat[i].callerLocation;
        if dat[i].parameters=nil
        then error.stacktrace[k].parameters:='n/a'
        else error.stacktrace[k].parameters:=dat[i].parameters^.toStringForErrorTrace;
        inc(k);
      end;
    end;
  end;

FUNCTION T_callStack.getEntry(CONST index:longint):T_callStackEntry;
  begin
    result:=dat[index];
  end;
{$endif}

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

PROCEDURE T_TokenStack.popDestroy(CONST recycler:P_recycler);
  begin
    recycler^.disposeToken(dat[topIndex]);
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
    if topIndex>=0 then dec(topIndex);
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
    result:=result+' § '+tokensToString(first,lengthLimit);
  end;

{$ifdef fullVersion}
FUNCTION T_TokenStack.toDebuggerString(CONST first:P_token; CONST lengthLimit:longint;
                                       CONST parameterVar,localVar,globalVar,inlineVar:P_variableTreeEntryCategoryNode):T_arrayOfString;
  VAR prevWasIdLike:boolean=false;
  FUNCTION toShorterString(CONST t:P_token):ansistring;
    VAR alternativeNode:P_variableTreeEntryNamedValue=nil;
    begin
      result:=t^.toString(prevWasIdLike,prevWasIdLike);
      if t^.tokType=tt_literal then begin
        if (parameterVar<>nil)                       then alternativeNode:=parameterVar^.findEntryForValue(t^.data);
        if (alternativeNode=nil) and (localVar<>nil) then alternativeNode:=localVar    ^.findEntryForValue(t^.data);
        if alternativeNode=nil                       then alternativeNode:=globalVar   ^.findEntryForValue(t^.data);
        if alternativeNode=nil                       then alternativeNode:=inlineVar   ^.findEntryForValue(t^.data);
        if (alternativeNode<>nil) then begin
          if (length(alternativeNode^.getIdOnly)<length(result)) then begin
            result:=alternativeNode^.getIdOnly;
            prevWasIdLike:=true;
          end;
        end else if (length(result)>20) then begin
          alternativeNode:=inlineVar^.findEntryForValue(t^.data,true);
          result:=alternativeNode^.getIdOnly;
          prevWasIdLike:=true;
        end;
      end;
    end;

  VAR i0,i:longint;
      remainingLength:longint;
      part:string;
      pt:P_token;
  begin
    result:=C_EMPTY_STRING_ARRAY;
    inlineVar^.clear;
    if topIndex>=0 then begin
      i0:=topIndex;
      remainingLength:=lengthLimit div 2;
      while (i0>0) and (remainingLength>0) do begin
        dec(remainingLength,length(toShorterString(dat[i0])));
        dec(i0);
      end;
      remainingLength:=lengthLimit-3;
      inlineVar^.clear;
      if i0>0 then result:='... ';
      prevWasIdLike:=false;
      for i:=i0 to topIndex do append(result,toShorterString(dat[i]));
    end else remainingLength:=lengthLimit-3;
    append(result,' § ');
    pt:=first;
    i:=length(result);
    while (pt<>nil) and (remainingLength>0) do begin
      part:=toShorterString(pt);
      dec(remainingLength,length(part));
      append(result,part);
      pt:=pt^.next;
    end;
    if pt<>nil then append(result,' ...');
  end;
{$endif}

end.
