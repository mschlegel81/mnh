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
CONST
  STACK_SEPARATOR =' ╟┤ ';

TYPE
  P_tokenStack=^T_TokenStack;
  T_TokenStack=object
    top:P_token;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE popLink(VAR first:P_token); inline;
    PROCEDURE push(VAR first:P_token); inline;
    PROCEDURE pushFreshlyCreatedToken(CONST t:P_token);
    FUNCTION toString(CONST first:P_token; CONST lengthLimit:longint=maxLongint):ansistring;
    {$ifdef fullVersion}
    FUNCTION toDebuggerString(CONST first:P_token; CONST lengthLimit:longint;
                              CONST parameterVar,localVar,globalVar,inlineVar:P_variableTreeEntryCategoryNode):T_arrayOfString;
    {$endif}
  end;

  {$ifdef fullVersion}
  T_callStackEntry=record
    callerLocation:T_tokenLocation;
    calleeId:T_idString;
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
                     CONST calleeId: T_idString; CONST calleeLocation:T_tokenLocation);
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
  CONST calleeId: T_idString;
  CONST calleeLocation:T_tokenLocation);
  begin
    if length(dat)<=fill then setLength(dat,length(dat)+16);
    dat[fill].callerLocation:=callerLocation;
    dat[fill].calleeId:=calleeId;
    dat[fill].calleeLocation:=calleeLocation;

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
    top:=nil;
  end;

DESTRUCTOR T_TokenStack.destroy;
  begin
    assert(top=nil);
  end;

PROCEDURE T_TokenStack.popLink(VAR first: P_token);
  VAR newTop:P_token;
  begin
    newTop:=top^.next;
            top^.next:=first;
                       first:=top;
                              top:=newTop;
  end;

PROCEDURE T_TokenStack.push(VAR first: P_token);
  VAR toPush:P_token;
  begin
    toPush:=first;
    first:=first^.next;
    toPush^.next:=top;
    top:=toPush;
  end;

PROCEDURE T_TokenStack.pushFreshlyCreatedToken(CONST t:P_token);
  begin
    t^.next:=top;
    top:=t;
  end;

FUNCTION T_TokenStack.toString(CONST first: P_token; CONST lengthLimit: longint): ansistring;
  VAR i:longint;
      prevWasIdLike:boolean=false;
      fullStack:array of P_token;
      token: P_token;
  begin
    if top<>nil then begin
      setLength(fullStack,0);
      token:=top;
      result:='';
      while (token<>nil) and (length(result)<lengthLimit) do begin
        result:=token^.toString(prevWasIdLike,prevWasIdLike,lengthLimit-length(result))+result;
        setLength(fullStack,length(fullStack)+1); fullStack[length(fullStack)-1]:=token; token:=token^.next;
      end;
      if token<>nil
      then result:='... '
      else result:='';
      prevWasIdLike:=false;
      for i:=length(fullStack)-1 downto 0 do result:=result+fullStack[i]^.toString(prevWasIdLike,prevWasIdLike);
      setLength(fullStack,0);
    end else result:='';
    result:=result+STACK_SEPARATOR+tokensToString(first,lengthLimit);
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

  VAR i:longint;
      remainingLength:longint;
      part:string;
      token:P_token;
      fullStack:array of P_token;
  begin
    result:=C_EMPTY_STRING_ARRAY;
    inlineVar^.clear;
    if top<>nil then begin
      setLength(fullStack,0);
      token:=top;
      remainingLength:=lengthLimit div 2;
      while (token<>nil) and (remainingLength>0) do begin
        dec(remainingLength,length(toShorterString(token)));
        setLength(fullStack,length(fullStack)+1); fullStack[length(fullStack)-1]:=token; token:=token^.next;
      end;
      if token<>nil
      then result:='... ';
      prevWasIdLike:=false;
      for i:=length(fullStack)-1 downto 0 do append(result,toShorterString(fullStack[i]));
      setLength(fullStack,0);
    end else remainingLength:=lengthLimit-3;
    append(result,STACK_SEPARATOR);
    token:=first;
    i:=length(result);
    while (token<>nil) and (remainingLength>0) do begin
      part:=toShorterString(token);
      dec(remainingLength,length(part));
      append(result,part);
      token:=token^.next;
    end;
    if token<>nil then append(result,' ...');
  end;
{$endif}

end.
