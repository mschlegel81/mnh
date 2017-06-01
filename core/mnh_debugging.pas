UNIT mnh_debugging;
INTERFACE
USES sysutils,
     //MNH:
     mnh_basicTypes,
     mnh_tokens,
     mnh_out_adapters,
     tokenStack;
TYPE
  P_debuggingSnapshot=^T_debuggingSnapshot;
  T_debuggingSnapshot=record
    location:T_tokenLocation;
    tokenStack:P_tokenStack;
    first:pointer;
    callStack:P_callStack;
  end;

  T_debuggerState=(breakSoonest,
                   breakOnLineChange,
                   breakOnStepOut,
                   breakOnStepIn,
                   runUntilBreakpoint,
                   dontBreakAtAll,
                   waitingForGUI);

  P_debuggingStepper=^T_debuggingStepper;
  T_debuggingStepper=object
    private
      breakpoints:array of T_searchTokenLocation;
      state:T_debuggerState;
      lastBreakLine:T_tokenLocation;
      lastBreakLevel:longint;
      snapshot:T_debuggingSnapshot;
      adapters:P_adapters;
      cs:TRTLCriticalSection;
    public
      CONSTRUCTOR create(CONST parentAdapters:P_adapters);
      DESTRUCTOR destroy;
      PROCEDURE resetForDebugging(CONST inPackage:P_objectWithPath);
      PROCEDURE stepping(CONST first:P_token; CONST stack:P_tokenStack; CONST callStack:P_callStack);
      //GUI interaction
      PROCEDURE haltEvaluation;
      PROCEDURE clearBreakpoints;
      PROCEDURE addBreakpoint(CONST fileName:string; CONST line:longint);
      PROCEDURE removeBreakpoint(CONST fileName:string; CONST line:longint);
      PROCEDURE setState(CONST newState: T_debuggerState);
      FUNCTION paused:boolean;
  end;

IMPLEMENTATION
CONSTRUCTOR T_debuggingStepper.create(CONST parentAdapters:P_adapters);
  begin
    initCriticalSection(cs);
    adapters:=parentAdapters;
    state:=dontBreakAtAll;
    setLength(breakpoints,0);
  end;

DESTRUCTOR T_debuggingStepper.destroy;
  begin
    enterCriticalSection(cs);
    setLength(breakpoints,0);
    leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.resetForDebugging(CONST inPackage:P_objectWithPath);
  begin
    enterCriticalSection(cs);
    state:=runUntilBreakpoint;
    lastBreakLevel:=-1;
    lastBreakLine.line:=-1;
    lastBreakLine.package:=inPackage;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.stepping(CONST first: P_token; CONST stack: P_tokenStack; CONST callStack:P_callStack);
  FUNCTION isEqualLine(CONST loc1,loc2:T_tokenLocation):boolean; inline;
    begin
      result:=(loc1.package=loc2.package) and
              (loc1.line   =loc2.line);
    end;

  FUNCTION isEqualLine(CONST loc1:T_tokenLocation; CONST loc2:T_searchTokenLocation):boolean; inline;
    begin
      result:=(loc1.package^.getPath=loc2.fileName) and
              (loc1.line            =loc2.line);
    end;

  FUNCTION breakpointEncountered:boolean;
    VAR i:longint;
    begin
      result:=false;
      for i:=0 to length(breakpoints)-1 do if isEqualLine(first^.location,breakpoints[i]) then exit(true);
    end;

  PROCEDURE prepareSnapshot;
    begin
      snapshot.location:=first^.location;
      snapshot.tokenStack:=stack;
      snapshot.first:=first;
      snapshot.callStack:=callStack;
    end;

  VAR lineChanged:boolean;
  begin
    if (state=dontBreakAtAll) then exit;
    system.enterCriticalSection(cs);
    if state=breakSoonest then state:=waitingForGUI
    else begin
      lineChanged:=not(isEqualLine(lastBreakLine,first^.location));
      if (state=breakOnLineChange ) and ((callStack^.size< lastBreakLevel) or lineChanged) or
         (state=breakOnStepOut    ) and  (callStack^.size< lastBreakLevel) or
         (state=breakOnStepIn     ) and  (callStack^.size> lastBreakLevel) or
         (state=runUntilBreakpoint) and ((callStack^.size<>lastBreakLevel) or lineChanged) and breakpointEncountered
      then state:=waitingForGUI;
    end;

    if state=waitingForGUI then begin
      lastBreakLine:=first^.location;
      lastBreakLevel:=callStack^.size;
      prepareSnapshot;
      adapters^.logBreakpointEncountered(@snapshot);
    end;
    while state=waitingForGUI do begin
      system.leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      system.enterCriticalSection(cs);
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.haltEvaluation;
  begin
    system.enterCriticalSection(cs);
    state:=dontBreakAtAll;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.clearBreakpoints;
  begin
    system.enterCriticalSection(cs);
    setLength(breakpoints,0);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.addBreakpoint(CONST fileName: string; CONST line: longint);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(breakpoints);
    setLength(breakpoints,i+1);
    breakpoints[i].fileName:=fileName;
    breakpoints[i].line    :=line;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.removeBreakpoint(CONST fileName:string; CONST line:longint);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=0;
    while (i<length(breakpoints)) and not((breakpoints[i].fileName=fileName) and (breakpoints[i].line=line)) do inc(i);
    if i<length(breakpoints) then begin
      breakpoints[i]:=breakpoints[length(breakpoints)-1];
      setLength(breakpoints,length(breakpoints)-1);
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.setState(CONST newState: T_debuggerState);
  begin
    system.enterCriticalSection(cs);
    if (state=waitingForGUI) or (newState=breakSoonest) then state:=newState;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_debuggingStepper.paused:boolean;
  begin
    system.enterCriticalSection(cs);
    result:=state=waitingForGUI;
    system.leaveCriticalSection(cs);
  end;

end.
