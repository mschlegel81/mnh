UNIT mnh_debugging;
INTERFACE
USES sysutils,
     //MNH:
     mnh_basicTypes,
     mnh_tokens,
     tokenStack;
TYPE
  T_debuggingSnapshot=record
    location:T_tokenLocation;
    tokenStack:P_tokenStack;
    first:pointer;
    callStack:P_callStack;
  end;

  P_debuggingStepper=^T_debuggingStepper;
  T_debuggingStepper=object
    private
      breakpoints:array of T_searchTokenLocation;
      state:(breakSoonest,
             breakOnLineChange,
             breakOnStepOut,
             breakOnStepIn,
             runUntilBreakpoint,
             dontBreakAtAll,
             waitingForGUI);
      lastBreakLine:T_tokenLocation;
      lastBreakLevel:longint;
      snapshot:T_debuggingSnapshot;
      cs:TRTLCriticalSection;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE resetForDebugging(CONST inPackage:P_objectWithPath);
      PROCEDURE stepping(CONST first:P_token; CONST stack:P_tokenStack; CONST callStack:P_callStack);
      //GUI interaction
      PROCEDURE haltEvaluation;
      PROCEDURE clearBreakpoints;
      PROCEDURE addBreakpoint(CONST fileName:string; CONST line:longint);
      PROCEDURE doContinue;
      PROCEDURE doStepInto;
      PROCEDURE doStepOut;
      PROCEDURE doStep;
      PROCEDURE doMicrostep;
      FUNCTION paused:boolean;
      FUNCTION getDebuggingSnapshot:T_debuggingSnapshot;
  end;


IMPLEMENTATION
CONSTRUCTOR T_debuggingStepper.create;
  begin
    initCriticalSection(cs);
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

PROCEDURE T_debuggingStepper.doContinue;
  begin
    system.enterCriticalSection(cs);
    state:=runUntilBreakpoint;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.doStepInto;
  begin
    system.enterCriticalSection(cs);
    state:=breakOnStepIn;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.doStepOut;
  begin
    system.enterCriticalSection(cs);
    state:=breakOnStepOut;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.doStep;
  begin
    system.enterCriticalSection(cs);
    state:=breakOnLineChange;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.doMicrostep;
  begin
    system.enterCriticalSection(cs);
    state:=breakSoonest;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_debuggingStepper.paused:boolean;
  begin
    system.enterCriticalSection(cs);
    result:=state=waitingForGUI;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_debuggingStepper.getDebuggingSnapshot:T_debuggingSnapshot;
  begin
    system.enterCriticalSection(cs);
    result:=snapshot;
    system.leaveCriticalSection(cs);
  end;

end.
