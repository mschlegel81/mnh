UNIT debugging;
INTERFACE
USES sysutils,
     //MNH:
     basicTypes,
     tokens,
     mnh_messages,
     out_adapters,
     tokenStack;
TYPE
  P_debuggingSnapshot=^T_debuggingSnapshot;
  T_debuggingSnapshot=object(T_payloadMessage)
    tokenStack:P_tokenStack;
    first:P_token;
    callStack:P_callStack;
    CONSTRUCTOR create(CONST tokens_:P_tokenStack; CONST first_:P_token; CONST stack_:P_callStack);
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
      adapters:P_messages;
      cs:TRTLCriticalSection;
    public
      CONSTRUCTOR create(CONST parentAdapters:P_messages);
      DESTRUCTOR destroy;
      PROCEDURE resetForDebugging(CONST inPackage:P_objectWithPath);
      PROCEDURE stepping(CONST first:P_token; CONST stack:P_tokenStack; CONST callStack:P_callStack);
      //GUI interaction
      PROCEDURE haltEvaluation;
      PROCEDURE setBreakpoints(CONST locations:array of T_searchTokenLocation);
      PROCEDURE addBreakpoint(CONST location:T_searchTokenLocation);
      PROCEDURE removeBreakpoint(CONST location:T_searchTokenLocation);
      PROCEDURE setState(CONST newState: T_debuggerState);
      FUNCTION paused:boolean;
  end;

IMPLEMENTATION

CONSTRUCTOR T_debuggingSnapshot.create(CONST tokens_: P_tokenStack; CONST first_: P_token; CONST stack_: P_callStack);
  begin
    inherited create(mt_debugger_breakpoint);
    first:=first_;
    tokenStack:=tokens_;
    callStack:=stack_;
    if first=nil then with location do begin
      fileName:='?';
      line:=1;
      column:=1;
    end else location:=first^.location;
  end;

CONSTRUCTOR T_debuggingStepper.create(CONST parentAdapters:P_messages);
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

  VAR snapshot:P_debuggingSnapshot;
  begin
    if (state=dontBreakAtAll) then exit;
    system.enterCriticalSection(cs);
    case state of
      breakSoonest      : state:=waitingForGUI;
      breakOnLineChange : if (callStack^.size<lastBreakLevel) or
                             (callStack^.size=lastBreakLevel) and not(isEqualLine(lastBreakLine,first^.location)) then state:=waitingForGUI;
      breakOnStepOut    : if (callStack^.size<lastBreakLevel) then state:=waitingForGUI;
      breakOnStepIn     : if (callStack^.size>lastBreakLevel) then state:=waitingForGUI;
    end;
    if (state<>waitingForGUI) and
       ((callStack^.size<>lastBreakLevel) or not(isEqualLine(lastBreakLine,first^.location))) and
       breakpointEncountered
    then state:=waitingForGUI;
    if state=waitingForGUI then begin
      {$ifdef debugMode}
      writeln('+----------------- - - -');
      writeln('| DEBUGGER HALTED');
      writeln('+----------------- - - -');
      writeln('| Level last ',lastBreakLevel);
      writeln('|       curr ',callStack^.size);
      writeln('| Line  last ',string(lastBreakLine));
      writeln('|       curr ',string(first^.location));
      writeln('| Breakpoint ',breakpointEncountered);
      writeln('+----------------- - - -');
      {$endif}

      lastBreakLine:=first^.location;
      lastBreakLevel:=callStack^.size;
      new(snapshot,create(stack,first,callStack));
      adapters^.postCustomMessage(snapshot);
      while state=waitingForGUI do begin
        system.leaveCriticalSection(cs);
        ThreadSwitch;
        sleep(1);
        system.enterCriticalSection(cs);
      end;
      disposeMessage(snapshot);
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.haltEvaluation;
  begin
    system.enterCriticalSection(cs);
    state:=dontBreakAtAll;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.setBreakpoints(CONST locations:array of T_searchTokenLocation);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    setLength(breakpoints,length(locations));
    for i:=0 to length(locations)-1 do breakpoints[i]:=locations[i];
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.addBreakpoint(CONST location:T_searchTokenLocation);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(breakpoints);
    setLength(breakpoints,i+1);
    breakpoints[i]:=location;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_debuggingStepper.removeBreakpoint(CONST location:T_searchTokenLocation);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=0;
    while (i<length(breakpoints)) and not((breakpoints[i]=location)) do inc(i);
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
