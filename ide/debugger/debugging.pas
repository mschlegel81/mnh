UNIT debugging;
INTERFACE
USES basicTypes,
     tokens,
     mnh_messages,
     out_adapters,
     debuggingVar,
     tokenStack;
TYPE
  P_debuggingSnapshot=^T_debuggingSnapshot;

  { T_debuggingSnapshot }

  T_debuggingSnapshot=object(T_payloadMessage)
    tokenStack:P_tokenStack;
    first:P_token;
    callStack:P_callStack;
    globalVariableReport,
    localVariableReport : P_variableTreeEntryCategoryNode;
    CONSTRUCTOR create(CONST tokens_:P_tokenStack; CONST first_:P_token; CONST stack_:P_callStack;
                       CONST globals,locals:P_variableTreeEntryCategoryNode);
    DESTRUCTOR destroy; virtual;
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
      lastContextVar:F_fillCategoryNode;
      adapters:P_messages;
      packageVar:F_fillCategoryNode;
      debuggerCs,breakCs:TRTLCriticalSection;
    public
      CONSTRUCTOR create(CONST parentAdapters:P_messages);
      DESTRUCTOR destroy;
      PROCEDURE resetForDebugging(CONST inPackage:P_objectWithPath; CONST packageVar_:F_fillCategoryNode);
      PROCEDURE stepping(CONST first:P_token; CONST stack:P_tokenStack; CONST callStack:P_callStack; CONST contextVar:F_fillCategoryNode);
      //GUI interaction
      PROCEDURE haltEvaluation;
      PROCEDURE setBreakpoints(CONST locations:array of T_searchTokenLocation);
      PROCEDURE setState(CONST newState: T_debuggerState);
      FUNCTION paused:boolean;
  end;

IMPLEMENTATION
USES sysutils;
CONSTRUCTOR T_debuggingSnapshot.create(CONST tokens_: P_tokenStack;
  CONST first_: P_token; CONST stack_: P_callStack; CONST globals,
  locals: P_variableTreeEntryCategoryNode);
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
    globalVariableReport:=globals;
    localVariableReport :=locals ;
  end;

DESTRUCTOR T_debuggingSnapshot.destroy;
  begin
    if globalVariableReport<>nil then dispose(globalVariableReport,destroy);
    if localVariableReport <>nil then dispose(localVariableReport ,destroy);
  end;

CONSTRUCTOR T_debuggingStepper.create(CONST parentAdapters:P_messages);
  begin
    initCriticalSection(debuggerCs);
    initCriticalSection(breakCs);
    adapters:=parentAdapters;
    state:=dontBreakAtAll;
    setLength(breakpoints,0);
    packageVar:=nil;
  end;

DESTRUCTOR T_debuggingStepper.destroy;
  begin
    enterCriticalSection(breakCs);
    enterCriticalSection(debuggerCs);
    try
      setLength(breakpoints,0);
    finally
      leaveCriticalSection(breakCs);
      leaveCriticalSection(debuggerCs);
      doneCriticalSection(breakCs);
      leaveCriticalSection(debuggerCs);
    end;
  end;

PROCEDURE T_debuggingStepper.resetForDebugging(CONST inPackage:P_objectWithPath; CONST packageVar_:F_fillCategoryNode);
  begin
    enterCriticalSection(debuggerCs);
    try
      state:=runUntilBreakpoint;
      lastBreakLevel:=-1;
      lastBreakLine.line:=-1;
      lastBreakLine.package:=inPackage;
      lastContextVar:=nil;
      packageVar:=packageVar_;
    finally
      leaveCriticalSection(debuggerCs);
    end;
  end;

PROCEDURE T_debuggingStepper.stepping(CONST first: P_token; CONST stack: P_tokenStack; CONST callStack:P_callStack; CONST contextVar:F_fillCategoryNode);
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
      if (callStack^.size<>lastBreakLevel) or
         not(isEqualLine(lastBreakLine,first^.location)) or
         (lastContextVar<>contextVar) then begin
        result:=false;
        for i:=0 to length(breakpoints)-1 do if isEqualLine(first^.location,breakpoints[i]) then exit(true);
      end else begin
        if (lastContextVar=contextVar) and (callStack^.size<lastBreakLevel) then lastBreakLevel:=callStack^.size;
        result:=false;
      end;
    end;

  VAR snapshot:P_debuggingSnapshot;
      globalVariableReport,
      localVariableReport :P_variableTreeEntryCategoryNode;
  begin
    if (state=dontBreakAtAll) then exit;
    system.enterCriticalSection(breakCs);
    system.enterCriticalSection(debuggerCs);
    try
      if (lastContextVar=nil) or (lastContextVar=contextVar) then
      case state of
        breakSoonest      : state:=waitingForGUI;
        breakOnLineChange : if (callStack^.size<lastBreakLevel) or
                               (callStack^.size=lastBreakLevel) and not(isEqualLine(lastBreakLine,first^.location)) then state:=waitingForGUI;
        breakOnStepOut    : if (callStack^.size<lastBreakLevel) then state:=waitingForGUI;
        breakOnStepIn     : if (callStack^.size>lastBreakLevel) then state:=waitingForGUI;
      end;
      if (state<>waitingForGUI) and breakpointEncountered then state:=waitingForGUI;
      if state=waitingForGUI then begin
        {$ifdef debugMode}
        writeln('+----------------- - - -');
        writeln('| DEBUGGER HALTED');
        writeln('+----------------- - - -');
        writeln('| Level last ',lastBreakLevel);
        writeln('|       curr ',callStack^.size);
        writeln('| Line  last ',string(lastBreakLine));
        writeln('|       curr ',string(first^.location));
        writeln('+----------------- - - -');
        {$endif}

        lastBreakLine:=first^.location;
        lastBreakLevel:=callStack^.size;

        new(globalVariableReport,create(dvc_global));
        if packageVar<>nil then packageVar(globalVariableReport^);
        new(localVariableReport,create(dvc_local));
        contextVar(localVariableReport^);

        new(snapshot,create(stack,first,callStack,globalVariableReport,localVariableReport));
        adapters^.postCustomMessage(snapshot);
        while state=waitingForGUI do begin
          system.leaveCriticalSection(debuggerCs);
          ThreadSwitch;
          sleep(1);
          system.enterCriticalSection(debuggerCs);
        end;
      end;
    finally
      system.leaveCriticalSection(debuggerCs);
      system.leaveCriticalSection(breakCs);
    end;
  end;

PROCEDURE T_debuggingStepper.haltEvaluation;
  begin
    system.enterCriticalSection(debuggerCs);
    try
      state:=dontBreakAtAll;
    finally
      system.leaveCriticalSection(debuggerCs);
    end;
  end;

PROCEDURE T_debuggingStepper.setBreakpoints(CONST locations:array of T_searchTokenLocation);
  VAR i:longint;
  begin
    system.enterCriticalSection(debuggerCs);
    try
      setLength(breakpoints,length(locations));
      for i:=0 to length(locations)-1 do breakpoints[i]:=locations[i];
    finally
      system.leaveCriticalSection(debuggerCs);
    end;
  end;

PROCEDURE T_debuggingStepper.setState(CONST newState: T_debuggerState);
  begin
    system.enterCriticalSection(debuggerCs);
    try
      if (state=waitingForGUI) or (newState=breakSoonest) then begin
        state:=newState;
        if state in [runUntilBreakpoint,dontBreakAtAll] then lastContextVar:=nil;
      end;
    finally
      system.leaveCriticalSection(debuggerCs);
    end;
  end;

FUNCTION T_debuggingStepper.paused:boolean;
  begin
    system.enterCriticalSection(debuggerCs);
    try
      result:=state=waitingForGUI;
    finally
      system.leaveCriticalSection(debuggerCs);
    end;
  end;

end.
