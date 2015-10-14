UNIT mnh_out_adapters;

INTERFACE

USES mnh_constants, mnh_tokLoc, myGenerics,mySys,sysutils;

CONST
  HALT_MESSAGE = 'Evaluation haltet (most probably by user).';
  NO_PARAMETERLESS_MAIN_MESSAGE = 'Cannot apply user defined rule main to parameter list ()';

TYPE
  T_storedMessage = record
    messageType : T_messageTypeOrErrorLevel;
    simpleMessage: ansistring;
    multiMessage: T_arrayOfString;
    location: T_tokenLocation;
  end;

  P_abstractOutAdapter = ^T_abstractOutAdapter;

  { T_abstractOutAdapter }

  T_abstractOutAdapter = object
    CONSTRUCTOR create;
    PROCEDURE clearConsole; virtual; abstract;
    PROCEDURE writeEcho(CONST mt:T_messageTypeOrErrorLevel; CONST s: ansistring); virtual; abstract;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual; abstract;
    PROCEDURE errorOut(CONST level:T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual; abstract;
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;

  { T_consoleOutAdapter }

  T_consoleOutAdapter = object(T_abstractOutAdapter)
    echoOn:boolean;
    minErrorLevel:T_messageTypeOrErrorLevel;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE writeEcho(CONST mt:T_messageTypeOrErrorLevel; CONST s: ansistring); virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST level:T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
  end;

  T_outputBehaviour= record
    doEchoInput: boolean;
    doEchoDeclaration: boolean;
    doShowExpressionOut: boolean;
  end;

  T_collectingOutAdapter=object(T_abstractOutAdapter)
    outputBehaviour:T_outputBehaviour;
    storedMessages:array of T_storedMessage;
    cs:TRTLCriticalSection;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE writeEcho(CONST mt:T_messageTypeOrErrorLevel; CONST s: ansistring); virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST level:T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
    PROCEDURE appendSingleMessage(CONST message:T_storedMessage);
    PROCEDURE clearMessages;
  end;

  {$ifdef fullVersion}
  T_debugSignal=(ds_wait,ds_run,ds_runUntilBreak,ds_step,ds_abort);

  T_stepper=object
    breakAtLine:T_tokenLocation;
    cs:TRTLCriticalSection;
    signal:T_debugSignal;
    toStep:longint;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION stepping(CONST location:T_tokenLocation):boolean;

    PROCEDURE setFreeRun;
    PROCEDURE setBreakpoint(CONST fileName:ansistring; CONST line:longint);
    PROCEDURE doStep;
    PROCEDURE doMultiStep(CONST stepCount:longint);
    PROCEDURE doAbort;
  end;
  {$endif}

VAR
  {$ifdef fullVersion}stepper:T_stepper;{$endif}
  outAdapter:P_abstractOutAdapter;
  consoleOutAdapter:T_consoleOutAdapter;
  maxErrorLevel: T_messageTypeOrErrorLevel;

PROCEDURE clearErrors;
PROCEDURE raiseError(CONST thisErrorLevel: T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
FUNCTION errorLevel: T_messageTypeOrErrorLevel;
PROCEDURE haltEvaluation;
PROCEDURE setDefaultCallbacks;

PROCEDURE haltWithAdaptedSystemErrorLevel;
VAR
  hasHaltMessage: boolean = false;
  hasNoParameterlessMainMessage: boolean = false;
  systemErrorlevel: specialize G_safeVar<longint>;


IMPLEMENTATION
VAR errorCount:longint=0;

PROCEDURE clearErrors;
  begin
    maxErrorLevel := el0_allOkay;
    hasHaltMessage := false;
    hasNoParameterlessMainMessage := false;
    errorCount:=0;
  end;

PROCEDURE raiseError(CONST thisErrorLevel: T_messageTypeOrErrorLevel;
  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    InterLockedIncrement(errorCount); if (thisErrorLevel<>els_step) and (errorCount>20) and (thisErrorLevel<=maxErrorLevel) then exit;
    if (thisErrorLevel > maxErrorLevel) then begin
      maxErrorLevel := thisErrorLevel;
      errorCount:=0;
    end;
    if errorMessage = HALT_MESSAGE then hasHaltMessage := true;
    if errorMessage = NO_PARAMETERLESS_MAIN_MESSAGE then hasNoParameterlessMainMessage := true;
    outAdapter^.errorOut(thisErrorLevel,errorMessage,errorLocation);
  end;

FUNCTION errorLevel: T_messageTypeOrErrorLevel;
  begin
    if hasHaltMessage then result:=el5_systemError else result := maxErrorLevel;
  end;

PROCEDURE haltEvaluation;
  begin
    raiseError(el5_systemError, HALT_MESSAGE, C_nilTokenLocation);
  end;

PROCEDURE setDefaultCallbacks;
  begin
    outAdapter:=@consoleOutAdapter;
  end;

PROCEDURE haltWithAdaptedSystemErrorLevel;
  VAR L:longint=0;
  begin
    if errorLevel>=el3_evalError then begin
      L:=103;
      if errorLevel>=el4_parsingError then begin
        L:=104;
        if errorLevel>=el5_systemError then L:=105;
      end;
    end;
    if L>systemErrorlevel.value then halt(L)
                                else halt(systemErrorlevel.value);
  end;

{ T_abstractOutAdapter }

CONSTRUCTOR T_abstractOutAdapter.create;
  begin
  end;

{ T_consoleOutAdapter }

CONSTRUCTOR T_consoleOutAdapter.create;
  begin
    echoOn:=false;
    minErrorLevel:=el3_evalError;
  end;

DESTRUCTOR T_consoleOutAdapter.destroy;
  begin
  end;

PROCEDURE T_consoleOutAdapter.clearConsole;
  begin
    mySys.clearConsole;
  end;

PROCEDURE T_consoleOutAdapter.writeEcho(CONST mt:T_messageTypeOrErrorLevel; CONST s: ansistring);
  begin
    if not(echoOn) then exit;
    writeln(C_errorLevelTxt[mt],s);
  end;

PROCEDURE T_consoleOutAdapter.printOut(CONST s: T_arrayOfString);
  VAR i:longint;
  begin
    for i:=0 to length(s)-1 do writeln(s[i]);
  end;

PROCEDURE T_consoleOutAdapter.errorOut(CONST level: T_messageTypeOrErrorLevel;
  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    if (level<minErrorLevel) or (level=elz_endOfEvaluation) then exit;
    writeln(stdErr, C_errorLevelTxt[errorLevel],ansistring(errorLocation),' ', errorMessage);
  end;

CONSTRUCTOR T_collectingOutAdapter.create;
  begin
    system.InitCriticalSection(cs);
    setLength(storedMessages,0);
  end;

DESTRUCTOR T_collectingOutAdapter.destroy;
  begin
    system.DoneCriticalsection(cs);
    clearMessages;
  end;

PROCEDURE T_collectingOutAdapter.clearConsole;
  VAR msg:T_storedMessage;
  begin
    system.enterCriticalSection(cs);
    clearMessages;
    msg.messageType:=elc_clearConsole;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.writeEcho(CONST mt: T_messageTypeOrErrorLevel; CONST s: ansistring);
  VAR msg:T_storedMessage;
  begin
    case mt of
      elo_echoOutput     : if not(outputBehaviour.doShowExpressionOut) then exit;
      eld_echoDeclaration: if not(outputBehaviour.doEchoDeclaration) then exit;
      ele_echoInput      : if not(outputBehaviour.doEchoInput) then exit;
    end;
    system.enterCriticalSection(cs);
    msg.messageType:=mt;
    msg.simpleMessage:=s;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.printOut(CONST s: T_arrayOfString);
  VAR msg:T_storedMessage;
  begin
    system.enterCriticalSection(cs);
    msg.messageType:=elp_printline;
    msg.multiMessage:=s;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.errorOut(CONST level: T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR msg:T_storedMessage;
  begin
    {$ifdef debugMode}
    consoleOutAdapter.errorOut(level,errorMessage,errorLocation);
    {$endif}
    if (level<el2_warning) and (level<>els_step) then exit;
    system.enterCriticalSection(cs);
    msg.messageType:=level;
    msg.simpleMessage:=errorMessage;
    msg.location:=errorLocation;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.appendSingleMessage(CONST message: T_storedMessage);
  begin
    setLength(storedMessages,length(storedMessages)+1);
    storedMessages[length(storedMessages)-1]:=message;
  end;

PROCEDURE T_collectingOutAdapter.clearMessages;
  begin
    setLength(storedMessages,0);
  end;

{$ifdef fullVersion}
  CONSTRUCTOR T_stepper.create;
  begin
    system.initCriticalSection(cs);
    setFreeRun;
  end;

DESTRUCTOR T_stepper.destroy;
  begin
    system.doneCriticalSection(cs);
  end;

FUNCTION T_stepper.stepping(CONST location:T_tokenLocation):boolean;
  begin
    if signal=ds_run then exit(false);
    system.enterCriticalSection(cs);
    if signal=ds_runUntilBreak then begin
      if (location.fileName=breakAtLine.fileName) and
         (location.line    =breakAtLine.line)
      then begin
        signal:=ds_wait;
        result:=true;
      end else result:=false;
      system.leaveCriticalSection(cs);
      exit(result);
    end;

    if signal=ds_wait then repeat
      system.leaveCriticalSection(cs);
      sleep(10);
      system.enterCriticalSection(cs);
    until signal<>ds_wait;
    if signal=ds_step then begin
      dec(toStep);
      if toStep<=0 then signal:=ds_wait;
    end;
    system.leaveCriticalSection(cs);
    result:=true;
  end;

PROCEDURE T_stepper.doStep;
  begin
    system.enterCriticalSection(cs);
    signal:=ds_step;
    toStep:=1;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.setFreeRun;
  begin
    system.enterCriticalSection(cs);
    signal:=ds_run;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.setBreakpoint(CONST fileName:ansistring; CONST line:longint);
  begin
    system.enterCriticalSection(cs);
    signal:=ds_runUntilBreak;
    breakAtLine.fileName:=fileName;
    breakAtLine.line:=line;
    system.leaveCriticalSection(cs);
  end;


PROCEDURE T_stepper.doMultiStep(CONST stepCount:longint);
  begin
    system.enterCriticalSection(cs);
    signal:=ds_step;
    toStep:=stepCount;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.doAbort;
  begin
    signal:=ds_run;
    haltEvaluation;
  end;
{$endif}

INITIALIZATION
  consoleOutAdapter.create;
  setDefaultCallbacks;
  maxErrorLevel := el0_allOkay;
  systemErrorlevel.create(0);
  {$ifdef fullVersion}
  stepper.create;
  {$endif}

FINALIZATION
  {$ifdef fullVersion}
  stepper.destroy;
  {$endif}
end.
