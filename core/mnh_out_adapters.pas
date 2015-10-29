UNIT mnh_out_adapters;

INTERFACE

USES mnh_constants, mnh_tokLoc, myGenerics,mySys,sysutils;

TYPE
  T_storedMessage = record
    messageType : T_messageType;
    simpleMessage: ansistring;
    multiMessage: T_arrayOfString;
    location: T_tokenLocation;
  end;

  P_abstractOutAdapter = ^T_abstractOutAdapter;

  { T_abstractOutAdapter }
  T_outputBehaviour= record
    doEchoInput: boolean;
    doEchoDeclaration: boolean;
    doShowExpressionOut: boolean;
    doShowTimingInfo: boolean;
    minErrorLevel: shortint;
  end;

  T_abstractOutAdapter = object
    outputBehaviour:T_outputBehaviour;
    CONSTRUCTOR create;
    PROCEDURE clearConsole; virtual; abstract;
    PROCEDURE writeEcho(CONST mt:T_messageType; CONST s: ansistring); virtual; abstract;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual; abstract;
    PROCEDURE errorOut(CONST level:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual; abstract;
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;

  { T_consoleOutAdapter }

  T_consoleOutAdapter = object(T_abstractOutAdapter)
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE writeEcho(CONST mt:T_messageType; CONST s: ansistring); virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST level:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
  end;

  T_collectingOutAdapter=object(T_abstractOutAdapter)
    storedMessages:array of T_storedMessage;
    cs:TRTLCriticalSection;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE writeEcho(CONST mt:T_messageType; CONST s: ansistring); virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST level:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
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

PROCEDURE clearErrors;
PROCEDURE raiseError_(CONST thisErrorLevel: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
PROCEDURE raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
PROCEDURE raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
PROCEDURE raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
FUNCTION noErrors: boolean; inline;
PROCEDURE haltEvaluation;
PROCEDURE setDefaultCallbacks;

PROCEDURE haltWithAdaptedSystemErrorLevel;
VAR
  maxErrorLevel: shortint;
  hasMessageOfType:array[T_messageType] of boolean;
  systemErrorlevel: specialize G_safeVar<longint>;
  parsingTime:double;

IMPLEMENTATION

PROCEDURE clearErrors;
  VAR mt:T_messageType;
  begin
    for mt:=low(T_messageType) to high(T_messageType) do begin
      hasMessageOfType[mt]:=false;
      if   maxErrorLevel>=C_errorLevelForMessageType[mt]
      then maxErrorLevel:=C_errorLevelForMessageType[mt]-1;
    end;
  end;

PROCEDURE raiseError_(CONST thisErrorLevel: T_messageType;  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    if maxErrorLevel< C_errorLevelForMessageType[thisErrorLevel] then
       maxErrorLevel:=C_errorLevelForMessageType[thisErrorLevel];
    hasMessageOfType[thisErrorLevel]:=true;
    outAdapter^.errorOut(thisErrorLevel,errorMessage,errorLocation);
  end;

PROCEDURE raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el3_evalError] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el3_evalError];
    hasMessageOfType[mt_el3_evalError]:=true;
    outAdapter^.errorOut(mt_el3_evalError,errorMessage,errorLocation);
  end;

PROCEDURE raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el2_warning] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el2_warning];
    hasMessageOfType[mt_el2_warning]:=true;
    outAdapter^.errorOut(mt_el2_warning,errorMessage,errorLocation);
  end;

PROCEDURE raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el1_note] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el1_note];
    hasMessageOfType[mt_el1_note]:=true;
    outAdapter^.errorOut(mt_el1_note,errorMessage,errorLocation);
  end;


FUNCTION noErrors: boolean; inline;
  begin
    result:=maxErrorLevel<3;
  end;

PROCEDURE haltEvaluation;
  begin
    raiseError_(mt_el5_haltMessageReceived, '', C_nilTokenLocation);
  end;

PROCEDURE setDefaultCallbacks;
  begin
    outAdapter:=@consoleOutAdapter;
  end;

PROCEDURE haltWithAdaptedSystemErrorLevel;
  VAR L:longint=0;
  begin
    if maxErrorLevel>=3 then begin
      L:=3;
      if maxErrorLevel>=4 then begin
        L:=4;
        if maxErrorLevel>=5 then L:=5;
      end;
    end;
    if L>systemErrorlevel.value then halt(L)
                                else halt(systemErrorlevel.value);
  end;

{ T_abstractOutAdapter }

CONSTRUCTOR T_abstractOutAdapter.create;
  begin
    with outputBehaviour do begin
      doEchoDeclaration:=false;
      doEchoInput:=false;
      doShowExpressionOut:=false;
      doShowTimingInfo:=false;
      minErrorLevel:=3;
    end;
  end;

{ T_consoleOutAdapter }

CONSTRUCTOR T_consoleOutAdapter.create;
  begin
    inherited create;
  end;

DESTRUCTOR T_consoleOutAdapter.destroy;
  begin
  end;

PROCEDURE T_consoleOutAdapter.clearConsole;
  begin
    mySys.clearConsole;
  end;

PROCEDURE T_consoleOutAdapter.writeEcho(CONST mt:T_messageType; CONST s: ansistring);
  begin
    case mt of
      mt_echo_output:      if not(outputBehaviour.doShowExpressionOut) then exit;
      mt_echo_declaration: if not(outputBehaviour.doEchoDeclaration)   then exit;
      mt_echo_input:       if not(outputBehaviour.doEchoInput)         then exit;
      else exit;
    end;
    writeln(C_errorLevelTxt[mt],s);
  end;

PROCEDURE T_consoleOutAdapter.printOut(CONST s: T_arrayOfString);
  VAR i:longint;
  begin
    for i:=0 to length(s)-1 do writeln(s[i]);
  end;

PROCEDURE T_consoleOutAdapter.errorOut(CONST level: T_messageType;
  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    if (C_errorLevelForMessageType[level]<outputBehaviour.minErrorLevel) or (level=mt_endOfEvaluation)
    or (level=mt_timing_info) and not(outputBehaviour.doShowTimingInfo) then exit;
    if level in [mt_debug_step,mt_el1_note,mt_el2_warning,mt_el3_evalError,mt_el4_parsingError,mt_el5_systemError]
    then writeln(stdErr, C_errorLevelTxt[level],ansistring(errorLocation),' ', errorMessage)
    else writeln(stdErr,                        ansistring(errorLocation),' ', errorMessage);
  end;

CONSTRUCTOR T_collectingOutAdapter.create;
  begin
    system.initCriticalSection(cs);
    setLength(storedMessages,0);
  end;

DESTRUCTOR T_collectingOutAdapter.destroy;
  begin
    system.doneCriticalSection(cs);
    clearMessages;
  end;

PROCEDURE T_collectingOutAdapter.clearConsole;
  VAR msg:T_storedMessage;
  begin
    system.enterCriticalSection(cs);
    clearMessages;
    msg.messageType:=mt_clearConsole;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.writeEcho(CONST mt: T_messageType; CONST s: ansistring);
  VAR msg:T_storedMessage;
  begin
    case mt of
      mt_echo_output     : if not(outputBehaviour.doShowExpressionOut) then exit;
      mt_echo_declaration: if not(outputBehaviour.doEchoDeclaration) then exit;
      mt_echo_input      : if not(outputBehaviour.doEchoInput) then exit;
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
    msg.messageType:=mt_printline;
    msg.multiMessage:=s;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.errorOut(CONST level: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR msg:T_storedMessage;
  begin
    {$ifdef debugMode}
    consoleOutAdapter.errorOut(level,errorMessage,errorLocation);
    {$endif}
    if ((C_errorLevelForMessageType[level]<outputBehaviour.minErrorLevel) or (level=mt_endOfEvaluation)
    or (level=mt_timing_info) and not(outputBehaviour.doShowTimingInfo)) and (level<>mt_debug_step) then exit;

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
  maxErrorLevel := -1;
  systemErrorlevel.create(0);
  {$ifdef fullVersion}
  stepper.create;
  {$endif}

FINALIZATION
  {$ifdef fullVersion}
  stepper.destroy;
  {$endif}
end.
