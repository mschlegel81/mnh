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
    PROCEDURE writeEcho(CONST messageType:T_messageType; CONST s: ansistring); virtual; abstract;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual; abstract;
    PROCEDURE errorOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual; abstract;
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;

  { T_consoleOutAdapter }

  T_consoleOutAdapter = object(T_abstractOutAdapter)
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE writeEcho(CONST messageType:T_messageType; CONST s: ansistring); virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
  end;

  T_collectingOutAdapter=object(T_abstractOutAdapter)
    storedMessages:array of T_storedMessage;
    cs:TRTLCriticalSection;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE writeEcho(CONST mt:T_messageType; CONST s: ansistring); virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
    PROCEDURE appendSingleMessage(CONST message:T_storedMessage); virtual;
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
  systemErrorlevel: specialize G_safeVar<longint>;
  hasMessageOfType:array[T_messageType] of boolean;
  stackTraceCount:longint;
  outAdapter:array[0..7] of P_abstractOutAdapter;
  consoleOutAdapter:T_consoleOutAdapter;

PROCEDURE clearErrors;
PROCEDURE raiseCustomMessage(CONST thisErrorLevel: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
PROCEDURE raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
PROCEDURE raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
PROCEDURE raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
PROCEDURE printOut(CONST s:T_arrayOfString);
PROCEDURE clearPrint;
FUNCTION noErrors: boolean; inline;
PROCEDURE haltEvaluation;
PROCEDURE setDefaultCallbacks;
PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter);
PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);

FUNCTION someEchoDeclaration:boolean;
FUNCTION someEchoInput:boolean;
FUNCTION someEchoOutput:boolean;

PROCEDURE haltWithAdaptedSystemErrorLevel;

IMPLEMENTATION
VAR
  maxErrorLevel: shortint;
  parsingTime:double;

PROCEDURE clearErrors;
  VAR mt:T_messageType;
  begin
    for mt:=low(T_messageType) to high(T_messageType) do begin
      hasMessageOfType[mt]:=false;
      if   maxErrorLevel>=C_errorLevelForMessageType[mt]
      then maxErrorLevel:=C_errorLevelForMessageType[mt]-1;
    end;
    stackTraceCount:=0;
  end;

PROCEDURE raiseCustomMessage(CONST thisErrorLevel: T_messageType;  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR i:byte;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[thisErrorLevel] then
       maxErrorLevel:=C_errorLevelForMessageType[thisErrorLevel];
    hasMessageOfType[thisErrorLevel]:=true;
    if (thisErrorLevel=mt_el3_stackTrace) then begin
      inc(stackTraceCount);
      if stackTraceCount>30 then exit;
    end;
    for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then outAdapter[i]^.errorOut(thisErrorLevel,errorMessage,errorLocation);
  end;

PROCEDURE raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR i:byte;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el3_evalError] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el3_evalError];
    hasMessageOfType[mt_el3_evalError]:=true;
    for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then outAdapter[i]^.errorOut(mt_el3_evalError,errorMessage,errorLocation);
  end;

PROCEDURE raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR i:byte;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el2_warning] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el2_warning];
    hasMessageOfType[mt_el2_warning]:=true;
    for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then outAdapter[i]^.errorOut(mt_el2_warning,errorMessage,errorLocation);
  end;

PROCEDURE raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR i:byte;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el1_note] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el1_note];
    hasMessageOfType[mt_el1_note]:=true;
    for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then outAdapter[i]^.errorOut(mt_el1_note,errorMessage,errorLocation);
  end;

PROCEDURE printOut(CONST s:T_arrayOfString);
  VAR i:byte;
  begin
    for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then outAdapter[i]^.printOut(s);
  end;

PROCEDURE clearPrint;
  VAR i:byte;
  begin
    for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then outAdapter[i]^.clearConsole;
  end;

FUNCTION noErrors: boolean; inline;
  begin
    result:=maxErrorLevel<3;
  end;

PROCEDURE haltEvaluation;
  begin
    raiseCustomMessage(mt_el5_haltMessageReceived, '', C_nilTokenLocation);
  end;

PROCEDURE setDefaultCallbacks;
  VAR i:longint;
  begin
    for i:=0 to length(outAdapter)-1 do outAdapter[i]:=nil;
    outAdapter[0]:=@consoleOutAdapter;
  end;

FUNCTION someEchoDeclaration:boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=0 to length(outAdapter)-1 do if (outAdapter[i]<>nil) and outAdapter[i]^.outputBehaviour.doEchoDeclaration then exit(true);
  end;

FUNCTION someEchoInput:boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=0 to length(outAdapter)-1 do if (outAdapter[i]<>nil) and outAdapter[i]^.outputBehaviour.doEchoInput then exit(true);
  end;

FUNCTION someEchoOutput:boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=0 to length(outAdapter)-1 do if (outAdapter[i]<>nil) and outAdapter[i]^.outputBehaviour.doShowExpressionOut then exit(true);
  end;

PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter);
  VAR i:byte;
  begin
    for i:=0 to length(outAdapter)-1 do if outAdapter[i]=nil then begin
      outAdapter[i]:=p;
      exit;
    end;
  end;

PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);
  VAR i:byte;
  begin
    for i:=0 to length(outAdapter)-1 do if outAdapter[i]=p then begin
      outAdapter[i]:=nil;
      exit;
    end;
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

PROCEDURE T_consoleOutAdapter.writeEcho(CONST messageType:T_messageType; CONST s: ansistring);
  begin
    case messageType of
      mt_echo_output:      if not(outputBehaviour.doShowExpressionOut) then exit;
      mt_echo_declaration: if not(outputBehaviour.doEchoDeclaration)   then exit;
      mt_echo_input:       if not(outputBehaviour.doEchoInput)         then exit;
      else exit;
    end;
    writeln(C_errorLevelTxt[messageType],s);
  end;

PROCEDURE T_consoleOutAdapter.printOut(CONST s: T_arrayOfString);
  VAR i:longint;
  begin
    for i:=0 to length(s)-1 do writeln(s[i]);
  end;

PROCEDURE T_consoleOutAdapter.errorOut(CONST messageType: T_messageType;
  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    if (C_errorLevelForMessageType[messageType]>=0) and (C_errorLevelForMessageType[messageType]<outputBehaviour.minErrorLevel)
    or (messageType in [mt_endOfEvaluation,mt_reloadRequired,mt_imageCreated])
    or (messageType=mt_timing_info) and not(outputBehaviour.doShowTimingInfo) then exit;
    if messageType in [mt_debug_step,mt_el1_note,mt_el2_warning,mt_el3_evalError,mt_el3_noMatchingMain, mt_el4_parsingError,mt_el5_systemError,mt_el5_haltMessageReceived]
    then writeln(stdErr, C_errorLevelTxt[messageType],ansistring(errorLocation),' ', errorMessage)
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

PROCEDURE T_collectingOutAdapter.errorOut(CONST messageType: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR msg:T_storedMessage;
  begin
    if (C_errorLevelForMessageType[messageType]>=0) and (C_errorLevelForMessageType[messageType]<outputBehaviour.minErrorLevel)
    or (messageType=mt_timing_info) and not(outputBehaviour.doShowTimingInfo) then exit;
    system.enterCriticalSection(cs);
    msg.messageType:=messageType;
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
