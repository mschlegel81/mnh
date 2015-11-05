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

  T_outputBehaviour= record
    doEchoInput: boolean;
    doEchoDeclaration: boolean;
    doShowExpressionOut: boolean;
    doShowTimingInfo: boolean;
    minErrorLevel: shortint;
  end;

  P_abstractOutAdapter = ^T_abstractOutAdapter;
  T_abstractOutAdapter = object
    outputBehaviour:T_outputBehaviour;
    CONSTRUCTOR create;
    PROCEDURE clearConsole; virtual; abstract;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual; abstract;
    PROCEDURE errorOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual; abstract;
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;
  T_consoleOutAdapter = object(T_abstractOutAdapter)
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
  end;

  P_collectingOutAdapter = ^T_collectingOutAdapter;
  T_collectingOutAdapter = object(T_abstractOutAdapter)
    storedMessages:array of T_storedMessage;
    cs:TRTLCriticalSection;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
    PROCEDURE appendSingleMessage(CONST message:T_storedMessage); virtual;
    PROCEDURE clearMessages;
  end;

  P_adapters=^T_adapters;

  { T_adapters }

  T_adapters=object
    private
      stackTraceCount:longint;
      maxErrorLevel: shortint;
      adapter:array of P_abstractOutAdapter;
      FUNCTION  getEchoInput                    : boolean;
      PROCEDURE setEchoInput        (CONST value: boolean );
      FUNCTION  getEchoDeclaration              : boolean;
      PROCEDURE setEchoDeclaration  (CONST value: boolean );
      FUNCTION  getShowExpressionOut            : boolean;
      PROCEDURE setShowExpressionOut(CONST value: boolean );
      FUNCTION  getShowTimingInfo               : boolean;
      PROCEDURE setShowTimingInfo   (CONST value: boolean );
      FUNCTION  getMinErrorLevel                : shortint;
      PROCEDURE setMinErrorLevel    (CONST value: shortint);
    public
      hasMessageOfType:array[T_messageType] of boolean;

      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROPERTY doEchoInput        : boolean  read getEchoInput         write setEchoInput        ;
      PROPERTY doEchoDeclaration  : boolean  read getEchoDeclaration   write setEchoDeclaration  ;
      PROPERTY doShowExpressionOut: boolean  read getShowExpressionOut write setShowExpressionOut;
      PROPERTY doShowTimingInfo   : boolean  read getShowTimingInfo    write setShowTimingInfo   ;
      PROPERTY minErrorLevel      : shortint read getMinErrorLevel     write setMinErrorLevel    ;


      PROCEDURE clearErrors;
      PROCEDURE raiseCustomMessage(CONST thisErrorLevel: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
      PROCEDURE raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
      PROCEDURE raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
      PROCEDURE raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
      PROCEDURE printOut(CONST s:T_arrayOfString);
      PROCEDURE clearPrint;
      FUNCTION noErrors: boolean; inline;
      PROCEDURE haltEvaluation;

      PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter);
      PROCEDURE addConsoleOutAdapter;
      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);
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
    PROCEDURE onAbort;
  end;
  {$endif}

VAR
  {$ifdef fullVersion}stepper:T_stepper;{$endif}
  nullAdapter:T_adapters;

IMPLEMENTATION
VAR consoleOutAdapter:T_consoleOutAdapter;

FUNCTION T_adapters.getEchoInput: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i]^.outputBehaviour.doEchoInput then exit(true);
    result:=false;
  end;

PROCEDURE T_adapters.setEchoInput(CONST value: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i]^.outputBehaviour.doEchoInput:=value;
  end;

FUNCTION T_adapters.getEchoDeclaration: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i]^.outputBehaviour.doEchoDeclaration then exit(true);
    result:=false;
  end;

PROCEDURE T_adapters.setEchoDeclaration(CONST value: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i]^.outputBehaviour.doEchoDeclaration:=value;
  end;

FUNCTION T_adapters.getShowExpressionOut: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i]^.outputBehaviour.doShowExpressionOut then exit(true);
    result:=false;
  end;

PROCEDURE T_adapters.setShowExpressionOut(CONST value: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i]^.outputBehaviour.doShowExpressionOut:=value;
  end;

FUNCTION T_adapters.getShowTimingInfo: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i]^.outputBehaviour.doShowTimingInfo then exit(true);
    result:=false;
  end;

PROCEDURE T_adapters.setShowTimingInfo(CONST value: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i]^.outputBehaviour.doShowTimingInfo:=value;
  end;

FUNCTION T_adapters.getMinErrorLevel: shortint;
  VAR i:longint;
  begin
    result:=100;
    for i:=0 to length(adapter)-1 do if adapter[i]^.outputBehaviour.minErrorLevel<result then result:=adapter[i]^.outputBehaviour.minErrorLevel;
  end;

PROCEDURE T_adapters.setMinErrorLevel(CONST value: shortint);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i]^.outputBehaviour.minErrorLevel:=value;
  end;

CONSTRUCTOR T_adapters.create;
  begin
    setLength(adapter,0);
  end;

DESTRUCTOR T_adapters.destroy;
  begin
    setLength(adapter,0);
  end;

PROCEDURE T_adapters.clearErrors;
  VAR mt:T_messageType;
  begin
    for mt:=low(T_messageType) to high(T_messageType) do begin
      hasMessageOfType[mt]:=false;
      if   maxErrorLevel>=C_errorLevelForMessageType[mt]
      then maxErrorLevel:=C_errorLevelForMessageType[mt]-1;
    end;
    stackTraceCount:=0;
  end;

PROCEDURE T_adapters.raiseCustomMessage(CONST thisErrorLevel: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[thisErrorLevel] then
       maxErrorLevel:=C_errorLevelForMessageType[thisErrorLevel];
    hasMessageOfType[thisErrorLevel]:=true;
    if (thisErrorLevel=mt_el3_stackTrace) then begin
      inc(stackTraceCount);
      if stackTraceCount>30 then exit;
    end;
    for i:=0 to length(adapter)-1 do adapter[i]^.errorOut(thisErrorLevel,errorMessage,errorLocation);
  end;

PROCEDURE T_adapters.raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el3_evalError] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el3_evalError];
    hasMessageOfType[mt_el3_evalError]:=true;
    for i:=0 to length(adapter)-1 do adapter[i]^.errorOut(mt_el3_evalError,errorMessage,errorLocation);
  end;

PROCEDURE T_adapters.raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el2_warning] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el2_warning];
    hasMessageOfType[mt_el2_warning]:=true;
    for i:=0 to length(adapter)-1 do adapter[i]^.errorOut(mt_el2_warning,errorMessage,errorLocation);
  end;

PROCEDURE T_adapters.raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el1_note] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el1_note];
    hasMessageOfType[mt_el1_note]:=true;
    for i:=0 to length(adapter)-1 do adapter[i]^.errorOut(mt_el1_note,errorMessage,errorLocation);
  end;

PROCEDURE T_adapters.printOut(CONST s: T_arrayOfString);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i]^.printOut(s);
  end;

PROCEDURE T_adapters.clearPrint;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i]^.clearConsole;
  end;

FUNCTION T_adapters.noErrors: boolean;
  begin
    result:=maxErrorLevel<3;
  end;

PROCEDURE T_adapters.haltEvaluation;
  begin
    raiseCustomMessage(mt_el5_haltMessageReceived, '', C_nilTokenLocation);
  end;

PROCEDURE T_adapters.addOutAdapter(CONST p: P_abstractOutAdapter);
  begin
    setLength(adapter,length(adapter)+1);
    adapter[length(adapter)-1]:=p;
  end;

PROCEDURE T_adapters.addConsoleOutAdapter;
  begin
    addOutAdapter(@consoleOutAdapter);
  end;

PROCEDURE T_adapters.removeOutAdapter(CONST p: P_abstractOutAdapter);
  VAR i,j:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i]=p then begin
      for j:=i to length(adapter)-2 do adapter[j]:=adapter[j+1];
      setLength(adapter,length(adapter)-1);
      exit;
    end;
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
    or (messageType=mt_timing_info) and not(outputBehaviour.doShowTimingInfo)
    or (messageType=mt_echo_declaration) and not(outputBehaviour.doEchoDeclaration)
    or (messageType=mt_echo_input) and not(outputBehaviour.doEchoInput)
    or (messageType=mt_echo_output) and not(outputBehaviour.doShowExpressionOut) then exit;
    if messageType in [mt_debug_step,mt_el1_note,mt_el2_warning,mt_el3_evalError,mt_el3_noMatchingMain, mt_el4_parsingError,mt_el5_systemError,mt_el5_haltMessageReceived]
    then writeln(stdErr, C_errorLevelTxt[messageType],ansistring(errorLocation),' ', errorMessage)
    else writeln(stdErr,                              ansistring(errorLocation),' ', errorMessage);
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
    or (messageType=mt_timing_info) and not(outputBehaviour.doShowTimingInfo)
    or (messageType=mt_echo_declaration) and not(outputBehaviour.doEchoDeclaration)
    or (messageType=mt_echo_input) and not(outputBehaviour.doEchoInput)
    or (messageType=mt_echo_output) and not(outputBehaviour.doShowExpressionOut) then exit;
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

PROCEDURE T_stepper.onAbort;
  begin
    signal:=ds_run;
  end;
{$endif}

INITIALIZATION
  consoleOutAdapter.create;
  nullAdapter.create;
  {$ifdef fullVersion}
  stepper.create;
  {$endif}

FINALIZATION
  {$ifdef fullVersion}
  stepper.destroy;
  {$endif}
  nullAdapter.destroy;
  consoleOutAdapter.destroy;
end.
