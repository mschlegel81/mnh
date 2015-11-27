UNIT mnh_out_adapters;

INTERFACE

USES mnh_constants, mnh_tokLoc, myGenerics,mySys,sysutils,myStringUtil{$ifdef fullVersion},mnh_plotData{$endif};

TYPE
  T_storedMessage = record
    messageType : T_messageType;
    simpleMessage: ansistring;
    multiMessage: T_arrayOfString;
    location: T_tokenLocation;
  end;
  T_storedMessages = array of T_storedMessage;

  T_outputBehaviour= record
    doEchoInput: boolean;
    doEchoDeclaration: boolean;
    doShowExpressionOut: boolean;
    doShowTimingInfo: boolean;
    minErrorLevel: shortint;
  end;

  T_adapterType=(at_unknown,at_console,at_textFile,at_htmlFile,at_gui);

  P_abstractOutAdapter = ^T_abstractOutAdapter;
  T_abstractOutAdapter = object
    adapterType:T_adapterType;
    outputBehaviour:T_outputBehaviour;
    CONSTRUCTOR create(CONST typ:T_adapterType);
    DESTRUCTOR destroy; virtual; abstract;
    PROCEDURE clearConsole; virtual; abstract;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual; abstract;
    PROCEDURE messageOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual; abstract;
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;
  T_consoleOutAdapter = object(T_abstractOutAdapter)
    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
    PROCEDURE clearConsole; virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE messageOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
  end;

  P_collectingOutAdapter = ^T_collectingOutAdapter;
  T_collectingOutAdapter = object(T_abstractOutAdapter)
    storedMessages:T_storedMessages;
    cs:TRTLCriticalSection;
    CONSTRUCTOR create(CONST typ:T_adapterType);
    DESTRUCTOR destroy; virtual;
    PROCEDURE clearConsole; virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE messageOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
    PROCEDURE appendSingleMessage(CONST message:T_storedMessage); virtual;
    PROCEDURE clearMessages;
  end;

  P_textFileOutAdapter = ^T_textFileOutAdapter;

  { T_textFileOutAdapter }

  T_textFileOutAdapter = object(T_collectingOutAdapter) //TYPE 1
    lastFileFlushTime:double;
    outputFileName:ansistring;
    longestLineUpToNow:longint;
    lastWasEndOfEvaluation:boolean;
    CONSTRUCTOR create(CONST fileName:ansistring);
    DESTRUCTOR destroy; virtual;
    PROCEDURE appendSingleMessage(CONST message: T_storedMessage); virtual;
    PROCEDURE flush;
  end;

  P_adapters=^T_adapters;

  { T_adapters }

  T_adapters=object
    private
      stackTraceCount:longint;
      errorCount:longint;
      maxErrorLevel: shortint;
      adapter:array of record ad:P_abstractOutAdapter; doDestroy:boolean; end;
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
      FUNCTION  getOutputBehaviour              : T_outputBehaviour;
      PROCEDURE setOutputBehaviour  (CONST value: T_outputBehaviour);
    public
      hasMessageOfType:array[T_messageType] of boolean;
      {$ifdef fullVersion}
      currentlyDebugging:boolean;
      plot:T_plot;
      {$endif}
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROPERTY doEchoInput        : boolean  read getEchoInput         write setEchoInput        ;
      PROPERTY doEchoDeclaration  : boolean  read getEchoDeclaration   write setEchoDeclaration  ;
      PROPERTY doShowExpressionOut: boolean  read getShowExpressionOut write setShowExpressionOut;
      PROPERTY doShowTimingInfo   : boolean  read getShowTimingInfo    write setShowTimingInfo   ;
      PROPERTY minErrorLevel      : shortint read getMinErrorLevel     write setMinErrorLevel    ;
      PROPERTY outputBehaviour : T_outputBehaviour read getOutputBehaviour write setOutputBehaviour;

      PROCEDURE clearErrors;
      PROCEDURE raiseCustomMessage(CONST thisErrorLevel: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
      {$ifdef fullVersion}
      PROCEDURE raiseDebugMessage(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
      {$endif}
      PROCEDURE raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
      PROCEDURE raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
      PROCEDURE raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
      PROCEDURE printOut(CONST s:T_arrayOfString);
      PROCEDURE clearPrint;
      PROCEDURE ClearAll;
      FUNCTION noErrors: boolean; inline;
      PROCEDURE haltEvaluation;

      PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter; CONST destroyIt:boolean);
      PROCEDURE addConsoleOutAdapter;
      PROCEDURE addFileOutAdapter(CONST fileName:ansistring);
      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);

      FUNCTION adapterCount:longint;
      FUNCTION getAdapter(CONST index:longint):P_abstractOutAdapter;
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

FUNCTION defaultFormatting(CONST message:T_storedMessage):ansistring;
FUNCTION defaultFormatting(CONST messageType : T_messageType; CONST message: ansistring; CONST location: T_tokenLocation):ansistring;
IMPLEMENTATION
FUNCTION defaultFormatting(CONST message: T_storedMessage): ansistring;
  begin
    with message do if (length(simpleMessage)=0) and (length(multiMessage)>0)
    then result:=defaultFormatting(messageType,join(multiMessage,C_lineBreakChar),location)
    else result:=defaultFormatting(messageType,simpleMessage,location);
  end;

FUNCTION defaultFormatting(CONST messageType : T_messageType; CONST message: ansistring; CONST location: T_tokenLocation):ansistring;
  begin
    case messageType of
      mt_printline: result:=message;
      mt_el1_note,mt_el2_warning,mt_el3_evalError,mt_el3_noMatchingMain, mt_el4_parsingError,mt_el5_systemError,mt_el5_haltMessageReceived:
           result:=C_errorLevelTxt[messageType]+ansistring(location)+' '+message;
      else result:=C_errorLevelTxt[messageType]+' '+message;
    end;
  end;

CONSTRUCTOR T_textFileOutAdapter.create(CONST fileName: ansistring);
  begin
    inherited create(at_textFile);
    lastWasEndOfEvaluation:=true;
    longestLineUpToNow:=0;
    outputFileName:=expandFileName(fileName);
    lastFileFlushTime:=now;
    with outputBehaviour do begin
      doEchoDeclaration  :=true;
      doEchoInput        :=true;
      doShowExpressionOut:=true;
      doShowTimingInfo   :=true;
      minErrorLevel      :=2;
    end;
  end;

DESTRUCTOR T_textFileOutAdapter.destroy;
  begin messageOut(mt_endOfEvaluation,'',C_nilTokenLocation); flush; inherited destroy; end;

PROCEDURE T_textFileOutAdapter.appendSingleMessage(CONST message: T_storedMessage);
  begin
    if (message.messageType<>mt_clearConsole) then inherited appendSingleMessage(message);
    with storedMessages[length(storedMessages)-1] do if messageType in [mt_debug_step,mt_el3_stackTrace] then simpleMessage:=replaceAll(simpleMessage,#28,' ');
    if (message.messageType in [mt_endOfEvaluation, mt_clearConsole]) or (now-lastFileFlushTime>1/(24*60*60)) then flush;
  end;

PROCEDURE T_textFileOutAdapter.flush;
  VAR handle:text;
      i,j:longint;
  PROCEDURE myWrite(CONST s:ansistring);
    begin
      if length(s)>=longestLineUpToNow then longestLineUpToNow:=length(s);
      writeln(handle,s);
    end;

  begin
    if length(storedMessages)=0 then exit;
    assign(handle,outputFileName);
    if fileExists(outputFileName)
    then system.append(handle)
    else rewrite(handle);
    for i:=0 to length(storedMessages)-1 do begin
      with storedMessages[i] do begin
        case messageType of
          mt_clearConsole, mt_reloadRequired{$ifdef fullVersion},mt_plotFileCreated,mt_plotCreatedWithDeferredDisplay,mt_plotCreatedWithInstantDisplay,mt_plotSettingsChanged{$endif}: begin end;
          mt_endOfEvaluation: if not(lastWasEndOfEvaluation) then writeln(handle,StringOfChar('=',longestLineUpToNow));
          mt_echo_input:       if outputBehaviour.doEchoInput         then myWrite(C_errorLevelTxt[messageType]+simpleMessage);
          mt_echo_output:      if outputBehaviour.doShowExpressionOut then myWrite(C_errorLevelTxt[messageType]+simpleMessage);
          mt_echo_declaration: if outputBehaviour.doEchoDeclaration   then myWrite(C_errorLevelTxt[messageType]+simpleMessage);
          mt_timing_info:      if outputBehaviour.doShowTimingInfo    then myWrite(C_errorLevelTxt[messageType]+simpleMessage);
          mt_printline: for j:=0 to length(multiMessage)-1 do myWrite(multiMessage[j]);
          else if (C_errorLevelForMessageType[messageType]>=0) and (C_errorLevelForMessageType[messageType]<outputBehaviour.minErrorLevel) then
            myWrite(C_errorLevelTxt[messageType]+ansistring(location)+' '+ simpleMessage);
        end;
      end;
      lastWasEndOfEvaluation:=storedMessages[i].messageType=mt_endOfEvaluation;
    end;
    close(handle);
    clearMessages;
    lastFileFlushTime:=now;
  end;

FUNCTION T_adapters.getEchoInput: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i].ad^.outputBehaviour.doEchoInput then exit(true);
    result:=false;
  end;

PROCEDURE T_adapters.setEchoInput(CONST value: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i].ad^.outputBehaviour.doEchoInput:=value;
  end;

FUNCTION T_adapters.getEchoDeclaration: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i].ad^.outputBehaviour.doEchoDeclaration then exit(true);
    result:=false;
  end;

PROCEDURE T_adapters.setEchoDeclaration(CONST value: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i].ad^.outputBehaviour.doEchoDeclaration:=value;
  end;

FUNCTION T_adapters.getShowExpressionOut: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i].ad^.outputBehaviour.doShowExpressionOut then exit(true);
    result:=false;
  end;

PROCEDURE T_adapters.setShowExpressionOut(CONST value: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i].ad^.outputBehaviour.doShowExpressionOut:=value;
  end;

FUNCTION T_adapters.getShowTimingInfo: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i].ad^.outputBehaviour.doShowTimingInfo then exit(true);
    result:=false;
  end;

PROCEDURE T_adapters.setShowTimingInfo(CONST value: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i].ad^.outputBehaviour.doShowTimingInfo:=value;
  end;

FUNCTION T_adapters.getMinErrorLevel: shortint;
  VAR i:longint;
  begin
    result:=100;
    for i:=0 to length(adapter)-1 do if adapter[i].ad^.outputBehaviour.minErrorLevel<result then result:=adapter[i].ad^.outputBehaviour.minErrorLevel;
  end;

PROCEDURE T_adapters.setMinErrorLevel(CONST value: shortint);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i].ad^.outputBehaviour.minErrorLevel:=value;
  end;

FUNCTION T_adapters.getOutputBehaviour: T_outputBehaviour;
  begin
    result.doEchoDeclaration:=doEchoDeclaration;
    result.doEchoInput:=doEchoInput;
    result.doShowExpressionOut:=doShowExpressionOut;
    result.doShowTimingInfo:=doShowTimingInfo;
    result.minErrorLevel:=minErrorLevel;
  end;

PROCEDURE T_adapters.setOutputBehaviour(CONST value: T_outputBehaviour);
  begin
    doEchoDeclaration:=value.doEchoDeclaration;
    doEchoInput:=value.doEchoInput;
    doShowExpressionOut:=value.doShowExpressionOut;
    doShowTimingInfo:=value.doShowTimingInfo;
    minErrorLevel:=value.minErrorLevel;
  end;

CONSTRUCTOR T_adapters.create;
  begin
    {$ifdef fullVersion}
    currentlyDebugging:=false;
    plot.createWithDefaults;
    {$endif}
    setLength(adapter,0);
  end;

DESTRUCTOR T_adapters.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i].doDestroy then dispose(adapter[i].ad,destroy);
    setLength(adapter,0);
    {$ifdef fullVersion}
    plot.destroy;
    {$endif}
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
    errorCount:=0;
  end;

PROCEDURE T_adapters.raiseCustomMessage(CONST thisErrorLevel: T_messageType;
  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[thisErrorLevel] then
       maxErrorLevel:=C_errorLevelForMessageType[thisErrorLevel];
    hasMessageOfType[thisErrorLevel]:=true;
    if (thisErrorLevel=mt_el3_stackTrace) then begin
      inc(stackTraceCount);
      if stackTraceCount>30 then exit;
    end;
    if (thisErrorLevel in [mt_el3_evalError,mt_el3_noMatchingMain,mt_el4_parsingError,mt_el5_haltMessageReceived,mt_el5_systemError]) then begin
      inc(errorCount);
      if errorCount>30 then exit;
    end;
    for i:=0 to length(adapter)-1 do adapter[i].ad^.messageOut(thisErrorLevel,errorMessage,errorLocation);
  end;

{$ifdef fullVersion}
PROCEDURE T_adapters.raiseDebugMessage(CONST errorMessage: ansistring;
  CONST errorLocation: T_tokenLocation);
  VAR i:longint;
  begin
    hasMessageOfType[mt_debug_step]:=true;
    currentlyDebugging:=true;
    for i:=0 to length(adapter)-1 do adapter[i].ad^.messageOut(mt_debug_step,errorMessage,errorLocation);
  end;
{$endif}

PROCEDURE T_adapters.raiseError(CONST errorMessage: ansistring;
  CONST errorLocation: T_tokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el3_evalError] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el3_evalError];
    hasMessageOfType[mt_el3_evalError]:=true;
    for i:=0 to length(adapter)-1 do adapter[i].ad^.messageOut(mt_el3_evalError,errorMessage,errorLocation);
  end;

PROCEDURE T_adapters.raiseWarning(CONST errorMessage: ansistring;
  CONST errorLocation: T_tokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el2_warning] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el2_warning];
    hasMessageOfType[mt_el2_warning]:=true;
    for i:=0 to length(adapter)-1 do adapter[i].ad^.messageOut(mt_el2_warning,errorMessage,errorLocation);
  end;

PROCEDURE T_adapters.raiseNote(CONST errorMessage: ansistring;
  CONST errorLocation: T_tokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el1_note] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el1_note];
    hasMessageOfType[mt_el1_note]:=true;
    for i:=0 to length(adapter)-1 do adapter[i].ad^.messageOut(mt_el1_note,errorMessage,errorLocation);
  end;

PROCEDURE T_adapters.printOut(CONST s: T_arrayOfString);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i].ad^.printOut(s);
  end;

PROCEDURE T_adapters.clearPrint;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do adapter[i].ad^.clearConsole;
  end;

PROCEDURE T_adapters.ClearAll;
  begin
    clearPrint;
    clearErrors;
  end;

FUNCTION T_adapters.noErrors: boolean;
  begin
    result:=maxErrorLevel<3;
  end;

PROCEDURE T_adapters.haltEvaluation;
  begin
    raiseCustomMessage(mt_el5_haltMessageReceived, '', C_nilTokenLocation);
  end;

PROCEDURE T_adapters.addOutAdapter(CONST p: P_abstractOutAdapter;
  CONST destroyIt: boolean);
  begin
    setLength(adapter,length(adapter)+1);
    adapter[length(adapter)-1].ad:=p;
    adapter[length(adapter)-1].doDestroy:=destroyIt;
  end;

PROCEDURE T_adapters.addConsoleOutAdapter;
  VAR consoleOutAdapter:P_consoleOutAdapter;
  begin
    new(consoleOutAdapter,create);
    addOutAdapter(consoleOutAdapter,true);
  end;

PROCEDURE T_adapters.addFileOutAdapter(CONST fileName: ansistring);
  VAR fileOutAdapter:P_textFileOutAdapter;
  begin
    new(fileOutAdapter,create(fileName));
    addOutAdapter(fileOutAdapter,true);
  end;

PROCEDURE T_adapters.removeOutAdapter(CONST p: P_abstractOutAdapter);
  VAR i,j:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i].ad=p then begin
      for j:=i to length(adapter)-2 do adapter[j]:=adapter[j+1];
      setLength(adapter,length(adapter)-1);
      exit;
    end;
  end;

FUNCTION T_adapters.adapterCount: longint;
  begin
    result:=length(adapter);
  end;

FUNCTION T_adapters.getAdapter(CONST index: longint): P_abstractOutAdapter;
  begin
    result:=adapter[index].ad;
  end;

CONSTRUCTOR T_abstractOutAdapter.create(CONST typ:T_adapterType);
  begin
    adapterType:=typ;
    with outputBehaviour do begin
      doEchoDeclaration:=false;
      doEchoInput:=false;
      doShowExpressionOut:=false;
      doShowTimingInfo:=false;
      minErrorLevel:=3;
    end;
  end;

CONSTRUCTOR T_consoleOutAdapter.create;
  begin
    inherited create(at_console);
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
      compound:ansistring;
  begin
    if (length(s)>0) and (s[0]=C_formFeedChar) then begin
      mySys.clearConsole;
      if length(s)>1 then begin
        compound:=s[1];
        for i:=2 to length(s)-1 do compound:=compound+LineEnding+s[i];
        writeln(compound);
      end;
    end else if length(s)>0 then begin
      compound:=s[0];
      for i:=1 to length(s)-1 do compound:=compound+LineEnding+s[i];
      writeln(compound);
    end;
  end;

PROCEDURE T_consoleOutAdapter.messageOut(CONST messageType: T_messageType;
  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    if (C_errorLevelForMessageType[messageType]>=0) and (C_errorLevelForMessageType[messageType]<outputBehaviour.minErrorLevel)
    or (messageType in [mt_endOfEvaluation,mt_reloadRequired{$ifdef fullVersion},mt_plotFileCreated,mt_plotCreatedWithDeferredDisplay,mt_plotCreatedWithInstantDisplay,mt_plotSettingsChanged{$endif}])
    or (messageType=mt_timing_info) and not(outputBehaviour.doShowTimingInfo)
    or (messageType=mt_echo_declaration) and not(outputBehaviour.doEchoDeclaration)
    or (messageType=mt_echo_input) and not(outputBehaviour.doEchoInput)
    or (messageType=mt_echo_output) and not(outputBehaviour.doShowExpressionOut) then exit;
    case messageType of
      mt_debug_step,mt_el3_stackTrace:
        writeln(stdErr, C_errorLevelTxt[messageType],ansistring(errorLocation),' ',replaceAll(errorMessage,#28,' '));
      mt_el1_note,mt_el2_warning,mt_el3_evalError,mt_el3_noMatchingMain, mt_el4_parsingError,mt_el5_systemError,mt_el5_haltMessageReceived:
        writeln(stdErr, C_errorLevelTxt[messageType],ansistring(errorLocation),' ',           errorMessage         );
      else
        writeln(stdErr, C_errorLevelTxt[messageType],' ', errorMessage);
    end;
  end;

CONSTRUCTOR T_collectingOutAdapter.create(CONST typ:T_adapterType);
  begin
    inherited create(typ);
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

PROCEDURE T_collectingOutAdapter.messageOut(CONST messageType: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
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
  nullAdapter.create;
  {$ifdef fullVersion}
  stepper.create;
  {$endif}

FINALIZATION
  {$ifdef fullVersion}
  stepper.destroy;
  {$endif}
  nullAdapter.destroy;
end.
