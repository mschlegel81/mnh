UNIT mnh_out_adapters;
INTERFACE

USES mnh_constants, mnh_tokLoc, myGenerics,mySys,sysutils,myStringUtil{$ifdef fullVersion},mnh_plotData,EpikTimer{$endif};

TYPE
  T_storedMessage = record
    messageType : T_messageType;
    simpleMessage: ansistring;
    multiMessage: T_arrayOfString;
    location: T_searchTokenLocation;
  end;
  T_storedMessages = array of T_storedMessage;

  T_outputBehaviour= record
    doEchoInput: boolean;
    doEchoDeclaration: boolean;
    doShowExpressionOut: boolean;
    doShowTimingInfo: boolean;
    minErrorLevel: shortint;
  end;

  T_adapterType=(at_unknown,at_console,at_textFile,at_htmlFile,at_gui,at_sandboxAdapter);

  P_abstractOutAdapter = ^T_abstractOutAdapter;
  T_abstractOutAdapter = object
    adapterType:T_adapterType;
    outputBehaviour:T_outputBehaviour;
    CONSTRUCTOR create(CONST typ:T_adapterType);
    DESTRUCTOR destroy; virtual; abstract;
    PROCEDURE clearConsole; virtual; abstract;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual; abstract;
    PROCEDURE messageOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation); virtual; abstract;
    PROCEDURE append(CONST message:T_storedMessage); virtual;
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;
  T_consoleOutAdapter = object(T_abstractOutAdapter)
    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
    PROCEDURE clearConsole; virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE messageOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation); virtual;
  end;

  P_collectingOutAdapter = ^T_collectingOutAdapter;
  T_collectingOutAdapter = object(T_abstractOutAdapter)
    storedMessages:T_storedMessages;
    cs:TRTLCriticalSection;
    CONSTRUCTOR create(CONST typ:T_adapterType);
    DESTRUCTOR destroy; virtual;
    PROCEDURE clearConsole; virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE messageOut(CONST messageType:T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation); virtual;
    PROCEDURE append(CONST message:T_storedMessage); virtual;
    PROCEDURE clearMessages;
  end;

  P_textFileOutAdapter = ^T_textFileOutAdapter;
  T_textFileOutAdapter = object(T_collectingOutAdapter)
    lastFileFlushTime:double;
    outputFileName:ansistring;
    longestLineUpToNow:longint;
    lastWasEndOfEvaluation:boolean;
    CONSTRUCTOR create(CONST fileName:ansistring);
    DESTRUCTOR destroy; virtual;
    PROCEDURE append(CONST message: T_storedMessage); virtual;
    PROCEDURE flush;
  end;

  P_adapters=^T_adapters;
  T_adapters=object
    private
      nextAdapterId:longint;
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
      PROCEDURE raiseCustomMessage(CONST thisErrorLevel: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseCustomMessage(CONST message:T_storedMessage);
      PROCEDURE raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE printOut(CONST s:T_arrayOfString);
      PROCEDURE clearPrint;
      PROCEDURE ClearAll;
      PROCEDURE stopEvaluation;
      FUNCTION noErrors: boolean; inline;
      PROCEDURE updateErrorlevel;
      {$ifdef fullVersion}FUNCTION hasNeedGUIerror:boolean;{$endif}
      PROCEDURE haltEvaluation;
      PROCEDURE logEndOfEvaluation;
      PROCEDURE raiseSystemError(CONST errorMessage: ansistring);

      PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter; CONST destroyIt:boolean);
      PROCEDURE addConsoleOutAdapter;
      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);

      FUNCTION adapterCount:longint;
      FUNCTION getAdapter(CONST index:longint):P_abstractOutAdapter;

      FUNCTION collectingClone:P_adapters;
      FUNCTION copyDataFromCollectingCloneDisposing(VAR clone:P_adapters; CONST errorCase:boolean):T_storedMessages;

      PROCEDURE setExitCode;
  end;

  {$ifdef fullVersion}
  T_timerEntry=record
    timer:TEpikTimer;
    into:longint;
  end;

  T_TimerMap=specialize G_stringKeyMap<T_timerEntry>;

  T_stepper=object
    private
      timerMap:T_TimerMap;

      waitingForGUI:boolean;

      breakpoints:array of T_searchTokenLocation;
      state:(breakSoonest,
             breakOnLineChange,
             breakOnStepOut,
             breakOnStepIn,
             runUntilBreakpoint,
             dontBreakAtAll);
      stepLevel:longint;
      lineChanged:boolean;
      levelChanged:boolean;

      contextPointer:pointer;
      tokenPointer:pointer;
      stackPointer:pointer;

      currentLine:T_tokenLocation;
      currentLevel:longint;
      cs:TRTLCriticalSection;
      //Single instance. So this can be private.
      {$WARN 3018 OFF}{$WARN 3019 OFF}
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clearTimers;
      PROCEDURE stopAllTimers;
      PROCEDURE resumeAllTimers;
    public
      //To be called by evaluation-loop
      PROCEDURE stepping   (CONST location:T_tokenLocation; CONST pointerToFirst,pointerToContext,pointerToStack:pointer);
      PROCEDURE steppingIn (CONST functionId:ansistring);
      PROCEDURE steppingOut(CONST functionId:ansistring);

      //To be called by GUI
      PROCEDURE doStart(CONST continue:boolean);
      PROCEDURE clearBreakpoints;
      PROCEDURE addBreakpoint(CONST fileName:string; CONST line:longint);
      PROCEDURE doStepInto;
      PROCEDURE doStepOut;
      PROCEDURE doStep;
      PROCEDURE doMicrostep;
      PROCEDURE doStop;
      FUNCTION haltet:boolean;
      FUNCTION context:pointer;
      FUNCTION token:pointer;
      FUNCTION stack:pointer;
      FUNCTION loc:T_tokenLocation;
      PROCEDURE showTimeInfo(VAR adapters:T_adapters);
  end;
  {$endif}

VAR
  {$ifdef fullVersion}
  stepper:T_stepper;
  currentlyDebugging:boolean=false;
  gui_started:boolean=false;
  {$endif}
  nullAdapter:T_adapters;

FUNCTION defaultFormatting(CONST message:T_storedMessage):ansistring;
FUNCTION defaultFormatting(CONST messageType : T_messageType; CONST message: ansistring; CONST location: T_searchTokenLocation):ansistring;
IMPLEMENTATION
FUNCTION defaultFormatting(CONST message: T_storedMessage): ansistring;
  begin
    with message do if (length(simpleMessage)=0) and (length(multiMessage)>0)
    then result:=defaultFormatting(messageType,join(multiMessage,C_lineBreakChar),location)
    else result:=defaultFormatting(messageType,simpleMessage,location);
  end;

FUNCTION defaultFormatting(CONST messageType : T_messageType; CONST message: ansistring; CONST location: T_searchTokenLocation):ansistring;
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
  begin flush; inherited destroy; end;

PROCEDURE T_textFileOutAdapter.append(CONST message: T_storedMessage);
  begin
    if (message.messageType<>mt_clearConsole) then inherited append(message);
    {$ifndef DEBUGMODE}
    //Debugmode: flush immediately
    if (message.messageType in [mt_endOfEvaluation, mt_clearConsole]) or (now-lastFileFlushTime>1/(24*60*60)) then
    {$endif}
    flush;
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
    try
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
            mt_timing_info:      if outputBehaviour.doShowTimingInfo    then myWrite(simpleMessage);
            mt_printline: for j:=0 to length(multiMessage)-1 do myWrite(multiMessage[j]);
            else if (C_errorLevelForMessageType[messageType]>=0) and (C_errorLevelForMessageType[messageType]<outputBehaviour.minErrorLevel) then
              myWrite(C_errorLevelTxt[messageType]+ansistring(location)+' '+ simpleMessage);
          end;
        end;
        lastWasEndOfEvaluation:=storedMessages[i].messageType=mt_endOfEvaluation;
      end;
      close(handle);
    except
    end;
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
    plot.createWithDefaults;
    {$endif}
    setLength(adapter,0);
    nextAdapterId:=0;
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

PROCEDURE T_adapters.raiseCustomMessage(CONST thisErrorLevel: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
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

PROCEDURE T_adapters.raiseCustomMessage(CONST message: T_storedMessage);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[message.messageType] then
       maxErrorLevel:=C_errorLevelForMessageType[message.messageType];
    hasMessageOfType[message.messageType]:=true;
    if (message.messageType=mt_el3_stackTrace) then begin
      inc(stackTraceCount);
      if stackTraceCount>30 then exit;
    end;
    if (message.messageType in [mt_el3_evalError,mt_el3_noMatchingMain,mt_el4_parsingError,mt_el5_haltMessageReceived,mt_el5_systemError]) then begin
      inc(errorCount);
      if errorCount>30 then exit;
    end;
    for i:=0 to length(adapter)-1 do adapter[i].ad^.append(message);
  end;

PROCEDURE T_adapters.raiseError(CONST errorMessage: ansistring;
  CONST errorLocation: T_searchTokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el3_evalError] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el3_evalError];
    hasMessageOfType[mt_el3_evalError]:=true;
    for i:=0 to length(adapter)-1 do adapter[i].ad^.messageOut(mt_el3_evalError,errorMessage,errorLocation);
  end;

PROCEDURE T_adapters.raiseWarning(CONST errorMessage: ansistring;
  CONST errorLocation: T_searchTokenLocation);
  VAR i:longint;
  begin
    if maxErrorLevel< C_errorLevelForMessageType[mt_el2_warning] then
       maxErrorLevel:=C_errorLevelForMessageType[mt_el2_warning];
    hasMessageOfType[mt_el2_warning]:=true;
    for i:=0 to length(adapter)-1 do adapter[i].ad^.messageOut(mt_el2_warning,errorMessage,errorLocation);
  end;

PROCEDURE T_adapters.raiseNote(CONST errorMessage: ansistring;
  CONST errorLocation: T_searchTokenLocation);
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

PROCEDURE T_adapters.stopEvaluation;
  begin
    hasMessageOfType[mt_el5_haltMessageQuiet]:=true;
    maxErrorLevel:=5;
  end;

FUNCTION T_adapters.noErrors: boolean;
  begin
    result:=(maxErrorLevel<3)
    {$ifdef fullVersion}
    and not(hasNeedGUIerror)
    {$endif};
  end;

PROCEDURE T_adapters.updateErrorlevel;
  VAR mt:T_messageType;
  begin
    maxErrorLevel:=0;
    for mt:=low(T_messageType) to high(T_messageType) do
    if (mt<>mt_el5_haltMessageQuiet) and
       (hasMessageOfType[mt]) and
       (C_errorLevelForMessageType[mt]>maxErrorLevel) then maxErrorLevel:=C_errorLevelForMessageType[mt];
  end;

{$ifdef fullVersion}
FUNCTION T_adapters.hasNeedGUIerror: boolean;
  VAR i:longint;
  begin
    if gui_started then exit(false);
    for i:=0 to length(   C_MESSAGE_TYPES_REQUIRING_GUI_STARTUP)-1 do
      if hasMessageOfType[C_MESSAGE_TYPES_REQUIRING_GUI_STARTUP[i]] then exit(true);
    result:=false;
  end;
{$endif}

CONST C_nilTokenLocation: T_searchTokenLocation = (fileName:'?'; line: 0; column: 0);
PROCEDURE T_adapters.haltEvaluation;
  begin
    raiseCustomMessage(mt_el5_haltMessageReceived, '', C_nilTokenLocation);
  end;

PROCEDURE T_adapters.logEndOfEvaluation;
  begin
    raiseCustomMessage(mt_endOfEvaluation,'',C_nilTokenLocation);
  end;

PROCEDURE T_adapters.raiseSystemError(CONST errorMessage: ansistring);
  begin
    raiseCustomMessage(mt_el5_systemError,errorMessage,C_nilTokenLocation);
  end;

PROCEDURE T_adapters.addOutAdapter(CONST p: P_abstractOutAdapter; CONST destroyIt: boolean);
  VAR oldBehavior:T_outputBehaviour;
  begin
    oldBehavior:=outputBehaviour;
    setLength(adapter,length(adapter)+1);
    adapter[length(adapter)-1].ad:=p;
    inc(nextAdapterId);
    adapter[length(adapter)-1].doDestroy:=destroyIt;
    outputBehaviour:=outputBehaviour;
  end;

PROCEDURE T_adapters.addConsoleOutAdapter;
  VAR consoleOutAdapter:P_consoleOutAdapter;
  begin
    new(consoleOutAdapter,create);
    addOutAdapter(consoleOutAdapter,true);
  end;

PROCEDURE T_adapters.removeOutAdapter(CONST p: P_abstractOutAdapter);
  VAR i,j:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i].ad=p then begin
      if adapter[i].doDestroy then dispose(adapter[i].ad,destroy);
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

FUNCTION T_adapters.collectingClone: P_adapters;
  VAR collector:P_collectingOutAdapter;
  begin
    new(result,create);
    new(collector,create(at_sandboxAdapter));
    result^.addOutAdapter(collector,true);
    result^.outputBehaviour:=outputBehaviour;
    {$ifdef fullVersion}
    result^.plot.CopyFrom(plot);
    {$endif}
  end;

FUNCTION T_adapters.copyDataFromCollectingCloneDisposing(VAR clone: P_adapters; CONST errorCase:boolean): T_storedMessages;
  VAR collector:P_collectingOutAdapter=nil;
      i:longint;
  PROCEDURE appendToResult;
    begin
      setLength(result,length(result)+1);
      result[length(result)-1]:=collector^.storedMessages[i];
    end;
  begin
    for i:=0 to length(clone^.adapter)-1 do
    if (collector=nil) and
       (clone^.adapter[i].ad^.adapterType=at_sandboxAdapter) then collector:=P_collectingOutAdapter(clone^.adapter[i].ad);
    setLength(result,0);
    {$ifdef fullVersion}
    if not(errorCase) and (clone^.hasMessageOfType[mt_plotFileCreated] or
                           clone^.hasMessageOfType[mt_plotCreatedWithDeferredDisplay] or
                           clone^.hasMessageOfType[mt_plotCreatedWithInstantDisplay] or
                           clone^.hasMessageOfType[mt_plotSettingsChanged]) then plot.CopyFrom(clone^.plot);
    {$endif}
    if (collector<>nil) then
    for i:=0 to length(collector^.storedMessages)-1 do case collector^.storedMessages[i].messageType of
      mt_el5_haltMessageReceived,
      mt_endOfEvaluation,
      mt_reloadRequired: begin appendToResult; raiseCustomMessage(collector^.storedMessages[i]); end;
      else begin
             if errorCase then appendToResult else begin
               raiseCustomMessage(collector^.storedMessages[i]);
             end;

      end;
    end;
    dispose(clone,destroy);
  end;

PROCEDURE T_adapters.setExitCode;
  CONST MAX_IGNORED_LEVEL=2;
  VAR mt:T_messageType;
      code:longint;
  begin
    code:=MAX_IGNORED_LEVEL;
    for mt:=low(T_messageType) to high(T_messageType) do
    if hasMessageOfType[mt] and (C_errorLevelForMessageType[mt]>code) then code:=C_errorLevelForMessageType[mt];
    if code>MAX_IGNORED_LEVEL then ExitCode:=code;
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

PROCEDURE T_abstractOutAdapter.append(CONST message:T_storedMessage);
  begin
    case message.messageType of
      mt_clearConsole: clearConsole;
      mt_printline: printOut(message.multiMessage);
      else messageOut(message.messageType,message.simpleMessage,message.location);
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

PROCEDURE T_consoleOutAdapter.messageOut(CONST messageType: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
  begin
    if (C_errorLevelForMessageType[messageType]>=0) and (C_errorLevelForMessageType[messageType]<outputBehaviour.minErrorLevel)
    or (messageType in [mt_endOfEvaluation,mt_reloadRequired{$ifdef fullVersion},mt_plotFileCreated,mt_plotCreatedWithDeferredDisplay,mt_plotCreatedWithInstantDisplay,mt_plotSettingsChanged{$endif}])
    or (messageType=mt_timing_info) and not(outputBehaviour.doShowTimingInfo)
    or (messageType=mt_echo_declaration) and not(outputBehaviour.doEchoDeclaration)
    or (messageType=mt_echo_input) and not(outputBehaviour.doEchoInput)
    or (messageType=mt_echo_output) and not(outputBehaviour.doShowExpressionOut) then exit;
    case messageType of
      mt_timing_info: writeln(stdErr,'',errorMessage);
      mt_el3_stackTrace:
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
    clearMessages;
    msg.messageType:=mt_clearConsole;
    append(msg);
  end;

PROCEDURE T_collectingOutAdapter.printOut(CONST s: T_arrayOfString);
  VAR msg:T_storedMessage;
  begin
    msg.messageType:=mt_printline;
    msg.multiMessage:=s;
    append(msg);
  end;

PROCEDURE T_collectingOutAdapter.messageOut(CONST messageType: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
  VAR msg:T_storedMessage;
  begin
    msg.messageType:=messageType;
    msg.simpleMessage:=errorMessage;
    msg.location:=errorLocation;
    append(msg);
  end;

PROCEDURE T_collectingOutAdapter.append(CONST message: T_storedMessage);
  begin
    with message do if (C_errorLevelForMessageType[messageType]>=0) and (C_errorLevelForMessageType[messageType]<outputBehaviour.minErrorLevel)
    or (messageType=mt_timing_info) and not(outputBehaviour.doShowTimingInfo)
    or (messageType=mt_echo_declaration) and not(outputBehaviour.doEchoDeclaration)
    or (messageType=mt_echo_input) and not(outputBehaviour.doEchoInput)
    or (messageType=mt_echo_output) and not(outputBehaviour.doShowExpressionOut) then exit;
    system.enterCriticalSection(cs);
    setLength(storedMessages,length(storedMessages)+1);
    storedMessages[length(storedMessages)-1]:=message;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.clearMessages;
  begin
    setLength(storedMessages,0);
  end;

{$ifdef fullVersion}
CONSTRUCTOR T_stepper.create;
  begin
    timerMap.create();
    system.initCriticalSection(cs);
    setLength(breakpoints,0);
    state:=breakSoonest;
    contextPointer:=nil;
    tokenPointer:=nil;
    stackPointer:=nil;
  end;

DESTRUCTOR T_stepper.destroy;
  begin
    clearTimers;
    timerMap.destroy;
    system.doneCriticalSection(cs);
  end;

PROCEDURE T_stepper.clearTimers;
  VAR v:array of T_timerEntry;
      i:longint;
  begin
    v:=timerMap.valueSet;
    for i:=0 to length(v)-1 do with v[i] do timer.destroy;
    timerMap.clear;
  end;

PROCEDURE T_stepper.stopAllTimers;
  VAR v:array of T_timerEntry;
      i:longint;
  begin
    v:=timerMap.valueSet;
    for i:=0 to length(v)-1 do with v[i] do if into>0 then timer.stop;
  end;

PROCEDURE T_stepper.resumeAllTimers;
  VAR v:array of T_timerEntry;
      i:longint;
  begin
    v:=timerMap.valueSet;
    for i:=0 to length(v)-1 do with v[i] do if into>0 then timer.start;
  end;

PROCEDURE T_stepper.stepping(CONST location: T_tokenLocation; CONST pointerToFirst, pointerToContext, pointerToStack: pointer);
  FUNCTION breakpointEncountered:boolean;
    VAR i:longint;
    begin
      for i:=0 to length(breakpoints)-1 do
        if (breakpoints[i].fileName=location.package^.getPath) and
           (breakpoints[i].line    =location.line            ) then exit(true);
      result:=false;
    end;

  begin
    system.enterCriticalSection(cs);
    if state=dontBreakAtAll then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    lineChanged:=lineChanged or (currentLine.package<>location.package) or (currentLine.line<>location.line);
    if (state=breakSoonest) or
       (state=breakOnStepIn) and (currentLevel>stepLevel) or
       (state=breakOnStepOut) and (currentLevel<stepLevel) or
      ((state=breakOnLineChange) and ((currentLevel<stepLevel) or (currentLevel=stepLevel) and (lineChanged and levelChanged))) or
      ((lineChanged or levelChanged or (currentLevel<>stepLevel)) and breakpointEncountered) then begin
      lineChanged:=false;
      levelChanged:=false;
      stepLevel:=currentLevel;
      currentLine:=location;
      contextPointer:=pointerToContext;
      tokenPointer:=pointerToFirst;
      stackPointer:=pointerToStack;
      stopAllTimers;
      waitingForGUI:=true;
      repeat
        system.leaveCriticalSection(cs);
        sleep(10);
        system.enterCriticalSection(cs);
      until not(waitingForGUI);
      resumeAllTimers;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.steppingIn(CONST functionId:ansistring);
  VAR t:T_timerEntry;
  begin
    system.enterCriticalSection(cs);
    levelChanged:=true;
    if not(timerMap.containsKey(functionId,t)) then begin
      t.timer:=TEpikTimer.create(nil);
      t.into:=1;
      t.timer.start;
    end else begin
      if t.into<=0 then t.timer.start;
      inc(t.into);
    end;
    timerMap.put(functionId,t);

    inc(currentLevel);
    {$ifdef DEBUGMODE}
    writeln('Stepping into level: ',currentLevel,' ',functionId);
    writeln('          steplevel: ',stepLevel);
    {$endif}
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.steppingOut(CONST functionId:ansistring);
  VAR t:T_timerEntry;
  begin
    system.enterCriticalSection(cs);
    levelChanged:=true;
    dec(currentLevel);
    {$ifdef DEBUGMODE}
    writeln('Stepping out to level: ',currentLevel,' ',functionId);
    writeln('            steplevel: ',stepLevel);
    {$endif}
    if timerMap.containsKey(functionId,t) then begin
      dec(t.into);
      if t.into<=0 then t.timer.stop;
      timerMap.put(functionId,t);
    end;

    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.doStep;
  begin
    system.enterCriticalSection(cs);
    state:=breakOnLineChange;
    stepLevel:=currentLevel;
    waitingForGUI:=false;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.doMicrostep;
  begin
    system.enterCriticalSection(cs);
    state:=breakSoonest;
    stepLevel:=currentLevel;
    waitingForGUI:=false;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.doStop;
  begin
    system.enterCriticalSection(cs);
    state:=dontBreakAtAll;
    waitingForGUI:=false;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.doStart(CONST continue: boolean);
  begin
    system.enterCriticalSection(cs);
    if not(continue) then begin
      clearTimers;
      lineChanged:=true;
      levelChanged:=true;
      stepLevel:=0;
      currentLevel:=0;
      currentLine.package:=nil;
      currentLine.column:=0;
      currentLine.line:=0;
      if (length(breakpoints)=0) then state:=breakOnLineChange
                                 else state:=runUntilBreakpoint;
    end else state:=runUntilBreakpoint;
    waitingForGUI:=false;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.clearBreakpoints;
  begin
    system.enterCriticalSection(cs);
    setLength(breakpoints,0);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.addBreakpoint(CONST fileName:string; CONST line: longint);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(breakpoints);
    setLength(breakpoints,i+1);
    breakpoints[i].fileName:=fileName;
    breakpoints[i].line:=line;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.doStepInto;
  begin
    system.enterCriticalSection(cs);
    state:=breakOnStepIn;
    waitingForGUI:=false;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.doStepOut;
  begin
    system.enterCriticalSection(cs);
    state:=breakOnStepOut;
    waitingForGUI:=false;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_stepper.haltet: boolean;
  begin
    system.enterCriticalSection(cs);
    result:=waitingForGUI;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_stepper.context: pointer;
  begin
    result:=contextPointer;
  end;

FUNCTION T_stepper.token: pointer;
  begin
    result:=tokenPointer;
  end;

FUNCTION T_stepper.stack:pointer;
  begin
    result:=stackPointer;
  end;

FUNCTION T_stepper.loc: T_tokenLocation;
  begin
    result:=currentLine;
  end;

PROCEDURE T_stepper.showTimeInfo(VAR adapters:T_adapters);
  CONST unknownLocTxt='Unknown location';
  VAR entrySet:T_TimerMap.KEY_VALUE_LIST;
      i,j:longint;
      swapTemp:T_TimerMap.KEY_VALUE_PAIR;
      keyWidth:longint=0;
  begin
    entrySet:=timerMap.entrySet;

    for i:=0 to length(entrySet)-1 do begin
      entrySet[i].value.timer.stop;
      if entrySet[i].key='' then entrySet[i].key:=unknownLocTxt;
      if length(entrySet[i].key)>keyWidth then keyWidth:=length(entrySet[i].key);
    end;

    for j:=1 to length(entrySet)-1 do for i:=0 to j-1 do if entrySet[i].value.timer.Elapsed<entrySet[j].value.timer.Elapsed then begin
      swapTemp:=entrySet[i];
      entrySet[i]:=entrySet[j];
      entrySet[j]:=swapTemp;
    end;

    adapters.raiseCustomMessage(mt_timing_info,'',C_nilTokenLocation);
    adapters.raiseCustomMessage(mt_timing_info,'Time spent by locations:',C_nilTokenLocation);
    for i:=0 to length(entrySet)-1 do adapters.raiseCustomMessage(mt_timing_info,entrySet[i].key+StringOfChar(' ',keyWidth-length(entrySet[i].key))+' '+floatToStr(entrySet[i].value.timer.Elapsed),C_nilTokenLocation);
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
