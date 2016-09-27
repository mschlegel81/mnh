UNIT mnh_out_adapters;
INTERFACE
USES mnh_constants, mnh_tokLoc, myGenerics,mySys,sysutils,myStringUtil{$ifdef fullVersion},mnh_plotData{$endif},EpikTimer{$ifdef IMIG},mypics{$endif};
TYPE
  T_storedMessage = record
    messageType : T_messageType;
    simpleMessage: ansistring;
    multiMessage: T_arrayOfString;
    location: T_searchTokenLocation;
  end;
  T_storedMessages = array of T_storedMessage;
  T_outputBehaviour= record
    doShowPrint:         boolean;
    doEchoInput:         boolean;
    doEchoDeclaration:   boolean;
    doShowExpressionOut: boolean;
    doShowTimingInfo:    boolean;
    minErrorLevel:       shortint;
  end;
  T_adapterType=(at_unknown,
                 at_console,
                 at_textFile,
                 at_htmlFile,
                 at_gui,
                 at_sandboxAdapter,
                 at_printTextFileAtRuntime);
CONST
  C_includableMessages:array[T_adapterType] of T_messageTypeSet=(
    {at_unknown}  [low(T_messageType)..high(T_messageType)],
    {at_console}  [mt_clearConsole..mt_el5_haltMessageReceived,mt_timing_info],
    {at_textFile} [mt_clearConsole..mt_el5_haltMessageReceived,mt_timing_info,mt_endOfEvaluation],
    {at_htmlFile} [low(T_messageType)..high(T_messageType)],
    {at_gui}      [low(T_messageType)..high(T_messageType)],
    {at_sandbo...}[low(T_messageType)..high(T_messageType)],
    {at_printT...}[mt_printline]);

TYPE
  P_abstractOutAdapter = ^T_abstractOutAdapter;
  T_abstractOutAdapter = object
    private
      autodestruct:boolean;
      messageTypesToInclude:T_messageTypeSet;
      PROCEDURE enableMessageType(CONST enabled:boolean; CONST mt:T_messageType);
      FUNCTION  getShowPrint                    : boolean;
      PROCEDURE setShowPrint        (CONST value: boolean );
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
    adapterType:T_adapterType;

    CONSTRUCTOR create(CONST typ:T_adapterType; CONST behaviour:T_outputBehaviour);
    DESTRUCTOR destroy; virtual; abstract;
    FUNCTION append(CONST message:T_storedMessage):boolean; virtual; abstract;

    PROPERTY doShowPrint        : boolean        read getShowPrint         write setShowPrint        ;
    PROPERTY doEchoInput        : boolean        read getEchoInput         write setEchoInput        ;
    PROPERTY doEchoDeclaration  : boolean        read getEchoDeclaration   write setEchoDeclaration  ;
    PROPERTY doShowExpressionOut: boolean        read getShowExpressionOut write setShowExpressionOut;
    PROPERTY doShowTimingInfo   : boolean        read getShowTimingInfo    write setShowTimingInfo   ;
    PROPERTY minErrorLevel      : shortint       read getMinErrorLevel     write setMinErrorLevel    ;
    PROPERTY outputBehaviour : T_outputBehaviour read getOutputBehaviour   write setOutputBehaviour;
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;
  T_consoleOutAdapter = object(T_abstractOutAdapter)
    CONSTRUCTOR create(CONST behaviour:T_outputBehaviour);
    DESTRUCTOR destroy; virtual;
    FUNCTION append(CONST message:T_storedMessage):boolean; virtual;
  end;

  P_collectingOutAdapter = ^T_collectingOutAdapter;
  T_collectingOutAdapter = object(T_abstractOutAdapter)
    storedMessages:T_storedMessages;
    includeNonTextOutput:boolean;
    cs:TRTLCriticalSection;
    CONSTRUCTOR create(CONST typ:T_adapterType; CONST behaviour:T_outputBehaviour);
    DESTRUCTOR destroy; virtual;
    FUNCTION append(CONST message:T_storedMessage):boolean; virtual;
    PROCEDURE clearMessages;
  end;

  T_abstractFileOutAdapter = object(T_collectingOutAdapter)
    protected
      outputFileName:ansistring;
      forceRewrite:boolean;
      FUNCTION switchFile(CONST newFileName:string):boolean; virtual;
      PROCEDURE flush(CONST finalFlush:boolean); virtual; abstract;
    public
      CONSTRUCTOR create(CONST typ:T_adapterType; CONST fileName:ansistring; CONST behaviour:T_outputBehaviour; CONST forceNewFile:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message: T_storedMessage):boolean; virtual;
  end;

  P_textFileOutAdapter = ^T_textFileOutAdapter;
  T_textFileOutAdapter = object(T_abstractFileOutAdapter)
    protected
      longestLineUpToNow:longint;
      FUNCTION switchFile(CONST newFileName:string):boolean; virtual;
      PROCEDURE flush(CONST finalFlush:boolean); virtual;
    public
      CONSTRUCTOR create(CONST fileName:ansistring; CONST behaviour:T_outputBehaviour; CONST forceNewFile:boolean);
      DESTRUCTOR destroy; virtual;
  end;

  P_adapters=^T_adapters;
  T_adapters=object
    private
      profiler:record
        unaccounted,
        importing,
        tokenizing,
        declarations,
        interpretation:double;
      end;
      stackTraceCount:longint;
      errorCount:longint;
      maxErrorLevel: shortint;
      adapter:array of P_abstractOutAdapter;

      someEchoInput        :boolean;
      someEchoDeclaration  :boolean;
      someShowExpressionOut:boolean;
      someShowTimingInfo   :boolean;
    public
      hasMessageOfType:array[T_messageType] of boolean;
      {$ifdef fullVersion}
      plot:T_plot;
      {$endif}
      {$ifdef IMIG}
      picture:specialize G_safeVar<P_rawImage>;
      {$endif}
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clearErrors;
      PROCEDURE raiseCustomMessage(CONST thisErrorLevel: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseCustomMessage(CONST message:T_storedMessage);
      PROCEDURE raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE printOut(CONST s:T_arrayOfString);
      PROCEDURE clearPrint;
      PROCEDURE clearAll;
      PROCEDURE stopEvaluation;
      FUNCTION noErrors: boolean; inline;
      FUNCTION hasHaltMessage: boolean;
      PROCEDURE resetErrorFlags;
      PROCEDURE updateErrorlevel;
      {$ifdef fullVersion}FUNCTION hasNeedGUIerror:boolean;{$endif}
      PROCEDURE haltEvaluation;
      PROCEDURE logEndOfEvaluation;
      PROCEDURE raiseSystemError(CONST errorMessage: ansistring);

      PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter; CONST destroyIt:boolean);
      PROCEDURE addConsoleOutAdapter(CONST verbosity:string='');
      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);
      PROCEDURE removeOutAdapter(CONST index:longint);
      PROCEDURE setPrintTextFileAdapter(CONST filenameOrBlank:string);

      FUNCTION adapterCount:longint;
      FUNCTION getAdapter(CONST index:longint):P_abstractOutAdapter;

      FUNCTION collectingClone:P_adapters;
      PROCEDURE copyDataFromCollectingCloneDisposing(VAR clone:P_adapters; CONST errorCase:boolean);

      PROCEDURE setExitCode;

      PROPERTY doEchoInput:         boolean read someEchoInput        ;
      PROPERTY doEchoDeclaration:   boolean read someEchoDeclaration  ;
      PROPERTY doShowExpressionOut: boolean read someShowExpressionOut;
      PROPERTY doShowTimingInfo:    boolean read someShowTimingInfo   ;

      PROCEDURE profileUnaccounted;
      PROCEDURE profileImporting;
      PROCEDURE profileTokenizing;
      PROCEDURE profileDeclarations;
      PROCEDURE profileInterpretation;
      PROCEDURE appendTimingInfoIfApplicable;
  end;

  {$ifdef fullVersion}
  T_timerEntry=record
    elapsed:extended;
    into:longint;
    functionId:string;
    callCount:longint;
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
      PROCEDURE steppingIn (CONST location,functionId:ansistring);
      PROCEDURE steppingOut(CONST location:ansistring);

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

CONST
  C_defaultOutputBehavior_interactive:T_outputBehaviour=(
    doShowPrint:         true;
    doEchoInput:         true;
    doEchoDeclaration:   true;
    doShowExpressionOut: true;
    doShowTimingInfo:    true;
    minErrorLevel:       3);
  C_defaultOutputBehavior_fileMode:T_outputBehaviour=(
    doShowPrint:         true;
    doEchoInput:         false;
    doEchoDeclaration:   false;
    doShowExpressionOut: false;
    doShowTimingInfo:    false;
    minErrorLevel:       3);
  C_collectAllOutputBehavior:T_outputBehaviour=(
    doShowPrint:         true;
    doEchoInput:         true;
    doEchoDeclaration:   true;
    doShowExpressionOut: true;
    doShowTimingInfo:    true;
    minErrorLevel:       1);

VAR
  wallClock: specialize G_lazyVar<TEpikTimer>;
  defaultOutputBehavior:T_outputBehaviour;
{$ifdef fullVersion}
  stepper:T_stepper;
  currentlyDebugging:boolean=false;
  gui_started:boolean=false;
{$endif}

FUNCTION defaultFormatting(CONST message:T_storedMessage):ansistring;
FUNCTION defaultFormatting(CONST messageType:T_messageType; CONST message: ansistring; CONST location: T_searchTokenLocation):ansistring;
OPERATOR :=(s:string):T_outputBehaviour;
IMPLEMENTATION
FUNCTION message(CONST messageType  : T_messageType;
                 CONST simpleMessage: ansistring;
                 CONST location     : T_searchTokenLocation):T_storedMessage;
  begin
    result.messageType  :=messageType  ;
    result.simpleMessage:=simpleMessage;
    result.location     :=location     ;
    result.multiMessage:=C_EMPTY_STRING_ARRAY;
  end;

FUNCTION defaultFormatting(CONST message: T_storedMessage): ansistring;
  begin
    with message do if (length(simpleMessage)=0) and (length(multiMessage)>0)
    then result:=defaultFormatting(messageType,join(multiMessage,C_lineBreakChar),location)
    else result:=defaultFormatting(messageType,simpleMessage,location);
  end;

FUNCTION defaultFormatting(CONST messageType:T_messageType; CONST message: ansistring; CONST location: T_searchTokenLocation):ansistring;
  begin
    if messageType=mt_printline then exit(message);
    with C_messageTypeMeta[messageType] do begin
      result:=prefix;
      if includeLocation then result:=result+ansistring(location)+' ';
      result:=replaceAll(result+message,UTF8_ZERO_WIDTH_SPACE,'');
    end;
  end;

OPERATOR :=(s:string):T_outputBehaviour;
  VAR i:longint;
  begin
    result:=defaultOutputBehavior;
    for i:=1 to length(s) do case s[i] of
      'v': begin
             result.minErrorLevel:=1;
             result.doShowPrint        :=true;
             result.doEchoInput        :=true;
             result.doEchoDeclaration  :=true;
             result.doShowExpressionOut:=true;
             result.doShowTimingInfo   :=true;
           end;
      'p': result.doShowPrint:=true;
      'P': result.doShowPrint:=false;
      'i': result.doEchoInput:=true;
      'I': result.doEchoInput:=false;
      'd': result.doEchoDeclaration:=true;
      'D': result.doEchoDeclaration:=false;
      'o': result.doShowExpressionOut:=true;
      'O': result.doShowExpressionOut:=false;
      't': result.doShowTimingInfo:=true;
      'T': result.doShowTimingInfo:=false;
      '1': result.minErrorLevel:=1;
      '2': result.minErrorLevel:=2;
      '3': result.minErrorLevel:=3;
      '4': result.minErrorLevel:=4;
      '5': result.minErrorLevel:=5;
    end;
  end;
//T_abstractOutAdapter:=========================================================
PROCEDURE T_abstractOutAdapter.enableMessageType(CONST enabled: boolean; CONST mt: T_messageType);
  begin
    if enabled and (mt in C_includableMessages[adapterType])
    then messageTypesToInclude:=messageTypesToInclude+[mt]
    else messageTypesToInclude:=messageTypesToInclude-[mt];
  end;

FUNCTION T_abstractOutAdapter.getShowPrint        : boolean; begin result:=mt_printline        in messageTypesToInclude; end;
FUNCTION T_abstractOutAdapter.getEchoInput        : boolean; begin result:=mt_echo_input       in messageTypesToInclude; end;
FUNCTION T_abstractOutAdapter.getEchoDeclaration  : boolean; begin result:=mt_echo_declaration in messageTypesToInclude; end;
FUNCTION T_abstractOutAdapter.getShowExpressionOut: boolean; begin result:=mt_echo_output      in messageTypesToInclude; end;
FUNCTION T_abstractOutAdapter.getShowTimingInfo   : boolean; begin result:=mt_timing_info      in messageTypesToInclude; end;
PROCEDURE T_abstractOutAdapter.setShowPrint        (CONST value: boolean); begin enableMessageType(value,mt_printline); enableMessageType(value,mt_clearConsole); end;
PROCEDURE T_abstractOutAdapter.setEchoInput        (CONST value: boolean); begin enableMessageType(value,mt_echo_input);       end;
PROCEDURE T_abstractOutAdapter.setEchoDeclaration  (CONST value: boolean); begin enableMessageType(value,mt_echo_declaration); end;
PROCEDURE T_abstractOutAdapter.setShowExpressionOut(CONST value: boolean); begin enableMessageType(value,mt_echo_output);      end;
PROCEDURE T_abstractOutAdapter.setShowTimingInfo   (CONST value: boolean); begin enableMessageType(value,mt_timing_info);      end;
FUNCTION T_abstractOutAdapter.getMinErrorLevel: shortint;
  VAR mt:T_messageType;
  begin
    result:=127;
    for mt:=low(T_messageType) to high(T_messageType) do
      if (mt in messageTypesToInclude       ) and
         (C_messageTypeMeta[mt].level>=0    ) and
         (C_messageTypeMeta[mt].level<result) then result:=C_messageTypeMeta[mt].level;
  end;

PROCEDURE T_abstractOutAdapter.setMinErrorLevel(CONST value: shortint);
  VAR mt:T_messageType;
  begin
    for mt:=low(T_messageType) to high(T_messageType) do
      if (C_messageTypeMeta[mt].level>=0) then enableMessageType(C_messageTypeMeta[mt].level>=value,mt);
  end;

FUNCTION T_abstractOutAdapter.getOutputBehaviour: T_outputBehaviour;
  begin
    result.doShowPrint        :=doShowPrint;
    result.doEchoInput        :=doEchoInput        ;
    result.doEchoDeclaration  :=doEchoDeclaration  ;
    result.doShowExpressionOut:=doShowExpressionOut;
    result.doShowTimingInfo   :=doShowTimingInfo   ;
    result.minErrorLevel      :=minErrorLevel      ;
  end;

PROCEDURE T_abstractOutAdapter.setOutputBehaviour(CONST value: T_outputBehaviour);
  begin
    doShowPrint        :=value.doShowPrint        ;
    doEchoInput        :=value.doEchoInput        ;
    doEchoDeclaration  :=value.doEchoDeclaration  ;
    doShowExpressionOut:=value.doShowExpressionOut;
    doShowTimingInfo   :=value.doShowTimingInfo   ;
    minErrorLevel      :=value.minErrorLevel      ;
  end;

CONSTRUCTOR T_abstractOutAdapter.create(CONST typ: T_adapterType; CONST behaviour:T_outputBehaviour);
  begin
    adapterType:=typ;
    setOutputBehaviour(behaviour);
  end;
//=========================================================:T_abstractOutAdapter
//T_consoleOutAdapter:==========================================================
CONSTRUCTOR T_consoleOutAdapter.create(CONST behaviour:T_outputBehaviour);
  begin
    inherited create(at_console,behaviour);
  end;

DESTRUCTOR T_consoleOutAdapter.destroy;
  begin
  end;

FUNCTION T_consoleOutAdapter.append(CONST message:T_storedMessage):boolean;
  VAR i:longint;
      compound:ansistring;
  begin
    result:=message.messageType in messageTypesToInclude;
    if result then with message do case messageType of
      mt_clearConsole: mySys.clearConsole;
      mt_printline: begin
        if not(mySys.isConsoleShowing) then mySys.showConsole;
        if (length(multiMessage)>0) and (multiMessage[0]=C_formFeedChar) then begin
          mySys.clearConsole;
          if length(multiMessage)>1 then begin
            compound:=multiMessage[1];
            for i:=2 to length(multiMessage)-1 do compound:=compound+LineEnding+multiMessage[i];
            writeln(compound);
          end;
        end else if length(multiMessage)>0 then begin
          compound:=multiMessage[0];
          for i:=1 to length(multiMessage)-1 do compound:=compound+LineEnding+multiMessage[i];
          writeln(compound);
        end;
      end
      else writeln(stdErr,defaultFormatting(message));
    end;
  end;
//==========================================================:T_consoleOutAdapter
//T_collectingOutAdapter:=======================================================
CONSTRUCTOR T_collectingOutAdapter.create(CONST typ:T_adapterType; CONST behaviour:T_outputBehaviour);
  begin
    inherited create(typ,behaviour);
    system.initCriticalSection(cs);
    setLength(storedMessages,0);
  end;

DESTRUCTOR T_collectingOutAdapter.destroy;
  begin
    clearMessages;
    system.doneCriticalSection(cs);
  end;

FUNCTION T_collectingOutAdapter.append(CONST message: T_storedMessage):boolean;
  begin
    result:=message.messageType in messageTypesToInclude;
    if result then with message do begin
      system.enterCriticalSection(cs);
      setLength(storedMessages,length(storedMessages)+1);
      storedMessages[length(storedMessages)-1]:=message;
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_collectingOutAdapter.clearMessages;
  begin
    system.enterCriticalSection(cs);
    setLength(storedMessages,0);
    system.leaveCriticalSection(cs);
  end;
//=======================================================:T_collectingOutAdapter
//T_abstractFileOutAdapter:=====================================================
CONSTRUCTOR T_abstractFileOutAdapter.create(CONST typ:T_adapterType; CONST fileName:ansistring; CONST behaviour:T_outputBehaviour; CONST forceNewFile:boolean);
  begin
    inherited create(typ,behaviour);
    outputFileName:=expandFileName(fileName);
    forceRewrite:=forceNewFile;
  end;

DESTRUCTOR T_abstractFileOutAdapter.destroy;
  begin
    flush(true);
    inherited destroy;
  end;

FUNCTION T_abstractFileOutAdapter.append(CONST message: T_storedMessage):boolean;
  begin
    result:=inherited append(message);
    if result {$ifndef DEBUGMODE} and (length(storedMessages)>=100) {$endif} then flush(false);
  end;

FUNCTION T_abstractFileOutAdapter.switchFile(CONST newFileName:string):boolean;
  VAR newFullFileName:string;
  begin
    newFullFileName:=expandFileName(newFileName);
    if newFileName=outputFileName then exit(false);
    flush(true);
    outputFileName:=newFullFileName;
    result:=true;
  end;
//=====================================================:T_abstractFileOutAdapter
//T_textFileOutAdapter:=========================================================
FUNCTION T_textFileOutAdapter.switchFile(CONST newFileName: string):boolean;
  begin
    result:=inherited switchFile(newFileName);
    if result then longestLineUpToNow:=0;
  end;

PROCEDURE T_textFileOutAdapter.flush(CONST finalFlush:boolean);
  VAR i,j:longint;
      handle:text;
  PROCEDURE myWrite(CONST s:ansistring);
    begin
      if length(s)>=longestLineUpToNow then longestLineUpToNow:=length(s);
      writeln(handle,s);
    end;
  begin
    if length(storedMessages)>0 then begin
      enterCriticalSection(cs);
      try
        assign(handle,outputFileName);
        if fileExists(outputFileName) and not(forceRewrite)
        then system.append(handle)
        else rewrite(handle);
        forceRewrite:=false;
        for i:=0 to length(storedMessages)-1 do with storedMessages[i] do case messageType of
          mt_printline: for j:=0 to length(multiMessage)-1 do myWrite(multiMessage[j]);
          else if C_messageTypeMeta[messageType].textOut then myWrite(defaultFormatting(storedMessages[i]));
        end;
        clearMessages;
        if finalFlush and (longestLineUpToNow>0) then writeln(handle,StringOfChar('=',longestLineUpToNow));
        close(handle);
      finally
        leaveCriticalSection(cs);
      end;
    end;
  end;

CONSTRUCTOR T_textFileOutAdapter.create(CONST fileName: ansistring; CONST behaviour:T_outputBehaviour; CONST forceNewFile:boolean);
  begin
    inherited create(at_textFile,fileName,behaviour,forceNewFile);
    longestLineUpToNow:=0;
  end;

DESTRUCTOR T_textFileOutAdapter.destroy;
  begin
    flush(true);
    inherited destroy;
  end;
//=========================================================:T_textFileOutAdapter
//T_adapters:===================================================================
CONSTRUCTOR T_adapters.create;
  begin
    {$ifdef fullVersion}
    plot.createWithDefaults;
    {$endif}
    {$ifdef IMIG}
    picture.create(nil);
    {$endif}
    setLength(adapter,0);
    clearAll;
  end;

DESTRUCTOR T_adapters.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i]^.autodestruct then dispose(adapter[i],destroy);
    setLength(adapter,0);
    {$ifdef fullVersion}
    plot.destroy;
    {$endif}
    {$ifdef IMIG}
    if (picture.value<>nil) then dispose(picture.value,destroy);
    picture.destroy;
    {$endif}
  end;

PROCEDURE T_adapters.clearErrors;
  VAR mt:T_messageType;
  begin
    for mt:=low(T_messageType) to high(T_messageType) do begin
      hasMessageOfType[mt]:=false;
      if   maxErrorLevel>=C_messageTypeMeta[mt].level
      then maxErrorLevel:=C_messageTypeMeta[mt].level-1;
    end;
    stackTraceCount:=0;
    errorCount:=0;
  end;

PROCEDURE T_adapters.raiseCustomMessage(CONST thisErrorLevel: T_messageType; CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
  VAR m:T_storedMessage;
  begin
    m.messageType:=thisErrorLevel;
    m.simpleMessage:=errorMessage;
    m.location:=errorLocation;
    raiseCustomMessage(m);
  end;

PROCEDURE T_adapters.raiseCustomMessage(CONST message: T_storedMessage);
  VAR i:longint;
  begin
    hasMessageOfType[message.messageType]:=true;
    if maxErrorLevel< C_messageTypeMeta[message.messageType].level then
       maxErrorLevel:=C_messageTypeMeta[message.messageType].level;
    if hasHaltMessage and not(message.messageType in [mt_endOfEvaluation,mt_timing_info]) then exit;
    if (message.messageType=mt_el3_stackTrace) then begin
      inc(stackTraceCount);
      if stackTraceCount>30 then exit;
    end;
    if (message.messageType in [mt_el3_evalError,mt_el3_noMatchingMain,mt_el4_parsingError,mt_el5_haltMessageReceived,mt_el5_systemError]) then begin
      inc(errorCount);
      if errorCount>30 then exit;
    end;
    for i:=0 to length(adapter)-1 do adapter[i]^.append(message);
  end;

PROCEDURE T_adapters.raiseError(CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
  begin
    raiseCustomMessage(message(mt_el3_evalError,errorMessage,errorLocation));
  end;

PROCEDURE T_adapters.raiseWarning(CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
  begin
    raiseCustomMessage(message(mt_el2_warning,errorMessage,errorLocation));
  end;

PROCEDURE T_adapters.raiseNote(CONST errorMessage: ansistring; CONST errorLocation: T_searchTokenLocation);
  begin
    raiseCustomMessage(message(mt_el1_note,errorMessage,errorLocation));
  end;

PROCEDURE T_adapters.printOut(CONST s: T_arrayOfString);
  VAR i:longint;
      m:T_storedMessage;
  begin
    m.messageType:=mt_printline;
    m.multiMessage:=s;
    for i:=0 to length(adapter)-1 do adapter[i]^.append(m);
  end;

CONST C_nilTokenLocation: T_searchTokenLocation = (fileName:'?'; line: 0; column: 0);
PROCEDURE T_adapters.clearPrint;
  begin
    raiseCustomMessage(mt_clearConsole,'',C_nilTokenLocation);
  end;

PROCEDURE T_adapters.clearAll;
  VAR i:longint;
  begin
    clearErrors;
    clearPrint;
    {$ifdef imig}
    if picture.value<>nil then dispose(picture.value,destroy);
    picture.value:=nil;
    {$endif}   
    writeln('Print cleared.');
    someEchoInput        :=false;
    someEchoDeclaration  :=false;
    someShowExpressionOut:=false;
    someShowTimingInfo   :=false;
    for i:=0 to length(adapter)-1 do begin
      someEchoInput        :=someEchoInput         or adapter[i]^.doEchoInput        ;
      someEchoDeclaration  :=someEchoDeclaration   or adapter[i]^.doEchoDeclaration  ;
      someShowExpressionOut:=someShowExpressionOut or adapter[i]^.doShowExpressionOut;
      someShowTimingInfo   :=someShowTimingInfo    or adapter[i]^.doShowTimingInfo   ;
    end;
    with profiler do begin
      unaccounted   :=0;
      importing     :=0;
      tokenizing    :=0;
      declarations  :=0;
      interpretation:=0;
    end;
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

FUNCTION T_adapters.hasHaltMessage:boolean;
  begin
    result:=hasMessageOfType[mt_el5_haltMessageQuiet] or
            hasMessageOfType[mt_el5_haltMessageReceived];
  end;

PROCEDURE T_adapters.resetErrorFlags;
  VAR mt:T_messageType;
  begin
    maxErrorLevel:=0;
    for mt:=low(T_messageType) to high(T_messageType) do hasMessageOfType[mt]:=false;
  end;

PROCEDURE T_adapters.updateErrorlevel;
  VAR mt:T_messageType;
  begin
    maxErrorLevel:=0;
    for mt:=low(T_messageType) to high(T_messageType) do
    if (mt<>mt_el5_haltMessageQuiet) and
       (hasMessageOfType[mt]) and
       (C_messageTypeMeta[mt].level>maxErrorLevel) then maxErrorLevel:=C_messageTypeMeta[mt].level;
  end;

{$ifdef fullVersion}
FUNCTION T_adapters.hasNeedGUIerror: boolean;
  VAR m:T_messageType;
  begin
    if gui_started then exit(false);
    for m:=low(T_messageType) to high(T_messageType) do
    if hasMessageOfType[m] and C_messageTypeMeta[m].triggersGuiStartup then exit(true);
    result:=false;
  end;
{$endif}

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
  begin
    setLength(adapter,length(adapter)+1);
    adapter[length(adapter)-1]:=p;
    p^.autodestruct:=destroyIt;
    someEchoInput        :=someEchoInput         or p^.doEchoInput        ;
    someEchoDeclaration  :=someEchoDeclaration   or p^.doEchoDeclaration  ;
    someShowExpressionOut:=someShowExpressionOut or p^.doShowExpressionOut;
    someShowTimingInfo   :=someShowTimingInfo    or p^.doShowTimingInfo   ;
  end;

PROCEDURE T_adapters.addConsoleOutAdapter(CONST verbosity:string='');
  VAR consoleOutAdapter:P_consoleOutAdapter;
  begin
    new(consoleOutAdapter,create(verbosity));
    addOutAdapter(consoleOutAdapter,true);
  end;

PROCEDURE T_adapters.removeOutAdapter(CONST p: P_abstractOutAdapter);
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i]=p then begin
      removeOutAdapter(i);
      exit;
    end;
  end;

PROCEDURE T_adapters.removeOutAdapter(CONST index:longint);
  VAR j:longint;
  begin
    if (index<0) or (index>=length(adapter)) then exit;
    if adapter[index]^.autodestruct then dispose(adapter[index],destroy);
    for j:=index to length(adapter)-2 do adapter[j]:=adapter[j+1];
    setLength(adapter,length(adapter)-1);
  end;

PROCEDURE T_adapters.setPrintTextFileAdapter(CONST filenameOrBlank:string);
  VAR currentAdapterIndex:longint;
      txtAdapter:P_textFileOutAdapter;
  begin
    currentAdapterIndex:=length(adapter)-1;
    while (currentAdapterIndex>=0) and (adapter[currentAdapterIndex]^.adapterType<>at_printTextFileAtRuntime) do dec(currentAdapterIndex);

    if isBlank(filenameOrBlank) then begin
      if currentAdapterIndex>=0 then removeOutAdapter(currentAdapterIndex);
      //...else there is no adapter to be removed
    end else begin
      if currentAdapterIndex>=0 then P_textFileOutAdapter(adapter[currentAdapterIndex])^.switchFile(filenameOrBlank)
      else begin
        new(txtAdapter,create(filenameOrBlank,defaultOutputBehavior,false));
        txtAdapter^.messageTypesToInclude:=[mt_printline];
      end;
    end;
  end;

FUNCTION T_adapters.adapterCount: longint;
  begin
    result:=length(adapter);
  end;

FUNCTION T_adapters.getAdapter(CONST index: longint): P_abstractOutAdapter;
  begin
    result:=adapter[index];
  end;

FUNCTION T_adapters.collectingClone: P_adapters;
  VAR collector:P_collectingOutAdapter;
  begin
    new(result,create);
    new(collector,create(at_sandboxAdapter,''));
    collector^.messageTypesToInclude:=[low(T_messageType)..high(T_messageType)];
    result^.addOutAdapter(collector,true);
    {$ifdef fullVersion}
    result^.plot.CopyFrom(plot);
    {$endif}
  end;

PROCEDURE T_adapters.copyDataFromCollectingCloneDisposing(VAR clone: P_adapters; CONST errorCase:boolean);
  VAR collector:P_collectingOutAdapter=nil;
      i:longint;
  begin
    for i:=0 to length(clone^.adapter)-1 do
    if (collector=nil) and
       (clone^.adapter[i]^.adapterType=at_sandboxAdapter) then collector:=P_collectingOutAdapter(clone^.adapter[i]);
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
      mt_reloadRequired: raiseCustomMessage(collector^.storedMessages[i]);
      else begin
        if not(errorCase) then raiseCustomMessage(collector^.storedMessages[i]);
      end;
    end;
    dispose(clone,destroy);
  end;

PROCEDURE T_adapters.setExitCode;
  CONST MAX_IGNORED_LEVEL=2;
  begin
    if maxErrorLevel>MAX_IGNORED_LEVEL then ExitCode:=maxErrorLevel;
  end;


PROCEDURE T_adapters.profileUnaccounted;    begin with profiler do if someShowTimingInfo then unaccounted   :=wallClock.value.elapsed-unaccounted   ; end;
PROCEDURE T_adapters.profileImporting;      begin with profiler do if someShowTimingInfo then importing     :=wallClock.value.elapsed-importing     ; end;
PROCEDURE T_adapters.profileTokenizing;     begin with profiler do if someShowTimingInfo then tokenizing    :=wallClock.value.elapsed-tokenizing    ; end;
PROCEDURE T_adapters.profileDeclarations;   begin with profiler do if someShowTimingInfo then declarations  :=wallClock.value.elapsed-declarations  ; end;
PROCEDURE T_adapters.profileInterpretation; begin with profiler do if someShowTimingInfo then interpretation:=wallClock.value.elapsed-interpretation; end;

PROCEDURE T_adapters.appendTimingInfoIfApplicable;
  VAR importing_     ,
      tokenizing_    ,
      declarations_  ,
      interpretation_,
      unaccounted_   ,
      total_         ,
      timeUnit:string;
      totalTime:double;
      longest:longint=0;
      formatString:ansistring;

  FUNCTION fmt(CONST d:double):string; begin result:=formatFloat(formatString,d); if length(result)>longest then longest:=length(result); end;
  FUNCTION fmt(CONST s:string):string; begin result:=StringOfChar(' ',longest-length(s))+s+timeUnit; end;
  begin
    with profiler do begin
      totalTime:=importing+tokenizing+declarations+interpretation+unaccounted;
      if totalTime<1 then begin
        importing     :=importing     *1000;
        tokenizing    :=tokenizing    *1000;
        declarations  :=declarations  *1000;
        interpretation:=interpretation*1000;
        unaccounted   :=unaccounted   *1000;
        totalTime     :=totalTime     *1000;
        timeUnit:='ms';
        formatString:='0.000';
      end else begin
        timeUnit:='s';
        formatString:='0.000000';
      end;
      importing_     :=fmt(importing     );
      tokenizing_    :=fmt(tokenizing    );
      declarations_  :=fmt(declarations  );
      interpretation_:=fmt(interpretation);
      unaccounted_   :=fmt(unaccounted   );
      total_         :=fmt(totalTime     );
      if importing     >0 then raiseCustomMessage(mt_timing_info,'Importing time      '+fmt(importing_     ),C_nilTokenLocation);
      if tokenizing    >0 then raiseCustomMessage(mt_timing_info,'Tokenizing time     '+fmt(tokenizing_    ),C_nilTokenLocation);
      if declarations  >0 then raiseCustomMessage(mt_timing_info,'Declaration time    '+fmt(declarations_  ),C_nilTokenLocation);
      if interpretation>0 then raiseCustomMessage(mt_timing_info,'Interpretation time '+fmt(interpretation_),C_nilTokenLocation);
      if unaccounted   >0 then raiseCustomMessage(mt_timing_info,'Unaccounted for     '+fmt(unaccounted_   ),C_nilTokenLocation);
                               raiseCustomMessage(mt_timing_info,StringOfChar('-',20+length(fmt(total_))   ),C_nilTokenLocation);
                               raiseCustomMessage(mt_timing_info,'                    '+fmt(total_         ),C_nilTokenLocation);
    end;
    {$ifdef FULLVERSION}
    if currentlyDebugging then stepper.showTimeInfo(self);
    {$endif}
  end;

//===================================================================:T_adapters
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
  begin
    timerMap.clear;
  end;

PROCEDURE T_stepper.stopAllTimers;
  begin
    wallClock.value.stop;
  end;

PROCEDURE T_stepper.resumeAllTimers;
  begin
    wallClock.value.start;
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
      {$ifdef DEBUGMODE}
      if (state=breakSoonest)                                 then writeln('Break condition met: breakSoonest');
      if (state=breakOnStepIn) and (currentLevel>stepLevel) then writeln('Break condition met: breakOnStepIn');
      if (state=breakOnStepOut) and (currentLevel<stepLevel)then writeln('Break condition met: breakOnStepOut');
      if (state=breakOnLineChange) and ((currentLevel<stepLevel) or (currentLevel=stepLevel) and (lineChanged and levelChanged)) then writeln('Break condition met: breakOnLineChange');
      if (lineChanged or levelChanged or (currentLevel<>stepLevel)) and breakpointEncountered then writeln('Break condition met: breakpointEncountered');
      {$endif}
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

PROCEDURE T_stepper.steppingIn(CONST location,functionId:ansistring);
  VAR t:T_timerEntry;
  begin
    system.enterCriticalSection(cs);
    levelChanged:=true;
    if not(timerMap.containsKey(location,t)) then begin
      t.elapsed:=wallClock.value.elapsed;
      t.into:=1;
      t.callCount:=1;
      t.functionId:=functionId;
    end else begin
      if t.into<=0 then t.elapsed:=wallClock.value.elapsed-t.elapsed;
      inc(t.callCount);
      inc(t.into);
      if t.functionId='' then t.functionId:=functionId;;
    end;
    timerMap.put(location,t);

    inc(currentLevel);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_stepper.steppingOut(CONST location:ansistring);
  VAR t:T_timerEntry;
  begin
    system.enterCriticalSection(cs);
    levelChanged:=true;
    dec(currentLevel);
    if timerMap.containsKey(location,t) then begin
      dec(t.into);
      if t.into<=0 then t.elapsed:=wallClock.value.elapsed-t.elapsed;
      timerMap.put(location,t);
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
      state:=runUntilBreakpoint;
      wallClock.value.clear;
      wallClock.value.start;
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
  FUNCTION nicestTime(CONST seconds:double):string;
    begin
      if seconds>=10         then result:=formatFloat('0.000',seconds)+C_invisibleTabChar+'s'
      else if seconds>=10E-3 then result:=formatFloat('0.000',seconds*1E3)+C_invisibleTabChar+'ms'
      else                        result:=formatFloat('0.000',seconds*1E6)+C_invisibleTabChar+'µs';
    end;

  CONST headerLine='Location'+C_tabChar+'ID'+C_tabChar+'count'+C_tabChar+'time';
  VAR entrySet:T_TimerMap.KEY_VALUE_LIST;
      i,j:longint;
      swapTemp:T_TimerMap.KEY_VALUE_PAIR;
      linesToPrint:T_arrayOfString;
  begin
    entrySet:=timerMap.entrySet;
    for j:=1 to length(entrySet)-1 do for i:=0 to j-1 do if entrySet[i].value.elapsed<entrySet[j].value.elapsed then begin
      swapTemp:=entrySet[i];
      entrySet[i]:=entrySet[j];
      entrySet[j]:=swapTemp;
    end;
    setLength(linesToPrint,length(entrySet)+1);
    linesToPrint[0]:=headerLine;
    for i:=0 to length(entrySet)-1 do begin
      if startsWith(entrySet[i].key,BUILTIN_PSEUDO_LOCATION_PREFIX)
      then linesToPrint[i+1]:=BUILTIN_PSEUDO_LOCATION_PREFIX
      else linesToPrint[i+1]:=entrySet[i].key;
      linesToPrint[i+1]:=linesToPrint[i+1]+C_tabChar+
                       entrySet[i].value.functionId+C_tabChar+
                       intToStr(entrySet[i].value.callCount)+C_tabChar+
                       nicestTime(entrySet[i].value.elapsed);
    end;
    linesToPrint:=formatTabs(linesToPrint);

    adapters.raiseCustomMessage(mt_timing_info,'',C_nilTokenLocation);
    adapters.raiseCustomMessage(mt_timing_info,'Time spent by locations:',C_nilTokenLocation);
    for i:=0 to length(linesToPrint)-1 do adapters.raiseCustomMessage(mt_timing_info,linesToPrint[i],C_nilTokenLocation);
  end;
{$endif}
FUNCTION initTimer:TEpikTimer;
  begin
    result:=TEpikTimer.create(nil);
    result.clear;
    result.start;
  end;

PROCEDURE disposeTimer(t:TEpikTimer);
  begin
    t.destroy;
  end;

INITIALIZATION
  wallClock.create(@initTimer,@disposeTimer);
  defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;
  {$ifdef fullVersion}
  stepper.create;
  {$endif}

FINALIZATION
  wallClock.destroy;
  {$ifdef fullVersion}
  stepper.destroy;
  {$endif}

end.
