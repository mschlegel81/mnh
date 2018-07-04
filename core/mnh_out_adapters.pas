UNIT mnh_out_adapters;
INTERFACE
USES sysutils,
     myGenerics,mySys,
     myStringUtil,
     mnh_messages,
     mnh_constants, mnh_basicTypes;
TYPE
  T_adapterType=(at_unknown,
                 at_console,
                 at_textFile,
                 at_gui,
                 at_sandboxAdapter,
                 at_printTextFileAtRuntime);
CONST
  C_includableMessages:array[T_adapterType] of T_messageTypeSet=(
    {at_unknown}  [low(T_messageType)..high(T_messageType)],
    {at_console}  [mt_clearConsole..mt_el4_systemError,mt_timing_info],
    {at_textFile} [mt_printline..mt_el4_systemError,mt_timing_info],
    {at_gui}      [low(T_messageType)..high(T_messageType)],
    {at_sandbo...}[low(T_messageType)..high(T_messageType)],
    {at_printT...}[mt_printline]);

TYPE
  P_abstractOutAdapter = ^T_abstractOutAdapter;
  T_abstractOutAdapter = object
    protected
      cs:TRTLCriticalSection;
    private
      messageTypesToInclude:T_messageTypeSet;
      PROCEDURE setOutputBehavior(CONST messageTypesToInclude_:T_messageTypeSet);
    public
    adapterType:T_adapterType;

    CONSTRUCTOR create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
    DESTRUCTOR destroy; virtual;
    FUNCTION append(CONST message:P_storedMessage):boolean; virtual; abstract;
    PROCEDURE clear; virtual;
    PROPERTY outputBehavior:T_messageTypeSet read messageTypesToInclude write setOutputBehavior;
    PROCEDURE enableMessageType(CONST enabled:boolean; CONST mt:T_messageTypeSet);
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;
  T_consoleOutAdapter = object(T_abstractOutAdapter)
    CONSTRUCTOR create(CONST messageTypesToInclude_:T_messageTypeSet);
    DESTRUCTOR destroy; virtual;
    FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
  end;

  P_collectingOutAdapter = ^T_collectingOutAdapter;
  T_collectingOutAdapter = object(T_abstractOutAdapter)
    storedMessages:T_storedMessages;
    CONSTRUCTOR create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
    DESTRUCTOR destroy; virtual;
    FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
    PROCEDURE removeDuplicateStoredMessages;
    PROCEDURE clear; virtual;
  end;

  P_messageConnector=^T_messageConnector;
  {$ifdef fullVersion}
  F_traceCallback=PROCEDURE(VAR error:T_errorMessage) of object;
  {$endif}

  P_threadLocalMessages=^T_threadLocalMessages;
  T_threadLocalMessages=object(T_collectingOutAdapter)
    private
      flags:T_stateFlags;
      {$ifdef fullVersion}
      traceCallback:F_traceCallback;
      {$endif}
      parentMessages:P_threadLocalMessages;
      childMessages:array of P_threadLocalMessages;
      PROCEDURE propagateFlags;
    public
      globalMessages:P_messageConnector;
      CONSTRUCTOR create({$ifdef fullVersion}CONST callback:F_traceCallback; {$endif});
      PROCEDURE raiseError(CONST text:string; CONST location:T_searchTokenLocation; CONST kind:T_messageType=mt_el3_evalError);

      PROCEDURE setParent(CONST parent:P_threadLocalMessages);
      PROCEDURE dropChild(CONST child:P_threadLocalMessages);
      PROCEDURE addChild (CONST child:P_threadLocalMessages);
      PROCEDURE postTextMessage(CONST kind:T_messageType; CONST location:T_searchTokenLocation; CONST txt:T_arrayOfString);
      PROPERTY getFlags:T_stateFlags read flags;
      PROCEDURE clear; virtual;
      PROCEDURE escalateErrors;
      FUNCTION continueEvaluation:boolean;
      DESTRUCTOR destroy; virtual;
      FUNCTION childCount:longint;
      PROCEDURE setStopFlag;
      PROCEDURE logGuiNeeded;
  end;

  T_abstractFileOutAdapter = object(T_collectingOutAdapter)
    protected
      outputFileName:ansistring;
      forceRewrite:boolean;
      lastOutput:double;
      FUNCTION switchFile(CONST newFileName:string):boolean; virtual;
      PROCEDURE flush; virtual; abstract;
    public
      CONSTRUCTOR create(CONST typ:T_adapterType; CONST fileName:ansistring; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message: P_storedMessage):boolean; virtual;
  end;

  P_textFileOutAdapter = ^T_textFileOutAdapter;
  T_textFileOutAdapter = object(T_abstractFileOutAdapter)
    protected
      FUNCTION switchFile(CONST newFileName:string):boolean; virtual;
      PROCEDURE flush; virtual;
    public
      CONSTRUCTOR create(CONST fileName:ansistring; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean);
      DESTRUCTOR destroy; virtual;
  end;

  P_connectorAdapter=^T_connectorAdapter;
  T_connectorAdapter = object(T_abstractOutAdapter)
    private
      connectedAdapters:P_messageConnector;
    public
      CONSTRUCTOR create(CONST connected:P_messageConnector; CONST includePrint,includeWarnings,includeErrors:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
  end;

  T_flaggedAdapter=record
    adapter:P_abstractOutAdapter;
    doDispose:boolean;
  end;

  T_messageConnector=object
    private
      connectorCS:TRTLCriticalSection;
      adapters:array of T_flaggedAdapter;
      collecting,collected:T_messageTypeSet;
      userDefinedExitCode:longint;
    public
      preferredEchoLineLength:longint;
      CONSTRUCTOR create;
      DESTRUCTOR destroy;

      PROCEDURE postSingal(CONST kind:T_messageType; CONST location:T_searchTokenLocation);
      PROCEDURE postTextMessage(CONST kind:T_messageType; CONST location:T_searchTokenLocation; CONST txt:T_arrayOfString);
      PROCEDURE postCustomMessage(CONST message:P_storedMessage);
      PROCEDURE postCustomMessages(CONST message:T_storedMessages);

      PROCEDURE clear;

      PROCEDURE updateCollecting;
      FUNCTION isCollecting(CONST messageType:T_messageType):boolean;
      FUNCTION hasCollected(CONST messageType:T_messageType):boolean;
      FUNCTION getAdapter(CONST index:longint):P_abstractOutAdapter;

      PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter; CONST destroyIt:boolean);
      FUNCTION addOutfile(CONST fileNameAndOptions:ansistring; CONST appendMode:boolean=true):P_textFileOutAdapter;
      FUNCTION addConsoleOutAdapter(CONST verbosity:string=''):P_consoleOutAdapter;
      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);

      FUNCTION connectToOther(CONST other:P_messageConnector; CONST includePrint,includeWarnings,includeErrors:boolean):P_connectorAdapter;

      PROCEDURE setUserDefinedExitCode(CONST code:longint);
      PROCEDURE setExitCode;
      FUNCTION triggersBeep:boolean;
  end;

  //T_adapters=object
  //  private
  //    stackTraceCount:longint;
  //    errorCount:longint;
  //    maxErrorLevel: shortint;
  //    adapter:array of P_abstractOutAdapter;
  //
  //    someEchoInput        :boolean;
  //    someEchoDeclaration  :boolean;
  //    someShowExpressionOut:boolean;
  //    someShowTimingInfo   :boolean;
  //    containedMessageTypes:T_messageTypeSet;
  //    {$ifdef fullVersion}
  //    privatePlot:P_plot;
  //    isTryingInstance:boolean;
  //    {$endif}
  //    subAdapters:array of P_adapters;
  //    userDefinedExitCode:longint;
  //    PROCEDURE raiseCustomMessage(CONST message:T_storedMessage);
  //  public
  //    {$ifdef fullVersion}
  //    preferredEchoLineLength:longint;
  //    hasNeedGUIerror:boolean;
  //    {$endif}
  //    CONSTRUCTOR create;
  //    DESTRUCTOR destroy;
  //    PROCEDURE clearErrors;
  //    PROCEDURE raiseError      (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
  //    PROCEDURE raiseWarning    (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
  //    PROCEDURE raiseNote       (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
  //    PROCEDURE raiseUserError  (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
  //    PROCEDURE raiseUserWarning(CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
  //    PROCEDURE raiseUserNote   (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
  //    PROCEDURE raiseSystemError(CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
  //
  //    PROCEDURE echoDeclaration(CONST m:string);
  //    PROCEDURE echoInput      (CONST m:string);
  //    PROCEDURE echoOutput     (CONST m:T_arrayOfString);
  //
  //    PROCEDURE raiseStoredMessages(VAR stored:T_storedMessages);
  //    {$ifdef fullVersion}
  //    PROCEDURE logGuiNeeded;
  //    PROCEDURE logDeferredPlot;
  //    FUNCTION isDeferredPlotLogged:boolean;
  //    PROCEDURE logInstantPlot;
  //    PROCEDURE resetFlagsAfterPlotDone;
  //    PROCEDURE logPlotSettingsChanged;
  //    PROCEDURE logBreakpointEncountered(CONST data:pointer);
  //    PROCEDURE logEndOfEditScript(CONST data:pointer; CONST success:boolean);
  //    PROCEDURE logPlotFileCreated(CONST fileName:string; CONST location:T_searchTokenLocation);
  //    PROCEDURE logDisplayTable;
  //    PROCEDURE logDisplayTreeView;
  //    PROCEDURE logDisplayCustomForm;
  //    {$endif}
  //    PROCEDURE logTimingInfo(CONST infoText:T_arrayOfString);
  //    PROCEDURE logCallStackInfo(CONST infoText:ansistring; CONST location:T_searchTokenLocation);
  //    PROCEDURE logMissingMain;
  //    PROCEDURE printOut(CONST s:T_arrayOfString);
  //    PROCEDURE printDirect(CONST s:T_arrayOfString);
  //    PROCEDURE clearPrint;
  //    PROCEDURE clearAll({$ifdef fullVersion}CONST includePlot:boolean=false{$endif});
  //    PROCEDURE stopEvaluation;
  //    FUNCTION noErrors: boolean; inline;
  //    FUNCTION hasNonSilentError:boolean;
  //    FUNCTION hasHaltMessage(CONST includeQuiet:boolean=true): boolean;
  //    FUNCTION hasFatalError: boolean;
  //    FUNCTION hasStackTrace:boolean;
  //    FUNCTION hasPrintOut:boolean;
  //    PROCEDURE resetErrorFlags;
  //    PROCEDURE updateErrorlevel;
  //    PROCEDURE haltEvaluation;
  //    PROCEDURE logEndOfEvaluation;
  //    PROCEDURE raiseSystemError(CONST errorMessage: ansistring);
  //
  //
  //    PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter; CONST destroyIt:boolean);
  //    FUNCTION addConsoleOutAdapter(CONST verbosity:string=''):P_consoleOutAdapter;
  //    PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);
  //    PROCEDURE removeOutAdapter(CONST index:longint);
  //    PROCEDURE setPrintTextFileAdapter(CONST filenameOrBlank:string);
  //
  //    FUNCTION getAdapter(CONST index:longint):P_abstractOutAdapter;
  //    FUNCTION getAdapter(CONST adapterType:T_adapterType):P_abstractOutAdapter;
  //
  //    PROCEDURE setUserDefinedExitCode(CONST code:longint);
  //    PROCEDURE setExitCode;
  //    FUNCTION triggersBeep:boolean;
  //
  //    PROPERTY doEchoInput:         boolean read someEchoInput        ;
  //    PROPERTY doEchoDeclaration:   boolean read someEchoDeclaration  ;
  //    PROPERTY doShowExpressionOut: boolean read someShowExpressionOut;
  //    PROPERTY doShowTimingInfo:    boolean read someShowTimingInfo   ;
  //
  //    {$ifdef fullVersion}
  //    FUNCTION plot:P_plot;
  //    {$endif}
  //    PROCEDURE addSubAdapters(CONST sub:P_adapters);
  //    PROCEDURE remSubAdapters(CONST sub:P_adapters);
  //
  //    FUNCTION getAdaptersForTry(OUT errorInterceptor:P_errorInterceptor):P_adapters;
  //end;

CONST
  C_defaultOutputBehavior_interactive:T_messageTypeSet=[mt_clearConsole,
    mt_printline,
    mt_printdirect,
    mt_echo_input,
    mt_echo_declaration,
    mt_echo_output,
    mt_echo_continued,
    mt_el3_evalError..high(T_messageTypeSet)];

  C_defaultOutputBehavior_fileMode:T_messageTypeSet=[mt_clearConsole,mt_printline,mt_printdirect,mt_el3_evalError..mt_endOfEvaluation];
  C_collectAllOutputBehavior:T_messageTypeSet=[low(T_messageType)..high(T_messageType)];

VAR
  defaultOutputBehavior:T_messageTypeSet;
{$ifdef fullVersion}
  gui_started:boolean=false;
{$endif}
OPERATOR :=(s:string):T_messageTypeSet;
IMPLEMENTATION
VAR globalLockCs:TRTLCriticalSection;

OPERATOR :=(s:string):T_messageTypeSet;
  VAR i,level:longint;
      mt:T_messageType;
  begin
    result:=defaultOutputBehavior;
    for i:=1 to length(s) do case s[i] of
      'v': result:=C_collectAllOutputBehavior;
      'V': result:=[];
      'u': result:=result+[mt_el1_userNote,mt_el2_userWarning,mt_el3_userDefined];
      'U': result:=result-[mt_el1_userNote,mt_el2_userWarning,mt_el3_userDefined];
      'p': result:=result+[mt_printline,mt_printdirect,mt_clearConsole];
      'P': result:=result-[mt_printline,mt_printdirect,mt_clearConsole];
      'i': result:=result+[mt_echo_input];
      'I': result:=result-[mt_echo_input];
      'd': result:=result+[mt_echo_declaration];
      'D': result:=result-[mt_echo_declaration];
      'o': result:=result+[mt_echo_output];
      'O': result:=result-[mt_echo_output];
      't': result:=result+[mt_timing_info];
      'T': result:=result-[mt_timing_info];
      'e': result:=result+[mt_echo_input,mt_echo_declaration,mt_echo_output];
      'E': result:=result-[mt_echo_input,mt_echo_declaration,mt_echo_output];
      'n': result:=result+[mt_el1_note,mt_el1_userNote];
      'N': result:=result-[mt_el1_note,mt_el1_userNote];
      'w': result:=result+[mt_el2_warning,mt_el2_userWarning];
      'W': result:=result-[mt_el2_warning,mt_el2_userWarning];
      '1'..'4': begin
        level:=strToInt(s[i]);
        for mt:=low(T_messageType) to high(T_messageType) do if C_messageTypeMeta[mt].level>0 then begin
          if C_messageTypeMeta[mt].level>=level then result:=result+[mt]
                                                else result:=result-[mt];
        end;
      end;
    end;
  end;

CONSTRUCTOR T_messageConnector.create;
  begin
    initCriticalSection(connectorCS);
    setLength(adapters,0);
  end;

DESTRUCTOR T_messageConnector.destroy;
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(connectorCS);
    for a in adapters do if a.doDispose then dispose(a.adapter,destroy);
    setLength(adapters,0);
    leaveCriticalSection(connectorCS);
    doneCriticalSection(connectorCS);
  end;

CONSTRUCTOR T_threadLocalMessages.create({$ifdef fullVersion} CONST callback: F_traceCallback;{$endif});
  begin
    inherited create(at_unknown,[mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined,mt_el4_systemError]);
    globalMessages       :=nil;
    parentMessages       :=nil;
    setLength(childMessages,0);
    {$ifdef fullVersion}
    traceCallback:=callback;
    {$endif}
  end;

PROCEDURE T_threadLocalMessages.propagateFlags;
  VAR child:P_threadLocalMessages;
  begin
    if ((FlagFatalError in flags) or (FlagGUINeeded in flags)) and (parentMessages<>nil) then begin
      parentMessages^.flags:=parentMessages^.flags+(flags*[FlagFatalError,FlagGUINeeded]);
      parentMessages^.propagateFlags;
    end;
    for child in childMessages do begin
      child^.flags:=child^.flags+flags;
      child^.propagateFlags;
    end;
  end;

PROCEDURE T_threadLocalMessages.raiseError(CONST text: string; CONST location: T_searchTokenLocation; CONST kind: T_messageType);
  VAR message:P_errorMessage;
  begin
    new(message,create(kind,location,split(text,C_lineBreakChar)));
    {$ifdef fullVersion}
    if traceCallback<>nil then traceCallback(message^);
    {$endif}
    flags:=flags+C_messageClassMeta[message^.messageClass].triggeredFlags;
    propagateFlags;
    if kind=mt_el4_systemError then escalateErrors;
    append(message);
    disposeMessage(message);
  end;

PROCEDURE T_threadLocalMessages.postTextMessage(CONST kind: T_messageType; CONST location: T_searchTokenLocation; CONST txt: T_arrayOfString);
  begin
    if globalMessages<>nil then globalMessages^.postTextMessage(kind,location,txt);
  end;

PROCEDURE T_threadLocalMessages.clear;
  begin
    inherited clear;
    flags:=[];
  end;

PROCEDURE T_threadLocalMessages.escalateErrors;
  VAR m:P_storedMessage;
  begin
    if parentMessages<>nil then begin
      for m in storedMessages do parentMessages^.append(m);
      parentMessages^.flags:=parentMessages^.flags+flags;
      exit;
    end;
    if globalMessages<>nil then for m in storedMessages do globalMessages^.postCustomMessage(m);
  end;

FUNCTION T_threadLocalMessages.continueEvaluation: boolean;
  begin
    result:=flags=[];
  end;

DESTRUCTOR T_threadLocalMessages.destroy;
  begin
    clear;
    if parentMessages<>nil then parentMessages^.dropChild(@self);
    inherited destroy;
  end;

PROCEDURE T_threadLocalMessages.setParent(CONST parent:P_threadLocalMessages);
  begin
    if parent=parentMessages then exit;
    enterCriticalSection(cs);
    if parentMessages<>nil then parentMessages^.dropChild(@self);
    parentMessages:=parent;
    if parent<>nil then begin
      parent^.addChild(@self);
      globalMessages:=parent^.globalMessages;
    end;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_threadLocalMessages.dropChild(CONST child: P_threadLocalMessages);
  VAR i:longint=0;
  begin
    enterCriticalSection(cs);
    while i<length(childMessages) do
    if childMessages[i]=child then begin
      childMessages[i]:=childMessages[length(childMessages)-1];
      setLength(childMessages,length(childMessages)-1);
    end else inc(i);
    leaveCriticalSection(cs);
  end;

PROCEDURE T_threadLocalMessages.addChild(CONST child: P_threadLocalMessages);
  VAR i:longint=0;
  begin
    enterCriticalSection(cs);
    while (i<length(childMessages)) and (childMessages[i]<>child) do inc(i);
    if i=length(childMessages) then begin
      setLength(childMessages,i+1);
      childMessages[i]:=child;
    end;
    leaveCriticalSection(cs);
  end;

FUNCTION T_threadLocalMessages.childCount:longint;
  begin
    enterCriticalSection(cs);
    result:=length(childMessages);
    leaveCriticalSection(cs);
  end;

PROCEDURE T_threadLocalMessages.setStopFlag;
  begin
    enterCriticalSection(cs);
    include(flags,FlagQuietHalt);
    leaveCriticalSection(cs);
    propagateFlags;
  end;

PROCEDURE T_threadLocalMessages.logGuiNeeded;
  begin
    enterCriticalSection(cs);
    include(flags,FlagQuietHalt);
    include(flags,FlagGUINeeded);
    leaveCriticalSection(cs);
    propagateFlags;
  end;

PROCEDURE T_messageConnector.postSingal(CONST kind:T_messageType; CONST location:T_searchTokenLocation);
  VAR message:P_storedMessage;
  begin
    new(message,create(kind,location));
    postCustomMessage(message);
    disposeMessage(message);
  end;

PROCEDURE T_messageConnector.postTextMessage(CONST kind: T_messageType; CONST location: T_searchTokenLocation; CONST txt: T_arrayOfString);
  VAR message:P_storedMessageWithText;
  begin
    new(message,create(kind,location,txt));
    postCustomMessage(message);
    disposeMessage(message);
  end;

PROCEDURE T_messageConnector.postCustomMessage(CONST message: P_storedMessage);
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(connectorCS);
    for a in adapters do if a.adapter^.append(message) then include(collected,message^.messageType);
    leaveCriticalSection(connectorCS);
  end;

PROCEDURE T_messageConnector.postCustomMessages(CONST message:T_storedMessages);
  VAR a:T_flaggedAdapter;
      m:P_storedMessage;
      anyCollected:boolean;
  begin
    enterCriticalSection(connectorCS);
    for m in message do begin
      anyCollected:=false;
      for a in adapters do if a.adapter^.append(m) then anyCollected:=true;
      if anyCollected then include(collected,m^.messageType);
    end;
    leaveCriticalSection(connectorCS);
  end;

PROCEDURE T_messageConnector.clear;
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(connectorCS);
    for a in adapters do a.adapter^.clear;
    collected:=[];
    userDefinedExitCode:=0;
    leaveCriticalSection(connectorCS);
  end;

PROCEDURE T_messageConnector.updateCollecting;
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(connectorCS);
    collecting:=[];
    for a in adapters do collecting:=collecting + a.adapter^.messageTypesToInclude;
    leaveCriticalSection(connectorCS);
  end;

FUNCTION T_messageConnector.isCollecting(CONST messageType: T_messageType): boolean;
  begin
    result:=messageType in collecting;
  end;

FUNCTION T_messageConnector.hasCollected(CONST messageType:T_messageType):boolean;
  begin
    result:=messageType in collected;
  end;

FUNCTION T_messageConnector.getAdapter(CONST index:longint):P_abstractOutAdapter;
  begin
    result:=adapters[index].adapter;
  end;

//CONSTRUCTOR T_errorInterceptor.create(CONST parent: P_adapters);
//  begin
//    inherited create(at_unknown,[low(T_messageType)..high(T_messageType)]);
//    parentAdapters:=parent;
//  end;
//
//FUNCTION T_errorInterceptor.append(CONST message: T_storedMessage): boolean;
//  begin
//    if C_messageTypeMeta[message.messageType].level=3
//    then result:=inherited append(message)
//    else begin
//      result:=true;
//      parentAdapters^.raiseCustomMessage(message);
//    end;
//  end;
//
//FUNCTION T_errorInterceptor.hasError: boolean;
//  begin
//    result:=length(storedMessages)>0;
//  end;

CONSTRUCTOR T_connectorAdapter.create(CONST connected: P_messageConnector; CONST includePrint,includeWarnings,includeErrors: boolean);
  VAR toInclude:T_messageTypeSet=[];
  begin
    if includePrint    then include(toInclude,mt_printline);
    if includeWarnings then toInclude:=toInclude+[mt_el1_note..mt_el2_userWarning];
    if includeErrors   then toInclude:=toInclude+[mt_el3_evalError..mt_el4_systemError];
    inherited create(at_unknown,toInclude);
    connectedAdapters:=connected;
  end;

DESTRUCTOR T_connectorAdapter.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_connectorAdapter.append(CONST message: P_storedMessage): boolean;
  begin
    if not(message^.messageType in messageTypesToInclude) then exit(false);
    connectedAdapters^.postCustomMessage(message^.rereferenced);
    result:=true;
  end;

//T_abstractOutAdapter:=========================================================
PROCEDURE T_abstractOutAdapter.enableMessageType(CONST enabled: boolean;
  CONST mt: T_messageTypeSet);
  begin
    if enabled
    then messageTypesToInclude:=(messageTypesToInclude+mt) * C_includableMessages[adapterType]
    else messageTypesToInclude:=(messageTypesToInclude-mt) * C_includableMessages[adapterType];
  end;

CONSTRUCTOR T_abstractOutAdapter.create(CONST typ: T_adapterType;
  CONST messageTypesToInclude_: T_messageTypeSet);
  begin
    adapterType:=typ;
    setOutputBehavior(messageTypesToInclude_);
    system.initCriticalSection(cs);
  end;

DESTRUCTOR T_abstractOutAdapter.destroy;
  begin
    system.doneCriticalSection(cs);
  end;

PROCEDURE T_abstractOutAdapter.clear; begin end;

PROCEDURE T_abstractOutAdapter.setOutputBehavior(
  CONST messageTypesToInclude_: T_messageTypeSet);
  begin
     messageTypesToInclude:=C_includableMessages[adapterType]*messageTypesToInclude_;
  end;

//=========================================================:T_abstractOutAdapter
//T_consoleOutAdapter:==========================================================
CONSTRUCTOR T_consoleOutAdapter.create(CONST messageTypesToInclude_:T_messageTypeSet);
  begin
    inherited create(at_console,messageTypesToInclude_);
  end;

DESTRUCTOR T_consoleOutAdapter.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_consoleOutAdapter.append(CONST message:P_storedMessage):boolean;
  VAR i:longint;
      s:string;
  begin
    result:=message^.messageType in messageTypesToInclude;
    if result then with message^ do begin
      enterCriticalSection(cs);
      case messageType of
        mt_clearConsole: mySys.clearConsole;
        mt_printline: begin
          if not(mySys.isConsoleShowing) then mySys.showConsole;
          for i:=0 to length(messageText)-1 do begin
            if messageText[i]=C_formFeedChar
            then mySys.clearConsole
            else writeln(messageText[i]);
          end;
        end;
        mt_printdirect: begin
          if not(mySys.isConsoleShowing) then mySys.showConsole;
          for i:=0 to length(messageText)-1 do write(messageText[i]);
        end;
        else for s in message^.toString({$ifdef fullVersion}false{$endif}) do writeln(stdErr,s);
      end;
      leaveCriticalSection(cs);
    end;
  end;
//==========================================================:T_consoleOutAdapter
//T_collectingOutAdapter:=======================================================
CONSTRUCTOR T_collectingOutAdapter.create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
  begin
    inherited create(typ,messageTypesToInclude_);
    setLength(storedMessages,0);
  end;

DESTRUCTOR T_collectingOutAdapter.destroy;
  begin
    clear;
    inherited destroy;
  end;

FUNCTION T_collectingOutAdapter.append(CONST message: P_storedMessage):boolean;
  begin
    result:=message^.messageType in messageTypesToInclude;
    if result then begin
      system.enterCriticalSection(cs);
      setLength(storedMessages,length(storedMessages)+1);
      storedMessages[length(storedMessages)-1]:=message^.rereferenced;
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_collectingOutAdapter.removeDuplicateStoredMessages;
  VAR i,j,k:longint;
      isDuplicate:boolean=false;
  begin
    k:=1;
    for j:=1 to length(storedMessages)-1 do begin
      isDuplicate:=false;
      for i:=0 to k-1 do isDuplicate:=isDuplicate or storedMessages[i]^.equals(storedMessages[k]);
      if not(isDuplicate) then begin
        storedMessages[k]:=storedMessages[j];
        inc(k);
      end else disposeMessage(storedMessages[j]);
    end;
    setLength(storedMessages,k);
  end;

PROCEDURE T_collectingOutAdapter.clear;
  VAR m:P_storedMessage;
  begin
    system.enterCriticalSection(cs);
    inherited clear;
    for m in storedMessages do disposeMessage(m);
    setLength(storedMessages,0);
    system.leaveCriticalSection(cs);
  end;
//=======================================================:T_collectingOutAdapter
//T_abstractFileOutAdapter:=====================================================
CONSTRUCTOR T_abstractFileOutAdapter.create(CONST typ:T_adapterType; CONST fileName:ansistring; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean);
  begin
    inherited create(typ,messageTypesToInclude_);
    outputFileName:=expandFileName(fileName);
    forceRewrite:=forceNewFile;
    lastOutput:=now;
  end;

DESTRUCTOR T_abstractFileOutAdapter.destroy;
  begin
    flush();
    inherited destroy;
  end;

FUNCTION T_abstractFileOutAdapter.append(CONST message: P_storedMessage):boolean;
  {$ifndef debugMode} CONST flushAt=10/(24*60*60); {$endif}//=10 seconds
  begin
    result:=inherited append(message);
    if result {$ifndef debugMode} and ((now>lastOutput+flushAt) or (length(storedMessages)>=100)) {$endif} then begin
      lastOutput:=now;
      flush();
    end;
  end;

FUNCTION T_abstractFileOutAdapter.switchFile(CONST newFileName:string):boolean;
  VAR newFullFileName:string;
  begin
    enterCriticalSection(cs);
    newFullFileName:=expandFileName(newFileName);
    if newFileName=outputFileName then begin
      leaveCriticalSection(cs);
      exit(false);
    end;
    lastOutput:=now;
    flush();
    outputFileName:=newFullFileName;
    result:=true;
    leaveCriticalSection(cs);
  end;
//=====================================================:T_abstractFileOutAdapter
//T_textFileOutAdapter:=========================================================
FUNCTION T_textFileOutAdapter.switchFile(CONST newFileName: string):boolean;
  begin
    result:=inherited switchFile(newFileName);
  end;

PROCEDURE T_textFileOutAdapter.flush;
  VAR handle:text;
      s:string;
      m:P_storedMessage;
  begin
    enterCriticalSection(cs);
    if length(storedMessages)>0 then begin
      try
        assign(handle,outputFileName);
        if fileExists(outputFileName) and not(forceRewrite)
        then system.append(handle)
        else rewrite(handle);
        forceRewrite:=false;
        for m in storedMessages do begin
          case m^.messageType of
            mt_printline  : for s in m^.messageText do writeln(handle,s);
            mt_printdirect: for s in m^.messageText do write  (handle,s);
            else for s in m^.toString({$ifdef fullVersion}false{$endif}) do writeln(handle,s);
          end;
        end;
        clear;
        close(handle);
      except
      end;
    end;
    leaveCriticalSection(cs);
  end;

CONSTRUCTOR T_textFileOutAdapter.create(CONST fileName: ansistring; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean);
  begin
    inherited create(at_textFile,fileName,messageTypesToInclude_,forceNewFile);
  end;

DESTRUCTOR T_textFileOutAdapter.destroy;
  begin
    inherited destroy;
  end;
//=========================================================:T_textFileOutAdapter
////T_adapters:===================================================================
//CONSTRUCTOR T_adapters.create;
//  begin
//    {$ifdef fullVersion}
//    privatePlot:=nil;
//    isTryingInstance:=false;
//    preferredEchoLineLength:=-1;
//    {$endif}
//    setLength(adapter,0);
//    setLength(subAdapters,0);
//    clearAll;
//  end;
//
//DESTRUCTOR T_adapters.destroy;
//  VAR i:longint;
//  begin
//    for i:=0 to length(adapter)-1 do if adapter[i]^.autodestruct then dispose(adapter[i],destroy);
//    setLength(adapter,0);
//    {$ifdef fullVersion}
//    if (privatePlot<>nil) and not(isTryingInstance) then dispose(privatePlot,destroy);
//    {$endif}
//  end;
//
//PROCEDURE T_adapters.clearErrors;
//  VAR mt:T_messageType;
//  begin
//    containedMessageTypes:=[];
//    for mt:=low(T_messageType) to high(T_messageType) do begin
//      if   maxErrorLevel>=C_messageTypeMeta[mt].level
//      then maxErrorLevel:=C_messageTypeMeta[mt].level-1;
//    end;
//    {$ifdef fullVersion}
//    hasNeedGUIerror:=false;
//    {$endif}
//    stackTraceCount:=0;
//    errorCount:=0;
//  end;
//
//PROCEDURE T_adapters.raiseCustomMessage(CONST message: T_storedMessage);
//  VAR i:longint;
//      sub:P_adapters;
//  begin
//    {$ifdef fullVersion}
//    hasNeedGUIerror:=hasNeedGUIerror or not(gui_started) and C_messageTypeMeta[message.messageType].triggersGuiStartup;
//    {$endif}
//    if maxErrorLevel< C_messageTypeMeta[message.messageType].level then
//       maxErrorLevel:=C_messageTypeMeta[message.messageType].level;
//    if maxErrorLevel>=3 then begin
//      enterCriticalSection(globalLockCs);
//      for sub in subAdapters do sub^.haltEvaluation;
//      leaveCriticalSection(globalLockCs);
//    end;
//    if hasHaltMessage and not(message.messageType in [mt_endOfEvaluation,mt_timing_info{$ifdef fullVersion},mt_displayTable]+C_guiOnlyMessages{$else}]{$endif}) then exit;
//    include(containedMessageTypes,message.messageType);
//    if (message.messageType=mt_el3_stackTrace) then begin
//      inc(stackTraceCount);
//      if stackTraceCount>30 then exit;
//    end;
//    if (message.messageType in [mt_el3_evalError,mt_el3_noMatchingMain,mt_el4_haltMessageReceived,mt_el4_systemError]) then begin
//      inc(errorCount);
//      if errorCount>30 then exit;
//    end;
//    for i:=0 to length(adapter)-1 do adapter[i]^.append(message);
//  end;
//
//PROCEDURE T_adapters.raiseError      (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el3_evalError   ,errorMessage        ,errorLocation)); end;
//PROCEDURE T_adapters.raiseWarning    (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el2_warning     ,errorMessage        ,errorLocation)); end;
//PROCEDURE T_adapters.raiseNote       (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el1_note        ,errorMessage        ,errorLocation)); end;
//PROCEDURE T_adapters.raiseUserError  (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el3_userDefined ,errorMessage        ,errorLocation)); end;
//PROCEDURE T_adapters.raiseUserWarning(CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el2_userWarning ,errorMessage        ,errorLocation)); end;
//PROCEDURE T_adapters.raiseUserNote   (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el1_userNote    ,errorMessage        ,errorLocation)); end;
//PROCEDURE T_adapters.raiseSystemError(CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el4_systemError ,errorMessage        ,errorLocation)); end;
//PROCEDURE T_adapters.logTimingInfo    (CONST infoText:T_arrayOfString);                                                 begin raiseCustomMessage(message(mt_timing_info     ,infoText            ,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.logCallStackInfo (CONST infoText:ansistring; CONST location:T_searchTokenLocation);                begin raiseCustomMessage(message(mt_el3_stackTrace  ,infoText            ,location)); end;
//PROCEDURE T_adapters.printOut         (CONST s: T_arrayOfString);                                                       begin raiseCustomMessage(message(mt_printline       ,s                   ,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.printDirect      (CONST s: T_arrayOfString);                                                       begin raiseCustomMessage(message(mt_printdirect     ,s                   ,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.clearPrint;                                                                                        begin raiseCustomMessage(message(mt_clearConsole    ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.echoDeclaration(CONST m:string);                                                                   begin raiseCustomMessage(message(mt_echo_declaration,m                   ,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.echoInput      (CONST m:string);                                                                   begin raiseCustomMessage(message(mt_echo_input      ,m                   ,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.echoOutput     (CONST m:T_arrayOfString);                                                          begin raiseCustomMessage(message(mt_echo_output     ,m                   ,C_nilTokenLocation)); end;
//
//PROCEDURE T_adapters.logMissingMain;
//  begin
//    include(containedMessageTypes,mt_el3_noMatchingMain);
//  end;
//
//PROCEDURE T_adapters.raiseStoredMessages(VAR stored:T_storedMessages);
//  VAR m:T_storedMessage;
//  begin for m in stored do raiseCustomMessage(m); end;
//
//{$ifdef fullVersion}
//PROCEDURE T_adapters.logGuiNeeded;                begin hasNeedGUIerror:=true; end;
//FUNCTION T_adapters.isDeferredPlotLogged:boolean; begin result:=mt_plotCreatedWithDeferredDisplay in containedMessageTypes; end;
//PROCEDURE T_adapters.resetFlagsAfterPlotDone;     begin containedMessageTypes:=containedMessageTypes-[mt_plotCreatedWithDeferredDisplay,mt_plotCreatedWithInstantDisplay ]; end;
//PROCEDURE T_adapters.logInstantPlot;              begin containedMessageTypes:=containedMessageTypes-[mt_plotCreatedWithDeferredDisplay];
//                                                                                                            raiseCustomMessage(message(mt_plotCreatedWithInstantDisplay ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation));
//                                                  end;
//PROCEDURE T_adapters.logDeferredPlot;                                                                 begin raiseCustomMessage(message(mt_plotCreatedWithDeferredDisplay,C_EMPTY_STRING_ARRAY,C_nilTokenLocation));  end;
//PROCEDURE T_adapters.logPlotSettingsChanged;                                                          begin raiseCustomMessage(message(mt_plotSettingsChanged           ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.logPlotFileCreated(CONST fileName:string; CONST location:T_searchTokenLocation); begin raiseCustomMessage(message(mt_plotFileCreated               ,fileName            ,location          )); end;
//PROCEDURE T_adapters.logBreakpointEncountered(CONST data: pointer);                                   begin raiseCustomMessage(message(mt_gui_breakpointEncountered,C_EMPTY_STRING_ARRAY,C_nilTokenLocation,data)); end;
//PROCEDURE T_adapters.logEndOfEditScript(CONST data: pointer; CONST success: boolean);
//  CONST mtOfSuccess:array[false..true] of T_messageType=(mt_gui_editScriptFailed,mt_gui_editScriptSucceeded);
//  begin
//    raiseCustomMessage(message(mtOfSuccess[success],C_EMPTY_STRING_ARRAY,C_nilTokenLocation,data));
//  end;
//PROCEDURE T_adapters.logDisplayTable;                                                                 begin raiseCustomMessage(message(mt_displayTable                  ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.logDisplayTreeView;                                                              begin raiseCustomMessage(message(mt_displayTreeView               ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.logDisplayCustomForm;                                                            begin raiseCustomMessage(message(mt_displayCustomDialog           ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
//{$endif}
//
//PROCEDURE T_adapters.clearAll({$ifdef fullVersion}CONST includePlot:boolean=false{$endif});
//  VAR i:longint;
//  begin
//    userDefinedExitCode:=0;
//    clearErrors;
//    clearPrint;
//    someEchoInput        :=false;
//    someEchoDeclaration  :=false;
//    someShowExpressionOut:=false;
//    someShowTimingInfo   :=false;
//    for i:=0 to length(adapter)-1 do begin
//      adapter[i]^.clear;
//      someEchoInput        :=someEchoInput         or (mt_echo_input       in adapter[i]^.messageTypesToInclude);
//      someEchoDeclaration  :=someEchoDeclaration   or (mt_echo_declaration in adapter[i]^.messageTypesToInclude);
//      someShowExpressionOut:=someShowExpressionOut or (mt_echo_output      in adapter[i]^.messageTypesToInclude);
//      someShowTimingInfo   :=someShowTimingInfo    or (mt_timing_info      in adapter[i]^.messageTypesToInclude);
//    end;
//    {$ifdef fullVersion}
//    if includePlot and (privatePlot<>nil) then privatePlot^.setDefaults;
//    {$endif}
//  end;
//
//PROCEDURE T_adapters.stopEvaluation;
//  begin
//    include(containedMessageTypes,mt_el4_haltMessageQuiet);
//    maxErrorLevel:=5;
//  end;
//
//FUNCTION T_adapters.noErrors: boolean;
//  begin
//    result:=(maxErrorLevel<3)
//    {$ifdef fullVersion}
//    and not(hasNeedGUIerror)
//    {$endif};
//  end;
//
//FUNCTION T_adapters.hasNonSilentError:boolean;
//  begin
//    result:=containedMessageTypes*[mt_el3_evalError,
//                                   mt_el3_noMatchingMain,
//                                   mt_el3_userDefined,
//                                   mt_el4_systemError]<>[];
//  end;
//
//FUNCTION T_adapters.hasHaltMessage(CONST includeQuiet:boolean=true):boolean;
//  begin
//    result:=(mt_el4_haltMessageReceived in containedMessageTypes) or
//            (mt_el4_haltMessageQuiet in containedMessageTypes) and includeQuiet;
//  end;
//
//FUNCTION T_adapters.hasFatalError: boolean;
//  begin
//    result:=containedMessageTypes*[mt_el4_haltMessageQuiet,mt_el4_haltMessageReceived,mt_el4_systemError]<>[];
//  end;
//
//FUNCTION T_adapters.hasStackTrace:boolean;
//  begin
//    result:=mt_el3_stackTrace in containedMessageTypes;
//  end;
//
//FUNCTION T_adapters.hasPrintOut:boolean;
//  begin
//    result:=containedMessageTypes*[mt_printline,mt_printdirect]<>[];
//  end;
//
//PROCEDURE T_adapters.resetErrorFlags;
//  begin
//    maxErrorLevel:=0;
//    containedMessageTypes:=[];
//  end;
//
//
//PROCEDURE T_adapters.haltEvaluation;                                   begin raiseCustomMessage(message(mt_el4_haltMessageReceived,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.logEndOfEvaluation;                               begin raiseCustomMessage(message(mt_endOfEvaluation        ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
//PROCEDURE T_adapters.raiseSystemError(CONST errorMessage: ansistring); begin raiseCustomMessage(message(mt_el4_systemError        ,errorMessage        ,C_nilTokenLocation)); end;
//
FUNCTION T_messageConnector.addOutfile(CONST fileNameAndOptions:ansistring; CONST appendMode:boolean=true):P_textFileOutAdapter;
  VAR fileName:string;
      options:string='';
  begin
    if pos('(',fileNameAndOptions)>0 then begin
      options :=copy(fileNameAndOptions,  pos('(',fileNameAndOptions),length(fileNameAndOptions));
      fileName:=copy(fileNameAndOptions,1,pos('(',fileNameAndOptions)-1);
    end else fileName:=fileNameAndOptions;

    new(result,create(fileName,options,not(appendMode)));
    addOutAdapter(result,true);
  end;

PROCEDURE T_messageConnector.addOutAdapter(CONST p: P_abstractOutAdapter; CONST destroyIt: boolean);
  begin
    enterCriticalSection(connectorCS);
    setLength(adapters,length(adapters)+1);
    adapters[length(adapters)-1].adapter:=p;
    adapters[length(adapters)-1].doDispose:=destroyIt;
    collecting:=collecting+p^.messageTypesToInclude;
    leaveCriticalSection(connectorCS);
  end;

FUNCTION T_messageConnector.connectToOther(CONST other:P_messageConnector; CONST includePrint,includeWarnings,includeErrors:boolean):P_connectorAdapter;
  begin
    new(result,create(@self,includePrint,includeWarnings,includeErrors));
    other^.addOutAdapter(result,true);
  end;

FUNCTION T_messageConnector.addConsoleOutAdapter(CONST verbosity:string=''):P_consoleOutAdapter;
  VAR consoleOutAdapter:P_consoleOutAdapter;
  begin
    new(consoleOutAdapter,create(verbosity));
    addOutAdapter(consoleOutAdapter,true);
    result:=consoleOutAdapter;
  end;

PROCEDURE T_messageConnector.removeOutAdapter(CONST p:P_abstractOutAdapter);
  VAR i:longint=0;
  begin
    enterCriticalSection(connectorCS);
    while i<length(adapters) do if adapters[i].adapter=p then begin
      if adapters[i].doDispose then dispose(adapters[i].adapter,destroy);
      adapters[i]:=adapters[length(adapters)-1];
      setLength(   adapters,length(adapters)-1);
    end else inc(i);
    leaveCriticalSection(connectorCS);
  end;

//
//PROCEDURE T_adapters.removeOutAdapter(CONST p: P_abstractOutAdapter);
//  VAR i:longint;
//  begin
//    for i:=0 to length(adapter)-1 do if adapter[i]=p then begin
//      removeOutAdapter(i);
//      exit;
//    end;
//  end;
//
//PROCEDURE T_adapters.removeOutAdapter(CONST index:longint);
//  VAR j:longint;
//  begin
//    if (index<0) or (index>=length(adapter)) then exit;
//    if adapter[index]^.autodestruct then dispose(adapter[index],destroy);
//    for j:=index to length(adapter)-2 do adapter[j]:=adapter[j+1];
//    setLength(adapter,length(adapter)-1);
//  end;
//
//PROCEDURE T_adapters.setPrintTextFileAdapter(CONST filenameOrBlank:string);
//  VAR currentAdapterIndex:longint;
//      txtAdapter:P_textFileOutAdapter;
//  begin
//    currentAdapterIndex:=length(adapter)-1;
//    while (currentAdapterIndex>=0) and (adapter[currentAdapterIndex]^.adapterType<>at_printTextFileAtRuntime) do dec(currentAdapterIndex);
//
//    if isBlank(filenameOrBlank) then begin
//      if currentAdapterIndex>=0 then removeOutAdapter(currentAdapterIndex);
//      //...else there is no adapter to be removed
//    end else begin
//      if currentAdapterIndex>=0 then P_textFileOutAdapter(adapter[currentAdapterIndex])^.switchFile(filenameOrBlank)
//      else begin
//        new(txtAdapter,create(filenameOrBlank,defaultOutputBehavior,false));
//        txtAdapter^.adapterType:=at_printTextFileAtRuntime;
//        txtAdapter^.messageTypesToInclude:=[mt_printline,mt_printdirect];
//        addOutAdapter(txtAdapter,true);
//      end;
//    end;
//  end;
//
//FUNCTION T_adapters.getAdapter(CONST index: longint): P_abstractOutAdapter;
//  begin
//    result:=adapter[index];
//  end;
//
//FUNCTION T_adapters.getAdapter(CONST adapterType:T_adapterType):P_abstractOutAdapter;
//  VAR a:P_abstractOutAdapter;
//  begin
//    for a in adapter do if a^.adapterType=adapterType then exit(a);
//    result:=nil;
//  end;
//
PROCEDURE T_messageConnector.setUserDefinedExitCode(CONST code:longint);
  begin
    userDefinedExitCode:=code;
  end;

PROCEDURE T_messageConnector.setExitCode;
  VAR mt:T_messageType;
      code:longint=0;
  begin
    code:=userDefinedExitCode;
    for mt in collected do if (C_messageTypeMeta[mt].systemErrorLevel>code) then code:=C_messageTypeMeta[mt].systemErrorLevel;
    ExitCode:=code;
  end;

FUNCTION T_messageConnector.triggersBeep:boolean;
  VAR mt:T_messageType;
  begin
    for mt in collected do if (C_messageTypeMeta[mt].systemErrorLevel>0) then begin
      {$ifdef debugMode}
      writeln(stdErr,'        DEBUG: Beep triggered by message type ',mt);
      {$endif}
      exit(true);
    end;
    result:=false;
  end;
//
//{$ifdef fullVersion}
//FUNCTION T_adapters.plot:P_plot;
//  begin
//    if privatePlot=nil then begin
//      enterCriticalSection(globalLockCs);
//      if privatePlot=nil then new(privatePlot,createWithDefaults);
//      leaveCriticalSection(globalLockCs);
//    end;
//    result:=privatePlot;
//  end;
//{$endif}
//
//PROCEDURE T_adapters.addSubAdapters(CONST sub:P_adapters);
//  VAR s:P_adapters;
//  begin
//    enterCriticalSection(globalLockCs);
//    for s in subAdapters do if s=sub then begin
//      leaveCriticalSection(globalLockCs);
//      exit;
//    end;
//    setLength(subAdapters,length(subAdapters)+1);
//    subAdapters[length(subAdapters)-1]:=sub;
//    leaveCriticalSection(globalLockCs);
//  end;
//
//PROCEDURE T_adapters.remSubAdapters(CONST sub:P_adapters);
//  VAR i:longint;
//  begin
//    enterCriticalSection(globalLockCs);
//    for i:=0 to length(subAdapters)-1 do if subAdapters[i]=sub then begin
//      subAdapters[i]:=subAdapters[length(subAdapters)-1];
//      setLength(subAdapters,length(subAdapters)-1);
//      leaveCriticalSection(globalLockCs);
//      exit;
//    end;
//    leaveCriticalSection(globalLockCs);
//  end;
//
//FUNCTION T_adapters.getAdaptersForTry(OUT errorInterceptor:P_errorInterceptor):P_adapters;
//  begin
//    new(result,create);
//    {$ifdef fullVersion}
//    result^.isTryingInstance:=true;
//    result^.privatePlot:=plot;
//    {$endif}
//    new(errorInterceptor,create(@self));
//    result^.addOutAdapter(errorInterceptor,true);
//  end;
//===================================================================:T_adapters

INITIALIZATION
  defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;
  initialize(globalLockCs);
  initCriticalSection(globalLockCs);

FINALIZATION
  doneCriticalSection(globalLockCs);
end.
