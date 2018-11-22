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
                 {$ifdef fullVersion}
                 at_gui,
                 at_plot,
                 {$endif}
                 at_sandboxAdapter,
                 at_printTextFileAtRuntime);
CONST
  C_includableMessages:array[T_adapterType] of T_messageTypeSet=(
    {at_unknown}  [low(T_messageType)..high(T_messageType)],
    {at_console}  [mt_clearConsole..mt_el4_systemError,mt_profile_call_info,mt_timing_info],
    {at_textFile} [mt_printline..mt_el4_systemError,mt_profile_call_info,mt_timing_info],
    {$ifdef fullVersion}
    {at_gui}      [low(T_messageType)..high(T_messageType)],
    {at_plot}     [mt_plot_addText..mt_plot_postDisplay,mt_endOfEvaluation],
    {$endif}
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
    FUNCTION appendAll(CONST messages:T_storedMessages):boolean;
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

  {$ifdef fullVersion}
  F_traceCallback=PROCEDURE(VAR error:T_errorMessage) of object;
  {$endif}

  P_textFileOutAdapter = ^T_textFileOutAdapter;
  T_textFileOutAdapter = object(T_collectingOutAdapter)
    protected
      outputFileName:ansistring;
      forceRewrite:boolean;
      lastOutput:double;
      FUNCTION flush:boolean;
    public
      CONSTRUCTOR create(CONST fileName:ansistring; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
  end;

  T_flaggedAdapter=record
    adapter:P_abstractOutAdapter;
    doDispose:boolean;
  end;

  P_abstractThreadContext=^T_abstractThreadContext;
  T_abstractThreadContext=object
    PROCEDURE raiseError(CONST text:string; CONST location:T_searchTokenLocation; CONST kind:T_messageType=mt_el3_evalError); virtual; abstract;
    FUNCTION continueEvaluation:boolean; virtual; abstract;
  end;

  P_messages=^T_messages;
  T_messages=object
    private
      messagesCs:TRTLCriticalSection;
      flags:T_stateFlags;
      userDefinedExitCode:longint;
      CONSTRUCTOR create(); //private, because it is abstract
    public
      preferredEchoLineLength:longint;
      DESTRUCTOR destroy; virtual;

      //Flags
      PROPERTY getFlags:T_stateFlags read flags;
      PROCEDURE setStopFlag;
      PROCEDURE logGuiNeeded;
      PROCEDURE clearFlags;
      FUNCTION continueEvaluation:boolean; virtual;

      //exit code
      PROCEDURE setUserDefinedExitCode(CONST code:longint);
      PROCEDURE setExitCode;

      //messages
      PROCEDURE postSingal(CONST kind:T_messageType; CONST location:T_searchTokenLocation);
      PROCEDURE postTextMessage(CONST kind:T_messageType; CONST location:T_searchTokenLocation; CONST txt:T_arrayOfString);
      PROCEDURE raiseSimpleError(CONST text:string; CONST location:T_searchTokenLocation; CONST kind:T_messageType=mt_el3_evalError);
      PROCEDURE postCustomMessage(CONST message:P_storedMessage; CONST disposeAfterPosting:boolean=false);                            virtual; abstract;
      PROCEDURE postCustomMessages(CONST message:T_storedMessages);
      PROCEDURE clear(CONST clearAllAdapters:boolean=true);                                                                           virtual;
      FUNCTION isCollecting(CONST messageType:T_messageType):boolean;                                                                 virtual; abstract;
      FUNCTION collectedMessageTypes:T_messageTypeSet;                                                                                virtual; abstract;
      FUNCTION triggersBeep:boolean;                                                                                                  virtual; abstract;
  end;

  P_messagesDistributor=^T_messagesDistributor;
  T_messagesDistributor=object(T_messages)
    private
      adapters:array of T_flaggedAdapter;
      collecting,collected:T_messageTypeSet;
      FUNCTION flushAllFiles:boolean;
      PROCEDURE updateCollecting;
    public
      CONSTRUCTOR createDistributor();
      DESTRUCTOR destroy; virtual;
      //messages
      PROCEDURE postCustomMessage(CONST message:P_storedMessage; CONST disposeAfterPosting:boolean=false);                            virtual;
      PROCEDURE clear(CONST clearAllAdapters:boolean=true);                                                                           virtual;
      FUNCTION isCollecting(CONST messageType:T_messageType):boolean;                                                                 virtual;
      FUNCTION collectedMessageTypes:T_messageTypeSet;                                                                                virtual;
      FUNCTION triggersBeep:boolean;                                                                                                  virtual;
      //adapters:
      PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter; CONST destroyIt:boolean);
      FUNCTION addOutfile(CONST fileNameAndOptions:ansistring; CONST appendMode:boolean=true):P_textFileOutAdapter;
      FUNCTION addConsoleOutAdapter(CONST verbosity:string=''):P_consoleOutAdapter;
      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);
  end;

  P_messagesRedirector=^T_messagesRedirector;

  { T_messagesRedirector }

  T_messagesRedirector=object(T_messagesDistributor)
    private
      messagesToRedirect:T_messageTypeSet;
      messageReceiver:P_messages;
      collector:T_collectingOutAdapter;
    public
      CONSTRUCTOR createRedirector();
      DESTRUCTOR destroy; virtual;

      PROCEDURE setupMessageRedirection(CONST receiver:P_messages; CONST typesToRedirect:T_messageTypeSet);
      //flags
      FUNCTION continueEvaluation:boolean; virtual;
      //messages
      PROCEDURE postCustomMessage(CONST message:P_storedMessage; CONST disposeAfterPosting:boolean=false);                            virtual;
      PROCEDURE clear(CONST clearAllAdapters:boolean=true);                                                                           virtual;
      FUNCTION isCollecting(CONST messageType:T_messageType):boolean;                                                                 virtual;
      FUNCTION storedMessages(CONST filterDuplicates:boolean):T_storedMessages;
  end;

  P_messagesErrorHolder=^T_messagesErrorHolder;

  { T_messagesErrorHolder }

  T_messagesErrorHolder=object(T_messages)
    private
      parentMessages:P_messages;
      heldTypes:T_messageTypeSet;
      collector:T_collectingOutAdapter;
    public
      CONSTRUCTOR createErrorHolder(CONST parent:P_messages; CONST typesToHold:T_messageTypeSet);
      DESTRUCTOR destroy; virtual;

      //flags
      FUNCTION continueEvaluation:boolean; virtual;
      //messages
      PROCEDURE postCustomMessage(CONST message:P_storedMessage; CONST disposeAfterPosting:boolean=false);                            virtual;
      PROCEDURE clear(CONST clearAllAdapters:boolean=true);                                                                           virtual;
      FUNCTION isCollecting(CONST messageType:T_messageType):boolean;                                                                 virtual;
      FUNCTION collectedMessageTypes:T_messageTypeSet;                                                                                virtual;
      FUNCTION triggersBeep:boolean;                                                                                                  virtual;
      FUNCTION storedMessages(CONST filterDuplicates:boolean):T_storedMessages;
  end;

  P_messagesDummy=^T_messagesDummy;

  T_messagesDummy=object(T_messages)
    CONSTRUCTOR createDummy;
    DESTRUCTOR destroy; virtual;

    PROCEDURE postCustomMessage(CONST message:P_storedMessage; CONST disposeAfterPosting:boolean=false); virtual;
    FUNCTION isCollecting(CONST messageType:T_messageType):boolean;                                      virtual;
    FUNCTION collectedMessageTypes:T_messageTypeSet;                                                     virtual;
    FUNCTION triggersBeep:boolean;                                                                       virtual;
  end;
//
//
//  T_messageConnector=object
//    private
//      connectorCS:TRTLCriticalSection;
//      adapters:array of T_flaggedAdapter;
//      collecting,collected:T_messageTypeSet;
//      userDefinedExitCode:longint;
//      flags:T_stateFlags;
//      {$ifdef fullVersion}
//      traceCallback:F_traceCallback;
//      {$endif}
//      FUNCTION flushAllFiles:boolean;
//    public
//      preferredEchoLineLength:longint;
//      CONSTRUCTOR create({$ifdef fullVersion}CONST callback:F_traceCallback{$endif});
//      DESTRUCTOR destroy;
//
//      PROCEDURE postSingal(CONST kind:T_messageType; CONST location:T_searchTokenLocation);
//      PROCEDURE postTextMessage(CONST kind:T_messageType; CONST location:T_searchTokenLocation; CONST txt:T_arrayOfString);
//      PROCEDURE raiseError(CONST text:string; CONST location:T_searchTokenLocation; CONST kind:T_messageType=mt_el3_evalError);
//      PROCEDURE postCustomMessage(CONST message:P_storedMessage; CONST disposeAfterPosting:boolean=false); virtual;
//      PROCEDURE postCustomMessages(CONST message:T_storedMessages);
//      PROCEDURE setStopFlag;
//
//      PROCEDURE clear(CONST clearAllAdapters:boolean=true);
//
//      PROCEDURE updateCollecting;
//      FUNCTION isCollecting(CONST messageType:T_messageType):boolean;
//      FUNCTION hasCollected(CONST messageType:T_messageType):boolean;
//      FUNCTION getAdapter(CONST index:longint):P_abstractOutAdapter;
//
//      PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter; CONST destroyIt:boolean);
//      FUNCTION addOutfile(CONST fileNameAndOptions:ansistring; CONST appendMode:boolean=true):P_textFileOutAdapter;
//      FUNCTION addConsoleOutAdapter(CONST verbosity:string=''):P_consoleOutAdapter;
//      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);
//
//      PROCEDURE setUserDefinedExitCode(CONST code:longint);
//      PROCEDURE setExitCode;
//      FUNCTION triggersBeep:boolean;
//      FUNCTION continueEvaluation:boolean; {$ifndef debugMode} inline; {$endif}
//  end;
//
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
{$ifndef fullVersion}CONST{$endif}
  gui_started:boolean=false;
OPERATOR :=(s:string):T_messageTypeSet;
IMPLEMENTATION
VAR globalAdaptersCs:TRTLCriticalSection;
    allConnectors:array of P_messagesDistributor;
    finalizing:longint=0;
    flushThreadsRunning:longint=0;

FUNCTION fileFlushThread({$WARN 5024 OFF}p:pointer):ptrint;
  VAR messageConnector:P_messagesDistributor;
      k   :longint=0;
  begin
    while finalizing=0 do begin
      if k>=10 then begin
        enterCriticalSection(globalAdaptersCs);
        for messageConnector in allConnectors do messageConnector^.flushAllFiles;
        leaveCriticalSection(globalAdaptersCs);
        k:=0;
      end else inc(k);
      sleep(100);
    end;
    result:=interlockedDecrement(flushThreadsRunning);
  end;

PROCEDURE ensureFileFlushThread;
  begin
    enterCriticalSection(globalAdaptersCs);
    if flushThreadsRunning=0 then begin;
      interLockedIncrement(flushThreadsRunning);
      beginThread(@fileFlushThread);
    end;
    leaveCriticalSection(globalAdaptersCs);
  end;

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
      't': result:=result+[mt_timing_info,mt_profile_call_info];
      'T': result:=result-[mt_timing_info,mt_profile_call_info];
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

CONSTRUCTOR T_messagesRedirector.createRedirector();
  begin
    inherited create;
    messagesToRedirect:=[];
    messageReceiver:=nil;
    collector.create(at_unknown,C_textMessages);
    addOutAdapter(@collector,false);
    inherited clear;
  end;

DESTRUCTOR T_messagesRedirector.destroy;
  begin
    inherited destroy;
    collector.destroy;
  end;

PROCEDURE T_messagesRedirector.setupMessageRedirection(
  CONST receiver: P_messages; CONST typesToRedirect: T_messageTypeSet);
  begin
    messageReceiver:=receiver;
    messagesToRedirect:=typesToRedirect;
    if (receiver=nil) or (messagesToRedirect=[]) then begin
      messageReceiver:=nil;
      messagesToRedirect:=[];
    end;
  end;

FUNCTION T_messagesRedirector.continueEvaluation: boolean;
  begin
    result:=(flags=[]) and ((messageReceiver=nil) or messageReceiver^.continueEvaluation);
  end;

PROCEDURE T_messagesRedirector.clear(CONST clearAllAdapters: boolean);
  begin
    enterCriticalSection(messagesCs);
    inherited clear;
    leaveCriticalSection(messagesCs);
  end;

FUNCTION T_messagesRedirector.isCollecting(CONST messageType: T_messageType
  ): boolean;
  begin
    result:=true;
  end;

FUNCTION T_messagesRedirector.storedMessages(CONST filterDuplicates:boolean): T_storedMessages;
  begin
    enterCriticalSection(messagesCs);
    if filterDuplicates then collector.removeDuplicateStoredMessages;
    result:=collector.storedMessages;
    leaveCriticalSection(messagesCs);
  end;

FUNCTION T_messagesErrorHolder.storedMessages(CONST filterDuplicates: boolean): T_storedMessages;
  begin
    enterCriticalSection(messagesCs);
    if filterDuplicates then collector.removeDuplicateStoredMessages;
    result:=collector.storedMessages;
    leaveCriticalSection(messagesCs);
  end;

FUNCTION T_messagesDistributor.flushAllFiles: boolean;
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(messagesCs);
    result:=false;
    for a in adapters do try
      if a.adapter^.adapterType=at_textFile
      then begin
        if P_textFileOutAdapter(a.adapter)^.flush
        then result:=true;
      end;
    except
    end;
    leaveCriticalSection(messagesCs);
  end;

PROCEDURE T_messagesDistributor.updateCollecting;
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(messagesCs);
    collecting:=[];;
    for a in adapters do try
      collecting:=collecting + a.adapter^.messageTypesToInclude;
    except
    end;
    leaveCriticalSection(messagesCs);
  end;

CONSTRUCTOR T_messagesDistributor.createDistributor();
  begin
    inherited create;
  end;

DESTRUCTOR T_messagesDistributor.destroy;
  VAR k:longint;
  begin
    enterCriticalSection(messagesCs);
    for k:=0 to length(adapters)-1 do if adapters[k].doDispose then dispose(adapters[k].adapter,destroy);
    setLength(adapters,0);
    leaveCriticalSection(messagesCs);
    inherited destroy;
  end;

PROCEDURE T_messagesDistributor.postCustomMessage(CONST message: P_storedMessage; CONST disposeAfterPosting: boolean);
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(messagesCs);
    flags:=flags+C_messageClassMeta[message^.messageClass].triggeredFlags;
    for a in adapters do if a.adapter^.append(message) then include(collected,message^.messageType);
    if disposeAfterPosting then disposeMessage(message);
    leaveCriticalSection(messagesCs);
  end;

PROCEDURE T_messagesErrorHolder.postCustomMessage(
  CONST message: P_storedMessage; CONST disposeAfterPosting: boolean);
  begin
    enterCriticalSection(messagesCs);
    flags:=flags+C_messageClassMeta[message^.messageClass].triggeredFlags;
    if message^.messageType in heldTypes
    then begin
      collector.append(message);
      if disposeAfterPosting then disposeMessage(message);
    end else parentMessages^.postCustomMessage(message,disposeAfterPosting);
    leaveCriticalSection(messagesCs);
  end;

PROCEDURE T_messagesRedirector.postCustomMessage(CONST message: P_storedMessage; CONST disposeAfterPosting: boolean);
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(messagesCs);
    flags:=flags+C_messageClassMeta[message^.messageClass].triggeredFlags;
    for a in adapters do if a.adapter^.append(message) then include(collected,message^.messageType);
    if message^.messageType in messagesToRedirect
    then messageReceiver^.postCustomMessage(message,disposeAfterPosting)
    else if disposeAfterPosting then disposeMessage(message);
    leaveCriticalSection(messagesCs);
  end;

PROCEDURE T_messagesDummy.postCustomMessage(CONST message: P_storedMessage; CONST disposeAfterPosting: boolean);
  begin if disposeAfterPosting then disposeMessage(message); end;

PROCEDURE T_messagesDistributor.clear(CONST clearAllAdapters: boolean);
  VAR a:T_flaggedAdapter;
  begin
    inherited clear(clearAllAdapters);
    if clearAllAdapters then for a in adapters do a.adapter^.clear;
    updateCollecting;
  end;

FUNCTION T_messagesDistributor.isCollecting(CONST messageType: T_messageType): boolean;
  begin
    result:=messageType in collecting;
  end;

FUNCTION T_messagesDistributor.collectedMessageTypes: T_messageTypeSet;
  begin
    result:=collected;
  end;

FUNCTION T_messagesDistributor.triggersBeep: boolean;
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

PROCEDURE T_messagesDistributor.addOutAdapter(CONST p: P_abstractOutAdapter;
  CONST destroyIt: boolean);
  begin
    enterCriticalSection(messagesCs);
    setLength(adapters,length(adapters)+1);
    adapters[length(adapters)-1].adapter:=p;
    adapters[length(adapters)-1].doDispose:=destroyIt;
    collecting:=collecting+p^.messageTypesToInclude;
    if p^.adapterType=at_textFile then ensureFileFlushThread;
    leaveCriticalSection(messagesCs);
  end;

FUNCTION T_messagesDistributor.addOutfile(CONST fileNameAndOptions: ansistring;
  CONST appendMode: boolean): P_textFileOutAdapter;
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

FUNCTION T_messagesDistributor.addConsoleOutAdapter(CONST verbosity: string
  ): P_consoleOutAdapter;
  VAR consoleOutAdapter:P_consoleOutAdapter;
  begin
    new(consoleOutAdapter,create(verbosity));
    addOutAdapter(consoleOutAdapter,true);
    result:=consoleOutAdapter;
  end;

PROCEDURE T_messagesDistributor.removeOutAdapter(CONST p: P_abstractOutAdapter);
  VAR i:longint=0;
  begin
    enterCriticalSection(messagesCs);
    while i<length(adapters) do if adapters[i].adapter=p then begin
      if adapters[i].doDispose then dispose(adapters[i].adapter,destroy);
      adapters[i]:=adapters[length(adapters)-1];
      setLength(   adapters,length(adapters)-1);
    end else inc(i);
    leaveCriticalSection(messagesCs);
  end;

CONSTRUCTOR T_messagesErrorHolder.createErrorHolder(CONST parent: P_messages;
  CONST typesToHold: T_messageTypeSet);
  begin
    inherited create();
    heldTypes:=typesToHold;
    parentMessages:=parent;
    collector.create(at_unknown,C_textMessages);
  end;

DESTRUCTOR T_messagesErrorHolder.destroy;
  begin
    clear;
    inherited destroy;
  end;

FUNCTION T_messagesErrorHolder.continueEvaluation: boolean;
  begin
    result:=(flags=[]) and (parentMessages^.continueEvaluation);
  end;

PROCEDURE T_messagesErrorHolder.clear(CONST clearAllAdapters: boolean);
  begin
    inherited clear;
    collector.clear;
  end;

FUNCTION T_messagesErrorHolder.isCollecting(CONST messageType: T_messageType
  ): boolean;
  begin
    result:=(messageType in heldTypes) or parentMessages^.isCollecting(messageType);
  end;

FUNCTION T_messagesErrorHolder.collectedMessageTypes: T_messageTypeSet;
  begin
    raise Exception.create('Unimplemented');

  end;

FUNCTION T_messagesErrorHolder.triggersBeep: boolean;
begin
  raise Exception.create('Unimplemented');

end;

CONSTRUCTOR T_messages.create();
  begin
    initCriticalSection(messagesCs);
    flags:=[];
    userDefinedExitCode:=0;
    preferredEchoLineLength:=120;
  end;

DESTRUCTOR T_messages.destroy;
  begin
    doneCriticalSection(messagesCs);
  end;

PROCEDURE T_messages.setStopFlag;
  begin
    enterCriticalSection(messagesCs);
    include(flags,FlagQuietHalt);
    leaveCriticalSection(messagesCs);
  end;

PROCEDURE T_messages.logGuiNeeded;
  begin
    enterCriticalSection(messagesCs);
    include(flags,FlagGUINeeded);
    leaveCriticalSection(messagesCs);
  end;

PROCEDURE T_messages.clearFlags;
  begin
    enterCriticalSection(messagesCs);
    flags:=[];
    leaveCriticalSection(messagesCs);
  end;

FUNCTION T_messages.continueEvaluation: boolean;
  begin
    result:=flags=[];
  end;

PROCEDURE T_messages.setUserDefinedExitCode(CONST code: longint);
  begin
    userDefinedExitCode:=code;
  end;

PROCEDURE T_messages.setExitCode;
  VAR mt:T_messageType;
      code:longint=0;
  begin
    code:=userDefinedExitCode;
    for mt in collectedMessageTypes do if (C_messageTypeMeta[mt].systemErrorLevel>code) then code:=C_messageTypeMeta[mt].systemErrorLevel;
    ExitCode:=code;
  end;

PROCEDURE T_messages.postSingal(CONST kind: T_messageType; CONST location: T_searchTokenLocation);
  VAR message:P_storedMessage;
  begin
    new(message,create(kind,location));
    postCustomMessage(message);
    disposeMessage(message);
  end;

PROCEDURE T_messages.postTextMessage(CONST kind: T_messageType; CONST location: T_searchTokenLocation; CONST txt: T_arrayOfString);
  VAR message:P_storedMessageWithText;
  begin
    new(message,create(kind,location,txt));
    postCustomMessage(message);
    disposeMessage(message);
  end;

PROCEDURE T_messages.raiseSimpleError(CONST text: string; CONST location: T_searchTokenLocation; CONST kind: T_messageType);
  VAR message:P_errorMessage;
  begin
    if (kind<>mt_el4_systemError) and (FlagQuietHalt in flags) then exit;
    new(message,create(kind,location,split(text,C_lineBreakChar)));
    flags:=flags+C_messageClassMeta[message^.messageClass].triggeredFlags;
    postCustomMessage(message);
    disposeMessage(message);
  end;

PROCEDURE T_messages.postCustomMessages(CONST message: T_storedMessages);
  VAR m:P_storedMessage;
  begin
    enterCriticalSection(messagesCs);
    for m in message do postCustomMessage(m);
    leaveCriticalSection(messagesCs);
  end;

PROCEDURE T_messages.clear(CONST clearAllAdapters: boolean);
  begin
    enterCriticalSection(messagesCs);
    flags:=[];
    userDefinedExitCode:=0;
    leaveCriticalSection(messagesCs);
  end;

CONSTRUCTOR T_messagesDummy.createDummy;
  begin inherited create; end;
DESTRUCTOR T_messagesDummy.destroy;
  begin inherited destroy; end;
FUNCTION T_messagesDummy.isCollecting(CONST messageType: T_messageType): boolean;
  begin result:=false; end;
FUNCTION T_messagesDummy.collectedMessageTypes: T_messageTypeSet;
  begin result:=[]; end;
FUNCTION T_messagesDummy.triggersBeep: boolean;
  begin result:=false; end;

//CONSTRUCTOR T_messageConnector.create({$ifdef fullVersion}CONST callback:F_traceCallback{$endif});
//  PROCEDURE registerConnector(CONST c:P_messagesDistributor);
//    VAR i:longint;
//    begin
//      enterCriticalSection(globalAdaptersCs);
//      for i:=0 to length(allConnectors)-1 do if allConnectors[i]=c then begin
//        leaveCriticalSection(globalAdaptersCs);
//        exit;
//      end;
//      i:=length(allConnectors);
//      setLength(allConnectors,i+1);
//      allConnectors[i]:=c;
//      leaveCriticalSection(globalAdaptersCs);
//    end;
//
//  begin
//    initCriticalSection(connectorCS);
//    setLength(adapters,0);
//    registerConnector(@self);
//    {$ifdef fullVersion}
//    traceCallback:=callback;
//    {$endif}
//  end;
//
//DESTRUCTOR T_messageConnector.destroy;
//  PROCEDURE unregisterConnector(CONST c:P_messagesDistributor);
//    VAR i:longint;
//    begin
//      enterCriticalSection(globalAdaptersCs);
//      for i:=0 to length(allConnectors)-1 do if allConnectors[i]=c then begin
//        allConnectors[i]:=allConnectors[length(allConnectors)-1];
//        setLength(allConnectors,length(allConnectors)-1);
//        leaveCriticalSection(globalAdaptersCs);
//        exit;
//      end;
//      leaveCriticalSection(globalAdaptersCs);
//    end;
//
//  VAR a:T_flaggedAdapter;
//  begin
//    unregisterConnector(@self);
//    enterCriticalSection(connectorCS);
//    for a in adapters do if a.doDispose then dispose(a.adapter,destroy);
//    setLength(adapters,0);
//    leaveCriticalSection(connectorCS);
//    doneCriticalSection(connectorCS);
//  end;

//CONSTRUCTOR T_threadLocalMessages.create({$ifdef fullVersion} CONST callback: F_traceCallback{$endif});
//  begin
//    inherited create(at_unknown,[mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined,mt_el4_systemError]);
//    globalMessages       :=nil;
//    parentMessages       :=nil;
//    setLength(childMessages,0);
//    {$ifdef fullVersion}
//    traceCallback:=callback;
//    {$endif}
//  end;
//
//PROCEDURE T_threadLocalMessages.propagateFlags(CONST up:boolean=true; CONST down:boolean=true);
//  VAR child:P_threadLocalMessages;
//  begin
//    if up and ((FlagFatalError in flags) or (FlagGUINeeded in flags)) and (parentMessages<>nil) then begin
//      parentMessages^.flags:=parentMessages^.flags+(flags*[FlagFatalError{$ifdef fullVersion} ,FlagGUINeeded{$endif}]);
//      parentMessages^.propagateFlags(true,false);
//    end;
//    {$ifdef fullVersion}
//    if (parentMessages=nil) and (globalMessages<>nil) then globalMessages^.flags:=globalMessages^.flags+flags;
//    {$endif}
//    if down then for child in childMessages do begin
//      try
//        child^.flags:=child^.flags+flags;
//        child^.propagateFlags(false,true);
//      except end;
//    end;
//  end;
//
//PROCEDURE T_threadLocalMessages.raiseError(CONST text: string; CONST location: T_searchTokenLocation; CONST kind: T_messageType);
//  VAR message:P_errorMessage;
//  begin
//    if (kind<>mt_el4_systemError) and (FlagQuietHalt in flags) then exit;
//    new(message,create(kind,location,split(text,C_lineBreakChar)));
//    {$ifdef fullVersion}
//    if traceCallback<>nil then traceCallback(message^);
//    {$endif}
//    flags:=flags+C_messageClassMeta[message^.messageClass].triggeredFlags;
//    if kind=mt_el4_systemError then escalateErrors;
//    append(message);
//    disposeMessage(message);
//  end;
//
//{$ifdef debugMode}
//FUNCTION T_threadLocalMessages.append(CONST message:P_storedMessage):boolean;
//  begin
//    if message^.messageType in [mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined,mt_el4_systemError]
//    then begin
//      result:=true;
//      inherited append(message);
//    end else raise Exception.create('Invalid call to T_threadLocalMessages.append');
//  end;
//{$endif}

//PROCEDURE T_threadLocalMessages.clear;
//  begin
//    inherited clear;
//    flags:=[];
//  end;
//
//PROCEDURE T_threadLocalMessages.escalateErrors;
//  VAR m:P_storedMessage;
//  begin
//    if parentMessages<>nil then begin
//      for m in storedMessages do parentMessages^.append(m);
//      parentMessages^.flags:=parentMessages^.flags+flags;
//      exit;
//    end;
//    if globalMessages<>nil then for m in storedMessages do globalMessages^.postCustomMessage(m);
//  end;
//
//FUNCTION T_threadLocalMessages.continueEvaluation: boolean;
//  begin
//    result:=flags=[];
//  end;

//FUNCTION T_messageConnector.continueEvaluation:boolean;
//  begin
//    result:=flags=[];
//  end;

//PROCEDURE T_threadLocalMessages.clearFlagsForCodeAssistance;
//  begin
//    flags:=[];
//  end;
//
//DESTRUCTOR T_threadLocalMessages.destroy;
//  begin
//    clear;
//    if parentMessages<>nil then parentMessages^.dropChild(@self);
//    inherited destroy;
//  end;

//PROCEDURE T_threadLocalMessages.setParent(CONST parent:P_threadLocalMessages);
//  begin
//    if parent=parentMessages then exit;
//    enterCriticalSection(cs);
//    if parentMessages<>nil then parentMessages^.dropChild(@self);
//    parentMessages:=parent;
//    if parent<>nil then begin
//      parent^.addChild(@self);
//      globalMessages:=parent^.globalMessages;
//    end;
//    leaveCriticalSection(cs);
//  end;
//
//PROCEDURE T_threadLocalMessages.dropChild(CONST child: P_threadLocalMessages);
//  VAR i,j:longint;
//  begin
//    enterCriticalSection(cs);
//    for i:=length(childMessages)-1 downto 0 do if pointer(childMessages[i])=pointer(child) then begin
//      for j:=i to length(childMessages)-2 do childMessages[j]:=childMessages[j+1];
//      setLength(childMessages,length(childMessages)-1);
//      leaveCriticalSection(cs);
//      exit;
//    end;
//    leaveCriticalSection(cs);
//  end;

//PROCEDURE T_threadLocalMessages.addChild(CONST child: P_threadLocalMessages);
//  VAR {$ifdef debugMode}i:longint=0;{$endif}
//      k:longint;
//  begin
//    enterCriticalSection(cs);
//    {$ifdef debugMode}
//    k:=length(childMessages);
//    while (i<k) and (childMessages[i]<>child) do inc(i);
//    if i=k then begin
//      setLength(childMessages,i+1);
//      childMessages[i]:=child;
//    end else raise Exception.create('Added child a second time in T_threadLocalMessages.addChild');
//    {$else}
//    k:=length(childMessages);
//    setLength(childMessages,k+1);
//    childMessages[k]:=child;
//    {$endif}
//    leaveCriticalSection(cs);
//  end;
//
//FUNCTION T_threadLocalMessages.childCount:longint;
//  begin
//    enterCriticalSection(cs);
//    result:=length(childMessages);
//    leaveCriticalSection(cs);
//  end;
//
//PROCEDURE T_threadLocalMessages.setStopFlag;
//  begin
//    enterCriticalSection(cs);
//    include(flags,FlagQuietHalt);
//    leaveCriticalSection(cs);
//    propagateFlags;
//  end;
//
//{$ifdef fullVersion}
//PROCEDURE T_threadLocalMessages.logGuiNeeded;
//  begin
//    enterCriticalSection(cs);
//    if not(gui_started) then begin
//      include(flags,FlagGUINeeded);
//      include(flags,FlagQuietHalt);
//      propagateFlags;
//    end;
//    leaveCriticalSection(cs);
//  end;
//{$endif}

//PROCEDURE T_messageConnector.postSingal(CONST kind:T_messageType; CONST location:T_searchTokenLocation);
//  VAR message:P_storedMessage;
//  begin
//    new(message,create(kind,location));
//    postCustomMessage(message);
//    disposeMessage(message);
//  end;
//
//PROCEDURE T_messageConnector.postTextMessage(CONST kind: T_messageType; CONST location: T_searchTokenLocation; CONST txt: T_arrayOfString);
//  VAR message:P_storedMessageWithText;
//  begin
//    new(message,create(kind,location,txt));
//    postCustomMessage(message);
//    disposeMessage(message);
//  end;
//
//PROCEDURE T_messageConnector.raiseError(CONST text:string; CONST location:T_searchTokenLocation; CONST kind:T_messageType=mt_el3_evalError);
//  VAR message:P_errorMessage;
//  begin
//    if (kind<>mt_el4_systemError) and (FlagQuietHalt in flags) then exit;
//    new(message,create(kind,location,split(text,C_lineBreakChar)));
//    {$ifdef fullVersion}
//    if traceCallback<>nil then traceCallback(message^);
//    {$endif}
//    flags:=flags+C_messageClassMeta[message^.messageClass].triggeredFlags;
//    postCustomMessage(message,true);
//  end;

//PROCEDURE T_messageConnector.postCustomMessage(CONST message: P_storedMessage; CONST disposeAfterPosting:boolean);
//  VAR a:T_flaggedAdapter;
//  begin
//    enterCriticalSection(connectorCS);
//    for a in adapters do if a.adapter^.append(message) then include(collected,message^.messageType);
//    if disposeAfterPosting then disposeMessage(message);
//    leaveCriticalSection(connectorCS);
//  end;
//
//PROCEDURE T_messageConnector.postCustomMessages(CONST message:T_storedMessages);
//  VAR a:T_flaggedAdapter;
//      m:P_storedMessage;
//      anyCollected:boolean;
//  begin
//    enterCriticalSection(connectorCS);
//    for m in message do begin
//      anyCollected:=false;
//      for a in adapters do if a.adapter^.append(m) then anyCollected:=true;
//      if anyCollected then include(collected,m^.messageType);
//    end;
//    leaveCriticalSection(connectorCS);
//  end;
//
//PROCEDURE T_messageConnector.setStopFlag;
//  begin
//    enterCriticalSection(connectorCS);
//    include(flags,FlagQuietHalt);
//    leaveCriticalSection(connectorCS);
//  end;
//
//PROCEDURE T_messageConnector.clear(CONST clearAllAdapters:boolean=true);
//  VAR a:T_flaggedAdapter;
//  begin
//    enterCriticalSection(connectorCS);
//    if clearAllAdapters then for a in adapters do a.adapter^.clear;
//    collected:=[];
//    updateCollecting;
//    flags:=[];
//    userDefinedExitCode:=0;
//    leaveCriticalSection(connectorCS);
//  end;
//
//PROCEDURE T_messageConnector.updateCollecting;
//  VAR a:T_flaggedAdapter;
//  begin
//    enterCriticalSection(connectorCS);
//    collecting:=[];
//    for a in adapters do collecting:=collecting + a.adapter^.messageTypesToInclude;
//    leaveCriticalSection(connectorCS);
//  end;
//
//FUNCTION T_messageConnector.isCollecting(CONST messageType: T_messageType): boolean;
//  begin
//    result:=messageType in collecting;
//  end;
//
//FUNCTION T_messageConnector.hasCollected(CONST messageType:T_messageType):boolean;
//  begin
//    result:=messageType in collected;
//  end;
//
//FUNCTION T_messageConnector.getAdapter(CONST index:longint):P_abstractOutAdapter;
//  begin
//    result:=adapters[index].adapter;
//  end;

//CONSTRUCTOR T_connectorAdapter.create(CONST connected: P_messages; CONST includePrint,includeWarnings,includeErrors: boolean);
//  VAR toInclude:T_messageTypeSet=[];
//  begin
//    if includePrint    then include(toInclude,mt_printline);
//    if includeWarnings then toInclude:=toInclude+[mt_el1_note..mt_el2_userWarning];
//    if includeErrors   then toInclude:=toInclude+[mt_el3_evalError..mt_el4_systemError];
//    inherited create(at_unknown,toInclude);
//    connectedAdapters:=connected;
//  end;
//
//DESTRUCTOR T_connectorAdapter.destroy;
//  begin
//    inherited destroy;
//  end;
//
//FUNCTION T_connectorAdapter.append(CONST message: P_storedMessage): boolean;
//  begin
//    if not(message^.messageType in messageTypesToInclude) then exit(false);
//    connectedAdapters^.postCustomMessage(message^.rereferenced);
//    result:=true;
//  end;

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

FUNCTION T_abstractOutAdapter.appendAll(CONST messages: T_storedMessages): boolean;
  VAR m:P_storedMessage;
  begin
    result:=false;
    for m in messages do if append(m) then result:=true;
  end;

PROCEDURE T_abstractOutAdapter.clear; begin end;

PROCEDURE T_abstractOutAdapter.setOutputBehavior(CONST messageTypesToInclude_: T_messageTypeSet);
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
//T_textFileOutAdapter:=========================================================
FUNCTION T_textFileOutAdapter.flush:boolean;
  VAR handle:text;
      s:string;
      m:P_storedMessage;
  begin
    enterCriticalSection(cs);
    if length(storedMessages)>0 then begin
      result:=true;
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
    end else result:=false;
    leaveCriticalSection(cs);
  end;

CONSTRUCTOR T_textFileOutAdapter.create(CONST fileName: ansistring; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean);
  begin
    inherited create(at_textFile,messageTypesToInclude_);
    outputFileName:=expandFileName(fileName);
    forceRewrite:=forceNewFile;
    lastOutput:=now;
  end;

FUNCTION T_textFileOutAdapter.append(CONST message:P_storedMessage):boolean;
  begin
    result:=inherited append(message);
    enterCriticalSection(cs);
    if length(storedMessages)>100 then flush;
    leaveCriticalSection(cs);
  end;

DESTRUCTOR T_textFileOutAdapter.destroy;
  begin
    flush;
    inherited destroy;
  end;
//=========================================================:T_textFileOutAdapter

//FUNCTION T_messageConnector.addOutfile(CONST fileNameAndOptions:ansistring; CONST appendMode:boolean=true):P_textFileOutAdapter;
//  VAR fileName:string;
//      options:string='';
//  begin
//    if pos('(',fileNameAndOptions)>0 then begin
//      options :=copy(fileNameAndOptions,  pos('(',fileNameAndOptions),length(fileNameAndOptions));
//      fileName:=copy(fileNameAndOptions,1,pos('(',fileNameAndOptions)-1);
//    end else fileName:=fileNameAndOptions;
//
//    new(result,create(fileName,options,not(appendMode)));
//    addOutAdapter(result,true);
//  end;
//
//FUNCTION T_messageConnector.flushAllFiles:boolean;
//  VAR a:T_flaggedAdapter;
//  begin
//    enterCriticalSection(connectorCS);
//    result:=false;
//    for a in adapters do try
//      if a.adapter^.adapterType=at_textFile
//      then begin
//        if P_textFileOutAdapter(a.adapter)^.flush
//        then result:=true;
//      end;
//    except
//    end;
//    leaveCriticalSection(connectorCS);
//  end;
//
//PROCEDURE T_messageConnector.addOutAdapter(CONST p: P_abstractOutAdapter; CONST destroyIt: boolean);
//  begin
//    enterCriticalSection(connectorCS);
//    setLength(adapters,length(adapters)+1);
//    adapters[length(adapters)-1].adapter:=p;
//    adapters[length(adapters)-1].doDispose:=destroyIt;
//    collecting:=collecting+p^.messageTypesToInclude;
//    if p^.adapterType=at_textFile then ensureFileFlushThread;
//    leaveCriticalSection(connectorCS);
//  end;
//
//FUNCTION T_messageConnector.connectToOther(CONST other:P_messages; CONST includePrint,includeWarnings,includeErrors:boolean):P_connectorAdapter;
//  begin
//    new(result,create(@self,includePrint,includeWarnings,includeErrors));
//    other^.addOutAdapter(result,true);
//  end;
//
//FUNCTION T_messageConnector.addConsoleOutAdapter(CONST verbosity:string=''):P_consoleOutAdapter;
//  VAR consoleOutAdapter:P_consoleOutAdapter;
//  begin
//    new(consoleOutAdapter,create(verbosity));
//    addOutAdapter(consoleOutAdapter,true);
//    result:=consoleOutAdapter;
//  end;
//
//PROCEDURE T_messageConnector.removeOutAdapter(CONST p:P_abstractOutAdapter);
//  VAR i:longint=0;
//  begin
//    enterCriticalSection(connectorCS);
//    while i<length(adapters) do if adapters[i].adapter=p then begin
//      if adapters[i].doDispose then dispose(adapters[i].adapter,destroy);
//      adapters[i]:=adapters[length(adapters)-1];
//      setLength(   adapters,length(adapters)-1);
//    end else inc(i);
//    leaveCriticalSection(connectorCS);
//  end;
//
//PROCEDURE T_messageConnector.setUserDefinedExitCode(CONST code:longint);
//  begin
//    userDefinedExitCode:=code;
//  end;
//
//PROCEDURE T_messageConnector.setExitCode;
//  VAR mt:T_messageType;
//      code:longint=0;
//  begin
//    code:=userDefinedExitCode;
//    for mt in collected do if (C_messageTypeMeta[mt].systemErrorLevel>code) then code:=C_messageTypeMeta[mt].systemErrorLevel;
//    ExitCode:=code;
//  end;
//
//FUNCTION T_messageConnector.triggersBeep:boolean;
//  VAR mt:T_messageType;
//  begin
//    for mt in collected do if (C_messageTypeMeta[mt].systemErrorLevel>0) then begin
//      {$ifdef debugMode}
//      writeln(stdErr,'        DEBUG: Beep triggered by message type ',mt);
//      {$endif}
//      exit(true);
//    end;
//    result:=false;
//  end;
//===================================================================:T_adapters

INITIALIZATION
  defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;
  initCriticalSection(globalAdaptersCs);
  setLength(allConnectors,0);
FINALIZATION
  interLockedIncrement(finalizing);
  while flushThreadsRunning>0 do sleep(1);
  doneCriticalSection(globalAdaptersCs);
end.
