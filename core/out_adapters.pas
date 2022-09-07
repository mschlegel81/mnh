UNIT out_adapters;
INTERFACE
USES sysutils,
     myGenerics,mySys,
     mnh_messages,
     mnh_constants, basicTypes,serializationUtil;
TYPE
  T_adapterType=(at_console,
                 at_textFile,
                 at_textMessageCollector,
                 {$ifdef fullVersion}
                 at_guiSynOutput,
                 at_guiEventsCollector,
                 at_plot,
                 at_table,
                 at_treeView,
                 at_customForm,
                 at_profilingView,
                 {$endif}
                 at_sandboxAdapter,
                 at_printTextFileAtRuntime);
  T_consoleOutMode=(com_normal,
                    com_stdout_only,
                    com_stderr_only);
CONST
  C_includableMessages:array[T_adapterType] of T_messageTypeSet=(
    {at_console}  [mt_clearConsole..mt_el4_systemError,mt_timing_info{$ifdef fullVersion},mt_profile_call_info{$endif}],
    {at_textFile} [mt_clearConsole..mt_el4_systemError,mt_timing_info{$ifdef fullVersion},mt_profile_call_info{$endif}],
    {at_textMe...}[mt_clearConsole..mt_el4_systemError,mt_timing_info{$ifdef fullVersion},mt_profile_call_info{$endif}],
    {$ifdef fullVersion}
    {at_guiSyn...}[mt_startOfEvaluation,mt_clearConsole..mt_el4_systemError,mt_timing_info,mt_endOfEvaluation],
    {at_guiEve...}[mt_startOfEvaluation,mt_endOfEvaluation,mt_debugger_breakpoint,mt_guiEdit_done,mt_guiEditScriptsLoaded],
    {at_plot}     [mt_startOfEvaluation,mt_plot_addText..mt_plot_postDisplay,mt_endOfEvaluation],
    {at_table}    [mt_startOfEvaluation,mt_displayTable],
    {at_treeView} [mt_startOfEvaluation,mt_displayVariableTree],
    {at_custom...}[                     mt_displayCustomForm,mt_endOfEvaluation],
    {at_profil...}[mt_startOfEvaluation,mt_profile_call_info],
    {$endif}
    {at_sandbo...}[low(T_messageType)..high(T_messageType)],
    {at_printT...}[mt_printline]);

  {$ifdef fullVersion}
  C_messagesLeadingToErrorIfNotHandled:T_messageTypeSet=[mt_debugger_breakpoint..mt_displayCustomForm];
  {$endif}

TYPE
  P_messages=^T_messages;
  P_abstractOutAdapter = ^T_abstractOutAdapter;
  T_abstractOutAdapter = object
    protected
      adapterCs:TRTLCriticalSection;
      messageTypesToInclude:T_messageTypeSet;
      parentMessage:P_messages;
    private
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
    FUNCTION isDoneFlushing:boolean; virtual; abstract;
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;
  T_consoleOutAdapter = object(T_abstractOutAdapter)
    private
      messageFormatProvider:P_messageFormatProvider;
    public
    mode:T_consoleOutMode;
    CONSTRUCTOR create(CONST messageTypesToInclude_:T_messageTypeSet; CONST consoleOutMode:T_consoleOutMode; CONST formatProvider:P_messageFormatProvider);
    DESTRUCTOR destroy; virtual;
    FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
    FUNCTION isDoneFlushing:boolean; virtual;
  end;

  P_collectingOutAdapter = ^T_collectingOutAdapter;
  T_collectingOutAdapter = object(T_abstractOutAdapter)
    protected
      collected:T_storedMessages;
      collectedFill:longint;
    public
      CONSTRUCTOR create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
      PROCEDURE removeDuplicateStoredMessages(CONST messageTypesToDrop:T_messageTypeSet=[mt_clearConsole..mt_timing_info]);
      PROCEDURE clear; virtual;
      FUNCTION typesOfStoredMessages:T_messageTypeSet;
      FUNCTION getStoredMessages:T_storedMessages;
      PROPERTY fill:longint read collectedFill;
      FUNCTION isDoneFlushing:boolean; virtual;
  end;

  {$ifdef fullVersion}
  P_abstractGuiOutAdapter = ^T_abstractGuiOutAdapter;
  T_abstractGuiOutAdapter = object(T_collectingOutAdapter)
    FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual; abstract;
  end;

  GENERIC G_multiChildGuiOutAdapter<childType>= object(T_abstractGuiOutAdapter)
    protected
      children:array of childType;
    public
      CONSTRUCTOR create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
      PROCEDURE childDestroyed(CONST child:childType);
      FUNCTION addChild(CONST child:childType):childType;
      PROCEDURE destroyAllChildren;
      DESTRUCTOR destroy; virtual;
  end;

  F_traceCallback=PROCEDURE(VAR error:T_errorMessage) of object;
  {$endif}

  T_textFileCase=(tfc_file,tfc_stdout,tfc_stderr);
  P_textFileAdapterSpecification=^T_textFileAdapterSpecification;
  T_textFileAdapterSpecification=object
    private
      verbosityPart:string;
      messagesToInclude:T_messageTypeSet;
    public
      textFileCase:T_textFileCase;
      fileName        :string;
      forceNewFile    :boolean;
      useLogFormatter :boolean;
      handlePrintAsLog:boolean;
      logDateFormat   :string;
      logLocationLen  :longint;

      PROCEDURE setDefaults;
      PROCEDURE setFilenameAndOptions(CONST s:string; CONST globalMessageTypes:T_messageTypeSet);
      FUNCTION  getFilenameAndOptions:string;
      FUNCTION canMergeInto(VAR other:T_textFileAdapterSpecification; CONST globalMessageTypes:T_messageTypeSet):boolean;
      PROCEDURE copy(CONST original:T_textFileAdapterSpecification);
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
      PROCEDURE finalizeBeforeApplication(CONST scriptName:string);
      PROPERTY getFilename:string read fileName;
      PROPERTY getVerbosityPart:string read verbosityPart;
      PROCEDURE setVerbosityPart(CONST s:string; CONST globalMessageTypes: T_messageTypeSet);
      PROPERTY getMessageTypes:T_messageTypeSet read messagesToInclude;

  end;

  P_textFileOutAdapter = ^T_textFileOutAdapter;
  T_textFileOutAdapter = object(T_collectingOutAdapter)
    protected
      messageFormatProvider:P_messageFormatProvider;
      outputFileName:ansistring;
      forceRewrite:boolean;
      lastOutput:double;
      FUNCTION flush:boolean;
    public
      CONSTRUCTOR create(CONST fileName:ansistring; CONST tfc:T_textFileCase; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean; CONST formatProvider:P_messageFormatProvider);
      DESTRUCTOR destroy; virtual;
      FUNCTION isDoneFlushing: boolean; virtual;
  end;

  T_flaggedAdapter=record
    adapter:P_abstractOutAdapter;
    doDispose:boolean;
  end;

  P_abstractContext=^T_abstractContext;
  T_abstractContext=object
    messages:P_messages;
    PROCEDURE raiseError(CONST text:string; CONST location:T_searchTokenLocation; CONST kind:T_messageType=mt_el3_evalError); virtual; abstract;
    FUNCTION continueEvaluation:boolean; virtual; abstract;
  end;

  T_messages=object
    private
      messagesCs:TRTLCriticalSection;
      flags:T_stateFlags;
      errorCount:longint;
      userDefinedExitCode:longint;
      CONSTRUCTOR create(); //private, because it is abstract
    public
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
      FUNCTION  getExitCode:longint;

      //messages
      PROCEDURE postSingal(CONST kind:T_messageType; CONST location:T_searchTokenLocation);
      PROCEDURE postTextMessage(CONST kind:T_messageType; CONST location:T_searchTokenLocation; CONST txt:T_arrayOfString);
      PROCEDURE raiseSimpleError(CONST text:string; CONST location:T_searchTokenLocation; CONST kind:T_messageType=mt_el3_evalError); virtual;
      PROCEDURE raiseUnhandledError(CONST unhandledMessage:P_storedMessage);
      PROCEDURE postCustomMessage(CONST message:P_storedMessage; CONST disposeAfterPosting:boolean=false);                            virtual; abstract;
      PROCEDURE postCustomMessages(CONST message:T_storedMessages);
      PROCEDURE clear(CONST clearAllAdapters:boolean=true);                                                                           virtual;
      FUNCTION isCollecting(CONST messageType:T_messageType):boolean;                                                                 virtual; abstract;
      FUNCTION collectedMessageTypes:T_messageTypeSet;                                                                                virtual; abstract;
      FUNCTION triggersBeep:boolean;                                                                                                  virtual; abstract;
      PROCEDURE awaitAllFlushed(CONST timeOutInSeconds:double); virtual;
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
      FUNCTION addOutfile(CONST specification:T_textFileAdapterSpecification; CONST messageFormatProvider:P_messageFormatProvider):P_abstractOutAdapter;
      FUNCTION addConsoleOutAdapter(CONST messageTypesToInclude:T_messageTypeSet; CONST consoleMode:T_consoleOutMode; CONST formatProvider:P_messageFormatProvider):P_consoleOutAdapter;
      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);
      FUNCTION getAdapter(CONST index:longint):P_abstractOutAdapter;
      PROCEDURE awaitAllFlushed(CONST timeOutInSeconds:double); virtual;
  end;

  {$ifdef fullVersion}
  P_guiMessagesDistributor = ^T_guiMessagesDistributor;
  T_guiMessagesDistributor = object(T_messagesDistributor)
    CONSTRUCTOR createGuiMessagesDistributor();
    FUNCTION flushToGui:T_messageTypeSet;
  end;
  {$endif}

  P_messagesRedirector=^T_messagesRedirector;
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
      PROCEDURE raiseSimpleError(CONST text:string; CONST location:T_searchTokenLocation; CONST kind:T_messageType=mt_el3_evalError); virtual;
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

CONST
  C_defaultOutputBehavior:T_messageTypeSet=[mt_clearConsole,mt_printline,mt_printdirect,mt_log,mt_el3_evalError..mt_endOfEvaluation];
  C_collectAllOutputBehavior:T_messageTypeSet=[low(T_messageType)..high(T_messageType)];
  messageSubset_user    :T_messageTypeSet=[mt_log,mt_el1_userNote,mt_el2_userWarning,mt_el3_userDefined];
  messageSubset_print   :T_messageTypeSet=[mt_printline,mt_printdirect,mt_clearConsole,mt_log];
  messageSubset_echo    :T_messageTypeSet=[mt_echo_input,mt_echo_declaration,mt_echo_output];
  messageSubset_timing  :T_messageTypeSet=[mt_timing_info{$ifdef fullVersion},mt_profile_call_info{$endif}];
  messageSubset_notes   :T_messageTypeSet=[mt_el1_note,mt_el1_userNote];
  messageSubset_warnings:T_messageTypeSet=[mt_el2_warning,mt_el2_userWarning];
PROCEDURE splitIntoLogNameAndOption(CONST nameAndOption:string; OUT fileName,options:string);
{$ifdef fullVersion}VAR{$else}CONST{$endif}
  gui_started:(NO,ide,REEVALUATION)=NO;
FUNCTION stringToMessageTypeSet(CONST s:string;           CONST toOverride:T_messageTypeSet=[mt_clearConsole,mt_printline,mt_printdirect,mt_log,mt_el3_evalError..mt_endOfEvaluation]):T_messageTypeSet;
FUNCTION messageTypeSetToString(CONST s:T_messageTypeSet; CONST toOverride:T_messageTypeSet=[mt_clearConsole,mt_printline,mt_printdirect,mt_log,mt_el3_evalError..mt_endOfEvaluation]):string;
IMPLEMENTATION
USES myStringUtil,strutils,fileWrappers,Classes;
VAR globalAdaptersCs:TRTLCriticalSection;
    allConnectors:array of P_messagesDistributor;
TYPE
  T_fileFlushThread=class(T_basicThread)
    protected
      PROCEDURE execute; override;
    public
      CONSTRUCTOR create();
  end;

VAR fileFlushThread:T_fileFlushThread=nil;
PROCEDURE T_fileFlushThread.execute;
  VAR messageConnector:P_messagesDistributor;
      k   :longint=0;
  begin
    threadStartsSleeping;
    while not(Terminated) do begin
      if k>=10 then begin
        enterCriticalSection(globalAdaptersCs);
        for messageConnector in allConnectors do messageConnector^.flushAllFiles;
        leaveCriticalSection(globalAdaptersCs);
        k:=0;
      end else inc(k);
      sleep(10);
    end;
    threadStopsSleeping;
    Terminate;
  end;

CONSTRUCTOR T_fileFlushThread.create();
  begin
    inherited create(tpLower);
    FreeOnTerminate:=false;
  end;

PROCEDURE ensureFileFlushThread;
  begin
    enterCriticalSection(globalAdaptersCs);
    if fileFlushThread=nil then fileFlushThread:=T_fileFlushThread.create();
    leaveCriticalSection(globalAdaptersCs);
  end;

FUNCTION stringToMessageTypeSet(CONST s:string; CONST toOverride:T_messageTypeSet=[mt_clearConsole,mt_printline,mt_printdirect,mt_log,mt_el3_evalError..mt_endOfEvaluation]):T_messageTypeSet;
  VAR i,level:longint;
      mt:T_messageType;
  begin
    result:=toOverride;
    for i:=1 to length(s) do case s[i] of
      'v': result:=C_collectAllOutputBehavior;
      'V': result:=[];
      'u': result:=result+messageSubset_user;
      'U': result:=result-messageSubset_user;
      'p': result:=result+messageSubset_print;
      'P': result:=result-messageSubset_print;
      'e': result:=result+messageSubset_echo;
      'E': result:=result-messageSubset_echo;
      't': result:=result+messageSubset_timing;
      'T': result:=result-messageSubset_timing;
      'n': result:=result+messageSubset_notes;
      'N': result:=result-messageSubset_notes;
      'w': result:=result+messageSubset_warnings;
      'W': result:=result-messageSubset_warnings;
      'i': result:=result+[mt_echo_input];
      'I': result:=result-[mt_echo_input];
      'd': result:=result+[mt_echo_declaration];
      'D': result:=result-[mt_echo_declaration];
      'o': result:=result+[mt_echo_output];
      'O': result:=result-[mt_echo_output];
      '1'..'4': begin
        level:=strToInt(s[i]);
        for mt:=low(T_messageType) to high(T_messageType) do if C_messageTypeMeta[mt].level>0 then begin
          if C_messageTypeMeta[mt].level>=level then result+=[mt]
                                                else result-=[mt];
        end;
      end;
    end;
  end;

FUNCTION messageTypeSetToString(CONST s:T_messageTypeSet; CONST toOverride:T_messageTypeSet=[mt_clearConsole,mt_printline,mt_printdirect,mt_log,mt_el3_evalError..mt_endOfEvaluation]):string;
  VAR toSwitchOn :T_messageTypeSet=[];
      toSwitchOff:T_messageTypeSet=[];
      mt:T_messageType;
      minLevel:longint=5;
  begin
    if s=C_collectAllOutputBehavior then exit('v');
    if s=[] then exit('V');
    if s=C_defaultOutputBehavior then exit('');
    for mt in T_messageType do begin
      if (mt in s) and not(mt in C_defaultOutputBehavior) then include(toSwitchOn ,mt);
      if not(mt in s) and (mt in C_defaultOutputBehavior) then include(toSwitchOff,mt);
    end;
    result:='';
    if toSwitchOn *messageSubset_user    =messageSubset_user     then begin toSwitchOn -=messageSubset_user;          result+='u'; end;
    if toSwitchOff*messageSubset_user    =messageSubset_user     then begin toSwitchOff-=messageSubset_user;          result+='U'; end;
    if toSwitchOn *messageSubset_print   =messageSubset_print    then begin toSwitchOn -=messageSubset_print;         result+='p'; end;
    if toSwitchOff*messageSubset_print   =messageSubset_print    then begin toSwitchOff-=messageSubset_print;         result+='P'; end;
    if toSwitchOn *messageSubset_echo    =messageSubset_echo     then begin toSwitchOn -=messageSubset_echo;          result+='e'; end;
    if toSwitchOff*messageSubset_echo    =messageSubset_echo     then begin toSwitchOff-=messageSubset_echo;          result+='E'; end;
    if toSwitchOn *messageSubset_timing  =messageSubset_timing   then begin toSwitchOn -=messageSubset_timing;        result+='t'; end;
    if toSwitchOff*messageSubset_timing  =messageSubset_timing   then begin toSwitchOff-=messageSubset_timing;        result+='T'; end;
    if toSwitchOn *messageSubset_notes   =messageSubset_notes    then begin toSwitchOn -=messageSubset_notes;         result+='n'; end;
    if toSwitchOff*messageSubset_notes   =messageSubset_notes    then begin toSwitchOff-=messageSubset_notes;         result+='N'; end;
    if toSwitchOn *messageSubset_warnings=messageSubset_warnings then begin toSwitchOn -=messageSubset_warnings;      result+='w'; end;
    if toSwitchOff*messageSubset_warnings=messageSubset_warnings then begin toSwitchOff-=messageSubset_warnings;      result+='W'; end;
    if mt_echo_input       in toSwitchOn                         then begin Exclude(toSwitchOn ,mt_echo_input);       result+='i'; end;
    if mt_echo_input       in toSwitchOff                        then begin Exclude(toSwitchOff,mt_echo_input);       result+='I'; end;
    if mt_echo_declaration in toSwitchOn                         then begin Exclude(toSwitchOn ,mt_echo_declaration); result+='d'; end;
    if mt_echo_declaration in toSwitchOff                        then begin Exclude(toSwitchOff,mt_echo_declaration); result+='D'; end;
    if mt_echo_output      in toSwitchOn                         then begin Exclude(toSwitchOn ,mt_echo_output);      result+='o'; end;
    if mt_echo_output      in toSwitchOff                        then begin Exclude(toSwitchOff,mt_echo_output);      result+='O'; end;
    for mt in toSwitchOn do if (C_messageTypeMeta[mt].level>0) and (C_messageTypeMeta[mt].level<minLevel) then minLevel:=C_messageTypeMeta[mt].level;
    if minLevel<5 then result+=intToStr(minLevel);
  end;

PROCEDURE T_textFileAdapterSpecification.setVerbosityPart(CONST s: string; CONST globalMessageTypes: T_messageTypeSet);
  begin
    verbosityPart:=s;
    messagesToInclude:=stringToMessageTypeSet(verbosityPart,globalMessageTypes);
  end;

PROCEDURE T_textFileAdapterSpecification.setDefaults;
  begin
    fileName:='?.log';
    textFileCase:=tfc_file;
    forceNewFile:=true;
    useLogFormatter:=true;
    handlePrintAsLog:=false;
    verbosityPart:='1';
    messagesToInclude:=stringToMessageTypeSet('1');
    logDateFormat:='hh:mm:ss.zzz';
    logLocationLen:=25;
  end;

PROCEDURE T_textFileAdapterSpecification.setFilenameAndOptions(CONST s: string;
  CONST globalMessageTypes: T_messageTypeSet);
  begin
    splitIntoLogNameAndOption(s,fileName,verbosityPart);
    if uppercase(fileName)='STDOUT'
    then textFileCase:=tfc_stdout
    else if uppercase(fileName)='STDOUT'
    then textFileCase:=tfc_stderr
    else textFileCase:=tfc_file;
    messagesToInclude:=stringToMessageTypeSet(verbosityPart,globalMessageTypes);
  end;

FUNCTION T_textFileAdapterSpecification.getFilenameAndOptions: string;
  begin
    case textFileCase of
      tfc_stdout: result:='stdOut('+verbosityPart+')';
      tfc_stderr: result:='stdErr('+verbosityPart+')';
      tfc_file  : result:=fileName+'('+verbosityPart+')';
      else begin
        result:='';
        assert(false);
      end;
    end;
  end;

FUNCTION T_textFileAdapterSpecification.canMergeInto(VAR other: T_textFileAdapterSpecification; CONST globalMessageTypes: T_messageTypeSet): boolean;
  begin
    if SameFileName(fileName,other.fileName) or (textFileCase in [tfc_stderr,tfc_stdout]) and (other.textFileCase=textFileCase) then begin
      //In conflict, revert to appending mode
      other.forceNewFile:=other.forceNewFile and forceNewFile;
      //In conflict: use log formatter
      other.useLogFormatter:=other.useLogFormatter or useLogFormatter;
      //Unite message types
      other.messagesToInclude+=messagesToInclude;
      //Update verbosity
      other.verbosityPart:=messageTypeSetToString(other.messagesToInclude,globalMessageTypes);
      //In doubt: convert print to log
      other.handlePrintAsLog:=other.handlePrintAsLog or handlePrintAsLog;
      //Use the longer time format for logging
      if length(logDateFormat)>length(other.logDateFormat) then other.logDateFormat:=logDateFormat;
      if logLocationLen>other.logLocationLen then other.logLocationLen:=logLocationLen;
      result:=true;
    end else result:=false;
  end;

PROCEDURE T_textFileAdapterSpecification.copy(CONST original: T_textFileAdapterSpecification);
  begin
    verbosityPart    :=original.verbosityPart    ;
    messagesToInclude:=original.messagesToInclude;

    textFileCase     :=original.textFileCase     ;
    fileName         :=original.fileName         ;

    forceNewFile     :=original.forceNewFile     ;
    useLogFormatter  :=original.useLogFormatter  ;
    handlePrintAsLog :=original.handlePrintAsLog ;
    logDateFormat    :=original.logDateFormat    ;
    logLocationLen   :=original.logLocationLen   ;
  end;

FUNCTION T_textFileAdapterSpecification.loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
  begin
    verbosityPart:=stream.readAnsiString;
    messagesToInclude:=stringToMessageTypeSet(verbosityPart);
    textFileCase:=T_textFileCase(stream.readByte([byte(tfc_file),byte(tfc_stderr),byte(tfc_stdout)]));
    fileName:=stream.readAnsiString;
    forceNewFile    :=stream.readBoolean   ;
    useLogFormatter :=stream.readBoolean   ;
    handlePrintAsLog:=stream.readBoolean   ;
    logDateFormat   :=stream.readAnsiString;
    logLocationLen  :=stream.readLongint   ;
    result:=stream.allOkay;
  end;

PROCEDURE T_textFileAdapterSpecification.saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  begin
    verbosityPart:=messageTypeSetToString(messagesToInclude);
    stream.writeAnsiString(verbosityPart);
    stream.writeByte(byte(textFileCase));
    stream.writeAnsiString(fileName);
    stream.writeBoolean(forceNewFile);
    stream.writeBoolean(useLogFormatter);
    stream.writeBoolean(handlePrintAsLog);
    stream.writeAnsiString(logDateFormat);
    stream.writeLongint(logLocationLen);
  end;

PROCEDURE T_textFileAdapterSpecification.finalizeBeforeApplication(CONST scriptName: string);
  begin
    fileName:=ansiReplaceStr(fileName,'?',scriptName);
  end;

{$ifdef fullVersion}
CONSTRUCTOR G_multiChildGuiOutAdapter.create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
  begin
    inherited create(typ,messageTypesToInclude_);
    setLength(children,0);
  end;

PROCEDURE G_multiChildGuiOutAdapter.childDestroyed(CONST child: childType);
  VAR i:longint=0;
  begin
    while i<length(children) do begin
      if children[i]=child then begin
        children[i]:=children[length(children)-1];
        setLength(   children,length(children)-1);
      end else inc(i);
    end;
  end;

FUNCTION G_multiChildGuiOutAdapter.addChild(CONST child: childType): childType;
  begin
    setLength(children,length(children)+1);
    children[length(children)-1]:=child;
    result:=child;
  end;

PROCEDURE G_multiChildGuiOutAdapter.destroyAllChildren;
  VAR toDestroy:childType;
  begin
    while length(children)>0 do begin
      toDestroy:=children[0];
      toDestroy.free;
    end;
  end;

DESTRUCTOR G_multiChildGuiOutAdapter.destroy;
  begin
    destroyAllChildren;
    inherited destroy;
  end;

CONSTRUCTOR T_guiMessagesDistributor.createGuiMessagesDistributor();
  begin
    inherited createDistributor();
  end;

FUNCTION T_guiMessagesDistributor.flushToGui: T_messageTypeSet;
  VAR a:T_flaggedAdapter;
  begin
    result:=[];
    for a in adapters do if a.adapter^.adapterType in
    [at_guiSynOutput,
     at_guiEventsCollector,
     at_customForm,
     at_plot,
     at_table,
     at_treeView,
     at_profilingView] then result+=P_abstractGuiOutAdapter(a.adapter)^.flushToGui(false);
  end;
{$endif}

CONSTRUCTOR T_messagesRedirector.createRedirector();
  begin
    inherited create;
    messagesToRedirect:=[];
    messageReceiver:=nil;
    collector.create(at_textMessageCollector,C_textMessages);
    addOutAdapter(@collector,false);
    inherited clear;
  end;

DESTRUCTOR T_messagesRedirector.destroy;
  begin
    inherited destroy;
    collector.destroy;
  end;

PROCEDURE T_messagesRedirector.setupMessageRedirection(CONST receiver: P_messages; CONST typesToRedirect: T_messageTypeSet);
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

FUNCTION T_messagesErrorHolder.continueEvaluation: boolean;
  begin
    result:=(flags=[]) and ((parentMessages=nil) or (parentMessages^.continueEvaluation));
  end;

FUNCTION T_messages.continueEvaluation: boolean;
  begin
    result:=flags=[];
  end;

PROCEDURE T_messagesRedirector.clear(CONST clearAllAdapters: boolean);
  begin
    enterCriticalSection(messagesCs);
    try
      inherited clear(clearAllAdapters);
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

PROCEDURE T_messagesDistributor.clear(CONST clearAllAdapters: boolean);
  VAR a:T_flaggedAdapter;
  begin
    inherited clear(clearAllAdapters);
    collected:=[];
    if clearAllAdapters then for a in adapters do a.adapter^.clear;
    updateCollecting;
  end;

PROCEDURE T_messagesErrorHolder.clear(CONST clearAllAdapters: boolean);
  begin
    inherited clear(clearAllAdapters);
    collector.clear;
  end;

PROCEDURE T_messages.clear(CONST clearAllAdapters: boolean);
  begin
    enterCriticalSection(messagesCs);
    try
      clearFlags;
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

PROCEDURE T_messages.awaitAllFlushed(CONST timeOutInSeconds:double);
  begin
    enterCriticalSection(messagesCs);
    leaveCriticalSection(messagesCs);
  end;

FUNCTION T_messagesRedirector.isCollecting(CONST messageType: T_messageType): boolean;
  begin result:=true; end;

FUNCTION T_messagesDistributor.isCollecting(CONST messageType: T_messageType): boolean;
  begin result:=messageType in collecting; end;

FUNCTION T_messagesErrorHolder.isCollecting(CONST messageType: T_messageType): boolean;
  begin result:=(messageType in heldTypes) or (parentMessages<>nil) and parentMessages^.isCollecting(messageType); end;

FUNCTION T_messagesDummy.isCollecting(CONST messageType: T_messageType): boolean;
  begin result:=false; end;

FUNCTION T_messagesRedirector.storedMessages(CONST filterDuplicates:boolean): T_storedMessages;
  begin
    enterCriticalSection(messagesCs);
    try
      if filterDuplicates then collector.removeDuplicateStoredMessages;
      result:=collector.getStoredMessages;
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

FUNCTION T_messagesErrorHolder.storedMessages(CONST filterDuplicates: boolean): T_storedMessages;
  begin
    enterCriticalSection(messagesCs);
    try
      if filterDuplicates then collector.removeDuplicateStoredMessages;
      result:=collector.getStoredMessages;
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

FUNCTION T_messagesDistributor.flushAllFiles: boolean;
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(messagesCs);
    try
      result:=false;
      for a in adapters do
        if a.adapter^.adapterType=at_textFile
        then begin
          if P_textFileOutAdapter(a.adapter)^.flush
          then result:=true;
        end;
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

PROCEDURE T_messagesDistributor.updateCollecting;
  VAR a:T_flaggedAdapter;
  begin
    enterCriticalSection(messagesCs);
    collecting:=[];;
    try
      for a in adapters do
        collecting:=collecting + a.adapter^.messageTypesToInclude;
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

CONSTRUCTOR T_messagesDistributor.createDistributor();
  begin
    inherited create;
    enterCriticalSection(globalAdaptersCs);
    setLength(allConnectors,length(allConnectors)+1);
    allConnectors[length(allConnectors)-1]:=@self;
    leaveCriticalSection(globalAdaptersCs);
  end;

DESTRUCTOR T_messagesDistributor.destroy;
  VAR k:longint=0;
  begin
    enterCriticalSection(globalAdaptersCs);
    while (k<length(allConnectors)) and (allConnectors[k]<>@self) do inc(k);
    if k<length(allConnectors) then begin
      allConnectors[k]:=allConnectors[length(allConnectors)-1];
      setLength(allConnectors,length(allConnectors)-1);
    end;
    leaveCriticalSection(globalAdaptersCs);
    enterCriticalSection(messagesCs);
    for k:=0 to length(adapters)-1 do if adapters[k].doDispose then dispose(adapters[k].adapter,destroy);
    setLength(adapters,0);
    leaveCriticalSection(messagesCs);
    inherited destroy;
  end;

PROCEDURE T_messagesDistributor.postCustomMessage(CONST message: P_storedMessage; CONST disposeAfterPosting: boolean);
  VAR a:T_flaggedAdapter;
      appended:boolean=false;
      doAppend:boolean=true;
  begin
    if not((FlagQuietHalt in flags) and (message^.messageType in C_messagesSuppressedOnQuietHalt)) then begin
      enterCriticalSection(messagesCs);
      try
        flags+=C_messageClassMeta[message^.messageClass].triggeredFlags;
        if (message^.messageClass in [mc_error,mc_fatal]) then begin
          inc(errorCount);
          if errorCount>20 then doAppend:=false;
        end;
        if doAppend then for a in adapters do if a.adapter^.append(message) then appended:=true;
        if appended then include(collected,message^.messageType);
        {$ifdef fullVersion}
        if not(appended) and (message^.messageType in C_messagesLeadingToErrorIfNotHandled)
        then raiseUnhandledError(message);
        {$endif}
      finally
        leaveCriticalSection(messagesCs);
      end;
    end;
    if disposeAfterPosting then disposeMessage_(message);
  end;

PROCEDURE T_messagesErrorHolder.postCustomMessage(CONST message: P_storedMessage; CONST disposeAfterPosting: boolean);
  {$ifdef fullVersion} VAR appended:boolean=false; {$endif}
  begin
    if not((FlagQuietHalt in flags) and (message^.messageType in C_messagesSuppressedOnQuietHalt)) then begin
      enterCriticalSection(messagesCs);
      try
        flags+=C_messageClassMeta[message^.messageClass].triggeredFlags;
        if message^.messageType in heldTypes then begin
          collector.append(message);
          {$ifdef fullVersion} appended:=true; {$endif}
        end else if parentMessages<>nil then begin
          parentMessages^.postCustomMessage(message);
          {$ifdef fullVersion} appended:=true; {$endif}
        end;
        {$ifdef fullVersion}
        if not(appended) and (message^.messageType in C_messagesLeadingToErrorIfNotHandled) then raiseUnhandledError(message);
        {$endif}
      finally
        leaveCriticalSection(messagesCs);
      end;
    end;
    if disposeAfterPosting then disposeMessage_(message);
  end;

PROCEDURE T_messagesRedirector.postCustomMessage(CONST message: P_storedMessage; CONST disposeAfterPosting: boolean);
  VAR a:T_flaggedAdapter;
      appended:boolean=false;
  begin
    if not((FlagQuietHalt in flags) and (message^.messageType in C_messagesSuppressedOnQuietHalt)) then begin
      enterCriticalSection(messagesCs);
      try
        flags+=C_messageClassMeta[message^.messageClass].triggeredFlags;
        for a in adapters do if a.adapter^.append(message) then appended:=true;
        if appended then include(collected,message^.messageType);
        if (message^.messageType in messagesToRedirect) and (messageReceiver<>nil)
        then begin
          messageReceiver^.postCustomMessage(message,false);
          appended:=true;
        end;
        {$ifdef fullVersion}
        if not(appended) and (message^.messageType in C_messagesLeadingToErrorIfNotHandled) then raiseUnhandledError(message);
        {$endif}
      finally
        leaveCriticalSection(messagesCs);
      end;
    end;
    if disposeAfterPosting then disposeMessage_(message);
  end;

PROCEDURE T_messagesDummy.postCustomMessage(CONST message: P_storedMessage; CONST disposeAfterPosting: boolean);
  begin if disposeAfterPosting then disposeMessage_(message); end;

FUNCTION T_messagesDistributor.collectedMessageTypes: T_messageTypeSet;
  begin
    result:=collected;
  end;

FUNCTION T_messagesErrorHolder.collectedMessageTypes: T_messageTypeSet;
  VAR i:longint;
  begin
    if parentMessages=nil then result:=[] else result:=parentMessages^.collectedMessageTypes;
    for i:=0 to collector.collectedFill-1 do include(result,collector.collected[i]^.messageType);
  end;

FUNCTION T_messagesDummy.collectedMessageTypes: T_messageTypeSet;
  begin result:=[]; end;

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

PROCEDURE T_messagesDistributor.addOutAdapter(CONST p: P_abstractOutAdapter; CONST destroyIt: boolean);
  VAR i:longint=0;
  begin
    enterCriticalSection(messagesCs);
    try
      while (i<length(adapters)) and (adapters[i].adapter<>p) do inc(i);
      if i>=length(adapters) then begin
        setLength(adapters,length(adapters)+1);
        adapters[length(adapters)-1].adapter:=p;
        adapters[length(adapters)-1].doDispose:=destroyIt;
        collecting:=collecting+p^.messageTypesToInclude;
        if p^.adapterType=at_textFile then ensureFileFlushThread;
        p^.parentMessage:=@self;
      end;
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

PROCEDURE splitIntoLogNameAndOption(CONST nameAndOption:string; OUT fileName,options:string);
  begin
    if pos('(',nameAndOption)>0 then begin
      options :=copy(nameAndOption,  pos('(',nameAndOption)+1,length(nameAndOption)-pos('(',nameAndOption)-1);
      fileName:=copy(nameAndOption,1,pos('(',nameAndOption)-1);
    end else begin
      options:='';
      fileName:=nameAndOption;
    end;
  end;

FUNCTION T_messagesDistributor.addOutfile(CONST specification:T_textFileAdapterSpecification; CONST messageFormatProvider:P_messageFormatProvider): P_abstractOutAdapter;
  begin
    case specification.textFileCase of
      tfc_file  : new(P_textFileOutAdapter(result),create(specification.fileName,specification.textFileCase,specification.messagesToInclude,specification.forceNewFile,messageFormatProvider));
      tfc_stdout: new(P_consoleOutAdapter (result),create(specification.messagesToInclude,com_stdout_only,messageFormatProvider));
      tfc_stderr: new(P_consoleOutAdapter (result),create(specification.messagesToInclude,com_stderr_only,messageFormatProvider));
      else result:=nil;
    end;
    addOutAdapter(result,true);
  end;

FUNCTION T_messagesDistributor.addConsoleOutAdapter(CONST messageTypesToInclude:T_messageTypeSet; CONST consoleMode:T_consoleOutMode; CONST formatProvider:P_messageFormatProvider): P_consoleOutAdapter;
  begin
    new(result,create(messageTypesToInclude,consoleMode,formatProvider));
    addOutAdapter(result,true);
  end;

PROCEDURE T_messagesDistributor.removeOutAdapter(CONST p: P_abstractOutAdapter);
  VAR i:longint=0;
  begin
    enterCriticalSection(messagesCs);
    try
      while i<length(adapters) do if adapters[i].adapter=p then begin
        if adapters[i].doDispose then dispose(adapters[i].adapter,destroy);
        adapters[i]:=adapters[length(adapters)-1];
        setLength(   adapters,length(adapters)-1);
      end else inc(i);
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

FUNCTION T_messagesDistributor.getAdapter(CONST index:longint):P_abstractOutAdapter;
  begin
    result:=adapters[index].adapter;
  end;

PROCEDURE T_messagesDistributor.awaitAllFlushed(CONST timeOutInSeconds:double);
  VAR allFlushed:boolean;
      fa:T_flaggedAdapter;
      timeOutAt:double;
  begin
    timeOutAt:=now+timeOutInSeconds*ONE_SECOND;
    repeat
      allFlushed:=true;
      for fa in adapters do allFlushed:=allFlushed and fa.adapter^.isDoneFlushing;
    until allFlushed or (now>=timeOutAt);
  end;

CONSTRUCTOR T_messagesErrorHolder.createErrorHolder(CONST parent: P_messages;
  CONST typesToHold: T_messageTypeSet);
  begin
    inherited create();
    heldTypes:=typesToHold;
    parentMessages:=parent;
    collector.create(at_textMessageCollector,C_textMessages);
  end;

DESTRUCTOR T_messagesErrorHolder.destroy;
  begin
    clear;
    inherited destroy;
  end;

FUNCTION T_messagesErrorHolder.triggersBeep: boolean;
  begin
    result:=false;
  end;

CONSTRUCTOR T_messages.create();
  begin
    initCriticalSection(messagesCs);
    flags:=[];
    userDefinedExitCode:=0;
    errorCount:=0;
  end;

DESTRUCTOR T_messages.destroy;
  begin
    doneCriticalSection(messagesCs);
  end;

PROCEDURE T_messages.setStopFlag;
  begin
    include(flags,FlagQuietHalt);
    if FlagQuietHalt in flags then exit;
    enterCriticalSection(messagesCs);
    try
      include(flags,FlagQuietHalt);
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

PROCEDURE T_messages.logGuiNeeded;
  begin
    enterCriticalSection(messagesCs);
    try
      include(flags,FlagGUINeeded);
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

PROCEDURE T_messages.clearFlags;
  begin
    enterCriticalSection(messagesCs);
    try
      flags:=[];
      errorCount:=0;
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

PROCEDURE T_messages.setUserDefinedExitCode(CONST code: longint);
  begin
    userDefinedExitCode:=code;
  end;

PROCEDURE T_messages.setExitCode;
  begin
    ExitCode:=getExitCode;
  end;

FUNCTION T_messages.getExitCode:longint;
  VAR mt:T_messageType;
  begin
    if userDefinedExitCode<>0 then result:=userDefinedExitCode else begin
      result:=0;
      for mt in collectedMessageTypes do if (C_messageTypeMeta[mt].systemErrorLevel>result) then result:=C_messageTypeMeta[mt].systemErrorLevel;
    end;
  end;

PROCEDURE T_messages.postSingal(CONST kind: T_messageType; CONST location: T_searchTokenLocation);
  VAR message:P_storedMessage;
  begin
    new(message,create(kind,location));
    postCustomMessage(message,true);
  end;

PROCEDURE T_messages.postTextMessage(CONST kind: T_messageType; CONST location: T_searchTokenLocation; CONST txt: T_arrayOfString);
  VAR message:P_storedMessageWithText;
  begin
    new(message,create(kind,location,txt));
    postCustomMessage(message,true);
  end;

PROCEDURE T_messages.raiseSimpleError(CONST text: string; CONST location: T_searchTokenLocation; CONST kind: T_messageType);
  VAR message:P_errorMessage;
  begin
    new(message,create(kind,location,split(text,C_lineBreakChar)));
    postCustomMessage(message,true);
  end;

PROCEDURE T_messagesErrorHolder.raiseSimpleError(CONST text: string; CONST location: T_searchTokenLocation; CONST kind: T_messageType);
  VAR message:P_errorMessage;
  begin
    new(message,create(kind,location,split(text,C_lineBreakChar)));
    postCustomMessage(message,true);
  end;

PROCEDURE T_messages.raiseUnhandledError(CONST unhandledMessage:P_storedMessage);
  VAR message:P_errorMessage;
  begin
    new(message,create(mt_el3_evalError,unhandledMessage^.getLocation,'Unhandled message of type "'+unhandledMessage^.getMessageTypeName+'"'));
    postCustomMessage(message,true);
  end;

PROCEDURE T_messages.postCustomMessages(CONST message: T_storedMessages);
  VAR m:P_storedMessage;
  begin
    enterCriticalSection(messagesCs);
    try
      for m in message do postCustomMessage(m);
    finally
      leaveCriticalSection(messagesCs);
    end;
  end;

CONSTRUCTOR T_messagesDummy.createDummy;
  begin inherited create; end;
DESTRUCTOR T_messagesDummy.destroy;
  begin inherited destroy; end;
FUNCTION T_messagesDummy.triggersBeep: boolean;
  begin result:=false; end;
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
    system.initCriticalSection(adapterCs);
  end;

DESTRUCTOR T_abstractOutAdapter.destroy;
  begin
    system.doneCriticalSection(adapterCs);
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
CONSTRUCTOR T_consoleOutAdapter.create(CONST messageTypesToInclude_:T_messageTypeSet; CONST consoleOutMode:T_consoleOutMode; CONST formatProvider:P_messageFormatProvider);
  begin
    inherited create(at_console,messageTypesToInclude_);

    assert(formatProvider<>nil);
    messageFormatProvider:=formatProvider^.getClonedInstance;
    mode:=consoleOutMode;
  end;

DESTRUCTOR T_consoleOutAdapter.destroy;
  begin
    inherited destroy;
    dispose(messageFormatProvider,destroy);
  end;

FUNCTION T_consoleOutAdapter.append(CONST message:P_storedMessage):boolean;
  VAR s:string;
      p:ansistring;
      formatted,toPrint:T_arrayOfString;
      k:longint=0;
  begin
    result:=message^.messageType in messageTypesToInclude;
    if result then begin
      enterCriticalSection(adapterCs);
      try
        case message^.messageType of
          mt_clearConsole: mySys.clearConsole;
          mt_printdirect: begin
            if not(mySys.isConsoleShowing) then mySys.showConsole;
            for s in P_storedMessageWithText(message)^.txt do
              begin
                p:=s;
                {$ifdef Windows} SetCodePage(RawByteString(p),CP_NONE,false); {$endif}
                if mode in [com_normal,com_stdout_only]
                then write(       p)
                else write(stdErr,p);
              end;
          end;
          else begin
            if not(mySys.isConsoleShowing) then mySys.showConsole;
            formatted:=messageFormatProvider^.formatMessage(message);
            setLength(toPrint,length(formatted));
            for s in formatted do if s=C_formFeedChar then mySys.clearConsole else begin toPrint[k]:=s; k+=1; end;
            setLength(toPrint,k);
            p:=join(toPrint,LineEnding);
            {$ifdef Windows} SetCodePage(RawByteString(p),CP_NONE,false); {$endif}
            if    (message^.messageType in [mt_printline,mt_log]) and (mode in [com_normal,com_stdout_only]) or
               not(message^.messageType in [mt_printline,mt_log]) and (mode = com_stdout_only)
            then writeln(       p)
            else writeln(stdErr,p);
          end;
        end;
        flush(StdOut);
        flush(stdErr);
      finally
        leaveCriticalSection(adapterCs);
      end;
    end;
  end;

FUNCTION T_consoleOutAdapter.isDoneFlushing:boolean; begin result:=true; end;
//==========================================================:T_consoleOutAdapter
//T_collectingOutAdapter:=======================================================
CONSTRUCTOR T_collectingOutAdapter.create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
  begin
    inherited create(typ,messageTypesToInclude_);
    setLength(collected,0);
    collectedFill:=0;
  end;

DESTRUCTOR T_collectingOutAdapter.destroy;
  begin
    enterCriticalSection(adapterCs);
    clear;
    leaveCriticalSection(adapterCs);
    inherited destroy;
  end;

FUNCTION T_collectingOutAdapter.append(CONST message: P_storedMessage):boolean;
  begin
    {$ifdef debugMode}
    if message=nil then raise Exception.create('Cannot append NIL message');
    {$endif}

    result:=message^.messageType in messageTypesToInclude;
    if result then begin
      system.enterCriticalSection(adapterCs);
      try
        if collectedFill>=length(collected) then setLength(collected,round(1+1.5*length(collected)));
        collected[collectedFill]:=message^.rereferenced;
        inc(collectedFill);
      finally
        system.leaveCriticalSection(adapterCs);
      end;
    end;
  end;

PROCEDURE T_collectingOutAdapter.removeDuplicateStoredMessages(CONST messageTypesToDrop:T_messageTypeSet=[mt_clearConsole..mt_timing_info]);
  VAR i,j,k:longint;
      isDuplicate:boolean=false;
  begin
    enterCriticalSection(adapterCs);
    if collectedFill<=1 then begin
      leaveCriticalSection(adapterCs);
      exit;
    end;
    try
      k:=1;
      for j:=1 to collectedFill-1 do begin
        isDuplicate:=false;
        if collected[j]^.messageType in messageTypesToDrop then
        for i:=0 to k-1 do isDuplicate:=isDuplicate or collected[i]^.equals(collected[j]);
        if not(isDuplicate) then begin
          collected[k]:=collected[j];
          inc(k);
        end else begin
          disposeMessage(collected[j]);
        end;
      end;
      collectedFill:=k;
    finally
      leaveCriticalSection(adapterCs);
    end;
  end;

PROCEDURE T_collectingOutAdapter.clear;
  VAR i:longint;
  begin
    system.enterCriticalSection(adapterCs);
    try
      inherited clear;
      for i:=0 to collectedFill-1 do disposeMessage(collected[i]);
      collectedFill:=0;
      setLength(collected,length(collected) div 2);
    finally
      system.leaveCriticalSection(adapterCs);
    end;
  end;

FUNCTION T_collectingOutAdapter.typesOfStoredMessages:T_messageTypeSet;
  VAR i:longint;
  begin
    system.enterCriticalSection(adapterCs);
    try
      result:=[];
      for i:=0 to collectedFill-1 do include(result,collected[i]^.messageType);
    finally
      system.leaveCriticalSection(adapterCs);
    end;
  end;

FUNCTION T_collectingOutAdapter.getStoredMessages:T_storedMessages;
  VAR i:longint;
  begin
    system.enterCriticalSection(adapterCs);
    try
      setLength(result,collectedFill);
      for i:=0 to length(result)-1 do result[i]:=collected[i];
    finally
      system.leaveCriticalSection(adapterCs);
    end;
  end;

FUNCTION T_collectingOutAdapter.isDoneFlushing:boolean;
  begin
    result:=true;
  end;

FUNCTION T_textFileOutAdapter.isDoneFlushing:boolean;
  begin
    if gui_started=ide then exit(true);
    enterCriticalSection(adapterCs);
    result:=collectedFill=0;
    leaveCriticalSection(adapterCs);
  end;

//=======================================================:T_collectingOutAdapter
//T_textFileOutAdapter:=========================================================
FUNCTION T_textFileOutAdapter.flush:boolean;
  VAR handle:text;
      s:string;
      i:longint;
      messages:T_storedMessages;
  begin
    enterCriticalSection(adapterCs);
    setLength(messages,collectedFill);
    for i:=0 to collectedFill-1 do messages[i]:=collected[i];
    collectedFill:=0;
    clear;
    leaveCriticalSection(adapterCs);

    if length(messages)>0 then begin
      result:=true;
      try
        ForceDirectories(extractFilePath(outputFileName));
        assign(handle,outputFileName);
        if fileExists(outputFileName) and not(forceRewrite)
        then system.append(handle)
        else rewrite(handle);

        forceRewrite:=false;
        for i:=0 to length(messages)-1 do begin
          case messages[i]^.messageType of
            mt_printdirect: for s in P_storedMessageWithText(messages[i])^.txt do write  (handle,s);
            else for s in messageFormatProvider^.formatMessage(messages[i]) do writeln(handle,s);
          end;
          disposeMessage(messages[i]);
        end;
        close(handle);
      except
      end;
    end else result:=false;
  end;

CONSTRUCTOR T_textFileOutAdapter.create(CONST fileName: ansistring; CONST tfc:T_textFileCase; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean; CONST formatProvider:P_messageFormatProvider);
  begin
    inherited create(at_textFile,messageTypesToInclude_);

    assert(formatProvider<>nil);
    messageFormatProvider:=formatProvider^.getClonedInstance;

    assert(tfc=tfc_file);
    outputFileName:=expandFileName(fileName);
    forceRewrite:=forceNewFile;
    lastOutput:=now;
  end;

DESTRUCTOR T_textFileOutAdapter.destroy;
  begin
    flush;
    dispose(messageFormatProvider,destroy);
    inherited destroy;
  end;
//=========================================================:T_textFileOutAdapter

INITIALIZATION
  initialize(globalAdaptersCs);
  initCriticalSection(globalAdaptersCs);
  setLength(allConnectors,0);

FINALIZATION
  if (fileFlushThread<>nil) then begin
    while not(fileFlushThread.Finished) do fileFlushThread.Terminate;
    FreeAndNil(fileFlushThread);
  end;
  doneCriticalSection(globalAdaptersCs);
end.
