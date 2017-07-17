UNIT mnh_out_adapters;
INTERFACE
USES sysutils,
     myGenerics,mySys,
     myStringUtil,
     mnh_constants, mnh_basicTypes
     {$ifdef fullVersion},mnh_plotData{$endif}
     {$ifdef imig},mypics{$endif};
TYPE
  T_storedMessage = record
    messageType: T_messageType;
    location:    T_searchTokenLocation;
    messageText: T_arrayOfString;
    data: pointer;
  end;
  T_storedMessages = array of T_storedMessage;
  T_adapterType=(at_unknown,
                 at_console,
                 at_textFile,
                 at_gui,
                 at_sandboxAdapter,
                 at_printTextFileAtRuntime);
CONST
  C_includableMessages:array[T_adapterType] of T_messageTypeSet=(
    {at_unknown}  [low(T_messageType)..high(T_messageType)],
    {at_console}  [mt_clearConsole..mt_el4_haltMessageReceived,mt_timing_info],
    {at_textFile} [mt_printline..mt_el4_haltMessageReceived,mt_timing_info],
    {at_gui}      [low(T_messageType)..high(T_messageType)],
    {at_sandbo...}[low(T_messageType)..high(T_messageType)],
    {at_printT...}[mt_printline]);

TYPE
  P_abstractOutAdapter = ^T_abstractOutAdapter;
  T_abstractOutAdapter = object
    private
      autodestruct:boolean;
      messageTypesToInclude:T_messageTypeSet;
      PROCEDURE setOutputBehavior(CONST messageTypesToInclude_:T_messageTypeSet);
      FUNCTION getOutputBehavior:T_messageTypeSet;
    public
    adapterType:T_adapterType;

    CONSTRUCTOR create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
    DESTRUCTOR destroy; virtual; abstract;
    FUNCTION append(CONST message:T_storedMessage):boolean; virtual; abstract;
    PROCEDURE clear; virtual;
    PROPERTY outputBehavior:T_messageTypeSet read getOutputBehavior write setOutputBehavior;
    PROCEDURE enableMessageType(CONST enabled:boolean; CONST mt:T_messageTypeSet);
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;
  T_consoleOutAdapter = object(T_abstractOutAdapter)
    CONSTRUCTOR create(CONST messageTypesToInclude_:T_messageTypeSet);
    DESTRUCTOR destroy; virtual;
    FUNCTION append(CONST message:T_storedMessage):boolean; virtual;
  end;

  P_collectingOutAdapter = ^T_collectingOutAdapter;
  T_collectingOutAdapter = object(T_abstractOutAdapter)
    storedMessages:T_storedMessages;
    cs:TRTLCriticalSection;
    CONSTRUCTOR create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
    DESTRUCTOR destroy; virtual;
    FUNCTION append(CONST message:T_storedMessage):boolean; virtual;
    PROCEDURE removeDuplicateStoredMessages;
    PROCEDURE clear; virtual;
  end;

  T_abstractFileOutAdapter = object(T_collectingOutAdapter)
    protected
      outputFileName:ansistring;
      forceRewrite:boolean;
      FUNCTION switchFile(CONST newFileName:string):boolean; virtual;
      PROCEDURE flush; virtual; abstract;
    public
      CONSTRUCTOR create(CONST typ:T_adapterType; CONST fileName:ansistring; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message: T_storedMessage):boolean; virtual;
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

  P_adapters=^T_adapters;

  P_connectorAdapter=^T_connectorAdapter;
  T_connectorAdapter = object(T_abstractOutAdapter)
    private
      connectedAdapters:P_adapters;
    public
      CONSTRUCTOR create(CONST connected:P_adapters; CONST includePrint,includeWarnings,includeErrors:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message:T_storedMessage):boolean; virtual;
  end;

  T_adapters=object
    private
      stackTraceCount:longint;
      errorCount:longint;
      maxErrorLevel: shortint;
      adapter:array of P_abstractOutAdapter;

      someEchoInput        :boolean;
      someEchoDeclaration  :boolean;
      someShowExpressionOut:boolean;
      someShowTimingInfo   :boolean;
      hasMessageOfType:array[T_messageType] of boolean;
      {$ifdef fullVersion}
      privatePlot:P_plot;
      {$endif}
      subAdapters:array of P_adapters;
      PROCEDURE raiseCustomMessage(CONST message:T_storedMessage);
    public
      {$ifdef fullVersion}
      preferredEchoLineLength:longint;
      hasNeedGUIerror:boolean;
      {$endif}
      {$ifdef imig}
      picture:specialize G_safeVar<P_rawImage>;
      {$endif}
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clearErrors;
      PROCEDURE raiseError      (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseWarning    (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseNote       (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseUserError  (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseUserWarning(CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseUserNote   (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);
      PROCEDURE raiseSystemError(CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation);

      PROCEDURE echoDeclaration(CONST m:string);
      PROCEDURE echoInput      (CONST m:string);
      PROCEDURE echoOutput     (CONST m:T_arrayOfString);

      PROCEDURE raiseStoredMessages(VAR stored:T_storedMessages);
      {$ifdef fullVersion}
      PROCEDURE logGuiNeeded;
      PROCEDURE logDeferredPlot;
      FUNCTION isDeferredPlotLogged:boolean;
      PROCEDURE logInstantPlot;
      PROCEDURE resetFlagsAfterPlotDone;
      PROCEDURE logPlotSettingsChanged;
      PROCEDURE logBreakpointEncountered(CONST data:pointer);
      PROCEDURE logEndOfEditScript(CONST data:pointer; CONST success:boolean);
      PROCEDURE logPlotFileCreated(CONST fileName:string; CONST location:T_searchTokenLocation);
      PROCEDURE logDisplayTable;
      {$ifdef imig}
      PROCEDURE logDisplayImage;
      {$endif}
      {$endif}
      PROCEDURE logTimingInfo(CONST infoText:T_arrayOfString);
      PROCEDURE logCallStackInfo(CONST infoText:ansistring; CONST location:T_searchTokenLocation);
      PROCEDURE logMissingMain;
      PROCEDURE printOut(CONST s:T_arrayOfString);
      PROCEDURE printDirect(CONST s:T_arrayOfString);
      PROCEDURE clearPrint;
      PROCEDURE clearAll;
      PROCEDURE stopEvaluation;
      FUNCTION noErrors: boolean; inline;
      FUNCTION hasNonSilentError:boolean;
      FUNCTION hasHaltMessage(CONST includeQuiet:boolean=true): boolean;
      FUNCTION hasFatalError: boolean;
      FUNCTION hasStackTrace:boolean;
      FUNCTION hasPrintOut:boolean;
      PROCEDURE resetErrorFlags;
      PROCEDURE updateErrorlevel;
      PROCEDURE haltEvaluation;
      PROCEDURE logEndOfEvaluation;
      PROCEDURE raiseSystemError(CONST errorMessage: ansistring);

      FUNCTION addOutfile(CONST fileNameAndOptions:ansistring; CONST appendMode:boolean=true):P_textFileOutAdapter;
      PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter; CONST destroyIt:boolean);
      FUNCTION addConsoleOutAdapter(CONST verbosity:string=''):P_consoleOutAdapter;
      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);
      PROCEDURE removeOutAdapter(CONST index:longint);
      PROCEDURE setPrintTextFileAdapter(CONST filenameOrBlank:string);

      FUNCTION getAdapter(CONST index:longint):P_abstractOutAdapter;
      FUNCTION getAdapter(CONST adapterType:T_adapterType):P_abstractOutAdapter;

      FUNCTION collectingClone:P_adapters;
      PROCEDURE copyDataFromCollectingCloneDisposing(VAR clone:P_adapters; CONST errorCase:boolean);

      PROCEDURE setExitCode;
      FUNCTION triggersBeep:boolean;

      PROPERTY doEchoInput:         boolean read someEchoInput        ;
      PROPERTY doEchoDeclaration:   boolean read someEchoDeclaration  ;
      PROPERTY doShowExpressionOut: boolean read someShowExpressionOut;
      PROPERTY doShowTimingInfo:    boolean read someShowTimingInfo   ;

      {$ifdef fullVersion}
      FUNCTION plot:P_plot;
      {$endif}
      PROCEDURE addSubAdapters(CONST sub:P_adapters);
      PROCEDURE remSubAdapters(CONST sub:P_adapters);
      FUNCTION getConnector(CONST includePrint,includeWarnings,includeErrors:boolean):P_connectorAdapter;
  end;

CONST
  C_defaultOutputBehavior_interactive:T_messageTypeSet=[mt_clearConsole,
    mt_printline,
    mt_printdirect,
    mt_echo_input,
    mt_echo_declaration,
    mt_echo_output,
    mt_echo_continued,
    mt_el3_evalError..high(T_messageTypeSet)];

  C_defaultOutputBehavior_fileMode:T_messageTypeSet=[mt_clearConsole,mt_printline,mt_printdirect,mt_el3_evalError..mt_endOfEvaluation
    {$ifdef fullVersion},
    mt_plotFileCreated,
    mt_plotCreatedWithDeferredDisplay,
    mt_plotCreatedWithInstantDisplay,
    mt_plotSettingsChanged,
    mt_displayTable,
    mt_guiPseudoPackageFound
    {$endif}
    {$ifdef imig},
    mt_displayImage
    {$endif}];
  C_collectAllOutputBehavior:T_messageTypeSet=[low(T_messageType)..high(T_messageType)];

VAR
  defaultOutputBehavior:T_messageTypeSet;
{$ifdef fullVersion}
  gui_started:boolean=false;
{$endif}
FUNCTION defaultFormatting(CONST message:T_storedMessage; CONST includeGuiMarker:boolean): T_arrayOfString;
OPERATOR :=(s:string):T_messageTypeSet;
FUNCTION clearConsoleMessage:T_storedMessage;
IMPLEMENTATION
VAR globalLockCs:TRTLCriticalSection;

FUNCTION message(CONST messageType: T_messageType;
                 CONST messageText: T_arrayOfString;
                 CONST location   : T_searchTokenLocation;
                 CONST data       : pointer=nil):T_storedMessage;
  begin
    result.messageType:=messageType;
    result.messageText:=messageText;
    result.location   :=location;
    result.data       :=data;
  end;

FUNCTION clearConsoleMessage:T_storedMessage;
  begin
    result:=message(mt_clearConsole,C_EMPTY_STRING_ARRAY,C_nilTokenLocation);
  end;

FUNCTION defaultFormatting(CONST message: T_storedMessage; CONST includeGuiMarker:boolean): T_arrayOfString;
  VAR i:longint;
      loc:string='';
  begin
    if message.messageType in [mt_printline,mt_printdirect] then exit(message.messageText);
    with message do begin
      setLength(result,length(messageText));
      with C_messageTypeMeta[messageType] do begin
        if includeLocation then loc:=ansistring(location)+' ';
        for i:=0 to length(result)-1 do begin
          result[i]:=prefix+loc+messageText[i];
          if i=0 then loc:=StringOfChar(' ',length(loc));
          if includeGuiMarker then result[i]:=C_messageClassMeta[mClass].guiMarker+result[i];
        end;
      end;
    end;
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

CONSTRUCTOR T_connectorAdapter.create(CONST connected: P_adapters; CONST includePrint,includeWarnings,includeErrors: boolean);
  VAR toInclude:T_messageTypeSet=[];
  begin
    if includePrint then include(toInclude,mt_printline);
    if includeWarnings then toInclude:=toInclude+[mt_el1_note..mt_el2_userWarning];
    if includeErrors   then toInclude:=toInclude+[mt_el3_evalError..mt_el4_systemError];
    inherited create(at_unknown,toInclude);
    connectedAdapters:=connected;
  end;

DESTRUCTOR T_connectorAdapter.destroy;
  begin
  end;

FUNCTION T_connectorAdapter.append(CONST message: T_storedMessage): boolean;
  begin
    if not(message.messageType in messageTypesToInclude) then exit(false);
    connectedAdapters^.raiseCustomMessage(message);
    result:=true;
  end;

//T_abstractOutAdapter:=========================================================
PROCEDURE T_abstractOutAdapter.enableMessageType(CONST enabled: boolean; CONST mt: T_messageTypeSet);
  begin
    if enabled
    then messageTypesToInclude:=(messageTypesToInclude+mt) * C_includableMessages[adapterType]
    else messageTypesToInclude:=(messageTypesToInclude-mt) * C_includableMessages[adapterType];
  end;

CONSTRUCTOR T_abstractOutAdapter.create(CONST typ: T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
  begin
    adapterType:=typ;
    setOutputBehavior(messageTypesToInclude_);
  end;

PROCEDURE T_abstractOutAdapter.clear; begin end;

PROCEDURE T_abstractOutAdapter.setOutputBehavior(CONST messageTypesToInclude_:T_messageTypeSet);
  begin
     messageTypesToInclude:=C_includableMessages[adapterType]*messageTypesToInclude_;
  end;

FUNCTION T_abstractOutAdapter.getOutputBehavior:T_messageTypeSet;
  begin
    result:=messageTypesToInclude;
  end;
//=========================================================:T_abstractOutAdapter
//T_consoleOutAdapter:==========================================================
CONSTRUCTOR T_consoleOutAdapter.create(CONST messageTypesToInclude_:T_messageTypeSet);
  begin
    inherited create(at_console,messageTypesToInclude_);
  end;

DESTRUCTOR T_consoleOutAdapter.destroy;
  begin
  end;

FUNCTION T_consoleOutAdapter.append(CONST message:T_storedMessage):boolean;
  VAR i:longint;
      s:string;
  begin
    result:=message.messageType in messageTypesToInclude;
    if result then with message do begin
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
        else for s in defaultFormatting(message,false) do writeln(stdErr,s);
      end;
    end;
  end;
//==========================================================:T_consoleOutAdapter
//T_collectingOutAdapter:=======================================================
CONSTRUCTOR T_collectingOutAdapter.create(CONST typ:T_adapterType; CONST messageTypesToInclude_:T_messageTypeSet);
  begin
    inherited create(typ,messageTypesToInclude_);
    system.initCriticalSection(cs);
    setLength(storedMessages,0);
  end;

DESTRUCTOR T_collectingOutAdapter.destroy;
  begin
    clear;
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

PROCEDURE T_collectingOutAdapter.removeDuplicateStoredMessages;
  FUNCTION equals(CONST m1,m2:T_storedMessage):boolean; inline;
    begin
      result:=(m1.messageType=m2.messageType) and
              (string(m1.location)=string(m2.location)) and
              (arrEquals(m1.messageText,m2.messageText)) and
              (m2.data=nil);
    end;

  VAR i,j,k:longint;
      isDuplicate:boolean=false;
  begin
    k:=1;
    for j:=1 to length(storedMessages)-1 do begin
      isDuplicate:=false;
      for i:=0 to k-1 do isDuplicate:=isDuplicate or equals(storedMessages[i],storedMessages[k]);
      if not(isDuplicate) then begin
        storedMessages[k]:=storedMessages[j];
        inc(k);
      end;
    end;
    setLength(storedMessages,k);
  end;

PROCEDURE T_collectingOutAdapter.clear;
  begin
    system.enterCriticalSection(cs);
    inherited clear;
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
  end;

DESTRUCTOR T_abstractFileOutAdapter.destroy;
  begin
    flush();
    inherited destroy;
  end;

FUNCTION T_abstractFileOutAdapter.append(CONST message: T_storedMessage):boolean;
  begin
    result:=inherited append(message);
    if result {$ifndef DEBUGMODE} and (length(storedMessages)>=100) {$endif} then flush();
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
  VAR i,j:longint;
      handle:text;
      s:string;
  begin
    enterCriticalSection(cs);
    if length(storedMessages)>0 then begin
      try
        assign(handle,outputFileName);
        if fileExists(outputFileName) and not(forceRewrite)
        then system.append(handle)
        else rewrite(handle);
        forceRewrite:=false;
        for i:=0 to length(storedMessages)-1 do with storedMessages[i] do case messageType of
          mt_printline  : for j:=0 to length(messageText)-1 do writeln(handle,messageText[j]);
          mt_printdirect: for j:=0 to length(messageText)-1 do write  (handle,messageText[j]);
          else for s in defaultFormatting(storedMessages[i],false) do writeln(handle,s);
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
//T_adapters:===================================================================
CONSTRUCTOR T_adapters.create;
  begin
    {$ifdef fullVersion}
    privatePlot:=nil;
    preferredEchoLineLength:=-1;
    {$endif}
    {$ifdef imig}
    picture.create(nil);
    {$endif}
    setLength(adapter,0);
    setLength(subAdapters,0);
    clearAll;
  end;
{$ifdef imig}
PROCEDURE dropImage(pic:P_rawImage);
  begin
    dispose(pic,destroy);
  end;
{$endif}

DESTRUCTOR T_adapters.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(adapter)-1 do if adapter[i]^.autodestruct then dispose(adapter[i],destroy);
    setLength(adapter,0);
    {$ifdef fullVersion}
    if privatePlot<>nil then dispose(privatePlot,destroy);
    {$endif}
    {$ifdef imig}
    if (picture.value<>nil) then dropImage(picture.value);
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
    {$ifdef fullVersion}
    hasNeedGUIerror:=false;
    {$endif}
    stackTraceCount:=0;
    errorCount:=0;
  end;

PROCEDURE T_adapters.raiseCustomMessage(CONST message: T_storedMessage);
  VAR i:longint;
      sub:P_adapters;
  begin
    {$ifdef fullVersion}
    hasNeedGUIerror:=hasNeedGUIerror or not(gui_started) and C_messageTypeMeta[message.messageType].triggersGuiStartup;
    {$endif}
    if maxErrorLevel< C_messageTypeMeta[message.messageType].level then
       maxErrorLevel:=C_messageTypeMeta[message.messageType].level;
    if maxErrorLevel>=3 then begin
      enterCriticalSection(globalLockCs);
      for sub in subAdapters do sub^.haltEvaluation;
      leaveCriticalSection(globalLockCs);
    end;
    if hasHaltMessage and not(message.messageType in [mt_endOfEvaluation,mt_timing_info{$ifdef fullVersion},mt_displayTable]+C_guiOnlyMessages{$else}]{$endif}) then exit;
    hasMessageOfType[message.messageType]:=true;
    if (message.messageType=mt_el3_stackTrace) then begin
      inc(stackTraceCount);
      if stackTraceCount>30 then exit;
    end;
    if (message.messageType in [mt_el3_evalError,mt_el3_noMatchingMain,mt_el4_haltMessageReceived,mt_el4_systemError]) then begin
      inc(errorCount);
      if errorCount>30 then exit;
    end;
    for i:=0 to length(adapter)-1 do adapter[i]^.append(message);
  end;

PROCEDURE T_adapters.raiseError      (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el3_evalError   ,errorMessage        ,errorLocation)); end;
PROCEDURE T_adapters.raiseWarning    (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el2_warning     ,errorMessage        ,errorLocation)); end;
PROCEDURE T_adapters.raiseNote       (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el1_note        ,errorMessage        ,errorLocation)); end;
PROCEDURE T_adapters.raiseUserError  (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el3_userDefined ,errorMessage        ,errorLocation)); end;
PROCEDURE T_adapters.raiseUserWarning(CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el2_userWarning ,errorMessage        ,errorLocation)); end;
PROCEDURE T_adapters.raiseUserNote   (CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el1_userNote    ,errorMessage        ,errorLocation)); end;
PROCEDURE T_adapters.raiseSystemError(CONST errorMessage: T_arrayOfString; CONST errorLocation: T_searchTokenLocation); begin raiseCustomMessage(message(mt_el4_systemError ,errorMessage        ,errorLocation)); end;
PROCEDURE T_adapters.logTimingInfo    (CONST infoText:T_arrayOfString);                                                 begin raiseCustomMessage(message(mt_timing_info     ,infoText            ,C_nilTokenLocation)); end;
PROCEDURE T_adapters.logCallStackInfo (CONST infoText:ansistring; CONST location:T_searchTokenLocation);                begin raiseCustomMessage(message(mt_el3_stackTrace  ,infoText            ,location)); end;
PROCEDURE T_adapters.printOut         (CONST s: T_arrayOfString);                                                       begin raiseCustomMessage(message(mt_printline       ,s                   ,C_nilTokenLocation)); end;
PROCEDURE T_adapters.printDirect      (CONST s: T_arrayOfString);                                                       begin raiseCustomMessage(message(mt_printdirect     ,s                   ,C_nilTokenLocation)); end;
PROCEDURE T_adapters.clearPrint;                                                                                        begin raiseCustomMessage(message(mt_clearConsole    ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
PROCEDURE T_adapters.echoDeclaration(CONST m:string);                                                                   begin raiseCustomMessage(message(mt_echo_declaration,m                   ,C_nilTokenLocation)); end;
PROCEDURE T_adapters.echoInput      (CONST m:string);                                                                   begin raiseCustomMessage(message(mt_echo_input      ,m                   ,C_nilTokenLocation)); end;
PROCEDURE T_adapters.echoOutput     (CONST m:T_arrayOfString);                                                          begin raiseCustomMessage(message(mt_echo_output     ,m                   ,C_nilTokenLocation)); end;

PROCEDURE T_adapters.logMissingMain;
  begin
    hasMessageOfType[mt_el3_noMatchingMain]:=true;
  end;

PROCEDURE T_adapters.raiseStoredMessages(VAR stored:T_storedMessages);
  VAR m:T_storedMessage;
  begin for m in stored do raiseCustomMessage(m); end;

{$ifdef fullVersion}
PROCEDURE T_adapters.logGuiNeeded;                begin hasNeedGUIerror:=true; end;
FUNCTION T_adapters.isDeferredPlotLogged:boolean; begin result:=hasMessageOfType[mt_plotCreatedWithDeferredDisplay]; end;
PROCEDURE T_adapters.resetFlagsAfterPlotDone;     begin         hasMessageOfType[mt_plotCreatedWithDeferredDisplay]:=false;
                                                                hasMessageOfType[mt_plotCreatedWithInstantDisplay ]:=false; end;
PROCEDURE T_adapters.logInstantPlot;              begin         hasMessageOfType[mt_plotCreatedWithDeferredDisplay]:=false;
                                                                                                            raiseCustomMessage(message(mt_plotCreatedWithInstantDisplay ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation));
                                                    while noErrors and hasMessageOfType[mt_plotCreatedWithInstantDisplay ] do sleep(1);
                                                  end;
PROCEDURE T_adapters.logDeferredPlot;                                                                 begin raiseCustomMessage(message(mt_plotCreatedWithDeferredDisplay,C_EMPTY_STRING_ARRAY,C_nilTokenLocation));  end;
PROCEDURE T_adapters.logPlotSettingsChanged;                                                          begin raiseCustomMessage(message(mt_plotSettingsChanged           ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
PROCEDURE T_adapters.logPlotFileCreated(CONST fileName:string; CONST location:T_searchTokenLocation); begin raiseCustomMessage(message(mt_plotFileCreated               ,fileName            ,location          )); end;
PROCEDURE T_adapters.logBreakpointEncountered(CONST data: pointer);                                   begin raiseCustomMessage(message(mt_gui_breakpointEncountered,C_EMPTY_STRING_ARRAY,C_nilTokenLocation,data)); end;
PROCEDURE T_adapters.logEndOfEditScript(CONST data: pointer; CONST success: boolean);
  CONST mtOfSuccess:array[false..true] of T_messageType=(mt_gui_editScriptFailed,mt_gui_editScriptSucceeded);
  begin
    raiseCustomMessage(message(mtOfSuccess[success],C_EMPTY_STRING_ARRAY,C_nilTokenLocation,data));
  end;
PROCEDURE T_adapters.logDisplayTable;                                                                 begin raiseCustomMessage(message(mt_displayTable                  ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
{$ifdef imig}
PROCEDURE T_adapters.logDisplayImage;
  begin
    raiseCustomMessage(message(mt_displayImage,C_EMPTY_STRING_ARRAY,C_nilTokenLocation));
  end;

{$endif}
{$endif}

PROCEDURE T_adapters.clearAll;
  VAR i:longint;
  begin
    clearErrors;
    clearPrint;
    {$ifdef imig}
    if picture.value<>nil then dropImage(picture.value);
    picture.value:=nil;
    {$endif}
    someEchoInput        :=false;
    someEchoDeclaration  :=false;
    someShowExpressionOut:=false;
    someShowTimingInfo   :=false;
    for i:=0 to length(adapter)-1 do begin
      adapter[i]^.clear;
      someEchoInput        :=someEchoInput         or (mt_echo_input       in adapter[i]^.messageTypesToInclude);
      someEchoDeclaration  :=someEchoDeclaration   or (mt_echo_declaration in adapter[i]^.messageTypesToInclude);
      someShowExpressionOut:=someShowExpressionOut or (mt_echo_output      in adapter[i]^.messageTypesToInclude);
      someShowTimingInfo   :=someShowTimingInfo    or (mt_timing_info      in adapter[i]^.messageTypesToInclude);
    end;
  end;

PROCEDURE T_adapters.stopEvaluation;
  begin
    hasMessageOfType[mt_el4_haltMessageQuiet]:=true;
    maxErrorLevel:=5;
  end;

FUNCTION T_adapters.noErrors: boolean;
  begin
    result:=(maxErrorLevel<3)
    {$ifdef fullVersion}
    and not(hasNeedGUIerror)
    {$endif};
  end;

FUNCTION T_adapters.hasNonSilentError:boolean;
  VAR m:T_messageType;
  begin
    for m in [mt_el3_evalError,
              mt_el3_noMatchingMain,
              mt_el3_userDefined,
              mt_el4_systemError] do if hasMessageOfType[m] then exit(true);
    result:=false;
  end;

FUNCTION T_adapters.hasHaltMessage(CONST includeQuiet:boolean=true):boolean;
  begin
    result:=hasMessageOfType[mt_el4_haltMessageReceived] or
            hasMessageOfType[mt_el4_haltMessageQuiet] and includeQuiet;
  end;

FUNCTION T_adapters.hasFatalError: boolean;
  begin
    result:=hasMessageOfType[mt_el4_haltMessageQuiet] or
            hasMessageOfType[mt_el4_haltMessageReceived] or
            hasMessageOfType[mt_el4_systemError];
  end;

FUNCTION T_adapters.hasStackTrace:boolean;
  begin
    result:=hasMessageOfType[mt_el3_stackTrace];
  end;

FUNCTION T_adapters.hasPrintOut:boolean;
  begin
    result:=hasMessageOfType[mt_printline] or
            hasMessageOfType[mt_printdirect];
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
    if (mt<>mt_el4_haltMessageQuiet) and
       (hasMessageOfType[mt]) and
       (C_messageTypeMeta[mt].level>maxErrorLevel) then maxErrorLevel:=C_messageTypeMeta[mt].level;
  end;

PROCEDURE T_adapters.haltEvaluation;                                   begin raiseCustomMessage(message(mt_el4_haltMessageReceived,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
PROCEDURE T_adapters.logEndOfEvaluation;                               begin raiseCustomMessage(message(mt_endOfEvaluation        ,C_EMPTY_STRING_ARRAY,C_nilTokenLocation)); end;
PROCEDURE T_adapters.raiseSystemError(CONST errorMessage: ansistring); begin raiseCustomMessage(message(mt_el4_systemError        ,errorMessage        ,C_nilTokenLocation)); end;

FUNCTION T_adapters.addOutfile(CONST fileNameAndOptions:ansistring; CONST appendMode:boolean=true):P_textFileOutAdapter;
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

PROCEDURE T_adapters.addOutAdapter(CONST p: P_abstractOutAdapter; CONST destroyIt: boolean);
  begin
    setLength(adapter,length(adapter)+1);
    adapter[length(adapter)-1]:=p;
    p^.autodestruct:=destroyIt;
    someEchoInput        :=someEchoInput         or (mt_echo_input       in p^.messageTypesToInclude);
    someEchoDeclaration  :=someEchoDeclaration   or (mt_echo_declaration in p^.messageTypesToInclude);
    someShowExpressionOut:=someShowExpressionOut or (mt_echo_output      in p^.messageTypesToInclude);
    someShowTimingInfo   :=someShowTimingInfo    or (mt_timing_info      in p^.messageTypesToInclude);
  end;

FUNCTION T_adapters.addConsoleOutAdapter(CONST verbosity:string=''):P_consoleOutAdapter;
  VAR consoleOutAdapter:P_consoleOutAdapter;
  begin
    new(consoleOutAdapter,create(verbosity));
    addOutAdapter(consoleOutAdapter,true);
    result:=consoleOutAdapter;
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
        txtAdapter^.adapterType:=at_printTextFileAtRuntime;
        txtAdapter^.messageTypesToInclude:=[mt_printline,mt_printdirect];
        addOutAdapter(txtAdapter,true);
      end;
    end;
  end;

FUNCTION T_adapters.getAdapter(CONST index: longint): P_abstractOutAdapter;
  begin
    result:=adapter[index];
  end;

FUNCTION T_adapters.getAdapter(CONST adapterType:T_adapterType):P_abstractOutAdapter;
  VAR a:P_abstractOutAdapter;
  begin
    for a in adapter do if a^.adapterType=adapterType then exit(a);
    result:=nil;
  end;

FUNCTION T_adapters.collectingClone: P_adapters;
  VAR collector:P_collectingOutAdapter;
  begin
    new(result,create);
    new(collector,create(at_sandboxAdapter,''));
    collector^.messageTypesToInclude:=[low(T_messageType)..high(T_messageType)];
    result^.addOutAdapter(collector,true);
    {$ifdef fullVersion}
    if privatePlot<>nil then begin
      new(result^.privatePlot,createWithDefaults);
      result^.privatePlot^.CopyFrom(privatePlot^);
    end;
    {$endif}
  end;

PROCEDURE T_adapters.copyDataFromCollectingCloneDisposing(VAR clone: P_adapters; CONST errorCase:boolean);
  VAR collector:P_collectingOutAdapter=nil;
      i:longint;
  begin
    collector:=P_collectingOutAdapter(clone^.getAdapter(at_sandboxAdapter));
    {$ifdef fullVersion}
    if not(errorCase) and (clone^.privatePlot<>nil) and
                          (clone^.hasMessageOfType[mt_plotFileCreated] or
                           clone^.hasMessageOfType[mt_plotCreatedWithDeferredDisplay] or
                           clone^.hasMessageOfType[mt_plotCreatedWithInstantDisplay] or
                           clone^.hasMessageOfType[mt_plotSettingsChanged]) then begin
      if privatePlot<>nil then dispose(privatePlot,destroy);
      privatePlot:=clone^.privatePlot;
      clone^.privatePlot:=nil;
    end;
    {$endif}
    if (collector<>nil) then
    for i:=0 to length(collector^.storedMessages)-1 do case collector^.storedMessages[i].messageType of
      mt_el4_haltMessageReceived,
      mt_endOfEvaluation: raiseCustomMessage(collector^.storedMessages[i]);
      else begin
        if not(errorCase) then raiseCustomMessage(collector^.storedMessages[i]);
      end;
    end;
    dispose(clone,destroy);
  end;

PROCEDURE T_adapters.setExitCode;
  VAR mt:T_messageType;
      code:longint=0;
  begin
    for mt:=low(T_messageType) to high(T_messageType) do if hasMessageOfType[mt] and (C_messageTypeMeta[mt].systemErrorLevel>code) then code:=C_messageTypeMeta[mt].systemErrorLevel;
    ExitCode:=code;
  end;

FUNCTION T_adapters.triggersBeep:boolean;
  VAR mt:T_messageType;
  begin
    for mt:=low(T_messageType) to high(T_messageType) do if hasMessageOfType[mt] and (C_messageTypeMeta[mt].systemErrorLevel>0) then exit(true);
    result:=false;
  end;

{$ifdef fullVersion}
FUNCTION T_adapters.plot:P_plot;
  begin
    if privatePlot=nil then begin
      enterCriticalSection(globalLockCs);
      if privatePlot=nil then new(privatePlot,createWithDefaults);
      leaveCriticalSection(globalLockCs);
    end;
    result:=privatePlot;
  end;
{$endif}

PROCEDURE T_adapters.addSubAdapters(CONST sub:P_adapters);
  VAR s:P_adapters;
  begin
    enterCriticalSection(globalLockCs);
    for s in subAdapters do if s=sub then begin
      leaveCriticalSection(globalLockCs);
      exit;
    end;
    setLength(subAdapters,length(subAdapters)+1);
    subAdapters[length(subAdapters)-1]:=sub;
    leaveCriticalSection(globalLockCs);
  end;

PROCEDURE T_adapters.remSubAdapters(CONST sub:P_adapters);
  VAR i:longint;
  begin
    enterCriticalSection(globalLockCs);
    for i:=0 to length(subAdapters)-1 do if subAdapters[i]=sub then begin
      subAdapters[i]:=subAdapters[length(subAdapters)-1];
      setLength(subAdapters,length(subAdapters)-1);
      leaveCriticalSection(globalLockCs);
      exit;
    end;
    leaveCriticalSection(globalLockCs);
  end;

FUNCTION T_adapters.getConnector(CONST includePrint,includeWarnings,includeErrors:boolean):P_connectorAdapter;
  begin new(result,create(@self,includePrint,includeWarnings,includeErrors)); end;
//===================================================================:T_adapters

INITIALIZATION
  defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;
  initCriticalSection(globalLockCs);

FINALIZATION
  doneCriticalSection(globalLockCs);
end.
