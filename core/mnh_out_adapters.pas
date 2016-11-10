UNIT mnh_out_adapters;
INTERFACE
USES mnh_constants, mnh_basicTypes, myGenerics,mySys,sysutils,myStringUtil{$ifdef fullVersion},mnh_plotData{$endif}{$ifdef imig},mypics{$endif};
TYPE
  T_storedMessage = record
    messageType : T_messageType;
    simpleMessage: ansistring;
    multiMessage: T_arrayOfString;
    location: T_searchTokenLocation;
  end;
  T_storedMessages = array of T_storedMessage;
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
    {at_textFile} [mt_printline..mt_el5_haltMessageReceived,mt_timing_info],
    {at_htmlFile} [mt_echo_input..mt_el5_haltMessageQuiet,mt_timing_info{$ifdef fullVersion},mt_plotFileCreated,mt_plotCreatedWithInstantDisplay{$endif}],
    {at_gui}      [low(T_messageType)..high(T_messageType)],
    {at_sandbo...}[low(T_messageType)..high(T_messageType)],
    {at_printT...}[mt_printline]);
  {$ifdef fullVersion}
  HTML_IMAGE_WIDTH        =1000;
  HTML_IMAGE_HEIGHT       = 750;
  HTML_IMAGE_SUPERSAMPLING=   2;
  {$endif}
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
      {$ifdef fullVersion}
      hasHtmlAdapter           :boolean;
      plotChangedAfterDeferring:boolean;
      {$endif}
    public
      hasMessageOfType:array[T_messageType] of boolean;
      {$ifdef fullVersion}
      hasNeedGUIerror:boolean;
      plot:T_plot;
      {$endif}
      {$ifdef imig}
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
      PROCEDURE haltEvaluation;
      PROCEDURE logEndOfEvaluation;
      PROCEDURE raiseSystemError(CONST errorMessage: ansistring);

      PROCEDURE addOutAdapter(CONST p:P_abstractOutAdapter; CONST destroyIt:boolean);
      FUNCTION addConsoleOutAdapter(CONST verbosity:string=''):P_consoleOutAdapter;
      PROCEDURE removeOutAdapter(CONST p:P_abstractOutAdapter);
      PROCEDURE removeOutAdapter(CONST index:longint);
      PROCEDURE setPrintTextFileAdapter(CONST filenameOrBlank:string);

      FUNCTION adapterCount:longint;
      FUNCTION getAdapter(CONST index:longint):P_abstractOutAdapter;

      FUNCTION collectingClone:P_adapters;
      PROCEDURE copyDataFromCollectingCloneDisposing(VAR clone:P_adapters; CONST errorCase:boolean);

      PROCEDURE setExitCode;
      FUNCTION triggersBeep:boolean;

      PROPERTY doEchoInput:         boolean read someEchoInput        ;
      PROPERTY doEchoDeclaration:   boolean read someEchoDeclaration  ;
      PROPERTY doShowExpressionOut: boolean read someShowExpressionOut;
      PROPERTY doShowTimingInfo:    boolean read someShowTimingInfo   ;
  end;

CONST
  C_defaultOutputBehavior_interactive:T_messageTypeSet=[mt_clearConsole,
    mt_printline,
    mt_echo_input,
    mt_echo_declaration,
    mt_echo_output,
    mt_echo_continued,
    mt_el3_evalError..high(T_messageTypeSet)];

  C_defaultOutputBehavior_fileMode:T_messageTypeSet=[mt_clearConsole,mt_printline,mt_el3_evalError..mt_reloadRequired
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
FUNCTION message(CONST messageType  : T_messageType;
                 CONST simpleMessage: ansistring;
                 CONST location     : T_searchTokenLocation):T_storedMessage;
FUNCTION defaultFormatting(CONST message:T_storedMessage):ansistring;
FUNCTION defaultFormatting(CONST messageType:T_messageType; CONST message: ansistring; CONST location: T_searchTokenLocation):ansistring;
OPERATOR :=(s:string):T_messageTypeSet;
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
      'p': result:=result+[mt_printline,mt_clearConsole];
      'P': result:=result-[mt_printline,mt_clearConsole];
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
      '1'..'5': begin
        level:=strToInt(s[i]);
        for mt:=low(T_messageType) to high(T_messageType) do if C_messageTypeMeta[mt].level>0 then begin
          if C_messageTypeMeta[mt].level>=level then result:=result+[mt]
                                                else result:=result-[mt];
        end;
      end;
    end;
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
  begin
    result:=message.messageType in messageTypesToInclude;
    if result then with message do case messageType of
      mt_clearConsole: mySys.clearConsole;
      mt_printline: begin
        if not(mySys.isConsoleShowing) then mySys.showConsole;
        for i:=0 to length(multiMessage)-1 do begin
          if multiMessage[i]=C_formFeedChar
          then mySys.clearConsole
          else writeln(multiMessage[i]);
        end;
      end
      else writeln(stdErr,defaultFormatting(message));
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

PROCEDURE T_collectingOutAdapter.clear;
  begin
    system.enterCriticalSection(cs);
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
          mt_printline: for j:=0 to length(multiMessage)-1 do writeln(handle,multiMessage[j]);
          else writeln(handle,defaultFormatting(storedMessages[i]));
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
    plot.createWithDefaults;
    {$endif}
    {$ifdef imig}
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
    {$ifdef imig}
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
    {$ifdef fullVersion}
    hasNeedGUIerror:=false;
    {$endif}
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
  {$ifdef fullVersion}
  PROCEDURE raiseModifiedMessage;
    VAR modifiedMessage:T_storedMessage;
        i:longint;
    begin
      modifiedMessage.messageType  :=message.messageType;
      modifiedMessage.location     :=message.location;
      modifiedMessage.simpleMessage:=message.simpleMessage;
      setLength(modifiedMessage.multiMessage,1);
      modifiedMessage.multiMessage[0]:=plot.renderToString(HTML_IMAGE_WIDTH,HTML_IMAGE_HEIGHT,HTML_IMAGE_SUPERSAMPLING);
      for i:=0 to length(adapter)-1 do
        if adapter[i]^.adapterType=at_htmlFile
        then adapter[i]^.append(modifiedMessage)
        else adapter[i]^.append(message);
    end;
  {$endif}

  VAR i:longint;
  begin
    {$ifdef fullVersion}
    hasNeedGUIerror:=hasNeedGUIerror or not(gui_started) and C_messageTypeMeta[message.messageType].triggersGuiStartup;
    {$endif}
    if maxErrorLevel< C_messageTypeMeta[message.messageType].level then
       maxErrorLevel:=C_messageTypeMeta[message.messageType].level;
    if hasHaltMessage and not(message.messageType in [mt_endOfEvaluation,mt_timing_info]) then exit;
    hasMessageOfType[message.messageType]:=true;
    if (message.messageType=mt_el3_stackTrace) then begin
      inc(stackTraceCount);
      if stackTraceCount>30 then exit;
    end;
    if (message.messageType in [mt_el3_evalError,mt_el3_noMatchingMain,mt_el4_parsingError,mt_el5_haltMessageReceived,mt_el5_systemError]) then begin
      inc(errorCount);
      if errorCount>30 then exit;
    end;
    {$ifdef fullVersion}
    if message.messageType in C_plotMessages then plotChangedAfterDeferring:=true;
    if hasHtmlAdapter and ((message.messageType in [mt_plotCreatedWithInstantDisplay,mt_plotFileCreated])
                        or (message.messageType=mt_endOfEvaluation) and plotChangedAfterDeferring)
    then begin
      raiseModifiedMessage;
      plotChangedAfterDeferring:=false;
      exit;
    end;
    {$endif}
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
  VAR m:T_storedMessage;
  begin
    m.messageType:=mt_printline;
    m.multiMessage:=s;
    raiseCustomMessage(m);
  end;

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
    someEchoInput        :=false;
    someEchoDeclaration  :=false;
    someShowExpressionOut:=false;
    someShowTimingInfo   :=false;
    {$ifdef fullVersion}
    hasHtmlAdapter           :=false;
    plotChangedAfterDeferring:=false;
    {$endif}
    for i:=0 to length(adapter)-1 do begin
      adapter[i]^.clear;
      someEchoInput        :=someEchoInput         or (mt_echo_input       in adapter[i]^.messageTypesToInclude);
      someEchoDeclaration  :=someEchoDeclaration   or (mt_echo_declaration in adapter[i]^.messageTypesToInclude);
      someShowExpressionOut:=someShowExpressionOut or (mt_echo_output      in adapter[i]^.messageTypesToInclude);
      someShowTimingInfo   :=someShowTimingInfo    or (mt_timing_info      in adapter[i]^.messageTypesToInclude);
      {$ifdef fullVersion}
      hasHtmlAdapter       :=hasHtmlAdapter        or (adapter[i]^.adapterType=at_htmlFile);
      {$endif}
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
    {$ifdef fullVersion}
    hasHtmlAdapter:=hasHtmlAdapter or (p^.adapterType=at_htmlFile);
    {$endif}
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
    {$ifdef fullVersion}
    hasHtmlAdapter:=false;
    for j:=0 to length(adapter)-1 do hasHtmlAdapter:=hasHtmlAdapter or (adapter[j]^.adapterType=at_htmlFile);
    {$endif}
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
        txtAdapter^.messageTypesToInclude:=[mt_printline];
        addOutAdapter(txtAdapter,true);
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
//===================================================================:T_adapters

INITIALIZATION
  defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;

end.
