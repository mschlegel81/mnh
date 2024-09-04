UNIT codeAssistance;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, Controls, Graphics, Dialogs, SynEdit,
  myGenerics,
  basicTypes,
  mnh_constants,
  fileWrappers,
  litVar,
  tokenArray,
  mnh_messages,
  out_adapters,
  contexts,
  recyclers,
  packages,
  funcs,
  Forms,
  ComCtrls;

CONST
  MARKER_USAGE_SCAN_END='#';

TYPE
  T_highlightingData=object
    private
      highlightingCs:TRTLCriticalSection;
      warnLocations:array of record line,column:longint; isError:boolean; end;
      userRules    :T_setOfString;
      localIdInfos :T_callAndIdInfos;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      FUNCTION isUserRule(CONST id: string): boolean;
      FUNCTION isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
      FUNCTION isLocalId(CONST id: string; CONST lineIndex, colIdx: longint): boolean;
      PROCEDURE clearLocalIdInfos;
      FUNCTION getRelatedLocations(CONST CaretX,CaretY:longint):T_relatedTokens;
  end;

  P_codeAssistanceResponse=^T_codeAssistanceResponse;
  T_codeAssistanceResponse=object(T_payloadMessage)
    private
      localErrors,externalErrors:T_storedMessages;
      callAndIdInfos:P_callAndIdInfos;
      responseStateHash:T_hashInt;

      CONSTRUCTOR create(CONST package_:P_package; CONST messages:T_storedMessages; CONST stateHash_:T_hashInt; CONST callAndIdInfos_:P_callAndIdInfos);
    public
      package:P_package;
      DESTRUCTOR destroy; virtual;
      PROPERTY  stateHash:T_hashInt read responseStateHash;
      FUNCTION getErrorHints(VAR edit:TSynEdit; OUT hasErrors, hasWarnings: boolean; CONST appendMode:boolean=false):T_searchTokenLocations;
      //Highlighter related:
      PROCEDURE updateHighlightingData(VAR highlightingData:T_highlightingData);
      //Rename/Help:
      FUNCTION  explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo):boolean;
      FUNCTION  renameIdentifierInLine(CONST idLocation:T_searchTokenLocation; CONST oldId,newId:string; VAR lineText:ansistring; CONST CaretY:longint):boolean;
      FUNCTION  usedAndExtendedPackages:T_arrayOfString;
      //Completion related
      FUNCTION  getImportablePackages:T_arrayOfString;
      FUNCTION  updateCompletionList(VAR wordsInEditor:T_setOfString; CONST lineIndex, colIdx: longint):boolean;
      //Shebang related
      FUNCTION  getBuiltinSideEffects:T_sideEffects;
      FUNCTION  isExecutablePackage:boolean;
  end;
  F_simpleCallback=PROCEDURE of object;

PROCEDURE finalizeCodeAssistance;
PROCEDURE ensureDefaultFiles(CONST progressCallback:F_simpleCallback; CONST overwriteExisting:boolean=false);
PROCEDURE postAssistanceRequest    (CONST scriptPath:string);
FUNCTION  getAssistanceResponseSync(CONST editorMeta:P_codeProvider; CONST secondary:P_codeProvider=nil):P_codeAssistanceResponse;
FUNCTION findScriptsUsing(CONST scriptName:string):T_arrayOfString;
FUNCTION findRelatedScriptsTransitive(CONST scriptName:string):T_arrayOfString;

VAR preparedResponses:specialize G_threadsafeQueue<P_codeAssistanceResponse>;
IMPLEMENTATION
USES FileUtil,sysutils,myStringUtil,commandLineParameters,SynHighlighterMnh,mnh_doc,messageFormatting,mySys,LazUTF8;
TYPE T_usagePair=record usedScript,usingScript:string; end;
VAR scriptUsage:record
      dat:array of T_usagePair;
      scriptsScanned:T_arrayOfString;
      scanning:boolean;
    end;

TYPE
  P_codeAssistanceRequest=^T_codeAssistanceRequest;
  T_codeAssistanceRequest=object
    private
      //Input:
      scriptPath             :string;
      provider,secondaryProvider:P_codeProvider;

      FUNCTION execute(CONST recycler:P_recycler; CONST givenGlobals:P_evaluationGlobals=nil; CONST givenAdapters:P_messagesErrorHolder=nil):P_codeAssistanceResponse; virtual;
    public
      CONSTRUCTOR createWithProvider(CONST editorMeta:P_codeProvider);
      CONSTRUCTOR create(CONST path:string);
      DESTRUCTOR destroy;
  end;

  T_codeAssistanceThread=class(T_basicThread)
    protected
      PROCEDURE execute; override;
    public
      CONSTRUCTOR create;
  end;

VAR codeAssistanceCs:TRTLCriticalSection;
    codeAssistanceThread:T_codeAssistanceThread=nil;
    codeAssistantIsRunning:boolean=false;
    shuttingDown          :boolean=false;
    pendingRequests       :specialize G_threadsafeQueue<P_codeAssistanceRequest>;
    isFinalized:boolean=false;

FUNCTION findScriptsUsing(CONST scriptName:string):T_arrayOfString;
  VAR i:longint;
  begin
    enterCriticalSection(codeAssistanceCs);
    try
      with scriptUsage do begin
        initialize(result);
        setLength(result,0);
        for i:=0 to length(dat)-1 do if dat[i].usedScript=scriptName then appendIfNew(result,dat[i].usingScript);
      end;
    finally
      leaveCriticalSection(codeAssistanceCs);
    end;
  end;

FUNCTION findRelatedScriptsTransitive(CONST scriptName:string):T_arrayOfString;
  VAR i:longint;
      countBefore:longint=0;
      script:string;
  begin
    enterCriticalSection(codeAssistanceCs);
    try
      with scriptUsage do begin
        result:=scriptName;
        repeat
          countBefore:=length(result);
          for script in result do
          for i:=0 to length(dat)-1 do
            if dat[i].usedScript=script
            then appendIfNew(result,dat[i].usingScript)
            else if dat[i].usingScript=script
            then appendIfNew(result,dat[i].usedScript);
        until countBefore=length(result);
        dropValues(result,scriptName);
        sortUnique(result);
      end;
    finally
      leaveCriticalSection(codeAssistanceCs);
    end;
  end;

PROCEDURE T_codeAssistanceThread.execute;
  CONST MAX_SLEEP_BETWEEN_RUNS_MILLIS =500;

  VAR sleepBetweenRunsMillis:longint=0;
      globals:P_evaluationGlobals;
      adapters:T_messagesErrorHolder;
      recycler:P_recycler;
      requests:array of P_codeAssistanceRequest;
      isLatest:boolean;
      i,j:longint;

  FUNCTION findUsedAndExtendedPackages(CONST fileName:string):T_arrayOfString;
    VAR package:T_package;
    begin
      try
        package.create(newCodeProvider(fileName),nil);
        globals^.resetForEvaluation(@package,@package.reportVariables,C_sideEffectsForCodeAssistance,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
        package.load(lu_usageScan,globals^,recycler,C_EMPTY_STRING_ARRAY);
        result:=package.usedAndExtendedPackages;
      except end;
      globals^.afterEvaluation(recycler,packageTokenLocation(@package));
      globals^.primaryContext.finalizeTaskAndDetachFromParent(recycler);
      package.destroy;
    end;

  PROCEDURE updateScriptUsage(CONST scriptName:string; CONST allUses:T_arrayOfString);
    VAR i,j:longint;
        canonicalScriptName:string;
    begin
      canonicalScriptName:=canonicalFileName(scriptName);
      try
        enterCriticalSection(codeAssistanceCs);
        with scriptUsage do begin
          j:=0;
          for i:=0 to length(dat)-1 do if dat[i].usingScript<>canonicalScriptName
          then begin
            if j<>i then dat[j]:=dat[i];
            inc(j);
          end;
          setLength(dat,j+length(allUses));
          for i:=0 to length(allUses)-1 do begin
            dat[j+i].usingScript:=canonicalScriptName;
            dat[j+i].usedScript:=canonicalFileName(allUses[i]);
          end;
        end;
      finally
        leaveCriticalSection(codeAssistanceCs);
      end;
    end;

  PROCEDURE scanSingleScriptUsage(CONST scriptFileName:string);
    begin
      enterCriticalSection(codeAssistanceCs);
      if arrContains(scriptUsage.scriptsScanned,scriptFileName) then begin
        leaveCriticalSection(codeAssistanceCs);
        exit;
      end;
      leaveCriticalSection(codeAssistanceCs);
      {$ifdef debugMode}
      writeln('  codeAssistanceThread: scan usages of ',scriptFileName);
      {$endif}

      updateScriptUsage(scriptFileName,findUsedAndExtendedPackages(scriptFileName));

      enterCriticalSection(codeAssistanceCs);
      append(scriptUsage.scriptsScanned,scriptFileName);
      leaveCriticalSection(codeAssistanceCs);
    end;

  PROCEDURE scanScript(CONST request:P_codeAssistanceRequest);
    VAR response: P_codeAssistanceResponse;
    begin
      {$ifdef debugMode}
      writeln('  code assistance processes script ',request^.scriptPath);
      {$endif}
      response:=request^.execute(recycler,globals,@adapters);
      {$ifdef debugMode}
      writeln('  code assistance response for     ',request^.scriptPath,' is ',response^.stateHash);
      {$endif}
      updateScriptUsage(response^.package^.getPath,response^.usedAndExtendedPackages);
      preparedResponses.append(response);
    end;

  VAR pendingFileScans:specialize G_queue<string>;
      s:string;
  begin
    {$ifdef debugMode}
    writeln('  codeAssistanceThread started');
    {$endif}
    pendingFileScans.create;
    threadStartsSleeping; //low prio thread
    postIdeMessage('Code assistance started',false);
    //setup:
    adapters.createErrorHolder(nil,C_errorsAndWarnings+[mt_el1_note]);
    new(globals,create(@adapters));
    recycler:=newRecycler;
    //:setup
    repeat
      requests:=pendingRequests.getAll;
      if length(requests)=0
      then begin
        if pendingFileScans.hasNext then begin
          scanSingleScriptUsage(pendingFileScans.next);
          sleepBetweenRunsMillis:=0;
        end else begin
          for s in fileCache.getAllFilesForBackgroundScan do
          if not arrContains(scriptUsage.scriptsScanned,s)
          then pendingFileScans.append(s);
          if not(pendingFileScans.hasNext) then begin
            sleepBetweenRunsMillis+=1;
            sleep(sleepBetweenRunsMillis);
          end;
        end;
      end else begin
        sleepBetweenRunsMillis:=0;
        {$ifdef debugMode}
        writeln('  processing ',length(requests),' code assistance requests');
        {$endif}
        for i:=0 to length(requests)-1 do begin
          isLatest:=not(shuttingDown);
          for j:=i+1 to length(requests)-1 do isLatest:=isLatest and (requests[i]^.scriptPath<>requests[j]^.scriptPath);
          if isLatest then scanScript(requests[i]);
          dispose(requests[i],destroy);
        end;
        setLength(requests,0);
      end;
      recycler^.cleanupIfPosted;
      enterCriticalSection(codeAssistanceCs);
      isLatest:=shuttingDown;
      leaveCriticalSection(codeAssistanceCs);
    until isLatest or (sleepBetweenRunsMillis>=MAX_SLEEP_BETWEEN_RUNS_MILLIS) or Terminated;
    //shutdown:
    dispose(globals,destroy);
    adapters.destroy;
    enterCriticalSection(codeAssistanceCs);
    codeAssistantIsRunning:=false;
    leaveCriticalSection(codeAssistanceCs);
    freeRecycler(recycler);
    //:shutdown
    threadStopsSleeping;
    postIdeMessage('Code assistance stopped',false);
    {$ifdef debugMode}
    writeln('  codeAssistanceThread stopped');
    {$endif}
    Terminate;
  end;

CONSTRUCTOR T_codeAssistanceThread.create;
  begin
    inherited create(tpLower);
  end;

DESTRUCTOR T_codeAssistanceRequest.destroy;
  begin
  end;

FUNCTION T_codeAssistanceRequest.execute(CONST recycler:P_recycler; CONST givenGlobals:P_evaluationGlobals=nil; CONST givenAdapters:P_messagesErrorHolder=nil):P_codeAssistanceResponse;
  VAR //temporary
      globals:P_evaluationGlobals;
      adapters:T_messagesErrorHolder;
      //output
      initialStateHash:T_hashInt=0;
      package:P_package;
      callAndIdInfos:P_callAndIdInfos;
      loadMessages:T_storedMessages;

  PROCEDURE loadSecondaryPackage(CONST name:string);
    VAR user:T_package;
        secondaryCallInfos:T_callAndIdInfos;
    begin
      globals^.primaryContext.callDepth:=STACK_DEPTH_LIMIT-100;
      if globals^.primaryContext.callDepth<0 then globals^.primaryContext.callDepth:=0;
      user.create(newCodeProvider(name),nil);
      secondaryCallInfos.create;
      user.load(lu_forCodeAssistanceSecondary,globals^,recycler,C_EMPTY_STRING_ARRAY,@secondaryCallInfos);
      callAndIdInfos^.includeUsages(@secondaryCallInfos);
      secondaryCallInfos.destroy;
      globals^.primaryContext.messages^.clearFlags;
      {$Q-}{$R-}
      initialStateHash:=initialStateHash*127+user.getCodeState;
      {$Q+}{$R+}
      user.destroy;
    end;

  VAR s,script:ansistring;
      codeProvider:P_codeProvider;
      additionalScriptsToScan:T_arrayOfString;
  begin
    {$ifdef debugMode}
    writeln('Code assistance start: ',scriptPath);
    {$endif}
    if provider=nil
    then codeProvider:=newCodeProvider(scriptPath)
    else codeProvider:=provider;

    new(package,create(codeProvider,nil));

    {$Q-}{$R-}
    additionalScriptsToScan:=findScriptsUsing(scriptPath);
    initialStateHash:=codeProvider^.stateHash+length(additionalScriptsToScan)*127;
    for s in additionalScriptsToScan do initialStateHash:=initialStateHash*31+hashOfAnsiString(s);
    {$Q+}{$R+}

    if givenGlobals=nil then begin
      adapters.createErrorHolder(nil,C_errorsAndWarnings+[mt_el1_note]);
      new(globals,create(@adapters));
    end else begin
      globals:=givenGlobals;
      givenAdapters^.clear;
    end;

    globals^.resetForEvaluation(nil,nil,C_sideEffectsForCodeAssistance,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
    globals^.primaryContext.callDepth:=STACK_DEPTH_LIMIT-100;
    if globals^.primaryContext.callDepth<0 then globals^.primaryContext.callDepth:=0;
    new(callAndIdInfos,create);
    {$ifdef debugMode}
    write('Additional scripts to scan :',length(additionalScriptsToScan),': ');
    for script in additionalScriptsToScan do write(script,',');
    writeln;
    {$endif}

    for script in additionalScriptsToScan do loadSecondaryPackage(script);
    globals^.primaryContext.messages^.clear();
    package^.load(lu_forCodeAssistance,globals^,recycler,C_EMPTY_STRING_ARRAY,callAndIdInfos,secondaryProvider);
    if givenGlobals<>nil then loadMessages:=givenAdapters^.storedMessages(true)
                         else loadMessages:=adapters      .storedMessages(true);
    globals^.afterEvaluation(recycler,packageTokenLocation(package));
    {$ifdef debugMode}
    writeln('Code assistance end  : ',scriptPath);
    {$endif}
    new(result,create(package,loadMessages,initialStateHash,callAndIdInfos));
    if givenGlobals=nil then begin
      adapters.destroy;
      dispose(globals,destroy);
    end;
  end;

PROCEDURE T_codeAssistanceResponse.updateHighlightingData(VAR highlightingData: T_highlightingData);
  VAR k:longint;
      e:P_storedMessage;
  begin
    enterCriticalSection(highlightingData.highlightingCs);
    enterCriticalSection(messageCs);
    try
      highlightingData.userRules.clear;
      package^.ruleMap.updateLists(highlightingData.userRules,false);
      highlightingData.localIdInfos.copyFrom(callAndIdInfos);
      setLength(highlightingData.warnLocations,length(localErrors));
      k:=0;
      for e in localErrors do begin
        if k>=length(highlightingData.warnLocations) then setLength(highlightingData.warnLocations,k+1);
        highlightingData.warnLocations[k].line   :=e^.getLocation.line;
        highlightingData.warnLocations[k].column :=e^.getLocation.column;
        highlightingData.warnLocations[k].isError:=C_messageTypeMeta[e^.messageType].level>2;
        inc(k);
      end;
      setLength(highlightingData.warnLocations,k);
    finally
      leaveCriticalSection(highlightingData.highlightingCs);
      leaveCriticalSection(messageCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo): boolean;
  VAR lexer:T_singleStringLexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  PROCEDURE appendUsageInfo;
    begin
      if (info.tokenType in [tt_userRule,tt_customType,tt_globalVariable,tt_customTypeCheck]) then begin
        info.referencedAt:=callAndIdInfos^.whoReferencesLocation(info.location);
      end;
    end;

  VAR trueCaretX:longint=1;
      i:longint=1;
  begin
    enterCriticalSection(messageCs);
    try
      loc.line:=CaretY;
      loc.column:=1;
      loc.package:=package;
      result:=(fullLine<>info.fullLine) or (CaretX<>info.CaretX);
      if result then begin
        //In case of multibyte characters, the input caret has to be "translated"
        while (trueCaretX<=length(fullLine)) and (i<CaretX) do begin
          trueCaretX+=UTF8CodepointSize(@fullLine[trueCaretX]);
          i+=1;
        end;
        lexer.create(fullLine,loc,package);
        enhanced:=lexer.getEnhancedTokens(callAndIdInfos);
        info:=enhanced.getTokenAtIndex(trueCaretX).toInfo;
        info.fullLine:=fullLine;
        info.CaretX:=CaretX;
        appendUsageInfo;
        enhanced.destroy;
        lexer.destroy;
      end;
    finally
      leaveCriticalSection(messageCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.renameIdentifierInLine(CONST idLocation: T_searchTokenLocation; CONST oldId, newId: string;
  VAR lineText: ansistring; CONST CaretY: longint): boolean;
  VAR lexer:T_singleStringLexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  begin
    enterCriticalSection(messageCs);
    try
      loc.line:=CaretY;
      loc.column:=1;
      loc.package:=package;
      lexer.create(lineText,loc,package);
      enhanced:=lexer.getEnhancedTokens(callAndIdInfos);
      result:=enhanced.renameInLine(lineText,idLocation,oldId,newId);
      enhanced.destroy;
      lexer.destroy;
    finally
      leaveCriticalSection(messageCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.usedAndExtendedPackages: T_arrayOfString;
  begin
    result:=package^.usedAndExtendedPackages;
  end;

FUNCTION T_codeAssistanceResponse.getImportablePackages: T_arrayOfString;
  begin
    result:=fileCache.availablePackages(extractFilePath(package^.getPath));
  end;

FUNCTION T_codeAssistanceResponse.updateCompletionList(VAR wordsInEditor: T_setOfString; CONST lineIndex, colIdx: longint): boolean;
  VAR s:string;
  begin
    enterCriticalSection(messageCs);
    try
      package^.ruleMap.updateLists(wordsInEditor,true);
      for s in callAndIdInfos^.allLocalIdsAt(lineIndex,colIdx) do wordsInEditor.put(s);
      result:=(package^.ruleMap.size>0) or not(callAndIdInfos^.isEmpty);
    finally
      leaveCriticalSection(messageCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.getBuiltinSideEffects:T_sideEffects;
  begin
    enterCriticalSection(messageCs);
    try
      result:=callAndIdInfos^.getBuiltinSideEffects;
    finally
      leaveCriticalSection(messageCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.isExecutablePackage: boolean;
  begin
    result:=package^.isExecutable;
  end;

PROCEDURE ensureCodeAssistanceThread;
  begin
    if (gui_started<>ide) or isFinalized then exit;
    enterCriticalSection(codeAssistanceCs);
    try
      if not(codeAssistantIsRunning) then begin
        codeAssistantIsRunning:=true;
        codeAssistanceThread:=T_codeAssistanceThread.create;
      end;
    finally
      leaveCriticalSection(codeAssistanceCs);
    end;
  end;

PROCEDURE postAssistanceRequest(CONST scriptPath:string);
  VAR request:P_codeAssistanceRequest;
  begin
    new(request,create(scriptPath));
    {$ifdef debugMode}
    writeln('  assistance request posted for ',scriptPath);
    {$endif}
    pendingRequests.append(request);
    ensureCodeAssistanceThread;
  end;

FUNCTION stringsEqual(CONST a,b:string):boolean;
  begin
    result:=a=b;
  end;

FUNCTION getAssistanceResponseSync(CONST editorMeta:P_codeProvider; CONST secondary:P_codeProvider=nil):P_codeAssistanceResponse;
  VAR request:P_codeAssistanceRequest;
      recycler:P_recycler;
  begin
    new(request,createWithProvider(editorMeta));
    recycler:=newRecycler;
    request^.secondaryProvider:=secondary;
    result:=request^.execute(recycler);
    freeRecycler(recycler);
    dispose(request,destroy);
  end;

FUNCTION getMessagesForInspection(CONST context:P_context; CONST recycler:P_recycler; CONST scriptPath:string):P_mapLiteral;
  VAR request:P_codeAssistanceRequest;
      response: P_codeAssistanceResponse;
      allErrors: T_storedMessages;
      i:longint;
  begin
    new(request,create(scriptPath));
    response:=request^.execute(recycler);
    dispose(request,destroy);

    setLength(allErrors,length(response^.localErrors)+length(response^.externalErrors));
    for i:=0 to length(response^.localErrors)-1 do
      allErrors[i]:=response^.localErrors[i];
    for i:=0 to length(response^.externalErrors)-1 do
      allErrors[i+length(response^.localErrors)]:=response^.externalErrors[i];

    if response^.package=nil
    then result:=recycler^.newMapLiteral(1)
    else result:=response^.package^.inspect(false,context,recycler,response^.callAndIdInfos);
    result^.put(recycler,'messages',messagesToLiteralForSandbox(recycler,allErrors,C_textMessages,SUPPRESS_EXIT_CODE),false);

    setLength(allErrors,0);
    response^.unreference;
    dispose(response,destroy);
  end;

PROCEDURE ensureDefaultFiles(CONST progressCallback:F_simpleCallback; CONST overwriteExisting: boolean);
  {$i res_defaultFiles.inc}
  VAR baseDir:string;
  PROCEDURE ensureFile(CONST index:longint);
    VAR fileName:string;
        fileContent:string;
    begin
      fileName:=baseDir+DEFAULT_FILES[index,0];
      if overwriteExisting or not(fileExists(fileName)) then begin
        fileContent:=decompressString(DEFAULT_FILES[index,1]);
        fileWrappers.writeFile(fileName,fileContent);
      end;
    end;

  VAR i:longint;
  begin
    baseDir:=configDir;
    for i:=0 to length(DEFAULT_FILES)-1 do begin
      ensureFile(i);
      if progressCallback<>nil then progressCallback();
    end;
  end;

CONSTRUCTOR T_codeAssistanceRequest.createWithProvider(CONST editorMeta:P_codeProvider);
  begin
    scriptPath:=editorMeta^.getPath;
    provider:=editorMeta;
    secondaryProvider:=nil;
  end;

CONSTRUCTOR T_codeAssistanceRequest.create(CONST path:string);
  begin
    scriptPath:=path;
    provider:=nil;
  end;

CONSTRUCTOR T_highlightingData.create;
  begin
    initCriticalSection(highlightingCs);
    enterCriticalSection(highlightingCs);
    try
      userRules   .create;
      localIdInfos.create;
      setLength(warnLocations,0);
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

PROCEDURE T_highlightingData.clear;
  begin
    enterCriticalSection(highlightingCs);
    try
      userRules   .clear;
      localIdInfos.clear;
      setLength(warnLocations,0);
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

DESTRUCTOR T_highlightingData.destroy;
  begin
    enterCriticalSection(highlightingCs);
    try
      userRules   .destroy;
      localIdInfos.destroy;
      setLength(warnLocations,0);
    finally
      leaveCriticalSection(highlightingCs);
      doneCriticalSection(highlightingCs);
    end;
  end;

FUNCTION T_highlightingData.isUserRule(CONST id: string): boolean;
  begin
    enterCriticalSection(highlightingCs);
    try
      result:=userRules.contains(id);
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

FUNCTION T_highlightingData.isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
  VAR k:longint;
  begin
    enterCriticalSection(highlightingCs);
    try
      result:=0;
      for k:=0 to length(warnLocations)-1 do if result<2 then with warnLocations[k] do
      if (result=0) and (lineIndex=line-1) and ((column<0) or (tokenStart<=column-1) and (tokenEnd>column-1)) then begin
        if isError
        then result:=2
        else if   result< 1
             then result:=1;
      end;
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

FUNCTION T_highlightingData.isLocalId(CONST id: string; CONST lineIndex, colIdx: longint): boolean;
  VAR dummyLocation:T_tokenLocation;
  begin
    enterCriticalSection(highlightingCs);
    try
      result:=localIdInfos.localTypeOf(id,lineIndex,colIdx,dummyLocation)=tt_blockLocalVariable;
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

PROCEDURE T_highlightingData.clearLocalIdInfos;
  begin
    enterCriticalSection(highlightingCs);
    try
      localIdInfos.clear;
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

FUNCTION T_highlightingData.getRelatedLocations(CONST CaretX,CaretY:longint):T_relatedTokens;
  begin
    enterCriticalSection(highlightingCs);
    try
      result:=localIdInfos.getRelated(CaretX,CaretY);
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

CONSTRUCTOR T_codeAssistanceResponse.create(CONST package_:P_package; CONST messages:T_storedMessages; CONST stateHash_:T_hashInt; CONST callAndIdInfos_:P_callAndIdInfos);
  PROCEDURE sortByPriorityAndLocation(VAR list:T_storedMessages);
    VAR i,j:longint;
        tmp:P_storedMessage;
    begin
      for i:=1 to length(list)-1 do for j:=0 to i-1 do
      if (C_messageTypeMeta[list[i]^.messageType].level>
          C_messageTypeMeta[list[j]^.messageType].level) or
         (C_messageTypeMeta[list[i]^.messageType].level=
          C_messageTypeMeta[list[j]^.messageType].level) and
         (list[i]^.getLocation<list[j]^.getLocation)
      then begin
        tmp:=list[i]; list[i]:=list[j]; list[j]:=tmp;
      end;
    end;

  VAR m:P_storedMessage;
      localCount:longint=0;
      externalCount:longint=0;
  begin
    inherited create(mt_ide_codeAssistanceResponse);
    package:=package_;
    responseStateHash:=stateHash_;
    callAndIdInfos:=callAndIdInfos_;
    callAndIdInfos^.cleanup;
    setLength(localErrors   ,length(messages));
    setLength(externalErrors,length(messages));

    for m in messages do
      if m^.getLocation.fileName=package_^.getPath
      then begin
        localErrors[localCount]:=m^.rereferenced;
        inc(localCount);
      end else begin
        externalErrors[externalCount]:=m^.rereferenced;
        inc(externalCount);
      end;
    setLength(localErrors,localCount);
    sortByPriorityAndLocation(localErrors);
    setLength(externalErrors,externalCount);
    sortByPriorityAndLocation(externalErrors);
  end;

DESTRUCTOR T_codeAssistanceResponse.destroy;
  VAR m:P_storedMessage;
  begin
    enterCriticalSection(messageCs);
    try
      for m in localErrors do disposeMessage_(m);
      setLength(localErrors,0);
      for m in externalErrors do disposeMessage_(m);
      setLength(externalErrors,0);
      dispose(callAndIdInfos,destroy);
      dispose(package,destroy);
    finally
      leaveCriticalSection(messageCs);
    end;
    inherited destroy;
  end;

FUNCTION T_codeAssistanceResponse.getErrorHints(VAR edit:TSynEdit; OUT hasErrors, hasWarnings: boolean; CONST appendMode:boolean=false):T_searchTokenLocations;
  VAR messageFormatter:T_guiFormatter;
      messagesAndLocations:T_messagesAndLocations;

  PROCEDURE addErrors(CONST list:T_storedMessages);
    VAR m:P_storedMessage;
    begin
      for m in list do begin
        hasErrors  :=hasErrors   or (C_messageTypeMeta[m^.messageType].level>2);
        hasWarnings:=hasWarnings or (C_messageTypeMeta[m^.messageType].level=2);
        messageFormatter.formatMessageAndLocation(m,messagesAndLocations);
      end;
    end;

  begin
    messageFormatter.create(false);
    enterCriticalSection(messageCs);
    messagesAndLocations.create(maxLongint);
    messageFormatter.preferredLineLength:=edit.charsInWindow;
    messageFormatter.wrapEcho:=true;
    hasErrors:=false;
    hasWarnings:=false;
    try
      addErrors(localErrors);
      addErrors(externalErrors);
    finally
      leaveCriticalSection(messageCs);
      messageFormatter.destroy;
    end;

    if not(appendMode) then begin
      edit.clearAll;
      edit.lines.clear;
    end;
    edit.lines.SetStrings(messagesAndLocations.text);
    result:=messagesAndLocations.locations;
    messagesAndLocations.destroy;
  end;

PROCEDURE finalizeCodeAssistance;
  VAR i:longint;
      response:P_codeAssistanceResponse;
      assistanceRequest:P_codeAssistanceRequest;
  begin
    enterCriticalSection(codeAssistanceCs);
    shuttingDown:=true;
    for i:=0 to 99 do if codeAssistantIsRunning then begin
      try
        codeAssistanceThread.Terminate;
      except
        //There is nothing we can do here...
      end;
      leaveCriticalSection(codeAssistanceCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(codeAssistanceCs);
    end;
    while pendingRequests.canGetNext(assistanceRequest) do dispose(assistanceRequest,destroy);

    if not(codeAssistantIsRunning)
    then begin
      doneCriticalSection(codeAssistanceCs);
      pendingRequests.destroy;
      while preparedResponses.canGetNext(response) do disposeMessage(response);
      preparedResponses.destroy;
    end;
    isFinalized:=true;
  end;

INITIALIZATION
  initialize(codeAssistanceCs);
  initCriticalSection(codeAssistanceCs);
  initialize(scriptUsage);
  packages.getMessagesForInspection:=@getMessagesForInspection;
  with scriptUsage do begin
    setLength(dat,0);
    setLength(scriptsScanned,0);
    scanning:=false;
  end;
  pendingRequests.create;
  preparedResponses.create;

FINALIZATION
  if not(isFinalized) then finalizeCodeAssistance;

end.

