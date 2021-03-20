UNIT codeAssistance;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, Controls, Graphics, Dialogs, SynEdit,
  myGenerics,
  basicTypes,
  mnh_constants,
  fileWrappers,
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
      PROCEDURE getErrorHints(VAR edit:TSynEdit; OUT hasErrors, hasWarnings: boolean; CONST appendMode:boolean=false);
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
      //Documentation-related
      PROCEDURE doCreateHtmlData;
  end;

PROCEDURE finalizeCodeAssistance;
PROCEDURE ensureDefaultFiles(Application:Tapplication; bar:TProgressBar; CONST overwriteExisting:boolean=false; CONST createHtmlDat:boolean=false);
PROCEDURE postAssistanceRequest    (CONST scriptPath:string);
FUNCTION  getAssistanceResponseSync(CONST editorMeta:P_codeProvider):P_codeAssistanceResponse;

PROCEDURE postUsageScan            (CONST folderToScan:string);
FUNCTION findScriptsUsing(CONST scriptName:string):T_arrayOfString;
FUNCTION findRelatedScriptsTransitive(CONST scriptName:string):T_arrayOfString;

VAR preparedResponses:specialize G_threadsafeQueue<P_codeAssistanceResponse>;
IMPLEMENTATION
USES FileUtil,sysutils,myStringUtil,commandLineParameters,SynHighlighterMnh,SynExportHTML,mnh_doc,messageFormatting,mySys;
TYPE T_usagePair=record usedScript,usingScript:string; end;
VAR scriptUsage:record
      dat:array of T_usagePair;
      foldersScanned:T_arrayOfString;
      scanning:boolean;
    end;

TYPE
  P_codeAssistanceRequest=^T_codeAssistanceRequest;
  T_codeAssistanceRequest=object
    private
      //Input:
      scriptPath             :string;
      provider               :P_codeProvider;

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
    pendingUsageScans     :specialize G_threadsafeQueue<string>;
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
      folderToScan:string;

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

  PROCEDURE scanFolder(CONST folder:string);
    VAR scriptsInThisFolder:TStringList;
        alreadyScannedFolder:string;
        script:string;
    begin
      enterCriticalSection(codeAssistanceCs);
      for alreadyScannedFolder in scriptUsage.foldersScanned do if folder=alreadyScannedFolder then begin
        leaveCriticalSection(codeAssistanceCs);
        exit;
      end;
      leaveCriticalSection(codeAssistanceCs);

      scriptsInThisFolder:=FindAllFiles(folder+DirectorySeparator,'*.mnh',false);
      //Load scripts
      for script in scriptsInThisFolder do if not Terminated then begin
        {$ifdef debugMode}
        writeln(stdErr,'T_codeAssistanceThread/scanFolder: scanning ',script);
        {$endif}
        updateScriptUsage(script,findUsedAndExtendedPackages(script))
      end;
      scriptsInThisFolder.free;

      enterCriticalSection(codeAssistanceCs);
      append(scriptUsage.foldersScanned,folder);
      leaveCriticalSection(codeAssistanceCs);
    end;

  VAR scriptsToScanAfterFileScanFinished:T_arrayOfString;
  PROCEDURE scanScript(CONST request:P_codeAssistanceRequest);
    VAR response: P_codeAssistanceResponse;
    begin
      appendIfNew(scriptsToScanAfterFileScanFinished,request^.scriptPath);
      response:=request^.execute(recycler,globals,@adapters);
      {$ifdef debugMode}
      writeln('  response for ',request^.scriptPath,' is ',response^.stateHash);
      {$endif}
      updateScriptUsage(response^.package^.getPath,response^.usedAndExtendedPackages);
      preparedResponses.append(response);
    end;

  begin
    {$ifdef debugMode}
    writeln('  codeAssistanceThread started');
    {$endif}
    setLength(scriptsToScanAfterFileScanFinished,0);
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
        if pendingUsageScans.canGetNext(folderToScan) then begin
          if folderToScan=MARKER_USAGE_SCAN_END
          then begin
            //rescan all scripts after folders have been scanned
            for folderToScan in scriptsToScanAfterFileScanFinished do postAssistanceRequest(folderToScan);
            setLength(scriptsToScanAfterFileScanFinished,0);

            //clean up pending folder scans
            scriptsToScanAfterFileScanFinished:=pendingUsageScans.getAll;
            dropValues(scriptsToScanAfterFileScanFinished,MARKER_USAGE_SCAN_END);
            sortUnique(scriptsToScanAfterFileScanFinished);
            for folderToScan in scriptsToScanAfterFileScanFinished do pendingUsageScans.append(folderToScan);
            if length(scriptsToScanAfterFileScanFinished)>0 then pendingUsageScans.append(MARKER_USAGE_SCAN_END);
            setLength(scriptsToScanAfterFileScanFinished,0);
          end
          else scanFolder(canonicalFileName(folderToScan));
          sleepBetweenRunsMillis:=0;
        end else begin
          sleepBetweenRunsMillis+=1;
          sleep(sleepBetweenRunsMillis);
        end;
      end
      else begin
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
    package^.load(lu_forCodeAssistance,globals^,recycler,C_EMPTY_STRING_ARRAY,callAndIdInfos);
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

  begin
    enterCriticalSection(messageCs);
    try
      loc.line:=CaretY;
      loc.column:=1;
      loc.package:=package;
      result:=(fullLine<>info.fullLine) or (CaretX<>info.CaretX);
      if result then begin
        lexer.create(fullLine,loc,package);
        enhanced:=lexer.getEnhancedTokens(callAndIdInfos);
        info:=enhanced.getTokenAtIndex(CaretX).toInfo;
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
    result:=listScriptIds(extractFilePath(package^.getPath));
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

PROCEDURE postUsageScan(CONST folderToScan: string);
  begin
    pendingUsageScans.append(folderToScan);
  end;

FUNCTION getAssistanceResponseSync(CONST editorMeta:P_codeProvider):P_codeAssistanceResponse;
  VAR request:P_codeAssistanceRequest;
      recycler:P_recycler;
  begin
    new(request,createWithProvider(editorMeta));
    recycler:=newRecycler;
    result:=request^.execute(recycler);
    freeRecycler(recycler);
    dispose(request,destroy);
  end;

PROCEDURE T_codeAssistanceResponse.doCreateHtmlData;
  FUNCTION getHtmlText: string;
    VAR highlighter:TMnhInputSyn;
        SynExporterHTML: TSynExporterHTML;
        outputStream:TMemoryStream;
        size:longint;
        content:TStringList;
        s:string;
    begin
      highlighter:=TMnhInputSyn.create(nil);
      updateHighlightingData(highlighter.highlightingData);
      SynExporterHTML:=TSynExporterHTML.create(nil);
      SynExporterHTML.title:=extractFileName(package^.getPath);
      SynExporterHTML.highlighter:=highlighter;
      content:=TStringList.create;
      for s in package^.getCodeProvider^.getLines do content.add(s);
      SynExporterHTML.ExportAll(content);
      outputStream:=TMemoryStream.create();
      SynExporterHTML.saveToStream(outputStream);
      SynExporterHTML.free;
      size:=outputStream.size;
      outputStream.Seek(0,soFromBeginning);
      initialize(result);
      setLength(result,size);
      outputStream.ReadBuffer(result[1],size);
      outputStream.free;
      highlighter.free;
      content.free;
    end;

  FUNCTION relatedBuiltinFunctionNames: T_arrayOfString;
    VAR b:P_builtinFunctionMetaData;
    begin
      result:=C_EMPTY_STRING_ARRAY;
      for b in callAndIdInfos^.calledBuiltinFunctions do append(result,b^.qualifiedId);
    end;

  VAR demoIndex:longint;
      id:string;
  begin
    enterCriticalSection(messageCs);
    try
      demoIndex:=addDemoFile(extractFileName(package^.getPath),getHtmlText);
      for id in relatedBuiltinFunctionNames do linkDemoToDoc(demoIndex,id);
    finally
      leaveCriticalSection(messageCs);
    end;
  end;

PROCEDURE ensureDefaultFiles(Application: Tapplication; bar: TProgressBar; CONST overwriteExisting: boolean; CONST createHtmlDat: boolean);
  {$i res_defaultFiles.inc}
  VAR baseDir:string;
  PROCEDURE ensureFile(CONST index:longint);
    VAR fileName:string;
        fileContent:string;
    begin
      fileName:=baseDir+DEFAULT_FILES[index,0];
      if overwriteExisting or not(fileExists(fileName)) then begin
        fileContent:=decompressString(DEFAULT_FILES[index,1]);
        writeFile(fileName,fileContent);
      end;
    end;

  PROCEDURE createHtmlData(CONST index:longint);
    VAR response:P_codeAssistanceResponse;
    begin
      response:=getAssistanceResponseSync(newFileCodeProvider(baseDir+DEFAULT_FILES[index,0]));
      response^.doCreateHtmlData;
      disposeMessage(response);
    end;

  VAR i:longint;
  begin
    baseDir:=configDir;
    if bar<>nil then begin
      bar.caption:='Creating packages and demos';
      Application.ProcessMessages;
    end;
    for i:=0 to length(DEFAULT_FILES)-1 do begin
      ensureFile(i);
      if bar<>nil then begin bar.position:=bar.position+1; Application.ProcessMessages; end;
    end;
    if createHtmlDat then for i:=0 to length(DEFAULT_FILES)-1 do begin
      createHtmlData(i);
      if bar<>nil then begin bar.position:=bar.position+1; Application.ProcessMessages; end;
    end else if bar<>nil then bar.position:=bar.position+length(DEFAULT_FILES);
  end;

CONSTRUCTOR T_codeAssistanceRequest.createWithProvider(CONST editorMeta:P_codeProvider);
  begin
    scriptPath:=editorMeta^.getPath;
    provider:=editorMeta;
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

CONSTRUCTOR T_codeAssistanceResponse.create(CONST package_:P_package; CONST messages:T_storedMessages; CONST stateHash_:T_hashInt; CONST callAndIdInfos_:P_callAndIdInfos);
  VAR m:P_storedMessage;
      level:longint;
  begin
    inherited create(mt_ide_codeAssistanceResponse);
    package:=package_;
    responseStateHash:=stateHash_;
    callAndIdInfos:=callAndIdInfos_;
    callAndIdInfos^.cleanup;
    setLength(localErrors,0);
    setLength(externalErrors,0);
    for level:=4 downto 1 do for m in messages do
    if C_messageTypeMeta[m^.messageType].level=level then begin
      if m^.getLocation.fileName=package_^.getPath
      then begin
        setLength(localErrors,length(localErrors)+1);
        localErrors[length(localErrors)-1]:=m^.rereferenced;
      end else begin
        setLength(externalErrors,length(externalErrors)+1);
        externalErrors[length(externalErrors)-1]:=m^.rereferenced;
      end;
    end;
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

PROCEDURE T_codeAssistanceResponse.getErrorHints(VAR edit:TSynEdit; OUT hasErrors, hasWarnings: boolean; CONST appendMode:boolean=false);
  VAR messageFormatter:T_guiFormatter;

  PROCEDURE addErrors(CONST list:T_storedMessages);
    VAR s:string;
        m:P_storedMessage;
    begin
      for m in list do begin
        hasErrors  :=hasErrors   or (C_messageTypeMeta[m^.messageType].level>2);
        hasWarnings:=hasWarnings or (C_messageTypeMeta[m^.messageType].level=2);
        for s in messageFormatter.formatMessage(m) do edit.lines.append(s);
      end;
    end;

  begin
    messageFormatter.create(false);
    enterCriticalSection(messageCs);
    try
      if not(appendMode) then begin
        edit.clearAll;
        edit.lines.clear;
      end;
      messageFormatter.preferredLineLength:=edit.charsInWindow;
      messageFormatter.wrapEcho:=true;
      hasErrors:=false;
      hasWarnings:=false;
      addErrors(localErrors);
      addErrors(externalErrors);
    finally
      leaveCriticalSection(messageCs);
      messageFormatter.destroy;
    end;
  end;

PROCEDURE finalizeCodeAssistance;
  VAR i:longint;
      response:P_codeAssistanceResponse;
      folderForUsageScan:ansistring;
      assistanceRequest:P_codeAssistanceRequest;
  begin
    enterCriticalSection(codeAssistanceCs);
    shuttingDown:=true;
    for i:=0 to 99 do if codeAssistantIsRunning then begin
      codeAssistanceThread.Terminate;
      leaveCriticalSection(codeAssistanceCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(codeAssistanceCs);
    end;
    while pendingUsageScans.canGetNext(folderForUsageScan) do folderForUsageScan:='';
    while pendingRequests.canGetNext(assistanceRequest) do dispose(assistanceRequest,destroy);

    if not(codeAssistantIsRunning)
    then doneCriticalSection(codeAssistanceCs);
    pendingRequests.destroy;
    pendingUsageScans.destroy;
    while preparedResponses.canGetNext(response) do disposeMessage(response);
    preparedResponses.destroy;
    isFinalized:=true;
  end;

INITIALIZATION
  initialize(codeAssistanceCs);
  initCriticalSection(codeAssistanceCs);
  initialize(scriptUsage);
  with scriptUsage do begin
    setLength(dat,0);
    setLength(foldersScanned,0);
    scanning:=false;
  end;
  pendingRequests.create;
  pendingUsageScans.create;
  preparedResponses.create;

FINALIZATION
  if not(isFinalized) then finalizeCodeAssistance;

end.

