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
  tokenStack,
  mnh_messages,
  out_adapters,
  contexts,
  recyclers,
  packages,
  funcs;

TYPE
  T_highlightingData=object
    private
      highlightingCs:TRTLCriticalSection;
      warnLocations:array of record line,column:longint; isError:boolean; end;
      userRules    :T_setOfString;
      localIdInfos :T_localIdInfos;
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
  T_codeAssistanceResponse=object
    private
      responseCs:TRTLCriticalSection;
      referenceCount:longint;
      localErrors,externalErrors:T_storedMessages;
      localIdInfos:P_localIdInfos;
      functionCallInfos:P_functionCallInfos;
      responseStateHash:T_hashInt;

      CONSTRUCTOR create(CONST package_:P_package; CONST messages:T_storedMessages; CONST stateHash_:T_hashInt; CONST localIdInfos_:P_localIdInfos; CONST functionCallInfos_:P_functionCallInfos);
      DESTRUCTOR destroy;

      FUNCTION  rereferenced:P_codeAssistanceResponse;
    public
      package:P_package;
      PROPERTY  stateHash:T_hashInt read responseStateHash;
      PROCEDURE getErrorHints(VAR edit:TSynEdit; OUT hasErrors, hasWarnings: boolean);
  end;

  P_codeAssistanceData=^T_codeAssistanceData;
  T_codeAssistanceData=object
    private
      //Input:
      provider               :P_codeProvider;
      additionalScriptsToScan:T_arrayOfString;
      //State:
      cs:TRTLCriticalSection;
      evaluating,querying,destroying:boolean;
      latestResponse:P_codeAssistanceResponse;
      paintedWithStateHash:T_hashInt;

      FUNCTION inputStateHash:T_hashInt;
      FUNCTION isAssistanceDataOutdated:boolean;
      PROCEDURE ensureResponse;
      FUNCTION  doCodeAssistanceSynchronouslyInCritialSection(VAR recycler:T_recycler; CONST givenGlobals:P_evaluationGlobals=nil; CONST givenAdapters:P_messagesErrorHolder=nil):boolean;
    public
      CONSTRUCTOR create(CONST editorMeta:P_codeProvider);
      DESTRUCTOR destroy;
      PROCEDURE setAddidionalScripts(CONST toScan:T_arrayOfString);
      //Highlighter related:
      FUNCTION  needRepaint:boolean;
      PROCEDURE updateHighlightingData(VAR highlightingData:T_highlightingData);
      //Rename/Help:
      FUNCTION  explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo):boolean;
      FUNCTION  renameIdentifierInLine(CONST location:T_searchTokenLocation; CONST oldId,newId:string; VAR lineText:ansistring; CONST CaretY:longint):boolean;
      FUNCTION  usedAndExtendedPackages:T_arrayOfString;
      //Completion related
      FUNCTION  getImportablePackages:T_arrayOfString;
      FUNCTION  updateCompletionList(VAR wordsInEditor:T_setOfString; CONST lineIndex, colIdx: longint):boolean;
      //Shebang related
      FUNCTION  getBuiltinRestrictions:T_specialFunctionRequirements;
      FUNCTION  isExecutablePackage:boolean;
      //Export
      FUNCTION getAssistanceResponseRereferenced:P_codeAssistanceResponse;
      //Quick-Evaluation
      FUNCTION assistanceStateHash:T_hashInt;
  end;

PROCEDURE disposeCodeAssistanceResponse(VAR r:P_codeAssistanceResponse);
PROCEDURE finalizeCodeAssistance;
PROCEDURE ensureCodeAssistanceThread;
PROCEDURE forceFullScan;
IMPLEMENTATION
USES sysutils,myStringUtil,commandLineParameters;

VAR codeAssistantIsRunning:boolean=false;
    codeAssistantThreadId :TThreadID;
    shuttingDown          :boolean=false;
    codeAssistanceData    :array of P_codeAssistanceData;
    codeAssistanceCs      :TRTLCriticalSection;

PROCEDURE disposeCodeAssistanceResponse(VAR r:P_codeAssistanceResponse);
  begin
    if (r<>nil) and (interlockedDecrement(r^.referenceCount)<=0) then dispose(r,destroy);
    r:=nil;
  end;

FUNCTION T_codeAssistanceData.inputStateHash: T_hashInt;
  VAR script:ansistring;
  begin
    {$Q-}{$R-}
    result:=provider^.stateHash*31+hashOfAnsiString(provider^.getPath)+127*length(additionalScriptsToScan);
    for script in additionalScriptsToScan do result:=result*31+hashOfAnsiString(script);
    {$Q+}{$R+}
  end;

FUNCTION T_codeAssistanceData.isAssistanceDataOutdated: boolean;
  begin
    enterCriticalSection(cs);
    try
      result:=(latestResponse=nil) or (latestResponse^.stateHash<>inputStateHash);
    finally
      leaveCriticalSection(cs);
    end;
  end;

VAR isFinalized:boolean=false;
DESTRUCTOR T_codeAssistanceData.destroy;
  VAR i,j:longint;
  begin
    enterCriticalSection(cs);
    destroying:=true;
    while evaluating or querying do begin
      leaveCriticalSection(cs);
      sleep(5);
      enterCriticalSection(cs);
    end;
    setLength(additionalScriptsToScan,0);
    if latestResponse<>nil then disposeCodeAssistanceResponse(latestResponse);
    if not(isFinalized) then begin
      enterCriticalSection(codeAssistanceCs);
      try
        j:=0;
        for i:=0 to length(codeAssistanceData)-1 do if codeAssistanceData[i]<>@self then begin
          codeAssistanceData[j]:=codeAssistanceData[i];
          inc(j);
        end;
        setLength(codeAssistanceData,j);
      finally
        leaveCriticalSection(codeAssistanceCs);
      end;
    end;
    leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

PROCEDURE T_codeAssistanceData.setAddidionalScripts(CONST toScan: T_arrayOfString);
  begin
    enterCriticalSection(cs);
    try
      additionalScriptsToScan:=toScan;
      ensureCodeAssistanceThread;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_codeAssistanceData.doCodeAssistanceSynchronouslyInCritialSection(VAR recycler: T_recycler; CONST givenGlobals: P_evaluationGlobals; CONST givenAdapters: P_messagesErrorHolder):boolean;
  VAR //temporary
      globals:P_evaluationGlobals;
      adapters:T_messagesErrorHolder;
      //output
      initialStateHash:T_hashInt;
      package:P_package;
      localIdInfos:P_localIdInfos;
      functionCallInfos:P_functionCallInfos;
      loadMessages:T_storedMessages;
      additionals:T_arrayOfString;

  PROCEDURE loadSecondaryPackage(CONST name:string);
    VAR user:T_package;
        secondaryCallInfos:T_functionCallInfos;
    begin
      {$ifdef debugMode}
      writeln('Loading ',name,' using ',provider^.getPath);
      {$endif}
      globals^.primaryContext.callDepth:=STACK_DEPTH_LIMIT-100;
      if globals^.primaryContext.callDepth<0 then globals^.primaryContext.callDepth:=0;
      user.create(newCodeProvider(name),nil);
      secondaryCallInfos.create;
      user.load(lu_forCodeAssistance,globals^,recycler,C_EMPTY_STRING_ARRAY,nil,@secondaryCallInfos);
      functionCallInfos^.includeUsages(@secondaryCallInfos);
      secondaryCallInfos.destroy;
      globals^.primaryContext.messages^.clearFlags;
      user.destroy;
    end;
  VAR script:ansistring;
      i:longint;
  begin
    initialStateHash:=inputStateHash;
    if ((latestResponse=nil) or (latestResponse^.stateHash<>initialStateHash)) then begin
      {$ifdef debugMode}
      writeln('Code assistance start: ',provider^.getPath);
      {$endif}
      new(package,create(provider,nil));
      setLength(additionals,length(additionalScriptsToScan));
      for i:=0 to length(additionalScriptsToScan)-1 do additionals[i]:=additionalScriptsToScan[i];
      evaluating:=true;
      leaveCriticalSection(cs);
      try
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
        new(localIdInfos,create);
        new(functionCallInfos,create);
        {$ifdef debugMode}
        writeln('Additional scripts to scan :',length(additionals),': ');
        for script in additionals do write(script,',');
        writeln;
        {$endif}
        for script in additionals do loadSecondaryPackage(script);
        globals^.primaryContext.messages^.clear();
        package^.load(lu_forCodeAssistance,globals^,recycler,C_EMPTY_STRING_ARRAY,localIdInfos,functionCallInfos);
        if givenGlobals<>nil then loadMessages:=givenAdapters^.storedMessages(true)
                             else loadMessages:=adapters      .storedMessages(true);
        globals^.afterEvaluation(recycler);
        {$ifdef debugMode}
        writeln('Code assistance end  : ',provider^.getPath);
        {$endif}
        enterCriticalSection(cs);
        try
          if latestResponse<>nil then disposeCodeAssistanceResponse(latestResponse);
          latestResponse:=nil;
          new(latestResponse,create(package,loadMessages,initialStateHash,localIdInfos,functionCallInfos));
        finally
          leaveCriticalSection(cs);
        end;
      finally
        evaluating:=false;
        if givenGlobals=nil then begin
          dispose(globals,destroy);
          adapters.destroy;
        end;
      end;
      result:=true;
    end else begin
      leaveCriticalSection(cs);
      result:=false;
    end;
  end;

PROCEDURE T_codeAssistanceData.ensureResponse;
  VAR recycler:T_recycler;
  begin
    if latestResponse<>nil then exit;
    if evaluating then begin
      while evaluating do begin
        leaveCriticalSection(cs);
        sleep(1);
        enterCriticalSection(cs);
      end;
    end else begin
      recycler.initRecycler;
      doCodeAssistanceSynchronouslyInCritialSection(recycler);
      recycler.cleanup;
    end;
  end;

FUNCTION T_codeAssistanceData.needRepaint: boolean;
  begin
    enterCriticalSection(cs);
    try
      if (latestResponse<>nil) and (latestResponse^.stateHash<>paintedWithStateHash) then begin
        paintedWithStateHash:=latestResponse^.stateHash;
        result:=true;
      end else result:=false;
    finally
      leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_codeAssistanceData.updateHighlightingData(VAR highlightingData: T_highlightingData);
  VAR k:longint;
      e:P_storedMessage;
  begin
    enterCriticalSection(cs);
    if destroying then begin
      leaveCriticalSection(cs);
      exit;
    end else querying:=true;
    try
      ensureResponse;
      enterCriticalSection(highlightingData.highlightingCs);
      try
        highlightingData.userRules.clear;
        latestResponse^.package^.ruleMap.updateLists(highlightingData.userRules,false);
        highlightingData.localIdInfos.copyFrom(latestResponse^.localIdInfos);
        setLength(highlightingData.warnLocations,length(latestResponse^.localErrors));
        k:=0;
        for e in latestResponse^.localErrors do begin
          if k>=length(highlightingData.warnLocations) then setLength(highlightingData.warnLocations,k+1);
          highlightingData.warnLocations[k].line   :=e^.getLocation.line;
          highlightingData.warnLocations[k].column :=e^.getLocation.column;
          highlightingData.warnLocations[k].isError:=C_messageTypeMeta[e^.messageType].level>2;
          inc(k);
        end;
        setLength(highlightingData.warnLocations,k);
      finally
        leaveCriticalSection(highlightingData.highlightingCs);
      end;
    finally
      querying:=false;
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_codeAssistanceData.explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo): boolean;
  VAR lexer:T_lexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  PROCEDURE appendUsageInfo;
    VAR ref:T_searchTokenLocations;
        r  :T_searchTokenLocation;
    begin
      if (info.tokenType in [tt_userRule,tt_customType,tt_globalVariable,tt_customTypeCheck]) then begin
        ref:=latestResponse^.functionCallInfos^.whoReferencesLocation(info.location);
        if length(ref)=0 then begin
          info.infoText+=C_lineBreakChar+C_lineBreakChar+'No reference found';
        end else begin
          info.infoText+=C_lineBreakChar+C_lineBreakChar+'Is referenced at';
          for r in ref do info.infoText+=C_lineBreakChar+string(r);
        end;
      end;
    end;

  begin
    enterCriticalSection(cs);
    if destroying then begin
      leaveCriticalSection(cs);
      exit;
    end else querying:=true;
    try
      ensureResponse;
      loc.line:=CaretY;
      loc.column:=1;
      loc.package:=latestResponse^.package;
      result:=(fullLine<>info.fullLine) or (CaretX<>info.CaretX);
      if result then begin
        lexer.create(fullLine,loc,latestResponse^.package);
        enhanced:=lexer.getEnhancedTokens(latestResponse^.localIdInfos);
        info:=enhanced.getTokenAtIndex(CaretX).toInfo;
        info.fullLine:=fullLine;
        info.CaretX:=CaretX;
        appendUsageInfo;
        enhanced.destroy;
        lexer.destroy;
      end;
    finally
      querying:=false;
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_codeAssistanceData.renameIdentifierInLine(CONST location: T_searchTokenLocation; CONST oldId, newId: string; VAR lineText: ansistring; CONST CaretY: longint): boolean;
  VAR lexer:T_lexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  begin
    enterCriticalSection(cs);
    if destroying then begin
      leaveCriticalSection(cs);
      exit(false);
    end else querying:=true;
    try
      ensureResponse;
      loc.line:=CaretY;
      loc.column:=1;
      loc.package:=latestResponse^.package;
      lexer.create(lineText,loc,latestResponse^.package);
      enhanced:=lexer.getEnhancedTokens(latestResponse^.localIdInfos);
      result:=enhanced.renameInLine(lineText,location,oldId,newId);
      enhanced.destroy;
      lexer.destroy;
    finally
      querying:=false;
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_codeAssistanceData.usedAndExtendedPackages: T_arrayOfString;
  begin
    enterCriticalSection(cs);
    if destroying then begin
      leaveCriticalSection(cs);
      exit(C_EMPTY_STRING_ARRAY);
    end else querying:=true;
    try
      ensureResponse;
      result:=latestResponse^.package^.usedAndExtendedPackages;
    finally
      querying:=false;
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_codeAssistanceData.getImportablePackages: T_arrayOfString;
  begin
    result:=listScriptIds(extractFilePath(provider^.getPath));
  end;

FUNCTION T_codeAssistanceData.updateCompletionList(VAR wordsInEditor: T_setOfString; CONST lineIndex, colIdx: longint): boolean;
  VAR s:string;
  begin
    enterCriticalSection(cs);
    if destroying then begin
      leaveCriticalSection(cs);
      exit(false);
    end else querying:=true;
    try
      ensureResponse;
      latestResponse^.package^.ruleMap.updateLists(wordsInEditor,true);
      for s in latestResponse^.localIdInfos^.allLocalIdsAt(lineIndex,colIdx) do wordsInEditor.put(s);
      result:=(latestResponse^.package^.ruleMap.size>0) or not(latestResponse^.localIdInfos^.isEmpty);
    finally
      querying:=false;
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_codeAssistanceData.getBuiltinRestrictions: T_specialFunctionRequirements;
  begin
    enterCriticalSection(cs);
    if destroying then begin
      leaveCriticalSection(cs);
      exit([]);
    end else querying:=true;
    try
      ensureResponse;
      result:=latestResponse^.functionCallInfos^.getBuiltinRestrictions;
    finally
      querying:=false;
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_codeAssistanceData.isExecutablePackage: boolean;
  begin
    enterCriticalSection(cs);
    if destroying then begin
      leaveCriticalSection(cs);
      exit(false);
    end else querying:=true;
    try
      ensureResponse;
      result:=latestResponse^.package^.isExecutable;
    finally
      querying:=false;
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_codeAssistanceData.getAssistanceResponseRereferenced: P_codeAssistanceResponse;
  begin
    enterCriticalSection(cs);
    try
      ensureResponse;
      result:=latestResponse^.rereferenced;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_codeAssistanceData.assistanceStateHash: T_hashInt;
  begin
    enterCriticalSection(cs);
    try
      if latestResponse=nil
      then result:=0
      else result:=latestResponse^.stateHash;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION codeAssistanceThread(p:pointer):ptrint;
  CONST STOP_AFTER_IDLE_LOOP_COUNT= 20;
        SLEEP_BETWEEN_RUNS_MILLIS =500;
  VAR globals:P_evaluationGlobals;
      adapters:T_messagesErrorHolder;

  FUNCTION isShutdownRequested:boolean;
    begin
      enterCriticalSection(codeAssistanceCs);
      result:=shuttingDown;
      leaveCriticalSection(codeAssistanceCs);
    end;

  VAR recycler:T_recycler;
      scanIndex:longint=0;
      runsWithoutUpdate:longint=0;
      anyScanned:boolean;
  begin
    {$ifdef debugMode}
    writeln(stdErr,'  codeAssistanceThread started');
    {$endif}
    //setup:
    adapters.createErrorHolder(nil,C_errorsAndWarnings+[mt_el1_note]);
    new(globals,create(@adapters));
    recycler.initRecycler;
    //:setup
    repeat
      enterCriticalSection(codeAssistanceCs);
      scanIndex:=0;
      anyScanned:=false;
      while scanIndex<length(codeAssistanceData) do begin
        if codeAssistanceData[scanIndex]^.isAssistanceDataOutdated then begin
          leaveCriticalSection(codeAssistanceCs);
          enterCriticalSection(codeAssistanceData[scanIndex]^.cs);
          if not(codeAssistanceData[scanIndex]^.destroying) and
             codeAssistanceData[scanIndex]^.doCodeAssistanceSynchronouslyInCritialSection(recycler,globals,@adapters)
          then anyScanned:=true;

          enterCriticalSection(codeAssistanceCs);
        end;
        inc(scanIndex);
      end;
      if anyScanned
      then runsWithoutUpdate:=0
      else inc(runsWithoutUpdate);
      leaveCriticalSection(codeAssistanceCs);
      sleep(SLEEP_BETWEEN_RUNS_MILLIS);
    until isShutdownRequested or (runsWithoutUpdate>=STOP_AFTER_IDLE_LOOP_COUNT);
    //shutdown:
    dispose(globals,destroy);
    adapters.destroy;
    enterCriticalSection(codeAssistanceCs);
    codeAssistantIsRunning:=false;
    leaveCriticalSection(codeAssistanceCs);
    result:=0;
    recycler.cleanup;
    //:shutdown
    {$ifdef debugMode}
    writeln(stdErr,'  codeAssistanceThread stopped');
    {$endif}
  end;

PROCEDURE ensureCodeAssistanceThread;
  begin
    if (gui_started<>ide) or isFinalized then exit;
    enterCriticalSection(codeAssistanceCs);
    try
      if not(codeAssistantIsRunning) then begin
        codeAssistantIsRunning:=true;
        codeAssistantThreadId:=beginThread(@codeAssistanceThread);
      end;
    finally
      leaveCriticalSection(codeAssistanceCs);
    end;
  end;

PROCEDURE forceFullScan;
  VAR ad:P_codeAssistanceData;
  begin
    enterCriticalSection(codeAssistanceCs);
    try
      for ad in codeAssistanceData do begin
        enterCriticalSection(ad^.cs);
        try
          if (ad^.latestResponse<>nil) then begin
            disposeCodeAssistanceResponse(ad^.latestResponse);
            ad^.latestResponse:=nil;
          end;
        finally
          leaveCriticalSection(ad^.cs);
        end;
      end;
      ensureCodeAssistanceThread;
    finally
      leaveCriticalSection(codeAssistanceCs);
    end;
  end;

CONSTRUCTOR T_codeAssistanceData.create(CONST editorMeta: P_codeProvider);
  begin
    provider:=editorMeta;
    setLength(additionalScriptsToScan,0);
    //State:
    initCriticalSection(cs);
    evaluating:=false;
    querying:=false;
    destroying:=false;
    latestResponse:=nil;
    paintedWithStateHash:=0;

    enterCriticalSection(codeAssistanceCs);
    setLength(codeAssistanceData,length(codeAssistanceData)+1);
    codeAssistanceData[length(codeAssistanceData)-1]:=@self;
    ensureCodeAssistanceThread;
    leaveCriticalSection(codeAssistanceCs);
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

CONSTRUCTOR T_codeAssistanceResponse.create(CONST package_:P_package; CONST messages:T_storedMessages; CONST stateHash_:T_hashInt; CONST localIdInfos_:P_localIdInfos; CONST functionCallInfos_:P_functionCallInfos);
  VAR m:P_storedMessage;
      level:longint;
  begin
    initCriticalSection(responseCs);
    enterCriticalSection(responseCs);
    try
      referenceCount:=1;
      package:=package_;
      responseStateHash:=stateHash_;
      localIdInfos:=localIdInfos_;
      functionCallInfos:=functionCallInfos_;
      functionCallInfos^.cleanup;
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
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

DESTRUCTOR T_codeAssistanceResponse.destroy;
  VAR m:P_storedMessage;
  begin
    enterCriticalSection(responseCs);
    try
      for m in localErrors do disposeMessage_(m);
      setLength(localErrors,0);
      for m in externalErrors do disposeMessage_(m);
      setLength(externalErrors,0);
      dispose(localIdInfos,destroy);
      dispose(functionCallInfos,destroy);
      dispose(package,destroy);
    finally
      leaveCriticalSection(responseCs);
      doneCriticalSection(responseCs);
    end;
  end;

PROCEDURE T_codeAssistanceResponse.getErrorHints(VAR edit:TSynEdit; OUT hasErrors, hasWarnings: boolean);
  VAR lengthLimit:longint;

  PROCEDURE splitAtSpace(VAR headOrAll:string; OUT tail:string; CONST dontSplitBefore,lengthLimit:longint);
    VAR splitIndex:longint;
    begin
      if length(headOrAll)<lengthLimit then begin
        tail:='';
        exit;
      end;
      splitIndex:=lengthLimit;
      while (splitIndex>dontSplitBefore) and (headOrAll[splitIndex]<>' ') do dec(splitIndex);
      if splitIndex<=dontSplitBefore then begin
        splitIndex:=lengthLimit;
        while (splitIndex<=length(headOrAll)) and (headOrAll[splitIndex]<>' ') do inc(splitIndex);
      end;
      tail:=copy(headOrAll,splitIndex,length(headOrAll));
      headOrAll:=copy(headOrAll,1,splitIndex-1);
    end;

  PROCEDURE addErrors(CONST list:T_storedMessages);
    VAR s,head,rest:string;
        m:P_storedMessage;
    begin
      for m in list do begin
        hasErrors  :=hasErrors   or (C_messageTypeMeta[m^.messageType].level> 2);
        hasWarnings:=hasWarnings or (C_messageTypeMeta[m^.messageType].level<=2);
        for s in m^.messageText do begin
          head:=ansistring(m^.getLocation);
          if length(head)>=lengthLimit-3 then begin
            edit.lines.append(C_messageTypeMeta[m^.messageType].guiMarker+head);
            head:='. '+s;
          end else head:=head+' '+s;
          repeat
            splitAtSpace(head,rest,3,lengthLimit);
            edit.lines.append(C_messageTypeMeta[m^.messageType].guiMarker+head);
            head:='. '+rest;
          until rest='';
        end;
      end;
    end;

  begin
    enterCriticalSection(responseCs);
    try
      edit.clearAll;
      edit.lines.clear;
      lengthLimit:=edit.charsInWindow;
      if lengthLimit<20 then lengthLimit:=20;
      hasErrors:=false;
      hasWarnings:=false;
      addErrors(localErrors);
      addErrors(externalErrors);
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.rereferenced:P_codeAssistanceResponse;
  begin
    enterCriticalSection(responseCs);
    try
      interLockedIncrement(referenceCount);
      result:=@self;
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

PROCEDURE finalizeCodeAssistance;
  VAR i:longint;
  begin
    enterCriticalSection(codeAssistanceCs);
    shuttingDown:=true;
    for i:=0 to 99 do if codeAssistantIsRunning then begin
      leaveCriticalSection(codeAssistanceCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(codeAssistanceCs);
    end;
    if codeAssistantIsRunning then KillThread(codeAssistantThreadId);
    doneCriticalSection(codeAssistanceCs);
    isFinalized:=true;
  end;

INITIALIZATION
  initialize(codeAssistanceCs);
  initCriticalSection(codeAssistanceCs);
  setLength(codeAssistanceData,0);

FINALIZATION
  if not(isFinalized) then finalizeCodeAssistance;

end.

