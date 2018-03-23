UNIT mnh_packages;
INTERFACE
USES //basic classes
     sysutils,typinfo, FileUtil, Classes,
     //my utilities:
     myGenerics, myStringUtil,
     //MNH:
     mnh_constants, mnh_basicTypes,
     mnh_litVar, mnh_fileWrappers, mnh_out_adapters,
     mnh_caches,
     mnh_tokens, mnh_contexts,
     mnh_profiling,
     {$ifdef fullVersion}
       mnh_doc,
       mnh_funcs_plot,
       mnh_settings,
       mnh_html,
       tokenStack,
       mnh_debuggingVar,
     {$else}
       mySys,
     {$endif}
     mnh_funcs,
     mnh_funcs_mnh,   mnh_funcs_types, mnh_funcs_math,  mnh_funcs_strings,
     mnh_funcs_list,  mnh_funcs_system, mnh_funcs_files,
     mnh_funcs_format,
     mnh_funcs_regex, mnh_funcs_xml, mnh_funcs_ipc, mnh_funcs_server,
     mnh_builtinGenerators,
     mnh_patterns,
     mnh_subrules,
     mnh_rule,
     mnh_tokenArray;

{$define include_interface}
TYPE
  P_package=^T_package;
  T_ruleMap=specialize G_stringKeyMap<P_rule>;
  T_packageLoadUsecase=(lu_NONE,lu_beingLoaded,lu_forImport,lu_forCallingMain,lu_forDirectExecution,lu_forCodeAssistance);

  T_packageReference=object
    id,path:ansistring;
    pack:P_package;
    locationOfDeclaration:T_tokenLocation;
    {Creates a package reference with a given packId (or fails reporting via adapters if no package with the given ID can be found)}
    CONSTRUCTOR create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
    {Creates a package reference with a specific path (or fails reporting via adapters if the file does not exist)}
    CONSTRUCTOR createWithSpecifiedPath(CONST path_:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
    {$ifdef fullVersion}
    {Returns true, if the package has <idOrPath> either as ID or as path; Used for code assistance only}
    FUNCTION hasIdOrPath(CONST idOrPath:string; CONST importingPackage:P_objectWithPath):boolean;
    {$endif}
    DESTRUCTOR destroy;
    {Loads the specified package using the appropriate T_packageLoadUsecase}
    PROCEDURE loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext; CONST forCodeAssistance:boolean);
  end;

  T_packageList=array of P_package;
  T_package=object(T_abstractPackage)
    private
      {The main package; for imported packages, this points to the importing packages; for main packages this is =@self}
      mainPackage:P_package;
      secondaryPackages:T_packageList;
      extendedPackages:array of P_extendedPackage;
      runAfter:array of P_subruleExpression;

      isPlainScript:boolean;
      commentOnPlainMain:ansistring;

      packageRules,importedRules:T_ruleMap;
      packageUses:array of T_packageReference;
      readyForUsecase:T_packageLoadUsecase;
      {$ifdef fullVersion}
      pseudoCallees:T_packageProfilingCalls;
      anyCalled:boolean;
      {$endif}

      PROCEDURE resolveRuleIds(CONST adapters:P_adapters);
      PROCEDURE clear(CONST includeSecondaries:boolean);
      FUNCTION ensureRuleId(CONST ruleId:T_idString; CONST modifiers:T_modifierSet; CONST ruleDeclarationStart:T_tokenLocation; VAR adapters:T_adapters):P_rule;
      PROCEDURE writeDataStores(VAR adapters:T_adapters; CONST recurse:boolean);
      FUNCTION inspect(CONST includeRulePointer:boolean; VAR context:T_threadContext):P_mapLiteral;
      PROCEDURE interpret(VAR statement:T_enhancedStatement; CONST usecase:T_packageLoadUsecase; VAR context:T_threadContext{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos=nil{$endif});
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider; CONST mainPackage_:P_package);
      FUNCTION getSecondaryPackageById(CONST id:ansistring):ansistring;
      PROCEDURE load(usecase:T_packageLoadUsecase; VAR context:T_threadContext; CONST mainParameters:T_arrayOfString{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;
      FUNCTION getHelpOnMain:ansistring;
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
      PROCEDURE resolveId(VAR token:T_token; CONST adaptersOrNil:P_adapters); virtual;
      FUNCTION isMain:boolean;
      FUNCTION getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
      PROCEDURE finalize(VAR context:T_threadContext);

      {$ifdef fullVersion}
      FUNCTION usedPackages:T_packageList;
      FUNCTION declaredRules:T_ruleList;
      PROCEDURE updateLists(VAR userDefinedRules:T_setOfString);
      PROCEDURE complainAboutUnused(VAR adapters:T_adapters);
      PROCEDURE reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
      PROCEDURE interpretInPackage(CONST input:T_arrayOfString; VAR context:T_threadContext);
      FUNCTION getImport(CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION getExtended(CONST idOrPath:string):P_abstractPackage; virtual;
      {$endif}
    end;

  {$ifdef fullVersion}

  P_codeAssistanceData=^T_codeAssistanceData;
  T_codeAssistanceData=object
    private
      package:P_package;
      packageIsValid:boolean;
      localErrors,externalErrors:T_storedMessages;
      stateHash:T_hashInt;
      userRules:T_setOfString;

      editorForUpdate:P_codeProvider;
      checkPending,currentlyProcessing:boolean;
      cs:TRTLCriticalSection;
      localIdInfos:P_localIdInfos;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION getErrorHints(OUT hasErrors,hasWarnings:boolean; CONST lengthLimit:longint): T_arrayOfString;
      FUNCTION isUserRule(CONST id: string): boolean;
      FUNCTION isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
      FUNCTION isLocalId(CONST id: string; CONST lineIndex, colIdx: longint): boolean;
      FUNCTION updateCompletionList(VAR wordsInEditor:T_setOfString; CONST lineIndex, colIdx: longint):boolean;
      PROCEDURE explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo); virtual;
      FUNCTION renameIdentifierInLine(CONST location:T_searchTokenLocation; CONST newId:string; VAR lineText:ansistring; CONST CaretY:longint):boolean;
      FUNCTION getStateHash:T_hashInt;
      PROCEDURE triggerUpdate(CONST editor:P_codeProvider); virtual;
      PROCEDURE synchronousUpdate(CONST editor:P_codeProvider); virtual;
      FUNCTION resolveImport(CONST id:string):string; virtual;
      FUNCTION getImportablePackages:T_arrayOfString; virtual;

      FUNCTION getPackageLocking:P_package;
      PROCEDURE releaseLock;
  end;

  P_blankCodeAssistanceData=^T_blankCodeAssistanceData;
  T_blankCodeAssistanceData=object(T_codeAssistanceData)
    CONSTRUCTOR createBlank;
    PROCEDURE explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo); virtual;
    PROCEDURE triggerUpdate(CONST editor:P_codeProvider); virtual;
    PROCEDURE synchronousUpdate(CONST editor:P_codeProvider); virtual;
    FUNCTION resolveImport(CONST id:string):string; virtual;
    FUNCTION getImportablePackages:T_arrayOfString; virtual;
  end;

  P_postEvaluationData=^T_postEvaluationData;
  T_postEvaluationData=object
    private
      editor:P_codeProvider;
      packageForPostEval:P_package;
      adapters:P_adapters;
      checkPending,currentlyProcessing:boolean;
      cs:TRTLCriticalSection;
    public
      CONSTRUCTOR create(CONST quickEdit:P_codeProvider; CONST quickAdapters:P_adapters);
      DESTRUCTOR destroy;
      PROCEDURE triggerUpdate(CONST package:P_package);
      PROCEDURE ensureStop;
      FUNCTION processing:boolean;
  end;
  {$endif}

  P_sandbox=^T_sandbox;
  T_sandbox=object
    private
      evaluationContext:T_evaluationContext;
      adapters:T_adapters;
      collector:T_collectingOutAdapter;
      busy:boolean;
      package:T_package;
      cs:TRTLCriticalSection;
      {$ifdef fullVersion}
      PROCEDURE updateCodeAssistanceData(CONST provider:P_codeProvider; VAR caData:T_codeAssistanceData);
      {$endif}
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION execute(CONST input:T_arrayOfString; CONST randomSeed:dword=4294967295):T_storedMessages;
      FUNCTION loadForCodeAssistance(VAR packageToInspect:T_package):T_storedMessages;
      FUNCTION runScript(CONST filenameOrId:string; CONST mainParameters:T_arrayOfString; CONST locationForWarning:T_tokenLocation; CONST callerAdapters:P_adapters; CONST connectLevel:byte; CONST enforceDeterminism:boolean):P_literal;
      {$ifdef fullVersion}
      PROCEDURE runInstallScript;
      PROCEDURE runUninstallScript;
      {$endif}
  end;

FUNCTION packageFromCode(CONST code:T_arrayOfString; CONST nameOrPseudoName:string):P_package;
FUNCTION sandbox:P_sandbox;
{$undef include_interface}
IMPLEMENTATION
VAR sandboxes:array of P_sandbox;
    sbLock:TRTLCriticalSection;
PROCEDURE setupSandboxes;
  begin
    initCriticalSection(sbLock);
    enterCriticalSection(sbLock);
    setLength(sandboxes,0);
    leaveCriticalSection(sbLock);
  end;

PROCEDURE doneSandboxes;
  VAR i:longint;
  begin
    enterCriticalSection(sbLock);
    for i:=0 to length(sandboxes)-1 do dispose(sandboxes[i],destroy);
    setLength(sandboxes,0);
    leaveCriticalSection(sbLock);
    doneCriticalSection(sbLock);
  end;

FUNCTION sandbox:P_sandbox;
  VAR i:longint;
      firstTry:boolean=false;
  begin
    result:=nil;
    enterCriticalSection(sbLock);
    repeat
      firstTry:=not(firstTry);
      for i:=0 to length(sandboxes)-1 do if result=nil then begin
        enterCriticalSection(sandboxes[i]^.cs);
        if not(sandboxes[i]^.busy) then begin
          sandboxes[i]^.busy:=true;
          result:=sandboxes[i];
        end;
        leaveCriticalSection(sandboxes[i]^.cs);
      end;
      if (result=nil) and firstTry then begin
        leaveCriticalSection(sbLock);
        sleep(1);
        ThreadSwitch;
        enterCriticalSection(sbLock);
      end;
    until (result<>nil) or not(firstTry);
    if result=nil then begin
      i:=length(sandboxes);
      setLength(sandboxes,i+1);
      new(sandboxes[i],create);
      result:=sandboxes[i];
      enterCriticalSection(result^.cs);
      result^.busy:=true;
      leaveCriticalSection(result^.cs);
    end;
    leaveCriticalSection(sbLock);
  end;

FUNCTION isTypeToType(CONST id:T_idString):T_idString;
  begin
    if (length(id)>=3) and (id[1]='i') and (id[2]='s')
    then result:=copy(id,3,length(id)-2)
    else result:='';
  end;

FUNCTION packageFromCode(CONST code:T_arrayOfString; CONST nameOrPseudoName:string):P_package;
  begin
    new(result,create(newVirtualFileCodeProvider(nameOrPseudoName,code),nil));
  end;

{$ifdef fullVersion}
CONSTRUCTOR T_postEvaluationData.create(CONST quickEdit: P_codeProvider; CONST quickAdapters: P_adapters);
  begin
    editor:=quickEdit;
    adapters:=quickAdapters;
    packageForPostEval:=nil;
    checkPending:=false;
    currentlyProcessing:=false;
    initCriticalSection(cs);
  end;

DESTRUCTOR T_postEvaluationData.destroy;
  begin
    ensureStop;
    doneCriticalSection(cs);
  end;

FUNCTION postEvalThread(p:pointer):ptrint;
  VAR evaluationContext:T_evaluationContext;
      sleepCount:longint=0;
  begin
    with P_postEvaluationData(p)^ do begin
      enterCriticalSection(cs);
      currentlyProcessing:=true;
      evaluationContext.create(adapters);
      while sleepCount<100 do begin
        while checkPending do begin
          sleepCount:=0;
          checkPending:=false;
          leaveCriticalSection(cs);
          adapters^.clearAll();
          evaluationContext.resetForEvaluation(packageForPostEval,ect_normal,C_EMPTY_STRING_ARRAY);
          adapters^.clearPrint;
          packageForPostEval^.interpretInPackage(editor^.getLines,evaluationContext.threadContext^);
          enterCriticalSection(cs);
        end;
        leaveCriticalSection(cs);
        sleep(10);
        inc(sleepCount);
        enterCriticalSection(cs);
      end;
      evaluationContext.destroy;
      currentlyProcessing:=false;
      leaveCriticalSection(cs);
    end;
    result:=0;
  end;

PROCEDURE T_postEvaluationData.triggerUpdate(CONST package: P_package);
  begin
    enterCriticalSection(cs);
    packageForPostEval:=package;
    if currentlyProcessing then begin
      checkPending:=true;
      leaveCriticalSection(cs);
      exit;
    end;
    checkPending:=true;
    currentlyProcessing:=true;
    beginThread(@postEvalThread,@self);
    leaveCriticalSection(cs);
  end;

PROCEDURE T_postEvaluationData.ensureStop;
  begin
    enterCriticalSection(cs);
    while currentlyProcessing do begin
      leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      enterCriticalSection(cs);
    end;
    leaveCriticalSection(cs);
  end;

FUNCTION T_postEvaluationData.processing:boolean;
  begin
    enterCriticalSection(cs);
    result:=currentlyProcessing;
    leaveCriticalSection(cs);
  end;

CONSTRUCTOR T_codeAssistanceData.create;
  begin
    package:=nil;
    packageIsValid:=false;
    setLength(localErrors,0);
    setLength(externalErrors,0);
    stateHash:=0;
    userRules.create;
    editorForUpdate:=nil;
    currentlyProcessing:=false;
    checkPending:=false;
    new(localIdInfos,create);
    initCriticalSection(cs);
  end;

CONSTRUCTOR T_blankCodeAssistanceData.createBlank;
  begin inherited create; end;

DESTRUCTOR T_codeAssistanceData.destroy;
  begin
    enterCriticalSection(cs);
    while currentlyProcessing do begin
      leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      enterCriticalSection(cs);
    end;
    if package<>nil then dispose(package,destroy);
    userRules.destroy;
    if localIdInfos<>nil then dispose(localIdInfos,destroy);
    leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

FUNCTION T_codeAssistanceData.getErrorHints(OUT hasErrors, hasWarnings: boolean; CONST lengthLimit: longint): T_arrayOfString;
  VAR k:longint;
  PROCEDURE resultAppend(CONST s:string);
    begin
      if k>=length(result) then setLength(result,round(k*1.1+1));
      result[k]:=s;
      inc(k);
    end;

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
    VAR i:longint;
        s,head,rest:string;
    begin
      for i:=0 to length(list)-1 do with list[i] do begin
        hasErrors  :=hasErrors   or (C_messageTypeMeta[messageType].level> 2);
        hasWarnings:=hasWarnings or (C_messageTypeMeta[messageType].level<=2);
        for s in messageText do begin
          head:=ansistring(location);
          if length(head)>=lengthLimit-3 then begin
            resultAppend(C_messageClassMeta[C_messageTypeMeta[messageType].mClass].guiMarker+head);
            head:='. '+s;
          end else head:=head+' '+s;
          repeat
            splitAtSpace(head,rest,3,lengthLimit);
            resultAppend(C_messageClassMeta[C_messageTypeMeta[messageType].mClass].guiMarker+head);
            head:='. '+rest;
          until rest='';
        end;
      end;
    end;

  begin
    enterCriticalSection(cs);
    hasErrors:=false;
    hasWarnings:=false;
    setLength(result,length(localErrors)+length(externalErrors));
    k:=0;
    addErrors(localErrors);
    addErrors(externalErrors);
    setLength(result,k);
    leaveCriticalSection(cs);
  end;

FUNCTION T_codeAssistanceData.isUserRule(CONST id: string): boolean;
  begin
    enterCriticalSection(cs);
    result:=userRules.contains(id);
    leaveCriticalSection(cs);
  end;

FUNCTION T_codeAssistanceData.isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
  VAR e:T_storedMessage;
  begin
    enterCriticalSection(cs);
    result:=0;
    for e in localErrors do with e do
    if (result=0) and (lineIndex=location.line-1) and ((location.column<0) or (tokenStart<=location.column-1) and (tokenEnd>location.column-1)) then begin
      if C_messageTypeMeta[messageType].level>2 then result:=2
      else if result<1 then result:=1;
    end;
    leaveCriticalSection(cs);
  end;

FUNCTION T_codeAssistanceData.isLocalId(CONST id: string; CONST lineIndex, colIdx: longint): boolean;
  VAR dummyLocation:T_tokenLocation;
  begin
    result:=localIdInfos^.localTypeOf(id,lineIndex,colIdx,dummyLocation)=tt_blockLocalVariable;
  end;

FUNCTION T_codeAssistanceData.updateCompletionList(VAR wordsInEditor:T_setOfString; CONST lineIndex, colIdx: longint):boolean;
  VAR s:string;
      wc:longint;
  begin
    enterCriticalSection(cs);
    wc:=wordsInEditor.size;
    wordsInEditor.put(userRules);
    for s in userRules.values do if pos(ID_QUALIFY_CHARACTER,s)<=0 then wordsInEditor.put(ID_QUALIFY_CHARACTER+s);
    for s in localIdInfos^.allLocalIdsAt(lineIndex,colIdx) do wordsInEditor.put(s);
    result:=wordsInEditor.size>wc;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_blankCodeAssistanceData.explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo); begin end;
PROCEDURE T_codeAssistanceData.explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo);
  VAR lexer:T_lexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  begin
    if (CaretY=info.startLoc.line) and (CaretX>=info.startLoc.column) and (CaretX<info.endLoc.column) then exit;
    enterCriticalSection(cs);
    loc.line:=CaretY;
    loc.column:=1;
    loc.package:=package;

    lexer.create(fullLine,loc,package);
    enhanced:=lexer.getEnhancedTokens(localIdInfos);
    info:=enhanced.getTokenAtIndex(CaretX).toInfo;
    enhanced.destroy;
    lexer.destroy;
    leaveCriticalSection(cs);
  end;

FUNCTION T_codeAssistanceData.renameIdentifierInLine(CONST location:T_searchTokenLocation; CONST newId:string; VAR lineText:ansistring; CONST CaretY:longint):boolean;
  VAR lexer:T_lexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  begin
    enterCriticalSection(cs);
    loc.line:=CaretY;
    loc.column:=1;
    loc.package:=package;
    lexer.create(lineText,loc,package);
    enhanced:=lexer.getEnhancedTokens(localIdInfos);
    result:=enhanced.renameInLine(lineText,location,newId);
    enhanced.destroy;
    lexer.destroy;
    leaveCriticalSection(cs);
  end;

FUNCTION T_codeAssistanceData.getStateHash:T_hashInt;
  begin
    enterCriticalSection(cs); result:=stateHash; leaveCriticalSection(cs);
  end;

FUNCTION T_blankCodeAssistanceData.resolveImport(CONST id:string):string; begin result:=''; end;
FUNCTION T_codeAssistanceData.resolveImport(CONST id:string):string;
  begin
    enterCriticalSection(cs);
    if package=nil then result:=''
                   else result:=package^.getSecondaryPackageById(id);
    leaveCriticalSection(cs);
  end;

FUNCTION T_blankCodeAssistanceData.getImportablePackages:T_arrayOfString; begin result:=C_EMPTY_STRING_ARRAY; end;
FUNCTION T_codeAssistanceData.getImportablePackages:T_arrayOfString;
  begin
    result:=listScriptIds(extractFilePath(package^.getPath));
  end;

FUNCTION T_codeAssistanceData.getPackageLocking:P_package;
  begin
    enterCriticalSection(cs);
    currentlyProcessing:=true;
    result:=package;
  end;

PROCEDURE T_codeAssistanceData.releaseLock;
  begin
    currentlyProcessing:=false;
    leaveCriticalSection(cs);
  end;

FUNCTION codeAssistantCheckThread(p:pointer):ptrint;
  begin
    sandbox^.updateCodeAssistanceData(P_codeAssistanceData(p)^.editorForUpdate,P_codeAssistanceData(p)^);
    result:=0;
  end;

PROCEDURE T_blankCodeAssistanceData.triggerUpdate(CONST editor:P_codeProvider); begin end;
PROCEDURE T_codeAssistanceData.triggerUpdate(CONST editor:P_codeProvider);
  begin
    enterCriticalSection(cs);
    if (editor=nil) then begin
      if not(checkPending) then begin
        leaveCriticalSection(cs);
        exit;
      end;
    end else editorForUpdate:=editor;
    if currentlyProcessing then begin
      checkPending:=true;
      leaveCriticalSection(cs);
      exit;
    end;
    checkPending:=false;
    currentlyProcessing:=true;
    beginThread(@codeAssistantCheckThread,@self);
    leaveCriticalSection(cs);
  end;

PROCEDURE T_blankCodeAssistanceData.synchronousUpdate(CONST editor:P_codeProvider); begin end;
PROCEDURE T_codeAssistanceData.synchronousUpdate(CONST editor:P_codeProvider);
  begin
    if editor=nil then exit;
    enterCriticalSection(cs);
    while currentlyProcessing do begin
      leaveCriticalSection(cs);
      ThreadSwitch; sleep(1);
      enterCriticalSection(cs);
    end;
    editorForUpdate:=editor;
    sandbox^.updateCodeAssistanceData(editor,self);
    leaveCriticalSection(cs);
  end;

PROCEDURE T_sandbox.updateCodeAssistanceData(CONST provider:P_codeProvider; VAR caData:T_codeAssistanceData);
  VAR newPackage:P_package;
  PROCEDURE updateErrors;
    VAR i:longint;
    begin
      setLength(caData.localErrors,0);
      setLength(caData.externalErrors,0);
      with collector do
      for i:=0 to length(storedMessages)-1 do with storedMessages[i] do
      if C_messageTypeMeta[messageType].level>=1 then begin
        if location.fileName=newPackage^.getPath
        then begin
          setLength(caData.localErrors,length(caData.localErrors)+1);
          caData.localErrors[length(caData.localErrors)-1]:=storedMessages[i];
        end else begin
          setLength(caData.externalErrors,length(caData.externalErrors)+1);
          caData.externalErrors[length(caData.externalErrors)-1]:=storedMessages[i];
        end;
      end;
    end;
  VAR newLocalIdInfos:P_localIdInfos;
  begin
    enterCriticalSection(cs); busy:=true; leaveCriticalSection(cs);
    {$ifdef debugMode}
    writeln(stdErr,'        DEBUG: updateCodeAssistanceData ',provider^.getPath,' - reset');
    {$endif}
    new(newPackage,create(provider,nil));
    adapters.clearAll;
    evaluationContext.resetForEvaluation(newPackage,ect_silent,C_EMPTY_STRING_ARRAY);
    new(newLocalIdInfos,create);
    {$ifdef debugMode}
    writeln(stdErr,'        DEBUG: updateCodeAssistanceData ',provider^.getPath,' - load');
    {$endif}
    newPackage^.load(lu_forCodeAssistance,evaluationContext.threadContext^,C_EMPTY_STRING_ARRAY,newLocalIdInfos);
    enterCriticalSection(caData.cs);
    {$ifdef debugMode}
    writeln(stdErr,'        DEBUG: updateCodeAssistanceData ',provider^.getPath,' - copy package');
    {$endif}
    if caData.package     <>nil then dispose(caData.package     ,destroy); caData.package     :=newPackage;
    if caData.localIdInfos<>nil then dispose(caData.localIdInfos,destroy); caData.localIdInfos:=newLocalIdInfos;
    caData.packageIsValid:=evaluationContext.adapters^.noErrors;
    caData.currentlyProcessing:=false;
    caData.package^.updateLists(caData.userRules);
    updateErrors;
    caData.stateHash:=caData.package^.getCodeState;
    leaveCriticalSection(caData.cs);
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
    {$ifdef debugMode}
    writeln(stdErr,'        DEBUG: updateCodeAssistanceData ',provider^.getPath,' - done');
    {$endif}
  end;
{$endif}

CONSTRUCTOR T_sandbox.create;
  begin
    initCriticalSection(cs);
    collector.create(at_unknown,C_collectAllOutputBehavior);
    adapters.create;
    adapters.addOutAdapter(@collector,false);
    evaluationContext.create(@adapters);
    package.create(newVirtualFileCodeProvider('?',C_EMPTY_STRING_ARRAY),nil);
    busy:=false;
  end;

DESTRUCTOR T_sandbox.destroy;
  begin
    enterCriticalSection(cs);
    package.destroy;
    evaluationContext.destroy;
    adapters.destroy;
    collector.destroy;
    leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

FUNCTION T_sandbox.execute(CONST input: T_arrayOfString; CONST randomSeed: dword): T_storedMessages;
  begin
    enterCriticalSection(cs); busy:=true; leaveCriticalSection(cs);
    adapters.clearAll;
    package.replaceCodeProvider(newVirtualFileCodeProvider('?',input));
    evaluationContext.resetForEvaluation({$ifdef fullVersion}@package,{$endif}ect_silent,C_EMPTY_STRING_ARRAY);
    if randomSeed<>4294967295 then evaluationContext.prng.resetSeed(randomSeed);
    package.load(lu_forDirectExecution,evaluationContext.threadContext^,C_EMPTY_STRING_ARRAY);
    result:=collector.storedMessages;
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
  end;

FUNCTION T_sandbox.loadForCodeAssistance(VAR packageToInspect:T_package):T_storedMessages;
  begin
    enterCriticalSection(cs); busy:=true; leaveCriticalSection(cs);
    adapters.clearAll;
    evaluationContext.resetForEvaluation({$ifdef fullVersion}@package,{$endif}ect_silent,C_EMPTY_STRING_ARRAY);
    packageToInspect.load(lu_forCodeAssistance,evaluationContext.threadContext^,C_EMPTY_STRING_ARRAY);
    result:=collector.storedMessages;
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
  end;

FUNCTION T_sandbox.runScript(CONST filenameOrId:string; CONST mainParameters:T_arrayOfString; CONST locationForWarning:T_tokenLocation; CONST callerAdapters:P_adapters; CONST connectLevel:byte; CONST enforceDeterminism:boolean):P_literal;
  VAR fileName:string='';
  begin
    enterCriticalSection(cs); busy:=true; leaveCriticalSection(cs);
    if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
    then fileName:=expandFileName(filenameOrId)
    else fileName:=locateSource(extractFilePath(locationForWarning.package^.getPath),filenameOrId);
    if (fileName='') or not(fileExists(fileName)) then begin
      callerAdapters^.raiseWarning('Cannot find script with id or path "'+filenameOrId+'"',locationForWarning);
      exit(nil);
    end;
    adapters.clearAll({$ifdef fullVersion}true{$endif});
    callerAdapters^.addSubAdapters(@adapters);
    if connectLevel>0 then adapters.addOutAdapter(callerAdapters^.getConnector(connectLevel>=1,connectLevel>=2,connectLevel>=3),true);
    if enforceDeterminism then evaluationContext.prng.resetSeed(0);
    package.replaceCodeProvider(newFileCodeProvider(filenameOrId));
    try
      evaluationContext.resetForEvaluation({$ifdef fullVersion}@package,{$endif}ect_silent,mainParameters);
      package.load(lu_forCallingMain,evaluationContext.threadContext^,mainParameters);
      evaluationContext.afterEvaluation;
    finally
      result:=messagesToLiteralForSandbox(collector.storedMessages);
      enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
    end;
  end;
{$ifdef fullVersion}
PROCEDURE T_sandbox.runInstallScript;
  {$i res_ensureAssoc.inc}
  begin execute(ensureAssoc_mnh); end;

PROCEDURE T_sandbox.runUninstallScript;
  {$i res_removeAssoc.inc}
  begin execute(removeAssoc_mnh); end;

PROCEDURE demoCallToHtml(CONST input:T_arrayOfString; OUT textOut,htmlOut,usedBuiltinIDs:T_arrayOfString);
  VAR messages:T_storedMessages;
      i:longint;
      tmp:ansistring;
      raw:T_rawTokenArray;
      tok:T_rawToken;
  begin
    messages:=sandbox^.execute(input);
    setLength(textOut,0);
    setLength(htmlOut,0);
    setLength(usedBuiltinIDs,0);
    for i:=0 to length(input)-1 do begin
      tmp:=trim(input[i]);
      raw:=rawTokenizeCallback(tmp);
      for tok in raw do if tok.tokType=tt_intrinsicRule then appendIfNew(usedBuiltinIDs,tok.txt);
      if startsWith(tmp,COMMENT_PREFIX) then begin
        append(htmlOut,            StringOfChar(' ',length(C_messageTypeMeta[mt_echo_input].prefix)+1)+toHtmlCode(raw));
        append(textOut,ECHO_MARKER+StringOfChar(' ',length(C_messageTypeMeta[mt_echo_input].prefix)+1)+tmp);
      end else begin
        append(htmlOut,            C_messageTypeMeta[mt_echo_input].prefix+' '+toHtmlCode(raw));
        append(textOut,ECHO_MARKER+C_messageTypeMeta[mt_echo_input].prefix+' '+tmp);
      end;
    end;
    for i:=0 to length(messages)-1 do with messages[i] do begin
      case messageType of
        mt_printline:  for tmp in messageText do append(htmlOut,escapeHtml(tmp));
        mt_echo_input, mt_echo_declaration, mt_el1_note: begin end;
        mt_echo_output: for tmp in messageText do append(htmlOut,C_messageTypeMeta[messageType].prefix+' '+toHtmlCode(escapeHtml(tmp)));
        {$ifdef fullVersion}
        mt_plotFileCreated: begin
          tmp:=extractFileName(messageText[0]);
          CopyFile(messageText[0],getHtmlRoot+DirectorySeparator+tmp);
          append(htmlOut,'Image created: '+imageTag(tmp));
        end;
        {$endif}
        else for tmp in messageText do append(htmlOut,span(C_messageClassMeta[C_messageTypeMeta[messageType].mClass].htmlSpan,C_messageTypeMeta[messageType].prefix+' '+escapeHtml(tmp)));
      end;
      if messageType<>mt_echo_input then
        for tmp in messageText do append(textOut,C_messageClassMeta[C_messageTypeMeta[messageType].mClass].guiMarker+C_messageTypeMeta[messageType].prefix+' '+tmp);
    end;
  end;
{$endif}

{$define include_implementation}
{$include mnh_funcs.inc}

PROCEDURE T_packageReference.loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext; CONST forCodeAssistance:boolean);
  CONST usecase:array[false..true] of T_packageLoadUsecase = (lu_forImport,lu_forCodeAssistance);
  VAR i:longint;
  begin
    with containingPackage^.mainPackage^ do begin
      for i:=0 to length(secondaryPackages)-1 do
        if secondaryPackages[i]^.getCodeProvider^.id = id then begin
          if  (secondaryPackages[i]^.readyForUsecase<>lu_NONE) and
              (secondaryPackages[i]^.codeChanged)
          then secondaryPackages[i]^.readyForUsecase:=lu_NONE;
          if secondaryPackages[i]^.readyForUsecase<>lu_beingLoaded then begin
            if secondaryPackages[i]^.readyForUsecase<>usecase[forCodeAssistance] then
            secondaryPackages[i]^.load(usecase[forCodeAssistance],context,C_EMPTY_STRING_ARRAY);
            pack:=secondaryPackages[i];
            exit;
          end else begin
            context.adapters^.raiseError('Cyclic package dependencies encountered; already loading "'+id+'"',tokenLocation);
            exit;
          end;
        end;
      new(pack,create(newFileCodeProvider(path),containingPackage^.mainPackage));
      setLength(secondaryPackages,length(secondaryPackages)+1);
      secondaryPackages[length(secondaryPackages)-1]:=pack;
      pack^.load(usecase[forCodeAssistance],context,C_EMPTY_STRING_ARRAY);
    end;
  end;

CONSTRUCTOR T_packageReference.create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
  begin
    locationOfDeclaration:=tokenLocation;
    id:=packId;
    path:=locateSource(extractFilePath(root),id);
    if adapters<>nil then begin
      if (path='')
      then adapters^.raiseError('Cannot locate package for id "'+id+'"',tokenLocation);
    end;
    pack:=nil;
  end;

CONSTRUCTOR T_packageReference.createWithSpecifiedPath(CONST path_:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
  begin
    locationOfDeclaration:=tokenLocation;
    path:=expandFileName(extractFilePath(tokenLocation.package^.getPath)+path_);
    id:=filenameToPackageId(path_);
    if not(fileExists(path)) and fileExists(path_) then path:=path_;
    if not(fileExists(path))
    then adapters^.raiseError('Cannot locate package "'+path+'"',tokenLocation);
    pack:=nil;
  end;

{$ifdef fullVersion}
FUNCTION T_packageReference.hasIdOrPath(CONST idOrPath:string; CONST importingPackage:P_objectWithPath):boolean;
  VAR p:string;
      dummy:longint;
  begin
    result:=false;
    if id  =idOrPath then exit(true);
    p:=unescapeString(idOrPath,1,dummy);
    if (              path =               p ) or
      (expandFileName(path)=expandFileName(p)) then exit(true);
    p:=extractFilePath(importingPackage^.getPath)+p;
    result:=(         path =               p ) or
      (expandFileName(path)=expandFileName(p));
  end;
{$endif}

DESTRUCTOR T_packageReference.destroy;
  begin
    id:='';
    path:='';
    pack:=nil;
  end;

PROCEDURE T_package.interpret(VAR statement:T_enhancedStatement; CONST usecase:T_packageLoadUsecase; VAR context:T_threadContext{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos=nil{$endif});
  VAR extendsLevel:byte=0;
      profile:boolean=false;

  PROCEDURE interpretIncludeClause;
    VAR locationForErrorFeedback:T_tokenLocation;
        {$ifdef fullVersion}
        clauseEnd:T_tokenLocation;
        {$endif}
        newId:string;
        first:P_token;
        helperUse:T_packageReference;
        lexer:T_lexer;
        importWrapper:P_extendedPackage;
        stmt:T_enhancedStatement;
    begin
      if statement.firstToken^.next=nil then begin
        context.adapters^.raiseError('Empty include clause',statement.firstToken^.location);
        context.recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      first:=statement.firstToken;
      locationForErrorFeedback:=first^.location;
      {$ifdef fullVersion}
      if (localIdInfos<>nil) and (first^.next<>nil) then begin
        clauseEnd:=first^.last^.location;
        inc(clauseEnd.column);
        localIdInfos^.add(first^.next^.singleTokenToString,first^.next^.location,clauseEnd,tt_include);
      end;
      {$endif}
      if extendsLevel>=32 then begin
        context.adapters^.raiseError('Max. extension level exceeded ',locationForErrorFeedback);
        exit;
      end;
      first:=context.recycler.disposeToken(first);
      if (first^.next=nil) and (first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule]) then begin
        newId:=first^.txt;
        helperUse.create(getCodeProvider^.getPath,first^.txt,first^.location,context.adapters);
      end else if (first^.next=nil) and (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_string) then begin
        newId:=P_stringLiteral(first^.data)^.value;
        helperUse.createWithSpecifiedPath(newId,first^.location,context.adapters);
      end else begin
        context.adapters^.raiseError('Invalid include clause ',locationForErrorFeedback);
        exit;
      end;
      context.recycler.cascadeDisposeToken(first);
      new(importWrapper,create(newFileCodeProvider(helperUse.path),@self));
      setLength(extendedPackages,length(extendedPackages)+1);
      extendedPackages[length(extendedPackages)-1]:=importWrapper;

      helperUse.destroy;
      lexer.create(importWrapper,@self);
      stmt:=lexer.getNextStatement(context.recycler,context.adapters^{$ifdef fullVersion},localIdInfos{$endif});
      inc(extendsLevel);
      while (context.adapters^.noErrors) and (stmt.firstToken<>nil) do begin
        interpret(stmt,usecase,context);
        stmt:=lexer.getNextStatement(context.recycler,context.adapters^{$ifdef fullVersion},localIdInfos{$endif});
      end;
      dec(extendsLevel);
      lexer.destroy;
    end;

  PROCEDURE interpretUseClause;
    VAR i,j:longint;
        locationForErrorFeedback:T_tokenLocation;
        {$ifdef fullVersion}
        clauseEnd:T_tokenLocation;
        {$endif}
        newId:string;
        first:P_token;

    PROCEDURE reloadAllPackages(CONST locationForErrorFeedback:T_tokenLocation);
      VAR i,j:longint;
          rulesSet:T_ruleMap.KEY_VALUE_LIST;
          dummyRule:P_rule;
      begin
        {$ifdef fullVersion}
        context.callStackPush(@self,pc_importing,pseudoCallees);
        {$endif}
        if profile then context.timeBaseComponent(pc_importing);
        for i:=0 to length(packageUses)-1 do packageUses[i].loadPackage(@self,locationForErrorFeedback,context,usecase=lu_forCodeAssistance);
        if profile then context.timeBaseComponent(pc_importing);
        {$ifdef fullVersion}
        context.callStackPop(nil);
        {$endif}
        i:=0;
        while i<length(packageUses) do begin
          if packageUses[i].pack=nil then begin
            for j:=i to length(packageUses)-2 do packageUses[j]:=packageUses[j+1];
            setLength(packageUses,length(packageUses)-1);
          end else inc(i);
        end;
        if context.adapters^.noErrors then for i:=length(packageUses)-1 downto 0 do begin
           rulesSet:=packageUses[i].pack^.packageRules.entrySet;
           for j:=0 to length(rulesSet)-1 do if rulesSet[j].value^.hasPublicSubrule then begin
             if not(importedRules.containsKey(rulesSet[j].key,dummyRule))
             then importedRules.put(rulesSet[j].key,rulesSet[j].value);
             importedRules.put(packageUses[i].id+ID_QUALIFY_CHARACTER+rulesSet[j].key,rulesSet[j].value);
           end;
        end;
      end;

    begin
      if statement.firstToken^.next=nil then begin
        context.adapters^.raiseError('Empty use clause',statement.firstToken^.location);
        context.recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      initialize(newId);
      first:=statement.firstToken;
      {$ifdef fullVersion}
      if (localIdInfos<>nil) then begin
        clauseEnd:=first^.last^.location;
        inc(clauseEnd.column);
      end;
      {$endif}
      locationForErrorFeedback:=first^.location;
      first:=context.recycler.disposeToken(first);
      while first<>nil do begin
        j:=-1;
        if first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule] then begin
          newId:=first^.txt;
          {$ifdef fullVersion}
          if localIdInfos<>nil then localIdInfos^.add(first^.txt,first^.location,clauseEnd,tt_use);
          if (newId=FORCE_GUI_PSEUDO_PACKAGE) then begin
            if not(gui_started) and (usecase<>lu_forCodeAssistance) then context.adapters^.logGuiNeeded;
          end else
          {$endif}
          begin
            j:=length(packageUses);
            setLength(packageUses,j+1);
            packageUses[j].create(getCodeProvider^.getPath,first^.txt,first^.location,context.adapters);
          end;
        end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_string) then begin
          {$ifdef fullVersion}
          if localIdInfos<>nil then localIdInfos^.add(first^.singleTokenToString,first^.location,clauseEnd,tt_use);
          {$endif}
          newId:=P_stringLiteral(first^.data)^.value;
          j:=length(packageUses);
          setLength(packageUses,j+1);
          packageUses[j].createWithSpecifiedPath(newId,first^.location,context.adapters);
        end else if first^.tokType<>tt_separatorComma then
          context.adapters^.raiseError('Cannot interpret use clause containing '+first^.singleTokenToString,first^.location);
        if (j>0) then for i:=0 to j-1 do
          if (expandFileName(packageUses[i].path)=expandFileName(packageUses[j].path))
                         or (packageUses[i].id   =               packageUses[j].id)
          then context.adapters^.raiseWarning('Duplicate import: '+newId,first^.location);
        first:=context.recycler.disposeToken(first);
      end;
      if not(context.adapters^.noErrors) then begin
        for i:=0 to length(packageUses)-1 do packageUses[i].destroy;
        setLength(packageUses,0);
      end;
      reloadAllPackages(locationForErrorFeedback);
    end;

  VAR assignmentToken:P_token;

  PROCEDURE parseRule;
    VAR p:P_token; //iterator
        ruleDeclarationStart:T_tokenLocation;
        {$ifdef fullVersion}
        ruleDeclarationEnd:T_tokenLocation;
        parameterId:T_patternElementLocation;
        {$endif}
        //rule meta data
        ruleModifiers:T_modifierSet=[];
        ruleId:T_idString='';
        evaluateBody:boolean;
        rulePattern:T_pattern;
        ruleBody:P_token;
        subRule:P_subruleExpression;
        ruleGroup:P_rule;
        inlineValue:P_literal;
    PROCEDURE addRuleToRunAfter(CONST ex:P_subruleExpression);
      begin
        if not(ex^.canApplyToNumberOfParameters(0)) then begin
          context.adapters^.raiseError('Attribute //@'+EXECUTE_AFTER_ATTRIBUTE+' is only allowed for nullary functions',ex^.getLocation);
          exit;
        end;
        setLength(runAfter,length(runAfter)+1);
        runAfter[length(runAfter)-1]:=ex;
        ex^.rereference;
      end;

    begin
      ruleDeclarationStart:=statement.firstToken^.location;
      evaluateBody:=(assignmentToken^.tokType=tt_assign);
      ruleBody:=assignmentToken^.next;
      assignmentToken^.next:=nil;
      //plausis:
      if (ruleBody=nil) then begin
        context.adapters^.raiseError('Missing function body after assignment/declaration token.',assignmentToken^.location);
        context.recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      while (statement.firstToken<>nil) and (statement.firstToken^.tokType=tt_modifier) and (C_modifierInfo[statement.firstToken^.getModifier].isRuleModifier) do begin
        include(ruleModifiers,statement.firstToken^.getModifier);
        statement.firstToken:=context.recycler.disposeToken(statement.firstToken);
      end;
      evaluateBody:=evaluateBody or (modifier_mutable in ruleModifiers);

      if not(statement.firstToken^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule, tt_customTypeRule]) then begin
        context.adapters^.raiseError('Declaration does not start with an identifier.',statement.firstToken^.location);
        context.recycler.cascadeDisposeToken(statement.firstToken);
        context.recycler.cascadeDisposeToken(ruleBody);
        exit;
      end;
      p:=statement.firstToken;
      while (p<>nil) and not(p^.tokType in [tt_assign,tt_declare]) do begin
        if (p^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule]) and isQualified(p^.txt) then begin
          context.adapters^.raiseError('Declaration head contains qualified ID.',p^.location);
          context.recycler.cascadeDisposeToken(statement.firstToken);
          context.recycler.cascadeDisposeToken(ruleBody);
          exit;
        end;
        p:=p^.next;
      end;
      //:plausis
      ruleId:=trim(statement.firstToken^.txt);
      statement.firstToken:=context.recycler.disposeToken(statement.firstToken);
      if not(statement.firstToken^.tokType in [tt_braceOpen,tt_assign,tt_declare])  then begin
        context.adapters^.raiseError('Invalid declaration head.',statement.firstToken^.location);
        context.recycler.cascadeDisposeToken(statement.firstToken);
        context.recycler.cascadeDisposeToken(ruleBody);
        exit;
      end;
      rulePattern.create;
      if statement.firstToken^.tokType=tt_braceOpen then rulePattern.parse(statement.firstToken,ruleDeclarationStart,context);
      {$ifdef fullVersion}
      if (localIdInfos<>nil) and (ruleBody<>nil) then begin
        ruleDeclarationEnd:=ruleBody^.last^.location;
        for parameterId in rulePattern.getNamedParameters do
          localIdInfos^.add(parameterId.id,parameterId.location,ruleDeclarationEnd,tt_parameterIdentifier);
      end;
      {$endif}

      if statement.firstToken<>nil then begin
        statement.firstToken:=context.recycler.disposeToken(statement.firstToken);
      end else begin
        context.adapters^.raiseError('Invalid declaration.',ruleDeclarationStart);
        context.recycler.cascadeDisposeToken(ruleBody);
        exit;
      end;
      rulePattern.toParameterIds(ruleBody);

      //[marker 1]
      if evaluateBody and (usecase<>lu_forCodeAssistance) and (context.adapters^.noErrors) then context.reduceExpression(ruleBody);

      if context.adapters^.noErrors then begin
        ruleGroup:=ensureRuleId(ruleId,ruleModifiers,ruleDeclarationStart,context.adapters^);
        if (context.adapters^.noErrors) and (ruleGroup^.getRuleType in C_mutableRuleTypes) and not(rulePattern.isValidMutablePattern)
        then context.adapters^.raiseError('Mutable rules are quasi variables and must therfore not accept any arguments',ruleDeclarationStart);
        if context.adapters^.noErrors then begin
          new(subRule,create(ruleGroup,rulePattern,ruleBody,ruleDeclarationStart,modifier_private in ruleModifiers,context));
          subRule^.metaData.setComment(join(statement.comments,C_lineBreakChar));
          subRule^.metaData.setAttributes(statement.attributes,subRule^.getLocation,context.adapters^);
          //in usecase lu_forCodeAssistance, the body might not be a literal because reduceExpression is not called at [marker 1]
          if (ruleGroup^.getRuleType in C_mutableRuleTypes)
          then begin
            if (usecase<>lu_forCodeAssistance)
            then begin
                   inlineValue:=subRule^.getInlineValue;
                   P_mutableRule(ruleGroup)^.setMutableValue(inlineValue,true);
                   inlineValue^.unreference;
                 end
            else P_mutableRule(ruleGroup)^.setMutableValue(newVoidLiteral,true);
            P_mutableRule(ruleGroup)^.metaData.setComment(join(statement.comments,C_lineBreakChar));
            P_mutableRule(ruleGroup)^.metaData.setAttributes(statement.attributes,subRule^.getLocation,context.adapters^);
            {$ifdef fullVersion}
            if P_mutableRule(ruleGroup)^.metaData.hasAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE) then begin
              if (modifier_private in ruleModifiers)
              then context.adapters^.raiseWarning('Attribute '+SUPPRESS_UNUSED_WARNING_ATTRIBUTE+' is ignored for private rules',ruleDeclarationStart)
              else ruleGroup^.setIdResolved;
            end;
            {$endif}
            dispose(subRule,destroy);
          end else begin
            if subRule^.metaData.hasAttribute(EXECUTE_AFTER_ATTRIBUTE) then addRuleToRunAfter(subRule);
            P_ruleWithSubrules(ruleGroup)^.addOrReplaceSubRule(subRule,context);
          end;
          statement.firstToken:=nil;
        end else begin
          context.recycler.cascadeDisposeToken(statement.firstToken);
          context.recycler.cascadeDisposeToken(ruleBody);
        end;
      end else begin
        context.recycler.cascadeDisposeToken(statement.firstToken);
        context.recycler.cascadeDisposeToken(ruleBody);
      end;
    end;

  PROCEDURE parseDataStore;
    VAR ruleModifiers:T_modifierSet=[];
        loc:T_tokenLocation;
    begin
      if (getCodeProvider^.isPseudoFile) then begin
        context.adapters^.raiseError('data stores require the package to be saved to a file.',statement.firstToken^.location);
        context.recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      while (statement.firstToken<>nil) and (statement.firstToken^.tokType=tt_modifier) and (C_modifierInfo[statement.firstToken^.getModifier].isRuleModifier) do begin
        include(ruleModifiers,statement.firstToken^.getModifier);
        loc:=statement.firstToken^.location;
        statement.firstToken:=context.recycler.disposeToken(statement.firstToken);
      end;
      if (statement.firstToken=nil) or not(statement.firstToken^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule]) or
         (statement.firstToken^.next<>nil) then begin
        if statement.firstToken<>nil then loc:=statement.firstToken^.location;
        context.adapters^.raiseError('Invalid datastore definition: '+tokensToString(statement.firstToken),loc);
        context.recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      ensureRuleId(statement.firstToken^.txt,
                   ruleModifiers,
                   statement.firstToken^.location,context.adapters^);
    end;

  FUNCTION getDeclarationOrAssignmentToken: P_token;
    VAR level:longint=0;
        t,newNext:P_token;
    begin
      t:=statement.firstToken;
      while (t<>nil) do begin
        if (t^.tokType=tt_iifElse) and (t^.next<>nil) and (t^.next^.tokType=tt_identifier) then begin
          resolveId(t^.next^,nil);
          {$ifdef fullVersion} if t^.next^.tokType=tt_customTypeRule then P_rule(t^.next^.data)^.setIdResolved; {$endif}
        end;
        if (t^.tokType=tt_iifElse) and (t^.next<>nil) and (t^.next^.tokType in [tt_customTypeRule,tt_type]) then begin
          newNext:=t^.next^.next;
          if t^.next^.tokType=tt_customTypeRule
          then t^.tokType:=tt_customTypeCheck
          else t^.tokType:=tt_typeCheck;
          t^.txt    :=t^.next^.txt;
          t^.data   :=t^.next^.data;
          context.recycler.disposeToken(t^.next);
          t^.next:=newNext;
        end;
        if t^.tokType      in C_openingBrackets then inc(level)
        else if t^.tokType in C_closingBrackets then dec(level);
        if (level=0) and (t^.tokType in [tt_assign,tt_declare]) then exit(t);
        t:=t^.next;
      end;
      result:=nil;
    end;

  begin
    profile:=context.adapters^.doShowTimingInfo and (usecase in [lu_forDirectExecution,lu_forCallingMain]);
    if statement.firstToken=nil then exit;
    if usecase=lu_forCodeAssistance then context.adapters^.resetErrorFlags;

    if not(context.adapters^.noErrors) then begin
      context.recycler.cascadeDisposeToken(statement.firstToken);
      exit;
    end;

    if (statement.firstToken^.tokType=tt_use) then begin
      interpretUseClause;
      exit;
    end;

    if (statement.firstToken^.tokType=tt_include) then begin
      interpretIncludeClause;
      exit;
    end;

    assignmentToken:=getDeclarationOrAssignmentToken;
    if (assignmentToken<>nil) then begin
      if not(se_alterPackageState in context.sideEffectWhitelist) then begin
        context.adapters^.raiseError('Rule declaration is not allowed here',assignmentToken^.location);
        context.recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      {$ifdef fullVersion}
      context.callStackPush(@self,pc_declaration,pseudoCallees);
      {$endif}
      if profile then context.timeBaseComponent(pc_declaration);
      if not ((assignmentToken^.next<>nil) and assignmentToken^.next^.areBracketsPlausible(context.adapters^)) then begin
        context.recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      predigest(assignmentToken,@self,context.recycler,context.adapters);
      if context.adapters^.doEchoDeclaration then context.adapters^.echoDeclaration(tokensToString(statement.firstToken)+';');
      parseRule;
      if profile then context.timeBaseComponent(pc_declaration);
      {$ifdef fullVersion}
      context.callStackPop(nil);
      {$endif}
    end else if statement.firstToken^.tokType=tt_modifier then begin
      if not(se_alterPackageState in context.sideEffectWhitelist) then begin
        context.adapters^.raiseError('Datastore declaration is not allowed here',statement.firstToken^.location);
        context.recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      {$ifdef fullVersion}
      context.callStackPush(@self,pc_declaration,pseudoCallees);
      {$endif}
      if profile then context.timeBaseComponent(pc_declaration);
      if context.adapters^.doEchoDeclaration then context.adapters^.echoDeclaration(tokensToString(statement.firstToken)+';');
      parseDataStore;
      if profile then context.timeBaseComponent(pc_declaration);
      {$ifdef fullVersion}
      context.callStackPop(nil);
      {$endif}
    end else if context.adapters^.noErrors then begin
      case usecase of
        lu_forDirectExecution:begin
          {$ifdef fullVersion}
          context.callStackPush(@self,pc_interpretation,pseudoCallees);
          {$endif}
          if profile then context.timeBaseComponent(pc_interpretation);
          if not ((statement.firstToken<>nil) and statement.firstToken^.areBracketsPlausible(context.adapters^)) then begin
            context.recycler.cascadeDisposeToken(statement.firstToken);
            exit;
          end;
          predigest(statement.firstToken,@self,context.recycler,context.adapters);
          if context.adapters^.doEchoInput then context.adapters^.echoInput(tokensToString(statement.firstToken)+';');
          context.reduceExpression(statement.firstToken);
          if profile then context.timeBaseComponent(pc_interpretation);
          {$ifdef fullVersion}
          context.callStackPop(nil);
          {$endif}
          if (statement.firstToken<>nil) and context.adapters^.doShowExpressionOut then begin
            {$ifdef fullVersion}
            if (statement.firstToken<>nil) and
               (statement.firstToken^.next=nil) and
               (statement.firstToken^.tokType=tt_literal) and
               (context.adapters^.preferredEchoLineLength>10) then begin
              context.adapters^.echoOutput(
                serializeToStringList(P_literal(statement.firstToken^.data),
                                      statement.firstToken^.location,
                                      nil,
                                      context.adapters^.preferredEchoLineLength));
            end else {$endif}
              context.adapters^.echoOutput(tokensToString(statement.firstToken));
          end;
        end;
        lu_forCodeAssistance: if (statement.firstToken<>nil) and statement.firstToken^.areBracketsPlausible(context.adapters^) then begin
          predigest(statement.firstToken,@self,context.recycler,context.adapters);
          resolveBuiltinIDs(statement.firstToken,context.adapters);
        end
        else context.adapters^.raiseNote('Skipping expression '+tokensToString(statement.firstToken,50),statement.firstToken^.location);
      end;
    end;
    if statement.firstToken<>nil then context.recycler.cascadeDisposeToken(statement.firstToken);
    statement.firstToken:=nil;
  end;

PROCEDURE T_package.load(usecase:T_packageLoadUsecase; VAR context:T_threadContext; CONST mainParameters:T_arrayOfString{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos{$endif});
  VAR profile:boolean=false;
  PROCEDURE executeMain;
    VAR mainRule:P_rule;
        parametersForMain:P_listLiteral=nil;
        t:P_token=nil;
        dummy:P_token=nil;
        i:longint;
        {$ifdef fullVersion}displayedHelp:boolean=false;{$endif}
    begin
      if not(readyForUsecase=lu_forCallingMain) or not(context.adapters^.noErrors) then exit;
      if not(packageRules.containsKey(MAIN_RULE_ID,mainRule)) then begin
        context.adapters^.raiseError('The specified package contains no main rule.',packageTokenLocation(@self));
      end else begin
        parametersForMain:=newListLiteral(length(mainParameters));
        for i:=0 to length(mainParameters)-1 do parametersForMain^.appendString(mainParameters[i]);

        {$ifdef fullVersion}
        context.callStackPush(@self,pc_interpretation,pseudoCallees);
        {$endif}
        if profile then context.timeBaseComponent(pc_interpretation);

        if mainRule^.replaces(tt_localUserRule,packageTokenLocation(@self),parametersForMain,t,dummy,@context)
        then context.reduceExpression(t)
        else if (length(mainParameters)=1) and (mainParameters[0]='-h') then begin
          writeln(getHelpOnMain);
          {$ifdef fullVersion}displayedHelp:=true;{$endif}
        end else context.raiseCannotApplyError('user defined rule '+mainRule^.getId,
                                               parametersForMain,
                                               mainRule^.getLocation,
                                               mainRule^.getCmdLineHelpText,true);
        if profile then context.timeBaseComponent(pc_interpretation);
        {$ifdef fullVersion}
        context.callStackPop(nil);
        {$endif}

        {$ifdef fullVersion}
        //error handling if main returns more than one token:------------------
        if not(displayedHelp) and ((t=nil) or (t^.next<>nil)) and context.adapters^.hasNeedGUIerror
          then context.adapters^.raiseNote('Evaluation requires GUI-startup. Re-evaluating.',packageTokenLocation(@self));
        //------------------:error handling if main returns more than one token
        {$endif}
        context.recycler.cascadeDisposeToken(t);
        disposeLiteral(parametersForMain);
        parametersForMain:=nil;
      end;
    end;

  {$ifdef fullVersion}
  PROCEDURE checkParameters;
    VAR rule:P_rule;
        pack:P_package;
    begin
      for pack in secondaryPackages do for rule in pack^.packageRules.valueSet do rule^.checkParameters(context);
      for rule in packageRules.valueSet do rule^.checkParameters(context);
    end;
  {$endif}

  VAR lexer:T_lexer;
      stmt :T_enhancedStatement;
      newCodeHash:T_hashInt;

  FUNCTION isPlainScriptStatement:boolean;
    begin
      result:=(stmt.firstToken<>nil) and
              (stmt.firstToken^.txt='plain') and
              (stmt.firstToken^.next<>nil) and
              (stmt.firstToken^.next^.txt='script') and
              (stmt.firstToken^.next^.next=nil);
    end;

  begin
    commentOnPlainMain:='Undocumented plain script';
    if usecase = lu_NONE        then raise Exception.create('Invalid usecase: lu_NONE');
    if usecase = lu_beingLoaded then raise Exception.create('Invalid usecase: lu_beingLoaded');
    if isMain then context.adapters^.clearErrors;
    profile:=context.adapters^.doShowTimingInfo and (usecase in [lu_forDirectExecution,lu_forCallingMain]);
    clear(false);
    readyForUsecase:=lu_beingLoaded;

    if profile then context.timeBaseComponent(pc_tokenizing);
    lexer.create(@self);
    newCodeHash:=getCodeProvider^.stateHash;
    if profile then context.timeBaseComponent(pc_tokenizing);
    stmt:=lexer.getNextStatement(context.recycler,context.adapters^{$ifdef fullVersion},localIdInfos{$endif});
    isPlainScript:=isPlainScriptStatement;
    if isPlainScript then begin
      case usecase of
        lu_forImport         : context.adapters^.raiseError('Cannot import package declared as "plain script"',stmt.firstToken^.location);
        lu_forCallingMain    : usecase:=lu_forDirectExecution;
      end;
      commentOnPlainMain:=join(stmt.comments,C_lineBreakChar);
      context.recycler.cascadeDisposeToken(stmt.firstToken);
      stmt:=lexer.getNextStatement(context.recycler,context.adapters^{$ifdef fullVersion},localIdInfos{$endif});
    end;
    if profile then context.timeBaseComponent(pc_tokenizing);

    while (context.adapters^.noErrors) and (stmt.firstToken<>nil) do begin
      interpret(stmt,usecase,context{$ifdef fullVersion},localIdInfos{$endif});
      if profile then context.timeBaseComponent(pc_tokenizing);
      stmt:=lexer.getNextStatement(context.recycler,context.adapters^{$ifdef fullVersion},localIdInfos{$endif});
      if profile then context.timeBaseComponent(pc_tokenizing);
    end;
    lexer.destroy;
    if usecase=lu_forCodeAssistance then begin
      readyForUsecase:=usecase;
      logReady(newCodeHash);
      {$ifdef fullVersion}
      if gui_started then begin
        resolveRuleIds(context.adapters);
        complainAboutUnused(context.adapters^);
        checkParameters;
      end;
      {$endif}
      exit;
    end;
    if context.adapters^.noErrors then begin
      readyForUsecase:=usecase;
      logReady(newCodeHash);
      if usecase=lu_forCallingMain then executeMain;
    end else readyForUsecase:=lu_NONE;
    if isMain and (usecase in [lu_forDirectExecution,lu_forCallingMain])
    then finalize(context);
  end;

PROCEDURE disposeRule(VAR rule:P_rule);
  begin
    dispose(rule,destroy);
  end;

CONSTRUCTOR T_package.create(CONST provider: P_codeProvider; CONST mainPackage_: P_package);
  begin
    inherited create(provider);
    mainPackage:=mainPackage_;
    if mainPackage=nil then mainPackage:=@self;
    setLength(secondaryPackages,0);
    setLength(extendedPackages,0);
    setLength(packageUses,0);
    setLength(runAfter,0);
    packageRules.create(@disposeRule);
    importedRules.create;
    {$ifdef fullVersion}
    pseudoCallees:=blankProfilingCalls;
    anyCalled:=false;
    {$endif}
  end;

PROCEDURE T_package.clear(CONST includeSecondaries: boolean);
  VAR i:longint;
  begin
    {$ifdef fullVersion}
    anyCalled:=false;
    {$endif}
    for i:=0 to length(runAfter)-1 do disposeLiteral(runAfter[i]);
    setLength(runAfter,0);
    if includeSecondaries then begin
      for i:=0 to length(secondaryPackages)-1 do dispose(secondaryPackages[i],destroy);
      setLength(secondaryPackages,0);
    end;
    for i:=0 to length(extendedPackages)-1 do dispose(extendedPackages[i],destroy);
    setLength(extendedPackages,0);
    for i:=0 to length(packageUses)-1 do packageUses[i].destroy; setLength(packageUses,0);
    packageRules.clear;
    importedRules.clear;
    readyForUsecase:=lu_NONE;
  end;

PROCEDURE T_package.writeDataStores(VAR adapters:T_adapters; CONST recurse:boolean);
  VAR rule:P_rule;
      i:longint;
  begin
    for rule in packageRules.valueSet do
      if rule^.getRuleType=rt_datastore
      then P_datastoreRule(rule)^.writeBack(adapters);
    if recurse then for i:=0 to length(packageUses)-1 do packageUses[i].pack^.writeDataStores(adapters,recurse);
  end;

PROCEDURE T_package.finalize(VAR context:T_threadContext);
  VAR ruleList:array of P_rule;
      i:longint;
  begin
    for i:=0 to length(packageUses)-1 do packageUses[i].pack^.finalize(context);
    for i:=0 to length(runAfter)-1 do runAfter[i]^.evaluate(packageTokenLocation(@self),@context,nil);
    mnh_funcs_server.onPackageFinalization(@self);
    mnh_funcs_ipc   .onPackageFinalization(@self);
    mnh_funcs_format.onPackageFinalization(@self);
    if isMain then begin
      context.getParent^.stopWorkers;
    end;
    context.adapters^.updateErrorlevel;
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do
      if ruleList[i]^.getRuleType=rt_datastore then P_datastoreRule(ruleList[i])^.writeBack(context.adapters^);
    setLength(ruleList,0);
  end;

DESTRUCTOR T_package.destroy;
  {$ifdef fullVersion}VAR c:T_profileCategory;{$endif}
  begin
    clear(true);
    packageRules.destroy;
    importedRules.destroy;
    {$ifdef fullVersion}
    for c in T_profileCategory do if pseudoCallees[c]<>nil then dispose(pseudoCallees[c],destroy);
    {$endif}
    inherited destroy;
  end;

FUNCTION T_package.ensureRuleId(CONST ruleId: T_idString;
  CONST modifiers: T_modifierSet; CONST ruleDeclarationStart: T_tokenLocation;
  VAR adapters: T_adapters): P_rule;
  VAR ruleType:T_ruleType=rt_normal;
      i:longint;
  PROCEDURE raiseModifierComplaint;
    VAR m:T_modifier;
        s:string='';
    begin
      for m:=low(T_modifier) to high(T_modifier) do if m in modifiers then s:=s+C_modifierInfo[m].name+' ';
      adapters.raiseError('Invalid combination of modifiers: '+s,ruleDeclarationStart);
    end;

  begin
    i:=0;
    while (i<length(C_validModifierCombinations)) and (C_validModifierCombinations[i].modifiers<>modifiers) do inc(i);
    if i<length(C_validModifierCombinations) then ruleType:=C_validModifierCombinations[i].ruleType
    else begin
      raiseModifierComplaint;
      exit(nil);
    end;
    if not(packageRules.containsKey(ruleId,result)) then begin
      if (ruleId=MAIN_RULE_ID) then begin
        if modifiers<>[] then begin
          adapters.raiseError('main rules must not have any modifiers',ruleDeclarationStart);
          exit;
        end;
      end;
      if (ruleType=rt_customTypeCheck) and not(ruleId[1] in ['A'..'Z']) then
        adapters.raiseWarning('Type rules should begin with an uppercase letter',ruleDeclarationStart);
      if startsWith(ruleId,'is') and packageRules.containsKey(isTypeToType(ruleId)) then
        adapters.raiseWarning('Rule '+ruleId+' hides implicit typecheck rule',ruleDeclarationStart);
      case ruleType of
        rt_memoized       : new(P_memoizedRule             (result),create(ruleId,ruleDeclarationStart));
        rt_customTypeCheck: new(P_typecheckRule            (result),create(ruleId,ruleDeclarationStart));
        rt_mutable        : new(P_mutableRule              (result),create(ruleId,ruleDeclarationStart,      modifier_private in modifiers));
        rt_datastore      : new(P_datastoreRule            (result),create(ruleId,ruleDeclarationStart,@self,modifier_private in modifiers,modifier_plain in modifiers));
        rt_synchronized   : new(P_protectedRuleWithSubrules(result),create(ruleId,ruleDeclarationStart));
        else                new(P_ruleWithSubrules         (result),create(ruleId,ruleDeclarationStart,ruleType));
      end;
      packageRules.put(ruleId,result);
      if intrinsicRuleMap.containsKey(ruleId) then adapters.raiseWarning('Hiding builtin rule "'+ruleId+'"!',ruleDeclarationStart);
    end else begin
      if (result^.getRuleType<>ruleType) and (ruleType<>rt_normal)
      then adapters.raiseError('Colliding modifiers! Rule '+ruleId+' is '+C_ruleTypeText[result^.getRuleType]+', redeclared as '+C_ruleTypeText[ruleType],ruleDeclarationStart)
      else if (ruleType in C_ruleTypesWithOnlyOneSubrule)
      then adapters.raiseError(C_ruleTypeText[ruleType]+'rules must have exactly one subrule',ruleDeclarationStart);
    end;
  end;

FUNCTION T_package.getSecondaryPackageById(CONST id: ansistring): ansistring;
  VAR i:longint;
  begin
    for i:=0 to length(secondaryPackages)-1 do if secondaryPackages[i]^.getId=id then exit(secondaryPackages[i]^.getPath);
    result:='';
  end;

PROCEDURE T_package.resolveRuleIds(CONST adapters: P_adapters);
  VAR rule:P_rule;
  begin
    for rule in packageRules.valueSet do rule^.resolveIds(adapters);
  end;

{$ifdef fullVersion}
PROCEDURE T_package.updateLists(VAR userDefinedRules: T_setOfString);
  FUNCTION typeToIsType(CONST id:T_idString):T_idString;
    begin
      result:=id;
      result[1]:=upCase(result[1]);
      result:='is'+result;
    end;

  VAR rule:P_rule;
  begin
    userDefinedRules.clear;
    for rule in packageRules.valueSet do begin
      userDefinedRules.put(rule^.getId);
      if rule^.getRuleType=rt_customTypeCheck then userDefinedRules.put(typeToIsType(rule^.getId));
    end;
    for rule in importedRules.valueSet do  begin
      userDefinedRules.put(rule^.getId);
      if rule^.getRuleType=rt_customTypeCheck then userDefinedRules.put(typeToIsType(rule^.getId));
    end;
  end;

PROCEDURE T_package.complainAboutUnused(VAR adapters: T_adapters);
  VAR rule:P_rule;
      import:T_packageReference;
  begin
    for rule in packageRules.valueSet do rule^.complainAboutUnused(adapters);
    for import in packageUses do if not(import.pack^.anyCalled) then
      adapters.raiseWarning('Unused import '+import.pack^.getId+' ('+import.pack^.getPath+')',import.locationOfDeclaration);
  end;
{$endif}

FUNCTION T_package.getHelpOnMain: ansistring;
  VAR mainRule:P_rule;
      docText:T_arrayOfString;
      i:longint;
  begin
    if isPlainScript then exit('Plain script: '+commentOnPlainMain);
    if not(packageRules.containsKey(MAIN_RULE_ID,mainRule))
    then exit('The package contains no main rule')
    else begin
      result:='Try one of the following:'+LineEnding;
      docText:=mainRule^.getCmdLineHelpText;
      for i:=0 to 1 do result:=result+LineEnding+docText[i];
      dropFirst(docText,2);
      result:=result+LineEnding+join(formatTabs(docText),LineEnding);
    end;
  end;

FUNCTION T_package.isImportedOrBuiltinPackage(CONST id: string): boolean;
  VAR ns:T_namespace;
      i:longint;
  begin
    for ns in T_namespace do if C_namespaceString[ns]=id then exit(true);
    for i:=0 to length(packageUses)-1 do if packageUses[i].id=id then exit(true);
    result:=false;
  end;

FUNCTION T_package.isMain: boolean; begin result:=(@self=mainPackage); end;
{$ifdef fullVersion}
PROCEDURE T_package.reportVariables(VAR variableReport: T_variableTreeEntryCategoryNode);
  PROCEDURE addRule(CONST rule:P_rule);
    VAR value:P_literal;
        reportId:string;
    begin
      if not(rule^.isReportable(value)) then exit;
      reportId:=filenameToPackageId(rule^.getLocation.package^.getPath)+'.'+rule^.getId;
      if (rule^.getRuleType=rt_datastore) and not(P_datastoreRule(rule)^.isInitialized) then reportId:=reportId+' (uninitialized)';
      variableReport.addEntry(reportId,value,true);
    end;

  VAR rule:P_rule;
  begin
    for rule in importedRules.valueSet do addRule(rule);
    for rule in packageRules.valueSet do addRule(rule);
  end;
{$endif}

FUNCTION T_package.inspect(CONST includeRulePointer:boolean; VAR context:T_threadContext):P_mapLiteral;
  FUNCTION usesList:P_listLiteral;
    VAR i:longint;
    begin
      result:=newListLiteral(length(packageUses));
      for i:=0 to length(packageUses)-1 do result^.append(
        newListLiteral^.appendString(packageUses[i].id)^
                       .appendString(packageUses[i].path),false);
    end;

  FUNCTION includeList:P_listLiteral;
    VAR i:longint;
    begin
      result:=newListLiteral(length(extendedPackages));
      for i:=0 to length(extendedPackages)-1 do result^.append(
        newListLiteral^.appendString(extendedPackages[i]^.getId)^
                       .appendString(extendedPackages[i]^.getPath),false);
    end;

  FUNCTION rulesList:P_mapLiteral;
    VAR allRules:array of P_rule;
        rule:P_rule;
    begin
      allRules:=packageRules.valueSet;
      result:=newMapLiteral();
      for rule in allRules do result^.put(rule^.getId,rule^.inspect(includeRulePointer,context),false);
    end;

  begin
    result:=newMapLiteral^.put('id'      ,getId)^
                          .put('path'    ,getPath)^
                          .put('source'  ,join(getCodeProvider^.getLines,C_lineBreakChar))^
                          .put('uses'    ,usesList,false)^
                          .put('includes',includeList,false)^
                          .put('declares',rulesList,false);
  end;

FUNCTION T_package.getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
  VAR rule:P_rule;
      subRule:P_subruleExpression;
      matchesAll:boolean;
      key:string;
  begin
    setLength(result,0);
    for rule in packageRules.valueSet do if (rule^.getRuleType in [rt_normal,rt_synchronized,rt_memoized,rt_customTypeCheck]) then
    for subRule in P_ruleWithSubrules(rule)^.getSubrules do begin
      matchesAll:=true;
      for key in attributeKeys do matchesAll:=matchesAll and subRule^.metaData.hasAttribute(key,caseSensitive);
      if matchesAll then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=subRule;
      end;
    end;
  end;

PROCEDURE T_package.resolveId(VAR token: T_token;
  CONST adaptersOrNil: P_adapters);
  VAR userRule:P_rule;
      intrinsicFuncPtr:P_intFuncCallback;
      ruleId:T_idString;
  PROCEDURE assignLocalRule(CONST tt:T_tokenType); inline;
    begin
      token.tokType:=tt;
      token.data:=userRule;
      {$ifdef fullVersion}
      userRule^.setIdResolved;
      {$endif}
    end;

  PROCEDURE assignImportedRule(CONST tt:T_tokenType); inline;
    begin
      token.tokType:=tt;
      token.data:=userRule;
      {$ifdef fullVersion}
      userRule^.setIdResolved;
      P_package(userRule^.getLocation.package)^.anyCalled:=true;
      {$endif}
    end;

  begin
    ruleId   :=token.txt;
    if packageRules.containsKey(ruleId,userRule) then begin
      if userRule^.getRuleType=rt_customTypeCheck
      then assignLocalRule(tt_customTypeRule)
      else assignLocalRule(tt_localUserRule );
      exit;
    end;
    if importedRules.containsKey(ruleId,userRule) then begin
      if userRule^.getRuleType=rt_customTypeCheck
      then assignImportedRule(tt_customTypeRule  )
      else assignImportedRule(tt_importedUserRule);
      exit;
    end;
    if intrinsicRuleMap.containsKey(ruleId,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRule;
      token.data:=intrinsicFuncPtr;
      exit;
    end;
    ruleId:=isTypeToType(ruleId);
    if ruleId<>'' then begin
      if packageRules.containsKey(ruleId,userRule) and (userRule^.getRuleType=rt_customTypeCheck) then begin
        assignLocalRule(tt_customTypeRule);
        exit;
      end;
      if importedRules.containsKey(ruleId,userRule) and (userRule^.getRuleType=rt_customTypeCheck) then begin
        assignImportedRule(tt_customTypeRule);
        exit;
      end;
    end;
    if adaptersOrNil<>nil then adaptersOrNil^.raiseError('Cannot resolve ID "'+token.txt+'"',token.location);
  end;

{$ifdef fullVersion}
FUNCTION T_package.usedPackages: T_packageList;
  VAR i:longint;
  begin
    setLength(result,length(packageUses));
    for i:=0 to length(result)-1 do result[i]:=packageUses[i].pack;
  end;

FUNCTION T_package.declaredRules: T_ruleList;
  VAR tmp:P_rule;
      i,j:longint;
  begin
    result:=packageRules.valueSet;
    for i:=1 to length(result)-1 do
    for j:=0 to i-1 do
    if result[i]^.getId<result[j]^.getId then begin
      tmp:=result[i]; result[i]:=result[j]; result[j]:=tmp;
    end;
  end;

PROCEDURE T_package.interpretInPackage(CONST input:T_arrayOfString; VAR context:T_threadContext);
  VAR lexer:T_lexer;
      stmt :T_enhancedStatement;
      oldSideEffects:T_sideEffects;
      needAfterEval:boolean=false;
  begin
    if not(readyForUsecase in [lu_forImport,lu_forCallingMain,lu_forDirectExecution]) or (codeChanged) then begin
      load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
      needAfterEval:=true;
    end;

    oldSideEffects:=context.setAllowedSideEffectsReturningPrevious(context.sideEffectWhitelist*
    [se_output,
     se_inputViaAsk,
     se_sound,
     se_sleep,
     se_readPackageState,
     se_alterContextState,
     se_alterPlotState,
     se_readFile,
     se_accessHttp,
     se_accessIpc]);
    lexer.create(input,packageTokenLocation(@self),@self);
    stmt:=lexer.getNextStatement(context.recycler,context.adapters^{$ifdef fullVersion},nil{$endif});
    while (context.adapters^.noErrors) and (stmt.firstToken<>nil) do begin
      interpret(stmt,lu_forDirectExecution,context);
      stmt:=lexer.getNextStatement(context.recycler,context.adapters^{$ifdef fullVersion},nil{$endif});
    end;
    lexer.destroy;
    context.setAllowedSideEffectsReturningPrevious(oldSideEffects);
    if needAfterEval then context.getParent^.afterEvaluation;
  end;

FUNCTION T_package.getImport(CONST idOrPath:string):P_abstractPackage;
  VAR ref:T_packageReference;
  begin
    for ref in packageUses do if ref.hasIdOrPath(idOrPath,@self) then exit(ref.pack);
    result:=nil;
  end;

FUNCTION T_package.getExtended(CONST idOrPath:string):P_abstractPackage;
  VAR e:P_extendedPackage;
      dummy:longint;
  begin
    for e in extendedPackages do if (e^.getId=idOrPath) or (e^.getPath=unescapeString(idOrPath,1,dummy)) then exit(e);
    result:=nil;
  end;

{$endif}

{$undef include_implementation}
INITIALIZATION
  setupSandboxes;
{$define include_initialization}
  //callbacks in doc
  {$ifdef fullVersion}
  demoCodeToHtmlCallback:=@demoCallToHtml;
  {$endif}
  {$include mnh_funcs.inc}
{$undef include_initialization}

FINALIZATION
  doneSandboxes;
{$define include_finalization}
{$include mnh_funcs.inc}
end.
