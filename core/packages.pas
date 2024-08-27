UNIT packages;
INTERFACE
USES //my utilities:
     myGenerics, myStringUtil,
     //MNH:
     mnh_constants, basicTypes,
     fileWrappers,
     mnh_messages,
     out_adapters,
     litVar,
     tokens, contexts,
     profiling,
     {$ifdef fullVersion}
       mnh_doc,
       funcs_plot,
       mnh_plotData,
       mnh_settings,
       debuggingVar,
       Forms,ComCtrls,
     {$else}
       mySys,
     {$endif}
     funcs,
     funcs_mnh,
     operators,
     funcs_format,
     funcs_ipc, funcs_server,
     patterns,
     subrules,
     rules,
     recyclers,
     datastores,
     tokenArray;

{$define include_interface}
TYPE
  P_package=^T_package;
  T_packageLoadUsecase=(lu_NONE,lu_beingLoaded,lu_forImport,lu_forCallingMain,lu_forDirectExecution,lu_forCodeAssistance,lu_forCodeAssistanceSecondary,lu_usageScan);

  T_packageReference=object
    id,path:ansistring;
    pack:P_package;
    locationOfDeclaration:T_tokenLocation;
    {$ifdef fullVersion}
    supressUnusedWarning:boolean;
    {$endif}
    {Creates a package reference with a given packId (or fails reporting via adapters if no package with the given ID can be found)}
    CONSTRUCTOR create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST messages:P_messages);
    {Creates a package reference with a specific path (or fails reporting via adapters if the file does not exist)}
    CONSTRUCTOR createWithSpecifiedPath(CONST path_:ansistring; CONST tokenLocation:T_tokenLocation; CONST messages:P_messages);
    {$ifdef fullVersion}
    {Returns true, if the package has <idOrPath> either as ID or as path; Used for code assistance only}
    FUNCTION hasIdOrPath(CONST idOrPath:string; CONST importingPackage:P_objectWithPath):boolean;
    {$endif}
    DESTRUCTOR destroy;
    {Loads the specified package using the appropriate T_packageLoadUsecase}
    PROCEDURE loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR globals:T_evaluationGlobals; CONST recycler:P_recycler; CONST parentUsecase:T_packageLoadUsecase);
  end;

  T_packageList=array of P_package;
  T_package=object(T_abstractPackage)
    private
      {The main package; for imported packages, this points to the importing packages; for main packages this is =@self}
      mainPackage:P_package;
      secondaryPackages:T_packageList;
      extendedPackages:array of P_extendedPackage;

      isPlainScript:boolean;
      commentOnPlainMain:ansistring;

      packageUses:array of T_packageReference;
      readyForUsecase:T_packageLoadUsecase;
      {$ifdef fullVersion}
      pseudoCallees:T_packageProfilingCalls;
      {$endif}
      FUNCTION writeDataStores(CONST messages:P_messages; CONST recurse:boolean; CONST literalRecycler:P_literalRecycler; VAR flush:T_datastoreFlush):boolean;
    public
      PROCEDURE interpret(VAR statement:T_enhancedStatement; CONST usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; CONST recycler:P_recycler{$ifdef fullVersion}; CONST callAndIdInfos:P_callAndIdInfos=nil{$endif});
      FUNCTION isMain:boolean; virtual;
      {$ifdef fullVersion}
      PROCEDURE complainAboutUnused(CONST messages:P_messages; CONST functionCallInfos:P_callAndIdInfos);
      {$endif}
    protected
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
    public
      ruleMap:T_ruleMap;
      PROCEDURE clear(CONST includeSecondaries:boolean);
      CONSTRUCTOR create(CONST provider:P_codeProvider; CONST mainPackage_:P_package);
      FUNCTION getSecondaryPackageById(CONST id:ansistring):ansistring;
      PROCEDURE load(usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; CONST recycler:P_recycler; CONST mainParameters:T_arrayOfString{$ifdef fullVersion}; CONST callAndIdInfos:P_callAndIdInfos=nil{$endif});

      DESTRUCTOR destroy; virtual;

      FUNCTION getHelpOnMain:ansistring;
      PROCEDURE finalize(CONST context:P_context; CONST recycler:P_recycler);
      FUNCTION resolveId(VAR token:T_token; CONST messagesOrNil:P_messages):boolean; virtual;
      FUNCTION resolveLocationForStackTrace(CONST location:T_tokenLocation):string; virtual;
      FUNCTION getTypeMap:T_typeMap; virtual;
      FUNCTION literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_recycler):string; virtual;
      FUNCTION inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; CONST functionCallInfos:P_callAndIdInfos{$endif}):P_mapLiteral; virtual;
      {$ifdef fullVersion}
      FUNCTION getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
      PROCEDURE reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
      FUNCTION declaredRules(CONST ruleSorting:T_ruleSorting):T_ruleMapEntries;
      FUNCTION usedPackages:T_packageList;
      FUNCTION getImport(CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION getExtended(CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION usedAndExtendedPackages:T_arrayOfString;
      FUNCTION isExecutable:boolean;
      {$endif}
    end;
CONST
  C_packageLoadUsecaseMeta:array[T_packageLoadUsecase] of record
    import_usecase:T_packageLoadUsecase;
    assistanceRun,
    loadUsedPacks ,
    interpretInputStatements,
    finalizeWorkers:boolean;
  end=(
 {lu_NONE                       }(import_usecase:lu_NONE                      ;assistanceRun:false;loadUsedPacks:true ;interpretInputStatements:false; finalizeWorkers:false),
 {lu_beingLoaded                }(import_usecase:lu_NONE                      ;assistanceRun:false;loadUsedPacks:true ;interpretInputStatements:false; finalizeWorkers:false),
 {lu_forImport                  }(import_usecase:lu_forImport                 ;assistanceRun:false;loadUsedPacks:true ;interpretInputStatements:false; finalizeWorkers:false),
 {lu_forCallingMain             }(import_usecase:lu_forImport                 ;assistanceRun:false;loadUsedPacks:true ;interpretInputStatements:false; finalizeWorkers:true ),
 {lu_forDirectExecution         }(import_usecase:lu_forImport                 ;assistanceRun:false;loadUsedPacks:true ;interpretInputStatements:true ; finalizeWorkers:true ),
 {lu_forCodeAssistance          }(import_usecase:lu_forCodeAssistance         ;assistanceRun:true ;loadUsedPacks:true ;interpretInputStatements:false; finalizeWorkers:false),
 {lu_forCodeAssistanceSecondary }(import_usecase:lu_forCodeAssistanceSecondary;assistanceRun:true ;loadUsedPacks:true ;interpretInputStatements:false; finalizeWorkers:false),
 {lu_usageScan                  }(import_usecase:lu_usageScan                 ;assistanceRun:true ;loadUsedPacks:false;interpretInputStatements:false; finalizeWorkers:false));

TYPE
  P_sandbox=^T_sandbox;
  T_sandbox=object
    private
      globals:T_evaluationGlobals;
      messages:T_messagesRedirector;
      busy:boolean;
      package:T_package;
      cs:TRTLCriticalSection;
      {$ifdef fullVersion}
      plotSystem:T_plotSystem;
      {$endif}
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION execute(CONST input:T_arrayOfString; CONST sideEffectWhitelist:T_sideEffects; CONST recycler:P_recycler; OUT ExitCode:longint; CONST randomSeed:dword=4294967295):T_storedMessages;
      FUNCTION loadForCodeAssistance(VAR packageToInspect:T_package; CONST recycler:P_recycler{$ifdef fullVersion}; OUT callAndIdInfos:P_callAndIdInfos{$endif}):T_storedMessages;
      FUNCTION runScript(CONST filenameOrId:string; CONST scriptSource,mainParameters:T_arrayOfString; CONST sideEffectWhitelist:T_sideEffects; CONST locationForWarning:T_tokenLocation; CONST callerContext:P_context; CONST recycler:P_recycler;  CONST connectLevel:byte; CONST enforceDeterminism:boolean):P_literal;
      {$ifdef fullVersion}
      PROCEDURE runInstallScript;
      PROCEDURE runUninstallScript;
      PROCEDURE demoCallInterpretation(CONST input:T_arrayOfString; CONST recycler:P_recycler; OUT textOut,usedBuiltinIDs:T_arrayOfString);
      {$endif}
  end;

CONST SUPPRESS_EXIT_CODE=maxLongint-159; //just some large, reasonably improbable code
FUNCTION packageFromCode(CONST code:T_arrayOfString; CONST nameOrPseudoName:string):P_package;
FUNCTION messagesToLiteralForSandbox(CONST literalRecycler:P_literalRecycler; CONST messages:T_storedMessages; CONST toInclude:T_messageTypeSet; CONST ExitCode:longint):P_listLiteral;
FUNCTION sandbox:P_sandbox;
VAR getMessagesForInspection: FUNCTION(CONST context:P_context; CONST recycler:P_recycler; CONST scriptPath:string):P_mapLiteral;
{$undef include_interface}
VAR newCodeProvider:F_newCodeProvider;
IMPLEMENTATION
USES sysutils,typinfo, FileUtil, Classes,messageFormatting
     {$ifdef fullVersion},commandLineParameters{$endif};

VAR sandboxes:array of P_sandbox;
    sbLock:TRTLCriticalSection;

PROCEDURE setupSandboxes;
  begin
    initCriticalSection(sbLock);
    enterCriticalSection(sbLock);
    try
      setLength(sandboxes,0);
    finally
      leaveCriticalSection(sbLock);
    end;
  end;

PROCEDURE doneSandboxes;
  VAR i:longint;
  begin
    enterCriticalSection(sbLock);
    try
      for i:=0 to length(sandboxes)-1 do begin
        while sandboxes[i]^.busy do begin
          leaveCriticalSection(sbLock);
          sleep(1); ThreadSwitch;
          enterCriticalSection(sbLock);
        end;
        dispose(sandboxes[i],destroy);
      end;
      setLength(sandboxes,0);
    finally
      leaveCriticalSection(sbLock);
    end;
    doneCriticalSection(sbLock);
  end;

FUNCTION sandbox:P_sandbox;
  VAR i:longint;
      firstTry:boolean=false;
  begin
    result:=nil;
    enterCriticalSection(sbLock);
    try
      repeat
        firstTry:=not(firstTry);
        for i:=0 to length(sandboxes)-1 do if result=nil then begin
          enterCriticalSection(sandboxes[i]^.cs);
          try
            if not(sandboxes[i]^.busy) then begin
              sandboxes[i]^.busy:=true;
              result:=sandboxes[i];
            end;
          finally
            leaveCriticalSection(sandboxes[i]^.cs);
          end;
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
    finally
      leaveCriticalSection(sbLock);
    end;
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

CONSTRUCTOR T_sandbox.create;
  begin
    initCriticalSection(cs);
    messages.createRedirector();
    {$ifdef fullVersion}
    plotSystem.create(nil,true);
    messages.addOutAdapter(@plotSystem,false);
    {$endif}
    globals.create(@messages);
    package.create(newVirtualFileCodeProvider('?',C_EMPTY_STRING_ARRAY),nil);
    busy:=false;
  end;

DESTRUCTOR T_sandbox.destroy;
  begin
    enterCriticalSection(cs);
    while busy do begin
      messages.setStopFlag;
      leaveCriticalSection(cs);
      sleep(1);
      ThreadSwitch;
      enterCriticalSection(cs);
    end;
    try
      package.destroy;
      globals.destroy;
      messages.destroy;
      {$ifdef fullVersion}
      plotSystem.destroy;
      {$endif}
    finally
      leaveCriticalSection(cs);
    end;
    doneCriticalSection(cs);
  end;

FUNCTION T_sandbox.execute(CONST input: T_arrayOfString; CONST sideEffectWhitelist:T_sideEffects; CONST recycler:P_recycler; OUT ExitCode:longint; CONST randomSeed: dword): T_storedMessages;
  begin
    messages.clear;
    messages.setupMessageRedirection(nil,[]);
    package.replaceCodeProvider(newVirtualFileCodeProvider('?',input));
    globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}sideEffectWhitelist,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
    if randomSeed<>4294967295 then globals.prng.resetSeed(randomSeed);
    package.load(lu_forDirectExecution,globals,recycler,C_EMPTY_STRING_ARRAY);
    globals.afterEvaluation(recycler,packageTokenLocation(@package));
    result:=messages.storedMessages(false);
    ExitCode:=messages.getExitCode;
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
  end;

FUNCTION T_sandbox.loadForCodeAssistance(VAR packageToInspect:T_package; CONST recycler:P_recycler{$ifdef fullVersion}; OUT callAndIdInfos:P_callAndIdInfos{$endif}):T_storedMessages;
  VAR errorHolder:T_messagesErrorHolder;
      m:P_storedMessage;
  begin
    errorHolder.createErrorHolder(nil,C_errorsAndWarnings);
    globals.primaryContext.messages:=@errorHolder;
    globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}C_sideEffectsForCodeAssistance,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
    {$ifdef fullVersion}
    new(callAndIdInfos,create);
    {$endif}
    packageToInspect.load(lu_forCodeAssistance,globals,recycler,C_EMPTY_STRING_ARRAY{$ifdef fullVersion},callAndIdInfos{$endif});
    globals.afterEvaluation(recycler,packageTokenLocation(@package));
    result:=errorHolder.storedMessages(true);
    for m in result do m^.rereferenced;
    errorHolder.destroy;
    globals.primaryContext.messages:=@messages;
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
  end;

FUNCTION messagesToLiteralForSandbox(CONST literalRecycler:P_literalRecycler; CONST messages:T_storedMessages; CONST toInclude:T_messageTypeSet; CONST ExitCode:longint):P_listLiteral;
  FUNCTION headByMessageType(CONST message:P_storedMessage):P_listLiteral;
    begin
      result:=literalRecycler^.newListLiteral(3);
      result^.appendString(literalRecycler,message^.getMessageTypeName);
    end;

  VAR m:P_storedMessage;
      messageEntry:P_listLiteral;
  begin
    result:=literalRecycler^.newListLiteral();
    for m in messages do if m^.messageType in toInclude then begin
      messageEntry:=P_listLiteral(headByMessageType(m)^.appendString(literalRecycler,ansistring(m^.getLocation)));
      if      m^.messageType in [mt_echo_input,mt_echo_declaration]
      then messageEntry^.appendString(literalRecycler,join(P_storedMessageWithText(m)^.txt,''))
      else if m^.isTextMessage
      then messageEntry^.appendString(literalRecycler,join(P_storedMessageWithText(m)^.txt,C_lineBreakChar))
      else if m^.messageType=mt_echo_output
      then messageEntry^.append(literalRecycler,P_echoOutMessage(m)^.getLiteral,true);
      result^.append(literalRecycler,messageEntry,false);
    end;
    if ExitCode<>SUPPRESS_EXIT_CODE
    then result^.append(literalRecycler,literalRecycler^.newListLiteral(3)^.appendString(literalRecycler,'exitCode')^.appendString(literalRecycler,'')^.appendInt(literalRecycler,ExitCode),false);
  end;

FUNCTION T_sandbox.runScript(CONST filenameOrId:string; CONST scriptSource,mainParameters:T_arrayOfString; CONST sideEffectWhitelist:T_sideEffects; CONST locationForWarning:T_tokenLocation; CONST callerContext:P_context; CONST recycler:P_recycler; CONST connectLevel:byte; CONST enforceDeterminism:boolean):P_literal;
  CONST TYPES_BY_LEVEL:array[0..3] of T_messageTypeSet=
        ([],
         [mt_clearConsole,mt_printline,mt_printdirect{$ifdef fullVersion},mt_displayTable..mt_displayCustomForm{$endif}],
         [mt_clearConsole,mt_printline,mt_printdirect{$ifdef fullVersion},mt_displayTable..mt_displayCustomForm{$endif},mt_el1_note..mt_el2_userWarning],
         [mt_clearConsole,mt_printline,mt_printdirect{$ifdef fullVersion},mt_displayTable..mt_displayCustomForm{$endif},mt_el1_note..mt_el2_userWarning,mt_el3_evalError..mt_el4_systemError]);

  VAR fileName:string;
      callContextType:T_evaluationContextType;
  begin
    if length(scriptSource)=0 then begin
      if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
      then fileName:=expandFileName(filenameOrId)
      else if not(fileCache.canLocateSource(extractFilePath(locationForWarning.package^.getPath),filenameOrId,fileName)) then fileName:='';
      if (fileName='') or not(fileExists(fileName)) then begin
        callerContext^.messages^.postTextMessage(mt_el2_warning,locationForWarning,'Cannot find script with id or path "'+filenameOrId+'"');
        fileName:='';
        enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
        exit(nil);
      end;
    end;
    if connectLevel=0 then callContextType:=ect_silent
                      else callContextType:=ect_normal;
    messages.clear;
    if connectLevel>0 then messages.setupMessageRedirection(callerContext^.messages,TYPES_BY_LEVEL[connectLevel]);

    if enforceDeterminism then globals.prng.resetSeed(0);
    try
      if length(scriptSource)=0
      then package.replaceCodeProvider(newCodeProvider(fileName))
      else package.replaceCodeProvider(newVirtualFileCodeProvider(filenameOrId,scriptSource));
      globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}sideEffectWhitelist,callContextType,mainParameters,recycler);
      if callerContext=nil
      then globals.primaryContext.callDepth:=0
      else globals.primaryContext.callDepth:=callerContext^.callDepth+50;
      package.load(lu_forCallingMain,globals,recycler,mainParameters);
    finally
      globals.afterEvaluation(recycler,packageTokenLocation(@package));
      result:=messagesToLiteralForSandbox(recycler,messages.storedMessages(false),C_textMessages,messages.getExitCode);
      messages.clear(true);
      enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
    end;
  end;

{$ifdef fullVersion}
PROCEDURE T_sandbox.runInstallScript;
  {$i res_ensureAssoc.inc}
  VAR recycler:P_recycler;
      noLoc:T_tokenLocation;
      targetExe:string;
  begin
    targetExe:=paramStr(0);
    recycler:=newRecycler;
    runScript('tempFile',
        {src} split(decompressString(ensureAssoc_mnh),C_lineBreakChar),
              targetExe,
              C_allSideEffects,
              noLoc,
              nil,
              recycler,0,false);
    freeRecycler(recycler);
  end;

PROCEDURE T_sandbox.runUninstallScript;
  {$i res_removeAssoc.inc}
  VAR recycler:P_recycler;
      exitDummy:longint;
  begin
    recycler:=newRecycler;
    execute(split(decompressString(removeAssoc_mnh),C_lineBreakChar),C_allSideEffects,recycler,exitDummy);
    freeRecycler(recycler);
  end;

PROCEDURE T_sandbox.demoCallInterpretation(CONST input:T_arrayOfString; CONST recycler:P_recycler; OUT textOut,usedBuiltinIDs:T_arrayOfString);
  VAR codeInput:T_arrayOfString;
  FUNCTION executeInternally:T_storedMessages;
    VAR
      callAndIdInfos: T_callAndIdInfos;
      meta: P_builtinFunctionMetaData;
    begin
      callAndIdInfos.create;
      messages.clear;
      messages.setupMessageRedirection(nil,[]);
      package.replaceCodeProvider(newVirtualFileCodeProvider('?',input));
      globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}C_allSideEffects,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
      globals.prng.resetSeed(1);
      package.load(lu_forDirectExecution,globals,recycler,C_EMPTY_STRING_ARRAY,@callAndIdInfos);
      globals.afterEvaluation(recycler,packageTokenLocation(@package));
      result:=messages.storedMessages(false);

      for meta in callAndIdInfos.calledBuiltinFunctions do append(usedBuiltinIDs,meta^.qualifiedId);
      callAndIdInfos.destroy;
    end;

  VAR storedMessages:T_storedMessages;
      i:longint;
      tmp:ansistring;
      m:P_storedMessage;
      formatter:T_guiFormatter;
      message:P_storedMessageWithText;
  begin
    setLength(codeInput,0);
    setLength(usedBuiltinIDs,0);
    setLength(textOut,0);
    formatter.create(true);
    formatter.wrapEcho:=true;
    formatter.preferredLineLength:=80;

    for i:=0 to length(input)-1 do begin
      tmp:=trim(input[i]);
      if startsWith(tmp,COMMENT_PREFIX)
      then append(textOut,ECHO_MARKER+StringOfChar(' ',C_echoPrefixLength)+tmp)
      else if startsWith(tmp,'#S ') or startsWith(tmp,'#C ') or startsWith(tmp,'#H ')
      then append(textOut,tmp)
      else if startsWith(tmp,'#I ') then begin end
      else append(codeInput,input[i]);
    end;

    storedMessages:=executeInternally;
    if length(codeInput)>0 then begin
      new(message,create(mt_echo_input,C_nilSearchTokenLocation,codeInput));
      append(textOut,formatter.formatMessage(message));
      disposeMessage(message);
    end;

    for m in storedMessages do begin
      if not(m^.messageType in [mt_timing_info,mt_echo_input]) then append(textOut,formatter.formatMessage(m));
    end;
    formatter.destroy;
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
  end;

PROCEDURE demoCallInterpretation(CONST input:T_arrayOfString; OUT textOut,usedBuiltinIDs:T_arrayOfString; CONST recycler:P_recycler);
  begin sandbox^.demoCallInterpretation(input,recycler,textOut,usedBuiltinIDs); end;
{$endif}

{$define include_implementation}
{$include funcs_package.inc}

PROCEDURE T_packageReference.loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR globals:T_evaluationGlobals; CONST recycler:P_recycler; CONST parentUsecase:T_packageLoadUsecase);
  VAR i:longint;
      {$ifdef fullVersion}
      callAndIdInfos:P_callAndIdInfos=nil;
      {$endif}
  begin
    with containingPackage^.mainPackage^ do begin
      if containingPackage^.mainPackage^.getCodeProvider^.id=id then begin
        globals.primaryContext.raiseError('Cyclic package dependencies encountered; already loading "'+id+'"',tokenLocation);
        exit;
      end;
      for i:=0 to length(secondaryPackages)-1 do begin
        if secondaryPackages[i]^.getCodeProvider^.id = id then begin
          if  (secondaryPackages[i]^.readyForUsecase<>lu_NONE) and
              (secondaryPackages[i]^.codeChanged)
          then secondaryPackages[i]^.readyForUsecase:=lu_NONE;
          if secondaryPackages[i]^.readyForUsecase<>lu_beingLoaded then begin
            if secondaryPackages[i]^.readyForUsecase<>C_packageLoadUsecaseMeta[parentUsecase].import_usecase then begin
              {$ifdef fullVersion}if parentUsecase in [lu_forCodeAssistance,lu_forCodeAssistanceSecondary] then new(callAndIdInfos,create);{$endif}
              secondaryPackages[i]^.load(C_packageLoadUsecaseMeta[parentUsecase].import_usecase,globals,recycler,C_EMPTY_STRING_ARRAY{$ifdef fullVersion} ,callAndIdInfos{$endif});
              {$ifdef fullVersion}if parentUsecase in [lu_forCodeAssistance,lu_forCodeAssistanceSecondary] then dispose(callAndIdInfos,destroy);{$endif}
            end;
            pack:=secondaryPackages[i];
            exit;
          end else begin
            globals.primaryContext.raiseError('Cyclic package dependencies encountered; already loading "'+id+'"',tokenLocation);
            exit;
          end;
        end;
      end;
      new(pack,create(newCodeProvider(path),containingPackage^.mainPackage));
      setLength(secondaryPackages,length(secondaryPackages)+1);
      secondaryPackages[length(secondaryPackages)-1]:=pack;
      {$ifdef fullVersion}if parentUsecase in [lu_forCodeAssistance,lu_forCodeAssistanceSecondary] then new(callAndIdInfos,create);{$endif}
      pack^.load(C_packageLoadUsecaseMeta[parentUsecase].import_usecase,globals,recycler,C_EMPTY_STRING_ARRAY{$ifdef fullVersion} ,callAndIdInfos{$endif});
      {$ifdef fullVersion}if parentUsecase in [lu_forCodeAssistance,lu_forCodeAssistanceSecondary] then dispose(callAndIdInfos,destroy);{$endif}
    end;
  end;

CONSTRUCTOR T_packageReference.create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST messages:P_messages);
  begin
    locationOfDeclaration:=tokenLocation;
    id:=packId;
    if not(fileCache.canLocateSource(extractFilePath(root),id,path)) then path:='';
    if (messages<>nil) and (path='') then messages^.raiseSimpleError('Cannot locate package for id "'+id+'"',tokenLocation);
    pack:=nil;
  end;

CONSTRUCTOR T_packageReference.createWithSpecifiedPath(CONST path_:ansistring; CONST tokenLocation:T_tokenLocation; CONST messages:P_messages);
  begin
    locationOfDeclaration:=tokenLocation;
    path:=expandFileName(extractFilePath(tokenLocation.package^.getPath)+path_);
    id:=filenameToPackageId(path_);
    if not(fileExists(path)) and fileExists(path_) then path:=path_;
    if not(fileExists(path))
    then messages^.raiseSimpleError('Cannot locate package "'+path+'"',tokenLocation);
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
      SameFileName(expandFileName(path),expandFileName(p)) then exit(true);
    p:=extractFilePath(importingPackage^.getPath)+p;
    result:=(         path =               p ) or
      SameFileName(expandFileName(path),expandFileName(p));
  end;
{$endif}

DESTRUCTOR T_packageReference.destroy;
  begin
    id:='';
    path:='';
    pack:=nil;
  end;

PROCEDURE T_package.interpret(VAR statement: T_enhancedStatement; CONST usecase: T_packageLoadUsecase; VAR globals: T_evaluationGlobals; CONST recycler:P_recycler{$ifdef fullVersion}; CONST callAndIdInfos:P_callAndIdInfos=nil{$endif});
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
        lexer:T_linesLexer;
        importWrapper:P_extendedPackage;
        stmt:T_enhancedStatement;
    begin
      if statement.token.first^.next=nil then begin
        globals.primaryContext.raiseError('Empty include clause',statement.token.first^.location);
        recycler^.cascadeDisposeToken(statement.token.first);
        exit;
      end;
      first:=statement.token.first;
      locationForErrorFeedback:=first^.location;
      {$ifdef fullVersion}
      if (callAndIdInfos<>nil) and (first^.next<>nil) then begin
        clauseEnd:=first^.last^.location;
        inc(clauseEnd.column);
        callAndIdInfos^.addLocalIdInfo(first^.next^.singleTokenToString,first^.next^.location,clauseEnd,tt_include,true);
      end;
      {$endif}
      if extendsLevel>=32 then begin
        globals.primaryContext.raiseError('Max. extension level exceeded ',locationForErrorFeedback);
        exit;
      end;
      first:=recycler^.disposeToken(first);
      if (first^.next=nil) and (first^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule]) then begin
        newId:=first^.txt;
        helperUse.create(getCodeProvider^.getPath,first^.txt,first^.location,globals.primaryContext.messages);
      end else if (first^.next=nil) and (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_string) then begin
        newId:=P_stringLiteral(first^.data)^.value;
        helperUse.createWithSpecifiedPath(newId,first^.location,globals.primaryContext.messages);
      end else begin
        globals.primaryContext.raiseError('Invalid include clause ',locationForErrorFeedback);
        exit;
      end;
      recycler^.cascadeDisposeToken(first);
      new(importWrapper,create(newCodeProvider(helperUse.path),@self));
      setLength(extendedPackages,length(extendedPackages)+1);
      extendedPackages[length(extendedPackages)-1]:=importWrapper;

      helperUse.destroy;
      lexer.createForExtendedPackage(importWrapper,@self{$ifdef fullVersion},callAndIdInfos{$endif});
      stmt:=lexer.getNextStatement(@globals.primaryContext,recycler);
      inc(extendsLevel);
      while (globals.primaryContext.continueEvaluation) and (stmt.token.first<>nil) do begin
        interpret(stmt,usecase,globals,recycler{$ifdef fullVersion},callAndIdInfos{$endif});
        stmt:=lexer.getNextStatement(@globals.primaryContext,recycler);
      end;
      if (stmt.token.first<>nil) then recycler^.cascadeDisposeToken(stmt.token.first);
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

      begin
        {$ifdef fullVersion}
        globals.primaryContext.callStackPushCategory(@self,pc_importing,pseudoCallees);
        {$endif}
        if profile then globals.timeBaseComponent(pc_importing);
        for i:=0 to length(packageUses)-1 do packageUses[i].loadPackage(@self,locationForErrorFeedback,globals,recycler,usecase);
        if profile then globals.timeBaseComponent(pc_importing);
        {$ifdef fullVersion}
        globals.primaryContext.callStackPop(nil);
        {$endif}
        i:=0;
        while i<length(packageUses) do begin
          if packageUses[i].pack=nil then begin
            for j:=i to length(packageUses)-2 do packageUses[j]:=packageUses[j+1];
            setLength(packageUses,length(packageUses)-1);
          end else inc(i);
        end;
        ruleMap.clearImports;
        if globals.primaryContext.continueEvaluation
        then for i:=0 to length(packageUses)-1 do ruleMap.addImports(@packageUses[i].pack^.ruleMap);
        customOperatorRules:=ruleMap.getOperators;
        ruleMap.resolveRuleIds(nil,ON_DELEGATION);
      end;
    {$ifdef fullVersion}
    VAR attribute:string;
        suppressUnusedImport:boolean=false;
    {$endif}
    begin
      {$ifdef fullVersion}
      for attribute in statement.attributes do if startsWith(attribute,SUPPRESS_UNUSED_WARNING_ATTRIBUTE) then suppressUnusedImport:=true;
      {$endif}
      if statement.token.first^.next=nil then begin
        globals.primaryContext.raiseError('Empty use clause',statement.token.first^.location);
        recycler^.cascadeDisposeToken(statement.token.first);
        exit;
      end;
      initialize(newId);
      first:=statement.token.first;
      {$ifdef fullVersion}
      if (callAndIdInfos<>nil) then begin
        clauseEnd:=first^.last^.location;
        inc(clauseEnd.column);
      end;
      {$endif}
      locationForErrorFeedback:=first^.location;
      first:=recycler^.disposeToken(first);
      while first<>nil do begin
        j:=-1;
        if first^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule] then begin
          newId:=first^.txt;
          {$ifdef fullVersion}
          if callAndIdInfos<>nil then callAndIdInfos^.addLocalIdInfo(first^.txt,first^.location,clauseEnd,tt_use,true);
          {$endif}
          if (newId=FORCE_GUI_PSEUDO_PACKAGE) then begin
            if (gui_started=NO) and not(C_packageLoadUsecaseMeta[usecase].assistanceRun) then globals.primaryContext.messages^.logGuiNeeded;
          end else begin
            j:=length(packageUses);
            setLength(packageUses,j+1);
            packageUses[j].create(getCodeProvider^.getPath,first^.txt,first^.location,globals.primaryContext.messages);
            {$ifdef fullVersion}
            packageUses[j].supressUnusedWarning:=suppressUnusedImport;
            {$endif}
          end;
        end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_string) then begin
          {$ifdef fullVersion}
          if callAndIdInfos<>nil then callAndIdInfos^.addLocalIdInfo(first^.singleTokenToString,first^.location,clauseEnd,tt_use,true);
          {$endif}
          newId:=P_stringLiteral(first^.data)^.value;
          j:=length(packageUses);
          setLength(packageUses,j+1);
          packageUses[j].createWithSpecifiedPath(newId,first^.location,globals.primaryContext.messages);
          {$ifdef fullVersion}
          packageUses[j].supressUnusedWarning:=suppressUnusedImport;
          {$endif}
        end else if first^.tokType<>tt_separatorComma then
          globals.primaryContext.raiseError('Cannot interpret use clause containing '+first^.singleTokenToString,first^.location);
        if (j>0) then for i:=0 to j-1 do
          if SameFileName(expandFileName(packageUses[i].path),expandFileName(packageUses[j].path))
                                     or (packageUses[i].id   =               packageUses[j].id)
          then globals.primaryContext.messages^.postTextMessage(mt_el2_warning,first^.location,'Duplicate import: '+newId);
        first:=recycler^.disposeToken(first);
      end;
      if not(globals.primaryContext.continueEvaluation) then begin
        for i:=0 to length(packageUses)-1 do packageUses[i].destroy;
        setLength(packageUses,0);
      end;
      if C_packageLoadUsecaseMeta[usecase].loadUsedPacks then reloadAllPackages(locationForErrorFeedback);
    end;

  VAR assignmentToken:P_token;

  FUNCTION continueDatastoreDeclaration:boolean;
    begin
      if getCodeProvider^.isPseudoFile then begin
        globals.primaryContext.messages^.raiseSimpleError('data stores require the package to be saved to a file.',statement.token.first^.location);
        recycler^.cascadeDisposeToken(statement.token.first);
        result:=false;
      end else result:=true;
    end;

  PROCEDURE assertThatAllIdsCanBeResolved(token:P_token);
    VAR t:P_token;
    begin
      t:=token;
      while t<>nil do begin
        if t^.tokType=tt_identifier then resolveId(t^,globals.primaryContext.messages);
        t:=t^.next;
      end;
    end;

  PROCEDURE parseRule;
    VAR p:P_token; //iterator
        ruleDeclarationStart:T_tokenLocation;
        {$ifdef fullVersion}
        ruleDeclarationEnd:T_tokenLocation;
        parameterId:T_patternElementLocation;
        {$endif}
        //rule meta data
        ruleModifiers:T_modifierSet=[];
        metaData:T_ruleMetaData;
        ruleId:T_idString='';
        evaluateBody:boolean;
        rulePattern:T_pattern;
        ruleBody:P_token;
        subRule:P_subruleExpression;

    begin
      ruleDeclarationStart:=statement.token.first^.location;
      evaluateBody:=(assignmentToken^.tokType in [tt_endOfPatternAssign,tt_assign]);
      ruleBody:=assignmentToken^.next;
      assignmentToken^.next:=nil;
      //plausis:
      if (ruleBody=nil) then begin
        globals.primaryContext.messages^.raiseSimpleError('Missing function body after assignment/declaration token.',assignmentToken^.location);
        recycler^.cascadeDisposeToken(statement.token.first);
        exit;
      end;
      while (statement.token.first<>nil) and (statement.token.first^.tokType=tt_modifier) and (C_modifierInfo[statement.token.first^.getModifier].isRuleModifier) do begin
        include(ruleModifiers,statement.token.first^.getModifier);
        statement.token.first:=recycler^.disposeToken(statement.token.first);
      end;
      evaluateBody:=(evaluateBody or (modifier_mutable in ruleModifiers));
      if (modifier_datastore in ruleModifiers) and not(modifier_memoized in ruleModifiers) and not(continueDatastoreDeclaration) then exit;
      if not(statement.token.first^.tokType in [tt_identifier, tt_userRule, tt_intrinsicRule]) then begin
        globals.primaryContext.messages^.raiseSimpleError('Declaration does not start with an identifier.',statement.token.first^.location);
        recycler^.cascadeDisposeToken(statement.token.first);
        recycler^.cascadeDisposeToken(ruleBody);
        exit;
      end;
      p:=statement.token.first;
      while (p<>nil) and not(p^.tokType in [tt_assign,tt_declare]) do begin
        if (p^.tokType in [tt_identifier, tt_userRule, tt_intrinsicRule]) and isQualified(p^.txt) then begin
          globals.primaryContext.messages^.raiseSimpleError('Declaration head contains qualified ID.',p^.location);
          recycler^.cascadeDisposeToken(statement.token.first);
          recycler^.cascadeDisposeToken(ruleBody);
          exit;
        end;
        p:=p^.next;
      end;
      //:plausis
      ruleId:=trim(statement.token.first^.txt);
      statement.token.first:=recycler^.disposeToken(statement.token.first);
      if isPlainScript and (ruleId=MAIN_RULE_ID) then begin
        globals.primaryContext.raiseError('plain scripts must not have main rules',ruleDeclarationStart);
      end;
      if not(statement.token.first^.tokType in [tt_functionPattern,tt_assign,tt_declare])  then begin
        globals.primaryContext.messages^.raiseSimpleError('Invalid declaration head.',statement.token.first^.location);
        recycler^.cascadeDisposeToken(statement.token.first);
        recycler^.cascadeDisposeToken(ruleBody);
        exit;
      end;
      rulePattern.create;
      if statement.token.first^.tokType=tt_functionPattern then begin
        rulePattern.clone(P_pattern(statement.token.first^.data)^);
        statement.token.first:=recycler^.disposeToken(statement.token.first);
      end;

      if statement.token.first<>nil then begin
        statement.token.first:=recycler^.disposeToken(statement.token.first);
      end else begin
        globals.primaryContext.messages^.raiseSimpleError('Invalid declaration.',ruleDeclarationStart);
        recycler^.cascadeDisposeToken(ruleBody);
        exit;
      end;

      if evaluateBody and (globals.primaryContext.continueEvaluation)
      then begin
        if (C_packageLoadUsecaseMeta[usecase].assistanceRun)
        then assertThatAllIdsCanBeResolved(ruleBody)
        else globals.primaryContext.reduceExpression(ruleBody,recycler);
      end;

      if globals.primaryContext.continueEvaluation then begin
        metaData.create;
        metaData.setComment(join(statement.comments,C_lineBreakChar));
        metaData.setAttributes(statement.attributes,ruleDeclarationStart,globals.primaryContext.messages);
        formatMetaData(metaData,ruleDeclarationStart,@globals.primaryContext,recycler);
        new(subRule,create(rulePattern,ruleBody,ruleDeclarationStart,modifier_private in ruleModifiers,@globals.primaryContext,recycler,metaData));
        ruleMap.declare(ruleId,ruleModifiers,ruleDeclarationStart,@globals.primaryContext,recycler,metaData,subRule);

        if isIdOfAnyOverloadableOperator(ruleId) then customOperatorRules:=ruleMap.getOperators;
      end else recycler^.cascadeDisposeToken(ruleBody);
    end;

  PROCEDURE parseDataStore;
    VAR ruleModifiers:T_modifierSet=[];
        loc:T_tokenLocation;
        metaData:T_ruleMetaData;
    begin
      if not(continueDatastoreDeclaration) then exit;
      while (statement.token.first<>nil) and (statement.token.first^.tokType=tt_modifier) and (C_modifierInfo[statement.token.first^.getModifier].isRuleModifier) do begin
        include(ruleModifiers,statement.token.first^.getModifier);
        loc:=statement.token.first^.location;
        statement.token.first:=recycler^.disposeToken(statement.token.first);
      end;
      if (statement.token.first=nil) or not(statement.token.first^.tokType in [tt_identifier, tt_userRule, tt_intrinsicRule]) or
         (statement.token.first^.next<>nil) then begin
        if statement.token.first<>nil then loc:=statement.token.first^.location;
        globals.primaryContext.messages^.raiseSimpleError('Invalid datastore definition: '+tokensToString(statement.token.first),loc);
        recycler^.cascadeDisposeToken(statement.token.first);
        exit;
      end;
      metaData.create;
      metaData.setComment(join(statement.comments,C_lineBreakChar));
      metaData.setAttributes(statement.attributes,statement.token.first^.location,globals.primaryContext.messages);
      formatMetaData(metaData,statement.token.first^.location,@globals.primaryContext,recycler);
      ruleMap.declare(statement.token.first^.txt,
                      ruleModifiers,
                      statement.token.first^.location,
                      @globals.primaryContext,
                      recycler,
                      metaData,
                      nil);
    end;

  FUNCTION findAssignmentToken:P_token;
    VAR tok:P_token;
        bracketLevel:longint=0;
    begin
      tok:=statement.token.first;
      result:=nil;
      while (tok<>nil) do begin
        if tok^.tokType in C_openingBrackets then inc(bracketLevel) else
        if tok^.tokType in C_closingBrackets then dec(bracketLevel) else
        if (bracketLevel=0) and (tok^.tokType in [tt_assign,tt_declare]) then exit(tok);
        tok:=tok^.next;
      end;
    end;

  begin
    if FlagGUINeeded in globals.primaryContext.messages^.getFlags
    then begin
      recycler^.cascadeDisposeToken(statement.token.first);
      exit;
    end;

    profile:=globals.primaryContext.messages^.isCollecting(mt_timing_info) and (usecase in [lu_forDirectExecution,lu_forCallingMain]);
    if statement.token.first=nil then exit;

    if not(C_packageLoadUsecaseMeta[usecase].assistanceRun) and not(globals.primaryContext.continueEvaluation) then begin
      recycler^.cascadeDisposeToken(statement.token.first);
      exit;
    end;

    if (statement.token.first^.tokType=tt_use) then begin
      interpretUseClause;
      exit;
    end;

    if (statement.token.first^.tokType=tt_include) then begin
      interpretIncludeClause;
      exit;
    end;

    if usecase<>lu_usageScan then begin
      assignmentToken:=findAssignmentToken;
      if (assignmentToken<>nil) then begin
        if not(se_alterPackageState in globals.primaryContext.sideEffectWhitelist) then begin
          globals.primaryContext.messages^.raiseSimpleError('Rule declaration is not allowed here',assignmentToken^.location);
          recycler^.cascadeDisposeToken(statement.token.first);
          exit;
        end;
        {$ifdef fullVersion}
        globals.primaryContext.callStackPushCategory(@self,pc_declaration,pseudoCallees);
        {$endif}
        if profile then globals.timeBaseComponent(pc_declaration);
        if assignmentToken^.next=nil then begin
          globals.primaryContext.messages^.raiseSimpleError('Missing rule body',assignmentToken^.location);
          recycler^.cascadeDisposeToken(statement.token.first);
          exit;
        end;
        if globals.primaryContext.messages^.isCollecting(mt_echo_declaration)
        then globals.primaryContext.messages^.postTextMessage(mt_echo_declaration,statement.token.first^.location,tokensToEcho(statement.token.first));
        parseRule;
        if profile then globals.timeBaseComponent(pc_declaration);
        {$ifdef fullVersion}
        globals.primaryContext.callStackPop(nil);
        {$endif}
      end else if statement.token.first^.tokType=tt_modifier then begin
        if not(se_alterPackageState in globals.primaryContext.sideEffectWhitelist) then begin
          globals.primaryContext.messages^.raiseSimpleError('Datastore declaration is not allowed here',statement.token.first^.location);
          recycler^.cascadeDisposeToken(statement.token.first);
          exit;
        end;
        {$ifdef fullVersion}
        globals.primaryContext.callStackPushCategory(@self,pc_declaration,pseudoCallees);
        {$endif}
        if profile then globals.timeBaseComponent(pc_declaration);
        if globals.primaryContext.messages^.isCollecting(mt_echo_declaration)
        then globals.primaryContext.messages^.postTextMessage(mt_echo_declaration,statement.token.first^.location,tokensToEcho(statement.token.first));
        parseDataStore;
        if profile then globals.timeBaseComponent(pc_declaration);
        {$ifdef fullVersion}
        globals.primaryContext.callStackPop(nil);
        {$endif}
      end else if globals.primaryContext.continueEvaluation then begin
        case usecase of
          lu_forDirectExecution:begin
            customOperatorRules:=ruleMap.getOperators;
            {$ifdef fullVersion}
            globals.primaryContext.callStackPushCategory(@self,pc_interpretation,pseudoCallees);
            {$endif}
            if profile then globals.timeBaseComponent(pc_interpretation);
            if (statement.token.first=nil) then exit;
            if globals.primaryContext.messages^.isCollecting(mt_echo_input)
            then globals.primaryContext.messages^.postTextMessage(mt_echo_input,statement.token.first^.location,tokensToEcho(statement.token.first));
            globals.primaryContext.reduceExpression(statement.token.first,recycler);
            if profile then globals.timeBaseComponent(pc_interpretation);
            {$ifdef fullVersion}
            globals.primaryContext.callStackPop(nil);
            {$endif}
            if (statement.token.first<>nil) and
               globals.primaryContext.messages^.isCollecting(mt_echo_output) and
               (statement.token.first^.next=nil) and
               (statement.token.first^.tokType=tt_literal)
            then globals.primaryContext.messages^.postCustomMessage(newEchoMessage(P_literal(statement.token.first^.data),statement.token.first^.location),true);
          end;
          lu_forCodeAssistance,lu_forCodeAssistanceSecondary: if (statement.token.first<>nil) then begin
            resolveBuiltinIDs(statement.token.first,globals.primaryContext.messages);
          end
          else globals.primaryContext.messages^.postTextMessage(mt_el1_note,statement.token.first^.location,'Skipping expression '+tokensToString(statement.token.first,50));
        end;
      end;
    end;
    if statement.token.first<>nil then recycler^.cascadeDisposeToken(statement.token.first);
    statement.token.first:=nil;
  end;

PROCEDURE T_package.load(usecase: T_packageLoadUsecase; VAR globals: T_evaluationGlobals; CONST recycler:P_recycler; CONST mainParameters: T_arrayOfString{$ifdef fullVersion}; CONST callAndIdInfos:P_callAndIdInfos=nil{$endif});
  VAR profile:boolean=false;
  PROCEDURE executeMain;
    VAR mainRule:P_rule;
        parametersForMain:P_listLiteral=nil;
        t:T_tokenRange=(first:nil;last:nil);
        i:longint;
        {$ifdef fullVersion}displayedHelp:boolean=false;{$endif}
    begin
      if not(readyForUsecase=lu_forCallingMain) or not(globals.primaryContext.continueEvaluation) then exit;
      mainRule:=ruleMap.getLocalMain;
      if mainRule=nil
      then globals.primaryContext.messages^.raiseSimpleError('The specified package contains no main rule.',packageTokenLocation(@self))
      else begin
        parametersForMain:=recycler^.newListLiteral(length(mainParameters));
        for i:=0 to length(mainParameters)-1 do parametersForMain^.appendString(recycler,mainParameters[i]);

        {$ifdef fullVersion}
        globals.primaryContext.callStackPushCategory(@self,pc_interpretation,pseudoCallees);
        {$endif}
        if profile then globals.timeBaseComponent(pc_interpretation);

        if mainRule^.canBeApplied(packageTokenLocation(@self),parametersForMain,t,@globals.primaryContext,recycler)
        then globals.primaryContext.reduceExpression(t.first,recycler)
        else if (length(mainParameters)=1) and (mainParameters[0]='-h') then begin
          globals.primaryContext.messages^.postTextMessage(mt_printline,C_nilSearchTokenLocation,split(getHelpOnMain));
          {$ifdef fullVersion}displayedHelp:=true;{$endif}
        end else begin
          globals.primaryContext.raiseCannotApplyError('user defined rule '+mainRule^.getId,
                                        parametersForMain,
                                        mainRule^.getLocation,
                                        C_lineBreakChar+join(mainRule^.getCmdLineHelpText,C_lineBreakChar),true);
          if (length(mainParameters)=1) and (mainParameters[0]='-h') then globals.primaryContext.messages^.postTextMessage(mt_printline,C_nilSearchTokenLocation,split(getHelpOnMain));
        end;
        if profile then globals.timeBaseComponent(pc_interpretation);
        {$ifdef fullVersion}
        globals.primaryContext.callStackPop(nil);
        {$endif}

        {$ifdef fullVersion}
        //error handling if main returns more than one token:------------------
        if not(displayedHelp) and ((t.first=nil) or (t.first^.next<>nil)) and (FlagGUINeeded in globals.primaryContext.messages^.getFlags)
          then globals.primaryContext.messages^.postTextMessage(mt_el1_note,packageTokenLocation(@self),'Evaluation requires GUI-startup. Re-evaluating.');
        //------------------:error handling if main returns more than one token
        {$endif}
        recycler^.cascadeDisposeToken(t.first);
        recycler^.disposeLiteral(parametersForMain);
        parametersForMain:=nil;
      end;
    end;

  VAR lexer:T_linesLexer;
      stmt :T_enhancedStatement;

  FUNCTION isPlainScriptStatement:boolean;
    begin
      result:=(stmt.token.first<>nil) and
              (stmt.token.first^.txt='plain') and
              (stmt.token.first^.next<>nil) and
              (stmt.token.first^.next^.txt='script') and
              (stmt.token.first^.next^.next=nil);
    end;

  {$ifdef fullVersion}
  PROCEDURE afterLoadForAssistance;
    PROCEDURE checkParameters;
      VAR rule:P_rule;
          pack:P_package;
      begin
        for rule in ruleMap.getAllLocalRules do
          rule^.checkParameters(@globals.primaryContext,callAndIdInfos);
        for pack in secondaryPackages do
          for rule in pack^.ruleMap.getAllLocalRules do
            rule^.checkParameters(@globals.primaryContext,callAndIdInfos);
      end;

    begin
      customOperatorRules:=ruleMap.getOperators;
      ruleMap.resolveRuleIds(globals.primaryContext.messages,ON_EVALUATION);
      complainAboutUnused(globals.primaryContext.messages,callAndIdInfos);
      checkParameters;
    end;
  {$endif}

  begin
    profile:=globals.primaryContext.messages^.isCollecting(mt_timing_info) and (usecase in [lu_forDirectExecution,lu_forCallingMain]);
    commentOnPlainMain:='Undocumented plain script';
    if usecase = lu_NONE        then raise Exception.create('Invalid usecase: lu_NONE');
    if usecase = lu_beingLoaded then raise Exception.create('Invalid usecase: lu_beingLoaded');
    clear(false);
    readyForUsecase:=lu_beingLoaded;
    logReady(getCodeProvider^.stateHash);

    if profile then globals.timeBaseComponent(pc_tokenizing);
    lexer.createForPackageParsing(@self{$ifdef fullVersion},callAndIdInfos{$endif});
    if profile then globals.timeBaseComponent(pc_tokenizing);
    stmt:=lexer.getNextStatement(@globals.primaryContext,recycler);
    isPlainScript:=isPlainScriptStatement;
    if isPlainScript then begin
      case usecase of
        lu_forImport         : globals.primaryContext.messages^.raiseSimpleError('Cannot import package declared as "plain script"',stmt.token.first^.location);
        lu_forCallingMain    : usecase:=lu_forDirectExecution;
      end;
      commentOnPlainMain:=join(stmt.comments,C_lineBreakChar);
      recycler^.cascadeDisposeToken(stmt.token.first);
      stmt:=lexer.getNextStatement(@globals.primaryContext,recycler);
    end;
    if profile then globals.timeBaseComponent(pc_tokenizing);

    while (C_packageLoadUsecaseMeta[usecase].assistanceRun or globals.primaryContext.continueEvaluation) and (stmt.token.first<>nil) do begin
      interpret(stmt,usecase,globals,recycler{$ifdef fullVersion},callAndIdInfos{$endif});
      if profile then globals.timeBaseComponent(pc_tokenizing);
      stmt:=lexer.getNextStatement(@globals.primaryContext,recycler);
      if profile then globals.timeBaseComponent(pc_tokenizing);
    end;
    if (stmt.token.first<>nil) then recycler^.cascadeDisposeToken(stmt.token.first);
    lexer.destroy;
    if  C_packageLoadUsecaseMeta[usecase].assistanceRun then begin
      readyForUsecase:=usecase;
      {$ifdef fullVersion}
      if (usecase in [lu_forCodeAssistance,lu_forCodeAssistanceSecondary]) and isMain then afterLoadForAssistance;
      {$endif}
      exit;
    end else if globals.primaryContext.continueEvaluation then begin
      customOperatorRules:=ruleMap.getOperators;
      readyForUsecase:=usecase;
      if usecase=lu_forCallingMain
      then executeMain
      else if isMain and isPlainScript and (length(mainParameters)=1) and (mainParameters[0]='-h')
      then globals.primaryContext.messages^.postTextMessage(mt_printline,C_nilSearchTokenLocation,split(getHelpOnMain));
    end else begin
      readyForUsecase:=lu_NONE;
      if isMain and
         isPlainScript and
         not(globals.primaryContext.continueEvaluation) and
         not(FlagGUINeeded in globals.primaryContext.messages^.getFlags)
      then globals.primaryContext.messages^.postTextMessage(mt_printline,C_nilSearchTokenLocation,split(getHelpOnMain));
    end;

    if isMain and C_packageLoadUsecaseMeta[usecase].finalizeWorkers then begin
      globals.startFinalization;
      finalize(@globals.primaryContext,recycler);
      globals.stopWorkers(recycler);
    end;
  end;

CONSTRUCTOR T_package.create(CONST provider: P_codeProvider; CONST mainPackage_: P_package);
  begin
    inherited create(provider);
    mainPackage:=mainPackage_;
    if mainPackage=nil then mainPackage:=@self;
    setLength(secondaryPackages,0);
    setLength(extendedPackages,0);
    setLength(packageUses,0);
    ruleMap.create(@self);
    {$ifdef fullVersion}
    pseudoCallees:=blankProfilingCalls;
    {$endif}
  end;

PROCEDURE T_package.clear(CONST includeSecondaries: boolean);
  VAR i:longint;
  begin
    ruleMap.clear;
    if includeSecondaries then begin
      for i:=0 to length(secondaryPackages)-1 do dispose(secondaryPackages[i],destroy);
      setLength(secondaryPackages,0);
    end;
    for i:=0 to length(extendedPackages)-1 do dispose(extendedPackages[i],destroy);
    setLength(extendedPackages,0);
    for i:=0 to length(packageUses)-1 do packageUses[i].destroy; setLength(packageUses,0);
    clearCustomOperators;
    readyForUsecase:=lu_NONE;
  end;

FUNCTION T_package.writeDataStores(CONST messages:P_messages; CONST recurse:boolean; CONST literalRecycler:P_literalRecycler; VAR flush:T_datastoreFlush):boolean;
  VAR i:longint;
  begin
    result:=ruleMap.writeBackDatastores(messages,literalRecycler,flush);
    if recurse then for i:=0 to length(packageUses)-1 do if packageUses[i].pack^.writeDataStores(messages,recurse,literalRecycler,flush) then result:=true;
  end;

PROCEDURE T_package.finalize(CONST context:P_context; CONST recycler:P_recycler);
  VAR i:longint;
      flush:T_datastoreFlush;
  begin
    for i:=0 to length(packageUses)-1 do packageUses[i].pack^.finalize(context,recycler);
    ruleMap.executeAfterRules(context,recycler);
    if isMain then begin
      flush.create(tb_undefined,tb_undefined);
      writeDataStores(context^.messages,true,recycler,flush);
      flush.finalize(context^.messages,recycler);
      flush.destroy;
    end;
  end;

FUNCTION T_package.literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_recycler):string;
  VAR toStringRule:T_ruleMapEntry;
      toReduce:T_tokenRange;
      parameters:P_listLiteral;
      stringOut:P_literal=nil;
  begin
    if ruleMap.containsKey('toString',toStringRule) and (toStringRule.entryType=tt_userRule)
    then begin
      parameters:=P_listLiteral(recycler^.newListLiteral(1)^.append(recycler,L,true));
      if P_rule(toStringRule.value)^.canBeApplied(location,parameters,toReduce,context,recycler)
      then stringOut:=P_context(context)^.reduceToLiteral(toReduce.first,recycler).literal;
      recycler^.disposeLiteral(parameters);
    end;

    if stringOut=nil then begin
      if (L^.literalType=lt_string)
      then result:=P_stringLiteral(L)^.value
      else result:=L^.toString();
    end else begin
      if stringOut^.literalType=lt_string
      then result:=P_stringLiteral(stringOut)^.value
      else result:=stringOut^.toString();
      recycler^.disposeLiteral(stringOut);
    end;
  end;

FUNCTION T_package.getTypeMap: T_typeMap;
  begin
    result:=ruleMap.getTypeMap;
  end;

DESTRUCTOR T_package.destroy;
  {$ifdef fullVersion}VAR c:T_profileCategory;{$endif}
  begin
    clear(true);
    ruleMap.destroy;
    {$ifdef fullVersion}
    for c in T_profileCategory do if pseudoCallees[c]<>nil then dispose(pseudoCallees[c],destroy);
    {$endif}
    inherited destroy;
  end;

FUNCTION T_package.getSecondaryPackageById(CONST id: ansistring): ansistring;
  VAR i:longint;
  begin
    for i:=0 to length(secondaryPackages)-1 do if secondaryPackages[i]^.getId=id then exit(secondaryPackages[i]^.getPath);
    result:='';
  end;

{$ifdef fullVersion}
PROCEDURE T_package.complainAboutUnused(CONST messages:P_messages; CONST functionCallInfos:P_callAndIdInfos);
  VAR import:T_packageReference;
      builtinOverrides:T_arrayOfString;
  begin
    if functionCallInfos=nil then exit;
    ruleMap.complainAboutUnused(messages,functionCallInfos);
    for import in packageUses do begin
      builtinOverrides:=import.pack^.ruleMap.getBuiltinOverrides;
      if not(import.supressUnusedWarning) and
         not(functionCallInfos^.isPackageReferenced(import.pack^.getPath)) and
         (length(builtinOverrides)=0)
      then messages^.postTextMessage(mt_el2_warning,
                                     import.locationOfDeclaration,
                                     'Unused import '+import.pack^.getId+' ('+import.pack^.getPath+')');
      if length(builtinOverrides)>0
      then messages^.postTextMessage(mt_el1_note,
                                     import.locationOfDeclaration,
                                     'Package '+import.pack^.getId+' overloads: '+join(builtinOverrides,', '));
    end;
  end;
{$endif}

FUNCTION T_package.getHelpOnMain: ansistring;
  VAR mainRule:P_rule;
      docText:T_arrayOfString;
      i:longint;
  begin
    if isPlainScript then exit('Plain script: '+commentOnPlainMain);
    mainRule:=ruleMap.getLocalMain;
    if mainRule=nil
    then exit('The package contains no main rule')
    else begin
      result:='';
      docText:=mainRule^.getCmdLineHelpText;
      for i:=0 to 1 do result+=LineEnding+docText[i];
      dropFirst(docText,2);
      result+=LineEnding+join(formatTabs(docText),LineEnding);
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
  PROCEDURE addRule(CONST entry:T_ruleMapEntry);
    VAR value:P_literal;
        rule:P_rule;
        variable:P_variable;
    begin
      case entry.entryType of
        tt_userRule: begin
          rule:=P_rule(entry.value);
          if not(rule^.isReportable(value)) then exit;
          variableReport.addEntry(filenameToPackageId(rule^.getLocation.package^.getPath)+'.'+rule^.getId,value,true);
        end;
        tt_globalVariable: begin
          variable:=P_variable(entry.value);
          if not(variable^.isReportable(value)) then exit;
          variableReport.addEntry(filenameToPackageId(variable^.getLocation.package^.getPath)+'.'+variable^.getId,value,true);
        end;
      end;
    end;

  VAR entry:T_ruleMapEntry;
  begin
    for entry in ruleMap.valueSet do addRule(entry);
  end;
{$endif}

FUNCTION T_package.inspect(CONST includeRulePointer: boolean; CONST context: P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; CONST functionCallInfos:P_callAndIdInfos{$endif}): P_mapLiteral;
  FUNCTION usesList:P_listLiteral;
    VAR i:longint;
    begin
      result:=recycler^.newListLiteral(length(packageUses));
      for i:=0 to length(packageUses)-1 do result^.append(
        recycler,
        recycler^.newListLiteral^
          .appendString(recycler, packageUses[i].id)^
          .appendString(recycler, packageUses[i].path),false);
    end;

  FUNCTION includeList:P_listLiteral;
    VAR i:longint;
    begin
      result:=recycler^.newListLiteral(length(extendedPackages));
      for i:=0 to length(extendedPackages)-1 do result^.append(
        recycler,
        recycler^.newListLiteral^
          .appendString(recycler, extendedPackages[i]^.getId)^
          .appendString(recycler, extendedPackages[i]^.getPath),false);
    end;

  {$ifdef fullVersion}
  FUNCTION builtinCallList:P_listLiteral;
    VAR builtin:P_builtinFunctionMetaData;
    begin
      result:=recycler^.newListLiteral();
      if functionCallInfos<>nil then
      for builtin in functionCallInfos^.calledBuiltinFunctions do result^.appendString(recycler,builtin^.qualifiedId);
    end;
  {$endif}

  begin
    {$ifdef fullVersion}
    if (functionCallInfos<>nil) then ruleMap.fillCallInfos(functionCallInfos);
    {$endif}

    result:=recycler^.newMapLiteral(7)^
        .put(recycler, 'id'      ,getId)^
        .put(recycler, 'path'    ,getPath)^
        .put(recycler, 'source'  ,join(getCodeProvider^.getLines,C_lineBreakChar))^
        .put(recycler, 'uses'    ,usesList,false)^
        .put(recycler, 'includes',includeList,false)^
        .put(recycler, 'declares',ruleMap.inspect(P_context(context),recycler,includeRulePointer),false)^
        .put(recycler, 'plain script',newBoolLiteral(isPlainScript),false);
    {$ifdef fullVersion}
    result^.put(recycler,'called builtin',builtinCallList,false);
    {$endif}
  end;

{$ifdef fullVersion}
FUNCTION T_package.getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
  VAR rule:P_rule;
      subRule:P_subruleExpression;
      matchesAll:boolean;
      key:string;
  begin
    setLength(result,0);
    for rule in ruleMap.getAllLocalRules do if (rule^.getRuleType in [rt_normal,rt_synchronized,rt_memoized,rt_customTypeCheck,rt_duckTypeCheck,rt_customTypeCast]) then
    for subRule in P_ruleWithSubrules(rule)^.getSubrules do begin
      matchesAll:=true;
      for key in attributeKeys do matchesAll:=matchesAll and subRule^.metaData.hasAttribute(key,caseSensitive);
      if matchesAll then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=subRule;
      end;
    end;
  end;
{$endif}

FUNCTION T_package.resolveId(VAR token: T_token; CONST messagesOrNil:P_messages):boolean;
  VAR intrinsicFuncPtr:P_intFuncCallback;
      entry:T_ruleMapEntry;
  begin
    if ruleMap.containsKey(token.txt,entry) then begin
      token.tokType:=entry.entryType;
      token.data   :=entry.value;
      exit(true);
    end;
    if builtinFunctionMap.containsFunctionForId(token.txt,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRule;
      token.data:=intrinsicFuncPtr;
      exit(true);
    end;
    if messagesOrNil<>nil then messagesOrNil^.raiseSimpleError('Cannot resolve ID "'+token.txt+'"',token.location);
    result:=false;
  end;

FUNCTION T_package.resolveLocationForStackTrace(CONST location:T_tokenLocation):string;
  begin
    //TODO: Find a better solution based on rule-map
    result:=inherited;
  end;

{$ifdef fullVersion}
FUNCTION T_package.usedPackages: T_packageList;
  VAR i:longint;
  begin
    setLength(result,length(packageUses));
    for i:=0 to length(result)-1 do result[i]:=packageUses[i].pack;
  end;

FUNCTION T_package.declaredRules(CONST ruleSorting:T_ruleSorting):T_ruleMapEntries;
  VAR tmp:T_ruleMapEntry;
      i,j:longint;
  begin
    result:=ruleMap.valueSet;
    j:=0;
    for i:=0 to length(result)-1 do if not(result[i].isImportedOrDelegateWithoutLocal) then begin
      result[j]:=result[i];
      inc(j);
    end;
    setLength(result,j);

    case ruleSorting of
      rs_byNameCaseSensitive:
        for i:=1 to length(result)-1 do
        for j:=0 to i-1 do
        if result[i].value^.getId<result[j].value^.getId then begin
          tmp:=result[i]; result[i]:=result[j]; result[j]:=tmp;
        end;
      rs_byNameCaseInsensitive:
        for i:=1 to length(result)-1 do
        for j:=0 to i-1 do
        if uppercase(result[i].value^.getId)<uppercase(result[j].value^.getId) then begin
          tmp:=result[i]; result[i]:=result[j]; result[j]:=tmp;
        end;
      rs_byLocation:
        for i:=1 to length(result)-1 do
        for j:=0 to i-1 do
        if result[i].value^.getLocation<result[j].value^.getLocation then begin
          tmp:=result[i]; result[i]:=result[j]; result[j]:=tmp;
        end;
    end;
  end;

FUNCTION T_package.getImport(CONST idOrPath: string): P_abstractPackage;
  VAR ref:T_packageReference;
  begin
    for ref in packageUses do if ref.hasIdOrPath(idOrPath,@self) then exit(ref.pack);
    result:=nil;
  end;

FUNCTION T_package.getExtended(CONST idOrPath: string): P_abstractPackage;
  VAR e:P_extendedPackage;
      dummy:longint;
  begin
    for e in extendedPackages do if (e^.getId=idOrPath) or (e^.getPath=unescapeString(idOrPath,1,dummy)) then exit(e);
    result:=nil;
  end;

FUNCTION T_package.usedAndExtendedPackages: T_arrayOfString;
  VAR ref:T_packageReference;
      ext:P_extendedPackage;
  begin
    setLength(result,0);
    for ref in packageUses      do append(result,ref.path);
    for ext in extendedPackages do append(result,ext^.getPath);
    sortUnique(result);
    dropValues(result,'');
  end;

FUNCTION T_package.isExecutable: boolean;
  begin
    result:=isPlainScript or ruleMap.containsKey(MAIN_RULE_ID);
  end;
{$endif}

{$undef include_implementation}
INITIALIZATION
  newCodeProvider:=@fileWrappers.newFileCodeProvider;
  setupSandboxes;
{$define include_initialization}
  {$ifdef fullVersion}
  demoCodeInterpretCallback:=@demoCallInterpretation;
  {$endif}
  {$include funcs_package.inc}
{$undef include_initialization}

FINALIZATION
  doneSandboxes;
end.
