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
       mnh_html,
       tokenStack,
       debuggingVar,
     {$else}
       mySys,
     {$endif}
     funcs,
     operators,
     funcs_format,
     funcs_ipc, funcs_server,
     patterns,
     subrules,
     rules,
     recyclers,
     tokenArray;

{$define include_interface}
TYPE
  P_package=^T_package;
  T_packageLoadUsecase=(lu_NONE,lu_beingLoaded,lu_forImport,lu_forCallingMain,lu_forDirectExecution,lu_forCodeAssistance);
  T_ruleSorting=(rs_none,rs_byNameCaseSensitive,rs_byNameCaseInsensitive,rs_byLocation);

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
    PROCEDURE loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler; CONST forCodeAssistance:boolean);
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
      PROCEDURE writeDataStores(CONST messages:P_messages; CONST recurse:boolean);
      public
      PROCEDURE interpret(VAR statement:T_enhancedStatement; CONST usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos; CONST functionCallInfos:P_functionCallInfos{$endif});
      private
      FUNCTION isMain:boolean;
      {$ifdef fullVersion}
      PROCEDURE complainAboutUnused(CONST messages:P_messages; CONST functionCallInfos:P_functionCallInfos);
      {$endif}
    protected
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
    public
      ruleMap:T_ruleMap;
      PROCEDURE clear(CONST includeSecondaries:boolean);
      CONSTRUCTOR create(CONST provider:P_codeProvider; CONST mainPackage_:P_package);
      FUNCTION getSecondaryPackageById(CONST id:ansistring):ansistring;
      PROCEDURE load(usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler; CONST mainParameters:T_arrayOfString{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos; CONST functionCallInfos:P_functionCallInfos{$endif});

      DESTRUCTOR destroy; virtual;

      FUNCTION getHelpOnMain:ansistring;
      PROCEDURE finalize(VAR context:T_context; VAR recycler:T_recycler);
      PROCEDURE resolveId(VAR token:T_token; CONST messagesOrNil:P_messages{$ifdef fullVersion};CONST markAsUsed:boolean=true{$endif}); virtual;
      FUNCTION getTypeMap:T_typeMap; virtual;
      FUNCTION literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; VAR recycler:T_recycler):string; virtual;
      FUNCTION inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; VAR recycler:T_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_functionCallInfos{$endif}):P_mapLiteral; virtual;
      {$ifdef fullVersion}
      FUNCTION getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
      PROCEDURE reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
      FUNCTION declaredRules(CONST ruleSorting:T_ruleSorting):T_ruleMapEntries;
      FUNCTION usedPackages:T_packageList;
      FUNCTION getImport(CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION getExtended(CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION usedAndExtendedPackages:T_arrayOfString;
      {$endif}
    end;

  {$ifdef fullVersion}
  //T_packageCallbackInObject=PROCEDURE (CONST package:P_package) of object;
  {$endif}

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
      FUNCTION execute(CONST input:T_arrayOfString; VAR recycler:T_recycler; CONST randomSeed:dword=4294967295):T_storedMessages;
      FUNCTION loadForCodeAssistance(VAR packageToInspect:T_package; VAR recycler:T_recycler{$ifdef fullVersion}; OUT functionCallInfos:P_functionCallInfos{$endif}):T_storedMessages;
      FUNCTION runScript(CONST filenameOrId:string; CONST scriptSource,mainParameters:T_arrayOfString; CONST locationForWarning:T_tokenLocation; CONST callerContext:P_context; VAR recycler:T_recycler;  CONST connectLevel:byte; CONST enforceDeterminism:boolean):P_literal;
      {$ifdef fullVersion}
      PROCEDURE runInstallScript;
      PROCEDURE runUninstallScript;
      FUNCTION  usedAndExtendedPackages(CONST fileName:string):T_arrayOfString;
      {$endif}
  end;

FUNCTION packageFromCode(CONST code:T_arrayOfString; CONST nameOrPseudoName:string):P_package;
FUNCTION sandbox:P_sandbox;
{$undef include_interface}
VAR newCodeProvider:F_newCodeProvider;
IMPLEMENTATION
USES sysutils,typinfo, FileUtil, Classes;

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
      for i:=0 to length(sandboxes)-1 do dispose(sandboxes[i],destroy);
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

FUNCTION T_sandbox.execute(CONST input: T_arrayOfString; VAR recycler:T_recycler; CONST randomSeed: dword): T_storedMessages;
  begin
    messages.clear;
    messages.setupMessageRedirection(nil,[]);
    package.replaceCodeProvider(newVirtualFileCodeProvider('?',input));
    globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}ect_silent,C_EMPTY_STRING_ARRAY,recycler);
    if randomSeed<>4294967295 then globals.prng.resetSeed(randomSeed);
    package.load(lu_forDirectExecution,globals,recycler,C_EMPTY_STRING_ARRAY{$ifdef fullVersion},nil,nil{$endif});
    globals.afterEvaluation(recycler);
    result:=messages.storedMessages(false);
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
  end;

FUNCTION T_sandbox.loadForCodeAssistance(VAR packageToInspect:T_package; VAR recycler:T_recycler{$ifdef fullVersion}; OUT functionCallInfos:P_functionCallInfos{$endif}):T_storedMessages;
  VAR errorHolder:T_messagesErrorHolder;
      m:P_storedMessage;
  begin
    errorHolder.createErrorHolder(nil,C_errorsAndWarnings);
    globals.primaryContext.messages:=@errorHolder;
    globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}ect_silent,C_EMPTY_STRING_ARRAY,recycler);
    {$ifdef fullVersion}
    new(functionCallInfos,create);
    {$endif}
    packageToInspect.load(lu_forCodeAssistance,globals,recycler,C_EMPTY_STRING_ARRAY{$ifdef fullVersion},nil,functionCallInfos{$endif});
    globals.afterEvaluation(recycler);
    result:=errorHolder.storedMessages(true);
    for m in result do m^.rereferenced;
    errorHolder.destroy;
    globals.primaryContext.messages:=@messages;
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
  end;

FUNCTION T_sandbox.runScript(CONST filenameOrId:string; CONST scriptSource,mainParameters:T_arrayOfString; CONST locationForWarning:T_tokenLocation; CONST callerContext:P_context; VAR recycler:T_recycler; CONST connectLevel:byte; CONST enforceDeterminism:boolean):P_literal;
  CONST TYPES_BY_LEVEL:array[0..3] of T_messageTypeSet=
        ([],
         [mt_clearConsole,mt_printline,mt_printdirect{$ifdef fullVersion},mt_displayTable..mt_displayCustomForm{$endif}],
         [mt_clearConsole,mt_printline,mt_printdirect{$ifdef fullVersion},mt_displayTable..mt_displayCustomForm{$endif},mt_el1_note..mt_el2_userWarning],
         [mt_clearConsole,mt_printline,mt_printdirect{$ifdef fullVersion},mt_displayTable..mt_displayCustomForm{$endif},mt_el1_note..mt_el2_userWarning,mt_el3_evalError..mt_el4_systemError]);

  VAR fileName:string='';
      callContextType:T_evaluationContextType;
  begin
    if length(scriptSource)=0 then begin
      if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
      then fileName:=expandFileName(filenameOrId)
      else fileName:=locateSource(extractFilePath(locationForWarning.package^.getPath),filenameOrId);
      if (fileName='') or not(fileExists(fileName)) then begin
        callerContext^.messages^.postTextMessage(mt_el2_warning,locationForWarning,'Cannot find script with id or path "'+filenameOrId+'"');
        exit(nil);
      end;
    end;
    if connectLevel=0 then callContextType:=ect_silent
                      else callContextType:=ect_normal;
    messages.clear;
    messages.setupMessageRedirection(callerContext^.messages,TYPES_BY_LEVEL[connectLevel]);

    if enforceDeterminism then globals.prng.resetSeed(0);
    if length(scriptSource)=0
    then package.replaceCodeProvider(newCodeProvider(fileName))
    else package.replaceCodeProvider(newVirtualFileCodeProvider(filenameOrId,scriptSource));
    try
      globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}callContextType,mainParameters,recycler);
      package.load(lu_forCallingMain,globals,recycler,mainParameters{$ifdef fullVersion},nil,nil{$endif});
    finally
      globals.afterEvaluation(recycler);
      result:=messagesToLiteralForSandbox(messages.storedMessages(false),C_textMessages);
      globals.primaryContext.finalizeTaskAndDetachFromParent(@recycler);
      enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
    end;
  end;

{$ifdef fullVersion}
PROCEDURE T_sandbox.runInstallScript;
  {$i res_ensureAssoc.inc}
  VAR recycler:T_recycler;
  begin recycler.initRecycler; execute(split(decompressString(ensureAssoc_mnh),C_lineBreakChar),recycler); recycler.cleanup; end;

PROCEDURE T_sandbox.runUninstallScript;
  {$i res_removeAssoc.inc}
  VAR recycler:T_recycler;
  begin recycler.initRecycler; execute(split(decompressString(removeAssoc_mnh),C_lineBreakChar),recycler); recycler.cleanup; end;

FUNCTION T_sandbox.usedAndExtendedPackages(CONST fileName:string):T_arrayOfString;
  VAR recycler:T_recycler;
  begin
    try
      recycler.initRecycler;
      package.replaceCodeProvider(newCodeProvider(fileName));
      globals.resetForEvaluation(@package,@package.reportVariables,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
      package.load(lu_forCodeAssistance,globals,recycler,C_EMPTY_STRING_ARRAY,nil,nil);
      result:=package.usedAndExtendedPackages;
    finally
      globals.afterEvaluation(recycler);
      package.clear(true);
      globals.primaryContext.finalizeTaskAndDetachFromParent(@recycler);
      enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
      recycler.cleanup;
    end;
  end;

PROCEDURE demoCallToHtml(CONST input:T_arrayOfString; OUT textOut,htmlOut,usedBuiltinIDs:T_arrayOfString; VAR recycler:T_recycler);
  VAR messages:T_storedMessages;
      i:longint;
      tmp:ansistring;
      raw:T_rawTokenArray;
      tok:T_rawToken;
      m:P_storedMessage;
  begin
    messages:=sandbox^.execute(input,recycler);
    setLength(textOut,0);
    setLength(htmlOut,0);
    setLength(usedBuiltinIDs,0);
    for i:=0 to length(input)-1 do begin
      tmp:=trim(input[i]);
      raw:=rawTokenizeCallback(tmp);
      for tok in raw do if tok.tokType=tt_intrinsicRule then appendIfNew(usedBuiltinIDs,tok.txt);
      if startsWith(tmp,COMMENT_PREFIX) then begin
        append(htmlOut,            StringOfChar(' ',length(getPrefix(mt_echo_input))+1)+toHtmlCode(raw));
        append(textOut,ECHO_MARKER+StringOfChar(' ',length(getPrefix(mt_echo_input))+1)+tmp);
      end else begin
        append(htmlOut,            getPrefix(mt_echo_input)+' '+toHtmlCode(raw));
        append(textOut,ECHO_MARKER+getPrefix(mt_echo_input)+' '+tmp);
      end;
    end;
    for m in messages do begin
      case m^.messageType of
        mt_printline:  for tmp in m^.messageText do append(htmlOut,escapeHtml(tmp));
        mt_echo_input, mt_echo_declaration, mt_el1_note, mt_timing_info: begin end;
        mt_echo_output: for tmp in m^.messageText do append(htmlOut,m^.prefix+' '+toHtmlCode(escapeHtml(tmp)));
        else for tmp in m^.messageText do append(htmlOut,span(C_messageClassMeta[m^.messageClass].htmlSpan,m^.prefix+' '+escapeHtml(tmp)));
      end;
      if not(m^.messageType in [mt_echo_input,mt_timing_info]) then
        for tmp in m^.messageText do append(textOut,C_messageTypeMeta[m^.messageType].guiMarker+m^.prefix+' '+tmp);
    end;
  end;
{$endif}

{$define include_implementation}
{$include funcs_package.inc}

PROCEDURE T_packageReference.loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler; CONST forCodeAssistance:boolean);
  CONST usecase:array[false..true] of T_packageLoadUsecase = (lu_forImport,lu_forCodeAssistance);
  VAR i:longint;
  begin
    with containingPackage^.mainPackage^ do begin
      if containingPackage^.mainPackage^.getCodeProvider^.id=id then begin
        globals.primaryContext.raiseError('Cyclic package dependencies encountered; already loading "'+id+'"',tokenLocation);
        exit;
      end;
      for i:=0 to length(secondaryPackages)-1 do
        if secondaryPackages[i]^.getCodeProvider^.id = id then begin
          if  (secondaryPackages[i]^.readyForUsecase<>lu_NONE) and
              (secondaryPackages[i]^.codeChanged)
          then secondaryPackages[i]^.readyForUsecase:=lu_NONE;
          if secondaryPackages[i]^.readyForUsecase<>lu_beingLoaded then begin
            if secondaryPackages[i]^.readyForUsecase<>usecase[forCodeAssistance] then
            secondaryPackages[i]^.load(usecase[forCodeAssistance],globals,recycler,C_EMPTY_STRING_ARRAY{$ifdef fullVersion},nil,nil{$endif});
            pack:=secondaryPackages[i];
            exit;
          end else begin
            globals.primaryContext.raiseError('Cyclic package dependencies encountered; already loading "'+id+'"',tokenLocation);
            exit;
          end;
        end;
      new(pack,create(newCodeProvider(path),containingPackage^.mainPackage));
      setLength(secondaryPackages,length(secondaryPackages)+1);
      secondaryPackages[length(secondaryPackages)-1]:=pack;
      pack^.load(usecase[forCodeAssistance],globals,recycler,C_EMPTY_STRING_ARRAY{$ifdef fullVersion},nil,nil{$endif});
    end;
  end;

CONSTRUCTOR T_packageReference.create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST messages:P_messages);
  begin
    locationOfDeclaration:=tokenLocation;
    id:=packId;
    path:=locateSource(extractFilePath(root),id);
    if messages<>nil then begin
      if (path='')
      then messages^.raiseSimpleError('Cannot locate package for id "'+id+'"',tokenLocation);
    end;
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

PROCEDURE T_package.interpret(VAR statement:T_enhancedStatement; CONST usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos; CONST functionCallInfos:P_functionCallInfos{$endif});
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
        globals.primaryContext.raiseError('Empty include clause',statement.firstToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
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
        globals.primaryContext.raiseError('Max. extension level exceeded ',locationForErrorFeedback);
        exit;
      end;
      first:=recycler.disposeToken(first);
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
      recycler.cascadeDisposeToken(first);
      new(importWrapper,create(newCodeProvider(helperUse.path),@self));
      setLength(extendedPackages,length(extendedPackages)+1);
      extendedPackages[length(extendedPackages)-1]:=importWrapper;

      helperUse.destroy;
      lexer.create(importWrapper,@self);
      stmt:=lexer.getNextStatement(globals.primaryContext.messages,recycler{$ifdef fullVersion},localIdInfos{$endif});
      inc(extendsLevel);
      while (globals.primaryContext.continueEvaluation) and (stmt.firstToken<>nil) do begin
        interpret(stmt,usecase,globals,recycler{$ifdef fullVersion},localIdInfos,functionCallInfos{$endif});
        stmt:=lexer.getNextStatement(globals.primaryContext.messages,recycler{$ifdef fullVersion},localIdInfos{$endif});
      end;
      if (stmt.firstToken<>nil) then recycler.cascadeDisposeToken(stmt.firstToken);
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
        for i:=0 to length(packageUses)-1 do packageUses[i].loadPackage(@self,locationForErrorFeedback,globals,recycler,usecase=lu_forCodeAssistance);
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
      if statement.firstToken^.next=nil then begin
        globals.primaryContext.raiseError('Empty use clause',statement.firstToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
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
      first:=recycler.disposeToken(first);
      while first<>nil do begin
        j:=-1;
        if first^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule] then begin
          newId:=first^.txt;
          {$ifdef fullVersion}
          if localIdInfos<>nil then localIdInfos^.add(first^.txt,first^.location,clauseEnd,tt_use);
          {$endif}
          if (newId=FORCE_GUI_PSEUDO_PACKAGE) then begin
            if not(gui_started) and (usecase<>lu_forCodeAssistance) then globals.primaryContext.messages^.logGuiNeeded;
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
          if localIdInfos<>nil then localIdInfos^.add(first^.singleTokenToString,first^.location,clauseEnd,tt_use);
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
        first:=recycler.disposeToken(first);
      end;
      if not(globals.primaryContext.messages^.continueEvaluation) then begin
        for i:=0 to length(packageUses)-1 do packageUses[i].destroy;
        setLength(packageUses,0);
      end;
      reloadAllPackages(locationForErrorFeedback);
    end;

  VAR assignmentToken:P_token;

  FUNCTION continueDatastoreDeclaration:boolean;
    begin
      if getCodeProvider^.isPseudoFile then begin
        globals.primaryContext.messages^.raiseSimpleError('data stores require the package to be saved to a file.',statement.firstToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
        result:=false;
      end else result:=true;
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
      ruleDeclarationStart:=statement.firstToken^.location;
      evaluateBody:=(assignmentToken^.tokType=tt_assign);
      ruleBody:=assignmentToken^.next;
      assignmentToken^.next:=nil;
      //plausis:
      if (ruleBody=nil) then begin
        globals.primaryContext.messages^.raiseSimpleError('Missing function body after assignment/declaration token.',assignmentToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      while (statement.firstToken<>nil) and (statement.firstToken^.tokType=tt_modifier) and (C_modifierInfo[statement.firstToken^.getModifier].isRuleModifier) do begin
        include(ruleModifiers,statement.firstToken^.getModifier);
        statement.firstToken:=recycler.disposeToken(statement.firstToken);
      end;
      evaluateBody:=evaluateBody or (modifier_mutable in ruleModifiers);
      if (modifier_datastore in ruleModifiers) and not(continueDatastoreDeclaration) then exit;
      if not(statement.firstToken^.tokType in [tt_identifier, tt_userRule, tt_intrinsicRule]) then begin
        globals.primaryContext.messages^.raiseSimpleError('Declaration does not start with an identifier.',statement.firstToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
        recycler.cascadeDisposeToken(ruleBody);
        exit;
      end;
      p:=statement.firstToken;
      while (p<>nil) and not(p^.tokType in [tt_assign,tt_declare]) do begin
        if (p^.tokType in [tt_identifier, tt_userRule, tt_intrinsicRule]) and isQualified(p^.txt) then begin
          globals.primaryContext.messages^.raiseSimpleError('Declaration head contains qualified ID.',p^.location);
          recycler.cascadeDisposeToken(statement.firstToken);
          recycler.cascadeDisposeToken(ruleBody);
          exit;
        end;
        p:=p^.next;
      end;
      //:plausis
      ruleId:=trim(statement.firstToken^.txt);
      statement.firstToken:=recycler.disposeToken(statement.firstToken);
      if not(statement.firstToken^.tokType in [tt_braceOpen,tt_assign,tt_declare])  then begin
        globals.primaryContext.messages^.raiseSimpleError('Invalid declaration head.',statement.firstToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
        recycler.cascadeDisposeToken(ruleBody);
        exit;
      end;
      rulePattern.create;
      if statement.firstToken^.tokType=tt_braceOpen then rulePattern.parse(statement.firstToken,ruleDeclarationStart,globals.primaryContext,recycler);
      {$ifdef fullVersion}
      if (localIdInfos<>nil) and (ruleBody<>nil) then begin
        ruleDeclarationEnd:=ruleBody^.last^.location;
        for parameterId in rulePattern.getNamedParameters do
          localIdInfos^.add(parameterId.id,parameterId.location,ruleDeclarationEnd,tt_parameterIdentifier);
      end;
      {$endif}

      if statement.firstToken<>nil then begin
        statement.firstToken:=recycler.disposeToken(statement.firstToken);
      end else begin
        globals.primaryContext.messages^.raiseSimpleError('Invalid declaration.',ruleDeclarationStart);
        recycler.cascadeDisposeToken(ruleBody);
        exit;
      end;
      rulePattern.toParameterIds(ruleBody);

      //[marker 1]
      if evaluateBody and
         (usecase<>lu_forCodeAssistance) and
         (globals.primaryContext.messages^.continueEvaluation)
      then globals.primaryContext.reduceExpression(ruleBody,recycler);

      if globals.primaryContext.messages^.continueEvaluation then begin
        metaData.create;
        metaData.setComment(join(statement.comments,C_lineBreakChar));
        metaData.setAttributes(statement.attributes,ruleDeclarationStart,globals.primaryContext.messages);
        formatMetaData(metaData,ruleDeclarationStart,@globals.primaryContext,recycler);
        new(subRule,create(rulePattern,ruleBody,ruleDeclarationStart,modifier_private in ruleModifiers,globals.primaryContext,recycler,metaData));
        ruleMap.declare(ruleId,ruleModifiers,ruleDeclarationStart,globals.primaryContext,metaData,subRule);
      end else recycler.cascadeDisposeToken(ruleBody);
    end;

  PROCEDURE parseDataStore;
    VAR ruleModifiers:T_modifierSet=[];
        loc:T_tokenLocation;
        metaData:T_ruleMetaData;
    begin
      if not(continueDatastoreDeclaration) then exit;
      while (statement.firstToken<>nil) and (statement.firstToken^.tokType=tt_modifier) and (C_modifierInfo[statement.firstToken^.getModifier].isRuleModifier) do begin
        include(ruleModifiers,statement.firstToken^.getModifier);
        loc:=statement.firstToken^.location;
        statement.firstToken:=recycler.disposeToken(statement.firstToken);
      end;
      if (statement.firstToken=nil) or not(statement.firstToken^.tokType in [tt_identifier, tt_userRule, tt_intrinsicRule]) or
         (statement.firstToken^.next<>nil) then begin
        if statement.firstToken<>nil then loc:=statement.firstToken^.location;
        globals.primaryContext.messages^.raiseSimpleError('Invalid datastore definition: '+tokensToString(statement.firstToken),loc);
        recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      metaData.create;
      metaData.setComment(join(statement.comments,C_lineBreakChar));
      metaData.setAttributes(statement.attributes,statement.firstToken^.location,globals.primaryContext.messages);
      formatMetaData(metaData,statement.firstToken^.location,@globals.primaryContext,recycler);
      ruleMap.declare(statement.firstToken^.txt,
                      ruleModifiers,
                      statement.firstToken^.location,
                      globals.primaryContext,
                      metaData,
                      nil);
    end;

  FUNCTION getDeclarationOrAssignmentToken: P_token;
    VAR level:longint=0;
        t,newNext:P_token;
        hasDeclareToken:boolean=false;
        hasAssignToken :boolean=false;

    begin
      t:=statement.firstToken;
      while (t<>nil) do begin
        if (t^.tokType=tt_iifElse) and (t^.next<>nil) and (t^.next^.tokType=tt_identifier) then resolveId(t^.next^,globals.primaryContext.messages);
        if (t^.tokType=tt_iifElse) and (t^.next<>nil) and (t^.next^.tokType in [tt_type,tt_customType]) then begin
          newNext:=t^.next^.next;
          if t^.next^.tokType=tt_type
          then t^.tokType:=tt_typeCheck
          else t^.tokType:=tt_customTypeCheck;
          t^.txt    :=t^.next^.txt;
          t^.data   :=t^.next^.data;
          {$ifdef fullVersion}
          ruleMap.markTypeAsUsed(t,functionCallInfos);
          {$endif}
          recycler.disposeToken(t^.next);
          t^.next:=newNext;
        end;
        if t^.tokType      in C_openingBrackets then inc(level)
        else if t^.tokType in C_closingBrackets then dec(level)
        else if t^.tokType in [tt_assign,tt_declare] then begin
          if (level=0) then exit(t);
          hasDeclareToken:=hasDeclareToken or (t^.tokType=tt_declare);
          hasAssignToken :=hasAssignToken  or (t^.tokType=tt_assign );
        end;
        t:=t^.next;
      end;
      result:=nil;
    end;

  begin
    profile:=globals.primaryContext.messages^.isCollecting(mt_timing_info) and (usecase in [lu_forDirectExecution,lu_forCallingMain]);
    if statement.firstToken=nil then exit;
    if usecase=lu_forCodeAssistance then globals.primaryContext.messages^.clearFlags;

    if (usecase<>lu_forCodeAssistance) and not(globals.primaryContext.messages^.continueEvaluation) then begin
      recycler.cascadeDisposeToken(statement.firstToken);
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
      if not(se_alterPackageState in globals.primaryContext.sideEffectWhitelist) then begin
        globals.primaryContext.messages^.raiseSimpleError('Rule declaration is not allowed here',assignmentToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      {$ifdef fullVersion}
      globals.primaryContext.callStackPushCategory(@self,pc_declaration,pseudoCallees);
      {$endif}
      if profile then globals.timeBaseComponent(pc_declaration);
      if assignmentToken^.next=nil then begin
        globals.primaryContext.messages^.raiseSimpleError('Missing rule body',assignmentToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      if usecase=lu_forCodeAssistance then begin
        //check, but ignore result
        assignmentToken^.next^.areBracketsPlausible(globals.primaryContext.messages);
        globals.primaryContext.messages^.clearFlags;
      end else if not(assignmentToken^.next^.areBracketsPlausible(globals.primaryContext.messages)) then begin
        recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      predigest(assignmentToken,@self,globals.primaryContext.messages,recycler{$ifdef fullVersion},functionCallInfos{$endif});
      if globals.primaryContext.messages^.isCollecting(mt_echo_declaration) then globals.primaryContext.messages^.postTextMessage(mt_echo_declaration,C_nilTokenLocation,tokensToString(statement.firstToken)+';');
      parseRule;
      if profile then globals.timeBaseComponent(pc_declaration);
      {$ifdef fullVersion}
      globals.primaryContext.callStackPop(nil);
      {$endif}
    end else if statement.firstToken^.tokType=tt_modifier then begin
      if not(se_alterPackageState in globals.primaryContext.sideEffectWhitelist) then begin
        globals.primaryContext.messages^.raiseSimpleError('Datastore declaration is not allowed here',statement.firstToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      {$ifdef fullVersion}
      globals.primaryContext.callStackPushCategory(@self,pc_declaration,pseudoCallees);
      {$endif}
      if profile then globals.timeBaseComponent(pc_declaration);
      if globals.primaryContext.messages^.isCollecting(mt_echo_declaration) then globals.primaryContext.messages^.postTextMessage(mt_echo_declaration,C_nilTokenLocation,tokensToString(statement.firstToken)+';');
      parseDataStore;
      if profile then globals.timeBaseComponent(pc_declaration);
      {$ifdef fullVersion}
      globals.primaryContext.callStackPop(nil);
      {$endif}
    end else if globals.primaryContext.messages^.continueEvaluation then begin
      case usecase of
        lu_forDirectExecution:begin
          customOperatorRules:=ruleMap.getOperators;
          {$ifdef fullVersion}
          globals.primaryContext.callStackPushCategory(@self,pc_interpretation,pseudoCallees);
          {$endif}
          if profile then globals.timeBaseComponent(pc_interpretation);
          if not ((statement.firstToken<>nil) and statement.firstToken^.areBracketsPlausible(globals.primaryContext.messages)) then begin
            recycler.cascadeDisposeToken(statement.firstToken);
            exit;
          end;
          predigest(statement.firstToken,@self,globals.primaryContext.messages,recycler{$ifdef fullVersion},functionCallInfos{$endif});
          if globals.primaryContext.messages^.isCollecting(mt_echo_input) then globals.primaryContext.messages^.postTextMessage(mt_echo_input,C_nilTokenLocation,tokensToString(statement.firstToken)+';');
          globals.primaryContext.reduceExpression(statement.firstToken,recycler);
          if profile then globals.timeBaseComponent(pc_interpretation);
          {$ifdef fullVersion}
          globals.primaryContext.callStackPop(nil);
          {$endif}
          if (statement.firstToken<>nil) and globals.primaryContext.messages^.isCollecting(mt_echo_output) then begin
            {$ifdef fullVersion}
            if (statement.firstToken<>nil) and
               (statement.firstToken^.next=nil) and
               (statement.firstToken^.tokType=tt_literal) and
               (globals.primaryContext.messages^.preferredEchoLineLength>10) then begin
              globals.primaryContext.messages^.postTextMessage(mt_echo_output,C_nilTokenLocation,
                serializeToStringList(P_literal(statement.firstToken^.data),
                                      statement.firstToken^.location,
                                      nil,
                                      globals.primaryContext.messages^.preferredEchoLineLength));
            end else {$endif}
              globals.primaryContext.messages^.postTextMessage(mt_echo_output,C_nilTokenLocation,tokensToString(statement.firstToken));
          end;
        end;
        lu_forCodeAssistance: if (statement.firstToken<>nil) and statement.firstToken^.areBracketsPlausible(globals.primaryContext.messages) then begin
          predigest(statement.firstToken,@self,globals.primaryContext.messages,recycler{$ifdef fullVersion},functionCallInfos{$endif});
          resolveBuiltinIDs(statement.firstToken,globals.primaryContext.messages);
        end
        else globals.primaryContext.messages^.postTextMessage(mt_el1_note,statement.firstToken^.location,'Skipping expression '+tokensToString(statement.firstToken,50));
      end;
    end;
    if statement.firstToken<>nil then recycler.cascadeDisposeToken(statement.firstToken);
    statement.firstToken:=nil;
    if usecase=lu_forCodeAssistance then globals.primaryContext.messages^.clearFlags;
  end;

PROCEDURE T_package.load(usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler; CONST mainParameters:T_arrayOfString{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos; CONST functionCallInfos:P_functionCallInfos{$endif});
  VAR profile:boolean=false;
  PROCEDURE executeMain;
    VAR mainRule:P_rule;
        parametersForMain:P_listLiteral=nil;
        t:P_token=nil;
        dummy:P_token=nil;
        i:longint;
        {$ifdef fullVersion}displayedHelp:boolean=false;{$endif}
    begin
      if not(readyForUsecase=lu_forCallingMain) or not(globals.primaryContext.messages^.continueEvaluation) then exit;
      mainRule:=ruleMap.getLocalMain;
      if mainRule=nil
      then globals.primaryContext.messages^.raiseSimpleError('The specified package contains no main rule.',packageTokenLocation(@self))
      else begin
        parametersForMain:=newListLiteral(length(mainParameters));
        for i:=0 to length(mainParameters)-1 do parametersForMain^.appendString(mainParameters[i]);

        {$ifdef fullVersion}
        globals.primaryContext.callStackPushCategory(@self,pc_interpretation,pseudoCallees);
        {$endif}
        if profile then globals.timeBaseComponent(pc_interpretation);

        if mainRule^.replaces(packageTokenLocation(@self),parametersForMain,t,dummy,@globals.primaryContext,recycler)
        then globals.primaryContext.reduceExpression(t,recycler)
        else if (length(mainParameters)=1) and (mainParameters[0]='-h') then begin
          globals.primaryContext.messages^.postTextMessage(mt_printline,C_nilTokenLocation,split(getHelpOnMain));
          {$ifdef fullVersion}displayedHelp:=true;{$endif}
        end else begin
          globals.primaryContext.raiseCannotApplyError('user defined rule '+mainRule^.getId,
                                        parametersForMain,
                                        mainRule^.getLocation,
                                        C_lineBreakChar+join(mainRule^.getCmdLineHelpText,C_lineBreakChar),true);
          if (length(mainParameters)=1) and (mainParameters[0]='-h') then globals.primaryContext.messages^.postTextMessage(mt_printline,C_nilTokenLocation,split(getHelpOnMain));
        end;
        if profile then globals.timeBaseComponent(pc_interpretation);
        {$ifdef fullVersion}
        globals.primaryContext.callStackPop(nil);
        {$endif}

        {$ifdef fullVersion}
        //error handling if main returns more than one token:------------------
        if not(displayedHelp) and ((t=nil) or (t^.next<>nil)) and (FlagGUINeeded in globals.primaryContext.messages^.getFlags)
          then globals.primaryContext.messages^.postTextMessage(mt_el1_note,packageTokenLocation(@self),'Evaluation requires GUI-startup. Re-evaluating.');
        //------------------:error handling if main returns more than one token
        {$endif}
        recycler.cascadeDisposeToken(t);
        disposeLiteral(parametersForMain);
        parametersForMain:=nil;
      end;
    end;

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

  {$ifdef fullVersion}
  PROCEDURE afterLoadForAssistance;
    PROCEDURE checkParameters;
      VAR rule:P_rule;
          pack:P_package;
      begin
        for rule in ruleMap.getAllLocalRules do
          rule^.checkParameters(globals.primaryContext);
        for pack in secondaryPackages do
          for rule in pack^.ruleMap.getAllLocalRules do
            rule^.checkParameters(globals.primaryContext);
      end;

    begin
      customOperatorRules:=ruleMap.getOperators;
      ruleMap.resolveRuleIds(globals.primaryContext.messages,ON_EVALUATION);
      complainAboutUnused(globals.primaryContext.messages,functionCallInfos);
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

    if profile then globals.timeBaseComponent(pc_tokenizing);
    lexer.create(@self);
    newCodeHash:=getCodeProvider^.stateHash;
    if profile then globals.timeBaseComponent(pc_tokenizing);
    stmt:=lexer.getNextStatement(globals.primaryContext.messages,recycler{$ifdef fullVersion},localIdInfos{$endif});
    isPlainScript:=isPlainScriptStatement;
    if isPlainScript then begin
      case usecase of
        lu_forImport         : globals.primaryContext.messages^.raiseSimpleError('Cannot import package declared as "plain script"',stmt.firstToken^.location);
        lu_forCallingMain    : usecase:=lu_forDirectExecution;
      end;
      commentOnPlainMain:=join(stmt.comments,C_lineBreakChar);
      recycler.cascadeDisposeToken(stmt.firstToken);
      stmt:=lexer.getNextStatement(globals.primaryContext.messages,recycler{$ifdef fullVersion},localIdInfos{$endif});
    end;
    if profile then globals.timeBaseComponent(pc_tokenizing);

    while ((usecase=lu_forCodeAssistance) or (globals.primaryContext.messages^.continueEvaluation)) and (stmt.firstToken<>nil) do begin
      interpret(stmt,usecase,globals,recycler{$ifdef fullVersion},localIdInfos,functionCallInfos{$endif});
      if profile then globals.timeBaseComponent(pc_tokenizing);
      stmt:=lexer.getNextStatement(globals.primaryContext.messages,recycler{$ifdef fullVersion},localIdInfos{$endif});
      if profile then globals.timeBaseComponent(pc_tokenizing);
    end;
    if (stmt.firstToken<>nil) then recycler.cascadeDisposeToken(stmt.firstToken);
    lexer.destroy;
    if usecase=lu_forCodeAssistance then begin
      readyForUsecase:=usecase;
      logReady(newCodeHash);
      {$ifdef fullVersion}
      afterLoadForAssistance;
      {$endif}
      exit;
    end;
    if globals.primaryContext.messages^.continueEvaluation then begin
      customOperatorRules:=ruleMap.getOperators;
      readyForUsecase:=usecase;
      logReady(newCodeHash);
      if usecase=lu_forCallingMain
      then executeMain
      else if isMain and isPlainScript and (length(mainParameters)=1) and (mainParameters[0]='-h')
      then globals.primaryContext.messages^.postTextMessage(mt_printline,C_nilTokenLocation,split(getHelpOnMain));
    end else begin
      readyForUsecase:=lu_NONE;
      if isMain and
         isPlainScript and
         not(globals.primaryContext.continueEvaluation) and
         not(FlagGUINeeded in globals.primaryContext.messages^.getFlags)
      then globals.primaryContext.messages^.postTextMessage(mt_printline,C_nilTokenLocation,split(getHelpOnMain));
    end;

    if isMain and (usecase in [lu_forDirectExecution,lu_forCallingMain])
    then finalize(globals.primaryContext,recycler);
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

PROCEDURE T_package.writeDataStores(CONST messages:P_messages; CONST recurse:boolean);
  VAR i:longint;
  begin
    ruleMap.writeBackDatastores(messages);
    if recurse then for i:=0 to length(packageUses)-1 do packageUses[i].pack^.writeDataStores(messages,recurse);
  end;

PROCEDURE T_package.finalize(VAR context:T_context; VAR recycler:T_recycler);
  VAR i:longint;
  begin
    ruleMap.executeAfterRules(context,recycler);
    for i:=0 to length(packageUses)-1 do packageUses[i].pack^.finalize(context,recycler);
    funcs_server.onPackageFinalization(@self);
    funcs_ipc   .onPackageFinalization(@self);
    funcs_format.onPackageFinalization(@self);
    ruleMap.writeBackDatastores(context.messages);
    if isMain then context.getGlobals^.stopWorkers(recycler);
  end;

FUNCTION T_package.literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; VAR recycler:T_recycler):string;
  VAR toStringRule:T_ruleMapEntry;
      toReduce,dummy:P_token;
      parameters:P_listLiteral;
      stringOut:P_literal=nil;
  begin
    if ruleMap.containsKey('toString',toStringRule) and (toStringRule.entryType=tt_userRule)
    then begin
      parameters:=P_listLiteral(newListLiteral(1)^.append(L,true));
      if P_rule(toStringRule.value)^.replaces(location,parameters,toReduce,dummy,context,recycler)
      then stringOut:=P_context(context)^.reduceToLiteral(toReduce,recycler).literal;
      disposeLiteral(parameters);
    end;

    if stringOut=nil then begin
      if (L^.literalType=lt_string)
      then result:=P_stringLiteral(L)^.value
      else result:=L^.toString();
    end else begin
      if stringOut^.literalType=lt_string
      then result:=P_stringLiteral(stringOut)^.value
      else result:=stringOut^.toString();
      disposeLiteral(stringOut);
    end;
  end;

FUNCTION T_package.getTypeMap:T_typeMap;
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
PROCEDURE T_package.complainAboutUnused(CONST messages:P_messages; CONST functionCallInfos:P_functionCallInfos);
  VAR import:T_packageReference;
  begin
    if functionCallInfos=nil then exit;
    ruleMap.complainAboutUnused(messages,functionCallInfos);
    for import in packageUses do if not(import.supressUnusedWarning) and not(functionCallInfos^.isPackageReferenced(import.pack)) then
      messages^.postTextMessage(mt_el2_warning,import.locationOfDeclaration,'Unused import '+import.pack^.getId+' ('+import.pack^.getPath+')');
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
          variableReport.addEntry(filenameToPackageId(rule^.getLocation.package^.getPath)+'.'+rule^.getId,value,true);
        end;
      end;
    end;

  VAR entry:T_ruleMapEntry;
  begin
    for entry in ruleMap.valueSet do addRule(entry);
  end;
{$endif}

FUNCTION T_package.inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; VAR recycler:T_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_functionCallInfos{$endif}):P_mapLiteral;
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

  {$ifdef fullVersion}
  FUNCTION builtinCallList:P_listLiteral;
    VAR builtin:T_builtinFunctionMetaData;
    begin
      result:=newListLiteral();
      for builtin in functionCallInfos^.calledBuiltinFunctions do result^.appendString(builtin.qualifiedId);
    end;

  FUNCTION customCallList:P_listLiteral;
    VAR userDef:P_objectWithIdAndLocation;
        qualifier:string;
    begin
      result:=newListLiteral();
      for userDef in functionCallInfos^.calledCustomFunctions do begin
        if userDef^.getLocation.package=@self
        then qualifier:=''
        else qualifier:=userDef^.getLocation.package^.getId+ID_QUALIFY_CHARACTER;
        result^.append(newListLiteral(2)^.appendString(qualifier+userDef^.getId)^.appendString(userDef^.getLocation),false);
      end;
    end;
  {$endif}

  begin
    {$ifdef fullVersion}
    if (functionCallInfos=nil) or functionCallInfos^.isEmpty then begin
      if functionCallInfos=nil then new(functionCallInfos,create);
      ruleMap.fillCallInfos(functionCallInfos);
    end;
    {$endif}

    result:=newMapLiteral(7)^.put('id'      ,getId)^
                             .put('path'    ,getPath)^
                             .put('source'  ,join(getCodeProvider^.getLines,C_lineBreakChar))^
                             .put('uses'    ,usesList,false)^
                             .put('includes',includeList,false)^
                             .put('declares',ruleMap.inspect(P_context(context)^,recycler,includeRulePointer),false)^
                             .put('plain script',newBoolLiteral(isPlainScript),false);
    {$ifdef fullVersion}
    functionCallInfos^.cleanup;
    result^.put('called builtin',builtinCallList,false)
          ^.put('used rules',customCallList,false);
    dispose(functionCallInfos,destroy);
    functionCallInfos:=nil;
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

PROCEDURE T_package.resolveId(VAR token: T_token; CONST messagesOrNil:P_messages{$ifdef fullVersion};CONST markAsUsed:boolean=true{$endif});
  VAR intrinsicFuncPtr:P_intFuncCallback;
      entry:T_ruleMapEntry;
  begin
    if ruleMap.containsKey(token.txt,entry) then begin
      token.tokType:=entry.entryType;
      token.data   :=entry.value;
      exit;
    end;
    if intrinsicRuleMap.containsKey(token.txt,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRule;
      token.data:=intrinsicFuncPtr;
      exit;
    end;
    if messagesOrNil<>nil then messagesOrNil^.raiseSimpleError('Cannot resolve ID "'+token.txt+'"',token.location);
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

FUNCTION T_package.usedAndExtendedPackages:T_arrayOfString;
  VAR ref:T_packageReference;
      ext:P_extendedPackage;
  begin
    setLength(result,0);
    for ref in packageUses      do append(result,ref.path);
    for ext in extendedPackages do append(result,ext^.getPath);
    sortUnique(result);
    dropValues(result,'');
  end;

{$endif}

{$undef include_implementation}
INITIALIZATION
  newCodeProvider:=@fileWrappers.newFileCodeProvider;
  setupSandboxes;
{$define include_initialization}
  {$ifdef fullVersion}
  demoCodeToHtmlCallback:=@demoCallToHtml;
  {$endif}
  {$include funcs_package.inc}
{$undef include_initialization}

FINALIZATION
  doneSandboxes;
end.
