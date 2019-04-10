UNIT packages;
INTERFACE
USES //basic classes
     sysutils,typinfo, FileUtil, Classes,
     //my utilities:
     myGenerics, myStringUtil,
     //MNH:
     mnh_constants, basicTypes,
     fileWrappers,
     mnh_messages,
     out_adapters,
     litVar,
     caches,
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
     funcs_mnh,   funcs_types, funcs_math,  funcs_strings,
     funcs_list,  funcs_system, funcs_files,
     funcs_format,
     funcs_regex, funcs_xml, funcs_ipc, funcs_server,
     builtinGenerators,
     patterns,
     subrules,
     rules,
     recyclers,
     tokenArray;

{$define include_interface}
TYPE
  P_package=^T_package;
  T_ruleMap=specialize G_stringKeyMap<P_rule>;
  T_packageLoadUsecase=(lu_NONE,lu_beingLoaded,lu_forImport,lu_forCallingMain,lu_forDirectExecution,lu_forCodeAssistance);
  T_ruleSorting=(rs_none,rs_byNameCaseSensitive,rs_byNameCaseInsensitive,rs_byLocation);

  T_packageReference=object
    id,path:ansistring;
    pack:P_package;
    locationOfDeclaration:T_tokenLocation;
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

      runAfter:array of P_subruleExpression;

      isPlainScript:boolean;
      commentOnPlainMain:ansistring;

      packageUses:array of T_packageReference;
      readyForUsecase:T_packageLoadUsecase;
      {$ifdef fullVersion}
      pseudoCallees:T_packageProfilingCalls;
      anyCalled:boolean;
      suppressAllUnusedWarnings:boolean;
      {$endif}

      PROCEDURE resolveRuleIds(CONST messages:P_messages);
      FUNCTION ensureRuleId(CONST ruleId:T_idString; CONST modifiers:T_modifierSet; CONST ruleDeclarationStart:T_tokenLocation; CONST messages:P_messages; VAR metaData:T_ruleMetaData; OUT newRuleCreated:boolean):P_rule;
      PROCEDURE writeDataStores(CONST messages:P_messages; CONST recurse:boolean);
      public
      PROCEDURE interpret(VAR statement:T_enhancedStatement; CONST usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos=nil{$endif});
      private
      FUNCTION isMain:boolean;
      {$ifdef fullVersion}
      PROCEDURE complainAboutUnused(CONST messages:P_messages);
      {$endif}
    protected
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
    public
      packageRules,importedRules:T_ruleMap;
      PROCEDURE clear(CONST includeSecondaries:boolean);
      CONSTRUCTOR create(CONST provider:P_codeProvider; CONST mainPackage_:P_package);
      FUNCTION getSecondaryPackageById(CONST id:ansistring):ansistring;
      PROCEDURE load(usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler; CONST mainParameters:T_arrayOfString{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;

      FUNCTION getHelpOnMain:ansistring;
      PROCEDURE finalize(VAR context:T_context; VAR recycler:T_recycler);
      PROCEDURE resolveId(VAR token:T_token; CONST messagesOrNil:P_messages{$ifdef fullVersion};CONST markAsUsed:boolean=true{$endif}); virtual;
      FUNCTION getTypeMap:T_typeMap; virtual;
      FUNCTION literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; VAR recycler:T_recycler):string; virtual;
      FUNCTION inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; VAR recycler:T_recycler):P_mapLiteral; virtual;
      {$ifdef fullVersion}
      PROCEDURE updateLists(VAR userDefinedRules:T_setOfString; CONST forCompletion:boolean);
      FUNCTION getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
      PROCEDURE reportVariables(VAR variableReport:T_variableTreeEntryCategoryNode);
      FUNCTION declaredRules(CONST ruleSorting:T_ruleSorting):T_ruleList;
      FUNCTION usedPackages:T_packageList;
      FUNCTION getImport(CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION getExtended(CONST idOrPath:string):P_abstractPackage; virtual;
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
      FUNCTION loadForCodeAssistance(VAR packageToInspect:T_package; VAR recycler:T_recycler):T_storedMessages;
      FUNCTION runScript(CONST filenameOrId:string; CONST mainParameters:T_arrayOfString; CONST locationForWarning:T_tokenLocation; CONST callerContext:P_context; VAR recycler:T_recycler;  CONST connectLevel:byte; CONST enforceDeterminism:boolean):P_literal;
      {$ifdef fullVersion}
      PROCEDURE runInstallScript;
      PROCEDURE runUninstallScript;
      {$endif}
  end;

FUNCTION packageFromCode(CONST code:T_arrayOfString; CONST nameOrPseudoName:string):P_package;
FUNCTION sandbox:P_sandbox;
{$undef include_interface}
VAR newCodeProvider:F_newCodeProvider;
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
    package.destroy;
    globals.destroy;
    messages.destroy;
    {$ifdef fullVersion}
    plotSystem.destroy;
    {$endif}
    leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

FUNCTION T_sandbox.execute(CONST input: T_arrayOfString; VAR recycler:T_recycler; CONST randomSeed: dword): T_storedMessages;
  begin
    messages.clear;
    messages.setupMessageRedirection(nil,[]);
    package.replaceCodeProvider(newVirtualFileCodeProvider('?',input));
    globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}ect_silent,C_EMPTY_STRING_ARRAY,recycler);
    if randomSeed<>4294967295 then globals.prng.resetSeed(randomSeed);
    package.load(lu_forDirectExecution,globals,recycler,C_EMPTY_STRING_ARRAY);
    globals.afterEvaluation(recycler);
    result:=messages.storedMessages(false);
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
  end;

FUNCTION T_sandbox.loadForCodeAssistance(VAR packageToInspect:T_package; VAR recycler:T_recycler):T_storedMessages;
  VAR errorHolder:T_messagesErrorHolder;
      m:P_storedMessage;
  begin
    errorHolder.createErrorHolder(nil,C_errorsAndWarnings);
    globals.primaryContext.messages:=@errorHolder;
    globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}ect_silent,C_EMPTY_STRING_ARRAY,recycler);
    packageToInspect.load(lu_forCodeAssistance,globals,recycler,C_EMPTY_STRING_ARRAY);
    globals.afterEvaluation(recycler);
    result:=errorHolder.storedMessages(true);
    for m in result do m^.rereferenced;
    errorHolder.destroy;
    globals.primaryContext.messages:=@messages;
    enterCriticalSection(cs); busy:=false; leaveCriticalSection(cs);
  end;

FUNCTION T_sandbox.runScript(CONST filenameOrId:string; CONST mainParameters:T_arrayOfString; CONST locationForWarning:T_tokenLocation; CONST callerContext:P_context; VAR recycler:T_recycler; CONST connectLevel:byte; CONST enforceDeterminism:boolean):P_literal;
  CONST TYPES_BY_LEVEL:array[0..3] of T_messageTypeSet=
        ([],
         [mt_clearConsole,mt_printline,mt_printdirect],
         [mt_clearConsole,mt_printline,mt_printdirect,mt_el1_note..mt_el2_userWarning],
         [mt_clearConsole,mt_printline,mt_printdirect,mt_el1_note..mt_el2_userWarning,mt_el3_evalError..mt_el4_systemError]);

  VAR fileName:string='';
      callContextType:T_evaluationContextType;
  begin
    if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
    then fileName:=expandFileName(filenameOrId)
    else fileName:=locateSource(extractFilePath(locationForWarning.package^.getPath),filenameOrId);
    if (fileName='') or not(fileExists(fileName)) then begin
      callerContext^.messages^.postTextMessage(mt_el2_warning,locationForWarning,'Cannot find script with id or path "'+filenameOrId+'"');
      exit(nil);
    end;
    if connectLevel=0 then callContextType:=ect_silent
                      else callContextType:=ect_normal;
    messages.clear;
    messages.setupMessageRedirection(callerContext^.messages,TYPES_BY_LEVEL[connectLevel]);

    if enforceDeterminism then globals.prng.resetSeed(0);
    package.replaceCodeProvider(newCodeProvider(fileName));
    try
      globals.resetForEvaluation({$ifdef fullVersion}@package,@package.reportVariables,{$endif}callContextType,mainParameters,recycler);
      package.load(lu_forCallingMain,globals,recycler,mainParameters);
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
  begin recycler.initRecycler; execute(ensureAssoc_mnh,recycler); recycler.cleanup; end;

PROCEDURE T_sandbox.runUninstallScript;
  {$i res_removeAssoc.inc}
  VAR recycler:T_recycler;
  begin recycler.initRecycler; execute(removeAssoc_mnh,recycler); recycler.cleanup; end;

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
      for i:=0 to length(secondaryPackages)-1 do
        if secondaryPackages[i]^.getCodeProvider^.id = id then begin
          if  (secondaryPackages[i]^.readyForUsecase<>lu_NONE) and
              (secondaryPackages[i]^.codeChanged)
          then secondaryPackages[i]^.readyForUsecase:=lu_NONE;
          if secondaryPackages[i]^.readyForUsecase<>lu_beingLoaded then begin
            if secondaryPackages[i]^.readyForUsecase<>usecase[forCodeAssistance] then
            secondaryPackages[i]^.load(usecase[forCodeAssistance],globals,recycler,C_EMPTY_STRING_ARRAY);
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
      pack^.load(usecase[forCodeAssistance],globals,recycler,C_EMPTY_STRING_ARRAY);
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

PROCEDURE T_package.interpret(VAR statement:T_enhancedStatement; CONST usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos=nil{$endif});
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
      if (first^.next=nil) and (first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule]) then begin
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
        interpret(stmt,usecase,globals,recycler);
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
          rulesSet:T_ruleMap.KEY_VALUE_LIST;
          dummyRule:P_rule;
          {$ifdef fullVersion}
          attribute:string;
          suppressUnusedImport:boolean=false;
          {$endif}
      begin
        {$ifdef fullVersion}
        globals.primaryContext.callStackPushCategory(@self,pc_importing,pseudoCallees);
        for attribute in statement.attributes do if startsWith(attribute,SUPPRESS_UNUSED_WARNING_ATTRIBUTE) then suppressUnusedImport:=true;
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
        if globals.primaryContext.continueEvaluation then for i:=length(packageUses)-1 downto 0 do begin
          rulesSet:=packageUses[i].pack^.packageRules.entrySet;
          for j:=0 to length(rulesSet)-1 do if rulesSet[j].value^.hasPublicSubrule then begin
            if not(importedRules.containsKey(rulesSet[j].key,dummyRule))
            then importedRules.put(rulesSet[j].key,rulesSet[j].value);
            importedRules.put(packageUses[i].id+ID_QUALIFY_CHARACTER+rulesSet[j].key,rulesSet[j].value);
          end;
        end;
        for i:=0 to length(packageUses)-1 do begin
          if mergeCustomOps(packageUses[i].pack,globals.primaryContext.messages) {$ifdef fullVersion} or suppressUnusedImport {$endif}
          then {$ifdef fullVersion} packageUses[i].pack^.anyCalled:=true; {$else} begin end;{$endif}
        end;
      end;

    begin
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
        if first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule] then begin
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
          end;
        end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_string) then begin
          {$ifdef fullVersion}
          if localIdInfos<>nil then localIdInfos^.add(first^.singleTokenToString,first^.location,clauseEnd,tt_use);
          {$endif}
          newId:=P_stringLiteral(first^.data)^.value;
          j:=length(packageUses);
          setLength(packageUses,j+1);
          packageUses[j].createWithSpecifiedPath(newId,first^.location,globals.primaryContext.messages);
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
        ruleGroup:P_rule=nil;
        inlineValue:P_literal;
        newRuleCreated:boolean=false;
    PROCEDURE addRuleToRunAfter(CONST ex:P_subruleExpression);
      begin
        if not(ex^.canApplyToNumberOfParameters(0)) then begin
          globals.primaryContext.messages^.raiseSimpleError('Attribute //@'+EXECUTE_AFTER_ATTRIBUTE+' is only allowed for nullary functions',ex^.getLocation);
          exit;
        end;
        setLength(runAfter,length(runAfter)+1);
        runAfter[length(runAfter)-1]:=ex;
        ex^.rereference;
      end;

    PROCEDURE declareTypeCastRule;
      VAR castRule:P_typeCastRule;
          otherRule:P_rule;
      begin
        if not(P_typeCheckRule(ruleGroup)^.castRuleIsValid) then exit;
        new(castRule,create(P_typeCheckRule(ruleGroup)^.getTypedef,P_typeCheckRule(ruleGroup)));
        if packageRules.containsKey(castRule^.getId,otherRule) then begin
          globals.primaryContext.messages^.raiseSimpleError(
            'Cannot declare implicit typecast rule '+castRule^.getId+C_lineBreakChar+
            'because a rule of the same name already exists '+ansistring(otherRule^.getLocation)+C_lineBreakChar+
            'Please overload the implicit typecast rule after the type definition',
          ruleGroup^.getLocation);
          dispose(castRule,destroy);
        end else packageRules.put(castRule^.getId,castRule);
      end;

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
      if not(statement.firstToken^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule, tt_customTypeRule]) then begin
        globals.primaryContext.messages^.raiseSimpleError('Declaration does not start with an identifier.',statement.firstToken^.location);
        recycler.cascadeDisposeToken(statement.firstToken);
        recycler.cascadeDisposeToken(ruleBody);
        exit;
      end;
      p:=statement.firstToken;
      while (p<>nil) and not(p^.tokType in [tt_assign,tt_declare]) do begin
        if (p^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule]) and isQualified(p^.txt) then begin
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
      if evaluateBody and (usecase<>lu_forCodeAssistance) and (globals.primaryContext.messages^.continueEvaluation) then globals.primaryContext.reduceExpression(ruleBody,recycler);

      if globals.primaryContext.messages^.continueEvaluation then begin
        metaData.create;
        metaData.setComment(join(statement.comments,C_lineBreakChar));
        metaData.setAttributes(statement.attributes,ruleDeclarationStart,globals.primaryContext.messages);
        ruleGroup:=ensureRuleId(ruleId,ruleModifiers,ruleDeclarationStart,globals.primaryContext.messages,metaData,newRuleCreated);

        if (globals.primaryContext.messages^.continueEvaluation) and (ruleGroup^.getRuleType in C_mutableRuleTypes) and not(rulePattern.isValidMutablePattern)
        then globals.primaryContext.messages^.raiseSimpleError('Mutable rules are quasi variables and must therefore not accept any arguments',ruleDeclarationStart);
        if globals.primaryContext.messages^.continueEvaluation then begin
          new(subRule,create(ruleGroup,rulePattern,ruleBody,ruleDeclarationStart,modifier_private in ruleModifiers,globals.primaryContext,recycler,metaData));
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
            {$ifdef fullVersion}
            if P_mutableRule(ruleGroup)^.metaData.hasAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE) then begin
              if P_mutableRule(ruleGroup)^.metaData.getAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE).value=SUPPRESS_ALL_UNUSED_VALUE then suppressAllUnusedWarnings:=true;
              if (modifier_private in ruleModifiers)
              then globals.primaryContext.messages^.postTextMessage(mt_el2_warning,ruleDeclarationStart,'Attribute '+SUPPRESS_UNUSED_WARNING_ATTRIBUTE+' is ignored for private rules')
              else ruleGroup^.setIdResolved;
            end else if suppressAllUnusedWarnings then P_mutableRule(ruleGroup)^.metaData.addSuppressUnusedWarningAttribute;
            {$endif}
            dispose(subRule,destroy);
          end else begin
            if subRule^.metaData.hasAttribute(EXECUTE_AFTER_ATTRIBUTE) then addRuleToRunAfter(subRule);
            {$ifdef fullVersion}
            if subRule^.metaData.getAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE).value=SUPPRESS_ALL_UNUSED_VALUE
            then suppressAllUnusedWarnings:=true
            else if suppressAllUnusedWarnings then subRule^.metaData.addSuppressUnusedWarningAttribute;
            {$endif}
            P_ruleWithSubrules(ruleGroup)^.addOrReplaceSubRule(subRule,globals.primaryContext);
            if (P_ruleWithSubrules(ruleGroup)^.getRuleType in [rt_customTypeCheck,rt_duckTypeCheck]) and globals.primaryContext.messages^.continueEvaluation then declareTypeCastRule;
          end;
          statement.firstToken:=nil;
        end else begin
          recycler.cascadeDisposeToken(statement.firstToken);
          recycler.cascadeDisposeToken(ruleBody);
        end;
      end else begin
        recycler.cascadeDisposeToken(statement.firstToken);
        recycler.cascadeDisposeToken(ruleBody);
      end;
      if newRuleCreated and not(globals.primaryContext.messages^.continueEvaluation) then packageRules.dropKey(ruleId);
    end;

  PROCEDURE parseDataStore;
    VAR ruleModifiers:T_modifierSet=[];
        loc:T_tokenLocation;
        metaData:T_ruleMetaData;
        newRuleCreated:boolean;
    begin
      if not(continueDatastoreDeclaration) then exit;
      while (statement.firstToken<>nil) and (statement.firstToken^.tokType=tt_modifier) and (C_modifierInfo[statement.firstToken^.getModifier].isRuleModifier) do begin
        include(ruleModifiers,statement.firstToken^.getModifier);
        loc:=statement.firstToken^.location;
        statement.firstToken:=recycler.disposeToken(statement.firstToken);
      end;
      if (statement.firstToken=nil) or not(statement.firstToken^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule]) or
         (statement.firstToken^.next<>nil) then begin
        if statement.firstToken<>nil then loc:=statement.firstToken^.location;
        globals.primaryContext.messages^.raiseSimpleError('Invalid datastore definition: '+tokensToString(statement.firstToken),loc);
        recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      metaData.create;
      metaData.setComment(join(statement.comments,C_lineBreakChar));
      metaData.setAttributes(statement.attributes,statement.firstToken^.location,globals.primaryContext.messages);
      ensureRuleId(statement.firstToken^.txt,
                   ruleModifiers,
                   statement.firstToken^.location,globals.primaryContext.messages,metaData,newRuleCreated);
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
        if (t^.tokType=tt_iifElse) and (t^.next<>nil) then case t^.next^.tokType of tt_customTypeRule,tt_type: begin
          newNext:=t^.next^.next;
          if t^.next^.tokType=tt_customTypeRule
          then t^.tokType:=tt_customTypeCheck
          else t^.tokType:=tt_typeCheck;
          t^.txt    :=t^.next^.txt;
          t^.data   :=t^.next^.data;
          recycler.disposeToken(t^.next);
          t^.next:=newNext;
        end;
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
      predigest(assignmentToken,@self,globals.primaryContext.messages,recycler);
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
          {$ifdef fullVersion}
          globals.primaryContext.callStackPushCategory(@self,pc_interpretation,pseudoCallees);
          {$endif}
          if profile then globals.timeBaseComponent(pc_interpretation);
          if not ((statement.firstToken<>nil) and statement.firstToken^.areBracketsPlausible(globals.primaryContext.messages)) then begin
            recycler.cascadeDisposeToken(statement.firstToken);
            exit;
          end;
          predigest(statement.firstToken,@self,globals.primaryContext.messages,recycler);
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
          predigest(statement.firstToken,@self,globals.primaryContext.messages,recycler);
          resolveBuiltinIDs(statement.firstToken,globals.primaryContext.messages);
        end
        else globals.primaryContext.messages^.postTextMessage(mt_el1_note,statement.firstToken^.location,'Skipping expression '+tokensToString(statement.firstToken,50));
      end;
    end;
    if statement.firstToken<>nil then recycler.cascadeDisposeToken(statement.firstToken);
    statement.firstToken:=nil;
    if usecase=lu_forCodeAssistance then globals.primaryContext.messages^.clearFlags;
  end;

PROCEDURE T_package.load(usecase:T_packageLoadUsecase; VAR globals:T_evaluationGlobals; VAR recycler:T_recycler; CONST mainParameters:T_arrayOfString{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos{$endif});
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
      if not(packageRules.containsKey(MAIN_RULE_ID,mainRule)) then begin
        globals.primaryContext.messages^.raiseSimpleError('The specified package contains no main rule.',packageTokenLocation(@self));
      end else begin
        parametersForMain:=newListLiteral(length(mainParameters));
        for i:=0 to length(mainParameters)-1 do parametersForMain^.appendString(mainParameters[i]);

        {$ifdef fullVersion}
        globals.primaryContext.callStackPushCategory(@self,pc_interpretation,pseudoCallees);
        {$endif}
        if profile then globals.timeBaseComponent(pc_interpretation);

        if mainRule^.replaces(tt_localUserRule,packageTokenLocation(@self),parametersForMain,t,dummy,@globals.primaryContext,recycler)
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

  {$ifdef fullVersion}
  PROCEDURE checkParameters;
    VAR rule:P_rule;
        pack:P_package;
    begin
      for pack in secondaryPackages do for rule in pack^.packageRules.valueSet do rule^.checkParameters(globals.primaryContext);
      for rule in packageRules.valueSet do rule^.checkParameters(globals.primaryContext);
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
      interpret(stmt,usecase,globals,recycler{$ifdef fullVersion},localIdInfos{$endif});
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
      if gui_started then begin
        resolveRuleIds(globals.primaryContext.messages);
        complainAboutUnused(globals.primaryContext.messages);
        checkParameters;
      end;
      {$endif}
      exit;
    end;
    if globals.primaryContext.messages^.continueEvaluation then begin
      readyForUsecase:=usecase;
      logReady(newCodeHash);
      if usecase=lu_forCallingMain then executeMain;
    end else readyForUsecase:=lu_NONE;
    if isMain and (usecase in [lu_forDirectExecution,lu_forCallingMain])
    then finalize(globals.primaryContext,recycler);
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
    suppressAllUnusedWarnings:=false;
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
    clearCustomOperators;
    packageRules.clear;
    importedRules.clear;
    readyForUsecase:=lu_NONE;
  end;

PROCEDURE T_package.writeDataStores(CONST messages:P_messages; CONST recurse:boolean);
  VAR rule:P_rule;
      i:longint;
  begin
    for rule in packageRules.valueSet do
      if rule^.getRuleType=rt_datastore
      then P_datastoreRule(rule)^.writeBack(messages);
    if recurse then for i:=0 to length(packageUses)-1 do packageUses[i].pack^.writeDataStores(messages,recurse);
  end;

PROCEDURE T_package.finalize(VAR context:T_context; VAR recycler:T_recycler);
  VAR rule:P_rule;
      i:longint;
  begin
    for i:=0 to length(runAfter)-1 do begin
      runAfter[i]^.evaluate(packageTokenLocation(@self),@context,@recycler,nil);
    end;
    for i:=0 to length(packageUses)-1 do packageUses[i].pack^.finalize(context,recycler);
    funcs_server.onPackageFinalization(@self);
    funcs_ipc   .onPackageFinalization(@self);
    funcs_format.onPackageFinalization(@self);
    for rule in packageRules.valueSet do if rule^.getRuleType=rt_datastore then P_datastoreRule(rule)^.writeBack(context.messages);
    if isMain then context.getGlobals^.stopWorkers(recycler);
  end;

FUNCTION T_package.literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; VAR recycler:T_recycler):string;
  VAR toStringRule:P_rule;
      toReduce,dummy:P_token;
      parameters:P_listLiteral;
      stringOut:P_literal=nil;
  begin
    if packageRules .containsKey('toString',toStringRule)
    or importedRules.containsKey('toString',toStringRule)
    then begin
      parameters:=P_listLiteral(newListLiteral(1)^.append(L,true));
      if toStringRule^.replaces(tt_localUserRule,location,parameters,toReduce,dummy,context,recycler)
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
  VAR r:P_rule;

  PROCEDURE addDef(CONST def:P_typedef);
    begin result.put(def^.getName,def); end;
  begin
    result.create();
    for r in importedRules.valueSet do if r^.getRuleType in [rt_customTypeCheck,rt_duckTypeCheck] then addDef(r^.getTypedef);
    for r in packageRules .valueSet do if r^.getRuleType in [rt_customTypeCheck,rt_duckTypeCheck] then addDef(r^.getTypedef);
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

FUNCTION T_package.ensureRuleId(CONST ruleId: T_idString; CONST modifiers: T_modifierSet; CONST ruleDeclarationStart: T_tokenLocation; CONST messages:P_messages; VAR metaData:T_ruleMetaData; OUT newRuleCreated:boolean): P_rule;
  PROCEDURE raiseModifierComplaint;
    VAR m:T_modifier;
        s:string='';
    begin
      for m:=low(T_modifier) to high(T_modifier) do if m in modifiers then s:=s+C_modifierInfo[m].name+' ';
      messages^.raiseSimpleError('Invalid combination of modifiers: '+s,ruleDeclarationStart);
    end;

  VAR ruleType:T_ruleType=rt_normal;
      i:longint;
      op:T_tokenType;
      hidden:P_intFuncCallback=nil;
      m:T_modifier;
  begin
    newRuleCreated:=false;
    i:=0;
    while (i<length(C_validModifierCombinations)) and (C_validModifierCombinations[i].modifiers<>modifiers-[modifier_curry]) do inc(i);
    if i<length(C_validModifierCombinations) then ruleType:=C_validModifierCombinations[i].ruleType
    else begin
      raiseModifierComplaint;
      exit(nil);
    end;
    if not(packageRules.containsKey(ruleId,result)) then begin
      if (ruleId=MAIN_RULE_ID) then begin
        if modifiers<>[] then begin
          messages^.raiseSimpleError('main rules must not have any modifiers',ruleDeclarationStart);
          exit;
        end;
      end;
      if intrinsicRuleMap.containsKey(ruleId,hidden) then begin
        for op in allOperators do if operatorName[op]=ruleId then ruleType:=rt_customOperator;
      if ruleType=rt_customOperator then begin
        for m in [modifier_mutable,
                  modifier_datastore,
                  modifier_plain,
                  modifier_synchronized,
                  modifier_customType,
                  modifier_customDuckType] do if m in modifiers then messages^.raiseSimpleError('modifier '+C_modifierInfo[m].name+' is not allowed when overriding operators',ruleDeclarationStart);
        if modifier_curry    in modifiers then messages^.postTextMessage(mt_el3_evalError,ruleDeclarationStart,'curry modifier is forbidden when overloading operators');
        if modifier_private  in modifiers then messages^.postTextMessage(mt_el2_warning  ,ruleDeclarationStart,'private modifier is ignored when overloading operators' );
        if modifier_memoized in modifiers then messages^.postTextMessage(mt_el2_warning  ,ruleDeclarationStart,'memoized modifier is ignored when overloading operators');
      end;

      if (ruleType=rt_customTypeCheck) and not(ruleId[1] in ['A'..'Z']) then
        messages^.postTextMessage(mt_el2_warning,ruleDeclarationStart,'Type rules should begin with an uppercase letter');
      end;
      if startsWith(ruleId,'is') and packageRules.containsKey(isTypeToType(ruleId)) then
        messages^.postTextMessage(mt_el2_warning,ruleDeclarationStart,'Rule '+ruleId+' hides implicit typecheck rule');

      case ruleType of
        rt_memoized       : new(P_memoizedRule             (result),create(ruleId,ruleDeclarationStart));
        rt_customTypeCheck,
        rt_duckTypeCheck  : new(P_typeCheckRule            (result),create(ruleId,ruleDeclarationStart,ruleType=rt_duckTypeCheck));
        rt_customTypeCast : raise Exception.create('Custom type casts should not be created this way.');
        rt_mutable        : new(P_mutableRule              (result),create(ruleId,ruleDeclarationStart,      metaData,modifier_private in modifiers));
        rt_datastore      : new(P_datastoreRule            (result),create(ruleId,ruleDeclarationStart,@self,metaData,modifier_private in modifiers,modifier_plain in modifiers));
        rt_synchronized   : new(P_protectedRuleWithSubrules(result),create(ruleId,ruleDeclarationStart));
        else                new(P_ruleWithSubrules         (result),create(ruleId,ruleDeclarationStart,ruleType));
      end;
      newRuleCreated:=true;
      if modifier_curry in modifiers then result^.allowCurrying:=true;
      packageRules.put(ruleId,result);
      if intrinsicRuleMap.containsKey(ruleId,hidden) then begin
        for op in allOperators do if operatorName[op]=ruleId
        then begin
          if op in overridableOperators then begin
            customOperatorRules[op]:=result;
            if not(metaData.hasAttribute(OVERRIDE_ATTRIBUTE)) then messages^.postTextMessage(mt_el2_warning,ruleDeclarationStart,'Overloading operator '+C_tokenInfo[op].defaultId);
            result^.allowCurrying:=false;
            {$ifdef fullVersion}
            result^.setIdResolved;
            {$endif}
          end else messages^.raiseSimpleError('Operator '+C_tokenInfo[op].defaultId+' cannot be overridden',ruleDeclarationStart);
          exit(result);
        end;
        result^.hiddenRule:=hidden;
        if not(metaData.hasAttribute(OVERRIDE_ATTRIBUTE)) then messages^.postTextMessage(mt_el1_note,ruleDeclarationStart,'Overloading builtin rule "'+ruleId+'"');
      end else result^.hiddenRule:=nil;
    end else begin
      if (result^.getRuleType<>ruleType) and (ruleType<>rt_normal)
      then messages^.raiseSimpleError('Colliding modifiers! Rule '+ruleId+' is '+C_ruleTypeText[result^.getRuleType]+', redeclared as '+C_ruleTypeText[ruleType],ruleDeclarationStart)
      else if (ruleType in C_ruleTypesWithOnlyOneSubrule)
      then messages^.raiseSimpleError(C_ruleTypeText[ruleType]+'rules must have exactly one subrule',ruleDeclarationStart);
    end;
  end;

FUNCTION T_package.getSecondaryPackageById(CONST id: ansistring): ansistring;
  VAR i:longint;
  begin
    for i:=0 to length(secondaryPackages)-1 do if secondaryPackages[i]^.getId=id then exit(secondaryPackages[i]^.getPath);
    result:='';
  end;

PROCEDURE T_package.resolveRuleIds(CONST messages:P_messages);
  VAR rule:P_rule;
  begin
    for rule in packageRules.valueSet do rule^.resolveIds(messages);
  end;

{$ifdef fullVersion}
PROCEDURE T_package.updateLists(VAR userDefinedRules: T_setOfString; CONST forCompletion:boolean);
  FUNCTION typeToIsType(CONST id:T_idString):T_idString;
    begin
      result:=id;
      result[1]:=upCase(result[1]);
      result:='is'+result;
    end;

  PROCEDURE wput(CONST s:ansistring; CONST packageOrNil:P_objectWithPath); inline;
    begin
      userDefinedRules.put(s);
      if forCompletion and (pos(ID_QUALIFY_CHARACTER,s)<=0) then begin
        userDefinedRules.put(ID_QUALIFY_CHARACTER+s);
        if packageOrNil<>nil then userDefinedRules.put(packageOrNil^.getId+ID_QUALIFY_CHARACTER+s);
      end;
    end;

  VAR rule:P_rule;
      use :P_package;
  begin
    for rule in packageRules.valueSet do begin
      wput(rule^.getId,nil);
      if rule^.getRuleType in [rt_customTypeCheck,rt_duckTypeCheck] then wput(typeToIsType(rule^.getId),nil);
    end;
    for rule in importedRules.valueSet do  begin
      wput(rule^.getId,rule^.getLocation.package);
      if rule^.getRuleType in [rt_customTypeCheck,rt_duckTypeCheck] then wput(typeToIsType(rule^.getId),rule^.getLocation.package);
    end;
    if not(forCompletion) then for use in secondaryPackages do userDefinedRules.put(use^.getId);
  end;

PROCEDURE T_package.complainAboutUnused(CONST messages:P_messages);
  VAR rule:P_rule;
      import:T_packageReference;
  begin
    for rule in packageRules.valueSet do rule^.complainAboutUnused(messages);
    for import in packageUses do if not(import.pack^.anyCalled) then
      messages^.postTextMessage(mt_el2_warning,import.locationOfDeclaration,'Unused import '+import.pack^.getId+' ('+import.pack^.getPath+')');
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

FUNCTION T_package.inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; VAR recycler:T_recycler):P_mapLiteral;
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
      for rule in allRules do result^.put(rule^.getId,rule^.inspect(includeRulePointer,P_context(context)^,recycler),false);
    end;

  begin
    result:=newMapLiteral^.put('id'      ,getId)^
                          .put('path'    ,getPath)^
                          .put('source'  ,join(getCodeProvider^.getLines,C_lineBreakChar))^
                          .put('uses'    ,usesList,false)^
                          .put('includes',includeList,false)^
                          .put('declares',rulesList,false);
  end;

{$ifdef fullVersion}
FUNCTION T_package.getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
  VAR rule:P_rule;
      subRule:P_subruleExpression;
      matchesAll:boolean;
      key:string;
  begin
    setLength(result,0);
    for rule in packageRules.valueSet do if (rule^.getRuleType in [rt_normal,rt_synchronized,rt_memoized,rt_customTypeCheck,rt_duckTypeCheck,rt_customTypeCast]) then
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
  VAR userRule:P_rule;
      intrinsicFuncPtr:P_intFuncCallback;
      ruleId:T_idString;
  PROCEDURE assignLocalRule(CONST tt:T_tokenType); inline;
    begin
      token.tokType:=tt;
      token.data:=userRule;
      {$ifdef fullVersion}
      if markAsUsed then userRule^.setIdResolved;
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
      if userRule^.getRuleType in [rt_customTypeCheck,rt_duckTypeCheck]
      then assignLocalRule(tt_customTypeRule)
      else assignLocalRule(tt_localUserRule );
      exit;
    end;
    if importedRules.containsKey(ruleId,userRule) then begin
      if userRule^.getRuleType in [rt_customTypeCheck,rt_duckTypeCheck]
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
      if packageRules.containsKey(ruleId,userRule) and (userRule^.getRuleType in [rt_customTypeCheck,rt_duckTypeCheck]) then begin
        assignLocalRule(tt_customTypeRule);
        exit;
      end;
      if importedRules.containsKey(ruleId,userRule) and (userRule^.getRuleType in [rt_customTypeCheck,rt_duckTypeCheck]) then begin
        assignImportedRule(tt_customTypeRule);
        exit;
      end;
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

FUNCTION T_package.declaredRules(CONST ruleSorting:T_ruleSorting): T_ruleList;
  VAR tmp:P_rule;
      i,j:longint;
  begin
    result:=packageRules.valueSet;
    case ruleSorting of
      rs_byNameCaseSensitive:
        for i:=1 to length(result)-1 do
        for j:=0 to i-1 do
        if result[i]^.getId<result[j]^.getId then begin
          tmp:=result[i]; result[i]:=result[j]; result[j]:=tmp;
        end;
      rs_byNameCaseInsensitive:
        for i:=1 to length(result)-1 do
        for j:=0 to i-1 do
        if uppercase(result[i]^.getId)<uppercase(result[j]^.getId) then begin
          tmp:=result[i]; result[i]:=result[j]; result[j]:=tmp;
        end;
      rs_byLocation:
        for i:=1 to length(result)-1 do
        for j:=0 to i-1 do
        if result[i]^.getLocation<result[j]^.getLocation then begin
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
