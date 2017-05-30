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
     {$ifdef fullVersion}mnh_doc, mnh_plotData,mnh_funcs_plot,mnh_settings,mnh_html,valueStore,{$else}mySys,{$endif}
     mnh_funcs,

     mnh_funcs_mnh,   mnh_funcs_server, mnh_funcs_types, mnh_funcs_math,  mnh_funcs_strings,
     mnh_funcs_list,  mnh_funcs_system, mnh_funcs_files, mnh_funcs_regex, mnh_funcs_xml,
     mnh_funcs_format,mnh_funcs_ipc,
     mnh_builtinGenerators,

     mnh_patterns,
     mnh_subrules,
     mnh_rule,
     mnh_tokenArray;

{$define include_interface}
TYPE
  P_package=^T_package;
  T_ruleMap=specialize G_stringKeyMap<P_rule>;
  T_packageLoadUsecase=(lu_NONE,lu_beingLoaded,lu_forImport,lu_forCallingMain,lu_forDirectExecution{$ifdef fullVersion},lu_forCodeAssistance{$endif});

  T_packageReference=object
    id,path:ansistring;
    pack:P_package;
    CONSTRUCTOR create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
    CONSTRUCTOR createWithSpecifiedPath(CONST path_:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
    DESTRUCTOR destroy;
    PROCEDURE loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext);
  end;

  T_package=object(T_abstractPackage)
    private
      mainPackage:P_package;
      secondaryPackages:array of P_package;
      extendedPackages:array of P_extendedPackage;

      packageRules,importedRules:T_ruleMap;
      packageUses:array of T_packageReference;
      readyForUsecase:T_packageLoadUsecase;
      {$ifdef fullVersion}pseudoCallees:T_packageProfilingCalls;{$endif}

      PROCEDURE resolveRuleIds(CONST adapters:P_adapters);
      PROCEDURE clear(CONST includeSecondaries:boolean);
      FUNCTION ensureRuleId(CONST ruleId:T_idString; CONST modifiers:T_modifierSet; CONST ruleDeclarationStart:T_tokenLocation; VAR adapters:T_adapters):P_rule;
      PROCEDURE clearPackageCache(CONST recurse:boolean);
      PROCEDURE writeDataStores(VAR adapters:T_adapters; CONST recurse:boolean);
      PROCEDURE finalize(VAR adapters:T_adapters);
      FUNCTION inspect:P_mapLiteral;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider; CONST mainPackage_:P_package);
      FUNCTION getSecondaryPackageById(CONST id:ansistring):ansistring;
      PROCEDURE load(CONST usecase:T_packageLoadUsecase; VAR context:T_threadContext; CONST mainParameters:T_arrayOfString);
      DESTRUCTOR destroy; virtual;
      {$ifdef fullVersion}
      PROCEDURE updateLists(VAR userDefinedRules:T_setOfString);
      PROCEDURE complainAboutUnused(CONST inMainPackage:boolean; VAR adapters:T_adapters);
      {$endif}
      FUNCTION getHelpOnMain:ansistring;
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
      PROCEDURE resolveId(VAR token:T_token; CONST adaptersOrNil:P_adapters); virtual;
      FUNCTION isMain:boolean;
      {$ifdef fullVersion}
      PROCEDURE reportVariables(VAR variableReport:T_variableReport);
      {$endif}
      FUNCTION getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
    end;

FUNCTION packageFromCode(CONST code:T_arrayOfString; CONST nameOrPseudoName:string):P_package;
PROCEDURE runAlone(CONST input:T_arrayOfString; adapter:P_adapters);
FUNCTION runAlone(CONST input:T_arrayOfString):T_storedMessages;
{$undef include_interface}
IMPLEMENTATION
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

PROCEDURE runAlone(CONST input:T_arrayOfString; adapter:P_adapters);
  VAR context:T_evaluationContext;
      package:T_package;
  begin
    context.create(adapter);
    package.create(newVirtualFileCodeProvider('?',input),nil);
    context.resetForEvaluation(@package,false,false,true);
    package.load(lu_forDirectExecution,context.threadContext^,C_EMPTY_STRING_ARRAY);
    package.destroy;
    context.destroy;
  end;

FUNCTION runAlone(CONST input:T_arrayOfString):T_storedMessages;
  VAR collector:T_collectingOutAdapter;
      adapter:T_adapters;
      i:longint;
  begin
    collector.create(at_unknown,C_collectAllOutputBehavior);
    adapter.create;
    adapter.addOutAdapter(@collector,false);
    runAlone(input,@adapter);
    setLength(result,length(collector.storedMessages));
    for i:=0 to length(result)-1 do result[i]:=collector.storedMessages[i];
    adapter.destroy;
    collector.destroy;
  end;

FUNCTION runScript(CONST filenameOrId:string; CONST mainParameters:T_arrayOfString; CONST locationForWarning:T_tokenLocation; CONST callerAdapters:P_adapters):P_literal;
  VAR fileName:string='';
      context:T_evaluationContext;
      package:T_package;
      tempAdapters:T_adapters;
      collector:T_collectingOutAdapter;
  begin
    if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
    then fileName:=expandFileName(filenameOrId)
    else fileName:=locateSource(extractFilePath(locationForWarning.package^.getPath),filenameOrId);
    if (fileName='') or not(fileExists(fileName)) then begin
      callerAdapters^.raiseWarning('Cannot find script with id or path "'+filenameOrId+'"',locationForWarning);
      exit(nil);
    end;
    {A} tempAdapters.create;
    {C} collector.create(at_unknown,C_collectAllOutputBehavior);
        tempAdapters.addOutAdapter(@collector,false);
    {X} context.create(@tempAdapters);
    {P} package.create(newFileCodeProvider(filenameOrId),nil);
    try
        context.resetForEvaluation(@package,false,false,true);
        package.load(lu_forCallingMain,context.threadContext^,mainParameters);
        context.afterEvaluation;
    finally
    {P} package.destroy;
    {X} context.destroy;
        result:=messagesToLiteralForSandbox(collector.storedMessages);
    {C} collector.destroy;
    {A} tempAdapters.destroy;
    end;
  end;

{$ifdef fullVersion}
PROCEDURE demoCallToHtml(CONST input:T_arrayOfString; OUT textOut,htmlOut,usedBuiltinIDs:T_arrayOfString);
  VAR messages:T_storedMessages;
      i:longint;
      tmp:ansistring;
      raw:T_rawTokenArray;
      tok:T_rawToken;
  begin
    messages:=runAlone(input);
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

PROCEDURE T_packageReference.loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext);
  VAR i:longint;
  begin
    with containingPackage^.mainPackage^ do begin
      for i:=0 to length(secondaryPackages)-1 do
        if secondaryPackages[i]^.getCodeProvider^.id = id then begin
          if  (secondaryPackages[i]^.readyForUsecase<>lu_NONE) and
              (secondaryPackages[i]^.codeChanged)
          then secondaryPackages[i]^.readyForUsecase:=lu_NONE;
          if secondaryPackages[i]^.readyForUsecase<>lu_beingLoaded then begin
            if secondaryPackages[i]^.readyForUsecase<>lu_forImport then
            secondaryPackages[i]^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
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
      pack^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
    end;
  end;

CONSTRUCTOR T_packageReference.create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
  begin
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
    path:=extractFilePath(tokenLocation.package^.getPath)+path_;
    id:=filenameToPackageId(path_);
    if not(fileExists(path)) and fileExists(path_) then path:=path_;
    if not(fileExists(path))
    then adapters^.raiseError('Cannot locate package "'+path+'"',tokenLocation);
    pack:=nil;
  end;

DESTRUCTOR T_packageReference.destroy;
  begin
    id:='';
    path:='';
    pack:=nil;
  end;

PROCEDURE T_package.load(CONST usecase:T_packageLoadUsecase; VAR context:T_threadContext; CONST mainParameters:T_arrayOfString);
  VAR statementCounter:longint=0;
      profile:boolean=false;

  PROCEDURE reloadAllPackages(CONST locationForErrorFeedback:T_tokenLocation);
    VAR i,j:longint;
        rulesSet:T_ruleMap.KEY_VALUE_LIST;
        dummyRule:P_rule;
    begin
      {$ifdef fullVersion}
      context.callStackPush(@self,pc_importing,pseudoCallees);
      {$endif}
      if profile then context.timeBaseComponent(pc_importing);
      for i:=0 to length(packageUses)-1 do packageUses[i].loadPackage(@self,locationForErrorFeedback,context);
      if profile then context.timeBaseComponent(pc_importing);
      {$ifdef fullVersion}
      context.callStackPop();
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

  VAR extendsLevel:byte=0;

  PROCEDURE interpret(VAR statement:T_enhancedStatement);
    PROCEDURE interpretIncludeClause;
      VAR locationForErrorFeedback:T_tokenLocation;
          newId:string;
          first:P_token;
          helperUse:T_packageReference;
          lexer:T_lexer;
          importWrapper:P_extendedPackage;
          stmt:T_enhancedStatement;
      begin
        first:=statement.firstToken;
        locationForErrorFeedback:=first^.location;
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
          context.adapters^.raiseError('Invalid extends clause ',locationForErrorFeedback);
          exit;
        end;
        context.recycler.cascadeDisposeToken(first);
        new(importWrapper,create(newFileCodeProvider(helperUse.path),@self));
        setLength(extendedPackages,length(extendedPackages)+1);
        extendedPackages[length(extendedPackages)-1]:=importWrapper;

        helperUse.destroy;
        lexer.create(importWrapper,@self);
        stmt:=lexer.getNextStatement(context.recycler,context.adapters^);
        inc(extendsLevel);
        while (context.adapters^.noErrors) and (stmt.firstToken<>nil) do begin
          interpret(stmt);
          stmt:=lexer.getNextStatement(context.recycler,context.adapters^);
        end;
        dec(extendsLevel);
        lexer.destroy;
      end;

    PROCEDURE interpretUseClause;
      VAR i,j:longint;
          locationForErrorFeedback:T_tokenLocation;
          newId:string;
          first:P_token;
      begin
        initialize(newId);
        first:=statement.firstToken;
        locationForErrorFeedback:=first^.location;
        first:=context.recycler.disposeToken(first);
        while first<>nil do begin
          j:=-1;
          if first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule] then begin
            newId:=first^.txt;
            {$ifdef fullVersion}
            if (newId=FORCE_GUI_PSEUDO_PACKAGE) then begin
              if not(gui_started) then context.adapters^.logGuiNeeded;
            end else
            {$endif}
            begin
              j:=length(packageUses);
              setLength(packageUses,j+1);
              packageUses[j].create(getCodeProvider^.getPath,first^.txt,first^.location,context.adapters);
            end;
          end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_string) then begin
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
        ruleDeclarationStart:T_tokenLocation;

    PROCEDURE parseRule;
      VAR p:P_token; //iterator
          //rule meta data
          ruleModifiers:T_modifierSet=[];
          ruleId:T_idString='';
          evaluateBody:boolean;
          rulePattern:T_pattern;
          ruleBody:P_token;
          subRule:P_subruleExpression;
          ruleGroup:P_rule;
          inlineValue:P_literal;
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
        while (statement.firstToken<>nil) and (statement.firstToken^.tokType in C_ruleModifiers) do begin
          include(ruleModifiers,statement.firstToken^.tokType);
          statement.firstToken:=context.recycler.disposeToken(statement.firstToken);
        end;
        evaluateBody:=evaluateBody
                   or (tt_modifier_mutable    in ruleModifiers);

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
            new(subRule,create(ruleGroup,rulePattern,ruleBody,ruleDeclarationStart,tt_modifier_private in ruleModifiers,context));
            subRule^.setComment(join(statement.comments,C_lineBreakChar));
            subRule^.setAttributes(statement.attributes,context.adapters^);
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
              dispose(subRule,destroy);
            end else P_ruleWithSubrules(ruleGroup)^.addOrReplaceSubRule(subRule,context);
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
        while (statement.firstToken<>nil) and (statement.firstToken^.tokType in C_ruleModifiers) do begin
          include(ruleModifiers,statement.firstToken^.tokType);
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

    begin
      if statement.firstToken=nil then exit;
      if usecase=lu_forCodeAssistance then context.adapters^.resetErrorFlags;

      if not(context.adapters^.noErrors) then begin
        context.recycler.cascadeDisposeToken(statement.firstToken);
        exit;
      end;
      inc(statementCounter);

      if (statement.firstToken^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule]) and
         (statement.firstToken^.next   <>nil) and
         ((statement.firstToken^.next^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule])
          or (statement.firstToken^.next^.tokType=tt_literal) and (P_literal(statement.firstToken^.next^.data)^.literalType=lt_string))
      then begin
        if (statement.firstToken^.txt=C_use) then begin
          interpretUseClause;
          exit;
        end;
        if (statement.firstToken^.txt=C_include) then begin
          interpretIncludeClause;
          exit;
        end;
      end;

      assignmentToken:=statement.firstToken^.getDeclarationOrAssignmentToken;
      if (assignmentToken<>nil) then begin
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
        context.callStackPop();
        {$endif}
      end else if statement.firstToken^.getTokenOnBracketLevel([tt_modifier_datastore],0)<>nil then begin
        {$ifdef fullVersion}
        context.callStackPush(@self,pc_declaration,pseudoCallees);
        {$endif}
        if profile then context.timeBaseComponent(pc_declaration);
        if context.adapters^.doEchoDeclaration then context.adapters^.echoDeclaration(tokensToString(statement.firstToken)+';');
        parseDataStore;
        if profile then context.timeBaseComponent(pc_declaration);
        {$ifdef fullVersion}
        context.callStackPop();
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
            context.callStackPop();
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
            if context.adapters^.doEchoInput then context.adapters^.echoInput(tokensToString(statement.firstToken)+';');
          end
          else context.adapters^.raiseNote('Skipping expression '+tokensToString(statement.firstToken,50),statement.firstToken^.location);
        end;
      end;
      if statement.firstToken<>nil then context.recycler.cascadeDisposeToken(statement.firstToken);
      statement.firstToken:=nil;
    end;

  PROCEDURE executeMain;
    VAR mainRule:P_rule;
        parametersForMain:P_listLiteral=nil;
        t:P_token;
        i:longint;
    begin
      if not(readyForUsecase=lu_forCallingMain) or not(context.adapters^.noErrors) then exit;
      if not(packageRules.containsKey(MAIN_RULE_ID,mainRule)) then begin
        context.adapters^.raiseError('The specified package contains no main rule.',packageTokenLocation(@self));
      end else begin
        t:=context.recycler.newToken(packageTokenLocation(@self),MAIN_RULE_ID,tt_localUserRule,mainRule);
        parametersForMain:=newListLiteral;
        parametersForMain^.rereference;
        for i:=0 to length(mainParameters)-1 do parametersForMain^.appendString(mainParameters[i]);
        t^.next:=context.recycler.newToken(packageTokenLocation(@self),'',tt_parList,parametersForMain);
        {$ifdef fullVersion}
        context.callStackPush(@self,pc_interpretation,pseudoCallees);
        {$endif}
        if profile then context.timeBaseComponent(pc_interpretation);
        context.reduceExpression(t);
        if profile then context.timeBaseComponent(pc_interpretation);
        {$ifdef fullVersion}
        context.callStackPop();
        {$endif}
        //error handling if main returns more than one token:------------------
        if (t=nil) or (t^.next<>nil) then begin
          {$ifdef fullVersion} if context.adapters^.hasNeedGUIerror
          then context.adapters^.raiseNote('Evaluation requires GUI-startup. Re-evaluating.',packageTokenLocation(@self));
          {$endif}
        end;
        //------------------:error handling if main returns more than one token
        //special handling if main returns an expression:----------------------
        if (t<>nil) and (t^.tokType=tt_literal) and (t^.next=nil) and
           (P_literal(t^.data)^.literalType=lt_expression) then begin
          P_expressionLiteral(t^.data)^.evaluateToLiteral(packageTokenLocation(@self),@context);
        end;
        //----------------------:special handling if main returns an expression
        context.recycler.cascadeDisposeToken(t);
        disposeLiteral(parametersForMain);
        parametersForMain:=nil;
      end;
    end;

  VAR lexer:T_lexer;
      stmt :T_enhancedStatement;
  begin
    if usecase = lu_NONE        then raise Exception.create('Invalid usecase: lu_NONE');
    if usecase = lu_beingLoaded then raise Exception.create('Invalid usecase: lu_beingLoaded');
    if isMain then context.adapters^.clearErrors;
    profile:=context.adapters^.doShowTimingInfo and (usecase in [lu_forDirectExecution,lu_forCallingMain]);
    clear(false);
    readyForUsecase:=lu_beingLoaded;

    if profile then context.timeBaseComponent(pc_tokenizing);
    lexer.create(@self);

    if profile then context.timeBaseComponent(pc_tokenizing);
    stmt:=lexer.getNextStatement(context.recycler,context.adapters^);
    if profile then context.timeBaseComponent(pc_tokenizing);

    while (context.adapters^.noErrors) and (stmt.firstToken<>nil) do begin
      interpret(stmt);
      if profile then context.timeBaseComponent(pc_tokenizing);
      stmt:=lexer.getNextStatement(context.recycler,context.adapters^);
      if profile then context.timeBaseComponent(pc_tokenizing);
    end;
    lexer.destroy;
    {$ifdef fullVersion}
    if usecase=lu_forCodeAssistance then begin
      readyForUsecase:=usecase;
      logReady;
      resolveRuleIds(context.adapters);
      complainAboutUnused(context.adapters^);
      exit;
    end;
    {$endif}
    if context.adapters^.noErrors then begin
      readyForUsecase:=usecase;
      logReady;
      if usecase=lu_forCallingMain then executeMain;
    end else readyForUsecase:=lu_NONE;
    if isMain and (usecase in [lu_forDirectExecution,lu_forCallingMain])
    then finalize(context.adapters^);
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

PROCEDURE T_package.finalize(VAR adapters: T_adapters);
  VAR ruleList:array of P_rule;
      i:longint;
  begin
    mnh_funcs_server.onPackageFinalization(@self);
    mnh_funcs_ipc   .onPackageFinalization(@self);
    mnh_funcs_format.onPackageFinalization(@self);
    adapters.updateErrorlevel;
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do begin
      if ruleList[i]^.getRuleType=rt_datastore then P_datastoreRule(ruleList[i])^.writeBack(adapters);
      ruleList[i]^.clearCache;
    end;
    setLength(ruleList,0);
    for i:=0 to length(packageUses)-1 do packageUses[i].pack^.finalize(adapters);
  end;

DESTRUCTOR T_package.destroy;
  {$ifdef fullVersion}VAR c:T_profileCategory;{$endif}
  begin
    inherited destroy;
    clear(true);
    packageRules.destroy;
    importedRules.destroy;
    {$ifdef fullVersion}
    for c in T_profileCategory do if pseudoCallees[c]<>nil then dispose(pseudoCallees[c],destroy);
    {$endif}
  end;

FUNCTION T_package.ensureRuleId(CONST ruleId: T_idString; CONST modifiers:T_modifierSet; CONST ruleDeclarationStart:T_tokenLocation; VAR adapters:T_adapters): P_rule;
  VAR ruleType:T_ruleType=rt_normal;
      i:longint;
  PROCEDURE raiseModifierComplaint;
    VAR m:T_modifier;
        s:string='';
    begin
      for m:=low(T_modifier) to high(T_modifier) do if m in modifiers then s:=s+C_tokenInfo[m].defaultId+' ';
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
        rt_memoized     : new(P_memoizedRule             (result),create(ruleId,ruleDeclarationStart));
        rt_mutable      : new(P_mutableRule              (result),create(ruleId,ruleDeclarationStart,tt_modifier_private in modifiers));
        rt_datastore    : new(P_datastoreRule            (result),create(ruleId,ruleDeclarationStart,tt_modifier_private in modifiers,tt_modifier_plain in modifiers));
        rt_synchronized : new(P_protectedRuleWithSubrules(result),create(ruleId,ruleDeclarationStart));
        else              new(P_ruleWithSubrules         (result),create(ruleId,ruleDeclarationStart,ruleType));
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

PROCEDURE T_package.clearPackageCache(CONST recurse:boolean);
  VAR r:T_ruleMap.VALUE_TYPE_ARRAY;
      i:longint;
  begin
    r:=packageRules.valueSet;
    for i:=0 to length(r)-1 do if r[i]^.getRuleType=rt_memoized then r[i]^.clearCache;
    if recurse then for i:=0 to length(secondaryPackages)-1 do secondaryPackages[i]^.clearPackageCache(true);
  end;

FUNCTION T_package.getSecondaryPackageById(CONST id:ansistring):ansistring;
  VAR i:longint;
  begin
    for i:=0 to length(secondaryPackages)-1 do if secondaryPackages[i]^.getId=id then exit(secondaryPackages[i]^.getPath);
    result:='';
  end;

PROCEDURE T_package.resolveRuleIds(CONST adapters:P_adapters);
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

PROCEDURE T_package.complainAboutUnused(VAR adapters:T_adapters);
  VAR rule:P_rule;
      import:T_packageReference;
  begin
    for rule in packageRules.valueSet do rule^.complainAboutUnused(adapters);
    for import in packageUses do if not(import.pack^.anyCalled) then
      adapters.raiseWarning('Unused import '+import.pack^.getId+' ('+import.pack^.getPath+')',packageTokenLocation(import.pack));
  end;
{$endif}

FUNCTION T_package.getHelpOnMain:ansistring;
  VAR mainRule:P_rule;
      docText:T_arrayOfString;
      i:longint;
  begin
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

FUNCTION T_package.isImportedOrBuiltinPackage(CONST id:string):boolean;
  VAR ns:T_namespace;
      i:longint;
  begin
    for ns in T_namespace do if C_namespaceString[ns]=id then exit(true);
    for i:=0 to length(packageUses)-1 do if packageUses[i].id=id then exit(true);
    result:=false;
  end;

FUNCTION T_package.isMain: boolean; begin result:=(@self=mainPackage); end;
{$ifdef fullVersion}
PROCEDURE T_package.reportVariables(VAR variableReport: T_variableReport);
  PROCEDURE addRule(CONST rule:P_rule);
    VAR value:P_literal;
        reportId:string;
    begin
      if not(rule^.isReportable(value)) then exit;
      reportId:=rule^.getId;
      if (rule^.getRuleType=rt_datastore) and not(P_datastoreRule(rule)^.isInitialized) then reportId:=reportId+' (uninitialized)';
      variableReport.addVariable(reportId,value,rule^.getLocation);
    end;

  VAR rule:P_rule;
  begin
    for rule in importedRules.valueSet do addRule(rule);
    for rule in packageRules.valueSet do addRule(rule);
  end;
{$endif}

FUNCTION T_package.inspect:P_mapLiteral;
  FUNCTION usesList:P_listLiteral;
    VAR i:longint;
    begin
      result:=newListLiteral;
      for i:=0 to length(packageUses)-1 do result^.append(newListLiteral^.appendString(packageUses[i].id)^.appendString(packageUses[i].path),false);
    end;

  FUNCTION rulesList:P_mapLiteral;
    VAR allRules:array of P_rule;
        rule:P_rule;
    begin
      allRules:=packageRules.valueSet;
      result:=newMapLiteral();
      for rule in allRules do result^.put(rule^.getId,rule^.inspect,false);
    end;

  begin
    result:=newMapLiteral^.put('id'      ,getId)^
                          .put('path'    ,getPath)^
                          .put('source'  ,join(getCodeProvider^.getLines,C_lineBreakChar))^
                          .put('uses'    ,usesList,false)^
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
      for key in attributeKeys do matchesAll:=matchesAll and subRule^.hasAttribute(key,caseSensitive);
      if matchesAll then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=subRule;
      end;
    end;
  end;

PROCEDURE T_package.resolveId(VAR token:T_token; CONST adaptersOrNil:P_adapters);
  VAR userRule:P_rule;
      intrinsicFuncPtr:P_intFuncCallback;
      ruleId:T_idString;
  begin
    ruleId   :=token.txt;
    if packageRules.containsKey(ruleId,userRule) then begin
      if userRule^.getRuleType=rt_customTypeCheck
      then token.tokType:=tt_customTypeRule
      else token.tokType:=tt_localUserRule;
      token.data:=userRule;
      {$ifdef fullVersion} userRule^.setIdResolved; {$endif}
      exit;
    end;
    if importedRules.containsKey(ruleId,userRule) then begin
      if userRule^.getRuleType=rt_customTypeCheck
      then token.tokType:=tt_customTypeRule
      else token.tokType:=tt_importedUserRule;
      token.data:=userRule;
      {$ifdef fullVersion} userRule^.setIdResolved; {$endif}
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
        token.tokType:=tt_customTypeRule;
        token.data:=userRule;
        {$ifdef fullVersion} userRule^.setIdResolved; {$endif}
        exit;
      end;
      if importedRules.containsKey(ruleId,userRule) and (userRule^.getRuleType=rt_customTypeCheck) then begin
        token.tokType:=tt_customTypeRule;
        token.data:=userRule;
        {$ifdef fullVersion} userRule^.setIdResolved; {$endif}
        exit;
      end;
    end;
    if adaptersOrNil<>nil then adaptersOrNil^.raiseError('Cannot resolve ID "'+token.txt+'"',token.location);
  end;

{$undef include_implementation}
INITIALIZATION
{$define include_initialization}
  //callbacks in doc
  {$ifdef fullVersion}
  demoCodeToHtmlCallback:=@demoCallToHtml;
  {$endif}
  {$include mnh_funcs.inc}
{$undef include_initialization}

FINALIZATION
{$define include_finalization}
{$include mnh_funcs.inc}
end.
