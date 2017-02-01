UNIT mnh_packages;
INTERFACE
USES myGenerics, mnh_constants, mnh_basicTypes, math, sysutils, myStringUtil,typinfo, FileUtil, //utilities
     mnh_litVar, mnh_fileWrappers, mnh_tokens, mnh_contexts, //types
     mnh_funcs, mnh_out_adapters, mnh_caches, //even more specific
     Classes,{$ifdef fullVersion}mnh_doc,mnh_plotData,mnh_funcs_plot,mnh_settings,mnh_html,{$else}mySys,{$endif}
     mnh_funcs_math,mnh_funcs_list,mnh_funcs_mnh,
     mnh_datastores;

{$define include_interface}
TYPE
  P_package=^T_package;
  {$include mnh_token.inc}
  P_subrule=^T_subrule;
  {$include mnh_pattern.inc}
  P_rule=^T_rule;
  T_ruleMap=specialize G_stringKeyMap<P_rule>;
  {$include mnh_subrule.inc}
  {$include mnh_rule.inc}
  {$include mnh_futureTask.inc}
  {$include mnh_procBlock.inc}
  {$include mnh_fmtStmt.inc}
  T_packageLoadUsecase=(lu_NONE,lu_forImport,lu_forCallingMain,lu_forDirectExecution,lu_forCodeAssistance);

  T_packageReference=object
    id,path:ansistring;
    pack:P_package;
    CONSTRUCTOR create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
    CONSTRUCTOR createWithSpecifiedPath(CONST path_:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
    DESTRUCTOR destroy;
    PROCEDURE loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext);
  end;

  T_package=object(T_objectWithPath)
    private
      mainPackage:P_package;
      secondaryPackages:array of P_package;
      packageRules,importedRules:T_ruleMap;
      packageUses,dynamicUses:array of T_packageReference;
      ready:T_packageLoadUsecase;
      codeProvider:T_codeProvider;
      pseudoCallees:T_packageProfilingCalls;
      PROCEDURE resolveRuleIds(CONST adapters:P_adapters);
    public
      CONSTRUCTOR create(CONST mainPackage_:P_package);
      PROCEDURE load(CONST usecase:T_packageLoadUsecase; VAR context:T_evaluationContext; CONST mainParameters:T_arrayOfString);
      PROCEDURE clear(CONST includeSecondaries:boolean);
      PROCEDURE writeDataStores(VAR adapters:T_adapters; CONST recurse:boolean);
      PROCEDURE finalize(VAR adapters:T_adapters);
      DESTRUCTOR destroy;
      PROCEDURE resolveRuleId(VAR token:T_token; CONST adaptersOrNil:P_adapters);
      FUNCTION ensureRuleId(CONST ruleId:T_idString; CONST modifiers:T_modifierSet; CONST ruleDeclarationStart,ruleDeclarationEnd:T_tokenLocation; VAR adapters:T_adapters):P_rule;
      PROCEDURE clearPackageCache(CONST recurse:boolean);
      FUNCTION getSecondaryPackageById(CONST id:ansistring):ansistring;
      {$ifdef fullVersion}
      PROCEDURE updateLists(VAR userDefinedRules:T_setOfString);
      PROCEDURE complainAboutUnused(CONST inMainPackage:boolean; VAR adapters:T_adapters);
      {$endif}
      FUNCTION getHelpOnMain:ansistring;
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean;
      FUNCTION isMain:boolean;
      FUNCTION getPath:ansistring; virtual;
      PROCEDURE setSourcePath(CONST path:ansistring);
      PROCEDURE setSourceUTF8AndPath(CONST sourceUtf8:TStrings; CONST pathOrPseudoPath:string);
      {$ifdef fullVersion}
      PROCEDURE reportVariables(VAR variableReport:T_variableReport);
      {$endif}
      FUNCTION getCodeProvider:P_codeProvider;
      FUNCTION inspect:P_mapLiteral;
      FUNCTION getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
      FUNCTION getDynamicUseMeta(VAR context:T_evaluationContext):P_mapLiteral;
    end;

FUNCTION packageFromCode(CONST code:T_arrayOfString; CONST nameOrPseudoName:string):P_package;
PROCEDURE reduceExpression(VAR first:P_token; CONST callDepth:word; VAR context:T_evaluationContext);

PROCEDURE runAlone(CONST input:T_arrayOfString; adapter:P_adapters);
FUNCTION runAlone(CONST input:T_arrayOfString):T_storedMessages;
FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_evaluationContext):P_expressionLiteral;

FUNCTION getFormat(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_preparedFormatStatement;

FUNCTION tokenStackToString(CONST stack:pointer; CONST first: P_token; CONST lengthLimit: longint): ansistring;
{$undef include_interface}
IMPLEMENTATION
CONST STACK_DEPTH_LIMIT={$ifdef Windows}60000{$else}3000{$endif};
VAR pendingTasks     :T_taskQueue;
FUNCTION isTypeToType(CONST id:T_idString):T_idString;
  begin
    if (length(id)>=3) and (id[1]='i') and (id[2]='s')
    then result:=copy(id,3,length(id)-2)
    else result:='';
  end;

FUNCTION packageFromCode(CONST code:T_arrayOfString; CONST nameOrPseudoName:string):P_package;
  begin
    new(result,create(nil));
    result^.codeProvider.setLines(code);
    result^.codeProvider.setPath(nameOrPseudoName);
  end;

PROCEDURE runAlone(CONST input:T_arrayOfString; adapter:P_adapters);
  VAR context:T_evaluationContext;
      package:T_package;
  begin
    context.createContext(adapter,ct_silentlyRunAlone);
    package.create(nil);
    package.codeProvider.setLines(input);
    package.load(lu_forDirectExecution,context,C_EMPTY_STRING_ARRAY);
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
        mt_echo_input: begin end;
        mt_echo_output: for tmp in messageText do append(htmlOut,C_messageTypeMeta[messageType].prefix+' '+toHtmlCode(escapeHtml(tmp)));
        {$ifdef fullVersion}
        mt_plotFileCreated: begin
          tmp:=extractFileName(data);
          CopyFile(data,getHtmlRoot+DirectorySeparator+tmp);
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
{$include mnh_token.inc}
{$include mnh_pattern.inc}
{$include mnh_subrule.inc}
{$include mnh_futureTask.inc}
{$include mnh_procBlock.inc}
{$include mnh_rule.inc}
{$include mnh_funcs.inc}
{$include mnh_fmtStmt.inc}

PROCEDURE T_packageReference.loadPackage(CONST containingPackage:P_package; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext);
  VAR i:longint;
  begin
    with containingPackage^.mainPackage^ do begin
      for i:=0 to length(secondaryPackages)-1 do
        if secondaryPackages[i]^.codeProvider.id = id then begin
          if secondaryPackages[i]^.ready<>lu_NONE then begin
            if secondaryPackages[i]^.ready<>lu_forImport then secondaryPackages[i]^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
            pack:=secondaryPackages[i];
            exit;
          end else begin
            context.adapters^.raiseError('Cyclic package dependencies encountered; already loading "'+id+'"',tokenLocation);
            exit;
          end;
        end;
      new(pack,create(containingPackage^.mainPackage));
      pack^.setSourcePath(path);
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
      then adapters^.raiseError('Cannot locate package for id "'+id+'"',tokenLocation)
      else adapters^.raiseNote('Importing "'+path+'" as '+id,tokenLocation);
    end;
    pack:=nil;
  end;

CONSTRUCTOR T_packageReference.createWithSpecifiedPath(CONST path_:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
  begin
    path:=extractFilePath(tokenLocation.package^.getPath)+path_;
    id:=filenameToPackageId(path_);
    if not(fileExists(path)) and fileExists(path_) then path:=path_;
    if not(fileExists(path))
    then adapters^.raiseError('Cannot locate package "'+path+'"',tokenLocation)
    else adapters^.raiseNote('Importing "'+path+'" as '+id,tokenLocation);
    pack:=nil;
  end;

DESTRUCTOR T_packageReference.destroy;
  begin
    id:='';
    path:='';
    pack:=nil;
  end;

PROCEDURE T_package.load(CONST usecase:T_packageLoadUsecase; VAR context:T_evaluationContext; CONST mainParameters:T_arrayOfString);
  VAR statementCounter:longint=0;
      commentLines,
      attributeLines:T_arrayOfString;
      profile:boolean=false;

  PROCEDURE reloadAllPackages(CONST locationForErrorFeedback:T_tokenLocation);
    VAR i,j:longint;
        rulesSet:T_ruleMap.KEY_VALUE_LIST;
        dummyRule:P_rule;
    begin
      context.callStackPush(@self,pc_importing,pseudoCallees);
      if profile then context.timeBaseComponent(pc_importing);
      for i:=0 to length(packageUses)-1 do packageUses[i].loadPackage(@self,locationForErrorFeedback,context);
      if profile then context.timeBaseComponent(pc_importing);
      context.callStackPop();
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

  PROCEDURE interpret(VAR first:P_token; CONST semicolonPosition:T_tokenLocation);
    PROCEDURE interpretUseClause;
      VAR i,j:longint;
          locationForErrorFeedback:T_tokenLocation;
          newId:string;
      begin
        initialize(newId);
        locationForErrorFeedback:=first^.location;
        first:=context.disposeToken(first);
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
              packageUses[j].create(codeProvider.getPath,first^.txt,first^.location,context.adapters);
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
            then context.adapters^.raiseError('Duplicate import: '+newId,first^.location);

          first:=context.disposeToken(first);
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
      VAR partLocation:T_tokenLocation;

      PROCEDURE fail(VAR firstOfPart:P_token);
        begin
          if firstOfPart=nil
          then context.adapters^.raiseError('Invalid declaration pattern element.',partLocation)
          else context.adapters^.raiseError('Invalid declaration pattern element: '+tokensToString(firstOfPart,20),firstOfPart^.location);
          context.cascadeDisposeToken(firstOfPart);
        end;

      PROCEDURE assertNil(VAR firstOfPart:P_token);
        begin
          if firstOfPart<>nil then fail(firstOfPart);
        end;

      CONST MSG_INVALID_OPTIONAL='Optional parameters are allowed only as last entry in a function head declaration.';
      VAR p:P_token; //iterator
          hasTrivialPattern:boolean=true;
          //rule meta data
          ruleModifiers:T_modifierSet=[];
          ruleId:T_idString='';
          evaluateBody:boolean;
          rulePattern:T_pattern;
          rulePatternElement:T_patternElement;
          ruleBody:P_token;
          subRule:P_subrule;
          ruleGroup:P_rule;
          parts:T_bodyParts;
          closingBracket:P_token;
          i:longint;
          inlineValue:P_literal;
      begin
        ruleDeclarationStart:=first^.location;
        evaluateBody:=(assignmentToken^.tokType=tt_assign);
        ruleBody:=assignmentToken^.next;
        assignmentToken^.next:=nil;
        //plausis:
        if (ruleBody=nil) then begin
          context.adapters^.raiseError('Missing function body after assignment/declaration token.',assignmentToken^.location);
          context.cascadeDisposeToken(first);
          exit;
        end;
        while (first<>nil) and (first^.tokType in C_ruleModifiers) do begin
          include(ruleModifiers,first^.tokType);
          first:=context.disposeToken(first);
        end;
        evaluateBody:=evaluateBody
                   or (tt_modifier_mutable    in ruleModifiers);

        if not(first^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule, tt_customTypeRule]) then begin
          context.adapters^.raiseError('Declaration does not start with an identifier.',first^.location);
          context.cascadeDisposeToken(first);
          context.cascadeDisposeToken(ruleBody);
          exit;
        end;
        p:=first;
        while (p<>nil) and not(p^.tokType in [tt_assign,tt_declare]) do begin
          if (p^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule]) and isQualified(first^.txt) then begin
            context.adapters^.raiseError('Declaration head contains qualified ID.',p^.location);
            context.cascadeDisposeToken(first);
            context.cascadeDisposeToken(ruleBody);
            exit;
          end;
          p:=p^.next;
        end;
        //:plausis
        ruleId:=trim(first^.txt);
        first:=context.disposeToken(first);
        if not(first^.tokType in [tt_braceOpen,tt_assign,tt_declare])  then begin
          context.adapters^.raiseError('Invalid declaration head.',first^.location);
          context.cascadeDisposeToken(first);
          context.cascadeDisposeToken(ruleBody);
          exit;
        end;
        rulePattern.create;
        if first^.tokType=tt_braceOpen then begin
          if (first^.next<>nil) and (first^.next^.tokType=tt_braceClose) then begin
            setLength(parts,0);
            closingBracket:=first^.next;
          end else begin
            hasTrivialPattern:=false;
            parts:=getBodyParts(first,0,context,closingBracket);
            for i:=0 to length(parts)-1 do begin
              partLocation:=parts[i].first^.location;
              if (parts[i].first^.tokType=tt_optionalParameters) and (parts[i].first^.next=nil) then begin
                if i<>length(parts)-1 then context.adapters^.raiseError(MSG_INVALID_OPTIONAL,parts[i].first^.location);
                //Optionals: f(...)->
                rulePattern.appendOptional;
                parts[i].first:=context.disposeToken(parts[i].first);
                assertNil(parts[i].first);
              end else if (parts[i].first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule]) then begin
                //Identified parameter: f(x)->
                rulePatternElement.create(parts[i].first^.txt);
                parts[i].first:=context.disposeToken(parts[i].first);
                if (parts[i].first<>nil) then begin
                  if (parts[i].first^.tokType in C_typeChecks) then begin
                    rulePatternElement.restrictionType:=parts[i].first^.tokType;
                    parts[i].first:=context.disposeToken(parts[i].first);

                    if rulePatternElement.restrictionType in C_modifieableTypeChecks then begin
                      if (parts[i].first=nil) then begin end else
                      if (parts[i].first^.tokType=tt_braceOpen) and
                         (parts[i].first^.next<>nil) and
                         (parts[i].first^.next^.tokType=tt_literal) and
                         (P_literal(parts[i].first^.next^.data)^.literalType=lt_int) and
                         (P_intLiteral(parts[i].first^.next^.data)^.value>=0) and
                         (parts[i].first^.next^.next<>nil) and
                         (parts[i].first^.next^.next^.tokType=tt_braceClose) and
                         (parts[i].first^.next^.next^.next=nil) then begin
                        rulePatternElement.restrictionIdx:=P_intLiteral(parts[i].first^.next^.data)^.value;
                        context.cascadeDisposeToken(parts[i].first);
                      end else fail(parts[i].first);
                    end else assertNil(parts[i].first);

                  end else if (parts[i].first^.tokType in C_patternElementComparators) then begin
                    rulePatternElement.restrictionType:=parts[i].first^.tokType;
                    parts[i].first:=context.disposeToken(parts[i].first);

                    if (parts[i].first=nil) then fail(parts[i].first) else
                    if parts[i].first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule] then begin
                      rulePatternElement.restrictionId:=parts[i].first^.txt;
                      parts[i].first:=context.disposeToken(parts[i].first);
                      assertNil(parts[i].first);
                    end else begin
                      reduceExpression(parts[i].first,0,context);
                      if (parts[i].first<>nil) and (parts[i].first^.tokType=tt_literal) then begin
                        rulePatternElement.restrictionValue:=parts[i].first^.data;
                        rulePatternElement.restrictionValue^.rereference;
                        parts[i].first:=context.disposeToken(parts[i].first);
                        assertNil(parts[i].first);
                      end else fail(parts[i].first);
                    end;

                  end else if (parts[i].first^.tokType=tt_customTypeCheck) then begin
                    rulePatternElement.restrictionType:=parts[i].first^.tokType;
                    rulePatternElement.customTypeCheck:=P_rule(parts[i].first^.data)^.subrules[0];
                    parts[i].first:=context.disposeToken(parts[i].first);

                    assertNil(parts[i].first);
                  end else fail(parts[i].first);
                end;
                rulePattern.append(rulePatternElement);
              end else begin
                //Anonymous equals: f(1)->
                reduceExpression(parts[i].first,0,context);
                if (parts[i].first<>nil) and (parts[i].first^.tokType=tt_literal) then begin
                  rulePatternElement.createAnonymous;
                  rulePatternElement.restrictionType:=tt_comparatorListEq;
                  rulePatternElement.restrictionValue:=parts[i].first^.data;
                  rulePatternElement.restrictionValue^.rereference;
                  parts[i].first:=context.disposeToken(parts[i].first);
                  assertNil(parts[i].first);
                  rulePattern.append(rulePatternElement);
                end else fail(parts[i].first);
              end;
            end;
            parts:=nil;
          end;
          rulePattern.finalizeRefs(ruleDeclarationStart,context);
          context.disposeToken(first);
          first:=context.disposeToken(closingBracket);
        end;
        if first<>nil then begin
          first:=context.disposeToken(first);
        end else begin
          context.adapters^.raiseError('Invalid declaration.',ruleDeclarationStart);
          context.cascadeDisposeToken(ruleBody);
          exit;
        end;
        rulePattern.toParameterIds(ruleBody);
        //[marker 1]
        if evaluateBody and (usecase<>lu_forCodeAssistance) and (context.adapters^.noErrors) then begin
          reduceExpression(ruleBody,0,context);
        end;

        if context.adapters^.noErrors then begin
          ruleGroup:=ensureRuleId(ruleId,ruleModifiers,ruleDeclarationStart,semicolonPosition,context.adapters^);
          if (context.adapters^.noErrors) and (ruleGroup^.ruleType in C_mutableRuleTypes) and not(hasTrivialPattern) then context.adapters^.raiseError('Mutable rules are quasi variables and must therfore not accept any arguments',ruleDeclarationStart);
          if context.adapters^.noErrors then begin
            new(subRule,create(ruleGroup,rulePattern,ruleBody,ruleDeclarationStart,tt_modifier_private in ruleModifiers,false,context));
            subRule^.comment:=join(commentLines,C_lineBreakChar);
            commentLines:=C_EMPTY_STRING_ARRAY;
            subRule^.setAttributes(attributeLines);
            attributeLines:=C_EMPTY_STRING_ARRAY;
            //in usecase lu_forCodeAssistance, the body might not be a literal because reduceExpression is not called at [marker 1]
            if (ruleGroup^.ruleType in C_mutableRuleTypes)
            then begin
              if (usecase<>lu_forCodeAssistance)
              then begin
                     inlineValue:=subRule^.getInlineValue;
                     ruleGroup^.setMutableValue(inlineValue,true);
                     inlineValue^.unreference;
                   end
              else ruleGroup^.setMutableValue(newVoidLiteral,true);
              dispose(subRule,destroy);
            end else ruleGroup^.addOrReplaceSubRule(subRule,context);
            first:=nil;
          end else begin
            context.cascadeDisposeToken(first);
            context.cascadeDisposeToken(ruleBody);
          end;
        end else begin
          context.cascadeDisposeToken(first);
          context.cascadeDisposeToken(ruleBody);
        end;
      end;

    PROCEDURE parseDataStore;
      VAR ruleModifiers:T_modifierSet=[];
          loc:T_tokenLocation;
      begin
        if (codeProvider.isPseudoFile) then begin
          context.adapters^.raiseError('data stores require the package to be saved to a file.',first^.location);
          context.cascadeDisposeToken(first);
          exit;
        end;
        while (first<>nil) and (first^.tokType in C_ruleModifiers) do begin
          include(ruleModifiers,first^.tokType);
          loc:=first^.location;
          first:=context.disposeToken(first);
        end;
        if (first=nil) or not(first^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule]) or
           (first^.next<>nil) then begin
          if first<>nil then loc:=first^.location;
          context.adapters^.raiseError('Invalid datastore definition: '+tokensToString(first),loc);
          context.cascadeDisposeToken(first);
          exit;
        end;
        ensureRuleId(first^.txt,
                     ruleModifiers,
                     first^.location,first^.location,context.adapters^);
      end;

    begin
      if first=nil then exit;
      if usecase=lu_forCodeAssistance then context.adapters^.resetErrorFlags;

      if not(context.adapters^.noErrors) then begin
        context.cascadeDisposeToken(first);
        exit;
      end;
      inc(statementCounter);

      if statementCounter=1 then begin
        if (first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule]) and
           (first^.txt    ='USE') and
           (first^.next   <>nil) and
           ((first^.next^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule])
            or (first^.next^.tokType=tt_literal) and (P_literal(first^.next^.data)^.literalType=lt_string))
        then begin
          interpretUseClause;
          exit;
        end;
      end;
      assignmentToken:=first^.getDeclarationOrAssignmentToken;
      if (assignmentToken<>nil) then begin
        context.callStackPush(@self,pc_declaration,pseudoCallees);
        if profile then context.timeBaseComponent(pc_declaration);
        if not ((assignmentToken^.next<>nil) and assignmentToken^.next^.areBracketsPlausible(context.adapters^)) then begin
          context.cascadeDisposeToken(first);
          exit;
        end;
        predigest(assignmentToken,@self,context);
        if context.adapters^.doEchoDeclaration then context.adapters^.echoDeclaration(tokensToString(first)+';');
        parseRule;
        if profile then context.timeBaseComponent(pc_declaration);
        context.callStackPop();
      end else if first^.getTokenOnBracketLevel([tt_modifier_datastore],0)<>nil then begin
        context.callStackPush(@self,pc_declaration,pseudoCallees);
        if profile then context.timeBaseComponent(pc_declaration);
        if context.adapters^.doEchoDeclaration then context.adapters^.echoDeclaration(tokensToString(first)+';');
        parseDataStore;
        if profile then context.timeBaseComponent(pc_declaration);
        context.callStackPop();
      end else if context.adapters^.noErrors then begin
        case usecase of
          lu_forDirectExecution:begin
            context.callStackPush(@self,pc_interpretation,pseudoCallees);
            if profile then context.timeBaseComponent(pc_interpretation);
            if not ((first<>nil) and first^.areBracketsPlausible(context.adapters^)) then begin
              context.cascadeDisposeToken(first);
              exit;
            end;
            predigest(first,@self,context);
            if context.adapters^.doEchoInput then context.adapters^.echoInput(tokensToString(first)+';');
            reduceExpression(first,0,context);
            if profile then context.timeBaseComponent(pc_interpretation);
            context.callStackPop();
            if (first<>nil) and context.adapters^.doShowExpressionOut then context.adapters^.echoOutput(tokensToString(first));
          end;
          lu_forCodeAssistance: if (first<>nil) and first^.areBracketsPlausible(context.adapters^) then begin
            predigest(first,@self,context);
            resolveBuiltinIDs(first,context.adapters);
            if context.adapters^.doEchoInput then context.adapters^.echoInput(tokensToString(first)+';');
          end
          else context.adapters^.raiseNote('Skipping expression '+tokensToString(first,50),first^.location);
        end;
      end;
      if first<>nil then context.cascadeDisposeToken(first);
      first:=nil;
    end;

  VAR fileTokens:T_tokenArray;

  PROCEDURE executeMain;
    VAR mainRule:P_rule;
        parametersForMain:P_listLiteral=nil;
        t:P_token;
        i:longint;
    begin
      if not(ready=lu_forCallingMain) or not(context.adapters^.noErrors) then exit;
      if not(packageRules.containsKey(MAIN_RULE_ID,mainRule)) then begin
        context.adapters^.raiseError('The specified package contains no main rule.',packageTokenLocation(@self));
      end else begin
        t:=context.newToken(packageTokenLocation(@self),MAIN_RULE_ID,tt_localUserRule,mainRule);
        parametersForMain:=newListLiteral;
        parametersForMain^.rereference;
        for i:=0 to length(mainParameters)-1 do parametersForMain^.appendString(mainParameters[i]);
        t^.next:=context.newToken(packageTokenLocation(@self),'',tt_parList,parametersForMain);
        context.callStackPush(@self,pc_interpretation,pseudoCallees);
        if profile then context.timeBaseComponent(pc_interpretation);
        reduceExpression(t,0,context);
        if profile then context.timeBaseComponent(pc_interpretation);
        context.callStackPop();
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
          P_subrule(P_expressionLiteral(t^.data)^.value)^.directEvaluateNullary(packageTokenLocation(@self),context,0);
        end;
        //----------------------:special handling if main returns an expression
        context.cascadeDisposeToken(t);
        disposeLiteral(parametersForMain);
        parametersForMain:=nil;
      end;
    end;

  {$define stepToken:=
    if profile then context.timeBaseComponent(pc_tokenizing);
    fileTokens.step(@self,commentLines,attributeLines,context.adapters^);
    if profile then context.timeBaseComponent(pc_tokenizing)}

  PROCEDURE processTokens(VAR fileTokens:T_tokenArray);
    VAR first:P_token=nil;
        last:P_token=nil;
        localIdStack:T_idStack;
    begin
      localIdStack.create;
      while not(fileTokens.atEnd) do begin
        if fileTokens.current.tokType=tt_beginBlock then begin
          if first=nil then begin
            first:=context.newToken(fileTokens.current); fileTokens.current.undefine;
            last :=first;
          end else begin
            last^.next:=context.newToken(fileTokens.current); fileTokens.current.undefine;
            last      :=last^.next;
          end;
          localIdStack.clear;
          localIdStack.scopePush;
          stepToken;
          while (not(fileTokens.atEnd)) and not((fileTokens.current.tokType=tt_endBlock) and (localIdStack.oneAboveBottom)) do begin
            case fileTokens.current.tokType of
              tt_beginBlock: localIdStack.scopePush;
              tt_endBlock  : localIdStack.scopePop;
              tt_identifier, tt_importedUserRule,tt_localUserRule,tt_intrinsicRule: if (last^.tokType=tt_modifier_local) then begin
                fileTokens.mutateCurrentTokType(tt_blockLocalVariable);
                localIdStack.addId(fileTokens.current.txt);
              end else if (localIdStack.hasId(fileTokens.current.txt)) then
                fileTokens.mutateCurrentTokType(tt_blockLocalVariable);
            end;
            last^.next:=context.newToken(fileTokens.current); fileTokens.current.undefine;
            last      :=last^.next;
            stepToken;
          end;
        end else if (fileTokens.current.tokType=tt_semicolon) then begin
          if (first<>nil)
          then interpret(first,fileTokens.current.location)
          else context.cascadeDisposeToken(first);
          last:=nil;
          first:=nil;
          stepToken;
        end else begin
          if first=nil then begin
            first:=context.newToken(fileTokens.current); fileTokens.current.undefine;
            last :=first
          end else begin
            last^.next:=context.newToken(fileTokens.current); fileTokens.current.undefine;
            last      :=last^.next;
          end;
          last^.next:=nil;
          stepToken;
        end;
      end;
      if profile then context.timeBaseComponent(pc_tokenizing);
      fileTokens.destroy;
      localIdStack.destroy;
      if profile then context.timeBaseComponent(pc_tokenizing);

      if (context.adapters^.noErrors)
      then begin if first<>nil then interpret(first,first^.location); end
      else context.cascadeDisposeToken(first);
    end;

  begin
    if usecase=lu_NONE then raise Exception.create('Invalid usecase: lu_NONE');
    if isMain then context.adapters^.clearErrors;
    profile:=context.wantBasicTiming and (usecase in [lu_forDirectExecution,lu_forCallingMain]);
    clear(false);

    if ((usecase=lu_forCallingMain) or not(isMain)) and codeProvider.fileHasChanged then codeProvider.load;
    if profile then context.timeBaseComponent(pc_tokenizing);
    fileTokens.create;
    fileTokens.tokenizeAll(codeProvider,@self,context.adapters^);
    commentLines  :=C_EMPTY_STRING_ARRAY;
    attributeLines:=C_EMPTY_STRING_ARRAY;
    fileTokens.step(@self,commentLines,attributeLines,context.adapters^);
    if profile then context.timeBaseComponent(pc_tokenizing);
    processTokens(fileTokens);

    ready:=usecase;
    case usecase of
      lu_forCodeAssistance: resolveRuleIds(context.adapters);
      lu_forCallingMain:    executeMain;
    end;
    {$ifdef fullVersion}
    if (usecase in [lu_forDirectExecution,lu_forCallingMain,lu_forCodeAssistance]) and gui_started and context.adapters^.noErrors
    then complainAboutUnused(true,context.adapters^);
    {$endif}
    if isMain and (usecase in [lu_forDirectExecution,lu_forCallingMain])
    then begin
      finalize(context.adapters^);
      clearCachedFormats;
    end;
  end;

PROCEDURE disposeRule(VAR rule:P_rule);
  begin
    dispose(rule,destroy);
  end;

CONSTRUCTOR T_package.create(CONST mainPackage_:P_package);
  begin
    mainPackage:=mainPackage_;
    if mainPackage=nil then mainPackage:=@self;
    setLength(secondaryPackages,0);
    setLength(packageUses,0);
    setLength(dynamicUses,0);
    codeProvider.create;
    packageRules.create(@disposeRule);
    importedRules.create;
    pseudoCallees:=blankProfilingCalls;
  end;

PROCEDURE T_package.clear(CONST includeSecondaries:boolean);
  VAR i:longint;
  begin
    if includeSecondaries then begin
      for i:=0 to length(secondaryPackages)-1 do dispose(secondaryPackages[i],destroy);
      setLength(secondaryPackages,0);
    end;
    for i:=0 to length(packageUses)-1 do packageUses[i].destroy; setLength(packageUses,0);
    for i:=0 to length(dynamicUses)-1 do dynamicUses[i].destroy; setLength(dynamicUses,0);
    packageRules.clear;
    importedRules.clear;
    ready:=lu_NONE;
  end;

PROCEDURE T_package.writeDataStores(VAR adapters:T_adapters; CONST recurse:boolean);
  VAR rule:P_rule;
      i:longint;
  begin
    for rule in packageRules.valueSet do
      if rule^.ruleType in [rt_datastore_private,rt_datastore_public]
      then rule^.writeBack(codeProvider,adapters);
    if recurse then for i:=0 to length(packageUses)-1 do packageUses[i].pack^.writeDataStores(adapters,recurse);
  end;

PROCEDURE T_package.finalize(VAR adapters:T_adapters);
  VAR ruleList:array of P_rule;
      i:longint;
  begin
    adapters.updateErrorlevel;
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do begin
      ruleList[i]^.writeBack(codeProvider,adapters);
      if ruleList[i]^.ruleType=rt_memoized then ruleList[i]^.cache^.clear;
    end;
    setLength(ruleList,0);
    for i:=0 to length(packageUses)-1 do packageUses[i].pack^.finalize(adapters);
  end;

DESTRUCTOR T_package.destroy;
  VAR c:T_profileCategory;
  begin
    clear(true);
    codeProvider.destroy;
    packageRules.destroy;
    importedRules.destroy;
    for c in T_profileCategory do if pseudoCallees[c]<>nil then dispose(pseudoCallees[c],destroy);
  end;

PROCEDURE T_package.resolveRuleId(VAR token: T_token; CONST adaptersOrNil:P_adapters);
  VAR userRule:P_rule;
      intrinsicFuncPtr:P_intFuncCallback;
      ruleId:T_idString;
  begin
    ruleId   :=token.txt;
    if packageRules.containsKey(ruleId,userRule) then begin
      if userRule^.ruleType=rt_customTypeCheck
      then token.tokType:=tt_customTypeRule
      else token.tokType:=tt_localUserRule;
      token.data:=userRule;
      userRule^.idResolved:=true;
      exit;
    end;
    if importedRules.containsKey(ruleId,userRule) then begin
      if userRule^.ruleType=rt_customTypeCheck
      then token.tokType:=tt_customTypeRule
      else token.tokType:=tt_importedUserRule;
      token.data:=userRule;
      userRule^.idResolved:=true;
      exit;
    end;
    if intrinsicRuleMap.containsKey(ruleId,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRule;
      token.data:=intrinsicFuncPtr;
      exit;
    end;
    ruleId:=isTypeToType(ruleId);
    if ruleId<>'' then begin
      if packageRules.containsKey(ruleId,userRule) and (userRule^.ruleType=rt_customTypeCheck) then begin
        token.tokType:=tt_customTypeRule;
        token.data:=userRule;
        userRule^.idResolved:=true;
        exit;
      end;
      if importedRules.containsKey(ruleId,userRule) and (userRule^.ruleType=rt_customTypeCheck) then begin
        token.tokType:=tt_customTypeRule;
        token.data:=userRule;
        userRule^.idResolved:=true;
        exit;
      end;
    end;
    if adaptersOrNil<>nil then adaptersOrNil^.raiseError('Cannot resolve ID "'+token.txt+'"',token.location);
  end;

FUNCTION T_package.ensureRuleId(CONST ruleId: T_idString; CONST modifiers:T_modifierSet; CONST ruleDeclarationStart,ruleDeclarationEnd:T_tokenLocation; VAR adapters:T_adapters): P_rule;
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
      new(result,create(ruleId,ruleType,ruleDeclarationStart));
      packageRules.put(ruleId,result);
      if ruleType=rt_customTypeCheck
      then adapters.raiseNote('Creating new rules: '+ruleId+' and is'+ruleId,ruleDeclarationStart)
      else adapters.raiseNote('Creating new rule: '+ruleId,ruleDeclarationStart);
      if intrinsicRuleMap.containsKey(ruleId) then adapters.raiseWarning('Hiding builtin rule "'+ruleId+'"!',ruleDeclarationStart);
    end else begin
      if (result^.ruleType<>ruleType) and (ruleType<>rt_normal)
      then adapters.raiseError('Colliding modifiers! Rule '+ruleId+' is '+C_ruleTypeText[result^.ruleType]+', redeclared as '+C_ruleTypeText[ruleType],ruleDeclarationStart)
      else if (ruleType in C_ruleTypesWithOnlyOneSubrule)
      then adapters.raiseError(C_ruleTypeText[ruleType]+'rules must have exactly one subrule',ruleDeclarationStart)
      else adapters.raiseNote('Extending rule: '+ruleId,ruleDeclarationStart);
    end;
    result^.declarationEnd:=ruleDeclarationEnd;
  end;

PROCEDURE T_package.clearPackageCache(CONST recurse:boolean);
  VAR r:T_ruleMap.VALUE_TYPE_ARRAY;
      i:longint;
  begin
    r:=packageRules.valueSet;
    for i:=0 to length(r)-1 do if r[i]^.ruleType=rt_memoized then r[i]^.clearCache;
    if recurse then for i:=0 to length(secondaryPackages)-1 do secondaryPackages[i]^.clearPackageCache(true);
  end;

FUNCTION T_package.getSecondaryPackageById(CONST id:ansistring):ansistring;
  VAR i:longint;
  begin
    for i:=0 to length(secondaryPackages)-1 do if secondaryPackages[i]^.getCodeProvider^.id=id then exit(secondaryPackages[i]^.getPath);
    result:='';
  end;

PROCEDURE T_package.resolveRuleIds(CONST adapters:P_adapters);
  VAR ruleList:array of P_rule;
      i:longint;
  begin
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do ruleList[i]^.resolveIds(adapters);
    setLength(ruleList,0);
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
      userDefinedRules.put(rule^.id);
      if rule^.ruleType=rt_customTypeCheck then userDefinedRules.put(typeToIsType(rule^.id));
    end;
    for rule in importedRules.valueSet do  begin
      userDefinedRules.put(rule^.id);
      if rule^.ruleType=rt_customTypeCheck then userDefinedRules.put(typeToIsType(rule^.id));
    end;
  end;

PROCEDURE T_package.complainAboutUnused(CONST inMainPackage:boolean; VAR adapters:T_adapters);
  VAR ruleList:array of P_rule;
      i:longint;
      anyCalled:boolean=false;
  begin
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do if not(ruleList[i]^.complainAboutUnused(adapters)) then anyCalled:=true;
    if not(anyCalled) and not(inMainPackage) then adapters.raiseWarning('Unused import '+codeProvider.id,packageTokenLocation(@self));
    if inMainPackage then begin
      for i:=0 to length(packageUses)-1 do begin
        packageUses[i].pack^.complainAboutUnused(false,adapters);
      end;
    end;
    setLength(ruleList,0);
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
    for ns:=low(T_namespace) to high(T_namespace) do if C_namespaceString[ns]=id then exit(true);
    for i:=0 to length(packageUses)-1 do if packageUses[i].id=id then exit(true);
    result:=false;
  end;

FUNCTION T_package.getPath:ansistring; begin result:=codeProvider.getPath; end;
FUNCTION T_package.isMain:boolean; begin result:=(@self=mainPackage); end;
PROCEDURE T_package.setSourcePath(CONST path:ansistring);
  begin
    codeProvider.setPath(path);
    codeProvider.load;
  end;

PROCEDURE T_package.setSourceUTF8AndPath(CONST sourceUtf8:TStrings; CONST pathOrPseudoPath:string);
  begin
    codeProvider.setLinesUTF8(sourceUtf8);
    codeProvider.setPath(pathOrPseudoPath);
  end;
{$ifdef fullVersion}
PROCEDURE T_package.reportVariables(VAR variableReport:T_variableReport);
  VAR i:longint;
      r:T_ruleMap.VALUE_TYPE_ARRAY;
      value:P_literal;
  begin
    r:=importedRules.valueSet;
    for i:=0 to length(r)-1 do if r[i]^.isReportable(value) then variableReport.addVariable(r[i]^.id, value,r[i]^.declarationStart);
    setLength(r,0);
    r:=packageRules.valueSet;
    for i:=0 to length(r)-1 do if r[i]^.isReportable(value) then variableReport.addVariable(r[i]^.id, value,r[i]^.declarationStart);
    setLength(r,0);
  end;
{$endif}

FUNCTION T_package.getCodeProvider:P_codeProvider;
  begin
    result:=@codeProvider;
  end;

FUNCTION T_package.inspect:P_mapLiteral;
  FUNCTION usesList:P_listLiteral;
    VAR i:longint;
    begin
      result:=newListLiteral;
      for i:=0 to length(packageUses)-1 do result^.append(newListLiteral^.appendString(packageUses[i].id)^.appendString(packageUses[i].path),false);
    end;

  FUNCTION rulesList:P_listLiteral;
    VAR allRules:array of P_rule;
        rule:P_rule;
    begin
      allRules:=packageRules.valueSet;
      result:=newListLiteral(length(allRules));
      for rule in allRules do result^.append(rule^.inspect,false);
    end;

  begin
    result:=newMapLiteral^.put('id'      ,codeProvider.id)^
                          .put('path'    ,getPath)^
                          .put('source'  ,join(codeProvider.getLines,C_lineBreakChar))^
                          .put('uses'    ,usesList,false)^
                          .put('declares',rulesList,false);
  end;

FUNCTION T_package.getSubrulesByAttribute(CONST attributeKeys:T_arrayOfString; CONST caseSensitive:boolean=true):T_subruleArray;
  VAR rule:P_rule;
      subRule:P_subrule;
      matchesAll:boolean;
      key:string;
  begin
    setLength(result,0);
    for rule in packageRules.valueSet do
    for subRule in rule^.subrules do begin
      matchesAll:=true;
      for key in attributeKeys do matchesAll:=matchesAll and subRule^.hasAttribute(key,caseSensitive);
      if matchesAll then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=subRule;
      end;
    end;
  end;

FUNCTION T_package.getDynamicUseMeta(VAR context:T_evaluationContext):P_mapLiteral;
  FUNCTION rulesMeta:P_listLiteral;
    VAR rule:P_rule;
    begin
      result:=newListLiteral();
      for rule in packageRules.valueSet do if rule^.hasPublicSubrule then
        result^.append(rule^.getDynamicUseMetaLiteral(context),false);
    end;

  FUNCTION subRulesMeta:P_listLiteral;
    VAR rule:P_rule;
        subRule:P_subrule;
    begin
      result:=newListLiteral();
      for rule in packageRules.valueSet do for subRule in rule^.subrules do if subRule^.typ=srt_normal_public then
        result^.append(newMapLiteral^
          .put('id'        ,subRule^.getId)^
          .put('subrule'   ,newExpressionLiteral(subRule),false)^
          .put('attributes',subRule^.getAttributesLiteral,false),false);
    end;

  begin
    result:=newMapLiteral^
              .put('rules'   ,rulesMeta   ,false)^
              .put('subrules',subRulesMeta,false);
  end;

{$undef include_implementation}
INITIALIZATION
{$define include_initialization}
  {$include mnh_token.inc}
  {$include mnh_fmtStmt.inc}
  {$include mnh_subrule.inc}
  pendingTasks.create;

  //callbacks in mnh_litvar:

  //callbacks in doc
  {$ifdef fullVersion}
  demoCodeToHtmlCallback:=@demoCallToHtml;
  mnh_funcs_plot.generateRow:=@generateRow;
  rawTokenizeCallback:=@tokenizeAllReturningRawTokens;
  {$endif}
  {$include mnh_funcs.inc}
{$undef include_initialization}

FINALIZATION
{$define include_finalization}
  pendingTasks.destroy;
{$include mnh_funcs.inc}
{$include mnh_fmtStmt.inc}
{$include mnh_subrule.inc}
end.
