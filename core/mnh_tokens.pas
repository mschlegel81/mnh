UNIT mnh_tokens;
INTERFACE
USES myGenerics, mnh_constants, math, sysutils, myStringUtil,typinfo, mySys, FileUtil, //utilities
     mnh_litVar, mnh_fileWrappers, mnh_tokLoc, //types
     EpikTimer,
     mnh_funcs, mnh_out_adapters, mnh_caches, mnh_doc, mnh_html, //even more specific
     mnh_funcs_mnh, mnh_funcs_math, mnh_funcs_strings, mnh_funcs_list, mnh_funcs_system, mnh_funcs_regex;

{$define include_interface}
TYPE
  P_package=^T_package;
  {$include mnh_tokens_token.inc}
  {$include mnh_tokens_pattern.inc}
  P_rule=^T_rule;
  T_ruleMap=specialize G_stringKeyMap<P_rule>;
  {$include mnh_tokens_recycler.inc}
  {$include mnh_tokens_subrule.inc}
  {$include mnh_tokens_rule.inc}
  {$include mnh_tokens_futureTask.inc}
  {$include mnh_tokens_procBlock.inc}

  T_packageLoadUsecase=(lu_forImport,lu_forCallingMain,lu_forDirectExecution,lu_forDocGeneration);

  T_packageReference=object
    id,path:ansistring;
    pack:P_package;
    CONSTRUCTOR create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters);
    DESTRUCTOR destroy;
  end;

  { T_package }
  T_package=object
    private
      packageRules,importedRules:T_ruleMap;
      packageUses:array of T_packageReference;
      ready:boolean;
      codeProvider:P_codeProvider;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider);
      FUNCTION needReload:boolean;
      PROCEDURE load(CONST usecase:T_packageLoadUsecase; VAR context:T_evaluationContext; CONST mainParameters:T_arrayOfString);
      PROCEDURE clear;
      PROCEDURE finalize(VAR adapters:T_adapters);
      DESTRUCTOR destroy;
      PROCEDURE resolveRuleId(VAR token:T_token; CONST adaptersOrNil:P_adapters);
      FUNCTION ensureRuleId(CONST ruleId:ansistring; CONST ruleIsPrivate,ruleIsMemoized,ruleIsMutable,ruleIsPersistent,ruleIsSynchronized:boolean; CONST ruleDeclarationStart,ruleDeclarationEnd:T_tokenLocation; VAR adapters:T_adapters):P_rule;
      PROCEDURE updateLists(VAR userDefinedRules:T_listOfString);
      PROCEDURE complainAboutUncalled(CONST inMainPackage:boolean; VAR adapters:T_adapters);
      FUNCTION getDoc:P_userPackageDocumentation;
      PROCEDURE printHelpOnMain(VAR adapters:T_adapters);
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean;
      FUNCTION getPackageReferenceForId(CONST id:string; VAR adapters:T_adapters):T_packageReference;
      FUNCTION isReady:boolean;
  end;

PROCEDURE reloadMainPackage(CONST usecase:T_packageLoadUsecase; VAR context:T_evaluationContext);
PROCEDURE callMainInMain(CONST parameters:T_arrayOfString; VAR context:T_evaluationContext);
PROCEDURE printMainPackageDocText(VAR adapters:T_adapters);
FUNCTION getMainPackage:P_package;
PROCEDURE findAndDocumentAllPackages;
PROCEDURE reduceExpression(VAR first:P_token; CONST callDepth:word; VAR context:T_evaluationContext);

PROCEDURE runAlone(CONST input:T_arrayOfString; adapter:P_adapters);
FUNCTION runAlone(CONST input:T_arrayOfString):T_storedMessages;
FUNCTION demoCallToHtml(CONST input:T_arrayOfString):T_arrayOfString;

FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_evaluationContext):P_expressionLiteral;

VAR mainPackageProvider:T_codeProvider;

{$undef include_interface}
IMPLEMENTATION
CONST STACK_DEPTH_LIMIT=60000;
VAR MAX_NUMBER_OF_SECONDARY_WORKER_THREADS:longint=3;
    secondaryPackages:array of P_package;
    mainPackage      :T_package;
    packagesAreFinalized:boolean=false;
    pendingTasks:T_taskQueue;

PROCEDURE runAlone(CONST input:T_arrayOfString; adapter:P_adapters);
  VAR context:T_evaluationContext;
      codeProvider:P_codeProvider;
      package:T_package;
  begin
    context.create(adapter);
    new(codeProvider,create);
    codeProvider^.setLines(input);
    package.create(codeProvider);
    package.load(lu_forDirectExecution,context,C_EMPTY_STRING_ARRAY);
    package.destroy;
    context.destroy;
  end;

FUNCTION runAlone(CONST input:T_arrayOfString):T_storedMessages;
  VAR collector:T_collectingOutAdapter;
      adapter:T_adapters;
      i:longint;
  begin
    collector.create;
    adapter.create;
    adapter.addOutAdapter(@collector,false);
    adapter.minErrorLevel:=0;
    adapter.doEchoDeclaration:=true;
    adapter.doEchoInput:=true;
    adapter.doShowExpressionOut:=true;
    adapter.doShowTimingInfo:=true;

    runAlone(input,@adapter);

    setLength(result,length(collector.storedMessages));
    for i:=0 to length(result)-1 do result[i]:=collector.storedMessages[i];

    adapter.destroy;
    collector.destroy;
  end;

FUNCTION demoCallToHtml(CONST input:T_arrayOfString):T_arrayOfString;
  VAR messages:T_storedMessages;
      i:longint;
      tmp:ansistring;
  begin
    messages:=runAlone(input);
    setLength(result,0);
    for i:=0 to length(input)-1 do begin
      tmp:=trim(input[i]);
      if copy(tmp,1,2)='//'
      then append(result,StringOfChar(' ',length(C_errorLevelTxt[mt_echo_input])+1)+toHtmlCode(tmp))
      else append(result,                        C_errorLevelTxt[mt_echo_input]+' '+toHtmlCode(tmp));
    end;
    for i:=0 to length(messages)-1 do with messages[i] do case messageType of
      mt_printline: append(result,multiMessage);
      mt_echo_output: append(result,C_errorLevelTxt[mt_echo_output]+' '+toHtmlCode(simpleMessage));
      mt_el1_note,
      mt_el2_warning: append(result,C_errorLevelTxt[messageType]+' '+simpleMessage);
      mt_el3_evalError,
      mt_el3_noMatchingMain,
      mt_el3_stackTrace,
      mt_el4_parsingError,
      mt_el5_systemError,
      mt_el5_haltMessageReceived: append(result,span('error',C_errorLevelTxt[messageType]+' '+simpleMessage));
      mt_imageCreated: begin
        tmp:=ExtractFileName(simpleMessage);
        CopyFile(simpleMessage,htmlRoot+DirectorySeparator+tmp);
        append(result,'Image created: '+imageTag(tmp));
      end;
    end;
  end;

FUNCTION guessPackageForProvider(CONST providerPath:ansistring):P_package;
  VAR packId:string;
      i:longint;
  begin
    if providerPath=mainPackage.codeProvider^.getPath then exit(@mainPackage);
    for i:=0 to length(secondaryPackages)-1 do
      if providerPath=secondaryPackages[i]^.codeProvider^.getPath then exit(secondaryPackages[i]);
    if (providerPath='?') or (providerPath='') then exit(@mainPackage);
    packId:=filenameToPackageId(providerPath);
    if packId=mainPackageProvider.id then exit(@mainPackage);
    if packId=mainPackage.codeProvider^.id then exit(@mainPackage);
    for i:=0 to length(mainPackage.packageUses)-1 do
      if packId=mainPackage.packageUses[i].id then exit(mainPackage.packageUses[i].pack);
    for i:=0 to length(secondaryPackages)-1 do
      if packId=secondaryPackages[i]^.codeProvider^.id then exit(secondaryPackages[i]);
    result:=@mainPackage;
  end;

FUNCTION guessPackageForToken(CONST token:T_token):P_package;
  begin
    result:=guessPackageForProvider(token.location.fileName);
  end;

{$define include_implementation}
{$include mnh_tokens_token.inc}
{$include mnh_tokens_recycler.inc}
{$include mnh_tokens_pattern.inc}
{$include mnh_tokens_subrule.inc}
{$include mnh_tokens_futureTask.inc}
{$include mnh_tokens_procBlock.inc}
{$include mnh_tokens_rule.inc}
{$include mnh_tokens_funcs.inc}

PROCEDURE reloadMainPackage(CONST usecase:T_packageLoadUsecase; VAR context:T_evaluationContext);
  VAR i,j:longint;
      used:T_listOfString;
  begin
    clearAllCaches;
    context.adapters^.clearErrors;
    mainPackage.load(usecase,context,C_EMPTY_STRING_ARRAY);
    //housekeeping:-------------------------------------------------------------
    clearAllCaches;
    used.create;
    for j:=0 to length(mainPackage.packageUses)-1 do used.add(mainPackage.packageUses[j].id);
    for i:=0 to length(secondaryPackages)-1 do
    for j:=0 to length(secondaryPackages[i]^.packageUses)-1 do used.add(secondaryPackages[i]^.packageUses[j].id);
    used.unique;
    j:=0;
    for i:=0 to length(secondaryPackages)-1 do begin
      if used.contains(secondaryPackages[i]^.codeProvider^.id) then begin
        if j<>i then secondaryPackages[j]:=secondaryPackages[i]; inc(j);
      end else dispose(secondaryPackages[j],destroy);
    end;
    setLength(secondaryPackages,j);
    used.destroy;
    //-------------------------------------------------------------:housekeeping
    context.adapters^.raiseCustomMessage(mt_endOfEvaluation,'',C_nilTokenLocation);
  end;

PROCEDURE finalizePackages;
  VAR i:longint;
  begin
    if packagesAreFinalized then exit;
    mainPackage.destroy;
    mainPackageProvider.destroy;
    clearAllCaches;
    for i:=length(secondaryPackages)-1 downto 0 do dispose(secondaryPackages[i],destroy);
    setLength(secondaryPackages,0);
    packagesAreFinalized:=true;
  end;

PROCEDURE loadPackage(VAR pack:T_packageReference; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext);
  VAR i:longint;
      newSource:P_codeProvider=nil;
  begin
    for i:=0 to length(secondaryPackages)-1 do
      if secondaryPackages[i]^.codeProvider^.id = pack.id then begin
        if secondaryPackages[i]^.ready then begin
          if secondaryPackages[i]^.needReload then secondaryPackages[i]^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
          pack.pack:=secondaryPackages[i];
          exit;
        end else begin
          context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Cyclic package dependencies encountered; already loading "'+pack.id+'"',tokenLocation);
          exit;
        end;
      end;
    new(newSource,create(pack.path));
    new(pack.pack,create(newSource));
    setLength(secondaryPackages,length(secondaryPackages)+1);
    secondaryPackages[length(secondaryPackages)-1]:=pack.pack;
    pack.pack^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
  end;

CONSTRUCTOR T_packageReference.create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters);
  begin
    id:=packId;
    path:=locateSource(root,id);
    if path='' then adapters.raiseCustomMessage(mt_el4_parsingError,'Cannot locate package for id "'+id+'"',tokenLocation);
    pack:=nil;
  end;

DESTRUCTOR T_packageReference.destroy;
  begin
    id:='';
    path:='';
    pack:=nil;
  end;


PROCEDURE T_package.load(CONST usecase:T_packageLoadUsecase; VAR context:T_evaluationContext; CONST mainParameters:T_arrayOfString);
  VAR isFirstLine:boolean=true;
      lastComment:ansistring;
      timeForTokenizing:double=0;
      timeForDeclarations:double=0;
      timeForInterpretation:double=0;

  PROCEDURE interpret(VAR first:P_token; CONST semicolonPosition:T_tokenLocation);
    PROCEDURE interpretUseClause;
      VAR temp:P_token;
          i,j:longint;
          locationForErrorFeedback:T_tokenLocation;
          newId:string;
          rulesSet:T_ruleMap.KEY_VALUE_LIST;
          dummyRule:P_rule;
      begin
        locationForErrorFeedback:=first^.location;
        temp:=first; first:=context.disposeToken(temp);
        while first<>nil do begin
          if first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule] then begin
            newId:=first^.txt;
            if isQualified(newId) then begin
              context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Cannot interpret use clause containing qualified identifier '+first^.singleTokenToString,first^.location);
              exit;
            end;
            //no duplicates are created; packages are always added at the end
            i:=0;
            while (i<length(packageUses)) and (packageUses[i].id<>newId) do inc(i);
            if i<length(packageUses) then for j:=i to length(packageUses)-2 do packageUses[j]:=packageUses[j+1]
                                     else setLength(packageUses,length(packageUses)+1);
            packageUses[length(packageUses)-1].create(codeProvider^.getPath,first^.txt,first^.location,context.adapters^);
          end else if first^.tokType<>tt_separatorComma then begin
            context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Cannot interpret use clause containing '+first^.singleTokenToString,first^.location);
            exit;
          end;
          temp:=first; first:=context.disposeToken(temp);
        end;
        if usecase<>lu_forDocGeneration then begin
          for i:=0 to length(packageUses)-1 do loadPackage(packageUses[i],locationForErrorFeedback,context);
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
               importedRules.put(packageUses[i].id+C_ID_QUALIFY_CHARACTER+rulesSet[j].key,rulesSet[j].value);
             end;
          end;
        end;
      end;

    VAR assignmentToken:P_token;

    PROCEDURE parseRule;
      VAR p,n,nn,nnn:P_token;
          ruleIsPrivate:boolean=false;
          ruleIsMemoized:boolean=false;
          ruleIsMutable:boolean=false;
          ruleIsPersistent:boolean=false;
          ruleIsSynchronized:boolean=false;
          ruleId:string;
          evaluateBody:boolean;
          rulePattern:T_pattern;
          ruleBody:P_token;
          ruleDeclarationStart:T_tokenLocation;
          subRule:P_subrule;
          ruleGroup:P_rule;
      begin
        ruleDeclarationStart:=first^.location;
        evaluateBody:=(assignmentToken^.tokType=tt_assign);
        ruleBody:=assignmentToken^.next;


        //plausis:
        if (ruleBody=nil) then begin
          context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Missing function body after assignment/declaration token.',assignmentToken^.location);
          context.cascadeDisposeToken(first);
          exit;
        end;
        while (first<>nil) and (first^.tokType in [tt_modifier_private,tt_modifier_memoized,tt_modifier_mutable,tt_modifier_persistent,tt_modifier_synchronized]) do begin
          if first^.tokType=tt_modifier_private      then ruleIsPrivate :=true;
          if first^.tokType=tt_modifier_memoized     then ruleIsMemoized:=true;
          if first^.tokType=tt_modifier_synchronized then ruleIsSynchronized:=true;
          if first^.tokType in [tt_modifier_mutable,tt_modifier_persistent]  then begin
            ruleIsMutable :=true;
            ruleIsPersistent:=(first^.tokType=tt_modifier_persistent);
            evaluateBody:=true;
          end;
          first:=context.disposeToken(first);
        end;
        if not(first^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule]) then begin
          context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Declaration does not start with an identifier.',first^.location);
          context.cascadeDisposeToken(first);
          exit;
        end;
        p:=first;
        while (p<>nil) and not(p^.tokType in [tt_assign,tt_declare]) do begin
          if (p^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_intrinsicRule]) and isQualified(first^.txt) then begin
            context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Declaration head contains qualified ID.',p^.location);
            context.cascadeDisposeToken(first);
            exit;
          end;
          p:=p^.next;
        end;
        //:plausis

        ruleId:=trim(first^.txt);
        first:=context.disposeToken(first);
        if not(first^.tokType in [tt_braceOpen,tt_assign,tt_declare])  then begin
          context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Invalid declaration head.',first^.location);
          context.cascadeDisposeToken(first);
          exit;
        end;
        rulePattern.create;
        if first^.tokType=tt_braceOpen then begin
          first:=context.disposeToken(first);
          if (first<>nil) and (first^.tokType=tt_braceClose) then first:=context.disposeToken(first);
          while not((first=nil) or (first^.tokType in [tt_assign,tt_declare])) do begin
            if rulePattern.hasOptionals then context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Optional parameters are allowed only as last entry in a function head declaration.',ruleDeclarationStart);
            n  :=first^.next;
            nn :=n    ^.next;
            nnn:=nn   ^.next;
            if (first^.tokType=tt_identifier)
            and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendFreeId(first^.txt);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
            end else if (first^.tokType=tt_optionalParameters)
                     and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              if n^.tokType=tt_separatorComma then context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Optional parameters are allowed only as last entry in a function head declaration.',ruleDeclarationStart);
              rulePattern.appendOptional;
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType in [lt_boolean, lt_int, lt_real, lt_string])
                     and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison('',tt_comparatorEq,P_scalarLiteral(first^.data));
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_emptyList) then begin
              rulePattern.appendTypeCheck('',tt_typeCheckEmptyList);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
            end else if (first^.tokType=tt_listBraceOpen) and (first^.next<>nil) and (first^.next^.tokType=tt_listBraceClose) then begin
              rulePattern.appendTypeCheck('',tt_typeCheckEmptyList);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_typeCheckScalar, tt_typeCheckList, tt_typeCheckBoolean, tt_typeCheckBoolList, tt_typeCheckInt, tt_typeCheckIntList, tt_typeCheckReal,tt_typeCheckRealList, tt_typeCheckString,tt_typeCheckStringList, tt_typeCheckNumeric, tt_typeCheckNumList, tt_typeCheckExpression, tt_typeCheckNonemptyList, tt_typeCheckEmptyList, tt_typeCheckKeyValueList])
                     and (nn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendTypeCheck(first^.txt,n^.tokType);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
                     and (nn^.tokType=tt_literal) and (P_literal(nn^.data)^.literalType in [lt_boolean, lt_int, lt_real, lt_string])
                     and (nnn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison(first^.txt,n^.tokType,P_scalarLiteral(nn^.data));
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
                     and (nn^.tokType=tt_identifier)
                     and (nnn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison(first^.txt,n^.tokType,nn^.txt);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
            end else begin
              context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Invalid declaration pattern element: '+tokensToString(first,10) ,first^.location);
              context.cascadeDisposeToken(first);
              exit;
            end;
          end;
        end;
        if first<>nil then begin
          first:=context.disposeToken(first);
        end else begin
          context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Invalid declaration.',ruleDeclarationStart);
          exit;
        end;

        if (usecase=lu_forDocGeneration) then begin
          context.cascadeDisposeToken(ruleBody);
          ruleBody:=context.newToken(C_nilTokenLocation,'',tt_literal,newVoidLiteral);
        end else begin
          rulePattern.toParameterIds(ruleBody);
          if evaluateBody then reduceExpression(ruleBody,0,context);
        end;

        if context.adapters^.noErrors then begin
          ruleGroup:=ensureRuleId(ruleId,ruleIsPrivate, ruleIsMemoized,ruleIsMutable,ruleIsPersistent, ruleIsSynchronized,ruleDeclarationStart,semicolonPosition,context.adapters^);
          if context.adapters^.noErrors then begin
            new(subRule,create(rulePattern,ruleBody,ruleDeclarationStart,ruleIsPrivate,context));
            subRule^.comment:=lastComment; lastComment:='';
            if ruleGroup^.ruleType in [rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private]
            then begin
              ruleGroup^.setMutableValue(subRule^.getInlineValue,true,context);
              dispose(subRule,destroy);
            end else ruleGroup^.addOrReplaceSubRule(subRule,context);
            first:=nil;
          end else if not(context.adapters^.hasMessageOfType[mt_el5_systemError] or context.adapters^.hasMessageOfType[mt_el5_haltMessageReceived]) then
            context.cascadeDisposeToken(first)
          else
            first:=nil;
        end else if (context.adapters^.hasMessageOfType[mt_el5_systemError] or context.adapters^.hasMessageOfType[mt_el5_haltMessageReceived]) then
          context.cascadeDisposeToken(first)
        else
          first:=nil;
      end;
    VAR startTime:double;
    begin
      if first=nil then exit;
      if isFirstLine then begin
        isFirstLine:=false;
        if (first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule]) and
           (first^.txt    ='USE') and
           (first^.next   <>nil) and
           (first^.next^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule])
        then begin
          interpretUseClause;
          exit;
        end;
      end;
      assignmentToken:=first^.getDeclarationOrAssignmentToken;
      if assignmentToken<>nil then begin
        startTime:=now;
        predigest(assignmentToken,@self,context);
        if context.adapters^.doEchoDeclaration then context.adapters^.raiseCustomMessage(mt_echo_declaration, tokensToString(first,maxLongint)+';',first^.location);
        parseRule;
        timeForDeclarations:=timeForDeclarations+now-startTime;
      end else begin
        if (usecase=lu_forDirectExecution) then begin
          startTime:=now;
          predigest(first,@self,context);
          if context.adapters^.doEchoInput then context.adapters^.raiseCustomMessage(mt_echo_input, tokensToString(first,maxLongint)+';',first^.location);
          reduceExpression(first,0,context);
          timeForInterpretation:=timeForInterpretation+now-startTime;
          if (first<>nil) and context.adapters^.doShowExpressionOut then context.adapters^.raiseCustomMessage(mt_echo_output, tokensToString(first,maxLongint),first^.location);
        end else context.adapters^.raiseNote('Skipping expression '+tokensToString(first,20),first^.location);
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
        startTime:double;
    begin
      if not(ready) or not(context.adapters^.noErrors) then begin
        context.adapters^.raiseCustomMessage(mt_el5_systemError,'Call of main has been rejected due to a previous error.',fileTokenLocation(codeProvider));
        context.destroy;
        exit;
      end;
      if not(packageRules.containsKey('main',mainRule)) then begin
        context.adapters^.raiseError('The specified package contains no main rule.',fileTokenLocation(codeProvider));
      end else begin
        t:=context.newToken(fileTokenLocation(@mainPackageProvider),'main',tt_localUserRule,mainRule);
        parametersForMain:=newListLiteral;
        parametersForMain^.rereference;
        for i:=0 to length(mainParameters)-1 do parametersForMain^.appendString(mainParameters[i]);
        t^.next:=context.newToken(fileTokenLocation(@mainPackageProvider),'',tt_parList,parametersForMain);
        startTime:=now;
        reduceExpression(t,0,context);
        timeForInterpretation:=timeForInterpretation+now-startTime;
        //special handling if main returns an expression:
        if (t<>nil) and (t^.tokType=tt_literal) and (t^.next=nil) and
           (P_literal(t^.data)^.literalType=lt_expression) then begin
          P_subrule(P_expressionLiteral(t^.data)^.value)^.directEvaluateNullary(nil,0,context);
        end;
        //:special handling if main returns an expression
        if context.adapters^.hasMessageOfType[mt_el3_noMatchingMain] then begin
          context.adapters^.printOut('');
          context.adapters^.printOut('Try one of the following:');
          context.adapters^.printOut('');
          mainPackage.printHelpOnMain(context.adapters^);
        end;
        context.cascadeDisposeToken(t);
      end;
      disposeLiteral(parametersForMain);
      parametersForMain:=nil;

    end;

  VAR localIdStack:T_idStack;
      first,last:P_token;
      startTime:double;
  begin
    clear;
    if ((usecase=lu_forCallingMain) or (codeProvider<>@mainPackageProvider))
    and codeProvider^.fileHasChanged then codeProvider^.load;
    startTime:=now;
    fileTokens:=tokenizeAll(codeProvider,@self,context.adapters^);
    timeForTokenizing:=now-startTime;
    fileTokens.step(@self,lastComment);
    first:=nil;
    last :=nil;
    localIdStack.create;
    while not(fileTokens.atEnd) do begin
      if fileTokens.current.tokType=tt_begin then begin
        if first=nil then begin
          first:=context.newToken(fileTokens.current); fileTokens.current.undefine;
          last :=first;
        end else begin
          last^.next:=context.newToken(fileTokens.current); fileTokens.current.undefine;
          last      :=last^.next;
        end;

        localIdStack.clear;
        localIdStack.scopePush;
        fileTokens.step(@self,lastComment);
        while (not(fileTokens.atEnd)) and not((fileTokens.current.tokType=tt_end) and (localIdStack.oneAboveBottom)) do begin
          case fileTokens.current.tokType of
            tt_begin: localIdStack.scopePush;
            tt_end  : localIdStack.scopePop;
            tt_identifier: if (last^.tokType=tt_modifier_local) then begin
              fileTokens.mutateCurrentTokType(tt_blockLocalVariable);
              localIdStack.addId(fileTokens.current.txt);
            end else if (localIdStack.hasId(fileTokens.current.txt)) then
              fileTokens.mutateCurrentTokType(tt_blockLocalVariable);
          end;
          last^.next:=context.newToken(fileTokens.current); fileTokens.current.undefine;
          last      :=last^.next;
          fileTokens.step(@self,lastComment);
        end;

      end else if (fileTokens.current.tokType=tt_semicolon) then begin
        if first<>nil then interpret(first,fileTokens.current.location);
        last:=nil;
        first:=nil;
        fileTokens.step(@self,lastComment);
      end else begin
        if first=nil then begin
          first:=context.newToken(fileTokens.current); fileTokens.current.undefine;
          last :=first
        end else begin
          last^.next:=context.newToken(fileTokens.current); fileTokens.current.undefine;
          last      :=last^.next;
        end;
        last^.next:=nil;
        fileTokens.step(@self,lastComment);
      end;
    end;
    localIdStack.destroy;
    if (context.adapters^.noErrors)
    then begin if first<>nil then interpret(first,C_nilTokenLocation); end
    else context.cascadeDisposeToken(first);
    ready:=usecase<>lu_forDocGeneration;

    if usecase=lu_forCallingMain then executeMain;
    if usecase in [lu_forDirectExecution,lu_forCallingMain] then begin
      if context.adapters^.noErrors then complainAboutUncalled(true,context.adapters^);
      if context.adapters^.noErrors then finalize(context.adapters^);
      context.adapters^.raiseCustomMessage(mt_timing_info,'Tokenizing time     '+myTimeToStr(timeForTokenizing),C_nilTokenLocation);
      context.adapters^.raiseCustomMessage(mt_timing_info,'Declaration time    '+myTimeToStr(timeForDeclarations),C_nilTokenLocation);
      context.adapters^.raiseCustomMessage(mt_timing_info,'Interpretation time '+myTimeToStr(timeForInterpretation),C_nilTokenLocation);
    end;
  end;

CONSTRUCTOR T_package.create(CONST provider: P_codeProvider);
  begin
    setLength(packageUses,0);
    codeProvider:=provider;
    packageRules.create;
    importedRules.create;
  end;

FUNCTION T_package.needReload: boolean;
  begin
    result:=codeProvider^.fileHasChanged;
  end;

PROCEDURE T_package.clear;
  VAR rule:P_rule;
  begin
    while packageRules.size>0 do begin
      rule:=packageRules.dropAny;
      dispose(rule,destroy);
    end;
    packageRules.clear;
    importedRules.clear;
    setLength(packageUses,0);
    ready:=false;
  end;

PROCEDURE T_package.finalize(VAR adapters:T_adapters);
  VAR ruleList:array of P_rule;
      wroteBack:boolean=false;
      i:longint;

  begin
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do if ruleList[i]^.writeBack(codeProvider^,adapters) then wroteBack:=true;
    setLength(ruleList,0);
    if wroteBack then begin
      codeProvider^.save;
      if @self=@mainPackage then adapters.raiseCustomMessage(mt_reloadRequired,'Persisting package '+codeProvider^.id,C_nilTokenLocation);
    end;
    for i:=0 to length(packageUses)-1 do packageUses[i].pack^.finalize(adapters);
  end;

DESTRUCTOR T_package.destroy;
  begin
    clear;
    if codeProvider<>@mainPackageProvider then dispose(codeProvider,destroy);
    packageRules.destroy;
    importedRules.destroy;
    setLength(packageUses,0);
  end;

PROCEDURE T_package.resolveRuleId(VAR token: T_token; CONST adaptersOrNil:P_adapters);
  VAR userRule:P_rule;
      intrinsicFuncPtr:T_intFuncCallback;
      ruleId:ansistring;
  begin
    ruleId   :=token.txt;
    if packageRules.containsKey(ruleId,userRule) then begin
      if token.tokType=tt_identifier_pon
      then token.tokType:=tt_localUserRule_pon
      else token.tokType:=tt_localUserRule;
      token.data:=userRule;
      userRule^.used:=true;
      exit;
    end;
    if importedRules.containsKey(ruleId,userRule) then begin
      if token.tokType=tt_identifier_pon
      then token.tokType:=tt_importedUserRule_pon
      else token.tokType:=tt_importedUserRule;
      token.data:=userRule;
      userRule^.used:=true;
      exit;
    end;
    if intrinsicRuleMap.containsKey(ruleId,intrinsicFuncPtr) then begin
      if token.tokType=tt_identifier_pon
      then token.tokType:=tt_intrinsicRule_pon
      else token.tokType:=tt_intrinsicRule;
      token.data:=intrinsicFuncPtr;
      exit;
    end;
    if adaptersOrNil<>nil then adaptersOrNil^.raiseCustomMessage(mt_el4_parsingError,'Cannot resolve ID "'+token.txt+'"',token.location);
  end;

FUNCTION T_package.ensureRuleId(CONST ruleId: ansistring; CONST ruleIsPrivate,ruleIsMemoized,ruleIsMutable,ruleIsPersistent, ruleIsSynchronized:boolean; CONST ruleDeclarationStart,ruleDeclarationEnd:T_tokenLocation; VAR adapters:T_adapters): P_rule;
  VAR ruleType:T_ruleType=rt_normal;
  begin
    if ruleIsSynchronized then ruleType:=rt_synchronized;
    if ruleIsMemoized then ruleType:=rt_memoized;
    if ruleIsMutable then begin
      if ruleIsPrivate then ruleType:=rt_mutable_private
                       else ruleType:=rt_mutable_public;
    end;
    if ruleIsPersistent then begin
      if ruleIsPrivate then ruleType:=rt_persistent_private
                       else ruleType:=rt_persistent_public;
    end;

    if not(packageRules.containsKey(ruleId,result)) then begin
      new(result,create(ruleId,ruleType,ruleDeclarationStart));
      packageRules.put(ruleId,result);
    end else if (result^.ruleType<>ruleType) and (ruleType<>rt_normal) then begin
      adapters.raiseCustomMessage(mt_el4_parsingError,'Colliding modifiers! Rule '+ruleId+' is '+C_ruleTypeText[result^.ruleType]+', redeclared as '+C_ruleTypeText[ruleType],ruleDeclarationStart);
    end;
    result^.declarationEnd:=ruleDeclarationEnd;
  end;

PROCEDURE T_package.updateLists(VAR userDefinedRules: T_listOfString);
  VAR i,j:longint;
      ids:T_arrayOfString;
      packageId:ansistring;
  begin
    userDefinedRules.clear;
    userDefinedRules.addAll(packageRules.keySet);
    for i:=0 to length(packageUses)-1 do if (packageUses[i].pack<>nil) and packageUses[i].pack^.ready then begin
      packageId:=packageUses[i].id;
      ids:=packageUses[i].pack^.packageRules.keySet;
      for j:=0 to length(ids)-1 do begin
        userDefinedRules.add(ids[j]);
        userDefinedRules.add(C_ID_QUALIFY_CHARACTER+ids[j]);
        userDefinedRules.add(packageId+C_ID_QUALIFY_CHARACTER+ids[j]);
      end;
    end;
    userDefinedRules.unique;
  end;

PROCEDURE T_package.complainAboutUncalled(CONST inMainPackage:boolean; VAR adapters:T_adapters);
  VAR ruleList:array of P_rule;
      i:longint;
      anyCalled:boolean=false;
  begin
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do if ruleList[i]^.complainAboutUncalled(inMainPackage,adapters) then anyCalled:=true;
    if not(anyCalled) and not(inMainPackage) then adapters.raiseWarning('Unused package '+codeProvider^.id,C_nilTokenLocation);
    if inMainPackage then begin
      for i:=0 to length(packageUses)-1 do begin
        packageUses[i].pack^.complainAboutUncalled(false,adapters);
      end;
    end;
    setLength(ruleList,0);
  end;

FUNCTION T_package.getDoc:P_userPackageDocumentation;
  VAR ruleList:array of P_rule;
      i:longint;
  begin
    new(result,create(codeProvider^.getPath,codeProvider^.id));
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do begin
      result^.addRuleDoc(ruleList[i]^.getDocHtml);
      if ruleList[i]^.id='main' then result^.isExecutable:=true;
    end;
    setLength(ruleList,0);
    for i:=0 to length(packageUses)-1 do result^.addUses(packageUses[i].path);
  end;

PROCEDURE T_package.printHelpOnMain(VAR adapters:T_adapters);
  VAR mainRule:P_rule;
      docText:T_arrayOfString;
      i:longint;
  begin
    if not(packageRules.containsKey('main',mainRule))
    then adapters.printOut('The package contains no main rule')
    else begin
      docText:=split(mainRule^.getDocTxt,C_lineBreakChar);
      for i:=0 to 1 do adapters.printOut(docText[i]);
      dropFirst(docText,2);
      adapters.printOut(formatTabs(docText));
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

FUNCTION T_package.getPackageReferenceForId(CONST id:string; VAR adapters:T_adapters):T_packageReference;
  VAR i:longint;
  begin
    for i:=0 to length(packageUses)-1 do if packageUses[i].id=id then exit(packageUses[i]);
    result.create(codeProvider^.getPath,'',C_nilTokenLocation,adapters);
  end;

FUNCTION T_package.isReady:boolean; begin result:=ready; end;

PROCEDURE callMainInMain(CONST parameters:T_arrayOfString; VAR context:T_evaluationContext);
  begin
    context.adapters^.clearErrors;
    mainPackage.load(lu_forCallingMain,context,parameters);
    context.adapters^.raiseCustomMessage(mt_endOfEvaluation,'',C_nilTokenLocation);
  end;

PROCEDURE printMainPackageDocText(VAR adapters:T_adapters);
  VAR silentContext:T_evaluationContext;
  begin
    silentContext.create(P_adapters(@nullAdapter));
    nullAdapter.clearErrors;
    mainPackageProvider.load;
    reloadMainPackage(lu_forDocGeneration,silentContext);
    mainPackage.printHelpOnMain(adapters);
    silentContext.destroy;
  end;

FUNCTION getMainPackage: P_package;
  begin
    result:=@mainPackage;
  end;

PROCEDURE findAndDocumentAllPackages;
  VAR sourceNames:T_arrayOfString;
      i:longint;
      p:T_package;
      provider:P_codeProvider;
      context:T_evaluationContext;
  begin
    context.create(P_adapters(@nullAdapter));
    sourceNames:=locateSources;
    for i:=0 to length(sourceNames)-1 do begin
      new(provider,create(sourceNames[i]));
      p.create(provider);
      nullAdapter.clearErrors;
      p.load(lu_forDocGeneration,context,C_EMPTY_STRING_ARRAY);
      addPackageDoc(p.getDoc);
      p.destroy;
    end;
    makeHtmlFromTemplate;
    context.destroy;
    nullAdapter.clearErrors;
  end;

{$undef include_implementation}
INITIALIZATION
{$define include_initialization}
  mainPackageProvider.create;
  mainPackage.create(@mainPackageProvider);
  setLength(secondaryPackages,0);
  pendingTasks.create;

  //callbacks in mnh_litvar:
  disposeSubruleCallback :=@disposeSubruleImpl;
  subruleToStringCallback:=@subruleToStringImpl;
  subruleApplyOpCallback :=@subruleApplyOpImpl;
  evaluateCompatorCallback:=@evaluateComparator;
  //callbacks in doc
  demoCodeToHtmlCallback:=@demoCallToHtml;
  //worker thread setup
  MAX_NUMBER_OF_SECONDARY_WORKER_THREADS:=getNumberOfCPUs-1;
  if MAX_NUMBER_OF_SECONDARY_WORKER_THREADS<1 then
     MAX_NUMBER_OF_SECONDARY_WORKER_THREADS:=1;
  {$include mnh_tokens_funcs.inc}

FINALIZATION
  pendingTasks.destroy;
  finalizePackages;

end.
