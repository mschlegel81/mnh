UNIT mnh_packages;
INTERFACE
USES myGenerics, mnh_constants, math, sysutils, myStringUtil,typinfo, mySys, FileUtil, //utilities
     mnh_litVar, mnh_fileWrappers, mnh_tokLoc, mnh_tokens, mnh_contexts, //types
     EpikTimer,
     mnh_funcs, mnh_out_adapters, mnh_caches, mnh_html, mnh_settings, //even more specific
     {$ifdef fullVersion}mnh_doc,{$endif}
     mnh_funcs_mnh, mnh_funcs_math, mnh_funcs_strings, mnh_funcs_list, mnh_funcs_system,
     mnh_funcs_regex;

{$define include_interface}
TYPE
  P_package=^T_package;
  {$include mnh_tokens_token.inc}
  {$include mnh_tokens_pattern.inc}
  P_rule=^T_rule;
  T_ruleMap=specialize G_stringKeyMap<P_rule>;
  {$include mnh_tokens_subrule.inc}
  {$include mnh_tokens_rule.inc}
  {$include mnh_tokens_futureTask.inc}
  {$include mnh_tokens_procBlock.inc}
  {$include mnh_tokens_fmtStmt.inc}
  T_packageLoadUsecase=(lu_forImport,lu_forCallingMain,lu_forDirectExecution,lu_forDocGeneration,lu_interactiveMode);

  T_packageReference=object
    id,path:ansistring;
    pack:P_package;
    CONSTRUCTOR create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
    DESTRUCTOR destroy;
  end;

  { T_package }
  T_package=object
    private
      packageRules,importedRules:T_ruleMap;
      packageUses:array of T_packageReference;
      ready:boolean;
      codeProvider:P_codeProvider;
      statementHashes:array of T_hashInt;
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
      {$ifdef fullVersion}FUNCTION getDoc:P_userPackageDocumentation; {$endif}
      PROCEDURE printHelpOnMain(VAR adapters:T_adapters);
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean;
      FUNCTION getPackageReferenceForId(CONST id:string; CONST adapters:P_adapters):T_packageReference;
      FUNCTION isReady:boolean;

      PROCEDURE reportVariables(VAR variableReport:T_variableReport);
    end;

PROCEDURE reloadMainPackage(CONST usecase:T_packageLoadUsecase; VAR context:T_evaluationContext);
PROCEDURE callMainInMain(CONST parameters:T_arrayOfString; VAR context:T_evaluationContext);
PROCEDURE printMainPackageDocText(VAR adapters:T_adapters);
{$ifdef fullVersion}
PROCEDURE findAndDocumentAllPackages;
{$endif}
PROCEDURE reduceExpression(VAR first:P_token; CONST callDepth:word; VAR context:T_evaluationContext);

PROCEDURE runAlone(CONST input:T_arrayOfString; adapter:P_adapters);
FUNCTION runAlone(CONST input:T_arrayOfString):T_storedMessages;
FUNCTION demoCallToHtml(CONST input:T_arrayOfString):T_arrayOfString;

FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_evaluationContext):P_expressionLiteral;

FUNCTION getFormat(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_preparedFormatStatement;
FUNCTION stringToLiteral(CONST s:ansistring; CONST location:T_tokenLocation; CONST package:P_package; VAR context:T_evaluationContext):P_literal;

TYPE T_packageEnvironment=record
       mainPackageProvider:P_codeProvider;
       secondaryPackages:array of P_package;
       mainPackage      :P_package;
     end;

VAR environment:T_packageEnvironment;
    killServersCallback:PROCEDURE;
{$undef include_interface}
IMPLEMENTATION
CONST STACK_DEPTH_LIMIT=60000;
VAR pendingTasks     :T_taskQueue;
    timer: specialize G_lazyVar<TEpikTimer>;

PROCEDURE runAlone(CONST input:T_arrayOfString; adapter:P_adapters);
  VAR context:T_evaluationContext;
      codeProvider:P_codeProvider;
      package:T_package;
      stash:T_packageEnvironment;
      i:longint;
  begin
    //stash current environment
    stash.mainPackage:=environment.mainPackage;
    stash.mainPackageProvider:=environment.mainPackageProvider;
    setLength(stash.secondaryPackages,length(environment.secondaryPackages));
    for i:=0 to length(stash.secondaryPackages)-1 do stash.secondaryPackages[i]:=environment.secondaryPackages[i];
    setLength(environment.secondaryPackages,0);

    context.createSanboxContext(adapter);
    new(codeProvider,create);
    codeProvider^.setLines(input);
    package.create(codeProvider);
    environment.mainPackage:=@package;
    environment.mainPackageProvider:=codeProvider;
    package.load(lu_forDirectExecution,context,C_EMPTY_STRING_ARRAY);
    package.destroy;
    context.destroy;
    for i:=length(environment.secondaryPackages)-1 downto 0 do dispose(environment.secondaryPackages[i],destroy);
    setLength(environment.secondaryPackages,0);

    //unstash environment
    environment.mainPackage:=stash.mainPackage;
    environment.mainPackageProvider:=stash.mainPackageProvider;
    setLength(environment.secondaryPackages,length(stash.secondaryPackages));
    for i:=0 to length(environment.secondaryPackages)-1 do environment.secondaryPackages[i]:=stash.secondaryPackages[i];
  end;

FUNCTION runAlone(CONST input:T_arrayOfString):T_storedMessages;
  VAR collector:T_collectingOutAdapter;
      adapter:T_adapters;
      i:longint;
  begin
    collector.create(at_unknown,'');
    adapter.create;
    adapter.addOutAdapter(@collector,false);
    adapter.minErrorLevel:=0;
    adapter.doEchoDeclaration:=true;
    adapter.doEchoInput:=true;
    adapter.doShowExpressionOut:=true;
    adapter.doShowTimingInfo:=false;
    runAlone(input,@adapter);
    setLength(result,length(collector.storedMessages));
    for i:=0 to length(result)-1 do begin
      result[i]:=collector.storedMessages[i];
      with result[i] do if messageType in [mt_debug_step,mt_el3_stackTrace] then simpleMessage:=replaceAll(simpleMessage,#28,' ');
    end;
    adapter.destroy;
    collector.destroy;
  end;

FUNCTION demoCallToHtml(CONST input:T_arrayOfString):T_arrayOfString;
  VAR messages:T_storedMessages;
      i:longint;
      blobLevel:longint=0;
      tmp:ansistring;
  begin
    messages:=runAlone(input);
    setLength(result,0);
    for i:=0 to length(input)-1 do begin
      tmp:=trim(input[i]);
      if copy(tmp,1,2)='//'
      then append(result,StringOfChar(' ',length(C_errorLevelTxt[mt_echo_input])+1)+toHtmlCode(tmp,blobLevel))
      else append(result,                        C_errorLevelTxt[mt_echo_input]+' '+toHtmlCode(tmp,blobLevel));
    end;
    for i:=0 to length(messages)-1 do begin
      blobLevel:=0;
      with messages[i] do case messageType of
        mt_printline: append(result,multiMessage);
        mt_echo_output: append(result,C_errorLevelTxt[mt_echo_output]+' '+toHtmlCode(simpleMessage,blobLevel));
        mt_el1_note,
        mt_el2_warning: append(result,C_errorLevelTxt[messageType]+' '+simpleMessage);
        mt_el3_evalError,
        mt_el3_noMatchingMain,
        mt_el3_stackTrace,
        mt_el4_parsingError,
        mt_el5_systemError,
        mt_el5_haltMessageReceived: append(result,span('error',C_errorLevelTxt[messageType]+' '+simpleMessage));
        {$ifdef fullVersion}
        mt_plotFileCreated: begin
          tmp:=extractFileName(simpleMessage);
          CopyFile(simpleMessage,getHtmlRoot+DirectorySeparator+tmp);
          append(result,'Image created: '+imageTag(tmp));
        end;
        {$endif}
      end;
    end;
  end;

FUNCTION guessPackageForProvider(CONST providerPath:ansistring):P_package;
  VAR packId:string;
      i:longint;
  begin with environment do begin
    if providerPath=mainPackage^.codeProvider^.getPath then exit(mainPackage);
    for i:=0 to length(secondaryPackages)-1 do
      if providerPath=secondaryPackages[i]^.codeProvider^.getPath then exit(secondaryPackages[i]);
    if (providerPath='?') or (providerPath='') then exit(mainPackage);
    packId:=filenameToPackageId(providerPath);
    if packId=mainPackageProvider^.id then exit(mainPackage);
    if packId=mainPackage^.codeProvider^.id then exit(mainPackage);
    for i:=0 to length(mainPackage^.packageUses)-1 do
      if packId=mainPackage^.packageUses[i].id then exit(mainPackage^.packageUses[i].pack);
    for i:=0 to length(secondaryPackages)-1 do
      if packId=secondaryPackages[i]^.codeProvider^.id then exit(secondaryPackages[i]);
    result:=mainPackage;
  end; end;

FUNCTION guessPackageForToken(CONST token:T_token):P_package;
  begin
    result:=guessPackageForProvider(token.location.fileName);
  end;

{$define include_implementation}
{$include mnh_tokens_token.inc}
{$include mnh_tokens_pattern.inc}
{$include mnh_tokens_subrule.inc}
{$include mnh_tokens_futureTask.inc}
{$include mnh_tokens_procBlock.inc}
{$include mnh_tokens_rule.inc}
{$include mnh_tokens_funcs.inc}
{$include mnh_tokens_fmtStmt.inc}

PROCEDURE reloadMainPackage(CONST usecase:T_packageLoadUsecase; VAR context:T_evaluationContext);
  VAR i,j:longint;
      used:T_listOfString;
  begin with environment do begin
    context.adapters^.clearErrors;
    mainPackage^.load(usecase,context,C_EMPTY_STRING_ARRAY);
    //housekeeping:-------------------------------------------------------------
    used.create;
    for j:=0 to length(mainPackage^.packageUses)-1 do used.add(mainPackage^.packageUses[j].id);
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
  end; end;

PROCEDURE loadPackage(VAR pack:T_packageReference; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext);
  VAR i:longint;
      newSource:P_codeProvider=nil;
  begin with environment do begin
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
  end; end;

CONSTRUCTOR T_packageReference.create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation; CONST adapters:P_adapters);
  begin
    id:=packId;
    path:=locateSource(root,id);
    if (path='') and (adapters<>nil) then adapters^.raiseCustomMessage(mt_el4_parsingError,'Cannot locate package for id "'+id+'"',tokenLocation);
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
      evaluateAll:boolean=false;
      lastComment:ansistring;
      profiler:record
        active:boolean;
        unaccounted,
        importing,
        tokenizing,
        declarations,
        interpretation:double;
      end = (active:false;unaccounted:0;importing:0;tokenizing:0;declarations:0;interpretation:0);

  PROCEDURE reloadAllPackages(CONST locationForErrorFeedback:T_tokenLocation);
    VAR i,j:longint;
        rulesSet:T_ruleMap.KEY_VALUE_LIST;
        dummyRule:P_rule;
    begin
      with profiler do if active then importing:=timer.value.Elapsed;
      for i:=0 to length(packageUses)-1 do loadPackage(packageUses[i],locationForErrorFeedback,context);
      with profiler do if active then importing:=timer.value.Elapsed-importing;
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

  PROCEDURE interpret(VAR first:P_token; CONST semicolonPosition:T_tokenLocation);
    PROCEDURE interpretUseClause;
      VAR temp:P_token;
          i,j:longint;
          locationForErrorFeedback:T_tokenLocation;
          newId:string;
          fullClause:string;
      begin
        locationForErrorFeedback:=first^.location;
        fullClause:=tokensToString(first);
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
            packageUses[length(packageUses)-1].create(codeProvider^.getPath,first^.txt,first^.location,context.adapters);
          end else if first^.tokType<>tt_separatorComma then begin
            context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Cannot interpret use clause containing '+first^.singleTokenToString,first^.location);
            context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Full clause: '+fullClause,first^.location);
            exit;
          end;
          temp:=first; first:=context.disposeToken(temp);
        end;
        if not(context.adapters^.noErrors) then context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Full clause: '+fullClause,first^.location);
        if usecase<>lu_forDocGeneration then reloadAllPackages(locationForErrorFeedback);
      end;

    VAR assignmentToken:P_token;
        ruleDeclarationStart:T_tokenLocation;

    PROCEDURE parseRule;
      CONST C_TYPE_RESTRICTIONS_WITH_ADDITIONAL_PARAMETER:set of T_tokenType=[tt_typeCheckExpression,tt_typeCheckList,tt_typeCheckBoolList,tt_typeCheckIntList,tt_typeCheckRealList,tt_typeCheckStringList,tt_typeCheckNumList,tt_typeCheckKeyValueList];

      PROCEDURE fail(VAR firstOfPart:P_token);
        begin
          context.adapters^.raiseCustomMessage(mt_el4_parsingError,'Invalid declaration pattern element: '+tokensToString(firstOfPart,10) ,firstOfPart^.location);
          context.cascadeDisposeToken(firstOfPart);
        end;

      PROCEDURE assertNil(VAR firstOfPart:P_token);
        begin
          if firstOfPart<>nil then fail(firstOfPart);
        end;

      CONST MSG_INVALID_OPTIONAL='Optional parameters are allowed only as last entry in a function head declaration.';
      VAR p:P_token; //iterator
          //modifier flags
          ruleIsPrivate:boolean=false;
          ruleIsMemoized:boolean=false;
          ruleIsMutable:boolean=false;
          ruleIsPersistent:boolean=false;
          ruleIsSynchronized:boolean=false;
          //rule meta data
          ruleId:string;
          evaluateBody:boolean;
          rulePattern:T_pattern;
          rulePatternElement:T_patternElement;
          ruleBody:P_token;
          subRule:P_subrule;
          ruleGroup:P_rule;
          parts:T_bodyParts;
          closingBracket:P_token;
          i:longint;
      begin
        ruleDeclarationStart:=first^.location;
        evaluateBody:=(assignmentToken^.tokType=tt_assign);
        ruleBody:=assignmentToken^.next;
        assignmentToken^.next:=nil;
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
            ruleIsMutable   :=true;
            ruleIsPersistent:=(first^.tokType=tt_modifier_persistent);
            evaluateBody    :=true;
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
          if (first^.next<>nil) and (first^.next^.tokType=tt_braceClose) then begin
            setLength(parts,0);
            closingBracket:=first^.next;
          end else begin
            parts:=getBodyParts(first,0,context,closingBracket);
            for i:=0 to length(parts)-1 do begin
              if (parts[i].first^.tokType=tt_optionalParameters) and (parts[i].first^.next=nil) then begin
                if i<>length(parts)-1 then context.adapters^.raiseCustomMessage(mt_el4_parsingError,MSG_INVALID_OPTIONAL,parts[i].first^.location);
                //Optionals: f(...)->
                rulePattern.appendOptional;
                parts[i].first:=context.disposeToken(parts[i].first);
                assertNil(parts[i].first);
              end else if (parts[i].first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule]) then begin
                //Identified parameter: f(x)->
                rulePatternElement.create(parts[i].first^.txt);
                parts[i].first:=context.disposeToken(parts[i].first);
                if (parts[i].first<>nil) then begin
                  if (parts[i].first^.tokType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq, tt_operatorIn,
                                                  tt_typeCheckScalar, tt_typeCheckList, tt_typeCheckBoolean, tt_typeCheckBoolList, tt_typeCheckInt, tt_typeCheckIntList,
                                                  tt_typeCheckReal,tt_typeCheckRealList, tt_typeCheckString,tt_typeCheckStringList, tt_typeCheckNumeric, tt_typeCheckNumList,
                                                  tt_typeCheckExpression, tt_typeCheckKeyValueList]) then
                  begin
                    rulePatternElement.restrictionType:=parts[i].first^.tokType;
                    parts[i].first:=context.disposeToken(parts[i].first);
                    if rulePatternElement.restrictionType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq, tt_operatorIn]
                    then begin
                      //Identified, restricted parameter: f(x>?)->
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
                    end else if rulePatternElement.restrictionType in C_TYPE_RESTRICTIONS_WITH_ADDITIONAL_PARAMETER then begin
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
          end;
          rulePattern.finalizeRefs(context,@self);
          context.disposeToken(first);
          first:=context.disposeToken(closingBracket);
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
          if evaluateBody and (context.adapters^.noErrors) then reduceExpression(ruleBody,0,context);
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
          end else context.cascadeDisposeToken(first);
        end else context.cascadeDisposeToken(first);
      end;

    VAR statementHash:T_hashInt;
    begin
      if first=nil then exit;
      if not(context.adapters^.noErrors) then begin
        context.cascadeDisposeToken(first);
        exit;
      end;
      //conditional skipping for interactive mode
      if (usecase=lu_interactiveMode) then begin
        statementHash:=first^.hash;
        if (statementCounter<length(statementHashes)) and (statementHashes[statementCounter]=statementHash) and not(evaluateAll) then begin
          context.cascadeDisposeToken(first);
          inc(statementCounter);
          exit;
        end;
        evaluateAll:=true;
        if (statementCounter>=length(statementHashes)) then setLength(statementHashes,statementCounter+1);
        statementHashes[statementCounter]:=statementHash;
        {$ifdef fullVersion}
        context.adapters^.raiseCustomMessage(mt_evaluatedStatementInInteractiveMode,intToStr(statementCounter),first^.location);
        {$endif}
      end;
      inc(statementCounter);

      if statementCounter=1 then begin
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
        with profiler do if active then declarations:=timer.value.Elapsed-declarations;
        predigest(assignmentToken,@self,context);
        if context.adapters^.doEchoDeclaration then context.adapters^.raiseCustomMessage(mt_echo_declaration, tokensToString(first,maxLongint)+';',first^.location);
        parseRule;
        with profiler do if active then declarations:=timer.value.Elapsed-declarations;
      end else if context.adapters^.noErrors then begin
        if (usecase in [lu_forDirectExecution, lu_interactiveMode]) then begin
          with profiler do if active then interpretation:=timer.value.Elapsed-interpretation;
          predigest(first,@self,context);
          if context.adapters^.doEchoInput then context.adapters^.raiseCustomMessage(mt_echo_input, tokensToString(first,maxLongint)+';',first^.location);
          reduceExpression(first,0,context);
          with profiler do if active then interpretation:=timer.value.Elapsed-interpretation;
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
    begin
      if not(ready) or not(context.adapters^.noErrors) then begin
        context.adapters^.raiseCustomMessage(mt_el5_systemError,'Call of main has been rejected due to a previous error.',fileTokenLocation(codeProvider));
        exit;
      end;
      if not(packageRules.containsKey('main',mainRule)) then begin
        context.adapters^.raiseError('The specified package contains no main rule.',fileTokenLocation(codeProvider));
      end else begin
        t:=context.newToken(fileTokenLocation(environment.mainPackageProvider),'main',tt_localUserRule,mainRule);
        parametersForMain:=newListLiteral;
        parametersForMain^.rereference;
        for i:=0 to length(mainParameters)-1 do parametersForMain^.appendString(mainParameters[i]);
        t^.next:=context.newToken(fileTokenLocation(environment.mainPackageProvider),'',tt_parList,parametersForMain);
        with profiler do if active then interpretation:=timer.value.Elapsed-interpretation;
        reduceExpression(t,0,context);
        with profiler do if active then interpretation:=timer.value.Elapsed-interpretation;
        //error handling if main returns more than one token:------------------
        if (t=nil) or (t^.next<>nil) then begin
          {$ifdef fullVersion} if context.adapters^.hasNeedGUIerror
          then context.adapters^.raiseNote('Evaluation requires GUI-startup. Re-evaluating.',fileTokenLocation(codeProvider))
          else {$endif} context.adapters^.raiseError('Evaluation of main seems to be incomplete or erroneous.',fileTokenLocation(codeProvider));
        end;
        //------------------:error handling if main returns more than one token
        //special handling if main returns an expression:----------------------
        if (t<>nil) and (t^.tokType=tt_literal) and (t^.next=nil) and
           (P_literal(t^.data)^.literalType=lt_expression) then begin
          P_subrule(P_expressionLiteral(t^.data)^.value)^.directEvaluateNullary(nil,0,context);
        end;
        //----------------------:special handling if main returns an expression
        if context.adapters^.hasMessageOfType[mt_el3_noMatchingMain] then begin
          context.adapters^.printOut('');
          context.adapters^.printOut('Try one of the following:');
          context.adapters^.printOut('');
          printHelpOnMain(context.adapters^);
        end;
        context.cascadeDisposeToken(t);
        disposeLiteral(parametersForMain);
        parametersForMain:=nil;
      end;
    end;

  PROCEDURE prettyPrintTime;
    VAR importing_     ,
        tokenizing_    ,
        declarations_  ,
        interpretation_,
        unaccounted_   ,
        total_         ,
        timeUnit:string;
        totalTime:double;
        longest:longint=0;
        formatString:ansistring;

    FUNCTION fmt(CONST d:double):string; begin result:=formatFloat(formatString,d); if length(result)>longest then longest:=length(result); end;
    FUNCTION fmt(CONST s:string):string; begin result:=StringOfChar(' ',longest-length(s))+s+timeUnit; end;
    begin
      with profiler do begin
        totalTime:=importing+tokenizing+declarations+interpretation+unaccounted;
        if totalTime<1 then begin
          importing     :=importing     *1000;
          tokenizing    :=tokenizing    *1000;
          declarations  :=declarations  *1000;
          interpretation:=interpretation*1000;
          unaccounted   :=unaccounted   *1000;
          totalTime     :=totalTime     *1000;
          timeUnit:='ms';
          formatString:='0.000';
        end else begin
          timeUnit:='s';
          formatString:='0.000000';
        end;
        importing_     :=fmt(importing     );
        tokenizing_    :=fmt(tokenizing    );
        declarations_  :=fmt(declarations  );
        interpretation_:=fmt(interpretation);
        unaccounted_   :=fmt(unaccounted   );
        total_         :=fmt(totalTime     );
        if importing     >0 then context.adapters^.raiseCustomMessage(mt_timing_info,'Importing time      '+fmt(importing_     ),C_nilTokenLocation);
        if tokenizing    >0 then context.adapters^.raiseCustomMessage(mt_timing_info,'Tokenizing time     '+fmt(tokenizing_    ),C_nilTokenLocation);
        if declarations  >0 then context.adapters^.raiseCustomMessage(mt_timing_info,'Declaration time    '+fmt(declarations_  ),C_nilTokenLocation);
        if interpretation>0 then context.adapters^.raiseCustomMessage(mt_timing_info,'Interpretation time '+fmt(interpretation_),C_nilTokenLocation);
        if unaccounted   >0 then context.adapters^.raiseCustomMessage(mt_timing_info,'Unaccounted for     '+fmt(unaccounted_   ),C_nilTokenLocation);
                                 context.adapters^.raiseCustomMessage(mt_timing_info,StringOfChar('-',20+length(fmt(total_))),C_nilTokenLocation);
                                 context.adapters^.raiseCustomMessage(mt_timing_info,'                    '+fmt(total_         ),C_nilTokenLocation);
      end;
    end;

  {$define stepToken:=
    with profiler do if active then tokenizing:=timer.value.Elapsed-tokenizing;
    fileTokens.step(@self,lastComment,context.adapters^);
    with profiler do if active then tokenizing:=timer.value.Elapsed-tokenizing}

  VAR localIdStack:T_idStack;
      first,last:P_token;
  begin
    if context.adapters^.doShowTimingInfo and (usecase in [lu_forDirectExecution,lu_forCallingMain,lu_interactiveMode]) then begin
      profiler.active:=usecase<>lu_interactiveMode;
      timer.value.clear;
      timer.value.start;
      profiler.unaccounted:=timer.value.Elapsed;
    end else profiler.active:=false;

    if usecase<>lu_interactiveMode
    then clear
    else reloadAllPackages(fileTokenLocation(codeProvider));

    if ((usecase=lu_forCallingMain) or (codeProvider<>environment.mainPackageProvider)) and codeProvider^.fileHasChanged then codeProvider^.load;
    with profiler do if active then tokenizing:=timer.value.Elapsed;
    fileTokens.create;
    fileTokens.tokenizeAll(codeProvider,@self,context.adapters^);
    //First step
    fileTokens.step(@self,lastComment,context.adapters^);
    with profiler do if active then tokenizing:=timer.value.Elapsed-tokenizing;
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
        stepToken;

        while (not(fileTokens.atEnd)) and not((fileTokens.current.tokType=tt_end) and (localIdStack.oneAboveBottom)) do begin
          case fileTokens.current.tokType of
            tt_begin: localIdStack.scopePush;
            tt_end  : localIdStack.scopePop;
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
        if (first<>nil) and (first^.areBracketsPlausible(context.adapters^))
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
    fileTokens.destroy;
    localIdStack.destroy;
    if (context.adapters^.noErrors)
    then begin if first<>nil then interpret(first,C_nilTokenLocation); end
    else context.cascadeDisposeToken(first);
    ready:=usecase<>lu_forDocGeneration;

    if usecase=lu_forCallingMain then executeMain;
    if (usecase in [lu_forDirectExecution,lu_forCallingMain]) and context.adapters^.noErrors
    then complainAboutUncalled(true,context.adapters^);
    if (usecase in [lu_forDirectExecution,lu_forCallingMain])
    then begin
      finalize(context.adapters^);
      clearCachedFormats;
      killServersCallback;
    end;

    with profiler do if active then begin
      unaccounted:=timer.value.Elapsed-unaccounted-importing-tokenizing-declarations-interpretation;
      prettyPrintTime;
    end;
  end;

PROCEDURE disposeRule(VAR rule:P_rule);
  begin
    dispose(rule,destroy);
  end;

CONSTRUCTOR T_package.create(CONST provider: P_codeProvider);
  begin
    setLength(packageUses,0);
    codeProvider:=provider;
    packageRules.create(@disposeRule);
    importedRules.create;
    setLength(statementHashes,0);
  end;

FUNCTION T_package.needReload: boolean;
  begin
    result:=codeProvider^.fileHasChanged;
  end;

PROCEDURE T_package.clear;
  begin
    packageRules.clear;
    importedRules.clear;
    setLength(packageUses,0);
    setLength(statementHashes,0);
    ready:=false;
  end;

PROCEDURE T_package.finalize(VAR adapters:T_adapters);
  VAR ruleList:array of P_rule;
      wroteBack:boolean=false;
      i:longint;
  begin
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do begin
      if ruleList[i]^.writeBack(codeProvider^,adapters) then wroteBack:=true;
      if ruleList[i]^.ruleType=rt_memoized then ruleList[i]^.cache^.clear;
    end;
    setLength(ruleList,0);
    if wroteBack then begin
      codeProvider^.save;
      adapters.raiseCustomMessage(mt_reloadRequired,codeProvider^.getPath,C_nilTokenLocation);
    end;
    for i:=0 to length(packageUses)-1 do packageUses[i].pack^.finalize(adapters);
  end;

DESTRUCTOR T_package.destroy;
  begin
    clear;
    if codeProvider<>environment.mainPackageProvider then dispose(codeProvider,destroy);
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

FUNCTION T_package.ensureRuleId(CONST ruleId: ansistring; CONST ruleIsPrivate,ruleIsMemoized,ruleIsMutable,ruleIsPersistent,ruleIsSynchronized:boolean; CONST ruleDeclarationStart,ruleDeclarationEnd:T_tokenLocation; VAR adapters:T_adapters): P_rule;
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
      adapters.raiseCustomMessage(mt_el1_note,'Creating new rule: '+ruleId,ruleDeclarationStart);
      if intrinsicRuleMap.containsKey(ruleId) then adapters.raiseWarning('Hiding builtin rule "'+ruleId+'"!',ruleDeclarationStart);
    end else begin
      if (result^.ruleType<>ruleType) and (ruleType<>rt_normal)
      then adapters.raiseCustomMessage(mt_el4_parsingError,'Colliding modifiers! Rule '+ruleId+' is '+C_ruleTypeText[result^.ruleType]+', redeclared as '+C_ruleTypeText[ruleType],ruleDeclarationStart)
      else adapters.raiseCustomMessage(mt_el1_note,'Extending rule: '+ruleId,ruleDeclarationStart);
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

{$ifdef fullVersion}
FUNCTION T_package.getDoc:P_userPackageDocumentation;
  VAR ruleList:array of P_rule;
      i:longint;
  begin
    new(result,create(codeProvider^.getPath,codeProvider^.id,codeProvider^.getLines));
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do begin
      result^.addRuleDoc(ruleList[i]^.getDocHtml);
      if ruleList[i]^.id='main' then result^.isExecutable:=true;
    end;
    setLength(ruleList,0);
    for i:=0 to length(packageUses)-1 do result^.addUses(expandFileName(packageUses[i].path));
  end;
{$endif}

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

FUNCTION T_package.getPackageReferenceForId(CONST id:string; CONST adapters:P_adapters):T_packageReference;
  VAR i:longint;
  begin
    for i:=0 to length(packageUses)-1 do if packageUses[i].id=id then exit(packageUses[i]);
    result.create(codeProvider^.getPath,'',C_nilTokenLocation,adapters);
  end;

FUNCTION T_package.isReady:boolean; begin result:=ready; end;

PROCEDURE T_package.reportVariables(VAR variableReport:T_variableReport);
  VAR firstInBlock:boolean=true;
      i:longint;
      r:T_ruleMap.VALUE_TYPE_ARRAY;
      value:P_literal;
  begin
    r:=importedRules.valueSet;
    for i:=0 to length(r)-1 do begin
      if r[i]^.isReportable(value) then begin
        if firstInBlock then variableReport.addCommentLine('imported:');
        firstInBlock:=false;
        variableReport.addVariable(r[i]^.id, value);
      end;
    end;
    setLength(r,0);
    r:=packageRules.valueSet;
    for i:=0 to length(r)-1 do begin
      if r[i]^.isReportable(value) then begin
        if firstInBlock then variableReport.addCommentLine('in main package:');
        firstInBlock:=false;
        variableReport.addVariable(r[i]^.id, value);
      end;
    end;
    setLength(r,0);
  end;


PROCEDURE callMainInMain(CONST parameters:T_arrayOfString; VAR context:T_evaluationContext);
  begin
    context.adapters^.clearErrors;
    environment.mainPackage^.load(lu_forCallingMain,context,parameters);
    context.adapters^.raiseCustomMessage(mt_endOfEvaluation,'',C_nilTokenLocation);
  end;

PROCEDURE printMainPackageDocText(VAR adapters:T_adapters);
  VAR silentContext:T_evaluationContext;
  begin
    silentContext.createSanboxContext(P_adapters(@nullAdapter));
    nullAdapter.clearErrors;
    reloadMainPackage(lu_forDocGeneration,silentContext);
    environment.mainPackage^.printHelpOnMain(adapters);
    silentContext.destroy;
  end;

{$ifdef fullVersion}
PROCEDURE findAndDocumentAllPackages;
  VAR sourceNames:T_arrayOfString;
      i:longint;
      p:T_package;
      provider:P_codeProvider;
      context:T_evaluationContext;
  begin
    ensureDemos;
    context.createSanboxContext(P_adapters(@nullAdapter));
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
{$endif}

FUNCTION initTimer:TEpikTimer;
  begin
    result:=TEpikTimer.create(nil);
    result.clear;
    result.start;
  end;

PROCEDURE disposeTimer(t:TEpikTimer);
  begin
    t.destroy;
  end;

{$undef include_implementation}
VAR i:longint;
INITIALIZATION
  timer.create(@initTimer,@disposeTimer);
{$define include_initialization}
{$include mnh_tokens_fmtStmt.inc}
  with environment do begin
    new(mainPackageProvider,create);
    new(mainPackage,create(mainPackageProvider));
    setLength(secondaryPackages,0);
  end;
  pendingTasks.create;

  //callbacks in mnh_litvar:
  disposeSubruleCallback :=@disposeSubruleImpl;
  subruleToStringCallback:=@subruleToStringImpl;
  subruleToArityCallback:=@subruleToArityImpl;
  subruleApplyOpCallback :=@subruleApplyOpImpl;
  evaluateCompatorCallback:=@evaluateComparator;
  evaluateSubruleCallback:=@evaluateSubrule;
  //callbacks in doc
  {$ifdef fullVersion}
  demoCodeToHtmlCallback:=@demoCallToHtml;
  {$endif}
  //callbacks in html
  rawTokenizeCallback:=@tokenizeAllReturningRawTokens;
  {$include mnh_tokens_funcs.inc}
{$undef include_initialization}

FINALIZATION
{$define include_finalization}
  pendingTasks.destroy;
  with environment do begin
    dispose(mainPackage,destroy);
    dispose(mainPackageProvider,destroy);
    for i:=length(secondaryPackages)-1 downto 0 do dispose(secondaryPackages[i],destroy);
    setLength(secondaryPackages,0);
  end;
  timer.destroy;
{$include mnh_tokens_fmtStmt.inc}
end.
