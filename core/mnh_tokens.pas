UNIT mnh_tokens;
INTERFACE
USES myGenerics, mnh_constants, math, sysutils, myStringutil,typinfo,  //utilities
     mnh_litVar, mnh_fileWrappers, mnh_tokLoc, //types
     EpikTimer,
     mnh_funcs, mnh_out_adapters, mnh_caches, mnh_doc, mnh_regex; //even more specific

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
    CONSTRUCTOR create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation);
    DESTRUCTOR destroy;
  end;

  { T_package }
  T_package=object
    private
      packageRules,importedRules:T_ruleMap;
      packageUses:array of T_packageReference;
      ready:boolean;
      codeProvider:P_codeProvider;
      loadedVersion:longint;
      hadBatchModeParts:boolean;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider);
      FUNCTION needReload:boolean;
      PROCEDURE load(CONST usecase:T_packageLoadUsecase; VAR recycler:T_tokenRecycler);
      PROCEDURE clear;
      DESTRUCTOR destroy;
      PROCEDURE resolveRuleId(VAR token:T_token; CONST failSilently:boolean);
      FUNCTION ensureRuleId(CONST ruleId:ansistring; CONST ruleIsMemoized,ruleIsMutable,ruleIsSynchronized:boolean; CONST ruleDeclarationStart:T_tokenLocation):P_rule;
      PROCEDURE updateLists(VAR userDefinedRules:T_listOfString);
      PROCEDURE complainAboutUncalled;
      FUNCTION getDoc:P_userPackageDocumentation;
      PROCEDURE printHelpOnMain;
  end;

FUNCTION parse_evaluate_return(CONST command:ansistring):ansistring;

PROCEDURE reloadMainPackage(CONST usecase:T_packageLoadUsecase);
PROCEDURE callMainInMain(CONST parameters:T_arrayOfString);
PROCEDURE printMainPackageDocText;
FUNCTION getMainPackage:P_package;
FUNCTION getTokenAt(CONST line:ansistring; CONST charIndex:longint):T_token;
PROCEDURE findAndDocumentAllPackages;
PROCEDURE reduceExpression(VAR first:P_token; CONST callDepth:word; VAR recycler:T_tokenRecycler);

VAR mainPackageProvider:T_codeProvider;

{$undef include_interface}
IMPLEMENTATION
CONST STACK_DEPTH_LIMIT=60000;
VAR secondaryPackages:array of P_package;
    mainPackage      :T_package;
    parametersForMain:P_listLiteral=nil;
    packagesAreFinalized:boolean=false;
    pendingTasks:T_taskQueue;

FUNCTION guessPackageForToken(CONST token:T_token):P_package;
  VAR providerPath:ansistring;
      packId:string;
      i:longint;
  begin
    providerPath:=token.location.fileName;
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

{$define include_implementation}
{$include mnh_tokens_token.inc}
{$include mnh_tokens_recycler.inc}
{$include mnh_tokens_pattern.inc}
{$include mnh_tokens_subrule.inc}
{$include mnh_tokens_futureTask.inc}
{$include mnh_tokens_procBlock.inc}
{$include mnh_tokens_rule.inc}
{$include mnh_tokens_funcs.inc}

PROCEDURE reloadMainPackage(CONST usecase:T_packageLoadUsecase);
  VAR i,j:longint;
      used:T_listOfString;
      recycler:T_tokenRecycler;
  begin
    clearAllCaches;
    clearErrors;
    recycler.create;
    mainPackage.load(usecase,recycler);
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
      end else begin
        dispose(secondaryPackages[j],destroy);
      end;
    end;
    setLength(secondaryPackages,j);
    used.destroy;
    //-------------------------------------------------------------:housekeeping
    recycler.destroy;
  end;

PROCEDURE finalizePackages;
  VAR i:longint;
  begin
    if packagesAreFinalized then exit;
    mainPackage.destroy;
    mainPackageProvider.destroy;
    clearAllCaches;
    clearErrors;
    for i:=length(secondaryPackages)-1 downto 0 do dispose(secondaryPackages[i],destroy);
    setLength(secondaryPackages,0);
    packagesAreFinalized:=true;
  end;

PROCEDURE loadPackage(VAR pack:T_packageReference; CONST tokenLocation:T_tokenLocation; VAR recycler:T_tokenRecycler);
  VAR i:longint;
      newSource:P_codeProvider=nil;
  begin
    for i:=0 to length(secondaryPackages)-1 do
      if secondaryPackages[i]^.codeProvider^.id = pack.id then begin
        if secondaryPackages[i]^.ready then begin
          if secondaryPackages[i]^.needReload then begin
            secondaryPackages[i]^.clear;
            secondaryPackages[i]^.load(lu_forImport,recycler);
          end;
          pack.pack:=secondaryPackages[i];
          exit;
        end else begin
          raiseError(el4_parsingError,'Cyclic package dependencies encountered; already loading "'+pack.id+'"',tokenLocation);
          exit;
        end;
      end;
    new(newSource,create(pack.path));
    new(pack.pack,create(newSource));
    setLength(secondaryPackages,length(secondaryPackages)+1);
    secondaryPackages[length(secondaryPackages)-1]:=pack.pack;
    pack.pack^.load(lu_forImport,recycler);
  end;

CONSTRUCTOR T_packageReference.create(CONST root,packId:ansistring; CONST tokenLocation:T_tokenLocation);
  begin
    id:=packId;
    path:=locateSource(root,id);
    if path='' then raiseError(el4_parsingError,'Cannot locate package for id "'+id+'"',tokenLocation);
    pack:=nil;
  end;

DESTRUCTOR T_packageReference.destroy;
  begin
    id:='';
    path:='';
    pack:=nil;
  end;


PROCEDURE T_package.load(CONST usecase:T_packageLoadUsecase; VAR recycler:T_tokenRecycler);
  VAR isFirstLine:boolean=true;
      batchMode:boolean=false;
      lastComment:ansistring;

  PROCEDURE interpret(VAR first:P_token);
    PROCEDURE interpretUseClause;
      VAR temp:P_token;
          i,j:longint;
          locationForErrorFeedback:T_tokenLocation;
          newId:string;
          rulesSet:T_ruleMap.KEY_VALUE_LIST;
          dummyRule:P_rule;
      begin
        locationForErrorFeedback:=first^.location;
        temp:=first; first:=recycler.disposeToken(temp);
        while first<>nil do begin
          if first^.tokType=tt_identifier then begin
            newId:=first^.txt;
            if isQualified(newId) then begin
              raiseError(el4_parsingError,'Cannot interpret use clause containing qualified identifier '+first^.singleTokenToString,first^.location);
              exit;
            end;
            //no duplicates are created; packages are always added at the end
            i:=0;
            while (i<length(packageUses)) and (packageUses[i].id<>newId) do inc(i);
            if i<length(packageUses) then for j:=i to length(packageUses)-2 do packageUses[j]:=packageUses[j+1]
                                     else setLength(packageUses,length(packageUses)+1);
            packageUses[length(packageUses)-1].create(codeProvider^.getPath,first^.txt,first^.location);
          end else if first^.tokType<>tt_separatorComma then begin
            raiseError(el4_parsingError,'Cannot interpret use clause containing '+first^.singleTokenToString,first^.location);
            exit;
          end;
          temp:=first; first:=recycler.disposeToken(temp);
        end;
        if usecase<>lu_forDocGeneration then begin
          for i:=0 to length(packageUses)-1 do loadPackage(packageUses[i],locationForErrorFeedback,recycler);
          i:=0;
          while i<length(packageUses) do begin
            if packageUses[i].pack=nil then begin
              for j:=i to length(packageUses)-2 do packageUses[j]:=packageUses[j+1];
              setLength(packageUses,length(packageUses)-1);
            end else inc(i);
          end;
          if errorLevel<el3_evalError then for i:=length(packageUses)-1 downto 0 do begin
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
          raiseError(el4_parsingError,'Missing function body after assignment/declaration token.',assignmentToken^.location);
          recycler.cascadeDisposeToken(first);
          exit;
        end;
        while (first<>nil) and (first^.tokType in [tt_modifier_private,tt_modifier_memoized,tt_modifier_mutable,tt_modifier_synchronized]) do begin
          if first^.tokType=tt_modifier_private      then ruleIsPrivate :=true;
          if first^.tokType=tt_modifier_memoized     then ruleIsMemoized:=true;
          if first^.tokType=tt_modifier_synchronized then ruleIsSynchronized:=true;
          if first^.tokType=tt_modifier_mutable  then begin
            ruleIsMutable :=true;
            evaluateBody:=true;
          end;
          first:=recycler.disposeToken(first);
        end;
        if not(first^.tokType in [tt_identifier, tt_localUserRulePointer, tt_importedUserRulePointer, tt_intrinsicRulePointer]) then begin
          raiseError(el4_parsingError,'Declaration does not start with an identifier.',first^.location);
          recycler.cascadeDisposeToken(first);
          exit;
        end;
        p:=first;
        while (p<>nil) and not(p^.tokType in [tt_assign,tt_declare]) do begin
          if (p^.tokType in [tt_identifier, tt_localUserRulePointer, tt_importedUserRulePointer, tt_intrinsicRulePointer]) and isQualified(first^.txt) then begin
            raiseError(el4_parsingError,'Declaration head contains qualified ID.',p^.location);
            recycler.cascadeDisposeToken(first);
            exit;
          end;
          p:=p^.next;
        end;
        //:plausis

        ruleId:=trim(first^.txt);
        first:=recycler.disposeToken(first);
        if not(first^.tokType in [tt_braceOpen,tt_assign,tt_declare])  then begin
          raiseError(el4_parsingError,'Invalid declaration head.',first^.location);
          recycler.cascadeDisposeToken(first);
          exit;
        end;
        rulePattern.create;
        if first^.tokType=tt_braceOpen then begin
          first:=recycler.disposeToken(first);
          while not((first=nil) or (first^.tokType in [tt_assign,tt_declare])) do begin
            if rulePattern.hasOptionals then raiseError(el4_parsingError,'Optional parameters are allowed only as last entry in a function head declaration.',ruleDeclarationStart);
            n  :=first^.next;
            nn :=n    ^.next;
            nnn:=nn   ^.next;
            if (first^.tokType=tt_identifier)
            and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendFreeId(first^.txt);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else if (first^.tokType=tt_optionalParameters)
                     and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              if n^.tokType=tt_separatorComma then raiseError(el4_parsingError,'Optional parameters are allowed only as last entry in a function head declaration.',ruleDeclarationStart);
              rulePattern.appendOptional;
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType in [lt_boolean, lt_int, lt_real, lt_string])
                     and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison('',tt_comparatorEq,P_scalarLiteral(first^.data));
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_emptyList) then begin
              rulePattern.appendTypeCheck('',tt_typeCheckEmptyList);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_typeCheckScalar, tt_typeCheckList, tt_typeCheckBoolean, tt_typeCheckBoolList, tt_typeCheckInt, tt_typeCheckIntList, tt_typeCheckReal,tt_typeCheckRealList, tt_typeCheckString,tt_typeCheckStringList, tt_typeCheckNumeric, tt_typeCheckNumList, tt_typeCheckExpression, tt_typeCheckNonemptyList, tt_typeCheckEmptyList, tt_typeCheckKeyValueList])
                     and (nn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendTypeCheck(first^.txt,n^.tokType);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
                     and (nn^.tokType=tt_literal) and (P_literal(nn^.data)^.literalType in [lt_boolean, lt_int, lt_real, lt_string])
                     and (nnn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison(first^.txt,n^.tokType,P_scalarLiteral(nn^.data));
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
                     and (nn^.tokType=tt_identifier)
                     and (nnn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison(first^.txt,n^.tokType,nn^.txt);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else begin
              raiseError(el4_parsingError,'Invalid declaration pattern element.',first^.location);
              recycler.cascadeDisposeToken(first);
              exit;
            end;
          end;
        end;
        if first<>nil then begin
          first:=recycler.disposeToken(first);
        end else begin
          raiseError(el4_parsingError,'Invalid declaration.',ruleDeclarationStart);
          exit;
        end;

        if (usecase<>lu_forDocGeneration) or ruleIsMutable then begin
          rulePattern.toParameterIds(ruleBody);
          if evaluateBody then reduceExpression(ruleBody,0,recycler);
        end;

        if errorLevel<el3_evalError then begin
          ruleGroup:=ensureRuleId(ruleId,ruleIsMemoized,ruleIsMutable,ruleIsSynchronized,ruleDeclarationStart);
          if errorLevel<el3_evalError then begin
            new(subRule,create(rulePattern,ruleBody,ruleDeclarationStart,ruleIsPrivate,recycler));
            subRule^.comment:=lastComment; lastComment:='';
            if ruleGroup^.ruleType=rt_mutable
            then begin
              ruleGroup^.setMutableValue(subRule^.getInlineValue);
              dispose(subRule,destroy);
            end else ruleGroup^.addOrReplaceSubRule(subRule);
            first:=nil;
          end else if errorLevel<el5_systemError then
            recycler.cascadeDisposeToken(first)
          else
            first:=nil;
        end else if errorLevel<el5_systemError then
          recycler.cascadeDisposeToken(first)
        else
          first:=nil;
      end;

    begin
      if first=nil then exit;
      if isFirstLine then begin
        isFirstLine:=false;
        if (first^.tokType=tt_identifier) and
           (first^.txt    ='USE') and
           (first^.next   <>nil) and
           (first^.next^.tokType=tt_identifier)
        then begin
          interpretUseClause;
          exit;
        end;
      end;
      predigestBeforeDeclarationParsing(first,recycler);
      assignmentToken:=first^.getDeclarationOrAssignmentToken;
      if assignmentToken<>nil then begin
        predigest(assignmentToken,@self,recycler);
        outAdapter^.writeEcho(eld_echoDeclaration, tokensToString(first));
        parseRule;
      end else begin
        if (usecase=lu_forDirectExecution) or (batchMode) then begin
          predigest(first,@self,recycler);
          outAdapter^.writeEcho(ele_echoInput, tokensToString(first));
          reduceExpression(first,0,recycler);
          if first<>nil then outAdapter^.writeEcho(elo_echoOutput, tokensToString(first));
        end else raiseError(el1_note,'Skipping expression '+tokensToString(first),first^.location);
      end;
      if first<>nil then recycler.cascadeDisposeToken(first);
      first:=nil;
    end;

  VAR currentTokenIndex:longint;
      fileTokens:T_tokenArray;
      {$define currentToken:=fileTokens[currentTokenIndex]}

  PROCEDURE stepToken;
    begin
      repeat
        inc(currentTokenIndex);
        if (currentTokenIndex<length(fileTokens)) and
           (currentToken.tokType=tt_EOL) then begin
          if      currentToken.txt=SPECIAL_COMMENT_BATCH_STYLE_ON  then batchMode:=(usecase<>lu_forDocGeneration)
          else if currentToken.txt=SPECIAL_COMMENT_BATCH_STYLE_OFF then batchMode:=false
          else if (currentToken.txt<>'') then lastComment:=currentToken.txt;
          if batchMode then hadBatchModeParts:=true;
        end;
      until (currentTokenIndex>=length(fileTokens)) or
            (currentToken.tokType<>tt_EOL);
    end;

  VAR localIdStack:T_idStack;
      first,last:P_token;
  begin
    clear;
    loadedVersion:=codeProvider^.getVersion((usecase=lu_forCallingMain) or (codeProvider<>@mainPackageProvider));
    fileTokens:=tokenizeAll(codeProvider,@self);
    currentTokenIndex:=-1;
    stepToken;
    first:=nil;
    last :=nil;
    localIdStack.create;
    while currentTokenIndex<length(fileTokens) do begin
      if currentToken.tokType=tt_procedureBlockBegin then begin
        if first=nil then begin
          first:=recycler.newToken(currentToken); currentToken.undefine;
          last :=first;
        end else begin
          last^.next:=recycler.newToken(currentToken); currentToken.undefine;
          last      :=last^.next;
        end;

        localIdStack.clear;
        localIdStack.scopePush;
        stepToken;
        while (currentTokenIndex<length(fileTokens)) and not((currentToken.tokType=tt_procedureBlockEnd) and (localIdStack.oneAboveBottom)) do begin
          case currentToken.tokType of
            tt_procedureBlockBegin: localIdStack.scopePush;
            tt_procedureBlockEnd  : localIdStack.scopePop;
            tt_identifier: if (last^.tokType=tt_modifier_local) then begin
              currentToken.tokType:=tt_blockLocalVariable;
              localIdStack.addId(currentToken.txt);
            end else if (localIdStack.hasId(currentToken.txt)) then
              currentToken.tokType:=tt_blockLocalVariable;
          end;
          last^.next:=recycler.newToken(currentToken); currentToken.undefine;
          last      :=last^.next;
          stepToken;
        end;

      end else if (currentToken.tokType=tt_semicolon) then begin
        if first<>nil then interpret(first);
        last:=nil;
        first:=nil;
        stepToken;
      end else begin
        if first=nil then begin
          first:=recycler.newToken(currentToken); currentToken.undefine;
          last :=first
        end else begin
          last^.next:=recycler.newToken(currentToken); currentToken.undefine;
          last      :=last^.next;
        end;
        last^.next:=nil;
        stepToken;
      end;
    end;
    localIdStack.destroy;
    if (errorLevel<el3_evalError)
    then begin if first<>nil then interpret(first); end
    else recycler.cascadeDisposeToken(first);
    ready:=true;
    if length(fileTokens)>0
    then raiseError(el0_allOkay,'Package '+codeProvider^.id+' ready.',fileTokens[length(fileTokens)-1].location)
    else raiseError(el0_allOkay,'Package '+codeProvider^.id+' ready.',C_nilTokenLocation);
    clearErrors;
    if usecase=lu_forDirectExecution then complainAboutUncalled;
  end;

CONSTRUCTOR T_package.create(CONST provider: P_codeProvider);
  begin
    setLength(packageUses,0);
    codeProvider:=provider;
    packageRules.create;
    importedRules.create;
    loadedVersion:=-1;
  end;

FUNCTION T_package.needReload: boolean;
  begin
    result:=loadedVersion<>codeProvider^.getVersion(true);
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
    hadBatchModeParts:=false;
    setLength(packageUses,0);
    ready:=false;
  end;

DESTRUCTOR T_package.destroy;
  begin
    clear;
    if codeProvider<>@mainPackageProvider then dispose(codeProvider,destroy);
    packageRules.destroy;
    importedRules.destroy;
    setLength(packageUses,0);
  end;

PROCEDURE T_package.resolveRuleId(VAR token: T_token; CONST failSilently: boolean);
  VAR userRule:P_rule;
      intrinsicFuncPtr:T_intFuncCallback;
      ruleId:ansistring;
  begin
    ruleId   :=token.txt;
    if packageRules.containsKey(ruleId,userRule) then begin
      token.tokType:=tt_localUserRulePointer;
      token.data:=userRule;
      exit;
    end;
    if importedRules.containsKey(ruleId,userRule) then begin
      token.tokType:=tt_importedUserRulePointer;
      token.data:=userRule;
      exit;
    end;
    if intrinsicRuleMap.containsKey(ruleId,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRulePointer;
      token.data:=intrinsicFuncPtr;
      exit;
    end;
    if not(failSilently) then raiseError(el4_parsingError,'Cannot resolve ID "'+token.txt+'"',token.location);
  end;

FUNCTION T_package.ensureRuleId(CONST ruleId: ansistring; CONST ruleIsMemoized,ruleIsMutable,ruleIsSynchronized:boolean; CONST ruleDeclarationStart:T_tokenLocation): P_rule;
  CONST ruleTypeTxt:array[T_ruleType] of string=('normal','memoized','mutable','synchronized');

  VAR ruleType:T_ruleType=rt_normal;
  begin
    if ruleIsSynchronized then ruleType:=rt_synchronized;
    if ruleIsMemoized then ruleType:=rt_memoized else
    if ruleIsMutable then ruleType:=rt_mutable;

    if not(packageRules.containsKey(ruleId,result)) then begin
      new(result,create(ruleId,ruleType));
      packageRules.put(ruleId,result);
      raiseError(el0_allOkay,'New rule '+ruleId,ruleDeclarationStart);
    end else if (result^.ruleType<>ruleType) and (ruleType<>rt_normal) then begin
      raiseError(el4_parsingError,'Colliding modifiers! Rule '+ruleId+' is '+ruleTypeTxt[result^.ruleType]+', redeclared as '+ruleTypeTxt[ruleType],ruleDeclarationStart);
    end;
  end;

PROCEDURE T_package.updateLists(VAR userDefinedRules: T_listOfString);
  VAR i,j:longint;
      ids:T_arrayOfString;
      packageId:ansistring;
  begin
    userDefinedRules.clear;
    userDefinedRules.addArr(packageRules.keySet);
    for i:=0 to length(packageUses)-1 do if (packageUses[i].pack<>nil) and packageUses[i].pack^.ready then begin
      packageId:=packageUses[i].id;
      ids:=packageUses[i].pack^.packageRules.keySet;
      for j:=0 to length(ids)-1 do begin
        userDefinedRules.add(ids[j]);
        userDefinedRules.add(packageId+C_ID_QUALIFY_CHARACTER+ids[j]);
      end;
    end;
    userDefinedRules.unique;
  end;

PROCEDURE T_package.complainAboutUncalled;
  VAR ruleList:array of P_rule;
      i:longint;
  begin
    ruleList:=packageRules.valueSet;
    for i:=0 to length(ruleList)-1 do ruleList[i]^.complainAboutUncalled;
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

PROCEDURE T_package.printHelpOnMain;
  VAR mainRule:P_rule;
      docText:T_arrayOfString;
      i:longint;
  begin
    if not(packageRules.containsKey('main',mainRule))
    then writeln('The package contains no main rule')
    else begin
      docText:=split(mainRule^.getDocTxt,C_lineBreakChar);
      for i:=0 to 1 do writeln(docText[i]);
      dropFirst(docText,2);
      docText:=formatTabs(docText);
      for i:=0 to length(docText)-1 do writeln(docText[i]);
    end;
  end;

PROCEDURE callMainInMain(CONST parameters:T_arrayOfString);
  VAR t:P_token;
      i:longint;
      mainRule:P_rule;
      recycler:T_tokenRecycler;
  begin
    clearErrors;
    recycler.create;

    parametersForMain:=newListLiteral;
    parametersForMain^.rereference;
    for i:=0 to length(parameters)-1 do parametersForMain^.appendString(parameters[i]);

    mainPackage.load(lu_forCallingMain,recycler);
    if not(mainPackage.ready) or (errorLevel>=el3_evalError) then begin
      raiseError(el5_systemError,'Call of main has been rejected due to a previous error.',fileTokenLocation(@mainPackageProvider));
      recycler.destroy;
      exit;
    end;

    t:=recycler.newToken(fileTokenLocation(@mainPackageProvider),'main',tt_identifier);

    if not(mainPackage.packageRules.containsKey('main',mainRule)) then begin
      if mainPackage.hadBatchModeParts
      then raiseError(el1_note     ,'The specified package contains no main rule.',fileTokenLocation(@mainPackageProvider))
      else raiseError(el3_evalError,'The specified package contains no main rule.',fileTokenLocation(@mainPackageProvider));
    end else begin
      t^.tokType:=tt_localUserRulePointer;
      t^.data:=mainRule;
      t^.next:=recycler.newToken(fileTokenLocation(@mainPackageProvider),'',tt_parList,parametersForMain);
      reduceExpression(t,0,recycler);
      //special handling if main returns an expression:
      if (t<>nil) and (t^.tokType=tt_literal) and (t^.next=nil) and
         (P_literal(t^.data)^.literalType=lt_expression) then begin
        P_subrule(P_expressionLiteral(t^.data)^.value)^.directEvaluateNullary(nil,0,recycler);
      end;
      //:special handling if main returns an expression
      mainPackage.complainAboutUncalled;
    end;
    recycler.cascadeDisposeToken(t);
    recycler.destroy;
    disposeLiteral(parametersForMain);
    parametersForMain:=nil;
  end;

PROCEDURE printMainPackageDocText;
  begin
    mainPackageProvider.load;
    reloadMainPackage(lu_forDocGeneration);
    mainPackage.printHelpOnMain;
  end;

FUNCTION getMainPackage: P_package;
  begin
    result:=@mainPackage;
  end;

FUNCTION getTokenAt(CONST line: ansistring; CONST charIndex: longint): T_token;
  VAR copyOfLine:ansistring;
      lineLocation:T_tokenLocation;
  begin
    try
      copyOfLine:=line;
      lineLocation.fileName:=mainPackage.codeProvider^.getPath;
      lineLocation.line:=0;
      lineLocation.column:=1;
      if (length(copyOfLine)>1) and (copyOfLine[1]=#10) then begin
        copyOfLine:=copy(copyOfLine,6,length(copyOfLine)-5);
        inc(lineLocation.column,5);
      end;
      result:=firstToken(copyOfLine,lineLocation,@mainPackage,false);
      while (length(copyOfLine)>0) and (lineLocation.column<charIndex) do
        result:=firstToken(copyOfLine,lineLocation,@mainPackage,false);
    except
      result.create;
      result.define(C_nilTokenLocation,'ERR',tt_EOL);
    end;
  end;

PROCEDURE findAndDocumentAllPackages;
  VAR sourceNames:T_arrayOfString;
      i:longint;
      p:T_package;
      provider:P_codeProvider;
      recycler:T_tokenRecycler;
  begin
    recycler.create;
    sourceNames:=locateSources;
    for i:=0 to length(sourceNames)-1 do begin
      new(provider,create(sourceNames[i]));
      p.create(provider);
      p.load(lu_forDocGeneration,recycler);
      addPackageDoc(p.getDoc);
      p.destroy;
    end;
    makeHtmlFromTemplate;
    recycler.destroy;
  end;

FUNCTION parse_evaluate_return(CONST command:ansistring):ansistring;
  VAR previousAdapter:P_abstractOutAdapter;
      tempAdapter:T_collectingOutAdapter;
      i,j:longint;

  begin
    //set-up:-----------------------------------------------------
    previousAdapter:=outAdapter;
    tempAdapter.create;
    with tempAdapter.outputBehaviour do begin
      doEchoDeclaration:=false;
      doEchoInput:=false;
      doShowExpressionOut:=true;
    end;
    outAdapter:=@tempAdapter;
    //-----------------------------------------------------:set-up
    mainPackageProvider.clear;
    mainPackageProvider.appendLine(command);
    reloadMainPackage(lu_forDirectExecution);
    result:='';
    for i:=0 to length(tempAdapter.storedMessages)-1 do with tempAdapter.storedMessages[i] do case messageType of
      elo_echoOutput: result:=result+'out>'+simpleMessage+C_lineBreakChar;
      elp_printline : for j:=0 to length(multiMessage)-1 do result:=result+multiMessage[j]+C_lineBreakChar;
    end;
    //set-down:---------------------------------------------------
    clearErrors;
    tempAdapter.clearMessages;
    outAdapter:=previousAdapter;
    tempAdapter.destroy;
    //---------------------------------------------------:set-down
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
  {$include mnh_tokens_funcs.inc}

FINALIZATION
  pendingTasks.destroy;
  finalizePackages;

end.
