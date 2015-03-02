UNIT mnh_tokens;
INTERFACE
USES LCLIntf, myGenerics, mnh_constants, math, sysutils, mnh_stringUtil,  //utilities
     mnh_litvar, mnh_fileWrappers, mnh_tokLoc, //types
     {$ifdef PROFILING}epiktimer,{$endif}
     mnh_funcs, mnh_out_adapters, mnh_caches, mnh_doc; //even more specific

{$define include_interface}
TYPE
  P_package=^T_package;
  {$include mnh_tokens_token.inc}
  {$include mnh_tokens_pattern.inc}
  {$include mnh_tokens_subrule.inc}
  {$include mnh_tokens_rule.inc}
  {$include mnh_tokens_futureTask.inc}

  T_packageLoadUsecase=(lu_forImport,lu_forCallingMain,lu_forDirectExecution,lu_forDocGeneration);
  
  { T_package }
  T_ruleMap=specialize G_stringKeyMap<P_rule>;
  T_package=object
    private
      rules:T_ruleMap;
      packageUses:array of record
        id:ansistring;
        pack:P_package;
      end;
      ready:boolean;
      codeProvider:P_codeProvider;
      loadedVersion:longint;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider);
      FUNCTION needReload:boolean;
      PROCEDURE load(CONST usecase:T_packageLoadUsecase; VAR recycler:T_tokenRecycler);
      PROCEDURE clear;
      DESTRUCTOR destroy;
      PROCEDURE resolveRuleId(VAR token:T_token; CONST failSilently:boolean);
      FUNCTION ensureRuleId(CONST ruleId:ansistring):P_rule;
      PROCEDURE updateLists(VAR userDefinedLocalRules,userDefinesImportedRules:T_listOfString);
  end;

CONST C_id_qualify_character='.';
  
PROCEDURE reloadMainPackage(CONST usecase:T_packageLoadUsecase);
PROCEDURE callMainInMain(CONST parameters:array of ansistring);
PROCEDURE printMainPackageDocText;
FUNCTION getMainPackage:P_package;
FUNCTION getTokenAt(CONST line:ansistring; CONST charIndex:longint):T_token;
PROCEDURE findAndDocumentAllPackages;

VAR mainPackageProvider:T_codeProvider;
    
{$undef include_interface}
IMPLEMENTATION
CONST STACK_DEPTH_LIMIT=60000;
VAR secondaryPackages:array of P_package;
    mainPackage      :T_package;
    packagesAreFinalized:boolean=false;
    pendingTasks:T_taskQueue;
    {$ifdef PROFILING}
    profiler: Tepiktimer;
    {$endif}

FUNCTION guessPackageForToken(CONST token:T_token):P_package;
  VAR provider:P_codeProvider;
      packId:string;
      i:longint;
  begin
    provider:=token.location.provider;
    if provider=mainPackage.codeProvider then exit(@mainPackage);
    for i:=0 to length(secondaryPackages)-1 do
      if provider=secondaryPackages[i]^.codeProvider then exit(secondaryPackages[i]);
    if provider=nil then exit(@mainPackage);
    packId:=provider^.id;
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
{$include mnh_tokens_pattern.inc}
{$include mnh_tokens_subrule.inc}
{$include mnh_tokens_futureTask.inc}
{$include mnh_tokens_rule.inc}


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

FUNCTION loadPackage(CONST packageId:ansistring; CONST tokenLocation:T_tokenLocation; VAR recycler:T_tokenRecycler):P_package;
  VAR i:longint;
      newSourceName:ansistring;
      newSource:P_codeProvider=nil;
  begin
    for i:=0 to length(secondaryPackages)-1 do 
      if secondaryPackages[i]^.codeProvider^.id = packageId then begin
        if secondaryPackages[i]^.ready then begin
          if secondaryPackages[i]^.needReload then begin
            secondaryPackages[i]^.clear;
            secondaryPackages[i]^.load(lu_forImport,recycler);
          end;
          exit(secondaryPackages[i]);
        end else begin
          raiseError(el4_parsingError,'Cyclic package dependencies encountered; already loading "'+packageId+'"',tokenLocation);
          exit(nil);
        end;
      end;


    if tokenLocation.provider<>nil
    then newSourceName:=locateSource(tokenLocation.provider^.getPath,packageId)
    else newSourceName:=locateSource(''                             ,packageId);
    if newSourceName<>'' then begin
      new(newSource,create(newSourceName));
      new(result,create(newSource));
      setLength(secondaryPackages,length(secondaryPackages)+1);
      secondaryPackages[length(secondaryPackages)-1]:=result;
      result^.load(lu_forImport,recycler);
    end else begin
      raiseError(el4_parsingError,'Cannot locate package for id "'+packageId+'"',tokenLocation);
      result:=nil;
    end;
  end;

PROCEDURE T_package.load(CONST usecase:T_packageLoadUsecase; VAR recycler:T_tokenRecycler);
  VAR isFirstLine:boolean=true;
      doc:P_userPackageDocumentation;

  PROCEDURE pseudoLoadPackage(id:ansistring);
    VAR newSourceName:ansistring;
    begin
      newSourceName:=locateSource(codeProvider^.getPath,id);
      if newSourceName<>'' then doc^.addUses(newSourceName);
    end;

  PROCEDURE interpret(VAR first:P_token);
    PROCEDURE interpretUseClause;
      VAR temp:P_token;
          i,j:longint;
          locationForErrorFeedback:T_tokenLocation;
          newId:string;
      begin
        locationForErrorFeedback:=first^.location;
        temp:=first; first:=recycler.disposeToken(temp);
        while first<>nil do begin
          if first^.tokType=tt_identifier then begin            
            newId:=first^.txt;
            if isQualified(newId) then begin
              raiseError(el4_parsingError,'Cannot interpret use clause containing qualified identifier '+first^.toString,first^.location);
              exit;            
            end;
            //no duplicates are created; packages are always added at the end
            i:=0;
            while (i<length(packageUses)) and (packageUses[i].id<>newId) do inc(i);
            if i<length(packageUses) then for j:=i to length(packageUses)-2 do packageUses[j]:=packageUses[j+1]
                                     else setLength(packageUses,length(packageUses)+1);
            with packageUses[length(packageUses)-1] do begin
              id:=first^.txt;
              pack:=nil;
            end;
          end else if first^.tokType<>tt_separatorComma then begin
            raiseError(el4_parsingError,'Cannot interpret use clause containing '+first^.toString,first^.location);
            exit;
          end;
          temp:=first; first:=recycler.disposeToken(temp);
        end;
        if usecase=lu_forDocGeneration then begin
          for i:=0 to length(packageUses)-1 do pseudoLoadPackage(packageUses[i].id);
          setLength(packageUses,0);
        end else begin
          for i:=0 to length(packageUses)-1 do with packageUses[i] do pack:=loadPackage(id,locationForErrorFeedback,recycler);
          i:=0;
          while i<length(packageUses) do begin
            if packageUses[i].pack=nil then begin
              for j:=i to length(packageUses)-2 do packageUses[j]:=packageUses[j+1];
              setLength(packageUses,length(packageUses)-1);
            end else inc(i);
          end;
        end;
      end;
      
    VAR assignmentToken:P_token;

    PROCEDURE parseRule;
      VAR p,n,nn,nnn:P_token;
          ruleIsPrivate:boolean=false;
          ruleIsMemoized:boolean=false;
          ruleId:string;
          evaluateBody:boolean;
          rulePattern:T_pattern;
          ruleBody:P_token;
          ruleDeclarationStart:T_tokenLocation;
          subRule:P_subrule;
      begin
        ruleDeclarationStart:=first^.location;
        evaluateBody:=(assignmentToken^.tokType=tt_assign);
        ruleBody:=assignmentToken^.next;


        //plausis:
        if (ruleBody=nil) then begin
          raiseError(el4_parsingError,'Missing FUNCTION body after assignment/declaration token.',assignmentToken^.location);
          recycler.cascadeDisposeToken(first);
          exit;
        end;
        p:=ruleBody^.getDeclarationOrAssignmentToken;
        if (p<>nil) then begin
          raiseError(el4_parsingError,'FUNCTION body contains unplausible assignment/declaration token.',p^.location);
          recycler.cascadeDisposeToken(first);
          exit;
        end;
        while (first<>nil) and (first^.tokType in [tt_modifier_private,tt_modifier_memoized]) do begin
          if first^.tokType=tt_modifier_private then ruleIsPrivate:=true;
          if first^.tokType=tt_modifier_memoized then ruleIsMemoized:=true;
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
            n  :=first^.next;
            nn :=n    ^.next;
            nnn:=nn   ^.next;
            if (first^.tokType=tt_identifier)
            and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendFreeId(first^.txt);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType in [lt_boolean, lt_int, lt_real, lt_string])
                     and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison('',tt_comparatorEq,P_scalarLiteral(first^.data));
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_list) and (P_listLiteral(first^.data)^.size=0) then begin
              rulePattern.appendTypeCheck('',tt_typeCheckEmptyList);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_typeCheckScalar, tt_typeCheckList, tt_typeCheckBoolean, tt_typeCheckBoolList, tt_typeCheckInt, tt_typeCheckIntList, tt_typeCheckReal,tt_typeCheckRealList, tt_typeCheckString,tt_typeCheckStringList, tt_typeCheckNumeric, tt_typeCheckNumList, tt_typeCheckExpression, tt_typeCheckNonemptyList, tt_typeCheckEmptyList])
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

        if evaluateBody and (usecase<>lu_forDocGeneration) then reduceExpression(ruleBody,0,recycler);

        if   errorLevel<el3_evalError then begin
          new(subrule,create(rulePattern,ruleBody,ruleDeclarationStart,ruleIsPrivate,recycler));
          ensureRuleId(ruleId)^.addOrReplaceSubRule(subrule);
          if ruleIsMemoized then ensureRuleId(ruleId)^.setMemoized;
          first:=nil;
          if usecase=lu_forDocGeneration then doc^.addSubRule(ruleId,subRule^.pattern.toString,ruleIsMemoized,ruleIsPrivate);
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
        writeDeclEcho(tokensToString(first));
        parseRule;
      end else begin
        if usecase=lu_forDirectExecution then begin
          predigest(first,@self,recycler);
          writeExprEcho(tokensToString(first));
          reduceExpression(first,0,recycler);
          if first<>nil then writeExprOut(tokensToString(first));
        end else raiseError(el1_note,'Skipping expression '+tokensToString(first),first^.location);
      end;
      if first<>nil then recycler.cascadeDisposeToken(first);
      first:=nil;
    end;

  VAR codeLines:T_stringList;
      i:longint;
      next:T_token;
      first,last:P_token;
      location:T_TokenLocation;
  begin
    if usecase=lu_forDocGeneration then new(doc,create(codeProvider^.getPath,codeProvider^.id));
    clear;
    loadedVersion:=codeProvider^.getVersion((usecase=lu_forCallingMain) or (codeProvider<>@mainPackageProvider));
    codeLines:=codeProvider^.getLines;

    first:=nil;
    last :=nil;
    location.provider:=codeProvider;
    for i:=0 to length(codeLines)-1 do if (errorLevel<el3_evalError) then begin
      location.line:=i+1;
      location.column:=1;
      while not(isBlank(codeLines[i])) and (errorLevel<el3_evalError) do begin
        next:=firstToken(codeLines[i],location,@self,true);
        if (next.tokType=tt_semicolon) then begin
          if first<>nil then interpret(first);
          last:=nil;
          first:=nil;
        end else if (next.tokType<>tt_eol) then begin
          if first=nil then begin
            first:=recycler.newToken(next); next.undefine;
            last :=first
          end else begin
            last^.next:=recycler.newToken(next); next.undefine;
            last      :=last^.next;
          end;
          last^.next:=nil;
        end else if (usecase=lu_forDocGeneration) and (next.txt<>'') then doc^.addComment(next.txt);
      end;
    end;
    if (errorLevel<el3_evalError) then begin
      if first<>nil then interpret(first);
    end else recycler.cascadeDisposeToken(first);
    ready:=true;
    raiseError(el0_allOkay,'Package '+codeProvider^.id+' ready.',location);
  end;

CONSTRUCTOR T_package.create(CONST provider: P_codeProvider);
  begin
    setLength(packageUses,0);
    codeProvider:=provider;
    rules.create;
    loadedVersion:=-1;
  end;

FUNCTION T_package.needReload: boolean;
  begin
    result:=loadedVersion<>codeProvider^.getVersion(true);
  end;

PROCEDURE T_package.clear;
  VAR rule:P_rule;
  begin
    while rules.size>0 do begin
      rule:=rules.dropAny;
      dispose(rule,destroy);
    end;
    rules.clear;

    setLength(packageUses,0);
    ready:=false;
  end;

DESTRUCTOR T_package.destroy;
  begin
    clear;
    if codeProvider<>@mainPackageProvider then dispose(codeProvider,destroy);
    rules.destroy;
    setLength(packageUses,0);
  end;

PROCEDURE T_package.resolveRuleId(VAR token: T_token;
  CONST failSilently: boolean);
  VAR i:longint;
      userRule:P_rule;
      intrinsicFuncPtr:T_intFuncCallback;
      packageId,ruleId:ansistring;
  begin    
    {$ifdef PROFILING}
    profiler.Clear;
    profiler.Start;
    {$endif}

    i:=pos(C_id_qualify_character,token.txt);
    if i>0 then begin
      packageId:=copy(token.txt,1,i-1);
      ruleId   :=copy(token.txt,i+1,length(token.txt));
    end else begin
      packageId:='';
      ruleId   :=token.txt;
    end;
    if ((codeProvider<>nil) and (packageId=codeProvider^.id) or (packageId=''))
    and rules.containsKey(ruleId,userRule) then begin
      token.tokType:=tt_localUserRulePointer;
      token.data:=userRule;
      {$ifdef PROFILING}
      writeln(stderr,'PROFILING;T_package.resolveRuleId (0);' + FloatToStr(profiler.Elapsed));
      {$endif}
      exit;
    end;

    for i:=length(packageUses)-1 downto 0 do
    if ((packageId=packageUses[i].id) and (packageUses[i].pack<>nil) and (packageUses[i].pack^.ready) or (packageId=''))
    and (packageUses[i].pack<>nil)
    and (packageUses[i].pack^.rules.containsKey(ruleId,userRule)) and (userRule^.hasPublicSubrule) then begin
      token.tokType:=tt_importedUserRulePointer;
      token.data:=userRule;
      {$ifdef PROFILING}
      writeln(stderr,'PROFILING;T_package.resolveRuleId (1);' + FloatToStr(profiler.Elapsed));
      {$endif}
      exit;
    end;

    if ((packageId='mnh') or (packageId='')) 
    and intrinsicRuleMap.containsKey(ruleId,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRulePointer;
      token.data:=intrinsicFuncPtr;
      {$ifdef PROFILING}
      writeln(stderr,'PROFILING;T_package.resolveRuleId (2);' + FloatToStr(profiler.Elapsed));
      {$endif}
      exit;
    end;
    {$ifdef PROFILING}
    writeln(stderr,'PROFILING;T_package.resolveRuleId (X);' + FloatToStr(profiler.Elapsed));
    {$endif}

    if not(failSilently) then raiseError(el4_parsingError,'Cannot resolve ID "'+token.txt+'"',token.location);
  end;

FUNCTION T_package.ensureRuleId(CONST ruleId: ansistring): P_rule;
  begin
    if not(rules.containsKey(ruleId,result)) then begin
      new(result,create(ruleId));
      rules.put(ruleId,result);
      raiseError(el0_allOkay,'New rule '+ruleId,fileTokenLocation(codeProvider));
    end;
  end;

PROCEDURE T_package.updateLists(VAR userDefinedLocalRules, userDefinesImportedRules: T_listOfString);
  VAR i,j:longint;
      ids:T_arrayOfString;
      packageId:ansistring;
  begin
    userDefinedLocalRules.clear;
    userDefinedLocalRules.addArr(rules.keySet);
    userDefinedLocalRules.unique;
    userDefinesImportedRules.clear;
    for i:=0 to length(packageUses)-1 do if (packageUses[i].pack<>nil) and packageUses[i].pack^.ready then begin
      packageId:=packageUses[i].id;
      ids:=packageUses[i].pack^.rules.keySet;
      for j:=0 to length(ids)-1 do begin
        userDefinesImportedRules.add(ids[j]);
        userDefinesImportedRules.add(packageId+C_id_qualify_character+ids[j]);
      end;
    end;
    userDefinesImportedRules.unique;
  end;

FUNCTION stringToExpression(s:ansistring; CONST location:T_tokenLocation):P_scalarLiteral;
  VAR next:T_token;
      first,last,temp:P_token;
      loc:T_tokenLocation;
      recycler:T_tokenRecycler;
  begin
    loc:=location;
    next:=firstToken(s,loc,nil,true);
    if next.tokType=tt_eol then exit(newErrorLiteralRaising('The parsed expression appears to be empty',location));
    recycler.create;
    first:=recycler.newToken(next);
    last:=first;
    repeat
      loc:=location;
      next:=firstToken(s,loc,nil,true);
      if next.tokType<>tt_eol then begin
        last^.next:=recycler.newToken(next);
        last:=last^.next;
      end;
    until (next.tokType in [tt_eol,tt_semicolon]) or (errorLevel>=el3_evalError);
    if errorLevel>=el3_evalError then begin
      recycler.cascadeDisposeToken(first);
      recycler.destroy;
      exit(newErrorLiteral);
    end;

    if first^.tokType<>tt_expBraceOpen then begin
      temp:=recycler.newToken(location,'',tt_expBraceOpen);
      temp^.next:=first; first:=temp;
      last^.next:=recycler.newToken(location,'',tt_expBraceClose);
      last:=last^.next;
    end;

    digestInlineExpression(first,recycler);
    if (errorLevel<el3_evalError) and (first^.next<>nil) then raiseError(el4_parsingError,'The parsed expression goes beyond the expected limit... I know this is a fuzzy error. Sorry.',location);
    if errorLevel>=el3_evalError then begin
      recycler.cascadeDisposeToken(first);
      recycler.destroy;
      exit(newErrorLiteral);
    end;
    if (first^.tokType<>tt_literal) or (P_literal(first^.data)^.literalType<>lt_expression) then begin
      recycler.disposeToken(first);
      raiseError(el5_systemError,'This is unexpected. The result of mnh_tokens.stringToExpression should be an expression!',location);
      recycler.destroy;
      exit(newErrorLiteral);
    end;
    result:=P_expressionLiteral(first^.data);
    first^.tokType:=tt_eol;
    first^.data:=nil;
    recycler.disposeToken(first);

    recycler.destroy;

  end;
  
PROCEDURE callMainInMain(CONST parameters:array of ansistring);
  VAR t:P_token;
      parLit:P_listLiteral;
      i:longint;
      mainRule:P_rule;
      recycler:T_tokenRecycler;
  begin
    recycler.create;
    if not(mainPackage.ready) or (errorLevel>el1_note) then begin
      raiseError(el5_systemError,'Call of main has been rejected due to a previous error or note.',fileTokenLocation(@mainPackageProvider));
      exit;
    end;

    t:=recycler.newToken(fileTokenLocation(@mainPackageProvider),'main',tt_identifier);
    mainPackage.load(lu_forCallingMain,recycler);
    if not(mainPackage.rules.containsKey('main',mainRule)) then
      raiseError(el3_evalError,'The specified package contains no main rule.',fileTokenLocation(@mainPackageProvider))
    else begin
      t^.tokType:=tt_localUserRulePointer;
      t^.data:=mainRule;

      parLit:=newListLiteral;
      for i:=0 to length(parameters)-1 do parLit^.append(newStringLiteral(parameters[i]),false);
      t^.next:=recycler.newToken(fileTokenLocation(@mainPackageProvider),'',tt_parList,parLit);
      reduceExpression(t,0,recycler);
      //special handling if main returns an expression:
      if (t^.tokType=tt_literal) and (t^.next=nil) and
         (P_literal(t^.data)^.literalType=lt_expression) then begin
        P_subrule(P_expressionLiteral(t^.data)^.value)^.directEvaluateNullary(nil,0,recycler);
      end;
      //:special handling if main returns an expression
    end;
    recycler.cascadeDisposeToken(t);
    recycler.destroy;
  end;

PROCEDURE printMainPackageDocText;
  begin
    mainPackageProvider.load;
    reloadMainPackage(lu_forDocGeneration);
    writeUserPackageHelpText;
  end;

FUNCTION getMainPackage: P_package;
  begin
    result:=@mainPackage;
  end;

FUNCTION getTokenAt(CONST line: ansistring; CONST charIndex: longint): T_token;
  VAR copyOfLine:ansistring;
      lineLocation:T_tokenLocation;
  begin
    {$ifdef PROFILING}
    profiler.Clear;
    profiler.Start;
    {$endif}
    
    try
      copyOfLine:=line;
      lineLocation.provider:=mainPackage.codeProvider;
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
      result.define(C_nilTokenLocation,'ERR',tt_eol);
    end;
    {$ifdef PROFILING}
    writeln(stderr,'PROFILING;getTokenAt;' + FloatToStr(profiler.Elapsed));
    {$endif}
  end;

PROCEDURE findAndDocumentAllPackages;
  VAR sourceNames:T_stringList;
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
      p.destroy;
    end;
    writeUserPackageDocumentations;
    documentBuiltIns;
    recycler.destroy;
  end;

INITIALIZATION
  mainThread:=ThreadId;
  mainPackageProvider.create;
  mainPackage.create(@mainPackageProvider);
  setLength(secondaryPackages,0);
  pendingTasks.create;

  //callbacks in mnh_litvar:
  disposeSubruleCallback :=@disposeSubruleImpl;
  subruleToStringCallback:=@subruleToStringImpl;
  subruleApplyOpCallback :=@subruleApplyOpImpl;
  //callbacks in mnh_funcs:
  resolveNullaryCallback:=@evaluateNullary;
  stringToExprCallback:=@stringToExpression;
  applyUnaryOnExpressionCallback:=@subruleApplyFuncImpl;
  {$ifdef PROFILING}
  profiler := TEpikTimer.create(nil);
  {$endif}
FINALIZATION
  pendingTasks.destroy;
  finalizePackages;

end.
