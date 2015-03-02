UNIT mnh_tokens;
INTERFACE
USES myGenerics, mnh_constants, math, sysutils, mnh_stringUtil,  //utilities
     mnh_litvar, mnh_fileWrappers, mnh_tokLoc, //types
     mnh_funcs, mnh_out_adapters, mnh_caches; //even more specific

{$define doTokenRecycling}
{$define include_interface}
TYPE
  P_package=^T_package;
  {$include mnh_tokens_token.inc}
  {$include mnh_tokens_pattern.inc}
  {$include mnh_tokens_subrule.inc}
  {$include mnh_tokens_rule.inc}

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
      PROCEDURE load;
      PROCEDURE clear;
      DESTRUCTOR destroy;
      PROCEDURE resolveRuleId(VAR token:T_token; CONST failSilently:boolean);
      FUNCTION ensureRuleId(CONST ruleId:ansistring):P_rule;
      PROCEDURE updateLists(VAR userDefinedLocalRules,userDefinesImportedRules:T_listOfString);
  end;

CONST C_id_qualify_character='.';
  
//FUNCTION isReloadOfMainPackageIndicated:boolean;
PROCEDURE reloadMainPackage;
PROCEDURE callMainInMain(CONST parameters:array of ansistring);
FUNCTION getMainPackage:P_package;
FUNCTION getTokenAt(CONST line:AnsiString; CONST charIndex:longint):T_token;
VAR mainPackageProvider:T_codeProvider;
{$undef include_interface}
IMPLEMENTATION
{$define include_implementation}
{$include mnh_tokens_token.inc}
{$include mnh_tokens_pattern.inc}
{$include mnh_tokens_subrule.inc}
{$include mnh_tokens_rule.inc}
VAR secondaryPackages:array of P_package;
    mainPackage      :T_package;
    packagesAreFinalized:boolean=false;
    
PROCEDURE reloadMainPackage;
  VAR i,j:longint;
      used:T_listOfString;
  begin
    clearAllCaches;
    clearErrors;
    mainPackage.load;

    //housekeeping:-------------------------------------------------------------
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
  end;
  
PROCEDURE finalizePackages;
  VAR i:longint;
  begin
    if packagesAreFinalized then exit;
    mainPackage.destroy;
    mainPackageProvider.destroy;  
    clearAllCaches;
    clearErrors;
    clearSourceScanPaths;
    for i:=length(secondaryPackages)-1 downto 0 do dispose(secondaryPackages[i],destroy);
    setLength(secondaryPackages,0);
    packagesAreFinalized:=true;
  end;

FUNCTION loadPackage(CONST packageId:ansistring; CONST tokenLocation:T_tokenLocation):P_package;
  VAR i:longint;
      newSourceName:ansistring;
      newSource:P_codeProvider=nil;
  begin
    for i:=0 to length(secondaryPackages)-1 do 
      if secondaryPackages[i]^.codeProvider^.id = packageId then begin
        if secondaryPackages[i]^.ready then begin
          if secondaryPackages[i]^.needReload then begin
            secondaryPackages[i]^.clear;
            secondaryPackages[i]^.load;
          end;
          exit(secondaryPackages[i]);
        end else begin
          raiseError(el4_parsingError,'Cyclic package dependencies encountered; already loading "'+packageId+'"',tokenLocation);
          exit(nil);
        end;
      end;
    
    newSourceName:=locateSource(packageId);
    if newSourceName<>'' then begin
      new(newSource,create(newSourceName));
      new(result,create(newSource));
      setLength(secondaryPackages,length(secondaryPackages)+1);
      secondaryPackages[length(secondaryPackages)-1]:=result;
      result^.load;
    end else begin
      raiseError(el4_parsingError,'Cannot locate package for id "'+packageId+'"',tokenLocation);
      result:=nil;
    end;
  end;

PROCEDURE T_package.load;
  PROCEDURE predigestBeforeDeclarationParsing(VAR first:P_token);
    VAR this,next,prev:P_token;
    begin
      this:=first; prev:=nil;
      while this<>nil do begin
        next:=this^.next;
        if (this^.tokType in [tt_operatorMinus,tt_operatorPlus]) and
           ((prev=nil) or (prev^.tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_set,tt_each,tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus]))
           and (this^.next<>nil) and (this^.next^.tokType in [tt_literal,tt_identifier,tt_braceOpen,tt_listBraceOpen,tt_expBraceOpen,tt_localUserRulePointer,tt_importedUserRulePointer,tt_intrinsicRulePointer,tt_parameterIdentifier]) then begin
          if this^.tokType=tt_operatorMinus then begin
            if (next<>nil) and (next^.tokType = tt_literal) then begin
              this^.tokType:=tt_literal;
              this^.data:=P_literal(next^.data)^.negate(this^.location);
              this^.next:=disposeToken(next);
            end else this^.tokType:=tt_unaryOpMinus;
          end else begin
            if prev=nil then begin
              first:=disposeToken(this);
              this:=first;
            end else begin
              prev^.next:=disposeToken(this);
              this:=next;
            end;
          end;
        end else if (this^.tokType=tt_listBraceOpen) and (this^.next<>nil) and (this^.next^.tokType=tt_listBraceClose) then begin
          this^.tokType:=tt_literal;
          this^.data:=newListLiteral;
          this^.next:=disposeToken(this^.next);
        end;
        prev:=this;
        this:=this^.next;
      end;
    end;

  PROCEDURE predigest(VAR first:P_token);
    VAR t:P_token;
    begin
      t:=first;
      while t<>nil do begin
        case t^.tokType of
        tt_identifier: t^.data:=@self;
        tt_set: if  (t^.next            <>nil) and (t^.next^            .tokType=tt_braceOpen)
                and (t^.next^.next      <>nil) and (t^.next^.next^      .tokType=tt_identifier)
                and (t^.next^.next^.next<>nil) and (t^.next^.next^.next^.tokType=tt_separatorComma) then begin
          t^.data:=ensureRuleId(t^.next^.next^.txt);
          t^.next:=disposeToken(t^.next); //dispose (
          t^.next:=disposeToken(t^.next); //dispose <id>
          t^.next:=disposeToken(t^.next); //dispose ,
        end else begin
          raiseError(el4_parsingError,'Invalid set-expression; expected to start with "set(<id>,"',t^.location);
          exit;
        end;
        tt_each:if  (t^.next            <>nil) and (t^.next^            .tokType=tt_braceOpen)
                and (t^.next^.next      <>nil) and (t^.next^.next^      .tokType=tt_identifier)
                and (t^.next^.next^.next<>nil) and (t^.next^.next^.next^.tokType=tt_separatorComma) then begin
          t^.txt:=t^.next^.next^.txt;
          t^.data:=nil;
          t^.next:=disposeToken(disposeToken(disposeToken(t^.next))); //dispose ( , <id> and ","
        end else begin
          raiseError(el4_parsingError,'Invalid each-expression; expected to start with "each(<id>,"',t^.location);
          exit;
        end;

        end;
        t:=t^.next;
      end;
    end;

  VAR isFirstLine:boolean=true;
  PROCEDURE interpret(VAR first:P_token);
    PROCEDURE interpretUseClause;
      VAR temp:P_token;
          i,j:longint;
          locationForErrorFeedback:T_tokenLocation;
          newId:string;
      begin
        locationForErrorFeedback:=first^.location;
        temp:=first; first:=disposeToken(temp);
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
          temp:=first; first:=disposeToken(temp);
        end;
        for i:=0 to length(packageUses)-1 do with packageUses[i] do pack:=loadPackage(id,locationForErrorFeedback);
        i:=0;
        while i<length(packageUses) do begin
          if packageUses[i].pack=nil then begin
            for j:=i to length(packageUses)-2 do packageUses[j]:=packageUses[j+1];
            setLength(packageUses,length(packageUses)-1);
          end else inc(i);
        end;
      end;
      
    VAR assignmentToken:P_token;

    PROCEDURE parseRule;
      VAR p,n,nn,nnn:P_token;
          ruleIsPrivate:boolean=false;
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
          raiseError(el4_parsingError,'Missing function body after assignment/declaration token.',assignmentToken^.location);
          cascadeDisposeToken(first);
          exit;
        end;
        p:=ruleBody^.getDeclarationOrAssignmentToken;
        if (p<>nil) then begin
          raiseError(el4_parsingError,'Function body contains unplausible assignment/declaration token.',p^.location);
          cascadeDisposeToken(first);
          exit;
        end;
        if not(first^.tokType in [tt_identifier, tt_localUserRulePointer, tt_importedUserRulePointer, tt_intrinsicRulePointer]) then begin
          raiseError(el4_parsingError,'Declaration does not start with an identifier.',first^.location);
          cascadeDisposeToken(first);
          exit;
        end;
        //:plausis
        if (first^.next^.tokType=tt_identifier) and (first^.tokType=tt_identifier) then begin
          if trim(first^.txt)='private' then ruleIsPrivate:=true
          else if trim(first^.txt)<>'public' then begin
            raiseError(el4_parsingError,'Invalid declaration head.',first^.location);
            cascadeDisposeToken(first);
            exit;
          end;
          first:=disposeToken(first);          
        end;
        p:=first;
        while (p<>nil) and not(p^.tokType in [tt_assign,tt_declare]) do begin
          if (p^.tokType in [tt_identifier, tt_localUserRulePointer, tt_importedUserRulePointer, tt_intrinsicRulePointer]) and isQualified(first^.txt) then begin
            raiseError(el4_parsingError,'Declaration head contains qualified ID.',p^.location);
            cascadeDisposeToken(first);
            exit;
          end;
          p:=p^.next;
        end;
        
        ruleId:=trim(first^.txt);
        p:=first;
        first:=disposeToken(p);
        if not(first^.tokType in [tt_braceOpen,tt_assign,tt_declare])  then begin
          raiseError(el4_parsingError,'Invalid declaration head.',first^.location);
          cascadeDisposeToken(first);
          exit;
        end;
        rulePattern.create;
        if first^.tokType=tt_braceOpen then begin
          first:=disposeToken(first);
          while not((first=nil) or (first^.tokType in [tt_assign,tt_declare])) do begin
            n  :=first^.next;
            nn :=n    ^.next;
            nnn:=nn   ^.next;
            if (first^.tokType=tt_identifier)
            and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendFreeId(first^.txt);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType in [lt_boolean, lt_int, lt_real, lt_string])
                     and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison('',tt_comparatorEq,P_scalarLiteral(first^.data));
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_list) and (P_listLiteral(first^.data)^.size=0) then begin
              rulePattern.appendTypeCheck('',tt_typeCheckEmptyList);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_typeCheckScalar, tt_typeCheckList, tt_typeCheckBoolean, tt_typeCheckBoolList, tt_typeCheckInt, tt_typeCheckIntList, tt_typeCheckReal,tt_typeCheckRealList, tt_typeCheckString,tt_typeCheckStringList, tt_typeCheckNumeric, tt_typeCheckNumList, tt_typeCheckExpression, tt_typeCheckNonemptyList, tt_typeCheckEmptyList])
                     and (nn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendTypeCheck(first^.txt,n^.tokType);
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
                     and (nn^.tokType=tt_literal) and (P_literal(nn^.data)^.literalType in [lt_boolean, lt_int, lt_real, lt_string])
                     and (nnn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison(first^.txt,n^.tokType,P_scalarLiteral(nn^.data));
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_identifier)
                     and (n^.tokType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
                     and (nn^.tokType=tt_identifier)
                     and (nnn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison(first^.txt,n^.tokType,nn^.txt);
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else begin
              raiseError(el4_parsingError,'Invalid declaration pattern element.',first^.location);
              cascadeDisposeToken(first);
              exit;
            end;
          end;
        end;
        if first<>nil then begin
          first:=disposeToken(first);
        end else begin
          raiseError(el4_parsingError,'Invalid declaration.',ruleDeclarationStart);
          exit;
        end;

        if evaluateBody then reduceExpression(ruleBody,0);
        if   errorLevel<el3_evalError then begin
          new(subrule,create(rulePattern,ruleBody,ruleDeclarationStart));
          subRule^.publish:=not(ruleIsPrivate);
          ensureRuleId(ruleId)^.addOrReplaceSubRule(subrule);
          first:=nil;
        end else if errorLevel<el5_systemError then
          cascadeDisposeToken(first)
        else
          first:=nil;
      end;
      
    FUNCTION parseCache:boolean;
      VAR ruleIdToken:P_token;
          specifier:P_token;
          cacheSize:longint;
      begin
        if not(first^.tokType in [tt_identifier,tt_intrinsicRulePointer,tt_localUserRulePointer,tt_importedUserRulePointer,tt_parameterIdentifier]) or
              (first^.txt<>'CACHE') or 
              (first^.next=nil) 
        then exit(false);
        ruleIdToken:=first^.next;
        if not(ruleIdToken^.tokType in [tt_identifier,tt_intrinsicRulePointer,tt_localUserRulePointer,tt_importedUserRulePointer,tt_parameterIdentifier])
        then exit(false);
        specifier:=ruleIdToken^.next;
        if (specifier=nil) or (specifier^.tokType=tt_literal) 
                and (P_literal(specifier^.data)^.literalType=lt_int)
                and (specifier^.next=nil) then begin          
          if specifier=nil then cacheSize:=100
                           else cacheSize:=P_intLiteral(specifier^.data)^.value;
          ensureRuleId (ruleIdToken^.txt)^.setCached(cacheSize);
          cascadeDisposeToken(first);
          first:=nil;
          result:=true;
        end else result:=false;
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
      if parseCache then exit;
      predigestBeforeDeclarationParsing(first);
      assignmentToken:=first^.getDeclarationOrAssignmentToken;
      if assignmentToken=nil then predigest(first)
                             else predigest(assignmentToken);
      if assignmentToken<>nil then begin
        writeDeclEcho(tokensToString(first));
        parseRule;
      end else begin
        writeExprEcho(tokensToString(first));
        reduceExpression(first,0);
        if first<>nil then writeExprOut(tokensToString(first));
      end;
      if first<>nil then cascadeDisposeToken(first);
    end;

  VAR codeLines:T_stringList;
      i:longint;
      next:T_token;
      first,last:P_token;
      location:T_TokenLocation;
  begin
    clear;
    loadedVersion:=codeProvider^.getVersion(true);
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
            first:=newToken(next); next.undefine;
            last :=first
          end else begin
            last^.next:=newToken(next); next.undefine;
            last      :=last^.next;
          end;
        end;
      end;
    end;
    if (errorLevel<el3_evalError) then begin
      if first<>nil then interpret(first);
    end else cascadeDisposeToken(first);
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
    i:=pos(C_id_qualify_character,token.txt);
    if i>0 then begin
      packageId:=copy(token.txt,1,i-1);
      ruleId   :=copy(token.txt,i+1,length(token.txt));
    end else begin
      packageId:='';
      ruleId   :=token.txt;
    end;
    if ((packageId=codeProvider^.id) or (packageId=''))
    and rules.containsKey(ruleId,userRule) then begin
      token.tokType:=tt_localUserRulePointer;
      token.data:=userRule;
      exit;
    end;

    for i:=length(packageUses)-1 downto 0 do
    if ((packageId=packageUses[i].id) or (packageId=''))
    and (packageUses[i].pack<>nil)
    and (packageUses[i].pack^.rules.containsKey(ruleId,userRule)) and (userRule^.hasPublicSubrule) then begin
      token.tokType:=tt_importedUserRulePointer;
      token.data:=userRule;
      exit;
    end;

    if ((packageId='mnh') or (packageId='')) 
    and intrinsicRuleMap.containsKey(ruleId,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRulePointer;
      token.data:=intrinsicFuncPtr;
      exit;
    end;

    if not(failSilently) then raiseError(el4_parsingError,'Cannot resolve ID "'+token.txt+'"',token.location);
  end;

FUNCTION T_package.ensureRuleId(CONST ruleId: ansistring): P_rule;
  begin
    if not(rules.containsKey(ruleId,result)) then begin
      new(result,create(ruleId));
      rules.put(ruleId,result);
      raiseError(el0_allOkay,'New rule '+ruleId,C_nilTokenLocation);
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
    for i:=0 to length(packageUses)-1 do begin
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
  begin
    loc:=location;
    next:=firstToken(s,loc,nil,true);
    if next.tokType=tt_eol then exit(newErrorLiteralRaising('The parsed expression appears to be empty',location));
    first:=newToken(next);
    last:=first;
    repeat
      loc:=location;
      next:=firstToken(s,loc,nil,true);
      if next.tokType<>tt_eol then begin
        last^.next:=newToken(next);
        last:=last^.next;
      end;
    until (next.tokType in [tt_eol,tt_semicolon]) or (errorLevel>=el3_evalError);
    if errorLevel>=el3_evalError then begin cascadeDisposeToken(first); exit(newErrorLiteral); end;

    if first^.tokType<>tt_expBraceOpen then begin
      temp:=newToken(location,'',tt_expBraceOpen);
      temp^.next:=first; first:=temp;
      last^.next:=newToken(location,'',tt_expBraceClose);
      last:=last^.next;
    end;

    digestInlineExpression(first);
    if (errorLevel<el3_evalError) and (first^.next<>nil) then raiseError(el4_parsingError,'The parsed expression goes beyond the expected limit... I know this is a fuzzy error. Sorry.',location);
    if errorLevel>=el3_evalError then begin cascadeDisposeToken(first); exit(newErrorLiteral); end;
    if (first^.tokType<>tt_literal) or (P_literal(first^.data)^.literalType<>lt_expression) then begin
      disposeToken(first);
      raiseError(el5_systemError,'This is unexpected. The result of mnh_tokens.stringToExpression should be an expression!',location);
      exit(newErrorLiteral);
    end;
    result:=P_expressionLiteral(first^.data);
    first^.tokType:=tt_eol;
    first^.data:=nil;
    disposeToken(first);
  end;
  
PROCEDURE callMainInMain(CONST parameters:array of ansistring);
  VAR t:P_token;
      parLit:P_listLiteral;
      i:longint;
  begin
    if not(mainPackage.ready) or (errorLevel>el0_allOkay) then exit;
    finalizeTokens;
    t:=newToken(C_nilTokenLocation,'main',tt_identifier);
    mainPackage.resolveRuleId(t^,false);
    if t^.tokType=tt_identifier then raiseError(el3_evalError,'The specified package contains no main rule.',C_nilTokenLocation)
    else begin
      parLit:=newListLiteral;
      for i:=0 to length(parameters)-1 do parLit^.append(newStringLiteral(parameters[i]),false);
      t^.next:=newToken(C_nilTokenLocation,'',tt_parList,parLit);
      reduceExpression(t,0);
    end;
    cascadeDisposeToken(t);
  end;

FUNCTION getMainPackage: P_package;
  begin
    result:=@mainPackage;
  end;

FUNCTION getTokenAt(CONST line: AnsiString; CONST charIndex: longint): T_token;
  VAR copyOfLine:AnsiString;
      lineLocation:T_tokenLocation;
  begin
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
  end;

INITIALIZATION
  mainPackageProvider.create;
  mainPackage.create(@mainPackageProvider);
  setLength(secondaryPackages,0);
  {$ifdef doTokenRecycling}
  tokenRecycling.fill:=0;
  {$endif}
  //callbacks in mnh_litvar:
  disposeSubruleCallback :=@disposeSubruleImpl;
  subruleToStringCallback:=@subruleToStringImpl;
  subruleApplyOpCallback :=@subruleApplyOpImpl;
  //callbacks in mnh_funcs:
  resolveNullaryCallback:=@evaluateNullary;
  stringToExprCallback:=@stringToExpression;
FINALIZATION
  finalizePackages;
  finalizeTokens;
 
end.
