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

  T_package=object
    private
      publicRules:specialize G_stringKeyMap<P_rule>;
      localRules:specialize G_stringKeyMap<P_rule>;
      packageUses:array of record
        id:ansistring;
        pack:P_package;
      end;
      ready:boolean;
      codeProvider:P_codeProvider;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider);
      PROCEDURE load;
      PROCEDURE clear;
      DESTRUCTOR destroy;
      PROCEDURE resolveRuleId(VAR token:T_token; CONST failSilently:boolean);
      FUNCTION canResolveId(CONST identifier:ansistring):byte;
      FUNCTION ruleLocation(CONST identifier:ansistring):T_tokenLocation;
      
      FUNCTION ensureLocalRuleId(CONST ruleId:ansistring):P_rule;
      FUNCTION ensurePublicRuleId(CONST ruleId:ansistring):P_rule;
  end;

CONST C_id_qualify_character='.';
  
FUNCTION isReloadOfAllPackagesIndicated:boolean;
FUNCTION isReloadOfMainPackageIndicated:boolean;
PROCEDURE reloadAllPackages;
PROCEDURE reloadMainPackage;
PROCEDURE clearAllPackages;
PROCEDURE initMainPackage(CONST filename:ansistring);
PROCEDURE initMainPackage(CONST directInputWrapper:P_codeProvider);
FUNCTION canResolveInMainPackage(CONST id:ansistring):byte;
FUNCTION getRuleLocation(CONST id:ansistring):T_tokenLocation;
FUNCTION getPackageLocation(CONST id:ansistring):T_tokenLocation;
PROCEDURE callMainInMain(CONST parameters:array of ansistring);
{$undef include_interface}
IMPLEMENTATION
{$define include_implementation}
{$include mnh_tokens_token.inc}
{$include mnh_tokens_pattern.inc}
{$include mnh_tokens_subrule.inc}
{$include mnh_tokens_rule.inc}
VAR packages:array of P_package;
FUNCTION isReloadOfAllPackagesIndicated:boolean;
  VAR i:longint;
  begin
    for i:=1 to length(packages)-1 do if packages[i]^.codeProvider^.fileChanged then exit(true);
    result:=false;
  end;

FUNCTION isReloadOfMainPackageIndicated:boolean;
  begin
    result:=(length(packages)>0) and packages[0]^.codeProvider^.fileChanged;
  end;

PROCEDURE reloadAllPackages;
  VAR i:longint;
  begin
    clearAllCaches;
    if length(packages)<=0 then exit;
    for i:=length(packages)-1 downto 1 do dispose(packages[i],destroy);
    setLength(packages,1);
    reloadMainPackage;
  end;

PROCEDURE reloadMainPackage;
  begin
    clearAllCaches;
    clearErrors;
    if length(packages)>0 then packages[0]^.load;
  end;

PROCEDURE clearAllPackages;
  VAR i:longint;
  begin
    clearAllCaches;
    clearErrors;
    clearSourceScanPaths;
    for i:=length(packages)-1 downto 0 do dispose(packages[i],destroy);
    setLength(packages,0);
  end;

PROCEDURE initMainPackage(CONST filename:ansistring);
  VAR fileWrapper:P_fileWrapper;
  begin
    if length(packages)>0 then exit;
      clearErrors;
    new(fileWrapper,create(filename));
    if fileWrapper^.exists then begin
      addSourceScanPath(extractFilePath(filename));
      setLength(packages,1);
      new(packages[0],create(fileWrapper));
    end else dispose(fileWrapper,destroy);
  end;

PROCEDURE initMainPackage(CONST directInputWrapper:P_codeProvider);
  begin
    if length(packages)>0 then exit;
    addSourceScanPath(ExtractFilePath(directInputWrapper^.filePath));
    clearErrors;
    setLength(packages,1);
    new(packages[0],create(directInputWrapper));
  end;

FUNCTION canResolveInMainPackage(const id: ansistring): byte;
  begin
    if (length(packages)<=0) or (packages[0]=nil) or not(packages[0]^.ready)
    then result:=0
    else result:=packages[0]^.canResolveId(id);
  end;

FUNCTION getRuleLocation(CONST id:ansistring):T_tokenLocation;
  begin
    if (length(packages)<=0) or (packages[0]=nil) or not(packages[0]^.ready)
    then result:=C_nilTokenLocation
    else result:=packages[0]^.ruleLocation(id);
  end;

FUNCTION getPackageLocation(const id: ansistring): T_tokenLocation;
  VAR i:longint;
  begin
    if (length(packages)<=0) or (packages[0]=nil) or not(packages[0]^.ready)
    then result:=C_nilTokenLocation
    else with packages[0]^ do begin
      for i:=0 to length(packageUses)-1 do if packageUses[i].id=id then begin
        result.provider:=packageUses[i].pack^.codeProvider;
        result.line:=1;
        result.column:=1;
        exit(result);
      end;
    end;
    result:=C_nilTokenLocation;
  end;

FUNCTION loadPackage(CONST packageId:ansistring; CONST tokenLocation:T_tokenLocation):P_package;
  VAR i:longint;
      newSource:P_codeProvider=nil;
  begin
    for i:=0 to length(packages)-1 do 
      if packages[i]^.codeProvider^.fileIdentifier = packageId then begin
        if packages[i]^.ready then exit(packages[i])
                              else begin
                                raiseError(el4_parsingError,'Cyclic package dependencies encountered; already ^ "'+packageId+'"',tokenLocation);
                                exit(nil);
                              end;
      end;
    
    newSource:=locateSource(packageId);
    if newSource<>nil then begin
      new(result,create(newSource));
      setLength(packages,length(packages)+1);
      packages[length(packages)-1]:=result;
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
           and (this^.next<>nil) and (this^.next^.tokType in [tt_literal,tt_identifier,tt_braceOpen,tt_listBraceOpen,tt_expBraceOpen,tt_userRulePointer,tt_intrinsicRulePointer,tt_parameterIdentifier]) then begin
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
          t^.data:=ensureLocalRuleId(t^.next^.next^.txt);
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
        if not(first^.tokType in [tt_identifier, tt_userRulePointer, tt_intrinsicRulePointer]) then begin
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
          if (p^.tokType in [tt_identifier, tt_userRulePointer, tt_intrinsicRulePointer]) and isQualified(first^.txt) then begin
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
          ensureLocalRuleId(ruleId)^.addOrReplaceSubRule(subrule);
          if not(ruleIsPrivate) then
          ensurePublicRuleId(ruleId)^.addOrSetSubRule(subrule);
          first:=nil;
        end else begin
          cascadeDisposeToken(first);
        end;
      end;
      
    FUNCTION parseCache:boolean;
      VAR ruleIdToken:P_token;
          specifier:P_token;
          cacheSize:longint;
      begin
        if not(first^.tokType in [tt_identifier,tt_intrinsicRulePointer,tt_userRulePointer,tt_parameterIdentifier]) or 
              (first^.txt<>'CACHE') or 
              (first^.next=nil) 
        then exit(false);
        ruleIdToken:=first^.next;
        if not(ruleIdToken^.tokType in [tt_identifier,tt_intrinsicRulePointer,tt_userRulePointer,tt_parameterIdentifier])
        then exit(false);
        specifier:=ruleIdToken^.next;
        if (specifier=nil) or (specifier^.tokType=tt_literal) 
                and (P_literal(specifier^.data)^.literalType=lt_int)
                and (specifier^.next=nil) then begin          
          if specifier=nil then cacheSize:=100
                           else cacheSize:=P_intLiteral(specifier^.data)^.value;
          ensureLocalRuleId (ruleIdToken^.txt)^.setCached(cacheSize);
          ensurePublicRuleId(ruleIdToken^.txt)^.setCached(cacheSize);
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
        writeExprOut(tokensToString(first));
      end;
      cascadeDisposeToken(first);
    end;

  VAR codeLines:T_stringList;
      i:longint;
      first,next,last:P_token;
      location:T_TokenLocation;
  begin
    clear;
    codeLines:=codeProvider^.fileLines;
    codeProvider^.logCheck;

    first:=nil;
    next :=nil;
    last :=nil;
    location.provider:=codeProvider;
    for i:=0 to length(codeLines)-1 do begin
      location.line:=i+1;
      location.column:=1;
      while not(isBlank(codeLines[i])) and (errorLevel<el3_evalError) do begin
        next:=firstToken(codeLines[i],location,@self);
        if (next<>nil) and (next^.tokType=tt_eol) then begin //EOL (end of input or semicolon)
          disposeToken(next);
          next:=nil;
          last:=nil;
        end;
        if next=nil then begin          
          if first<>nil then interpret(first);
          last:=nil;
          first:=nil;
        end else begin
          if first=nil then begin
            first:=next;
            last :=next;
          end else begin
            last^.next:=next;
            last      :=next;
          end;
        end;
      end;
    end;
    if first<>nil then interpret(first);    
    ready:=true;    
  end;

CONSTRUCTOR T_package.create(CONST provider:P_codeProvider);
  begin
    setLength(packageUses,0);
    codeProvider:=provider;
    publicRules.create;
    localRules.create;
  end;

PROCEDURE T_package.clear;
  VAR rule:P_rule;
  begin
    while publicRules.size>0 do begin
      rule:=publicRules.dropAny;
      setLength(rule^.subRules,0);
      dispose(rule,destroy);
    end;
    publicRules.clear;

    while localRules.size>0 do begin
      rule:=localRules.dropAny;
      dispose(rule,destroy);
    end;
    localRules.clear;

    setLength(packageUses,0);
    ready:=false;
  end;

DESTRUCTOR T_package.destroy;
  begin
    clear;
    dispose(codeProvider,destroy);
    publicRules.destroy;
    localRules.destroy;
    setLength(packageUses,0);
  end;

PROCEDURE T_package.resolveRuleId(VAR token:T_token; CONST failSilently:boolean);
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

    if ((packageId=codeProvider^.fileIdentifier) or (packageId='')) 
    and localRules.containsKey(ruleId,userRule) then begin
      token.tokType:=tt_userRulePointer;
      token.data:=userRule;
      exit;
    end;

    for i:=length(packageUses)-1 downto 0 do
    if ((packageId=packageUses[i].id) or (packageId='')) 
    and packageUses[i].pack^.publicRules.containsKey(ruleId,userRule) then begin
      token.tokType:=tt_userRulePointer;
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

FUNCTION T_package.canResolveId(CONST identifier:ansistring):byte;
  VAR i:longint;
      userRule:P_rule;
      intrinsicFuncPtr:T_intFuncCallback;
      packageId,ruleId:ansistring;
  begin
    i:=pos(C_id_qualify_character,identifier);
    if i>0 then begin
      packageId:=copy(identifier,1,i-1);
      ruleId   :=copy(identifier,i+1,length(identifier));
    end else begin
      packageId:='';
      ruleId   :=identifier;
    end;

    result:=0;

    if ((packageId=codeProvider^.fileIdentifier) or (packageId=''))
    and localRules.containsKey(ruleId,userRule) then exit(1);

    for i:=length(packageUses)-1 downto 0 do
    if ((packageId=packageUses[i].id) or (packageId=''))
    and packageUses[i].pack^.publicRules.containsKey(ruleId,userRule) then exit(2);

    if ((packageId='mnh') or (packageId=''))
    and intrinsicRuleMap.containsKey(ruleId,intrinsicFuncPtr) then exit(3);
  end;

FUNCTION T_package.ruleLocation(CONST identifier:ansistring):T_tokenLocation;
  VAR i:longint;
      userRule:P_rule;
      intrinsicFuncPtr:T_intFuncCallback;
      packageId,ruleId:ansistring;
  begin
    i:=pos(C_id_qualify_character,identifier);
    if i>0 then begin
      packageId:=copy(identifier,1,i-1);
      ruleId   :=copy(identifier,i+1,length(identifier));
    end else begin
      packageId:='';
      ruleId   :=identifier;
    end;

    result:=C_nilTokenLocation;

    if ((packageId=codeProvider^.fileIdentifier) or (packageId=''))
    and localRules.containsKey(ruleId,userRule) then exit(userRule^.getLocationOfDeclaration);

    for i:=length(packageUses)-1 downto 0 do
    if ((packageId=packageUses[i].id) or (packageId=''))
    and packageUses[i].pack^.publicRules.containsKey(ruleId,userRule) then exit(userRule^.getLocationOfDeclaration);

    if ((packageId='mnh') or (packageId=''))
    and intrinsicRuleMap.containsKey(ruleId,intrinsicFuncPtr) then result.column:=-111;
  end;

FUNCTION T_package.ensureLocalRuleId(CONST ruleId:ansistring):P_rule;
  begin
    if not(localRules.containsKey(ruleId,result)) then begin
      new(result,create(ruleId));
      localRules.put(ruleId,result);
    end;
  end;

FUNCTION T_package.ensurePublicRuleId(CONST ruleId:ansistring):P_rule;
  begin
    if not(publicRules.containsKey(ruleId,result)) then begin
      new(result,create(ruleId));
      publicRules.put(ruleId,result);
    end;
  end;

FUNCTION packagePointerToSource(CONST package:pointer):P_codeProvider;
  begin
    result:=P_package(package)^.codeProvider;
  end;

FUNCTION stringToExpression(s:ansistring; CONST location:T_tokenLocation):P_scalarLiteral;
  VAR first,last,next:P_token;
      loc:T_tokenLocation;
  begin
    loc:=location;
    first:=firstToken(s,loc,nil);
    last:=first;
    next:=nil;
    repeat
      loc:=location;
      next:=firstToken(s,loc,nil);
      if next<>nil then begin
        last^.next:=next;
        last:=next;
      end;
    until (next=nil) or (errorLevel>=el3_evalError);
    if errorLevel>=el3_evalError then begin cascadeDisposeToken(first); exit(newErrorLiteral); end;

    if first^.tokType<>tt_expBraceOpen then begin
      next:=newToken(location,'',tt_expBraceOpen);
      next^.next:=first; first:=next;
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
    if (length(packages)<=0) or (packages[0]=nil) or not(packages[0]^.ready) or (errorLevel>el0_allOkay) then exit;
    finalizeTokens;
    t:=newToken(C_nilTokenLocation,'main',tt_identifier);
    packages[0]^.resolveRuleId(t^,false);
    if t^.tokType=tt_literal then raiseError(el3_evalError,'The specified package contains no main rule.',C_nilTokenLocation)
    else begin
      parLit:=newListLiteral;
      for i:=0 to length(parameters)-1 do parLit^.append(newStringLiteral(parameters[i]),false);
      t^.next:=newToken(C_nilTokenLocation,'',tt_parList,parLit);
      reduceExpression(t,0);
    end;
    cascadeDisposeToken(t);
  end;

INITIALIZATION
  setLength(packages,0);
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
  clearAllPackages;
  finalizeTokens;
end.
