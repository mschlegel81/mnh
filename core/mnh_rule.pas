UNIT mnh_rule;
INTERFACE
USES sysutils,math,
     myGenerics, myStringUtil,
     mnh_constants,mnh_basicTypes,
     mnh_out_adapters,
     mnh_litVar, valueStore,
     mnh_funcs,
     mnh_tokens,
     mnh_contexts,
     mnh_datastores, mnh_caches, mnh_patterns, mnh_subrules;
TYPE
  T_subruleArray=array of P_subruleExpression;

  P_rule=^T_rule;

  T_rule=object(T_abstractRule)
    FUNCTION replaces(CONST param:P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep:P_token; CONST includePrivateRules:boolean; VAR context:T_threadContext):boolean; virtual; abstract;
    FUNCTION getFunctionPointer(VAR context:T_threadContext; CONST ruleTokenType:T_tokenType; CONST location:T_tokenLocation):P_expressionLiteral; virtual; abstract;
    FUNCTION getDocTxt: ansistring; virtual; abstract;
  end;

  P_ruleWithSubrules=^T_ruleWithSubrules;
  T_ruleWithSubrules=object(T_rule)
    private
      subrules:T_subruleArray;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType=rt_normal);
      DESTRUCTOR destroy; virtual;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_threadContext); virtual;
      PROCEDURE resolveIds(CONST adapters:P_adapters); virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION getCmdLineHelpText:T_arrayOfString; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION getInlineValue:P_literal;
      PROPERTY getSubrules:T_subruleArray read subrules;
      FUNCTION replaces(CONST param:P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep:P_token; CONST includePrivateRules:boolean; VAR context:T_threadContext):boolean; virtual;
      FUNCTION inspect:P_mapLiteral; virtual;
      FUNCTION getFunctionPointer(VAR context:T_threadContext; CONST ruleTokenType:T_tokenType; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      FUNCTION getDocTxt: ansistring; virtual;
  end;

  P_protectedRuleWithSubrules=^T_protectedRuleWithSubrules;
  T_protectedRuleWithSubrules=object(T_ruleWithSubrules)
    private
      rule_cs:system.TRTLCriticalSection;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType=rt_synchronized);
      DESTRUCTOR destroy; virtual;
      FUNCTION replaces(CONST param:P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep:P_token; CONST includePrivateRules:boolean; VAR context:T_threadContext):boolean; virtual;
      FUNCTION getFunctionPointer(VAR context:T_threadContext; CONST ruleTokenType:T_tokenType; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
  end;

  P_memoizedRule=^T_memoizedRule;
  T_memoizedRule=object(T_protectedRuleWithSubrules)
    private
      cache:T_cache;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation);
      DESTRUCTOR destroy; virtual;
      PROCEDURE clearCache; virtual;
      FUNCTION doPutCache(CONST param:P_listLiteral):P_literal;
      FUNCTION replaces(CONST param:P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep:P_token; CONST includePrivateRules:boolean; VAR context:T_threadContext):boolean; virtual;
  end;

  P_mutableRule=^T_mutableRule;
  T_mutableRule=object(T_rule)
    private
      rule_cs:system.TRTLCriticalSection;
      privateRule:boolean;
      called,
      valueChangedAfterDeclaration:boolean;
      namedValue:T_namedVariable;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST isPrivate:boolean; CONST ruleType:T_ruleType=rt_mutable);
      DESTRUCTOR destroy; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      PROCEDURE setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean); virtual;
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION replaces(CONST param:P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep:P_token; CONST includePrivateRules:boolean; VAR context:T_threadContext):boolean; virtual;
      FUNCTION inspect:P_mapLiteral; virtual;
      FUNCTION getFunctionPointer(VAR context:T_threadContext; CONST ruleTokenType:T_tokenType; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      FUNCTION getDocTxt: ansistring; virtual;
  end;

  P_datastoreRule=^T_datastoreRule;
  T_datastoreRule=object(T_mutableRule)
    private
      dataStoreMeta:T_datastoreMeta;
      encodeAsText:boolean;
      PROCEDURE readDataStore(VAR context:T_threadContext);
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST datastorePackage:P_objectWithPath; CONST isPrivate,usePlainTextEncoding:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal; virtual;
      PROCEDURE writeBack(VAR adapters:T_adapters);
      FUNCTION isInitialized:boolean;
      FUNCTION replaces(CONST param:P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep:P_token; CONST includePrivateRules:boolean; VAR context:T_threadContext):boolean; virtual;
  end;

IMPLEMENTATION

CONSTRUCTOR T_ruleWithSubrules.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    inherited create(ruleId,startAt,ruleTyp);
    setLength(subrules,0);
  end;

CONSTRUCTOR T_protectedRuleWithSubrules.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    initCriticalSection(rule_cs);
    inherited create(ruleId,startAt,ruleTyp);
  end;

CONSTRUCTOR T_memoizedRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation);
  begin
    inherited create(ruleId,startAt);
    cache.create(rule_cs);
  end;

CONSTRUCTOR T_mutableRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST isPrivate: boolean; CONST ruleType: T_ruleType);
  begin
    inherited create(ruleId,startAt,ruleType);
    privateRule:=isPrivate;
    namedValue.create(ruleId,newVoidLiteral,false);
    initCriticalSection(rule_cs);
  end;

CONSTRUCTOR T_datastoreRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST datastorePackage:P_objectWithPath; CONST isPrivate,usePlainTextEncoding: boolean);
  begin
    inherited create(ruleId,startAt,isPrivate,rt_datastore);
    encodeAsText:=usePlainTextEncoding;
    dataStoreMeta.create(datastorePackage^.getPath,ruleId);
  end;

DESTRUCTOR T_ruleWithSubrules.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do disposeLiteral(subrules[i]);
    setLength(subrules,0);
    inherited destroy;
  end;

DESTRUCTOR T_protectedRuleWithSubrules.destroy;
  begin
    enterCriticalSection(rule_cs);
    inherited destroy;
    leaveCriticalSection(rule_cs);
    doneCriticalSection(rule_cs);
  end;

DESTRUCTOR T_memoizedRule.destroy;
  begin
    cache.destroy;
    inherited destroy;
  end;

DESTRUCTOR T_mutableRule.destroy;
  begin
    enterCriticalSection(rule_cs);
    inherited destroy;
    namedValue.destroy;
    leaveCriticalSection(rule_cs);
    doneCriticalSection(rule_cs);
  end;

DESTRUCTOR T_datastoreRule.destroy;
  begin
    dataStoreMeta.destroy;
    inherited destroy;
  end;

PROCEDURE T_ruleWithSubrules.addOrReplaceSubRule(CONST rule: P_subruleExpression; VAR context: T_threadContext);
  VAR i,j:longint;
  begin
    if (getId=MAIN_RULE_ID) and not(rule^.hasValidMainPattern) then context.adapters^.raiseError('Invalid pattern/signature for main rule! Must accept strings.',rule^.getLocation);
    if (getRuleType=rt_customTypeCheck) and not(rule^.hasValidValidCustomTypeCheckPattern) then context.adapters^.raiseError('Invalid pattern/signature for custom type check! Must accept exactly one parameter.',rule^.getLocation);
    i:=0;
    while (i<length(subrules)) and not(rule^.hasEquivalentPattern(subrules[i])) do inc(i);
    if i>=length(subrules) then begin
      setLength(subrules,i+1);
      for j:=0 to i-1 do if subrules[j]^.hidesSubrule(rule) then context.adapters^.raiseWarning('Rule '+rule^.getId+' seems to be hidden by '+subrules[j]^.getId+' @'+ansistring(subrules[j]^.getLocation),rule^.getLocation);
    end else begin
      dispose(subrules[i],destroy);
      context.adapters^.raiseWarning('Overriding rule '+rule^.getId,rule^.getLocation);
    end;
    subrules[i]:=rule;
    if (length(subrules)>1) and (getRuleType in C_ruleTypesWithOnlyOneSubrule) then context.adapters^.raiseError('Cannot add a subrule to a '+C_ruleTypeText[getRuleType]+'rule!',rule^.getLocation);
    clearCache;
  end;

PROCEDURE T_ruleWithSubrules.resolveIds(CONST adapters: P_adapters);
  VAR s:P_subruleExpression;
  begin
    for s in subrules do s^.resolveIds(adapters);
  end;

FUNCTION T_ruleWithSubrules.hasPublicSubrule: boolean;
  VAR s:P_subruleExpression;
  begin
    for s in subrules do if s^.typ=et_normal_public then exit(true);
    result:=false;
  end;

FUNCTION T_ruleWithSubrules.getCmdLineHelpText: T_arrayOfString;
  VAR i:longint;
      txt:string;
  begin
    txt:=inherited getCmdLineHelpText[0];
    for i:=0 to length(subrules)-1 do txt:=txt+C_lineBreakChar+subrules[i]^.getCmdLineHelpText();
    result:=formatTabs(split(txt));
  end;

FUNCTION T_ruleWithSubrules.isReportable(OUT value: P_literal): boolean;
  begin
    value:=getInlineValue;
    if value=nil then exit(false);
    value^.unreference;
    result:=true;
  end;

FUNCTION T_ruleWithSubrules.getInlineValue: P_literal;
  begin
    if length(subrules)=1 then result:=subrules[0]^.getInlineValue
                          else result:=nil;
  end;

FUNCTION T_ruleWithSubrules.replaces(CONST param: P_listLiteral; CONST location: T_tokenLocation; OUT firstRep, lastRep: P_token; CONST includePrivateRules: boolean; VAR context: T_threadContext): boolean;
  VAR uncurrying:boolean;
      sub:P_subruleExpression;
  begin
    result:=false;
    for uncurrying:=false to true do
    for sub in subrules do if (includePrivateRules or (sub^.typ=et_normal_public)) and sub^.replaces(param,location,firstRep,lastRep,context,uncurrying) then begin
      exit(true);
    end;
    if getRuleType=rt_customTypeCheck then begin
      firstRep:=context.recycler.newToken(getLocation,'',tt_literal,newBoolLiteral(false));
      lastRep:=firstRep;
      exit(true);
    end;
  end;

FUNCTION T_protectedRuleWithSubrules.replaces(CONST param: P_listLiteral; CONST location: T_tokenLocation; OUT firstRep, lastRep: P_token; CONST includePrivateRules: boolean; VAR context: T_threadContext): boolean;
  begin
    result:=false;
    if context.callDepth>=STACK_DEPTH_LIMIT then context.adapters^.raiseSystemError('Stack depth limit exceeded calling '+getId+'.',getLocation)
    else if inherited replaces(param,location,firstRep,lastRep,includePrivateRules,context) then begin
      system.enterCriticalSection(rule_cs);
      result:=true;
      context.reduceExpression(firstRep);
      if firstRep<>nil then lastRep:=firstRep^.last else begin
        lastRep:=nil;
        result:=false;
      end;
      system.leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_memoizedRule.replaces(CONST param: P_listLiteral; CONST location: T_tokenLocation; OUT firstRep, lastRep: P_token; CONST includePrivateRules: boolean; VAR context: T_threadContext): boolean;
{$MACRO ON}
{$define CLEAN_EXIT:=
if param=nil then disposeLiteral(useParam);
system.leaveCriticalSection(rule_cs);
exit}
  VAR lit:P_literal;
      useParam:P_listLiteral;
      uncurrying:boolean;
      sub:P_subruleExpression;
  PROCEDURE wrapResultInPutCacheRule;
    VAR newFirst,t:P_token;
    begin
      newFirst      :=context.recycler.newToken(firstRep^.location, getId+'.put.cache',tt_rulePutCacheValue,@self);
      newFirst^.next:=context.recycler.newToken(firstRep^.location, '', tt_parList_constructor,newListLiteral(1)^.append(useParam,true)); t:=newFirst^.next;
      t       ^.next:=firstRep;
      firstRep:=newFirst;
      lastRep^.next:=context.recycler.newToken(firstRep^.location, '', tt_braceClose);
      lastRep:=lastRep^.next;
    end;

  begin
    result:=false;
    if param=nil then useParam:=newListLiteral
                 else useParam:=param;
    enterCriticalSection(rule_cs);
    lit:=cache.get(useParam);
    if lit<>nil then begin
      lit^.rereference;
      firstRep:=context.recycler.newToken(getLocation,'',tt_literal,lit);
      lastRep:=firstRep;
      CLEAN_EXIT(true);
    end else for uncurrying:=false to true do
             for sub in subrules do if (includePrivateRules or (sub^.typ=et_normal_public)) and sub^.replaces(useParam,location,firstRep,lastRep,context,uncurrying) then begin
      if (context.callDepth>=STACK_DEPTH_LIMIT) then begin wrapResultInPutCacheRule; CLEAN_EXIT(true); end;
      if (context.adapters^.noErrors) then context.reduceExpression(firstRep);
      if (context.adapters^.noErrors) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
        lit:=firstRep^.data;
        cache.put(useParam,lit);
        lastRep:=firstRep;
      end else begin
        context.recycler.cascadeDisposeToken(firstRep);
        firstRep:=context.recycler.newToken(getLocation,'',tt_literal,newVoidLiteral);
        lastRep:=firstRep;
      end;
      CLEAN_EXIT(true);
    end;
    CLEAN_EXIT(false);
  end;

FUNCTION T_mutableRule.replaces(CONST param: P_listLiteral; CONST location: T_tokenLocation; OUT firstRep, lastRep: P_token; CONST includePrivateRules: boolean; VAR context: T_threadContext): boolean;
  begin
    result:=(includePrivateRules or not(privateRule)) and ((param=nil) or (param^.size=0));
    if result then begin
      system.enterCriticalSection(rule_cs);
      firstRep:=context.recycler.newToken(getLocation,'',tt_literal,namedValue.getValue);
      system.leaveCriticalSection(rule_cs);
      lastRep:=firstRep;
      called:=true;
    end;
  end;

FUNCTION T_datastoreRule.replaces(CONST param: P_listLiteral; CONST location: T_tokenLocation; OUT firstRep, lastRep: P_token; CONST includePrivateRules: boolean; VAR context: T_threadContext): boolean;
  begin
    result:=(includePrivateRules or not(privateRule)) and ((param=nil) or (param^.size=0));
    if result then begin
      system.enterCriticalSection(rule_cs);
      readDataStore(context);
      firstRep:=context.recycler.newToken(getLocation,'',tt_literal,namedValue.getValue);
      system.leaveCriticalSection(rule_cs);
      lastRep:=firstRep;
      called:=true;
    end;
  end;

FUNCTION T_ruleWithSubrules.inspect: P_mapLiteral;
  FUNCTION subrulesList:P_listLiteral;
    VAR sub:P_subruleExpression;
    begin
      result:=newListLiteral(length(subrules));
      for sub in subrules do result^.append(sub^.inspect,false);
    end;

  begin
    result:=newMapLiteral^
      .put('type'    ,C_ruleTypeText[getRuleType])^
      .put('location',getLocation       )^
      .put('subrules',subrulesList,false);
  end;

FUNCTION T_mutableRule.inspect: P_mapLiteral;
  FUNCTION privateOrPublic:string;
    begin
      if privateRule then result:=C_tokenInfo[tt_modifier_private].defaultId
                     else result:=PUBLIC_TEXT;
    end;

  FUNCTION subrulesList:P_listLiteral;
    VAR value:P_literal;
    begin
      value:=namedValue.getValue;
      result:=newListLiteral(1);
      result^.append(newMapLiteral^
        .put('pattern' ,'()'            )^
        .put('location',getLocation     )^
        .put('type'    ,privateOrPublic )^
        .put('comment' ,''              )^
        .put('body'    ,value^.toString ),false);
      value^.unreference;
    end;

  begin
    result:=newMapLiteral^
      .put('type'    ,C_ruleTypeText[getRuleType])^
      .put('location',getLocation        )^
      .put('subrules',subrulesList,false      );
  end;

FUNCTION T_ruleWithSubrules.getFunctionPointer(VAR context: T_threadContext; CONST ruleTokenType: T_tokenType; CONST location: T_tokenLocation): P_expressionLiteral;
  VAR minPatternLength:longint=maxLongint;
      maxPatternLength:longint=0;
      sub:P_subruleExpression;
      tempToken:P_token=nil;
  begin
    if (getRuleType=rt_normal) and (length(subrules)=1) then exit(P_expressionLiteral(subrules[0]^.rereferenced));
    for sub in subrules do begin
      minPatternLength:=min(minPatternLength,sub^.arity);
      maxPatternLength:=max(maxPatternLength,sub^.arity);
      if sub^.isVariadic then maxPatternLength:=maxLongint;
    end;
    tempToken      :=context.recycler.newToken(location,getId,ruleTokenType,@self);
    tempToken^.next:=getParametersForPseudoFuncPtr(minPatternLength,maxPatternLength>minPatternLength,location,context);
    new(P_inlineExpression(result),createFromInline(tempToken,context));
  end;

FUNCTION T_protectedRuleWithSubrules.getFunctionPointer(VAR context: T_threadContext; CONST ruleTokenType: T_tokenType; CONST location: T_tokenLocation): P_expressionLiteral;
  VAR minPatternLength:longint=maxLongint;
      maxPatternLength:longint=0;
      sub:P_subruleExpression;
      tempToken:P_token=nil;
  begin
    for sub in subrules do begin
      minPatternLength:=min(minPatternLength,sub^.arity);
      maxPatternLength:=max(maxPatternLength,sub^.arity);
      if sub^.isVariadic then maxPatternLength:=maxLongint;
    end;
    tempToken            :=context.recycler.newToken(location,getId,ruleTokenType,@self);
    tempToken^.next      :=getParametersForPseudoFuncPtr(minPatternLength,maxPatternLength>minPatternLength,location,context);
    new(P_inlineExpression(result),createFromInline(tempToken,context));
  end;

FUNCTION T_mutableRule.getFunctionPointer(VAR context: T_threadContext; CONST ruleTokenType: T_tokenType; CONST location: T_tokenLocation): P_expressionLiteral;
  VAR tempToken:P_token=nil;
  begin
    tempToken            :=context.recycler.newToken(location,getId,ruleTokenType,@self);
    tempToken^.next      :=context.recycler.newToken(location,'',tt_braceOpen );
    tempToken^.next^.next:=context.recycler.newToken(location,'',tt_braceClose);
    new(P_inlineExpression(result),createFromInline(tempToken,context));
  end;

FUNCTION T_ruleWithSubrules.getDocTxt: ansistring;
  VAR s:P_subruleExpression;
  begin
    result:='';
    for s in subrules do result:=result+C_lineBreakChar+s^.getDocTxt();
    result:=join(formatTabs(split(result)),LineEnding);
    result:=ECHO_MARKER+C_ruleTypeText[getRuleType]+'rule '+getId+C_lineBreakChar+
            'in '+getLocation.package^.getPath+result;
  end;

FUNCTION T_mutableRule.getDocTxt: ansistring;
  begin
    result:=ECHO_MARKER+C_ruleTypeText[getRuleType]+'rule '+getId+C_lineBreakChar+
            'in '+getLocation.package^.getPath+C_lineBreakChar+
            'declared '+ansistring(getLocation);
  end;

FUNCTION T_mutableRule.hasPublicSubrule: boolean;
  begin
    result:=not(privateRule);
  end;

PROCEDURE T_mutableRule.setMutableValue(CONST value: P_literal; CONST onDeclaration: boolean);
  begin
    system.enterCriticalSection(rule_cs);
    namedValue.setValue(value);
    if not(onDeclaration) then begin
      valueChangedAfterDeclaration:=true;
      called:=true;
    end;
    system.leaveCriticalSection(rule_cs);
  end;

FUNCTION T_mutableRule.isReportable(OUT value: P_literal): boolean;
  begin
    value:=namedValue.getValue;
    if value=nil then exit(false);
    value^.unreference;
    result:=true;
  end;

PROCEDURE T_datastoreRule.readDataStore(VAR context:T_threadContext);
  VAR lit:P_literal;
  begin
    if not(called) or (not(valueChangedAfterDeclaration) and dataStoreMeta.fileChangedSinceRead) then begin
      {$ifdef debugMode}
      writeln(stdErr,'        DEBUG: reading datastore for rule ',getId,' ',string(getLocation),
                     '; called: ',called,
                     '; valueChangedAfterDeclaration: ',valueChangedAfterDeclaration,
                     '; fileChanged: ',dataStoreMeta.fileChangedSinceRead);
      {$endif}
      lit:=dataStoreMeta.readValue(getLocation,context);
      if lit<>nil then begin
        namedValue.setValue(lit);
        lit^.unreference;
      end;
    end;
  end;

FUNCTION T_mutableRule.mutateInline(CONST mutation: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation; VAR context: T_threadContext): P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    result:=namedValue.mutate(mutation,RHS,location,context.adapters^,@context);
    valueChangedAfterDeclaration:=true;
    called:=true;
    system.leaveCriticalSection(rule_cs);
  end;

FUNCTION T_datastoreRule.mutateInline(CONST mutation: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation; VAR context: T_threadContext): P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    if not(called) and not(valueChangedAfterDeclaration) then readDataStore(context);
    result:=inherited mutateInline(mutation,RHS,location,context);
    system.leaveCriticalSection(rule_cs);
  end;

PROCEDURE T_datastoreRule.writeBack(VAR adapters: T_adapters);
  VAR L:P_literal;
  begin
    if (adapters.noErrors) and valueChangedAfterDeclaration then begin
      L:=namedValue.getValue;
      dataStoreMeta.writeValue(L,getLocation,@adapters,encodeAsText);
      disposeLiteral(L);
    end;
  end;

FUNCTION T_datastoreRule.isInitialized:boolean;
  begin
    result:=called or valueChangedAfterDeclaration;
  end;

PROCEDURE T_memoizedRule.clearCache;
  begin
    enterCriticalSection(rule_cs);
    cache.clear;
    leaveCriticalSection(rule_cs);
  end;

FUNCTION T_memoizedRule.doPutCache(CONST param: P_listLiteral): P_literal;
  begin
    enterCriticalSection(rule_cs);
    cache.put(P_listLiteral(param^[0]),
                            param^[1] );
    result:=param^[1]^.rereferenced;
    leaveCriticalSection(rule_cs);
  end;

FUNCTION customTypeCheckToExpression(CONST rule:pointer):P_expressionLiteral;
  begin
    result:=P_ruleWithSubrules(rule)^.subrules[0];
  end;

INITIALIZATION
  customTypeCheckToExpressionCallback:=@customTypeCheckToExpression;
end.
