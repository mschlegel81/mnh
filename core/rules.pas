UNIT rules;
INTERFACE
USES sysutils,math,
     myGenerics, myStringUtil,
     mnh_constants,basicTypes,
     out_adapters,
     litVar, valueStore,
     funcs,
     tokens,
     tokenArray,
     mnh_messages,
     contexts,
     patterns,
     recyclers,
     datastores, caches, subrules,operators;
TYPE
  T_subruleArray=array of P_subruleExpression;

  P_rule=^T_rule;
  T_ruleList=array of P_rule;
  T_rule=object(T_abstractRule)
    hiddenRule:P_intFuncCallback;
    allowCurrying:boolean;
    FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual; abstract;
    FUNCTION inspect({$WARN 5024 OFF}CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
    {$ifdef fullVersion}
    PROCEDURE checkParameters(VAR context:T_context); virtual;
    {$endif}
    FUNCTION isFallbackPossible(CONST callLocation:T_tokenLocation; CONST givenParameters:P_listLiteral; OUT firstRep,lastRep:P_token; VAR context:T_context; VAR recycler:T_recycler):boolean;
    FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST p1,p2:P_literal;       VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal; virtual;
    FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST parList:P_listLiteral; VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal; virtual;
    FUNCTION commonArity:longint; virtual;
  end;

  P_ruleWithSubrules=^T_ruleWithSubrules;
  T_ruleWithSubrules=object(T_rule)
    private
      subrules:T_subruleArray;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType=rt_normal);
      DESTRUCTOR destroy; virtual;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context); virtual;
      PROCEDURE resolveIds(CONST adapters:P_messages); virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION getCmdLineHelpText:T_arrayOfString; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION getInlineValue:P_literal; virtual;
      PROPERTY getSubrules:T_subruleArray read subrules;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
      FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      {$ifdef fullVersion}
      FUNCTION getDocTxt: ansistring; virtual;
      PROCEDURE checkParameters(VAR context:T_context); virtual;
      {$endif}
      {Returns the common arity of all subrules or -1 if arity differs or any subrule has optional parameters}
      FUNCTION commonArity:longint; virtual;
  end;

  P_operatorDelegatorRule=^T_operatorDelegatorRule;
  T_operatorDelegatorRule=object(T_ruleWithSubrules)
    private
      intOperator:T_tokenType;
      isUnary:boolean;
      localRule:P_ruleWithSubrules;
      imported:array of P_ruleWithSubrules;
    public
      CONSTRUCTOR create(CONST op:T_tokenType; CONST declaredInPackage:P_abstractPackage);
      DESTRUCTOR destroy; virtual;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context); virtual;
      PROCEDURE resolveIds(CONST adapters:P_messages); virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
      FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      {$ifdef fullVersion}
      FUNCTION getDocTxt: ansistring; virtual;
      PROCEDURE checkParameters(VAR context:T_context); virtual;
      {$endif}
      {Returns the common arity of all subrules or -1 if arity differs or any subrule has optional parameters}
      FUNCTION commonArity:longint; virtual;

      PROCEDURE clearImported;
      PROCEDURE mergeImported(CONST rulesFromOtherPackage:P_abstractRule);
      PROPERTY getLocalRule:P_ruleWithSubrules read localRule;
  end;

  P_protectedRuleWithSubrules=^T_protectedRuleWithSubrules;
  T_protectedRuleWithSubrules=object(T_ruleWithSubrules)
    private
      rule_cs:system.TRTLCriticalSection;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType=rt_synchronized);
      DESTRUCTOR destroy; virtual;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
  end;

  P_memoizedRule=^T_memoizedRule;
  T_memoizedRule=object(T_protectedRuleWithSubrules)
    private
      cache:T_cache;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleType:T_ruleType=rt_memoized);
      DESTRUCTOR destroy; virtual;
      PROCEDURE clearCache; virtual;
      FUNCTION doPutCache(CONST param:P_listLiteral):P_literal;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION commonArity:longint; virtual;
  end;

  P_typeCastRule=^T_typeCastRule;
  T_typeCastRule=object(T_ruleWithSubrules)
    private
      typedef:P_typedef;
      related:P_ruleWithSubrules;
    public
      CONSTRUCTOR create(CONST def:P_typedef; CONST relatedCheckRule:P_ruleWithSubrules);
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context); virtual;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION getRootId:T_idString; virtual;
      {$ifdef fullVersion}
      PROCEDURE setIdResolved; virtual;
      {$endif}
      FUNCTION inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
      FUNCTION commonArity:longint; virtual;
  end;

  P_typeCheckRule=^T_typeCheckRule;
  T_typeCheckRule=object(T_ruleWithSubrules)
    private
      typedef:P_typedef;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ducktyping:boolean);
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context); virtual;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION getFirstParameterTypeWhitelist:T_literalTypeSet; virtual;
      FUNCTION getRootId:T_idString; virtual;
      FUNCTION getTypedef:P_typedef; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION castRuleIsValid:boolean;
      DESTRUCTOR destroy; virtual;
      FUNCTION commonArity:longint; virtual;
  end;

  P_mutableRule=^T_mutableRule;
  T_mutableRule=object(T_rule)
    private
      rule_cs:system.TRTLCriticalSection;
      privateRule:boolean;
      called,
      valueChangedAfterDeclaration:boolean;
      namedValue:T_namedVariable;
      meta:T_ruleMetaData;
    public
      PROPERTY metaData:T_ruleMetaData read meta;

      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; VAR meta_:T_ruleMetaData; CONST isPrivate:boolean; CONST ruleType:T_ruleType=rt_mutable);
      DESTRUCTOR destroy; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      PROCEDURE setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean); virtual;
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
      FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      {$ifdef fullVersion}
      FUNCTION getDocTxt: ansistring; virtual;
      {$endif}
      FUNCTION getValue(VAR context:T_context; VAR recycler:T_recycler):P_literal; virtual;
      FUNCTION commonArity:longint; virtual;
  end;

  P_datastoreRule=^T_datastoreRule;
  T_datastoreRule=object(T_mutableRule)
    private
      dataStoreMeta:T_datastoreMeta;
      encodeAsText:boolean;
      PROCEDURE readDataStore(VAR context:T_context; VAR recycler:T_recycler);
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST datastorePackage:P_objectWithPath; VAR meta_:T_ruleMetaData; CONST isPrivate,usePlainTextEncoding:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal; virtual;
      PROCEDURE writeBack(CONST adapters:P_messages);
      PROCEDURE memoryCleanup;
      FUNCTION isInitialized:boolean;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION getValue(VAR context:T_context; VAR recycler:T_recycler):P_literal; virtual;
  end;

FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_context):P_expressionLiteral;
IMPLEMENTATION
USES mySys
     {$ifdef fullVersion},debuggingVar{$endif};
FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_context):P_expressionLiteral;
  begin
    if      tok^.tokType in C_operators   then result:=getIntrinsicRuleAsExpression(intFuncForOperator[tok^.tokType])
    else if tok^.tokType=tt_intrinsicRule then result:=getIntrinsicRuleAsExpression(                   tok^.data    )
    else begin
      result:=nil;
      raise Exception.create('Invalid argument for createPrimitiveAggregatorLiteral('+safeTokenToString(tok)+')');
    end;
  end;

CONSTRUCTOR T_operatorDelegatorRule.create(CONST op:T_tokenType; CONST declaredInPackage:P_abstractPackage);
  begin
    inherited create(operatorName[op],packageTokenLocation(declaredInPackage),rt_delegate);
    intOperator:=op;
    localRule:=nil;
    setLength(imported,0);
    isUnary:=op in unaryOperators;
  end;

DESTRUCTOR T_operatorDelegatorRule.destroy;
  begin
    if localRule<>nil then dispose(localRule,destroy);
    inherited destroy;
  end;

PROCEDURE T_operatorDelegatorRule.clearImported;
  begin
    setLength(imported,0);
  end;

PROCEDURE T_operatorDelegatorRule.mergeImported(CONST rulesFromOtherPackage:P_abstractRule);
  VAR operatorToAdd:P_ruleWithSubrules;
      r:P_ruleWithSubrules;
  begin
    if (rulesFromOtherPackage=nil) or (rulesFromOtherPackage^.getRuleType<>rt_delegate) then exit;
    operatorToAdd:=P_operatorDelegatorRule(rulesFromOtherPackage)^.localRule;
    if operatorToAdd=nil then exit;
    for r in imported do if r=operatorToAdd then exit;
    setLength(imported,length(imported)+1);
    imported[length(imported)-1]:=operatorToAdd;
  end;

FUNCTION T_operatorDelegatorRule.isReportable(OUT value: P_literal): boolean;
  begin
    value:=nil;
    result:=false;
  end;

FUNCTION T_rule.isFallbackPossible(CONST callLocation: T_tokenLocation;
  CONST givenParameters: P_listLiteral; OUT firstRep, lastRep: P_token;
  VAR context: T_context; VAR recycler: T_recycler): boolean;
  VAR tempToken:P_token;
      tempInline:P_inlineExpression;
      parHead,parTail:P_listLiteral;
      tempLiteral:P_literal;
  begin
    if hiddenRule<>nil then begin
      inc(context.callDepth);
      tempLiteral:=hiddenRule(givenParameters,callLocation,context,recycler);
      dec(context.callDepth);
      if tempLiteral<>nil then begin
        firstRep:=recycler.newToken(callLocation,'',tt_literal,tempLiteral);
        lastRep:=firstRep;
        exit(true);
      end;
    end;
    if not(allowCurrying) or (givenParameters=nil) or (commonArity<0) then exit(false);
    result:=false;
    if (givenParameters^.size<commonArity) then begin
      //CURRY
      //  rule : f(x,y)->...
      //  input: f(x)
      //  out  : {f(x,$y)}
      tempToken      :=recycler.newToken(callLocation,getId,tt_userRule,@self);
      tempToken^.next:=getParametersForUncurrying(givenParameters,commonArity,callLocation,context,recycler);
      new(tempInline,createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
      firstRep:=recycler.newToken(callLocation,'',tt_literal,tempInline);
      lastRep:=firstRep;
      result:=true;
    end else if (givenParameters^.size>commonArity) then begin
      //UNCURRY
      //  rule : f(x)->...
      //  input: f(x,y)
      //  out  : f(x)(y)
      parHead:=givenParameters^.head(commonArity);
      if replaces(callLocation,parHead,firstRep,lastRep,@context,recycler) then begin
        parTail:=givenParameters^.tail(commonArity);
        tempToken:=recycler.newToken(firstRep^.location,'',tt_braceOpen);
        tempToken^.next:=firstRep;
        firstRep:=tempToken;

        lastRep^.next:=recycler.newToken(firstRep^.location,'',tt_braceClose);
        lastRep:=lastRep^.next;
        lastRep^.next:=recycler.newToken(firstRep^.location,'',tt_parList,parTail);
        lastRep:=lastRep^.next;
        result:=true;
      end;
      disposeLiteral(parHead);
    end;
  end;

FUNCTION T_rule.evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST p1,p2:P_literal; VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal;
  VAR parList:P_listLiteral;
      firstRep,lastRep:P_token;
  begin
    if P_context(context)^.callDepth>=STACK_DEPTH_LIMIT then begin
      P_context(context)^.raiseError('Stack overflow in rule '+getId+' ('+string(getLocation)+')',callLocation,mt_el4_systemError);
      exit(nil);
    end;
    inc(P_context(context)^.callDepth);
    parList:=P_listLiteral(newListLiteral(2)^.append(p1,true)^.append(p2,true));
    if replaces(callLocation,parList,firstRep,lastRep,context,recycler,false)
    then result:=P_context(context)^.reduceToLiteral(firstRep,recycler).literal
    else result:=nil;
    dec(P_context(context)^.callDepth);
    disposeLiteral(parList);
  end;

FUNCTION T_rule.evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST parList:P_listLiteral; VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal;
  VAR firstRep,lastRep:P_token;
  begin
    if P_context(context)^.callDepth>=STACK_DEPTH_LIMIT then begin
      P_context(context)^.raiseError('Stack overflow in rule '+getId+' ('+string(getLocation)+')',callLocation,mt_el4_systemError);
      exit(nil);
    end;
    inc(P_context(context)^.callDepth);
    if replaces(callLocation,parList,firstRep,lastRep,context,recycler,false)
    then result:=P_context(context)^.reduceToLiteral(firstRep,recycler).literal
    else result:=nil;
    dec(P_context(context)^.callDepth);
  end;

FUNCTION T_rule.commonArity: longint;
  begin
    result:=-1;
  end;

FUNCTION T_operatorDelegatorRule.commonArity: longint;
  begin
    if isUnary then result:=1 else result:=2;
  end;

FUNCTION T_ruleWithSubrules.commonArity:longint;
  VAR s:P_subruleExpression;
  begin
    if length(subrules)=0 then exit(-1);
    result:=subrules[0]^.getPattern.arity;
    for s in subrules do if (s^.getPattern.arity<>result) or (s^.getPattern.isVariadic) then exit(-1);
  end;

FUNCTION T_memoizedRule.commonArity: longint;
  begin
    result:=0;
  end;

FUNCTION T_typeCheckRule.commonArity: longint;
  begin
    result:=1;
  end;

FUNCTION T_mutableRule.commonArity: longint;
  begin
    result:=0;
  end;

FUNCTION T_typeCastRule.commonArity: longint;
  begin
    result:=1;
  end;

{$ifdef fullVersion}
PROCEDURE T_rule.checkParameters(VAR context:T_context);
  begin end;

PROCEDURE T_ruleWithSubrules.checkParameters(VAR context:T_context);
  VAR s:P_subruleExpression;
  begin
    if length(subrules)=1 then
    for s in subrules do s^.checkParameters(context);
  end;

PROCEDURE T_operatorDelegatorRule.checkParameters(VAR context: T_context);
  VAR r:P_rule;
  begin
    if localRule<>nil then localRule^.checkParameters(context);
    for r in imported do           r^.checkParameters(context);
  end;
{$endif}

CONSTRUCTOR T_ruleWithSubrules.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    inherited create(ruleId,startAt,ruleTyp);
    hiddenRule:=nil;
    allowCurrying:=false;
    setLength(subrules,0);
  end;

CONSTRUCTOR T_protectedRuleWithSubrules.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    initCriticalSection(rule_cs);
    inherited create(ruleId,startAt,ruleTyp);
  end;

CONSTRUCTOR T_memoizedRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleType:T_ruleType=rt_memoized);
  begin
    inherited create(ruleId,startAt,ruleType);
    cache.create(rule_cs);
  end;

CONSTRUCTOR T_typeCastRule.create(CONST def:P_typedef; CONST relatedCheckRule:P_ruleWithSubrules);
  begin
    inherited create('to'+def^.getName,relatedCheckRule^.getLocation,rt_customTypeCast);
    typedef:=def;
    allowCurrying:=false;
    related:=relatedCheckRule;
    {$ifdef fullVersion}
    if def^.isDucktyping then setIdResolved;
    {$endif}
  end;

CONSTRUCTOR T_typeCheckRule.create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ducktyping:boolean);
  begin
    if ducktyping
    then inherited create(ruleId,startAt,rt_duckTypeCheck)
    else inherited create(ruleId,startAt,rt_customTypeCheck);
    typedef:=nil;
    allowCurrying:=false;
  end;

CONSTRUCTOR T_mutableRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; VAR meta_:T_ruleMetaData; CONST isPrivate: boolean; CONST ruleType:T_ruleType=rt_mutable);
  begin
    inherited create(ruleId,startAt,ruleType);
    hiddenRule:=nil;
    allowCurrying:=false;
    meta:=meta_;
    privateRule:=isPrivate;
    namedValue.create(ruleId,newVoidLiteral,false);
    initCriticalSection(rule_cs);
  end;

CONSTRUCTOR T_datastoreRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST datastorePackage:P_objectWithPath; VAR meta_:T_ruleMetaData; CONST isPrivate,usePlainTextEncoding: boolean);
  begin
    inherited create(ruleId,startAt,meta_,isPrivate,rt_datastore);
    encodeAsText:=usePlainTextEncoding;
    dataStoreMeta.create(datastorePackage^.getPath,ruleId);
    memoryCleaner.registerObjectForCleanup(@memoryCleanup);
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
    meta.destroy;
    inherited destroy;
    namedValue.destroy;
    leaveCriticalSection(rule_cs);
    doneCriticalSection(rule_cs);
  end;

DESTRUCTOR T_datastoreRule.destroy;
  begin
    dataStoreMeta.destroy;
    memoryCleaner.unregisterObjectForCleanup(@memoryCleanup);
    inherited destroy;
  end;

FUNCTION T_typeCastRule .getRootId:T_idString; begin result:=typedef^.getName; end;
FUNCTION T_typeCheckRule.getRootId:T_idString; begin result:=typedef^.getName; end;

PROCEDURE T_ruleWithSubrules.addOrReplaceSubRule(CONST rule: P_subruleExpression; VAR context: T_context);
  VAR i,j:longint;
  begin
    if (getId=MAIN_RULE_ID) and not(rule^.hasValidMainPattern) then context.messages^.raiseSimpleError('Invalid pattern/signature for main rule! Must accept strings.',rule^.getLocation);
    if (getRuleType=rt_customOperator) then begin
      if isUnaryOperatorId(getId) then begin
        if not(rule^.canApplyToNumberOfParameters(1)) then context.messages^.raiseSimpleError('Overloaded operator must accept one parameter',rule^.getLocation);
      end else begin
        if not(rule^.canApplyToNumberOfParameters(2)) then context.messages^.raiseSimpleError('Overloaded operator must accept two parameter',rule^.getLocation);
      end;
      if not(rule^.getPattern.usesStrictCustomTyping) then context.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Overloading operators based on ducktype is discouraged! Use explicit types instead.');
    end;
    i:=0;
    while (i<length(subrules)) and not(rule^.hasEquivalentPattern(subrules[i])) do inc(i);
    if i>=length(subrules) then begin
      setLength(subrules,i+1);
      for j:=0 to i-1 do if subrules[j]^.hidesSubrule(rule) then context.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Rule '+rule^.getId+' seems to be hidden by '+subrules[j]^.getId+' @'+ansistring(subrules[j]^.getLocation));
    end else begin
      disposeLiteral(subrules[i]);
      if not(rule^.metaData.hasAttribute(OVERRIDE_ATTRIBUTE))
      then context.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Overriding rule '+rule^.getId+'; you can suppress this warning with '+ATTRIBUTE_PREFIX+OVERRIDE_ATTRIBUTE);
    end;
    subrules[i]:=rule;
    if (length(subrules)>1) and (getRuleType in C_ruleTypesWithOnlyOneSubrule) then context.messages^.raiseSimpleError('Cannot add a subrule to a '+C_ruleTypeText[getRuleType]+'rule!',rule^.getLocation);
    {$ifdef fullVersion}
    if rule^.metaData.hasAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE) or
       rule^.metaData.hasAttribute(EXECUTE_AFTER_ATTRIBUTE) then setIdResolved;
    {$endif}
    clearCache;
  end;

PROCEDURE T_operatorDelegatorRule.addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context);
  begin
    if localRule=nil then begin
      new(localRule,create(getId,rule^.getLocation));
      declarationStart:=rule^.getLocation;
    end;
    localRule^.addOrReplaceSubRule(rule,context);
  end;

PROCEDURE T_typeCastRule.addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context);
  begin
    inherited addOrReplaceSubRule(rule,context);
    if not(rule^.metaData.hasAttribute(OVERRIDE_ATTRIBUTE)) then context.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Overloading implicit typecast rule');
  end;

PROCEDURE T_typeCheckRule.addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context);
  VAR rulePattern:T_patternElement;
      inlineValue:P_literal;
      alwaysTrue:boolean=false;
  begin
    if not(rule^.hasValidValidCustomTypeCheckPattern(getRuleType=rt_duckTypeCheck)) then begin
     if (getRuleType=rt_customTypeCheck)
     then context.messages^.raiseSimpleError('Invalid pattern/signature for custom type check! Must accept exactly one List, Set, Map or Expression parameter.',rule^.getLocation)
     else context.messages^.raiseSimpleError('Invalid pattern/signature for custom type check! Must accept exactly one parameter.',rule^.getLocation);
     exit;
    end;
    if length(subrules)>0 then begin
      context.messages^.raiseSimpleError('Type definitions must have only one subrule and may not be overridden',rule^.getLocation);
      exit;
    end;
    setLength(subrules,1);
    subrules[0]:=rule;
    {$ifdef fullVersion}
    if rule^.metaData.hasAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE) or
       rule^.metaData.hasAttribute(EXECUTE_AFTER_ATTRIBUTE) then setIdResolved;
    {$endif}
    if typedef=nil then begin
      rulePattern:=rule^.getPattern.getFirst;
      if rulePattern.isTypeCheckOnly then begin
        inlineValue:=rule^.getInlineValue;
        if inlineValue<>nil then begin
          alwaysTrue:=boolLit[true].equals(inlineValue);
          disposeLiteral(inlineValue);
        end;
      end else alwaysTrue:=false;
      new(typedef,create(getId,
                         rulePattern.getBuiltinTypeCheck,
                         rulePattern.getBuiltinCheckParameter,
                         rulePattern.getCustomTypeCheck,
                         P_expressionLiteral(rule^.rereferenced),
                         getRuleType=rt_duckTypeCheck,
                         alwaysTrue));
    end;
  end;

FUNCTION T_typeCheckRule.castRuleIsValid:boolean;
  begin
    result:=(getRuleType=rt_customTypeCheck) or subrules[0]^.hasValidValidCustomTypeCheckPattern(false);
  end;

PROCEDURE T_operatorDelegatorRule.resolveIds(CONST adapters:P_messages);
  begin
    if localRule<>nil then localRule^.resolveIds(adapters);
  end;

PROCEDURE T_ruleWithSubrules.resolveIds(CONST adapters: P_messages);
  VAR s:P_subruleExpression;
  begin
    for s in subrules do s^.resolveIds(adapters);
  end;

FUNCTION T_ruleWithSubrules.hasPublicSubrule: boolean;
  VAR s:P_subruleExpression;
  begin
    for s in subrules do if s^.isPublic then exit(true);
    result:=false;
  end;

FUNCTION T_operatorDelegatorRule.hasPublicSubrule: boolean;
  VAR r:P_rule;
  begin
    if (localRule<>nil) and localRule^.hasPublicSubrule then exit(true);
    for r in imported do if r^.hasPublicSubrule then exit(true);
    result:=false;
  end;

FUNCTION T_typeCastRule .hasPublicSubrule:boolean; begin result:=true; end;
FUNCTION T_typeCheckRule.hasPublicSubrule:boolean; begin result:=true; end;

{$ifdef fullVersion}
PROCEDURE T_typeCastRule.setIdResolved;
  begin
    inherited setIdResolved;
    related^.setIdResolved;
  end;
{$endif}

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

FUNCTION T_operatorDelegatorRule.replaces(CONST callLocation: T_tokenLocation; CONST param: P_listLiteral; OUT firstRep, lastRep: P_token; CONST context: P_abstractContext; VAR recycler: T_recycler; CONST calledFromDelegator: boolean): boolean;
  VAR r:P_rule;
  begin
    if (localRule<>nil) and localRule^.replaces(callLocation,param,firstRep,lastRep,context,recycler,true) then exit(true);
    for r in imported do if         r^.replaces(callLocation,param,firstRep,lastRep,context,recycler,true) then exit(true);
    result:=false;
  end;

FUNCTION T_ruleWithSubrules.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  VAR sub:P_subruleExpression;
  begin
    result:=false;
    for sub in subrules do if ((sub^.isPublic) or (sub^.getLocation.package=callLocation.package)) and sub^.replaces(param,callLocation,firstRep,lastRep,P_context(context)^,recycler) then exit(true);
    if (getId<>MAIN_RULE_ID) and not(calledFromDelegator)
    then result:=isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler);
  end;

FUNCTION T_protectedRuleWithSubrules.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  begin
    result:=false;
    if P_context(context)^.callDepth>=STACK_DEPTH_LIMIT then P_context(context)^.raiseError('Stack depth limit exceeded calling '+getId+'.',getLocation,mt_el4_systemError)
    else if inherited replaces(callLocation,param,firstRep,lastRep,context,recycler,calledFromDelegator) then begin
      system.enterCriticalSection(rule_cs);
      try
        result:=true;
        P_context(context)^.reduceExpression(firstRep,recycler);
        if firstRep<>nil then lastRep:=firstRep^.last else begin
          lastRep:=nil;
          result:=false;
        end;
      finally
        system.leaveCriticalSection(rule_cs);
      end;
    end;
  end;

FUNCTION T_memoizedRule.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
{$MACRO ON}
{$define CLEAN_EXIT:=
if param=nil then disposeLiteral(useParam);
system.leaveCriticalSection(rule_cs);
exit}
  VAR lit:P_literal;
      useParam:P_listLiteral;
      sub:P_subruleExpression;
  PROCEDURE wrapResultInPutCacheRule;
    VAR newFirst,t:P_token;
    begin
      newFirst      :=recycler.newToken(firstRep^.location, getId+'.put.cache',tt_rulePutCacheValue,@self);
      newFirst^.next:=recycler.newToken(firstRep^.location, '', tt_parList_constructor,newListLiteral(1)^.append(useParam,true)); t:=newFirst^.next;
      t       ^.next:=firstRep;
      firstRep:=newFirst;
      lastRep^.next:=recycler.newToken(firstRep^.location, '', tt_braceClose);
      lastRep:=lastRep^.next;
    end;

  FUNCTION enterCriticalSectionWithDeadlockDetection:boolean; inline;
    CONST millesecondsBeforeRetry=10;
    begin
      while (tryEnterCriticalsection(rule_cs)=0) do begin
        if not(P_context(context)^.messages^.continueEvaluation) then exit(false);
        ThreadSwitch;
        sleep(millesecondsBeforeRetry);
      end;
      result:=true;
    end;

  begin
    result:=false;
    if param=nil then useParam:=newListLiteral
                 else useParam:=param;

    if not(enterCriticalSectionWithDeadlockDetection) then begin
      P_context(context)^.raiseError('Deadlock detected, trying to access memoized rule '+getId,callLocation);
      exit(false);
    end;
    lit:=cache.get(useParam);
    if lit<>nil then begin
      lit^.rereference;
      firstRep:=recycler.newToken(getLocation,'',tt_literal,lit);
      lastRep:=firstRep;
      CLEAN_EXIT(true);
    end else for sub in subrules do if ((sub^.isPublic) or (sub^.getLocation.package=callLocation.package)) and sub^.replaces(useParam,callLocation,firstRep,lastRep,P_context(context)^,recycler) then begin
      if (P_context(context)^.callDepth>=STACK_DEPTH_LIMIT) then begin wrapResultInPutCacheRule; CLEAN_EXIT(true); end;
      if (P_context(context)^.messages^.continueEvaluation) then P_context(context)^.reduceExpression(firstRep,recycler);
      if (P_context(context)^.messages^.continueEvaluation) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
        lit:=firstRep^.data;
        cache.put(useParam,lit);
        lastRep:=firstRep;
      end else begin
        recycler.cascadeDisposeToken(firstRep);
        firstRep:=recycler.newToken(getLocation,'',tt_literal,newVoidLiteral);
        lastRep:=firstRep;
      end;
      CLEAN_EXIT(true);
    end;
    if not(calledFromDelegator) and isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler)
    then begin CLEAN_EXIT(true); end
    else begin CLEAN_EXIT(false); end;
  end;

FUNCTION T_typeCastRule.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  VAR cast:P_typableLiteral;
      raw :P_literal;
  begin
    if inherited replaces(callLocation,param,firstRep,lastRep,context,recycler) then begin
      if P_context(context)^.callDepth>STACK_DEPTH_LIMIT then begin
        P_context(context)^.raiseError('Stack overflow in typecast rule',callLocation,mt_el4_systemError);
        exit(false);
      end;
      inc(P_context(context)^.callDepth);
      raw:=P_context(context)^.reduceToLiteral(firstRep,recycler).literal;
      dec(P_context(context)^.callDepth);
      if not(P_context(context)^.messages^.continueEvaluation) then begin
        if raw<>nil then disposeLiteral(raw);
        exit(false);
      end;
    end else if (param<>nil) and (param^.size=1)
    then raw:=param^.value[0]^.rereferenced
    else exit(false);

    cast:=typedef^.cast(raw,callLocation,context,@recycler);
    disposeLiteral(raw);
    if cast=nil then exit(false)
    else begin
      result:=true;
      firstRep:=recycler.newToken(callLocation,'',tt_literal,cast);
      lastRep:=firstRep;
    end;
  end;

FUNCTION T_typeCheckRule.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  VAR boolResult:boolean=false;
  begin
    boolResult:=(param<>nil)
            and (param^.size=1)
            and (typedef<>nil)
            and (typedef^.matchesLiteral(param^.value[0],callLocation,context,@recycler));
    firstRep:=recycler.newToken(callLocation,'',tt_literal,newBoolLiteral(boolResult));
    lastRep:=firstRep;
    result:=true;
  end;

FUNCTION T_mutableRule.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  begin
    result:=(not(privateRule) or (getLocation.package=callLocation.package)) and ((param=nil) or (param^.size=0));
    if result then begin
      {$ifdef fullVersion}
      if tco_stackTrace in P_context(context)^.threadOptions
      then P_context(context)^.callStackPush(callLocation,@self,newCallParametersNode(param));
      {$endif}
      system.enterCriticalSection(rule_cs);
      try
        firstRep:=recycler.newToken(getLocation,'',tt_literal,namedValue.getValue);
      finally
        system.leaveCriticalSection(rule_cs);
      end;
      lastRep:=firstRep;
      called:=true;
      {$ifdef fullVersion}
      if tco_stackTrace in P_context(context)^.threadOptions
      then P_context(context)^.callStackPop(firstRep);
      {$endif}
    end else result:=isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler);
  end;

FUNCTION T_datastoreRule.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  begin
    result:=(not(privateRule) or (getLocation.package=callLocation.package)) and ((param=nil) or (param^.size=0));
    if result then begin
      {$ifdef fullVersion}
      if tco_stackTrace in P_context(context)^.threadOptions
      then P_context(context)^.callStackPush(callLocation,@self,newCallParametersNode(param));
      {$endif}
      system.enterCriticalSection(rule_cs);
      try
        readDataStore(P_context(context)^,recycler);
        firstRep:=recycler.newToken(getLocation,'',tt_literal,namedValue.getValue);
        lastRep:=firstRep;
      finally
        system.leaveCriticalSection(rule_cs);
      end;
      {$ifdef fullVersion}
      if tco_stackTrace in P_context(context)^.threadOptions
      then P_context(context)^.callStackPop(firstRep);
      {$endif}
    end else result:=isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler);
  end;

FUNCTION T_typeCheckRule.getFirstParameterTypeWhitelist: T_literalTypeSet;
  begin
    result:=subrules[0]^.getPattern.getFirstParameterTypeWhitelist;
  end;

FUNCTION T_typeCheckRule.getTypedef: P_typedef;
  begin
    result:=typedef;
  end;

DESTRUCTOR T_typeCheckRule.destroy;
  begin
    if typedef<>nil then dispose(typedef,destroy);
    inherited destroy;
  end;

FUNCTION T_mutableRule.getValue(VAR context:T_context; VAR recycler:T_recycler):P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    try
      result:=namedValue.getValue;
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_datastoreRule.getValue(VAR context:T_context; VAR recycler:T_recycler):P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    try
      readDataStore(context,recycler);
      result:=namedValue.getValue;
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_rule.inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral;
  begin
    result:=newMapLiteral^
      .put(newStringLiteral('type'    ),newStringLiteral(C_ruleTypeText[getRuleType]),false)^
      .put(newStringLiteral('location'),newStringLiteral(getLocation                ),false)
      {$ifdef fullVersion}
      ^.put(newStringLiteral('used'),newBoolLiteral(isIdResolved),false)
      {$endif};
  end;

FUNCTION T_operatorDelegatorRule.inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral;
  begin
    if localRule=nil
    then result:=nil
    else result:=localRule^.inspect(includeFunctionPointer,context,recycler);
  end;

FUNCTION T_ruleWithSubrules.inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler): P_mapLiteral;
  FUNCTION subrulesList:P_listLiteral;
    VAR sub:P_subruleExpression;
    begin
      result:=newListLiteral(length(subrules));
      for sub in subrules do result^.append(sub^.inspect,false);
    end;

  begin
    result:=inherited inspect(includeFunctionPointer,context,recycler)^
            .put(newStringLiteral('subrules'),subrulesList,false);
    if includeFunctionPointer then
    result^.put(newStringLiteral('function'),getFunctionPointer(context,recycler,getLocation),false);
  end;

FUNCTION T_typeCastRule.inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral;
  FUNCTION privateOrPublic:string;
    begin
      if related^.hasPublicSubrule
      then result:=PUBLIC_TEXT
      else result:=PRIVATE_TEXT;
    end;

  FUNCTION subrulesList:P_listLiteral;
    VAR sub:P_subruleExpression;
    begin
      result:=newListLiteral(length(subrules)+1);
      result^.append(newMapLiteral^
        .put('pattern'   ,'()'           )^
        .put('location'  ,getLocation    )^
        .put('type'      ,privateOrPublic)^
        .put('comment'   ,related^.subrules[0]^.metaData.comment)^
        .put('attributes',related^.subrules[0]^.metaData.getAttributesLiteral,false),false);
      for sub in subrules do result^.append(sub^.inspect,false);
    end;

  begin
    result:=inherited inspect(includeFunctionPointer,context,recycler)^
            .put('subrules',subrulesList,false);
    if includeFunctionPointer then
    result^.put('function',getFunctionPointer(context,recycler,getLocation),false);
  end;

FUNCTION T_mutableRule.inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler): P_mapLiteral;
  FUNCTION privateOrPublic:string;
    begin
      if privateRule then result:=PRIVATE_TEXT
                     else result:=PUBLIC_TEXT;
    end;

  FUNCTION subrulesList:P_listLiteral;
    VAR value:P_literal;
    begin
      value:=namedValue.getValue;
      result:=newListLiteral(1);
      result^.append(newMapLiteral^
        .put('pattern'   ,'()'           )^
        .put('location'  ,getLocation    )^
        .put('type'      ,privateOrPublic)^
        .put('comment'   ,meta.comment   )^
        .put('attributes',meta.getAttributesLiteral,false),false);
      value^.unreference;
    end;

  begin
    result:=inherited inspect(includeFunctionPointer,context,recycler)^
      .put(newStringLiteral('subrules'),subrulesList,false);
    if includeFunctionPointer then begin
      result^.put(newStringLiteral('function'),getFunctionPointer(context,recycler,getLocation),false);
    end;
  end;

FUNCTION T_ruleWithSubrules.getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
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
    tempToken      :=recycler.newToken(location,getId,tt_userRule,@self);
    tempToken^.next:=getParametersForPseudoFuncPtr(minPatternLength,maxPatternLength>minPatternLength,location,context,recycler);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

FUNCTION T_operatorDelegatorRule.getFunctionPointer(VAR context: T_context; VAR recycler: T_recycler; CONST location: T_tokenLocation): P_expressionLiteral;
  begin
    result:=getIntrinsicRuleAsExpression(intFuncForOperator[intOperator]);
  end;

FUNCTION T_protectedRuleWithSubrules.getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
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
    tempToken            :=recycler.newToken(location,getId,tt_userRule,@self);
    tempToken^.next      :=getParametersForPseudoFuncPtr(minPatternLength,maxPatternLength>minPatternLength,location,context,recycler);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

FUNCTION T_typeCastRule.getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
  VAR tempToken:P_token=nil;
  begin
    tempToken            :=recycler.newToken(location,getId,tt_userRule,@self);
    tempToken^.next      :=getParametersForPseudoFuncPtr(1,false,location,context,recycler);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

FUNCTION T_mutableRule.getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
  VAR tempToken:P_token=nil;
  begin
    tempToken            :=recycler.newToken(location,getId,tt_userRule,@self);
    tempToken^.next      :=recycler.newToken(location,'',tt_braceOpen );
    tempToken^.next^.next:=recycler.newToken(location,'',tt_braceClose);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

{$ifdef fullVersion}
FUNCTION T_operatorDelegatorRule.getDocTxt: string;
  VAR r:P_rule;
  begin
    if localRule<>nil then result:=localRule^.getDocTxt
                      else result:='';
    for r in imported do   result:=result+BoolToStr(result='','',C_lineBreakChar)+r^.getDocTxt;
  end;

FUNCTION T_ruleWithSubrules.getDocTxt: ansistring;
  VAR s:P_subruleExpression;
  begin
    result:='';
    for s in subrules do result:=result+C_lineBreakChar+s^.getDocTxt();
    result:=join(formatTabs(split(result)),LineEnding);
    result:=ECHO_MARKER+C_ruleTypeText[getRuleType]+'rule '+getId+' '+ansistring(getLocation)+result;
  end;

FUNCTION T_mutableRule.getDocTxt: ansistring;
  begin
    result:=ECHO_MARKER+C_ruleTypeText[getRuleType]+'rule '+getId+' '+ansistring(getLocation);
    result:=result+meta.getDocTxt;
  end;
{$endif}

FUNCTION T_mutableRule.hasPublicSubrule: boolean;
  begin
    result:=not(privateRule);
  end;

PROCEDURE T_mutableRule.setMutableValue(CONST value: P_literal; CONST onDeclaration: boolean);
  begin
    system.enterCriticalSection(rule_cs);
    try
      namedValue.setValue(value);
      if not(onDeclaration) then begin
        valueChangedAfterDeclaration:=true;
        called:=true;
      end;
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_mutableRule.isReportable(OUT value: P_literal): boolean;
  begin
    value:=namedValue.getValue;
    if value=nil then exit(false);
    value^.unreference;
    result:=true;
  end;

PROCEDURE T_datastoreRule.readDataStore(VAR context:T_context; VAR recycler:T_recycler);
  VAR lit:P_literal;
  begin
    if not(called) or (not(valueChangedAfterDeclaration) and dataStoreMeta.fileChangedSinceRead) then begin
      lit:=dataStoreMeta.readValue(getLocation,context,recycler);
      if lit<>nil then begin
        namedValue.setValue(lit);
        lit^.unreference;
        called:=true;
      end;
    end;
  end;

FUNCTION T_mutableRule.mutateInline(CONST mutation: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation; VAR context: T_context; VAR recycler:T_recycler): P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    try
      result:=namedValue.mutate(mutation,RHS,location,@context,@recycler);
      valueChangedAfterDeclaration:=true;
      called:=true;
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_datastoreRule.mutateInline(CONST mutation: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation; VAR context: T_context; VAR recycler:T_recycler): P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    try
      if not(called) and not(valueChangedAfterDeclaration) then readDataStore(context,recycler);
      result:=inherited mutateInline(mutation,RHS,location,context,recycler);
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

PROCEDURE T_datastoreRule.writeBack(CONST adapters:P_messages);
  VAR L:P_literal;
  begin
    if adapters^.continueEvaluation and valueChangedAfterDeclaration then begin
      L:=namedValue.getValue;
      dataStoreMeta.writeValue(L,getLocation,adapters,encodeAsText);
      disposeLiteral(L);
    end;
  end;

PROCEDURE T_datastoreRule.memoryCleanup;
  begin
    enterCriticalSection(rule_cs);
    try
      if called and not(valueChangedAfterDeclaration) //The store has been read but not modified
         and (dataStoreMeta.fileExists)               //and the store can be re-read
      then begin
        namedValue.setValue(newVoidLiteral);
        called:=false;
      end;
    finally
      leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_datastoreRule.isInitialized:boolean;
  begin
    result:=called or valueChangedAfterDeclaration;
  end;

PROCEDURE T_memoizedRule.clearCache;
  begin
    enterCriticalSection(rule_cs);
    try
      cache.clear;
    finally
      leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_memoizedRule.doPutCache(CONST param: P_listLiteral): P_literal;
  begin
    enterCriticalSection(rule_cs);
    try
      cache.put(P_listLiteral(param^.value[0]),
                              param^.value[1] );
      result:=param^.value[1]^.rereferenced;
    finally
      leaveCriticalSection(rule_cs);
    end;
  end;

end.
