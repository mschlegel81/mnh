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
  P_rule=^T_rule;
  T_subruleArray=array of P_subrule;

  T_rule=object(T_abstractRule)
    private
      idResolved,
      called,valueChangedAfterDeclaration:boolean;
      dataStoreMeta:P_datastoreMeta;
      declarationStart:T_tokenLocation;
      namedValue:T_namedVariable;
      ruleType:T_ruleType;
      cache:P_cache;
      rule_cs:system.TRTLCriticalSection;

      id:T_idString;
      subrules:T_subruleArray;
      PROCEDURE readDataStore(CONST adapters:P_adapters);
    public
      FUNCTION getId:T_idString; virtual;
      FUNCTION getLocation:T_tokenLocation; virtual;
      FUNCTION getRuleType:T_ruleType; virtual;
      PROPERTY getSubrules:T_subruleArray read subrules;
      PROCEDURE setIdResolved;

      CONSTRUCTOR create(CONST ruleId:T_idString; CONST ruleTyp:T_ruleType; CONST startAt:T_tokenLocation);
      DESTRUCTOR destroy;
      FUNCTION doPutCache(CONST param:P_listLiteral):P_literal;
      PROCEDURE clearCache;
      FUNCTION replaces(CONST param:P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep:P_token; CONST includePrivateRules:boolean; VAR context:T_threadContext):boolean;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subrule; VAR context:T_threadContext);
      FUNCTION getLocationOfDeclaration:T_tokenLocation;
      PROCEDURE setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean);
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal;
      FUNCTION hasPublicSubrule:boolean;
      FUNCTION getInlineValue:P_literal;
      {$ifdef fullVersion}
      FUNCTION complainAboutUnused(VAR adapters:T_adapters):boolean;
      {$endif}
      FUNCTION getCmdLineHelpText:T_arrayOfString;
      FUNCTION getDocTxt:ansistring;
      PROCEDURE writeBack(VAR adapters:T_adapters);
      FUNCTION isReportable(OUT value:P_literal):boolean;
      FUNCTION inspect:P_mapLiteral;
      PROCEDURE resolveIds(CONST adapters:P_adapters);
      FUNCTION idForErrorFeedback:ansistring;
      FUNCTION getFunctionPointer(VAR context:T_threadContext; CONST ruleTokenType:T_tokenType; CONST location:T_tokenLocation):P_expressionLiteral;
      FUNCTION getDynamicUseMetaLiteral(VAR context:T_threadContext):P_mapLiteral;
    end;

FUNCTION getParametersForPseudoFuncPtr(CONST minPatternLength:longint; CONST variadic:boolean; VAR context:T_threadContext; CONST location:T_tokenLocation):P_token;
FUNCTION getParametersForPseudoFuncPtr(CONST builtinRulePointer:pointer; VAR context:T_threadContext; CONST location:T_tokenLocation):P_token;
IMPLEMENTATION
FUNCTION getParametersForPseudoFuncPtr(CONST minPatternLength:longint; CONST variadic:boolean; VAR context:T_threadContext; CONST location:T_tokenLocation):P_token;
  VAR last:P_token;
      i:longint;
  begin
    result:=context.recycler.newToken(location,'',tt_braceOpen);
    last:=result;
    for i:=0 to minPatternLength-1 do begin
      if i>0 then begin
        last^.next:=context.recycler.newToken(location,'',tt_separatorComma);
        last:=last^.next;
      end;
      last^.next:=context.recycler.newToken(location,'$'+intToStr(i),tt_parameterIdentifier);
      last:=last^.next;
    end;
    last^.next:=context.recycler.newToken(location,'',tt_braceClose);
    last:=last^.next;
    if variadic then begin
      last^.next:=context.recycler.newToken(location,'',tt_listToParameterList);
      last:=last^.next;
      if minPatternLength>0 then begin
        last^.next:=context.recycler.newToken(location,'',tt_optionalParameters);
      end else begin
        last^.next:=context.recycler.newToken(location,ALL_PARAMETERS_TOKEN_TEXT,tt_parameterIdentifier);
      end;
    end;
  end;

FUNCTION getParametersForPseudoFuncPtr(CONST builtinRulePointer:pointer; VAR context:T_threadContext; CONST location:T_tokenLocation):P_token;
  VAR a:T_arityKind;
  begin
    a:=getMeta(builtinRulePointer).arityKind;
    result:=getParametersForPseudoFuncPtr(C_arityKind[a].fixedParameters,C_arityKind[a].variadic,context,location);
  end;

PROCEDURE T_rule.readDataStore(CONST adapters:P_adapters);
  VAR lit:P_literal;
  begin
    if not(called) or (not(valueChangedAfterDeclaration) and dataStoreMeta^.fileChangedSinceRead) then begin
      lit:=dataStoreMeta^.readValue(declarationStart,adapters);
      if lit<>nil then begin
        namedValue.setValue(lit);
        lit^.unreference;
      end;
    end;
  end;

FUNCTION T_rule.getId:T_idString;
  begin
    result:=id;
  end;

FUNCTION T_rule.getLocation:T_tokenLocation;
  begin
    result:=declarationStart;
  end;

FUNCTION T_rule.getRuleType:T_ruleType;
  begin
    result:=ruleType;
  end;

PROCEDURE T_rule.setIdResolved;
  begin
    idResolved:=true;
  end;

CONSTRUCTOR T_rule.create(CONST ruleId: T_idString; CONST ruleTyp:T_ruleType; CONST startAt:T_tokenLocation);
  begin
    called:=false;
    idResolved:=false;
    valueChangedAfterDeclaration:=false;
    declarationStart:=startAt;
    ruleType:=ruleTyp;
    id:=ruleId;
    setLength(subrules,0);

    if ruleType=rt_memoized
    then new(cache,create(rule_cs));

    if ruleType in C_csProtectedRuleTypes
    then system.initCriticalSection(rule_cs);

    if ruleType in C_mutableRuleTypes
    then namedValue.create(id,newVoidLiteral,false);

    if ruleType in [rt_datastore_private,rt_datastore_public] then
      new(dataStoreMeta,create(startAt.package^.getPath,ruleId));
  end;

DESTRUCTOR T_rule.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do dispose(subrules[i],destroy);
    setLength(subrules,0);

    if ruleType=rt_memoized
    then dispose(cache,destroy);

    if ruleType in C_csProtectedRuleTypes
    then system.doneCriticalSection(rule_cs);

    if ruleType in C_mutableRuleTypes
    then namedValue.destroy;

    if ruleType in [rt_datastore_private,rt_datastore_public] then
      dispose(dataStoreMeta,destroy);

    ruleType:=rt_normal;
    id:='';
  end;

FUNCTION T_rule.doPutCache(CONST param:P_listLiteral):P_literal;
  begin
    enterCriticalSection(rule_cs);
    cache^.put(P_listLiteral(param^[0]),
                             param^[1] );
    leaveCriticalSection(rule_cs);
    result:=param^[1]^.rereferenced;
  end;

PROCEDURE T_rule.clearCache;
  begin
    if not(ruleType=rt_memoized) then exit;
    enterCriticalSection(rule_cs);
    cache^.clear;
    leaveCriticalSection(rule_cs);
  end;

FUNCTION T_rule.replaces(CONST param: P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep: P_token; CONST includePrivateRules: boolean; VAR context:T_threadContext): boolean;
{$MACRO ON}
{$define CLEAN_EXIT:=
if param=nil then disposeLiteral(useParam);
system.leaveCriticalSection(rule_cs);
exit}
  VAR lit:P_literal;
      useParam:P_listLiteral;
      uncurrying:boolean;
      sub:P_subrule;
  PROCEDURE wrapResultInPutCacheRule;
    VAR newFirst,t:P_token;
    begin
      newFirst      :=context.recycler.newToken(firstRep^.location, id+'.put.cache',tt_rulePutCacheValue,@self);
      newFirst^.next:=context.recycler.newToken(firstRep^.location, '', tt_parList_constructor,newListLiteral(1)^.append(useParam,true)); t:=newFirst^.next;
      t       ^.next:=firstRep;
      firstRep:=newFirst;
      lastRep^.next:=context.recycler.newToken(firstRep^.location, '', tt_braceClose);
      lastRep:=lastRep^.next;
    end;

  begin
    result:=false;
    case ruleType of
      rt_memoized: begin
        if param=nil then useParam:=newListLiteral
                     else useParam:=param;
        enterCriticalSection(rule_cs);
        lit:=cache^.get(useParam);
        if lit<>nil then begin
          lit^.rereference;
          firstRep:=context.recycler.newToken(declarationStart,'',tt_literal,lit);
          lastRep:=firstRep;
          CLEAN_EXIT(true);
        end else for uncurrying:=false to true do
                 for sub in subrules do if (includePrivateRules or (sub^.getType=srt_normal_public)) and sub^.replaces(useParam,location,firstRep,lastRep,context,uncurrying) then begin
          if (context.callDepth>=STACK_DEPTH_LIMIT) then begin called:=true; wrapResultInPutCacheRule; CLEAN_EXIT(true); end;
          if (context.adapters^.noErrors) then context.reduceExpression(firstRep);
          if (context.adapters^.noErrors) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
            lit:=firstRep^.data;
            cache^.put(useParam,lit);
            lastRep:=firstRep;
          end else begin
            context.recycler.cascadeDisposeToken(firstRep);
            firstRep:=context.recycler.newToken(declarationStart,'',tt_literal,newVoidLiteral);
            lastRep:=firstRep;
          end;
          called:=true;
          CLEAN_EXIT(true);
        end;
        CLEAN_EXIT(false);
      end;
      rt_synchronized: begin
        for uncurrying:=false to true do
        for sub in subrules do if (includePrivateRules or (sub^.getType=srt_normal_public)) and sub^.replaces(param,location,firstRep,lastRep,context,uncurrying) then begin
          if context.callDepth>=STACK_DEPTH_LIMIT then context.adapters^.raiseSystemError('Stack depth limit exceeded calling '+id+'.',declarationStart)
          else begin
            system.enterCriticalSection(rule_cs);
            context.reduceExpression(firstRep);
            lastRep:=firstRep^.last;
            system.leaveCriticalSection(rule_cs);
          end;
          called:=true;
          exit(true);
        end;
        result:=false;
      end;
      rt_mutable_public,rt_mutable_private,rt_datastore_public,rt_datastore_private: begin
        result:=(includePrivateRules or (ruleType in C_publicRuleTypes)) and ((param=nil) or (param^.size=0));
        if result then begin
          system.enterCriticalSection(rule_cs);
          if not(valueChangedAfterDeclaration) and (ruleType in [rt_datastore_private,rt_datastore_public]) then readDataStore(context.adapters);
          firstRep:=context.recycler.newToken(declarationStart,'',tt_literal,namedValue.getValue);
          system.leaveCriticalSection(rule_cs);
          lastRep:=firstRep;
          called:=true;
        end;
      end;
      rt_customTypeCheck: begin
        for uncurrying:=false to true do
        for sub in subrules do if (includePrivateRules or (sub^.getType=srt_normal_public)) and sub^.replaces(param,location,firstRep,lastRep,context,uncurrying) then begin
          called:=true;
          exit(true);
        end;
        firstRep:=context.recycler.newToken(declarationStart,'',tt_literal,newBoolLiteral(false));
        lastRep:=firstRep;
        exit(true);
      end;
      rt_normal: begin
        for uncurrying:=false to true do
        for sub in subrules do if (includePrivateRules or (sub^.getType=srt_normal_public)) and sub^.replaces(param,location,firstRep,lastRep,context,uncurrying) then begin
          called:=true;
          exit(true);
        end;
        result:=false;
      end;
    end;
  end;

PROCEDURE T_rule.addOrReplaceSubRule(CONST rule: P_subrule; VAR context:T_threadContext);
  VAR i,j:longint;
  begin
    if (id=MAIN_RULE_ID) and not(rule^.hasValidMainPattern) then context.adapters^.raiseError('Invalid pattern/signature for main rule! Must accept strings.',rule^.getLocation);
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
    if (length(subrules)>1) and (ruleType in C_ruleTypesWithOnlyOneSubrule) then context.adapters^.raiseError('Cannot add a subrule to a '+C_ruleTypeText[ruleType]+'rule!',rule^.getLocation);
    if (ruleType=rt_memoized) then cache^.clear;
  end;

FUNCTION T_rule.getLocationOfDeclaration: T_tokenLocation;
  begin
    result:=declarationStart;
  end;

PROCEDURE T_rule.setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean);
  begin
    system.enterCriticalSection(rule_cs);
    namedValue.setValue(value);
    if not(onDeclaration) then valueChangedAfterDeclaration:=true;
    system.leaveCriticalSection(rule_cs);
  end;

FUNCTION T_rule.mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    if not(called) and (ruleType in [rt_datastore_private,rt_datastore_public]) then readDataStore(context.adapters);
    result:=namedValue.mutate(mutation,RHS,location,context.adapters^);
    valueChangedAfterDeclaration:=true;
    called:=true;
    system.leaveCriticalSection(rule_cs);
  end;

FUNCTION T_rule.hasPublicSubrule: boolean;
  VAR i:longint;
  begin
    if ruleType in C_publicRuleTypes then exit(true);
    for i:=0 to length(subrules)-1 do if subrules[i]^.getType=srt_normal_public then exit(true);
    result:=false;
  end;

FUNCTION T_rule.getInlineValue:P_literal;
  begin
    if length(subrules)=1 then result:=subrules[0]^.getInlineValue
                          else result:=nil;
    called:=called or (result<>nil);
  end;

{$ifdef fullVersion}
FUNCTION T_rule.complainAboutUnused(VAR adapters:T_adapters):boolean;
  CONST PUBLIC_MITIGATION=' (the rule is public and might still be used by importing packages)';
  FUNCTION mitigate:string;
    begin
      if hasPublicSubrule then result:=PUBLIC_MITIGATION else result:='';
    end;

  begin
    result:=(id<>MAIN_RULE_ID) and not(idResolved);
    if result then adapters.raiseWarning('Unused rule '+id+mitigate,lineLocation(declarationStart));
  end;
{$endif}

FUNCTION T_rule.getCmdLineHelpText:T_arrayOfString;
  VAR i:longint;
      txt:string;
  begin
    txt:=C_ruleTypeText[ruleType]+'rule '+id+C_lineBreakChar+'in '+declarationStart.package^.getPath;
    for i:=0 to length(subrules)-1 do txt:=txt+C_lineBreakChar+subrules[i]^.getCmdLineHelpText();
    result:=formatTabs(split(txt));
  end;

FUNCTION T_rule.getDocTxt:ansistring;
  VAR i:longint;
  begin
    result:='';
    for i:=0 to length(subrules)-1 do result:=result+C_lineBreakChar+subrules[i]^.getDocTxt();
    if ruleType in C_mutableRuleTypes then result:=result+C_lineBreakChar+'declared '+ansistring(declarationStart);
    result:=join(formatTabs(split(result)),LineEnding);
    result:=ECHO_MARKER+C_ruleTypeText[ruleType]+'rule '+id+C_lineBreakChar+'in '+declarationStart.package^.getPath+result;
  end;

PROCEDURE T_rule.writeBack(VAR adapters:T_adapters);
  VAR L:P_literal;
  begin
    if (adapters.noErrors) and valueChangedAfterDeclaration and (ruleType in [rt_datastore_private,rt_datastore_public]) then begin
      L:=namedValue.getValue;
      dataStoreMeta^.writeValue(L,declarationStart,@adapters);
      disposeLiteral(L);
    end;
  end;

FUNCTION T_rule.isReportable(OUT value:P_literal):boolean;
  begin
    if ruleType in [rt_memoized,rt_synchronized,rt_normal,rt_customTypeCheck]
    then value:=getInlineValue
    else value:=namedValue.getValue;
    if value=nil then exit(false);
    value^.unreference;
    result:=true;
  end;

FUNCTION T_rule.inspect:P_mapLiteral;
  FUNCTION privateOrPublic:string;
    begin
      if ruleType in C_publicRuleTypes then result:=PUBLIC_TEXT
                                       else result:=C_tokenInfo[tt_modifier_private].defaultId;
    end;

  FUNCTION subrulesList:P_listLiteral;
    VAR value:P_literal;
        sub:P_subrule;
    begin
      if ruleType in C_mutableRuleTypes then begin
        value:=namedValue.getValue;
        result:=newListLiteral(1);
        result^.append(newMapLiteral^
          .put('pattern' ,'()'            )^
          .put('location',declarationStart)^
          .put('type'    ,privateOrPublic )^
          .put('comment' ,''              )^
          .put('body'    ,value^.toString ),false);
        value^.unreference;
      end else begin
        result:=newListLiteral(length(subrules));
        for sub in subrules do result^.append(sub^.inspect,false);
      end;
    end;

  begin
    result:=newMapLiteral^
      .put('type'    ,C_ruleTypeText[ruleType])^
      .put('location',declarationStart        )^
      .put('subrules',subrulesList,false      );
  end;

PROCEDURE T_rule.resolveIds(CONST adapters:P_adapters);
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do subrules[i]^.resolveIds(adapters);
  end;

FUNCTION T_rule.idForErrorFeedback:ansistring;
  begin
    result:=declarationStart.package^.getPath+'.'+id+' ('+ansistring(declarationStart)+')';
  end;

FUNCTION T_rule.getFunctionPointer(VAR context:T_threadContext; CONST ruleTokenType:T_tokenType; CONST location:T_tokenLocation):P_expressionLiteral;
  VAR minPatternLength:longint=maxLongint;
      maxPatternLength:longint=0;
      sub:P_subrule;
      tempToken:P_token=nil;
  begin
    if ruleType in C_mutableRuleTypes then begin
      tempToken            :=context.recycler.newToken(location,id,ruleTokenType,@self);
      tempToken^.next      :=context.recycler.newToken(location,'',tt_braceOpen );
      tempToken^.next^.next:=context.recycler.newToken(location,'',tt_braceClose);
    end else begin
      if (ruleType=rt_normal) and (length(subrules)=1) then exit(P_expressionLiteral(subrules[0]^.rereferenced));
      for sub in subrules do begin
        minPatternLength:=min(minPatternLength,sub^.arity);
        maxPatternLength:=max(maxPatternLength,sub^.arity);
        if sub^.isVariadic then maxPatternLength:=maxLongint;
      end;
      tempToken            :=context.recycler.newToken(location,id,ruleTokenType,@self);
      tempToken^.next      :=getParametersForPseudoFuncPtr(minPatternLength,maxPatternLength>minPatternLength,context,location);
    end;
    new(P_subrule(result),createFromInline(tempToken,context));
  end;

FUNCTION T_rule.getDynamicUseMetaLiteral(VAR context:T_threadContext):P_mapLiteral;
  VAR attributes:P_mapLiteral;
      subAttributes:P_mapLiteral;
      sub:P_subrule;
  begin
    attributes:=newMapLiteral;
    for sub in subrules do begin
      subAttributes:=sub^.getAttributesLiteral;
      attributes^.putAll(subAttributes);
      disposeLiteral(subAttributes);
    end;

    result:=newMapLiteral^
              .put('rule'      ,getFunctionPointer(context,tt_importedUserRule,declarationStart),false)^
              .put('attributes',attributes,false);
  end;

FUNCTION customTypeCheckToExpression(CONST rule:pointer):P_expressionLiteral;
  begin
    result:=P_rule(rule)^.subrules[0];
  end;

INITIALIZATION
  customTypeCheckToExpressionCallback:=@customTypeCheckToExpression;
end.
