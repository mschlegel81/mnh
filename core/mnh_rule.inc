{$ifdef include_interface}
  T_rule=object
    private
      idResolved,called,valueChangedAfterDeclaration:boolean;
      dataStoreMeta:P_datastoreMeta;
      declarationStart,declarationEnd:T_tokenLocation;
      namedValue:T_namedVariable;
      ruleType:T_ruleType;
      cache:P_cache;
      rule_cs:system.TRTLCriticalSection;

      id:T_idString;
      subrules:array of P_subrule;
      PROCEDURE readDataStore(CONST adapters:P_adapters);
    public
      CONSTRUCTOR create(CONST ruleId:T_idString; CONST ruleTyp:T_ruleType; CONST startAt:T_tokenLocation);
      DESTRUCTOR destroy;
      FUNCTION doPutCache(CONST param:P_listLiteral):P_literal;
      PROCEDURE clearCache;
      FUNCTION replaces(CONST param:P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep:P_token; CONST callDepth:word; CONST includePrivateRules:boolean; VAR context:T_evaluationContext):boolean;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subrule; VAR context:T_evaluationContext);
      FUNCTION getLocationOfDeclaration:T_tokenLocation;
      PROCEDURE setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean);
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
      FUNCTION hasPublicSubrule:boolean;
      FUNCTION getInlineValue:P_literal;
      {$ifdef fullVersion}
      FUNCTION complainAboutUnused(VAR adapters:T_adapters):boolean;
      {$endif}
      FUNCTION getCmdLineHelpText:T_arrayOfString;
      FUNCTION getDocTxt:ansistring;
      FUNCTION writeBack(VAR provider:T_codeProvider; VAR adapters:T_adapters):boolean;
      FUNCTION isReportable(OUT value:P_literal):boolean;
      FUNCTION inspect:P_mapLiteral;
      PROCEDURE resolveIds(CONST adapters:P_adapters);
      FUNCTION idForErrorFeedback:ansistring;
      FUNCTION getParametersForPseudoFuncPointer(VAR context:T_evaluationContext; CONST location:T_tokenLocation):P_token;
      FUNCTION getDynamicUseMetaLiteral(VAR context:T_evaluationContext):P_mapLiteral;
    end;
{$endif}
{$ifdef include_implementation}
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

CONSTRUCTOR T_rule.create(CONST ruleId: T_idString; CONST ruleTyp:T_ruleType; CONST startAt:T_tokenLocation);
  begin
    called:=false;
    idResolved:=false;
    valueChangedAfterDeclaration:=false;
    declarationStart:=startAt;
    declarationEnd:=startAt;
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

FUNCTION T_rule.replaces(CONST param: P_listLiteral; CONST location:T_tokenLocation; OUT firstRep,lastRep: P_token; CONST callDepth: word; CONST includePrivateRules: boolean; VAR context:T_evaluationContext): boolean;
{$define CLEAN_EXIT:=
if param=nil then disposeLiteral(useParam);
system.leaveCriticalSection(rule_cs);
exit}
  VAR i:longint;
      lit:P_literal;
      useParam:P_listLiteral;
      useUncurryingFallback:boolean;

  PROCEDURE wrapResultInPutCacheRule;
    VAR newFirst,t:P_token;
    begin
      newFirst      :=context.newToken(firstRep^.location, id+'.put.cache',tt_rulePutCacheValue,@self);
      newFirst^.next:=context.newToken(firstRep^.location, '', tt_parList_constructor,newListLiteral(1)^.append(useParam,true)); t:=newFirst^.next;
      t       ^.next:=firstRep;
      firstRep:=newFirst;
      lastRep^.next:=context.newToken(firstRep^.location, '', tt_braceClose);
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
          firstRep:=context.newToken(declarationStart,'',tt_literal,lit);
          lastRep:=firstRep;
          CLEAN_EXIT(true);
        end else for useUncurryingFallback:=false to true do
                 for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(useParam,nil,location,firstRep,lastRep,context,useUncurryingFallback) then begin
          if (callDepth>=STACK_DEPTH_LIMIT) then begin called:=true; wrapResultInPutCacheRule; CLEAN_EXIT(true); end;
          if (context.adapters^.noErrors) then reduceExpression(firstRep,callDepth+1,context);
          if (context.adapters^.noErrors) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
            lit:=firstRep^.data;
            cache^.put(useParam,lit);
            lastRep:=firstRep;
          end else begin
            if (callDepth mod 1000)=0 then context.adapters^.raiseNote('Quitting evaluation of cached rule '+id+' on level '+intToStr(callDepth),declarationStart);
            context.cascadeDisposeToken(firstRep);
            firstRep:=context.newToken(declarationStart,'',tt_literal,newVoidLiteral);
            lastRep:=firstRep;
          end;
          called:=true;
          CLEAN_EXIT(true);
        end;
        CLEAN_EXIT(false);
      end;
      rt_synchronized: begin
        for useUncurryingFallback:=false to true do
        for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(param,nil,location,firstRep,lastRep,context,useUncurryingFallback) then begin
          if callDepth>=STACK_DEPTH_LIMIT then context.adapters^.raiseSystemError('Stack depth limit exceeded calling '+id+'.',declarationStart)
          else begin
            system.enterCriticalSection(rule_cs);
            reduceExpression(firstRep,callDepth+1,context);
            lastRep:=firstRep^.last;
            system.leaveCriticalSection(rule_cs);
          end;
          called:=true;
          exit(true);
        end;
        result:=false;
      end;
      rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private,rt_datastore_public,rt_datastore_private: begin
        result:=(includePrivateRules or (ruleType in C_publicRuleTypes)) and ((param=nil) or (param^.size=0));
        if result then begin
          system.enterCriticalSection(rule_cs);
          if not(valueChangedAfterDeclaration) and (ruleType in [rt_datastore_private,rt_datastore_public]) then readDataStore(context.adapters);
          firstRep:=context.newToken(declarationStart,'',tt_literal,namedValue.getValue);
          system.leaveCriticalSection(rule_cs);
          lastRep:=firstRep;
          called:=true;
        end;
      end;
      rt_customTypeCheck: begin
        for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(param,nil,location,firstRep,lastRep,context,false) then begin
          called:=true;
          exit(true);
        end;
        firstRep:=context.newToken(declarationStart,'',tt_literal,newBoolLiteral(false));
        lastRep:=firstRep;
        exit(true);
      end;
      rt_normal: begin
        for useUncurryingFallback:=false to true do
        for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(param,nil,location,firstRep,lastRep,context,useUncurryingFallback) then begin
          called:=true;
          exit(true);
        end;
        result:=false;
      end;
    end;
  end;

PROCEDURE T_rule.addOrReplaceSubRule(CONST rule: P_subrule; VAR context:T_evaluationContext);
  VAR i,j:longint;
  begin
    if (id=MAIN_RULE_ID) and not(rule^.pattern.isValidMainPattern) then context.adapters^.raiseError('Invalid pattern/signature for main rule! Must accept strings.',rule^.declaredAt);
    i:=0;
    while (i<length(subrules)) and not(rule^.pattern.isEquivalent(subrules[i]^.pattern)) do inc(i);
    if i>=length(subrules) then begin
      setLength(subrules,i+1);
      for j:=0 to i-1 do if subrules[j]^.pattern.hides(rule^.pattern) then context.adapters^.raiseWarning('Rule '+rule^.getId+' seems to be hidden by '+subrules[j]^.getId+' @'+ansistring(subrules[j]^.declaredAt),rule^.declaredAt);
    end else begin
      dispose(subrules[i],destroy);
      context.adapters^.raiseWarning('Overriding rule '+rule^.getId,rule^.declaredAt);
    end;
    subrules[i]:=rule;
    if (length(subrules)>1) and (ruleType in C_ruleTypesWithOnlyOneSubrule) then context.adapters^.raiseError('Cannot add a subrule to a '+C_ruleTypeText[ruleType]+'rule!',rule^.declaredAt);
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

FUNCTION T_rule.mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
    for i:=0 to length(subrules)-1 do if subrules[i]^.typ=srt_normal_public then exit(true);
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
    result:=C_ruleTypeText[ruleType]+'rule '+id+C_lineBreakChar+'in '+declarationStart.package^.getPath+result;
  end;

FUNCTION T_rule.writeBack(VAR provider:T_codeProvider; VAR adapters:T_adapters):boolean;
  VAR code:ansistring;
      L:P_literal;
  begin
    if (adapters.noErrors) and valueChangedAfterDeclaration then begin
      case ruleType of
        rt_persistent_private,rt_persistent_public:
        begin
          if ruleType=rt_persistent_private then code:=C_tokenInfo[tt_modifier_private].defaultId+' '
                                            else code:='';
          code:=code+
                C_tokenInfo[tt_modifier_persistent].defaultId+' '+
                id;
          adapters.raiseNote('Writing back: '+code,declarationStart);
          L:=namedValue.getValue;
          code:=code+
                C_tokenInfo[tt_assign].defaultId+
                serialize(L,declarationStart,@adapters,true)+
                C_tokenInfo[tt_semicolon].defaultId;
          disposeLiteral(L);
          provider.replaceCode(
            declarationStart.line-1,
            declarationStart.column,
            declarationEnd.line-1,
            declarationEnd.column,
            code);
          result:=true;
        end;
        rt_datastore_private,rt_datastore_public:
        begin
          L:=namedValue.getValue;
          dataStoreMeta^.writeValue(L,declarationStart,@adapters);
          disposeLiteral(L);
          result:=false; //because the package file remains unchanged
        end;
        else result:=false;
      end;//case ruleType
    end else result:=false;
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
      {$ifdef debugMode}
      writeln(stdErr,'inspecting ',id,' ',ansistring(declarationStart));
      {$endif}
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
    result:=newMapLiteral^.put(id,newMapLiteral^
      .put('type'    ,C_ruleTypeText[ruleType])^
      .put('location',declarationStart        )^
      .put('subrules',subrulesList,false      ),false);
  end;

PROCEDURE T_rule.resolveIds(CONST adapters:P_adapters);
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do subrules[i]^.resolveIds(adapters);
  end;

FUNCTION T_rule.idForErrorFeedback:ansistring;
  begin
    result:=P_package(declarationStart.package)^.codeProvider.id+'.'+id+' ('+ansistring(declarationStart)+')';
  end;

FUNCTION T_rule.getParametersForPseudoFuncPointer(VAR context:T_evaluationContext; CONST location:T_tokenLocation):P_token;
  VAR minPatternLength:longint=maxLongint;
      maxPatternLength:longint=0;
      sub:P_subrule;
  begin
    if ruleType in C_mutableRuleTypes then begin
      result:=context.newToken(location,'',tt_braceOpen);
      result^.next:=context.newToken(location,'',tt_braceClose);
    end else begin
      for sub in subrules do begin
        minPatternLength:=min(minPatternLength,length(sub^.pattern.sig));
        maxPatternLength:=max(maxPatternLength,length(sub^.pattern.sig));
        if sub^.pattern.hasOptionals then maxPatternLength:=maxLongint;
      end;
      result:=getParametersForPseudoFuncPtr(minPatternLength,maxPatternLength>minPatternLength,context,location);
    end;
  end;

FUNCTION T_rule.getDynamicUseMetaLiteral(VAR context:T_evaluationContext):P_mapLiteral;
  VAR attributes:P_setLiteral;
      subAttributes:P_listLiteral;
      sub:P_subrule;
      expressionToken:P_token;
  begin
    attributes:=newSetLiteral();
    for sub in subrules do begin
      subAttributes:=sub^.getAttributesLiteral;
      attributes^.appendAll(subAttributes);
      disposeLiteral(subAttributes);
    end;

    expressionToken:=context.newToken(declarationStart,id,tt_importedUserRule,@self);
    expressionToken^.next:=getParametersForPseudoFuncPointer(context,declarationStart);
    new(sub,createFromInline(expressionToken,context));

    result:=newMapLiteral^
              .put('id'        ,id)^
              .put('rule'      ,newExpressionLiteral(sub),false)^
              .put('attributes',attributes               ,false);
  end;

{$endif}
