{$ifdef include_interface}
  T_rule=object
    private
      used,valueChangedAfterDeclaration:boolean;
      declarationStart,declarationEnd:T_tokenLocation;
      namedValue:T_namedVariable;
      ruleType:T_ruleType;
      cache:P_cache;
      rule_cs:system.TRTLCriticalSection;

      id:ansistring;
      subrules:array of P_subrule;
      PROCEDURE readDataStore(CONST package:P_package; CONST adapters:P_adapters);
    public
      CONSTRUCTOR create(CONST ruleId:ansistring; CONST ruleTyp:T_ruleType; CONST startAt:T_tokenLocation);
      DESTRUCTOR destroy;
      FUNCTION doPutCache(CONST param:P_listLiteral):P_literal;
      FUNCTION replaces(CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST callDepth:word; CONST includePrivateRules:boolean; VAR context:T_evaluationContext):boolean;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subrule; VAR context:T_evaluationContext);
      FUNCTION getLocationOfDeclaration:T_tokenLocation;
      FUNCTION getSubRuleHeaders:ansistring;
      PROCEDURE setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean; CONST adapters:P_adapters);
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
      FUNCTION hasPublicSubrule:boolean;
      FUNCTION getInlineValue:P_literal;
      {$ifdef fullVersion}
      FUNCTION complainAboutUncalled(CONST inMainPackage:boolean; VAR adapters:T_adapters):boolean;
      {$endif}
      FUNCTION getDocTxt:ansistring;
      FUNCTION getDocHtml:ansistring;
      FUNCTION writeBack(VAR provider:T_codeProvider; VAR adapters:T_adapters; CONST package:P_package):boolean;
      FUNCTION isReportable(OUT value:P_literal):boolean;
      FUNCTION inspect:P_listLiteral;
      PROCEDURE resolveIds(CONST adapters:P_adapters);
      FUNCTION idForErrorFeedback:ansistring;
    end;
{$endif}
{$ifdef include_implementation}
PROCEDURE T_rule.readDataStore(CONST package:P_package; CONST adapters:P_adapters);
  VAR stream:T_streamWrapper;
      lit:P_literal;
  begin
    stream:=package^.getDatastoreForId(id,true);
    if stream.allOkay then begin
      lit:=newLiteralFromStream(stream,declarationStart,adapters);
      if stream.allOkay then namedValue.setValue(lit);
      lit^.unreference;
    end;
    stream.destroy;
  end;

CONSTRUCTOR T_rule.create(CONST ruleId: ansistring; CONST ruleTyp:T_ruleType; CONST startAt:T_tokenLocation);
  begin
    used:=false;
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

    ruleType:=rt_normal;
    id:='';
  end;

FUNCTION T_rule.doPutCache(CONST param:P_listLiteral):P_literal;
  begin
    enterCriticalSection(rule_cs);
    cache^.put(P_listLiteral(param^.value(0)),
               param^.value(1));
    leaveCriticalSection(rule_cs);
    result:=param^.value(1);
    result^.rereference;
  end;

FUNCTION T_rule.replaces(CONST param: P_listLiteral; OUT firstRep,lastRep: P_token; CONST callDepth: word; CONST includePrivateRules: boolean; VAR context:T_evaluationContext): boolean;
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
      useParam^.rereference; //rereference useParam because it is used below
      newFirst      :=context.newToken(firstRep^.location, id+'.put.cache',tt_rulePutCacheValue,@self);
      newFirst^.next:=context.newToken(firstRep^.location, '', tt_parList_constructor,newOneElementListLiteral(useParam,true)); t:=newFirst^.next;
      t       ^.next:=firstRep;
      firstRep:=newFirst;
      lastRep^.next:=context.newToken(firstRep^.location, '', tt_braceClose);
      lastRep:=lastRep^.next;
    end;

  begin
    case ruleType of
      rt_memoized: begin
        if param=nil then useParam:=newListLiteral
                     else useParam:=param;
        if system.TryEnterCriticalsection(rule_cs)=0 then begin
          result:=false;
          for useUncurryingFallback:=false to true do for i:=0 to length(subrules)-1 do result:=result or ((includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(useParam,nil,firstRep,lastRep,context,useUncurryingFallback));
          if result then begin
            used:=true;
            wrapResultInPutCacheRule;
          end;
          if param=nil then disposeLiteral(useParam);
          exit(result);
        end;

        lit:=cache^.get(useParam);
        if lit<>nil then begin
          lit^.rereference;
          firstRep:=context.newToken(declarationStart,'',tt_literal,lit);
          lastRep:=firstRep;
          CLEAN_EXIT(true);
        end else for useUncurryingFallback:=false to true do
                 for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(useParam,nil,firstRep,lastRep,context,useUncurryingFallback) then begin
          if (callDepth>=STACK_DEPTH_LIMIT) then begin used:=true; wrapResultInPutCacheRule; CLEAN_EXIT(true); end;
          if (context.adapters^.noErrors) then reduceExpression(firstRep,callDepth+1,context);
          if (context.adapters^.noErrors) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
            lit:=firstRep^.data;
            cache^.put(useParam,lit);
          end;
          lastRep:=firstRep^.last;
          if not(context.adapters^.noErrors) then begin
            if (callDepth mod 1000)=0 then context.adapters^.raiseNote('Quitting evaluation of cached rule '+id+' on level '+intToStr(callDepth),declarationStart);
            context.cascadeDisposeToken(firstRep);
            firstRep:=context.newToken(declarationStart,'',tt_literal,newErrorLiteral);
            lastRep:=firstRep;
          end;
          used:=true;
          CLEAN_EXIT(true);
        end;
        CLEAN_EXIT(false);
      end;
      rt_synchronized: begin
        for useUncurryingFallback:=false to true do
        for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(param,nil,firstRep,lastRep,context,useUncurryingFallback) then begin
          if callDepth>=STACK_DEPTH_LIMIT then context.adapters^.raiseCustomMessage(mt_el5_systemError,'Stack depth limit exceeded calling '+id+'.',declarationStart)
          else begin
            system.enterCriticalSection(rule_cs);
            reduceExpression(firstRep,callDepth+1,context);
            lastRep:=firstRep^.last;
            system.leaveCriticalSection(rule_cs);
          end;
          used:=true;
          exit(true);
        end;
        result:=false;
      end;
      rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private,rt_datastore_public,rt_datastore_private: begin
        result:=(includePrivateRules or (ruleType in C_publicRuleTypes)) and ((param=nil) or (param^.size=0));
        if result then begin
          if not(used) and (ruleType in [rt_datastore_private,rt_datastore_public]) then readDataStore(P_package(declarationStart.package),context.adapters);
          system.enterCriticalSection(rule_cs);
          firstRep:=context.newToken(declarationStart,'',tt_literal,namedValue.getValue);
          system.leaveCriticalSection(rule_cs);
          lastRep:=firstRep;
          used:=true;
        end;
      end;
      rt_normal: begin
        for useUncurryingFallback:=false to true do
        for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(param,nil,firstRep,lastRep,context,useUncurryingFallback) then begin
          used:=true;
          exit(true);
        end;
        result:=false;
      end;
    end;
  end;

PROCEDURE T_rule.addOrReplaceSubRule(CONST rule: P_subrule; VAR context:T_evaluationContext);
  VAR i:longint;
  begin
    i:=0;
    while (i<length(subrules)) and not(rule^.pattern.isEquivalent(subrules[i]^.pattern)) do inc(i);
    if i>=length(subrules) then setLength(subrules,i+1)
                           else dispose(subrules[i],destroy);
    subrules[i]:=rule;
    if (length(subrules)>0) and (ruleType in C_mutableRuleTypes) then context.adapters^.raiseError('Cannot add a subrule to a mutable rule!',rule^.declaredAt);
    if (ruleType=rt_memoized) then cache^.clear;
  end;

FUNCTION T_rule.getLocationOfDeclaration: T_tokenLocation;
  begin
    result:=declarationStart;
  end;

FUNCTION T_rule.getSubRuleHeaders:ansistring;
  VAR i:longint;
  begin
    result:='';
    for i:=0 to length(subrules)-1 do
      result:=result+'#'+id+subrules[i]^.pattern.toString;
  end;

PROCEDURE T_rule.setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean; CONST adapters:P_adapters);
  begin
    system.enterCriticalSection(rule_cs);
    namedValue.setValue(value);
    if not(onDeclaration) then valueChangedAfterDeclaration:=true;
    system.leaveCriticalSection(rule_cs);
  end;

FUNCTION T_rule.mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    if not(used) and (ruleType in [rt_datastore_private,rt_datastore_public]) then readDataStore(P_package(declarationStart.package),context.adapters);
    result:=namedValue.mutate(mutation,RHS,location,context.adapters^);
    valueChangedAfterDeclaration:=true;
    used:=true;
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
    used:=used or (result<>nil);
  end;

{$ifdef fullVersion}
FUNCTION T_rule.complainAboutUncalled(CONST inMainPackage:boolean; VAR adapters:T_adapters):boolean;
  CONST PUBLIC_MITIGATION=' (the rule is public and might still be used by importing packages)';
  VAR i,j:longint;
      sameSignatureFound:boolean;
      mitigation:string;
  begin
    //If a rule is public, it may be used by importing packages -> no complaint
    if not(used) then begin
      if inMainPackage then adapters.raiseWarning('Uncalled rule '+id,declarationStart);
      exit(false);
    end;
    //If a rule is used and has only one subrule, this one is also used per definition -> no complaint
    if used and inMainPackage and (length(subrules)>1) then
      for i:=0 to length(subrules)-1 do begin
        if subrules[i]^.called then continue;
        //Might be part of a case distinction; Check for other rules with equal signature (ignoring value restrictions)
        sameSignatureFound:=false;
        for j:=0 to length(subrules)-1 do sameSignatureFound:=sameSignatureFound or (j<>i) and subrules[i]^.pattern.isEquivalentIgnoringRestrictions(subrules[j]^.pattern);
        if subrules[i]^.typ=srt_normal_public then mitigation:=PUBLIC_MITIGATION else mitigation:='';
        if not(sameSignatureFound) then adapters.raiseWarning('Uncalled subrule '+id+subrules[i]^.pattern.toString+mitigation,subrules[i]^.declaredAt);
      end;
    result:=true;
  end;
{$endif}

FUNCTION T_rule.getDocTxt:ansistring;
  VAR i:longint;
  begin
    result:=C_ruleTypeText[ruleType]+'rule '+id+C_lineBreakChar+'in '+declarationStart.package^.getPath;
    for i:=0 to length(subrules)-1 do result:=result+C_lineBreakChar+subrules[i]^.getDocTxt(id);
    if ruleType in C_mutableRuleTypes then result:=result+C_lineBreakChar+'declared '+ansistring(declarationStart);
  end;

FUNCTION T_rule.getDocHtml:ansistring;
  VAR anyPublic:boolean=false;
      i:longint;
  begin
    result:='<table><tr class="ruleHead"><td>';
    result:=result+toHtmlCode(C_ruleTypeText[ruleType]+' ');
    if id = 'main' then result:=result+'<b>'+id+'</b>' else result:=result+id;
    result:=result+'</td></tr>';
    for i:=0 to length(subrules)-1 do if subrules[i]^.typ=srt_normal_public then begin
      anyPublic:=true;
      result:=result+subrules[i]^.getDocHtml(id);
    end;
    result:=result+'</table>';
    if not(anyPublic) then result:='';
  end;

FUNCTION T_rule.writeBack(VAR provider:T_codeProvider; VAR adapters:T_adapters; CONST package:P_package):boolean;
  VAR code:ansistring;
      stream:T_streamWrapper;
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
                serialize(L,declarationStart,@adapters,0,true)+
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
          stream:=package^.getDatastoreForId(id,false);
          L:=namedValue.getValue;
          writeLiteralToStream(L,stream,declarationStart,@adapters);
          disposeLiteral(L);
          stream.destroy;
          result:=false; //because the package file remains unchanged
        end;
        else result:=false;
      end;//case ruleType
    end else result:=false;
  end;

FUNCTION T_rule.isReportable(OUT value:P_literal):boolean;
  begin
    if ruleType in [rt_memoized,rt_synchronized,rt_normal]
    then value:=getInlineValue
    else value:=namedValue.getValue;
    if value=nil then exit(false);
    value^.unreference;
    result:=true;
  end;

FUNCTION T_rule.inspect:P_listLiteral;
  FUNCTION privateOrPublic:string;
    begin
      if ruleType in C_publicRuleTypes then result:='public'
                                       else result:=C_tokenInfo[tt_modifier_private].defaultId;
    end;

  FUNCTION subrulesList:P_listLiteral;
    VAR i:longint;
    begin
      if ruleType in C_mutableRuleTypes then begin
        result:=newListLiteral^.append(newListLiteral^
          .append(newListLiteral^.appendString('pattern')^.appendString('()'),false)^
          .append(newListLiteral^.appendString('location')^.appendString(declarationStart),false)^
          .append(newListLiteral^.appendString('type')^.appendString(privateOrPublic),false)^
          .append(newListLiteral^.appendString('comment')^.appendString(''),false)^
          .append(newListLiteral^.appendString('body')^.appendString(namedValue.getValue^.toString),false),false);
      end else begin
        result:=newListLiteral;
        for i:=0 to length(subrules)-1 do
          result^.append(subrules[i]^.inspect,false);
      end;
    end;

  begin
    result:=newListLiteral^.appendString(id)^.append(newListLiteral^
      .append(newListLiteral^.appendString('type')^.appendString(C_ruleTypeText[ruleType]),false)^
      .append(newListLiteral^.appendString('location')^.appendString(declarationStart),false)^
      .append(newListLiteral^.appendString('subrules')^.append(subrulesList,false),false),false);
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

{$endif}
