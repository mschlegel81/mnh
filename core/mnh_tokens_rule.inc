{$ifdef include_interface}
  { T_rule }
  T_ruleType=(rt_normal,
              rt_memoized,
              rt_mutable_public,
              rt_mutable_private,
              rt_persistent_public,
              rt_persistent_private,
              rt_synchronized);

  T_rule=object
    private
      called,valueChangedAfterDeclaration:boolean;
      declarationStart,declarationEnd:T_tokenLocation;
      namedValue:T_namedVariable;
      ruleType:T_ruleType;
      cache:P_cache;
      rule_cs:system.TRTLCriticalSection;

      id:ansistring;
      subrules:array of P_subrule;
    public
      CONSTRUCTOR create(CONST ruleId:ansistring; CONST ruleTyp:T_ruleType; CONST startAt:T_tokenLocation);
      DESTRUCTOR destroy;
      FUNCTION doPutCache(CONST param:P_listLiteral):P_literal;
      FUNCTION replaces(CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST callDepth:word; CONST includePrivateRules:boolean; VAR recycler:T_tokenRecycler):boolean;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subrule);
      FUNCTION getLocationOfDeclaration:T_tokenLocation;
      FUNCTION getSubRuleHeaders:ansistring;
      PROCEDURE setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean);
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
      FUNCTION hasPublicSubrule:boolean;
      FUNCTION getInlineValue:P_literal;
      FUNCTION complainAboutUncalled(CONST inMainPackage:boolean):boolean;
      FUNCTION getDocTxt:ansistring;
      FUNCTION getDocHtml:ansistring;
      FUNCTION writeBack(VAR provider:T_codeProvider):boolean;
    end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_rule.create(CONST ruleId: ansistring; CONST ruleTyp:T_ruleType; CONST startAt:T_tokenLocation);
  begin
    called:=false;
    valueChangedAfterDeclaration:=false;
    declarationStart:=startAt;
    declarationEnd:=C_nilTokenLocation;
    ruleType:=ruleTyp;
    id:=ruleId;
    setLength(subrules,0);
    case ruleTyp of
      rt_memoized:begin
        new(cache,create);
        system.initCriticalSection(rule_cs);
      end;
      rt_mutable_public, rt_mutable_private, rt_persistent_public, rt_persistent_private: begin
        namedValue.create(id,newVoidLiteral);
        system.initCriticalSection(rule_cs);
      end;
      rt_synchronized:
        system.initCriticalSection(rule_cs);
    end;
  end;

DESTRUCTOR T_rule.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do dispose(subrules[i],destroy);
    setLength(subrules,0);
    if ruleType=rt_memoized then begin
      dispose(cache,destroy);
      system.doneCriticalSection(rule_cs);
    end else if ruleType in [rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private,rt_synchronized] then begin
      system.doneCriticalSection(rule_cs);
    end;
    if ruleType in [rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private] then namedValue.destroy;
    ruleType:=rt_normal;
    id:='';
  end;

FUNCTION T_rule.doPutCache(CONST param:P_listLiteral):P_literal;
  begin
    enterCriticalSection(rule_cs);
    cache^.put(P_listLiteral(param^.value(0)),
               cache^.getBinIdx(P_listLiteral(param^.value(0))),
               param^.value(1));
    leaveCriticalSection(rule_cs);
    result:=param^.value(1);
    result^.rereference;
  end;

FUNCTION T_rule.replaces(CONST param: P_listLiteral; OUT firstRep,lastRep: P_token; CONST callDepth: word; CONST includePrivateRules: boolean; VAR recycler:T_tokenRecycler): boolean;
{$define CLEAN_EXIT:=
if param=nil then disposeLiteral(useParam);
system.leaveCriticalSection(rule_cs);
exit}
  VAR i:longint;
      lit:P_literal;
      useParam:P_listLiteral;
      binIdx:longint;

  PROCEDURE wrapResultInPutCacheRule;
    VAR newFirst,t:P_token;
    begin
      newFirst      :=recycler.newToken(firstRep^.location, id+'.put.cache',tt_rulePutCacheValue,@self);
      newFirst^.next:=recycler.newToken(firstRep^.location, '', tt_braceOpen); t:=newFirst^.next;
      t       ^.next:=recycler.newToken(firstRep^.location, '', tt_literal, useParam); useParam^.rereference; t:=t^.next;
      t       ^.next:=recycler.newToken(firstRep^.location, '', tt_separatorComma); t:=t^.next;
      t       ^.next:=firstRep;
      firstRep:=newFirst;
      lastRep^.next:=recycler.newToken(firstRep^.location, '', tt_braceClose);
      lastRep:=lastRep^.next;
    end;

  begin
    case ruleType of
      rt_memoized: begin
        if param=nil then useParam:=newListLiteral
                     else useParam:=param;
        binIdx:=cache^.getBinIdx(useParam);
        if (callDepth>=STACK_DEPTH_LIMIT) or (system.TryEnterCriticalsection(rule_cs)=0) then begin
          result:=false;
          for i:=0 to length(subrules)-1 do result:=result or ((includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(useParam,nil,firstRep,lastRep,recycler));
          if result then begin
            called:=true;
            wrapResultInPutCacheRule;
            if param=nil then disposeLiteral(useParam);
          end;
          exit(result);
        end;

        lit:=cache^.get(useParam,binIdx);
        if lit<>nil then begin
          lit^.rereference;
          firstRep:=recycler.newToken(C_nilTokenLocation,'',tt_literal,lit);
          lastRep:=firstRep;
          CLEAN_EXIT(true);
        end else for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(useParam,nil,firstRep,lastRep,recycler) then begin
          if (callDepth>=STACK_DEPTH_LIMIT) then begin CLEAN_EXIT(true); end;
          if (noErrors) then reduceExpression(firstRep,callDepth+1,recycler);
          if (noErrors) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
            lit:=firstRep^.data;
            cache^.put(useParam,binIdx,lit);
          end;
          lastRep:=firstRep^.last;
          if not(noErrors) then begin
            if (callDepth mod 1000)=0 then raiseNote('Quitting evaluation of cached rule '+id+' on level '+intToStr(callDepth),C_nilTokenLocation);
            recycler.cascadeDisposeToken(firstRep);
            firstRep:=recycler.newToken(C_nilTokenLocation,'',tt_literal,newErrorLiteral);
            lastRep:=firstRep;
          end;
          CLEAN_EXIT(true);
        end;
        CLEAN_EXIT(false);
      end;
      rt_synchronized: begin
        for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(param,nil,firstRep,lastRep,recycler) then begin
          if callDepth>=STACK_DEPTH_LIMIT then raiseError_(mt_el5_systemError,'Stack depth limit exceeded calling '+id+'.',C_nilTokenLocation)
          else begin
            system.enterCriticalSection(rule_cs);
            reduceExpression(firstRep,callDepth+1,recycler);
            lastRep:=firstRep^.last;
            system.leaveCriticalSection(rule_cs);
          end;
          called:=true;
          exit(true);
        end;
        result:=false;
      end;
      rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private: begin
        result:=(includePrivateRules or (ruleType in [rt_mutable_public,rt_persistent_public])) and ((param=nil) or (param^.size=0));
        if result then begin
          system.enterCriticalSection(rule_cs);
          firstRep:=recycler.newToken(C_nilTokenLocation,'',tt_literal,namedValue.getValue);
          system.leaveCriticalSection(rule_cs);
          lastRep:=firstRep;
          called:=true;
        end;
      end;
      rt_normal: begin
        for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(param,nil,firstRep,lastRep,recycler) then begin
          called:=true;
          exit(true);
        end;
        result:=false;
      end;
    end;
  end;

PROCEDURE T_rule.addOrReplaceSubRule(CONST rule: P_subrule);
  VAR i:longint;
  begin
    i:=0;
    while (i<length(subrules)) and not(rule^.pattern.isEquivalent(subrules[i]^.pattern)) do inc(i);
    if i>=length(subrules) then setLength(subrules,i+1)
                           else dispose(subrules[i],destroy);
    subrules[i]:=rule;
    if (length(subrules)>0) and (ruleType in [rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private]) then raiseError('Cannot add a subrule to a mutable rule!',rule^.declaredAt);
    if (ruleType=rt_memoized) then cache^.clear;
  end;

FUNCTION T_rule.getLocationOfDeclaration: T_tokenLocation;
  begin
    if length(subrules)>0 then result:=subrules[0]^.declaredAt
                          else result:=C_nilTokenLocation;
  end;

FUNCTION T_rule.getSubRuleHeaders:ansistring;
  VAR i:longint;
  begin
    result:='';
    for i:=0 to length(subrules)-1 do
      result:=result+'#'+id+subrules[i]^.pattern.toString;
  end;

PROCEDURE T_rule.setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean);
  begin
    if (ruleType in [rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private]) then begin
      system.enterCriticalSection(rule_cs);
      namedValue.setValue(value);
      if not(onDeclaration) then valueChangedAfterDeclaration:=true;
      system.leaveCriticalSection(rule_cs);
    end else raiseError_(mt_el5_systemError,'This should not have happened: Trying to mutate an immutable rule!',C_nilTokenLocation);
  end;

FUNCTION T_rule.mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  begin
    if (ruleType in [rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private]) then begin
      system.enterCriticalSection(rule_cs);
      result:=namedValue.mutate(mutation,RHS,location);
      valueChangedAfterDeclaration:=true;
      system.leaveCriticalSection(rule_cs);
    end else raiseError_(mt_el5_systemError,'You cannot mutate an immutable rule!',C_nilTokenLocation);
  end;

FUNCTION T_rule.hasPublicSubrule: boolean;
  VAR i:longint;
  begin
    if ruleType in [rt_mutable_public,rt_persistent_public] then exit(true);
    for i:=0 to length(subrules)-1 do if subrules[i]^.typ=srt_normal_public then exit(true);
    result:=false;
  end;

FUNCTION T_rule.getInlineValue:P_literal;
  begin
    if length(subrules)=1 then result:=subrules[0]^.getInlineValue
                          else result:=nil;
  end;

FUNCTION T_rule.complainAboutUncalled(CONST inMainPackage:boolean):boolean;
  VAR i:longint;
  begin
    if not(called) then begin
      if inMainPackage then raiseWarning('Unused rule '+id,declarationStart);
      exit(false);
    end;
    if inMainPackage then for i:=0 to length(subrules)-1 do subrules[i]^.complainAboutUncalled(id,inMainPackage);
  end;

CONST C_ruleTypeText:array[T_ruleType] of string=(
    '','memoized ',
       'mutable ',
       'private mutable ',
       'persistent ',
       'private persistent ',
       'synchronized ');

FUNCTION T_rule.getDocTxt:ansistring;
  VAR i:longint;
  begin
    result:=C_ruleTypeText[ruleType]+'rule '+id+C_lineBreakChar+'in '+declarationStart.fileName;
    for i:=0 to length(subrules)-1 do result:=result+C_lineBreakChar+subrules[i]^.getDocTxt(id);
    if ruleType in [rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private] then
      result:=result+'declared '+ansistring(declarationStart);
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

FUNCTION T_rule.writeBack(VAR provider:T_codeProvider):boolean;
  VAR code:ansistring;
  begin
    if (noErrors) and (ruleType in [rt_persistent_private,rt_persistent_public]) and valueChangedAfterDeclaration then begin
      code:=C_tokenString[tt_modifier_persistent]+' '+
            id+
            C_tokenString[tt_assign]+
            namedValue.getValue^.toString+
            C_tokenString[tt_semicolon];
      raiseNote('Writing back: '+code,declarationStart);
      provider.replaceCode(
        declarationStart.line-1,
        declarationStart.column,
        declarationEnd.line-1,
        declarationEnd.column,
        code);
      result:=true;
    end else result:=false;
  end;

{$endif}
