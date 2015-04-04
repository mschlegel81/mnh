{$ifdef include_interface}
  P_rule=^T_rule;

  { T_rule }
  T_ruleType=(rt_normal,rt_memoized,rt_mutable,rt_synchronized);

  T_rule=object
    private
      ruleType:T_ruleType;
      cache:P_cache;
      rule_cs:system.TRTLCriticalSection;

      id:ansistring;
      subrules:array of P_subrule;
    public
      CONSTRUCTOR create(CONST ruleId:ansistring);
      CONSTRUCTOR createClone(VAR original:T_rule);
      DESTRUCTOR destroy;
      FUNCTION doPutCache(CONST param:P_listLiteral):P_literal;
      FUNCTION replaces(CONST param:P_listLiteral; OUT firstRep,lastRep:P_Token; CONST callDepth:word; CONST includePrivateRules:boolean; VAR recycler:T_tokenRecycler):boolean;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subrule);
      FUNCTION getLocationOfDeclaration:T_tokenLocation;
      PROCEDURE setMemoized    (CONST location:T_tokenLocation);
      PROCEDURE setMutable     (CONST location:T_tokenLocation);
      PROCEDURE setSynchronized(CONST location:T_tokenLocation);
      PROCEDURE setMutableValue(CONST value:P_literal);
      FUNCTION hasPublicSubrule:boolean;
      FUNCTION getInlineValue:P_literal;
      PROCEDURE complainAboutUncalled;
    end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_rule.create(CONST ruleId: ansistring);
  begin
    ruleType:=rt_normal;
    id:=ruleId;
    setLength(subrules,0);
  end;

CONSTRUCTOR T_rule.createClone(VAR original:T_rule);
  VAR i:longint;
  begin
    ruleType:=original.ruleType;
    if ruleType=rt_memoized then new(cache,create);
    if ruleType in [rt_memoized,rt_synchronized,rt_mutable] then system.InitCriticalSection(rule_cs);
    id:=original.id;
    setLength(subrules,length(original.subrules));
    for i:=0 to length(subrules)-1 do new(subrules[i],createClone(original.subrules[i]^));
  end;

DESTRUCTOR T_rule.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do dispose(subrules[i],destroy);
    setLength(subrules,0);
    if ruleType=rt_memoized then begin
      {$ifdef debugMode} writeln(stdErr,'Destroying cache of rule '+id); {$endif}
      dispose(cache,destroy);
      {$ifdef debugMode} writeln(stdErr,'Destroying critical section of rule '+id); {$endif}
      system.DoneCriticalsection(rule_cs);
    end else if ruleType in [rt_mutable,rt_synchronized] then begin
      system.DoneCriticalsection(rule_cs);
    end;
    {$ifdef debugMode} writeln(stdErr,'Rule '+id+' destroyed'); {$endif}
    ruleType:=rt_normal;
    id:='';
  end;

FUNCTION T_rule.doPutCache(CONST param:P_listLiteral):P_literal;
  begin
    EnterCriticalsection(rule_cs);
    cache^.put(P_listLiteral(param^.value(0)),
               cache^.getBinIdx(P_listLiteral(param^.value(0))),
               param^.value(1));
    LeaveCriticalsection(rule_cs);
    result:=param^.value(1);
    result^.rereference;
  end;

FUNCTION T_rule.replaces(CONST param: P_listLiteral; OUT firstRep,lastRep: P_Token; CONST callDepth: word; CONST includePrivateRules: boolean; VAR recycler:T_tokenRecycler): boolean;
{$define CLEAN_EXIT:=
if param=nil then disposeLiteral(useParam);
system.LeaveCriticalSection(rule_cs);
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
          if (errorLevel<el3_evalError) then reduceExpression(firstRep,false,callDepth+1,recycler);
          if (errorLevel<el3_evalError) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
            lit:=firstRep^.data;
            cache^.put(useParam,binIdx,lit);
          end;
          lastRep:=firstRep^.last;
          if (errorLevel>=el3_evalError) then begin
            if (callDepth mod 1000)=0 then raiseError(el1_note,'Quitting evaluation of cached rule '+id+' on level '+IntToStr(callDepth),C_nilTokenLocation);
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
          if callDepth>=STACK_DEPTH_LIMIT then raiseError(el5_systemError,'Stack depth limit exceeded calling '+id+'.',C_nilTokenLocation)
          else begin
            system.EnterCriticalSection(rule_cs);
            reduceExpression(firstRep,false,callDepth+1,recycler);
            lastRep:=firstRep^.last;
            system.LeaveCriticalSection(rule_cs);
          end;
          exit(true);
        end;
        result:=false;
      end;
      rt_mutable: begin
        system.EnterCriticalSection(rule_cs);
        result:=(includePrivateRules or (subrules[0]^.typ=srt_normal_public)) and subrules[0]^.replaces(param,nil,firstRep,lastRep,recycler);
        system.LeaveCriticalsection(rule_cs);
      end;
      rt_normal: begin
        for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(param,nil,firstRep,lastRep,recycler) then exit(true);
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
    if (length(subrules)>1) and (ruleType=rt_mutable) then raiseError(el4_parsingError,'Cannot add a subrule to a mutable rule!',rule^.declaredAt);
    if (ruleType=rt_memoized) then cache^.Clear;
  end;

FUNCTION T_rule.getLocationOfDeclaration: T_tokenLocation;
  begin
    result:=subrules[0]^.declaredAt;
  end;

PROCEDURE T_rule.setMemoized(CONST location:T_tokenLocation);
  begin
    if (ruleType=rt_normal) then begin
      ruleType:=rt_memoized;
      new(cache,create);
      system.InitCriticalSection(rule_cs);
    end else if ruleType=rt_mutable then raiseError(el2_warning,'A mutable rule cannot be memoized',location);
  end;

PROCEDURE T_rule.setMutable(CONST location:T_tokenLocation);
  begin
    if (ruleType=rt_normal) then begin
      if length(subrules)<>1 then raiseError(el4_parsingError,'A rule with more than one subrule cannot be mutable!',location)
      else begin
        subrules[0]^.setMutable(location);
        ruleType:=rt_mutable;
        system.InitCriticalSection(rule_cs);
      end;
    end else if ruleType in [rt_memoized,rt_synchronized] then raiseError(el4_parsingError,'A memoized or synchronized rule cannot be mutable!',location);
  end;

PROCEDURE T_rule.setSynchronized(CONST location:T_tokenLocation);
  begin
    if (ruleType=rt_normal) then begin
      ruleType:=rt_synchronized;
      system.InitCriticalSection(rule_cs);
    end else if ruleType = rt_mutable then raiseError(el4_parsingError,'A mutable rule cannot be synchronized!',location);
  end;

PROCEDURE T_rule.setMutableValue(CONST value:P_literal);
  begin
    if (ruleType=rt_mutable) then begin
      system.EnterCriticalSection(rule_cs);
      subrules[0]^.setMutableValue(value);
      system.LeaveCriticalSection(rule_cs);
    end else raiseError(el5_systemError,'This should not have happened: Trying to mutate an immutable rule!',C_nilTokenLocation);
  end;

FUNCTION T_rule.hasPublicSubrule: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do if subrules[i]^.typ=srt_normal_public then exit(true);
    result:=false;
  end;

FUNCTION T_rule.getInlineValue:P_literal;
  begin
    if length(subrules)=1 then result:=subrules[0]^.getInlineValue
                          else result:=nil;
  end;

PROCEDURE T_rule.complainAboutUncalled;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do subrules[i]^.complainAboutUncalled(id);
  end;

{$endif}
