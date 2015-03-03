{$ifdef include_interface}
  P_rule=^T_rule;

  { T_rule }
  T_ruleType=(rt_normal,rt_memoized,rt_mutable,rt_synchronized);

  T_rule=object
    private
      ruleType:T_ruleType;
      cache:T_cache;
      rule_cs:system.TRTLCriticalSection;
      rule_timer:TEpikTimer;

      id:ansistring;
      subrules:array of P_subrule;
    public
      CONSTRUCTOR create(CONST ruleId:ansistring);
      DESTRUCTOR destroy;
      FUNCTION replaces(CONST param:P_listLiteral; OUT firstRep,lastRep:P_Token; CONST callDepth:word; CONST includePrivateRules:boolean; VAR recycler:T_tokenRecycler):boolean;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subrule);
      FUNCTION getLocationOfDeclaration:T_tokenLocation;
      PROCEDURE setMemoized    (CONST location:T_tokenLocation);
      PROCEDURE setMutable     (CONST location:T_tokenLocation);
      PROCEDURE setSynchronized(CONST location:T_tokenLocation);
      PROCEDURE setMutableValue(CONST value:P_literal);
      FUNCTION hasPublicSubrule:boolean;
      FUNCTION getInlineValue:P_literal;
    end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_rule.create(CONST ruleId: ansistring);
  begin
    ruleType:=rt_normal;
    id:=ruleId;
    setLength(subrules,0);
  end;

DESTRUCTOR T_rule.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do dispose(subrules[i],destroy);
    setLength(subrules,0);
    if ruleType=rt_memoized then begin
      {$ifdef debugMode} writeln(stdErr,'Destroying cache of rule '+id); {$endif}
      cache.destroy;
      {$ifdef debugMode} writeln(stdErr,'Destroying critical section of rule '+id); {$endif}
      system.DoneCriticalsection(rule_cs);
      {$ifdef debugMode} writeln(stdErr,'Destroying timer of rule '+id); {$endif}
      rule_timer.Free;
    end else if ruleType in [rt_mutable,rt_synchronized] then begin
      system.DoneCriticalsection(rule_cs);
    end;
    {$ifdef debugMode} writeln(stdErr,'Rule '+id+' destroyed'); {$endif}
    ruleType:=rt_normal;
    id:='';
  end;

FUNCTION T_rule.replaces(CONST param: P_listLiteral; OUT firstRep,lastRep: P_Token; CONST callDepth: word; CONST includePrivateRules: boolean; VAR recycler:T_tokenRecycler): boolean;
  VAR i:longint;
      lit:P_literal;
      useParam:P_listLiteral;
      evalCost:longint;
  begin
    case ruleType of
      rt_memoized: begin
        if param=nil then useParam:=newListLiteral
                     else useParam:=param;
        system.EnterCriticalSection(rule_cs);
        lit:=cache.get(useParam);
        if lit<>nil then begin
          lit^.rereference;
          firstRep:=recycler.newToken(C_nilTokenLocation,'',tt_literal,lit);
          lastRep:=firstRep;
          if param=nil then disposeLiteral(useParam);
          system.LeaveCriticalSection(rule_cs);
          exit(true);
        end else for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(useParam,nil,firstRep,lastRep,recycler) then begin
          if (callDepth>=STACK_DEPTH_LIMIT) then begin
            system.LeaveCriticalSection(rule_cs);
            exit(true);
          end;
          evalCost:=round(1000000*rule_timer.Elapsed());
          if (errorLevel<el3_evalError) then reduceExpression(firstRep,false,callDepth+1,recycler);
          evalCost:=round(1000000*rule_timer.Elapsed())-evalCost;
          if (errorLevel<el3_evalError) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
            lit:=firstRep^.data;
            cache.put(useParam,lit,evalCost);
          end;
          lastRep:=firstRep^.last;
          if param=nil then disposeLiteral(useParam);
          if (errorLevel>=el3_evalError) then begin
            if (callDepth mod 1000)=0 then raiseError(el1_note,'Quitting evaluation of cached rule '+id+' on level '+IntToStr(callDepth),C_nilTokenLocation);
            recycler.cascadeDisposeToken(firstRep);
            firstRep:=recycler.newToken(C_nilTokenLocation,'',tt_literal,newErrorLiteral);
            lastRep:=firstRep;
          end;
          system.LeaveCriticalSection(rule_cs);
          exit(true);
        end;
        system.LeaveCriticalSection(rule_cs);
        result:=false;
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
  end;

FUNCTION T_rule.getLocationOfDeclaration: T_tokenLocation;
  begin
    result:=subrules[0]^.declaredAt;
  end;

PROCEDURE T_rule.setMemoized(CONST location:T_tokenLocation);
  begin
    if (ruleType=rt_normal) then begin
      ruleType:=rt_memoized;
      cache.create;
      system.InitCriticalSection(rule_cs);
      rule_timer:= TEpikTimer.create(nil);
      rule_timer.Clear;
      rule_timer.Start;
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


{$endif}
