{$ifdef include_interface}
  P_rule=^T_rule;

  { T_rule }

  T_rule=object
    private
      isCached:boolean;
      cache:T_cache;

      id:ansistring;
      subrules:array of P_subrule;
    public
      CONSTRUCTOR create(CONST ruleId:ansistring);
      DESTRUCTOR destroy;
      FUNCTION replaces(CONST param:P_listLiteral; OUT firstRep,lastRep:P_Token; CONST callDepth:word; CONST includePrivateRules:boolean):boolean;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subrule);
      PROCEDURE addOrSetSubRule(CONST rule:P_subrule);
      FUNCTION getLocationOfDeclaration:T_tokenLocation;
      PROCEDURE setMemoized;
      FUNCTION hasPublicSubrule:boolean;
  end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_rule.create(CONST ruleId: ansistring);
  begin
    isCached:=false;
    id:=ruleId;
    setLength(subrules,0);
  end;

DESTRUCTOR T_rule.destroy;
  VAR i:longint;
  begin
    id:='';
    for i:=0 to length(subrules)-1 do dispose(subrules[i],destroy);
    setLength(subrules,0);
    if isCached then cache.destroy;
    isCached:=false;
  end;

FUNCTION T_rule.replaces(CONST param: P_listLiteral; OUT firstRep,lastRep: P_Token; CONST callDepth: word; CONST includePrivateRules: boolean): boolean;
  VAR i:longint;
      lit:P_literal;
      useParam:P_listLiteral;
      evalCost:longint;
  begin
    if isCached {and (ThreadID=mainThread)} then begin
      if param=nil then useParam:=newListLiteral
                   else useParam:=param;
      lit:=cache.get(useParam);
      if lit<>nil then begin
        lit^.rereference;
        firstRep:=newToken(C_nilTokenLocation,'',tt_literal,lit);
        lastRep:=firstRep;
        if param=nil then disposeLiteral(useParam);
        exit(true);
      end else for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(useParam,nil,firstRep,lastRep) then begin
        if callDepth>=STACK_DEPTH_LIMIT then exit(true);
        evalCost:=GetTickCount;
        if (errorLevel<el3_evalError) then reduceExpression(firstRep,callDepth+1);
        evalCost:=getTickCount-evalCost;
        if (errorLevel<el3_evalError) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
          lit:=firstRep^.data;
          if (ThreadID=mainThread) and (poolThreadsRunning=0) then cache.polish;
          cache.put(useParam,lit,evalCost);
        end;  
        lastRep:=firstRep^.last;
        if param=nil then disposeLiteral(useParam);
        if (errorLevel>=el3_evalError) then begin
          if (callDepth mod 1000)=0 then raiseError(el1_note,'Quitting evaluation of cached rule on level '+IntToStr(callDepth),C_nilTokenLocation);
          cascadeDisposeToken(firstRep);
          firstRep:=newToken(C_nilTokenLocation,'',tt_literal,newErrorLiteral);
          lastRep:=firstRep;
        end;
        exit(true);
      end;
      result:=false;
    end else begin
      for i:=0 to length(subrules)-1 do if (includePrivateRules or (subrules[i]^.typ=srt_normal_public)) and subrules[i]^.replaces(param,nil,firstRep,lastRep) then exit(true);
      result:=false;
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
  end;

PROCEDURE T_rule.addOrSetSubRule(CONST rule: P_subrule);
  VAR i:longint;
  begin
    writeln('adding or setting subrule: ',rule^.toString);
    i:=0;
    while (i<length(subrules)) and not(rule^.pattern.isEquivalent(subrules[i]^.pattern)) do inc(i);
    if i>=length(subrules) then setLength(subrules,i+1);
    subrules[i]:=rule;
  end;

FUNCTION T_rule.getLocationOfDeclaration: T_tokenLocation;
  begin
    result:=subrules[0]^.declaredAt;
  end;

PROCEDURE T_rule.setMemoized;
  begin
    if not(isCached) then begin
      isCached:=true;
      cache.create;
    end;
  end;

FUNCTION T_rule.hasPublicSubrule: boolean;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do if subrules[i]^.typ=srt_normal_public then exit(true);
    result:=false;
  end;

{$endif}
