{$ifdef include_interface}
  P_rule=^T_rule;
  T_rule=object
    private
      id:string;
      subrules:array of P_subrule;
    public
      CONSTRUCTOR create(CONST ruleId:string);
      DESTRUCTOR destroy;
      FUNCTION replaces(CONST param:P_listLiteral; OUT firstRep,lastRep:P_Token):boolean;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subrule);
      PROCEDURE addOrSetSubRule(CONST rule:P_subrule);
      PROCEDURE performSet(CONST value:P_literal);
      FUNCTION getLineNumbers:T_listOfIntegers;
  end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_rule.create(CONST ruleId:string);
  begin
    id:=ruleId;
    setLength(subrules,0);
  end;

DESTRUCTOR T_rule.destroy;
  VAR i:longint;
  begin
    id:='';
    for i:=0 to length(subrules)-1 do dispose(subrules[i],destroy);
    setLength(subrules,0);
  end;

FUNCTION T_rule.replaces(CONST param:P_listLiteral; OUT firstRep,lastRep:P_Token):boolean;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do if subrules[i]^.replaces(param,firstRep,lastRep) then exit(true);
    result:=false;
  end;

PROCEDURE T_rule.addOrReplaceSubRule(CONST rule:P_subrule);
  VAR i:longint;
  begin
    i:=0;
    while (i<length(subrules)) and not(rule^.pattern.isEquivalent(subrules[i]^.pattern)) do inc(i);
    if i>=length(subrules) then setLength(subrules,i+1)
                           else dispose(subrules[i],destroy);
    subrules[i]:=rule;
  end;

PROCEDURE T_rule.addOrSetSubRule(CONST rule:P_subrule);
  VAR i:longint;
  begin
    i:=0;
    while (i<length(subrules)) and not(rule^.pattern.isEquivalent(subrules[i]^.pattern)) do inc(i);
    if i>=length(subrules) then setLength(subrules,i+1);
    subrules[i]:=rule;
  end;

PROCEDURE T_rule.performSet(CONST value:P_literal);
  VAR i:longint;
  begin
    i:=0;
    while (i<length(subrules)) and (length(subrules[i]^.pattern.sig)>0) do inc(i);
    if i<length(subrules) then begin
      subrules[i]^.performSet(value);
    end else begin
      setLength(subrules,i+1);
      new(subRules[i],createConstant(value));
    end;
    value^.rereference;
  end;
  
FUNCTION T_rule.getLineNumbers:T_listOfIntegers;
  VAR i:longint;
  begin
    result.create;
    for i:=0 to length(subrules)-1 do subRules[i]^.getLineNumbers(result);
  end;
{$endif}
