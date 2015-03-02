{$ifdef include_interface}
  P_subrule=^T_subrule;
  T_subrule=object
    private
      declaredAt:T_tokenLocation;
      pattern:T_pattern;
      expression:array of record
                   parIdx:longint;
                   token:P_token;
                 end;
      isDeclaredInline:boolean;
      PROCEDURE updatePatternForInline;
      PROCEDURE embrace;
      FUNCTION needEmbrace:boolean;
    public
      CONSTRUCTOR create          (CONST pat:T_pattern; CONST rep:P_Token; CONST declAt:T_tokenLocation);
      CONSTRUCTOR createConstant  (CONST value:P_literal);
      CONSTRUCTOR createForEachBody(CONST parameterId:ansistring; CONST rep:P_Token);
      CONSTRUCTOR createFromInline(CONST rep:P_Token);
      CONSTRUCTOR createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
      DESTRUCTOR destroy;
      FUNCTION replaces(CONST param:P_listLiteral; OUT firstRep,lastRep:P_token):boolean;
      FUNCTION toString:ansistring;
      PROCEDURE performSet(CONST newValue:P_literal);
      FUNCTION directEvaluateNullary():P_literal;
      FUNCTION directEvaluateUnary(CONST x:P_literal):P_literal;
      PROCEDURE getLineNumbers(VAR collector:T_listOfIntegers);
  end;

{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_subrule.create(CONST pat:T_pattern; CONST rep:P_Token; CONST declAt:T_tokenLocation);
  VAR t:P_token;
  begin
    pattern:=pat;
    pattern.finalizeRefs;
    declaredAt:=declAt;
    isDeclaredInline:=false;
    setLength(expression,0);
    t:=rep;
    while t<>nil do begin
      setLength(expression,length(expression)+1);
      with expression[length(expression)-1] do begin
        token:=t;
        t:=t^.next;
        token^.next:=nil;
        if token^.tokType in [tt_identifier, tt_userRulePointer, tt_parameterIdentifier, tt_intrinsicRulePointer] then begin
          parIdx:=pattern.indexOfId(token^.txt);
          if parIdx>=0 then token^.tokType:=tt_identifier;
        end else parIdx:=-1;
      end;
    end;
    if needEmbrace then embrace;
  end;

CONSTRUCTOR T_subrule.createConstant  (CONST value:P_literal);
  begin
    pattern.create;
    declaredAt.package:=nil;
    isDeclaredInline:=true;
    setLength(expression,1);
    with expression[0] do begin
      token:=newToken(declaredAt,'',tt_literal,value);
      parIdx:=-1;
    end;
  end;

CONSTRUCTOR T_subrule.createForEachBody(CONST parameterId:ansistring; CONST rep:P_Token);
  VAR t:P_token;
  begin
    pattern.create;
    pattern.appendFreeId(parameterId);
    declaredAt:=rep^.location;
    isDeclaredInline:=true;
    setLength(expression,0);
    t:=rep;
    while t<>nil do begin
      setLength(expression,length(expression)+1);
      with expression[length(expression)-1] do begin
        token:=t;
        t:=t^.next;
        token^.next:=nil;
        if token^.tokType in [tt_identifier, tt_userRulePointer, tt_parameterIdentifier] then begin
          parIdx:=pattern.indexOfId(token^.txt);
          if parIdx>=0 then token^.tokType:=tt_identifier;
        end else parIdx:=-1;
      end;
    end;
    if needEmbrace then embrace;
  end;


PROCEDURE T_subrule.embrace;
  VAR i:longint;
  begin
    setLength(expression,length(expression)+2);
    for i:=length(expression)-2 downto 1 do expression[i]:=expression[i-1];
    with expression[0] do begin
      token:=newToken(declaredAt,'',tt_braceOpen);
      parIdx:=-1;
    end;
    with expression[length(expression)-1] do begin
      token:=newToken(declaredAt,'',tt_braceClose);
      parIdx:=-1;
    end;
  end;

FUNCTION T_subrule.needEmbrace:boolean;
  VAR bracketLevel,i:longint;
  begin
    if length(expression)<=1 then exit(false);
    bracketLevel:=0;
    for i:=0 to length(expression)-1 do case expression[i].token^.tokType of
      tt_set, tt_each, tt_braceOpen, tt_parList_constructor,tt_listBraceOpen,  tt_list_constructor, tt_expBraceOpen: inc(bracketLevel);
      tt_braceClose, tt_listBraceClose, tt_expBraceClose: dec(bracketLevel);
      tt_separatorComma, tt_separatorCnt,
      tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq,
      tt_operatorAnd, tt_operatorOr, tt_operatorXor,
      tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
      tt_operatorStrConcat,
      tt_operatorExtractL0, tt_operatorExtractL1, tt_operatorExtractL2, tt_operatorExtractL3,
      tt_operatorConcat, tt_operatorIn,
      tt_iifCheck, tt_iifElse,
      tt_typeCheckScalar,  tt_typeCheckList,
      tt_typeCheckBoolean, tt_typeCheckBoolList,
      tt_typeCheckInt,     tt_typeCheckIntList,
      tt_typeCheckReal,    tt_typeCheckRealList,
      tt_typeCheckString,  tt_typeCheckStringList,
      tt_typeCheckNumeric, tt_typeCheckNumList,
      tt_typeCheckExpression,
      tt_typeCheckNonemptyList,
      tt_typeCheckEmptyList: if bracketLevel=0 then exit(true);
    end;
    result:=false;
  end;

PROCEDURE T_subrule.updatePatternForInline;
  VAR i:longint;
  begin
    pattern.clear;
    for i:=0 to length(expression)-1 do with expression[i] do
    if token^.tokType=tt_parameterIdentifier then begin
      parIdx:=pattern.indexOfId(token^.txt); //reuse ID
      if parIdx<0 then parIdx:=pattern.appendFreeId(token^.txt); //new parameter
    end;
  end;

CONSTRUCTOR T_subrule.createFromInline(CONST rep:P_Token);
  VAR t:P_token;
      bracketLevel:longint=0;
  begin
    declaredAt:=rep^.location;
    pattern.create;
    isDeclaredInline:=true;
    setLength(expression,0);
    t:=rep;
    while (t<>nil) and ((t^.tokType<>tt_expBraceClose) or (bracketLevel>0)) do begin
      setLength(expression,length(expression)+1);
      with expression[length(expression)-1] do begin
        token:=t;
        case token^.tokType of
          tt_expBraceOpen: inc(bracketLevel);
          tt_expBraceClose: dec(bracketLevel);
        end;
        t:=t^.next;
        token^.next:=nil;
        parIdx:=-1;
      end;
    end;
    if needEmbrace then embrace;
    updatePatternForInline;
  end;

DESTRUCTOR T_subrule.destroy;
  VAR i:longint;
  begin
    declaredAt.package:=nil;
    pattern.destroy;
    for i:=0 to length(expression)-1 do disposeToken(expression[i].token);
    setLength(expression,0);
  end;

PROCEDURE disposeSubruleImpl (VAR p:pointer);
  begin
    dispose(P_subrule(p),destroy);
    p:=nil;
  end;

FUNCTION T_subrule.replaces(CONST param:P_listLiteral; OUT firstRep,lastRep:P_token):boolean;
  VAR t:P_token;
      i:longint;
  begin
    if (param=nil) and (length(pattern.sig)=0) or  pattern.matches(param^) then begin
      with expression[0] do if parIdx>=0 then begin
        firstRep:=newToken(token^.location,'',tt_literal,param^.value(parIdx));
        param^.value(parIdx)^.rereference;
      end else firstRep:=newToken(token);
      t:=firstRep;
      for i:=1 to length(expression)-1 do with expression[i] do begin
        if parIdx>=0 then begin
          t^.next:=newToken(token^.location,'',tt_literal,param^.value(parIdx));
          param^.value(parIdx)^.rereference;
        end else begin
          if (token^.tokType = tt_identifier) and (token^.data<>nil) then P_package(token^.data)^.resolveRuleId(token^,true);
          t^.next:=newToken(token);
        end;
        t:=t^.next;
      end;
      lastRep:=t;
      result:=true;
    end else if (param<>nil)
            and (length(expression)=1)
            and (expression[0].token^.tokType=tt_literal)
            and (P_literal(expression[0].token^.data)^.literalType = lt_expression)
            and (P_subrule(P_expressionLiteral(expression[0].token^.data)^.value)^.pattern.matches(param^))
    then result:=P_subrule(P_expressionLiteral(expression[0].token^.data)^.value)^.replaces(param,firstRep,lastRep)
    else begin
      result:=false;
      if isDeclaredInline then raiseError(el3_evalError,'Cannot evaluate inline function with the given number of parameters; Got '+intToStr(param^.size)+', expected '+intToStr(length(pattern.sig)),declaredAt);
    end;
  end;

CONSTRUCTOR T_subrule.createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
  VAR i,i0:longint;
      r:P_subrule;
  begin
    declaredAt:=opLocation;
    pattern.create;
    isDeclaredInline:=true;

    if LHS^.literalType=lt_expression then begin
      r:=P_expressionLiteral(LHS)^.value;
      setLength(expression,length(r^.expression));
      for i:=0 to length(expression)-1 do with expression[i] do begin
        token:=newToken(r^.expression[i].token);
        parIdx:=-1;
      end;
    end else begin
      setLength(expression,1);
      with expression[0] do begin
        token:=newToken(opLocation,'',tt_literal,LHS);
        parIdx:=-1;
      end;
    end;

    i0:=length(expression);
    setLength(expression,i0+1);
    with expression[i0] do begin
      token:=newToken(opLocation,'',op);
      parIdx:=-1;
    end;

    i0:=length(expression);
    if RHS^.literalType=lt_expression then begin
      r:=P_expressionLiteral(RHS)^.value;
      setLength(expression,i0+length(r^.expression));
      for i:=0 to length(r^.expression)-1 do with expression[i0+i] do begin
        token:=newToken(r^.expression[i].token);
        parIdx:=-1;
      end;
    end else begin
      setLength(expression,i0+1);
      with expression[i0] do begin
        token:=newToken(opLocation,'',tt_literal,RHS);
        parIdx:=-1;
      end;
    end;
    embrace;
    updatePatternForInline;
  end;

FUNCTION subruleApplyOpImpl (CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation):pointer;
  VAR newRule:P_subrule;
  begin
    new(newRule,createFromOp(LHS,op,RHS,tokenLocation));
    result:=newRule;
  end;

FUNCTION T_subrule.toString:ansistring;
  VAR i:longint;
  begin
    result:='';
    for i:=0 to length(expression)-1 do result:=result+expression[i].token^.toString;
    if isDeclaredInline then result:='{'+result+'}'
    else begin
      result:=pattern.toString+result;
    end;
  end;

FUNCTION subruleToStringImpl(CONST p:pointer):string;
  begin
    result:=P_subrule(p)^.toString;
  end;

PROCEDURE T_subrule.performSet(CONST newValue:P_literal);
  begin
    if (length(expression)=1) and (expression[0].token^.tokType=tt_literal) then begin
      disposeLiteral(expression[0].token^.data);
      expression[0].token^.data:=newValue;
    end else raiseError(el3_evalError,'Cannot process set-operation',declaredAt);
  end;

FUNCTION T_subrule.directEvaluateNullary():P_literal;
  VAR firstRep,lastRep:P_token;
  begin
    if replaces(nil,firstRep,lastRep) then begin
      reduceExpression(firstRep);
      if (firstRep<>nil) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal)
      then begin
        result:=firstRep^.data;
        result^.rereference;
      end else begin
        result:=nil;
        raiseError(el3_evalError,'Cannot directly resolve nullary call. Make sure the function can be resolved without errors, returning only a literal.',declaredAt);
      end;
      cascadeDisposeToken(firstRep);
    end else result:=nil;
  end;

FUNCTION evaluateNullary(CONST subrulePointer:pointer):P_literal;
  begin
    result:=P_subrule(subrulePointer)^.directEvaluateNullary;
  end;

FUNCTION T_subrule.directEvaluateUnary(CONST x:P_literal):P_literal;
  VAR firstRep,lastRep:P_token;
      param:P_listLiteral;
  begin
    param:=newOneElementListLiteral(x,true);
    if replaces(param,firstRep,lastRep) then begin
      reduceExpression(firstRep);
      if (firstRep<>nil) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal)
      then begin
        result:=firstRep^.data;
        result^.rereference;
      end else begin
        result:=nil;
        raiseError(el3_evalError,'Cannot directly resolve unary call with parameter '+x^.toString+'. Make sure the function can be resolved without errors, returning only a literal.',declaredAt);
      end;
      cascadeDisposeToken(firstRep);
    end else result:=nil;
    disposeLiteral(param);
  end;
  
PROCEDURE T_subrule.getLineNumbers(VAR collector:T_listOfIntegers);
  VAR i:longint;
  begin
    result.add(declaredAt.line);
    for i:=0 to length(expression)-1 do with expression[i].token^.location do 
      if package<>nil then result.add(line);
    result.unique;
  end;
{$endif}
