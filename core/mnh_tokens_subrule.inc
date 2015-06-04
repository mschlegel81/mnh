{$ifdef include_interface}
  T_subruleType=(srt_normal_public,
                 srt_normal_private,
                 srt_inline_for_literal,
                 srt_inline_for_each);
  P_subrule=^T_subrule;
  T_subrule=object
    private
      called:boolean;
      typ:T_subruleType;
      declaredAt:T_tokenLocation;
      pattern:T_pattern;
      expression:array of record
                   parIdx:longint;
                   token:T_token;
                 end;

      PROCEDURE updatePatternForInline;
      PROCEDURE embrace;
      FUNCTION needEmbrace:boolean;
      FUNCTION canUnbrace(CONST outerOp:T_tokenType):boolean;
      PROCEDURE constructExpression(CONST rep:P_token; VAR recycler:T_tokenRecycler);
    public
      CONSTRUCTOR create           (CONST pat:T_pattern; CONST rep:P_Token; CONST declAt:T_tokenLocation; CONST isPrivate:boolean; VAR recycler:T_tokenRecycler);
      CONSTRUCTOR createForEachBody(CONST parameterId:ansistring; CONST rep:P_Token; VAR recycler:T_tokenRecycler);
      CONSTRUCTOR createFromInline (CONST rep:P_Token; VAR recycler:T_tokenRecycler);
      CONSTRUCTOR createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
      CONSTRUCTOR createFromInlineWithOp(CONST original:P_expressionLiteral; CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation);
      DESTRUCTOR destroy;
      FUNCTION replaces(CONST param:P_listLiteral; CONST selfLiteral:P_expressionLiteral; OUT firstRep,lastRep:P_token; VAR recycler:T_tokenRecycler):boolean;
      FUNCTION toString:ansistring;
      FUNCTION directEvaluateNullary(CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR recycler:T_tokenRecycler):P_literal;
      FUNCTION directEvaluateUnary(CONST x:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR recycler:T_tokenRecycler):P_literal;
      FUNCTION directEvaluateComparator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR recycler:T_tokenRecycler):boolean;
      FUNCTION directEvaluateAggregator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR recycler:T_tokenRecycler):P_literal;
      FUNCTION getInlineValue:P_literal;
      PROCEDURE complainAboutUncalled(CONST ruleId:ansistring);
  end;

{$endif}
{$ifdef include_implementation}
PROCEDURE T_subrule.constructExpression(CONST rep:P_token; VAR recycler:T_tokenRecycler);
  VAR t:P_token;
      inlineLiteral:P_literal;
  begin
    setLength(expression,0);
    t:=rep;
    while t<>nil do begin
      setLength(expression,length(expression)+1);
      with expression[length(expression)-1] do begin
        token:=t^;
        t^.tokType:=tt_eol; t:=recycler.disposeToken(t);
        token.next:=nil;
        if token.tokType=tt_optionalParameters then begin
          parIdx:=REMAINING_PARAMETERS_IDX;
        end else if token.tokType in [tt_identifier, tt_localUserRulePointer, tt_importedUserRulePointer, tt_parameterIdentifier, tt_intrinsicRulePointer] then begin
          parIdx:=pattern.indexOfId(token.txt);
          if parIdx>=0 then begin
            if parIdx>=REMAINING_PARAMETERS_IDX
            then token.tokType:=tt_parameterIdentifier
            else token.tokType:=tt_identifier;
          end else begin
            if (token.tokType = tt_identifier) and (token.data<>nil) then P_package(token.data)^.resolveRuleId(token,true);
            if (token.tokType in [tt_localUserRulePointer,tt_importedUserRulePointer]) and
               ((t=nil) or not(t^.tokType in [tt_braceOpen,tt_parList_constructor,tt_parList,tt_listToParameterList])) //rule must not be followed by an opening bracket, indicating a call with parameters
            then begin
              inlineLiteral:=P_rule(token.data)^.getInlineValue;
              if inlineLiteral<>nil then begin
                token.data:=inlineLiteral;
                token.txt:='';
                token.tokType:=tt_literal;
              end;
            end;
          end;
        end else parIdx:=-1;
      end;
    end;
  end;

CONSTRUCTOR T_subrule.create(CONST pat:T_pattern; CONST rep:P_Token; CONST declAt:T_tokenLocation; CONST isPrivate:boolean; VAR recycler:T_tokenRecycler);
  begin
    called:=false;
    if isPrivate then typ:=srt_normal_private
                 else typ:=srt_normal_public;
    pattern:=pat;
    pattern.finalizeRefs;
    declaredAt:=declAt;

    constructExpression(rep,recycler);
    if needEmbrace then embrace;
  end;

CONSTRUCTOR T_subrule.createForEachBody(CONST parameterId:ansistring; CONST rep:P_Token; VAR recycler:T_tokenRecycler);
  begin
    called:=false;
    typ:=srt_inline_for_each;
    pattern.create;
    pattern.appendFreeId(parameterId);
    declaredAt:=rep^.location;
    constructExpression(rep,recycler);
    if needEmbrace then embrace;
  end;


PROCEDURE T_subrule.embrace;
  VAR i:longint;
  begin
    setLength(expression,length(expression)+2);
    for i:=length(expression)-2 downto 1 do expression[i]:=expression[i-1];
    with expression[0] do begin
      token.create; token.define(declaredAt,'',tt_braceOpen);
      parIdx:=-1;
    end;
    with expression[length(expression)-1] do begin
      token.create; token.define(declaredAt,'',tt_braceClose);
      parIdx:=-1;
    end;
  end;

FUNCTION T_subrule.needEmbrace:boolean;
  VAR bracketStack:array of longint;
  PROCEDURE push(CONST index:longint);
    begin
      setLength(bracketStack,length(bracketStack)+1);
      bracketStack[length(bracketStack)-1]:=index;
    end;

  PROCEDURE pop(CONST index:longint);
    FUNCTION compatible(CONST oT,cT:T_tokenType):boolean;
      begin
        result:=(oT in [tt_each, tt_parallelEach, tt_braceOpen, tt_parList_constructor]) and (ct=tt_braceClose)
             or (oT in [tt_listBraceOpen, tt_list_constructor]) and (ct=tt_listBraceClose)
             or (oT=tt_expBraceOpen) and (ct=tt_expBraceClose)
             or (ot=tt_procedureBlockBegin) and (ct=tt_procedureBlockEnd);
      end;

    begin
      if length(bracketStack)=0 then begin
        raiseError(el4_parsingError,'Unbalanced paranthesis: missing opening bracket',expression[index].token.location);
        exit;
      end;
      if not(compatible(expression[bracketStack[length(bracketStack)-1]].token.tokType,
                        expression[index                               ].token.tokType)) then begin
        raiseError(el4_parsingError,'Unbalanced paranthesis: bracket mismatch (open)' ,expression[bracketStack[length(bracketStack)-1]].token.location);
        raiseError(el4_parsingError,'Unbalanced paranthesis: bracket mismatch (close)',expression[index                               ].token.location);
      end;
      setLength(bracketStack,length(bracketStack)-1);
    end;

  PROCEDURE checkStackAtEnd;
    VAR i:longint;
    begin
      if length(bracketStack)>0 then for i:=0 to length(bracketStack)-1 do
        raiseError(el4_parsingError,'Unbalanced paranthesis: missing closing bracket',expression[bracketStack[i]].token.location);
      setLength(bracketStack,0);
    end;

  VAR i:longint;
  begin
    setLength(bracketStack,0);
    if length(expression)<=1 then exit(false);

    result:=false;
    for i:=0 to length(expression)-1 do case expression[i].token.tokType of
      tt_each, tt_parallelEach, tt_braceOpen, tt_parList_constructor,tt_listBraceOpen,  tt_list_constructor, tt_expBraceOpen,tt_procedureBlockBegin: push(i);
      tt_braceClose, tt_listBraceClose, tt_expBraceClose,tt_procedureBlockEnd: pop(i);

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
      tt_typeCheckEmptyList,
      tt_typeCheckKeyValueList: if length(bracketStack)=0 then result:=true;
    end;
    checkStackAtEnd;
  end;

FUNCTION T_subrule.canUnbrace(CONST outerOp:T_tokenType):boolean;
  VAR bracketLevel,i:longint;
  begin
    if length(expression)<=1 then exit(false);
    bracketLevel:=0;
    i:=length(expression)-1;
    if (expression[0].token.tokType<>tt_braceOpen) or
       (expression[i].token.tokType<>tt_braceClose) then exit(false);
    for i:=1 to length(expression)-2 do begin
      case expression[i].token.tokType of
        tt_each, tt_parallelEach, tt_braceOpen, tt_parList_constructor,tt_listBraceOpen,  tt_list_constructor, tt_expBraceOpen: inc(bracketLevel);
        tt_braceClose, tt_listBraceClose, tt_expBraceClose: dec(bracketLevel);
        tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq,
        tt_operatorAnd, tt_operatorOr, tt_operatorXor,
        tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
        tt_operatorStrConcat,
        tt_operatorExtractL0, tt_operatorExtractL1, tt_operatorExtractL2, tt_operatorExtractL3,
        tt_operatorConcat, tt_operatorIn: if C_opPrecedence[expression[i].token.tokType]>C_opPrecedence[outerOp] then exit(false);
      end;
      if bracketLevel<0 then exit(false);
    end;
    result:=true;
  end;

PROCEDURE T_subrule.updatePatternForInline;
  VAR i:longint;
  begin
    pattern.clear;
    for i:=0 to length(expression)-1 do with expression[i] do
    if token.tokType=tt_parameterIdentifier then begin
      parIdx:=pattern.indexOfId(token.txt); //reuse ID
      if parIdx<0 then parIdx:=pattern.appendFreeId(token.txt) //new parameter
      else if parIdx=SELF_TOKEN_PAR_IDX then pattern.containsSelfPointer:=true
      else if parIdx=REMAINING_PARAMETERS_IDX then pattern.hasOptionals:=true;
    end else if token.tokType=tt_optionalParameters then begin
      parIdx:=REMAINING_PARAMETERS_IDX;
      pattern.hasOptionals:=true;
    end;
  end;

CONSTRUCTOR T_subrule.createFromInline(CONST rep:P_Token; VAR recycler:T_tokenRecycler);
  VAR t:P_token;
  begin
    called:=false;
    typ:=srt_inline_for_literal;
    declaredAt:=rep^.location;
    pattern.create;

    setLength(expression,0);

    t:=rep;
    while (t<>nil) do begin
      setLength(expression,length(expression)+1);
      with expression[length(expression)-1] do begin
        token:=t^;
        t^.tokType:=tt_eol;
        t:=recycler.disposeToken(t);
        token.next:=nil;
        parIdx:=-1;
      end;
    end;
    if needEmbrace then embrace;
    updatePatternForInline;
  end;

DESTRUCTOR T_subrule.destroy;
  VAR i:longint;
  begin
    declaredAt:=C_nilTokenLocation;
    pattern.destroy;
    for i:=0 to length(expression)-1 do expression[i].token.destroy;
    setLength(expression,0);
  end;

PROCEDURE disposeSubruleImpl (VAR p:pointer);
  begin
    dispose(P_subrule(p),destroy);
    p:=nil;
  end;

FUNCTION T_subrule.replaces(CONST param:P_listLiteral; CONST selfLiteral:P_expressionLiteral; OUT firstRep,lastRep:P_token; VAR recycler:T_tokenRecycler):boolean;
  VAR t:P_token;
      i:longint;
      remaining:P_listLiteral=nil;

  FUNCTION tokenFromLiteral(VAR original:T_token; CONST index:longint):P_token; inline;
    VAR L:P_literal;
    begin
      if index>=0 then begin
        if index=ALL_PARAMETERS_PAR_IDX then L:=param
        else if index=SELF_TOKEN_PAR_IDX then L:=selfLiteral
        else if index=REMAINING_PARAMETERS_IDX then begin
          if remaining=nil then begin
            if param=nil
            then remaining:=newListLiteral
            else remaining:=param^.tail(length(pattern.sig));
            remaining^.unreference;
          end;
          L:=remaining;
        end else L:=param^.value(index);
        if L=nil then exit(recycler.newToken(@original));
        result:=recycler.newToken(original.location,'',tt_literal,L);
        L^.rereference;
      end else begin
        if (original.tokType=tt_identifier) and (original.data<>nil) then P_package(original.data)^.resolveRuleId(original,true);
        result:=recycler.newToken(@original);
      end;
    end;

  begin
    if (param= nil) and pattern.matchesNilPattern or
       (param<>nil) and pattern.matches(param^,selfLiteral<>nil) then begin
      called:=true;
      with expression[0] do firstRep:=tokenFromLiteral(token,parIdx);
      t:=firstRep;
      for i:=1 to length(expression)-1 do with expression[i] do begin
        t^.next:=tokenFromLiteral(token,parIdx);
        t:=t^.next;
      end;
      lastRep:=t;
      result:=true;
    end else if (param<>nil)
            and (length(expression)=1)
            and (expression[0].token.tokType=tt_literal)
            and (P_literal(expression[0].token.data)^.literalType=lt_expression)
            and (P_subrule(P_expressionLiteral(expression[0].token.data)^.value)^.pattern.matches(param^,true))
    then begin
      result:=P_subrule(P_expressionLiteral(expression[0].token.data)^.value)^.replaces(param,P_expressionLiteral(expression[0].token.data),firstRep,lastRep,recycler);
      called:=called or result;
    end else begin
      result:=false;
      case typ of
        srt_inline_for_each: begin
          if param=nil then raiseError(el3_evalError,'Cannot evaluate each body with the given number of parameters; Got none, expected '+intToStr(length(pattern.sig)),declaredAt)
                       else raiseError(el3_evalError,'Cannot evaluate each body with the given number of parameters; Got '+intToStr(param^.size)+', expected '+intToStr(length(pattern.sig)),declaredAt);
        end;
        srt_inline_for_literal: begin
          if param=nil then raiseError(el3_evalError,'Cannot evaluate inline function '+toString+' with the given number of parameters; Got none, expected '+intToStr(length(pattern.sig)),declaredAt)
                       else raiseError(el3_evalError,'Cannot evaluate inline function '+toString+' with the given number of parameters; Got '+intToStr(param^.size)+', expected '+intToStr(length(pattern.sig)),declaredAt);
        end;
      end;
    end;
  end;

CONSTRUCTOR T_subrule.createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
  VAR i:longint;
      r:P_subrule;
  PROCEDURE appendToExpression(VAR T:T_token);
    begin
      setLength(expression,length(expression)+1);
      with expression[length(expression)-1] do begin
        token.create;
        token.define(T);
        parIdx:=-1;
      end;
    end;

  PROCEDURE appendToExpression(CONST L:P_literal);
    begin
      setLength(expression,length(expression)+1);
      L^.rereference;
      with expression[length(expression)-1] do begin
        token.create;
        token.define(opLocation,'',tt_literal,L);
        parIdx:=-1;
      end;
    end;

  PROCEDURE appendToExpression(CONST op:T_tokenType);
    begin
      setLength(expression,length(expression)+1);
      with expression[length(expression)-1] do begin
        token.create;
        token.define(opLocation,'',op);
        parIdx:=-1;
      end;
    end;

  begin
    called:=false;
    declaredAt:=opLocation;
    pattern.create;
    typ:=srt_inline_for_literal;
    setLength(expression,0);

    if LHS^.literalType=lt_expression then begin
      r:=P_expressionLiteral(LHS)^.value;
      if r^.canUnbrace(op) then for i:=1 to length(r^.expression)-2 do appendToExpression(r^.expression[i].token)
                           else for i:=0 to length(r^.expression)-1 do appendToExpression(r^.expression[i].token);
    end else appendToExpression(LHS);
    appendToExpression(op);
    if RHS^.literalType=lt_expression then begin
      r:=P_expressionLiteral(RHS)^.value;
      if r^.canUnbrace(op) then for i:=1 to length(r^.expression)-2 do appendToExpression(r^.expression[i].token)
                           else for i:=0 to length(r^.expression)-1 do appendToExpression(r^.expression[i].token);
    end else appendToExpression(RHS);
    embrace;
    updatePatternForInline;
  end;

FUNCTION subruleApplyOpImpl (CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation):pointer;
  VAR newRule:P_subrule;
  begin
    new(newRule,createFromOp(LHS,op,RHS,tokenLocation));
    result:=newRule;
  end;

CONSTRUCTOR T_subrule.createFromInlineWithOp(CONST original:P_expressionLiteral; CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation);
  VAR origRule:P_subrule;
      i:longint;
  PROCEDURE appendToExpression(VAR T:T_token);
    begin
      setLength(expression,length(expression)+1);
      with expression[length(expression)-1] do begin
        token.create;
        token.define(T);
        parIdx:=-1;
      end;
    end;

  PROCEDURE appendToExpression(CONST op:T_tokenType);
    begin
      setLength(expression,length(expression)+1);
      with expression[length(expression)-1] do begin
        token.create;
        token.define(funcLocation,'',op);
        parIdx:=-1;
      end;
    end;

  begin
    called:=false;
    origRule:=original^.value;
    declaredAt:=funcLocation;
    pattern.create;
    typ:=srt_inline_for_literal;

    setLength(expression,1);
    with expression[0] do begin token.create; token.define(declaredAt,intrinsicRuleId,tt_intrinsicRulePointer,intrinsicRuleMap.get(intrinsicRuleId)); parIdx:=-1; end;
    if not(origRule^.canUnbrace(tt_unaryOpPlus)) then appendToExpression(tt_braceOpen);
    for i:=0 to length(origRule^.expression)-1 do appendToExpression(origRule^.expression[i].token);
    if not(origRule^.canUnbrace(tt_unaryOpPlus)) then appendToExpression(tt_braceClose);
    updatePatternForInline;
  end;

FUNCTION subruleApplyFuncImpl(CONST original:P_expressionLiteral; CONST intrinsicRuleId:ansistring; CONST funcLocation:T_tokenLocation):P_expressionLiteral;
  VAR newRule:P_subrule;
  begin
    new(newRule,createFromInlineWithOp(original,intrinsicRuleId,funcLocation));
    result:=newExpressionLiteral(newRule);
  end;

FUNCTION T_subrule.toString:ansistring;
  VAR i:longint;
      prevIdLike,idLike:boolean;
  begin
    prevIdLike:=false;
    result:='';
    for i:=0 to length(expression)-1 do begin
      result:=result+expression[i].token.toString(prevIdLike,idLike);
      prevIdLike:=idLike;
    end;
    if typ=srt_inline_for_literal then result:='{'+result+'}'
    else begin
      result:=pattern.toString+result;
    end;
  end;

FUNCTION subruleToStringImpl(CONST p:pointer):string;
  begin
    result:=P_subrule(p)^.toString;
  end;

FUNCTION T_subrule.directEvaluateNullary(CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR recycler:T_tokenRecycler):P_literal;
  VAR firstRep,lastRep:P_token;
  begin
    if replaces(nil,selfLiteral,firstRep,lastRep,recycler) then begin
      reduceExpression(firstRep,callDepth+1,recycler);
      if (errorLevel<el3_evalError) and (firstRep<>nil) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal)
      then begin
        result:=firstRep^.data;
        result^.rereference;
      end else begin
        result:=nil;
        raiseError(el3_evalError,'Cannot directly resolve nullary call. Make sure the function can be resolved without errors, returning only a literal.',declaredAt);
      end;
      recycler.cascadeDisposeToken(firstRep);
    end else result:=nil;
  end;

FUNCTION T_subrule.directEvaluateUnary(CONST x:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR recycler:T_tokenRecycler):P_literal;
  VAR firstRep,lastRep:P_token;
      param:T_listLiteral;
  begin
    param.create;
    param.append(x,true);
    if replaces(@param,selfLiteral,firstRep,lastRep,recycler) then begin
      reduceExpression(firstRep,callDepth+1,recycler);
      if (errorLevel<el3_evalError) and (firstRep<>nil) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal)
      then begin
        result:=firstRep^.data;
        result^.rereference;
      end else begin
        result:=nil;
        raiseError(el3_evalError,'Cannot directly resolve unary call with parameter '+x^.toString+'. Make sure the function can be resolved without errors, returning only a literal.',declaredAt);
      end;
      recycler.cascadeDisposeToken(firstRep);
    end else result:=nil;
    param.destroy;
  end;

FUNCTION T_subrule.directEvaluateComparator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR recycler:T_tokenRecycler):boolean;
  VAR firstRep,lastRep:P_token;
      param:T_listLiteral;
  begin
    param.create;
    param.append(x,true);
    param.append(y,true);
    if replaces(@param,selfLiteral,firstRep,lastRep,recycler) then begin
      reduceExpression(firstRep,callDepth+1,recycler);
      if (errorLevel<el3_evalError) and (firstRep<>nil) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) and (P_literal(firstRep^.data)^.literalType=lt_boolean)
      then begin
        result:=P_boolLiteral(firstRep^.data)^.value;
      end else begin
        result:=false;
        raiseError(el3_evalError,'Cannot directly resolve comparator call with parameters ('+x^.toString+','+y^.toString+'). Make sure the function can be resolved without errors, returning only a boolean literal.',declaredAt);
      end;
      recycler.cascadeDisposeToken(firstRep);
    end else result:=false;
    param.destroy;
  end;

FUNCTION T_subrule.directEvaluateAggregator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR recycler:T_tokenRecycler):P_literal;
  VAR firstRep,lastRep:P_token;
      param:T_listLiteral;
  begin
    param.create;
    param.append(x,true);
    param.append(y,true);
    if replaces(@param,selfLiteral,firstRep,lastRep,recycler) then begin
      reduceExpression(firstRep,callDepth+1,recycler);
      if (errorLevel<el3_evalError) and (firstRep<>nil) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal)
      then begin
        result:=firstRep^.data;
        result^.rereference;
      end else begin
        result:=nil;
        raiseError(el3_evalError,'Cannot directly resolve aggregator call with parameters ('+x^.toString+','+y^.toString+'). Make sure the function can be resolved without errors, returning only a literal.',declaredAt);
      end;
      recycler.cascadeDisposeToken(firstRep);
    end else result:=nil;
    param.destroy;
  end;

FUNCTION evaluateComparator(CONST subruleLiteral:P_expressionLiteral; CONST LHSComparand,RHScomparand:P_literal):boolean;
  VAR tempRecycler:T_tokenRecycler;
  begin
    tempRecycler.create;
    result:=P_subrule(subruleLiteral^.value)^.directEvaluateComparator(LHSComparand,RHScomparand,subruleLiteral,0,tempRecycler);
    tempRecycler.destroy;
  end;


FUNCTION T_subrule.getInlineValue:P_literal;
  begin
    if (length(expression)<>1) or (typ in [srt_inline_for_each,srt_inline_for_literal]) then exit(nil);
    with expression[0] do if token.tokType=tt_literal then begin
      called:=true;
      result:=token.data;
      result^.rereference;
    end else result:=nil;
  end;

PROCEDURE T_subrule.complainAboutUncalled(CONST ruleId:ansistring);
  begin
    if (typ=srt_normal_private) and not(called) then raiseError(el2_warning,'Unused private subrule '+ruleId+pattern.toString,declaredAt);
  end;

{$endif}
