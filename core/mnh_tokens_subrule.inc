{$ifdef include_interface}
  T_subruleType=(srt_normal_public,
                 srt_normal_private,
                 srt_inline_for_literal,
                 srt_inline_for_each);
  T_preparedToken=record
    parIdx:longint;
    token:T_token;
  end;

  P_subrule=^T_subrule;
  T_subrule=object
    private
      comment:ansistring;
      called:boolean;
      typ:T_subruleType;
      declaredAt:T_tokenLocation;
      pattern:T_pattern;
      preparedBody:array of T_preparedToken;

      PROCEDURE updatePatternForInline;
      PROCEDURE embrace;
      FUNCTION needEmbrace:boolean;
      FUNCTION canUnbrace(CONST outerPrecedence:longint):boolean;
      PROCEDURE constructExpression(CONST rep:P_token; VAR context:T_evaluationContext; CONST forEachBody:boolean);
    public
      CONSTRUCTOR create           (CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate:boolean; VAR context:T_evaluationContext);
      CONSTRUCTOR createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; VAR context:T_evaluationContext);
      CONSTRUCTOR createFromInline (CONST rep:P_token; VAR context:T_evaluationContext);
      CONSTRUCTOR createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
      CONSTRUCTOR createFromInlineWithOp(CONST original:P_expressionLiteral; CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation);
      CONSTRUCTOR createPrimitiveAggregator(CONST tok:P_token; VAR context:T_evaluationContext);
      CONSTRUCTOR clone(CONST original:P_subrule);
      DESTRUCTOR destroy;
      FUNCTION replaces(CONST param:P_listLiteral; CONST selfLiteral:P_expressionLiteral; OUT firstRep,lastRep:P_token; VAR context:T_evaluationContext):boolean;
      FUNCTION toString:ansistring;
      FUNCTION directEvaluate(CONST selfLiteral:P_expressionLiteral; CONST params:P_listLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
      FUNCTION directEvaluateNullary(CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
      FUNCTION directEvaluateUnary(CONST x:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
      FUNCTION directEvaluateComparator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):boolean;
      FUNCTION directEvaluateAggregator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
      FUNCTION getInlineValue:P_literal;
      FUNCTION complainAboutUncalled(CONST ruleId:ansistring; CONST inMainPackage:boolean; VAR adapters:T_adapters):boolean;
      FUNCTION getDocTxt(CONST ruleId:ansistring):ansistring;
      FUNCTION getDocHtml(CONST ruleId:ansistring):ansistring;
      FUNCTION arity:longint;
  end;

{$endif}
{$ifdef include_implementation}
PROCEDURE T_subrule.constructExpression(CONST rep:P_token; VAR context:T_evaluationContext; CONST forEachBody:boolean);
  VAR t:P_token;
      inlineLiteral:P_literal;
  begin
    setLength(preparedBody,0);
    t:=rep;
    while t<>nil do begin
      setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[length(preparedBody)-1] do begin
        token:=t^;
        t^.tokType:=tt_EOL; t:=context.disposeToken(t);
        token.next:=nil;
        if token.tokType=tt_optionalParameters then begin
          parIdx:=REMAINING_PARAMETERS_IDX;
        end else if token.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_parameterIdentifier, tt_intrinsicRule] then begin
          parIdx:=pattern.indexOfId(token.txt);
          if parIdx>=0 then begin
            if parIdx>=REMAINING_PARAMETERS_IDX
            then token.tokType:=tt_parameterIdentifier
            else token.tokType:=tt_identifier;
          end else if forEachBody and (token.txt=C_eachIndexIdentifier) then token.tokType:=tt_blockLocalVariable
          else begin
            if (token.tokType in [tt_identifier,tt_identifier_pon]) and (token.data<>nil) then P_package(token.data)^.resolveRuleId(token,nil);
            if (token.tokType in [tt_localUserRule,tt_importedUserRule]) and
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

CONSTRUCTOR T_subrule.create(CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate:boolean; VAR context:T_evaluationContext);
  begin
    called:=false;
    if isPrivate then typ:=srt_normal_private
                 else typ:=srt_normal_public;
    pattern:=pat;
    declaredAt:=declAt;
    constructExpression(rep,context,false);
    if needEmbrace then embrace;
  end;

CONSTRUCTOR T_subrule.createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; VAR context:T_evaluationContext);
  begin
    called:=false;
    typ:=srt_inline_for_each;
    pattern.create;
    pattern.appendFreeId(parameterId);
    declaredAt:=rep^.location;
    constructExpression(rep,context,true);
  end;


PROCEDURE T_subrule.embrace;
  VAR i:longint;
  begin
    setLength(preparedBody,length(preparedBody)+2);
    for i:=length(preparedBody)-2 downto 1 do preparedBody[i]:=preparedBody[i-1];
    with preparedBody[0] do begin
      token.create; token.define(declaredAt,'',tt_braceOpen);
      parIdx:=-1;
    end;
    with preparedBody[length(preparedBody)-1] do begin
      token.create; token.define(declaredAt,'',tt_braceClose);
      parIdx:=-1;
    end;
  end;

FUNCTION T_subrule.needEmbrace:boolean;
  VAR i:longint;
      level:longint=0;
  begin
    if length(preparedBody)<=1 then exit(false);
    result:=false;
    for i:=0 to length(preparedBody)-1 do with preparedBody[i].token do begin
      if      tokType in C_openingBrackets then inc(level)
      else if tokType in C_closingBrackets then dec(level)
      else if (tokType in[      tt_separatorComma, tt_separatorCnt,
      tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq,
      tt_operatorAnd, tt_operatorOr, tt_operatorXor,
      tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
      tt_operatorStrConcat,
      tt_operatorConcat, tt_operatorIn,
      tt_iifCheck, tt_iifElse,
      tt_typeCheckScalar,  tt_typeCheckList,
      tt_typeCheckBoolean, tt_typeCheckBoolList,
      tt_typeCheckInt,     tt_typeCheckIntList,
      tt_typeCheckReal,    tt_typeCheckRealList,
      tt_typeCheckString,  tt_typeCheckStringList,
      tt_typeCheckNumeric, tt_typeCheckNumList,
      tt_typeCheckExpression,
      tt_typeCheckKeyValueList]) and (level=0) then exit(true);
    end;
  end;

FUNCTION T_subrule.canUnbrace(CONST outerPrecedence:longint):boolean;
  VAR i:longint;
      level:longint=0;
  begin
    if length(preparedBody)<=1 then exit(false);
    level:=0;
    i:=length(preparedBody)-1;
    if (preparedBody[0].token.tokType<>tt_braceOpen) or
       (preparedBody[i].token.tokType<>tt_braceClose) then exit(false);
    for i:=1 to length(preparedBody)-2 do with preparedBody[i].token do begin
      if tokType in C_openingBrackets then inc(level)
      else if tokType in C_closingBrackets then dec(level)
      else if (tokType in C_operatorsAndComparators) and (level=0) and (C_opPrecedence[preparedBody[i].token.tokType]>outerPrecedence) then exit(false);
    end;
    result:=true;
  end;

PROCEDURE T_subrule.updatePatternForInline;
  VAR i:longint;
  begin
    pattern.clear;
    for i:=0 to length(preparedBody)-1 do with preparedBody[i] do
    if token.tokType=tt_parameterIdentifier then begin
      parIdx:=pattern.indexOfIdForInline(token.txt);
    end else if token.tokType=tt_optionalParameters then begin
      parIdx:=REMAINING_PARAMETERS_IDX;
      pattern.hasOptionals:=true;
    end;
  end;

CONSTRUCTOR T_subrule.createFromInline(CONST rep:P_token; VAR context:T_evaluationContext);
  VAR t:P_token;
  begin
    called:=false;
    typ:=srt_inline_for_literal;
    declaredAt:=rep^.location;
    pattern.create;

    setLength(preparedBody,0);

    t:=rep;
    while (t<>nil) do begin
      setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[length(preparedBody)-1] do begin
        token:=t^;
        t^.tokType:=tt_EOL;
        t:=context.disposeToken(t);
        token.next:=nil;
        parIdx:=-1;
      end;
    end;
    if needEmbrace then embrace;
    updatePatternForInline;
  end;

CONSTRUCTOR T_subrule.createPrimitiveAggregator(CONST tok:P_token; VAR context:T_evaluationContext);
  VAR dummyBool:boolean;
      funcPtr:T_intFuncCallback=nil;
  begin
    comment:='';
    called:=false;
    typ:=srt_inline_for_literal;
    declaredAt:=tok^.location;
    pattern.create;
    setLength(preparedBody,0);
    if tok^.tokType in C_operatorsForAggregators then begin
      pattern.appendFreeId('$x');
      pattern.appendFreeId('$y');
      setLength(preparedBody,5);
      with preparedBody[0] do begin token.create; token.define(tok^.location,'',tt_braceOpen);             parIdx:=-1; end;
      with preparedBody[1] do begin token.create; token.define(tok^.location,'$x',tt_parameterIdentifier); parIdx:= 0; end;
      with preparedBody[2] do begin token.create; token.define(tok^.location,'',tok^.tokType);             parIdx:=-1; end;
      with preparedBody[3] do begin token.create; token.define(tok^.location,'$y',tt_parameterIdentifier); parIdx:= 1; end;
      with preparedBody[4] do begin token.create; token.define(tok^.location,'',tt_braceClose);            parIdx:=-1; end;
    end else if (tok^.tokType in [tt_identifier,tt_intrinsicRule,tt_localUserRule,tt_importedUserRule]) then begin
      if (T_intFuncCallback(tok^.data)=BUILTIN_MIN) or (T_intFuncCallback(tok^.data)=BUILTIN_MAX) then funcPtr:=T_intFuncCallback(tok^.data)
      else begin
        context.adapters^.raiseError('Cannot construct primitive aggregator from token: '+tok^.toString(false,dummyBool),declaredAt);
        exit;
      end;

      pattern.appendFreeId('$x');
      pattern.appendFreeId('$y');
      setLength(preparedBody,6);
      with preparedBody[0] do begin token.create; token.define(declaredAt,tok^.txt,tt_intrinsicRule,funcPtr); parIdx:=-1; end;
      with preparedBody[1] do begin token.create; token.define(tok^.location,'',tt_braceOpen);             parIdx:=-1; end;
      with preparedBody[2] do begin token.create; token.define(tok^.location,'$x',tt_parameterIdentifier); parIdx:= 0; end;
      with preparedBody[3] do begin token.create; token.define(tok^.location,'',tt_separatorComma);        parIdx:=-1; end;
      with preparedBody[4] do begin token.create; token.define(tok^.location,'$y',tt_parameterIdentifier); parIdx:= 1; end;
      with preparedBody[5] do begin token.create; token.define(tok^.location,'',tt_braceClose);            parIdx:=-1; end;
    end else context.adapters^.raiseError('Cannot construct primitive aggregator from token: '+tok^.toString(false,dummyBool),declaredAt);
  end;

CONSTRUCTOR T_subrule.clone(CONST original:P_subrule);
  VAR i:longint;
  begin
    comment:=original^.comment;
    called:=false;
    typ:=original^.typ;
    declaredAt:=original^.declaredAt;
    pattern.clone(original^.pattern);

    setLength(preparedBody,length(original^.preparedBody));
    for i:=0 to length(preparedBody)-1 do with preparedBody[i] do begin
      token:=original^.preparedBody[i].token;
      case token.tokType of
        tt_literal,tt_aggregatorExpressionLiteral,tt_list_constructor,tt_parList_constructor,tt_parList: P_literal(token.data)^.rereference;
        tt_each,tt_parallelEach: if token.data<>nil then P_literal(token.data)^.rereference;
      end;
      parIdx:=original^.preparedBody[i].parIdx;
    end;
  end;

DESTRUCTOR T_subrule.destroy;
  VAR i:longint;
  begin
    declaredAt:=C_nilTokenLocation;
    pattern.destroy;
    for i:=0 to length(preparedBody)-1 do preparedBody[i].token.destroy;
    setLength(preparedBody,0);
  end;

FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_evaluationContext):P_expressionLiteral;
  VAR subRule:P_subrule;
  begin
    new(subRule,createPrimitiveAggregator(tok,context));
    result:=newExpressionLiteral(subRule);
  end;

PROCEDURE disposeSubruleImpl (VAR p:pointer);
  begin
    dispose(P_subrule(p),destroy);
    p:=nil;
  end;

FUNCTION T_subrule.replaces(CONST param:P_listLiteral; CONST selfLiteral:P_expressionLiteral; OUT firstRep,lastRep:P_token; VAR context:T_evaluationContext):boolean;
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
        if L=nil then exit(context.newToken(@original));
        result:=context.newToken(original.location,'',tt_literal,L);
        L^.rereference;
      end else begin
        result:=context.newToken(@original);
      end;
    end;

  begin
    if (param= nil) and pattern.matchesNilPattern or
       (param<>nil) and pattern.matches(param^,selfLiteral<>nil) then begin
      called:=true;
      with preparedBody[0] do firstRep:=tokenFromLiteral(token,parIdx);
      t:=firstRep;
      for i:=1 to length(preparedBody)-1 do with preparedBody[i] do begin
        t^.next:=tokenFromLiteral(token,parIdx);
        t:=t^.next;
      end;
      lastRep:=t;
      result:=true;
    end else if (param<>nil)
            and (length(preparedBody)=1)
            and (preparedBody[0].token.tokType=tt_literal)
            and (P_literal(preparedBody[0].token.data)^.literalType=lt_expression)
            and (P_subrule(P_expressionLiteral(preparedBody[0].token.data)^.value)^.pattern.matches(param^,true))
    then begin
      result:=P_subrule(P_expressionLiteral(preparedBody[0].token.data)^.value)^.replaces(param,P_expressionLiteral(preparedBody[0].token.data),firstRep,lastRep,context);
      called:=called or result;
    end else begin
      result:=false;
      case typ of
        srt_inline_for_each: begin
          if param=nil then context.adapters^.raiseError('Cannot evaluate each body with the given number of parameters; Got none, expected '+intToStr(length(pattern.sig)),declaredAt)
                       else context.adapters^.raiseError('Cannot evaluate each body with the given number of parameters; Got '+intToStr(param^.size)+', expected '+intToStr(length(pattern.sig)),declaredAt);
        end;
        srt_inline_for_literal: begin
          if param=nil then context.adapters^.raiseError('Cannot evaluate inline function '+toString+' with the given number of parameters; Got none, expected '+intToStr(length(pattern.sig)),declaredAt)
                       else context.adapters^.raiseError('Cannot evaluate inline function '+toString+' with the given number of parameters; Got '+intToStr(param^.size)+', expected '+intToStr(length(pattern.sig)),declaredAt);
        end;
      end;
    end;
  end;

CONSTRUCTOR T_subrule.createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
  VAR i:longint;
      r:P_subrule;
  PROCEDURE appendToExpression(VAR T:T_preparedToken);
    begin
      setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[length(preparedBody)-1] do begin
        token.create;
        token.define(T.token);
        parIdx:=T.parIdx;
        if parIdx>=0 then token.txt:=pattern.idForIndexInline(parIdx);
      end;
    end;

  PROCEDURE appendToExpression(CONST L:P_literal);
    begin
      setLength(preparedBody,length(preparedBody)+1);
      L^.rereference;
      with preparedBody[length(preparedBody)-1] do begin
        token.create;
        token.define(opLocation,'',tt_literal,L);
        parIdx:=-1;
      end;
    end;

  PROCEDURE appendToExpression(CONST op:T_tokenType);
    begin
      setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[length(preparedBody)-1] do begin
        token.create;
        token.define(opLocation,'',op);
        parIdx:=-1;
      end;
    end;

  begin
    called:=false;
    declaredAt:=opLocation;
    //Pattern (including final parameter names)
    if LHS^.literalType=lt_expression then begin
      if RHS^.literalType=lt_expression
      then pattern.combineForInline(P_subrule(P_expressionLiteral(LHS)^.value)^.pattern,
                                    P_subrule(P_expressionLiteral(RHS)^.value)^.pattern)
      else pattern.clone(P_subrule(P_expressionLiteral(LHS)^.value)^.pattern);
    end else begin
      if RHS^.literalType=lt_expression
      then pattern.clone(P_subrule(P_expressionLiteral(RHS)^.value)^.pattern)
      else pattern.create;
    end;

    typ:=srt_inline_for_literal;
    setLength(preparedBody,0);

    if LHS^.literalType=lt_expression then begin
      r:=P_expressionLiteral(LHS)^.value;
      if r^.canUnbrace(C_opPrecedence[op])
      then for i:=1 to length(r^.preparedBody)-2 do appendToExpression(r^.preparedBody[i])
      else for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
    end else appendToExpression(LHS);
    appendToExpression(op);
    if RHS^.literalType=lt_expression then begin
      r:=P_expressionLiteral(RHS)^.value;
      if r^.canUnbrace(C_opPrecedence[op]-1)
      then for i:=1 to length(r^.preparedBody)-2 do appendToExpression(r^.preparedBody[i])
      else for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
    end else appendToExpression(RHS);
    embrace;
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
      setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[length(preparedBody)-1] do begin
        token.create;
        token.define(T);
        parIdx:=-1;
      end;
    end;

  PROCEDURE appendToExpression(CONST op:T_tokenType);
    begin
      setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[length(preparedBody)-1] do begin
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

    setLength(preparedBody,1);
    with preparedBody[0] do begin token.create; token.define(declaredAt,intrinsicRuleId,tt_intrinsicRule,intrinsicRuleMap.get(intrinsicRuleId)); parIdx:=-1; end;
    if not(origRule^.canUnbrace(C_opPrecedence[tt_unaryOpPlus])) then appendToExpression(tt_braceOpen);
    for i:=0 to length(origRule^.preparedBody)-1 do appendToExpression(origRule^.preparedBody[i].token);
    if not(origRule^.canUnbrace(C_opPrecedence[tt_unaryOpPlus])) then appendToExpression(tt_braceClose);
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
    for i:=0 to length(preparedBody)-1 do begin
      result:=result+preparedBody[i].token.toString(prevIdLike,idLike);
      prevIdLike:=idLike;
    end;
    if      typ=srt_inline_for_literal then result:=                 '{'+result+'}'
    else if typ=srt_inline_for_each    then result:=pattern.toString+'{'+result+'}'
    else result:=pattern.toString+C_tokenString[tt_declare]+result;
  end;

FUNCTION subruleToStringImpl(CONST p:pointer):string;
  begin
    result:=P_subrule(p)^.toString;
  end;

FUNCTION subruleToArityImpl(CONST p:pointer):string;
  begin
    result:=intToStr(P_subrule(p)^.arity);
  end;


FUNCTION T_subrule.directEvaluate(CONST selfLiteral:P_expressionLiteral; CONST params:P_listLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
  VAR firstRep,lastRep:P_token;
  begin
    if replaces(params,selfLiteral,firstRep,lastRep,context) then begin
      reduceExpression(firstRep,callDepth+1,context);
      if (context.adapters^.noErrors) and (firstRep<>nil) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal)
      then begin
        result:=firstRep^.data;
        result^.rereference;
      end else begin
        result:=nil;
        if (params<>nil) and (params^.size>0)
        then context.adapters^.raiseError('Cannot directly resolve call with parameters '+params^.toParameterListString(true)+'. Make sure the function can be resolved without errors, returning only a literal. ',declaredAt)
        else context.adapters^.raiseError('Cannot directly resolve call without parameters. '                                  +'Make sure the function can be resolved without errors, returning only a literal. ',declaredAt);
        context.adapters^.raiseError('The failed function is:'+toString,declaredAt);
        context.adapters^.raiseError('The resulting expression is: '+tokensToString(firstRep,maxLongint),declaredAt);
      end;
      context.cascadeDisposeToken(firstRep);
    end else result:=nil;
  end;

FUNCTION T_subrule.directEvaluateNullary(CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
  begin
    result:=directEvaluate(selfLiteral,nil,callDepth,context);
  end;

FUNCTION T_subrule.directEvaluateUnary(CONST x:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
  VAR param:T_listLiteral;
  begin
    param.create;
    param.append(x,true,context.adapters^);
    result:=directEvaluate(selfLiteral,@param,callDepth,context);
    param.destroy;
  end;

FUNCTION T_subrule.directEvaluateComparator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):boolean;
  VAR param:T_listLiteral;
      lit:P_literal;
  begin
    param.create;
    param.append(x,true,context.adapters^);
    param.append(y,true,context.adapters^);
    lit:=directEvaluate(selfLiteral,@param,callDepth,context);
    param.destroy;
    if (context.adapters^.noErrors) and (lit<>nil) and (lit^.literalType=lt_boolean) then result:=P_boolLiteral(lit)^.value
    else begin
      result:=false;
      context.adapters^.raiseError('Cannot directly resolve comparator call with parameters ('+x^.toString+','+y^.toString+'). Make sure the function can be resolved without errors, returning only a boolean literal.',declaredAt);
    end;
    if lit<>nil then disposeLiteral(lit);
  end;

FUNCTION T_subrule.directEvaluateAggregator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
  VAR param:T_listLiteral;
  begin
    param.create;
    param.append(x,true,context.adapters^);
    param.append(y,true,context.adapters^);
    result:=directEvaluate(selfLiteral,@param,callDepth,context);
    param.destroy;
  end;

FUNCTION evaluateComparator(CONST subruleLiteral:P_expressionLiteral; CONST LHSComparand,RHScomparand:P_literal; VAR adapters:T_adapters):boolean;
  VAR tempcontext:T_evaluationContext;
  begin
    tempcontext.createNormalContext(P_adapters(@adapters));
    result:=P_subrule(subruleLiteral^.value)^.directEvaluateComparator(LHSComparand,RHScomparand,subruleLiteral,0,tempcontext);
    tempcontext.destroy;
  end;


FUNCTION T_subrule.getInlineValue:P_literal;
  begin
    if (length(preparedBody)<>1) or (typ in [srt_inline_for_each,srt_inline_for_literal]) then exit(nil);
    with preparedBody[0] do if token.tokType=tt_literal then begin
      called:=true;
      result:=token.data;
      result^.rereference;
    end else result:=nil;
  end;

FUNCTION T_subrule.complainAboutUncalled(CONST ruleId:ansistring; CONST inMainPackage:boolean; VAR adapters:T_adapters):boolean;
  begin
    result:=not(called) and (typ=srt_normal_private);
    if inMainPackage and result then adapters.raiseWarning('Unused private subrule '+ruleId+pattern.toString,declaredAt);
  end;

FUNCTION T_subrule.getDocTxt(CONST ruleId:ansistring):ansistring;
  begin
    result:='@Line '+intToStr(declaredAt.line)+': '+C_tabChar;
    if typ=srt_normal_private then result:=result+'private ';
    result:=result+ruleId+pattern.toString+';';
    if comment<>'' then result:=result+C_tabChar+'//'+replaceAll(comment,C_lineBreakChar,C_lineBreakChar+C_tabChar+C_tabChar+'//');
  end;

FUNCTION T_subrule.getDocHtml(CONST ruleId:ansistring):ansistring;
  CONST Shift='&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;';
        zeroBlobLevel:longint=0;
  begin
    result:='<tr><td>@Line '+intToStr(declaredAt.line)+': ';
    if comment<>'' then begin
      if pos(C_lineBreakChar,comment)>0
      then result:=result+'<br>'+Shift+'<i>'+replaceAll(comment,C_lineBreakChar,'<br>'+Shift)+'</i><br>'
      else result:=result+'<i>'+comment+'</i><br>';
    end;
    result:=result+'<code>'+toHtmlCode(ruleId+pattern.toString,zeroBlobLevel)+'</code></td></tr>';
  end;

FUNCTION T_subrule.arity:longint;
  begin
    result:=length(pattern.sig);
  end;

{$endif}
