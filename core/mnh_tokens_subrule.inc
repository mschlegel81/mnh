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
      parent:P_rule;
      typ:T_subruleType;
      declaredAt:T_tokenLocation;
      pattern:T_pattern;
      preparedBody:array of T_preparedToken;

      PROCEDURE updatePatternForInline;
      PROCEDURE embrace;
      FUNCTION needEmbrace:boolean;
      FUNCTION canUnbrace(CONST outerPrecedence:longint):boolean;
      PROCEDURE constructExpression(CONST rep:P_token; VAR context:T_evaluationContext; CONST construct:T_tokenType);
      PROCEDURE modifyBeginEnd;
    public
      CONSTRUCTOR create           (CONST parent_:P_rule; CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate,forSpecialConstruct:boolean; VAR context:T_evaluationContext);
      CONSTRUCTOR createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; VAR context:T_evaluationContext);
      CONSTRUCTOR createFromInline (CONST rep:P_token; VAR context:T_evaluationContext);
      CONSTRUCTOR createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
      CONSTRUCTOR createFromInlineWithOp(CONST original:P_expressionLiteral; CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation);
      CONSTRUCTOR createPrimitiveAggregator(CONST tok:P_token; VAR context:T_evaluationContext);
      CONSTRUCTOR clone(CONST original:P_subrule);
      DESTRUCTOR destroy;
      FUNCTION replaces(CONST param:P_listLiteral; CONST selfLiteral:P_expressionLiteral; OUT firstRep,lastRep:P_token; VAR context:T_evaluationContext; CONST useUncurryingFallback:boolean):boolean;
      FUNCTION toString(CONST includePattern:boolean=true; CONST lengthLimit:longint=maxLongint):ansistring;
      FUNCTION directEvaluate(CONST selfLiteral:P_expressionLiteral; CONST params:P_listLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
      FUNCTION directEvaluateNullary(CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
      FUNCTION directEvaluateComparator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):boolean;
      FUNCTION directEvaluateAggregator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
      FUNCTION getInlineValue:P_literal;
      FUNCTION getDocTxt:ansistring;
      FUNCTION getDocHtml:ansistring;
      FUNCTION getSubruleId:ansistring;
      FUNCTION arity:longint;
      PROCEDURE increaseArity(CONST newArity:longint);
      FUNCTION inspect:P_listLiteral;
      PROCEDURE resolveIds(CONST adapters:P_adapters);
  end;

{$endif}
{$ifdef include_implementation}
PROCEDURE T_subrule.constructExpression(CONST rep:P_token; VAR context:T_evaluationContext; CONST construct:T_tokenType);
  VAR t:P_token;
  begin
    setLength(preparedBody,0);
    t:=rep;
    while t<>nil do begin
      setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[length(preparedBody)-1] do begin
        token:=t^;
        t^.tokType:=tt_EOL; t:=context.disposeToken(t);
        token.next:=nil;
        case token.tokType of
          tt_optionalParameters: parIdx:=REMAINING_PARAMETERS_IDX;
          tt_identifier, tt_localUserRule, tt_importedUserRule, tt_parameterIdentifier, tt_intrinsicRule: begin
            parIdx:=pattern.indexOfId(token.txt);
            if parIdx>=0 then begin
              if parIdx>=REMAINING_PARAMETERS_IDX
              then token.tokType:=tt_parameterIdentifier
              else token.tokType:=tt_identifier;
            end else if (construct=tt_each) and (token.txt=C_eachIndexIdentifier) then token.tokType:=tt_blockLocalVariable
            else if token.tokType<>tt_parameterIdentifier then token.tokType:=tt_identifier;
          end;
          tt_return: begin parIdx:=-1; if construct<>tt_EOL then context.adapters^.raiseError('return is not allowed in special constructs',token.location); end;
          else parIdx:=-1;
        end;
      end;
    end;
    resolveIds(nil);
    if not(construct<>tt_EOL) then modifyBeginEnd;
  end;

PROCEDURE T_subrule.modifyBeginEnd;
  VAR last:longint;
  begin
    last:=length(preparedBody)-1;
    if (preparedBody[0   ].token.tokType=tt_beginBlock) and
       (preparedBody[last].token.tokType=tt_endBlock) then begin
      preparedBody  [0   ].token.tokType:=tt_beginFunc;
      preparedBody  [last].token.tokType:=tt_endFunc;
    end;
  end;

CONSTRUCTOR T_subrule.create(CONST parent_:P_rule; CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate,forSpecialConstruct:boolean; VAR context:T_evaluationContext);
  CONST C_specialConstruct:array[false..true] of T_tokenType=(tt_EOL,tt_while);
  begin
    parent:=parent_;
    called:=false;
    if isPrivate then typ:=srt_normal_private
                 else typ:=srt_normal_public;
    pattern:=pat;
    declaredAt:=declAt;
    constructExpression(rep,context,C_specialConstruct[forSpecialConstruct]);
    if needEmbrace then embrace;
  end;

CONSTRUCTOR T_subrule.createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; VAR context:T_evaluationContext);
  begin
    parent:=nil;
    called:=false;
    typ:=srt_inline_for_each;
    pattern.create;
    pattern.appendFreeId(parameterId);
    declaredAt:=rep^.location;
    constructExpression(rep,context,tt_each);
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
      if (tokType in [tt_separatorComma, tt_separatorCnt,
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
      if      tokType in C_openingBrackets then inc(level)
      else if tokType in C_closingBrackets then dec(level);
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
    parent:=nil;
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
    parent:=nil;
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
        if token.tokType=tt_return then context.adapters^.raiseError('return is not allowed in inline-expressions',token.location);
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
    parent:=nil;
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
    end else if (tok^.tokType=tt_intrinsicRule) then begin
      if (T_intFuncCallback(tok^.data)=BUILTIN_MIN) or
         (T_intFuncCallback(tok^.data)=BUILTIN_MAX) or
         (T_intFuncCallback(tok^.data)=BUILTIN_HEAD) then funcPtr:=T_intFuncCallback(tok^.data)
      else begin
        context.adapters^.raiseError('Cannot construct primitive aggregator from token: '+tok^.toString(false,dummyBool),declaredAt);
        exit;
      end;
      if T_intFuncCallback(tok^.data)=BUILTIN_HEAD then begin
        pattern.appendFreeId('$x');
        pattern.appendFreeId('$y');
        setLength(preparedBody,1);
        with preparedBody[0] do begin token.create; token.define(tok^.location,'$x',tt_parameterIdentifier); parIdx:= 0; end;
      end else begin
        pattern.appendFreeId('$x');
        pattern.appendFreeId('$y');
        setLength(preparedBody,6);
        with preparedBody[0] do begin token.create; token.define(declaredAt,tok^.txt,tt_intrinsicRule,funcPtr); parIdx:=-1; end;
        with preparedBody[1] do begin token.create; token.define(tok^.location,'',tt_braceOpen);             parIdx:=-1; end;
        with preparedBody[2] do begin token.create; token.define(tok^.location,'$x',tt_parameterIdentifier); parIdx:= 0; end;
        with preparedBody[3] do begin token.create; token.define(tok^.location,'',tt_separatorComma);        parIdx:=-1; end;
        with preparedBody[4] do begin token.create; token.define(tok^.location,'$y',tt_parameterIdentifier); parIdx:= 1; end;
        with preparedBody[5] do begin token.create; token.define(tok^.location,'',tt_braceClose);            parIdx:=-1; end;
      end;
    end else context.adapters^.raiseError('Cannot construct primitive aggregator from token: '+tok^.toString(false,dummyBool),declaredAt);
  end;

CONSTRUCTOR T_subrule.clone(CONST original:P_subrule);
  VAR i:longint;
  begin
    parent:=nil;
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
        tt_each,tt_parallelEach,tt_forcedParallelEach: if token.data<>nil then P_literal(token.data)^.rereference;
      end;
      parIdx:=original^.preparedBody[i].parIdx;
    end;
  end;

DESTRUCTOR T_subrule.destroy;
  VAR i:longint;
  begin
    declaredAt.package:=nil;
    declaredAt.column:=0;
    declaredAt.line:=0;
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

FUNCTION T_subrule.replaces(CONST param:P_listLiteral; CONST selfLiteral:P_expressionLiteral; OUT firstRep,lastRep:P_token; VAR context:T_evaluationContext; CONST useUncurryingFallback:boolean):boolean;
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

  {$ifdef fullVersion}
  PROCEDURE wrapResult;
    FUNCTION parId(CONST index:longint):string;
      begin
        if (index>=0) and (index<length(pattern.sig))
        then result:=pattern.sig[index].id
        else result:='';
        if result='' then result:='#'+intToStr(i);
      end;

    VAR newFirst,tmp:P_token;
        i,i1:longint;
        subruleId:ansistring;
        subruleLocation:ansistring;
    begin
      //Example: f(x,y)->...
      //add before: begin
      //              local x:=x;
      //              local y:=y;
      //add after:  ; end
      //pattern.sig[0].id;
      subruleId:=getSubruleId;
      subruleLocation:=ansistring(declaredAt);
      if (firstRep^.tokType=tt_beginFunc) and
         (lastRep ^.tokType=tt_endFunc) then begin
        firstRep  ^.tokType:=tt_beginBlock;
        lastRep   ^.tokType:=tt_endBlock;
      end;
      if typ in [srt_normal_private,srt_normal_public]
      then newFirst:=context.newToken(declaredAt,subruleId,tt_beginFunc)
      else newFirst:=context.newToken(declaredAt,subruleId,tt_beginBlock);
      tmp:=newFirst;
      if (param<>nil) then i1:=param^.size else i1:=length(pattern.sig);
      for i:=0 to i1-1 do begin
        tmp^.next:=context.newToken(declaredAt,parId(i),tt_assignNewBlockLocal); tmp:=tmp^.next;
        tmp^.next:=context.newToken(declaredAt,'',tt_literal,param^.value(i));   tmp:=tmp^.next;
        tmp^.next:=context.newToken(declaredAt,'',tt_semicolon);                 tmp:=tmp^.next;
        param^.value(i)^.rereference;
      end;
      tmp^.next:=firstRep;
      firstRep:=newFirst;
      lastRep^.next:=context.newToken(declaredAt,'',tt_semicolon); lastRep:=lastRep^.next;

      if typ in [srt_normal_private,srt_normal_public]
      then lastRep^.next:=context.newToken(declaredAt,'',tt_endFunc)
      else lastRep^.next:=context.newToken(declaredAt,'',tt_endBlock);
      lastRep:=lastRep^.next;
      if typ in [srt_normal_private,srt_normal_public]
      then stepper.steppingIn(subruleLocation,subruleId);
    end;
  {$endif}

  FUNCTION fallbackFeasible:boolean;
    begin
      result:=useUncurryingFallback and
              //The given parameters must match
             (param<>nil) and pattern.matchesForFallback(param^,selfLiteral<>nil) and
              //The function result must (likely) be an expression
         (    (preparedBody[0].token.tokType=tt_literal)
          and (P_literal(preparedBody[0].token.data)^.literalType=lt_expression)
           or (length(preparedBody)>=2)
          and (preparedBody[0].token.tokType=tt_expBraceOpen)
          and (preparedBody[length(preparedBody)-1].token.tokType=tt_expBraceClose));
    end;

  PROCEDURE prepareResult;
    VAR i:longint;
    begin
      called:=true;
      with preparedBody[0] do firstRep:=tokenFromLiteral(token,parIdx);
      t:=firstRep;
      for i:=1 to length(preparedBody)-1 do with preparedBody[i] do begin
        t^.next:=tokenFromLiteral(token,parIdx);
        t:=t^.next;
      end;
      lastRep:=t;
      result:=true;
    end;

  VAR tempInnerParam:P_listLiteral;
  begin
    if (param= nil) and pattern.matchesNilPattern or
       (param<>nil) and pattern.matches(param^,selfLiteral<>nil) then begin
      prepareResult;
    end else if fallbackFeasible then begin
      prepareResult;
      tempInnerParam:=newListLiteral;
      for i:=length(pattern.sig) to param^.size-1 do tempInnerParam^.append(param^.value(i),true);
      lastRep^.next:=context.newToken(declaredAt,'',tt_parList,tempInnerParam);
      lastRep:=lastRep^.next;
    end else begin
      result:=false;
      if useUncurryingFallback then case typ of
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
    {$ifdef fullVersion}
    if currentlyDebugging and result and (typ<>srt_inline_for_each) and (param<>nil) and (param^.size<>0) then wrapResult;
    {$endif}
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
    parent:=nil;
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
    parent:=nil;
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

FUNCTION T_subrule.toString(CONST includePattern:boolean=true; CONST lengthLimit:longint=maxLongint):ansistring;
  VAR i,remainingLength:longint;
      prevIdLike,idLike:boolean;
  begin
    prevIdLike:=false;
    result:='';
    remainingLength:=lengthLimit;
    for i:=0 to length(preparedBody)-1 do begin
      result:=result+preparedBody[i].token.toString(prevIdLike,idLike,remainingLength);
      remainingLength:=lengthLimit-length(result);
      prevIdLike:=idLike;
    end;
    if not(includePattern) then exit(result);
    if      typ=srt_inline_for_literal then result:=                 '{'+result+'}'
    else if typ=srt_inline_for_each    then result:=pattern.toString+'{'+result+'}'
    else result:=pattern.toString+C_tokenInfo[tt_declare].defaultId+result;
  end;

FUNCTION subruleToStringImpl(CONST p:pointer; CONST lengthLimit:longint):string;
  begin
    result:=P_subrule(p)^.toString(true,lengthLimit);
  end;

FUNCTION subruleToArityImpl(CONST p:pointer):longint;
  begin
    result:=P_subrule(p)^.arity;
  end;


FUNCTION T_subrule.directEvaluate(CONST selfLiteral:P_expressionLiteral; CONST params:P_listLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
  VAR firstRep,lastRep:P_token;
  begin
    if replaces(params,selfLiteral,firstRep,lastRep,context,false) then begin
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
        context.adapters^.raiseError('The resulting expression is: '+tokensToString(firstRep,50),declaredAt);
      end;
      context.cascadeDisposeToken(firstRep);
    end else begin
      context.adapters^.raiseError('Cannot apply function to parameters: '+params^.toParameterListString(true),declaredAt);
      result:=nil;
    end;
  end;

FUNCTION T_subrule.directEvaluateNullary(CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):P_literal;
  begin
    result:=directEvaluate(selfLiteral,nil,callDepth,context);
  end;

FUNCTION T_subrule.directEvaluateComparator(CONST x,y:P_literal; CONST selfLiteral:P_expressionLiteral; CONST callDepth:word; VAR context:T_evaluationContext):boolean;
  VAR param:T_listLiteral;
      lit:P_literal;
  begin
    param.create;
    param.append(x,true);
    param.append(y,true);
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
    param.append(x,true);
    param.append(y,true);
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

FUNCTION evaluateSubrule(CONST subruleLiteral:P_expressionLiteral; CONST parameters:P_listLiteral; CONST context:pointer):P_literal;
  begin
    result:=P_subrule(subruleLiteral^.value)^.directEvaluate(subruleLiteral,parameters,0,P_evaluationContext(context)^);
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

FUNCTION T_subrule.getDocTxt():ansistring;
  begin
    result:='@Line '+intToStr(declaredAt.line)+': '+C_tabChar;
    if typ=srt_normal_private then result:=result+'private ';
    result:=result+getSubruleId+';';
    if comment<>'' then result:=result+C_tabChar+'//'+replaceAll(comment,C_lineBreakChar,C_lineBreakChar+C_tabChar+C_tabChar+'//');
  end;

FUNCTION T_subrule.getDocHtml():ansistring;
  CONST Shift='&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;';
  begin
    result:='<tr><td>@Line '+intToStr(declaredAt.line)+': ';
    if comment<>'' then begin
      if pos(C_lineBreakChar,comment)>0
      then result:=result+'<br>'+Shift+'<i>'+replaceAll(comment,C_lineBreakChar,'<br>'+Shift)+'</i><br>'
      else result:=result+'<i>'+comment+'</i><br>';
    end;
    result:=result+'<code>'+toHtmlCode(getSubruleId)+'</code></td></tr>';
  end;

FUNCTION T_subrule.getSubruleId:ansistring;
  begin
    case typ of
      srt_inline_for_literal: result:='expression';
      srt_inline_for_each   : result:='each body ';
      else begin
        if parent=nil then result:='?'
                      else result:=parent^.id;
        result:=result+pattern.toString;
      end;
    end;
  end;

FUNCTION T_subrule.arity:longint;
  begin
    result:=length(pattern.sig);
  end;

PROCEDURE T_subrule.increaseArity(CONST newArity:longint);
  begin
    while length(pattern.sig)<newArity do pattern.appendFreeId('');
  end;

FUNCTION T_subrule.inspect:P_listLiteral;
  FUNCTION typeString:string;
    begin
      case typ of
        srt_normal_public     : result:='public';
        srt_normal_private    : result:='private';
        srt_inline_for_literal: result:='for_literal';
        srt_inline_for_each   : result:='for_each';
      end;
    end;

  begin
    result:=newListLiteral^
      .append(newListLiteral^.appendString('pattern')^.appendString(pattern.toString),false)^
      .append(newListLiteral^.appendString('location')^.appendString(declaredAt),false)^
      .append(newListLiteral^.appendString('type')^.appendString(typeString),false)^
      .append(newListLiteral^.appendString('comment')^.appendString(comment),false)^
      .append(newListLiteral^.appendString('body')^.appendString(toString(false)),false);
  end;

PROCEDURE T_subrule.resolveIds(CONST adapters:P_adapters);
  VAR i:longint;
      bracketStack:T_TokenStack;
  FUNCTION isEachIdentifier(CONST id:string):boolean;
    VAR k:longint;
    begin
      result:=false;
      for k:=0 to bracketStack.topIndex do
      if (bracketStack.dat[k]^.tokType in [tt_each,tt_parallelEach,tt_forcedParallelEach]) and
         (bracketStack.dat[k]^.txt=id) or (id=C_eachIndexIdentifier) then exit(true);
    end;

  begin
    bracketStack.create;
    for i:=0 to length(preparedBody)-1 do with preparedBody[i] do begin
      case token.tokType of
        tt_each,tt_parallelEach,tt_forcedParallelEach,tt_braceOpen: bracketStack.quietPush(@token);
        tt_braceClose:bracketStack.quietPop;
      end;
      if (parIdx<0) and (token.tokType=tt_identifier) and not(isEachIdentifier(token.txt)) then
      P_package(token.location.package)^.resolveRuleId(token,adapters);
    end;
    bracketStack.destroy;
  end;


{$ifdef FULLVERSION}
FUNCTION generateRow(CONST f:P_expressionLiteral; CONST t0,t1:T_myFloat; CONST samples:longint; CONST location:T_tokenLocation; VAR context:T_evaluationContext):T_dataRow;
  VAR subRule:P_subrule;

      firstRep,lastRep:P_token;
      TList:P_listLiteral;
      params:T_listLiteral;
      resultLiteral:P_listLiteral;

      tempAdapters:T_adapters;
      tempcontext:T_evaluationContext;

      dataReadyVectorized:boolean=false;
      dataReadyScalar    :boolean=false;

  FUNCTION evaluateOk:boolean;
    begin
      reduceExpression(firstRep,0,tempcontext);
      result:=tempAdapters.noErrors and
              (firstRep<>nil) and
              (firstRep^.next=nil) and
              (firstRep^.tokType=tt_literal) and
              (P_literal(firstRep^.data)^.literalType in [lt_list,lt_realList,lt_intList,lt_numList]) and
              (P_listLiteral(firstRep^.data)^.size = TList^.size);
      tempAdapters.clearErrors;
      if not(result) then context.cascadeDisposeToken(firstRep);
    end;

  PROCEDURE constructPEachExpression;
    begin
      firstRep:=context.newToken(location,'',tt_literal,TList); TList^.rereference;
      firstRep^.next:=context.newToken(location,'t',tt_parallelEach);
      lastRep:=firstRep^.next;
      lastRep^.next:=context.newToken(location,'t',tt_identifier);
      lastRep:=lastRep^.next;
      lastRep^.next:=context.newToken(location,'',tt_ponFlipper);
      lastRep:=lastRep^.next;
      lastRep^.next:=context.newToken(location,'',tt_literal,f); f^.rereference;
      lastRep:=lastRep^.next;
      lastRep^.next:=context.newToken(location,'',tt_braceClose);
    end;

  PROCEDURE constructInitialTList;
    VAR sampleIndex:longint;
    begin
      TList:=newListLiteral;
      for sampleIndex:=0 to samples-1 do TList^.appendReal(t0+(t1-t0)/(samples-1)*sampleIndex);
    end;

  //PROCEDURE refineDataRow;
  //  VAR distThreshold:double=0;
  //      distThresholdSamples:longint=0;
  //      i:longint;
  //      newTimes:T_arrayOfDouble;
  //  begin
  //    setLength(newTimes,0);
  //    for i:=1 to length(dataRow)-1 do
  //    if not(IsNan(dataRow[i,0])) and not(IsNan(dataRow[i-1,0])) and
  //       not(IsNan(dataRow[i,1])) and not(IsNan(dataRow[i-1,1])) then begin
  //      distThreshold:=distThreshold+sqr(dataRow[i,0]-dataRow[i-1,0])
  //                                  +sqr(dataRow[i,1]-dataRow[i-1,1]);
  //      inc(distThresholdSamples);
  //    end;
  //    distThreshold:=distThreshold/distThresholdSamples;
  //
  //    for i:=0
  //
  //  end;

  begin
    subRule:=P_subrule(f^.value);

    tempAdapters.create;
    tempcontext.createNormalContext(@tempAdapters);

    constructInitialTList;
    params.create;
    params.append(TList,true);
    if (subRule^.replaces(@params,f,firstRep,lastRep,tempcontext,false) or
        subRule^.replaces(@params,f,firstRep,lastRep,tempcontext,true)) and evaluateOk then dataReadyVectorized:=true;
    params.destroy;

    if not(dataReadyVectorized) then begin
      constructPEachExpression;
      if evaluateOk then dataReadyScalar:=true;
    end;

    tempAdapters.destroy;
    tempcontext.destroy;
    if dataReadyScalar or dataReadyVectorized then begin
      resultLiteral:=P_listLiteral(firstRep^.data);
      if resultLiteral^.literalType in [lt_intList,lt_realList,lt_numList]
      then result:=newDataRow(resultLiteral,TList)
      else result:=newDataRow(resultLiteral);
      context.cascadeDisposeToken(firstRep);
    end else begin
      context.adapters^.raiseError('Cannot prepare sample row using function '+f^.toString(),location);
      setLength(result,0);
    end;

    disposeLiteral(TList);
  end;
{$endif}


{$endif}

