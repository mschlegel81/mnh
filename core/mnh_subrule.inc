{$ifdef include_interface}
  T_subruleType=(srt_normal_public,
                 srt_normal_private,
                 srt_inline_for_literal,
                 srt_inline_for_each,
                 srt_inline_for_while);
  T_preparedToken=record
    parIdx:longint;
    token:T_token;
  end;

  T_subrule=object(T_objectWithIdAndLocation)
    private
      firstCallCs:TRTLCriticalSection;
      functionIdsReady:boolean;

      comment:ansistring;
      parent:P_rule;
      typ:T_subruleType;
      declaredAt:T_tokenLocation;
      pattern:T_pattern;
      preparedBody:array of T_preparedToken;

      PROCEDURE updatePatternForInline;
      PROCEDURE constructExpression(CONST rep:P_token; VAR context:T_evaluationContext; CONST forEach:boolean);
      CONSTRUCTOR init(CONST srt: T_subruleType; CONST location: T_tokenLocation; CONST parentRule:P_rule=nil);
      PROCEDURE resolveIds(CONST adapters:P_adapters);
      FUNCTION needEmbrace(CONST outerPrecedence:longint):boolean;
    public
      CONSTRUCTOR create           (CONST parent_:P_rule; CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate,forWhile:boolean; VAR context:T_evaluationContext);
      CONSTRUCTOR createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; VAR context:T_evaluationContext);
      CONSTRUCTOR createFromInline (CONST rep:P_token; VAR context:T_evaluationContext);
      CONSTRUCTOR createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
      CONSTRUCTOR createFromInlineWithOp(CONST original:P_expressionLiteral; CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation);
      CONSTRUCTOR createPrimitiveAggregator(CONST tok:P_token; VAR context:T_evaluationContext);
      CONSTRUCTOR clone(CONST original:P_subrule);
      DESTRUCTOR destroy;
      //Evaluation calls:
      FUNCTION replaces                (CONST param:P_listLiteral;  CONST expressionLiteralOrNil:P_expressionLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_evaluationContext; CONST useUncurryingFallback:boolean):boolean;
      FUNCTION directEvaluate          (CONST params:P_listLiteral; CONST callLocation:T_tokenLocation;                               VAR context:T_evaluationContext; CONST callDepth:word):P_literal; inline;
      FUNCTION directEvaluateNullary   (                            CONST callLocation:T_tokenLocation;                               VAR context:T_evaluationContext; CONST callDepth:word):P_literal;
      FUNCTION directEvaluateUnary     (CONST x:P_literal;          CONST callLocation:T_tokenLocation;                               VAR context:T_evaluationContext; CONST callDepth:word):P_literal;
      FUNCTION directEvaluateComparator(CONST x,y:P_literal;        CONST callLocation:T_tokenLocation; VAR context:T_evaluationContext;                               CONST callDepth:word):boolean;
      FUNCTION directEvaluateAggregator(CONST x,y:P_literal;        CONST callLocation:T_tokenLocation; VAR context:T_evaluationContext;                               CONST callDepth:word):P_literal;
      FUNCTION accept                  (CONST x:P_literal;                                                                            VAR context:T_evaluationContext):boolean;
      FUNCTION getInlineValue:P_literal;

      //Inspection/documentation calls
      FUNCTION toString(CONST includePattern:boolean=true; CONST lengthLimit:longint=maxLongint):ansistring;
      FUNCTION getCmdLineHelpText:ansistring;
      FUNCTION getDocTxt:ansistring;
      FUNCTION getDocHtml:ansistring;
      FUNCTION getId:T_idString; virtual;
      FUNCTION getLocation:T_tokenLocation; virtual;
      FUNCTION arity:longint;
      PROCEDURE increaseArity(CONST newArity:longint);
      FUNCTION inspect:P_listLiteral;
  end;

{$endif}
{$ifdef include_implementation}
PROCEDURE T_subrule.constructExpression(CONST rep:P_token; VAR context:T_evaluationContext; CONST forEach:boolean);
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
            end else if forEach and (token.txt=EACH_INDEX_IDENTIFIER) then token.tokType:=tt_blockLocalVariable
            else if token.tokType<>tt_parameterIdentifier then token.tokType:=tt_identifier;
          end;
          else parIdx:=-1;
        end;
      end;
    end;
  end;

CONSTRUCTOR T_subrule.init(CONST srt:T_subruleType; CONST location:T_tokenLocation; CONST parentRule:P_rule=nil);
  begin
    initCriticalSection(firstCallCs);
    functionIdsReady:=false;
    comment:='';
    parent:=parentRule;
    typ:=srt;
    declaredAt:=location;
    setLength(preparedBody,0);
  end;

CONSTRUCTOR T_subrule.create(CONST parent_:P_rule; CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate,forWhile:boolean; VAR context:T_evaluationContext);
  begin
    init(srt_normal_public,declAt,parent_);
    if       forWhile then typ:=srt_inline_for_while
    else if isPrivate then typ:=srt_normal_private;
    pattern:=pat;
    constructExpression(rep,context,false);
    resolveIds(nil);
  end;

CONSTRUCTOR T_subrule.createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; VAR context:T_evaluationContext);
  begin
    init(srt_inline_for_each,rep^.location);
    pattern.create;
    pattern.appendFreeId(parameterId);
    constructExpression(rep,context,true);
    resolveIds(nil);
  end;

FUNCTION T_subrule.needEmbrace(CONST outerPrecedence:longint):boolean;
  VAR i:longint;
      level:longint=0;
  begin
    if length(preparedBody)<=1 then exit(false);
    level:=0;
    i:=length(preparedBody)-1;
    for i:=0 to length(preparedBody)-1 do with preparedBody[i].token do begin
      if tokType in C_openingBrackets then inc(level)
      else if tokType in C_closingBrackets then dec(level)
      else if (tokType in C_operatorsAndComparators) and (level=0) and (C_opPrecedence[preparedBody[i].token.tokType]>outerPrecedence) then exit(true);
    end;
    result:=false;
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
    init(srt_inline_for_literal,rep^.location);
    pattern.create;
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
    updatePatternForInline;
  end;

CONSTRUCTOR T_subrule.createPrimitiveAggregator(CONST tok:P_token; VAR context:T_evaluationContext);
  VAR dummyBool:boolean;
      funcPtr:P_intFuncCallback=nil;
  begin
    init(srt_inline_for_literal,tok^.location);
    pattern.create;
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
      if (P_intFuncCallback(tok^.data)=BUILTIN_MIN) or
         (P_intFuncCallback(tok^.data)=BUILTIN_MAX) or
         (P_intFuncCallback(tok^.data)=BUILTIN_HEAD) then funcPtr:=P_intFuncCallback(tok^.data)
      else begin
        context.adapters^.raiseError('Cannot construct primitive aggregator from token: '+tok^.toString(false,dummyBool),declaredAt);
        exit;
      end;
      if P_intFuncCallback(tok^.data)=BUILTIN_HEAD then begin
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
    init(original^.typ,original^.declaredAt,original^.parent);
    comment:=original^.comment;
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
    declaredAt.package:=nil;
    declaredAt.column:=0;
    declaredAt.line:=0;
    pattern.destroy;
    for i:=0 to length(preparedBody)-1 do preparedBody[i].token.destroy;
    setLength(preparedBody,0);
    doneCriticalSection(firstCallCs);
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

FUNCTION T_subrule.replaces(CONST param:P_listLiteral; CONST expressionLiteralOrNil:P_expressionLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_evaluationContext; CONST useUncurryingFallback:boolean):boolean;
  VAR i:longint;
  FUNCTION fallbackFeasible:boolean;
    begin
      result:=useUncurryingFallback and
              //The given parameters must match
             (param<>nil) and pattern.matchesForFallback(param^,context) and
              //The function result must (likely) be an expression
         (    (preparedBody[0].token.tokType=tt_literal)
          and (P_literal(preparedBody[0].token.data)^.literalType=lt_expression)
           or (length(preparedBody)>=2)
          and (preparedBody[0].token.tokType=tt_expBraceOpen)
          and (preparedBody[length(preparedBody)-1].token.tokType=tt_expBraceClose));
    end;

  PROCEDURE prepareResult;
    CONST beginToken:array[false..true] of T_tokenType=(tt_beginExpression,tt_beginRule);
          endToken  :array[false..true] of T_tokenType=(tt_endExpression  ,tt_endRule  );
    VAR i:longint;
        b:boolean;
        L:P_literal;
        remaining:P_listLiteral=nil;
    begin
      if not(functionIdsReady) then resolveIds(context.adapters);
      b:=typ in [srt_normal_private,srt_normal_public];
      firstRep:=context.newToken(declaredAt,'',beginToken[b]);
      lastRep:=firstRep;
      for i:=0 to length(preparedBody)-1 do with preparedBody[i] do begin
        if parIdx>=0 then begin
          if parIdx=ALL_PARAMETERS_PAR_IDX then L:=param
          else if parIdx=REMAINING_PARAMETERS_IDX then begin
            if remaining=nil then begin
              if param=nil
              then remaining:=newListLiteral
              else remaining:=param^.tail(length(pattern.sig));
              remaining^.unreference;
            end;
            L:=remaining;
          end else L:=param^.value(parIdx);
          {$ifdef debugMode}
          if L=nil then raise Exception.create('Whoops! Unressolved parameter pointer.');
          {$endif}
          lastRep^.next:=context.newToken(token.location,'',tt_literal,L);
          L^.rereference;
        end else lastRep^.next:=context.newToken(token);
        lastRep:=lastRep^.next;
      end;
      lastRep^.next:=context.newToken(declaredAt,'',tt_semicolon);
      lastRep:=lastRep^.next;
      lastRep^.next:=context.newToken(declaredAt,'',endToken[b]);
      lastRep:=lastRep^.next;
    end;

  VAR tempInnerParam:P_listLiteral;
  begin
    lastRep:=nil;
    if (param= nil) and pattern.matchesNilPattern or
       (param<>nil) and pattern.matches(param^,context) then begin
      prepareResult;
      result:=true;
      context.callStackPush(callLocation,@self,param,expressionLiteralOrNil);
    end else if fallbackFeasible then begin
      prepareResult;
      result:=true;
      context.callStackPush(callLocation,@self,param,expressionLiteralOrNil);
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
  end;

CONSTRUCTOR T_subrule.createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
  VAR i:longint;
      r:P_subrule;
      embrace:boolean;
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
    init(srt_inline_for_literal,opLocation);
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
    if LHS^.literalType=lt_expression then begin
      r:=P_expressionLiteral(LHS)^.value;
      embrace:=r^.needEmbrace(C_opPrecedence[op]);
      if embrace then appendToExpression(tt_braceOpen);
      for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
      if embrace then appendToExpression(tt_braceClose);
    end else appendToExpression(LHS);
    appendToExpression(op);
    if RHS^.literalType=lt_expression then begin
      r:=P_expressionLiteral(RHS)^.value;
      embrace:=r^.needEmbrace(C_opPrecedence[op]);
      if embrace then appendToExpression(tt_braceOpen);
      for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
      if embrace then appendToExpression(tt_braceClose);
    end else appendToExpression(RHS);
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
    init(srt_inline_for_literal,funcLocation);
    origRule:=original^.value;
    pattern.create;
    setLength(preparedBody,1);
    with preparedBody[0] do begin token.create; token.define(declaredAt,intrinsicRuleId,tt_intrinsicRule,intrinsicRuleMap.get(intrinsicRuleId)); parIdx:=-1; end;
    appendToExpression(tt_braceOpen);
    for i:=0 to length(origRule^.preparedBody)-1 do appendToExpression(origRule^.preparedBody[i].token);
    appendToExpression(tt_braceClose);
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
    if      typ in [srt_inline_for_literal,srt_inline_for_while]
                                    then result:=                 '{'+result+'}'
    else if typ=srt_inline_for_each then result:=pattern.toString+'{'+result+'}'
    else                                 result:=pattern.toString+C_tokenInfo[tt_declare].defaultId+result;
  end;

FUNCTION subruleToStringImpl(CONST p:pointer; CONST lengthLimit:longint):string;
  begin
    result:=P_subrule(p)^.toString(true,lengthLimit);
  end;

FUNCTION subruleToArityImpl(CONST p:pointer):longint;
  begin
    result:=P_subrule(p)^.arity;
  end;

FUNCTION subruleAcceptsParCount(CONST p:pointer; CONST parCount:longint):boolean;
  begin
    result:=(P_subrule(p)^.arity<=parCount) and (P_subrule(p)^.pattern.hasOptionals or (P_subrule(p)^.arity=parCount));
  end;

FUNCTION T_subrule.directEvaluate(CONST params:P_listLiteral; CONST callLocation:T_tokenLocation; VAR context:T_evaluationContext; CONST callDepth:word):P_literal;
  VAR firstRep,lastRep:P_token;
  begin
    if replaces(params, nil, callLocation, firstRep,lastRep,context,false) then begin
      reduceExpression(firstRep,callDepth,context);
      if (context.adapters^.noErrors) and (firstRep<>nil) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal)
      then begin
        result:=firstRep^.data;
        result^.rereference;
      end else begin
        result:=nil;
      end;
      context.cascadeDisposeToken(firstRep);
    end else begin
      context.adapters^.raiseError('Cannot apply function to parameters: '+toParameterListString(params,true),declaredAt);
      result:=nil;
    end;
  end;

FUNCTION evaluateSubrule(CONST subruleLiteral:P_expressionLiteral; CONST location:T_tokenLocation; CONST parameters:P_listLiteral; CONST context:pointer):P_literal;
  begin
    result:=P_subrule(subruleLiteral^.value)^.directEvaluate(parameters,location,P_evaluationContext(context)^,0);
  end;

FUNCTION T_subrule.directEvaluateNullary(CONST callLocation:T_tokenLocation; VAR context:T_evaluationContext; CONST callDepth:word):P_literal;
  begin
    result:=directEvaluate(nil,callLocation,context,callDepth);
  end;

FUNCTION T_subrule.directEvaluateUnary(CONST x:P_literal; CONST callLocation:T_tokenLocation; VAR context:T_evaluationContext; CONST callDepth:word):P_literal;
  VAR param:P_listLiteral;
  begin
    param:=newOneElementListLiteral(x,true);
    result:=directEvaluate(param,callLocation,context,callDepth);
    disposeLiteral(param);
  end;

FUNCTION T_subrule.directEvaluateComparator(CONST x,y:P_literal; CONST callLocation:T_tokenLocation; VAR context:T_evaluationContext; CONST callDepth:word):boolean;
  VAR param:P_listLiteral;
      lit:P_literal;
  begin
    param:=newListLiteral(2)^.append(x,true)^.append(y,true);
    lit:=directEvaluate(param,callLocation,context,callDepth);
    disposeLiteral(param);
    if (context.adapters^.noErrors) and (lit<>nil) and (lit^.literalType=lt_boolean) then result:=P_boolLiteral(lit)^.value
    else begin
      result:=false;
      context.adapters^.raiseError('Cannot directly resolve comparator call with parameters ('+x^.toString+','+y^.toString+'). Make sure the function can be resolved without errors, returning only a boolean literal.',declaredAt);
    end;
    if lit<>nil then disposeLiteral(lit);
  end;

FUNCTION evaluateComparator(CONST subruleLiteral:P_expressionLiteral; CONST LHSComparand,RHScomparand:P_literal; CONST callLocation:T_tokenLocation; VAR adapters:T_adapters):boolean;
  VAR tempcontext:T_evaluationContext;
  begin
    tempcontext.createContext(P_adapters(@adapters),ct_silentlyRunAlone);
    result:=P_subrule(subruleLiteral^.value)^.directEvaluateComparator(LHSComparand,RHScomparand,callLocation,tempcontext,0);
    tempcontext.destroy;
  end;

FUNCTION T_subrule.directEvaluateAggregator(CONST x,y:P_literal;CONST callLocation:T_tokenLocation; VAR context:T_evaluationContext; CONST callDepth:word):P_literal;
  VAR param:P_listLiteral;
  begin
    param:=newListLiteral(2)^.append(x,true)^.append(y,true);
    result:=directEvaluate(param,callLocation,context,callDepth);
    disposeLiteral(param);
  end;

FUNCTION T_subrule.accept(CONST x:P_literal; VAR context:T_evaluationContext):boolean;
  VAR param:P_listLiteral;
      L:P_literal;
  begin
    param:=newOneElementListLiteral(x,true);
    L:=directEvaluate(param,declaredAt,context,0);
    dispose(param,destroy);
    result:=(L<>nil) and (L^.literalType=lt_boolean) and P_boolLiteral(L)^.value;
    if L<>nil then disposeLiteral(L);
  end;

FUNCTION T_subrule.getInlineValue:P_literal;
  begin
    if (length(preparedBody)<>1) or (typ in [srt_inline_for_each,srt_inline_for_while,srt_inline_for_literal]) then exit(nil);
    with preparedBody[0] do if token.tokType=tt_literal then begin
      result:=token.data;
      result^.rereference;
    end else result:=nil;
  end;

FUNCTION T_subrule.getCmdLineHelpText:ansistring;
  begin
    result:='  '+pattern.toCmdLineHelpStringString;
    if comment<>'' then result:=result+C_tabChar+'//'+replaceAll(comment,C_lineBreakChar,C_lineBreakChar+C_tabChar+C_tabChar+'//');
  end;

FUNCTION T_subrule.getDocTxt():ansistring;
  begin
    result:='@Line '+intToStr(declaredAt.line)+': '+C_tabChar;
    if typ=srt_normal_private then result:=result+'private ';
    result:=result+getId+';';
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
    result:=result+'<code>'+toHtmlCode(getId)+'</code></td></tr>';
  end;

FUNCTION T_subrule.getId:T_idString;
  begin
    case typ of
      srt_inline_for_literal: result:='expression';
      srt_inline_for_each   : result:='each body';
      srt_inline_for_while  : result:='while body';
      else begin
        if parent=nil then result:='?'
                      else result:=parent^.id;
        result:=result+pattern.toString;
      end;
    end;
  end;

FUNCTION T_subrule.getLocation:T_tokenLocation;
  begin
    result:=declaredAt;
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
        srt_inline_for_while  : result:='for_while';
        else result:='';
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
      if (bracketStack.dat[k]^.tokType in [tt_each,tt_parallelEach]) and
         (bracketStack.dat[k]^.txt=id) or (id=EACH_INDEX_IDENTIFIER) then exit(true);
    end;

  begin
    enterCriticalSection(firstCallCs);
    if not(functionIdsReady) then begin
      bracketStack.create;
      functionIdsReady:=true;
      for i:=0 to length(preparedBody)-1 do with preparedBody[i] do begin
        case token.tokType of
          tt_each,tt_parallelEach,tt_braceOpen: bracketStack.quietPush(@token);
          tt_braceClose:bracketStack.quietPop;
        end;
        if (parIdx<0) and (token.tokType=tt_identifier) and not(isEachIdentifier(token.txt)) then begin
          P_package(token.location.package)^.resolveRuleId(token,adapters);
          functionIdsReady:=functionIdsReady and (token.tokType<>tt_identifier);
        end;
      end;
      bracketStack.destroy;
    end;
    leaveCriticalSection(firstCallCs);
  end;

{$ifdef fullVersion}
VAR generateRowIdentification:T_identifiedInternalFunction;
FUNCTION generateRow(CONST f:P_expressionLiteral; CONST t0,t1:T_myFloat; CONST samples:longint; CONST location:T_tokenLocation; VAR context:T_evaluationContext):T_dataRow;
  VAR subRule:P_subrule;

      firstRep,lastRep:P_token;

      tRow :T_arrayOfDouble;
      TList:P_listLiteral;
      dataRow:T_dataRow;

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

  FUNCTION evaluatePEachExpressionOk:boolean;
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
      result:=evaluateOk;
    end;

  FUNCTION evaluateVectorExpressionOk:boolean;
    VAR params:T_listLiteral;
    begin
      params.create;
      params.append(TList,true);
      result:=(subRule^.replaces(@params,nil,location,firstRep,lastRep,tempcontext,false) or
               subRule^.replaces(@params,nil,location,firstRep,lastRep,tempcontext,true)) and evaluateOk;
      params.destroy;
    end;

  PROCEDURE constructInitialTList;
    VAR sampleIndex:longint;
        initialSampleCount:longint;
        t:T_myFloat;
    begin
      initialSampleCount:=(samples div 100)-1;
      if initialSampleCount< 1 then
         initialSampleCount:=1;
      TList:=newListLiteral;
      setLength(tRow,initialSampleCount+1);
      for sampleIndex:=0 to initialSampleCount do begin
        t:=t0+(t1-t0)*sampleIndex/initialSampleCount;
        TList^.appendReal(t);
        tRow[sampleIndex]:=t;
      end;
    end;

  PROCEDURE refineDataRow;
    VAR distThreshold:double=0;
        distThresholdSamples:longint=0;
        i,j,k:longint;
        oldTimes,newTimes:T_arrayOfDouble;
        oldRow  ,newRow  :T_dataRow;

        oLogRow:T_dataRow;
        t:double;
        stillOk:boolean=true;

    begin
      while stillOk and (length(dataRow)<samples) do begin
        //Prepare threshold:----------------------------------------------------
        oLogRow:=context.adapters^.plot.olxy(dataRow);
        for i:=1 to length(dataRow)-1 do
        if not(isNan(oLogRow[i,0])) and not(isNan(oLogRow[i-1,0])) and
           not(isNan(oLogRow[i,1])) and not(isNan(oLogRow[i-1,1])) then begin
          distThreshold:=distThreshold+sqr(oLogRow[i,0]-oLogRow[i-1,0])
                                      +sqr(oLogRow[i,1]-oLogRow[i-1,1]);
          inc(distThresholdSamples);
        end;
        distThreshold:=distThreshold/distThresholdSamples;
        //----------------------------------------------------:Prepare threshold
        //Prepare new time samples:---------------------------------------------
        setLength(newTimes,0);
        TList:=newListLiteral;
        for i:=1 to length(dataRow)-1 do
        if (isNan(oLogRow[i,0])) or (isNan(oLogRow[i-1,0])) or
           (isNan(oLogRow[i,1])) or (isNan(oLogRow[i-1,1])) or
          (sqr(oLogRow[i,0]-oLogRow[i-1,0])+sqr(oLogRow[i,1]-oLogRow[i-1,1])>=distThreshold) then begin
          t:=tRow[i]*0.5+tRow[i-1]*0.5;
          TList^.appendReal(t);
          append(newTimes,t);
        end;
        //fallback: ensure that new samples are always added
        if length(newTimes)=0 then begin
          {$ifdef debugMode}writeln(stdErr,'Fallback: refining all; ',length(tRow)-1,' new samples. TList^.size=',TList^.size);{$endif}
          setLength(newTimes,length(tRow)-1);
          for i:=1 to length(tRow)-1 do begin
            t:=tRow[i]*0.5+tRow[i-1]*0.5;
            TList^.appendReal(t);
            newTimes[i-1]:=t;
          end;
        end;
        //---------------------------------------------:Prepare new time samples
        //Prepare new point samples:--------------------------------------------
        stillOk:=dataReadyVectorized and evaluateVectorExpressionOk
              or dataReadyScalar     and evaluatePEachExpressionOk;
        if stillOk then begin
          resultLiteral:=P_listLiteral(firstRep^.data);
          if resultLiteral^.literalType in [lt_intList,lt_realList,lt_numList]
          then newRow:=newDataRow(resultLiteral,TList)
          else newRow:=newDataRow(resultLiteral);
          //Merge samples:------------------------------------------------------
          setLength(oldRow,length(dataRow));
          for i:=0 to length(oldRow)-1 do oldRow[i]:=dataRow[i];
          setLength(oldTimes,length(tRow));
          for i:=0 to length(oldTimes)-1 do oldTimes[i]:=tRow[i];
          setLength(dataRow,length(oldRow  )+length(newRow  ));
          setLength(tRow   ,length(oldTimes)+length(newTimes));
          {$ifdef debugMode}
          writeln('Merging samples of size ',length(oldRow),'+',length(newRow),'->',length(dataRow));
          writeln('                        ',length(oldTimes),'+',length(newTimes),'->',length(tRow));
          {$endif}
          i:=0;
          j:=0;
          k:=0;
          while (i<length(oldTimes)) and (j<length(newTimes)) do begin
            if oldTimes[i]<=newTimes[j] then begin
              tRow   [k]:=oldTimes[i];
              dataRow[k]:=oldRow[i];
              inc(i); inc(k);
            end else begin
              tRow   [k]:=newTimes[j];
              dataRow[k]:=newRow[j];
              inc(j); inc(k);
            end;
          end;
          while (i<length(oldTimes)) do begin
            tRow   [k]:=oldTimes[i];
            dataRow[k]:=oldRow[i];
            inc(i); inc(k);
          end;
          while (j<length(newTimes)) do begin
            tRow   [k]:=newTimes[j];
            dataRow[k]:=newRow[j];
            inc(j); inc(k);
          end;
          //------------------------------------------------------:Merge samples
        end;
        context.cascadeDisposeToken(firstRep);
        disposeLiteral(TList);
        //--------------------------------------------:Prepare new point samples
      end;
    end;

  begin
    subRule:=P_subrule(f^.value);

    tempAdapters.create;
    tempcontext.createContext(@tempAdapters,ct_silentlyRunAlone);

    constructInitialTList;

    dataReadyVectorized:=evaluateVectorExpressionOk;
    if not(dataReadyVectorized) then dataReadyScalar:=evaluatePEachExpressionOk;

    if dataReadyScalar or dataReadyVectorized then begin
      resultLiteral:=P_listLiteral(firstRep^.data);
      if resultLiteral^.literalType in [lt_intList,lt_realList,lt_numList]
      then dataRow:=newDataRow(resultLiteral,TList)
      else dataRow:=newDataRow(resultLiteral);
      context.cascadeDisposeToken(firstRep);
      disposeLiteral(TList);
      refineDataRow;
    end else begin
      context.adapters^.raiseError('Cannot prepare sample row using function '+f^.toString(),location);
      setLength(dataRow,0);
    end;
    tempAdapters.destroy;
    tempcontext.destroy;

    result:=dataRow;
  end;

{$endif}
FUNCTION expressionToTokens(CONST subruleLiteral:P_expressionLiteral):P_listLiteral;
  VAR sub:P_subrule;
      i:longint;
  begin
    sub:=P_subrule(subruleLiteral^.value);
    result:=newListLiteral(length(sub^.preparedBody));
    for i:=0 to length(sub^.preparedBody)-1 do with sub^.preparedBody[i] do begin
      if (token.tokType=tt_literal) and not(P_literal(token.data)^.literalType in [lt_void,lt_string])
      then result^.append(token.data,true)
      else result^.appendString(safeTokenToString(@token));
    end;
  end;

{$endif}
{$ifdef include_initialization}
{$ifdef fullVersion}
generateRowIdentification.create(PLOT_NAMESPACE,'generate-row-for-plot');
{$endif}
disposeSubruleCallback :=@disposeSubruleImpl;
subruleToStringCallback:=@subruleToStringImpl;
subruleToArityCallback:=@subruleToArityImpl;
subruleAcceptParCountCallback:=@subruleAcceptsParCount;
subruleApplyOpCallback :=@subruleApplyOpImpl;
evaluateCompatorCallback:=@evaluateComparator;
evaluateSubruleCallback:=@evaluateSubrule;
expressionToTokensCallback:=@expressionToTokens;
{$endif}
{$ifdef include_finalization}
{$ifdef fullVersion}
generateRowIdentification.destroy;
{$endif}
{$endif}
