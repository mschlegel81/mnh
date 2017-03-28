UNIT mnh_subrules;
INTERFACE
USES //basic classes
     sysutils, {$ifdef fullVersion}math,{$endif}
     //my utilities
     myGenerics,myStringUtil,
     //MNH:
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_litVar,
     mnh_tokens,
     tokenStack,
     mnh_contexts,
     mnh_tokenArray,
     {$ifdef fullVersion}
     mnh_plotData,mnh_funcs_plot,
     {$endif}
     mnh_funcs,mnh_funcs_math,mnh_funcs_list,mnh_funcs_strings,
     mnh_patterns;
TYPE
  T_subruleType=(srt_normal_public,
                 srt_normal_private,
                 srt_inline_for_literal,
                 srt_inline_for_each,
                 srt_inline_for_while);
  T_subruleAttribute=record
    key,value:string;
  end;

  T_preparedToken=record
    parIdx:longint;
    token:T_token;
  end;

  P_subrule=^T_subrule;
  T_subruleArray=array of P_subrule;

  T_subrule=object(T_expressionLiteral)
    private
      attributes:array of T_subruleAttribute;
      firstCallCs:TRTLCriticalSection;
      functionIdsReady:boolean;
      comment:ansistring;
      parent:P_objectWithIdAndLocation;
      typ:T_subruleType;
      declaredAt:T_tokenLocation;
      pattern:T_pattern;
      preparedBody:array of T_preparedToken;

      PROCEDURE updatePatternForInline;
      PROCEDURE constructExpression(CONST rep:P_token; VAR context:T_threadContext; CONST forEach:boolean);
      CONSTRUCTOR init(CONST srt: T_subruleType; CONST location: T_tokenLocation; CONST parentRule:P_objectWithIdAndLocation=nil);
      FUNCTION needEmbrace(CONST outerPrecedence:longint):boolean;
      CONSTRUCTOR createFromInlineWithOp(CONST original:P_expressionLiteral; CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation);
    public
      PROCEDURE resolveIds(CONST adapters:P_adapters);
      CONSTRUCTOR create           (CONST parent_:P_objectWithIdAndLocation; CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate,forWhile:boolean; VAR context:T_threadContext);
      CONSTRUCTOR createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; VAR context:T_threadContext);
      CONSTRUCTOR createFromInline (CONST rep:P_token; VAR context:T_threadContext);
      CONSTRUCTOR createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
      CONSTRUCTOR createPrimitiveAggregator(CONST tok:P_token; VAR context:T_threadContext);
      CONSTRUCTOR clone(CONST original:P_subrule);
      DESTRUCTOR destroy; virtual;
      FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation):P_expressionLiteral; virtual;

      //Pattern related:
      FUNCTION arity:longint; virtual;
      FUNCTION isVariadic:boolean;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      FUNCTION hasValidMainPattern:boolean;
      FUNCTION hasEquivalentPattern(CONST s:P_subrule):boolean;
      FUNCTION hidesSubrule(CONST s:P_subrule):boolean;
      FUNCTION getParameterNames:P_listLiteral;
      //Literal routines:
      FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION typeString:string; virtual;

      //Evaluation calls:
      FUNCTION replaces(CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_threadContext; CONST useUncurryingFallback:boolean):boolean;
      FUNCTION evaluateToBoolean(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):boolean; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
      FUNCTION evaluate         (CONST location:T_tokenLocation; CONST context:pointer; CONST parameters:P_listLiteral):P_literal;               virtual;

      FUNCTION getInlineValue:P_literal;
      FUNCTION getParentId:T_idString; virtual;

      //Inspection/documentation calls
      PROCEDURE setComment(CONST commentText:ansistring);
      PROPERTY getType:T_subruleType read typ;
      FUNCTION toDocString(CONST includePattern:boolean=true; CONST lengthLimit:longint=maxLongint):ansistring;
      FUNCTION getCmdLineHelpText:ansistring;
      FUNCTION getDocTxt:ansistring;
      FUNCTION getId:T_idString; virtual;
      FUNCTION getLocation:T_tokenLocation; virtual;
      FUNCTION inspect:P_mapLiteral;
      FUNCTION hasAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):boolean;
      FUNCTION getAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):T_subruleAttribute;
      PROCEDURE setAttributes(CONST attributeLines:T_arrayOfString; VAR adapters:T_adapters);
      FUNCTION acceptsSingleLiteral(CONST literalTypeToAccept:T_literalType):boolean;
      FUNCTION getAttributesLiteral:P_mapLiteral;
  end;

PROCEDURE resolveBuiltinIDs(CONST first:P_token; CONST adapters:P_adapters);
FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_threadContext):P_expressionLiteral;
PROCEDURE digestInlineExpression(VAR rep:P_token; VAR context:T_threadContext);
FUNCTION stringOrListToExpression(CONST L:P_literal; CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal;
IMPLEMENTATION
PROCEDURE digestInlineExpression(VAR rep:P_token; VAR context:T_threadContext);
  VAR t,prev,inlineRuleTokens:P_token;
      bracketLevel:longint=0;
      inlineSubRule:P_subrule;
  begin
    predigest(rep,nil,context.recycler,context.adapters);
    if (rep^.tokType<>tt_expBraceOpen) then begin
      context.adapters^.raiseError('Error creating subrule from inline; expression does not start with "{"',rep^.location);
      exit;
    end;
    t:=rep^.next; prev:=rep;
    inlineRuleTokens:=t;
    while (t<>nil) and ((t^.tokType<>tt_expBraceClose) or (bracketLevel>0)) do begin
      case t^.tokType of
        tt_expBraceOpen: begin
          digestInlineExpression(t,context);
          if t^.tokType=tt_expBraceOpen then inc(bracketLevel);
        end;
      end;
      prev:=t;
      t:=t^.next;
    end;
    if (t=nil) or (t^.tokType<>tt_expBraceClose) then begin
      context.adapters^.raiseError('Error creating subrule from inline; expression does not end with an }',rep^.location);
      exit;
    end;

    rep^.next:=t^.next; //remove expression from parent expression
    prev^.next:=nil; //unlink closing curly bracket
    context.recycler.disposeToken(t); //dispose closing curly bracket
    if context.adapters^.noErrors then begin
      new(inlineSubRule,createFromInline(inlineRuleTokens,context));
      if context.adapters^.noErrors then begin
        rep^.tokType:=tt_literal;
        rep^.data:=inlineSubRule;
      end else dispose(inlineSubRule,destroy);
    end;
  end;

PROCEDURE T_subrule.constructExpression(CONST rep:P_token; VAR context:T_threadContext; CONST forEach:boolean);
  VAR t:P_token;
      i:longint;
  begin
    setLength(preparedBody,0);
    t:=rep;
    i:=0;
    while t<>nil do begin
      if (i>=length(preparedBody)) then setLength(preparedBody,round(length(preparedBody)*1.1)+1);
      with preparedBody[i] do begin
        token:=t^;
        t^.tokType:=tt_EOL; t:=context.recycler.disposeToken(t);
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
      inc(i);
    end;
    setLength(preparedBody,i);
  end;

CONSTRUCTOR T_subrule.init(CONST srt:T_subruleType; CONST location:T_tokenLocation; CONST parentRule:P_objectWithIdAndLocation=nil);
  begin
    inherited init(lt_expression);
    initCriticalSection(firstCallCs);
    setLength(attributes,0);
    functionIdsReady:=false;
    comment:='';
    parent:=parentRule;
    typ:=srt;
    declaredAt:=location;
    setLength(preparedBody,0);
  end;

CONSTRUCTOR T_subrule.create(CONST parent_:P_objectWithIdAndLocation; CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate,forWhile:boolean; VAR context:T_threadContext);
  begin
    init(srt_normal_public,declAt,parent_);
    if       forWhile then typ:=srt_inline_for_while
    else if isPrivate then typ:=srt_normal_private;
    pattern:=pat;
    constructExpression(rep,context,false);
    resolveIds(nil);
  end;

CONSTRUCTOR T_subrule.createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; VAR context:T_threadContext);
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

CONSTRUCTOR T_subrule.createFromInline(CONST rep:P_token; VAR context:T_threadContext);
  VAR t:P_token;
      i:longint;
  begin
    init(srt_inline_for_literal,rep^.location);
    pattern.create;
    t:=rep;
    i:=0;
    while (t<>nil) do begin
      if (i>=length(preparedBody)) then setLength(preparedBody,round(length(preparedBody)*1.1)+1);
      with preparedBody[i] do begin
        token:=t^;
        t^.tokType:=tt_EOL;
        t:=context.recycler.disposeToken(t);
        token.next:=nil;
        parIdx:=-1;
      end;
      inc(i);
    end;
    setLength(preparedBody,i);
    updatePatternForInline;
  end;

CONSTRUCTOR T_subrule.createPrimitiveAggregator(CONST tok:P_token; VAR context:T_threadContext);
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
    inherited destroy;
    declaredAt.package:=nil;
    declaredAt.column:=0;
    declaredAt.line:=0;
    pattern.destroy;
    for i:=0 to length(preparedBody)-1 do preparedBody[i].token.destroy;
    setLength(preparedBody,0);
    setLength(attributes,0);
    doneCriticalSection(firstCallCs);
  end;

FUNCTION T_subrule.canApplyToNumberOfParameters(CONST parCount:longint):boolean;
  begin
    result:=(arity<=parCount) and pattern.hasOptionals or (arity=parCount);
  end;

FUNCTION T_subrule.isVariadic:boolean;
  begin
    result:=pattern.hasOptionals;
  end;

FUNCTION T_subrule.hasValidMainPattern:boolean;
  begin
    result:=pattern.isValidMainPattern;
  end;

FUNCTION T_subrule.hasEquivalentPattern(CONST s:P_subrule):boolean;
  begin
    result:=pattern.isEquivalent(s^.pattern);
  end;

FUNCTION T_subrule.hidesSubrule(CONST s:P_subrule):boolean;
  begin
    result:=pattern.hides(s^.pattern);
  end;

FUNCTION T_subrule.getParameterNames:P_listLiteral;
  begin
    result:=pattern.getParameterNames;
  end;

FUNCTION T_subrule.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR myTxt,otherTxt:ansistring;
  begin
    case relation of
      tt_operatorIn      : exit((other^.literalType in C_containingTypes[lt_expression]) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    if other^.literalType<>lt_expression then exit(false);
    myTxt   :=toString;
    otherTxt:=other^.toString;
    result:=(myTxt=otherTxt) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
         or (myTxt<otherTxt) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (myTxt>otherTxt) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_subrule.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newVoidLiteral; adapters.raiseError('Cannot negate expression. Please use "-1*..." instead.', minusLocation); end;

FUNCTION T_subrule.typeString: string; begin result:=C_typeString[literalType]+'('+intToStr(arity)+')'; end;

FUNCTION T_subrule.hash: T_hashInt;
  VAR i:longint;
      s:string;
  begin
    {$Q-}{$R-}
    s:= toString;
    result:=T_hashInt(lt_expression)+T_hashInt(length(s));
    for i:=1 to length(s) do result:=result*31+ord(s[i]);
    {$Q+}{$R+}
  end;

FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_threadContext):P_expressionLiteral;
  VAR subRule:P_subrule;
  begin
    new(subRule,createPrimitiveAggregator(tok,context));
    result:=subRule;
  end;

FUNCTION T_subrule.replaces(CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_threadContext; CONST useUncurryingFallback:boolean):boolean;
  VAR i:longint;
  FUNCTION fallbackFeasible:boolean;
    begin
      result:=useUncurryingFallback and
              //The given parameters must match
             (param<>nil) and pattern.matchesForFallback(param^,callLocation,context) and
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
        blocking:boolean;
        L:P_literal;
        remaining:P_listLiteral=nil;
    begin
      if not(functionIdsReady) then resolveIds(context.adapters);
      blocking:=typ in [srt_normal_private,srt_normal_public];
      firstRep:=context.recycler.newToken(declaredAt,'',beginToken[blocking]);
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
          end else L:=param^[parIdx];
          {$ifdef debugMode}
          if L=nil then raise Exception.create('Whoops! Unressolved parameter pointer.');
          {$endif}
          lastRep^.next:=context.recycler.newToken(token.location,'',tt_literal,L);
          L^.rereference;
        end else lastRep^.next:=context.recycler.newToken(token);
        lastRep:=lastRep^.next;
      end;
      lastRep^.next:=context.recycler.newToken(declaredAt,'',tt_semicolon);
      lastRep:=lastRep^.next;
      lastRep^.next:=context.recycler.newToken(declaredAt,'',endToken[blocking]);
      lastRep:=lastRep^.next;
    end;

  VAR tempInnerParam:P_listLiteral;
  begin
    lastRep:=nil;
    if (param= nil) and pattern.matchesNilPattern or
       (param<>nil) and pattern.matches(param^,callLocation,context) then begin
      prepareResult;
      result:=true;
      context.callStackPush(callLocation,@self,param,@self);
    end else if fallbackFeasible then begin
      prepareResult;
      result:=true;
      context.callStackPush(callLocation,@self,param,@self);
      tempInnerParam:=newListLiteral;
      for i:=length(pattern.sig) to param^.size-1 do tempInnerParam^.append(param^[i],true);
      lastRep^.next:=context.recycler.newToken(declaredAt,'',tt_parList,tempInnerParam);
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

FUNCTION subruleReplaces(CONST subrulePointer:pointer; CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_threadContext; CONST useUncurryingFallback:boolean):boolean;
  begin
    result:=P_subrule(subrulePointer)^.replaces(param,callLocation,firstRep,lastRep,context,useUncurryingFallback);
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
      then pattern.combineForInline(P_subrule(LHS)^.pattern,
                                    P_subrule(RHS)^.pattern)
      else pattern.clone(P_subrule(LHS)^.pattern);
    end else begin
      if RHS^.literalType=lt_expression
      then pattern.clone(P_subrule(RHS)^.pattern)
      else pattern.create;
    end;
    if LHS^.literalType=lt_expression then begin
      r:=P_subrule(LHS);
      embrace:=r^.needEmbrace(C_opPrecedence[op]);
      if embrace then appendToExpression(tt_braceOpen);
      for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
      if embrace then appendToExpression(tt_braceClose);
    end else appendToExpression(LHS);
    appendToExpression(op);
    if RHS^.literalType=lt_expression then begin
      r:=P_subrule(RHS);
      embrace:=r^.needEmbrace(C_opPrecedence[op]);
      if embrace then appendToExpression(tt_braceOpen);
      for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
      if embrace then appendToExpression(tt_braceClose);
    end else appendToExpression(RHS);
  end;

FUNCTION subruleApplyOpImpl (CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation):P_expressionLiteral;
  VAR newRule:P_subrule;
  begin
    new(newRule,createFromOp(LHS,op,RHS,tokenLocation));
    result:=newRule;
  end;

FUNCTION T_subrule.applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation):P_expressionLiteral;
  begin
    new(P_subrule(result),createFromInlineWithOp(@self,intrinsicRuleId,funcLocation));
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
    origRule:=P_subrule(original);
    pattern.create;
    setLength(preparedBody,1);
    with preparedBody[0] do begin token.create; token.define(declaredAt,intrinsicRuleId,tt_intrinsicRule,intrinsicRuleMap.get(intrinsicRuleId)); parIdx:=-1; end;
    appendToExpression(tt_braceOpen);
    for i:=0 to length(origRule^.preparedBody)-1 do appendToExpression(origRule^.preparedBody[i].token);
    appendToExpression(tt_braceClose);
    updatePatternForInline;
  end;

FUNCTION T_subrule.toString(CONST lengthLimit:longint=maxLongint): ansistring;
  begin result:=toDocString(true,lengthLimit); end;

PROCEDURE T_subrule.setComment(CONST commentText:ansistring);
  begin
    comment:=commentText;
  end;

FUNCTION T_subrule.toDocString(CONST includePattern:boolean=true; CONST lengthLimit:longint=maxLongint):ansistring;
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

FUNCTION T_subrule.evaluateToBoolean(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):boolean;
  VAR resultLiteral:P_literal;
  begin
    resultLiteral:=evaluateToLiteral(location,context,a,b);
    if (resultLiteral<>nil) and (resultLiteral^.literalType=lt_boolean) then begin
      result:=P_boolLiteral(resultLiteral)^.value;
    end else begin
      result:=false;
      P_threadContext(context)^.adapters^.raiseError('Expression does not return a boolean.',location);
    end;
  end;

FUNCTION T_subrule.evaluate(CONST location:T_tokenLocation; CONST context:pointer; CONST parameters:P_listLiteral):P_literal;
  VAR toReduce,dummy:P_token;
  begin
    if replaces(parameters,location,toReduce,dummy,P_threadContext(context)^,false)
    then begin
      P_threadContext(context)^.reduceExpression(toReduce);
      result:=P_threadContext(context)^.cascadeDisposeToLiteral(toReduce);
    end else result:=nil;
  end;

FUNCTION T_subrule.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
  VAR parameterList:T_listLiteral;
  begin
    parameterList.create(2);
    if a<>nil then parameterList.append(a,true);
    if b<>nil then parameterList.append(b,true);
    result:=evaluate(location,context,@parameterList);
    parameterList.destroy;
  end;

FUNCTION T_subrule.getInlineValue:P_literal;
  begin
    if (length(preparedBody)<>1) or (typ in [srt_inline_for_each,srt_inline_for_while,srt_inline_for_literal]) then exit(nil);
    with preparedBody[0] do if token.tokType=tt_literal then begin
      result:=token.data;
      result^.rereference;
    end else result:=nil;
  end;

FUNCTION T_subrule.getParentId:T_idString;
  begin
    result:=parent^.getId;
  end;

FUNCTION T_subrule.getCmdLineHelpText:ansistring;
  begin
    result:='  '+pattern.toCmdLineHelpStringString;
    if comment<>'' then result:=result+C_tabChar+COMMENT_PREFIX+replaceAll(comment,C_lineBreakChar,C_lineBreakChar+C_tabChar+COMMENT_PREFIX);
  end;

FUNCTION T_subrule.getDocTxt():ansistring;
  VAR att:T_subruleAttribute;
  begin
    result:=ECHO_MARKER+'@Line '+intToStr(declaredAt.line)+': '+C_tabChar;
    if typ=srt_normal_private then result:=result+'private ';
    result:=result+getId+';';
    if comment<>'' then result:=result+C_tabChar+COMMENT_PREFIX+replaceAll(comment,C_lineBreakChar,C_lineBreakChar+ECHO_MARKER+C_tabChar+C_tabChar+COMMENT_PREFIX);
    for att in attributes do begin
      result:=result+C_lineBreakChar+C_tabChar+'@'+att.key;
      if att.value<>'' then result:=result+'='+att.value;
    end;
  end;

FUNCTION T_subrule.getId:T_idString;
  begin
    case typ of
      srt_inline_for_literal: result:='expression';
      srt_inline_for_each   : result:='each body';
      srt_inline_for_while  : result:='while body';
      else begin
        if parent=nil then result:='?'
                      else result:=parent^.getId;
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

FUNCTION T_subrule.inspect:P_mapLiteral;
  FUNCTION srtString:string;
    begin
      case typ of
        srt_normal_public     : result:=PUBLIC_TEXT;
        srt_normal_private    : result:=C_tokenInfo[tt_modifier_private].defaultId;
        srt_inline_for_literal: result:='for_literal';
        srt_inline_for_each   : result:='for_each';
        srt_inline_for_while  : result:='for_while';
        else result:='';
      end;
    end;

  FUNCTION attributeList:P_mapLiteral;
    VAR attrib:T_subruleAttribute;
    begin
      result:=newMapLiteral;
      for attrib in attributes do result^.put(attrib.key,attrib.value);
    end;

  begin
    result:=newMapLiteral;
    P_mapLiteral(result)^.put('pattern'   ,pattern.toString)^
                         .put('location'  ,declaredAt      )^
                         .put('type'      ,srtString       )^
                         .put('comment'   ,comment         )^
                         .put('attributes',attributeList   ,false)^
                         .put('body'      ,toDocString(false) );
  end;

FUNCTION T_subrule.hasAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):boolean;
  begin
    result:=getAttribute(attributeKey,caseSensitive).key<>'';
  end;

FUNCTION T_subrule.getAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):T_subruleAttribute;
  CONST blankAttribute:T_subruleAttribute=(key:'';value:'');
        publicAttribute:T_subruleAttribute=(key:PUBLIC_TEXT;value:'');
        privateAttribute:T_subruleAttribute=(key:PRIVATE_TEXT;value:'');
  VAR attrib:T_subruleAttribute;
      useKey:string;
  begin
    if caseSensitive then begin
      if (attributeKey=privateAttribute.key) and (typ=srt_normal_private) then exit(privateAttribute);
      if (attributeKey=publicAttribute .key) and (typ=srt_normal_public ) then exit(publicAttribute);
      for attrib in attributes do if attrib.key=attributeKey then exit(attrib);
    end else begin
      useKey:=uppercase(attributeKey);
      if (useKey=uppercase(privateAttribute.key)) and (typ=srt_normal_private) then exit(privateAttribute);
      if (useKey=uppercase(publicAttribute .key)) and (typ=srt_normal_public ) then exit(publicAttribute);
      for attrib in attributes do if attrib.key=useKey then exit(attrib);
    end;
    result:=blankAttribute;
  end;

PROCEDURE T_subrule.setAttributes(CONST attributeLines:T_arrayOfString; VAR adapters:T_adapters);
  VAR line:string;
      parts:T_arrayOfString;
      newAttriuteIndex:longint=0;
  FUNCTION addAttribute(CONST key:ansistring):longint;
    VAR i:longint;
    begin
      for i:=0 to length(attributes)-1 do if attributes[i].key=key then begin
        adapters.raiseWarning('Duplicate attribute key "'+key+'"',declaredAt);
        exit(i);
      end;
      result:=length(attributes);
      setLength(attributes,result+1);
      attributes[result].key:=key;
    end;

  begin
    setLength(attributes,0);
    for line in attributeLines do begin
      parts:=split(trim(line),'=');
      if length(parts)>0 then begin
        newAttriuteIndex:=addAttribute(trim(parts[0]));
        dropFirst(parts,1);
        attributes[newAttriuteIndex].value:=trim(join(parts,'='));
      end;
    end;
  end;

FUNCTION T_subrule.acceptsSingleLiteral(CONST literalTypeToAccept:T_literalType):boolean;
  begin
    result:=(length(pattern.sig)=1) and (pattern.sig[0].acceptType(literalTypeToAccept));
  end;

FUNCTION T_subrule.getAttributesLiteral:P_mapLiteral;
  VAR i:longint;
  begin
    result:=newMapLiteral();
    for i:=0 to length(attributes)-1 do result^.put(attributes[i].key,attributes[i].value);
  end;

PROCEDURE resolveBuiltinIDs(CONST first:P_token; CONST adapters:P_adapters);
  VAR bracketStack:T_TokenStack;
  FUNCTION isEachIdentifier(CONST id:string):boolean;
    VAR k:longint;
    begin
      result:=false;
      for k:=0 to bracketStack.topIndex do
      if (bracketStack.dat[k]^.tokType in [tt_each,tt_parallelEach]) and
         (bracketStack.dat[k]^.txt=id) or (id=EACH_INDEX_IDENTIFIER) then exit(true);
    end;

  VAR t:P_token;
  begin
    t:=first;
    bracketStack.create;
    while t<>nil do begin
      with t^ do begin
        case tokType of
          tt_each,tt_parallelEach,tt_braceOpen: bracketStack.quietPush(t);
          tt_braceClose:bracketStack.quietPop;
        end;
        if (tokType=tt_identifier) and not(isEachIdentifier(txt)) then
          resolveRuleId(nil,adapters);
      end;
      t:=t^.next;
    end;
    bracketStack.destroy;

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
          token.resolveRuleId(nil,adapters);
          functionIdsReady:=functionIdsReady and (token.tokType<>tt_identifier);
        end;
      end;
      bracketStack.destroy;
    end;
    leaveCriticalSection(firstCallCs);
  end;

{$ifdef fullVersion}
VAR generateRowIdentification:T_identifiedInternalFunction;
FUNCTION generateRow(CONST f:P_expressionLiteral; CONST t0,t1:T_myFloat; CONST samples:longint; CONST location:T_tokenLocation; VAR context:T_threadContext):T_dataRow;
  VAR subRule:P_subrule=nil;

      firstRep:P_token=nil;
      lastRep:P_token=nil;

      tRow :T_arrayOfDouble;
      TList:P_listLiteral=nil;
      dataRow:T_dataRow;

      resultLiteral:P_listLiteral;

      tempcontext:T_evaluationContext;

      dataReadyVectorized:boolean=false;
      dataReadyScalar    :boolean=false;

  FUNCTION evaluateOk:boolean;
    begin
      tempcontext.threadContext^.reduceExpression(firstRep);
      result:=tempcontext.adapters^.noErrors and
              (firstRep<>nil) and
              (firstRep^.next=nil) and
              (firstRep^.tokType=tt_literal) and
              (P_literal(firstRep^.data)^.literalType in [lt_list,lt_realList,lt_intList,lt_numList]) and
              (P_listLiteral(firstRep^.data)^.size = TList^.size);
      tempcontext.adapters^.clearErrors;
      if not(result) then context.recycler.cascadeDisposeToken(firstRep);
    end;

  FUNCTION evaluatePEachExpressionOk:boolean;
    begin
      firstRep:=context.recycler.newToken(location,'',tt_literal,TList); TList^.rereference;
      firstRep^.next:=context.recycler.newToken(location,'t',tt_parallelEach);
      lastRep:=firstRep^.next;
      lastRep^.next:=context.recycler.newToken(location,'t',tt_identifier);
      lastRep:=lastRep^.next;
      lastRep^.next:=context.recycler.newToken(location,'',tt_ponFlipper);
      lastRep:=lastRep^.next;
      lastRep^.next:=context.recycler.newToken(location,'',tt_literal,f); f^.rereference;
      lastRep:=lastRep^.next;
      lastRep^.next:=context.recycler.newToken(location,'',tt_braceClose);
      result:=evaluateOk;
    end;

  FUNCTION evaluateVectorExpressionOk:boolean;
    VAR params:T_listLiteral;
    begin
      params.create(1);
      params.append(TList,true);
      result:=subRule^.replaces(@params,location,firstRep,lastRep,tempcontext.threadContext^,false) and evaluateOk;
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
        oLogRow:=context.adapters^.plot^.olxy(dataRow);
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
        context.recycler.cascadeDisposeToken(firstRep);
        disposeLiteral(TList);
        //--------------------------------------------:Prepare new point samples
      end;
    end;

  begin
    subRule:=P_subrule(f);
    tempcontext.createAndResetSilentContext(nil);
    constructInitialTList;

    dataReadyVectorized:=evaluateVectorExpressionOk;
    if not(dataReadyVectorized) then dataReadyScalar:=evaluatePEachExpressionOk;

    if dataReadyScalar or dataReadyVectorized then begin
      resultLiteral:=P_listLiteral(firstRep^.data);
      if resultLiteral^.literalType in [lt_intList,lt_realList,lt_numList]
      then dataRow:=newDataRow(resultLiteral,TList)
      else dataRow:=newDataRow(resultLiteral);
      context.recycler.cascadeDisposeToken(firstRep);
      disposeLiteral(TList);
      refineDataRow;
    end else begin
      context.adapters^.raiseError('Cannot prepare sample row using function '+f^.toString(),location);
      setLength(dataRow,0);
    end;
    tempcontext.destroy;
    result:=dataRow;
  end;

{$endif}

{$i mnh_func_defines.inc}
FUNCTION arity_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_expression)
    then result:=newIntLiteral(P_expressionLiteral(arg0)^.arity);
  end;

FUNCTION parameterNames_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_expression)
    then result:=P_subrule(arg0)^.getParameterNames;
  end;

FUNCTION tokenSplit_impl intFuncSignature;
  FUNCTION expressionToTokens(CONST subruleLiteral:P_expressionLiteral):P_listLiteral;
    VAR sub:P_subrule;
        i:longint;
    begin
      sub:=P_subrule(subruleLiteral);
      result:=newListLiteral(length(sub^.preparedBody));
      for i:=0 to length(sub^.preparedBody)-1 do with sub^.preparedBody[i] do begin
        if (token.tokType=tt_literal) and not(P_literal(token.data)^.literalType in [lt_void,lt_string])
        then result^.append(token.data,true)
        else result^.appendString(safeTokenToString(@token));
      end;
    end;

  VAR language:string='MNH';
      stringToSplit:ansistring;
      tokens:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      if (arg1^.literalType=lt_string)
      then language:=str1^.value
      else exit(nil);
    end;
    if (params<>nil) and (params^.size>=1) then
    case arg0^.literalType of
      lt_string:begin
        stringToSplit:=str0^.value;
        tokens:=tokenSplit(stringToSplit,language);
        result:=newListLiteral;
        for i:=0 to length(tokens)-1 do result:=listResult^.appendString(tokens[i]);
      end;
      lt_expression: if uppercase(language)='MNH' then
        result:=expressionToTokens(P_expressionLiteral(arg0));
    end;
  end;

FUNCTION stringToTokens(CONST s:ansistring; CONST location:T_tokenLocation; CONST package:P_abstractPackage; VAR context:T_threadContext):P_token;
  VAR lexer:T_lexer;
      statement:T_enhancedStatement;
  begin
    lexer.create(s,location,package);
    statement:=lexer.getNextStatement(context.recycler,context.adapters^);
    lexer.destroy;
    if statement.firstToken=nil then begin
      context.adapters^.raiseError('The parsed expression appears to be empty',location);
      exit(nil);
    end else if not(context.adapters^.noErrors) then begin
      exit(nil); //Parsing error ocurred
    end;
    result:=statement.firstToken;
  end;

FUNCTION listToTokens(CONST l:P_listLiteral; CONST location:T_tokenLocation; CONST package:P_abstractPackage; VAR context:T_threadContext):P_token;
  VAR last:P_token=nil;
      i:longint;
      subTokens:P_token;
  begin
    result:=nil;
    for i:=0 to L^.size-1 do begin
      if L^[i]^.literalType=lt_string
      then subTokens:=stringToTokens(P_stringLiteral(L^[i])^.value,location,package,context)
      else begin
        subTokens:=context.recycler.newToken(location,'',tt_literal,L^[i]);
        L^[i]^.rereference;
      end;
      if subTokens=nil then begin
        if result<>nil then context.recycler.cascadeDisposeToken(result);
        exit(nil);
      end;
      if result=nil then result:=subTokens
                    else last^.next:=subTokens;
      last:=subTokens^.last;
    end;
  end;

FUNCTION stringOrListToExpression(CONST L:P_literal; CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal;
  VAR first:P_token=nil;
      temp:P_token;
      package:P_abstractPackage;
  begin
    result:=nil;
    package:=P_abstractPackage(location.package);
    if      L^.literalType=lt_string      then first:=stringToTokens(P_stringLiteral(L)^.value,location,package,context)
    else if L^.literalType in C_listTypes then first:=listToTokens  (P_listLiteral  (L)       ,location,package,context);
    if first=nil then exit(nil);

    if not(first^.areBracketsPlausible(context.adapters^)) then begin
      context.recycler.cascadeDisposeToken(first);
      exit(nil);
    end;
    if first^.tokType<>tt_expBraceOpen then begin
      temp:=context.recycler.newToken(location,'',tt_expBraceOpen);
      temp^.next:=first; first:=temp;
      temp:=first^.last;
      temp^.next:=context.recycler.newToken(location,'',tt_expBraceClose);
    end;

    digestInlineExpression(first,context);
    if (context.adapters^.noErrors) and (first^.next<>nil) then context.adapters^.raiseError('The parsed expression goes beyond the expected limit... I know this is a fuzzy error. Sorry.',location);
    if not(context.adapters^.noErrors) then begin
      context.recycler.cascadeDisposeToken(first);
      exit(newErrorLiteral);
    end;
    if (first^.tokType<>tt_literal) or (P_literal(first^.data)^.literalType<>lt_expression) then begin
      context.recycler.disposeToken(first);
      context.adapters^.raiseSystemError('This is unexpected. The result of mnh_tokens.stringToExpression should be an expression!',location);
      exit(newErrorLiteral);
    end;
    result:=P_expressionLiteral(first^.data);
    first^.tokType:=tt_EOL;
    first^.data:=nil;
    context.recycler.disposeToken(first);
  end;

FUNCTION toExpression_imp intFuncSignature;
  FUNCTION primitiveExpression(CONST l:P_literal):P_expressionLiteral;
    VAR first:P_token;
    begin
      //Create token-series { <Literal> }
      first            :=context.recycler.newToken(tokenLocation,'',tt_expBraceOpen);
      first^.next      :=context.recycler.newToken(tokenLocation,'',tt_literal,l); L^.rereference;
      first^.next^.next:=context.recycler.newToken(tokenLocation,'',tt_expBraceClose);
      //Reduce to inline expression
      digestInlineExpression(first,context);
      result:=P_expressionLiteral(first^.data);
      first^.tokType:=tt_EOL;
      first^.data:=nil;
      context.recycler.disposeToken(first);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then case arg0^.literalType of
      lt_expression:  begin result:=arg0; arg0^.rereference; end;
      lt_boolean,lt_int,lt_real: result:=primitiveExpression(arg0);
      else result:=stringOrListToExpression(arg0,tokenLocation,context);
    end;
  end;


INITIALIZATION
  {$ifdef fullVersion}
  generateRowIdentification.create(PLOT_NAMESPACE,'generate-row-for-plot');
  mnh_funcs_plot.generateRow:=@generateRow;
  {$endif}
  subruleApplyOpCallback    :=@subruleApplyOpImpl;
  subruleReplacesCallback   :=@subruleReplaces;
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'arity'         ,@arity_imp         ,true,ak_unary,'arity(e:expression);//Returns the arity of expression e');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'parameterNames',@parameterNames_imp,true,ak_unary,'parameterNames(e:expression);//Returns the IDs of named parameters of e');
  registerRule(STRINGS_NAMESPACE,'tokenSplit'    ,@tokenSplit_impl   ,true,ak_variadic_1,'tokenSplit(S:string);#tokenSplit(S:string,language:string);//Returns a list of strings from S for a given language#//Languages: <code>MNH, Pascal, Java</code>');
  registerRule(TYPECAST_NAMESPACE       ,'toExpression'  ,@toExpression_imp  ,false,ak_unary,'toExpression(S);//Returns an expression parsed from S');

FINALIZATION
  {$ifdef fullVersion}
  generateRowIdentification.destroy;
  {$endif}
end.
