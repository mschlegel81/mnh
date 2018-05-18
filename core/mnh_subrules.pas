UNIT mnh_subrules;
INTERFACE
USES //basic classes
     sysutils,
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
     valueStore,
     {$ifdef fullVersion}
     mnh_plotData,mnh_funcs_plot,plotMath,
     mnh_debuggingVar,
     {$endif}
     mnh_funcs,mnh_funcs_math,mnh_funcs_mnh, mnh_funcs_strings,
     mnh_patterns;
TYPE
  T_subruleAttribute=record
    key,value:string;
  end;

  T_preparedToken=record
    parIdx:longint;
    token:T_token;
  end;

  P_expression=^T_expression;
  T_expression=object(T_expressionLiteral)
    private
      FUNCTION getParameterNames:P_listLiteral; virtual; abstract;
    public
      FUNCTION evaluateToBoolean(CONST location:T_tokenLocation; CONST context:pointer; CONST allowRaiseError:boolean; CONST a:P_literal=nil; CONST b:P_literal=nil):boolean; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      PROCEDURE validateSerializability(CONST adapters:P_adapters); virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION getParentId:T_idString; virtual;
      FUNCTION clone(CONST location:T_tokenLocation; CONST context:pointer):P_expressionLiteral; virtual;
  end;

  T_ruleMetaData=object
    private
      attributes:array of T_subruleAttribute;
      sideEffectWhitelist:T_sideEffects;
    public
      comment:ansistring;
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION hasAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):boolean;
      FUNCTION getAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):T_subruleAttribute;
      PROCEDURE setAttributes(CONST attributeLines:T_arrayOfString; CONST location:T_tokenLocation; VAR adapters:T_adapters);
      FUNCTION getAttributesLiteral:P_mapLiteral;
      FUNCTION getDocTxt:ansistring;
      PROCEDURE setComment(CONST commentText:ansistring);
  end;

  P_inlineExpression=^T_inlineExpression;
  T_inlineExpression=object(T_expression)
    private
      subruleCallCs:TRTLCriticalSection;
      functionIdsReady:boolean;
      pattern:T_pattern;
      preparedBody:array of T_preparedToken;
      customId:T_idString;
      meta:T_ruleMetaData;

      //save related:
      indexOfSave:longint;
      saveValueStore:P_valueStore;
      currentlyEvaluating:boolean;

      PROCEDURE updatePatternForInline;
      PROCEDURE constructExpression(CONST rep:P_token; VAR context:T_threadContext);
      CONSTRUCTOR init(CONST srt: T_expressionType; CONST location: T_tokenLocation);
      FUNCTION needEmbrace(CONST outerPrecedence:longint):boolean;
      CONSTRUCTOR createFromInlineWithOp(CONST original:P_inlineExpression; CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation);
      FUNCTION getParameterNames:P_listLiteral; virtual;
    public
      PROCEDURE resolveIds(CONST adapters:P_adapters);
      CONSTRUCTOR createForWhile   (CONST rep:P_token; CONST declAt:T_tokenLocation; VAR context:T_threadContext);
      CONSTRUCTOR createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; VAR context:T_threadContext);
      CONSTRUCTOR createFromInline (CONST rep:P_token; VAR context:T_threadContext; CONST customId_:T_idString='');
      CONSTRUCTOR createFromOp(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
      DESTRUCTOR destroy; virtual;
      FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; CONST threadContext:pointer):P_expressionLiteral; virtual;
      PROCEDURE validateSerializability(CONST adapters:P_adapters); virtual;
      //Pattern related:
      FUNCTION arity:longint; virtual;
      FUNCTION isVariadic:boolean;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      PROPERTY getPattern:T_pattern read pattern;
      //Literal routines:
      FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION clone(CONST location:T_tokenLocation; CONST context:pointer):P_expressionLiteral; virtual;

      //Evaluation calls:
      FUNCTION replaces(CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_threadContext):boolean;
      FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:pointer; CONST parameters:P_listLiteral):T_evaluationResult; virtual;

      //Inspection/documentation calls
      FUNCTION toDocString(CONST includePattern:boolean=true; CONST lengthLimit:longint=maxLongint):ansistring;
      FUNCTION getId:T_idString; virtual;
      FUNCTION inspect:P_mapLiteral; virtual;
      FUNCTION patternString:string;
  end;

  P_subruleExpression=^T_subruleExpression;
  T_subruleExpression=object(T_inlineExpression)
    private
      parent:P_objectWithIdAndLocation;
      publicSubrule:boolean;
    public
      PROPERTY metaData:T_ruleMetaData read meta;

      CONSTRUCTOR create(CONST parent_:P_objectWithIdAndLocation; CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate:boolean; VAR context:T_threadContext; VAR meta_:T_ruleMetaData);
      DESTRUCTOR destroy; virtual;
      FUNCTION hasValidMainPattern:boolean;
      FUNCTION hasValidValidCustomTypeCheckPattern(CONST forDuckTyping:boolean):boolean;
      FUNCTION hasEquivalentPattern(CONST s:P_subruleExpression):boolean;
      FUNCTION hidesSubrule(CONST s:P_subruleExpression):boolean;
      //Inspection/documentation calls
      FUNCTION getInlineValue:P_literal;
      FUNCTION getParentId:T_idString; virtual;
      FUNCTION getCmdLineHelpText:ansistring;
      FUNCTION getDocTxt:ansistring;
      FUNCTION getId:T_idString; virtual;
      FUNCTION inspect:P_mapLiteral; virtual;
      FUNCTION acceptsSingleLiteral(CONST literalTypeToAccept:T_literalType):boolean;
      {$ifdef fullVersion}
      PROCEDURE checkParameters(VAR context:T_threadContext);
      {$endif}
      PROPERTY isPublic:boolean read publicSubrule;
  end;

  P_builtinExpression=^T_builtinExpression;
  T_builtinExpression=object(T_expression)
    private
      func:P_intFuncCallback;
      FUNCTION getEquivalentInlineExpression(VAR context:T_threadContext):P_inlineExpression;
      FUNCTION getParameterNames:P_listLiteral; virtual;
    public
      CONSTRUCTOR create(CONST f:P_intFuncCallback; CONST location:T_tokenLocation);
      FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:pointer; CONST parameters:P_listLiteral):T_evaluationResult;  virtual;
      FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; CONST threadContext:pointer):P_expressionLiteral; virtual;
      FUNCTION arity:longint; virtual;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      FUNCTION getId:T_idString; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
  end;

  P_builtinGeneratorExpression=^T_builtinGeneratorExpression;
  T_builtinGeneratorExpression=object(T_expression)
    private
      FUNCTION getParameterNames:P_listLiteral; virtual;
    public
      CONSTRUCTOR create(CONST location:T_tokenLocation; CONST et:T_expressionType=et_builtinIteratable);
      FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:pointer; CONST parameters:P_listLiteral):T_evaluationResult;  virtual;
      FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; CONST threadContext:pointer):P_expressionLiteral; virtual;
      FUNCTION arity:longint; virtual;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual; abstract;
      FUNCTION getId:T_idString; virtual;
  end;

PROCEDURE resolveBuiltinIDs(CONST first:P_token; CONST adapters:P_adapters);
FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token):P_expressionLiteral;
PROCEDURE digestInlineExpression(VAR rep:P_token; VAR context:T_threadContext);
FUNCTION stringOrListToExpression(CONST L:P_literal; CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal;
FUNCTION getParametersForPseudoFuncPtr(CONST minPatternLength:longint; CONST variadic:boolean; CONST location:T_tokenLocation; VAR context:T_threadContext):P_token;
FUNCTION getParametersForUncurrying   (CONST givenParameters:P_listLiteral; CONST expectedArity:longint; CONST location:T_tokenLocation; VAR context:T_threadContext):P_token;
VAR createLazyMap:FUNCTION(CONST generator,mapping:P_expressionLiteral; CONST tokenLocation:T_tokenLocation):P_builtinGeneratorExpression;
IMPLEMENTATION
PROCEDURE digestInlineExpression(VAR rep:P_token; VAR context:T_threadContext);
  VAR t,prev,inlineRuleTokens:P_token;
      bracketLevel:longint=0;
      inlineSubRule:P_inlineExpression;
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

PROCEDURE T_inlineExpression.constructExpression(CONST rep:P_token; VAR context:T_threadContext);
  VAR t:P_token;
      i:longint;
      scopeLevel:longint=0;
      subExpressionLevel:longint=0;

  begin
    setLength(preparedBody,0);
    t:=rep;
    i:=0;
    indexOfSave:=-1;
    while t<>nil do begin
      if (i>=length(preparedBody)) then setLength(preparedBody,round(length(preparedBody)*1.1)+1);
      with preparedBody[i] do begin
        token:=t^;
        t^.tokType:=tt_EOL; t:=context.recycler.disposeToken(t);
        token.next:=nil;
        case token.tokType of
          tt_beginBlock   : begin inc(scopeLevel        ); parIdx:=-1; end;
          tt_endBlock     : begin dec(scopeLevel        ); parIdx:=-1; end;
          tt_expBraceOpen : begin inc(subExpressionLevel); parIdx:=-1; end;
          tt_expBraceClose: begin dec(subExpressionLevel); parIdx:=-1; end;
          tt_save: begin
            if subExpressionLevel=0 then begin
              if indexOfSave>=0     then context.adapters^.raiseError('save is allowed only once in a function body (other location: '+string(preparedBody[indexOfSave].token.location)+')',token.location)
              else if scopeLevel<>1 then context.adapters^.raiseError('save is allowed only on the scope level 1 (here: '+intToStr(scopeLevel)+')',token.location)
              else begin
                indexOfSave:=i;
                makeStateful(context.adapters,token.location);
              end;
            end;
            parIdx:=-1;
          end;
          tt_return: begin
            if not(typ in [et_subrule,et_subruleIteratable,et_subruleStateful,et_eachBody,et_whileBody]) then context.adapters^.raiseError('return statements are currently only allowed in subrules',token.location);
            parIdx:=-1;
          end;
          tt_optionalParameters: parIdx:=REMAINING_PARAMETERS_IDX;
          tt_identifier, tt_localUserRule, tt_importedUserRule, tt_parameterIdentifier, tt_intrinsicRule: begin
            parIdx:=pattern.indexOfId(token.txt);
            if parIdx>=0 then begin
              if parIdx>=REMAINING_PARAMETERS_IDX
              then token.tokType:=tt_parameterIdentifier
              else token.tokType:=tt_identifier;
            end else if (typ=et_eachBody) and (token.txt=EACH_INDEX_IDENTIFIER) then token.tokType:=tt_blockLocalVariable
            else if token.tokType<>tt_parameterIdentifier then token.tokType:=tt_identifier;
          end;
          else parIdx:=-1;
        end;
      end;
      inc(i);
    end;
    setLength(preparedBody,i);
  end;

CONSTRUCTOR T_inlineExpression.init(CONST srt: T_expressionType; CONST location: T_tokenLocation);
  begin
    inherited create(srt,location);
    meta.create;
    customId:='';
    initCriticalSection(subruleCallCs);
    functionIdsReady:=false;
    setLength(preparedBody,0);
    indexOfSave:=-1;
    saveValueStore:=nil;
    currentlyEvaluating:=false;
  end;

CONSTRUCTOR T_inlineExpression.createForWhile(CONST rep: P_token; CONST declAt: T_tokenLocation;
  VAR context: T_threadContext);
  begin
    init(et_whileBody,declAt);
    pattern.create;
    constructExpression(rep,context);
    resolveIds(nil);
  end;

CONSTRUCTOR T_subruleExpression.create(CONST parent_:P_objectWithIdAndLocation; CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate:boolean; VAR context:T_threadContext; VAR meta_:T_ruleMetaData);
  begin
    init(et_subrule,declAt);
    meta.destroy;
    meta:=meta_;
    publicSubrule:=not(isPrivate);
    pattern:=pat;
    constructExpression(rep,context);
    parent:=parent_;
    resolveIds(nil);
  end;

CONSTRUCTOR T_inlineExpression.createForEachBody(CONST parameterId: ansistring; CONST rep: P_token; VAR context: T_threadContext);
  begin
    init(et_eachBody,rep^.location);
    pattern.create;
    pattern.appendFreeId(parameterId,rep^.location);
    constructExpression(rep,context);
    resolveIds(nil);
  end;

FUNCTION T_inlineExpression.needEmbrace(CONST outerPrecedence: longint): boolean;
  VAR i:longint;
      level:longint=0;
  begin
    if length(preparedBody)<=1 then exit(false);
    level:=0;
    i:=length(preparedBody)-1;
    for i:=0 to length(preparedBody)-1 do with preparedBody[i].token do begin
      if tokType in C_openingBrackets then inc(level)
      else if tokType in C_closingBrackets then dec(level)
      else if (tokType in C_operators) and (level=0) and (C_opPrecedence[preparedBody[i].token.tokType]>outerPrecedence) then exit(true);
    end;
    result:=false;
  end;

PROCEDURE T_inlineExpression.updatePatternForInline;
  VAR i:longint;
  begin
    pattern.clear;
    for i:=0 to length(preparedBody)-1 do with preparedBody[i] do
    if token.tokType=tt_parameterIdentifier then begin
      parIdx:=pattern.indexOfIdForInline(token.txt,getLocation);
    end else if token.tokType=tt_optionalParameters then begin
      parIdx:=REMAINING_PARAMETERS_IDX;
      pattern.appendOptional;
    end;
  end;

CONSTRUCTOR T_inlineExpression.createFromInline(CONST rep: P_token; VAR context: T_threadContext; CONST customId_:T_idString);
  VAR t:P_token;
      i:longint;
      scopeLevel:longint=0;
      subExpressionLevel:longint=0;
  begin
    init(et_inline,rep^.location);
    customId:=customId_;
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
        case token.tokType of
          tt_beginBlock   : inc(scopeLevel        );
          tt_endBlock     : dec(scopeLevel        );
          tt_expBraceOpen : inc(subExpressionLevel);
          tt_expBraceClose: dec(subExpressionLevel);
          tt_save: if subExpressionLevel=0 then begin
            if indexOfSave>=0 then context.adapters^.raiseError('save is allowed only once in a function body (other location: '+string(preparedBody[indexOfSave].token.location)+')',token.location);
            if scopeLevel<>1 then context.adapters^.raiseError('save is allowed only on the scope level 1 (here: '+intToStr(scopeLevel)+')',token.location);
            makeStateful(context.adapters,token.location);
            indexOfSave:=i;
          end;
        end;
        parIdx:=-1;
      end;
      inc(i);
    end;
    setLength(preparedBody,i);
    updatePatternForInline;
  end;

DESTRUCTOR T_inlineExpression.destroy;
  VAR i:longint;
  begin
    pattern.destroy;
    for i:=0 to length(preparedBody)-1 do preparedBody[i].token.destroy;
    setLength(preparedBody,0);
    if (saveValueStore<>nil) then dispose(saveValueStore,destroy);
    meta.destroy;
    doneCriticalSection(subruleCallCs);
  end;

DESTRUCTOR T_subruleExpression.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_inlineExpression.canApplyToNumberOfParameters(CONST parCount: longint): boolean; begin result:=pattern.canApplyToNumberOfParameters(parCount); end;
FUNCTION T_builtinExpression.canApplyToNumberOfParameters(CONST parCount: longint): boolean;
  VAR meta:T_builtinFunctionMetaData;
  begin
    meta:=getMeta(func);
    result:=(parCount>=C_arityKind[meta.arityKind].fixedParameters) and
            (parCount<=C_arityKind[meta.arityKind].fixedParameters) or
                       C_arityKind[meta.arityKind].variadic;
  end;
FUNCTION T_builtinGeneratorExpression.canApplyToNumberOfParameters(CONST parCount: longint): boolean; begin result:=parCount=0; end;

FUNCTION T_inlineExpression.isVariadic: boolean; begin result:=pattern.isVariadic;                             end;
FUNCTION T_subruleExpression.hasValidMainPattern                                 :boolean; begin result:=pattern.isValidMainPattern;                     end;
FUNCTION T_subruleExpression.hasValidValidCustomTypeCheckPattern(CONST forDuckTyping:boolean):boolean; begin result:=pattern.isValidCustomTypeCheckPattern(forDuckTyping) end;
FUNCTION T_subruleExpression.hasEquivalentPattern(CONST s:P_subruleExpression)   :boolean; begin result:=pattern.isEquivalent(s^.pattern);               end;
FUNCTION T_subruleExpression.hidesSubrule        (CONST s:P_subruleExpression)   :boolean; begin result:=pattern.hides(s^.pattern);                      end;

FUNCTION T_inlineExpression.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR myTxt,otherTxt:ansistring;
  begin
    case relation of
      tt_operatorIn      : exit((other^.literalType in C_typeInfo[lt_expression].containedIn) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    if other^.literalType<>lt_expression then exit(false);
    myTxt   :=toString;
    otherTxt:=other^.toString;
    result:=(myTxt=otherTxt) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
         or (myTxt<otherTxt) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (myTxt>otherTxt) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token):P_expressionLiteral;
  begin
    if      tok^.tokType in C_operators   then new(P_builtinExpression(result),create(intFuncForOperator[tok^.tokType],tok^.location))
    else if tok^.tokType=tt_intrinsicRule then new(P_builtinExpression(result),create( P_intFuncCallback(tok^.data   ),tok^.location))
    else begin
      result:=nil;
      raise Exception.create('Invalid argument for createPrimitiveAggregatorLiteral('+safeTokenToString(tok)+')');
    end;
  end;

FUNCTION T_inlineExpression.replaces(CONST param: P_listLiteral; CONST callLocation: T_tokenLocation; OUT firstRep, lastRep: P_token; VAR context: T_threadContext): boolean;
  PROCEDURE updateBody;
    VAR i:longint;
        level:longint=1;
    begin
      for i:=indexOfSave+2 to length(preparedBody)-1 do with preparedBody[i] do
      case token.tokType of
        tt_assignNewBlockLocal: if level=1 then token.tokType:=tt_assignExistingBlockLocal;
        tt_beginBlock: inc(level);
        tt_endBlock:   dec(level);
      end;
    end;

  PROCEDURE prepareResult;
    CONST beginToken:array[false..true] of T_tokenType=(tt_beginExpression,tt_beginRule);
    VAR i:longint;
        firstRelevantToken,lastRelevantToken:longint;
        blocking:boolean;
        L:P_literal;
        allParams:P_listLiteral=nil;
        remaining:P_listLiteral=nil;
        previousValueStore:P_valueStore;
        previousSideEffectWhitelist:T_sideEffects;
        firstCallOfResumable:boolean=false;
        {$ifdef fullVersion}
        parametersNode:P_variableTreeEntryCategoryNode=nil;
        {$endif}
    begin
      enterCriticalSection(subruleCallCs);
      {$ifdef fullVersion}
      if tco_debugging in context.threadOptions then new(parametersNode,create(dvc_callParameter));
      {$endif}
      if (indexOfSave>=0) and currentlyEvaluating then begin
        firstRep:=nil;
        lastRep:=nil;
        leaveCriticalSection(subruleCallCs);
        context.adapters^.raiseError('Expressions/subrules containing a "save" construct must not be called recursively.',callLocation);
        exit;
      end;
      currentlyEvaluating:=true;

      if not(functionIdsReady) then resolveIds(context.adapters);
      blocking:=typ in C_subruleExpressionTypes;
      firstRep:=context.recycler.newToken(getLocation,'',beginToken[blocking]);
      lastRep:=firstRep;

      if (preparedBody[                     0].token.tokType=tt_beginBlock) and
         (preparedBody[length(preparedBody)-1].token.tokType=tt_endBlock  ) and
         (preparedBody[length(preparedBody)-2].token.tokType=tt_semicolon )
      then begin
        firstRelevantToken:=1;
        lastRelevantToken:=length(preparedBody)-3;
        if (indexOfSave>=0) and (saveValueStore<>nil) then firstRelevantToken:=indexOfSave+2;
      end else begin
        firstRelevantToken:=0;
        lastRelevantToken:=length(preparedBody)-1;
      end;

      {$ifdef fullVersion}
      if parametersNode<>nil then begin
        for i:=0 to pattern.arity-1 do parametersNode^.addEntry(pattern.idForIndexInline(i),param^.value[i],true);
      end;
      {$endif}

      for i:=firstRelevantToken to lastRelevantToken do with preparedBody[i] do begin
        if parIdx>=0 then begin
          if parIdx=ALL_PARAMETERS_PAR_IDX then begin
            if allParams=nil then begin
              allParams:=newListLiteral;
              if param<>nil then allParams^.appendAll(param);
              {$ifdef fullVersion}
              if parametersNode<>nil then parametersNode^.addEntry(ALL_PARAMETERS_TOKEN_TEXT,allParams,true);
              {$endif}
              allParams^.unreference;
            end;
            L:=allParams;
          end else if parIdx=REMAINING_PARAMETERS_IDX then begin
            if remaining=nil then begin
              if param=nil
              then remaining:=newListLiteral
              else remaining:=param^.tail(pattern.arity);
              {$ifdef fullVersion}
              if parametersNode<>nil then parametersNode^.addEntry(C_tokenInfo[tt_optionalParameters].defaultId,remaining,true);
              {$endif}
              remaining^.unreference;
            end;
            L:=remaining;
          end else begin
            L:=param^.value[parIdx];
          end;
          lastRep^.next:=context.recycler.newToken(token.location,'',tt_literal,L^.rereferenced);
        end else lastRep^.next:=context.recycler.newToken(token);
        lastRep:=lastRep^.next;
      end;

      {$ifdef fullVersion}
      context.callStackPush(callLocation,@self,parametersNode);
      {$endif}
      if indexOfSave>=0 then begin
        if saveValueStore=nil then begin
          new(saveValueStore,create);
          saveValueStore^.scopePush(blocking);
          firstCallOfResumable:=true;
        end;
        previousValueStore:=context.valueStore;
        context.valueStore:=saveValueStore;
        context.valueStore^.parentStore:=previousValueStore;

        previousSideEffectWhitelist:=context.setAllowedSideEffectsReturningPrevious(context.sideEffectWhitelist*meta.sideEffectWhitelist);
        firstRep:=context.recycler.disposeToken(firstRep);
        context.setAllowedSideEffectsReturningPrevious(previousSideEffectWhitelist);

        context.reduceExpression(firstRep);
        if firstRep=nil
        then lastRep:=nil
        else lastRep:=firstRep^.last;
        context.valueStore:=previousValueStore;
        if firstCallOfResumable then begin
          if context.adapters^.noErrors then begin
            updateBody;
          end else begin
            dispose(saveValueStore,destroy);
            saveValueStore:=nil;
          end;
        end;
      end else begin
        lastRep^.next:=context.recycler.newToken(getLocation,'',tt_semicolon);
        lastRep:=lastRep^.next;
        lastRep^.next:=context.getNewEndToken(blocking,getLocation);
        context.setAllowedSideEffectsReturningPrevious(context.sideEffectWhitelist*meta.sideEffectWhitelist);
        lastRep:=lastRep^.next;
      end;
      currentlyEvaluating:=false;
      leaveCriticalSection(subruleCallCs);
    end;

  begin
    lastRep:=nil;
    if (param= nil) and pattern.matchesNilPattern or
       (param<>nil) and pattern.matches(param^,callLocation,context) then begin
      prepareResult;
      result:=lastRep<>nil;
    end else begin
      result:=false;
      case typ of
        et_eachBody: begin
          if param=nil then context.adapters^.raiseError('Cannot evaluate each body with the given number of parameters; Got none, expected '+intToStr(pattern.arity),getLocation)
                       else context.adapters^.raiseError('Cannot evaluate each body with the given number of parameters; Got '+intToStr(param^.size)+', expected '+intToStr(pattern.arity),getLocation);
        end;
        et_inline,et_inlineIteratable,et_inlineStateful: begin
          if param=nil then context.adapters^.raiseError('Cannot evaluate inline function '+toString+' with the given number of parameters; Got none, expected '+intToStr(pattern.arity),getLocation)
                       else context.adapters^.raiseError('Cannot evaluate inline function '+toString+' with the given number of parameters; Got '+intToStr(param^.size)+', expected '+intToStr(pattern.arity),getLocation);
        end;
      end;
    end;
  end;

FUNCTION subruleReplaces(CONST subrulePointer:pointer; CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_threadContext):boolean;
  begin
    result:=P_subruleExpression(subrulePointer)^.replaces(param,callLocation,firstRep,lastRep,context);
  end;

CONSTRUCTOR T_inlineExpression.createFromOp(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST opLocation: T_tokenLocation);
  VAR i:longint;
      r:P_inlineExpression;
      embrace:boolean;
      preparedBodyLength:longint=0;

  PROCEDURE appendToExpression(VAR T:T_preparedToken);
    begin
      with preparedBody[preparedBodyLength] do begin
        token.create;
        token.define(T.token);
        parIdx:=T.parIdx;
        if parIdx>=0 then token.txt:=pattern.idForIndexInline(parIdx);
      end;
      inc(preparedBodyLength);
    end;

  PROCEDURE appendToExpression(CONST L:P_literal);
    begin
      L^.rereference;
      with preparedBody[preparedBodyLength] do begin
        token.create;
        token.define(opLocation,'',tt_literal,L);
        parIdx:=-1;
      end;
      inc(preparedBodyLength);
    end;

  PROCEDURE appendToExpression(CONST op:T_tokenType);
    begin
      with preparedBody[preparedBodyLength] do begin
        token.create;
        token.define(opLocation,'',op);
        parIdx:=-1;
      end;
      inc(preparedBodyLength);
    end;

  begin
    init(et_inline,opLocation);
    preparedBodyLength:=1;
    //Pattern (including final parameter names)
    if LHS^.literalType=lt_expression then begin
      //Lenght of prepared body of LHS, maybe brackets
      if RHS^.literalType=lt_expression
      then pattern.combineForInline(P_subruleExpression(LHS)^.pattern,
                                    P_subruleExpression(RHS)^.pattern,
                                    P_subruleExpression(LHS)^.getLocation)
      else pattern.clone(P_subruleExpression(LHS)^.pattern);
    end else begin
      if RHS^.literalType=lt_expression
      then pattern.clone(P_subruleExpression(RHS)^.pattern)
      else pattern.create;
    end;
    //calculate needed tokens
    preparedBodyLength:=1;
    if RHS^.literalType=lt_expression
    then inc(preparedBodyLength,length(P_inlineExpression(RHS)^.preparedBody)+2)
    else inc(preparedBodyLength);
    if LHS^.literalType=lt_expression
    then inc(preparedBodyLength,length(P_inlineExpression(LHS)^.preparedBody)+2)
    else inc(preparedBodyLength);
    setLength(preparedBody,preparedBodyLength);
    preparedBodyLength:=0;
    //construct
    if LHS^.literalType=lt_expression then begin
      r:=P_inlineExpression(LHS);
      if r^.typ in C_statefulExpressionTypes   then makeStateful(nil,opLocation);
      if r^.typ in C_iteratableExpressionTypes then makeIteratable(nil,opLocation);
      embrace:=r^.needEmbrace(C_opPrecedence[op]);
      if embrace then appendToExpression(tt_braceOpen);
      for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
      if embrace then appendToExpression(tt_braceClose);
    end else appendToExpression(LHS);
    appendToExpression(op);
    if RHS^.literalType=lt_expression then begin
      r:=P_inlineExpression(RHS);
      if r^.typ in C_statefulExpressionTypes   then makeStateful(nil,opLocation);
      if r^.typ in C_iteratableExpressionTypes then makeIteratable(nil,opLocation);
      embrace:=r^.needEmbrace(C_opPrecedence[op]);
      if embrace then appendToExpression(tt_braceOpen);
      for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
      if embrace then appendToExpression(tt_braceClose);
    end else appendToExpression(RHS);
    setLength(preparedBody,preparedBodyLength);
  end;

FUNCTION newIdentityRule(VAR context:T_threadContext; CONST location:T_tokenLocation):P_subruleExpression;
  begin
    new(result,
        createFromInline(context.recycler.newToken(location,'$x',tt_parameterIdentifier,nil),
                         context)); //={$x}
  end;

FUNCTION subruleApplyOpImpl(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST threadContext:pointer):P_literal;
  VAR newRule:P_subruleExpression;
      LHSinstead:P_literal=nil;
      RHSinstead:P_literal=nil;
  begin
    if (LHS^.literalType=lt_expression) and (P_expressionLiteral(LHS)^.typ in C_iteratableExpressionTypes) and not(RHS^.literalType=lt_expression) then begin
      // generator+3 -> lazyMap(generator,{$x+3})
      LHSinstead:=newIdentityRule(P_threadContext(threadContext)^,tokenLocation);
      new(newRule,createFromOp(LHSinstead,op,RHS,tokenLocation));
      disposeLiteral(LHSinstead);
      RHSinstead:=newRule; //={$x+3}
      result:=createLazyMap(P_expressionLiteral(LHS),P_expressionLiteral(RHSinstead),tokenLocation);
      disposeLiteral(RHSinstead);
      exit(result);
    end;
    if (RHS^.literalType=lt_expression) and (P_expressionLiteral(RHS)^.typ in C_iteratableExpressionTypes) and not(LHS^.literalType=lt_expression) then begin
      // 2^generator -> lazyMap(generator,{2^$x})
      RHSinstead:=newIdentityRule(P_threadContext(threadContext)^,tokenLocation);
      new(newRule,createFromOp(LHS,op,RHSinstead,tokenLocation));
      disposeLiteral(RHSinstead);
      LHSinstead:=newRule; //={2^$x}
      result:=createLazyMap(P_expressionLiteral(RHS),P_expressionLiteral(LHSinstead),tokenLocation);
      disposeLiteral(LHSinstead);
      exit(result);
    end;
    if (LHS^.literalType=lt_expression) and (P_expressionLiteral(LHS)^.typ in C_statefulExpressionTypes) or
       (RHS^.literalType=lt_expression) and (P_expressionLiteral(RHS)^.typ in C_statefulExpressionTypes) then begin
      P_threadContext(threadContext)^.adapters^.raiseError('Cannot perform direct operations on stateful expressions.',tokenLocation);
      exit(newVoidLiteral);
    end;
    if (LHS^.literalType=lt_expression) and (P_expressionLiteral(LHS)^.typ in C_builtinExpressionTypes)
    then LHSinstead:=P_builtinExpression(LHS)^.getEquivalentInlineExpression(P_threadContext(threadContext)^)
    else LHSinstead:=LHS^.rereferenced;
    if (RHS^.literalType=lt_expression) and (P_expressionLiteral(RHS)^.typ in C_builtinExpressionTypes)
    then RHSinstead:=P_builtinExpression(RHS)^.getEquivalentInlineExpression(P_threadContext(threadContext)^)
    else RHSinstead:=RHS^.rereferenced;
    new(newRule,createFromOp(LHSinstead,op,RHSinstead,tokenLocation));
    result:=newRule;
    disposeLiteral(LHSinstead);
    disposeLiteral(RHSinstead);
  end;

FUNCTION T_inlineExpression.applyBuiltinFunction(CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; CONST threadContext:pointer): P_expressionLiteral;
  begin
    new(P_inlineExpression(result),createFromInlineWithOp(@self,intrinsicRuleId,funcLocation));
  end;

FUNCTION T_builtinExpression.applyBuiltinFunction(CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; CONST threadContext:pointer): P_expressionLiteral;
  VAR temp:P_inlineExpression;
  begin
    temp:=getEquivalentInlineExpression(P_threadContext(threadContext)^);
    new(P_inlineExpression(result),createFromInlineWithOp(temp,intrinsicRuleId,funcLocation));
    disposeLiteral(temp);
  end;

FUNCTION T_builtinGeneratorExpression.applyBuiltinFunction(CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; CONST threadContext:pointer): P_expressionLiteral;
  VAR identity:P_subruleExpression;
      mapper  :P_expressionLiteral;
  begin
    identity:=newIdentityRule(P_threadContext(threadContext)^,funcLocation);
    mapper:=identity^.applyBuiltinFunction(intrinsicRuleId,funcLocation,threadContext);
    disposeLiteral(identity);
    if mapper=nil then exit(nil);
    result:=createLazyMap(@self,mapper,funcLocation);
    disposeLiteral(mapper);
  end;

PROCEDURE T_inlineExpression.validateSerializability(CONST adapters: P_adapters);
  begin
    if adapters=nil then exit;
    if (typ<>et_inline) then adapters^.raiseError('Expression literal '+toString(20)+' is not serializable',getLocation);
  end;

PROCEDURE T_expression.validateSerializability(CONST adapters: P_adapters);
  begin
    if adapters<>nil then adapters^.raiseError('Expression '+toString()+' cannot be serialized',getLocation);
  end;

CONSTRUCTOR T_inlineExpression.createFromInlineWithOp(
  CONST original: P_inlineExpression; CONST intrinsicRuleId: string;
  CONST funcLocation: T_tokenLocation);
  VAR i:longint;
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
    init(original^.typ,funcLocation);
    pattern.create;
    if intrinsicRuleId<>'' then begin
      setLength(preparedBody,1);
      with preparedBody[0] do begin token.create; token.define(getLocation,intrinsicRuleId,tt_intrinsicRule,intrinsicRuleMap.get(intrinsicRuleId)); parIdx:=-1; end;
      appendToExpression(tt_braceOpen);
    end else setLength(preparedBody,0);
    for i:=0 to length(original^.preparedBody)-1 do appendToExpression(original^.preparedBody[i].token);
    indexOfSave:=original^.indexOfSave;
    if original^.saveValueStore<>nil then saveValueStore:=original^.saveValueStore^.clone;
    if intrinsicRuleId<>'' then appendToExpression(tt_braceClose);
    meta:=original^.meta;
    updatePatternForInline;
  end;

FUNCTION T_inlineExpression.getParameterNames: P_listLiteral;
  begin
    result:=pattern.getParameterNames;
  end;

FUNCTION getParametersForPseudoFuncPtr(CONST minPatternLength:longint; CONST variadic:boolean; CONST location:T_tokenLocation; VAR context:T_threadContext):P_token;
  FUNCTION newToken(CONST tokenText:T_idString; CONST tokenType:T_tokenType; CONST ptr:pointer=nil):P_token; inline;
    begin result:=context.recycler.newToken(location,tokenText,tokenType,ptr); end;
  VAR last:P_token;
      i:longint;
  begin
    result:=newToken('',tt_braceOpen);
    last:=result;
    for i:=0 to minPatternLength-1 do begin
      if i>0 then begin
        last^.next:=newToken('',tt_separatorComma);
        last:=last^.next;
      end;
      last^.next:=newToken('$'+intToStr(i),tt_parameterIdentifier);
      last:=last^.next;
    end;
    last^.next:=newToken('',tt_braceClose);
    last:=last^.next;
    if variadic then begin
      last^.next:=newToken('',tt_listToParameterList);
      last:=last^.next;
      if minPatternLength>0 then begin
        last^.next:=newToken('',tt_optionalParameters);
      end else begin
        last^.next:=newToken(ALL_PARAMETERS_TOKEN_TEXT,tt_parameterIdentifier);
      end;
    end;
  end;

FUNCTION getParametersForUncurrying(CONST givenParameters:P_listLiteral; CONST expectedArity:longint; CONST location:T_tokenLocation; VAR context:T_threadContext):P_token;
  FUNCTION newToken(CONST tokenText:T_idString; CONST tokenType:T_tokenType; CONST ptr:pointer=nil):P_token; inline;
    begin result:=context.recycler.newToken(location,tokenText,tokenType,ptr); end;
  VAR last:P_token;
      i:longint;
  begin
    result:=newToken('',tt_parList_constructor,givenParameters^.rereferenced);
    last:=result;
    for i:=givenParameters^.size to expectedArity-1 do begin
      last^.next:=newToken('$'+intToStr(i-givenParameters^.size),tt_parameterIdentifier);
      last:=last^.next;
    end;
    last^.next:=newToken('',tt_braceClose);
    last:=last^.next;
  end;

FUNCTION T_builtinExpression.getEquivalentInlineExpression(VAR context:T_threadContext): P_inlineExpression;
  VAR meta:T_builtinFunctionMetaData;
      first:P_token;
  begin
    meta:=getMeta(func);
    first:=context.recycler.newToken(getLocation,meta.qualifiedId,tt_intrinsicRule,func);
    first^.next:=getParametersForPseudoFuncPtr(C_arityKind[meta.arityKind].fixedParameters,C_arityKind[meta.arityKind].variadic,getLocation,context);
    new(result,createFromInline(first,context));
  end;

FUNCTION T_builtinExpression.getParameterNames: P_listLiteral;
  VAR i:longint;
  begin
    result:=newListLiteral(arity);
    for i:=0 to arity-1 do result^.appendString('$'+intToStr(i));
  end;

FUNCTION T_builtinGeneratorExpression.getParameterNames: P_listLiteral; begin result:=newListLiteral(); end;

CONSTRUCTOR T_builtinExpression.create(CONST f: P_intFuncCallback; CONST location:T_tokenLocation);
  begin
    inherited create(et_builtin,location);
    func:=f;
  end;

CONSTRUCTOR T_builtinGeneratorExpression.create(CONST location:T_tokenLocation; CONST et:T_expressionType=et_builtinIteratable);
  begin
    inherited create(et,location);
  end;

FUNCTION T_inlineExpression.toString(CONST lengthLimit: longint): ansistring;
  begin result:=toDocString(true,lengthLimit); end;
FUNCTION T_expression.toString(CONST lengthLimit: longint): ansistring;
  begin result:=C_tokenInfo[tt_pseudoFuncPointer].defaultId+getId; end;

FUNCTION T_inlineExpression.toDocString(CONST includePattern: boolean; CONST lengthLimit: longint): ansistring;
  VAR i,remainingLength:longint;
      prevIdLike,idLike:boolean;
  begin
    prevIdLike:=false;
    result:='';
    remainingLength:=lengthLimit;
    for i:=0 to length(preparedBody)-1 do if remainingLength>0 then begin
      result:=result+preparedBody[i].token.toString(prevIdLike,idLike,remainingLength);
      remainingLength:=lengthLimit-length(result);
      prevIdLike:=idLike;
    end;
    if not(includePattern) then exit(result);
    if      typ in C_inlineExpressionTypes
                            then result:=                 '{'+result+'}'
    else if typ=et_eachBody then result:=pattern.toString+'{'+result+'}'
    else                         result:=pattern.toString+C_tokenInfo[tt_declare].defaultId+result;
  end;

FUNCTION T_expression.evaluateToBoolean(CONST location: T_tokenLocation; CONST context: pointer; CONST allowRaiseError:boolean; CONST a: P_literal; CONST b: P_literal): boolean;
  VAR resultLiteral:P_literal;
  begin
    resultLiteral:=evaluateToLiteral(location,context,a,b).literal;
    if (resultLiteral<>nil) and (resultLiteral^.literalType=lt_boolean) then begin
      result:=P_boolLiteral(resultLiteral)^.value;
    end else begin
      result:=false;
      if allowRaiseError then P_threadContext(context)^.adapters^.raiseError('Expression does not return a boolean.',location);
    end;
  end;

FUNCTION T_inlineExpression.evaluate(CONST location: T_tokenLocation; CONST context: pointer; CONST parameters: P_listLiteral): T_evaluationResult;
  VAR toReduce,dummy:P_token;
  begin
    if replaces(parameters,location,toReduce,dummy,P_threadContext(context)^)
    then begin
      result:=P_threadContext(context)^.reduceToLiteral(toReduce);
    end else result.literal:=nil;
  end;

FUNCTION T_builtinExpression.evaluate(CONST location: T_tokenLocation; CONST context: pointer; CONST parameters: P_listLiteral): T_evaluationResult;
  begin
    {$ifdef fullVersion} P_threadContext(context)^.callStackPush(location,@self,nil); {$endif}
    result.literal:=func(parameters,location,P_threadContext(context)^);
    result.triggeredByReturn:=false;
    {$ifdef fullVersion} P_threadContext(context)^.callStackPop(nil); {$endif}
  end;

FUNCTION T_builtinGeneratorExpression.evaluate(CONST location: T_tokenLocation; CONST context: pointer; CONST parameters: P_listLiteral): T_evaluationResult;
  begin
    if (parameters<>nil) and (parameters^.size<>0) then exit(NIL_EVAL_RESULT);
    {$ifdef fullVersion} P_threadContext(context)^.callStackPush(location,@self,nil); {$endif}
    result:=evaluateToLiteral(location,context);
    {$ifdef fullVersion} P_threadContext(context)^.callStackPop(nil); {$endif}
  end;

FUNCTION T_expression.evaluateToLiteral(CONST location: T_tokenLocation;
  CONST context: pointer; CONST a: P_literal; CONST b: P_literal): T_evaluationResult;
  VAR parameterList:T_listLiteral;
  begin
    parameterList.create(2);
    if a<>nil then parameterList.append(a,true);
    if b<>nil then parameterList.append(b,true);
    result:=evaluate(location,context,@parameterList);
    parameterList.destroy;
  end;

FUNCTION T_subruleExpression.getInlineValue: P_literal;
  begin
    if (length(preparedBody)<>1) or not(typ in C_subruleExpressionTypes) then exit(nil);
    with preparedBody[0] do if token.tokType=tt_literal then begin
      result:=token.data;
      result^.rereference;
    end else result:=nil;
  end;

FUNCTION T_expression.getParentId: T_idString; begin result:=''; end;

FUNCTION T_expression.clone(CONST location: T_tokenLocation; CONST context: pointer): P_expressionLiteral;
  begin
    raise Exception.create('Clone is not implemented for expressions of type '+C_expressionTypeString[typ]);
    result:=nil;
  end;

FUNCTION T_inlineExpression.clone(CONST location:T_tokenLocation; CONST context:pointer):P_expressionLiteral;
  begin
    new(P_inlineExpression(result),createFromInlineWithOp(@self,'',location));
  end;

FUNCTION T_subruleExpression.getParentId: T_idString; begin if parent=nil then result:='' else result:=parent^.getId; end;

FUNCTION T_subruleExpression.getCmdLineHelpText: ansistring;
  begin
    result:='  '+pattern.toCmdLineHelpStringString;
    if meta.comment<>'' then result:=result+C_tabChar+COMMENT_PREFIX+replaceAll(meta.comment,C_lineBreakChar,C_lineBreakChar+C_tabChar+COMMENT_PREFIX);
  end;

FUNCTION T_subruleExpression.getDocTxt: ansistring;
  begin
    result:=meta.getDocTxt+ECHO_MARKER;
    if not(publicSubrule) then result:=result+'private ';
    result:=result+getId+';'+C_tabChar+COMMENT_PREFIX+'declared '+ansistring(getLocation);
  end;

FUNCTION T_inlineExpression.getId: T_idString;
  begin
    if customId='' then result:='inline_expression' else result:=customId;
  end;

FUNCTION T_subruleExpression.getId: T_idString;
  begin
    if customId<>'' then exit(customId);
    if parent=nil then result:='?'
                  else result:=parent^.getId;
    result:=result+pattern.toString;
  end;

FUNCTION T_builtinExpression.getId:T_idString;
  VAR unqalified:string;
  begin
    result:=getMeta(func).qualifiedId;
    unqalified:=unqualifiedId(result);
    if startsWith(unqalified,'::') then result:=copy(unqalified,3,length(unqalified)-2);
  end;

FUNCTION T_builtinGeneratorExpression.getId:T_idString;
  begin
    result:=toString();
  end;

FUNCTION T_builtinExpression.equals(CONST other:P_literal):boolean;
  begin
    result:=(other^.literalType=lt_expression) and (P_expressionLiteral(other)^.typ=et_builtin) and (P_builtinExpression(other)^.func=func);
  end;

FUNCTION T_inlineExpression .arity: longint; begin result:=pattern.arity; end;
FUNCTION T_builtinExpression.arity: longint; begin result:=C_arityKind[getMeta(func).arityKind].fixedParameters; end;
FUNCTION T_builtinGeneratorExpression.arity: longint; begin result:=0; end;

FUNCTION T_inlineExpression.inspect: P_mapLiteral;
  begin
    result:=newMapLiteral;
    P_mapLiteral(result)^.put('pattern' ,pattern.toString)^
                         .put('location',getLocation     )^
                         .put('type'    ,C_expressionTypeString[typ])^
                         .put('body'    ,toDocString(false) );
  end;

FUNCTION T_subruleExpression.inspect: P_mapLiteral;
  begin
    result:=inherited inspect^.put('comment'   ,meta.comment            )^
                              .put('attributes',meta.getAttributesLiteral,false);
  end;

FUNCTION T_inlineExpression.patternString:string; begin result:=pattern.toString; end;

CONSTRUCTOR T_ruleMetaData.create; begin comment:=''; setLength(attributes,0); sideEffectWhitelist:=C_allSideEffects; end;
DESTRUCTOR T_ruleMetaData.destroy; begin comment:=''; setLength(attributes,0); sideEffectWhitelist:=C_allSideEffects; end;
PROCEDURE T_ruleMetaData.setComment(CONST commentText: ansistring);
  begin
    comment:=commentText;
  end;

FUNCTION T_ruleMetaData.hasAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):boolean;
  begin
    result:=getAttribute(attributeKey,caseSensitive).key<>'';
  end;

FUNCTION T_ruleMetaData.getAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):T_subruleAttribute;
  CONST blankAttribute:T_subruleAttribute=(key:'';value:'');
  VAR attrib:T_subruleAttribute;
      useKey:string;
  begin
    if caseSensitive then begin
      for attrib in attributes do if attrib.key=attributeKey then exit(attrib);
    end else begin
      useKey:=uppercase(attributeKey);
      for attrib in attributes do if attrib.key=useKey then exit(attrib);
    end;
    result:=blankAttribute;
  end;

PROCEDURE T_ruleMetaData.setAttributes(CONST attributeLines:T_arrayOfString; CONST location:T_tokenLocation; VAR adapters:T_adapters);
  VAR line:string;
      parts:T_arrayOfString;
      newAttriuteIndex:longint=0;
  FUNCTION addAttribute(CONST key:ansistring):longint;
    VAR i:longint;
    begin
      for i:=0 to length(attributes)-1 do if attributes[i].key=key then begin
        adapters.raiseWarning('Duplicate attribute key "'+key+'"',location);
        exit(i);
      end;
      result:=length(attributes);
      setLength(attributes,result+1);
      attributes[result].key:=key;
    end;

  PROCEDURE parseSideEffects(CONST s:string);
    VAR sideEffectName:string;
        sideEffect:T_sideEffect;
        requirePure:boolean=false;
        hasUnknownSideEffects:boolean=false;
        compoundMessage:T_arrayOfString;
    begin
      sideEffectWhitelist:=[];
      for sideEffectName in split(s,',') do begin
        if trim(sideEffectName)=ALLOW_NO_SIDE_EFFECTS_ATTRIBUTE_VALUE
        then requirePure:=true
        else begin
          if isSideEffectName(trim(sideEffectName),sideEffect)
          then include(sideEffectWhitelist,sideEffect)
          else begin
            adapters.raiseWarning('Unknown side effect: '+trim(sideEffectName),location);
            hasUnknownSideEffects:=true;
          end;
        end;
      end;
      if requirePure and (sideEffectWhitelist<>[]) then adapters.raiseError('Conflicting side effect restrictions: pure cannot be combined',location);
      if hasUnknownSideEffects then begin
        compoundMessage:='The following side effects are defined:';
        for sideEffect in C_allSideEffects do append(compoundMessage,C_sideEffectName[sideEffect]);
        adapters.raiseWarning(compoundMessage,location);
      end;
    end;

  begin
    setLength(attributes,0);
    for line in attributeLines do begin
      parts:=split(trim(line),'=');
      if length(parts)>0 then begin
        newAttriuteIndex:=addAttribute(trim(parts[0]));
        dropFirst(parts,1);
        attributes[newAttriuteIndex].value:=trim(join(parts,'='));
        if attributes[newAttriuteIndex].key=ALLOW_SIDE_EFFECT_ATTRIBUTE then parseSideEffects(attributes[newAttriuteIndex].value);
      end;
    end;
  end;

FUNCTION T_subruleExpression.acceptsSingleLiteral(CONST literalTypeToAccept:T_literalType):boolean;
  begin
    result:=pattern.acceptsSingleLiteral(literalTypeToAccept);
  end;

{$ifdef fullVersion}
PROCEDURE T_subruleExpression.checkParameters(VAR context:T_threadContext);
  VAR t:T_preparedToken;
      used:T_arrayOfLongint;
  begin
    if meta.hasAttribute(SUPPRESS_UNUSED_PARAMETER_WARNING_ATTRIBUTE) then exit;
    setLength(used,0);
    for t in preparedBody do with t do if (parIdx>=0) then append(used,parIdx);
    pattern.complainAboutUnusedParameters(used,context,getLocation);
  end;
{$endif}

FUNCTION T_ruleMetaData.getAttributesLiteral: P_mapLiteral;
  VAR i:longint;
  begin
    result:=newMapLiteral();
    for i:=0 to length(attributes)-1 do result^.put(attributes[i].key,attributes[i].value);
  end;

FUNCTION T_ruleMetaData.getDocTxt:ansistring;
  PROCEDURE addLine(CONST s:string);
    begin
      if result='' then result:=                       ECHO_MARKER+COMMENT_PREFIX+s
                   else result:=result+C_lineBreakChar+ECHO_MARKER+COMMENT_PREFIX+s;
    end;

  VAR att:T_subruleAttribute;
      s:string;
  begin
    result:='';
    for att in attributes do begin
      addLine('@'+att.key);
      if att.value<>'' then result:=result+'='+att.value;
    end;
    if comment<>'' then for s in split(comment,C_lineBreakChar) do addLine(s);
    if result<>'' then result:=result+C_lineBreakChar;
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
          BLANK_ABSTRACT_PACKAGE.resolveId(t^,adapters);
      end;
      t:=t^.next;
    end;
    bracketStack.destroy;

  end;

PROCEDURE T_inlineExpression.resolveIds(CONST adapters: P_adapters);
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
    enterCriticalSection(subruleCallCs);
    if not(functionIdsReady) then begin
      bracketStack.create;
      functionIdsReady:=true;
      for i:=0 to length(preparedBody)-1 do with preparedBody[i] do begin
        case token.tokType of
          tt_each,tt_parallelEach,tt_braceOpen: bracketStack.quietPush(@token);
          tt_braceClose: bracketStack.quietPop;
          tt_identifier: if (parIdx<0) and not(isEachIdentifier(token.txt)) then begin
            P_abstractPackage(token.location.package)^.resolveId(token,adapters);
            functionIdsReady:=functionIdsReady and (token.tokType<>tt_identifier);
          end;
          {$ifdef fullVersion}
          tt_localUserRule,
          tt_customTypeRule,
          tt_customTypeCheck,
          tt_importedUserRule:P_abstractRule(token.data)^.setIdResolved;
          {$endif}
        end;
      end;
      bracketStack.destroy;
    end;
    leaveCriticalSection(subruleCallCs);
  end;

{$ifdef fullVersion}
VAR generateRowIdentification:T_identifiedInternalFunction;
FUNCTION generateRow(CONST f:P_expressionLiteral; CONST t0,t1:T_myFloat; CONST samples:longint; CONST location:T_tokenLocation; VAR context:T_threadContext):T_dataRow;
  VAR tRow :T_arrayOfDouble;
      TList:P_listLiteral=nil;
      dataRow:T_dataRow;

      resultLiteral:P_listLiteral=nil;

      tempcontext:T_evaluationContext;
      collector  :T_collectingOutAdapter;

      dataReadyVectorized:boolean=false;
      dataReadyScalar    :boolean=false;

  FUNCTION evaluatePEachExpressionOk:boolean;
    VAR firstRep:P_token=nil;
        lastRep:P_token=nil;
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
      tempcontext.threadContext^.reduceExpression(firstRep);
      result:=tempcontext.adapters^.noErrors and
              (firstRep<>nil) and
              (firstRep^.next=nil) and
              (firstRep^.tokType=tt_literal) and
              (P_literal(firstRep^.data)^.literalType in [lt_list,lt_realList,lt_intList,lt_numList]) and
              (P_listLiteral(firstRep^.data)^.size = TList^.size);
      tempcontext.adapters^.clearErrors;
      if result then resultLiteral:=P_listLiteral(P_literal(firstRep^.data)^.rereferenced);
      context.recycler.cascadeDisposeToken(firstRep);
    end;

  FUNCTION evaluateVectorExpressionOk:boolean;
    VAR params:T_listLiteral;
        temp:P_literal;
    begin
      params.create(1);
      params.append(TList,true);
      temp:=f^.evaluate(location,tempcontext.threadContext,@params).literal;
      params.destroy;
      result:=tempcontext.adapters^.noErrors and
              (temp<>nil) and
              (temp^.literalType in [lt_list,lt_realList,lt_intList,lt_numList]) and
              (P_listLiteral(temp)^.size = TList^.size);
      if result then resultLiteral:=P_listLiteral(temp)
      else if temp<>nil then disposeLiteral(temp);
      tempcontext.adapters^.clearErrors;
    end;

  PROCEDURE constructInitialTList;
    VAR sampleIndex:longint;
        initialSampleCount:longint;
        t:T_myFloat;
    begin
      initialSampleCount:=(samples div 100)-1;
      if initialSampleCount< 5 then
         initialSampleCount:=5;
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
        screenRow:T_rowToPaint;
        t:double;
        stillOk:boolean=true;

    begin
      while stillOk and (length(dataRow)<samples) do begin
        //Prepare threshold:----------------------------------------------------
        screenRow:=context.adapters^.plot^.options.transformRow(dataRow,1,0,0);
        for i:=1 to length(dataRow)-1 do
        if screenRow[i-1].valid and screenRow[i].valid then begin
          distThreshold:=distThreshold+sqr(screenRow[i].x-screenRow[i-1].x)
                                      +sqr(screenRow[i].y-screenRow[i-1].y);
          inc(distThresholdSamples);
        end;
        distThreshold:=distThreshold/distThresholdSamples;
        //----------------------------------------------------:Prepare threshold
        //Prepare new time samples:---------------------------------------------
        setLength(newTimes,0);
        TList:=newListLiteral;
        for i:=1 to length(dataRow)-1 do
        if not(screenRow[i  ].valid) or
           not(screenRow[i-1].valid) or
          (sqr(screenRow[i].x-screenRow[i-1].x)
          +sqr(screenRow[i].y-screenRow[i-1].y)>=distThreshold) then begin
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
          disposeLiteral(resultLiteral);
        end;
        disposeLiteral(TList);
        //--------------------------------------------:Prepare new point samples
      end;
    end;

  begin
    tempcontext.createAndResetSilentContext(nil,C_EMPTY_STRING_ARRAY,[]);
    collector.create(at_unknown,[mt_el3_evalError,mt_el3_userDefined,mt_el4_systemError]);
    tempcontext.adapters^.addOutAdapter(@collector,false);
    constructInitialTList;

    dataReadyVectorized:=evaluateVectorExpressionOk;
    if not(dataReadyVectorized) then dataReadyScalar:=evaluatePEachExpressionOk;
    if dataReadyScalar or dataReadyVectorized then begin
      if resultLiteral^.literalType in [lt_intList,lt_realList,lt_numList]
      then dataRow:=newDataRow(resultLiteral,TList)
      else dataRow:=newDataRow(resultLiteral);
      disposeLiteral(resultLiteral);
      disposeLiteral(TList);
      refineDataRow;
    end else begin
      disposeLiteral(TList);
      context.adapters^.raiseError('Cannot prepare sample row using function '+f^.toString(),location);
      collector.removeDuplicateStoredMessages;
      context.adapters^.raiseStoredMessages(collector.storedMessages);
      setLength(dataRow,0);
    end;
    tempcontext.destroy;
    collector.destroy;
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
    then result:=P_expression(arg0)^.getParameterNames;
  end;

FUNCTION tokenSplit_impl intFuncSignature;
  FUNCTION expressionToTokens(CONST subruleLiteral:P_expressionLiteral):P_listLiteral;
    VAR sub:P_subruleExpression;
        i:longint;
    begin
      sub:=P_subruleExpression(subruleLiteral);
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
    statement:=lexer.getNextStatement(context.recycler,context.adapters^{$ifdef fullVersion},nil{$endif});
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
      if L^.value[i]^.literalType=lt_string
      then subTokens:=stringToTokens(P_stringLiteral(L^.value[i])^.value,location,package,context)
      else begin
        subTokens:=context.recycler.newToken(location,'',tt_literal,L^.value[i]);
        L^.value[i]^.rereference;
      end;
      if subTokens=nil then begin
        if result<>nil then context.recycler.cascadeDisposeToken(result);
        exit(nil);
      end;
      if result=nil then result:=subTokens
                    else last^.next:=subTokens;
      last:=subTokens^.last;
    end;
    preprocessStatement(result,context.adapters^{$ifdef fullVersion},nil{$endif});
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
      exit(nil);
    end;
    if (first^.tokType<>tt_literal) or (P_literal(first^.data)^.literalType<>lt_expression) then begin
      context.recycler.disposeToken(first);
      context.adapters^.raiseSystemError('This is unexpected. The result of mnh_tokens.stringToExpression should be an expression!',location);
      exit(nil);
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

FUNCTION interpret_imp intFuncSignature;
  VAR first:P_token=nil;
      package:P_abstractPackage;
      previousPrivileges:T_sideEffects;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then begin
      package:=P_abstractPackage(tokenLocation.package);
      if      arg0^.literalType=lt_string      then first:=stringToTokens(str0^.value,tokenLocation,package,context)
      else if arg0^.literalType in C_listTypes then first:=listToTokens  (list0      ,tokenLocation,package,context);
      if first=nil then exit(nil);
      previousPrivileges:=context.setAllowedSideEffectsReturningPrevious(
                          context.sideEffectWhitelist - [se_inputViaAsk]);
      result:=context.reduceToLiteral(first).literal;
      context.setAllowedSideEffectsReturningPrevious(previousPrivileges);
    end;
  end;

INITIALIZATION
  {$ifdef fullVersion}
  generateRowIdentification.create(PLOT_NAMESPACE,'generate-row-for-plot');
  mnh_funcs_plot.generateRow:=@generateRow;
  {$endif}
  subruleApplyOpCallback    :=@subruleApplyOpImpl;
  subruleReplacesCallback   :=@subruleReplaces;
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'arity'         ,@arity_imp         ,ak_unary,'arity(e:expression);//Returns the arity of expression e');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'parameterNames',@parameterNames_imp,ak_unary,'parameterNames(e:expression);//Returns the IDs of named parameters of e');
  registerRule(STRINGS_NAMESPACE        ,'tokenSplit'    ,@tokenSplit_impl   ,ak_variadic_1,'tokenSplit(S:string);#tokenSplit(S:string,language:string);//Returns a list of strings from S for a given language#//Languages: <code>MNH, Pascal, Java</code>');
  registerRule(TYPECAST_NAMESPACE       ,'toExpression'  ,@toExpression_imp  ,ak_unary,'toExpression(S);//Returns an expression parsed from string or list S');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'interpret'     ,@interpret_imp     ,ak_unary,'interpret(S);//Interprets a string or list S');
FINALIZATION
  {$ifdef fullVersion}
  generateRowIdentification.destroy;
  {$endif}
end.
