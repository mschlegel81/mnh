UNIT subrules;
INTERFACE
USES //my utilities
     myGenerics,myStringUtil,
     //MNH:
     basicTypes,mnh_constants,
     out_adapters,
     litVar,
     tokens,
     contexts,
     tokenArray,
     valueStore,
     recyclers,
     {$ifdef fullVersion}
     mnh_plotData,funcs_plot,plotMath,
     debuggingVar,
     profiling,
     {$endif}
     funcs,
     mnh_messages,
     serializationUtil,
     patterns;
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
      FUNCTION getParameterNames(CONST literalRecycler:P_literalRecycler):P_listLiteral; virtual; abstract;
    public
      FUNCTION evaluateToBoolean(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST allowRaiseError:boolean; CONST a:P_literal; CONST b:P_literal):boolean; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal; CONST b:P_literal):T_evaluationResult; virtual;
      PROCEDURE validateSerializability(CONST messages:P_messages); virtual;
      FUNCTION toString({$WARN 5024 OFF}CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION getParentId:T_idString; virtual;
      FUNCTION clone(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_expressionLiteral; virtual;
      FUNCTION containsReturnToken:boolean; virtual;
  end;

  T_ruleMetaData=object
    private
      attributes:array of T_subruleAttribute;
      sideEffects:T_sideEffects;
      FUNCTION getAttributeValue(CONST index:longint):string;
      PROCEDURE setAttributeValue(CONST index:longint; CONST value:string);
    public
      comment:ansistring;
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      {$ifdef fullVersion}
      PROCEDURE addSuppressUnusedWarningAttribute;
      {$endif}
      FUNCTION hasAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):boolean;
      FUNCTION getAttribute(CONST attributeKey:string; CONST caseSensitive:boolean=true):T_subruleAttribute;
      PROCEDURE setAttributes(CONST attributeLines:T_arrayOfString; CONST location:T_tokenLocation; CONST messages:P_messages);
      FUNCTION getAttributesLiteral(VAR literalRecycler:T_literalRecycler):P_mapLiteral;
      FUNCTION getDocTxt:ansistring;
      PROCEDURE setComment(CONST commentText:ansistring);
      FUNCTION attributeCount:longint;
      PROPERTY attributeValue[index:longint]:string read getAttributeValue write setAttributeValue;
  end;

  T_resolveIdContext=(ON_DECLARATION,ON_DELEGATION,ON_EVALUATION);

  P_inlineExpression=^T_inlineExpression;
  T_inlineExpression=object(T_expression)
    private
      subruleCallCs:TRTLCriticalSection;
      functionIdsReady:(NOT_READY,IDS_RESOLVED,IDS_RESOLVED_AND_INLINED);
      pattern:T_pattern;
      preparedBody:array of T_preparedToken;
      customId:T_idString;
      meta:T_ruleMetaData;

      //save related:
      indexOfSave:longint;
      saveValueStore:P_valueScope;
      currentlyEvaluating:boolean;

      PROCEDURE updatePatternForInline(VAR literalRecycler:T_literalRecycler);
      PROCEDURE constructExpression(CONST rep:P_token; VAR context:T_context; VAR recycler:T_recycler; CONST eachLocation:T_tokenLocation);
      CONSTRUCTOR init(CONST srt: T_expressionType; CONST location: T_tokenLocation);
      FUNCTION needEmbrace(CONST outerOperator:T_tokenType; CONST appliedFromLeft:boolean):boolean;
      {Calling with intrinsicTuleId='' means the original is cloned}
      CONSTRUCTOR createFromInlineWithOp(CONST original:P_inlineExpression; CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; VAR recycler:T_recycler);
      FUNCTION getParameterNames(CONST literalRecycler:P_literalRecycler):P_listLiteral; virtual;
    public
      PROCEDURE resolveIds(CONST messages:P_messages; CONST resolveIdContext:T_resolveIdContext);
      FUNCTION usedGlobalVariables:T_arrayOfPointer;
      CONSTRUCTOR createForWhile   (CONST rep:P_token; CONST declAt:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler);
      CONSTRUCTOR createForEachBody(CONST parameterId:ansistring; CONST rep:P_token; CONST eachLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler);
      CONSTRUCTOR createFromInline (CONST rep:P_token; VAR context:T_context; VAR recycler:T_recycler; CONST customId_:T_idString=''; CONST retainFirstToken:boolean=false);
      CONSTRUCTOR createFromOp(VAR literalRecycler:T_literalRecycler; CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST opLocation:T_tokenLocation);
      PROCEDURE cleanup(CONST literalRecycler: P_literalRecycler); virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer):P_expressionLiteral; virtual;
      PROCEDURE validateSerializability(CONST messages:P_messages); virtual;
      //Pattern related:
      FUNCTION arity:T_arityInfo; virtual;
      FUNCTION isVariadic:boolean;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      PROPERTY getPattern:T_pattern read pattern;
      //Literal routines:
      FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION clone(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_expressionLiteral; virtual;

      //Evaluation calls:
      FUNCTION matchesPatternAndReplaces(CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT output:T_tokenRange; VAR context:T_context; VAR recycler:T_recycler):boolean;
      FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST parameters:P_listLiteral):T_evaluationResult; virtual;
      FUNCTION evaluateFormat(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler; CONST parameters:P_listLiteral):P_literal;

      //Inspection/documentation calls
      FUNCTION toDocString(CONST includePattern:boolean=true; CONST lengthLimit:longint=maxLongint):ansistring;
      FUNCTION getId:T_idString; virtual;
      FUNCTION inspect(VAR literalRecycler:T_literalRecycler):P_mapLiteral; virtual;
      FUNCTION patternString:string;
      FUNCTION containsReturnToken:boolean; virtual;
      FUNCTION writeToStream(CONST literalRecycler:P_literalRecycler; CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
      FUNCTION loadFromStream(CONST literalRecycler:P_literalRecycler; CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):boolean;
      FUNCTION referencesAnyUserPackage:boolean; virtual;
      FUNCTION getSideEffects:T_sideEffects;
  end;

  P_subruleExpression=^T_subruleExpression;
  T_subruleExpression=object(T_inlineExpression)
    private
      publicSubrule:boolean;
    public
      parent:P_abstractRule;
      PROPERTY metaData:T_ruleMetaData read meta;

      CONSTRUCTOR create(CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate:boolean; VAR context:T_context; VAR recycler:T_recycler; VAR meta_:T_ruleMetaData);
      DESTRUCTOR destroy; virtual;
      FUNCTION hasValidMainPattern:boolean;
      FUNCTION hasValidValidCustomTypeCheckPattern(CONST forDuckTyping:boolean):boolean;
      FUNCTION hasEquivalentPattern(CONST s:P_subruleExpression):boolean;
      FUNCTION hidesSubrule(CONST s:P_subruleExpression):boolean;
      //Inspection/documentation calls
      FUNCTION getInlineValue:P_literal;
      FUNCTION getParentId:T_idString; virtual;
      FUNCTION getCmdLineHelpText:ansistring;
      FUNCTION getStructuredInfo:T_structuredRuleInfo; virtual;
      FUNCTION getId:T_idString; virtual;
      FUNCTION inspect(VAR literalRecycler:T_literalRecycler):P_mapLiteral; virtual;
      FUNCTION acceptsSingleLiteral(CONST literalTypeToAccept:T_literalType):boolean;
      {$ifdef fullVersion}
      FUNCTION getUsedParameters:T_arrayOfLongint;
      PROCEDURE checkParameters(CONST distinction:T_arrayOfLongint; VAR context:T_context);
      PROCEDURE fillCallInfos(CONST infos:P_callAndIdInfos);
      {$endif}
      PROPERTY isPublic:boolean read publicSubrule;
  end;

  T_builtinGeneratorType=(bgt_future,
                          bgt_listIterator,
                          bgt_singleValueIterator,
                          bgt_rangeGenerator,
                          bgt_permutationIterator,
                          bgt_filterGenerator,
                          bgt_mapGenerator,
                          bgt_parallelMapGenerator,
                          bgt_fileLineIterator,
                          bgt_primeGenerator,
                          bgt_stringIterator,
                          bgt_realRandomGenerator,
                          bgt_xorIntRandomGenerator,
                          bgt_isaacIntRandomGenerator,
                          bgt_vanDerCorputGenerator,
                          bgt_parallelFilterGenerator,
                          bgt_flatMapGenerator,
                          bgt_chunkMapGenerator);

  P_builtinGeneratorExpression=^T_builtinGeneratorExpression;
  T_builtinGeneratorExpression=object(T_expression)
    private
      FUNCTION getParameterNames(CONST literalRecycler:P_literalRecycler):P_listLiteral; virtual;
    public
      CONSTRUCTOR create(CONST location:T_tokenLocation; CONST et:T_expressionType=et_builtinIteratable);
      FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST parameters:P_listLiteral):T_evaluationResult;  virtual;
      FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer):P_expressionLiteral; virtual;
      FUNCTION arity:T_arityInfo; virtual;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual; abstract;
      FUNCTION getId:T_idString; virtual;
      FUNCTION writeToStream(CONST literalRecycler:P_literalRecycler; CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
      FUNCTION referencesAnyUserPackage:boolean; virtual;
  end;

PROCEDURE resolveBuiltinIDs(CONST first:P_token; CONST messages:P_messages);
PROCEDURE digestInlineExpression(VAR rep:P_token; VAR context:T_context; VAR recycler:T_recycler);
FUNCTION stringOrListToExpression(CONST L:P_literal; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_inlineExpression;
FUNCTION getParametersForPseudoFuncPtr(CONST minPatternLength:longint; CONST variadic:boolean; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_token;
FUNCTION getParametersForUncurrying   (CONST givenParameters:P_listLiteral; CONST expectedArity:longint; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_token;
FUNCTION subruleApplyOpImpl(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST threadContext:P_abstractContext; VAR recycler:T_recycler):P_literal;
VAR createLazyMap:FUNCTION(CONST generator,mapping:P_expressionLiteral; CONST tokenLocation:T_tokenLocation):P_builtinGeneratorExpression;
    newGeneratorFromStreamCallback: FUNCTION(VAR literalRecycler:T_literalRecycler; CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):P_builtinGeneratorExpression;
    BUILTIN_PMAP:P_intFuncCallback;
VAR identifiedInternalFunctionTally:longint=0;
IMPLEMENTATION
USES sysutils,strutils,funcs_mnh,typinfo
     {$ifdef fullVersion},plotstyles{$endif}
     ;

TYPE
P_builtinExpression=^T_builtinExpression;
T_builtinExpression=object(T_expression)
  private
    meta:P_builtinFunctionMetaData;
    id:T_idString;
    func:P_intFuncCallback;
    FUNCTION getEquivalentInlineExpression(VAR context:T_context; VAR recycler:T_recycler):P_inlineExpression;
    FUNCTION getParameterNames(CONST literalRecycler:P_literalRecycler):P_listLiteral; virtual;
    CONSTRUCTOR createSecondaryInstance(CONST meta_:P_builtinFunctionMetaData; CONST internalId:longint);
  public
    CONSTRUCTOR create(CONST meta_:P_builtinFunctionMetaData);
    DESTRUCTOR destroy; virtual;
    FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST parameters:P_listLiteral):T_evaluationResult;  virtual;
    FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer):P_expressionLiteral; virtual;
    FUNCTION arity:T_arityInfo; virtual;
    FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
    FUNCTION getId:T_idString; virtual;
    FUNCTION equals(CONST other:P_literal):boolean; virtual;
    FUNCTION clone(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_expressionLiteral; virtual;
    FUNCTION writeToStream(CONST literalRecycler:P_literalRecycler; CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
    FUNCTION referencesAnyUserPackage:boolean; virtual;
  end;

PROCEDURE digestInlineExpression(VAR rep:P_token; VAR context:T_context; VAR recycler:T_recycler);
  VAR t,prev,inlineRuleTokens:P_token;
      bracketLevel:longint=0;
      inlineSubRule:P_inlineExpression;
      lambdaForm:boolean;
  begin
    if not(rep^.tokType in [tt_expBraceOpen,tt_functionPattern]) then begin
      context.raiseError('Error creating subrule from inline; expression does not start with "{"',rep^.location);
      exit;
    end;
    lambdaForm:=rep^.tokType=tt_functionPattern;
    if lambdaForm then begin

      t:=rep^.next; prev:=rep;
      inlineRuleTokens:=rep;
      while (t<>nil) and (bracketLevel>=0) do begin
        case t^.tokType of
          tt_expBraceOpen: begin
            digestInlineExpression(t,context,recycler);
            if t^.tokType=tt_expBraceOpen then inc(bracketLevel);
          end;
          tt_separatorComma,tt_semicolon:
            if bracketLevel=0 then dec(bracketLevel);
          else if t^.tokType in C_openingBrackets then inc(bracketLevel)
          else if t^.tokType in C_closingBrackets then dec(bracketLevel);
        end;
        if bracketLevel>=0 then begin
          prev:=t;
          t:=t^.next;
        end;
      end;
      prev^.next:=nil; //unlink closing curly bracket
      if context.messages^.continueEvaluation then begin
        new(inlineSubRule,createFromInline(inlineRuleTokens,context,recycler,'',lambdaForm));
        if context.messages^.continueEvaluation then begin
          rep^.tokType:=tt_literal;
          rep^.data:=inlineSubRule;
          rep^.next:=t;
        end else begin
          dispose(inlineSubRule,destroy);
          prev^.next:=t;
        end;
      end;
    end else begin//-------------------------------------------------------------------------------------------------
      t:=rep^.next; prev:=rep;
      inlineRuleTokens:=t;
      while (t<>nil) and ((t^.tokType<>tt_expBraceClose) or (bracketLevel>0)) do begin
        case t^.tokType of
          tt_expBraceOpen,tt_functionPattern: begin
            digestInlineExpression(t,context,recycler);
            if t^.tokType=tt_expBraceOpen then inc(bracketLevel);
          end;
        end;
        prev:=t;
        t:=t^.next;
      end;
      if (t=nil) or (t^.tokType<>tt_expBraceClose) then begin
        context.raiseError('Error creating subrule from inline; expression does not end with an }',rep^.location);
        exit;
      end;
      rep^.next:=t^.next; //remove expression from parent expression
      prev^.next:=nil; //unlink closing curly bracket
      recycler.disposeToken(t); //dispose closing curly bracket
      if context.messages^.continueEvaluation then begin
        new(inlineSubRule,createFromInline(inlineRuleTokens,context,recycler));
        if context.messages^.continueEvaluation then begin
          rep^.tokType:=tt_literal;
          rep^.data:=inlineSubRule;
        end else dispose(inlineSubRule,destroy);
      end;
    end;
  end;

PROCEDURE T_inlineExpression.constructExpression(CONST rep:P_token; VAR context:T_context; VAR recycler:T_recycler; CONST eachLocation:T_tokenLocation);
  VAR t:P_token;
      i:longint;
      scopeLevel:longint=0;
      subExpressionLevel:longint=0;

  begin
    setLength(preparedBody,rep^.getCount);
    t:=rep;
    i:=0;
    indexOfSave:=-1;
    while t<>nil do begin
      if (i>=length(preparedBody)) then setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[i] do begin
        token:=t^;
        t^.tokType:=tt_EOL; t:=recycler.disposeToken(t);
        token.next:=nil;
        case token.tokType of
          tt_beginBlock   : begin inc(scopeLevel        ); parIdx:=-1; end;
          tt_endBlock     : begin dec(scopeLevel        ); parIdx:=-1; end;
          tt_expBraceOpen : begin inc(subExpressionLevel); parIdx:=-1; end;
          tt_expBraceClose: begin dec(subExpressionLevel); parIdx:=-1; end;
          tt_save: begin
            if subExpressionLevel=0 then begin
              if indexOfSave>=0     then context.raiseError('save is allowed only once in a function body (other location: '+string(preparedBody[indexOfSave].token.location)+')',token.location)
              else if scopeLevel<>1 then context.raiseError('save is allowed only on the scope level 1 (here: '+intToStr(scopeLevel)+')',token.location)
              else begin
                indexOfSave:=i;
                makeStateful(@context,token.location);
              end;
            end;
            parIdx:=-1;
          end;
          tt_return: begin
            if not(typ in [et_subrule,et_subruleIteratable,et_subruleStateful,et_eachBody,et_whileBody]) then context.raiseError('return statements are currently only allowed in subrules, not in inline expressions',token.location);
            parIdx:=-1;
          end;
          tt_optionalParameters: parIdx:=REMAINING_PARAMETERS_IDX;
          tt_identifier, tt_eachParameter, tt_userRule, tt_globalVariable, tt_customType, tt_parameterIdentifier, tt_intrinsicRule: begin
            parIdx:=pattern.indexOfId(token.txt);
            if parIdx>=0 then begin
              if parIdx>=REMAINING_PARAMETERS_IDX
              then begin
                if (parIdx=ALL_PARAMETERS_PAR_IDX) and (subExpressionLevel>0)
                then parIdx:=-1
                else token.tokType:=tt_parameterIdentifier;
              end
              else if token.tokType<>tt_eachParameter then token.tokType:=tt_identifier;
            end
            else if not(token.tokType in [tt_parameterIdentifier,tt_eachParameter]) then token.tokType:=tt_identifier;
          end;
          tt_eachIndex: begin
            if (token.location=eachLocation)
            then parIdx:=pattern.indexOfId(token.txt)
            else parIdx:=-1;
          end
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
    functionIdsReady:=NOT_READY;
    setLength(preparedBody,0);
    indexOfSave:=-1;
    saveValueStore:=nil;
    currentlyEvaluating:=false;
  end;

CONSTRUCTOR T_inlineExpression.createForWhile(CONST rep: P_token; CONST declAt: T_tokenLocation; VAR context: T_context; VAR recycler:T_recycler);
  begin
    init(et_whileBody,declAt);
    pattern.create;
    constructExpression(rep,context,recycler,declAt);
  end;

CONSTRUCTOR T_subruleExpression.create(CONST pat:T_pattern; CONST rep:P_token; CONST declAt:T_tokenLocation; CONST isPrivate:boolean; VAR context:T_context; VAR recycler:T_recycler; VAR meta_:T_ruleMetaData);
  begin
    init(et_subrule,declAt);
    parent:=nil;
    meta.destroy;
    meta:=meta_;
    publicSubrule:=not(isPrivate);
    pattern:=pat;
    constructExpression(rep,context,recycler,declAt);
  end;

CONSTRUCTOR T_inlineExpression.createForEachBody(CONST parameterId: ansistring; CONST rep: P_token; CONST eachLocation: T_tokenLocation; VAR context: T_context; VAR recycler: T_recycler);
  begin
    init(et_eachBody,rep^.location);
    pattern.create;
    pattern.appendFreeId(parameterId,rep^.location);
    pattern.appendFreeId(EACH_INDEX_IDENTIFIER,rep^.location);
    constructExpression(rep,context,recycler,eachLocation);
  end;

FUNCTION T_inlineExpression.needEmbrace(CONST outerOperator:T_tokenType; CONST appliedFromLeft:boolean):boolean;
  FUNCTION bracketsNeededFor(CONST innerOperator:T_tokenType):boolean;
    begin
      result:=appliedFromLeft and
              ((outerOperator = tt_operatorConcat) and (innerOperator in [tt_operatorConcatAlt]) or
               (outerOperator = tt_operatorMinus) and not(innerOperator in [tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorMod) and not(innerOperator in [tt_operatorPot]) or
               (outerOperator = tt_operatorDivReal) and not(innerOperator in [tt_operatorPot]) or
               (outerOperator = tt_comparatorGeq) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorOr) and not(innerOperator in [tt_operatorMinus,tt_operatorMod,tt_operatorOr,tt_operatorPot,tt_operatorAnd,tt_operatorPlus,tt_operatorDivInt,tt_operatorMult]) or
               (outerOperator = tt_operatorPot) and not(innerOperator in [tt_operatorPot]) or
               (outerOperator = tt_operatorAnd) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivInt,tt_operatorPot,tt_operatorAnd,tt_operatorMult]) or
               (outerOperator = tt_comparatorLeq) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_comparatorGrt) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorPlus) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_comparatorNeq) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorConcatAlt) or
               (outerOperator = tt_operatorXor) and not(innerOperator in [tt_operatorMinus,tt_operatorMod,tt_operatorPot,tt_operatorAnd,tt_operatorPlus,tt_operatorXor,tt_operatorDivInt,tt_operatorMult]) or
               (outerOperator = tt_operatorDivInt) and not(innerOperator in [tt_operatorPot]) or
               (outerOperator = tt_operatorStrConcat) and not(innerOperator in [tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorPot,tt_operatorPlus,tt_operatorStrConcat,tt_operatorDivInt,tt_operatorMult]) or
               (outerOperator = tt_comparatorLss) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_comparatorEq) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorMult) and not(innerOperator in [tt_operatorPot,tt_operatorMult])) or
              not(appliedFromLeft) and
              ((outerOperator = tt_operatorConcat) and (innerOperator in [tt_operatorConcatAlt]) or
               (outerOperator = tt_operatorMinus) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorMod) and not(innerOperator in [tt_operatorMod,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorDivReal) and not(innerOperator in [tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_comparatorGeq) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorOr) and not(innerOperator in [tt_operatorMinus,tt_operatorMod,tt_operatorOr,tt_operatorPot,tt_operatorAnd,tt_operatorPlus,tt_operatorXor,tt_operatorDivInt,tt_operatorMult]) or
               (outerOperator = tt_operatorPot) or
               (outerOperator = tt_operatorAnd) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivInt,tt_operatorPot,tt_operatorAnd,tt_operatorMult]) or
               (outerOperator = tt_comparatorLeq) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_comparatorGrt) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorPlus) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_comparatorNeq) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorConcatAlt) and not(innerOperator in [tt_operatorConcat]) or
               (outerOperator = tt_operatorXor) and not(innerOperator in [tt_operatorMinus,tt_operatorMod,tt_operatorOr,tt_operatorPot,tt_operatorAnd,tt_operatorPlus,tt_operatorXor,tt_operatorDivInt,tt_operatorMult]) or
               (outerOperator = tt_operatorDivInt) and not(innerOperator in [tt_operatorMod,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorStrConcat) and not(innerOperator in [tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorPot,tt_operatorPlus,tt_operatorStrConcat,tt_operatorDivInt,tt_operatorMult]) or
               (outerOperator = tt_comparatorLss) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_comparatorEq) and not(innerOperator in [tt_operatorPlus,tt_operatorMinus,tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]) or
               (outerOperator = tt_operatorMult) and not(innerOperator in [tt_operatorMod,tt_operatorDivReal,tt_operatorDivInt,tt_operatorPot,tt_operatorMult]));
    end;

  VAR i:longint;
      level:longint=0;
  begin
    if length(preparedBody)<=1 then exit(false);
    level:=0;
    i:=length(preparedBody)-1;
    for i:=0 to length(preparedBody)-1 do with preparedBody[i].token do begin
      if tokType in C_openingBrackets then inc(level)
      else if tokType in C_closingBrackets then dec(level)
      else if (tokType in C_operators) and (level=0) and (bracketsNeededFor(preparedBody[i].token.tokType)) then exit(true);
    end;
    result:=false;
  end;

PROCEDURE T_inlineExpression.updatePatternForInline(VAR literalRecycler:T_literalRecycler);
  VAR i:longint;
  begin
    pattern.cleanup(literalRecycler);
    for i:=0 to length(preparedBody)-1 do with preparedBody[i] do
    if token.tokType=tt_parameterIdentifier then begin
      parIdx:=pattern.indexOfIdForInline(token.txt,getLocation);
    end else if token.tokType=tt_optionalParameters then begin
      parIdx:=REMAINING_PARAMETERS_IDX;
      pattern.appendOptional;
    end;
  end;

CONSTRUCTOR T_inlineExpression.createFromInline(CONST rep: P_token; VAR context: T_context; VAR recycler:T_recycler; CONST customId_:T_idString=''; CONST retainFirstToken:boolean=false);
  VAR t:P_token;
      i:longint;
      scopeLevel:longint=0;
      subExpressionLevel:longint=0;
  begin
    init(et_inline,rep^.location);
    customId:=customId_;
    if rep^.tokType=tt_functionPattern then begin
      assert(retainFirstToken,'Invalid state!');
      pattern.clone(P_pattern(rep^.data)^);
      disposePattern(rep^.data,recycler.literalRecycler);
      rep^.tokType:=tt_EOL;
      constructExpression(rep^.next,context,recycler,rep^.location);
      exit;
    end;
    pattern.create;
    t:=rep;
    i:=0;
    setLength(preparedBody,rep^.getCount);
    while (t<>nil) do begin
      if (i>=length(preparedBody)) then setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[i] do begin
        token:=t^;
        t^.tokType:=tt_EOL;
        t:=recycler.disposeToken(t);
        token.next:=nil;
        case token.tokType of
          tt_beginBlock   : inc(scopeLevel        );
          tt_endBlock     : dec(scopeLevel        );
          tt_expBraceOpen : inc(subExpressionLevel);
          tt_expBraceClose: dec(subExpressionLevel);
          tt_save: if subExpressionLevel=0 then begin
            if indexOfSave>=0 then context.raiseError('save is allowed only once in a function body (other location: '+string(preparedBody[indexOfSave].token.location)+')',token.location);
            if scopeLevel<>1 then context.raiseError('save is allowed only on the scope level 1 (here: '+intToStr(scopeLevel)+')',token.location);
            makeStateful(@context,token.location);
            indexOfSave:=i;
          end;
        end;
        parIdx:=-1;
      end;
      inc(i);
    end;
    setLength(preparedBody,i);
    updatePatternForInline(recycler.literalRecycler);
  end;

PROCEDURE T_inlineExpression.cleanup(CONST literalRecycler: P_literalRecycler);
  VAR i:longint;
  begin
    pattern.cleanup(literalRecycler^);
    pattern.destroy;
    for i:=0 to length(preparedBody)-1 do begin
      preparedBody[i].token.undefine(literalRecycler^);
      preparedBody[i].token.destroy;
    end;
    setLength(preparedBody,0);
    noRecycler_disposeScope(saveValueStore);
    meta.destroy;
  end;

DESTRUCTOR T_inlineExpression.destroy;
  begin
    inherited destroy;
    assert(length(preparedBody)=0);
    doneCriticalSection(subruleCallCs);
  end;

DESTRUCTOR T_subruleExpression.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_inlineExpression.canApplyToNumberOfParameters(CONST parCount: longint): boolean; begin result:=pattern.canApplyToNumberOfParameters(parCount); end;
FUNCTION T_builtinExpression.canApplyToNumberOfParameters(CONST parCount: longint): boolean;
  begin
    result:=(parCount>=C_arityKind[meta^.arityKind].fixedParameters) and
           ((parCount<=C_arityKind[meta^.arityKind].fixedParameters) or
                       C_arityKind[meta^.arityKind].variadic);
  end;
FUNCTION T_builtinGeneratorExpression.canApplyToNumberOfParameters(CONST parCount: longint): boolean; begin result:=parCount=0; end;

FUNCTION T_inlineExpression.isVariadic: boolean; begin result:=pattern.isVariadic;                             end;
FUNCTION T_subruleExpression.hasValidMainPattern: boolean; begin result:=pattern.isValidMainPattern;                     end;
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

FUNCTION T_inlineExpression.matchesPatternAndReplaces(CONST param: P_listLiteral; CONST callLocation: T_tokenLocation; OUT output:T_tokenRange; VAR context: T_context; VAR recycler:T_recycler): boolean;
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
          endToken  :array[false..true] of T_tokenType=(tt_endExpression  ,tt_endRule);
    VAR i:longint;
        firstRelevantToken,lastRelevantToken:longint;
        blocking:boolean;
        L:P_literal;
        allParams:P_listLiteral=nil;
        remaining:P_listLiteral=nil;
        previousValueScope:P_valueScope;
        firstCallOfResumable:boolean=false;
        {$ifdef fullVersion}
        parametersNode:P_variableTreeEntryCategoryNode=nil;
        {$endif}
    begin
      enterCriticalSection(subruleCallCs);
      {$ifdef fullVersion}
      if tco_stackTrace in context.threadOptions then parametersNode:=newCallParametersNode(nil);
      {$endif}
      if (indexOfSave>=0) and currentlyEvaluating then begin
        output.first:=nil;
        output.last:=nil;
        leaveCriticalSection(subruleCallCs);
        context.raiseError('Expressions/subrules containing a "save" construct must not be called recursively.',callLocation);
      end else begin
        try
          currentlyEvaluating:=true;

          if not(functionIdsReady=IDS_RESOLVED_AND_INLINED) then resolveIds(context.messages,ON_EVALUATION);
          blocking:=typ in C_subruleExpressionTypes;
          output.first:=recycler.newToken(getLocation,'',beginToken[blocking]);
          output.last:=output.first;

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
                  allParams:=recycler.literalRecycler.newListLiteral;
                  if param<>nil then allParams^.appendAll(@recycler.literalRecycler,param);
                  {$ifdef fullVersion}
                  if parametersNode<>nil then parametersNode^.addEntry(ALL_PARAMETERS_TOKEN_TEXT,allParams,true);
                  {$endif}
                  allParams^.unreference;
                end;
                L:=allParams;
              end else if parIdx=REMAINING_PARAMETERS_IDX then begin
                if remaining=nil then begin
                  if param=nil
                  then remaining:=recycler.literalRecycler.newListLiteral
                  else remaining:=param^.tail(@recycler.literalRecycler,pattern.arity);
                  {$ifdef fullVersion}
                  if parametersNode<>nil then parametersNode^.addEntry(C_tokenDefaultId[tt_optionalParameters],remaining,true);
                  {$endif}
                  remaining^.unreference;
                end;
                L:=remaining;
              end else begin
                L:=param^.value[parIdx];
              end;
              output.last^.next:=recycler.newToken(token.location,'',tt_literal,L^.rereferenced);
            end else output.last^.next:=recycler.newToken(token);
            output.last:=output.last^.next;
          end;

          {$ifdef fullVersion}
          context.callStackPush(callLocation,@self,parametersNode);
          {$endif}
          if indexOfSave>=0 then begin
            if saveValueStore=nil then begin
              saveValueStore:=recycler.newValueScopeAsChildOf(context.valueScope);
              firstCallOfResumable:=true;
            end else begin
              saveValueStore^.attachParent(context.valueScope);
            end;
            //WARNING: At this point we have (temporary) cyclic referencing (@self -> saveValueStore -> context.valueScope -> ... -> @self)
            previousValueScope:=context.valueScope;
            context.valueScope:=saveValueStore;

            output.first:=recycler.disposeToken(output.first);

            context.reduceExpression(output.first,recycler);
            if output.first=nil
            then output.last:=nil
            else output.last:=output.first^.last;
            context.valueScope:=previousValueScope;
            if firstCallOfResumable then begin
              if context.messages^.continueEvaluation then begin
                updateBody;
                //We have to detach the parent after evaluation to resolve temporary cyclic referencing
                saveValueStore^.detachParent;
              end else begin
                recycler.disposeScope(saveValueStore);
              end;
            end else saveValueStore^.detachParent;
            {$ifdef fullVersion}
            context.callStackPop(output.first);
            {$endif}
          end else begin
            output.last^.next:=recycler.newToken(getLocation,'',tt_semicolon);
            output.last:=output.last^.next;
            output.last^.next:=recycler.newToken(getLocation,'',endToken[blocking]);
            output.last:=output.last^.next;
          end;
          currentlyEvaluating:=false;
        finally
          leaveCriticalSection(subruleCallCs);
        end;
      end;
    end;

  begin
    output.last:=nil;
    if (param= nil) and pattern.matchesNilPattern or
       (param<>nil) and pattern.matches(param^,callLocation,context,recycler) then begin
      prepareResult;
      result:=output.last<>nil;
    end else begin
      result:=false;
      if typ=et_eachBody
      then raise Exception.create('Cannot evaluate each body; '+parameterListTypeString(param));
    end;
  end;

FUNCTION subruleReplaces(CONST subrulePointer:pointer; CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT output:T_tokenRange; VAR context:T_context; VAR recycler:T_recycler):boolean;
  begin
    result:=P_subruleExpression(subrulePointer)^.matchesPatternAndReplaces(param,callLocation,output,context,recycler);
  end;

CONSTRUCTOR T_inlineExpression.createFromOp(VAR literalRecycler:T_literalRecycler; CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST opLocation: T_tokenLocation);
  VAR i:longint;
      r:P_inlineExpression;
      embrace:boolean;
      preparedBodyLength:longint=0;
      ignoreLHS:boolean=false;

  PROCEDURE appendToExpression(VAR T:T_preparedToken);
    begin
      with preparedBody[preparedBodyLength] do begin
        token.create;
        token.define(T.token,literalRecycler);
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
    ignoreLHS:=op in [tt_unaryOpMinus,tt_unaryOpPlus,tt_unaryOpNegate];

    preparedBodyLength:=1;
    //Pattern (including final parameter names)
    if not(ignoreLHS) and (LHS^.literalType=lt_expression) then begin
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
    if not(ignoreLHS) then begin
      if LHS^.literalType=lt_expression
      then inc(preparedBodyLength,length(P_inlineExpression(LHS)^.preparedBody)+2)
      else inc(preparedBodyLength);
    end;
    setLength(preparedBody,preparedBodyLength);
    preparedBodyLength:=0;
    //construct
    if not(ignoreLHS) then begin
      if LHS^.literalType=lt_expression then begin
        r:=P_inlineExpression(LHS);
        if r^.typ in C_statefulExpressionTypes   then makeStateful(nil,opLocation);
        if r^.typ in C_iteratableExpressionTypes then makeIteratable(nil,opLocation);
        embrace:=r^.needEmbrace(op,false);
        if embrace then appendToExpression(tt_braceOpen);
        for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
        if embrace then appendToExpression(tt_braceClose);
      end else appendToExpression(LHS);
    end;
    appendToExpression(op);
    if RHS^.literalType=lt_expression then begin
      r:=P_inlineExpression(RHS);
      if r^.typ in C_statefulExpressionTypes   then makeStateful(nil,opLocation);
      if r^.typ in C_iteratableExpressionTypes then makeIteratable(nil,opLocation);
      embrace:=ignoreLHS and (length(r^.preparedBody)>1) or
          not(ignoreLHS) and r^.needEmbrace(op,true);
      if embrace then appendToExpression(tt_braceOpen);
      for i:=0 to length(r^.preparedBody)-1 do appendToExpression(r^.preparedBody[i]);
      if embrace then appendToExpression(tt_braceClose);
    end else appendToExpression(RHS);
    setLength(preparedBody,preparedBodyLength);
  end;

FUNCTION newIdentityRule(VAR context:T_context; CONST location:T_tokenLocation; VAR recycler:T_recycler):P_subruleExpression;
  begin
    new(result,
        createFromInline(recycler.newToken(location,'$x',tt_parameterIdentifier,nil),
                         context,recycler)); //={$x}
  end;

FUNCTION subruleApplyOpImpl(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST threadContext:P_abstractContext; VAR recycler:T_recycler):P_literal;
  VAR newRule:P_subruleExpression;
      LHSinstead:P_literal=nil;
      RHSinstead:P_literal=nil;
  begin
    if (LHS<>nil) and (LHS^.literalType=lt_expression) and (P_expressionLiteral(LHS)^.typ in C_iteratableExpressionTypes) and not(RHS^.literalType=lt_expression) then begin
      // generator+3 -> lazyMap(generator,{$x+3})
      LHSinstead:=newIdentityRule(P_context(threadContext)^,tokenLocation,recycler);
      new(newRule,createFromOp(recycler.literalRecycler,LHSinstead,op,RHS,tokenLocation));
      recycler.literalRecycler.disposeLiteral(LHSinstead);
      RHSinstead:=newRule; //={$x+3}
      result:=createLazyMap(P_expressionLiteral(LHS),P_expressionLiteral(RHSinstead),tokenLocation);
      recycler.literalRecycler.disposeLiteral(RHSinstead);
      exit(result);
    end;
    if (RHS^.literalType=lt_expression) and (P_expressionLiteral(RHS)^.typ in C_iteratableExpressionTypes) and not(LHS^.literalType=lt_expression) then begin
      // 2^generator -> lazyMap(generator,{2^$x})
      RHSinstead:=newIdentityRule(P_context(threadContext)^,tokenLocation,recycler);
      new(newRule,createFromOp(recycler.literalRecycler,LHS,op,RHSinstead,tokenLocation));
      recycler.literalRecycler.disposeLiteral(RHSinstead);
      LHSinstead:=newRule; //={2^$x}
      result:=createLazyMap(P_expressionLiteral(RHS),P_expressionLiteral(LHSinstead),tokenLocation);
      recycler.literalRecycler.disposeLiteral(LHSinstead);
      exit(result);
    end;
    if (LHS<>nil) and (LHS^.literalType=lt_expression) and (P_expressionLiteral(LHS)^.typ in C_statefulExpressionTypes) or
                      (RHS^.literalType=lt_expression) and (P_expressionLiteral(RHS)^.typ in C_statefulExpressionTypes) then begin
      P_context(threadContext)^.raiseError('Cannot perform direct operations on stateful expressions.',tokenLocation);
      exit(newVoidLiteral);
    end;
    if LHS<>nil then begin
      if (LHS^.literalType=lt_expression) and (P_expressionLiteral(LHS)^.typ in C_builtinExpressionTypes)
      then LHSinstead:=P_builtinExpression(LHS)^.getEquivalentInlineExpression(P_context(threadContext)^,recycler)
      else LHSinstead:=LHS^.rereferenced;
    end else LHSinstead:=nil;
    if (RHS^.literalType=lt_expression) and (P_expressionLiteral(RHS)^.typ in C_builtinExpressionTypes)
    then RHSinstead:=P_builtinExpression(RHS)^.getEquivalentInlineExpression(P_context(threadContext)^,recycler)
    else RHSinstead:=RHS^.rereferenced;
    new(newRule,createFromOp(recycler.literalRecycler,LHSinstead,op,RHSinstead,tokenLocation));
    result:=newRule;
    if LHSinstead<>nil then
    recycler.literalRecycler.disposeLiteral(LHSinstead);
    recycler.literalRecycler.disposeLiteral(RHSinstead);
  end;

FUNCTION T_inlineExpression.applyBuiltinFunction(CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer): P_expressionLiteral;
  begin
    new(P_inlineExpression(result),createFromInlineWithOp(@self,intrinsicRuleId,funcLocation,P_recycler(recycler)^));
  end;

FUNCTION T_builtinExpression.applyBuiltinFunction(CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer): P_expressionLiteral;
  VAR temp:P_inlineExpression;
  begin
    temp:=getEquivalentInlineExpression(P_context(threadContext)^,P_recycler(recycler)^);
    new(P_inlineExpression(result),createFromInlineWithOp(temp,intrinsicRuleId,funcLocation,P_recycler(recycler)^));
    P_recycler(recycler)^.literalRecycler.disposeLiteral(temp);
  end;

FUNCTION T_builtinGeneratorExpression.applyBuiltinFunction(CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer): P_expressionLiteral;
  VAR identity:P_subruleExpression;
      mapper  :P_expressionLiteral;
  begin
    identity:=newIdentityRule(P_context(threadContext)^,funcLocation,P_recycler(recycler)^);
    mapper:=identity^.applyBuiltinFunction(intrinsicRuleId,funcLocation,threadContext,recycler);
    P_recycler(recycler)^.literalRecycler.disposeLiteral(identity);
    if mapper=nil then exit(nil);
    result:=createLazyMap(@self,mapper,funcLocation);
    P_recycler(recycler)^.literalRecycler.disposeLiteral(mapper);
  end;

PROCEDURE T_inlineExpression.validateSerializability(CONST messages:P_messages);
  begin
    if messages=nil then exit;
    if (typ<>et_inline) then messages^.raiseSimpleError('Expression literal '+toString(20)+' is not serializable',getLocation);
  end;

PROCEDURE T_expression.validateSerializability(CONST messages:P_messages);
  begin
    if messages<>nil then messages^.raiseSimpleError('Expression '+toString()+' cannot be serialized',getLocation);
  end;

CONSTRUCTOR T_inlineExpression.createFromInlineWithOp(CONST original: P_inlineExpression; CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; VAR recycler:T_recycler);
  VAR i:longint;
  PROCEDURE appendToExpression(VAR T:T_token; CONST parameterIndex:longint);
    begin
      setLength(preparedBody,length(preparedBody)+1);
      with preparedBody[length(preparedBody)-1] do begin
        token.create;
        token.define(T,recycler.literalRecycler);
        parIdx:=parameterIndex;
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
    pattern.clone(original^.pattern);
    if intrinsicRuleId<>'' then begin
      setLength(preparedBody,1);
      with preparedBody[0] do begin token.create; token.define(getLocation,intrinsicRuleId,tt_intrinsicRule,builtinFunctionMap.getFunctionForId(intrinsicRuleId)); parIdx:=-1; end;
      appendToExpression(tt_braceOpen);
    end else setLength(preparedBody,0);
    for i:=0 to length(original^.preparedBody)-1 do appendToExpression(original^.preparedBody[i].token,original^.preparedBody[i].parIdx);
    indexOfSave:=original^.indexOfSave;
    if original^.saveValueStore<>nil then saveValueStore:=recycler.cloneSafeValueStore(original^.saveValueStore);
    if intrinsicRuleId<>'' then appendToExpression(tt_braceClose);
    meta:=original^.meta;
  end;

FUNCTION T_inlineExpression.getParameterNames(CONST literalRecycler:P_literalRecycler): P_listLiteral;
  begin
    result:=pattern.getParameterNames(literalRecycler^);
  end;

FUNCTION getParametersForPseudoFuncPtr(CONST minPatternLength:longint; CONST variadic:boolean; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_token;
  VAR last:P_token;
      i:longint;
  begin
    result:=recycler.newToken(location,'',tt_braceOpen);
    last:=result;
    for i:=0 to minPatternLength-1 do begin
      if i>0 then begin
        last^.next:=recycler.newToken(location,'',tt_separatorComma);
        last:=last^.next;
      end;
      last^.next:=recycler.newToken(location,'$'+intToStr(i),tt_parameterIdentifier);
      last:=last^.next;
    end;
    last^.next:=recycler.newToken(location,'',tt_braceClose);
    last:=last^.next;
    if variadic then begin
      last^.next:=recycler.newToken(location,'',tt_listToParameterList);
      last:=last^.next;
      if minPatternLength>0 then begin
        last^.next:=recycler.newToken(location,'',tt_optionalParameters);
      end else begin
        last^.next:=recycler.newToken(location,ALL_PARAMETERS_TOKEN_TEXT,tt_parameterIdentifier);
      end;
    end;
  end;

FUNCTION getParametersForUncurrying(CONST givenParameters:P_listLiteral; CONST expectedArity:longint; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_token;
  VAR last:P_token;
      i:longint;
  begin
    result:=recycler.newToken(location,'',tt_parList_constructor,givenParameters^.rereferenced);
    last:=result;
    for i:=givenParameters^.size to expectedArity-1 do begin
      last^.next:=recycler.newToken(location,'$'+intToStr(i-givenParameters^.size),tt_parameterIdentifier);
      last:=last^.next;
      if i<expectedArity-1 then begin
        last^.next:=recycler.newToken(location,'',tt_separatorComma);
        last:=last^.next;
      end;
    end;
    last^.next:=recycler.newToken(location,'',tt_braceClose);
    last:=last^.next;
  end;

FUNCTION T_builtinExpression.getEquivalentInlineExpression(VAR context:T_context; VAR recycler:T_recycler): P_inlineExpression;
  VAR first:P_token;
  begin
    first:=recycler.newToken(getLocation,meta^.qualifiedId,tt_intrinsicRule,func);
    first^.next:=getParametersForPseudoFuncPtr(C_arityKind[meta^.arityKind].fixedParameters,C_arityKind[meta^.arityKind].variadic,getLocation,context,recycler);
    new(result,createFromInline(first,context,recycler));
  end;

FUNCTION T_builtinExpression.getParameterNames(CONST literalRecycler:P_literalRecycler): P_listLiteral;
  VAR i:longint;
  begin
    result:=literalRecycler^.newListLiteral(arity.minPatternLength);
    for i:=0 to arity.minPatternLength-1 do result^.appendString(literalRecycler,'$'+intToStr(i));
  end;

FUNCTION T_builtinGeneratorExpression.getParameterNames(CONST literalRecycler:P_literalRecycler): P_listLiteral; begin result:=literalRecycler^.newListLiteral(); end;

FUNCTION newBuiltinExpression(CONST meta:P_builtinFunctionMetaData):P_expressionLiteral;
  begin
    new(P_builtinExpression(result),create(meta));
  end;

CONSTRUCTOR T_builtinExpression.createSecondaryInstance(CONST meta_:P_builtinFunctionMetaData; CONST internalId:longint);
  VAR loc:T_tokenLocation;
  begin
    loc.package:=@MNH_PSEUDO_PACKAGE;
    loc.column:=1;
    loc.line:=internalId;
    inherited create(et_builtin,loc);
    meta:=meta_;
    id  :=meta^.qualifiedId;
    func:=meta^.functionPointer;
  end;

CONSTRUCTOR T_builtinExpression.create(CONST meta_:P_builtinFunctionMetaData);
  VAR loc:T_tokenLocation;
  begin
    loc.package:=@MNH_PSEUDO_PACKAGE;
    loc.column:=1;
    loc.line:=interLockedIncrement(identifiedInternalFunctionTally);
    inherited create(et_builtin,loc);
    meta:=meta_;
    id  :=meta^.qualifiedId;
    func:=meta^.functionPointer;
  end;

DESTRUCTOR T_builtinExpression.destroy;
  begin
    inherited destroy;
    id:='';
    func:=nil;
  end;

CONSTRUCTOR T_builtinGeneratorExpression.create(CONST location:T_tokenLocation; CONST et:T_expressionType=et_builtinIteratable);
  begin
    inherited create(et,location);
  end;

FUNCTION T_inlineExpression.toString(CONST lengthLimit: longint): ansistring;
  begin result:=toDocString(true,lengthLimit); end;
FUNCTION T_expression.toString(CONST lengthLimit: longint): ansistring;
  begin result:=C_tokenDefaultId[tt_pseudoFuncPointer]+getId; end;

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
    if      (typ in C_inlineExpressionTypes) and (pattern.isNaiveInlinePattern)
                            then result:=                 '{'+result+'}'
    else if typ=et_eachBody then result:=pattern.toString+'{'+result+'}'
    else                         result:=pattern.toString+C_tokenDefaultId[tt_declare]+result;
  end;

FUNCTION T_expression.evaluateToBoolean(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler:pointer; CONST allowRaiseError:boolean; CONST a: P_literal; CONST b: P_literal): boolean;
  VAR evResult:T_evaluationResult;
      parameterList:T_listLiteral;
  begin
    parameterList.create(2);
    if a<>nil then parameterList.append(@P_recycler(recycler)^.literalRecycler,a,true);
    if b<>nil then parameterList.append(@P_recycler(recycler)^.literalRecycler,b,true);
    evResult:=evaluate(location,context,recycler,@parameterList);
    if (evResult.literal<>nil) and (evResult.literal^.literalType=lt_boolean) then begin
      result:=P_boolLiteral(evResult.literal)^.value;
    end else begin
      result:=false;
      if allowRaiseError then begin
        if evResult.reasonForStop=rr_patternMismatch then P_context(context)^.raiseCannotApplyError('filter expression '+toString(50),@parameterList,location)
        else if (evResult.literal<>nil) then P_context(context)^.raiseError('Expression does not return a boolean but a '+evResult.literal^.typeString,location);
      end;
    end;
    parameterList.cleanup(@P_recycler(recycler)^.literalRecycler);
    parameterList.destroy;
    if evResult.literal<>nil then P_recycler(recycler)^.literalRecycler.disposeLiteral(evResult.literal);
  end;

FUNCTION T_inlineExpression.evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler:pointer; CONST parameters: P_listLiteral): T_evaluationResult;
  VAR toReduce:T_tokenRange;
  begin
    if matchesPatternAndReplaces(parameters,location,toReduce,P_context(context)^,P_recycler(recycler)^)
    then begin
      result:=P_context(context)^.reduceToLiteral(toReduce.first,P_recycler(recycler)^);
    end else begin
      result.literal:=nil;
      result.reasonForStop:=rr_patternMismatch;
    end;
  end;

FUNCTION T_inlineExpression.evaluateFormat(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler; CONST parameters:P_listLiteral):P_literal;
  VAR toReduce: T_tokenRange;
      t: P_token;
      k: longint;
  begin
    if not(functionIdsReady=IDS_RESOLVED_AND_INLINED) then begin
      enterCriticalSection(subruleCallCs);
      try
        resolveIds(nil,ON_EVALUATION);
        functionIdsReady:=IDS_RESOLVED_AND_INLINED;
        for k:=0 to length(preparedBody)-1 do if preparedBody[k].token.tokType=tt_identifier then preparedBody[k].token.tokType:=tt_blockLocalVariable;
      finally
        leaveCriticalSection(subruleCallCs);
      end;
    end;
    if matchesPatternAndReplaces(parameters,location,toReduce,context,recycler)
    then begin
      if (toReduce.first^.tokType=tt_beginExpression) and (toReduce.last^.tokType=tt_endExpression) then begin
        toReduce.first:=recycler.disposeToken(toReduce.first);
        t:=toReduce.first;
        while t^.next^.next<>toReduce.last do t:=t^.next;
        t^.next:=recycler.disposeToken(t^.next);
        t^.next:=recycler.disposeToken(t^.next);
      end;
      result:=context.reduceToLiteral(toReduce.first,recycler).literal;
    end else result:=nil;
  end;

FUNCTION T_builtinExpression.evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler:pointer; CONST parameters: P_listLiteral): T_evaluationResult;
  begin
    {$ifdef fullVersion} P_context(context)^.callStackPush(location,@self,nil); {$endif}
    result.literal:=func(parameters,location,P_context(context)^,P_recycler(recycler)^);
    result.reasonForStop:=rr_ok;
    {$ifdef fullVersion} P_context(context)^.callStackPop(nil); {$endif}
  end;

FUNCTION T_builtinGeneratorExpression.evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler:pointer; CONST parameters: P_listLiteral): T_evaluationResult;
  begin
    if (parameters<>nil) and (parameters^.size<>0) then exit(NIL_EVAL_RESULT);
    {$ifdef fullVersion} P_context(context)^.callStackPush(location,@self,nil); {$endif}
    result:=evaluateToLiteral(location,P_context(context),recycler,nil,nil);
    {$ifdef fullVersion} P_context(context)^.callStackPop(nil); {$endif}
  end;

FUNCTION T_expression.evaluateToLiteral(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler:pointer; CONST a: P_literal; CONST b: P_literal): T_evaluationResult;
  VAR parameterList:T_listLiteral;
  begin
    if (a<>nil) or (b<>nil) then begin
      parameterList.create(2);
      if a<>nil then parameterList.append(@P_recycler(recycler)^.literalRecycler,a,true);
      if b<>nil then parameterList.append(@P_recycler(recycler)^.literalRecycler,b,true);
      result:=evaluate(location,context,recycler,@parameterList);
      if (result.literal=nil) and context^.continueEvaluation then context^.raiseError('An error ocurred when trying to evaluate function '+toString(50),location);
      parameterList.cleanup(@P_recycler(recycler)^.literalRecycler);
      parameterList.destroy;
    end else result:=evaluate(location,context,recycler,nil);
  end;

FUNCTION T_subruleExpression.getInlineValue: P_literal;
  begin
    if (length(preparedBody)<>1) or not(typ in C_subruleExpressionTypes) or (pattern.arity>0) then exit(nil);
    with preparedBody[0] do if token.tokType=tt_literal then begin
      result:=token.data;
      result^.rereference;
    end else result:=nil;
  end;

FUNCTION T_expression.getParentId: T_idString; begin result:=''; end;

FUNCTION T_expression.clone(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler:pointer): P_expressionLiteral;
  begin
    raise Exception.create('Clone is not implemented for expressions of type '+C_expressionTypeString[typ]);
    result:=nil;
  end;

FUNCTION T_inlineExpression.clone(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_expressionLiteral;
  begin
    new(P_inlineExpression(result),createFromInlineWithOp(@self,'',location,P_recycler(recycler)^));
  end;

FUNCTION T_builtinExpression.clone(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_expressionLiteral;
  begin
    new(P_builtinExpression(result),createSecondaryInstance(meta,getLocation.line));
  end;

FUNCTION T_subruleExpression.getParentId: T_idString; begin if parent=nil then result:='' else result:=parent^.getId; end;

FUNCTION T_subruleExpression.getCmdLineHelpText: ansistring;
  begin
    result:='  '+pattern.toCmdLineHelpStringString;
    if meta.comment<>'' then result:=result+C_tabChar+COMMENT_PREFIX+ansiReplaceStr(meta.comment,C_lineBreakChar,C_lineBreakChar+C_tabChar+COMMENT_PREFIX);
  end;

FUNCTION T_subruleExpression.getStructuredInfo: T_structuredRuleInfo;
  FUNCTION bodyToString(CONST lengthLimit:longint):ansistring;
    VAR t:T_preparedToken;
        lastWasIdLike:boolean=false;
    begin
      result:='';
      for t in preparedBody do
        if length(result)<lengthLimit
        then result+=t.token.toString(lastWasIdLike,lastWasIdLike,lengthLimit-length(result))
        else exit(result+'...');
    end;

  begin
    result.comment:=meta.getDocTxt;
    if not(publicSubrule)
    then result.idAndSignature:=PRIVATE_TEXT+' '
    else result.idAndSignature:='';
    result.idAndSignature+=getId;
    result.body:=bodyToString(50);
    result.location:=getLocation;
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
    result+=pattern.toString;
  end;

FUNCTION T_builtinExpression.getId: T_idString;
  begin
    result:=id;
  end;

FUNCTION T_builtinGeneratorExpression.getId: T_idString;
  begin
    result:=toString();
  end;

FUNCTION T_builtinExpression.equals(CONST other: P_literal): boolean;
  begin
    result:=(other^.literalType=lt_expression) and (P_expressionLiteral(other)^.typ=et_builtin) and (P_builtinExpression(other)^.func=func);
  end;

FUNCTION T_inlineExpression.arity: T_arityInfo;
  begin
    result.minPatternLength:=pattern.arity;
    if pattern.isVariadic
    then result.maxPatternLength:=maxLongint
    else result.maxPatternLength:=result.minPatternLength;
  end;

FUNCTION T_builtinExpression.arity: T_arityInfo;
  begin
    result.minPatternLength:=C_arityKind[meta^.arityKind].fixedParameters;
    if C_arityKind[meta^.arityKind].variadic
    then result.maxPatternLength:=maxLongint
    else result.maxPatternLength:=result.minPatternLength;
  end;

FUNCTION T_builtinGeneratorExpression.arity: T_arityInfo; begin result.minPatternLength:=0; result.minPatternLength:=0; end;

FUNCTION T_expression.containsReturnToken:boolean; begin result:=false; end;
FUNCTION T_inlineExpression.containsReturnToken: boolean;
  VAR p:T_preparedToken;
  begin
    result:=false;
    for p in preparedBody do if p.token.tokType=tt_return then exit(true);
  end;

FUNCTION T_builtinExpression.writeToStream(CONST literalRecycler:P_literalRecycler; CONST locationOfSerializeCall: T_tokenLocation; CONST adapters: P_messages; CONST stream: P_outputStreamWrapper): boolean;
  begin
    stream^.writeByte(byte(typ));
    stream^.writeAnsiString(id);
    result:=true;
  end;

FUNCTION T_builtinGeneratorExpression.writeToStream(CONST literalRecycler:P_literalRecycler; CONST locationOfSerializeCall: T_tokenLocation; CONST adapters: P_messages; CONST stream: P_outputStreamWrapper): boolean;
  begin
    stream^.logWrongTypeError;
    if adapters<>nil
    then adapters^.raiseSimpleError('Cannot serialize builtin generator expression.',locationOfSerializeCall)
    else raise Exception.create(    'Cannot serialize builtin generator expression.');
    result:=false;
  end;

FUNCTION T_inlineExpression.writeToStream(CONST literalRecycler:P_literalRecycler; CONST locationOfSerializeCall: T_tokenLocation; CONST adapters: P_messages; CONST stream: P_outputStreamWrapper): boolean;
  VAR i:longint;
  begin
    if referencesAnyUserPackage then exit(false);
    stream^.writeByte(byte(typ));
    stream^.writeAnsiString(customId);
    result:=pattern.writeToStream(literalRecycler^,locationOfSerializeCall,adapters,stream)
            and stream^.allOkay;
    stream^.writeNaturalNumber(length(preparedBody));
    for i:=0 to length(preparedBody)-1 do begin
      result:=result and preparedBody[i].token.serializeSingleToken(literalRecycler^,locationOfSerializeCall,adapters,stream);
      stream^.writeInteger(preparedBody[i].parIdx);
    end;
    if (customType=nil)
    then stream^.writeAnsiString('')
    else stream^.writeAnsiString(customType^.getName);
    if typ in C_statefulExpressionTypes
    then stream^.writeNaturalNumber(indexOfSave);
    result:=result and stream^.allOkay;
  end;

FUNCTION T_inlineExpression.loadFromStream(CONST literalRecycler:P_literalRecycler; CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):boolean;
  VAR i:longint;
      customTypeName:string;
  begin
    //This must happen on construction:
    //typ=T_expressionType(stream^.readByte([low(T_expressionType)..high(T_expressionType)]));
    customId:=stream^.readAnsiString;
    result:=pattern.loadFromStream(literalRecycler^,stream,location,adapters,typeMap)
            and stream^.allOkay;
    setLength(preparedBody,stream^.readNaturalNumber);
    for i:=0 to length(preparedBody)-1 do begin
      preparedBody[i].token.create;
      preparedBody[i].token.deserializeSingleToken(literalRecycler^,location,adapters,stream,typeMap);
      preparedBody[i].parIdx:=stream^.readInteger;
    end;
    customTypeName:=stream^.readAnsiString;
    if customTypeName<>'' then begin
      if not(typeMap.containsKey(customTypeName,customType)) then begin
        if adapters<>nil
        then adapters^.raiseSimpleError('Unknown custom type for expression: '+customTypeName,location)
        else raise Exception.create    ('Unknown custom type for expression: '+customTypeName);
        stream^.logWrongTypeError;
      end;
    end;
    if typ in C_statefulExpressionTypes
    then indexOfSave:=stream^.readNaturalNumber
    else indexOfSave:=-1;
    result:=result and stream^.allOkay;
  end;

FUNCTION T_inlineExpression.inspect(VAR literalRecycler:T_literalRecycler): P_mapLiteral;
  begin
    result:=literalRecycler.newMapLiteral(0);
    P_mapLiteral(result)^.put(@literalRecycler,'pattern' ,pattern.toString)^
                         .put(@literalRecycler,'location',getLocation     )^
                         .put(@literalRecycler,'type'    ,C_expressionTypeString[typ]);
  end;

FUNCTION T_subruleExpression.inspect(VAR literalRecycler:T_literalRecycler): P_mapLiteral;
  begin
    result:=inherited inspect(literalRecycler)^.put(@literalRecycler,'comment'   ,meta.comment            )^
                              .put(@literalRecycler,'attributes',meta.getAttributesLiteral(literalRecycler),false);
  end;

FUNCTION T_inlineExpression.patternString: string; begin result:=pattern.toString; end;

CONSTRUCTOR T_ruleMetaData.create; begin sideEffects:=[]; comment:=''; setLength(attributes,0); end;
DESTRUCTOR T_ruleMetaData.destroy; begin comment:=''; setLength(attributes,0); end;
PROCEDURE T_ruleMetaData.setComment(CONST commentText: ansistring);
  begin
    if commentText<>'' then comment:=commentText;
  end;

FUNCTION T_ruleMetaData.attributeCount:longint;
  begin
    result:=length(attributes);
  end;

FUNCTION T_ruleMetaData.getAttributeValue(CONST index:longint):string;
  begin
    if (index>=0) and (index<length(attributes))
    then result:=attributes[index].value
    else result:='';
  end;

PROCEDURE T_ruleMetaData.setAttributeValue(CONST index:longint; CONST value:string);
  begin
    if (index>=0) and (index<length(attributes))
    then attributes[index].value:=value;
  end;

{$ifdef fullVersion}
PROCEDURE T_ruleMetaData.addSuppressUnusedWarningAttribute;
  CONST supressUnusedAttribute:T_subruleAttribute=(key:SUPPRESS_UNUSED_WARNING_ATTRIBUTE;value:'');
  begin
    if not(hasAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE)) then begin
      setLength(attributes,length(attributes)+1);
      attributes[length(attributes)-1]:=supressUnusedAttribute;
    end;
  end;
{$endif}

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

PROCEDURE T_ruleMetaData.setAttributes(CONST attributeLines:T_arrayOfString; CONST location:T_tokenLocation; CONST messages:P_messages);
  VAR line:string;
      parts:T_arrayOfString;
      newAttriuteIndex:longint=0;
  FUNCTION addAttribute(CONST key:ansistring):longint;
    VAR i:longint;
    begin
      for i:=0 to length(attributes)-1 do if attributes[i].key=key then begin
        messages^.postTextMessage(mt_el2_warning,location,'Duplicate attribute key "'+key+'"');
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

FUNCTION T_subruleExpression.acceptsSingleLiteral(CONST literalTypeToAccept:T_literalType):boolean;
  begin
    result:=pattern.acceptsSingleLiteral(literalTypeToAccept);
  end;

{$ifdef fullVersion}
FUNCTION T_subruleExpression.getUsedParameters: T_arrayOfLongint;
  VAR t:T_preparedToken;
  begin
    result:=C_EMPTY_LONGINT_ARRAY;
    for t in preparedBody do with t do if (parIdx>=0) then append(result,parIdx);
  end;

PROCEDURE T_subruleExpression.checkParameters(CONST distinction:T_arrayOfLongint; VAR context:T_context);
  VAR t:T_preparedToken;
      used:T_arrayOfLongint;
  begin
    if meta.hasAttribute(SUPPRESS_UNUSED_PARAMETER_WARNING_ATTRIBUTE) then exit;
    setLength(used,0);
    append(used,distinction);
    for t in preparedBody do with t do if (parIdx>=0) then append(used,parIdx);
    pattern.complainAboutUnusedParameters(used,context,getLocation);
  end;

PROCEDURE T_subruleExpression.fillCallInfos(CONST infos: P_callAndIdInfos);
  VAR t:T_preparedToken;
  begin
    for t in preparedBody do infos^.add(@t.token);
  end;
{$endif}

FUNCTION T_ruleMetaData.getAttributesLiteral(VAR literalRecycler:T_literalRecycler): P_mapLiteral;
  VAR i:longint;
  begin
    result:=literalRecycler.newMapLiteral(0);
    for i:=0 to length(attributes)-1 do result^.put(@literalRecycler,attributes[i].key,attributes[i].value);
  end;

FUNCTION T_ruleMetaData.getDocTxt:ansistring;
  PROCEDURE addLine(CONST s:string);
    begin
      if result='' then result:=                ECHO_MARKER+s
                   else result+=C_lineBreakChar+ECHO_MARKER+s;
    end;

  VAR att:T_subruleAttribute;
      s:string;
  begin
    result:='';
    for att in attributes do begin
      addLine(ATTRIBUTE_PREFIX+att.key);
      if att.value<>'' then result:=result+'='+att.value;
    end;
    if comment<>'' then for s in split(comment,C_lineBreakChar) do addLine(COMMENT_PREFIX+s);
  end;

PROCEDURE resolveBuiltinIDs(CONST first:P_token; CONST messages:P_messages);
  VAR t:P_token;
  begin
    t:=first;
    while t<>nil do begin
      with t^ do
        if (tokType=tt_identifier) then
          BLANK_ABSTRACT_PACKAGE.resolveId(t^,messages);
      t:=t^.next;
    end;
  end;

PROCEDURE T_inlineExpression.resolveIds(CONST messages:P_messages; CONST resolveIdContext:T_resolveIdContext);
  VAR i:longint;
      inlineValue:P_literal;
      idsReady:boolean;
  begin
    enterCriticalSection(subruleCallCs);
    try
      if (functionIdsReady=NOT_READY) or
         (functionIdsReady=IDS_RESOLVED) and (resolveIdContext in [ON_DELEGATION,ON_EVALUATION]) then begin
        idsReady:=true;
        meta.sideEffects:=[];
        for i:=0 to length(preparedBody)-1 do with preparedBody[i] do begin
          case token.tokType of
            tt_identifier: if (parIdx<0) then begin
              P_abstractPackage(token.location.package)^.resolveId(token,messages);
              idsReady:=idsReady and (token.tokType<>tt_identifier);
            end;
            tt_userRule, tt_globalVariable: begin
              if resolveIdContext=ON_DELEGATION then begin
                token.tokType:=tt_identifier;
                P_abstractPackage(token.location.package)^.resolveId(token,messages);
              end;
            end;
          end;
          case token.tokType of
            tt_userRule: if (P_abstractRule(token.data)^.getRuleType in [rt_normal,rt_delegate]) and (resolveIdContext=ON_EVALUATION) then begin
              inlineValue:=P_abstractRule(token.data)^.getInlineValue;
              if inlineValue<>nil then begin
                token.data:=inlineValue;
                token.tokType:=tt_literal;
              end;
            end;
            tt_intrinsicRule: meta.sideEffects+=builtinFunctionMap.getSideEffects(P_intFuncCallback(token.data));
          end;
        end;
      end;
      if idsReady then case resolveIdContext of
        ON_EVALUATION: functionIdsReady:=IDS_RESOLVED_AND_INLINED
        else           functionIdsReady:=IDS_RESOLVED;
      end else functionIdsReady:=NOT_READY;
    finally
      leaveCriticalSection(subruleCallCs);
    end;
  end;

FUNCTION T_inlineExpression.getSideEffects:T_sideEffects;
  begin
    enterCriticalSection(subruleCallCs);
    try
      if functionIdsReady=NOT_READY then resolveIds(nil,ON_DELEGATION);
      result:=meta.sideEffects;
    finally
      leaveCriticalSection(subruleCallCs);
    end;
  end;

FUNCTION T_inlineExpression.usedGlobalVariables: T_arrayOfPointer;
  VAR prep:T_preparedToken;
  begin
    enterCriticalSection(subruleCallCs);
    try
      setLength(result,0);
      for prep in preparedBody do if prep.token.tokType=tt_globalVariable then appendIfNew(result,prep.token.data);
    finally
      leaveCriticalSection(subruleCallCs);
    end;
  end;

{$ifdef fullVersion}
FUNCTION generateRow(CONST f:P_expressionLiteral; CONST t0,t1:T_myFloat; CONST samples:longint; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):T_dataRow;
  VAR tRow :T_arrayOfDouble;
      TList:P_listLiteral=nil;
      dataRow:T_dataRow;
      oldMessages:P_messages;
      holder:P_messagesErrorHolder;

      resultLiteral:P_listLiteral=nil;

      collector  :T_collectingOutAdapter;

      dataReadyVectorized:boolean=false;
      dataReadyScalar    :boolean=false;

  FUNCTION evaluatePEachExpressionOk:boolean;
    VAR firstRep:P_token=nil;
    begin
      firstRep:=recycler.newToken(location,'pMap',tt_intrinsicRule,BUILTIN_PMAP);
      firstRep^.next:=recycler.newToken(location,'',tt_parList,recycler.literalRecycler.newListLiteral(2)^
                                                                    .append(@recycler.literalRecycler, TList,true)^
                                                                    .append(@recycler.literalRecycler, f    ,true));
      context.reduceExpression(firstRep,recycler);
      result:=context.messages^.continueEvaluation and
              (firstRep<>nil) and
              (firstRep^.next=nil) and
              (firstRep^.tokType=tt_literal) and
              (P_literal(firstRep^.data)^.literalType in [lt_list,lt_realList,lt_intList,lt_numList]) and
              (P_listLiteral(firstRep^.data)^.size = TList^.size);
      collector.appendAll(holder^.storedMessages(true));
      holder^.clear;
      if result then resultLiteral:=P_listLiteral(P_literal(firstRep^.data)^.rereferenced);
      recycler.cascadeDisposeToken(firstRep);
    end;

  FUNCTION evaluateVectorExpressionOk:boolean;
    VAR params:T_listLiteral;
        temp:P_literal;
    begin
      params.create(1);
      params.append(@recycler.literalRecycler,TList,true);
      temp:=f^.evaluate(location,@context,@recycler,@params).literal;
      params.cleanup(@recycler.literalRecycler);
      params.destroy;
      result:=context.messages^.continueEvaluation and
              (temp<>nil) and
              (temp^.literalType in [lt_list,lt_realList,lt_intList,lt_numList]) and
              (P_listLiteral(temp)^.size = TList^.size);
      if result then resultLiteral:=P_listLiteral(temp)
      else if temp<>nil then recycler.literalRecycler.disposeLiteral(temp);
      collector.appendAll(holder^.storedMessages(true));
      holder^.clear;
    end;

  PROCEDURE constructInitialTList;
    VAR sampleIndex:longint;
        initialSampleCount:longint;
        t:T_myFloat;
    begin
      initialSampleCount:=(samples div 100)-1;
      if initialSampleCount< 5 then
         initialSampleCount:=5;
      TList:=recycler.literalRecycler.newListLiteral;
      setLength(tRow,initialSampleCount+1);
      for sampleIndex:=0 to initialSampleCount do begin
        t:=t0+(t1-t0)*sampleIndex/initialSampleCount;
        TList^.appendReal(@recycler.literalRecycler,t);
        tRow[sampleIndex]:=t;
      end;
    end;

  PROCEDURE refineDataRow;
    VAR i,j,k:longint;
        oldTimes,newTimes:T_arrayOfDouble;
        oldRow  ,newRow  :T_dataRow;
        t:double;
        stillOk:boolean=true;
        scalingOptions:T_scalingOptions;

        refinementSteps:T_arrayOfLongint;
        refinementRun:longint=0;
    begin
      scalingOptions:=getOptionsViaAdapters(context.messages);
      while stillOk and (dataRow.size<samples) and (refinementRun<3) do begin
        //Prepare threshold:----------------------------------------------------
        if refinementRun=0 then k:=(samples-dataRow.size) div 4 else
        if refinementRun=1 then k:=(samples-dataRow.size) div 2 else
                                k:=(samples-dataRow.size);
        inc(refinementRun);
        refinementSteps:=scalingOptions.getRefinementSteps(dataRow,k);
        //----------------------------------------------------:Prepare threshold
        //Prepare new time samples:---------------------------------------------
        setLength(newTimes,0);
        TList:=recycler.literalRecycler.newListLiteral;
        for i:=0 to dataRow.size-2 do
        for j:=1 to refinementSteps[i] do begin
          t:=j/(refinementSteps[i]+1);
          t:=tRow[i]*(1-t)+tRow[i+1]*t;
          TList^.appendReal(@recycler.literalRecycler,t);
          append(newTimes,t);
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
          dataRow.cloneTo(oldRow);
          setLength(oldTimes,length(tRow));
          for i:=0 to length(oldTimes)-1 do oldTimes[i]:=tRow[i];
          dataRow.size  :=oldRow.size+newRow.size;
          setLength(tRow,length(oldTimes)+length(newTimes));
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
          newRow.free;
          oldRow.free;
          //------------------------------------------------------:Merge samples
          recycler.literalRecycler.disposeLiteral(resultLiteral);
        end;
        recycler.literalRecycler.disposeLiteral(TList);
        //--------------------------------------------:Prepare new point samples
      end;
    end;

  begin
    oldMessages:=context.messages;
    new(holder,createErrorHolder(oldMessages,[mt_el3_evalError,mt_el3_userDefined,mt_el4_systemError]));
    context.messages:=holder;

    collector.create(at_textMessageCollector,[mt_el3_evalError,mt_el3_userDefined,mt_el4_systemError]);
    constructInitialTList;

    dataReadyVectorized:=evaluateVectorExpressionOk;
    if not(dataReadyVectorized) then dataReadyScalar:=evaluatePEachExpressionOk;
    if dataReadyScalar or dataReadyVectorized then begin
      if resultLiteral^.literalType in [lt_intList,lt_realList,lt_numList]
      then dataRow:=newDataRow(resultLiteral,TList)
      else dataRow:=newDataRow(resultLiteral);
      recycler.literalRecycler.disposeLiteral(resultLiteral);
      recycler.literalRecycler.disposeLiteral(TList);
      refineDataRow;
    end else begin
      recycler.literalRecycler.disposeLiteral(TList);
      collector.removeDuplicateStoredMessages;
      context.raiseError('Cannot prepare sample row using function '+f^.toString(),location);
      oldMessages^.postCustomMessages(collector.getStoredMessages);
      dataRow.init();
    end;
    context.messages:=oldMessages;
    dispose(holder,destroy);
    collector.destroy;
    result:=dataRow;
  end;

{$endif}

{$i func_defines.inc}
FUNCTION arity_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_expression)
    then result:=recycler.literalRecycler.newIntLiteral(P_expressionLiteral(arg0)^.arity.minPatternLength);
  end;

FUNCTION parameterNames_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_expression)
    then result:=P_expression(arg0)^.getParameterNames(@recycler.literalRecycler);
  end;

FUNCTION tokenSplit_impl intFuncSignature;
  FUNCTION expressionToTokens(CONST subruleLiteral:P_expressionLiteral):P_listLiteral;
    VAR sub:P_subruleExpression;
        i:longint;
    begin
      sub:=P_subruleExpression(subruleLiteral);
      result:=recycler.literalRecycler.newListLiteral(length(sub^.preparedBody));
      for i:=0 to length(sub^.preparedBody)-1 do with sub^.preparedBody[i] do begin
        if (token.tokType=tt_literal) and not(P_literal(token.data)^.literalType in [lt_void,lt_string])
        then result^.append      (@recycler.literalRecycler,token.data,true)
        else result^.appendString(@recycler.literalRecycler,safeTokenToString(@token));
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
        result:=recycler.literalRecycler.newListLiteral;
        for i:=0 to length(tokens)-1 do result:=listResult^.appendString(@recycler.literalRecycler,tokens[i]);
      end;
      lt_expression: if uppercase(language)='MNH' then
        result:=expressionToTokens(P_expressionLiteral(arg0));
    end;
  end;

FUNCTION stringToTokens(CONST s:ansistring; CONST location:T_tokenLocation; CONST package:P_abstractPackage; VAR context:T_context; VAR recycler:T_recycler):P_token;
  VAR lexer:T_singleStringLexer;
      statement:T_enhancedStatement;
  begin
    lexer.create(s,location,package);
    statement:=lexer.getNextStatement(context.messages,recycler);
    result:=statement.token.first;
    statement:=lexer.getNextStatement(context.messages,recycler);
    if statement.token.first<>nil then begin
      context.raiseError('Unexpected additional statement: '+tokensToString(statement.token.first,50),location);
      recycler.cascadeDisposeToken(statement.token.first);
    end;
    lexer.destroy;
    if result=nil
    then context.raiseError('The parsed expression appears to be empty',location)
    else if not (context.messages^.continueEvaluation) then begin
      recycler.cascadeDisposeToken(result);
      result:=nil
    end;
  end;

FUNCTION listToTokens(CONST l:P_listLiteral; CONST location:T_tokenLocation; CONST package:P_abstractPackage; VAR context:T_context; VAR recycler:T_recycler):P_token;
  VAR lexer:T_variableLexer;
      statement: T_enhancedStatement;
  begin
    result:=nil;
    lexer.create(l^.iteratableList,location,package);
    statement:=lexer.getNextStatement(context.messages,recycler);
    result:=statement.token.first;
    statement:=lexer.getNextStatement(context.messages,recycler);
    if statement.token.first<>nil then begin
      context.raiseError('Unexpected additional statement: '+tokensToString(statement.token.first,50),location);
      recycler.cascadeDisposeToken(statement.token.first);
    end;
    lexer.destroy;
    if result=nil
    then context.raiseError('The parsed expression appears to be empty',location)
    else if not (context.messages^.continueEvaluation) then begin
      recycler.cascadeDisposeToken(result);
      result:=nil
    end;
  end;

FUNCTION stringOrListToExpression(CONST L:P_literal; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_inlineExpression;
  VAR first:P_token=nil;
      temp:P_token;
      package:P_abstractPackage;
  begin
    result:=nil;
    package:=P_abstractPackage(location.package);
    if      L^.literalType=lt_string      then first:=stringToTokens(P_stringLiteral(L)^.value,location,package,context,recycler)
    else if L^.literalType in C_listTypes then first:=listToTokens  (P_listLiteral  (L)       ,location,package,context,recycler);
    if first=nil then exit(nil);

    if not(first^.tokType in [tt_expBraceOpen,tt_startOfPattern]) then begin
      temp:=recycler.newToken(location,'',tt_expBraceOpen);
      temp^.next:=first; first:=temp;
      temp:=first^.last;
      temp^.next:=recycler.newToken(location,'',tt_expBraceClose);
    end;
    predigest(first,package,context,recycler{$ifdef fullVersion},nil{$endif});
    digestInlineExpression(first,context,recycler);
    if (context.messages^.continueEvaluation) and (first^.next<>nil) then context.raiseError('The parsed expression goes beyond the expected limit... I know this is a fuzzy error. Sorry.',location);
    if not(context.messages^.continueEvaluation) then begin
      recycler.cascadeDisposeToken(first);
      exit(nil);
    end;
    {$ifdef debugMode}
    if (first^.tokType<>tt_literal) or (P_literal(first^.data)^.literalType<>lt_expression) then begin
      recycler.disposeToken(first);
      context.raiseError('This is unexpected. The result of stringToExpression should be an expression!',location,mt_el4_systemError);
      exit(nil);
    end;
    {$endif}
    result:=P_inlineExpression(first^.data);
    first^.tokType:=tt_EOL;
    first^.data:=nil;
    recycler.disposeToken(first);
  end;

FUNCTION toExpression_imp intFuncSignature;
  FUNCTION primitiveExpression(CONST l:P_literal):P_expressionLiteral;
    VAR first:P_token;
    begin
      //Create token-series { <Literal> }
      first            :=recycler.newToken(tokenLocation,'',tt_expBraceOpen);
      first^.next      :=recycler.newToken(tokenLocation,'',tt_literal,l); L^.rereference;
      first^.next^.next:=recycler.newToken(tokenLocation,'',tt_expBraceClose);
      //Reduce to inline expression
      digestInlineExpression(first,context,recycler);
      result:=P_expressionLiteral(first^.data);
      first^.tokType:=tt_EOL;
      first^.data:=nil;
      recycler.disposeToken(first);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then case arg0^.literalType of
      lt_expression:  begin result:=arg0; arg0^.rereference; end;
      lt_boolean,lt_smallint,lt_bigint,lt_real: result:=primitiveExpression(arg0);
      else result:=stringOrListToExpression(arg0,tokenLocation,context,recycler);
    end;
  end;

FUNCTION interpret_imp intFuncSignature;
  VAR first:P_token=nil;
      package:P_abstractPackage;
      previousPrivileges :T_sideEffects;
      sideEffectWhitelist:T_sideEffects=[];
  begin
    result:=nil;
    if (params=nil) or (params^.size<1) or (params^.size>2) then exit(nil);
    if (params^.size=2) and not(canInterpretAsSideEffectList(arg1,true,tokenLocation,context,sideEffectWhitelist)) then exit(nil);

    if (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(0)) then begin
      previousPrivileges:=context.setAllowedSideEffectsReturningPrevious(context.sideEffectWhitelist*sideEffectWhitelist);
      result:=P_expressionLiteral(arg0)^.evaluateToLiteral(tokenLocation,@context,@recycler,nil,nil).literal;
      context.setAllowedSideEffectsReturningPrevious(previousPrivileges);
    end else begin
      package:=P_abstractPackage(tokenLocation.package);
      if      arg0^.literalType=lt_string      then first:=stringToTokens(str0^.value,tokenLocation,package,context,recycler)
      else if arg0^.literalType in C_listTypes then first:=listToTokens  (list0      ,tokenLocation,package,context,recycler);
      if first=nil then exit(nil);
      previousPrivileges:=context.setAllowedSideEffectsReturningPrevious(context.sideEffectWhitelist*sideEffectWhitelist);
      result:=context.reduceToLiteral(first,recycler).literal;
      context.setAllowedSideEffectsReturningPrevious(previousPrivileges);
    end;
  end;

FUNCTION readExpressionFromStream(VAR literalRecycler:T_literalRecycler; CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):P_expressionLiteral;
  VAR expressionType:T_expressionType;
      builtinId :string;
      inlineEx :P_inlineExpression;
  begin
    expressionType:=T_expressionType(stream^.readByte([byte(low(T_expressionType))..byte(high(T_expressionType))]));
    result:=nil;
    if stream^.allOkay then case expressionType of
      et_builtin          :
        begin
          builtinId:=stream^.readAnsiString;
          if not(builtinFunctionMap.canGetIntrinsicRuleAsExpression(builtinId,result)) then begin
            if adapters<>nil
            then adapters^.raiseSimpleError('Cannot deserialize builtin function: '+builtinId,location)
            else raise Exception.create    ('Cannot deserialize builtin function: '+builtinId);
            stream^.logWrongTypeError;
          end;
        end;
      et_builtinIteratable,
      et_builtinAsyncOrFuture    :
        result:=newGeneratorFromStreamCallback(literalRecycler,stream,location,adapters,typeMap);
      et_subrule          ,
      et_inline           ,
      et_subruleIteratable,
      et_inlineIteratable ,
      et_subruleStateful  ,
      et_inlineStateful:
        begin
          new(inlineEx,init(expressionType,location));
          if not(inlineEx^.loadFromStream(@literalRecycler,stream,location,adapters,typeMap))
          then dispose(inlineEx,destroy)
          else result:=inlineEx;
        end;
      else begin
        if adapters<>nil
        then adapters^.raiseSimpleError('Cannot deserialize expression of type: '+getEnumName(TypeInfo(expressionType),ord(expressionType)),location)
        else raise Exception.create    ('Cannot deserialize expression of type: '+getEnumName(TypeInfo(expressionType),ord(expressionType)));
        stream^.logWrongTypeError;
      end;
    end;
  end;

FUNCTION T_inlineExpression.referencesAnyUserPackage: boolean;
  VAR pt:T_preparedToken;
      locals:T_setOfString;
  begin
    locals.create;
    for pt in preparedBody do begin
      if pt.token.tokType in [tt_userRule,tt_globalVariable] then begin
        locals.destroy;
        exit(true);
      end;
      if pt.token.tokType=tt_assignNewBlockLocal then locals.put(pt.token.txt);
      if (pt.token.tokType in [tt_assignExistingBlockLocal,tt_blockLocalVariable]) and not(locals.contains(pt.token.txt)) then begin
        locals.destroy;
        exit(true);
      end;
    end;
    locals.destroy;
    result:=false;
  end;

FUNCTION T_builtinGeneratorExpression.referencesAnyUserPackage: boolean;
  begin
    result:=true;
    //conservative assumption;
    //some builtin generators may contain expressions with in turn may refrerence user packages
  end;

FUNCTION T_builtinExpression.referencesAnyUserPackage: boolean;
  begin
    result:=false;
  end;

INITIALIZATION
  {$ifdef fullVersion}
  funcs_plot.generateRow:=@generateRow;
  {$endif}
  litVar.readExpressionFromStreamCallback:=@readExpressionFromStream;
  funcs.makeBuiltinExpressionCallback:=@newBuiltinExpression;
  subruleReplacesCallback   :=@subruleReplaces;
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'arity'         ,@arity_imp         ,ak_unary
    {$ifdef fullVersion},'arity(e:expression);//Returns the arity of expression e'{$endif});
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'parameterNames',@parameterNames_imp,ak_unary
    {$ifdef fullVersion},'parameterNames(e:expression);//Returns the IDs of named parameters of e'{$endif});
  builtinFunctionMap.registerRule(STRINGS_NAMESPACE        ,'tokenSplit'    ,@tokenSplit_impl   ,ak_variadic_1
    {$ifdef fullVersion},'tokenSplit(S:string);#tokenSplit(S:string,language:string);//Returns a list of strings from S for a given language#//Languages: <code>MNH, Pascal, Java</code>'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE       ,'toExpression'  ,@toExpression_imp  ,ak_unary
    {$ifdef fullVersion},'toExpression(S);//Returns an expression parsed from string or list S'{$endif});
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'interpret'     ,@interpret_imp     ,ak_unary
    {$ifdef fullVersion},'interpret(E);//Interprets a String, StringList or Expression(0) E#interpret(E,sideEffectWhitelist:StringCollection);//As above, but restricting the allowed side effects.'{$endif});
end.
