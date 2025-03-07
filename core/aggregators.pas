UNIT aggregators;
INTERFACE
USES funcs,
     mnh_constants, basicTypes,
     contexts,
     litVar,
     recyclers,
     operators;
TYPE
  F_addToAggregationCall=PROCEDURE (er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler) of object;

  T_aggregator=object
  private
    resultLiteral:P_literal;
    hasReturnLiteral,
    hasClosedSignal:boolean;
  public
    CONSTRUCTOR create(CONST initialValue:P_literal);
    PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
    DESTRUCTOR destroy; virtual;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual; abstract;
    FUNCTION earlyAbort:boolean; virtual;
    FUNCTION getResult(CONST literalRecycler:P_literalRecycler):P_literal; virtual;
    PROPERTY hasReturn:boolean read hasReturnLiteral;
  end;

  T_listAggregator=object(T_aggregator)
    CONSTRUCTOR create(CONST literalRecycler:P_literalRecycler);
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_concatAggregator=object(T_aggregator)
    CONSTRUCTOR create(CONST literalRecycler:P_literalRecycler);
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_concatAltAggregator=object(T_aggregator)
    CONSTRUCTOR create(CONST literalRecycler:P_literalRecycler);
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_headAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
    FUNCTION earlyAbort:boolean; virtual;
    FUNCTION getResult(CONST literalRecycler:P_literalRecycler):P_literal; virtual;
  end;

  T_minAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_maxAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_stringConcatAggregator=object(T_aggregator)
    CONSTRUCTOR create(CONST literalRecycler:P_literalRecycler);
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_andAggregator=object(T_aggregator)
    private
      boolResult:boolean;
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
      FUNCTION earlyAbort:boolean; virtual;
      FUNCTION getResult(CONST literalRecycler:P_literalRecycler):P_literal; virtual;
  end;

  T_orAggregator=object(T_aggregator)
    private
      boolResult:boolean;
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
      FUNCTION earlyAbort:boolean; virtual;
      FUNCTION getResult(CONST literalRecycler:P_literalRecycler):P_literal; virtual;
  end;

  T_opAggregator=object(T_aggregator)
    private
      op:T_tokenType;
      opPointer:P_intFuncCallback;
    public
      CONSTRUCTOR create(CONST operatorToken:T_tokenType; CONST initialValue:P_literal);
      PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_binaryExpressionAggregator=object(T_aggregator)
    private
      aggregator:P_expressionLiteral;
    public
      CONSTRUCTOR create(CONST ex:P_expressionLiteral);
      PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
      DESTRUCTOR destroy; virtual;
      PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_unaryExpressionAggregator=object(T_binaryExpressionAggregator)
    CONSTRUCTOR create(CONST ex:P_expressionLiteral);
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_trailingAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

  T_setAggregator=object(T_aggregator)
    CONSTRUCTOR create(CONST literalRecycler:P_literalRecycler);
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
  end;

TYPE T_counterMap=specialize G_literalKeyMap<longint>;
  T_elementFrequencyAggregator=object(T_aggregator)
    private
      VAR counterMap:T_counterMap;
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler); virtual;
      PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
      FUNCTION getResult(CONST literalRecycler:P_literalRecycler):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

  P_aggregator            =^T_aggregator;
  P_listAggregator        =^T_listAggregator;
  P_concatAggregator      =^T_concatAggregator;
  P_concatAltAggregator   =^T_concatAltAggregator;
  P_headAggregator        =^T_headAggregator;
  P_minAggregator         =^T_minAggregator;
  P_maxAggregator         =^T_maxAggregator;
  P_stringConcatAggregator=^T_stringConcatAggregator;
  P_andAggregator         =^T_andAggregator;
  P_orAggregator          =^T_orAggregator;
  P_opAggregator          =^T_opAggregator;
  P_binaryExpressionAggregator=^T_binaryExpressionAggregator;
  P_unaryExpressionAggregator =^T_unaryExpressionAggregator;
  P_trailingAggregator    =^T_trailingAggregator;
  P_setAggregator         =^T_setAggregator;
  P_elementFrequencyAggregator=^T_elementFrequencyAggregator;

FUNCTION newAggregator(CONST op:T_tokenType; CONST literalRecycler:P_literalRecycler):P_aggregator;
FUNCTION newListAggregator(CONST literalRecycler:P_literalRecycler):P_listAggregator;
FUNCTION newMinAggregator                                          :P_minAggregator;
FUNCTION newMaxAggregator                                          :P_maxAggregator;
FUNCTION newHeadAggregator                                         :P_headAggregator;
FUNCTION newTrailingAggregator                                     :P_trailingAggregator;
FUNCTION newSetAggregator (CONST literalRecycler:P_literalRecycler):P_setAggregator;
FUNCTION newElementFrequencyAggregator      :P_elementFrequencyAggregator;
FUNCTION newCustomAggregator(CONST ex:P_expressionLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context):P_aggregator;
IMPLEMENTATION
FUNCTION newAggregator(CONST op:T_tokenType; CONST literalRecycler:P_literalRecycler):P_aggregator;
  begin
    case op of
      tt_operatorConcat   : new(P_concatAggregator      (result),create(literalRecycler));
      tt_operatorConcatAlt: new(P_concatAltAggregator   (result),create(literalRecycler));
      tt_operatorStrConcat: new(P_stringConcatAggregator(result),create(literalRecycler));
      tt_operatorLazyAnd  : new(P_andAggregator         (result),create);
      tt_operatorLazyOr   : new(P_orAggregator          (result),create);
      tt_operatorOrElse   : new(P_headAggregator        (result),create);
//      tt_operatorPlus     : new(P_opAggregator          (result),create(op,literalRecycler^.newIntLiteral(0)));
//      tt_operatorMult     : new(P_opAggregator          (result),create(op,literalRecycler^.newIntLiteral(1)));
      else                  new(P_opAggregator          (result),create(op,newVoidLiteral));
    end;
  end;

FUNCTION newListAggregator    (CONST literalRecycler:P_literalRecycler):P_listAggregator;     begin new(result,create(literalRecycler)); end;
FUNCTION newMinAggregator                                            :P_minAggregator;      begin new(result,create); end;
FUNCTION newMaxAggregator                                            :P_maxAggregator;      begin new(result,create); end;
FUNCTION newHeadAggregator                                           :P_headAggregator;     begin new(result,create); end;
FUNCTION newTrailingAggregator                                       :P_trailingAggregator; begin new(result,create); end;
FUNCTION newSetAggregator     (CONST literalRecycler:P_literalRecycler):P_setAggregator;      begin new(result,create(literalRecycler)); end;
FUNCTION newElementFrequencyAggregator:P_elementFrequencyAggregator; begin new(result,create); end;

FUNCTION newCustomAggregator(CONST ex:P_expressionLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context):P_aggregator;
  VAR res1:P_unaryExpressionAggregator;
      res2:P_binaryExpressionAggregator;
  begin
    if (ex^.canApplyToNumberOfParameters(2)) then begin
      new(res2,create(ex));
      result:=res2;
    end else if (ex^.canApplyToNumberOfParameters(1)) then begin
      new(res1,create(ex));
      result:=res1;
    end else begin
      result:=nil;
      context^.raiseError('Invalid expression for aggregation: '+ex^.toString(50),tokenLocation);
    end;
  end;

CONSTRUCTOR T_aggregator.create(CONST initialValue:P_literal); begin hasClosedSignal:=false; hasReturnLiteral:=false; resultLiteral:=initialValue; end;
CONSTRUCTOR T_listAggregator     .create(CONST literalRecycler:P_literalRecycler); begin inherited create(literalRecycler^.newListLiteral); end;
CONSTRUCTOR T_concatAggregator   .create(CONST literalRecycler:P_literalRecycler); begin inherited create(literalRecycler^.newListLiteral); end;
CONSTRUCTOR T_concatAltAggregator.create(CONST literalRecycler:P_literalRecycler); begin inherited create(literalRecycler^.newListLiteral); end;
CONSTRUCTOR T_headAggregator     .create;                                        begin inherited create(nil);            end;
CONSTRUCTOR T_minAggregator      .create;                                        begin inherited create(newVoidLiteral); end;
CONSTRUCTOR T_maxAggregator      .create;                                        begin inherited create(newVoidLiteral); end;
CONSTRUCTOR T_setAggregator      .create(CONST literalRecycler:P_literalRecycler); begin inherited create(literalRecycler^.newSetLiteral(0));  end;
CONSTRUCTOR T_elementFrequencyAggregator.create;
  begin
    inherited create(nil);
    counterMap.create();
  end;
CONSTRUCTOR T_trailingAggregator .create; begin inherited create(newVoidLiteral); end;
CONSTRUCTOR T_stringConcatAggregator.create(CONST literalRecycler:P_literalRecycler); begin inherited create(literalRecycler^.newStringLiteral('','',true)); end;
CONSTRUCTOR T_andAggregator         .create; begin inherited create(nil); boolResult:=true;  end;
CONSTRUCTOR T_orAggregator          .create; begin inherited create(nil); boolResult:=false; end;
CONSTRUCTOR T_opAggregator.create(CONST operatorToken: T_tokenType; CONST initialValue:P_literal);
  begin
    inherited create(initialValue);
    op:=operatorToken;
    opPointer:=intFuncForOperator[op];
  end;

CONSTRUCTOR T_binaryExpressionAggregator.create(CONST ex: P_expressionLiteral);
  begin
    inherited create(nil);
    aggregator:=ex; aggregator^.rereference;
  end;

CONSTRUCTOR T_unaryExpressionAggregator.create(CONST ex: P_expressionLiteral);
  begin
    inherited create(ex);
    resultLiteral:=newVoidLiteral;
  end;

PROCEDURE T_aggregator.cleanup(CONST literalRecycler:P_literalRecycler);
  begin
    if resultLiteral<>nil then literalRecycler^.disposeLiteral(resultLiteral);
  end;

DESTRUCTOR T_aggregator          .destroy; begin assert(resultLiteral=nil); end;
DESTRUCTOR T_binaryExpressionAggregator.destroy; begin assert(aggregator=nil); inherited destroy; end;

PROCEDURE T_binaryExpressionAggregator.cleanup(CONST literalRecycler:P_literalRecycler);
  begin
    literalRecycler^.disposeLiteral(aggregator);
    inherited;
  end;

{$MACRO ON}
{$define aggregationDefaultHandling:=if (er.literal=nil) then exit;
if er.literal^.literalType=lt_generatorClosed then hasClosedSignal:=true;
if earlyAbort then begin
  recycler^.disposeLiteral(er.literal);
  exit;
end;
if er.reasonForStop=rr_okWithReturn then begin
  if resultLiteral<>nil then recycler^.disposeLiteral(resultLiteral);
  resultLiteral:=er.literal;
  hasReturnLiteral:=true;
  if not(doDispose) then er.literal^.rereference;
  exit;
end}

PROCEDURE T_listAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    P_listLiteral(resultLiteral)^.append(recycler,er.literal,not(doDispose));
  end;

PROCEDURE T_setAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    P_setLiteral(resultLiteral)^.append(recycler,er.literal,not(doDispose));
  end;

PROCEDURE T_elementFrequencyAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  VAR prev:longint;
  begin
    aggregationDefaultHandling;
    if counterMap.putNew(er.literal,counterMap.get(er.literal,0)+1,prev) then begin
      if not(doDispose) then er.literal^.rereference;
    end else begin
      if doDispose then recycler^.disposeLiteral(er.literal);
    end;
  end;

FUNCTION T_elementFrequencyAggregator.getResult(CONST literalRecycler:P_literalRecycler):P_literal;
  VAR entry:T_counterMap.CACHE_ENTRY;
      mapEntry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
      dummy:P_literal;
  begin
    if hasReturnLiteral
    then begin
      result:=resultLiteral^.rereferenced
    end else begin
      result:=literalRecycler^.newMapLiteral(counterMap.fill);
      for entry in counterMap.keyValueList do begin
        mapEntry.key:=entry.key^.rereferenced;
        mapEntry.value:=literalRecycler^.newIntLiteral(entry.value);
        mapEntry.keyHash:=entry.keyHash;
        P_mapLiteral(result)^.underlyingMap^.putNew(mapEntry,dummy);
      end;
      P_mapLiteral(result)^.ensureType;
    end;
  end;

PROCEDURE T_elementFrequencyAggregator.cleanup(CONST literalRecycler:P_literalRecycler);
  VAR keySet:T_arrayOfLiteral;
  begin
    keySet:=counterMap.keySet;
    literalRecycler^.disposeLiterals(keySet);
    counterMap.destroy;
  end;

DESTRUCTOR T_elementFrequencyAggregator.destroy;
  begin
    counterMap.destroy;
    inherited;
  end;

PROCEDURE T_concatAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType in C_compoundTypes
    then P_listLiteral(resultLiteral)^.appendAll(recycler, P_compoundLiteral(er.literal))
    else P_listLiteral(resultLiteral)^.append   (recycler, er.literal,true);
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_concatAltAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    P_listLiteral(resultLiteral)^.append(recycler,er.literal,true);
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_headAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    if not(earlyAbort) then begin
      if resultLiteral<>nil then recycler^.disposeLiteral(resultLiteral);
      resultLiteral:=er.literal^.rereferenced;
    end;
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_trailingAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType=lt_void then begin
      if doDispose then recycler^.disposeLiteral(er.literal);
      exit;
    end;
    recycler^.disposeLiteral(resultLiteral);
    resultLiteral:=er.literal;
    if not(doDispose) then er.literal^.rereference;
  end;

PROCEDURE T_minAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    if (er.literal^.literalType<>lt_void) and ((resultLiteral^.literalType=lt_void) or er.literal^.leqForSorting(resultLiteral)) then begin
      recycler^.disposeLiteral(resultLiteral);
      resultLiteral:=er.literal^.rereferenced;
    end;
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_maxAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    if (er.literal^.literalType<>lt_void) and ((resultLiteral^.literalType=lt_void) or not(er.literal^.leqForSorting(resultLiteral))) then begin
      recycler^.disposeLiteral(resultLiteral);
      resultLiteral:=er.literal^.rereferenced;
    end;
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_stringConcatAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  VAR newResult:P_literal;
      param:P_listLiteral;
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType<>lt_void then begin
      if (resultLiteral^.literalType=lt_string) and (er.literal^.literalType in C_scalarTypes) then begin
        if er.literal^.literalType=lt_string
        then P_stringLiteral(resultLiteral)^.append(P_stringLiteral(er.literal)^.value)
        else P_stringLiteral(resultLiteral)^.append(er.literal^.toString());
      end else begin
        param:=P_listLiteral(recycler^.newListLiteral(2)^
          .append(recycler, resultLiteral,true)^
          .append(recycler, er.literal,true));
        newResult:=operator_StrConcat(param,location,P_context(context),recycler);
        recycler^.disposeLiteral(param);
        recycler^.disposeLiteral(resultLiteral);
        resultLiteral:=newResult;
      end;
    end;
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_andAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType=lt_boolean then begin
      boolResult:=boolResult and P_boolLiteral(er.literal)^.value;
    end else if er.literal^.literalType<>lt_void then begin
      context^.raiseError('Cannot apply AND-aggregator to element of type '+er.literal^.typeString,location);
    end;
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_orAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType=lt_boolean then begin
      boolResult:=boolResult or P_boolLiteral(er.literal)^.value;
    end else if er.literal^.literalType<>lt_void then begin
      context^.raiseError('Cannot apply OR-aggregator to element of type '+er.literal^.typeString,location);
    end;
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_opAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  VAR newResult:P_literal;
      param:P_listLiteral;
  begin
    aggregationDefaultHandling;
    if resultLiteral=nil
    then resultLiteral:=er.literal^.rereferenced
    else if er.literal^.literalType<>lt_void then begin
      param:=P_listLiteral(recycler^.newListLiteral(2)^
        .append(recycler, resultLiteral,true)^
        .append(recycler, er.literal,true));
      newResult:=opPointer(param,location,P_context(context),recycler);
      recycler^.disposeLiteral(param);
      recycler^.disposeLiteral(resultLiteral);
      resultLiteral:=newResult;
    end;
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_binaryExpressionAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  VAR newValue:P_literal;
  begin
    aggregationDefaultHandling;
    if resultLiteral=nil then resultLiteral:=er.literal^.rereferenced
    else if resultLiteral^.literalType=lt_void then begin
      recycler^.disposeLiteral(resultLiteral);
      resultLiteral:=er.literal^.rereferenced;
    end else if not(er.literal^.literalType in [lt_void,lt_generatorClosed]) then begin
      inc(context^.callDepth,8); //higher priorization of aggregation : enter
      newValue:=evaluteExpression(aggregator,location,context,recycler,resultLiteral,er.literal).literal;
      dec(context^.callDepth,8); //higher priorization of aggregation : exit
      recycler^.disposeLiteral(resultLiteral);
      resultLiteral:=newValue;
      if resultLiteral=nil then begin
        context^.raiseError('Aggregation failed for element '+er.literal^.toString(50),location);
        resultLiteral:=newVoidLiteral;
      end;
    end;
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

PROCEDURE T_unaryExpressionAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  VAR newValue:P_literal;
  begin
    aggregationDefaultHandling;
    if not(er.literal^.literalType in [lt_void,lt_generatorClosed]) then begin
      inc(context^.callDepth,8); //higher priorization of aggregation : enter
      newValue:=evaluteExpression(aggregator,location,context,recycler,er.literal).literal;
      dec(context^.callDepth,8); //higher priorization of aggregation : exit
      if newValue=nil then context^.raiseError('Aggregation failed for element '+er.literal^.toString(50),location)
                      else recycler^.disposeLiteral(newValue);
    end;
    if doDispose then recycler^.disposeLiteral(er.literal);
  end;

FUNCTION T_aggregator.earlyAbort:boolean; begin result:=hasReturnLiteral or hasClosedSignal; end;
FUNCTION T_headAggregator.earlyAbort: boolean;
  begin
    result:=hasClosedSignal or hasReturnLiteral or (resultLiteral<>nil) and (resultLiteral^.literalType<>lt_void);
  end;
FUNCTION T_andAggregator.earlyAbort: boolean;
  begin
    result:=hasClosedSignal or hasReturnLiteral or not(boolResult);
  end;
FUNCTION T_orAggregator.earlyAbort: boolean;
  begin
    result:=hasClosedSignal or hasReturnLiteral or boolResult;
  end;

FUNCTION T_aggregator.getResult(CONST literalRecycler:P_literalRecycler): P_literal;
  begin
    if resultLiteral=nil
    then result:=newVoidLiteral
    else result:=resultLiteral^.rereferenced;
  end;

FUNCTION T_headAggregator.getResult(CONST literalRecycler:P_literalRecycler): P_literal;
  begin
    if resultLiteral=nil then result:=newVoidLiteral
                         else result:=resultLiteral^.rereferenced;
  end;

FUNCTION T_andAggregator.getResult(CONST literalRecycler:P_literalRecycler): P_literal;
  begin
    if hasReturnLiteral
    then result:=resultLiteral^.rereferenced
    else result:=newBoolLiteral(boolResult);
  end;

FUNCTION T_orAggregator.getResult(CONST literalRecycler:P_literalRecycler): P_literal;
  begin
    if hasReturnLiteral
    then result:=resultLiteral^.rereferenced
    else result:=newBoolLiteral(boolResult);
  end;

end.
