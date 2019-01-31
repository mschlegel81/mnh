UNIT aggregators;
INTERFACE
USES funcs,
     mnh_constants, basicTypes,
     out_adapters,
     contexts,
     litVar,
     recyclers,
     operators;
TYPE
  T_aggregator=object
  private
    resultLiteral:P_literal;
    hasReturnLiteral:boolean;
  public
    CONSTRUCTOR create(CONST initialValue:P_literal);
    DESTRUCTOR destroy; virtual;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual; abstract;
    FUNCTION earlyAbort:boolean; virtual;
    FUNCTION getResult:P_literal; virtual;
    PROPERTY hasReturn:boolean read hasReturnLiteral;
    FUNCTION isEarlyAbortingAggregator:boolean; virtual;
  end;

  T_listAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_concatAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_concatAltAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_headAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
    FUNCTION earlyAbort:boolean; virtual;
    FUNCTION getResult:P_literal; virtual;
    FUNCTION isEarlyAbortingAggregator:boolean; virtual;
  end;

  T_minAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_maxAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_stringConcatAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_andAggregator=object(T_aggregator)
    private
      boolResult:boolean;
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
      FUNCTION earlyAbort:boolean; virtual;
      FUNCTION getResult:P_literal; virtual;
      FUNCTION isEarlyAbortingAggregator:boolean; virtual;
  end;

  T_orAggregator=object(T_aggregator)
    private
      boolResult:boolean;
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
      FUNCTION earlyAbort:boolean; virtual;
      FUNCTION getResult:P_literal; virtual;
      FUNCTION isEarlyAbortingAggregator:boolean; virtual;
  end;

  T_opAggregator=object(T_aggregator)
    private
      op:T_tokenType;
      opPointer:P_intFuncCallback;
    public
      CONSTRUCTOR create(CONST operatorToken:T_tokenType);
      PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_binaryExpressionAggregator=object(T_aggregator)
    private
      aggregator:P_expressionLiteral;
    public
      CONSTRUCTOR create(CONST ex:P_expressionLiteral);
      DESTRUCTOR destroy; virtual;
      PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_unaryExpressionAggregator=object(T_binaryExpressionAggregator)
    CONSTRUCTOR create(CONST ex:P_expressionLiteral);
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_trailingAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
  end;

  T_setAggregator=object(T_aggregator)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler); virtual;
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

FUNCTION newAggregator(CONST op:T_tokenType):P_aggregator;
FUNCTION newListAggregator                  :P_listAggregator;
FUNCTION newMinAggregator                   :P_minAggregator;
FUNCTION newMaxAggregator                   :P_maxAggregator;
FUNCTION newHeadAggregator                  :P_headAggregator;
FUNCTION newTrailingAggregator              :P_trailingAggregator;
FUNCTION newSetAggregator                   :P_setAggregator;
FUNCTION newCustomAggregator(CONST ex:P_expressionLiteral):P_aggregator;
IMPLEMENTATION
FUNCTION newAggregator(CONST op:T_tokenType):P_aggregator;
  begin
    case op of
      tt_operatorConcat   : new(P_concatAggregator      (result),create);
      tt_operatorConcatAlt: new(P_concatAltAggregator   (result),create);
      tt_operatorStrConcat: new(P_stringConcatAggregator(result),create);
      tt_operatorLazyAnd  : new(P_andAggregator         (result),create);
      tt_operatorLazyOr   : new(P_orAggregator          (result),create);
      tt_operatorOrElse   : new(P_headAggregator        (result),create);
      else                  new(P_opAggregator          (result),create(op));
    end;
  end;

FUNCTION newListAggregator    :P_listAggregator;     begin new(result,create); end;
FUNCTION newMinAggregator     :P_minAggregator;      begin new(result,create); end;
FUNCTION newMaxAggregator     :P_maxAggregator;      begin new(result,create); end;
FUNCTION newHeadAggregator    :P_headAggregator;     begin new(result,create); end;
FUNCTION newTrailingAggregator:P_trailingAggregator; begin new(result,create); end;
FUNCTION newSetAggregator     :P_setAggregator;      begin new(result,create); end;

FUNCTION newCustomAggregator(CONST ex:P_expressionLiteral):P_aggregator;
  VAR res1:P_unaryExpressionAggregator;
      res2:P_binaryExpressionAggregator;
  begin
    if (ex^.canApplyToNumberOfParameters(2)) then begin
      new(res2,create(ex));
      result:=res2;
    end else begin
      new(res1,create(ex));
      result:=res1;
    end;
  end;

CONSTRUCTOR T_aggregator.create(CONST initialValue:P_literal); begin hasReturnLiteral:=false; resultLiteral:=initialValue; end;
CONSTRUCTOR T_listAggregator     .create; begin inherited create(newListLiteral); end;
CONSTRUCTOR T_concatAggregator   .create; begin inherited create(newListLiteral); end;
CONSTRUCTOR T_concatAltAggregator.create; begin inherited create(newListLiteral); end;
CONSTRUCTOR T_headAggregator     .create; begin inherited create(nil);            end;
CONSTRUCTOR T_minAggregator      .create; begin inherited create(newVoidLiteral); end;
CONSTRUCTOR T_maxAggregator      .create; begin inherited create(newVoidLiteral); end;
CONSTRUCTOR T_setAggregator      .create; begin inherited create(newSetLiteral);  end;
CONSTRUCTOR T_trailingAggregator .create; begin inherited create(newVoidLiteral); end;
CONSTRUCTOR T_stringConcatAggregator.create; begin inherited create(newStringLiteral('',true)); end;
CONSTRUCTOR T_andAggregator         .create; begin inherited create(nil); boolResult:=true;  end;
CONSTRUCTOR T_orAggregator          .create; begin inherited create(nil); boolResult:=false; end;
CONSTRUCTOR T_opAggregator.create(CONST operatorToken: T_tokenType);
  begin
    inherited create(newVoidLiteral);
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

DESTRUCTOR T_aggregator          .destroy; begin if resultLiteral<>nil then disposeLiteral(resultLiteral); end;
DESTRUCTOR T_binaryExpressionAggregator.destroy; begin disposeLiteral(aggregator); inherited destroy; end;

{$MACRO ON}
{$define aggregationDefaultHandling:=
if (er.literal=nil) then exit;
if earlyAbort then begin
  disposeLiteral(er.literal);
  exit;
end;
if er.triggeredByReturn then begin
  if resultLiteral<>nil then disposeLiteral(resultLiteral);
  resultLiteral:=er.literal;
  hasReturnLiteral:=true;
  if not(doDispose) then er.literal^.rereference;
  exit;
end}

PROCEDURE T_listAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    P_listLiteral(resultLiteral)^.append(er.literal,not(doDispose));
  end;

PROCEDURE T_setAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    P_setLiteral(resultLiteral)^.append(er.literal,not(doDispose));
  end;

PROCEDURE T_concatAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType in C_compoundTypes
    then P_listLiteral(resultLiteral)^.appendAll(P_compoundLiteral(er.literal))
    else P_listLiteral(resultLiteral)^.append(er.literal,true);
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_concatAltAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    P_listLiteral(resultLiteral)^.append(er.literal,true);
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_headAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    if not(earlyAbort) then begin
      if resultLiteral<>nil then disposeLiteral(resultLiteral);
      resultLiteral:=er.literal^.rereferenced;
    end;
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_trailingAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType=lt_void then begin
      if doDispose then disposeLiteral(er.literal);
      exit;
    end;
    disposeLiteral(resultLiteral);
    resultLiteral:=er.literal;
    if not(doDispose) then er.literal^.rereference;
  end;

PROCEDURE T_minAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    if (er.literal^.literalType<>lt_void) and ((resultLiteral^.literalType=lt_void) or er.literal^.leqForSorting(resultLiteral)) then begin
      disposeLiteral(resultLiteral);
      resultLiteral:=er.literal^.rereferenced;
    end;
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_maxAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    if (er.literal^.literalType<>lt_void) and ((resultLiteral^.literalType=lt_void) or not(er.literal^.leqForSorting(resultLiteral))) then begin
      disposeLiteral(resultLiteral);
      resultLiteral:=er.literal^.rereferenced;
    end;
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_stringConcatAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
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
        param:=P_listLiteral(newListLiteral(2)^.append(resultLiteral,true)^.append(er.literal,true));
        newResult:=operator_StrConcat(param,location,P_context(context)^,recycler);
        disposeLiteral(param);
        disposeLiteral(resultLiteral);
        resultLiteral:=newResult;
      end;
    end;
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_andAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType=lt_boolean then begin
      boolResult:=boolResult and P_boolLiteral(er.literal)^.value;
    end else if er.literal^.literalType<>lt_void then begin
      context^.raiseError('Cannot apply AND-aggregator to element of type '+er.literal^.typeString,location);
    end;
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_orAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType=lt_boolean then begin
      boolResult:=boolResult or P_boolLiteral(er.literal)^.value;
    end else if er.literal^.literalType<>lt_void then begin
      context^.raiseError('Cannot apply OR-aggregator to element of type '+er.literal^.typeString,location);
    end;
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_opAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  VAR newResult:P_literal;
      param:P_listLiteral;
  begin
    aggregationDefaultHandling;
    if resultLiteral=nil
    then resultLiteral:=er.literal^.rereferenced
    else if er.literal^.literalType<>lt_void then begin
      param:=P_listLiteral(newListLiteral(2)^.append(resultLiteral,true)^.append(er.literal,true));
      newResult:=opPointer(param,location,P_context(context)^,recycler);
      disposeLiteral(param);
      disposeLiteral(resultLiteral);
      resultLiteral:=newResult;
    end;
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_binaryExpressionAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  VAR newValue:P_literal;
  begin
    aggregationDefaultHandling;
    if resultLiteral=nil then resultLiteral:=er.literal^.rereferenced
    else if resultLiteral^.literalType=lt_void then begin
      disposeLiteral(resultLiteral);
      resultLiteral:=er.literal^.rereferenced;
    end else if er.literal^.literalType<>lt_void then begin
      newValue:=aggregator^.evaluateToLiteral(location,context,@recycler,resultLiteral,er.literal).literal;
      disposeLiteral(resultLiteral);
      resultLiteral:=newValue;
      if resultLiteral=nil then begin
        context^.raiseError('Aggregation failed for element '+er.literal^.toString(50),location);
        resultLiteral:=newVoidLiteral;
      end;
    end;
    if doDispose then disposeLiteral(er.literal);
  end;

PROCEDURE T_unaryExpressionAggregator.addToAggregation(er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
  VAR newValue:P_literal;
  begin
    aggregationDefaultHandling;
    if er.literal^.literalType<>lt_void then begin
      newValue:=aggregator^.evaluateToLiteral(location,context,@recycler,er.literal,nil).literal;
      if newValue=nil then context^.raiseError('Aggregation failed for element '+er.literal^.toString(50),location)
                      else disposeLiteral(newValue);
    end;
    if doDispose then disposeLiteral(er.literal);
  end;

FUNCTION T_aggregator.isEarlyAbortingAggregator:boolean; begin result:=false; end;
FUNCTION T_aggregator.earlyAbort:boolean; begin result:=hasReturnLiteral; end;
FUNCTION T_headAggregator.isEarlyAbortingAggregator:boolean; begin result:=true; end;
FUNCTION T_headAggregator.earlyAbort: boolean;
  begin
    result:=hasReturnLiteral or (resultLiteral<>nil) and (resultLiteral^.literalType<>lt_void);
  end;
FUNCTION T_andAggregator.isEarlyAbortingAggregator:boolean; begin result:=true; end;
FUNCTION T_andAggregator.earlyAbort: boolean;
  begin
    result:=hasReturnLiteral or not(boolResult);
  end;
FUNCTION T_orAggregator.isEarlyAbortingAggregator:boolean; begin result:=true; end;
FUNCTION T_orAggregator.earlyAbort: boolean;
  begin
    result:=hasReturnLiteral or boolResult;
  end;

FUNCTION T_aggregator.getResult: P_literal;
  begin
    if resultLiteral=nil
    then result:=newVoidLiteral
    else result:=resultLiteral^.rereferenced;
  end;

FUNCTION T_headAggregator.getResult: P_literal;
  begin
    if resultLiteral=nil then result:=newVoidLiteral
                         else result:=resultLiteral^.rereferenced;
  end;

FUNCTION T_andAggregator.getResult: P_literal;
  begin
    if hasReturnLiteral
    then result:=resultLiteral^.rereferenced
    else result:=newBoolLiteral(boolResult);
  end;

FUNCTION T_orAggregator.getResult: P_literal;
  begin
    if hasReturnLiteral
    then result:=resultLiteral^.rereferenced
    else result:=newBoolLiteral(boolResult);
  end;

end.
