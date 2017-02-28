UNIT mnh_aggregators;
INTERFACE
USES //my libraries
     myGenerics,
     //MNH:
     mnh_constants, mnh_basicTypes,
     mnh_out_adapters,
     mnh_litVar;
TYPE
  P_aggregator=^T_aggregator;
  T_aggregator=object
    private
      resultLiteral:P_literal;
    public
    DESTRUCTOR destroy; virtual;
    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual; abstract;
    FUNCTION earlyAbort:boolean; virtual;
    FUNCTION getResult:P_literal; virtual;
  end;

  P_listAggregator=^T_listAggregator;
  T_listAggregator=object(T_aggregator)
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  P_concatAggregator=^T_concatAggregator;
  T_concatAggregator=object(T_aggregator)
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  P_headAggregator=^T_headAggregator;
  T_headAggregator=object(T_aggregator)
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
      FUNCTION earlyAbort:boolean; virtual;
      FUNCTION getResult:P_literal; virtual;
  end;

  P_minAggregator=^T_minAggregator;
  T_minAggregator=object(T_aggregator)
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  P_maxAggregator=^T_maxAggregator;
  T_maxAggregator=object(T_aggregator)
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  P_stringConcatAggregator=^T_stringConcatAggregator;
  T_stringConcatAggregator=object(T_aggregator)
    private
      resultText:ansistring;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
      FUNCTION getResult:P_literal; virtual;
  end;

  P_andAggregator=^T_andAggregator;
  T_andAggregator=object(T_aggregator)
    private
      boolResult:boolean;
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
      FUNCTION earlyAbort:boolean; virtual;
      FUNCTION getResult:P_literal; virtual;
  end;

  P_orAggregator=^T_orAggregator;
  T_orAggregator=object(T_aggregator)
    private
      boolResult:boolean;
    public
      CONSTRUCTOR create;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
      FUNCTION earlyAbort:boolean; virtual;
      FUNCTION getResult:P_literal; virtual;
  end;

  P_opAggregator=^T_opAggregator;
  T_opAggregator=object(T_aggregator)
    private
      op:T_tokenType;
    public
      CONSTRUCTOR create(CONST operatorToken:T_tokenType);
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  P_expressionAggregator=^T_expressionAggregator;
  T_expressionAggregator=object(T_aggregator)
    private
      aggregator:P_expressionLiteral;
      aggregationContext:pointer;
    public
      CONSTRUCTOR create(CONST ex:P_expressionLiteral; CONST contextPointer:pointer);
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;


IMPLEMENTATION
DESTRUCTOR T_aggregator.destroy;
  begin
    if resultLiteral<>nil then disposeLiteral(resultLiteral);
  end;

FUNCTION T_aggregator.earlyAbort: boolean;
  begin
    result:=false;
  end;

FUNCTION T_aggregator.getResult: P_literal;
  begin
    if resultLiteral=nil
    then result:=newVoidLiteral
    else result:=resultLiteral^.rereferenced;
  end;

CONSTRUCTOR T_listAggregator.create;
  begin
    resultLiteral:=newListLiteral();
  end;

PROCEDURE T_listAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    P_listLiteral(resultLiteral)^.append(L,not(doDispose));
  end;

CONSTRUCTOR T_concatAggregator.create;
  begin
    resultLiteral:=newListLiteral();
  end;

PROCEDURE T_concatAggregator.addToAggregation(L: P_literal; CONST doDispose: boolean; CONST location: T_tokenLocation; CONST adapters: P_adapters);
  begin
    if L^.literalType in C_compoundTypes
    then P_listLiteral(resultLiteral)^.appendAll(P_compoundLiteral(L))
    else P_listLiteral(resultLiteral)^.append(L,true);
    if doDispose then disposeLiteral(L);
  end;

CONSTRUCTOR T_headAggregator.create;
  begin
    resultLiteral:=nil;
  end;

PROCEDURE T_headAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if not(earlyAbort) then resultLiteral:=L;
    if not(doDispose) then L^.rereference;
  end;

FUNCTION T_headAggregator.earlyAbort: boolean;
  begin
    result:=(resultLiteral<>nil) and (resultLiteral^.literalType<>lt_void);
  end;

FUNCTION T_headAggregator.getResult: P_literal;
  begin
    if resultLiteral=nil then result:=newVoidLiteral
                         else result:=resultLiteral^.rereferenced;
  end;

CONSTRUCTOR T_minAggregator.create;
  begin
    resultLiteral:=newVoidLiteral;
  end;

PROCEDURE T_minAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if (resultLiteral^.literalType=lt_void) or (L^.leqForSorting(resultLiteral)) then begin
      disposeLiteral(resultLiteral);
      resultLiteral:=L^.rereferenced;
    end;
    if doDispose then disposeLiteral(L);
  end;

CONSTRUCTOR T_maxAggregator.create;
  begin
    resultLiteral:=newVoidLiteral;
  end;

PROCEDURE T_maxAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if (resultLiteral^.literalType=lt_void) or not(L^.leqForSorting(resultLiteral)) then begin
      disposeLiteral(resultLiteral);
      resultLiteral:=L^.rereferenced;
    end;
    if doDispose then disposeLiteral(L);
  end;

CONSTRUCTOR T_stringConcatAggregator.create;
  begin
    resultText:='';
    resultLiteral:=nil;
  end;

DESTRUCTOR T_stringConcatAggregator.destroy;
  begin
    resultText:='';
    inherited destroy;
  end;

PROCEDURE T_stringConcatAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if L^.literalType=lt_string
    then resultText:=resultText+P_stringLiteral(L)^.value
    else resultText:=resultText+L^.toString();
    if doDispose then disposeLiteral(L);
  end;

FUNCTION T_stringConcatAggregator.getResult: P_literal;
  begin
    result:=newStringLiteral(resultText);
  end;

CONSTRUCTOR T_andAggregator.create;
  begin
    resultLiteral:=nil;
    boolResult:=true;
  end;

PROCEDURE T_andAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if L^.literalType=lt_boolean then begin
      boolResult:=boolResult and P_boolLiteral(L)^.value;
    end else begin
      adapters^.raiseError('Cannot apply AND-aggregator to element of type '+L^.typeString,location);
    end;
    if doDispose then disposeLiteral(L);
  end;

FUNCTION T_andAggregator.earlyAbort: boolean;
  begin
    result:=not(boolResult);
  end;

FUNCTION T_andAggregator.getResult: P_literal;
  begin
    result:=newBoolLiteral(boolResult);
  end;

CONSTRUCTOR T_orAggregator.create;
  begin
    boolResult:=false;
    resultLiteral:=nil;
  end;

PROCEDURE T_orAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if L^.literalType=lt_boolean then begin
      boolResult:=boolResult or P_boolLiteral(L)^.value;
    end else begin
      adapters^.raiseError('Cannot apply OR-aggregator to element of type '+L^.typeString,location);
    end;
    if doDispose then disposeLiteral(L);
  end;

FUNCTION T_orAggregator.earlyAbort: boolean;
  begin
    result:=boolResult;
  end;

FUNCTION T_orAggregator.getResult: P_literal;
  begin
    result:=newBoolLiteral(boolResult);
  end;

CONSTRUCTOR T_opAggregator.create(CONST operatorToken: T_tokenType);
  begin
    resultLiteral:=nil;
    op:=operatorToken;
  end;

PROCEDURE T_opAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR newResult:P_literal;
  begin
    if resultLiteral=nil
    then resultLiteral:=L^.rereferenced
    else begin
      newResult:=resolveOperator(resultLiteral,op,L,location,adapters^);
      disposeLiteral(resultLiteral);
      resultLiteral:=newResult;
    end;
    if doDispose then disposeLiteral(L);
  end;

CONSTRUCTOR T_expressionAggregator.create(CONST ex: P_expressionLiteral; CONST contextPointer: pointer);
  begin
    aggregator:=ex;
    aggregationContext:=contextPointer;
  end;

PROCEDURE T_expressionAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR aggParams:P_listLiteral;
  begin
    if resultLiteral=nil
    then resultLiteral:=L
    else begin
      aggParams:=newListLiteral(2);
      aggParams^.append(resultLiteral,false)^.append(L,true);
      resultLiteral:=aggregator^.evaluate(aggParams,location,aggregationContext);
      if resultLiteral=nil then begin
        adapters^.raiseError('Aggregation failed for element '+L^.toString(50),location);
        resultLiteral:=newVoidLiteral;
      end;
      disposeLiteral(aggParams);
    end;
    if doDispose then disposeLiteral(L);
  end;

end.
