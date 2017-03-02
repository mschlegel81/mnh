UNIT mnh_aggregators;
INTERFACE
USES //my libraries
     myGenerics,
     //MNH:
     mnh_constants, mnh_basicTypes,
     mnh_out_adapters,
     mnh_litVar,
     mnh_tokens;
TYPE
  T_aggregator=object
    DESTRUCTOR destroy; virtual; abstract;
    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual; abstract;
    FUNCTION earlyAbort:boolean; virtual;
    FUNCTION getResult:P_literal; virtual; abstract;
  end;

  T_aggregatorWithResultLiteral=object(T_aggregator)
    private
      resultLiteral:P_literal;
    public
      CONSTRUCTOR create(CONST initialValue:P_literal);
      DESTRUCTOR destroy; virtual;
      FUNCTION getResult:P_literal; virtual;
  end;

  T_listAggregator=object(T_aggregatorWithResultLiteral)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  T_concatAggregator=object(T_aggregatorWithResultLiteral)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  T_headAggregator=object(T_aggregatorWithResultLiteral)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
    FUNCTION earlyAbort:boolean; virtual;
    FUNCTION getResult:P_literal; virtual;
  end;

  T_minAggregator=object(T_aggregatorWithResultLiteral)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  T_maxAggregator=object(T_aggregatorWithResultLiteral)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  T_stringConcatAggregator=object(T_aggregatorWithResultLiteral)
    CONSTRUCTOR create;
    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  T_andAggregator=object(T_aggregator)
    private
      boolResult:boolean;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
      FUNCTION earlyAbort:boolean; virtual;
      FUNCTION getResult:P_literal; virtual;
  end;

  T_orAggregator=object(T_aggregator)
    private
      boolResult:boolean;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
      FUNCTION earlyAbort:boolean; virtual;
      FUNCTION getResult:P_literal; virtual;
  end;

  T_opAggregator=object(T_aggregatorWithResultLiteral)
    private
      op:T_tokenType;
    public
      CONSTRUCTOR create(CONST operatorToken:T_tokenType);
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  T_expressionAggregator=object(T_aggregatorWithResultLiteral)
    private
      aggregator:P_expressionLiteral;
      aggregationContext:pointer;
    public
      CONSTRUCTOR create(CONST ex:P_expressionLiteral; CONST contextPointer:pointer);
      DESTRUCTOR destroy; virtual;
      PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters); virtual;
  end;

  P_aggregator=^T_aggregator;
  P_listAggregator=^T_listAggregator;
  P_concatAggregator=^T_concatAggregator;
  P_headAggregator=^T_headAggregator;
  P_minAggregator=^T_minAggregator;
  P_maxAggregator=^T_maxAggregator;
  P_stringConcatAggregator=^T_stringConcatAggregator;
  P_andAggregator=^T_andAggregator;
  P_orAggregator=^T_orAggregator;
  P_opAggregator=^T_opAggregator;
  P_expressionAggregator=^T_expressionAggregator;

FUNCTION newAggregator(CONST op:T_tokenType):P_aggregator;
FUNCTION newListAggregator                  :P_listAggregator;
FUNCTION newMinAggregator                   :P_minAggregator;
FUNCTION newMaxAggregator                   :P_maxAggregator;
FUNCTION newHeadAggregator                  :P_headAggregator;
FUNCTION newCustomAggregator(CONST ex:P_expressionLiteral; CONST contextPointer:pointer):P_expressionAggregator;
IMPLEMENTATION
FUNCTION newAggregator(CONST op:T_tokenType):P_aggregator;
  begin
    case op of
      tt_operatorConcat   : new(P_concatAggregator      (result),create);
      tt_operatorStrConcat: new(P_stringConcatAggregator(result),create);
      tt_operatorLazyAnd  : new(P_andAggregator         (result),create);
      tt_operatorLazyOr   : new(P_orAggregator          (result),create);
      tt_operatorOrElse   : new(P_headAggregator        (result),create);
      else                  new(P_opAggregator          (result),create(op));
    end;
  end;

FUNCTION newListAggregator:P_listAggregator; begin new(result,create); end;
FUNCTION newMinAggregator :P_minAggregator;  begin new(result,create); end;
FUNCTION newMaxAggregator :P_maxAggregator;  begin new(result,create); end;
FUNCTION newHeadAggregator:P_headAggregator; begin new(result,create); end;
FUNCTION newCustomAggregator(CONST ex:P_expressionLiteral; CONST contextPointer:pointer):P_expressionAggregator;
  begin
    new(result,create(ex,contextPointer));
  end;

CONSTRUCTOR T_aggregatorWithResultLiteral.create(CONST initialValue:P_literal); begin resultLiteral:=initialValue; end;
CONSTRUCTOR T_listAggregator  .create; begin inherited create(newListLiteral); end;
CONSTRUCTOR T_concatAggregator.create; begin inherited create(newListLiteral); end;
CONSTRUCTOR T_headAggregator  .create; begin inherited create(nil);            end;
CONSTRUCTOR T_minAggregator   .create; begin inherited create(newVoidLiteral); end;
CONSTRUCTOR T_maxAggregator   .create; begin inherited create(newVoidLiteral); end;
CONSTRUCTOR T_stringConcatAggregator.create; begin inherited create(newStringLiteral('',true)); end;
CONSTRUCTOR T_andAggregator         .create; begin boolResult:=true;  end;
CONSTRUCTOR T_orAggregator          .create; begin boolResult:=false; end;
CONSTRUCTOR T_opAggregator.create(CONST operatorToken: T_tokenType);
  begin
    inherited create(newVoidLiteral);
    op:=operatorToken;
  end;
CONSTRUCTOR T_expressionAggregator.create(CONST ex: P_expressionLiteral; CONST contextPointer: pointer);
  begin
    inherited create(nil);
    aggregator:=ex; aggregator^.rereference;
    aggregationContext:=contextPointer;
  end;

DESTRUCTOR T_aggregatorWithResultLiteral.destroy; begin if resultLiteral<>nil then disposeLiteral(resultLiteral); end;
DESTRUCTOR T_expressionAggregator       .destroy; begin disposeLiteral(aggregator); inherited destroy; end;
DESTRUCTOR T_andAggregator              .destroy; begin end;
DESTRUCTOR T_orAggregator               .destroy; begin end;

PROCEDURE T_listAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if L=nil then exit;
    P_listLiteral(resultLiteral)^.append(L,not(doDispose));
  end;

PROCEDURE T_concatAggregator.addToAggregation(L: P_literal; CONST doDispose: boolean; CONST location: T_tokenLocation; CONST adapters: P_adapters);
  begin
    if L=nil then exit;
    if L^.literalType in C_compoundTypes
    then P_listLiteral(resultLiteral)^.appendAll(P_compoundLiteral(L))
    else P_listLiteral(resultLiteral)^.append(L,true);
    if doDispose then disposeLiteral(L);
  end;

PROCEDURE T_headAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if L=nil then exit;
    if not(earlyAbort) then begin
      if resultLiteral<>nil then disposeLiteral(resultLiteral);
      resultLiteral:=L^.rereferenced;
    end;
    if doDispose then disposeLiteral(L);
  end;

PROCEDURE T_minAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if L=nil then exit;
    if (L^.literalType<>lt_void) and ((resultLiteral^.literalType=lt_void) or L^.leqForSorting(resultLiteral)) then begin
      disposeLiteral(resultLiteral);
      resultLiteral:=L^.rereferenced;
    end;
    if doDispose then disposeLiteral(L);
  end;

PROCEDURE T_maxAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if L=nil then exit;
    if (L^.literalType<>lt_void) and ((resultLiteral^.literalType=lt_void) or not(L^.leqForSorting(resultLiteral))) then begin
      disposeLiteral(resultLiteral);
      resultLiteral:=L^.rereferenced;
    end;
    if doDispose then disposeLiteral(L);
  end;

PROCEDURE T_stringConcatAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR newResult:P_literal;
  begin
    if L=nil then exit;
    if (resultLiteral^.literalType=lt_string) and (L^.literalType in C_scalarTypes) then begin
      P_stringLiteral(resultLiteral)^.append(P_scalarLiteral(L)^.stringForm);
    end else begin
      newResult:=resolveOperator(resultLiteral,tt_operatorStrConcat,L,location,adapters^);
      disposeLiteral(resultLiteral);
      resultLiteral:=newResult;
    end;
    if doDispose then disposeLiteral(L);
  end;

PROCEDURE T_andAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if L=nil then exit;
    if L^.literalType=lt_boolean then begin
      boolResult:=boolResult and P_boolLiteral(L)^.value;
    end else begin
      adapters^.raiseError('Cannot apply AND-aggregator to element of type '+L^.typeString,location);
    end;
    if doDispose then disposeLiteral(L);
  end;

PROCEDURE T_orAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  begin
    if L=nil then exit;
    if L^.literalType=lt_boolean then begin
      boolResult:=boolResult or P_boolLiteral(L)^.value;
    end else begin
      adapters^.raiseError('Cannot apply OR-aggregator to element of type '+L^.typeString,location);
    end;
    if doDispose then disposeLiteral(L);
  end;

PROCEDURE T_opAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR newResult:P_literal;
  begin
    if L=nil then exit;
    if resultLiteral=nil
    then resultLiteral:=L^.rereferenced
    else begin
      newResult:=resolveOperator(resultLiteral,op,L,location,adapters^);
      disposeLiteral(resultLiteral);
      resultLiteral:=newResult;
    end;
    if doDispose then disposeLiteral(L);
  end;

PROCEDURE T_expressionAggregator.addToAggregation(L:P_literal; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR newValue:P_literal;
      toReduce,dummy:P_token;
  begin
    if L=nil then exit;
    //writeln('Aggregating using T_expressionAggregator - ',resultLiteral=nil);
    if resultLiteral=nil
    then resultLiteral:=L^.rereferenced
    else begin
      newValue:=aggregator^.evaluateToLiteral(location,aggregationContext,resultLiteral,L);
      disposeLiteral(resultLiteral);
      resultLiteral:=newValue;
      if resultLiteral=nil then begin
        adapters^.raiseError('Aggregation failed for element '+L^.toString(50),location);
        resultLiteral:=newVoidLiteral;
      end;
    end;
    if doDispose then disposeLiteral(L);
  end;

FUNCTION T_aggregator.earlyAbort: boolean;
  begin
    result:=false;
  end;

FUNCTION T_headAggregator.earlyAbort: boolean;
  begin
    result:=(resultLiteral<>nil) and (resultLiteral^.literalType<>lt_void);
  end;

FUNCTION T_andAggregator.earlyAbort: boolean;
  begin
    result:=not(boolResult);
  end;

FUNCTION T_orAggregator.earlyAbort: boolean;
  begin
    result:=boolResult;
  end;

FUNCTION T_aggregatorWithResultLiteral.getResult: P_literal;
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
    result:=newBoolLiteral(boolResult);
  end;

FUNCTION T_orAggregator.getResult: P_literal;
  begin
    result:=newBoolLiteral(boolResult);
  end;

end.
