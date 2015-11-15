{$ifdef include_implementation}
{$MACRO ON}
{$define UNARY_NUM_TO_REAL:=
FUNCTION recurse(CONST x:P_literal):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    case x^.literalType of
      lt_expression: result:=subruleApplyFuncImpl(P_expressionLiteral(x),ID_MACRO,tokenLocation);
      lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
      lt_int : try result:=newRealLiteral(CALL_MACRO(P_intLiteral (x)^.value)); except result:=newRealLiteral(Nan) end;
      lt_real: try result:=newRealLiteral(CALL_MACRO(P_realLiteral(x)^.value)); except result:=newRealLiteral(Nan) end;
      lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: begin
        result:=newListLiteral;
        for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false,adapters);
      end;
      else raiseNotApplicableError(ID_MACRO,x^.literalType,'',tokenLocation,adapters);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1)
  then result:=recurse(params^.value(0));
end}

FUNCTION sqrt_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=sqrt}
{$define ID_MACRO:='sqrt'}
UNARY_NUM_TO_REAL;

FUNCTION sin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=sin}
{$define ID_MACRO:='sin'}
UNARY_NUM_TO_REAL;

FUNCTION arcsin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=arcsin}
{$define ID_MACRO:='arcsin'}
UNARY_NUM_TO_REAL;

FUNCTION cos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=cos}
{$define ID_MACRO:='cos'}
UNARY_NUM_TO_REAL;

FUNCTION arccos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=arccos}
{$define ID_MACRO:='arccos'}
UNARY_NUM_TO_REAL;

FUNCTION tan_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=tan}
{$define ID_MACRO:='tan'}
UNARY_NUM_TO_REAL;

FUNCTION arctan_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=arctan}
{$define ID_MACRO:='arctan'}
UNARY_NUM_TO_REAL;

FUNCTION exp_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=exp}
{$define ID_MACRO:='exp'}
UNARY_NUM_TO_REAL;

FUNCTION ln_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=ln}
{$define ID_MACRO:='ln'}
UNARY_NUM_TO_REAL;

{$undef UNARY_NUM_TO_REAL}

FUNCTION not_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  FUNCTION not_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: result:=subruleApplyFuncImpl(P_expressionLiteral(x),'not',tokenLocation);
        lt_boolean: result:=newBoolLiteral(not(P_boolLiteral(x)^.value));
        lt_int:     result:=newIntLiteral (not(P_intLiteral (x)^.value));
        lt_list,lt_booleanList, lt_intList, lt_emptyList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do
            P_listLiteral(result)^.append(not_rec(P_listLiteral(x)^.value(i)),false,adapters);
        end;
        else raiseNotApplicableError('not',x^.literalType,'',tokenLocation,adapters);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=not_rec(params^.value(0));
  end;

{$define UNARY_NUM_TO_SAME:=
FUNCTION recurse(CONST x:P_literal):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    case x^.literalType of
      lt_expression: result:=subruleApplyFuncImpl(P_expressionLiteral(x),ID_MACRO,tokenLocation);
      lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
      lt_int : result:=newIntLiteral (CALL_MACRO(P_intLiteral (x)^.value));
      lt_real: result:=newRealLiteral(CALL_MACRO(P_realLiteral(x)^.value));
      lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: begin
        result:=newListLiteral;
        for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false,adapters);
      end;
      else raiseNotApplicableError(ID_MACRO,x^.literalType,'',tokenLocation,adapters);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1)
  then result:=recurse(params^.value(0));
end}

FUNCTION abs_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=abs}
{$define ID_MACRO:='abs'}
UNARY_NUM_TO_SAME;

FUNCTION sqr_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=sqr}
{$define ID_MACRO:='sqr'}
UNARY_NUM_TO_SAME;

{$undef UNARY_NUM_TO_SAME}

{$define ROUND_IMPLEMENTATION:=
  FUNCTION recurse1(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: result:=subruleApplyFuncImpl(P_expressionLiteral(x),ID_MACRO,tokenLocation);
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : begin result:=x; x^.rereference; end;
        lt_real: result:=newIntLiteral(CALL_MACRO(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse1(P_listLiteral(x)^.value(i)),false,adapters);
        end;
        else raiseNotApplicableError(ID_MACRO,x^.literalType,'',tokenLocation,adapters);
      end;
    end;

  FUNCTION recurse2(CONST x,y:P_literal):P_literal;
    FUNCTION myRound(CONST x:T_myFloat; CONST y:int64):P_literal; inline;
      VAR pot:T_myFloat;
          i:int64;
      begin
        pot:=1;
        i:=0;
        while i<y do begin pot:=pot*10;  inc(i); end;
        while i>y do begin pot:=pot*0.1; dec(i); end;
        result:=newRealLiteral(CALL_MACRO(x*pot)/pot);
      end;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : begin result:=x; x^.rereference; end;
        lt_real: case y^.literalType of
          lt_error,lt_listWithError: begin result:=y; result^.rereference; end;
          lt_int: result:=myRound(P_realLiteral(x)^.value,P_intLiteral(y)^.value);
          lt_list,lt_intList,lt_emptyList: begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(y)^.size-1 do P_listLiteral(result)^.append(recurse2(x,P_listLiteral(y)^.value(i)),false,adapters);
          end;
          else raiseNotApplicableError(ID_MACRO,y^.literalType,' (second parameter)',tokenLocation,adapters);
        end;
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: case y^.literalType of
          lt_error,lt_listWithError: begin result:=y; result^.rereference; end;
          lt_int: begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse2(P_listLiteral(x)^.value(i),y),false,adapters);
          end;
          lt_list,lt_intList,lt_emptyList: if P_listLiteral(x)^.size=P_listLiteral(y)^.size then begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse2(P_listLiteral(x)^.value(i),P_listLiteral(y)^.value(i)),false,adapters);
          end else adapters.raiseError('Incompatible list lengths given for built in function '+ID_MACRO,tokenLocation);
          else raiseNotApplicableError(ID_MACRO,y^.literalType,' (second parameter)',tokenLocation,adapters);
        end;
        else raiseNotApplicableError(ID_MACRO,x^.literalType,' (first parameter)',tokenLocation,adapters);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=recurse1(params^.value(0)) else
    if (params<>nil) and (params^.size=2) then result:=recurse2(params^.value(0),params^.value(1));
  end}

FUNCTION round_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=round}
{$define ID_MACRO:='round'}
ROUND_IMPLEMENTATION;

FUNCTION ceil_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=ceil}
{$define ID_MACRO:='ceil'}
ROUND_IMPLEMENTATION;

FUNCTION floor_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=floor}
{$define ID_MACRO:='floor'}
ROUND_IMPLEMENTATION;
{$undef ROUND_IMPLEMENTATION}

FUNCTION sign_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  FUNCTION sign_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: result:=subruleApplyFuncImpl(P_expressionLiteral(x),'sign',tokenLocation);
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newIntLiteral(sign(P_intLiteral (x)^.value));
        lt_real: result:=newIntLiteral(sign(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(sign_rec(P_listLiteral(x)^.value(i)),false,adapters);
        end;
        else raiseNotApplicableError('sign',x^.literalType,'',tokenLocation,adapters);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=sign_rec(params^.value(0));
  end;


FUNCTION time_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR res:P_literal;
      profiler:TEpikTimer;
      t:double;

  PROCEDURE appendPair(VAR result:P_literal; CONST el0:string; CONST el1:P_literal);
    begin
      P_listLiteral(result)^.append(newListLiteral^.appendString(el0)^.append(el1,false,adapters),false,adapters);
    end;

  FUNCTION evaluateNullary(CONST subruleLiteral:P_expressionLiteral):P_literal;
    VAR tempcontext:T_evaluationContext;
    begin
      tempcontext.create(P_adapters(@adapters));
      profiler.start;
      result:=P_subrule(subruleLiteral^.value)^.directEvaluateNullary(subruleLiteral,0,tempcontext);
      t:=profiler.Elapsed;
      tempcontext.destroy;
    end;

  FUNCTION evaluate(CONST subruleLiteral:P_expressionLiteral; CONST parameters:P_listLiteral):P_literal;
    VAR tempcontext:T_evaluationContext;
        first:P_token;
    begin
      tempcontext.create(P_adapters(@adapters));
      first:=tempcontext.newToken(tokenLocation,'',tt_literal,subruleLiteral); subruleLiteral^.rereference;
      first^.next:=tempcontext.newToken(tokenLocation,'',tt_parList,parameters); parameters^.rereference;
      profiler.start;
      reduceExpression(first,0,tempcontext);
      t:=profiler.Elapsed;
      if (first<>nil) and (first^.next=nil) and (first^.tokType=tt_literal)
      then begin result:=first^.data; result^.rereference; end
      else result:=nil;
      if first<>nil then tempcontext.cascadeDisposeToken(first);
      tempcontext.destroy;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_expression) and
      ((params^.size=1) or (params^.size=2) and (params^.value(1)^.literalType in C_validListTypes)) then begin
      profiler := TEpikTimer.create(nil);
      profiler.clear;
      if params^.size=2 then res:=evaluate(P_expressionLiteral(params^.value(0)),P_listLiteral(params^.value(1)))
                        else res:=evaluateNullary(P_expressionLiteral(params^.value(0)));
      profiler.free;
      if res<>nil then begin
        result:=newListLiteral;
        appendPair(result,'expression',newStringLiteral(params^.value(0)^.toString));
        if res^.literalType<>lt_void then appendPair(result,'result',res)
                                     else disposeLiteral(res);
        appendPair(result,'time'  ,newRealLiteral(t));
      end;
    end;
  end;

FUNCTION sleep_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  CONST SECONDS_PER_DAY=24*60*60;
  VAR sleepUntil:double;
      sleepInt:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_real,lt_int]) then begin
      sleepUntil:=now;
      result:=newVoidLiteral;
      if params^.value(0)^.literalType=lt_int
      then sleepUntil:=sleepUntil+P_intLiteral (params^.value(0))^.value/SECONDS_PER_DAY
      else sleepUntil:=sleepUntil+P_realLiteral(params^.value(0))^.value/SECONDS_PER_DAY;
      while (now<sleepUntil) and (adapters.noErrors) do begin
        sleepInt:=round(900*(sleepUntil-now)*SECONDS_PER_DAY);
        if sleepInt>100 then sleepInt:=100;
        if (sleepInt>0) then sleep(sleepInt);
      end;
    end;
  end;

FUNCTION stringToExpression(s:ansistring; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_scalarLiteral;
  VAR first,last,temp:P_token;

      context:T_evaluationContext;
      exTokens:T_tokenArray;
      package:P_package;
      dummyForLastComment:ansistring='';
  begin
    context.create(P_adapters(@adapters));

    package:=guessPackageForProvider(location.fileName);
    exTokens:=tokenizeAll(s,location,package,adapters);
    exTokens.step(package,dummyForLastComment);
    if exTokens.atEnd then begin
      adapters.raiseError('The parsed expression appears to be empty',location);
      exit(nil);
    end;
    first:=context.newToken(exTokens.current);
    last:=first;
    exTokens.step(package,dummyForLastComment);
    while not(exTokens.atEnd) do begin
      last^.next:=context.newToken(exTokens.current);
      last:=last^.next;
      exTokens.step(package,dummyForLastComment);
    end;
    exTokens.destroy;


    if first^.tokType<>tt_expBraceOpen then begin
      temp:=context.newToken(location,'',tt_expBraceOpen);
      temp^.next:=first; first:=temp;
      last^.next:=context.newToken(location,'',tt_expBraceClose);
      last:=last^.next;
    end;

    digestInlineExpression(first,context);
    if (adapters.noErrors) and (first^.next<>nil) then adapters.raiseCustomMessage(mt_el4_parsingError,'The parsed expression goes beyond the expected limit... I know this is a fuzzy error. Sorry.',location);
    if not(adapters.noErrors) then begin
      context.cascadeDisposeToken(first);
      context.destroy;
      exit(newErrorLiteral);
    end;
    if (first^.tokType<>tt_literal) or (P_literal(first^.data)^.literalType<>lt_expression) then begin
      context.disposeToken(first);
      adapters.raiseCustomMessage(mt_el5_systemError,'This is unexpected. The result of mnh_tokens.stringToExpression should be an expression!',location);
      context.destroy;
      exit(newErrorLiteral);
    end;
    result:=P_expressionLiteral(first^.data);
    first^.tokType:=tt_EOL;
    first^.data:=nil;
    context.disposeToken(first);
    context.destroy;
  end;

FUNCTION expression_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  FUNCTION expression_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_string: result:=stringToExpression(P_stringLiteral(x)^.value,tokenLocation,adapters);
        lt_list,lt_stringList,lt_emptyList:  begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if adapters.noErrors then
            P_listLiteral(result)^.append(expression_rec(P_listLiteral(x)^.value(i)),false,adapters);
        end;
        else result:=newErrorLiteralRaising('Cannot apply "expression" to literal of type '+C_typeString[x^.literalType],tokenLocation,adapters);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_emptyList,lt_string])
    then result:=expression_rec(params^.value(0));
  end;

FUNCTION arity_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  FUNCTION getArity(CONST subruleLiteral:P_expressionLiteral):longint;
    begin
      result:=length(P_subrule(subruleLiteral^.value)^.pattern.sig);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_expression)
    then result:=newIntLiteral(getArity(P_expressionLiteral(params^.value(0))));
  end;

FUNCTION parameterNames_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  FUNCTION getNames(CONST subRule:P_subrule):P_listLiteral;
    VAR i:longint;
    begin
      result:=newListLiteral;
      for i:=0 to length(subRule^.pattern.sig)-1 do result^.appendString(subRule^.pattern.sig[i].id);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_expression)
    then result:=getNames(P_subrule(P_expressionLiteral(params^.value(0))^.value));
  end;

FUNCTION setThreadCount_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_int) then begin
      limitPoolThreads(P_intLiteral(params^.value(0))^.value-1);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION sandBoxExecute_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR src:T_arrayOfString;
      i,j:longint;
      messages:T_storedMessages;

  FUNCTION headByMessageType(CONST messageType:T_messageType):P_listLiteral;
    begin
      if C_errorLevelTxt[messageType]=''
      then result:=newListLiteral^.appendString(copy(getEnumName(TypeInfo(messageType),ord(messageType)),4,1000))
      else result:=newListLiteral^.appendString(C_errorLevelTxt[messageType]);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_string,lt_stringList]) then begin
      if params^.value(0)^.literalType=lt_string then src:=P_stringLiteral(params^.value(0))^.value else begin
        setLength(src,0);
        for i:=0 to P_listLiteral(params^.value(0))^.size-1 do append(src,P_stringLiteral(P_listLiteral(params^.value(0))^.value(i))^.value);
      end;
      messages:=runAlone(src);
      result:=newListLiteral;
      for i:=0 to length(messages)-1 do with messages[i] do begin
        if length(multiMessage)>0 then begin
          simpleMessage:=multiMessage[0];
          for j:=1 to length(multiMessage)-1 do simpleMessage:=simpleMessage+C_lineBreakChar+multiMessage[j];
        end;
        P_listLiteral(result)^.append(
           headByMessageType(messageType)^
          .appendString(ansistring(location))^
          .appendString(simpleMessage),false,adapters);
      end;
    end;
  end;

FUNCTION group_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  TYPE T_groupMap=specialize G_literalKeyMap<P_literal>;
  VAR listToGroup:P_listLiteral;
      keyList:array of P_literal;
      aggregator:P_expressionLiteral;
      tempContext:T_evaluationContext;
      groupMap:T_groupMap;
      groupList:T_groupMap.KEY_VALUE_LIST;

  PROCEDURE makeKeysByList(CONST list:P_listLiteral);
    VAR i:longint;
    begin
      setLength(keyList,list^.size);
      for i:=0 to length(keyList)-1 do keyList[i]:=list^.value(i);
    end;

  PROCEDURE makeKeysByIndex(CONST index:longint);
    VAR i:longint;
        dummy:P_literal;
    begin
      dummy:=newErrorLiteral;
      setLength(keyList,listToGroup^.size);
      for i:=0 to length(keyList)-1 do begin
        if (listToGroup^.value(i)^.literalType in C_validListTypes) and (P_listLiteral(listToGroup^.value(i))^.size>index)
        then keyList[i]:=P_listLiteral(listToGroup^.value(i))^.value(index)
        else keyList[i]:=dummy;
      end;
      disposeLiteral(dummy);
    end;

  PROCEDURE addToAggregation(CONST groupKey:P_literal; CONST L:P_literal); inline;
    VAR newLit:P_literal;
        resultLiteral:P_literal;
    begin
      resultLiteral:=groupMap.get(groupKey,nil);

      if aggregator=nil then begin
        if resultLiteral=nil then resultLiteral:=newListLiteral;
        P_listLiteral(resultLiteral)^.append(L,true,adapters);
      end else begin
        if resultLiteral=nil then begin
          resultLiteral:=L; L^.rereference;
        end else begin
          newLit:=P_subrule(aggregator^.value)^.directEvaluateAggregator(resultLiteral,L,aggregator,0,tempContext);
          if newLit<>nil then begin
            disposeLiteral(resultLiteral);
            resultLiteral:=newLit;
          end else begin
            adapters.raiseError('Error performing aggregation in group-construct with aggregator '+aggregator^.toString,C_nilTokenLocation);
            exit;
          end;
        end;
      end;
      groupMap.put(groupKey,resultLiteral);
    end;

  VAR inputIndex,outputIndex:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.size<=3) and
       (params^.value(0)^.literalType in C_validListTypes) and
      ((params^.value(1)^.literalType in C_validListTypes) and (P_listLiteral(params^.value(1))^.size=P_listLiteral(params^.value(0))^.size) or (params^.value(1)^.literalType=lt_int)) and
      ((params^.size=2) or (params^.value(2)^.literalType=lt_expression))
    then begin
      listToGroup:=P_listLiteral(params^.value(0));

      if params^.value(1)^.literalType=lt_int
      then makeKeysByIndex(P_intLiteral(params^.value(1))^.value)
      else makeKeysByList(P_listLiteral(params^.value(1)));

      if (params^.size=3) then aggregator:=P_expressionLiteral(params^.value(2))
                          else aggregator:=nil;

      groupMap.create();
      tempContext.create(P_adapters(@adapters));
      for inputIndex:=0 to length(keyList)-1 do if adapters.noErrors then
        addToAggregation(keyList[inputIndex],listToGroup^.value(inputIndex));
      groupList:=groupMap.keyValueList;
      result:=newListLiteral;
      for outputIndex:=0 to length(groupList)-1 do P_listLiteral(result)^.append(groupList[outputIndex].value,false,adapters);
      groupMap.destroy;
      tempContext.destroy;
    end;
  end;

FUNCTION inner_formatting(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):T_arrayOfString;
  FUNCTION splitFormatString(CONST formatString:ansistring):T_arrayOfString;
    VAR i:longint=1;
        partStart:longint=1;
        bracketLevel:longint=0;
        part:ansistring;
        fmtPart:boolean=false;
    begin
      part:='';
      setLength(result,0);
      while i<=length(formatString) do begin
        case formatString[i] of
          '{': begin
                 if fmtPart then inc(bracketLevel);
                 part:=part+formatString[i];
               end;
          '}': begin
                 if fmtPart then dec(bracketLevel);
                 part:=part+formatString[i];
               end;
          '%': if fmtPart then begin
                 if bracketLevel>0
                 then part:=part+formatString[i]
                 else adapters.raiseError('Invalid format specification: '+copy(formatString,partStart,length(formatString)),tokenLocation);
               end else begin
                 if (i+1<=length(formatString)) and (formatString[i+1]='%') then begin
                   part:=part+'%';
                   inc(i);
                 end else begin
                   setLength(result,length(result)+1);
                   result[length(result)-1]:=part;
                   part:='%';
                   partStart:=i;
                   bracketLevel:=0;
                   fmtPart:=true;
                 end;
               end;
          'a'..'z','A'..'Z':
               if fmtPart then begin
                 part:=part+formatString[i];
                 if bracketLevel<=0 then begin
                   if not(formatString[i] in FORMAT_CHARS) then
                     adapters.raiseError('Invalid format specification: Unknown format "'+formatString[i]+'"',tokenLocation);
                   setLength(result,length(result)+1);
                   result[length(result)-1]:=part;
                   part:='';
                   partStart:=i;
                   bracketLevel:=0;
                   fmtPart:=false;
                 end;
               end else part:=part+formatString[i];
          else part:=part+formatString[i];
        end;
        inc(i);
      end;
      if part<>'' then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=part;
      end;
    end;

  FUNCTION getFormatSubrule(VAR parts:T_arrayOfString):P_literal;
    VAR i,k:longint;
        needSubRule:boolean=false;
        expressionString:ansistring;

    PROCEDURE splitPart(VAR part:ansistring; CONST index:longint);
      VAR expPart:ansistring;
      begin
        part:=trim(part);
        if pos('{',part)<=0 then begin
          if pos('}',part)>0 then adapters.raiseError('Invalid format specification: '+escapeString(part),tokenLocation);
          expPart:='$'+intToStr(index);
        end else begin
          expPart:=copy(part,3,pos('}',part)-3);
          part:='%'+copy(part,pos('}',part)+1,length(part));
          if (pos('{',part)>0) or (pos('}',part)>0) then adapters.raiseError('Invalid format specification: '+escapeString(part),tokenLocation);
        end;
        if expressionString=''
        then expressionString:=                 '['+expPart
        else expressionString:=expressionString+','+expPart;
        if part='%' then part:='%S';
      end;

    begin
      //Check if a rule is needed at all:
      for i:=0 to length(parts)-1 do if odd(i) and (copy(trim(parts[i]),2,1)='{') then needSubRule:=true;
      if not(needSubRule) then exit(nil);

      expressionString:='';
      k:=0;
      for i:=0 to length(parts)-1 do begin
        if odd(i) then begin
          if parts[i]<>'' then begin
            splitPart(parts[i],k);
            inc(k);
          end;
        end;
      end;
      if adapters.noErrors
      then result:=stringToExpression('{'+expressionString+']}',
                                      tokenLocation,
                                      adapters)
      else result:=nil;
    end;

  VAR parts:T_arrayOfString;
      formats:array of T_format;
      formatSubrule:P_literal;
      i:longint;
      listSize:longint=-1;
      context:T_evaluationContext;

  FUNCTION getFormattedString(CONST index:longint):ansistring;
    FUNCTION simpleFormat(CONST parts:T_arrayOfString; CONST p:T_listLiteral):ansistring;
      VAR i,k:longint;
          L:P_literal;
      begin
        result:='';
        k:=0;
        try
          for i:=0 to length(parts)-1 do if odd(i) then begin
            if k<p.size then begin
              L:=p.value(k);
              case L^.literalType of
                lt_int   : result:=result+formats[i].format(P_intLiteral   (L)^.value);
                lt_real  : result:=result+formats[i].format(P_realLiteral  (L)^.value);
                lt_string: result:=result+formats[i].format(P_stringLiteral(L)^.value);
                else       result:=result+formats[i].format(L^.toString);
              end;
            end else result:=result+'%'+parts[i]+'%';
            inc(k);
          end else result:=result+parts[i];
        except on E:Exception do adapters.raiseError('Error during formatting: '+e.message,tokenLocation);
        end;
      end;
    VAR fpar:P_listLiteral;
        temp:P_literal;
        k:longint;
    begin
      //prepare parameters
      fpar:=newListLiteral;
      for k:=1 to params^.size-1 do
      if params^.value(k)^.literalType in C_validListTypes
      then fpar^.append(P_listLiteral(params^.value(k))^.value(index),true,adapters)
      else fpar^.append(              params^.value(k)               ,true,adapters);

      if formatSubrule<>nil then begin
        temp:=P_subrule(P_expressionLiteral(formatSubrule)^.value)^.directEvaluate(
          P_expressionLiteral(formatSubrule),
          fpar,
          0,
          context);
        disposeLiteral(fpar);
        if (temp<>nil) and (temp^.literalType in C_validListTypes)
        then fpar:=P_listLiteral(temp)
        else begin
          if temp<>nil then disposeLiteral(temp);
          exit(''); //One of the called routines already raised a proper error
        end;
      end;
      result:=simpleFormat(parts,fpar^);
      disposeLiteral(fpar);
    end;

  begin
    setLength(result,0);
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string) then begin
      for i:=1 to params^.size-1 do if params^.value(i)^.literalType in C_validListTypes then begin
        if listSize=-1 then listSize:=P_listLiteral(params^.value(i))^.size
                    else if listSize<>P_listLiteral(params^.value(i))^.size then begin
          adapters.raiseError('Invalid list lengths '+intToStr(listSize)+' and '+intToStr(P_listLiteral(params^.value(i))^.size)+' for formatting.',tokenLocation);
          exit;
        end;
      end;
      if listSize=-1 then listSize:=1;
      parts:=splitFormatString(P_stringLiteral(params^.value(0))^.value);
      formatSubrule:=getFormatSubrule(parts);
      if (formatSubrule<>nil) and (formatSubrule^.literalType<>lt_expression) then begin
        disposeLiteral(formatSubrule);
        exit;
      end;
      if formatSubrule=nil
      then i:=length(parts) shr 1
      else i:=length(P_subrule(P_expressionLiteral(formatSubrule)^.value)^.pattern.sig);
      if i<>(params^.size-1) then begin
        adapters.raiseError('Invalid format statement; found '+intToStr(i)+' placeholders but '+intToStr(params^.size-1)+' variables.',tokenLocation);
        if formatSubrule<>nil then begin
          adapters.raiseError('Helper subrule is: '+formatSubrule^.toString,tokenLocation);
          disposeLiteral(formatSubrule);
        end;
        exit;
      end;
      setLength(formats,length(parts));
      for i:=0 to length(parts)-1 do if odd(i) then formats[i].create(parts[i]);
      if (formatSubrule<>nil) then context.create(P_adapters(@adapters));
      setLength(result,listSize);
      for i:=0 to listSize-1 do if (adapters.noErrors) then result[i]:=getFormattedString(i);
      for i:=0 to length(parts)-1 do if odd(i) then formats[i].destroy;
      if (formatSubrule<>nil) then begin
        context.destroy;
        disposeLiteral(formatSubrule);
      end;
      if not(adapters.noErrors) then setLength(result,0);
    end;
  end;

FUNCTION format_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR txt:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string) then begin
      txt:=inner_formatting(params,tokenLocation,adapters);
      if length(txt)=1 then result:=newStringLiteral(txt[0])
      else begin
        result:=newListLiteral;
        for i:=0 to length(txt)-1 do P_listLiteral(result)^.appendString(txt[i]);
      end;
    end;
  end;

FUNCTION printf_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string) then begin
      system.enterCriticalSection(print_cs);
      adapters.printOut(formatTabs(reSplit(inner_formatting(params,tokenLocation,adapters))));
      system.leaveCriticalSection(print_cs);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION formatTime_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR L:P_listLiteral;
      i:longint;
      fmt:ansistring;
  FUNCTION fmtIt(CONST t:double):P_stringLiteral; inline;
    begin
      result:=newStringLiteral(FormatDateTime(fmt,t));
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) then begin
      fmt:=P_stringLiteral(params^.value(0))^.value;
      case params^.value(1)^.literalType of
        lt_int:  result:=fmtIt(P_intLiteral (params^.value(1))^.value);
        lt_real: result:=fmtIt(P_realLiteral(params^.value(1))^.value);
        lt_emptyList: result:=newListLiteral;
        lt_realList,lt_intList,lt_numList: begin
          result:=newListLiteral;
          L:=P_listLiteral(params^.value(1));
          for i:=0 to L^.size-1 do case(L^.value(i)^.literalType) of
            lt_int : P_listLiteral(result)^.append(fmtIt(P_intLiteral (L^.value(i))^.value),false,adapters);
            lt_real: P_listLiteral(result)^.append(fmtIt(P_realLiteral(L^.value(i))^.value),false,adapters);
          end;
        end;
      end;
    end;
  end;

FUNCTION parseTime_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR format:ansistring;
  FUNCTION encodeDateTime(input:ansistring):double;
    CONST digits:charSet=['0'..'9'];
    VAR yStr:string='';
        mStr:string='';
        dStr:string='';
        hStr:string='';
        nStr:string='';
        sStr:string='';
        zStr:string='';
        i:word;
        yNum,mNum,dNum:word;
        hNum:word=0;
        nNum:word=0;
        sNum:word=0;
        zNum:word=0;
    begin
      if length(format)<>length(input) then begin
        adapters.raiseError('parseTime expects two strings of equal length as parameters',tokenLocation);
        exit;
      end;
      input:=cleanString(input,digits,'0');
      for i:=1 to length(format) do case format[i] of
        'Y': yStr:=yStr+input[i];
        'M': mStr:=mStr+input[i];
        'D': dStr:=dStr+input[i];
        'H': hStr:=hStr+input[i];
        'N': nStr:=nStr+input[i];
        'S': sStr:=sStr+input[i];
        'Z': zStr:=zStr+input[i];
      end;
      DecodeDate(now,yNum,mNum,dNum);
      yNum:=strToIntDef(yStr,yNum);
      mNum:=strToIntDef(mStr,mNum);
      dNum:=strToIntDef(dStr,dNum);
      hNum:=strToIntDef(hStr,hNum);
      nNum:=strToIntDef(nStr,nNum);
      sNum:=strToIntDef(sStr,sNum);
      zNum:=strToIntDef(zStr,zNum);
      try
        result:=EncodeDate(yNum,mNum,dNum)+EncodeTime(hNum,nNum,sNum,zNum);
      except
        on E:exception do adapters.raiseError('parseTime failed:'+E.message,tokenLocation);
      end;
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) then begin
      format:= uppercase(P_stringLiteral(params^.value(0))^.value);
      if (params^.value(1)^.literalType=lt_string) then
        result:=newRealLiteral(encodeDateTime(P_stringLiteral(params^.value(1))^.value))
      else if (params^.value(1)^.literalType in [lt_stringList,lt_emptyList]) then begin
        result:=newListLiteral;
        for i:=0 to P_listLiteral(params^.value(1))^.size-1 do
          P_listLiteral(result)^.appendReal(encodeDateTime(P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value));
      end;
    end;
  end;

{$endif}
{$ifdef include_initialization}
  //Unary Numeric -> real
  registerRule(MATH_NAMESPACE,'sqrt',@sqrt_imp,'sqrt(n);#Returns the square root of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'sin',@sin_imp,'sin(n);#Returns the sine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arcsin',@arcsin_imp,'arcsin(n);#Returns the arcsine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'cos',@cos_imp,'cos(n);#Returns the cosine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arccos',@arccos_imp,'arccos(n);#Returns the arccosine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'tan',@tan_imp,'tan(n);#Returns the tangent of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arctan',@arctan_imp,'arctan(n);#Returns the arctangent of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'exp',@exp_imp,'exp(n);#Returns the exponential of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'ln',@ln_imp,'ln(n);#Returns the natural logarithm of numeric or expression parameter n');
  //Unary Boolean -> boolean
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'not',@not_imp,'not(b:boolean);#not(b:booleanList);#Returns the negated value of b#not(i:int);#not(i:intList);#Returns the bitwise negated value of i');
  //Unary Numeric -> same (i.e. I -> I, R -> R)
  registerRule(MATH_NAMESPACE,'abs',@abs_imp,'abs(n);#Returns the absolute value of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'sqr',@sqr_imp,'sqr(n);#Returns the square of numeric or expression parameter n');
  //Unary Numeric -> Integer
  registerRule(MATH_NAMESPACE,'sign',@sign_imp,'sign(n);#Returns the sign of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'ceil',@ceil_imp,'ceil(x);#Returns the smallest integer >=x#ceil(x,k);#Does the same but with k digits precision');
  registerRule(MATH_NAMESPACE,'floor',@floor_imp,'floor(x);#Returns the largest integer <=x#floor(x,k);#Does the same but with k digits precision');
  //round might be binary...
  registerRule(MATH_NAMESPACE,'round',@round_imp,'round(x);#Returns the value of x, rounded to the nearest integer#round(x,k);#Returns the value of x rounded to k-digits precision');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'time',@time_imp,'time(E:expression);#Evaluates E (without parameters) and returns a nested List with evaluation details.#'+
                                                         'time(E:expression,par:list);#Evaluates E@par and returns a nested List with evaluation details.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleep',@sleep_imp,'sleep(seconds:number);#Sleeps for the given number of seconds before returning void');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'expression',@expression_imp,'expression(S:string);#Returns an expression parsed from S');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'arity',@arity_imp,'arity(e:expression);#Returns the arity of expression e');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'parameterNames',@parameterNames_imp,'parameterNames(e:expression);#Returns the IDs of named parameters of e');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'setThreadCount',@setThreadCount_imp,'setThreadCount(n:int);#Sets the number of threads');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'group',@group_imp,'group(list,grouping);#Re-groups list by grouping (which is a sub-index or a list)#group(list,grouping,aggregator:expression);#Groups by grouping using aggregator on a per group basis');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sandBoxExecute',@sandBoxExecute_imp,'sandBoxExecute(source);#Executes the source (string or stringList) and returns created messages');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printf',@printf_imp,'fprint(formatString:string,...);#Prints a formatted version of the given 0..n parameters and returns void');
  registerRule(STRINGS_NAMESPACE,'format',@format_imp,'format(formatString:string,...);#Returns a formatted version of the given 0..n parameters');
  registerRule(STRINGS_NAMESPACE,'formatTime',@formatTime_imp,'formatTime(formatString:string,t);#Returns time t (numeric list or scalar) formatted using format string.');
  registerRule(STRINGS_NAMESPACE,'parseTime',@parseTime_imp,'parseTime(formatString:string,input:string);#Parses time from a given date format and input');
{$endif}
