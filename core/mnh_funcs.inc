{$ifdef include_implementation}
VAR builtinLocation_group,
    builtinLocation_time,
    builtinLocation_try,
    builtinLocation_printf,
    builtinLocation_format:T_identifiedInternalFunction;

{$i mnh_func_defines.inc}

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
        for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false);
      end;
      else raiseNotApplicableError(ID_MACRO,x^.literalType,'',tokenLocation,context.adapters^);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1)
  then result:=recurse(arg0);
end}

FUNCTION sqrt_imp intFuncSignature;
{$define CALL_MACRO:=sqrt}
{$define ID_MACRO:='sqrt'}
UNARY_NUM_TO_REAL;

FUNCTION sin_imp intFuncSignature;
{$define CALL_MACRO:=sin}
{$define ID_MACRO:='sin'}
UNARY_NUM_TO_REAL;

FUNCTION arcsin_imp intFuncSignature;
{$define CALL_MACRO:=arcsin}
{$define ID_MACRO:='arcsin'}
UNARY_NUM_TO_REAL;

FUNCTION cos_imp intFuncSignature;
{$define CALL_MACRO:=cos}
{$define ID_MACRO:='cos'}
UNARY_NUM_TO_REAL;

FUNCTION arccos_imp intFuncSignature;
{$define CALL_MACRO:=arccos}
{$define ID_MACRO:='arccos'}
UNARY_NUM_TO_REAL;

FUNCTION tan_imp intFuncSignature;
{$define CALL_MACRO:=tan}
{$define ID_MACRO:='tan'}
UNARY_NUM_TO_REAL;

FUNCTION arctan_imp intFuncSignature;
{$define CALL_MACRO:=arctan}
{$define ID_MACRO:='arctan'}
UNARY_NUM_TO_REAL;

FUNCTION exp_imp intFuncSignature;
{$define CALL_MACRO:=exp}
{$define ID_MACRO:='exp'}
UNARY_NUM_TO_REAL;

FUNCTION ln_imp intFuncSignature;
{$define CALL_MACRO:=ln}
{$define ID_MACRO:='ln'}
UNARY_NUM_TO_REAL;

{$undef UNARY_NUM_TO_REAL}

FUNCTION not_imp intFuncSignature;
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
            P_listLiteral(result)^.append(not_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('not',x^.literalType,'',tokenLocation,context.adapters^);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=not_rec(arg0);
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
        for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false);
      end;
      else raiseNotApplicableError(ID_MACRO,x^.literalType,'',tokenLocation,context.adapters^);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1)
  then result:=recurse(arg0);
end}

FUNCTION abs_imp intFuncSignature;
{$define CALL_MACRO:=abs}
{$define ID_MACRO:='abs'}
UNARY_NUM_TO_SAME;

FUNCTION sqr_imp intFuncSignature;
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
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse1(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError(ID_MACRO,x^.literalType,'',tokenLocation,context.adapters^);
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
            for i:=0 to P_listLiteral(y)^.size-1 do P_listLiteral(result)^.append(recurse2(x,P_listLiteral(y)^.value(i)),false);
          end;
          else raiseNotApplicableError(ID_MACRO,y^.literalType,' (second parameter)',tokenLocation,context.adapters^);
        end;
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: case y^.literalType of
          lt_error,lt_listWithError: begin result:=y; result^.rereference; end;
          lt_int: begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse2(P_listLiteral(x)^.value(i),y),false);
          end;
          lt_list,lt_intList,lt_emptyList: if P_listLiteral(x)^.size=P_listLiteral(y)^.size then begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse2(P_listLiteral(x)^.value(i),P_listLiteral(y)^.value(i)),false);
          end else context.adapters^.raiseError('Incompatible list lengths given for built in function '+ID_MACRO,tokenLocation);
          else raiseNotApplicableError(ID_MACRO,y^.literalType,' (second parameter)',tokenLocation,context.adapters^);
        end;
        else raiseNotApplicableError(ID_MACRO,x^.literalType,' (first parameter)',tokenLocation,context.adapters^);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=recurse1(arg0) else
    if (params<>nil) and (params^.size=2) then result:=recurse2(arg0,arg1);
  end}

FUNCTION round_imp intFuncSignature;
{$define CALL_MACRO:=round}
{$define ID_MACRO:='round'}
ROUND_IMPLEMENTATION;

FUNCTION ceil_imp intFuncSignature;
{$define CALL_MACRO:=ceil}
{$define ID_MACRO:='ceil'}
ROUND_IMPLEMENTATION;

FUNCTION floor_imp intFuncSignature;
{$define CALL_MACRO:=floor}
{$define ID_MACRO:='floor'}
ROUND_IMPLEMENTATION;
{$undef ROUND_IMPLEMENTATION}

FUNCTION sign_imp intFuncSignature;
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
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(sign_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('sign',x^.literalType,'',tokenLocation,context.adapters^);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=sign_rec(arg0);
  end;

FUNCTION time_imp intFuncSignature;
  VAR res:P_literal;
      t:double;

  PROCEDURE appendPair(VAR result:P_literal; CONST el0:string; CONST el1:P_literal);
    begin
      P_listLiteral(result)^.append(newListLiteral^.appendString(el0)^.append(el1,false),false);
    end;

  FUNCTION evaluateNullary(CONST subruleLiteral:P_expressionLiteral):P_literal;
    begin
      t:=context.wallclockTime;
      result:=P_subrule(subruleLiteral^.value)^.directEvaluateNullary(tokenLocation,context,0);
      t:=context.wallclockTime-t;
    end;

  FUNCTION evaluate(CONST subruleLiteral:P_expressionLiteral; CONST parameters:P_listLiteral):P_literal;
    VAR first:P_token;
    begin
      first:=context.newToken(tokenLocation,'',tt_literal,subruleLiteral); subruleLiteral^.rereference;
      first^.next:=context.newToken(tokenLocation,'',tt_parList,parameters); parameters^.rereference;
      t:=context.wallclockTime;
      reduceExpression(first,0,context);
      t:=context.wallclockTime-t;
      if (first<>nil) and (first^.next=nil) and (first^.tokType=tt_literal)
      then begin result:=first^.data; result^.rereference; end
      else result:=nil;
      if first<>nil then context.cascadeDisposeToken(first);
    end;

  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then exit(newRealLiteral(context.wallclockTime))
    else if (params^.size>=1) and (arg0^.literalType=lt_expression) and
      ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_validListTypes)) then begin
      context.callStackPush(tokenLocation,@builtinLocation_time,params,nil);
      if params^.size=2 then res:=evaluate(P_expressionLiteral(arg0),list1)
                        else res:=evaluateNullary(P_expressionLiteral(arg0));
      context.callStackPop();
      if res<>nil then begin
        result:=newListLiteral;
        appendPair(result,'expression',newStringLiteral(arg0^.toString));
        if res^.literalType<>lt_void then appendPair(result,'result',res)
                                     else disposeLiteral(res);
        appendPair(result,'time'  ,newRealLiteral(t));
      end;
    end;
  end;

FUNCTION try_imp intFuncSignature;
  VAR oldAdapters:P_adapters;
      errorCase:boolean;
      messages:P_literal;
  begin
    result:=nil;
    if (params^.size>=1) and (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(0)) and
      ((params^.size=1) or (params^.size=2)) then begin
      context.callStackPush(tokenLocation,@builtinLocation_try,params,nil);
      oldAdapters:=context.enterTryStatementReturningPreviousAdapters;
      result:=P_subrule(P_expressionLiteral(arg0)^.value)^.directEvaluateNullary(tokenLocation,context,0);
      if context.adapters^.noErrors
      then errorCase:=false
      else begin
        if result<>nil then disposeLiteral(result);
        messages:=messagesToLiteralForSandbox(P_collectingOutAdapter(context.adapters^.getAdapter(0))^.storedMessages);
        errorCase:=true;
      end;
      context.leaveTryStatementReassumingPreviousAdapters(oldAdapters,errorCase);
      if errorCase then begin
        if params^.size=2 then begin
          if arg1^.literalType=lt_expression then begin
            if P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)
            then result:=P_subrule(P_expressionLiteral(arg1)^.value)^.directEvaluateUnary(messages,tokenLocation,context,0)
            else result:=P_subrule(P_expressionLiteral(arg1)^.value)^.directEvaluateNullary       (tokenLocation,context,0);
          end else begin
            result:=arg1;
            result^.rereference;
          end;
        end else result:=newVoidLiteral;
        disposeLiteral(messages);
      end;
      context.callStackPop();
    end;
  end;

TYPE P_asyncTask=^T_asyncTask;
     T_asyncTask=record
       task:P_token;
       context:P_evaluationContext;
     end;

FUNCTION doAsync(p:pointer):ptrint;
  begin
    result:=0;
    with P_asyncTask(p)^ do begin
      context^.resetForEvaluation(nil);
      reduceExpression(task,0,context^);
      context^.cascadeDisposeToken(task);
      context^.afterEvaluation;
      dispose(context,destroy);
    end;
    freeMem(p,sizeOf(T_asyncTask));
  end;

FUNCTION async_imp intFuncSignature;
  VAR p:P_asyncTask;
      childContext:P_evaluationContext;
  begin
    result:=nil;
    if (params^.size>=1) and (arg0^.literalType=lt_expression) and
       ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_validListTypes)) then begin
      childContext:=context.getNewAsyncContext;
      if childContext<>nil then begin
        getMem(p,sizeOf(T_asyncTask));
        p^.context:=childContext;
        p^.task:=context.newToken(tokenLocation,'',tt_literal,arg0);
        arg0^.rereference;
        if params^.size=2 then begin
          p^.task^.next:=context.newToken(tokenLocation,'',tt_parList,arg1);
          arg1^.rereference;
        end else
          p^.task^.next:=context.newToken(tokenLocation,'',tt_parList,newListLiteral);
        beginThread(@doAsync,p);
        result:=newVoidLiteral;
      end else begin
        context.adapters^.raiseError('Creation of asynchronous tasks is forbidden for the current context',tokenLocation);
      end;
    end;
  end;

FUNCTION stringToTokens(CONST s:ansistring; CONST location:T_tokenLocation; CONST package:P_package; VAR context:T_evaluationContext):P_token;
  VAR exTokens:T_tokenArray;
      dummyForLastComment:ansistring='';
      last:P_token;
  begin
    exTokens.create;
    exTokens.tokenizeAll(s,location,package,context.adapters^,false);
    exTokens.step(package,dummyForLastComment,context.adapters^);
    if exTokens.atEnd then begin
      context.adapters^.raiseError('The parsed expression appears to be empty',location);
      exTokens.destroy;
      exit(nil);
    end else if not(context.adapters^.noErrors) then begin
      exTokens.destroy;
      exit(nil); //Parsing error ocurred
    end;
    result:=context.newToken(exTokens.current); exTokens.current.undefine;
    last:=result;
    exTokens.step(package,dummyForLastComment,context.adapters^);
    while not(exTokens.atEnd) do begin
      last^.next:=context.newToken(exTokens.current);  exTokens.current.undefine;
      last:=last^.next;
      exTokens.step(package,dummyForLastComment,context.adapters^);
    end;
    exTokens.destroy;
  end;

FUNCTION listToTokens(CONST l:P_listLiteral; CONST location:T_tokenLocation; CONST package:P_package; VAR context:T_evaluationContext):P_token;
  VAR last:P_token=nil;
      i:longint;
      subTokens:P_token;
  begin
    result:=nil;
    for i:=0 to L^.size-1 do begin
      if L^.value(i)^.literalType=lt_string
      then subTokens:=stringToTokens(P_stringLiteral(L^.value(i))^.value,location,package,context)
      else begin
        subTokens:=context.newToken(location,'',tt_literal,L^.value(i));
        L^.value(i)^.rereference;
      end;
      if subTokens=nil then begin
        if result<>nil then context.cascadeDisposeToken(result);
        exit(nil);
      end;
      if result=nil then result:=subTokens
                    else last^.next:=subTokens;
      last:=subTokens^.last;
    end;
  end;

FUNCTION stringOrListToExpression(CONST L:P_literal; CONST location:T_tokenLocation; VAR context:T_evaluationContext):P_scalarLiteral;
  VAR first:P_token=nil;
      temp:P_token;
      package:P_package;
  begin
    result:=nil;
    package:=P_package(location.package);
    if      L^.literalType=lt_string           then first:=stringToTokens(P_stringLiteral(L)^.value,location,package,context)
    else if L^.literalType in C_validListTypes then first:=listToTokens  (P_listLiteral  (L)       ,location,package,context);
    if first=nil then exit(nil);

    if not(first^.areBracketsPlausible(context.adapters^)) then begin
      context.cascadeDisposeToken(first);
      exit(nil);
    end;
    if first^.tokType<>tt_expBraceOpen then begin
      temp:=context.newToken(location,'',tt_expBraceOpen);
      temp^.next:=first; first:=temp;
      temp:=first^.last;
      temp^.next:=context.newToken(location,'',tt_expBraceClose);
    end;

    digestInlineExpression(first,context);
    if (context.adapters^.noErrors) and (first^.next<>nil) then context.adapters^.raiseCustomMessage(mt_el4_parsingError,'The parsed expression goes beyond the expected limit... I know this is a fuzzy error. Sorry.',location);
    if not(context.adapters^.noErrors) then begin
      context.cascadeDisposeToken(first);
      exit(newErrorLiteral);
    end;
    if (first^.tokType<>tt_literal) or (P_literal(first^.data)^.literalType<>lt_expression) then begin
      context.disposeToken(first);
      context.adapters^.raiseCustomMessage(mt_el5_systemError,'This is unexpected. The result of mnh_tokens.stringToExpression should be an expression!',location);
      exit(newErrorLiteral);
    end;
    result:=P_expressionLiteral(first^.data);
    first^.tokType:=tt_EOL;
    first^.data:=nil;
    context.disposeToken(first);
  end;

FUNCTION toExpression_imp intFuncSignature;
  FUNCTION primitiveExpression(CONST l:P_literal):P_expressionLiteral;
    VAR first:P_token;
    begin
      //Create token-series { <Literal> }
      first            :=context.newToken(tokenLocation,'',tt_expBraceOpen);
      first^.next      :=context.newToken(tokenLocation,'',tt_literal,l); L^.rereference;
      first^.next^.next:=context.newToken(tokenLocation,'',tt_expBraceClose);
      //Reduce to inline expression
      digestInlineExpression(first,context);
      result:=P_expressionLiteral(first^.data);
      first^.tokType:=tt_EOL;
      first^.data:=nil;
      context.disposeToken(first);
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

FUNCTION arity_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_expression)
    then result:=newIntLiteral(P_subrule (P_expressionLiteral(arg0)^.value)^.arity);
  end;

FUNCTION parameterNames_imp intFuncSignature;
  FUNCTION getNames(CONST subRule:P_subrule):P_listLiteral;
    VAR i:longint;
    begin
      result:=newListLiteral;
      for i:=0 to length(subRule^.pattern.sig)-1 do result^.appendString(subRule^.pattern.sig[i].id);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_expression)
    then result:=getNames(P_subrule(P_expressionLiteral(arg0)^.value));
  end;

FUNCTION sandBoxExecute_imp intFuncSignature;
  VAR src:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string,lt_stringList]) then begin
      if arg0^.literalType=lt_string
      then src:=P_stringLiteral(arg0)^.value
      else begin
        setLength(src,0);
        for i:=0 to P_listLiteral(arg0)^.size-1 do append(src,P_stringLiteral(P_listLiteral(arg0)^.value(i))^.value);
      end;
      result:=messagesToLiteralForSandbox(runAlone(src));
    end;
  end;

FUNCTION group_imp intFuncSignature;
  TYPE T_groupMap=specialize G_literalKeyMap<P_literal>;
  VAR listToGroup:P_listLiteral;
      keyList:array of P_literal;
      aggregator:P_expressionLiteral;
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
        P_listLiteral(resultLiteral)^.append(L,true);
      end else begin
        if resultLiteral=nil then begin
          resultLiteral:=L; L^.rereference;
        end else begin
          newLit:=P_subrule(aggregator^.value)^.directEvaluateAggregator(resultLiteral,L,tokenLocation,context,0);
          if newLit<>nil then begin
            disposeLiteral(resultLiteral);
            resultLiteral:=newLit;
          end else begin
            context.adapters^.raiseError('Error performing aggregation in group-construct with aggregator '+aggregator^.toString,tokenLocation);
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
       (arg0^.literalType in C_validListTypes) and
      ((arg1^.literalType in C_validListTypes) and (list1^.size=P_listLiteral(arg0)^.size) or (arg1^.literalType=lt_int)) and
      ((params^.size=2) or (params^.value(2)^.literalType=lt_expression))
    then begin
      setLength(keyList,0);
      listToGroup:=P_listLiteral(arg0);

      if arg1^.literalType=lt_int
      then makeKeysByIndex(P_intLiteral(arg1)^.value)
      else makeKeysByList(list1);

      if (params^.size=3) then aggregator:=P_expressionLiteral(params^.value(2))
                          else aggregator:=nil;
      if aggregator<>nil then context.callStackPush(tokenLocation,@builtinLocation_group,params,nil);
      groupMap.create();
      for inputIndex:=0 to length(keyList)-1 do if context.adapters^.noErrors then
        addToAggregation(keyList[inputIndex],listToGroup^.value(inputIndex));
      groupList:=groupMap.keyValueList;
      result:=newListLiteral;
      for outputIndex:=0 to length(groupList)-1 do P_listLiteral(result)^.append(groupList[outputIndex].value,false);
      groupMap.destroy;
      if aggregator<>nil then context.callStackPop();
    end;
  end;

FUNCTION format_imp intFuncSignature;
  VAR txt:T_arrayOfString;
      i:longint;
      preparedStatement:P_preparedFormatStatement;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      context.callStackPush(tokenLocation,@builtinLocation_format,params,nil);
      preparedStatement:=getFormat(P_stringLiteral(arg0)^.value,tokenLocation,context);
      context.callStackPop();
      if not(context.adapters^.noErrors) then exit(nil);
      txt:=preparedStatement^.format(params,tokenLocation,context);
      if length(txt)=1 then result:=newStringLiteral(txt[0])
      else begin
        result:=newListLiteral;
        for i:=0 to length(txt)-1 do P_listLiteral(result)^.appendString(txt[i]);
      end;
    end;
  end;

FUNCTION printf_imp intFuncSignature;
  VAR preparedStatement:P_preparedFormatStatement;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      context.callStackPush(tokenLocation,@builtinLocation_printf,params,nil);
      preparedStatement:=getFormat(P_stringLiteral(arg0)^.value,tokenLocation,context);
      if not(context.adapters^.noErrors) then begin
        context.callStackPop();
        exit(nil);
      end;
      system.enterCriticalSection(print_cs);
      context.adapters^.printOut(formatTabs(reSplit(preparedStatement^.format(params,tokenLocation,context))));
      system.leaveCriticalSection(print_cs);
      result:=newVoidLiteral;
      context.callStackPop();
    end;
  end;

FUNCTION formatTime_imp intFuncSignature;
  VAR L:P_listLiteral;
      i:longint;
      fmt:ansistring='';
  FUNCTION fmtIt(CONST t:double):P_stringLiteral; inline;
    begin
      result:=newStringLiteral(FormatDateTime(fmt,t));
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) then begin
      fmt:=P_stringLiteral(arg0)^.value;
      case arg1^.literalType of
        lt_int:  result:=fmtIt(P_intLiteral (arg1)^.value);
        lt_real: result:=fmtIt(P_realLiteral(arg1)^.value);
        lt_emptyList: result:=newListLiteral;
        lt_realList,lt_intList,lt_numList: begin
          result:=newListLiteral;
          L:=list1;
          for i:=0 to L^.size-1 do case(L^.value(i)^.literalType) of
            lt_int : P_listLiteral(result)^.append(fmtIt(P_intLiteral (L^.value(i))^.value),false);
            lt_real: P_listLiteral(result)^.append(fmtIt(P_realLiteral(L^.value(i))^.value),false);
          end;
        end;
      end;
    end;
  end;

FUNCTION parseTime_imp intFuncSignature;
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
        context.adapters^.raiseError('parseTime expects two strings of equal length as parameters',tokenLocation);
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
        on E:Exception do context.adapters^.raiseError('parseTime failed:'+E.message,tokenLocation);
      end;
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) then begin
      format:= uppercase(P_stringLiteral(arg0)^.value);
      if (arg1^.literalType=lt_string) then
        result:=newRealLiteral(encodeDateTime(P_stringLiteral(arg1)^.value))
      else if (arg1^.literalType in [lt_stringList,lt_emptyList]) then begin
        result:=newListLiteral;
        for i:=0 to list1^.size-1 do
          P_listLiteral(result)^.appendReal(encodeDateTime(P_stringLiteral(list1^.value(i))^.value));
      end;
    end;
  end;

FUNCTION inspect_imp intFuncSignature;
  VAR i:longint;
      id:string;
      tempPackage:T_package;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=P_package(tokenLocation.package)^.inspect
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      id:=P_stringLiteral(arg0)^.value;

      with P_package(tokenLocation.package)^ do
        for i:=0 to length(packageUses)-1 do
        if (packageUses[i].id=id) or (packageUses[i].path=id)
        then exit(packageUses[i].pack^.inspect);

      if not(fileExists(id)) then id:=locateSource(extractFilePath(tokenLocation.package^.getPath),id);

      if (id<>'') and fileExists(id) then begin
        tempPackage.create(nil);
        tempPackage.setSourcePath(id);
        tempPackage.loadForDocumentation;
        result:=tempPackage.inspect;
        tempPackage.destroy;
        exit(result);
      end;

      result:=newListLiteral;
      context.adapters^.raiseNote('Cannot locate package with id or path "'+id+'"',tokenLocation);
    end;
  end;

FUNCTION clearRuleCache_imp intFuncSignature;
  VAR ruleIdToken:T_token;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      ruleIdToken.create;
      ruleIdToken.define(tokenLocation,P_stringLiteral(arg0)^.value,tt_identifier);
      P_package(tokenLocation.package)^.resolveRuleId(ruleIdToken,context.adapters);
      if ruleIdToken.tokType in [tt_customTypeCheck,tt_customTypeRule,tt_localUserRule,tt_importedUserRule] then P_rule(ruleIdToken.data)^.clearCache;
      ruleIdToken.destroy;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION clearPackageCache_imp intFuncSignature;
  VAR i:longint;
      id:string;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.clearPackageCache(false);
      result:=newVoidLiteral;
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      id:=P_stringLiteral(arg0)^.value;
      with P_package(tokenLocation.package)^ do
        for i:=0 to length(packageUses)-1 do begin
          if (packageUses[i].id=id) or (packageUses[i].path=id) then begin
            packageUses[i].pack^.clearPackageCache(false);
            if result=nil then result:=newVoidLiteral;
          end;
        end;
      if result=nil then context.adapters^.raiseError('Cannot locate package with id or path "'+id+'"',tokenLocation);
    end;
  end;

FUNCTION clearAllCaches_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.clearPackageCache(true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

{$endif}
{$ifdef include_initialization}
  //Unary Numeric -> real
  registerRule(MATH_NAMESPACE,'sqrt',@sqrt_imp,'sqrt(n);//Returns the square root of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'sin',@sin_imp,'sin(n);//Returns the sine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arcsin',@arcsin_imp,'arcsin(n);//Returns the arcsine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'cos',@cos_imp,'cos(n);//Returns the cosine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arccos',@arccos_imp,'arccos(n);//Returns the arccosine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'tan',@tan_imp,'tan(n);//Returns the tangent of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arctan',@arctan_imp,'arctan(n);//Returns the arctangent of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'exp',@exp_imp,'exp(n);//Returns the exponential of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'ln',@ln_imp,'ln(n);//Returns the natural logarithm of numeric or expression parameter n');
  //Unary Boolean -> boolean
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'not',@not_imp,'not(b:boolean);#not(b:booleanList);//Returns the negated value of b#not(i:int);#not(i:intList);//Returns the bitwise negated value of i');
  //Unary Numeric -> same (i.e. I -> I, R -> R)
  registerRule(MATH_NAMESPACE,'abs',@abs_imp,'abs(n);//Returns the absolute value of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'sqr',@sqr_imp,'sqr(n);//Returns the square of numeric or expression parameter n');
  //Unary Numeric -> Integer
  registerRule(MATH_NAMESPACE,'sign',@sign_imp,'sign(n);//Returns the sign of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'ceil',@ceil_imp,'ceil(x);//Returns the smallest integer >=x#ceil(x,k);//Does the same but with k digits precision');
  registerRule(MATH_NAMESPACE,'floor',@floor_imp,'floor(x);//Returns the largest integer <=x#floor(x,k);//Does the same but with k digits precision');
  //round might be binary...
  registerRule(MATH_NAMESPACE,'round',@round_imp,'round(x);//Returns the value of x, rounded to the nearest integer#round(x,k);//Returns the value of x rounded to k-digits precision');
  builtinLocation_time.create(SYSTEM_BUILTIN_NAMESPACE,'time');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'time',@time_imp,'time;//Returns an internal time for time difference measurement.#'+
                                                         'time(E:expression);//Evaluates E (without parameters) and returns a nested List with evaluation details.#'+
                                                         'time(E:expression,par:list);//Evaluates E@par and returns a nested List with evaluation details.');
  builtinLocation_try.create(SYSTEM_BUILTIN_NAMESPACE,'try');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'try',@try_imp,'try(E:expression(0));//Evaluates E and returns the result if successful or void if failed.#'+
                                                       'try(E:expression(0),except(1):expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed with the errors as first paramter ($0).#'+
                                                       'try(E:expression(0),except:expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed without paramters.#'+
                                                       'try(E:expression(0),except);//Evaluates E and returns the result if successful. Otherwise <except> (any type except expression) is returned.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'async',@async_imp,'async(E:expression);//Calls E asynchronously (without parameters) and returns void.#'+
                                                           'async(E:expression,par:list);//Calls E@par and asynchronously and returns void.#//Asynchronous tasks are killed at the end of (synchonous) evaluation.');
  registerRule(TYPECAST_NAMESPACE,'toExpression',@toExpression_imp,'toExpression(S);//Returns an expression parsed from S');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'arity',@arity_imp,'arity(e:expression);//Returns the arity of expression e');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'parameterNames',@parameterNames_imp,'parameterNames(e:expression);//Returns the IDs of named parameters of e');
  builtinLocation_group.create(DEFAULT_BUILTIN_NAMESPACE,'group');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'group',@group_imp,'group(list,grouping);//Re-groups list by grouping (which is a sub-index or a list)#group(list,grouping,aggregator:expression);//Groups by grouping using aggregator on a per group basis');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sandBoxExecute',@sandBoxExecute_imp,'sandBoxExecute(source);//Executes the source (string or stringList) and returns created messages');
  builtinLocation_printf.create(SYSTEM_BUILTIN_NAMESPACE,'printf');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printf',@printf_imp,'printf(formatString:string,...);//Prints a formatted version of the given 0..n parameters and returns void, see <a href="formatStrings.html">Format Strings</a>');
  builtinLocation_format.create(STRINGS_NAMESPACE,'format');
  registerRule(STRINGS_NAMESPACE,'format',@format_imp,'format(formatString:string,...);//Returns a formatted version of the given 0..n parameters, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(STRINGS_NAMESPACE,'formatTime',@formatTime_imp,'formatTime(formatString:string,t);//Returns time t (numeric list or scalar) formatted using format string, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(STRINGS_NAMESPACE,'parseTime',@parseTime_imp,'parseTime(formatString:string,input:string);//Parses time from a given date format and input, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'inspect',@inspect_imp,'inspect;//Returs information on the current package#inspect(idOrPath:string);//Returns information on an imported package or a specified MNH source file');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearRuleCache',@clearRuleCache_imp,'clearRuleCache(id:string);//Clears memoized rule caches for a specified rule and returns void.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearPackageCache',@clearPackageCache_imp,'clearPackageCache(id:string);//Clears memoized rule caches for a used package specified by id or path and returns void.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearAllCaches',@clearAllCaches_imp,'clearAllCaches;//Clears all memoized rule caches and returns void.');

{$endif}
{$ifdef include_finalization}
  builtinLocation_group .destroy;
  builtinLocation_time  .destroy;
  builtinLocation_try   .destroy;
  builtinLocation_printf.destroy;
  builtinLocation_format.destroy;
{$endif}
