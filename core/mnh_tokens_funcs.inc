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
        for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false);
      end;
      else raiseNotApplicableError(ID_MACRO,x^.literalType,'',tokenLocation);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1)
  then result:=recurse(params^.value(0))
  else raiseNotApplicableError(ID_MACRO,params,tokenLocation);
end}

FUNCTION sqrt_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=sqrt}
{$define ID_MACRO:='sqrt'}
UNARY_NUM_TO_REAL;

FUNCTION sin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=sin}
{$define ID_MACRO:='sin'}
UNARY_NUM_TO_REAL;

FUNCTION arcsin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=arcsin}
{$define ID_MACRO:='arcsin'}
UNARY_NUM_TO_REAL;

FUNCTION cos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=cos}
{$define ID_MACRO:='cos'}
UNARY_NUM_TO_REAL;

FUNCTION arccos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=arccos}
{$define ID_MACRO:='arccos'}
UNARY_NUM_TO_REAL;

FUNCTION tan_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=tan}
{$define ID_MACRO:='tan'}
UNARY_NUM_TO_REAL;

FUNCTION arctan_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=arctan}
{$define ID_MACRO:='arctan'}
UNARY_NUM_TO_REAL;

FUNCTION exp_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=exp}
{$define ID_MACRO:='exp'}
UNARY_NUM_TO_REAL;

FUNCTION ln_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=ln}
{$define ID_MACRO:='ln'}
UNARY_NUM_TO_REAL;

{$undef UNARY_NUM_TO_REAL}

FUNCTION not_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
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
        else raiseNotApplicableError('not',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=not_rec(params^.value(0))
    else raiseNotApplicableError('not',params,tokenLocation);
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
      else raiseNotApplicableError(ID_MACRO,x^.literalType,'',tokenLocation);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1)
  then result:=recurse(params^.value(0))
  else raiseNotApplicableError(ID_MACRO,params,tokenLocation);
end}

FUNCTION abs_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=abs}
{$define ID_MACRO:='abs'}
UNARY_NUM_TO_SAME;

FUNCTION sqr_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
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
        else raiseNotApplicableError(ID_MACRO,x^.literalType,'',tokenLocation);
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
          else raiseNotApplicableError(ID_MACRO,y^.literalType,' (second parameter)',tokenLocation);
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
          end else raiseError(el3_evalError,'Incompatible list lengths given for built in function '+ID_MACRO,tokenLocation);
          else raiseNotApplicableError(ID_MACRO,y^.literalType,' (second parameter)',tokenLocation);
        end;
        else raiseNotApplicableError(ID_MACRO,x^.literalType,' (first parameter)',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=recurse1(params^.value(0)) else
    if (params<>nil) and (params^.size=2) then result:=recurse2(params^.value(0),params^.value(1))
    else raiseNotApplicableError(ID_MACRO,params,tokenLocation);
  end}

FUNCTION round_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=round}
{$define ID_MACRO:='round'}
ROUND_IMPLEMENTATION;

FUNCTION ceil_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=ceil}
{$define ID_MACRO:='ceil'}
ROUND_IMPLEMENTATION;

FUNCTION floor_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=floor}
{$define ID_MACRO:='floor'}
ROUND_IMPLEMENTATION;
{$undef ROUND_IMPLEMENTATION}

FUNCTION sign_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
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
        else raiseNotApplicableError('sign',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=sign_rec(params^.value(0))
    else raiseNotApplicableError('sign',params,tokenLocation);
  end;


FUNCTION time_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR res:P_literal;
      profiler:TEpikTimer;
      t:double;

  PROCEDURE appendPair(VAR result:P_literal; CONST el0:string; CONST el1:P_literal);
    VAR aid:P_listLiteral;
    begin
      aid:=newListLiteral;
      aid^.append(newStringLiteral(el0),false);
      aid^.append(el1,false);
      P_listLiteral(result)^.append(aid,false);
    end;

  FUNCTION evaluateNullary(CONST subruleLiteral:P_expressionLiteral):P_literal;
    VAR tempRecycler:T_tokenRecycler;
    begin
      tempRecycler.create;
      result:=P_subrule(subruleLiteral^.value)^.directEvaluateNullary(subruleLiteral,0,tempRecycler);
      tempRecycler.destroy;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_expression) then begin
      profiler := TEpikTimer.create(nil);
      profiler.Clear;
      profiler.Start;
      res:=evaluateNullary(P_expressionLiteral(params^.value(0)));
      t:=profiler.Elapsed;
      if res<>nil then begin
        result:=newListLiteral;
        appendPair(result,'expression',newStringLiteral(params^.value(0)^.toString));
        appendPair(result,'result',res);
        appendPair(result,'time'  ,newRealLiteral(t));
      end;
      profiler.Free;
    end else raiseNotApplicableError('time',params,tokenLocation);
  end;

FUNCTION sleep_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  CONST secondsPerDay=24*60*60;
  VAR sleepUntil:double;
      sleepInt:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_real,lt_int]) then begin
      sleepUntil:=now;
      result:=newVoidLiteral;
      if params^.value(0)^.literalType=lt_int
      then sleepUntil:=sleepUntil+P_intLiteral (params^.value(0))^.value/secondsPerDay
      else sleepUntil:=sleepUntil+P_realLiteral(params^.value(0))^.value/secondsPerDay;
      while (now<sleepUntil) and (errorLevel<el3_evalError) do begin
        sleepInt:=round(900*(sleepUntil-now)*secondsPerDay);
        if sleepInt>100 then sleepInt:=100;
        if (sleepInt>0) then sleep(sleepInt);
      end;
    end else raiseNotApplicableError('sleep',params,tokenLocation);
  end;

FUNCTION expression_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION stringToExpression(s:ansistring; CONST location:T_tokenLocation):P_scalarLiteral;
    VAR next:T_token;
        first,last,temp:P_token;
        loc:T_tokenLocation;
        recycler:T_tokenRecycler;
    begin
      loc:=location;
      next:=firstToken(s,loc,nil,true);
      while (next.tokType=tt_eol) and (s<>'') and (errorLevel<el3_evalError) do next:=firstToken(s,loc,nil,true);
      if next.tokType=tt_eol then exit(newErrorLiteralRaising('The parsed expression appears to be empty',location));
      recycler.create;
      first:=recycler.newToken(next);
      last:=first;
      repeat
        loc:=location;
        next:=firstToken(s,loc,nil,true);
        while (next.tokType=tt_eol) and (s<>'') and (errorLevel<el3_evalError) do next:=firstToken(s,loc,nil,true);
        if next.tokType<>tt_eol then begin
          last^.next:=recycler.newToken(next);
          last:=last^.next;
        end;
      until (next.tokType in [tt_eol,tt_semicolon]) or (errorLevel>=el3_evalError);
      if errorLevel>=el3_evalError then begin
        recycler.cascadeDisposeToken(first);
        recycler.destroy;
        exit(newErrorLiteral);
      end;

      if first^.tokType<>tt_expBraceOpen then begin
        temp:=recycler.newToken(location,'',tt_expBraceOpen);
        temp^.next:=first; first:=temp;
        last^.next:=recycler.newToken(location,'',tt_expBraceClose);
        last:=last^.next;
      end;

      digestInlineExpression(first,recycler);
      if (errorLevel<el3_evalError) and (first^.next<>nil) then raiseError(el4_parsingError,'The parsed expression goes beyond the expected limit... I know this is a fuzzy error. Sorry.',location);
      if errorLevel>=el3_evalError then begin
        recycler.cascadeDisposeToken(first);
        recycler.destroy;
        exit(newErrorLiteral);
      end;
      if (first^.tokType<>tt_literal) or (P_literal(first^.data)^.literalType<>lt_expression) then begin
        recycler.disposeToken(first);
        raiseError(el5_systemError,'This is unexpected. The result of mnh_tokens.stringToExpression should be an expression!',location);
        recycler.destroy;
        exit(newErrorLiteral);
      end;
      result:=P_expressionLiteral(first^.data);
      first^.tokType:=tt_eol;
      first^.data:=nil;
      recycler.disposeToken(first);
      recycler.destroy;
    end;

  FUNCTION expression_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_string: result:=stringToExpression(P_stringLiteral(x)^.value,tokenLocation);
        lt_list,lt_stringList,lt_emptyList:  begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if errorLevel<el3_evalError then
            P_listLiteral(result)^.append(expression_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else result:=newErrorLiteralRaising('Cannot apply "expression" to literal of type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_emptyList,lt_string])
    then result:=expression_rec(params^.value(0))
    else raiseNotApplicableError('expression',params,tokenLocation);
  end;

FUNCTION arity_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION getArity(CONST subruleLiteral:P_expressionLiteral):longint;
    begin
      result:=length(P_subrule(subruleLiteral^.value)^.pattern.sig);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_expression) then begin
      result:=newIntLiteral(getArity(P_expressionLiteral(params^.value(0))));
    end else raiseNotApplicableError('arity',params,tokenLocation);
  end;

FUNCTION parameterNames_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION getNames(CONST subrule:P_subrule):P_listLiteral;
    VAR i:longint;
    begin
      result:=newListLiteral;
      for i:=0 to length(subrule^.pattern.sig)-1 do result^.append(newStringLiteral(subrule^.pattern.sig[i].id),false);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_expression) then begin
      result:=getNames(P_subrule(P_expressionLiteral(params^.value(0))^.value));
    end else raiseNotApplicableError('parameterNames',params,tokenLocation);
  end;

FUNCTION mainParams_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=parametersForMain;
      if result<>nil
      then result^.rereference
      else result:=newListLiteral;
    end else raiseNotApplicableError('mainParams',params,tokenLocation);
  end;

{$endif}
{$ifdef include_initialization}
  //Unary Numeric -> real
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sqrt'          ,@sqrt_imp      ,'sqrt(n);#Returns the square root of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sin'           ,@sin_imp       ,'sin(n);#Returns the sine of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'arcsin'        ,@arcsin_imp    ,'arcsin(n);#Returns the arcsine of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'cos'           ,@cos_imp       ,'cos(n);#Returns the cosine of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'arccos'        ,@arccos_imp    ,'arccos(n);#Returns the arccosine of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'tan'           ,@tan_imp       ,'tan(n);#Returns the tangent of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'arctan'        ,@arctan_imp    ,'tan(n);#Returns the arctangent of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'exp'           ,@exp_imp       ,'exp(n);#Returns the exponential of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ln'            ,@ln_imp        ,'ln(n);#Returns the natural logarithm of numeric parameter n');
  //Unary Boolean -> boolean
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'not'           ,@not_imp       ,'not(b:boolean);#not(b:booleanList);#Returns the negated value of b#not(i:int);#not(i:intList);#Returns the bitwise negated value of i');
  //Unary Numeric -> same (i.e. I -> I, R -> R)
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'abs'           ,@abs_imp       ,'abs(n);#Returns the absolute value of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sqr'           ,@sqr_imp       ,'sqr(n);#Returns the square of numeric parameter n');
  //Unary Numeric -> Integer
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sign'          ,@sign_imp      ,'sign(n);#Returns the sign of numeric parameter n');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ceil'          ,@ceil_imp      ,'ceil(x);#Returns the smallest integer >=x');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'floor'         ,@floor_imp     ,'floor(x);#Returns the largest integer <=x');
  //round might be binary...
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'round'         ,@round_imp     ,'round(x);#Returns the value of x, rounded to the nearest integer#round(x,k);#Returns the value of x rounded to k-digits precision');

  registerRule(DEFAULT_BUILTIN_NAMESPACE,'time'          ,@time_imp      ,'time(E:expression);#Evaluates E (without parameters) and returns a nested List with evaluation details.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleep'         ,@sleep_imp     ,'sleep(seconds:number);#Sleeps for the given number of seconds before returning void');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'expression'    ,@expression_imp,'expression(S:string);#Returns an expression parsed from S');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'arity'         ,@arity_imp     ,'arity(e:expression);#Returns the arity of expression e');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'parameterNames',@parameterNames_imp,'parameterNames(e:expression);#Returns the IDs of named parameters of e');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'mainParams'    ,@mainParams_imp,'mainParams;#Returns the parameters passed to the program');
{$endif}
