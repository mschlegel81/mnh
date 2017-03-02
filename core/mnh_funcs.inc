{$ifdef include_implementation}
VAR builtinLocation_group,
    builtinLocation_time,
    builtinLocation_try,
    builtinLocation_printf,
    builtinLocation_format:T_identifiedInternalFunction;

{$i mnh_func_defines.inc}

{$define UNARY_NUM_TO_REAL:=
FUNCTION recurse(CONST x:P_literal):P_literal;
  VAR iter:T_arrayOfLiteral;
      y:P_literal;
  begin
    result:=nil;
    case x^.literalType of
      lt_expression: new(P_subrule(result),createFromInlineWithOp(P_expressionLiteral(x),ID_MACRO,tokenLocation));
      lt_int : try result:=newRealLiteral(CALL_MACRO(P_intLiteral (x)^.value)); except result:=newRealLiteral(Nan) end;
      lt_real: try result:=newRealLiteral(CALL_MACRO(P_realLiteral(x)^.value)); except result:=newRealLiteral(Nan) end;
      lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
      lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: begin
        result:=P_collectionLiteral(x)^.newOfSameType;
        iter  :=P_collectionLiteral(x)^.iteratableList;
        for y in iter do collResult^.append(recurse(y),false);
        disposeLiteral(iter);
        if collResult^.containsError then begin
          raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
          disposeLiteral(result);
        end;
      end;
      else raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
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
    VAR y:P_literal;
        iter:T_arrayOfLiteral;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: new(P_subrule(result),createFromInlineWithOp(P_expressionLiteral(x),'not',tokenLocation));
        lt_boolean: result:=newBoolLiteral(not(P_boolLiteral(x)^.value));
        lt_int:     result:=newIntLiteral (not(P_intLiteral (x)^.value));
        lt_list,lt_booleanList,lt_intList,lt_emptyList,
        lt_set ,lt_booleanSet ,lt_intSet ,lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType;
          iter:=P_collectionLiteral(x)^.iteratableList;
          for y in iter do P_collectionLiteral(result)^.append(not_rec(y),false);
          disposeLiteral(iter);
          if collResult^.containsError then begin
            raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
            disposeLiteral(result);
          end;
        end;
        else raiseNotApplicableError('not',x,tokenLocation,context.adapters^);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=not_rec(arg0);
  end;

{$define UNARY_NUM_TO_SAME:=
FUNCTION recurse(CONST x:P_literal):P_literal;
  VAR sub:P_literal;
      iter:T_arrayOfLiteral;
  begin
    result:=nil;
    case x^.literalType of
      lt_expression: new(P_subrule(result),createFromInlineWithOp(P_expressionLiteral(x),ID_MACRO,tokenLocation));
      lt_error: begin result:=x; result^.rereference; end;
      lt_int : result:=newIntLiteral (CALL_MACRO(P_intLiteral (x)^.value));
      lt_real: result:=newRealLiteral(CALL_MACRO(P_realLiteral(x)^.value));
      lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
      lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: begin
        result:=P_collectionLiteral(x)^.newOfSameType;
        iter  :=P_collectionLiteral(x)^.iteratableList;
        for sub in iter do collResult^.append(recurse(sub),false);
        disposeLiteral(iter);
        if collResult^.containsError then begin
          raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
          disposeLiteral(result);
        end;
      end;
      else raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
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

{$define ROUND_IMPLEMENTATION:=FUNCTION recurse1(CONST x:P_literal):P_literal;
    VAR sub:P_literal;
        iter:T_arrayOfLiteral;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: new(P_subrule(result),createFromInlineWithOp(P_expressionLiteral(x),ID_MACRO,tokenLocation));
        lt_error,lt_int: result:=x^.rereferenced;
        lt_real: result:=newIntLiteral(CALL_MACRO(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType;
          iter  :=P_collectionLiteral(x)^.iteratableList;
          for sub in iter do collResult^.append(recurse1(sub),false);
          disposeLiteral(iter);
          if collResult^.containsError then begin
            raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
            disposeLiteral(result);
          end;
        end;
        else raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
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

    FUNCTION myRound(CONST x:P_intLiteral; CONST y:int64):P_literal; inline;
      VAR pot,i:int64;
      begin
        if y>=0 then exit(x^.rereferenced);
        pot:=1;
        i:=0;
        while i>y do begin pot:=pot*10; dec(i); end;
        result:=newIntLiteral(CALL_MACRO(x^.value div pot) * pot);
      end;

    VAR sub,ySub:P_literal;
        yIter:T_arrayOfLiteral;
        xIter:T_arrayOfLiteral;
        i    :longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error: result:=x^.rereferenced;
        lt_int : case y^.literalType of
          lt_error: result:=y^.rereferenced;
          lt_int: result:=myRound(P_intLiteral(x),P_intLiteral(y)^.value);
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: begin
            result:=P_collectionLiteral(y)^.newOfSameType;
            yIter :=P_collectionLiteral(y)^.iteratableList;
            for sub in yIter do collResult^.append(recurse2(x,sub),false);
            disposeLiteral(yIter);
            if collResult^.containsError then begin
              disposeLiteral(result);
              raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
            end;
          end;
          else raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
        end;
        lt_real: case y^.literalType of
          lt_error: result:=y^.rereferenced;
          lt_int: result:=myRound(P_realLiteral(x)^.value,P_intLiteral(y)^.value);
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: begin
            result:=P_collectionLiteral(y)^.newOfSameType;
            yIter :=P_collectionLiteral(y)^.iteratableList;
            for sub in yIter do collResult^.append(recurse2(x,sub),false);
            disposeLiteral(yIter);
            if collResult^.containsError then begin
              disposeLiteral(result);
              raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
            end;
          end;
          else raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
        end;
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: case y^.literalType of
          lt_error: result:=y^.rereferenced;
          lt_int: begin
            result:=newListLiteral;
            xIter:=P_collectionLiteral(x)^.iteratableList;
            for sub in xIter do P_listLiteral(result)^.append(recurse2(sub,y),false);
            disposeLiteral(xIter);
            if collResult^.containsError then begin
              disposeLiteral(result);
              raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
            end;
          end;
          lt_list,lt_intList,lt_emptyList: if P_listLiteral(x)^.size=P_listLiteral(y)^.size then begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(x)^.size-1 do listResult^.append(recurse2(P_listLiteral(x)^[i],P_listLiteral(y)^[i]),false);
          end else context.adapters^.raiseError('Incompatible list lengths given for built in function '+ID_MACRO,tokenLocation);
          else raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
        end;
        lt_set,lt_intSet,lt_realSet,lt_numSet,lt_emptySet: case y^.literalType of
          lt_error: result:=y^.rereferenced;
          lt_int: begin
            result:=newSetLiteral;
            xIter:=P_collectionLiteral(x)^.iteratableList;
            for sub in xIter do P_setLiteral(result)^.append(recurse2(sub,y),false);
            disposeLiteral(xIter);
            if collResult^.containsError then begin
              disposeLiteral(result);
              raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
            end;
          end;
          lt_set,lt_intSet,lt_emptySet: begin
            result:=newSetLiteral;
            xIter:=P_collectionLiteral(x)^.iteratableList;
            yIter:=P_collectionLiteral(y)^.iteratableList;
            for sub in xIter do for ySub in yIter do setResult^.append(recurse2(sub,ySub),false);
            if collResult^.containsError then begin
              disposeLiteral(result);
              raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
            end;
          end;
          else raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
        end;
        else raiseNotApplicableError(ID_MACRO,x,y,tokenLocation,context.adapters^);
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
{$undef CALL_MACRO}
{$undef ID_MACRO}

FUNCTION sign_imp intFuncSignature;
  FUNCTION sign_rec(CONST x:P_literal):P_literal;
    VAR iter:T_arrayOfLiteral;
        sub:P_literal;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: new(P_subrule(result),createFromInlineWithOp(P_expressionLiteral(x),'sign',tokenLocation));
        lt_error: result:=x^.rereferenced;
        lt_int : result:=newIntLiteral(sign(P_intLiteral (x)^.value));
        lt_real: result:=newIntLiteral(sign(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType;
          iter:=P_collectionLiteral(x)^.iteratableList;
          for sub in iter do collResult^.append(sign_rec(sub),false);
          disposeLiteral(iter);
          if collResult^.containsError then begin
            disposeLiteral(result);
            raiseNotApplicableError('sign',x,tokenLocation,context.adapters^);
          end;
        end;
        else raiseNotApplicableError('sign',x,tokenLocation,context.adapters^);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=sign_rec(arg0);
  end;

FUNCTION time_imp intFuncSignature;
  VAR res:P_literal;
      t:double=0;

  PROCEDURE appendPair(VAR result:P_literal; CONST el0:string; CONST el1:P_literal);
    begin
      P_listLiteral(result)^.append(newListLiteral^.appendString(el0)^.append(el1,false),false);
    end;

  FUNCTION evaluate(CONST subruleLiteral:P_subrule; CONST parameters:P_listLiteral=nil):P_literal;
    begin
      t:=context.wallclockTime(true);
      result:=subruleLiteral^.evaluate(tokenLocation,@context,parameters);
      t:=context.wallclockTime(true)-t;
    end;

  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then exit(newRealLiteral(context.wallclockTime(true)))
    else if (params^.size>=1) and (arg0^.literalType=lt_expression) and
      ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) then begin
      context.callStackPush(tokenLocation,@builtinLocation_time,params,nil);
      if params^.size=2 then res:=evaluate(P_subrule(arg0),list1)
                        else res:=evaluate(P_subrule(arg0));
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
      messages:P_literal=nil;
  begin
    result:=nil;
    if (params^.size>=1) and (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(0)) and
      ((params^.size=1) or (params^.size=2)) then begin
      context.callStackPush(tokenLocation,@builtinLocation_try,params,nil);
      oldAdapters:=context.enterTryStatementReturningPreviousAdapters;
      result:=P_subrule(arg0)^.evaluateToLiteral(tokenLocation,@context);
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
            then result:=P_subrule(arg1)^.evaluateToLiteral(tokenLocation,@context,messages)
            else result:=P_subrule(arg1)^.evaluateToLiteral(tokenLocation,@context);
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
       context:P_threadContext;
     end;

FUNCTION doAsync(p:pointer):ptrint;
  begin
    result:=0;
    with P_asyncTask(p)^ do begin
      reduceExpression(task,context^);
      context^.recycler.cascadeDisposeToken(task);
      context^.doneEvaluating;
      dispose(context,destroy);
    end;
    freeMem(p,sizeOf(T_asyncTask));
  end;

FUNCTION async_imp intFuncSignature;
  VAR p:P_asyncTask;
      childContext:P_threadContext;
      parameters:P_listLiteral=nil;
      dummy:P_token;
  begin
    result:=nil;
    if (params^.size>=1) and (arg0^.literalType=lt_expression) and
       ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) then begin
      childContext:=context.getNewAsyncContext;
      if childContext<>nil then begin
        getMem(p,sizeOf(T_asyncTask));
        p^.context:=childContext;
        if params^.size=2 then parameters:=list1;
        if not(P_subrule(arg0)^.replaces(parameters,tokenLocation,p^.task,dummy,context,false)) then begin
          freeMem(p,sizeOf(T_asyncTask));
          childContext^.doneEvaluating;
          dispose(childContext,destroy);
          exit(nil);
        end;
        beginThread(@doAsync,p);
        result:=newVoidLiteral;
      end else begin
        context.adapters^.raiseError('Creation of asynchronous tasks is forbidden for the current context',tokenLocation);
      end;
    end;
  end;

FUNCTION stringToTokens(CONST s:ansistring; CONST location:T_tokenLocation; CONST package:P_package; VAR context:T_threadContext):P_token;
  VAR exTokens:T_tokenArray;
      commentDummy,
      attributeDummy:T_arrayOfString;
      last:P_token;
  begin
    commentDummy  :=C_EMPTY_STRING_ARRAY;
    attributeDummy:=C_EMPTY_STRING_ARRAY;
    exTokens.create;
    exTokens.tokenizeAll(s,location,package,context.adapters^,false);
    exTokens.step(package,commentDummy,attributeDummy,context.adapters^);
    if exTokens.atEnd then begin
      context.adapters^.raiseError('The parsed expression appears to be empty',location);
      exTokens.destroy;
      exit(nil);
    end else if not(context.adapters^.noErrors) then begin
      exTokens.destroy;
      exit(nil); //Parsing error ocurred
    end;
    result:=context.recycler.newToken(exTokens.current); exTokens.current.undefine;
    last:=result;
    exTokens.step(package,commentDummy,attributeDummy,context.adapters^);
    while not(exTokens.atEnd) do begin
      last^.next:=context.recycler.newToken(exTokens.current);  exTokens.current.undefine;
      last:=last^.next;
      exTokens.step(package,commentDummy,attributeDummy,context.adapters^);
    end;
    exTokens.destroy;
  end;

FUNCTION listToTokens(CONST l:P_listLiteral; CONST location:T_tokenLocation; CONST package:P_package; VAR context:T_threadContext):P_token;
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
      package:P_package;
  begin
    result:=nil;
    package:=P_package(location.package);
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
        for i:=0 to P_listLiteral(arg0)^.size-1 do append(src,P_stringLiteral(P_listLiteral(arg0)^[i])^.value);
      end;
      result:=messagesToLiteralForSandbox(runAlone(src));
    end;
  end;

FUNCTION group_imp intFuncSignature;
  TYPE T_groupMap=specialize G_literalKeyMap<P_literal>;
  VAR listToGroup:P_listLiteral;
      keyList:T_arrayOfLiteral;
      aggregator:P_expressionLiteral;
      groupMap:T_groupMap;
      groupList :T_groupMap.KEY_VALUE_LIST;
      groupEntry:T_groupMap.CACHE_ENTRY;

  PROCEDURE makeKeysByIndex(CONST index:longint);
    VAR i:longint;
        dummy:P_literal;
    begin
      dummy:=newErrorLiteral;
      setLength(keyList,listToGroup^.size);
      for i:=0 to length(keyList)-1 do begin
        if (listToGroup^[i]^.literalType in C_listTypes) and (P_listLiteral(listToGroup^[i])^.size>index)
        then keyList[i]:=P_listLiteral(listToGroup^[i])^[index]^.rereferenced
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
          newLit:=P_subrule(aggregator)^.evaluateToLiteral(tokenLocation,@context,resultLiteral,L);
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

  VAR inputIndex:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.size<=3) and
       (arg0^.literalType in C_listTypes) and
      ((arg1^.literalType in C_listTypes) and (list1^.size=list0^.size) or (arg1^.literalType=lt_int)) and
      ((params^.size=2) or (arg2^.literalType=lt_expression))
    then begin
      listToGroup:=P_listLiteral(arg0);

      if arg1^.literalType=lt_int
      then makeKeysByIndex(P_intLiteral(arg1)^.value)
      else keyList:=list1^.iteratableList;

      if (params^.size=3) then aggregator:=P_expressionLiteral(arg2)
                          else aggregator:=nil;
      if aggregator<>nil then context.callStackPush(tokenLocation,@builtinLocation_group,params,nil);
      groupMap.create();
      for inputIndex:=0 to length(keyList)-1 do if context.adapters^.noErrors then
        addToAggregation(keyList[inputIndex],listToGroup^[inputIndex]);
      disposeLiteral(keyList);

      groupList:=groupMap.keyValueList;
      result:=newListLiteral(length(groupList));
      for groupEntry in groupList do listResult^.append(groupEntry.value,false);
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
      fmt:ansistring;
  FUNCTION fmtIt(CONST t:double):P_stringLiteral;
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
          for i:=0 to L^.size-1 do case(L^[i]^.literalType) of
            lt_int : P_listLiteral(result)^.append(fmtIt(P_intLiteral (L^[i])^.value),false);
            lt_real: P_listLiteral(result)^.append(fmtIt(P_realLiteral(L^[i])^.value),false);
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
          P_listLiteral(result)^.appendReal(encodeDateTime(P_stringLiteral(list1^[i])^.value));
      end;
    end;
  end;

FUNCTION obtainPackage(CONST callerPackage:P_package; CONST locationForWarning:T_tokenLocation; CONST filenameOrId:string; VAR context:T_threadContext; CONST registerAsSecondaryPackage:boolean; OUT freshlyCreated:boolean):P_package;
  VAR fileName:string='';
      id      :string='';
      p       :P_package;
  begin
    freshlyCreated:=false;
    if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
    then fileName:=expandFileName(filenameOrId)
    else id      :=filenameOrId;
    for p in callerPackage^.mainPackage^.secondaryPackages do
      if ({$ifdef Windows}uppercase{$endif}(id      )={$ifdef Windows}uppercase{$endif}(p^.codeProvider^.id    ))
      or ({$ifdef Windows}uppercase{$endif}(fileName)={$ifdef Windows}uppercase{$endif}(expandFileName(p^.getPath)))
      then exit(p);
    if id<>'' then fileName:=locateSource(extractFilePath(callerPackage^.getPath),id);
    if (fileName='') or not(fileExists(fileName)) then begin
      context.adapters^.raiseWarning('Cannot locate package with id or path "'+id+'"',locationForWarning);
      exit(nil);
    end;
    new(result,create(newFileCodeProvider(fileName),callerPackage^.mainPackage));
    result^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
    freshlyCreated:=true;
    if registerAsSecondaryPackage then begin
      setLength(callerPackage^.mainPackage^.secondaryPackages,
         length(callerPackage^.mainPackage^.secondaryPackages)+1);
                callerPackage^.mainPackage^.secondaryPackages[
         length(callerPackage^.mainPackage^.secondaryPackages)-1]:=result;
    end;
  end;

FUNCTION inspect_imp intFuncSignature;
  VAR i:longint;
      id:string;
      tempPackage:P_package;
      source:T_arrayOfString;
      needDispose:boolean=false;
      loadMessages:P_collectionLiteral=nil;

  PROCEDURE loadTemporaryPackage;
    VAR prevAdapters:P_adapters;
        collector   :P_collectingOutAdapter;
        mt:T_messageType;
    begin
      prevAdapters:=context.enterTryStatementReturningPreviousAdapters;
      collector:=P_collectingOutAdapter(context.adapters^.getAdapter(at_sandboxAdapter));
      for mt in T_messageType do collector^.enableMessageType(C_messageTypeMeta[mt].mClass in [mc_echo,mc_warning],[mt]);
      tempPackage^.load(lu_forCodeAssistance,context,C_EMPTY_STRING_ARRAY);
      loadMessages:=messagesToLiteralForSandbox(collector^.storedMessages);
      context.leaveTryStatementReassumingPreviousAdapters(prevAdapters,true);
    end;

  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=P_package(tokenLocation.package)^.inspect
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      id:=P_stringLiteral(arg0)^.value;

      tempPackage:=obtainPackage(P_package(tokenLocation.package),tokenLocation,id,context,false,needDispose);
      if tempPackage<>nil then begin
        if needDispose then loadTemporaryPackage;
        result:=tempPackage^.inspect;
        if loadMessages<>nil then mapResult^.put('messages',loadMessages,false);
        if needDispose then dispose(tempPackage,destroy);
        exit(result);
      end;
      result:=newListLiteral;
    end else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_stringList) then begin
      setLength(source,list1^.size);
      for i:=0 to list1^.size-1 do source[i]:=P_stringLiteral(list1^[i])^.value;
      new(tempPackage,create(newVirtualFileCodeProvider(str0^.value,source),nil));
      loadTemporaryPackage;
      result:=tempPackage^.inspect^.put('messages',loadMessages,false);
      dispose(tempPackage,destroy);
    end;
  end;

FUNCTION import_imp intFuncSignature;
  VAR p:P_package;
      freshlyCreated:boolean;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      p:=obtainPackage(P_package(tokenLocation.package),tokenLocation,str0^.value,context,true,freshlyCreated);
      if freshlyCreated then p^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
      if p=nil then result:=newListLiteral
               else result:=p^.getDynamicUseMeta(context);
    end else result:=nil;
  end;

FUNCTION clearRuleCache_imp intFuncSignature;
  VAR ruleIdToken:T_token;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      ruleIdToken.create;
      ruleIdToken.define(tokenLocation,P_stringLiteral(arg0)^.value,tt_identifier);
      ruleIdToken.resolveRuleId(tokenLocation.package,context.adapters);
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

FUNCTION writeAllDataStores_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.writeDataStores(context.adapters^,true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION writeDataStores_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.writeDataStores(context.adapters^,false);
      result:=newVoidLiteral;
    end else result:=nil;
  end;
{$endif}
{$ifdef include_initialization}
  //Unary Numeric -> real
  registerRule(MATH_NAMESPACE,'sqrt'  ,@sqrt_imp  ,true,ak_unary,'sqrt(n);//Returns the square root of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'sin'   ,@sin_imp   ,true,ak_unary,'sin(n);//Returns the sine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arcsin',@arcsin_imp,true,ak_unary,'arcsin(n);//Returns the arcsine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'cos'   ,@cos_imp   ,true,ak_unary,'cos(n);//Returns the cosine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arccos',@arccos_imp,true,ak_unary,'arccos(n);//Returns the arccosine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'tan'   ,@tan_imp   ,true,ak_unary,'tan(n);//Returns the tangent of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arctan',@arctan_imp,true,ak_unary,'arctan(n);//Returns the arctangent of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'exp'   ,@exp_imp   ,true,ak_unary,'exp(n);//Returns the exponential of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'ln'    ,@ln_imp    ,true,ak_unary,'ln(n);//Returns the natural logarithm of numeric or expression parameter n');
  //Unary Boolean -> boolean
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'not',@not_imp,true,ak_unary,'not(b:boolean);#not(b:booleanList);//Returns the negated value of b#not(i:int);#not(i:intList);//Returns the bitwise negated value of i');
  //Unary Numeric -> same (i.e. I -> I, R -> R)
  registerRule(MATH_NAMESPACE,'abs',@abs_imp,true,ak_unary,'abs(n);//Returns the absolute value of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'sqr',@sqr_imp,true,ak_unary,'sqr(n);//Returns the square of numeric or expression parameter n');
  //Unary Numeric -> Integer
  registerRule(MATH_NAMESPACE,'sign' ,@sign_imp ,true,ak_unary     ,'sign(n);//Returns the sign of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'ceil' ,@ceil_imp ,true,ak_variadic_1,'ceil(x);//Returns the smallest integer >=x#ceil(x,k);//Does the same but with k digits precision');
  registerRule(MATH_NAMESPACE,'floor',@floor_imp,true,ak_variadic_1,'floor(x);//Returns the largest integer <=x#floor(x,k);//Does the same but with k digits precision');
  registerRule(MATH_NAMESPACE,'round',@round_imp,true,ak_variadic_1,'round(x);//Returns the value of x, rounded to the nearest integer#round(x,k);//Returns the value of x rounded to k-digits precision');
  builtinLocation_time.create(SYSTEM_BUILTIN_NAMESPACE,'time');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'time',@time_imp,false,ak_variadic,'time;//Returns an internal time for time difference measurement.#'+
               'time(E:expression);//Evaluates E (without parameters) and returns a nested List with evaluation details.#'+
               'time(E:expression,par:list);//Evaluates E@par and returns a nested List with evaluation details.');
  builtinLocation_try.create(SYSTEM_BUILTIN_NAMESPACE,'try');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'try',@try_imp,false,ak_variadic,
               'try(E:expression(0));//Evaluates E and returns the result if successful or void if failed.#'+
               'try(E:expression(0),except(1):expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed with the errors as first paramter ($0).#'+
               'try(E:expression(0),except:expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed without paramters.#'+
               'try(E:expression(0),except);//Evaluates E and returns the result if successful. Otherwise <except> (any type except expression) is returned.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'async',@async_imp,false,ak_variadic_1,'async(E:expression);//Calls E asynchronously (without parameters) and returns void.#'+
               'async(E:expression,par:list);//Calls E@par and asynchronously and returns void.#//Asynchronous tasks are killed at the end of (synchonous) evaluation.');
  registerRule(TYPECAST_NAMESPACE       ,'toExpression'  ,@toExpression_imp  ,false,ak_unary,'toExpression(S);//Returns an expression parsed from S');
  builtinLocation_group.create(DEFAULT_BUILTIN_NAMESPACE,'group');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'group'         ,@group_imp         ,true,ak_variadic_2,'group(list,grouping);//Re-groups list by grouping (which is a sub-index or a list)#group(list,grouping,aggregator:expression);//Groups by grouping using aggregator on a per group basis');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sandBoxExecute',@sandBoxExecute_imp,false,ak_unary,'sandBoxExecute(source);//Executes the source (string or stringList) and returns created messages');
  builtinLocation_printf.create(SYSTEM_BUILTIN_NAMESPACE,'printf');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printf'         ,@printf_imp,true,ak_variadic_1,'printf(formatString:string,...);//Prints a formatted version of the given 0..n parameters and returns void, see <a href="formatStrings.html">Format Strings</a>');
  builtinLocation_format.create(STRINGS_NAMESPACE,'format');
  registerRule(STRINGS_NAMESPACE        ,'format'           ,@format_imp           ,true ,ak_variadic_1,'format(formatString:string,...);//Returns a formatted version of the given 0..n parameters, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(STRINGS_NAMESPACE        ,'formatTime'       ,@formatTime_imp       ,true ,ak_binary    ,'formatTime(formatString:string,t);//Returns time t (numeric list or scalar) formatted using format string, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(STRINGS_NAMESPACE        ,'parseTime'        ,@parseTime_imp        ,false,ak_binary    ,'parseTime(formatString:string,input:string);//Parses time from a given date format and input, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'inspect'          ,@inspect_imp          ,false,ak_variadic_1,'inspect;//Returs information on the current package#inspect(idOrPath:string);//Returns information on an imported package or a specified MNH source file#inspect(path:string,source:stringList);//Returns information of a package with given path and source');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'import'           ,@import_imp           ,false,ak_unary     ,'import(idOrPath:string);//Returns information on rules of an imported package or a specified MNH source file');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearRuleCache'   ,@clearRuleCache_imp   ,false,ak_unary     ,'clearRuleCache(id:string);//Clears memoized rule caches for a specified rule and returns void.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearPackageCache',@clearPackageCache_imp,false,ak_unary     ,'clearPackageCache(id:string);//Clears memoized rule caches for a used package specified by id or path and returns void.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearAllCaches'   ,@clearAllCaches_imp   ,false,ak_nullary   ,'clearAllCaches;//Clears all memoized rule caches and returns void.');

  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeAllDataStores',@writeAllDataStores_imp,false,ak_nullary,'writeAllDataStores;//Writes back all modified datastores (including stores in imported packages).');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeDataStores'   ,@writeDataStores_imp   ,false,ak_nullary,'writeDataStores;//Writes back all modified datastores of the current package (i.e. excluding stores in imported packages).');

{$endif}
{$ifdef include_finalization}
  builtinLocation_group .destroy;
  builtinLocation_time  .destroy;
  builtinLocation_try   .destroy;
  builtinLocation_printf.destroy;
  builtinLocation_format.destroy;
{$endif}
