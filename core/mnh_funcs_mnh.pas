UNIT mnh_funcs_mnh;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     myGenerics,
     myStringUtil,
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_litVar,
     mnh_tokens,
     mnh_contexts,
     mnh_funcs;
FUNCTION getMnhInfo:string;
VAR intFuncForOperator:array[tt_comparatorEq..tt_operatorIn] of P_intFuncCallback;
IMPLEMENTATION
VAR builtinLocation_try:T_identifiedInternalFunction;
{$i mnh_func_defines.inc}

FUNCTION sleep_imp intFuncSignature;
  VAR sleepUntil:double;
      sleepInt:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_real,lt_int]) then begin
      sleepUntil:=context.wallclockTime(true);
      result:=newVoidLiteral;
      if arg0^.literalType=lt_int
      then sleepUntil:=sleepUntil+P_intLiteral (arg0)^.value
      else sleepUntil:=sleepUntil+P_realLiteral(arg0)^.value;
      while (context.wallclockTime(true)<sleepUntil) and (context.adapters^.noErrors) do begin
        sleepInt:=round(900*(sleepUntil-context.wallclockTime(true)));
        if sleepInt>1000 then sleepInt:=1000;
        if (sleepInt>0) then sleep(sleepInt);
      end;
    end;
  end;

FUNCTION myPath_impl intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      if tokenLocation.package=nil then result:=newStringLiteral('<Unknown>')
                                   else result:=newStringLiteral(tokenLocation.package^.getPath);
    end;
  end;

FUNCTION executor_impl intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then result:=newStringLiteral(paramStr(0));
  end;

FUNCTION hash_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=newIntLiteral(arg0^.hash);
  end;

FUNCTION listBuiltin_imp intFuncSignature;
  VAR keys:T_arrayOfString;
      key:ansistring;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      keys:=intrinsicRuleMap.keySet;
      result:=newSetLiteral;
      for key in keys do setResult^.appendString(key);
      setLength(keys,0);
    end;
  end;

FUNCTION listKeywords_imp intFuncSignature;
  VAR i:longint;
      tt:T_tokenType;
      subList:array[T_reservedWordClass] of P_listLiteral;
      rc:T_reservedWordClass;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      for rc:=low(T_reservedWordClass) to high(T_reservedWordClass) do subList[rc]:=newListLiteral;
      for tt:=low(T_tokenType) to high(T_tokenType) do
      if isIdentifier(C_tokenInfo[tt].defaultId,false) or
         ((copy(C_tokenInfo[tt].defaultId,1,1)='.') or (copy(C_tokenInfo[tt].defaultId,1,1)=':')) and
         isIdentifier(copy(C_tokenInfo[tt].defaultId,2,length(C_tokenInfo[tt].defaultId)-1),false)
      then subList[C_tokenInfo[tt].reservedWordClass]^.appendString(C_tokenInfo[tt].defaultId);
      for i:=0 to length(C_specialWordInfo)-1 do subList[C_specialWordInfo[i].reservedWordClass]^.appendString(C_specialWordInfo[i].txt);
      disposeLiteral(subList[rwc_not_reserved]);
      result:=newListLiteral^
        .append(newListLiteral^.appendString('specialLiterals')^
                .append(subList[rwc_specialLiteral],false),false)^
        .append(newListLiteral^.appendString('specialConstructs')^
                .append(subList[rwc_specialConstruct],false),false)^
        .append(newListLiteral^.appendString('operators')^
                .append(subList[rwc_operator],false),false)^
        .append(newListLiteral^.appendString('typeChecks')^
                .append(subList[rwc_typeCheck],false),false)^
        .append(newListLiteral^.appendString('modifiers')^
                .append(subList[rwc_modifier],false),false);
    end;
  end;

FUNCTION ord_imp intFuncSignature;
  FUNCTION recurse(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_boolean: if P_boolLiteral(x)^.value
                    then exit(newIntLiteral(1))
                    else exit(newIntLiteral(0));
        lt_int: begin x^.rereference; exit(x); end;
        lt_string : if length(P_stringLiteral(x)^.value)=1
                    then exit(newIntLiteral(ord(P_stringLiteral(x)^.value[1])))
                    else exit(newIntLiteral(-1));
        lt_error,lt_void, lt_real,lt_expression: begin
          context.adapters^.raiseError('ord can only be applied to booleans, ints and strings',tokenLocation);
          exit(newVoidLiteral);
        end else begin
          if x^.literalType in C_listTypes
          then result:=newListLiteral(P_compoundLiteral(x)^.size)
          else result:=newSetLiteral;
          for i:=0 to P_compoundLiteral(x)^.size-1 do if context.adapters^.noErrors then
            collResult^.append(recurse(P_compoundLiteral(x)^[i]),false);
        end;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=recurse(arg0);
  end;

FUNCTION mnhInfo_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then
    result:=newMapLiteral^
      .put('isFullVersion'  ,{$ifdef fullVersion}true{$else}false{$endif})^
      .put('isDebugVersion' ,{$ifdef debugMode}  true{$else}false{$endif})^
      .put('is64bit'        ,{$ifdef CPU64}      true{$else}false{$endif})^
      .put('compileTime'    , {$I %DATE%}+' '+{$I %TIME%}                )^
      .put('compilerVersion', {$I %FPCVERSION%}                          )^
      .put('targetCpu'      , {$I %FPCTARGET%}                           )^
      .put('targetOs'       , {$I %FPCTargetOS%}                         )^
      .put('codeVersion'    , CODE_HASH                                  )^
      .put('built'          , BUILT_NUMBER                               )^
      .put('flavour'        ,{$ifdef fullVersion}'F'{$else}'L'{$endif}+
                             {$ifdef debugMode}  'D'{$else}'O'{$endif}+
                             {$I %FPCTargetOS%}                          )
    else result:=nil;
  end;

FUNCTION getMnhInfo:string;
  VAR L:P_literal;
      pseudoLoc:T_tokenLocation=(package:nil; line: 0; column: 0);
      dummyContext:T_threadContext;
  begin
    L:=mnhInfo_imp(nil,pseudoLoc,dummyContext);
    result:=L^.toString();
    disposeLiteral(L);
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
      result:=P_expressionLiteral(arg0)^.evaluateToLiteral(tokenLocation,@context);
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
            then result:=P_expressionLiteral(arg1)^.evaluateToLiteral(tokenLocation,@context,messages)
            else result:=P_expressionLiteral(arg1)^.evaluateToLiteral(tokenLocation,@context);
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
      context^.reduceExpression(task);
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
        if not(subruleReplacesCallback(arg0,parameters,tokenLocation,p^.task,dummy,context,false)) then begin
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

{$MACRO ON}
{$define funcForOp:=intFuncSignature; begin if (params<>nil) and (params^.size=2) then result:=resolveOperator(arg0,OP,arg1,tokenLocation,context.adapters^) else result:=nil; end}
{$define OP:=tt_comparatorEq     } FUNCTION funcFor_comparatorEq      funcForOp;
{$define OP:=tt_comparatorNeq    } FUNCTION funcFor_comparatorNeq     funcForOp;
{$define OP:=tt_comparatorLeq    } FUNCTION funcFor_comparatorLeq     funcForOp;
{$define OP:=tt_comparatorGeq    } FUNCTION funcFor_comparatorGeq     funcForOp;
{$define OP:=tt_comparatorLss    } FUNCTION funcFor_comparatorLss     funcForOp;
{$define OP:=tt_comparatorGrt    } FUNCTION funcFor_comparatorGrt     funcForOp;
{$define OP:=tt_comparatorListEq } FUNCTION funcFor_comparatorListEq  funcForOp;
{$define OP:=tt_operatorAnd      } FUNCTION funcFor_operatorAnd       funcForOp;
{$define OP:=tt_operatorOr       } FUNCTION funcFor_operatorOr        funcForOp;
{$define OP:=tt_operatorXor      } FUNCTION funcFor_operatorXor       funcForOp;
FUNCTION funcFor_operatorLazyAnd intFuncSignature;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_boolean) then begin
      if bool0^.value then result:=arg1 else result:=arg0;
      result^.rereference;
    end else result:=nil;
  end;

FUNCTION funcFor_operatorLazyOr  intFuncSignature;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_boolean) then begin
      if bool0^.value then result:=arg0 else result:=arg1;
      result^.rereference;
    end else result:=nil;
  end;
{$define OP:=tt_operatorPlus     } FUNCTION funcFor_operatorPlus      funcForOp;
{$define OP:=tt_operatorMinus    } FUNCTION funcFor_operatorMinus     funcForOp;
{$define OP:=tt_operatorMult     } FUNCTION funcFor_operatorMult      funcForOp;
{$define OP:=tt_operatorDivReal  } FUNCTION funcFor_operatorDivReal   funcForOp;
{$define OP:=tt_operatorDivInt   } FUNCTION funcFor_operatorDivInt    funcForOp;
{$define OP:=tt_operatorMod      } FUNCTION funcFor_operatorMod       funcForOp;
{$define OP:=tt_operatorPot      } FUNCTION funcFor_operatorPot       funcForOp;
{$define OP:=tt_unaryOpPlus      } FUNCTION funcFor_unaryOpPlus       funcForOp;
{$define OP:=tt_unaryOpMinus     } FUNCTION funcFor_unaryOpMinus      funcForOp;
{$define OP:=tt_operatorStrConcat} FUNCTION funcFor_operatorStrConcat funcForOp;
FUNCTION funcFor_operatorOrElse intFuncSignature;
  begin
    if (params<>nil) and ((params^.size=1) or (params^.size=2)) then begin
      result:=arg0;
      result^.rereference;
    end else result:=nil;
  end;
{$define OP:=tt_operatorConcat   } FUNCTION funcFor_operatorConcat    funcForOp;
{$define OP:=tt_operatorIn       } FUNCTION funcFor_operatorIn        funcForOp;
{$undef OP}
{$undef funcForOp}
INITIALIZATION
  builtinLocation_try.create(SYSTEM_BUILTIN_NAMESPACE,'try');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'try',@try_imp,false,ak_variadic,
               'try(E:expression(0));//Evaluates E and returns the result if successful or void if failed.#'+
               'try(E:expression(0),except(1):expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed with the errors as first paramter ($0).#'+
               'try(E:expression(0),except:expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed without paramters.#'+
               'try(E:expression(0),except);//Evaluates E and returns the result if successful. Otherwise <except> (any type except expression) is returned.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'async',@async_imp,false,ak_variadic_1,'async(E:expression);//Calls E asynchronously (without parameters) and returns void.#'+
               'async(E:expression,par:list);//Calls E@par and asynchronously and returns void.#//Asynchronous tasks are killed at the end of (synchonous) evaluation.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleep'       ,@sleep_imp       ,false,ak_unary  ,'sleep(seconds:number);//Sleeps for the given number of seconds before returning void');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath'      ,@myPath_impl     ,false,ak_nullary,'myPath;//returns the path to the current package');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor'    ,@executor_impl   ,false,ak_nullary,'executor;//returns the path to the currently executing instance of MNH');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash'        ,@hash_imp        ,true ,ak_unary  ,'hash(x);//Returns the builtin hash for the given literal');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listBuiltin' ,@listBuiltin_imp ,false,ak_nullary,'listBuiltin;//Returns a list of all built-in functions (qualified and non-qualified)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listKeywords',@listKeywords_imp,false,ak_nullary,'listKeywords;//Returns a list of all keywords by category');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord'         ,@ord_imp         ,true ,ak_unary  ,'ord(x);//Returns the ordinal value of x');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'mnhInfo'     ,@mnhInfo_imp     ,false,ak_nullary,'mnhInfo;//Returns a key-value list with info on the currently executing instance of MNH');
  intFuncForOperator[tt_comparatorEq     ]:=@funcFor_comparatorEq     ;
  intFuncForOperator[tt_comparatorNeq    ]:=@funcFor_comparatorNeq    ;
  intFuncForOperator[tt_comparatorLeq    ]:=@funcFor_comparatorLeq    ;
  intFuncForOperator[tt_comparatorGeq    ]:=@funcFor_comparatorGeq    ;
  intFuncForOperator[tt_comparatorLss    ]:=@funcFor_comparatorLss    ;
  intFuncForOperator[tt_comparatorGrt    ]:=@funcFor_comparatorGrt    ;
  intFuncForOperator[tt_comparatorListEq ]:=@funcFor_comparatorListEq ;
  intFuncForOperator[tt_operatorAnd      ]:=@funcFor_operatorAnd      ;
  intFuncForOperator[tt_operatorOr       ]:=@funcFor_operatorOr       ;
  intFuncForOperator[tt_operatorXor      ]:=@funcFor_operatorXor      ;
  intFuncForOperator[tt_operatorLazyAnd  ]:=@funcFor_operatorLazyAnd  ;
  intFuncForOperator[tt_operatorLazyOr   ]:=@funcFor_operatorLazyOr   ;
  intFuncForOperator[tt_operatorPlus     ]:=@funcFor_operatorPlus     ;
  intFuncForOperator[tt_operatorMinus    ]:=@funcFor_operatorMinus    ;
  intFuncForOperator[tt_operatorMult     ]:=@funcFor_operatorMult     ;
  intFuncForOperator[tt_operatorDivReal  ]:=@funcFor_operatorDivReal  ;
  intFuncForOperator[tt_operatorDivInt   ]:=@funcFor_operatorDivInt   ;
  intFuncForOperator[tt_operatorMod      ]:=@funcFor_operatorMod      ;
  intFuncForOperator[tt_operatorPot      ]:=@funcFor_operatorPot      ;
  intFuncForOperator[tt_unaryOpPlus      ]:=@funcFor_unaryOpPlus      ;
  intFuncForOperator[tt_unaryOpMinus     ]:=@funcFor_unaryOpMinus     ;
  intFuncForOperator[tt_operatorStrConcat]:=@funcFor_operatorStrConcat;
  intFuncForOperator[tt_operatorOrElse   ]:=@funcFor_operatorOrElse   ;
  intFuncForOperator[tt_operatorConcat   ]:=@funcFor_operatorConcat   ;
  intFuncForOperator[tt_operatorIn       ]:=@funcFor_operatorIn       ;
FINALIZATION
  builtinLocation_try   .destroy;

end.
