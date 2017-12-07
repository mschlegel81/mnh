UNIT mnh_funcs_mnh;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     myGenerics,
     myStringUtil,
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_litVar,
     mnh_contexts,
     mnh_funcs;
FUNCTION getMnhInfo:string;
{$i mnh_func_defines.inc}
VAR intFuncForOperator:array[tt_comparatorEq..tt_operatorConcatAlt] of P_intFuncCallback;
    BUILTIN_MYPATH:P_intFuncCallback;
IMPLEMENTATION
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

FUNCTION sleepUntil_imp intFuncSignature;
  VAR sleepUntil:double;
      sleepInt:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_real,lt_int]) then begin
      result:=newVoidLiteral;
      if arg0^.literalType=lt_int
      then sleepUntil:=P_intLiteral (arg0)^.value
      else sleepUntil:=P_realLiteral(arg0)^.value;
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
      tc:T_typeCheck;
      md:T_modifier;
      subList:array[T_reservedWordClass] of P_listLiteral;
      rc:T_reservedWordClass;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      for rc:=low(T_reservedWordClass) to high(T_reservedWordClass) do subList[rc]:=newListLiteral;
      for tc in T_typeCheck do subList[rwc_type]^.appendString(C_typeCheckInfo[tc].name);
      for md in T_modifier do subList[rwc_modifier]^.appendString(C_modifierInfo[md].name);
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
        .append(newListLiteral^.appendString('types')^
                .append(subList[rwc_type],false),false)^
        .append(newListLiteral^.appendString('modifiers')^
                .append(subList[rwc_modifier],false),false);
    end;
  end;

FUNCTION ord_imp intFuncSignature;
  FUNCTION recurse(CONST x:P_literal):P_literal;
    VAR iter:T_arrayOfLiteral;
        sub:P_literal;
    begin
      case x^.literalType of
        lt_boolean: if P_boolLiteral(x)^.value
                    then exit(newIntLiteral(1))
                    else exit(newIntLiteral(0));
        lt_int: begin x^.rereference; exit(x); end;
        lt_string : if length(P_stringLiteral(x)^.value)=1
                    then exit(newIntLiteral(ord(P_stringLiteral(x)^.value[1])))
                    else exit(newIntLiteral(-1));
        lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction('ord',tokenLocation,@context);
        lt_error,lt_void, lt_real: begin
          context.adapters^.raiseError('ord can only be applied to booleans, ints and strings',tokenLocation);
          exit(newVoidLiteral);
        end else begin
          if x^.literalType in C_listTypes
          then result:=newListLiteral(P_compoundLiteral(x)^.size)
          else result:=newSetLiteral;
          iter:=P_compoundLiteral(x)^.iteratableList;
          for sub in iter do if context.adapters^.noErrors then
            collResult^.append(recurse(sub),false);
          disposeLiteral(iter);
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
      .put('build'          , BUILT_NUMBER                               )^
      .put('flavour'        ,{$ifdef fullVersion}'F'{$else}'L'{$endif}+
                             {$ifdef debugMode}  'D'{$else}'O'{$endif}+
                             {$I %FPCTargetOS%}                          )^
      .put('configDir'      ,configDir)
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

{$MACRO ON}
{$define funcForOp:=begin if (params<>nil) and (params^.size=2) then result:=resolveOperator(arg0,OP,arg1,tokenLocation,context.adapters^,@context) else result:=nil; end}
{$define OP:=tt_comparatorEq     } FUNCTION funcFor_comparatorEq      intFuncSignature; funcForOp;
{$define OP:=tt_comparatorNeq    } FUNCTION funcFor_comparatorNeq     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorLeq    } FUNCTION funcFor_comparatorLeq     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorGeq    } FUNCTION funcFor_comparatorGeq     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorLss    } FUNCTION funcFor_comparatorLss     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorGrt    } FUNCTION funcFor_comparatorGrt     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorListEq } FUNCTION funcFor_comparatorListEq  intFuncSignature; funcForOp;
{$define OP:=tt_operatorAnd      } FUNCTION funcFor_operatorAnd       intFuncSignature; funcForOp;
{$define OP:=tt_operatorOr       } FUNCTION funcFor_operatorOr        intFuncSignature; funcForOp;
{$define OP:=tt_operatorXor      } FUNCTION funcFor_operatorXor       intFuncSignature; funcForOp;
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
{$define OP:=tt_operatorPlus     } FUNCTION funcFor_operatorPlus      intFuncSignature; funcForOp;
{$define OP:=tt_operatorMinus    } FUNCTION funcFor_operatorMinus     intFuncSignature; funcForOp;
{$define OP:=tt_operatorMult     } FUNCTION funcFor_operatorMult      intFuncSignature; funcForOp;
{$define OP:=tt_operatorDivReal  } FUNCTION funcFor_operatorDivReal   intFuncSignature; funcForOp;
{$define OP:=tt_operatorDivInt   } FUNCTION funcFor_operatorDivInt    intFuncSignature; funcForOp;
{$define OP:=tt_operatorMod      } FUNCTION funcFor_operatorMod       intFuncSignature; funcForOp;
{$define OP:=tt_operatorPot      } FUNCTION funcFor_operatorPot       intFuncSignature; funcForOp;
{$define OP:=tt_operatorStrConcat} FUNCTION funcFor_operatorStrConcat intFuncSignature; funcForOp;
FUNCTION funcFor_operatorOrElse intFuncSignature;
  begin
    if (params<>nil) and ((params^.size=1) or (params^.size=2)) then begin
      result:=arg0;
      result^.rereference;
    end else result:=nil;
  end;
{$define OP:=tt_operatorConcat   } FUNCTION funcFor_operatorConcat intFuncSignature; funcForOp;
{$define OP:=tt_operatorIn       } FUNCTION funcFor_operatorIn     intFuncSignature; funcForOp;
{$undef OP}
{$undef funcForOp}
INITIALIZATION
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleep'       ,@sleep_imp       ,[se_sleep],ak_unary  ,'sleep(seconds:number);//Sleeps for the given number of seconds before returning void');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleepUntil'  ,@sleepUntil_imp  ,[se_sleep],ak_unary  ,'sleepUntil(wallClockSeconds:number);//Sleeps until the wallclock reaches the given value');
  BUILTIN_MYPATH:=
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath'      ,@myPath_impl     ,[se_scriptDependent],ak_nullary,'myPath;//returns the path to the current package');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor'    ,@executor_impl   ,[se_executableDependent],ak_nullary,'executor;//returns the path to the currently executing instance of MNH');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash'        ,@hash_imp        ,[] ,ak_unary  ,'hash(x);//Returns the builtin hash for the given literal');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listBuiltin' ,@listBuiltin_imp ,[se_versionDependent],ak_nullary,'listBuiltin;//Returns a list of all built-in functions (qualified and non-qualified)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listKeywords',@listKeywords_imp,[se_versionDependent],ak_nullary,'listKeywords;//Returns a list of all keywords by category');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord'         ,@ord_imp         ,[] ,ak_unary  ,'ord(x);//Returns the ordinal value of x');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'mnhInfo'     ,@mnhInfo_imp     ,[se_versionDependent],ak_nullary,'mnhInfo;//Returns a key-value list with info on the currently executing instance of MNH');
  intFuncForOperator[tt_comparatorEq     ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::='     ,@funcFor_comparatorEq     ,[],ak_binary,'//Function wrapper for operator =');
  intFuncForOperator[tt_comparatorNeq    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::!='    ,@funcFor_comparatorNeq    ,[],ak_binary,'//Function wrapper for operator !=');
  intFuncForOperator[tt_comparatorLeq    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::<='    ,@funcFor_comparatorLeq    ,[],ak_binary,'//Function wrapper for operator <=');
  intFuncForOperator[tt_comparatorGeq    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::>='    ,@funcFor_comparatorGeq    ,[],ak_binary,'//Function wrapper for operator >=');
  intFuncForOperator[tt_comparatorLss    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::<'     ,@funcFor_comparatorLss    ,[],ak_binary,'//Function wrapper for operator <');
  intFuncForOperator[tt_comparatorGrt    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::>'     ,@funcFor_comparatorGrt    ,[],ak_binary,'//Function wrapper for operator >');
  intFuncForOperator[tt_comparatorListEq ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::=='    ,@funcFor_comparatorListEq ,[],ak_binary,'//Function wrapper for operator ==');
  intFuncForOperator[tt_operatorAnd      ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::and'   ,@funcFor_operatorAnd      ,[],ak_binary,'//Function wrapper for operator and');
  intFuncForOperator[tt_operatorOr       ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::or'    ,@funcFor_operatorOr       ,[],ak_binary,'//Function wrapper for operator or');
  intFuncForOperator[tt_operatorXor      ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::xor'   ,@funcFor_operatorXor      ,[],ak_binary,'//Function wrapper for operator xor');
  intFuncForOperator[tt_operatorLazyAnd  ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::AND'   ,@funcFor_operatorLazyAnd  ,[],ak_binary,'//Function wrapper for operator AND#');
  intFuncForOperator[tt_operatorLazyOr   ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::OR'    ,@funcFor_operatorLazyOr   ,[],ak_binary,'//Function wrapper for operator OR'    );
  intFuncForOperator[tt_operatorPlus     ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::+'     ,@funcFor_operatorPlus     ,[],ak_binary,'//Function wrapper for operator +'     );
  intFuncForOperator[tt_operatorMinus    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::-'     ,@funcFor_operatorMinus    ,[],ak_binary,'//Function wrapper for operator -'     );
  intFuncForOperator[tt_operatorMult     ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::*'     ,@funcFor_operatorMult     ,[],ak_binary,'//Function wrapper for operator *'     );
  intFuncForOperator[tt_operatorDivReal  ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::/'     ,@funcFor_operatorDivReal  ,[],ak_binary,'//Function wrapper for operator /'     );
  intFuncForOperator[tt_operatorDivInt   ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::div'   ,@funcFor_operatorDivInt   ,[],ak_binary,'//Function wrapper for operator div'   );
  intFuncForOperator[tt_operatorMod      ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::mod'   ,@funcFor_operatorMod      ,[],ak_binary,'//Function wrapper for operator mod'   );
  intFuncForOperator[tt_operatorPot      ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::^'     ,@funcFor_operatorPot      ,[],ak_binary,'//Function wrapper for operator ^'     );
  intFuncForOperator[tt_operatorStrConcat]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::&'     ,@funcFor_operatorStrConcat,[],ak_binary,'//Function wrapper for operator &'     );
  intFuncForOperator[tt_operatorOrElse   ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::orElse',@funcFor_operatorOrElse   ,[],ak_binary,'//Function wrapper for operator orElse');
  intFuncForOperator[tt_operatorConcat   ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::|'     ,@funcFor_operatorConcat   ,[],ak_binary,'//Function wrapper for operator |'     );
  intFuncForOperator[tt_operatorIn       ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::in'    ,@funcFor_operatorIn       ,[],ak_binary,'//Function wrapper for operator in'    );

end.
