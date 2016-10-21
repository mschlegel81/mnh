UNIT mnh_funcs_mnh;
INTERFACE
{$WARN 5024 OFF}
USES mnh_basicTypes,mnh_litVar,mnh_constants, mnh_funcs,sysutils,myGenerics,mnh_out_adapters,myStringUtil,mnh_html,mnh_contexts;
FUNCTION getMnhInfo:string;
VAR intFuncForOperator:array[tt_comparatorEq..tt_operatorIn] of P_intFuncCallback;
IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION sleep_imp intFuncSignature;
  VAR sleepUntil:double;
      sleepInt:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_real,lt_int]) then begin
      sleepUntil:=context.wallclockTime;
      result:=newVoidLiteral;
      if arg0^.literalType=lt_int
      then sleepUntil:=sleepUntil+P_intLiteral (arg0)^.value
      else sleepUntil:=sleepUntil+P_realLiteral(arg0)^.value;
      while (context.wallclockTime<sleepUntil) and (context.adapters^.noErrors) do begin
        sleepInt:=round(900*(sleepUntil-context.wallclockTime));
        if sleepInt>100 then sleepInt:=100;
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
      i:longint;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      keys:=intrinsicRuleMap.keySet;
      result:=newListLiteral;
      for i:=0 to length(keys)-1 do lResult^.appendString(keys[i]);
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
        lt_error,lt_void, lt_real,lt_expression: exit(newErrorLiteralRaising('ord can only be applied to booleans, ints and strings',tokenLocation,context.adapters^));
        else begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if context.adapters^.noErrors then
            lResult^.append(recurse(P_listLiteral(x)^.value(i)),false);
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
    result:=newListLiteral(9)^
      .append(newListLiteral(2)^.appendString('isFullVersion'  )^.appendBool  ({$ifdef fullVersion}true{$else}false{$endif}),false)^
      .append(newListLiteral(2)^.appendString('isDebugVersion' )^.appendBool  ({$ifdef debugMode}  true{$else}false{$endif}),false)^
      .append(newListLiteral(2)^.appendString('is64bit'        )^.appendBool  ({$ifdef CPU64}      true{$else}false{$endif}),false)^
      .append(newListLiteral(2)^.appendString('compileTime'    )^.appendString( {$I %DATE%}+' '+{$I %TIME%}                ),false)^
      .append(newListLiteral(2)^.appendString('compilerVersion')^.appendString( {$I %FPCVERSION%}                          ),false)^
      .append(newListLiteral(2)^.appendString('targetCpu'      )^.appendString( {$I %FPCTARGET%}                           ),false)^
      .append(newListLiteral(2)^.appendString('targetOs'       )^.appendString( {$I %FPCTargetOS%}                         ),false)^
      .append(newListLiteral(2)^.appendString('codeVersion'    )^.appendString( CODE_HASH                                  ),false)^
      .append(newListLiteral(2)^.appendString('flavour'        )^.appendString({$ifdef fullVersion}'F'{$else}'L'{$endif}+
                                                                               {$ifdef debugMode}  'D'{$else}'O'{$endif}+
                                                                               {$I %FPCTargetOS%}                          ),false)
    else result:=nil;
  end;

FUNCTION getMnhInfo:string;
  VAR L:P_literal;
      pseudoLoc:T_tokenLocation=(package:nil; line: 0; column: 0);
      context:T_evaluationContext;
  begin
    context.createContext(nil,ct_normal);
    L:=mnhInfo_imp(nil,pseudoLoc,context);
    context.destroy;
    result:=L^.toString();
    disposeLiteral(L);
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
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleep',@sleep_imp,'sleep(seconds:number);#Sleeps for the given number of seconds before returning void');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath',@myPath_impl,'myPath;#returns the path to the current package');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor',@executor_impl,'executor;#returns the path to the currently executing instance of MNH');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash',@hash_imp,'hash(x);#Returns the builtin hash for the given literal');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listBuiltin',@listBuiltin_imp,'listBuiltin;#Returns a list of all built-in functions (qualified and non-qualified)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listKeywords',@listKeywords_imp,'listKeywords;#Returns a list of all keywords by category');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord',@ord_imp,'ord(x);#Returns the ordinal value of x');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'mnhInfo',@mnhInfo_imp,'mnhInfo;#Returns a key-value list with info on the currently executing instance of MNH');
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

end.
