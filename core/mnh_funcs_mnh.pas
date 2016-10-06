UNIT mnh_funcs_mnh;
INTERFACE
{$WARN 5024 OFF}
USES mnh_basicTypes,mnh_litVar,mnh_constants, mnh_funcs,sysutils,myGenerics,mnh_out_adapters,myStringUtil,mnh_html,mnh_contexts;
FUNCTION getMnhInfo:string;
IMPLEMENTATION
{$MACRO ON}
{$define str0:=P_stringLiteral(params^.value(0))}
{$define str1:=P_stringLiteral(params^.value(1))}
{$define list0:=P_listLiteral(params^.value(0))}
{$define list1:=P_listLiteral(params^.value(1))}
{$define int0:=P_intLiteral(params^.value(0))}
{$define real0:=P_realLiteral(params^.value(0))}
{$define bool0:=P_boolLiteral(params^.value(0))}
{$define arg0:=params^.value(0)}
{$define arg1:=params^.value(1)}

FUNCTION sleep_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR sleepUntil:double;
      sleepInt:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_real,lt_int]) then begin
      sleepUntil:=context.wallclockTime;
      result:=newVoidLiteral;
      if params^.value(0)^.literalType=lt_int
      then sleepUntil:=sleepUntil+P_intLiteral (params^.value(0))^.value
      else sleepUntil:=sleepUntil+P_realLiteral(params^.value(0))^.value;
      while (context.wallclockTime<sleepUntil) and (context.adapters^.noErrors) do begin
        sleepInt:=round(900*(sleepUntil-context.wallclockTime));
        if sleepInt>100 then sleepInt:=100;
        if (sleepInt>0) then sleep(sleepInt);
      end;
    end;
  end;

FUNCTION softCast_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION softCastRecurse(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.softCast;
        lt_list..lt_listWithError: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do
            P_listLiteral(result)^.append(softCastRecurse(P_listLiteral(x)^.value(i)),false);
        end;
        else begin
          x^.rereference;
          result:=x;
        end;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=softCastRecurse(arg0);
  end;

FUNCTION toString_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_string then begin
        result:=arg0;
        result^.rereference;
      end else result:=newStringLiteral(arg0^.toString);
    end;
  end;

FUNCTION toBoolean_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_boolean: begin result:=arg0; result^.rereference; exit(result); end;
        lt_string: if lowercase(str0^.value) = LITERAL_BOOL_TEXT[false] then exit(newBoolLiteral(false))
              else if lowercase(str0^.value) = LITERAL_BOOL_TEXT[true]  then exit(newBoolLiteral(true));
        lt_int: if int0^.value=0 then exit(newBoolLiteral(false))
           else if int0^.value=1 then exit(newBoolLiteral(true));
        lt_real: if real0^.value=0 then exit(newBoolLiteral(false))
            else if real0^.value=1 then exit(newBoolLiteral(true));
      end;
      context.adapters^.raiseError(arg0^.toString+' cannot be cast to boolean',tokenLocation);
    end;
  end;

FUNCTION toInt_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR len:longint;
      i:int64;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_int: begin result:=arg0; result^.rereference; exit(result); end;
        lt_boolean: if bool0^.value then exit(newIntLiteral(1)) else exit(newIntLiteral(0));
        lt_real: if real0^.value=round(real0^.value) then exit(newIntLiteral(round(real0^.value)));
        lt_string: begin
          result:=parseNumber(str0^.value,1,false,len);
          if (result=nil) or (result^.literalType=lt_int) then exit(result);
          //parsed a real number
          if P_realLiteral(result)^.value=round(P_realLiteral(result)^.value) then begin
            i:=round(P_realLiteral(result)^.value);
            disposeLiteral(result);
            exit(newIntLiteral(i));
          end else begin
            disposeLiteral(result);
            result:=nil;
          end;
        end;
      end;
      context.adapters^.raiseError(arg0^.toString+' cannot be cast to int',tokenLocation);
    end;
  end;

FUNCTION toReal_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR len:longint;
      x:T_myFloat;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_real: begin result:=arg0; result^.rereference; exit(result); end;
        lt_boolean: if bool0^.value then exit(newIntLiteral(1)) else exit(newIntLiteral(0));
        lt_int: exit(newRealLiteral(int0^.value));
        lt_string: begin
          result:=parseNumber(str0^.value,1,false,len);
          if (result=nil) or (result^.literalType=lt_real) then exit(result);
          //parsed an integer
          x:=P_intLiteral(result)^.value;
          disposeLiteral(result);
          exit(newRealLiteral(x));
        end;
      end;
      context.adapters^.raiseError(arg0^.toString+' cannot be cast to real',tokenLocation);
    end;
  end;

FUNCTION myPath_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      if tokenLocation.package=nil then result:=newStringLiteral('<Unknown>')
                                   else result:=newStringLiteral(tokenLocation.package^.getPath);
    end;
  end;

FUNCTION executor_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then result:=newStringLiteral(paramStr(0));
  end;

FUNCTION hash_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=newIntLiteral(arg0^.hash);
  end;

FUNCTION listBuiltin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR keys:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      keys:=intrinsicRuleMap.keySet;
      result:=newListLiteral;
      for i:=0 to length(keys)-1 do P_listLiteral(result)^.appendString(keys[i]);
      setLength(keys,0);
    end;
  end;

FUNCTION listKeywords_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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

FUNCTION ord_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
            P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false);
        end;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=recurse(arg0);
  end;

FUNCTION mnhInfo_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
                                                                               {$ifdef IMIG}'I'{$else}''{$endif}+
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

INITIALIZATION
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleep',@sleep_imp,'sleep(seconds:number);#Sleeps for the given number of seconds before returning void');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'softCast',@softCast_imp,'softCast(X);#Returns a simplified version of X, trying to parse integers, real values and booleans');
  registerRule(TYPECAST_NAMESPACE,'toString',@toString_imp,'toString(X);#Casts X to string');
  registerRule(TYPECAST_NAMESPACE,'toBoolean',@toBoolean_imp,'toBoolean(X);#Casts X to boolean or throws an error if not possible');
  registerRule(TYPECAST_NAMESPACE,'toInt',@toInt_imp,'toInt(X);#Casts X to int or throws an error if not possible');
  registerRule(TYPECAST_NAMESPACE,'toReal',@toReal_imp,'toReal(X);#Casts X to real or throws an error if not possible');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath',@myPath_impl,'myPath;#returns the path to the current package');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor',@executor_impl,'executor;#returns the path to the currently executing instance of MNH');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash',@hash_imp,'hash(x);#Returns the builtin hash for the given literal');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listBuiltin',@listBuiltin_imp,'listBuiltin;#Returns a list of all built-in functions (qualified and non-qualified)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listKeywords',@listKeywords_imp,'listKeywords;#Returns a list of all keywords by category');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord',@ord_imp,'ord(x);#Returns the ordinal value of x');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'mnhInfo',@mnhInfo_imp,'mnhInfo;#Returns a key-value list with info on the currently executing instance of MNH');

end.
