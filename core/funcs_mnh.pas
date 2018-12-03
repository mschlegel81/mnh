UNIT funcs_mnh;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     myGenerics,
     myStringUtil,
     basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_litVar,
     recyclers,
     contexts,
     mnh_settings,
     funcs;
FUNCTION getMnhInfo:T_arrayOfString;
{$i func_defines.inc}
VAR BUILTIN_MYPATH:P_intFuncCallback;
IMPLEMENTATION
PROCEDURE mySleep(CONST argument:P_numericLiteral; CONST argIsEndTime:boolean; VAR context:T_context); inline;
  VAR sleepUntil:double=0;
      sleepInt:longint;
  begin
    if argIsEndTime then sleepUntil:=0 else sleepUntil:=context.wallclockTime;
    case argument^.literalType of
      lt_smallint: sleepUntil:=sleepUntil+P_smallIntLiteral(argument)^.value;
      lt_bigint  : sleepUntil:=sleepUntil+P_bigIntLiteral  (argument)^.value.toFloat;
      lt_real    : sleepUntil:=sleepUntil+P_realLiteral    (argument)^.value;
    end;
    while (context.wallclockTime<sleepUntil) and (context.messages^.continueEvaluation) do begin
      sleepInt:=round(900*(sleepUntil-context.wallclockTime));
      if sleepInt>1000 then sleepInt:=1000;
      if (sleepInt>0) then sleep(sleepInt);
    end;
  end;

FUNCTION sleep_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_real,lt_smallint,lt_bigint]) and context.checkSideEffects('sleep',tokenLocation,[se_sleep]) then begin
      mySleep(P_numericLiteral(arg0),false,context);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION sleepUntil_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_real,lt_smallint,lt_bigint]) and context.checkSideEffects('sleepUntil',tokenLocation,[se_sleep]) then begin
      mySleep(P_numericLiteral(arg0),true,context);
      result:=newVoidLiteral;
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

FUNCTION listSideEffects_imp intFuncSignature;
  VAR se:T_sideEffect;
  begin
    if (params=nil) or (params^.size=0) then begin
      result:=newListLiteral();
      for se in T_sideEffect do listResult^.appendString(C_sideEffectName[se]);
    end else result:=nil;
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
        lt_smallint,lt_bigint: exit(x^.rereferenced);
        lt_string : if length(P_stringLiteral(x)^.value)=1
                    then exit(newIntLiteral(ord(P_stringLiteral(x)^.value[1])))
                    else exit(newIntLiteral(-1));
        lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction('ord',tokenLocation,@context,@recycler);
        lt_error,lt_void, lt_real: begin
          context.raiseError('ord can only be applied to booleans, ints and strings',tokenLocation);
          exit(newVoidLiteral);
        end else begin
          if x^.literalType in C_listTypes
          then result:=newListLiteral(P_compoundLiteral(x)^.size)
          else result:=newSetLiteral;
          iter:=P_compoundLiteral(x)^.iteratableList;
          for sub in iter do if context.messages^.continueEvaluation then
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
      .put('compileTime'    ,{$I %DATE%}+' '+{$I %TIME%})^
      .put('compilerVersion',{$I %FPCVERSION%}          )^
      .put('targetCpu'      ,{$I %FPCTARGET%}           )^
      .put('targetOs'       ,{$I %FPCTargetOS%}         )^
      .put('codeVersion'    ,CODE_HASH                  )^
      .put('build'          ,BUILD_NUMBER               )^
      .put('flavour'        ,FLAVOUR_STRING             )^
      .put('configDir'      ,configDir                  )^
      .put('fullVersionPath',settings.fullFlavourLocation)
    else result:=nil;
  end;

FUNCTION getMnhInfo:T_arrayOfString;
  VAR L:P_literal;
      pseudoLoc:T_tokenLocation=(package:nil; line: 0; column: 0);
      dummyContext:T_context;
      recycler:T_recycler;
  begin
    recycler.initRecycler;
    initialize(dummyContext);
    L:=mnhInfo_imp(nil,pseudoLoc,dummyContext,recycler);
    result:=serializeToStringList(L,pseudoLoc,nil);
    disposeLiteral(L);
    recycler.cleanup;
  end;

INITIALIZATION
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleep'       ,@sleep_imp       ,ak_unary  ,'sleep(seconds:Numeric);//Sleeps for the given number of seconds before returning void');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleepUntil'  ,@sleepUntil_imp  ,ak_unary  ,'sleepUntil(wallClockSeconds:Numeric);//Sleeps until the wallclock reaches the given value');
  BUILTIN_MYPATH:=
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath'      ,@myPath_impl     ,ak_nullary,'myPath;//returns the path to the current package');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor'    ,@executor_impl   ,ak_nullary,'executor;//returns the path to the currently executing instance of MNH');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash'        ,@hash_imp        ,ak_unary  ,'hash(x);//Returns the builtin hash for the given literal');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listBuiltin' ,@listBuiltin_imp ,ak_nullary,'listBuiltin;//Returns a list of all built-in functions (qualified and non-qualified)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listKeywords',@listKeywords_imp,ak_nullary,'listKeywords;//Returns a list of all keywords by category');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listSideEffects',@listSideEffects_imp,ak_nullary,'listSideEffects;//Returns a list of all side effects, e.g. as parameters for interpret');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord'         ,@ord_imp         ,ak_unary  ,'ord(x);//Returns the ordinal value of x');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'mnhInfo'     ,@mnhInfo_imp     ,ak_nullary,'mnhInfo;//Returns a key-value list with info on the currently executing instance of MNH');
end.
