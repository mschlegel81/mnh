UNIT funcs_mnh;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     myGenerics,
     basicTypes,mnh_constants,
     litVar,
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
      lt_smallint: sleepUntil+=P_smallIntLiteral(argument)^.value;
      lt_bigint  : sleepUntil+=P_bigIntLiteral  (argument)^.value.toFloat;
      lt_real    : sleepUntil+=P_realLiteral    (argument)^.value;
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
      .put('fullVersionPath' ,settings.fullFlavourLocation)^
      .put('lightVersionPath',settings.lightFlavourLocation)^
      .put('configured_cpus',settings.cpuCount)^
      .put('configured_mem' ,settings.memoryLimit)
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
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleep'       ,@sleep_imp       ,ak_unary  {$ifdef fullVersion},'sleep(seconds:Numeric);//Sleeps for the given number of seconds before returning void'{$endif});
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleepUntil'  ,@sleepUntil_imp  ,ak_unary  {$ifdef fullVersion},'sleepUntil(wallClockSeconds:Numeric);//Sleeps until the wallclock reaches the given value'{$endif});
  BUILTIN_MYPATH:=
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath'      ,@myPath_impl     ,ak_nullary{$ifdef fullVersion},'myPath;//returns the path to the current package'{$endif});
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor'    ,@executor_impl   ,ak_nullary{$ifdef fullVersion},'executor;//returns the path to the currently executing instance of MNH'{$endif});
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash'        ,@hash_imp        ,ak_unary  {$ifdef fullVersion},'hash(x);//Returns the builtin hash for the given literal'{$endif});
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listSideEffects',@listSideEffects_imp,ak_nullary{$ifdef fullVersion},'listSideEffects;//Returns a list of all side effects, e.g. as parameters for interpret'{$endif});
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord'         ,@ord_imp         ,ak_unary  {$ifdef fullVersion},'ord(x);//Returns the ordinal value of x'{$endif});
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'mnhInfo'     ,@mnhInfo_imp     ,ak_nullary{$ifdef fullVersion},'mnhInfo;//Returns a key-value list with info on the currently executing instance of MNH'{$endif});
end.
