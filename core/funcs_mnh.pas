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
FUNCTION canInterpretAsSideEffectList(L:P_literal; CONST raiseErrors:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR sideEffects:T_sideEffects):boolean;
{$i func_defines.inc}
VAR BUILTIN_MYPATH:P_intFuncCallback;
IMPLEMENTATION
USES mySys;
FUNCTION canInterpretAsSideEffectList(L:P_literal; CONST raiseErrors:boolean; CONST location:T_tokenLocation; CONST context:P_context; VAR sideEffects:T_sideEffects):boolean;
  VAR iter:T_arrayOfLiteral;
      seId:P_literal;
      se:T_sideEffect;
      anyMatch:boolean;
      preliminary:T_sideEffects=[];
  begin
    if not(L^.literalType in [lt_stringList,lt_stringSet,lt_emptyList,lt_emptySet]) then begin
      if raiseErrors then context^.raiseError('Invalid specification of side effects. Type is '+L^.typeString+' should be collection of strings',location);
      exit(false);
    end;
    iter:=P_collectionLiteral(L)^.tempIterableList;
    result:=true;
    for seId in iter do begin
      anyMatch:=false;
      for se in T_sideEffect do if C_sideEffectName[se]=P_stringLiteral(seId)^.value then begin
        anyMatch:=true;
        include(preliminary,se);
      end;
      if not(anyMatch) then begin
        if raiseErrors then context^.raiseError('Invalid side effect name '+seId^.toString(),location);
        result:=false;
      end;
    end;
    if result then sideEffects:=preliminary;
    setLength(iter,0);
  end;

PROCEDURE mySleep(CONST argument:P_numericLiteral; CONST argIsEndTime:boolean; CONST context:P_context); inline;
  VAR sleepUntil:double=0;
      sleepInt:longint;
  begin
    if argIsEndTime then sleepUntil:=0 else sleepUntil:=context^.wallclockTime;
    case argument^.literalType of
      lt_smallint: sleepUntil+=P_smallIntLiteral(argument)^.value;
      lt_bigint  : sleepUntil+=P_bigIntLiteral  (argument)^.value.toFloat;
      lt_real    : sleepUntil+=P_realLiteral    (argument)^.value;
    end;
    threadStartsSleeping;
    if getGlobalRunningThreads<settings.cpuCount then context^.getGlobals^.taskQueue.ensurePoolThreads();
    while (context^.wallclockTime<sleepUntil) and (context^.messages^.continueEvaluation) do begin
      sleepInt:=round(900*(sleepUntil-context^.wallclockTime));
      if sleepInt>1000 then sleepInt:=1000;
      if (sleepInt>0) then sleep(sleepInt);
    end;
    threadStopsSleeping;
  end;

FUNCTION sleep_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_real,lt_smallint,lt_bigint]) and context^.checkSideEffects('sleep',tokenLocation,[se_sleep]) then begin
      mySleep(P_numericLiteral(arg0),false,context);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION sleepUntil_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_real,lt_smallint,lt_bigint]) and context^.checkSideEffects('sleepUntil',tokenLocation,[se_sleep]) then begin
      mySleep(P_numericLiteral(arg0),true,context);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION myPath_impl intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      if tokenLocation.package=nil then result:=recycler^.newStringLiteral('<Unknown>')
                                   else result:=recycler^.newStringLiteral(tokenLocation.package^.getPath);
    end;
  end;

FUNCTION executor_impl intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then result:=recycler^.newStringLiteral(paramStr(0));
  end;

FUNCTION hash_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=recycler^.newIntLiteral(arg0^.hash);
  end;

FUNCTION listSideEffects_imp intFuncSignature;
  VAR se:T_sideEffect;
  begin
    if (params=nil) or (params^.size=0) then begin
      result:=recycler^.newListLiteral();
      for se in T_sideEffect do listResult^.appendString(recycler, C_sideEffectName[se]);
    end else result:=nil;
  end;

FUNCTION ord_imp intFuncSignature;
  FUNCTION recurse(CONST x:P_literal):P_literal;
    VAR iter:T_arrayOfLiteral;
        sub:P_literal;
    begin
      case x^.literalType of
        lt_boolean: if P_boolLiteral(x)^.value
                    then exit(recycler^.newIntLiteral(1))
                    else exit(recycler^.newIntLiteral(0));
        lt_smallint,lt_bigint: exit(x^.rereferenced);
        lt_string : if length(P_stringLiteral(x)^.value)=1
                    then exit(recycler^.newIntLiteral(ord(P_stringLiteral(x)^.value[1])))
                    else exit(recycler^.newIntLiteral(-1));
        lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction('ord',tokenLocation,context,recycler);
        lt_void, lt_real: begin
          context^.raiseError('ord can only be applied to booleans, ints and strings',tokenLocation);
          exit(newVoidLiteral);
        end else begin
          if x^.literalType in C_listTypes
          then result:=recycler^.newListLiteral(P_compoundLiteral(x)^.size)
          else result:=recycler^.newSetLiteral (P_compoundLiteral(x)^.size);
          iter:=P_listLiteral(x)^.tempIterableList;
          for sub in iter do if context^.messages^.continueEvaluation then
            collResult^.append(recycler,recurse(sub),false);
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
    result:=recycler^.newMapLiteral(17)^
      .put(recycler,'isFullVersion'  ,{$ifdef fullVersion}true{$else}false{$endif})^
      .put(recycler,'isDebugVersion' ,{$ifdef debugMode}  true{$else}false{$endif})^
      .put(recycler,'is64bit'        ,{$ifdef CPU64}      true{$else}false{$endif})^
      .put(recycler,'compileTime'    ,{$I %DATE%}+' '+{$I %TIME%})^
      .put(recycler,'compilerVersion',{$I %FPCVERSION%}          )^
      .put(recycler,'targetCpu'      ,{$I %FPCTARGET%}           )^
      .put(recycler,'targetOs'       ,{$I %FPCTargetOS%}         )^
      .put(recycler,'version'        ,VERSION                    )^
      .put(recycler,'codeVersion'    ,CODE_HASH                  )^
      .put(recycler,'build'          ,BUILD_NUMBER               )^
      .put(recycler,'flavour'        ,FLAVOUR_STRING             )^
      .put(recycler,'configDir'      ,configDir                  )^
      .put(recycler,'executor'       ,paramStr(0))^
      .put(recycler,'configured_cpus',settings.cpuCount)^
      .put(recycler,'configured_mem' ,settings.memoryLimit)^
      .put(recycler,'used_mem'       ,memoryCleaner.getMemoryUsedInBytes)^
      .put(recycler,'PID'            ,GetProcessID)
    else result:=nil;
  end;

FUNCTION getMnhInfo:T_arrayOfString;
  VAR L:P_literal;
      pseudoLoc:T_tokenLocation=(package:nil; line: 0; column: 0);
      globals:T_evaluationGlobals;
      recycler:P_recycler;
  begin
    recycler:=newRecycler;
    globals.create(nil);
    L:=mnhInfo_imp(nil,pseudoLoc,@globals.primaryContext,recycler);
    result:=serializeToStringList(L,pseudoLoc,nil,su_full);
    recycler^.disposeLiteral(L);
    globals.destroy;
    freeRecycler(recycler);
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleep'       ,@sleep_imp       ,ak_unary  ,[se_sleep]);
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'sleepUntil'  ,@sleepUntil_imp  ,ak_unary  ,[se_sleep]);
  BUILTIN_MYPATH:=
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath'      ,@myPath_impl     ,ak_nullary);
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor'    ,@executor_impl   ,ak_nullary);
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash'        ,@hash_imp        ,ak_unary  );
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'listSideEffects',@listSideEffects_imp,ak_nullary);
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord'         ,@ord_imp         ,ak_unary  );
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'mnhInfo'     ,@mnhInfo_imp     ,ak_nullary);
end.
