UNIT funcs_system;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     bigint,
     Classes,LazFileUtils,LazUTF8,
     myGenerics,{$ifdef Windows}windows,{$endif}mySys,myStringUtil,
     basicTypes,mnh_constants,
     litVar,
     funcs,
     recyclers,
     contexts;
IMPLEMENTATION
USES out_adapters,mnh_messages;
{$i func_defines.inc}
FUNCTION resetRandom_impl intFuncSignature;
  begin
    if not(context^.checkSideEffects('resetRandom',tokenLocation,[se_readContextState,se_alterContextState])) then exit(nil);
    result:=nil;
    if (params= nil) or  (params^.size=0) then begin context^.getGlobals^.prng.resetSeed(0); result:=newVoidLiteral; end else
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_bigint)   then begin context^.getGlobals^.prng.resetSeed(P_bigIntLiteral(arg0)^.value.getRawBytes); result:=newVoidLiteral; end;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_smallint) then begin context^.getGlobals^.prng.resetSeed(P_smallIntLiteral(arg0)^.value          ); result:=newVoidLiteral; end;
  end;

FUNCTION random_imp intFuncSignature;
  VAR i,count:longint;
  begin
    if not(context^.checkSideEffects('random',tokenLocation,[se_readContextState,se_alterContextState])) then exit(nil);
    if (params=nil) or (params^.size=0) then exit(recycler^.newRealLiteral(context^.getGlobals^.prng.realRandom))
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_smallint) then begin
      count:=int0^.intValue;
      if count>0 then begin
        result:=recycler^.newListLiteral;
        for i:=1 to count do listResult^.appendReal(recycler,context^.getGlobals^.prng.realRandom);
        exit(result);
      end;
    end;
    result:=nil;
  end;

FUNCTION intRandom_imp intFuncSignature;
  VAR i,count:longint;
  FUNCTION singleIntRandom:P_abstractIntLiteral;
    begin
      if arg0^.literalType=lt_bigint
      then result:=recycler^.newIntLiteral(randomInt(@(context^.getGlobals^.prng.dwordRandom),P_bigIntLiteral(arg0)^.value))
      else result:=recycler^.newIntLiteral(context^.getGlobals^.prng.intRandom(P_smallIntLiteral(arg0)^.value));
    end;

  begin
    if not(context^.checkSideEffects('intRandom',tokenLocation,[se_readContextState,se_alterContextState])) then exit(nil);
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint])
    then exit(singleIntRandom)
    else if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_smallint,lt_bigint]) and (arg1^.literalType in [lt_smallint,lt_bigint]) then begin
      count:=int1^.intValue;
      if count>=0 then begin
        result:=recycler^.newListLiteral;
        for i:=1 to count do listResult^.append(recycler,singleIntRandom,false);
        exit(result);
      end;
    end;
    result:=nil;
  end;

FUNCTION systime_imp intFuncSignature;
  begin
    if not(context^.checkSideEffects('systime',tokenLocation,[se_readContextState])) then exit(nil);
    result:=nil;
    if (params=nil) or (params^.size=0)
    then exit(recycler^.newRealLiteral(now));
  end;

FUNCTION beep_imp intFuncSignature;
  begin
    if not(context^.checkSideEffects('beep',tokenLocation,[se_sound])) then exit(nil);
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=newVoidLiteral;
      {$ifdef debugMode} writeln(stdErr,'        DEBUG: beep by function call'); {$endif}
      sysutils.beep;
    end;
  end;
FUNCTION driveInfo_imp intFuncSignature;
  {$ifdef Windows}
  FUNCTION infoForLetter(CONST drive:char):P_literal;
    VAR DriveLetter: ansistring;
        driveType:longint;
        NotUsed:     dword=0;
        VolumeFlags: dword=0;
        VolumeInfo:  array[0..MAX_PATH] of char;
        VolumeSerialNumber: dword;
        Buf: array [0..MAX_PATH] of char;
        infoMap:P_mapLiteral;
    begin
      DriveLetter := drive + ':\';
      driveType:=GetDriveType(PChar(DriveLetter));
      if not(driveType in [DRIVE_REMOVABLE,DRIVE_FIXED,DRIVE_REMOTE,DRIVE_CDROM,DRIVE_RAMDISK])
      then exit(nil);
      infoMap:=recycler^.newMapLiteral(5);
      case driveType of
        DRIVE_REMOVABLE: infoMap^.put(recycler,'type','removable');
        DRIVE_FIXED:     infoMap^.put(recycler,'type','fixed'    );
        DRIVE_REMOTE:    infoMap^.put(recycler,'type','network'  );
        DRIVE_CDROM:     infoMap^.put(recycler,'type','CD_ROM'   );
        DRIVE_RAMDISK:   infoMap^.put(recycler,'type','RAM_disk' );
      end;
      {$WARN 5036 OFF}
      GetVolumeInformation(PChar(DriveLetter),
        Buf, sizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
        VolumeFlags, nil, 0);
      if VolumeFlags=0 then begin
        recycler^.disposeLiteral(infoMap);
        exit(nil);
      end;
      setString(DriveLetter, Buf, StrLen(Buf));
      infoMap^.put(recycler,'serial',VolumeSerialNumber);
      infoMap^.put(recycler,'label' ,DriveLetter);
      infoMap^.put(recycler,'flags', VolumeFlags);
      result:=infoMap;
    end;

  VAR c:char;
      info:P_literal;
  begin
    if not(context^.checkSideEffects('driveInfo',tokenLocation,[se_executingExternal])) then exit(nil);
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=recycler^.newMapLiteral(4);
      for c:='A' to 'Z' do begin
        info:=infoForLetter(c);
        if info<>nil then mapResult^.put(recycler,c,info,false);
      end;
    end;
  end;
  {$else}
  begin
    result:=nil;
    context^.raiseError('Function driveInfo is not avaliable under Linux',tokenLocation);
  end;
  {$endif}

FUNCTION getEnv_impl intFuncSignature;
  VAR envString:ansistring;
      envKey:ansistring;
      envValue:ansistring;
      envTuple:T_arrayOfString;
      inner:P_listLiteral;

  begin
    if not(context^.checkSideEffects('getEnv',tokenLocation,[se_executingExternal])) then exit(nil);
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=recycler^.newMapLiteral(30);
      for envString in getEnvironment do begin
        envTuple:=split(envString,'=');
        envKey:=envTuple[0];
        if length(envTuple)>=2 then begin
          envTuple:=split(envTuple[1],';');
          if length(envTuple)=1 then mapResult^.put(recycler,envKey,envTuple[0]) else begin
            inner:=recycler^.newListLiteral(length(envTuple));
            for envValue in envTuple do inner^.appendString(recycler,envValue);
            mapResult^.put(recycler,envKey,inner,false);
          end;
        end else mapResult^.put(recycler,envKey,newVoidLiteral,false);
      end;
    end;
  end;

FUNCTION setExitCode_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint]) and context^.checkSideEffects('setExitCode',tokenLocation,[se_alterContextState]) then begin
      ExitCode:=int0^.intValue;
      context^.messages^.setUserDefinedExitCode(ExitCode);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION scriptTime_imp intFuncSignature;
  begin
    if not(context^.checkSideEffects('scriptTime',tokenLocation,[se_readContextState])) then exit(nil);
    result:=recycler^.newRealLiteral(context^.wallclockTime);
  end;

{$ifdef fullVersion}VAR timeLoc:P_intFuncCallback;{$endif}
FUNCTION time_imp intFuncSignature;
  VAR res:P_literal;
      t:double=0;

  FUNCTION evaluate(CONST subruleLiteral:P_expressionLiteral; CONST parameters:P_listLiteral=nil):P_literal;
    begin
      t:=context^.wallclockTime;
      result:=subruleLiteral^.evaluate(tokenLocation,context,recycler,parameters).literal;
      t:=context^.wallclockTime-t;
    end;

  begin
    if not(context^.checkSideEffects('time',tokenLocation,[se_readContextState])) then exit(nil);
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_expression) and
      ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) then begin
      {$ifdef fullVersion}
      context^.callStackPush(tokenLocation,builtinFunctionMap.getIntrinsicRuleAsExpression(timeLoc,false),nil);
      {$endif}
      if params^.size=2 then res:=evaluate(P_expressionLiteral(arg0),list1)
                        else res:=evaluate(P_expressionLiteral(arg0));
      {$ifdef fullVersion}
      context^.callStackPop(nil);
      {$endif}
      if res<>nil then begin
        result:=recycler^.newMapLiteral(3)
          ^.put(recycler,'expression',arg0^.toString(100))
          ^.put(recycler,'time',t );
        if res^.literalType<>lt_void then P_mapLiteral(result)^.put(recycler,'result',res,false)
                                     else recycler^.disposeLiteral(res);
      end;
    end;
  end;

FUNCTION changeDirectory_impl intFuncSignature;
  begin
    if not(context^.checkSideEffects('changeDirectory',tokenLocation,[se_alterContextState])) then exit(nil);
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context^.checkSideEffects('setExitCode',tokenLocation,[se_alterContextState])  then begin
      SetCurrentDir(str0^.value);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION callMemoryCleaner_impl intFuncSignature;
  begin
    if not(context^.checkSideEffects('callMemoryCleaner',tokenLocation,[se_alterContextState])) then exit(nil);
    if (params=nil) or (params^.size=0) then begin
      memoryCleaner.callCleanupMethods(3);
      recycler^.cleanupIfPosted;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION assertGuiStarted_impl intFuncSignature;
  begin
    if (gui_started=NO) then begin
      context^.messages^.logGuiNeeded;
      result:=nil;
    end else result:=newVoidLiteral;
  end;

FUNCTION isGuiStarted_impl intFuncSignature;
  begin
    result:=newBoolLiteral(gui_started<>NO);
  end;

FUNCTION showConsole_impl intFuncSignature;
  begin
    result:=newBoolLiteral(showConsole);
  end;

FUNCTION hideConsole_impl intFuncSignature;
  begin
    result:=newBoolLiteral(hideConsole);
  end;

FUNCTION freeConsole_impl intFuncSignature;
  begin
    detachFromConsole;
    result:=newVoidLiteral;
  end;

FUNCTION getCPULoadPercentage_impl intFuncSignature;
  begin
    {$ifdef Windows}
    if not(context^.checkSideEffects('getCPULoadPercentage',tokenLocation,[se_executingExternal])) then exit(nil);
    result:=recycler^.newIntLiteral(mySys.getCPULoadPercentage);
    {$else}
    context^.raiseError('Implemented for Windows flavours only',tokenLocation);
    result:=nil;
    {$endif}
  end;

FUNCTION getTaskInfo_impl intFuncSignature;
  {$ifdef Windows}
  VAR info:T_taskInfoArray;
      i   :T_taskInfo;
  {$endif}
  begin
    {$ifdef Windows}
    if not(context^.checkSideEffects('getTaskInfo',tokenLocation,[se_executingExternal])) then exit(nil);
    info:=mySys.getTaskInfo;
    result:=recycler^.newListLiteral(length(info));
    for i in info do listResult^.append(recycler,recycler^.newMapLiteral(6)
      ^.put(recycler,'caption',i.caption)
      ^.put(recycler,'commandLine',i.commandLine)
      ^.put(recycler,'PID',i.pid)
      ^.put(recycler,'parentPID',i.parentPID)
      ^.put(recycler,'workingSetSize',i.workingSetSize)
      ^.put(recycler,'userModeTime',i.userModeTime),false);
    {$else}
    context^.raiseError('Implemented for Windows flavours only',tokenLocation);
    result:=nil;
    {$endif}
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'resetRandom',@resetRandom_impl        ,ak_variadic  ,[se_readContextState,se_alterContextState]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'random'     ,@random_imp              ,ak_variadic  ,[se_readContextState,se_alterContextState]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'intRandom'  ,@intRandom_imp           ,ak_variadic_1,[se_readContextState,se_alterContextState]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'systime'    ,@systime_imp             ,ak_nullary   ,[se_readContextState]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'beep'       ,@beep_imp                ,ak_variadic  ,[se_sound]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'driveInfo'  ,@driveInfo_imp           ,ak_nullary   ,[se_executingExternal]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'getEnv'         ,@getEnv_impl         ,ak_nullary   ,[se_executingExternal]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'changeDirectory',@changeDirectory_impl,ak_unary     ,[se_alterContextState]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'setExitCode'    ,@setExitCode_impl    ,ak_unary     ,[se_alterContextState]);
  {$ifdef fullVersion}timeLoc:={$endif}
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'scriptTime',@scriptTime_imp,ak_variadic,[se_readContextState]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'time',@time_imp,ak_variadic_1,[se_readContextState]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'callMemoryCleaner',@callMemoryCleaner_impl,ak_nullary,[se_alterContextState]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'assertGuiStarted',@assertGuiStarted_impl,ak_nullary);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'isGuiStarted',@isGuiStarted_impl,ak_nullary);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'showConsole',@showConsole_impl,ak_nullary);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'hideConsole',@hideConsole_impl,ak_nullary);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'freeConsole',@freeConsole_impl,ak_nullary);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'getCPULoadPercentage',@getCPULoadPercentage_impl,ak_nullary,[se_executingExternal]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'getTaskInfo',@getTaskInfo_impl,ak_nullary,[se_executingExternal]);
end.
