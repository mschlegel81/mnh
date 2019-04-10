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
{$i func_defines.inc}
FUNCTION resetRandom_impl intFuncSignature;
  begin
    if not(context.checkSideEffects('resetRandom',tokenLocation,[se_alterContextState])) then exit(nil);
    result:=nil;
    if (params= nil) or  (params^.size=0) then begin context.getGlobals^.prng.resetSeed(0); result:=newVoidLiteral; end else
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_bigint)   then begin context.getGlobals^.prng.resetSeed(P_bigIntLiteral(arg0)^.value.getRawBytes); result:=newVoidLiteral; end;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_smallint) then begin context.getGlobals^.prng.resetSeed(P_smallIntLiteral(arg0)^.value          ); result:=newVoidLiteral; end;
  end;

FUNCTION random_imp intFuncSignature;
  VAR i,count:longint;
  begin
    if not(context.checkSideEffects('random',tokenLocation,[se_alterContextState])) then exit(nil);
    if (params=nil) or (params^.size=0) then exit(newRealLiteral(context.getGlobals^.prng.realRandom))
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_smallint) then begin
      count:=int0^.intValue;
      if count>0 then begin
        result:=newListLiteral;
        for i:=1 to count do listResult^.appendReal(context.getGlobals^.prng.realRandom);
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
      then result:=newIntLiteral(randomInt(@(context.getGlobals^.prng.dwordRandom),P_bigIntLiteral(arg0)^.value))
      else result:=newIntLiteral(context.getGlobals^.prng.intRandom(P_smallIntLiteral(arg0)^.value));
    end;

  begin
    if not(context.checkSideEffects('intRandom',tokenLocation,[se_alterContextState])) then exit(nil);

    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint])
    then exit(singleIntRandom)
    else if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_smallint,lt_bigint]) and (arg1^.literalType in [lt_smallint,lt_bigint]) then begin
      count:=int1^.intValue;
      if count>=0 then begin
        result:=newListLiteral;
        for i:=1 to count do listResult^.append(singleIntRandom,false);
        exit(result);
      end;
    end;
    result:=nil;
  end;

FUNCTION systime_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then exit(newRealLiteral(now));
  end;

FUNCTION beep_imp intFuncSignature;
  begin
    if not(context.checkSideEffects('beep',tokenLocation,[se_sound])) then exit(nil);
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=newVoidLiteral;
      {$ifdef debugMode} writeln(stdErr,'        DEBUG: beep by function call'); {$endif}
      sysutils.beep;
    end;
  end;
{$ifdef Windows}
FUNCTION driveInfo_imp intFuncSignature;
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
      infoMap:=newMapLiteral;
      case driveType of
        DRIVE_REMOVABLE: infoMap^.put('type','removable');
        DRIVE_FIXED:     infoMap^.put('type','fixed'    );
        DRIVE_REMOTE:    infoMap^.put('type','network'  );
        DRIVE_CDROM:     infoMap^.put('type','CD_ROM'   );
        DRIVE_RAMDISK:   infoMap^.put('type','RAM_disk' );
      end;
      {$WARN 5036 OFF}
      GetVolumeInformation(PChar(DriveLetter),
        Buf, sizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
        VolumeFlags, nil, 0);
      setString(DriveLetter, Buf, StrLen(Buf));
      infoMap^.put('serial',VolumeSerialNumber);
      infoMap^.put('label' ,DriveLetter);
      result:=infoMap;
    end;

  VAR c:char;
      info:P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=newMapLiteral;
      for c:='A' to 'Z' do begin
        info:=infoForLetter(c);
        if info<>nil then mapResult^.put(c,info,false);
      end;
    end;
  end;
{$endif}

FUNCTION getEnv_impl intFuncSignature;
  VAR envString:ansistring;
      envKey:ansistring;
      envValue:ansistring;
      envTuple:T_arrayOfString;
      inner:P_listLiteral;

  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=newMapLiteral;
      for envString in getEnvironment do begin
        envTuple:=split(envString,'=');
        envKey:=envTuple[0];
        if length(envTuple)>=2 then begin
          envTuple:=split(envTuple[1],';');
          if length(envTuple)=1 then mapResult^.put(envKey,envTuple[0]) else begin
            inner:=newListLiteral(length(envTuple));
            for envValue in envTuple do inner^.appendString(envValue);
            mapResult^.put(envKey,inner,false);
          end;
        end else mapResult^.put(envKey,newVoidLiteral,false);
      end;
    end;
  end;

FUNCTION setExitCode_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint]) and context.checkSideEffects('setExitCode',tokenLocation,[se_alterContextState]) then begin
      ExitCode:=int0^.intValue;
      context.messages^.setUserDefinedExitCode(ExitCode);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

{$ifdef fullVersion}VAR timeLoc:P_intFuncCallback;{$endif}
FUNCTION time_imp intFuncSignature;
  VAR res:P_literal;
      t:double=0;

  FUNCTION evaluate(CONST subruleLiteral:P_expressionLiteral; CONST parameters:P_listLiteral=nil):P_literal;
    begin
      t:=context.wallclockTime;
      result:=subruleLiteral^.evaluate(tokenLocation,@context,@recycler,parameters).literal;
      t:=context.wallclockTime-t;
    end;

  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then exit(newRealLiteral(context.wallclockTime))
    else if (params^.size>=1) and (arg0^.literalType=lt_expression) and
      ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) then begin
      {$ifdef fullVersion}
      context.callStackPush(tokenLocation,getIntrinsicRuleAsExpression(timeLoc),nil);
      {$endif}
      if params^.size=2 then res:=evaluate(P_expressionLiteral(arg0),list1)
                        else res:=evaluate(P_expressionLiteral(arg0));
      {$ifdef fullVersion}
      context.callStackPop(nil);
      {$endif}
      if res<>nil then begin
        result:=newMapLiteral
          ^.put('expression',arg0^.toString(100))
          ^.put('time',t );
        if res^.literalType<>lt_void then P_mapLiteral(result)^.put('result',res,false)
                                     else disposeLiteral(res);
      end;
    end;
  end;

FUNCTION changeDirectory_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context.checkSideEffects('setExitCode',tokenLocation,[se_alterContextState])  then begin
      SetCurrentDir(str0^.value);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

INITIALIZATION
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'resetRandom',@resetRandom_impl        ,ak_variadic  {$ifdef fullVersion},'resetRandom(seed:Int);//Resets internal PRNG with the given seed'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'random'     ,@random_imp              ,ak_variadic  {$ifdef fullVersion},'random;//Returns a random value in range [0,1]#random(n);//Returns a list of n random values in range [0,1]'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'intRandom'  ,@intRandom_imp           ,ak_variadic_1{$ifdef fullVersion},'intRandom(k>1);//Returns an integer random value in range [0,k-1]#intRandom(k>1,n>0);//Returns a list of n integer random values in range [0,k-1]'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'systime'    ,@systime_imp             ,ak_nullary   {$ifdef fullVersion},'systime;//Returns the current time as a real number'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'beep'       ,@beep_imp                ,ak_variadic  {$ifdef fullVersion},'beep;//Makes a beep'{$endif});
  {$ifdef Windows}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'driveInfo'  ,@driveInfo_imp           ,ak_nullary   {$ifdef fullVersion},'driveInfo;//Returns info on the computer''''s drives/volumes (Windows only).'{$endif});
  {$endif}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'getEnv'         ,@getEnv_impl         ,ak_nullary   {$ifdef fullVersion},'getEnv;//Returns the current environment variables as a nested list.'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'changeDirectory',@changeDirectory_impl,ak_unary     {$ifdef fullVersion},'changeDirectory(folder:String);//Sets the working directory'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'setExitCode'    ,@setExitCode_impl    ,ak_unary     {$ifdef fullVersion},'setExitCode(code:Int);//Sets the exit code of the executable.#//Might be overridden by an evaluation error.'{$endif});
  {$ifdef fullVersion}timeLoc:={$endif}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'time',@time_imp,ak_variadic{$ifdef fullVersion},'time;//Returns an internal time for time difference measurement.#'+
               'time(E:expression);//Evaluates E (without parameters) and returns a nested List with evaluation details.#'+
               'time(E:expression,par:list);//Evaluates E@par and returns a nested List with evaluation details.'{$endif});

end.
