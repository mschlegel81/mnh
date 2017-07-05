UNIT mnh_funcs_system;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     Classes,FileUtil,LazFileUtils,LazUTF8,
     myGenerics,{$ifdef Windows}windows,{$endif}mySys,myStringUtil,
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_litVar,
     mnh_funcs,
     mnh_contexts;
IMPLEMENTATION
{$i mnh_func_defines.inc}
VAR builtinLocation_time:T_identifiedInternalFunction;
FUNCTION resetRandom_impl intFuncSignature;
  begin
    result:=nil;
    if (params= nil) or  (params^.size=0) then begin context.getParent^.prng.resetSeed(0); result:=newVoidLiteral; end else
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin context.getParent^.prng.resetSeed(int0^.value); result:=newVoidLiteral; end;
  end;

FUNCTION random_imp intFuncSignature;
  VAR i,count:longint;
  begin
    if (params=nil) or (params^.size=0) then exit(newRealLiteral(random))
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin
      count:=int0^.value;
      if count>0 then begin
        result:=newListLiteral;
        for i:=1 to count do listResult^.appendReal(context.getParent^.prng.realRandom);
        exit(result);
      end;
    end;
    result:=nil;
  end;

FUNCTION intRandom_imp intFuncSignature;
  VAR i,count:longint;
  begin
     if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then exit(newIntLiteral(context.getParent^.prng.intRandom(int0^.value)))
     else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_int) and (arg1^.literalType=lt_int) then begin
      count:=int1^.value;
      if count>=0 then begin
        result:=newListLiteral;
        for i:=1 to count do listResult^.appendInt(context.getParent^.prng.intRandom(int0^.value));
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
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=newVoidLiteral;
      sysutils.beep;
    {$ifdef Windows}
    end else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_int) and (arg1^.literalType=lt_int) then begin
      result:=newVoidLiteral;
      windows.beep(int0^.value,
                   int1^.value);
    {$endif}
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
      SetString(DriveLetter, Buf, StrLen(Buf));
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

FUNCTION logTo_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_boolean)  then begin
      context.adapters^.addOutfile(str0^.value,bool1^.value);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION printTo_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)  then begin
      context.adapters^.setPrintTextFileAdapter(str0^.value);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION setExitCode_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin
      ExitCode:=int0^.value;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION time_imp intFuncSignature;
  VAR res:P_literal;
      t:double=0;

  FUNCTION evaluate(CONST subruleLiteral:P_expressionLiteral; CONST parameters:P_listLiteral=nil):P_literal;
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
      {$ifdef fullVersion}
      context.callStackPush(tokenLocation,@builtinLocation_time);
      {$endif}
      if params^.size=2 then res:=evaluate(P_expressionLiteral(arg0),list1)
                        else res:=evaluate(P_expressionLiteral(arg0));
      {$ifdef fullVersion}
      context.callStackPop();
      {$endif}
      if res<>nil then begin
        result:=newMapLiteral
          ^.put('expression',arg0^.toString)
          ^.put('time',t );
        if res^.literalType<>lt_void then P_mapLiteral(result)^.put('result',res,false)
                                     else disposeLiteral(res);
      end;
    end;
  end;

FUNCTION changeDirectory_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      SetCurrentDir(str0^.value);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

INITIALIZATION
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'resetRandom',@resetRandom_impl        ,[se_writingInternal],ak_variadic  ,'resetRandom(seed:int);//Resets internal PRNG with the given seed');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'random'     ,@random_imp              ,[se_readingInternal,se_writingInternal],ak_variadic  ,'random;//Returns a random value in range [0,1]#random(n);//Returns a list of n random values in range [0,1]');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'intRandom'  ,@intRandom_imp           ,[se_readingInternal,se_writingInternal],ak_variadic_1,'intRandom(k);//Returns an integer random value in range [0,k-1]#random(k,n);//Returns a list of n integer random values in range [0,k-1]');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'systime'    ,@systime_imp             ,[se_readingInternal],ak_nullary   ,'systime;//Returns the current time as a real number');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'beep'       ,@beep_imp                ,[se_sound],ak_variadic  ,'beep;//Makes a beep'{$ifdef Windows}+'#beep(freq:int,duration:int);//Makes a beep of given frequency and duration'{$endif});
  {$ifdef Windows}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'driveInfo'  ,@driveInfo_imp           ,[se_readingExternal],ak_nullary   ,'driveInfo;//Returns info on the computer''''s drives/volumes (Windows only).');
  {$endif}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'getEnv'         ,@getEnv_impl         ,[se_readingExternal],ak_nullary   ,'getEnv;//Returns the current environment variables as a nested list.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'changeDirectory',@changeDirectory_impl,[se_readingInternal,se_writingInternal],ak_unary     ,'changeDirectory(folder:string);//Sets the working directory');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'logTo'          ,@logTo_impl          ,[se_writingInternal,se_outputViaAdapter],ak_binary    ,'logTo(logName:string,appendMode:boolean);//Adds a log with given name and write mode and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printTo'        ,@printTo_impl        ,[se_writingInternal,se_outputViaAdapter],ak_unary     ,'printTo(logName:string);//Adds a log receiving only print messages with given name and and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'setExitCode'    ,@setExitCode_impl    ,[se_writingInternal],ak_unary     ,'setExitCode(code:int);//Sets the exit code of the executable.#//Might be overridden by an evaluation error.');
  builtinLocation_time.create(SYSTEM_BUILTIN_NAMESPACE,'time');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'time',@time_imp,[se_readingInternal],ak_variadic,'time;//Returns an internal time for time difference measurement.#'+
               'time(E:expression);//Evaluates E (without parameters) and returns a nested List with evaluation details.#'+
               'time(E:expression,par:list);//Evaluates E@par and returns a nested List with evaluation details.');
FINALIZATION
  builtinLocation_time.destroy;

end.
