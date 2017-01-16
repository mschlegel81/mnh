UNIT mnh_funcs_system;
INTERFACE
{$WARN 5024 OFF}
USES mnh_basicTypes,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,myGenerics,
     sysutils, Classes,FileUtil,{$ifdef Windows}windows,{$endif}mySys,myStringUtil,mnh_contexts,
     LazFileUtils,LazUTF8;
IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION resetRandom_impl intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin randseed:=0; result:=newVoidLiteral; end else
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin randseed:=int0^.value; result:=newVoidLiteral; end;
  end;

FUNCTION random_imp intFuncSignature;
  VAR i,count:longint;
  begin
    if (params=nil) or (params^.size=0) then exit(newRealLiteral(random))
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin
      count:=int0^.value;
      if count>0 then begin
        result:=newListLiteral;
        for i:=1 to count do listResult^.appendReal(random);
        exit(result);
      end;
    end;
    result:=nil;
  end;

FUNCTION intRandom_imp intFuncSignature;
  VAR i,count:longint;
  begin
     if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then exit(newIntLiteral(random(int0^.value)))
     else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_int) and (arg1^.literalType=lt_int) then begin
      count:=int1^.value;
      if count>=0 then begin
        result:=newListLiteral;
        for i:=1 to count do listResult^.appendInt(random(int0^.value));
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
      if driveType in [DRIVE_REMOVABLE,DRIVE_FIXED,DRIVE_REMOTE,DRIVE_CDROM,DRIVE_RAMDISK]
      then result:=newListLiteral
      else exit(nil);
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
  VAR e:T_arrayOfString;
      envString:ansistring;
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
          if length(envTuple)=1 then mapResult^.put(envKey,envTuple[1]) else begin
            inner:=newListLiteral(length(envTuple));
            for envValue in envTuple do inner^.appendString(envValue);
            mapResult^.put(envKey,inner,false);
          end;
        end else mapResult^.put(envKey,newVoidLiteral,false);
      end;
    end;
  end;

FUNCTION newCollectingOutAdapter:P_collectingOutAdapter;
  begin new(result,create(at_unknown,C_collectAllOutputBehavior)); end;

VAR collector: specialize G_lazyVar<P_collectingOutAdapter> ;
FUNCTION collectOutput_impl intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      if collector.isObtained
      then collector.value^.clear
      else context.adapters^.addOutAdapter(collector.value,true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION collectedOutput_impl intFuncSignature;
  begin
    if (params=nil) or (params^.size=0)
    then result:=messagesToLiteralForSandbox(collector.value^.storedMessages)
    else result:=nil;
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

INITIALIZATION
  collector.create(@newCollectingOutAdapter,nil);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'resetRandom',@resetRandom_impl        ,false,ak_variadic  ,'resetRandom(seed:int);#Resets internal PRNG with the given seed');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'random'     ,@random_imp              ,false,ak_variadic  ,'random;#Returns a random value in range [0,1]#random(n);Returns a list of n random values in range [0,1]');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'intRandom'  ,@intRandom_imp           ,false,ak_variadic_1,'intRandom(k);#Returns an integer random value in range [0,k-1]#random(k,n);Returns a list of n integer random values in range [0,k-1]');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'systime'    ,@systime_imp             ,false,ak_nullary   ,'systime;#Returns the current time as a real number');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'beep'       ,@beep_imp                ,false,ak_variadic  ,'beep;#Makes a beep'{$ifdef Windows}+'#beep(freq:int,duration:int);#Makes a beep of given frequency and duration'{$endif});
  {$ifdef Windows}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'driveInfo'  ,@driveInfo_imp           ,false,ak_nullary   ,'driveInfo;#Returns info on the computer''''s drives/volumes (Windows only).');
  {$endif}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'getEnv'         ,@getEnv_impl         ,false,ak_nullary   ,'getEnv;#Returns the current environment variables as a nested list.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'collectOutput'  ,@collectOutput_impl  ,false,ak_nullary   ,'collectOutput;#Starts collecting output messages to be accessed via function collectedOutput');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'collectedOutput',@collectedOutput_impl,false,ak_nullary   ,'collectedOutput;#Returns messages collected since the last call of collectOutput.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'logTo'          ,@logTo_impl          ,false,ak_binary    ,'logTo(logName:string,appendMode:boolean);#Adds a log with given name and write mode and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printTo'        ,@printTo_impl        ,false,ak_unary     ,'printTo(logName:string);#Adds a log receiving only print messages with given name and and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'setExitCode'    ,@setExitCode_impl    ,false,ak_unary     ,'setExitCode(code:int);#Sets the exit code of the executable.#Might be overridden by an evaluation error.');
FINALIZATION
  collector.destroy;
end.
