UNIT mnh_funcs_system;
INTERFACE
{$WARN 5024 OFF}
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,myGenerics,
     sysutils, Classes,fphttpclient,FileUtil,{$ifdef Windows}windows,{$endif}mySys,myStringUtil,mnh_contexts,lclintf,
     LazFileUtils,LazUTF8,mnh_html;
IMPLEMENTATION
{$MACRO ON}
{$define str0:=P_stringLiteral(params^.value(0))}
{$define str1:=P_stringLiteral(params^.value(1))}
{$define str2:=P_stringLiteral(params^.value(2))}
{$define list0:=P_listLiteral(params^.value(0))}
{$define list1:=P_listLiteral(params^.value(1))}
{$define int0:=P_intLiteral(params^.value(0))}
{$define int1:=P_intLiteral(params^.value(1))}
{$define int2:=P_intLiteral(params^.value(2))}
{$define real2:=P_realLiteral(params^.value(2))}
{$define arg0:=params^.value(0)}
{$define arg1:=params^.value(1)}
{$define arg2:=params^.value(2)}

FUNCTION resetRandom_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin randseed:=0; result:=newVoidLiteral; end else
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin randseed:=int0^.value; result:=newVoidLiteral; end;
  end;

FUNCTION random_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR i,count:longint;
  begin
    if (params=nil) or (params^.size=0) then exit(newRealLiteral(random))
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin
      count:=int0^.value;
      if count>0 then begin
        result:=newListLiteral;
        for i:=1 to count do P_listLiteral(result)^.appendReal(random);
        exit(result);
      end;
    end;
    result:=nil;
  end;

FUNCTION intRandom_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR i,count:longint;
  begin
     if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then exit(newIntLiteral(random(int0^.value)))
     else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_int) and (arg1^.literalType=lt_int) then begin
      count:=int1^.value;
      if count>=0 then begin
        result:=newListLiteral;
        for i:=1 to count do P_listLiteral(result)^.appendInt(random(int0^.value));
        exit(result);
      end;
    end;
    result:=nil;
  end;

FUNCTION systime_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then exit(newRealLiteral(now));
  end;

FUNCTION httpGet_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR resultText:ansistring;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      try
        resultText:=TFPCustomHTTPClient.SimpleGet(str0^.value);
      except
        on E : Exception do begin
          resultText:='';
          context.adapters^.raiseCustomMessage(mt_el5_systemError,'httpGet failed with:'+E.message,tokenLocation);
        end;
      end;
      result:=newStringLiteral(resultText);
    end;
  end;

FUNCTION openUrl_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(OpenURL(str0^.value))
    else result:=nil;
  end;

FUNCTION beep_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
FUNCTION driveInfo_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION infoForLetter(CONST drive:char):P_literal;
    VAR DriveLetter: ansistring;
        driveType:longint;
        NotUsed:     dword=0;
        VolumeFlags: dword=0;
        VolumeInfo:  array[0..MAX_PATH] of char;
        VolumeSerialNumber: dword;
        Buf: array [0..MAX_PATH] of char;
        infoPair:P_listLiteral;
        infoMap:P_listLiteral;
    begin
      DriveLetter := drive + ':\';
      driveType:=GetDriveType(PChar(DriveLetter));
      if driveType in [DRIVE_REMOVABLE,DRIVE_FIXED,DRIVE_REMOTE,DRIVE_CDROM,DRIVE_RAMDISK] then begin
        result:=newListLiteral;
      end else exit(newVoidLiteral);
      infoMap:=newListLiteral;
      P_listLiteral(result)^.appendString(drive)^.append(infoMap,false);

      infoPair:=newListLiteral;
      infoPair^.appendString('type');
      case driveType of
        DRIVE_REMOVABLE: infoPair^.appendString('removable');
        DRIVE_FIXED:     infoPair^.appendString('fixed'    );
        DRIVE_REMOTE:    infoPair^.appendString('network'  );
        DRIVE_CDROM:     infoPair^.appendString('CD_ROM'   );
        DRIVE_RAMDISK:   infoPair^.appendString('RAM_disk' );
      end;
      infoMap^.append(infoPair,false);
      {$WARN 5036 OFF}
      GetVolumeInformation(PChar(DriveLetter),
        Buf, sizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
        VolumeFlags, nil, 0);
      SetString(DriveLetter, Buf, StrLen(Buf));

      infoMap^.append(
        newListLiteral^.
        appendString('serial')^.
        appendInt(VolumeSerialNumber),false);

      infoMap^.append(
        newListLiteral^.
        appendString('label')^.
        appendString(DriveLetter),false);
    end;

  VAR c:char;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=newListLiteral;
      for c:='A' to 'Z' do P_listLiteral(result)^.append(infoForLetter(c),false);
    end;
  end;
{$endif}

FUNCTION getEnv_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR e:T_arrayOfString;
      i:longint;
  FUNCTION environmentPair(CONST envString:ansistring):P_listLiteral;
    VAR env:T_arrayOfString;
        inner:P_listLiteral;
        k:longint;
    begin
      env:=split(envString,'=');
      result:=newListLiteral^.appendString(env[0]);
      if length(env)>=2 then begin
        env:=split(env[1],';');
        if length(env)=1 then exit(result^.appendString(env[0])) else begin
          inner:=newListLiteral;
          for k:=0 to length(env)-1 do inner^.appendString(env[k]);
          result:=result^.append(inner,false);
        end;
      end;
    end;

  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      e:=getEnvironment;
      result:=newListLiteral;
      for i:=0 to length(e)-1 do P_listLiteral(result)^.append(environmentPair(e[i]),false);
      setLength(e,0);
    end;
  end;

FUNCTION isGuiActive_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params=nil) or (params^.size=0) then result:=newBoolLiteral({$ifdef fullVersion}gui_started{$else}false{$endif})
                                        else result:=nil;
  end;

FUNCTION newCollectingOutAdapter:P_collectingOutAdapter;
  begin new(result,create(at_unknown,C_collectAllOutputBehavior)); end;

VAR collector: specialize G_lazyVar<P_collectingOutAdapter> ;
FUNCTION collectOutput_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params=nil) or (params^.size=0) then begin
      if collector.isObtained
      then collector.value^.clearMessages
      else context.adapters^.addOutAdapter(collector.value,true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION collectedOutput_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params=nil) or (params^.size=0)
    then result:=messagesToLiteralForSandbox(collector.value^.storedMessages)
    else result:=nil;
  end;

FUNCTION logTo_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_boolean)  then begin
      addOutfile(context.adapters^,str0^.value,P_boolLiteral(arg1)^.value);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION printTo_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)  then begin
      context.adapters^.setPrintTextFileAdapter(str0^.value);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION setExitCode_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin
      ExitCode:=int0^.value;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

INITIALIZATION
  collector.create(@newCollectingOutAdapter,nil);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'resetRandom',@resetRandom_impl,'resetRandom(seed:int);#Resets internal PRNG with the given seed');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'random',@random_imp,'random;#Returns a random value in range [0,1]#random(n);Returns a list of n random values in range [0,1]');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'intRandom',@intRandom_imp,'intRandom(k);#Returns an integer random value in range [0,k-1]#random(k,n);Returns a list of n integer random values in range [0,k-1]');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'systime',@systime_imp,'systime;#Returns the current time as a real number');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'beep',@beep_imp,'beep;#Makes a beep'{$ifdef WINDOWS}+'#beep(freq:int,duration:int);#Makes a beep of given frequency and duration'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'httpGet',@httpGet_imp,'httpGet(URL:string);#Retrieves the contents of the given URL and returns them as a string');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'openUrl',@openUrl_imp,'openUrl(URL:string);#Opens the URL in the default browser');
  {$ifdef Windows}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'driveInfo',@driveInfo_imp,'driveInfo;#Returns info on the computer''''s drives/volumes.');
  {$endif}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'getEnv',@getEnv_impl,'getEnv;#Returns the current environment variables as a nested list.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'isGuiActive',@isGuiActive_impl,'isGuiActive;#Returns true if GUI is showing and false otherwise.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'collectOutput',@collectOutput_impl,'collectOutput;#Starts collecting output messages to be accessed via function collectedOutput');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'collectedOutput',@collectedOutput_impl,'collectedOutput;#Returns messages collected since the last call of collectOutput.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'logTo',@logTo_impl,'logTo(logName:string,appendMode:boolean);#Adds a log with given name and write mode and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printTo',@printTo_impl,'printTo(logName:string);#Adds a log receiving only print messages with given name and and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'setExitCode',@setExitCode_impl,'setExitCode(code:int);#Sets the exit code of the executable.#Might be overridden by an evaluation error.');
FINALIZATION
  collector.destroy;
end.
