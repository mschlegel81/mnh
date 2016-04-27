UNIT mnh_funcs_system;
INTERFACE
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,myGenerics,mnh_fileWrappers,
     sysutils, Classes,Process,fphttpclient,FileUtil,windows,mySys,myStringUtil,mnh_contexts,lclintf,
     LazFileUtils,LazUTF8;
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
      if count>0 then begin
        result:=newListLiteral;
        for i:=1 to count do P_listLiteral(result)^.appendInt(random(int0^.value));
        exit(result);
      end;
    end;
    result:=nil;
  end;


FUNCTION filesOrDirs_impl(CONST pathOrPathList:P_literal; CONST filesAndNotFolders,recurseSubDirs:boolean):P_listLiteral;
  VAR i,j:longint;
      found:T_arrayOfString;
  FUNCTION searchString(CONST index:longint):ansistring;
    begin
      if pathOrPathList^.literalType=lt_string
      then result:=P_stringLiteral(pathOrPathList)^.value
      else result:=P_stringLiteral(P_listLiteral(pathOrPathList)^.value(index))^.value;
      if not(filesAndNotFolders) and recurseSubDirs and (pos('*',result)<=0) then result:=result+DirectorySeparator+'*';
    end;

  begin
    result:=newListLiteral;
    if pathOrPathList^.literalType=lt_string then begin
      found:=find(searchString(0),filesAndNotFolders,recurseSubDirs);
      if recurseSubDirs and DirectoryExists(P_stringLiteral(pathOrPathList)^.value)
                                        then result^.append(pathOrPathList,true,nullAdapter);
      for i:=0 to length(found)-1 do result^.appendString(replaceAll(found[i],'\','/'));
    end else if pathOrPathList^.literalType=lt_stringList then begin
      for j:=0 to P_listLiteral(pathOrPathList)^.size-1 do begin
        found:=find(searchString(j),filesAndNotFolders,recurseSubDirs);
        if recurseSubDirs and DirectoryExists(P_stringLiteral(P_listLiteral(pathOrPathList)^.value(j))^.value)
                                          then result^.append(P_listLiteral(pathOrPathList)^.value(j),true,nullAdapter);
        for i:=0 to length(found)-1 do result^.appendString(replaceAll(found[i],'\','/'));
      end;
    end;
  end;

FUNCTION files_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string, lt_stringList, lt_emptyList])
    then result:=filesOrDirs_impl(arg0,true,false);
  end;

FUNCTION folders_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string, lt_stringList, lt_emptyList])
    then result:=filesOrDirs_impl(arg0,false,false);
  end;

FUNCTION allFolders_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string, lt_stringList, lt_emptyList])
    then result:=filesOrDirs_impl(arg0,false,true);
  end;

FUNCTION fileExists_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(fileExists(str0^.value));
  end;

FUNCTION folderExists_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(DirectoryExists(str0^.value));
  end;


FUNCTION fileContents_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR accessed:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=newStringLiteral(fileContent(str0^.value,accessed));
      if not(accessed) then begin
        context.adapters^.raiseWarning('File "'+str0^.value+'" cannot be accessed',tokenLocation);
        disposeLiteral(result);
        result:=newStringLiteral('');
      end;
    end;
  end;

FUNCTION fileLines_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR accessed:boolean;
      L:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      L:=fileLines(str0^.value,accessed);
      result:=newListLiteral;
      for i:=0 to length(L)-1 do P_listLiteral(result)^.appendString(L[i]);
      if not(accessed) then begin
        context.adapters^.raiseWarning('File "'+str0^.value+'" cannot be accessed',tokenLocation);
        disposeLiteral(result);
        result:=newListLiteral;
      end;
    end else if (params<>nil) and (params^.size=3) and
                (arg0^.literalType=lt_string) and
                (arg1^.literalType=lt_int) and
                (arg2^.literalType=lt_int) then begin
      L:=fileLines(str0^.value,
                   int1^.value,
                   int2^.value,accessed);
      result:=newListLiteral;
      for i:=0 to length(L)-1 do P_listLiteral(result)^.appendString(L[i]);
      if not(accessed) then begin
        context.adapters^.raiseWarning('File "'+str0^.value+'" cannot be accessed',tokenLocation);
        disposeLiteral(result);
        result:=newListLiteral;
      end;
    end;
  end;

FUNCTION writeFile_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR ok:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string)
                                          and (arg1^.literalType=lt_string) then begin
      ok:=mnh_fileWrappers.writeFile(UTF8ToWinCP(str0^.value),
                                     str1^.value);
      result:=newBoolLiteral(ok);
      if not(ok) then context.adapters^.raiseWarning('File "'+str0^.value+'" cannot be accessed',tokenLocation);
    end;
  end;

FUNCTION writeFileLines_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR ok:boolean;
      L:T_arrayOfString;
      i:longint;
      sep:string;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.size<=3)
       and (arg0^.literalType=lt_string)
       and (arg1^.literalType in [lt_stringList,lt_emptyList])
       and ((params^.size=2) or (arg2^.literalType=lt_string)) then begin
      if params^.size=3 then sep:=str2^.value
                        else sep:='';
      setLength(L,list1^.size);
      for i:=0 to length(L)-1 do L[i]:=P_stringLiteral(list1^.value(i))^.value;
      ok:=writeFileLines(UTF8ToWinCP(str0^.value),L,sep);
      result:=newBoolLiteral(ok);
      if not(ok) then context.adapters^.raiseWarning('File "'+str0^.value+'" cannot be accessed',tokenLocation);
    end;
  end;


FUNCTION execSync_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION runCommand(CONST executable: ansistring; CONST parameters: T_arrayOfString; OUT output: TStringList): boolean;
    CONST
      READ_BYTES = 2048;
    VAR
      memStream: TMemoryStream;
      tempProcess: TProcess;
      n: longint;
      BytesRead: longint;
      sleepTime: longint = 1;
    begin
      memStream := TMemoryStream.create;
      BytesRead := 0;
      tempProcess := TProcess.create(nil);
      tempProcess.executable := UTF8ToWinCP(executable);
      for n := 0 to length(parameters)-1 do
        tempProcess.parameters.add(UTF8ToWinCP(parameters[n]));
      tempProcess.options := [poUsePipes, poStderrToOutPut];
      tempProcess.ShowWindow := swoHIDE;
      try
        tempProcess.execute;
        tempProcess.CloseInput;
        while tempProcess.running and context.adapters^.noErrors do begin
          memStream.SetSize(BytesRead+READ_BYTES);
          n := tempProcess.output.read((memStream.memory+BytesRead)^, READ_BYTES);
          if n>0 then begin sleepTime:=1; inc(BytesRead, n); end
                 else begin inc(sleepTime); sleep(sleepTime); end;
        end;
        if tempProcess.running then tempProcess.Terminate(999);
        repeat
          memStream.SetSize(BytesRead+READ_BYTES);
          n := tempProcess.output.read((memStream.memory+BytesRead)^, READ_BYTES);
          if n>0 then inc(BytesRead, n);
        until n<=0;
        result := (tempProcess.exitStatus = 0);
      except
        result := false;
      end;
      tempProcess.free;
      memStream.SetSize(BytesRead);
      output := TStringList.create;
      output.LoadFromStream(memStream);
      memStream.free;
    end;

  VAR executable:ansistring;
      cmdLinePar:T_arrayOfString;
      output:TStringList;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string)
      and ((params^.size=1) or (params^.size=2) and (arg1^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_flatList])) then begin
      setLength(cmdLinePar,0);
      executable:=str0^.value;
      if params^.size=2 then begin
        setLength(cmdLinePar,list1^.size);
        for i:=0 to list1^.size-1 do begin
          cmdLinePar[i]:=P_scalarLiteral(list1^.value(i))^.stringForm;
        end;
      end;
      runCommand(executable,
                 cmdLinePar,
                 output);
      result:=newListLiteral;
      for i:=0 to output.count-1 do P_listLiteral(result)^.appendString(ConsoleToUTF8(output[i]));
      output.free;
    end;
  end;

FUNCTION execAsyncOrPipeless(CONST params:P_listLiteral; CONST doAsynch:boolean):P_literal;
  VAR executable:ansistring;
      cmdLinePar:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string)
      and ((params^.size=1) or (params^.size=2) and (arg1^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_flatList])) then begin
      setLength(cmdLinePar,0);
      executable:=UTF8ToWinCP(str0^.value);
      if params^.size=2 then begin
        setLength(cmdLinePar,list1^.size);
        for i:=0 to list1^.size-1 do
          cmdLinePar[i]:=UTF8ToWinCP(P_scalarLiteral(list1^.value(i))^.stringForm);
      end;
      showConsole;
      runCommandAsyncOrPipeless(executable,
                                cmdLinePar,doAsynch);
      hideConsole;
      result:=newBoolLiteral(true);
    end;
  end;

FUNCTION execAsync_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=execAsyncOrPipeless(params,true);
  end;

FUNCTION execPipeless_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=execAsyncOrPipeless(params,false);
  end;

FUNCTION systime_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then exit(newRealLiteral(now));
  end;


FUNCTION deleteFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=newBoolLiteral(DeleteFileUTF8(str0^.value));
    end;
  end;

FUNCTION deleteDir_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=newBoolLiteral(DeleteDirectory(str0^.value,false));
    end;
  end;

FUNCTION copyFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string)  then begin
      ensurePath(str1^.value);
      result:=newBoolLiteral(
      FileUtil.CopyFile(str0^.value,str1^.value,true));
    end;
  end;

FUNCTION moveFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string)  then begin
      ensurePath(str1^.value);
      result:=newBoolLiteral(
      RenameFile(str0^.value,str1^.value));
    end;
  end;

FUNCTION fileInfo_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR time:double;
      size:int64;
      isExistent,
      isArchive,
      isDirectory,
      isReadOnly,
      isSystem,
      isHidden:boolean;
      //-------------------------
      resultAsList:P_listLiteral;
      attributeList:P_listLiteral;

  PROCEDURE appendKeyValuePair(CONST key:string; CONST value:P_literal);
    begin
      resultAsList^.append(
        newListLiteral^.
        appendString(key)^.
        append(value,false,context.adapters^),false,context.adapters^);
    end;

  VAR i:longint;
      tmpParam:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      getFileInfo(str0^.value,time,size,isExistent,isArchive,isDirectory,isReadOnly,isSystem,isHidden);
      resultAsList:=newListLiteral;
      appendKeyValuePair('exists',newBoolLiteral(isExistent));
      if isExistent then begin
        if size>=0 then appendKeyValuePair('size',newIntLiteral(size));
        if time<>-1 then appendKeyValuePair('time',newRealLiteral(time));
        attributeList:=newListLiteral;
        if isArchive   then attributeList^.appendString('archive'  );
        if isDirectory then attributeList^.appendString('directory');
        if isReadOnly  then attributeList^.appendString('readonly' );
        if isSystem    then attributeList^.appendString('system'   );
        if isHidden    then attributeList^.appendString('hidden'   );
        appendKeyValuePair('attributes',attributeList);
      end;
      result:=resultAsList;
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_stringList,lt_emptyList]) then begin
      result:=newListLiteral;
      for i:=0 to list0^.size-1 do begin
        tmpParam:=newOneElementListLiteral(list0^.value(i),true,context.adapters^);
        P_listLiteral(result)^.append(fileInfo_imp(tmpParam,tokenLocation,context),false,context.adapters^);
        disposeLiteral(tmpParam);
      end;
    end;
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
    end else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_int) and (arg1^.literalType=lt_int) then begin
      result:=newVoidLiteral;
      windows.beep(int0^.value,
                   int1^.value);
    end;
  end;

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
      P_listLiteral(result)^.appendString(drive)^.append(infoMap,false,context.adapters^);

      infoPair:=newListLiteral;
      infoPair^.appendString('type');
      case driveType of
        DRIVE_REMOVABLE: infoPair^.appendString('removable');
        DRIVE_FIXED:     infoPair^.appendString('fixed'    );
        DRIVE_REMOTE:    infoPair^.appendString('network'  );
        DRIVE_CDROM:     infoPair^.appendString('CD_ROM'   );
        DRIVE_RAMDISK:   infoPair^.appendString('RAM_disk' );
      end;
      infoMap^.append(infoPair,false,context.adapters^);

      GetVolumeInformation(PChar(DriveLetter),
        Buf, sizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
        VolumeFlags, nil, 0);
      SetString(DriveLetter, Buf, StrLen(Buf));

      infoMap^.append(
        newListLiteral^.
        appendString('serial')^.
        appendInt(VolumeSerialNumber),false,context.adapters^);

      infoMap^.append(
        newListLiteral^.
        appendString('label')^.
        appendString(DriveLetter),false,context.adapters^);
    end;

  VAR c:char;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=newListLiteral;
      for c:='A' to 'Z' do P_listLiteral(result)^.append(infoForLetter(c),false,context.adapters^);
    end;
  end;

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
          result:=result^.append(inner,false,context.adapters^);
        end;
      end;
    end;

  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      e:=getEnvironment;
      result:=newListLiteral;
      for i:=0 to length(e)-1 do P_listLiteral(result)^.append(environmentPair(e[i]),false,context.adapters^);
      setLength(e,0);
    end;
  end;

INITIALIZATION
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'random',@random_imp,'random;#Returns a random value in range [0,1]#random(n);Returns a list of n random values in range [0,1]',fc_stateful);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'intRandom',@intRandom_imp,'intRandom(k);#Returns an integer random value in range [0,k-1]#random(k,n);Returns a list of n integer random values in range [0,k-1]',fc_stateful);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'files',@files_impl,'files(searchPattern:string);#Returns a list of files matching the given search pattern',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'folders',@folders_impl,'folders(searchPattern:string);#Returns a list of folders matching the given search pattern',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'allFolders',@allFolders_impl,'allFolders(searchPattern:string);#Returns a list of all folders below and including a given root directory',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileExists',@fileExists_impl,'fileExists(filename:string);#Returns true if the specified file exists and false otherwise',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'folderExists',@folderExists_impl,'folderExists(foldername:string);#Returns true if the specified folder exists and false otherwise',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileContents',@fileContents_impl,'fileContents(filename:string);#Returns the contents of the specified file as one string',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileLines',@fileLines_impl,'fileLines(filename:string);#Returns the contents of the specified file as a list of strings#Information on the line breaks is lost#'+
                                                         'fileLines(filename:string,firstIdx:int,lastIdx:int);#Returns the specified range of lines or the empty list if no line was found in the range. Indexes are inclusive and start with 0.',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeFile',@writeFile_impl,'writeFile(filename:string, content:string);#Writes the specified content to the specified file and returns true',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeFileLines',@writeFileLines_impl,'writeFileLines(filename:string, content:stringList);#Writes the specified content to the specified file and returns true. If the file exists, the routine uses the previously used line breaks.#'+
                                                                              'writeFileLines(filename:string, content:stringList, lineEnding:string);#As above with specified line ending',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'exec',@execSync_impl,'exec(programPath:string,parameters ...);#Executes the specified program and returns the text output',fc_callingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'execAsync',@execAsync_impl,'execAsync(programPath:string,parameters ...);#Starts the specified program and returns true',fc_callingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'execPipeless',@execPipeless_impl,'execPipeless(programPath:string,parameters ...);#Executes the specified program, waiting for exit and returning true',fc_callingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'systime',@systime_imp,'sytime;#Returns the current time as a real number',fc_stateful);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'beep',@beep_imp,'beep;#Makes a beep#beep(freq:int,duration:int);#Makes a beep of given frequency and duration',fc_outputGeneral);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deleteFile',@deleteFile_imp,'deleteFile(filename:string);#Deletes the given file, returning true on success and false otherwise',fc_outputGeneral);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deleteDir',@deleteDir_imp,'deleteDir(directoryname:string);#Deletes the given directory, returning true on success and false otherwise',fc_outputGeneral);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'copyFile',@copyFile_imp,'copyFile(source:string,dest:string);#Copies a file from source to dest, returning true on success and false otherwise',fc_outputGeneral);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'moveFile',@moveFile_imp,'moveFile(source:string,dest:string);#Moves a file from source to dest, returning true on success and false otherwise',fc_outputGeneral);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileInfo',@fileInfo_imp,'fileInfo(filename:string);#Retuns file info as a key-value-list',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'httpGet',@httpGet_imp,'httpGet(URL:string);#Retrieves the contents of the given URL and returns them as a string',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'openUrl',@openUrl_imp,'openUrl(URL:string);#Opens the URL in the default browser',fc_callingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'driveInfo',@driveInfo_imp,'driveInfo;#Returns info on the computer''''s drives/volumes.',fc_readingExternal);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'getEnv',@getEnv_impl,'getEnv;#Returns the current environment variables as a nested list.',fc_readingExternal);
end.
