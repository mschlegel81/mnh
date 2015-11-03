UNIT mnh_funcs_system;
INTERFACE
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,myGenerics,mnh_fileWrappers,
     sysutils, Classes,process,fphttpclient,FileUtil,windows,mySys,myStringUtil;
IMPLEMENTATION
VAR lockedFiles:specialize G_stringKeyMap<TThreadId>;

PROCEDURE obtainLock(CONST fileName:ansistring);
  VAR lockedBy:TThreadID;
  begin
    repeat
      while (lockedFiles.containsKey(fileName,lockedBy)) and (lockedBy<>ThreadID) do sleep(10);
      lockedFiles.put(fileName,ThreadID);
    until lockedFiles.containsKey(fileName,lockedBy)
  end;

PROCEDURE releaseLock(CONST fileName:ansistring);
  VAR lockedBy:TThreadID;
  begin
    while (lockedFiles.containsKey(fileName,lockedBy)) and (lockedBy=ThreadID) do begin
      lockedFiles.dropKey(fileName);
      sleep(10);
    end;
  end;

FUNCTION random_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR i,count:longint;
  begin
    if (params=nil) or (params^.size=0) then exit(newRealLiteral(random))
    else if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_int) then begin
      count:=P_intLiteral(params^.value(0))^.value;
      if count>0 then begin
        result:=newListLiteral;
        for i:=1 to count do P_listLiteral(result)^.appendReal(random);
        exit(result);
      end;
    end;
    result:=nil;
    raiseNotApplicableError('random',params,tokenLocation);
  end;

FUNCTION intRandom_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR i,count:longint;
  begin
     if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_int) then exit(newIntLiteral(random(P_intLiteral(params^.value(0))^.value)))
     else if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_int) and (params^.value(1)^.literalType=lt_int) then begin
      count:=P_intLiteral(params^.value(1))^.value;
      if count>0 then begin
        result:=newListLiteral;
        for i:=1 to count do P_listLiteral(result)^.appendInt(random(P_intLiteral(params^.value(0))^.value));
        exit(result);
      end;
    end;
    result:=nil;
    raiseNotApplicableError('intRandom',params,tokenLocation);
  end;


FUNCTION filesOrDirs_impl(CONST pathOrPathList:P_literal; CONST filesAndNotFolders:boolean):P_listLiteral;
  VAR i,j:longint;
      found:T_arrayOfString;
  begin
    result:=newListLiteral;
    if pathOrPathList^.literalType=lt_string then begin
      found:=find(P_stringLiteral(pathOrPathList)^.value,filesAndNotFolders);
      for i:=0 to length(found)-1 do result^.appendString(replaceAll(found[i],'\','/'));
    end else if pathOrPathList^.literalType=lt_stringList then begin
      for j:=0 to P_listLiteral(pathOrPathList)^.size-1 do begin
        found:=find(P_stringLiteral(P_listLiteral(pathOrPathList)^.value(j))^.value,filesAndNotFolders);
        for i:=0 to length(found)-1 do result^.appendString(replaceAll(found[i],'\','/'));
      end;
    end;
  end;

FUNCTION files_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_string, lt_stringList, lt_emptyList]) then begin
      result:=filesOrDirs_impl(params^.value(0),true);
    end else raiseNotApplicableError('files',params,tokenLocation);
  end;

FUNCTION folders_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_string, lt_stringList, lt_emptyList]) then begin
      result:=filesOrDirs_impl(params^.value(0),false);
    end else raiseNotApplicableError('folders',params,tokenLocation);
  end;

FUNCTION allFolders_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR resultList:TStringList;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newListLiteral;
      if DirectoryExists(P_stringLiteral(params^.value(0))^.value) then begin
        P_listLiteral(result)^.append(params^.value(0),true);
        resultList:=FindAllDirectories(P_stringLiteral(params^.value(0))^.value);
        for i:=0 to resultList.count-1 do P_listLiteral(result)^.appendString(replaceAll(resultList[i],'\','/'));
        resultList.free;
      end;
    end else raiseNotApplicableError('allFolders',params,tokenLocation);
  end;

FUNCTION fileExists_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newBoolLiteral(fileExists(P_stringLiteral(params^.value(0))^.value));
    end else raiseNotApplicableError('fileExists',params,tokenLocation);
  end;

FUNCTION folderExists_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newBoolLiteral(DirectoryExists(P_stringLiteral(params^.value(0))^.value));
    end else raiseNotApplicableError('folderExists',params,tokenLocation);
  end;


FUNCTION fileContents_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR accessed:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      result:=newStringLiteral(fileContent(P_stringLiteral(params^.value(0))^.value,accessed));
      releaseLock(P_stringLiteral(params^.value(0))^.value);
      if not(accessed) then begin
        raiseWarning('File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
        disposeLiteral(result);
        result:=newStringLiteral('');
      end;
    end else raiseNotApplicableError('fileContents',params,tokenLocation);
  end;

FUNCTION fileLines_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR accessed:boolean;
      L:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      L:=fileLines(P_stringLiteral(params^.value(0))^.value,accessed);
      releaseLock(P_stringLiteral(params^.value(0))^.value);
      result:=newListLiteral;
      for i:=0 to length(L)-1 do P_listLiteral(result)^.appendString(L[i]);
      if not(accessed) then begin
        raiseWarning('File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
        disposeLiteral(result);
        result:=newListLiteral;
      end;
    end else if (params<>nil) and (params^.size=3) and
                (params^.value(0)^.literalType=lt_string) and
                (params^.value(1)^.literalType=lt_int) and
                (params^.value(2)^.literalType=lt_int) then begin
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      L:=fileLines(P_stringLiteral(params^.value(0))^.value,
                   P_intLiteral   (params^.value(1))^.value,
                   P_intLiteral   (params^.value(2))^.value,accessed);
      releaseLock(P_stringLiteral(params^.value(0))^.value);
      result:=newListLiteral;
      for i:=0 to length(L)-1 do P_listLiteral(result)^.appendString(L[i]);
      if not(accessed) then begin
        raiseWarning('File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
        disposeLiteral(result);
        result:=newListLiteral;
      end;
    end else raiseNotApplicableError('fileLines',params,tokenLocation);
  end;

FUNCTION writeFile_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR ok:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string)
                                          and (params^.value(1)^.literalType=lt_string) then begin
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      ok:=mnh_fileWrappers.writeFile(P_stringLiteral(params^.value(0))^.value,
                                     P_stringLiteral(params^.value(1))^.value);
      releaseLock(P_stringLiteral(params^.value(0))^.value);
      result:=newBoolLiteral(ok);
      if not(ok) then raiseWarning('File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('writeFile',params,tokenLocation);
  end;

FUNCTION writeFileLines_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR ok:boolean;
      L:T_arrayOfString;
      i:longint;
      sep:string;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.size<=3)
       and (params^.value(0)^.literalType=lt_string)
       and (params^.value(1)^.literalType in [lt_stringList,lt_emptyList])
       and ((params^.size=2) or (params^.value(2)^.literalType=lt_string)) then begin
      if params^.size=3 then sep:=P_stringLiteral(params^.value(2))^.value
                        else sep:='';
      setLength(L,P_listLiteral(params^.value(1))^.size);
      for i:=0 to length(L)-1 do L[i]:=P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value;
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      ok:=writeFileLines(P_stringLiteral(params^.value(0))^.value,L,sep);
      releaseLock(P_stringLiteral(params^.value(0))^.value);
      result:=newBoolLiteral(ok);
      if not(ok) then raiseWarning('File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('writeFileLines',params,tokenLocation);
  end;


FUNCTION execSync_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
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
      tempProcess.executable := executable;
      for n := 0 to length(parameters)-1 do
        tempProcess.parameters.add(parameters [n]);
      tempProcess.options := [poUsePipes, poStderrToOutPut];
      tempProcess.ShowWindow := swoHIDE;
      try
        tempProcess.execute;
        tempProcess.CloseInput;
        while tempProcess.running and (noErrors) do begin
          memStream.SetSize(BytesRead+READ_BYTES);
          n := tempProcess.output.read((memStream.Memory+BytesRead)^, READ_BYTES);
          if n>0 then begin sleepTime:=1; inc(BytesRead, n); end
                 else begin inc(sleepTime); sleep(sleepTime); end;
        end;
        if tempProcess.running then tempProcess.Terminate(999);
        repeat
          memStream.SetSize(BytesRead+READ_BYTES);
          n := tempProcess.output.read((memStream.Memory+BytesRead)^, READ_BYTES);
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
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string)
      and ((params^.size=1) or (params^.size=2) and (params^.value(1)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_flatList])) then begin
      setLength(cmdLinePar,0);
      executable:=P_stringLiteral(params^.value(0))^.value;
      if params^.size=2 then begin
        setLength(cmdLinePar,P_listLiteral(params^.value(1))^.size);
        for i:=0 to P_listLiteral(params^.value(1))^.size-1 do
          cmdLinePar[i]:=P_scalarLiteral(P_listLiteral(params^.value(1))^.value(i))^.stringForm;
      end;
      runCommand(executable,
                 cmdLinePar,
                 output);
      result:=newListLiteral;
      for i:=0 to output.count-1 do P_listLiteral(result)^.appendString(output[i]);
      output.free;
    end else raiseNotApplicableError('exec',params,tokenLocation);
  end;

FUNCTION execAsyncOrPipeless(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST functionName:ansistring; CONST doAsynch:boolean):P_literal;
  VAR executable:ansistring;
      cmdLinePar:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string)
      and ((params^.size=1) or (params^.size=2) and (params^.value(1)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_flatList])) then begin
      setLength(cmdLinePar,0);
      executable:=P_stringLiteral(params^.value(0))^.value;
      if params^.size=2 then begin
        setLength(cmdLinePar,P_listLiteral(params^.value(1))^.size);
        for i:=0 to P_listLiteral(params^.value(1))^.size-1 do
          cmdLinePar[i]:=P_scalarLiteral(P_listLiteral(params^.value(1))^.value(i))^.stringForm;
      end;
      runCommandAsyncOrPipeless(executable,
                                cmdLinePar,doAsynch);
      result:=newBoolLiteral(true);
    end else raiseNotApplicableError(functionName,params,tokenLocation);
  end;

FUNCTION execAsync_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=execAsyncOrPipeless(params,tokenLocation,'execAsync',true);
  end;

FUNCTION execPipeless_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=execAsyncOrPipeless(params,tokenLocation,'execPipeless',false);
  end;

FUNCTION systime_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then exit(newRealLiteral(now))
    else raiseNotApplicableError('systime',params,tokenLocation);
  end;


FUNCTION deleteFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      result:=newBoolLiteral(DeleteFileUTF8(UTF8Encode(P_stringLiteral(params^.value(0))^.value)));
      releaseLock(P_stringLiteral(params^.value(0))^.value);
    end else raiseNotApplicableError('deleteFile',params,tokenLocation);
  end;

FUNCTION deleteDir_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      result:=newBoolLiteral(DeleteDirectory(P_stringLiteral(params^.value(0))^.value,false));
      releaseLock(P_stringLiteral(params^.value(0))^.value);
    end else raiseNotApplicableError('deleteDir',params,tokenLocation);
  end;

FUNCTION copyFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) and (params^.value(1)^.literalType=lt_string)  then begin
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      obtainLock(P_stringLiteral(params^.value(1))^.value);
      ensurePath(P_stringLiteral(params^.value(1))^.value);
      result:=newBoolLiteral(
      FileUtil.CopyFile(P_stringLiteral(params^.value(0))^.value,
                        P_stringLiteral(params^.value(1))^.value,true));
      releaseLock(P_stringLiteral(params^.value(0))^.value);
      releaseLock(P_stringLiteral(params^.value(1))^.value);
    end else raiseNotApplicableError('copyFile',params,tokenLocation);
  end;

FUNCTION moveFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) and (params^.value(1)^.literalType=lt_string)  then begin
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      obtainLock(P_stringLiteral(params^.value(1))^.value);
      ensurePath(P_stringLiteral(params^.value(1))^.value);
      result:=newBoolLiteral(
      FileUtil.RenameFileUTF8(UTF8Encode(P_stringLiteral(params^.value(0))^.value),
                              UTF8Encode(P_stringLiteral(params^.value(1))^.value)));
      releaseLock(P_stringLiteral(params^.value(0))^.value);
      releaseLock(P_stringLiteral(params^.value(1))^.value);
    end else raiseNotApplicableError('moveFile',params,tokenLocation);
  end;

FUNCTION fileInfo_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
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
        append(value,false),false);
    end;

  VAR i:longint;
      tmpParam:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      obtainLock(P_stringLiteral(params^.value(0))^.value);
      getFileInfo(P_stringLiteral(params^.value(0))^.value,time,size,isExistent,isArchive,isDirectory,isReadOnly,isSystem,isHidden);
      releaseLock(P_stringLiteral(params^.value(0))^.value);
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
    end else if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_stringList,lt_emptyList]) then begin
      result:=newListLiteral;
      for i:=0 to P_listLiteral(params^.value(0))^.size-1 do begin
        tmpParam:=newOneElementListLiteral(P_listLiteral(params^.value(0))^.value(i),true);
        P_listLiteral(result)^.append(fileInfo_imp(tmpParam,tokenLocation),false);
        disposeLiteral(tmpParam);
      end;
    end else raiseNotApplicableError('fileInfo',params,tokenLocation);
  end;

FUNCTION httpGet_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR resultText:ansistring;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      try
        resultText:=TFPCustomHTTPClient.SimpleGet(P_stringLiteral(params^.value(0))^.value);
      except
        On E : Exception do begin
          resultText:='';
          raiseCustomMessage(mt_el5_systemError,'httpGet failed with:'+E.message,tokenLocation);
        end;
      end;
      result:=newStringLiteral(resultText);
    end else raiseNotApplicableError('httpGet',params,tokenLocation);
  end;

FUNCTION beep_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=newVoidLiteral;
      sysutils.Beep;
    end else if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_int) and (params^.value(1)^.literalType=lt_int) then begin
      result:=newVoidLiteral;
      windows.Beep(P_intLiteral(params^.value(0))^.value,
                   P_intLiteral(params^.value(1))^.value);
    end else raiseNotApplicableError('beep',params,tokenLocation);
  end;

FUNCTION setErrorlevel_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_int) then begin
      mnh_out_adapters.systemErrorlevel.value:=P_intLiteral(params^.value(0))^.value;
      result:=newVoidLiteral;
    end else raiseNotApplicableError('setErrorlevel',params,tokenLocation);
  end;

FUNCTION driveInfo_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION infoForLetter(CONST drive:char):P_literal;
    VAR DriveLetter: string;
        driveType:longint;
        NotUsed:     dword=0;
        VolumeFlags: dword=0;
        VolumeInfo:  array[0..MAX_PATH] of char;
        VolumeSerialNumber: dword;
        Buf: array [0..MAX_PATH] of char;
        infoPair:P_listLiteral;
    begin
      DriveLetter := drive + ':\';
      driveType:=GetDriveType(PChar(DriveLetter));
      if driveType in [DRIVE_REMOVABLE,DRIVE_FIXED,DRIVE_REMOTE,DRIVE_CDROM,DRIVE_RAMDISK] then begin
        result:=newListLiteral;
      end else exit(newVoidLiteral);

      P_listLiteral(result)^.append(newListLiteral^.appendString('drive')^.appendString( drive ),false);

      infoPair:=newListLiteral;
      infoPair^.appendString('type');
      case driveType of
        DRIVE_REMOVABLE: infoPair^.appendString('removable');
        DRIVE_FIXED:     infoPair^.appendString('fixed'    );
        DRIVE_REMOTE:    infoPair^.appendString('network'  );
        DRIVE_CDROM:     infoPair^.appendString('CD_ROM'   );
        DRIVE_RAMDISK:   infoPair^.appendString('RAM_disk' );
      end;
      P_listLiteral(result)^.append(infoPair,false);

      GetVolumeInformation(PChar(DriveLetter),
        Buf, sizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
        VolumeFlags, nil, 0);
      SetString(DriveLetter, Buf, StrLen(Buf));

      P_listLiteral(result)^.append(
        newListLiteral^.
        appendString('serial')^.
        appendInt(VolumeSerialNumber),false);

      P_listLiteral(result)^.append(
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
    end else raiseNotApplicableError('driveInfo',params,tokenLocation);
  end;

FUNCTION getEnv_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR e:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      e:=getEnvironment;
      result:=newListLiteral;
      for i:=0 to length(e)-1 do P_listLiteral(result)^.appendString(e[i]);
      setLength(e,0);
    end else raiseNotApplicableError('getEnv',params,tokenLocation);
  end;

INITIALIZATION
  lockedFiles.create();
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'random',@random_imp,'random;#Returns a random value in range [0,1]#random(n);Returns a list of n random values in range [0,1]');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'intRandom',@intRandom_imp,'intRandom(k);#Returns an integer random value in range [0,k-1]#random(k,n);Returns a list of n integer random values in range [0,k-1]');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'files',@files_impl,'files(searchPattern:string);#Returns a list of files matching the given search pattern');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'folders',@folders_impl,'folders(searchPattern:string);#Returns a list of folders matching the given search pattern');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'allFolders',@allFolders_impl,'allFolders(searchPattern:string);#Returns a list of all folders below and including a given root directory');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileExists',@fileExists_impl,'fileExists(filename:string);#Returns true if the specified file exists and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'folderExists',@folderExists_impl,'folderExists(foldername:string);#Returns true if the specified folder exists and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileContents',@fileContents_impl,'fileContents(filename:string);#Returns the contents of the specified file as one string');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileLines',@fileLines_impl,'fileLines(filename:string);#Returns the contents of the specified file as a list of strings#Information on the line breaks is lost#'+
                                                         'fileLines(filename:string,firstIdx:int,lastIdx:int);#Returns the specified range of lines or the empty list if no line was found in the range. Indexes are inclusive and start with 0.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeFile',@writeFile_impl,'writeFile(filename:string, content:string);#Writes the specified content to the specified file and returns true');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeFileLines',@writeFileLines_impl,'writeFileLines(filename:string, content:stringList);#Writes the specified content to the specified file and returns true#writeFileLines(filename:string, content:stringList, lineEnding:string);#As above with specified line ending');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'exec',@execSync_impl,'exec(programPath:string,parameters ...);#Executes the specified program and returns the text output');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'execAsync',@execAsync_impl,'execAsync(programPath:string,parameters ...);#Starts the specified program and returns true');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'execPipeless',@execPipeless_impl,'execPipeless(programPath:string,parameters ...);#Executes the specified program, waiting for exit and returning true');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'systime',@systime_imp,'sytime;#Returns the current time as a real number');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'beep',@beep_imp,'beep;#Makes a beep#beep(freq:int,duration:int);#Makes a beep of given frequency and duration');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deleteFile',@deleteFile_imp,'deleteFile(filename:string);#Deletes the given file, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deleteDir',@deleteDir_imp,'deleteDir(directoryname:string);#Deletes the given directory, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'copyFile',@copyFile_imp,'copyFile(source:string,dest:string);#Copies a file from source to dest, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'moveFile',@moveFile_imp,'moveFile(source:string,dest:string);#Moves a file from source to dest, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileInfo',@fileInfo_imp,'fileInfo(filename:string);#Retuns file info as a key-value-list');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'httpGet',@httpGet_imp,'httpGet(URL:string);#Retrieves the contents of the given URL and returns them as a string');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'setErrorlevel',@setErrorlevel_imp,'setErrorlevel(level:int);#Sets the errorlevel returned by the interpreter');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'driveInfo',@driveInfo_imp,'driveInfo;#Returns info on the computer''''s drives/volumes.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'getEnv',@getEnv_impl,'getEnv;#Returns the current environment variables.');

FINALIZATION
  lockedFiles.destroy;

end.
