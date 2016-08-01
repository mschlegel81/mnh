UNIT mnh_funcs_system;
INTERFACE
{$WARN 5024 OFF}
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,myGenerics,mnh_fileWrappers,
     sysutils, Classes,Process,UTF8Process,fphttpclient,FileUtil,{$ifdef Windows}windows,{$endif}mySys,myStringUtil,mnh_contexts,lclintf,
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
                                        then result^.append(pathOrPathList,true);
      for i:=0 to length(found)-1 do result^.appendString(replaceAll(found[i],'\','/'));
    end else if pathOrPathList^.literalType=lt_stringList then begin
      for j:=0 to P_listLiteral(pathOrPathList)^.size-1 do begin
        found:=find(searchString(j),filesAndNotFolders,recurseSubDirs);
        if recurseSubDirs and DirectoryExists(P_stringLiteral(P_listLiteral(pathOrPathList)^.value(j))^.value)
                                          then result^.append(P_listLiteral(pathOrPathList)^.value(j),true);
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

FUNCTION allFiles_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR pattern:string='';
      recurse:boolean=true;
      i:longint;

  PROCEDURE searchInRoot(CONST root:string);
    VAR list:TStringList;
        k:longint;
    begin
      list:=FindAllFiles(root,pattern,recurse);
      for k:=0 to list.count-1 do P_listLiteral(result)^.appendString(replaceAll(list[k],'\','/'));
      list.destroy;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=3) and (arg0^.literalType in [lt_stringList,lt_string,lt_emptyList]) then begin
      if arg0^.literalType=lt_emptyList then begin arg0^.rereference; exit(arg0); end;
      if (params^.size>=2) then begin
        case arg1^.literalType of
          lt_emptyList: begin end;
          lt_string: pattern:=str1^.value;
          lt_stringList: begin
            pattern:=P_stringLiteral(list1^.value(0))^.value;
            for i:=1 to list1^.size-1 do pattern:=pattern+';'+P_stringLiteral(list1^.value(i))^.value;
          end;
          else exit(nil);
        end;
      end;
      if (params^.size>=3) then begin
        if arg2^.literalType<>lt_boolean then exit(nil);
        recurse:=P_boolLiteral(arg2)^.value;
      end;
      result:=newListLiteral;
      if arg0^.literalType=lt_string
      then searchInRoot(str0^.value)
      else for i:=0 to list0^.size-1 do searchInRoot(P_stringLiteral(list0^.value(i))^.value);
    end;
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
    end;
  end;

FUNCTION writeFile_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR ok:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string)
                                          and (arg1^.literalType=lt_string) then begin
      ok:=mnh_fileWrappers.writeFile(str0^.value,
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
      ok:=writeFileLines(str0^.value,L,sep);
      result:=newBoolLiteral(ok);
      if not(ok) then context.adapters^.raiseWarning('File "'+str0^.value+'" cannot be accessed',tokenLocation);
    end;
  end;

FUNCTION execSync_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION runCommand(CONST executable: ansistring; CONST parameters: T_arrayOfString; OUT output: TStringList; CONST includeStdErr:boolean): boolean;
    CONST
      READ_BYTES = 2048;
    VAR
      stdErrDummy:array[0..READ_BYTES-1] of byte;
      memStream: TMemoryStream;
      tempProcess: TProcessUTF8;
      n: longint;
      BytesRead: longint;
      sleepTime: longint = 1;
    begin
      memStream := TMemoryStream.create;
      BytesRead := 0;
      tempProcess := TProcessUTF8.create(nil);
      tempProcess.executable := executable;
      for n := 0 to length(parameters)-1 do tempProcess.parameters.add(parameters[n]);
      if includeStdErr then tempProcess.options := [poUsePipes, poStderrToOutPut]
                       else tempProcess.options := [poUsePipes];
      tempProcess.ShowWindow := swoHIDE;
      try
        tempProcess.execute;
        tempProcess.CloseInput;
        while tempProcess.running and context.adapters^.noErrors do begin
          memStream.SetSize(BytesRead+READ_BYTES);
          if not(includeStdErr) then tempProcess.stdErr.read(stdErrDummy,READ_BYTES);
          n := tempProcess.output.read((memStream.memory+BytesRead)^, READ_BYTES);
          if n>0 then begin sleepTime:=1; inc(BytesRead, n); end
                 else begin inc(sleepTime); sleep(sleepTime); end;
        end;
        if tempProcess.running then tempProcess.Terminate(999);
        repeat
          memStream.SetSize(BytesRead+READ_BYTES);
          if not(includeStdErr) then tempProcess.stdErr.read(stdErrDummy,READ_BYTES);
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
      output.loadFromStream(memStream);
      memStream.free;
    end;

  VAR executable:ansistring;
      cmdLinePar:T_arrayOfString;
      output:TStringList;
      i:longint;
      includeStdErr:boolean=true;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=3) and (arg0^.literalType=lt_string) then begin
      setLength(cmdLinePar,0);
      if (params^.size>=2) then begin
        if arg1^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_flatList] then begin
          setLength(cmdLinePar,list1^.size);
          for i:=0 to list1^.size-1 do begin
            cmdLinePar[i]:=P_scalarLiteral(list1^.value(i))^.stringForm;
          end;
        end else if (arg1^.literalType=lt_boolean) then includeStdErr:=P_boolLiteral(arg1)^.value
        else exit(nil);
      end;
      if (params^.size=3) then begin
        if (arg2^.literalType=lt_boolean) then includeStdErr:=P_boolLiteral(arg2)^.value
        else exit(nil);
      end;
      executable:=str0^.value;
      runCommand(executable,
                 cmdLinePar,
                 output,
                 includeStdErr);
      result:=newListLiteral;
      for i:=0 to output.count-1 do P_listLiteral(result)^.appendString(output[i]);
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
      executable:=str0^.value;
      if params^.size=2 then begin
        setLength(cmdLinePar,list1^.size);
        for i:=0 to list1^.size-1 do
          cmdLinePar[i]:=P_scalarLiteral(list1^.value(i))^.stringForm;
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
        append(value,false),false);
    end;

  VAR i:longint;
      tmpParam:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      getFileInfo(str0^.value,time,size,isExistent,isArchive,isDirectory,isReadOnly,isSystem,isHidden);
      if not(isExistent) then begin
        size:=-1;
        time:= 0;
        isArchive:=false;
        isDirectory:=false;
        isReadOnly:=false;
        isSystem:=false;
        isHidden:=false;
      end;
      resultAsList:=newListLiteral;
      appendKeyValuePair('exists',newBoolLiteral(isExistent));
      appendKeyValuePair('size',newIntLiteral(size));
      appendKeyValuePair('time',newRealLiteral(time));
      attributeList:=newListLiteral;
      if isArchive   then attributeList^.appendString('archive'  );
      if isDirectory then attributeList^.appendString('directory');
      if isReadOnly  then attributeList^.appendString('readonly' );
      if isSystem    then attributeList^.appendString('system'   );
      if isHidden    then attributeList^.appendString('hidden'   );
      appendKeyValuePair('attributes',attributeList);
      result:=resultAsList;
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_stringList,lt_emptyList]) then begin
      result:=newListLiteral;
      for i:=0 to list0^.size-1 do begin
        tmpParam:=newOneElementListLiteral(list0^.value(i),true);
        P_listLiteral(result)^.append(fileInfo_imp(tmpParam,tokenLocation,context),false);
        disposeLiteral(tmpParam);
      end;
    end;
  end;

FUNCTION fileStats_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR lineCount,wordCount,byteCount:longint;
      hash:T_hashInt;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      fileStats(str0^.value,lineCount,wordCount,byteCount,hash);
      result:=newListLiteral^.appendInt(lineCount)^.appendInt(wordCount)^.appendInt(byteCount)^.appendInt(hash);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_stringList) then begin
      result:=newListLiteral;
      for i:=0 to list0^.size-1 do begin
        fileStats(P_stringLiteral(list0^.value(i))^.value,lineCount,wordCount,byteCount,hash);
        P_listLiteral(result)^.append(newListLiteral^.appendInt(lineCount)^.appendInt(wordCount)^.appendInt(byteCount)^.appendInt(hash),false);
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
  begin new(result,create(at_unknown)); end;

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
    then result:=messagesToLiteral(collector.value^.storedMessages)
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
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'files',@files_impl,'files(searchPattern:string);#Returns a list of files matching the given search pattern');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'allFiles',@allFiles_impl,'allFiles(root);#Returns a list of all files below root (string or stringList)#'+
                                                                  'allFiles(root,pattern);#Returns a list of all files matching pattern(s) (string or stringList)#'+
                                                                  'allFiles(root,pattern,recurse=false);#As above but without recursing subfolders');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'folders',@folders_impl,'folders(searchPattern:string);#Returns a list of folders matching the given search pattern');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'allFolders',@allFolders_impl,'allFolders(rootFolder:string);#Returns a list of all folders below and including a given root directory');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileExists',@fileExists_impl,'fileExists(filename:string);#Returns true if the specified file exists and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'folderExists',@folderExists_impl,'folderExists(foldername:string);#Returns true if the specified folder exists and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileContents',@fileContents_impl,'fileContents(filename:string);#Returns the contents of the specified file as one string');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileLines',@fileLines_impl,'fileLines(filename:string);#Returns the contents of the specified file as a list of strings#Information on the line breaks is lost');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeFile',@writeFile_impl,'writeFile(filename:string, content:string);#Writes the specified content to the specified file and returns true');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeFileLines',@writeFileLines_impl,'writeFileLines(filename:string, content:stringList);#Writes the specified content to the specified file and returns true. If the file exists, the routine uses the previously used line breaks.#'+
                                                                              'writeFileLines(filename:string, content:stringList, lineEnding:string);#As above with specified line ending');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'exec',@execSync_impl,'exec(programPath:string);#Executes the specified program and returns the text output including stdErr output#'+
                                                              'exec(programPath:string,parameters:flatList);#Executes the specified program with given command line parameters and returns the text output including stdErr output#'+
                                                              'exec(programPath:string,includeStdErr:boolean);#Executes the specified program and returns the text output optionally including stdErr output#'+
                                                              'exec(programPath:string,parameters:flatList,parameters:flatList);#Executes the specified program with given command line parameters and returns the text output optionally including stdErr output');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'execAsync',@execAsync_impl,'execAsync(programPath:string,parameters ...);#Starts the specified program and returns true');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'execPipeless',@execPipeless_impl,'execPipeless(programPath:string,parameters ...);#Executes the specified program, waiting for exit and returning true');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'systime',@systime_imp,'systime;#Returns the current time as a real number');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'beep',@beep_imp,'beep;#Makes a beep'{$ifdef WINDOWS}+'#beep(freq:int,duration:int);#Makes a beep of given frequency and duration'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deleteFile',@deleteFile_imp,'deleteFile(filename:string);#Deletes the given file, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deleteDir',@deleteDir_imp,'deleteDir(directoryname:string);#Deletes the given directory, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'copyFile',@copyFile_imp,'copyFile(source:string,dest:string);#Copies a file from source to dest, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'moveFile',@moveFile_imp,'moveFile(source:string,dest:string);#Moves a file from source to dest, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileInfo',@fileInfo_imp,'fileInfo(filename:string);#Retuns file info as a key-value-list');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileStats',@fileStats_imp,'fileStats(filename:string);#Retuns a triplet [lineCount,wordCount,byteCount,hash].');
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
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'setExitCode',@setExitCode_impl,'setExitCode(code:int);#Sets the exit code of the executable.#Might be overridden by an evaluation error.');
FINALIZATION
  collector.destroy;
end.
