UNIT mnh_funcs_files;
INTERFACE
{$WARN 5024 OFF}
USES mnh_basicTypes,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,myGenerics,mnh_fileWrappers,
     sysutils, Classes,Process,UTF8Process,FileUtil,{$ifdef Windows}windows,{$endif}mySys,myStringUtil,mnh_contexts,lclintf,
     LazFileUtils,LazUTF8;
IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION filesOrDirs_impl(CONST pathOrPathList:P_literal; CONST filesAndNotFolders,recurseSubDirs:boolean):P_listLiteral;
  VAR i,j:longint;
      found:T_arrayOfString;
  FUNCTION searchString(CONST index:longint):ansistring;
    begin
      if pathOrPathList^.literalType=lt_string
      then result:=P_stringLiteral(pathOrPathList)^.value
      else result:=P_stringLiteral(P_listLiteral(pathOrPathList)^[index])^.value;
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
        if recurseSubDirs and DirectoryExists(P_stringLiteral(P_listLiteral(pathOrPathList)^[j])^.value)
                                          then result^.append(P_listLiteral(pathOrPathList)^[j],true);
        for i:=0 to length(found)-1 do result^.appendString(replaceAll(found[i],'\','/'));
      end;
    end;
  end;

FUNCTION files_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string, lt_stringList, lt_emptyList])
    then result:=filesOrDirs_impl(arg0,true,false);
  end;

FUNCTION allFiles_impl intFuncSignature;
  VAR pattern:string='';
      recurse:boolean=true;
      i:longint;

  PROCEDURE searchInRoot(CONST root:string);
    VAR list:TStringList;
        k:longint;
    begin
      list:=FindAllFiles(root,pattern,recurse);
      for k:=0 to list.count-1 do listResult^.appendString(replaceAll(list[k],'\','/'));
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
            pattern:=P_stringLiteral(list1^[0])^.value;
            for i:=1 to list1^.size-1 do pattern:=pattern+';'+P_stringLiteral(list1^[i])^.value;
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
      else for i:=0 to list0^.size-1 do searchInRoot(P_stringLiteral(list0^[i])^.value);
    end;
  end;

FUNCTION folders_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string, lt_stringList, lt_emptyList])
    then result:=filesOrDirs_impl(arg0,false,false);
  end;

FUNCTION allFolders_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string, lt_stringList, lt_emptyList])
    then result:=filesOrDirs_impl(arg0,false,true);
  end;

FUNCTION fileExists_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(fileExists(str0^.value));
  end;

FUNCTION folderExists_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(DirectoryExists(str0^.value));
  end;

FUNCTION fileContents_impl intFuncSignature;
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

FUNCTION fileLines_impl intFuncSignature;
  VAR accessed:boolean;
      L:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      L:=fileLines(str0^.value,accessed);
      result:=newListLiteral;
      for i:=0 to length(L)-1 do listResult^.appendString(L[i]);
      if not(accessed) then begin
        context.adapters^.raiseWarning('File "'+str0^.value+'" cannot be accessed',tokenLocation);
        disposeLiteral(result);
        result:=newListLiteral;
      end;
    end;
  end;

FUNCTION writeFile_impl intFuncSignature;
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

FUNCTION writeOrAppendFileLines(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext; CONST doAppend:boolean):P_literal;
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
      for i:=0 to length(L)-1 do L[i]:=P_stringLiteral(list1^[i])^.value;
      ok:=writeFileLines(str0^.value,L,sep,doAppend);
      result:=newBoolLiteral(ok);
      if not(ok) then context.adapters^.raiseWarning('File "'+str0^.value+'" cannot be accessed',tokenLocation);
    end;
  end;

FUNCTION writeFileLines_impl intFuncSignature;
  begin result:=writeOrAppendFileLines(params,tokenLocation,context,false); end;

FUNCTION appendFileLines_impl intFuncSignature;
  begin result:=writeOrAppendFileLines(params,tokenLocation,context,true); end;

FUNCTION execSync_impl intFuncSignature;
  FUNCTION runCommand(CONST executable: ansistring; CONST parameters: T_arrayOfString; OUT output: TStringList; CONST includeStdErr:boolean): int64;
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
      initialize(stdErrDummy);
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
          if not(includeStdErr) then begin
            while tempProcess.stdErr.NumBytesAvailable>0 do
            tempProcess.stdErr.read(stdErrDummy,READ_BYTES);
          end;
          if tempProcess.running then begin
            if tempProcess.output.NumBytesAvailable>0
            then n:=tempProcess.output.read((memStream.memory+BytesRead)^, READ_BYTES)
            else n:=0;
          end;
          if tempProcess.running then begin
            if n>0 then begin sleepTime:=1; inc(BytesRead, n); end
                   else begin inc(sleepTime); sleep(sleepTime); end;
          end;
        end;
        if tempProcess.running then tempProcess.Terminate(999);
        repeat
          memStream.SetSize(BytesRead+READ_BYTES);
          if not(includeStdErr) then tempProcess.stdErr.read(stdErrDummy,READ_BYTES);
          n := tempProcess.output.read((memStream.memory+BytesRead)^, READ_BYTES);
          if n>0 then inc(BytesRead, n);
        until n<=0;
        result := tempProcess.ExitCode;
      except
        result := $ffffffff;
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
      outputLit:P_listLiteral;
      i:longint;
      includeStdErr:boolean=true;
      processExitCode:int64;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=3) and (arg0^.literalType=lt_string) then begin
      setLength(cmdLinePar,0);
      if (params^.size>=2) then begin
        if arg1^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList] then begin
          setLength(cmdLinePar,list1^.size);
          for i:=0 to list1^.size-1 do begin
            cmdLinePar[i]:=P_scalarLiteral(list1^[i])^.stringForm;
          end;
        end else if (arg1^.literalType=lt_boolean) then includeStdErr:=bool1^.value
        else exit(nil);
      end;
      if (params^.size=3) then begin
        if (arg2^.literalType=lt_boolean) then includeStdErr:=P_boolLiteral(arg2)^.value
        else exit(nil);
      end;
      executable:=str0^.value;
      processExitCode:=runCommand(executable,cmdLinePar,output,includeStdErr);
      outputLit:=newListLiteral(output.count);
      for i:=0 to output.count-1 do outputLit^.appendString(output[i]);
      result:=newListLiteral(2)^.append(outputLit,false)^.appendInt(processExitCode);
      output.free;
    end;
  end;

FUNCTION execAsyncOrPipeless(CONST params:P_listLiteral; CONST doAsynch:boolean):P_literal;
  VAR executable:ansistring;
      cmdLinePar:T_arrayOfString;
      i:longint;
      processExitCode:int64;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string)
      and ((params^.size=1) or (params^.size=2) and (arg1^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList])) then begin
      setLength(cmdLinePar,0);
      executable:=str0^.value;
      if params^.size=2 then begin
        setLength(cmdLinePar,list1^.size);
        for i:=0 to list1^.size-1 do
          cmdLinePar[i]:=P_scalarLiteral(list1^[i])^.stringForm;
      end;
      showConsole;
      processExitCode:=runCommandAsyncOrPipeless(executable,cmdLinePar,doAsynch);
      if doAsynch then
        result:=newVoidLiteral
      else begin
        hideConsole;
        result:=newIntLiteral(processExitCode);
      end;
    end;
  end;

FUNCTION execAsync_impl intFuncSignature;
  begin
    result:=execAsyncOrPipeless(params,true);
  end;

FUNCTION execPipeless_impl intFuncSignature;
  begin
    result:=execAsyncOrPipeless(params,false);
  end;

FUNCTION deleteFile_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=newBoolLiteral(DeleteFileUTF8(str0^.value));
    end;
  end;

FUNCTION deleteDir_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=newBoolLiteral(DeleteDirectory(str0^.value,false));
    end;
  end;

FUNCTION copyFile_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string)  then begin
      ensurePath(str1^.value);
      result:=newBoolLiteral(
      FileUtil.CopyFile(str0^.value,str1^.value,true));
    end;
  end;

FUNCTION moveFile_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string)  then begin
      ensurePath(str1^.value);
      result:=newBoolLiteral(
      RenameFile(str0^.value,str1^.value));
    end;
  end;

FUNCTION fileInfo_imp intFuncSignature;
  FUNCTION infoForSearch(CONST s:string):P_compoundLiteral;
    FUNCTION infoToLiteral(CONST info:T_fileInfo):P_mapLiteral;
      VAR a:P_setLiteral;
          att:T_fileAttrib;
      begin
        a:=newSetLiteral;
        for att in info.attributes do a^.appendString(C_fileAttribName[att]);
        result:=newMapLiteral^
                .put('path'      ,info.filePath)^
                .put('time'      ,info.time)^
                .put('size'      ,info.size)^
                .put('attributes',a,false);
      end; //infoToLiteral

    VAR info:T_fileInfo;
    begin
      if containsPlaceholder(s) then begin
        result:=newListLiteral(0);
        for info in findFileInfo(s) do listResult^.append(infoToLiteral(info),false);
      end else for info in findFileInfo(s) do exit(infoToLiteral(info));
    end; //infoForSearch

  VAR iter:T_arrayOfLiteral;
      sub :P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_string: exit(infoForSearch(str0^.value));
      lt_stringList,lt_stringSet: begin
        result:=collection0^.newOfSameType;
        iter  :=collection0^.iteratableList;
        for sub in iter do collResult^.append(infoForSearch(P_stringLiteral(sub)^.value),false);
        disposeLiteral(iter);
      end;
      lt_emptyList: result:=newListLiteral(0);
      lt_emptySet : result:=newSetLiteral;
    end;
  end; //fileInfo_imp

FUNCTION fileStats_imp intFuncSignature;
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
        fileStats(P_stringLiteral(list0^[i])^.value,lineCount,wordCount,byteCount,hash);
        listResult^.append(newListLiteral^.appendInt(lineCount)^.appendInt(wordCount)^.appendInt(byteCount)^.appendInt(hash),false);
      end;
    end;
  end;

{$define fileNameBody:=VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (arg0^.literalType=lt_string) then result:=newStringLiteral(internal(str0^.value))
      else if arg0^.literalType in [lt_stringList,lt_emptyList] then begin
        result:=newListLiteral(list0^.size);
        for i:=0 to list0^.size-1 do listResult^.appendString(internal(P_stringLiteral(list0^[i])^.value));
      end;
    end;
  end}

FUNCTION expandedFileName_imp intFuncSignature;
  FUNCTION internal(CONST s:string):string;
    begin result:=replaceAll(replaceAll(expandFileName(s),'\','/'),'//','/'); end;
  fileNameBody;

FUNCTION extractFileDirectory_imp intFuncSignature;
  FUNCTION internal(CONST s:string):string;
    begin
      if ExtractFileDir(s)=''
      then result:='.'
      else result:=replaceAll(ExtractFileDir(s),'\','/');
    end;
  fileNameBody;

FUNCTION extractFileName_imp intFuncSignature;
  FUNCTION internal(CONST s:string):string;
    begin result:=replaceAll(extractFileName(s),'\','/'); end;
  fileNameBody;

FUNCTION extractFileNameOnly_imp intFuncSignature;
  FUNCTION internal(CONST s:string):string;
    begin result:=replaceAll(ExtractFileNameOnly(s),'\','/'); end;
  fileNameBody;

FUNCTION extractFileExt_imp intFuncSignature;
  FUNCTION internal(CONST s:string):string;
    begin result:=replaceAll(extractFileExt(s),'\','/'); end;
  fileNameBody;

FUNCTION changeFileExtension_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string)
    then result:=newStringLiteral(ChangeFileExt(str0^.value,str1^.value));
  end;

FUNCTION relativeFilename_impl intFuncSignature;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then case arg0^.literalType of
      lt_string: case arg1^.literalType of
        lt_string: exit(newStringLiteral(
            replaceAll(
            extractRelativePath(str0^.value+'/',
                                str1^.value),
            '\','/')));
        lt_stringList,lt_emptyList: begin
          result:=newListLiteral;
          for i:=0 to list1^.size-1 do
            listResult^.appendString(
            replaceAll(
            extractRelativePath(str0^.value+'/',
                                P_stringLiteral(list1^[i])^.value),
            '\','/'));
        end;
      end;
      lt_stringList,lt_emptyList: case arg1^.literalType of
        lt_string: begin
          result:=newListLiteral;
          for i:=0 to list1^.size-1 do
            listResult^.appendString(
            replaceAll(
            extractRelativePath(P_stringLiteral(list0^[i])^.value+'/',
                                str1^.value),
            '\','/'));
        end;
        lt_stringList,lt_emptyList: if  list0^.size= list1^.size then begin
          result:=newListLiteral;
          for i:=0 to list1^.size-1 do
            listResult^.appendString(
            replaceAll(
            extractRelativePath(P_stringLiteral(list0^[i])^.value+'/',
                                P_stringLiteral(list1^[i])^.value),
            '\','/'));
        end;
      end;
    end;
  end;

INITIALIZATION
  registerRule(FILES_BUILTIN_NAMESPACE,'files'          ,@files_impl         ,[se_os_query] ,ak_unary     ,'files(searchPattern:string);//Returns a list of files matching the given search pattern');
  registerRule(FILES_BUILTIN_NAMESPACE,'allFiles'       ,@allFiles_impl      ,[se_os_query] ,ak_variadic_1,'allFiles(root);//Returns a list of all files below root (string or stringList)#'+
                                                                                                           'allFiles(root,pattern);//Returns a list of all files matching pattern(s) (string or stringList)#'+
                                                                                                           'allFiles(root,pattern,recurse=false);//As above but without recursing subfolders');
  registerRule(FILES_BUILTIN_NAMESPACE,'folders'        ,@folders_impl       ,[se_os_query] ,ak_unary     ,'folders(searchPattern:string);//Returns a list of folders matching the given search pattern');
  registerRule(FILES_BUILTIN_NAMESPACE,'allFolders'     ,@allFolders_impl    ,[se_os_query] ,ak_unary     ,'allFolders(rootFolder:string);//Returns a list of all folders below and including a given root directory');
  registerRule(FILES_BUILTIN_NAMESPACE,'fileExists'     ,@fileExists_impl    ,[se_os_query] ,ak_unary     ,'fileExists(filename:string);//Returns true if the specified file exists and false otherwise');
  registerRule(FILES_BUILTIN_NAMESPACE,'folderExists'   ,@folderExists_impl  ,[se_os_query] ,ak_unary     ,'folderExists(foldername:string);//Returns true if the specified folder exists and false otherwise');
  registerRule(FILES_BUILTIN_NAMESPACE,'fileContents'   ,@fileContents_impl  ,[se_readFile] ,ak_unary     ,'fileContents(filename:string);//Returns the contents of the specified file as one string');
  registerRule(FILES_BUILTIN_NAMESPACE,'fileLines'      ,@fileLines_impl     ,[se_readFile] ,ak_unary     ,'fileLines(filename:string);//Returns the contents of the specified file as a list of strings#//Information on the line breaks is lost');
  registerRule(FILES_BUILTIN_NAMESPACE,'writeFile'      ,@writeFile_impl     ,[se_writeFile],ak_binary    ,'writeFile(filename:string, content:string);//Writes the specified content to the specified file and returns true');
  registerRule(FILES_BUILTIN_NAMESPACE,'writeFileLines' ,@writeFileLines_impl,[se_writeFile],ak_variadic_2,'writeFileLines(filename:string, content:stringList);//Writes the specified content to the specified file and returns true. If the file exists, the routine uses the previously used line breaks.#'+
                                                                                                           'writeFileLines(filename:string, content:stringList, lineEnding:string);//As above with specified line ending');
  registerRule(FILES_BUILTIN_NAMESPACE,'appendFileLines',@appendFileLines_impl,[se_readFile,se_writeFile],ak_variadic_2,'appendFileLines(filename:string, content:stringList);//Appends the specified content to the specified file and returns true. If the file exists, the routine uses the previously used line breaks.#'+
                                                                              'appendFileLines(filename:string, content:stringList, lineEnding:string);//As above with specified line ending (will be used only if a new file is created)');
  registerRule(FILES_BUILTIN_NAMESPACE,'exec'           ,@execSync_impl,[se_executingExternal],ak_variadic_1,
                                       'exec(programPath:string);//Executes the specified program and returns the text output including stdErr output and the exitcode as a nested list: [[output,...],exitCode]#'+
                                       'exec(programPath:string,parameters:flatList);//Executes the specified program with given command line parameters#'+
                                       'exec(programPath:string,includeStdErr:boolean);//Executes the specified program and returns the text output optionally including stdErr output#'+
                                       'exec(programPath:string,parameters:flatList,parameters:flatList);//Executes the specified program with given command line parameters and returns the text output optionally including stdErr output');
  registerRule(FILES_BUILTIN_NAMESPACE,'execAsync'           ,@execAsync_impl   ,[se_executingExternal],ak_variadic_1,'execAsync(programPath:string,parameters ...);//Starts the specified program and returns void');
  registerRule(FILES_BUILTIN_NAMESPACE,'execPipeless'        ,@execPipeless_impl,[se_executingExternal],ak_variadic_1,'execPipeless(programPath:string,parameters ...);//Executes the specified program, waiting for exit and returns the exit code');
  registerRule(FILES_BUILTIN_NAMESPACE,'deleteFile'          ,@deleteFile_imp   ,[se_writeFile],ak_unary     ,'deleteFile(filename:string);//Deletes the given file, returning true on success and false otherwise');
  registerRule(FILES_BUILTIN_NAMESPACE,'deleteDir'           ,@deleteDir_imp    ,[se_writeFile],ak_unary     ,'deleteDir(directoryname:string);//Deletes the given directory, returning true on success and false otherwise');
  registerRule(FILES_BUILTIN_NAMESPACE,'copyFile'            ,@copyFile_imp     ,[se_readFile,se_writeFile],ak_binary    ,'copyFile(source:string,dest:string);//Copies a file from source to dest, returning true on success and false otherwise');
  registerRule(FILES_BUILTIN_NAMESPACE,'moveFile'            ,@moveFile_imp     ,[se_readFile,se_writeFile],ak_binary    ,'moveFile(source:string,dest:string);//Moves a file from source to dest, returning true on success and false otherwise');
  registerRule(FILES_BUILTIN_NAMESPACE,'fileInfo'            ,@fileInfo_imp     ,[se_os_query],ak_unary     ,'fileInfo(filenameOrPattern:string);//Retuns file info as a key-value-list#fileInfo(filenameOrPattern:stringList);');
  registerRule(FILES_BUILTIN_NAMESPACE,'fileStats'           ,@fileStats_imp    ,[se_readFile],ak_unary     ,'fileStats(filename:string);//Retuns a triplet [lineCount,wordCount,byteCount,hash].#fileStats(filename:stringList);');
  registerRule(FILES_BUILTIN_NAMESPACE,'expandedFileName'    ,@expandedFileName_imp    ,[],ak_unary ,'expandedFileName(F);//Returns the expanded file name of file(s) given by string or stringList F');
  registerRule(FILES_BUILTIN_NAMESPACE,'extractFileDirectory',@extractFileDirectory_imp,[],ak_unary ,'extractFileDirectory(F);//Returns the expanded file directories of file(s) given by string or stringList F');
  registerRule(FILES_BUILTIN_NAMESPACE,'extractFileName'     ,@extractFileName_imp     ,[],ak_unary ,'extractFileName(F);//Returns the expanded file names (without path) of file(s) given by string or stringList F');
  registerRule(FILES_BUILTIN_NAMESPACE,'extractFileNameOnly' ,@extractFileNameOnly_imp ,[],ak_unary ,'extractFileNameOnly(F);//Returns the expanded file names (without path and extension) of file(s) given by string or stringList F');
  registerRule(FILES_BUILTIN_NAMESPACE,'extractFileExt'      ,@extractFileExt_imp      ,[],ak_unary ,'extractFileExt(F);//Returns the extension(s) of file(s) given by string or stringList F');
  registerRule(FILES_BUILTIN_NAMESPACE,'changeFileExt'       ,@changeFileExtension_imp ,[],ak_binary,'changeFileExt(filename,newExtension);//Returns the path of file with the new extension');
  registerRule(FILES_BUILTIN_NAMESPACE,'relativeFilename'    ,@relativeFilename_impl   ,[],ak_binary,'relativeFilename(reference,file);//Returns the path of file relative to reference');

end.
