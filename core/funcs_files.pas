UNIT funcs_files;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,Classes,Process,UTF8Process,FileUtil,{$ifdef Windows}windows,{$endif}LazFileUtils,LazUTF8,
     myGenerics,mySys,
     myCrypto,bigint,
     mnh_constants,basicTypes,
     fileWrappers,
     out_adapters,
     litVar,
     mnh_messages,
     funcs,tokenArray,
     recyclers,
     contexts,datastores;
IMPLEMENTATION
USES strutils,myStringUtil;
{$i func_defines.inc}

FUNCTION filenameResult(s:string):string;
  VAR dummy:boolean;
  begin
    result:=replaceRecursively(replaceRecursively(s,'\','/',dummy),'//','/',dummy);
  end;

FUNCTION filenameResultLiteral(CONST literalRecycler:P_literalRecycler; s:string):P_stringLiteral;
  begin
    result:=literalRecycler^.newStringLiteral(filenameResult(s));
  end;

FUNCTION filesOrDirs_impl(CONST literalRecycler:P_literalRecycler; CONST pathOrPathList:P_literal; CONST filesAndNotFolders,recurseSubDirs:boolean):P_listLiteral;
  VAR i,j:longint;
      found:T_arrayOfString;
  FUNCTION searchString(CONST index:longint):ansistring;
    begin
      if pathOrPathList^.literalType=lt_string
      then result:=P_stringLiteral(pathOrPathList)^.value
      else result:=P_stringLiteral(P_listLiteral(pathOrPathList)^.value[index])^.value;
      if not(filesAndNotFolders) and recurseSubDirs and (pos('*',result)<=0) then result:=result+DirectorySeparator+'*';
    end;

  begin
    result:=literalRecycler^.newListLiteral;
    if pathOrPathList^.literalType=lt_string then begin
      found:=find(searchString(0),filesAndNotFolders,recurseSubDirs);
      if recurseSubDirs and DirectoryExists(P_stringLiteral(pathOrPathList)^.value)
                                        then result^.append(literalRecycler,pathOrPathList,true);
      for i:=0 to length(found)-1 do result^.appendString(literalRecycler,filenameResult(found[i]));
    end else if pathOrPathList^.literalType=lt_stringList then begin
      for j:=0 to P_listLiteral(pathOrPathList)^.size-1 do begin
        found:=find(searchString(j),filesAndNotFolders,recurseSubDirs);
        if recurseSubDirs and DirectoryExists(P_stringLiteral(P_listLiteral(pathOrPathList)^.value[j])^.value)
        then result^.append(literalRecycler,P_listLiteral(pathOrPathList)^.value[j],true);
        for i:=0 to length(found)-1 do result^.appendString(literalRecycler,filenameResult(found[i]));
      end;
    end;
  end;

FUNCTION files_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string, lt_stringList, lt_emptyList])
    then result:=filesOrDirs_impl(recycler,arg0,true,false);
  end;

FUNCTION allFiles_impl intFuncSignature;
  VAR pattern:string='';
      recurse:boolean=true;
      i:longint;

  PROCEDURE searchInRoot(CONST root:string);
    VAR list:TStringList;
        k:longint;
    begin
      list:=FindAllFiles(root,pattern,recurse,faAnyFile and not faArchive);
      for k:=0 to list.count-1 do listResult^.appendString(recycler,filenameResult(list[k]));
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
            pattern:=P_stringLiteral(list1^.value[0])^.value;
            for i:=1 to list1^.size-1 do pattern:=pattern+';'+P_stringLiteral(list1^.value[i])^.value;
          end;
          else exit(nil);
        end;
      end;
      if (params^.size>=3) then begin
        if arg2^.literalType<>lt_boolean then exit(nil);
        recurse:=P_boolLiteral(arg2)^.value;
      end;
      result:=recycler^.newListLiteral;
      if arg0^.literalType=lt_string
      then searchInRoot(str0^.value)
      else for i:=0 to list0^.size-1 do searchInRoot(P_stringLiteral(list0^.value[i])^.value);
    end;
  end;

FUNCTION folders_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string, lt_stringList, lt_emptyList])
    then result:=filesOrDirs_impl(recycler,arg0,false,false);
  end;

FUNCTION allFolders_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string, lt_stringList, lt_emptyList])
    then result:=filesOrDirs_impl(recycler,arg0,false,true);
  end;

FUNCTION fileExists_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (arg0^.literalType=lt_string)
      then result:=newBoolLiteral(fileExists(str0^.value))
      else result:=genericVectorization('fileExists',params,tokenLocation,context,recycler);
    end;
  end;

FUNCTION folderExists_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (arg0^.literalType=lt_string)
      then result:=newBoolLiteral(DirectoryExists(str0^.value))
      else result:=genericVectorization('folderExists',params,tokenLocation,context,recycler);
    end;
  end;

FUNCTION fileContents_impl intFuncSignature;
  VAR accessed:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context^.checkSideEffects('fileContents',tokenLocation,[se_readFile]) then begin
      result:=recycler^.newStringLiteral(fileContent(str0^.value,accessed));
      if not(accessed) then begin
        context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'File "'+str0^.value+'" cannot be accessed');
        recycler^.disposeLiteral(result);
        result:=recycler^.newStringLiteral('');
      end;
    end;
  end;

FUNCTION readDatastore_impl intFuncSignature;
  VAR meta:T_datastoreMeta;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string) then begin
      meta.create(str0^.value,str1^.value);
      result:=meta.readValue(tokenLocation,context,recycler);
      meta.destroy;
      if (result=nil) and (context^.continueEvaluation) then begin
        result:=newVoidLiteral;
        context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'Datastore for script '+str0^.toString()+' and rule '+str1^.toString()+' does not exist');
      end;
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      meta.create('dummyPackage','');
      result:=meta.readFromSpecificFileIncludingId(str0^.value,tokenLocation,context,recycler);
      meta.destroy;
    end;
  end;

FUNCTION serialize_impl intFuncSignature;
  VAR deflate:boolean=true;
      reuse:boolean=false;
  VAR void:P_literal;
  begin
    if (params<>nil) and (params^.size>=1)
    then begin
      if (params^.size>=2) then begin
        if arg1^.literalType=lt_boolean
        then deflate:=boolLit[true].equals(bool1)
        else exit(nil);
      end;
      if (params^.size>=3) then begin
        if arg2^.literalType=lt_boolean
        then reuse:=boolLit[true].equals(bool2)
        else exit(nil);
      end;
      result:=recycler^.newStringLiteral(serialize(arg0,tokenLocation,context^.messages,deflate,reuse))
    end
    else if (params=nil) then begin
      void:=newVoidLiteral;
      result:=recycler^.newStringLiteral(serialize(void,tokenLocation,context^.messages,true,false));
      recycler^.disposeLiteral(void);
    end else result:=nil;
  end;

FUNCTION deserialize_impl intFuncSignature;
  VAR typeMap:T_typeMap;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then begin
      typeMap:=P_abstractPackage(tokenLocation.package)^.getTypeMap;
      result:=deserialize(P_stringLiteral(arg0)^.value,tokenLocation,context^.messages,typeMap);
      typeMap.destroy;
    end else result:=nil;
  end;

FUNCTION fileLines_impl intFuncSignature;
  VAR accessed:boolean;
      L:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context^.checkSideEffects('fileLines',tokenLocation,[se_readFile]) then begin
      L:=fileLines(str0^.value,accessed);
      result:=recycler^.newListLiteral;
      for i:=0 to length(L)-1 do listResult^.appendString(recycler,L[i]);
      if not(accessed) then begin
        context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'File "'+str0^.value+'" cannot be accessed');
        recycler^.disposeLiteral(result);
        result:=recycler^.newListLiteral;
      end;
    end;
  end;

FUNCTION writeFile_impl intFuncSignature;
  VAR ok:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string)
                                          and (arg1^.literalType=lt_string)
      and context^.checkSideEffects('writeFile',tokenLocation,[se_writeFile]) then begin
      ok:=fileWrappers.writeFile(str0^.value,
                                 str1^.value);
      result:=newBoolLiteral(ok);
      if not(ok) then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'File "'+str0^.value+'" cannot be accessed');
    end;
  end;

FUNCTION writeOrAppendFileLines(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST doAppend:boolean):P_literal;
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
      for i:=0 to length(L)-1 do L[i]:=P_stringLiteral(list1^.value[i])^.value;
      ok:=writeFileLines(str0^.value,L,sep,doAppend);
      result:=newBoolLiteral(ok);
      if not(ok) then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'File "'+str0^.value+'" cannot be accessed');
    end;
  end;

FUNCTION writeFileLines_impl intFuncSignature;
  begin if context^.checkSideEffects('writeFileLines' ,tokenLocation,[se_writeFile]            ) then result:=writeOrAppendFileLines(params,tokenLocation,context,false) else result:=nil; end;
FUNCTION appendFileLines_impl intFuncSignature;
  begin if context^.checkSideEffects('appendFileLines',tokenLocation,[se_readFile,se_writeFile]) then result:=writeOrAppendFileLines(params,tokenLocation,context,true)  else result:=nil; end;

FUNCTION setFileDate_impl intFuncSignature;
  begin
    result:=nil;
    if not(context^.checkSideEffects('setFileDate',tokenLocation,[se_writeFile]))
    then exit(nil);
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType in [lt_real,lt_smallint]) then begin
      result:=newBoolLiteral(FileSetDate(str0^.value,DateTimeToFileDate(P_numericLiteral(arg1)^.floatValue))=0);
    end else result:=genericVectorization('setFileDate',params,tokenLocation,context,recycler);
  end;

TYPE
  T_execParameters=record
    executable:string;
    cmdLinePar:T_arrayOfString;
    prio:TProcessPriority;
    teeRoutine:P_expressionLiteral;
    includeStdErr:boolean;
  end;

FUNCTION canExtractExecParameters(CONST params:P_listLiteral; CONST allowTee,allowStderr:boolean; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST id_for_side_effect_checking:string; OUT exec_params:T_execParameters):boolean;
  VAR k: integer;
      i:longint;
  begin
    result:=false;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string)
       and context^.checkSideEffects('id_for_side_effect_checking',tokenLocation,[se_executingExternal]) then begin
      exec_params.prio:=ppNormal;
      exec_params.teeRoutine:=nil;
      exec_params.includeStdErr:=false;
      setLength(exec_params.cmdLinePar,0);
      exec_params.executable:=str0^.value;
      for k:=1 to params^.size-1 do case params^.value[k]^.literalType of
        lt_booleanList,lt_intList,lt_realList,lt_stringList: if k=1 then begin
          setLength(exec_params.cmdLinePar,list1^.size);
          for i:=0 to list1^.size-1 do begin
            if list1^.value[i]^.literalType=lt_string
            then exec_params.cmdLinePar[i]:=P_stringLiteral(list1^.value[i])^.value
            else exec_params.cmdLinePar[i]:=list1^.value[i]^.toString();
          end;
        end else exit(false);
        lt_boolean:
          if allowStderr
          then exec_params.includeStdErr:=P_boolLiteral(params^.value[k])^.value
          else exit(false);
        lt_expression:
          if allowTee
          then exec_params.teeRoutine:=P_expressionLiteral(params^.value[k])
          else exit(false);
        lt_smallint: begin
          if      P_smallIntLiteral(params^.value[k])^.value<0 then exec_params.prio:=ppIdle
          else if P_smallIntLiteral(params^.value[k])^.value>0 then exec_params.prio:=ppHigh;
        end
        else exit(false);
      end;
      result:=true;
    end;
  end;

FUNCTION internalExec(CONST params:T_execParameters; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler; CONST tee:boolean):P_literal;
  FUNCTION runCommand(OUT output: TStringList): int64;
    CONST READ_BYTES = 8192;
    VAR ReadBuffer:array[0..READ_BYTES-1] of char;
        memStream  : TMemoryStream;
        tempProcess: TProcessUTF8;
        teebuffer  : ansistring='';

    PROCEDURE flushTeeBuffer;
      VAR wrappedTeeBuffer:P_stringLiteral;
          teeCallResult:T_evaluationResult;
      begin
        if (length(teebuffer)>0) and (teebuffer[length(teebuffer)]=#13)
        then setLength(teebuffer,length(teebuffer)-1);
        if params.teeRoutine=nil then writeln(teebuffer)
        else begin
          wrappedTeeBuffer:=recycler^.newStringLiteral(teebuffer);
          teeCallResult:=evaluteExpression(params.teeRoutine,tokenLocation,context,recycler,wrappedTeeBuffer);
          if teeCallResult.literal<>nil then recycler^.disposeLiteral(teeCallResult.literal);
          recycler^.disposeLiteral(wrappedTeeBuffer);
        end;
        teebuffer:='';
      end;

    FUNCTION flushStdOut:boolean;
      VAR k:longint;
          n:longint;
      begin
        result:=false;
        while tempProcess.output.NumBytesAvailable>0 do begin
          n:=tempProcess.output.read(ReadBuffer,READ_BYTES);
          if n>0 then begin
            memStream.write(ReadBuffer, n);
            result:=true;
            if tee then begin
              for k:=0 to n-1 do begin
                if ReadBuffer[k]=#10 then flushTeeBuffer
                                     else teebuffer+=ReadBuffer[k];
              end;
            end;
          end;
        end;
        if not(params.includeStdErr) then while tempProcess.stdErr.NumBytesAvailable>0 do begin
          tempProcess.stdErr.read(ReadBuffer,READ_BYTES);
          result:=true;
        end;
      end;

    VAR i:longint;
    begin
      initialize(ReadBuffer);
      memStream := TMemoryStream.create;
      tempProcess := TProcessUTF8.create(nil);
      tempProcess.Priority:=params.prio;
      tempProcess.executable := params.executable;
      for i := 0 to length(params.cmdLinePar)-1 do tempProcess.parameters.add(params.cmdLinePar[i]);
      if params.includeStdErr
      then tempProcess.options := [poUsePipes, poStderrToOutPut]
      else tempProcess.options := [poUsePipes];
      tempProcess.ShowWindow := swoHIDE;
      try
        tempProcess.execute;
        tempProcess.CloseInput;
        tempProcess.PipeBufferSize:=READ_BYTES;
        while tempProcess.running and context^.continueEvaluation do begin
          if not(flushStdOut) then sleep(1);
        end;
        if tempProcess.running then begin
          {$ifdef debugMode}
          context^.messages^.postTextMessage(mt_el1_note,tokenLocation,'Trying to stop process '+intToStr(tempProcess.ProcessID));
          {$endif}
          tempProcess.Terminate(999);
        end;
        flushStdOut;
        result := tempProcess.ExitCode;
      except
        result := $ffffffff;
      end;
      tempProcess.free;
      memStream.position:=0;
      output := TStringList.create;
      output.loadFromStream(memStream);
      memStream.free;
      if tee and (length(teebuffer)>0) then flushTeeBuffer;
    end;

  VAR output:TStringList;
      outputLit:P_listLiteral;
      i:longint;
      processExitCode:int64;
  begin
    if tee and (params.teeRoutine=nil) and not(mySys.isConsoleShowing) then mySys.showConsole;
    processExitCode:=runCommand(output);
    outputLit:=recycler^.newListLiteral(output.count);
    for i:=0 to output.count-1 do outputLit^.appendString(recycler,output[i]);
    result:=recycler^.newListLiteral(2)^
      .append   (recycler,outputLit,false)^
      .appendInt(recycler,processExitCode);
    output.free;
  end;

FUNCTION execSync_impl    intFuncSignature;
  VAR exec_params: T_execParameters;
  begin
    if canExtractExecParameters(params,false,true,tokenLocation,context,'exec',exec_params)
    then result:=internalExec(exec_params,tokenLocation,context,recycler,false)
    else result:=nil;
  end;

FUNCTION teeExecSync_impl intFuncSignature;
  VAR exec_params: T_execParameters;
  begin
    if canExtractExecParameters(params,true,true,tokenLocation,context,'teeExec',exec_params)
    then result:=internalExec(exec_params,tokenLocation,context,recycler,true)
    else result:=nil;
  end;

FUNCTION execAsyncOrPipeless(CONST exec_params: T_execParameters; CONST doAsynch:boolean; CONST literalRecycler:P_literalRecycler):P_literal;
  VAR pid:longint;
      processExitCode:int64;
      consoleShowedBefore:boolean;
  begin
    consoleShowedBefore:=mySys.isConsoleShowing;
    if not consoleShowedBefore then mySys.showConsole;
    processExitCode:=runCommandAsyncOrPipeless(exec_params.executable,exec_params.cmdLinePar,exec_params.prio,doAsynch,pid);
    if doAsynch
    then result:=literalRecycler^.newIntLiteral(pid)
    else result:=literalRecycler^.newIntLiteral(processExitCode);
    if not consoleShowedBefore then mySys.hideConsole;
  end;

FUNCTION execAsync_impl intFuncSignature;
  VAR exec_params: T_execParameters;
  begin
    if canExtractExecParameters(params,false,false,tokenLocation,context,'execAsync',exec_params)
    then result:=execAsyncOrPipeless(exec_params,true,recycler)
    else result:=nil;
  end;

FUNCTION execPipeless_impl intFuncSignature;
  VAR exec_params: T_execParameters;
  begin
   if canExtractExecParameters(params,false,false,tokenLocation,context,'execPipeless',exec_params)
   then result:=execAsyncOrPipeless(exec_params,false,recycler)
   else result:=nil;
  end;

FUNCTION deleteFile_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context^.checkSideEffects('deleteFile',tokenLocation,[se_writeFile]) then begin
      result:=newBoolLiteral(DeleteFileUTF8(str0^.value));
    end;
  end;

FUNCTION deleteDir_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context^.checkSideEffects('deleteDir',tokenLocation,[se_writeFile]) then begin
      result:=newBoolLiteral(DeleteDirectory(str0^.value,false));
    end;
  end;

FUNCTION copyFile_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string) and context^.checkSideEffects('copyFile',tokenLocation,[se_readFile,se_writeFile]) then begin
      ensurePath(str1^.value);
      result:=newBoolLiteral(
      FileUtil.CopyFile(str0^.value,str1^.value,true));
    end;
  end;

FUNCTION moveFile_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string) and context^.checkSideEffects('moveFile',tokenLocation,[se_readFile,se_writeFile]) then begin
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
        a:=recycler^.newSetLiteral(4);
        for att in info.attributes do a^.appendString(recycler,C_fileAttribName[att]);
        result:=recycler^.newMapLiteral(4)^
                .put(recycler,'path'      ,info.filePath)^
                .put(recycler,'time'      ,info.time)^
                .put(recycler,'size'      ,info.size)^
                .put(recycler,'attributes',a,false);
      end; //infoToLiteral

    VAR info:T_fileInfo;
    begin
      result:=nil;
      if containsPlaceholder(s) then begin
        result:=recycler^.newListLiteral(0);
        for info in findFileInfo(s) do listResult^.append(recycler,infoToLiteral(info),false);
      end else for info in findFileInfo(s) do exit(infoToLiteral(info));
    end; //infoForSearch

  VAR iter:T_arrayOfLiteral;
      sub :P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_string: exit(infoForSearch(str0^.value));
      lt_stringList,lt_stringSet: begin
        result:=collection0^.newOfSameType(recycler,true);
        iter  :=collection0^.tempIterableList;
        for sub in iter do collResult^.append(recycler,infoForSearch(P_stringLiteral(sub)^.value),false);
      end;
      lt_emptyList: result:=recycler^.newListLiteral(0);
      lt_emptySet : result:=recycler^.newSetLiteral(0);
    end;
  end; //fileInfo_imp

FUNCTION fileStats_imp intFuncSignature;
  VAR lineCount,wordCount,byteCount:int64;
      i:longint;
      sha256Int:T_bigInt;
      sha256Digest:T_sha256Hash;
      sha256Digits:T_arrayOfLongint;
  begin
    if not(context^.checkSideEffects('fileStats',tokenLocation,[se_readFile])) then exit(nil);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      fileStats(str0^.value,lineCount,wordCount,byteCount,sha256Digest);
      setLength(sha256Digits,length(sha256Digest));
      for i:=0 to length(sha256Digest)-1 do sha256Digits[i]:=sha256Digest[i];
      sha256Int.createFromDigits(256,sha256Digits);
      result:=recycler^.newListLiteral^
        .appendInt(recycler,lineCount)^
        .appendInt(recycler,wordCount)^
        .appendInt(recycler,byteCount)^
        .append   (recycler,recycler^.newIntLiteral(sha256Int),false);
    end else result:=genericVectorization('fileStats',params,tokenLocation,context,recycler)
  end;

FUNCTION expandedFileName_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=filenameResultLiteral(recycler,expandFileName(str0^.value))
    else result:=genericVectorization('expandedFileName',params,tokenLocation,context,recycler);
  end;

FUNCTION extractFileDirectory_imp intFuncSignature;
  FUNCTION internal(CONST s:string):string;
    begin
      if ExtractFileDir(s)=''
      then result:='.'
      else result:=filenameResult(ExtractFileDir(s));
    end;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=recycler^.newStringLiteral(internal(str0^.value))
    else result:=genericVectorization('extractFileDirectory',params,tokenLocation,context,recycler);
  end;

FUNCTION extractFileName_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=filenameResultLiteral(recycler,extractFileName(str0^.value))
    else result:=genericVectorization('extractFileName',params,tokenLocation,context,recycler);
  end;

FUNCTION extractFileNameOnly_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=filenameResultLiteral(recycler,ExtractFileNameOnly(str0^.value))
    else result:=genericVectorization('extractFileNameOnly',params,tokenLocation,context,recycler);
  end;

FUNCTION extractFileExt_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=filenameResultLiteral(recycler,extractFileExt(str0^.value))
    else result:=genericVectorization('extractFileExt',params,tokenLocation,context,recycler);
  end;

FUNCTION changeFileExtension_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string)
    then result:=recycler^.newStringLiteral(ChangeFileExt(str0^.value,str1^.value))
    else result:=genericVectorization('changeFileExt',params,tokenLocation,context,recycler);
  end;

FUNCTION relativeFilename_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string)
    then result:=filenameResultLiteral(recycler,extractRelativePath(str0^.value+'/',str1^.value))
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
         then result:=filenameResultLiteral(recycler,extractRelativePath(GetCurrentDirUTF8+'/',str0^.value))
         else result:=genericVectorization('relativeFileName',params,tokenLocation,context,recycler);
  end;

FUNCTION systemSpecificFilename_impl intFuncSignature;
  FUNCTION convert(CONST s:P_stringLiteral):P_literal;
    begin
      if DirectorySeparator='/'
      then exit(filenameResultLiteral(recycler,s^.value))
      else exit(recycler^.newStringLiteral(ansiReplaceStr(filenameResult(s^.value),'/',DirectorySeparator)));
    end;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=convert(str0)
    else result:=genericVectorization('systemSpecificFilename',params,tokenLocation,context,recycler);
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'files'          ,@files_impl         ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'allFiles'       ,@allFiles_impl      ,ak_variadic_1);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'folders'        ,@folders_impl       ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'allFolders'     ,@allFolders_impl    ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'fileExists'     ,@fileExists_impl    ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'folderExists'   ,@folderExists_impl  ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'fileContents'   ,@fileContents_impl  ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'readDatastore'  ,@readDatastore_impl  ,ak_variadic_1);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'serialize'     ,@serialize_impl   ,ak_variadic_1  );
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'deserialize'   ,@deserialize_impl ,ak_unary   );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'fileLines'      ,@fileLines_impl     ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'writeFile'      ,@writeFile_impl     ,ak_binary    );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'writeFileLines' ,@writeFileLines_impl,ak_variadic_2);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'appendFileLines',@appendFileLines_impl,ak_variadic_2);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'exec'           ,@execSync_impl,ak_variadic_1);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'teeExec'        ,@teeExecSync_impl,ak_variadic_1);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'execAsync'           ,@execAsync_impl   ,ak_variadic_1);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'execPipeless'        ,@execPipeless_impl,ak_variadic_1);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'deleteFile'          ,@deleteFile_imp   ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'deleteDir'           ,@deleteDir_imp    ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'copyFile'            ,@copyFile_imp     ,ak_binary    );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'moveFile'            ,@moveFile_imp     ,ak_binary    );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'fileInfo'            ,@fileInfo_imp     ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'fileStats'           ,@fileStats_imp    ,ak_unary     );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'expandedFileName'    ,@expandedFileName_imp    ,ak_unary );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'extractFileDirectory',@extractFileDirectory_imp,ak_unary );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'extractFileName'     ,@extractFileName_imp     ,ak_unary );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'extractFileNameOnly' ,@extractFileNameOnly_imp ,ak_unary );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'extractFileExt'      ,@extractFileExt_imp      ,ak_unary );
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'changeFileExt'       ,@changeFileExtension_imp ,ak_binary);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'relativeFileName'    ,@relativeFilename_impl   ,ak_variadic_1);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'systemSpecificFilename',@systemSpecificFilename_impl,ak_unary);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'setFileDate'           ,@setFileDate_impl           ,ak_binary);
end.
