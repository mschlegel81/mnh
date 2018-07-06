UNIT mnh_fileWrappers;
INTERFACE
USES mnh_basicTypes, mySys,FileUtil,sysutils,Classes,Process,UTF8Process, myGenerics,mnh_constants,myStringUtil,LazUTF8,LazFileUtils;
TYPE
  P_codeProvider = ^T_codeProvider;
  T_codeProvider = object
    DESTRUCTOR destroy;                 virtual; abstract;
    FUNCTION getLines: T_arrayOfString; virtual; abstract;
    FUNCTION getPath: ansistring;       virtual; abstract;
    FUNCTION stateHash:T_hashInt;       virtual; abstract;
    FUNCTION isPseudoFile:boolean;      virtual; abstract;
    FUNCTION disposeOnPackageDestruction:boolean; virtual;
    FUNCTION id:      ansistring;
  end;

  P_fileCodeProvider=^T_fileCodeProvider;
  T_fileCodeProvider=object(T_codeProvider)
    private
      filePath: ansistring;
    public
      CONSTRUCTOR create(CONST path:ansistring);
      DESTRUCTOR destroy;                 virtual;
      FUNCTION getLines: T_arrayOfString; virtual;
      FUNCTION getPath: ansistring;       virtual;
      FUNCTION stateHash:T_hashInt;       virtual;
      FUNCTION isPseudoFile:boolean;      virtual;
  end;

  P_virtualFileCodeProvider=^T_virtualFileCodeProvider;
  T_virtualFileCodeProvider=object(T_fileCodeProvider)
    private
      lines:T_arrayOfString;
    public
      CONSTRUCTOR create(CONST path:ansistring; CONST lineData:T_arrayOfString);
      DESTRUCTOR destroy;                 virtual;
      FUNCTION getLines: T_arrayOfString; virtual;
      FUNCTION stateHash:T_hashInt;       virtual;
      FUNCTION isPseudoFile:boolean;      virtual;
  end;

  P_blankCodeProvider=^T_blankCodeProvider;
  T_blankCodeProvider=object(T_codeProvider)
    CONSTRUCTOR create;
    DESTRUCTOR destroy;                 virtual;
    FUNCTION getLines: T_arrayOfString; virtual;
    FUNCTION getPath: ansistring;       virtual;
    FUNCTION stateHash:T_hashInt;       virtual;
    FUNCTION isPseudoFile:boolean;      virtual;
    FUNCTION disposeOnPackageDestruction:boolean; virtual;
  end;

FUNCTION newFileCodeProvider(CONST path:ansistring):P_fileCodeProvider;
FUNCTION newVirtualFileCodeProvider(CONST path:ansistring; CONST lineData:T_arrayOfString):P_virtualFileCodeProvider;

FUNCTION fileContent(CONST name: ansistring; OUT accessed: boolean): ansistring;
PROCEDURE fileStats(CONST name:ansistring; OUT lineCount,wordCount,byteCount:longint; OUT hash:T_hashInt);
FUNCTION fileLines(CONST name: ansistring; OUT accessed: boolean): T_arrayOfString;
FUNCTION writeFile(CONST name, textToWrite: ansistring): boolean;
FUNCTION writeFileLines(CONST name: ansistring; CONST textToWrite: T_arrayOfString; CONST lineSeparator:string; CONST doAppend:boolean): boolean;
FUNCTION find(CONST pattern: ansistring; CONST filesAndNotFolders,recurseSubDirs: boolean): T_arrayOfString;
FUNCTION filenameToPackageId(CONST filenameOrPath:ansistring):ansistring;

FUNCTION locateSource(CONST rootPath, id: ansistring): ansistring;
FUNCTION listScriptIds(CONST rootPath: ansistring): T_arrayOfString;

FUNCTION runCommandAsyncOrPipeless(CONST executable: ansistring; CONST parameters: T_arrayOfString; CONST asynch:boolean): int64;
PROCEDURE ensurePath(CONST path:ansistring);

IMPLEMENTATION
VAR fileByIDCache:specialize G_stringKeyMap<string>;
    lastFileCacheWorkingDir:string='';
    fileByIdCs:TRTLCriticalSection;

PROCEDURE putFileCache(CONST searchRoot,searchForId,foundFile:string);
  begin
    enterCriticalSection(fileByIdCs);
    fileByIDCache.put(searchRoot+'#'+searchForId,foundFile);
    leaveCriticalSection(fileByIdCs);
  end;

FUNCTION getCachedFile(CONST searchRoot,searchForId:string):string;
  begin
    enterCriticalSection(fileByIdCs);
    if GetCurrentDir=lastFileCacheWorkingDir then begin
      if not(fileByIDCache.containsKey(searchRoot+'#'+searchForId,result)) then result:='';
    end else begin
      lastFileCacheWorkingDir:=GetCurrentDir;
      fileByIDCache.clear;
      result:='';
    end;
    leaveCriticalSection(fileByIdCs);
  end;

PROCEDURE ensurePath(CONST path:ansistring);
  begin
    ForceDirectories(extractFilePath(expandFileName(path)));
  end;

FUNCTION locateSource(CONST rootPath, id: ansistring): ansistring;
  PROCEDURE recursePath(CONST path: ansistring);
    VAR info: TSearchRec;
    begin
      if (findFirst(path+id+SCRIPT_EXTENSION, faAnyFile and not(faDirectory), info) = 0) and
         ((info.Attr and faDirectory)<>faDirectory)
      then result:=expandFileName(path+info.name);
      sysutils.findClose(info);
      if result<>'' then exit;

      if findFirst(path+'*', faAnyFile, info) = 0 then repeat
        if ((info.Attr and faDirectory)=faDirectory) and (info.name<>'.') and (info.name<>'..') then recursePath(path+info.name+DirectorySeparator);
      until (findNext(info)<>0) or (result<>'');
      sysutils.findClose(info);
    end;

  begin
    if id='' then exit('');
    result := getCachedFile(rootPath,id);
    if result<>'' then exit(result);
    if result = ''
    then recursePath(rootPath);
    if (result = '') and (configDir<>rootPath)
    then recursePath     (configDir);
    if (result = '') and (extractFilePath(paramStr(0))<>rootPath)
                     and (extractFilePath(paramStr(0))<>configDir)
    then recursePath     (extractFilePath(paramStr(0)));
    if result<>'' then putFileCache(rootPath,id,result);
  end;

FUNCTION listScriptIds(CONST rootPath: ansistring): T_arrayOfString;
  PROCEDURE recursePath(CONST path: ansistring);
    VAR info: TSearchRec;
    begin
      if (findFirst(path+'*'+SCRIPT_EXTENSION, faAnyFile and not(faDirectory), info) = 0)
      then repeat
        if ((info.Attr and faDirectory)<>faDirectory) then
        appendIfNew(result,filenameToPackageId(info.name));
      until (findNext(info)<>0) ;
      sysutils.findClose(info);

      if findFirst(path+'*', faAnyFile, info) = 0
      then repeat
        if ((info.Attr and faDirectory)=faDirectory) and (info.name<>'.') and (info.name<>'..')
        then recursePath(path+info.name+DirectorySeparator);
      until (findNext(info)<>0);
      sysutils.findClose(info);
    end;
  begin
    setLength(result,0);
    recursePath(rootPath);
    recursePath(configDir);
    recursePath(extractFilePath(paramStr(0)));
  end;

FUNCTION newFileCodeProvider(CONST path: ansistring): P_fileCodeProvider;
  begin new(result,create(path)); end;

FUNCTION newVirtualFileCodeProvider(CONST path: ansistring; CONST lineData: T_arrayOfString): P_virtualFileCodeProvider;
  begin new(result,create(path,lineData)); end;

FUNCTION fileContent(CONST name: ansistring; OUT accessed: boolean): ansistring;
  VAR stream:TMemoryStream;
      size:longint;
  begin
    stream:=TMemoryStream.create;
    accessed:=false;
    try
      stream.loadFromFile(name);
      accessed:=true;
      size:=stream.size;
      stream.Seek(0,soFromBeginning);
      setLength(result,size);
      stream.ReadBuffer(result[1],size);
    except
      accessed:=false;
    end;
    stream.destroy;
  end;

PROCEDURE fileStats(CONST name:ansistring; OUT lineCount,wordCount,byteCount:longint; OUT hash:T_hashInt);
  VAR stream:TMemoryStream;
      i:longint;
      b:byte;
      space:boolean=true;
  begin
    stream:=TMemoryStream.create;
    lineCount:=0;
    wordCount:=0;
    byteCount:=0;
    hash:=0;
    if trim(name) = '' then exit;
    try
      stream.loadFromFile(name);
      stream.Seek(0,soFromBeginning);
      byteCount:=stream.size;
      hash:=T_hashInt(lt_string)+T_hashInt(byteCount);
      for i:=1 to byteCount do begin
        b:=stream.readByte;
        case b of
        ord('a')..ord('z'),ord('A')..ord('Z'): begin
          if space then inc(wordCount);
          space:=false;
        end;
        ord(C_lineBreakChar): begin
          inc(lineCount);
          space:=true;
        end;
        else space:=true;
        end;
        {$Q-}{$R-}
        hash:=hash*31+b;
        {$Q+}{$R+}
      end;
    except
      lineCount:=0;
      wordCount:=0;
      byteCount:=0;
      hash:=0;
    end;
    stream.destroy;
  end;

FUNCTION fileLines(CONST name: ansistring; OUT accessed: boolean): T_arrayOfString;
  VAR strings:TStringList;
      i:longint;
  begin
    strings:=TStringList.create;
    accessed:=false;
    try
      strings.loadFromFile(name);
      accessed:=true;
      setLength(result,strings.count);
      for i:=0 to length(result)-1 do result[i]:=strings[i];
    except
      accessed:=false;
    end;
    strings.destroy;
  end;

FUNCTION writeFile(CONST name, textToWrite: ansistring): boolean;
  VAR stream:TFileStream;
  begin
    ensurePath(name);
    stream:=TFileStream.create(name,fmCreate);
    try
      stream.Seek(0,soFromBeginning);
      stream.WriteBuffer(textToWrite[1],length(textToWrite));
      result:=true;
    except
      result:=false;
    end;
    stream.destroy;
  end;

FUNCTION writeFileLines(CONST name: ansistring; CONST textToWrite: T_arrayOfString; CONST lineSeparator:string; CONST doAppend:boolean): boolean;
  VAR i: longint;
      textLineEnding:string;
      stream:TFileStream;

  PROCEDURE findTextLineEnding;
    FUNCTION currentTextLineEnding(): string;
      VAR stream:TFileStream;
          i:longint;
          c:array[0..1] of char=(#0,#0);
      begin
        result:='';
        stream:=TFileStream.create(name,fmOpenRead);
        try
          stream.Seek(0,soFromBeginning);
          i:=stream.size;
          while (i>0) and (result='') do begin
            c[0]:=c[1]; c[1]:=chr(stream.readByte);
            dec(i);
            if (c[0]=C_carriageReturnChar) and (c[1]=C_lineBreakChar) or
               (c[0]=C_lineBreakChar) and (c[1]=C_carriageReturnChar)
            then result:=c[0]+c[1]
            else if c[0]=C_lineBreakChar then result:=c[0];
          end;
        except
        end;
        stream.destroy;
        if result='' then result:=LineEnding;
      end;

    begin
      if doAppend and fileExists(name) then textLineEnding:=currentTextLineEnding
      else if lineSeparator<>''        then textLineEnding:=lineSeparator
      else if fileExists(name)         then textLineEnding:=currentTextLineEnding
                                       else textLineEnding:=LineEnding;
    end;

    VAR outputBuffer:array[0..9999] of char;
        outputBufferFill:longint=0;
    PROCEDURE writeChar(CONST c:char);
      begin
        outputBuffer[outputBufferFill]:=c;
        inc(outputBufferFill);
        if outputBufferFill>=length(outputBuffer) then begin
          stream.write(outputBuffer,length(outputBuffer));
          outputBufferFill:=0;
        end;
      end;

    PROCEDURE flushBuffer;
      begin
        stream.write(outputBuffer,outputBufferFill);
        outputBufferFill:=0;
      end;
    VAR c:char;
  begin
    if trim(name) = '' then exit(false);
    try
      ensurePath(name);
      findTextLineEnding;
      if doAppend and fileExists(name)
      then begin
             stream:=TFileStream.create(name,fmOpenReadWrite);
             stream.Seek(0, soEnd);
           end
      else stream:=TFileStream.create(name,fmCreate);
      for i:=0 to length(textToWrite)-1 do begin
        for c in textToWrite[i] do writeChar(c);
        for c in textLineEnding do writeChar(c);
      end;
      flushBuffer;
      stream.destroy;
      result := true;
    except
      on e:Exception do begin
        result := false;
        writeln(stdErr,'Erron in writeFile Lines: ',e.message,'; ',e.ClassName);
      end;
    end;
  end;

FUNCTION find(CONST pattern: ansistring; CONST filesAndNotFolders,recurseSubDirs: boolean): T_arrayOfString;
  VAR info: TSearchRec;
      path: ansistring;
  begin
    path := extractFilePath(pattern);
    setLength(result, 0);
    if findFirst(pattern, faAnyFile, info) = 0 then repeat
      if (info.name<>'.') and
         (info.name<>'..') and
        (((info.Attr and faDirectory) =faDirectory) and not(filesAndNotFolders) or
         ((info.Attr and faDirectory)<>faDirectory) and     filesAndNotFolders) then begin
        setLength(result, length(result)+1);
        result[length(result)-1] := path+info.name;
        if recurseSubDirs and not(filesAndNotFolders) then append(result,find(path+info.name+DirectorySeparator+'*',false,true));
      end;
    until (findNext(info)<>0);
    sysutils.findClose(info);
  end;

FUNCTION runCommandAsyncOrPipeless(CONST executable: ansistring; CONST parameters: T_arrayOfString; CONST asynch:boolean): int64;
  VAR tempProcess: TProcessUTF8;
      i: longint;
  begin
    result := $ffffffff;
    try
      tempProcess := TProcessUTF8.create(nil);
      tempProcess.executable := executable;
      if asynch or not(isConsoleShowing) then tempProcess.options:=tempProcess.options +[poNewConsole];
      if not(asynch)                     then tempProcess.options:=tempProcess.options +[poWaitOnExit];
      for i := 0 to length(parameters)-1 do tempProcess.parameters.add(parameters[i]);
      tempProcess.execute;
      while not(asynch) and tempProcess.running do sleep(1);
      result:=tempProcess.exitStatus;
      tempProcess.free;
    except
      result := $ffffffff;
    end;
  end;

FUNCTION filenameToPackageId(CONST filenameOrPath:ansistring):ansistring;
  begin
    result:=ExtractFileNameOnly(filenameOrPath);
  end;

CONSTRUCTOR T_blankCodeProvider.create; begin end;
DESTRUCTOR T_blankCodeProvider.destroy; begin end;
FUNCTION T_blankCodeProvider.getLines: T_arrayOfString; begin result:=C_EMPTY_STRING_ARRAY; end;
FUNCTION T_blankCodeProvider.getPath: ansistring; begin result:='-'; end;
FUNCTION T_blankCodeProvider.stateHash: T_hashInt; begin result:=1; end;
FUNCTION T_blankCodeProvider.isPseudoFile: boolean; begin result:=true; end;
FUNCTION T_blankCodeProvider.disposeOnPackageDestruction: boolean; begin result:=true; end;

CONSTRUCTOR T_virtualFileCodeProvider.create(CONST path: ansistring; CONST lineData: T_arrayOfString);
  begin
    inherited create(path);
    lines:=lineData;
  end;

DESTRUCTOR T_virtualFileCodeProvider.destroy;
  begin
    setLength(lines,0);
  end;

CONSTRUCTOR T_fileCodeProvider.create(CONST path: ansistring);
  begin
    filePath:=path;
  end;

DESTRUCTOR T_fileCodeProvider.destroy;
  begin
  end;

FUNCTION T_fileCodeProvider.getLines: T_arrayOfString;
  VAR accessed:boolean;
  begin
    result:=fileLines(filePath,accessed);
    if not(accessed) then exit(C_EMPTY_STRING_ARRAY);
  end;

FUNCTION T_virtualFileCodeProvider.getLines: T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,length(lines));
    for i:=0 to length(lines)-1 do result[i]:=lines[i];
  end;

FUNCTION T_fileCodeProvider.getPath: ansistring;
  begin
    result:=filePath;
  end;

FUNCTION T_fileCodeProvider.stateHash:T_hashInt;
  begin
    result:=T_hashInt(fileAge(filePath));
    if result=0 then result:=1;
  end;

FUNCTION T_virtualFileCodeProvider.stateHash:T_hashInt;
  VAR s:ansistring;
  begin
    {$Q-}{$R-}
    result:=length(lines);
    for s in lines do result:=result*31+hashOfAnsiString(s);
    {$Q+}{$R+}
    if result=0 then result:=1;
  end;

FUNCTION T_fileCodeProvider.isPseudoFile: boolean;
  begin
    result:=false;
  end;

FUNCTION T_virtualFileCodeProvider.isPseudoFile: boolean;
  begin
    result:=true;
  end;

FUNCTION T_codeProvider.disposeOnPackageDestruction: boolean;
  begin
    result:=true;
  end;

FUNCTION T_codeProvider.id: ansistring;
  begin
    result:=filenameToPackageId(getPath);
  end;

INITIALIZATION
  fileByIDCache.create();
  initialize(fileByIdCs);
  initCriticalSection(fileByIdCs);

FINALIZATION
  {$ifdef debugMode}writeln(stdErr,'finalizing mnh_fileWrappers');{$endif}
  fileByIDCache.destroy;
  doneCriticalSection(fileByIdCs);

end.
