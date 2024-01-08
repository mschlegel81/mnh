UNIT fileWrappers;
INTERFACE
USES basicTypes, mySys,FileUtil,sysutils,Classes,Process,UTF8Process, myGenerics,mnh_constants,myStringUtil,LazUTF8,LazFileUtils,myCrypto;
TYPE
  P_codeProvider = ^T_codeProvider;
  T_codeProvider = object
    DESTRUCTOR destroy;                 virtual; abstract;
    FUNCTION getLines: T_arrayOfString; virtual; abstract;
    FUNCTION getPath: ansistring;       virtual; abstract;
    FUNCTION stateHash:T_hashInt;       virtual;
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
  end;

  F_newCodeProvider=FUNCTION (CONST path:ansistring):P_codeProvider;

  P_folderContents=^T_folderContents;

  { T_folderContents }

  T_folderContents=object
    folderName:string;
    dontScanBefore:double;
    Files:T_arrayOfString;
    subfolders:array of P_folderContents;
    CONSTRUCTOR create(CONST fn:string);
    CONSTRUCTOR create(CONST original:P_folderContents);
    DESTRUCTOR destroy;
    PROCEDURE updateFrom(CONST other:P_folderContents);
    PROCEDURE scan;
    FUNCTION canFind(CONST packageId:string; OUT fullPath:string; CONST allowScan:boolean; OUT scanRequired:boolean):boolean;
    FUNCTION hasSubdirectory(CONST directoryName:string; OUT subDir:P_folderContents):boolean;
    FUNCTION listIdsRecursively(CONST timeout:double):T_arrayOfString;
    FUNCTION listFilesRecursively:T_arrayOfString;
    FUNCTION getDirectSubfolder(CONST fn:string):P_folderContents;
  end;

  { T_fileCache }

  T_fileCache=object
    private
      cacheCs:TRTLCriticalSection;
      roots:array of P_folderContents;
      cachePopulated:boolean;
      scan_task_state:(stopped,running,running_stop_requested);
      FUNCTION ensurePathInCache(CONST path:string):P_folderContents;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION canLocateSource(CONST searchRoot,packageName:string; OUT foundFilePath:string):boolean;
      FUNCTION availablePackages(CONST searchRoot:string):T_arrayOfString;
      PROCEDURE postFolderToScan(CONST folderName:string);
      PROCEDURE Invalidate(CONST folderName:string);
      PROCEDURE scanInBackground;
      FUNCTION listFilesMatching(CONST toMatch:string):T_arrayOfString;
      FUNCTION getAllFilesForBackgroundScan:T_arrayOfString;
      FUNCTION allKnownFolders:T_arrayOfString;
  end;

  F_notify_event=PROCEDURE (CONST messageText:string; CONST warn:boolean);

FUNCTION newFileCodeProvider(CONST path:ansistring):P_codeProvider;
FUNCTION newVirtualFileCodeProvider(CONST path:ansistring; CONST lineData:T_arrayOfString):P_virtualFileCodeProvider;
FUNCTION fileContent(CONST name: ansistring; OUT accessed: boolean): ansistring;
PROCEDURE fileStats(CONST name:ansistring; OUT lineCount,wordCount,byteCount:int64; OUT hash:T_sha256Hash);
FUNCTION fileLines(CONST name: ansistring; OUT accessed: boolean): T_arrayOfString;
FUNCTION writeFile(CONST name, textToWrite: ansistring): boolean;
FUNCTION writeFileLines(CONST name: ansistring; CONST textToWrite: T_arrayOfString; CONST lineSeparator:string; CONST doAppend:boolean): boolean;
FUNCTION find(CONST pattern: ansistring; CONST filesAndNotFolders,recurseSubDirs: boolean): T_arrayOfString;
FUNCTION filenameToPackageId(CONST filenameOrPath:ansistring):ansistring;

FUNCTION runCommandAsyncOrPipeless(CONST executable: ansistring; CONST parameters: T_arrayOfString; CONST asynch:boolean; OUT pid:longint; CONST customFolder:string=''): int64;
PROCEDURE ensurePath(CONST path:ansistring);

VAR fileCache:T_fileCache;
    notify_event:F_notify_event=nil;
    FILE_CACHE_MAX_AGE:double=1; //one day
IMPLEMENTATION

FUNCTION cleanPath(CONST pathWithTrailingSeparator:string):string;
  begin
    if pathWithTrailingSeparator='' then exit('');
    result:=pathWithTrailingSeparator;
    while result[length(result)]=DirectorySeparator do result:=copy(result,1,length(result)-1);
  end;

PROCEDURE ensurePath(CONST path:ansistring);
  begin
    ForceDirectories(extractFilePath(expandFileName(path)));
  end;

FUNCTION newFileCodeProvider(CONST path: ansistring): P_codeProvider;
  begin new(P_fileCodeProvider(result),create(path)); end;

FUNCTION newVirtualFileCodeProvider(CONST path: ansistring; CONST lineData: T_arrayOfString): P_virtualFileCodeProvider;
  begin new(result,create(path,lineData)); end;

FUNCTION fileContent(CONST name: ansistring; OUT accessed: boolean): ansistring;
  VAR stream:TFileStream;
      size:longint;
  begin
    accessed:=false;
    try
      stream:=TFileStream.create(name, fmOpenRead or fmShareDenyNone);
      size:=stream.size;
      stream.Seek(0,soFromBeginning);
      setLength(result,size);
      if size>0 then stream.ReadBuffer(result[1],size);
      accessed:=true;
      stream.destroy;
    except
      accessed:=false;
    end;
  end;

PROCEDURE fileStats(CONST name:ansistring; OUT lineCount,wordCount,byteCount:int64; OUT hash:T_sha256Hash);
  CONST BUFFER_SIZE=8192;
  VAR buffer:array[0..BUFFER_SIZE-1] of byte;
      bufferFill:longint;
      stream:TFileStream;
      i:longint;
      b:byte;
      space:boolean=true;
      sha256:T_sha256;
  begin
    lineCount:=0;
    wordCount:=0;
    byteCount:=0;
    if trim(name) = '' then exit;
    try
      stream:=TFileStream.create(name, fmOpenRead or fmShareDenyNone);
      lineCount:=0;
      wordCount:=0;
      byteCount:=0;
      sha256.create;
      try
        stream.Seek(0,soFromBeginning);
        repeat
          bufferFill:=stream.read(buffer,BUFFER_SIZE);
          byteCount+=bufferFill;
          sha256.fillBuffer(buffer,bufferFill);
          for i:=0 to bufferFill-1 do begin
            b:=buffer[i];
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
          end;
        until bufferFill=0;;
      except
        lineCount:=0;
        wordCount:=0;
        byteCount:=0;
        for i:=0 to length(hash)-1 do hash[i]:=0;
      end;
      hash:=sha256.getHash;
      sha256.destroy;
      stream.destroy;
    except
    end;
  end;

FUNCTION fileLines(CONST name: ansistring; OUT accessed: boolean): T_arrayOfString;
  VAR stream:TFileStream;
      strings:TStringList;
      i:longint;
  begin
    strings:=TStringList.create;
    accessed:=false;
    try
      stream :=TFileStream.create(name, fmOpenRead or fmShareDenyNone);
      strings.loadFromStream(stream,true);
      stream.destroy;
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
    try
      ensurePath(name);
      stream:=TFileStream.create(name,fmCreate);
      try
        stream.Seek(0,soFromBeginning);
        if length(textToWrite)>0 then
        stream.WriteBuffer(textToWrite[1],length(textToWrite));
        result:=true;
      except
        result:=false;
      end;
      stream.destroy;
    except
      result:=false;
    end;
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
        stream:=TFileStream.create(name,fmOpenRead or fmShareDenyNone);
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
             stream:=TFileStream.create(name,fmOpenReadWrite or fmShareDenyWrite);
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
        writeln(stdErr,'Error in writeFileLines: ',e.message,'; ',e.ClassName);
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

FUNCTION runCommandAsyncOrPipeless(CONST executable: ansistring; CONST parameters: T_arrayOfString; CONST asynch:boolean; OUT pid:longint; CONST customFolder:string=''): int64;
  VAR tempProcess: TProcessUTF8;
      i: longint;
  begin
    result := $ffffffff;
    try
      {$ifdef debugMode}
      writeln('Executing: ',executable,' "',join(parameters,'" "'),'"');
      writeln('async: ',asynch,'; in folder: "',customFolder,'"');
      {$endif}
      tempProcess := TProcessUTF8.create(nil);
      tempProcess.executable := executable;
      if customFolder<>'' then tempProcess.CurrentDirectory:=customFolder;
      if asynch or not(isConsoleShowing) then tempProcess.options:=tempProcess.options +[poNewConsole];
      if not(asynch)                     then tempProcess.options:=tempProcess.options +[poWaitOnExit];
      for i := 0 to length(parameters)-1 do tempProcess.parameters.add(parameters[i]);
      tempProcess.execute;
      pid:=tempProcess.ProcessID;
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

{ T_folderContents }

CONSTRUCTOR T_folderContents.create(CONST fn: string);
  begin
    folderName:=fn;
    dontScanBefore:=0;
    setLength(Files,0);
    setLength(subfolders,0);
  end;

CONSTRUCTOR T_folderContents.create(CONST original:P_folderContents);
  begin
    create(original^.folderName);
    updateFrom(original);
  end;

DESTRUCTOR T_folderContents.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(Files)-1 do Files[i]:='';
    setLength(Files,0);
    for i:=0 to length(subfolders)-1 do dispose(subfolders[i],destroy);
    setLength(subfolders,0);
  end;

PROCEDURE T_folderContents.updateFrom(CONST other: P_folderContents);
  VAR i,j:longint;
      sub,othersub:P_folderContents;
  begin
    assert(other^.folderName=folderName);
    if other^.dontScanBefore>dontScanBefore then begin

      setLength(Files,length(other^.Files));
      for i:=0 to length(Files)-1 do Files[i]:=other^.Files[i];

      //Remove subfolders which do not exist anymore
      i:=0;
      while i<length(subfolders) do begin
        if other^.getDirectSubfolder(subfolders[i]^.folderName)<>nil
        then inc(i)
        else begin
          dispose(subfolders[i],destroy);
          for j:=i to length(subfolders)-2 do subfolders[j]:=subfolders[j+1];
          setLength(subfolders,length(subfolders)-1);
        end;
      end;

      //update/add subfolders
      for othersub in other^.subfolders do begin
        sub:=getDirectSubfolder(othersub^.folderName);
        if sub=nil then begin
          setLength(subfolders,length(subfolders)+1);
          new(subfolders[length(subfolders)-1],create(othersub));
        end else sub^.updateFrom(othersub);
      end;

      dontScanBefore:=other^.dontScanBefore;
    end;
  end;

PROCEDURE T_folderContents.scan;
  VAR _temp_list:T_arrayOfString;
      _temp_fill:longint=0;
  PROCEDURE startList;
    begin
      _temp_fill:=0;
    end;

  PROCEDURE addToList(CONST s:ansistring);
    begin
      if _temp_fill>=length(_temp_list) then setLength(_temp_list,_temp_fill+_temp_fill);
      _temp_list[_temp_fill]:=s;
      inc(_temp_fill);
    end;

  FUNCTION finalizeList:T_arrayOfString;
    VAR i:longint;
    begin
      setLength(result,_temp_fill);
      for i:=0 to _temp_fill-1 do begin result[i]:=_temp_list[i]; _temp_list[i]:=''; end;
      sortUnique(result);
    end;

  VAR info: TSearchRec;
      subPath: string;
      allFoundSubfolders:T_arrayOfString;
      i,j:longint;
      prefix: string;
  begin
    setLength(_temp_list,10);
    for i:=0 to length(Files)-1 do Files[i]:='';
    startList;
    if (findFirst(folderName+DirectorySeparator+'*'+SCRIPT_EXTENSION, faAnyFile and not(faDirectory), info) = 0)
    then repeat
      if ((info.Attr and faDirectory)<>faDirectory) then addToList(info.name);
    until (findNext(info)<>0);
    sysutils.findClose(info);
    Files:=finalizeList;
    prefix:=folderName+DirectorySeparator;
    startList;
    if findFirst(prefix+'*', faAnyFile, info) = 0
    then repeat
      if ((info.Attr and faDirectory)=faDirectory) and (info.name<>'.') and (info.name<>'..')
      then addToList(prefix+info.name);
    until (findNext(info)<>0);
    sysutils.findClose(info);
    allFoundSubfolders:=finalizeList;

    //Drop subfolders which are not found anymore...
    i:=0;
    while i<length(subfolders) do begin
      if arrContains(allFoundSubfolders, subfolders[i]^.folderName)
      then inc(i)
      else begin
        dispose(subfolders[i],destroy);
        for j:=i to length(subfolders)-2 do subfolders[j]:=subfolders[j+1];
        setLength(subfolders,length(subfolders)-1);
      end;
    end;

    //Add new subfolders
    for subPath in allFoundSubfolders do begin
      i:=0;
      while (i<length(subfolders)) and (subfolders[i]^.folderName<>subPath) do inc(i);
      if i>=length(subfolders) then begin
        setLength(subfolders,i+1);
        new(subfolders[i],create(subPath));
      end;
    end;

    for i:=0 to length(allFoundSubfolders)-1 do allFoundSubfolders[i]:='';
    allFoundSubfolders:=C_EMPTY_STRING_ARRAY;
    _temp_list        :=C_EMPTY_STRING_ARRAY;
    if length(Files)=0
    then dontScanBefore:=now+ONE_HOUR
    else dontScanBefore:=now+FILE_CACHE_MAX_AGE;

    {$ifdef debugMode}
    writeln('Path "',folderName,'" scanned. Next scan: ',DateTimeToStr(dontScanBefore));
    {$endif}
  end;

FUNCTION T_folderContents.canFind(CONST packageId: string; OUT fullPath: string; CONST allowScan:boolean; OUT scanRequired:boolean): boolean;
  VAR fileName:string;
      sub:P_folderContents;
      subscanRequired:boolean;
  begin
    scanRequired:=false;
    if (dontScanBefore<now) then begin
      if allowScan
      then scan
      else scanRequired:=true;
    end;

    for fileName in Files do if SameFileName(ExtractFileNameOnly(fileName),packageId) then begin
      fullPath:=folderName+DirectorySeparator+fileName;
      if fileExists(fullPath)
      then exit(true)
      else begin
        scanRequired:=true;
        dontScanBefore:=0;
      end;
    end;
    for sub in subfolders do begin
      if sub^.canFind(packageId, fullPath, allowScan, subscanRequired) then exit(true);
      scanRequired:=scanRequired or subscanRequired;
    end;
    result:=false;
  end;

FUNCTION T_folderContents.hasSubdirectory(CONST directoryName: string; OUT subDir: P_folderContents): boolean;
  VAR sub:P_folderContents;
  begin
    if directoryName=folderName then begin
      subDir:=@self;
      exit(true);
    end;
    if not(                               startsWith(          directoryName ,          folderName ) or
           not(FileNameCaseSensitive) and startsWith(lowercase(directoryName),lowercase(folderName)))
    then exit(false);
    if dontScanBefore<now then scan;
    for sub in subfolders do if sub^.hasSubdirectory(directoryName,subDir) then exit(true);
    result:=false;
  end;

FUNCTION T_folderContents.listIdsRecursively(CONST timeout:double): T_arrayOfString;
  VAR sub:P_folderContents;
  begin
    if (dontScanBefore<now) and (timeout<now) then scan;
    result:=Files;
    for sub in subfolders do append(result,sub^.listIdsRecursively(timeout));
  end;

FUNCTION T_folderContents.listFilesRecursively: T_arrayOfString;
  VAR i:longint;
      sub:P_folderContents;
  begin
    setLength(result,length(Files));
    for i:=0 to length(Files)-1 do result[i]:=folderName+DirectorySeparator+Files[i];
    for sub in subfolders do append(result,sub^.listFilesRecursively);
  end;

FUNCTION T_folderContents.getDirectSubfolder(CONST fn:string):P_folderContents;
  VAR sub:P_folderContents;
  begin
    result:=nil;
    for sub in subfolders do if sub^.folderName=fn then exit(sub);
  end;

{ T_fileCache }

FUNCTION T_fileCache.ensurePathInCache(CONST path: string): P_folderContents;
  VAR root:P_folderContents;
      i,j:longint;
  begin
    for root in roots do if root^.hasSubdirectory(path,result) then exit(result);
    //Directory is not listed yet: create it!
    new(result,create(path));

    //Possible an existing root is subfolder of the new result:
    i:=0;
    while i<length(roots) do
      if result^.hasSubdirectory(roots[i]^.folderName,root) then begin
        root^.updateFrom(roots[i]);
        dispose(roots[i],destroy);
        for j:=i to length(roots)-2 do roots[j]:=roots[j+1];
        setLength(roots,length(roots)-1);
      end else inc(i);

    setLength(roots,length(roots)+1);
    roots[length(roots)-1]:=result;
  end;

CONSTRUCTOR T_fileCache.create;
  begin
    initCriticalSection(cacheCs);
    cachePopulated:=false;
    scan_task_state:=stopped;
    setLength(roots,0);
    ensurePathInCache(cleanPath(configDir));
    ensurePathInCache(cleanPath(extractFilePath(paramStr(0))));
  end;

DESTRUCTOR T_fileCache.destroy;
  VAR i:longint;
  begin
    enterCriticalSection(cacheCs);
    if scan_task_state=running then begin
      scan_task_state:=running_stop_requested;
      repeat
        leaveCriticalSection(cacheCs);
        sleep(100);
        enterCriticalSection(cacheCs);
      until scan_task_state=stopped;
    end;
    for i:=0 to length(roots)-1 do dispose(roots[i],destroy);
    setLength(roots,0);
    leaveCriticalSection(cacheCs);
    doneCriticalSection(cacheCs);
  end;

FUNCTION T_fileCache.canLocateSource(CONST searchRoot, packageName: string; OUT foundFilePath: string): boolean;
  VAR scanned:T_arrayOfString;
  FUNCTION tryScan(CONST path:string):boolean;
    VAR temp:T_folderContents;
        cached:P_folderContents;
        scanRequired:boolean;
    begin
      if (path='') or arrContains(scanned,path) then exit(false);
      append(scanned,path);

      enterCriticalSection(cacheCs);
      cached:=ensurePathInCache(path);
      result:=cached^.canFind(packageName,foundFilePath,false,scanRequired);
      if result or not(scanRequired) then begin
        leaveCriticalSection(cacheCs);
        exit(result);
      end else begin
        //Some folders seem outdated, so we rescan (outside of critical section)...
        temp.create(path);
        temp.updateFrom(cached); //We update before scanning, to skip scans wherever possible
        leaveCriticalSection(cacheCs);
      end;
      result:=temp.canFind(packageName,foundFilePath,true,scanRequired);

      //After scanning, the cache might need refreshing
      enterCriticalSection(cacheCs);
      ensurePathInCache(path)^.updateFrom(@temp);
      leaveCriticalSection(cacheCs);
      temp.destroy;
    end;

  begin
    setLength(scanned,0);
    result:=tryScan(cleanPath(searchRoot                  ))
         or tryScan(cleanPath(configDir                   ))
         or tryScan(cleanPath(extractFilePath(paramStr(0))));
  end;

FUNCTION T_fileCache.availablePackages(CONST searchRoot: string): T_arrayOfString;
  VAR root:P_folderContents;
      timeout:double;
  begin
    timeout:=now+ONE_SECOND;
    enterCriticalSection(cacheCs);
    try
      result:=C_EMPTY_STRING_ARRAY;
      for root in roots do append(result,root^.listIdsRecursively(timeout));
      sortUnique(result);
    finally
      leaveCriticalSection(cacheCs);
    end;
  end;

FUNCTION fileCacheScanTask(p:pointer):ptrint;
  FUNCTION nextPathToUpdate:string;
    VAR oldest:P_folderContents=nil;
    PROCEDURE recurse(CONST folderContents:P_folderContents);
      VAR sub:P_folderContents;
      begin
        if (oldest=nil) or (oldest^.dontScanBefore>folderContents^.dontScanBefore) then begin
          oldest:=folderContents;
          if oldest^.dontScanBefore=0 then exit;
        end;
        for sub in folderContents^.subfolders do recurse(sub);
      end;

    VAR root:P_folderContents;
    begin
      enterCriticalSection(fileCache.cacheCs);
      for root in fileCache.roots do recurse(root);
      if (oldest=nil) or (oldest^.dontScanBefore>now)
      then result:=''
      else result:=oldest^.folderName;
      leaveCriticalSection(fileCache.cacheCs);
    end;

  VAR temp:T_folderContents;
  PROCEDURE updateFolder;
    VAR root, fcSub:P_folderContents;
    begin
      enterCriticalSection(fileCache.cacheCs);
      for root in fileCache.roots do if root^.hasSubdirectory(temp.folderName,fcSub) then fcSub^.updateFrom(@temp);
      leaveCriticalSection(fileCache.cacheCs);
    end;

  VAR folderToScan:string;
      i: integer;
      lastRunWasIdle:boolean=false;
  begin
    repeat
      folderToScan:=nextPathToUpdate;
      if folderToScan='' then begin
        if not(lastRunWasIdle) then begin
          if (notify_event<>nil) then notify_event('Background folder scan finished',false);
          fileCache.cachePopulated:=true;
        end;
        for i:=0 to 9 do
          if fileCache.scan_task_state<>running_stop_requested
          then sleep(1000);
        lastRunWasIdle:=true;
      end else begin
        if lastRunWasIdle and (notify_event<>nil) then notify_event('Rescanning folders ('+folderToScan+')',false);
        temp.create(folderToScan);
        temp.scan;
        updateFolder;
        temp.destroy;
        lastRunWasIdle:=false;
      end;
    until fileCache.scan_task_state=running_stop_requested;

    enterCriticalSection(fileCache.cacheCs);
    fileCache.scan_task_state:=stopped;
    leaveCriticalSection(fileCache.cacheCs);
    result:=0;
  end;

PROCEDURE T_fileCache.scanInBackground;
  begin
    enterCriticalSection(cacheCs);
    if scan_task_state=stopped then begin
      if notify_event<>nil then notify_event('Background file scan started',false);
      beginThread(@fileCacheScanTask);
      scan_task_state:=running;
    end;
    leaveCriticalSection(cacheCs);
  end;

FUNCTION T_fileCache.listFilesMatching(CONST toMatch: string): T_arrayOfString;
  VAR root:P_folderContents;
  begin
    enterCriticalSection(cacheCs);
    result:=C_EMPTY_STRING_ARRAY;
    for root in roots do append(result,root^.listFilesRecursively);
    leaveCriticalSection(cacheCs);
    result:=getListOfSimilarWords(toMatch,result,100,true);
  end;

FUNCTION T_fileCache.getAllFilesForBackgroundScan:T_arrayOfString;
  VAR root:P_folderContents;
  begin
    result:=C_EMPTY_STRING_ARRAY;
    if not(cachePopulated) then exit(result);
    enterCriticalSection(cacheCs);
    for root in roots do append(result,root^.listFilesRecursively);
    leaveCriticalSection(cacheCs);
    sortUnique(result);
  end;

FUNCTION T_fileCache.allKnownFolders:T_arrayOfString;
  VAR count:longint=0;
      cache:T_arrayOfString;
  PROCEDURE recurse(CONST p:P_folderContents);
    VAR sub:P_folderContents;
    begin
      if count>=length(cache) then setLength(cache,length(cache)*2+1);
      cache[count]:=p^.folderName;
      inc(count);
      for sub in p^.subfolders do recurse(sub);
    end;

  VAR root:P_folderContents;
  begin
    enterCriticalSection(cacheCs);
    setLength(cache,length(roots));
    for root in roots do recurse(root);
    setLength(cache,count);
    result:=cache;
    leaveCriticalSection(cacheCs);
  end;

PROCEDURE T_fileCache.postFolderToScan(CONST folderName: string);
  begin
    if folderName='' then begin
      exit;
    end;
    enterCriticalSection(cacheCs);
    ensurePathInCache(cleanPath(folderName));
    leaveCriticalSection(cacheCs);
    scanInBackground;
  end;

PROCEDURE T_fileCache.Invalidate(CONST folderName:string);
  begin
    enterCriticalSection(cacheCs);
    ensurePathInCache(cleanPath(folderName))^.dontScanBefore:=0;
    leaveCriticalSection(cacheCs);
    scanInBackground;
  end;

CONSTRUCTOR T_blankCodeProvider.create; begin end;
DESTRUCTOR T_blankCodeProvider.destroy; begin end;
FUNCTION T_blankCodeProvider.getLines: T_arrayOfString; begin result:=C_EMPTY_STRING_ARRAY; end;
FUNCTION T_blankCodeProvider.getPath: ansistring; begin result:='-'; end;
FUNCTION T_blankCodeProvider.stateHash: T_hashInt; begin result:=1; end;
FUNCTION T_blankCodeProvider.isPseudoFile: boolean; begin result:=true; end;

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

FUNCTION T_codeProvider.stateHash:T_hashInt;
  VAR s:ansistring;
      lines:T_arrayOfString;
  begin
    {$Q-}{$R-}
    lines:=getLines;
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
  fileCache.create;

FINALIZATION
  fileCache.destroy;

end.
