UNIT mnh_fileWrappers;
INTERFACE
USES sysutils,classes,process;
TYPE
  T_stringList=array of ansistring;

  P_codeProvider=^T_codeProvider;

  { T_codeProvider }

  T_codeProvider=object
    private
      lock:TThreadID;
      filepath:ansistring;
      lineData:T_stringList;
      syncedFileAge:double;
      version:longint;

    public
      CONSTRUCTOR create;
      CONSTRUCTOR create(CONST path:ansistring);
      DESTRUCTOR destroy;
      FUNCTION getLines:T_stringList;
      PROCEDURE setLines(CONST value:T_stringList);
      PROCEDURE setLines(CONST value:TStrings);
      PROCEDURE setLines(CONST value:ansistring);

      PROCEDURE setPath(CONST path:ansistring);
      FUNCTION getPath:ansistring;
      PROCEDURE load;
      PROCEDURE save;
      FUNCTION filename:ansistring;
      FUNCTION fileHasChanged:boolean;
      FUNCTION getVersion(CONST reloadIfNecessary:boolean):longint;
      FUNCTION id:ansistring;
      PROCEDURE clear;
  end;

FUNCTION fileContent(CONST name:ansistring; OUT accessed:boolean):ansistring;
FUNCTION fileLines  (CONST name:ansistring; OUT accessed:boolean):T_stringList;
FUNCTION writeFile     (CONST name,textToWrite:ansistring):boolean;
FUNCTION writeFileLines(CONST name:ansistring; CONST textToWrite:T_stringList):boolean;
FUNCTION find(CONST pattern:ansistring; CONST filesAndNotFolders:boolean):T_stringList;

PROCEDURE clearSourceScanPaths;
PROCEDURE addSourceScanPath(CONST path:ansistring);
FUNCTION locateSource(CONST id:ansistring):ansistring;

FUNCTION runCommandAsync(CONST executable:ansistring; CONST parameters:T_stringList):boolean;
FUNCTION runCommand(CONST executable:ansistring; CONST parameters:T_stringList; OUT output:TStringList):boolean;

IMPLEMENTATION
VAR sourceScanPath:T_stringList;
PROCEDURE clearSourceScanPaths;
  begin
    setLength(sourceScanPath,0);
    addSourceScanPath(extractFilePath(paramstr(0)));
    addSourceScanPath('');
  end;

PROCEDURE addSourceScanPath(CONST path:ansistring);
  VAR expandedPath:ansistring;
      i:longint;
  begin
    expandedPath:=expandFileName(path);
    for i:=0 to length(sourceScanPath)-1 do if sourceScanPath[i]=expandedPath then exit;
    setLength(sourceScanPath,length(sourceScanPath)+1);
    sourceScanPath[length(sourceScanPath)-1]:=expandedPath;
  end;

FUNCTION locateSource(CONST id:ansistring):ansistring;
  CONST sourceExt='.MNH';

  FUNCTION nameToId(CONST fname:ansistring):ansistring;
    begin
      if uppercase(extractFileExt(fname))=sourceExt then begin
        result:=extractFileName(fname);
        result:=copy(result,1,length(result)-length(sourceExt));
      end else result:='';
      if UpperCase(result)='MNH' then result:='';
    end;

  PROCEDURE recursePath(CONST path:ansistring);
    VAR info   :TSearchRec;
    begin
      if findFirst(path+'*',faAnyFile,info)=0 then repeat        
        if (info.attr and faDirectory)=faDirectory then begin
          if (info.name<>'.') and (info.name<>'..')
          then recursePath(path+info.name+DirectorySeparator);
        end else if nameToId(info.name)=id then result:=path+info.name;
      until (findNext(info)<>0) or (result<>'');
      sysutils.findClose(info);
    end;

  VAR i:longint;
  begin
    result:='';
    for i:=0 to length(sourceScanPath)-1 do
    if result='' then recursePath(sourceScanPath[i]);
  end;

  
FUNCTION fileContent(CONST name:ansistring; OUT accessed:boolean):ansistring;
  VAR handle:file of char;
      block:array[0..1023] of char;
      actuallyRead,i:longint;
  begin    
    if trim(name)='' then begin
      accessed:=false;
      exit;
    end;
    try
      accessed:=true;
      assign(handle,name);
      reset(handle);
      result:='';
      repeat
        blockread(handle,block,length(block),actuallyRead);
        for i:=0 to actuallyRead-1 do result:=result+block[i];
      until actuallyRead<length(block);
      close(handle);
    except
      accessed:=false;
      result:='';
    end;
  end;
  
FUNCTION fileLines  (CONST name:ansistring; OUT accessed:boolean):T_stringList;
  VAR handle:textFile;
  begin
    setLength(result,0);
    if trim(name)='' then begin
      accessed:=false;
      exit;
    end;
    try
      accessed:=true;
      assign(handle,name);
      reset(handle);
      while not(eof(handle)) do begin
        setLength(result,length(result)+1);
        readln(handle,result[length(result)-1]);
      end;
      close(handle);
    except
      accessed:=false;
      setLength(result,0);
    end;
  end;
  
FUNCTION writeFile(CONST name,textToWrite:ansistring):boolean;
  VAR handle:file of char;
      block:array[0..1023] of char;
      i,j:longint;
  begin
    if trim(name)='' then exit(false);
    try
      result:=true;
      assign(handle,name);
      rewrite(handle);
      i:=1;
      while i<=length(textToWrite) do begin
        j:=0;
        while (i<=length(textToWrite)) and (j<length(block)) do begin
          block[j]:=textToWrite[i];
          inc(i);
          inc(j);
        end;
        blockwrite(handle,block,j);
      end;
      close(handle);
    except
      result:=false;
    end;
  end;

FUNCTION writeFileLines(CONST name:ansistring; CONST textToWrite:T_stringList):boolean;
  VAR handle:TextFile;
      i:longint;
  begin
    if trim(name)='' then exit(false);
    try
      assign(handle,name);
      rewrite(handle);
      for i:=0 to length(textToWrite)-1 do writeln(handle,textToWrite[i]);
      close(handle);
      result:=true;
    except
      result:=false;
    end;
  end;

FUNCTION find(CONST pattern:ansistring; CONST filesAndNotFolders:boolean):T_stringList;
  VAR info   :TSearchRec;
      path:ansistring;
  begin
    path:=ExtractFilePath(pattern);
    setLength(result,0);
    if findFirst(pattern,faAnyFile,info)=0 then repeat
      if (info.name<>'.') and (info.name<>'..') and
         (((info.attr and faDirectory)= faDirectory) and not(filesAndNotFolders) or
          ((info.attr and faDirectory)<>faDirectory) and     filesAndNotFolders ) then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=path+info.name;
      end;
    until (findNext(info)<>0);
    sysutils.findClose(info);
  end;

FUNCTION runCommandAsync(CONST executable:ansistring; CONST parameters:T_stringList):boolean;
  VAR tempProcess:TProcess;
      i:longint;
  begin
    result:=true;
    try
      tempProcess :=TProcess.Create(nil);
      tempProcess.Executable:=executable;
      for i:=0 to length(parameters)-1 do  tempProcess.Parameters.Add(parameters[i]);
      tempProcess.execute;
      tempProcess.Free;
    except
      result:=false;
    end;
  end;

FUNCTION runCommand(CONST executable:ansistring; CONST parameters:T_stringList; OUT output:TStringList):boolean;
  CONST READ_BYTES = 2048;
  VAR memStream: TMemoryStream;
      tempProcess: TProcess;
      n: LongInt;
      BytesRead: LongInt;
  begin
    memStream := TMemoryStream.Create;
    BytesRead := 0;
    tempProcess := TProcess.Create(nil);
    tempProcess.Executable:=executable;
    for n:=0 to length(parameters)-1 do  tempProcess.Parameters.Add(parameters[n]);
    tempProcess.Options := [poUsePipes,poStderrToOutPut];
    tempProcess.ShowWindow:=swoHIDE;
    try
      tempProcess.Execute;
      while tempProcess.Running do begin
        memStream.SetSize(BytesRead + READ_BYTES);
        n := tempProcess.Output.Read((memStream.Memory + BytesRead)^, READ_BYTES);
        if n>0  then Inc(BytesRead, n) else Sleep(10);
      end;
      repeat
        memStream.SetSize(BytesRead + READ_BYTES);
        n := tempProcess.Output.Read((memStream.Memory + BytesRead)^, READ_BYTES);
        if n > 0 then Inc(BytesRead, n);
      until n <= 0;
      result:=(tempProcess.ExitStatus=0);
    except
      result:=false;
    end;
    tempProcess.Free;
    memStream.SetSize(BytesRead);
    output := TStringList.Create;
    output.LoadFromStream(memStream);
    memStream.Free;
  end;

{ T_codeProvider }

FUNCTION T_codeProvider.getLines: T_stringList;
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock:=ThreadID until lock=ThreadID;
    result:=lineData;
    repeat lock:=0 until lock=0;
  end;

PROCEDURE T_codeProvider.setLines(CONST value: T_stringList);
  VAR i:longint;
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock:=ThreadID until lock=ThreadID;
    setLength(lineData,length(value));
    for i:=0 to length(value)-1 do lineData[i]:=value[i];
    inc(version);
    repeat lock:=0 until lock=0;
  end;

PROCEDURE T_codeProvider.setLines(CONST value: TStrings);
  VAR i:longint;
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock:=ThreadID until lock=ThreadID;
    setLength(lineData,value.Count);
    for i:=0 to value.Count-1 do lineData[i]:=value[i];
    inc(version);
    repeat lock:=0 until lock=0;
  end;
  
PROCEDURE T_codeProvider.setLines(CONST value:ansistring);
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock:=ThreadID until lock=ThreadID;
    setLength(lineData,1);
    lineData[0]:=value;
    inc(version);
    repeat lock:=0 until lock=0;
  end;
  
CONSTRUCTOR T_codeProvider.create;
  begin
    lock:=0;
    clear;
  end;

CONSTRUCTOR T_codeProvider.create(CONST path: ansistring);
  begin
    lock:=0;
    clear;
    filepath:=path;
    if FileExists(path) then load;
  end;

DESTRUCTOR T_codeProvider.destroy;
  begin
    setLength(lineData,0);
  end;

PROCEDURE T_codeProvider.setPath(CONST path: ansistring);
  begin
    filepath:=path;
  end;

FUNCTION T_codeProvider.getPath: ansistring;
  begin
    result:=filepath;
  end;

PROCEDURE T_codeProvider.load;
  VAR accessed:boolean;
      L:T_stringList;
      i:longint;
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock:=ThreadID until lock=ThreadID;
    L:=fileLines(filepath,accessed);
    if accessed then begin
      setLength(lineData,length(L));
      for i:=0 to length(L)-1 do lineData[i]:=L[i];
      FileAge(filepath,syncedFileAge);
      inc(version);
    end;
    repeat lock:=0 until lock=0;
  end;

PROCEDURE T_codeProvider.save;
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock:=ThreadID until lock=ThreadID;
    if (filepath<>'') and writeFileLines(filepath,lineData) then
      FileAge(filepath,syncedFileAge);
    repeat lock:=0 until lock=0;
  end;

FUNCTION T_codeProvider.filename: ansistring;
  begin
    result:=ExtractFileName(filepath);
  end;

FUNCTION T_codeProvider.fileHasChanged: boolean;
  VAR currentFileAge:double;
  begin
    if (filepath<>'') and FileExists(filepath) then begin
      FileAge(filepath,currentFileAge);
      result:=currentFileAge<>syncedFileAge;
    end else result:=false;
  end;

FUNCTION T_codeProvider.getVersion(CONST reloadIfNecessary: boolean): longint;
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock:=ThreadID until lock=ThreadID;
    if reloadIfNecessary and fileHasChanged then load;
    result:=version;
    repeat lock:=0 until lock=0;
  end;

FUNCTION T_codeProvider.id: ansistring;
  VAR i:longint;
  begin
    result:=filename;    
    i:=1;
    while (i<=length(result)) and (result[i] in ['a'..'z','A'..'Z','0'..'9','_']) do inc(i);
    result:=copy(result,1,i-1);
  end;

PROCEDURE T_codeProvider.clear;
  begin
    filepath:='';
    SetLength(lineData,0);
    version:=0;
    syncedFileAge:=0;
  end;

INITIALIZATION
  clearSourceScanPaths;
end.
