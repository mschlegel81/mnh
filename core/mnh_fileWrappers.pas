UNIT mnh_fileWrappers;
INTERFACE
USES SysUtils,Classes,process;
TYPE
  T_stringList = array of ansistring;

  P_codeProvider = ^T_codeProvider;

  { T_codeProvider }

  T_codeProvider = object
  private
    lock: TThreadID;
    filepath: ansistring;
    lineData: T_stringList;
    syncedFileAge: double;
    fileVersion: longint;
    version: longint;

  public
    CONSTRUCTOR create;
    CONSTRUCTOR create(CONST path: ansistring);
    DESTRUCTOR destroy;
    FUNCTION getLines: T_stringList;
    PROCEDURE setLines(CONST value: T_stringList);
    PROCEDURE setLines(CONST value: TStrings);
    PROCEDURE setLines(CONST value: ansistring);
    PROCEDURE appendLine(CONST value: ansistring);

    PROCEDURE setPath(CONST path: ansistring);
    FUNCTION getPath: ansistring;
    PROCEDURE load;
    PROCEDURE save;
    FUNCTION filename: ansistring;
    FUNCTION fileHasChanged: boolean;
    FUNCTION fileIsOutOfSync: boolean;
    FUNCTION getVersion(CONST reloadIfNecessary: boolean): longint;
    FUNCTION id: ansistring;
    PROCEDURE Clear;
  end;

  //T_fileCursor=object
  //  filename:ansistring;
  //  handle:TextFile;
  //  isOpen:boolean;
  //
  //  CONSTRUCTOR create(CONST fname:string);
  //  FUNCTION hasNext:boolean;
  //  FUNCTION next:ansistring;
  //  PROCEDURE close;
  //  PROCEDURE resetCursor;
  //  DESTRUCTOR destroy;
  //end;

CONST sourceExt = '.MNH';

FUNCTION fileContent(CONST Name: ansistring; OUT accessed: boolean): ansistring;
FUNCTION fileLines(CONST Name: ansistring; OUT accessed: boolean): T_stringList;
FUNCTION fileLines(CONST Name: ansistring; CONST firstIdx,lastIdx:longint; OUT accessed: boolean): T_stringList;
FUNCTION writeFile(CONST Name, textToWrite: ansistring): boolean;
FUNCTION writeFileLines(CONST Name: ansistring; CONST textToWrite: T_stringList): boolean;
FUNCTION find(CONST pattern: ansistring; CONST filesAndNotFolders: boolean): T_stringList;

PROCEDURE setMainPackagePath(CONST path: ansistring);
FUNCTION locateSource(CONST rootPath, id: ansistring): ansistring;
FUNCTION locateSources: T_stringList;

FUNCTION runCommandAsync(CONST executable: ansistring; CONST parameters: T_stringList): boolean;
FUNCTION runCommand(CONST executable: ansistring; CONST parameters: T_stringList; OUT output: TStringList): boolean;

IMPLEMENTATION
VAR mainPackagePath: ansistring;


PROCEDURE setMainPackagePath(CONST path: ansistring);
  begin
    mainPackagePath := expandFileName(extractFilePath(path));
  end;

FUNCTION locateSource(CONST rootPath, id: ansistring): ansistring;
  FUNCTION nameToId(CONST fname: ansistring): ansistring;
    begin
      if uppercase(extractFileExt(fname)) = sourceExt then
        begin
        result := extractFileName(fname);
        result := copy(result, 1, length(result)-length(sourceExt));
        end
      else
        result := '';
      if UpperCase(result) = 'MNH' then
        result := '';
    end;

  PROCEDURE recursePath(CONST path: ansistring);
    VAR
      info: TSearchRec;
    begin
      if findFirst(path+'*', faAnyFile, info) = 0 then
        repeat
          if (info.attr and faDirectory) = faDirectory then
            begin
            if (info.Name<>'.') and (info.Name<>'..') then
              recursePath(path+info.Name+DirectorySeparator);
            end
          else if nameToId(info.Name) = id then
            result := path+info.Name;
        until (findNext(info)<>0) or (result<>'');
      SysUtils.findClose(info);
    end;

  begin
    result := '';
    recursePath(expandFileName(extractFilePath(rootPath)));
    if result = '' then recursePath(expandFileName(''));
  end;

FUNCTION locateSources: T_stringList;
  PROCEDURE recursePath(CONST path: ansistring);
    VAR
      info: TSearchRec;
    begin
      if findFirst(path+'*', faAnyFile, info) = 0 then
        repeat
          if (info.attr and faDirectory) = faDirectory then
            begin
            if (info.Name<>'.') and (info.Name<>'..') then
              recursePath(path+info.Name+DirectorySeparator);
            end
          else if uppercase(extractFileExt(info.Name)) = sourceExt then
            begin
            setLength(result, length(result)+1);
            result[length(result)-1] := path+info.Name;
            end;
        until (findNext(info)<>0);
      SysUtils.findClose(info);
    end;

  begin
    setLength(result, 0);  recursePath('');
    if length(result) = 0 then recursePath('..'+DirectorySeparator);
    if length(result) = 0 then
      recursePath('..'+DirectorySeparator+'..'+DirectorySeparator);
  end;


FUNCTION fileContent(CONST Name: ansistring; OUT accessed: boolean): ansistring;
  VAR
    handle: FILE of char;
    block: array[0..1023] of char;
    actuallyRead, i: longint;
  begin
    if trim(Name) = '' then
      begin
      accessed := false;
      exit;
      end;
      try
      accessed := true;
      assign(handle, Name);
      reset(handle);
      result := '';
      repeat
        blockread(handle, block, length(block), actuallyRead);
        for i := 0 to actuallyRead-1 do
          result := result+block [i];
      until actuallyRead<length(block);
      close(handle);
      except
      accessed := false;
      result := '';
      end;
  end;

FUNCTION fileLines(CONST Name: ansistring; OUT accessed: boolean): T_stringList;
  begin
    result:=fileLines(name,0,maxLongint,accessed);
  end;

FUNCTION fileLines(CONST Name: ansistring; CONST firstIdx,lastIdx:longint; OUT accessed: boolean): T_stringList;
  VAR handle: textFile;
      lineIndex:longint=0;
      tempLine:ansistring;
  begin
    setLength(result, 0);
    if trim(Name) = '' then begin
      accessed := false;
      exit;
    end;
    try
      accessed := true;
      assign(handle, Name);
      reset(handle);
      while not (EOF(handle)) and (lineIndex<=lastIdx) do begin
        readln(handle,tempLine);
        if (lineIndex>=firstIdx) and (lineIndex<=lastIdx) then begin
          setLength(result, length(result)+1);
          result [length(result)-1]:=tempLine;
        end;
        inc(lineIndex);
      end;
      close(handle);
    except
      accessed := false;
      setLength(result, 0);
    end;
  end;

FUNCTION writeFile(CONST Name, textToWrite: ansistring): boolean;
  VAR
    handle: FILE of char;
    block: array[0..1023] of char;
    i, j: longint;
  begin
    if trim(Name) = '' then
      exit(false);
      try
      result := true;
      assign(handle, Name);
      rewrite(handle);
      i := 1;
      while i<=length(textToWrite) do
        begin
        j := 0;
        while (i<=length(textToWrite)) and (j<length(block)) do
          begin
          block[j] := textToWrite [i];
          Inc(i);
          Inc(j);
          end;
        blockwrite(handle, block, j);
        end;
      close(handle);
      except
      result := false;
      end;
  end;

FUNCTION writeFileLines(CONST Name: ansistring; CONST textToWrite: T_stringList): boolean;
  VAR
    handle: TextFile;
    i: longint;
  begin
    if trim(Name) = '' then
      exit(false);
      try
      assign(handle, Name);
      rewrite(handle);
      for i := 0 to length(textToWrite)-1 do
        writeln(handle, textToWrite [i]);
      close(handle);
      result := true;
      except
      result := false;
      end;
  end;

FUNCTION find(CONST pattern: ansistring; CONST filesAndNotFolders: boolean): T_stringList;
  VAR info: TSearchRec;
    path: ansistring;
  begin
    path := ExtractFilePath(pattern);
    setLength(result, 0);
    if findFirst(pattern, faAnyFile, info) = 0 then
      repeat
        if (info.Name<>'.') and (info.Name<>'..') and
          (((info.attr and faDirectory) = faDirectory) and not
          (filesAndNotFolders) or  ((info.attr and faDirectory)<>faDirectory) and
          filesAndNotFolders) then
          begin
          setLength(result, length(result)+1);
          result[length(result)-1] := path+info.Name;
          end;
      until (findNext(info)<>0);
    SysUtils.findClose(info);
  end;

FUNCTION runCommandAsync(CONST executable: ansistring; CONST parameters: T_stringList): boolean;
  VAR tempProcess: TProcess;
       i: longint;
  begin
    result := true;
    try
      tempProcess := TProcess.create(nil);
      tempProcess.Executable := executable;
      for i := 0 to length(parameters)-1 do tempProcess.Parameters.Add(parameters [i]);
      tempProcess.Execute;
      tempProcess.Free;
    except
      result := false;
    end;
  end;

FUNCTION runCommand(CONST executable: ansistring; CONST parameters: T_stringList; OUT output: TStringList): boolean;
  CONST
    READ_BYTES = 2048;
  VAR
    memStream: TMemoryStream;
    tempProcess: TProcess;
    n: longint;
    BytesRead: longint;
  begin
    memStream := TMemoryStream.create;
    BytesRead := 0;
    tempProcess := TProcess.create(nil);
    tempProcess.Executable := executable;
    for n := 0 to length(parameters)-1 do
      tempProcess.Parameters.Add(parameters [n]);
    tempProcess.Options := [poUsePipes, poStderrToOutPut];
    tempProcess.ShowWindow := swoHIDE;
      try
      tempProcess.Execute;
      while tempProcess.Running do
        begin
        memStream.SetSize(BytesRead+READ_BYTES);
        n := tempProcess.Output.Read((memStream.Memory+BytesRead)^, READ_BYTES);
        if n>0 then
          Inc(BytesRead, n)
        else
          Sleep(10);
        end;
      repeat
        memStream.SetSize(BytesRead+READ_BYTES);
        n := tempProcess.Output.Read((memStream.Memory+BytesRead)^, READ_BYTES);
        if n>0 then
          Inc(BytesRead, n);
      until n<=0;
      result := (tempProcess.ExitStatus = 0);
      except
      result := false;
      end;
    tempProcess.Free;
    memStream.SetSize(BytesRead);
    output := TStringList.create;
    output.LoadFromStream(memStream);
    memStream.Free;
  end;

{ T_codeProvider }

FUNCTION T_codeProvider.getLines: T_stringList;
  VAR i: longint;
  begin
    while (lock<>0) and (lock<>ThreadID) do
      sleep(1);
    repeat
      lock := ThreadID
    until lock = ThreadID;
    setLength(result, length(lineData));
    for i := 0 to length(lineData)-1 do result[i] := lineData [i];
    repeat
      lock := 0
    until lock = 0;
  end;

PROCEDURE T_codeProvider.setLines(CONST value: T_stringList);
  VAR
    i: longint;
  begin
    while (lock<>0) and (lock<>ThreadID) do
      sleep(1);
    repeat
      lock := ThreadID
    until lock = ThreadID;
    setLength(lineData, length(value));
    for i := 0 to length(value)-1 do
      lineData[i] := value [i];
    Inc(version);
    repeat
      lock := 0
    until lock = 0;
  end;

PROCEDURE T_codeProvider.setLines(CONST value: TStrings);
  VAR i: longint;
    changed: boolean = false;
    cleanCount: longint;
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock := ThreadID until lock = ThreadID;
    cleanCount := value.Count;
    while (cleanCount>0) and (trim(value [cleanCount-1]) = '') do Dec(cleanCount);
    if length(lineData)<>cleanCount then
      begin
      setLength(lineData, cleanCount);
      changed := true;
      end;
    for i := 0 to cleanCount-1 do
      begin
      changed := changed or (trim(lineData [i])<>trim(value [i]));
      lineData[i] := value [i];

      end;
    if changed then Inc(version);
    repeat lock := 0 until lock = 0;
  end;

PROCEDURE T_codeProvider.setLines(CONST value: ansistring);
  begin
    while (lock<>0) and (lock<>ThreadID) do
      sleep(1);
    repeat
      lock := ThreadID
    until lock = ThreadID;
    setLength(lineData, 1);
    lineData[0] := value;
    Inc(version);
    repeat
      lock := 0
    until lock = 0;
  end;

PROCEDURE T_codeProvider.appendLine(CONST value: ansistring);
  begin
    while (lock<>0) and (lock<>ThreadID) do
      sleep(1);
    repeat
      lock := ThreadID
    until lock = ThreadID;
    setLength(lineData, length(lineData)+1);
    lineData[length(lineData)-1] := value;
    Inc(version);
    repeat
      lock := 0
    until lock = 0;
  end;

CONSTRUCTOR T_codeProvider.create;
  begin
    lock := 0;
    Clear;
  end;

CONSTRUCTOR T_codeProvider.create(CONST path: ansistring);
  begin
    lock := 0;
    Clear;
    filepath := path;
    if FileExists(path) then
      load;
  end;

DESTRUCTOR T_codeProvider.destroy;
  begin
    setLength(lineData, 0);
  end;

PROCEDURE T_codeProvider.setPath(CONST path: ansistring);
  begin
    filepath := path;
  end;

FUNCTION T_codeProvider.getPath: ansistring;
  begin
    result := filepath;
  end;

PROCEDURE T_codeProvider.load;
  VAR
    accessed: boolean;
    L: T_stringList;
    i: longint;
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock := ThreadID until lock = ThreadID;
    L := fileLines(filepath, accessed);
    if accessed then
      begin
      setLength(lineData, length(L));
      for i := 0 to length(L)-1 do lineData[i] := L [i];
      FileAge(filepath, syncedFileAge);
      Inc(version);
      fileVersion := version;
      i := length(lineData);
      while (i>0) and (trim(lineData [i-1]) = '') do
        begin
        setLength(lineData, i-1);
        Dec(i);
        end;
      end;
    repeat lock := 0 until lock = 0;
  end;

PROCEDURE T_codeProvider.save;
  begin
    while (lock<>0) and (lock<>ThreadID) do sleep(1);
    repeat lock := ThreadID until lock = ThreadID;
    if (filepath<>'') and writeFileLines(filepath, lineData) then
      begin
      FileAge(filepath, syncedFileAge);
      fileVersion := version;
      end;
    repeat lock := 0 until lock = 0;
  end;

FUNCTION T_codeProvider.filename: ansistring;
  begin
    result := ExtractFileName(filepath);
  end;

FUNCTION T_codeProvider.fileHasChanged: boolean;
  VAR currentFileAge: double;
  begin
    if (filepath<>'') and FileExists(filepath) then begin
      FileAge(filepath, currentFileAge);
      result := currentFileAge<>syncedFileAge;
    end else result := false;
  end;

FUNCTION T_codeProvider.fileIsOutOfSync: boolean;
  begin
    result := fileHasChanged or (filepath<>'') and (version<>fileVersion);
  end;

FUNCTION T_codeProvider.getVersion(CONST reloadIfNecessary: boolean): longint;
  begin
    while (lock<>0) and (lock<>ThreadID) do
      sleep(1);
    repeat lock := ThreadID until lock = ThreadID;
    if reloadIfNecessary and fileHasChanged then
      load;
    result := version;
    repeat
      lock := 0
    until lock = 0;
  end;

FUNCTION T_codeProvider.id: ansistring;
  VAR
    i: longint;
  begin
    result := filename;
    i := 1;
    while (i<=length(result)) and (result [i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
      Inc(i);
    result := copy(result, 1, i-1);
  end;

PROCEDURE T_codeProvider.Clear;
  begin
    filepath := '';
    setLength(lineData, 0);
    version := 0;
    fileVersion := 0;
    syncedFileAge := 0;
  end;

//CONSTRUCTOR T_fileCursor.create(CONST fname:ansistring);
//  begin
//    filename:=fname;
//    try
//      assign(handle,filename);
//      reset(handle);
//      isOpen:=true;
//    except
//      isOpen:=false;
//    end;
//  end;
//
//FUNCTION T_fileCursor.hasNext:boolean;
//  begin
//    if isOpen and eof(handle) then begin
//      result:=false;
//      close;
//    end else result:=isOpen;
//  end;
//
//FUNCTION T_fileCursor.next:ansistring;
//  begin
//    if not(hasNext) then exit('');
//    readln(handle,result);
//  end;
//
//PROCEDURE T_fileCursor.close;
//  begin
//    if isOpen then begin
//      system.close(handle);
//      isOpen:=false;
//    end;
//  end;
//
//PROCEDURE T_fileCursor.resetCursor;
//  begin
//    close;
//    try
//      reset(handle);
//      isOpen:=true;
//    except
//      isOpen:=false;
//    end;
//  end;
//
//DESTRUCTOR T_fileCursor.destroy;
//  begin close; end;

INITIALIZATION
  setMainPackagePath('');

end.
