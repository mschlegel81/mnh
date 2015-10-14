UNIT mySys;
INTERFACE
USES dos,myGenerics,sysutils,process,windows,FileUtil;

FUNCTION getEnvironment:T_arrayOfString;
FUNCTION findDeeply(CONST rootPath,searchPattern:ansistring):ansistring;
PROCEDURE clearConsole;
PROCEDURE getFileInfo(CONST filePath:string; OUT time:double; OUT size:int64; OUT isExistent, isArchive, isDirectory, isReadOnly, isSystem, isHidden:boolean);
FUNCTION getNumberOfCPUs:longint;

VAR CMD_PATH,
    SEVEN_ZIP_PATH,
    NOTEPAD_PATH:specialize G_lazyVar<ansistring>;

IMPLEMENTATION

FUNCTION getNumberOfCPUs:longint;
VAR SystemInfo:SYSTEM_INFO;
begin
  getSystemInfo(SystemInfo);
  result:=SystemInfo.dwNumberOfProcessors;
end;

FUNCTION getEnvironment:T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,0);
    for i:=1 to GetEnvironmentVariableCount do append(result,GetEnvironmentString(i));
  end;

FUNCTION findDeeply(CONST rootPath,searchPattern:ansistring):ansistring;
  PROCEDURE recursePath(CONST path: ansistring);
    VAR info: TSearchRec;
    FUNCTION deeper:ansistring;
      begin
        if (pos('?',path)>=0) or (pos('*',path)>=0)
        then result:=ExtractFileDir(path)+DirectorySeparator+info.name+DirectorySeparator
        else result:=path+info.name+DirectorySeparator;
      end;

    begin
      if (result='') and (findFirst(path+searchPattern, faAnyFile, info) = 0) then result:=path+info.name;
      sysutils.findClose(info);

      if findFirst(path+'*', faDirectory, info) = 0 then repeat
        if ((info.Attr and faDirectory) = faDirectory) and
           (info.name<>'.') and
           (info.name<>'..')
        then recursePath(deeper);
      until (findNext(info)<>0) or (result<>'');
      sysutils.findClose(info);
    end;

  begin
    result:='';
    recursePath(rootPath);
  end;

PROCEDURE clearConsole;
  VAR tempProcess: TProcess;
  begin
    try
      tempProcess := TProcess.create(nil);
      tempProcess.options:=tempProcess.options+[poWaitOnExit];
      tempProcess.executable := CMD_PATH.value;
      tempProcess.parameters.add('/C');
      tempProcess.parameters.add('cls');
      tempProcess.execute;
      tempProcess.free;
    except
    end;
  end;

FUNCTION obtainCmd:ansistring; begin result:=findDeeply('C:\*Win*','cmd.exe'); end;
FUNCTION obtain7Zip:ansistring; begin result:=findDeeply('C:\*Program*','7z.exe'); end;
FUNCTION obtainNotepad:ansistring; begin result:=findDeeply('C:\*Program*','notepad++.exe'); end;

PROCEDURE getFileInfo(CONST filePath:string;
  OUT time:double;
  OUT size:int64;
  OUT isExistent,
      isArchive,
      isDirectory,
      isReadOnly,
      isSystem,
      isHidden:boolean);
  VAR f:file of byte;
      Attr:word;
      ft:longint;
  begin
    time:=-1;
    size:=-1;
    isExistent :=false;
    isArchive  :=false;
    isDirectory:=false;
    isReadOnly :=false;
    isSystem   :=false;
    isHidden   :=false;
    if DirectoryExists(filePath) or fileExists(filePath) then begin
      isExistent:=true;
      ft:=fileAge(filePath);
      if ft<>-1 then time:=FileDateToDateTime(ft);

      assign (f,filePath);
      GetFAttr(f,Attr);
      isArchive  :=(Attr and archive  )<>0;
      isDirectory:=(Attr and directory)<>0;
      isReadOnly :=(Attr and readonly )<>0;
      isSystem   :=(Attr and sysfile  )<>0;
      isHidden   :=(Attr and hidden   )<>0;
      if fileExists(filePath) then try
        size:=FileSize(filePath);
      except
        size:=-2;
      end;
    end;
  end;

INITIALIZATION
  CMD_PATH.create(@obtainCmd);
  SEVEN_ZIP_PATH.create(@obtain7Zip);
  NOTEPAD_PATH.create(@obtainNotepad);

FINALIZATION
  CMD_PATH.destroy;
  SEVEN_ZIP_PATH.destroy;
  NOTEPAD_PATH.destroy;

end.
