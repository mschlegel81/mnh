UNIT mySys;
INTERFACE
USES myGenerics,sysutils,process;

FUNCTION getEnvironment:T_arrayOfString;
FUNCTION findDeeply(CONST rootPath,searchPattern:ansistring):ansistring;
PROCEDURE clearConsole;

VAR CMD_PATH,
    SEVEN_ZIP_PATH,
    NOTEPAD_PATH:specialize G_lazyVar<ansistring>;

IMPLEMENTATION

FUNCTION getEnvironment:T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,0);
    For i:=1 to GetEnvironmentVariableCount do append(result,GetEnvironmentString(i));
  end;

FUNCTION findDeeply(CONST rootPath,searchPattern:ansistring):ansistring;
  PROCEDURE recursePath(CONST path: ansistring);
    VAR info: TSearchRec;
    FUNCTION deeper:ansistring;
      begin
        if (pos('?',path)>=0) or (pos('*',path)>=0)
        then result:=ExtractFileDir(path)+DirectorySeparator+info.Name+DirectorySeparator
        else result:=path+info.Name+DirectorySeparator;
      end;

    begin
      if (result='') and (findFirst(path+searchPattern, faAnyFile, info) = 0) then result:=path+info.name;
      SysUtils.findClose(info);

      if findFirst(path+'*', faDirectory, info) = 0 then repeat
        if ((info.attr and faDirectory) = faDirectory) and
           (info.Name<>'.') and
           (info.Name<>'..')
        then recursePath(deeper);
      until (findNext(info)<>0) or (result<>'');
      SysUtils.findClose(info);
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
      tempProcess.Options:=tempProcess.Options+[poWaitOnExit];
      tempProcess.Executable := CMD_PATH.value;
      tempProcess.Parameters.Add('/C');
      tempProcess.Parameters.Add('cls');
      tempProcess.Execute;
      tempProcess.Free;
    except
    end;
  end;  
  
FUNCTION obtainCmd:ansistring; begin result:=findDeeply('C:\*Win*','cmd.exe'); end;
FUNCTION obtain7Zip:ansistring; begin result:=findDeeply('C:\*Program*','7z.exe'); end;
FUNCTION obtainNotepad:ansistring; begin result:=findDeeply('C:\*Program*','notepad++.exe'); end;

INITIALIZATION
  CMD_PATH.create(@obtainCmd);
  SEVEN_ZIP_PATH.create(@obtain7Zip);
  NOTEPAD_PATH.create(@obtainNotepad);;
  sleep(10);

FINALIZATION
  CMD_PATH.destroy;
  SEVEN_ZIP_PATH.destroy;
  NOTEPAD_PATH.destroy;

end.
