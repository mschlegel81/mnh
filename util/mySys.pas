UNIT mySys;
INTERFACE
USES myGenerics,sysutils;

FUNCTION getEnvironment:T_arrayOfString;
 
VAR CMD_PATH,SEVEN_ZIP_PATH,NOTEPAD_PATH:specialize G_safeVar<ansistring>;
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

  
FUNCTION fillProgramMap(p:pointer):ptrint;  
  begin
    CMD_PATH.lock;
    SEVEN_ZIP_PATH.lock;
    NOTEPAD_PATH.lock;
    CMD_PATH      .value:=findDeeply('C:\*Win*','cmd.exe');            CMD_PATH.unlock;
    SEVEN_ZIP_PATH.value:=findDeeply('C:\*Program*','7z.exe');        SEVEN_ZIP_PATH.unlock;
    NOTEPAD_PATH  .value:=findDeeply('C:\*Program*','notepad++.exe'); NOTEPAD_PATH.unlock;
  end;


INITIALIZATION
  CMD_PATH.create('');
  SEVEN_ZIP_PATH.create('');
  NOTEPAD_PATH.create('');
  beginThread(@fillProgramMap);
  sleep(10);

FINALIZATION
  CMD_PATH.destroy;
  SEVEN_ZIP_PATH.destroy;
  NOTEPAD_PATH.destroy;

end.
