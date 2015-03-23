UNIT mySys;
INTERFACE
USES myGenerics,sysutils;
CONST programsToLocate:T_arrayOfString=['cmd.exe','7z.exe','notepad++.exe'];

FUNCTION getEnvironment:T_arrayOfString;
VAR programMap: specialize G_stringKeyMap<ansistring>;
IMPLEMENTATION
FUNCTION getEnvironment:T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,0);
    For i:=1 to GetEnvironmentVariableCount do append(result,GetEnvironmentString(i));
  end;

FUNCTION findDeeply(CONST searchPaths,searchPatterns:T_arrayOfString):T_arrayOfString;
  PROCEDURE recursePath(CONST path: ansistring);
    VAR info: TSearchRec;
        patternIdx:longint;
    begin
      if findFirst(path+'*', faDirectory, info) = 0 then repeat
        if ((info.attr and faDirectory) = faDirectory) and 
           (info.Name<>'.') and 
           (info.Name<>'..') 
        then recursePath(path+info.Name+DirectorySeparator);
      until (findNext(info)<>0);
      SysUtils.findClose(info);
      for patternIdx:=0 to length(searchPatterns)-1 do begin
        if findFirst(path+searchPatterns[patternIdx], faAnyFile, info) = 0 then repeat
          append(result,path+info.name);
        until (findNext(info)<>0);
        SysUtils.findClose(info);
      end;
    end;
  
  VAR pathIdx:longint;
  begin
    setLength(result,0);
    for pathIdx:=0 to length(searchPaths)-1 do recursePath(searchPaths[pathIdx]);
  end;

  
FUNCTION fillProgramMap(p:pointer):ptrint;
  begin
  
  end;


begin
  

end.