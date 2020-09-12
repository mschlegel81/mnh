UNIT mnh_settings;
INTERFACE
USES sysutils,
     serializationUtil,mySys,
     fileWrappers,
     mnh_constants,
     mnh_messages;
CONST
  FONT_STYLE_BOLD  =1;
  FONT_STYLE_ITALIC=2;

  LINE_ENDING_UNCHANGED=0;
  LINE_ENDING_DEFAULT=1;
  LINE_ENDING_LINUX=2;
  LINE_ENDING_WINDOWS=3;

  LINE_ENDING:array[0..3] of string=('',LineEnding,#10,#13#10);

  MAX_WORKSPACE_HISTORY_SIZE=1024;

TYPE
T_controlType=(ctEditor,ctTable,ctGeneral,ctPlot,ctNoneOrUnknown);
T_cmdLineFlag=(clf_GUI         ,
               clf_QUIET       ,
               clf_SILENT      ,
               clf_HEADLESS    ,
               clf_PAUSE_ON_ERR,
               clf_PAUSE_ALWAYS,
               clf_PROFILE     ,
               clf_SHOW_HELP   ,
               clf_EXEC_CMD    ,
               clf_SHOW_INFO   ,
               clf_FORCE_STDOUT,
               clf_FORCE_STDERR);
T_cmdLineFlags=set of T_cmdLineFlag;
P_Settings=^T_settings;
T_settings=object(T_serializable)
  //Global:
  memoryLimit:int64;
  cpuCount:longint;

  fullFlavourLocation,
  lightFlavourLocation:string;

  newFileLineEnding,overwriteLineEnding:byte;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  PROCEDURE initDefaults;
  PROCEDURE fixLocations;
end;

FUNCTION settingsFileName: string;
FUNCTION defaultWorkspaceFilename:string;
FUNCTION ideSettingsFilename: string;
FUNCTION runParameterHistoryFileName:string;
PROCEDURE saveSettings;
VAR settings:T_settings;
IMPLEMENTATION

FUNCTION settingsFileName: string;
  begin
    result:=configDir+'mnh.settings';
  end;

FUNCTION ideSettingsFilename: string;
  begin
    result:=configDir+'mnh.ide.settings';
  end;

FUNCTION defaultWorkspaceFilename:string;
  begin
    result:=configDir+'mnh.workspace';
  end;

FUNCTION runParameterHistoryFileName:string;
  begin
    result:=configDir+'mnh.parameterhistory';
  end;

PROCEDURE saveSettings;
  begin
    ensurePath(settingsFileName);
    settings.saveToFile(settingsFileName);
  end;

CONSTRUCTOR T_settings.create;
  begin
    cpuCount:=1;
    fullFlavourLocation:='';
    lightFlavourLocation:='';
  end;

DESTRUCTOR T_settings.destroy;
  begin
    fullFlavourLocation:='';
    lightFlavourLocation:='';
  end;

FUNCTION T_settings.getSerialVersion: dword; begin result:=164423; end;
FUNCTION T_settings.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  {$MACRO ON}
  {$define cleanExit:=begin initDefaults; exit(false) end}
  begin
    if not inherited loadFromStream(stream) then cleanExit;

    cpuCount:=stream.readLongint;
    if cpuCount<=0 then cpuCount:=getNumberOfCPUs;
    memoryLimit:=stream.readInt64;
    fullFlavourLocation:=stream.readAnsiString;
    lightFlavourLocation:=stream.readAnsiString;
    newFileLineEnding  :=stream.readByte([LINE_ENDING_DEFAULT,LINE_ENDING_LINUX,LINE_ENDING_WINDOWS]);
    overwriteLineEnding:=stream.readByte([LINE_ENDING_DEFAULT,LINE_ENDING_LINUX,LINE_ENDING_WINDOWS,LINE_ENDING_UNCHANGED]);
    if not(stream.allOkay) then cleanExit else result:=true;
  end;

PROCEDURE T_settings.saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  begin
    inherited saveToStream(stream);
    stream.writeLongint(cpuCount);
    stream.writeInt64(memoryLimit);
    stream.writeAnsiString(fullFlavourLocation);
    stream.writeAnsiString(lightFlavourLocation);
    stream.writeByte(newFileLineEnding);
    stream.writeByte(overwriteLineEnding);
  end;

PROCEDURE T_settings.initDefaults;
  begin
    cpuCount:=getNumberOfCPUs;
    memoryLimit:={$ifdef Windows}
                   {$ifdef CPU32}
                   1000000000;
                   {$else}
                   int64(maxLongint)*2;
                   {$endif}
                 {$else}
                 1000000000;
                 {$endif}
    fullFlavourLocation:={$ifdef fullVersion}paramStr(0){$else}''{$endif};
    lightFlavourLocation:={$ifdef fullVersion}''{$else}paramStr(0){$endif};
    newFileLineEnding:=LINE_ENDING_DEFAULT;
    overwriteLineEnding:=LINE_ENDING_UNCHANGED;
  end;

PROCEDURE T_settings.fixLocations;
  begin
    {$ifdef fullVersion}
    fullFlavourLocation:=paramStr(0);
    if lightFlavourLocation='' then begin
      lightFlavourLocation:=ExtractFileDir(paramStr(0))+DirectorySeparator+'mnh_light'{$ifdef Windows}+'.exe'{$endif};
      if not(fileExists(lightFlavourLocation)) then lightFlavourLocation:='';
    end;
    {$else}
    lightFlavourLocation:=paramStr(0);
    if fullFlavourLocation='' then begin
      fullFlavourLocation:=ExtractFileDir(paramStr(0))+DirectorySeparator+'mnh'{$ifdef Windows}+'.exe'{$endif};
      if not(fileExists(fullFlavourLocation)) then fullFlavourLocation:='';
    end;
    {$endif};
  end;

INITIALIZATION
  settings.create;
  if fileExists(settingsFileName)
  then settings.loadFromFile(settingsFileName)
  else settings.initDefaults;
  {$ifndef FULLVERSION}
  settings.fixLocations;
  {$endif}

FINALIZATION
  settings.destroy;

end.
