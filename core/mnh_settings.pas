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
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  PROCEDURE initDefaults;
  PROCEDURE fixLocations;
end;

{$ifdef fullVersion}
T_outlineSettings=object(T_serializable)
  showPrivate,
  showImported:boolean;
  ruleSorting:T_ruleSorting;
  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
end;

{ T_ideSettings }

T_ideSettings=object(T_serializable)
  private
    currentWorkspace:string;
    workspaceHistory:array of string;
  public

  //IDE:
  Font:array[ctEditor..ctGeneral] of record
    fontName :string;
    style    :byte;
    fontSize :longint;
  end;
  doResetPlotOnEvaluation: boolean;
  cacheAnimationFrames: boolean;

  outputBehavior,
  quickOutputBehavior: T_ideMessageConfig;
  outputLinesLimit:longint;

  outlineSettings:T_outlineSettings;

  CONSTRUCTOR create;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  PROCEDURE initDefaults;
  FUNCTION workspaceFilename: string;
end;
{$endif}

FUNCTION settingsFileName: string;
FUNCTION defaultWorkspaceFilename:string;
FUNCTION ideSettingsFilename: string;
FUNCTION runParameterHistoryFileName:string;
PROCEDURE saveSettings;
VAR settings:T_settings;
    {$ifdef fullVersion}
    ideSettings:T_ideSettings;
    {$endif}
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

{$ifdef fullVersion}
CONSTRUCTOR T_outlineSettings.create;
  begin
    showPrivate:=true;
    showImported:=false;
    ruleSorting:=rs_byLocation;
  end;

DESTRUCTOR T_outlineSettings.destroy;
  begin
  end;

FUNCTION T_outlineSettings.getSerialVersion: dword;
  begin
    result:=1;
  end;

FUNCTION T_outlineSettings.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    {$ifdef debugMode}
    writeln('Loading T_outlineSettings @',stream.streamPos);
    {$endif}
    try
      showPrivate:=stream.readBoolean;
      showImported:=stream.readBoolean;
      ruleSorting:=T_ruleSorting(stream.readByte);
      result:=stream.allOkay;
    except
      result:=false;
    end;
    if not(result) then begin
      showPrivate:=true;
      showImported:=false;
      ruleSorting:=rs_byLocation;
    end;
  end;

PROCEDURE T_outlineSettings.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    {$ifdef debugMode}
    writeln('Saving T_outlineSettings @',stream.streamPos);
    {$endif}
    stream.writeBoolean(showPrivate);
    stream.writeBoolean(showImported);
    stream.writeByte(byte(ruleSorting));
  end;

CONSTRUCTOR T_ideSettings.create;
  begin
    initDefaults;
    outputBehavior.create;
    quickOutputBehavior.create;
    outlineSettings.create;
    initialize(workspaceHistory);
  end;

FUNCTION T_ideSettings.getSerialVersion: dword;
  begin
    result:=24823582;
  end;

FUNCTION T_ideSettings.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  {$MACRO ON}
  {$define cleanExit:=begin initDefaults; exit(false) end}
  VAR c:T_controlType;
      i:longint;
  begin
    if not(inherited loadFromStream(stream)) then cleanExit;
    for c:=low(Font) to high(Font) do with Font[c] do begin
      fontSize:=stream.readLongint;
      style   :=stream.readByte([0..3]);
      fontName:=stream.readAnsiString;
    end;
    doResetPlotOnEvaluation := stream.readBoolean;
    cacheAnimationFrames    := stream.readBoolean;

    outputBehavior     .loadFromStream(stream);
    quickOutputBehavior.loadFromStream(stream);
    outputLinesLimit:=stream.readLongint;
    if outputLinesLimit<0 then stream.logWrongTypeError;
    outlineSettings.loadFromStream(stream);
    currentWorkspace:=stream.readAnsiString;
    i:=stream.readNaturalNumber;
    if (i>MAX_WORKSPACE_HISTORY_SIZE) then stream.logWrongTypeError else begin
      setLength(workspaceHistory,i);
      for i:=0 to length(workspaceHistory)-1 do workspaceHistory[i]:=stream.readAnsiString;
    end;
    if not(stream.allOkay) then cleanExit else result:=true;
  end;

PROCEDURE T_ideSettings.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR c:T_controlType;
      i:longint;
  begin
    inherited saveToStream(stream);
    for c:=low(Font) to high(Font) do with Font[c] do begin
      stream.writeLongint(fontSize);
      stream.writeByte(style);
      stream.writeAnsiString(fontName);
    end;
    stream.writeBoolean(doResetPlotOnEvaluation);
    stream.writeBoolean(cacheAnimationFrames);
    outputBehavior.saveToStream(stream);
    quickOutputBehavior.saveToStream(stream);
    stream.writeLongint(outputLinesLimit);
    outlineSettings.saveToStream(stream);
    stream.writeAnsiString(currentWorkspace);
    if length(workspaceHistory)>MAX_WORKSPACE_HISTORY_SIZE then setLength(workspaceHistory,MAX_WORKSPACE_HISTORY_SIZE);
    stream.writeNaturalNumber(length(workspaceHistory));
    for i:=0 to length(workspaceHistory)-1 do stream.writeAnsiString(workspaceHistory[i]);
  end;

PROCEDURE T_ideSettings.initDefaults;
  VAR c:T_controlType;
  begin
    for c:=low(Font) to high(Font) do with Font[c] do begin
      fontName:='Courier New';
      style   :=0;
      fontSize:=10;
    end;
    doResetPlotOnEvaluation:=true;
    cacheAnimationFrames:=true;
    outputBehavior.reset;
    quickOutputBehavior.reset;
    outputLinesLimit:=maxLongint;
    currentWorkspace:='';
    setLength(workspaceHistory,0);
  end;

FUNCTION T_ideSettings.workspaceFilename: string;
  begin
    if currentWorkspace=''
    then result:=defaultWorkspaceFilename
    else result:=currentWorkspace;
  end;

{$endif}

CONSTRUCTOR T_settings.create;
  begin
    cpuCount:=1;
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
    fullFlavourLocation :=paramStr(0);
    if lightFlavourLocation='' then begin
      lightFlavourLocation:=ExtractFileDir(paramStr(0))+DirectorySeparator+'mnh_light'{$ifdef Windows}+'.exe'{$endif};
      if not(fileExists(lightFlavourLocation)) then lightFlavourLocation:='';
    end;
    {$else} lightFlavourLocation:=paramStr(0);{$endif};
  end;

INITIALIZATION
  settings.create;
  if fileExists(settingsFileName)
  then settings.loadFromFile(settingsFileName)
  else settings.initDefaults;
  {$ifdef fullVersion}
  ideSettings.create;
  {$endif}

end.
