UNIT mnh_settings;
INTERFACE
USES Classes,sysutils,dateutils,typinfo,
     myGenerics,serializationUtil,mySys,
     fileWrappers,
     mnh_constants,mnh_messages,
     out_adapters;
CONST
  FILE_HISTORY_MAX_SIZE=100;
  FONT_STYLE_BOLD  =1;
  FONT_STYLE_ITALIC=2;

TYPE
T_controlType=(ctEditor,ctTable,ctGeneral,ctPlot,ctNoneOrUnknown);

P_fileHistory=^T_fileHistory;
T_fileHistory=object(T_serializable)
  items: T_arrayOfString;
  isFolderHistory:boolean;
  CONSTRUCTOR create(CONST forFolders:boolean);
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  FUNCTION polishHistory: boolean;
  PROCEDURE fileClosed(CONST fileName:ansistring);
  FUNCTION historyItem(CONST index:longint):ansistring;
  FUNCTION findFiles(CONST rootPath:string):T_arrayOfString;
end;

T_cmdLineFlag=(clf_GUI,clf_QUIET,clf_SILENT,clf_HEADLESS,clf_PROFILE,clf_PAUSEALWAYS);

P_Settings=^T_settings;
T_settings=object(T_serializable)
  //Global:
  memoryLimit:int64;
  cpuCount:longint;
  //IDE:
  Font:array[ctEditor..ctGeneral] of record
    fontName :string;
    style    :byte;
    fontSize :longint;
  end;
  doResetPlotOnEvaluation: boolean;
  cacheAnimationFrames: boolean;

  fullFlavourLocation,
  lightFlavourLocation:string;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  PROCEDURE initDefaults;
  PROCEDURE fixLocations;
end;

FUNCTION settingsFileName: string;
FUNCTION workspaceFilename:string;
PROCEDURE saveSettings;
VAR settings:T_settings;
IMPLEMENTATION
USES FileUtil;

FUNCTION settingsFileName: string;
  begin
    result:=configDir+'mnh.settings';
  end;

FUNCTION workspaceFilename:string;
  begin
    result:=configDir+'mnh.workspace';
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
  end;

FUNCTION T_settings.getSerialVersion: dword; begin result:=164423; end;
FUNCTION T_settings.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  {$MACRO ON}
  {$define cleanExit:=begin initDefaults; exit(false) end}
  VAR c:T_controlType;
  begin
    if not inherited loadFromStream(stream) then cleanExit;

    cpuCount:=stream.readLongint;
    if cpuCount<=0 then cpuCount:=getNumberOfCPUs;
    for c:=low(Font) to high(Font) do with Font[c] do begin
      fontSize:=stream.readLongint;
      style   :=stream.readByte([0..3]);
      fontName:=stream.readAnsiString;
    end;
    doResetPlotOnEvaluation := stream.readBoolean;
    cacheAnimationFrames    := stream.readBoolean;
    memoryLimit:=stream.readInt64;
    fullFlavourLocation:=stream.readAnsiString;
    lightFlavourLocation:=stream.readAnsiString;
    if not(stream.allOkay) then cleanExit else result:=true;
  end;

PROCEDURE T_settings.saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  VAR c:T_controlType;
  begin
    inherited saveToStream(stream);
    stream.writeLongint(cpuCount);
    for c:=low(Font) to high(Font) do with Font[c] do begin
      stream.writeLongint(fontSize);
      stream.writeByte(style);
      stream.writeAnsiString(fontName);
    end;
    stream.writeBoolean(doResetPlotOnEvaluation);
    stream.writeBoolean(cacheAnimationFrames);
    stream.writeInt64(memoryLimit);
    stream.writeAnsiString(fullFlavourLocation);
    stream.writeAnsiString(lightFlavourLocation);
  end;

PROCEDURE T_settings.initDefaults;
  VAR c:T_controlType;
  begin
    cpuCount:=getNumberOfCPUs;
    for c:=low(Font) to high(Font) do with Font[c] do begin
      fontName:='Courier New';
      style   :=0;
      fontSize:=10;
    end;
    doResetPlotOnEvaluation:=true;
    cacheAnimationFrames:=true;
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

CONSTRUCTOR T_fileHistory.create(CONST forFolders:boolean);
  begin
    items:=C_EMPTY_STRING_ARRAY;
    isFolderHistory:=forFolders;
  end;

DESTRUCTOR T_fileHistory.destroy;
  begin
    setLength(items,0);
  end;

FUNCTION T_fileHistory.getSerialVersion:dword; begin result:=176493; end;

FUNCTION T_fileHistory.loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
  VAR i,count:longint;
  begin
    {$ifdef debugMode}
    writeln('Loading T_fileHistory @',stream.streamPos);
    {$endif}
    if not(inherited loadFromStream(stream)) then exit(false);
    setLength(items,0);
    count:=stream.readNaturalNumber;
    if count>FILE_HISTORY_MAX_SIZE then exit(false);
    setLength(items,count);
    for i:=0 to count-1 do items[i]:=stream.readAnsiString;
    result:=stream.allOkay;
  end;

PROCEDURE T_fileHistory.saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  VAR i,count:longint;
  begin
    {$ifdef debugMode}
    writeln('Saving T_fileHistory @',stream.streamPos);
    {$endif}
    inherited saveToStream(stream);
    count:=length(items);
    if count>FILE_HISTORY_MAX_SIZE then count:=FILE_HISTORY_MAX_SIZE;
    stream.writeNaturalNumber(count);
    for i:=0 to count-1 do stream.writeAnsiString(items[i]);
  end;

FUNCTION T_fileHistory.polishHistory: boolean;
  VAR i, j: longint;
  begin
    result := false;
    for i:=0 to length(items)-1 do
    if not(isFolderHistory) and not(fileExists     (items[i])) or
           isFolderHistory  and not(DirectoryExists(items[i])) then begin
      items[i]:='';
      result:=true;
    end;
    for i:=1 to length(items)-1 do
    if (items[i]<>'') then for j:=0 to i-1 do
    if (expandFileName(items[i])=expandFileName(items[j])) then begin
      items[i]:='';
      result:=true;
    end;
    dropValues(items,'');
  end;

PROCEDURE T_fileHistory.fileClosed(CONST fileName: ansistring);
  begin
    prepend(items,fileName);
    polishHistory;
  end;

FUNCTION T_fileHistory.historyItem(CONST index: longint): ansistring;
  begin
    if (index>=0) and (index<length(items))
    then result:=items[index]
    else result:='';
  end;

FUNCTION T_fileHistory.findFiles(CONST rootPath:string):T_arrayOfString;
  VAR allPathsToScan:T_arrayOfString;
      fileName:string;
      pathToScan:string;
      list:TStringList;
  begin
    allPathsToScan:=items;
    result:=listScriptFileNames(rootPath);
    for pathToScan in allPathsToScan do begin
      list:=FindAllFiles(pathToScan+DirectorySeparator,'*.mnh',false);
      for fileName in list do append(result,fileName);
      list.free;
    end;
    sortUnique(result);
  end;

INITIALIZATION
  settings.create;
  if fileExists(settingsFileName)
  then settings.loadFromFile(settingsFileName)
  else settings.initDefaults;

FINALIZATION
  settings.destroy;

end.
