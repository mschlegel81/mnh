UNIT mnh_settings;
INTERFACE
USES Classes,sysutils,dateutils,typinfo,
     FileUtil,
     myGenerics,serializationUtil,mySys,
     fileWrappers,
     mnh_constants,mnh_messages,
     out_adapters;
CONST
  C_SAVE_INTERVAL:array[0..6] of record text:string; interval:double; end=
  ((text:'off';        interval:1E6),
   (text:'1 minute';   interval: 1/(24*60)),
   (text:'2 minutes';  interval: 2/(24*60)),
   (text:'5 minutes';  interval: 5/(24*60)),
   (text:'10 minutes'; interval:10/(24*60)),
   (text:'30 minutes'; interval:30/(24*60)),
   (text:'1 hour';     interval: 1/24));
  FILE_HISTORY_MAX_SIZE=100;

  LINE_ENDING_UNCHANGED=0;
  LINE_ENDING_DEFAULT=1;
  LINE_ENDING_LINUX=2;
  LINE_ENDING_WINDOWS=3;

  LINE_ENDING:array[0..3] of string=('',LineEnding,#10,#13#10);
TYPE
T_formPosition=object(T_serializable)
  top, Left, width, height: longint;
  isFullscreen: boolean;
  relativeSplitterPosition:double;
  CONSTRUCTOR create;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
end;

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
  private
    //Nonpersistent:
    wasLoaded:boolean;
    savedAt:double;
  public
  //Global:
  memoryLimit:int64;
  cpuCount:longint;
  saveIntervalIdx:byte;
  //IDE:
  editor:record
    fontName        :string;
    fontSize        :longint;
    antialiasedFonts:boolean;
  end;
  mainForm:T_formPosition;
  outline:record
    showPrivate,showImported,sortByName,sortByNameCaseSen,sortByLoc,
    fullHeight:boolean;
    viewWidth:longint;
  end;
  outputBehavior,
  quickOutputBehavior: T_ideMessageConfig;
  outputLinesLimit:longint;
  doResetPlotOnEvaluation: boolean;
  cacheAnimationFrames: boolean;

  htmlDocGeneratedForCodeHash:string;
  doShowSplashScreen:boolean;
  fullFlavourLocation,
  lightFlavourLocation:string;

  externalRunOptions:record
    flags:set of T_cmdLineFlag;
    callLightFlavour:boolean;
    verbosity,customFolder:ansistring;
  end;

  newFileLineEnding,overwriteLineEnding:byte;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  PROCEDURE initDefaults;
  PROCEDURE fixLocations;

  FUNCTION savingRequested:boolean;
  PROPERTY loaded:boolean read wasLoaded;
end;

FUNCTION settingsFileName: string;
PROCEDURE saveSettings;
VAR settings:T_settings;
IMPLEMENTATION

FUNCTION settingsFileName: string;
  begin
    result:=configDir+'mnh.settings';
  end;

PROCEDURE saveSettings;
  begin
    ensurePath(settingsFileName);
    settings.saveToFile(settingsFileName);
  end;

CONSTRUCTOR T_settings.create;
  begin
    cpuCount:=1;
    mainForm.create;
    wasLoaded:=false;
    fullFlavourLocation:='';
    lightFlavourLocation:='';
  end;

DESTRUCTOR T_settings.destroy;
  begin
  end;

FUNCTION T_settings.getSerialVersion: dword; begin result:=1644235082; end;
FUNCTION T_settings.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  {$MACRO ON}
  {$define cleanExit:=begin initDefaults; exit(false) end}
  begin
    if not inherited loadFromStream(stream) then cleanExit;

    cpuCount:=stream.readLongint;
    if cpuCount<=0 then cpuCount:=getNumberOfCPUs;
    with editor do begin
      fontSize:=stream.readLongint;
      fontName := stream.readAnsiString;
      antialiasedFonts:=stream.readBoolean;
    end;
    mainForm.loadFromStream(stream);
    with outline do begin
      showPrivate      :=stream.readBoolean;
      showImported     :=stream.readBoolean;
      sortByName       :=stream.readBoolean;
      sortByNameCaseSen:=stream.readBoolean;
      sortByLoc        :=stream.readBoolean;
      fullHeight       :=stream.readBoolean;
      viewWidth        :=stream.readLongint;
    end;
    stream.read(outputBehavior,sizeOf(outputBehavior));
    stream.read(quickOutputBehavior,sizeOf(quickOutputBehavior));
    doResetPlotOnEvaluation := stream.readBoolean;
    cacheAnimationFrames    := stream.readBoolean;
    saveIntervalIdx:=stream.readByte;
    memoryLimit:=stream.readInt64;
    outputLinesLimit:=stream.readLongint;
    htmlDocGeneratedForCodeHash:=stream.readAnsiString;
    doShowSplashScreen:=stream.readBoolean or (CODE_HASH<>htmlDocGeneratedForCodeHash);
    fullFlavourLocation:=stream.readAnsiString;
    lightFlavourLocation:=stream.readAnsiString;
    newFileLineEnding:=stream.readByte;
    overwriteLineEnding:=stream.readByte;
    with externalRunOptions do begin
      stream.read(flags,sizeOf(flags));
      callLightFlavour:=stream.readBoolean;
      verbosity   :=stream.readAnsiString;
      customFolder:=stream.readAnsiString;
    end;
    if not(stream.allOkay) then cleanExit else result:=true;
    savedAt:=now;
    wasLoaded:=result;
  end;

PROCEDURE T_settings.saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  begin
    inherited saveToStream(stream);
    stream.writeLongint(cpuCount);
    with editor do begin
      stream.writeLongint(fontSize);
      stream.writeAnsiString(fontName);
      stream.writeBoolean(antialiasedFonts);
    end;
    mainForm.saveToStream(stream);
    with outline do begin
      stream.writeBoolean(showPrivate      );
      stream.writeBoolean(showImported     );
      stream.writeBoolean(sortByName       );
      stream.writeBoolean(sortByNameCaseSen);
      stream.writeBoolean(sortByLoc        );
      stream.writeBoolean(fullHeight       );
      stream.writeLongint(viewWidth        );
    end;
    stream.write(outputBehavior,sizeOf(outputBehavior));
    stream.write(quickOutputBehavior,sizeOf(quickOutputBehavior));
    stream.writeBoolean(doResetPlotOnEvaluation);
    stream.writeBoolean(cacheAnimationFrames);
    stream.writeByte(saveIntervalIdx);
    stream.writeInt64(memoryLimit);
    stream.writeLongint(outputLinesLimit);
    stream.writeAnsiString(htmlDocGeneratedForCodeHash);
    stream.writeBoolean(doShowSplashScreen);
    stream.writeAnsiString(fullFlavourLocation);
    stream.writeAnsiString(lightFlavourLocation);
    stream.writeByte(newFileLineEnding);
    stream.writeByte(overwriteLineEnding);
    with externalRunOptions do begin
      stream.write(flags,sizeOf(flags));
      stream.writeBoolean   (callLightFlavour);
      stream.writeAnsiString(verbosity   );
      stream.writeAnsiString(customFolder);
    end;
    savedAt:=now;
  end;

PROCEDURE T_settings.initDefaults;
  begin
    cpuCount:=getNumberOfCPUs;
    with editor do begin
      fontName:='';
      fontSize:=0;
      antialiasedFonts:=false;
    end;
    with mainForm do begin
      top := 0;
      Left := 0;
      width := 480;
      height := 480;
      isFullscreen := false;
    end;
    with outline do begin
      showPrivate:=true;
      showImported:=true;
      sortByName:=false;
      sortByNameCaseSen:=true;
      sortByLoc:=false;
      fullHeight:=false;
      viewWidth:=100;
    end;
    outputBehavior    :=C_defaultIdeMessageConfig;
    quickOutputBehavior:=C_defaultIdeMessageConfig;
    doResetPlotOnEvaluation:=true;
    cacheAnimationFrames:=true;
    saveIntervalIdx:=0;
    wasLoaded:=false;
    savedAt:=now;
    memoryLimit:={$ifdef Windows}
                   {$ifdef CPU32}
                   1000000000;
                   {$else}
                   int64(maxLongint)*2;
                   {$endif}
                 {$else}
                 1000000000;
                 {$endif}
    outputLinesLimit:=maxLongint;
    doShowSplashScreen:=true;
    fullFlavourLocation:={$ifdef fullVersion}paramStr(0){$else}''{$endif};
    lightFlavourLocation:={$ifdef fullVersion}''{$else}paramStr(0){$endif};
    htmlDocGeneratedForCodeHash:='';
    newFileLineEnding:=LINE_ENDING_DEFAULT;
    overwriteLineEnding:=LINE_ENDING_UNCHANGED;
    with externalRunOptions do begin
      flags:=[];
      callLightFlavour:=false;
      verbosity:='';
      customFolder:='';
    end;
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

FUNCTION T_settings.savingRequested: boolean;
  begin
    result:=(now-savedAt)>C_SAVE_INTERVAL[saveIntervalIdx].interval;
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

FUNCTION T_fileHistory.getSerialVersion:dword; begin result:=176454893; end;

FUNCTION T_fileHistory.loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
  VAR i,count:longint;
  begin
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

CONSTRUCTOR T_formPosition.create;
  begin
    top:=0;
    Left:=0;
    width:=500;
    height:=500;
    isFullscreen:=true;
  end;

FUNCTION T_formPosition.getSerialVersion:dword; begin result:=1529642237; end;
FUNCTION T_formPosition.loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
  begin
    if not inherited loadFromStream(stream) then exit(false);
    top   :=stream.readLongint;
    Left  :=stream.readLongint;
    width :=stream.readLongint;
    height:=stream.readLongint;
    relativeSplitterPosition:=stream.readWord/65535;
    isFullscreen:=stream.readBoolean;
    result:=true;
  end;

PROCEDURE T_formPosition.saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  begin
    inherited saveToStream(stream);
    stream.writeLongint(top);
    stream.writeLongint(Left);
    stream.writeLongint(width);
    stream.writeLongint(height);
    if relativeSplitterPosition<0      then stream.writeWord(0)
    else if relativeSplitterPosition>1 then stream.writeWord(65535)
                                       else stream.writeWord(round(65535*relativeSplitterPosition));
    stream.writeBoolean(isFullscreen);
  end;

INITIALIZATION
  settings.create;
  if fileExists(settingsFileName)
  then settings.loadFromFile(settingsFileName)
  else settings.initDefaults;

FINALIZATION
  settings.destroy;

end.
