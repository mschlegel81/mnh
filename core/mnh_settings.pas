UNIT mnh_settings;
INTERFACE
USES Classes,sysutils,dateutils,typinfo,
     FileUtil,
     myGenerics,serializationUtil,mySys,
     mnh_fileWrappers,
     mnh_constants,mnh_messages,
     mnh_out_adapters;
CONST
  C_SAVE_INTERVAL:array[0..6] of record text:string; interval:double; end=
  ((text:'off';        interval:1E6),
   (text:'1 minute';   interval: 1/(24*60)),
   (text:'2 minutes';  interval: 2/(24*60)),
   (text:'5 minutes';  interval: 5/(24*60)),
   (text:'10 minutes'; interval:10/(24*60)),
   (text:'30 minutes'; interval:30/(24*60)),
   (text:'1 hour';     interval: 1/24));
  FILE_HISTORY_MAX_SIZE=1000;
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

T_fileHistory=object(T_serializable)
  items: T_arrayOfString;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  FUNCTION polishHistory: boolean;
  PROCEDURE fileClosed(CONST fileName:ansistring);
  FUNCTION recentFolders:T_arrayOfString;
  FUNCTION historyItem(CONST index:longint):ansistring;
  FUNCTION findFiles(CONST rootPath:string):T_arrayOfString;
end;

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
  editorFontname: string;
  fontSize:longint;
  antialiasedFonts:boolean;
  mainForm:T_formPosition;
  outputBehaviour: T_messageTypeSet;
  wordWrapEcho:boolean;
  outputLinesLimit:longint;
  doResetPlotOnEvaluation: boolean;
  cacheAnimationFrames: boolean;

  htmlDocGeneratedForCodeHash:string;
  doShowSplashScreen:boolean;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  PROCEDURE initDefaults;

  FUNCTION savingRequested:boolean;
  PROPERTY loaded:boolean read wasLoaded;
end;

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
  end;

DESTRUCTOR T_settings.destroy;
  begin
  end;

FUNCTION T_settings.getSerialVersion: dword; begin result:=1644235078; end;
FUNCTION T_settings.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  {$MACRO ON}
  {$define cleanExit:=begin initDefaults; exit(false) end}
  begin
    if not inherited loadFromStream(stream) then cleanExit;

    cpuCount:=stream.readLongint;
    if cpuCount<=0 then begin
      cpuCount:=getNumberOfCPUs;
      if cpuCount<1 then cpuCount:=1;
    end;
    fontSize:=stream.readLongint;
    editorFontname := stream.readAnsiString;
    antialiasedFonts:=stream.readBoolean;
    mainForm.loadFromStream(stream);
    outputBehaviour:=stream.readNaturalNumber;
    outputBehaviour:=outputBehaviour+[mt_clearConsole,mt_printline];
    doResetPlotOnEvaluation := stream.readBoolean;
    cacheAnimationFrames    := stream.readBoolean;
    saveIntervalIdx:=stream.readByte;
    wordWrapEcho:=stream.readBoolean;
    memoryLimit:=stream.readInt64;
    outputLinesLimit:=stream.readLongint;
    htmlDocGeneratedForCodeHash:=stream.readAnsiString;
    doShowSplashScreen:=stream.readBoolean or (CODE_HASH<>htmlDocGeneratedForCodeHash);
    if not(stream.allOkay) then cleanExit else result:=true;
    savedAt:=now;
    wasLoaded:=result;
  end;

PROCEDURE T_settings.saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  begin
    inherited saveToStream(stream);
    stream.writeLongint(cpuCount);
    stream.writeLongint(fontSize);
    stream.writeAnsiString(editorFontname);
    stream.writeBoolean(antialiasedFonts);
    mainForm.saveToStream(stream);
    stream.writeNaturalNumber(outputBehaviour);
    stream.writeBoolean(doResetPlotOnEvaluation);
    stream.writeBoolean(cacheAnimationFrames);
    stream.writeByte(saveIntervalIdx);
    stream.writeBoolean(wordWrapEcho);
    stream.writeInt64(memoryLimit);
    stream.writeLongint(outputLinesLimit);
    stream.writeAnsiString(htmlDocGeneratedForCodeHash);
    stream.writeBoolean(doShowSplashScreen);
    savedAt:=now;
  end;

PROCEDURE T_settings.initDefaults;
  begin
    wordWrapEcho:=false;
    cpuCount:=getNumberOfCPUs;
    editorFontname:='';
    fontSize:=0;
    antialiasedFonts:=false;
    with mainForm do begin
      top := 0;
      Left := 0;
      width := 480;
      height := 480;
      isFullscreen := false;
    end;
    outputBehaviour:=C_defaultOutputBehavior_interactive;
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
    htmlDocGeneratedForCodeHash:='';
  end;

FUNCTION T_settings.savingRequested: boolean;
  begin
    result:=(now-savedAt)>C_SAVE_INTERVAL[saveIntervalIdx].interval;
  end;

CONSTRUCTOR T_fileHistory.create;
  begin
    items:=C_EMPTY_STRING_ARRAY;
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
    if not(fileExists(items[i])) then begin
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

FUNCTION T_fileHistory.recentFolders:T_arrayOfString;
  VAR fileName:string;
  begin
    setLength(result,0);
    for fileName in items do appendIfNew(result,ExtractFileDir(fileName));
  end;

FUNCTION T_fileHistory.findFiles(CONST rootPath:string):T_arrayOfString;
  VAR allPathsToScan:T_arrayOfString;
      fileName:string;
      pathToScan:string;
      list:TStringList;
  begin
    allPathsToScan:=recentFolders;
    result:=C_EMPTY_STRING_ARRAY;
    listScriptFileNames(rootPath);
    for pathToScan in allPathsToScan do begin
      list:=FindAllFiles(pathToScan+DirectorySeparator,'',false);
      for fileName in list do begin
        append(result,fileName);
      end;
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
