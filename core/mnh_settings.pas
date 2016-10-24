UNIT mnh_settings;
INTERFACE
USES myGenerics,dateutils,Classes,sysutils,mnh_fileWrappers,mnh_out_adapters,mySys,mnh_constants,serializationUtil,typinfo;
CONST
  C_SAVE_INTERVAL:array[0..6] of record text:string; interval:double; end=
  ((text:'off';        interval:1E6),
   (text:'1 minute';   interval:1/(24*60)),
   (text:'2 minutes';  interval:2/(24*60)),
   (text:'5 minutes';  interval:5/(24*60)),
   (text:'10 minutes'; interval:10/(24*60)),
   (text:'30 minutes'; interval:30/(24*60)),
   (text:'1 hour';     interval:1/24));
  FILE_HISTORY_MAX_SIZE=20;
TYPE
T_formPosition=object(T_serializable)
  top, Left, width, height: longint;
  isFullscreen: boolean;
  CONSTRUCTOR create;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_streamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_streamWrapper); virtual;
end;

T_editorState=object(T_serializable)
  visible:boolean;
  filePath:ansistring;
  fileAccessAge:double;
  changed:boolean;
  lines:T_arrayOfString;
  markedLines:T_arrayOfLongint;
  caret:array['x'..'y'] of longint;
  language:byte;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  PROCEDURE getLines(CONST dat: TStrings);
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_streamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_streamWrapper); virtual;
end;

T_fileHistory=object(T_serializable)
  items: T_listOfString;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_streamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_streamWrapper); virtual;
  FUNCTION polishHistory: boolean;
  PROCEDURE fileClosed(CONST fileName:ansistring);
  FUNCTION historyItem(CONST index:longint):ansistring;
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

  //Workspace:
  doResetPlotOnEvaluation: boolean;
  fileHistory:T_fileHistory;
  editorState: array of T_editorState;
  activePage:longint;
  outputBehaviour: T_messageTypeSet;
  wordWrapEcho:boolean;
  outputLinesLimit:longint;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_streamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_streamWrapper); virtual;
  PROCEDURE initDefaults;

  FUNCTION savingRequested:boolean;
  PROPERTY loaded:boolean read wasLoaded;
end;

PROCEDURE saveSettings;
FUNCTION workerThreadCount:longint;
VAR settings:specialize G_lazyVar<P_Settings>;
IMPLEMENTATION

FUNCTION settingsFileName: string;
  begin
    result:=configDir+'mnh_gui.settings';
  end;

PROCEDURE saveSettings;
  begin
    settings.value^.saveToFile(settingsFileName);
  end;

CONSTRUCTOR T_settings.create;
  begin
    cpuCount:=0;
    mainForm.create;
    setLength(editorState,0);
    fileHistory.create;
    wasLoaded:=false;
  end;

DESTRUCTOR T_settings.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(editorState)-1 do editorState[i].destroy;
    setLength(editorState,0);
  end;

FUNCTION workerThreadCount:longint;
  begin
    result:=settings.value^.cpuCount-1;
    if result>=0 then exit(result);
    result:=getNumberOfCPUs-1;
    if result<0 then result:=0;
    settings.value^.cpuCount:=result+1;
  end;

FUNCTION T_settings.getSerialVersion:dword; begin result:=1644235074; end;
FUNCTION T_settings.loadFromStream(VAR stream:T_streamWrapper): boolean;
  {$MACRO ON}
  {$define cleanExit:=begin initDefaults; exit(false) end}
  VAR i:longint;
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
    if not(fileHistory.loadFromStream(stream)) then cleanExit;
    setLength(editorState,stream.readNaturalNumber);
    if not(stream.allOkay) then cleanExit;
    for i:=0 to length(editorState)-1 do begin
      editorState[i].create;
      editorState[i].loadFromStream(stream);
    end;
    activePage:=stream.readLongint;
    saveIntervalIdx:=stream.readByte;
    wordWrapEcho:=stream.readBoolean;
    memoryLimit:=stream.readInt64;
    outputLinesLimit:=stream.readLongint;
    if not(stream.allOkay) then cleanExit else result:=true;
    savedAt:=now;
    wasLoaded:=result;
  end;

PROCEDURE T_settings.saveToStream(VAR stream:T_streamWrapper);
  VAR i:longint;
      visibleEditorCount:longint=0;
  begin
    inherited saveToStream(stream);
    stream.writeLongint(cpuCount);
    stream.writeLongint(fontSize);
    stream.writeAnsiString(editorFontname);
    stream.writeBoolean(antialiasedFonts);
    mainForm.saveToStream(stream);
    stream.writeNaturalNumber(outputBehaviour);
    stream.writeBoolean(doResetPlotOnEvaluation);
    fileHistory.saveToStream(stream);
    for i:=0 to length(editorState)-1 do if editorState[i].visible then inc(visibleEditorCount);
    stream.writeNaturalNumber(visibleEditorCount);
    for i:=0 to length(editorState)-1 do if editorState[i].visible then editorState[i].saveToStream(stream);
    stream.writeLongint(activePage);
    stream.writeByte(saveIntervalIdx);
    stream.writeBoolean(wordWrapEcho);
    stream.writeInt64(memoryLimit);
    stream.writeLongint(outputLinesLimit);
    savedAt:=now;
  end;

PROCEDURE T_settings.initDefaults;
  VAR i:longint;
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
    saveIntervalIdx:=0;
    wasLoaded:=false;
    savedAt:=now;
    for i:=0 to length(editorState)-1 do editorState[i].destroy;
    setLength(editorState,1);
    editorState[0].create;
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
  end;

FUNCTION T_settings.savingRequested: boolean;
  begin
    result:=(now-savedAt)>C_SAVE_INTERVAL[saveIntervalIdx].interval;
  end;


CONSTRUCTOR T_fileHistory.create;
  begin
    items.create;
  end;

DESTRUCTOR T_fileHistory.destroy;
  begin
    items.destroy;
  end;

FUNCTION T_fileHistory.getSerialVersion:dword; begin result:=176454893; end;

FUNCTION T_fileHistory.loadFromStream(VAR stream:T_streamWrapper):boolean;
  VAR i,count:longint;
  begin
    if not(inherited loadFromStream(stream)) then exit(false);
    items.clear;
    count:=stream.readNaturalNumber;
    if count>FILE_HISTORY_MAX_SIZE then exit(false);
    for i:=0 to count-1 do items.add(stream.readAnsiString);
    result:=stream.allOkay;
  end;

PROCEDURE T_fileHistory.saveToStream(VAR stream:T_streamWrapper);
  VAR i,count:longint;
  begin
    inherited saveToStream(stream);
    count:=items.size;
    if count>FILE_HISTORY_MAX_SIZE then count:=FILE_HISTORY_MAX_SIZE;
    stream.writeNaturalNumber(count);
    for i:=0 to count-1 do stream.writeAnsiString(items[i]);
  end;

FUNCTION T_fileHistory.polishHistory: boolean;
  VAR i, j: longint;
  begin
    result := false;
    for i:=0 to items.size-1 do
      if not(fileExists(items[i])) then begin
        items[i]:='';
        result:=true;
      end;
    i:=0;
    for i:=1 to items.size-1 do
      if (items[i]<>'') then for j:=0 to i-1 do
        if (expandFileName(items[i])=expandFileName(items[j])) then begin
          items[j]:='';
          result:=true;
        end;
    items.remValue('');
  end;

PROCEDURE T_fileHistory.fileClosed(CONST fileName: ansistring);
  begin
    items.add(fileName,0);
    polishHistory;
  end;

FUNCTION T_fileHistory.historyItem(CONST index: longint): ansistring;
  begin
    if (index>=0) and (index<items.size)
    then result:=items[index]
    else result:='';
  end;

CONSTRUCTOR T_formPosition.create;
  begin
    top:=0;
    Left:=0;
    width:=500;
    height:=500;
    isFullscreen:=true;
  end;

FUNCTION T_formPosition.getSerialVersion:dword; begin result:=1529642236; end;
FUNCTION T_formPosition.loadFromStream(VAR stream:T_streamWrapper):boolean;
  begin
    if not inherited loadFromStream(stream) then exit(false);
    top   :=stream.readLongint;
    Left  :=stream.readLongint;
    width :=stream.readLongint;
    height:=stream.readLongint;
    isFullscreen:=stream.readBoolean;
    result:=true;
  end;

PROCEDURE T_formPosition.saveToStream(VAR stream:T_streamWrapper);
  begin
    inherited saveToStream(stream);
    stream.writeLongint(top);
    stream.writeLongint(Left);
    stream.writeLongint(width);
    stream.writeLongint(height);
    stream.writeBoolean(isFullscreen);
  end;

CONSTRUCTOR T_editorState.create;
  begin
    visible:=false;
    setLength(lines,0);
    filePath:='';
    fileAccessAge:=0;
    changed:=false;
    setLength(lines,0);
    setLength(markedLines,0);
  end;

DESTRUCTOR T_editorState.destroy;
  begin
    filePath:='';
    fileAccessAge:=0;
    setLength(lines,0);
    setLength(markedLines,0);
  end;

PROCEDURE T_editorState.getLines(CONST dat: TStrings);
  VAR i:longint;
  begin
    dat.clear;
    for i:=0 to length(lines)-1 do dat.append(lines[i]);
  end;

FUNCTION T_editorState.getSerialVersion:dword; begin result:=1417366166; end;

FUNCTION T_editorState.loadFromStream(VAR stream:T_streamWrapper):boolean;
  VAR i:longint;
  begin
    if not inherited loadFromStream(stream) then exit(false);
    visible:=true;
    filePath:=stream.readAnsiString;
    changed:=stream.readBoolean;
    if changed then begin
      fileAccessAge:=stream.readDouble;
      setLength(lines,stream.readNaturalNumber);
      for i:=0 to length(lines)-1 do lines[i]:=stream.readAnsiString;
     end else begin
      lines:=fileLines(filePath,result);
      if result then fileAge(filePath,fileAccessAge)
                else visible:=false;
    end;
    setLength(markedLines,stream.readNaturalNumber);
    for i:=0 to length(markedLines)-1 do markedLines[i]:=stream.readLongint;
    caret['x']:=stream.readNaturalNumber;
    caret['y']:=stream.readNaturalNumber;
    language:=stream.readByte;
    result:=stream.allOkay;
  end;

PROCEDURE T_editorState.saveToStream(VAR stream:T_streamWrapper);
  VAR i:longint;
  begin
    inherited saveToStream(stream);
    if filePath<>'' then filePath:=expandFileName(filePath);
    stream.writeAnsiString(filePath);
    stream.writeBoolean(changed);
    if changed then begin
      stream.writeDouble(fileAccessAge);
      stream.writeNaturalNumber(length(lines));
      for i:=0 to length(lines)-1 do stream.writeAnsiString(lines[i]);
    end;
    stream.writeNaturalNumber(length(markedLines));
    for i:=0 to length(markedLines)-1 do stream.writeLongint(markedLines[i]);
    stream.writeNaturalNumber(caret['x']);
    stream.writeNaturalNumber(caret['y']);
    stream.writeByte(language);
  end;

FUNCTION obtainSettings:P_Settings;
  begin
    ensurePath(settingsFileName);
    new(result,create);
    if fileExists(settingsFileName)
    then result^.loadFromFile(settingsFileName)
    else result^.initDefaults;
  end;

PROCEDURE disposeSettings(settings:P_Settings);
  begin
    dispose(settings,destroy);
  end;

INITIALIZATION
  settings.create(@obtainSettings,@disposeSettings);

FINALIZATION
  settings.destroy;

end.
