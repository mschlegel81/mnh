UNIT mnh_settings;
INTERFACE
USES myFiles,myGenerics,Classes,sysutils,mnh_fileWrappers,mnh_out_adapters,mySys;
CONST
  C_SAVE_INTERVAL:array[0..6] of record text:string; interval:double; end=
  ((text:'off';        interval:1E6),
   (text:'1 minute';   interval:1/(24*60)),
   (text:'2 minutes';  interval:2/(24*60)),
   (text:'5 minutes';  interval:5/(24*60)),
   (text:'10 minutes'; interval:10/(24*60)),
   (text:'30 minutes'; interval:30/(24*60)),
   (text:'1 hour';     interval:1/24));

TYPE
T_formPosition=object(T_serializable)
  top, Left, width, height: longint;
  isFullscreen: boolean;
  CONSTRUCTOR create;
  FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual;
  PROCEDURE saveToFile(VAR F:T_file);           virtual;
end;

T_editorState=object(T_serializable)
  visible:boolean;
  filePath:ansistring;
  fileAccessAge:double;
  changed:boolean;
  lines:T_arrayOfString;
  markedLines:T_arrayOfLongint;

  CONSTRUCTOR create;
  CONSTRUCTOR create(CONST path:ansistring; CONST age:double; CONST change:boolean; CONST dat:TStrings; CONST markedIdx:T_arrayOfLongint);
  PROCEDURE getLines(CONST dat:TStrings);
  FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual;
  PROCEDURE saveToFile(VAR F:T_file);           virtual;
end;

{ T_guiSettings }

P_Settings=^T_settings;

{ T_settings }

T_settings=object(T_serializable)
  cpuCount:longint;
  editorFontname: string;
  fontSize:longint;
  antialiasedFonts:boolean;
  mainForm:T_formPosition;
  instantEvaluation: boolean;
  doResetPlotOnEvaluation: boolean;
  fileHistory: array[0..9] of ansistring;
  editorState: array[0..9] of T_editorState;
  activePage:longint;
  outputBehaviour: T_outputBehaviour;
  saveIntervalIdx:byte;

  wasLoaded:boolean;
  savedAt:double;

  CONSTRUCTOR create;
  PROCEDURE reset;
  DESTRUCTOR destroy;
  FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual;
  PROCEDURE saveToFile(VAR F:T_file);           virtual;

  FUNCTION savingRequested:boolean;
  FUNCTION polishHistory: boolean;
  PROCEDURE fileClosed(CONST fileName:ansistring);
  FUNCTION historyItem(CONST index:longint):ansistring;
end;

PROCEDURE saveSettings;
FUNCTION workerThreadCount:longint;
VAR settings:specialize G_lazyVar<P_Settings>;
IMPLEMENTATION

FUNCTION settingsFileName: string;
  begin
    result := GetAppConfigDir(true)+'mnh_gui.settings';
  end;

PROCEDURE saveSettings;
  begin
    settings.value^.saveToFile(settingsFileName);
  end;

CONSTRUCTOR T_settings.create;
  VAR i:longint;
  begin
    cpuCount:=0;
    mainForm.create;
    for i:=0 to length(editorState)-1 do editorState[i].create;
    wasLoaded:=false;
  end;

DESTRUCTOR T_settings.destroy;
  begin
  end;

FUNCTION workerThreadCount:longint;
  begin
    result:=settings.value^.cpuCount-1;
    if result>=0 then exit(result);
    result:=getNumberOfCPUs-1;
    if result<0 then result:=0;
    settings.value^.cpuCount:=result+1;
  end;

FUNCTION T_settings.loadFromFile(VAR F: T_file): boolean;
  VAR i:longint;
  begin
    cpuCount:=F.readLongint;
    if cpuCount<=0 then begin
      cpuCount:=getNumberOfCPUs-1;
      if cpuCount<1 then cpuCount:=1;
    end;
    fontSize:=f.readLongint;
    editorFontname := f.readAnsiString;
    antialiasedFonts:=f.readBoolean;
    mainForm.loadFromFile(f);
    with outputBehaviour do begin
      doEchoInput := f.readBoolean;
      doEchoDeclaration := f.readBoolean;
      doShowExpressionOut := f.readBoolean;
      doShowTimingInfo:= f.readBoolean;
      minErrorLevel:=f.readShortint;
    end;
    instantEvaluation := f.readBoolean;
    doResetPlotOnEvaluation := f.readBoolean;
    for i := 0 to 9 do fileHistory[i] := f.readAnsiString;
    for i:=0 to 9 do editorState[i].loadFromFile(f);
    activePage:=f.readLongint;
    saveIntervalIdx:=f.readByte;
    if F.allOkay then result:=true
    else begin
      reset;
      result:=false;
    end;
    savedAt:=now;
    wasLoaded:=result;
  end;

PROCEDURE T_settings.saveToFile(VAR F: T_file);
  VAR i:longint;
  begin
    f.writeLongint(cpuCount);
    F.writeLongint(fontSize);
    F.writeAnsiString(editorFontname);
    F.writeBoolean(antialiasedFonts);
    mainForm.saveToFile(F);
    with outputBehaviour do begin
      F.writeBoolean(doEchoInput);
      F.writeBoolean(doEchoDeclaration);
      F.writeBoolean(doShowExpressionOut);
      F.writeBoolean(doShowTimingInfo);
      F.writeShortint(minErrorLevel);
    end;
    F.writeBoolean(instantEvaluation);
    F.writeBoolean(doResetPlotOnEvaluation);
    for i:=0 to 9 do F.writeAnsiString(fileHistory [i]);
    for i:=0 to 9 do editorState[i].saveToFile(F);
    F.writeLongint(activePage);
    f.writeByte(saveIntervalIdx);
    savedAt:=now;
  end;

FUNCTION T_settings.savingRequested: boolean;
  begin
    result:=(now-savedAt)>C_SAVE_INTERVAL[saveIntervalIdx].interval;
  end;

PROCEDURE T_settings.reset;
  VAR i:longint;
  begin
    for i := 0 to 9 do fileHistory[i] := '';
    editorFontname := '';
    fontSize := 0;
    with mainForm do begin
      top := 0;
      Left := 0;
      width := 480;
      height := 480;
      isFullscreen := false;
    end;
    with outputBehaviour do begin
      doEchoInput := true;
      doEchoDeclaration := true;
      doShowExpressionOut := true;
      doShowTimingInfo:=false;
      minErrorLevel:=3;
    end;
    instantEvaluation := true;
    doResetPlotOnEvaluation := false;
    editorState[0].visible:=true;
    wasLoaded:=false;
    savedAt:=now;
  end;

FUNCTION T_settings.polishHistory: boolean;
  VAR i, j: longint;
  begin
    result := false;
    for i:=0 to length(fileHistory)-1 do
      if not(fileExists(fileHistory[i])) then begin
        fileHistory[i]:='';
        result:=true;
      end;
    for i:=1 to length(fileHistory)-1 do
      if (fileHistory[i]<>'') then for j:=0 to i-1 do
        if (expandFileName(fileHistory[i])=expandFileName(fileHistory[j])) then begin
          fileHistory[i]:='';
          result:=true;
        end;
    for i := 0 to length(fileHistory)-1 do if (fileHistory [i]='') then begin
      for j := i to length(fileHistory)-2 do fileHistory[j] := fileHistory [j+1];
      fileHistory[length(fileHistory)-1] := '';
      result := true;
    end;
  end;

PROCEDURE T_settings.fileClosed(CONST fileName: ansistring);
  VAR i:longint;
  begin
    for i:=0 to length(fileHistory)-1 do if fileHistory[i]='' then begin
      fileHistory[i]:=fileName;
      polishHistory;
      exit;
    end;
    for i:=0 to length(fileHistory)-2 do fileHistory[i]:=fileHistory[i+1];
    fileHistory[length(fileHistory)-1]:=fileName;
    polishHistory;
  end;

FUNCTION T_settings.historyItem(CONST index: longint): ansistring;
  begin
    if (index>=0) and (index<length(fileHistory))
    then result:=fileHistory[index]
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

FUNCTION T_formPosition.loadFromFile(VAR F: T_file): boolean;
  begin
    top   :=f.readLongint;
    Left  :=f.readLongint;
    width :=f.readLongint;
    height:=f.readLongint;
    isFullscreen:=f.readBoolean;
    result:=true;
  end;

PROCEDURE T_formPosition.saveToFile(VAR F: T_file);
  begin
    f.writeLongint(top);
    f.writeLongint(Left);
    f.writeLongint(width);
    f.writeLongint(height);
    f.writeBoolean(isFullscreen);
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

CONSTRUCTOR T_editorState.create(CONST path: ansistring; CONST age: double; CONST change: boolean; CONST dat: TStrings; CONST markedIdx: T_arrayOfLongint);
  VAR i:longint;
  begin
    visible:=true;
    filePath:=path;
    fileAccessAge:=age;
    changed:=change;
    setLength(lines,dat.count);
    for i:=0 to dat.count-1 do lines[i]:=dat[i];
    markedLines:=markedIdx;
  end;

PROCEDURE T_editorState.getLines(CONST dat: TStrings);
  VAR i:longint;
  begin
    dat.clear;
    for i:=0 to length(lines)-1 do dat.append(lines[i]);
  end;

FUNCTION T_editorState.loadFromFile(VAR F: T_file): boolean;
  VAR i:longint;
  begin
    visible:=f.readBoolean;
    if not(visible) then exit(true);
    filePath:=f.readAnsiString;
    changed:=f.readBoolean;
    if changed then begin
      fileAccessAge:=f.readDouble;
      i:=f.readLongint;
      if i>=0 then begin
        setLength(lines,i);
        for i:=0 to length(lines)-1 do lines[i]:=f.readAnsiString;
        result:=true;
      end else result:=false;
    end else begin
      lines:=fileLines(filePath,result);
      if result then fileAge(filePath,fileAccessAge)
                else visible:=false;
      result:=true;
    end;
    i:=f.readLongint;
    if i>=0 then begin
      setLength(markedLines,i);
      for i:=0 to length(markedLines)-1 do markedLines[i]:=f.readLongint;
    end else result:=false;
  end;

PROCEDURE T_editorState.saveToFile(VAR F: T_file);
  VAR i:longint;
  begin
    f.writeBoolean(visible);
    if not(visible) then exit;
    if filePath<>'' then filePath:=expandFileName(filePath);
    f.writeAnsiString(filePath);
    f.writeBoolean(changed);
    if changed then begin
      f.writeDouble(fileAccessAge);
      f.writeLongint(length(lines));
      for i:=0 to length(lines)-1 do f.writeAnsiString(lines[i]);
    end;
    f.writeLongint(length(markedLines));
    for i:=0 to length(markedLines)-1 do f.writeLongint(markedLines[i]);
  end;

FUNCTION obtainSettings:P_Settings;
  begin
    ensurePath(settingsFileName);
    new(result,create);
    if fileExists(settingsFileName) then result^.loadFromFile(settingsFileName);
  end;

PROCEDURE disposeSettings(settings:P_Settings);
  begin
    settings^.saveToFile(settingsFileName);
    dispose(settings,destroy);
  end;

INITIALIZATION
  settings.create(@obtainSettings,@disposeSettings);

FINALIZATION
  settings.destroy;

end.
