UNIT mnh_settings;
INTERFACE
USES myFiles,myGenerics,Classes,sysutils,mnh_fileWrappers,mnh_out_adapters,mySys;
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

  CONSTRUCTOR create;
  CONSTRUCTOR create(CONST path:ansistring; CONST age:double; CONST change:boolean; CONST dat:TStrings);
  PROCEDURE getLines(CONST dat:TStrings);
  FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual;
  PROCEDURE saveToFile(VAR F:T_file);           virtual;
end;

{ T_guiSettings }

P_Settings=^T_Settings;
T_Settings=object(T_serializable)
  workerThreadCount:longint;
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

  wasLoaded:boolean;

  CONSTRUCTOR create;
  PROCEDURE reset;
  DESTRUCTOR destroy;
  FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual;
  PROCEDURE saveToFile(VAR F:T_file);           virtual;
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

constructor T_settings.create;
  VAR i:longint;
  begin
    workerThreadCount:=-1;
    mainForm.create;
    for i:=0 to length(editorState)-1 do editorState[i].create;
    wasLoaded:=false;
  end;

destructor T_settings.destroy;
  begin
  end;

FUNCTION workerThreadCount:longint;
  VAR F:T_file;
  begin
    result:=0;
    if settings.isObtained then result:=settings.value^.workerThreadCount;
    if fileExists(settingsFileName) then begin
      F.createToRead(settingsFileName);
      Result:=F.readLongint;
      F.destroy;
    end;
    if result<=0 then begin
      result:=getNumberOfCPUs-1;
      if result<1 then result:=1;
    end;
  end;

function T_settings.loadFromFile(var F: T_file): boolean;
  VAR i:longint;
  begin
    workerThreadCount:=F.readLongint;
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
    for i := 0 to 9 do
      fileHistory[i] := f.readAnsiString;

    for i:=0 to 9 do begin
      editorState[i].loadFromFile(f);
    end;
    activePage:=f.readLongint;
    if F.allOkay then begin
      reset;
      result:=false;
    end else result:=false;
  end;

procedure T_settings.saveToFile(var F: T_file);
  VAR i:longint;
  begin
    f.writeLongint(workerThreadCount);
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
    for i := 0 to 9 do F.writeAnsiString(fileHistory [i]);
    for i:=0 to 9 do begin
      editorState[i].saveToFile(F);
    end;
    F.writeLongint(activePage);
  end;

procedure T_settings.reset;
  VAR i:longint;
  begin
    for i := 0 to 9 do fileHistory[i] := '';
    editorFontname := 'Courier New';
    fontSize := 11;
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
  end;

CONSTRUCTOR T_formPosition.create;
  begin
    top:=0;
    left:=0;
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
  end;

CONSTRUCTOR T_editorState.create(CONST path: ansistring; CONST age: double; CONST change: boolean; CONST dat: TStrings);
  VAR i:longint;
  begin
    visible:=true;
    filePath:=path;
    fileAccessAge:=age;
    changed:=change;
    setLength(lines,dat.count);
    for i:=0 to dat.count-1 do lines[i]:=dat[i];
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
  end;

FUNCTION obtainSettings:P_settings;
  begin
    new(result,create);
    if FileExists(settingsFileName) then result^.loadFromFile(settingsFileName);
  end;

PROCEDURE disposeSettings(settings:P_settings);
  begin
    dispose(settings,destroy);
  end;

INITIALIZATION
  settings.create(@obtainSettings,nil);

finalization
  settings.destroy;

end.
