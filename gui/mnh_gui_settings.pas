UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, myFiles, mnh_funcs, myGenerics, mySys, mnh_out_adapters,mnh_constants,mnh_fileWrappers;

CONST
  STATE_SAVE_INTERVAL=ONE_MINUTE;

TYPE

  { TSettingsForm }

  { T_formPosition }

  T_formPosition=object(T_serializable)
    top, Left, width, height: longint;
    isFullscreen: boolean;
    CONSTRUCTOR create;
    FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual;
    PROCEDURE saveToFile(VAR F:T_file);           virtual;
  end;

  { T_editorState }

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

  TSettingsForm = class(TForm)
    FontButton: TButton;
    AntialiasCheckbox: TCheckBox;
    FontSizeEdit: TEdit;
    EditorFontDialog: TFontDialog;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    PROCEDURE FontButtonClick(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
  private
    { private declarations }
    editorFontname: string;
    mainForm:T_formPosition;
    outputBehaviour:T_outputBehaviour;
    instantEvaluation: boolean;
    doResetPlotOnEvaluation: boolean;
    fileHistory: array[0..9] of ansistring;
    editorState: array[0..9] of T_editorState;
    activePage:longint;
    //nonpersistent:
    savedAt:double;
    FUNCTION getFontSize: longint;
    PROCEDURE setFontSize(CONST value: longint);
    FUNCTION getMainFormPosition:T_formPosition;
    PROCEDURE setMainFormPosition(CONST value:T_formPosition);
    FUNCTION getInstantEvaluation:boolean;
    PROCEDURE setInstantEvaluation(CONST value:boolean);
    FUNCTION getOutputBehaviour:T_outputBehaviour;
    PROCEDURE setOutputBehaviour(CONST value:T_outputBehaviour);
    FUNCTION getResetPlotOnEvaluation:boolean;
    PROCEDURE setResetPlotOnEvaluation(CONST value:boolean);
    FUNCTION getPageIndex:longint;
    PROCEDURE setPageIndex(CONST value:longint);
  public
    { public declarations }
    PROPERTY fontSize: longint read getFontSize write setFontSize;
    PROPERTY mainFormPosition: T_formPosition read getMainFormPosition write setMainFormPosition;
    PROPERTY wantInstantEvaluation: boolean read getInstantEvaluation write setInstantEvaluation;
    PROPERTY behaviour:T_outputBehaviour read getOutputBehaviour write setOutputBehaviour;
    PROPERTY resetPlotOnEvaluation:boolean read getResetPlotOnEvaluation write setResetPlotOnEvaluation;
    PROPERTY pageIndex:longint read getPageIndex write setPageIndex;
    FUNCTION getEditorFontName: string;
    PROCEDURE saveSettings;
    FUNCTION timeForSaving:boolean;
    PROCEDURE setEditorState(CONST index:longint; CONST dat:T_editorState);
    FUNCTION  getEditorState(CONST index:longint):T_editorState;
    FUNCTION polishHistory: boolean;
    PROCEDURE fileClosed(CONST fileName:ansistring);
    FUNCTION historyItem(CONST index:longint):ansistring;
  end;

VAR
  SettingsForm: TSettingsForm;

IMPLEMENTATION

{$R *.lfm}

FUNCTION settingsFileName: string;
  begin
    result := expandFileName(extractFilePath(paramStr(0)))+'mnh_gui.settings';
  end;

{ T_editorState }

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

{ T_formPosition }
CONSTRUCTOR T_formPosition.create;
  begin
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

{ TSettingsForm }

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  VAR
    ff: T_file;
    i: longint;

  begin
    mainForm.create;
    for i:=0 to 9 do editorState[i].create;
    if fileExists(settingsFileName) then begin
      ff.createToRead(settingsFileName);

      fontSize:=ff.readLongint;
      editorFontname := ff.readAnsiString;
      EditorFontDialog.Font.name := editorFontname;
      AntialiasCheckbox.Checked := ff.readBoolean;

      mainForm.loadFromFile(ff);
      with outputBehaviour do begin
        doEchoInput := ff.readBoolean;
        doEchoDeclaration := ff.readBoolean;
        doShowExpressionOut := ff.readBoolean;
        doShowTimingInfo:= ff.readBoolean;
        minErrorLevel:=ff.readShortint;
      end;
      instantEvaluation := ff.readBoolean;
      doResetPlotOnEvaluation := ff.readBoolean;
      for i := 0 to 9 do
        fileHistory[i] := ff.readAnsiString;

      for i:=0 to 9 do begin
        editorState[i].loadFromFile(ff);
      end;
      activePage:=ff.readLongint;
      ff.destroy;
    end else begin
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
      activePage:=0;
    end;
    FontButton.Font.name := editorFontname;
    FontButton.Font.size := getFontSize;
    FontButton.Caption := editorFontname;
    with mainForm do begin
      if top<0 then
        top := 0;
      if Left<0 then
        Left := 0;
      if height>screen.height-top then
        height := screen.height-top;
      if width>screen.width-Left then
        width := screen.width-Left;
      if (height<0) or (width<0) then
        begin
        top := 0;
        Left := 0;
        width := 480;
        height := 480;
        end;
    end;
    polishHistory;
    savedAt:=now;
  end;

PROCEDURE TSettingsForm.FontButtonClick(Sender: TObject);
  begin
    if EditorFontDialog.execute then begin
      setFontSize(EditorFontDialog.Font.size);
      editorFontname := EditorFontDialog.Font.name;

      FontButton.Font.name := editorFontname;
      FontButton.Font.size := getFontSize;
      FontButton.Caption := editorFontname;
    end;
  end;

PROCEDURE TSettingsForm.FormDestroy(Sender: TObject);
  begin
    saveSettings;
  end;

FUNCTION TSettingsForm.getFontSize: longint;
  begin
    result := StrToInt64Def(trim(FontSizeEdit.text), 12);
  end;

PROCEDURE TSettingsForm.setFontSize(CONST value: longint);
  begin
    FontSizeEdit.text := intToStr(value);
    EditorFontDialog.Font.size := value;
  end;

FUNCTION TSettingsForm.getMainFormPosition: T_formPosition;
  begin
    result:=mainForm;
  end;

PROCEDURE TSettingsForm.setMainFormPosition(CONST value: T_formPosition);
  begin
    mainForm:=value;
  end;

FUNCTION TSettingsForm.getInstantEvaluation: boolean;
  begin
    result:=instantEvaluation;
  end;

PROCEDURE TSettingsForm.setInstantEvaluation(CONST value: boolean);
  begin
    instantEvaluation:=value;
  end;

FUNCTION TSettingsForm.getOutputBehaviour: T_outputBehaviour;
  begin
    result:=outputBehaviour;
  end;

PROCEDURE TSettingsForm.setOutputBehaviour(CONST value: T_outputBehaviour);
  begin
    outputBehaviour:=value;
  end;

FUNCTION TSettingsForm.getResetPlotOnEvaluation: boolean;
  begin
    result:=doResetPlotOnEvaluation;
  end;

PROCEDURE TSettingsForm.setResetPlotOnEvaluation(CONST value: boolean);
  begin
    doResetPlotOnEvaluation:=value;
  end;

FUNCTION TSettingsForm.getPageIndex: longint;
  begin
    result:=activePage;
  end;

PROCEDURE TSettingsForm.setPageIndex(CONST value: longint);
  begin
    if (value>=0) and (value<=9) then activePage:=value;
  end;

FUNCTION TSettingsForm.getEditorFontName: string;
  begin
    result := editorFontname;
  end;

PROCEDURE TSettingsForm.saveSettings;
  VAR
    ff: T_file;
    i: longint;
  begin
    ff.createToWrite(settingsFileName);

    ff.writeLongint(fontSize);
    ff.writeAnsiString(editorFontname);
    ff.writeBoolean(AntialiasCheckbox.Checked);

    mainForm.saveToFile(ff);
    with outputBehaviour do begin
      ff.writeBoolean(doEchoInput);
      ff.writeBoolean(doEchoDeclaration);
      ff.writeBoolean(doShowExpressionOut);
      ff.writeBoolean(doShowTimingInfo);
      ff.writeShortint(minErrorLevel);
    end;
    ff.writeBoolean(instantEvaluation);
    ff.writeBoolean(doResetPlotOnEvaluation);
    for i := 0 to 9 do ff.writeAnsiString(fileHistory [i]);
    for i:=0 to 9 do begin
      editorState[i].saveToFile(ff);
    end;
    ff.writeLongint(activePage);
    ff.destroy;
    savedAt:=now;
  end;

FUNCTION TSettingsForm.timeForSaving: boolean;
  begin
    result:=(now-savedAt>STATE_SAVE_INTERVAL);
  end;

PROCEDURE TSettingsForm.setEditorState(CONST index: longint;
  CONST dat: T_editorState);
  begin
    editorState[index]:=dat;
  end;

FUNCTION TSettingsForm.getEditorState(CONST index: longint): T_editorState;
  begin
    result:=editorState[index];
  end;

FUNCTION TSettingsForm.polishHistory: boolean;
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

PROCEDURE TSettingsForm.fileClosed(CONST fileName:ansistring);
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

FUNCTION TSettingsForm.historyItem(CONST index: longint): ansistring;
  begin
    if (index>=0) and (index<length(fileHistory))
    then result:=fileHistory[index]
    else result:='';
  end;

end.
