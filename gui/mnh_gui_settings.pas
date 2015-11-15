UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, myFiles, mnh_funcs, myGenerics, mySys, mnh_out_adapters,mnh_constants;

CONST
  STATE_SAVE_INTERVAL=ONE_MINUTE;

TYPE

  { TSettingsForm }

  T_formPosition=record
    top, left, width, height: longint;
    isFullscreen: boolean;
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
    fileInEditor: ansistring;
    mainForm:T_formPosition;
    outputBehaviour:T_outputBehaviour;
    instantEvaluation: boolean;
    doResetPlotOnEvaluation: boolean;
    fileContents: T_arrayOfString;
    fileHistory: array[0..9] of ansistring;

    //nonpersistent:
    changedSinceStore:boolean;
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
  public
    { public declarations }
    PROPERTY fontSize: longint read getFontSize write setFontSize;
    PROPERTY mainFormPosition: T_formPosition read getMainFormPosition write setMainFormPosition;
    PROPERTY wantInstantEvaluation: boolean read getInstantEvaluation write setInstantEvaluation;
    PROPERTY behaviour:T_outputBehaviour read getOutputBehaviour write setOutputBehaviour;
    PROPERTY resetPlotOnEvaluation:boolean read getResetPlotOnEvaluation write setResetPlotOnEvaluation;
    FUNCTION getEditorFontName: string;
    PROCEDURE saveSettings;
    PROCEDURE saveSettingsMaybe;
    PROCEDURE setFileContents(CONST data: TStrings);
    PROCEDURE getFileContents(CONST data: TStrings);
    FUNCTION setFileInEditor(fileName: ansistring): boolean;
    FUNCTION getFileInEditor: ansistring;
    FUNCTION polishHistory: boolean;
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

{ TSettingsForm }

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  VAR
    ff: T_file;
    iMax, i: longint;

  begin
    if fileExists(settingsFileName) then begin
      ff.createToRead(settingsFileName);

      setFontSize(ff.readLongint);
      editorFontname := ff.readAnsiString;
      EditorFontDialog.Font.name := editorFontname;

      AntialiasCheckbox.Checked := ff.readBoolean;
      with mainForm do begin
        top := ff.readLongint;
        left := ff.readLongint;
        width := ff.readLongint;
        height := ff.readLongint;
        isFullscreen := ff.readBoolean;
      end;
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

      fileInEditor := ff.readAnsiString;
      if fileInEditor = '' then begin
        iMax := ff.readLongint;
        if iMax>=0 then begin
          setLength(fileContents, iMax);
          for i := 0 to iMax-1 do
            fileContents[i] := ff.readAnsiString;
        end else setLength(fileContents, 0);
      end;
      ff.destroy;
      if not (fileExists(fileInEditor)) then
        fileInEditor := '';
    end else begin
      for i := 0 to 9 do fileHistory[i] := '';
      editorFontname := 'Courier New';
      fontSize := 11;
      with mainForm do begin
        top := 0;
        left := 0;
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
      fileInEditor := '';
    end;
    FontButton.Font.name := editorFontname;
    FontButton.Font.size := getFontSize;
    FontButton.Caption := editorFontname;
    with mainForm do begin
      if top<0 then
        top := 0;
      if left<0 then
        left := 0;
      if height>screen.height-top then
        height := screen.height-top;
      if width>screen.width-left then
        width := screen.width-left;
      if (height<0) or (width<0) then
        begin
        top := 0;
        left := 0;
        width := 480;
        height := 480;
        end;
    end;
    polishHistory;
    changedSinceStore:=false;
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
      changedSinceStore := true;
    end;
  end;

PROCEDURE TSettingsForm.FormDestroy(Sender: TObject);
  begin
    if changedSinceStore then saveSettings;
  end;

FUNCTION TSettingsForm.getFontSize: longint;
  begin
    result := StrToInt64Def(trim(FontSizeEdit.text), 12);
  end;

PROCEDURE TSettingsForm.setFontSize(CONST value: longint);
  begin
    FontSizeEdit.text := intToStr(value);
    EditorFontDialog.Font.size := value;
    changedSinceStore:=true;
  end;

FUNCTION TSettingsForm.getMainFormPosition: T_formPosition;
  begin
    result:=mainForm;
  end;

PROCEDURE TSettingsForm.setMainFormPosition(CONST value: T_formPosition);
  begin
    mainForm:=value;
    changedSinceStore:=true;
  end;

FUNCTION TSettingsForm.getInstantEvaluation: boolean;
  begin
    result:=instantEvaluation;
  end;

PROCEDURE TSettingsForm.setInstantEvaluation(CONST value: boolean);
  begin
    instantEvaluation:=value;
    changedSinceStore:=true;
  end;

FUNCTION TSettingsForm.getOutputBehaviour: T_outputBehaviour;
  begin
    result:=outputBehaviour;
  end;

PROCEDURE TSettingsForm.setOutputBehaviour(CONST value: T_outputBehaviour);
  begin
    outputBehaviour:=value;
    changedSinceStore:=true;
  end;

FUNCTION TSettingsForm.getResetPlotOnEvaluation: boolean;
  begin
    result:=doResetPlotOnEvaluation;
  end;

PROCEDURE TSettingsForm.setResetPlotOnEvaluation(CONST value: boolean);
  begin
    doResetPlotOnEvaluation:=value;
    changedSinceStore:=true;
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

    ff.writeLongint(getFontSize);
    ff.writeAnsiString(editorFontname);
    ff.writeBoolean(AntialiasCheckbox.Checked);

    with mainForm do begin
      ff.writeLongint(top);
      ff.writeLongint(left);
      ff.writeLongint(width);
      ff.writeLongint(height);
      ff.writeBoolean(isFullscreen);
    end;
    with outputBehaviour do begin
      ff.writeBoolean(doEchoInput);
      ff.writeBoolean(doEchoDeclaration);
      ff.writeBoolean(doShowExpressionOut);
      ff.writeBoolean(doShowTimingInfo);
      ff.writeShortint(minErrorLevel);
    end;
    ff.writeBoolean(instantEvaluation);
    ff.writeBoolean(doResetPlotOnEvaluation);
    for i := 0 to 9 do
      ff.writeAnsiString(fileHistory [i]);
    ff.writeAnsiString(fileInEditor);
    if fileInEditor = '' then
      begin
      ff.writeLongint(length(fileContents));
      for i := 0 to length(fileContents)-1 do
        ff.writeAnsiString(fileContents [i]);
      end;
    ff.destroy;
    changedSinceStore:=false;
    savedAt:=now;
  end;

PROCEDURE TSettingsForm.saveSettingsMaybe;
  begin
    if changedSinceStore and (now-savedAt>STATE_SAVE_INTERVAL) then saveSettings;
  end;

PROCEDURE TSettingsForm.setFileContents(CONST data: TStrings);
  VAR i: longint;
  begin
    if fileInEditor<>'' then begin
      setLength(fileContents, 0);
      exit;
    end;
    setLength(fileContents, data.count);
    for i := 0 to data.count-1 do fileContents[i] := data [i];
    changedSinceStore:=true;
  end;

PROCEDURE TSettingsForm.getFileContents(CONST data: TStrings);
  VAR  i: longint;
  begin
    if fileInEditor<>'' then exit;
    data.clear;
    for i := 0 to length(fileContents)-1 do
      data.append(fileContents [i]);
  end;

FUNCTION TSettingsForm.setFileInEditor(fileName: ansistring): boolean;
  VAR
    i: longint;
    tmp: ansistring;
  begin
    fileName:=expandFileName(fileName);
    if fileInEditor<>'' then begin
      polishHistory;
      i := 0;
      while (i<length(fileHistory)) and (fileHistory [i]<>fileInEditor) do inc(i);
      if (i>=length(fileHistory)) then begin
        i := length(fileHistory)-1;
        fileHistory[i] := fileInEditor;
      end;
      while (i>0) do begin
        tmp := fileHistory [i];
        fileHistory[i] := fileHistory [i-1];
        fileHistory[i-1] := tmp;
        dec(i);
      end;
      result := true;
    end else result := polishHistory;
    fileInEditor := fileName;
    changedSinceStore:=true;
  end;

FUNCTION TSettingsForm.getFileInEditor: ansistring;
  begin
    result := fileInEditor;
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
    changedSinceStore:=true;
  end;

FUNCTION TSettingsForm.historyItem(CONST index: longint): ansistring;
  begin
    if (index>=0) and (index<length(fileHistory))
    then result:=fileHistory[index]
    else result:='';
  end;

end.
