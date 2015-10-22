UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, myFiles, mnh_funcs, myGenerics, mySys, mnh_out_adapters;

TYPE

  { TSettingsForm }

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
    FUNCTION getFontSize: longint;
    PROCEDURE setFontSize(value: longint);
  public
    { public declarations }
    mainForm: record  top, left, width, height: longint;
      isFullscreen: boolean;
    end;
    outputBehaviour:T_outputBehaviour;
    instantEvaluation: boolean;
    resetPlotOnEvaluation: boolean;

    fileContents: T_arrayOfString;
    fileHistory: array[0..9] of ansistring;

    PROPERTY fontSize: longint read getFontSize write setFontSize;
    FUNCTION getEditorFontName: string;
    PROCEDURE saveSettings;
    PROCEDURE setFileContents(CONST data: TStrings);
    PROCEDURE getFileContents(CONST data: TStrings);
    FUNCTION setFileInEditor(CONST fileName: ansistring): boolean;
    FUNCTION getFileInEditor: ansistring;
    FUNCTION polishHistory: boolean;
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
      end;
      instantEvaluation := ff.readBoolean;
      resetPlotOnEvaluation := ff.readBoolean;
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
      end;
      instantEvaluation := true;
      resetPlotOnEvaluation := false;
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

PROCEDURE TSettingsForm.setFontSize(value: longint);
  begin
    FontSizeEdit.text := intToStr(value);
    EditorFontDialog.Font.size := value;
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

    with mainForm do
      begin
      ff.writeLongint(top);
      ff.writeLongint(left);
      ff.writeLongint(width);
      ff.writeLongint(height);
      ff.writeBoolean(isFullscreen);
      end;
    with outputBehaviour do
      begin
      ff.writeBoolean(doEchoInput);
      ff.writeBoolean(doEchoDeclaration);
      ff.writeBoolean(doShowExpressionOut);
      end;
    ff.writeBoolean(instantEvaluation);
    ff.writeBoolean(resetPlotOnEvaluation);
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
  end;

PROCEDURE TSettingsForm.setFileContents(CONST data: TStrings);
  VAR
    i: longint;
  begin
    if fileInEditor<>'' then
      begin
      setLength(fileContents, 0);
      exit;
      end;
    setLength(fileContents, data.count);
    for i := 0 to data.count-1 do
      fileContents[i] := data [i];
  end;

PROCEDURE TSettingsForm.getFileContents(CONST data: TStrings);
  VAR
    i: longint;
  begin
    if fileInEditor<>'' then
      exit;
    data.clear;
    for i := 0 to length(fileContents)-1 do
      data.append(fileContents [i]);
  end;

FUNCTION TSettingsForm.setFileInEditor(CONST fileName: ansistring): boolean;
  VAR
    i: longint;
    tmp: ansistring;
  begin
    if fileInEditor<>'' then
      begin
      polishHistory;
      i := 0;
      while (i<length(fileHistory)) and (fileHistory [i]<>fileInEditor) do inc(i);
      if (i>=length(fileHistory)) then
        begin
        i := length(fileHistory)-1;
        fileHistory[i] := fileInEditor;
        end;
      while (i>0) do
        begin
        tmp := fileHistory [i];
        fileHistory[i] := fileHistory [i-1];
        fileHistory[i-1] := tmp;
        dec(i);
        end;
      result := true;
      end
    else result := polishHistory;
    fileInEditor := fileName;
  end;

FUNCTION TSettingsForm.getFileInEditor: ansistring;
  begin
    result := fileInEditor;
  end;

FUNCTION TSettingsForm.polishHistory: boolean;
  VAR i, j: longint;
  begin
    result := false;
    for i := 0 to length(fileHistory)-1 do if (fileHistory [i]<>'') and not(fileExists(fileHistory [i])) then begin
      for j := i to length(fileHistory)-2 do fileHistory[j] := fileHistory [j+1];
      fileHistory[length(fileHistory)-1] := '';
      result := true;
    end;
  end;

end.
