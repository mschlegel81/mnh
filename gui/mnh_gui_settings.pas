UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, myFiles, mnh_fileWrappers, mnh_stringutil, mnh_funcs;

CONST
  default_notepad_path = 'c:\Program Files (x86)\Notepad++\notepad++.exe';

TYPE

  { TSettingsForm }

  TSettingsForm = CLASS(TForm)
    MnhConsoleFileNameEdit: TFileNameEdit;
    FontButton: TButton;
    AntialiasCheckbox: TCheckBox;
    FontSizeEdit: TEdit;
    Label4: TLabel;
    NotepadFileNameEdit: TFileNameEdit;
    EditorFontDialog: TFontDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    PROCEDURE FontButtonClick(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE MnhConsoleFileNameEditChange(Sender: TObject);
  private
    { private declarations }
    editorFontname: string;
    fileInEditor: ansistring;
    FUNCTION getFontSize: longint;
    PROCEDURE setFontSize(Value: longint);
  public
    { public declarations }
    mainForm: RECORD
      top, left, Width, Height: longint;
    END;
    outputBehaviour: RECORD
      doEchoInput: boolean;
      doEchoDeclaration: boolean;
      doShowExpressionOut: boolean;
    END;

    fileContents: ARRAY OF ansistring;
    fileHistory: ARRAY[0..9] OF ansistring;

    PROPERTY fontSize: longint read getFontSize write setFontSize;
    FUNCTION getEditorFontName: string;
    FUNCTION canOpenFile(CONST filename: ansistring; CONST lineNumber: longint): boolean;
    PROCEDURE saveSettings;
    PROCEDURE setFileContents(CONST Data: TStrings);
    PROCEDURE getFileContents(CONST Data: TStrings);
    FUNCTION setFileInEditor(CONST filename: ansistring): boolean;
    FUNCTION getFileInEditor: ansistring;
    FUNCTION polishHistory:boolean;
  END;

VAR
  SettingsForm: TSettingsForm;

IMPLEMENTATION

{$R *.lfm}

FUNCTION settingsFileName: string;
  BEGIN
    Result := ExpandFileName(extractFilePath(ParamStr(0))) + 'mnh_gui.settings';
  end;

FUNCTION defaultConsoleName: string;
  begin
    result := replaceOne(ParamStr(0), 'mnh_gui', 'mnh_console');
  end;

{ TSettingsForm }

procedure TSettingsForm.FormCreate(Sender: TObject);
  VAR
    ff: T_file;
    iMax, i: longint;

  begin
    if fileExists(settingsFileName) then
      begin
      ff.createToRead(settingsFileName);

      NotepadFileNameEdit.FileName := ff.readAnsiString;
      mnh_funcs.mnh_console_executable := ff.readAnsiString;
      MnhConsoleFileNameEdit.FileName := mnh_funcs.mnh_console_executable;

      setFontSize(ff.readLongint);
      editorFontname := ff.readAnsiString;
      EditorFontDialog.Font.Name := editorFontname;

      AntialiasCheckbox.Checked := ff.readBoolean;
      WITH mainForm do
        begin
        top := ff.readLongint;
        left := ff.readLongint;
        Width := ff.readLongint;
        Height := ff.readLongint;
        end;
      WITH outputBehaviour do
        begin
        doEchoInput := ff.readBoolean;
        doEchoDeclaration := ff.readBoolean;
        doShowExpressionOut := ff.readBoolean;
        end;
      for i := 0 to 9 do
        fileHistory[i] := ff.readAnsiString;

      fileInEditor := ff.readAnsiString;
      if fileInEditor = '' then
        begin
        iMax := ff.readLongint;
        if iMax >= 0 then
          begin
          setLength(fileContents, iMax);
          for i := 0 to iMax - 1 do
            fileContents[i] := ff.readAnsiString;
          end
        else
          setLength(fileContents, 0);
        end;
      ff.Destroy;
      if not (FileExists(fileInEditor)) then
        fileInEditor := '';
      end
    else
      begin
      if FileExists(default_notepad_path) then
        NotepadFileNameEdit.Filename := default_notepad_path;
      if FileExists(defaultConsoleName) then
        begin
        mnh_console_executable := defaultConsoleName;
        MnhConsoleFileNameEdit.FileName := defaultConsoleName;
        end
      else
        begin
        mnh_console_executable := '';
        MnhConsoleFileNameEdit.FileName := '';
        end;
      for i := 0 to 9 do
        fileHistory[i] := '';
      editorFontname := 'Courier New';
      fontSize := 11;
      WITH mainForm do
        begin
        top := 0;
        left := 0;
        Width := 480;
        Height := 480;
        end;
      WITH outputBehaviour do
        begin
        doEchoInput := True;
        doEchoDeclaration := True;
        doShowExpressionOut := True;
        end;
      fileInEditor := '';
      end;
    FontButton.Font.Name := editorFontname;
    FontButton.Font.Size := getFontSize;
    FontButton.Caption := editorFontname;
    WITH mainForm do
      begin
      if top < 0 then
        top := 0;
      if left < 0 then
        left := 0;
      if Height > Screen.Height - top then
        Height := screen.Height - top;
      if Width > screen.Width - left then
        Width := screen.Width - left;
      if (Height < 0) or (Width < 0) then
        begin
        top := 0;
        left := 0;
        Width := 480;
        Height := 480;
        end;
      end;
  end;

procedure TSettingsForm.FontButtonClick(Sender: TObject);
  begin
    if EditorFontDialog.Execute then
      begin
      setFontSize(EditorFontDialog.Font.Size);
      editorFontname := EditorFontDialog.Font.Name;

      FontButton.Font.Name := editorFontname;
      FontButton.Font.Size := getFontSize;
      FontButton.Caption := editorFontname;
      end;
  end;

procedure TSettingsForm.FormDestroy(Sender: TObject);
  begin
    saveSettings;
  end;

procedure TSettingsForm.MnhConsoleFileNameEditChange(Sender: TObject);
  begin
    mnh_console_executable := MnhConsoleFileNameEdit.FileName;
  end;

function TSettingsForm.getFontSize: longint;
  begin
    result := StrToInt64Def(Trim(FontSizeEdit.Text), 12);
  end;

procedure TSettingsForm.setFontSize(Value: longint);
  begin
    FontSizeEdit.Text := IntToStr(Value);
    EditorFontDialog.Font.Size := Value;
  end;

function TSettingsForm.getEditorFontName: string;
  begin
    result := editorFontname;
  end;

function TSettingsForm.canOpenFile(const filename: ansistring;
  const lineNumber: longint): boolean;
  VAR
    par: T_stringList;
  begin
    if (trim(NotepadFileNameEdit.Filename) <> '') and not
      (FileExistsUTF8(NotepadFileNameEdit.Filename)) then
      NotepadFileNameEdit.Filename := '';
    if (trim(NotepadFileNameEdit.Filename) <> '') then
      begin
      setLength(par, 1);
      par[0] := filename;
      if lineNumber > 0 then
        begin
        setLength(par, 2);
        par[1] := '-n' + IntToStr(lineNumber);
        end;
      result := runCommandAsync(NotepadFileNameEdit.FileName, par);
      end
    else
      result := False;
  end;

procedure TSettingsForm.saveSettings;
  VAR
    ff: T_file;
    i: longint;
  begin
    ff.createToWrite(settingsFileName);

    ff.writeAnsiString(NotepadFileNameEdit.FileName);
    ff.writeAnsiString(mnh_console_executable);

    ff.writeLongint(getFontSize);
    ff.writeAnsiString(editorFontname);
    ff.writeBoolean(AntialiasCheckbox.Checked);

    WITH mainForm do
      begin
      ff.writeLongint(top);
      ff.writeLongint(left);
      ff.writeLongint(Width);
      ff.writeLongint(Height);
      end;
    WITH outputBehaviour do
      begin
      ff.writeBoolean(doEchoInput);
      ff.writeBoolean(doEchoDeclaration);
      ff.writeBoolean(doShowExpressionOut);
      end;
    for i := 0 to 9 do
      ff.writeAnsiString(fileHistory[i]);
    ff.writeAnsiString(fileInEditor);
    if fileInEditor = '' then
      begin
      ff.writeLongint(length(fileContents));
      for i := 0 to length(fileContents) - 1 do
        ff.writeAnsiString(fileContents[i]);
      end;
    ff.Destroy;
  end;

procedure TSettingsForm.setFileContents(const Data: TStrings);
  VAR
    i: longint;
  begin
    if fileInEditor <> '' then
      begin
      setLength(fileContents, 0);
      exit;
      end;
    setLength(fileContents, Data.Count);
    for i := 0 to Data.Count - 1 do
      fileContents[i] := Data[i];
  end;

procedure TSettingsForm.getFileContents(const Data: TStrings);
  VAR
    i: longint;
  begin
    if fileInEditor <> '' then
      exit;
    Data.Clear;
    for i := 0 to length(fileContents) - 1 do
      Data.Append(fileContents[i]);
  end;

function TSettingsForm.setFileInEditor(const filename: ansistring): boolean;
  VAR
    i: longint;
    tmp: ansistring;
  begin
    if fileInEditor <> '' then begin
      polishHistory;
      i := 0;
      while (i < length(fileHistory)) and (fileHistory[i] <> fileInEditor) do Inc(i);
      if (i >= length(fileHistory)) then begin
        i := length(fileHistory) - 1;
        fileHistory[i] := fileInEditor;
      end;
      while (i > 0) do begin
        tmp := fileHistory[i];
        fileHistory[i] := fileHistory[i - 1];
        fileHistory[i - 1] := tmp;
        Dec(i);
      end;
      result := True;
    end else result := polishHistory;
    fileInEditor := filename;
  end;

function TSettingsForm.getFileInEditor: ansistring;
  begin
    result := fileInEditor;
  end;

function TSettingsForm.polishHistory:boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=0 to length(fileHistory)-1 do if (fileHistory[i]<>'') and not(FileExists(fileHistory[i])) then begin
      for j:=i to length(fileHistory)-2 do fileHistory[j]:=fileHistory[j+1];
      fileHistory[length(fileHistory)-1]:='';
      result:=true;
    end;
  end;

end.
