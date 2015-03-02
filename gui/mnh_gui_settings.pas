unit mnh_gui_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, myFiles, mnh_fileWrappers, mnh_stringutil, mnh_funcs;

CONST default_notepad_path='c:\Program Files (x86)\Notepad++\notepad++.exe';

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
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
    procedure FontButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MnhConsoleFileNameEditChange(Sender: TObject);
  private
    { private declarations }
    editorFontname:string;
    FUNCTION getFontSize:longint;
    PROCEDURE setFontSize(value:longint);
  public
    { public declarations }
    mainForm:record
      top,left,width,height:longint;
    end;
    outputBehaviour:record
      doEchoInput:boolean;
      doEchoDeclaration:boolean;
      doShowExpressionOut:boolean;
    end;
    fileInEditor:ansistring;
    PROPERTY fontSize:longint read getFontSize write setFontSize;
    FUNCTION getEditorFontName:string;
    FUNCTION canOpenFile(CONST filename:ansistring; CONST lineNumber:longint):boolean;
    PROCEDURE saveSettings;
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

FUNCTION settingsFileName:string;
  begin
    result:=ExpandFileName(extractFilePath(paramstr(0)))+'mnh_gui.settings';
  end;

FUNCTION defaultConsoleName:string;
  begin
    result:=replaceOne(paramstr(0),'mnh_gui','mnh_console');
  end;

{ TSettingsForm }

procedure TSettingsForm.FormCreate(Sender: TObject);
  VAR ff:T_file;
  begin
    if fileExists(settingsFileName) then begin
      ff.createToRead(settingsFileName);

      NotepadFileNameEdit.FileName:=ff.readAnsiString;
      mnh_funcs.mnh_console_executable:=ff.readAnsiString;
      MnhConsoleFileNameEdit.FileName:=mnh_funcs.mnh_console_executable;

      setFontSize(ff.readLongint);
      editorFontname:=ff.readAnsiString;
      EditorFontDialog.Font.Name:=editorFontname;

      AntialiasCheckbox.Checked:=ff.readBoolean;
      with mainForm do begin
        top   :=ff.readLongint;
        left  :=ff.readLongint;
        width :=ff.readLongint;
        height:=ff.readLongint;
      end;
      with outputBehaviour do begin
        doEchoInput:=ff.readBoolean;
        doEchoDeclaration:=ff.readBoolean;
        doShowExpressionOut:=ff.readBoolean;
      end;
      fileInEditor:=ff.readAnsiString;
      ff.destroy;
      if not(FileExists(fileInEditor)) then fileInEditor:='';
    end else begin
      if FileExists(default_notepad_path) then NotepadFileNameEdit.Filename:=default_notepad_path;
      if FileExists(defaultConsoleName) then begin
        mnh_console_executable:=defaultConsoleName;
        MnhConsoleFileNameEdit.FileName:=defaultConsoleName;
      end else begin
        mnh_console_executable:='';
        MnhConsoleFileNameEdit.FileName:='';
      end;
      editorFontname:='Courier New';
      fontSize:=11;
      with mainForm do begin
        top   :=0;
        left  :=0;
        width :=480;
        height:=480;
      end;
      with outputBehaviour do begin
        doEchoInput:=true;
        doEchoDeclaration:=true;
        doShowExpressionOut:=true;
      end;
      fileInEditor:='';
    end;
    FontButton.Font.Name:=editorFontname;
    FontButton.Font.Size:=getFontSize;
    FontButton.Caption:=editorFontname;
    with mainForm do begin
      if top<0 then top:=0;
      if left<0 then left:=0;
      if height>Screen.Height-top then height:=screen.Height-top;
      if width>screen.Width-left then width:=screen.Width-left;
      if (height<0) or (width<0) then begin
        top   :=0;
        left  :=0;
        width :=480;
        height:=480;
      end;
    end;
  end;

PROCEDURE TSettingsForm.FontButtonClick(Sender: TObject);
  begin
    if EditorFontDialog.Execute then begin
      setFontSize(EditorFontDialog.Font.Size);
      editorFontname:=EditorFontDialog.Font.Name;

      FontButton.Font.Name:=editorFontname;
      FontButton.Font.Size:=getFontSize;
      FontButton.Caption:=editorFontname;
    end;
  end;

PROCEDURE TSettingsForm.FormDestroy(Sender: TObject);
  begin
    saveSettings;
  end;

PROCEDURE TSettingsForm.MnhConsoleFileNameEditChange(Sender: TObject);
  begin
    mnh_console_executable:=MnhConsoleFileNameEdit.FileName;
  end;

FUNCTION TSettingsForm.getFontSize: longint;
  begin
    result:=StrToInt64Def(Trim(FontSizeEdit.Text),12);
  end;

PROCEDURE TSettingsForm.setFontSize(value: longint);
  begin
    FontSizeEdit.Text:=IntToStr(value);
    EditorFontDialog.Font.Size:=value;
  end;

FUNCTION TSettingsForm.getEditorFontName: string;
  begin
    result:=editorFontname;
  end;

FUNCTION TSettingsForm.canOpenFile(CONST filename: ansistring; CONST lineNumber: longint): boolean;
  VAR par:T_stringList;
  begin
    if (trim(NotepadFileNameEdit.Filename)<>'') and not(FileExistsUTF8(NotepadFileNameEdit.Filename)) then NotepadFileNameEdit.Filename:='';
    if (trim(NotepadFileNameEdit.Filename)<>'') then begin
      setLength(par,1); par[0]:=filename;
      if lineNumber>0 then begin
        setLength(par,2); par[1]:='-n'+IntToStr(lineNumber);
      end;
      result:=runCommandAsync(NotepadFileNameEdit.FileName,par);
    end else result:=false;
  end;

PROCEDURE TSettingsForm.saveSettings;
  VAR ff:T_file;
  begin
    ff.createToWrite(settingsFileName);

    ff.writeAnsiString(NotepadFileNameEdit.FileName);
    ff.writeAnsiString(mnh_console_executable);

    ff.writeLongint(getFontSize);
    ff.writeAnsiString(editorFontname);
    ff.writeBoolean(AntialiasCheckbox.Checked);

    with mainForm do begin
      ff.writeLongint(top);
      ff.writeLongint(left);
      ff.writeLongint(width);
      ff.writeLongint(height);
    end;
    with outputBehaviour do begin
      ff.writeBoolean(doEchoInput);
      ff.writeBoolean(doEchoDeclaration);
      ff.writeBoolean(doShowExpressionOut);
    end;
    ff.writeAnsiString(fileInEditor);
    ff.destroy;
  end;

end.

