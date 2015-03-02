unit mnh_gui_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, myFiles;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    FontButton: TButton;
    AntialiasCheckbox: TCheckBox;
    FontSizeEdit: TEdit;
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
  private
    { private declarations }
    editorFontname:string;
    FUNCTION getFontSize:longint;
    PROCEDURE setFontSize(value:longint);
  public
    { public declarations }
    PROPERTY fontSize:longint read getFontSize write setFontSize;
    FUNCTION getEditorFontName:string;
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

FUNCTION settingsFileName:string;
  begin
    result:=ExpandFileName(extractFilePath(paramstr(0)))+'mnh_gui.settings';
  end;

{ TSettingsForm }

procedure TSettingsForm.FormCreate(Sender: TObject);
  VAR ff:T_file;
  begin
    if fileExists(settingsFileName) then begin
      ff.createToRead(settingsFileName);

      NotepadFileNameEdit.FileName:=ff.readAnsiString;

      setFontSize(ff.readLongint);
      editorFontname:=ff.readAnsiString;
      EditorFontDialog.Font.Name:=editorFontname;

      FontButton.Font.Name:=editorFontname;
      FontButton.Font.Size:=getFontSize;
      FontButton.Caption:=editorFontname;

      AntialiasCheckbox.Checked:=ff.readBoolean;

      ff.destroy;
    end;
  end;

procedure TSettingsForm.FontButtonClick(Sender: TObject);
  begin
    if EditorFontDialog.Execute then begin
      setFontSize(EditorFontDialog.Font.Size);
      editorFontname:=EditorFontDialog.Font.Name;

      FontButton.Font.Name:=editorFontname;
      FontButton.Font.Size:=getFontSize;
      FontButton.Caption:=editorFontname;
    end;
  end;

procedure TSettingsForm.FormDestroy(Sender: TObject);
  VAR ff:T_file;
  begin
    ff.createToWrite(settingsFileName);

    ff.writeAnsiString(NotepadFileNameEdit.FileName);

    ff.writeLongint(getFontSize);
    ff.writeAnsiString(EditorFontDialog.Font.Name);
    ff.writeBoolean(AntialiasCheckbox.Checked);

    ff.destroy;
  end;

function TSettingsForm.getFontSize: longint;
  begin
    result:=StrToInt64Def(Trim(FontSizeEdit.Text),12);
  end;

procedure TSettingsForm.setFontSize(value: longint);
  begin
    FontSizeEdit.Text:=IntToStr(value);
    EditorFontDialog.Font.Size:=value;
  end;

function TSettingsForm.getEditorFontName: string;
  begin
    result:=editorFontname;
  end;

end.

