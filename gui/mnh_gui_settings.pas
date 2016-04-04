UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, mnh_funcs, myGenerics, mySys, mnh_out_adapters,mnh_constants,
  mnh_packages,myStringUtil,mnh_settings;

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
    Label8: TLabel;
    TabSheet1: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    uninstallToggleBox: TToggleBox;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    workerThreadCountEdit: TEdit;
    Label4: TLabel;
    autosaveComboBox: TComboBox;
    PROCEDURE FontButtonClick(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE Button1Click(Sender: TObject);
    PROCEDURE Button2Click(Sender: TObject);
    PROCEDURE workerThreadCountEditEditingDone(Sender: TObject);
    PROCEDURE AntialiasCheckboxChange(Sender: TObject);
    PROCEDURE autosaveComboBoxChange(Sender: TObject);
  private
    FUNCTION getFontSize: longint;
    PROCEDURE setFontSize(CONST value: longint);
  public
    PROPERTY fontSize:longint read getFontSize write setFontSize;
  end;

VAR
  SettingsForm: TSettingsForm;

IMPLEMENTATION

{$R *.lfm}

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  PROCEDURE ensurePackages;
    {$i res_ensurePackages.inc}
    VAR code:T_arrayOfString;
        i:longint;
    begin
      setLength(code,length(ensurePackages_mnh));
      for i:=0 to length(code)-1 do code[i]:=ensurePackages_mnh[i];
      append(code,'('+escapeString(GetAppConfigDir(true))+')');
      runAlone(code);
    end;

  VAR i:longint;

  begin
    EditorFontDialog.Font.name := settings.value^.editorFontname;
    AntialiasCheckbox.Checked := settings.value^.antialiasedFonts;
    setFontSize(settings.value^.fontSize);
    if not(settings.value^.wasLoaded) then begin
      settings.value^.activePage:=0;
      ensurePackages;
    end;
    workerThreadCountEdit.text:=intToStr(settings.value^.cpuCount);
    FontButton.Font.name := settings.value^.editorFontname;
    FontButton.Font.size := getFontSize;
    FontButton.Caption := settings.value^.editorFontname;
    with settings.value^.mainForm do begin
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
    settings.value^.polishHistory;

    autosaveComboBox.Items.clear;
    for i:=0 to length(C_SAVE_INTERVAL)-1 do autosaveComboBox.Items.add(C_SAVE_INTERVAL[i].text);
    autosaveComboBox.ItemIndex:=settings.value^.saveIntervalIdx;
  end;

PROCEDURE TSettingsForm.FontButtonClick(Sender: TObject);
  begin
    if EditorFontDialog.execute then begin
      setFontSize(EditorFontDialog.Font.size);
      settings.value^.editorFontname := EditorFontDialog.Font.name;

      FontButton.Font.name := settings.value^.editorFontname;
      FontButton.Font.size := getFontSize;
      FontButton.Caption := settings.value^.editorFontname;
    end;
  end;

PROCEDURE TSettingsForm.Button1Click(Sender: TObject);
  {$i res_ensureNppHighlighting.inc}
  begin
    runAlone(ensureNotepad__Highlighting_mnh);
  end;

PROCEDURE TSettingsForm.Button2Click(Sender: TObject);
  {$i res_ensureMnhFileAssociations.inc}
  begin
    runAlone(ensureMnhFileAssociations_mnh);
  end;

PROCEDURE TSettingsForm.workerThreadCountEditEditingDone(Sender: TObject);
  VAR newValue:longint;
  begin
    newValue:=strToIntDef(workerThreadCountEdit.text,0);
    if newValue<=0 then workerThreadCountEdit.text:=intToStr(settings.value^.cpuCount)
                   else settings.value^.cpuCount:=newValue;
  end;

PROCEDURE TSettingsForm.AntialiasCheckboxChange(Sender: TObject);
  begin
    settings.value^.antialiasedFonts:=AntialiasCheckbox.Checked;
  end;

PROCEDURE TSettingsForm.autosaveComboBoxChange(Sender: TObject);
  begin
    settings.value^.saveIntervalIdx:=autosaveComboBox.ItemIndex;
  end;

FUNCTION TSettingsForm.getFontSize: longint;
  begin
    result := StrToInt64Def(trim(FontSizeEdit.text), 12);
  end;

PROCEDURE TSettingsForm.setFontSize(CONST value: longint);
  begin
    FontSizeEdit.text := intToStr(value);
    EditorFontDialog.Font.size := value;
    settings.value^.fontSize:=value;
  end;

end.
