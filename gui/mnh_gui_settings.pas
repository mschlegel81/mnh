UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, mySys,mnh_funcs, mnh_out_adapters, mnh_constants,
  mnh_packages,mnh_settings,mnh_doc;

CONST MINIMUM_OUTPUT_LINES=16;
TYPE

  { TSettingsForm }

  TSettingsForm = class(TForm)
    restorePacksAndDemosButton: TButton;
    installButton: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    uninstallButton: TButton;
    outputSizeLimit: TEdit;
    Label7: TLabel;
    memLimitEdit: TEdit;
    Label6: TLabel;
    FontButton: TButton;
    AntialiasCheckbox: TCheckBox;
    FontSizeEdit: TEdit;
    EditorFontDialog: TFontDialog;
    Label2: TLabel;
    Label3: TLabel;
    PageControl: TPageControl;
    TabSheet_display: TTabSheet;
    TabSheet_install: TTabSheet;
    TabSheet_global: TTabSheet;
    Label1: TLabel;
    workerThreadCountEdit: TEdit;
    Label4: TLabel;
    autosaveComboBox: TComboBox;
    PROCEDURE restorePacksAndDemosButtonClick(Sender: TObject);
    PROCEDURE installButtonClick(Sender: TObject);
    PROCEDURE FontButtonClick(Sender: TObject);
    PROCEDURE FontSizeEditEditingDone(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE memLimitEditEditingDone(Sender: TObject);
    PROCEDURE outputSizeLimitEditingDone(Sender: TObject);
    PROCEDURE uninstallButtonClick(Sender: TObject);
    PROCEDURE workerThreadCountEditEditingDone(Sender: TObject);
    PROCEDURE AntialiasCheckboxChange(Sender: TObject);
    PROCEDURE autosaveComboBoxChange(Sender: TObject);
  private
    FUNCTION getFontSize: longint;
    PROCEDURE setFontSize(CONST value: longint);
    FUNCTION getOutputLimit: longint;
    PROCEDURE setOutputLimit(CONST value: longint);
  public
    PROPERTY fontSize:longint read getFontSize write setFontSize;
    PROCEDURE ensureFont(CONST editorFont:TFont);
  end;

FUNCTION SettingsForm: TSettingsForm;
IMPLEMENTATION
VAR mySettingsForm: TSettingsForm=nil;
FUNCTION SettingsForm: TSettingsForm;
  begin
    if mySettingsForm=nil then mySettingsForm:=TSettingsForm.create(nil);
    result:=mySettingsForm;
  end;

{$R *.lfm}

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    if settings.editorFontname<>'' then begin
      EditorFontDialog.Font.name := settings.editorFontname;
      FontButton.Font.name := settings.editorFontname;
    end;
    AntialiasCheckbox.checked := settings.antialiasedFonts;
    setFontSize(settings.fontSize);
    setOutputLimit(settings.outputLinesLimit);
    if not(settings.loaded) then ensureDemosAndPackages;
    workerThreadCountEdit.text:=intToStr(settings.cpuCount);
    memLimitEdit.text:=intToStr(settings.memoryLimit shr 20);
    FontButton.Font.size := getFontSize;
    FontButton.caption := settings.editorFontname;
    with settings.mainForm do begin
      if top<0  then top := 0;
      if Left<0 then Left := 0;
      if height>screen.height-top then height := screen.height-top;
      if width>screen.width-Left then width := screen.width-Left;
      if (height<0) or (width<0) then  begin
        top := 0;
        Left := 0;
        width := 480;
        height := 480;
      end;
    end;
    autosaveComboBox.items.clear;
    for i:=0 to length(C_SAVE_INTERVAL)-1 do autosaveComboBox.items.add(C_SAVE_INTERVAL[i].text);
    autosaveComboBox.ItemIndex:=settings.saveIntervalIdx;
  end;

PROCEDURE TSettingsForm.FontButtonClick(Sender: TObject);
  begin
    if EditorFontDialog.execute then begin
      setFontSize(EditorFontDialog.Font.size);
      settings.editorFontname := EditorFontDialog.Font.name;

      FontButton.Font.name := settings.editorFontname;
      FontButton.Font.size := getFontSize;
      FontButton.caption := settings.editorFontname;
    end;
  end;

PROCEDURE TSettingsForm.FontSizeEditEditingDone(Sender: TObject);
  begin
    setFontSize(getFontSize);
  end;

PROCEDURE TSettingsForm.installButtonClick(Sender: TObject);
  begin
    sandbox^.runInstallScript;
    settings.fixLocations;
  end;

PROCEDURE TSettingsForm.restorePacksAndDemosButtonClick(Sender: TObject);
  begin
    ensureDemosAndPackages(true);
  end;

PROCEDURE TSettingsForm.uninstallButtonClick(Sender: TObject);
  begin
    sandbox^.runUninstallScript;
  end;

PROCEDURE TSettingsForm.ensureFont(CONST editorFont:TFont);
  begin
    if settings.editorFontname<>'' then exit;
    settings.editorFontname:=editorFont.name;
    EditorFontDialog.Font.name := settings.editorFontname;
    FontButton.Font.name := settings.editorFontname;
    setFontSize(editorFont.size);
    FontButton.Font.size := getFontSize;
    FontButton.caption := settings.editorFontname;
  end;

PROCEDURE TSettingsForm.FormShow(Sender: TObject);
  begin
    {$ifndef Windows}
    TabSheet_install.visible:=false;
    TabSheet_install.enabled:=false;
    TabSheet_install.tabVisible:=false;
    {$endif}
  end;

PROCEDURE TSettingsForm.memLimitEditEditingDone(Sender: TObject);
  begin
    settings.memoryLimit:=StrToInt64Def(trim(memLimitEdit.text),1) shl 20;
    memoryComfortThreshold:=settings.memoryLimit;
    memLimitEdit.text:=intToStr(settings.memoryLimit shr 20);
  end;

PROCEDURE TSettingsForm.outputSizeLimitEditingDone(Sender: TObject);
  begin
    setOutputLimit(getOutputLimit);
  end;

PROCEDURE TSettingsForm.workerThreadCountEditEditingDone(Sender: TObject);
  VAR newValue:longint;
  begin
    newValue:=strToIntDef(workerThreadCountEdit.text,0);
    if newValue<=0 then workerThreadCountEdit.text:=intToStr(settings.cpuCount)
                   else settings.cpuCount:=newValue;
  end;

PROCEDURE TSettingsForm.AntialiasCheckboxChange(Sender: TObject);
  begin
    settings.antialiasedFonts:=AntialiasCheckbox.checked;
  end;

PROCEDURE TSettingsForm.autosaveComboBoxChange(Sender: TObject);
  begin
    settings.saveIntervalIdx:=autosaveComboBox.ItemIndex;
  end;

FUNCTION TSettingsForm.getFontSize: longint;
  begin
    result := StrToInt64Def(trim(FontSizeEdit.text), 12);
  end;

PROCEDURE TSettingsForm.setFontSize(CONST value: longint);
  begin
    FontSizeEdit.text := intToStr(value);
    EditorFontDialog.Font.size := value;
    settings.fontSize:=value;
  end;

FUNCTION TSettingsForm.getOutputLimit: longint;
  begin
    result := StrToInt64Def(trim(outputSizeLimit.text), maxLongint);
    if result<MINIMUM_OUTPUT_LINES then result:=MINIMUM_OUTPUT_LINES;
  end;

PROCEDURE TSettingsForm.setOutputLimit(CONST value: longint);
  begin
    if value<MINIMUM_OUTPUT_LINES
    then setOutputLimit(MINIMUM_OUTPUT_LINES)
    else begin
      outputSizeLimit.text := intToStr(value);
      settings.outputLinesLimit:=value;
    end;
  end;

FINALIZATION
  if mySettingsForm<>nil then FreeAndNil(mySettingsForm);

end.
