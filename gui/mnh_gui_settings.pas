UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, mnh_funcs, mnh_out_adapters, mnh_constants,
  mnh_packages,mnh_settings;

CONST MINIMUM_OUTPUT_LINES=16;
TYPE

  { TSettingsForm }

  TSettingsForm = class(TForm)
    installButton: TButton;
    uninstallButton: TButton;
    outputSizeLimit: TEdit;
    Label7: TLabel;
    memLimitEdit: TEdit;
    Label6: TLabel;
    logNameEdit: TEdit;
    FontButton: TButton;
    AntialiasCheckbox: TCheckBox;
    FontSizeEdit: TEdit;
    EditorFontDialog: TFontDialog;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    PageControl: TPageControl;
    rbLogOff: TRadioButton;
    rbLogPerProgramStart: TRadioButton;
    rbLogPerRun: TRadioButton;
    TabSheet_display: TTabSheet;
    Label8: TLabel;
    TabSheet_install: TTabSheet;
    Button1: TButton;
    TabSheet_global: TTabSheet;
    Label1: TLabel;
    workerThreadCountEdit: TEdit;
    Label4: TLabel;
    autosaveComboBox: TComboBox;
    PROCEDURE installButtonClick(Sender: TObject);
    PROCEDURE FontButtonClick(Sender: TObject);
    PROCEDURE FontSizeEditEditingDone(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE Button1Click(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE logNameEditChange(Sender: TObject);
    PROCEDURE memLimitEditEditingDone(Sender: TObject);
    PROCEDURE outputSizeLimitEditingDone(Sender: TObject);
    PROCEDURE rbLogOffChange(Sender: TObject);
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

VAR
  SettingsForm: TSettingsForm;

IMPLEMENTATION

{$R *.lfm}

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    if settings.value^.editorFontname<>'' then begin
      EditorFontDialog.Font.name := settings.value^.editorFontname;
      FontButton.Font.name := settings.value^.editorFontname;
    end;
    AntialiasCheckbox.Checked := settings.value^.antialiasedFonts;
    setFontSize(settings.value^.fontSize);
    setOutputLimit(settings.value^.outputLinesLimit);
    if not(settings.value^.wasLoaded) then settings.value^.activePage:=0;
    workerThreadCountEdit.text:=intToStr(settings.value^.cpuCount);
    memLimitEdit.text:=intToStr(settings.value^.memoryLimit shr 20);
    FontButton.Font.size := getFontSize;
    FontButton.caption := settings.value^.editorFontname;
    with settings.value^.mainForm do begin
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
    settings.value^.polishHistory;
    logNameEdit.text:=settings.value^.textLogName;
    if logNameEdit.text=''            then rbLogOff.Checked:=true
    else if settings.value^.logPerRun then rbLogPerRun.Checked:=true
                                      else rbLogPerProgramStart.Checked:=true;

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
      FontButton.caption := settings.value^.editorFontname;
    end;
  end;

PROCEDURE TSettingsForm.FontSizeEditEditingDone(Sender: TObject);
  begin
    setFontSize(getFontSize);
  end;

PROCEDURE TSettingsForm.installButtonClick(Sender: TObject);
  {$ifdef imig}
  {$i res_ensureAssoc_imig.inc}
  {$else}
  {$i res_ensureAssoc.inc}
  {$endif}
  begin
    {$ifdef imig}
    runAlone(ensureAssoc_imig_mnh);
    {$else}
    runAlone(ensureAssoc_mnh);
    {$endif}
  end;

PROCEDURE TSettingsForm.uninstallButtonClick(Sender: TObject);
  {$ifdef imig}
  {$i res_removeAssoc_imig.inc}
  {$else}
  {$i res_removeAssoc.inc}
  {$endif}
  begin
    {$ifdef imig}
    runAlone(removeAssoc_imig_mnh);
    {$else}
    runAlone(removeAssoc_mnh);
    {$endif}
  end;

PROCEDURE TSettingsForm.ensureFont(CONST editorFont:TFont);
  begin
    if settings.value^.editorFontname<>'' then exit;
    settings.value^.editorFontname:=editorFont.name;
    EditorFontDialog.Font.name := settings.value^.editorFontname;
    FontButton.Font.name := settings.value^.editorFontname;
    setFontSize(editorFont.size);
    FontButton.Font.size := getFontSize;
    FontButton.caption := settings.value^.editorFontname;
  end;

PROCEDURE TSettingsForm.Button1Click(Sender: TObject);
  {$i res_ensureNppHighlighting.inc}
  begin
    runAlone(ensureNotepad__Highlighting_mnh);
  end;

PROCEDURE TSettingsForm.FormShow(Sender: TObject);
  begin
    {$ifndef Windows}
    TabSheet_install.visible:=false;
    TabSheet_install.enabled:=false;
    TabSheet_install.tabVisible:=false;
    {$endif}
  end;

PROCEDURE TSettingsForm.logNameEditChange(Sender: TObject);
  begin
    settings.value^.textLogName:=logNameEdit.text;
    if settings.value^.getLogName='' then rbLogOff.Checked:=true
    else if settings.value^.logPerRun then rbLogPerRun.Checked:=true
                                      else rbLogPerProgramStart.Checked:=true;
  end;

PROCEDURE TSettingsForm.memLimitEditEditingDone(Sender: TObject);
  begin
    settings.value^.memoryLimit:=StrToInt64Def(trim(memLimitEdit.text),1) shl 20;
    memLimitEdit.text:=intToStr(settings.value^.memoryLimit shr 20);
  end;

PROCEDURE TSettingsForm.outputSizeLimitEditingDone(Sender: TObject);
  begin
    setOutputLimit(getOutputLimit);
  end;

PROCEDURE TSettingsForm.rbLogOffChange(Sender: TObject);
  begin
    if rbLogOff.Checked then begin
      logNameEdit.text:='';
      settings.value^.textLogName:='';
    end else begin
      settings.value^.logPerRun:=rbLogPerRun.Checked;
      if not(rbLogOff.Checked) and (settings.value^.getLogName='') then rbLogOff.Checked:=true;
    end;
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
      settings.value^.outputLinesLimit:=value;
    end;
  end;

end.
