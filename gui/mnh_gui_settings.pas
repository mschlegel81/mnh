UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, mySys,funcs, out_adapters, mnh_constants,
  packages,mnh_settings,mnh_doc,editorMeta;

CONST MINIMUM_OUTPUT_LINES=16;
      PORTABLE_BUTTON_CAPTION:array[false..true] of string=
        ('Convert to normal (non-portable) version',
         'Convert to portable version');
TYPE
  TSettingsForm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    rb_saveNoChange: TRadioButton;
    rb_saveNewDefault: TRadioButton;
    rb_saveDefault: TRadioButton;
    rb_saveNewLinux: TRadioButton;
    rb_saveLinux: TRadioButton;
    rb_saveNewWindows: TRadioButton;
    rb_saveWindows: TRadioButton;
    TabSheet_lineEnding: TTabSheet;
    togglePortableButton: TButton;
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
    PROCEDURE rb_saveDefaultChange(Sender: TObject);
    PROCEDURE rb_saveNewDefaultChange(Sender: TObject);
    PROCEDURE restorePacksAndDemosButtonClick(Sender: TObject);
    PROCEDURE installButtonClick(Sender: TObject);
    PROCEDURE FontButtonClick(Sender: TObject);
    PROCEDURE FontSizeEditEditingDone(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE memLimitEditEditingDone(Sender: TObject);
    PROCEDURE outputSizeLimitEditingDone(Sender: TObject);
    PROCEDURE togglePortableButtonClick(Sender: TObject);
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
    case settings.overwriteLineEnding of
      LINE_ENDING_UNCHANGED: rb_saveNoChange.checked:=true;
      LINE_ENDING_DEFAULT  : rb_saveDefault .checked:=true;
      LINE_ENDING_LINUX    : rb_saveLinux   .checked:=true;
      LINE_ENDING_WINDOWS  : rb_saveWindows .checked:=true;
    end;
    case settings.newFileLineEnding of
      LINE_ENDING_DEFAULT  : rb_saveNewDefault.checked:=true;
      LINE_ENDING_LINUX    : rb_saveNewLinux  .checked:=true;
      LINE_ENDING_WINDOWS  : rb_saveNewWindows.checked:=true;
    end;
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
    ensureDemosAndPackages(nil,nil,true);
  end;

PROCEDURE TSettingsForm.rb_saveNewDefaultChange(Sender: TObject);
  begin
    if rb_saveNewDefault.checked then settings.newFileLineEnding:=LINE_ENDING_DEFAULT;
    if rb_saveNewLinux  .checked then settings.newFileLineEnding:=LINE_ENDING_LINUX  ;
    if rb_saveNewWindows.checked then settings.newFileLineEnding:=LINE_ENDING_WINDOWS;
  end;

PROCEDURE TSettingsForm.rb_saveDefaultChange(Sender: TObject);
  begin
    if rb_saveNoChange.checked then settings.overwriteLineEnding:= LINE_ENDING_UNCHANGED;
    if rb_saveDefault .checked then settings.overwriteLineEnding:= LINE_ENDING_DEFAULT  ;
    if rb_saveLinux   .checked then settings.overwriteLineEnding:= LINE_ENDING_LINUX    ;
    if rb_saveWindows .checked then settings.overwriteLineEnding:= LINE_ENDING_WINDOWS  ;
  end;

PROCEDURE TSettingsForm.uninstallButtonClick(Sender: TObject);
  begin
    sandbox^.runUninstallScript;
    DeleteDirectory(getHtmlRoot    ,false);
    DeleteDirectory(getDemosRoot   ,false);
    DeleteDirectory(getPackagesRoot,false);
    DeleteFile(settingsFileName);
    DeleteFile(workspaceFilename);
    DeleteFile(settings.lightFlavourLocation);
    APP_STYLE:=APP_STYLE_BLANK;
    deleteMyselfOnExit;
    halt;
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
    installButton.visible:=false;
    installButton.enabled:=false;
    uninstallButton.visible:=false;
    uninstallButton.enabled:=false;
    togglePortableButton.visible:=false;
    togglePortableButton.enabled:=false;
    {$else}
    togglePortableButton.caption:=PORTABLE_BUTTON_CAPTION[APP_STYLE=APP_STYLE_NORMAL];
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

PROCEDURE TSettingsForm.togglePortableButtonClick(Sender: TObject);
  {$ifdef Windows}
  VAR sourceFolder:string;
      oldWasNormal:boolean=false;
      foldersToMove:array[0..2,0..1] of string;
      filesToDelete:array[0..1] of string;
      k:longint;
      allOkay:boolean=true;
  begin
    sourceFolder:=configDir;
    foldersToMove[0,0]:=getHtmlRoot;
    foldersToMove[1,0]:=getDemosRoot;
    foldersToMove[2,0]:=getPackagesRoot;
    filesToDelete[0]:=settingsFileName;
    filesToDelete[1]:=workspaceFilename;
    if APP_STYLE=APP_STYLE_NORMAL then begin
      oldWasNormal:=true;
      APP_STYLE:=APP_STYLE_PORTABLE;
    end else begin
      APP_STYLE:=APP_STYLE_NORMAL;
    end;
    foldersToMove[0,1]:=getHtmlRoot;
    foldersToMove[1,1]:=getDemosRoot;
    foldersToMove[2,1]:=getPackagesRoot;
    try
      for k:=0 to 2 do allOkay:=allOkay and CopyDirTree(foldersToMove[k,0],foldersToMove[k,1]);
      for k:=0 to 1 do allOkay:=allOkay and DeleteFile(filesToDelete[k]);
      if oldWasNormal  then allOkay:=allOkay and DeleteDirectory(sourceFolder,false)
      else for k:=0 to 2 do allOkay:=allOkay and DeleteDirectory(foldersToMove[k,0],false);
    except
      allOkay:=false;
    end;
    if not(allOkay) then begin
      if oldWasNormal
      then APP_STYLE:=APP_STYLE_NORMAL
      else APP_STYLE:=APP_STYLE_PORTABLE;
    end;
    saveSettings;
    saveWorkspace;
    ensureDemosAndPackages(nil,nil,false);
    togglePortableButton.caption:=PORTABLE_BUTTON_CAPTION[APP_STYLE=APP_STYLE_NORMAL];
  end;
  {$else}begin end;{$endif}

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
