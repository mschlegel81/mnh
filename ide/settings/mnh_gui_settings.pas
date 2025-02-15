UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, EditBtn,
  mnh_constants,
  mnh_settings,
  packages,
  editorMeta,
  ideLayoutUtil;

CONST MINIMUM_OUTPUT_LINES=16;
      MINIMUM_OUTPUT_LINES_PER_LITERAL=3;
      PORTABLE_BUTTON_CAPTION:array[false..true] of string=
        ('Convert to normal (non-portable) version',
         'Convert to portable version');
TYPE

  { TSettingsForm }

  TSettingsForm = class(TForm)
    buttonApplyAssociations: TButton;
    cbCopyAsHtml: TCheckBox;
    cbForceFullLiteralOutput: TCheckBox;
    CloseButton: TButton;
    fileAssociationGroupBox: TGroupBox;
    Label8: TLabel;
    miSaveBeforeRun: TCheckBox;
    clearFileHistoryButton: TButton;
    outputSizeLimit1: TEdit;
    rbAssociateFull: TRadioButton;
    rbAssociateNone: TRadioButton;
    TableFontButton: TButton;
    GeneralFontButton: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label5: TLabel;
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
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    uninstallButton: TButton;
    outputSizeLimit: TEdit;
    Label7: TLabel;
    memLimitEdit: TEdit;
    Label6: TLabel;
    EditorFontButton: TButton;
    EditorFontDialog: TFontDialog;
    Label2: TLabel;
    PageControl: TPageControl;
    TabSheet_display: TTabSheet;
    TabSheet_install: TTabSheet;
    TabSheet_global: TTabSheet;
    Label1: TLabel;
    workerThreadCountEdit: TEdit;
    Label4: TLabel;
    autosaveComboBox: TComboBox;
    PROCEDURE buttonApplyAssociationsClick(Sender: TObject);
    PROCEDURE cbCopyAsHtmlChange(Sender: TObject);
    PROCEDURE cbForceFullLiteralOutputChange(Sender: TObject);
    PROCEDURE clearFileHistoryButtonClick(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE GeneralFontButtonClick(Sender: TObject);
    PROCEDURE miSaveBeforeRunChange(Sender: TObject);
    PROCEDURE outputSizeLimit1EditingDone(Sender: TObject);
    PROCEDURE PageControlChange(Sender: TObject);
    PROCEDURE rb_saveDefaultChange(Sender: TObject);
    PROCEDURE rb_saveNewDefaultChange(Sender: TObject);
    PROCEDURE restorePacksAndDemosButtonClick(Sender: TObject);
    PROCEDURE EditorFontButtonClick(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE memLimitEditEditingDone(Sender: TObject);
    PROCEDURE outputSizeLimitEditingDone(Sender: TObject);
    PROCEDURE TableFontButtonClick(Sender: TObject);
    PROCEDURE togglePortableButtonClick(Sender: TObject);
    PROCEDURE uninstallButtonClick(Sender: TObject);
    PROCEDURE workerThreadCountEditEditingDone(Sender: TObject);
    PROCEDURE autosaveComboBoxChange(Sender: TObject);
  private
    FUNCTION getOutputLimit(CONST s: string): longint;
    PROCEDURE setOutputLimit (CONST value: longint);
    PROCEDURE setOutputLimit1(CONST value: longint);
  public
    FUNCTION  getFontSize(CONST c:T_controlType): longint;
    PROCEDURE setFontSize(CONST c:T_controlType; CONST value: longint);
    PROPERTY fontSize[c:T_controlType]:longint read getFontSize write setFontSize;
  end;

FUNCTION SettingsForm: TSettingsForm;
IMPLEMENTATION
USES mySys,mnh_doc,closeDialog,codeAssistance;
VAR mySettingsForm: TSettingsForm=nil;
FUNCTION SettingsForm: TSettingsForm;
  begin
    if mySettingsForm=nil then mySettingsForm:=TSettingsForm.create(Application);
    result:=mySettingsForm;
  end;

{$R *.lfm}

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    ideLayoutUtil.getFontSize_callback:=@getFontSize;
    ideLayoutUtil.setFontSize_callback:=@setFontSize;

    EditorFontButton.caption   :=ideSettings.Font[ctEditor].fontName;
    EditorFontButton.Font.name :=ideSettings.Font[ctEditor].fontName;
    EditorFontButton.Font.size :=ideSettings.Font[ctEditor].fontSize;
    EditorFontButton.Font.style:=ideSettings.Font[ctEditor].style;

    TableFontButton.caption   :=ideSettings.Font[ctTable].fontName;
    TableFontButton.Font.name :=ideSettings.Font[ctTable].fontName;
    TableFontButton.Font.size :=ideSettings.Font[ctTable].fontSize;
    TableFontButton.Font.style:=ideSettings.Font[ctTable].style;

    GeneralFontButton.caption   :=ideSettings.Font[ctGeneral].fontName;
    GeneralFontButton.Font.name :=ideSettings.Font[ctGeneral].fontName;
    GeneralFontButton.Font.size :=ideSettings.Font[ctGeneral].fontSize;
    GeneralFontButton.Font.style:=ideSettings.Font[ctGeneral].style;

    propagateFont(EditorFontButton .Font,ctEditor);
    propagateFont(TableFontButton  .Font,ctTable);
    propagateFont(GeneralFontButton.Font,ctGeneral);

    setOutputLimit (ideSettings.outputLinesLimit);
    setOutputLimit1(ideSettings.outputLinesLimitPerLiteral);
    cbForceFullLiteralOutput.checked:=ideSettings.forceFullLiterals;
    workerThreadCountEdit.text:=intToStr(settings.cpuCount);
    memLimitEdit.text:=intToStr(settings.memoryLimit shr 20);

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

    autosaveComboBox.items.clear;
    for i:=0 to length(C_SAVE_INTERVAL)-1 do autosaveComboBox.items.add(C_SAVE_INTERVAL[i].text);
    autosaveComboBox.ItemIndex:=workspace.saveIntervalIdx;
    miSaveBeforeRun.checked:=workspace.autosaveBeforeEachExecution;
    cbCopyAsHtml.checked:=ideSettings.copyTextAsHtml;
  end;

PROCEDURE TSettingsForm.EditorFontButtonClick(Sender: TObject);
  begin
    EditorFontDialog.Font:=EditorFontButton.Font;
    EditorFontDialog.options:=EditorFontDialog.options+[fdFixedPitchOnly];
    if EditorFontDialog.execute then begin
      EditorFontButton.caption   := EditorFontDialog.Font.name;
      EditorFontButton.Font      := EditorFontDialog.Font;
      EditorFontButton.Font.style:= [];
      propagateFont(EditorFontButton.Font,ctEditor);
    end;
  end;

PROCEDURE TSettingsForm.restorePacksAndDemosButtonClick(Sender: TObject);
  begin
    ensureDefaultFiles(nil,true);
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

PROCEDURE TSettingsForm.GeneralFontButtonClick(Sender: TObject);
  begin
    EditorFontDialog.Font:=GeneralFontButton.Font;
    EditorFontDialog.options:=EditorFontDialog.options-[fdFixedPitchOnly];
    if EditorFontDialog.execute then begin
      GeneralFontButton.caption   := EditorFontDialog.Font.name;
      GeneralFontButton.Font      := EditorFontDialog.Font;
      propagateFont(GeneralFontButton.Font,ctGeneral);
    end;
  end;

PROCEDURE TSettingsForm.FormDestroy(Sender: TObject);
  begin
    ideLayoutUtil.setFontSize_callback:=nil;
    ideLayoutUtil.getFontSize_callback:=nil;
  end;

PROCEDURE TSettingsForm.clearFileHistoryButtonClick(Sender: TObject);
  begin
    workspace.fileHistory.clear;
    workspace.fileHistory.fileTouched(getPackagesRoot+'/.mnh',false);
    workspace.fileHistory.fileTouched(getDemosRoot   +'/.mnh',false);
  end;

PROCEDURE TSettingsForm.cbCopyAsHtmlChange(Sender: TObject);
  begin
    ideSettings.copyTextAsHtml:=cbCopyAsHtml.checked;
  end;

PROCEDURE TSettingsForm.cbForceFullLiteralOutputChange(Sender: TObject);
  begin
    ideSettings.forceFullLiterals:=cbForceFullLiteralOutput.checked;
  end;

PROCEDURE TSettingsForm.buttonApplyAssociationsClick(Sender: TObject);
  begin
    if      rbAssociateFull .checked then begin sandbox^.runInstallScript;   ideSettings.registeredAssociation:=raFullVersion;   end
    else if rbAssociateNone .checked then begin sandbox^.runUninstallScript; ideSettings.registeredAssociation:=raNone;         end;
  end;

PROCEDURE TSettingsForm.miSaveBeforeRunChange(Sender: TObject);
  begin
    workspace.autosaveBeforeEachExecution:=miSaveBeforeRun.checked;
  end;

PROCEDURE TSettingsForm.outputSizeLimit1EditingDone(Sender: TObject);
  begin
    setOutputLimit1(getOutputLimit(outputSizeLimit1.text));
  end;

PROCEDURE TSettingsForm.PageControlChange(Sender: TObject);
  VAR c:TSizeConstraints;
  begin
    c:=Constraints;
    c.MinWidth:=width;
    c.MinHeight:=height;
  end;

PROCEDURE TSettingsForm.uninstallButtonClick(Sender: TObject);
  {$ifdef Windows}
  {$include res_toDeleteOnUninstall.inc}
  VAR fileName:string;
  {$endif}
  begin
    if closeDialogForm.showOnUninstall<>cda_continueUninstall then exit;
    sandbox^.runUninstallScript;
    DeleteDirectory(getHtmlRoot    ,false);
    DeleteDirectory(getDemosRoot   ,false);
    DeleteDirectory(getPackagesRoot,false);
    DeleteFile(settingsFileName);
    DeleteFile(ideSettingsFilename);
    DeleteFile(defaultWorkspaceFilename);
    DeleteFile(runParameterHistoryFileName);
    {$ifdef Windows}
    for fileName in ADDITIONAL_FILES_TO_DELETE do DeleteFile(fileName);
    APP_STYLE:=APP_STYLE_BLANK;
    deleteMyselfOnExit;
    {$endif}
    halt;
  end;

PROCEDURE TSettingsForm.FormShow(Sender: TObject);
  begin
    {$ifndef Windows}
    uninstallButton.visible:=false;
    uninstallButton.enabled:=false;
    togglePortableButton.visible:=false;
    togglePortableButton.enabled:=false;
    fileAssociationGroupBox.visible:=false;
    fileAssociationGroupBox.enabled:=false;
    {$else}
    togglePortableButton.caption:=PORTABLE_BUTTON_CAPTION[APP_STYLE=APP_STYLE_NORMAL];
    case ideSettings.registeredAssociation of
      raFullVersion : rbAssociateFull.checked :=true;
      raNone        : rbAssociateNone.checked :=true;
    end;
    {$endif}
  end;

PROCEDURE TSettingsForm.memLimitEditEditingDone(Sender: TObject);
  begin
    settings.memoryLimit:=StrToInt64Def(trim(memLimitEdit.text),1) shl 20;
    memoryCleaner.memoryComfortThreshold:=settings.memoryLimit;
    memLimitEdit.text:=intToStr(settings.memoryLimit shr 20);
  end;

PROCEDURE TSettingsForm.outputSizeLimitEditingDone(Sender: TObject);
  begin
    setOutputLimit(getOutputLimit(outputSizeLimit.text));
  end;

PROCEDURE TSettingsForm.TableFontButtonClick(Sender: TObject);
  begin
    EditorFontDialog.Font:=TableFontButton.Font;
    EditorFontDialog.options:=EditorFontDialog.options-[fdFixedPitchOnly];
    if EditorFontDialog.execute then begin
      TableFontButton.caption   := EditorFontDialog.Font.name;
      TableFontButton.Font      := EditorFontDialog.Font;
      propagateFont(TableFontButton.Font,ctTable);
    end;
  end;

PROCEDURE TSettingsForm.togglePortableButtonClick(Sender: TObject);
  {$ifdef Windows}
  VAR sourceFolder:string;
      oldWasNormal:boolean=false;
      foldersToMove:array[0..2,0..1] of string;
      filesToDelete:array[0..3] of string;
      k:longint;
      allOkay:boolean=true;
  begin
    sourceFolder:=configDir;
    foldersToMove[0,0]:=getHtmlRoot;
    foldersToMove[1,0]:=getDemosRoot;
    foldersToMove[2,0]:=getPackagesRoot;
    filesToDelete[0]:=settingsFileName;
    filesToDelete[1]:=ideSettingsFilename;
    filesToDelete[2]:=defaultWorkspaceFilename;
    filesToDelete[3]:=runParameterHistoryFileName;
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
      for k:=0 to 3 do allOkay:=allOkay and (DeleteFile(filesToDelete[k]) or not(fileExists(filesToDelete[k])));
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
    workspace.postSaveRequest;
    ensureDefaultFiles(nil,true);
    htmlDocGeneratedForCodeHash:='';
    makeHtmlFromTemplate;
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

PROCEDURE TSettingsForm.autosaveComboBoxChange(Sender: TObject);
  begin
    workspace.saveIntervalIdx:=autosaveComboBox.ItemIndex;
  end;

FUNCTION TSettingsForm.getFontSize(CONST c:T_controlType): longint;
  begin
    case c of
      ctEditor : result := EditorFontButton .Font.size;
      ctTable  : result := TableFontButton  .Font.size;
      ctGeneral: result := GeneralFontButton.Font.size;
      else result:=10;
    end;
  end;

PROCEDURE TSettingsForm.setFontSize(CONST c:T_controlType; CONST value: longint);
  begin
    case c of
      ctEditor : begin EditorFontButton .Font.size:=value; propagateFont(EditorFontButton .Font,c); end;
      ctTable  : begin TableFontButton  .Font.size:=value; propagateFont(TableFontButton  .Font,c); end;
      ctGeneral: begin GeneralFontButton.Font.size:=value; propagateFont(GeneralFontButton.Font,c); end;
    end;
  end;

FUNCTION TSettingsForm.getOutputLimit(CONST s:string): longint;
  begin
    result := strToIntDef(trim(s), maxLongint);
    if result<MINIMUM_OUTPUT_LINES then result:=MINIMUM_OUTPUT_LINES;
  end;

PROCEDURE TSettingsForm.setOutputLimit(CONST value: longint);
  begin
    if value<MINIMUM_OUTPUT_LINES
    then setOutputLimit(MINIMUM_OUTPUT_LINES)
    else begin
      outputSizeLimit.text := intToStr(value);
      ideSettings.outputLinesLimit:=value;
    end;
  end;

PROCEDURE TSettingsForm.setOutputLimit1(CONST value: longint);
  begin
    if value<MINIMUM_OUTPUT_LINES_PER_LITERAL
    then setOutputLimit1(MINIMUM_OUTPUT_LINES_PER_LITERAL)
    else begin
      outputSizeLimit1.text := intToStr(value);
      ideSettings.outputLinesLimitPerLiteral:=value;
    end;
  end;

end.
