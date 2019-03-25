UNIT ideMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Dialogs, Menus, ExtCtrls,
  ComCtrls, StdCtrls, ideLayoutUtil, mnh_gui_settings,
  editorMeta,editorMetaBase,evalThread,guiOutAdapters,codeAssistance,
  outputFormUnit,debugging,assistanceFormUnit,debuggerForms,breakpointsForms,searchModel,outlineFormUnit,serializationUtil,mySys,math,customRunDialog,mnh_plotForm,
  helperForms,debuggerVarForms,mnh_settings,quickEvalForms;

TYPE

  { TIdeMainForm }

  TIdeMainForm = class(T_mnhIdeForm)
    bookmarkImages: TImageList;
    breakpointImages: TImageList;
    evaluationStateLabel: TLabel;
    EditLocationLabel: TLabel;
    miUndock2: TMenuItem;
    miUndock3: TMenuItem;
    miUndock4: TMenuItem;
    miUndock1: TMenuItem;
    PlotPositionLabel: TLabel;
    MemoryUsageLabel: TLabel;
    MainMenu: TMainMenu;
    miOutput: TMenuItem;
    miAbout: TMenuItem;
    miHelp: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miKeepStackTrace: TMenuItem;
    miDebug: TMenuItem;
    miProfile: TMenuItem;
    miRunScriptExternally: TMenuItem;
    miRunScript: TMenuItem;
    miRunDirect: TMenuItem;
    miHaltEvaluation: TMenuItem;
    miEditScriptFile: TMenuItem;
    miToggleFullscreen: TMenuItem;
    miDecFontSize: TMenuItem;
    miIncFontSize: TMenuItem;
    miRename: TMenuItem;
    miGotoLine: TMenuItem;
    miFindPrevious: TMenuItem;
    miFindNext: TMenuItem;
    miFind: TMenuItem;
    miReplace: TMenuItem;
    MemoryUsageShape: TShape;
    MemoryUsageFrame: TShape;
    UndockPopup2: TPopupMenu;
    StatusPanel: TPanel;
    smEdit: TMenuItem;
    smRecent: TMenuItem;
    smHistory: TMenuItem;
    miRestore: TMenuItem;
    miExportToHtml: TMenuItem;
    miSaveAs: TMenuItem;
    miSave: TMenuItem;
    miClose: TMenuItem;
    miOpenClassical: TMenuItem;
    miOpen: TMenuItem;
    miBreakpoints: TMenuItem;
    miDebugger: TMenuItem;
    miQuickEval: TMenuItem;
    miOutline: TMenuItem;
    miAssistant: TMenuItem;
    smAppearance: TMenuItem;
    miLanguage: TMenuItem;
    miNew: TMenuItem;
    miSettings: TMenuItem;
    smFile: TMenuItem;
    smShow: TMenuItem;

    PageControl1: TPageControl;
    PageControl2: TPageControl;
    PageControl3: TPageControl;
    EditorsPageControl: TPageControl;
    PageControl4: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    timer: TTimer;
    UndockPopup3: TPopupMenu;
    UndockPopup4: TPopupMenu;
    UndockPopup1: TPopupMenu;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miAboutClick(Sender: TObject);
    PROCEDURE miAssistantClick(Sender: TObject);
    PROCEDURE miBreakpointsClick(Sender: TObject);
    PROCEDURE miCloseClick(Sender: TObject);
    PROCEDURE miDebugClick(Sender: TObject);
    PROCEDURE miDebuggerClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miEditScriptFileClick(Sender: TObject);
    PROCEDURE miExportToHtmlClick(Sender: TObject);
    PROCEDURE miFindClick(Sender: TObject);
    PROCEDURE miFindNextClick(Sender: TObject);
    PROCEDURE miFindPreviousClick(Sender: TObject);
    PROCEDURE miGotoLineClick(Sender: TObject);
    PROCEDURE miHelpClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miKeepStackTraceClick(Sender: TObject);
    PROCEDURE miNewClick(Sender: TObject);
    PROCEDURE miOpenClassicalClick(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miOutlineClick(Sender: TObject);
    PROCEDURE miOutputClick(Sender: TObject);
    PROCEDURE miProfileClick(Sender: TObject);
    PROCEDURE miQuickEvalClick(Sender: TObject);
    PROCEDURE miRenameClick(Sender: TObject);
    PROCEDURE miReplaceClick(Sender: TObject);
    PROCEDURE miRestoreClick(Sender: TObject);
    PROCEDURE miRunDirectClick(Sender: TObject);
    PROCEDURE miRunScriptClick(Sender: TObject);
    PROCEDURE miRunScriptExternallyClick(Sender: TObject);
    PROCEDURE miSaveAsClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miSettingsClick(Sender: TObject);
    PROCEDURE miToggleFullscreenClick(Sender: TObject);
    PROCEDURE miUndock1Click(Sender: TObject);
    PROCEDURE miUndock2Click(Sender: TObject);
    PROCEDURE miUndock3Click(Sender: TObject);
    PROCEDURE miUndock4Click(Sender: TObject);
    PROCEDURE Splitter1Moved(Sender: TObject);
    PROCEDURE attachNewForm(CONST form:T_mnhComponentForm); override;

    PROCEDURE onEditFinished(CONST data:P_editScriptTask   ); override;
    PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); override;
    PROCEDURE onDebuggerEvent;                                override;
    PROCEDURE onEndOfEvaluation;                              override;
    PROCEDURE TimerTimer(Sender: TObject);
  private
    subTimerCounter:longint;
    splitterPositions:T_splitterPositions;
    PROCEDURE startDock(CONST PageControl:TPageControl);
  public
    PROCEDURE saveIdeSettings;
    { public declarations }
  end;

VAR
  IdeMainForm: TIdeMainForm;

IMPLEMENTATION
USES mnh_splash;
{$R ideMain.lfm}

PROCEDURE TIdeMainForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  begin
    editorMeta.workspace.addOrGetEditorMetaForFiles(FileNames,true);
  end;

PROCEDURE TIdeMainForm.FormCreate(Sender: TObject);
  VAR outputForm:TOutputForm;
      stream:T_bufferedInputStreamWrapper;
      activeComponents:T_ideComponentSet;
  begin
    subTimerCounter:=0;
    splitterPositions[1]:=16384;
    splitterPositions[2]:=10000;
    splitterPositions[3]:=16384;
    splitterPositions[4]:=16384;
    ideLayoutUtil.mainForm:=self;

    outputForm:=TOutputForm.create(self);
    attachNewForm(outputForm);

    mnh_plotForm.mainFormCoordinatesLabel:=PlotPositionLabel;

    setupEditorMetaBase(miLanguage);
    runnerModel.create;
    initGuiOutAdapters(self,outputForm.OutputSynEdit);

    initUnit(@guiAdapters);

    workspace.create(self,
                     EditorsPageControl,
                     breakpointImages,
                     bookmarkImages);

    outlineSettings.create;

    splashOnStartup;

    stream.createToReadFromFile(workspaceFilename);

    if stream.allOkay
    and loadMainFormLayout(stream,splitterPositions,activeComponents)
    and loadOutputSettings(stream)
    and workspace.loadFromStream(stream)
    and runnerModel.loadFromStream(stream)
    and outlineSettings.loadFromStream(stream)
    then begin
      if icOutline             in activeComponents then ensureOutlineForm;
      if icHelp                in activeComponents then ensureHelpForm;
      if icAssistance          in activeComponents then ensureAssistanceForm;
      if icQuickEval           in activeComponents then ensureQuickEvalForm;
      if icDebugger            in activeComponents then ensureDebuggerForm;
      if icDebuggerVariables   in activeComponents then ensureDebuggerVarForm;
      if icDebuggerBreakpoints in activeComponents then ensureBreakpointsForm;
      //Apply splitter positions:
      FormResize(self);

      miDebug         .checked:=runnerModel.debugMode;
      miProfile       .checked:=runnerModel.profiling;
      miKeepStackTrace.checked:=runnerModel.stackTracing;

    end;
    stream.destroy;
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    timer.enabled:=false;
    earlyFinalization;
    finalizeCodeAssistance;
    saveIdeSettings;
    workspace.destroy;
  end;

PROCEDURE TIdeMainForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    //TODO: Reject close request if an evaluation is running
  end;

PROCEDURE TIdeMainForm.FormResize(Sender: TObject);
  begin
    PageControl1.width := width*splitterPositions[1] div 65535;
    PageControl2.height:=height*splitterPositions[2] div 65535;
    PageControl3.width := width*splitterPositions[3] div 65535;
    PageControl4.width := width*splitterPositions[4] div 65535;
  end;

PROCEDURE TIdeMainForm.miAboutClick(Sender: TObject);
  begin
    splashForAbout;
  end;

PROCEDURE TIdeMainForm.miAssistantClick(Sender: TObject);
  begin
    ensureAssistanceForm;
  end;

PROCEDURE TIdeMainForm.miBreakpointsClick(Sender: TObject);
  begin
    ensureBreakpointsForm;
  end;

PROCEDURE TIdeMainForm.miCloseClick(Sender: TObject);
  begin
    workspace.closeCurrentFile;
  end;

PROCEDURE TIdeMainForm.miDebugClick(Sender: TObject);
begin
  runnerModel.debugMode:=miDebug.checked;
end;

PROCEDURE TIdeMainForm.miDebuggerClick(Sender: TObject);
  begin
    ensureDebuggerForm;
  end;

PROCEDURE TIdeMainForm.miDecFontSizeClick(Sender: TObject);
  VAR activeType:T_controlType;
  begin
    activeType:=typeOfFocusedControl;
    case activeType of
      ctEditor,ctGeneral,ctTable: SettingsForm.fontSize[activeType]:=SettingsForm.fontSize[activeType]-1;
      ctPlot: plotForm.miDecFontSizeClick(Sender);
    end;
  end;

PROCEDURE TIdeMainForm.miEditScriptFileClick(Sender: TObject);
  begin
    workspace.addOrGetEditorMetaForFiles(utilityScriptFileName,true);
  end;

PROCEDURE TIdeMainForm.miExportToHtmlClick(Sender: TObject);
  begin
    workspace.exportCurrentFileToHtml;
  end;

PROCEDURE TIdeMainForm.miFindClick(Sender: TObject);
  begin
    searchReplaceModel.beginFindOrReplace(focusedEditor,true);
  end;

PROCEDURE TIdeMainForm.miFindNextClick(Sender: TObject);
  begin
    searchReplaceModel.doFindNext(focusedEditor);
  end;

PROCEDURE TIdeMainForm.miFindPreviousClick(Sender: TObject);
  begin
    searchReplaceModel.doFindPrevious(focusedEditor);
  end;

PROCEDURE TIdeMainForm.miGotoLineClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miHelpClick(Sender: TObject);
  begin
    ensureHelpForm;
  end;

PROCEDURE TIdeMainForm.miIncFontSizeClick(Sender: TObject);
  VAR activeType:T_controlType;
  begin
    activeType:=typeOfFocusedControl;
    case activeType of
      ctEditor,ctGeneral,ctTable: SettingsForm.fontSize[activeType]:=SettingsForm.fontSize[activeType]+1;
      ctPlot: plotForm.miIncFontSizeClick(Sender);
    end;
  end;

PROCEDURE TIdeMainForm.miKeepStackTraceClick(Sender: TObject);
  begin
    runnerModel.stackTracing:=miKeepStackTrace.checked;
  end;

PROCEDURE TIdeMainForm.miNewClick(Sender: TObject);
  begin
    workspace.createNewFile;
  end;

PROCEDURE TIdeMainForm.miOpenClassicalClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miOpenClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miOutlineClick(Sender: TObject);
  begin
    ensureOutlineForm;
  end;

PROCEDURE TIdeMainForm.miOutputClick(Sender: TObject);
  begin
    ensureOutputForm;
  end;

PROCEDURE TIdeMainForm.miProfileClick(Sender: TObject);
  begin
    runnerModel.profiling:=miProfile.checked;
  end;

PROCEDURE TIdeMainForm.miQuickEvalClick(Sender: TObject);
  begin
    ensureQuickEvalForm;
  end;

PROCEDURE TIdeMainForm.miRenameClick(Sender: TObject);
  begin
    workspace.renameWordUnderCursor;
  end;

PROCEDURE TIdeMainForm.miReplaceClick(Sender: TObject);
  begin
    searchReplaceModel.beginFindOrReplace(focusedEditor,false);
  end;

PROCEDURE TIdeMainForm.miRestoreClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miRunDirectClick(Sender: TObject);
  begin
    runnerModel.customRun(false);
  end;

PROCEDURE TIdeMainForm.miRunScriptClick(Sender: TObject);
  begin
    if customRunForm(false).ShowModal=mrOk then runnerModel.customRun(true,customRunForm(false).scriptParamEdit.text);
  end;

PROCEDURE TIdeMainForm.miRunScriptExternallyClick(Sender: TObject);
  begin
    if customRunForm(true).ShowModal=mrOk then runnerModel.runExternally(customRunForm(true).scriptParamEdit.text);
  end;

PROCEDURE TIdeMainForm.miSaveAsClick(Sender: TObject);
  begin
    workspace.saveCurrentFile(true);
  end;

PROCEDURE TIdeMainForm.miSaveClick(Sender: TObject);
  begin
    workspace.saveCurrentFile();
  end;

PROCEDURE TIdeMainForm.miSettingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
  end;

PROCEDURE TIdeMainForm.miToggleFullscreenClick(Sender: TObject);
  begin
    if WindowState=wsFullScreen
    then begin
      BorderStyle:=bsSizeable;
      WindowState:=wsMaximized;
    end else begin
      BorderStyle:=bsNone;
      WindowState:=wsFullScreen;
    end;
  end;

PROCEDURE TIdeMainForm.miUndock1Click(Sender: TObject); begin startDock(PageControl1); end;
PROCEDURE TIdeMainForm.miUndock2Click(Sender: TObject); begin startDock(PageControl2); end;
PROCEDURE TIdeMainForm.miUndock3Click(Sender: TObject); begin startDock(PageControl3); end;
PROCEDURE TIdeMainForm.miUndock4Click(Sender: TObject); begin startDock(PageControl4); end;

PROCEDURE TIdeMainForm.Splitter1Moved(Sender: TObject);
  begin
    splitterPositions[1]:=PageControl1.width *65535 div  width;
    splitterPositions[2]:=PageControl2.height*65535 div height;
    splitterPositions[3]:=PageControl3.width *65535 div  width;
    splitterPositions[4]:=PageControl4.width *65535 div  width;
  end;

PROCEDURE TIdeMainForm.startDock(CONST PageControl: TPageControl);
  VAR control:TControl;
      newForm:T_mnhComponentForm;
  begin
    //Only handle pages with one control
    if PageControl.activePage.ControlCount<>1 then exit;
    control:=PageControl.activePage.Controls[0];
    //If the sheet is a TForm return it directly
    if control.ClassType.InheritsFrom(T_mnhComponentForm.ClassType) then newForm:=T_mnhComponentForm(control)
    else begin
      raise Exception.create('Not an mnhComponent form!');
    end;
    newForm.ManualDock(nil);
    newForm.BringToFront;
  end;

PROCEDURE TIdeMainForm.saveIdeSettings;
  VAR stream:T_bufferedOutputStreamWrapper;
  begin
    mnh_settings.saveSettings;

    stream.createToWriteToFile(workspaceFilename);
    saveMainFormLayout(stream,splitterPositions);
    saveOutputSettings(stream);
    workspace.saveToStream(stream);
    runnerModel.saveToStream(stream);
    outlineSettings.saveToStream(stream);
    stream.destroy;
  end;

PROCEDURE TIdeMainForm.attachNewForm(CONST form: T_mnhComponentForm);
  VAR dockMeta:TDragDockObject=nil;
      componentParent:T_componentParent;
  begin
    componentParent:=lastDockLocationFor[form.getIdeComponentType];
    if componentParent in [cpPageControl1..cpPageControl4] then dockMeta:=TDragDockObject.create(form);
    case componentParent of
      cpPageControl1: begin PageControl1.DockDrop(dockMeta,0,0); if PageControl1.width <100 then PageControl1.width :=100; end;
      cpPageControl2: begin PageControl2.DockDrop(dockMeta,0,0); if PageControl2.height<100 then PageControl2.height:=100; end;
      cpPageControl3: begin PageControl3.DockDrop(dockMeta,0,0); if PageControl3.width <100 then PageControl3.width :=100; end;
      cpPageControl4: begin PageControl4.DockDrop(dockMeta,0,0); if PageControl4.width <100 then PageControl4.width :=100; end;
      else begin
        form.top :=top +(height-form.height) div 2;
        form.Left:=Left+(width -form.width ) div 2;
      end;
    end;
    form.myComponentParent:=componentParent;
    form.Show;
    if dockMeta<>nil then FreeAndNil(dockMeta);
  end;

PROCEDURE TIdeMainForm.onEditFinished(CONST data: P_editScriptTask);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.onBreakpoint(CONST data: P_debuggingSnapshot);
  begin
    debuggerForms.currentSnapshot:=data;
    ensureDebuggerForm;
  end;

PROCEDURE TIdeMainForm.onDebuggerEvent;
  begin
    workspace.updateEditorsByGuiStatus;
  end;

PROCEDURE TIdeMainForm.onEndOfEvaluation;
  begin
    workspace.updateEditorsByGuiStatus;
    ensureOutputForm;
    unfreezeOutput;
  end;

PROCEDURE TIdeMainForm.TimerTimer(Sender: TObject);
  PROCEDURE drawMemoryUsage;
    VAR fraction:double;

    begin
      MemoryUsageLabel.caption:=getMemoryUsedAsString(fraction);
      if isNan(fraction) or isInfinite(fraction) or (fraction>1) then fraction:=1;
      if fraction<0 then fraction:=0;

      MemoryUsageShape.width:=round(fraction*MemoryUsageFrame.width);
      MemoryUsageShape.Brush.color:=
        round(255*max(0,min(1,2-2*fraction))) shl 8 or
        round(255*max(0,min(1,  2*fraction)));
    end;

  PROCEDURE slowUpdates; inline;
    VAR edit:P_editorMeta;
    begin
      if workspace.savingRequested then saveIdeSettings;

      edit:=workspace.currentEditor;
      if (edit<>nil) then begin
        edit^.pollAssistanceResult;
      end;
      performSlowUpdates;
      drawMemoryUsage;
    end;

  PROCEDURE fastUpdates; inline;
    begin
      performFastUpdates;
      guiEventsAdapter.flushToGui;
    end;

  begin
    fastUpdates;
    inc(subTimerCounter);
    if subTimerCounter>50 then begin
      slowUpdates;
      subTimerCounter:=0;
    end;
  end;

end.
