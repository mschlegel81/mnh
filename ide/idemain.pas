UNIT idemain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Dialogs, Menus, ExtCtrls,
  ComCtrls, StdCtrls, ideLayoutUtil, mnh_gui_settings,
  editorMeta,editorMetaBase,codeAssistance,
  debugging,assistanceFormUnit,debuggerForms,breakpointsForms,searchModel,outlineFormUnit,serializationUtil,mySys,math,customRunDialog,mnh_plotForm,
  helperForms,debuggerVarForms,mnh_settings,quickEvalForms,openFile,ipcModel,editScripts,litVar,mnh_messages,
  closeDialog, gotoLineDialogs,SynEdit,askDialog;

TYPE
  TIdeMainForm = class(T_mnhIdeForm)
    bookmarkImages: TImageList;
    breakpointImages: TImageList;
    evaluationStateLabel: TLabel;
    EditLocationLabel: TLabel;
    FindDialog1: TFindDialog;
    MenuItem1: TMenuItem;
    miEventsView: TMenuItem;
    openRelatedSubmenu: TMenuItem;
    miOpenDependenciesUsed: TMenuItem;
    miOpenDependenciesUsing: TMenuItem;
    miOpenDependenciesBoth: TMenuItem;
    miCopyCurrentFileName: TMenuItem;
    miShebang: TMenuItem;
    miFocusEditor: TMenuItem;
    miUndockAll: TMenuItem;
    miDockAll: TMenuItem;
    miDebuggerVar: TMenuItem;
    PlotPositionLabel: TLabel;
    MemoryUsageLabel: TLabel;
    MainMenu: TMainMenu;
    miOutput: TMenuItem;
    miAbout: TMenuItem;
    miHelp: TMenuItem;
    ReplaceDialog1: TReplaceDialog;
    smScripts: TMenuItem;
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
    StatusPanel: TPanel;
    smEdit: TMenuItem;
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
    PROCEDURE EditorsPageControlChange(Sender: TObject);
    PROCEDURE FormActivate(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
    PROCEDURE FormKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE MemoryUsageShapeMouseUp(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE miAboutClick(Sender: TObject);
    PROCEDURE miAssistantClick(Sender: TObject);
    PROCEDURE miBreakpointsClick(Sender: TObject);
    PROCEDURE miCloseClick(Sender: TObject);
    PROCEDURE miCopyCurrentFileNameClick(Sender: TObject);
    PROCEDURE miDebugClick(Sender: TObject);
    PROCEDURE miDebuggerClick(Sender: TObject);
    PROCEDURE miDebuggerVarClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miDockAllClick(Sender: TObject);
    PROCEDURE miEditScriptFileClick(Sender: TObject);
    PROCEDURE miEventsViewClick(Sender: TObject);
    PROCEDURE miExportToHtmlClick(Sender: TObject);
    PROCEDURE miFindClick(Sender: TObject);
    PROCEDURE miFindNextClick(Sender: TObject);
    PROCEDURE miFindPreviousClick(Sender: TObject);
    PROCEDURE miFocusEditorClick(Sender: TObject);
    PROCEDURE miGotoLineClick(Sender: TObject);
    PROCEDURE miHaltEvaluationClick(Sender: TObject);
    PROCEDURE miHelpClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miKeepStackTraceClick(Sender: TObject);
    PROCEDURE miNewClick(Sender: TObject);
    PROCEDURE miOpenClassicalClick(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miOpenDependenciesBothClick(Sender: TObject);
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
    PROCEDURE miShebangClick(Sender: TObject);
    PROCEDURE miToggleFullscreenClick(Sender: TObject);
    PROCEDURE miUndockAllClick(Sender: TObject);
    PROCEDURE Splitter1Moved(Sender: TObject);
    PROCEDURE attachNewForm(CONST form:T_mnhComponentForm); override;

    PROCEDURE onEditFinished(CONST data:P_storedMessage    ); override;
    PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); override;
    PROCEDURE onDebuggerEvent;                                override;
    PROCEDURE onEndOfEvaluation;                              override;
    PROCEDURE TimerTimer(Sender: TObject);
  private
    firstStart,
    fastUpdating,
    slowUpdating,
    quitPosted:boolean;
    subTimerCounter:longint;
    PROCEDURE ensureTimerSuspend;
  public
    PROCEDURE saveIdeSettings;
    { public declarations }
  end;

VAR
  IdeMainForm: TIdeMainForm;

IMPLEMENTATION
USES mnh_splash,out_adapters,cmdLineInterpretation,shebangDialog,Clipbrd,eventsComponent,LCLType,myStringUtil,mnh_constants;
{$R idemain.lfm}

PROCEDURE TIdeMainForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  begin
    if length(FileNames)=0 then exit;
    editorMeta.workspace.addOrGetEditorMetaForFiles(FileNames,true,true);
    BringToFront;
  end;

PROCEDURE TIdeMainForm.FormKeyPress(Sender: TObject; VAR key: char);
  begin
    if typeOfFocusedControl=ctPlot then TplotForm(mainForm.ActiveControl).FormKeyPress(Sender,key);
  end;

PROCEDURE TIdeMainForm.FormCreate(Sender: TObject);
  begin
    initIpcServer(self);
    setupEventsComponentOnIdeStartup;
    new(dockSites[cpNone        ],create(cpNone        ,nil         ));
    new(dockSites[cpPageControl1],create(cpPageControl1,PageControl1));
    new(dockSites[cpPageControl2],create(cpPageControl2,PageControl2));
    new(dockSites[cpPageControl3],create(cpPageControl3,PageControl3));
    new(dockSites[cpPageControl4],create(cpPageControl4,PageControl4));
    quitPosted:=false;
    slowUpdating:=false;
    fastUpdating:=false;
    subTimerCounter:=-10;
    ideLayoutUtil.mainForm:=self;
    initializePlotForm(PlotPositionLabel);
    setupEditorMetaBase(miLanguage);
    runnerModel.create(self);
    postIdeMessage('Setting up workspace',false);
    workspace.create(self,
                     EditorsPageControl,
                     breakpointImages,
                     bookmarkImages,
                     smHistory,
                     smScripts);
    runParameterHistory.create;

    gui_started:=ide;
    firstStart:=splashOnStartup;
    FormResize(self);
    miDebug         .checked:=runnerModel.debugMode;
    miProfile       .checked:=runnerModel.profiling;
    miKeepStackTrace.checked:=runnerModel.stackTracing;

    {$ifdef debugMode}writeln(stdErr,'Ensuring edit scripts');{$endif}
    runnerModel.ensureEditScripts;

    FormDropFiles(Sender,commandLine.filesToOpenInEditor);
    searchReplaceModel.create(FindDialog1,ReplaceDialog1);
    miFocusEditorClick(Sender);
    threadStartsSleeping; //IDE thread is mainly idle
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.FormDestroy(Sender: TObject);
  begin
    dispose(dockSites[cpNone        ],destroy);
    dispose(dockSites[cpPageControl1],destroy);
    dispose(dockSites[cpPageControl2],destroy);
    dispose(dockSites[cpPageControl3],destroy);
    dispose(dockSites[cpPageControl4],destroy);
  end;

PROCEDURE TIdeMainForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    ensureTimerSuspend;
    saveIdeSettings;
    closeAllForms;
    finalizeCodeAssistance;
    workspace.destroy;
    runnerModel.destroy;
    searchReplaceModel.destroy;
    runParameterHistory.destroy;
  end;

PROCEDURE TIdeMainForm.EditorsPageControlChange(Sender: TObject);
  VAR meta:P_editorMeta;
  begin
    meta:=workspace.currentEditor;
    if meta<>nil then meta^.activate;
  end;

PROCEDURE TIdeMainForm.FormActivate(Sender: TObject);
  PROCEDURE updateShortcuts;
    begin
      COMPONENT_SHORTCUT[icOutline            ]:=shortcutToString(miOutline    .ShortCut);
      COMPONENT_SHORTCUT[icHelp               ]:=shortcutToString(miHelp       .ShortCut);
      COMPONENT_SHORTCUT[icAssistance         ]:=shortcutToString(miAssistant  .ShortCut);
      COMPONENT_SHORTCUT[icQuickEval          ]:=shortcutToString(miQuickEval  .ShortCut);
      COMPONENT_SHORTCUT[icDebugger           ]:=shortcutToString(miDebugger   .ShortCut);
      COMPONENT_SHORTCUT[icDebuggerVariables  ]:=shortcutToString(miDebuggerVar.ShortCut);
      COMPONENT_SHORTCUT[icDebuggerBreakpoints]:=shortcutToString(miBreakpoints.ShortCut);
      COMPONENT_SHORTCUT[icOutput             ]:=shortcutToString(miOutput     .ShortCut);
      COMPONENT_SHORTCUT[icIdeEvents          ]:=shortcutToString(miEventsView .ShortCut);
      miIncFontSize.ShortCut:=VK_OEM_PLUS+scCtrl;
    end;

  VAR meta:P_editorMeta;
  begin
    updateShortcuts;
    with ideSettings do begin
      case windowStateForUpdate of
        wsfuNormal    : begin BorderStyle:=bsSizeable; WindowState:=wsNormal;     end;
        wsfuMaximized : begin BorderStyle:=bsSizeable; WindowState:=wsMaximized;  end;
        wsfuFullscreen: begin BorderStyle:=bsNone;     WindowState:=wsFullScreen; end;
      end;
      windowStateForUpdate:=wsfuNone;
      if activeComponents<>[] then postIdeMessage('Activating IDE',false);
      if icOutline             in activeComponents then ensureOutlineForm;
      if icHelp                in activeComponents then ensureHelpForm;
      if icAssistance          in activeComponents then ensureAssistanceForm;
      if icQuickEval           in activeComponents then ensureQuickEvalForm;
      if icDebugger            in activeComponents then ensureDebuggerForm;
      if icDebuggerVariables   in activeComponents then ensureDebuggerVarForm;
      if icDebuggerBreakpoints in activeComponents then ensureBreakpointsForm;
      if icOutput              in activeComponents then runnerModel.ensureStdOutForm;
      if icIdeEvents           in activeComponents then ensureEventsForm;
      activeComponents:=[];
    end;
    if firstStart then begin
      miSettingsClick(Sender);
      workspace.addOrGetEditorMetaForFiles(configDir+'/demos/helloWorld.mnh',false,false);
    end;
    meta:=workspace.currentEditor;
    if meta<>nil then ActiveControl:=meta^.editor
                 else ActiveControl:=workspace.createNewFile^.editor;
  end;

FUNCTION anyEvaluationRunning:boolean;
  begin
    result:=runnerModel.anyRunning or isQuickEvaluationRunning;
  end;

PROCEDURE TIdeMainForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    if anyEvaluationRunning then begin
      ensureTimerSuspend;
      case closeDialogForm.showOnQuitWhileEvaluating of
        cda_quitAfterEval: begin
          quitPosted:=true;
          if anyEvaluationRunning then CanClose:=false;
        end;
        cda_dontQuit: begin
          quitPosted:=false;
          CanClose:=false;
        end;
        cda_cancelEvalAndQuit: begin
          runnerModel.postHalt;
          stopQuickEvaluation;
          quitPosted:=true;
          CanClose:=not(anyEvaluationRunning);
        end;
      end;
      timer.enabled:=true;
    end;
  end;

PROCEDURE TIdeMainForm.FormResize(Sender: TObject);
  VAR cp:T_componentParent;
  begin
    for cp in PAGES do dockSites[cp]^.updateAbsSizeByRelSize;
  end;

PROCEDURE TIdeMainForm.MemoryUsageShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    memoryCleaner.callCleanupMethods;
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
    ensureTimerSuspend;
    workspace.closeCurrentFile;
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miCopyCurrentFileNameClick(Sender: TObject);
  begin
    if Clipboard=nil then exit;
    Clipboard.AsText:=workspace.currentEditor^.getPath;
  end;

PROCEDURE TIdeMainForm.miDebugClick(Sender: TObject);
  begin
    runnerModel.debugMode:=miDebug.checked;
  end;

PROCEDURE TIdeMainForm.miDebuggerClick(Sender: TObject);
  begin
    ensureDebuggerForm;
  end;

PROCEDURE TIdeMainForm.miDebuggerVarClick(Sender: TObject);
  begin
    ensureDebuggerVarForm;
  end;

PROCEDURE TIdeMainForm.miDecFontSizeClick(Sender: TObject);
  VAR activeType:T_controlType;
  begin
    BeginFormUpdate;
    activeType:=typeOfFocusedControl;
    case activeType of
      ctEditor,ctGeneral,ctTable: SettingsForm.fontSize[activeType]:=SettingsForm.fontSize[activeType]-1;
      ctPlot: TplotForm(mainForm.ActiveControl).miDecFontSizeClick(Sender);
    end;
    EndFormUpdate;
  end;

PROCEDURE TIdeMainForm.miDockAllClick(Sender: TObject);
  VAR cp:T_componentParent;
  begin
    dockAllForms;
    for cp in PAGES do dockSites[cp]^.fixSize;
  end;

PROCEDURE TIdeMainForm.miEditScriptFileClick(Sender: TObject);
  begin
    workspace.addOrGetEditorMetaForFiles(utilityScriptFileName,true,true);
  end;

PROCEDURE TIdeMainForm.miEventsViewClick(Sender: TObject);
  begin
    ensureEventsForm;
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

PROCEDURE TIdeMainForm.miFocusEditorClick(Sender: TObject);
  VAR meta:P_editorMeta;
  begin
    meta:=workspace.currentEditor;
    if meta<>nil then ActiveControl:=meta^.editor;
  end;

PROCEDURE TIdeMainForm.miGotoLineClick(Sender: TObject);
  VAR meta:P_editorMeta;
  begin
    if (ActiveControl.ClassName='TSynEdit')
    then gotoLineByDialog(TSynEdit(ActiveControl))
    else begin
      meta:=workspace.currentEditor;
      if meta<>nil then gotoLineByDialog(meta^.editor);
    end;
  end;

PROCEDURE TIdeMainForm.miHaltEvaluationClick(Sender: TObject);
  begin
    if isQuickEvaluationRunning
    then begin
      postIdeMessage('Halting quick evaluation',false);
      stopQuickEvaluation;
    end else begin
      postIdeMessage('Halting evaluation',false);
      runnerModel.postHalt;
    end;
  end;

PROCEDURE TIdeMainForm.miHelpClick(Sender: TObject);
  begin
    ensureHelpForm;
  end;

PROCEDURE TIdeMainForm.miIncFontSizeClick(Sender: TObject);
  VAR activeType:T_controlType;
  begin
    BeginFormUpdate;
    activeType:=typeOfFocusedControl;
    case activeType of
      ctEditor,ctGeneral,ctTable: SettingsForm.fontSize[activeType]:=SettingsForm.fontSize[activeType]+1;
      ctPlot: TplotForm(mainForm.ActiveControl).miIncFontSizeClick(Sender);
    end;
    EndFormUpdate;
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
  VAR fileName:string;
  begin
    ensureTimerSuspend;
    if (openFileDialog.showClassicDialog=mrOk)
    then for fileName in openFileDialog.getSelectedFile do
      if fileExists(fileName)
      then workspace.addOrGetEditorMetaForFiles(fileName,true,false);
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miOpenClick(Sender: TObject);
  VAR currentEdit:P_editorMeta;
      currentPath:string;
      fileName:string;
  begin
    ensureTimerSuspend;
    currentEdit:=workspace.currentEditor;
    if (currentEdit=nil) or (currentEdit^.isPseudoFile)
    then currentPath:=GetCurrentDir
    else currentPath:=ExtractFileDir(currentEdit^.getPath);
    if (openFileDialog.showForRoot(currentPath)=mrOk)
    then for fileName in openFileDialog.getSelectedFile do
      if fileExists(fileName)
      then workspace.addOrGetEditorMetaForFiles(openFileDialog.getSelectedFile,true,false);
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miOpenDependenciesBothClick(Sender: TObject);
  begin
    if Sender.ClassType=TMenuItem.ClassType
    then workspace.openDependenciesForCurrentScript(TMenuItem(Sender).Tag);
  end;

PROCEDURE TIdeMainForm.miOutlineClick(Sender: TObject);
  begin
    ensureOutlineForm;
  end;

PROCEDURE TIdeMainForm.miOutputClick(Sender: TObject);
  begin
    runnerModel.ensureStdOutForm.showComponent(false);
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
    ensureTimerSuspend;
    workspace.renameWordUnderCursor;
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miReplaceClick(Sender: TObject);
  begin
    ensureTimerSuspend;
    searchReplaceModel.beginFindOrReplace(focusedEditor,false);
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miRestoreClick(Sender: TObject);
  VAR meta:P_editorMeta;
  begin
    meta:=workspace.currentEditor;
    if meta<>nil then meta^.reloadFile;
  end;

PROCEDURE TIdeMainForm.miRunDirectClick(Sender: TObject);
  begin
    if runnerModel.canRunMain(true) then runnerModel.customRun(false);
  end;

PROCEDURE TIdeMainForm.miRunScriptClick(Sender: TObject);
  begin
    ensureTimerSuspend;
    if runnerModel.canRunMain(true) and showCustomRunForm(false) then runnerModel.customRun(true);
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miRunScriptExternallyClick(Sender: TObject);
  begin
    ensureTimerSuspend;
    if showCustomRunForm(true) then runnerModel.runExternally();
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miSaveAsClick(Sender: TObject);
  begin
    ensureTimerSuspend;
    workspace.saveCurrentFile(true);
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miSaveClick(Sender: TObject);
  begin
    ensureTimerSuspend;
    workspace.saveCurrentFile();
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miSettingsClick(Sender: TObject);
  begin
    ensureTimerSuspend;
    SettingsForm.ShowModal;
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miShebangClick(Sender: TObject);
  begin
    ensureTimerSuspend;
    showShebangWizard(workspace.currentEditor);
    timer.enabled:=true;
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

PROCEDURE TIdeMainForm.miUndockAllClick(Sender: TObject);
  VAR cp:T_componentParent;
  begin
    for cp in PAGES do dockSites[cp]^.undockAll;
  end;

PROCEDURE TIdeMainForm.Splitter1Moved(Sender: TObject);
  VAR cp:T_componentParent;
  begin
    for cp in PAGES do dockSites[cp]^.updateRelSizeByAbsSize;
  end;

PROCEDURE TIdeMainForm.saveIdeSettings;
  begin
    mnh_settings.saveSettings;
    runParameterHistory.saveToFile(runParameterHistoryFileName);
    workspace          .saveToFile(ideSettings.workspaceFilename);
    ideSettings        .saveToFile(ideSettingsFilename);
    settings           .saveToFile(settingsFileName);
  end;

PROCEDURE TIdeMainForm.attachNewForm(CONST form: T_mnhComponentForm);
  VAR dockMeta:TDragDockObject=nil;
      componentParent:T_componentParent;
  begin
    componentParent:=form.lastDock;
    if componentParent in [cpPageControl1..cpPageControl4] then dockMeta:=TDragDockObject.create(form);
    case componentParent of
      cpPageControl1: PageControl1.DockDrop(dockMeta,0,0);
      cpPageControl2: PageControl2.DockDrop(dockMeta,0,0);
      cpPageControl3: PageControl3.DockDrop(dockMeta,0,0);
      cpPageControl4: PageControl4.DockDrop(dockMeta,0,0);
      else begin
        form.top :=top +(height-form.height) div 2;
        form.Left:=Left+(width -form.width ) div 2;
      end;
    end;
    dockSites[componentParent]^.fixSize;
    form.myComponentParent:=componentParent;
    if componentParent<>cpNone then form.lastDock:=componentParent;
    form.showComponent(false);
    if dockMeta<>nil then FreeAndNil(dockMeta);
  end;

PROCEDURE TIdeMainForm.onEditFinished(CONST data: P_storedMessage);
  begin
    workspace.processEditScriptMessage(data);
  end;

PROCEDURE TIdeMainForm.onBreakpoint(CONST data: P_debuggingSnapshot);
  begin
    ensureDebuggerForm(data);
  end;

PROCEDURE TIdeMainForm.onDebuggerEvent;
  begin
    workspace.updateEditorsByGuiStatus;
    miDebug.checked:=runnerModel.debugMode;
  end;

PROCEDURE TIdeMainForm.onEndOfEvaluation;
  begin
    if currentSnapshot<>nil then disposeMessage(currentSnapshot);
    workspace.clearDebugLine;
    workspace.updateEditorsByGuiStatus;
  end;

PROCEDURE TIdeMainForm.TimerTimer(Sender: TObject);

  PROCEDURE slowUpdates;
    PROCEDURE drawMemoryUsage;
      VAR fraction:double;

      begin
        MemoryUsageLabel.caption:=memoryCleaner.getMemoryUsedAsString(fraction);
        if isNan(fraction) or isInfinite(fraction) or (fraction>1) then fraction:=1;
        if fraction<0 then fraction:=0;

        MemoryUsageShape.width:=round(fraction*MemoryUsageFrame.width);
        MemoryUsageShape.Brush.color:=
          round(255*max(0,min(1,2-2*fraction))) shl 8 or
          round(255*max(0,min(1,  2*fraction)));
      end;

    begin
      if slowUpdating then exit;
      try
        slowUpdating:=true;
        if workspace.savingRequested then begin
          postIdeMessage('Saving settings',false);
          saveIdeSettings;
        end;
        performSlowUpdates(runnerModel.anyRunning(false));
        drawMemoryUsage;

        FormDropFiles(Sender,ipcModel.getFilesToOpen);
        evaluationStateLabel.caption:=runnerModel.getStateLabel+BoolToStr(quitPosted,' QUIT POSTED','');
        if quitPosted and not(anyEvaluationRunning) then begin
          OnCloseQuery:=nil;
          slowUpdating:=false;
          close;
        end;
      finally
        slowUpdating:=false;
      end;
    end;

  PROCEDURE fastUpdates;
    PROCEDURE enableItems;
      VAR unlocked:boolean;
      begin
        miHaltEvaluation.enabled:=runnerModel.anyRunning();
        unlocked:=not(runnerModel.areEditorsLocked);
        miSave     .enabled:=unlocked;
        miSaveAs   .enabled:=unlocked;
        miRestore  .enabled:=unlocked;
        smScripts  .enabled:=unlocked;
        miReplace  .enabled:=unlocked;
      end;

    FUNCTION caretLabel(edit:TSynEdit):string;
      begin
        result:=intToStr(edit.CaretY)+','+intToStr(edit.CaretX);
      end;

    VAR edit:P_editorMeta;
    begin
      if fastUpdating then exit;
      try
        fastUpdating:=true;
        performFastUpdates;
        runnerModel.flushMessages;

        edit:=workspace.currentEditor;
        if (edit<>nil) then begin
          openRelatedSubmenu.enabled:=(edit^.language=LANG_MNH);
          workspace.processPendingMessagesInMainThread;
          if edit^.isPseudoFile
          then caption:='MNH'{$ifdef debugMode}+' [debug]'{$endif}
          else caption:='MNH '{$ifdef debugMode}+'[debug] '{$endif}+edit^.pseudoName();
        end else begin
          caption:='MNH'{$ifdef debugMode}+' [debug]'{$endif};
          openRelatedSubmenu.enabled:=false;
        end;

        if Assigned(ActiveControl) and ActiveControl.ClassNameIs('TSynEdit') then EditLocationLabel.caption:=caretLabel(TSynEdit(ActiveControl));

        if askForm.displayPending then askForm.Show;
        enableItems;
      finally
        fastUpdating:=false;
      end;
    end;

  begin
    inc(subTimerCounter);
    if subTimerCounter>0 then fastUpdates;
    if subTimerCounter>50 then begin
      slowUpdates;
      subTimerCounter:=0;
    end;
  end;

PROCEDURE TIdeMainForm.ensureTimerSuspend;
  VAR counter:longint=0;
  begin
    {$ifdef debugMode}
    writeln('TIdeMainForm.ensureTimerSuspend...');
    {$endif}
    if timer.enabled then begin
      timer.enabled:=false;
      while (slowUpdating or fastUpdating) and (counter<1000) do begin
        inc(counter);
        sleep(1);
      end;
    end;
    {$ifdef debugMode}
    writeln('TIdeMainForm.ensureTimerSuspend - timer suspended');
    {$endif}
  end;

end.

