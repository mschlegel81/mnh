UNIT idemain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Dialogs, Menus, ExtCtrls,
  ComCtrls, StdCtrls, ideLayoutUtil, mnh_gui_settings,
  editorMeta,editorMetaBase,guiOutAdapters,codeAssistance,
  debugging,assistanceFormUnit,debuggerForms,breakpointsForms,searchModel,outlineFormUnit,serializationUtil,mySys,math,customRunDialog,mnh_plotForm,
  helperForms,debuggerVarForms,mnh_settings,quickEvalForms,openFile,ipcModel,editScripts,litVar,mnh_messages,
  closeDialog, gotoLineDialogs,SynEdit,outputFormUnit,askDialog;

TYPE
  { TIdeMainForm }

  TIdeMainForm = class(T_mnhIdeForm)
    bookmarkImages: TImageList;
    breakpointImages: TImageList;
    evaluationStateLabel: TLabel;
    EditLocationLabel: TLabel;
    FindDialog1: TFindDialog;
    MenuItem1: TMenuItem;
    miFocusEditor: TMenuItem;
    miUndockAll: TMenuItem;
    miDockAll: TMenuItem;
    miDebuggerVar: TMenuItem;
    miClose2: TMenuItem;
    miClose3: TMenuItem;
    miClose4: TMenuItem;
    miClose1: TMenuItem;
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
    UndockPopup2: TPopupMenu;
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
    UndockPopup3: TPopupMenu;
    UndockPopup4: TPopupMenu;
    UndockPopup1: TPopupMenu;
    PROCEDURE EditorsPageControlChange(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miAboutClick(Sender: TObject);
    PROCEDURE miAssistantClick(Sender: TObject);
    PROCEDURE miBreakpointsClick(Sender: TObject);
    PROCEDURE miCloseClick(Sender: TObject);
    PROCEDURE miDebugClick(Sender: TObject);
    PROCEDURE miDebuggerClick(Sender: TObject);
    PROCEDURE miDebuggerVarClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miDockAllClick(Sender: TObject);
    PROCEDURE miEditScriptFileClick(Sender: TObject);
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
    PROCEDURE miUndockAllClick(Sender: TObject);
    PROCEDURE Splitter1Moved(Sender: TObject);
    PROCEDURE attachNewForm(CONST form:T_mnhComponentForm); override;

    PROCEDURE onEditFinished(CONST data:P_storedMessage    ); override;
    PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); override;
    PROCEDURE onDebuggerEvent;                                override;
    PROCEDURE onEndOfEvaluation;                              override;
    PROCEDURE TimerTimer(Sender: TObject);
  private
    fastUpdating,
    slowUpdating,
    quitPosted:boolean;
    subTimerCounter:longint;
  public
    PROCEDURE saveIdeSettings;
    { public declarations }
  end;

VAR
  IdeMainForm: TIdeMainForm;

IMPLEMENTATION
USES mnh_splash,out_adapters,cmdLineInterpretation;
{$R idemain.lfm}

PROCEDURE TIdeMainForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  begin
    if length(FileNames)=0 then exit;
    editorMeta.workspace.addOrGetEditorMetaForFiles(FileNames,true);
  end;

PROCEDURE TIdeMainForm.FormCreate(Sender: TObject);
  VAR stream:T_bufferedInputStreamWrapper;
      activeComponents:T_ideComponentSet;
  begin
    initIpcServer(self);
    new(dockSites[cpNone        ],create(cpNone        ,nil         ,nil      ,nil     ,nil         ));
    new(dockSites[cpPageControl1],create(cpPageControl1,PageControl1,miUndock1,miClose1,UndockPopup1));
    new(dockSites[cpPageControl2],create(cpPageControl2,PageControl2,miUndock2,miClose2,UndockPopup2));
    new(dockSites[cpPageControl3],create(cpPageControl3,PageControl3,miUndock3,miClose3,UndockPopup3));
    new(dockSites[cpPageControl4],create(cpPageControl4,PageControl4,miUndock4,miClose4,UndockPopup4));

    quitPosted:=false;
    slowUpdating:=false;
    fastUpdating:=false;
    subTimerCounter:=999;
    ideLayoutUtil.mainForm:=self;

    initializePlotForm(PlotPositionLabel);

    setupEditorMetaBase(miLanguage);
    runnerModel.create(self,@ensureStdOutAdapter.adapter);
    workspace.create(self,
                     EditorsPageControl,
                     breakpointImages,
                     bookmarkImages,
                     smHistory,
                     smScripts);

    outlineSettings.create;

    splashOnStartup;

    stream.createToReadFromFile(workspaceFilename);

    if stream.allOkay
    and loadMainFormLayout(stream,activeComponents)
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

      workspace.fileHistory.updateHistoryMenu;
    end;
    stream.destroy;
    timer.enabled:=true;
    gui_started:=true;

    runnerModel.ensureEditScripts;

    FormDropFiles(Sender,filesToOpenInEditor);
    searchReplaceModel.create(FindDialog1,ReplaceDialog1);
    {$ifdef LINUX}
    miIncFontSize.ShortCut:=16605;
    {$endif}
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
    timer.enabled:=false;
    finalizeCodeAssistance;
    saveIdeSettings;
    runnerModel.destroy;
    workspace.destroy;
    searchReplaceModel.destroy;
  end;

PROCEDURE TIdeMainForm.EditorsPageControlChange(Sender: TObject);
  VAR meta:P_editorMeta;
  begin
    meta:=workspace.currentEditor;
    if meta<>nil then meta^.activate;
  end;

FUNCTION anyEvaluationRunning:boolean;
  begin
    result:=runnerModel.anyRunning or isQuickEvaluationRunning;
  end;

PROCEDURE TIdeMainForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  VAR mr:integer=mrClose;
  begin
    if anyEvaluationRunning
    then mr:=closeDialogForm.showOnQuitWhileEvaluating;

    if mr<>mrClose then begin
      quitPosted:=(mr=mrOk);
      if anyEvaluationRunning then CanClose:=false;
    end;
  end;

PROCEDURE TIdeMainForm.FormResize(Sender: TObject);
  VAR cp:T_componentParent;
  begin
    for cp in PAGES do dockSites[cp]^.updateAbsSizeByRelSize;
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

PROCEDURE TIdeMainForm.miDebuggerVarClick(Sender: TObject);
  begin
    ensureDebuggerVarForm;
  end;

PROCEDURE TIdeMainForm.miDecFontSizeClick(Sender: TObject);
  VAR activeType:T_controlType;
  begin
    activeType:=typeOfFocusedControl;
    case activeType of
      ctEditor,ctGeneral,ctTable: SettingsForm.fontSize[activeType]:=SettingsForm.fontSize[activeType]-1;
      ctPlot: TplotForm(mainForm.ActiveControl).miDecFontSizeClick(Sender);
    end;
  end;

PROCEDURE TIdeMainForm.miDockAllClick(Sender: TObject);
  VAR cp:T_componentParent;
  begin
    dockAllForms;
    for cp in PAGES do dockSites[cp]^.fixSize;
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
    runnerModel.postHalt;
    stopQuickEvaluation;
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
      ctPlot: TplotForm(mainForm.ActiveControl).miIncFontSizeClick(Sender);
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
    timer.enabled:=false;
    if (openFileDialog.showClassicDialog=mrOk) and fileExists(openFileDialog.getSelectedFile)
    then workspace.addOrGetEditorMetaForFiles(openFileDialog.getSelectedFile,true);
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miOpenClick(Sender: TObject);
  VAR currentEdit:P_editorMeta;
      currentPath:string;
  begin
    timer.enabled:=false;
    currentEdit:=workspace.currentEditor;
    if (currentEdit=nil) or (currentEdit^.isPseudoFile)
    then currentPath:=GetCurrentDir
    else currentPath:=ExtractFileDir(currentEdit^.getPath);
    if (openFileDialog.showForRoot(currentPath)=mrOk) and fileExists(openFileDialog.getSelectedFile)
    then workspace.addOrGetEditorMetaForFiles(openFileDialog.getSelectedFile,true);
    timer.enabled:=true;
  end;

PROCEDURE TIdeMainForm.miOutlineClick(Sender: TObject);
  begin
    ensureOutlineForm;
  end;

PROCEDURE TIdeMainForm.miOutputClick(Sender: TObject);
  begin
    ensureStdOutAdapter;
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
  VAR meta:P_editorMeta;
  begin
    meta:=workspace.currentEditor;
    if meta<>nil then meta^.reloadFile;
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

PROCEDURE TIdeMainForm.miUndockAllClick(Sender: TObject);
  VAR cp:T_componentParent;
  begin
    for cp in PAGES do dockSites[cp]^.undockAll;
    lastDockLocationFor:=C_dockSetupUnDockAll;
  end;

PROCEDURE TIdeMainForm.Splitter1Moved(Sender: TObject);
  VAR cp:T_componentParent;
  begin
    for cp in PAGES do dockSites[cp]^.updateRelSizeByAbsSize;
  end;

PROCEDURE TIdeMainForm.saveIdeSettings;
  VAR stream:T_bufferedOutputStreamWrapper;
  begin
    mnh_settings.saveSettings;

    stream.createToWriteToFile(workspaceFilename);
    saveMainFormLayout(stream);
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
    form.showComponent;
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
    workspace.updateEditorsByGuiStatus;
  end;

PROCEDURE TIdeMainForm.TimerTimer(Sender: TObject);

  PROCEDURE slowUpdates; inline;
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

    VAR edit:P_editorMeta;
    begin
      if slowUpdating then exit;
      slowUpdating:=true;
      if workspace.savingRequested then saveIdeSettings;

      edit:=workspace.currentEditor;
      if (edit<>nil) then begin
        edit^.pollAssistanceResult;
        if edit^.isPseudoFile
        then caption:='MNH'{$ifdef debugMode}+' [debug]'{$endif}
        else caption:='MNH '{$ifdef debugMode}+'[debug] '{$endif}+edit^.pseudoName();
      end else EditLocationLabel.caption:='';

      performSlowUpdates;
      drawMemoryUsage;

      FormDropFiles(Sender,ipcModel.getFilesToOpen);
      evaluationStateLabel.caption:=runnerModel.getStateLabel;
      if quitPosted and not(anyEvaluationRunning) then close;
      workspace.checkForFileChanges;
      slowUpdating:=false;
    end;

  PROCEDURE fastUpdates; inline;
    PROCEDURE enableItems;
      VAR unlocked:boolean;
          canRun:boolean;
      begin
        miHaltEvaluation.enabled:=runnerModel.anyRunning();// or quick.task.processing;
        canRun:=runnerModel.canRun;
        unlocked:=not(runnerModel.areEditorsLocked);
        miRunDirect.enabled:=canRun;
        miRunScript.enabled:=canRun;
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

    begin
      if fastUpdating then exit;
      fastUpdating:=true;
      performFastUpdates;
      runnerModel.flushMessages;

      if ActiveControl.ClassNameIs('TSynEdit') then EditLocationLabel.caption:=caretLabel(TSynEdit(ActiveControl));

      if askForm.displayPending then askForm.Show;
      enableItems;
      fastUpdating:=false;
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

