{$ifdef includeInterface}
PROCEDURE EditorPopupMenuPopup(Sender: TObject);
PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
PROCEDURE FormCreate(Sender: TObject);
PROCEDURE FormDestroy(Sender: TObject);
PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE FormResize(Sender: TObject);
PROCEDURE FormShow(Sender: TObject);
PROCEDURE InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE InputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE InputEditProcessUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
PROCEDURE InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
PROCEDURE MenuItem4Click(Sender: TObject);
PROCEDURE OutputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE OutputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE OutputEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE Splitter1Moved(Sender: TObject);
PROCEDURE Splitter3Moved(Sender: TObject);
PROCEDURE SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
PROCEDURE SynCompletionExecute(Sender: TObject);
PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);
PROCEDURE UpdateTimeTimerTimer(Sender: TObject);
PROCEDURE assistanceSynEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE assistanceSynEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE assistanceSynEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE callStackListSelectionChange(Sender: TObject; User: boolean);
PROCEDURE inputPageControlChange(Sender: TObject);
PROCEDURE miAboutClick(Sender: TObject);
PROCEDURE miClearClick(Sender: TObject);
PROCEDURE miCloseAllButCurrentClick(Sender: TObject);
PROCEDURE miCloseAllUnmodifiedClick(Sender: TObject);
PROCEDURE miCloseClick(Sender: TObject);
PROCEDURE miDebugClick(Sender: TObject);
PROCEDURE miDecFontSizeClick(Sender: TObject);
PROCEDURE miDeclarationEchoClick(Sender: TObject);
PROCEDURE miEditGuiScriptsClick(Sender: TObject);
PROCEDURE miEvaluateNowClick(Sender: TObject);
PROCEDURE miExpressionEchoClick(Sender: TObject);
PROCEDURE miExpressionResultClick(Sender: TObject);
PROCEDURE miFileHistory0Click(Sender: TObject);
PROCEDURE miFindClick(Sender: TObject);
PROCEDURE miReplaceClick(Sender: TObject);
PROCEDURE miFindNextClick(Sender: TObject);
PROCEDURE miFindPreviousClick(Sender: TObject);
PROCEDURE miFullscreenClick(Sender: TObject);
PROCEDURE miGotoClick(Sender: TObject);
PROCEDURE miHaltEvalutaionClick(Sender: TObject);
PROCEDURE miHelpClick(Sender: TObject);
PROCEDURE miHelpExternallyClick(Sender: TObject);
PROCEDURE miHtmlExportClick(Sender: TObject);
PROCEDURE miIncFontSizeClick(Sender: TObject);
PROCEDURE miLangMnhClick(Sender: TObject);
PROCEDURE miMinErrorlevel1Click(Sender: TObject);
PROCEDURE miMinErrorlevel2Click(Sender: TObject);
PROCEDURE miMinErrorlevel3Click(Sender: TObject);
PROCEDURE miMinErrorlevel4Click(Sender: TObject);
PROCEDURE miNewCentralPackageClick(Sender: TObject);
PROCEDURE miOpenClick(Sender: TObject);
PROCEDURE miOpenDemoClick(Sender: TObject);
PROCEDURE miProfileClick(Sender: TObject);
PROCEDURE miReloadClick(Sender: TObject);
PROCEDURE miRunCustomUtilScript(Sender: TObject);
PROCEDURE miSaveAsClick(Sender: TObject);
PROCEDURE miSaveClick(Sender: TObject);
PROCEDURE miTimingInfoClick(Sender: TObject);
PROCEDURE miUtilityScriptRootClick(Sender: TObject);
PROCEDURE miWrapEchoClick(Sender: TObject);
PROCEDURE mi_settingsClick(Sender: TObject);
PROCEDURE pmiOpenFile(CONST idOrName:string);
PROCEDURE tbMicroStepClick(Sender: TObject);
PROCEDURE tbRunClick(Sender: TObject);
PROCEDURE tbStepClick(Sender: TObject);
PROCEDURE tbStepInClick(Sender: TObject);
PROCEDURE tbStepOutClick(Sender: TObject);
PROCEDURE tbStopClick(Sender: TObject);
PROCEDURE variablesTreeViewExpanding(Sender: TObject; node: TTreeNode; VAR AllowExpansion: boolean);
{$endif}
{$ifdef includeImplementation}
PROCEDURE TMnhForm.EditorPopupMenuPopup(Sender: TObject);
  begin
    popupModel.setFiles(focusedEditor.GetWordAtRowCol(focusedEditor.CaretXY),
                        focusedEditor.TextBetweenPoints[focusedEditor.BlockBegin,focusedEditor.BlockEnd]);
  end;

PROCEDURE TMnhForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    if runEvaluator      .evaluationRunning then runEvaluator      .haltEvaluation;
    if assistancEvaluator.evaluationRunning then assistancEvaluator.haltEvaluation;
    storeEditorsToSettings;
  end;

PROCEDURE TMnhForm.FormCreate(Sender: TObject);


  PROCEDURE updateRules;
    begin
      reregisterRule(SYSTEM_BUILTIN_NAMESPACE,'ask',@ask_impl);
      registerRule(GUI_NAMESPACE,'editors'       ,@editors_impl      ,false,ak_nullary,'editors(...);//Lists all editors');
      registerRule(GUI_NAMESPACE,'editorContent' ,@editorContent_impl,false,ak_unary,'editorContent(name:string);//Returns the content of the given editor as a string or void if no such editor was found.');
      registerRule(GUI_NAMESPACE,'openInEditor'  ,@openInEditor_impl ,false,ak_unary,'openInEditor(filename:string);//opens an editor tab for the given file');
    end;

  begin
    registerForm(self,true,true);
    updateRules; //rules must be updated before splash for correct documentation
    splashOnStartup;
    //Dynamic GUI components:
    setLength(scriptMenuItems[st_edit],0);
    setLength(scriptMenuItems[st_insert],0);
    setLength(scriptMenuItems[st_util],0);
    setLength(historyMenuItems,0);
    outputHighlighter:=TSynMnhSyn.create(self,msf_output);
    helpHighlighter  :=TSynMnhSyn.create(self,msf_help  );
    debugHighlighter :=TSynMnhSyn.create(self,msf_input );
    OutputEdit           .highlighter:=outputHighlighter;
    assistanceSynEdit    .highlighter:=outputHighlighter;
    helpPopupMemo        .highlighter:=helpHighlighter  ;
    currentExpressionMemo.highlighter:=debugHighlighter ;
    OutputEdit       .clearAll;
    assistanceSynEdit.clearAll;
    //Initialize state:
    popupModel.create(pmiOpenFile1,pmiOpenFile2,@pmiOpenFile);
    searchReplaceModel.create(FindDialog,ReplaceDialog);
    //lastWordsCaret:=maxLongint;
    //wordsInEditor.create;
    //forceInputEditFocusOnOutputEditMouseUp:=false;
    //doNotCheckFileBefore:=now+ONE_SECOND;
    initIpcServer(self);
    //Initialize other units
    setupCallbacks;

    initGuiOutAdapters(MnhForm,true);
    {$ifdef debugMode}
    if wantConsoleAdapter then guiAdapters.addConsoleOutAdapter^.enableMessageType(false,[mt_clearConsole]);
    {$endif}

    mnh_evalThread.initUnit(@guiAdapters,true);
    runEvaluator.ensureEditScripts;
    SynHighlighterMnh.initLists;
    guiTaskQueue.create;
  end;

PROCEDURE TMnhForm.FormDestroy(Sender: TObject);
  begin
    //Suppress timer-events:
    UpdateTimeTimer.enabled:=false;
    //Finalze other units
    mnh_evalThread.earlyFinalization;
    saveSettings;
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    popupModel.destroy;
    searchReplaceModel.destroy;
    //wordsInEditor.destroy;
    guiTaskQueue.destroy;
  end;

PROCEDURE TMnhForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  begin
    inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(FileNames,true);
  end;

PROCEDURE TMnhForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(self,ssShift in Shift);
  end;

VAR firstShow:boolean=true;
PROCEDURE TMnhForm.FormResize(Sender: TObject);
  begin
    if not(firstShow) then updateFormPositionInSettings;
    //if helpPopupMemo.visible then positionHelpNotifier;
  end;


PROCEDURE TMnhForm.FormShow(Sender: TObject);
  begin
    if firstShow then begin
      initializeSettings;
      editorMeta.setupUnit(self,
                           inputPageControl,
                           EditorPopupMenu,
                           SaveDialog,
                           breakpointsImagesList,
                           assistanceSynEdit,
                           SynCompletion,
                           outputHighlighter,
                           @InputEditKeyDown,
                           @InputEditMouseDown,
                           @InputEditProcessUserCommand,
                           @InputEditSpecialLineMarkup);
      if hasEditor then getEditor^.activate;
      firstShow:=false;
    end;
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  VAR help,jump,mark:boolean;
  begin
    help:=helpPopupMemo.visible;
    jump:=(key=13) and (ssCtrl in Shift);
    mark:=(key=13) and (ssAlt  in Shift);
    getEditor^.setUnderCursor(mark,help or jump);
    if jump then openLocation(getHelpLocation);
    if help then positionHelpNotifier;
  end;

PROCEDURE TMnhForm.InputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  CONST mark=true;
  VAR help,jump:boolean;
      point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    point:=getEditor^.editor.PixelsToRowColumn(point);
    help:=helpPopupMemo.visible;
    jump:=(ssCtrl in Shift);
    getEditor^.setUnderCursor(mark,help or jump,point);
    if jump then openLocation(getHelpLocation);
    if help then positionHelpNotifier;
  end;

PROCEDURE TMnhForm.InputEditProcessUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  begin
    if command=ecUserDefinedFirst   then getEditor^.toggleComment;
    if command=ecUserDefinedFirst+1 then cycleEditors(true);
    if command=ecUserDefinedFirst+2 then cycleEditors(false);
    if command=ecUserDefinedFirst+3 then begin
      getEditor^.toggleBreakpoint;
      debugMode:=true;
    end;
  end;

PROCEDURE TMnhForm.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
//    Special:=runEvaluator.context.isPaused and runEvaluator.evaluationRunning and (Sender=debugLine.editor) and (line=debugLine.line);
  end;

PROCEDURE TMnhForm.MenuItem4Click(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.OutputEditKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin end;
PROCEDURE TMnhForm.OutputEditMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin end;
PROCEDURE TMnhForm.OutputEditMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin end;
PROCEDURE TMnhForm.Splitter1Moved(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.Splitter3Moved(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.SynCompletionCodeCompletion(VAR value: string;
  sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
  begin end;
PROCEDURE TMnhForm.SynCompletionExecute(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.SynCompletionSearchPosition(VAR APosition: integer);
  begin end;
PROCEDURE TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.assistanceSynEditKeyUp(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin end;
PROCEDURE TMnhForm.assistanceSynEditMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin end;
PROCEDURE TMnhForm.assistanceSynEditMouseUp(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin end;
PROCEDURE TMnhForm.callStackListSelectionChange(Sender: TObject; User: boolean);
  begin end;
PROCEDURE TMnhForm.inputPageControlChange(Sender: TObject);
  begin
    if hasEditor then getEditor^.activate;
  end;

PROCEDURE TMnhForm.miAboutClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miCloseAllButCurrentClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miCloseAllUnmodifiedClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miCloseClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miDebugClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miDeclarationEchoClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miEditGuiScriptsClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miExpressionEchoClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miExpressionResultClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miFileHistory0Click(Sender: TObject);
  begin end;

PROCEDURE TMnhForm.miFindClick        (Sender: TObject); begin searchReplaceModel.beginFindOrReplace(focusedEditor,true);  end;
PROCEDURE TMnhForm.miReplaceClick     (Sender: TObject); begin searchReplaceModel.beginFindOrReplace(focusedEditor,false); end;
PROCEDURE TMnhForm.miFindNextClick    (Sender: TObject); begin searchReplaceModel.doFindNext        (focusedEditor);       end;
PROCEDURE TMnhForm.miFindPreviousClick(Sender: TObject); begin searchReplaceModel.doFindPrevious    (focusedEditor);       end;

PROCEDURE TMnhForm.miFullscreenClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miGotoClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miHelpClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miHelpExternallyClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miHtmlExportClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miLangMnhClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miMinErrorlevel1Click(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miMinErrorlevel2Click(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miMinErrorlevel3Click(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miMinErrorlevel4Click(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miNewCentralPackageClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miOpenClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miOpenDemoClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miProfileClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miReloadClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miRunCustomUtilScript(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miTimingInfoClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miUtilityScriptRootClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.miWrapEchoClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.mi_settingsClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.pmiOpenFile(CONST idOrName: string);
  VAR fileName:string;
  begin
    if fileExists(idOrName)
    then begin
      inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(idOrName,true);
      exit;
    end;
    fileName:=assistancEvaluator.resolveImport(idOrName);
    if (fileName<>'') and fileExists(fileName) then inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(fileName,true);
  end;

PROCEDURE TMnhForm.tbMicroStepClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.tbRunClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.tbStepClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.tbStepInClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.tbStepOutClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.tbStopClick(Sender: TObject);
  begin end;
PROCEDURE TMnhForm.variablesTreeViewExpanding(Sender: TObject; node: TTreeNode;
  VAR AllowExpansion: boolean);
  begin end;
{$endif}
