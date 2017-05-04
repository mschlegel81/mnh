{$ifdef includeInterface}
PROCEDURE EditorPopupMenuPopup(Sender: TObject);

PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
PROCEDURE FormCreate(Sender: TObject);
PROCEDURE FormDestroy(Sender: TObject);
PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE FormResize(Sender: TObject);
PROCEDURE FormShow(Sender: TObject);
//Jump to declaration:
PROCEDURE InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE InputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE OutputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE OutputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE OutputEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE assistanceSynEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE assistanceSynEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE assistanceSynEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);

PROCEDURE InputEditProcessUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
//Search/Replace
PROCEDURE miFindClick(Sender: TObject);
PROCEDURE miReplaceClick(Sender: TObject);
PROCEDURE miFindNextClick(Sender: TObject);
PROCEDURE miFindPreviousClick(Sender: TObject);

//Running
PROCEDURE miCallMainClick(Sender: TObject);
PROCEDURE miEvaluateNowClick(Sender: TObject);
PROCEDURE miDebugClick(Sender: TObject);
PROCEDURE miHaltEvalutaionClick(Sender: TObject);
PROCEDURE tbMicroStepClick(Sender: TObject);
PROCEDURE tbRunClick(Sender: TObject);
PROCEDURE tbStepClick(Sender: TObject);
PROCEDURE tbStepInClick(Sender: TObject);
PROCEDURE tbStepOutClick(Sender: TObject);
PROCEDURE tbStopClick(Sender: TObject);

PROCEDURE Splitter1Moved(Sender: TObject);
PROCEDURE UpdateTimeTimerTimer(Sender: TObject);
PROCEDURE inputPageControlChange(Sender: TObject);
PROCEDURE miAboutClick(Sender: TObject);

//File handling
PROCEDURE miClearClick(Sender: TObject);
PROCEDURE miCloseAllButCurrentClick(Sender: TObject);
PROCEDURE miCloseAllUnmodifiedClick(Sender: TObject);
PROCEDURE miCloseClick(Sender: TObject);
PROCEDURE miEditGuiScriptsClick(Sender: TObject);
PROCEDURE miOpenClick(Sender: TObject);
PROCEDURE miReloadClick(Sender: TObject);
PROCEDURE miSaveAsClick(Sender: TObject);
PROCEDURE miSaveClick(Sender: TObject);

//Settings
PROCEDURE miDecFontSizeClick(Sender: TObject);
PROCEDURE miIncFontSizeClick(Sender: TObject);
PROCEDURE miMinErrorlevel1Click(Sender: TObject);
PROCEDURE miMinErrorlevel2Click(Sender: TObject);
PROCEDURE miMinErrorlevel3Click(Sender: TObject);
PROCEDURE miMinErrorlevel4Click(Sender: TObject);
PROCEDURE miDeclarationEchoClick(Sender: TObject);
PROCEDURE miExpressionEchoClick(Sender: TObject);
PROCEDURE miExpressionResultClick(Sender: TObject);
PROCEDURE miTimingInfoClick(Sender: TObject);
PROCEDURE miWrapEchoClick(Sender: TObject);
PROCEDURE miProfileClick(Sender: TObject);
PROCEDURE miFullscreenClick(Sender: TObject);

PROCEDURE subMenuFileClick(Sender: TObject);
PROCEDURE miFileHistory0Click(Sender: TObject);
PROCEDURE miGotoClick(Sender: TObject);
PROCEDURE miHelpClick(Sender: TObject);
PROCEDURE miHelpExternallyClick(Sender: TObject);
PROCEDURE miHtmlExportClick(Sender: TObject);
PROCEDURE miNewCentralPackageClick(Sender: TObject);
PROCEDURE miOpenDemoClick(Sender: TObject);
PROCEDURE miRunCustomUtilScript(Sender: TObject);
PROCEDURE miUtilityScriptRootClick(Sender: TObject);
PROCEDURE mi_settingsClick(Sender: TObject);
PROCEDURE pmiOpenFile(CONST idOrName:string);

PROCEDURE Splitter3Moved(Sender: TObject);
PROCEDURE callStackListSelectionChange(Sender: TObject; User: boolean);
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
    setLength(scriptMenuItems[st_edit  ],0);
    setLength(scriptMenuItems[st_insert],0);
    setLength(scriptMenuItems[st_util ],0);
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
    runnerModel.create;
    focusEditorOnEditMouseUp:=false;

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
    runnerModel.destroy;
    completionLogic.destroy;
    guiTaskQueue.destroy;
  end;

PROCEDURE TMnhForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  begin
    inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(FileNames,true);
  end;

PROCEDURE TMnhForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(self,ssShift in Shift);
    if (key=116) and (ssShift in Shift) and tbRun      .enabled then tbRunClick(Sender);
    if (key=117) and                        tbStepIn   .enabled then tbStepInClick(Sender);
    if (key=118) then begin
      if not(ssShift in Shift)          and tbStep     .enabled then tbStepClick(Sender);
      if    (ssShift in Shift)          and tbMicroStep.enabled then tbMicroStepClick(Sender);
    end;
    if (key=119)                        and tbStepOut  .enabled then tbStepOutClick(Sender);
  end;

VAR firstShow:boolean=true;
PROCEDURE TMnhForm.FormResize(Sender: TObject);
  begin
    if not(firstShow) then updateFormPositionInSettings;
    if helpPopupMemo.visible then positionHelpNotifier;
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
                           miLanguageRoot,
                           @InputEditKeyDown,
                           @InputEditMouseDown,
                           @InputEditProcessUserCommand);
      UpdateTimeTimer.enabled:=true;
      enableDynamicItems;
      firstShow:=false;
    end;
    if hasEditor then getEditor^.activate;
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

PROCEDURE TMnhForm.OutputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=13) and (ssCtrl in Shift) then openLocation(guessLocationFromString(OutputEdit.lines[OutputEdit.CaretY-1],false));
  end;

PROCEDURE TMnhForm.OutputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    if (ssCtrl) in Shift then begin
      point.x:=x;
      point.y:=y;
      point:=OutputEdit.PixelsToRowColumn(point);
      focusEditorOnEditMouseUp:=openLocation(guessLocationFromString(OutputEdit.lines[point.y-1],false));
    end;
  end;

PROCEDURE TMnhForm.OutputEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if focusEditorOnEditMouseUp then ActiveControl:=getEditor^.editor;
    focusEditorOnEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.assistanceSynEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=13) and (ssCtrl in Shift) then openLocation(guessLocationFromString(assistanceSynEdit.lines[assistanceSynEdit.CaretY-1],false));
  end;

PROCEDURE TMnhForm.assistanceSynEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    if (ssCtrl) in Shift then begin
      point.x:=x;
      point.y:=y;
      point:=assistanceSynEdit.PixelsToRowColumn(point);
      focusEditorOnEditMouseUp:=openLocation(guessLocationFromString(assistanceSynEdit.lines[point.y-1],false));
    end;
  end;

PROCEDURE TMnhForm.assistanceSynEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if focusEditorOnEditMouseUp then ActiveControl:=getEditor^.editor;
    focusEditorOnEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.InputEditProcessUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  begin
    if command=ecUserDefinedFirst   then getEditor^.toggleComment;
    if command=ecUserDefinedFirst+1 then cycleEditors(true);
    if command=ecUserDefinedFirst+2 then cycleEditors(false);
    if command=ecUserDefinedFirst+3 then begin
      getEditor^.toggleBreakpoint;
      miDebug.Checked:=true;
      runnerModel.debugMode:=true;
      onDebuggerEvent;
    end;
  end;

PROCEDURE TMnhForm.miFindClick        (Sender: TObject); begin searchReplaceModel.beginFindOrReplace(focusedEditor,true);  end;
PROCEDURE TMnhForm.miReplaceClick     (Sender: TObject); begin searchReplaceModel.beginFindOrReplace(focusedEditor,false); end;
PROCEDURE TMnhForm.miFindNextClick    (Sender: TObject); begin searchReplaceModel.doFindNext        (focusedEditor);       end;
PROCEDURE TMnhForm.miFindPreviousClick(Sender: TObject); begin searchReplaceModel.doFindPrevious    (focusedEditor);       end;

PROCEDURE TMnhForm.miCallMainClick(Sender: TObject);
  begin
    if not(runnerModel.canRun) then exit;
    askForm.initWithQuestion('Please give command line parameters');
    if askForm.ShowModal=mrOk
    then runnerModel.customRun(true,miProfile.Checked,askForm.getLastAnswerReleasing(nil))
    else                                              askForm.getLastAnswerReleasing(nil);
  end;

PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    runnerModel.customRun(false,miProfile.Checked);
  end;

PROCEDURE TMnhForm.miDebugClick(Sender: TObject);
  begin
    miDebug.Checked:=not(miDebug.Checked);
    runnerModel.debugMode:=miDebug.Checked;
    onDebuggerEvent;
  end;

PROCEDURE TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin
    runnerModel.haltEvaluation;
    currentSnapshot:=nil;
  end;

PROCEDURE TMnhForm.tbRunClick(Sender: TObject);
  begin
    if runnerModel.canRun then runnerModel.rerun(miProfile.Checked)
    else if runEvaluator.context.isPaused then runnerModel.doDebuggerAction(runUntilBreakpoint)
                                          else runnerModel.doDebuggerAction(breakSoonest);
    currentSnapshot:=nil;
  end;

PROCEDURE TMnhForm.tbMicroStepClick(Sender: TObject); begin if tbMicroStep.enabled then begin runnerModel.doDebuggerAction(breakSoonest     ); currentSnapshot:=nil; end; end;
PROCEDURE TMnhForm.tbStepClick     (Sender: TObject); begin if tbStep     .enabled then begin runnerModel.doDebuggerAction(breakOnLineChange); currentSnapshot:=nil; end; end;
PROCEDURE TMnhForm.tbStepInClick   (Sender: TObject); begin if tbStepIn   .enabled then begin runnerModel.doDebuggerAction(breakOnStepIn    ); currentSnapshot:=nil; end; end;
PROCEDURE TMnhForm.tbStepOutClick  (Sender: TObject); begin if tbStepOut  .enabled then begin runnerModel.doDebuggerAction(breakOnStepOut   ); currentSnapshot:=nil; end; end;
PROCEDURE TMnhForm.tbStopClick(Sender: TObject);
  begin
    runnerModel.haltEvaluation;
    currentSnapshot:=nil;
  end;

PROCEDURE TMnhForm.Splitter1Moved(Sender: TObject);
  begin
    if helpPopupMemo.visible then positionHelpNotifier;
    if not(firstShow) then settings.value^.mainForm.relativeSplitterPosition:=outputPageControl.height/ClientHeight;
  end;

PROCEDURE TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  FUNCTION processIpcMessages:boolean;
    VAR filesToOpen:T_arrayOfString;
    begin
      filesToOpen:=ipcModel.getFilesToOpen;
      if length(filesToOpen)=0 then exit(false);
      inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(filesToOpen,true);
      result:=true;
    end;

  PROCEDURE autosaveSettings;
    begin
      if not(settings.value^.savingRequested) then exit;
      storeEditorsToSettings;
      saveSettings;
    end;

  begin
    if askForm.displayPending then askForm.Show;
    if guiOutAdapter.flushToGui(OutputEdit) then exit;
    while guiTaskQueue.executeTask do;
    if processIpcMessages then exit;
    autosaveSettings;
    checkForFileChanges;
  end;

PROCEDURE TMnhForm.inputPageControlChange(Sender: TObject);
  begin
    if hasEditor then getEditor^.activate;
  end;

PROCEDURE TMnhForm.miAboutClick(Sender: TObject); begin splashForm.ShowModal; end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject); begin inputPageControl.activePageIndex:=addEditorMetaForNewFile(); end;

PROCEDURE TMnhForm.miCloseAllButCurrentClick(Sender: TObject); begin closeAllEditorsButCurrent; end;
PROCEDURE TMnhForm.miCloseAllUnmodifiedClick(Sender: TObject); begin closeAllUnmodifiedEditors; end;
PROCEDURE TMnhForm.miCloseClick(Sender: TObject);              begin if hasEditor then getEditor^.closeEditorWithDialogs; end;
PROCEDURE TMnhForm.miEditGuiScriptsClick(Sender: TObject);
  begin
    inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(utilityScriptFileName,true);
  end;

PROCEDURE TMnhForm.miOpenClick(Sender: TObject);
  begin
    OpenDialog.FilterIndex:=1;
    OpenDialog.options:=OpenDialog.options+[ofPathMustExist,ofFileMustExist];
    OpenDialog.title:='Open file';
    if OpenDialog.execute and fileExists(OpenDialog.fileName)
    then inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(OpenDialog.fileName,true);
  end;

PROCEDURE TMnhForm.miReloadClick(Sender: TObject); begin if hasEditor then getEditor^.reloadFile(getEditor^.getPath); end;
PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject); begin if hasEditor then getEditor^.saveAsWithDialog;               end;
PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin
    if hasEditor then begin
      getEditor^.saveWithDialog;
      if getEditor^.getPath=utilityScriptFileName then runEvaluator.ensureEditScripts;
    end;
  end;

PROCEDURE TMnhForm.miDecFontSizeClick(Sender: TObject); begin SettingsForm.fontSize:=SettingsForm.fontSize-1; processFontSettings; end;
PROCEDURE TMnhForm.miIncFontSizeClick(Sender: TObject); begin SettingsForm.fontSize:=SettingsForm.fontSize+1; processFontSettings; end;
PROCEDURE TMnhForm.miMinErrorlevel1Click(Sender: TObject); begin setVerbosity(1); end;
PROCEDURE TMnhForm.miMinErrorlevel2Click(Sender: TObject); begin setVerbosity(2); end;
PROCEDURE TMnhForm.miMinErrorlevel3Click(Sender: TObject); begin setVerbosity(3); end;
PROCEDURE TMnhForm.miMinErrorlevel4Click(Sender: TObject); begin setVerbosity(4); end;
PROCEDURE TMnhForm.miDeclarationEchoClick (Sender: TObject); begin setSpecificMessageType(miDeclarationEcho .Checked,mt_echo_declaration); end;
PROCEDURE TMnhForm.miExpressionEchoClick  (Sender: TObject); begin setSpecificMessageType(miExpressionEcho  .Checked,mt_echo_input);       end;
PROCEDURE TMnhForm.miExpressionResultClick(Sender: TObject); begin setSpecificMessageType(miExpressionResult.Checked,mt_echo_output);      end;
PROCEDURE TMnhForm.miTimingInfoClick      (Sender: TObject); begin setSpecificMessageType(miTimingInfo      .Checked,mt_timing_info);      end;
PROCEDURE TMnhForm.miWrapEchoClick        (Sender: TObject);
  begin
    settings.value^.wordWrapEcho:=miWrapEcho.Checked;
    updateWordWrap;
  end;

PROCEDURE TMnhForm.miProfileClick(Sender: TObject);
  begin
    miProfile.Checked:=not(miProfile.Checked);
    if miProfile.Checked and not(miTimingInfo.Checked) then miTimingInfoClick(Sender);
  end;

PROCEDURE TMnhForm.miFullscreenClick(Sender: TObject);
  begin
    if BorderStyle=bsSizeable then begin
      BorderStyle:=bsNone;
      WindowState:=wsFullScreen;
    end else begin
      BorderStyle:=bsSizeable;
      WindowState:=wsMaximized;
    end;
  end;

PROCEDURE TMnhForm.subMenuFileClick(Sender: TObject);
  begin
    updateFileHistory;
  end;

PROCEDURE TMnhForm.miFileHistory0Click(Sender: TObject);
  VAR historyIdx:longint;
  begin
    historyIdx:=TMenuItem(Sender).Tag;
    with settings.value^.workspace.fileHistory do begin
      if fileExists(historyItem(historyIdx))
      then inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(historyItem(historyIdx),true)
      else if polishHistory then updateFileHistory;
    end;
  end;

PROCEDURE TMnhForm.miGotoClick(Sender: TObject);
  VAR i:longint;
  begin
    if not(hasEditor) then exit;
    with getEditor^ do begin
      askForm.initWithQuestion('Go to line');
      askForm.ShowModal;
      i:=strToIntDef(askForm.getLastAnswerReleasing(nil),-1);
      if i>=1 then begin
        editor.CaretX:=1;
        editor.CaretY:=i;
      end;
    end;
  end;

PROCEDURE TMnhForm.miHelpClick(Sender: TObject);
  begin
    miHelp.Checked:=not(miHelp.Checked);
    if not(miHelp.Checked) then helpPopupMemo.visible:=false
                           else begin
                             helpPopupMemo.visible:=true;
                             getEditor^.setUnderCursor(false,false);
                             positionHelpNotifier;
                           end;
  end;

PROCEDURE TMnhForm.miHelpExternallyClick(Sender: TObject);
  begin
    makeHtmlFromTemplate();
    OpenURL('file:///'+replaceAll(expandFileName(getHtmlRoot+'/index.html'   ),'\','/'));
  end;

PROCEDURE TMnhForm.miHtmlExportClick(Sender: TObject); begin if hasEditor then getEditor^.exportToHtml; end;
PROCEDURE TMnhForm.miNewCentralPackageClick(Sender: TObject);
  begin
    if newCentralPackageForm.ShowModal=mrOk then
        inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(newCentralPackageForm.fileNameEdit.text,true);
  end;

PROCEDURE TMnhForm.miOpenDemoClick(Sender: TObject);
  begin
    if openDemoDialogForm.ShowModal=mrOk then
       inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(openDemoDialogForm.selectedFile,true);
  end;

PROCEDURE TMnhForm.miRunCustomUtilScript(Sender: TObject);
  VAR k:longint;
  begin
    if not(hasEditor) then exit;
    k:=inputPageControl.activePageIndex;
    with getEditor^ do runEvaluator.runUtilScript(TMenuItem(Sender).Tag,k,editor.lines,defaultExtensionByLanguage,pseudoName());
    updateEditorsByGuiStatus;
  end;

PROCEDURE TMnhForm.miUtilityScriptRootClick(Sender: TObject);
  begin
    updateScriptMenus;
  end;

PROCEDURE TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    initializeOutputSettings;
    processFontSettings;
  end;

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

PROCEDURE TMnhForm.Splitter3Moved(Sender: TObject); begin updateExpressionMemo; end;

FUNCTION treeLit_canExpand(L:P_literal):boolean;
  begin
    result:=(L<>nil) and (L^.literalType in C_compoundTypes) and (length(L^.toString(100))>=100);
  end;

FUNCTION treeLit_toString(L:P_literal):string;
  begin
    if L=nil then result:=' ' else
    if treeLit_canExpand(L) then result:=L^.typeString
                            else result:=L^.toString();
  end;

PROCEDURE TMnhForm.callStackListSelectionChange(Sender: TObject; User: boolean);
  VAR stackIdx:longint;
      report:T_variableReport;
      i:longint;
  PROCEDURE addVariable(CONST id:string; CONST value:P_literal);
    VAR newNode:TTreeNode;
        i:longint;
    begin
      newNode:=variablesTreeView.items.add(nil,id+'='+treeLit_toString(value));
      newNode.data:=value;
      if treeLit_canExpand(value) then for i:=0 to P_listLiteral(value)^.size-1 do
        variablesTreeView.items.AddChild(newNode,treeLit_toString(P_listLiteral(value)^[i])).data:=P_listLiteral(value)^[i];
    end;

  begin
    if currentSnapshot=nil then exit;
    stackIdx:=currentSnapshot^.callStack^.size-1-callStackList.ItemIndex;
    variablesTreeView.items.clear;

    if (stackIdx>=0) and (stackIdx<currentSnapshot^.callStack^.size) then begin
      callStackInfoStringGrid.Cells[1,0]:=ansistring(currentSnapshot^.callStack^[stackIdx].calleeLocation);
      callStackInfoStringGrid.Cells[1,1]:=currentSnapshot^.callStack^[stackIdx].calleeId;
      callStackInfoStringGrid.Cells[1,2]:=ansistring(currentSnapshot^.callStack^[stackIdx].calleeLocation);
    end;

    if (callStackList.ItemIndex<=0) then begin
      report.create;
      runEvaluator.reportVariables(report);
      runEvaluator.context.threadContext^.reportVariables(report);
      for i:=length(report.dat)-1 downto 0 do
        addVariable(report.dat[i].id+' ['+report.dat[i].location+']',report.dat[i].value);
      report.destroy;
    end;
  end;

PROCEDURE TMnhForm.variablesTreeViewExpanding(Sender: TObject; node: TTreeNode; VAR AllowExpansion: boolean);
  PROCEDURE addChildren(node2:TTreeNode);
    VAR L:P_literal;
        iter:T_arrayOfLiteral;
    begin
      iter:=P_compoundLiteral(node2.data)^.iteratableList;
      for L in iter do variablesTreeView.items.AddChild(node2,treeLit_toString(L)).data:=L;
      disposeLiteral(iter);
    end;

  VAR i:longint;
  begin
    AllowExpansion:=treeLit_canExpand(node.data);
    if not(AllowExpansion) then exit;
    for i:=0 to node.count-1 do
    if not(node.items[i].HasChildren) and (treeLit_canExpand(node.items[i].data)) then addChildren(node.items[i]);
  end;


{$endif}
