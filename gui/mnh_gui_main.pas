UNIT mnh_gui_main;

{$mode objfpc}{$H+}
INTERFACE
USES
  //basic classes
  Classes, sysutils, LCLType, lclintf, types,
  //my utilities:
  mnhFormHandler, myStringUtil, myGenerics,mySys,
  //GUI: LCL components
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, Grids, StdCtrls,
  //GUI: SynEdit
  SynEdit, SynMemo, SynGutterMarks, SynEditKeyCmds,
  //GUI: highlighters
  SynHighlighterMnh,
  SynExportHTML,
  //Other Forms:
  mnh_gui_settings,
  askDialog,
  mnh_tables,
  mnh_plotForm,
  mnh_splash,
  closeDialog,
  variableTreeViews,
  //MNH:
  editorMetaBase,
  editorMeta,
  searchModel,
  mnhCompletion,
  ipcModel,
  mnh_constants, basicTypes,mnh_settings,
  mnh_messages,
  out_adapters,
  litVar,
  funcs,
  debuggingVar,
  debugging,
  contexts,
  recyclers,
  packages,
  mnh_doc,
  cmdLineInterpretation,
  evalThread,
  treeUtil,
  menuUtil,
  guiOutAdapters,
  synOutAdapter,
  renameDialog,
  mnhCustomForm,
  openFile,
  saveFile,
  outlines;
TYPE
  {$define includeInterface}
  {$WARN 5024 OFF}

  { TMnhForm }

  TMnhForm = class(T_abstractMnhForm)
    cbOutlineShowPrivate,
    cbOutlineShowImported:     TCheckBox;
    autoShowOutputCheckbox: TCheckBox;
    evaluateQuickInCurrentPackageCheckbox: TCheckBox;
    FindDialog:                TFindDialog;
    callStackGroupBox,
    currentExpressionGroupBox,
    outlineGroupBox:                 TGroupBox;
    markerImageList,
    outlineImageList,
    breakpointsImagesList,
    debugItemsImageList:       TImageList;
    callStackList:             TListBox;
    MainMenu1:                 TMainMenu;
    miLanguageRoot,
    MenuItem4,
    miAbout,
    miCallMain,
    miClear,
    miClose,
    miCloseAllButCurrent,
    miCloseAllUnmodified,
    miDebug,
    miDecFontSize,
    miDeclarationEcho,
    miEvaluateNow,
    miExpressionEcho,
    miExpressionResult,
    miFind,
    miFindNext,
    miFindPrevious,
    miFullscreen,
    miGoto,
    miHaltEvaluation,
    miHelp,
    miIncFontSize,
    miMinErrorlevel1,
    miMinErrorlevel2,
    miMinErrorlevel3,
    miMinErrorlevel4,
    miOpen,
    miOpenDocumentation,
    miProfile,
    miReload,
    miReplace,
    miRunExternally,
    miSave,
    miSaveAs,
    miTimingInfo,
    miWrapEcho,
    mi_settings,
    subMenuCode,
    subMenuEvaluation,
    subMenuFile,
    subMenuHelp,
    subMenuSearch,
    miEditGuiScripts,
    submenuEditorAppearance,
    miStackTracing,
    miUserErrors,
    miFileHistoryRoot,
    miHtmlExport,
    miRecentFileRoot,
    miRename,
    miRestoreDefaultFile,
    miShowQuickEval,
    miShowOutput,
    miShowAssistance:          TMenuItem;
    rbOutlineSortByLoc: TRadioButton;
    rbOutlineSortByName: TRadioButton;
    rbOutlineSortByNameCase: TRadioButton;
    OpenDialog:                TOpenDialog;
    inputPageControl,
    outputPageControl:         TPageControl;
    BottomPanel,
    Panel1,
    Panel2:                    TPanel;
    ReplaceDialog:             TReplaceDialog;
    Splitter1,
    Splitter2,
    Splitter3,
    Splitter4,
    Splitter5:                 TSplitter;
    StatusBar:                 TStatusBar;
    callStackInfoStringGrid:   TStringGrid;
    outputEdit,
    QuickOutputEdit,
    assistanceSynEdit:         TSynEdit;
    SynExporterHTML:           TSynExporterHTML;
    SynGutterMarks0:           TSynGutterMarks;
    helpPopupMemo,
    currentExpressionMemo:     TSynMemo;
    debugTabSheet,
    outputTabSheet,
    assistanceTabSheet,
    QuickTabSheet:             TTabSheet;
    HelpSheet: TTabSheet;
    outlineTreeView: TTreeView;
    UpdateTimeTimer:           TTimer;
    DebugToolbar:              TToolBar;
    tbMicroStep,
    tbRun,
    tbStep,
    tbStepIn,
    tbStepOut,
    tbStop:                    TToolButton;
    variablesTreeView:         TTreeView;
    {$i mnh_gui_main_events.inc}
    PROCEDURE onEditFinished(CONST data:P_editScriptTask   ); override;
    PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); override;
    PROCEDURE onDebuggerEvent;                                override;
    PROCEDURE onEndOfEvaluation;                              override;
    PROCEDURE triggerFastPolling;                             override;
    PROCEDURE activeFileChanged(CONST newCaption:string; CONST isMnhFile:boolean; CONST isPseudoFile:boolean); override;
    FUNCTION openLocation(CONST location:T_searchTokenLocation):boolean;
    PROCEDURE enableDynamicItems;
    PROCEDURE updateScriptMenus;
    PROCEDURE updateFileHistory;
  private
    quitPosted:boolean;
    focusEditorOnEditMouseUp:boolean;
    outputHighlighter,debugHighlighter,helpHighlighter:TSynMnhSyn;
    scriptMenu:T_submenuModel;
    historyMenuItems:array of TMenuItem;
    recentFileMenuItems:array of TMenuItem;
    variablesTreeViewModel:T_treeModel;

    debuggerData:record
      globalVariableReport,
      localVariableReport,
      inlineVariableReport:P_variableTreeEntryCategoryNode;
    end;
    quick:record
      meta:T_basicEditorMeta;
      adapters:P_messagesDistributor;
      task:T_postEvaluationData;
      completion:T_completionLogic;
      evaluationDeferred:boolean;
    end;
    PROCEDURE QuickEditChange(Sender: TObject);
    FUNCTION focusedEditor:TSynEdit;
    PROCEDURE updateExpressionMemo;
  end;

VAR MnhForm: TMnhForm;

{$undef includeInterface}
IMPLEMENTATION

{$R *.lfm}
{$define includeImplementation}
{$i settingsLogic.inc}

FUNCTION TMnhForm.focusedEditor: TSynEdit;
  begin
    result:=assistanceSynEdit;
    if result.Focused then exit(result);
    if hasEditor then begin
      result:=getEditor^.editor;
      if result.Focused then exit(result);
    end;
    result:=outputEdit;
  end;

{$i func_defines.inc}
FUNCTION editors_impl intFuncSignature;
  VAR s:string;
  begin
    result:=newListLiteral();
    for s in allPseudoNames do listResult^.appendString(s);
  end;

FUNCTION editorContent_impl intFuncSignature;
  VAR meta:P_editorMeta;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      meta:=editorMeta.getMeta(str0^.value);
      if meta=nil then exit(newVoidLiteral);
      result:=newListLiteral(meta^.editor.lines.count);
      for i:=0 to meta^.editor.lines.count-1 do listResult^.appendString(meta^.editor.lines[i]);
    end;
  end;

PROCEDURE TMnhForm.onEditFinished(CONST data:P_editScriptTask);
  VAR outIdx:longint;
  begin
    {$ifdef debugMode} writeln('        DEBUG: TMnhForm.onEditFinished; data present: ',data<>nil,'; successful: ',(data<>nil) and (data^.successful)); {$endif}
    if data^.successful then begin
      if (data^.wantOutput) and (data^.getOutput<>nil) and (data^.getOutput^.literalType=lt_stringList) then begin
        if data^.wantNewEditor then outIdx:=addEditorMetaForNewFile
                               else outIdx:=data^.inputIdx;
        inputPageControl.activePageIndex:=outIdx;
        getEditor^.setLanguage(data^.getOutputLanguage,LANG_TXT);
        getEditor^.updateContentAfterEditScript(P_listLiteral(data^.getOutput));
      end else if (data^.wantInsert) and (data^.getOutput<>nil) and (data^.getOutput^.literalType=lt_string) then begin
        inputPageControl.activePageIndex:=data^.inputIdx;
        getEditor^.insertText(P_stringLiteral(data^.getOutput)^.value);
      end;
    end;
    disposeMessage(data);
    updateEditorsByGuiStatus;
  end;

VAR currentSnapshot:P_debuggingSnapshot=nil;

PROCEDURE TMnhForm.onBreakpoint(CONST data:P_debuggingSnapshot);
  PROCEDURE jumpToFile;
    begin
      runnerModel.markDebugLine(outputEdit,-1);
      if currentSnapshot^.getLocation.fileName='?' then exit;
      if openLocation(currentSnapshot^.getLocation) then begin
        runnerModel.markDebugLine(getEditor^.editor,currentSnapshot^.getLocation.line);
        getEditor^.editor.Repaint;
      end;
    end;

  PROCEDURE clearStackView;
    VAR i:longint;
    begin
      variablesTreeView.items.clear;
      for i:=0 to callStackInfoStringGrid.RowCount-1 do callStackInfoStringGrid.Cells[1,i]:='';

      callStackList.items.clear;
      for i:=currentSnapshot^.callStack^.size-1 downto 0 do
      callStackList.items.add(currentSnapshot^.callStack^[i].calleeId);
      callStackList.ItemIndex:=0;
    end;

  begin
    if not(runnerModel.debugMode) or (data=nil) then exit;
    onDebuggerEvent;

    currentSnapshot:=P_debuggingSnapshot(data);

    if debuggerData.globalVariableReport<>nil then dispose(debuggerData.globalVariableReport,destroy);
    debuggerData.globalVariableReport:=runEvaluator.reportVariables;

    if debuggerData.localVariableReport<>nil then debuggerData.localVariableReport^.clear
                                             else new(debuggerData.localVariableReport,create(dvc_local));
    runEvaluator.globals.primaryContext.reportVariables(debuggerData.localVariableReport^);

    if debuggerData.inlineVariableReport<>nil then debuggerData.inlineVariableReport^.clear
                                              else new(debuggerData.inlineVariableReport,create(dvc_inline));

    jumpToFile;
    clearStackView;
    callStackListSelectionChange(nil,false);

    outputPageControl.activePage:=debugTabSheet;
  end;

PROCEDURE TMnhForm.onDebuggerEvent;
   begin
    enableDynamicItems;
    updateEditorsByGuiStatus;
  end;

PROCEDURE TMnhForm.onEndOfEvaluation;
  begin
    enableDynamicItems;
    updateEditorsByGuiStatus;
    if   outputPageControl.activePage<>QuickTabSheet
    then outputPageControl.activePage:=outputTabSheet;

    if debuggerData.globalVariableReport<>nil then begin dispose(debuggerData.globalVariableReport,destroy); debuggerData.globalVariableReport:=nil; end;
    if debuggerData.localVariableReport <>nil then begin dispose(debuggerData.localVariableReport ,destroy); debuggerData.localVariableReport :=nil; end;
    if debuggerData.inlineVariableReport<>nil then begin dispose(debuggerData.inlineVariableReport,destroy); debuggerData.inlineVariableReport:=nil; end;
  end;

PROCEDURE TMnhForm.triggerFastPolling;
  begin
    UpdateTimeTimer.interval:=1;
  end;

PROCEDURE TMnhForm.activeFileChanged(CONST newCaption:string; CONST isMnhFile:boolean; CONST isPseudoFile:boolean);
  begin
    caption:=newCaption;
    miRunExternally.enabled:=isMnhFile and not(isPseudoFile);
  end;

FUNCTION TMnhForm.openLocation(CONST location: T_searchTokenLocation): boolean;
  VAR newIdx:longint;
  begin
    if (location.fileName='') or (location.fileName='?') then exit(false);
    newIdx:=addOrGetEditorMetaForFiles(location.fileName,false);
    if newIdx<0 then exit(false);
    inputPageControl.activePageIndex:=newIdx;
    getEditor^.setCaret(location);
    ActiveControl:=getEditor^.editor;
    result:=true;
  end;

PROCEDURE TMnhForm.enableDynamicItems;
  VAR running:boolean;
      debugging:boolean;
      halted:boolean;
      locked:boolean;

  PROCEDURE handleButton(VAR button:TToolButton; CONST enabled:boolean; CONST enabledImageIndex:longint; CONST enableAlways:boolean=false);
    begin
      button.enabled:=enabled or enableAlways;
      if enabled then button.ImageIndex:=enabledImageIndex
                 else button.ImageIndex:=enabledImageIndex+1;
    end;

  begin
    running  :=runEvaluator.evaluationRunning;
    debugging:=runnerModel.debugMode;
    halted   :=runEvaluator.getRunnerStateInfo.state=es_debugHalted;
    locked   :=runnerModel.areEditorsLocked;

    miHaltEvaluation.enabled:=running or quick.task.processing;
    miEvaluateNow   .enabled:=runnerModel.canRun;
    miCallMain      .enabled:=runnerModel.canRun;

    debugTabSheet.tabVisible:=debugging;
    debugTabSheet.enabled   :=debugging;
    DebugToolbar .visible   :=debugging;
    DebugToolbar .enabled   :=debugging;
    if DebugToolbar.visible then DebugToolbar .top:=0
                            else if outputPageControl.activePage =debugTabSheet then
                                    outputPageControl.activePage:=outputTabSheet;
    handleButton(tbStop     ,halted or running, 2);
    handleButton(tbRun      ,halted or runnerModel.canRun, 0,true);
    handleButton(tbStep     ,halted , 4);
    handleButton(tbStepIn   ,halted , 6);
    handleButton(tbStepOut  ,halted , 8);
    handleButton(tbMicroStep,halted ,10);

    miSave             .enabled:=not(locked);
    miSaveAs           .enabled:=not(locked);
    miReload           .enabled:=not(locked);
    subMenuCode        .enabled:=not(locked);
    miEditGuiScripts   .enabled:=not(locked);
    miFileHistoryRoot  .enabled:=not(locked);
    miReplace          .enabled:=not(locked);
  end;

PROCEDURE TMnhForm.updateScriptMenus;
  VAR i:longint;
      scriptList:T_scriptMetaArray;
  begin
    scriptMenu.clear;
    scriptList:=runEvaluator.getScripts;
    for i:=0 to length(scriptList)-1 do scriptMenu.addItem(scriptList[i]^.getName,i);
    setLength(scriptList,0);
  end;

PROCEDURE TMnhForm.updateFileHistory;
  VAR i:longint;
      histItems:T_arrayOfString;
  begin
    for i:=0 to length(historyMenuItems)-1 do begin
      miFileHistoryRoot.remove(historyMenuItems[i]);
      FreeAndNil(historyMenuItems[i]);
    end;
    histItems:=fileHistory.items;
    setLength(historyMenuItems,length(histItems));
    for i:=0 to length(histItems)-1 do begin
      historyMenuItems[i]:=TMenuItem.create(MainMenu1);
      historyMenuItems[i].caption:=intToStr(i)+': '+histItems[i];
      historyMenuItems[i].Tag:=i;
      historyMenuItems[i].OnClick:=@miFileHistory0Click;
      miFileHistoryRoot.add(historyMenuItems[i]);
    end;
    //-----------------------------------------------------------
    for i:=0 to length(recentFileMenuItems)-1 do begin
      miRecentFileRoot.remove(recentFileMenuItems[i]);
      FreeAndNil(recentFileMenuItems[i]);
    end;
    recentlyActivated.polishHistory;
    if length(recentlyActivated.items)<=1 then exit;
    setLength(histItems,length(recentlyActivated.items)-1);
    for i:=1 to length(recentlyActivated.items)-1 do histItems[i-1]:=recentlyActivated.items[i];
    setLength(recentFileMenuItems,length(histItems));
    for i:=0 to length(histItems)-1 do begin
      recentFileMenuItems[i]:=TMenuItem.create(MainMenu1);
      recentFileMenuItems[i].caption:=intToStr(i)+': '+histItems[i];
      recentFileMenuItems[i].Tag:=i+1;
      recentFileMenuItems[i].OnClick:=@miRecentFileItemClick;
      miRecentFileRoot.add(recentFileMenuItems[i]);
    end;
  end;

PROCEDURE TMnhForm.QuickEditChange(Sender: TObject);
  VAR edit:P_editorMeta;
  begin
    if evaluateQuickInCurrentPackageCheckbox.checked then begin
      edit:=getEditor;
      quick.completion.assignEditor(quick.meta.editor,edit^.getCodeAssistanceData,true);
      if runnerModel.canRun(true) then begin
        if edit<>nil then edit^.setWorkingDir;
        quick.evaluationDeferred:=false;
        quick.task.triggerUpdate(runEvaluator.getPackageForPostEvaluation(edit,runnerModel.firstCallAfterActivation));
      end else quick.evaluationDeferred:=true;
    end else begin
      quick.completion.assignEditor(quick.meta.editor,nil,true);
      quick.task.triggerUpdate(nil);
    end;
  end;

PROCEDURE TMnhForm.updateExpressionMemo;
  VAR lines,chars:longint;
      tokens:T_arrayOfString;
      txt:ansistring;
      k:longint=0;
      stackIdx:longint;
      parameterInfo:P_variableTreeEntryCategoryNode=nil;
      firstInLine:boolean;
  begin
    currentExpressionMemo.lines.clear;
    if currentSnapshot=nil then exit;

    lines:=currentExpressionMemo.LinesInWindow;
    chars:=currentExpressionMemo.charsInWindow;
    if (lines*chars<50) then begin
      lines:=1;
      chars:=50;
    end;

    stackIdx:=currentSnapshot^.callStack^.size-1-callStackList.ItemIndex;
    if (stackIdx>=0) and (stackIdx<currentSnapshot^.callStack^.size)
    then parameterInfo:=currentSnapshot^.callStack^[stackIdx].parameters;

    tokens:=currentSnapshot^.tokenStack^
            .toDebuggerString(currentSnapshot^.first,
                              round(lines*chars*0.9),
                              parameterInfo,
                              debuggerData.localVariableReport,
                              debuggerData.globalVariableReport,
                              debuggerData.inlineVariableReport);

    while k<length(tokens) do begin
      txt:='';
      firstInLine:=true;
      while (k<length(tokens)) and (firstInLine or (length(txt)+length(tokens[k])<=chars)) do begin
        txt:=txt+tokens[k];
        inc(k);
        firstInLine:=false;
      end;
      currentExpressionMemo.lines.append(txt);
    end;
    setLength(tokens,0);
  end;

{$i mnh_gui_main_events.inc}

end.
