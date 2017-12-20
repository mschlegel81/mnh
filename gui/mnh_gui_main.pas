UNIT mnh_gui_main;

{$mode objfpc}{$H+}
INTERFACE
USES
  //basic classes
  Classes, sysutils, LCLType, lclintf, types,
  //my utilities:
  mnhFormHandler, myStringUtil, myGenerics,
  //GUI: LCL components
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, Grids, StdCtrls,
  //GUI: SynEdit
  SynEdit, SynMemo, SynGutterMarks, SynEditKeyCmds,
  //GUI: highlighters
  SynHighlighterMnh,
  SynExportHTML,
  //Other Forms:
  newCentralPackageDialog,
  mnh_gui_settings,
  askDialog,
  mnh_tables,
  openDemoDialog,
  mnh_plotForm,
  mnh_splash,
  //MNH:
  editorMeta,
  editPopupModel,
  searchModel,
  mnhCompletion,
  ipcModel,
  mnh_constants, mnh_basicTypes,mnh_settings,
  mnh_out_adapters,
  mnh_litVar,
  mnh_funcs, valueStore,
  mnh_debugging,
  mnh_contexts,
  mnh_packages,
  mnh_doc,
  mnh_cmdLineInterpretation,
  mnh_evalThread,
  guiOutAdapters;
TYPE
  {$define includeInterface}
  {$include guiEditorInterface.inc}
  {$WARN 5024 OFF}

  TMnhForm = class(T_abstractMnhForm)
    cbOutlineSortByName,
    cbOutlineShowPrivate,
    cbOutlineShowImported:     TCheckBox;
    FindDialog:                TFindDialog;
    callStackGroupBox,
    currentExpressionGroupBox,
    GroupBox1:                 TGroupBox;
    breakpointsImagesList,
    debugItemsImageList:       TImageList;
    callStackList:             TListBox;
    MainMenu1:                 TMainMenu;
    editScriptRoot,
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
    miHaltEvalutaion,
    miHelp,
    miIncFontSize,
    miMinErrorlevel1,
    miMinErrorlevel2,
    miMinErrorlevel3,
    miMinErrorlevel4,
    miNewCentralPackage,
    miOpen,
    miOpenDemo,
    miOpenDocumentation,
    miProfile,
    miReload,
    miReplace,
    miSave,
    miSaveAs,
    miTimingInfo,
    miWrapEcho,
    mi_settings,
    pmiOpenFile1,
    pmiOpenFile2,
    subMenuCode,
    subMenuEvaluation,
    subMenuFile,
    subMenuHelp,
    subMenuSearch,
    miInserScriptRoot,
    miEditGuiScripts,
    miUtilityScriptRoot,
    submenuEditorAppearance,
    miStackTracing,
    miUserErrors,
    miFileHistoryRoot,
    miHtmlExport,
    miRecentFileRoot,
    miShowQuickEval,
    miShowOutput,
    miShowAssistance:          TMenuItem;
    OpenDialog:                TOpenDialog;
    inputPageControl,
    outputPageControl:         TPageControl;
    leftHandSidePanel,
    BottomPanel,
    Panel1,
    Panel2:                    TPanel;
    EditorPopupMenu:           TPopupMenu;
    ReplaceDialog:             TReplaceDialog;
    SaveDialog:                TSaveDialog;
    Splitter1,
    Splitter2,
    Splitter3,
    Splitter4:                 TSplitter;
    StatusBar:                 TStatusBar;
    callStackInfoStringGrid:   TStringGrid;
    outlineSynEdit,
    OutputEdit,
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
    PROCEDURE onEditFinished(CONST data:pointer; CONST successful:boolean); override;
    PROCEDURE onBreakpoint  (CONST data:pointer);                           override;
    PROCEDURE onDebuggerEvent;                                              override;
    PROCEDURE onEndOfEvaluation;                                            override;
    PROCEDURE triggerFastPolling;                                           override;
    FUNCTION openLocation(CONST location:T_searchTokenLocation):boolean;
    PROCEDURE enableDynamicItems;
    PROCEDURE updateScriptMenus;
    PROCEDURE updateFileHistory;
  private
    focusEditorOnEditMouseUp:boolean;
    outputHighlighter,debugHighlighter,helpHighlighter:TSynMnhSyn;
    scriptMenuItems:array[T_scriptType] of array of TMenuItem;
    historyMenuItems:array of TMenuItem;
    recentFileMenuItems:array of TMenuItem;
    quick:record
      meta:T_editorMeta;
      adapters:P_adapters;
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
{$include guiEditorInterface.inc}
{$i settingsLogic.inc}

FUNCTION TMnhForm.focusedEditor: TSynEdit;
  begin
    result:=assistanceSynEdit;
    if result.Focused then exit(result);
    if hasEditor then begin
      result:=getEditor^.editor;
      if result.Focused then exit(result);
    end;
    result:=OutputEdit;
  end;

{$i mnh_func_defines.inc}
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

FUNCTION openInEditor_impl intFuncSignature;
  VAR task:P_openEditorTask;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      new(task,create(str0^.value));
      guiTaskQueue.enqueueTask(task);
      result:=newVoidLiteral;
    end;
  end;

PROCEDURE TMnhForm.onEditFinished(CONST data: pointer; CONST successful: boolean);
  VAR task:P_editScriptTask;
      outIdx:longint;
  begin
    {$ifdef debugMode} writeln('        DEBUG: TMnhForm.onEditFinished; data present: ',data<>nil,'; successful: ',successful); {$endif}
    task:=data;
    if successful then begin
      if (task^.wantOutput) and (task^.getOutput<>nil) and (task^.getOutput^.literalType=lt_stringList) then begin
        if task^.wantNewEditor then outIdx:=addEditorMetaForNewFile
                               else outIdx:=task^.inputIdx;
        inputPageControl.activePageIndex:=outIdx;
        getEditor^.setLanguage(task^.getOutputLanguage,LANG_TXT);
        getEditor^.updateContentAfterEditScript(P_listLiteral(task^.getOutput));
      end else if (task^.wantInsert) and (task^.getOutput<>nil) and (task^.getOutput^.literalType=lt_string) then begin
        inputPageControl.activePageIndex:=task^.inputIdx;
        getEditor^.insertText(P_stringLiteral(task^.getOutput)^.value);
      end;
    end;
    dispose(task,destroy);
    updateEditorsByGuiStatus;
  end;

VAR currentSnapshot:P_debuggingSnapshot=nil;

PROCEDURE TMnhForm.onBreakpoint(CONST data: pointer);
  PROCEDURE jumpToFile;
    begin
      runnerModel.markDebugLine(OutputEdit,-1);
      if currentSnapshot^.location.package=nil then exit;
      if openLocation(currentSnapshot^.location) then begin
        runnerModel.markDebugLine(getEditor^.editor,currentSnapshot^.location.line);
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

    jumpToFile;
    clearStackView;
    updateExpressionMemo;

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
  end;

PROCEDURE TMnhForm.triggerFastPolling;
  begin
    UpdateTimeTimer.interval:=1;
  end;

FUNCTION TMnhForm.openLocation(CONST location: T_searchTokenLocation): boolean;
  VAR newIdx:longint;
  begin
    if location.fileName='' then exit(false);
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

    miHaltEvalutaion.enabled:=running;
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
    editScriptRoot     .enabled:=not(locked);
    miInserScriptRoot  .enabled:=not(locked);
    miEditGuiScripts   .enabled:=not(locked);
    miUtilityScriptRoot.enabled:=not(locked);
    miFileHistoryRoot  .enabled:=not(locked);
    miReplace          .enabled:=not(locked);
    miOpenDemo         .enabled:=not(locked);
  end;

PROCEDURE TMnhForm.updateScriptMenus;
  VAR i,k:longint;
      scriptList:T_scriptMetaArray;
      script:P_scriptMeta;
      scriptType:T_scriptType;
      root:TMenuItem;
  begin
    for scriptType in T_scriptType do begin
      case scriptType of
        st_edit: root:=editScriptRoot;
        st_util: root:=miUtilityScriptRoot;
        else     root:=miInserScriptRoot;
      end;
      for i:=0 to length(scriptMenuItems[scriptType])-1 do begin
        root.remove(scriptMenuItems[scriptType][i]);
        FreeAndNil(scriptMenuItems[scriptType][i]);
      end;
      setLength(scriptMenuItems[scriptType],0);
    end;
    scriptList:=runEvaluator.getScripts;
    for i:=0 to length(scriptList)-1 do begin
      script:=scriptList[i];
      scriptType:=script^.getScriptType;
      case scriptType of
        st_edit: root:=editScriptRoot;
        st_util: root:=miUtilityScriptRoot;
        else     root:=miInserScriptRoot;
      end;
      k:=length(scriptMenuItems[scriptType]);
      setLength(scriptMenuItems[scriptType],k+1);
      scriptMenuItems[scriptType][k]:=TMenuItem.create(MainMenu1);
      scriptMenuItems[scriptType][k].caption:=script^.getName;
      scriptMenuItems[scriptType][k].Tag:=i;
      scriptMenuItems[scriptType][k].OnClick:=@miRunCustomUtilScript;
      root.add(scriptMenuItems[scriptType][k]);
    end;
  end;

PROCEDURE TMnhForm.updateFileHistory;
  VAR i:longint;
      histItems:T_arrayOfString;
  begin
    for i:=0 to length(historyMenuItems)-1 do begin
      miFileHistoryRoot.remove(historyMenuItems[i]);
      FreeAndNil(historyMenuItems[i]);
    end;
    histItems:=settings.value^.workspace.fileHistory.items;
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
    edit:=getEditor;
    if (edit=nil) or (edit^.language<>LANG_MNH) then exit;
    quick.completion.assignEditor(quick.meta.editor,edit^.getAssistant);
    if runnerModel.canRun then begin
      edit^.setWorkingDir;
      quick.evaluationDeferred:=false;
      quick.task.triggerUpdate(runEvaluator.getPackageForPostEvaluation(edit));
    end else quick.evaluationDeferred:=true;
  end;

PROCEDURE TMnhForm.updateExpressionMemo;
  VAR lines,chars:longint;
      tokens:T_arrayOfString;
      txt:ansistring;
      k:longint=0;
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

    tokens:=tokenSplit(currentSnapshot^.tokenStack^.toString(currentSnapshot^.first,round(lines*chars*0.9)));

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
