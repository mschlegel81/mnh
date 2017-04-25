UNIT mnh_gui_main;

{$mode objfpc}{$H+}
INTERFACE
USES
  //basic classes
  Classes, sysutils, FileUtil, LazUTF8, LCLType, lclintf, types,
  //my utilities:
  mnhFormHandler, myStringUtil, myGenerics,
  //GUI: LCL components
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, Grids, StdCtrls, simpleipc,
  //GUI: SynEdit
  SynEdit, SynEditTypes, SynCompletion, SynPluginMultiCaret, SynEditMiscClasses, SynMemo, SynGutterMarks, SynEditMarks, SynEditKeyCmds,
  //GUI: highlighters
  SynHighlighterMnh, SynHighlighterPas, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterJScript, SynHighlighterPerl, SynHighlighterHTML,
  SynHighlighterXML, SynHighlighterDiff, synhighlighterunixshellscript,
  SynHighlighterCss, SynHighlighterPHP, SynHighlighterSQL, SynHighlighterPython,
  SynHighlighterVB, SynHighlighterBat, SynHighlighterIni, SynEditHighlighter,
  SynExportHTML,
  //Other Forms:
  newCentralPackageDialog,
  mnh_gui_settings,
  closeDialog,
  askDialog,
  mnh_tables,
  openDemoDialog,
  mnh_plotForm,
  mnh_splash,
  //MNH:
  mnh_constants, mnh_basicTypes, mnh_fileWrappers,mnh_settings,
  mnh_out_adapters,
  mnh_litVar,
  mnh_funcs, valueStore,
  mnh_debugging,
  mnh_contexts,
  mnh_doc,
  mnh_cmdLineInterpretation,
  mnh_evalThread,
  guiOutAdapters;
CONST UNIQUE_EDITOR_IPC_ID='MNH5-editingGuiInstance';

CONST LANG_MNH   = 0;
      LANG_CPP   = 1;
      LANG_CSS   = 2;
      LANG_DIFF  = 3;
      LANG_HTML  = 4;
      LANG_INI   = 5;
      LANG_JAVA  = 6;
      LANG_JS    = 7;
      LANG_PAS   = 8;
      LANG_PERL  = 9;
      LANG_PHP   =10;
      LANG_PYTHON=11;
      LANG_SHELL =12;
      LANG_SQL   =13;
      LANG_VB    =14;
      LANG_BAT   =15;
      LANG_XML   =16;
      LANG_TXT   =17;

VAR fileTypeMeta:array of record
      extensionWithoutDot:string;
      language:byte;
      highlighter:TSynCustomHighlighter;
      menuItem:TMenuItem;
    end;

TYPE
  {$define includeInterface}
  {$include guiEditorInterface.inc}
  {$include editorMeta.inc}
  {$WARN 5024 OFF}
  { TMnhForm }

  TMnhForm = class(T_abstractMnhForm)
    FindDialog:                TFindDialog;
    callStackGroupBox,
    currentExpressionGroupBox: TGroupBox;
    breakpointsImagesList,
    debugItemsImageList:       TImageList;
    callStackList:             TListBox;
    MainMenu1:                 TMainMenu;
    editScriptRoot,
    MenuItem1,
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
    miLangBat,
    miLangCpp,
    miLangCss,
    miLangDiff,
    miLangHtml,
    miLangIni,
    miLangJS,
    miLangJava,
    miLangMnh,
    miLangPascal,
    miLangPerl,
    miLangPhp,
    miLangPython,
    miLangShell,
    miLangSql,
    miLangVb,
    miLangXml,
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
    miLangTxt,
    submenuEditorAppearance:   TMenuItem;
    miFileHistoryRoot: TMenuItem;
    miHtmlExport: TMenuItem;
    OpenDialog:                TOpenDialog;
    inputPageControl,
    outputPageControl:         TPageControl;
    Panel1,
    Panel2:                    TPanel;
    EditorPopupMenu:           TPopupMenu;
    ReplaceDialog:             TReplaceDialog;
    SaveDialog:                TSaveDialog;
    Splitter1,
    Splitter2,
    Splitter3:                 TSplitter;
    StatusBar:                 TStatusBar;
    callStackInfoStringGrid:   TStringGrid;
    SynBatSyn1:                TSynBatSyn;
    SynCompletion:             TSynCompletion;
    SynCppSyn1:                TSynCppSyn;
    SynCssSyn1:                TSynCssSyn;
    SynDiffSyn1:               TSynDiffSyn;
    OutputEdit:                TSynEdit;
    assistanceSynEdit:         TSynEdit;
    SynExporterHTML:           TSynExporterHTML;
    SynFreePascalSyn1:         TSynFreePascalSyn;
    SynGutterMarks0:           TSynGutterMarks;
    SynHTMLSyn1:               TSynHTMLSyn;
    SynIniSyn1:                TSynIniSyn;
    SynJScriptSyn1:            TSynJScriptSyn;
    SynJavaSyn1:               TSynJavaSyn;
    helpPopupMemo,
    currentExpressionMemo:     TSynMemo;
    SynPHPSyn1:                TSynPHPSyn;
    SynPerlSyn1:               TSynPerlSyn;
    SynPythonSyn1:             TSynPythonSyn;
    SynSQLSyn1:                TSynSQLSyn;
    SynUNIXShellScriptSyn1:    TSynUNIXShellScriptSyn;
    SynVBSyn1:                 TSynVBSyn;
    SynXMLSyn1:                TSynXMLSyn;
    assistanceTabSheet:        TTabSheet;
    debugTabSheet:             TTabSheet;
    outputTabSheet:            TTabSheet;
    UpdateTimeTimer:           TTimer;
    DebugToolbar:              TToolBar;
    tbMicroStep,
    tbRun,
    tbStep,
    tbStepIn,
    tbStepOut,
    tbStop:                    TToolButton;
    variablesTreeView:         TTreeView;
    {$i mnh_gui_main_events.pas}
    PROCEDURE tbStepInClick(Sender: TObject);
    PROCEDURE tbStepClick(Sender: TObject);
    PROCEDURE tbStepOutClick(Sender: TObject);
    PROCEDURE tbStopClick(Sender: TObject);
    PROCEDURE tbMicroStepClick(Sender: TObject);
    PROCEDURE variablesTreeViewExpanding(Sender: TObject; node: TTreeNode; VAR AllowExpansion: boolean);
    PROCEDURE callStackListSelectionChange(Sender: TObject; User: boolean);
    PROCEDURE handleBreak;
    PROCEDURE updateDebugParts;
    PROCEDURE updateExpressionMemo;
    PROCEDURE miDebugClick(Sender: TObject);
    PROCEDURE miRunCustomUtilScript(Sender: TObject);
    PROCEDURE doConditionalPlotReset;
    PROCEDURE processSettings;
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miDeclarationEchoClick(Sender: TObject);
    PROCEDURE miExpressionEchoClick(Sender: TObject);
    PROCEDURE miExpressionResultClick(Sender: TObject);
    PROCEDURE _setErrorlevel_(CONST i: byte);
    PROCEDURE miMinErrorlevel1Click(Sender: TObject);
    PROCEDURE miMinErrorlevel2Click(Sender: TObject);
    PROCEDURE miMinErrorlevel3Click(Sender: TObject);
    PROCEDURE miMinErrorlevel4Click(Sender: TObject);
    PROCEDURE miTimingInfoClick(Sender: TObject);
    PROCEDURE miWrapEchoClick(Sender: TObject);
    PROCEDURE mi_settingsClick(Sender: TObject);
    PROCEDURE miEditGuiScriptsClick(Sender: TObject);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
    PROCEDURE miClearClick(Sender: TObject);
    PROCEDURE miCloseClick(Sender: TObject);
    PROCEDURE miCloseAllButCurrentClick(Sender: TObject);
    PROCEDURE miCloseAllUnmodifiedClick(Sender: TObject);
    PROCEDURE openFromHistory(CONST historyIdx:byte);
    PROCEDURE miReloadClick(Sender: TObject);
    PROCEDURE miFileHistory0Click(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miSaveAsClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);

    PROCEDURE inputEditReposition(CONST caret:TPoint; CONST doJump,updateMarker:boolean);
    PROCEDURE outputEditReposition(CONST caret:TPoint; CONST doJump:boolean);
    PROCEDURE assistanceEditReposition(CONST caret:TPoint; CONST doJump:boolean);
    FUNCTION _doSaveAs_(CONST index:longint):boolean;
    FUNCTION _doSave_(CONST index:longint):boolean;
    FUNCTION addEditorMetaForNewFile(CONST newFileName: ansistring=''):longint;
    FUNCTION addOrGetEditorMetaForFile(CONST fileName: ansistring):longint;
    FUNCTION editForSearch(CONST replacing:boolean):TSynEdit;

    PROCEDURE pmiOpenFile(CONST idOrName:string);
    PROCEDURE pmiOpenFile1Click(Sender: TObject);
    PROCEDURE pmiOpenFile2Click(Sender: TObject);

    PROCEDURE InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE assistanceSynEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE OutputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);

    PROCEDURE InputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE assistanceSynEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE OutputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);

    PROCEDURE assistanceSynEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE OutputEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);

    PROCEDURE miFindClick(Sender: TObject);
    PROCEDURE miReplaceClick(Sender: TObject);
    PROCEDURE FindDialogFind(Sender: TObject);
    PROCEDURE ReplaceDialogReplace(Sender: TObject);
    PROCEDURE ReplaceDialogFind(Sender: TObject);
    PROCEDURE miFindNextClick(Sender: TObject);
    PROCEDURE miFindPreviousClick(Sender: TObject);
    PROCEDURE miGotoClick(Sender: TObject);

    PROCEDURE ensureWordsInEditorForCompletion;
    PROCEDURE SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    PROCEDURE SynCompletionExecute(Sender: TObject);
    PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);


    PROCEDURE InputEditProcessUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);

    PROCEDURE EditorPopupMenuPopup(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE miFullscreenClick(Sender: TObject);
    PROCEDURE miAboutClick(Sender: TObject);
    PROCEDURE miHelpClick(Sender: TObject);
    PROCEDURE miHelpExternallyClick(Sender: TObject);
    PROCEDURE miLangMnhClick(Sender: TObject);
    PROCEDURE miProfileClick(Sender: TObject);
    PROCEDURE inputPageControlChange(Sender: TObject);
    PROCEDURE Splitter1Moved(Sender: TObject);
    PROCEDURE Splitter3Moved(Sender: TObject);
    PROCEDURE UpdateTimeTimerTimer(Sender: TObject);
    PROCEDURE miOpenDemoClick(Sender: TObject);
    PROCEDURE miNewCentralPackageClick(Sender: TObject);
    PROCEDURE InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);

    PROCEDURE onEndOfEvaluation; override;
  private
    uniqueEditorInstanceIpcServer: TSimpleIPCServer;
    outputHighlighter,debugHighlighter,helpHighlighter:TSynMnhSyn;
    underCursor:T_tokenInfo;
    settingsReady:boolean;
    popupFile:array[1..2] of string;

    outputFocusedOnFind:boolean;
    forceInputEditFocusOnOutputEditMouseUp:boolean;

    doNotMarkWordBefore:double;
    doNotCheckFileBefore:double;
    breakPointHandlingPending:boolean;
    debugLine:record
      editor:TSynEdit;
      line:longint;
    end;
    lastWordsCaret:longint;
    wordsInEditor:T_setOfString;
    lastReportedRunnerInfo:T_runnerStateInfo;

    scriptMenuItems:array[T_scriptType] of array of TMenuItem;
    historyMenuItems:array of TMenuItem;
    PROCEDURE positionHelpNotifier;
    PROCEDURE setUnderCursor(CONST wordText:ansistring; CONST updateMarker,forJump:boolean);
    FUNCTION hasEditor:boolean;
    FUNCTION getEditor:P_editorMeta;

  public
    editorMeta:array of P_editorMeta;
    PROCEDURE updateMainMenuItems(CONST includeHistory,includeScriptMenu:boolean);
  end;

VAR MnhForm: TMnhForm;

{$undef includeInterface}
IMPLEMENTATION
VAR closeGuiFlag:boolean=false;
{$R *.lfm}
{$define includeImplementation}
{$include guiEditorInterface.inc}
{$include editorMeta.inc}

{$i runnerLogic.inc}
{$i mnh_gui_main_events.pas}


PROCEDURE TMnhForm.tbStepInClick(Sender: TObject);
  begin
    runEvaluator.context.stepper^.doStepInto;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.tbStepClick(Sender: TObject);
  begin
    runEvaluator.context.stepper^.doStep;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.tbStepOutClick(Sender: TObject);
  begin
    runEvaluator.context.stepper^.doStepOut;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.tbStopClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.tbMicroStepClick(Sender: TObject);
  begin
    runEvaluator.context.stepper^.doMicrostep;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

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

PROCEDURE TMnhForm.callStackListSelectionChange(Sender: TObject; User: boolean);
  VAR snapshot:T_debuggingSnapshot;
      stackIdx:longint;
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
    if not(runEvaluator.context.isPaused and runEvaluator.evaluationRunning) then exit;
    snapshot:=runEvaluator.context.stepper^.getDebuggingSnapshot;
    stackIdx:=snapshot.callStack^.size-1-callStackList.ItemIndex;
    variablesTreeView.items.clear;

    if (stackIdx>=0) and (stackIdx<snapshot.callStack^.size) then begin
      callStackInfoStringGrid.Cells[1,0]:=ansistring(snapshot.callStack^[stackIdx].calleeLocation);
      callStackInfoStringGrid.Cells[1,1]:=snapshot.callStack^[stackIdx].calleeId;
      callStackInfoStringGrid.Cells[1,2]:=ansistring(snapshot.callStack^[stackIdx].calleeLocation);
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

PROCEDURE TMnhForm.handleBreak;
  VAR snapshot:T_debuggingSnapshot;

  PROCEDURE jumpToFile;
    VAR pageIdx:longint;
        newCaret:TPoint;
    begin
      debugLine.line:=-1;
      if (snapshot.location.package=nil) then exit;
      pageIdx:=addOrGetEditorMetaForFile(snapshot.location.package^.getPath);
      if pageIdx>=0 then begin
        inputPageControl.activePageIndex:=pageIdx;
        newCaret.x:=snapshot.location.column;
        newCaret.y:=snapshot.location.line;
        editorMeta[pageIdx]^.editor.CaretXY:=newCaret;
        debugLine.editor:=editorMeta[pageIdx]^.editor;
        debugLine.line:=newCaret.y;
        editorMeta[pageIdx]^.editor.Repaint;
      end;
    end;

  VAR i:longint;
  begin
    if not(runEvaluator.context.isPaused) then exit;
    updateDebugParts;

    snapshot:=runEvaluator.context.stepper^.getDebuggingSnapshot;

    breakPointHandlingPending:=false;
    jumpToFile;

    variablesTreeView.items.clear;
    for i:=0 to callStackInfoStringGrid.RowCount-1 do callStackInfoStringGrid.Cells[1,i]:='';

    callStackList.items.clear;
    for i:=snapshot.callStack^.size-1 downto 0 do
    callStackList.items.add(snapshot.callStack^[i].calleeId);
    callStackList.ItemIndex:=0;
    updateExpressionMemo;

    outputPageControl.activePage:=debugTabSheet;
  end;

PROCEDURE TMnhForm.updateExpressionMemo;
  VAR lines,chars:longint;
      snapshot:T_debuggingSnapshot;
      tokens:T_arrayOfString;
      txt:ansistring;
      k:longint=0;
      firstInLine:boolean;
  begin
    if not(runEvaluator.context.isPaused and runEvaluator.evaluationRunning) then exit;
    snapshot:=runEvaluator.context.stepper^.getDebuggingSnapshot;
    lines:=currentExpressionMemo.LinesInWindow;
    chars:=currentExpressionMemo.charsInWindow;
    if (lines*chars<50) then begin
      lines:=1;
      chars:=50;
    end;

    currentExpressionMemo.lines.clear;
    tokens:=tokenSplit(snapshot.tokenStack^.toString(snapshot.first,round(lines*chars*0.9)));

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

PROCEDURE TMnhForm.updateDebugParts;
  PROCEDURE handleButton(VAR button:TToolButton; CONST enabled:boolean; CONST enabledImageIndex:longint; CONST enableAlways:boolean=false);
    begin
      button.enabled:=enabled or enableAlways;
      if enabled then button.ImageIndex:=enabledImageIndex
                 else button.ImageIndex:=enabledImageIndex+1;
    end;

  VAR i:longint;
      isPaused:boolean;
      isRunning:boolean;
  begin
    if miDebug.Checked then begin
      debugTabSheet.visible:=true;
      debugTabSheet.tabVisible:=true;
      for i:=0 to length(editorMeta)-1 do with editorMeta[i]^ do if language=LANG_MNH then editor.Gutter.MarksPart.visible:=true;
      DebugToolbar.visible:=true;
      DebugToolbar.enabled:=true;
      DebugToolbar.top:=0;
      isPaused:=runEvaluator.context.isPaused;
      isRunning:=runEvaluator.evaluationRunning;
      handleButton(tbStop     ,    isRunning             ,2);
      handleButton(tbRun      ,not(isRunning) or isPaused,0,true);
      handleButton(tbStep     ,    isRunning and isPaused,4);
      handleButton(tbStepIn   ,    isRunning and isPaused,6);
      handleButton(tbStepOut  ,    isRunning and isPaused,8);
      handleButton(tbMicroStep,    isRunning and isPaused,10);
    end else begin
      debugTabSheet.visible:=false;
      debugTabSheet.tabVisible:=false;
      for i:=0 to length(editorMeta)-1 do editorMeta[i]^.editor.Gutter.MarksPart.visible:=false;
      outputPageControl.activePage:=outputTabSheet;
      DebugToolbar.visible:=false;
      DebugToolbar.enabled:=false;
    end;
  end;

PROCEDURE TMnhForm.miDebugClick(Sender: TObject);
  begin
    miDebug.Checked:=not(miDebug.Checked);
    updateDebugParts;
  end;


PROCEDURE TMnhForm.miRunCustomUtilScript(Sender: TObject);
  VAR i,k:longint;
  begin
    k:=inputPageControl.activePageIndex;
    if (k<0) or (k>=length(editorMeta)) then exit;
    for i:=0 to length(editorMeta)-1 do editorMeta[i]^.editor.readonly:=true;
    with editorMeta[k]^ do runEvaluator.runUtilScript(TMenuItem(Sender).Tag,k,editor.lines,languageName,pseudoName());
  end;

PROCEDURE TMnhForm.doConditionalPlotReset;
  begin
    if settings.value^.doResetPlotOnEvaluation then begin
      guiAdapters.plot^.setDefaults;
      if plotFormIsInitialized then plotForm.pullPlotSettingsToGui();
    end;
  end;

PROCEDURE TMnhForm._setErrorlevel_(CONST i: byte);
  VAR j:longint;
  begin
    if settingsReady then begin
      case i of
        1: miMinErrorlevel1.Checked:=true;
        2: miMinErrorlevel2.Checked:=true;
        3: miMinErrorlevel3.Checked:=true;
        4: miMinErrorlevel4.Checked:=true;
      end;
      for j:=1 to 4 do guiOutAdapter.enableMessageType(j>=i,C_errorMessageTypes[j]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miMinErrorlevel1Click(Sender: TObject); begin _setErrorlevel_(1); end;
PROCEDURE TMnhForm.miMinErrorlevel2Click(Sender: TObject); begin _setErrorlevel_(2); end;
PROCEDURE TMnhForm.miMinErrorlevel3Click(Sender: TObject); begin _setErrorlevel_(3); end;
PROCEDURE TMnhForm.miMinErrorlevel4Click(Sender: TObject); begin _setErrorlevel_(4); end;

PROCEDURE TMnhForm.processSettings;
  VAR formPosition:T_formPosition;
      i,j:longint;
      maxLevel:longint=5;
  begin
    if not(settingsReady) then begin
      if settings.value^.loaded then begin
        formPosition:=settings.value^.mainForm;
        top   :=formPosition.top;
        Left  :=formPosition.Left;
        width :=formPosition.width;
        height:=formPosition.height;
        if formPosition.isFullscreen then WindowState:=wsMaximized;
        outputPageControl.height:=round(ClientHeight*formPosition.relativeSplitterPosition);
        StatusBar.top:=outputPageControl.top+outputPageControl.height;
      end else settings.value^.mainForm.relativeSplitterPosition:=outputPageControl.height/ClientHeight;

      miDeclarationEcho .Checked:=mt_echo_declaration in settings.value^.outputBehaviour;
      miExpressionEcho  .Checked:=mt_echo_input       in settings.value^.outputBehaviour;
      miExpressionResult.Checked:=mt_echo_output      in settings.value^.outputBehaviour;
      miTimingInfo      .Checked:=mt_timing_info      in settings.value^.outputBehaviour;;
      miWrapEcho.Checked:=settings.value^.wordWrapEcho;
      for j:=4 downto 1 do if (C_errorMessageTypes[j] * settings.value^.outputBehaviour <> []) then maxLevel:=j;
      miMinErrorlevel1.Checked:=maxLevel=1;
      miMinErrorlevel2.Checked:=maxLevel=2;
      miMinErrorlevel3.Checked:=maxLevel=3;
      miMinErrorlevel4.Checked:=maxLevel>=4;
      guiOutAdapter.outputBehavior:=settings.value^.outputBehaviour;

      setupOutputBehaviourFromCommandLineOptions(guiAdapters,nil);

      SettingsForm.ensureFont(OutputEdit.Font);

      setLength(editorMeta,length(settings.value^.workspace.editorState));
      runEvaluator.context.stepper^.clearBreakpoints;
      for i:=0 to length(editorMeta)-1 do begin
        new(editorMeta[i],create(i,settings.value^.workspace.editorState[i]));
        editorMeta[i]^.setStepperBreakpoints;
      end;
      i:=settings.value^.workspace.activePage;
      inputPageControl.activePageIndex:=i;

      for i:=0 to length(filesToOpenInEditor)-1 do FormDropFiles(nil,filesToOpenInEditor[i]);

      if (inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta)) then SynCompletion.editor:=getEditor^.editor;
      {$ifdef UNIX}
      miIncFontSize.ShortCut:=16605;
      {$endif}

      settingsReady:=true;
      if (inputPageControl.activePageIndex>=0) and
         (inputPageControl.activePageIndex<length(editorMeta)) and
         (getEditor^.language=LANG_MNH)
      then assistancEvaluator.evaluate((editorMeta[inputPageControl.activePageIndex]));
    end;

    OutputEdit.Font.name:=settings.value^.editorFontname;
    OutputEdit.Font.size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then OutputEdit.Font.quality:=fqCleartypeNatural
    else OutputEdit.Font.quality:=fqNonAntialiased;
    for i:=0 to length(editorMeta)-1 do editorMeta[i]^.editor.Font:=OutputEdit.Font;
    if settings.value^.wordWrapEcho
    then guiAdapters.preferredEchoLineLength:=OutputEdit.charsInWindow-6
    else guiAdapters.preferredEchoLineLength:=-1;

    currentExpressionMemo.Font:=OutputEdit.Font;
    assistanceSynEdit.Font:=OutputEdit.Font;
    helpPopupMemo.Font:=OutputEdit.Font;
    helpPopupMemo.Font.size:=helpPopupMemo.Font.size-2;
  end;

PROCEDURE TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin
    if settingsReady then begin
      SettingsForm.fontSize:=SettingsForm.fontSize-1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin
    if settingsReady then begin
      SettingsForm.fontSize:=SettingsForm.fontSize+1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm.miDeclarationEchoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miDeclarationEcho.Checked:=not(miDeclarationEcho.Checked);
      guiOutAdapter.enableMessageType(miDeclarationEcho.Checked,[mt_echo_declaration]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miExpressionEchoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miExpressionEcho.Checked:=not(miExpressionEcho.Checked);
      guiOutAdapter.enableMessageType(miExpressionEcho.Checked,[mt_echo_input]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miExpressionResultClick(Sender: TObject);
  begin
    if settingsReady then begin
      miExpressionResult.Checked:=not(miExpressionResult.Checked);
      guiOutAdapter.enableMessageType(miExpressionResult.Checked,[mt_echo_output]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miTimingInfoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miTimingInfo.Checked:=not(miTimingInfo.Checked);
      guiOutAdapter.enableMessageType(miTimingInfo.Checked,[mt_timing_info]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miWrapEchoClick(Sender: TObject);
  begin
    settings.value^.wordWrapEcho:=not(settings.value^.wordWrapEcho);
    miWrapEcho.Checked:=settings.value^.wordWrapEcho;
    processSettings;
  end;

PROCEDURE TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;

PROCEDURE TMnhForm.miEditGuiScriptsClick(Sender: TObject);
  begin
    FormDropFiles(Sender,utilityScriptFileName);
  end;

PROCEDURE TMnhForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  VAR i:longint;
  begin
    for i:=0 to length(FileNames)-1 do inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(FileNames[i]);
    BringToFront;
  end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  begin
    inputPageControl.activePageIndex:=addEditorMetaForNewFile();
  end;

PROCEDURE TMnhForm.miCloseClick(Sender: TObject);
  VAR i,mr:longint;
  begin
    if (inputPageControl.activePageIndex<0) or (inputPageControl.activePageIndex>=length(editorMeta)) then exit;
    with getEditor^ do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(inputPageControl.activePageIndex)) then exit;
        if mr=mrCancel then exit;
      end;
      if isFile then begin
        settings.value^.workspace.fileHistory.fileClosed(fileInfo.filePath);
      end;
      closeEditor;
    end;

    mr:=-1;
    for i:=0 to length(editorMeta)-1 do if editorMeta[i]^.sheet.tabVisible then mr:=i;
    if mr=-1 then inputPageControl.activePageIndex:=addEditorMetaForNewFile()
             else inputPageControl.activePageIndex:=mr;
  end;

PROCEDURE TMnhForm.miCloseAllButCurrentClick(Sender: TObject);
  VAR mr,pageIdx:longint;
  begin
    if not(hasEditor) then exit;
    for pageIdx:=0 to length(editorMeta)-1 do if (pageIdx<>inputPageControl.activePageIndex) and (editorMeta[pageIdx]^.sheet.tabVisible) then
    with editorMeta[pageIdx]^ do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(pageIdx)) then exit;
        if mr=mrCancel then exit;
      end;
      if isFile then begin
        settings.value^.workspace.fileHistory.fileClosed(fileInfo.filePath);
      end;
      closeEditor;
    end;
  end;

PROCEDURE TMnhForm.miCloseAllUnmodifiedClick(Sender: TObject);
  VAR i,pageIdx:longint;
      activePageClosed:boolean=false;
  begin
    if not(hasEditor) then exit;
    for pageIdx:=0 to length(editorMeta)-1 do if (pageIdx<>inputPageControl.activePageIndex) and (editorMeta[pageIdx]^.sheet.tabVisible) and not(editorMeta[pageIdx]^.changed) then
    with editorMeta[pageIdx]^ do begin
      if isFile then begin
        settings.value^.workspace.fileHistory.fileClosed(fileInfo.filePath);
      end;
      closeEditor;
      activePageClosed:=activePageClosed or (pageIdx=inputPageControl.activePageIndex);
    end;
    if activePageClosed then begin
      pageIdx:=-1;
      for i:=0 to length(editorMeta)-1 do if editorMeta[i]^.sheet.tabVisible then pageIdx:=i;
      if pageIdx=-1 then inputPageControl.activePageIndex:=addEditorMetaForNewFile()
                    else inputPageControl.activePageIndex:=pageIdx;
    end;
  end;

PROCEDURE TMnhForm.openFromHistory(CONST historyIdx: byte);
  begin
    with settings.value^.workspace.fileHistory do begin
      if fileExists(historyItem(historyIdx))
      then inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(historyItem(historyIdx))
      else if polishHistory then updateMainMenuItems(true,false);
    end;
  end;

PROCEDURE TMnhForm.miReloadClick(Sender: TObject);
  begin
    if hasEditor then with getEditor^ do if sheet.visible then reloadFile(getPath);
  end;

PROCEDURE TMnhForm.miFileHistory0Click(Sender: TObject); begin openFromHistory(TMenuItem(Sender).Tag); end;

PROCEDURE TMnhForm.inputEditReposition(CONST caret: TPoint; CONST doJump,updateMarker: boolean);
  VAR wordUnderCursor:string;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    with getEditor^ do begin
      wordUnderCursor:=editor.GetWordAtRowCol(caret);
      setUnderCursor(wordUnderCursor,updateMarker,doJump);
      if not(doJump) then exit;
      if (underCursor.tokenText<>wordUnderCursor) or
         (underCursor.location.column<=0) then exit;
      if (underCursor.location.fileName='') or (underCursor.location.fileName='?') then exit;
      pageIdx:=addOrGetEditorMetaForFile(underCursor.location.fileName);
      if pageIdx>=0 then begin
        inputPageControl.activePageIndex:=pageIdx;
        newCaret.x:=underCursor.location.column;
        newCaret.y:=underCursor.location.line;
        editorMeta[pageIdx]^.editor.CaretXY:=newCaret;
      end;
    end;
  end;

PROCEDURE TMnhForm.outputEditReposition(CONST caret: TPoint; CONST doJump: boolean);
  VAR loc:T_searchTokenLocation;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(OutputEdit.GetWordAtRowCol(caret),true,doJump);
    loc:=guessLocationFromString(OutputEdit.lines[caret.y-1],false);
    if not(doJump) then exit;
    if (loc.fileName='') or (loc.fileName='?') then exit;
    pageIdx:=addOrGetEditorMetaForFile(loc.fileName);
    if pageIdx<0 then exit;
    inputPageControl.activePageIndex:=pageIdx;
    with editorMeta[pageIdx]^ do begin
      editor.SetFocus;
      highlighter.setMarkedToken(loc.line-1,loc.column-1);
      newCaret.x:=loc.column;
      newCaret.y:=loc.line;
      editor.CaretXY:=newCaret;
      forceInputEditFocusOnOutputEditMouseUp:=true;
    end;
  end;

PROCEDURE TMnhForm.assistanceEditReposition(CONST caret: TPoint; CONST doJump: boolean);
  VAR loc:T_searchTokenLocation;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(assistanceSynEdit.GetWordAtRowCol(caret),true,doJump);
    loc:=guessLocationFromString(assistanceSynEdit.lines[caret.y-1],false);
    if not(doJump) then exit;
    if (loc.fileName='') or (loc.fileName='?') then exit;
    pageIdx:=addOrGetEditorMetaForFile(loc.fileName);
    if pageIdx<0 then exit;
    inputPageControl.activePageIndex:=pageIdx;
    with editorMeta[pageIdx]^ do begin
      editor.SetFocus;
      highlighter.setMarkedToken(loc.line-1,loc.column-1);
      newCaret.x:=loc.column;
      newCaret.y:=loc.line;
      editor.CaretXY:=newCaret;
      forceInputEditFocusOnOutputEditMouseUp:=true;
    end;
  end;

FUNCTION TMnhForm._doSaveAs_(CONST index: longint): boolean;
  begin
    if index<0 then exit(false);
    if editorMeta[index]^.language=LANG_MNH
    then begin
      SaveDialog.FilterIndex:=1;
      SaveDialog.options:=SaveDialog.options+[ofExtensionDifferent];
    end else begin
      SaveDialog.FilterIndex:=2;
      SaveDialog.options:=SaveDialog.options-[ofExtensionDifferent];
    end;
    if SaveDialog.execute then with editorMeta[index]^ do begin
      caption:=saveFile(SaveDialog.fileName);
      result:=true;
    end else result:=false;
  end;

FUNCTION TMnhForm._doSave_(CONST index: longint): boolean;
  begin
    if index<0 then exit(false);
    with editorMeta[index]^ do if not(isFile) then result:=_doSaveAs_(index)
    else begin
      caption:=saveFile();
      result:=true;
    end;
  end;

FUNCTION TMnhForm.addEditorMetaForNewFile(CONST newFileName: ansistring): longint;
  VAR i:longint;
  begin
    i:=length(editorMeta)-1;
    //decrease i until a visible meta is encountered
    while (i>=0) and not(editorMeta[i]^.sheet.tabVisible) do dec(i);
    inc(i);
    //i now is the index of the last visible editor meta +1
    if (i>=0) and (i<length(editorMeta)) then begin
      editorMeta[i]^.initForNewFile;
      exit(i);
    end;

    i:=length(editorMeta);
    setLength(editorMeta,i+1);
    new(editorMeta[i],create(i));
    editorMeta[i]^.editor.Font:=OutputEdit.Font;
    if newFileName<>'' then _doSave_(i);
    result:=i;
    if miDebug.Checked then editorMeta[i]^.editor.Gutter.MarksPart.visible:=true;
  end;

FUNCTION TMnhForm.addOrGetEditorMetaForFile(CONST fileName: ansistring): longint;
  FUNCTION isPseudoName:boolean;
    begin
      result:=(length(fileName)>1)
          and (fileName[1]='<')
          and (fileName[length(fileName)]='>');
    end;

  VAR filePath:ansistring;
      i:longint;
  begin
    if isPseudoName then begin
      for i:=0 to length(editorMeta)-1 do if (editorMeta[i]^.sheet.tabVisible) and (editorMeta[i]^.pseudoName=fileName) then exit(i);
      result:=-1;
    end else begin
      filePath:=expandFileName(fileName);
      for i:=0 to length(editorMeta)-1 do if (editorMeta[i]^.sheet.tabVisible) and (editorMeta[i]^.fileInfo.filePath=filePath) then exit(i);
      result:=addEditorMetaForNewFile();
      editorMeta[result]^.setFile(filePath);
      editorMeta[result]^.editor.Font:=OutputEdit.Font;
      updateMainMenuItems(true,false);
    end;
  end;

FUNCTION TMnhForm.editForSearch(CONST replacing: boolean): TSynEdit;
  begin
    if outputFocusedOnFind and not(replacing) then exit(OutputEdit);
    if (inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta))
    then result:=getEditor^.editor
    else exit(OutputEdit); //not nice, but a valid fallback
  end;

PROCEDURE TMnhForm.miOpenClick(Sender: TObject);
  begin
    OpenDialog.FilterIndex:=1;
    OpenDialog.options:=OpenDialog.options+[ofPathMustExist,ofFileMustExist];
    OpenDialog.title:='Open file';
    if OpenDialog.execute and fileExists(OpenDialog.fileName)
    then inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(OpenDialog.fileName);
  end;

PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    _doSaveAs_(inputPageControl.activePageIndex);
  end;

PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin
    _doSave_(inputPageControl.activePageIndex);
  end;

PROCEDURE TMnhForm.pmiOpenFile(CONST idOrName:string);
  VAR fileName:string;
  begin
    with settings.value^ do begin
      if fileExists(idOrName)
      then begin
        inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(idOrName);
        exit;
      end;
      fileName:=assistancEvaluator.resolveImport(idOrName);
      if (fileName<>'') and fileExists(fileName) then inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(fileName);
    end;
  end;

PROCEDURE TMnhForm.pmiOpenFile1Click(Sender: TObject);
  begin
    pmiOpenFile(popupFile[1]);
  end;

PROCEDURE TMnhForm.pmiOpenFile2Click(Sender: TObject);
  begin
    pmiOpenFile(popupFile[2]);
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=13) and ((ssCtrl in Shift) or (ssAlt in Shift))
    then inputEditReposition(getEditor^.editor.CaretXY,ssCtrl in Shift,true)
    else inputEditReposition(getEditor^.editor.CaretXY,false,false);
    if runEvaluator.context.isPaused then begin
      if (key=116) and tbRun      .enabled then tbRunClick(Sender);
      if (key=117) and tbStepIn   .enabled then tbStepInClick(Sender);
      if (key=118) then begin
        if (ssShift in Shift) then begin
          if tbMicroStep.enabled then tbMicroStepClick(Sender);
        end else begin
          if tbStep     .enabled then tbStepClick(Sender);
        end;
      end;
      if (key=119) and tbStepOut  .enabled then tbStepOutClick(Sender);
    end;
  end;

PROCEDURE TMnhForm.assistanceSynEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if ((key=13) and (ssCtrl in Shift)) then assistanceEditReposition(assistanceSynEdit.CaretXY,true);
    if forceInputEditFocusOnOutputEditMouseUp and (inputPageControl.activePageIndex>=0) then ActiveControl:=getEditor^.editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.OutputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if ((key=13) and (ssCtrl in Shift)) then outputEditReposition(OutputEdit.CaretXY,true);
    if forceInputEditFocusOnOutputEditMouseUp and (inputPageControl.activePageIndex>=0) then ActiveControl:=getEditor^.editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.InputEditMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    inputEditReposition(getEditor^.editor.PixelsToRowColumn(point),ssCtrl in Shift,true);
  end;

PROCEDURE TMnhForm.assistanceSynEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    assistanceEditReposition(assistanceSynEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.OutputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    outputEditReposition(OutputEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.assistanceSynEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=getEditor^.editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.OutputEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=getEditor^.editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.miFindClick(Sender: TObject);
  VAR wordUnderCursor:string;
  begin
    if OutputEdit.Focused then begin
      OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      wordUnderCursor:=OutputEdit.TextBetweenPoints[OutputEdit.BlockBegin,OutputEdit.BlockEnd];
      if wordUnderCursor='' then wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      outputFocusedOnFind:=true;
    end else begin
      with getEditor^ do begin
        wordUnderCursor:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
        if wordUnderCursor='' then wordUnderCursor:=editor.GetWordAtRowCol(editor.CaretXY);
      end;
      outputFocusedOnFind:=false;
    end;
    if wordUnderCursor<>'' then FindDialog.FindText:=wordUnderCursor;
    FindDialog.execute;
    ReplaceDialog.FindText:=FindDialog.FindText;
  end;

PROCEDURE TMnhForm.miReplaceClick(Sender: TObject);
  VAR wordUnderCursor:string;
  begin
    if OutputEdit.Focused then begin
      OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      wordUnderCursor:=OutputEdit.TextBetweenPoints[OutputEdit.BlockBegin,OutputEdit.BlockEnd];
      if wordUnderCursor='' then wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      outputFocusedOnFind:=true;
    end else begin
      with getEditor^ do begin
        wordUnderCursor:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
        if wordUnderCursor='' then wordUnderCursor:=editor.GetWordAtRowCol(editor.CaretXY);
      end;
      outputFocusedOnFind:=false;
    end;
    if wordUnderCursor<>'' then ReplaceDialog.FindText:=wordUnderCursor;
    ReplaceDialog.execute;
    FindDialog.FindText:=ReplaceDialog.FindText;
  end;

FUNCTION FindOptionsToSearchOptions (CONST FindOptions: TFindOptions): TSynSearchOptions;
  begin
    result:=[];
    if frMatchCase       in FindOptions then include(result,ssoMatchCase);
    if frWholeWord       in FindOptions then include(result,ssoWholeWord);
    if frReplace         in FindOptions then include(result,ssoReplace);
    if frReplaceAll      in FindOptions then include(result,ssoReplaceAll);
    if frHideEntireScope in FindOptions then include(result,ssoEntireScope);
    if frPromptOnReplace in FindOptions then include(result,ssoPrompt);
    if frFindNext        in FindOptions then include(result,ssoFindContinue);
    if not(frDown in FindOptions) then include(result,ssoBackwards);
  end;

PROCEDURE TMnhForm.FindDialogFind(Sender: TObject);
  begin
    editForSearch(false).SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options));
  end;

PROCEDURE TMnhForm.ReplaceDialogReplace(Sender: TObject);
  begin
    editForSearch(true).SearchReplace(ReplaceDialog.FindText,ReplaceDialog.ReplaceText,FindOptionsToSearchOptions(ReplaceDialog.options));
  end;

PROCEDURE TMnhForm.ReplaceDialogFind(Sender: TObject);
  begin
    editForSearch(true).SearchReplace(ReplaceDialog.FindText,ReplaceDialog.FindText,FindOptionsToSearchOptions(ReplaceDialog.options));
  end;

PROCEDURE TMnhForm.miFindNextClick(Sender: TObject);
  begin
    outputFocusedOnFind:=OutputEdit.Focused;
    editForSearch(false).SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options)-[ssoBackwards]);
  end;

PROCEDURE TMnhForm.miFindPreviousClick(Sender: TObject);
  begin
    outputFocusedOnFind:=OutputEdit.Focused;
    editForSearch(false).SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options)+[ssoBackwards]);
  end;

PROCEDURE TMnhForm.miGotoClick(Sender: TObject);
  VAR i:longint;
  begin
    i:=inputPageControl.activePageIndex;
    if (i<0) or (i>=length(editorMeta)) then exit;
    with editorMeta[i]^ do begin
      askForm.initWithQuestion('Go to line');
      askForm.ShowModal;
      i:=strToIntDef(askForm.getLastAnswerReleasing(nil),-1);
      if i>=1 then begin
        editor.CaretX:=1;
        editor.CaretY:=i;
      end;
    end;
  end;

PROCEDURE TMnhForm.ensureWordsInEditorForCompletion;
  VAR caret:TPoint;
      i:longint;
  begin
    with getEditor^ do caret:=editor.CaretXY;
    if (wordsInEditor.size>0) and (lastWordsCaret=caret.y) then exit;
    lastWordsCaret:=caret.y;
    wordsInEditor.clear;
    with getEditor^ do begin
      for i:=0 to editor.lines.count-1 do
        if i=caret.y-1 then collectIdentifiers(editor.lines[i],wordsInEditor,caret.x)
                       else collectIdentifiers(editor.lines[i],wordsInEditor,-1);
      if language=LANG_MNH then assistancEvaluator.extendCompletionList(wordsInEditor);
    end;
  end;

PROCEDURE TMnhForm.SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
  begin
    value:=copy(sourceValue,1,LastDelimiter('.',sourceValue)-1)+value;
    wordsInEditor.clear;
  end;

PROCEDURE TMnhForm.SynCompletionExecute(Sender: TObject);
  VAR s:string;
      w:string;
  begin
    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    for w in wordsInEditor.values do if (s='') or (pos(s,w)=1) then SynCompletion.ItemList.add(w);
  end;

PROCEDURE TMnhForm.SynCompletionSearchPosition(VAR APosition: integer);
  VAR i:longint;
      s:string;
      w:string;
  begin
    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    i:=LastDelimiter('.',s);
    if i>1 then begin
      s:=copy(s,i,length(s));
      SynCompletion.CurrentString:=s;
    end;
    for w in wordsInEditor.values do if pos(s,w)=1 then SynCompletion.ItemList.add(w);
    if SynCompletion.ItemList.count>0 then APosition:=0 else APosition:=-1;
  end;

{$undef includeImplementation}
{$i mnh_func_defines.inc}
FUNCTION editors_impl intFuncSignature;
  VAR meta:P_editorMeta;
  begin
    result:=newListLiteral();
    for meta in MnhForm.editorMeta do if meta^.sheet.tabVisible then listResult^.appendString(meta^.pseudoName());
  end;

FUNCTION editorContent_impl intFuncSignature;
  VAR meta:P_editorMeta;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      for meta in MnhForm.editorMeta do if meta^.sheet.tabVisible and (meta^.pseudoName()=str0^.value) then begin
        result:=newListLiteral(meta^.editor.lines.count);
        for i:=0 to meta^.editor.lines.count-1 do listResult^.appendString(meta^.editor.lines[i]);
        exit(result);
      end;
      result:=newVoidLiteral;
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

PROCEDURE TMnhForm.positionHelpNotifier;
  VAR maxLineLength:longint=0;
      i:longint;
      p:TPoint;
  begin
    p:=getEditor^.caretInMainFormCoordinates;

    helpPopupMemo.visible:=true;
    helpPopupMemo.Left:=p.x;
    helpPopupMemo.top :=p.y;
    for i:=0 to helpPopupMemo.lines.count-1 do if length(helpPopupMemo.lines[i])>maxLineLength then maxLineLength:=length(helpPopupMemo.lines[i]);
    if (maxLineLength=0) then begin
      helpPopupMemo.width:=0;
      helpPopupMemo.height:=0;
    end else begin
      helpPopupMemo.width:=helpPopupMemo.CharWidth*(maxLineLength+1);
      helpPopupMemo.height:=helpPopupMemo.LineHeight*(helpPopupMemo.lines.count+1);
    end;
    if helpPopupMemo.Left>width-helpPopupMemo.width then helpPopupMemo.Left:=width-helpPopupMemo.width;
    if helpPopupMemo.Left<0 then helpPopupMemo.Left:=0;
  end;

PROCEDURE TMnhForm.setUnderCursor(CONST wordText: ansistring; CONST updateMarker,forJump: boolean);
  VAR i:longint;
  begin
    if getEditor^.language<>LANG_MNH then exit;
    if updateMarker then begin
      outputHighlighter.setMarkedWord(wordText);
      for i:=0 to length(editorMeta)-1 do editorMeta[i]^.setMarkedWord(wordText);
    end;
    if miHelp.Checked or forJump then with getEditor^.editor do
      assistancEvaluator.explainIdentifier(lines[CaretY-1],CaretY,CaretX,underCursor);
    if miHelp.Checked then begin
      helpPopupMemo.text:=ECHO_MARKER+underCursor.tokenText+C_lineBreakChar+underCursor.tokenExplanation;
      positionHelpNotifier;
    end;
  end;

FUNCTION TMnhForm.hasEditor: boolean;
  begin
    result:=(inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta));
  end;

FUNCTION TMnhForm.getEditor: P_editorMeta;
  begin
    result:=editorMeta[inputPageControl.activePageIndex];
  end;

PROCEDURE TMnhForm.FormCreate(Sender: TObject);
  PROCEDURE initFileTypes;
    PROCEDURE addFileType(CONST extension:string; CONST language:byte; CONST highlighter:TSynCustomHighlighter; CONST menuItem:TMenuItem);
      begin
        setLength(fileTypeMeta,length(fileTypeMeta)+1);
        fileTypeMeta[length(fileTypeMeta)-1].extensionWithoutDot:=uppercase(extension);
        fileTypeMeta[length(fileTypeMeta)-1].language   :=language;
        fileTypeMeta[length(fileTypeMeta)-1].highlighter:=highlighter;
        fileTypeMeta[length(fileTypeMeta)-1].menuItem   :=menuItem;
      end;

    begin
      addFileType('mnh',LANG_MNH,nil,miLangMnh);
      addFileType('cpp',LANG_CPP,SynCppSyn1,miLangCpp);
      addFileType('c',LANG_CPP,SynCppSyn1,miLangCpp);
      addFileType('h',LANG_CPP,SynCppSyn1,miLangCpp);
      addFileType('hh',LANG_CPP,SynCppSyn1,miLangCpp);
      addFileType('css',LANG_CSS,SynCssSyn1,miLangCss);
      addFileType('diff',LANG_DIFF,SynDiffSyn1,miLangDiff);
      addFileType('html',LANG_HTML,SynHTMLSyn1,miLangHtml);
      addFileType('ini',LANG_INI,SynIniSyn1,miLangIni);
      addFileType('java',LANG_JAVA,SynJavaSyn1,miLangJava);
      addFileType('js',LANG_JS,SynJScriptSyn1,miLangJS);
      addFileType('json',LANG_JS,SynJScriptSyn1,miLangJS);
      addFileType('pas',LANG_PAS,SynFreePascalSyn1,miLangPascal);
      addFileType('perl',LANG_PERL,SynPerlSyn1,miLangPerl);
      addFileType('php',LANG_PHP,SynPHPSyn1,miLangPhp);
      addFileType('py',LANG_PYTHON,SynPythonSyn1,miLangPython);
      addFileType('sh',LANG_SHELL,SynUNIXShellScriptSyn1,miLangShell);
      addFileType('sql',LANG_SQL,SynSQLSyn1,miLangSql);
      addFileType('vb',LANG_VB,SynVBSyn1,miLangVb);
      addFileType('bat',LANG_BAT,SynBatSyn1,miLangBat);
      addFileType('xml',LANG_XML,SynXMLSyn1,miLangXml);
      addFileType('txt',LANG_TXT,nil,miLangTxt);
    end;

  PROCEDURE initHighlighters;
    begin
      SynBatSyn1            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynCppSyn1            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynCssSyn1            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynFreePascalSyn1     .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynIniSyn1            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynJavaSyn1           .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynJScriptSyn1        .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynPerlSyn1           .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynPHPSyn1            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynPythonSyn1         .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynSQLSyn1            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynUNIXShellScriptSyn1.NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynVBSyn1             .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynCppSyn1            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynCssSyn1            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynFreePascalSyn1     .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynHTMLSyn1           .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynIniSyn1            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynJavaSyn1           .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynJScriptSyn1        .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynPerlSyn1           .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynPHPSyn1            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynPythonSyn1         .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynSQLSyn1            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynUNIXShellScriptSyn1.SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynVBSyn1             .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynXMLSyn1            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynBatSyn1            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynCppSyn1            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynCssSyn1            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynFreePascalSyn1     .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynHTMLSyn1           .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynIniSyn1            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynJavaSyn1           .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynJScriptSyn1        .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynPerlSyn1           .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynPHPSyn1            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynPythonSyn1         .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynSQLSyn1            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynUNIXShellScriptSyn1.KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynVBSyn1             .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynBatSyn1            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynCppSyn1            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynCssSyn1            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynFreePascalSyn1     .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynHTMLSyn1           .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynIniSyn1            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynJavaSyn1           .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynJScriptSyn1        .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynPerlSyn1           .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynPHPSyn1            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynPythonSyn1         .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynSQLSyn1            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynUNIXShellScriptSyn1.CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynVBSyn1             .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynXMLSyn1            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynCppSyn1            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynCssSyn1            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynFreePascalSyn1     .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynIniSyn1            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynJavaSyn1           .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynJScriptSyn1        .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynPerlSyn1           .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynPHPSyn1            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynPythonSyn1         .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynSQLSyn1            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynUNIXShellScriptSyn1.StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynVBSyn1             .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
    end;

  PROCEDURE initIpcServer;
    begin
      uniqueEditorInstanceIpcServer:=TSimpleIPCServer.create(self);
      uniqueEditorInstanceIpcServer.serverId:=UNIQUE_EDITOR_IPC_ID;
      uniqueEditorInstanceIpcServer.Global:=true;
      uniqueEditorInstanceIpcServer.StartServer;
    end;

  PROCEDURE updateRules;
    begin
      reregisterRule(SYSTEM_BUILTIN_NAMESPACE,'ask',@ask_impl);
      registerRule(GUI_NAMESPACE,'editors'       ,@editors_impl      ,false,ak_nullary,'editors(...);//Lists all editors');
      registerRule(GUI_NAMESPACE,'editorContent' ,@editorContent_impl,false,ak_unary,'editorContent(name:string);//Returns the content of the given editor as a string or void if no such editor was found.');
      registerRule(GUI_NAMESPACE,'openInEditor'  ,@openInEditor_impl ,false,ak_unary,'openInEditor(filename:string);//opens an editor tab for the given file');
    end;

  begin
    setLength(scriptMenuItems[st_edit],0);
    setLength(scriptMenuItems[st_insert],0);
    setLength(scriptMenuItems[st_util],0);
    setLength(historyMenuItems,0);

    updateRules;
    splashOnStartup;
    initIpcServer;
    initGuiOutAdapters(MnhForm,true);
    guiTaskQueue.create;
    SynHighlighterMnh.initLists;
    mnh_evalThread.initUnit(@guiAdapters,true);
    setupCallbacks;

    setLength(editorMeta,0);
    initFileTypes;
    registerForm(self,true,true);
    lastWordsCaret:=maxLongint;
    wordsInEditor.create;
    forceInputEditFocusOnOutputEditMouseUp:=false;
    settingsReady:=false;
    lastStart.mainCall:=false;
    doNotMarkWordBefore:=now;
    doNotCheckFileBefore:=now+ONE_SECOND;

    outputHighlighter:=TSynMnhSyn.create(nil,msf_output);
    OutputEdit.highlighter:=outputHighlighter;
    assistanceSynEdit.highlighter:=outputHighlighter;

    helpHighlighter:=TSynMnhSyn.create(nil,msf_help);
    helpPopupMemo.highlighter:=helpHighlighter;

    debugHighlighter:=TSynMnhSyn.create(nil,msf_input);
    currentExpressionMemo.highlighter:=debugHighlighter;

    initHighlighters;
    OutputEdit.clearAll;
    assistanceSynEdit.clearAll;
    {$ifdef debugMode}
    if wantConsoleAdapter then guiAdapters.addConsoleOutAdapter^.enableMessageType(false,[mt_clearConsole]);
    {$endif}
    mnh_out_adapters.gui_started:=true;
    runEvaluator.ensureEditScripts;
    updateDebugParts;
  end;

PROCEDURE TMnhForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  VAR i:integer;
  begin
    if runEvaluator      .evaluationRunning then runEvaluator      .haltEvaluation;
    if assistancEvaluator.evaluationRunning then assistancEvaluator.haltEvaluation;
    for i:=0 to length(editorMeta)-1 do editorMeta[i]^.writeToEditorState(settings.value);
  end;

PROCEDURE TMnhForm.EditorPopupMenuPopup(Sender: TObject);
  begin
    if OutputEdit.Focused then begin
      popupFile[1]:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      popupFile[2]:=OutputEdit.TextBetweenPoints[OutputEdit.BlockBegin,OutputEdit.BlockEnd];
    end else begin
      with getEditor^ do begin
        popupFile[1]:=editor.GetWordAtRowCol(editor.CaretXY);
        popupFile[2]:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
      end;
    end;
    pmiOpenFile1.caption:='Open: "'+popupFile[1]+'"';
    pmiOpenFile2.caption:='Open: "'+popupFile[2]+'"';
  end;

PROCEDURE TMnhForm.FormDestroy(Sender: TObject);
  VAR i:longint;
  begin
    mnh_evalThread.earlyFinalization;
    UpdateTimeTimer.enabled:=false;
    saveSettings;
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    outputHighlighter.destroy;
    debugHighlighter.destroy;
    helpHighlighter.destroy;
    wordsInEditor.destroy;
    guiTaskQueue.destroy;
    for i:=0 to length(editorMeta)-1 do dispose(editorMeta[i],destroy);
    setLength(editorMeta,0);
  end;

PROCEDURE TMnhForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(self,ssShift in Shift);
  end;

PROCEDURE TMnhForm.FormResize(Sender: TObject);
  begin
    if settingsReady then with settings.value^ do begin
      mainForm.top   :=top;
      mainForm.Left  :=Left;
      mainForm.width :=width;
      mainForm.height:=height;
      mainForm.isFullscreen:=(WindowState=wsMaximized);
      processSettings;
    end;
    if helpPopupMemo.visible then positionHelpNotifier;
  end;

PROCEDURE TMnhForm.FormShow(Sender: TObject);
  begin
    if not(settingsReady) then begin
      processSettings;
      KeyPreview:=true;
      UpdateTimeTimer.enabled:=true;
      updateMainMenuItems(true,true);
    end;
  end;

PROCEDURE TMnhForm.InputEditChange(Sender: TObject);
  begin
    if not(settingsReady) or
       (inputPageControl.activePageIndex<0) or
       (inputPageControl.activePageIndex>=length(editorMeta)) or
       (not(getEditor^.sheet.tabVisible)) then exit;
    if getEditor^.language=LANG_MNH
    then assistancEvaluator.evaluate((editorMeta[inputPageControl.activePageIndex]));
    caption:=getEditor^.updateSheetCaption;
  end;

PROCEDURE TMnhForm.InputEditProcessUserCommand(Sender: TObject;
  VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  VAR i:longint;
  begin
    if command=ecUserDefinedFirst then getEditor^.toggleComment;
    if command=ecUserDefinedFirst+1 then begin
      for i:=1 to length(editorMeta)-1 do if editorMeta[(i+inputPageControl.activePageIndex) mod length(editorMeta)]^.sheet.tabVisible then begin
        inputPageControl.activePageIndex:=(i+inputPageControl.activePageIndex) mod length(editorMeta);
        getEditor^.editor.SetFocus;
        exit;
      end;
    end;
    if command=ecUserDefinedFirst+2 then begin
      for i:=length(editorMeta)-1 downto 1 do if editorMeta[(i+inputPageControl.activePageIndex) mod length(editorMeta)]^.sheet.tabVisible then begin
        inputPageControl.activePageIndex:=(i+inputPageControl.activePageIndex) mod length(editorMeta);
        getEditor^.editor.SetFocus;
        exit;
      end;
    end;
    if (command=ecUserDefinedFirst+3) then begin
      getEditor^.toggleBreakpoint;
      runEvaluator.context.stepper^.clearBreakpoints;
      for i:=0 to length(editorMeta)-1 do editorMeta[i]^.setStepperBreakpoints;
      miDebug.Checked:=true;
      updateDebugParts;
    end;
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

PROCEDURE TMnhForm.miHelpClick(Sender: TObject);
  begin
    miHelp.Checked:=not(miHelp.Checked);
    if not(miHelp.Checked) then helpPopupMemo.visible:=false
                           else begin
                             helpPopupMemo.visible:=true;
                             inputEditReposition(getEditor^.editor.CaretXY,false,false);
                           end;
  end;

PROCEDURE TMnhForm.miAboutClick(Sender: TObject);
  begin
    splashForm.ShowModal;
  end;


PROCEDURE makeAndShowDoc();
  begin
    makeHtmlFromTemplate();
    OpenURL('file:///'+replaceAll(expandFileName(getHtmlRoot+'/index.html'   ),'\','/'));
  end;

PROCEDURE TMnhForm.miHelpExternallyClick(Sender: TObject);
  begin
    makeAndShowDoc();
  end;

PROCEDURE TMnhForm.miLangMnhClick(Sender: TObject);
  VAR i,editorIdx:longint;
  begin
    editorIdx:=inputPageControl.PageIndex;
    if (editorIdx<0) or (editorIdx>=length(editorMeta)) then exit;
    for i:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[i].menuItem.Checked then begin
      editorMeta[editorIdx]^.setLanguage(fileTypeMeta[i].language);
      exit;
    end;
  end;

PROCEDURE TMnhForm.updateMainMenuItems(CONST includeHistory,includeScriptMenu:boolean);
  PROCEDURE updateScriptMenus;
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

  PROCEDURE updateFileHistory;
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
    end;

  VAR evaluating:boolean;
      i:longint;
  begin
    miReload.enabled:=hasEditor and not(getEditor^.isPseudoFile);

    evaluating:=runEvaluator.evaluationRunning;
    miHaltEvalutaion.enabled:=evaluating;
    miEvaluateNow.enabled:=not(evaluating) and hasEditor and (getEditor^.language=LANG_MNH);
    miCallMain   .enabled:=not(evaluating) and hasEditor and (getEditor^.language=LANG_MNH);

    for i:=0 to length(fileTypeMeta)-1 do if hasEditor and (fileTypeMeta[i].language=getEditor^.language) then fileTypeMeta[i].menuItem.Checked:=true;

    if includeHistory then updateFileHistory;
    if includeScriptMenu then updateScriptMenus;
  end;

PROCEDURE TMnhForm.miProfileClick(Sender: TObject);
  begin
    miProfile.Checked:=not(miProfile.Checked);
    if miProfile.Checked and not(miTimingInfo.Checked) then miTimingInfoClick(Sender);
  end;

PROCEDURE TMnhForm.inputPageControlChange(Sender: TObject);
  begin
    if (inputPageControl.activePageIndex>=0) then begin
      SynCompletion.editor:=getEditor^.editor;
      settings.value^.workspace.activePage:=inputPageControl.activePageIndex;
      with getEditor^ do if language=LANG_MNH then assistancEvaluator.evaluate((editorMeta[inputPageControl.activePageIndex]));
      updateMainMenuItems(false,false);
    end;
  end;

PROCEDURE TMnhForm.Splitter1Moved(Sender: TObject);
  begin
    if helpPopupMemo.visible then positionHelpNotifier;
    if settingsReady then settings.value^.mainForm.relativeSplitterPosition:=outputPageControl.height/ClientHeight;
  end;

PROCEDURE TMnhForm.Splitter3Moved(Sender: TObject);
  begin
    updateExpressionMemo;
  end;

PROCEDURE TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  CONST MIN_INTERVALL=1;
        MAX_INTERVALL=1000;

  PROCEDURE processEditResult(CONST task:P_editScriptTask; CONST currentlyDebugging:boolean);
    VAR outIdx:longint;
        i:longint;
    begin
      if (task^.wantOutput) and (task^.getOutput<>nil) and (task^.getOutput^.literalType=lt_stringList) then begin
        if task^.wantNewEditor then outIdx:=addEditorMetaForNewFile
                               else outIdx:=task^.inputIdx;
        editorMeta[outIdx]^.setLanguage(task^.getOutputLanguage,LANG_TXT);
        editorMeta[outIdx]^.updateContentAfterEditScript(P_listLiteral(task^.getOutput));
        inputPageControl.activePageIndex:=outIdx;
      end else if (task^.wantInsert) and (task^.getOutput<>nil) and (task^.getOutput^.literalType=lt_string) then
        editorMeta[task^.inputIdx]^.insertText(P_stringLiteral(task^.getOutput)^.value);
      for i:=0 to length(editorMeta)-1 do editorMeta[i]^.editor.readonly:=currentlyDebugging;
    end;

  VAR aid:ansistring;
      flushPerformed:boolean=false;
      canRun:boolean;
      i,modalRes:longint;
      currentRunnerInfo:T_runnerStateInfo;
  begin
    currentRunnerInfo:=runEvaluator.getRunnerStateInfo;
    //progress time:------------------------------------------------------------
    if inputPageControl.activePageIndex>=0
    then aid:=C_tabChar+intToStr(getEditor^.editor.CaretY)+','+intToStr(getEditor^.editor.CaretX)
    else aid:='';
    StatusBar.SimpleText:=currentRunnerInfo.message+aid;
    //------------------------------------------------------------:progress time

    if currentRunnerInfo<>lastReportedRunnerInfo then begin
      {$ifdef debugMode} writeln('States: ',lastReportedRunnerInfo.state,' -> ',currentRunnerInfo.state); {$endif}
      if breakPointHandlingPending then handleBreak;
      //Halt/Run enabled states:--------------------------------------------------
      miHaltEvalutaion.enabled:=(currentRunnerInfo.state in C_runningStates);
      //--------------------------------------------------:Halt/Run enabled states
      lastReportedRunnerInfo:=currentRunnerInfo;
    end;
    canRun:=(inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta)) and (getEditor^.language=LANG_MNH) and not(currentRunnerInfo.state in C_runningStates);
    if canRun<>miEvaluateNow.enabled then begin
      miEvaluateNow.enabled:=canRun;
      miCallMain.enabled:=canRun;
    end;

    if currentRunnerInfo.hasPendingEditResult then begin
      processEditResult(runEvaluator.getCurrentEdit,currentRunnerInfo.state in [es_debugHalted,es_debugRunning]);
      runEvaluator.freeCurrentEdit;
      currentRunnerInfo.hasPendingEditResult:=false;
    end;

    if showing then begin
      while guiTaskQueue.executeTask do;
      //Form caption:-------------------------------------------------------------
      if (inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta))
      then begin
        aid:=getEditor^.updateSheetCaption;
        getEditor^.repaintWithStateCounter(assistancEvaluator.getStateCounter,assistancEvaluator.getErrorHints);
      end else aid:=APP_TITLE;
      if aid<>caption then caption:=aid;
      //-------------------------------------------------------------:Form caption

      //File checks:------------------------------------------------------------
      if (now>doNotCheckFileBefore) then begin
        doNotCheckFileBefore:=now+1;
        for i:=0 to length(editorMeta)-1 do with editorMeta[i]^ do
        if fileIsDeleted then begin
          modalRes:=closeDialogForm.showOnDeleted(fileInfo.filePath);
          if modalRes=mrOk then closeEditor;
          if modalRes=mrClose then begin if not(_doSave_(i)) then fileInfo.isChanged:=true; end else
          fileInfo.isChanged:=true;
          continue;
        end else if fileIsModifiedOnFileSystem then begin
          modalRes:=closeDialogForm.showOnOutOfSync(fileInfo.filePath);
          if modalRes=mrOk then reloadFile(fileInfo.filePath);
          if modalRes=mrClose then begin if not(_doSave_(i)) then fileInfo.isChanged:=true; end else
          fileInfo.isChanged:=true;
        end;
        doNotCheckFileBefore:=now+ONE_SECOND;

        if settings.value^.savingRequested then begin
          for i:=0 to length(editorMeta)-1 do editorMeta[i]^.writeToEditorState(settings.value);
          saveSettings;
        end;
      end;
      //-----------------------------------------------------------.:File checks
    end;

    if uniqueEditorInstanceIpcServer.PeekMessage(1,true)
    then FormDropFiles(self,split(uniqueEditorInstanceIpcServer.StringMessage,C_lineBreakChar));

    flushPerformed:=guiOutAdapter.flushToGui(OutputEdit);
    if flushPerformed and (outputPageControl.activePage<>outputTabSheet) then outputPageControl.activePage:=outputTabSheet;
    if guiAdapters.isDeferredPlotLogged and not(currentRunnerInfo.state in C_runningStates) then plotForm.doPlot();
    if askForm.displayPending then begin
      askForm.Show;
      flushPerformed:=true;
    end;
    if flushPerformed or (currentRunnerInfo.state in [es_debugHalted,es_debugRunning]) then UpdateTimeTimer.interval:=MIN_INTERVALL else begin
      UpdateTimeTimer.interval:=UpdateTimeTimer.interval+1;
      if UpdateTimeTimer.interval>MAX_INTERVALL then UpdateTimeTimer.interval:=MAX_INTERVALL;
    end;
    if closeGuiFlag then close;
  end;

PROCEDURE TMnhForm.miOpenDemoClick(Sender: TObject);
  begin
    if openDemoDialogForm.ShowModal=mrOk then
       inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(openDemoDialogForm.selectedFile);
  end;

PROCEDURE TMnhForm.miNewCentralPackageClick(Sender: TObject);
  begin
    if newCentralPackageForm.ShowModal=mrOk then
      inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(newCentralPackageForm.fileNameEdit.text);
  end;

PROCEDURE TMnhForm.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
    Special:=runEvaluator.context.isPaused and runEvaluator.evaluationRunning and (Sender=debugLine.editor) and (line=debugLine.line);
  end;

PROCEDURE TMnhForm.onEndOfEvaluation;
  VAR j:longint;
  begin
    for j:=0 to length(editorMeta)-1 do editorMeta[j]^.doneDebugging;
    updateDebugParts;
  end;

end.
