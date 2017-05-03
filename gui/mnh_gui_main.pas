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
  SynHighlighterMnh,
  SynEditHighlighter,
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
  editorMeta,
  editPopupModel,
  searchModel,
  ipcModel,
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


TYPE
  {$define includeInterface}
  {$include guiEditorInterface.inc}
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
    SynCompletion:             TSynCompletion;

    OutputEdit:                TSynEdit;
    assistanceSynEdit:         TSynEdit;
    SynExporterHTML:           TSynExporterHTML;
    SynGutterMarks0:           TSynGutterMarks;
    helpPopupMemo,
    currentExpressionMemo:     TSynMemo;
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
    {$i mnh_gui_main_events.inc}
    PROCEDURE onAssistantFinished;                                          override;
    PROCEDURE onEditFinished(CONST data:pointer; CONST successful:boolean); override;
    PROCEDURE onBreakpoint  (CONST data);                                   override;
    PROCEDURE onDebuggerEvent;                                              override;
    PROCEDURE onEndOfEvaluation;                                            override;
    PROCEDURE positionHelpNotifier;
    FUNCTION openLocation(CONST location:T_searchTokenLocation):boolean;
    PROCEDURE updateExpressionMemo;
  private
    focusEditorOnEditMouseUp:boolean;
    outputHighlighter,debugHighlighter,helpHighlighter:TSynMnhSyn;
    scriptMenuItems:array[T_scriptType] of array of TMenuItem;
    historyMenuItems:array of TMenuItem;
    FUNCTION focusedEditor:TSynEdit;

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

PROCEDURE TMnhForm.onAssistantFinished;
begin

end;

PROCEDURE TMnhForm.onEditFinished(CONST data: pointer; CONST successful: boolean);
begin

end;

PROCEDURE TMnhForm.onBreakpoint(CONST data);
begin

end;

PROCEDURE TMnhForm.onDebuggerEvent;
  VAR running:boolean;
      debugging:boolean;
      halted:boolean;

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

    miHaltEvalutaion.enabled:=running;
    miEvaluateNow   .enabled:=runnerModel.canRun;
    miCallMain      .enabled:=runnerModel.canRun;

    debugTabSheet.tabVisible:=debugging;
    DebugToolbar .visible   :=debugging;
    DebugToolbar .enabled   :=debugging;
    if DebugToolbar.visible then DebugToolbar .top:=0
                            else if outputPageControl.activePage =debugTabSheet then
                                    outputPageControl.activePage:=outputTabSheet;
    handleButton(tbStop     ,halted or running, 2);
    handleButton(tbRun      ,runnerModel.canRun, 0,true);
    handleButton(tbStep     ,halted , 4);
    handleButton(tbStepIn   ,halted , 6);
    handleButton(tbStepOut  ,halted , 8);
    handleButton(tbMicroStep,halted ,10);
    updateEditorsByGuiStatus;
  end;

PROCEDURE TMnhForm.onEndOfEvaluation;
  begin
    updateEditorsByGuiStatus;
  end;

PROCEDURE TMnhForm.positionHelpNotifier;
  VAR maxLineLength:longint=0;
      i:longint;
      p:TPoint;
  begin
    p:=getEditor^.caretInMainFormCoordinates;
    helpPopupMemo.text:=getHelpPopupText;
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

FUNCTION TMnhForm.openLocation(CONST location:T_searchTokenLocation):boolean;
  VAR newIdx:longint;
  begin
    newIdx:=addOrGetEditorMetaForFiles(location.fileName,false);
    if newIdx<0 then exit(false);
    inputPageControl.activePageIndex:=newIdx;
    getEditor^.setCaret(location);
    result:=true;
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

{$i mnh_gui_main_events.inc}

end.
