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
  runnerModel,
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
    PROCEDURE onEndOfEvaluation; override;
    //FUNCTION editForSearch(CONST replacing: boolean): TSynEdit;
    PROCEDURE positionHelpNotifier;
    PROCEDURE openLocation(CONST location:T_searchTokenLocation);
  private
    //uniqueEditorInstanceIpcServer: TSimpleIPCServer;
    ////underCursor:T_tokenInfo;
    ////
    //outputFocusedOnFind:boolean;
    //forceInputEditFocusOnOutputEditMouseUp:boolean;
    ////
    ////doNotMarkWordBefore:double;
    //doNotCheckFileBefore:double;
    ////breakPointHandlingPending:boolean;
    ////debugLine:record
    ////  editor:TSynEdit;
    ////  line:longint;
    ////end;
    //lastWordsCaret:longint;
    //wordsInEditor:T_setOfString;
    ////lastReportedRunnerInfo:T_runnerStateInfo;

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
{$i runnerLogic.inc}
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

PROCEDURE TMnhForm.onEndOfEvaluation;
  begin
    updateEditorsByGuiStatus;
  end;

//FUNCTION TMnhForm.editForSearch(CONST replacing: boolean): TSynEdit;
//  begin
//    if outputFocusedOnFind and not(replacing) then exit(OutputEdit);
//    if hasEditor then result:=getEditor^.editor
//                 else exit(OutputEdit); //not nice, but a valid fallback
//  end;

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

PROCEDURE TMnhForm.openLocation(CONST location:T_searchTokenLocation);
  VAR newIdx:longint;
  begin
    newIdx:=addOrGetEditorMetaForFiles(location.fileName,false);
    if newIdx<0 then exit;
    inputPageControl.activePageIndex:=newIdx;
    getEditor^.setCaret(location);
  end;

{$i mnh_gui_main_events.inc}

end.
