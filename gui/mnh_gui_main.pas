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
  SynHighlighterPas, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterJScript, SynHighlighterPerl, SynHighlighterHTML,
  SynHighlighterXML, SynHighlighterDiff, synhighlighterunixshellscript,
  SynHighlighterCss, SynHighlighterPHP, SynHighlighterSQL, SynHighlighterPython,
  SynHighlighterVB, SynHighlighterBat, SynHighlighterIni, SynEditHighlighter,
  //Other Forms:
  newCentralPackageDialog,
  mnh_gui_settings,
  closeDialog,
  askDialog,
  mnh_tables,
  openDemoDialog,
  mnh_workspaces,
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
    miFileHistory0,
    miFileHistory1,
    miFileHistory10,
    miFileHistory11,
    miFileHistory12,
    miFileHistory13,
    miFileHistory14,
    miFileHistory15,
    miFileHistory16,
    miFileHistory17,
    miFileHistory18,
    miFileHistory19,
    miFileHistory2,
    miFileHistory3,
    miFileHistory4,
    miFileHistory5,
    miFileHistory6,
    miFileHistory7,
    miFileHistory8,
    miFileHistory9,
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
    miWorkspaces,
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
    {$include debuggerLogic.inc}
    {$include runnerLogic.inc}
    {$include settingsLogic.inc}
    {$include filesLogic.inc}
    {$include searchLogic.inc}
    {$include completionLogic.inc}

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
    PROCEDURE miWorkspacesClick(Sender: TObject);
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
    lastStart:record
      mainCall:boolean;
      parameters:string;
    end;
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
    PROCEDURE positionHelpNotifier;
    PROCEDURE setUnderCursor(CONST wordText:ansistring; CONST updateMarker,forJump:boolean);
  public
    editorMeta:array of P_editorMeta;
    PROCEDURE enableMenuForLanguage(CONST languageIndex:byte);
    PROCEDURE updateScriptMenus;
  end;

VAR MnhForm: TMnhForm;

{$undef includeInterface}
IMPLEMENTATION
VAR closeGuiFlag:boolean=false;
{$R *.lfm}
{$define includeImplementation}
{$include guiEditorInterface.inc}
{$include editorMeta.inc}
{$include debuggerLogic.inc}
{$include runnerLogic.inc}
{$include settingsLogic.inc}
{$include filesLogic.inc}
{$include searchLogic.inc}
{$include completionLogic.inc}
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
    p:=editorMeta[inputPageControl.activePageIndex]^.caretInMainFormCoordinates;

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
    if editorMeta[inputPageControl.activePageIndex]^.language<>LANG_MNH then exit;
    if updateMarker then begin
      outputHighlighter.setMarkedWord(wordText);
      for i:=0 to length(editorMeta)-1 do editorMeta[i]^.setMarkedWord(wordText);
    end;
    if miHelp.Checked or forJump then with editorMeta[inputPageControl.activePageIndex]^.editor do
      assistancEvaluator.explainIdentifier(lines[CaretY-1],CaretY,CaretX,underCursor);
    if miHelp.Checked then begin
      helpPopupMemo.text:=ECHO_MARKER+underCursor.tokenText+C_lineBreakChar+underCursor.tokenExplanation;
      positionHelpNotifier;
    end;
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

  begin
    splashOnStartup;
    uniqueEditorInstanceIpcServer:=TSimpleIPCServer.create(self);
    uniqueEditorInstanceIpcServer.serverId:=UNIQUE_EDITOR_IPC_ID;
    uniqueEditorInstanceIpcServer.Global:=true;
    uniqueEditorInstanceIpcServer.StartServer;

    initGuiOutAdapters(MnhForm,true);
    guiTaskQueue.create;
    reregisterRule(SYSTEM_BUILTIN_NAMESPACE,'ask',@ask_impl);
    registerRule(GUI_NAMESPACE,'editors'       ,@editors_impl      ,false,ak_nullary,'editors(...);//Lists all editors');
    registerRule(GUI_NAMESPACE,'editorContent' ,@editorContent_impl,false,ak_unary,'editorContent(name:string);//Returns the content of the given editor as a string or void if no such editor was found.');
    registerRule(GUI_NAMESPACE,'openInEditor'  ,@openInEditor_impl ,false,ak_unary,'openInEditor(filename:string);//opens an editor tab for the given file');

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
    setLength(scriptMenuItems[st_edit],0);
    setLength(scriptMenuItems[st_insert],0);
    setLength(scriptMenuItems[st_util],0);
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
      with editorMeta[inputPageControl.activePageIndex]^ do begin
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
    end;
  end;

PROCEDURE TMnhForm.InputEditChange(Sender: TObject);
  begin
    if not(settingsReady) or
       (inputPageControl.activePageIndex<0) or
       (inputPageControl.activePageIndex>=length(editorMeta)) or
       (not(editorMeta[inputPageControl.activePageIndex]^.sheet.tabVisible)) then exit;
    if editorMeta[inputPageControl.activePageIndex]^.language=LANG_MNH
    then assistancEvaluator.evaluate((editorMeta[inputPageControl.activePageIndex]));
    caption:=editorMeta[inputPageControl.activePageIndex]^.updateSheetCaption;
  end;

PROCEDURE TMnhForm.InputEditProcessUserCommand(Sender: TObject;
  VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  VAR i:longint;
  begin
    if command=ecUserDefinedFirst then editorMeta[inputPageControl.activePageIndex]^.toggleComment;
    if command=ecUserDefinedFirst+1 then begin
      for i:=1 to length(editorMeta)-1 do if editorMeta[(i+inputPageControl.activePageIndex) mod length(editorMeta)]^.sheet.tabVisible then begin
        inputPageControl.activePageIndex:=(i+inputPageControl.activePageIndex) mod length(editorMeta);
        editorMeta[inputPageControl.activePageIndex]^.editor.SetFocus;
        exit;
      end;
    end;
    if command=ecUserDefinedFirst+2 then begin
      for i:=length(editorMeta)-1 downto 1 do if editorMeta[(i+inputPageControl.activePageIndex) mod length(editorMeta)]^.sheet.tabVisible then begin
        inputPageControl.activePageIndex:=(i+inputPageControl.activePageIndex) mod length(editorMeta);
        editorMeta[inputPageControl.activePageIndex]^.editor.SetFocus;
        exit;
      end;
    end;
    if (command=ecUserDefinedFirst+3) then begin
      editorMeta[inputPageControl.activePageIndex]^.toggleBreakpoint;
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
                             inputEditReposition(editorMeta[inputPageControl.activePageIndex]^.editor.CaretXY,false,false);
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

PROCEDURE TMnhForm.enableMenuForLanguage(CONST languageIndex:byte);
  VAR i:longint;
  begin
    for i:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[i].language=languageIndex then fileTypeMeta[i].menuItem.Checked:=true;
  end;

PROCEDURE TMnhForm.updateScriptMenus;
  VAR i,k:longint;
      scriptList:T_scriptMetaArray;
      script:P_scriptMeta;
      scriptType:T_scriptType;
      root:TMenuItem;
  begin
    {$ifdef debugMode}writeln(stdErr,'updateScriptMenus start');{$endif}
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
    {$ifdef debugMode}writeln(stdErr,'fetching scripts');{$endif}
    scriptList:=runEvaluator.getScripts;
    {$ifdef debugMode}writeln(stdErr,'updating menu from ',length(scriptList),' scripts');{$endif}
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
      {$ifdef debugMode}writeln(stdErr,'creating entry ',scriptType,',',k,' ',script^.getName);{$endif}
      scriptMenuItems[scriptType][k]:=TMenuItem.create(MainMenu1);
      scriptMenuItems[scriptType][k].caption:=script^.getName;
      scriptMenuItems[scriptType][k].Tag:=i;
      scriptMenuItems[scriptType][k].OnClick:=@miRunCustomUtilScript;
      root.add(scriptMenuItems[scriptType][k]);
    end;
    {$ifdef debugMode}writeln(stdErr,'updateScriptMenus end');{$endif}
  end;

PROCEDURE TMnhForm.miProfileClick(Sender: TObject);
  begin
    miProfile.Checked:=not(miProfile.Checked);
    if miProfile.Checked and not(miTimingInfo.Checked) then miTimingInfoClick(Sender);
  end;

PROCEDURE TMnhForm.miWorkspacesClick(Sender: TObject);
  VAR i:longint;
  begin
    if runEvaluator.evaluationRunning then exit;
    for i:=0 to length(editorMeta)-1 do editorMeta[i]^.writeToEditorState(settings.value);
    if switchWorkspace then begin
      for i:=0 to length(editorMeta)-1 do editorMeta[i]^.closeEditor;
      for i:=0 to length(settings.value^.workspace.editorState)-1 do begin
        if i>=length(editorMeta) then begin
          setLength(editorMeta,i+1);
          editorMeta[i]^.create(i,settings.value^.workspace.editorState[i]);
        end else editorMeta[i]^.initWithState(settings.value^.workspace.editorState[i]);
        editorMeta[i]^.setStepperBreakpoints;
      end;
      i:=settings.value^.workspace.activePage;
      inputPageControl.activePageIndex:=i;
      if (i>=0) and (i<length(editorMeta)) then SynCompletion.editor:=editorMeta[inputPageControl.activePageIndex]^.editor;
    end;
  end;

PROCEDURE TMnhForm.inputPageControlChange(Sender: TObject);
  begin
    if (inputPageControl.activePageIndex>=0) then begin
      SynCompletion.editor:=editorMeta[inputPageControl.activePageIndex]^.editor;
      settings.value^.workspace.activePage:=inputPageControl.activePageIndex;
      with editorMeta[inputPageControl.activePageIndex]^ do if language=LANG_MNH then assistancEvaluator.evaluate((editorMeta[inputPageControl.activePageIndex]));
      enableMenuForLanguage(editorMeta[inputPageControl.activePageIndex]^.language);
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
    then aid:=C_tabChar+intToStr(editorMeta[inputPageControl.activePageIndex]^.editor.CaretY)+','+intToStr(editorMeta[inputPageControl.activePageIndex]^.editor.CaretX)
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
    canRun:=(inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta)) and (editorMeta[inputPageControl.activePageIndex]^.language=LANG_MNH) and not(currentRunnerInfo.state in C_runningStates);
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
        aid:=editorMeta[inputPageControl.activePageIndex]^.updateSheetCaption;
        editorMeta[inputPageControl.activePageIndex]^.repaintWithStateCounter(assistancEvaluator.getStateCounter,assistancEvaluator.getErrorHints);
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
