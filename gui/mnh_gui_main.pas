UNIT mnh_gui_main;

{$mode objfpc}{$H+}
INTERFACE
USES
  mnhFormHandler, Classes, sysutils, FileUtil, SynEdit, SynEditTypes,
  SynCompletion, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls,
  Grids, SynHighlighterMnh, mnh_settings, mnh_gui_settings, mnh_basicTypes,
  mnh_out_adapters, myStringUtil, mnh_evalThread, mnh_constants, types, LCLType,
  mnh_plotData, mnh_funcs, mnh_litVar, mnh_doc, lclintf, StdCtrls, mnh_packages,
  closeDialog, askDialog, SynEditKeyCmds, SynMemo, myGenerics, mnh_fileWrappers,
  mySys, mnh_html, mnh_cmdLineInterpretation, mnh_plotForm,
  newCentralPackageDialog, SynGutterMarks, SynEditMarks, mnh_contexts,
  SynPluginMultiCaret, SynEditMiscClasses,
  SynHighlighterPas, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterJScript, SynHighlighterPerl, SynHighlighterHTML,
  SynHighlighterXML, SynHighlighterDiff, synhighlighterunixshellscript,
  SynHighlighterCss, SynHighlighterPHP, SynHighlighterSQL, SynHighlighterPython,
  SynHighlighterVB, SynHighlighterBat, SynHighlighterIni, SynEditHighlighter,
  LazUTF8, mnh_tables,  openDemoDialog;

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

VAR fileTypeMeta:array of record
      extension:string;
      language:byte;
      highlighter:TSynCustomHighlighter;
      menuItem:TMenuItem;
    end;

TYPE
  {$define includeInterface}
  {$include editorMeta.inc}
  {$include guiOutAdapter.inc}
  {$undef includeInterface}
  {$WARN 5024 OFF}
  { TMnhForm }

  TMnhForm = class(TForm)
    debugItemsImageList: TImageList;
    callStackGroupBox: TGroupBox;
    callStackList: TListBox;
    MenuItem1: TMenuItem;
    miLangPython: TMenuItem;
    miLangXml: TMenuItem;
    miLangDiff: TMenuItem;
    miLangShell: TMenuItem;
    miLangCss: TMenuItem;
    miLangPhp: TMenuItem;
    miLangSql: TMenuItem;
    miLangVb: TMenuItem;
    miLangBat: TMenuItem;
    miLangIni: TMenuItem;
    miLangMnh: TMenuItem;
    miLangPascal: TMenuItem;
    miLangCpp: TMenuItem;
    miLangJava: TMenuItem;
    miLangJS: TMenuItem;
    miLangPerl: TMenuItem;
    miLangHtml: TMenuItem;
    miFullscreen: TMenuItem;
    miProfile: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    pmiOpenFile2: TMenuItem;
    pmiOpenFile1: TMenuItem;
    miFileHistory14: TMenuItem;
    miFileHistory15: TMenuItem;
    miFileHistory16: TMenuItem;
    miFileHistory17: TMenuItem;
    miFileHistory18: TMenuItem;
    miFileHistory19: TMenuItem;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    callStackInfoStringGrid: TStringGrid;
    subMenuCode: TMenuItem;
    miFileHistory10: TMenuItem;
    miFileHistory11: TMenuItem;
    miFileHistory12: TMenuItem;
    miFileHistory13: TMenuItem;
    miOpenDocumentationPack: TMenuItem;
    miWrapEcho: TMenuItem;
    mi_insertFilename: TMenuItem;
    miClose: TMenuItem;
    miMinErrorlevel5: TMenuItem;
    miOpenDocumentation: TMenuItem;
    MenuItem4: TMenuItem;
    miMinErrorlevel1: TMenuItem;
    miMinErrorlevel2: TMenuItem;
    miMinErrorlevel3: TMenuItem;
    miMinErrorlevel4: TMenuItem;
    miTimingInfo: TMenuItem;
    miDebug: TMenuItem;
    miCallMain: TMenuItem;
    miHelp: TMenuItem;
    subMenuHelp: TMenuItem;
    OutputEdit: TSynEdit;
    miFileHistory6: TMenuItem;
    miFileHistory7: TMenuItem;
    miFileHistory8: TMenuItem;
    miFileHistory9: TMenuItem;
    subMenuEvaluation: TMenuItem;
    miFileHistory0: TMenuItem;
    miFileHistory1: TMenuItem;
    miFileHistory2: TMenuItem;
    miFileHistory3: TMenuItem;
    miFileHistory4: TMenuItem;
    miFileHistory5: TMenuItem;
    miHaltEvalutaion: TMenuItem;
    miEvaluateNow: TMenuItem;
    miClear: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    OpenDialog: TOpenDialog;
    MainMenu1: TMainMenu;
    subMenuFile: TMenuItem;
    EditorPopupMenu: TPopupMenu;
    Splitter1: TSplitter;
    submenuEditorAppearance: TMenuItem;
    miExpressionEcho: TMenuItem;
    miExpressionResult: TMenuItem;
    miDeclarationEcho: TMenuItem;
    miDecFontSize: TMenuItem;
    miIncFontSize: TMenuItem;
    mi_settings: TMenuItem;
    inputPageControl: TPageControl;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    SynBatSyn1: TSynBatSyn;
    SynCompletion: TSynCompletion;
    assistanceTabSheet: TTabSheet;
    assistanceSynEdit: TSynEdit;
    SynCppSyn1: TSynCppSyn;
    SynCssSyn1: TSynCssSyn;
    SynDiffSyn1: TSynDiffSyn;
    SynFreePascalSyn1: TSynFreePascalSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynIniSyn1: TSynIniSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynPerlSyn1: TSynPerlSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynPythonSyn1: TSynPythonSyn;
    SynSQLSyn1: TSynSQLSyn;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
    SynVBSyn1: TSynVBSyn;
    SynXMLSyn1: TSynXMLSyn;
    tbMicroStep: TToolButton;
    variablesTreeView: TTreeView;
    UpdateTimeTimer: TTimer;
    helpPopupMemo: TSynMemo;
    miOpenDemo: TMenuItem;
    miNewCentralPackage: TMenuItem;
    FindDialog: TFindDialog;
    ReplaceDialog: TReplaceDialog;
    subMenuSearch: TMenuItem;
    miFind: TMenuItem;
    miReplace: TMenuItem;
    miFindNext: TMenuItem;
    miFindPrevious: TMenuItem;
    DebugToolbar: TToolBar;
    tbRun: TToolButton;
    tbStepIn: TToolButton;
    tbStep: TToolButton;
    tbStepOut: TToolButton;
    tbStop: TToolButton;
    SynGutterMarks0: TSynGutterMarks;
    breakpointsImagesList: TImageList;
    outputPageControl: TPageControl;
    outputTabSheet: TTabSheet;
    debugTabSheet: TTabSheet;
    currentExpressionGroupBox: TGroupBox;
    currentExpressionMemo: TSynMemo;
    PROCEDURE assistanceSynEditKeyUp(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE assistanceSynEditMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE assistanceSynEditMouseUp(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE callStackListSelectionChange(Sender: TObject; User: boolean);
    PROCEDURE EditorPopupMenuPopup(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE InputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE InputEditProcessUserCommand(Sender: TObject;
      VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
    PROCEDURE MenuItem4Click(Sender: TObject);
    PROCEDURE miClearClick(Sender: TObject);
    PROCEDURE miCloseClick(Sender: TObject);
    PROCEDURE miDebugCancelClick(Sender: TObject);
    PROCEDURE miDebugClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miDeclarationEchoClick(Sender: TObject);
    PROCEDURE miEvaluateNowClick(Sender: TObject);
    PROCEDURE miExpressionEchoClick(Sender: TObject);
    PROCEDURE miExpressionResultClick(Sender: TObject);
    PROCEDURE miFileHistory0Click(Sender: TObject);
    PROCEDURE miFileHistory10Click(Sender: TObject);
    PROCEDURE miFileHistory11Click(Sender: TObject);
    PROCEDURE miFileHistory12Click(Sender: TObject);
    PROCEDURE miFileHistory13Click(Sender: TObject);
    PROCEDURE miFileHistory14Click(Sender: TObject);
    PROCEDURE miFileHistory15Click(Sender: TObject);
    PROCEDURE miFileHistory16Click(Sender: TObject);
    PROCEDURE miFileHistory17Click(Sender: TObject);
    PROCEDURE miFileHistory18Click(Sender: TObject);
    PROCEDURE miFileHistory19Click(Sender: TObject);
    PROCEDURE miFileHistory1Click(Sender: TObject);
    PROCEDURE miFileHistory2Click(Sender: TObject);
    PROCEDURE miFileHistory3Click(Sender: TObject);
    PROCEDURE miFileHistory4Click(Sender: TObject);
    PROCEDURE miFileHistory5Click(Sender: TObject);
    PROCEDURE miFileHistory6Click(Sender: TObject);
    PROCEDURE miFileHistory7Click(Sender: TObject);
    PROCEDURE miFileHistory8Click(Sender: TObject);
    PROCEDURE miFileHistory9Click(Sender: TObject);
    PROCEDURE miFullscreenClick(Sender: TObject);
    PROCEDURE miHaltEvalutaionClick(Sender: TObject);
    PROCEDURE miHelpClick(Sender: TObject);
    PROCEDURE miHelpExternallyClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miLangMnhClick(Sender: TObject);
    PROCEDURE miMinErrorlevel1Click(Sender: TObject);
    PROCEDURE miMinErrorlevel2Click(Sender: TObject);
    PROCEDURE miMinErrorlevel3Click(Sender: TObject);
    PROCEDURE miMinErrorlevel4Click(Sender: TObject);
    PROCEDURE miMinErrorlevel5Click(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miOpenDocumentationPackClick(Sender: TObject);
    PROCEDURE miProfileClick(Sender: TObject);
    PROCEDURE miSaveAsClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miTimingInfoClick(Sender: TObject);
    PROCEDURE miWrapEchoClick(Sender: TObject);
    PROCEDURE mi_insertFilenameClick(Sender: TObject);
    PROCEDURE mi_settingsClick(Sender: TObject);
    PROCEDURE OutputEditKeyDown(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE OutputEditMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE OutputEditMouseUp(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE inputPageControlChange(Sender: TObject);
    PROCEDURE pmiOpenFile(CONST idOrName:string);
    PROCEDURE pmiOpenFile1Click(Sender: TObject);
    PROCEDURE pmiOpenFile2Click(Sender: TObject);
    PROCEDURE PopupNotifier1Close(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE Splitter1Moved(Sender: TObject);
    PROCEDURE Splitter3Moved(Sender: TObject);
    PROCEDURE SynCompletionCodeCompletion(VAR value: string;
      sourceValue: string; VAR SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    PROCEDURE SynCompletionExecute(Sender: TObject);
    PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);
    PROCEDURE tbMicroStepClick(Sender: TObject);
    PROCEDURE UpdateTimeTimerTimer(Sender: TObject);
    PROCEDURE miOpenDemoClick(Sender: TObject);
    PROCEDURE miNewCentralPackageClick(Sender: TObject);
    PROCEDURE miFindClick(Sender: TObject);
    PROCEDURE miReplaceClick(Sender: TObject);
    PROCEDURE FindDialogFind(Sender: TObject);
    PROCEDURE ReplaceDialogReplace(Sender: TObject);
    PROCEDURE ReplaceDialogFind(Sender: TObject);
    PROCEDURE miFindNextClick(Sender: TObject);
    PROCEDURE miFindPreviousClick(Sender: TObject);
    PROCEDURE tbRunClick(Sender: TObject);
    PROCEDURE tbStepInClick(Sender: TObject);
    PROCEDURE tbStepClick(Sender: TObject);
    PROCEDURE tbStepOutClick(Sender: TObject);
    PROCEDURE tbStopClick(Sender: TObject);
    PROCEDURE InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
    PROCEDURE variablesTreeViewExpanding(Sender: TObject; node: TTreeNode;
      VAR AllowExpansion: boolean);

  private
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
    wordsInEditor:T_listOfString;

    PROCEDURE setEditorMode(CONST enable:boolean);
    FUNCTION editForSearch(CONST replacing:boolean):TSynEdit;
    PROCEDURE processSettings;
    PROCEDURE processFileHistory;
    PROCEDURE positionHelpNotifier;
    PROCEDURE setUnderCursor(CONST wordText:ansistring; CONST updateMarker,forJump:boolean);
    PROCEDURE ensureWordsInEditorForCompletion;

    PROCEDURE doConditionalPlotReset;
    PROCEDURE openFromHistory(CONST historyIdx:byte);
    PROCEDURE doStartEvaluation(CONST reEvaluating:boolean);
    PROCEDURE inputEditReposition(CONST caret:TPoint; CONST doJump,updateMarker:boolean);
    PROCEDURE outputEditReposition(CONST caret:TPoint; CONST doJump:boolean);
    PROCEDURE assistanceEditReposition(CONST caret:TPoint; CONST doJump:boolean);
    PROCEDURE _setErrorlevel_(CONST i: byte);
    FUNCTION _doSaveAs_(CONST index:longint):boolean;
    FUNCTION _doSave_(CONST index:longint):boolean;
    PROCEDURE updateDebugParts;
    PROCEDURE handleBreak;
    PROCEDURE updateExpressionMemo;
    FUNCTION preferredContextType:T_contextType;
  public
    editorMeta:array of T_editorMeta;
    FUNCTION addEditorMetaForNewFile(CONST newFileName: ansistring=''):longint;
    FUNCTION addOrGetEditorMetaForFile(CONST fileName: ansistring):longint;
    PROCEDURE enableMenuForLanguage(CONST languageIndex:byte);
  end;

VAR MnhForm: TMnhForm;

PROCEDURE lateInitialization;
PROCEDURE doFinalization;
IMPLEMENTATION
VAR guiOutAdapter: T_guiOutAdapter;
    guiAdapters: T_adapters;
    closeGuiFlag:boolean=false;
{$R *.lfm}
{$define includeImplementation}
{$include editorMeta.inc}
{$include guiOutAdapter.inc}
{$undef includeImplementation}

PROCEDURE TMnhForm.positionHelpNotifier;
  VAR maxLineLength:longint=0;
      i:longint;
      p:TPoint;
  begin
    p:=editorMeta[inputPageControl.activePageIndex].caretInMainFormCoordinates;

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
    if updateMarker then begin
      outputHighlighter.setMarkedWord(wordText);
      for i:=0 to length(editorMeta)-1 do editorMeta[i].setMarkedWord(wordText);
    end;
    if miHelp.Checked or forJump then with editorMeta[inputPageControl.activePageIndex].editor do assistancEvaluator.explainIdentifier(lines[CaretY-1],CaretY,CaretX,underCursor);
    if miHelp.Checked then begin
      helpPopupMemo.text:=underCursor.tokenText+C_lineBreakChar+underCursor.tokenExplanation;
      positionHelpNotifier;
    end;
  end;

PROCEDURE TMnhForm.doConditionalPlotReset;
  begin
    if settings.value^.doResetPlotOnEvaluation then begin
      guiAdapters.plot.setDefaults;
      if plotFormIsInitialized then plotForm.pullPlotSettingsToGui();
    end;
  end;

PROCEDURE TMnhForm.openFromHistory(CONST historyIdx: byte);
  begin
    with settings.value^.workspace.fileHistory do begin
      if fileExists(historyItem(historyIdx))
      then inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(historyItem(historyIdx))
      else if polishHistory then processFileHistory;
    end;
  end;

PROCEDURE TMnhForm.doStartEvaluation(CONST reEvaluating: boolean);
  VAR i:longint;
  begin
    guiOutAdapter.flushClear;
    UpdateTimeTimerTimer(self);
    doConditionalPlotReset;
    resetTableForms;
    if not(reEvaluating) then editorMeta[inputPageControl.activePageIndex].setWorkingDir;
    underCursor.tokenText:='';
    if miDebug.Checked or reEvaluating and profilingRun then begin
      if reEvaluating and profilingRun then runEvaluator.context.clearBreakpoints;
      for i:=0 to length(editorMeta)-1 do editorMeta[i].startDebugging;
      updateDebugParts;
    end;
    breakPointHandlingPending:=true;
    UpdateTimeTimer.interval:=1;
  end;

PROCEDURE TMnhForm.inputEditReposition(CONST caret: TPoint; CONST doJump,updateMarker: boolean);
  VAR wordUnderCursor:string;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    with editorMeta[inputPageControl.activePageIndex] do begin
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
        editorMeta[pageIdx].editor.CaretXY:=newCaret;
      end;
    end;
  end;

PROCEDURE TMnhForm.outputEditReposition(CONST caret: TPoint;
  CONST doJump: boolean);
  VAR loc:T_searchTokenLocation;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(OutputEdit.GetWordAtRowCol(caret),true,doJump);
    loc:=guessLocationFromString(OutputEdit.lines[caret.y-1],false);
    if not(doJump) then exit;
    if reEvaluationWithGUIrequired then begin
      if runEvaluator.evaluationRunning
      then exit
      else setEditorMode(true);
    end;
    if (loc.fileName='') or (loc.fileName='?') then exit;
    pageIdx:=addOrGetEditorMetaForFile(loc.fileName);
    if pageIdx<0 then exit;
    inputPageControl.activePageIndex:=pageIdx;
    with editorMeta[pageIdx] do begin
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
    with editorMeta[pageIdx] do begin
      editor.SetFocus;
      highlighter.setMarkedToken(loc.line-1,loc.column-1);
      newCaret.x:=loc.column;
      newCaret.y:=loc.line;
      editor.CaretXY:=newCaret;
      forceInputEditFocusOnOutputEditMouseUp:=true;
    end;
  end;

PROCEDURE TMnhForm.FormCreate(Sender: TObject);
  PROCEDURE initFileTypes;
    PROCEDURE addFileType(CONST extension:string; CONST language:byte; CONST highlighter:TSynCustomHighlighter; CONST menuItem:TMenuItem);
      begin
        setLength(fileTypeMeta,length(fileTypeMeta)+1);
        fileTypeMeta[length(fileTypeMeta)-1].extension  :='.'+uppercase(extension);
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

  VAR i:longint;
  begin
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

    helpHighlighter:=TSynMnhSyn.create(nil,msf_guessing);
    helpPopupMemo.highlighter:=helpHighlighter;

    debugHighlighter:=TSynMnhSyn.create(nil,msf_input);
    currentExpressionMemo.highlighter:=debugHighlighter;

    initHighlighters;
    OutputEdit.clearAll;
    for i:=0 to length(LOGO)-1 do OutputEdit.lines.append(LOGO[i]);
    assistanceSynEdit.clearAll;
    for i:=0 to length(LOGO)-1 do assistanceSynEdit.lines.append(LOGO[i]);
    {$ifdef debugMode}
    if wantConsoleAdapter then guiAdapters.addConsoleOutAdapter^.enableMessageType(false,[mt_clearConsole]);
    {$endif}
    mnh_out_adapters.gui_started:=true;
    updateDebugParts;
  end;

PROCEDURE TMnhForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  VAR i:integer;
  begin
    if runEvaluator      .evaluationRunning then runEvaluator      .haltEvaluation;
    if assistancEvaluator.evaluationRunning then assistancEvaluator.haltEvaluation;
    for i:=0 to length(editorMeta)-1 do editorMeta[i].writeToEditorState(settings.value);
  end;

PROCEDURE TMnhForm.EditorPopupMenuPopup(Sender: TObject);
  begin
    if OutputEdit.Focused then begin
      popupFile[1]:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      popupFile[2]:=OutputEdit.TextBetweenPoints[OutputEdit.BlockBegin,OutputEdit.BlockEnd];
    end else begin
      with editorMeta[inputPageControl.activePageIndex] do begin
        popupFile[1]:=editor.GetWordAtRowCol(editor.CaretXY);
        popupFile[2]:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
      end;
    end;
    pmiOpenFile1.caption:='Open: "'+popupFile[1]+'"';
    pmiOpenFile2.caption:='Open: "'+popupFile[2]+'"';
  end;

PROCEDURE TMnhForm.assistanceSynEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if ((key=13) and (ssCtrl in Shift)) then assistanceEditReposition(assistanceSynEdit.CaretXY,true);
    if forceInputEditFocusOnOutputEditMouseUp and (inputPageControl.activePageIndex>=0) then ActiveControl:=editorMeta[inputPageControl.activePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.assistanceSynEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    outputEditReposition(OutputEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.assistanceSynEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=editorMeta[inputPageControl.activePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.FormDestroy(Sender: TObject);
  begin
    UpdateTimeTimer.enabled:=false;
    if not(reEvaluationWithGUIrequired) then saveSettings;
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    outputHighlighter.destroy;
    debugHighlighter.destroy;
    helpHighlighter.destroy;
    wordsInEditor.destroy;
  end;

PROCEDURE TMnhForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  VAR i:longint;
  begin
    for i:=0 to length(FileNames)-1 do inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(FileNames[i]);
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
    end;
    if helpPopupMemo.visible then positionHelpNotifier;
  end;

PROCEDURE TMnhForm.FormShow(Sender: TObject);
  begin
    if not(settingsReady) then begin
      processSettings;
      KeyPreview:=true;
      UpdateTimeTimer.enabled:=true;
      if reEvaluationWithGUIrequired then begin
        doStartEvaluation(true);
        if profilingRun then runEvaluator.reEvaluateWithGUI(ct_profiling)
                        else runEvaluator.reEvaluateWithGUI(ct_normal);
        setEditorMode(false);
      end;
    end;
  end;

PROCEDURE TMnhForm.InputEditChange(Sender: TObject);
  begin
    if not(settingsReady) or
       reEvaluationWithGUIrequired or
       (inputPageControl.activePageIndex<0) or
       (inputPageControl.activePageIndex>=length(editorMeta)) or
       (not(editorMeta[inputPageControl.activePageIndex].sheet.tabVisible)) then exit;

    with editorMeta[inputPageControl.activePageIndex] do assistancEvaluator.evaluate(pseudoName,editor.lines);
    caption:=editorMeta[inputPageControl.activePageIndex].updateSheetCaption;
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if (key=13) and ((ssCtrl in Shift) or (ssAlt in Shift))
    then inputEditReposition(editorMeta[inputPageControl.activePageIndex].editor.CaretXY,ssCtrl in Shift,true)
    else inputEditReposition(editorMeta[inputPageControl.activePageIndex].editor.CaretXY,false,false);
    if runEvaluator.context.hasOption(cp_debug) and runEvaluator.evaluationRunning then begin
      if (key=116) and tbRun      .enabled then tbRunClick(Sender);
      if (key=117) and tbStepIn   .enabled then tbStepInClick(Sender);
      if (key=118) and tbStep     .enabled then tbStepClick(Sender);
      if (key=119) and tbStepOut  .enabled then tbStepOutClick(Sender);
      if (key=121) and tbMicroStep.enabled then tbMicroStepClick(Sender);
    end;
  end;

PROCEDURE TMnhForm.InputEditMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    inputEditReposition(editorMeta[inputPageControl.activePageIndex].editor.PixelsToRowColumn(point),ssCtrl in Shift,true);
  end;

PROCEDURE TMnhForm.InputEditProcessUserCommand(Sender: TObject;
  VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  VAR i:longint;
  begin
    if command=ecUserDefinedFirst then editorMeta[inputPageControl.activePageIndex].toggleComment;
    if command=ecUserDefinedFirst+1 then begin
      for i:=1 to length(editorMeta)-1 do if editorMeta[(i+inputPageControl.activePageIndex) mod length(editorMeta)].sheet.tabVisible then begin
        inputPageControl.activePageIndex:=(i+inputPageControl.activePageIndex) mod length(editorMeta);
        editorMeta[inputPageControl.activePageIndex].editor.SetFocus;
        exit;
      end;
    end;
    if command=ecUserDefinedFirst+2 then begin
      for i:=length(editorMeta)-1 downto 1 do if editorMeta[(i+inputPageControl.activePageIndex) mod length(editorMeta)].sheet.tabVisible then begin
        inputPageControl.activePageIndex:=(i+inputPageControl.activePageIndex) mod length(editorMeta);
        editorMeta[inputPageControl.activePageIndex].editor.SetFocus;
        exit;
      end;
    end;
    if (command=ecUserDefinedFirst+3) then begin
      editorMeta[inputPageControl.activePageIndex].toggleBreakpoint;
      runEvaluator.context.clearBreakpoints;
      for i:=0 to length(editorMeta)-1 do editorMeta[i].setStepperBreakpoints;
      miDebug.Checked:=true;
      updateDebugParts;
    end;
  end;

PROCEDURE TMnhForm.MenuItem4Click(Sender: TObject);
  begin
    if inputPageControl.activePageIndex<0 then exit;
    askForm.initWithQuestion('Please give command line parameters');
    if askForm.ShowModal=mrOk then begin
      doStartEvaluation(false);
      lastStart.mainCall:=true;
      lastStart.parameters:=askForm.getLastAnswerReleasing(nil);
      with editorMeta[inputPageControl.activePageIndex] do runEvaluator.callMain(pseudoName,editor.lines,lastStart.parameters,preferredContextType);
    end else askForm.getLastAnswerReleasing(nil);
  end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  begin
    inputPageControl.activePageIndex:=addEditorMetaForNewFile();
  end;

PROCEDURE TMnhForm.miCloseClick(Sender: TObject);
  VAR i,mr:longint;
  begin
    if (inputPageControl.activePageIndex<0) or (inputPageControl.activePageIndex>=length(editorMeta)) then exit;
    with editorMeta[inputPageControl.activePageIndex] do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(inputPageControl.activePageIndex)) then exit;
        if mr=mrCancel then exit;
      end;
      if isFile then begin
        settings.value^.workspace.fileHistory.fileClosed(fileInfo.filePath);
        processFileHistory;
      end;
      closeEditor;
    end;

    mr:=-1;
    for i:=0 to length(editorMeta)-1 do if editorMeta[i].sheet.tabVisible then mr:=i;
    if mr=-1 then inputPageControl.activePageIndex:=addEditorMetaForNewFile()
             else inputPageControl.activePageIndex:=mr;
  end;

PROCEDURE TMnhForm.miDebugCancelClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
  end;

PROCEDURE TMnhForm.miDebugClick(Sender: TObject);
  begin
    miDebug.Checked:=not(miDebug.Checked);
    if miDebug.Checked then miProfile.Checked:=true;
    updateDebugParts;
  end;

PROCEDURE TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin
    if settingsReady then begin
      SettingsForm.fontSize:=SettingsForm.fontSize-1;
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

PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    if not(runEvaluator.evaluationRunningOrPending) then begin
      doStartEvaluation(false);
      lastStart.mainCall:=false;
      with editorMeta[inputPageControl.activePageIndex] do runEvaluator.evaluate(pseudoName,editor.lines,preferredContextType);
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

PROCEDURE TMnhForm.miFileHistory0Click(Sender: TObject); begin openFromHistory(0); end;
PROCEDURE TMnhForm.miFileHistory1Click(Sender: TObject); begin openFromHistory(1); end;
PROCEDURE TMnhForm.miFileHistory2Click(Sender: TObject); begin openFromHistory(2); end;
PROCEDURE TMnhForm.miFileHistory3Click(Sender: TObject); begin openFromHistory(3); end;
PROCEDURE TMnhForm.miFileHistory4Click(Sender: TObject); begin openFromHistory(4); end;
PROCEDURE TMnhForm.miFileHistory5Click(Sender: TObject); begin openFromHistory(5); end;
PROCEDURE TMnhForm.miFileHistory6Click(Sender: TObject); begin openFromHistory(6); end;
PROCEDURE TMnhForm.miFileHistory7Click(Sender: TObject); begin openFromHistory(7); end;
PROCEDURE TMnhForm.miFileHistory8Click(Sender: TObject); begin openFromHistory(8); end;
PROCEDURE TMnhForm.miFileHistory9Click(Sender: TObject); begin openFromHistory(9); end;

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

PROCEDURE TMnhForm.miFileHistory10Click(Sender: TObject); begin openFromHistory(10); end;
PROCEDURE TMnhForm.miFileHistory11Click(Sender: TObject); begin openFromHistory(11); end;
PROCEDURE TMnhForm.miFileHistory12Click(Sender: TObject); begin openFromHistory(12); end;
PROCEDURE TMnhForm.miFileHistory13Click(Sender: TObject); begin openFromHistory(13); end;
PROCEDURE TMnhForm.miFileHistory14Click(Sender: TObject); begin openFromHistory(14); end;
PROCEDURE TMnhForm.miFileHistory15Click(Sender: TObject); begin openFromHistory(15); end;
PROCEDURE TMnhForm.miFileHistory16Click(Sender: TObject); begin openFromHistory(16); end;
PROCEDURE TMnhForm.miFileHistory17Click(Sender: TObject); begin openFromHistory(17); end;
PROCEDURE TMnhForm.miFileHistory18Click(Sender: TObject); begin openFromHistory(18); end;
PROCEDURE TMnhForm.miFileHistory19Click(Sender: TObject); begin openFromHistory(19); end;

PROCEDURE TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
  end;

PROCEDURE TMnhForm.miHelpClick(Sender: TObject);
  begin
    miHelp.Checked:=not(miHelp.Checked);
    if not(miHelp.Checked) then helpPopupMemo.visible:=false
                           else begin
                             helpPopupMemo.visible:=true;
                             inputEditReposition(editorMeta[inputPageControl.activePageIndex].editor.CaretXY,false,false);
                           end;
  end;

PROCEDURE TMnhForm.miHelpExternallyClick(Sender: TObject);
  begin
    makeAndShowDoc(false);
  end;

PROCEDURE TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin
    if settingsReady then begin
      SettingsForm.fontSize:=SettingsForm.fontSize+1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm.miLangMnhClick(Sender: TObject);
  VAR i,editorIdx:longint;
  begin
    editorIdx:=inputPageControl.PageIndex;
    if (editorIdx<0) or (editorIdx>=length(editorMeta)) then exit;
    for i:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[i].menuItem.Checked then begin
      editorMeta[editorIdx].setLanguage(fileTypeMeta[i].language);
      exit;
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
        5: miMinErrorlevel5.Checked:=true;
      end;
      for j:=1 to 5 do guiOutAdapter.enableMessageType(j>=i,C_errorMessageTypes[j]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

FUNCTION TMnhForm._doSaveAs_(CONST index: longint): boolean;
  begin
    if index<0 then exit(false);
    if SaveDialog.execute then with editorMeta[index] do begin
      caption:=saveFile(SaveDialog.fileName);
      result:=true;
    end else result:=false;
  end;

FUNCTION TMnhForm._doSave_(CONST index: longint): boolean;
  begin
    if index<0 then exit(false);
    with editorMeta[index] do if not(isFile) then result:=_doSaveAs_(index)
    else begin
      caption:=saveFile();
      result:=true;
    end;
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
      for i:=0 to length(editorMeta)-1 do editorMeta[i].editor.Gutter.MarksPart.visible:=true;
      DebugToolbar.visible:=true;
      DebugToolbar.enabled:=true;
      DebugToolbar.top:=0;
      isPaused:=runEvaluator.context.paused;
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
      for i:=0 to length(editorMeta)-1 do editorMeta[i].editor.Gutter.MarksPart.visible:=false;
      outputPageControl.activePage:=outputTabSheet;
      DebugToolbar.visible:=false;
      DebugToolbar.enabled:=false;
    end;
  end;

FUNCTION TMnhForm.preferredContextType:T_contextType;
  begin
    if miDebug.Checked then result:=ct_debugging
    else if reEvaluationWithGUIrequired and profilingRun or miProfile.Checked
    then result:=ct_profiling
    else result:=ct_normal;
  end;

FUNCTION treeLit_canExpand(L:P_literal):boolean;
  begin
    result:=(L^.literalType in C_allListTypes) and (length(L^.toString(100))>=100);
  end;

FUNCTION treeLit_toString(L:P_literal):string;
  begin
    if treeLit_canExpand(L) then result:=L^.typeString
                            else result:=L^.toString();
  end;

PROCEDURE TMnhForm.variablesTreeViewExpanding(Sender: TObject; node: TTreeNode; VAR AllowExpansion: boolean);
  PROCEDURE addChildren(node2:TTreeNode);
    VAR k:longint;
        L:P_literal;
    begin
      for k:=0 to P_listLiteral(node2.data)^.size-1 do begin
        L:=P_listLiteral(node2.data)^.value(k);
        variablesTreeView.items.AddChild(node2,treeLit_toString(L)).data:=L;
      end;
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
        variablesTreeView.items.AddChild(newNode,treeLit_toString(P_listLiteral(value)^.value(i))).data:=P_listLiteral(value)^.value(i);
    end;

  begin
    if not(runEvaluator.context.paused and runEvaluator.evaluationRunning) then exit;
    snapshot:=runEvaluator.context.getDebuggingSnapshot;
    stackIdx:=length(snapshot.callStack)-1-callStackList.ItemIndex;
    variablesTreeView.items.clear;

    if (stackIdx>=0) and (stackIdx<length(snapshot.callStack)) then begin
      callStackInfoStringGrid.Cells[1,0]:=ansistring(snapshot.callStack[stackIdx].callerLocation);
      if snapshot.callStack[stackIdx].calleeLiteral=nil
      then callStackInfoStringGrid.Cells[1,1]:=snapshot.callStack[stackIdx].callee^.getId
      else callStackInfoStringGrid.Cells[1,1]:=snapshot.callStack[stackIdx].calleeLiteral^.toString(200);
      callStackInfoStringGrid.Cells[1,2]:=ansistring(snapshot.callStack[stackIdx].callee^.getLocation);
      addVariable('$params',snapshot.callStack[stackIdx].callParameters);
    end;

    if (callStackList.ItemIndex<=0) then begin
      report.create;
      runEvaluator.reportVariables(report);
      runEvaluator.context.reportVariables(report);
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
        editorMeta[pageIdx].editor.CaretXY:=newCaret;
        debugLine.editor:=editorMeta[pageIdx].editor;
        debugLine.line:=newCaret.y;
        editorMeta[pageIdx].editor.Repaint;
      end;
    end;

  VAR i:longint;
  begin
    if not(runEvaluator.context.paused) then exit;
    updateDebugParts;

    snapshot:=runEvaluator.context.getDebuggingSnapshot;

    breakPointHandlingPending:=false;
    jumpToFile;


    variablesTreeView.items.clear;
    for i:=0 to callStackInfoStringGrid.RowCount-1 do callStackInfoStringGrid.Cells[1,i]:='';

    callStackList.items.clear;
    for i:=length(snapshot.callStack)-1 downto 0 do
    callStackList.items.add(snapshot.callStack[i].callee^.getId);
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
    if not(runEvaluator.context.paused and runEvaluator.evaluationRunning) then exit;
    snapshot:=runEvaluator.context.getDebuggingSnapshot;
    lines:=currentExpressionMemo.LinesInWindow;
    chars:=currentExpressionMemo.charsInWindow;
    if (lines*chars<50) then begin
      lines:=1;
      chars:=50;
    end;

    currentExpressionMemo.lines.clear;
    tokens:=tokenSplit(tokenStackToString(snapshot.tokenStack,snapshot.first,round(lines*chars*0.9)));

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

FUNCTION TMnhForm.addEditorMetaForNewFile(CONST newFileName: ansistring): longint;
  VAR i:longint;
  begin
    i:=length(editorMeta)-1;
    //decrease i until a visible meta is encountered
    while (i>=0) and not(editorMeta[i].sheet.tabVisible) do dec(i);
    inc(i);
    //i now is the index of the last visible editor meta +1
    if (i>=0) and (i<length(editorMeta)) then begin
      editorMeta[i].initForNewFile;
      exit(i);
    end;

    i:=length(editorMeta)-1;
    while (i>=0) and (editorMeta[i].sheet.visible) do dec(i);
    i:=length(editorMeta);
    setLength(editorMeta,i+1);
    editorMeta[i].create(i);
    editorMeta[i].editor.Font:=OutputEdit.Font;
    if newFileName<>'' then _doSave_(i);
    result:=i;
    if miDebug.Checked then editorMeta[i].editor.Gutter.MarksPart.visible:=true;
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
      for i:=0 to length(editorMeta)-1 do if (editorMeta[i].sheet.tabVisible) and (editorMeta[i].pseudoName=fileName) then exit(i);
      result:=-1;
    end else begin
      filePath:=expandFileName(fileName);
      for i:=0 to length(editorMeta)-1 do if (editorMeta[i].sheet.tabVisible) and (editorMeta[i].fileInfo.filePath=filePath) then exit(i);
      result:=addEditorMetaForNewFile();
      editorMeta[result].setFile(filePath);
      editorMeta[result].editor.Font:=OutputEdit.Font;
      enableMenuForLanguage(editorMeta[result].language);
    end;
  end;

PROCEDURE TMnhForm.enableMenuForLanguage(CONST languageIndex:byte);
  VAR i:longint;
  begin
    for i:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[i].language=languageIndex then fileTypeMeta[i].menuItem.Checked:=true;
  end;

PROCEDURE TMnhForm.miMinErrorlevel1Click(Sender: TObject); begin _setErrorlevel_(1); end;
PROCEDURE TMnhForm.miMinErrorlevel2Click(Sender: TObject); begin _setErrorlevel_(2); end;
PROCEDURE TMnhForm.miMinErrorlevel3Click(Sender: TObject); begin _setErrorlevel_(3); end;
PROCEDURE TMnhForm.miMinErrorlevel4Click(Sender: TObject); begin _setErrorlevel_(4); end;
PROCEDURE TMnhForm.miMinErrorlevel5Click(Sender: TObject); begin _setErrorlevel_(5); end;

PROCEDURE TMnhForm.miOpenClick(Sender: TObject);
  begin
    OpenDialog.FilterIndex:=1;
    OpenDialog.options:=OpenDialog.options+[ofPathMustExist,ofFileMustExist];
    OpenDialog.title:='Open file';
    if OpenDialog.execute and fileExists(OpenDialog.fileName)
    then inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(OpenDialog.fileName);
  end;

PROCEDURE TMnhForm.miOpenDocumentationPackClick(Sender: TObject);
  begin
    makeAndShowDoc(true);
  end;

PROCEDURE TMnhForm.miProfileClick(Sender: TObject);
  begin
    miProfile.Checked:=not(miProfile.Checked) or miDebug.Checked;
  end;

PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    _doSaveAs_(inputPageControl.activePageIndex);
  end;

PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin
    _doSave_(inputPageControl.activePageIndex);
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
  end;

PROCEDURE TMnhForm.mi_insertFilenameClick(Sender: TObject);
  begin
    OpenDialog.FilterIndex:=2;
    OpenDialog.options:=OpenDialog.options-[ofPathMustExist,ofFileMustExist];
    if OpenDialog.execute then editorMeta[inputPageControl.activePageIndex].insertText(escapeString(OpenDialog.fileName,es_pickShortest));
  end;

PROCEDURE TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;

PROCEDURE TMnhForm.OutputEditKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if ((key=13) and (ssCtrl in Shift)) then outputEditReposition(OutputEdit.CaretXY,true);
    if forceInputEditFocusOnOutputEditMouseUp and (inputPageControl.activePageIndex>=0) then ActiveControl:=editorMeta[inputPageControl.activePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.OutputEditMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    outputEditReposition(OutputEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.OutputEditMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=editorMeta[inputPageControl.activePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.inputPageControlChange(Sender: TObject);
  begin
    if (inputPageControl.activePageIndex>=0) and not(reEvaluationWithGUIrequired) then begin
      SynCompletion.editor:=editorMeta[inputPageControl.activePageIndex].editor;
      settings.value^.workspace.activePage:=inputPageControl.activePageIndex;
      with editorMeta[inputPageControl.activePageIndex] do if language=LANG_MNH then assistancEvaluator.evaluate(pseudoName,editor.lines);
      enableMenuForLanguage(editorMeta[inputPageControl.activePageIndex].language);
    end;
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
      if workspace.fileHistory.polishHistory then processFileHistory;
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

PROCEDURE TMnhForm.PopupNotifier1Close(Sender: TObject;
  VAR CloseAction: TCloseAction);
  begin
    miHelp.Checked:=false;
  end;

PROCEDURE TMnhForm.Splitter1Moved(Sender: TObject);
  begin
    if helpPopupMemo.visible then positionHelpNotifier;
  end;

PROCEDURE TMnhForm.Splitter3Moved(Sender: TObject);
  begin
    updateExpressionMemo;
  end;

PROCEDURE TMnhForm.SynCompletionCodeCompletion(VAR value: string;
  sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
  begin
    value:=copy(sourceValue,1,LastDelimiter('.',sourceValue)-1)+value;
    wordsInEditor.clear;
  end;

PROCEDURE TMnhForm.ensureWordsInEditorForCompletion;
  VAR caret:TPoint;
      i:longint;
  begin
    with editorMeta[inputPageControl.activePageIndex] do caret:=editor.CaretXY;
    if (wordsInEditor.size>0) and (lastWordsCaret=caret.y) then exit;
    lastWordsCaret:=caret.y;
    wordsInEditor.clear;
    with editorMeta[inputPageControl.activePageIndex] do begin
      for i:=0 to caret.y-1 do
        if i=caret.y-1 then collectIdentifiers(editor.lines[i],wordsInEditor,caret.x)
                       else collectIdentifiers(editor.lines[i],wordsInEditor,-1);
    end;
    assistancEvaluator.extendCompletionList(wordsInEditor);
    wordsInEditor.unique;
  end;

PROCEDURE TMnhForm.SynCompletionExecute(Sender: TObject);
  VAR i:longint;
      s:string;
  begin
    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    for i:=0 to wordsInEditor.size-1 do
      if (s='') or (pos(s,wordsInEditor[i])=1) then SynCompletion.ItemList.add(wordsInEditor[i]);
  end;

PROCEDURE TMnhForm.SynCompletionSearchPosition(VAR APosition: integer);
  VAR i:longint;
      s:string;
  begin
    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    i:=LastDelimiter('.',s);
    if i>1 then begin
      s:=copy(s,i,length(s));
      SynCompletion.CurrentString:=s;
    end;
    for i:=0 to wordsInEditor.size-1 do
      if pos(s,wordsInEditor[i])=1 then SynCompletion.ItemList.add(wordsInEditor[i]);
    if SynCompletion.ItemList.count>0 then APosition:=0 else APosition:=-1;
  end;

PROCEDURE TMnhForm.tbMicroStepClick(Sender: TObject);
  begin
    runEvaluator.context.doMicrostep;
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  CONST MIN_INTERVALL=1;
        MAX_INTERVALL=250;
  VAR aid:ansistring;
      isEvaluationRunning:boolean;
      flushPerformed:boolean=false;
      canRun:boolean;
      i,modalRes:longint;
  begin
    isEvaluationRunning:=runEvaluator.evaluationRunning;
    if askForm.displayPending then askForm.Show;
    if showing then begin
      if not (reEvaluationWithGUIrequired) then begin
        //Form caption:-------------------------------------------------------------
        if (inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta))
        then begin
          aid:=editorMeta[inputPageControl.activePageIndex].updateSheetCaption;
          editorMeta[inputPageControl.activePageIndex].repaintWithStateCounter(assistancEvaluator.getStateCounter,assistancEvaluator.getErrorHints);
        end else aid:=APP_TITLE;
        if aid<>caption then caption:=aid;
        //-------------------------------------------------------------:Form caption
      end;
      //progress time:------------------------------------------------------------
      if inputPageControl.activePageIndex>=0
      then aid:=C_tabChar+intToStr(editorMeta[inputPageControl.activePageIndex].editor.CaretY)+','+intToStr(editorMeta[inputPageControl.activePageIndex].editor.CaretX)
      else aid:='';
      if isEvaluationRunning then begin
        if runEvaluator.context.hasOption(cp_debug) then begin
          if runEvaluator.context.paused then begin
            StatusBar.SimpleText:='Debugging [HALTED]'+aid;
            if breakPointHandlingPending then handleBreak;
          end else StatusBar.SimpleText:='Debugging...'+aid;
        end else StatusBar.SimpleText:='Evaluating...'+aid;
      end else StatusBar.SimpleText:=runEvaluator.getEndOfEvaluationText+aid;
      //------------------------------------------------------------:progress time
      //Halt/Run enabled states:--------------------------------------------------
      if isEvaluationRunning<>miHaltEvalutaion.enabled then miHaltEvalutaion.enabled:=isEvaluationRunning;
      canRun:=(inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta)) and (editorMeta[inputPageControl.activePageIndex].language=LANG_MNH) and not(isEvaluationRunning);
      if canRun<>miEvaluateNow.enabled then begin
        miEvaluateNow.enabled:=canRun;
        miCallMain.enabled:=canRun;
      end;
      //--------------------------------------------------:Halt/Run enabled states
      //File checks:------------------------------------------------------------
      if (now>doNotCheckFileBefore) then begin
        doNotCheckFileBefore:=now+1;
        for i:=0 to length(editorMeta)-1 do with editorMeta[i] do
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
      end;
      //-----------------------------------------------------------.:File checks
    end;

    flushPerformed:=guiOutAdapter.flushToGui(OutputEdit);
    if flushPerformed and (outputPageControl.activePage=assistanceTabSheet) then outputPageControl.activePage:=outputTabSheet;
    if guiAdapters.hasMessageOfType[mt_plotCreatedWithDeferredDisplay] and
       not(runEvaluator.evaluationRunning) then plotForm.doPlot();

    if not(flushPerformed) then begin
      UpdateTimeTimer.interval:=UpdateTimeTimer.interval+10;
      if UpdateTimeTimer.interval>MAX_INTERVALL then UpdateTimeTimer.interval:=MAX_INTERVALL;
    end else UpdateTimeTimer.interval:=MIN_INTERVALL;

    if not(reEvaluationWithGUIrequired) and settings.value^.savingRequested then begin
      for i:=0 to length(editorMeta)-1 do editorMeta[i].writeToEditorState(settings.value);
      saveSettings;
    end;

    if closeGuiFlag or reEvaluationWithGUIrequired and not(runEvaluator.evaluationRunning) and not(guiAdapters.hasMessageOfType[mt_el3_evalError]) and not(anyFormShowing) then close;
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

PROCEDURE TMnhForm.miFindClick(Sender: TObject);
  VAR wordUnderCursor:string;
  begin
    if OutputEdit.Focused then begin
      OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      wordUnderCursor:=OutputEdit.TextBetweenPoints[OutputEdit.BlockBegin,OutputEdit.BlockEnd];
      if wordUnderCursor='' then wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      outputFocusedOnFind:=true;
    end else begin
      with editorMeta[inputPageControl.activePageIndex] do begin
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
      with editorMeta[inputPageControl.activePageIndex] do begin
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

PROCEDURE TMnhForm.tbRunClick(Sender: TObject);
  begin
    if not(runEvaluator.evaluationRunning) then begin
      doStartEvaluation(false);
      if lastStart.mainCall then begin
        with editorMeta[inputPageControl.activePageIndex] do runEvaluator.callMain(pseudoName,editor.lines,lastStart.parameters,ct_debugging);
      end else begin
        with editorMeta[inputPageControl.activePageIndex] do runEvaluator.evaluate(pseudoName,editor.lines,ct_debugging);
      end;
    end else if runEvaluator.context.paused then runEvaluator.context.doContinue
                                            else runEvaluator.context.doMicrostep;
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.tbStepInClick(Sender: TObject);
  begin
    runEvaluator.context.doStepInto;
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.tbStepClick(Sender: TObject);
  begin
    runEvaluator.context.doStep;
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.tbStepOutClick(Sender: TObject);
  begin
    runEvaluator.context.doStepOut;
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.tbStopClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
  end;

PROCEDURE TMnhForm.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
    Special:=runEvaluator.context.hasOption(cp_debug) and runEvaluator.evaluationRunning and (Sender=debugLine.editor) and (line=debugLine.line);
  end;

FUNCTION TMnhForm.editForSearch(CONST replacing: boolean): TSynEdit;
  begin
    if outputFocusedOnFind and not(replacing) then exit(OutputEdit);
    if (inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta))
    then result:=editorMeta[inputPageControl.activePageIndex].editor
    else exit(OutputEdit); //not nice, but a valid fallback
  end;

PROCEDURE TMnhForm.setEditorMode(CONST enable:boolean);
  begin
    if enable then begin
      outputPageControl.Align:=alBottom;
      Splitter1.visible:=true;
      Splitter1.enabled:=true;
      inputPageControl.visible:=true;
      inputPageControl.enabled:=true;
      reEvaluationWithGUIrequired:=false;
      Show;
    end else begin
      inputPageControl.visible:=false;
      inputPageControl.enabled:=false;
      outputPageControl.Align:=alClient;
      Splitter1.visible:=false;
      Splitter1.enabled:=false;
      caption:=APP_TITLE+' '+getFileOrCommandToInterpretFromCommandLine;
      Hide;
    end;
    subMenuFile.enabled:=enable;
    subMenuFile.visible:=enable;

    subMenuEvaluation.enabled:=enable;
    subMenuEvaluation.visible:=enable;

    subMenuHelp.enabled:=enable;
    subMenuHelp.visible:=enable;

    subMenuCode.enabled:=enable;
    subMenuCode.visible:=enable;

    outputPageControl.ShowTabs:=enable;
  end;

PROCEDURE TMnhForm.processSettings;
  VAR formPosition:T_formPosition;
      i,j:longint;
      maxLevel:longint=5;
  begin
    if not(settingsReady) then begin
      formPosition:=settings.value^.mainForm;
      top   :=formPosition.top;
      Left  :=formPosition.Left;
      width :=formPosition.width;
      height:=formPosition.height;
      if formPosition.isFullscreen then WindowState:=wsMaximized;

      miDeclarationEcho .Checked:=mt_echo_declaration in settings.value^.outputBehaviour;
      miExpressionEcho  .Checked:=mt_echo_input       in settings.value^.outputBehaviour;
      miExpressionResult.Checked:=mt_echo_output      in settings.value^.outputBehaviour;
      miTimingInfo      .Checked:=mt_timing_info      in settings.value^.outputBehaviour;;
      miWrapEcho.Checked:=settings.value^.wordWrapEcho;
      for j:=5 downto 1 do if (C_errorMessageTypes[j] * settings.value^.outputBehaviour <> []) then maxLevel:=j;
      miMinErrorlevel1.Checked:=maxLevel=1;
      miMinErrorlevel2.Checked:=maxLevel=2;
      miMinErrorlevel3.Checked:=maxLevel=3;
      miMinErrorlevel4.Checked:=maxLevel=4;
      miMinErrorlevel5.Checked:=maxLevel>=5;
      guiOutAdapter.outputBehavior:=settings.value^.outputBehaviour;

      if reEvaluationWithGUIrequired
      then setupOutputBehaviourFromCommandLineOptions(guiAdapters,@guiOutAdapter)
      else setupOutputBehaviourFromCommandLineOptions(guiAdapters,nil);

      processFileHistory;
      SettingsForm.ensureFont(OutputEdit.Font);

      setLength(editorMeta,length(settings.value^.workspace.editorState));
      runEvaluator.context.clearBreakpoints;
      for i:=0 to length(editorMeta)-1 do begin
        editorMeta[i].create(i,settings.value^.workspace.editorState[i]);
        editorMeta[i].setStepperBreakpoints;
      end;

      i:=settings.value^.workspace.activePage;
      inputPageControl.activePageIndex:=i;
      if (i>=0) and (i<length(editorMeta)) then SynCompletion.editor:=editorMeta[inputPageControl.activePageIndex].editor;
      {$ifdef UNIX}
      miIncFontSize.ShortCut:=16605;
      {$endif}
      for i:=0 to length(filesToOpenInEditor)-1 do FormDropFiles(nil,filesToOpenInEditor[i]);

      settingsReady:=true;
      if not(reEvaluationWithGUIrequired) and (inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta)) then with editorMeta[inputPageControl.activePageIndex] do assistancEvaluator.evaluate(pseudoName,editor.lines);
    end;

    OutputEdit.Font.name:=settings.value^.editorFontname;
    OutputEdit.Font.size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then OutputEdit.Font.quality:=fqCleartypeNatural
    else OutputEdit.Font.quality:=fqNonAntialiased;
    for i:=0 to length(editorMeta)-1 do editorMeta[i].editor.Font:=OutputEdit.Font;

    currentExpressionMemo.Font:=OutputEdit.Font;
    assistanceSynEdit.Font:=OutputEdit.Font;
    helpPopupMemo.Font:=OutputEdit.Font;
    helpPopupMemo.Font.size:=helpPopupMemo.Font.size-2;
  end;

PROCEDURE TMnhForm.processFileHistory;
  FUNCTION historyMenuItem(index:byte):TMenuItem;
    begin
      case index of
        0: result:=miFileHistory0;
        1: result:=miFileHistory1;
        2: result:=miFileHistory2;
        3: result:=miFileHistory3;
        4: result:=miFileHistory4;
        5: result:=miFileHistory5;
        6: result:=miFileHistory6;
        7: result:=miFileHistory7;
        8: result:=miFileHistory8;
        9: result:=miFileHistory9;
        10: result:=miFileHistory10;
        11: result:=miFileHistory11;
        12: result:=miFileHistory12;
        13: result:=miFileHistory13;
        14: result:=miFileHistory14;
        15: result:=miFileHistory15;
        16: result:=miFileHistory16;
        17: result:=miFileHistory17;
        18: result:=miFileHistory18;
        19: result:=miFileHistory19;
      else result:=nil;
      end;
    end;
  VAR i:longint;
  begin
    for i:=0 to 19 do if settings.value^.workspace.fileHistory.historyItem(i)='' then begin
      historyMenuItem(i).enabled:=false;
      historyMenuItem(i).visible:=false;
    end else begin
      historyMenuItem(i).enabled:=true;
      historyMenuItem(i).visible:=true;
      historyMenuItem(i).caption:=intToStr(i)+': '+settings.value^.workspace.fileHistory.historyItem(i);
    end;
  end;

PROCEDURE lateInitialization;
  begin
    SynHighlighterMnh.initLists;

    guiOutAdapter.create;
    guiAdapters.create;
    mnh_plotForm.guiAdapters:=@guiAdapters;

    guiAdapters.addOutAdapter(@guiOutAdapter,false);
    registerRule(SYSTEM_BUILTIN_NAMESPACE,'ask', @ask_impl,'');
    mnh_evalThread.initUnit(@guiAdapters);
    setupCallbacks;
  end;

PROCEDURE doFinalization;
  begin
    guiAdapters.destroy;
    guiOutAdapter.destroy;
  end;

end.


