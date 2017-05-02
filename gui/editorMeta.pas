UNIT editorMeta;
INTERFACE
USES  //basic classes
  Classes, sysutils, FileUtil, LazUTF8, LCLType, lclintf, types,
  //my utilities:
  mnhFormHandler, myStringUtil, myGenerics,
  //GUI: LCL components
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, Grids, StdCtrls, simpleipc,
  //GUI: SynEdit
  SynEdit, SynEditTypes, SynCompletion, SynPluginMultiCaret, SynEditMiscClasses, SynMemo, SynGutterMarks, SynEditMarks, SynEditKeyCmds,
  SynEditMarkupSpecialLine,
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
  runnerModel,
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


TYPE
P_editorMeta=^T_editorMeta;
T_editorMeta=object(T_codeProvider)
  private
    index:longint;
    paintedWithState:T_hashInt;
    fileInfo:record
      filePath:ansistring;
      fileAccessAge:double;
      isChanged:boolean;
    end;
    language_:byte;


    sheet       : TTabSheet;
    editor_     : TSynEdit;
    plugin      : TSynPluginMultiCaret;
    highlighter : TSynMnhSyn;
    PROCEDURE setLanguage(CONST languageIndex:byte);
    PROCEDURE setLanguage(CONST extensionWithoutDot:string; CONST fallback:byte);
    PROCEDURE guessLanguage(CONST fallback:byte);
    FUNCTION languageName:string;

  public
    CONSTRUCTOR create(CONST idx:longint);
    CONSTRUCTOR create(CONST idx:longint; VAR state:T_editorState);

    DESTRUCTOR destroy; virtual;
    //T_codeProvider:
    FUNCTION getLines: T_arrayOfString; virtual;
    FUNCTION getPath: ansistring;                                    virtual;
    FUNCTION stateHash:T_hashInt;                                    virtual;
    FUNCTION disposeOnPackageDestruction:boolean;                    virtual;
    FUNCTION isPseudoFile:boolean;                                   virtual;

    PROPERTY language:byte read language_ write setLanguage;
    PROPERTY editor:TSynEdit read editor_;
    PROCEDURE activate;
    FUNCTION caretInMainFormCoordinates:TPoint;
    PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret:TPoint);
    PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean);
    PROCEDURE setCaret(CONST location: T_searchTokenLocation);
    PROCEDURE toggleComment;
    PROCEDURE toggleBreakpoint;
  private
    PROCEDURE initWithState(VAR state:T_editorState);
  PROCEDURE closeEditor;

  //Events handling:
  PROCEDURE InputEditChange(Sender: TObject);

  //Advanced:
  FUNCTION saveAsWithDialog:boolean;
  FUNCTION saveWithDialog:boolean;


  //Presentation helper:

  FUNCTION isFile:boolean;


  PROCEDURE setFile(CONST fileName:string);
  PROCEDURE initForNewFile;
  PROCEDURE reloadFile(CONST fileName:string);
  PROCEDURE setMarkedWord(CONST wordText:string);
  PROCEDURE setWorkingDir;
  PROCEDURE writeToEditorState(CONST settings:P_Settings);
  PROCEDURE insertText(CONST s:string);
  FUNCTION pseudoName(CONST short:boolean=false):ansistring;
  PROCEDURE setStepperBreakpoints;
  PROCEDURE _add_breakpoint_(CONST lineIndex:longint);
  FUNCTION updateSheetCaption:ansistring;
  PROCEDURE repaintWithStateCounter(CONST stateCounter:longint; CONST errorHints:T_arrayOfString);
  FUNCTION changed:boolean;
  FUNCTION saveFile(CONST fileName:string=''):string;
  FUNCTION fileIsDeleted:boolean;
  FUNCTION fileIsModifiedOnFileSystem:boolean;
  PROCEDURE updateContentAfterEditScript(CONST stringListLiteral:P_listLiteral);
  PROCEDURE exportToHtml;
end;

PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
                    CONST p_inputPageControl      :TPageControl;
                    CONST p_EditorPopupMenu       :TPopupMenu;
                    CONST p_SaveDialog            :TSaveDialog;
                    CONST p_breakpointsImagesList :TImageList;
                    CONST p_assistanceSynEdit     :TSynEdit;
                    CONST p_SynCompletion         :TSynCompletion;
                    CONST outputHighlighter       :TSynMnhSyn;
                    CONST p_EditKeyUp             :TKeyEvent;
                    CONST p_EditMouseDown         :TMouseEvent;
                    CONST p_EditProcessUserCommand:TProcessCommandEvent;
                    CONST p_EditSpecialLineMarkup :TSpecialLineMarkupEvent);

FUNCTION hasEditor:boolean;
FUNCTION getEditor:P_editorMeta;
FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):longint;

PROCEDURE updateFonts;
FUNCTION allPseudoNames:T_arrayOfString;
FUNCTION getMeta(CONST nameOrPseudoName:string):P_editorMeta;
PROCEDURE storeEditorsToSettings;
FUNCTION getHelpPopupText:string;
FUNCTION getHelpLocation:T_searchTokenLocation;
PROCEDURE cycleEditors(CONST cycleForward:boolean);
PROCEDURE updateEditorsByGuiStatus;
IMPLEMENTATION
VAR mainForm              :T_abstractMnhForm;
    inputPageControl      :TPageControl;
    EditorPopupMenu       :TPopupMenu;
    SaveDialog            :TSaveDialog;
    breakpointsImagesList :TImageList;
    EditKeyUp             :TKeyEvent;
    EditMouseDown         :TMouseEvent;
    EditProcessUserCommand:TProcessCommandEvent;
    EditSpecialLineMarkup :TSpecialLineMarkupEvent;
    assistanceSynEdit     :TSynEdit;
    SynCompletion1        :TSynCompletion;

VAR fileTypeMeta:array of record
      extensionWithoutDot:string;
      language:byte;
      highlighter:TSynCustomHighlighter;
    end;
VAR editorMetaData:array of P_editorMeta;
    dummyEditor:T_editorMeta;
    underCursor:T_tokenInfo;

PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
                    CONST p_inputPageControl      :TPageControl;
                    CONST p_EditorPopupMenu       :TPopupMenu;
                    CONST p_SaveDialog            :TSaveDialog;
                    CONST p_breakpointsImagesList :TImageList;
                    CONST p_assistanceSynEdit     :TSynEdit;
                    CONST p_SynCompletion         :TSynCompletion;
                    CONST outputHighlighter       :TSynMnhSyn;
                    CONST p_EditKeyUp             :TKeyEvent;
                    CONST p_EditMouseDown         :TMouseEvent;
                    CONST p_EditProcessUserCommand:TProcessCommandEvent;
                    CONST p_EditSpecialLineMarkup :TSpecialLineMarkupEvent);

  VAR SynBatSyn1            : TSynBatSyn            ;
      SynCppSyn1            : TSynCppSyn            ;
      SynCssSyn1            : TSynCssSyn            ;
      SynDiffSyn1           : TSynDiffSyn           ;
      SynFreePascalSyn1     : TSynFreePascalSyn     ;
      SynHTMLSyn1           : TSynHTMLSyn           ;
      SynIniSyn1            : TSynIniSyn            ;
      SynJScriptSyn1        : TSynJScriptSyn        ;
      SynJavaSyn1           : TSynJavaSyn           ;
      SynPHPSyn1            : TSynPHPSyn            ;
      SynPerlSyn1           : TSynPerlSyn           ;
      SynPythonSyn1         : TSynPythonSyn         ;
      SynSQLSyn1            : TSynSQLSyn            ;
      SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
      SynVBSyn1             : TSynVBSyn             ;
      SynXMLSyn1            : TSynXMLSyn            ;

  PROCEDURE initHighlighters;
    begin
      SynBatSyn1            :=TSynBatSyn            .create(mainForm);
      SynCppSyn1            :=TSynCppSyn            .create(mainForm);
      SynCssSyn1            :=TSynCssSyn            .create(mainForm);
      SynDiffSyn1           :=TSynDiffSyn           .create(mainForm);
      SynFreePascalSyn1     :=TSynFreePascalSyn     .create(mainForm);
      SynHTMLSyn1           :=TSynHTMLSyn           .create(mainForm);
      SynIniSyn1            :=TSynIniSyn            .create(mainForm);
      SynJScriptSyn1        :=TSynJScriptSyn        .create(mainForm);
      SynJavaSyn1           :=TSynJavaSyn           .create(mainForm);
      SynPHPSyn1            :=TSynPHPSyn            .create(mainForm);
      SynPerlSyn1           :=TSynPerlSyn           .create(mainForm);
      SynPythonSyn1         :=TSynPythonSyn         .create(mainForm);
      SynSQLSyn1            :=TSynSQLSyn            .create(mainForm);
      SynUNIXShellScriptSyn1:=TSynUNIXShellScriptSyn.create(mainForm);
      SynVBSyn1             :=TSynVBSyn             .create(mainForm);
      SynXMLSyn1            :=TSynXMLSyn            .create(mainForm);
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

  PROCEDURE initFileTypes;
    PROCEDURE addFileType(CONST extension:string; CONST language:byte; CONST highlighter:TSynCustomHighlighter);
      begin
        setLength(fileTypeMeta,length(fileTypeMeta)+1);
        fileTypeMeta[length(fileTypeMeta)-1].extensionWithoutDot:=uppercase(extension);
        fileTypeMeta[length(fileTypeMeta)-1].language   :=language;
        fileTypeMeta[length(fileTypeMeta)-1].highlighter:=highlighter;
      end;

    begin
      addFileType('mnh' ,LANG_MNH   ,nil                   );
      addFileType('cpp' ,LANG_CPP   ,SynCppSyn1            );
      addFileType('c'   ,LANG_CPP   ,SynCppSyn1            );
      addFileType('h'   ,LANG_CPP   ,SynCppSyn1            );
      addFileType('hh'  ,LANG_CPP   ,SynCppSyn1            );
      addFileType('css' ,LANG_CSS   ,SynCssSyn1            );
      addFileType('diff',LANG_DIFF  ,SynDiffSyn1           );
      addFileType('html',LANG_HTML  ,SynHTMLSyn1           );
      addFileType('ini' ,LANG_INI   ,SynIniSyn1            );
      addFileType('java',LANG_JAVA  ,SynJavaSyn1           );
      addFileType('js'  ,LANG_JS    ,SynJScriptSyn1        );
      addFileType('json',LANG_JS    ,SynJScriptSyn1        );
      addFileType('pas' ,LANG_PAS   ,SynFreePascalSyn1     );
      addFileType('perl',LANG_PERL  ,SynPerlSyn1           );
      addFileType('php' ,LANG_PHP   ,SynPHPSyn1            );
      addFileType('py'  ,LANG_PYTHON,SynPythonSyn1         );
      addFileType('sh'  ,LANG_SHELL ,SynUNIXShellScriptSyn1);
      addFileType('sql' ,LANG_SQL   ,SynSQLSyn1            );
      addFileType('vb'  ,LANG_VB    ,SynVBSyn1             );
      addFileType('bat' ,LANG_BAT   ,SynBatSyn1            );
      addFileType('xml' ,LANG_XML   ,SynXMLSyn1            );
      addFileType('txt' ,LANG_TXT   ,nil                   );
    end;

  PROCEDURE restoreEditors;
    VAR i:longint;
    begin
      setLength(editorMetaData,length(settings.value^.workspace.editorState));
      runEvaluator.context.stepper^.clearBreakpoints;
      for i:=0 to length(editorMetaData)-1 do begin
        new(editorMetaData[i],create(i,settings.value^.workspace.editorState[i]));
        editorMetaData[i]^.setStepperBreakpoints;
      end;
      i:=settings.value^.workspace.activePage;
      inputPageControl.activePageIndex:=i;
      inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(filesToOpenInEditor,true);
    end;

  begin
    mainForm              :=P_mainForm              ;
    inputPageControl      :=P_inputPageControl      ;
    EditorPopupMenu       :=P_EditorPopupMenu       ;
    SaveDialog            :=P_SaveDialog            ;
    breakpointsImagesList :=P_breakpointsImagesList ;
    EditKeyUp             :=P_EditKeyUp             ;
    EditMouseDown         :=P_EditMouseDown         ;
    EditProcessUserCommand:=P_EditProcessUserCommand;
    EditSpecialLineMarkup :=P_EditSpecialLineMarkup ;
    assistanceSynEdit     :=P_assistanceSynEdit     ;
    SynCompletion1        :=p_SynCompletion         ;
    initHighlighters;
    initFileTypes;
    restoreEditors;
  end;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint);
  PROCEDURE addKeystroke(CONST command:TSynEditorCommand; CONST ShortCut:TShortCut);
    VAR keyStroke:TSynEditKeyStroke;
    begin
      keyStroke:=plugin.Keystrokes.add;
      keyStroke.command:=command;
      keyStroke.ShortCut:=ShortCut;
    end;

  VAR style:TSynSelectedColor;
  begin
    paintedWithState:=0;
    index:=idx;
    sheet:=TTabSheet.create(inputPageControl);
    sheet.PageControl:=inputPageControl;

    editor_:=TSynEdit.create(mainForm);
    editor_.parent:=sheet;
    editor_.Align:=alClient;
    editor_.ScrollBars:=ssAutoBoth;
    editor_.WantTabs:=false;
    editor_.Gutter.MarksPart.visible:=false;
    editor_.Gutter.CodeFoldPart.visible:=false;
    plugin:=TSynPluginMultiCaret.create(editor_);
    plugin.editor:=editor_;
    plugin.Keystrokes.clear;
    style:=editor.BracketMatchColor;
    style.background:=clLime;
    editor_.OnChange:=@InputEditChange;
    editor_.OnKeyUp             :=EditKeyUp;
    editor_.OnMouseDown         :=EditMouseDown;
    editor_.OnProcessCommand    :=EditProcessUserCommand;
    editor_.OnProcessUserCommand:=EditProcessUserCommand;
    editor_.OnSpecialLineMarkup :=EditSpecialLineMarkup;
    editor_.Gutter.width:=34;
    editor_.RightEdge:=-1;
    editor_.Keystrokes.clear;
    addKeystroke(ecUp,38);
    addKeystroke(ecSelUp,8230);
    addKeystroke(ecScrollUp,16422);
    addKeystroke(ecDown,40);
    addKeystroke(ecSelDown,8232);
    addKeystroke(ecScrollDown,16424);
    addKeystroke(ecLeft,37);
    addKeystroke(ecSelLeft,8229);
    addKeystroke(ecWordLeft,16421);
    addKeystroke(ecSelWordLeft,24613);
    addKeystroke(ecRight,39);
    addKeystroke(ecSelRight,8231);
    addKeystroke(ecWordRight,16423);
    addKeystroke(ecSelWordRight,24615);
    addKeystroke(ecPageDown,34);
    addKeystroke(ecSelPageDown,8226);
    addKeystroke(ecUserDefinedFirst+1,16418);
    addKeystroke(ecSelPageBottom,24610);
    addKeystroke(ecPageUp,33);
    addKeystroke(ecSelPageUp,8225);
    addKeystroke(ecUserDefinedFirst+2,16417);
    addKeystroke(ecSelPageTop,24609);
    addKeystroke(ecLineStart,36);
    addKeystroke(ecSelLineStart,8228);
    addKeystroke(ecEditorTop,16420);
    addKeystroke(ecSelEditorTop,24612);
    addKeystroke(ecLineEnd,35);
    addKeystroke(ecSelLineEnd,8227);
    addKeystroke(ecEditorBottom,16419);
    addKeystroke(ecSelEditorBottom,24611);
    addKeystroke(ecToggleMode,45);
    addKeystroke(ecCopy,16429);
    addKeystroke(ecPaste,8237);
    addKeystroke(ecDeleteChar,46);
    addKeystroke(ecCut,8238);
    addKeystroke(ecDeleteLastChar,8);
    addKeystroke(ecDeleteLastChar,8200);
    addKeystroke(ecDeleteLastWord,16392);
    addKeystroke(ecUndo,32776);
    addKeystroke(ecRedo,40968);
    addKeystroke(ecLineBreak,13);
    addKeystroke(ecSelectAll,16449);
    addKeystroke(ecCopy,16451);
    addKeystroke(ecLineBreak,16461);
    addKeystroke(ecDeleteWord,16468);
    addKeystroke(ecBlockUnindent,16469);
    addKeystroke(ecPaste,16470);
    addKeystroke(ecCut,16472);
    addKeystroke(ecDeleteLine,16473);
    addKeystroke(ecUndo,16474);
    addKeystroke(ecRedo,24666);
    addKeystroke(ecUserDefinedFirst,24643);
    addKeystroke(ecLineSelect,24652);
    addKeystroke(ecTab,9);
    addKeystroke(ecShiftTab,8201);
    addKeystroke(ecUserDefinedFirst+3,24642);
    addKeystroke(ecColSelUp,40998);
    addKeystroke(ecColSelDown,41000);
    addKeystroke(ecColSelLeft,40997);
    addKeystroke(ecColSelRight,40999);
    addKeystroke(ecColSelPageDown,40994);
    addKeystroke(ecColSelPageBottom,57378);
    addKeystroke(ecColSelPageUp,40993);
    addKeystroke(ecColSelPageTop,57377);
    addKeystroke(ecColSelLineStart,40996);
    addKeystroke(ecColSelLineEnd,40995);
    addKeystroke(ecColSelEditorTop,57380);
    addKeystroke(ecColSelEditorBottom,57379);
    addKeystroke(ecBlockIndent,16457);

    highlighter:=TSynMnhSyn.create(mainForm,msf_input);
    editor_.highlighter:=highlighter;
    editor_.PopupMenu:=EditorPopupMenu;
    initForNewFile;
  end;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint; VAR state: T_editorState);
  begin
    create(idx);
    initWithState(state);
  end;

PROCEDURE T_editorMeta.initWithState(VAR state: T_editorState);
  VAR i:longint;
  begin
    sheet.tabVisible:=true;
    with fileInfo do begin
      isChanged    :=state.changed;
      fileAccessAge:=state.fileAccessAge;
      filePath     :=state.filePath;
    end;
    state.getLines(editor.lines);
    for i:=0 to length(state.markedLines)-1 do _add_breakpoint_(state.markedLines[i]);
    editor.CaretX:=state.caret['x'];
    editor.CaretY:=state.caret['y'];
    setLanguage(state.language);
    updateSheetCaption;
  end;

DESTRUCTOR T_editorMeta.destroy;
  begin
  end;

FUNCTION T_editorMeta.getLines: T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,editor.lines.count);
    for i:=0 to length(result)-1 do result[i]:=editor.lines[i];
  end;

FUNCTION T_editorMeta.getPath: ansistring;
  begin
    result:=pseudoName(false);
  end;

FUNCTION T_editorMeta.stateHash: T_hashInt;
  VAR s:ansistring;
  begin
    {$Q-}{$R-}
    result:=editor.lines.count;
    for s in editor.lines do result:=result*31+hashOfAnsiString(s);
    {$Q+}{$R+}
    if result=0 then result:=1;
  end;

FUNCTION T_editorMeta.disposeOnPackageDestruction: boolean;
  begin
    result:=false;
  end;

FUNCTION T_editorMeta.isPseudoFile: boolean;
  begin
    result:=fileInfo.filePath='';
  end;

PROCEDURE T_editorMeta.activate;
  begin
    if not(sheet.tabVisible) then exit;
    {$ifdef debugMode}
    writeln('Activating editor "',pseudoName(),'"');
    {$endif}
    editor_.Font:=assistanceSynEdit.Font;
    SynCompletion1.editor:=editor_;
    if language=LANG_MNH then assistancEvaluator.evaluate(@self);
    mainForm.caption:=updateSheetCaption;
  end;

PROCEDURE T_editorMeta.InputEditChange(Sender: TObject);
  begin
    if not(sheet.tabVisible) then exit;
    if language=LANG_MNH then assistancEvaluator.evaluate(@self);
    mainForm.caption:=updateSheetCaption;
  end;

FUNCTION T_editorMeta.saveAsWithDialog: boolean;
  begin
    if language=LANG_MNH
    then begin
      SaveDialog.FilterIndex:=1;
      SaveDialog.options:=SaveDialog.options+[ofExtensionDifferent];
    end else begin
      SaveDialog.FilterIndex:=2;
      SaveDialog.options:=SaveDialog.options-[ofExtensionDifferent];
    end;
    if SaveDialog.execute then begin
      mainForm.caption:=saveFile(SaveDialog.fileName);
      result:=true;
    end else result:=false;
  end;

FUNCTION T_editorMeta.saveWithDialog: boolean;
  begin
    if isFile then begin
      mainForm.caption:=saveFile();
      result:=true;
    end else result:=saveAsWithDialog;
  end;

PROCEDURE T_editorMeta.closeEditor;
  begin
    sheet.tabVisible:=false;
    editor.clearAll;
    with fileInfo do begin
      filePath:='';
      isChanged:=false;
    end;
    editor.modified:=false;
  end;

PROCEDURE T_editorMeta.setLanguage(CONST languageIndex: byte);
  VAR metaIdx:longint;
  begin
    language_:=languageIndex;
    for metaIdx:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[metaIdx].language=languageIndex then begin
      if fileTypeMeta[metaIdx].language=LANG_MNH
      then editor.highlighter:=highlighter
      else editor.highlighter:=fileTypeMeta[metaIdx].highlighter;
      editor.Gutter.MarksPart.visible:=(debugMode) and (language_=LANG_MNH);
      editor.readonly                :=areEditorsLocked;
      {$ifdef debugMode}writeln('Set language ',fileTypeMeta[metaIdx].language,' - ',fileTypeMeta[metaIdx].extensionWithoutDot);{$endif}
      exit;
    end;
  end;

PROCEDURE T_editorMeta.setLanguage(CONST extensionWithoutDot: string; CONST fallback: byte);
  VAR metaIdx:longint;
      ext:string;
  begin
    ext:=uppercase(extensionWithoutDot);
    for metaIdx:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[metaIdx].extensionWithoutDot=ext then begin
      setLanguage(fileTypeMeta[metaIdx].language);
      exit;
    end;
    setLanguage(fallback);
  end;

PROCEDURE T_editorMeta.guessLanguage(CONST fallback: byte);
  begin
    setLanguage(copy(extractFileExt(fileInfo.filePath),2,10),fallback);
  end;

PROCEDURE T_editorMeta.setFile(CONST fileName: string);
  begin
    sheet.tabVisible:=true;
    fileInfo.filePath:=fileName;
    editor.clearAll;
    try
      editor.lines.loadFromFile(fileInfo.filePath);
      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
      fileInfo.isChanged:=false;
      editor.modified:=false;
    except
      editor.lines.clear;
      fileInfo.isChanged:=true;
      fileInfo.fileAccessAge:=0;
    end;
    guessLanguage(LANG_TXT);
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.initForNewFile;
  begin
    sheet.tabVisible:=true;
    with fileInfo do begin
      isChanged       :=false;
      fileAccessAge:=0;
      filePath     :='';
    end;
    editor.clearAll;
    editor.modified:=false;
    setLanguage(0);
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.reloadFile(CONST fileName: string);
  begin
    if sheet.tabVisible and (fileInfo.filePath=SysToUTF8(fileName)) and (fileExists(fileName)) then begin
      editor.lines.loadFromFile(fileInfo.filePath);
      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
      editor.modified:=false;
      fileInfo.isChanged:=false;
    end;
  end;

FUNCTION T_editorMeta.caretInMainFormCoordinates: TPoint;
  begin
    result.x:=editor.CaretXPix;
    result.y:=editor.CaretYPix+editor.LineHeight;
    result:=editor.ClientToParent(result,mainForm);
  end;

PROCEDURE T_editorMeta.setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret:TPoint);
  VAR m:P_editorMeta;
      wordUnderCursor:string;
  begin
    if (language_<>LANG_MNH) or not(updateMarker or forHelpOrJump) then exit;
    wordUnderCursor:=editor.GetWordAtRowCol(caret);
    if updateMarker then begin
      for m in editorMetaData do m^.setMarkedWord(wordUnderCursor);
      editor.Repaint;
    end;
    if forHelpOrJump then with editor do
      assistancEvaluator.explainIdentifier(lines[caret.y-1],caret.y,caret.x,underCursor);
  end;


PROCEDURE T_editorMeta.setUnderCursor(CONST updateMarker, forHelpOrJump: boolean);
  begin
    setUnderCursor(updateMarker,forHelpOrJump,editor.CaretXY);
  end;

PROCEDURE T_editorMeta.setCaret(CONST location: T_searchTokenLocation);
  VAR newCaret:TPoint;
  begin
    newCaret.x:=location.column;
    newCaret.y:=location.line;
    editor.CaretXY:=newCaret;
  end;

PROCEDURE T_editorMeta.setMarkedWord(CONST wordText: string);
  begin
    if sheet.tabVisible and (language_=LANG_MNH) then highlighter.setMarkedWord(wordText);
  end;

PROCEDURE T_editorMeta.setWorkingDir;
  begin
    if fileInfo.filePath='' then SetCurrentDir(ExtractFileDir(paramStr(0)))
                            else SetCurrentDir(ExtractFileDir(fileInfo.filePath));
  end;

PROCEDURE T_editorMeta.writeToEditorState(CONST settings: P_Settings);
  VAR i:longint;
  begin
    i:=length(settings^.workspace.editorState);
    while i<=index do begin
      setLength(settings^.workspace.editorState,i+1);
      settings^.workspace.editorState[i].create;
      inc(i);
    end;
    settings^.workspace.editorState[index].visible:=sheet.tabVisible;
    if not(settings^.workspace.editorState[index].visible) then exit;
    setLength(settings^.workspace.editorState[index].markedLines,0);
    for i:=0 to editor.Marks.count-1 do appendIfNew(settings^.workspace.editorState[index].markedLines,editor.Marks[i].line);

    settings^.workspace.editorState[index].filePath:=fileInfo.filePath;
    settings^.workspace.editorState[index].fileAccessAge:=fileInfo.fileAccessAge;
    settings^.workspace.editorState[index].changed:=changed;
    setLength(settings^.workspace.editorState[index].lines,editor.lines.count);
    for i:=0 to length(settings^.workspace.editorState[index].lines)-1 do settings^.workspace.editorState[index].lines[i]:=editor.lines[i];
    settings^.workspace.editorState[index].caret['x']:=editor.CaretX;
    settings^.workspace.editorState[index].caret['y']:=editor.CaretY;
    settings^.workspace.editorState[index].language:=language;
  end;

PROCEDURE T_editorMeta.toggleComment;
  PROCEDURE commentLine(CONST CaretY:longint);
    VAR c0,c1:TPoint;
    begin
      c0.y:=CaretY;
      c1.y:=CaretY;
      c0.x:=0;
      c1.x:=0;
      editor.TextBetweenPoints[c0,c1]:=COMMENT_PREFIX;
    end;

  PROCEDURE uncommentLine(CONST CaretY:longint);
    VAR c0,c1:TPoint;
    begin
      c0.y:=CaretY;
      c1.y:=CaretY;
      c0.x:=pos(COMMENT_PREFIX,editor.lines[CaretY-1]);
      c1.x:=c0.x+length(COMMENT_PREFIX);
      editor.TextBetweenPoints[c0,c1]:='';
    end;

  FUNCTION lineIsCommented(CONST CaretY:longint):boolean;
    begin
      result:=startsWith(trim(editor.lines[CaretY-1]),COMMENT_PREFIX);
    end;

  VAR cy:longint;
      commented:boolean=true;
  begin
    if editor.readonly then exit;
    editor.BeginUndoBlock;
    if (editor.BlockBegin.y<1) then begin
      if lineIsCommented(editor.CaretY)
      then uncommentLine(editor.CaretY)
      else   commentLine(editor.CaretY);
    end else begin
      for cy:=editor.BlockBegin.y to editor.BlockEnd.y do commented:=commented and lineIsCommented(cy);
      if commented then for cy:=editor.BlockBegin.y to editor.BlockEnd.y do uncommentLine(cy)
                   else for cy:=editor.BlockBegin.y to editor.BlockEnd.y do   commentLine(cy);
    end;
    editor.EndUndoBlock;
  end;

PROCEDURE T_editorMeta.insertText(CONST s: string);
  begin
    editor.InsertTextAtCaret(s);
  end;

PROCEDURE T_editorMeta.toggleBreakpoint;
  VAR i:longint;
  begin
    for i:=0 to editor.Marks.count-1 do if editor.Marks[i].line=editor.CaretY then begin
      editor.Marks.remove(editor.Marks[i]);
      runEvaluator.context.stepper^.removeBreakpoint(pseudoName,editor.CaretY);
      exit;
    end;
    runEvaluator.context.stepper^.addBreakpoint(pseudoName,editor.CaretY);
    _add_breakpoint_(editor.CaretY);
  end;

FUNCTION T_editorMeta.isFile: boolean;
  begin
    result:=fileInfo.filePath<>'';
  end;

FUNCTION T_editorMeta.pseudoName(CONST short: boolean): ansistring;
  begin
    if fileInfo.filePath<>'' then begin
      if short then result:=extractFileName(fileInfo.filePath)
               else result:=fileInfo.filePath;
    end else result:='<new '+intToStr(index)+'>';
  end;

PROCEDURE T_editorMeta.setStepperBreakpoints;
  VAR i:longint;
  begin
    for i:=0 to editor.Marks.count-1 do runEvaluator.context.stepper^.addBreakpoint(pseudoName,editor.Marks[i].line);
  end;

PROCEDURE T_editorMeta._add_breakpoint_(CONST lineIndex: longint);
  VAR m:TSynEditMark;
  begin
    m:=TSynEditMark.create(editor);
    m.line:=lineIndex;
    m.ImageList:=breakpointsImagesList;
    m.ImageIndex:=0;
    m.visible:=true;
    editor.Marks.add(m);
  end;

FUNCTION T_editorMeta.updateSheetCaption: ansistring;
  begin
    if changed then result:=' *'
               else result:='';
    sheet.caption:=pseudoName(true)+result;
    result:=APP_TITLE+' '+pseudoName(false)+result;
  end;

PROCEDURE T_editorMeta.repaintWithStateCounter(CONST stateCounter: longint;
  CONST errorHints: T_arrayOfString);
  VAR i:longint;
  begin
    if (stateCounter<>paintedWithState) then begin
      paintedWithState:=stateCounter;
      editor.Repaint;
      assistanceSynEdit.clearAll;
      assistanceSynEdit.lines.clear;
      for i:=0 to length(errorHints)-1 do assistanceSynEdit.lines.add(errorHints[i]);
    end;
  end;

FUNCTION T_editorMeta.changed: boolean;
  begin
    result:=fileInfo.isChanged or editor.modified;
  end;

FUNCTION T_editorMeta.saveFile(CONST fileName: string): string;
  VAR arr:T_arrayOfString;
      i:longint;
  begin
    if fileName<>'' then fileInfo.filePath:=expandFileName(fileName);
    setLength(arr,editor.lines.count);
    for i:=0 to length(arr)-1 do arr[i]:=editor.lines[i];
    with fileInfo do begin
      writeFileLines(filePath,arr,'',false);
      fileAge(filePath,fileAccessAge);
      isChanged:=false;
      editor.modified:=false;
      editor.MarkTextAsSaved;
      if (filePath=utilityScriptFileName) then runEvaluator.ensureEditScripts();
    end;
    result:=updateSheetCaption;
  end;

FUNCTION T_editorMeta.fileIsDeleted: boolean;
  begin
    result:=sheet.tabVisible and isFile and not(fileExists(fileInfo.filePath));
  end;

FUNCTION T_editorMeta.fileIsModifiedOnFileSystem: boolean;
  VAR currentFileAge:double;
  begin
    if not(sheet.tabVisible and isFile) or changed then exit(false);
    fileAge(fileInfo.filePath,currentFileAge);
    result:=currentFileAge<>fileInfo.fileAccessAge;
  end;

FUNCTION T_editorMeta.languageName: string;
  VAR i:longint;
  begin
    for i:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[i].language=language_ then exit(lowercase(fileTypeMeta[i].extensionWithoutDot));
    result:='';
  end;

PROCEDURE T_editorMeta.updateContentAfterEditScript(
  CONST stringListLiteral: P_listLiteral);
  VAR concatenatedText:ansistring='';
      i:longint;
  begin
    if stringListLiteral^.literalType<>lt_stringList then exit;
    for i:=0 to stringListLiteral^.size-1 do begin
      if i>0 then concatenatedText:=concatenatedText+LineEnding;
      concatenatedText:=concatenatedText+P_stringLiteral(stringListLiteral^[i])^.value;
    end;
    editor.BeginUndoBlock;
    editor.SelectAll;
    editor.SelText:=concatenatedText;
    editor.EndUndoBlock;
  end;

PROCEDURE T_editorMeta.exportToHtml;
  VAR SynExporterHTML: TSynExporterHTML;
  begin
    SaveDialog.FilterIndex:=2;
    if not(SaveDialog.execute) then exit;
    SynExporterHTML:=TSynExporterHTML.create(nil);
    SynExporterHTML.title:=pseudoName();
    SynExporterHTML.highlighter:=editor.highlighter;
    SynExporterHTML.ExportAll(editor.lines);
    SynExporterHTML.saveToFile(SaveDialog.fileName);
    SynExporterHTML.free;
  end;

//==================================================================
FUNCTION hasEditor:boolean;
  VAR i:longint;
  begin
    i:=inputPageControl.activePageIndex;
    result:=(i>=0) and (i<length(editorMetaData));
  end;

FUNCTION getEditor:P_editorMeta;
  VAR i:longint;
  begin
    i:=inputPageControl.activePageIndex;
    if (i>=0) and (i<length(editorMetaData))
    then result:=editorMetaData[i]
    else result:=nil;
  end;

FUNCTION addEditorMetaForNewFile:longint;
  VAR i:longint;
  begin
    i:=length(editorMetaData)-1;
    //decrease i until a visible meta is encountered
    while (i>=0) and not(editorMetaData[i]^.sheet.tabVisible) do dec(i);
    inc(i);
    //i now is the index of the last visible editor meta +1
    if (i>=0) and (i<length(editorMetaData)) then begin
      editorMetaData[i]^.initForNewFile;
      exit(i);
    end;

    i:=length(editorMetaData);
    setLength(editorMetaData,i+1);
    new(editorMetaData[i],create(i));
    editorMetaData[i]^.editor.Font:=assistanceSynEdit.Font;

    result:=i;
    editorMetaData[i]^.editor.Gutter.MarksPart.visible:=(debugMode) and (editorMetaData[i]^.language=LANG_MNH);
    editorMetaData[i]^.editor.readonly                :=areEditorsLocked;
  end;

FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):longint;

  PROCEDURE openSingleFile(CONST fileName:ansistring);
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
        for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.sheet.tabVisible) and (editorMetaData[i]^.pseudoName=fileName) then begin
          result:=i;
          exit;
        end;
      end else begin
        filePath:=expandFileName(fileName);
        for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.sheet.tabVisible) and (editorMetaData[i]^.fileInfo.filePath=filePath) then begin
          result:=i;
          exit;
        end;
        result:=addEditorMetaForNewFile();
        editorMetaData[result]^.setFile(filePath);
        editorMetaData[result]^.editor.Font:=assistanceSynEdit.Font;
      end;
    end;


  VAR f:string;
  begin
    if useCurrentPageAsFallback then result:=inputPageControl.activePageIndex
                                else result:=-1;
    for f in FileNames do openSingleFile(f);
  end;

PROCEDURE updateFonts;
  VAR m:P_editorMeta;
  begin
    for m in editorMetaData do m^.editor.Font:=assistanceSynEdit.Font;
  end;

FUNCTION allPseudoNames:T_arrayOfString;
  VAR m:P_editorMeta;
  begin
    setLength(result,0);
    for m in editorMetaData do if m^.sheet.tabVisible then append(result,m^.pseudoName);
  end;

FUNCTION getMeta(CONST nameOrPseudoName:string):P_editorMeta;
  VAR m:P_editorMeta;
  begin
    result:=nil;
    for m in editorMetaData do if (m^.sheet.tabVisible) and (m^.pseudoName()=nameOrPseudoName) then exit(m);
  end;

PROCEDURE storeEditorsToSettings;
  VAR e:P_editorMeta;
  begin
    for e in editorMetaData do e^.writeToEditorState(settings.value);
  end;

FUNCTION getHelpPopupText:string;
  begin
    result:=ECHO_MARKER+underCursor.tokenText+C_lineBreakChar+underCursor.tokenExplanation;
  end;

FUNCTION getHelpLocation:T_searchTokenLocation;
  begin
    result:=underCursor.location;
  end;

PROCEDURE cycleEditors(CONST cycleForward:boolean);
  VAR i,k:longint;
      delta:longint;
  begin
    i:=inputPageControl.activePageIndex;
    if cycleForward then delta:=1 else delta:=length(editorMetaData)-1;
    for k:=0 to length(editorMetaData)-1 do begin
      i:=(i+delta) mod (length(editorMetaData));
      if editorMetaData[i]^.sheet.tabVisible then begin
        inputPageControl.activePageIndex:=i;
        editorMetaData[i]^.activate;
        exit;
      end;
    end;
    inputPageControl.activePageIndex:=addEditorMetaForNewFile;
  end;

PROCEDURE updateEditorsByGuiStatus;
  VAR m:P_editorMeta;
  begin
    for m in editorMetaData do begin
      m^.editor.Gutter.MarksPart.visible:=(debugMode) and (m^.language=LANG_MNH);
      m^.editor.readonly                :=areEditorsLocked;
    end;
  end;

PROCEDURE finalizeEditorMeta;
  VAR i:longint;
  begin
    for i:=0 to length(editorMetaData)-1 do dispose(editorMetaData[i],destroy);
  end;

INITIALIZATION
  setLength(editorMetaData,0);
  dummyEditor.create(-1);
FINALIZATION
  finalizeEditorMeta;
  dummyEditor.destroy;
end.
