UNIT editorMeta;
INTERFACE
USES  //basic classes
  Classes, sysutils, LazUTF8, LCLType, types,
  //my utilities:
  myStringUtil, myGenerics,
  //GUI: LCL components
  Controls, Graphics, Dialogs, Menus, ComCtrls, StdCtrls,
  //GUI: SynEdit
  SynEdit, SynPluginMultiCaret, SynEditMiscClasses, SynEditMarks, SynEditKeyCmds,
  //GUI: highlighters
  SynHighlighterMnh, SynHighlighterPas, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterJScript, SynHighlighterPerl, SynHighlighterHTML,
  SynHighlighterXML, SynHighlighterDiff, synhighlighterunixshellscript,
  SynHighlighterCss, SynHighlighterPHP, SynHighlighterSQL, SynHighlighterPython,
  SynHighlighterVB, SynHighlighterBat, SynHighlighterIni, SynEditHighlighter,
  SynExportHTML,
  closeDialog,
  mnh_tables,
  mnh_plotForm,
  //MNH:
  outlines,
  mnh_constants, mnh_basicTypes, mnh_fileWrappers,mnh_settings,
  mnh_contexts,
  mnh_litVar,
  mnh_funcs,
  mnh_debugging,
  mnh_cmdLineInterpretation,
  mnh_evalThread,
  mnh_packages,
  mnhCompletion,
  guiOutAdapters;

TYPE T_language=(LANG_MNH   = 0,
                 LANG_CPP   = 1,
                 LANG_CSS   = 2,
                 LANG_DIFF  = 3,
                 LANG_HTML  = 4,
                 LANG_INI   = 5,
                 LANG_JAVA  = 6,
                 LANG_JS    = 7,
                 LANG_PAS   = 8,
                 LANG_PERL  = 9,
                 LANG_PHP   =10,
                 LANG_PYTHON=11,
                 LANG_SHELL =12,
                 LANG_SQL   =13,
                 LANG_VB    =14,
                 LANG_BAT   =15,
                 LANG_XML   =16,
                 LANG_TXT   =17);

CONST DETACHED_EDITOR_META_INDEX=-1;

TYPE
P_editorMeta=^T_editorMeta;
T_editorMeta=object(T_codeProvider)
  private
    index:longint;
    paintedWithStateHash:T_hashInt;
    fileInfo:record
      filePath:ansistring;
      fileAccessAge:double;
      isChanged:boolean;
      ignoreDeleted:boolean;
    end;
    assistant   : P_codeAssistanceData;
    language_   : T_language;
    tabsheet    : TTabSheet;
    editor_     : TSynEdit;
    plugin      : TSynPluginMultiCaret;
    highlighter : TSynMnhSyn;
    PROCEDURE setLanguage(CONST languageIndex:T_language);
    PROCEDURE guessLanguage(CONST fallback:T_language);
    CONSTRUCTOR create(CONST idx:longint);
    CONSTRUCTOR create(CONST idx:longint; VAR state:T_editorState);
  public
    CONSTRUCTOR createWithParent(CONST idx:longint; CONST parent:TWinControl);
    FUNCTION enabled:boolean;

    DESTRUCTOR destroy; virtual;
    //T_codeProvider:
    FUNCTION getLines: T_arrayOfString; virtual;
    FUNCTION getPath: ansistring;                                    virtual;
    FUNCTION stateHash:T_hashInt;                                    virtual;
    FUNCTION disposeOnPackageDestruction:boolean;                    virtual;
    FUNCTION isPseudoFile:boolean;                                   virtual;

    PROCEDURE setLanguage(CONST extensionWithoutDot:string; CONST fallback:T_language);

    PROPERTY language:T_language read language_ write setLanguage;
    PROPERTY editor:TSynEdit read editor_;
    PROCEDURE activate;
    FUNCTION caretInMainFormCoordinates:TPoint;
    PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret:TPoint);
    PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean);
    PROCEDURE setCaret(CONST location: T_searchTokenLocation);
    PROCEDURE toggleComment;
    PROCEDURE moveLine(CONST up:boolean);
    PROCEDURE toggleBreakpoint;
    PROCEDURE setWorkingDir;
    PROCEDURE closeEditorWithDialogs;

    FUNCTION saveAsWithDialog:boolean;
    FUNCTION saveWithDialog:boolean;
    PROCEDURE reloadFile(CONST fileName:string);
    PROCEDURE exportToHtml;
    FUNCTION pseudoName(CONST short:boolean=false):ansistring;
    FUNCTION defaultExtensionByLanguage:ansistring;
    PROCEDURE insertText(CONST s:string);
    PROCEDURE updateContentAfterEditScript(CONST stringListLiteral:P_listLiteral);
    FUNCTION resolveImport(CONST text:string):string;
    PROCEDURE assignAdditionalHighlighter(CONST additionalHighlighter:TSynMnhSyn);
    PROCEDURE pollAssistanceResult;
  private
    PROCEDURE ensureAssistant;
    PROCEDURE dropAssistant;
    PROCEDURE triggerCheck;

    PROCEDURE initWithState(VAR state:T_editorState);
    PROCEDURE closeEditorQuietly;
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE languageMenuItemClick(Sender: TObject);
    FUNCTION isFile:boolean;
    PROCEDURE setFile(CONST fileName:string);
    PROCEDURE initForNewFile;
    PROCEDURE setMarkedWord(CONST wordText:string);
    PROCEDURE writeToEditorState(CONST settings:P_Settings);
    PROCEDURE setStepperBreakpoints;
    PROCEDURE _add_breakpoint_(CONST lineIndex:longint);
    FUNCTION updateSheetCaption:ansistring;
    FUNCTION changed:boolean;
    FUNCTION saveFile(CONST fileName:string=''):string;
    FUNCTION fileIsDeleted:boolean;
    FUNCTION fileIsModifiedOnFileSystem:boolean;
end;

T_runnerModel=object
  private
    lastStart:record
      mainCall:boolean;
      parameters:string;
    end;
    debugLine:record
      editor:TSynEdit;
      line:longint;
    end;
    debugMode_:boolean;
    PROCEDURE setDebugMode(CONST value:boolean);
  public
    stackTracing:boolean;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION areEditorsLocked:boolean;
    PROPERTY debugMode:boolean read debugMode_ write setDebugMode;
    FUNCTION canRun(CONST quickMode:boolean=false):boolean;
    PROCEDURE customRun(CONST mainCall,profiling:boolean; CONST mainParameters:string='');
    PROCEDURE rerun(CONST profiling:boolean);
    PROCEDURE InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
    PROCEDURE doDebuggerAction(CONST newState:T_debuggerState);
    PROCEDURE markDebugLine(CONST editor:TSynEdit; CONST line:longint);
    PROCEDURE haltEvaluation;
  end;

PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
                    CONST p_inputPageControl      :TPageControl;
                    CONST p_EditorPopupMenu       :TPopupMenu;
                    CONST p_SaveDialog            :TSaveDialog;
                    CONST p_breakpointsImagesList :TImageList;
                    CONST p_assistanceSynEdit     :TSynEdit;
                    CONST p_assistanceTabSheet    :TTabSheet;
                    CONST outputHighlighter       :TSynMnhSyn;
                    CONST languageMenuRoot        :TMenuItem;
                    CONST p_EditKeyUp             :TKeyEvent;
                    CONST p_EditMouseDown         :TMouseEvent;
                    CONST p_EditProcessUserCommand:TProcessCommandEvent;
                    CONST p_outlineGroupBox       :TGroupBox;
                    CONST p_outlineTreeView       :TTreeView;
                    CONST p_outlineFilterPrivateCb,p_outlineFilterImportedCb:TCheckBox;
                    CONST p_openlocation          :T_openLocationCallback);
FUNCTION hasEditor:boolean;
FUNCTION getEditor:P_editorMeta;
FUNCTION addEditorMetaForNewFile:longint;
FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):longint;
PROCEDURE updateFonts(CONST Font:TFont);
FUNCTION allPseudoNames:T_arrayOfString;
FUNCTION getMeta(CONST nameOrPseudoName:string):P_editorMeta;
PROCEDURE storeEditorsToSettings;
FUNCTION getHelpPopupText:string;
FUNCTION getHelpLocation:T_searchTokenLocation;
PROCEDURE cycleEditors(CONST cycleForward:boolean);
PROCEDURE updateEditorsByGuiStatus;
PROCEDURE closeAllEditorsButCurrent;
PROCEDURE closeAllUnmodifiedEditors;
PROCEDURE checkForFileChanges;
PROCEDURE finalizeEditorMeta;

FUNCTION getSafeAssistant(CONST editor:P_editorMeta):P_codeAssistanceData;
VAR runnerModel:T_runnerModel;
    completionLogic:T_completionLogic;
    recentlyActivated:T_fileHistory;
IMPLEMENTATION
VAR mainForm              :T_abstractMnhForm;
    inputPageControl      :TPageControl;
    EditorPopupMenu       :TPopupMenu;
    SaveDialog            :TSaveDialog;
    breakpointsImagesList :TImageList;
    EditKeyUp             :TKeyEvent;
    EditMouseDown         :TMouseEvent;
    EditProcessUserCommand:TProcessCommandEvent;
    assistanceSynEdit     :TSynEdit;
    assistanceTabSheet    :TTabSheet;
    editorFont            :TFont;

    outlineModel          :P_outlineTreeModel=nil;
    outlineGroupBox       :TGroupBox;
    fallbackCodeAssistant :P_blankCodeAssistanceData=nil;

VAR fileTypeMeta:array[T_language] of record
      highlighter:TSynCustomHighlighter;
      extensions:T_arrayOfString;
      menuItem:TMenuItem;
    end;

VAR editorMetaData:array of P_editorMeta;
    underCursor:T_tokenInfo;

PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
                    CONST p_inputPageControl      :TPageControl;
                    CONST p_EditorPopupMenu       :TPopupMenu;
                    CONST p_SaveDialog            :TSaveDialog;
                    CONST p_breakpointsImagesList :TImageList;
                    CONST p_assistanceSynEdit     :TSynEdit;
                    CONST p_assistanceTabSheet    :TTabSheet;
                    CONST outputHighlighter       :TSynMnhSyn;
                    CONST languageMenuRoot        :TMenuItem;
                    CONST p_EditKeyUp             :TKeyEvent;
                    CONST p_EditMouseDown         :TMouseEvent;
                    CONST p_EditProcessUserCommand:TProcessCommandEvent;
                    CONST p_outlineGroupBox       :TGroupBox;
                    CONST p_outlineTreeView       :TTreeView;
                    CONST p_outlineFilterPrivateCb,p_outlineFilterImportedCb:TCheckBox;
                    CONST p_openlocation          :T_openLocationCallback);

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
    PROCEDURE addFileType(CONST language:T_language; CONST extension:string; CONST highlighter:TSynCustomHighlighter=nil; CONST menuCaption:string='');
      VAR menuItem:TMenuItem;
      begin
        if menuCaption<>'' then begin
          menuItem:=TMenuItem.create(mainForm);
          menuItem.caption:=menuCaption;
          menuItem.Tag:=ord(language);
          languageMenuRoot.add(menuItem);

          fileTypeMeta[language].menuItem:=menuItem;
          fileTypeMeta[language].highlighter:=highlighter;
          fileTypeMeta[language].extensions:=uppercase(extension);
        end else append(fileTypeMeta[language].extensions,uppercase(extension));
      end;

    begin
      addFileType(LANG_MNH   ,'mnh' ,nil                   ,'&MNH');
      addFileType(LANG_CPP   ,'cpp' ,SynCppSyn1            ,'&C++');
      addFileType(LANG_CPP   ,'c'   );
      addFileType(LANG_CPP   ,'h'   );
      addFileType(LANG_CPP   ,'hh'  );
      addFileType(LANG_CSS   ,'css' ,SynCssSyn1            ,'CSS'        );
      addFileType(LANG_DIFF  ,'diff',SynDiffSyn1           ,'&diff'      );
      addFileType(LANG_HTML  ,'html',SynHTMLSyn1           ,'&HTML'      );
      addFileType(LANG_INI   ,'ini' ,SynIniSyn1            ,'&ini'       );
      addFileType(LANG_JAVA  ,'java',SynJavaSyn1           ,'&Java'      );
      addFileType(LANG_JS    ,'js'  ,SynJScriptSyn1        ,'JavaScript' );
      addFileType(LANG_JS    ,'json');
      addFileType(LANG_PAS   ,'pas' ,SynFreePascalSyn1     ,'&Pascal'             );
      addFileType(LANG_PERL  ,'perl',SynPerlSyn1           ,'Perl'                );
      addFileType(LANG_PHP   ,'php' ,SynPHPSyn1            ,'PHP'                 );
      addFileType(LANG_PYTHON,'py'  ,SynPythonSyn1         ,'Python'              );
      addFileType(LANG_SHELL ,'sh'  ,SynUNIXShellScriptSyn1,'Shell script'        );
      addFileType(LANG_SQL   ,'sql' ,SynSQLSyn1            ,'&SQL'                );
      addFileType(LANG_VB    ,'vb'  ,SynVBSyn1             ,'&Visual Basic'       );
      addFileType(LANG_BAT   ,'bat' ,SynBatSyn1            ,'Windows &Batch File' );
      addFileType(LANG_XML   ,'xml' ,SynXMLSyn1            ,'&XML'                );
      addFileType(LANG_TXT   ,'txt' ,nil                   ,'unknown (&Text)'     );
    end;

  PROCEDURE restoreEditors;
    VAR i:longint;
    begin
      setLength(editorMetaData,length(settings.value^.workspace.editorState));
      for i:=0 to length(editorMetaData)-1 do new(editorMetaData[i],create(i,settings.value^.workspace.editorState[i]));
      i:=settings.value^.workspace.activePage;
      inputPageControl.activePageIndex:=i;
      inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(filesToOpenInEditor,true);
    end;

  begin
    mainForm              :=p_mainForm              ;
    inputPageControl      :=p_inputPageControl      ;
    EditorPopupMenu       :=p_EditorPopupMenu       ;
    SaveDialog            :=p_SaveDialog            ;
    breakpointsImagesList :=p_breakpointsImagesList ;
    EditKeyUp             :=p_EditKeyUp             ;
    EditMouseDown         :=p_EditMouseDown         ;
    EditProcessUserCommand:=p_EditProcessUserCommand;
    assistanceSynEdit     :=p_assistanceSynEdit     ;
    assistanceTabSheet    :=p_assistanceTabSheet    ;
    outlineGroupBox       :=p_outlineGroupBox       ;
    new(outlineModel,create(p_outlineTreeView,p_outlineFilterPrivateCb,p_outlineFilterImportedCb,p_openlocation));

    initHighlighters;
    initFileTypes;
    completionLogic.create;
    editorFont:=assistanceSynEdit.Font;
    restoreEditors;
  end;

CONSTRUCTOR T_editorMeta.createWithParent(CONST idx:longint; CONST parent:TWinControl);
  PROCEDURE addKeystroke(CONST command:TSynEditorCommand; CONST ShortCut:TShortCut);
    VAR keyStroke:TSynEditKeyStroke;
    begin
      keyStroke:=plugin.Keystrokes.add;
      keyStroke.command:=command;
      keyStroke.ShortCut:=ShortCut;
    end;

  VAR style:TSynSelectedColor;
  begin
    paintedWithStateHash:=0;
    index:=idx;
    editor_:=TSynEdit.create(mainForm);
    editor_.parent:=parent;
    editor_.Align:=alClient;
    editor_.ScrollBars:=ssAutoBoth;
    editor_.WantTabs:=false;
    editor_.Gutter.MarksPart.visible:=false;
    editor_.Gutter.CodeFoldPart.visible:=false;
    editor_.Gutter.ChangesPart.visible:=false;
    editor_.Gutter.MarksPart.width:=breakpointsImagesList.width;
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
    editor_.OnSpecialLineMarkup :=@(runnerModel.InputEditSpecialLineMarkup);
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
    addKeystroke(ecUserDefinedFirst+4,32806);//Alt+Up
    addKeystroke(ecUserDefinedFirst+5,32808);//Alt+Down
    highlighter:=TSynMnhSyn.create(mainForm,msf_input);
    editor_.highlighter:=highlighter;
    editor_.PopupMenu:=EditorPopupMenu;
    initForNewFile;
  end;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint);
  begin
    paintedWithStateHash:=0;
    index:=idx;
    tabsheet:=TTabSheet.create(inputPageControl);
    tabsheet.PageControl:=inputPageControl;
    createWithParent(idx,tabsheet);
  end;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint; VAR state: T_editorState);
  begin
    create(idx);
    initWithState(state);
  end;

PROCEDURE T_editorMeta.initWithState(VAR state: T_editorState);
  VAR i:longint;
  begin
    if index<>DETACHED_EDITOR_META_INDEX then tabsheet.tabVisible:=true;
    with fileInfo do begin
      isChanged    :=state.changed;
      fileAccessAge:=state.fileAccessAge;
      filePath     :=state.filePath;
      ignoreDeleted:=false;
    end;
    state.getLines(editor.lines);
    for i:=0 to length(state.markedLines)-1 do _add_breakpoint_(state.markedLines[i]);
    editor.CaretX:=state.caret['x'];
    editor.CaretY:=state.caret['y'];
    language_:=T_language(state.language);
    updateSheetCaption;
  end;

DESTRUCTOR T_editorMeta.destroy;
  begin
    dropAssistant;
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
  VAR l:T_language;
  begin
    if not(enabled) then exit;
    {$ifdef debugMode}
    writeln(stdErr,'        DEBUG: Activating editor "',pseudoName(),'"');
    {$endif}
    editor_.Font:=editorFont;
    mainForm.caption:=updateSheetCaption;

    for l in T_language do begin
      fileTypeMeta[l].menuItem.OnClick:=@languageMenuItemClick;
      fileTypeMeta[l].menuItem.Checked:=(l=language);
    end;
    try
      recentlyActivated.fileClosed(getPath);
      if language_=LANG_MNH then begin
        outlineGroupBox.visible:=true;
        editor.highlighter:=highlighter;
        paintedWithStateHash:=0;
        assistanceTabSheet.tabVisible:=true;
        triggerCheck;
        completionLogic.assignEditor(editor_,assistant);
      end else begin
        outlineGroupBox.visible:=false;
        editor.highlighter:=fileTypeMeta[language_].highlighter;
        assistanceSynEdit.clearAll;
        assistanceTabSheet.caption:='';
        assistanceTabSheet.tabVisible:=false;
        dropAssistant;
        completionLogic.assignEditor(editor_,nil);
      end;
      editor.Gutter.MarksPart.visible:=runnerModel.debugMode and (language_=LANG_MNH);
      editor.readonly                :=runnerModel.areEditorsLocked;
      settings.value^.workspace.activePage:=index;
      mainForm.onDebuggerEvent;
    except end; //catch and ignore all exceptions
  end;

PROCEDURE T_editorMeta.InputEditChange(Sender: TObject);
  begin
    {$ifdef debugMode} writeln(stdErr,'        DEBUG: T_editorMeta.InputEditChange for ',pseudoName(),'; language: ',language_); {$endif}
    if not(enabled) then exit;
    if language_=LANG_MNH then triggerCheck;
    if index<>DETACHED_EDITOR_META_INDEX then mainForm.caption:=updateSheetCaption;
  end;

PROCEDURE T_editorMeta.languageMenuItemClick(Sender: TObject);
  begin
    setLanguage(T_language(TMenuItem(Sender).Tag));
  end;

FUNCTION T_editorMeta.saveAsWithDialog: boolean;
  begin
    if language=LANG_MNH
    then begin
      SaveDialog.FilterIndex:=0;
      SaveDialog.options:=SaveDialog.options+[ofExtensionDifferent];
    end else begin
      SaveDialog.FilterIndex:=1;
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

PROCEDURE T_editorMeta.closeEditorQuietly;
  begin
    if index<>DETACHED_EDITOR_META_INDEX then tabsheet.tabVisible:=false;
    editor.clearAll;
    with fileInfo do begin
      filePath:='';
      isChanged:=false;
      ignoreDeleted:=false;
    end;
    editor.modified:=false;
    getEditor^.activate;
  end;

PROCEDURE T_editorMeta.closeEditorWithDialogs;
  VAR mr:longint;
  begin
    if not(enabled) then exit;
    if changed then begin
      mr:=closeDialogForm.showOnClose(pseudoName(true));
      if mr=mrOk then if not(saveWithDialog) then exit;
      if mr=mrCancel then exit;
    end;
    if isFile then settings.value^.workspace.fileHistory.fileClosed(fileInfo.filePath);
    closeEditorQuietly;
  end;

PROCEDURE T_editorMeta.setLanguage(CONST languageIndex: T_language);
  begin
    if language_=languageIndex then exit;

    language_:=languageIndex;
    {$ifdef debugMode}writeln(stdErr,'        DEBUG: Set language ',language_);{$endif}
    activate;
  end;

PROCEDURE T_editorMeta.setLanguage(CONST extensionWithoutDot: string;
  CONST fallback: T_language);
  VAR l:T_language;
      s:string;
      ext:string;
  begin
    ext:=uppercase(extensionWithoutDot);
    for l in T_language do
    for s in fileTypeMeta[l].extensions do if ext=s then begin
      setLanguage(l);
      exit;
    end;
    setLanguage(fallback);
  end;

FUNCTION T_editorMeta.enabled:boolean;
  begin
    result:=(index=DETACHED_EDITOR_META_INDEX) or (tabsheet.tabVisible);
  end;

PROCEDURE T_editorMeta.guessLanguage(CONST fallback: T_language);
  begin
    setLanguage(copy(extractFileExt(fileInfo.filePath),2,10),fallback);
  end;

PROCEDURE T_editorMeta.setFile(CONST fileName: string);
  begin
    tabsheet.tabVisible:=true;
    fileInfo.filePath:=fileName;
    fileInfo.ignoreDeleted:=false;
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
    if (index<>DETACHED_EDITOR_META_INDEX) then tabsheet.tabVisible:=true;
    with fileInfo do begin
      isChanged       :=false;
      fileAccessAge:=0;
      filePath     :='';
      ignoreDeleted:=false;
    end;
    editor.clearAll;
    editor.modified:=false;
    setLanguage(LANG_MNH);
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.reloadFile(CONST fileName: string);
  begin
    if enabled and (fileInfo.filePath=SysToUTF8(fileName)) and (fileExists(fileName)) then begin
      editor.lines.loadFromFile(fileInfo.filePath);
      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
      editor.modified:=false;
      fileInfo.isChanged:=false;
      mainForm.caption:=updateSheetCaption;
      if language_=LANG_MNH then triggerCheck;
    end;
  end;

FUNCTION T_editorMeta.caretInMainFormCoordinates: TPoint;
  begin
    result.x:=editor.CaretXPix;
    result.y:=editor.CaretYPix+editor.LineHeight;
    result:=editor.ClientToParent(result,mainForm);
  end;

PROCEDURE T_editorMeta.setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret: TPoint);
  VAR m:P_editorMeta;
      wordUnderCursor:string;
  begin
    if (language_<>LANG_MNH) or not(updateMarker or forHelpOrJump) then exit;
    wordUnderCursor:=editor.GetWordAtRowCol(caret);
    if updateMarker then begin
      for m in editorMetaData do m^.setMarkedWord(wordUnderCursor);
      editor.Repaint;
    end;
    if forHelpOrJump then with editor do begin
      ensureAssistant;
      assistant^.explainIdentifier(lines[caret.y-1],caret.y,caret.x,underCursor);
    end;
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
    if enabled and (language_=LANG_MNH) then highlighter.setMarkedWord(wordText);
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
    settings^.workspace.editorState[index].visible:=enabled;
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
    settings^.workspace.editorState[index].language:=ord(language);
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

PROCEDURE T_editorMeta.moveLine(CONST up:boolean);
  VAR first,last:longint;
      oldBegin,oldEnd:TPoint;
      blockDelta:longint=0;
  PROCEDURE moveUp;
    VAR oldLineText:string;
        oldLineStart:TPoint;
        oldLineEnd  :TPoint;
        newLineStart:TPoint;
    begin
      if first-2<0 then exit;
      oldLineText:=editor.lines[first-2];
      oldLineStart.y:=first-1; oldLineStart.x:=0;
      oldLineEnd  .y:=first  ; oldLineEnd  .x:=0;
      editor.SetTextBetweenPoints(oldLineStart,oldLineEnd,'');
      newLineStart.y:=last;
      newLineStart.x:=0;
      editor.TextBetweenPoints[newLineStart,newLineStart]:=oldLineText+LineEnding;
      blockDelta:=-1;
    end;

  PROCEDURE moveDown;
    VAR oldLineText:string;
        oldLineStart:TPoint;
        oldLineEnd  :TPoint;
        newLineStart:TPoint;
    begin
      if last>=editor.lines.count then exit;
      oldLineText:=editor.lines[last];
      oldLineStart.y:=last+1; oldLineStart.x:=0;
      oldLineEnd  .y:=last+2; oldLineEnd  .x:=0;
      editor.SetTextBetweenPoints(oldLineStart,oldLineEnd,'');
      newLineStart.y:=first;
      newLineStart.x:=0;
      editor.TextBetweenPoints[newLineStart,newLineStart]:=oldLineText+LineEnding;
      blockDelta:=1;
    end;

  begin
    if editor.readonly then exit;
    oldBegin:=editor.BlockBegin;
    oldEnd  :=editor.BlockEnd;
    if editor.BlockBegin.y<1 then begin
      first:=editor.CaretY;
      last :=first;
    end else begin
      first:=editor.BlockBegin.y;
      last :=editor.BlockEnd  .y;
    end;
    editor.BeginUndoBlock;
    if up then moveUp else moveDown;
    if (oldBegin.y>=1) then begin
      inc(oldBegin.y,blockDelta); editor.BlockBegin:=oldBegin;
      inc(oldEnd  .y,blockDelta); editor.BlockEnd  :=oldEnd;
      editor.CaretY:=editor.CaretY+blockDelta;
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
    for i:=0 to editor_.Marks.count-1 do if editor_.Marks[i].line=editor_.CaretY then begin
      editor_.Marks.remove(editor.Marks[i]);
      runEvaluator.context.stepper^.removeBreakpoint(pseudoName,editor_.CaretY);
      exit;
    end;
    runEvaluator.context.stepper^.addBreakpoint(pseudoName,editor_.CaretY);
    _add_breakpoint_(editor_.CaretY);
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

FUNCTION T_editorMeta.defaultExtensionByLanguage: ansistring;
  begin
    result:=fileTypeMeta[language_].extensions[0];
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
    if index=DETACHED_EDITOR_META_INDEX then exit('');
    if changed then result:=' *'
               else result:='';
    tabsheet.caption:=pseudoName(true)+result;
    result:=APP_TITLE+' '+pseudoName(false)+result;
  end;

PROCEDURE T_editorMeta.ensureAssistant;
  begin
    if assistant=nil then new(assistant,create);
    highlighter.codeAssistant:=assistant;
  end;

PROCEDURE T_editorMeta.assignAdditionalHighlighter(CONST additionalHighlighter:TSynMnhSyn);
  begin
    additionalHighlighter.codeAssistant:=assistant;
  end;

PROCEDURE T_editorMeta.dropAssistant;
  begin
    if (assistant<>nil) then dispose(assistant,destroy);
    highlighter.codeAssistant:=nil;
    assistant:=nil;
  end;

PROCEDURE T_editorMeta.triggerCheck;
  begin
    ensureAssistant;
    assistant^.triggerUpdate(@self);
  end;

PROCEDURE T_editorMeta.pollAssistanceResult;
  CONST SHORTCUT_SUFFIX=' (F2)';
  VAR s:string;
      hints:T_arrayOfString;
      hasErrors,hasWarnings:boolean;
  begin
    if language_<>LANG_MNH then begin
      outlineModel^.update(getSafeAssistant(nil));
      exit;
    end;
    if (paintedWithStateHash<>assistant^.getStateHash) then begin
      paintedWithStateHash:=assistant^.getStateHash;
      highlighter.codeAssistant:=assistant;
      editor.highlighter:=highlighter;
      editor.Repaint;
      assistanceSynEdit.clearAll;
      assistanceSynEdit.lines.clear;
      hints:=assistant^.getErrorHints(hasErrors,hasWarnings,assistanceSynEdit.charsInWindow);
      if hasErrors then begin if hasWarnings then assistanceTabSheet.caption:='Errors + Warnings'+SHORTCUT_SUFFIX
                                             else assistanceTabSheet.caption:='Errors'+SHORTCUT_SUFFIX; end
                   else begin if hasWarnings then assistanceTabSheet.caption:='Warnings'+SHORTCUT_SUFFIX
                                             else assistanceTabSheet.caption:='(no warnings)'+SHORTCUT_SUFFIX; end;
      for s in hints do assistanceSynEdit.lines.add(s);
      outlineModel^.update(assistant);
    end;
    assistant^.triggerUpdate(nil);
  end;

FUNCTION T_editorMeta.changed: boolean;
  begin
    result:=fileInfo.isChanged or editor.modified;
  end;

FUNCTION T_editorMeta.saveFile(CONST fileName: string): string;
  VAR arr:T_arrayOfString;
      i:longint;
      previousName:string;
  begin
    previousName:=fileInfo.filePath;
    if fileName<>'' then fileInfo.filePath:=expandFileName(fileName);
    if (previousName<>'') and (previousName<>fileInfo.filePath) then settings.value^.workspace.fileHistory.fileClosed(previousName);
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
    result:=enabled and isFile and not(fileExists(fileInfo.filePath));
  end;

FUNCTION T_editorMeta.fileIsModifiedOnFileSystem: boolean;
  VAR currentFileAge:double;
  begin
    if not(enabled and isFile) or changed then exit(false);
    fileAge(fileInfo.filePath,currentFileAge);
    result:=currentFileAge<>fileInfo.fileAccessAge;
  end;

PROCEDURE T_editorMeta.updateContentAfterEditScript(
  CONST stringListLiteral: P_listLiteral);
  VAR concatenatedText:ansistring='';
      i:longint;
  begin
    if stringListLiteral^.literalType<>lt_stringList then exit;
    for i:=0 to stringListLiteral^.size-1 do begin
      if i>0 then concatenatedText:=concatenatedText+LineEnding;
      concatenatedText:=concatenatedText+P_stringLiteral(stringListLiteral^.value[i])^.value;
    end;
    editor.BeginUndoBlock;
    editor.SelectAll;
    editor.SelText:=concatenatedText;
    editor.EndUndoBlock;
  end;

FUNCTION T_editorMeta.resolveImport(CONST text: string): string;
  begin
    if assistant=nil then result:='' else result:=assistant^.resolveImport(text);
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

    if result=nil then result:=editorMetaData[addEditorMetaForNewFile];
  end;

FUNCTION addEditorMetaForNewFile:longint;
  VAR i:longint;
  begin
    i:=length(editorMetaData)-1;
    //decrease i until a visible meta is encountered
    while (i>=0) and not(editorMetaData[i]^.enabled) do dec(i);
    inc(i);
    //i now is the index of the last visible editor meta +1
    if (i>=0) and (i<length(editorMetaData)) then begin
      editorMetaData[i]^.initForNewFile;
      editorMetaData[i]^.activate;
      exit(i);
    end;

    i:=length(editorMetaData);
    setLength(editorMetaData,i+1);
    new(editorMetaData[i],create(i));
    editorMetaData[i]^.editor.Font:=editorFont;

    result:=i;
    editorMetaData[i]^.editor.Gutter.MarksPart.visible:=runnerModel.debugMode and (editorMetaData[i]^.language=LANG_MNH);
    editorMetaData[i]^.editor.readonly                :=runnerModel.areEditorsLocked;
    editorMetaData[i]^.activate;
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
        for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.enabled) and (editorMetaData[i]^.pseudoName=fileName) then begin
          result:=i;
          exit;
        end;
      end else begin
        filePath:=expandFileName(fileName);
        for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.enabled) and (editorMetaData[i]^.fileInfo.filePath=filePath) then begin
          result:=i;
          exit;
        end;
        result:=addEditorMetaForNewFile();
        editorMetaData[result]^.setFile(filePath);
        editorMetaData[result]^.editor.Font:=editorFont;
      end;
    end;

  VAR f:string;
  begin
    if useCurrentPageAsFallback then result:=inputPageControl.activePageIndex
                                else result:=-1;
    for f in FileNames do openSingleFile(f);
    if result<>-1 then editorMetaData[result]^.activate;
  end;

PROCEDURE updateFonts(CONST Font:TFont);
  VAR m:P_editorMeta;
  begin
    editorFont:=Font;
    for m in editorMetaData do m^.editor.Font:=editorFont;
  end;

FUNCTION allPseudoNames:T_arrayOfString;
  VAR m:P_editorMeta;
  begin
    setLength(result,0);
    for m in editorMetaData do if m^.enabled then append(result,m^.pseudoName);
  end;

FUNCTION getMeta(CONST nameOrPseudoName:string):P_editorMeta;
  VAR m:P_editorMeta;
  begin
    result:=nil;
    for m in editorMetaData do if (m^.enabled) and (m^.pseudoName()=nameOrPseudoName) then exit(m);
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
    {$ifdef debugMode} writeln(stdErr,'        DEBUG: getHelpLocation filename="',underCursor.location.fileName,'"; line=',underCursor.location.line,'; column=',underCursor.location.column); {$endif}
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
      if editorMetaData[i]^.enabled then begin
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
      m^.editor.Gutter.MarksPart.visible:=runnerModel.debugMode and (m^.language=LANG_MNH);
      m^.editor.readonly                :=runnerModel.areEditorsLocked;
    end;
  end;

PROCEDURE closeAllEditorsButCurrent;
  VAR m:P_editorMeta;
  begin
    if not(hasEditor and getEditor^.enabled) then exit;
    for m in editorMetaData do if (m^.index<>inputPageControl.activePageIndex) and (m^.enabled) then m^.closeEditorWithDialogs;
  end;

PROCEDURE closeAllUnmodifiedEditors;
  VAR m:P_editorMeta;
  begin
    for m in editorMetaData do if not(m^.changed) then m^.closeEditorWithDialogs;
    if not(hasEditor and getEditor^.enabled) then cycleEditors(true);
  end;

VAR doNotCheckFileBefore:double;
PROCEDURE checkForFileChanges;
  VAR m:P_editorMeta;
      modalRes:longint;
  begin
    if now<doNotCheckFileBefore then exit;
    doNotCheckFileBefore:=now+1;
    for m in editorMetaData do with m^ do
    if fileIsDeleted and not(fileInfo.ignoreDeleted) then begin
      modalRes:=closeDialogForm.showOnDeleted(fileInfo.filePath);
      if modalRes=mrOk then closeEditorQuietly;
      if modalRes=mrClose then begin if not(saveWithDialog) then fileInfo.isChanged:=true; end else begin
        fileInfo.ignoreDeleted:=true;
        fileInfo.isChanged:=true;
        updateSheetCaption;
      end;
      continue;
    end else if fileIsModifiedOnFileSystem then begin
      modalRes:=closeDialogForm.showOnOutOfSync(fileInfo.filePath);
      if modalRes=mrOk then reloadFile(fileInfo.filePath);
      if modalRes=mrClose then begin if not(saveWithDialog) then fileInfo.isChanged:=true; end else
      fileInfo.isChanged:=true;
    end;
    doNotCheckFileBefore:=now+ONE_SECOND;
  end;

PROCEDURE finalizeEditorMeta;
  VAR i:longint;
  begin
    if outlineModel<>nil then begin
      dispose(outlineModel,destroy);
      outlineModel:=nil;
    end;
    for i:=0 to length(editorMetaData)-1 do dispose(editorMetaData[i],destroy);
    setLength(editorMetaData,0);
  end;

PROCEDURE T_runnerModel.setDebugMode(CONST value: boolean);
  begin
    if value=debugMode_ then exit;
    debugMode_:=value;
    if (runEvaluator.evaluationRunning) and not(runEvaluator.getRunnerStateInfo.state=es_editRunning) then runEvaluator.haltEvaluation;
  end;

CONSTRUCTOR T_runnerModel.create;
  begin
    debugMode_:=false;
    stackTracing:=false;
    with lastStart do begin mainCall:=false; parameters:=''; end;
  end;

DESTRUCTOR T_runnerModel.destroy;
  begin

  end;

FUNCTION T_runnerModel.areEditorsLocked: boolean;
  begin
    result:=(debugMode_ and runEvaluator.evaluationRunning) or (runEvaluator.getRunnerStateInfo.state=es_editRunning);
  end;

FUNCTION T_runnerModel.canRun(CONST quickMode:boolean=false): boolean;
  begin
    result:=not(runEvaluator.evaluationRunningOrPending) and
            (quickMode or hasEditor and (getEditor^.language=LANG_MNH));
  end;

PROCEDURE T_runnerModel.customRun(CONST mainCall, profiling: boolean; CONST mainParameters: string);
  VAR m:P_editorMeta;
      contextType:T_evaluationContextType;
  begin
    if not(canRun) then exit;
    guiOutAdapter.flushClear;
    resetPlot;
    if settings.value^.doResetPlotOnEvaluation then begin
      guiAdapters.plot^.setDefaults;
      if plotFormIsInitialized then plotForm.pullPlotSettingsToGui();
    end;
    resetTableForms;
    getEditor^.setWorkingDir;
    if debugMode then begin
      updateEditorsByGuiStatus;
      runEvaluator.context.stepper^.clearBreakpoints;
      for m in editorMetaData do m^.setStepperBreakpoints;
    end;
    if profiling then begin
      if debugMode_ then contextType:=ect_debuggingAndProfiling
                    else contextType:=ect_profiling;
    end else begin
      if debugMode_        then contextType:=ect_debugging
      else if stackTracing then contextType:=ect_stackTracing
                           else contextType:=ect_normal;
    end;

    if mainCall then runEvaluator.callMain(getEditor,mainParameters,contextType)
                else runEvaluator.evaluate(getEditor,               contextType);
    lastStart.mainCall:=mainCall;
    lastStart.parameters:=mainParameters;
  end;

PROCEDURE T_runnerModel.rerun(CONST profiling:boolean);
  begin
    customRun(lastStart.mainCall,profiling,lastStart.parameters);
  end;

PROCEDURE T_runnerModel.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
    Special:=runEvaluator.context.isPaused and runEvaluator.evaluationRunning and (Sender=debugLine.editor) and (line=debugLine.line);
  end;

PROCEDURE T_runnerModel.doDebuggerAction(CONST newState: T_debuggerState);
  begin
    runEvaluator.context.stepper^.setState(newState);
    mainForm.onDebuggerEvent;
    if hasEditor then with getEditor^ do begin
      editor.Gutter.MarksPart.visible:=debugMode_ and (language=LANG_MNH);
      editor.readonly:=areEditorsLocked;
    end;
  end;

PROCEDURE T_runnerModel.markDebugLine(CONST editor:TSynEdit; CONST line:longint);
  begin
    debugLine.editor:=editor;
    debugLine.line  :=line;
  end;

PROCEDURE T_runnerModel.haltEvaluation;
  begin
    runEvaluator.haltEvaluation;
    if debugMode_ then runEvaluator.context.stepper^.haltEvaluation;
  end;

FUNCTION getSafeAssistant(CONST editor:P_editorMeta):P_codeAssistanceData;
  begin
    if editor=nil then result:=nil else result:=editor^.assistant;
    if result=nil then begin
      if fallbackCodeAssistant=nil then new(fallbackCodeAssistant,createBlank);
      result:=fallbackCodeAssistant;
    end;
  end;

INITIALIZATION
  setLength(editorMetaData,0);
  doNotCheckFileBefore:=now;
  recentlyActivated.create;
FINALIZATION
  recentlyActivated.destroy;
  if fallbackCodeAssistant<>nil then dispose(fallbackCodeAssistant,destroy);
end.
