UNIT editorMetaBase;
INTERFACE
USES  //basic classes
  Classes, sysutils, LCLType, types,
  //my utilities:
  myStringUtil, myGenerics,
  //GUI: LCL components
  Controls, Graphics, Menus, StdCtrls, Forms,
  //GUI: SynEdit
  SynEdit, SynPluginMultiCaret, SynEditMiscClasses, SynEditKeyCmds, SynEditTypes,
  //GUI: highlighters
  SynHighlighterMnh, SynHighlighterPas, SynHighlighterCpp, SynHighlighterJava,
  SynHighlighterJScript, SynHighlighterPerl, SynHighlighterHTML,
  SynHighlighterXML, SynHighlighterDiff, synhighlighterunixshellscript,
  SynHighlighterCss, SynHighlighterPHP, SynHighlighterSQL, SynHighlighterPython,
  SynHighlighterVB, SynHighlighterBat, SynHighlighterIni, SynEditHighlighter,
  //MNH:
  mnh_settings,
  ideLayoutUtil,
  mnh_constants, basicTypes,
  fileWrappers,
  mnhCompletion,
  codeAssistance;

CONST editCommandToggleComment    =ecUserDefinedFirst;
      editCommandPageRight        =ecUserDefinedFirst+1;
      editCommandPageLeft         =ecUserDefinedFirst+2;
      editCommandMoveLineUp       =ecUserDefinedFirst+4;
      editCommandToggleBookmark   =ecUserDefinedFirst+3;
      editCommandMoveLineDown     =ecUserDefinedFirst+5;
      editCommandEscapeSelection  =ecUserDefinedFirst+6;
      editCommandUnescapeSelection=ecUserDefinedFirst+7;
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
                 LANG_TXT   =17,
                 LANG_OUTPUT=18);

  T_lineRange=array[0..1] of longint;

  P_basicEditorMeta=^T_basicEditorMeta;
  T_basicEditorMeta=object(T_codeProvider)
    protected
      completionLogic:T_completionLogic;
      language_   : T_language;
      editor_     : TSynEdit;
      plugin      : TSynPluginMultiCaret;
      highlighter : TSynMnhSyn;
      PROCEDURE setLanguage(CONST languageIndex:T_language);
      FUNCTION currentBlockOrLine:T_lineRange;
    public
      PROCEDURE processUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer); virtual;
      CONSTRUCTOR createWithParent(CONST parent:TWinControl; CONST bookmarkImages:TImageList);
      CONSTRUCTOR createWithExistingEditor(CONST existingEditor:TSynEdit; CONST bookmarkImages:TImageList);
      DESTRUCTOR destroy; virtual;
      //T_codeProvider:
      FUNCTION getLines: T_arrayOfString; virtual;
      FUNCTION getPath: ansistring;                                    virtual;
      FUNCTION disposeOnPackageDestruction:boolean;                    virtual;
      FUNCTION isPseudoFile:boolean;                                   virtual;

      PROPERTY language:T_language read language_ write setLanguage;
      PROPERTY editor:TSynEdit read editor_;
      PROCEDURE setLanguage(CONST extensionWithoutDot:string; CONST fallback:T_language);

      PROCEDURE setCaret(CONST location: T_searchTokenLocation);
      PROCEDURE toggleComment;
      PROCEDURE moveLine(CONST up:boolean);
      PROCEDURE insertText(CONST s: string);
      PROCEDURE setMarkedWord(CONST wordText:string);
      PROCEDURE upperLowerCaseBlock(CONST upper:boolean);
      PROCEDURE escapeSelection(CONST unescape:boolean);
  end;

  T_quickEvalEditorMeta=object(T_basicEditorMeta)
    private
      latestAssistanceResponse:P_codeAssistanceResponse;
      paintedWithStateHash:T_hashInt;
    public
      CONSTRUCTOR create(CONST existingEditor:TSynEdit);
      DESTRUCTOR destroy; virtual;
      PROCEDURE updateAssistanceResponse(CONST response: P_codeAssistanceResponse);
  end;

PROCEDURE setupEditorMetaBase(CONST languageMenuRoot :TMenuItem);
VAR fileTypeMeta:array[T_language] of record
      highlighter:TSynCustomHighlighter;
      extensions:T_arrayOfString;
      menuItem:TMenuItem;
    end;
IMPLEMENTATION

PROCEDURE setupEditorMetaBase(CONST languageMenuRoot        :TMenuItem);
  VAR SynBatSyn            : TSynBatSyn            ;
      SynCppSyn            : TSynCppSyn            ;
      SynCssSyn            : TSynCssSyn            ;
      SynDiffSyn           : TSynDiffSyn           ;
      SynFreePascalSyn     : TSynFreePascalSyn     ;
      SynHTMLSyn           : TSynHTMLSyn           ;
      SynIniSyn            : TSynIniSyn            ;
      SynJScriptSyn        : TSynJScriptSyn        ;
      SynJavaSyn           : TSynJavaSyn           ;
      SynPHPSyn            : TSynPHPSyn            ;
      SynPerlSyn           : TSynPerlSyn           ;
      SynPythonSyn         : TSynPythonSyn         ;
      SynSQLSyn            : TSynSQLSyn            ;
      SynUNIXShellScriptSyn: TSynUNIXShellScriptSyn;
      SynVBSyn             : TSynVBSyn             ;
      SynXMLSyn            : TSynXMLSyn            ;
      outputHighlighter    : TSynMnhSyn;

  PROCEDURE initHighlighters;
    begin
      SynBatSyn            :=TSynBatSyn            .create(Application);
      SynCppSyn            :=TSynCppSyn            .create(Application);
      SynCssSyn            :=TSynCssSyn            .create(Application);
      SynDiffSyn           :=TSynDiffSyn           .create(Application);
      SynFreePascalSyn     :=TSynFreePascalSyn     .create(Application);
      SynHTMLSyn           :=TSynHTMLSyn           .create(Application);
      SynIniSyn            :=TSynIniSyn            .create(Application);
      SynJScriptSyn        :=TSynJScriptSyn        .create(Application);
      SynJavaSyn           :=TSynJavaSyn           .create(Application);
      SynPHPSyn            :=TSynPHPSyn            .create(Application);
      SynPerlSyn           :=TSynPerlSyn           .create(Application);
      SynPythonSyn         :=TSynPythonSyn         .create(Application);
      SynSQLSyn            :=TSynSQLSyn            .create(Application);
      SynUNIXShellScriptSyn:=TSynUNIXShellScriptSyn.create(Application);
      SynVBSyn             :=TSynVBSyn             .create(Application);
      SynXMLSyn            :=TSynXMLSyn            .create(Application);
      outputHighlighter    :=TSynMnhSyn.create(Application,msf_output);
      SynBatSyn            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynCppSyn            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynCssSyn            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynFreePascalSyn     .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynIniSyn            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynJavaSyn           .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynJScriptSyn        .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynPerlSyn           .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynPHPSyn            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynPythonSyn         .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynSQLSyn            .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynUNIXShellScriptSyn.NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynVBSyn             .NumberAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkNonStringLiteral);
      SynCppSyn            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynCssSyn            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynFreePascalSyn     .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynHTMLSyn           .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynIniSyn            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynJavaSyn           .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynJScriptSyn        .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynPerlSyn           .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynPHPSyn            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynPythonSyn         .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynSQLSyn            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynUNIXShellScriptSyn.SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynVBSyn             .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynXMLSyn            .SymbolAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkOperator);
      SynBatSyn            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynCppSyn            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynCssSyn            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynFreePascalSyn     .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynHTMLSyn           .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynIniSyn            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynJavaSyn           .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynJScriptSyn        .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynPerlSyn           .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynPHPSyn            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynPythonSyn         .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynSQLSyn            .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynUNIXShellScriptSyn.KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynVBSyn             .KeyAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkBultinRule);
      SynBatSyn            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynCppSyn            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynCssSyn            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynFreePascalSyn     .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynHTMLSyn           .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynIniSyn            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynJavaSyn           .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynJScriptSyn        .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynPerlSyn           .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynPHPSyn            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynPythonSyn         .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynSQLSyn            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynUNIXShellScriptSyn.CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynVBSyn             .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynXMLSyn            .CommentAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkComment);
      SynCppSyn            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynCssSyn            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynFreePascalSyn     .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynIniSyn            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynJavaSyn           .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynJScriptSyn        .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynPerlSyn           .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynPHPSyn            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynPythonSyn         .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynSQLSyn            .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynUNIXShellScriptSyn.StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
      SynVBSyn             .StringAttri:=outputHighlighter.getAttributeForKind(SynHighlighterMnh.tkString);
    end;

  PROCEDURE initFileTypes;
    PROCEDURE addFileType(CONST language:T_language; CONST extension:string; CONST highlighter:TSynCustomHighlighter=nil; CONST menuCaption:string='');
      VAR menuItem:TMenuItem;
      begin
        if menuCaption<>'' then begin
          if Assigned(languageMenuRoot) then begin
            menuItem:=TMenuItem.create(languageMenuRoot);
            menuItem.caption:=menuCaption;
            menuItem.Tag:=ord(language);
            menuItem.RadioItem:=true;
            menuItem.AutoCheck:=true;
            languageMenuRoot.add(menuItem);
            fileTypeMeta[language].menuItem:=menuItem;
          end else fileTypeMeta[language].menuItem:=nil;
          fileTypeMeta[language].highlighter:=highlighter;
          fileTypeMeta[language].extensions:=uppercase(extension);
        end else append(fileTypeMeta[language].extensions,uppercase(extension));
      end;

    begin
      addFileType(LANG_MNH   ,'mnh' ,nil                   ,'&MNH');
      addFileType(LANG_CPP   ,'cpp' ,SynCppSyn            ,'&C++');
      addFileType(LANG_CPP   ,'c'   );
      addFileType(LANG_CPP   ,'h'   );
      addFileType(LANG_CPP   ,'hh'  );
      addFileType(LANG_CSS   ,'css' ,SynCssSyn            ,'CSS'        );
      addFileType(LANG_DIFF  ,'diff',SynDiffSyn           ,'&diff'      );
      addFileType(LANG_HTML  ,'html',SynHTMLSyn           ,'&HTML'      );
      addFileType(LANG_INI   ,'ini' ,SynIniSyn            ,'&ini'       );
      addFileType(LANG_JAVA  ,'java',SynJavaSyn           ,'&Java'      );
      addFileType(LANG_JS    ,'js'  ,SynJScriptSyn        ,'JavaScript' );
      addFileType(LANG_JS    ,'json');
      addFileType(LANG_PAS   ,'pas' ,SynFreePascalSyn     ,'&Pascal'             );
      addFileType(LANG_PERL  ,'perl',SynPerlSyn           ,'Perl'                );
      addFileType(LANG_PHP   ,'php' ,SynPHPSyn            ,'PHP'                 );
      addFileType(LANG_PYTHON,'py'  ,SynPythonSyn         ,'Python'              );
      addFileType(LANG_SHELL ,'sh'  ,SynUNIXShellScriptSyn,'Shell script'        );
      addFileType(LANG_SQL   ,'sql' ,SynSQLSyn            ,'&SQL'                );
      addFileType(LANG_VB    ,'vb'  ,SynVBSyn             ,'&Visual Basic'       );
      addFileType(LANG_BAT   ,'bat' ,SynBatSyn            ,'Windows &Batch File' );
      addFileType(LANG_XML   ,'xml' ,SynXMLSyn            ,'&XML'                );
      addFileType(LANG_TXT   ,'txt' ,nil                   ,'unknown (&Text)'     );
      fileTypeMeta[LANG_OUTPUT].extensions:='OUTPUT';
      fileTypeMeta[LANG_OUTPUT].highlighter:=outputHighlighter;
      fileTypeMeta[LANG_OUTPUT].menuItem:=nil;
    end;

  begin
    initHighlighters;
    initFileTypes;
    SynHighlighterMnh.initLists;
  end;

CONSTRUCTOR T_quickEvalEditorMeta.create(CONST existingEditor: TSynEdit);
  begin
    latestAssistanceResponse:=nil;
    paintedWithStateHash:=0;
    inherited createWithExistingEditor(existingEditor,nil);
  end;

DESTRUCTOR T_quickEvalEditorMeta.destroy;
  begin
    inherited destroy;
    disposeCodeAssistanceResponse(latestAssistanceResponse);
  end;

PROCEDURE T_quickEvalEditorMeta.updateAssistanceResponse(CONST response: P_codeAssistanceResponse);
  begin
    disposeCodeAssistanceResponse(latestAssistanceResponse);
    if response=nil
    then latestAssistanceResponse:=nil
    else latestAssistanceResponse:=response^.rereferenced;
    completionLogic.assignEditor(editor_,latestAssistanceResponse,true);
    if (latestAssistanceResponse= nil) and (paintedWithStateHash<>0) or
       (latestAssistanceResponse<>nil) and (paintedWithStateHash<>latestAssistanceResponse^.stateHash) then begin
      if latestAssistanceResponse=nil
      then paintedWithStateHash:=0
      else paintedWithStateHash:=latestAssistanceResponse^.stateHash;
      if latestAssistanceResponse=nil
      then highlighter.highlightingData.clear
      else begin
        latestAssistanceResponse^.updateHighlightingData(highlighter.highlightingData);
        highlighter.highlightingData.clearLocalIdInfos; //local id infos are not applicable to quick evaluation editor
      end;
      editor.highlighter:=highlighter;
      editor.Repaint;
    end;
  end;

PROCEDURE T_basicEditorMeta.processUserCommand(Sender: TObject;
  VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  begin
    if      command=editCommandToggleComment     then begin command:=ecNone; toggleComment;              end
    else if command=editCommandMoveLineUp        then begin command:=ecNone; moveLine(true);             end
    else if command=editCommandMoveLineDown      then begin command:=ecNone; moveLine(false);            end
    else if command=ecUpperCaseBlock             then begin command:=ecNone; upperLowerCaseBlock(true);  end
    else if command=ecLowerCaseBlock             then begin command:=ecNone; upperLowerCaseBlock(false); end
    else if command=editCommandEscapeSelection   then begin command:=ecNone; escapeSelection(false);     end
    else if command=editCommandUnescapeSelection then begin command:=ecNone; escapeSelection(true);      end;
  end;

PROCEDURE T_basicEditorMeta.setLanguage(CONST languageIndex: T_language);
  begin
    language_:=languageIndex;
    if language_=LANG_MNH then editor.highlighter:=highlighter
                          else editor.highlighter:=fileTypeMeta[language_].highlighter;
    completionLogic.assignEditor(editor_,nil);
  end;

CONSTRUCTOR T_basicEditorMeta.createWithParent(CONST parent: TWinControl; CONST bookmarkImages: TImageList);
  VAR tempEdit:TSynEdit;
  begin
    tempEdit:=TSynEdit.create(parent);
    tempEdit.parent:=parent;
    tempEdit.Align:=alClient;
    createWithExistingEditor(tempEdit,bookmarkImages);
  end;

CONSTRUCTOR T_basicEditorMeta.createWithExistingEditor(CONST existingEditor:TSynEdit; CONST bookmarkImages:TImageList);
  PROCEDURE addKeystroke(CONST command:TSynEditorCommand; CONST ShortCut:TShortCut);
    VAR keyStroke:TSynEditKeyStroke;
    begin
      keyStroke:=plugin.Keystrokes.add;
      keyStroke.command:=command;
      keyStroke.ShortCut:=ShortCut;
    end;
  CONST leftArrow=37;
        upArrow=38;
        rightArrow=39;
        downArrow=40;
        pageUpKey=33;
        pageDownKey=34;
        endKey=35;
        pos1Key=36;
        insertKey=45;
        deleteKey=46;
        backspaceKey=8;
        tabKey=9;
        enterKey=13;

  VAR style:TSynSelectedColor;
      isRealEditor:boolean;
  begin
    editor_:=existingEditor;
    registerFontControl(editor_,ctEditor);
    isRealEditor:=bookmarkImages<>nil;
    completionLogic.create;
    editor_.ScrollBars:=ssAutoBoth;
    editor_.WantTabs:=false;
    editor_.Gutter.MarksPart.visible:=Assigned(bookmarkImages);
    editor_.Gutter.MarksPart.width:=45;
    editor_.Gutter.CodeFoldPart.visible:=false;
    editor_.Gutter.ChangesPart.visible:=false;
    plugin:=TSynPluginMultiCaret.create(editor_);
    plugin.editor:=editor_;
    plugin.Keystrokes.clear;
    style:=editor_.BracketMatchColor;
    style.background:=clLime;
    editor_.RightEdge:=-1;
    editor_.Keystrokes.clear;
    editor_.BookMarkOptions.GlyphsVisible:=Assigned(bookmarkImages);
    editor_.BookMarkOptions.bookmarkImages:=bookmarkImages;
    addKeystroke(ecUp                ,upArrow);
    addKeystroke(ecSelUp             ,scShift+upArrow);
    addKeystroke(ecScrollUp          ,scCtrl+upArrow);
    addKeystroke(ecDown              ,downArrow);
    addKeystroke(ecSelDown           ,scShift+downArrow);
    addKeystroke(ecScrollDown        ,scCtrl+downArrow);
    addKeystroke(ecLeft              ,leftArrow);
    addKeystroke(ecSelLeft           ,scShift+leftArrow);
    addKeystroke(ecWordLeft          ,scCtrl+leftArrow);
    addKeystroke(ecSelWordLeft       ,scShift+scCtrl+leftArrow);
    addKeystroke(ecRight             ,rightArrow);
    addKeystroke(ecSelRight          ,scShift+rightArrow);
    addKeystroke(ecWordRight         ,scCtrl+rightArrow);
    addKeystroke(ecSelWordRight      ,scShift+scCtrl+rightArrow);
    addKeystroke(ecPageDown          ,pageDownKey);
    addKeystroke(ecSelPageDown       ,scShift+pageDownKey);
    if isRealEditor then
    addKeystroke(editCommandPageRight,scCtrl+pageDownKey);
    addKeystroke(ecSelPageBottom     ,scShift+scCtrl+pageDownKey);
    addKeystroke(ecPageUp            ,pageUpKey);
    addKeystroke(ecSelPageUp         ,scShift + pageUpKey);
    if isRealEditor then
    addKeystroke(editCommandPageLeft ,scCtrl + pageUpKey);
    addKeystroke(ecSelPageTop        ,scShift + scCtrl + pageUpKey);
    addKeystroke(ecLineStart         ,pos1Key);
    addKeystroke(ecSelLineStart      ,scShift + pos1Key);
    addKeystroke(ecEditorTop         ,scCtrl + pos1Key);
    addKeystroke(ecSelEditorTop      ,scShift + scCtrl + pos1Key);
    addKeystroke(ecLineEnd           ,endKey);
    addKeystroke(ecSelLineEnd        ,scShift + endKey);
    addKeystroke(ecEditorBottom      ,scCtrl + endKey);
    addKeystroke(ecSelEditorBottom   ,scShift + scCtrl + endKey);
    addKeystroke(ecToggleMode        ,insertKey);
    addKeystroke(ecCopy              ,scCtrl + insertKey);
    addKeystroke(ecPaste             ,scShift + insertKey);
    addKeystroke(ecDeleteChar        ,deleteKey);
    addKeystroke(ecCut               ,scShift + deleteKey);
    addKeystroke(ecDeleteLastChar    ,backspaceKey);
    addKeystroke(ecUndo              ,scAlt + backspaceKey);
    addKeystroke(ecRedo              ,scShift + scAlt + backspaceKey);
    addKeystroke(ecLineBreak         ,enterKey);
    addKeystroke(ecSelectAll         ,scCtrl + ord('A'));
    addKeystroke(ecCopy              ,scCtrl + ord('C'));
    addKeystroke(ecBlockUnindent     ,scCtrl + ord('U'));
    addKeystroke(ecPaste             ,scCtrl + ord('V'));
    addKeystroke(ecCut               ,scCtrl + ord('X'));
    addKeystroke(ecDeleteLine        ,scCtrl + ord('Y'));
    addKeystroke(ecUndo              ,scCtrl + ord('Z'));
    addKeystroke(ecRedo              ,scShift + scCtrl + ord('Z'));
    addKeystroke(editCommandToggleComment  ,scShift + scCtrl + ord('C'));
    addKeystroke(ecShiftTab          ,scShift + tabKey);
    if isRealEditor then
    addKeystroke(editCommandToggleBookmark,scShift + scCtrl + ord('B'));
    addKeystroke(ecColSelUp          ,scShift + scAlt + upArrow);
    addKeystroke(ecColSelDown        ,scShift + scAlt + downArrow);
    addKeystroke(ecColSelLeft        ,scShift + scAlt + leftArrow);
    addKeystroke(ecColSelRight       ,scShift + scAlt + rightArrow);
    addKeystroke(ecColSelPageDown    ,scShift + scAlt + pageDownKey);
    addKeystroke(ecColSelPageBottom  ,scShift + scCtrl + scAlt + pageDownKey);
    addKeystroke(ecColSelPageUp      ,scShift + scAlt + pageUpKey);
    addKeystroke(ecColSelPageTop     ,scShift + scCtrl + scAlt + pageUpKey);
    addKeystroke(ecColSelEditorTop   ,scShift + scCtrl + scAlt + pos1Key);
    addKeystroke(ecColSelEditorBottom,scShift + scCtrl + scAlt + endKey);
    if Assigned(bookmarkImages) then begin
      addKeystroke(ecToggleMarker0,scCtrl+scShift+ord('0'));  addKeystroke(ecGotoMarker0,scCtrl+ord('0'));
      addKeystroke(ecToggleMarker1,scCtrl+scShift+ord('1'));  addKeystroke(ecGotoMarker1,scCtrl+ord('1'));
      addKeystroke(ecToggleMarker2,scCtrl+scShift+ord('2'));  addKeystroke(ecGotoMarker2,scCtrl+ord('2'));
      addKeystroke(ecToggleMarker3,scCtrl+scShift+ord('3'));  addKeystroke(ecGotoMarker3,scCtrl+ord('3'));
      addKeystroke(ecToggleMarker4,scCtrl+scShift+ord('4'));  addKeystroke(ecGotoMarker4,scCtrl+ord('4'));
      addKeystroke(ecToggleMarker5,scCtrl+scShift+ord('5'));  addKeystroke(ecGotoMarker5,scCtrl+ord('5'));
      addKeystroke(ecToggleMarker6,scCtrl+scShift+ord('6'));  addKeystroke(ecGotoMarker6,scCtrl+ord('6'));
      addKeystroke(ecToggleMarker7,scCtrl+scShift+ord('7'));  addKeystroke(ecGotoMarker7,scCtrl+ord('7'));
      addKeystroke(ecToggleMarker8,scCtrl+scShift+ord('8'));  addKeystroke(ecGotoMarker8,scCtrl+ord('8'));
      addKeystroke(ecToggleMarker9,scCtrl+scShift+ord('9'));  addKeystroke(ecGotoMarker9,scCtrl+ord('9'));
    end;
    addKeystroke(ecBlockIndent               ,scCtrl +           ord('I'));
    addKeystroke(ecUpperCaseBlock            ,scCtrl + scShift + ord('U'));
    addKeystroke(ecLowerCaseBlock            ,scCtrl + scShift + ord('L'));
    addKeystroke(editCommandMoveLineUp       ,scAlt  + upArrow);
    addKeystroke(editCommandMoveLineDown     ,scAlt  + downArrow);
    addKeystroke(editCommandEscapeSelection  ,scCtrl +           ord('E'));
    addKeystroke(editCommandUnescapeSelection,scCtrl + scShift + ord('E'));
    highlighter:=TSynMnhSyn.create(editor_,msf_input);
    editor_.highlighter:=highlighter;

    editor_.OnProcessCommand    :=@processUserCommand;
    setLanguage(LANG_MNH);
  end;

DESTRUCTOR T_basicEditorMeta.destroy;
  begin
    unregisterFontControl(editor_);
    completionLogic.destroy;
  end;

FUNCTION T_basicEditorMeta.getLines: T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,editor_.lines.count);
    for i:=0 to length(result)-1 do result[i]:=editor_.lines[i];
  end;

FUNCTION T_basicEditorMeta.getPath: ansistring;
  begin
    result:='';
  end;

FUNCTION T_basicEditorMeta.disposeOnPackageDestruction: boolean;
  begin
    result:=false;
  end;

FUNCTION T_basicEditorMeta.isPseudoFile: boolean;
  begin
    result:=true;
  end;

PROCEDURE T_basicEditorMeta.setLanguage(CONST extensionWithoutDot: string;
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

PROCEDURE T_basicEditorMeta.setCaret(CONST location: T_searchTokenLocation);
  VAR newCaret:TPoint;
  begin
    newCaret.x:=location.column;
    newCaret.y:=location.line;
    editor.CaretXY:=newCaret;
  end;

FUNCTION T_basicEditorMeta.currentBlockOrLine: T_lineRange;
  begin
    initialize(result);
    if editor.BlockBegin.y<1 then begin
      result[0]:=editor.CaretY;
      result[1]:=editor.CaretY;
    end else begin
      result[0]:=editor.BlockBegin.y;
      result[1]:=editor.BlockEnd.y;
      if (result[1]>result[0]) and (editor.BlockEnd.x=1) then dec(result[1]);
    end;
  end;

PROCEDURE T_basicEditorMeta.toggleComment;
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
      range:T_lineRange;
  begin
    if editor.readonly then exit;
    editor.BeginUndoBlock;
    range:=currentBlockOrLine;

    for cy:=range[0] to range[1] do commented:=commented and lineIsCommented(cy);
    if commented then for cy:=range[0] to range[1] do uncommentLine(cy)
                 else for cy:=range[0] to range[1] do   commentLine(cy);
    editor.EndUndoBlock;
  end;

PROCEDURE T_basicEditorMeta.moveLine(CONST up: boolean);
  VAR range:T_lineRange;
      blockDelta:longint=0;
  PROCEDURE moveUp;
    VAR oldLineText:string;
        oldLineStart:TPoint;
        oldLineEnd  :TPoint;
        newLineStart:TPoint;
    begin
      if (range[0]-2<0) or (range[1]>editor.lines.count) then exit;
      oldLineText:=editor.lines[range[0]-2];
      oldLineStart.y:=range[0]-1; oldLineStart.x:=1;
      oldLineEnd  .y:=range[0]  ; oldLineEnd  .x:=1;
      editor.SetTextBetweenPoints(oldLineStart,oldLineEnd,'');
      if range[1]>editor.lines.count then begin
        newLineStart.y:=range[1]-1;
        newLineStart.x:=length(editor.lines[range[1]-2])+1;
        editor.TextBetweenPoints[newLineStart,newLineStart]:=LineEnding+oldLineText;
      end else begin
        newLineStart.y:=range[1];
        newLineStart.x:=1;
        editor.TextBetweenPoints[newLineStart,newLineStart]:=oldLineText+LineEnding;
      end;
      blockDelta:=-1;
    end;

  PROCEDURE moveDown;
    VAR oldLineText:string;
        oldLineStart:TPoint;
        oldLineEnd  :TPoint;
        newLineStart:TPoint;
        lastLine:boolean=false;
    begin
      if range[1]>=editor.lines.count-1 then begin
        newLineStart.y:=                    editor.lines.count   ;
        newLineStart.x:=length(editor.lines[editor.lines.count-1])+1;
        editor.SetTextBetweenPoints(newLineStart,newLineStart,LineEnding);
        lastLine:=true;
      end;
      oldLineText:=editor.lines[range[1]];
      oldLineStart.y:=range[1]+1; oldLineStart.x:=1;
      oldLineEnd  .y:=range[1]+2; oldLineEnd  .x:=1;
      editor.SetTextBetweenPoints(oldLineStart,oldLineEnd,'');
      newLineStart.y:=range[0];
      newLineStart.x:=1;
      editor.TextBetweenPoints[newLineStart,newLineStart]:=oldLineText+LineEnding;
      if lastLine then begin
        oldLineStart.y:=editor.lines.count-1; oldLineStart.x:=length(editor.lines[editor.lines.count-2])+1;
        oldLineEnd  .y:=editor.lines.count  ; oldLineEnd  .x:=1;
        editor.SetTextBetweenPoints(oldLineStart,oldLineEnd,'');
      end;
      blockDelta:=1;
    end;

  VAR oldBegin,oldEnd,oldCaret:TPoint;
  begin
    if editor.readonly then exit;
    oldBegin:=editor.BlockBegin;
    oldEnd  :=editor.BlockEnd;
    oldCaret:=editor.CaretXY;
    range:=currentBlockOrLine;
    editor.BeginUndoBlock;
    //move lines
    if up then moveUp else moveDown;
    //update caret
     editor.CaretXY:=oldCaret;
    editor.CaretY:=editor.CaretY+blockDelta;
    //update selection range
    if (oldBegin.y>=1) then begin
      inc(oldBegin.y,blockDelta); editor.BlockBegin:=oldBegin;
      inc(oldEnd  .y,blockDelta); editor.BlockEnd  :=oldEnd;
    end;
    editor.EndUndoBlock;
  end;

PROCEDURE T_basicEditorMeta.insertText(CONST s: string);
  begin
    editor.InsertTextAtCaret(s);
  end;

PROCEDURE T_basicEditorMeta.setMarkedWord(CONST wordText: string);
  begin
    if (language_=LANG_MNH) then highlighter.setMarkedWord(wordText);
  end;

PROCEDURE T_basicEditorMeta.upperLowerCaseBlock(CONST upper: boolean);
  VAR txt:string;
      oldBegin,oldEnd,oldCaret:TPoint;
      oldMode:TSynSelectionMode;
      y:longint;
      p0,p1:TPoint;

  begin
    if editor.readonly then exit;
    oldBegin:=editor.BlockBegin;
    oldEnd  :=editor.BlockEnd;
    oldCaret:=editor.CaretXY;
    oldMode :=editor.SelectionMode;

    if oldMode=smColumn then begin
      editor.BeginUndoBlock;
      p0.x:=oldBegin.x;
      p1.x:=oldEnd  .x;
      for y:=oldBegin.y to oldEnd.y do begin
        p0.y:=y;
        p1.y:=y;
        txt:=editor.TextBetweenPoints[p0,p1];
        if upper then txt:=uppercase(txt)
                 else txt:=lowercase(txt);
        editor.SetTextBetweenPoints(p0,p1,txt);
      end;
      editor.EndUndoBlock;
    end else begin
      txt:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
      if upper then txt:=uppercase(txt)
               else txt:=lowercase(txt);
      editor.SetTextBetweenPoints(editor.BlockBegin,editor.BlockEnd,txt);
    end;
    editor.CaretXY      :=oldCaret;
    editor.BlockBegin   :=oldBegin;
    editor.BlockEnd     :=oldEnd  ;
    editor.SelectionMode:=oldMode;
  end;

PROCEDURE T_basicEditorMeta.escapeSelection(CONST unescape:boolean);
  CONST lineBreaks:array[0..1] of string=(C_lineBreakChar,C_carriageReturnChar+C_lineBreakChar);
  VAR oldTxt,newTxt:string;
      lengthDelta:longint=0;
      s,q:string;
      first:boolean=true;
      oldBegin,oldEnd,oldCaret:TPoint;
      oldMode:TSynSelectionMode;
      unescapedLength:longint=0;
      nonescapableFound:boolean;
  begin
    if editor.readonly then exit;
    oldBegin:=editor.BlockBegin;
    oldEnd  :=editor.BlockEnd;
    oldCaret:=editor.CaretXY;
    oldMode :=editor.SelectionMode;

    if oldMode=smColumn then exit;
    oldTxt:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
    if unescape then begin
      newTxt:=unescapeString(oldTxt,1,unescapedLength);
      if unescapedLength<length(oldTxt) then newTxt+=copy(oldTxt,unescapedLength+1,length(oldTxt));
      lengthDelta:=length(newTxt)-length(oldTxt);
    end else begin
      newTxt:='';
      for s in split(oldTxt,lineBreaks,false) do begin
        if not(first) then newTxt:=newTxt+'&'+C_lineBreakChar;
        q:=escapeString(s,es_pickShortest,nonescapableFound);
        newTxt+=q;
        lengthDelta:=length(q)-length(s);
        first:=false;
      end;
    end;
    if oldCaret=oldEnd then oldCaret.x+=lengthDelta;
    oldEnd.x+=lengthDelta;
    editor.SetTextBetweenPoints(editor.BlockBegin,editor.BlockEnd,newTxt);
    editor.CaretXY      :=oldCaret;
    editor.BlockBegin   :=oldBegin;
    editor.BlockEnd     :=oldEnd  ;
    editor.SelectionMode:=oldMode;
  end;

end.

