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
  mnh_constants, mnh_basicTypes, mnh_fileWrappers,
  mnhCompletion;

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
      PROCEDURE processUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
      CONSTRUCTOR createWithParent(CONST parent:TWinControl; CONST bookmarkImages:TImageList);
      DESTRUCTOR destroy; virtual;
      //T_codeProvider:
      FUNCTION getLines: T_arrayOfString; virtual;
      FUNCTION getPath: ansistring;                                    virtual;
      FUNCTION stateHash:T_hashInt;                                    virtual;
      FUNCTION disposeOnPackageDestruction:boolean;                    virtual;
      FUNCTION isPseudoFile:boolean;                                   virtual;

      PROPERTY language:T_language read language_ write setLanguage;
      PROPERTY editor:TSynEdit read editor_;
      PROCEDURE setLanguage(CONST extensionWithoutDot:string; CONST fallback:T_language);

      PROCEDURE activate; virtual;

      PROCEDURE setCaret(CONST location: T_searchTokenLocation);
      PROCEDURE toggleComment;
      PROCEDURE moveLine(CONST up:boolean);
      PROCEDURE insertText(CONST s: string);
      PROCEDURE setMarkedWord(CONST wordText:string);
      PROCEDURE upperLowerCaseBlock(CONST upper:boolean);
  end;

PROCEDURE setupEditorMetaBase(CONST outputHighlighter:TSynMnhSyn;
                              CONST languageMenuRoot :TMenuItem);
VAR fileTypeMeta:array[T_language] of record
      highlighter:TSynCustomHighlighter;
      extensions:T_arrayOfString;
      menuItem:TMenuItem;
    end;
    editorFont:TFont;
    disposeHighlighters:boolean=false;
IMPLEMENTATION

PROCEDURE setupEditorMetaBase(CONST outputHighlighter:TSynMnhSyn; CONST languageMenuRoot        :TMenuItem);
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
      SynBatSyn1            :=TSynBatSyn            .create(languageMenuRoot);
      SynCppSyn1            :=TSynCppSyn            .create(languageMenuRoot);
      SynCssSyn1            :=TSynCssSyn            .create(languageMenuRoot);
      SynDiffSyn1           :=TSynDiffSyn           .create(languageMenuRoot);
      SynFreePascalSyn1     :=TSynFreePascalSyn     .create(languageMenuRoot);
      SynHTMLSyn1           :=TSynHTMLSyn           .create(languageMenuRoot);
      SynIniSyn1            :=TSynIniSyn            .create(languageMenuRoot);
      SynJScriptSyn1        :=TSynJScriptSyn        .create(languageMenuRoot);
      SynJavaSyn1           :=TSynJavaSyn           .create(languageMenuRoot);
      SynPHPSyn1            :=TSynPHPSyn            .create(languageMenuRoot);
      SynPerlSyn1           :=TSynPerlSyn           .create(languageMenuRoot);
      SynPythonSyn1         :=TSynPythonSyn         .create(languageMenuRoot);
      SynSQLSyn1            :=TSynSQLSyn            .create(languageMenuRoot);
      SynUNIXShellScriptSyn1:=TSynUNIXShellScriptSyn.create(languageMenuRoot);
      SynVBSyn1             :=TSynVBSyn             .create(languageMenuRoot);
      SynXMLSyn1            :=TSynXMLSyn            .create(languageMenuRoot);
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
          if Assigned(languageMenuRoot) then begin
            menuItem:=TMenuItem.create(languageMenuRoot);
            menuItem.caption:=menuCaption;
            menuItem.Tag:=ord(language);
            languageMenuRoot.add(menuItem);
            fileTypeMeta[language].menuItem:=menuItem;
          end else fileTypeMeta[language].menuItem:=nil;
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
      fileTypeMeta[LANG_OUTPUT].extensions:='OUTPUT';
      fileTypeMeta[LANG_OUTPUT].highlighter:=outputHighlighter;
      fileTypeMeta[LANG_OUTPUT].menuItem:=nil;
    end;

  begin
    initHighlighters;
    initFileTypes;
    disposeHighlighters:=not(Assigned(languageMenuRoot));
  end;

PROCEDURE T_basicEditorMeta.processUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  begin
    if      command=ecUserDefinedFirst   then begin command:=ecNone; toggleComment;              end
    else if command=ecUserDefinedFirst+4 then begin command:=ecNone; moveLine(true);             end
    else if command=ecUserDefinedFirst+5 then begin command:=ecNone; moveLine(false);            end
    else if command=ecUpperCaseBlock     then begin command:=ecNone; upperLowerCaseBlock(true);  end
    else if command=ecLowerCaseBlock     then begin command:=ecNone; upperLowerCaseBlock(false); end;
  end;

PROCEDURE T_basicEditorMeta.setLanguage(CONST languageIndex: T_language);
  begin
    if language_=languageIndex then exit;
    language_:=languageIndex;
    activate;
  end;

CONSTRUCTOR T_basicEditorMeta.createWithParent(CONST parent: TWinControl; CONST bookmarkImages:TImageList);
  PROCEDURE addKeystroke(CONST command:TSynEditorCommand; CONST ShortCut:TShortCut);
    VAR keyStroke:TSynEditKeyStroke;
    begin
      keyStroke:=plugin.Keystrokes.add;
      keyStroke.command:=command;
      keyStroke.ShortCut:=ShortCut;
    end;

  VAR style:TSynSelectedColor;
  begin
    completionLogic.create;
    editor_:=TSynEdit.create(parent);
    editor_.parent:=parent;
    editor_.Align:=alClient;
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
    addKeystroke(ecUndo,32776);
    addKeystroke(ecRedo,40968);
    addKeystroke(ecLineBreak,13);
    addKeystroke(ecSelectAll,16449);
    addKeystroke(ecCopy,16451);
    addKeystroke(ecBlockUnindent,16469);
    addKeystroke(ecPaste,16470);
    addKeystroke(ecCut,16472);
    addKeystroke(ecDeleteLine,16473);
    addKeystroke(ecUndo,16474);
    addKeystroke(ecRedo,24666);
    addKeystroke(ecUserDefinedFirst,24643);
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
    addKeystroke(ecColSelEditorTop,57380);
    addKeystroke(ecColSelEditorBottom,57379);
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
    addKeystroke(ecBlockIndent,16457);
    addKeystroke(ecUpperCaseBlock,scCtrl+scShift+ord('U'));
    addKeystroke(ecLowerCaseBlock,scCtrl+scShift+ord('L'));
    addKeystroke(ecUserDefinedFirst+4,32806);//Alt+Up
    addKeystroke(ecUserDefinedFirst+5,32808);//Alt+Down
    highlighter:=TSynMnhSyn.create(parent,msf_input);
    editor_.highlighter:=highlighter;

    editor_.OnProcessCommand    :=@processUserCommand;
  end;

DESTRUCTOR T_basicEditorMeta.destroy;
  begin
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

FUNCTION T_basicEditorMeta.stateHash: T_hashInt;
  VAR s:ansistring;
  begin
    {$Q-}{$R-}
    try
      result:=editor_.lines.count;
      for s in editor_.lines do result:=result*31+hashOfAnsiString(s);
    except
      exit(0);
    end;
    {$Q+}{$R+}
    if result=0 then result:=1;
  end;

FUNCTION T_basicEditorMeta.disposeOnPackageDestruction: boolean;
  begin
    result:=false;
  end;

FUNCTION T_basicEditorMeta.isPseudoFile: boolean;
  begin
    result:=true;
  end;

PROCEDURE T_basicEditorMeta.setLanguage(CONST extensionWithoutDot: string; CONST fallback: T_language);
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

PROCEDURE T_basicEditorMeta.activate;
  begin
    editor_.Font:=editorFont;
    if language_=LANG_MNH then editor.highlighter:=highlighter
                          else editor.highlighter:=fileTypeMeta[language_].highlighter;
    completionLogic.assignEditor(editor_,nil);
  end;

PROCEDURE T_basicEditorMeta.setCaret(CONST location: T_searchTokenLocation);
  VAR newCaret:TPoint;
  begin
    newCaret.x:=location.column;
    newCaret.y:=location.line;
    editor.CaretXY:=newCaret;
  end;

FUNCTION T_basicEditorMeta.currentBlockOrLine:T_lineRange;
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
      if (range[0]-2<0) or (range[1]>=editor.lines.count) then exit;
      oldLineText:=editor.lines[range[0]-2];
      oldLineStart.y:=range[0]-1; oldLineStart.x:=1;
      oldLineEnd  .y:=range[0]  ; oldLineEnd  .x:=1;
      editor.SetTextBetweenPoints(oldLineStart,oldLineEnd,'');
      newLineStart.y:=range[1];
      newLineStart.x:=1;
      editor.TextBetweenPoints[newLineStart,newLineStart]:=oldLineText+LineEnding;
      blockDelta:=-1;
    end;

  PROCEDURE moveDown;
    VAR oldLineText:string;
        oldLineStart:TPoint;
        oldLineEnd  :TPoint;
        newLineStart:TPoint;
    begin
      if range[1]>=editor.lines.count-1 then exit;
      oldLineText:=editor.lines[range[1]];
      oldLineStart.y:=range[1]+1; oldLineStart.x:=1;
      oldLineEnd  .y:=range[1]+2; oldLineEnd  .x:=1;
      editor.SetTextBetweenPoints(oldLineStart,oldLineEnd,'');
      newLineStart.y:=range[0];
      newLineStart.x:=1;
      editor.TextBetweenPoints[newLineStart,newLineStart]:=oldLineText+LineEnding;
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

PROCEDURE T_basicEditorMeta.upperLowerCaseBlock(CONST upper:boolean);
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

FINALIZATION
  if disposeHighlighters then begin
    fileTypeMeta[LANG_CPP   ].highlighter.destroy;
    fileTypeMeta[LANG_CSS   ].highlighter.destroy;
    fileTypeMeta[LANG_DIFF  ].highlighter.destroy;
    fileTypeMeta[LANG_HTML  ].highlighter.destroy;
    fileTypeMeta[LANG_INI   ].highlighter.destroy;
    fileTypeMeta[LANG_JAVA  ].highlighter.destroy;
    fileTypeMeta[LANG_JS    ].highlighter.destroy;
    fileTypeMeta[LANG_PAS   ].highlighter.destroy;
    fileTypeMeta[LANG_PERL  ].highlighter.destroy;
    fileTypeMeta[LANG_PHP   ].highlighter.destroy;
    fileTypeMeta[LANG_PYTHON].highlighter.destroy;
    fileTypeMeta[LANG_SHELL ].highlighter.destroy;
    fileTypeMeta[LANG_SQL   ].highlighter.destroy;
    fileTypeMeta[LANG_VB    ].highlighter.destroy;
    fileTypeMeta[LANG_BAT   ].highlighter.destroy;
    fileTypeMeta[LANG_XML   ].highlighter.destroy;
  end;
end.

