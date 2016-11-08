{$ifdef includeInterface}
P_editorMeta=^T_editorMeta;
T_editorMeta=object
  index:longint;
  paintedWithState:longint;
  fileInfo:record
    filePath:ansistring;
    fileAccessAge:double;
    isChanged:boolean;
  end;
  language:byte;

  sheet       : TTabSheet;
  editor      : TSynEdit;
  plugin      : TSynPluginMultiCaret;
  highlighter : TSynMnhSyn;
  CONSTRUCTOR create(CONST idx:longint);
  CONSTRUCTOR create(CONST idx:longint; VAR state:T_editorState);
  PROCEDURE initWithState(VAR state:T_editorState);
  DESTRUCTOR destroy;

  PROCEDURE startDebugging;
  PROCEDURE doneDebugging;
  PROCEDURE closeEditor;
  PROCEDURE setLanguage(CONST languageIndex:byte);
  PROCEDURE setLanguage(CONST extensionWithoutDot:string);
  PROCEDURE guessLanguage;
  PROCEDURE setFile(CONST fileName:string);
  PROCEDURE initForNewFile;
  PROCEDURE reloadFile(CONST fileName:string);
  FUNCTION caretInMainFormCoordinates:TPoint;
  PROCEDURE setMarkedWord(CONST wordText:string);
  PROCEDURE setWorkingDir;
  PROCEDURE writeToEditorState(CONST settings:P_Settings);
  PROCEDURE toggleComment;
  PROCEDURE insertText(CONST s:string);
  PROCEDURE toggleBreakpoint;
  FUNCTION pseudoName(CONST short:boolean=false):ansistring;
  FUNCTION isFile:boolean;
  PROCEDURE setStepperBreakpoints;
  PROCEDURE _add_breakpoint_(CONST lineIndex:longint);
  FUNCTION updateSheetCaption:ansistring;
  PROCEDURE repaintWithStateCounter(CONST stateCounter:longint; CONST errorHints:T_arrayOfString);
  FUNCTION changed:boolean;
  FUNCTION saveFile(CONST fileName:string=''):string;
  FUNCTION fileIsDeleted:boolean;
  FUNCTION fileIsModifiedOnFileSystem:boolean;
  FUNCTION languageName:string;
  PROCEDURE updateContentAfterEditScript(CONST stringListLiteral:P_listLiteral);
end;
{$endif}
{$ifdef includeImplementation}

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
    paintedWithState:=-1;
    index:=idx;
    sheet:=TTabSheet.create(MnhForm.inputPageControl);
    sheet.PageControl:=MnhForm.inputPageControl;

    editor:=TSynEdit.create(MnhForm);
    editor.parent:=sheet;
    editor.Align:=alClient;
    editor.ScrollBars:=ssAutoBoth;
    editor.WantTabs:=false;
    editor.Gutter.MarksPart.visible:=false;
    editor.Gutter.CodeFoldPart.visible:=false;
    plugin:=TSynPluginMultiCaret.create(editor);
    plugin.editor:=editor;
    plugin.Keystrokes.clear;
    style:=editor.BracketMatchColor;
    style.background:=clLime;
    editor.OnChange:=@MnhForm.InputEditChange;
    editor.OnKeyUp:=@MnhForm.InputEditKeyDown;
    editor.OnMouseDown:=@MnhForm.InputEditMouseDown;
    editor.OnProcessCommand:=@MnhForm.InputEditProcessUserCommand;
    editor.OnProcessUserCommand:=@MnhForm.InputEditProcessUserCommand;
    editor.OnSpecialLineMarkup:=@MnhForm.InputEditSpecialLineMarkup;
    editor.Gutter.width:=34;
    editor.RightEdge:=-1;
    editor.Keystrokes.clear;
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

    highlighter:=TSynMnhSyn.create(MnhForm,msf_input);
    editor.highlighter:=highlighter;
    editor.PopupMenu:=MnhForm.EditorPopupMenu;
    initForNewFile;
  end;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint; VAR state: T_editorState);
  begin
    create(idx);
    initWithState(state);
  end;

PROCEDURE T_editorMeta.initWithState(VAR state:T_editorState);
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

PROCEDURE T_editorMeta.startDebugging;
  begin
    editor.readonly:=true;
  end;

PROCEDURE T_editorMeta.doneDebugging;
  begin
    highlighter.setMarkedToken(-1,-1);
    editor.readonly:=false;
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

PROCEDURE T_editorMeta.setLanguage(CONST languageIndex:byte);
  VAR metaIdx:longint;
  begin
    language:=languageIndex;
    for metaIdx:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[metaIdx].language=languageIndex then begin
      if fileTypeMeta[metaIdx].highlighter=nil
      then editor.highlighter:=highlighter
      else editor.highlighter:=fileTypeMeta[metaIdx].highlighter;
      exit;
    end;
  end;

PROCEDURE T_editorMeta.setLanguage(CONST extensionWithoutDot:string);
  VAR metaIdx:longint;
      ext:string;
  begin
    ext:='.'+uppercase(extensionWithoutDot);
    for metaIdx:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[metaIdx].extension=ext then begin
      language:=fileTypeMeta[metaIdx].language;
      if fileTypeMeta[metaIdx].highlighter=nil
      then editor.highlighter:=highlighter
      else editor.highlighter:=fileTypeMeta[metaIdx].highlighter;
      exit;
    end;
    setLanguage(0);
  end;

PROCEDURE T_editorMeta.guessLanguage();
  VAR metaIdx:longint;
      ext:string;
  begin
    ext:=uppercase(extractFileExt(fileInfo.filePath));
    for metaIdx:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[metaIdx].extension=ext then begin
      language:=fileTypeMeta[metaIdx].language;
      if fileTypeMeta[metaIdx].highlighter=nil
      then editor.highlighter:=highlighter
      else editor.highlighter:=fileTypeMeta[metaIdx].highlighter;
      exit;
    end;
    setLanguage(0);
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
    guessLanguage;
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
    end else if sheet.tabVisible and (fileName=pseudoName) and (runEvaluator.getCodeProvider^.getPath=pseudoName) then begin
      runEvaluator.getCodeProvider^.getLinesUTF8(editor.lines);
      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
      editor.modified:=changed;
    end;
  end;

FUNCTION T_editorMeta.caretInMainFormCoordinates: TPoint;
  begin
    result.x:=editor.CaretXPix;
    result.y:=editor.CaretYPix+editor.LineHeight;
    result:=editor.ClientToParent(result,MnhForm);
  end;

PROCEDURE T_editorMeta.setMarkedWord(CONST wordText: string);
  begin
    if sheet.tabVisible and highlighter.setMarkedWord(wordText) then editor.Repaint;
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
      exit;
    end;
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
    for i:=0 to editor.Marks.count-1 do runEvaluator.context.addBreakpoint(pseudoName,editor.Marks[i].line);
  end;

PROCEDURE T_editorMeta._add_breakpoint_(CONST lineIndex: longint);
  VAR m:TSynEditMark;
  begin
    m:=TSynEditMark.create(editor);
    m.line:=lineIndex;
    m.ImageList:=MnhForm.breakpointsImagesList;
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
      MnhForm.assistanceSynEdit.clearAll;
      MnhForm.assistanceSynEdit.lines.clear;
      for i:=0 to length(errorHints)-1 do MnhForm.assistanceSynEdit.lines.add(errorHints[i]);
    end;
  end;

FUNCTION T_editorMeta.changed: boolean;
  begin
    result:=fileInfo.isChanged or editor.modified;
  end;

FUNCTION T_editorMeta.saveFile(CONST fileName: string):string;
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
      if filePath=editScriptFileName then runEvaluator.ensureEditScripts();
    end;
    result:=updateSheetCaption;
  end;

FUNCTION T_editorMeta.fileIsDeleted:boolean;
  begin
    result:=sheet.tabVisible and isFile and not(fileExists(fileInfo.filePath));
  end;

FUNCTION T_editorMeta.fileIsModifiedOnFileSystem:boolean;
  VAR currentFileAge:double;
  begin
    if not(sheet.tabVisible and isFile) or changed then exit(false);
    fileAge(fileInfo.filePath,currentFileAge);
    result:=currentFileAge<>fileInfo.fileAccessAge;
  end;

FUNCTION T_editorMeta.languageName:string;
  VAR i:longint;
  begin
    for i:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[i].language=language then exit(lowercase(copy(fileTypeMeta[i].extension,2,10)));
    result:='';
  end;

PROCEDURE T_editorMeta.updateContentAfterEditScript(CONST stringListLiteral:P_listLiteral);
  VAR concatenatedText:ansistring='';
      i:longint;
  begin
    if stringListLiteral^.literalType<>lt_stringList then exit;
    for i:=0 to stringListLiteral^.size-1 do begin
      if i>0 then concatenatedText:=concatenatedText+LineEnding;
      concatenatedText:=concatenatedText+P_stringLiteral(stringListLiteral^.value(i))^.value;
    end;
    editor.BeginUndoBlock;
    editor.SelectAll;
    editor.SelText:=concatenatedText;
    editor.EndUndoBlock;
  end;

{$endif}
