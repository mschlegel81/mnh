{$ifdef includeInterface}
P_editorMeta=^T_editorMeta;

{ T_editorMeta }

T_editorMeta=object(T_codeProvider)
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

  DESTRUCTOR destroy; virtual;
  PROCEDURE closeEditor;
  //T_codeProvider:
  FUNCTION getLines: T_arrayOfString; virtual;
  FUNCTION getPath: ansistring;                                    virtual;
  FUNCTION stateHash:T_hashInt;                                    virtual;
  FUNCTION disposeOnPackageDestruction:boolean;                    virtual;
  FUNCTION isPseudoFile:boolean;                                   virtual;
  //Events handling:
  PROCEDURE InputEditChange(Sender: TObject);
  PROCEDURE guiStatusChange(Sender: TObject);

  //Advanced:
  FUNCTION saveAsWithDialog:boolean;
  FUNCTION saveWithDialog:boolean;

  PROCEDURE setLanguage(CONST languageIndex:byte);
  PROCEDURE setLanguage(CONST extensionWithoutDot:string; CONST fallback:byte);
  PROCEDURE guessLanguage(CONST fallback:byte);
  FUNCTION languageName:string;

  //Presentation helper:
  FUNCTION caretInMainFormCoordinates:TPoint;
  PROCEDURE setCaret(CONST location: T_searchTokenLocation);

  FUNCTION isFile:boolean;


  PROCEDURE setFile(CONST fileName:string);
  PROCEDURE initForNewFile;
  PROCEDURE reloadFile(CONST fileName:string);
  PROCEDURE setMarkedWord(CONST wordText:string);
  PROCEDURE setWorkingDir;
  PROCEDURE writeToEditorState(CONST settings:P_Settings);
  PROCEDURE toggleComment;
  PROCEDURE insertText(CONST s:string);
  PROCEDURE toggleBreakpoint;
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
  PROCEDURE exportToHtml(CONST fileName:string);
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
    registerOnGuiStatusChange(@guiStatusChange);
    editor.OnChange:=@InputEditChange;
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

PROCEDURE T_editorMeta.InputEditChange(Sender: TObject);
  begin
    if not(getStatus.active) or not(sheet.tabVisible) then exit;
    if language=LANG_MNH then assistancEvaluator.evaluate(@self);
    MnhForm.caption:=updateSheetCaption;
  end;

PROCEDURE T_editorMeta.guiStatusChange(Sender: TObject);
  begin
    editor.Gutter.MarksPart.visible:=(language=LANG_MNH) and (getStatus.debugMode);
    editor.readonly:=(getStatus.debugMode and getStatus.running) or (getStatus.scriptEditing);
  end;

FUNCTION T_editorMeta.saveAsWithDialog: boolean;
begin
  with MnhForm do begin
    if language=LANG_MNH
    then begin
      SaveDialog.FilterIndex:=1;
      SaveDialog.options:=SaveDialog.options+[ofExtensionDifferent];
    end else begin
      SaveDialog.FilterIndex:=2;
      SaveDialog.options:=SaveDialog.options-[ofExtensionDifferent];
    end;
    if SaveDialog.execute then begin
      MnhForm.caption:=saveFile(SaveDialog.fileName);
      result:=true;
    end else result:=false;
  end;

end;

FUNCTION T_editorMeta.saveWithDialog: boolean;
  begin
    if isFile then begin
      MnhForm.caption:=saveFile();
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
    language:=languageIndex;
    for metaIdx:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[metaIdx].language=languageIndex then begin
      if fileTypeMeta[metaIdx].language=LANG_MNH
      then editor.highlighter:=highlighter
      else editor.highlighter:=fileTypeMeta[metaIdx].highlighter;
      {$ifdef debugMode}writeln('Set language ',fileTypeMeta[metaIdx].language,' - ',fileTypeMeta[metaIdx].extensionWithoutDot);{$endif}
      exit;
    end;
  end;

PROCEDURE T_editorMeta.setLanguage(CONST extensionWithoutDot: string;
  CONST fallback: byte);
  VAR metaIdx:longint;
      ext:string;
  begin
    ext:=uppercase(extensionWithoutDot);
    for metaIdx:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[metaIdx].extensionWithoutDot=ext then begin
      language:=fileTypeMeta[metaIdx].language;
      if fileTypeMeta[metaIdx].highlighter=nil
      then editor.highlighter:=highlighter
      else editor.highlighter:=fileTypeMeta[metaIdx].highlighter;
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
    result:=editor.ClientToParent(result,MnhForm);
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
    for i:=0 to length(fileTypeMeta)-1 do if fileTypeMeta[i].language=language then exit(lowercase(fileTypeMeta[i].extensionWithoutDot));
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

PROCEDURE T_editorMeta.exportToHtml(CONST fileName: string);
  begin
    with MnhForm do begin
      SynExporterHTML.title:=pseudoName();
      SynExporterHTML.highlighter:=editor.highlighter;
      SynExporterHTML.ExportAll(editor.lines);
      SynExporterHTML.saveToFile(fileName);
    end;
  end;

//==================================================================
VAR editorMetaData:array of P_editorMeta;
FUNCTION hasEditor:boolean;
  VAR i:longint;
  begin
    i:=MnhForm.inputPageControl.activePageIndex;
    result:=(i>=0) and (i<length(editorMetaData));
  end;

FUNCTION getEditor:P_editorMeta;
  VAR i:longint;
  begin
    i:=MnhForm.inputPageControl.activePageIndex;
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
    editorMetaData[i]^.editor.Font:=MnhForm.OutputEdit.Font;

    result:=i;
    if MnhForm.miDebug.Checked then editorMetaData[i]^.editor.Gutter.MarksPart.visible:=true;
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
        editorMetaData[result]^.editor.Font:=MnhForm.OutputEdit.Font;
      end;
    end;


  VAR f:string;
  begin
    if useCurrentPageAsFallback then result:=MnhForm.inputPageControl.activePageIndex
                                else result:=-1;
    for f in FileNames do openSingleFile(f);
    MnhForm.updateMainMenuItems(true,false);
  end;

{$endif}
