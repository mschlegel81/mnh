{$ifdef includeInterface}
T_editorMeta=object
  index:longint;
  filePath:ansistring;
  fileAccessAge:double;
  changed:boolean;

  sheet       : TTabSheet;
  editor      : TSynEdit;
  highlighter : TSynMnhSyn;
  CONSTRUCTOR create(CONST idx:longint);
  CONSTRUCTOR create(CONST idx:longint; CONST state:T_editorState);
  DESTRUCTOR destroy;

  PROCEDURE startDebugging;
  PROCEDURE doneDebugging;
  PROCEDURE closeEditor;
  PROCEDURE setFile(CONST fileName:string);
  PROCEDURE initForNewFile;
  PROCEDURE reloadFile(CONST fileName:string);
  FUNCTION caretInMainFormCoordinates:TPoint;
  PROCEDURE setMarkedWord(CONST wordText:string);
  PROCEDURE setWorkingDir;
  PROCEDURE writeToEditorState(CONST settings:P_Settings);
  PROCEDURE toggleComment;
  PROCEDURE toggleBreakpoint;
  FUNCTION pseudoName(CONST short:boolean=false):ansistring;
  PROCEDURE setStepperBreakpoints;
  PROCEDURE _add_breakpoint_(CONST lineIndex:longint);
  FUNCTION updateSheetCaption:ansistring;
end;
{$endif}
{$ifdef includeImplementation}

CONSTRUCTOR T_editorMeta.create(CONST idx: longint);
  PROCEDURE addKeystroke(CONST command:TSynEditorCommand; CONST shortCut:TShortCut);
    VAR keyStroke:TSynEditKeyStroke;
    begin
      keyStroke:=editor.Keystrokes.add;
      keyStroke.command:=command;
      keyStroke.ShortCut:=shortCut;
    end;

  begin
    index:=idx;
    sheet:=TTabSheet.create(MnhForm.PageControl);
    sheet.PageControl:=MnhForm.PageControl;

    editor:=TSynEdit.create(MnhForm);
    editor.parent:=sheet;
    editor.Align:=alClient;
    editor.ScrollBars:=ssAutoBoth;
    editor.WantTabs:=false;

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

    initForNewFile;
  end;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint; CONST state: T_editorState);
  VAR i:longint;
  begin
    create(idx);
    sheet.TabVisible:=true;
    changed      :=state.changed;
    fileAccessAge:=state.fileAccessAge;
    filePath     :=state.filePath;
    state.getLines(editor.lines);
    for i:=0 to length(state.markedLines)-1 do _add_breakpoint_(state.markedLines[i]);
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
    sheet.TabVisible:=false;
    editor.ClearAll;
    filePath:='';
    changed:=false;
  end;

PROCEDURE T_editorMeta.setFile(CONST fileName: string);
  begin
    sheet.TabVisible:=true;
    filePath:=fileName;
    editor.clear;
    editor.lines.loadFromFile(filePath);
    fileAge(filePath,fileAccessAge);
    changed:=false;
  end;

PROCEDURE T_editorMeta.initForNewFile;
  begin
    sheet.TabVisible:=true;
    changed      :=false;
    fileAccessAge:=0;
    filePath     :='';
    editor.ClearAll;
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.reloadFile(CONST fileName: string);
  begin
    if sheet.TabVisible and not(changed) and (filePath=SysToUTF8(fileName)) then begin
      editor.lines.loadFromFile(filePath);
      fileAge(filePath,fileAccessAge);
      changed:=false;
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
    if sheet.TabVisible and highlighter.setMarkedWord(wordText) then editor.Repaint;
  end;

PROCEDURE T_editorMeta.setWorkingDir;
  begin
    if filePath='' then SetCurrentDir(ExtractFileDir(paramStr(0)))
                   else SetCurrentDir(ExtractFileDir(filePath));
  end;

PROCEDURE T_editorMeta.writeToEditorState(CONST settings: P_Settings);
  VAR i:longint;
  begin
    i:=length(settings^.editorState);
    while i<=index do begin
      setLength(settings^.editorState,i+1);
      settings^.editorState[i].create;
      inc(i);
    end;
    settings^.editorState[index].visible:=sheet.TabVisible;
    if not(settings^.editorState[index].visible) then exit;
    setLength(settings^.editorState[index].markedLines,0);
    for i:=0 to editor.Marks.count-1 do appendIfNew(settings^.editorState[index].markedLines,editor.Marks[i].line);

    settings^.editorState[index].filePath:=filePath;
    settings^.editorState[index].fileAccessAge:=fileAccessAge;
    settings^.editorState[index].changed:=changed;
    setLength(settings^.editorState[index].lines,editor.lines.count);
    for i:=0 to length(settings^.editorState[index].lines)-1 do settings^.editorState[index].lines[i]:=editor.lines[i];
  end;

PROCEDURE T_editorMeta.toggleComment;
  VAR i0,i1,i:longint;
      commented:boolean=true;
  begin
    if (editor.BlockBegin.y<1) then exit;
    i0:=editor.BlockBegin.y-1;
    i1:=editor.BlockEnd.y-1;
    if editor.BlockEnd.x<=1 then dec(i1);
    for i:=i0 to i1 do commented:=commented and (copy(trim(editor.lines[i]),1,2)='//');
    if commented then for i:=i0 to i1 do editor.lines[i]:=replaceOne(editor.lines[i],'//','')
                 else for i:=i0 to i1 do editor.lines[i]:='//'+editor.lines[i];
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

FUNCTION T_editorMeta.pseudoName(CONST short: boolean): ansistring;
  begin
    if filePath<>'' then begin
                           if short then result:=extractFileName(filePath)
                                    else result:=filePath;
                         end
                    else result:='<new '+intToStr(index)+'>';
  end;

PROCEDURE T_editorMeta.setStepperBreakpoints;
  VAR i:longint;
  begin
    for i:=0 to editor.Marks.count-1 do stepper.addBreakpoint(pseudoName,editor.Marks[i].line);
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
    result:=pseudoName(true);
    if changed then result:=result+' *';
    sheet.Caption:=result;
    result:='MNH5 '+result;
  end;

{$endif}
