{$ifdef includeInterface}
T_editorMeta=object
  index:longint;
  filePath:ansistring;
  fileAccessAge:double;
  changed:boolean;

  sheet       : TTabSheet;
  editor      : TSynEdit;
  highlighter : TSynMnhSyn;
  PROCEDURE startDebugging;
  PROCEDURE doneDebugging;
  PROCEDURE reloadFile(CONST fileName:string);
  FUNCTION caretInMainFormCoordinates:TPoint;
  PROCEDURE setMarkedWord(CONST wordText:string);
  PROCEDURE setWorkingDir;
  PROCEDURE writeToEditorState(VAR state:T_editorState);
  PROCEDURE defineByState(CONST state:T_editorState);
  PROCEDURE toggleComment;
  PROCEDURE toggleBreakpoint;
  FUNCTION pseudoName:ansistring;
  PROCEDURE setStepperBreakpoints;
  PROCEDURE _add_breakpoint_(CONST lineIndex:longint);
end;
{$endif}
{$ifdef includeImplementation}
PROCEDURE T_editorMeta.startDebugging;
  begin
    editor.readonly:=true;
  end;

PROCEDURE T_editorMeta.doneDebugging;
  begin
    highlighter.setMarkedToken(-1,-1);
    editor.readonly:=false;
  end;

PROCEDURE T_editorMeta.reloadFile(CONST fileName:string);
  begin
    if sheet.TabVisible and not(changed) and (filePath=SysToUTF8(fileName)) then begin
      editor.lines.loadFromFile(filePath);
      fileAge(filePath,fileAccessAge);
      changed:=false;
    end;
  end;

FUNCTION T_editorMeta.caretInMainFormCoordinates:TPoint;
  begin
    result.x:=editor.CaretXPix;
    result.y:=editor.CaretYPix+editor.LineHeight;
    result:=editor.ClientToParent(result,MnhForm);
  end;

PROCEDURE T_editorMeta.setMarkedWord(CONST wordText:string);
  begin
    if highlighter.setMarkedWord(wordText) and sheet.TabVisible then editor.Repaint;
  end;

PROCEDURE T_editorMeta.setWorkingDir;
  begin
    if filePath='' then SetCurrentDir(ExtractFileDir(paramStr(0)))
                   else SetCurrentDir(ExtractFileDir(filePath));
  end;

PROCEDURE T_editorMeta.writeToEditorState(VAR state:T_editorState);
  VAR i:longint;
  begin
    state.visible:=sheet.TabVisible;
    if not(state.visible) then exit;
    setLength(state.markedLines,0);
    for i:=0 to editor.Marks.count-1 do appendIfNew(state.markedLines,editor.Marks[i].line);

    state.filePath:=filePath;
    state.fileAccessAge:=fileAccessAge;
    state.changed:=changed;
    setLength(state.lines,editor.Lines.Count);
    for i:=0 to length(State.lines)-1 do state.lines[i]:=editor.Lines[i];
  end;

PROCEDURE T_editorMeta.defineByState(CONST state:T_editorState);
  VAR i:longint;
  begin
    sheet.TabVisible:=state.visible;
    if state.visible then begin
      changed      :=state.changed;
      fileAccessAge:=state.fileAccessAge;
      filePath     :=state.filePath;
      state.getLines(editor.lines);
      for i:=0 to length(state.markedLines)-1 do _add_breakpoint_(state.markedLines[i]);
    end;
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

FUNCTION T_editorMeta.pseudoName:ansistring;
  begin
    if filePath<>'' then result:=filePath
                    else result:='<new '+intToStr(index)+'>';
  end;

PROCEDURE T_editorMeta.setStepperBreakpoints;
  VAR i:longint;
  begin
    for i:=0 to editor.Marks.count-1 do stepper.addBreakpoint(pseudoName,editor.Marks[i].line);
  end;

PROCEDURE T_editorMeta._add_breakpoint_(CONST lineIndex:longint);
  VAR m:TSynEditMark;
  begin
    m:=TSynEditMark.create(editor);
    m.line:=lineIndex;
    m.ImageList:=MnhForm.breakpointsImagesList;
    m.ImageIndex:=0;
    m.visible:=true;
    editor.Marks.add(m);
  end;

{$endif}
