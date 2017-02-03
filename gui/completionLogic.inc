{$ifdef includeInterface}
PROCEDURE ensureWordsInEditorForCompletion;
PROCEDURE SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
PROCEDURE SynCompletionExecute(Sender: TObject);
PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);
{$endif}
{$ifdef includeImplementation}
PROCEDURE TMnhForm.ensureWordsInEditorForCompletion;
  VAR caret:TPoint;
      i:longint;
  begin
    with editorMeta[inputPageControl.activePageIndex]^ do caret:=editor.CaretXY;
    if (wordsInEditor.size>0) and (lastWordsCaret=caret.y) then exit;
    lastWordsCaret:=caret.y;
    wordsInEditor.clear;
    with editorMeta[inputPageControl.activePageIndex]^ do begin
      for i:=0 to editor.lines.count-1 do
        if i=caret.y-1 then collectIdentifiers(editor.lines[i],wordsInEditor,caret.x)
                       else collectIdentifiers(editor.lines[i],wordsInEditor,-1);
      if language=LANG_MNH then assistancEvaluator.extendCompletionList(wordsInEditor);
    end;
  end;

PROCEDURE TMnhForm.SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
  begin
    value:=copy(sourceValue,1,LastDelimiter('.',sourceValue)-1)+value;
    wordsInEditor.clear;
  end;

PROCEDURE TMnhForm.SynCompletionExecute(Sender: TObject);
  VAR s:string;
      w:string;
  begin
    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    for w in wordsInEditor.values do if (s='') or (pos(s,w)=1) then SynCompletion.ItemList.add(w);
  end;

PROCEDURE TMnhForm.SynCompletionSearchPosition(VAR APosition: integer);
  VAR i:longint;
      s:string;
      w:string;
  begin
    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    i:=LastDelimiter('.',s);
    if i>1 then begin
      s:=copy(s,i,length(s));
      SynCompletion.CurrentString:=s;
    end;
    for w in wordsInEditor.values do if pos(s,w)=1 then SynCompletion.ItemList.add(w);
    if SynCompletion.ItemList.count>0 then APosition:=0 else APosition:=-1;
  end;
{$endif}
