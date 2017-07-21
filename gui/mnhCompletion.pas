UNIT mnhCompletion;
INTERFACE
USES Classes, sysutils, LCLType, types,
     //my utilities:
     myStringUtil, myGenerics,
     //GUI: SynEdit
     SynEdit, SynCompletion,
     mnh_litVar,
     mnh_packages;

TYPE
T_completionLogic=object
  private
    wordsInEditor:T_setOfString;
    editor:TSynEdit;
    lastWordsCaret:longint;
    SynCompletion:TSynCompletion;
    package:P_package;
    PROCEDURE ensureWordsInEditorForCompletion;
  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE assignEditor(CONST edit:TSynEdit; CONST pack:P_package);
    PROCEDURE SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    PROCEDURE SynCompletionExecute(Sender: TObject);
    PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);
end;

IMPLEMENTATION
PROCEDURE T_completionLogic.ensureWordsInEditorForCompletion;
  VAR caret:TPoint;
      i:longint;
  begin
    caret:=editor.CaretXY;
    if (wordsInEditor.size>0) and (lastWordsCaret=caret.y) then exit;
    lastWordsCaret:=caret.y;
    if package<>nil then package^.updateLists(wordsInEditor)
                    else wordsInEditor.clear;
    for i:=0 to editor.lines.count-1 do
      if i=caret.y-1 then collectIdentifiers(editor.lines[i],wordsInEditor,caret.x)
                     else collectIdentifiers(editor.lines[i],wordsInEditor,-1);
  end;

CONSTRUCTOR T_completionLogic.create;
  begin
    editor:=nil;
    package:=nil;
    lastWordsCaret:=maxLongint;
    wordsInEditor.create;
    SynCompletion:=TSynCompletion.create(nil);
    SynCompletion.OnCodeCompletion:=@SynCompletionCodeCompletion;
    SynCompletion.OnExecute       :=@SynCompletionExecute;
    SynCompletion.OnSearchPosition:=@SynCompletionSearchPosition;
  end;

DESTRUCTOR T_completionLogic.destroy;
  begin
    wordsInEditor.destroy;
    SynCompletion.destroy;
  end;

PROCEDURE T_completionLogic.assignEditor(CONST edit:TSynEdit; CONST pack:P_package);
  begin
    editor:=edit;
    package:=pack;
    wordsInEditor.clear;
    SynCompletion.editor:=editor;
  end;

PROCEDURE T_completionLogic.SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
  begin
    value:=copy(sourceValue,1,LastDelimiter('.',sourceValue)-1)+value;
    wordsInEditor.clear;
  end;

PROCEDURE T_completionLogic.SynCompletionExecute(Sender: TObject);
  VAR s:string;
      w:string;
  begin
    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    for w in wordsInEditor.values do if (s='') or (pos(s,w)=1) then SynCompletion.ItemList.add(w);
  end;

PROCEDURE T_completionLogic.SynCompletionSearchPosition(VAR APosition: integer);
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

end.
