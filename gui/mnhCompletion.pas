UNIT mnhCompletion;
INTERFACE
USES Classes, sysutils, LCLType, types,
     //my utilities:
     myStringUtil, myGenerics,
     //GUI: SynEdit
     SynEdit, SynCompletion,
     mnh_constants,
     mnh_litVar,
     mnh_funcs,
     mnh_packages;

TYPE
T_completionLogic=object
  private
    wordsInEditor:T_setOfString;
    editor:TSynEdit;
    lastWordsCaret:longint;
    SynCompletion:TSynCompletion;
    relatedAssistant:P_codeAssistanceData;
    PROCEDURE ensureWordsInEditorForCompletion;
  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE assignEditor(CONST edit:TSynEdit; CONST ad:P_codeAssistanceData);
    PROCEDURE SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    PROCEDURE SynCompletionExecute(Sender: TObject);
    PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);
end;

IMPLEMENTATION
VAR intrinsicRulesForCompletion:T_setOfString;
    intrinsicRulesForCompletion_ready:boolean=false;
PROCEDURE initIntrinsicRuleList;
  VAR id:string;
      i:longint;
      tt:T_tokenType;
      tc:T_typeCheck;
  begin
    if intrinsicRulesForCompletion_ready then exit;
    intrinsicRulesForCompletion.create;
    for id in mnh_funcs.intrinsicRuleMap.keySet do begin
      if pos(ID_QUALIFY_CHARACTER,id)<=0 then begin
        intrinsicRulesForCompletion.put(id);
        intrinsicRulesForCompletion.put(ID_QUALIFY_CHARACTER+id);
      end else begin
        intrinsicRulesForCompletion.put(id);
        intrinsicRulesForCompletion.put(split(id,ID_QUALIFY_CHARACTER)[0]);
        intrinsicRulesForCompletion.put(split(id,ID_QUALIFY_CHARACTER)[1]);
        intrinsicRulesForCompletion.put(ID_QUALIFY_CHARACTER+split(id,ID_QUALIFY_CHARACTER)[0]);
        intrinsicRulesForCompletion.put(ID_QUALIFY_CHARACTER+split(id,ID_QUALIFY_CHARACTER)[1]);
      end;
    end;
    for tt in T_tokenType do if isIdentifier(C_tokenInfo[tt].defaultId,true) then
      intrinsicRulesForCompletion.put(replaceAll(C_tokenInfo[tt].defaultId,'.',''));
    for tc in T_typeCheck do
      intrinsicRulesForCompletion.put(C_typeCheckInfo[tc].name);
    for i:=low(C_specialWordInfo) to high(C_specialWordInfo) do
      intrinsicRulesForCompletion.put(C_specialWordInfo[i].txt);
    intrinsicRulesForCompletion_ready:=true;
  end;

PROCEDURE T_completionLogic.ensureWordsInEditorForCompletion;
  VAR caret:TPoint;
      i:longint;
      isUseClause:boolean;
  begin
    caret:=editor.CaretXY;
    if (wordsInEditor.size>0) and (lastWordsCaret=caret.y) then exit;
    isUseClause:=(pos(C_tokenInfo[tt_use    ].defaultId,editor.lines[caret.y-1])>0)
              or (pos(C_tokenInfo[tt_include].defaultId,editor.lines[caret.y-1])>0);
    lastWordsCaret:=caret.y;
    wordsInEditor.clear;

    if relatedAssistant<>nil then begin
      //Completion for assistant...
      if isUseClause then begin
        wordsInEditor.put(relatedAssistant^.getImportablePackages);
      end else begin
        initIntrinsicRuleList;
        wordsInEditor.put(intrinsicRulesForCompletion);
        relatedAssistant^.updateCompletionList(wordsInEditor);
        for i:=0 to editor.lines.count-1 do
          if i=caret.y-1 then collectIdentifiers(editor.lines[i],wordsInEditor,caret.x)
                         else collectIdentifiers(editor.lines[i],wordsInEditor,-1);
      end;
    end else begin
      for i:=0 to editor.lines.count-1 do
        if i=caret.y-1 then collectIdentifiers(editor.lines[i],wordsInEditor,caret.x)
                       else collectIdentifiers(editor.lines[i],wordsInEditor,-1);
    end;
  end;

CONSTRUCTOR T_completionLogic.create;
  begin
    editor:=nil;
    relatedAssistant:=nil;
    lastWordsCaret:=maxLongint;
    wordsInEditor.create;
    SynCompletion:=TSynCompletion.create(nil);
    SynCompletion.OnCodeCompletion:=@SynCompletionCodeCompletion;
    SynCompletion.OnExecute       :=@SynCompletionExecute;
    SynCompletion.OnSearchPosition:=@SynCompletionSearchPosition;
    SynCompletion.EndOfTokenChr:='()[],{}+-*/&^:?<>=@';
  end;

DESTRUCTOR T_completionLogic.destroy;
  begin
    wordsInEditor.destroy;
    SynCompletion.destroy;
  end;

PROCEDURE T_completionLogic.assignEditor(CONST edit:TSynEdit; CONST ad:P_codeAssistanceData);
  begin
    editor:=edit;
    relatedAssistant:=ad;
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

FINALIZATION
  if intrinsicRulesForCompletion_ready then intrinsicRulesForCompletion.destroy;

end.
