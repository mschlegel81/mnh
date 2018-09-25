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
     codeAssistance;

TYPE
T_completionLogic=object
  private
    wordsInEditor:T_setOfString;
    editor:TSynEdit;
    SynCompletion:TSynCompletion;
    completionStart:longint;
    assistanceResponse:P_codeAssistanceResponse;
    PROCEDURE ensureWordsInEditorForCompletion;
  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE assignEditor(CONST edit:TSynEdit; CONST ad:P_codeAssistanceResponse);
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
      m :T_modifier;
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
    for tt in T_tokenType do if isIdentifier(C_tokenInfo[tt].defaultId,false) then
      intrinsicRulesForCompletion.put(replaceAll(C_tokenInfo[tt].defaultId,'.',''))
    else if (copy(C_tokenInfo[tt].defaultId,1,1)='.') and isIdentifier(copy(C_tokenInfo[tt].defaultId,2,1000),false) then
      intrinsicRulesForCompletion.put(C_tokenInfo[tt].defaultId);
    for tc in T_typeCheck do
      intrinsicRulesForCompletion.put(C_typeCheckInfo[tc].name);
    for m in T_modifier do
      intrinsicRulesForCompletion.put(C_modifierInfo[m].name);
    for i:=low(C_specialWordInfo) to high(C_specialWordInfo) do
      intrinsicRulesForCompletion.put(C_specialWordInfo[i].txt);

    intrinsicRulesForCompletion.put(ATTRIBUTE_PREFIX+SUPPRESS_UNUSED_PARAMETER_WARNING_ATTRIBUTE);
    intrinsicRulesForCompletion.put(ATTRIBUTE_PREFIX+SUPPRESS_UNUSED_WARNING_ATTRIBUTE);
    intrinsicRulesForCompletion.put(ATTRIBUTE_PREFIX+ALLOW_SIDE_EFFECT_ATTRIBUTE+'='+ALLOW_NO_SIDE_EFFECTS_ATTRIBUTE_VALUE);
    intrinsicRulesForCompletion.put(ATTRIBUTE_PREFIX+ALLOW_SIDE_EFFECT_ATTRIBUTE+'='+ALLOW_ALL_SIDE_EFFECTS_ATTRIBUTE_VALUE);
    intrinsicRulesForCompletion.put(ATTRIBUTE_PREFIX+EXECUTE_AFTER_ATTRIBUTE);
    intrinsicRulesForCompletion.put(ATTRIBUTE_PREFIX+OVERRIDE_ATTRIBUTE);

    intrinsicRulesForCompletion.put('?');
    intrinsicRulesForCompletion_ready:=true;
  end;

PROCEDURE T_completionLogic.ensureWordsInEditorForCompletion;
  VAR caret:TPoint;
      isUseClause:boolean;
  PROCEDURE collectAllIdentifiers;
    VAR i:longint;
    begin
      for i:=0 to editor.lines.count-1 do
        if i=caret.y-1 then collectIdentifiers(editor.lines[i],wordsInEditor,caret.x)
                       else collectIdentifiers(editor.lines[i],wordsInEditor,-1);
    end;

  begin
    caret:=editor.CaretXY;
    wordsInEditor.clear;
    if assistanceResponse<>nil then begin
      //Completion for assistant...
      isUseClause:=(pos(C_tokenInfo[tt_use    ].defaultId,editor.lines[caret.y-1])>0)
                or (pos(C_tokenInfo[tt_include].defaultId,editor.lines[caret.y-1])>0);
      if isUseClause
      then wordsInEditor.put(assistanceResponse^.getImportablePackages)
      else begin
        initIntrinsicRuleList;
        wordsInEditor.put(intrinsicRulesForCompletion);
        if not(assistanceResponse^.updateCompletionList(wordsInEditor,caret.y,caret.x))
        then collectAllIdentifiers;
      end;
    end else collectAllIdentifiers;
  end;

CONSTRUCTOR T_completionLogic.create;
  begin
    editor:=nil;
    assistanceResponse:=nil;
    wordsInEditor.create;
    SynCompletion:=TSynCompletion.create(nil);
    SynCompletion.OnCodeCompletion:=@SynCompletionCodeCompletion;
    SynCompletion.OnExecute       :=@SynCompletionExecute;
    SynCompletion.OnSearchPosition:=@SynCompletionSearchPosition;
    SynCompletion.EndOfTokenChr:='()[],{}+-*/&^:<>=';
  end;

DESTRUCTOR T_completionLogic.destroy;
  begin
    wordsInEditor.destroy;
    SynCompletion.destroy;
  end;

PROCEDURE T_completionLogic.assignEditor(CONST edit:TSynEdit; CONST ad:P_codeAssistanceResponse);
  begin
    editor:=edit;
    assistanceResponse:=ad;
    wordsInEditor.clear;
    SynCompletion.editor:=editor;
  end;

PROCEDURE T_completionLogic.SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
  CONST delimiters:set of char=['(',')','[',']',',','{','}','+','-','*','/','&','^',':','?','<','>','=','@','.',' '];
  VAR i:longint;
  begin
    {$ifdef debugMode}
    writeln(stdErr,'        DEBUG: SynCompletionCodeCompletion value: ',value);
    writeln(stdErr,'        DEBUG:                       sourceValue: ',sourceValue);
    writeln(stdErr,'        DEBUG:                       sourceStart: ',SourceStart.x);
    {$endif}
    if value='begin' then begin
      value:=value+C_lineBreakChar+StringOfChar(' ',SourceStart.x-1)+'end';
      exit;
    end;
    if (length(value)>=1) and (value[1] in delimiters) then begin
      for i:=length(sourceValue) downto 1 do
        if copy(sourceValue,i,length(sourceValue)-i+1)=
           copy(value      ,1,length(sourceValue)-i+1) then begin
          value:=copy(sourceValue,1,i-1)+value;
          exit;
        end;
    end;
  end;

PROCEDURE T_completionLogic.SynCompletionExecute(Sender: TObject);
  VAR s:string;
      w:string;
      i:longint;
  begin
    s:=SynCompletion.CurrentString;
    i:=LastDelimiter('.',s);
    if i>1 then begin
      s:=copy(s,i,length(s));
      {$ifdef debugMode}
      writeln(stdErr,'        DEBUG: Last delimiter is @',i,'; currentString: ',SynCompletion.CurrentString,' -> ',s);
      {$endif}
      completionStart:=i;
      SynCompletion.CurrentString:=s;
    end else begin
      completionStart:=1;
      {$ifdef debugMode}
      writeln(stdErr,'        DEBUG: Completion start reset "',s,'"')
      {$endif}
    end;

    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    for w in getListOfSimilarWords(s,wordsInEditor.values,32) do SynCompletion.ItemList.add(w);
  end;

PROCEDURE T_completionLogic.SynCompletionSearchPosition(VAR APosition: integer);
  VAR s:string;
      w:string;
  begin
    s:=SynCompletion.CurrentString;

    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    for w in getListOfSimilarWords(s,wordsInEditor.values,32) do  SynCompletion.ItemList.add(w);
    if SynCompletion.ItemList.count>0 then APosition:=0 else APosition:=-1;
  end;

FINALIZATION
  if intrinsicRulesForCompletion_ready then intrinsicRulesForCompletion.destroy;

end.
