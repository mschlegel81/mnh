UNIT mnhCompletion;
INTERFACE
USES Classes, sysutils, LCLType, types,
     //my utilities:
     myStringUtil, myGenerics,
     //GUI: SynEdit
     SynEdit, SynCompletion,
     mnh_constants,
     litVar,
     funcs,
     codeAssistance;
CONST
  COMPLETION_LIST_TARGET_SIZE=100;
TYPE
T_completionLogic=object
  private
    wordsInEditor:T_setOfString;
    editor:TSynEdit;
    SynCompletion:TSynCompletion;
    completionStart:longint;
    assistanceResponse:P_codeAssistanceResponse;
    quickEdit:boolean;
    PROCEDURE ensureWordsInEditorForCompletion;
  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE assignEditor(CONST edit:TSynEdit; CONST ad:P_codeAssistanceResponse; CONST isQuickEdit:boolean=false);
    PROCEDURE SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    FUNCTION fillFilteredItems(CONST part:string):longint;
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
    for id in funcs.intrinsicRuleMap.keySet do begin
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
    for tt in T_tokenType do if isIdentifier(C_tokenDefaultId[tt],false) then
      intrinsicRulesForCompletion.put(replaceAll(C_tokenDefaultId[tt],'.',''))
    else if (copy(C_tokenDefaultId[tt],1,1)='.') and isIdentifier(copy(C_tokenDefaultId[tt],2,1000),false) then
      intrinsicRulesForCompletion.put(C_tokenDefaultId[tt]);
    for tc in T_typeCheck do
      intrinsicRulesForCompletion.put(C_typeCheckInfo[tc].name);
    for m in T_modifier do
      intrinsicRulesForCompletion.put(C_modifierInfo[m].name);
    for i:=low(C_specialWordInfo) to high(C_specialWordInfo) do
      intrinsicRulesForCompletion.put(C_specialWordInfo[i].txt);

    intrinsicRulesForCompletion.put(ATTRIBUTE_PREFIX+SUPPRESS_UNUSED_PARAMETER_WARNING_ATTRIBUTE);
    intrinsicRulesForCompletion.put(ATTRIBUTE_PREFIX+SUPPRESS_UNUSED_WARNING_ATTRIBUTE);
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
      isUseClause:=(pos(C_tokenDefaultId[tt_use    ],editor.lines[caret.y-1])>0)
                or (pos(C_tokenDefaultId[tt_include],editor.lines[caret.y-1])>0);
      if isUseClause
      then wordsInEditor.put(assistanceResponse^.getImportablePackages)
      else begin
        initIntrinsicRuleList;
        wordsInEditor.put(intrinsicRulesForCompletion);
        if not(assistanceResponse^.updateCompletionList(wordsInEditor,caret.y,caret.x))
        then collectAllIdentifiers;
      end;
    end else if quickEdit then begin
      initIntrinsicRuleList;
      wordsInEditor.put(intrinsicRulesForCompletion);
    end else collectAllIdentifiers;
  end;

CONSTRUCTOR T_completionLogic.create;
  begin
    editor:=nil;
    assistanceResponse:=nil;
    quickEdit:=false;
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

PROCEDURE T_completionLogic.assignEditor(CONST edit:TSynEdit; CONST ad:P_codeAssistanceResponse; CONST isQuickEdit:boolean=false);
  begin
    editor:=edit;
    assistanceResponse:=ad;
    wordsInEditor.clear;
    SynCompletion.editor:=editor;
    quickEdit:=isQuickEdit;
  end;

CONST delimiters:set of char=['(',')','[',']',',','{','}','+','-','*','/','&','^',':','?','<','>','=','@','.',' '];

PROCEDURE T_completionLogic.SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
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

FUNCTION T_completionLogic.fillFilteredItems(CONST part:string):longint;
  FUNCTION hasExactPrefix(CONST txt:string):boolean;
    begin
      result:=startsWith(txt,part);
    end;

  FUNCTION hasCaseInsensitivePrefix(CONST txt:string):boolean;
    begin
      result:=startsWith(uppercase(txt),uppercase(part));
    end;
  VAR allWords,
      words:T_arrayOfString;
      w    :string;
      k    :longint=0;
  begin
    ensureWordsInEditorForCompletion;
    allWords:=wordsInEditor.values;
    setLength(words,length(allWords));
    for w in allWords do if hasExactPrefix(w) then begin
      words[k]:=w; inc(k);
    end;
    if k>0 //if we have at least one exact match, then we are done here
    then begin
      setLength(words,k);
      sort(words);
    end else begin
      setLength(words,k+length(allWords));
      for w in allWords do if hasCaseInsensitivePrefix(w) then begin
        words[k]:=w; inc(k);
      end;
      setLength(words,k);
      if k<COMPLETION_LIST_TARGET_SIZE
      then append(words,getListOfSimilarWords(part,allWords,COMPLETION_LIST_TARGET_SIZE-k,false));
      sortUnique(words);
    end;

    SynCompletion.ItemList.clear;
    for w in words do SynCompletion.ItemList.add(w);
    if SynCompletion.ItemList.count>0 then result:=0 else result:=-1;

    //cleanup
    setLength(allWords,0);
    setLength(words   ,0);
  end;

PROCEDURE T_completionLogic.SynCompletionExecute(Sender: TObject);
  VAR s:string;
      i:longint;
  FUNCTION indexOfLastDelimiter:longint;
    VAR k:longint;
    begin
      result:=0;
      for k:=1 to length(s) do if s[k] in delimiters then result:=k;
    end;

  begin
    s:=SynCompletion.CurrentString;
    i:=indexOfLastDelimiter;
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
    fillFilteredItems(s);
  end;

PROCEDURE T_completionLogic.SynCompletionSearchPosition(VAR APosition: integer);
  VAR s:string;
  begin
    s:=SynCompletion.CurrentString;
    APosition:=fillFilteredItems(s);
  end;

FINALIZATION
  if intrinsicRulesForCompletion_ready then intrinsicRulesForCompletion.destroy;

end.
