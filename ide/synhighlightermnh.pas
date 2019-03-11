UNIT SynHighlighterMnh;
INTERFACE

USES
  sysutils, Classes, FileUtil, Controls, Graphics,
  SynEditTypes, SynEditHighlighter,
  myGenerics,myStringUtil,
  mnh_constants,
  mnh_messages,
  funcs,
  packages,
  codeAssistance;

TYPE
  T_tokenKind = (
    tkComment,
    tkDocComment,
    tkSpecialComment,
    tkDefault,
    tkDollarIdentifier,
    tkUserRule,
    tkBultinRule,
    tkSpecialRule,
    tkLocalVar,
    tkOperator,
    tkNonStringLiteral,
    tkString,
    tkModifier,
    tkNull,
    tkError,
    tkWarning,
    tkNote,
    tkTimingNote,
    tkHighlightedItem);
  T_tokenSubKind =(skNormal,skWarn,skError);
  T_mnhSynFlavour=(msf_input,msf_output,msf_help,msf_debug);

CONST tokenKindByPrefix:array [0..6] of record marker:string[3]; tokenkind:T_tokenKind; end=(
         (marker:''            ; tokenkind:tkDefault),
         (marker:ECHO_MARKER   ; tokenkind:tkDefault),
         (marker:NOTE_MARKER   ; tokenkind:tkNote),
         (marker:ERROR_MARKER  ; tokenkind:tkError),
         (marker:WARNING_MARKER; tokenkind:tkWarning),
         (marker:TIMING_MARKER ; tokenkind:tkTimingNote),
         (marker:TIMING_MARKER2; tokenkind:tkTimingNote));
     SPECIAL_LINE_CASE_ECHO=1;
     SPECIAL_LINE_CASE_TM2 =6;

TYPE
  TSynMnhSyn = class(TSynCustomHighlighter)
  private
    //Initialized only
    flavour :T_mnhSynFlavour;
    styleTable: array[T_tokenKind,T_tokenSubKind] of TSynHighlighterAttributes;
    fLine: PChar;
    blobEnder:char;
    run: longint;
    fTokenPos: integer;
    fTokenId: T_tokenKind;
    fTokenSubId: T_tokenSubKind;
    fLineNumber: integer;
    markedWord:string;

  protected
    FUNCTION GetIdentChars: TSynIdentChars; override;
  public
    class FUNCTION GetLanguageName: ansistring; override;
  public
    highlightingData:T_highlightingData;
    CONSTRUCTOR create(AOwner: TComponent; CONST flav:T_mnhSynFlavour); reintroduce;
    DESTRUCTOR destroy; override;
    {$WARN 5024 OFF}
    FUNCTION GetDefaultAttribute(index: integer): TSynHighlighterAttributes; override;
    FUNCTION GetEol: boolean; override;
    FUNCTION getRange: pointer; override;
    FUNCTION getToken: ansistring; override;
    PROCEDURE GetTokenEx(OUT tokenStart: PChar; OUT tokenLength: integer); override;
    FUNCTION GetTokenAttribute: TSynHighlighterAttributes; override;
    FUNCTION GetTokenKind: integer; override;
    FUNCTION GetTokenPos: integer; override;
    PROCEDURE next; override;
    PROCEDURE ResetRange; override;
    PROCEDURE setRange(value: pointer); override;
    PROCEDURE SetLine(CONST newValue: ansistring; LineNumber: integer); override;
    FUNCTION setMarkedWord(CONST s:ansistring):boolean;
    FUNCTION getAttributeForKind(CONST kind:T_tokenKind):TSynHighlighterAttributes;
  end;

PROCEDURE initLists;
IMPLEMENTATION
VAR listsAreInitialized:boolean=false;
    tokenTypeMap:specialize G_stringKeyMap<T_tokenKind>;
    builtinRules:T_setOfString;

CONSTRUCTOR TSynMnhSyn.create(AOwner: TComponent; CONST flav:T_mnhSynFlavour);
  VAR t:T_tokenKind;
      s:T_tokenSubKind;
  begin
    inherited create(AOwner);
    highlightingData.create;
    flavour:=flav;
    for s:=low(T_tokenSubKind) to high(T_tokenSubKind) do begin
      styleTable[tkComment         ,s]:=TSynHighlighterAttributes.create('Comment');
      styleTable[tkDocComment      ,s]:=TSynHighlighterAttributes.create('DocComment');
      styleTable[tkSpecialComment  ,s]:=TSynHighlighterAttributes.create('SpecialComment');
      styleTable[tkDefault         ,s]:=TSynHighlighterAttributes.create('Default');
      styleTable[tkDollarIdentifier,s]:=TSynHighlighterAttributes.create('DollarIdentifier');
      styleTable[tkUserRule        ,s]:=TSynHighlighterAttributes.create('UserRule');
      styleTable[tkBultinRule      ,s]:=TSynHighlighterAttributes.create('BultinRule');
      styleTable[tkSpecialRule     ,s]:=TSynHighlighterAttributes.create('SpecialRule');
      styleTable[tkLocalVar        ,s]:=TSynHighlighterAttributes.create('LocalVar');
      styleTable[tkOperator        ,s]:=TSynHighlighterAttributes.create('Operator');
      styleTable[tkNonStringLiteral,s]:=TSynHighlighterAttributes.create('NonStringLiteral');
      styleTable[tkString          ,s]:=TSynHighlighterAttributes.create('String');
      styleTable[tkModifier        ,s]:=TSynHighlighterAttributes.create('Modifier');
      styleTable[tkNull            ,s]:=TSynHighlighterAttributes.create('Null');
      styleTable[tkError           ,s]:=TSynHighlighterAttributes.create('Error');
      styleTable[tkWarning         ,s]:=TSynHighlighterAttributes.create('Warn');
      styleTable[tkNote            ,s]:=TSynHighlighterAttributes.create('Note');
      styleTable[tkTimingNote      ,s]:=TSynHighlighterAttributes.create('Time');
      styleTable[tkHighlightedItem ,s]:=TSynHighlighterAttributes.create('Highlighted');

      styleTable[tkComment         ,s].style:=[fsItalic];
      styleTable[tkDocComment      ,s].style:=[fsItalic,fsBold];
      styleTable[tkSpecialComment  ,s].style:=[fsItalic,fsBold,fsUnderline];
      styleTable[tkDollarIdentifier,s].style:=[fsItalic];
      styleTable[tkBultinRule      ,s].style:=[fsBold];
      styleTable[tkSpecialRule     ,s].style:=[fsBold];
      styleTable[tkOperator        ,s].style:=[fsBold];
      styleTable[tkModifier        ,s].style:=[fsBold];
      styleTable[tkError           ,s].style:=[fsBold];
      styleTable[tkWarning         ,s].style:=[fsBold];
      styleTable[tkNote            ,s].style:=[fsBold];
      styleTable[tkTimingNote      ,s].style:=[fsBold];
      styleTable[tkHighlightedItem ,s].style:=[fsBold];

      styleTable[tkComment         ,s].foreground:=$00999999;
      styleTable[tkDocComment      ,s].foreground:=$00999999;
      styleTable[tkSpecialComment  ,s].foreground:=$00999999;
      styleTable[tkDefault         ,s].foreground:=$00000000;
      styleTable[tkDollarIdentifier,s].foreground:=$00000000;
      styleTable[tkUserRule        ,s].foreground:=$00FF0000;
      styleTable[tkLocalVar        ,s].foreground:=$00AA0000;
      styleTable[tkBultinRule      ,s].foreground:=$00FF0000;
      styleTable[tkSpecialRule     ,s].foreground:=$00FF0000;
      styleTable[tkOperator        ,s].foreground:=$00880000;
      styleTable[tkNonStringLiteral,s].foreground:=$000000ff;
      styleTable[tkString          ,s].foreground:=$00008800;
      styleTable[tkModifier        ,s].foreground:=$000088ff;
      styleTable[tkNull            ,s].foreground:=$00000000;
      styleTable[tkError           ,s].foreground:=$000000ff;
      styleTable[tkError           ,s].background:=$0000FFFF;
      styleTable[tkWarning         ,s].foreground:=$000000ff;
      styleTable[tkTimingNote      ,s].background:=$00EEEEEE;
      styleTable[tkHighlightedItem ,s].background:=$00AAFF00;
    end;
    styleTable[tkTimingNote,skWarn].style:=[];
    styleTable[tkTimingNote,skWarn].background:=$00F6F6F6;
    for t:=low(T_tokenKind) to high(T_tokenKind) do begin
      if flavour=msf_debug
      then styleTable[t,skWarn].background:=$00EEEEEE
      else if t<>tkTimingNote then begin
        styleTable[t,skWarn].FrameColor:=clRed;
        styleTable[t,skWarn].FrameStyle:=slsWaved;
        styleTable[t,skWarn].FrameEdges:=sfeBottom;
      end;
      styleTable[t,skError].style:=styleTable[t,skError].style+[fsBold];
      styleTable[t,skError].background:=$0000FFFF;
      styleTable[t,skError].FrameColor:=clRed;
      styleTable[t,skError].FrameStyle:=slsWaved;
      styleTable[t,skError].FrameEdges:=sfeBottom;
    end;
    markedWord:='';
  end; { Create }

DESTRUCTOR TSynMnhSyn.destroy;
  VAR t: T_tokenKind;
      s: T_tokenSubKind;
  begin
    for t:=low(T_tokenKind)    to high(T_tokenKind) do
    for s:=low(T_tokenSubKind) to high(T_tokenSubKind) do styleTable[t,s].destroy;
    highlightingData.destroy;
    inherited destroy;
  end; { Destroy }

FUNCTION TSynMnhSyn.GetDefaultAttribute(index: integer): TSynHighlighterAttributes;
  begin
    result := styleTable [tkDefault,skNormal];
  end;

PROCEDURE TSynMnhSyn.SetLine(CONST newValue: ansistring; LineNumber: integer);
  begin
    inherited;
    fLine := PChar(newValue);
    run := 0;
    fLineNumber := LineNumber;
    ResetRange;
    next;
  end;

FUNCTION TSynMnhSyn.setMarkedWord(CONST s:ansistring):boolean;
  begin
    result:=(s<>markedWord);
    markedWord:=s;
  end;

FUNCTION TSynMnhSyn.getAttributeForKind(CONST kind:T_tokenKind):TSynHighlighterAttributes;
  begin
    result:=styleTable[kind,skNormal];
  end;

PROCEDURE TSynMnhSyn.next;
  CONST RUN_LIMIT=10000;
  VAR localId: shortstring;
      i,j: longint;
      specialLineCase:byte=0;

  FUNCTION continuesWith(CONST part:shortstring; CONST offset:longint):boolean;
    VAR k:longint;
    begin
      result:=length(part)>0;
      for k:=1 to length(part) do if fLine[k-1+offset]<>part[k] then exit(false);
    end;

  FUNCTION startsWith(CONST prefix:shortstring):boolean;
    begin
      result:=continuesWith(prefix,0);
    end;

  PROCEDURE handleComment(endedBy:T_charSet; CONST isHashComment:boolean);
    begin
      case fLine[run] of
        SPECIAL_COMMENT_BLOB_BEGIN_INFIX: if isHashComment
        then fTokenId:=tkComment
        else begin
          fTokenId:=tkString;
          blobEnder:=fLine[run+1];
          if blobEnder=#0 then begin
            blobEnder:='''';
            inc(run);
          end else inc(run,2);
          exit;
        end;
        DOC_COMMENT_INFIX:       fTokenId:=tkDocComment;
        else                     fTokenId:=tkComment;
      end;
      while not(fLine[run] in endedBy) do inc(run);
      if fLine[run]<>#0 then inc(run);
    end;

  begin
    fTokenId := tkDefault;
    fTokenSubId:=skNormal;
    fTokenPos := run;
    if run>=RUN_LIMIT then begin
      fTokenId:=tkNull;
      exit;
    end;

    if (run = 0) and (flavour in [msf_output,msf_help]) then begin
      i:=-1;
      for j:=1 to length(tokenKindByPrefix)-1 do if startsWith(tokenKindByPrefix[j].marker) then specialLineCase:=j;
      if i>=0 then run:=i+1;
      fTokenId:=tokenKindByPrefix[specialLineCase].tokenkind;
      if specialLineCase=SPECIAL_LINE_CASE_TM2 then fTokenSubId:=skWarn;
      if (specialLineCase=SPECIAL_LINE_CASE_ECHO) then begin
        if (flavour=msf_output) then begin
          while (run<RUN_LIMIT) and not(fLine[run] in [#0,'>']) do inc(run);
          if fLine[run]='>' then inc(run);
        end else inc(run,3);
      end else while (run<RUN_LIMIT) and (fLine[run]<>#0) do inc(run);
      if (run>0) and not((flavour=msf_help) and (specialLineCase=SPECIAL_LINE_CASE_ECHO)) then exit;
    end else while (run<RUN_LIMIT) and (fLine[run]=' ') do inc(run);
    if (flavour<>msf_debug) and (blobEnder<>#0) then begin
      if fLine[run]=#0 then begin
        fTokenId := tkNull;
        exit;
      end;
      while (fLine[run]<>#0) and (fLine[run]<>blobEnder) do inc(run);
      if fLine[run]=blobEnder then begin
        blobEnder:=#0;
        inc(run);
      end;
      fTokenId:=tkString;
    end else case fLine [run] of
      #0: fTokenId := tkNull;
      '{': begin
             inc(run);
             fTokenId := tkDefault;
           end;
      '}': begin
             inc(run);
             fTokenId := tkDefault;
           end;
      '0'..'9': begin
        while fLine [run] in ['0'..'9', '.'] do inc(run);
        if fLine[run] in ['E','e'] then begin
          inc(run);
          if fLine[run] in ['+','-'] then inc(run);
          while fLine[run] in ['0'..'9'] do inc(run);
        end;
        fTokenId := tkNonStringLiteral;
      end;
      #194: begin
        fTokenId   :=tkError;
        fTokenSubId:=skError;
        inc(run);
        if (fLine[run]=#167) and (flavour=msf_debug) then blobEnder:=#194;
        if fLine[run]<>#0 then inc(run);
      end;
      '$': begin
        inc(run);
        while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do inc(run);
        fTokenId := tkDollarIdentifier;
      end;
      'a'..'z', 'A'..'Z': begin
        localId := fLine [run];
        inc(run);
        while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do begin
          localId := localId+fLine [run];
          inc(run);
        end;
        if localId=markedWord then fTokenId:=tkHighlightedItem
        else if tokenTypeMap.containsKey(localId,fTokenId) then begin end
        else if highlightingData.isUserRule(localId)                then fTokenId:=tkUserRule
        else if highlightingData.isLocalId(localId,lineIndex+1,run) then fTokenId:=tkLocalVar
        else if builtinRules.contains(localId) then fTokenId:=tkBultinRule
        else fTokenId := tkDefault;
      end;
      '@': if fTokenPos=0 then begin
             fTokenId:=tkSpecialComment;
             while fLine[run]<>#0 do inc(run);
           end else begin
             inc(run);
             fTokenId := tkOperator;
           end;
      '|', '^', '?', '+', '&', '*', '.': begin
        inc(run);
        fTokenId := tkOperator;
      end;
      '=','<','>','-': begin
        while fLine [run] in ['=', '<', '>' ,'-'] do inc(run);
        fTokenId := tkOperator;
      end;
      '/': begin
        inc(run);
        if fLine [run] = '/' then begin
          inc(run);
          handleComment([#0],false);
        end
        else fTokenId := tkOperator;
      end;
      ':': begin
        inc(run);
        case fLine [run] of
          'm','c', 'b', 'e', 'i', 'l', 'n', 's', 'r', 'k', 'f': begin
            localId := ':';
            i:=run;
            while fLine [i] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do begin
              localId := localId+fLine [i];
              inc(i);
            end;
            fTokenId := tkDefault;
            if fTokenId<>tkOperator then begin
              fTokenId := tkOperator;
            end;
          end;
          else fTokenId := tkOperator;
        end;
      end;
      '"': begin
        inc(run);
        while (fLine [run]<>#0) and ((fLine [run]<>'"') or (fLine [run-1] = '\') and (fLine [run-2]<>'\')) do inc(run);
        if (fLine [run] = '"') then inc(run);
        fTokenId := tkString;
      end;
      '''': begin
        inc(run);
        while (fLine [run]<>#0) and (fLine [run]<>'''') do inc(run);
        if (fLine [run] = '''') then inc(run);
        fTokenId := tkString;
      end;
      '#': begin
        inc(run);
        if fLine[run] in ['0'..'9'] then begin
          while (fLine[run] in ['0'..'9']) do inc(run);
          fTokenId:=tkString;
        end else handleComment(['#',#0],true);
      end
      else begin
        fTokenId := tkDefault;
        inc(run);
      end;
    end;
    if (flavour=msf_input) then case highlightingData.isErrorLocation(fLineNumber,fTokenPos,run) of
      2: fTokenSubId:=skError;
      1: fTokenSubId:=skWarn;
    end;
    if (flavour=msf_debug) and (blobEnder=#0) then begin
      fTokenSubId:=skWarn;
    end;
  end;

FUNCTION TSynMnhSyn.GetEol: boolean;
  begin
    result := fTokenId = tkNull;
  end;

FUNCTION TSynMnhSyn.getRange: pointer;
  begin
    result:=nil;
    move(blobEnder,result,sizeOf(blobEnder));
  end;

FUNCTION TSynMnhSyn.getToken: ansistring;
  VAR len: longint;
  begin
    len := run-fTokenPos;
    result := '';
    setString(result, (fLine+fTokenPos), len);
  end;

PROCEDURE TSynMnhSyn.GetTokenEx(OUT tokenStart: PChar; OUT tokenLength: integer);
  begin
    tokenLength := run-fTokenPos;
    tokenStart := fLine+fTokenPos;
  end;

FUNCTION TSynMnhSyn.GetTokenAttribute: TSynHighlighterAttributes;
  begin
    result := styleTable [fTokenId,fTokenSubId];
    if (flavour<>msf_debug) and (blobEnder<>#0) and (fTokenId<>tkSpecialComment) then result:=styleTable[tkString,skNormal];
  end;

FUNCTION TSynMnhSyn.GetTokenKind: integer;
  begin
    result := ord(fTokenId);
  end;

FUNCTION TSynMnhSyn.GetTokenPos: integer;
  begin
    result := fTokenPos;
  end;

PROCEDURE TSynMnhSyn.ResetRange;
  begin
    if lineIndex=0 then blobEnder:=#0;
  end;

PROCEDURE TSynMnhSyn.setRange(value: pointer);
  begin
    move(value,blobEnder,sizeOf(blobEnder));
  end;

class FUNCTION TSynMnhSyn.GetLanguageName: ansistring;
  begin
    result := 'MNH';
  end;

FUNCTION TSynMnhSyn.GetIdentChars: TSynIdentChars;
  begin
    result := IDENTIFIER_CHARS;
  end;

PROCEDURE initLists;
  PROCEDURE put(CONST wc:T_reservedWordClass; CONST txt:string);
    CONST tt:array[T_reservedWordClass] of T_tokenKind=(
                   {rwc_not_reserved    } tkBultinRule,
                   {rwc_specialLiteral  } tkNonStringLiteral,
                   {rwc_specialConstruct} tkSpecialRule,
                   {rwc_operator        } tkOperator,
                   {rwc_typeCheck       } tkOperator,
                   {rwc_modifier        } tkModifier);
    begin
      if (length(txt)<=0) then exit;
      if (txt[1]='.') then begin
        put(wc,copy(txt,2,length(txt)-1));
        exit;
      end;
      if not(isIdentifier(txt,true)) then exit;
      tokenTypeMap.put(txt,tt[wc]);
    end;

  VAR tt:T_tokenType;
      tc:T_typeCheck;
      md:T_modifier;
      i:longint;
  begin
    if listsAreInitialized then exit;
    tokenTypeMap.create();
    for tt:=low(T_tokenType) to high(T_tokenType) do with C_tokenInfo[tt] do put(reservedWordClass,defaultId);
    for i:=0 to high(C_specialWordInfo) do with C_specialWordInfo[i] do put(reservedWordClass,txt);
    for tc in T_typeCheck do put(rwc_type,C_typeCheckInfo[tc].name);
    for md in T_modifier do put(rwc_modifier,C_modifierInfo[md].name);
    builtinRules.create;
    builtinRules.put(intrinsicRuleMap.keySet);
    listsAreInitialized:=true;
  end;

FINALIZATION
  if listsAreInitialized then begin
    tokenTypeMap.destroy;
    builtinRules.destroy;
  end;
end.
