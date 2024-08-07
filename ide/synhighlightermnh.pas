UNIT SynHighlighterMnh;
INTERFACE

USES
  Classes, Controls, Graphics,
  SynEditTypes, SynEditHighlighter,
  myGenerics,myStringUtil,
  mnh_constants,
  mnh_messages,
  funcs,
  codeAssistance,
  tokenArray;

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

TYPE
  TAbstractSynMnhSyn = class(TSynCustomHighlighter)
  private
    styleTable: array[T_tokenKind,T_tokenSubKind] of TSynHighlighterAttributes;
    fLine: PChar;
    blobEnder:char;
    run: longint;
    fTokenPos: integer;
    fTokenId: T_tokenKind;
    fTokenSubId: T_tokenSubKind;
    fLineNumber: integer;
    firstInLine: boolean;

  protected
    FUNCTION GetIdentChars: TSynIdentChars; override;
  public
    class FUNCTION GetLanguageName: ansistring; override;
  public
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
    FUNCTION getAttributeForKind(CONST kind:T_tokenKind):TSynHighlighterAttributes;
    PROCEDURE handle_inline_if(CONST line:longint); virtual;
    PROCEDURE handle194; virtual;
    PROCEDURE handleId(CONST id:string; CONST line:longint; VAR col:longint); virtual;
    FUNCTION lineContinues(CONST head,full_part:string; CONST line:longint; VAR col:longint):boolean;
  end;

  TMnhInputSyn = class(TAbstractSynMnhSyn)
    private
      related:T_relatedTokens;
      markedWord:string;
    public
      highlightingData:T_highlightingData;
      CONSTRUCTOR create(AOwner: TComponent); reintroduce;
      DESTRUCTOR destroy; override;
      FUNCTION setMarkedWord(CONST s:ansistring):boolean;
      FUNCTION setCaretLocation(CONST caret:TPoint):boolean;
      PROCEDURE next; override;
      FUNCTION handleRelatedPosition(CONST line:longint):boolean;
      PROCEDURE handle_inline_if(CONST line:longint); override;
      PROCEDURE handleId(CONST id: string; CONST line: longint; VAR col: longint); override;
  end;

  TMnhOutputSyn = class(TAbstractSynMnhSyn)
    public
      CONSTRUCTOR create(AOwner: TComponent); reintroduce;
      DESTRUCTOR destroy; override;
      PROCEDURE next; override;
  end;

  TMnhDebugSyn = class(TAbstractSynMnhSyn)
    public
      CONSTRUCTOR create(AOwner: TComponent); reintroduce;
      DESTRUCTOR destroy; override;
      PROCEDURE next; override;
      PROCEDURE handle194; override;
  end;

PROCEDURE initLists;
IMPLEMENTATION
USES sysutils,
     funcs_ftp,
     func_queues,
     funcs_interpolators;
VAR listsAreInitialized:boolean=false;
    tokenTypeMap:specialize G_stringKeyMap<T_tokenKind>;
    builtinRules:T_setOfString;

FUNCTION TMnhInputSyn.setMarkedWord(CONST s: ansistring): boolean;
  begin
    result:=(s<>markedWord);
    markedWord:=s;
  end;

FUNCTION TMnhInputSyn.setCaretLocation(CONST caret: TPoint):boolean;
  VAR k:longint;
      before:T_relatedTokens;
  begin
    before:=related;
    related:=highlightingData.getRelatedLocations(caret.X,caret.Y);
    for k:=0 to related.count-1 do related.position[k].x-=1;
    result:=(before<>related);
  end;

CONSTRUCTOR TAbstractSynMnhSyn.create(AOwner: TComponent; CONST flav: T_mnhSynFlavour);
  VAR t:T_tokenKind;
      s:T_tokenSubKind;
  begin
    inherited create(AOwner);
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
      styleTable[tkError           ,s].style:=[fsBold,fsUnderline];
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
      styleTable[tkWarning         ,s].foreground:=$000000ff;
      styleTable[tkNote            ,s].foreground:=$00666666;
      styleTable[tkTimingNote      ,s].background:=$00EEEEEE;
      styleTable[tkHighlightedItem ,s].background:=$00AAFF00;
    end;
    styleTable[tkTimingNote,skWarn ].style:=[];
    styleTable[tkTimingNote,skWarn ].background:=$00F6F6F6;
    styleTable[tkTimingNote,skError].style:=[fsBold];
    styleTable[tkTimingNote,skError].background:=$00FFFFFF;
    for t:=low(T_tokenKind) to high(T_tokenKind) do begin
      if flav=msf_debug
      then styleTable[t,skWarn].background:=$00EEEEEE
      else if t<>tkTimingNote then begin
        styleTable[t,skWarn].FrameColor:=clRed;
        styleTable[t,skWarn].FrameStyle:=slsWaved;
        styleTable[t,skWarn].FrameEdges:=sfeBottom;

        styleTable[t,skError].style:=styleTable[t,skError].style+[fsBold];
        styleTable[t,skError].background:=$0000FFFF;
        styleTable[t,skError].FrameColor:=clRed;
        styleTable[t,skError].FrameStyle:=slsWaved;
        styleTable[t,skError].FrameEdges:=sfeBottom;
      end;
    end;
  end; { Create }

CONSTRUCTOR TMnhInputSyn.create(AOwner: TComponent);
  begin
    inherited create(AOwner,msf_input);
    highlightingData.create;
    markedWord:='';
    related.count:=0;
  end;

CONSTRUCTOR TMnhOutputSyn.create(AOwner: TComponent);
  begin
    inherited create(AOwner,msf_output);
  end;

CONSTRUCTOR TMnhDebugSyn.create(AOwner: TComponent);
  begin
    inherited create(AOwner,msf_debug)
  end;

DESTRUCTOR TAbstractSynMnhSyn.destroy;
  VAR t: T_tokenKind;
      s: T_tokenSubKind;
  begin
    for t:=low(T_tokenKind)    to high(T_tokenKind) do
    for s:=low(T_tokenSubKind) to high(T_tokenSubKind) do styleTable[t,s].destroy;
    inherited destroy;
  end;

DESTRUCTOR TMnhInputSyn.destroy;
  begin
    highlightingData.destroy;
    inherited destroy;
  end;

DESTRUCTOR TMnhOutputSyn.destroy;
  begin
    inherited destroy;
  end;

DESTRUCTOR TMnhDebugSyn.destroy;
  begin
    inherited destroy;
  end;

FUNCTION TAbstractSynMnhSyn.GetDefaultAttribute(index: integer): TSynHighlighterAttributes;
  begin
    result := styleTable [tkDefault,skNormal];
  end;

PROCEDURE TAbstractSynMnhSyn.SetLine(CONST newValue: ansistring; LineNumber: integer);
  begin
    inherited;
    fLine := PChar(newValue);
    run := 0;
    fLineNumber := LineNumber;
    ResetRange;
    next;
  end;

FUNCTION TAbstractSynMnhSyn.getAttributeForKind(CONST kind: T_tokenKind): TSynHighlighterAttributes;
  begin
    result:=styleTable[kind,skNormal];
  end;

PROCEDURE TAbstractSynMnhSyn.handle_inline_if(CONST line:longint);
  begin
    inc(run);
    fTokenId:=tkOperator;
  end;

PROCEDURE TAbstractSynMnhSyn.handle194;
  begin
    inc(run);
    if fLine[run] in [#178,#179] then begin
      fTokenId:=tkOperator;
      inc(run);
    end else begin
      fTokenId   :=tkError;
      fTokenSubId:=skError;
      if fLine[run]<>#0 then inc(run);
    end;
  end;

PROCEDURE TMnhDebugSyn.handle194;
  begin
    fTokenId   :=tkError;
    fTokenSubId:=skError;
    inc(run);
    if (fLine[run]=#167) then blobEnder:=#194;
    if fLine[run]<>#0 then inc(run);
  end;

FUNCTION TAbstractSynMnhSyn.lineContinues(CONST head,full_part:string; CONST line:longint; VAR col:longint):boolean;
  VAR offset:longint;
      i:longint;
  begin
    offset:=length(head)+1;
    result:=true;
    for i:=0 to length(full_part)-offset-1 do
      if fLine[col+i]<>full_part[offset+i] then exit(false);
    inc(col,length(full_part)-length(head));
  end;

PROCEDURE TAbstractSynMnhSyn.handleId(CONST id:string; CONST line:longint; VAR col:longint);
  begin
    if (id=C_tokenDefaultId[tt_do]) and lineContinues(id,DO_PARALLEL_TEXT,line,col) then fTokenId:=tkOperator else
    if not(tokenTypeMap.containsKey(id,fTokenId))
    then begin
      if builtinRules.contains(id)
      then fTokenId:=tkBultinRule
      else if (id=ALTERNATIVE_NOT_TEXT) then fTokenId:=tkOperator
      else fTokenId:=tkDefault;
    end;
  end;

FUNCTION TMnhInputSyn.handleRelatedPosition(CONST line:longint):boolean;
  VAR k:longint;
  begin
    for k:=0 to related.count-1 do
      if (related.position[k].y=line) and (related.position[k].x=fTokenPos)
      then begin
        fTokenId:=tkHighlightedItem;
        exit(true);
      end;
    result:=false;
  end;

PROCEDURE TMnhInputSyn.handle_inline_if(CONST line:longint);
  begin
    inherited;
    handleRelatedPosition(line);
  end;

PROCEDURE TMnhInputSyn.handleId(CONST id: string; CONST line: longint; VAR col: longint);
  begin
    if handleRelatedPosition(line) then exit;
    if id=markedWord then fTokenId:=tkHighlightedItem
    else if (id=C_tokenDefaultId[tt_do]) and lineContinues(id,DO_PARALLEL_TEXT,line,col) then fTokenId:=tkOperator
    else if tokenTypeMap.containsKey(id,fTokenId)    then begin end
    else if highlightingData.isUserRule(id)          then fTokenId:=tkUserRule
    else if highlightingData.isLocalId (id,line,col) then fTokenId:=tkLocalVar
    else if builtinRules    .contains  (id)          then fTokenId:=tkBultinRule
    else if (id=ALTERNATIVE_NOT_TEXT)                then fTokenId:=tkOperator
    else                                                  fTokenId:=tkDefault;
  end;

PROCEDURE TAbstractSynMnhSyn.next;
  VAR localId: shortstring;

  FUNCTION continuesWith(CONST part:shortstring; CONST offset:longint):boolean;
    VAR k:longint;
    begin
      result:=length(part)>0;
      for k:=1 to length(part) do if fLine[k-1+offset]<>part[k] then exit(false);
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

  PROCEDURE handleDoubleQuoteString;
    begin
      inc(run);
      while (fLine [run]<>#0) and ((fLine [run]<>'"') or (fLine [run-1] = '\') and (fLine [run-2]<>'\')) do inc(run);
      if (fLine [run] = '"') then inc(run);
      fTokenId := tkString;
      firstInLine:=false;
    end;

  PROCEDURE handleSingleQuoteString;
    begin
      inc(run);
      while (fLine [run]<>#0) and (fLine [run]<>'''') do inc(run);
      if (fLine [run] = '''') then inc(run);
      fTokenId := tkString;
      firstInLine:=false;
    end;

  begin
    case fLine [run] of
      #0: fTokenId := tkNull;
      '{': begin
             inc(run);
             fTokenId := tkDefault;
             firstInLine:=false;
           end;
      '}': begin
             inc(run);
             fTokenId := tkDefault;
             firstInLine:=false;
           end;
      '0'..'9': begin
        while fLine [run] in ['0'..'9', '.'] do inc(run);
        if fLine[run] in ['E','e'] then begin
          inc(run);
          if fLine[run] in ['+','-'] then inc(run);
          while fLine[run] in ['0'..'9'] do inc(run);
        end;
        fTokenId := tkNonStringLiteral;
        firstInLine:=false;
      end;
      #194: handle194;
      '$': begin
        inc(run);
        while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do inc(run);
        fTokenId := tkDollarIdentifier;
        firstInLine:=false;
      end;
      'a'..'e','g'..'z', 'A'..'Z','_': begin
        localId := fLine [run];
        inc(run);
        while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do begin
          localId := localId+fLine [run];
          inc(run);
        end;
        handleId(localId,lineIndex+1,run);
        firstInLine:=false;
      end;
      'f': begin
        localId := fLine [run];
        inc(run);
        case fLine[run] of
          '"': handleDoubleQuoteString;
          '''': handleSingleQuoteString;
          else begin
            while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do begin
              localId := localId+fLine [run];
              inc(run);
            end;
            handleId(localId,lineIndex+1,run);
          end;
        end;
        firstInLine:=false;
      end;
      '@': if firstInLine then begin
             fTokenId:=tkSpecialComment;
             while fLine[run]<>#0 do inc(run);
           end else begin
             inc(run);
             fTokenId := tkOperator;
           end;
      '?',':': begin
        handle_inline_if(lineIndex+1);
        firstInLine:=false;
      end;
      '=','<','>','-','|', '^', '+', '&', '*', '.': begin
        inc(run);
        while fLine [run] in ['=', '<', '>' ,'-','|', '^', '+', '&', '*', '.'] do inc(run);
        fTokenId := tkOperator;
        firstInLine:=false;
      end;
      '/': begin
        inc(run);
        if fLine [run] = '/' then begin
          inc(run);
          handleComment([#0],false);
        end
        else fTokenId := tkOperator;
        firstInLine:=false;
      end;
      '"': handleDoubleQuoteString;
      '''': handleSingleQuoteString;
      '#': begin
        inc(run);
        if fLine[run] in ['0'..'9'] then begin
          while (fLine[run] in ['0'..'9']) do inc(run);
          fTokenId:=tkString;
        end else handleComment([#0],true);
        firstInLine:=false;
      end
      else begin
        fTokenId := tkDefault;
        //TODO: ⚡ Ensure, that utf8 glyphs are handled as a single token and not bytewise ⚡
        inc(run);
      end;
    end;
  end;

PROCEDURE TMnhInputSyn.next;
  begin
    fTokenId := tkDefault;
    fTokenSubId:=skNormal;
    fTokenPos := run;
    if  (blobEnder<>#0) then begin
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
    end else
    inherited next;
    case highlightingData.isErrorLocation(fLineNumber,fTokenPos,run) of
      2: fTokenSubId:=skError;
      1: fTokenSubId:=skWarn;
    end;
  end;

PROCEDURE TMnhOutputSyn.next;
  CONST SECTION_HEAD_IDX=6;
  CONST tokenKindByPrefix:array [0..6] of record marker:string[3]; tokenkind:T_tokenKind; end=(
    (marker:''            ; tokenkind:tkDefault),    //0
    (marker:ECHO_MARKER   ; tokenkind:tkDefault),    //1
    (marker:NOTE_MARKER   ; tokenkind:tkNote),       //2
    (marker:ERROR_MARKER  ; tokenkind:tkError),      //3
    (marker:WARNING_MARKER; tokenkind:tkWarning),    //4
    (marker:TIMING_MARKER ; tokenkind:tkTimingNote), //5
    (marker:SECTION_MARKER; tokenkind:tkTimingNote));//6=SECTION_HEAD_IDX

  VAR b:byte;

  FUNCTION startsWith(CONST part:shortstring):boolean;
    VAR k:longint;
    begin
      if length(part)<=0 then exit(false);
      for k:=1 to length(part) do if fLine[k-1]<>part[k] then exit(false);
      result:=true;
    end;

  begin
    fTokenId := tkDefault;
    fTokenSubId:=skNormal;
    fTokenPos := run;
    //Marker handling...
    if run=0 then begin
      blobEnder:=#0;
      for b:=1 to length(tokenKindByPrefix)-1 do if startsWith(tokenKindByPrefix[b].marker) then begin
        blobEnder:=chr(b);
        run:=3;
        exit;
      end;
      if fLine[run]=#0 then begin
        fTokenId:=tkNull;
        inc(run);
      end else while fLine[run]<>#0 do inc(run);
      exit;
    end;
    case ord(blobEnder) of
      1: if (run<3+C_echoPrefixLength) and (
           startsWith(ECHO_MARKER+C_echoOutInfix) or
           startsWith(ECHO_MARKER+C_echoInInfix) or
           startsWith(ECHO_MARKER+C_echoDeclInfix) or
           startsWith(ECHO_MARKER+C_echoContdInfix))
         then begin
           fTokenId:=tkOperator;
           run:=3+C_echoPrefixLength;
         end else inherited next;
      else begin
        fTokenId:=tokenKindByPrefix[ord(blobEnder)].tokenkind;
        if blobEnder=chr(SECTION_HEAD_IDX) then fTokenSubId:=skError;
        if fLine[run]=#0 then begin
          fTokenId:=tkNull;
          inc(run);
        end else while fLine[run]<>#0 do inc(run);
      end;
    end;
  end;

PROCEDURE TMnhDebugSyn.next;
  begin
    fTokenId := tkDefault;
    fTokenSubId:=skNormal;
    fTokenPos := run;
    inherited;
    if (blobEnder=#0) then fTokenSubId:=skWarn;
  end;

FUNCTION TAbstractSynMnhSyn.GetEol: boolean;
  begin
    result := fTokenId = tkNull;
  end;

FUNCTION TAbstractSynMnhSyn.getRange: pointer;
  begin
    result:=nil;
    move(blobEnder,result,sizeOf(blobEnder));
  end;

FUNCTION TAbstractSynMnhSyn.getToken: ansistring;
  VAR len: longint;
  begin
    len := run-fTokenPos;
    result := '';
    setString(result, (fLine+fTokenPos), len);
  end;

PROCEDURE TAbstractSynMnhSyn.GetTokenEx(OUT tokenStart: PChar; OUT tokenLength: integer);
  begin
    tokenLength := run-fTokenPos;
    tokenStart := fLine+fTokenPos;
  end;

FUNCTION TAbstractSynMnhSyn.GetTokenAttribute: TSynHighlighterAttributes;
  begin
    result := styleTable [fTokenId,fTokenSubId];
  end;

FUNCTION TAbstractSynMnhSyn.GetTokenKind: integer;
  begin
    result := ord(fTokenId);
  end;

FUNCTION TAbstractSynMnhSyn.GetTokenPos: integer;
  begin
    result := fTokenPos;
  end;

PROCEDURE TAbstractSynMnhSyn.ResetRange;
  begin
    if lineIndex=0 then blobEnder:=#0;
    firstInLine:=true;
  end;

PROCEDURE TAbstractSynMnhSyn.setRange(value: pointer);
  begin
    move(value,blobEnder,sizeOf(blobEnder));
  end;

class FUNCTION TAbstractSynMnhSyn.GetLanguageName: ansistring;
  begin
    result := 'MNH';
  end;

FUNCTION TAbstractSynMnhSyn.GetIdentChars: TSynIdentChars;
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
      id: string;
  begin
    if listsAreInitialized then exit;
    tokenTypeMap.create();
    for tt:=low(T_tokenType) to high(T_tokenType) do put(C_tokenDoc[tt].reservedWordClass,C_tokenDefaultId[tt]);
    for i:=0 to high(C_specialWordInfo) do with C_specialWordInfo[i] do put(reservedWordClass,txt);
    for tc in T_typeCheck do put(rwc_type,C_typeCheckInfo[tc].name);

    for id in INTERPOLATOR_TYPE_NAMES do put(rwc_type,id);
    put(rwc_type,FTP_TYPE_STRING);
    put(rwc_type,QUEUE_TYPE_NAME);

    for md in T_modifier do put(rwc_modifier,C_modifierInfo[md].name);
    builtinRules.create;
    builtinRules.put(builtinFunctionMap.getAllIds);
    listsAreInitialized:=true;
  end;

FINALIZATION
  if listsAreInitialized then begin
    tokenTypeMap.destroy;
    builtinRules.destroy;
  end;
end.
