UNIT SynHighlighterMnh;

{I SynEdit.inc}

INTERFACE

USES
  SysUtils, Classes, FileUtil, Controls, Graphics,
  SynEditTypes, SynEditHighlighter, mnh_evalThread,mnh_litvar,mnh_constants;

CONST
  C_specialHeads: array [1..5] of record
    head:string[5];
    backgroundColor:longint;
    foregroundColor:longint;
  end =
    ((head:#10+' in>';backgroundColor:$00D7D7D7; foregroundColor:maxlongint),
     (head:#10+'_in>';backgroundColor:$00EBEBEB; foregroundColor:maxlongint),
     (head:#10+'out>';backgroundColor:$00F5F5F5; foregroundColor:maxlongint),
     (head:#10+'!! >';backgroundColor:$00F5F5F5; foregroundColor:maxlongint),
     (head:#10+' > >';backgroundColor:$00F5F5F5; foregroundColor:maxlongint));

TYPE
  TtkTokenKind = (
    tkComment,
    tkDocComment,
    tkSpecialComment,
    tkDefault,
    tkDollarIdentifier,
    tkUserRule,
    tkBultinRule,
    tkSpecialRule,
    tkOperator,
    tkNonStringLiteral,
    tkString,
    tkModifier,
    tkNull,
    tkError,
    tkStacktrace);

TYPE
  { TSynMnhSyn }

  TSynMnhSyn = class(TSynCustomHighlighter)
  private
    specialLineCase:byte;
    isMarked:    boolean;
    defaultToPrint:boolean;

    fLine: PChar;
    styleTable: array[TtkTokenKind] of TSynHighlighterAttributes;
    Run: longint;
    fTokenPos: integer;
    fTokenID: TtkTokenKind;
    fLineNumber: integer;

    markedWord:string;
    markedLine,
    markedCol:longint;
    atMarkedToken:boolean;

  protected
    FUNCTION GetIdentChars: TSynIdentChars; override;
  public
    class FUNCTION GetLanguageName: string; override;
  public
    CONSTRUCTOR create(AOwner: TComponent; forOutput:boolean);
    DESTRUCTOR destroy; override;
    FUNCTION GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    FUNCTION GetEol: boolean; override;
    FUNCTION GetRange: Pointer; override;
    FUNCTION GetTokenID: TtkTokenKind;
    FUNCTION GetToken: string; override;
    PROCEDURE GetTokenEx(OUT TokenStart: PChar; OUT TokenLength: integer); override;
    FUNCTION GetTokenAttribute: TSynHighlighterAttributes; override;
    FUNCTION GetTokenKind: integer; override;
    FUNCTION GetTokenPos: integer; override;
    PROCEDURE Next; override;
    PROCEDURE ResetRange; override;
    PROCEDURE SetRange(value: Pointer); override;
    PROCEDURE SetLine(CONST NewValue: string; LineNumber: integer); override;
    FUNCTION setMarkedWord(CONST s:ansistring):boolean;
    FUNCTION setMarkedLine(CONST i,j:longint):boolean;
  end;

IMPLEMENTATION

CONSTRUCTOR TSynMnhSyn.create(AOwner: TComponent; forOutput:boolean);
  begin
    inherited create(AOwner);
    defaultToPrint:=forOutput;
    styleTable[tkComment         ]:=TSynHighlighterAttributes.create('Comment');
    styleTable[tkDocComment      ]:=TSynHighlighterAttributes.create('DocComment');
    styleTable[tkSpecialComment  ]:=TSynHighlighterAttributes.create('SpecialComment');
    styleTable[tkDefault         ]:=TSynHighlighterAttributes.create('Default');
    styleTable[tkDollarIdentifier]:=TSynHighlighterAttributes.create('DollarIdentifier');
    styleTable[tkUserRule        ]:=TSynHighlighterAttributes.create('UserRule');
    styleTable[tkBultinRule      ]:=TSynHighlighterAttributes.create('BultinRule');
    styleTable[tkSpecialRule     ]:=TSynHighlighterAttributes.create('SpecialRule');
    styleTable[tkOperator        ]:=TSynHighlighterAttributes.create('Operator');
    styleTable[tkNonStringLiteral]:=TSynHighlighterAttributes.create('NonStringLiteral');
    styleTable[tkString          ]:=TSynHighlighterAttributes.create('String');
    styleTable[tkModifier        ]:=TSynHighlighterAttributes.create('Modifier');
    styleTable[tkNull            ]:=TSynHighlighterAttributes.create('Null');
    styleTable[tkError           ]:=TSynHighlighterAttributes.create('Error');
    styleTable[tkStacktrace      ]:=TSynHighlighterAttributes.create('Stacktrace');

    styleTable[tkComment         ].style:=[fsItalic];
    styleTable[tkDocComment      ].style:=[fsItalic,fsBold];
    styleTable[tkSpecialComment  ].style:=[fsItalic,fsBold,fsUnderline];
    styleTable[tkDollarIdentifier].style:=[fsItalic];
    styleTable[tkBultinRule      ].style:=[fsBold];
    styleTable[tkSpecialRule     ].style:=[fsBold];
    styleTable[tkOperator        ].style:=[fsBold];
    styleTable[tkModifier        ].style:=[fsBold];

    styleTable[tkComment         ].Foreground:=$00999999;
    styleTable[tkDocComment      ].foreground:=$00999999;
    styleTable[tkSpecialComment  ].foreground:=$00999999;
    styleTable[tkDefault         ].foreground:=$00000000;
    styleTable[tkDollarIdentifier].foreground:=$00000000;
    styleTable[tkUserRule        ].foreground:=$00FF0000;
    styleTable[tkBultinRule      ].foreground:=$00FF0000;
    styleTable[tkSpecialRule     ].foreground:=$00FF0000;
    styleTable[tkOperator        ].foreground:=$00880000;
    styleTable[tkNonStringLiteral].foreground:=$000000FF;
    styleTable[tkString          ].foreground:=$00008800;
    styleTable[tkModifier        ].foreground:=$000088FF;
    styleTable[tkNull            ].foreground:=$00000000;
    styleTable[tkError           ].Foreground:=$000000FF; styleTable[tkError].Background:=$0000FFFF;
    styleTable[tkStacktrace      ].Foreground:=$00FF0000;

    markedWord:='';
    markedLine:=-1;
  end; { Create }

DESTRUCTOR TSynMnhSyn.destroy;
  VAR
    tk: TtkTokenKind;
  begin
    for tk := tkComment to tkNull do
      styleTable[tk].destroy;
    inherited destroy;
  end; { Destroy }

FUNCTION TSynMnhSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
  begin
    result := styleTable [tkDefault];
  end;

PROCEDURE TSynMnhSyn.SetLine(CONST NewValue: string; LineNumber: integer);
  begin
    inherited;
    specialLineCase:=0;
    fLine := PChar(NewValue);
    Run := 0;
    fLineNumber := LineNumber;
    Next;
  end;

FUNCTION TSynMnhSyn.setMarkedWord(CONST s:ansistring):boolean;
  begin
    result:=(s<>markedWord);
    markedWord:=s;
  end;

FUNCTION TSynMnhSyn.setMarkedLine(CONST i,j:longint):boolean;
  begin
    result:=(i<>markedLine) or (j<>markedCol);
    markedLine:=i;
    markedCol:=j;
  end;

PROCEDURE TSynMnhSyn.Next;
  VAR
    localId: shortString;
    i: longint;
    runStart:longint;
  begin
    atMarkedToken:=false;
    runStart:=run;

    isMarked:=false;
    fTokenID := tkDefault;
    fTokenPos := Run;
    if defaultToPrint then begin
      if (run = 0) and (fLine [0] = #10) then begin
        i := 0;
        localId := '';
        while (length(localId)<5) and (fLine [i]<>#0) do begin
          localId := localId+fLine [i];
          Inc(i);
        end;
        specialLineCase:=5;
        while (specialLineCase>0) and (C_specialHeads[specialLineCase].head<>localId) do dec(specialLineCase);
        if specialLineCase>0 then begin
          run:=i+1;
          if specialLineCase>=4 then begin
            if specialLineCase=4 then fTokenID:=tkError
                                 else fTokenID:=tkStacktrace;
            while (fLine[run]<>#0) do inc(run);
          end;
          exit;
        end;
      end;

      if (specialLineCase=0) then begin
        if fLine[run]=#0 then fTokenID := tkNull
        else begin
          while (fLine[run]<>#0) do inc(run);
          fTokenID:=tkDefault;
        end;
        exit;
      end;
    end;

    case fLine [Run] of
      #0: fTokenID := tkNull;
      ';': begin
        Inc(run);
        fTokenID := tkDefault;
        end;
      '0'..'9': begin
        while fLine [run] in ['0'..'9', '-', '+', '.', 'E', 'e'] do
          Inc(run);
        fTokenId := tkNonStringLiteral;
        end;
      '$': begin
        Inc(run);
        while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do
          Inc(run);
        fTokenID := tkDollarIdentifier;
        end;
      'a'..'z', 'A'..'Z': begin
        localId := fLine [run];
        Inc(run);
        while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9', C_ID_QUALIFY_CHARACTER] do
          begin
          localId := localId+fLine [run];
          Inc(run);
          end;
        if (localId = C_tokenString[tt_operatorXor]   ) or
           (localId = C_tokenString[tt_operatorOr]    ) or
           (localId = C_tokenString[tt_operatorMod]   ) or
           (localId = C_tokenString[tt_operatorIn]    ) or
           (localId = C_tokenString[tt_operatorDivInt]) or
           (localId = C_tokenString[tt_operatorAnd]   ) then fTokenId := tkOperator
        else if (localId = C_boolText[true]) or
                (localId = C_boolText[false]) or
                (localId = 'Nan') or
                (localId = 'Inf') or
                (localId = 'void')  then fTokenId := tkNonStringLiteral
        else if (localId = C_tokenString[tt_modifier_private] ) or
                (localId = C_tokenString[tt_modifier_memoized]) or
                (localId = C_tokenString[tt_modifier_mutable] ) or
                (localId = C_tokenString[tt_modifier_synchronized]) or
                (localId = C_tokenString[tt_modifier_local]) then fTokenID := tkModifier
        else if (localId = C_tokenString[tt_aggregatorConstructor]) or
                (localId = C_tokenString[tt_procedureBlockBegin]) or
                (localId = C_tokenString[tt_procedureBlockEnd]) or
                (localId = C_tokenString[tt_procedureBlockWhile] ) or
                (localId = C_tokenString[tt_each]             ) or
                (localId = C_tokenString[tt_parallelEach]     ) then
          fTokenId := tkSpecialRule
        else
        if      userRules     .contains(localId) then fTokenID := tkUserRule
        else if intrinsicRules.contains(localId) then fTokenID := tkBultinRule
        else fTokenID := tkDefault;
        isMarked:=(localId=markedWord);
      end;
      '|', '^', '?', '+', '&', '%', '*', '=', '<', '>' ,'-', '@': begin
        Inc(run);
        fTokenID := tkOperator;
      end;
      '/': begin
        Inc(run);
        if fLine [run] = '/' then begin
          Inc(run);
          if      fLine[run]='!' then fTokenID:=tkSpecialComment
          else if fLine[run]='*' then fTokenID:=tkDocComment
                                 else fTokenID:=tkComment;
          while fLine [run]<>#0 do Inc(run);
        end
        else fTokenId := tkOperator;
      end;
      ':': begin
        Inc(run);
        case fLine [run] of
          'b', 'e', 'i', 'l', 'n', 's', 'r', 'k': begin
            localId := ':';
            i:=run;
            while fLine [i] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do begin
              localId := localId+fLine [i];
              Inc(i);
            end;
            if (localId=C_tokenString[tt_typeCheckScalar      ]) or
               (localId=C_tokenString[tt_typeCheckList        ]) or
               (localId=C_tokenString[tt_typeCheckBoolean     ]) or
               (localId=C_tokenString[tt_typeCheckBoolList    ]) or
               (localId=C_tokenString[tt_typeCheckInt         ]) or
               (localId=C_tokenString[tt_typeCheckIntList     ]) or
               (localId=C_tokenString[tt_typeCheckReal        ]) or
               (localId=C_tokenString[tt_typeCheckRealList    ]) or
               (localId=C_tokenString[tt_typeCheckString      ]) or
               (localId=C_tokenString[tt_typeCheckStringList  ]) or
               (localId=C_tokenString[tt_typeCheckNumeric     ]) or
               (localId=C_tokenString[tt_typeCheckNumList     ]) or
               (localId=C_tokenString[tt_typeCheckExpression  ]) or
               (localId=C_tokenString[tt_typeCheckKeyValueList]) then begin
              fTokenID := tkOperator;
              run:=i;
            end
            else begin
              fTokenID := tkOperator;
              Inc(run);
            end;
          end;
          else fTokenID := tkOperator;
        end;
      end;
      '"': begin
        Inc(run);
        while (fLine [run]<>#0) and ((fLine [run]<>'"') or (fLine [run-1] = '\') and (fLine [run-2]<>'\')) do
          Inc(run);
        if (fLine [run] = '"') then
          Inc(run);
        fTokenId := tkString;
      end;
      '''': begin
        Inc(run);
        while (fLine [run]<>#0) and ((fLine [run]<>'''') or (fLine [run-1] = '\') and (fLine [run-2]<>'\')) do
          Inc(run);
        if (fLine [run] = '''') then
          Inc(run);
        fTokenId := tkString;
      end;
      else begin
        fTokenID := tkDefault;
        Inc(Run);
      end;
    end;
    atMarkedToken:=(LineIndex=markedLine) and ((markedCol<0) or (runStart<=markedCol) and (markedCol<run));
  end;

FUNCTION TSynMnhSyn.GetEol: boolean;
  begin
    result := fTokenId = tkNull;
  end;

FUNCTION TSynMnhSyn.GetRange: Pointer;
  begin
    result := nil;
  end;

FUNCTION TSynMnhSyn.GetToken: string;
  VAR
    Len: longint;
  begin
    Len := Run-fTokenPos;
    result := '';
    SetString(result, (FLine+fTokenPos), Len);
  end;

PROCEDURE TSynMnhSyn.GetTokenEx(OUT TokenStart: PChar; OUT TokenLength: integer);
  begin
    TokenLength := Run-fTokenPos;
    TokenStart := FLine+fTokenPos;
  end;

FUNCTION TSynMnhSyn.GetTokenID: TtkTokenKind;
  begin
    result := fTokenId;
  end;

FUNCTION TSynMnhSyn.GetTokenAttribute: TSynHighlighterAttributes;
  begin
    result := styleTable [fTokenID];
    if not(atMarkedToken) then begin
      if specialLineCase in [1..3] then with C_specialHeads[specialLineCase] do begin
        if backgroundColor<>maxLongint then result.background:=backgroundColor;
        if foregroundColor<>maxLongint then result.Foreground:=foregroundColor;
      end;
    end else begin
      result.Background := $0000FFFF;
    end;
    if isMarked then result.FrameColor:=$00888888
    else begin
      if atMarkedToken then result.FrameColor:=$00888888
                       else result.FrameColor:=clNone;
    end;
  end;

FUNCTION TSynMnhSyn.GetTokenKind: integer;
  begin
    result := Ord(fTokenId);
  end;

FUNCTION TSynMnhSyn.GetTokenPos: integer;
  begin
    result := fTokenPos;
  end;

PROCEDURE TSynMnhSyn.ResetRange;
  begin
    specialLineCase:=0;
    isMarked:=false;
  end;

PROCEDURE TSynMnhSyn.SetRange(value: Pointer);
  begin
    //expressionLevel:=ptrUInt(expressionLevel);
    //fRange := TRangeState(PtrUInt(Value));
  end;

class FUNCTION TSynMnhSyn.GetLanguageName: string;
  begin
    result := 'MNH';
  end;

FUNCTION TSynMnhSyn.GetIdentChars: TSynIdentChars;
  begin
    result := ['a'..'z', 'A'..'Z', C_ID_QUALIFY_CHARACTER, '_', '0'..'9'];
  end;

end.
