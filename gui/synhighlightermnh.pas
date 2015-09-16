UNIT SynHighlighterMnh;

{I SynEdit.inc}

INTERFACE

USES
  sysutils, Classes, FileUtil, Controls, Graphics,
  SynEditTypes, SynEditHighlighter, mnh_evalThread,mnh_litVar,mnh_constants;

CONST
  C_specialHeads: array [1..5] of record
    head:string[5];
    backgroundColor:longint;
    foregroundColor:longint;
  end =
    ((head:#10+' in>';backgroundColor:$00D7D7D7; foregroundColor:maxLongint),
     (head:#10+'_in>';backgroundColor:$00EBEBEB; foregroundColor:maxLongint),
     (head:#10+'out>';backgroundColor:$00F5F5F5; foregroundColor:maxLongint),
     (head:#10+'!! >';backgroundColor:$00F5F5F5; foregroundColor:maxLongint),
     (head:#10+' > >';backgroundColor:$00F5F5F5; foregroundColor:maxLongint));

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
    fTokenId: TtkTokenKind;
    fLineNumber: integer;

    markedWord:string;

  protected
    FUNCTION GetIdentChars: TSynIdentChars; override;
  public
    class FUNCTION GetLanguageName: string; override;
  public
    CONSTRUCTOR create(AOwner: TComponent; forOutput:boolean);
    DESTRUCTOR destroy; override;
    FUNCTION GetDefaultAttribute(index: integer): TSynHighlighterAttributes; override;
    FUNCTION GetEol: boolean; override;
    FUNCTION getRange: pointer; override;
    FUNCTION GetTokenID: TtkTokenKind;
    FUNCTION GetToken: string; override;
    PROCEDURE GetTokenEx(OUT TokenStart: PChar; OUT TokenLength: integer); override;
    FUNCTION GetTokenAttribute: TSynHighlighterAttributes; override;
    FUNCTION GetTokenKind: integer; override;
    FUNCTION GetTokenPos: integer; override;
    PROCEDURE next; override;
    PROCEDURE ResetRange; override;
    PROCEDURE setRange(value: pointer); override;
    PROCEDURE SetLine(CONST newValue: string; LineNumber: integer); override;
    FUNCTION setMarkedWord(CONST s:ansistring):boolean;
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

    styleTable[tkComment         ].foreground:=$00999999;
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
    styleTable[tkError           ].foreground:=$000000FF; styleTable[tkError].background:=$0000FFFF;
    styleTable[tkStacktrace      ].foreground:=$00FF0000;

    markedWord:='';
  end; { Create }

DESTRUCTOR TSynMnhSyn.destroy;
  VAR
    tk: TtkTokenKind;
  begin
    for tk := tkComment to tkNull do
      styleTable[tk].destroy;
    inherited destroy;
  end; { Destroy }

FUNCTION TSynMnhSyn.GetDefaultAttribute(index: integer): TSynHighlighterAttributes;
  begin
    result := styleTable [tkDefault];
  end;

PROCEDURE TSynMnhSyn.SetLine(CONST newValue: string; LineNumber: integer);
  begin
    inherited;
    specialLineCase:=0;
    fLine := PChar(newValue);
    Run := 0;
    fLineNumber := LineNumber;
    next;
  end;

FUNCTION TSynMnhSyn.setMarkedWord(CONST s:ansistring):boolean;
  begin
    result:=(s<>markedWord);
    markedWord:=s;
  end;

PROCEDURE TSynMnhSyn.next;
  VAR
    localId: shortString;
    i: longint;
  begin
    isMarked:=false;
    fTokenId := tkDefault;
    fTokenPos := Run;
    if defaultToPrint then begin
      if (run = 0) and (fLine [0] = #10) then begin
        i := 0;
        localId := '';
        while (length(localId)<5) and (fLine [i]<>#0) do begin
          localId := localId+fLine [i];
          inc(i);
        end;
        specialLineCase:=5;
        while (specialLineCase>0) and (C_specialHeads[specialLineCase].head<>localId) do dec(specialLineCase);
        if specialLineCase>0 then begin
          run:=i+1;
          if specialLineCase>=4 then begin
            if specialLineCase=4 then fTokenId:=tkError
                                 else fTokenId:=tkStacktrace;
            while (fLine[run]<>#0) do inc(run);
          end;
          exit;
        end;
      end;

      if (specialLineCase=0) then begin
        if fLine[run]=#0 then fTokenId := tkNull
        else begin
          while (fLine[run]<>#0) do inc(run);
          fTokenId:=tkDefault;
        end;
        exit;
      end;
    end;

    case fLine [Run] of
      #0: fTokenId := tkNull;
      ';': begin
        inc(run);
        fTokenId := tkDefault;
        end;
      '0'..'9': begin
        while fLine [run] in ['0'..'9', '-', '+', '.', 'E', 'e'] do
          inc(run);
        fTokenId := tkNonStringLiteral;
        end;
      '$': begin
        inc(run);
        while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do
          inc(run);
        fTokenId := tkDollarIdentifier;
        end;
      'a'..'z', 'A'..'Z': begin
        localId := fLine [run];
        inc(run);
        while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9', C_ID_QUALIFY_CHARACTER] do
          begin
          localId := localId+fLine [run];
          inc(run);
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
                (localId = C_tokenString[tt_modifier_local]) then fTokenId := tkModifier
        else if (localId = C_tokenString[tt_aggregatorConstructor]) or
                (localId = C_tokenString[tt_procedureBlockBegin]) or
                (localId = C_tokenString[tt_procedureBlockEnd]) or
                (localId = C_tokenString[tt_procedureBlockWhile] ) or
                (localId = C_tokenString[tt_each]             ) or
                (localId = C_tokenString[tt_parallelEach]     ) then
          fTokenId := tkSpecialRule
        else
        if      userRules     .contains(localId) then fTokenId := tkUserRule
        else if intrinsicRules.contains(localId) then fTokenId := tkBultinRule
        else fTokenId := tkDefault;
        isMarked:=(localId=markedWord);
      end;
      '|', '^', '?', '+', '&', '%', '*', '=', '<', '>' ,'-', '@': begin
        inc(run);
        fTokenId := tkOperator;
      end;
      '/': begin
        inc(run);
        if fLine [run] = '/' then begin
          inc(run);
          if      fLine[run]='!' then fTokenId:=tkSpecialComment
          else if fLine[run]='*' then fTokenId:=tkDocComment
                                 else fTokenId:=tkComment;
          while fLine [run]<>#0 do inc(run);
        end
        else fTokenId := tkOperator;
      end;
      ':': begin
        inc(run);
        case fLine [run] of
          'b', 'e', 'i', 'l', 'n', 's', 'r', 'k': begin
            localId := ':';
            i:=run;
            while fLine [i] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do begin
              localId := localId+fLine [i];
              inc(i);
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
              fTokenId := tkOperator;
              run:=i;
            end
            else begin
              fTokenId := tkOperator;
              inc(run);
            end;
          end;
          else fTokenId := tkOperator;
        end;
      end;
      '"': begin
        inc(run);
        while (fLine [run]<>#0) and ((fLine [run]<>'"') or (fLine [run-1] = '\') and (fLine [run-2]<>'\')) do
          inc(run);
        if (fLine [run] = '"') then
          inc(run);
        fTokenId := tkString;
      end;
      '''': begin
        inc(run);
        while (fLine [run]<>#0) and ((fLine [run]<>'''') or (fLine [run-1] = '\') and (fLine [run-2]<>'\')) do
          inc(run);
        if (fLine [run] = '''') then
          inc(run);
        fTokenId := tkString;
      end;
      else begin
        fTokenId := tkDefault;
        inc(Run);
      end;
    end;
  end;

FUNCTION TSynMnhSyn.GetEol: boolean;
  begin
    result := fTokenId = tkNull;
  end;

FUNCTION TSynMnhSyn.getRange: pointer;
  begin
    result := nil;
  end;

FUNCTION TSynMnhSyn.GetToken: string;
  VAR
    Len: longint;
  begin
    Len := Run-fTokenPos;
    result := '';
    SetString(result, (fLine+fTokenPos), Len);
  end;

PROCEDURE TSynMnhSyn.GetTokenEx(OUT TokenStart: PChar; OUT TokenLength: integer);
  begin
    TokenLength := Run-fTokenPos;
    TokenStart := fLine+fTokenPos;
  end;

FUNCTION TSynMnhSyn.GetTokenID: TtkTokenKind;
  begin
    result := fTokenId;
  end;

FUNCTION TSynMnhSyn.GetTokenAttribute: TSynHighlighterAttributes;
  begin
    result := styleTable [fTokenId];
    if specialLineCase in [1..3] then with C_specialHeads[specialLineCase] do begin
      if backgroundColor<>maxLongint then result.background:=backgroundColor;
      if foregroundColor<>maxLongint then result.foreground:=foregroundColor;
    end;
    if isMarked then result.FrameColor:=$00888888
                else result.FrameColor:=clNone;
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
    specialLineCase:=0;
    isMarked:=false;
  end;

PROCEDURE TSynMnhSyn.setRange(value: pointer);
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
