UNIT SynHighlighterMnh;

{I SynEdit.inc}

INTERFACE

USES
  sysutils, Classes, FileUtil, Controls, Graphics,
  SynEditTypes, SynEditHighlighter, mnh_evalThread,mnh_litVar,mnh_constants,myGenerics;

CONST
  C_startOfHeader=#01;

  C_errLineCase:array[T_messageTypeOrErrorLevel] of record
    backgroundColor:longint;
    foregroundColor:longint;
  end=((backgroundColor:maxLongint; foregroundColor:maxLongint),  //elc_clearConsole
       (backgroundColor:maxLongint; foregroundColor:maxLongint),  //elp_printline,
       (backgroundColor:$00EBEBEB;  foregroundColor:maxLongint),  //ele_echoInput,
       (backgroundColor:$00D7D7D7;  foregroundColor:maxLongint),  //eld_echoDeclaration,
       (backgroundColor:$00F5F5F5;  foregroundColor:maxLongint),  //elo_echoOutput,
       (backgroundColor:maxLongint; foregroundColor:maxLongint),  //el0_allOkay,
       (backgroundColor:maxLongint; foregroundColor:maxLongint),  //el1_note,
       (backgroundColor:maxLongint; foregroundColor:maxLongint),  //el2_warning,
       (backgroundColor:maxLongint; foregroundColor:$000000FF ),  //el3_evalError,
       (backgroundColor:maxLongint; foregroundColor:$000000FF ),  //el4_parsingError,
       (backgroundColor:$0000FFFF;  foregroundColor:$000000FF )); //el5_systemError

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
    specialLineCase:T_messageTypeOrErrorLevel;
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
VAR modifierStrings:T_listOfString;
    operatorStrings:T_listOfString;
    specialLiteralStrings:T_listOfString;
    specialConstructStrings:T_listOfString;

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
    specialLineCase:=elc_clearConsole;
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
    lc:T_messageTypeOrErrorLevel;
  begin
    isMarked:=false;
    fTokenId := tkDefault;
    fTokenPos := Run;
    if defaultToPrint then begin
      if (run = 0) and (fLine [0] = C_startOfHeader) then begin
        i := 0;
        localId := '';
        inc(i); //skip C_startOfHeader character
        while (fLine [i]<>C_startOfHeader) do begin
          localId := localId+fLine [i];
          inc(i);
        end;
        for lc:=low(T_messageTypeOrErrorLevel) to high(T_messageTypeOrErrorLevel) do
          if C_errorLevelTxt[lc]=localId then specialLineCase:=lc;
        if specialLineCase<>elc_clearConsole then begin
          run:=i+1;
          if specialLineCase>=el3_evalError then begin
            fTokenId:=tkError;
            while (fLine[run]<>#0) do inc(run);
          end;
          exit;
        end;
      end;

      if (specialLineCase=elc_clearConsole) then begin
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
        if      operatorStrings        .contains(localId) then fTokenId := tkOperator
        else if specialLiteralStrings  .contains(localId) then fTokenId := tkNonStringLiteral
        else if modifierStrings        .contains(localId) then fTokenId := tkModifier
        else if specialConstructStrings.contains(localId) then fTokenId := tkSpecialRule
        else
        if      userRules     .contains(localId) then fTokenId := tkUserRule
        else if intrinsicRules.contains(localId) then fTokenId := tkBultinRule
        else fTokenId := tkDefault;
        isMarked:=(localId=markedWord);
      end;
      '|', '^', '?', '+', '&', '%', '*', '=', '<', '>' ,'-', '@', '.': begin
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
    with C_errLineCase[specialLineCase] do begin
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
    specialLineCase:=elc_clearConsole;
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

INITIALIZATION
  operatorStrings:=reservedWordsByClass(rwc_operator);
  modifierStrings:=reservedWordsByClass(rwc_modifier);
  specialConstructStrings:=reservedWordsByClass(rwc_specialConstruct);
  specialLiteralStrings:=reservedWordsByClass(rwc_specialLiteral);
FINALIZATION
  operatorStrings.destroy;
  modifierStrings.destroy;
  specialConstructStrings.destroy;
  specialLiteralStrings.destroy;
end.
