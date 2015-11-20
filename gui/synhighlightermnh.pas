UNIT SynHighlighterMnh;

{I SynEdit.inc}

INTERFACE

USES
  sysutils, Classes, FileUtil, Controls, Graphics,
  SynEditTypes, SynEditHighlighter, mnh_evalThread,mnh_constants,myGenerics;

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
    tkHighlightedItem,
    tkDebugInfo,
    tkWhiteOnWhite);
  T_mnhSynFlavour=(msf_input,msf_output,msf_debugger);

TYPE
  { TSynMnhSyn }

  TSynMnhSyn = class(TSynCustomHighlighter)
  private

    isMarked,
    isDebugInfoLine :boolean;
    flavour :T_mnhSynFlavour;

    fLine: PChar;
    styleTable: array[TtkTokenKind] of TSynHighlighterAttributes;
    run: longint;
    fTokenPos: integer;
    fTokenId: TtkTokenKind;
    fLineNumber: integer;
    markedToken:record
                  line,column:longint;
                end;

    markedWord:string;

  protected
    FUNCTION GetIdentChars: TSynIdentChars; override;
  public
    class FUNCTION GetLanguageName: string; override;
  public
    CONSTRUCTOR create(AOwner: TComponent; CONST flav:T_mnhSynFlavour);
    DESTRUCTOR destroy; override;
    FUNCTION GetDefaultAttribute(index: integer): TSynHighlighterAttributes; override;
    FUNCTION GetEol: boolean; override;
    FUNCTION getRange: pointer; override;
    FUNCTION getToken: string; override;
    PROCEDURE GetTokenEx(OUT TokenStart: PChar; OUT TokenLength: integer); override;
    FUNCTION GetTokenAttribute: TSynHighlighterAttributes; override;
    FUNCTION GetTokenKind: integer; override;
    FUNCTION GetTokenPos: integer; override;
    PROCEDURE next; override;
    PROCEDURE ResetRange; override;
    PROCEDURE setRange(value: pointer); override;
    PROCEDURE SetLine(CONST newValue: string; LineNumber: integer); override;
    FUNCTION setMarkedWord(CONST s:ansistring):boolean;
    PROCEDURE setMarkedToken(CONST line,column:longint);
  end;

IMPLEMENTATION
VAR modifierStrings:T_listOfString;
    operatorStrings:T_listOfString;
    specialLiteralStrings:T_listOfString;
    specialConstructStrings:T_listOfString;

CONSTRUCTOR TSynMnhSyn.create(AOwner: TComponent; CONST flav:T_mnhSynFlavour);
  begin
    inherited create(AOwner);
    flavour:=flav;
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
    styleTable[tkHighlightedItem ]:=TSynHighlighterAttributes.create('Highlighted');
    styleTable[tkDebugInfo       ]:=TSynHighlighterAttributes.create('DebugInfo');
    styleTable[tkWhiteOnWhite    ]:=TSynHighlighterAttributes.create('WhiteOnWhite');

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
    styleTable[tkNonStringLiteral].foreground:=$000000ff;
    styleTable[tkString          ].foreground:=$00008800;
    styleTable[tkModifier        ].foreground:=$000088ff;
    styleTable[tkNull            ].foreground:=$00000000;
    styleTable[tkError           ].foreground:=$000000ff; styleTable[tkError].background:=$0000FFFF;
    styleTable[tkHighlightedItem ].foreground:=$00000000;
    styleTable[tkHighlightedItem ].background:=$0000FFFF;
    styleTable[tkWhiteOnWhite    ].foreground:=$00FFFFFF;
    styleTable[tkWhiteOnWhite    ].background:=$00FEFEFE;
    styleTable[tkDebugInfo       ].FrameColor:=$00FF0000;

    markedWord:='';
    setMarkedToken(-1,-1);
  end; { Create }

DESTRUCTOR TSynMnhSyn.destroy;
  VAR tk: TtkTokenKind;
  begin
    for tk := low(TtkTokenKind) to high(TtkTokenKind) do styleTable[tk].destroy;
    inherited destroy;
  end; { Destroy }

FUNCTION TSynMnhSyn.GetDefaultAttribute(index: integer): TSynHighlighterAttributes;
  begin
    result := styleTable [tkDefault];
  end;

PROCEDURE TSynMnhSyn.SetLine(CONST newValue: string; LineNumber: integer);
  begin
    inherited;
    fLine := PChar(newValue);
    run := 0;
    fLineNumber := LineNumber;
    next;
  end;

FUNCTION TSynMnhSyn.setMarkedWord(CONST s:ansistring):boolean;
  begin
    result:=(s<>markedWord);
    markedWord:=s;
  end;

PROCEDURE TSynMnhSyn.setMarkedToken(CONST line,column:longint);
  begin
    markedToken.line:=line;
    markedToken.column:=column;
  end;

PROCEDURE TSynMnhSyn.next;
  VAR localId: shortString;
      i: longint;
      lc:T_messageType;
      specialLineCase:T_messageType;
  FUNCTION startsWith(CONST prefix:shortString):boolean;
    VAR k:longint;
    begin
      result:=length(prefix)>0;
      for k:=1 to length(prefix) do begin
        if fLine[k-1]<>prefix[k] then exit(false);
      end;
    end;

  begin
    isMarked:=false;
    fTokenId := tkDefault;
    fTokenPos := run;
    if (run = 0) then begin
      if flavour=msf_output then begin// in [msf_output,msf_debugger] and
        specialLineCase:=mt_clearConsole;
        i:=-1;
        for lc:=low(T_messageType) to high(T_messageType) do if startsWith(C_errorLevelTxt[lc]) then begin
          specialLineCase:=lc;
          i:=length(C_errorLevelTxt[lc]);
        end;
        if i>=0 then run:=i+1;
        if C_errorLevelForMessageType[specialLineCase]>=3 then fTokenId:=tkError
                                                          else fTokenId:=tkDefault;
        if not(specialLineCase in [mt_echo_output,mt_echo_declaration,mt_echo_input,mt_debug_step]) then while (fLine[run]<>#0) do inc(run);
        if run>0 then exit;
      end else if (flavour=msf_debugger) and (fLine[run]='#') then begin
        isDebugInfoLine:=true;
        fTokenId:=tkWhiteOnWhite;
        inc(run);
        exit;
      end else isDebugInfoLine:=false;
    end;
    if (flavour=msf_debugger) and (isDebugInfoLine) then begin
      if fLine[run]=#0 then begin
        fTokenId := tkNull;
      end else begin
        while not(fLine[run] in [#0,#28]) do inc(run);
        if fLine[run]=#28 then inc(run);
        if run>fTokenPos+1 then fTokenId:=tkDebugInfo
                           else fTokenId:=tkWhiteOnWhite;
      end;
      exit;
    end;

    case fLine [run] of
      #0: fTokenId := tkNull;
      ';': begin
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
      else begin
        fTokenId := tkDefault;
        inc(run);
      end;
    end;
    if (fLineNumber=markedToken.line) and (fTokenPos<=markedToken.column) and (run>markedToken.column) then begin
      fTokenId:=tkHighlightedItem;
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

FUNCTION TSynMnhSyn.getToken: string;
  VAR
    len: longint;
  begin
    len := run-fTokenPos;
    result := '';
    SetString(result, (fLine+fTokenPos), len);
  end;

PROCEDURE TSynMnhSyn.GetTokenEx(OUT TokenStart: PChar; OUT TokenLength: integer);
  begin
    TokenLength := run-fTokenPos;
    TokenStart := fLine+fTokenPos;
  end;

FUNCTION TSynMnhSyn.GetTokenAttribute: TSynHighlighterAttributes;
  begin
    result := styleTable [fTokenId];
    if isMarked then result.FrameColor:=$000000ff
                else if fTokenId<>tkDebugInfo then result.FrameColor:=clNone;
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
    isMarked:=false;
    isDebugInfoLine:=false;
  end;

PROCEDURE TSynMnhSyn.setRange(value: pointer);
  begin
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
