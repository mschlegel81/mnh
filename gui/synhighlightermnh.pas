UNIT SynHighlighterMnh;

{I SynEdit.inc}

INTERFACE

USES
  SysUtils, Classes, FileUtil, Controls, Graphics,
  SynEditTypes, SynEditHighlighter, mnh_evalThread,mnh_litvar,mnh_constants;
CONST
  C_DeclEchoHead = #10+' in>';
  C_ExprEchoHead = #10+'_in>';
  C_ExprOutHead = #10+'out>';


TYPE
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkDollarIdentifier,
    tkUserRule,
    tkIntrinsicRuleOrKeyword,
    tkTypeCheck,
    tkString,
    tkNumber,
    tkBoolean,
    tkOperator,
    tkDeclarationOp,
    tkUnknown,
    tkNull);

  TProcTableProc = PROCEDURE of object;

  TRangeState = (rsANil, rsAnsi, rsPasStyle, rsCStyle, rsUnKnown);

TYPE
  { TSynMnhSyn }

  TSynMnhSyn = CLASS(TSynCustomHighlighter)
  private
    isDeclInput: boolean;
    isExprInput: boolean;
    isOutput: boolean;

    fLine: PChar;
    styleTable: array[TtkTokenKind] of TSynHighlighterAttributes;
    fRange: TRangeState;
    Run: longint;
    fTokenPos: integer;
    fTokenID: TtkTokenKind;
    fLineNumber: integer;

  protected
    FUNCTION GetIdentChars: TSynIdentChars; override;
  public
    CLASS FUNCTION GetLanguageName: string; override;
  public
    CONSTRUCTOR create(AOwner: TComponent); override;
    DESTRUCTOR destroy; override;
    FUNCTION GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
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
  end;

IMPLEMENTATION

CONSTRUCTOR TSynMnhSyn.create(AOwner: TComponent);
  CONST
    identifierForeground: TColor = $00FF0000;
  begin
    inherited create(AOwner);
    styleTable[tkComment] := TSynHighlighterAttributes.create('comment');
    styleTable[tkComment].Style := [fsItalic];
    styleTable[tkComment].Foreground := $00999999;

    styleTable[tkIdentifier] := TSynHighlighterAttributes.create('identifier');
    styleTable[tkIdentifier].Foreground := identifierForeground;

    styleTable[tkDollarIdentifier] :=
      TSynHighlighterAttributes.create('dollarIdentifier');
    styleTable[tkDollarIdentifier].Style := [fsItalic];

    styleTable[tkUserRule] := TSynHighlighterAttributes.create('userRule');
    styleTable[tkUserRule].Style := [fsBold];
    styleTable[tkUserRule].Foreground := identifierForeground;

    styleTable[tkIntrinsicRuleOrKeyword] :=
      TSynHighlighterAttributes.create('intrinsicRuleOrKeyword');
    styleTable[tkIntrinsicRuleOrKeyword].Style := [fsBold];
    styleTable[tkIntrinsicRuleOrKeyword].Foreground := $00888800;

    styleTable[tkTypeCheck] := TSynHighlighterAttributes.create('typeCheck');
    styleTable[tkTypeCheck].Style := [fsBold];
    styleTable[tkTypeCheck].Foreground := $00880088;

    styleTable[tkString] := TSynHighlighterAttributes.create('string');
    styleTable[tkString].Foreground := $000000FF;

    styleTable[tkNumber] := TSynHighlighterAttributes.create('number');
    styleTable[tkNumber].Foreground := $000088FF;

    styleTable[tkBoolean] := TSynHighlighterAttributes.create('boolean');
    styleTable[tkBoolean].Foreground := $000044FF;
    styleTable[tkBoolean].Style := [fsItalic];

    styleTable[tkOperator] := TSynHighlighterAttributes.create('operator');
    styleTable[tkOperator].Foreground := $00008800;

    styleTable[tkDeclarationOp] := TSynHighlighterAttributes.create('declarationOp');
    styleTable[tkDeclarationOp].Foreground := $00008800;
    styleTable[tkDeclarationOp].Style := [fsBold];

    styleTable[tkUnknown] := TSynHighlighterAttributes.create('unknown');
    styleTable[tkUnknown].Foreground := clBlack;
    styleTable[tkUnknown].Background := $000088FF;

    styleTable[tkNull] := TSynHighlighterAttributes.create('null');
    styleTable[tkNull].Foreground := clBlack;
    styleTable[tkNull].Background := clYellow;
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
    result := styleTable [tkUnknown];
  end;

PROCEDURE TSynMnhSyn.SetLine(CONST NewValue: string; LineNumber: integer);
  begin
    inherited;
    isDeclInput := false;
    isExprInput := false;
    isOutput := false;
    fLine := PChar(NewValue);
    Run := 0;
    fLineNumber := LineNumber;
    Next;
  end; { SetLine }

PROCEDURE TSynMnhSyn.Next;
  VAR
    localId: shortString;
    i: longint;
  begin
    fTokenID := tkUnknown;
    fTokenPos := Run;
    if (run = 0) and (fLine [0] = #10) then
      begin
      i := 0;
      localId := '';
      while (length(localId)<length(C_ExprOutHead)) and (fLine [i]<>#0) do
        begin
        localId := localId+fLine [i];
        Inc(i);
        end;
      if localId = C_DeclEchoHead then
        begin
        isDeclInput := true;
        run := i+1;
        exit;
        end
      else if localId = C_ExprEchoHead then
        begin
        isExprInput := true;
        run := i+1;
        exit;
        end
      else if localId = C_ExprOutHead then
        begin
        isOutput := true;
        run := i+1;
        exit;
        end;
      end;

    case fLine [Run] of
      #0: fTokenID := tkNull;
      ';': begin
        Inc(run);
        fTokenID := tkUnknown;
        ResetRange;
        end;
      '0'..'9': begin
        while fLine [run] in ['0'..'9', '-', '+', '.', 'E', 'e'] do
          Inc(run);
        fTokenId := tkNumber;
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
        while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9', '.'] do
          begin
          localId := localId+fLine [run];
          Inc(run);
          end;
        if (localId = C_tokenString[tt_operatorXor]   ) or
           (localId = C_tokenString[tt_operatorOr]    ) or
           (localId = C_tokenString[tt_operatorMod]   ) or
           (localId = C_tokenString[tt_operatorIn]    ) or
           (localId = C_tokenString[tt_operatorDivInt]) or
           (localId = C_tokenString[tt_operatorAnd]   ) then
          fTokenId := tkOperator
        else if (localId = C_boolText[true]) or (localId = C_boolText[false]) then
          fTokenId := tkBoolean
        else if (localId = 'Nan') or (localId = 'Inf')  then
          fTokenId := tkNumber
        else if (localId = 'void') or
                (localId = 'main') or
                (localId = 'USE') or
                (localId = C_tokenString[tt_modifier_private] ) or
                (localId = C_tokenString[tt_modifier_memoized]) or
                (localId = C_tokenString[tt_each]             ) or
                (localId = C_tokenString[tt_parallelEach]     ) then
          fTokenId := tkIntrinsicRuleOrKeyword
        else
        if localUserRules.contains(localId) then
          fTokenID := tkUserRule
        else if importedUserRules.contains(localId) then
          fTokenID := tkUserRule
        else if intrinsicRules.contains(localId) then
          fTokenID := tkIntrinsicRuleOrKeyword
        else
          fTokenID := tkIdentifier;
        end;
      '|', '^', '?', '+', '&', '%', '*', '=', '<', '>': begin
        Inc(run);
        fTokenID := tkOperator;
        end;
      '-': begin
        Inc(run);
        if fLine [run] = '>' then
          begin
          Inc(run);
          fTokenID := tkDeclarationOp;
          end
        else
          fTokenID := tkOperator;
        end;
      '/': begin
        Inc(run);
        if fLine [run] = '/' then
          begin
          while fLine [run]<>#0 do
            Inc(run);
          fTokenId := tkComment;
          end
        else
          fTokenId := tkOperator;
        end;
      ':': begin
        Inc(run);
        case fLine [run] of
          'b', 'e', 'i', 'l', 'n', 's', 'r': begin
            localId := ':';
            while fLine [run] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do
              begin
              localId := localId+fLine [run];
              Inc(run);
              end;
            if (localId = ':booleanList') or (localId = ':boolean') or
              (localId = ':expression') or (localId = ':intList') or
              (localId = ':int') or (localId = ':list') or
              (localId = ':numericList') or (localId = ':numeric') or
              (localId = ':stringList') or (localId = ':scalar') or
              (localId = ':string') or (localId = ':realList') or
              (localId = ':real') then
              fTokenID := tkTypeCheck
            else
              fTokenID := tkUnknown;
            end;
          '=': begin
            Inc(run);
            fTokenID := tkDeclarationOp;
            end;
        else fTokenID := tkOperator;
          end;
        end;
      '"': begin
        Inc(run);
        while (fLine [run]<>#0) and ((fLine [run]<>'"') or
            (fLine [run-1] = '\') and (fLine [run-2]<>'\')) do
          Inc(run);
        if (fLine [run] = '"') then
          Inc(run);
        fTokenId := tkString;
        end;
      '''': begin
        Inc(run);
        while (fLine [run]<>#0) and ((fLine [run]<>'''') or (fLine [run-1] = '\')) do
          Inc(run);
        if (fLine [run] = '''') then
          Inc(run);
        fTokenId := tkString;
        end;
    else begin
      fTokenID := tkUnknown;
      Inc(Run);
      end;
      end;
  end;

FUNCTION TSynMnhSyn.GetEol: boolean;
  begin
    result := fTokenId = tkNull;
  end;

FUNCTION TSynMnhSyn.GetRange: Pointer;
  begin
    //result := Pointer(PtrUInt(fRange));
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
  VAR
    bg: longint;
  begin
    bg := 255;
    if isDeclInput then
      bg := bg-40;
    if isExprInput then
      bg := bg-20;
    if isOutput then
      bg := bg-10;
    result := styleTable [fTokenID];
    result.Background := (bg or (bg shl 8) or (bg shl 16));
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
    fRange := rsUnknown;
    isDeclInput := false;
    isExprInput := false;
    isOutput := false;
  end;

PROCEDURE TSynMnhSyn.SetRange(value: Pointer);
  begin
    //expressionLevel:=ptrUInt(expressionLevel);
    //fRange := TRangeState(PtrUInt(Value));
  end;

CLASS FUNCTION TSynMnhSyn.GetLanguageName: string;
  begin
    result := 'mnh';
  end;

FUNCTION TSynMnhSyn.GetIdentChars: TSynIdentChars;
  begin
    result := ['a'..'z', 'A'..'Z', '.', '_', '0'..'9'];
  end;




end.
