unit SynHighlighterMnh;

{I SynEdit.inc}

interface

uses
  SysUtils, Classes, FileUtil, Controls, Graphics,
  SynEditTypes, SynEditHighlighter, mnh_evalThread;

CONST C_DeclEchoHead=#10+' in>';
      C_ExprEchoHead=#10+'_in>';
      C_ExprOutHead =#10+'out>';


type
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

  TProcTableProc = procedure of object;

  TRangeState = (rsANil, rsAnsi, rsPasStyle, rsCStyle, rsUnKnown);

type
  { TSynMnhSyn }

  TSynMnhSyn = class(TSynCustomHighlighter)
  private
    isDeclInput:boolean;
    isExprInput:boolean;
    isOutput:boolean;

    fLine: PChar;
    styleTable: array[TtkTokenKind] of TSynHighlighterAttributes;
    fRange: TRangeState;
    Run: LongInt;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fLineNumber : Integer;

  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
  end;

implementation
constructor TSynMnhSyn.Create(AOwner: TComponent);
CONST identifierForeground:TColor=$00FF0000;
begin
  inherited Create(AOwner);
  styleTable[tkComment]:=TSynHighlighterAttributes.Create('comment');
  styleTable[tkComment].Style:=[fsItalic];
  styleTable[tkComment].Foreground:=$00999999;

  styleTable[tkIdentifier]:=TSynHighlighterAttributes.Create('identifier');
  styleTable[tkIdentifier].Foreground:=identifierForeground;

  styleTable[tkDollarIdentifier]:=TSynHighlighterAttributes.Create('dollarIdentifier');
  styleTable[tkDollarIdentifier].Style:=[fsItalic];

  styleTable[tkUserRule]:=TSynHighlighterAttributes.Create('userRule');
  styleTable[tkUserRule].Style:=[fsBold];
  styleTable[tkUserRule].Foreground:=identifierForeground;

  styleTable[tkIntrinsicRuleOrKeyword]:=TSynHighlighterAttributes.Create('intrinsicRuleOrKeyword');
  styleTable[tkIntrinsicRuleOrKeyword].Style:=[fsBold];
  styleTable[tkIntrinsicRuleOrKeyword].Foreground:=$00888800;

  styleTable[tkTypeCheck]:=TSynHighlighterAttributes.Create('typeCheck');
  styleTable[tkTypeCheck].Style:=[fsBold];
  styleTable[tkTypeCheck].Foreground:=$00880088;

  styleTable[tkString]:=TSynHighlighterAttributes.Create('string');
  styleTable[tkString].Foreground:=$000000FF;

  styleTable[tkNumber]:=TSynHighlighterAttributes.Create('number');
  styleTable[tkNumber].Foreground:=$000088FF;

  styleTable[tkBoolean]:=TSynHighlighterAttributes.Create('boolean');
  styleTable[tkBoolean].Foreground:=$000044FF;
  styleTable[tkBoolean].Style:=[fsItalic];

  styleTable[tkOperator]:=TSynHighlighterAttributes.Create('operator');
  styleTable[tkOperator].Foreground:=$00008800;

  styleTable[tkDeclarationOp]:=TSynHighlighterAttributes.Create('declarationOp');
  styleTable[tkDeclarationOp].Foreground:=$00008800;
  styleTable[tkDeclarationOp].Style:=[fsBold];

  styleTable[tkUnknown]:=TSynHighlighterAttributes.Create('unknown');
  styleTable[tkUnknown].Foreground:=clBlack;
  styleTable[tkUnknown].Background:=$000088FF;

  styleTable[tkNull]:=TSynHighlighterAttributes.Create('null');
  styleTable[tkNull].Foreground:=clBlack;
  styleTable[tkNull].Background:=clYellow;
end; { Create }

destructor TSynMnhSyn.Destroy;
  VAR tk:TtkTokenKind;
begin
  for tk:=tkComment to tkNull do styleTable[tk].destroy;
  inherited Destroy;
end; { Destroy }

function TSynMnhSyn.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
  result:=styleTable[tkUnknown];
end;

procedure TSynMnhSyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  inherited;
  isDeclInput:=false;
  isExprInput:=false;
  isOutput:=false;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynMnhSyn.Next;
VAR localId:shortString;
    i:longint;
    idInfo:T_idInfo;
begin
  fTokenID:=tkUnknown;
  fTokenPos := Run;
  if (run=0) and (fLine[0]=#10) then begin
    i:=0;
    localId:='';
    while (length(localId)<length(C_ExprOutHead)) and (fLine[i]<>#0) do begin
      localId:=localId+fLine[i];
      inc(i);
    end;
    if localId=C_DeclEchoHead then begin
      isDeclInput:=true; run:=i+1; exit;
    end else if localId=C_ExprEchoHead then begin
      isExprInput:=true; run:=i+1; exit;
    end else if localId=C_ExprOutHead then begin
      isOutput   :=true; run:=i+1; exit;
    end;
  end;

  case fLine[Run] of
    #0: fTokenID:=tkNull;
    ';': begin
      inc(run);
      fTokenID:=tkUnknown;
      ResetRange;
    end;
    '0'..'9': begin
      while fLine[run] in ['0'..'9','-','+','.','E','e'] do inc(run);
      fTokenId:=tkNumber;
    end;
    '$': begin
      inc(run);
      while fLine[run] in ['a'..'z','A'..'Z','_','0'..'9'] do inc(run);
      fTokenID:=tkDollarIdentifier;
    end;
    'a'..'z','A'..'Z':begin
      localId:=fLine[run];
      inc(run);
      while fLine[run] in ['a'..'z','A'..'Z','_','0'..'9','.'] do begin
        localId:=localId+fLine[run];
        inc(run);
      end;
      if (localId='xor') or (localId='or') or (localId='mod') or (localId='in') or (localId='div') or (localId='and') then fTokenId:=tkOperator
      else if (localId='true') or (localId='false') then fTokenId:=tkBoolean
      else if (localId='Nan') or (localId='Inf') then fTokenId:=tkNumber
      else if (localId='set') or (localId='each')
           or (localId='CACHE') or (localId='USE') then fTokenId:=tkIntrinsicRuleOrKeyword
      else begin
        idInfo:=ad_getIdInfo(localId);
        if      idInfo.isBuiltIn     then fTokenID:=tkIntrinsicRuleOrKeyword
        else if idInfo.isUserDefined then fTokenID:=tkUserRule
        else                              fTokenID:=tkIdentifier;
      end;
    end;
    '|','^','?','+','&','%','*','=','<','>': begin
      inc(run);
      fTokenID:=tkOperator;
    end;
    '-': begin
      inc(run);
      if fLine[run]='>' then begin
        inc(run);
        fTokenID:=tkDeclarationOp;
      end else fTokenID:=tkOperator;
    end;
    '/': begin
      inc(run);
      if fLine[run]='/' then begin
        while fLine[run+1]<>#0 do inc(run);
        fTokenId:=tkComment;
      end else fTokenId:=tkOperator;
    end;
    ':': begin
      inc(run);
      case fLine[run] of
        'b','e','i','l','n','s','r': begin
          localId:=':';
          while fLine[run] in ['a'..'z','A'..'Z','_','0'..'9'] do begin
            localId:=localId+fLine[run];
            inc(run);
          end;
          if (localId=':booleanList') or (localId=':boolean') or (localId=':expression') or (localId=':intList') or
             (localId=':int') or (localId=':list') or (localId=':numericList') or (localId=':numeric') or
             (localId=':stringList') or (localId=':scalar') or (localId=':string') or (localId=':realList') or (localId=':real')
          then fTokenID:=tkTypeCheck
          else fTokenID:=tkUnknown;
        end;
        '=': begin
          inc(run);
          fTokenID:=tkDeclarationOp;
        end;
        else fTokenID:=tkOperator;
      end;
    end;
    '"': begin
      inc(run);
      while (fLine[run]<>#0) and ((fLine[run]<>'"') or (fLine[run-1]='\')) do inc(run);
      if (fLine[run]='"') then inc(run);
      fTokenId:=tkString;
    end;
    else begin
      fTokenID:=tkUnknown;
      inc(Run);
    end;
  end;
end;

function TSynMnhSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynMnhSyn.GetRange: Pointer;
begin
  //Result := Pointer(PtrUInt(fRange));
  result:=nil;
end;

function TSynMnhSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  Result:='';
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynMnhSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

function TSynMnhSyn.GetTokenID: TtkTokenKind;
  begin
    Result := fTokenId;
  end;

function TSynMnhSyn.GetTokenAttribute: TSynHighlighterAttributes;
  VAR bg:longint;
  begin
    bg:=255;
    if isDeclInput then bg:=bg-40;
    if isExprInput then bg:=bg-20;
    if isOutput    then bg:=bg-10;
    result:=styleTable[fTokenID];
    result.Background:=(bg or (bg shl 8) or (bg shl 16));
  end;

function TSynMnhSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynMnhSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynMnhSyn.ResetRange;
begin
  fRange := rsUnknown;
  isDeclInput:=false;
  isExprInput:=false;
  isOutput:=false;
end;

procedure TSynMnhSyn.SetRange(Value: Pointer);
begin
  //expressionLevel:=ptrUInt(expressionLevel);
  //fRange := TRangeState(PtrUInt(Value));
end;

class function TSynMnhSyn.GetLanguageName: string;
begin
  Result := 'mnh';
end;

function TSynMnhSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['a'..'z','A'..'Z','.','_','0'..'9'];
end;

end.

