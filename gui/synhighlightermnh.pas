UNIT SynHighlighterMnh;

{I SynEdit.inc}

INTERFACE

USES
  sysutils, Classes, FileUtil, Controls, Graphics,
  SynEditTypes, SynEditHighlighter, mnh_evalThread,mnh_constants,myGenerics,myStringUtil;

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
    tkHashVariable);
  T_mnhSynFlavour=(msf_input,msf_output,msf_debugger,msf_guessing);

TYPE
  { TSynMnhSyn }
  T_contextStackElement=(cse_bottom,cse_expression,cse_blobstring);
  T_contextStack=object
    private
      dat:array of record
        e:T_contextStackElement;
        endsWith:char;
      end;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE push(CONST e:T_contextStackElement; CONST ender:char);
      PROCEDURE pop(CONST e:T_contextStackElement);
      FUNCTION top:T_contextStackElement;
      FUNCTION topEnder:char;
  end;

  TSynMnhSyn = class(TSynCustomHighlighter)
  private
    contextStack:T_contextStack;

    isMarked:boolean;
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
    class FUNCTION GetLanguageName: ansistring; override;
  public
    CONSTRUCTOR create(AOwner: TComponent; CONST flav:T_mnhSynFlavour);
    DESTRUCTOR destroy; override;
    FUNCTION GetDefaultAttribute(index: integer): TSynHighlighterAttributes; override;
    FUNCTION GetEol: boolean; override;
    FUNCTION getRange: pointer; override;
    FUNCTION getToken: ansistring; override;
    PROCEDURE GetTokenEx(OUT TokenStart: PChar; OUT TokenLength: integer); override;
    FUNCTION GetTokenAttribute: TSynHighlighterAttributes; override;
    FUNCTION GetTokenKind: integer; override;
    FUNCTION GetTokenPos: integer; override;
    PROCEDURE next; override;
    PROCEDURE ResetRange; override;
    PROCEDURE setRange(value: pointer); override;
    PROCEDURE SetLine(CONST newValue: ansistring; LineNumber: integer); override;
    FUNCTION setMarkedWord(CONST s:ansistring):boolean;
    PROCEDURE setMarkedToken(CONST line,column:longint);
  end;

IMPLEMENTATION
VAR modifierStrings:T_listOfString;
    operatorStrings:T_listOfString;
    specialLiteralStrings:T_listOfString;
    specialConstructStrings:T_listOfString;

CONSTRUCTOR T_contextStack.create; begin clear; end;
DESTRUCTOR T_contextStack.destroy; begin clear; end;
PROCEDURE T_contextStack.clear; begin setLength(dat,0); end;
PROCEDURE T_contextStack.push(CONST e: T_contextStackElement; CONST ender: char);
  begin
    setLength(dat,length(dat)+1);
    dat[length(dat)-1].e:=e;
    dat[length(dat)-1].endsWith:=ender;
  end;

PROCEDURE T_contextStack.pop(CONST e: T_contextStackElement);
  begin
    if (length(dat)>0) and (dat[length(dat)-1].e=e) then setLength(dat,length(dat)-1);
  end;

FUNCTION T_contextStack.top: T_contextStackElement;
  begin
    if (length(dat)>0) then result:=dat[length(dat)-1].e
                       else result:=cse_bottom;
  end;

FUNCTION T_contextStack.topEnder: char;
  begin
    if (length(dat)>0) and (dat[length(dat)-1].e=cse_blobstring)
                      then result:=dat[length(dat)-1].endsWith
                      else result:=#0;
  end;

CONSTRUCTOR TSynMnhSyn.create(AOwner: TComponent; CONST flav:T_mnhSynFlavour);
  begin
    inherited create(AOwner);
    contextStack.create;
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
    styleTable[tkHashVariable    ]:=TSynHighlighterAttributes.create('HashVariable');

    styleTable[tkComment         ].style:=[fsItalic];
    styleTable[tkDocComment      ].style:=[fsItalic,fsBold];
    styleTable[tkSpecialComment  ].style:=[fsItalic,fsBold,fsUnderline];
    styleTable[tkDollarIdentifier].style:=[fsItalic];
    styleTable[tkBultinRule      ].style:=[fsBold];
    styleTable[tkSpecialRule     ].style:=[fsBold];
    styleTable[tkOperator        ].style:=[fsBold];
    styleTable[tkModifier        ].style:=[fsBold];
    styleTable[tkHashVariable    ].style:=[fsBold];

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
    styleTable[tkHashVariable    ].foreground:=$00FF0000;
    styleTable[tkHashVariable    ].background:=$00FFEEDD;

    markedWord:='';
    setMarkedToken(-1,-1);
  end; { Create }

DESTRUCTOR TSynMnhSyn.destroy;
  VAR tk: TtkTokenKind;
  begin
    for tk := low(TtkTokenKind) to high(TtkTokenKind) do styleTable[tk].destroy;
    contextStack.destroy;
    inherited destroy;
  end; { Destroy }

FUNCTION TSynMnhSyn.GetDefaultAttribute(index: integer): TSynHighlighterAttributes;
  begin
    result := styleTable [tkDefault];
  end;

PROCEDURE TSynMnhSyn.SetLine(CONST newValue: ansistring; LineNumber: integer);
  begin
    inherited;
    fLine := PChar(newValue);
    run := 0;
    fLineNumber := LineNumber;
    ResetRange;
    if LineNumber=0 then contextStack.clear;
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

  FUNCTION continuesWith(CONST part:shortString; CONST offset:longint):boolean;
    VAR k:longint;
    begin
      result:=length(part)>0;
      for k:=1 to length(part) do if fLine[k-1+offset]<>part[k] then exit(false);
    end;

  FUNCTION startsWith(CONST prefix:shortString):boolean;
    begin
      result:=continuesWith(prefix,0);
    end;

  begin
    isMarked:=false;
    fTokenId := tkDefault;
    fTokenPos := run;
    if (run = 0) then begin
      if flavour in [msf_output,msf_guessing] then begin
        specialLineCase:=mt_clearConsole;
        i:=-1;
        for lc:=low(T_messageType) to high(T_messageType) do if startsWith(C_errorLevelTxt[lc]) then begin
          specialLineCase:=lc;
          i:=length(C_errorLevelTxt[lc]);
        end;
        if (flavour=msf_guessing) and (specialLineCase=mt_clearConsole) then begin
          i:=0;
          while (fLine[i]<>#0) and (fLine[i]<>';') do inc(i);
          if fLine[i]=';' then specialLineCase:=mt_echo_input;
          i:=-1;
        end;
        if i>=0 then run:=i+1;
        if C_errorLevelForMessageType[specialLineCase]>=3 then fTokenId:=tkError
                                                          else fTokenId:=tkDefault;
        if not(specialLineCase in [mt_echo_output,mt_echo_declaration,mt_echo_input,mt_debug_step]) then while (fLine[run]<>#0) do inc(run);
        if run>0 then exit;
      end;
    end;
    if contextStack.top=cse_blobstring then begin
      if fLine[run]=#0 then begin
        fTokenId := tkNull;
        exit;
      end;
      while (fLine[run]<>#0) and (fLine[run]<>contextStack.topEnder) do inc(run);
      if fLine[run]=contextStack.topEnder then begin
        contextStack.pop(cse_blobstring);
        inc(run);
      end;
      fTokenId:=tkString;
    end else case fLine [run] of
      #0: fTokenId := tkNull;
      '{': begin
             if contextStack.top in [cse_bottom,cse_expression] then contextStack.push(cse_expression,'}');
             inc(run);
             fTokenId := tkDefault;
           end;
      '}': begin
             contextStack.pop(cse_expression);
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
        else if modifierStrings        .contains(localId) or
                (fLineNumber=0) and (localId='USE') and (flavour=msf_input) then fTokenId := tkModifier
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
          if fLine[run]='!' then begin
            fTokenId:=tkSpecialComment;
            contextStack.push(cse_blobstring,fLine[run+1]);
            inc(run,2);
            exit;
          end
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
      '#': if flavour=msf_debugger then begin
        inc(run);
        if fLine[run] in ['0'..'9'] then begin
          fTokenId := tkHashVariable;
          while fLine [run] in ['0'..'9'] do inc(run);
        end else fTokenId:=tkOperator;
      end else begin
        fTokenId := tkDefault;
        inc(run);
      end
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

FUNCTION TSynMnhSyn.getToken: ansistring;
  VAR len: longint;
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
                else begin
                  result.FrameColor:=clNone;
                  if (contextStack.top=cse_expression) or (fTokenId=tkSpecialComment)
                     then result.style:=result.style + [fsUnderline]
                     else result.style:=result.style - [fsUnderline];
                  if (contextStack.top=cse_blobstring) and (fTokenId<>tkSpecialComment) then result:=styleTable[tkString];
                end;
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
  end;

PROCEDURE TSynMnhSyn.setRange(value: pointer);
  begin
    ResetRange;
  end;

class FUNCTION TSynMnhSyn.GetLanguageName: ansistring;
  begin
    result := 'MNH';
  end;

FUNCTION TSynMnhSyn.GetIdentChars: TSynIdentChars;
  begin
    result := IDENTIFIER_CHARS;
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
