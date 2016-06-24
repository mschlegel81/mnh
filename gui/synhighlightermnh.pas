UNIT SynHighlighterMnh;
INTERFACE

USES
  sysutils, Classes, FileUtil, Controls, Graphics,mnh_funcs,
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
    tkWarning,
    tkNote,
    tkTimingNote,
    tkHighlightedItem,
    tkHashVariable);
  T_mnhSynFlavour=(msf_input,msf_output,msf_guessing);

CONST tokenKindForMt:array[T_messageType] of TtkTokenKind=(
{mt_clearConsole,                      } tkDefault,
{mt_printline,                         } tkDefault,
{mt_echo_input,                        } tkDefault,
{mt_echo_declaration,                  } tkDefault,
{mt_echo_output,                       } tkDefault,
{mt_echo_continued,                    } tkDefault,
{mt_el1_note,                          } tkNote,
{mt_el2_warning,                       } tkWarning,
{mt_el3_evalError,                     } tkError,
{mt_el3_noMatchingMain,                } tkError,
{mt_el3_stackTrace,                    } tkError,
{mt_el3_userDefined,                   } tkError,
{mt_el4_parsingError,                  } tkError,
{mt_el5_systemError,                   } tkError,
{mt_el5_haltMessageReceived,           } tkError,
{mt_endOfEvaluation,                   } tkDefault,
{mt_reloadRequired,                    } tkNote,
{mt_timing_info                        } tkTimingNote,
{mt_plotFileCreated,                   } tkNote,
{mt_plotCreatedWithDeferredDisplay,    } tkNote,
{mt_plotCreatedWithInstantDisplay,     } tkNote,
{mt_plotSettingsChanged,               } tkNote,
{mt_evaluatedStatementInInteractiveMode} tkNote,
{mt_displayTable                       } tkNote);

TYPE
  { TSynMnhSyn }
  TSynMnhSyn = class(TSynCustomHighlighter)
  private
    blobEnder:char;

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
    {$WARN 5024 OFF}
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

PROCEDURE initLists;
IMPLEMENTATION
VAR listsAreInitialized:boolean=false;
    modifierStrings:T_listOfString;
    intrinsicRules:T_listOfString;
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
    styleTable[tkWarning         ]:=TSynHighlighterAttributes.create('Warn');
    styleTable[tkNote            ]:=TSynHighlighterAttributes.create('Note');
    styleTable[tkTimingNote      ]:=TSynHighlighterAttributes.create('Time');
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
    styleTable[tkError           ].style:=[fsBold];
    styleTable[tkWarning         ].style:=[fsBold];
    styleTable[tkNote            ].style:=[fsBold];
    styleTable[tkTimingNote      ].style:=[fsBold];

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
    styleTable[tkWarning         ].foreground:=$000000ff;
    styleTable[tkTimingNote      ].background:=$00EEEEEE;
    styleTable[tkHashVariable    ].foreground:=$00FF0000;
    styleTable[tkHashVariable    ].background:=$00FFEEDD;

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

PROCEDURE TSynMnhSyn.setMarkedToken(CONST line,column:longint);
  begin
    markedToken.line:=line;
    markedToken.column:=column;
  end;

PROCEDURE TSynMnhSyn.next;
  CONST RUN_LIMIT=10000;
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
    if run>=RUN_LIMIT then begin
      fTokenId:=tkNull;
      exit;
    end;
    if (run = 0) and (flavour in [msf_output,msf_guessing]) then begin
      specialLineCase:=mt_clearConsole;
      i:=-1;
      for lc:=low(T_messageType) to high(T_messageType) do if (C_errorLevelTxt[lc]<>'') and startsWith(UTF8_ZERO_WIDTH_SPACE+C_errorLevelTxt[lc]) then begin
        specialLineCase:=lc;
        i:=length(UTF8_ZERO_WIDTH_SPACE+C_errorLevelTxt[lc]);
      end;
      if (flavour=msf_guessing) and (specialLineCase=mt_clearConsole) then begin
        i:=0;
        while (fLine[i]<>#0) and (fLine[i]<>';') do inc(i);
        if fLine[i]=';' then specialLineCase:=mt_echo_input;
        i:=-1;
      end;
      if i>=0 then run:=i+1;
      fTokenId:=tokenKindForMt[specialLineCase];
      if not(specialLineCase in [mt_echo_output,mt_echo_declaration,mt_echo_input,mt_echo_continued]) then while (run<RUN_LIMIT) and (fLine[run]<>#0) do inc(run);
      if run>0 then exit;
    end;
    if blobEnder<>#0 then begin
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
      '|', '^', '?', '+', '&', '*', '@', '.': begin
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
          if fLine[run]='!' then begin
            fTokenId:=tkSpecialComment;
            blobEnder:=fLine[run+1];
            if blobEnder=#0 then begin
              blobEnder:='''';
              inc(run);
            end else inc(run,2);
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
            if (localId=C_tokenInfo[tt_typeCheckScalar      ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckList        ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckBoolean     ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckBoolList    ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckInt         ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckIntList     ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckReal        ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckRealList    ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckString      ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckStringList  ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckNumeric     ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckNumList     ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckExpression  ].defaultId) or
               (localId=C_tokenInfo[tt_typeCheckKeyValueList].defaultId) then begin
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
      '#': begin
        fTokenId := tkDefault;
        inc(run);
      end
      else begin
        fTokenId := tkDefault;
        inc(run);
      end;
    end;
    if (fLineNumber=markedToken.line) and (fTokenPos<=markedToken.column) and (run>markedToken.column) then fTokenId:=tkHighlightedItem;
    if (flavour=msf_input) and (fTokenId<>tkNull) and docEvaluator.isErrorLocation(fLineNumber,fTokenPos,run)
    then fTokenId:=tkError;
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
                  if (blobEnder<>#0) and (fTokenId<>tkSpecialComment) then result:=styleTable[tkString];
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
  VAR tt:T_tokenType;
      i:longint;

  begin
    operatorStrings.create;
    modifierStrings.create;
    specialConstructStrings.create;
    specialLiteralStrings.create;
    intrinsicRules.create;
    for tt:=low(T_tokenType) to high(T_tokenType) do begin
      case C_tokenInfo[tt].reservedWordClass of
        rwc_typeCheck,
        rwc_operator:         operatorStrings        .add(C_tokenInfo[tt].defaultId);
        rwc_specialLiteral:   specialLiteralStrings  .add(C_tokenInfo[tt].defaultId);
        rwc_specialConstruct: specialConstructStrings.add(replaceAll(C_tokenInfo[tt].defaultId,'.',''));
        rwc_modifier:         modifierStrings        .add(C_tokenInfo[tt].defaultId);
      end;
    end;
    for i:=1 to high(C_specialWordInfo) do begin
      case C_specialWordInfo[i].reservedWordClass of
        rwc_typeCheck,
        rwc_operator:         operatorStrings        .add(C_specialWordInfo[i].txt);
        rwc_specialLiteral:   specialLiteralStrings  .add(C_specialWordInfo[i].txt);
        rwc_specialConstruct: specialConstructStrings.add(C_specialWordInfo[i].txt);
        rwc_modifier:         modifierStrings        .add(C_specialWordInfo[i].txt);
      end;
    end;
    intrinsicRules.addAll(intrinsicRuleMap.keySet);

    operatorStrings.unique;
    modifierStrings.unique;
    specialConstructStrings.unique;
    specialLiteralStrings.unique;
    intrinsicRules.unique;
    listsAreInitialized:=true;
  end;

FINALIZATION
  if listsAreInitialized then begin
    operatorStrings.destroy;
    modifierStrings.destroy;
    specialConstructStrings.destroy;
    specialLiteralStrings.destroy;
    intrinsicRules.destroy;
  end;
end.
