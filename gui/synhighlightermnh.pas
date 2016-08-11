UNIT SynHighlighterMnh;
INTERFACE

USES
  sysutils, Classes, FileUtil, Controls, Graphics,mnh_funcs,
  SynEditTypes, SynEditHighlighter, mnh_evalThread,mnh_constants,myGenerics,myStringUtil;

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
  T_tokenSubKind =(skNormal,skWarn,skError);
  T_mnhSynFlavour=(msf_input,msf_output,msf_guessing);

CONST tokenKindForMt:array[T_messageType] of T_tokenKind=(
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
{mt_el5_haltMessageQuiet,              } tkNote,
{mt_endOfEvaluation,                   } tkDefault,
{mt_reloadRequired,                    } tkNote,
{mt_timing_info                        } tkTimingNote,
{mt_plotFileCreated,                   } tkNote,
{mt_plotCreatedWithDeferredDisplay,    } tkNote,
{mt_plotCreatedWithInstantDisplay,     } tkNote,
{mt_plotSettingsChanged,               } tkNote,
{mt_evaluatedStatementInInteractiveMode} tkNote,
{mt_displayTable                       } tkNote,
{mt_guiPseudoPackageFound              } tkNote);

TYPE
  TSynMnhSyn = class(TSynCustomHighlighter)
  private
    //Initialized only
    flavour :T_mnhSynFlavour;
    styleTable: array[T_tokenKind,T_tokenSubKind] of TSynHighlighterAttributes;

    isMarked:boolean;
    fLine: PChar;

    blobEnder:char;
    run: longint;
    fTokenPos: integer;
    fTokenId: T_tokenKind;
    fTokenSubId: T_tokenSubKind;
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
  VAR t:T_tokenKind;
      s:T_tokenSubKind;
  begin
    inherited create(AOwner);
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
      styleTable[tkHashVariable    ,s]:=TSynHighlighterAttributes.create('HashVariable');

      styleTable[tkComment         ,s].style:=[fsItalic];
      styleTable[tkDocComment      ,s].style:=[fsItalic,fsBold];
      styleTable[tkSpecialComment  ,s].style:=[fsItalic,fsBold,fsUnderline];
      styleTable[tkDollarIdentifier,s].style:=[fsItalic];
      styleTable[tkBultinRule      ,s].style:=[fsBold];
      styleTable[tkSpecialRule     ,s].style:=[fsBold];
      styleTable[tkOperator        ,s].style:=[fsBold];
      styleTable[tkModifier        ,s].style:=[fsBold];
      styleTable[tkHashVariable    ,s].style:=[fsBold];
      styleTable[tkError           ,s].style:=[fsBold];
      styleTable[tkWarning         ,s].style:=[fsBold];
      styleTable[tkNote            ,s].style:=[fsBold];
      styleTable[tkTimingNote      ,s].style:=[fsBold];

      styleTable[tkComment         ,s].foreground:=$00999999;
      styleTable[tkDocComment      ,s].foreground:=$00999999;
      styleTable[tkSpecialComment  ,s].foreground:=$00999999;
      styleTable[tkDefault         ,s].foreground:=$00000000;
      styleTable[tkDollarIdentifier,s].foreground:=$00000000;
      styleTable[tkUserRule        ,s].foreground:=$00FF0000;
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
      styleTable[tkHashVariable    ,s].foreground:=$00FF0000;
      styleTable[tkHashVariable    ,s].background:=$00FFEEDD;
    end;
    for t:=low(T_tokenKind) to high(T_tokenKind) do begin
      styleTable[t,skWarn].style:=styleTable[t,skWarn].style+[fsUnderline];
      styleTable[t,skError].style:=styleTable[t,skError].style+[fsUnderline];
      styleTable[t,skError].background:=$0000FFFF;
    end;
    markedWord:='';
    setMarkedToken(-1,-1);
  end; { Create }

DESTRUCTOR TSynMnhSyn.destroy;
  VAR t: T_tokenKind;
      s: T_tokenSubKind;
  begin
    for t:=low(T_tokenKind)    to high(T_tokenKind) do
    for s:=low(T_tokenSubKind) to high(T_tokenSubKind) do styleTable[t,s].destroy;
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
    fTokenSubId:=skNormal;
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
        if intrinsicRules.contains(localId) then fTokenId := tkBultinRule
        else if codeAssistant.isUserRule(localId) then fTokenId := tkUserRule
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
    if (flavour=msf_input) then begin
      if codeAssistant.isErrorLocation  (fLineNumber,fTokenPos,run) then fTokenSubId:=skError else
      if codeAssistant.isWarningLocation(fLineNumber,fTokenPos,run) then fTokenSubId:=skWarn;
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
    SetString(result, (fLine+fTokenPos), len);
  end;

PROCEDURE TSynMnhSyn.GetTokenEx(OUT TokenStart: PChar; OUT TokenLength: integer);
  begin
    TokenLength := run-fTokenPos;
    TokenStart := fLine+fTokenPos;
  end;

FUNCTION TSynMnhSyn.GetTokenAttribute: TSynHighlighterAttributes;
  begin
    result := styleTable [fTokenId,fTokenSubId];
    if isMarked then result.FrameColor:=$000000ff
                else begin
                  result.FrameColor:=clNone;
                  if (blobEnder<>#0) and (fTokenId<>tkSpecialComment) then result:=styleTable[tkString,skNormal];
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
