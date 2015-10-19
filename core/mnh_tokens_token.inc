{$ifdef include_interface}
  P_token=^T_token;
  T_token=packed object
    next    :P_token;
    location:T_tokenLocation;
    txt     :shortString;
    tokType :T_tokenType;
    data    :pointer;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer);
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType);
    PROCEDURE define(CONST original:T_token);
    PROCEDURE undefine;
    FUNCTION last:P_token;
    FUNCTION toString(CONST lastWasIdLike:boolean; OUT idLike:boolean):ansistring;
    FUNCTION singleTokenToString:ansistring;
    FUNCTION getDeclarationOrAssignmentToken:P_token;
  end;

  T_tokenArray=object
    T:array of T_token;
    i:longint;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE append(CONST newTok:T_token);
    PROCEDURE step(CONST package:P_package; VAR lastComment:ansistring);
    FUNCTION current:T_token;
    PROCEDURE mutateCurrentTokType(CONST newTokType:T_tokenType);
    FUNCTION atEnd:boolean;
  end;

{$endif}
{$ifdef include_implementation}
FUNCTION tokensToString(CONST first:P_token):ansistring;
  VAR p:P_token;
      idLike,prevIdLike:boolean;
  begin
    prevIdLike:=false;
    result:='';
    p:=first;
    while p<>nil do begin
      result:=result+p^.toString(prevIdLike,idLike);
      prevIdLike:=idLike;
      p:=p^.next;
    end;
  end;

FUNCTION safeTokenToString(CONST t:P_token):ansistring;
  begin
    if t=nil then result:='<EOL>'
    else result:=t^.singleTokenToString;
  end;

CONSTRUCTOR T_token.create;
  begin
  end;

DESTRUCTOR T_token.destroy;
  begin
    undefine;
  end;

PROCEDURE T_token.define(CONST tokenLocation:T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer);
  begin
    location:=tokenLocation;
    if (tokenText='') or (C_tokenString[tokenType]<>'')
      then txt:=C_tokenString[tokenType]
      else txt:=tokenText;
    tokType:=tokenType;
    data:=ptr;
  end;

PROCEDURE T_token.define(CONST tokenLocation:T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType);
  begin
    location:=tokenLocation;
    if tokenText='' then txt:=C_tokenString[tokenType]
                    else txt:=tokenText;
    tokType:=tokenType;
    data:=nil;
  end;

PROCEDURE T_token.define(CONST original:T_token);
  begin
    location:=original.location;
    txt     :=original.txt;
    tokType :=original.tokType;
    data    :=original.data;
    if (tokType in [tt_literal,tt_aggregatorExpressionLiteral,tt_parList]) or (tokType in [tt_each,tt_parallelEach]) and (data<>nil) then P_literal(data)^.rereference
    else if (tokType in [tt_list_constructor,tt_parList_constructor]) then data:=P_listLiteral(original.data)^.clone;
  end;

PROCEDURE T_token.undefine;
  begin
    case tokType of
      tt_literal,tt_aggregatorExpressionLiteral,tt_list_constructor,tt_parList_constructor,tt_parList: disposeLiteral(data);
      tt_each,tt_parallelEach: if data<>nil then disposeLiteral(data);
      else data:=nil;
    end;
    tokType:=tt_EOL;
    location:=C_nilTokenLocation;
  end;

FUNCTION T_token.last:P_token;
  begin
    result:=@self;
    while result^.next<>nil do result:=result^.next;
  end;

FUNCTION T_token.toString(CONST lastWasIdLike:boolean; OUT idLike:boolean):ansistring;
  begin
    idLike:=false;
    case tokType of
      tt_each, tt_parallelEach: begin
        result:=C_tokenString[tokType];
        if txt<>'' then result:=result+'('+txt+',';
        if data<>nil then result:=result+P_literal(data)^.toString+',';
      end;
      tt_aggregatorExpressionLiteral,
      tt_literal            : result:=P_literal(data)^.toString;
      tt_parList_constructor: result:=P_listLiteral(data)^.toParameterListString(false);
      tt_parList            : result:=P_listLiteral(data)^.toParameterListString(true);
      tt_list_constructor   : result:=P_listLiteral(data)^.listConstructorToString;
      tt_assignNewBlockLocal: result:=C_tokenString[tt_modifier_local]+' '+txt+C_tokenString[tokType];
      tt_mutate, tt_assignExistingBlockLocal, tt_cso_assignPlus..tt_cso_assignAppend: result:=txt+C_tokenString[tokType];
      tt_identifier_pon,
      tt_localUserRulePointer_pon,
      tt_importedUserRulePointer_pon,
      tt_intrinsicRulePointer_pon: result:='.'+txt;
      tt_identifier,
      tt_localUserRulePointer,
      tt_importedUserRulePointer,
      tt_intrinsicRulePointer,
      tt_rulePutCacheValue,
      tt_parameterIdentifier,
      tt_blockLocalVariable: result:=txt;
      else result:=C_tokenString[tokType];
    end;
    if length(result)<1 then begin
      idLike:=false; exit(result);
    end;
    if lastWasIdLike and (result[1] in ['a'..'z','A'..'Z','?',':','0'..'9'])
      or (tokType in [tt_operatorAnd,tt_operatorDivInt,tt_operatorIn,tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorMod,tt_operatorOr,tt_operatorXor,tt_iifCheck,tt_iifElse])
    then result:=' '+result;
    idLike:=(result[length(result)] in ['a'..'z','A'..'Z','?',':','_']);
  end;

FUNCTION T_token.singleTokenToString:ansistring;
  VAR dummy:boolean;
  begin
    result:=toString(false,dummy);
  end;

FUNCTION T_token.getDeclarationOrAssignmentToken:P_token;
  VAR scopeLevel:longint=0;
      t:P_token;
  begin
    result:=nil;
    t:=@self;
    while t<>nil do begin
      case t^.tokType of
        tt_declare,tt_assign: if scopeLevel=0 then exit(t);
        tt_procedureBlockBegin,tt_braceOpen ,tt_expBraceOpen ,tt_listBraceOpen:  inc(scopeLevel);
        tt_procedureBlockEnd  ,tt_braceClose,tt_expBraceClose,tt_listBraceClose: dec(scopeLevel);
      end;
      t:=t^.next;
    end;
  end;

CONSTRUCTOR T_tokenArray.create;
  begin
    setLength(t,0);
    i:=-1;
  end;

DESTRUCTOR T_tokenArray.destroy;
  begin
    setLength(t,0);
    i:=-1;
  end;

PROCEDURE T_tokenArray.append(CONST newTok:T_token);
  begin
    setLength(t,length(t)+1);
    t[length(t)-1]:=newTok;
  end;

PROCEDURE T_tokenArray.step(CONST package:P_package; VAR lastComment:ansistring);
  begin
    repeat
      inc(i);
      if (i<length(t)) and
         (t[i].tokType=tt_EOL) then begin
        if (t[i].txt<>'') then lastComment:=t[i].txt;
      end;
      //resolve ambiguous notation "x.y" to qualified identifier "x.y" or "x .y" (pseudo-object-notation)
      if (i<length(t)-1) and
         (t[i].tokType                   =tt_identifier) and
         (t[i+1].tokType=tt_identifier_pon) and
         package^.isImportedOrBuiltinPackage(t[i].txt) then begin
        inc(i);
        t[i].txt:=t[i-1].txt+'.'+t[i].txt;
        t[i].tokType:=tt_identifier;
      end;
      if (i<length(t)-1) and
         (t[i].tokType=tt_listBraceOpen ) and
         (t[i+1].tokType=tt_listBraceClose) then begin
        inc(i);
        t[i].tokType:=tt_literal;
        t[i].data:=newListLiteral;
      end;
      if (i<length(t)-3) and
         (t[i].tokType                   in [tt_each,tt_parallelEach]) and
         (t[i+1].tokType=tt_braceOpen) and
         (t[i+2].tokType in [tt_identifier,tt_localUserRulePointer,tt_importedUserRulePointer,tt_intrinsicRulePointer]) and
         (t[i+3].tokType=tt_separatorComma) then begin
        t[i+3].tokType:=t[i].tokType;
        t[i+3].txt:=t[i+2].txt;
        t[i+3].data:=nil;
        inc(i,3); //skip tt_braceOpen, identifier and comma
      end;
    until (i>=length(t)) or
          (t[i].tokType<>tt_EOL);
  end;

FUNCTION T_tokenArray.current:T_token;
  begin
    result:=t[i];
  end;

PROCEDURE T_tokenArray.mutateCurrentTokType(CONST newTokType:T_tokenType);
  begin
    t[i].tokType:=newTokType;
  end;

FUNCTION T_tokenArray.atEnd:boolean;
  begin
    result:=i>=length(t);
  end;


FUNCTION firstToken(VAR line:ansistring; VAR lineLocation:T_tokenLocation; CONST inPackage:P_package; CONST inEvaluationLoop:boolean):T_token;
  VAR parsedLength:longint=0;

  PROCEDURE fail(CONST message:ansistring);
    begin
      if inEvaluationLoop then begin
        raiseError(el4_parsingError,message,lineLocation);
      end else parsedLength:=length(line);
    end;

  FUNCTION leadingId(CONST allowQualified:boolean):ansistring;
    VAR i:longint;
    begin
      i:=1;
      while (i<length(line)) and (line[i+1] in ['a'..'z','A'..'Z','0'..'9','_',C_ID_QUALIFY_CHARACTER]) do begin
        inc(i);
        if line[i]=C_ID_QUALIFY_CHARACTER then begin
          if (i<length(line)) and (line[i+1]=C_ID_QUALIFY_CHARACTER) then begin
            parsedLength:=i-1;
            exit(copy(line,1,i-1));
          end else if not(allowQualified) then begin
            parsedLength:=i-1;
            exit(copy(line,1,i-1));
          end;
        end;
      end;
      parsedLength:=i;
      result:=copy(line,1,i);
    end;

  PROCEDURE removeLeadingBlanks();
    VAR i:longint;
    begin
      i:=1;
      while (i<=length(line)) and (line[i] in [' ',C_lineBreakChar,C_tabChar,C_carriageReturnChar]) do inc(i);
      if i>1 then line:=copy(line,i,length(line)+1-i);
      inc(lineLocation.column,i-1);
    end;

  VAR id:ansistring;
      tt:T_tokenType;
  begin
    removeLeadingBlanks;
    result.location:=lineLocation;
    result.txt:='';
    result.tokType:=tt_EOL;
    result.data:=nil;
    result.next:=nil;
    if length(line)<1 then exit(result);
    case line[1] of
      '0'..'9': begin
        result.data:=parseNumber(line,not(inEvaluationLoop),parsedLength);
        if parsedLength<=0 then fail('Cannot parse numeric literal '+line)
                           else result.tokType:=tt_literal;
      end;
      '"','''': begin
        id:=unescapeString(line,parsedLength);
        if parsedLength=0 then fail('Cannot parse string literal '+line)
        else begin
          result.tokType:=tt_literal;
          if inEvaluationLoop then result.data:=newStringLiteral(id);
        end;
      end;
      '$': begin
        result.txt:=leadingId(false);
        parsedLength:=length(result.txt);
        result.tokType:=tt_parameterIdentifier;
      end;
      'a'..'z','A'..'Z': begin
        id:=leadingId(false);

        //parsedLength:=length(id);
        result.tokType:=tt_identifier;
        for tt:=low(T_tokenType) to high(T_tokenType) do
        if id=C_tokenString[tt] then begin
          result.tokType:=tt;
          result.data:=nil;
          result.txt:='';
        end;
        if result.tokType=tt_identifier then begin
          if      id=C_boolText[true]  then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newBoolLiteral(true); end
          else if id=C_boolText[false] then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newBoolLiteral(false); end
          else if id=C_nanText         then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newRealLiteral(Nan); end
          else if id=C_infText         then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newRealLiteral(Infinity); end
          else if id=C_voidText        then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newVoidLiteral; end
          else begin
            result.txt:=id;
            result.data:=inPackage;
            if not(inEvaluationLoop) then inPackage^.resolveRuleId(result,true);
          end;
        end;
      end;
      '/': if startsWith(line,'//') then begin //comments
        parsedLength:=2;
        while (parsedLength<length(line)) and not(line[parsedLength+1] in [C_lineBreakChar,C_carriageReturnChar]) do inc(parsedLength);
        result.tokType:=tt_EOL;
        if      startsWith(line,DOC_COMMENT_PREFIX) then result.txt:=trim(copy(line,length(DOC_COMMENT_PREFIX)+1,length(line)-length(DOC_COMMENT_PREFIX)))
        else if startsWith(line,SPECIAL_COMMENT_BLOB_BEGIN)  then result.txt:=SPECIAL_COMMENT_BLOB_BEGIN
        else if startsWith(line,SPECIAL_COMMENT_BLOB_END) then result.txt:=SPECIAL_COMMENT_BLOB_END;
      end else if startsWith(line,C_tokenString[tt_cso_assignDiv]) then begin
        parsedLength:=2;
        result.tokType:=tt_cso_assignDiv;
      end else begin
        parsedLength:=1;
        result.tokType:=tt_operatorDivReal;
      end;
      '.': if startsWith(line,C_tokenString[tt_optionalParameters]) then begin
        parsedLength:=length(C_tokenString[tt_optionalParameters]);
        result.tokType:=tt_optionalParameters;
      end else if startsWith(line,C_tokenString[tt_separatorCnt]) then begin
        parsedLength:=length(C_tokenString[tt_separatorCnt]);
        result.tokType:=tt_separatorCnt;
      end else if (length(line)>=2) and (line[2] in ['a'..'z','A'..'Z']) then begin
        id:=copy(leadingId(false),2,length(line));
        parsedLength:=1+length(id);
        result.txt:=id;
        result.tokType:=tt_identifier_pon;
        result.data:=inPackage;
        if not(inEvaluationLoop) then inPackage^.resolveRuleId(result,true);
      end else fail('Cannot parse: '+line);
      ':': if startsWith(line,':=') then begin
        parsedLength:=2;
        result.tokType:=tt_assign;
      end else if (length(line)>=4) and (line[2] in ['b','e','i','l','n','s','r','k']) then begin
        id:=leadingId(false);
        parsedLength:=length(id);
        if      id=C_tokenString[tt_typeCheckBoolList  ] then result.tokType:=tt_typeCheckBoolList
        else if id=C_tokenString[tt_typeCheckBoolean   ] then result.tokType:=tt_typeCheckBoolean
        else if id=C_tokenString[tt_typeCheckExpression] then result.tokType:=tt_typeCheckExpression
        else if id=C_tokenString[tt_typeCheckIntList   ] then result.tokType:=tt_typeCheckIntList
        else if id=C_tokenString[tt_typeCheckInt       ] then result.tokType:=tt_typeCheckInt
        else if id=C_tokenString[tt_typeCheckList      ] then result.tokType:=tt_typeCheckList
        else if id=C_tokenString[tt_typeCheckNumList   ] then result.tokType:=tt_typeCheckNumList
        else if id=C_tokenString[tt_typeCheckNumeric   ] then result.tokType:=tt_typeCheckNumeric
        else if id=C_tokenString[tt_typeCheckStringList] then result.tokType:=tt_typeCheckStringList
        else if id=C_tokenString[tt_typeCheckScalar    ] then result.tokType:=tt_typeCheckScalar
        else if id=C_tokenString[tt_typeCheckString    ] then result.tokType:=tt_typeCheckString
        else if id=C_tokenString[tt_typeCheckRealList  ] then result.tokType:=tt_typeCheckRealList
        else if id=C_tokenString[tt_typeCheckReal      ] then result.tokType:=tt_typeCheckReal
        else if id=C_tokenString[tt_typeCheckKeyValueList] then result.tokType:=tt_typeCheckKeyValueList
        else begin
          parsedLength:=1;
          result.tokType:=tt_iifElse;
        end;
      end else begin
        parsedLength:=1;
        result.tokType:=tt_iifElse;
      end;
      ';':                                                                  begin parsedLength:=1; result.tokType:=tt_semicolon;             end;
      '}':                                                                  begin parsedLength:=1; result.tokType:=tt_expBraceClose;         end;
      '{':                                                                  begin parsedLength:=1; result.tokType:=tt_expBraceOpen;          end;
      '^':                                                                  begin parsedLength:=1; result.tokType:=tt_operatorPot;           end;
      ']':                                                                  begin parsedLength:=1; result.tokType:=tt_listBraceClose;        end;
      '[':                                                                  begin parsedLength:=1; result.tokType:=tt_listBraceOpen;         end;
      '?':                                                                  begin parsedLength:=1; result.tokType:=tt_iifCheck;              end;
      ',':                                                                  begin parsedLength:=1; result.tokType:=tt_separatorComma;        end;
      '@':                                                                  begin parsedLength:=1; result.tokType:=tt_listToParameterList;   end;
      ')':                                                                  begin parsedLength:=1; result.tokType:=tt_braceClose;            end;
      '(':                                                                  begin parsedLength:=1; result.tokType:=tt_braceOpen;             end;
      '|': if startsWith(line,C_tokenString[tt_cso_assignAppend])      then begin parsedLength:=2; result.tokType:=tt_cso_assignAppend;      end
                                                                       else begin parsedLength:=1; result.tokType:=tt_operatorConcat;        end;
      '+': if startsWith(line,C_tokenString[tt_cso_assignPlus])        then begin parsedLength:=2; result.tokType:=tt_cso_assignPlus;        end
                                                                       else begin parsedLength:=1; result.tokType:=tt_operatorPlus;          end;
      '&': if startsWith(line,C_tokenString[tt_cso_assignStrConcat])   then begin parsedLength:=2; result.tokType:=tt_cso_assignStrConcat;   end
                                                                       else begin parsedLength:=1; result.tokType:=tt_operatorStrConcat;     end;
      '-': if startsWith(line,C_tokenString[tt_declare])               then begin parsedLength:=2; result.tokType:=tt_declare;               end else
           if startsWith(line,C_tokenString[tt_cso_assignMinus])       then begin parsedLength:=2; result.tokType:=tt_cso_assignMinus;       end
                                                                       else begin parsedLength:=1; result.tokType:=tt_operatorMinus;         end;
      '*': if startsWith(line,'**')                                    then begin parsedLength:=2; result.tokType:=tt_operatorPot;           end else
           if startsWith(line,C_tokenString[tt_cso_assignMult])        then begin parsedLength:=2; result.tokType:=tt_cso_assignMult;        end
                                                                       else begin parsedLength:=1; result.tokType:=tt_operatorMult;          end;
      '>': if startsWith(line,C_tokenString[tt_comparatorGeq])         then begin parsedLength:=2; result.tokType:=tt_comparatorGeq;         end
                                                                       else begin parsedLength:=1; result.tokType:=tt_comparatorGrt;         end;
      '=': if startsWith(line,C_tokenString[tt_typeCheckEmptyList])    then begin parsedLength:=3; result.tokType:=tt_typeCheckEmptyList;    end else
           if startsWith(line,C_tokenString[tt_comparatorListEq])      then begin parsedLength:=2; result.tokType:=tt_comparatorListEq;      end
                                                                       else begin parsedLength:=1; result.tokType:=tt_comparatorEq;          end;
      '<': if startsWith(line,C_tokenString[tt_typeCheckNonemptyList]) then begin parsedLength:=4; result.tokType:=tt_typeCheckNonemptyList; end else
           if startsWith(line,'<>')                                    then begin parsedLength:=2; result.tokType:=tt_comparatorNeq;         end else
           if startsWith(line,'<=')                                    then begin parsedLength:=2; result.tokType:=tt_comparatorLeq;         end
                                                                       else begin parsedLength:=1; result.tokType:=tt_comparatorLss;         end;
      '%': if startsWith(line,C_tokenString[tt_operatorExtractL3])     then begin parsedLength:=4; result.tokType:=tt_operatorExtractL3;     end else
           if startsWith(line,C_tokenString[tt_operatorExtractL2])     then begin parsedLength:=3; result.tokType:=tt_operatorExtractL2;     end else
           if startsWith(line,C_tokenString[tt_operatorExtractL1])     then begin parsedLength:=2; result.tokType:=tt_operatorExtractL1;     end
                                                                       else begin parsedLength:=1; result.tokType:=tt_operatorExtractL0;     end;
      '!': if startsWith(line,'!=')                                    then begin parsedLength:=2; result.tokType:=tt_comparatorNeq;         end
                                                                       else begin fail('Cannot parse: '+line+' (first char is "'+line[1]+'"=#'+intToStr(ord(line[1]))+')'); line:=''; end;
      else begin
        fail('Cannot parse: '+line+' (first char is "'+line[1]+'"=#'+intToStr(ord(line[1]))+')');
        line:='';
      end;
    end;
    if parsedLength>0 then begin
      if not(inEvaluationLoop) then result.txt:=copy(line,1,parsedLength);
      line:=copy(line,parsedLength+1,length(line));
      inc(lineLocation.column,parsedLength);
    end;
  end;

FUNCTION tokenizeAll(inputString:ansistring; location:T_tokenLocation;  CONST inPackage:P_package):T_tokenArray;
  VAR next:T_token;
  begin
    result.create;
    while not(isBlank(inputString)) and (errorLevel<el3_evalError) do begin
      next:=firstToken(inputString,location,inPackage,true);
      if (next.tokType<>tt_EOL) or (next.txt<>'') then begin
        if next.txt=SPECIAL_COMMENT_BLOB_BEGIN then begin
          inputString:='';
        end else begin
          result.append(next);
        end;
      end;
    end;
  end;

FUNCTION tokenizeAll(CONST provider:P_codeProvider; CONST inPackage:P_package):T_tokenArray;
  VAR location:T_tokenLocation;
      lineIndex:longint;
      next:T_token;
      lines:T_arrayOfString;
      blob:record
        level :longint; //0 for "There is no blob"
        text  :ansistring;
        start :T_tokenLocation;
      end;

  begin
    blob.level:=0;
    lines:=provider^.getLines;
    result.create;
    location.fileName:=provider^.getPath;
    for lineIndex:=0 to length(lines)-1 do begin
      location.line:=lineIndex+1;
      location.column:=1;
      if blob.level>0 then begin
        if trim(lines[lineIndex])=SPECIAL_COMMENT_BLOB_END then dec(blob.level);
        if trim(lines[lineIndex])=SPECIAL_COMMENT_BLOB_BEGIN then inc(blob.level);
        if blob.level=0 then begin
          next.create;
          next.define(blob.start,'',tt_literal,newStringLiteral(blob.text));
          result.append(next);
        end else begin
          if blob.text=C_carriageReturnChar
          then blob.text:=                          lines[lineIndex]
          else blob.text:=blob.text+C_lineBreakChar+lines[lineIndex];
        end;
      end else while not(isBlank(lines[lineIndex])) and (errorLevel<el3_evalError) do begin
        next:=firstToken(lines[lineIndex],location,inPackage,true);
        if (next.tokType<>tt_EOL) or (next.txt<>'') then begin
          if next.txt=SPECIAL_COMMENT_BLOB_BEGIN then with blob do begin
            level:=1;
            start:=location;
            text:=C_carriageReturnChar;
            lines[lineIndex]:='';
          end else begin
            result.append(next);
          end;
        end;
      end;
    end;
  end;

PROCEDURE predigestBeforeDeclarationParsing(VAR first:P_token; VAR recycler:T_tokenRecycler);
  VAR this,next,prev:P_token;
  begin
    this:=first; prev:=nil;
    while this<>nil do begin
      next:=this^.next;
      if (this^.tokType in [tt_operatorMinus,tt_operatorPlus]) and
         ((prev=nil) or (prev^.tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach, tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus]))
         and (this^.next<>nil) and (this^.next^.tokType in [tt_literal,tt_identifier,tt_braceOpen,tt_listBraceOpen,tt_expBraceOpen,tt_localUserRulePointer,tt_importedUserRulePointer,tt_intrinsicRulePointer,tt_parameterIdentifier]) then begin
        if this^.tokType=tt_operatorMinus then begin
          if (next<>nil) and (next^.tokType = tt_literal) then begin
            this^.tokType:=tt_literal;
            this^.data:=P_literal(next^.data)^.negate(this^.location);
            this^.next:=recycler.disposeToken(next);
          end else this^.tokType:=tt_unaryOpMinus;
        end else begin
          if prev=nil then begin
            first:=recycler.disposeToken(this);
            this:=first;
          end else begin
            prev^.next:=recycler.disposeToken(this);
            this:=next;
          end;
        end;
      end;
      prev:=this;
      this:=this^.next;
    end;
  end;

PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_package; VAR recycler:T_tokenRecycler);
  VAR t:P_token;
      rule:P_rule;
  begin
    t:=first;
    while t<>nil do begin
      case t^.tokType of
        tt_identifier: if inPackage<>nil then begin
          if t^.data=nil then t^.data:=inPackage;
          if (t^.next<>nil) and (t^.next^.tokType in [tt_assign,tt_cso_assignPlus..tt_cso_assignAppend]) then begin
            inPackage^.resolveRuleId(t^,true);
            if t^.tokType<>tt_identifier then begin
              if t^.tokType=tt_localUserRulePointer then begin
                rule:=t^.data;
                if rule^.ruleType=rt_mutable then begin
                  t^.data:=rule;
                  t^.tokType:=t^.next^.tokType;
                  if t^.tokType=tt_assign then t^.tokType:=tt_mutate;
                  t^.txt:=t^.txt;
                  t^.next:=recycler.disposeToken(t^.next);
                end else raiseError(el4_parsingError,'You can only mutate local mutable rules! Rule '+t^.txt+' is not mutable',t^.next^.location);
              end else raiseError(el4_parsingError,'You can only mutate local mutable rules! Rule '+t^.txt+' is a '+C_ruleTypeString[t^.tokType],t^.next^.location);
            end else begin
              raiseError(el2_warning,'Cannot resolve identifier "'+t^.txt+'" using dynamic scoping (expecting a block-local variable for this identifier) as a fallback.',t^.location);
              t^.tokType:=t^.next^.tokType;
              if t^.tokType=tt_assign then t^.tokType:=tt_assignExistingBlockLocal;
              t^.data:=nil;
              t^.next:=recycler.disposeToken(t^.next);
              //raiseError(el4_parsingError,'You can only mutate local mutable rules! Identifier '+t^.txt+' cannot be resolved.',t^.next^.location);
            end;
          end;
        end;
        tt_modifier_local: if (t^.next<>nil) and (t^.next^.tokType=tt_blockLocalVariable) and (t^.next^.next<>nil) and (t^.next^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignNewBlockLocal;
          t^.data:=nil;
          t^.txt:=t^.next^.txt;
          t^.next:=recycler.disposeToken(t^.next);
          t^.next:=recycler.disposeToken(t^.next);
        end;
        tt_blockLocalVariable: if (t^.next<>nil) and (t^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignExistingBlockLocal;
          t^.data:=nil;
          t^.next:=recycler.disposeToken(t^.next);
        end else if (t^.next<>nil) and (t^.next^.tokType in [tt_cso_assignPlus..tt_cso_assignAppend]) then begin
          t^.tokType:=t^.next^.tokType;
          t^.data:=nil;
          t^.next:=recycler.disposeToken(t^.next);
        end;
        end;
      t:=t^.next;
    end;
  end;

PROCEDURE digestInlineExpression(VAR rep:P_token; VAR recycler:T_tokenRecycler);
  VAR t,prev,inlineRule:P_token;
      bracketLevel:longint=0;
      inlineSubRule:P_subrule;
  begin
    predigest(rep,nil,recycler);
    if (rep^.tokType<>tt_expBraceOpen) then begin
      raiseError(el4_parsingError,'Error creating subrule from inline; expression does not start with "{"',rep^.location);
      exit;
    end;
    t:=rep^.next; prev:=rep;
    inlineRule:=t;
    while (t<>nil) and ((t^.tokType<>tt_expBraceClose) or (bracketLevel>0)) do begin
      case t^.tokType of
        tt_expBraceOpen: begin
          digestInlineExpression(t,recycler);
          if t^.tokType=tt_expBraceOpen then inc(bracketLevel);
        end;
      end;
      prev:=t;
      t:=t^.next;
    end;
    if (t=nil) or (t^.tokType<>tt_expBraceClose) then begin
      raiseError(el4_parsingError,'Error creating subrule from inline; expression does not end with an }',rep^.location);
      exit;
    end;

    rep^.next:=t^.next; //remove expression from parent expression
    prev^.next:=nil; //unlink closing curly bracket
    recycler.disposeToken(t); //dispose closing curly bracket
    if   errorLevel<=el2_warning then begin
      new(inlineSubRule,createFromInline(inlineRule,recycler));
      if   errorLevel<=el2_warning then begin
        rep^.tokType:=tt_literal;
        rep^.data:=newExpressionLiteral(inlineSubRule);
      end else dispose(inlineSubRule,destroy);
    end;
  end;

PROCEDURE reduceExpression(VAR first:P_token; CONST callDepth:word; VAR recycler:T_tokenRecycler);
  VAR stack:array of P_token;
      newLit:P_literal;
      didSubstitution:boolean;
      cTokType:array[-1..2] of T_tokenType;
      maxFutureForwardInitialization:longint=16;

  PROCEDURE initTokTypes; inline;
    begin
      if length(stack)>0 then cTokType[-1]:=stack[length(stack)-1]^.tokType
                         else cTokType[-1]:=tt_EOL;
      if first<>nil then begin
        cTokType[0]:=first^.tokType;
        if first^.next<>nil then begin
          cTokType[1]:=first^.next^.tokType;
          if first^.next^.next<>nil then cTokType[2]:=first^.next^.next^.tokType
                                    else cTokType[2]:=tt_EOL;
        end else begin
          cTokType[1]:=tt_EOL;
          cTokType[2]:=tt_EOL;
        end;
      end else begin
        cTokType[0]:=tt_EOL;
        cTokType[1]:=tt_EOL;
        cTokType[2]:=tt_EOL;
      end;
    end;

  PROCEDURE stack_popDestroy; inline;
    begin
      recycler.disposeToken(stack[length(stack)-1]);
      setLength(stack,length(stack)-1);
      didSubstitution:=true;
    end;

  PROCEDURE stack_popLink; inline;
    VAR ti:longint;
    begin
      ti:=length(stack)-1;
      stack[ti]^.next:=first;
      first:=stack[ti];
      setLength(stack,ti);
      didSubstitution:=true;
    end;

  PROCEDURE stack_push; inline;
    begin
      setLength(stack,length(stack)+1);
      stack[length(stack)-1]:=first;
      first:=first^.next;
      didSubstitution:=true;
    end;

  PROCEDURE stack_push_push; inline;
    begin
      setLength(stack,length(stack)+2);
      stack[length(stack)-2]:=first; first:=first^.next;
      stack[length(stack)-1]:=first; first:=first^.next;
      didSubstitution:=true;
    end;

  PROCEDURE resolveEach(inParallel:boolean);
    VAR t,bracketClosingEach:P_token;
        bodyRule:array of P_subrule;
        aggregatorLocation:T_tokenLocation;
        aggregatorToken:T_tokenType;
        aggregatorExpression:P_expressionLiteral;
        aggregatorPresent:(no,yes_as_operator,yes_as_expression)=no;

    FUNCTION parseBodyOk:boolean;
      VAR bracketLevel,i:longint;
          p:P_token;
          bodyPart:array of array[0..1] of P_token;
      begin
        //first token is <each>-Token
        //find closing bracket and body parts
        bracketLevel:=0; //note: first token will raise bracket Level to 1
        t:=first; p:=nil;
        if (first^.next<>nil) and (first^.next^.tokType<>tt_separatorComma) then begin
          setLength(bodyPart,1);
          bodyPart[0,0]:=first^.next;
        end else begin
          raiseError(el4_parsingError,'Invalid each-construct; Cannot find closing bracket.',first^.location);
          exit(false);
        end;
        while (t<>nil) and not((t^.tokType=tt_braceClose) and (bracketLevel=1)) do begin
          if      t^.tokType in [tt_braceOpen,tt_each,tt_parallelEach,tt_listBraceOpen ,tt_expBraceOpen ,tt_procedureBlockBegin] then inc(bracketLevel)
          else if t^.tokType in [tt_braceClose                       ,tt_listBraceClose,tt_expBraceClose,tt_procedureBlockEnd  ] then dec(bracketLevel)
          else if t^.tokType in [tt_blockLocalVariable,tt_modifier_local] then inParallel:=false
          else if (t^.tokType=tt_separatorComma) and (bracketLevel=1) then begin
            bodyPart[length(bodyPart)-1,1]:=p; //end of body part is token before comma
            setLength(bodyPart,length(bodyPart)+1);
            bodyPart[length(bodyPart)-1,0]:=t^.next; //start of next body part is token after comma
          end;
          p:=t; t:=t^.next;
        end;
        bodyPart[length(bodyPart)-1,1]:=p; //end of body part is token before comma
        if (t=nil) or (t^.tokType<>tt_braceClose) or (bracketLevel<>1) then begin
          raiseError(el4_parsingError,'Invalid each-construct; Cannot find closing bracket.',first^.location);
          exit(false);
        end;
        bracketClosingEach:=t;
        for i:=0 to length(bodyPart)-1 do begin
          if bodyPart[i,1]^.next<>bracketClosingEach then recycler.disposeToken(bodyPart[i,1]^.next);
          bodyPart[i,1]^.next:=nil;
        end;
        //process aggregator part (if any)----------------------------------------------
        aggregatorPresent:=no;
        t:=bodyPart[0,0];
        if t^.tokType=tt_aggregatorConstructor then begin
          reduceExpression(bodyPart[0,0],callDepth+1,recycler);
          bodyPart[0,1]:=bodyPart[0,0]^.last;
          t:=bodyPart[0,0];
        end;
        if t^.next=nil then begin
          if t^.tokType in [tt_comparatorEq..tt_operatorIn] then begin
            aggregatorToken:=t^.tokType;
            aggregatorPresent:=yes_as_operator;
            aggregatorLocation:=t^.location;
          end else if t^.tokType=tt_aggregatorExpressionLiteral then begin
            aggregatorExpression:=P_expressionLiteral(t^.data);
            aggregatorExpression^.rereference;
            aggregatorPresent:=yes_as_expression;
            aggregatorLocation:=t^.location
          end;
        end;
        if aggregatorPresent<>no then begin
          recycler.disposeToken(bodyPart[0,0]);
          for i:=0 to length(bodyPart)-2 do bodyPart[i]:=bodyPart[i+1];
          setLength(bodyPart,length(bodyPart)-1);
          first^.tokType:=tt_braceOpen;
          bracketClosingEach^.tokType:=tt_braceClose;
        end;
        //----------------------------------------------process aggregator part (if any)
        //process other body parts (if any)---------------------------------------------
        setLength(bodyRule       ,length(bodyPart));
        for i:=0 to length(bodyPart)-1 do
          new(bodyRule[i],createForEachBody(first^.txt,bodyPart[i,0],recycler));
        //---------------------------------------------process other body parts (if any)
        result:=true;
      end;

    VAR itList:P_listLiteral;
        i,j:longint;
        eachLocation:T_tokenLocation;
        resultLiteral:P_literal;
        booleanResult:boolean;
        lazyAbort:boolean=false;

    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean); inline;
      VAR newLit:P_literal;
      begin
        if L=nil then begin
          if resultLiteral=nil then resultLiteral:=newErrorLiteral;
          raiseError(el3_evalError,'Error performing aggregation in each-construct - NIL literal encountered',eachLocation);
          exit;
        end else if L^.literalType=lt_void then exit;
        case aggregatorPresent of
          no: begin
            if resultLiteral=nil then resultLiteral:=newListLiteral;
            P_listLiteral(resultLiteral)^.append(L,true);
          end;
          yes_as_operator: begin
            case aggregatorToken of
              tt_operatorConcat: begin
                if resultLiteral=nil then resultLiteral:=newListLiteral;
                if (L^.literalType in [lt_error, lt_boolean, lt_int, lt_real, lt_string, lt_expression])
                then P_listLiteral(resultLiteral)^.append   (L, true)
                else P_listLiteral(resultLiteral)^.appendAll(P_listLiteral(L));
              end;
              tt_operatorLazyAnd: if resultLiteral=nil then begin
                if (L^.literalType=lt_boolean) then booleanResult:=P_boolLiteral(L)^.value
                else raiseError(el3_evalError, 'Lazy boolean operators can only be applied to scalar booleans.',aggregatorLocation);
                if not(booleanResult) then begin
                  lazyAbort:=true;
                  resultLiteral:=L;
                  L^.rereference;
                end;
              end;
              tt_operatorLazyOr: if resultLiteral=nil then begin
                if (L^.literalType=lt_boolean) then booleanResult:=P_boolLiteral(L)^.value
                else raiseError(el3_evalError, 'Lazy boolean operators can only be applied to scalar booleans.',aggregatorLocation);
                if booleanResult then begin
                  lazyAbort:=true;
                  resultLiteral:=L;
                  L^.rereference;
                end;
              end;
              else begin
                if resultLiteral=nil then begin
                  resultLiteral:=L; L^.rereference;
                end else begin
                  newLit:=resolveOperator(resultLiteral,
                                          aggregatorToken,
                                          L,
                                          eachLocation);
                  if newLit<>nil then begin
                    disposeLiteral(resultLiteral);
                    resultLiteral:=newLit;
                  end else raiseError(el3_evalError,'Error performing aggregation in each-construct with aggregator ('+C_tokenString[aggregatorToken]+')',eachLocation);
                end;
              end;
            end;
          end;
          yes_as_expression: begin
            if resultLiteral=nil then begin
              resultLiteral:=L; L^.rereference;
            end else begin
              newLit:=P_subrule(aggregatorExpression^.value)^.directEvaluateAggregator(resultLiteral,L,aggregatorExpression,callDepth+1,recycler);
              if newLit<>nil then begin
                disposeLiteral(resultLiteral);
                resultLiteral:=newLit;
              end else raiseError(el3_evalError,'Error performing aggregation in each-construct with aggregator '+aggregatorExpression^.toString,eachLocation);
            end;
          end;
        end;
        if doDispose then disposeLiteral(L);
      end;

    PROCEDURE finalizeAggregation; inline;
      begin
        first^.tokType:=tt_literal;
        if (resultLiteral=nil) and (aggregatorPresent=yes_as_operator) and (aggregatorToken in [tt_operatorLazyAnd,tt_operatorLazyOr]) then begin
          resultLiteral:=newBoolLiteral(booleanResult);
        end;
        if resultLiteral=nil then begin
          if aggregatorPresent<>no then begin
            first^.data:=newErrorLiteral;
            raiseError(el3_evalError,'Error performing finalizing aggregation each-construct - NIL literal encountered',eachLocation);
          end else first^.data:=newListLiteral;
        end else first^.data:=resultLiteral;
        first^.txt:='';
        first^.next:=recycler.disposeToken(bracketClosingEach);
      end;

    VAR future:array of P_futureTask;
        nextFuture:longint=0;
        initializedFutures:longint=0;

    PROCEDURE aggregateFutures(CONST finalizing:boolean);
      begin
        while (nextFuture<initializedFutures)
          and (future[nextFuture]<>nil)
          and ((pendingTasks.queuedCount>maxFutureForwardInitialization) or
               finalizing or
               (future[nextFuture]^.state in [fts_errorOnInitialization,fts_ready])) do begin
          while (future[nextFuture]^.state in [fts_pending,fts_evaluating]) do pendingTasks.activeDeqeue(callDepth+1,recycler);
          addToAggregation(future[nextFuture]^.getResultAsLiteral,true);
          future[nextFuture]^.free(recycler);
          dispose(future[nextFuture],proFormaDestroy);
          inc(nextFuture);
        end;
        if finalizing then begin
          finalizeAggregation;
          setLength(future,0);
        end else begin
          if (nextFuture-initializedFutures<MAX_NUMBER_OF_SECONDARY_WORKER_THREADS) and
             (maxFutureForwardInitialization<16*MAX_NUMBER_OF_SECONDARY_WORKER_THREADS)
          then inc(maxFutureForwardInitialization,maxFutureForwardInitialization);
        end;
      end;

    PROCEDURE initFutures;
      VAR i,j:longint;
      begin
        if (aggregatorPresent=yes_as_operator) and (aggregatorToken in [tt_operatorLazyAnd,tt_operatorLazyOr])
        then maxFutureForwardInitialization:=MAX_NUMBER_OF_SECONDARY_WORKER_THREADS
        else maxFutureForwardInitialization:=MAX_NUMBER_OF_SECONDARY_WORKER_THREADS*8;
        initializedFutures:=0;
        setLength(future,length(bodyRule)*itList^.size);
        for i:=0 to itList^.size-1 do
        for j:=0 to length(bodyRule)-1 do begin
          if (errorLevel<el3_evalError) and not(lazyAbort) then begin
            future[initializedFutures]:=pendingTasks.enqueue(bodyRule[j],itList^.value(i),recycler);
            if future[initializedFutures]^.state=fts_errorOnInitialization then raiseError(el3_evalError,'Error initializing future.',eachLocation);
          end else future[initializedFutures]:=nil;
          inc(initializedFutures);
          if ((initializedFutures and 255)=0) or (pendingTasks.queuedCount>maxFutureForwardInitialization) then aggregateFutures(false);
        end;
      end;

    begin
      eachLocation:=first^.location;
      if callDepth>STACK_DEPTH_LIMIT then begin
        raiseError(el5_systemError,'Stack overflow in (p)each construct.',eachLocation);
        exit;
      end;
      if not(parseBodyOk) then exit;
      if (P_literal(first^.data)^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression])
      then itList:=newOneElementListLiteral(first^.data,false)
      else itList:=first^.data;
      resultLiteral:=nil;
      first^.data:=nil;
      first^.txt:='';
      //iterate over itList----------------------------------------------------------
      t:=first;
      if (aggregatorPresent<>no) and (itList^.size=0)
      then raiseError(el3_evalError,'Each construct with aggregator is invalid for empty lists to iterate',eachLocation)
      else if length(bodyRule)>0 then begin
        if inParallel then begin
          initFutures;
          aggregateFutures(true);
        end else begin
          for i:=0 to itList^.size-1 do for j:=0 to length(bodyRule)-1 do if (errorLevel<=el2_warning) and not(lazyAbort) then
            addToAggregation(bodyRule[j]^.directEvaluateUnary(itList^.value(i),nil,callDepth+1,recycler),true);
          finalizeAggregation;
        end;
      end else begin
        for i:=0 to itList^.size-1 do if errorLevel<=el2_warning then addToAggregation(itList^.value(i),false);
        finalizeAggregation;
      end;
      //----------------------------------------------------------iterate over itList
      //cleanup----------------------------------------------------------------------
      if aggregatorPresent=yes_as_expression then disposeLiteral(aggregatorExpression);
      for i:=0 to length(bodyRule)-1 do dispose(bodyRule[i],destroy);
      disposeLiteral(itList);
      //----------------------------------------------------------------------cleanup
      didSubstitution:=true;
    end;

  PROCEDURE resolveWhile;
    VAR t,bracketClosingWhile:P_token;
        headRule:P_subrule;
        bodyRule:P_subrule;

    FUNCTION parseBodyOk:boolean;
      VAR bracketLevel,i:longint;
          p:P_token;
          bodyPart:array of array[0..1] of P_token;
          emptyPattern:T_pattern;
      begin
        //first token is <while>-Token
        //find closing bracket and body parts
        bracketLevel:=1;
        t:=first; p:=nil;
        if (first^.next<>nil) and (first^.next^.tokType<>tt_separatorComma) then begin
          setLength(bodyPart,1);
          bodyPart[0,0]:=first^.next;
        end else begin
          raiseError(el4_parsingError,'Invalid while-construct; Cannot find closing bracket.',first^.location);
          exit(false);
        end;
        while (t<>nil) and not((t^.tokType=tt_braceClose) and (bracketLevel=1)) do begin
          if      t^.tokType in [tt_braceOpen,tt_each,tt_parallelEach,tt_listBraceOpen ,tt_expBraceOpen ,tt_procedureBlockBegin] then inc(bracketLevel)
          else if t^.tokType in [tt_braceClose                       ,tt_listBraceClose,tt_expBraceClose,tt_procedureBlockEnd]   then dec(bracketLevel)
          else if (t^.tokType=tt_separatorComma) and (bracketLevel=1) then begin
            bodyPart[length(bodyPart)-1,1]:=p; //end of body part is token before comma
            setLength(bodyPart,length(bodyPart)+1);
            bodyPart[length(bodyPart)-1,0]:=t^.next; //start of next body part is token after comma
          end;
          p:=t; t:=t^.next;
        end;
        bodyPart[length(bodyPart)-1,1]:=p; //end of body part is token before comma
        if (t=nil) or (t^.tokType<>tt_braceClose) or (bracketLevel<>1) then begin
          raiseError(el4_parsingError,'Invalid while-construct; Cannot find closing bracket.',first^.location);
          exit(false);
        end;
        bracketClosingWhile:=t;
        if length(bodyPart)<>2 then begin
          raiseError(el4_parsingError,'Invalid while-construct; Exactly two arguments (head and body) are expected.',first^.location);
          exit(false);
        end;

        for i:=0 to length(bodyPart)-1 do begin
          if bodyPart[i,1]^.next<>bracketClosingWhile then recycler.disposeToken(bodyPart[i,1]^.next);
          bodyPart[i,1]^.next:=nil;
        end;


        //create head/body rules------------------------------------------------
        emptyPattern.create;
        new(headRule,create(emptyPattern,bodyPart[0,0],bodyPart[0,0]^.location,true,recycler));
        new(bodyRule,create(emptyPattern,bodyPart[1,0],bodyPart[1,0]^.location,true,recycler));
        emptyPattern.destroy;
        //------------------------------------------------create head/body rules
        result:=true;
      end;

    VAR whileLocation:T_tokenLocation;

    FUNCTION evaluateHead:boolean;
      VAR res:P_literal;
      begin
        res:=headRule^.directEvaluateNullary(nil,callDepth+1,recycler);
        if (res<>nil) and (res^.literalType=lt_boolean) then begin
          result:=P_boolLiteral(res)^.value;
        end else begin
          raiseError(el3_evalError,'The head-part of a while statement must return a scalar boolean!',headRule^.declaredAt);
          result:=false;
        end;
        if res<>nil then disposeLiteral(res);
      end;

    PROCEDURE evaluateBody;
      VAR res:P_literal;
      begin
        res:=bodyRule^.directEvaluateNullary(nil,callDepth+1,recycler);
        if res<>nil then disposeLiteral(res);
      end;

    begin
      whileLocation:=first^.location;
      if callDepth>STACK_DEPTH_LIMIT then begin
        raiseError(el5_systemError,'Stack overflow in while construct.',whileLocation);
        exit;
      end;
      if not(parseBodyOk) then exit;
      while evaluateHead and (errorLevel<=el3_evalError) do evaluateBody;
      first^.txt:='';
      first^.tokType:=tt_literal;
      first^.data:=newVoidLiteral;
      first^.next:=recycler.disposeToken(bracketClosingWhile);
      //cleanup----------------------------------------------------------------------
      dispose(headRule,destroy);
      dispose(bodyRule,destroy);
      //----------------------------------------------------------------------cleanup
      didSubstitution:=true;
    end;


  PROCEDURE applyRule(CONST parameterListToken:P_token; CONST firstTokenAfterCall:P_token);
    VAR firstReplace,lastReplace:P_token;
        newLiteral:P_literal;
        parameterListLiteral:P_listLiteral;
        inlineRule:P_subrule;
    begin
      if parameterListToken=nil then parameterListLiteral:=nil
                                else parameterListLiteral:=parameterListToken^.data;
      if (first^.tokType in [tt_localUserRulePointer,tt_importedUserRulePointer]) then begin
        if not(P_rule(first^.data)^.replaces(parameterListLiteral,firstReplace,lastReplace,callDepth,first^.tokType=tt_localUserRulePointer,recycler)) then begin
          if parameterListLiteral=nil
          then raiseError(el3_evalError,'Cannot apply user defined rule '+P_rule(first^.data)^.id+' to empty parameter list',first^.location)
          else raiseError(el3_evalError,'Cannot apply user defined rule '+P_rule(first^.data)^.id+' to parameter list '+parameterListLiteral^.toParameterListString(true),first^.location);
          exit;
        end;
      end else if (first^.tokType=tt_rulePutCacheValue) then begin
        newLiteral:=P_rule(first^.data)^.doPutCache(parameterListLiteral);
        firstReplace:=recycler.newToken(first^.location,'',tt_literal,newLiteral);
        lastReplace:=firstReplace;
      end else if (first^.tokType=tt_aggregatorConstructor) then begin
        if (parameterListLiteral<>nil) and (parameterListLiteral^.size=1) and (parameterListLiteral^.value(0)^.literalType=lt_expression) then begin
          firstReplace:=recycler.newToken(first^.location,'',tt_aggregatorExpressionLiteral,parameterListLiteral^.value(0));
          parameterListLiteral^.value(0)^.rereference;
          lastReplace:=firstReplace;
        end else raiseError(el3_evalError,'Aggregators can only be constructed from expression literals!',first^.location);
      end else if (first^.tokType=tt_intrinsicRulePointer) then begin
        newLiteral:=T_intFuncCallback(first^.data)(parameterListLiteral,first^.location);
        if newLiteral<>nil then begin
          firstReplace:=recycler.newToken(first^.location,'',tt_literal,newLiteral);
          lastReplace:=firstReplace;
        end else begin
          if parameterListLiteral=nil
          then raiseError(el3_evalError,'Cannot apply intrinsic rule '+first^.txt+' to empty parameter list',first^.location)
          else raiseError(el3_evalError,'Cannot apply intrinsic rule '+first^.txt+' to parameter list '+parameterListLiteral^.toParameterListString(true),first^.location);
          exit;
        end;
      end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_expression) then begin
        inlineRule:=P_expressionLiteral(first^.data)^.value;
        //failing "replaces" for inline rules will raise evaluation error.
        if not(inlineRule^.replaces(parameterListLiteral,P_expressionLiteral(first^.data),firstReplace,lastReplace,recycler)) then exit;
      end else begin
        raiseError(el3_evalError,'Trying to apply a rule which is no rule!',first^.location);
        exit;
      end;
      recycler.disposeToken(first);
      if parameterListToken<>nil then recycler.disposeToken(parameterListToken);
      first:=firstReplace;
      lastReplace^.next:=firstTokenAfterCall;
      didSubstitution:=true;
    end;

  PROCEDURE resolveInlineIf(CONST conditionLit:boolean); inline;
    VAR p,prev,tokenBeforeElse,lastThen:P_token;
        bracketLevel:longint=0;
    begin
      prev:=first;
      stack_push; //push "?"
      p:=first;
      while (p<>nil) and not((p^.tokType=tt_iifElse) and (bracketLevel=0)) do begin
        case p^.tokType of
          tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck,tt_expBraceOpen ,tt_procedureBlockBegin: inc(bracketLevel);
          tt_braceClose,                         tt_listBraceClose ,tt_iifElse ,tt_expBraceClose,tt_procedureBlockEnd  : dec(bracketLevel);
        end;
        prev:=p; p:=p^.next;
      end;
      if not((p<>nil) and (p^.tokType=tt_iifElse) and (bracketLevel=0)) then begin
        stack_popLink;
        raiseError(el3_evalError,'Cannot evaluate inline-if; cannot locate then-marker',first^.location);
        exit;
      end;
      tokenBeforeElse:=prev;
      while (p<>nil) and not((p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon]) and (bracketLevel=-1)) do begin
        case p^.tokType of
          tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck,tt_expBraceOpen ,tt_procedureBlockBegin: inc(bracketLevel);
          tt_braceClose,                         tt_listBraceClose ,tt_iifElse ,tt_expBraceClose,tt_procedureBlockEnd  : dec(bracketLevel);
        end;
        prev:=p; p:=p^.next;
      end;
      if  not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon]) and (bracketLevel=-1)) then begin
        stack_popLink;
        raiseError(el3_evalError,'Cannot evaluate inline-if; cannot locate end of then-expression',first^.location);
        exit;
      end;
      lastThen:=prev;
      if conditionLit then begin
        //take then-subexpression -> drop else-subexpression
        p:=tokenBeforeElse^.next;              //store tt_iifElse-token
        tokenBeforeElse^.next:=lastThen^.next; //unlink else-expression (head)
        lastThen^.next:=nil;                   //unlink else-expression (tail);
        recycler.cascadeDisposeToken(p);       //dispose else-expression
      end else begin
        //take else-subexpression -> drop then-subexpression
        p:=first;
        first:=tokenBeforeElse^.next^.next;
        tokenBeforeElse^.next^.next:=nil;
        recycler.cascadeDisposeToken(p);
      end;
      stack_popDestroy; //pop "?"
      stack_popDestroy; //pop condition literal
    end;

  PROCEDURE applyCheck;
    VAR checkResult:boolean;
    begin
      checkResult:=P_literal(first^.data)^.literalType in C_matchingTypes[first^.next^.tokType];
      first^.next:=recycler.disposeToken(first^.next);
      disposeLiteral(first^.data);
      first^.data:=newBoolLiteral(checkResult);
      didSubstitution:=true;
    end;

  PROCEDURE applyMutation;
    VAR newValue:P_literal;
    begin
      newValue:=first^.next^.data;
      P_rule(first^.data)^.setMutableValue(newValue);
      first:=recycler.disposeToken(first);
    end;

  PROCEDURE applyLocalAssignment(CONST kind:T_tokenType);
    VAR newValue:P_literal;
    begin
      newValue:=first^.next^.data;
      case kind of
        tt_assignNewBlockLocal: begin
          recycler.declareLocalValue(first^.txt,newValue);
          first:=recycler.disposeToken(first);
        end;
        tt_assignExistingBlockLocal: begin
          recycler.setLocalValue(first^.txt,newValue);
          first:=recycler.disposeToken(first);
        end;
        tt_cso_assignPlus..tt_cso_assignAppend: if first^.data=nil then begin
          newValue:=recycler.mutateInline(first^.txt,kind,newValue,first^.location);
          if errorLevel<el3_evalError then begin
            first:=recycler.disposeToken(first);
            disposeLiteral(first^.data);
            first^.data:=newValue;
          end;
        end else begin
          newValue:=P_rule(first^.data)^.mutateInline(kind,newValue,first^.location);
          if errorLevel<el3_evalError then begin
            first:=recycler.disposeToken(first);
            disposeLiteral(first^.data);
            first^.data:=newValue;
          end;
        end;
      end;
    end;

  PROCEDURE startOrPushParameterList; inline;
    begin
      setLength(stack,length(stack)+2);
      stack[length(stack)-2]:=first;
      first:=first^.next;
      if first^.tokType=tt_braceOpen then begin
        first^.tokType:=tt_parList_constructor;
        first^.data:=newListLiteral;
      end;
      stack[length(stack)-1]:=first;
      first:=first^.next;
      didSubstitution:=true;
    end;

  PROCEDURE pon_flip;
    VAR newFunctionToken:P_token;
        newParameterListToken:P_token;
        oldSecond:P_token;
    begin
      //Assuming:
      //cTokType[0]=tt_literal
      //cTokType[1]=tt_???_pon
      //cTokType[2]=may be (
      //Transforms:
      //  <Lit>.func     -> func(<Lit>)
      //  <Lit>.func(... -> func(<Lit>,...
      newFunctionToken:=recycler.newToken(first^.next);
      with newFunctionToken^ do case tokType of
                 tt_identifier_pon:begin
        tokType:=tt_identifier;
          if newFunctionToken^.data=nil then newFunctionToken^.data:=guessPackageForToken(newFunctionToken^);
          P_package(newFunctionToken^.data)^.resolveRuleId(newFunctionToken^,false);
        end;
                 tt_importedUserRulePointer_pon:
        tokType:=tt_importedUserRulePointer;
                 tt_localUserRulePointer_pon:
        tokType:=tt_localUserRulePointer;
                 tt_intrinsicRulePointer_pon:
        tokType:=tt_intrinsicRulePointer;
        else raiseError(el5_systemError,'Something went wrong in pon_flip.',first^.next^.location);
      end;
      newParameterListToken:=recycler.newToken(first^.next^.location,'',tt_parList,newOneElementListLiteral(first^.data,false));


      first^.data:=nil; first^.tokType:=tt_identifier;
      first^.next^.data:=nil;
      oldSecond:=recycler.disposeToken(recycler.disposeToken(first));

      first:=newFunctionToken; newFunctionToken^.next:=newParameterListToken; newParameterListToken^.next:=oldSecond;

      if cTokType[2]=tt_braceOpen then begin
        newParameterListToken^.next:=recycler.disposeToken(oldSecond);
        newParameterListToken^.tokType:=tt_parList_constructor;
      end;
      didSubstitution:=true;
    end;

  PROCEDURE process_op_lit; inline;
    begin
      if (cTokType[1] in [tt_comparatorEq..tt_operatorIn]) and (C_opPrecedence[cTokType[1]]>=C_opPrecedence[cTokType[-1]]) or
         (cTokType[1] in [tt_braceClose,tt_listBraceClose,tt_EOL,tt_separatorComma,tt_semicolon, tt_separatorCnt, tt_iifCheck, tt_iifElse])
      then begin
        newLit:=resolveOperator(stack[length(stack)-2]^.data,
                                stack[length(stack)-1]^.tokType,
                                first^.data,
                                stack[length(stack)-1]^.location);
        disposeLiteral(first^.data);
        first^.data:=newLit; //store new literal in head
        stack_popDestroy; //pop operator from stack
        stack_popDestroy; //pop LHS-Literal from stack
      end else if cTokType[1]=tt_parList                                                  then applyRule(first^.next,first^.next^.next)
      else if cTokType[1] in [tt_parList_constructor,tt_braceOpen,tt_listToParameterList] then startOrPushParameterList
      else if cTokType[1] in [tt_typeCheckScalar..tt_typeCheckKeyValueList]               then applyCheck
      else if cTokType[1] in [tt_identifier_pon,tt_localUserRulePointer_pon,tt_importedUserRulePointer_pon,tt_intrinsicRulePointer_pon] then pon_flip
      else stack_push_push;
    end;

  PROCEDURE operator_and_literal_push; inline;
    VAR p:P_token;
        bracketLevel:longint=0;
    begin
      case cTokType[1] of
        tt_operatorLazyAnd: if (cTokType[0]=tt_literal) and (P_literal(first^.data)^.literalType=lt_boolean) then begin
          if (P_boolLiteral(first^.data)^.value) then begin
            //true AND ... -> ...
            first:=recycler.disposeToken(first); //drop true
            first:=recycler.disposeToken(first); //drop AND
            didSubstitution:=true;
          end else begin
            //false AND ... -> false
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse,tt_operatorLazyOr,tt_operatorOr]) and (bracketLevel=0)) do begin
              case p^.tokType of
                tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck,tt_expBraceOpen ,tt_procedureBlockBegin: inc(bracketLevel);
                tt_braceClose,                         tt_listBraceClose ,tt_iifElse ,tt_expBraceClose,tt_procedureBlockEnd  : dec(bracketLevel);
              end;
              p:=recycler.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end;
        end else raiseError(el3_evalError,'Lazy boolean operators can only be applied to scalar booleans.',first^.next^.location);
        tt_operatorLazyOr:if (cTokType[0]=tt_literal) and (P_literal(first^.data)^.literalType=lt_boolean) then begin
          if (P_boolLiteral(first^.data)^.value) then begin
            //true OR ... -> true
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse]) and (bracketLevel=0)) do begin
              case p^.tokType of
                tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck,tt_expBraceOpen ,tt_procedureBlockBegin: inc(bracketLevel);
                tt_braceClose,                         tt_listBraceClose ,tt_iifElse ,tt_expBraceClose,tt_procedureBlockEnd  : dec(bracketLevel);
              end;
              p:=recycler.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end else begin
            //false OR ... -> ...
            first:=recycler.disposeToken(first); //drop false
            first:=recycler.disposeToken(first); //drop OR
            didSubstitution:=true;
          end;
        end else raiseError(el3_evalError,'Lazy boolean operators can only be applied to scalar booleans.',first^.next^.location);
        else begin
          setLength(stack,length(stack)+2);
          stack[length(stack)-2]:=first; first:=first^.next;
          stack[length(stack)-1]:=first; first:=first^.next;
          didSubstitution:=true;
        end;
      end;
    end;

  FUNCTION stateToString:ansistring;
    VAR i:longint;
        idLike:boolean=false;
        p:P_token;
    begin
      result:='';
      for i:=0 to length(stack)-1 do result:=result+stack[i]^.toString(idLike,idLike);
      result:=result+' || ';
      p:=first;
      while p<>nil do begin
        result:=result+p^.toString(idLike,idLike);
        p:=p^.next;
      end;
    end;

{$MACRO ON}
{$define COMMON_CASES:=
tt_braceOpen, tt_parList_constructor, tt_listToParameterList: startOrPushParameterList;
tt_parList:  applyRule(first^.next,first^.next^.next);
tt_comparatorEq..tt_operatorIn: operator_and_literal_push;
tt_iifCheck: stack_push;
tt_identifier_pon,tt_importedUserRulePointer_pon,tt_localUserRulePointer_pon,tt_intrinsicRulePointer_pon: pon_flip;
tt_typeCheckScalar..tt_typeCheckKeyValueList: applyCheck}
{$define COMMON_SEMICOLON_HANDLING:=
tt_semicolon: if (cTokType[2]=tt_procedureBlockEnd) and (cTokType[-1]=tt_procedureBlockBegin) then begin
  stack_popDestroy;
  first^.next:=recycler.disposeToken(first^.next);
  first^.next:=recycler.disposeToken(first^.next);
  recycler.scopePop;
  didSubstitution:=true;
end else begin
  first:=recycler.disposeToken(first);
  first:=recycler.disposeToken(first);
  didSubstitution:=true;
end}

  begin
    setLength(stack,0);
    {$ifndef debugMode} try {$endif}
    repeat
      didSubstitution:=false;
      initTokTypes;
      {$ifdef fullVersion} if stepper.stepping(first^.location) then raiseError(els_step,stateToString,first^.location); {$endif}
      case cTokType[0] of
        tt_literal,tt_aggregatorExpressionLiteral: case cTokType[-1] of
          tt_listToParameterList: if P_literal(first^.data)^.literalType in C_validListTypes then begin
            stack_popDestroy;
            first^.tokType:=tt_parList;
            stack_popLink;
          end;
          tt_unaryOpPlus: stack_popDestroy;
          tt_unaryOpMinus: begin
            newLit:=P_literal(first^.data)^.negate(stack[length(stack)-1]^.location);
            disposeLiteral(first^.data);
            first^.data:=newLit;
            stack_popDestroy;
          end;
          tt_comparatorEq..tt_comparatorListEq: begin //operators with special cascading
            if (cTokType[1] in [tt_comparatorEq..tt_comparatorListEq]) then begin
              // x < y < z -> [x < y] and y < z
              newLit:=resolveOperator(stack[length(stack)-2]^.data,
                                      stack[length(stack)-1]^.tokType,
                                      first^.data,
                                      stack[length(stack)-1]^.location);
              //LHS literal is now result of first comparison (still a literal)
              disposeLiteral(stack[length(stack)-2]^.data);
              stack[length(stack)-2]^.data:=newLit;
              //applied comparator is replaced by operator 'and'
              stack[length(stack)-1]^.tokType:=tt_operatorAnd;
              didSubstitution:=true;
            end else process_op_lit;
          end;
          tt_operatorAnd, tt_operatorOr, //operators with lazy evaluation ?
          tt_operatorXor, tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt,
          tt_operatorMod, tt_operatorPot, tt_operatorStrConcat, tt_operatorExtractL0, tt_operatorExtractL1,
          tt_operatorExtractL2, tt_operatorExtractL3, tt_operatorConcat, tt_operatorIn: process_op_lit;
          tt_braceOpen : case cTokType[1] of // ( | <Lit>
            tt_braceClose: begin  // ( | <Lit> )
              stack_popDestroy;
              first^.next:=recycler.disposeToken(first^.next);
              didSubstitution:=true;
            end;
            COMMON_CASES;
            COMMON_SEMICOLON_HANDLING;
            else raiseError(el3_evalError,'Unable to resolve paranthesis!',stack[length(stack)-1]^.location);
          end;
          tt_list_constructor: case cTokType[1] of
            tt_separatorComma, tt_separatorCnt: begin // [ | <Lit> ,
              repeat
                P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location);
                if first^.next^.tokType=tt_separatorCnt then P_listLiteral(stack[length(stack)-1]^.data)^.setRangeAppend;
                first:=recycler.disposeToken(first);
                first:=recycler.disposeToken(first);

              until (first=nil) or (first^.toktype<>tt_literal) or
                    (first^.next=nil) or not(first^.next^.tokType in [tt_separatorComma,tt_separatorCnt]);
              didSubstitution:=true;
            end;
            //tt_separatorCnt: begin // [ | <Lit> ..
            //  P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location);
            //  P_listLiteral(stack[length(stack)-1]^.data)^.setRangeAppend;
            //  first:=recycler.disposeToken(first);
            //  first:=recycler.disposeToken(first);
            //  didSubstitution:=true;
            //end;
            tt_listBraceClose: begin // [ | <Lit> ]
              P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              stack_popLink;
              first^.tokType:=tt_literal;
              didSubstitution:=true;
            end;
            COMMON_CASES;
            COMMON_SEMICOLON_HANDLING;
          end;
          tt_parList_constructor: case cTokType[1] of
            tt_braceClose: begin // <F> <par(> | <Lit> ) -> <F> <par>
              P_listLiteral(stack[length(stack)-1]^.data)^.append(first^.data,true);
              stack[length(stack)-1]^.tokType:=tt_parList; //mutate <tt_parList_constructor> -> <tt_parList>
              first:=recycler.disposeToken(first); //dispose literal
              first:=recycler.disposeToken(first); //dispose closing bracket
              stack_popLink; //pop parameter list
              stack_popLink; //pop FUNCTION
            end;
            tt_separatorComma: begin // <F> <par(> | <Lit> , -> <F> <par(> |
              P_listLiteral(stack[length(stack)-1]^.data)^.append(first^.data,true);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              didSubstitution:=true;
            end;
            COMMON_CASES;
            COMMON_SEMICOLON_HANDLING;
          end;
          tt_each,tt_parallelEach: if (stack[length(stack)-1]^.data=nil) then case cTokType[1] of
            tt_separatorComma: if cTokType[2]<>tt_EOL then begin
              // each(<id>, || <literal> , ...
              stack[length(stack)-1]^.data:=first^.data; //store literal list in each-token
              P_literal(first^.data)^.rereference; //rereference literal to prevent destruction
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              stack_popLink;
            end else raiseError(el3_evalError,'Invalid syntax for each construct - EOL comes to early!',stack[length(stack)-1]^.location);
            COMMON_CASES;
            COMMON_SEMICOLON_HANDLING;
            else begin
              raiseError(el3_evalError,'Invalid syntax for each construct - token "'+safeTokenToString(first^.next)+'" cannot be interpreted after each! Expression tail:',stack[length(stack)-1]^.location);
            end;
          end else raiseError(el3_evalError,'Invalid syntax for each construct - each already has a literal!',stack[length(stack)-1]^.location);
          tt_mutate: case cTokType[1] of
            tt_semicolon: if (cTokType[2]=tt_procedureBlockEnd) and (cTokType[-1]=tt_procedureBlockBegin) then begin
              stack_popDestroy;
              first^.next:=recycler.disposeToken(first^.next);
              first^.next:=recycler.disposeToken(first^.next);
              recycler.scopePop;
              didSubstitution:=true;
            end else begin
              stack_popLink;
              applyMutation;
            end;
            tt_braceClose,tt_separatorCnt,tt_separatorComma,tt_EOL,tt_expBraceClose,tt_listBraceClose: begin
              stack_popLink;
              applyMutation;
            end;
            COMMON_CASES;
          end;
          tt_assignNewBlockLocal, tt_assignExistingBlockLocal,tt_cso_assignPlus..tt_cso_assignAppend: case cTokType[1] of
            tt_semicolon: if (cTokType[2]=tt_procedureBlockEnd) and (cTokType[-1]=tt_procedureBlockBegin) then begin
              first:=recycler.disposeToken(first);
              first^.next:=recycler.disposeToken(first^.next);
              first^.next:=recycler.disposeToken(first^.next);
              recycler.scopePop;
              didSubstitution:=true;
            end else begin
              stack_popLink;
              applyLocalAssignment(cTokType[-1]);
            end;
            tt_braceClose,tt_separatorCnt,tt_separatorComma,tt_EOL,tt_expBraceClose,tt_listBraceClose: begin
              stack_popLink;
              applyLocalAssignment(cTokType[-1]);
            end;
            COMMON_CASES;
          end;
          else begin
            case cTokType[1] of
              COMMON_CASES;
              COMMON_SEMICOLON_HANDLING;
            end;
          end;
        end;
        tt_procedureBlockBegin: begin
          recycler.scopePush;
          stack_push;
          didSubstitution:=true;
        end;
        tt_assignNewBlockLocal, tt_assignExistingBlockLocal,tt_cso_assignPlus..tt_cso_assignAppend: stack_push;
        tt_blockLocalVariable: begin
          first^.data:=recycler.getLocalValueOrNull(first^.txt);
          if first^.data<>nil then begin
            first^.tokType:=tt_literal;
            didSubstitution:=true;
          end else raiseError(el3_evalError,'Cannot find value for local id "'+first^.txt+'"',first^.location);
        end;
        tt_operatorPlus: begin first^.tokType:=tt_unaryOpPlus; stack_push; end;
        tt_operatorMinus: begin first^.tokType:=tt_unaryOpMinus; stack_push; end;
        tt_unaryOpPlus, tt_unaryOpMinus: stack_push;
        tt_comparatorEq..tt_operatorXor,tt_operatorMult..tt_operatorPot,tt_operatorStrConcat..tt_operatorIn:
          raiseError(el3_evalError,'Undefined prefix operator '+first^.singleTokenToString,first^.location);
        tt_braceOpen: stack_push;
        tt_expBraceOpen: begin
          digestInlineExpression(first,recycler);
          didSubstitution:=true;
        end;
        tt_braceClose: if cTokType[-1]=tt_parList_constructor then begin
          first:=recycler.disposeToken(first);
          stack_popLink;
          first^.tokType:=tt_parList;
          stack_popLink;
          didSubstitution:=true;
        end;
        tt_listBraceOpen: if cTokType[1]=tt_listBraceClose then begin
          //empty list
          first^.data:=newListLiteral;
          first^.tokType:=tt_literal;
          first^.next:=recycler.disposeToken(first^.next);
          didSubstitution:=true;
        end else begin
          first^.data:=newListLiteral;
          first^.tokType:=tt_list_constructor;
          stack_push;
        end;
        tt_list_constructor: stack_push;
        tt_identifier: begin
          if not(recycler.scopeBottom) and (recycler.hasId(first^.txt)) then begin
            first^.tokType:=tt_blockLocalVariable;
            first^.data:=nil;
            didSubstitution:=true;
          end else begin
            if first^.data=nil then first^.data:=guessPackageForToken(first^);
            P_package(first^.data)^.resolveRuleId(first^,false);
            if first^.tokType<>tt_identifier then didSubstitution:=true;
          end;
        end;
        tt_identifier_pon: begin
          if first^.data=nil then first^.data:=guessPackageForToken(first^);
          P_package(first^.data)^.resolveRuleId(first^,false);
          if first^.tokType<>tt_identifier_pon then didSubstitution:=true;
        end;
        tt_each,tt_parallelEach: if (first^.data=nil) then stack_push else resolveEach(
        (first^.tokType=tt_parallelEach) and (callDepth<STACK_DEPTH_LIMIT));
        tt_mutate: stack_push;
        tt_localUserRulePointer, tt_importedUserRulePointer, tt_intrinsicRulePointer, tt_rulePutCacheValue, tt_aggregatorConstructor : case cTokType[1] of
          tt_braceOpen, tt_parList_constructor, tt_listToParameterList: startOrPushParameterList;
          tt_parList: applyRule(first^.next,first^.next^.next);
          tt_braceClose,tt_listBraceClose,tt_comparatorEq..tt_operatorIn,tt_EOL,tt_iifCheck,tt_iifElse,tt_separatorCnt,tt_separatorComma,tt_semicolon,
          tt_typeCheckScalar..tt_typeCheckKeyValueList, tt_localUserRulePointer_pon,tt_importedUserRulePointer_pon,tt_intrinsicRulePointer_pon,tt_identifier_pon : applyRule(nil,first^.next);
        end;
        tt_procedureBlockWhile: if (cTokType[1]=tt_braceOpen) then begin
          first^.next:=recycler.disposeToken(first^.next);
          resolveWhile;
        end;
        tt_iifCheck: if (cTokType[-1]=tt_literal) then begin
          if (P_literal(stack[length(stack)-1]^.data)^.literalType=lt_boolean)
          then resolveInlineIf(P_boolLiteral(stack[length(stack)-1]^.data)^.value)
          else raiseError(el3_evalError,'Invalid syntax for inline-if; first operand is expected to be a boolean. Instead I found a '+C_typeString[P_literal(stack[length(stack)-1]^.data)^.literalType]+': '+stack[length(stack)-1]^.singleTokenToString,first^.location);
        end else raiseError(el3_evalError,'Invalid syntax for inline-if; first operand is expected to be a boolean. Here, the first operand is not even a literal.',first^.location);
      end;
    until not(didSubstitution) or (errorLevel>=el3_evalError);
    {$ifndef debugMode}
    except
      on EOutOfMemory do begin
        raiseError(el5_systemError,'An unhandled EOutOfMemory exception was caught in reduceExpression on callDepth='+intToStr(callDepth),C_nilTokenLocation);
      end else begin
        raiseError(el5_systemError,'An unhandled, unknown exception was caught in reduceExpression on callDepth='+intToStr(callDepth),C_nilTokenLocation);
      end;
    end;
    {$endif}

    if (errorLevel>=el3_evalError) and (first<>nil) then raiseError(el3_evalError,intToStr(callDepth)+' State: '+stateToString,first^.location);

    if callDepth=0 then begin
      if first=nil then raiseError(el0_allOkay,'Reduce expression done',C_nilTokenLocation)
                   else raiseError(el0_allOkay,'Reduce expression done',first^.location);
    end;
    if (errorLevel>=el5_systemError) then begin
      while (length(stack)>0) do stack_popDestroy;
      if (callDepth=0) then begin
        raiseError(el1_note,'Cleaning up partial result after system error',C_nilTokenLocation);
        recycler.cascadeDisposeToken(first);
      end;
    end else while (length(stack)>0) do stack_popLink;
  end;

{$endif}
