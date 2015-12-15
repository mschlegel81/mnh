{$ifdef include_interface}
T_tokenArray=object
  private
    T:array of T_token;
    i:longint;
    FUNCTION getToken(CONST line:ansistring; VAR lineLocation:T_tokenLocation; CONST inPackage:P_package; VAR adapters:T_adapters; CONST retainBlanks:boolean=false):T_token;
  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE append(CONST newTok:T_token);
    PROCEDURE step(CONST package:P_package; VAR lastComment:ansistring);
    FUNCTION current:T_token;
    PROCEDURE mutateCurrentTokType(CONST newTokType:T_tokenType);
    FUNCTION atEnd:boolean;
    FUNCTION getRawTokensUndefining:T_rawTokenArray;
    PROCEDURE tokenizeAll(CONST inputString:ansistring; CONST location:T_tokenLocation; CONST inPackage:P_package; VAR adapters:T_adapters; CONST retainBlanks:boolean);
    PROCEDURE tokenizeAll(CONST provider:P_codeProvider; CONST inPackage:P_package; VAR adapters:T_adapters);
end;

{$endif}
{$ifdef include_implementation}
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
      if (i<length(t)) then case t[i].tokType of
        tt_EOL: if (t[i].txt<>'') then begin
           if lastComment=''
           then lastComment:=t[i].txt
           else lastComment:=lastComment+C_lineBreakChar+t[i].txt;
        end;
        tt_identifier: if (i<length(t)-1) and (t[i+1].tokType=tt_identifier_pon) and (package<>nil) and package^.isImportedOrBuiltinPackage(t[i].txt) then begin
          //resolve ambiguous notation "x.y" to qualified identifier "x.y" or "x .y" (pseudo-object-notation)
          inc(i);
          t[i].txt:=t[i-1].txt+'.'+t[i].txt;
          t[i].tokType:=tt_identifier;
          package^.resolveRuleId(t[i],nil);
        end else if package<>nil then package^.resolveRuleId(t[i],nil);
        tt_each,tt_parallelEach: if (i<length(t)-3) and
         (t[i+1].tokType=tt_braceOpen) and
         (t[i+2].tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule]) and
         (t[i+3].tokType=tt_separatorComma) then begin
           //resolve each ( i , ... to each(i, ... (each with identifier; expected in further processing!)
          t[i+3].tokType:=t[i].tokType;
          t[i+3].txt:=t[i+2].txt;
          t[i+3].data:=nil;
          inc(i,3); //skip tt_braceOpen, identifier and comma
        end;
        tt_operatorMinus,tt_operatorPlus: if (i<length(t)-1) and
         ((i=0) or (t[i-1].tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach, tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus])) and
         (t[i+1].tokType in [tt_literal,tt_identifier,tt_braceOpen,tt_listBraceOpen,tt_expBraceOpen,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule,tt_parameterIdentifier]) then begin
          if t[i].tokType=tt_operatorMinus
          then t[i].tokType:=tt_unaryOpMinus //unary - is special token
          else inc(i);                       //unary + is skipped
        end;
      end; //case //end if
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

FUNCTION T_tokenArray.getRawTokensUndefining:T_rawTokenArray;
  VAR k:longint;
      lastComment:ansistring='';
      intrinsicFuncPtr:T_intFuncCallback;
  begin
    setLength(result,0);
    repeat
      step(nil,lastComment);
      if not(atEnd) then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=current.getRawToken;
        with result[length(result)-1] do if tokType in [tt_identifier,tt_identifier_pon] then begin
          if intrinsicRuleMap.containsKey(txt,intrinsicFuncPtr) then begin
            if tokType=tt_identifier_pon
            then tokType:=tt_intrinsicRule_pon
            else tokType:=tt_intrinsicRule;
          end;
        end;
      end;
    until atEnd;
    for k:=0 to length(t)-1 do t[k].undefine;
  end;

FUNCTION T_tokenArray.getToken(CONST line:ansistring; VAR lineLocation:T_tokenLocation; CONST inPackage:P_package; VAR adapters:T_adapters; CONST retainBlanks:boolean=false):T_token;
  VAR parsedLength:longint=0;

  PROCEDURE fail(CONST message:ansistring);
    begin
      adapters.raiseCustomMessage(mt_el4_parsingError,message,lineLocation);
    end;

  FUNCTION leadingId(CONST allowQualified:boolean):ansistring;
    VAR i:longint;
    begin
      i:=lineLocation.column;
      while (i<length(line)) and (line[i+1] in ['a'..'z','A'..'Z','0'..'9','_',C_ID_QUALIFY_CHARACTER]) do begin
        inc(i);
        if line[i]=C_ID_QUALIFY_CHARACTER then begin
          if (i<length(line)) and (line[i+1]=C_ID_QUALIFY_CHARACTER) then begin
            parsedLength:=i-lineLocation.column;
            exit(copy(line,lineLocation.column,parsedLength));
          end else if not(allowQualified) then begin
            parsedLength:=i-lineLocation.column;
            exit(copy(line,lineLocation.column,parsedLength));
          end;
        end;
      end;
      parsedLength:=i-lineLocation.column+1;
      result:=copy(line,lineLocation.column,parsedLength);
    end;

  FUNCTION startsWith(CONST c:char):boolean;
    begin result:=line[lineLocation.column]=c; end;
  FUNCTION startsWith(CONST prefix:string):boolean;
    begin result:=copy(line,lineLocation.column,length(prefix))=prefix; end;

  VAR id:ansistring;
      tt:T_tokenType;
  begin
    result.location:=lineLocation;
    result.txt:='';
    result.tokType:=tt_EOL;
    result.data:=nil;
    result.next:=nil;
    if retainBlanks then begin
      while (lineLocation.column<=length(line)) and
            (line[lineLocation.column] in [' ',C_lineBreakChar,C_tabChar,C_carriageReturnChar]) do begin
        result.txt:=result.txt+line[lineLocation.column];
        inc(lineLocation.column);
      end;
      if result.txt<>'' then begin
        result.tokType:=tt_blank;
        exit(result);
      end;
    end else begin
      while (lineLocation.column<=length(line)) and
            (line[lineLocation.column] in [' ',C_lineBreakChar,C_tabChar,C_carriageReturnChar]) do inc(lineLocation.column);
      result.location:=lineLocation;
    end;
    if length(line)<lineLocation.column then exit(result);
    case line[lineLocation.column] of
      '0'..'9': begin
        result.data:=parseNumber(line,lineLocation.column, false,parsedLength);
        if parsedLength<=0 then fail('Cannot parse numeric literal '+line)
                           else result.tokType:=tt_literal;
      end;
      '"','''': begin
        id:=unescapeString(line,lineLocation.column,parsedLength);
        if parsedLength=0 then fail('Cannot parse string literal '+line)
        else begin
          result.tokType:=tt_literal;
          result.data:=newStringLiteral(id,copy(line,lineLocation.column,parsedLength));
        end;
      end;
      '$': begin
        result.txt:=leadingId(false);
        parsedLength:=length(result.txt);
        result.tokType:=tt_parameterIdentifier;
      end;
      'a'..'z','A'..'Z': begin
        id:=leadingId(false);
        result.tokType:=tt_identifier;
        for tt:=low(T_tokenType) to high(T_tokenType) do
        if id=C_tokenString[tt] then begin
          result.tokType:=tt;
          result.txt:='';
        end;
        if result.tokType=tt_identifier then begin
          if      id=C_boolText[true]  then begin result.tokType:=tt_literal; result.data:=newBoolLiteral(true); end
          else if id=C_boolText[false] then begin result.tokType:=tt_literal; result.data:=newBoolLiteral(false); end
          else if id=C_nanText         then begin result.tokType:=tt_literal; result.data:=newRealLiteral(Nan); end
          else if id=C_infText         then begin result.tokType:=tt_literal; result.data:=newRealLiteral(Infinity); end
          else if id=C_voidText        then begin result.tokType:=tt_literal; result.data:=newVoidLiteral; end
          else begin
            result.txt:=id;
            result.data:=inPackage;
          end;
        end;
      end;
      '/': if startsWith('//') then begin //comments
        parsedLength:=2;
        while (parsedLength+lineLocation.column<=length(line)) and not(line[parsedLength+lineLocation.column] in [C_lineBreakChar,C_carriageReturnChar]) do inc(parsedLength);
        if retainBlanks then begin
          result.tokType:=tt_blank;
          result.txt:=copy(line,lineLocation.column,length(line));
        end else begin
          result.tokType:=tt_EOL;
          if      startsWith(DOC_COMMENT_PREFIX)         then result.txt:=trim(copy(line,length(DOC_COMMENT_PREFIX)+lineLocation.column,length(line)-length(DOC_COMMENT_PREFIX)+1-lineLocation.column))
          else if startsWith(SPECIAL_COMMENT_BLOB_BEGIN) then result.txt:=SPECIAL_COMMENT_BLOB_BEGIN
          else if startsWith(SPECIAL_COMMENT_BLOB_END)   then result.txt:=SPECIAL_COMMENT_BLOB_END;
        end;
      end else if startsWith(C_tokenString[tt_cso_assignDiv]) then begin
        parsedLength:=2;
        result.tokType:=tt_cso_assignDiv;
      end else begin
        parsedLength:=1;
        result.tokType:=tt_operatorDivReal;
      end;
      '.': if startsWith(C_tokenString[tt_optionalParameters]) then begin
        parsedLength:=length(C_tokenString[tt_optionalParameters]);
        result.tokType:=tt_optionalParameters;
      end else if startsWith(C_tokenString[tt_separatorCnt]) then begin
        parsedLength:=length(C_tokenString[tt_separatorCnt]);
        result.tokType:=tt_separatorCnt;
      end else if (length(line)>=lineLocation.column+1) and (line[lineLocation.column+1] in ['a'..'z','A'..'Z']) then begin
        id:=copy(leadingId(false),2,length(line));
        parsedLength:=1+length(id);
        result.txt:=id;
        result.tokType:=tt_identifier_pon;
        result.data:=inPackage;
      end else fail('Cannot parse: '+line);
      ':': if startsWith(':=') then begin
        parsedLength:=2;
        result.tokType:=tt_assign;
      end else if (length(line)>=lineLocation.column+3) and (line[lineLocation.column+1] in ['b','e','i','l','n','s','r','k']) then begin
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
      ';':                                                        begin parsedLength:=1; result.tokType:=tt_semicolon;             end;
      '}':                                                        begin parsedLength:=1; result.tokType:=tt_expBraceClose;         end;
      '{':                                                        begin parsedLength:=1; result.tokType:=tt_expBraceOpen;          end;
      '^':                                                        begin parsedLength:=1; result.tokType:=tt_operatorPot;           end;
      ']':                                                        begin parsedLength:=1; result.tokType:=tt_listBraceClose;        end;
      '[':                                                        begin parsedLength:=1; result.tokType:=tt_listBraceOpen;         end;
      '?':                                                        begin parsedLength:=1; result.tokType:=tt_iifCheck;              end;
      ',':                                                        begin parsedLength:=1; result.tokType:=tt_separatorComma;        end;
      '@':                                                        begin parsedLength:=1; result.tokType:=tt_listToParameterList;   end;
      ')':                                                        begin parsedLength:=1; result.tokType:=tt_braceClose;            end;
      '(':                                                        begin parsedLength:=1; result.tokType:=tt_braceOpen;             end;
      '|': if startsWith(C_tokenString[tt_cso_assignAppend])      then begin parsedLength:=2; result.tokType:=tt_cso_assignAppend;      end
                                                                  else begin parsedLength:=1; result.tokType:=tt_operatorConcat;        end;
      '+': if startsWith(C_tokenString[tt_cso_assignPlus])        then begin parsedLength:=2; result.tokType:=tt_cso_assignPlus;        end
                                                                  else begin parsedLength:=1; result.tokType:=tt_operatorPlus;          end;
      '&': if startsWith(C_tokenString[tt_cso_assignStrConcat])   then begin parsedLength:=2; result.tokType:=tt_cso_assignStrConcat;   end
                                                                  else begin parsedLength:=1; result.tokType:=tt_operatorStrConcat;     end;
      '-': if startsWith(C_tokenString[tt_declare])               then begin parsedLength:=2; result.tokType:=tt_declare;               end else
           if startsWith(C_tokenString[tt_cso_assignMinus])       then begin parsedLength:=2; result.tokType:=tt_cso_assignMinus;       end
                                                                  else begin parsedLength:=1; result.tokType:=tt_operatorMinus;         end;
      '*': if startsWith('**')                                    then begin parsedLength:=2; result.tokType:=tt_operatorPot;           end else
           if startsWith(C_tokenString[tt_cso_assignMult])        then begin parsedLength:=2; result.tokType:=tt_cso_assignMult;        end
                                                                  else begin parsedLength:=1; result.tokType:=tt_operatorMult;          end;
      '>': if startsWith(C_tokenString[tt_comparatorGeq])         then begin parsedLength:=2; result.tokType:=tt_comparatorGeq;         end
                                                                  else begin parsedLength:=1; result.tokType:=tt_comparatorGrt;         end;
      '=': if startsWith(C_tokenString[tt_typeCheckEmptyList])    then begin parsedLength:=3; result.tokType:=tt_typeCheckEmptyList;    end else
           if startsWith(C_tokenString[tt_comparatorListEq])      then begin parsedLength:=2; result.tokType:=tt_comparatorListEq;      end
                                                                  else begin parsedLength:=1; result.tokType:=tt_comparatorEq;          end;
      '<': if startsWith(C_tokenString[tt_typeCheckNonemptyList]) then begin parsedLength:=4; result.tokType:=tt_typeCheckNonemptyList; end else
           if startsWith('<>')                                    then begin parsedLength:=2; result.tokType:=tt_comparatorNeq;         end else
           if startsWith('<=')                                    then begin parsedLength:=2; result.tokType:=tt_comparatorLeq;         end
                                                                  else begin parsedLength:=1; result.tokType:=tt_comparatorLss;         end;
      '%': if startsWith(C_tokenString[tt_operatorExtractL3])     then begin parsedLength:=4; result.tokType:=tt_operatorExtractL3;     end else
           if startsWith(C_tokenString[tt_operatorExtractL2])     then begin parsedLength:=3; result.tokType:=tt_operatorExtractL2;     end else
           if startsWith(C_tokenString[tt_operatorExtractL1])     then begin parsedLength:=2; result.tokType:=tt_operatorExtractL1;     end
                                                                  else begin parsedLength:=1; result.tokType:=tt_operatorExtractL0;     end;
      '!': if startsWith('!=')                                    then begin parsedLength:=2; result.tokType:=tt_comparatorNeq;         end
                                                                  else begin fail('Cannot parse: '+copy(line,lineLocation.column,20)+' (first char is "'+line[lineLocation.column]+'"=#'+intToStr(ord(line[lineLocation.column]))+')'); lineLocation.column:=length(line)+1; end;
      else begin
        fail('Cannot parse: '+copy(line,lineLocation.column,20)+' (first char is "'+line[lineLocation.column]+'"=#'+intToStr(ord(line[lineLocation.column]))+')');
        lineLocation.column:=length(line)+1;
      end;
    end;
    if parsedLength>0 then inc(lineLocation.column,parsedLength);
  end;

PROCEDURE T_tokenArray.tokenizeAll(CONST inputString:ansistring; CONST location:T_tokenLocation;  CONST inPackage:P_package; VAR adapters:T_adapters; CONST retainBlanks:boolean);
  VAR next:T_token;
      workLocation:T_tokenLocation;
  begin
    workLocation:=location;
    workLocation.column:=1;
    while (workLocation.column<=length(inputString)) and (adapters.noErrors) do begin
      next:=getToken(inputString,workLocation,inPackage,adapters,retainBlanks);
      if (next.tokType<>tt_EOL) or (next.txt<>'') then begin
        if next.txt=SPECIAL_COMMENT_BLOB_BEGIN then begin
          workLocation.column:=length(inputString)+1;
          if retainBlanks then append(next);
        end else begin
          next.location:=workLocation;
          append(next);
        end;
      end;
    end;
  end;

FUNCTION tokenizeAllReturningRawTokens(CONST inputString:ansistring):T_rawTokenArray;
  VAR tokenArray:T_tokenArray;
      location:T_tokenLocation;
      adapters:T_adapters;
  begin
    location:=C_nilTokenLocation;
    location.column:=1;
    tokenArray.create;
    adapters.create;
    tokenArray.tokenizeAll(inputString,location,nil,adapters,true);
    adapters.destroy;
    result:=tokenArray.getRawTokensUndefining;
    tokenArray.destroy;
  end;

PROCEDURE T_tokenArray.tokenizeAll(CONST provider:P_codeProvider; CONST inPackage:P_package; VAR adapters:T_adapters);
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
          append(next);
        end else begin
          if blob.text=C_carriageReturnChar
          then blob.text:=                          lines[lineIndex]
          else blob.text:=blob.text+C_lineBreakChar+lines[lineIndex];
        end;
      end else while (location.column<=length(lines[lineIndex])) and (adapters.noErrors) do begin
        next:=getToken(lines[lineIndex],location,inPackage,adapters);
        if (next.tokType<>tt_EOL) or (next.txt<>'') then begin
          if next.txt=SPECIAL_COMMENT_BLOB_BEGIN then with blob do begin
            level:=1;
            start:=location;
            text:=C_carriageReturnChar;
            lines[lineIndex]:='';
          end else begin
            append(next);
          end;
        end;
      end;
    end;
    //Handle unfinished blobs
    if blob.level>0 then begin
      next.create;
      next.define(blob.start,'',tt_literal,newStringLiteral(blob.text));
      append(next);
    end;
  end;

PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_package; VAR context:T_evaluationContext);
  VAR t:P_token;
      rule:P_rule;
  begin
    t:=first;
    while t<>nil do begin
      case t^.tokType of
        tt_identifier,tt_localUserRule,tt_importedUserRule: if inPackage<>nil then begin
          if t^.data=nil then t^.data:=inPackage;
          if (t^.next<>nil) and (t^.next^.tokType in [tt_assign,tt_cso_assignPlus..tt_cso_assignAppend]) then begin
            inPackage^.resolveRuleId(t^,nil);
            if t^.tokType<>tt_identifier then begin
              if t^.tokType=tt_localUserRule then begin
                rule:=t^.data;
                if rule^.ruleType in [rt_mutable_private,rt_mutable_public,rt_persistent_private,rt_persistent_public] then begin
                  t^.data:=rule;
                  t^.tokType:=t^.next^.tokType;
                  if t^.tokType=tt_assign then t^.tokType:=tt_mutate;
                  t^.txt:=t^.txt;
                  t^.next:=context.disposeToken(t^.next);
                end else context.adapters^.raiseError('You can only mutate local mutable rules! Rule '+t^.txt+' is not mutable',t^.next^.location);
              end else context.adapters^.raiseError('You can only mutate local mutable rules! Rule '+t^.txt+' is a '+C_ruleTypeString[t^.tokType],t^.next^.location);
            end else context.adapters^.raiseError('Cannot resolve identifier "'+t^.txt+'".',t^.location);
          end;
        end;
        tt_modifier_local: if (t^.next<>nil) and (t^.next^.tokType=tt_blockLocalVariable) and (t^.next^.next<>nil) and (t^.next^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignNewBlockLocal;
          t^.data:=nil;
          t^.txt:=t^.next^.txt;
          t^.next:=context.disposeToken(t^.next);
          t^.next:=context.disposeToken(t^.next);
        end;
        tt_blockLocalVariable: if (t^.next<>nil) and (t^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignExistingBlockLocal;
          t^.data:=nil;
          t^.next:=context.disposeToken(t^.next);
        end else if (t^.next<>nil) and (t^.next^.tokType in [tt_cso_assignPlus..tt_cso_assignAppend]) then begin
          t^.tokType:=t^.next^.tokType;
          t^.data:=nil;
          t^.next:=context.disposeToken(t^.next);
        end;
        end;
      t:=t^.next;
    end;
  end;

PROCEDURE digestInlineExpression(VAR rep:P_token; VAR context:T_evaluationContext);
  VAR t,prev,inlineRule:P_token;
      bracketLevel:longint=0;
      inlineSubRule:P_subrule;
  begin
    predigest(rep,nil,context);
    if (rep^.tokType<>tt_expBraceOpen) then begin
      context.adapters^.raiseError('Error creating subrule from inline; expression does not start with "{"',rep^.location);
      exit;
    end;
    t:=rep^.next; prev:=rep;
    inlineRule:=t;
    while (t<>nil) and ((t^.tokType<>tt_expBraceClose) or (bracketLevel>0)) do begin
      case t^.tokType of
        tt_expBraceOpen: begin
          digestInlineExpression(t,context);
          if t^.tokType=tt_expBraceOpen then inc(bracketLevel);
        end;
      end;
      prev:=t;
      t:=t^.next;
    end;
    if (t=nil) or (t^.tokType<>tt_expBraceClose) then begin
      context.adapters^.raiseError('Error creating subrule from inline; expression does not end with an }',rep^.location);
      exit;
    end;

    rep^.next:=t^.next; //remove expression from parent expression
    prev^.next:=nil; //unlink closing curly bracket
    context.disposeToken(t); //dispose closing curly bracket
    if context.adapters^.noErrors then begin
      new(inlineSubRule,createFromInline(inlineRule,context));
      if context.adapters^.noErrors then begin
        rep^.tokType:=tt_literal;
        rep^.data:=newExpressionLiteral(inlineSubRule);
      end else dispose(inlineSubRule,destroy);
    end;
  end;

PROCEDURE reduceExpression(VAR first:P_token; CONST callDepth:word; VAR context:T_evaluationContext);
  VAR stack:array of P_token;
      newLit:P_literal;
      didSubstitution:boolean;
      {$ifdef fullVersion}reportableStep:boolean;{$endif}
      cTokType:array[-1..2] of T_tokenType;

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
      context.disposeToken(stack[length(stack)-1]);
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

  TYPE T_bodyParts=array of record first,last:P_token; end;

  FUNCTION getBodyParts(CONST first:P_token; CONST initialBracketLevel:longint; OUT closingBracket:P_token):T_bodyParts;
    VAR t,p:P_token;
        bracketLevel,i:longint;
    begin
      closingBracket:=nil;
      bracketLevel:=initialBracketLevel;
      t:=first; p:=nil;
      if (first^.next<>nil) and (first^.next^.tokType<>tt_separatorComma) then begin
        setLength(result,1);
        result[0].first:=first^.next;
      end else begin
        context.adapters^.raiseError('Invalid special construct; Cannot find closing bracket.',first^.location);
        setLength(result,0);
        exit;
      end;
      while (t<>nil) and not((t^.tokType=tt_braceClose) and (bracketLevel=1)) do begin
        case t^.tokType of
          tt_braceOpen,tt_each,tt_parallelEach,tt_listBraceOpen ,tt_expBraceOpen ,tt_begin: inc(bracketLevel);
          tt_braceClose                       ,tt_listBraceClose,tt_expBraceClose,tt_end  : dec(bracketLevel);
          tt_separatorComma: if bracketLevel=1 then begin
            result[length(result)-1].last:=p; //end of body part is token before comma
            setLength(result,length(result)+1);
            result[length(result)-1].first:=t^.next; //start of next body part is token after comma
          end;
        end;
        p:=t; t:=t^.next;
      end;
      result[length(result)-1].last:=p; //end of body part is token before comma
      if (t=nil) or (t^.tokType<>tt_braceClose) or (bracketLevel<>1) then begin
        context.adapters^.raiseError('Invalid special construct; Cannot find closing bracket.',first^.location);
        setLength(result,0);
        exit;
      end;
      closingBracket:=t;
      for i:=0 to length(result)-1 do begin
        if result[i].last^.next<>closingBracket then context.disposeToken(result[i].last^.next);
        result[i].last^.next:=nil;
      end;
    end;

  PROCEDURE raiseLazyBooleanError(CONST location:T_tokenLocation);
    begin
      context.adapters^.raiseError('Lazy boolean operators can only be applied to scalar booleans.',location);
    end;

  PROCEDURE resolveEach(inParallel:boolean);
    VAR bracketClosingEach:P_token;
        bodyRule:array of P_subrule;
        aggregatorLocation:T_tokenLocation;
        aggregatorToken:T_tokenType;
        aggregatorExpression:P_expressionLiteral;
        aggregatorPresent:(no,yes_as_operator,yes_as_expression)=no;
        maxFutureForwardInitialization:longint=16;

    FUNCTION parseBodyOk:boolean;
      VAR i:longint;
          t:P_token;
          bodyParts:T_bodyParts;
      begin
        //first token is <each>-Token
        //find closing bracket and body parts
        bodyParts:=getBodyParts(first,0,bracketClosingEach);
        if bracketClosingEach=nil then exit(false);

        //process aggregator part (if any)----------------------------------------------
        aggregatorPresent:=no;
        t:=bodyParts[0].first;
        if t^.tokType=tt_aggregatorConstructor then begin
          reduceExpression(bodyParts[0].first,callDepth+1,context);
          bodyParts[0].last:=bodyParts[0].first^.last;
          t:=bodyParts[0].first;
        end;
        if t^.next=nil then begin
          case t^.tokType of
            tt_comparatorEq..tt_operatorIn: begin
              aggregatorToken:=t^.tokType;
              aggregatorPresent:=yes_as_operator;
              aggregatorLocation:=t^.location;
            end;
            tt_aggregatorExpressionLiteral: begin
              aggregatorExpression:=P_expressionLiteral(t^.data);
              aggregatorExpression^.rereference;
              aggregatorPresent:=yes_as_expression;
              aggregatorLocation:=t^.location
            end;
          end;
        end;
        if aggregatorPresent<>no then begin
          context.disposeToken(bodyParts[0].first);
          for i:=0 to length(bodyParts)-2 do bodyParts[i]:=bodyParts[i+1];
          setLength(bodyParts,length(bodyParts)-1);
          first^.tokType:=tt_braceOpen;
          bracketClosingEach^.tokType:=tt_braceClose;
        end;
        //----------------------------------------------process aggregator part (if any)
        //process other body parts (if any)---------------------------------------------
        setLength(bodyRule       ,length(bodyParts));
        for i:=0 to length(bodyParts)-1 do
          new(bodyRule[i],createForEachBody(first^.txt,bodyParts[i].first,context));
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
          context.adapters^.raiseError('Error performing aggregation in each-construct - NIL literal encountered',eachLocation);
          exit;
        end else if L^.literalType=lt_void then exit;
        case aggregatorPresent of
          no: begin
            if resultLiteral=nil then resultLiteral:=newListLiteral;
            P_listLiteral(resultLiteral)^.append(L,true,context.adapters^);
          end;
          yes_as_operator: case aggregatorToken of
            tt_operatorConcat: begin
              if resultLiteral=nil then resultLiteral:=newListLiteral;
              if (L^.literalType in [lt_error, lt_boolean, lt_int, lt_real, lt_string, lt_expression])
              then P_listLiteral(resultLiteral)^.append   (L, true,context.adapters^)
              else P_listLiteral(resultLiteral)^.appendAll(P_listLiteral(L));
            end;
            tt_operatorLazyAnd: if resultLiteral=nil then begin
              if (L^.literalType=lt_boolean) then booleanResult:=P_boolLiteral(L)^.value
              else raiseLazyBooleanError(aggregatorLocation);
              if not(booleanResult) then begin
                lazyAbort:=true;
                resultLiteral:=L;
                L^.rereference;
              end;
            end;
            tt_operatorLazyOr: if resultLiteral=nil then begin
              if (L^.literalType=lt_boolean) then booleanResult:=P_boolLiteral(L)^.value
              else raiseLazyBooleanError(aggregatorLocation);
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
                                        eachLocation,context.adapters^);
                if newLit<>nil then begin
                  disposeLiteral(resultLiteral);
                  resultLiteral:=newLit;
                end else context.adapters^.raiseError('Error performing aggregation in each-construct with aggregator ('+C_tokenString[aggregatorToken]+')',eachLocation);
              end;
            end;
          end;
          yes_as_expression: begin
            if resultLiteral=nil then begin
              resultLiteral:=L; L^.rereference;
            end else begin
              newLit:=P_subrule(aggregatorExpression^.value)^.directEvaluateAggregator(resultLiteral,L,aggregatorExpression,callDepth+1,context);
              if newLit<>nil then begin
                disposeLiteral(resultLiteral);
                resultLiteral:=newLit;
              end else context.adapters^.raiseError('Error performing aggregation in each-construct with aggregator '+aggregatorExpression^.toString,eachLocation);
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
            context.adapters^.raiseError('Error performing finalizing aggregation each-construct - NIL literal encountered',eachLocation);
          end else first^.data:=newListLiteral;
        end else begin
          if resultLiteral^.literalType=lt_listWithError then begin
            disposeLiteral(resultLiteral);
            resultLiteral:=newErrorLiteral;
          end;
          first^.data:=resultLiteral;
        end;
        first^.txt:='';
        first^.next:=context.disposeToken(bracketClosingEach);
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
          while (future[nextFuture]^.state in [fts_pending,fts_evaluating]) do pendingTasks.activeDeqeue(callDepth+1,context);
          addToAggregation(future[nextFuture]^.getResultAsLiteral(context),true);
          future[nextFuture]^.free(context);
          dispose(future[nextFuture],proFormaDestroy);
          inc(nextFuture);
        end;
        if finalizing then begin
          finalizeAggregation;
          setLength(future,0);
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
        for i:=0 to itList^.size-1 do for j:=0 to length(bodyRule)-1 do begin
          if (context.adapters^.noErrors) and not(lazyAbort) then begin
            future[initializedFutures]:=pendingTasks.enqueue(bodyRule[j],itList^.value(i),context);
            if future[initializedFutures]^.state=fts_errorOnInitialization then context.adapters^.raiseError('Error initializing future.',eachLocation);
          end else future[initializedFutures]:=nil;
          inc(initializedFutures);
          if ((initializedFutures and 255)=0) or (pendingTasks.queuedCount>maxFutureForwardInitialization) then aggregateFutures(false);
        end;
      end;

    begin
      eachLocation:=first^.location;
      if P_literal(first^.data)^.literalType in [lt_error,lt_listWithError,lt_void] then begin
        context.adapters^.raiseError('Cannot apply each construct to void literal',eachLocation);
        exit
      end;
      if callDepth>STACK_DEPTH_LIMIT then begin
        context.adapters^.raiseCustomMessage(mt_el5_systemError,'Stack overflow in (p)each construct.',eachLocation);
        exit;
      end;
      if not(parseBodyOk) then exit;
      if (P_literal(first^.data)^.literalType in [lt_boolean,lt_int,lt_real,lt_string,lt_expression])
      then itList:=newOneElementListLiteral(first^.data,false,context.adapters^)
      else itList:=first^.data;
      resultLiteral:=nil;
      first^.data:=nil;
      first^.txt:='';
      //iterate over itList----------------------------------------------------------
      //t:=first;
      if (aggregatorPresent<>no) and (itList^.size=0)
      then context.adapters^.raiseError('Each construct with aggregator is invalid for empty lists to iterate',eachLocation)
      else if length(bodyRule)>0 then begin
        if inParallel then begin
          initFutures;
          aggregateFutures(true);
        end else begin
          for i:=0 to itList^.size-1 do for j:=0 to length(bodyRule)-1 do if (context.adapters^.noErrors) and not(lazyAbort) then
            addToAggregation(bodyRule[j]^.directEvaluateUnary(itList^.value(i),nil,callDepth+1,context),true);
          finalizeAggregation;
        end;
      end else begin
        if inParallel then context.adapters^.raiseNote('There is no paralellization for pEach statements without body (i.e. pure aggregators)',eachLocation);
        for i:=0 to itList^.size-1 do if context.adapters^.noErrors then addToAggregation(itList^.value(i),false);
        finalizeAggregation;
      end;
      //----------------------------------------------------------iterate over itList
      //cleanup----------------------------------------------------------------------
      if aggregatorPresent=yes_as_expression then disposeLiteral(aggregatorExpression);
      for i:=0 to length(bodyRule)-1 do dispose(bodyRule[i],destroy);
      disposeLiteral(itList);
      //----------------------------------------------------------------------cleanup
      didSubstitution:=true;
      {$ifdef fullVersion}reportableStep:=true;{$endif}
    end;

  PROCEDURE resolveWhile;
    VAR bracketClosingWhile:P_token;
        headRule:P_subrule;
        bodyRule:P_subrule;

    FUNCTION parseBodyOk:boolean;
      VAR i:longint;
          bodyParts:T_bodyParts;
          emptyPattern:T_pattern;
      begin
        //first token is <while>-Token
        //find closing bracket and body parts
        bodyParts:=getBodyParts(first,1,bracketClosingWhile);
        if bracketClosingWhile=nil then exit(false);
        if length(bodyParts)<>2 then begin
          context.adapters^.raiseError('Invalid while-construct; Exactly two arguments (head and body) are expected.',first^.location);
          exit(false);
        end;

        for i:=0 to length(bodyParts)-1 do begin
          if bodyParts[i].last^.next<>bracketClosingWhile then context.disposeToken(bodyParts[i].last^.next);
          bodyParts[i].last^.next:=nil;
        end;


        //create head/body rules------------------------------------------------
        emptyPattern.create;
        new(headRule,create(emptyPattern,bodyParts[0].first,bodyParts[0].first^.location,true,context));
        new(bodyRule,create(emptyPattern,bodyParts[1].first,bodyParts[1].first^.location,true,context));
        emptyPattern.destroy;
        //------------------------------------------------create head/body rules
        result:=true;
      end;

    VAR whileLocation:T_tokenLocation;

    FUNCTION evaluateHead:boolean;
      VAR res:P_literal;
      begin
        res:=headRule^.directEvaluateNullary(nil,callDepth+1,context);
        if (res<>nil) and (res^.literalType=lt_boolean) then begin
          result:=P_boolLiteral(res)^.value;
        end else begin
          if res<>nil
          then context.adapters^.raiseError('The head-part of a while statement must return a scalar boolean! Instead it returned: '+res^.toString,headRule^.declaredAt)
          else context.adapters^.raiseError('Evaluation of the head-part of a while statement failed.',headRule^.declaredAt);
          result:=false;
        end;
        if res<>nil then disposeLiteral(res);
      end;

    PROCEDURE evaluateBody;
      VAR res:P_literal;
      begin
        res:=bodyRule^.directEvaluateNullary(nil,callDepth+1,context);
        if res<>nil then disposeLiteral(res);
      end;

    begin
      whileLocation:=first^.location;
      if callDepth>STACK_DEPTH_LIMIT then begin
        context.adapters^.raiseCustomMessage(mt_el5_systemError,'Stack overflow in while construct.',whileLocation);
        exit;
      end;
      if not(parseBodyOk) then exit;
      while evaluateHead and (context.adapters^.noErrors) do evaluateBody;
      first^.txt:='';
      first^.tokType:=tt_literal;
      first^.data:=newVoidLiteral;
      first^.next:=context.disposeToken(bracketClosingWhile);
      //cleanup----------------------------------------------------------------------
      dispose(headRule,destroy);
      dispose(bodyRule,destroy);
      //----------------------------------------------------------------------cleanup
      didSubstitution:=true;
      {$ifdef fullVersion}reportableStep:=true;{$endif}
    end;

  PROCEDURE resolveWhen;
    VAR bodyParts:T_bodyParts;
        closingBracket:P_token;

        conditionIdx,i:longint;
        resultIndex:longint=-1;

        locationForComplaint:T_tokenLocation;
    begin
      bodyParts:=getBodyParts(first,1,closingBracket);
      if closingBracket=nil then exit;

      for i:=0 to length(bodyParts)-1 do begin
        if bodyParts[i].last^.next<>closingBracket then context.disposeToken(bodyParts[i].last^.next);
        bodyParts[i].last^.next:=nil;
      end;

      conditionIdx:=0;
      while (conditionIdx<length(bodyParts)-1) and context.adapters^.noErrors and (resultIndex<0) do begin
        locationForComplaint:=bodyParts[conditionIdx].first^.location;
        reduceExpression(bodyParts[conditionIdx].first,callDepth+1,context);

        if context.adapters^.noErrors and
          (bodyParts[conditionIdx].first<>nil) and
          (bodyParts[conditionIdx].first^.next=nil) and
          (bodyParts[conditionIdx].first^.tokType=tt_literal) and
          (P_literal(bodyParts[conditionIdx].first^.data)^.literalType=lt_boolean) then begin
          if P_boolLiteral(bodyParts[conditionIdx].first^.data)^.value then resultIndex:=conditionIdx+1;
        end else context.adapters^.raiseError('Conditions in switch construct must return scalar booleans.',locationForComplaint);
        inc(conditionIdx,2);
      end;
      //Fallback to default case
      if (resultIndex<0) and //no result found and
        odd(length(bodyParts)) //there is a default case
      then resultIndex:=length(bodyParts)-1;

      for i:=0 to length(bodyParts)-1 do if (i<>resultIndex) then context.cascadeDisposeToken(bodyParts[i].first);

      if resultIndex<0 then begin
        first^.tokType:=tt_literal; //mutate when-token to literal
        first^.data:=newVoidLiteral;
        first^.next:=closingBracket^.next;
        context.disposeToken(closingBracket);
      end else begin
        first^.tokType:=tt_braceOpen; //mutate when-token to (
        first^.next:=bodyParts[resultIndex].first;
        bodyParts[resultIndex].last^.next:=closingBracket;
      end;
      setLength(bodyParts,0);
      didSubstitution:=true;
      {$ifdef fullVersion}reportableStep:=true;{$endif}
    end;

  PROCEDURE applyRule(CONST parameterListToken:P_token; CONST firstTokenAfterCall:P_token);
    VAR firstReplace,lastReplace:P_token;
        newLiteral:P_literal;
        parameterListLiteral:P_listLiteral;
        inlineRule:P_subrule;
    begin
      if parameterListToken=nil then parameterListLiteral:=nil
                                else parameterListLiteral:=parameterListToken^.data;
      if (first^.tokType in [tt_localUserRule,tt_importedUserRule]) then begin
        if not(P_rule(first^.data)^.replaces(parameterListLiteral,firstReplace,lastReplace,callDepth,first^.tokType=tt_localUserRule,context)) then begin
          if parameterListLiteral=nil
          then context.adapters^.raiseError('Cannot apply user defined rule '+P_rule(first^.data)^.id+' to empty parameter list',first^.location)
          else begin
            if (P_rule(first^.data)^.id='main')
            then context.adapters^.raiseCustomMessage(mt_el3_noMatchingMain,'Cannot apply user defined rule '+P_rule(first^.data)^.id+' to parameter list '+parameterListLiteral^.toParameterListString(true),first^.location)
            else context.adapters^.raiseError('Cannot apply user defined rule '+P_rule(first^.data)^.id+' to parameter list '+parameterListLiteral^.toParameterListString(true),first^.location);
          end;
          exit;
        end;
      end else if (first^.tokType=tt_rulePutCacheValue) then begin
        newLiteral:=P_rule(first^.data)^.doPutCache(parameterListLiteral,context);
        firstReplace:=context.newToken(first^.location,'',tt_literal,newLiteral);
        lastReplace:=firstReplace;
      end else if (first^.tokType=tt_aggregatorConstructor) then begin
        if (parameterListLiteral<>nil) and (parameterListLiteral^.size=1) and (parameterListLiteral^.value(0)^.literalType=lt_expression) then begin

          if parameterListLiteral^.value(0)^.getReferenceCount=1 then begin
            newLiteral:=parameterListLiteral^.value(0);
            newLiteral^.rereference;
            inlineRule:=P_expressionLiteral(newLiteral)^.value;
          end else begin
            new(inlineRule,clone(P_subrule(P_expressionLiteral(parameterListLiteral^.value(0))^.value)));
            newLiteral:=newExpressionLiteral(inlineRule);
          end;
          inlineRule^.pattern.appendOptional;

          firstReplace:=context.newToken(first^.location,'',tt_aggregatorExpressionLiteral,newLiteral);

          lastReplace:=firstReplace;
        end else context.adapters^.raiseError('Aggregators can only be constructed from expression literals!',first^.location);
      end else if (first^.tokType=tt_intrinsicRule) then begin
        newLiteral:=T_intFuncCallback(first^.data)(parameterListLiteral,first^.location,context);
        if newLiteral<>nil then begin
          firstReplace:=context.newToken(first^.location,'',tt_literal,newLiteral);
          lastReplace:=firstReplace;
        end else if not(context.adapters^.noErrors) then exit else begin
          if parameterListLiteral=nil
          then context.adapters^.raiseError('Cannot apply intrinsic rule '+first^.txt+' to empty parameter list',first^.location)
          else context.adapters^.raiseError('Cannot apply intrinsic rule '+first^.txt+' to parameter list '+parameterListLiteral^.toParameterListString(true),first^.location);
          exit;
        end;
      end else if (first^.tokType in [tt_literal,tt_aggregatorExpressionLiteral]) and (P_literal(first^.data)^.literalType=lt_expression) then begin
        inlineRule:=P_expressionLiteral(first^.data)^.value;
        //failing "replaces" for inline rules will raise evaluation error.
        if not(inlineRule^.replaces(parameterListLiteral,P_expressionLiteral(first^.data),firstReplace,lastReplace,context)) then exit;
      end else begin
        context.adapters^.raiseError('Trying to apply a rule which is no rule!',first^.location);
        exit;
      end;
      context.disposeToken(first);
      if parameterListToken<>nil then context.disposeToken(parameterListToken);
      first:=firstReplace;
      lastReplace^.next:=firstTokenAfterCall;
      didSubstitution:=true;
      {$ifdef fullVersion}reportableStep:=true;{$endif}
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
          tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck,tt_expBraceOpen ,tt_begin: inc(bracketLevel);
          tt_braceClose,                         tt_listBraceClose ,tt_iifElse ,tt_expBraceClose,tt_end  : dec(bracketLevel);
        end;
        prev:=p; p:=p^.next;
      end;
      if not((p<>nil) and (p^.tokType=tt_iifElse) and (bracketLevel=0)) then begin
        stack_popLink;
        context.adapters^.raiseError('Cannot evaluate inline-if; cannot locate then-marker',first^.location);
        exit;
      end;
      tokenBeforeElse:=prev;
      while (p<>nil) and not((p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon]) and (bracketLevel=-1)) do begin
        case p^.tokType of
          tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck,tt_expBraceOpen ,tt_begin: inc(bracketLevel);
          tt_braceClose,                         tt_listBraceClose ,tt_iifElse ,tt_expBraceClose,tt_end  : dec(bracketLevel);
        end;
        prev:=p; p:=p^.next;
      end;
      if  not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon]) and (bracketLevel=-1)) then begin
        stack_popLink;
        context.adapters^.raiseError('Cannot evaluate inline-if; cannot locate end of then-expression',first^.location);
        exit;
      end;
      lastThen:=prev;
      if conditionLit then begin
        //take then-subexpression -> drop else-subexpression
        p:=tokenBeforeElse^.next;              //store tt_iifElse-token
        tokenBeforeElse^.next:=lastThen^.next; //unlink else-expression (head)
        lastThen^.next:=nil;                   //unlink else-expression (tail);
        context.cascadeDisposeToken(p);       //dispose else-expression
      end else begin
        //take else-subexpression -> drop then-subexpression
        p:=first;
        first:=tokenBeforeElse^.next^.next;
        tokenBeforeElse^.next^.next:=nil;
        context.cascadeDisposeToken(p);
      end;
      stack_popDestroy; //pop "?"
      stack_popDestroy; //pop condition literal
    end;

  PROCEDURE applyCheck;
    VAR checkResult:boolean;
    begin
      checkResult:=P_literal(first^.data)^.literalType in C_matchingTypes[first^.next^.tokType];
      first^.next:=context.disposeToken(first^.next);
      disposeLiteral(first^.data);
      first^.data:=newBoolLiteral(checkResult);
      didSubstitution:=true;
      {$ifdef fullVersion}reportableStep:=true;{$endif}
    end;

  PROCEDURE applyMutation;
    VAR newValue:P_literal;
    begin
      newValue:=first^.next^.data;
      P_rule(first^.data)^.setMutableValue(newValue,false,context);
      first:=context.disposeToken(first);
    end;

  PROCEDURE applyLocalAssignment(CONST kind:T_tokenType);
    VAR newValue:P_literal;
    begin
      newValue:=first^.next^.data;
      case kind of
        tt_assignNewBlockLocal: begin
          context.declareLocalValue(first^.txt,newValue);
          first:=context.disposeToken(first);
        end;
        tt_assignExistingBlockLocal: begin
          context.setLocalValue(first^.txt,newValue);
          first:=context.disposeToken(first);
        end;
        tt_cso_assignPlus..tt_cso_assignAppend: if first^.data=nil then begin
          newValue:=context.mutateInline(first^.txt,kind,newValue,first^.location);
          if context.adapters^.noErrors then begin
            first:=context.disposeToken(first);
            disposeLiteral(first^.data);
            first^.data:=newValue;
          end;
        end else begin
          newValue:=P_rule(first^.data)^.mutateInline(kind,newValue,first^.location,context);
          if context.adapters^.noErrors then begin
            first:=context.disposeToken(first);
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
      newFunctionToken:=context.newToken(first^.next);

      with newFunctionToken^ do begin
        tokType:=C_ponToFunc[tokType];
        if tokType=tt_identifier then begin
          if data=nil then data:=guessPackageForToken(newFunctionToken^);
          P_package(data)^.resolveRuleId(newFunctionToken^,context.adapters);
        end;
      end;
      newParameterListToken:=context.newToken(first^.next^.location,'',tt_parList,newOneElementListLiteral(first^.data,false,context.adapters^));

      first^.data:=nil; first^.tokType:=tt_identifier;
      first^.next^.data:=nil;
      oldSecond:=context.disposeToken(context.disposeToken(first));

      first:=newFunctionToken; newFunctionToken^.next:=newParameterListToken; newParameterListToken^.next:=oldSecond;

      if cTokType[2]=tt_braceOpen then begin
        newParameterListToken^.next:=context.disposeToken(oldSecond);
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
                                stack[length(stack)-1]^.location,
                                context.adapters^);
        disposeLiteral(first^.data);
        first^.data:=newLit; //store new literal in head
        stack_popDestroy; //pop operator from stack
        stack_popDestroy; //pop LHS-Literal from stack
      end else if cTokType[1]=tt_parList                                                  then applyRule(first^.next,first^.next^.next)
      else if cTokType[1] in [tt_parList_constructor,tt_braceOpen,tt_listToParameterList] then startOrPushParameterList
      else if cTokType[1] in [tt_typeCheckScalar..tt_typeCheckKeyValueList]               then applyCheck
      else if cTokType[1] in [tt_identifier_pon,tt_localUserRule_pon,tt_importedUserRule_pon,tt_intrinsicRule_pon] then pon_flip
      else stack_push_push;
    end;

  PROCEDURE operator_and_literal_push;
    VAR p:P_token;
        bracketLevel:longint=0;
    begin
      case cTokType[1] of
        tt_operatorLazyAnd: if (cTokType[0]=tt_literal) and (P_literal(first^.data)^.literalType=lt_boolean) then begin
          if (P_boolLiteral(first^.data)^.value) then begin
            //true AND ... -> ...
            first:=context.disposeToken(first); //drop true
            first:=context.disposeToken(first); //drop AND
            didSubstitution:=true;
            {$ifdef fullVersion}reportableStep:=true;{$endif}
          end else begin
            //false AND ... -> false
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse,tt_operatorLazyOr,tt_operatorOr]) and (bracketLevel=0)) do begin
              case p^.tokType of
                tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck,tt_expBraceOpen ,tt_begin: inc(bracketLevel);
                tt_braceClose,                         tt_listBraceClose ,tt_iifElse ,tt_expBraceClose,tt_end  : dec(bracketLevel);
              end;
              p:=context.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
            {$ifdef fullVersion}reportableStep:=true;{$endif}
          end;
        end else raiseLazyBooleanError(first^.next^.location);
        tt_operatorLazyOr:if (cTokType[0]=tt_literal) and (P_literal(first^.data)^.literalType=lt_boolean) then begin
          if (P_boolLiteral(first^.data)^.value) then begin
            //true OR ... -> true
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse]) and (bracketLevel=0)) do begin
              case p^.tokType of
                tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck,tt_expBraceOpen ,tt_begin: inc(bracketLevel);
                tt_braceClose,                         tt_listBraceClose ,tt_iifElse ,tt_expBraceClose,tt_end  : dec(bracketLevel);
              end;
              p:=context.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
            {$ifdef fullVersion}reportableStep:=true;{$endif}
          end else begin
            //false OR ... -> ...
            first:=context.disposeToken(first); //drop false
            first:=context.disposeToken(first); //drop OR
            didSubstitution:=true;
            {$ifdef fullVersion}reportableStep:=true;{$endif}
          end;
        end else raiseLazyBooleanError(first^.next^.location);
        else begin
          setLength(stack,length(stack)+2);
          stack[length(stack)-2]:=first; first:=first^.next;
          stack[length(stack)-1]:=first; first:=first^.next;
          didSubstitution:=true;
          {$ifdef fullVersion}reportableStep:=true;{$endif}
        end;
      end;
    end;

  PROCEDURE currentStateMessage({$ifdef fullVersion}CONST debug:boolean{$endif});
    VAR i,i0:longint;
        prevWasIdLike:boolean=false;
        p:P_token;
        tokenMessage:ansistring;
    begin
      if length(stack)>16 then begin
        tokenMessage:='... ';
        i0:=length(stack)-16;
      end else begin
        tokenMessage:=' ';
        i0:=0;
      end;
      for i:=i0 to length(stack)-1 do tokenMessage:=tokenMessage+stack[i]^.toString(prevWasIdLike,prevWasIdLike);
      i:=0;
      p:=first;
      while (p<>nil) and (i<16) do begin
        tokenMessage:=tokenMessage+p^.toString(prevWasIdLike,prevWasIdLike);
        p:=p^.next;
        inc(i);
      end;
      if (p<>nil) then tokenMessage:=tokenMessage+' ...';
      {$ifdef fullVersion}
      if debug then begin
        context.adapters^.raiseDebugMessage(tokenMessage,first^.location);
        context.adapters^.raiseCustomMessage(mt_debug_varInfo,C_carriageReturnChar,C_nilTokenLocation);
        if environment.mainPackage<>nil then environment.mainPackage^.reportVariables(context.adapters^);
        context.reportVariables;
      end else
      {$endif}
      begin
        context.adapters^.raiseCustomMessage(mt_el3_stackTrace,tokenMessage,first^.location);
      end;
    end;

{$MACRO ON}
{$define COMMON_CASES:=
tt_braceOpen, tt_parList_constructor, tt_listToParameterList: startOrPushParameterList;
tt_parList:  applyRule(first^.next,first^.next^.next);
tt_comparatorEq..tt_operatorIn: operator_and_literal_push;
tt_iifCheck: stack_push;
tt_identifier_pon,tt_importedUserRule_pon,tt_localUserRule_pon,tt_intrinsicRule_pon: pon_flip;
tt_typeCheckScalar..tt_typeCheckKeyValueList: applyCheck}

{$define FORBIDDEN_SEPARATORS:=
tt_separatorCnt:   context.adapters^.raiseError('Token .. is only allowed in list constructors.',first^.next^.location);
tt_separatorComma: context.adapters^.raiseError('Token , is only allowed in parameter lists and list constructors.',first^.next^.location)}

{$define COMMON_SEMICOLON_HANDLING:=
tt_semicolon: if (cTokType[-1]=tt_begin) then begin
  if (cTokType[2]=tt_end) then begin
    stack_popDestroy;
    first^.next:=context.disposeToken(first^.next);
    first^.next:=context.disposeToken(first^.next);
    context.scopePop;
    didSubstitution:=true;
  end else begin
    first:=context.disposeToken(first);
    first:=context.disposeToken(first);
    didSubstitution:=true;
  end;
end else context.adapters^.raiseError('Token ; is only allowed in begin-end-blocks',first^.next^.location)}

  begin
    setLength(stack,0);
    {$ifndef debugMode} try {$endif}
    {$ifdef fullVersion} reportableStep:=false; {$endif}
    repeat
      didSubstitution:=false;
      initTokTypes;
      {$ifdef fullVersion}
      if reportableStep then begin
        if stepper.stepping(first^.location) then currentStateMessage(true)
                                             else context.adapters^.currentlyDebugging:=false;
      end;
      reportableStep:=false;
      {$endif}
      case cTokType[0] of
        tt_literal,tt_aggregatorExpressionLiteral: case cTokType[-1] of
          tt_listToParameterList: if P_literal(first^.data)^.literalType in C_validListTypes then begin
            stack_popDestroy;
            first^.tokType:=tt_parList;
            stack_popLink;
          end;
          tt_unaryOpPlus: stack_popDestroy;
          tt_unaryOpMinus: begin
            newLit:=P_literal(first^.data)^.negate(stack[length(stack)-1]^.location,context.adapters^);
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
                                      stack[length(stack)-1]^.location,
                                      context.adapters^);
              //LHS literal is now result of first comparison (still a literal)
              disposeLiteral(stack[length(stack)-2]^.data);
              stack[length(stack)-2]^.data:=newLit;
              //applied comparator is replaced by operator 'and'
              stack[length(stack)-1]^.tokType:=tt_operatorAnd;
              didSubstitution:=true;
              {$ifdef fullVersion}reportableStep:=true;{$endif}
            end else process_op_lit;
          end;
          tt_operatorAnd, tt_operatorOr, //operators with lazy evaluation ?
          tt_operatorXor, tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt,
          tt_operatorMod, tt_operatorPot, tt_operatorStrConcat, tt_operatorExtractL0, tt_operatorExtractL1,
          tt_operatorExtractL2, tt_operatorExtractL3, tt_operatorConcat, tt_operatorIn: process_op_lit;
          tt_braceOpen : case cTokType[1] of // ( | <Lit>
            tt_braceClose: begin  // ( | <Lit> )
              stack_popDestroy;
              first^.next:=context.disposeToken(first^.next);
              didSubstitution:=true;
            end;
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
            FORBIDDEN_SEPARATORS;
            else context.adapters^.raiseError('Unable to resolve paranthesis!',stack[length(stack)-1]^.location);
          end;
          tt_list_constructor: case cTokType[1] of
            tt_separatorComma, tt_separatorCnt: begin // [ | <Lit> ,
              repeat
                P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location,context.adapters^);
                if first^.next^.tokType=tt_separatorCnt then P_listLiteral(stack[length(stack)-1]^.data)^.setRangeAppend;
                first:=context.disposeToken(first);
                first:=context.disposeToken(first);

              until (first=nil) or (first^.tokType<>tt_literal) or
                    (first^.next=nil) or not(first^.next^.tokType in [tt_separatorComma,tt_separatorCnt]);
              didSubstitution:=true;
            end;
            tt_listBraceClose: begin // [ | <Lit> ]
              P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location,context.adapters^);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              stack_popLink;
              first^.tokType:=tt_literal;
              didSubstitution:=true;
              {$ifdef fullVersion}reportableStep:=true;{$endif}
            end;
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
          end;
          tt_parList_constructor: case cTokType[1] of
            tt_braceClose: begin // <F> <par(> | <Lit> ) -> <F> <par>
              P_listLiteral(stack[length(stack)-1]^.data)^.append(first^.data,true,context.adapters^);
              stack[length(stack)-1]^.tokType:=tt_parList; //mutate <tt_parList_constructor> -> <tt_parList>
              first:=context.disposeToken(first); //dispose literal
              first:=context.disposeToken(first); //dispose closing bracket
              stack_popLink; //pop parameter list
              stack_popLink; //pop FUNCTION
              {$ifdef fullVersion}reportableStep:=true;{$endif}
            end;
            tt_separatorComma: begin // <F> <par(> | <Lit> , -> <F> <par(> |
              P_listLiteral(stack[length(stack)-1]^.data)^.append(first^.data,true,context.adapters^);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              didSubstitution:=true;
            end;
            tt_separatorCnt:   context.adapters^.raiseError('Token .. is only allowed in list constructors.',first^.next^.location);
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
          end;
          tt_each,tt_parallelEach: if (stack[length(stack)-1]^.data=nil) then case cTokType[1] of
            tt_separatorComma: if cTokType[2]<>tt_EOL then begin
              // each(<id>, || <literal> , ...
              stack[length(stack)-1]^.data:=first^.data; //store literal list in each/pEach-token
              P_literal(first^.data)^.rereference; //rereference literal to prevent destruction
              first:=context.disposeToken(first); //dispose old literal
              first:=context.disposeToken(first); //dispose comma
              stack_popLink;
            end else context.adapters^.raiseError('Invalid syntax for each/pEach construct - EOL comes to early!',stack[length(stack)-1]^.location);
            tt_separatorCnt:   context.adapters^.raiseError('Token .. is only allowed in list constructors.',first^.next^.location);
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
            else context.adapters^.raiseError('Invalid syntax for each construct - token "'+safeTokenToString(first^.next)+'" cannot be interpreted after each! Expression tail:',stack[length(stack)-1]^.location);
          end else context.adapters^.raiseError('Invalid syntax for each construct - each already has a literal!',stack[length(stack)-1]^.location);
          tt_mutate: case cTokType[1] of
            tt_semicolon: if (cTokType[2]=tt_end) and (cTokType[-1]=tt_begin) then begin
              stack_popDestroy;
              first^.next:=context.disposeToken(first^.next);
              first^.next:=context.disposeToken(first^.next);
              context.scopePop;
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
            tt_semicolon: if (cTokType[2]=tt_end) and (cTokType[-1]=tt_begin) then begin
              first:=context.disposeToken(first);
              first^.next:=context.disposeToken(first^.next);
              first^.next:=context.disposeToken(first^.next);
              context.scopePop;
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
              COMMON_SEMICOLON_HANDLING;
              COMMON_CASES;
              FORBIDDEN_SEPARATORS;
            end;
          end;
        end;
        tt_begin: begin
          context.scopePush;
          stack_push;
          didSubstitution:=true;
        end;
        tt_assignNewBlockLocal, tt_assignExistingBlockLocal,tt_cso_assignPlus..tt_cso_assignAppend: stack_push;
        tt_blockLocalVariable: begin
          first^.data:=context.getLocalValueOrNull(first^.txt);
          if first^.data<>nil then begin
            first^.tokType:=tt_literal;
            didSubstitution:=true;
            {$ifdef fullVersion}reportableStep:=true;{$endif}
          end else context.adapters^.raiseError('Cannot find value for local id "'+first^.txt+'"',first^.location);
        end;
        tt_operatorPlus: begin first^.tokType:=tt_unaryOpPlus; stack_push; end;
        tt_operatorMinus: begin first^.tokType:=tt_unaryOpMinus; stack_push; end;
        tt_unaryOpPlus, tt_unaryOpMinus: stack_push;
        tt_comparatorEq..tt_operatorXor,tt_operatorMult..tt_operatorPot,tt_operatorStrConcat..tt_operatorIn:
          context.adapters^.raiseError('Undefined prefix operator '+first^.singleTokenToString,first^.location);
        tt_braceOpen: stack_push;
        tt_expBraceOpen: begin
          digestInlineExpression(first,context);
          didSubstitution:=true;
        end;
        tt_braceClose: if cTokType[-1]=tt_parList_constructor then begin
          first:=context.disposeToken(first);
          stack_popLink;
          first^.tokType:=tt_parList;
          stack_popLink;
          didSubstitution:=true;
          {$ifdef fullVersion}reportableStep:=true;{$endif}
        end;
        tt_listBraceOpen: if cTokType[1]=tt_listBraceClose then begin
          //empty list
          first^.data:=newListLiteral;
          first^.tokType:=tt_literal;
          first^.next:=context.disposeToken(first^.next);
          didSubstitution:=true;
        end else begin
          first^.data:=newListLiteral;
          first^.tokType:=tt_list_constructor;
          stack_push;
        end;
        tt_list_constructor: stack_push;
        tt_identifier: begin
          if not(context.scopeBottom) and (context.hasId(first^.txt)) then begin
            first^.tokType:=tt_blockLocalVariable;
            first^.data:=nil;
            didSubstitution:=true;
          end else begin
            if first^.data=nil then first^.data:=guessPackageForToken(first^);
            P_package(first^.data)^.resolveRuleId(first^,context.adapters);
            if first^.tokType<>tt_identifier then didSubstitution:=true;
          end;
        end;
        tt_identifier_pon: begin
          if first^.data=nil then first^.data:=guessPackageForToken(first^);
          P_package(first^.data)^.resolveRuleId(first^,context.adapters);
          if first^.tokType<>tt_identifier_pon then didSubstitution:=true;
        end;
        tt_each,tt_parallelEach: if (first^.data=nil) then stack_push else resolveEach((first^.tokType=tt_parallelEach) and (callDepth<STACK_DEPTH_LIMIT) and context.allowDelegation);
        tt_mutate: stack_push;
        tt_aggregatorConstructor: case cTokType[1] of
          tt_braceOpen, tt_parList_constructor, tt_listToParameterList: begin
            if cTokType[2]=tt_identifier then begin
              if first^.next^.next^.data=nil then first^.next^.next^.data:=guessPackageForToken(first^.next^.next^);
              P_package(first^.next^.next^.data)^.resolveRuleId(first^.next^.next^,context.adapters);
              cTokType[2]:=first^.next^.next^.tokType;
            end;
            if ((cTokType[2] in C_operatorsForAggregators) or (cTokType[2]=tt_intrinsicRule)) and
               (first^.next^.next^.next<>nil) and
               (first^.next^.next^.next^.tokType=tt_braceClose) then begin
              // || aggregator ( + )
              first^.tokType:=tt_aggregatorExpressionLiteral;
              first^.data:=createPrimitiveAggregatorLiteral(first^.next^.next,context);
              first^.next:=context.disposeToken(first^.next); //drop (
              first^.next:=context.disposeToken(first^.next); //drop +
              first^.next:=context.disposeToken(first^.next); //drop )
              didSubstitution:=true;
              {$ifdef fullVersion}reportableStep:=true;{$endif}
            end else startOrPushParameterList;
          end;
          tt_parList: applyRule(first^.next,first^.next^.next);
        end;

        tt_localUserRule, tt_importedUserRule, tt_intrinsicRule, tt_rulePutCacheValue : case cTokType[1] of
          tt_braceOpen, tt_parList_constructor, tt_listToParameterList: startOrPushParameterList;
          tt_parList: applyRule(first^.next,first^.next^.next);
          tt_braceClose,tt_listBraceClose,tt_comparatorEq..tt_operatorIn,tt_EOL,tt_iifCheck,tt_iifElse,tt_separatorCnt,tt_separatorComma,tt_semicolon,
          tt_typeCheckScalar..tt_typeCheckKeyValueList, tt_localUserRule_pon,tt_importedUserRule_pon,tt_intrinsicRule_pon,tt_identifier_pon : applyRule(nil,first^.next);
        end;
        tt_while: if (cTokType[1]=tt_braceOpen) then begin
          first^.next:=context.disposeToken(first^.next);
          resolveWhile;
        end;
        tt_when: if (cTokType[1]=tt_braceOpen) then begin
          first^.next:=context.disposeToken(first^.next);
          resolveWhen;
        end;

        tt_iifCheck: if (cTokType[-1]=tt_literal) then begin
          if (P_literal(stack[length(stack)-1]^.data)^.literalType=lt_boolean)
          then resolveInlineIf(P_boolLiteral(stack[length(stack)-1]^.data)^.value)
          else context.adapters^.raiseError('Invalid syntax for inline-if; first operand is expected to be a boolean. Instead I found a '+C_typeString[P_literal(stack[length(stack)-1]^.data)^.literalType]+': '+stack[length(stack)-1]^.singleTokenToString,first^.location);
        end else context.adapters^.raiseError('Invalid syntax for inline-if; first operand is expected to be a boolean. Here, the first operand is not even a literal.',first^.location);
      end;
    until not(didSubstitution) or not(context.adapters^.noErrors);
    {$ifndef debugMode}
    except
      on EOutOfMemory do begin
        context.adapters^.raiseCustomMessage(mt_el5_systemError,'An unhandled EOutOfMemory exception was caught in reduceExpression on callDepth='+intToStr(callDepth),C_nilTokenLocation);
      end else begin
        context.adapters^.raiseCustomMessage(mt_el5_systemError,'An unhandled, unknown exception was caught in reduceExpression on callDepth='+intToStr(callDepth),C_nilTokenLocation);
      end;
    end;
    {$endif}

    if not(context.adapters^.noErrors) and (first<>nil) then currentStateMessage({$ifdef fullVersion} false{$endif});
    if (context.adapters^.hasMessageOfType[mt_el5_haltMessageReceived] or context.adapters^.hasMessageOfType[mt_el5_systemError]) then begin
      while (length(stack)>0) do stack_popDestroy;
      if (callDepth=0) then begin
        context.adapters^.raiseNote('Cleaning up partial result after system error',C_nilTokenLocation);
        context.cascadeDisposeToken(first);
      end;
    end else while (length(stack)>0) do stack_popLink;
  end;

{$endif}
