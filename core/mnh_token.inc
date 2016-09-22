{$ifdef include_interface}

{ T_tokenArray }

T_tokenArray=object
  private
    blob:record
      closer:char;
      text:ansistring;
      start:T_tokenLocation;
    end;
    T:array of T_token;
    i:longint;
    FUNCTION getToken(CONST line:ansistring; VAR lineLocation:T_tokenLocation; CONST inPackage:P_package; VAR adapters:T_adapters; CONST retainBlanks:boolean=false):T_token;
  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE append(CONST newTok:T_token);
    PROCEDURE step(CONST package:P_package; VAR lastComment:ansistring; VAR adapters:T_adapters);
    FUNCTION current:T_token;
    PROCEDURE mutateCurrentTokType(CONST newTokType:T_tokenType);
    FUNCTION atEnd:boolean;
    FUNCTION lastToken:T_token;
    FUNCTION getRawTokensUndefining:T_rawTokenArray;
    PROCEDURE tokenizeAll(CONST inputString:ansistring; CONST location:T_tokenLocation; CONST inPackage:P_package; VAR adapters:T_adapters; CONST retainBlanks:boolean);
    PROCEDURE tokenizeAll(VAR provider:T_codeProvider; CONST inPackage:P_package; VAR adapters:T_adapters);
end;

P_tokenStack=^T_TokenStack;
T_TokenStack=object
  topIndex:longint;
  dat:array of P_token;
  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  PROCEDURE popDestroy(VAR context:T_evaluationContext);
  PROCEDURE popLink(VAR first:P_token);
  PROCEDURE push(VAR first:P_token);
  PROCEDURE quietPush(CONST first:P_token);
  PROCEDURE quietPop;
  FUNCTION toString(CONST first:P_token; CONST lengthLimit:longint=maxLongint):ansistring;
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

PROCEDURE T_tokenArray.append(CONST newTok: T_token);
  begin
    setLength(t,length(t)+1);
    t[length(t)-1]:=newTok;
  end;

PROCEDURE T_tokenArray.step(CONST package: P_package;
  VAR lastComment: ansistring; VAR adapters: T_adapters);
  VAR fullText:ansistring;
      line:longint;
  begin
    repeat
      inc(i);
      if (i<length(t)) then case t[i].tokenType of
        tt_EOL: if (t[i].comment<>'') then begin
          if lastComment=''
          then lastComment:=                            t[i].comment
          else lastComment:=lastComment+C_lineBreakChar+t[i].comment;
        end;
        tt_literal: if (t[i].literal^.literalType=lt_string) and (i<length(t)-1)
                   and (t[i+1].tokenType=tt_literal) and (t[i+1].literal^.literalType=lt_string)
                   and (t[i].location.line=t[i+1].location.line)
        then begin
          line:=t[i].location.line;
          fullText:=P_stringLiteral(t[i].literal)^.value;
          t[i].undefine;
          while (i<length(t)-1) and (t[i+1].tokenType=tt_literal) and (t[i+1].literal^.literalType=lt_string) and (t[i+1].location.line=line) do begin
            inc(i);
            fullText:=fullText+P_stringLiteral(t[i].literal)^.value;
            t[i].undefine;
          end;
          t[i].tokenType:=tt_literal;
          t[i].literal:=newStringLiteral(fullText);
        end;
        tt_identifier: if (i<length(t)-2) and (t[i+1].tokenType=tt_ponFlipper) and (t[i+2].tokenType=tt_identifier) and (package<>nil) and package^.isImportedOrBuiltinPackage(t[i].id) then begin
          //resolve ambiguous notation "x.y" to qualified identifier "x.y" if applicable
          inc(i,2);
          t[i].id:=t[i-2].id+C_ID_QUALIFY_CHARACTER+t[i].id;
          package^.resolveRuleId(t[i],nil);
        end else if package<>nil then package^.resolveRuleId(t[i],nil);
        tt_each,tt_forcedParallelEach,tt_parallelEach: if (i<length(t)-3) and
         (t[i+1].tokenType=tt_braceOpen) and
         (t[i+2].tokenType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_customTypeRule,tt_intrinsicRule]) and
         (t[i+3].tokenType=tt_separatorComma) then begin
           //resolve each ( i , ... to each(i, ... (each with identifier; expected in further processing!)
          t[i+3].tokenType:=t[i  ].tokenType;
          t[i+3].id       :=t[i+2].id;
          inc(i,3); //skip tt_braceOpen, identifier and comma
        end else adapters.raiseCustomMessage(mt_el4_parsingError,'Invalid (p)Each construct. First argument must be an identifier. At least two arguments must be given.',t[i].location);
        tt_agg: if (i<length(t)-1) and
          (t[i+1].tokenType=tt_braceOpen) then begin
          t[i+1].tokenType:=tt_each;
          inc(i);
        end;
        tt_operatorMinus,tt_operatorPlus: if (i<length(t)-1) and
         ((i=0) or (t[i-1].tokenType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach,tt_forcedParallelEach,tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus])) and
         (t[i+1].tokenType in [tt_literal,tt_identifier,tt_braceOpen,tt_listBraceOpen,tt_expBraceOpen,tt_localUserRule,tt_importedUserRule,tt_customTypeRule,tt_intrinsicRule,tt_parameterIdentifier]) then begin
          if t[i].tokenType=tt_operatorMinus
          then t[i].tokenType:=tt_unaryOpMinus //unary - is special token
          else inc(i);                 //unary + is skipped
        end;
      end; //case //end if
    until (i>=length(t)) or
          (t[i].tokenType<>tt_EOL);
  end;

FUNCTION T_tokenArray.current: T_token;
  begin
    result:=t[i];
  end;

PROCEDURE T_tokenArray.mutateCurrentTokType(CONST newTokType: T_tokenType);
  begin
    t[i].tokenType:=newTokType;
  end;

FUNCTION T_tokenArray.atEnd: boolean;
  begin
    result:=i>=length(t);
  end;

FUNCTION T_tokenArray.lastToken: T_token;
  begin
    if length(t)>0 then exit(t[length(t)-1]);
    result.create;
    result.undefine;
  end;

FUNCTION T_tokenArray.getRawTokensUndefining: T_rawTokenArray;
  VAR k:longint;
      lastComment:ansistring='';
      intrinsicFuncPtr:P_builtinFuncPayload;
      nullAdapter:T_adapters;
  begin
    nullAdapter.create;
    setLength(result,0);
    repeat
      step(nil,lastComment,nullAdapter);
      if not(atEnd) then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=current.getRawToken;
        with result[length(result)-1] do if (tokType=tt_identifier)
        and intrinsicRuleMap.containsKey(txt,intrinsicFuncPtr) then tokType:=tt_intrinsicRule;
      end;
    until atEnd;
    for k:=0 to length(t)-1 do t[k].undefine;
    nullAdapter.destroy;
  end;

FUNCTION T_tokenArray.getToken(CONST line: ansistring;
  VAR lineLocation: T_tokenLocation; CONST inPackage: P_package;
  VAR adapters: T_adapters; CONST retainBlanks: boolean): T_token;
  VAR parsedLength:longint=0;

  PROCEDURE fail(message:ansistring);
    begin
      adapters.raiseCustomMessage(mt_el4_parsingError,message,lineLocation);
    end;

  FUNCTION leadingId:T_idString;
    VAR i:longint;
        tt:T_tokenType;
        match:boolean;
    begin
      i:=lineLocation.column;
      while (i<length(line)) and (i-lineLocation.column<maxIdLength) and (line[i+1] in ['a'..'z','A'..'Z','0'..'9','_']) do inc(i);
      parsedLength:=i-lineLocation.column+1;
      for tt:=low(T_tokenType) to high(T_tokenType) do if length(C_tokenInfo[tt].defaultId)=parsedLength then begin
        match:=true;
        for i:=0 to parsedLength-1 do match:=match and (line[lineLocation.column+i]=C_tokenInfo[tt].defaultId[i+1]);
        if match then exit(C_tokenInfo[tt].defaultId);
      end;
      result:=copy(line,lineLocation.column,parsedLength);
    end;

  FUNCTION parseCharCode:ansistring;
    VAR i:longint;
        charCode:longint;
    begin
      i:=lineLocation.column+1;
      while (i<length(line)) and (line[i+1] in ['0'..'9']) do inc(i);
      result:=copy(line,lineLocation.column+1,i-lineLocation.column);
      charCode:=strToIntDef(result,256);
      if (charCode<0) or (charCode>255)
      then fail('Invalid char code #'+result)
      else begin
        result:=chr(charCode);
        parsedLength:=i-lineLocation.column+1;
      end;
    end;

  FUNCTION startsWith(CONST c:char):boolean;
    begin result:=line[lineLocation.column]=c; end;
  FUNCTION startsWith(CONST prefix:string):boolean;
    begin result:=copy(line,lineLocation.column,length(prefix))=prefix; end;

  VAR id:T_idString='';
      stringValue:ansistring='';
      tt:T_tokenType;
  begin
    result.location:=lineLocation;
    result.undefine;
    result.next:=nil;
    with blob do if closer<>#0 then begin
      //stringValue now is rest of line
      stringValue:=copy(line,lineLocation.column,length(line));
      if pos(closer,stringValue)<=0 then begin
        parsedLength:=length(stringValue);
        inc(lineLocation.column,parsedLength);
        if text='' then text:=stringValue
                   else text:=text+C_lineBreakChar+stringValue;
      end else begin
        parsedLength:=pos(closer,stringValue)+length(closer)-1;
        inc(lineLocation.column,parsedLength);
        if text='' then text:=copy(stringValue,1,pos(closer,stringValue)-1)
                   else text:=text+C_lineBreakChar+copy(stringValue,1,pos(closer,stringValue)-1);
        closer:=#0;
        exit(result);
      end;
    end else if text<>'' then begin
      result.location:=start;
      result.tokenType:=tt_literal;
      result.literal:=newStringLiteral(text);
      text:='';
      exit(result);
    end;
    if retainBlanks then begin
      stringValue:='';
      while (lineLocation.column<=length(line)) and
            (line[lineLocation.column] in [' ',C_lineBreakChar,C_tabChar,C_carriageReturnChar]) do begin
        stringValue:=stringValue+line[lineLocation.column];
        inc(lineLocation.column);
      end;
      if stringValue<>'' then begin
        result.tokenType:=tt_blank;
        result.comment:=stringValue;
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
        result.tokenType:=tt_literal;
        result.literal:=parseNumber(line,lineLocation.column, false,parsedLength);
        if parsedLength<=0 then fail('Cannot parse numeric literal '+line);
      end;
      '"','''': begin
        stringValue:=unescapeString(line,lineLocation.column,parsedLength);
        if parsedLength=0 then fail('Cannot parse string literal '+line)
        else begin
          result.tokenType:=tt_literal;
          result.literal:=newStringLiteral(stringValue);
        end;
        stringValue:='';
      end;
      '#': begin
        result.tokenType:=tt_literal;
        result.literal:=newStringLiteral(parseCharCode);
      end;
      '$': begin
        result.tokenType:=tt_parameterIdentifier;
        result.id:=leadingId;
      end;
      'a'..'z','A'..'Z': begin
        id:=leadingId;
        result.tokenType:=tt_identifier;
        for tt:=low(T_tokenType) to high(T_tokenType) do
        if id=C_tokenInfo[tt].defaultId then result.tokenType:=tt;
        if result.tokenType=tt_identifier then begin
          if      id=C_boolText[true]  then begin result.tokenType:=tt_literal; result.literal:=newBoolLiteral(true); end
          else if id=C_boolText[false] then begin result.tokenType:=tt_literal; result.literal:=newBoolLiteral(false); end
          else if id=C_nanText         then begin result.tokenType:=tt_literal; result.literal:=newRealLiteral(Nan); end
          else if id=C_infText         then begin result.tokenType:=tt_literal; result.literal:=newRealLiteral(infinity); end
          else if id=C_voidText        then begin result.tokenType:=tt_literal; result.literal:=newVoidLiteral; end
          else result.id:=id;
        end;
      end;
      '/': if startsWith('//') then begin //comments
        parsedLength:=2;
        while (parsedLength+lineLocation.column<=length(line)) and not(line[parsedLength+lineLocation.column] in [C_lineBreakChar,C_carriageReturnChar]) do inc(parsedLength);
        if retainBlanks then begin
          result.tokenType:=tt_blank;
          result.comment:=copy(line,lineLocation.column,length(line));
        end else begin
          result.tokenType:=tt_EOL;
          if      startsWith(DOC_COMMENT_PREFIX)         then result.comment:=trim(copy(line,length(DOC_COMMENT_PREFIX)+lineLocation.column,length(line)-length(DOC_COMMENT_PREFIX)+1-lineLocation.column))
          else if startsWith(SPECIAL_COMMENT_BLOB_BEGIN) then begin
            result.comment:=SPECIAL_COMMENT_BLOB_BEGIN;
            blob.start:=lineLocation;
            if length(line)>=lineLocation.column+length(SPECIAL_COMMENT_BLOB_BEGIN)
            then begin
              blob.closer:=line[lineLocation.column+length(SPECIAL_COMMENT_BLOB_BEGIN)];
              parsedLength:=length(SPECIAL_COMMENT_BLOB_BEGIN)+1;
            end else blob.closer:='''';
          end;
        end;
      end else if startsWith(C_tokenInfo[tt_cso_assignDiv].defaultId) then begin
        parsedLength:=2;
        result.tokenType:=tt_cso_assignDiv;
      end else begin
        parsedLength:=1;
        result.tokenType:=tt_operatorDivReal;
      end;
      '.': if startsWith(C_tokenInfo[tt_each].defaultId) then begin
        parsedLength:=length(C_tokenInfo[tt_each].defaultId);
        result.tokenType:=tt_each;
      end else if startsWith(C_tokenInfo[tt_parallelEach].defaultId) then begin
        parsedLength:=length(C_tokenInfo[tt_parallelEach].defaultId);
        result.tokenType:=tt_parallelEach;
      end else if startsWith(C_tokenInfo[tt_forcedParallelEach].defaultId) then begin
        parsedLength:=length(C_tokenInfo[tt_forcedParallelEach].defaultId);
        result.tokenType:=tt_forcedParallelEach;
      end else if startsWith(C_tokenInfo[tt_agg].defaultId) then begin
        parsedLength:=length(C_tokenInfo[tt_agg].defaultId);
        result.tokenType:=tt_agg;
      end else if startsWith(C_tokenInfo[tt_optionalParameters].defaultId) then begin
        parsedLength:=length(C_tokenInfo[tt_optionalParameters].defaultId);
        result.tokenType:=tt_optionalParameters;
      end else if startsWith(C_tokenInfo[tt_separatorCnt].defaultId) then begin
        parsedLength:=length(C_tokenInfo[tt_separatorCnt].defaultId);
        result.tokenType:=tt_separatorCnt;
      end else begin
        result.tokenType:=tt_ponFlipper;
        parsedLength:=1;
      end;
      ':': if startsWith(C_tokenInfo[tt_assign].defaultId) then begin
        parsedLength:=2;
        result.tokenType:=tt_assign;
      end else if startsWith(C_tokenInfo[tt_pseudoFuncPointer].defaultId) then begin
        parsedLength:=2;
        result.tokenType:=tt_pseudoFuncPointer;
      end else if (length(line)>=lineLocation.column+3) and (line[lineLocation.column+1] in ['b','e','i','l','n','s','r','k']) then begin
        id:=leadingId;
        if      id=C_tokenInfo[tt_typeCheckBoolList  ].defaultId then result.tokenType:=tt_typeCheckBoolList
        else if id=C_tokenInfo[tt_typeCheckBoolean   ].defaultId then result.tokenType:=tt_typeCheckBoolean
        else if id=C_tokenInfo[tt_typeCheckExpression].defaultId then result.tokenType:=tt_typeCheckExpression
        else if id=C_tokenInfo[tt_typeCheckIntList   ].defaultId then result.tokenType:=tt_typeCheckIntList
        else if id=C_tokenInfo[tt_typeCheckInt       ].defaultId then result.tokenType:=tt_typeCheckInt
        else if id=C_tokenInfo[tt_typeCheckList      ].defaultId then result.tokenType:=tt_typeCheckList
        else if id=C_tokenInfo[tt_typeCheckNumList   ].defaultId then result.tokenType:=tt_typeCheckNumList
        else if id=C_tokenInfo[tt_typeCheckNumeric   ].defaultId then result.tokenType:=tt_typeCheckNumeric
        else if id=C_tokenInfo[tt_typeCheckStringList].defaultId then result.tokenType:=tt_typeCheckStringList
        else if id=C_tokenInfo[tt_typeCheckScalar    ].defaultId then result.tokenType:=tt_typeCheckScalar
        else if id=C_tokenInfo[tt_typeCheckString    ].defaultId then result.tokenType:=tt_typeCheckString
        else if id=C_tokenInfo[tt_typeCheckRealList  ].defaultId then result.tokenType:=tt_typeCheckRealList
        else if id=C_tokenInfo[tt_typeCheckReal      ].defaultId then result.tokenType:=tt_typeCheckReal
        else if id=C_tokenInfo[tt_typeCheckKeyValueList].defaultId then result.tokenType:=tt_typeCheckKeyValueList
        else begin
          parsedLength:=1;
          result.tokenType:=tt_iifElse;
        end;
      end else begin
        parsedLength:=1;
        result.tokenType:=tt_iifElse;
      end;
      ';': begin parsedLength:=1; result.tokenType:=tt_semicolon;             end;
      '}': begin parsedLength:=1; result.tokenType:=tt_expBraceClose;         end;
      '{': begin parsedLength:=1; result.tokenType:=tt_expBraceOpen;          end;
      '^': begin parsedLength:=1; result.tokenType:=tt_operatorPot;           end;
      ']': begin parsedLength:=1; result.tokenType:=tt_listBraceClose;        end;
      '[': begin parsedLength:=1; result.tokenType:=tt_listBraceOpen;         end;
      '?': begin parsedLength:=1; result.tokenType:=tt_iifCheck;              end;
      ',': begin parsedLength:=1; result.tokenType:=tt_separatorComma;        end;
      '@': begin parsedLength:=1; result.tokenType:=tt_listToParameterList;   end;
      ')': begin parsedLength:=1; result.tokenType:=tt_braceClose;            end;
      '(': begin parsedLength:=1; result.tokenType:=tt_braceOpen;             end;
      '|': if startsWith(C_tokenInfo[tt_cso_assignAppend].defaultId)
           then begin parsedLength:=2; result.tokenType:=tt_cso_assignAppend;      end
           else begin parsedLength:=1; result.tokenType:=tt_operatorConcat;        end;
      '+': if startsWith(C_tokenInfo[tt_cso_assignPlus].defaultId)
           then begin parsedLength:=2; result.tokenType:=tt_cso_assignPlus;        end
           else begin parsedLength:=1; result.tokenType:=tt_operatorPlus;          end;
      '&': if startsWith(C_tokenInfo[tt_cso_assignStrConcat].defaultId)
           then begin parsedLength:=2; result.tokenType:=tt_cso_assignStrConcat;   end
           else begin parsedLength:=1; result.tokenType:=tt_operatorStrConcat;     end;
      '-': if startsWith(C_tokenInfo[tt_declare].defaultId)
           then begin parsedLength:=2; result.tokenType:=tt_declare;               end
           else if startsWith(C_tokenInfo[tt_cso_assignMinus].defaultId)
           then begin parsedLength:=2; result.tokenType:=tt_cso_assignMinus;       end
           else begin parsedLength:=1; result.tokenType:=tt_operatorMinus;         end;
      '*': if startsWith('**')
           then begin parsedLength:=2; result.tokenType:=tt_operatorPot;           end
           else if startsWith(C_tokenInfo[tt_cso_assignMult].defaultId)
           then begin parsedLength:=2; result.tokenType:=tt_cso_assignMult;        end
           else begin parsedLength:=1; result.tokenType:=tt_operatorMult;          end;
      '>': if startsWith(C_tokenInfo[tt_comparatorGeq].defaultId)
           then begin parsedLength:=2; result.tokenType:=tt_comparatorGeq;         end
           else begin parsedLength:=1; result.tokenType:=tt_comparatorGrt;         end;
      '=': if startsWith(C_tokenInfo[tt_comparatorListEq].defaultId)
           then begin parsedLength:=2; result.tokenType:=tt_comparatorListEq;      end
           else begin parsedLength:=1; result.tokenType:=tt_comparatorEq;          end;
      '<': if startsWith('<>')
           then begin parsedLength:=2; result.tokenType:=tt_comparatorNeq;         end
           else if startsWith('<=')
           then begin parsedLength:=2; result.tokenType:=tt_comparatorLeq;         end
           else begin parsedLength:=1; result.tokenType:=tt_comparatorLss;         end;
      '!': if startsWith('!=')
           then begin parsedLength:=2; result.tokenType:=tt_comparatorNeq;         end
           else begin fail('Cannot parse: '+copy(line,lineLocation.column,20)+' (first char is "'+line[lineLocation.column]+'"=#'+intToStr(ord(line[lineLocation.column]))+')'); lineLocation.column:=length(line)+1; end;
      else begin
        fail('Cannot parse: '+copy(line,lineLocation.column,20)+' (first char is "'+line[lineLocation.column]+'"=#'+intToStr(ord(line[lineLocation.column]))+')');
        lineLocation.column:=length(line)+1;
      end;
    end;
    if parsedLength>0 then inc(lineLocation.column,parsedLength);
  end;

PROCEDURE T_tokenArray.tokenizeAll(CONST inputString: ansistring;
  CONST location: T_tokenLocation; CONST inPackage: P_package;
  VAR adapters: T_adapters; CONST retainBlanks: boolean);
  VAR next:T_token;
      workLocation:T_tokenLocation;
  begin
    workLocation:=location;
    workLocation.column:=1;
    while (workLocation.column<=length(inputString)) and (adapters.noErrors) do begin
      next:=getToken(inputString,workLocation,inPackage,adapters,retainBlanks);
      if (next.tokenType<>tt_EOL) or (next.comment<>'') then begin
        if (next.tokenType=tt_EOL) and (next.comment=SPECIAL_COMMENT_BLOB_BEGIN) then begin
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
    location.package:=nil;
    location.line:=0;
    location.column:=1;
    tokenArray.create;
    adapters.create;
    tokenArray.tokenizeAll(inputString,location,nil,adapters,true);
    adapters.destroy;
    result:=tokenArray.getRawTokensUndefining;
    tokenArray.destroy;
  end;

PROCEDURE T_tokenArray.tokenizeAll(VAR provider: T_codeProvider;
  CONST inPackage: P_package; VAR adapters: T_adapters);
  VAR location:T_tokenLocation;
      lineIndex:longint;
      next:T_token;
      lines:T_arrayOfString;

  begin
    lines:=provider.getLines;
    location.package:=inPackage;
    for lineIndex:=0 to length(lines)-1 do begin
      location.line:=lineIndex+1;
      location.column:=1;
      while (location.column<=length(lines[lineIndex])) and (adapters.noErrors) do begin
        next:=getToken(lines[lineIndex],location,inPackage,adapters);
        if (next.tokenType<>tt_EOL) or (next.comment<>'') then append(next) else next.destroy;
      end;
    end;
    //Handle unfinished blobs
    if blob.text<>'' then begin
      next.create;
      next.location:=blob.start;
      next.tokenType:=tt_literal;
      next.literal:=newStringLiteral(blob.text);
      append(next);
      blob.text:='';
    end;
  end;

FUNCTION getBodyParts(CONST first:P_token; CONST initialBracketLevel:longint; VAR context:T_evaluationContext; OUT closingBracket:P_token):T_bodyParts;
  VAR t,p:P_token;
      bracketLevel,i:longint;
      partLength:longint=-1;
  begin
    closingBracket:=nil;
    bracketLevel:=initialBracketLevel;
    t:=first; p:=nil;
    if (first^.next<>nil) and (first^.next^.tokenType<>tt_separatorComma) then begin
      setLength(result,1);
      result[0].first:=first^.next;
    end else begin
      context.adapters^.raiseError('Invalid special construct; Cannot find closing bracket.',first^.location);
      setLength(result,0);
      exit;
    end;
    while (t<>nil) and not((t^.tokenType=tt_braceClose) and (bracketLevel=1)) do begin
      if t^.tokenType in C_openingBrackets then inc(bracketLevel)
      else if t^.tokenType in C_closingBrackets then dec(bracketLevel)
      else if (t^.tokenType=tt_separatorComma) and (bracketLevel=1) then begin
        if partLength=0 then context.adapters^.raiseError('Empty body part.',result[length(result)-1].first^.location);
        result[length(result)-1].last:=p; //end of body part is token before comma
        setLength(result,length(result)+1);
        result[length(result)-1].first:=t^.next; //start of next body part is token after comma
        partLength:=-1; //excluding delimiting separators
      end;
      p:=t; t:=t^.next; inc(partLength);
    end;
    result[length(result)-1].last:=p; //end of body part is token before comma
    if (t=nil) or (t^.tokenType<>tt_braceClose) or (bracketLevel<>1) then begin
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

PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_package; VAR context:T_evaluationContext);
  VAR t:P_token;
      rule:P_rule;
  begin
    t:=first;
    while t<>nil do begin
      case t^.tokenType of
        tt_identifier,tt_localUserRule,tt_importedUserRule,tt_customTypeRule: if inPackage<>nil then begin
          if (t^.next<>nil) and (t^.next^.tokenType in [tt_assign,tt_cso_assignPlus..tt_cso_assignAppend]) then begin
            inPackage^.resolveRuleId(t^,nil);
            if t^.tokenType<>tt_identifier then begin
              if t^.tokenType=tt_localUserRule then begin
                rule:=P_rule(t^.rawData);
                if rule^.ruleType in C_mutableRuleTypes then begin
                  t^.tokenType:=t^.next^.tokenType;
                  if t^.tokenType=tt_assign then t^.tokenType:=tt_mutate;
                  t^.setIdWithPointer(rule^.id,rule);
                  t^.next:=context.disposeToken(t^.next);
                end else context.adapters^.raiseError('You can only mutate mutable rules! Rule '+rule^.idForErrorFeedback+' is not mutable',t^.next^.location);
              end else context.adapters^.raiseError('You can only mutate mutable rules! Rule '+t^.id+' is a '+C_ruleTypeString[t^.tokenType],t^.next^.location);
            end else context.adapters^.raiseError('Cannot resolve identifier "'+t^.id+'".',t^.location);
          end;
        end;
        tt_modifier_local: if (t^.next<>nil) and (t^.next^.tokenType=tt_blockLocalVariable) and (t^.next^.next<>nil) and (t^.next^.next^.tokenType=tt_assign) then begin
          t^.tokenType:=tt_assignNewBlockLocal;
          t^.setIdWithPointer(t^.next^.id,nil);
          t^.next:=context.disposeToken(t^.next);
          t^.next:=context.disposeToken(t^.next);
        end;
        tt_blockLocalVariable: if (t^.next<>nil) and (t^.next^.tokenType=tt_assign) then begin
          t^.tokenType:=tt_assignExistingBlockLocal;
          t^.next:=context.disposeToken(t^.next);
        end else if (t^.next<>nil) and (t^.next^.tokenType in [tt_cso_assignPlus..tt_cso_assignAppend]) then begin
          t^.tokenType:=t^.next^.tokenType;
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
    if (rep^.tokenType<>tt_expBraceOpen) then begin
      context.adapters^.raiseError('Error creating subrule from inline; expression does not start with "{"',rep^.location);
      exit;
    end;
    t:=rep^.next; prev:=rep;
    inlineRule:=t;
    while (t<>nil) and ((t^.tokenType<>tt_expBraceClose) or (bracketLevel>0)) do begin
      case t^.tokenType of
        tt_expBraceOpen: begin
          digestInlineExpression(t,context);
          if t^.tokenType=tt_expBraceOpen then inc(bracketLevel);
        end;
      end;
      prev:=t;
      t:=t^.next;
    end;
    if (t=nil) or (t^.tokenType<>tt_expBraceClose) then begin
      context.adapters^.raiseError('Error creating subrule from inline; expression does not end with an }',rep^.location);
      exit;
    end;

    rep^.next:=t^.next; //remove expression from parent expression
    prev^.next:=nil; //unlink closing curly bracket
    context.disposeToken(t); //dispose closing curly bracket
    if context.adapters^.noErrors then begin
      new(inlineSubRule,createFromInline(inlineRule,context));
      if context.adapters^.noErrors then begin
        rep^.tokenType:=tt_literal;
        rep^.literal:=newExpressionLiteral(inlineSubRule);
      end else dispose(inlineSubRule,destroy);
    end;
  end;

PROCEDURE reduceExpression(VAR first:P_token; CONST callDepth:word; VAR context:T_evaluationContext);
  VAR stack:T_TokenStack;
      newLit:P_literal;
      didSubstitution:boolean;
      cTokType:array[-1..2] of T_tokenType;

  PROCEDURE initTokTypes; inline;
    begin
      if stack.topIndex>=0 then cTokType[-1]:=stack.dat[stack.topIndex]^.tokenType
                       else cTokType[-1]:=tt_EOL;
      if first<>nil then begin
        cTokType[0]:=first^.tokenType;
        if first^.next<>nil then begin
          cTokType[1]:=first^.next^.tokenType;
          if first^.next^.next<>nil then cTokType[2]:=first^.next^.next^.tokenType
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

  PROCEDURE raiseLazyBooleanError(CONST location:T_tokenLocation);
    begin
      context.adapters^.raiseError('Lazy boolean operators can only be applied to scalar booleans.',location);
    end;

  PROCEDURE resolveEach(eachType:T_tokenType);
    VAR bracketClosingEach:P_token;
        bodyRule:array of P_subrule;
        aggregatorLocation:T_tokenLocation;
        aggregatorToken:T_tokenType;
        aggregatorExpression:P_expressionLiteral;
        aggregatorPresent:(no,yes_as_operator,yes_as_expression,yes_as_head)=no;
        maxFutureForwardInitialization:longint=16;
        eachToken:P_token;

    FUNCTION parseBodyOk:boolean;
      VAR i,lastPart:longint;
          t:P_token;
          bodyParts:T_bodyParts;

      FUNCTION isPureAggregator:boolean;
        begin
          result:=eachToken^.id='';
        end;

      begin
        //first^.next token is <each>-Token
        eachToken:=first^.next;
        if (eachToken^.next=nil) or (eachToken^.next^.tokenType=tt_braceClose) then begin
          //This can only happen with the agg-construct!
          context.adapters^.raiseError('Invalid agg-construct: aggregator is missing',eachToken^.location);
          exit(false);
        end;
        //find closing bracket and body parts
        bodyParts:=getBodyParts(eachToken,0,context,bracketClosingEach);
        if (bracketClosingEach=nil) or not(context.adapters^.noErrors) then exit(false);
        if (length(bodyParts)>1) and isPureAggregator then begin
          context.adapters^.raiseError('Invalid agg-construct: argument must be an aggregator or aggregator prototype.',eachToken^.location);
          exit(false);
        end;

        //process aggregator part (if any)----------------------------------------------
        aggregatorPresent:=no;
        lastPart:=length(bodyParts)-1;
        t:=bodyParts[lastPart].first;
        if t^.tokenType=tt_aggregatorConstructor then begin
          reduceExpression(bodyParts[lastPart].first,callDepth+1,context);
          bodyParts[lastPart].last:=bodyParts[lastPart].first^.last;
          t:=bodyParts[lastPart].first;
        end;
        if t^.next=nil then begin
          case t^.tokenType of
            tt_comparatorEq..tt_operatorIn: begin
              aggregatorToken:=t^.tokenType;
              aggregatorPresent:=yes_as_operator;
              aggregatorLocation:=t^.location;
            end;
            tt_aggregatorExpressionLiteral: begin
              aggregatorExpression:=P_expressionLiteral(t^.literal);
              aggregatorExpression^.rereference;
              aggregatorPresent:=yes_as_expression;
              aggregatorLocation:=t^.location
            end;
            tt_literal: if isPureAggregator and (t^.literal^.literalType=lt_expression) then begin
              aggregatorExpression:=P_expressionLiteral(t^.literal);
              aggregatorExpression^.rereference;
              aggregatorPresent:=yes_as_expression;
              aggregatorLocation:=t^.location
            end else if isPureAggregator then context.adapters^.raiseError('Invalid agg-construct: argument must be an aggregator or aggregator prototype.',eachToken^.location);
            tt_intrinsicRule: if (T_intFuncCallback(t^.getIdWithPointer.ptr)=BUILTIN_MIN^.func) or
                                 (T_intFuncCallback(t^.getIdWithPointer.ptr)=BUILTIN_MAX^.func) then begin
              aggregatorExpression:=createPrimitiveAggregatorLiteral(t,context);
              aggregatorPresent:=yes_as_expression;
              aggregatorLocation:=t^.location
            end else if (T_intFuncCallback(t^.getIdWithPointer.ptr)=BUILTIN_HEAD^.func) then begin
              aggregatorPresent:=yes_as_head;
              aggregatorLocation:=t^.location
            end;
          end;
        end else if isPureAggregator then begin
          if t^.tokenType=tt_expBraceOpen then begin
            digestInlineExpression(t,context);
            if context.adapters^.noErrors then begin
              aggregatorExpression:=P_expressionLiteral(t^.literal);
              aggregatorExpression^.rereference;
              aggregatorPresent:=yes_as_expression;
              aggregatorLocation:=t^.location
            end;
          end else context.adapters^.raiseError('Invalid agg-construct: argument must be an aggregator or aggregator prototype.',eachToken^.location);
        end;

        if aggregatorPresent<>no then begin
          context.disposeToken(bodyParts[lastPart].first);
          setLength(bodyParts,length(bodyParts)-1);
        end else if isPureAggregator then begin
          result:=false;
          context.adapters^.raiseError('Invalid agg-construct: aggregator is missing.',eachToken^.location);
        end;
        //----------------------------------------------process aggregator part (if any)
        //process other body parts (if any)---------------------------------------------
        setLength(bodyRule,length(bodyParts));
        for i:=0 to length(bodyParts)-1 do
          new(bodyRule[i],createForEachBody(eachToken^.id,bodyParts[i].first,context));
        //---------------------------------------------process other body parts (if any)
        result:=true;
      end;

    VAR itList:P_listLiteral;
        i:longint;
        eachLocation:T_tokenLocation;
        resultLiteral:P_literal;
        booleanResult:boolean;
        lazyAbort:boolean=false;

    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean); inline;
      VAR newLit:P_literal;
      begin
        if not(context.adapters^.noErrors) then begin
          if (L<>nil) and doDispose then disposeLiteral(L);
          exit;
        end;
        if (L=nil) then begin
          if resultLiteral=nil then resultLiteral:=newErrorLiteral;
          context.adapters^.raiseError('Error performing aggregation in each-construct - NIL literal encountered',eachLocation);
          exit;
        end else if L^.literalType=lt_void then exit
        else if L^.literalType in [lt_error,lt_listWithError] then begin
          if doDispose then disposeLiteral(L);
          context.adapters^.raiseError('Error performing aggregation in each-construct - ERROR literal encountered',eachLocation);
          exit;
        end;
        case aggregatorPresent of
          no: begin
            if resultLiteral=nil then resultLiteral:=newListLiteral;
            P_listLiteral(resultLiteral)^.append(L,true);
          end;
          yes_as_head: begin
            if (L^.literalType<>lt_void) and (resultLiteral=nil) then begin
              resultLiteral:=L;
              L^.rereference;
              lazyAbort:=true;
            end;
          end;
          yes_as_operator: case aggregatorToken of
            tt_operatorConcat: begin
              if resultLiteral=nil then resultLiteral:=newListLiteral;
              if (L^.literalType in [lt_error, lt_boolean, lt_int, lt_real, lt_string, lt_expression])
              then P_listLiteral(resultLiteral)^.append   (L, true)
              else P_listLiteral(resultLiteral)^.appendAll(P_listLiteral(L));
            end;
            tt_operatorLazyAnd, tt_operatorLazyOr: if resultLiteral=nil then begin
              if (L^.literalType=lt_boolean) then booleanResult:=P_boolLiteral(L)^.value
              else raiseLazyBooleanError(aggregatorLocation);
              if booleanResult=(aggregatorToken=tt_operatorLazyOr) then begin
                lazyAbort:=true;
                resultLiteral:=L;
                L^.rereference;
              end;
            end;
            tt_operatorOrElse: if resultLiteral=nil then begin
              lazyAbort:=true;
              resultLiteral:=L;
              L^.rereference;
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
                end else context.adapters^.raiseError('Error performing aggregation in each-construct with aggregator ('+C_tokenInfo[aggregatorToken].defaultId+')',eachLocation);
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
        first^.tokenType:=tt_literal;
        if (resultLiteral=nil) and (aggregatorPresent=yes_as_operator) and (aggregatorToken in [tt_operatorLazyAnd,tt_operatorLazyOr]) then begin
          resultLiteral:=newBoolLiteral(booleanResult);
        end;
        if resultLiteral=nil then begin
          if aggregatorPresent<>no
          then first^.literal:=newVoidLiteral
          else first^.literal:=newListLiteral;
        end else begin
          if resultLiteral^.literalType=lt_listWithError then begin
            disposeLiteral(resultLiteral);
            resultLiteral:=newErrorLiteral;
          end;
          first^.literal:=resultLiteral;
        end;
        first^.next:=context.disposeToken(bracketClosingEach);
      end;

    PROCEDURE initFutures;
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

      VAR i,j:longint;
      begin
        if (aggregatorPresent=yes_as_operator) and (aggregatorToken in [tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorOrElse])
        then maxFutureForwardInitialization:=workerThreadCount
        else maxFutureForwardInitialization:=1024;
        if eachType=tt_forcedParallelEach then maxFutureForwardInitialization:=maxLongint;
        initializedFutures:=0;
        setLength(future,length(bodyRule)*itList^.size);
        for i:=0 to itList^.size-1 do for j:=0 to length(bodyRule)-1 do begin
          if (context.adapters^.noErrors) and not(lazyAbort) then begin
            future[initializedFutures]:=pendingTasks.enqueue(bodyRule[j],i,itList^.value(i),@context,eachType=tt_forcedParallelEach);
            if future[initializedFutures]^.state=fts_errorOnInitialization then context.adapters^.raiseError('Error initializing future.',eachLocation);
          end else future[initializedFutures]:=nil;
          inc(initializedFutures);
          if ((initializedFutures and 255)=0) or (pendingTasks.queuedCount>maxFutureForwardInitialization) then aggregateFutures(false);
        end;
        aggregateFutures(true);
      end;

    PROCEDURE initNonFutures;
      VAR task:T_futureTask;
          i,j:longint;
      begin
        for i:=0 to itList^.size-1 do for j:=0 to length(bodyRule)-1 do if (context.adapters^.noErrors) and not(lazyAbort) then begin
          task.createTask(bodyRule[j],i,itList^.value(i),@context);
          if task.state=fts_errorOnInitialization then context.adapters^.raiseError('Error initializing (pseudo-)future.',eachLocation)
          else begin
            task.evaluate(callDepth+1,context);
            addToAggregation(task.getResultAsLiteral(context),true);
          end;
          task.free(context);
          task.proFormaDestroy;
        end;
        finalizeAggregation;
      end;

    begin
      if (eachType=tt_parallelEach) and
         ((callDepth>=STACK_DEPTH_LIMIT) or
         not(context.allowDelegation) or
         (workerThreadCount<=0))
      then eachType:=tt_each;
      eachLocation:=first^.location;
      {$ifdef fullVersion}
      if currentlyDebugging and (eachType=tt_parallelEach) then begin
        eachType:=tt_each;
        context.adapters^.raiseNote('Not parallelizing pEach during debugging.',eachLocation);
      end;
      if currentlyDebugging and (eachType=tt_forcedParallelEach) then begin
        context.adapters^.raiseError('PEach is not allowed during debugging. Sorry.',eachLocation);
        exit
      end;
      {$endif}
      if (first^.literal=nil) or (first^.literal^.literalType in [lt_error,lt_listWithError,lt_void]) then begin
        context.adapters^.raiseError('Cannot apply each construct to void literal',eachLocation);
        exit
      end;
      if callDepth>STACK_DEPTH_LIMIT then begin
        context.adapters^.raiseCustomMessage(mt_el5_systemError,'Stack overflow in (p)each construct.',eachLocation);
        exit;
      end;
      if not(parseBodyOk) then exit;
      if (P_literal(first^.getIdWithPointer.ptr)^.literalType in C_validListTypes)
      then itList:=first^.getIdWithPointer.ptr
      else itList:=newOneElementListLiteral(first^.getIdWithPointer.ptr,false);

      first^.next:=context.disposeToken(first^.next);
      resultLiteral:=nil;
      //iterate over itList----------------------------------------------------------
      if (aggregatorPresent<>no) and (itList^.size=0) then begin
        resultLiteral:=newVoidLiteral;
        finalizeAggregation;
      end else if length(bodyRule)>0 then begin
        if eachType in [tt_parallelEach,tt_forcedParallelEach]
        then initFutures
        else initNonFutures;
      end else begin
        if eachType in [tt_parallelEach,tt_forcedParallelEach] then context.adapters^.raiseNote('There is no paralellization for pEach statements without body (i.e. pure aggregators)',eachLocation);
        for i:=0 to itList^.size-1 do addToAggregation(itList^.value(i),false);
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
        bodyParts:=getBodyParts(first,1,context,bracketClosingWhile);
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
        new(headRule,create(nil,emptyPattern,bodyParts[0].first,bodyParts[0].first^.location,true,true,context));
        new(bodyRule,create(nil,emptyPattern,bodyParts[1].first,bodyParts[1].first^.location,true,true,context));
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
      first^.tokenType:=tt_literal;
      first^.literal:=newVoidLiteral;
      first^.next:=context.disposeToken(bracketClosingWhile);
      //cleanup----------------------------------------------------------------------
      dispose(headRule,destroy);
      dispose(bodyRule,destroy);
      //----------------------------------------------------------------------cleanup
      didSubstitution:=true;
    end;

  PROCEDURE resolveTry;
    VAR bodyParts:T_bodyParts;
        closingBracket:P_token;
        errorCase:boolean;
        oldAdapters:P_adapters;
        prev_allowDelegation:boolean;

    begin
      bodyParts:=getBodyParts(first,1,context,closingBracket);
      if closingBracket=nil then exit;

      if length(bodyParts)<>2 then begin
        context.adapters^.raiseError('Invalid try-construct; Exactly two arguments (try-clause and except-clause) are expected.',first^.location);
        exit;
      end;

      oldAdapters:=context.adapters;
      prev_allowDelegation:=context.allowDelegation;
      context.allowDelegation:=false;
      context.adapters:=oldAdapters^.collectingClone;
      reduceExpression(bodyParts[0].first,callDepth+1,context);
      if context.adapters^.noErrors then begin
        context.cascadeDisposeToken(bodyParts[1].first);
        bodyParts[0].last:=bodyParts[0].first^.last;
        errorCase:=false;
      end else begin
        context.cascadeDisposeToken(bodyParts[0].first);
        errorCase:=true;
      end;
      oldAdapters^.copyDataFromCollectingCloneDisposing(context.adapters,errorCase);
      context.adapters:=oldAdapters;
      context.allowDelegation:=prev_allowDelegation;
      if errorCase then begin
        first^.tokenType:=tt_beginBlock; //mutate try-token to begin
        first^.next:=bodyParts[1].first;
        bodyParts[1].last^.next:=context.newToken(bodyParts[1].last^.location,tt_semicolon);
        bodyParts[1].last:=bodyParts[1].last^.next;
        bodyParts[1].last^.next:=closingBracket;
        closingBracket^.tokenType:=tt_endBlock; //mutate closing bracket to end
        stack.push(first);
        didSubstitution:=true;
        context.valueStore.scopePush(false);
      end else begin
        first^.tokenType:=tt_braceOpen; //mutate try-token to (
        first^.next:=bodyParts[0].first;
        bodyParts[0].last^.next:=closingBracket;
      end;
      setLength(bodyParts,0);
      didSubstitution:=true;
    end;

  PROCEDURE applyRule(CONST parameterListToken:P_token; CONST firstTokenAfterCall:P_token);
    VAR firstReplace,lastReplace:P_token;
        newLiteral:P_literal;
        parameterListLiteral:P_listLiteral;
        inlineRule:P_subrule;
    begin
      if parameterListToken=nil then parameterListLiteral:=nil
                                else parameterListLiteral:=P_listLiteral(parameterListToken^.literal);
      if (first^.tokenType in [tt_localUserRule,tt_importedUserRule,tt_customTypeRule]) then begin
        {$ifndef DEBUGMODE}
        try
        {$endif}
          if not(P_rule(first^.rawData)^.replaces(parameterListLiteral,firstReplace,lastReplace,callDepth,first^.tokenType in [tt_localUserRule,tt_customTypeRule],context)) then begin
            if parameterListLiteral=nil
            then context.adapters^.raiseError('Cannot apply user defined rule '+first^.id+' to empty parameter list',first^.location)
            else begin
              if (P_rule(first^.rawData)^.id=C_mainRuleId)
              then context.adapters^.raiseCustomMessage(mt_el3_noMatchingMain,'Cannot apply user defined rule '+first^.id+' to parameter list '+parameterListLiteral^.parameterListTypeString+':  '+parameterListLiteral^.toString(tt_parList),first^.location)
              else context.adapters^.raiseError('Cannot apply user defined rule '+first^.id+' to parameter list '+parameterListLiteral^.toString(tt_parList),first^.location);
            end;
            exit;
          end;
        {$ifndef DEBUGMODE}
        except
          on e:Exception do begin
            context.adapters^.raiseCustomMessage(mt_el5_systemError,'Severe error trying to apply user defined rule '+P_rule(first^.data)^.id,first^.location);
            context.adapters^.raiseCustomMessage(mt_el5_systemError,e.message,first^.location);
            exit;
          end;
        end;
        {$endif}
      end else if (first^.tokenType=tt_rulePutCacheValue) then begin
        newLiteral:=P_rule(first^.rawData)^.doPutCache(parameterListLiteral);
        firstReplace:=context.newLiteralToken(first^.location,newLiteral);
        lastReplace:=firstReplace;
      end else if (first^.tokenType=tt_aggregatorConstructor) then begin
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
          firstReplace:=context.newLiteralToken(first^.location,newLiteral);
          lastReplace:=firstReplace;
        end else context.adapters^.raiseError('Aggregators can only be constructed from expression literals!',first^.location);
      end else if (first^.tokenType=tt_intrinsicRule) then begin
        {$ifdef FULLVERSION}
        if currentlyDebugging then stepper.steppingIn(C_builtinPseudolocationPrefix+IntToHex(ptrint(first^.getIdWithPointer.ptr),2*sizeOf(pointer)),first^.id);
        {$endif}
        newLiteral:=T_intFuncCallback(first^.getIdWithPointer.ptr)(parameterListLiteral,first^.location,context);
        {$ifdef FULLVERSION}
        if currentlyDebugging then stepper.steppingOut(C_builtinPseudolocationPrefix+IntToHex(ptrint(first^.getIdWithPointer.ptr),2*sizeOf(pointer)));
        {$endif}
        if newLiteral<>nil then begin
          firstReplace:=context.newLiteralToken(first^.location,newLiteral);
          lastReplace:=firstReplace;
        end else if not(context.adapters^.noErrors) then exit else begin
          if parameterListLiteral=nil
          then context.adapters^.raiseError('Cannot apply intrinsic rule '+first^.id+' to empty parameter list',first^.location)
          else context.adapters^.raiseError('Cannot apply intrinsic rule '+first^.id+' to parameter list '+parameterListLiteral^.parameterListTypeString+':  '+parameterListLiteral^.toString(tt_parList),first^.location);
          exit;
        end;
      end else if (first^.tokenType in [tt_literal,tt_aggregatorExpressionLiteral]) and (first^.literal^.literalType=lt_expression) then begin
        inlineRule:=P_expressionLiteral(first^.literal)^.value;
        //failing "replaces" for inline rules will raise evaluation error.
        if not(inlineRule^.replaces(parameterListLiteral,P_expressionLiteral(first^.literal),firstReplace,lastReplace,context,false)) and
           not(inlineRule^.replaces(parameterListLiteral,P_expressionLiteral(first^.literal),firstReplace,lastReplace,context,true )) then exit;
      end else begin
        context.adapters^.raiseError('Trying to apply a rule which is no rule!',first^.location);
        exit;
      end;
      context.disposeToken(first);
      if parameterListToken<>nil then context.disposeToken(parameterListToken);
      first:=firstReplace;
      lastReplace^.next:=firstTokenAfterCall;
      didSubstitution:=true;
    end;

  PROCEDURE resolveInlineIf(CONST conditionLit:boolean); inline;
    VAR p,prev,tokenBeforeElse,lastThen:P_token;
        bracketLevel:longint=0;
    begin
      prev:=first;
      stack.push(first); //push "?"
      p:=first;
      while (p<>nil) and not((p^.tokenType=tt_iifElse) and (bracketLevel=0)) do begin
        if p^.tokenType in      C_openingBrackets then inc(bracketLevel)
        else if p^.tokenType in C_closingBrackets then dec(bracketLevel);
        prev:=p; p:=p^.next;
      end;
      if not((p<>nil) and (p^.tokenType=tt_iifElse) and (bracketLevel=0)) then begin
        stack.popLink(first);
        context.adapters^.raiseError('Cannot evaluate inline-if; cannot locate then-marker',first^.location);
        exit;
      end;
      tokenBeforeElse:=prev;
      while (p<>nil) and not((p^.tokenType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon]) and (bracketLevel=-1)) do begin
        if p^.tokenType in      C_openingBrackets then inc(bracketLevel)
        else if p^.tokenType in C_closingBrackets then dec(bracketLevel);
        prev:=p; p:=p^.next;
      end;
      if  not((p=nil) or (p^.tokenType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon]) and (bracketLevel=-1)) then begin
        stack.popLink(first);
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
      stack.popDestroy(context); //pop "?"
      stack.popDestroy(context); //pop condition literal
      didSubstitution:=true;
    end;

  PROCEDURE startOrPushParameterList; inline;
    begin
      stack.push(first);
      if first^.tokenType=tt_braceOpen then begin
        first^.tokenType:=tt_parList_constructor;
        first^.literal:=newListLiteral;
      end;
      stack.push(first);
      didSubstitution:=true;
    end;

  PROCEDURE applyMutation;
    VAR newValue:P_literal;
    begin
      newValue:=first^.next^.literal;
      P_rule(first^.rawData)^.setMutableValue(newValue,false);
      first:=context.disposeToken(first);
    end;

  PROCEDURE applyLocalAssignment(CONST kind:T_tokenType);
    VAR newValue:P_literal;
        rhsValue:P_literal;
    begin
      newValue:=first^.next^.getIdWithPointer.ptr;
      case kind of
        tt_assignNewBlockLocal: begin
          context.valueStore.createVariable(first^.getIdWithPointer.id,newValue,false);
          first:=context.disposeToken(first);
        end;
        tt_assignExistingBlockLocal: begin
          context.getVariable(first^.getIdWithPointer.id)^.setValue(newValue);
          first:=context.disposeToken(first);
        end;
        tt_cso_assignPlus..tt_cso_assignAppend: if first^.getIdWithPointer.ptr=nil then begin
          newValue:=context.getVariable(first^.getIdWithPointer.id)^.mutate(kind,newValue,first^.location,context.adapters^);
          if context.adapters^.noErrors then begin
            first:=context.disposeToken(first);
            first^.disposeLiteral;
            first^.literal:=newValue;
          end;
        end else begin
          newValue:=P_rule(first^.getIdWithPointer.ptr)^.mutateInline(kind,newValue,first^.location,context);
          if context.adapters^.noErrors then begin
            first:=context.disposeToken(first);
            first^.disposeLiteral;
            first^.literal:=newValue;
          end;
        end;
      end;
    end;

  PROCEDURE pon_flip;
    VAR newFunctionToken:P_token;
        newParameterListToken:P_token;
        oldSecond:P_token;
        expression:P_literal;
        ruleIdResolved:boolean=false;
        cTokType3:T_tokenType;
    begin
      if first^.next^.next=nil then exit;
      //Assuming:
      //cTokType[0]=tt_literal
      //cTokType[1]=tt_ponFlipper
      //cTokType[2] is a resolved identifier or a blocklocal variable;
      //Transforms:
      //  <Lit> . func     -> func(<Lit>)
      //  <Lit> . func(... -> func(<Lit>,...
      newFunctionToken:=context.newToken(first^.next^.next);

      if cTokType[2]=tt_EOL then cTokType3:=tt_EOL else begin
        oldSecond:=first^.next^.next^.next;
        if oldSecond=nil then cTokType3:=tt_EOL
        else cTokType3:=oldSecond^.tokenType;
      end;

      with newFunctionToken^ do begin
        ruleIdResolved:=not(tokenType in [tt_identifier,tt_blockLocalVariable]);
        if tokenType=tt_blockLocalVariable then begin
          expression:=context.getVariableValue(newFunctionToken^.id);
          if (expression<>nil) then begin
            if expression^.literalType<>lt_expression then expression^.unreference
            else begin
              newFunctionToken^.tokenType:=tt_literal;
              newFunctionToken^.literal:=expression;
              ruleIdResolved:=true;
            end;
          end;
        end;
      end;
      if ruleIdResolved then begin
        //resolved rule ID mutate x . y -> y(x)
        newParameterListToken:=context.newToken(first^.next^.next^.location,tt_parList);
        newParameterListToken^.literal:=newOneElementListLiteral(first^.literal,false);

        first^.literal:=nil; first^.tokenType:=tt_identifier;
                               //Disposing from:   <Lit> . func ...
        oldSecond:=context.disposeToken(         //|   | | ^^^^
                   context.disposeToken(         //|   | ^
                   context.disposeToken(first)));//^^^^^

        first:=newFunctionToken; newFunctionToken^.next:=newParameterListToken; newParameterListToken^.next:=oldSecond;

        if cTokType3=tt_braceOpen then begin
          newParameterListToken^.next:=context.disposeToken(oldSecond);
          newParameterListToken^.tokenType:=tt_parList_constructor;
        end;
        didSubstitution:=true;
        exit;
      end else begin
        context.adapters^.raiseError('Unresolved identifier: '+newFunctionToken^.id,newFunctionToken^.location);
        context.disposeToken(newFunctionToken);
        exit;
      end;
    end;

  PROCEDURE process_op_lit; {$ifndef DEBUGMODE} inline;{$endif}
    begin
      if (cTokType[1] in [tt_comparatorEq..tt_operatorIn]) and (C_opPrecedence[cTokType[1]]>=C_opPrecedence[cTokType[-1]]) or
         (cTokType[1] in [tt_braceClose,tt_listBraceClose,tt_EOL,tt_separatorComma,tt_semicolon, tt_separatorCnt, tt_iifCheck, tt_iifElse])
      then begin
        newLit:=resolveOperator(stack.dat[stack.topIndex-1]^.literal,
                                stack.dat[stack.topIndex]^.tokenType,
                                first^.literal,
                                stack.dat[stack.topIndex]^.location,
                                context.adapters^);
        first^.disposeLiteral;
        first^.literal:=newLit; //store new literal in head
        first^.location:=stack.dat[stack.topIndex]^.location;
        stack.popDestroy(context); //pop operator from stack
        stack.popDestroy(context); //pop LHS-Literal from stack
        didSubstitution:=true;
      end
      else if (cTokType[1]=tt_parList) then begin
        if cTokType[2]=tt_listToParameterList then begin
          stack.push(first);
          stack.push(first);
          stack.push(first);
          didSubstitution:=true;
        end else applyRule(first^.next,first^.next^.next)
      end
      else if cTokType[1] in [tt_parList_constructor,tt_braceOpen,tt_listToParameterList] then startOrPushParameterList
      else if cTokType[1]=tt_ponFlipper then pon_flip
      else if cTokType[1] in [tt_each,tt_forcedParallelEach,tt_parallelEach] then resolveEach(cTokType[1])
      else if cTokType[1]= tt_listBraceOpen then begin stack.push(first); didSubstitution:=true; end
      else begin
        stack.push(first);
        stack.push(first);
        didSubstitution:=true;
      end;
    end;

  PROCEDURE operator_and_literal_push;
    VAR p:P_token;
        bracketLevel:longint=0;
    begin
      case cTokType[1] of
        tt_operatorLazyAnd: if (cTokType[0]=tt_literal) and (first^.literal^.literalType=lt_boolean) then begin
          if (P_boolLiteral(first^.literal)^.value) then begin
            //true AND ... -> ...
            first:=context.disposeToken(first); //drop true
            first:=context.disposeToken(first); //drop AND
            didSubstitution:=true;
          end else begin
            //false AND ... -> false
            p:=first^.next;
            while not((p=nil) or (p^.tokenType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse,tt_operatorLazyOr,tt_operatorOr]) and (bracketLevel=0)) do begin
              if      p^.tokenType in C_openingBrackets then inc(bracketLevel)
              else if p^.tokenType in C_closingBrackets then dec(bracketLevel);
              p:=context.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end;
        end else raiseLazyBooleanError(first^.next^.location);
        tt_operatorLazyOr:if (cTokType[0]=tt_literal) and (P_literal(first^.literal)^.literalType=lt_boolean) then begin
          if (P_boolLiteral(first^.literal)^.value) then begin
            //true OR ... -> true
            p:=first^.next;
            while not((p=nil) or (p^.tokenType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse]) and (bracketLevel=0)) do begin
              if      p^.tokenType in C_openingBrackets then inc(bracketLevel)
              else if p^.tokenType in C_closingBrackets then dec(bracketLevel);
              p:=context.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end else begin
            //false OR ... -> ...
            first:=context.disposeToken(first); //drop false
            first:=context.disposeToken(first); //drop OR
            didSubstitution:=true;
          end;
        end else raiseLazyBooleanError(first^.next^.location);
        tt_operatorOrElse: if (cTokType[0]=tt_literal) then begin
          if (P_literal(first^.literal)^.literalType = lt_void) then begin
            //void orElse ... -> ...
            first:=context.disposeToken(first); //drop void
            first:=context.disposeToken(first); //drop orElse
            didSubstitution:=true;
          end else begin
            //<Lit> orElse ... -> <Lit>
            p:=first^.next;
            while not((p=nil) or (p^.tokenType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse,tt_operatorLazyOr,tt_operatorOr]) and (bracketLevel=0)) do begin
              if      p^.tokenType in C_openingBrackets then inc(bracketLevel)
              else if p^.tokenType in C_closingBrackets then dec(bracketLevel);
              p:=context.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end;
        end;
        else begin
          stack.push(first);
          stack.push(first);
          didSubstitution:=true;
        end;
      end;
    end;

  PROCEDURE resolvePseudoFuncPointer;
    VAR exRule:P_subrule;
        ruleToken:P_token;
        temp:P_token;
        location:T_tokenLocation;
    begin
      //state @pre: ::f ...
      location:=first^.location;;
      ruleToken:=context.disposeToken(first); //dispose ::, store f
      temp:=ruleToken^.next; //store ...
      ruleToken^.next      :=context.newToken(location,tt_listToParameterList); //f -> f@
      ruleToken^.next^.next:=context.newToken(location,tt_parameterIdentifier); //f@ -> f@$params
      ruleToken^.next^.next^.id:=ALL_PARAMETERS_TOKEN_TEXT;

      new(exRule,createFromInline(ruleToken,context));
      first:=context.newLiteralToken(location,newExpressionLiteral(exRule)); // {f@$params}
      first^.next:=temp; //-> {f@$params} ...
      didSubstitution:=true;
    end;

  FUNCTION tokenContextString:ansistring;
    begin
      result:=getEnumName(TypeInfo(cTokType[-1]),ord(cTokType[-1]))+' # '+
              getEnumName(TypeInfo(cTokType[ 0]),ord(cTokType[ 0]))+' '+
              getEnumName(TypeInfo(cTokType[ 1]),ord(cTokType[ 1]))+' '+
              getEnumName(TypeInfo(cTokType[ 2]),ord(cTokType[ 2]));
    end;

{$MACRO ON}
{$define COMMON_CASES:=
tt_listBraceOpen: begin
  first^.next^.tokenType:=tt_list_constructor;
  first^.next^.literal:=newListLiteral;
  stack.push(first);
  didSubstitution:=true;
end;
tt_ponFlipper: if (cTokType[2]=tt_expBraceOpen) then begin
  stack.push(first);
  stack.push(first);
  didSubstitution:=true;
end else pon_flip;
tt_braceOpen, tt_parList_constructor, tt_listToParameterList: startOrPushParameterList;
tt_parList:  if (cTokType[2]=tt_listToParameterList) then begin
               stack.push(first);
               stack.push(first);
               stack.push(first);
               didSubstitution:=true;
             end else applyRule(first^.next,first^.next^.next);
tt_comparatorEq..tt_operatorIn: operator_and_literal_push;
tt_iifCheck: begin stack.push(first); didSubstitution:=true; end;
tt_each,tt_parallelEach,tt_forcedParallelEach: resolveEach(cTokType[1])}

{$define FORBIDDEN_SEPARATORS:=
tt_separatorCnt:   context.adapters^.raiseError('Token .. is only allowed in list constructors.',first^.next^.location);
tt_separatorComma: context.adapters^.raiseError('Token , is only allowed in parameter lists and list constructors.',first^.next^.location)}

{$WARN 2005 OFF}
{$define COMMON_SEMICOLON_HANDLING:=
tt_semicolon: if (cTokType[-1] in [tt_beginBlock,tt_beginFunc]) then begin
  if (cTokType[2]=C_compatibleEnd[cTokType[-1]]) then begin
    {$ifdef fullVersion}  if currentlyDebugging and (cTokType[-1]=tt_beginFunc) then context.callStackPop; {$endif}
    stack.popDestroy(context);
    first^.next:=context.disposeToken(first^.next);
    first^.next:=context.disposeToken(first^.next);
    context.valueStore.scopePop;
    didSubstitution:=true;
  end else begin
    first:=context.disposeToken(first);
    first:=context.disposeToken(first);
    didSubstitution:=true;
  end;
end else context.adapters^.raiseError('Token ; is only allowed in begin-end-blocks',first^.next^.location)}

  begin
    stack.create;
    {$ifndef DEBUGMODE}try{$endif}
    repeat
      didSubstitution:=false;
      initTokTypes;
      //writeln(tokensToString(first,50));      writeln(tokenContextString);
      {$ifdef fullVersion}
      if currentlyDebugging then stepper.stepping(first^.location,first,@context,@stack);
      {$endif}
      case cTokType[0] of
        tt_literal,tt_aggregatorExpressionLiteral: case cTokType[-1] of
          tt_ponFlipper: if (first^.literal^.literalType=lt_expression)
            and (stack.topIndex>0) and (stack.dat[stack.topIndex-1]^.tokenType=tt_literal) then begin
            // <Lit> . # {$x}
            stack.popLink(first);
            stack.popLink(first);
            // -> # <Lit> {$x}
            initTokTypes;
            pon_flip;
          end;
          tt_listToParameterList: if first^.literal^.literalType in C_validListTypes then begin
            stack.popDestroy(context);
            first^.tokenType:=tt_parList;
            stack.popLink(first);
            while first^.tokenType=tt_parList do begin
              P_listLiteral(first^.literal)^.appendAll(P_listLiteral(first^.next^.literal));
              first^.next:=context.disposeToken(first^.next);
              stack.popLink(first);
            end;
            didSubstitution:=true;
          end;
          tt_unaryOpPlus: begin
            stack.popDestroy(context);
            didSubstitution:=true;
          end;
          tt_unaryOpMinus: begin
            newLit:=first^.literal^.negate(stack.dat[stack.topIndex]^.location,context.adapters^);
            first^.disposeLiteral;
            first^.literal:=newLit;
            stack.popDestroy(context);
            didSubstitution:=true;
          end;
          tt_comparatorEq..tt_comparatorListEq: begin //operators with special cascading
            if (cTokType[1] in [tt_comparatorEq..tt_comparatorListEq]) then begin
              // x < y < z -> [x < y] and y < z
              newLit:=resolveOperator(stack.dat[stack.topIndex-1]^.literal,
                                      stack.dat[stack.topIndex]^.tokenType,
                                      first^.literal,
                                      stack.dat[stack.topIndex]^.location,
                                      context.adapters^);
              //LHS literal is now result of first comparison (still a literal)
              stack.dat[stack.topIndex-1]^.disposeLiteral;
              stack.dat[stack.topIndex-1]^.literal:=newLit;
              stack.dat[stack.topIndex-1]^.location:=stack.dat[stack.topIndex]^.location;
              //applied comparator is replaced by operator 'and'
              stack.dat[stack.topIndex]^.tokenType:=tt_operatorAnd;
              didSubstitution:=true;
            end else process_op_lit;
          end;
          tt_operatorAnd, tt_operatorOr,
          tt_operatorXor, tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt,
          tt_operatorMod, tt_operatorPot, tt_operatorStrConcat, tt_operatorConcat, tt_operatorIn: process_op_lit;
          tt_braceOpen : case cTokType[1] of // ( | <Lit>
            tt_braceClose: begin  // ( | <Lit> )
              stack.popDestroy(context);
              first^.next:=context.disposeToken(first^.next);
              didSubstitution:=true;
            end;
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
            FORBIDDEN_SEPARATORS;
            else context.adapters^.raiseError('Unable to resolve paranthesis!',stack.dat[stack.topIndex]^.location);
          end;
          tt_list_constructor: case cTokType[1] of
            tt_separatorComma, tt_separatorCnt: begin // [ | <Lit> ,
              repeat
                P_listLiteral(stack.dat[stack.topIndex]^.literal)^.appendConstructing(first^.literal,first^.next^.location,context.adapters^);
                if first^.next^.tokenType=tt_separatorCnt then P_listLiteral(stack.dat[stack.topIndex]^.literal)^.setRangeAppend;
                first:=context.disposeToken(first);
                first:=context.disposeToken(first);
              until (first=nil) or (first^.tokenType<>tt_literal) or
                    (first^.next=nil) or not(first^.next^.tokenType in [tt_separatorComma,tt_separatorCnt]);
              didSubstitution:=true;
            end;
            tt_listBraceClose: begin // [ | <Lit> ] ...
              P_listLiteral(stack.dat[stack.topIndex]^.literal)^.appendConstructing(first^.literal,first^.next^.location,context.adapters^);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              stack.popLink(first);   // -> ? | [ ...
              first^.tokenType:=tt_literal; // -> ? | <NewList>
              didSubstitution:=true;
              if (stack.topIndex>=0) and (stack.dat[stack.topIndex]^.tokenType=tt_literal) then begin
                // <Lit> | <NewList> ...
                stack.popLink(first); // -> | <Lit> <NewList> ...
                newLit:=newListLiteral;
                P_listLiteral(newLit)^
                .append(first^.literal,false)^
                .appendAll(P_listLiteral(first^.next^.literal));
                first^.next^.disposeLiteral;
                first^.tokenType:=tt_intrinsicRule;
                first^.setIdWithPointer('get',@get_imp);
                first^.next^.tokenType:=tt_parList;
                first^.next^.literal:=newLit;        // -> | get (<Lit>|<NewList>) ...
              end;
            end;
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
          end;
          tt_parList_constructor: case cTokType[1] of
            tt_braceClose: begin // <F> <par(> | <Lit> ) -> <F> <par>
              P_listLiteral(stack.dat[stack.topIndex]^.literal)^.append(first^.literal,true);
              stack.dat[stack.topIndex]^.tokenType:=tt_parList; //mutate <tt_parList_constructor> -> <tt_parList>
              first:=context.disposeToken(first); //dispose literal
              first:=context.disposeToken(first); //dispose closing bracket
              stack.popLink(first); //pop parameter list
              stack.popLink(first); //pop FUNCTION
              didSubstitution:=true;
            end;
            tt_separatorComma: begin // <F> <par(> | <Lit> , -> <F> <par(> |
              P_listLiteral(stack.dat[stack.topIndex]^.literal)^.append(first^.literal,true);
              first:=context.disposeToken(first);
              first:=context.disposeToken(first);
              didSubstitution:=true;
            end;
            tt_separatorCnt:   context.adapters^.raiseError('Token .. is only allowed in list constructors.',first^.next^.location);
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
          end;
          tt_mutate: case cTokType[1] of
            tt_semicolon: if (cTokType[-1] in [tt_beginBlock,tt_beginFunc]) and (cTokType[2]=C_compatibleEnd[cTokType[-1]])  then begin
              {$ifdef fullVersion}  if currentlyDebugging and (cTokType[-1]=tt_beginFunc) then context.callStackPop(); {$endif}
              stack.popDestroy(context);
              first^.next:=context.disposeToken(first^.next);
              first^.next:=context.disposeToken(first^.next);
              context.valueStore.scopePop;
              didSubstitution:=true;
            end else begin
              stack.popLink(first);
              applyMutation;
              didSubstitution:=true;
            end;
            tt_braceClose,tt_separatorCnt,tt_separatorComma,tt_EOL,tt_expBraceClose,tt_listBraceClose: begin
              stack.popLink(first);
              applyMutation;
              didSubstitution:=true;
            end;
            COMMON_CASES;
          end;
          tt_assignNewBlockLocal, tt_assignExistingBlockLocal,tt_cso_assignPlus..tt_cso_assignAppend: case cTokType[1] of
            tt_semicolon: if (cTokType[-1] in [tt_beginBlock,tt_beginFunc]) and (cTokType[2]=C_compatibleEnd[cTokType[-1]]) then begin
              first:=context.disposeToken(first);
              {$ifdef fullVersion}  if currentlyDebugging and (cTokType[-1]=tt_beginFunc) then context.callStackPop(); {$endif}
              first^.next:=context.disposeToken(first^.next);
              first^.next:=context.disposeToken(first^.next);
              context.valueStore.scopePop;
              didSubstitution:=true;
            end else begin
              stack.popLink(first);
              applyLocalAssignment(cTokType[-1]);
              didSubstitution:=true;
            end;
            tt_braceClose,tt_separatorCnt,tt_separatorComma,tt_EOL,tt_expBraceClose,tt_listBraceClose: begin
              stack.popLink(first);
              applyLocalAssignment(cTokType[-1]);
              didSubstitution:=true;
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
        tt_beginBlock,tt_beginFunc: begin
          context.valueStore.scopePush(cTokType[0]=tt_beginFunc);
          stack.push(first);
          didSubstitution:=true;
        end;
        tt_assignNewBlockLocal, tt_assignExistingBlockLocal,tt_cso_assignPlus..tt_cso_assignAppend: begin
          stack.push(first);
          didSubstitution:=true;
        end;
        tt_blockLocalVariable: begin
          newLit:=context.getVariableValue(first^.id);
          if newLit<>nil then begin
            first^.tokenType:=tt_literal;
            first^.literal:=newLit;
            didSubstitution:=true;
          end else context.adapters^.raiseError('Cannot find value for local id "'+first^.id+'"',first^.location);
        end;
        tt_operatorPlus:                 begin first^.tokenType:=tt_unaryOpPlus;  stack.push(first); didSubstitution:=true; end;
        tt_operatorMinus:                begin first^.tokenType:=tt_unaryOpMinus; stack.push(first); didSubstitution:=true; end;
        tt_unaryOpPlus, tt_unaryOpMinus: begin                                  stack.push(first); didSubstitution:=true; end;
        tt_comparatorEq..tt_operatorXor,tt_operatorMult..tt_operatorPot,tt_operatorStrConcat..tt_operatorIn:
          context.adapters^.raiseError('Undefined prefix operator '+first^.singleTokenToString,first^.location);
        tt_braceOpen: begin stack.push(first); didSubstitution:=true; end;
        tt_expBraceOpen: begin
          digestInlineExpression(first,context);
          didSubstitution:=true;
        end;
        tt_braceClose: if cTokType[-1]=tt_parList_constructor then begin
          first:=context.disposeToken(first);
          stack.popLink(first);
          first^.tokenType:=tt_parList;
          stack.popLink(first);
          didSubstitution:=true;
        end;
        tt_listBraceOpen: if cTokType[1]=tt_listBraceClose then begin
          //empty list
          first^.tokenType:=tt_literal;
          first^.literal:=newListLiteral;
          first^.next:=context.disposeToken(first^.next);
          didSubstitution:=true;
        end else begin
          first^.tokenType:=tt_list_constructor;
          first^.literal:=newListLiteral;
          stack.push(first);
          didSubstitution:=true;
        end;
        tt_list_constructor: begin stack.push(first); didSubstitution:=true; end;
        tt_identifier: context.adapters^.raiseError('Unresolved identifier "'+first^.id+'"',first^.location);
        tt_mutate: begin stack.push(first); didSubstitution:=true; end;
        tt_aggregatorConstructor: case cTokType[1] of
          tt_braceOpen, tt_parList_constructor, tt_listToParameterList: begin
            if ((cTokType[2] in C_operatorsForAggregators) or (cTokType[2]=tt_intrinsicRule)) and
               (first^.next^.next^.next<>nil) and
               (first^.next^.next^.next^.tokenType=tt_braceClose) then begin
              // || aggregator ( + )
              first^.tokenType:=tt_aggregatorExpressionLiteral;
              first^.literal:=createPrimitiveAggregatorLiteral(first^.next^.next,context);
              first^.next:=context.disposeToken(first^.next); //drop (
              first^.next:=context.disposeToken(first^.next); //drop +
              first^.next:=context.disposeToken(first^.next); //drop )
              didSubstitution:=true;
            end else startOrPushParameterList;
          end;
          tt_parList: if (cTokType[2]=tt_listToParameterList) then begin
            stack.push(first);
            stack.push(first);
            stack.push(first);
            didSubstitution:=true;
          end else applyRule(first^.next,first^.next^.next);
        end;

        tt_localUserRule, tt_importedUserRule, tt_customTypeRule, tt_intrinsicRule, tt_rulePutCacheValue, tt_toId : case cTokType[1] of
          tt_braceOpen, tt_parList_constructor, tt_listToParameterList: startOrPushParameterList;
          tt_parList: if cTokType[0]=tt_toId then begin
            if (P_listLiteral(first^.next^.literal)^.size=1) and (P_listLiteral(first^.next^.literal)^.value(0)^.literalType=lt_string) then begin
              first^.tokenType:=tt_identifier;
              first^.id:=P_stringLiteral(P_listLiteral(first^.next^.literal)^.value(0))^.value;
              P_package(first^.location.package)^.resolveRuleId(first^,context.adapters);
              first^.next:=context.disposeToken(first^.next);
              didSubstitution:=true;
            end else context.adapters^.raiseError('Special function toId cannot be applied to parameter list '+safeTokenToString(first^.next),first^.location);
          end else if (cTokType[2]=tt_listToParameterList) then begin
            stack.push(first);
            stack.push(first);
            stack.push(first);
            didSubstitution:=true;
          end else applyRule(first^.next,first^.next^.next);
          tt_braceClose,tt_listBraceClose,tt_comparatorEq..tt_operatorIn,tt_EOL,tt_iifCheck,tt_iifElse,tt_separatorCnt,tt_separatorComma,tt_semicolon,
          tt_ponFlipper, tt_each,tt_parallelEach,tt_forcedParallelEach,tt_listBraceOpen : applyRule(nil,first^.next);
        end;
        tt_while: if (cTokType[1]=tt_braceOpen) then begin
          first^.next:=context.disposeToken(first^.next);
          resolveWhile;
        end;
        tt_try: if (cTokType[1]=tt_braceOpen) then begin
          first^.next:=context.disposeToken(first^.next);
          resolveTry;
        end;
        tt_iifCheck: if (cTokType[-1]=tt_literal) then begin
          if (stack.dat[stack.topIndex]^.literal^.literalType=lt_boolean)
          then resolveInlineIf(P_boolLiteral(stack.dat[stack.topIndex]^.literal)^.value)
          else context.adapters^.raiseError('Invalid syntax for inline-if; first operand is expected to be a boolean. Instead I found a '+C_typeString[P_literal(stack.dat[stack.topIndex]^.literal)^.literalType]+': '+stack.dat[stack.topIndex]^.singleTokenToString,first^.location);
        end else context.adapters^.raiseError('Invalid syntax for inline-if; first operand is expected to be a boolean. Here, the first operand is not even a literal.',first^.location);
        tt_pseudoFuncPointer: if cTokType[1] in [tt_localUserRule, tt_importedUserRule, tt_customTypeRule, tt_intrinsicRule] then resolvePseudoFuncPointer;
      end;
    until not(didSubstitution) or not(context.adapters^.noErrors);
    {$ifndef DEBUGMODE}
    except
      on e:Exception do begin
        context.adapters^.raiseSystemError('An unhandled, exception was caught in reduceExpression on callDepth='+intToStr(callDepth));
        context.adapters^.raiseSystemError(e.message);
      end;
    end;
    {$endif}
    if context.adapters^.noErrors then begin
      if (stack.topIndex>=0) or (first^.next<>nil) then begin
        if first=nil then while (stack.topIndex>=0) do stack.popLink(first);
        context.adapters^.raiseError('Cannot reduce expression.',first^.location);
        context.adapters^.raiseError('Remaining rest is:'+C_lineBreakChar+tokensToString(first,50),first^.location);
        context.adapters^.raiseError('Token Context is: '+tokenContextString,first^.location);
      end;
    end else if (context.adapters^.hasMessageOfType[mt_el5_haltMessageReceived] or
                 context.adapters^.hasMessageOfType[mt_el5_systemError] or
                 context.adapters^.hasMessageOfType[mt_el5_haltMessageQuiet]) then begin
      while (stack.topIndex>=0) do stack.popDestroy(context);
      if (callDepth=0) then context.cascadeDisposeToken(first);
    end else begin
      {$ifdef fullVersion}
      if gui_started then context.adapters^.raiseCustomMessage(mt_el3_stackTrace,stack.toString(first,50),first^.location);
      {$endif}
      while (stack.topIndex>=0) do stack.popLink(first);
    end;
    stack.destroy;
  end;

CONSTRUCTOR T_TokenStack.create;
  begin
    setLength(dat,0);
    topIndex:=-1;
  end;

DESTRUCTOR T_TokenStack.destroy;
  begin
    setLength(dat,0);
  end;

PROCEDURE T_TokenStack.popDestroy(VAR context: T_evaluationContext);
  begin
    context.disposeToken(dat[topIndex]);
    dec(topIndex);
    if topIndex<length(dat)-100 then setLength(dat,topIndex+1);
  end;

PROCEDURE T_TokenStack.popLink(VAR first: P_token);
  begin
    dat[topIndex]^.next:=first;
    first:=dat[topIndex];
    dec(topIndex);
    if topIndex<length(dat)-100 then setLength(dat,topIndex+1);
  end;

PROCEDURE T_TokenStack.push(VAR first: P_token);
  begin
    inc(topIndex);
    if topIndex>=length(dat) then setLength(dat,length(dat)+10);
    dat[topIndex]:=first;
    first:=first^.next;
  end;

PROCEDURE T_TokenStack.quietPush(CONST first:P_token);
  begin
    inc(topIndex);
    if topIndex>=length(dat) then setLength(dat,length(dat)+10);
    dat[topIndex]:=first;
  end;

PROCEDURE T_TokenStack.quietPop;
  begin
    dec(topIndex);
  end;

FUNCTION T_TokenStack.toString(CONST first: P_token; CONST lengthLimit: longint): ansistring;
  VAR i0,i:longint;
      prevWasIdLike:boolean=false;
  begin
    if topIndex>=0 then begin
      i0:=topIndex;
      result:='';
      while (i0>0) and (length(result)<lengthLimit) do begin
        result:=dat[i0]^.toString(prevWasIdLike,prevWasIdLike,lengthLimit-length(result))+result;
        dec(i0);
      end;
      if i0>0 then result:='... '
              else result:='';
      prevWasIdLike:=false;
      for i:=i0 to topIndex do result:=result+dat[i]^.toString(prevWasIdLike,prevWasIdLike);
    end else result:='';
    result:=result+' # '+tokensToString(first,lengthLimit);
  end;

{$endif}
