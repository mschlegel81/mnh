UNIT mnh_tokenArray;
INTERFACE
USES sysutils,math,
     myGenerics,myStringUtil,
     mnh_basicTypes,mnh_constants,
     mnh_fileWrappers,
     mnh_litVar,
     mnh_funcs,
     {$ifdef fullVersion}
     mnh_html,
     {$endif}
     mnh_tokens,mnh_out_adapters;
TYPE
  P_abstractPackage=^T_abstractPackage;
  T_abstractPackage=object(T_objectWithPath)
    protected
      codeProvider:P_codeProvider;
    public
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual; abstract;
  end;

  T_tokenArray=object
  private
    blob:record
      closer:char;
      text:string;
      start:T_tokenLocation;
    end;
    token:array of T_token;
    tokenFill:longint;
    stepIndex:longint;
    FUNCTION getToken(CONST line:ansistring; VAR lineLocation:T_tokenLocation; CONST inPackage:P_objectWithPath; VAR adapters:T_adapters; CONST retainBlanks:boolean=false):T_token;
    PROCEDURE append(CONST newTok:T_token);
  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE step(CONST package:P_abstractPackage; VAR comments,attributes:T_arrayOfString; VAR adapters:T_adapters);
    FUNCTION current:T_token;
    PROCEDURE mutateCurrentTokType(CONST newTokType:T_tokenType);
    FUNCTION atEnd:boolean;
    PROCEDURE tokenizeAll(CONST inPackage:P_abstractPackage; VAR adapters:T_adapters);
    PROCEDURE tokenizeAll(CONST inputString:ansistring; CONST location:T_tokenLocation; CONST inPackage:P_abstractPackage; VAR adapters:T_adapters; CONST retainBlanks:boolean);
    {$ifdef fullVersion}
    FUNCTION lastToken:T_token;
    FUNCTION getRawTokensUndefining:T_rawTokenArray;
    {$endif}
  end;

IMPLEMENTATION
CONSTRUCTOR T_tokenArray.create;
  begin
    setLength(token,100);
    tokenFill:=0;
    stepIndex:=-1;
  end;

DESTRUCTOR T_tokenArray.destroy;
  begin
    setLength(token,0);
    tokenFill:=0;
    stepIndex:=-1;
  end;

PROCEDURE T_tokenArray.append(CONST newTok: T_token);
  begin
    if tokenFill>=length(token) then setLength(token,round(length(token)*1.1)+1);
    token[tokenFill]:=newTok;
    inc(tokenFill);
  end;

PROCEDURE T_tokenArray.step(CONST package: P_abstractPackage; VAR comments,attributes:T_arrayOfString; VAR adapters: T_adapters);
  VAR fullText:ansistring;
      line:longint;
  begin
    repeat
      inc(stepIndex);
      if (stepIndex<tokenFill) then case token[stepIndex].tokType of
        tt_docComment: myGenerics.append(comments,token[stepIndex].txt);
        tt_attributeComment: if (token[stepIndex].txt<>'') then myGenerics.append(attributes,token[stepIndex].txt);
        tt_literal: if (P_literal(token[stepIndex].data)^.literalType=lt_string) and (stepIndex<tokenFill-1)
                   and (token[stepIndex+1].tokType=tt_literal) and (P_literal(token[stepIndex+1].data)^.literalType=lt_string)
                   and (token[stepIndex].location.line=token[stepIndex+1].location.line)
        then begin
          line:=token[stepIndex].location.line;
          fullText:=P_stringLiteral(token[stepIndex].data)^.value;
          disposeLiteral(token[stepIndex].data);
          token[stepIndex].tokType:=tt_EOL;
          while (stepIndex<tokenFill-1) and (token[stepIndex+1].tokType=tt_literal) and (P_literal(token[stepIndex+1].data)^.literalType=lt_string) and (token[stepIndex+1].location.line=line) do begin
            inc(stepIndex);
            fullText:=fullText+P_stringLiteral(token[stepIndex].data)^.value;
            disposeLiteral(token[stepIndex].data);
            token[stepIndex].tokType:=tt_EOL;
          end;
          token[stepIndex].data:=newStringLiteral(fullText);
          token[stepIndex].tokType:=tt_literal;
        end;
        tt_identifier: if (stepIndex<tokenFill-2) and (token[stepIndex+1].tokType=tt_ponFlipper) and (token[stepIndex+2].tokType=tt_identifier) and (package<>nil) and package^.isImportedOrBuiltinPackage(token[stepIndex].txt) then begin
          //resolve ambiguous notation "x.y" to qualified identifier "x.y" if applicable
          inc(stepIndex,2);
          token[stepIndex].txt:=token[stepIndex-2].txt+ID_QUALIFY_CHARACTER+token[stepIndex].txt;
          token[stepIndex].tokType:=tt_identifier;
          token[stepIndex].resolveRuleId(package,nil);
        end else if package<>nil then token[stepIndex].resolveRuleId(package,nil);
        tt_each,tt_parallelEach: if (stepIndex<tokenFill-3) and
         (token[stepIndex+1].tokType=tt_braceOpen) and
         (token[stepIndex+2].tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_customTypeRule,tt_intrinsicRule]) and
         (token[stepIndex+3].tokType=tt_separatorComma) then begin
           //resolve each ( i , ... to each(i, ... (each with identifier; expected in further processing!)
          token[stepIndex+3].tokType:=token[stepIndex].tokType;
          token[stepIndex+3].txt:=token[stepIndex+2].txt;
          token[stepIndex+3].data:=nil;
          inc(stepIndex,3); //skip tt_braceOpen, identifier and comma
        end else adapters.raiseError('Invalid (p)Each construct. First argument must be an identifier. At least two arguments must be given.',token[stepIndex].location);
        tt_agg: if (stepIndex<tokenFill-1) and
          (token[stepIndex+1].tokType=tt_braceOpen) then begin
          token[stepIndex+1].tokType:=tt_each;
          token[stepIndex+1].txt:='';
          token[stepIndex+1].data:=nil;
          inc(stepIndex);
        end;
        tt_operatorMinus,tt_operatorPlus: if (stepIndex<tokenFill-1) and
         ((stepIndex=0) or (token[stepIndex-1].tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach,tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus])) and
         (token[stepIndex+1].tokType in [tt_literal,tt_identifier,tt_braceOpen,tt_listBraceOpen,tt_expBraceOpen,tt_localUserRule,tt_importedUserRule,tt_customTypeRule,tt_intrinsicRule,tt_parameterIdentifier]) then begin
          if token[stepIndex].tokType=tt_operatorMinus
          then token[stepIndex].tokType:=tt_unaryOpMinus //unary - is special token
          else inc(stepIndex);                       //unary + is skipped
        end;
      end; //case //end if
    until (stepIndex>=tokenFill) or
          not(token[stepIndex].tokType in [tt_EOL,tt_docComment,tt_attributeComment]);
  end;

FUNCTION T_tokenArray.current: T_token;
  begin
    if stepIndex>=tokenFill then raise Exception.create('Invalid call to T_tokenArray.current; Index to query is '+intToStr(stepIndex)+'; array length is '+intToStr(tokenFill));
    result:=token[stepIndex];
  end;

PROCEDURE T_tokenArray.mutateCurrentTokType(CONST newTokType: T_tokenType);
  begin
    token[stepIndex].tokType:=newTokType;
  end;

FUNCTION T_tokenArray.atEnd: boolean;
  begin
    result:=stepIndex>=tokenFill;
  end;

{$ifdef fullVersion}
FUNCTION T_tokenArray.lastToken:T_token;
  begin
    if tokenFill>0 then exit(token[tokenFill-1]);
    result.create;
    result.tokType:=tt_EOL;
    result.txt:='';
    result.data:=nil;
  end;

FUNCTION T_tokenArray.getRawTokensUndefining: T_rawTokenArray;
  VAR k:longint;
      commentDummy,
      attributeDummy:T_arrayOfString;
      intrinsicFuncPtr:P_intFuncCallback;
      nullAdapter:T_adapters;
  begin
    commentDummy  :=C_EMPTY_STRING_ARRAY;
    attributeDummy:=C_EMPTY_STRING_ARRAY;
    nullAdapter.create;
    setLength(result,0);
    repeat
      step(nil,commentDummy,attributeDummy,nullAdapter);
      if not(atEnd) then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=current.getRawToken;
        with result[length(result)-1] do if (tokType=tt_identifier)
        and intrinsicRuleMap.containsKey(txt,intrinsicFuncPtr) then tokType:=tt_intrinsicRule;
      end;
    until atEnd;
    for k:=0 to tokenFill-1 do token[k].undefine;
    nullAdapter.destroy;
  end;
{$endif}

FUNCTION T_tokenArray.getToken(CONST line: ansistring; VAR lineLocation: T_tokenLocation; CONST inPackage: P_objectWithPath; VAR adapters: T_adapters; CONST retainBlanks: boolean): T_token;
  VAR parsedLength:longint=0;

  PROCEDURE fail(message:ansistring);
    begin
      adapters.raiseError(message,lineLocation);
    end;

  FUNCTION leadingId:ansistring;
    VAR i:longint;
        tt:T_tokenType;
        match:boolean;
    begin
      i:=lineLocation.column;
      while (i<length(line)) and (line[i+1] in ['a'..'z','A'..'Z','0'..'9','_']) do inc(i);
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

  FUNCTION startsWith(CONST c:char):boolean; inline;
    begin result:=line[lineLocation.column]=c; end;
  FUNCTION startsWith(CONST prefix:string):boolean;  inline;
    begin result:=copy(line,lineLocation.column,length(prefix))=prefix; end;
  FUNCTION startsWith(CONST t:T_tokenType):boolean; inline;
    begin result:=copy(line,lineLocation.column,length(C_tokenInfo[t].defaultId))=C_tokenInfo[t].defaultId; end;
  PROCEDURE apply(CONST t:T_tokenType); inline;
    begin
      result.tokType:=t;
      parsedLength:=length(C_tokenInfo[t].defaultId);
    end;
  PROCEDURE apply(CONST len:longint; CONST t:T_tokenType); inline;
    begin
      result.tokType:=t;
      parsedLength:=len;
    end;

  VAR id:ansistring='';
      stringValue:ansistring='';
      tt:T_tokenType;
  begin
    result.location:=lineLocation;
    result.txt:='';
    result.tokType:=tt_EOL;
    result.data:=nil;
    result.next:=nil;
    with blob do if closer<>#0 then begin
      //id now is rest of line
      id:=copy(line,lineLocation.column,length(line));
      if pos(closer,id)<=0 then begin
        parsedLength:=length(id);
        inc(lineLocation.column,parsedLength);
        if text='' then text:=id
                   else text:=text+C_lineBreakChar+id;
      end else begin
        parsedLength:=pos(closer,id)+length(closer)-1;
        inc(lineLocation.column,parsedLength);
        if text='' then text:=copy(id,1,pos(closer,id)-1)
                   else text:=text+C_lineBreakChar+copy(id,1,pos(closer,id)-1);
        result.txt:=closer;
        closer:=#0;
        exit(result);
      end;
    end else if text<>'' then begin
      result.location:=start;
      result.tokType:=tt_literal;
      result.data:=newStringLiteral(text);
      text:='';
      exit(result);
    end;
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
        stringValue:=unescapeString(line,lineLocation.column,parsedLength);
        if parsedLength=0 then fail('Cannot parse string literal '+line)
        else begin
          result.tokType:=tt_literal;
          result.data:=newStringLiteral(stringValue);
        end;
        stringValue:='';
      end;
      '#': begin
        result.tokType:=tt_literal;
        result.data:=newStringLiteral(parseCharCode);
      end;
      '$': begin
        result.txt:=leadingId;
        result.tokType:=tt_parameterIdentifier;
      end;
      'a'..'z','A'..'Z': begin
        result.txt:=leadingId;
        result.tokType:=tt_identifier;
        for tt:=low(T_tokenType) to high(T_tokenType) do
        if result.txt=C_tokenInfo[tt].defaultId then result.tokType:=tt;
        if result.tokType=tt_identifier then begin
          if      result.txt=LITERAL_BOOL_TEXT[true]  then begin result.tokType:=tt_literal; result.data:=newBoolLiteral(true); end
          else if result.txt=LITERAL_BOOL_TEXT[false] then begin result.tokType:=tt_literal; result.data:=newBoolLiteral(false); end
          else if result.txt=LITERAL_NAN_TEXT         then begin result.tokType:=tt_literal; result.data:=newRealLiteral(Nan); end
          else if result.txt=LITERAL_INF_TEXT         then begin result.tokType:=tt_literal; result.data:=newRealLiteral(infinity); end
          else if result.txt=LITERAL_TEXT_VOID        then begin result.tokType:=tt_literal; result.data:=newVoidLiteral; end
          else begin
            result.data:=inPackage;
          end;
        end;
      end;
      '/': if startsWith(COMMENT_PREFIX) then begin //comments
        parsedLength:=2;
        while (parsedLength+lineLocation.column<=length(line)) and not(line[parsedLength+lineLocation.column] in [C_lineBreakChar,C_carriageReturnChar]) do inc(parsedLength);
        if retainBlanks then begin
          result.tokType:=tt_blank;
          result.txt:=copy(line,lineLocation.column,length(line));
        end else begin
          result.tokType:=tt_EOL;
          if startsWith(DOC_COMMENT_PREFIX) then begin
            result.txt:=trimRight(copy(line,length(DOC_COMMENT_PREFIX)+lineLocation.column,length(line)-length(DOC_COMMENT_PREFIX)+1-lineLocation.column));
            result.tokType:=tt_docComment;
          end else if startsWith(ATTRIBUTE_COMMENT_PREFIX) then begin
            result.txt:=trim(copy(line,length(ATTRIBUTE_COMMENT_PREFIX)+lineLocation.column,length(line)-length(ATTRIBUTE_COMMENT_PREFIX)+1-lineLocation.column));
            result.tokType:=tt_attributeComment;
          end else if startsWith(SPECIAL_COMMENT_BLOB_BEGIN) then begin
            result.txt:=SPECIAL_COMMENT_BLOB_BEGIN;
            blob.start:=lineLocation;
            if length(line)>=lineLocation.column+length(SPECIAL_COMMENT_BLOB_BEGIN)
            then begin
              blob.closer:=line[lineLocation.column+length(SPECIAL_COMMENT_BLOB_BEGIN)];
              parsedLength:=length(SPECIAL_COMMENT_BLOB_BEGIN)+1;
            end else blob.closer:='''';
          end;
        end;
      end else if startsWith(tt_cso_assignDiv) then apply(tt_cso_assignDiv)
                                               else apply(tt_operatorDivReal);
      ':': if startsWith(tt_assign)            then apply(tt_assign)
      else if startsWith(tt_pseudoFuncPointer) then apply(tt_pseudoFuncPointer)
      else if (length(line)>=lineLocation.column+3) and (line[lineLocation.column+1] in ['b','c','e','i','l','n','s','r','k','f','m']) then begin
        id:=leadingId;
        result.tokType:=tt_iifElse;
        for tt in C_typeChecks do if id=C_tokenInfo[tt].defaultId then result.tokType:=tt;
        if result.tokType=tt_iifElse then parsedLength:=1;
      end else apply(tt_iifElse);
      '.': if      startsWith(tt_each)           then apply(tt_each) else
           if startsWith(tt_parallelEach)        then apply(tt_parallelEach) else
           if startsWith(tt_agg)                 then apply(tt_agg) else
           if startsWith(tt_optionalParameters)  then apply(tt_optionalParameters) else
           if startsWith(tt_separatorCnt)        then apply(tt_separatorCnt)
                                                 else apply(tt_ponFlipper);
      ';':                                            apply(tt_semicolon);
      '}':                                            apply(tt_expBraceClose);
      '{':                                            apply(tt_expBraceOpen);
      '^':                                            apply(tt_operatorPot);
      ']':                                            apply(tt_listBraceClose);
      '[':                                            apply(tt_listBraceOpen);
      '?':                                            apply(tt_iifCheck);
      ',':                                            apply(tt_separatorComma);
      '@':                                            apply(tt_listToParameterList);
      ')':                                            apply(tt_braceClose);
      '(':                                            apply(tt_braceOpen);
      '|': if startsWith(tt_cso_assignAppend)    then apply(tt_cso_assignAppend)
                                                 else apply(tt_operatorConcat);
      '+': if startsWith(tt_cso_assignPlus)      then apply(tt_cso_assignPlus)
                                                 else apply(tt_operatorPlus);
      '&': if startsWith(tt_cso_assignStrConcat) then apply(tt_cso_assignStrConcat)
                                                 else apply(tt_operatorStrConcat);
      '-': if startsWith(tt_declare)             then apply(tt_declare) else
           if startsWith(tt_cso_assignMinus)     then apply(tt_cso_assignMinus)
                                                 else apply(tt_operatorMinus);
      '*': if startsWith('**')                   then apply(2,tt_operatorPot) else
           if startsWith(tt_cso_assignMult)      then apply(tt_cso_assignMult)
                                                 else apply(tt_operatorMult);
      '>': if startsWith(tt_comparatorGeq)       then apply(tt_comparatorGeq) else
           if startsWith(tt_cso_mapDrop)         then apply(tt_cso_mapDrop)
                                                 else apply(tt_comparatorGrt);
      '=': if startsWith(tt_comparatorListEq)    then apply(tt_comparatorListEq)
                                                 else apply(tt_comparatorEq);
      '<': if startsWith(tt_comparatorNeq)       then apply(tt_comparatorNeq) else
           if startsWith(tt_cso_mapPut)          then apply(tt_cso_mapPut) else
           if startsWith(tt_comparatorLeq)       then apply(tt_comparatorLeq)
                                                 else apply(tt_comparatorLss);
      '!': if startsWith('!=')                   then apply(2,tt_comparatorNeq)
           else begin fail('Cannot parse: '+copy(line,lineLocation.column,20)+' (first char is "'+line[lineLocation.column]+'"=#'+intToStr(ord(line[lineLocation.column]))+')'); lineLocation.column:=length(line)+1; end;
      else begin
        fail('Cannot parse: '+copy(line,lineLocation.column,20)+' (first char is "'+line[lineLocation.column]+'"=#'+intToStr(ord(line[lineLocation.column]))+')');
        lineLocation.column:=length(line)+1;
      end;
    end;
    if parsedLength>0 then inc(lineLocation.column,parsedLength);
  end;

PROCEDURE T_tokenArray.tokenizeAll(CONST inputString: ansistring; CONST location: T_tokenLocation; CONST inPackage: P_abstractPackage; VAR adapters: T_adapters; CONST retainBlanks: boolean);
  VAR next:T_token;
      workLocation:T_tokenLocation;
  begin
    workLocation:=location;
    workLocation.column:=1;
    while (workLocation.column<=length(inputString)) and (adapters.noErrors) do begin
      next:=getToken(inputString,workLocation,inPackage,adapters,retainBlanks);
      if (next.tokType<>tt_EOL) or (next.txt<>'') then begin
        if next.txt=SPECIAL_COMMENT_BLOB_BEGIN then begin
          if retainBlanks then append(next);
        end else begin
          next.location:=workLocation;
          append(next);
        end;
      end;
    end;
    setLength(token,tokenFill);
  end;

{$ifdef fullVersion}
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
{$endif}

PROCEDURE T_tokenArray.tokenizeAll(CONST inPackage: P_abstractPackage; VAR adapters: T_adapters);
  VAR location:T_tokenLocation;
      lineIndex:longint;
      next:T_token;
      lines:T_arrayOfString;

  begin
    lines:=inPackage^.codeProvider^.getLines;
    location.package:=inPackage;
    for lineIndex:=0 to length(lines)-1 do begin
      location.line:=lineIndex+1;
      location.column:=1;
      while (location.column<=length(lines[lineIndex])) and (adapters.noErrors) do begin
        next:=getToken(lines[lineIndex],location,inPackage,adapters);
        if (next.tokType<>tt_EOL) or (next.txt<>'') then append(next) else next.destroy;
      end;
    end;
    //Handle unfinished blobs
    if blob.text<>'' then begin
      next.create;
      next.define(blob.start,'',tt_literal,newStringLiteral(blob.text));
      append(next);
      blob.text:='';
    end;
    setLength(token,tokenFill);
  end;

INITIALIZATION
  {$ifdef fullVersion}
  rawTokenizeCallback:=@tokenizeAllReturningRawTokens;
  {$endif}

end.
