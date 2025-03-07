UNIT funcs_format;
INTERFACE
USES sysutils,
     myGenerics,myStringUtil,mySys,
     basicTypes,mnh_constants,
     tokenArray,
     litVar,
     subrules,
     mnh_messages,
     recyclers,
     contexts,
     funcs;

PROCEDURE formatMetaData(VAR meta:T_ruleMetaData; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
PROCEDURE clearFormatCache;
IMPLEMENTATION
USES out_adapters,LazUTF8,tokens;
TYPE
  T_format=object
    category:(fmtCat_decimal,
              fmtCat_scientific,
              fmtCat_fixedPoint,
              fmtCat_general,
              fmtCat_currency,
              fmtCat_number,
              fmtCat_string,
              fmtCat_hex);
    realFmt,strFmt:string;
    lengthPar1,lengthPar2:longint;
    CONSTRUCTOR create(CONST formatString:ansistring);
    PROCEDURE formatAppend(VAR txt:ansistring; CONST l:P_literal; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
    DESTRUCTOR destroy;
  end;

  P_preparedFormatStatement=^T_preparedFormatStatement;
  T_preparedFormatStatement=object
    private
      _formatString:ansistring;
      _formatLocation:T_tokenLocation;
      _refCount:longint;
    public
    inPackage:P_objectWithPath;
    parts:T_locatedStrings;
    fStringRanges:T_simpleTokenRanges;
    formats:array of T_format;
    formatSubrule:P_inlineExpression;
    CONSTRUCTOR create(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
    DESTRUCTOR destroy;
    FUNCTION format(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):T_arrayOfString;
    PROCEDURE markAsUnused;
    FUNCTION equals(CONST fmtString:ansistring; CONST loc:T_tokenLocation):boolean;
  end;

{$i func_defines.inc}
CONSTRUCTOR T_format.create(CONST formatString: ansistring);
  VAR parts:T_arrayOfString;
  begin
    DefaultFormatSettings.DecimalSeparator:='.';
    parts:=split(copy(formatString,2,length(formatString)-2),'.');
    if length(parts)>0 then lengthPar1:=strToIntDef(parts[0],-1) else lengthPar1:=-1;
    if length(parts)>1 then lengthPar2:=strToIntDef(parts[1],-1) else lengthPar2:=-1;

    if length(formatString)>0 then case formatString[length(formatString)] of
      'd','D': begin
        category:=fmtCat_decimal;
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      'e','E': begin
        category:=fmtCat_scientific;
        realFmt:=formatString;
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
        lengthPar2:=-1;
      end;
      'f','F': begin
        category:=fmtCat_fixedPoint;
        realFmt:=formatString;
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'g','G': begin
        category:=fmtCat_general;
        realFmt:=formatString;
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
        lengthPar2:=-1;
      end;
      'm','M': begin
        category:=fmtCat_currency;
        realFmt:=formatString;
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'n','N': begin
        category:=fmtCat_number;
        realFmt:=formatString;
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      's','S': begin
        category:=fmtCat_string;
        strFmt :=formatString;
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      'x','X': begin
        category:=fmtCat_hex;
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      else begin
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
    end;
  end;

PROCEDURE T_format.formatAppend(VAR txt:ansistring; CONST l:P_literal; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  FUNCTION pad(CONST s:string; CONST numberFormat:boolean):string;
    VAR leadingMinus:byte=0;
    begin
      result:=s;
      if numberFormat and (copy(result,1,1)='-') then begin
        leadingMinus:=1;
        result:=copy(result,2,length(result)-1);
      end;
      while length(result)+leadingMinus<lengthPar2 do result:='0'+result;
      if leadingMinus=1 then result:='-'+result;
      while length(result)             <lengthPar1 do result:=' '+result;
    end;

  FUNCTION formatUtf8String(CONST s:string):string;
    begin
      if (lengthPar2>0) and (UTF8Length(s)>lengthPar2)
      then result:=UTF8Copy(s,1,lengthPar2)
      else result:=s;
      if lengthPar1<0
      then while UTF8Length(result)<-lengthPar1 do result+=' '
      else while UTF8Length(result)< lengthPar1 do result:=' '+result;
    end;

  begin
    DefaultFormatSettings.DecimalSeparator:='.';
    case category of
      fmtCat_scientific, fmtCat_fixedPoint, fmtCat_general, fmtCat_currency, fmtCat_number: case l^.literalType of
        lt_real,
        lt_smallint,
        lt_bigint: begin txt:=txt+sysutils.format(realFmt,[extended(P_numericLiteral(l)^.floatValue)]); exit; end;
      end;
      fmtCat_decimal: if l^.literalType in [lt_smallint,lt_bigint] then begin txt+=pad(P_abstractIntLiteral(l)^.toString   ,true); exit; end;
      fmtCat_hex    : if l^.literalType in [lt_smallint,lt_bigint] then begin txt+=pad(P_abstractIntLiteral(l)^.toHexString,true); exit; end;
    end;
    if (L^.literalType=lt_string) and (P_stringLiteral(L)^.getEncoding=se_utf8)
    then txt+=formatUtf8String(       P_abstractPackage(location.package)^.literalToString(L,location,context,recycler))
    else txt+=sysutils.format(strFmt,[P_abstractPackage(location.package)^.literalToString(L,location,context,recycler)]);
  end;

DESTRUCTOR T_format.destroy;
  begin
    realFmt:='';
    strFmt:='';
  end;

FUNCTION formatComment(CONST commentString:string; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):string;
  VAR fmt:T_preparedFormatStatement;
      dummyParams:P_listLiteral;
      resultList:T_arrayOfString;
      tryMessages:T_messagesErrorHolder;
      oldMessages:P_messages;
  begin
    if pos('{',commentString)<=0 then exit(commentString);
    oldMessages:=context^.messages;
    tryMessages.createErrorHolder(context^.messages,[mt_el3_evalError..mt_el3_userDefined]);
    context^.messages:=@tryMessages;
    fmt.create(commentString,tokenLocation,context,recycler);
    dummyParams:=recycler^.newListLiteral(1);
    dummyParams^.appendString(recycler,commentString);
    resultList:=fmt.format(dummyParams,tokenLocation,context,recycler);
    fmt.destroy;
    recycler^.disposeLiteral(dummyParams);
    if (tryMessages.getFlags*[FlagError,FlagFatalError]=[]) and (length(resultList)=1)
    then result:=resultList[0]
    else result:=commentString;
    tryMessages.destroy;
    context^.messages:=oldMessages;
  end;

PROCEDURE formatMetaData(VAR meta:T_ruleMetaData; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  VAR k:longint;
  begin
    for k:=0 to meta.attributeCount-1 do
      meta.attributeValue[k]:=formatComment(meta.attributeValue[k],tokenLocation,context,recycler);
      meta.comment          :=formatComment(meta.comment          ,tokenLocation,context,recycler);
  end;

VAR formatCacheCs:TRTLCriticalSection;
    formatCache:array of P_preparedFormatStatement;

PROCEDURE clearFormatCache;
  VAR i:longint;
      j:longint=0;
  begin
    enterCriticalSection(formatCacheCs);
    for i:=0 to length(formatCache)-1 do
      if (formatCache[i]^._refCount<=0)
      then dispose(formatCache[i],destroy)
      else begin
        formatCache[j]:=formatCache[i];
        inc(j);
      end;
    setLength(formatCache,j);
    leaveCriticalSection(formatCacheCs);
  end;

FUNCTION getFormat(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_preparedFormatStatement;
  VAR i:longint;
  begin;
    enterCriticalSection(formatCacheCs);
    i:=0;
    while (i<length(formatCache)) and not formatCache[i]^.equals(formatString,tokenLocation) do inc(i);
    if i<length(formatCache)
    then result:=formatCache[i]
    else begin
      new(result,create(formatString,tokenLocation,context,recycler));
      setLength(formatCache,i+1);
      formatCache[i]:=result;
    end;
    interLockedIncrement(result^._refCount);
    leaveCriticalSection(formatCacheCs);
  end;

FUNCTION getFormatTokens(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler; OUT formatStringRanges:T_simpleTokenRanges):P_token;
  VAR temp_format: P_preparedFormatStatement;
  begin
    temp_format:=getFormat(formatString,tokenLocation,context,recycler);
    if temp_format^.formatSubrule=nil
    then begin
      result:=nil;
      setLength(formatStringRanges,0);
    end else begin
      result:=temp_format^.formatSubrule^.getResolvableTokens(recycler);
      formatStringRanges:=temp_format^.fStringRanges;
    end;
    temp_format^.markAsUnused;
  end;

CONSTRUCTOR T_preparedFormatStatement.create(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  FUNCTION splitFormatString(CONST formatString:ansistring):T_locatedStrings;
    CONST FORMAT_CHARS:T_charSet=['d','D','e','E','f','F','g','G','m','M','n','N','s','S','x','X'];
    VAR i:longint=1;
        partStart:longint=1;
        bracketLevel:longint=0;
        part:T_locatedString;
        fmtPart:boolean=false;
        simpleFmtPart:boolean=false;
        resultFill:longint=0;
        rangeCount:longint=1;
        currentLine:longint;
        currentCol :longint;

    PROCEDURE appendPart;
      begin
        if resultFill>=length(result) then setLength(result,resultFill*2);
        result[resultFill]:=part; inc(resultFill);
        part.txt:='';
        part.loc.column:= currentCol+1;
        part.loc.line  :=currentLine;
      end;

    PROCEDURE closeRange;
      begin
        fStringRanges[rangeCount-1].width:=currentCol-fStringRanges[rangeCount-1].x;
        if fStringRanges[rangeCount-1].width=0 then dec(rangeCount);
      end;

    PROCEDURE openRange;
      begin
        if rangeCount>=length(fStringRanges) then setLength(fStringRanges,rangeCount*2);
        fStringRanges[rangeCount].y:=currentLine;
        fStringRanges[rangeCount].x:= currentCol;
        inc(rangeCount);
      end;

    begin
      setLength(fStringRanges,1);
      currentLine       :=tokenLocation.line;
      fStringRanges[0].y:=tokenLocation.line;
      currentCol        :=tokenLocation.column-1; //Offset because highlighter indexes are 0-based
      fStringRanges[0].x:=tokenLocation.column-1;
      part.txt:='';
      part.loc:=tokenLocation;
      setLength(result,1);
      while i<=length(formatString) do begin
        case formatString[i] of
          '\': if not(fmtPart) and (i+1<=length(formatString)) and (formatString[i+1] in ['%','{','}']) then begin
                 part.txt+=formatString[i+1];
                 inc(i); inc(currentCol);
               end else part.txt+=formatString[i];
          '{': if fmtPart then begin
                 if bracketLevel=0 then closeRange;
                 inc(bracketLevel);
                 part.txt+=formatString[i];
               end else begin
                 appendPart;
                 part.txt:='%{';
                 closeRange;
                 dec(part.loc.column);
                 partStart:=i;
                 bracketLevel:=1;
                 fmtPart:=true; simpleFmtPart:=true;
               end;
          '}': begin
                 if fmtPart then dec(bracketLevel);
                 part.txt+=formatString[i];
                 if bracketLevel=0 then begin inc(currentCol); openRange; dec(currentCol); end;
                 if (bracketLevel=0) and fmtPart and simpleFmtPart then begin
                   part.txt+='s';
                   appendPart;
                   partStart:=i;
                   bracketLevel:=0;
                   fmtPart:=false;
                   simpleFmtPart:=false;
                 end;
               end;
          '%': if fmtPart then begin
                 if bracketLevel>0
                 then part.txt+=formatString[i]
                 else context^.raiseError('Invalid format specification: '+copy(formatString,partStart,length(formatString)),tokenLocation);
               end else begin
                 if (i+1<=length(formatString)) and (formatString[i+1]='%') then begin
                   part.txt+='%';
                   inc(i); inc(currentCol);
                 end else begin
                   appendPart;
                   part.txt:='%';
                   partStart:=i;
                   bracketLevel:=0;
                   fmtPart:=true;
                 end;
               end;
          'a'..'z','A'..'Z':
               if fmtPart then begin
                 part.txt+=formatString[i];
                 if bracketLevel<=0 then begin
                   if not(formatString[i] in FORMAT_CHARS) then
                     context^.raiseError('Invalid format specification: Unknown format "'+formatString[i]+'"',tokenLocation);
                   appendPart;
                   part.txt:='';
                   partStart:=i;
                   bracketLevel:=0;
                   fmtPart:=false;
                 end;
               end else part.txt+=formatString[i];
          C_lineBreakChar:
              begin
                closeRange; inc(currentLine); currentCol:=0; openRange; currentCol:=-1;
                part.txt+=formatString[i];
              end
          else part.txt+=formatString[i];

        end;
        inc(i); inc(currentCol);
      end;
      if part.txt<>'' then appendPart;
      closeRange;
      setLength(fStringRanges,rangeCount);
      setLength(result,resultFill);
    end;

  FUNCTION getFormatSubrule(VAR loc_parts:T_locatedStrings):P_inlineExpression;
    VAR i,k:longint;
        needSubRule:boolean=false;
        expressionParts:T_locatedStrings;
        lexer:T_locatedStringLexer;
        statement: T_enhancedStatement;

    PROCEDURE splitPart(VAR part:T_locatedString; CONST index:longint);
      VAR expPart:ansistring;
          nonescapableFound:boolean;
          openAt,closeAt:longint;
      begin
        while (length(part.txt)>=1) and (part.txt[1] in [' ',C_tabChar]) do begin
          part.txt:=copy(part.txt,2,length(part.txt));
          part.loc.column+=1;
        end;
        openAt :=pos('{',part.txt);
        closeAt:=length(part.txt); while (closeAt>0) and (part.txt[closeAt]<>'}') do dec(closeAt);
        if openAt<=0 then begin
          if closeAt>0 then context^.raiseError('Invalid format specification: '+escapeString(part.txt,es_dontCare,se_testPending,nonescapableFound),tokenLocation);
          expPart:='$'+intToStr(index);
        end else begin
          expPart:=copy(part.txt,openAt+1,closeAt-openAt-1);
          part.txt:=copy(part.txt,1,openAt-1)+copy(part.txt,closeAt+1,length(part.txt));
          if (pos('{',part.txt)>0) or (pos('}',part.txt)>0) then context^.raiseError('Invalid format specification: '+escapeString(part.txt,es_dontCare,se_testPending,nonescapableFound),tokenLocation);
        end;

        if length(expressionParts)=1
        then expressionParts[length(expressionParts)-1].txt+='('
        else expressionParts[length(expressionParts)-1].txt+='),(';

        setLength(expressionParts,length(expressionParts)+1);
        expressionParts[length(expressionParts)-1].txt:=expPart;
        expressionParts[length(expressionParts)-1].loc:=part.loc;

        if part.txt='%' then part.txt:='%S';
      end;

    begin
      result:=nil;
      //Check if a rule is needed at all:
      for i:=0 to length(loc_parts)-1 do if odd(i) and ((copy(trim(loc_parts[i].txt),2,1)='{') or (copy(trim(loc_parts[i].txt),1,1)='{')) then needSubRule:=true;
      if not(needSubRule) then exit(nil);

      setLength(expressionParts,1);
      expressionParts[0].loc:=tokenLocation;
      expressionParts[0].txt:='{[';

      k:=0;
      for i:=0 to length(loc_parts)-1 do begin
        if odd(i) then begin
          if loc_parts[i].txt<>'' then begin
            splitPart(loc_parts[i],k);
            inc(k);
          end;
        end;
      end;
      expressionParts[length(expressionParts)-1].txt+=')]}';
      if context^.continueEvaluation
      then begin
        lexer.create(expressionParts,P_abstractPackage(tokenLocation.package));
        statement:=lexer.getNextStatement(context,recycler);
        if (lexer.getNextStatement(context,recycler).token.first<>nil) or
           (statement.assignmentToken<>nil)
        then context^.raiseError('Invalid format statement.',tokenLocation);
        lexer.destroy;

        if (statement.token.first=nil) then context^.raiseError('Invalid format statement.',tokenLocation)
        else begin
          digestInlineExpression(statement.token.first,context,recycler);
          if (statement.token.first=nil) or (statement.token.first^.tokType<>tt_literal) or (P_literal(statement.token.first^.data)^.literalType<>lt_expression) or (statement.token.first^.next<>nil)
          then context^.raiseError('Invalid format statement.',tokenLocation)
          else result:=P_inlineExpression(P_literal(statement.token.first^.data)^.rereferenced);
        end;
        recycler^.cascadeDisposeToken(statement.token.first);

      end
      else result:=nil;
    end;

  VAR i:longint;
  begin
    self._formatLocation:=tokenLocation;
    self._formatString  :=formatString;
    self._refCount      :=0;
    inPackage:=tokenLocation.package;
    parts:=splitFormatString(formatString);
    formatSubrule:=getFormatSubrule(parts);
    setLength(formats,length(parts));
    for i:=0 to length(parts)-1 do if odd(i) then formats[i].create(parts[i].txt);
  end;

DESTRUCTOR T_preparedFormatStatement.destroy;
  VAR i:longint;
  begin
    if formatSubrule<>nil then globalLiteralRecycler.disposeLiteral(formatSubrule);
    for i:=0 to length(parts)-1 do parts[i].txt:='';
    setLength(parts,0);
    for i:=0 to length(formats)-1 do formats[i].destroy;
    setLength(formats,0);
  end;

PROCEDURE T_preparedFormatStatement.markAsUnused;
  begin
    interlockedDecrement(_refCount);
  end;

FUNCTION T_preparedFormatStatement.equals(CONST fmtString:ansistring; CONST loc:T_tokenLocation):boolean;
  begin
    result:=(_formatString=fmtString) and (_formatLocation=loc);
  end;

FUNCTION T_preparedFormatStatement.format(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):T_arrayOfString;
  VAR iter:array of T_arrayOfLiteral;

  FUNCTION getFormattedString(CONST index:longint):ansistring;
    FUNCTION simpleFormat(CONST p:T_listLiteral):ansistring;
      VAR i,k:longint;
      begin
        result:='';
        k:=0;
        try
          for i:=0 to length(parts)-1 do if odd(i) then begin
            if k<p.size
            then formats[i].formatAppend(result,p.value[k],tokenLocation,context,recycler)
            else result:=result+'%'+parts[i].txt+'%';
            inc(k);
          end else result:=result+parts[i].txt;
        except on E:Exception do context^.raiseError('Error during formatting: '+e.message,tokenLocation);
        end;
      end;

    VAR fpar:P_listLiteral;
        temp:P_literal;
        k:longint;
        oldSideEffectWhitelist:T_sideEffects;
    begin
      //prepare parameters
      fpar:=recycler^.newListLiteral(params^.size-1);
      for k:=1 to params^.size-1 do
      if params^.value[k]^.literalType in C_collectionTypes
      then fpar^.append(recycler,iter[k][index],true)
      else fpar^.append(recycler,iter[k][    0],true);

      if formatSubrule<>nil then begin
        oldSideEffectWhitelist:=context^.setAllowedSideEffectsReturningPrevious(C_sideEffectsForFormatting*context^.sideEffectWhitelist);
        temp:=formatSubrule^.evaluateFormat(tokenLocation,context,recycler,fpar);
        context^.setAllowedSideEffectsReturningPrevious(oldSideEffectWhitelist);
        recycler^.disposeLiteral(fpar);
        if (temp<>nil) and (temp^.literalType in C_listTypes)
        then fpar:=P_listLiteral(temp)
        else begin
          if temp<>nil then recycler^.disposeLiteral(temp);
          exit(''); //One of the called routines already raised a proper error
        end;
      end;
      result:=simpleFormat(fpar^);
      recycler^.disposeLiteral(fpar);
    end;

  VAR i:longint;
      listSize:longint=-1;
  begin
    for i:=1 to params^.size-1 do if (params^.value[i]^.literalType in C_collectionTypes) and (P_collectionLiteral(params^.value[i])^.customType=nil) then begin
      if listSize=-1 then listSize:=P_collectionLiteral(params^.value[i])^.size
                  else if listSize<>P_collectionLiteral(params^.value[i])^.size then begin
        context^.raiseError('Invalid list lengths '+intToStr(listSize)+' and '+intToStr(P_collectionLiteral(params^.value[i])^.size)+' for formatting.',tokenLocation);
        exit(C_EMPTY_STRING_ARRAY);
      end;
    end;
    if listSize=-1 then listSize:=1;
    setLength(iter,params^.size);
    for i:=1 to params^.size-1 do if (params^.value[i]^.literalType in C_collectionTypes) and (P_collectionLiteral(params^.value[i])^.customType=nil)
    then iter[i]:=P_collectionLiteral(params^.value[i])^.forcedIterableList(recycler)
    else begin setLength(iter[i],1); iter[i][0]:=params^.value[i]^.rereferenced; end;

    if formatSubrule=nil
    then i:=length(parts) shr 1
    else i:=formatSubrule^.arity.minPatternLength;

    if i<>(params^.size-1) then begin
      context^.raiseError('Invalid format statement; found '+intToStr(i)+' placeholders but '+intToStr(params^.size-1)+' variables.',tokenLocation);
      if formatSubrule<>nil then begin
        context^.raiseError('Helper subrule is: '+formatSubrule^.toString,tokenLocation);
        recycler^.disposeLiteral(formatSubrule);
        formatSubrule:=nil;
      end;
      for i:=1 to length(iter)-1 do recycler^.disposeLiterals(iter[i]);
      exit(C_EMPTY_STRING_ARRAY);
    end;
    setLength(result,listSize);
    for i:=0 to listSize-1 do if (context^.messages^.continueEvaluation) then result[i]:=getFormattedString(i);
    if not(context^.messages^.continueEvaluation) then setLength(result,0);
    for i:=1 to length(iter)-1 do recycler^.disposeLiterals(iter[i]);
  end;

FUNCTION format_imp intFuncSignature;
  VAR txt:T_arrayOfString;
      i:longint;
      preparedStatement:P_preparedFormatStatement;

  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      preparedStatement:=getFormat(P_stringLiteral(arg0)^.value,tokenLocation,context,recycler);
      if not(context^.messages^.continueEvaluation) then begin
        preparedStatement^.markAsUnused;
        exit(nil);
      end;
      txt:=preparedStatement^.format(params,tokenLocation,context,recycler);
      preparedStatement^.markAsUnused;
      if length(txt)=1 then result:=recycler^.newStringLiteral(txt[0])
      else begin
        result:=recycler^.newListLiteral;
        for i:=0 to length(txt)-1 do P_listLiteral(result)^.appendString(recycler,txt[i]);
      end;
    end;
  end;

FUNCTION printf_imp intFuncSignature;
  VAR preparedStatement:P_preparedFormatStatement;
      textToPost: T_arrayOfString;
  begin
    result:=nil;
    if not(context^.checkSideEffects('printf',tokenLocation,[se_output])) then exit(nil);
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      preparedStatement:=getFormat(P_stringLiteral(arg0)^.value,tokenLocation,context,recycler);
      if not(context^.messages^.continueEvaluation) then begin
        preparedStatement^.markAsUnused;
        exit(nil);
      end;
      textToPost:=formatTabs(reSplit(preparedStatement^.format(params,tokenLocation,context,recycler)));
      preparedStatement^.markAsUnused;
      context^.messages^.postTextMessage(mt_printline,tokenLocation,textToPost);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION formatTime_imp intFuncSignature;
  FUNCTION fmtIt(CONST fmt:string; CONST t:double):P_stringLiteral;
    CONST placeholders:T_charSet=['Y','y','M','m','D','d','H','h','N','n','S','s','Z','z'];
    VAR simpleResult:shortstring;
        i,i1:longint;
    begin
      simpleResult:=FormatDateTime(fmt,t);
      i1:=length(fmt);
      i :=length(simpleResult);
      if i<i1 then i1:=i;
      for i:=1 to i1 do if not(fmt[i] in placeholders) then simpleResult[i]:=fmt[i];
      result:=recycler^.newStringLiteral(simpleResult);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg1^.literalType in [lt_smallint,lt_bigint,lt_real]) and (arg0^.literalType=lt_string)
    then result:=fmtIt(str0^.value,P_numericLiteral(arg1)^.floatValue)
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=fmtIt('',P_numericLiteral(arg0)^.floatValue)
    else result:=genericVectorization('formatTime',params,tokenLocation,context,recycler);
  end;

FUNCTION parseTime_imp intFuncSignature;
  VAR format:ansistring;
  FUNCTION encodeDateTime(input:ansistring):double;
    CONST digits:T_charSet=['0'..'9'];
    VAR yStr:string='';
        mStr:string='';
        dStr:string='';
        hStr:string='';
        nStr:string='';
        sStr:string='';
        zStr:string='';
        i:word;
        yNum,mNum,dNum:word;
        hNum:word=0;
        nNum:word=0;
        sNum:word=0;
        zNum:word=0;
    begin
      if length(format)<>length(input) then begin
        context^.raiseError('parseTime expects two strings of equal length as parameters',tokenLocation);
        exit;
      end;
      input:=cleanString(input,digits,'0');
      for i:=1 to length(format) do case format[i] of
        'Y': yStr:=yStr+input[i];
        'M': mStr:=mStr+input[i];
        'D': dStr:=dStr+input[i];
        'H': hStr:=hStr+input[i];
        'N': nStr:=nStr+input[i];
        'S': sStr:=sStr+input[i];
        'Z': zStr:=zStr+input[i];
      end;
      DecodeDate(now,yNum,mNum,dNum);
      yNum:=strToIntDef(yStr,yNum);
      mNum:=strToIntDef(mStr,mNum);
      dNum:=strToIntDef(dStr,dNum);
      hNum:=strToIntDef(hStr,hNum);
      nNum:=strToIntDef(nStr,nNum);
      sNum:=strToIntDef(sStr,sNum);
      zNum:=strToIntDef(zStr,zNum);
      try
        result:=EncodeDate(yNum,mNum,dNum)+EncodeTime(hNum,nNum,sNum,zNum);
      except
        on E:Exception do context^.raiseError('parseTime failed:'+E.message,tokenLocation);
      end;
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) then begin
      format:= uppercase(P_stringLiteral(arg0)^.value);
      if (arg1^.literalType=lt_string) then
        result:=recycler^.newRealLiteral(encodeDateTime(P_stringLiteral(arg1)^.value))
      else if (arg1^.literalType in [lt_stringList,lt_emptyList]) then begin
        result:=recycler^.newListLiteral;
        for i:=0 to list1^.size-1 do
          P_listLiteral(result)^.appendReal(recycler,encodeDateTime(P_stringLiteral(list1^.value[i])^.value));
      end;
    end else result:=genericVectorization('parseTime',params,tokenLocation,context,recycler);
  end;

INITIALIZATION
  initCriticalSection(formatCacheCs);
  contexts.clearFormatCache:=@clearFormatCache;
  memoryCleaner.registerCleanupMethod(1,@clearFormatCache);
  PRINTF_FUNCTION:=
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'printf'           ,@printf_imp    ,ak_variadic_1,[se_output]);
  FORMAT_FUNCTION:=
  builtinFunctionMap.registerRule(STRINGS_NAMESPACE       ,'format'           ,@format_imp    ,ak_variadic_1);
  builtinFunctionMap.registerRule(STRINGS_NAMESPACE       ,'formatTime'       ,@formatTime_imp,ak_binary    );
  builtinFunctionMap.registerRule(STRINGS_NAMESPACE       ,'parseTime'        ,@parseTime_imp ,ak_binary    );
  {$ifdef fullVersion}
  tokenArray.getFormatTokens:=@getFormatTokens;
  {$endif}
FINALIZATION
  doneCriticalSection(formatCacheCs);
end.
