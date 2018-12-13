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
CONST MAX_FORMATS_TO_CACHE=4096;

PROCEDURE onPackageFinalization(CONST package:P_objectWithPath);
IMPLEMENTATION
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
    PROCEDURE formatAppend(VAR txt:ansistring; CONST l:P_literal; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
    DESTRUCTOR destroy;
  end;

  P_preparedFormatStatement=^T_preparedFormatStatement;
  T_preparedFormatStatement=object
    inPackage:P_objectWithPath;
    parts:T_arrayOfString;
    formats:array of T_format;
    formatSubrule:P_inlineExpression;
    isTemporary:boolean;
    CONSTRUCTOR create(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler; CONST temp:boolean=false);
    DESTRUCTOR destroy;
    FUNCTION format(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):T_arrayOfString;
  end;

{$i func_defines.inc}
VAR cachedFormats:specialize G_stringKeyMap<P_preparedFormatStatement>;
    cachedFormatCS:TRTLCriticalSection;

PROCEDURE onPackageFinalization(CONST package:P_objectWithPath);
  VAR formats:cachedFormats.KEY_VALUE_LIST;
      i:longint;
  begin
    enterCriticalSection(cachedFormatCS);
    formats:=cachedFormats.entrySet;
    for i:=0 to length(formats)-1 do
    if formats[i].value^.inPackage=package then begin
      dispose(formats[i].value,destroy);
      cachedFormats.dropKey(formats[i].key);
    end;
    leaveCriticalSection(cachedFormatCS);
  end;

PROCEDURE clearCachedFormats;
  VAR f:cachedFormats.VALUE_TYPE_ARRAY;
      i:longint;
  begin
    system.enterCriticalSection(cachedFormatCS);
    f:=cachedFormats.valueSet;
    for i:=0 to length(f)-1 do dispose(f[i],destroy);
    setLength(f,0);
    cachedFormats.clear;
    system.leaveCriticalSection(cachedFormatCS);
  end;

CONSTRUCTOR T_format.create(CONST formatString: ansistring);
  VAR parts:T_arrayOfString;
  begin
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

PROCEDURE T_format.formatAppend(VAR txt:ansistring; CONST l:P_literal; CONST location:T_tokenLocation; CONST context:P_context; VAR recycler:T_recycler);
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

  begin
    case category of
      fmtCat_scientific, fmtCat_fixedPoint, fmtCat_general, fmtCat_currency, fmtCat_number: case l^.literalType of
        lt_real,
        lt_smallint,
        lt_bigint: begin txt:=txt+sysutils.format(realFmt,[extended(P_numericLiteral(l)^.floatValue)]); exit; end;
      end;
      fmtCat_decimal: if l^.literalType in [lt_smallint,lt_bigint] then begin txt+=pad(P_abstractIntLiteral(l)^.toString   ,true); exit; end;
      fmtCat_hex    : if l^.literalType in [lt_smallint,lt_bigint] then begin txt+=pad(P_abstractIntLiteral(l)^.toHexString,true); exit; end;
    end;
    txt+=sysutils.format(strFmt,[P_abstractPackage(location.package)^.literalToString(L,location,context,recycler)]);
  end;

DESTRUCTOR T_format.destroy;
  begin
    realFmt:='';
    strFmt:='';
  end;

FUNCTION getFormat(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_preparedFormatStatement;
  begin;
    if not(context.messages^.continueEvaluation) then exit(nil);
    if tryEnterCriticalsection(cachedFormatCS)=0 then begin
      new(result,create(formatString,tokenLocation,context,recycler,true));
      exit(result);
    end;
    if cachedFormats.containsKey(formatString,result) then begin
      system.leaveCriticalSection(cachedFormatCS);
      exit(result);
    end;
    new(result,create(formatString,tokenLocation,context,recycler));
    if context.messages^.continueEvaluation then cachedFormats.put(formatString,result)
    else begin
      dispose(result,destroy);
      result:=nil;
    end;
    system.leaveCriticalSection(cachedFormatCS);
  end;

CONSTRUCTOR T_preparedFormatStatement.create(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler; CONST temp:boolean=false);
  FUNCTION splitFormatString(CONST formatString:ansistring):T_arrayOfString;
    CONST FORMAT_CHARS:T_charSet=['d','D','e','E','f','F','g','G','m','M','n','N','s','S','x','X'];
    VAR i:longint=1;
        partStart:longint=1;
        bracketLevel:longint=0;
        part:ansistring;
        fmtPart:boolean=false;
        simpleFmtPart:boolean=false;
    begin
      part:='';
      setLength(result,0);
      while i<=length(formatString) do begin
        case formatString[i] of
          '\': if not(fmtPart) and (i+1<=length(formatString)) and (formatString[i+1] in ['%','{','}']) then begin
                 part+=formatString[i+1];
                 inc(i);
               end else part+=formatString[i];
          '{': if fmtPart then begin
                 inc(bracketLevel);
                 part+=formatString[i];
               end else begin
                 setLength(result,length(result)+1);
                 result[length(result)-1]:=part;
                 part:='%{';
                 partStart:=i;
                 bracketLevel:=1;
                 fmtPart:=true; simpleFmtPart:=true;
               end;
          '}': begin
                 if fmtPart then dec(bracketLevel);
                 part+=formatString[i];
                 if (bracketLevel=0) and fmtPart and simpleFmtPart then begin
                   part+='s';
                   setLength(result,length(result)+1);
                   result[length(result)-1]:=part;
                   part:='';
                   partStart:=i;
                   bracketLevel:=0;
                   fmtPart:=false;
                   simpleFmtPart:=false;
                 end;
               end;
          '%': if fmtPart then begin
                 if bracketLevel>0
                 then part+=formatString[i]
                 else context.raiseError('Invalid format specification: '+copy(formatString,partStart,length(formatString)),tokenLocation);
               end else begin
                 if (i+1<=length(formatString)) and (formatString[i+1]='%') then begin
                   part+='%';
                   inc(i);
                 end else begin
                   setLength(result,length(result)+1);
                   result[length(result)-1]:=part;
                   part:='%';
                   partStart:=i;
                   bracketLevel:=0;
                   fmtPart:=true;
                 end;
               end;
          'a'..'z','A'..'Z':
               if fmtPart then begin
                 part+=formatString[i];
                 if bracketLevel<=0 then begin
                   if not(formatString[i] in FORMAT_CHARS) then
                     context.raiseError('Invalid format specification: Unknown format "'+formatString[i]+'"',tokenLocation);
                   setLength(result,length(result)+1);
                   result[length(result)-1]:=part;
                   part:='';
                   partStart:=i;
                   bracketLevel:=0;
                   fmtPart:=false;
                 end;
               end else part+=formatString[i];
          else part+=formatString[i];
        end;
        inc(i);
      end;
      if part<>'' then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=part;
      end;
    end;

  FUNCTION getFormatSubrule(VAR parts:T_arrayOfString):P_inlineExpression;
    VAR i,k:longint;
        needSubRule:boolean=false;
        expressionString:ansistring;
        tempStringLiteral:P_stringLiteral;

    PROCEDURE splitPart(VAR part:ansistring; CONST index:longint);
      VAR expPart:ansistring;
      begin
        part:=trim(part);
        if pos('{',part)<=0 then begin
          if pos('}',part)>0 then context.raiseError('Invalid format specification: '+escapeString(part,es_dontCare),tokenLocation);
          expPart:='$'+intToStr(index);
        end else begin
          expPart:=copy(part,3,pos('}',part)-3);
          part:='%'+copy(part,pos('}',part)+1,length(part));
          if (pos('{',part)>0) or (pos('}',part)>0) then context.raiseError('Invalid format specification: '+escapeString(part,es_dontCare),tokenLocation);
        end;
        if expressionString=''
        then expressionString:=                 '['+expPart
        else expressionString:=expressionString+','+expPart;
        if part='%' then part:='%S';
      end;

    begin
      //Check if a rule is needed at all:
      for i:=0 to length(parts)-1 do if odd(i) and (copy(trim(parts[i]),2,1)='{') then needSubRule:=true;
      if not(needSubRule) then exit(nil);

      expressionString:='';
      k:=0;
      for i:=0 to length(parts)-1 do begin
        if odd(i) then begin
          if parts[i]<>'' then begin
            splitPart(parts[i],k);
            inc(k);
          end;
        end;
      end;
      if context.continueEvaluation
      then begin
        tempStringLiteral:=newStringLiteral('{'+expressionString+']}');
        result:=stringOrListToExpression(tempStringLiteral,
                                         tokenLocation,
                                         context,recycler);
        disposeLiteral(tempStringLiteral);
      end
      else result:=nil;
    end;

  VAR i:longint;
  begin
    isTemporary:=temp;
    inPackage:=tokenLocation.package;
    parts:=splitFormatString(formatString);
    formatSubrule:=getFormatSubrule(parts);
    setLength(formats,length(parts));
    for i:=0 to length(parts)-1 do if odd(i) then formats[i].create(parts[i]);
  end;

DESTRUCTOR T_preparedFormatStatement.destroy;
  VAR i:longint;
  begin
    if formatSubrule<>nil then disposeLiteral(formatSubrule);
    for i:=0 to length(parts)-1 do parts[i]:='';
    setLength(parts,0);
    for i:=0 to length(formats)-1 do formats[i].destroy;
    setLength(formats,0);
  end;

FUNCTION T_preparedFormatStatement.format(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):T_arrayOfString;
  VAR iter:array of T_arrayOfLiteral;

  FUNCTION getFormattedString(CONST index:longint):ansistring;
    FUNCTION simpleFormat(CONST parts:T_arrayOfString; CONST p:T_listLiteral):ansistring;
      VAR i,k:longint;
      begin
        result:='';
        k:=0;
        try
          for i:=0 to length(parts)-1 do if odd(i) then begin
            if k<p.size
            then formats[i].formatAppend(result,p.value[k],tokenLocation,@context,recycler)
            else result:=result+'%'+parts[i]+'%';
            inc(k);
          end else result:=result+parts[i];
        except on E:Exception do context.raiseError('Error during formatting: '+e.message,tokenLocation);
        end;
      end;

    VAR fpar:P_listLiteral;
        temp:P_literal;
        k:longint;
        oldSideEffectWhitelist:T_sideEffects;
    begin
      //prepare parameters
      fpar:=newListLiteral(params^.size-1);
      for k:=1 to params^.size-1 do
      if params^.value[k]^.literalType in C_compoundTypes
      then fpar^.append(iter[k][index],true)
      else fpar^.append(iter[k][    0],true);

      if formatSubrule<>nil then begin
        oldSideEffectWhitelist:=context.setAllowedSideEffectsReturningPrevious([
          se_readPackageState,
          se_alterContextState,
          se_readFile,
          se_accessHttp,
          se_accessIpc,
          se_executingExternal]*context.sideEffectWhitelist);
        temp:=formatSubrule^.evaluateFormat(tokenLocation,context,recycler,fpar);
        context.setAllowedSideEffectsReturningPrevious(oldSideEffectWhitelist);
        disposeLiteral(fpar);
        if (temp<>nil) and (temp^.literalType in C_listTypes)
        then fpar:=P_listLiteral(temp)
        else begin
          if temp<>nil then disposeLiteral(temp);
          exit(''); //One of the called routines already raised a proper error
        end;
      end;
      result:=simpleFormat(parts,fpar^);
      disposeLiteral(fpar);
    end;

  VAR i:longint;
      listSize:longint=-1;
  begin
    for i:=1 to params^.size-1 do if (params^.value[i]^.literalType in C_compoundTypes) and (P_compoundLiteral(params^.value[i])^.customType=nil) then begin
      if listSize=-1 then listSize:=P_compoundLiteral(params^.value[i])^.size
                  else if listSize<>P_compoundLiteral(params^.value[i])^.size then begin
        context.raiseError('Invalid list lengths '+intToStr(listSize)+' and '+intToStr(P_compoundLiteral(params^.value[i])^.size)+' for formatting.',tokenLocation);
        exit(C_EMPTY_STRING_ARRAY);
      end;
    end;
    if listSize=-1 then listSize:=1;
    setLength(iter,params^.size);
    for i:=1 to params^.size-1 do if (params^.value[i]^.literalType in C_compoundTypes) and (P_compoundLiteral(params^.value[i])^.customType=nil)
    then iter[i]:=P_compoundLiteral(params^.value[i])^.iteratableList
    else begin setLength(iter[i],1); iter[i][0]:=params^.value[i]^.rereferenced; end;

    if formatSubrule=nil
    then i:=length(parts) shr 1
    else i:=formatSubrule^.arity;
    if i<>(params^.size-1) then begin
      context.raiseError('Invalid format statement; found '+intToStr(i)+' placeholders but '+intToStr(params^.size-1)+' variables.',tokenLocation);
      if formatSubrule<>nil then begin
        context.raiseError('Helper subrule is: '+formatSubrule^.toString,tokenLocation);
        disposeLiteral(formatSubrule);
        formatSubrule:=nil;
      end;
      for i:=1 to length(iter)-1 do disposeLiteral(iter[i]);
      exit(C_EMPTY_STRING_ARRAY);
    end;
    setLength(result,listSize);
    for i:=0 to listSize-1 do if (context.messages^.continueEvaluation) then result[i]:=getFormattedString(i);
    if not(context.messages^.continueEvaluation) then setLength(result,0);
    for i:=1 to length(iter)-1 do disposeLiteral(iter[i]);
  end;

{$ifdef fullVersion}VAR formatLoc:P_intFuncCallback; {$endif}
FUNCTION format_imp intFuncSignature;
  VAR txt:T_arrayOfString;
      i:longint;
      preparedStatement:P_preparedFormatStatement;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      {$ifdef fullVersion}
      context.callStackPush(tokenLocation,getIntrinsicRuleAsExpression(formatLoc),nil);
      {$endif}
      preparedStatement:=getFormat(P_stringLiteral(arg0)^.value,tokenLocation,context,recycler);
      {$ifdef fullVersion}
      context.callStackPop(nil);
      {$endif}
      if not(context.messages^.continueEvaluation) then exit(nil);
      txt:=preparedStatement^.format(params,tokenLocation,context,recycler);
      if preparedStatement^.isTemporary then dispose(preparedStatement,destroy);
      if length(txt)=1 then result:=newStringLiteral(txt[0])
      else begin
        result:=newListLiteral;
        for i:=0 to length(txt)-1 do P_listLiteral(result)^.appendString(txt[i]);
      end;
    end;
  end;

{$ifdef fullVersion}VAR printfLoc:P_intFuncCallback;{$endif}
FUNCTION printf_imp intFuncSignature;
  VAR preparedStatement:P_preparedFormatStatement;
  begin
    result:=nil;
    if not(context.checkSideEffects('printf',tokenLocation,[se_output])) then exit(nil);
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      {$ifdef fullVersion}
      context.callStackPush(tokenLocation,getIntrinsicRuleAsExpression(printfLoc),nil);
      {$endif}
      preparedStatement:=getFormat(P_stringLiteral(arg0)^.value,tokenLocation,context,recycler);
      if not(context.messages^.continueEvaluation) then begin
        {$ifdef fullVersion}context.callStackPop(nil);{$endif}
        exit(nil);
      end;
      system.enterCriticalSection(print_cs);
      context.messages^.postTextMessage(mt_printline,C_nilTokenLocation,formatTabs(reSplit(preparedStatement^.format(params,tokenLocation,context,recycler))));
      system.leaveCriticalSection(print_cs);
      if preparedStatement^.isTemporary then dispose(preparedStatement,destroy);
      result:=newVoidLiteral;
      {$ifdef fullVersion}context.callStackPop(nil);{$endif}
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
      result:=newStringLiteral(simpleResult);
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
        context.raiseError('parseTime expects two strings of equal length as parameters',tokenLocation);
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
        on E:Exception do context.raiseError('parseTime failed:'+E.message,tokenLocation);
      end;
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) then begin
      format:= uppercase(P_stringLiteral(arg0)^.value);
      if (arg1^.literalType=lt_string) then
        result:=newRealLiteral(encodeDateTime(P_stringLiteral(arg1)^.value))
      else if (arg1^.literalType in [lt_stringList,lt_emptyList]) then begin
        result:=newListLiteral;
        for i:=0 to list1^.size-1 do
          P_listLiteral(result)^.appendReal(encodeDateTime(P_stringLiteral(list1^.value[i])^.value));
      end;
    end else result:=genericVectorization('parseTime',params,tokenLocation,context,recycler);
  end;

INITIALIZATION
  cachedFormats.create;
  initialize(cachedFormatCS);
  system.initCriticalSection(cachedFormatCS);
  {$ifdef fullVersion}printfLoc:={$endif}
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printf'           ,@printf_imp,ak_variadic_1,'printf(formatString:String,...);//Prints a formatted version of the given 0..n parameters and returns void, see <a href="formatStrings.html">Format Strings</a>');
  {$ifdef fullVersion}formatLoc:={$endif}
  registerRule(STRINGS_NAMESPACE       ,'format'           ,@format_imp           ,ak_variadic_1,'format(formatString:String,...);//Returns a formatted version of the given 0..n parameters, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(STRINGS_NAMESPACE       ,'formatTime'       ,@formatTime_imp       ,ak_binary    ,'formatTime(formatString:String,t);//Returns time t (numeric list or scalar) formatted using format string, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(STRINGS_NAMESPACE       ,'parseTime'        ,@parseTime_imp        ,ak_binary    ,'parseTime(formatString:String,input:String);//Parses time from a given date format and input, see <a href="formatStrings.html">Format Strings</a>');
  memoryCleaner.registerCleanupMethod(@clearCachedFormats)

FINALIZATION
  clearCachedFormats;
  cachedFormats.destroy;
  system.doneCriticalSection(cachedFormatCS);

end.
