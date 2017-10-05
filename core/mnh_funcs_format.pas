UNIT mnh_funcs_format;
INTERFACE
USES sysutils,
     myGenerics,myStringUtil,
     mnh_basicTypes,mnh_constants,
     mnh_litVar,
     mnh_subrules,
     mnh_contexts,
     mnh_funcs;
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
    intFmt,realFmt,strFmt:string;

    CONSTRUCTOR create(CONST formatString:ansistring);
    PROCEDURE formatAppend(VAR txt:ansistring; CONST l:P_literal);
    DESTRUCTOR destroy;
  end;

  P_preparedFormatStatement=^T_preparedFormatStatement;
  T_preparedFormatStatement=object
    inPackage:P_objectWithPath;
    parts:T_arrayOfString;
    formats:array of T_format;
    formatSubrule:P_literal;
    CONSTRUCTOR create(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext);
    DESTRUCTOR destroy;
    FUNCTION format(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):T_arrayOfString;
  end;

PROCEDURE onPackageFinalization(CONST package:P_objectWithPath);
IMPLEMENTATION
{$i mnh_func_defines.inc}
VAR cachedFormats:specialize G_stringKeyMap<P_preparedFormatStatement>;
    cachedFormatCS:TRTLCriticalSection;
    builtinLocation_printf,
    builtinLocation_format:T_identifiedInternalFunction;

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
  begin
    if length(formatString)>0 then case formatString[length(formatString)] of
      'd','D': begin
        category:=fmtCat_decimal;
        intFmt :=formatString;
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      'e','E': begin
        category:=fmtCat_scientific;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'f','F': begin
        category:=fmtCat_fixedPoint;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'g','G': begin
        category:=fmtCat_general;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'm','M': begin
        category:=fmtCat_currency;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'n','N': begin
        category:=fmtCat_number;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      's','S': begin
        category:=fmtCat_string;
        strFmt :=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      'x','X': begin
        category:=fmtCat_hex;
        intFmt :=formatString;
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      else begin
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
    end;
  end;

PROCEDURE T_format.formatAppend(VAR txt:ansistring; CONST l:P_literal);
  begin
    case category of
      fmtCat_scientific, fmtCat_fixedPoint, fmtCat_general, fmtCat_currency, fmtCat_number: case l^.literalType of
        lt_real: begin txt:=txt+sysutils.format(realFmt,[P_realLiteral(l)^.value]); exit; end;
        lt_int : begin txt:=txt+sysutils.format(realFmt,[extended(P_intLiteral(l)^.value)]); exit; end;
      end;
      fmtCat_decimal, fmtCat_hex:
      if l^.literalType=lt_int then begin
        txt:=txt+sysutils.format(intFmt,[P_intLiteral(l)^.value]);
        exit;
      end;
    end;
    if l^.literalType in C_scalarTypes
    then txt:=txt+sysutils.format(strFmt,[P_scalarLiteral(l)^.stringForm])
    else txt:=txt+sysutils.format(strFmt,[l^.toString]);
  end;

DESTRUCTOR T_format.destroy;
  begin
    intFmt:='';
    realFmt:='';
    strFmt:='';
  end;

FUNCTION getFormat(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_preparedFormatStatement;
  begin;
    if not(context.adapters^.noErrors) then exit(nil);
    system.enterCriticalSection(cachedFormatCS);
    if cachedFormats.containsKey(formatString,result) then begin
      system.leaveCriticalSection(cachedFormatCS);
      exit(result);
    end;
    new(result,create(formatString,tokenLocation,context));
    if context.adapters^.noErrors then cachedFormats.put(formatString,result)
    else begin
      dispose(result,destroy);
      result:=nil;
    end;
    system.leaveCriticalSection(cachedFormatCS);
  end;

CONSTRUCTOR T_preparedFormatStatement.create(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext);
  FUNCTION splitFormatString(CONST formatString:ansistring):T_arrayOfString;
    CONST FORMAT_CHARS:T_charSet=['d','D','e','E','f','F','g','G','m','M','n','N','s','S','x','X'];
    VAR i:longint=1;
        partStart:longint=1;
        bracketLevel:longint=0;
        part:ansistring;
        fmtPart:boolean=false;
    begin
      part:='';
      setLength(result,0);
      while i<=length(formatString) do begin
        case formatString[i] of
          '{': begin
                 if fmtPart then inc(bracketLevel);
                 part:=part+formatString[i];
               end;
          '}': begin
                 if fmtPart then dec(bracketLevel);
                 part:=part+formatString[i];
               end;
          '%': if fmtPart then begin
                 if bracketLevel>0
                 then part:=part+formatString[i]
                 else context.adapters^.raiseError('Invalid format specification: '+copy(formatString,partStart,length(formatString)),tokenLocation);
               end else begin
                 if (i+1<=length(formatString)) and (formatString[i+1]='%') then begin
                   part:=part+'%';
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
                 part:=part+formatString[i];
                 if bracketLevel<=0 then begin
                   if not(formatString[i] in FORMAT_CHARS) then
                     context.adapters^.raiseError('Invalid format specification: Unknown format "'+formatString[i]+'"',tokenLocation);
                   setLength(result,length(result)+1);
                   result[length(result)-1]:=part;
                   part:='';
                   partStart:=i;
                   bracketLevel:=0;
                   fmtPart:=false;
                 end;
               end else part:=part+formatString[i];
          else part:=part+formatString[i];
        end;
        inc(i);
      end;
      if part<>'' then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=part;
      end;
    end;

  FUNCTION getFormatSubrule(VAR parts:T_arrayOfString):P_literal;
    VAR i,k:longint;
        needSubRule:boolean=false;
        expressionString:ansistring;
        tempStringLiteral:P_stringLiteral;

    PROCEDURE splitPart(VAR part:ansistring; CONST index:longint);
      VAR expPart:ansistring;
      begin
        part:=trim(part);
        if pos('{',part)<=0 then begin
          if pos('}',part)>0 then context.adapters^.raiseError('Invalid format specification: '+escapeString(part,es_dontCare),tokenLocation);
          expPart:='$'+intToStr(index);
        end else begin
          expPart:=copy(part,3,pos('}',part)-3);
          part:='%'+copy(part,pos('}',part)+1,length(part));
          if (pos('{',part)>0) or (pos('}',part)>0) then context.adapters^.raiseError('Invalid format specification: '+escapeString(part,es_dontCare),tokenLocation);
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
      if context.adapters^.noErrors
      then begin
        tempStringLiteral:=newStringLiteral('{'+expressionString+']}');
        result:=stringOrListToExpression(tempStringLiteral,
                                         tokenLocation,
                                         context);
        disposeLiteral(tempStringLiteral);
      end
      else result:=nil;
    end;

  VAR i:longint;
  begin
    inPackage:=tokenLocation.package;
    parts:=splitFormatString(formatString);
    formatSubrule:=getFormatSubrule(parts);
    if (formatSubrule<>nil) and (formatSubrule^.literalType<>lt_expression) then begin
      disposeLiteral(formatSubrule);
      formatSubrule:=nil;
      exit;
    end;
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

FUNCTION T_preparedFormatStatement.format(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):T_arrayOfString;
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
            then formats[i].formatAppend(result,p.value[k])
            else result:=result+'%'+parts[i]+'%';
            inc(k);
          end else result:=result+parts[i];
        except on E:Exception do context.adapters^.raiseError('Error during formatting: '+e.message,tokenLocation);
        end;
      end;

    VAR fpar:P_listLiteral;
        temp:P_literal;
        k:longint;
        oldSideEffectWhitelist:T_sideEffects;
    begin
      //prepare parameters
      fpar:=newListLiteral;
      for k:=1 to params^.size-1 do
      if params^.value[k]^.literalType in C_compoundTypes
      then fpar^.append(iter[k][index],true)
      else fpar^.append(iter[k][    0],true);

      if formatSubrule<>nil then begin
        oldSideEffectWhitelist:=context.setAllowedSideEffectsReturningPrevious([
          se_outputViaAdapter,
          se_readPackageState,
          se_alterContextState,
          se_readFile,
          se_os_query,
          se_accessHttp,
          se_accessIpc,
          se_executingExternal,
          se_scriptDependent,
          se_executableDependent,
          se_versionDependent]*context.sideEffectWhitelist);
        temp:=P_expression(formatSubrule)^.evaluate(tokenLocation,@context,fpar);
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
    for i:=1 to params^.size-1 do if params^.value[i]^.literalType in C_compoundTypes then begin
      if listSize=-1 then listSize:=P_compoundLiteral(params^.value[i])^.size
                  else if listSize<>P_compoundLiteral(params^.value[i])^.size then begin
        context.adapters^.raiseError('Invalid list lengths '+intToStr(listSize)+' and '+intToStr(P_compoundLiteral(params^.value[i])^.size)+' for formatting.',tokenLocation);
        exit(C_EMPTY_STRING_ARRAY);
      end;
    end;
    if listSize=-1 then listSize:=1;
    setLength(iter,params^.size);
    for i:=1 to params^.size-1 do if params^.value[i]^.literalType in C_compoundTypes
    then iter[i]:=P_compoundLiteral(params^.value[i])^.iteratableList
    else begin setLength(iter[i],1); iter[i][0]:=params^.value[i]^.rereferenced; end;

    if formatSubrule=nil
    then i:=length(parts) shr 1
    else i:=P_expression(formatSubrule)^.arity;
    if i<>(params^.size-1) then begin
      context.adapters^.raiseError('Invalid format statement; found '+intToStr(i)+' placeholders but '+intToStr(params^.size-1)+' variables.',tokenLocation);
      if formatSubrule<>nil then begin
        context.adapters^.raiseError('Helper subrule is: '+formatSubrule^.toString,tokenLocation);
        disposeLiteral(formatSubrule);
        formatSubrule:=nil;
      end;
      for i:=1 to length(iter)-1 do disposeLiteral(iter[i]);
      exit(C_EMPTY_STRING_ARRAY);
    end;
    setLength(result,listSize);
    for i:=0 to listSize-1 do if (context.adapters^.noErrors) then result[i]:=getFormattedString(i);
    if not(context.adapters^.noErrors) then setLength(result,0);
    for i:=1 to length(iter)-1 do disposeLiteral(iter[i]);
  end;

FUNCTION format_imp intFuncSignature;
  VAR txt:T_arrayOfString;
      i:longint;
      preparedStatement:P_preparedFormatStatement;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      {$ifdef fullVersion}
      context.callStackPush(tokenLocation,@builtinLocation_format);
      {$endif}
      preparedStatement:=getFormat(P_stringLiteral(arg0)^.value,tokenLocation,context);
      {$ifdef fullVersion}
      context.callStackPop();
      {$endif}
      if not(context.adapters^.noErrors) then exit(nil);
      txt:=preparedStatement^.format(params,tokenLocation,context);
      if length(txt)=1 then result:=newStringLiteral(txt[0])
      else begin
        result:=newListLiteral;
        for i:=0 to length(txt)-1 do P_listLiteral(result)^.appendString(txt[i]);
      end;
    end;
  end;

FUNCTION printf_imp intFuncSignature;
  VAR preparedStatement:P_preparedFormatStatement;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      {$ifdef fullVersion}
      context.callStackPush(tokenLocation,@builtinLocation_printf);
      {$endif}
      preparedStatement:=getFormat(P_stringLiteral(arg0)^.value,tokenLocation,context);
      if not(context.adapters^.noErrors) then begin
        {$ifdef fullVersion}context.callStackPop();{$endif}
        exit(nil);
      end;
      system.enterCriticalSection(print_cs);
      context.adapters^.printOut(formatTabs(reSplit(preparedStatement^.format(params,tokenLocation,context))));
      system.leaveCriticalSection(print_cs);
      result:=newVoidLiteral;
      {$ifdef fullVersion}context.callStackPop();{$endif}
    end;
  end;

FUNCTION formatTime_imp intFuncSignature;
  VAR L:P_listLiteral;
      i:longint;
      fmt:ansistring;
  FUNCTION fmtIt(CONST t:double):P_stringLiteral;
    begin
      result:=newStringLiteral(FormatDateTime(fmt,t));
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) then begin
      fmt:=P_stringLiteral(arg0)^.value;
      case arg1^.literalType of
        lt_int:  result:=fmtIt(P_intLiteral (arg1)^.value);
        lt_real: result:=fmtIt(P_realLiteral(arg1)^.value);
        lt_emptyList: result:=newListLiteral;
        lt_realList,lt_intList,lt_numList: begin
          result:=newListLiteral;
          L:=list1;
          for i:=0 to L^.size-1 do case(L^.value[i]^.literalType) of
            lt_int : P_listLiteral(result)^.append(fmtIt(P_intLiteral (L^.value[i])^.value),false);
            lt_real: P_listLiteral(result)^.append(fmtIt(P_realLiteral(L^.value[i])^.value),false);
          end;
        end;
      end;
    end;
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
        context.adapters^.raiseError('parseTime expects two strings of equal length as parameters',tokenLocation);
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
        on E:Exception do context.adapters^.raiseError('parseTime failed:'+E.message,tokenLocation);
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
    end;
  end;

INITIALIZATION
  cachedFormats.create;
  initialize(cachedFormatCS);
  system.initCriticalSection(cachedFormatCS);
  builtinLocation_printf.create(SYSTEM_BUILTIN_NAMESPACE,'printf');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printf'         ,@printf_imp,[se_outputViaAdapter],ak_variadic_1,'printf(formatString:string,...);//Prints a formatted version of the given 0..n parameters and returns void, see <a href="formatStrings.html">Format Strings</a>');
  builtinLocation_format.create(STRINGS_NAMESPACE,'format');
  registerRule(STRINGS_NAMESPACE        ,'format'           ,@format_imp           ,[],ak_variadic_1,'format(formatString:string,...);//Returns a formatted version of the given 0..n parameters, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(STRINGS_NAMESPACE        ,'formatTime'       ,@formatTime_imp       ,[],ak_binary    ,'formatTime(formatString:string,t);//Returns time t (numeric list or scalar) formatted using format string, see <a href="formatStrings.html">Format Strings</a>');
  registerRule(STRINGS_NAMESPACE        ,'parseTime'        ,@parseTime_imp        ,[],ak_binary    ,'parseTime(formatString:string,input:string);//Parses time from a given date format and input, see <a href="formatStrings.html">Format Strings</a>');

FINALIZATION
  clearCachedFormats;
  cachedFormats.destroy;
  system.doneCriticalSection(cachedFormatCS);
  builtinLocation_printf.destroy;
  builtinLocation_format.destroy;

end.
