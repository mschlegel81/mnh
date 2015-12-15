{$ifdef include_interface}
P_preparedFormatStatement=^T_preparedFormatStatement;
T_preparedFormatStatement=object
  parts:T_arrayOfString;
  formats:array of T_format;
  formatSubrule:P_literal;
  CONSTRUCTOR create(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext);
  DESTRUCTOR destroy;
  FUNCTION format(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):T_arrayOfString;
end;

{$endif}
{$ifdef include_implementation}
VAR cachedFormats:specialize G_stringKeyMap<P_preparedFormatStatement>;
    cachedFormatCS:TRTLCriticalSection;

FUNCTION getFormat(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_preparedFormatStatement;
  begin;
    if not(context.adapters^.noErrors) then exit;
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

CONSTRUCTOR T_preparedFormatStatement.create(CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext);
  FUNCTION splitFormatString(CONST formatString:ansistring):T_arrayOfString;
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

    PROCEDURE splitPart(VAR part:ansistring; CONST index:longint);
      VAR expPart:ansistring;
      begin
        part:=trim(part);
        if pos('{',part)<=0 then begin
          if pos('}',part)>0 then context.adapters^.raiseError('Invalid format specification: '+escapeString(part),tokenLocation);
          expPart:='$'+intToStr(index);
        end else begin
          expPart:=copy(part,3,pos('}',part)-3);
          part:='%'+copy(part,pos('}',part)+1,length(part));
          if (pos('{',part)>0) or (pos('}',part)>0) then context.adapters^.raiseError('Invalid format specification: '+escapeString(part),tokenLocation);
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
      then result:=stringToExpression('{'+expressionString+']}',
                                      tokenLocation,
                                      context)
      else result:=nil;
    end;

  VAR i:longint;
  begin
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
    setLength(parts,0);
    for i:=0 to length(formats)-1 do formats[i].destroy;
    setLength(formats,0);
  end;

FUNCTION T_preparedFormatStatement.format(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):T_arrayOfString;
  FUNCTION getFormattedString(CONST index:longint):ansistring;
    FUNCTION simpleFormat(CONST parts:T_arrayOfString; CONST p:T_listLiteral):ansistring;
      VAR i,k:longint;
      begin
        result:='';
        k:=0;
        try
          for i:=0 to length(parts)-1 do if odd(i) then begin
            if k<p.size
            then formats[i].formatAppend(result,p.value(k))
            else result:=result+'%'+parts[i]+'%';
            inc(k);
          end else result:=result+parts[i];
        except on E:Exception do context.adapters^.raiseError('Error during formatting: '+e.message,tokenLocation);
        end;
      end;

    VAR fpar:P_listLiteral;
        temp:P_literal;
        k:longint;
    begin
      //prepare parameters
      fpar:=newListLiteral;
      for k:=1 to params^.size-1 do
      if params^.value(k)^.literalType in C_validListTypes
      then fpar^.append(P_listLiteral(params^.value(k))^.value(index),true,context.adapters^)
      else fpar^.append(              params^.value(k)               ,true,context.adapters^);

      if formatSubrule<>nil then begin
        temp:=P_subrule(P_expressionLiteral(formatSubrule)^.value)^.directEvaluate(
          P_expressionLiteral(formatSubrule),
          fpar,
          0,
          context);
        disposeLiteral(fpar);
        if (temp<>nil) and (temp^.literalType in C_validListTypes)
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
    for i:=1 to params^.size-1 do if params^.value(i)^.literalType in C_validListTypes then begin
      if listSize=-1 then listSize:=P_listLiteral(params^.value(i))^.size
                  else if listSize<>P_listLiteral(params^.value(i))^.size then begin
        context.adapters^.raiseError('Invalid list lengths '+intToStr(listSize)+' and '+intToStr(P_listLiteral(params^.value(i))^.size)+' for formatting.',tokenLocation);
        exit;
      end;
    end;
    if listSize=-1 then listSize:=1;

    if formatSubrule=nil
    then i:=length(parts) shr 1
    else i:=length(P_subrule(P_expressionLiteral(formatSubrule)^.value)^.pattern.sig);
    if i<>(params^.size-1) then begin
      context.adapters^.raiseError('Invalid format statement; found '+intToStr(i)+' placeholders but '+intToStr(params^.size-1)+' variables.',tokenLocation);
      if formatSubrule<>nil then begin
        context.adapters^.raiseError('Helper subrule is: '+formatSubrule^.toString,tokenLocation);
        disposeLiteral(formatSubrule);
        formatSubrule:=nil;
      end;
      exit;
    end;
    setLength(result,listSize);
    for i:=0 to listSize-1 do if (context.adapters^.noErrors) then result[i]:=getFormattedString(i);
    if not(context.adapters^.noErrors) then setLength(result,0);
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

{$endif}
{$ifdef include_initialization}
cachedFormats.create;
system.initCriticalSection(cachedFormatCS);
{$endif}
{$ifdef include_finalization}
clearCachedFormats;
cachedFormats.destroy;
system.doneCriticalSection(cachedFormatCS)
{$endif}
