UNIT mnh_funcs_regex;
INTERFACE
USES sysutils,Classes,
     myGenerics,
     RegExpr,
     mnh_constants,mnh_basicTypes,
     mnh_out_adapters,
     mnh_litVar,
     mnh_funcs,
     mnh_contexts;

IMPLEMENTATION
{$i mnh_func_defines.inc}
TYPE T_triplet=record
       x,y,z:ansistring;
     end;

PROCEDURE disposeRegex(VAR r:TRegExpr); begin r.free; end;
FUNCTION assertRegexCache(VAR context:T_threadContext):P_regexMap;
  begin
    if context.regexCache=nil then new(context.regexCache,create(@disposeRegex));
    result:=context.regexCache;
  end;

FUNCTION regexForExpression(CONST cache:P_regexMap; CONST expression:ansistring):TRegExpr;
  begin
    if not(cache^.containsKey(expression,result)) then begin
      result:=TRegExpr.create(expression);
      cache^.put(expression,result);
      result.expression:=expression;
    end;
  end;

CONST IS_SCALAR=-1;
FUNCTION listSize(CONST xLit,yLit,zLit:P_literal):longint;
  begin
    if      xLit^.literalType=lt_string     then result:=IS_SCALAR
    else if xLit^.literalType in [lt_stringList,lt_emptyList] then result:=P_listLiteral(xLit)^.size
    else exit(-2);
    if      yLit^.literalType in [lt_stringList,lt_emptyList] then begin
      if result=IS_SCALAR then result:=   P_listLiteral(yLit)^.size
                          else if result<>P_listLiteral(yLit)^.size then exit(-2);
    end else if yLit^.literalType<>lt_string then exit(-2);
    if zLit=nil then exit(result);
    if      zLit^.literalType in [lt_stringList,lt_emptyList] then begin
      if result=IS_SCALAR then result :=P_listLiteral(zLit)^.size
                          else if result<>P_listLiteral(zLit)^.size then exit(-2);
    end else if zLit^.literalType<>lt_string then exit(-2);
  end;

{$WARN 5093 OFF}
FUNCTION triplet(CONST xLit,yLit,zLit:P_literal; CONST index:longint):T_triplet;
  begin
    if xLit<>nil then begin
      if xLit^.literalType=lt_string
      then result.x:=P_stringLiteral(              xLit               )^.value
      else result.x:=P_stringLiteral(P_listLiteral(xLit)^.value[index])^.value;
    end;
    if yLit<>nil then begin
      if yLit^.literalType=lt_string
      then result.y:=P_stringLiteral(              yLit               )^.value
      else result.y:=P_stringLiteral(P_listLiteral(yLit)^.value[index])^.value;
    end;
    if zLit<>nil then begin
      if zLit^.literalType=lt_string
      then result.z:=P_stringLiteral(              zLit               )^.value
      else result.z:=P_stringLiteral(P_listLiteral(zLit)^.value[index])^.value;
    end;
  end;

FUNCTION regexValidate_imp intFuncSignature;
  VAR regexCache:P_regexMap;
      regex:TRegExpr;
      message:string='';
      feedbackMethod:P_expressionLiteral=nil;
      feedbackInput :P_stringLiteral;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      feedbackMethod:=P_expressionLiteral(arg1);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
    end else exit(nil);
    regexCache:=assertRegexCache(context);
    regex:=regexForExpression(regexCache,str0^.value);
    try
      regex.Exec(str0^.value+str0^.value);
    except
      on e:Exception do begin message:=e.message; end;
    end;
    if feedbackMethod=nil then exit(newBoolLiteral(message=''));
    if message<>'' then begin
      feedbackInput:=newStringLiteral(message);
      result:=feedbackMethod^.evaluateToLiteral(tokenLocation,@context,feedbackInput).literal;
      disposeLiteral(feedbackInput);
      exit(result);
    end;
    result:=newVoidLiteral;
  end;

FUNCTION regexMatch_imp intFuncSignature;
  VAR regexCache:P_regexMap;
  FUNCTION regexMatches(CONST trip:T_triplet):boolean;
    VAR regex:TRegExpr;
    begin
      regex:=regexForExpression(regexCache,trip.x);
      regex.inputString:=trip.y;
      try
        result:=regex.Exec(trip.y);
      except
        on e:Exception do begin
           context.adapters^.raiseSystemError(e.message,tokenLocation);
        end;
      end;
    end;
  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      regexCache:=assertRegexCache(context);
      i1:=listSize(arg0,arg1,nil);
      if i1<IS_SCALAR then exit(nil)
      else if i1=IS_SCALAR then result:=newBoolLiteral(regexMatches(triplet(arg1,arg0,nil,0)))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do listResult^.appendBool(regexMatches(triplet(arg1,arg0,nil,i)));
      end;
    end;
  end;

FUNCTION regexMatchComposite_imp intFuncSignature;
  VAR regexCache:P_regexMap;
  FUNCTION regexMatchComposite(CONST trip:T_triplet):P_listLiteral;
    VAR i:longint;
        regex:TRegExpr;
    begin
      regex:=regexForExpression(regexCache,trip.x);
      regex.inputString:=trip.y;
      result:=newListLiteral;
      try
        if regex.Exec(trip.y) then repeat
          for i:=0 to regex.SubExprMatchCount do begin
            result^.append(
              newListLiteral^.
              appendString(regex.match   [i])^.
              appendInt   (regex.MatchPos[i])^.
              appendInt   (regex.MatchLen[i]),false);
          end;
        until not(regex.ExecNext);
      except
        on e:Exception do begin
          context.adapters^.raiseSystemError(e.message,tokenLocation);
        end;
      end;
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      regexCache:=assertRegexCache(context);
      i1:=listSize(arg0,arg1,nil);
      if i1<IS_SCALAR then exit(nil)
      else if i1=IS_SCALAR then result:=regexMatchComposite(triplet(arg1,arg0,nil,0))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do listResult^.append(regexMatchComposite(triplet(arg1,arg0,nil,i)),false);
      end;
    end;
  end;

FUNCTION regexSplit_imp intFuncSignature;
  VAR regexCache:P_regexMap;
  FUNCTION regexSplit(CONST trip:T_triplet):P_listLiteral;
    VAR i:longint;
        pieces : TStrings;
        regex:TRegExpr;
    begin
      regex:=regexForExpression(regexCache,trip.x);
      pieces:=TStringList.create;
      try
        regex.split(trip.y,pieces);
      except
        on e:Exception do begin
          context.adapters^.raiseSystemError(e.message,tokenLocation);
        end;
      end;
      result:=newListLiteral;
      for i:=0 to pieces.count-1 do result^.appendString(pieces[i]);
      pieces.free;
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      regexCache:=assertRegexCache(context);
      i1:=listSize(arg0,arg1,nil);
      if i1<IS_SCALAR then exit(nil)
      else if i1=IS_SCALAR then result:=regexSplit(triplet(arg1,arg0,nil,0))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do listResult^.append(regexSplit(triplet(arg1,arg0,nil,i)),false);
      end;
    end;
  end;

FUNCTION regexReplace_imp intFuncSignature;
  VAR regexCache:P_regexMap;
  FUNCTION regexReplace(CONST trip:T_triplet):ansistring;
    VAR regex:TRegExpr;
    begin
      regex:=regexForExpression(regexCache,trip.x);
      try
        result:=regex.Replace(trip.y,trip.z,false);
      except
        on e:Exception do begin
          context.adapters^.raiseSystemError(e.message,tokenLocation);
        end;
      end;
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) then begin
      regexCache:=assertRegexCache(context);
      i1:=listSize(arg0,arg1,arg2);
      if i1<IS_SCALAR then exit(nil)
      else if i1=IS_SCALAR then result:=newStringLiteral(regexReplace(triplet(arg1,arg0,arg2,0)))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do listResult^.appendString(regexReplace(triplet(arg1,arg0,arg2,i)));
      end;
    end;
  end;

CONST SYNTAX_LINK='#For the syntax of regular expressions see <a href="http://regexpstudio.com/en/regexp_syntax.html">the used library''s website.</a>';
INITIALIZATION
  mnh_funcs.registerRule(REGEX_NAMESPACE,'validateRegex' ,@regexValidate_imp      ,ak_variadic_1,'validateRegex(regex:string);//Returns true iff regex is valid, false otherwise#validateRegex(regex:string,feedback:expression(1));//Returns void iff regex is valid, invokes feedback with error message otherwise');
  mnh_funcs.registerRule(REGEX_NAMESPACE,'matches'       ,@regexMatch_imp         ,ak_binary ,'matches(searchString,regex);//returns true if string/-list searchString matches string/-list regex#//If lists are given they must have equal sizes.'+SYNTAX_LINK);
  mnh_funcs.registerRule(REGEX_NAMESPACE,'matchComposite',@regexMatchComposite_imp,ak_binary ,'matchComposite(searchString,regex);//returns a (list of) triplets: [match,position,length] for string/-list regex and searchString//If lists are given they must have equal sizes.'+SYNTAX_LINK);
  mnh_funcs.registerRule(REGEX_NAMESPACE,'split'         ,@regexSplit_imp         ,ak_binary ,'split(searchString,regex);//splits the string/-list searchString using string/-list regex//If lists are given they must have equal sizes.'+SYNTAX_LINK, true);
  mnh_funcs.registerRule(REGEX_NAMESPACE,'replace'       ,@regexReplace_imp       ,ak_ternary,'replace(searchString,regex,replaceString);//replaces all matching occurences of string/-list regex in string/-list searchString by string/-list replaceString//If lists are given they must have equal sizes.'+SYNTAX_LINK,true);

end.
