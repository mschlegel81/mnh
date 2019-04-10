UNIT funcs_regex;
INTERFACE
USES sysutils,Classes,
     myGenerics,
     RegExpr,
     mnh_constants,basicTypes,
     mnh_messages,
     litVar,
     funcs,
     recyclers,
     contexts;

IMPLEMENTATION
{$i func_defines.inc}
TYPE T_triplet=record
       x,y,z:ansistring;
     end;
     T_regexMapEntry=record
       entryCS:TRTLCriticalSection;
       temporary:boolean;
       RegExpr:TRegExpr;
     end;

     T_regexMap=specialize G_stringKeyMap<T_regexMapEntry>;
VAR regexCache:T_regexMap;
    regexCacheCs:TRTLCriticalSection;
PROCEDURE disposeRegex(VAR r:T_regexMapEntry);
  begin
    enterCriticalSection(r.entryCS);
    r.RegExpr.free;
    leaveCriticalSection(r.entryCS);
    doneCriticalSection(r.entryCS);
  end;

PROCEDURE hardDisposeRegex(VAR r:T_regexMapEntry);
  begin
    r.RegExpr.free;
    doneCriticalSection(r.entryCS);
  end;

FUNCTION regexForExpression(CONST expression:ansistring):T_regexMapEntry;
  begin
    enterCriticalSection(regexCacheCs);
    if regexCache.containsKey(expression,result) then begin
      //contained in map:
      if (tryEnterCriticalsection(result.entryCS)<>0) then begin
        leaveCriticalSection(regexCacheCs);
        exit(result);
      end;
      //contained in map but not accessible
      result.temporary:=true;
      result.RegExpr:=TRegExpr.create(expression);
      result.RegExpr.expression:=expression;
    end else begin
      //not contained in map:
      initCriticalSection(result.entryCS);
      enterCriticalSection(result.entryCS);
      result.temporary:=false;
      result.RegExpr:=TRegExpr.create(expression);
      result.RegExpr.expression:=expression;
      regexCache.put(expression,result);
    end;
    leaveCriticalSection(regexCacheCs);
  end;

PROCEDURE doneRegex(VAR entry:T_regexMapEntry); inline;
  begin
    if entry.temporary
    then entry.RegExpr.free
    else begin
      leaveCriticalSection(entry.entryCS);
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
  VAR regex:T_regexMapEntry;
      message:string='';
      feedbackMethod:P_expressionLiteral=nil;
      feedbackInput :P_stringLiteral;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      feedbackMethod:=P_expressionLiteral(arg1);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
    end else exit(nil);
    regex:=regexForExpression(str0^.value);
    try
      regex.RegExpr.Exec(str0^.value+str0^.value);
    except
      on e:Exception do begin message:=e.message; end;
    end;
    doneRegex(regex);
    if feedbackMethod=nil then exit(newBoolLiteral(message=''));
    if message<>'' then begin
      feedbackInput:=newStringLiteral(message);
      result:=feedbackMethod^.evaluateToLiteral(tokenLocation,@context,@recycler,feedbackInput,nil).literal;
      disposeLiteral(feedbackInput);
      exit(result);
    end;
    result:=newVoidLiteral;
  end;

FUNCTION regexMatch_imp intFuncSignature;
  FUNCTION regexMatches(CONST trip:T_triplet):boolean;
    VAR regex:T_regexMapEntry;
    begin
      regex:=regexForExpression(trip.x);
      regex.RegExpr.inputString:=trip.y;
      try
        result:=regex.RegExpr.Exec(trip.y);
      except
        on e:Exception do begin
          context.raiseError(e.message,tokenLocation,mt_el4_systemError);
        end;
      end;
      doneRegex(regex);
    end;
  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
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
  FUNCTION regexMatchComposite(CONST trip:T_triplet):P_listLiteral;
    VAR i:longint;
        regex:T_regexMapEntry;
    begin
      regex:=regexForExpression(trip.x);
      regex.RegExpr.inputString:=trip.y;
      result:=newListLiteral;
      try
        if regex.RegExpr.Exec(trip.y) then repeat
          for i:=0 to regex.RegExpr.SubExprMatchCount do begin
            result^.append(
              newListLiteral^.
              appendString(regex.RegExpr.match   [i])^.
              appendInt   (regex.RegExpr.MatchPos[i])^.
              appendInt   (regex.RegExpr.MatchLen[i]),false);
          end;
        until not(regex.RegExpr.ExecNext);
      except
        on e:Exception do begin
          context.raiseError(e.message,tokenLocation,mt_el4_systemError);
        end;
      end;
      doneRegex(regex);
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
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
  FUNCTION regexSplit(CONST trip:T_triplet):P_listLiteral;
    VAR i:longint;
        pieces : TStrings;
        regex:T_regexMapEntry;
    begin
      regex:=regexForExpression(trip.x);
      pieces:=TStringList.create;
      try
        regex.RegExpr.split(trip.y,pieces);
      except
        on e:Exception do begin
          context.raiseError(e.message,tokenLocation,mt_el4_systemError);
        end;
      end;
      doneRegex(regex);
      result:=newListLiteral;
      for i:=0 to pieces.count-1 do result^.appendString(pieces[i]);
      pieces.free;
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
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
  FUNCTION regexReplace(CONST trip:T_triplet):ansistring;
    VAR regex:T_regexMapEntry;
    begin
      regex:=regexForExpression(trip.x);
      try
        result:=regex.RegExpr.Replace(trip.y,trip.z,false);
      except
        on e:Exception do begin
          context.raiseError(e.message,tokenLocation,mt_el4_systemError);
        end;
      end;
      doneRegex(regex);
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) then begin
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
  funcs.registerRule(REGEX_NAMESPACE,'validateRegex' ,@regexValidate_imp      ,ak_variadic_1{$ifdef fullVersion},'validateRegex(regex:String);//Returns true iff regex is valid, false otherwise#validateRegex(regex:String,feedback:expression(1));//Returns void iff regex is valid, invokes feedback with error message otherwise'{$endif});
  funcs.registerRule(REGEX_NAMESPACE,'matches'       ,@regexMatch_imp         ,ak_binary    {$ifdef fullVersion},'matches(searchString,regex);//returns true if string/-list searchString matches string/-list regex#//If lists are given they must have equal sizes.'+SYNTAX_LINK{$endif});
  funcs.registerRule(REGEX_NAMESPACE,'matchComposite',@regexMatchComposite_imp,ak_binary    {$ifdef fullVersion},'matchComposite(searchString,regex);//returns a (list of) triplets: [match,position,length] for string/-list regex and searchString//If lists are given they must have equal sizes.'+SYNTAX_LINK{$endif});
  funcs.registerRule(REGEX_NAMESPACE,'split'         ,@regexSplit_imp         ,ak_binary    {$ifdef fullVersion},'split(searchString,regex);//splits the string/-list searchString using string/-list regex//If lists are given they must have equal sizes.'+SYNTAX_LINK{$endif}, true);
  funcs.registerRule(REGEX_NAMESPACE,'replace'       ,@regexReplace_imp       ,ak_ternary   {$ifdef fullVersion},'replace(searchString,regex,replaceString);//replaces all matching occurences of string/-list regex in string/-list searchString by string/-list replaceString//If lists are given they must have equal sizes.'+SYNTAX_LINK{$endif},true);
  initialize(regexCacheCs);
  initCriticalSection(regexCacheCs);
  regexCache.create(@disposeRegex)
FINALIZATION
  regexCache.overrideDisposer(@hardDisposeRegex);
  try regexCache.destroy; except end;
  doneCriticalSection(regexCacheCs);
end.
