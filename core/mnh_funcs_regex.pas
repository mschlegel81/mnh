UNIT mnh_funcs_regex;
INTERFACE
USES RegExpr,Classes,mnh_litVar,mnh_funcs,mnh_constants,mnh_basicTypes,sysutils,mnh_out_adapters,mnh_contexts;

IMPLEMENTATION
{$i mnh_func_defines.inc}
TYPE T_triplet=record
       x,y,z:ansistring;
     end;

FUNCTION listSize(CONST xLit,yLit,zLit:P_literal):longint;
  begin
    if      xLit^.literalType=lt_string     then result:=0
    else if xLit^.literalType=lt_stringList then result:=P_listLiteral(xLit)^.size
    else exit(-1);
    if      yLit^.literalType=lt_stringList then begin
      if result=0 then result:=   P_listLiteral(yLit)^.size
                  else if result<>P_listLiteral(yLit)^.size then exit(-1);
    end else if yLit^.literalType<>lt_string then exit(-1);
    if zLit=nil then exit(result);
    if      zLit^.literalType=lt_stringList then begin
      if result=0 then result :=P_listLiteral(zLit)^.size
                  else if result<>P_listLiteral(zLit)^.size then exit(-1);
    end else if zLit^.literalType<>lt_string then exit(-1);
  end;

{$WARN 5093 OFF}
FUNCTION triplet(CONST xLit,yLit,zLit:P_literal; CONST index:longint):T_triplet;
  begin
    if xLit<>nil then begin
      if xLit^.literalType=lt_string
      then result.x:=P_stringLiteral(              xLit               )^.value
      else result.x:=P_stringLiteral(P_listLiteral(xLit)^.value(index))^.value;
    end;
    if yLit<>nil then begin
      if yLit^.literalType=lt_string
      then result.y:=P_stringLiteral(              yLit               )^.value
      else result.y:=P_stringLiteral(P_listLiteral(yLit)^.value(index))^.value;
    end;
    if zLit<>nil then begin
      if zLit^.literalType=lt_string
      then result.z:=P_stringLiteral(              zLit               )^.value
      else result.z:=P_stringLiteral(P_listLiteral(zLit)^.value(index))^.value;
    end;
  end;

FUNCTION regexMatch_imp intFuncSignature;
  VAR regex:TRegExpr;
  FUNCTION regexMatches(CONST trip:T_triplet):boolean;
    begin
      regex.expression:=trip.x;
      regex.inputString:=trip.y;
      try
        result:=regex.Exec(trip.y);
      except
        on e:Exception do begin
           context.adapters^.raiseCustomMessage(mt_el5_systemError,e.message,tokenLocation);
        end;
      end;
    end;
  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      regex:=TRegExpr.create;
      i1:=listSize(arg0,arg1,nil);
      if i1<0 then exit(nil)
      else if i1=0 then result:=newBoolLiteral(regexMatches(triplet(arg1,arg0,nil,0)))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do lResult^.appendBool(regexMatches(triplet(arg1,arg0,nil,i)));
      end;
      regex.free;
    end;
  end;

FUNCTION regexMatchComposite_imp intFuncSignature;
  VAR regex:TRegExpr;
  FUNCTION regexMatchComposite(CONST trip:T_triplet):P_listLiteral;
    VAR i:longint;
    begin
      regex.expression:=trip.x;
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
          context.adapters^.raiseCustomMessage(mt_el5_systemError,e.message,tokenLocation);
        end;
      end;
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      regex:=TRegExpr.create;
      i1:=listSize(arg0,arg1,nil);
      if i1<0 then exit(nil)
      else if i1=0 then result:=regexMatchComposite(triplet(arg1,arg0,nil,0))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do lResult^.append(regexMatchComposite(triplet(arg1,arg0,nil,i)),false);
      end;
      regex.free;
    end;
  end;

FUNCTION regexSplit_imp intFuncSignature;
  VAR regex:TRegExpr;
  FUNCTION regexSplit(CONST trip:T_triplet):P_listLiteral;
    VAR i:longint;
        pieces : TStrings;
    begin
      regex.expression:=trip.x;
      pieces:=TStringList.create;
      try
        regex.split(trip.y,pieces);
      except
        on e:Exception do begin
          context.adapters^.raiseCustomMessage(mt_el5_systemError,e.message,tokenLocation);
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
      regex:=TRegExpr.create;
      i1:=listSize(arg0,arg1,nil);
      if i1<0 then exit(nil)
      else if i1=0 then result:=regexSplit(triplet(arg1,arg0,nil,0))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do lResult^.append(regexSplit(triplet(arg1,arg0,nil,i)),false);
      end;
      regex.free;
    end;
  end;

FUNCTION regexReplace_imp intFuncSignature;
  VAR regex:TRegExpr;
  FUNCTION regexReplace(CONST trip:T_triplet):ansistring;
    begin
      regex.expression:=trip.x;
      try
        result:=regex.Replace(trip.y,trip.z,false);
      except
        on e:Exception do begin
          context.adapters^.raiseCustomMessage(mt_el5_systemError,e.message,tokenLocation);
        end;
      end;
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) then begin
      regex:=TRegExpr.create;
      i1:=listSize(arg0,arg1,arg2);
      if i1<0 then exit(nil)
      else if i1=0 then result:=newStringLiteral(regexReplace(triplet(arg1,arg0,arg2,0)))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do lResult^.appendString(regexReplace(triplet(arg1,arg0,arg2,i)));
      end;
    end;
    regex.free;
  end;

CONST SYNTAX_LINK='#For the syntax of regular expressions see <a href="http://regexpstudio.com/TRegExpr/Help/RegExp_Syntax.html">the used library''s website.</a>';
INITIALIZATION
  mnh_funcs.registerRule(REGEX_NAMESPACE,'matches', @regexMatch_imp,'matches(searchString,regex);#returns true if string/-list searchString matches string/-list regex#If lists are given they must have equal sizes.'+SYNTAX_LINK);
  mnh_funcs.registerRule(REGEX_NAMESPACE,'matchComposite', @regexMatchComposite_imp,'matchComposite(searchString,regex);#returns a (list of) triplets: [match,position,length] for string/-list regex and searchString#If lists are given they must have equal sizes.'+SYNTAX_LINK);
  mnh_funcs.registerRule(REGEX_NAMESPACE,'split',@regexSplit_imp,'split(searchString,regex);#splits the string/-list searchString using string/-list regex#If lists are given they must have equal sizes.'+SYNTAX_LINK, true);
  mnh_funcs.registerRule(REGEX_NAMESPACE,'replace',@regexReplace_imp,'replace(searchString,regex,replaceString);#replaces all matching occurences of string/-list regex in string/-list searchString by string/-list replaceString#If lists are given they must have equal sizes.'+SYNTAX_LINK,true);

end.
