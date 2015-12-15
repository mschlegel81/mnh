UNIT mnh_funcs_regex;
INTERFACE
USES RegExpr,Classes,mnh_litVar,mnh_funcs,mnh_constants,mnh_tokLoc,sysutils,mnh_out_adapters,mnh_contexts;

IMPLEMENTATION
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

FUNCTION regexMatch_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION regexMatches(CONST trip:T_triplet):boolean;
    VAR regex:TRegExpr;
    begin
      regex:=TRegExpr.create;
      regex.expression:=trip.x;
      regex.inputString:=trip.y;
      try
        result:=regex.Exec(trip.y);
      except
        on e:Exception do begin
           context.adapters^.raiseCustomMessage(mt_el5_systemError,e.message,tokenLocation);
        end;
      end;
      regex.free;
    end;
  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      i1:=listSize(params^.value(0),params^.value(1),nil);
      if i1<0 then exit(nil)
      else if i1=0 then result:=newBoolLiteral(regexMatches(triplet(params^.value(1),params^.value(0),nil,0)))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(regexMatches(triplet(params^.value(1),params^.value(0),nil,i)));
      end;
    end;
  end;

FUNCTION regexMatchComposite_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION regexMatchComposite(CONST trip:T_triplet):P_listLiteral;
    VAR regex:TRegExpr;
        i:longint;
    begin
      regex:=TRegExpr.create;
      regex.expression:=trip.x;
      regex.inputString:=trip.y;
      result:=newListLiteral;
      try
        if regex.Exec(trip.y) then repeat
          for i:=0 to regex.SubExprMatchCount do begin
            result^.append(
              newListLiteral^.
              appendString(regex.Match   [i])^.
              appendInt   (regex.MatchPos[i])^.
              appendInt   (regex.MatchLen[i]),false,context.adapters^);
          end;
        until not(regex.ExecNext);
      except
        on e:Exception do begin
          context.adapters^.raiseCustomMessage(mt_el5_systemError,e.message,tokenLocation);
        end;
      end;
      regex.free;
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      i1:=listSize(params^.value(0),params^.value(1),nil);
      if i1<0 then exit(nil)
      else if i1=0 then result:=regexMatchComposite(triplet(params^.value(1),params^.value(0),nil,0))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do P_listLiteral(result)^.append(regexMatchComposite(triplet(params^.value(1),params^.value(0),nil,i)),false,context.adapters^);
      end;
    end;
  end;

FUNCTION regexSplit_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION regexSplit(CONST trip:T_triplet):P_listLiteral;
    VAR regex:TRegExpr;
        i:longint;
        pieces : TStrings;
    begin
      regex:=TRegExpr.create;
      regex.expression:=trip.x;
      pieces:=TStringList.create;
      try
        regex.split(trip.y,pieces);
      except
        on e:Exception do begin
          context.adapters^.raiseCustomMessage(mt_el5_systemError,e.message,tokenLocation);
        end;
      end;
      regex.free;
      result:=newListLiteral;
      for i:=0 to pieces.count-1 do result^.appendString(pieces[i]);
      pieces.free;
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      i1:=listSize(params^.value(0),params^.value(1),nil);
      if i1<0 then exit(nil)
      else if i1=0 then result:=regexSplit(triplet(params^.value(1),params^.value(0),nil,0))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do P_listLiteral(result)^.append(regexSplit(triplet(params^.value(1),params^.value(0),nil,i)),false,context.adapters^);
      end;
    end;
  end;

FUNCTION regexReplace_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION regexReplace(CONST trip:T_triplet):ansistring;
    VAR regex:TRegExpr;
    begin
      regex:=TRegExpr.create;
      regex.expression:=trip.x;
      try
        result:=regex.Replace(trip.y,trip.z,false);
      except
        on e:Exception do begin
          context.adapters^.raiseCustomMessage(mt_el5_systemError,e.message,tokenLocation);
        end;
      end;
      regex.free;
    end;

  VAR i,i1:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) then begin
      i1:=listSize(params^.value(0),params^.value(1),params^.value(2));
      if i1<0 then exit(nil)
      else if i1=0 then result:=newStringLiteral(regexReplace(triplet(params^.value(1),params^.value(0),params^.value(2),0)))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do P_listLiteral(result)^.appendString(regexReplace(triplet(params^.value(1),params^.value(0),params^.value(2),i)));
      end;
    end;
  end;
CONST SYNTAX_LINK='#For the syntax of regular expressions refer to <a href="http://regexpstudio.com/TRegExpr/Help/RegExp_Syntax.html">the used library''s website.</a>';
INITIALIZATION
  mnh_funcs.registerRule(REGEX_NAMESPACE,'matches', @regexMatch_imp,'matches(searchString,regex);#returns true if string/-list searchString matches string/-list regex#If lists are given they must have equal sizes.'+SYNTAX_LINK);
  mnh_funcs.registerRule(REGEX_NAMESPACE,'matchComposite', @regexMatchComposite_imp,'matchComposite(searchString,regex);#returns a (list of) triplets: [match,position,length] for string/-list regex and searchString#If lists are given they must have equal sizes.'+SYNTAX_LINK);
  mnh_funcs.registerRule(REGEX_NAMESPACE,'split',@regexSplit_imp,'split(searchString,regex);#splits the string/-list searchString using string/-list regex#If lists are given they must have equal sizes.'+SYNTAX_LINK,true);
  mnh_funcs.registerRule(REGEX_NAMESPACE,'replace',@regexReplace_imp,'replace(searchString,regex,replaceString);#replaces all matching occurences of string/-list regex in string/-list searchString by string/-list replaceString#If lists are given they must have equal sizes.'+SYNTAX_LINK,true);

end.
