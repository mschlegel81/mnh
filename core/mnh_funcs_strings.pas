UNIT mnh_funcs_strings;
INTERFACE
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,myGenerics,myStringUtil,sysutils;
IMPLEMENTATION
{$MACRO ON}
FUNCTION length_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case params^.value(0)^.literalType of
        lt_string: result:=newIntLiteral(length(P_stringLiteral(params^.value(0))^.value));
        lt_stringList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
            P_listLiteral(result)^.appendInt(length(P_stringLiteral(P_listLiteral(params^.value(0))^.value(i))^.value));
        end;
        else raiseNotApplicableError('length',params,tokenLocation);
      end;
    end else raiseNotApplicableError('length',params,tokenLocation);
  end;

FUNCTION pos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION posInt(x,y:P_literal):P_intLiteral;
    begin
      result:=newIntLiteral(pos(P_stringLiteral(x)^.value,
                                P_stringLiteral(y)^.value)-1);
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       (params^.value(0)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(1)^.literalType in [lt_string,lt_stringList]) then begin
      if params^.value(0)^.literalType=lt_string then begin
        if params^.value(1)^.literalType=lt_string then begin
          result:=posInt(params^.value(0),
                         params^.value(1));
        end else begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(params^.value(1))^.size-1 do
            P_listLiteral(result)^.append(posInt(              params^.value(0),
                                                 P_listLiteral(params^.value(1))^.value(i)),false);
        end;
      end else begin
        if params^.value(1)^.literalType=lt_string then begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
            P_listLiteral(result)^.append(posInt(P_listLiteral(params^.value(0))^.value(i),
                                                               params^.value(1)           ),false);
        end else begin
          if P_listLiteral(params^.value(0))^.size=P_listLiteral(params^.value(1))^.size then begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
              P_listLiteral(result)^.append(posInt(P_listLiteral(params^.value(0))^.value(i),
                                                   P_listLiteral(params^.value(1))^.value(i)),false);
          end else raiseError('Incompatible list lengths for function pos.',tokenLocation)
        end;
      end;
    end else raiseNotApplicableError('pos',params,tokenLocation);
  end;

FUNCTION copy_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR anyList:boolean=false;
      allOkay:boolean=true;
      i1,i:longint;

  PROCEDURE checkLength(L:P_literal);
    VAR s:longint;
    begin
      s:=P_listLiteral(L)^.size;
      if not(anyList) then begin i1:=s; anyList:=true; end
                      else if    i1<>s then allOkay:=false;
    end;

  FUNCTION safeString(index:longint):ansistring;
    begin
      if params^.value(0)^.literalType=lt_string
        then result:=P_stringLiteral(              params^.value(0)               )^.value
        else result:=P_stringLiteral(P_listLiteral(params^.value(0))^.value(index))^.value;
    end;

  FUNCTION safeStart(index:longint):longint;
    begin
      if params^.value(1)^.literalType=lt_int
        then result:=P_intLiteral(              params^.value(1)               )^.value
        else result:=P_intLiteral(P_listLiteral(params^.value(1))^.value(index))^.value;
      inc(result);
    end;

  FUNCTION safeLen(index:longint):longint;
    begin
      if params^.value(2)^.literalType=lt_int
        then result:=P_intLiteral(              params^.value(2)               )^.value
        else result:=P_intLiteral(P_listLiteral(params^.value(2))^.value(index))^.value;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (params^.value(0)^.literalType in [lt_string,lt_stringList])
                                          and (params^.value(1)^.literalType in [lt_int   ,lt_intList])
                                          and (params^.value(2)^.literalType in [lt_int   ,lt_intList]) then begin
      anyList:=false;
      if params^.value(0)^.literalType=lt_stringList then checkLength(params^.value(0));
      if params^.value(1)^.literalType=lt_intList    then checkLength(params^.value(1));
      if params^.value(2)^.literalType=lt_intList    then checkLength(params^.value(2));
      if not(allOkay) then raiseNotApplicableError('copy',params,tokenLocation)
      else if not(anyList) then
        result:=newStringLiteral(copy(P_stringLiteral(params^.value(0))^.value,
                                      P_intLiteral   (params^.value(1))^.value+1,
                                      P_intLiteral   (params^.value(2))^.value))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do
          P_listLiteral(result)^.appendString(
              copy(safeString(i),
                   safeStart(i),
                   safeLen(i)));
      end;
    end else raiseNotApplicableError('copy',params,tokenLocation);
  end;

FUNCTION chars_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION chars_internal(CONST input:P_literal):P_listLiteral;
    VAR i:longint;
        txt:ansistring;
    begin
      result:=newListLiteral;
      txt:=P_stringLiteral(input)^.value;
      for i:=1 to length(txt) do result^.appendString(txt[i]);
    end;

  VAR i:longint;
  begin
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=chars_internal(params^.value(0));
    end else if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_stringList) then begin
      result:=newListLiteral;
      for i:=0 to P_listLiteral(params^.value(0))^.size-1 do P_listLiteral(result)^.append(chars_internal(P_listLiteral(params^.value(0))^.value(i)),false);
    end else if (params=nil) or (params^.size=0) then begin
      result:=newListLiteral;
      for i:=0 to 255 do P_listLiteral(result)^.appendString(chr(i));
    end else raiseNotApplicableError('chars',params,tokenLocation);
  end;

FUNCTION split_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR splitters:T_arrayOfString;
  PROCEDURE initSplitters;
    VAR i:longint;
    begin
      if params^.value(1)^.literalType=lt_string then begin
        setLength(splitters,1);
        splitters[0]:=P_stringLiteral(params^.value(1))^.value;
      end else begin
        setLength(splitters,P_listLiteral(params^.value(1))^.size);
        for i:=0 to length(splitters)-1 do
          splitters[i]:=P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value;
      end;
    end;

  FUNCTION splitOneString(CONST s:P_stringLiteral):P_listLiteral;
    PROCEDURE firstSplitterPos(CONST s:ansistring; OUT splitterStart,splitterEnd:longint);
      VAR i,p:longint;
      begin
        splitterStart:=0;
        for i:=0 to length(splitters)-1 do begin
          p:=pos(splitters[i],s);
          if (p>0) and ((splitterStart=0) or (p<splitterStart)) then begin
            splitterStart:=p;
            splitterEnd:=p+length(splitters[i]);
          end;
        end;
      end;

    VAR sp0,sp1:longint;
        rest:ansistring;
    begin
      firstSplitterPos(s^.value,sp0,sp1);
      if sp0<0 then exit(newOneElementListLiteral(s,true));
      result:=newListLiteral;
      rest:=s^.value;
      while sp0>0 do begin
        result^.appendString(copy(rest,1,sp0-1));
        rest:=copy(rest,sp1,length(rest));
        firstSplitterPos(rest,sp0,sp1);
      end;
      result^.appendString(rest);
    end;

  FUNCTION splitRecurse(CONST p:P_literal):P_literal;
    VAR i:longint;
    begin
      case p^.literalType of
        lt_string: result:=splitOneString(P_stringLiteral(p));
        lt_list,lt_stringList,lt_emptyList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(p)^.size-1 do if noErrors then
            P_listLiteral(result)^.append(splitRecurse(P_listLiteral(p)^.value(i)),false);
        end
       else result:=newErrorLiteralRaising('Cannot split non-string varables ',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2)
      and (params^.value(0)^.literalType in [lt_string,lt_stringList,lt_list,lt_emptyList])
      and (params^.value(1)^.literalType in [lt_string,lt_stringList,lt_emptyList]) then begin
      initSplitters;
      result:=splitRecurse(params^.value(0));
    end else raiseNotApplicableError('split',params,tokenLocation);
  end;

FUNCTION join_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION stringOfLit(CONST L:P_literal):ansistring;
    begin
      case L^.literalType of
        lt_boolean,
        lt_int,
        lt_real,
        lt_string,
        lt_expression: result:= P_scalarLiteral(L)^.stringForm;
        lt_list..lt_listWithError: result:=L^.toString;
        else result:='';
      end;
    end;

  VAR resTxt:ansistring='';
      joiner:ansistring='';
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and ((params^.size=1) or (params^.size=2) and (params^.value(1)^.literalType=lt_string)) and
       (params^.value(0)^.literalType in C_validListTypes) then begin

      if P_listLiteral(params^.value(0))^.size=0 then exit(newStringLiteral(''));
      if params^.size=2 then joiner:=P_stringLiteral(params^.value(1))^.value;
      resTxt:=stringOfLit(P_listLiteral(params^.value(0))^.value(0));
      for i:=1 to P_listLiteral(params^.value(0))^.size-1 do
        resTxt:=resTxt+joiner+stringOfLit(P_listLiteral(params^.value(0))^.value(i));
      result:=newStringLiteral(resTxt);
    end else raiseNotApplicableError('join',params,tokenLocation);
  end;

{$define STRINGLITERAL_ROUTINE:=
FUNCTION recurse(CONST x:P_literal):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    case x^.literalType of
      lt_string: result:=P_stringLiteral(x)^.CALL_MACRO;
      lt_list,lt_stringList,lt_emptyList:  begin
        result:=newListLiteral;
        for i:=0 to P_listLiteral(x)^.size-1 do if noErrors then
          P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false);
        if result^.literalType = lt_listWithError then begin
          disposeLiteral(result);
          result:=newErrorLiteral;
        end;
      end;
      else result:=newErrorLiteralRaising('Cannot apply '+ID_MACRO+' to literal of type '+C_typeString[x^.literalType],tokenLocation);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_string,lt_emptyList])
  then result:=recurse(params^.value(0))
  else raiseNotApplicableError(ID_MACRO,params,tokenLocation);
end}


FUNCTION trim_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=trim}
{$define ID_MACRO:='trim'}
STRINGLITERAL_ROUTINE;

FUNCTION trimLeft_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=trimLeft}
{$define ID_MACRO:='trimLeft'}
STRINGLITERAL_ROUTINE;

FUNCTION trimRight_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=trimRight}
{$define ID_MACRO:='trimRight'}
STRINGLITERAL_ROUTINE;

FUNCTION upper_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=upper}
{$define ID_MACRO:='upper'}
STRINGLITERAL_ROUTINE;

FUNCTION lower_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=lower}
{$define ID_MACRO:='lower'}
STRINGLITERAL_ROUTINE;

FUNCTION unbrace_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=unbrace}
{$define ID_MACRO:='unbrace'}
STRINGLITERAL_ROUTINE;

FUNCTION escape_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=escape}
{$define ID_MACRO:='escape'}
STRINGLITERAL_ROUTINE;
{$undef STRINGLITERAL_ROUTINE}

FUNCTION replace_one_or_all(CONST params:P_listLiteral; CONST all:boolean):P_literal;
  VAR lookFor,replaceBy:T_arrayOfString;
      i:longint;

  PROCEDURE initArrays;
    VAR L:P_literal;
        i:longint;
    PROCEDURE elongate(VAR list:T_arrayOfString);
      begin
        if length(list)=0 then begin
          setLength(list,1);
          list[0]:='';
        end else begin
          setLength(list,length(list)+1);
          list[length(list)-1]:=list[length(list)-2];
        end;
      end;

    begin
      L:=params^.value(1);
      if L^.literalType=lt_string then begin
        setLength(lookFor,1);
        lookFor[0]:=P_stringLiteral(L)^.value;
      end else begin
        setLength(lookFor,P_listLiteral(L)^.size);
        for i:=0 to length(lookFor)-1 do
          lookFor[i]:=P_stringLiteral(P_listLiteral(L)^.value(i))^.value;
      end;
      L:=params^.value(2);
      if L^.literalType=lt_string then begin
        setLength(replaceBy,1);
        replaceBy[0]:=P_stringLiteral(L)^.value;
      end else begin
        setLength(replaceBy,P_listLiteral(L)^.size);
        for i:=0 to length(replaceBy)-1 do
          replaceBy[i]:=P_stringLiteral(P_listLiteral(L)^.value(i))^.value;
      end;
      while length(replaceBy)<length(lookFor) do elongate(replaceBy);
      while length(lookFor)<length(replaceBy) do elongate(lookFor);
    end;

  FUNCTION modify(CONST original:ansistring):ansistring;
    VAR i:longint;
        dummy:boolean;
    begin
      result:=original;
      if all then for i:=0 to length(lookFor)-1 do result:=replaceRecursively(result,lookFor[i],replaceBy[i],dummy)
             else for i:=0 to length(lookFor)-1 do result:=replaceOne        (result,lookFor[i],replaceBy[i]);
    end;

  begin
    initArrays;
    if params^.value(0)^.literalType=lt_string then result:=newStringLiteral(modify(P_stringLiteral(params^.value(0))^.value))
    else begin
      result:=newListLiteral;
      for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
        P_listLiteral(result)^.appendString(modify(P_stringLiteral(P_listLiteral(params^.value(0))^.value(i))^.value));
    end;
    setLength(lookFor,0);
    setLength(replaceBy,0);
  end;

FUNCTION replaceOne_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (params^.value(0)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(1)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(2)^.literalType in [lt_string,lt_stringList]) then begin
      result:=replace_one_or_all(params,false);
    end else raiseNotApplicableError('replaceOne',params,tokenLocation);
  end;

FUNCTION replace_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (params^.value(0)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(1)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(2)^.literalType in [lt_string,lt_stringList]) then begin
      result:=replace_one_or_all(params,true);
    end else raiseNotApplicableError('replace',params,tokenLocation);
  end;

FUNCTION repeat_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR sub,res:ansistring;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       (params^.value(0)^.literalType = lt_string) and
       (params^.value(1)^.literalType = lt_int) then begin
      res:='';
      sub:=P_stringLiteral(params^.value(0))^.value;
      for i:=1 to P_intLiteral(params^.value(1))^.value do res:=res+sub;
      result:=newStringLiteral(res);
    end else raiseNotApplicableError('repeat',params,tokenLocation);
  end;

FUNCTION tokenSplit_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR stringToSplit:ansistring;
      i0,i1:longint;

  PROCEDURE stepToken;
    begin
      P_listLiteral(result)^.appendString(copy(stringToSplit,i0,i1-i0));
      i0:=i1;
    end;

  VAR doubleQuoteString:boolean=false;
      singleQuoteString:boolean=false;
      escapeStringDelimiter:boolean=false;
      curlyBracketsDelimitOneToken:boolean=false;
      cStyleComments:boolean=false;
      dollarVariables:boolean=false;
      t:T_tokenType;

  PROCEDURE setLanguage(name:string);
    begin
      if trim(uppercase(name))='MNH' then begin
        doubleQuoteString:=true;
        singleQuoteString:=true;
        escapeStringDelimiter:=true;
        curlyBracketsDelimitOneToken:=true;
        cStyleComments:=true;
        dollarVariables:=true;
      end else if trim(uppercase(name))='JAVA' then begin
        doubleQuoteString:=true;
        singleQuoteString:=true;
        escapeStringDelimiter:=true;
        curlyBracketsDelimitOneToken:=false;
        cStyleComments:=true;
      end else if trim(uppercase(name))='PASCAL' then begin
        doubleQuoteString:=false;
        singleQuoteString:=true;
        escapeStringDelimiter:=false;
        curlyBracketsDelimitOneToken:=true;
        cStyleComments:=true;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      if (params^.value(1)^.literalType=lt_string)
      then setLanguage(P_stringLiteral(params^.value(1))^.value)
      else begin
        raiseNotApplicableError('tokenSplit',params,tokenLocation);
        exit(nil);
      end;
    end;

    if (params<>nil) and (params^.size>=1) and
       (params^.value(0)^.literalType=lt_string) then begin
      stringToSplit:=P_stringLiteral(params^.value(0))^.value;

      result:=newListLiteral;
      i0:=1;
      while i0<=length(stringToSplit) do begin
        if stringToSplit[i0] in [' ',C_lineBreakChar,C_carriageReturnChar,C_tabChar] then begin //whitespace
          i1:=i0;
          while (i1<=length(stringToSplit)) and (stringToSplit[i1] in [' ',C_lineBreakChar,C_carriageReturnChar,C_tabChar]) do inc(i1);
        end else if (stringToSplit[i0]='''') and singleQuoteString or
                    (stringToSplit[i0]='"') and doubleQuoteString then begin
          if escapeStringDelimiter then begin
            unescapeString(copy(stringToSplit,i0,length(stringToSplit)-i0+1),1,i1);
            if i1<=0 then i1:=i0+1
                     else i1:=i0+i1;
          end else begin
            i1:=i0+1;
            while (i1<=length(stringToSplit)) and (stringToSplit[i1]<>stringToSplit[i0]) do inc(i1);
            inc(i1);
          end;
        end else if (stringToSplit[i0]='{') and curlyBracketsDelimitOneToken then begin
          i1:=i0+1;
          while (i1<=length(stringToSplit)) and (stringToSplit[i1]<>'}') do inc(i1);
          inc(i1);
        end else if (copy(stringToSplit,i0,2)='//') and cStyleComments then begin
          i1:=i0+1;
          while (i1<=length(stringToSplit)) and not(stringToSplit[i1] in [C_lineBreakChar,C_carriageReturnChar]) do inc(i1);
        end else if (stringToSplit[i0] in ['a'..'z','A'..'Z']) or (stringToSplit[i0]='$') and dollarVariables then begin
          i1:=i0+1;
          while (i1<=length(stringToSplit)) and (stringToSplit[i1] in ['a'..'z','A'..'Z','_','0'..'9']) do inc(i1);
        end else if stringToSplit[i0] in ['0'..'9'] then begin //numbers
          parseNumber(copy(stringToSplit,i0,length(stringToSplit)-i0+1),1,false,i1);
          if i1<=0 then i1:=i0
                   else i1:=i0+i1;
        end else begin
          //symbols, etc.
          t:=tt_literal;
          i1:=i0;
          for t:=tt_literal to tt_EOL do begin
            if (C_tokenString[t]<>'') and
               (copy(stringToSplit,i0,length(C_tokenString[t]))=C_tokenString[t]) and
               (i0+length(C_tokenString[t])>i1)
            then i1:=i0+length(C_tokenString[t]);
          end;
          if i1=i0 then i1:=i0+1;
        end;
        stepToken;
      end;

    end else raiseNotApplicableError('tokenSplit',params,tokenLocation);
  end;


INITIALIZATION
  //Functions on Strings:
  registerRule(STRINGS_NAMESPACE,'length',@length_imp,'length(S:string);#Returns the number of characters in string S');
  registerRule(STRINGS_NAMESPACE,'pos',@pos_imp,'pos(subString,searchInString);#Returns the index of the first occurence of subString in searchInString or -1 if there is none');
  registerRule(STRINGS_NAMESPACE,'copy',@copy_imp,'copy(S,start,length):#Returns the substring of S starting at index start and having specified length');
  registerRule(STRINGS_NAMESPACE,'chars',@chars_imp,'chars(S);#Returns the characters in S as a list#chars;#Returns all ANSI characters in natural ordering');
  registerRule(STRINGS_NAMESPACE,'split',@split_imp,'split(S:string,splitter:string);#Returns a list of strings obtained by splitting S at the specified splitters#The splitters themselves are not contained in the result');
  registerRule(STRINGS_NAMESPACE,'join',@join_impl,'join(L:list);#Returns a string-concatenation of all elements in L#join(L:list,joiner:string);#Returns a string-concatenation of all elements, with joiner between.');
  registerRule(STRINGS_NAMESPACE,'trim',@trim_imp,'trim(S:string);#Returns string S without leading or trailing spaces');
  registerRule(STRINGS_NAMESPACE,'trimLeft',@trimLeft_imp,'trimLeft(S:string);#Returns string S without leading spaces');
  registerRule(STRINGS_NAMESPACE,'trimRight',@trimRight_imp,'trimRight(S:string);#Returns string S without trailing spaces');
  registerRule(STRINGS_NAMESPACE,'upper',@upper_imp,'upper(S:string);#Returns an uppercase representation of S');
  registerRule(STRINGS_NAMESPACE,'lower',@lower_imp,'lower(S:string);#Returns an lowercase representation of S');
  registerRule(STRINGS_NAMESPACE,'unbrace',@unbrace_imp,'unbrace(S:string);#Returns an unbraced representation of S');
  registerRule(STRINGS_NAMESPACE,'escape',@escape_imp,'escape(S:string);#Returns an escaped representation of S');
  registerRule(STRINGS_NAMESPACE,'replaceOne',@replaceOne_impl,'replaceOne(source:string,lookFor,replaceBy);#Replaces the first occurences of lookFor in source by replaceBy#lookFor and replaceBy may be of type string or stringList');
  registerRule(STRINGS_NAMESPACE,'replace',@replace_impl,'replace(source:string,lookFor,replaceBy);#Recursively replaces all occurences of lookFor in source by replaceBy#lookFor and replaceBy may be of type string or stringList');
  registerRule(STRINGS_NAMESPACE,'repeat',@repeat_impl,'repeat(s:string,k:int);#Returns a string containing s repeated k times');
  registerRule(STRINGS_NAMESPACE,'tokenSplit',@tokenSplit_impl,'tokenSplit(S:string);#tokenSplit(S:string,language:string);#Returns a list of strings from S for a given language#Languages: <code>MNH, Pascal, Java</code>');
end.
