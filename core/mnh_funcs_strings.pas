UNIT mnh_funcs_strings;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     LazUTF8,base64,LConvEncoding,
     synacode,
     diff,
     myGenerics,myStringUtil,
     mnh_basicTypes,
     mnh_constants,
     mnh_out_adapters,
     mnh_litVar,mnh_funcs,mnh_contexts;
IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION pos_imp intFuncSignature;
  FUNCTION posInt(x,y:P_literal):P_intLiteral;
    begin
      if P_stringLiteral(x)^.getEncoding=se_utf8
      then result:=newIntLiteral(int64(UTF8Pos(P_stringLiteral(x)^.value,
                                               P_stringLiteral(y)^.value))-int64(1))
      else result:=newIntLiteral(int64(    pos(P_stringLiteral(x)^.value,
                                               P_stringLiteral(y)^.value))-int64(1));
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType in [lt_string,lt_stringList,lt_emptyList]) and
       (arg1^.literalType in [lt_string,lt_stringList,lt_emptyList]) then begin
      if arg0^.literalType=lt_string then begin
        if arg1^.literalType=lt_string then begin
          result:=posInt(arg0,
                         arg1);
        end else begin
          result:=newListLiteral;
          for i:=0 to list1^.size-1 do
            listResult^.append(posInt(arg0,list1^.value[i]),false);
        end;
      end else begin
        if arg1^.literalType=lt_string then begin
          result:=newListLiteral;
          for i:=0 to list0^.size-1 do
            listResult^.append(posInt(list0^.value[i],
                                                               arg1           ),false);
        end else begin
          if list0^.size=list1^.size then begin
            result:=newListLiteral;
            for i:=0 to list0^.size-1 do
              listResult^.append(posInt(list0^.value[i],
                                        list1^.value[i]),false);
          end else context.adapters^.raiseError('Incompatible list lengths for function pos.',tokenLocation)
        end;
      end;
    end;
  end;

FUNCTION copy_imp intFuncSignature;
  VAR anyList:boolean=false;
      allOkay:boolean=true;
      i1:longint=0;
      i:longint;

  PROCEDURE checkLength(CONST L:P_literal); inline;
    VAR s:longint;
    begin
      s:=P_listLiteral(L)^.size;
      if not(anyList) then begin i1:=s; anyList:=true; end
                      else if    i1<>s then allOkay:=false;
    end;

  FUNCTION safeString(CONST index:longint):P_stringLiteral; inline;
    begin
      if arg0^.literalType=lt_string
        then result:=str0
        else result:=P_stringLiteral(list0^.value[index]);
    end;

  FUNCTION safeStart(CONST index:longint):longint; inline;
    begin
      if arg1^.literalType=lt_int
        then result:=int1^.value
        else result:=P_intLiteral(list1^.value[index])^.value;
      inc(result);
    end;

  FUNCTION safeLen(CONST index:longint):longint; inline;
    begin
      if arg2^.literalType=lt_int
        then result:=int2^.value
        else result:=P_intLiteral(list2^.value[index])^.value;
    end;

  FUNCTION myCopy(CONST s:P_stringLiteral; CONST start,len:int64):string; inline;
    begin
      if s^.getEncoding=se_utf8 then result:=UTF8Copy(s^.value,start,len)
                                else result:=    copy(s^.value,start,len);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (arg0^.literalType in [lt_string,lt_stringList,lt_emptyList])
                                          and (arg1^.literalType in [lt_int   ,lt_intList   ,lt_emptyList])
                                          and (arg2^.literalType in [lt_int   ,lt_intList   ,lt_emptyList]) then begin
      anyList:=false;
      if arg0^.literalType in [lt_stringList,lt_emptyList] then checkLength(arg0);
      if arg1^.literalType in [lt_intList   ,lt_emptyList] then checkLength(arg1);
      if arg2^.literalType in [lt_intList   ,lt_emptyList] then checkLength(arg2);
      if not(allOkay) then exit(nil)
      else if not(anyList) then
        result:=newStringLiteral(myCopy(str0,
                           P_intLiteral(arg1)^.value+1,
                           P_intLiteral(arg2)^.value))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do
          listResult^.appendString(
                myCopy(safeString(i),
                       safeStart(i),
                       safeLen(i)));
      end;
    end;
  end;

FUNCTION bytes_internal(CONST input:P_literal):P_listLiteral;
  VAR txt:ansistring;
      c:char;
  begin
    txt:=P_stringLiteral(input)^.value;
    result:=newListLiteral(length(txt));
    for c in txt do result^.appendString(c);
  end;

FUNCTION chars_internal(CONST input:P_literal):P_listLiteral;
  VAR charIndex,
      byteIndex:longint;
      i:longint;
      txt:ansistring;
      sub:ansistring;
  begin
    if not(P_stringLiteral(input)^.getEncoding=se_utf8) then exit(bytes_internal(input));
    result:=newListLiteral;
    txt:=P_stringLiteral(input)^.value;
    byteIndex:=1;
    for charIndex:=1 to UTF8Length(txt) do begin
      sub:='';
      for i:=0 to UTF8CharacterLength(@txt[byteIndex])-1 do begin
        sub:=sub+txt[byteIndex];
        inc(byteIndex)
      end;
      result^.appendString(sub);
    end;
  end;

FUNCTION chars_imp intFuncSignature;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=chars_internal(arg0);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_stringList,lt_emptyList]) then begin
      result:=newListLiteral;
      for i:=0 to list0^.size-1 do listResult^.append(chars_internal(list0^.value[i]),false);
    end else if (params=nil) or (params^.size=0) then begin
      result:=newListLiteral;
      for i:=0 to 255 do listResult^.appendString(chr(i));
    end;
  end;

FUNCTION charSet_imp intFuncSignature;
  FUNCTION charset_internal(CONST input:P_literal):P_setLiteral;
    VAR charIndex,
        byteIndex:longint;
        i:longint;
        txt:ansistring;
        sub:ansistring;
        charSetUtf8:T_setOfString;
        byteSet:T_charSet=[];
        c:char;
    begin
      if P_stringLiteral(input)^.getEncoding=se_utf8 then begin
        charSetUtf8.create;
        txt:=P_stringLiteral(input)^.value;
        byteIndex:=1;
        for charIndex:=1 to UTF8Length(txt) do begin
          sub:='';
          for i:=0 to UTF8CharacterLength(@txt[byteIndex])-1 do begin
            sub:=sub+txt[byteIndex];
            inc(byteIndex)
          end;
          charSetUtf8.put(sub);
        end;
        result:=newSetLiteral;
        for sub in charSetUtf8.values do result^.appendString(sub);
        charSetUtf8.destroy;
      end else begin
        txt:=P_stringLiteral(input)^.value;
        for c in txt do include(byteSet,c);
        result:=newSetLiteral;
        for c in byteSet do result^.appendString(c);
      end;
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=charset_internal(arg0);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_stringList,lt_emptyList]) then begin
      result:=newListLiteral;
      for i:=0 to list0^.size-1 do listResult^.append(charset_internal(list0^.value[i]),false);
    end;
  end;

FUNCTION bytes_imp intFuncSignature;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=bytes_internal(arg0);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_stringList,lt_emptyList]) then begin
      result:=newListLiteral;
      for i:=0 to list0^.size-1 do listResult^.append(bytes_internal(list0^.value[i]),false);
    end;
  end;

FUNCTION split_imp intFuncSignature;
  VAR splitters:T_arrayOfString;
  PROCEDURE initSplitters;
    VAR i:longint;
    begin
      if arg1^.literalType=lt_string then begin
        setLength(splitters,1);
        splitters[0]:=str1^.value;
      end else begin
        setLength(splitters,list1^.size);
        for i:=0 to length(splitters)-1 do
          splitters[i]:=P_stringLiteral(list1^.value[i])^.value;
      end;
    end;

  FUNCTION splitOneString(CONST s:P_stringLiteral):P_collectionLiteral;
    VAR part:string;
    begin
      result:=newListLiteral(1);
      for part in split(s^.value,splitters) do result^.appendString(part);
    end;

  FUNCTION splitRecurse(CONST p:P_literal):P_literal;
    VAR iter:T_arrayOfLiteral;
        sub:P_literal;
    begin
      result:=nil;
      case p^.literalType of
        lt_string: result:=splitOneString(P_stringLiteral(p));
        lt_list,lt_stringList,lt_emptyList,
        lt_set ,lt_stringSet ,lt_emptySet: begin
          result:=P_collectionLiteral(p)^.newOfSameType(true);
          iter:=P_collectionLiteral(p)^.iteratableList;
          for sub in iter do collResult^.append(splitRecurse(sub),false);
          disposeLiteral(iter);
        end;
        else raiseNotApplicableError('split',p,tokenLocation,context.adapters^);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2)
      and (arg0^.literalType in [lt_string,lt_stringList,lt_list,lt_emptyList])
      and (arg1^.literalType in [lt_string,lt_stringList,lt_emptyList]) then begin
      initSplitters;
      result:=splitRecurse(arg0);
    end;
  end;

FUNCTION join_impl intFuncSignature;
  FUNCTION stringOfLit(CONST L:P_literal):ansistring;
    begin
      case L^.literalType of
        lt_boolean,
        lt_int,
        lt_real,
        lt_string,
        lt_expression: result:= P_scalarLiteral(L)^.stringForm;
        lt_list..lt_emptyMap: result:=L^.toString;
        else result:='';
      end;
    end;

  VAR resTxt:ansistring='';
      joiner:ansistring='';
      i:longint;
      iter:T_arrayOfLiteral;
  begin
    result:=nil;
    if (params<>nil) and ((params^.size=1) or (params^.size=2) and (arg1^.literalType=lt_string)) then begin
      if params^.size=2 then joiner:=str1^.value;
      if (arg0^.literalType in C_listTypes+C_setTypes) then begin
        if collection0^.size=0 then exit(newStringLiteral(''));
        iter:=collection0^.iteratableList;
        resTxt:=stringOfLit(iter[0]);
        for i:=1 to length(iter)-1 do resTxt:=resTxt+joiner+stringOfLit(iter[i]);
        disposeLiteral(iter);
        result:=newStringLiteral(resTxt);
      end else if (arg0^.literalType in C_scalarTypes) then
        result:=newStringLiteral(stringOfLit(arg0));
    end;
  end;

{$define STRINGLITERAL_ROUTINE:=
FUNCTION recurse(CONST x:P_literal):P_literal;
  VAR iter:T_arrayOfLiteral;
      sub:P_literal;
  begin
    result:=nil;
    case x^.literalType of
      lt_string: result:=P_stringLiteral(x)^.CALL_MACRO;
      lt_list,lt_stringList,lt_emptyList,
      lt_set ,lt_stringSet ,lt_emptySet :  begin
        result:=P_collectionLiteral(x)^.newOfSameType(true);
        iter  :=P_collectionLiteral(x)^.iteratableList;
        for sub in iter do collResult^.append(recurse(sub),false);
        disposeLiteral(iter);
      end;
      else raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_list,lt_stringList,lt_string,lt_emptyList,lt_set,lt_stringSet,lt_emptySet])
  then result:=recurse(arg0);
end}

FUNCTION trim_imp      intFuncSignature; {$define CALL_MACRO:=trim}     {$define ID_MACRO:='trim'}     STRINGLITERAL_ROUTINE;
FUNCTION trimLeft_imp  intFuncSignature; {$define CALL_MACRO:=trimLeft} {$define ID_MACRO:='trimLeft'} STRINGLITERAL_ROUTINE;
FUNCTION trimRight_imp intFuncSignature; {$define CALL_MACRO:=trimRight}{$define ID_MACRO:='trimRight'}STRINGLITERAL_ROUTINE;
FUNCTION upper_imp     intFuncSignature; {$define CALL_MACRO:=upper}    {$define ID_MACRO:='upper'}    STRINGLITERAL_ROUTINE;
FUNCTION lower_imp     intFuncSignature; {$define CALL_MACRO:=lower}    {$define ID_MACRO:='lower'}    STRINGLITERAL_ROUTINE;
FUNCTION unbrace_imp   intFuncSignature; {$define CALL_MACRO:=unbrace}  {$define ID_MACRO:='unbrace'}  STRINGLITERAL_ROUTINE;
FUNCTION escape_imp    intFuncSignature; {$define CALL_MACRO:=escape}   {$define ID_MACRO:='escape'}   STRINGLITERAL_ROUTINE;
{$undef STRINGLITERAL_ROUTINE}

FUNCTION escapePascal_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then result:=newStringLiteral(escapeString(str0^.value,es_strictPascalStyle));
  end;

FUNCTION escapeJava_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then result:=newStringLiteral(escapeString(str0^.value,es_javaStyle));
  end;

FUNCTION replace_one_or_all(CONST params:P_listLiteral; CONST all:boolean):P_literal;
  VAR lookFor,replaceBy:T_arrayOfString;
      i:longint;

  PROCEDURE initArrays;
    VAR L:P_literal;
        i:longint;
        iter:T_arrayOfLiteral;

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
      L:=arg1;
      if L^.literalType=lt_string then begin
        setLength(lookFor,1);
        lookFor[0]:=P_stringLiteral(L)^.value;
      end else begin
        iter:=P_compoundLiteral(L)^.iteratableList;
        setLength(lookFor,length(iter));
        for i:=0 to length(lookFor)-1 do lookFor[i]:=P_stringLiteral(iter[i])^.value;
        disposeLiteral(iter);
      end;
      L:=arg2;
      if L^.literalType=lt_string then begin
        setLength(replaceBy,1);
        replaceBy[0]:=P_stringLiteral(L)^.value;
      end else begin
        iter:=P_compoundLiteral(L)^.iteratableList;
        setLength(replaceBy,length(iter));
        for i:=0 to length(replaceBy)-1 do replaceBy[i]:=P_stringLiteral(iter[i])^.value;
        disposeLiteral(iter);
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

  VAR iter:T_arrayOfLiteral;
      sub:P_literal;
  begin
    if not((params<>nil) and (params^.size=3) and
       (arg0^.literalType in [lt_string,lt_stringList,lt_emptyList,lt_stringSet,lt_emptySet]) and
       (arg1^.literalType in [lt_string,lt_stringList,lt_emptyList,lt_stringSet,lt_emptySet]) and
       (arg2^.literalType in [lt_string,lt_stringList,lt_emptyList,lt_stringSet,lt_emptySet])) then exit(nil);
    i:=0;
    if (arg0^.literalType in C_setTypes) then inc(i);
    if (arg1^.literalType in C_setTypes) then inc(i);
    if (arg2^.literalType in C_setTypes) then inc(i);
    if i>1 then exit(nil);

    initArrays;
    if arg0^.literalType=lt_string then result:=newStringLiteral(modify(str0^.value))
    else begin
      result:=collection0^.newOfSameType(true);
      iter:=collection0^.iteratableList;
      for sub in iter do listResult^.appendString(modify(P_stringLiteral(sub)^.value));
      disposeLiteral(iter);
    end;
    setLength(lookFor,0);
    setLength(replaceBy,0);
  end;

FUNCTION replaceOne_impl intFuncSignature; begin result:=replace_one_or_all(params,false); end;
FUNCTION replace_impl    intFuncSignature; begin result:=replace_one_or_all(params,true);  end;

FUNCTION repeat_impl intFuncSignature;
  VAR sub,res:ansistring;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType = lt_string) and
       (arg1^.literalType = lt_int) then begin
      res:='';
      sub:=str0^.value;
      for i:=1 to int1^.value do res:=res+sub;
      result:=newStringLiteral(res);
    end;
  end;

FUNCTION clean_impl intFuncSignature; {input,whitelist,instead,joinSuccessiveChars}
  VAR asciiWhitelist:T_charSet=[];
      utf8WhiteList:T_setOfString;
      instead:ansistring;
      insteadC:char;
      keepCharCount:boolean=true;

  FUNCTION innerClean(CONST input:string):ansistring;
    VAR i,byteIndex,charIndex:longint;
        c:char;
        cUtf8:string[6];
        last:(WHITE,BLACK,GREY)=GREY;
        thisWhite:boolean;
    begin
      if isAsciiEncoded(input) then begin
        if (length(instead)=1) and keepCharCount then begin
          setLength(result,length(input));
          for i:=1 to length(input) do
            if input[i] in asciiWhitelist
            then result[i]:=input[i]
            else result[i]:=insteadC;
        end else begin
          result:='';
          for c in input do begin
            thisWhite:= c in asciiWhitelist;
            if thisWhite then begin
              result:=result+c;
              last:=WHITE;
            end else begin
              if (last<>BLACK) or keepCharCount then result:=result+instead;
              last:=BLACK;
            end;
          end;
        end;
      end else begin
        result:='';
        byteIndex:=1;
        for charIndex:=1 to UTF8Length(input) do begin
          cUtf8:='';
          for i:=0 to UTF8CharacterLength(@input[byteIndex])-1 do begin
            cUtf8:=cUtf8+input[byteIndex];
            inc(byteIndex)
          end;
          if length(cUtf8)=1
          then thisWhite:=(cUtf8[1] in asciiWhitelist)
          else thisWhite:=utf8WhiteList.contains(cUtf8);
          if thisWhite then begin
            result:=result+cUtf8;
            last:=WHITE;
          end else begin
            if (last<>BLACK) or keepCharCount then result:=result+instead;
            last:=BLACK;
          end;
        end;
      end;
    end;

  VAR iter:T_arrayOfLiteral;
      l   :P_literal;
      s   :ansistring;
      k   :longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=3) and (params^.size<=4) and
       (arg0^.literalType in [lt_string,lt_stringList,lt_emptyList,lt_stringSet,lt_emptySet]) and
       (arg1^.literalType in [lt_stringSet,lt_stringList]) and
       (arg2^.literalType=lt_string) and
       ((params^.size=3) or (arg3^.literalType=lt_boolean)) then begin
      utf8WhiteList.create;

      iter:=compound1^.iteratableList;
      for l in iter do begin
        s:=P_stringLiteral(l)^.value;
        if length(s)=1 then include(asciiWhitelist,s[1]);
        utf8WhiteList.put(s);
      end;
      disposeLiteral(iter);

      instead:=str2^.value;
      if length(instead)=1 then insteadC:=instead[1];

      keepCharCount:=keepCharCount and not((params^.size>3) and bool3^.value);

      case arg0^.literalType of
        lt_string: result:=newStringLiteral(innerClean(str0^.value));
        lt_stringList,lt_emptyList: begin
          result:=newListLiteral(list0^.size);
          for k:=0 to list0^.size-1 do listResult^.appendString(innerClean(P_stringLiteral(list0^.value[k])^.value));
        end;
        lt_stringSet,lt_emptySet: begin
          result:=newSetLiteral;
          iter:=set0^.iteratableList;
          for l in iter do setResult^.appendString(innerClean(P_stringLiteral(l)^.value));
          disposeLiteral(iter);
        end;
      end;

      utf8WhiteList.destroy;
    end;
  end;

FUNCTION reverseString_impl intFuncSignature;
  FUNCTION rev(CONST input:P_literal):ansistring;
    VAR charIndex,
        byteIndex:longint;
        i:longint;
        txt:ansistring;
        sub:ansistring;
    begin
      result:='';
      txt:=P_stringLiteral(input)^.value;
      if P_stringLiteral(input)^.getEncoding=se_utf8 then begin
        byteIndex:=1;
        for charIndex:=1 to UTF8Length(txt) do begin
          sub:='';
          for i:=0 to UTF8CharacterLength(@txt[byteIndex])-1 do begin
            sub:=sub+txt[byteIndex];
            inc(byteIndex)
          end;
          result:=sub+result;
        end;
      end else begin
        i:=length(txt);
        setLength(result,i);
        for byteIndex:=1 to i do result[byteIndex]:=txt[i+1-byteIndex];
      end;
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_string: result:=newStringLiteral(rev(arg0));
      lt_stringList,lt_emptyList: begin
        result:=newListLiteral;
        for i:=0 to list0^.size-1 do
          listResult^.appendString(rev(list0^.value[i]));
      end;
    end;
  end;

FUNCTION prepareDiff(CONST A,B:P_literal; OUT diff:TDiff):P_mapLiteral;
  VAR aHashes,bHashes:PInteger;
      aLen,bLen:integer;
      i:longint;
      {$ifdef withEditScript}
      comp:P_listLiteral;
      {$endif}
      tempChars:P_listLiteral;
  begin
    diff.create();
    if A^.literalType=lt_string then tempChars:=chars_internal(A)
                                else tempChars:=P_listLiteral(A^.rereferenced);
    with tempChars^ do begin
      aLen:=size;
      getMem(aHashes,aLen*sizeOf(integer));
      {$R-}
      for i:=0 to aLen-1 do aHashes[i]:=value[i]^.hash;
      {$R+}
    end;
    disposeLiteral(tempChars);

    if B^.literalType=lt_string then tempChars:=chars_internal(B)
                                else tempChars:=P_listLiteral(B^.rereferenced);
    with tempChars^ do begin
      bLen:=size;
      getMem(bHashes,bLen*sizeOf(integer));
      {$R-}
      for i:=0 to bLen-1 do bHashes[i]:=value[i]^.hash;
      {$R+}
    end;
    disposeLiteral(tempChars);

    diff.execute(aHashes,bHashes,aLen,bLen);
    freeMem(aHashes,aLen*sizeOf(integer));
    freeMem(bHashes,bLen*sizeOf(integer));
    result:=newMapLiteral^
            .put('adds'    ,diff.DiffStats.adds    )^
            .put('deletes' ,diff.DiffStats.deletes )^
            .put('matches' ,diff.DiffStats.matches )^
            .put('modifies',diff.DiffStats.modifies);
  end;

FUNCTION diff_impl intFuncSignature;
  CONST changeKindChar:array[TChangeKind] of char=(' ','+','-','M');
  VAR diff:TDiff;

  FUNCTION simpleEdit:P_listLiteral;
    VAR i:longint;
    begin
      result:=newListLiteral(diff.count);
      for i:=0 to diff.count-1 do
        result^.append(newListLiteral(3)^
          .appendString(changeKindChar[diff.Compares[i].kind    ])^
          .appendInt   (               diff.Compares[i].oldIndex1)^
          .appendInt   (               diff.Compares[i].oldIndex2),false);
    end;

  FUNCTION blockEdit:P_listLiteral;
    TYPE T_diffRec=record
                     kind:char;
                     i0,i1:longint;
                   end;
         T_diffArr=array of T_diffRec;

    FUNCTION diffRec(CONST kind:char; CONST i0,i1:longint):T_diffRec;
      begin
        result.kind:=kind;
        result.i0  :=i0;
        result.i1  :=i1;
      end;

    PROCEDURE add(VAR arr:T_diffArr; CONST rec:T_diffRec);
      begin
        setLength(arr,length(arr)+1);
        arr[length(arr)-1]:=rec;
      end;

    VAR digest,modifyRun:T_diffArr;
        rec:T_diffRec;
        i,k:longint;
    begin
      //Convert modify runs to adds and deletes:--------------------------------
      setLength(modifyRun,0);
      setLength(digest,0);
      i:=0;
      while i<diff.count do
      if diff.Compares[i].kind=ckModify then begin
        while (i<diff.count) and (diff.Compares[i].kind=ckModify) do begin
          add(modifyRun,diffRec('M',diff.Compares[i].oldIndex1,diff.Compares[i].oldIndex2));
          inc(i);
        end;
        for k:=0 to length(modifyRun)-1 do add(digest,diffRec('-',modifyRun[k].i0,modifyRun[k].i1));
        for k:=0 to length(modifyRun)-1 do add(digest,diffRec('+',modifyRun[k].i0,modifyRun[k].i1));
        setLength(modifyRun,0)
      end else begin
        add(digest,diffRec(changeKindChar[diff.Compares[i].kind],diff.Compares[i].oldIndex1,diff.Compares[i].oldIndex2));
        inc(i);
      end;
      //---------------------------------Convert modify runs to adds and deletes
      result:=newListLiteral(length(digest));
      for rec in digest do result^.append(newListLiteral(3)^.appendString(rec.kind)^.appendInt(rec.i0)^.appendInt(rec.i1),false);
    end;

  begin
    result:=nil;
    if (params<>nil) and ((params^.size=2) or (params^.size=3)) and
       ((arg0^.literalType in [lt_stringList,lt_emptyList]) and
        (arg1^.literalType in [lt_stringList,lt_emptyList]) or
        (arg0^.literalType=lt_string) and
        (arg1^.literalType=lt_string)) and
       ((params^.size=2) or (arg2^.literalType=lt_boolean)) then begin
      result:=prepareDiff(arg0,arg1,diff);
      if (params^.size=3) and (bool2^.value)
      then mapResult^.put('edit',blockEdit ,false)
      else mapResult^.put('edit',simpleEdit,false);
      diff.destroy;
    end;
  end;

FUNCTION diffStats_impl intFuncSignature;
  VAR diff:TDiff;
  begin
    result:=nil;
    if (params<>nil) and ((params^.size=2) or (params^.size=3)) and
       ((arg0^.literalType in [lt_stringList,lt_emptyList]) and
        (arg1^.literalType in [lt_stringList,lt_emptyList]) or
        (arg0^.literalType=lt_string) and
        (arg1^.literalType=lt_string)) and
       ((params^.size=2) or (arg2^.literalType=lt_int) and (int2^.value>=0)) then begin
      result:=prepareDiff(arg0,arg1,diff);
      diff.destroy;
    end;
  end;

FUNCTION isUtf8_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(str0^.getEncoding in [se_utf8,se_ascii]);
  end;

FUNCTION isAscii_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(str0^.getEncoding=se_ascii);
  end;

FUNCTION utf8ToAnsi_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(UTF8ToCP1252(str0^.value));
  end;

FUNCTION ansiToUtf8_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(CP1252ToUTF8(str0^.value));
  end;

FUNCTION base64encode_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(EncodeStringBase64(str0^.value));
  end;

FUNCTION base64decode_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(DecodeStringBase64(str0^.value));
  end;

FUNCTION compress_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(compressString(str0^.value,0))
    else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_int)
      then result:=newStringLiteral(compressString(str0^.value,int1^.value))
  end;

FUNCTION decompress_impl intFuncSignature;
  VAR resultString:ansistring;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then begin
      try
        resultString:=decompressString(str0^.value);
      except
        context.adapters^.raiseError('Internal error on decompression. The string may not be a cleanly compressed string.',tokenLocation);
        exit(nil);
      end;
      result:=newStringLiteral(resultString);
    end;
  end;

FUNCTION formatTabs_impl intFuncSignature;
  VAR arr:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      arr:=formatTabs(split(str0^.value));
      result:=newListLiteral;
      for i:=0 to length(arr)-1 do listResult^.appendString(arr[i]);
      setLength(arr,0);
    end;
  end;

{$define LENGTH_MACRO:=(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  FUNCTION innerRec(l:P_literal):P_literal;
    VAR iter:T_arrayOfLiteral;
        sub :P_literal;
    begin
      result:=nil;
      case l^.literalType of
        lt_string: if P_stringLiteral(L)^.getEncoding=se_utf8
                   then result:=newIntLiteral(LENGTH_FUNC(P_stringLiteral(L)^.value))
                   else result:=newIntLiteral(     length(P_stringLiteral(L)^.value));
        lt_list,lt_stringList,lt_emptyList,
        lt_set,lt_stringSet,lt_emptySet:
        begin
          result:=P_collectionLiteral(l)^.newOfSameType(false);
          iter  :=P_collectionLiteral(l)^.iteratableList;
          for sub in iter do P_collectionLiteral(result)^.append(innerRec(sub),false);
          disposeLiteral(iter);
        end;
        else raiseNotApplicableError(ID_MACRO,l,tokenLocation,context.adapters^);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=innerRec(arg0);
  end}

FUNCTION length_imp     {$define LENGTH_FUNC:=UTF8Length} {$define ID_MACRO:='length'}     LENGTH_MACRO;
FUNCTION byteLength_imp {$define LENGTH_FUNC:=length}     {$define ID_MACRO:='byteLength'} LENGTH_MACRO;
{$undef LENGTH_FUNC}
{$undef LENGTH_MACRO}

FUNCTION md5_imp intFuncSignature;
  CONST hexDig:array[0..15]of char=('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  VAR md5String:string;
      hexString:string='';
      c:char;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then begin
      md5String:=MD5(str0^.value);
      for c in md5String do begin
        hexString:=hexString+hexDig[(ord(c) shr 4) and 15]
                            +hexDig[ ord(c)        and 15];
      end;
      result:=newStringLiteral(hexString)
    end else result:=nil;
  end;

INITIALIZATION
  //Functions on Strings:
  registerRule(STRINGS_NAMESPACE,'length'        ,@length_imp        ,ak_unary     ,'length(S:string);//Returns the number of characters in string S');
  registerRule(STRINGS_NAMESPACE,'byteLength'    ,@byteLength_imp    ,ak_unary     ,'byteLength(S:string);//Returns the number of bytes in string S');
  registerRule(STRINGS_NAMESPACE,'pos'           ,@pos_imp           ,ak_binary    ,'pos(subString,searchInString);//Returns the index of the first occurence of subString in searchInString or -1 if there is none');
  registerRule(STRINGS_NAMESPACE,'copy'          ,@copy_imp          ,ak_ternary   ,'copy(S,start,length)://Returns the substring of S starting at index start and having specified length');
  registerRule(STRINGS_NAMESPACE,'chars'         ,@chars_imp         ,ak_unary     ,'chars(S);//Returns the characters in S as a list#chars;//Returns all possible single-byte characters in natural ordering');
  registerRule(STRINGS_NAMESPACE,'charSet'       ,@charSet_imp       ,ak_unary     ,'charSet(S);//Returns the characters in S as a set (ordered list without duplicates)');
  registerRule(STRINGS_NAMESPACE,'bytes'         ,@bytes_imp         ,ak_unary     ,'bytes(S);//Returns the bytes in S as a list of strings');
  registerRule(STRINGS_NAMESPACE,'split'         ,@split_imp         ,ak_binary    ,'split(S:string,splitter:string);//Returns a list of strings obtained by splitting S at the specified splitters#//The splitters themselves are not contained in the result');
  registerRule(STRINGS_NAMESPACE,'join'          ,@join_impl         ,ak_unary     ,'join(L:list);//Returns a string-concatenation of all elements in L#join(L:list,joiner:string);//Returns a string-concatenation of all elements, with joiner between.');
  registerRule(STRINGS_NAMESPACE,'trim'          ,@trim_imp          ,ak_unary     ,'trim(S:string);//Returns string S without leading or trailing spaces');
  registerRule(STRINGS_NAMESPACE,'trimLeft'      ,@trimLeft_imp      ,ak_unary     ,'trimLeft(S:string);//Returns string S without leading spaces');
  registerRule(STRINGS_NAMESPACE,'trimRight'     ,@trimRight_imp     ,ak_unary     ,'trimRight(S:string);//Returns string S without trailing spaces');
  registerRule(STRINGS_NAMESPACE,'upper'         ,@upper_imp         ,ak_unary     ,'upper(S:string);//Returns an uppercase representation of S');
  registerRule(STRINGS_NAMESPACE,'lower'         ,@lower_imp         ,ak_unary     ,'lower(S:string);//Returns an lowercase representation of S');
  registerRule(STRINGS_NAMESPACE,'clean'         ,@clean_impl        ,ak_variadic_2,'clean(s,whiteList:stringCollection,instead:string);//Replaces all characters in s which are not in whitelist by instead.#clean(s,whiteList:stringCollection,instead:string,joinPlaceholders:boolean);//As above but joining placeholders');
  registerRule(STRINGS_NAMESPACE,'unbrace'       ,@unbrace_imp       ,ak_unary     ,'unbrace(S:string);//Returns an unbraced representation of S');
  registerRule(STRINGS_NAMESPACE,'escape'        ,@escape_imp        ,ak_unary     ,'escape(S:string);//Returns an escaped representation of S');
  registerRule(STRINGS_NAMESPACE,'escapePascal'  ,@escapePascal_imp  ,ak_unary     ,'escapePascal(S:string);//Returns an escaped representation of S for use in Pascal source code');
  registerRule(STRINGS_NAMESPACE,'escapeJava'    ,@escapeJava_imp    ,ak_unary     ,'escapeJava(S:string);//Returns an escaped representation of S for use in Java source code');
  registerRule(STRINGS_NAMESPACE,'replaceOne'    ,@replaceOne_impl   ,ak_ternary   ,'replaceOne(source:string,lookFor,replaceBy);//Replaces the first occurences of lookFor in source by replaceBy#//lookFor and replaceBy may be of type string or stringList');
  registerRule(STRINGS_NAMESPACE,'replace'       ,@replace_impl      ,ak_ternary   ,'replace(source:string,lookFor,replaceBy);//Recursively replaces all occurences of lookFor in source by replaceBy#//lookFor and replaceBy may be of type string or stringList');
  registerRule(STRINGS_NAMESPACE,'repeat'        ,@repeat_impl       ,ak_binary    ,'repeat(s:string,k:int);//Returns a string containing s repeated k times');
  registerRule(STRINGS_NAMESPACE,'reverseString' ,@reverseString_impl,ak_unary     ,'reverseString(S:string);//reverseString(S:stringList);//Returns returns S reversed (character wise not bytewise)');
  registerRule(STRINGS_NAMESPACE,'diff'          ,@diff_impl         ,ak_variadic_2,'diff(A,B);//Shows diff statistics and edit script for strings A and B or string lists A and B#diff(A,B,convertModifies:boolean);//As above but optionally convert modifies to adds and deletes');
  registerRule(STRINGS_NAMESPACE,'diffStats'     ,@diffStats_impl    ,ak_binary    ,'diffStats(A,B);//Shows diff statistics for strings A and B or string lists A and B');
  registerRule(STRINGS_NAMESPACE,'isUtf8'        ,@isUtf8_impl       ,ak_unary     ,'isUtf8(S:string);//Returns true if S is UTF8 encoded and false otherwise');
  registerRule(STRINGS_NAMESPACE,'isAscii'       ,@isAscii_impl      ,ak_unary     ,'isAscii(S:string);//Returns true if S is ASCII encoded and false otherwise');
  registerRule(STRINGS_NAMESPACE,'utf8ToAnsi'    ,@utf8ToAnsi_impl   ,ak_unary     ,'utf8ToAnsi(S:string);//Converts a UTF8 encoded string to an ANSI encoded string.');
  registerRule(STRINGS_NAMESPACE,'ansiToUtf8'    ,@ansiToUtf8_impl   ,ak_unary     ,'ansiToUtf8(S:string);//Converts an ANSI encoded string to a UTF8 encoded string.');
  registerRule(STRINGS_NAMESPACE,'base64encode'  ,@base64encode_impl ,ak_unary     ,'base64encode(S:string);//Converts a string to a base64 encoded string.');
  registerRule(STRINGS_NAMESPACE,'base64decode'  ,@base64decode_impl ,ak_unary     ,'base64decode(S:string);//Converts a base64 encoded string to a string.');
  registerRule(STRINGS_NAMESPACE,'compress'      ,@compress_impl     ,ak_unary     ,'compress(S:string);#Returns a compressed version of S#compress(S:string,k:int);#'+
                                                           'As above but with a specified algorithm:#'+
                                                           '  1: deflate#'+
                                                           '  2: huffman with default model#'+
                                                           '  3: huffman with another model#'+
                                                           '255: don''''t compress'+
                                                           '  other: try out algorithms and return the shortest representation#'+
                                                           '  The first character of the result indicates the algorithm used');
  registerRule(STRINGS_NAMESPACE,'decompress'    ,@decompress_impl,ak_unary,'decompress(S:string);#Returns an uncompressed version of S');
  registerRule(STRINGS_NAMESPACE,'formatTabs'    ,@formatTabs_impl,ak_unary,'formatTabs(S:string);#Applies tab formatting as on print');
  registerRule(STRINGS_NAMESPACE,'md5'           ,@md5_imp,ak_unary,'md5(S:string);#Returns MD5 string for given input S');
end.
