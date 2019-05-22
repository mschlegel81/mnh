UNIT funcs_strings;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,math,
     LazUTF8,base64,LConvEncoding,
     synacode,
     diff,
     myGenerics,myStringUtil,myCrypto,bigint,
     basicTypes,
     mnh_constants,
     out_adapters,
     recyclers,
     litVar,funcs,contexts;
IMPLEMENTATION
{$i func_defines.inc}

FUNCTION pos_imp intFuncSignature;
  VAR i:int64;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string)
    then begin
      if str0^.getEncoding=se_utf8
      then i:=int64(UTF8Pos(str0^.value,
                            str1^.value))
      else i:=int64(    pos(str0^.value,
                            str1^.value));
      i-=1;
      if i<0 then result:=newRealLiteral(infinity)
             else result:=newIntLiteral(i);
    end else result:=genericVectorization('pos',params,tokenLocation,context,recycler);
  end;

FUNCTION hasPrefix_imp intFuncSignature;
  VAR a,b:string;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string)
    then begin
      a:=str0^.value;
      b:=str1^.value;
      result:=newBoolLiteral((length(b)<=length(a)) and (pos(b,a)=1));
    end else result:=genericVectorization('hasPrefix',params,tokenLocation,context,recycler);
  end;

FUNCTION copy_imp intFuncSignature;
  FUNCTION myCopy(CONST s:P_stringLiteral; CONST start,len:int64):P_stringLiteral; inline;
    begin
      if s^.getEncoding=se_utf8 then result:=newStringLiteral(UTF8Copy(s^.value,start,len))
                                else result:=newStringLiteral(    copy(s^.value,start,len));
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (arg0^.literalType=lt_string) and (arg1^.literalType in [lt_smallint,lt_bigint]) and (arg2^.literalType in [lt_smallint,lt_bigint])
    then result:=myCopy(str0,
                        P_abstractIntLiteral(arg1)^.intValue+1,
                        P_abstractIntLiteral(arg2)^.intValue)
    else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType in [lt_smallint,lt_bigint])
    then result:=myCopy(str0,
                        P_abstractIntLiteral(arg1)^.intValue+1,
                        maxLongint)
    else result:=genericVectorization('copy',params,tokenLocation,context,recycler);
  end;

FUNCTION bytes_internal(CONST input:P_literal):P_listLiteral;
  VAR c:char;
  begin
    result:=newListLiteral(length(P_stringLiteral(input)^.value));
    for c in P_stringLiteral(input)^.value do result^.appendString(c);
  end;

FUNCTION chars_internal(CONST input:P_literal):P_listLiteral;
  VAR charIndex,
      byteIndex:longint;
      i:longint;
      sub:ansistring;
  begin
    if not(P_stringLiteral(input)^.getEncoding=se_utf8) then exit(bytes_internal(input));
    result:=newListLiteral;
    byteIndex:=1;
    for charIndex:=1 to UTF8Length(P_stringLiteral(input)^.value) do begin
      sub:='';
      for i:=0 to UTF8CharacterLength(@(P_stringLiteral(input)^.value[byteIndex]))-1 do begin
        sub:=sub+P_stringLiteral(input)^.value[byteIndex];
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

FUNCTION byteToChar_imp intFuncSignature;
  FUNCTION charOf(CONST i:P_abstractIntLiteral):P_literal;
    VAR ib:longint;
    begin
      result:=nil;
      if i^.literalType=lt_smallint then begin
        ib:=P_smallIntLiteral(i)^.value;
        if (ib>=0) and (ib<=255) then exit(charLit[chr(ib)].rereferenced);
      end else if (i^.literalType=lt_bigint) and P_bigIntLiteral(i)^.value.canBeRepresentedAsInt32 then begin
        ib:=P_bigIntLiteral(i)^.intValue;
        if (ib>=0) and (ib<=255) then exit(charLit[chr(ib)].rereferenced);
      end;
      context.raiseError('Value '+i^.toString+' is not a valid byte; must be in range [0..255]',tokenLocation);
    end;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint])
    then result:=charOf(P_abstractIntLiteral(int0))
    else result:=genericVectorization('byteToChar',params,tokenLocation,context,recycler);
  end;

FUNCTION charSet_imp intFuncSignature;
  FUNCTION charset_internal(CONST input:P_stringLiteral):P_setLiteral;
    VAR charIndex,
        byteIndex:longint;
        i:longint;
        sub:ansistring;
        charSetUtf8:T_setOfString;
        byteSet:T_charSet=[];
        c:char;
    begin
      if input^.getEncoding=se_utf8 then begin
        charSetUtf8.create;
        byteIndex:=1;
        for charIndex:=1 to UTF8Length(input^.value) do begin
          sub:='';
          for i:=0 to UTF8CharacterLength(@(input^.value[byteIndex]))-1 do begin
            sub:=sub+input^.value[byteIndex];
            inc(byteIndex)
          end;
          charSetUtf8.put(sub);
        end;
        result:=newSetLiteral;
        for sub in charSetUtf8.values do result^.appendString(sub);
        charSetUtf8.destroy;
      end else begin
        for c in input^.value do include(byteSet,c);
        result:=newSetLiteral;
        for c in byteSet do result^.appendString(c);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=charset_internal(str0)
    else result:=genericVectorization('charSet',params,tokenLocation,context,recycler);
  end;

FUNCTION bytes_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=bytes_internal(arg0)
    else result:=genericVectorization('bytes',params,tokenLocation,context,recycler);
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
        else raiseNotApplicableError('split',p,tokenLocation,context);
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
        lt_string: result:=P_stringLiteral(L)^.value;
        lt_boolean,
        lt_smallint,
        lt_bigint,
        lt_real,
        lt_expression,
        lt_list..lt_emptyMap: result:=L^.toString;
        else result:='';
      end;
    end;

  VAR i:longint;
      iter:T_arrayOfLiteral;
      resultParts:T_arrayOfString;
      resultString:ansistring;
  begin
    result:=nil;
    if (params<>nil) and ((params^.size=1) or (params^.size=2) and (arg1^.literalType=lt_string)) then begin
      if (arg0^.literalType in C_listTypes+C_setTypes) then begin
        if collection0^.size=0 then exit(newStringLiteral(''));
        iter:=collection0^.iteratableList;
        setLength(resultParts,length(iter));
        for i:=0 to length(iter)-1 do resultParts[i]:=stringOfLit(iter[i]);
        disposeLiteral(iter);

        if params^.size=2
        then resultString:=join(resultParts,str1^.value)
        else resultString:=join(resultParts,''         );
        result:=newStringLiteral(resultString);
        setLength(resultParts,0);
      end else if (arg0^.literalType in C_scalarTypes) then
        result:=newStringLiteral(stringOfLit(arg0));
    end;
  end;

{$define STRINGLITERAL_ROUTINE:=
begin
  result:=nil;
  if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
  then result:=str0^.CALL_MACRO
  else result:=genericVectorization(ID_MACRO,params,tokenLocation,context,recycler);
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
  VAR dummy:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(escapeString(str0^.value,es_mnhPascalStyle,str0^.getEncoding,dummy))
    else result:=genericVectorization('escapePascal',params,tokenLocation,context,recycler);
  end;

FUNCTION escapeJava_imp intFuncSignature;
  VAR nonescapableFound:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then begin
      result:=newStringLiteral(escapeString(str0^.value,es_javaStyle,str0^.getEncoding,nonescapableFound));
      if nonescapableFound then context.raiseError('escapeJava cannot be applied to this string because it contains characters that cannot be represented.',tokenLocation);
    end
    else result:=genericVectorization('escapeJava',params,tokenLocation,context,recycler);
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
       (arg1^.literalType = lt_smallint) then begin
      res:='';
      sub:=str0^.value;
      for i:=1 to int1^.intValue do res:=res+sub;
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
        sub:ansistring;
    begin
      result:='';
      if P_stringLiteral(input)^.getEncoding=se_utf8 then begin
        byteIndex:=1;
        for charIndex:=1 to UTF8Length(P_stringLiteral(input)^.value) do begin
          sub:='';
          for i:=0 to UTF8CharacterLength(@(P_stringLiteral(input)^.value[byteIndex]))-1 do begin
            sub:=sub+P_stringLiteral(input)^.value[byteIndex];
            inc(byteIndex)
          end;
          result:=sub+result;
        end;
      end else begin
        i:=length(P_stringLiteral(input)^.value);
        setLength(result,i);
        for byteIndex:=1 to i do result[byteIndex]:=P_stringLiteral(input)^.value[i+1-byteIndex];
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(rev(arg0))
    else result:=genericVectorization('reverseString',params,tokenLocation,context,recycler);
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
    if (params<>nil) and (params^.size=2) and
       ((arg0^.literalType in [lt_stringList,lt_emptyList]) and
        (arg1^.literalType in [lt_stringList,lt_emptyList]) or
        (arg0^.literalType=lt_string) and
        (arg1^.literalType=lt_string)) then begin
      result:=prepareDiff(arg0,arg1,diff);
      diff.destroy;
    end;
  end;

FUNCTION isUtf8_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(str0^.getEncoding in [se_utf8,se_ascii])
    else result:=genericVectorization('isUtf8',params,tokenLocation,context,recycler);
  end;

FUNCTION isAscii_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(str0^.getEncoding=se_ascii)
    else result:=genericVectorization('isAscii',params,tokenLocation,context,recycler);
  end;

FUNCTION utf8ToAnsi_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(UTF8ToCP1252(str0^.value))
    else result:=genericVectorization('utf8ToAnsi',params,tokenLocation,context,recycler);
  end;

FUNCTION ansiToUtf8_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(CP1252ToUTF8(str0^.value))
    else result:=genericVectorization('ansiToUtf8',params,tokenLocation,context,recycler);
  end;

FUNCTION base64encode_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(EncodeStringBase64(str0^.value))
    else result:=genericVectorization('base64encode',params,tokenLocation,context,recycler);
  end;

FUNCTION base64decode_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(DecodeStringBase64(str0^.value))
    else result:=genericVectorization('base64decode',params,tokenLocation,context,recycler);
  end;

FUNCTION compress_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(compressString(str0^.value,[0..255]))
    else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType in [lt_bigint,lt_smallint])
    then result:=newStringLiteral(compressString(str0^.value,[byte(int1^.intValue)]))
    else result:=genericVectorization('compress',params,tokenLocation,context,recycler);
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
        context.raiseError('Internal error on decompression. The string may not be a cleanly compressed string.',tokenLocation);
        exit(nil);
      end;
      result:=newStringLiteral(resultString);
    end else result:=genericVectorization('decompress',params,tokenLocation,context,recycler);
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
{$WARN 5024 OFF}
{$define LENGTH_MACRO:=(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal;
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
        else raiseNotApplicableError(ID_MACRO,l,tokenLocation,context);
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
  VAR md5String:string;
      md5Int:T_bigInt;
      digits:T_arrayOfLongint;
      i:longint;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then begin
      md5String:=MD5(str0^.value);
      setLength(digits,length(md5String));
      for i:=1 to length(md5String) do digits[i-1]:=ord(md5String[i]);
      md5Int.createFromDigits(256,digits);
      result:=newIntLiteral(md5Int);
    end else result:=genericVectorization('md5',params,tokenLocation,context,recycler);
  end;

FUNCTION sha256_imp intFuncSignature;
  VAR sha256Int:T_bigInt;
      sha256Digest:T_sha256Hash;
      sha256Digits:T_arrayOfLongint;
      i:longint;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then begin
      sha256Digest:=sha256(str0^.value);
      setLength(sha256Digits,length(sha256Digest));
      for i:=0 to length(sha256Digest)-1 do sha256Digits[i]:=sha256Digest[i];
      sha256Int.createFromDigits(256,sha256Digits);
      result:=newIntLiteral(sha256Int);
    end else result:=genericVectorization('sha256',params,tokenLocation,context,recycler);
  end;

INITIALIZATION
  //Functions on Strings:
  registerRule(STRINGS_NAMESPACE,'length'        ,@length_imp        ,ak_unary     {$ifdef fullVersion},'length(S:String);//Returns the number of characters in string S'{$endif});
  registerRule(STRINGS_NAMESPACE,'byteLength'    ,@byteLength_imp    ,ak_unary     {$ifdef fullVersion},'byteLength(S:String);//Returns the number of bytes in string S'{$endif});
  registerRule(STRINGS_NAMESPACE,'pos'           ,@pos_imp           ,ak_binary    {$ifdef fullVersion},'pos(subString,searchInString);//Returns the index of the first occurence of subString in searchInString or Infinity if there is none'{$endif});
  registerRule(STRINGS_NAMESPACE,'hasPrefix'     ,@hasPrefix_imp     ,ak_binary    {$ifdef fullVersion},'hasPrefix(s,prefix);//Returns true if s starts with prefix'{$endif});
  registerRule(STRINGS_NAMESPACE,'copy'          ,@copy_imp          ,ak_ternary   {$ifdef fullVersion},'copy(S,start,length)://Returns the substring of S starting at index start and having specified length'{$endif});
  registerRule(STRINGS_NAMESPACE,'chars'         ,@chars_imp         ,ak_unary     {$ifdef fullVersion},'chars(S);//Returns the characters in S as a list#chars;//Returns all possible single-byte characters in natural ordering'{$endif});
  registerRule(STRINGS_NAMESPACE,'charSet'       ,@charSet_imp       ,ak_unary     {$ifdef fullVersion},'charSet(S);//Returns the characters in S as a set (ordered list without duplicates)'{$endif});
  registerRule(STRINGS_NAMESPACE,'byteToChar'    ,@byteToChar_imp    ,ak_unary     {$ifdef fullVersion},'byteToChar(b in [0..255]);//Returns the corresponding character as a string of one byte length'{$endif});
  registerRule(STRINGS_NAMESPACE,'bytes'         ,@bytes_imp         ,ak_unary     {$ifdef fullVersion},'bytes(S);//Returns the bytes in S as a list of strings'{$endif});
  registerRule(STRINGS_NAMESPACE,'split'         ,@split_imp         ,ak_binary    {$ifdef fullVersion},'split(S:String,splitter:String);//Returns a list of strings obtained by splitting S at the specified splitters#//The splitters themselves are not contained in the result'{$endif});
  registerRule(STRINGS_NAMESPACE,'join'          ,@join_impl         ,ak_unary     {$ifdef fullVersion},'join(L:List);//Returns a string-concatenation of all elements in L#join(L:List,joiner:String);//Returns a string-concatenation of all elements, with joiner between.'{$endif});
  registerRule(STRINGS_NAMESPACE,'trim'          ,@trim_imp          ,ak_unary     {$ifdef fullVersion},'trim(S:String);//Returns string S without leading or trailing spaces'{$endif});
  registerRule(STRINGS_NAMESPACE,'trimLeft'      ,@trimLeft_imp      ,ak_unary     {$ifdef fullVersion},'trimLeft(S:String);//Returns string S without leading spaces'{$endif});
  registerRule(STRINGS_NAMESPACE,'trimRight'     ,@trimRight_imp     ,ak_unary     {$ifdef fullVersion},'trimRight(S:String);//Returns string S without trailing spaces'{$endif});
  registerRule(STRINGS_NAMESPACE,'upper'         ,@upper_imp         ,ak_unary     {$ifdef fullVersion},'upper(S:String);//Returns an uppercase representation of S'{$endif});
  registerRule(STRINGS_NAMESPACE,'lower'         ,@lower_imp         ,ak_unary     {$ifdef fullVersion},'lower(S:String);//Returns an lowercase representation of S'{$endif});
  registerRule(STRINGS_NAMESPACE,'clean'         ,@clean_impl        ,ak_variadic_2{$ifdef fullVersion},'clean(s,whiteList:StringCollection,instead:String);//Replaces all characters in s which are not in whitelist by instead.#clean(s,whiteList:StringCollection,instead:String,joinPlaceholders:boolean);//As above but joining placeholders'{$endif});
  registerRule(STRINGS_NAMESPACE,'unbrace'       ,@unbrace_imp       ,ak_unary     {$ifdef fullVersion},'unbrace(S:String);//Returns an unbraced representation of S'{$endif});
  registerRule(STRINGS_NAMESPACE,'escape'        ,@escape_imp        ,ak_unary     {$ifdef fullVersion},'escape(S:String);//Returns an escaped representation of S'{$endif});
  registerRule(STRINGS_NAMESPACE,'escapePascal'  ,@escapePascal_imp  ,ak_unary     {$ifdef fullVersion},'escapePascal(S:String);//Returns an escaped representation of S for use in Pascal source code'{$endif});
  registerRule(STRINGS_NAMESPACE,'escapeJava'    ,@escapeJava_imp    ,ak_unary     {$ifdef fullVersion},'escapeJava(S:String);//Returns an escaped representation of S for use in Java source code'{$endif});
  registerRule(STRINGS_NAMESPACE,'replaceOne'    ,@replaceOne_impl   ,ak_ternary   {$ifdef fullVersion},'replaceOne(source:String,lookFor,replaceBy);//Replaces the first occurences of lookFor in source by replaceBy#//lookFor and replaceBy may be of type string or stringList'{$endif});
  registerRule(STRINGS_NAMESPACE,'replace'       ,@replace_impl      ,ak_ternary   {$ifdef fullVersion},'replace(source:String,lookFor,replaceBy);//Recursively replaces all occurences of lookFor in source by replaceBy#//lookFor and replaceBy may be of type string or stringList'{$endif});
  registerRule(STRINGS_NAMESPACE,'repeat'        ,@repeat_impl       ,ak_binary    {$ifdef fullVersion},'repeat(s:String,k:Int);//Returns a string containing s repeated k times'{$endif});
  registerRule(STRINGS_NAMESPACE,'reverseString' ,@reverseString_impl,ak_unary     {$ifdef fullVersion},'reverseString(S:String);//reverseString(S:StringList);//Returns returns S reversed (character wise not bytewise)'{$endif});
  registerRule(STRINGS_NAMESPACE,'diff'          ,@diff_impl         ,ak_variadic_2{$ifdef fullVersion},'diff(A,B);//Shows diff statistics and edit script for strings A and B or string lists A and B#diff(A,B,convertModifies:boolean);//As above but optionally convert modifies to adds and deletes'{$endif});
  registerRule(STRINGS_NAMESPACE,'diffStats'     ,@diffStats_impl    ,ak_binary    {$ifdef fullVersion},'diffStats(A,B);//Shows diff statistics for strings A and B or string lists A and B'{$endif});
  registerRule(STRINGS_NAMESPACE,'isUtf8'        ,@isUtf8_impl       ,ak_unary     {$ifdef fullVersion},'isUtf8(S:String);//Returns true if S is UTF8 encoded and false otherwise'{$endif});
  registerRule(STRINGS_NAMESPACE,'isAscii'       ,@isAscii_impl      ,ak_unary     {$ifdef fullVersion},'isAscii(S:String);//Returns true if S is ASCII encoded and false otherwise'{$endif});
  registerRule(STRINGS_NAMESPACE,'utf8ToAnsi'    ,@utf8ToAnsi_impl   ,ak_unary     {$ifdef fullVersion},'utf8ToAnsi(S:String);//Converts a UTF8 encoded string to an ANSI encoded string.'{$endif});
  registerRule(STRINGS_NAMESPACE,'ansiToUtf8'    ,@ansiToUtf8_impl   ,ak_unary     {$ifdef fullVersion},'ansiToUtf8(S:String);//Converts an ANSI encoded string to a UTF8 encoded string.'{$endif});
  registerRule(STRINGS_NAMESPACE,'base64encode'  ,@base64encode_impl ,ak_unary     {$ifdef fullVersion},'base64encode(S:String);//Converts a string to a base64 encoded string.'{$endif});
  registerRule(STRINGS_NAMESPACE,'base64decode'  ,@base64decode_impl ,ak_unary     {$ifdef fullVersion},'base64decode(S:String);//Converts a base64 encoded string to a string.'{$endif});
  registerRule(STRINGS_NAMESPACE,'compress'      ,@compress_impl     ,ak_unary     {$ifdef fullVersion},'compress(S:String);#Returns a compressed version of S#compress(S:String,k:Int);#'+
                                                           'As above but with a specified algorithm:#'+
                                                           '  1: deflate#'+
                                                           '  2: huffman with relaxed default model#'+
                                                           '  3: huffman with strict default model#'+
                                                           '  4: huffman with numbers model#'+
                                                           '  5: huffman with Wikipedia.de model#'+
                                                           '  6: huffman with MNH code model#'+
                                                           '  7: huffman with MNH datastore model#'+
                                                           '255: don''''t compress#'+
                                                           '  other: try out algorithms and return the shortest representation#'+
                                                           '  The first character of the result indicates the algorithm used'{$endif});
  registerRule(STRINGS_NAMESPACE,'decompress'    ,@decompress_impl,ak_unary{$ifdef fullVersion},'decompress(S:String);#Returns an uncompressed version of S'{$endif});
  registerRule(STRINGS_NAMESPACE,'formatTabs'    ,@formatTabs_impl,ak_unary{$ifdef fullVersion},'formatTabs(S:String);#Applies tab formatting as on print'{$endif});
  registerRule(STRINGS_NAMESPACE,'md5'           ,@md5_imp,ak_unary{$ifdef fullVersion},'md5(S:String);#Returns the MD5 digest as hexadecimal string for given input S'{$endif});
  registerRule(STRINGS_NAMESPACE,'sha256'        ,@sha256_imp,ak_unary{$ifdef fullVersion},'sha256(S:String);#Returns SHA256 digest as hexadecimal string for given input S'{$endif});
end.
