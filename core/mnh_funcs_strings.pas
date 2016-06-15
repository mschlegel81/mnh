// MIT License
//
// Copyright (c) 2016 Martin Schlegel
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

UNIT mnh_funcs_strings;
INTERFACE
{$WARN 5024 OFF}
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,myGenerics,myStringUtil,sysutils,diff,mnh_contexts,LazUTF8;
IMPLEMENTATION
{$MACRO ON}
{$define str0:=P_stringLiteral(params^.value(0))}
{$define str1:=P_stringLiteral(params^.value(1))}
{$define str2:=P_stringLiteral(params^.value(2))}
{$define list0:=P_listLiteral(params^.value(0))}
{$define list1:=P_listLiteral(params^.value(1))}
{$define list2:=P_listLiteral(params^.value(2))}
{$define int1:=P_intLiteral(params^.value(1))}
{$define int2:=P_intLiteral(params^.value(2))}
{$define arg0:=params^.value(0)}
{$define arg1:=params^.value(1)}
{$define arg2:=params^.value(2)}

FUNCTION length_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then
    case arg0^.literalType of
      lt_string: result:=newIntLiteral(length(str0^.value));
      lt_stringList,lt_emptyList: begin
        result:=newListLiteral;
        for i:=0 to list0^.size-1 do
          P_listLiteral(result)^.appendInt(length(P_stringLiteral(list0^.value(i))^.value));
      end;
    end;
  end;

FUNCTION pos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION posInt(x,y:P_literal):P_intLiteral;
    begin
      result:=newIntLiteral(int64(pos(P_stringLiteral(x)^.value,
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
            P_listLiteral(result)^.append(posInt(arg0,list1^.value(i)),false);
        end;
      end else begin
        if arg1^.literalType=lt_string then begin
          result:=newListLiteral;
          for i:=0 to list0^.size-1 do
            P_listLiteral(result)^.append(posInt(list0^.value(i),
                                                               arg1           ),false);
        end else begin
          if list0^.size=list1^.size then begin
            result:=newListLiteral;
            for i:=0 to list0^.size-1 do
              P_listLiteral(result)^.append(posInt(list0^.value(i),
                                                   list1^.value(i)),false);
          end else context.adapters^.raiseError('Incompatible list lengths for function pos.',tokenLocation)
        end;
      end;
    end;
  end;

FUNCTION copy_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
      if arg0^.literalType=lt_string
        then result:=str0^.value
        else result:=P_stringLiteral(list0^.value(index))^.value;
    end;

  FUNCTION safeStart(index:longint):longint;
    begin
      if arg1^.literalType=lt_int
        then result:=int1^.value
        else result:=P_intLiteral(list1^.value(index))^.value;
      inc(result);
    end;

  FUNCTION safeLen(index:longint):longint;
    begin
      if arg2^.literalType=lt_int
        then result:=int2^.value
        else result:=P_intLiteral(list2^.value(index))^.value;
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
        result:=newStringLiteral(copy(str0^.value,
                                      P_intLiteral   (arg1)^.value+1,
                                      P_intLiteral   (arg2)^.value))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do
          P_listLiteral(result)^.appendString(
              copy(safeString(i),
                   safeStart(i),
                   safeLen(i)));
      end;
    end;
  end;

FUNCTION chars_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=chars_internal(arg0);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_stringList,lt_emptyList]) then begin
      result:=newListLiteral;
      for i:=0 to list0^.size-1 do P_listLiteral(result)^.append(chars_internal(list0^.value(i)),false);
    end else if (params=nil) or (params^.size=0) then begin
      result:=newListLiteral;
      for i:=0 to 255 do P_listLiteral(result)^.appendString(chr(i));
    end;
  end;

FUNCTION split_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
          splitters[i]:=P_stringLiteral(list1^.value(i))^.value;
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
          for i:=0 to P_listLiteral(p)^.size-1 do if context.adapters^.noErrors then
            P_listLiteral(result)^.append(splitRecurse(P_listLiteral(p)^.value(i)),false);
        end
       else result:=newErrorLiteralRaising('Cannot split non-string varables ',tokenLocation,context.adapters^);
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

FUNCTION join_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
    if (params<>nil) and ((params^.size=1) or (params^.size=2) and (arg1^.literalType=lt_string)) then begin
      if params^.size=2 then joiner:=str1^.value;
      if (arg0^.literalType in C_validListTypes) then begin
        if list0^.size=0 then exit(newStringLiteral(''));
        resTxt:=stringOfLit(list0^.value(0));
        for i:=1 to list0^.size-1 do
          resTxt:=resTxt+joiner+stringOfLit(list0^.value(i));
        result:=newStringLiteral(resTxt);
      end else if (arg0^.literalType in C_validScalarTypes) then
        result:=newStringLiteral(stringOfLit(arg0));
    end;
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
        for i:=0 to P_listLiteral(x)^.size-1 do if context.adapters^.noErrors then
          P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false);
        if result^.literalType = lt_listWithError then begin
          disposeLiteral(result);
          result:=newErrorLiteral;
        end;
      end;
      else result:=newErrorLiteralRaising('Cannot apply '+ID_MACRO+' to literal of type '+C_typeString[x^.literalType],tokenLocation,context.adapters^);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_list,lt_stringList,lt_string,lt_emptyList])
  then result:=recurse(arg0);
end}


FUNCTION trim_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=trim}
{$define ID_MACRO:='trim'}
STRINGLITERAL_ROUTINE;

FUNCTION trimLeft_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=trimLeft}
{$define ID_MACRO:='trimLeft'}
STRINGLITERAL_ROUTINE;

FUNCTION trimRight_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=trimRight}
{$define ID_MACRO:='trimRight'}
STRINGLITERAL_ROUTINE;

FUNCTION upper_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=upper}
{$define ID_MACRO:='upper'}
STRINGLITERAL_ROUTINE;

FUNCTION lower_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=lower}
{$define ID_MACRO:='lower'}
STRINGLITERAL_ROUTINE;

FUNCTION unbrace_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=unbrace}
{$define ID_MACRO:='unbrace'}
STRINGLITERAL_ROUTINE;

FUNCTION escape_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
      L:=arg1;
      if L^.literalType=lt_string then begin
        setLength(lookFor,1);
        lookFor[0]:=P_stringLiteral(L)^.value;
      end else begin
        setLength(lookFor,P_listLiteral(L)^.size);
        for i:=0 to length(lookFor)-1 do
          lookFor[i]:=P_stringLiteral(P_listLiteral(L)^.value(i))^.value;
      end;
      L:=arg2;
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
    if arg0^.literalType=lt_string then result:=newStringLiteral(modify(str0^.value))
    else begin
      result:=newListLiteral;
      for i:=0 to list0^.size-1 do
        P_listLiteral(result)^.appendString(modify(P_stringLiteral(list0^.value(i))^.value));
    end;
    setLength(lookFor,0);
    setLength(replaceBy,0);
  end;

FUNCTION replaceOne_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType in [lt_string,lt_stringList,lt_emptyList]) and
       (arg1^.literalType in [lt_string,lt_stringList,lt_emptyList]) and
       (arg2^.literalType in [lt_string,lt_stringList,lt_emptyList]) then begin
      result:=replace_one_or_all(params,false);
    end;
  end;

FUNCTION replace_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType in [lt_string,lt_stringList,lt_emptyList]) and
       (arg1^.literalType in [lt_string,lt_stringList,lt_emptyList]) and
       (arg2^.literalType in [lt_string,lt_stringList,lt_emptyList]) then begin
      result:=replace_one_or_all(params,true);
    end;
  end;

FUNCTION repeat_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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

FUNCTION clean_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  //clean(input,whitelist,instead)
  VAR whiteList:charSet;
      instead:char;
      tmp:ansistring;
      i:longint;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType in [lt_string,lt_stringList,lt_emptyList]) and
       (arg1^.literalType=lt_stringList) and
       (arg2^.literalType=lt_string) and
       (length(str2^.value)=1) then begin
      whiteList:=[];
      for i:=0 to list1^.size-1 do begin
        tmp:=P_stringLiteral(list1^.value(i))^.value;
        if length(tmp)=1 then include(whiteList,tmp[1])
        else begin
          context.adapters^.raiseError('Built in function clean expects a list of single-character strings as whitelist (second argument)',tokenLocation);
          exit(nil);
        end;
      end;
      instead:=str2^.value[1];
      if arg0^.literalType=lt_string then begin
        result:=newStringLiteral(cleanString(str0^.value,whiteList,instead));
      end else begin
        result:=newListLiteral;
        for i:=0 to list0^.size-1 do
        P_listLiteral(result)^.appendString(cleanString(P_stringLiteral(list0^.value(i))^.value,whiteList,instead));
      end;
    end;
  end;

FUNCTION tokenSplit_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR language:string='MNH';
      stringToSplit:ansistring;
      tokens:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      if (arg1^.literalType=lt_string)
      then language:=str1^.value
      else exit(nil);
    end;
    if (params<>nil) and (params^.size>=1) and
       (arg0^.literalType=lt_string) then begin
      stringToSplit:=str0^.value;
      tokens:=tokenSplit(stringToSplit,language);
      result:=newListLiteral;
      for i:=0 to length(tokens)-1 do result:=P_listLiteral(result)^.appendString(tokens[i]);
    end;
  end;

FUNCTION reverseString_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION rev(CONST L:P_literal):ansistring;
    FUNCTION revShort(CONST s:shortString):shortString; inline;
      VAR i:longint;
      begin
        result:=s;
        for i:=1 to length(s) do result[i]:=s[length(s)+1-i];
      end;

    VAR s:ansistring;
        i:longint;
    begin
      s:=P_stringLiteral(L)^.value;
      if length(s)<=255 then exit(revShort(s));
      result:='';
      for i:=length(s) downto 1 do result:=result+s[i];
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_string: result:=newStringLiteral(rev(arg0));
      lt_stringList,lt_emptyList: begin
        result:=newListLiteral;
        for i:=0 to list0^.size-1 do
          P_listLiteral(result)^.appendString(rev(list0^.value(i)));
      end;
    end;
  end;

{$define diffStatOrDiff_impl:=
  VAR aHashes,bHashes:PInteger;
      aLen,bLen:integer;
      diff:TDiff;
      i:longint;
      {$ifdef withEditScript}
      comp:P_listLiteral;
      {$endif}
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       ((arg0^.literalType in [lt_stringList,lt_emptyList]) and
        (arg1^.literalType in [lt_stringList,lt_emptyList]) or
        (arg0^.literalType=lt_string) and
        (arg1^.literalType=lt_string)) then begin
      diff.create();
      if (arg0^.literalType in [lt_stringList,lt_emptyList]) and
         (arg1^.literalType in [lt_stringList,lt_emptyList]) then begin
        with list0^ do begin
          aLen:=size;
          getMem(aHashes,aLen*sizeOf(integer));
          for i:=0 to aLen-1 do aHashes[i]:=value(i)^.hash;
        end;
        with list1^ do begin
          bLen:=size;
          getMem(bHashes,bLen*sizeOf(integer));
          for i:=0 to bLen-1 do bHashes[i]:=value(i)^.hash;
        end;
        diff.execute(aHashes,bHashes,aLen,bLen);
        freeMem(aHashes,aLen*sizeOf(integer));
        freeMem(bHashes,bLen*sizeOf(integer));
      end else if (arg0^.literalType=lt_string) and
                  (arg1^.literalType=lt_string) then begin
        diff.execute(str0^.value,
                     str1^.value);
      end;

      result:=newListLiteral^
             .append(newListLiteral^
                    .appendString('adds')^
                    .appendInt(diff.DiffStats.adds),false)^
             .append(newListLiteral^
                    .appendString('deletes')^
                    .appendInt(diff.DiffStats.deletes),false)^
             .append(newListLiteral^
                    .appendString('matches')^
                    .appendInt(diff.DiffStats.matches),false)^
             .append(newListLiteral^
                    .appendString('modifies')^
                    .appendInt(diff.DiffStats.modifies),false);
      {$ifdef withEditScript}
      comp:=newListLiteral;
      for i:=0 to diff.count-1 do begin
        case diff.Compares[i].kind of
          ckNone:   comp^.append(newListLiteral^.appendString('.')^.appendInt(diff.Compares[i].oldIndex1)^.appendInt(diff.Compares[i].oldIndex2),false);
          ckAdd:    comp^.append(newListLiteral^.appendString('+')^.appendInt(diff.Compares[i].oldIndex1)^.appendInt(diff.Compares[i].oldIndex2),false);
          ckDelete: comp^.append(newListLiteral^.appendString('-')^.appendInt(diff.Compares[i].oldIndex1)^.appendInt(diff.Compares[i].oldIndex2),false);
          ckModify: comp^.append(newListLiteral^.appendString('M')^.appendInt(diff.Compares[i].oldIndex1)^.appendInt(diff.Compares[i].oldIndex2),false);
        end;
      end;
      P_listLiteral(result)^.append(newListLiteral^.appendString('edit')^.append(comp,false),false);
      {$endif}
      diff.destroy;
    end;
  end}
{$define withEditScript}
FUNCTION diff_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
diffStatOrDiff_impl;
{$undef withEditScript}
FUNCTION diffStats_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
diffStatOrDiff_impl;

FUNCTION sysToUtf8_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_string: result:=newStringLiteral(WinCPToUTF8(str0^.value));
      lt_stringList: begin
        result:=newListLiteral;
        with list0^ do for i:=0 to size-1 do P_listLiteral(result)^.appendString(WinCPToUTF8(P_stringLiteral(value(i))^.value));
      end;
      lt_emptyList: begin
        result:=arg0;
        result^.rereference;
      end;
    end;
  end;

FUNCTION utf8ToSys_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_string: result:=newStringLiteral(UTF8ToWinCP(str0^.value));
      lt_stringList: begin
        result:=newListLiteral;
        with list0^ do for i:=0 to size-1 do P_listLiteral(result)^.appendString(UTF8ToWinCP(P_stringLiteral(value(i))^.value));
      end;
      lt_emptyList: begin
        result:=arg0;
        result^.rereference;
      end;
    end;
  end;

FUNCTION isUtf8_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(isUtf8Encoded(str0^.value));
  end;

FUNCTION isAscii_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(isAsciiEncoded(str0^.value));
  end;

FUNCTION compress_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newStringLiteral(compressString(str0^.value,0))
    else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_int)
      then result:=newStringLiteral(compressString(str0^.value,int1^.value))
  end;

FUNCTION decompress_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
  registerRule(STRINGS_NAMESPACE,'clean',@clean_impl,'clean(s,whiteList:stringList,instead:string);#Replaces all characters in s which are not in whitelist by instead. Whitelist must be a list of characters, instead must be a character');
  registerRule(STRINGS_NAMESPACE,'tokenSplit',@tokenSplit_impl,'tokenSplit(S:string);#tokenSplit(S:string,language:string);#Returns a list of strings from S for a given language#Languages: <code>MNH, Pascal, Java</code>');
  registerRule(STRINGS_NAMESPACE,'reverseString',@reverseString_impl,'reverseString(S:string);#reverseString(S:stringList);#Returns returns S reversed');
  registerRule(STRINGS_NAMESPACE,'diff',@diff_impl,'diff(A,B);#Shows diff statistics and edit script for strings A and B or string lists A and B');
  registerRule(STRINGS_NAMESPACE,'diffStats',@diffStats_impl,'diffStats(A,B);#Shows diff statistics for strings A and B or string lists A and B');
  registerRule(STRINGS_NAMESPACE,'utf8ToSys',@utf8ToSys_impl,'utf8ToSys(S);#Converts a string or a string list from UTF-8 to the system encoding');
  registerRule(STRINGS_NAMESPACE,'sysToUtf8',@sysToUtf8_impl,'sysToUtf8(S);#Converts a string or a string list from the system encoding to UTF-8');
  registerRule(STRINGS_NAMESPACE,'isUtf8',@isUtf8_impl,'isUtf8(S:string);#Returns true if S is UTF8 encoded and false otherwise');
  registerRule(STRINGS_NAMESPACE,'isAscii',@isAscii_impl,'isAscii(S:string);#Returns true if S is ASCII encoded and false otherwise');
  registerRule(STRINGS_NAMESPACE,'compress',@compress_impl,'compress(S:string);#Returns a compressed version of S#compress(S:string,k:int);#As above but with a specified algorithm:#  1: deflate#  2: huffman with default model#  3: huffman with another model#  other: try out algorithms and return the shortest representation#  The first character of the result indicates the algorithm used');
  registerRule(STRINGS_NAMESPACE,'decompress',@decompress_impl,'decompress(S:string);#Returns an uncompressed version of S');
end.
