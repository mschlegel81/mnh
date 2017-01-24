UNIT mnh_funcs_list;
INTERFACE
{$WARN 5024 OFF}
USES mnh_basicTypes,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,mnh_contexts;
VAR BUILTIN_HEAD,BUILTIN_GET,BUILTIN_TAIL:P_intFuncCallback;

FUNCTION flatten_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
IMPLEMENTATION
VAR builtinLocation_sort:T_identifiedInternalFunction;
{$i mnh_func_defines.inc}
{$define SUB_LIST_IMPL:=
begin
  result:=nil;
  if (params<>nil) and (params^.size>=1) then begin
    if arg0^.literalType in C_listTypes then begin
      if      (params^.size=1) then result:=list0^.CALL_MACRO
      else if (params^.size=2) and (arg1^.literalType=lt_int) then result:=list0^.CALL_MACRO(int1^.value);
    end else if arg0^.literalType in C_scalarTypes then SCALAR_FALLBACK;
  end;
end}

FUNCTION head_imp intFuncSignature;
{$define CALL_MACRO:=head}
{$define SCALAR_FALLBACK:=begin result:=arg0; result^.rereference; end}
SUB_LIST_IMPL;

FUNCTION trailing_imp intFuncSignature;
{$define CALL_MACRO:=trailing}
SUB_LIST_IMPL;

FUNCTION tail_imp intFuncSignature;
{$define CALL_MACRO:=tail}
{$define SCALAR_FALLBACK:=result:=newListLiteral}
SUB_LIST_IMPL;

FUNCTION leading_imp intFuncSignature;
{$define CALL_MACRO:=leading}
SUB_LIST_IMPL;

{$undef SUB_LIST_IMPL}
{$undef CALL_MACRO}
{$undef SCALAR_FALLBACK}

{$define cloneOrCopyList0:=if arg0^.getReferenceCount=1 then result:=arg0^.rereferenced else result:=list0^.clone}

FUNCTION sort_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_compoundTypes) then begin
      if arg0^.literalType in C_listTypes then begin
        cloneOrCopyList0;
        P_listLiteral(result)^.sort;
      end else
      if arg0^.literalType in C_setTypes
      then result:=set0^.getManifestation^.rereferenced
      else result:=map0^.getManifestation^.rereferenced;
    end else if (params<>nil) and (params^.size=2)
            and (arg0^.literalType in C_compoundTypes)
            and (arg1^.literalType=lt_expression) then begin
      if arg0^.literalType in C_listTypes then cloneOrCopyList0
      else if arg0^.literalType in C_setTypes
      then result:=set0^.getManifestation^.clone
      else result:=map0^.getManifestation^.clone;
      context.callStackPush(tokenLocation,@builtinLocation_sort,params,nil);
      P_listLiteral(result)^.customSort(P_expressionLiteral(arg1),tokenLocation,context.adapters^);
      context.callStackPop();
    end else if (params<>nil) and (params^.size=2)
            and (arg0^.literalType in C_compoundTypes)
            and (arg1^.literalType=lt_int) then begin
      if arg0^.literalType in C_listTypes then cloneOrCopyList0
      else if arg0^.literalType in C_setTypes
      then result:=set0^.getManifestation^.clone
      else result:=map0^.getManifestation^.clone;
      P_listLiteral(result)^.sortBySubIndex(int1^.value,tokenLocation,context.adapters^);
    end
  end;

FUNCTION sortPerm_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_listTypes)
    then result:=list0^.sortPerm;
  end;

FUNCTION unique_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (arg0^.literalType in C_listTypes) then begin
        cloneOrCopyList0;
        P_listLiteral(result)^.unique;
      end else if (arg0^.literalType in C_setTypes) then exit(arg0^.rereferenced);
    end;
  end;

FUNCTION transpose_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_listTypes)
    then result:=list0^.transpose(arg1);
  end;

FUNCTION getElementFreqency intFuncSignature;
  TYPE T_freqMap=specialize G_literalKeyMap<longint>;
  VAR freqMap:T_freqMap;
      freqList:T_freqMap.KEY_VALUE_LIST;
      i:longint;
      list:P_listLiteral;

  begin
    if (params=nil) or (params^.size<>1) then exit(nil);
    if (arg0^.literalType in C_setTypes) or (arg0^.literalType in C_mapTypes) then begin
      result:=newMapLiteral;
      for i:=0 to set0^.size-1 do P_mapLiteral(result)^.put(set0^.value[i]^.rereferenced,newIntLiteral(1),false);
      exit(result);
    end;
    if not(arg0^.literalType in C_listTypes) then exit(nil);
    list:=list0;

    freqMap.create;
    for i:=0 to list^.size-1 do freqMap.put(list^.value[i],
                                freqMap.get(list^.value[i],0)+1);
    if not(context.adapters^.noErrors) then begin
      freqMap.destroy;
      exit(nil);
    end;
    freqList:=freqMap.keyValueList;
    result:=newMapLiteral;
    for i:=0 to length(freqList)-1 do P_mapLiteral(result)^.put(freqList[i].key^.rereferenced,freqList[i].value,false);
    freqMap.destroy;
  end;

FUNCTION flatten_imp intFuncSignature;
  PROCEDURE recurse_flatten(CONST L:P_compoundLiteral);
    VAR i:longint;
    begin
      for i:=0 to L^.size-1 do
      if L^[i]^.literalType in C_compoundTypes
      then recurse_flatten(P_compoundLiteral(L^[i]))
      else listResult^.append(L^.value[i],true);
    end;

  begin
    if params<>nil then begin
      result:=newListLiteral;
      recurse_flatten(params);
    end else result:=nil;
  end;

FUNCTION size_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType in C_compoundTypes
      then result:=newIntLiteral(compound0^.size)
      else result:=newIntLiteral(1);
    end;
  end;

FUNCTION trueCount_impl intFuncSignature;
  VAR i,c:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_boolean: if bool0^.value then exit(newIntLiteral(1)) else exit(newIntLiteral(0));
        lt_booleanList,lt_booleanSet: begin
          c:=0;
          for i:=0 to compound0^.size-1 do if P_boolLiteral(compound0^[i])^.value then inc(c);
          exit(newIntLiteral(c));
        end;
        lt_emptyList,lt_emptySet: exit(newIntLiteral(0));
      end;
    end;
  end;

FUNCTION reverseList_impl intFuncSignature;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_listTypes) then begin
      result:=newListLiteral;
      for i:=list0^.size-1 downto 0 do listResult^.append(list0^.value[i],true);
    end;
  end;

FUNCTION setUnion_imp     intFuncSignature; begin result:=setUnion    (params); end;
FUNCTION setIntersect_imp intFuncSignature; begin result:=setIntersect(params); end;
FUNCTION setMinus_imp     intFuncSignature; begin result:=setMinus    (params); end;

FUNCTION get_imp intFuncSignature;
  VAR tmpPar:T_listLiteral;
      i:longint;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_compoundTypes)
    then result:=compound0^.get(arg1)
    else if (params<>nil) and (params^.size>=2) and (arg0^.literalType in C_compoundTypes) then begin
      tmpPar.create(2);
      tmpPar.append(arg0,true)^
            .append(arg1,true);
      result:=get_imp(@tmpPar,tokenLocation,context);
      tmpPar.destroy;
      if (result=nil) or (result^.literalType=lt_void) then exit(result);
      tmpPar.create(params^.size-1);
      tmpPar.append(result,false);
      for i:=2 to params^.size-1 do tmpPar.append(params^[i],true);
      result:=get_imp(@tmpPar,tokenLocation,context);
      tmpPar.destroy;
    end
    else result:=nil;
  end;

FUNCTION getAll_imp intFuncSignature;
  VAR iter:T_arrayOfLiteral;
      sub:P_literal;
      got:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_compoundTypes) and (arg1^.literalType in C_compoundTypes) then begin
      if arg1^.literalType in C_listTypes
      then result:=newListLiteral
      else result:=newSetLiteral;
      iter:=compound1^.iteratableList;
      for sub in iter do if result<>nil then begin
        got:=compound0^.get(sub);
        if got<>nil then begin
          if got^.literalType<>lt_void
          then collResult^.append(got,false)
          else disposeLiteral(got);
        end else disposeLiteral(result);
      end;
      disposeLiteral(iter);
    end else if (params<>nil) and (params^.size=3) and (arg0^.literalType in C_compoundTypes) and (arg1^.literalType in C_listTypes) and (arg2^.literalType in C_listTypes) and (list1^.size=list2^.size) then begin
      result:=newListLiteral;
      for i:=0 to list1^.size-1 do if result<>nil then begin
        got:=compound0^.get(list1^[i]);
        if got<>nil then begin
          if got^.literalType<>lt_void
          then collResult^.append(got,false)
          else begin
            disposeLiteral(got);
            collResult^.append(list2^[i],true);
          end;
        end else disposeLiteral(result);
      end;
    end;
  end;

FUNCTION getInner_imp intFuncSignature;
  VAR tmpPar:T_listLiteral;
      i:longint;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_compoundTypes)
    then result:=compound0^.getInner(arg1)
    else if (params<>nil) and (params^.size>=2) and (arg0^.literalType in C_compoundTypes) then begin
      tmpPar.create(2);
      tmpPar.append(arg0,true)^
            .append(arg1,true);
      result:=getInner_imp(@tmpPar,tokenLocation,context);
      tmpPar.destroy;
      if result<>nil then begin
        tmpPar.create(params^.size-1);
        tmpPar.append(result,false);
        for i:=2 to params^.size-1 do tmpPar.append(params^[i],true);
        result:=getInner_imp(@tmpPar,tokenLocation,context);
        tmpPar.destroy;
      end;
    end
    else result:=nil;
  end;

FUNCTION indexOf_impl intFuncSignature;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_booleanList,lt_emptyList]) then begin
      result:=newListLiteral;
      with list0^ do for i:=0 to size-1 do if P_boolLiteral(list0^[i])^.value then listResult^.appendInt(i);
    end;
  end;

INITIALIZATION
  //Functions on lists:
  BUILTIN_HEAD:=
  registerRule(LIST_NAMESPACE,'head'    ,@head_imp    ,true,ak_variadic_1,'head(L);//Returns the first element of list L or [] if L is empty#head(L,k);//Returns the first min(k,size(L)) elements of L or [] if L is empty');
  BUILTIN_TAIL:=
  registerRule(LIST_NAMESPACE,'tail'    ,@tail_imp    ,true,ak_variadic_1,'tail(L);//Returns list L without the first element#tail(L,k);//Returns L without the first k elements');
  registerRule(LIST_NAMESPACE,'leading' ,@leading_imp ,true,ak_variadic_1,'leading(L);//Returns L without the last element or [] if L is empty#leading(L,k);//Returns L without the last k elements or [] if L is empty');
  registerRule(LIST_NAMESPACE,'trailing',@trailing_imp,true,ak_variadic_1,'trailing(L);//Returns the last element of L#trailing(L,k);//Returns the last k elements of L');
  builtinLocation_sort.create(LIST_NAMESPACE,'sort');
  registerRule(LIST_NAMESPACE,'sort'    ,@sort_imp    ,true,ak_variadic_1,
                                               'sort(L);//Returns list L sorted ascending (using fallbacks for uncomparable types)#'+
                                               'sort(L,leqExpression:expression);//Returns L sorted using the custom binary expression, interpreted as "is lesser or equal"#'+
                                               'sort(L,innerIndex:int);//Returns L sorted by given inner index');
  registerRule(LIST_NAMESPACE,'sortPerm'        ,@sortPerm_imp      ,true,ak_unary     ,'sortPerm(L);//Returns indexes I so that L%I==sort(L)');
  registerRule(LIST_NAMESPACE,'unique'          ,@unique_imp        ,true,ak_unary     ,'unique(L:list);//Returns list L without duplicates and enhanced for faster lookup');
  registerRule(LIST_NAMESPACE,'elementFrequency',@getElementFreqency,true,ak_unary     ,'elementFrequency(L);//Returns a list of pairs [count,e] containing distinct elements e of L and their respective frequencies');
  registerRule(LIST_NAMESPACE,'transpose'       ,@transpose_imp     ,true,ak_unary     ,'transpose(L,filler);//Returns list L transposed. If sub lists of L have different lengths, filler is used.');
  registerRule(LIST_NAMESPACE,'union'           ,@setUnion_imp      ,true,ak_variadic_1,'union(A,...);//Returns a union of all given parameters. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'intersect'       ,@setIntersect_imp  ,true,ak_variadic_1,'intersect(A,...);//Returns an intersection of all given parameters. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'minus'           ,@setMinus_imp      ,true,ak_binary    ,'minus(A,B);//Returns the asymmetric set difference of A and B. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'flatten'         ,@flatten_imp       ,true,ak_variadic  ,'flatten(L,...);//Returns all parameters as a flat list.');
  registerRule(LIST_NAMESPACE,'size'            ,@size_imp          ,true,ak_unary     ,'size(L);//Returns the number of elements in list L');
  registerRule(LIST_NAMESPACE,'trueCount'       ,@trueCount_impl    ,true,ak_unary     ,'trueCount(B:booleanList);//Returns the number of true values in B');
  registerRule(LIST_NAMESPACE,'reverseList'     ,@reverseList_impl  ,true,ak_unary     ,'reverseList(L:list);//Returns L reversed');
  BUILTIN_GET:=
  registerRule(LIST_NAMESPACE,'get'     ,@get_imp     ,true,ak_variadic_2,'get(L,accessor);//Returns elements of list, set or map L by accessor');
  registerRule(LIST_NAMESPACE,'getAll'  ,@getAll_imp  ,true,ak_binary    ,'getAll(L,accessors);//Returns elements of list, set or map L by collection of accessors#getAll(L,accessors:list,fallback:list);//Returns elements of list, set or map L by collection of accessors.#//If no such element is found, the respective fallback entry is used.#//fallback must have the same size as accessors');
  registerRule(LIST_NAMESPACE,'getInner',@getInner_imp,true,ak_variadic_2,'getInner(L:list,index);');
  registerRule(LIST_NAMESPACE,'indexOf' ,@indexOf_impl,true,ak_unary     ,'indexOf(B:booleanList);//Returns the indexes for which B is true.');

FINALIZATION
  builtinLocation_sort.destroy;
end.
