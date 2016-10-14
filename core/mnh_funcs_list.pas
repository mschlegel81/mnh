UNIT mnh_funcs_list;
INTERFACE
{$WARN 5024 OFF}
USES mnh_basicTypes,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,mnh_contexts;
VAR BUILTIN_HEAD,BUILTIN_GET:P_intFuncCallback;

FUNCTION flatten_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
IMPLEMENTATION
VAR builtinLocation_sort:T_identifiedInternalFunction;
{$i mnh_func_defines.inc}

FUNCTION add_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_validListTypes) then begin
      if arg0^.getReferenceCount=1 then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      lResult^.append(arg1,true);
    end;
  end;

{$define SUB_LIST_IMPL:=
begin
  result:=nil;
  if (params<>nil) and (params^.size>=1) then begin
    if (arg0^.literalType in C_validListTypes) then begin
      if      (params^.size=1) then result:=list0^.CALL_MACRO
      else if (params^.size=2) and (arg1^.literalType=lt_int) then result:=list0^.CALL_MACRO(int1^.value);
    end else SCALAR_FALLBACK;
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

FUNCTION sort_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes) then begin
      if (arg0^.getReferenceCount=1) then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      lResult^.sort;
    end else if (params<>nil) and (params^.size=2)
            and (arg0^.literalType in C_validListTypes)
            and (arg1^.literalType=lt_expression) then begin
      if (arg0^.getReferenceCount=1) then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      context.callStackPush(tokenLocation,@builtinLocation_sort,params,nil);
      lResult^.customSort(P_expressionLiteral(arg1),tokenLocation,context.adapters^);
      context.callStackPop();
    end else if (params<>nil) and (params^.size=2)
            and (arg0^.literalType in C_validListTypes)
            and (arg1^.literalType=lt_int) then begin
      if (arg0^.getReferenceCount=1) then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      lResult^.sortBySubIndex(int1^.value,tokenLocation,context.adapters^);
    end
  end;

FUNCTION sortPerm_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes)
    then result:=list0^.sortPerm;
  end;

FUNCTION unique_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes) then begin
      if (arg0^.getReferenceCount=1) then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      lResult^.unique;
    end;
  end;

FUNCTION mapOf_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_emptyList,lt_keyValueList]) then begin
      if (arg0^.getReferenceCount=1) then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      lResult^.toKeyValueList;
    end;
  end;

FUNCTION transpose_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes)
    then result:=list0^.transpose;
  end;

FUNCTION getElementFreqency intFuncSignature;
  FUNCTION pair(CONST count:longint; CONST value:P_literal):P_listLiteral;
    begin result:=newListLiteral^.appendInt(count)^.append(value,true); end;

  TYPE T_freqMap=specialize G_literalKeyMap<longint>;
  VAR freqMap:T_freqMap;
      freqList:T_freqMap.KEY_VALUE_LIST;
      i:longint;
      list:P_listLiteral;

  begin
    if not((params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes)) then exit(nil);
    list:=P_listLiteral(arg0);

    freqMap.create;
    for i:=0 to list^.size-1 do freqMap.put(list^.value(i),freqMap.get(list^.value(i),0)+1);

    if not(context.adapters^.noErrors) then begin
      freqMap.destroy;
      exit(newErrorLiteral);
    end;

    freqList:=freqMap.keyValueList;
    result:=newListLiteral;
    for i:=0 to length(freqList)-1 do lResult^.append(pair(freqList[i].value,freqList[i].key),false);

    if not(context.adapters^.noErrors) then begin
      freqMap.destroy;
      exit(result);
    end;

    lResult^.sortBySubIndex(1,tokenLocation,context.adapters^);
    freqMap.destroy;
  end;

FUNCTION setUnion intFuncSignature;
  TYPE T_set=specialize G_literalKeyMap<boolean>;
  VAR resultSet:T_set;
      resultList:T_set.KEY_VALUE_LIST;
      i,j:longint;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^.value(i)^.literalType in C_validListTypes) then exit(nil);
    if params^.size=1 then begin
      result:=arg0;
      result^.rereference;
    end;

    resultSet.create;
    for i:=0 to params^.size-1 do
      with P_listLiteral(params^.value(i))^ do
        for j:=0 to size-1 do resultSet.put(value(j),true);

    result:=newListLiteral;
    resultList:=resultSet.keyValueList;
    for i:=0 to length(resultList)-1 do lResult^.append(resultList[i].key,true);
    lResult^.unique;
    setLength(resultList,0);
    resultSet.destroy;
  end;

FUNCTION setIntersect intFuncSignature;
  TYPE T_set=specialize G_literalKeyMap<longint>;
  VAR resultSet:T_set;
      resultList:T_set.KEY_VALUE_LIST;
      i,j:longint;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^.value(i)^.literalType in C_validListTypes) then exit(nil);
    if params^.size=1 then begin
      result:=arg0;
      result^.rereference;
    end;

    resultSet.create;
    for i:=0 to params^.size-1 do
      with P_listLiteral(params^.value(i))^ do
        for j:=0 to size-1 do if resultSet.get(value(j),0)=i then resultSet.put(value(j),i+1);

    i:=params^.size;
    resultList:=resultSet.keyValueList;
    result:=newListLiteral;
    for j:=0 to length(resultList)-1 do if resultList[j].value=i then
      lResult^.append(resultList[j].key,true);
    lResult^.unique;
    setLength(resultList,0);
    resultSet.destroy;
  end;

FUNCTION setMinus intFuncSignature;
  VAR rhsSet:specialize G_literalKeyMap<boolean>;
      i:longint;
      LHS,RHS:P_listLiteral;
  begin
    if not((params<>nil) and
           (params^.size=2) and
           (arg0^.literalType in C_validListTypes) and
           (arg1^.literalType in C_validListTypes))
    then exit(nil);

    LHS:=P_listLiteral(arg0);
    RHS:=P_listLiteral(arg1);
    rhsSet.create;
    for i:=0 to RHS^.size-1 do rhsSet.put(RHS^.value(i),true);
    result:=newListLiteral;
    for i:=0 to LHS^.size-1 do
      if not(rhsSet.get(LHS^.value(i),false))
      then lResult^.append(LHS^.value(i),true);
    lResult^.unique;
    rhsSet.destroy;
  end;

FUNCTION flatten_imp intFuncSignature;
  PROCEDURE recurse_flatten(CONST L:P_listLiteral);
    VAR i:longint;
    begin
      for i:=0 to L^.size-1 do
      if L^.value(i)^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression]
      then lResult^.append(L^.value(i),true)
      else recurse_flatten(P_listLiteral(L^.value(i)));
    end;

  begin
    if params<>nil then begin
      result:=newListLiteral;
      recurse_flatten(params);
    end else result:=nil;
  end;

FUNCTION unflatten_imp intFuncSignature;
  VAR openerCloser:array of array[0..1] of P_literal;
      i:longint;
      iterIdx:longint=0;
      failed:boolean=false;

  FUNCTION unflatten(CONST stopAt:P_literal):P_listLiteral;
    FUNCTION matchingCloser(CONST p:P_literal):P_literal;
      VAR k:longint;
      begin
        for k:=0 to length(openerCloser)-1 do
        if openerCloser[k,0]^.equals(p) then exit(openerCloser[k,1]);
        result:=nil;
      end;

    VAR nextCloser,nextLiteral:P_literal;
    begin
      result:=newListLiteral();
      while (iterIdx<list0^.size) do begin
        nextLiteral:=list0^.value(iterIdx);
        inc(iterIdx);
        if (stopAt<>nil) and (stopAt^.equals(nextLiteral)) then exit(result);
        nextCloser:=matchingCloser(nextLiteral);
        if nextCloser=nil
        then result^.append(nextLiteral,true)
        else result^.append(unflatten(nextCloser),false);
      end;
      if stopAt<>nil then failed:=true;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType in C_matchingTypes[tt_typeCheckFlatList]) and
       ((arg1^.literalType in C_matchingTypes[tt_typeCheckFlatList]) and (list1^.size>0) and
        (arg2^.literalType in C_matchingTypes[tt_typeCheckFlatList]) and (list2^.size=list1^.size)
      or (arg1^.literalType in C_matchingTypes[tt_typeCheckScalar]) and
         (arg2^.literalType in C_matchingTypes[tt_typeCheckScalar])) then begin
      if (list0^.size<=0) then begin
        arg0^.rereference;
        exit(arg0);
      end;
      if (arg1^.literalType in C_matchingTypes[tt_typeCheckFlatList]) then begin
        setLength(openerCloser,list1^.size);
        for i:=0 to list1^.size-1 do begin
          openerCloser[i,0]:=list1^.value(i);
          openerCloser[i,1]:=list2^.value(i);
        end;
      end else begin
        setLength(openerCloser,1);
        openerCloser[0,0]:=arg1;
        openerCloser[0,1]:=arg2;
      end;
      result:=unflatten(nil);
      if failed then begin
        disposeLiteral(result);
        context.adapters^.raiseError('Error in unflatten: unbalanced paranthesis found',tokenLocation);
      end;
      setLength(openerCloser,0);
    end;
  end;

FUNCTION size_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_error..  lt_expression: result:=newIntLiteral(1);
        lt_list..lt_listWithError: result:=newIntLiteral(list0^.size);
      end;
    end;
  end;

FUNCTION trueCount_impl intFuncSignature;
  VAR B:P_literal;
      i,c:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      B:=arg0;
      case B^.literalType of
        lt_boolean: if P_boolLiteral(B)^.value then exit(newIntLiteral(1)) else exit(newIntLiteral(0));
        lt_booleanList: begin
          c:=0;
          for i:=0 to P_listLiteral(B)^.size-1 do if P_boolLiteral(P_listLiteral(B)^.value(i))^.value then inc(c);
          exit(newIntLiteral(c));
        end;
        lt_emptyList: exit(newIntLiteral(0));
      end;
    end;
  end;

FUNCTION reverseList_impl intFuncSignature;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes) then begin
      result:=newListLiteral;
      for i:=list0^.size-1 downto 0 do
        lResult^.append(list0^.value(i),true);
    end;
  end;

FUNCTION mapPut_imp intFuncSignature;
  begin
    result:=mapPut(params,tokenLocation,context.adapters^);
  end;

FUNCTION mapGet_imp intFuncSignature;
  begin
    result:=mapGet(params,tokenLocation,context.adapters^);
  end;

FUNCTION mapDrop_imp intFuncSignature;
  begin
    result:=mapDrop(params,tokenLocation,context.adapters^);
  end;

FUNCTION get_imp intFuncSignature;
  VAR tmpPar:T_listLiteral;
      i:longint;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_validListTypes)
    then result:=list0^.get(arg1,tokenLocation,context.adapters^)
    else if (params<>nil) and (params^.size>=2) and (arg0^.literalType in C_validListTypes) then begin
      tmpPar.create;
      tmpPar.append(arg0,true)^
            .append(arg1,true);
      result:=get_imp(@tmpPar,tokenLocation,context);
      tmpPar.destroy;
      if result^.literalType=lt_void then exit(result);
      if result<>nil then begin
        tmpPar.create;
        tmpPar.append(result,false);
        for i:=2 to params^.size-1 do tmpPar.append(params^.value(i),true);
        result:=get_imp(@tmpPar,tokenLocation,context);
        tmpPar.destroy;
      end;
    end
    else result:=nil;
  end;

FUNCTION getInner_imp intFuncSignature;
  VAR tmpPar:T_listLiteral;
      i:longint;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_validListTypes)
    then result:=list0^.getInner(arg1,tokenLocation,context.adapters^)
    else if (params<>nil) and (params^.size>=2) and (arg0^.literalType in C_validListTypes) then begin
      tmpPar.create;
      tmpPar.append(arg0,true)^
            .append(arg1,true);
      result:=getInner_imp(@tmpPar,tokenLocation,context);
      tmpPar.destroy;
      if result<>nil then begin
        tmpPar.create;
        tmpPar.append(result,false);
        for i:=2 to params^.size-1 do tmpPar.append(params^.value(i),true);
        result:=getInner_imp(@tmpPar,tokenLocation,context);
        tmpPar.destroy;
      end;
    end
    else result:=nil;
  end;

FUNCTION indexOf_impl intFuncSignature;
  VAR i:longint;
      R:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_booleanList,lt_emptyList]) then begin
      R:=newListLiteral;
      with list0^ do for i:=0 to size-1 do if P_boolLiteral(value(i))^.value
        then R^.appendInt(i);
      result:=R;
    end;
  end;

INITIALIZATION
  //Functions on lists:
  registerRule(LIST_NAMESPACE,'add',@add_imp,'add(L,e);//Returns L with the new element e appended');
  BUILTIN_HEAD:=
  registerRule(LIST_NAMESPACE,'head',@head_imp,'head(L);//Returns the first element of list L or [] if L is empty#head(L,k);//Returns the first min(k,size(L)) elements of L or [] if L is empty');
  registerRule(LIST_NAMESPACE,'tail',@tail_imp,'tail(L);//Returns list L without the first element#tail(L,k);//Returns L without the first k elements');
  registerRule(LIST_NAMESPACE,'leading',@leading_imp,'leading(L);//Returns L without the last element or [] if L is empty#leading(L,k);//Returns L without the last k elements or [] if L is empty');
  registerRule(LIST_NAMESPACE,'trailing',@trailing_imp,'trailing(L);//Returns the last element of L#trailing(L,k);//Returns the last k elements of L');
  builtinLocation_sort.create(LIST_NAMESPACE,'sort');
  registerRule(LIST_NAMESPACE,'sort',@sort_imp,'sort(L);//Returns list L sorted ascending (using fallbacks for uncomparable types)#'+
                                               'sort(L,leqExpression:expression);//Returns L sorted using the custom binary expression, interpreted as "is lesser or equal"#'+
                                               'sort(L,innerIndex:int);//Returns L sorted by given inner index');
  registerRule(LIST_NAMESPACE,'sortPerm',@sortPerm_imp,'sortPerm(L);//Returns indexes I so that L%I==sort(L)');
  registerRule(LIST_NAMESPACE,'unique',@unique_imp,'unique(L:list);//Returns list L without duplicates and enhanced for faster lookup');
  registerRule(LIST_NAMESPACE,'toMap',@mapOf_imp,'toMap(L:keyValueList);//Returns key L without duplicate keys and enhanced for faster lookup');
  registerRule(LIST_NAMESPACE,'elementFrequency',@getElementFreqency,'elementFrequency(L);//Returns a list of pairs [count,e] containing distinct elements e of L and their respective frequencies');
  registerRule(LIST_NAMESPACE,'transpose',@transpose_imp,'transpose(L);//Returns list L transposed.');
  registerRule(LIST_NAMESPACE,'union',@setUnion,'union(A,...);//Returns a union of all given parameters. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'intersect',@setIntersect,'intersect(A,...);//Returns an intersection of all given parameters. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'minus',@setMinus,'minus(A,B);//Returns the asymmetric set difference of A and B. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'flatten',@flatten_imp,'flatten(L,...);//Returns all parameters as a flat list.');
  registerRule(LIST_NAMESPACE,'unflatten',@unflatten_imp,'unflatten(L,openers,closers);//Returns L expanded to a nexted list using openers and closers.');
  registerRule(LIST_NAMESPACE,'size',@size_imp,'size(L);//Returns the number of elements in list L');
  registerRule(LIST_NAMESPACE,'trueCount',@trueCount_impl,'trueCount(B:booleanList);//Returns the number of true values in B');
  registerRule(LIST_NAMESPACE,'reverseList',@reverseList_impl,'reverse(L:list);//Returns L reversed');
  BUILTIN_GET:=
  registerRule(LIST_NAMESPACE,'get',@get_imp,'get(L:list,index);');
  registerRule(LIST_NAMESPACE,'getInner',@getInner_imp,'getInner(L:list,index);');
  registerRule(LIST_NAMESPACE,'mapPut',@mapPut_imp,'mapPut(L:keyValueList,key:string,value);//Returns L with an additional or modified key-value-pair [key,value].');
  registerRule(LIST_NAMESPACE,'mapGet',@mapGet_imp,'mapGet(L:keyValueList,key:string);//Returns the element with matching key or the empty list if no such element was found.#'+
                                            'mapGet(L:keyValueList,key:string,fallback);//Returns the element with matching key or fallback if no such element was found.');
  registerRule(LIST_NAMESPACE,'drop',@mapDrop_imp,'drop(L:keyValueList,key:string);//Returns L without [key,?].');
  registerRule(LIST_NAMESPACE,'indexOf',@indexOf_impl,'indexOf(B:booleanList);//Returns the indexes for which B is true.');

FINALIZATION
  builtinLocation_sort.destroy;
end.
