UNIT mnh_funcs_list;
INTERFACE
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters,mnh_contexts;
FUNCTION get_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
IMPLEMENTATION
{$MACRO ON}
{$define list0:=P_listLiteral(params^.value(0))}
{$define arg0:=params^.value(0)}
{$define arg1:=params^.value(1)}

FUNCTION add_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_validListTypes) then begin
      if arg0^.getReferenceCount=1 then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      P_listLiteral(result)^.append(arg1,true,context.adapters^);
    end;
  end;

{$define SUB_LIST_IMPL:=
begin
  result:=nil;
  if (params<>nil) and (params^.size>=1) and (arg0^.literalType in C_validListTypes) then begin
    if      (params^.size=1) then result:=list0^.CALL_MACRO
    else if (params^.size=2) and (arg1^.literalType=lt_int) then result:=list0^.CALL_MACRO(P_intLiteral(arg1)^.value);
  end;
end}

FUNCTION head_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=head}
{$define ID_MACRO:='head'}
SUB_LIST_IMPL;

FUNCTION tail_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=tail}
{$define ID_MACRO:='tail'}
SUB_LIST_IMPL;

FUNCTION leading_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=leading}
{$define ID_MACRO:='leading'}
SUB_LIST_IMPL;

FUNCTION trailing_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
{$define CALL_MACRO:=trailing}
{$define ID_MACRO:='trailing'}
SUB_LIST_IMPL;

{$undef SUB_LIST_IMPL}

FUNCTION sort_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes) then begin
      if (arg0^.getReferenceCount=1) then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      P_listLiteral(result)^.sort;
    end else if (params<>nil) and (params^.size=2)
            and (arg0^.literalType in C_validListTypes)
            and (arg1^.literalType=lt_expression) then begin
      if (arg0^.getReferenceCount=1) then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      P_listLiteral(result)^.customSort(P_expressionLiteral(arg1),context.adapters^);
    end;
  end;

FUNCTION sortPerm_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes)
    then result:=list0^.sortPerm;
  end;

FUNCTION unique_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes) then begin
      if (arg0^.getReferenceCount=1) then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      P_listLiteral(result)^.unique;
    end;
  end;

FUNCTION mapOf_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_emptyList,lt_keyValueList]) then begin
      if (arg0^.getReferenceCount=1) then begin
        result:=arg0;
        result^.rereference;
      end else result:=list0^.clone;
      P_listLiteral(result)^.toKeyValueList;
    end;
  end;

FUNCTION getElementFreqency(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION pair(CONST count:longint; CONST value:P_literal):P_listLiteral;
    begin result:=newListLiteral^.appendInt(count)^.append(value,true,nullAdapter); end;

  TYPE T_freqMap=specialize G_literalKeyMap<longint>;
  VAR freqMap:T_freqMap;
      freqList:T_freqMap.KEY_VALUE_LIST;
      i:longint;
      list:P_listLiteral;

  begin
    if not((params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in C_validListTypes)) then exit(nil);
    list:=P_listLiteral(params^.value(0));

    freqMap.create;
    for i:=0 to list^.size-1 do freqMap.put(list^.value(i),freqMap.get(list^.value(i),0)+1);
    freqList:=freqMap.keyValueList;
    result:=newListLiteral;
    for i:=0 to length(freqList)-1 do P_listLiteral(result)^.append(pair(freqList[i].value,freqList[i].key),false,nullAdapter);
    freqMap.destroy;
  end;

FUNCTION setUnion(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  TYPE T_set=specialize G_literalKeyMap<boolean>;
  VAR resultSet:T_set;
      resultList:T_set.KEY_VALUE_LIST;
      i,j:longint;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^.value(i)^.literalType in C_validListTypes) then exit(nil);
    if params^.size=1 then begin
      result:=params^.value(0);
      result^.rereference;
    end;

    resultSet.create;
    for i:=0 to params^.size-1 do
      with P_listLiteral(params^.value(i))^ do
        for j:=0 to size-1 do resultSet.put(value(j),true);

    result:=newListLiteral;
    resultList:=resultSet.keyValueList;
    for i:=0 to length(resultList)-1 do P_listLiteral(result)^.append(resultList[i].key,true,nullAdapter);
    setLength(resultList,0);
    resultSet.destroy;
  end;

FUNCTION setIntersect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  TYPE T_set=specialize G_literalKeyMap<longint>;
  VAR resultSet:T_set;
      resultList:T_set.KEY_VALUE_LIST;
      i,j:longint;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^.value(i)^.literalType in C_validListTypes) then exit(nil);
    if params^.size=1 then begin
      result:=params^.value(0);
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
      P_listLiteral(result)^.append(resultList[j].key,true,nullAdapter);
    setLength(resultList,0);
    resultSet.destroy;
  end;

FUNCTION setMinus(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR rhsSet:specialize G_literalKeyMap<boolean>;
      i:longint;
      LHS,RHS:P_listLiteral;
  begin
    if not((params<>nil) and
           (params^.size=2) and
           (params^.value(0)^.literalType in C_validListTypes) and
           (params^.value(1)^.literalType in C_validListTypes))
    then exit(nil);

    LHS:=P_listLiteral(params^.value(0));
    RHS:=P_listLiteral(params^.value(1));
    rhsSet.create;
    for i:=0 to RHS^.size-1 do rhsSet.put(RHS^.value(i),true);
    result:=newListLiteral;
    for i:=0 to LHS^.size-1 do
      if not(rhsSet.get(LHS^.value(i),false))
      then P_listLiteral(result)^.append(LHS^.value(i),true,nullAdapter);
    rhsSet.destroy;
  end;

FUNCTION flatten_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  PROCEDURE recurse_flatten(CONST L:P_listLiteral);
    VAR i:longint;
    begin
      for i:=0 to L^.size-1 do
      if L^.value(i)^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression]
      then P_listLiteral(result)^.append(L^.value(i),true,context.adapters^)
      else recurse_flatten(P_listLiteral(L^.value(i)));
    end;

  begin
    if params<>nil then begin
      result:=newListLiteral;
      recurse_flatten(params);
    end else result:=nil;
  end;

FUNCTION size_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_error..  lt_expression: result:=newIntLiteral(1);
        lt_list..lt_listWithError: result:=newIntLiteral(list0^.size);
      end;
    end;
  end;

FUNCTION trueCount_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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

FUNCTION reverseList_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_validListTypes) then begin
      result:=newListLiteral;
      for i:=list0^.size-1 downto 0 do
        P_listLiteral(result)^.append(list0^.value(i),true,context.adapters^);
    end;
  end;

FUNCTION mapPut_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=mapPut(params,tokenLocation,context.adapters^);
  end;

FUNCTION mapGet_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=mapGet(params,tokenLocation,context.adapters^);
  end;

FUNCTION mapDrop_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=mapDrop(params,tokenLocation,context.adapters^);
  end;

FUNCTION get_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR tmpPar:T_listLiteral;
      i:longint;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_validListTypes)
    then result:=list0^.get(arg1,tokenLocation,context.adapters^)
    else if (params<>nil) and (params^.size>=2) and (arg0^.literalType in C_validListTypes) then begin
      tmpPar.create;
      tmpPar.append(params^.value(0),true,context.adapters^)^
            .append(params^.value(1),true,context.adapters^);
      result:=get_imp(@tmpPar,tokenLocation,context);
      tmpPar.destroy;
      if result<>nil then begin
        tmpPar.create;
        tmpPar.append(result,false,context.adapters^);
        for i:=2 to params^.size-1 do tmpPar.append(params^.value(i),true,context.adapters^);
        result:=get_imp(@tmpPar,tokenLocation,context);
        tmpPar.destroy;
      end;
    end
    else result:=nil;
  end;

FUNCTION getInner_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR tmpPar:T_listLiteral;
      i:longint;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_validListTypes)
    then result:=list0^.getInner(arg1,tokenLocation,context.adapters^)
    else if (params<>nil) and (params^.size>=2) and (arg0^.literalType in C_validListTypes) then begin
      tmpPar.create;
      tmpPar.append(params^.value(0),true,context.adapters^)^
            .append(params^.value(1),true,context.adapters^);
      result:=getInner_imp(@tmpPar,tokenLocation,context);
      tmpPar.destroy;
      if result<>nil then begin
        tmpPar.create;
        tmpPar.append(result,false,context.adapters^);
        for i:=2 to params^.size-1 do tmpPar.append(params^.value(i),true,context.adapters^);
        result:=getInner_imp(@tmpPar,tokenLocation,context);
        tmpPar.destroy;
      end;
    end
    else result:=nil;
  end;

FUNCTION indexOf_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
  registerRule(LIST_NAMESPACE,'add',@add_imp,'add(L,e);#Returns L with the new element e appended',fc_pure);
  registerRule(LIST_NAMESPACE,'head',@head_imp,'head(L);#Returns the first element of list L or [] if L is empty#head(L,k);#Returns the first min(k,size(L)) elements of L or [] if L is empty',fc_pure);
  registerRule(LIST_NAMESPACE,'tail',@tail_imp,'tail(L);#Returns list L without the first element#tail(L,k);#Returns L without the first k elements',fc_pure);
  registerRule(LIST_NAMESPACE,'leading',@leading_imp,'leading(L);#Returns L without the last element or [] if L is empty#leading(L,k);#Returns L without the last k elements or [] if L is empty',fc_pure);
  registerRule(LIST_NAMESPACE,'trailing',@trailing_imp,'trailing(L);#Returns the last element of L#trailing(L,k);#Returns the last k elements of L',fc_pure);
  registerRule(LIST_NAMESPACE,'sort',@sort_imp,'sort(L);#Returns list L sorted ascending (using fallbacks for uncomparable types)#sort(L,leqExpression:expression);#Returns L sorted using the custom binary expression, interpreted as "is lesser or equal"',fc_pure);
  registerRule(LIST_NAMESPACE,'sortPerm',@sortPerm_imp,'sortPerm(L);#Returns indexes I so that L%I==sort(L)',fc_pure);
  registerRule(LIST_NAMESPACE,'unique',@unique_imp,'unique(L:list);#Returns list L without duplicates and enhanced for faster lookup',fc_pure);
  registerRule(LIST_NAMESPACE,'toMap',@mapOf_imp,'toMap(L:keyValueList);#Returns key L without duplicate keys and enhanced for faster lookup',fc_pure);
  registerRule(LIST_NAMESPACE,'elementFrequency',@getElementFreqency,'elementFrequency(L);#Returns a list of pairs [count,e] containing distinct elements e of L and their respective frequencies',fc_pure);
  registerRule(LIST_NAMESPACE,'union',@setUnion,'union(A,...);#Returns a union of all given parameters. All parameters must be lists.',fc_pure);
  registerRule(LIST_NAMESPACE,'intersect',@setIntersect,'intersect(A,...);#Returns an intersection of all given parameters. All parameters must be lists.',fc_pure);
  registerRule(LIST_NAMESPACE,'minus',@setMinus,'minus(A,B);#Returns the asymmetric set difference of A and B. All parameters must be lists.',fc_pure);
  registerRule(LIST_NAMESPACE,'flatten',@flatten_imp,'flatten(L,...);#Returns all parameters as a flat list.',fc_pure);
  registerRule(LIST_NAMESPACE,'size',@size_imp,'size(L);#Returns the number of elements in list L',fc_pure);
  registerRule(LIST_NAMESPACE,'trueCount',@trueCount_impl,'trueCount(B:booleanList);#Returns the number of true values in B',fc_pure);
  registerRule(LIST_NAMESPACE,'reverseList',@reverseList_impl,'reverse(L:list);#Returns L reversed',fc_pure);
  registerRule(LIST_NAMESPACE,'get',@get_imp,'get(L:list,index);',fc_pure);
  registerRule(LIST_NAMESPACE,'getInner',@getInner_imp,'getInner(L:list,index);',fc_pure);
  registerRule(LIST_NAMESPACE,'mapPut',@mapPut_imp,'mapPut(L:keyValueList,key:string,value);#Returns L with an additional or modified key-value-pair [key,value].',fc_pure);
  registerRule(LIST_NAMESPACE,'mapGet',@mapGet_imp,'mapGet(L:keyValueList,key:string);#Returns the element with matching key or the empty list if no such element was found.#'+
                                            'mapGet(L:keyValueList,key:string,fallback);#Returns the element with matching key or fallback if no such element was found.',fc_pure);
  registerRule(LIST_NAMESPACE,'drop',@mapDrop_imp,'drop(L:keyValueList,key:string);#Returns L without [key,?].',fc_pure);
  registerRule(LIST_NAMESPACE,'indexOf',@indexOf_impl,'indexOf(B:booleanList);#Returns the indexes for which B is true.',fc_pure);
end.
