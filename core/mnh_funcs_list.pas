UNIT mnh_funcs_list;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     myGenerics,
     mnh_constants, mnh_basicTypes,
     mnh_out_adapters,
     mnh_litVar,
     mnh_contexts,
     listProcessing,
     mnh_funcs;
VAR BUILTIN_HEAD,BUILTIN_GET,BUILTIN_TAIL,BUILTIN_TRAILING:P_intFuncCallback;

IMPLEMENTATION
{$i mnh_func_defines.inc}
VAR builtinLocation_sort,
    builtinLocation_group:T_identifiedInternalFunction;
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
      if arg0^.literalType in C_listTypes
      then cloneOrCopyList0
      else result:=compound0^.toList;
      P_listLiteral(result)^.sort;
    end else if (params<>nil) and (params^.size=2)
            and (arg0^.literalType in C_compoundTypes)
            and (arg1^.literalType=lt_expression) then begin
      if arg0^.literalType in C_listTypes
      then cloneOrCopyList0
      else result:=compound0^.toList;
      {$ifdef fullVersion}
      context.callStackPush(tokenLocation,@builtinLocation_sort);
      {$endif}
      P_listLiteral(result)^.customSort(P_expressionLiteral(arg1),tokenLocation,@context,context.adapters^);
      {$ifdef fullVersion}
      context.callStackPop;
      {$endif}
    end else if (params<>nil) and (params^.size=2)
            and (arg0^.literalType in C_compoundTypes)
            and (arg1^.literalType=lt_int) then begin
      if arg0^.literalType in C_listTypes
      then cloneOrCopyList0
      else result:=compound0^.toList;
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
    then result:=list0^.transpose(arg1)
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_listTypes)
    then begin
      result:=list0^.transpose(nil);
      if P_listLiteral(result)^.containsError then begin
        disposeLiteral(result);
        context.adapters^.raiseError('The given list cannot be transposed without a filler.',tokenLocation);
      end;
    end;
  end;

FUNCTION getElementFreqency intFuncSignature;
  TYPE T_freqMap=specialize G_literalKeyMap<longint>;
  VAR freqMap:T_freqMap;
      freqList:T_freqMap.KEY_VALUE_LIST;
      i:longint;
      list:P_listLiteral;
      iter:T_arrayOfLiteral;
      x:P_literal;
  begin
    if (params=nil) or (params^.size<>1) then exit(nil);
    if (arg0^.literalType in C_setTypes) or (arg0^.literalType in C_mapTypes) then begin
      result:=newMapLiteral;
      iter:=compound0^.iteratableList;
      for x in iter do P_mapLiteral(result)^.put(x,newIntLiteral(1),false);
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
    VAR iter:T_arrayOfLiteral;
        x:P_literal;
    begin
      iter:=L^.iteratableList;
      for x in iter do if x^.literalType in C_compoundTypes
      then recurse_flatten(P_compoundLiteral(x))
      else listResult^.append(x,true);
      disposeLiteral(iter);
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
      trueLit:P_boolLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_boolean: if bool0^.value then exit(newIntLiteral(1)) else exit(newIntLiteral(0));
        lt_booleanList: begin
          c:=0;
          for i:=0 to list0^.size-1 do if P_boolLiteral(list0^[i])^.value then inc(c);
          exit(newIntLiteral(c));
        end;
        lt_booleanSet: begin
          trueLit:=newBoolLiteral(true);
          if set0^.contains(trueLit)
          then result:=newIntLiteral(1)
          else result:=newIntLiteral(0);
          disposeLiteral(trueLit);
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

FUNCTION cross_impl intFuncSignature;
  VAR i:longint;
      resultSize:longint=1;
      allCompound:boolean=true;
      resultList:P_listLiteral;

  PROCEDURE recurseBuild(CONST index:longint; CONST lit:T_arrayOfLiteral);
    VAR iter:T_arrayOfLiteral;
        subLit:T_arrayOfLiteral;
        resultElement:P_listLiteral;
        l:P_literal;
        k:longint;
    begin
      if index>=params^.size then begin
        resultElement:=newListLiteral(length(lit));
        for l in lit do resultElement^.append(l,true);
        resultList^.append(resultElement,false);
      end else begin
        setLength(subLit,length(lit)+1);
        for k:=0 to length(lit)-1 do subLit[k]:=lit[k];
        k:=length(lit);

        iter:=P_compoundLiteral(params^[index])^.iteratableList;
        for l in iter do begin
          subLit[k]:=l;
          recurseBuild(index+1,subLit);
        end;
        disposeLiteral(iter);
      end;
    end;

  VAR emptyList:T_arrayOfLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType in C_compoundTypes) then begin
      if (params^.size=1) then exit(arg0^.rereferenced);
      resultSize:=compound0^.size;
      for i:=1 to params^.size-1 do
        if (params^[i]^.literalType in C_compoundTypes)
        then resultSize:=resultSize*P_compoundLiteral(params^[i])^.size
        else allCompound:=false;
      if not(allCompound) then exit(nil);
      resultList:=newListLiteral(resultSize);
      setLength(emptyList,0);
      recurseBuild(0,emptyList);
      result:=resultList;
    end;
  end;

FUNCTION group_imp intFuncSignature;
  TYPE T_groupMap=specialize G_literalKeyMap<P_literal>;
  VAR listToGroup:P_listLiteral;
      keyList:T_arrayOfLiteral;
      aggregator:P_expressionLiteral;
      groupMap:T_groupMap;
      groupList :T_groupMap.KEY_VALUE_LIST;
      groupEntry:T_groupMap.CACHE_ENTRY;

  PROCEDURE makeKeysByIndex(CONST index:longint);
    VAR i:longint;
        dummy:P_literal;
        hasError:boolean=false;
    begin
      dummy:=newErrorLiteral;
      setLength(keyList,listToGroup^.size);
      for i:=0 to length(keyList)-1 do begin
        if (listToGroup^[i]^.literalType in C_listTypes) and (P_listLiteral(listToGroup^[i])^.size>index)
        then keyList[i]:=P_listLiteral(listToGroup^[i])^[index]^.rereferenced
        else begin
          keyList[i]:=dummy^.rereferenced;
          if not(hasError) then
          context.adapters^.raiseError('Grouping by sublist-index is not possible for this list. Problematic entry: '+
                                       listToGroup^[i]^.toString(50)+' at index '+intToStr(i),tokenLocation);
          hasError:=true;
        end;
      end;
      disposeLiteral(dummy);
    end;

  PROCEDURE addToAggregation(CONST groupKey:P_literal; CONST L:P_literal); inline;
    VAR newLit:P_literal;
        resultLiteral:P_literal;
    begin
      resultLiteral:=groupMap.get(groupKey,nil);

      if aggregator=nil then begin
        if resultLiteral=nil then resultLiteral:=newListLiteral;
        P_listLiteral(resultLiteral)^.append(L,true);
      end else begin
        if resultLiteral=nil then begin
          resultLiteral:=L; L^.rereference;
        end else begin
          newLit:=P_expressionLiteral(aggregator)^.evaluateToLiteral(tokenLocation,@context,resultLiteral,L);
          if newLit<>nil then begin
            disposeLiteral(resultLiteral);
            resultLiteral:=newLit;
          end else begin
            context.adapters^.raiseError('Error performing aggregation in group-construct with aggregator '+aggregator^.toString,tokenLocation);
            exit;
          end;
        end;
      end;
      groupMap.put(groupKey,resultLiteral);
    end;

  VAR inputIndex:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.size<=3) and
       (arg0^.literalType in C_listTypes) and
      ((arg1^.literalType in C_listTypes) and (list1^.size=list0^.size) or (arg1^.literalType=lt_int)) and
      ((params^.size=2) or (arg2^.literalType=lt_expression))
    then begin
      listToGroup:=P_listLiteral(arg0);

      if arg1^.literalType=lt_int
      then makeKeysByIndex(P_intLiteral(arg1)^.value)
      else keyList:=list1^.iteratableList;

      if (params^.size=3) then aggregator:=P_expressionLiteral(arg2)
                          else aggregator:=nil;
      {$ifdef fullVersion}
      if aggregator<>nil then context.callStackPush(tokenLocation,@builtinLocation_group);
      {$endif}
      groupMap.create();
      for inputIndex:=0 to length(keyList)-1 do if context.adapters^.noErrors then
        addToAggregation(keyList[inputIndex],listToGroup^[inputIndex]);
      disposeLiteral(keyList);

      groupList:=groupMap.keyValueList;
      result:=newListLiteral(length(groupList));
      for groupEntry in groupList do listResult^.append(groupEntry.value,false);
      groupMap.destroy;
      {$ifdef fullVersion}
      if aggregator<>nil then context.callStackPop();
      {$endif}
    end;
  end;

FUNCTION map_imp intFuncSignature;
  VAR iterator:P_iterator;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      iterator:=newIterator(arg0);
      result:=processMapSerial(iterator,P_expressionLiteral(arg1),context);
      dispose(iterator,destroy);
    end;
  end;

FUNCTION pMap_imp intFuncSignature;
  VAR iterator:P_iterator;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      iterator:=newIterator(arg0);
      if tco_spawnWorker in context.threadOptions
      then result:=processMapParallel(iterator,P_expressionLiteral(arg1),context)
      else result:=processMapSerial  (iterator,P_expressionLiteral(arg1),context);
      dispose(iterator,destroy);
    end;
  end;

INITIALIZATION
  //Functions on lists:
  BUILTIN_HEAD:=
  registerRule(LIST_NAMESPACE,'head'    ,@head_imp    ,[],ak_variadic_1,'head(L);//Returns the first element of list L or [] if L is empty#head(L,k);//Returns the first min(k,size(L)) elements of L or [] if L is empty');
  BUILTIN_TAIL:=
  registerRule(LIST_NAMESPACE,'tail'    ,@tail_imp    ,[],ak_variadic_1,'tail(L);//Returns list L without the first element#tail(L,k);//Returns L without the first k elements');
  registerRule(LIST_NAMESPACE,'leading' ,@leading_imp ,[],ak_variadic_1,'leading(L);//Returns L without the last element or [] if L is empty#leading(L,k);//Returns L without the last k elements or [] if L is empty');
  BUILTIN_TRAILING:=
  registerRule(LIST_NAMESPACE,'trailing',@trailing_imp,[],ak_variadic_1,'trailing(L);//Returns the last element of L#trailing(L,k);//Returns the last k elements of L');
  builtinLocation_sort.create(LIST_NAMESPACE,'sort');
  registerRule(LIST_NAMESPACE,'sort'    ,@sort_imp    ,[],ak_variadic_1,
                                               'sort(L);//Returns list L sorted ascending (using fallbacks for uncomparable types)#'+
                                               'sort(L,leqExpression:expression);//Returns L sorted using the custom binary expression, interpreted as "is lesser or equal"#'+
                                               'sort(L,innerIndex:int);//Returns L sorted by given inner index');
  registerRule(LIST_NAMESPACE,'sortPerm'        ,@sortPerm_imp      ,[],ak_unary     ,'sortPerm(L);//Returns indexes I so that L%I==sort(L)');
  registerRule(LIST_NAMESPACE,'unique'          ,@unique_imp        ,[],ak_unary     ,'unique(L:list);//Returns list L without duplicates and enhanced for faster lookup');
  registerRule(LIST_NAMESPACE,'elementFrequency',@getElementFreqency,[],ak_unary     ,'elementFrequency(L);//Returns a list of pairs [count,e] containing distinct elements e of L and their respective frequencies');
  registerRule(LIST_NAMESPACE,'transpose'       ,@transpose_imp     ,[],ak_unary     ,'transpose(L,filler);//Returns list L transposed. If sub lists of L have different lengths, filler is used.#transpose(L);//Returns list L transposed. If sub lists of L have different lengths, filler is used.');
  registerRule(LIST_NAMESPACE,'union'           ,@setUnion_imp      ,[],ak_variadic_1,'union(A,...);//Returns a union of all given parameters. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'intersect'       ,@setIntersect_imp  ,[],ak_variadic_1,'intersect(A,...);//Returns an intersection of all given parameters. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'minus'           ,@setMinus_imp      ,[],ak_binary    ,'minus(A,B);//Returns the asymmetric set difference of A and B. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'flatten'         ,@flatten_imp       ,[],ak_variadic  ,'flatten(L,...);//Returns all parameters as a flat list.');
  registerRule(LIST_NAMESPACE,'size'            ,@size_imp          ,[],ak_unary     ,'size(L);//Returns the number of elements in list L');
  registerRule(LIST_NAMESPACE,'trueCount'       ,@trueCount_impl    ,[],ak_unary     ,'trueCount(B:booleanList);//Returns the number of true values in B');
  registerRule(LIST_NAMESPACE,'reverseList'     ,@reverseList_impl  ,[],ak_unary     ,'reverseList(L:list);//Returns L reversed');
  BUILTIN_GET:=
  registerRule(LIST_NAMESPACE,'get'     ,@get_imp     ,[],ak_variadic_2,'get(L,accessor);//Returns elements of list, set or map L by accessor');
  registerRule(LIST_NAMESPACE,'getAll'  ,@getAll_imp  ,[],ak_binary    ,'getAll(L,accessors);//Returns elements of list, set or map L by collection of accessors#getAll(L,accessors:list,fallback:list);//Returns elements of list, set or map L by collection of accessors.#//If no such element is found, the respective fallback entry is used.#//fallback must have the same size as accessors');
  registerRule(LIST_NAMESPACE,'getInner',@getInner_imp,[],ak_variadic_2,'getInner(L:list,index);');
  registerRule(LIST_NAMESPACE,'indexOf' ,@indexOf_impl,[],ak_unary     ,'indexOf(B:booleanList);//Returns the indexes for which B is true.');
  registerRule(LIST_NAMESPACE,'cross'   ,@cross_impl  ,[],ak_variadic_2,'cross(A,...);//Returns the cross product of the arguments (each of which must be a list, set or map)');
  builtinLocation_group.create(DEFAULT_BUILTIN_NAMESPACE,'group');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'group'         ,@group_imp         ,[],ak_variadic_2,'group(list,grouping);//Re-groups list by grouping (which is a sub-index or a list)#group(list,grouping,aggregator:expression);//Groups by grouping using aggregator on a per group basis');
  registerRule(LIST_NAMESPACE,'map',    @map_imp   ,[],ak_binary,'map(L,f:expression(1));//Returns a list with f(x) for each x in L#//L may be a generator');
  registerRule(LIST_NAMESPACE,'pMap',   @pMap_imp  ,[],ak_binary,'pMap(L,f:expression(1));//Returns a list with f(x) for each x in L#//L may be a generator');

FINALIZATION
  builtinLocation_sort.destroy;
  builtinLocation_group .destroy;

end.
