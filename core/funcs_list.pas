UNIT funcs_list;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     mySys,
     mnh_constants, basicTypes,
     mnh_messages,
     litVar,
     contexts,
     listProcessing,
     recyclers,
     subrules,
     funcs,
     aggregators;
VAR BUILTIN_HEAD,BUILTIN_GET,BUILTIN_TAIL,BUILTIN_TRAILING,BUILTIN_ELEMENT_FREQUENCY:P_intFuncCallback;

IMPLEMENTATION
{$i func_defines.inc}
USES func_queues;
{$define SUB_LIST_IMPL:=
begin
  result:=nil;
  if (params<>nil) and (params^.size>=1) then begin
    if arg0^.literalType in C_listTypes then begin
      if      (params^.size=1)                                                    then result:=list0^.CALL_MACRO(recycler)
      else if (params^.size=2) and (arg1^.literalType in [lt_smallint,lt_bigint]) then result:=list0^.CALL_MACRO(recycler,int1^.intValue);
    end else if arg0^.literalType in C_compoundTypes then begin
      tempList:=compound0^.toList(recycler);
      if      (params^.size=1)                                                    then result:=tempList^.CALL_MACRO(recycler)
      else if (params^.size=2) and (arg1^.literalType in [lt_smallint,lt_bigint]) then result:=tempList^.CALL_MACRO(recycler,int1^.intValue);
      recycler^.disposeLiteral(tempList);
    end else if arg0^.literalType in C_scalarTypes then SCALAR_FALLBACK;
  end;
end}
{$define IS_GENERATOR_CASE:=
           (params<>nil)
       and (params^.size=2)
       and (arg0^.literalType=lt_expression)
       and (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes)
       and (arg1^.literalType in [lt_smallint,lt_bigint])
       and (int1^.intValue>=0)}

FUNCTION head_imp intFuncSignature;
{$define CALL_MACRO:=head}
{$define SCALAR_FALLBACK:=result:=arg0^.rereferenced}
  VAR i:longint;
      valueToAppend:P_literal;
      tempList:P_listLiteral;
  begin
    if IS_GENERATOR_CASE then begin
       if int1^.intValue=0 then exit(recycler^.newListLiteral());
       result:=recycler^.newListLiteral(int1^.intValue);
       for i:=1 to int1^.intValue do begin
         valueToAppend:=P_expressionLiteral(arg0)^.evaluate(tokenLocation,context,recycler).literal;
         if (valueToAppend=nil) or (valueToAppend^.literalType=lt_void)
         then break
         else listResult^.append(recycler,valueToAppend,false);
       end;
       exit(result);
    end;
    SUB_LIST_IMPL;
  end;

FUNCTION trailing_imp intFuncSignature;
{$define CALL_MACRO:=trailing}
  VAR i:longint;
      valueToAppend:P_literal;
      buffer:T_arrayOfLiteral;
      tempList:P_listLiteral;
  begin
    if IS_GENERATOR_CASE then begin
       if int1^.intValue=0 then exit(recycler^.newListLiteral());
       setLength(buffer,int1^.intValue);
       for i:=0 to length(buffer)-1 do buffer[i]:=nil;
       repeat
         valueToAppend:=P_expressionLiteral(arg0)^.evaluate(tokenLocation,context,recycler).literal;
         if (valueToAppend<>nil) and (valueToAppend^.literalType<>lt_void) then begin
           if buffer[0]<>nil then recycler^.disposeLiteral(buffer[0]);
           //TODO MSC: This could be optimized by using a rolling buffer.
           for i:=1 to length(buffer)-1 do buffer[i-1]:=buffer[i];
           buffer[length(buffer)-1]:=valueToAppend;
         end;
       until (valueToAppend=nil) or (valueToAppend^.literalType=lt_void);
       result:=recycler^.newListLiteral(int1^.intValue);
       for i:=0 to length(buffer)-1 do if buffer[i]<>nil then listResult^.append(recycler,buffer[i],false,false);
       exit(result);
    end;
    SUB_LIST_IMPL;
  end;

FUNCTION tail_imp intFuncSignature;
{$define CALL_MACRO:=tail}
{$define SCALAR_FALLBACK:=result:=recycler^.newListLiteral}
  VAR L:P_literal;
      i:longint;
      tempList:P_listLiteral;
  begin
    if IS_GENERATOR_CASE then begin
       //"tail" of a generator is obtained by calling it n times
       for i:=1 to int1^.intValue do begin
         L:=P_expressionLiteral(arg0)^.evaluate(tokenLocation,context,recycler).literal;
         if L<>nil then recycler^.disposeLiteral(L);
       end;
       exit(arg0^.rereferenced);
    end;
    SUB_LIST_IMPL;
  end;

FUNCTION leading_imp intFuncSignature;
{$define CALL_MACRO:=leading}
  VAR valueToAppend:P_literal;
      tempList:P_listLiteral;
  begin
    if IS_GENERATOR_CASE then begin
       tempList:=recycler^.newListLiteral();
       repeat
         valueToAppend:=P_expressionLiteral(arg0)^.evaluate(tokenLocation,context,recycler).literal;
         if (valueToAppend<>nil) and (valueToAppend^.literalType<>lt_void) then tempList^.append(recycler,valueToAppend,false);
       until (valueToAppend=nil) or (valueToAppend^.literalType=lt_void);
       result:=tempList^.leading(recycler,int1^.intValue);
       recycler^.disposeLiteral(tempList);
       exit(result);
    end;
    SUB_LIST_IMPL;
  end;

{$undef SUB_LIST_IMPL}
{$undef CALL_MACRO}
{$undef SCALAR_FALLBACK}

{$define cloneOrCopyList0:=if arg0^.getReferenceCount=1 then result:=arg0^.rereferenced else result:=list0^.clone(recycler)}
{$ifdef fullVersion}VAR sortLoc:P_intFuncCallback;{$endif}
FUNCTION sort_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_compoundTypes) then begin
      if arg0^.literalType in C_listTypes
      then cloneOrCopyList0
      else result:=compound0^.toList(recycler);
      P_listLiteral(result)^.sort;
    end else if (params<>nil) and (params^.size=2)
            and (arg0^.literalType in C_compoundTypes)
            and (arg1^.literalType=lt_expression) then begin
      if arg0^.literalType in C_listTypes
      then cloneOrCopyList0
      else result:=compound0^.toList(recycler);
      {$ifdef fullVersion}
      context^.callStackPush(tokenLocation,builtinFunctionMap.getIntrinsicRuleAsExpression(sortLoc,false),nil);
      {$endif}
      if P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(2)
      then P_listLiteral(result)^.customSort_2(P_expressionLiteral(arg1),tokenLocation,context,recycler)
      else if P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)
      then P_listLiteral(result)^.customSort_1(P_expressionLiteral(arg1),tokenLocation,context,recycler)
      else begin
        recycler^.disposeLiteral(result);
        result:=nil;
      end;
      {$ifdef fullVersion}
      context^.callStackPop(nil);
      {$endif}
    end else if (params<>nil) and (params^.size=2)
            and (arg0^.literalType in C_compoundTypes)
            and (arg1^.literalType in [lt_smallint,lt_bigint]) then begin
      if arg0^.literalType in C_listTypes
      then cloneOrCopyList0
      else result:=compound0^.toList(recycler);
      P_listLiteral(result)^.sortBySubIndex(int1^.intValue,tokenLocation,context);
    end
  end;

FUNCTION sortPerm_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_listTypes)
    then result:=list0^.sortPerm(recycler);
  end;

FUNCTION unique_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (arg0^.literalType in C_listTypes) then begin
        cloneOrCopyList0;
        P_listLiteral(result)^.unique(recycler);
      end else if (arg0^.literalType in C_setTypes) then exit(arg0^.rereferenced);
    end;
  end;

FUNCTION transpose_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_listTypes)
    then result:=list0^.transpose(recycler,arg1)
    else if (params<>nil) and (params^.size>=1) and (arg0^.literalType in C_mapTypes)
    then result:=map0^.transpose(recycler)
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_listTypes)
    then begin
      result:=list0^.transpose(recycler,nil);
      if result=nil then context^.raiseError('The given list cannot be transposed without a filler.',tokenLocation);
    end;
  end;

FUNCTION getElementFreqency intFuncSignature;
  TYPE T_freqMap=specialize G_literalKeyMap<longint>;
  VAR freqMap:T_freqMap;
      freqList:T_freqMap.KEY_VALUE_LIST;
      freqEntry:T_freqMap.P_CACHE_ENTRY;
      mapEntry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
      i:longint;
      list:P_listLiteral;
      iter:T_arrayOfLiteral;
      x:P_literal;

      valueToAppend:T_evaluationResult;
      aggregator:P_elementFrequencyAggregator;
  begin
    if (params=nil) or (params^.size<>1) then exit(nil);
    result:=nil;
    if (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes) then begin
      new(aggregator,create);

      valueToAppend:=P_expressionLiteral(arg0)^.evaluate(tokenLocation,context,recycler);
      while (valueToAppend.literal<>nil) and (valueToAppend.literal^.literalType<>lt_void) do begin
        aggregator^.addToAggregation(valueToAppend,true,tokenLocation,context,recycler);
        valueToAppend:=P_expressionLiteral(arg0)^.evaluate(tokenLocation,context,recycler);
      end;
      result:=aggregator^.getResult(recycler);
      dispose(aggregator,destroy);
      exit(result);
    end;

    if (arg0^.literalType in C_setTypes) or (arg0^.literalType in C_mapTypes) then begin
      result:=newMapLiteral(100);
      iter:=compound0^.forcedIteratableList(recycler);
      for x in iter do P_mapLiteral(result)^.put(recycler,x,recycler^.newIntLiteral(1),false);
      exit(result);
    end;
    if not(arg0^.literalType in C_listTypes) then exit(nil);
    list:=list0;

    freqMap.create;
    for i:=0 to list^.size-1 do begin
      freqEntry:=freqMap.getEntry(list^.value[i]);
      if freqEntry=nil
      then freqMap.put(list^.value[i],1)
      else inc(freqEntry^.value);
    end;
    if not(context^.messages^.continueEvaluation) then begin
      freqMap.destroy;
      exit(nil);
    end;
    freqList:=freqMap.keyValueList;
    result:=newMapLiteral(length(freqList));
    for i:=0 to length(freqList)-1 do begin
      mapEntry.key:=freqList[i].key^.rereferenced;
      mapEntry.value:=recycler^.newIntLiteral(freqList[i].value);
      mapEntry.keyHash:=freqList[i].keyHash;
      P_mapLiteral(result)^.underlyingMap^.putNew(mapEntry,x);
    end;
    P_mapLiteral(result)^.ensureType;
    freqMap.destroy;
  end;

FUNCTION flatten_imp intFuncSignature;
  PROCEDURE recurse_flatten(CONST L:P_compoundLiteral);
    VAR iter:T_arrayOfLiteral;
        x:P_literal;
    begin
      iter:=L^.forcedIteratableList(recycler);
      for x in iter do if x^.literalType in C_compoundTypes
      then recurse_flatten(P_compoundLiteral(x))
      else listResult^.append(recycler,x,true);
      recycler^.disposeLiterals(iter);
    end;

  begin
    if params<>nil then begin
      result:=recycler^.newListLiteral;
      recurse_flatten(params);
    end else result:=nil;
  end;

FUNCTION size_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType in C_compoundTypes
      then result:=recycler^.newIntLiteral(compound0^.size)
      else if (arg0^.literalType=lt_expression)  and (P_expressionLiteral(arg0)^.typ=et_builtinIteratable) and (P_builtinGeneratorExpression(arg0)^.getBultinGeneratorType=bgt_queue)
      then result:=recycler^.newIntLiteral(P_queue(arg0)^.getQueuedCount)
      else result:=recycler^.newIntLiteral(1);
    end;
  end;

FUNCTION trueCount_impl intFuncSignature;
  VAR i,c:longint;
      trueLit:P_boolLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_boolean: if bool0^.value then exit(recycler^.newIntLiteral(1)) else exit(recycler^.newIntLiteral(0));
        lt_booleanList: begin
          c:=0;
          for i:=0 to list0^.size-1 do if P_boolLiteral(list0^.value[i])^.value then inc(c);
          exit(recycler^.newIntLiteral(c));
        end;
        lt_booleanSet: begin
          trueLit:=newBoolLiteral(true);
          if set0^.contains(trueLit)
          then result:=recycler^.newIntLiteral(1)
          else result:=recycler^.newIntLiteral(0);
          recycler^.disposeLiteral(trueLit);
        end;
        lt_emptyList,lt_emptySet: exit(recycler^.newIntLiteral(0));
      end;
    end;
  end;

FUNCTION reverseList_impl intFuncSignature;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_listTypes) then begin
      result:=recycler^.newListLiteral;
      for i:=list0^.size-1 downto 0 do listResult^.append(recycler,list0^.value[i],true);
    end;
  end;

FUNCTION setUnion_imp     intFuncSignature; begin result:=setUnion    (recycler,params); end;
FUNCTION setIntersect_imp intFuncSignature; begin result:=setIntersect(recycler,params); end;
FUNCTION setMinus_imp     intFuncSignature; begin result:=setMinus    (recycler,params); end;
FUNCTION isSubsetOf_imp   intFuncSignature;
  VAR iter:T_arrayOfLiteral;
      a:P_literal;
      allContained:boolean=true;
  begin
    if (params<>nil) and (params^.size=2) then begin
      if (arg0^.literalType in C_compoundTypes) then case arg1^.literalType of
        lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList,
        lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet,
        lt_map:
          begin
            iter:=compound0^.forcedIteratableList(recycler);
            for a in iter do allContained:=allContained and list1^.contains(a);
            recycler^.disposeLiterals(iter);
            result:=newBoolLiteral(allContained);
          end;
        lt_emptyList,lt_emptySet,lt_emptyMap:
          result:=newBoolLiteral(compound0^.size=0);
        else result:=newBoolLiteral(false);
      end else result:=newBoolLiteral(false);
    end else result:=nil;
  end;
FUNCTION mergeMaps_imp    intFuncSignature; begin result:=mapMerge    (recycler,params,tokenLocation,context,recycler); end;

FUNCTION get_imp intFuncSignature;
  VAR tmpPar:T_listLiteral;
      i:longint;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_compoundTypes)
    then result:=compound0^.get(recycler,arg1)
    else if (params<>nil) and (params^.size>=2) and (arg0^.literalType in C_compoundTypes) then begin
      tmpPar.create(2);
      tmpPar.append(recycler,arg0,true)^
            .append(recycler,arg1,true);
      result:=get_imp(@tmpPar,tokenLocation,context,recycler);
      tmpPar.cleanup(recycler);
      tmpPar.destroy;
      if (result=nil) or (result^.literalType=lt_void) then exit(result);
      tmpPar.create(params^.size-1);
      tmpPar.append(recycler,result,false);
      for i:=2 to params^.size-1 do tmpPar.append(recycler,params^.value[i],true);
      result:=get_imp(@tmpPar,tokenLocation,context,recycler);
      tmpPar.cleanup(recycler);
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
      result:=P_collectionLiteral(arg1)^.newOfSameType(recycler,true);
      iter:=compound1^.forcedIteratableList(recycler);
      for sub in iter do if result<>nil then begin
        got:=compound0^.get(recycler,sub);
        if got<>nil then begin
          if got^.literalType<>lt_void
          then collResult^.append(recycler,got,false)
          else recycler^.disposeLiteral(got);
        end else recycler^.disposeLiteral(result);
      end;
      recycler^.disposeLiterals(iter);
    end else if (params<>nil) and (params^.size=3) and (arg0^.literalType in C_compoundTypes) and (arg1^.literalType in C_listTypes) and (arg2^.literalType in C_listTypes) and (list1^.size=list2^.size) then begin
      result:=recycler^.newListLiteral;
      for i:=0 to list1^.size-1 do if result<>nil then begin
        got:=compound0^.get(recycler,list1^.value[i]);
        if got<>nil then begin
          if got^.literalType<>lt_void
          then collResult^.append(recycler,got,false)
          else begin
            recycler^.disposeLiteral(got);
            collResult^.append(recycler,list2^.value[i],true);
          end;
        end else recycler^.disposeLiteral(result);
      end;
    end;
  end;

FUNCTION getInner_imp intFuncSignature;
  VAR tmpPar:T_listLiteral;
      i:longint;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_compoundTypes)
    then result:=compound0^.getInner(recycler,arg1)
    else if (params<>nil) and (params^.size>=2) and (arg0^.literalType in C_compoundTypes) then begin
      tmpPar.create(2);
      tmpPar.append(recycler,arg0,true)^
            .append(recycler,arg1,true);
      result:=getInner_imp(@tmpPar,tokenLocation,context,recycler);
      tmpPar.cleanup(recycler);
      tmpPar.destroy;
      if result<>nil then begin
        tmpPar.create(params^.size-1);
        tmpPar.append(recycler,result,false);
        for i:=2 to params^.size-1 do tmpPar.append(recycler,params^.value[i],true);
        result:=getInner_imp(@tmpPar,tokenLocation,context,recycler);
        tmpPar.cleanup(recycler);
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
      result:=recycler^.newListLiteral;
      with list0^ do for i:=0 to size-1 do if P_boolLiteral(list0^.value[i])^.value then listResult^.appendInt(recycler,i);
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
        resultElement:=recycler^.newListLiteral(length(lit));
        for l in lit do resultElement^.append(recycler,l,true);
        resultList^.append(recycler,resultElement,false);
      end else begin
        setLength(subLit,length(lit)+1);
        for k:=0 to length(lit)-1 do subLit[k]:=lit[k];
        k:=length(lit);

        iter:=P_compoundLiteral(params^.value[index])^.forcedIteratableList(recycler);
        for l in iter do begin //if not(memoryPanic) then begin
          subLit[k]:=l;
          recurseBuild(index+1,subLit);
        end;
        recycler^.disposeLiterals(iter);
      end;
    end;

  VAR emptyList:T_arrayOfLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType in C_compoundTypes) then begin
      if (params^.size=1) then exit(arg0^.rereferenced);
      resultSize:=compound0^.size;
      for i:=1 to params^.size-1 do
        if (params^.value[i]^.literalType in C_compoundTypes)
        then resultSize:=resultSize*P_compoundLiteral(params^.value[i])^.size
        else allCompound:=false;
      if not(allCompound) then exit(nil);
      resultList:=recycler^.newListLiteral(resultSize);
      setLength(emptyList,0);
      recurseBuild(0,emptyList);
      result:=resultList;
    end;
  end;

FUNCTION group_imp intFuncSignature;
  VAR listToGroup:P_listLiteral;
      keyList:T_arrayOfLiteral;
      aggregator: P_expressionLiteral=nil;
      groupMap:P_literalKeyLiteralValueMap;

  PROCEDURE makeKeysByIndex(CONST index:longint);
    VAR i:longint;
        dummy:P_literal;
        hasError:boolean=false;
    begin
      dummy:=newVoidLiteral;
      setLength(keyList,listToGroup^.size);
      for i:=0 to length(keyList)-1 do begin
        if (listToGroup^.value[i]^.literalType in C_listTypes) and (P_listLiteral(listToGroup^.value[i])^.size>index)
        then keyList[i]:=P_listLiteral(listToGroup^.value[i])^.value[index]^.rereferenced
        else begin
          keyList[i]:=dummy^.rereferenced;
          if not(hasError) then
          context^.raiseError('Grouping by sublist-index is not possible for this list. Problematic entry: '+
                             listToGroup^.value[i]^.toString(50)+' at index '+intToStr(i),tokenLocation);
          hasError:=true;
        end;
      end;
      recycler^.disposeLiteral(dummy);
    end;

  PROCEDURE addToAggregation(CONST groupKey:P_literal; CONST L:P_literal); {$ifndef debugMode} inline; {$endif}
    VAR newLit:P_literal;
        resultLiteral:P_literal;
    begin
      resultLiteral:=groupMap^.get(groupKey,nil);
      if aggregator=nil then begin
        if resultLiteral=nil then begin
          resultLiteral:=recycler^.newListLiteral;
          groupMap^.put(groupKey^.rereferenced,resultLiteral);
        end;
        P_listLiteral(resultLiteral)^.append(recycler,L,true);
      end else begin
        if resultLiteral=nil then begin
          groupKey^.rereference;
          resultLiteral:=L; L^.rereference;
        end else begin
          newLit:=evaluteExpression(aggregator,tokenLocation,context,recycler,resultLiteral,L).literal;
          if newLit<>nil then begin
            recycler^.disposeLiteral(resultLiteral);
            resultLiteral:=newLit;
          end else begin
            context^.raiseError('Error performing aggregation in group-construct with aggregator '+arg2^.toString,tokenLocation);
            exit;
          end;
        end;
        groupMap^.put(groupKey,resultLiteral);
      end;
    end;

  VAR inputIndex:longint;
      newLit:P_literal;
      resultLiteral:P_literal;

  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.size<=3) and
       (arg0^.literalType in C_listTypes) and
      ((arg1^.literalType in C_listTypes) and (list1^.size=list0^.size) or (arg1^.literalType in [lt_smallint,lt_bigint])) and
      ((params^.size=2) or (arg2^.literalType=lt_expression))
    then begin
      listToGroup:=P_listLiteral(arg0);
      if (params^.size=3) then aggregator:=P_expressionLiteral(arg2)
                          else aggregator:=nil;
      if arg1^.literalType in [lt_smallint,lt_bigint]
      then begin
        initialize(keyList);
        makeKeysByIndex(int1^.intValue);
      end else keyList:=list1^.forcedIteratableList(recycler);

      result:=newMapLiteral(0);
      groupMap:=P_mapLiteral(result)^.underlyingMap;

      if aggregator=nil then begin
        for inputIndex:=0 to length(keyList)-1 do if context^.continueEvaluation then begin
          resultLiteral:=groupMap^.get(keyList[inputIndex],nil);
          if resultLiteral=nil then begin
            resultLiteral:=recycler^.newListLiteral;
            groupMap^.put(keyList[inputIndex]^.rereferenced,resultLiteral);
          end;
          P_listLiteral(resultLiteral)^.append(recycler,listToGroup^.value[inputIndex],true);

        end;
      end else begin
        for inputIndex:=0 to length(keyList)-1 do if context^.continueEvaluation then begin
          resultLiteral:=groupMap^.get(keyList[inputIndex],nil);
          if resultLiteral=nil then begin
            keyList[inputIndex]^.rereference;
            resultLiteral:=listToGroup^.value[inputIndex]; listToGroup^.value[inputIndex]^.rereference;
          end else begin
            newLit:=evaluteExpression(aggregator,tokenLocation,context,recycler,resultLiteral,listToGroup^.value[inputIndex]).literal;
            if newLit<>nil then begin
              recycler^.disposeLiteral(resultLiteral);
              resultLiteral:=newLit;
            end else begin
              context^.raiseError('Error performing aggregation in group-construct with aggregator '+arg2^.toString,tokenLocation);
              exit;
            end;
          end;
          groupMap^.put(keyList[inputIndex],resultLiteral);
        end;

      end;
      recycler^.disposeLiterals(keyList);
      P_mapLiteral(result)^.ensureType;
    end;
  end;

FUNCTION groupToList_imp intFuncSignature;
  VAR keyList     :array of longint;
      defaultValue:P_literal;
      aggregator  :P_expressionLiteral=nil;

      temp        :P_literal;
      resultValues:T_arrayOfLiteral;
      resultValueCount:longint=0;
      i,key       :longint;
      allOkay     :boolean=true;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=4) and (params^.size<=5) and
       (arg0^.literalType in C_listTypes) and
       (arg1^.literalType in [lt_emptyList,lt_intList]) and
       (list1^.size=list0^.size) and
       (arg3^.literalType=lt_expression) and
       ((params^.size=4) or (params^.value[4]^.literalType=lt_smallint) and (P_smallIntLiteral(params^.value[4])^.value>=0))
    then begin
      defaultValue:=arg2;
      aggregator:=P_expressionLiteral(arg3);
      if params^.size<=4
      then resultValueCount:=0
      else resultValueCount:=P_smallIntLiteral(params^.value[4])^.value;

      setLength(keyList,list1^.size);
      for i:=0 to list1^.size-1 do begin
        temp:=list1^.value[i];
        if (temp^.literalType=lt_bigint) then begin
          allOkay:=false;
          context^.raiseError('Index out of bounds in groupToList: '+temp^.toString,tokenLocation);
        end else begin
          key:=P_smallIntLiteral(temp)^.value;
          keyList[i]:=key;
          if key>=resultValueCount then resultValueCount:=key+1;
          if key<0 then begin
            allOkay:=false;
            context^.raiseError('Index out of bounds in groupToList: '+temp^.toString,tokenLocation);
          end;
        end;
      end;
      if not(allOkay) then begin
        setLength(keyList,0);
        exit(nil);
      end;

      setLength(resultValues,resultValueCount);
      for i:=0 to length(resultValues)-1 do resultValues[i]:=nil;

      for i:=0 to list0^.size-1 do if allOkay then begin
        key:=keyList[i];
        if resultValues[key]=nil
        then resultValues[key]:=list0^.value[i]^.rereferenced
        else begin
          temp:=evaluteExpression(aggregator,tokenLocation,context,recycler,resultValues[key],list0^.value[i]).literal;
          if temp<>nil then begin
            recycler^.disposeLiteral(resultValues[key]);
            resultValues[key]:=temp;
          end else begin
            context^.raiseError('Error performing aggregation in group-construct with aggregator '+arg3^.toString,tokenLocation);
            allOkay:=false;
          end;
        end;
      end;
      if allOkay then begin
        result:=recycler^.newListLiteral(length(resultValues));
        for temp in resultValues do
          if temp=nil
          then listResult^.append(recycler,defaultValue,true)
          else listResult^.append(recycler,temp,false);
      end;
      setLength(keyList,0);
      setLength(resultValues,0);
    end;
  end;

FUNCTION toGenerator_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (arg0^.literalType=lt_expression) then begin
        result:=arg0^.rereferenced;
        P_expressionLiteral(result)^.makeIteratable(context,tokenLocation);
      end else result:=newIterator(recycler,arg0,tokenLocation);
    end;
  end;

FUNCTION vectorIfThenElse_imp intFuncSignature;
  VAR i:longint;
  begin
    if (params<>nil) and (params^.size=3) and (arg0^.literalType in [lt_emptyList,lt_booleanList]) then begin
      result:=recycler^.newListLiteral(list0^.size);
      if (arg1^.literalType in C_listTypes) and (list1^.size=list0^.size) then begin
        if (arg2^.literalType in C_listTypes) and (list2^.size=list0^.size) then begin
          for i:=0 to list0^.size-1 do if boolLit[true].equals(list0^.value[i])
          then listResult^.append(recycler,list1^.value[i]^.rereferenced,false)
          else listResult^.append(recycler,list2^.value[i]^.rereferenced,false);
        end else begin
          for i:=0 to list0^.size-1 do if boolLit[true].equals(list0^.value[i])
          then listResult^.append(recycler,list1^.value[i]^.rereferenced,false)
          else listResult^.append(recycler,arg2           ^.rereferenced,false);
        end;
      end else begin
        if (arg2^.literalType in C_listTypes) and (list2^.size=list0^.size) then begin
          for i:=0 to list0^.size-1 do if boolLit[true].equals(list0^.value[i])
          then listResult^.append(recycler,arg1           ^.rereferenced,false)
          else listResult^.append(recycler,list2^.value[i]^.rereferenced,false);
        end else begin
          for i:=0 to list0^.size-1 do if boolLit[true].equals(list0^.value[i])
          then listResult^.append(recycler,arg1^.rereferenced,false)
          else listResult^.append(recycler,arg2^.rereferenced,false);
        end;
      end;
    end else result:=nil;
  end;

INITIALIZATION
  //Functions on lists:
  BUILTIN_HEAD:=
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'head'    ,@head_imp    ,ak_variadic_1);
  BUILTIN_TAIL:=
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'tail'    ,@tail_imp    ,ak_variadic_1);
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'leading' ,@leading_imp ,ak_variadic_1);
  BUILTIN_TRAILING:=
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'trailing',@trailing_imp,ak_variadic_1);
  {$ifdef fullVersion}sortLoc:={$endif}
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'sort'    ,@sort_imp    ,ak_variadic_1);
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'sortPerm'        ,@sortPerm_imp      ,ak_unary     );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'unique'          ,@unique_imp        ,ak_unary     );
  BUILTIN_ELEMENT_FREQUENCY:=
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'elementFrequency',@getElementFreqency,ak_unary     );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'transpose'       ,@transpose_imp     ,ak_unary     );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'union'           ,@setUnion_imp      ,ak_variadic_1);
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'intersect'       ,@setIntersect_imp  ,ak_variadic_1);
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'minus'           ,@setMinus_imp      ,ak_binary    );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'isSubsetOf'      ,@isSubsetOf_imp    ,ak_binary    );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'mergeMaps'       ,@mergeMaps_imp     ,ak_ternary   );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'flatten'         ,@flatten_imp       ,ak_variadic  );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'size'            ,@size_imp          ,ak_unary     );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'trueCount'       ,@trueCount_impl    ,ak_unary     );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'reverseList'     ,@reverseList_impl  ,ak_unary     );
  BUILTIN_GET:=
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'get'     ,@get_imp     ,ak_variadic_2);
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'getAll'  ,@getAll_imp  ,ak_binary    );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'getInner',@getInner_imp,ak_variadic_2);
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'indexOf' ,@indexOf_impl,ak_unary     );
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'cross'   ,@cross_impl  ,ak_variadic_2);
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'group',  @group_imp ,ak_variadic_2);
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'groupToList',  @groupToList_imp ,ak_variadic_4);
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'toGenerator'   ,
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'toIteratableExpression',@toGenerator_imp,ak_unary),ak_unary);
  builtinFunctionMap.registerRule(LIST_NAMESPACE,'vectorIfThenElse',@vectorIfThenElse_imp,ak_ternary);
end.
