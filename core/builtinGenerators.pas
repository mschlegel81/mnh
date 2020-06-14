UNIT builtinGenerators;
INTERFACE
USES sysutils,
     mySys,myCrypto,bigint,
     myStringUtil,
     mnh_constants,
     basicTypes,
     out_adapters,
     litVar,
     funcs,contexts,
     listProcessing,
     recyclers,
     subrules;

IMPLEMENTATION
USES mnh_settings,
     typinfo,
     serializationUtil;
{$i func_defines.inc}
TYPE
  P_listIterator=^T_listIterator;
  T_listIterator=object(T_builtinGeneratorExpression)
    index:longint;
    underlying:P_literal;
    values:T_arrayOfLiteral;
    CONSTRUCTOR create(CONST v:P_compoundLiteral; CONST location:T_tokenLocation);
    FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
    FUNCTION evaluateToLiteral({$WARN 5024 OFF}CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
  end;

CONSTRUCTOR T_listIterator.create(CONST v: P_compoundLiteral; CONST location:T_tokenLocation);
  begin
    inherited create(location);
    index:=0;
    underlying:=v^.rereferenced;
    values:=v^.iteratableList;
  end;

FUNCTION T_listIterator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:='listIterator('+underlying^.toString(lengthLimit-14)+')';
  end;

FUNCTION T_listIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  begin
    result.triggeredByReturn:=false;
    if index>=length(values)
    then result.literal:=newVoidLiteral
    else result.literal:=values[index]^.rereferenced;
    inc(index);
  end;

DESTRUCTOR T_listIterator.destroy;
  begin
    disposeLiteral(underlying);
    disposeLiteral(values);
  end;

TYPE
P_singleValueIterator=^T_singleValueIterator;
T_singleValueIterator=object(T_builtinGeneratorExpression)
  didDeliver:boolean;
  value:P_literal;
  CONSTRUCTOR create(CONST v:P_literal; CONST location:T_tokenLocation);
  FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
  FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
  DESTRUCTOR destroy; virtual;
  FUNCTION writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
end;

CONSTRUCTOR T_singleValueIterator.create(CONST v: P_literal; CONST location:T_tokenLocation);
  begin
    inherited create(location);
    didDeliver:=false;
    value:=v^.rereferenced;
  end;

FUNCTION T_singleValueIterator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:='singleValueIterator('+value^.toString(20)+')';
  end;

FUNCTION T_singleValueIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  begin
    if didDeliver
    then result.literal:=newVoidLiteral
    else result.literal:=value^.rereferenced;
    result.triggeredByReturn:=false;
    didDeliver:=true;
  end;

DESTRUCTOR T_singleValueIterator.destroy;
  begin
    disposeLiteral(value);
  end;

FUNCTION newIterator(CONST input:P_literal; CONST location:T_tokenLocation):P_expressionLiteral;
  begin
    if input^.literalType in C_compoundTypes then new(P_listIterator(result),create(P_compoundLiteral(input),location))
    else if (input^.literalType=lt_expression) and
            (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then result:=P_expressionLiteral(input^.rereferenced)
    else new(P_singleValueIterator(result),create(input,location));
  end;

TYPE
  P_rangeGenerator=^T_rangeGenerator;
  T_rangeGenerator=object(T_builtinGeneratorExpression)
    private
      bounding:(bUpper,bLower,bNone);
      workBig:boolean;
      smallNext,smallLim:int64;
      bigNext  ,bigLim  :T_bigInt;
    public
      CONSTRUCTOR create(CONST first,last:P_abstractIntLiteral; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
  end;

CONSTRUCTOR T_rangeGenerator.create(CONST first,last: P_abstractIntLiteral; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    workBig:=(last<>nil) and ((last^.literalType =lt_bigint) and not(P_bigIntLiteral(last )^.value.canBeRepresentedAsInt64))
                          or ((first^.literalType=lt_bigint) and not(P_bigIntLiteral(first)^.value.canBeRepresentedAsInt64));
    if workBig then begin
      if first^.literalType=lt_bigint
      then bigNext.create (P_bigIntLiteral(first)^.value)
      else bigNext.fromInt(first^.intValue);
    end else smallNext:=first^.intValue;

    if last=nil then begin
      bounding:=bNone;
    end else begin
      if last^.isInRelationTo(tt_comparatorGrt,first)
      then bounding:=bUpper
      else bounding:=bLower;
      if workBig then begin
        if last^.literalType=lt_bigint
        then bigLim.create (P_bigIntLiteral(last)^.value)
        else bigLim.fromInt(last^.intValue);
      end else smallLim:=last^.intValue;
    end;
  end;

FUNCTION T_rangeGenerator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    if workBig
    then result:='rangeGenerator('+bigNext.toString   +','+bigLim.toString   +')'
    else result:='rangeGenerator('+intToStr(smallNext)+','+intToStr(smallLim)+')';
  end;

FUNCTION T_rangeGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  VAR tmp:T_bigInt;
  begin
    result.triggeredByReturn:=false;
    if workBig then case bounding of
      bUpper:
        if bigNext.compare(bigLim) in [CR_LESSER,CR_EQUAL] then begin
          tmp:=bigNext+1;
          result.literal:=newIntLiteral(bigNext);
          bigNext:=tmp;
        end else result.literal:=newVoidLiteral;
      bLower:
        if bigNext.compare(bigLim) in [CR_GREATER,CR_EQUAL] then begin
          tmp:=bigNext-1;
          result.literal:=newIntLiteral(bigNext);
          bigNext:=tmp;
        end else result.literal:=newVoidLiteral;
      bNone : begin
        tmp:=bigNext+1;
        result.literal:=newIntLiteral(bigNext);
        bigNext:=tmp;
      end;
    end else case bounding of
      bUpper:
        if smallNext<=smallLim then begin
          result.literal:=newIntLiteral(smallNext);
          inc(smallNext);
        end else result.literal:=newVoidLiteral;
      bLower:
        if smallNext>=smallLim then begin
          result.literal:=newIntLiteral(smallNext);
          dec(smallNext);
        end else result.literal:=newVoidLiteral;
      bNone : begin
        result.literal:=newIntLiteral(smallNext);
        inc(smallNext);
        if smallNext=9223372036854775807 then begin
          workBig:=true;
          bigNext.fromInt(smallNext);
        end;
      end;
    end;
  end;

DESTRUCTOR T_rangeGenerator.destroy;
  begin
    if workBig then begin
      bigNext.clear;
      if bounding<>bNone then bigLim.clear;
    end;
  end;

FUNCTION rangeGenerator intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_smallint,lt_bigint]) and (arg1^.literalType in [lt_smallint,lt_bigint])
    then new(P_rangeGenerator(result),create(int0,int1,tokenLocation))
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint])
    then new(P_rangeGenerator(result),create(int0,nil,tokenLocation));
  end;

TYPE
  P_permutationIterator=^T_permutationIterator;
  T_permutationIterator=object(T_builtinGeneratorExpression)
    private
      underlying:P_literal;
      nextPermutation:T_arrayOfLiteral;
      first:boolean;
    public
      CONSTRUCTOR create(CONST i:int64; CONST loc:T_tokenLocation);
      CONSTRUCTOR create(CONST arr:P_compoundLiteral; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
  end;

CONSTRUCTOR T_permutationIterator.create(CONST i: int64; CONST loc: T_tokenLocation);
  VAR k:longint;
  begin
    inherited create(loc);
    underlying:=newIntLiteral(i);
    first:=true;
    setLength(nextPermutation,i);
    for k:=0 to i-1 do nextPermutation[k]:=newIntLiteral(k);
  end;

CONSTRUCTOR T_permutationIterator.create(CONST arr: P_compoundLiteral; CONST loc: T_tokenLocation);
  VAR sorted:P_listLiteral;
  begin
    inherited create(loc);
    underlying:=arr^.rereferenced;
    first:=true;
    sorted:=newListLiteral();
    sorted^.appendAll(arr);
    sorted^.sort;
    nextPermutation:=sorted^.iteratableList;
    disposeLiteral(sorted);
  end;

FUNCTION T_permutationIterator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:='permutationIterator('+underlying^.toString(lengthLimit-21)+')';
  end;

FUNCTION T_permutationIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  VAR i,k,l:longint;
      swapTmp:P_literal;
  begin
    result.triggeredByReturn:=false;
    if first then begin
      result.literal:=newListLiteral(length(nextPermutation));
      for i:=0 to length(nextPermutation)-1 do P_listLiteral(result.literal)^.append(nextPermutation[i],true);
      first:=false;
      exit(result);
    end;
    k:=-1;
    //find largest index k so that P[k]<P[k+1] <=> not(P[k+1]<=P[k]);
    for i:=0 to length(nextPermutation)-2 do if not(nextPermutation[i+1]^.leqForSorting(nextPermutation[i])) then k:=i;
    //no such k found: done
    if k<0 then begin result.literal:=newVoidLiteral; exit; end;
    //find largest index l so that P[k]<P[l] <=> not(P[l]<=P[k])
    l:=-1;
    for i:=0 to length(nextPermutation)-1 do if not(nextPermutation[i]^.leqForSorting(nextPermutation[k])) then l:=i;
    //swap entries with indexes k and l
    swapTmp           :=nextPermutation[k];
    nextPermutation[k]:=nextPermutation[l];
    nextPermutation[l]:=swapTmp;
    //reverse tail after k
    // k+1 <-> length(P)-1
    // k+2 <-> length(P)-2
    // ...
    // k+l <-> length(P)-l
    // l = (length(P)-k) shr 1
    l:=(length(nextPermutation)-k) shr 1;
    for i:=1 to l do begin
      swapTmp:=nextPermutation[k+i];
      nextPermutation[k+i]:=nextPermutation[length(nextPermutation)-i];
      nextPermutation[length(nextPermutation)-i]:=swapTmp;
    end;
    //construct result
    result.literal:=newListLiteral(length(nextPermutation));
    for i:=0 to length(nextPermutation)-1 do P_listLiteral(result.literal)^.append(nextPermutation[i],true);
  end;

DESTRUCTOR T_permutationIterator.destroy;
  begin
    disposeLiteral(underlying);
    disposeLiteral(nextPermutation);
  end;

FUNCTION permutationIterator intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_smallint then begin
        new(P_permutationIterator(result),create(int0^.intValue,tokenLocation));
      end else if arg0^.literalType in C_compoundTypes then begin
        new(P_permutationIterator(result),create(compound0,tokenLocation));
      end;
    end;
  end;

TYPE
  P_filterGenerator=^T_filterGenerator;
  T_filterGenerator=object(T_builtinGeneratorExpression)
    private
      sourceGenerator:P_expressionLiteral;
      filterExpression:P_expressionLiteral;
    public
      CONSTRUCTOR create(CONST source,filter:P_expressionLiteral; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
  end;

CONSTRUCTOR T_filterGenerator.create(CONST source,filter: P_expressionLiteral; CONST loc:T_tokenLocation);
  begin
    inherited create(loc);
    sourceGenerator:=source;
    sourceGenerator^.rereference;
    filterExpression:=filter;
    filterExpression^.rereference;
  end;

FUNCTION T_filterGenerator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:=sourceGenerator^.toString(30)+'.filter('+filterExpression^.toString(20)+')';
  end;

FUNCTION T_filterGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  VAR nextUnfiltered:T_evaluationResult;
  begin
    result:=NIL_EVAL_RESULT;
    repeat
      nextUnfiltered:=sourceGenerator^.evaluateToLiteral(location,context,recycler,nil,nil);
      if (nextUnfiltered.literal<>nil) and (nextUnfiltered.literal^.literalType<>lt_void) then begin
        if filterExpression^.evaluateToBoolean(location,context,recycler,true,nextUnfiltered.literal,nil)
        then exit          (nextUnfiltered)
        else disposeLiteral(nextUnfiltered.literal);
      end else begin
        if nextUnfiltered.literal=nil
        then begin result.triggeredByReturn:=false; result.literal:=newVoidLiteral; exit(result); end
        else exit(nextUnfiltered);
      end;
    until (result.literal<>nil) or not(P_context(context)^.messages^.continueEvaluation);
  end;

DESTRUCTOR T_filterGenerator.destroy;
  begin
    disposeLiteral(sourceGenerator);
    disposeLiteral(filterExpression);
  end;

FUNCTION filter_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_compoundTypes+[lt_expression]) and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      case arg0^.literalType of
        lt_list..lt_emptyMap: result:=processFilterSerial(P_compoundLiteral(arg0),P_expressionLiteral(arg1),tokenLocation,context,recycler);
        lt_expression: if (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes) then begin
          new(P_filterGenerator(result),create(P_expressionLiteral(arg0),P_expressionLiteral(arg1),tokenLocation));
        end;
      end;
    end;
  end;

FUNCTION parallelFilter_imp intFuncSignature;
  begin
    result:=nil;
    if not(tco_spawnWorker in context.threadOptions) or (context.callDepth>STACK_DEPTH_LIMIT-16) or not(isMemoryInComfortZone) then exit(filter_imp(params,tokenLocation,context,recycler));
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_compoundTypes+[lt_expression]) and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      case arg0^.literalType of
        lt_list..lt_emptyMap: result:=processFilterParallel(P_compoundLiteral(arg0),P_expressionLiteral(arg1),tokenLocation,context,recycler);
        lt_expression: if (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes) then begin
          new(P_filterGenerator(result),create(P_expressionLiteral(arg0),P_expressionLiteral(arg1),tokenLocation));
        end;
      end;
    end;
  end;

TYPE
  P_mapGenerator=^T_mapGenerator;
  T_mapGenerator=object(T_builtinGeneratorExpression)
    private
      sourceGenerator:P_expressionLiteral;
      mapExpression:P_expressionLiteral;
      isNullary:boolean;
    public
      CONSTRUCTOR create(CONST source,mapEx:P_expressionLiteral; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
  end;

CONSTRUCTOR T_mapGenerator.create(CONST source,mapEx: P_expressionLiteral; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    sourceGenerator:=source;
    mapExpression:=mapEx;
    mapExpression^.rereference;
    isNullary:=mapEx^.canApplyToNumberOfParameters(0);
  end;

FUNCTION T_mapGenerator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:=sourceGenerator^.toString(30)+'.lazyMap('+mapExpression^.toString(20)+')';
  end;

FUNCTION T_mapGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  VAR nextUnmapped:P_literal;
  begin
    result:=NIL_EVAL_RESULT;
    repeat
      nextUnmapped:=sourceGenerator^.evaluateToLiteral(location,context,recycler,nil,nil).literal;
      if (nextUnmapped<>nil) and (nextUnmapped^.literalType<>lt_void) then begin
        if isNullary then result:=mapExpression^.evaluateToLiteral(location,context,recycler,nil         ,nil)
                     else result:=mapExpression^.evaluateToLiteral(location,context,recycler,nextUnmapped,nil);
        disposeLiteral(nextUnmapped);
        //error handling
        if result.literal=nil then begin result.literal:=newVoidLiteral; result.triggeredByReturn:=false; exit(result); end;
        if result.literal^.literalType=lt_void then disposeLiteral(result.literal);
      end else begin
        if nextUnmapped=nil then begin result.literal:=newVoidLiteral; result.triggeredByReturn:=false; exit(result); end
                            else begin result.literal:=nextUnmapped;   result.triggeredByReturn:=false; exit(result); end;
      end;
    until (result.literal<>nil) or not(P_context(context)^.messages^.continueEvaluation);
  end;

DESTRUCTOR T_mapGenerator.destroy;
  begin
    disposeLiteral(sourceGenerator);
    disposeLiteral(mapExpression);
  end;

FUNCTION createLazyMapImpl(CONST generator,mapping:P_expressionLiteral; CONST tokenLocation:T_tokenLocation):P_builtinGeneratorExpression;
  begin
    new(P_mapGenerator(result),create(newIterator(generator,tokenLocation),mapping,tokenLocation));
  end;

FUNCTION lazyMap_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and ((arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes) or (arg0^.literalType in C_compoundTypes))
                                          and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1) or P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(0)) then begin
      new(P_mapGenerator(result),create(newIterator(arg0,tokenLocation),P_expressionLiteral(arg1),tokenLocation));
    end;
  end;

TYPE
  P_futureMapGenerator=^T_futureMapGenerator;
  T_futureMapGenerator=object(T_builtinGeneratorExpression)
    private
      sourceGenerator:P_expressionLiteral;
      mapExpression:P_expressionLiteral;
      isNullary:boolean;
      firstToAggregate,
      lastToAggregate:P_futureMapLiteral;
      doneFetching:boolean;
      PROCEDURE enqueueNext(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: pointer);
    public
      CONSTRUCTOR create(CONST source,mapEx:P_expressionLiteral; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
  end;

PROCEDURE T_futureMapGenerator.enqueueNext(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: pointer);
  VAR nextUnmapped:P_literal;
      task:P_futureMapLiteral;
      taskParameters:P_listLiteral;
  begin
    nextUnmapped:=sourceGenerator^.evaluateToLiteral(location,context,recycler,nil,nil).literal;
    if nextUnmapped=nil then begin
      doneFetching:=true;
      exit;
    end;
    if nextUnmapped^.literalType=lt_void
    then begin
      doneFetching:=true;
      disposeLiteral(nextUnmapped);
      exit;
    end;
    if (P_context(context)^.messages^.continueEvaluation) then begin
      taskParameters:=newListLiteral(1);
      if not(isNullary) then taskParameters^.append(nextUnmapped,false)
                        else disposeLiteral(nextUnmapped);
      new(task,create(mapExpression,taskParameters,location));
      task^.rereference;
      enqueueFutureTask(task,P_context(context)^,P_recycler(recycler)^);
      disposeLiteral(taskParameters);
      if firstToAggregate=nil then begin
        firstToAggregate:=task;
        lastToAggregate:=task;
      end else begin
        lastToAggregate^.nextToAggregate:=task;
        lastToAggregate:=task;
      end;
    end else disposeLiteral(nextUnmapped);
  end;

CONSTRUCTOR T_futureMapGenerator.create(CONST source, mapEx: P_expressionLiteral; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    sourceGenerator:=source;
    mapExpression:=mapEx;
    mapExpression^.rereference;
    isNullary:=mapEx^.canApplyToNumberOfParameters(0);
    firstToAggregate:=nil;
    lastToAggregate:=nil;
    doneFetching:=false;
  end;

FUNCTION T_futureMapGenerator.toString(CONST lengthLimit: longint): string;
  begin
    result:=sourceGenerator^.toString(30)+'.futureMap('+mapExpression^.toString(20)+')';
  end;

FUNCTION T_futureMapGenerator.evaluateToLiteral(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: pointer; CONST a: P_literal; CONST b: P_literal): T_evaluationResult;
  VAR curr:P_futureMapLiteral;
  begin
    if firstToAggregate=nil then exit(NIL_EVAL_RESULT);
    repeat
      curr:=firstToAggregate;
      firstToAggregate:=firstToAggregate^.nextToAggregate;
      result:=curr^.evaluateToLiteral(location,context,recycler);
      disposeLiteral(curr);
      if not(doneFetching) then enqueueNext(location,context,recycler);
    until (firstToAggregate=nil) or (result.literal<>nil) and (result.literal^.literalType<>lt_void);
  end;

DESTRUCTOR T_futureMapGenerator.destroy;
  VAR curr:P_futureMapLiteral;
  begin
    while firstToAggregate<>nil do begin
      curr:=firstToAggregate;
      firstToAggregate:=firstToAggregate^.nextToAggregate;
      disposeLiteral(curr);
    end;
    disposeLiteral(sourceGenerator);
    disposeLiteral(mapExpression);
  end;

FUNCTION futureMap_imp intFuncSignature;
  CONST TASKS_TO_QUEUE_PER_CPU=4;
  VAR f:P_futureMapGenerator;
      i:longint;
  begin
    result:=nil;
    if not(tco_spawnWorker in context.threadOptions) then exit(lazyMap_imp(params,tokenLocation,context,recycler));
    if (params<>nil) and (params^.size=2) and ((arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes) or (arg0^.literalType in C_compoundTypes))
                                          and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1) or P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(0)) then begin
      new(f,create(newIterator(arg0,tokenLocation),P_expressionLiteral(arg1),tokenLocation));
      for i:=1 to TASKS_TO_QUEUE_PER_CPU*settings.cpuCount do f^.enqueueNext(tokenLocation,@context,@recycler);
      result:=f;
    end;
  end;

TYPE
  P_fileLineIterator=^T_fileLineIterator;
  T_fileLineIterator=object(T_builtinGeneratorExpression)
    private
      initialized:boolean;
      fname:string;
      fileHandle:textFile;
    public
      CONSTRUCTOR create(CONST fileName:string; CONST loc:T_tokenLocation; VAR context:T_context);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_fileLineIterator.create(CONST fileName: string; CONST loc: T_tokenLocation; VAR context: T_context);
  begin
    inherited create(loc);
    try
      fname:=fileName;
      assign(fileHandle,fileName);
      reset(fileHandle);
      initialized:=true;
    except
      context.raiseError('Error when trying to open file: '+fileName,loc);
      initialized:=false;
    end;
  end;

FUNCTION T_fileLineIterator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:='fileLineIterator('''+fname+''')';
  end;

FUNCTION T_fileLineIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  VAR l:string;
  begin
    result.triggeredByReturn:=false;
    if eof(fileHandle)
    then result.literal:=newVoidLiteral
    else begin
      readln(fileHandle,l);
      result.literal:=newStringLiteral(l);
    end;
  end;

DESTRUCTOR T_fileLineIterator.destroy;
  begin
    if initialized then try
      close(fileHandle);
    finally
    end;
  end;

FUNCTION fileLineIterator intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and fileExists(str0^.value) and context.checkSideEffects('fileLineIterator',tokenLocation,[se_readFile]) then begin
      new(P_fileLineIterator(result),create(str0^.value,tokenLocation,context));
      if not(P_fileLineIterator(result)^.initialized) then begin
        dispose(result,destroy);
        result:=nil;
      end;
    end;
  end;

TYPE
  P_primeGenerator=^T_primeGenerator;
  T_primeGenerator=object(T_builtinGeneratorExpression)
    private
      CONST
        CHUNK_SIZE_LOG2=16;
        CHUNK_SIZE=1 shl CHUNK_SIZE_LOG2;
      VAR table:array of bitpacked array[0..CHUNK_SIZE-1] of boolean;
      index:int64;
    public
      CONSTRUCTOR create(CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
  end;

CONSTRUCTOR T_primeGenerator.create(CONST loc: T_tokenLocation);
  VAR i,j:longint;
  begin
    inherited create(loc);
    setLength(table,1);
    index:=2;

    for i:=0 to CHUNK_SIZE-1 do table[0][i]:=i>=2;
    for i:=2 to 1 shl (CHUNK_SIZE_LOG2 div 2) do begin
      if table[0][i] then begin
        j:=i*i;
        while j<CHUNK_SIZE do begin
          table[0][j]:=false;
          inc(j,i);
        end;
      end;
    end;
  end;

FUNCTION T_primeGenerator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:='primeGenerator';
  end;

FUNCTION T_primeGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  PROCEDURE extendTable;
    VAR offset,newMax:int64;
        k0,k:longint;
        i,j:{$ifdef CPU32}longint{$else}int64{$endif};
    begin
      k0:=length(table);
      offset:=k0 shl CHUNK_SIZE_LOG2;
      setLength(table,k0*2);
      newMax:=int64(CHUNK_SIZE)*int64(length(table))-int64(1);
      {$ifdef debugMode}
      writeln(stdErr,'        DEBUG: computing primes in range ',offset,'..',newMax);
      {$endif}
      for k:=k0 to length(table)-1 do for i:=0 to CHUNK_SIZE-1 do table[k][i]:=odd(i);
      for i:=3 to round(sqrt(newMax)) do begin
        if table[i shr CHUNK_SIZE_LOG2][i and (CHUNK_SIZE-1)] then begin
          j:=i*i;
          while j<offset do inc(j,i);
          while j<=newMax do begin
            table[j shr CHUNK_SIZE_LOG2][j and (CHUNK_SIZE-1)]:=false;
            inc(j,i);
          end;
        end;
      end;
    end;

  begin
    if index>=int64(CHUNK_SIZE)*int64(length(table)) then extendTable;
    while not(table[index shr CHUNK_SIZE_LOG2][index and (CHUNK_SIZE-1)]) do begin
      inc(index);
      if index>=int64(CHUNK_SIZE)*int64(length(table)) then extendTable;
    end;
    result.triggeredByReturn:=false;
    result.literal:=newIntLiteral(index);
    inc(index);
  end;

DESTRUCTOR T_primeGenerator.destroy;
  begin
    setLength(table,0);
  end;

FUNCTION primeGenerator intFuncSignature;
  begin
    if (params=nil) or (params^.size=0)
    then new(P_primeGenerator(result),create(tokenLocation))
    else result:=nil;
  end;

TYPE
  P_stringIterator=^T_stringIterator;
  T_stringIterator=object(T_builtinGeneratorExpression)
    private
      charSet:array of char;
      currIdx:array of longint;
      minL,maxL:longint;
      first:boolean;
    public
      CONSTRUCTOR create(CONST loc:T_tokenLocation; CONST chars:T_charSet; CONST minLength,maxLength:longint);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_stringIterator.create(CONST loc:T_tokenLocation; CONST chars:T_charSet; CONST minLength,maxLength:longint);
  VAR i:longint;
      c:char;
  begin
    inherited create(loc);
    setLength(charSet,256);
    i:=0;
    for c in chars do begin
      charSet[i]:=c;
      inc(i);
    end;
    setLength(charSet,i);

    minL:=minLength;
    maxL:=maxLength;

    first:=true;
  end;

FUNCTION T_stringIterator.toString(CONST lengthLimit:longint=maxLongint):string;
  VAR i:longint;
      nonescapableFound:boolean;
  begin
    result:='stringIterator(['+escapeString(charSet[0],es_pickShortest,se_testPending,nonescapableFound);
    for i:=1 to length(charSet)-1 do result+=','+escapeString(charSet[i],es_pickShortest,se_testPending,nonescapableFound);
    result+='],'+intToStr(minL)+','+intToStr(maxL)+')';
  end;

FUNCTION T_stringIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  VAR s:string;
      i:longint=0;
      carry:boolean=true;
  begin
    if first then begin
      setLength(currIdx,minL);
      for i:=0 to minL-1 do currIdx[i]:=0;
      first:=false;
    end else begin
      i:=0;
      while (i<length(currIdx)) and carry do begin
        inc(currIdx[i]);
        if currIdx[i]>=length(charSet) then begin
          currIdx[i]:=0;
          inc(i);
        end else carry:=false
      end;
      if carry then begin
        setLength(currIdx,length(currIdx)+1);
        currIdx[length(currIdx)-1]:=0;
      end;
    end;
    if length(currIdx)>maxL then begin
      result.triggeredByReturn:=false;
      result.literal:=newVoidLiteral;
      exit(result);
    end;

    setLength(s,length(currIdx));
    for i:=0 to length(currIdx)-1 do s[length(s)-i]:=charSet[currIdx[i]];
    result.triggeredByReturn:=false;
    result.literal:=newStringLiteral(s);
  end;

DESTRUCTOR T_stringIterator.destroy;
  begin
    setLength(charSet,0);
    setLength(currIdx,0);
  end;

FUNCTION stringIterator intFuncSignature;
  VAR charSet:T_charSet;
      iter:T_arrayOfLiteral;
      c:P_literal;
      s:string;
      err:boolean=false;
  begin
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType in [lt_stringList,lt_stringSet]) and
       (arg1^.literalType=lt_smallint) and (int1^.intValue>=0) and
       (arg2^.literalType=lt_smallint) and (int2^.intValue>=int1^.intValue) then begin
      charSet:=[];
      iter:=collection0^.iteratableList;
      for c in iter do begin
        s:=P_stringLiteral(c)^.value;
        if length(s)=1
        then include(charSet,s[1])
        else begin
          err:=true;
          context.raiseError('Charset must only contain strings of 1 byte.',tokenLocation);
        end;
      end;
      if length(iter)=0 then begin
        err:=true;
        context.raiseError('Charset must contain at least one string of 1 byte',tokenLocation);
      end;
      disposeLiteral(iter);
      if err then result:=nil
             else new(P_stringIterator(result),create(tokenLocation,charSet,int1^.intValue,int2^.intValue));
    end else result:=nil;
  end;

TYPE
  P_realRandomGenerator=^T_realRandomGenerator;
  T_realRandomGenerator=object(T_builtinGeneratorExpression)
    private
      XOS:T_xosPrng;
    public
      CONSTRUCTOR create(CONST seed:P_abstractIntLiteral; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
  end;

  T_abstractRandomGenerator=object(T_builtinGeneratorExpression)
    private
      range:T_bigInt;
    public
      CONSTRUCTOR create(CONST maxValExclusive:P_abstractIntLiteral; CONST loc:T_tokenLocation);
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST parameters:P_listLiteral):T_evaluationResult;  virtual;
      DESTRUCTOR destroy; virtual;
  end;

  P_intRandomGenerator=^T_intRandomGenerator;
  T_intRandomGenerator=object(T_abstractRandomGenerator)
    private
      XOS:T_xosPrng;
    public
      CONSTRUCTOR create(CONST seed,maxValExclusive:P_abstractIntLiteral; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
  end;

  P_isaacRandomGenerator=^T_isaacRandomGenerator;
  T_isaacRandomGenerator=object(T_abstractRandomGenerator)
    private
      isaac:T_ISAAC;
    public
      CONSTRUCTOR create(CONST seed,maxValExclusive:P_abstractIntLiteral; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_abstractRandomGenerator.create(CONST maxValExclusive: P_abstractIntLiteral; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    if maxValExclusive^.literalType=lt_bigint
    then range.create(P_bigIntLiteral(maxValExclusive)^.value)
    else range.fromInt(maxValExclusive^.intValue);
  end;

FUNCTION T_abstractRandomGenerator.canApplyToNumberOfParameters(CONST parCount: longint): boolean;
  begin
    result:=(parCount=0) or (parCount=1);
  end;

FUNCTION T_abstractRandomGenerator.evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST parameters:P_listLiteral):T_evaluationResult;
  begin
    result.triggeredByReturn:=false;
    result.literal:=nil;
    if (parameters=nil) or (parameters^.size=0) then begin
      result:=evaluateToLiteral(location,context,recycler,nil,nil);
    end else if (parameters<>nil) and (parameters^.size=1) and (parameters^.value[0]^.literalType in [lt_smallint,lt_bigint]) then begin
      range.clear;
      if parameters^.value[0]^.literalType=lt_smallint
      then range.fromInt(P_smallIntLiteral(parameters^.value[0])^.value)
      else range.create (P_bigIntLiteral  (parameters^.value[0])^.value);
      result.literal:=newStringLiteral('random range altered to '+parameters^.value[0]^.toString());
    end else begin
      P_context(context)^.raiseError('Cannot alter range by paramters '+toParameterListString(parameters,true),location);
      result.literal:=newVoidLiteral;
    end;
  end;

DESTRUCTOR T_abstractRandomGenerator.destroy;
  begin
    range.clear;
  end;

CONSTRUCTOR T_realRandomGenerator.create(CONST seed: P_abstractIntLiteral; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    XOS.create;
    if seed^.literalType=lt_bigint
    then XOS.resetSeed(P_bigIntLiteral(seed)^.value.getRawBytes)
    else XOS.resetSeed(seed^.intValue);
  end;

CONSTRUCTOR T_isaacRandomGenerator.create(CONST seed, maxValExclusive: P_abstractIntLiteral; CONST loc: T_tokenLocation);
  begin
    inherited create(maxValExclusive,loc);
    isaac.create;
    if seed^.literalType=lt_bigint
    then isaac.setSeed(P_bigIntLiteral(seed)^.value.getRawBytes)
    else isaac.setSeed(seed^.intValue);
  end;

CONSTRUCTOR T_intRandomGenerator.create(CONST seed, maxValExclusive: P_abstractIntLiteral; CONST loc: T_tokenLocation);
  begin
    inherited create(maxValExclusive,loc);
    XOS.create;
    if seed^.literalType=lt_bigint
    then XOS.resetSeed(P_bigIntLiteral(seed)^.value.getRawBytes)
    else XOS.resetSeed(seed^.intValue);
  end;

DESTRUCTOR T_realRandomGenerator.destroy;
  begin
    XOS.destroy;
  end;

DESTRUCTOR T_intRandomGenerator.destroy;
  begin
    XOS.destroy;
    inherited destroy;
  end;

DESTRUCTOR T_isaacRandomGenerator.destroy;
  begin
    isaac.destroy;
    inherited destroy;
  end;

FUNCTION T_realRandomGenerator.toString(CONST lengthLimit: longint): string;
  begin
    result:='randomGenerator';
  end;

FUNCTION T_intRandomGenerator.toString(CONST lengthLimit: longint): string;
  begin
    result:='intRandomGenerator('+range.toString+')';
  end;

FUNCTION T_isaacRandomGenerator.toString(CONST lengthLimit: longint): string;
  begin
    result:='isaacRandomGenerator('+range.toString+')';
  end;

FUNCTION T_realRandomGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  begin
    result.triggeredByReturn:=false;
    result.literal:=newRealLiteral(XOS.realRandom);
  end;

FUNCTION T_intRandomGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  begin
    result.triggeredByReturn:=false;
    result.literal:=newIntLiteral(bigint.randomInt(@XOS.dwordRandom,range));
  end;

FUNCTION T_isaacRandomGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  begin
    result.triggeredByReturn:=false;
    result.literal:=newIntLiteral(bigint.randomInt(@isaac.iRandom,range));
  end;

FUNCTION randomGenerator_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint]) then
      new(P_realRandomGenerator(result),create(int0,tokenLocation));
  end;

FUNCTION isValidIntRandomRange(CONST l:P_literal):boolean;
  begin
    result:=(l^.literalType=lt_smallint) and (P_smallIntLiteral(l)^.value>0) or
            (l^.literalType=lt_bigint) and not(P_bigIntLiteral(l)^.value.isNegative) and not(P_bigIntLiteral(l)^.value.isZero);
  end;

FUNCTION intRandomGenerator_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_smallint,lt_bigint]) and isValidIntRandomRange(arg1) then
      new(P_intRandomGenerator(result),create(int0,int1,tokenLocation));
  end;

FUNCTION isaacRandomGenerator_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_smallint,lt_bigint]) and isValidIntRandomRange(arg1) then
      new(P_isaacRandomGenerator(result),create(int0,int1,tokenLocation));
  end;

TYPE
P_vanDerCorputGenerator=^T_vanDerCorputGenerator;
T_vanDerCorputGenerator=object(T_builtinGeneratorExpression)
  private
    invTable:array[0..30] of double;
    base,counter:longint;
  public
    CONSTRUCTOR create(CONST base_:longint; CONST loc:T_tokenLocation);
    FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
    FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
    FUNCTION writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual;
end;

CONSTRUCTOR T_vanDerCorputGenerator.create(CONST base_: longint; CONST loc: T_tokenLocation);
  VAR i:longint;
      d:double=1;
  begin
    inherited create(loc);
    base:=base_;
    counter:=0;
    for i:=0 to length(invTable)-1 do begin
      d/=base;
      invTable[i]:=d;
    end;
  end;

FUNCTION T_vanDerCorputGenerator.toString(CONST lengthLimit: longint): string;
  begin
    result:='vanDerCorputGenerator('+intToStr(base)+')';
  end;

FUNCTION T_vanDerCorputGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  VAR k:longint;
      i:longint=0;
      x:double=0;
  begin
    k:=counter;
    inc(counter);
    while k>0 do begin
      x   +=(k mod base)*invTable[i];
      k   := k div base;
      inc(i);
    end;
    result.triggeredByReturn:=false;
    result.literal:=newRealLiteral(x);
  end;

FUNCTION vanDerCorputGenerator_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint]) and int0^.isBetween(2,maxLongint) then
      new(P_vanDerCorputGenerator(result),create(int0^.intValue,tokenLocation));
  end;

FUNCTION T_listIterator.writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean;
  begin
    stream^.writeByte(byte(typ));
    stream^.writeByte(byte(bgt_listIterator));
    writeLiteralToStream(underlying,stream,locationOfSerializeCall,adapters);
    result:=stream^.allOkay;
  end;

FUNCTION T_singleValueIterator.writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean;
  begin
    stream^.writeByte(byte(typ));
    stream^.writeByte(byte(bgt_singleValueIterator));
    writeLiteralToStream(value,stream,locationOfSerializeCall,adapters);
    result:=stream^.allOkay;
  end;

FUNCTION T_rangeGenerator.writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean;
  VAR tmp:T_bigInt;
  begin
    stream^.writeByte(byte(typ));
    stream^.writeByte(byte(bgt_rangeGenerator));
    stream^.writeBoolean(bounding<>bNone);
    if workBig then begin
      bigNext.writeToStream(stream);
      if bounding<>bNone
      then bigLim .writeToStream(stream);
    end else begin
      tmp.fromInt(smallNext);
      tmp.writeToStream(stream);
      if bounding<>bNone
      then begin
        tmp.fromInt(smallLim);
        tmp.writeToStream(stream);
      end;
    end;
    result:=stream^.allOkay;
  end;

FUNCTION T_permutationIterator.writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean;
  begin
    stream^.writeByte(byte(typ));
    stream^.writeByte(byte(bgt_permutationIterator));
    writeLiteralToStream(underlying,stream,locationOfSerializeCall,adapters);
    result:=true;
  end;

FUNCTION T_filterGenerator.writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean;
  begin
    stream^.writeByte(byte(typ));
    stream^.writeByte(byte(bgt_filterGenerator));
    writeLiteralToStream(sourceGenerator ,stream,locationOfSerializeCall,adapters);
    writeLiteralToStream(filterExpression,stream,locationOfSerializeCall,adapters);
    result:=true;
  end;

FUNCTION T_mapGenerator.writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean;
  begin
    stream^.writeByte(byte(typ));
    stream^.writeByte(byte(bgt_mapGenerator));
    writeLiteralToStream(sourceGenerator,stream,locationOfSerializeCall,adapters);
    writeLiteralToStream(mapExpression  ,stream,locationOfSerializeCall,adapters);
    result:=true;
  end;

FUNCTION T_primeGenerator.writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean;
  begin
    stream^.writeByte(byte(typ));
    stream^.writeByte(byte(bgt_primeGenerator));
    result:=true;
  end;

FUNCTION T_vanDerCorputGenerator.writeToStream(CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean;
  begin
    stream^.writeByte(byte(typ));
    stream^.writeByte(byte(bgt_vanDerCorputGenerator));
    stream^.writeNaturalNumber(base);
    result:=true;
  end;

FUNCTION newGeneratorFromStream(CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):P_builtinGeneratorExpression;
  VAR generatorType:T_builtinGeneratorType;
      lit,lit2:P_literal;
      bigint:T_bigInt;
      bounded:boolean;
  begin
    generatorType:=T_builtinGeneratorType(stream^.readByte([byte(low(T_builtinGeneratorType))..byte(high(T_builtinGeneratorType))]));
    result:=nil;
    case generatorType of
      bgt_listIterator: begin
        lit:=newLiteralFromStream(stream,location,adapters,typeMap);
        if lit^.literalType in C_compoundTypes
        then new(P_listIterator(result),create(P_compoundLiteral(lit),location))
        else stream^.logWrongTypeError;
        disposeLiteral(lit); //is rereferenced in T_listIterator.create;
      end;
      bgt_singleValueIterator: begin
        lit:=newLiteralFromStream(stream,location,adapters,typeMap);
        if lit^.literalType in C_compoundTypes
        then new(P_listIterator(result),create(P_compoundLiteral(lit),location))
        else stream^.logWrongTypeError;
        disposeLiteral(lit); //is rereferenced in T_singleValueIterator.create;
      end;
      bgt_rangeGenerator: begin
        bounded:=stream^.readBoolean;
        bigint.createZero;
        bigint.readFromStream(stream);
        lit:=newIntLiteral(bigint);
        if bounded then begin
          bigint.createZero;
          bigint.readFromStream(stream);
          lit2:=newIntLiteral(bigint);
        end else lit2:=nil;
        new(P_rangeGenerator(result),create(P_abstractIntLiteral(lit),P_abstractIntLiteral(lit2),location));
        disposeLiteral(lit);                    //is rereferenced in T_rangeGenerator.create
        if lit2<>nil then disposeLiteral(lit2); //is rereferenced in T_rangeGenerator.create
      end;
      bgt_permutationIterator: begin
        lit:=newLiteralFromStream(stream,location,adapters,typeMap);
        if lit^.literalType in C_compoundTypes
        then new(P_permutationIterator(result),create(P_compoundLiteral(lit),location))
        else if lit^.literalType=lt_smallint
             then new(P_permutationIterator(result),create(P_smallIntLiteral(lit)^.value,location));
        disposeLiteral(lit);
      end;
      bgt_filterGenerator: begin
        lit :=newLiteralFromStream(stream,location,adapters,typeMap);
        lit2:=newLiteralFromStream(stream,location,adapters,typeMap);
        if (lit^.literalType=lt_expression) and (lit2^.literalType=lt_expression)
        then new(P_filterGenerator(result),create(P_expressionLiteral(lit),P_expressionLiteral(lit2),location));
        disposeLiteral(lit);
        disposeLiteral(lit2);
      end;
      bgt_mapGenerator: begin
        lit :=newLiteralFromStream(stream,location,adapters,typeMap);
        lit2:=newLiteralFromStream(stream,location,adapters,typeMap);
        if (lit^.literalType=lt_expression) and (lit2^.literalType=lt_expression)
        then new(P_mapGenerator(result),create(P_expressionLiteral(lit),P_expressionLiteral(lit2),location));
        disposeLiteral(lit2);
      end;
      bgt_primeGenerator: new(P_primeGenerator(result),create(location));
      bgt_vanDerCorputGenerator: new(P_vanDerCorputGenerator(result),create(stream^.readNaturalNumber,location));
    end;
    if result=nil then begin
      stream^.logWrongTypeError;
      if adapters<>nil
      then adapters^.raiseSimpleError('Cannot deserialize generator/iterator expression of type: '+getEnumName(TypeInfo(generatorType),ord(generatorType)),location)
      else raise Exception.create    ('Cannot deserialize generator/iterator expression of type: '+getEnumName(TypeInfo(generatorType),ord(generatorType)));
    end;
  end;

INITIALIZATION
  createLazyMap:=@createLazyMapImpl;
  newGeneratorFromStreamCallback:=@newGeneratorFromStream;
  registerRule(MATH_NAMESPACE,'rangeGenerator',@rangeGenerator,ak_binary{$ifdef fullVersion},'rangeGenerator(i0:Int,i1:Int);//returns a generator generating the range [i0..i1]#rangeGenerator(start:Int);//returns an unbounded increasing generator'{$endif});
  registerRule(MATH_NAMESPACE,'permutationIterator',@permutationIterator,ak_binary{$ifdef fullVersion},'permutationIterator(i:Int);//returns a generator generating the permutations of [1..i]#permutationIterator(c:collection);//returns a generator generating permutationf of c'{$endif});
  registerRule(LIST_NAMESPACE,'filter', @filter_imp,ak_binary{$ifdef fullVersion},'filter(L,acceptor:expression(1));//Returns compound literal or generator L with all elements x for which acceptor(x) returns true'{$endif});
  registerRule(LIST_NAMESPACE,'pFilter', @parallelFilter_imp,ak_binary{$ifdef fullVersion},'pFilter(L,acceptor:expression(1));//Returns compound literal or generator L with all elements x for which acceptor(x) returns true#//As filter but processing in parallel'{$endif});
  registerRule(LIST_NAMESPACE,'lazyMap', @lazyMap_imp,ak_binary{$ifdef fullVersion},'lazyMap(G:expression(0),mapFunc:expression(1));//Returns generator G mapped using mapFunc'{$endif});
  registerRule(LIST_NAMESPACE,'futureMap', @futureMap_imp,ak_binary{$ifdef fullVersion},'futureMap(G:expression(0),mapFunc:expression(1));//Returns generator G mapped using mapFunc. The tasks are processed in parallel.'{$endif});
  registerRule(FILES_BUILTIN_NAMESPACE,'fileLineIterator', @fileLineIterator,ak_binary{$ifdef fullVersion},'fileLineIterator(filename:String);//returns an iterator over all lines in f'{$endif});
  registerRule(MATH_NAMESPACE,'primeGenerator',@primeGenerator,ak_nullary{$ifdef fullVersion},'primeGenerator;//returns a generator generating all prime numbers#//Note that this is an infinite generator!'{$endif});
  registerRule(STRINGS_NAMESPACE,'stringIterator',@stringIterator,ak_ternary{$ifdef fullVersion},'stringIterator(charSet:StringCollection,minLength>=0,maxLength>=minLength);//returns a generator generating all strings using the given chars'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'randomGenerator',@randomGenerator_impl,ak_unary{$ifdef fullVersion},'randomGenerator(seed:Int);//returns a XOS generator for real valued random numbers in range [0,1)'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'intRandomGenerator',@intRandomGenerator_impl,ak_binary{$ifdef fullVersion},'intRandomGenerator(seed:Int,range>0);//returns a XOS generator generating pseudo random integers in range [0,range)#//The range of the returned generator can be changed by calling it with an integer argument.'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'isaacRandomGenerator',@isaacRandomGenerator_impl,ak_binary{$ifdef fullVersion},'isaacRandomGenerator(seed:Int,range>0);//returns an ISAAC generator generating pseudo random integers in range [0,range)#//www.burtleburtle.net/bob/rand/isaacafa.html#//The range of the returned generator can be changed by calling it with an integer argument.'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'vanDerCorputGenerator',@vanDerCorputGenerator_impl,ak_unary{$ifdef fullVersion},'vanDerCorputGenerator(base>=2);//returns a Van der Corput generator for the given base'{$endif});
  listProcessing.newIterator:=@newIterator;
end.
