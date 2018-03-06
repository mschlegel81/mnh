UNIT mnh_builtinGenerators;
INTERFACE
USES sysutils,
     mySys,myCrypto,bigint,
     myGenerics,
     myStringUtil,
     mnh_constants,
     mnh_basicTypes,
     mnh_litVar,
     mnh_funcs,mnh_contexts,mnh_out_adapters,
     listProcessing,
     mnh_subrules;

IMPLEMENTATION
{$i mnh_func_defines.inc}
TYPE
  P_listIterator=^T_listIterator;
  T_listIterator=object(T_builtinGeneratorExpression)
    index:longint;
    id:string;
    values:T_arrayOfLiteral;
    CONSTRUCTOR create(CONST v:P_compoundLiteral);
    FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
    FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_singleValueIterator=^T_singleValueIterator;
  T_singleValueIterator=object(T_builtinGeneratorExpression)
    didDeliver:boolean;
    value:P_literal;
    CONSTRUCTOR create(CONST v:P_literal);
    FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
    FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
    DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_listIterator.create(CONST v: P_compoundLiteral);
  begin
    init(lt_expression);
    index:=0;
    id:='listIterator('+v^.toString(20)+')';
    values:=v^.iteratableList;
  end;

FUNCTION T_listIterator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:=id;
  end;

FUNCTION T_listIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
  begin
    if index>=length(values)
    then result:=nil
    else result:=values[index]^.rereferenced;
    inc(index);
  end;

DESTRUCTOR T_listIterator.destroy;
  begin
    disposeLiteral(values);
  end;

CONSTRUCTOR T_singleValueIterator.create(CONST v: P_literal);
  begin
    init(lt_expression);
    didDeliver:=false;
    value:=v^.rereferenced;
  end;

FUNCTION T_singleValueIterator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:='singleValueIterator('+value^.toString(20)+')';
  end;

FUNCTION T_singleValueIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
  begin
    if didDeliver then exit(nil);
    result:=value^.rereferenced;
    didDeliver:=true;
  end;

DESTRUCTOR T_singleValueIterator.destroy;
  begin
    disposeLiteral(value);
  end;

FUNCTION newIterator(CONST input:P_literal):P_expressionLiteral;
  begin
    if input^.literalType in C_compoundTypes then new(P_listIterator(result),create(P_compoundLiteral(input)))
    else if (input^.literalType=lt_expression) and
            (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then result:=P_expressionLiteral(input^.rereferenced)
    else new(P_singleValueIterator(result),create(input));
  end;

TYPE
  P_rangeGenerator=^T_rangeGenerator;
  T_rangeGenerator=object(T_builtinGeneratorExpression)
    private
      nextVal,minVal,maxVal,increment:int64;
    public
      CONSTRUCTOR create(CONST first,last:int64; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
  end;

CONSTRUCTOR T_rangeGenerator.create(CONST first,last: int64; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    nextVal:=first;
    if last>=first then begin
      maxVal:=last;
      minVal:=first;
      increment:=1;
    end else begin
      maxVal:=first;
      minVal:=last;
      increment:=-1;
    end;
  end;

FUNCTION T_rangeGenerator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:='rangeGenerator('+intToStr(minVal)+','+intToStr(maxVal)+')';
  end;

FUNCTION T_rangeGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
  begin
    if (minVal<=nextVal) and (nextVal<=maxVal) then begin
      result:=newIntLiteral(nextVal);
      inc(nextVal,increment);
    end else result:=newVoidLiteral;
  end;

FUNCTION rangeGenerator intFuncSignature;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_int) and (arg1^.literalType=lt_int) then begin
      new(P_rangeGenerator(result),create(int0^.value.toInt,int1^.value.toInt,tokenLocation));
    end else result:=nil;
  end;

TYPE
  P_permutationIterator=^T_permutationIterator;
  T_permutationIterator=object(T_builtinGeneratorExpression)
    private
      nextPermutation:T_arrayOfLiteral;
      id:string;
      first:boolean;
    public
      CONSTRUCTOR create(CONST i:int64; CONST loc:T_tokenLocation);
      CONSTRUCTOR create(CONST arr:P_compoundLiteral; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_permutationIterator.create(CONST i: int64; CONST loc: T_tokenLocation);
  VAR k:longint;
  begin
    inherited create(loc);
    id:='permutationIterator('+intToStr(i)+')';
    first:=true;
    setLength(nextPermutation,i);
    for k:=0 to i-1 do nextPermutation[k]:=newIntLiteral(k);
  end;

CONSTRUCTOR T_permutationIterator.create(CONST arr: P_compoundLiteral; CONST loc: T_tokenLocation);
  VAR sorted:P_listLiteral;
  begin
    inherited create(loc);
    id:='permutationIterator('+arr^.toString(20)+')';
    first:=true;
    sorted:=newListLiteral();
    sorted^.appendAll(arr);
    sorted^.sort;
    nextPermutation:=sorted^.iteratableList;
    disposeLiteral(sorted);
  end;

FUNCTION T_permutationIterator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:=id;
  end;

FUNCTION T_permutationIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
  VAR i,k,l:longint;
  begin
    if first then begin
      result:=newListLiteral(length(nextPermutation));
      for i:=0 to length(nextPermutation)-1 do P_listLiteral(result)^.append(nextPermutation[i],true);
      first:=false;
      exit(result);
    end;
    k:=-1;
    //find largest index k so that P[k]<P[k+1] <=> not(P[k+1]<=P[k]);
    for i:=0 to length(nextPermutation)-2 do if not(nextPermutation[i+1]^.leqForSorting(nextPermutation[i])) then k:=i;
    //no such k found: done
    if k<0 then exit(newVoidLiteral);
    //find largest index l so that P[k]<P[l] <=> not(P[l]<=P[k])
    l:=-1;
    for i:=0 to length(nextPermutation)-1 do if not(nextPermutation[i]^.leqForSorting(nextPermutation[k])) then l:=i;
    //swap entries with indexes k and l
    result            :=nextPermutation[k];
    nextPermutation[k]:=nextPermutation[l];
    nextPermutation[l]:=result;
    //reverse tail after k
    // k+1 <-> length(P)-1
    // k+2 <-> length(P)-2
    // ...
    // k+l <-> length(P)-l
    // l = (length(P)-k) shr 1
    l:=(length(nextPermutation)-k) shr 1;
    for i:=1 to l do begin
      result:=nextPermutation[k+i];
      nextPermutation[k+i]:=nextPermutation[length(nextPermutation)-i];
      nextPermutation[length(nextPermutation)-i]:=result;
    end;
    //construct result
    result:=newListLiteral(length(nextPermutation));
    for i:=0 to length(nextPermutation)-1 do P_listLiteral(result)^.append(nextPermutation[i],true);
  end;

DESTRUCTOR T_permutationIterator.destroy;
  begin
    disposeLiteral(nextPermutation);
  end;

FUNCTION permutationIterator intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_int then begin
        new(P_permutationIterator(result),create(int0^.value.toInt,tokenLocation));
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
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
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

FUNCTION T_filterGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
  VAR nextUnfiltered:P_literal;
  begin
    result:=nil;
    repeat
      nextUnfiltered:=sourceGenerator^.evaluateToLiteral(location,context);
      if (nextUnfiltered<>nil) and (nextUnfiltered^.literalType<>lt_void) then begin
        if filterExpression^.evaluateToBoolean(location,context,nextUnfiltered)
        then exit          (nextUnfiltered)
        else disposeLiteral(nextUnfiltered);
      end else begin
        if nextUnfiltered=nil then exit(newVoidLiteral)
                              else exit(nextUnfiltered);
      end;
    until (result<>nil) or not(P_threadContext(context)^.adapters^.noErrors);
  end;

DESTRUCTOR T_filterGenerator.destroy;
  begin
    dispose(sourceGenerator,destroy);
    disposeLiteral(filterExpression);
  end;

FUNCTION filter_imp intFuncSignature;
  VAR iter:T_arrayOfLiteral;
      x:P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_compoundTypes+[lt_expression]) and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      case arg0^.literalType of
        lt_emptyList,
        lt_emptySet,
        lt_emptyMap: result:=arg0^.rereferenced;
        lt_map:begin
          result:=newMapLiteral;
          iter:=map0^.iteratableList;
          for x in iter do if P_expressionLiteral(arg1)^.evaluateToBoolean(tokenLocation,@context,x) then
            mapResult^.put(P_listLiteral(x)^.value[0],P_listLiteral(x)^.value[1],true);
          disposeLiteral(iter);
        end;
        lt_list..lt_stringList,
        lt_set ..lt_stringSet: begin
          result:=collection0^.newOfSameType(false);
          iter:=collection0^.iteratableList;
          for x in iter do if P_expressionLiteral(arg1)^.evaluateToBoolean(tokenLocation,@context,x) then
            collResult^.append(x,true);
          disposeLiteral(iter);
        end;
        lt_expression: if (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes) then begin
          new(P_filterGenerator(result),create(P_expressionLiteral(arg0),P_expressionLiteral(arg1),tokenLocation));
        end;
      end;
    end;
  end;

FUNCTION parallelFilter_imp intFuncSignature;
  VAR iterator:P_expressionLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in C_compoundTypes+[lt_expression]) and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      case arg0^.literalType of
        lt_emptyList,
        lt_emptySet,
        lt_emptyMap: result:=arg0^.rereferenced;
        lt_map:begin
          result:=newMapLiteral;
          iterator:=newIterator(map0);
          processFilterParallel(iterator,P_expressionLiteral(arg1),tokenLocation,context,P_mapLiteral(result));
          disposeLiteral(iterator);
        end;
        lt_list..lt_stringList,
        lt_set ..lt_stringSet: begin
          result:=collection0^.newOfSameType(false);
          iterator:=newIterator(collection0);
          processFilterParallel(iterator,P_expressionLiteral(arg1),tokenLocation,context,P_collectionLiteral(result));
          disposeLiteral(iterator);
        end;
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
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
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

FUNCTION T_mapGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
  VAR nextUnmapped:P_literal;
  begin
    result:=nil;
    repeat
      nextUnmapped:=sourceGenerator^.evaluateToLiteral(location,context);
      if (nextUnmapped<>nil) and (nextUnmapped^.literalType<>lt_void) then begin
        if isNullary then result:=mapExpression^.evaluateToLiteral(location,context)
                     else result:=mapExpression^.evaluateToLiteral(location,context,nextUnmapped);
        disposeLiteral(nextUnmapped);
        //error handling
        if result=nil then exit(newVoidLiteral);
        if result^.literalType=lt_void then disposeLiteral(result);
      end else begin
        if nextUnmapped=nil then exit(newVoidLiteral)
                            else exit(nextUnmapped);
      end;
    until (result<>nil) or not(P_threadContext(context)^.adapters^.noErrors);
  end;

DESTRUCTOR T_mapGenerator.destroy;
  begin
    dispose(sourceGenerator,destroy);
    disposeLiteral(mapExpression);
  end;

FUNCTION createLazyMapImpl(CONST generator,mapping:P_expressionLiteral; CONST tokenLocation:T_tokenLocation):P_builtinGeneratorExpression;
  begin
    new(P_mapGenerator(result),create(newIterator(generator),mapping,tokenLocation));
  end;

FUNCTION lazyMap_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes)
                                          and (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1) or P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(0)) then begin
      new(P_mapGenerator(result),create(newIterator(arg0),P_expressionLiteral(arg1),tokenLocation));
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
      CONSTRUCTOR create(CONST fileName:string; CONST loc:T_tokenLocation; VAR context:T_threadContext);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_fileLineIterator.create(CONST fileName: string; CONST loc: T_tokenLocation; VAR context: T_threadContext);
  begin
    inherited create(loc);
    try
      fname:=fileName;
      assign(fileHandle,fileName);
      reset(fileHandle);
      initialized:=true;
    except
      context.adapters^.raiseError('Error when trying to open file: '+fileName,loc);
      initialized:=false;
    end;
  end;

FUNCTION T_fileLineIterator.toString(CONST lengthLimit:longint=maxLongint):string;
  begin
    result:='fileLineIterator('''+fname+''')';
  end;

FUNCTION T_fileLineIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
  VAR l:string;
  begin
    if eof(fileHandle) then exit(newVoidLiteral);
    readln(fileHandle,l);
    result:=newStringLiteral(l);
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
        CHUNK_SIZE_LOG2=12;
        CHUNK_SIZE=1 shl CHUNK_SIZE_LOG2;
      VAR table:array of bitpacked array[0..CHUNK_SIZE-1] of boolean;
      index:int64;
    public
      CONSTRUCTOR create(CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
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

FUNCTION T_primeGenerator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
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
    result:=newIntLiteral(index);
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
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
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
  begin
    result:='stringIterator(['+escapeString(charSet[0],es_pickShortest);
    for i:=1 to length(charSet)-1 do result+=','+escapeString(charSet[i],es_pickShortest);
    result+='],'+intToStr(minL)+','+intToStr(maxL)+')';
  end;

FUNCTION T_stringIterator.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal;
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
    if length(currIdx)>maxL then exit(newVoidLiteral);

    setLength(s,length(currIdx));
    for i:=0 to length(currIdx)-1 do s[length(s)-i]:=charSet[currIdx[i]];
    result:=newStringLiteral(s);
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
       (arg1^.literalType=lt_int) and (int1^.value.toInt>=0) and
       (arg2^.literalType=lt_int) and (int2^.value.toInt>=int1^.value.toInt) then begin
      charSet:=[];
      iter:=collection0^.iteratableList;
      for c in iter do begin
        s:=P_stringLiteral(c)^.value;
        if length(s)=1
        then include(charSet,s[1])
        else begin
          err:=true;
          context.adapters^.raiseError('Charset must only contain strings of 1 byte.',tokenLocation);
        end;
      end;
      if length(iter)=0 then begin
        err:=true;
        context.adapters^.raiseError('Charset must contain at least one string of 1 byte',tokenLocation);
      end;
      disposeLiteral(iter);
      if err then result:=nil
             else new(P_stringIterator(result),create(tokenLocation,charSet,int1^.value.toInt,int2^.value.toInt));
    end else result:=nil;
  end;

TYPE
  P_realRandomGenerator=^T_realRandomGenerator;
  T_realRandomGenerator=object(T_builtinGeneratorExpression)
    private
      XOS:T_xosPrng;
    public
      CONSTRUCTOR create(CONST seed:T_bigInt; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

  P_intRandomGenerator=^T_intRandomGenerator;
  T_intRandomGenerator=object(T_builtinGeneratorExpression)
    private
      XOS:T_xosPrng;
      range:T_bigInt;
    public
      CONSTRUCTOR create(CONST seed,maxValExclusive:T_bigInt; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

  P_isaacRandomGenerator=^T_isaacRandomGenerator;
  T_isaacRandomGenerator=object(T_builtinGeneratorExpression)
    private
      isaac:T_ISAAC;
      range:T_bigInt;
    public
      CONSTRUCTOR create(CONST seed,maxValExclusive:T_bigInt; CONST loc:T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_realRandomGenerator.create(CONST seed: T_bigInt; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    XOS.create;
    XOS.resetSeed(seed.getRawBytes);
  end;

CONSTRUCTOR T_isaacRandomGenerator.create(CONST seed, maxValExclusive: T_bigInt; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    isaac.create;
    isaac.setSeed(seed.getRawBytes);
    range.create(maxValExclusive);
  end;

CONSTRUCTOR T_intRandomGenerator.create(CONST seed, maxValExclusive: T_bigInt; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    XOS.create;
    XOS.resetSeed(seed.getRawBytes);
    range.create(maxValExclusive);
  end;

DESTRUCTOR T_realRandomGenerator.destroy;
  begin
    XOS.destroy;
  end;

DESTRUCTOR T_intRandomGenerator.destroy;
  begin
    XOS.destroy;
    range.destroy;
  end;

DESTRUCTOR T_isaacRandomGenerator.destroy;
  begin
    isaac.destroy;
    range.destroy;
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

FUNCTION T_realRandomGenerator.evaluateToLiteral(CONST location: T_tokenLocation; CONST context: pointer; CONST a: P_literal; CONST b: P_literal): P_literal;
  begin
    result:=newRealLiteral(XOS.realRandom);
  end;

FUNCTION T_intRandomGenerator.evaluateToLiteral(CONST location: T_tokenLocation; CONST context: pointer; CONST a: P_literal; CONST b: P_literal): P_literal;
  begin
    result:=newIntLiteral(bigint.randomInt(@XOS.dwordRandom,range));
  end;

FUNCTION T_isaacRandomGenerator.evaluateToLiteral(CONST location: T_tokenLocation; CONST context: pointer; CONST a: P_literal; CONST b: P_literal): P_literal;
  begin
    result:=newIntLiteral(bigint.randomInt(@isaac.iRandom,range));
  end;

FUNCTION randomGenerator_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then
      new(P_realRandomGenerator(result),create(int0^.value,tokenLocation));
  end;

FUNCTION intRandomGenerator_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_int) and (arg1^.literalType=lt_int) and not(int1^.value.isNegative) and not(int1^.value.isZero) then
      new(P_intRandomGenerator(result),create(int0^.value,int1^.value,tokenLocation));
  end;

FUNCTION isaacRandomGenerator_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_int) and (arg1^.literalType=lt_int) and not(int1^.value.isNegative) and not(int1^.value.isZero) then
      new(P_isaacRandomGenerator(result),create(int0^.value,int1^.value,tokenLocation));
  end;

INITIALIZATION
  createLazyMap:=@createLazyMapImpl;
  registerRule(MATH_NAMESPACE,'rangeGenerator',@rangeGenerator,ak_binary,'rangeGenerator(i0:int,i1:int);//returns a generator generating the range [i0..i1]');
  registerRule(MATH_NAMESPACE,'permutationIterator',@permutationIterator,ak_binary,'permutationIterator(i:int);//returns a generator generating the permutations of [1..i]#permutationIterator(c:collection);//returns a generator generating permutationf of c');
  registerRule(LIST_NAMESPACE,'filter', @filter_imp,ak_binary,'filter(L,acceptor:expression(1));//Returns compound literal or generator L with all elements x for which acceptor(x) returns true');
  registerRule(LIST_NAMESPACE,'pFilter', @parallelFilter_imp,ak_binary,'pFilter(L,acceptor:expression(1));//Returns compound literal or generator L with all elements x for which acceptor(x) returns true#//As filter but processing in parallel');
  registerRule(LIST_NAMESPACE,'lazyMap', @lazyMap_imp,ak_binary,'lazyMap(G:expression(0),mapFunc:expression(1));//Returns generator G mapped using mapFunc');
  registerRule(FILES_BUILTIN_NAMESPACE,'fileLineIterator', @fileLineIterator,ak_binary,'fileLineIterator(filename:string);//returns an iterator over all lines in f');
  registerRule(MATH_NAMESPACE,'primeGenerator',@primeGenerator,ak_nullary,'primeGenerator;//returns a generator generating all prime numbers#//Note that this is an infinite generator!');
  registerRule(STRINGS_NAMESPACE,'stringIterator',@stringIterator,ak_ternary,'stringIterator(charSet:T_stringCollection,minLength>=0,maxLength>=minLength);//returns a generator generating all strings using the given chars');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'randomGenerator',@randomGenerator_impl,ak_unary,'randomGenerator(seed:Int);//returns a XOS generator for real valued random numbers in range [0,1)');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'intRandomGenerator',@intRandomGenerator_impl,ak_binary,'intRandomGenerator(seed:Int,range>0);//returns a XOS generator generating pseudo random integers in range [0,range)');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'isaacRandomGenerator',@isaacRandomGenerator_impl,ak_binary,'isaacRandomGenerator(seed:Int,range>0);//returns an ISAAC generator generating pseudo random integers in range [0,range)');
  listProcessing.newIterator:=@newIterator;
end.
