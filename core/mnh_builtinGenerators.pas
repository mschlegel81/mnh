UNIT mnh_builtinGenerators;
INTERFACE
USES sysutils,
     myGenerics,
     mnh_constants,
     mnh_basicTypes,
     mnh_litVar,
     mnh_funcs,mnh_contexts,mnh_out_adapters,
     listProcessing,
     mnh_subrules;

IMPLEMENTATION
{$i mnh_func_defines.inc}
TYPE
  P_rangeGenerator=^T_rangeGenerator;
  T_rangeGenerator=object(T_builtinGeneratorExpression)
    private
      nextVal,minVal,maxVal,increment:int64;
    public
      CONSTRUCTOR create(CONST min_,max_:int64; CONST loc:T_tokenLocation);
      FUNCTION getId:T_idString; virtual;
      FUNCTION next(CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal; virtual;
  end;

CONSTRUCTOR T_rangeGenerator.create(CONST min_, max_: int64; CONST loc: T_tokenLocation);
  begin
    inherited create(loc);
    nextVal:=min_;
    minVal:=min_;
    maxVal:=max_;
    if maxVal>minVal then increment:=1 else increment:=-1;
  end;

FUNCTION T_rangeGenerator.getId: T_idString;
  begin
    result:='rangeGenerator('+intToStr(minVal)+', '+intToStr(maxVal)+')';
  end;

FUNCTION T_rangeGenerator.next(CONST location: T_tokenLocation; VAR context: T_threadContext): P_literal;
  begin
    if (minVal<=nextVal) and (nextVal<=maxVal) then begin
      result:=newIntLiteral(nextVal);
      inc(nextVal,increment);
    end else result:=newVoidLiteral;
  end;

FUNCTION rangeGenerator intFuncSignature;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_int) and (arg1^.literalType=lt_int) then begin
      new(P_rangeGenerator(result),create(int0^.value,int1^.value,tokenLocation));
    end else result:=nil;
  end;

TYPE
  P_permutationIterator=^T_permutationIterator;
  T_permutationIterator=object(T_builtinGeneratorExpression)
    private
      nextPermutation:T_arrayOfLiteral;
    public
      CONSTRUCTOR create(CONST i:int64; CONST loc:T_tokenLocation);
      CONSTRUCTOR create(CONST arr:P_compoundLiteral; CONST loc:T_tokenLocation);
      FUNCTION getId:T_idString; virtual;
      FUNCTION next(CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_permutationIterator.create(CONST i: int64; CONST loc: T_tokenLocation);
  VAR k:longint;
  begin
    inherited create(loc);
    setLength(nextPermutation,i);
    for k:=0 to i-1 do nextPermutation[k]:=newIntLiteral(k);
  end;

CONSTRUCTOR T_permutationIterator.create(CONST arr: P_compoundLiteral; CONST loc: T_tokenLocation);
  VAR sorted:P_listLiteral;
  begin
    inherited create(loc);
    sorted:=newListLiteral();
    sorted^.appendAll(arr);
    sorted^.sort;
    nextPermutation:=sorted^.iteratableList;
    disposeLiteral(sorted);
  end;

FUNCTION T_permutationIterator.getId: T_idString;
  begin
    result:='permutationIterator';
  end;

FUNCTION T_permutationIterator.next(CONST location: T_tokenLocation; VAR context: T_threadContext): P_literal;
  VAR i,k,l:longint;
  begin
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
        new(P_permutationIterator(result),create(int0^.value,tokenLocation));
      end else if arg0^.literalType in C_compoundTypes then begin
        new(P_permutationIterator(result),create(compound0,tokenLocation));
      end;
    end;
  end;

TYPE
  P_filterGenerator=^T_filterGenerator;
  T_filterGenerator=object(T_builtinGeneratorExpression)
    private
      sourceGenerator:P_iterator;
      filterExpression:P_expressionLiteral;
    public
      CONSTRUCTOR create(CONST source:P_iterator; CONST filter:P_expressionLiteral; CONST loc:T_tokenLocation);
      FUNCTION getId:T_idString; virtual;
      FUNCTION next(CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_filterGenerator.create(CONST source: P_iterator; CONST filter: P_expressionLiteral; CONST loc:T_tokenLocation);
  begin
    inherited create(loc);
    sourceGenerator:=source;
    filterExpression:=filter;
    filterExpression^.rereference;
  end;

FUNCTION T_filterGenerator.getId: T_idString;
  begin
    result:='filter/generator';
  end;

FUNCTION T_filterGenerator.next(CONST location: T_tokenLocation; VAR context: T_threadContext): P_literal;
  VAR nextUnfiltered:P_literal;
  begin
    result:=nil;
    repeat
      nextUnfiltered:=sourceGenerator^.next(@context);
      if (nextUnfiltered<>nil) and (nextUnfiltered^.literalType<>lt_void) then begin
        if filterExpression^.evaluateToBoolean(location,@context,nextUnfiltered)
        then exit          (nextUnfiltered)
        else disposeLiteral(nextUnfiltered);
      end else begin
        if nextUnfiltered=nil then exit(newVoidLiteral)
                              else exit(nextUnfiltered);
      end;
    until (result<>nil) or not(context.adapters^.noErrors);
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
            mapResult^.put(P_listLiteral(x)^[0],P_listLiteral(x)^[1],true);
          disposeLiteral(iter);
        end;
        lt_list..lt_stringList,
        lt_set ..lt_stringSet: begin
          result:=collection0^.newOfSameType;
          iter:=collection0^.iteratableList;
          for x in iter do if P_expressionLiteral(arg1)^.evaluateToBoolean(tokenLocation,@context,x) then
            collResult^.append(x,true);
          disposeLiteral(iter);
        end;
        lt_expression: if (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(0)) and
                          (P_expressionLiteral(arg0)^.isStateful) then begin
          new(P_filterGenerator(result),create(newIterator(arg0),P_expressionLiteral(arg1),tokenLocation));
        end;
      end;
    end;
  end;

TYPE
  P_fileLineIterator=^T_fileLineIterator;
  T_fileLineIterator=object(T_builtinGeneratorExpression)
    private
      initialized:boolean;
      fileHandle:textFile;
    public
      CONSTRUCTOR create(CONST fileName:string; CONST loc:T_tokenLocation; VAR context:T_threadContext);
      FUNCTION getId:T_idString; virtual;
      FUNCTION next(CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_fileLineIterator.create(CONST fileName: string; CONST loc: T_tokenLocation; VAR context: T_threadContext);
  begin
    inherited create(loc);
    try
      assign(fileHandle,fileName);
      reset(fileHandle);
      initialized:=true;
    except
      context.adapters^.raiseError('Error when trying to open file: '+fileName,loc);
      initialized:=false;
    end;
  end;

FUNCTION T_fileLineIterator.getId: T_idString;
  begin
    result:='fileLineIterator';
  end;

FUNCTION T_fileLineIterator.next(CONST location: T_tokenLocation; VAR context: T_threadContext): P_literal;
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
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and fileExists(str0^.value) then begin
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
      table:array of bitpacked array[0..255] of boolean;
      index:int64;
    public
      CONSTRUCTOR create(CONST loc:T_tokenLocation);
      FUNCTION getId:T_idString; virtual;
      FUNCTION next(CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal; virtual;
      DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_primeGenerator.create(CONST loc: T_tokenLocation);
  VAR i,j:longint;
  begin
    inherited create(loc);
    setLength(table,1);
    index:=2;

    for i:=0 to 255 do table[0][i]:=i>=2;
    for i:=2 to 16 do begin
      if table[0][i] then begin
        j:=i*i;
        while j<=255 do begin
          table[0][j]:=false;
          inc(j,i);
        end;
      end;
    end;
  end;

FUNCTION T_primeGenerator.getId: T_idString;
  begin
    result:='primeGenerator';
  end;

FUNCTION T_primeGenerator.next(CONST location: T_tokenLocation; VAR context: T_threadContext): P_literal;
  PROCEDURE extendTable;
    VAR offset,newMax:int64;
        k0,k:longint;
        i,j:{$ifdef CPU32}longint{$else}int64{$endif};
    begin
      k0:=length(table);
      offset:=k0 shl 8;
      setLength(table,k0*2);
      newMax:=256*length(table)-1;
      {$ifdef debugMode}
      writeln(stdErr,'        DEBUG: computing primes in range ',offset,'..',newMax);
      {$endif}
      for k:=k0 to length(table)-1 do for i:=0 to 255 do table[k][i]:=odd(i);
      for i:=3 to round(sqrt(newMax)) do begin
        if table[i shr 8][i and 255] then begin
          j:=i*i;
          while j<offset do inc(j,i);
          while j<=newMax do begin
            table[j shr 8][j and 255]:=false;
            inc(j,i);
          end;
        end;
      end;
    end;

  begin
    if index>=256*length(table) then extendTable;
    while not(table[index shr 8][index and 255]) do begin
      inc(index);
      if index>=256*length(table) then extendTable;
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

INITIALIZATION
  registerRule(MATH_NAMESPACE,'rangeGenerator',@rangeGenerator,[],ak_binary,'rangeGenerator(i0:int,i1:int);//returns a generator generating the range [i0..i1]');
  registerRule(MATH_NAMESPACE,'permutationIterator',@permutationIterator,[],ak_binary,'permutationIterator(i:int);//returns a generator generating the permutations of [1..i]#permutationIterator(c:collection);//returns a generator generating permutationf of c');
  registerRule(LIST_NAMESPACE,'filter', @filter_imp,[],ak_binary,'filter(L,acceptor:expression(1));//Returns compound literal or generator L with all elements x for which acceptor(x) returns true');
  registerRule(LIST_NAMESPACE,'fileLineIterator', @fileLineIterator,[se_readingExternal],ak_binary,'fileLineIterator(filename:string);//returns an iterator over all lines in f');
  registerRule(MATH_NAMESPACE,'primeGenerator',@primeGenerator,[],ak_nullary,'primeGenerator;//returns a generator generating all prime numbers#//Note that this is an infinite generator!');
end.
