UNIT mnh_funcs_math;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,mnh_basicTypes,mnh_litVar,mnh_constants, mnh_funcs,math,mnh_out_adapters,mnh_contexts,myGenerics;
VAR BUILTIN_MIN,
    BUILTIN_MAX:P_intFuncCallback;
IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION pi_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then result:=newRealLiteral(pi) else result:=nil;
  end;

FUNCTION max_imp intFuncSignature;
  VAR x:P_literal;
      it:T_arrayOfLiteral;
  begin
    result:=nil;
    if params=nil then exit(nil);
    if (params^.size=1)
    then x:=arg0
    else x:=params;
    if x^.literalType in [lt_emptyList,lt_emptySet,lt_emptyMap] then exit(nil);
    if x^.literalType in C_scalarTypes then exit(x^.rereferenced);
    it:=P_compoundLiteral(x)^.iteratableList;
    result:=it[0];
    for x in it do if not(x^.leqForSorting(result)) then result:=x;
    result^.rereference;
    disposeLiteral(it);
  end;

FUNCTION min_imp intFuncSignature;
  VAR x:P_literal;
      it:T_arrayOfLiteral;
  begin
    result:=nil;
    if params=nil then exit(nil);
    if (params^.size=1)
    then x:=arg0
    else x:=params;
    if x^.literalType in [lt_emptyList,lt_emptySet,lt_emptyMap] then exit(nil);
    if x^.literalType in C_scalarTypes then exit(x^.rereferenced);
    it:=P_compoundLiteral(x)^.iteratableList;
    result:=it[0];
    for x in it do if x^.leqForSorting(result) then result:=x;
    result^.rereference;
    disposeLiteral(it);
  end;

FUNCTION argMax_imp intFuncSignature;
  VAR x,xMax:P_literal;
      L:P_listLiteral;
      i,imax:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0<>nil) and (arg0^.literalType in C_listTypes) then begin
      L:=list0;
      imax:=0;
      xMax:=L^[0];
      for i:=1 to L^.size-1 do begin
        x:=L^[i];
        if not(x^.leqForSorting(xMax)) then begin
          imax:=i;
          xMax:=x;
        end;
      end;
      result:=newIntLiteral(imax);
    end;
  end;

FUNCTION argMin_imp intFuncSignature;
  VAR x,xMin:P_literal;
      L:P_listLiteral;
      i,iMin:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0<>nil) and (arg0^.literalType in C_listTypes) then begin
      L:=list0;
      iMin:=0;
      xMin:=L^.value[0];
      for i:=1 to L^.size-1 do begin
        x:=L^.value[i];
        if x^.leqForSorting(xMin) then begin
          iMin:=i;
          xMin:=x;
        end;
      end;
      result:=newIntLiteral(iMin);
    end;
  end;

{$define nan_or_inf_impl:=
  VAR i:longint;
      x:P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and
       (arg0^.literalType in [lt_real,lt_int,
                              lt_realList,lt_intList,lt_numList,lt_emptyList,
                              lt_realSet ,lt_intSet ,lt_numSet ,lt_emptySet]) then begin
      case arg0^.literalType of
        lt_real: exit(newBoolLiteral(PREDICATE(real0^.value)));
        lt_int:  exit(newBoolLiteral(false));
        lt_emptyList,lt_emptySet: exit(arg0^.rereferenced);
        lt_intList: begin
          result:=newListLiteral(list0^.size);
          for i:=0 to list0^.size-1 do listResult^.appendBool(false);
        end;
        lt_intSet: exit(newSetLiteral^.appendBool(false));
        else begin
          result:=collection0^.newOfSameType;
          for i:=0 to collection0^.size-1 do begin
            x:=collection0^[i];
            collResult^.appendBool((x^.literalType=lt_real) and PREDICATE(P_realLiteral(x)^.value));
          end;
        end;
      end;
    end;
  end}

FUNCTION isNan_impl      {$define PREDICATE:=isNan}      intFuncSignature; nan_or_inf_impl;
FUNCTION isInfinite_impl {$define PREDICATE:=isInfinite} intFuncSignature; nan_or_inf_impl;
{$undef nan_or_inf_impl}
{$undef PREDICATE}

FUNCTION subSets_impl intFuncSignature;
  VAR sets:specialize G_literalKeyMap<byte>;
  PROCEDURE recurseBuildSets(CONST mustContain,mightContain:T_arrayOfLiteral);
    VAR newMust,newMight:T_arrayOfLiteral;
        newSet:P_collectionLiteral;
        i:longint;
    begin
      if length(mightContain)>0 then begin
        setLength(newMight,length(mightContain)-1);
        for i:=0 to length(newMight)-1 do newMight[i]:=mightContain[i+1];
        setLength(newMust,length(mustContain));
        for i:=0 to length(newMust)-1 do newMust[i]:=mustContain[i];
        recurseBuildSets(newMust,newMight);
        setLength(newMust,length(newMust)+1);
        newMust[length(newMust)-1]:=mightContain[0];
        recurseBuildSets(newMust,newMight);
        setLength(newMust,0);
        setLength(newMight,0);
      end else begin
        newSet:=collection0^.newOfSameType;
        for i:=0 to length(mustContain)-1 do newSet^.append(mustContain[i],true);
        if newSet^.literalType in C_listTypes then P_listLiteral(newSet)^.sort;
        if sets.get(newSet,0)=0 then sets.put(newSet,1)
                                else disposeLiteral(newSet);
      end;
    end;
  VAR mustContain,mightContain:T_arrayOfLiteral;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (arg0^.literalType in C_listTypes) or (arg0^.literalType in C_setTypes) then begin
        sets.create;
        setLength(mustContain,0);
        mightContain:=P_compoundLiteral(arg0)^.iteratableList;
        recurseBuildSets(mustContain,mightContain);
        disposeLiteral(mightContain);
        mustContain:=sets.keySet;
        result:=newListLiteral;
        for i:=0 to length(mustContain)-1 do listResult^.append(mustContain[i],false);
        sets.destroy;
      end else begin
        result:=newListLiteral^.append(newListLiteral                   ,false)
                              ^.append(newListLiteral^.append(arg0,true),false);
      end;
    end;
  end;

FUNCTION permutations_impl intFuncSignature;
  VAR mustContain,mightContain:T_arrayOfLiteral;
      i:longint;
  PROCEDURE recurseBuildPermutations(CONST mustContain,mightContain:T_arrayOfLiteral);
    VAR newMust,newMight:T_arrayOfLiteral;
        newList:P_listLiteral;
        i,j,k:longint;
    begin
      if length(mightContain)>0 then begin
        setLength(newMust ,length(mustContain )+1);
        for k:=0 to length(mustContain)-1 do newMust[k]:=mustContain[k];
        setLength(newMight,length(mightContain)-1);
        for i:=0 to length(mightContain)-1 do begin
          newMust[length(newMust)-1]:=mightContain[i];
          k:=0;
          for j:=0 to length(mightContain)-1 do if i<>j then begin
            newMight[k]:=mightContain[j]; inc(k);
          end;
          recurseBuildPermutations(newMust,newMight);
        end;
        setLength(newMust,0);
        setLength(newMight,0);
      end else begin
        newList:=newListLiteral(length(mustContain));
        for i:=0 to length(mustContain)-1 do newList^.append(mustContain[i],true);
        setResult^.append(newList,false);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and ((arg0^.literalType in C_listTypes) or (arg0^.literalType in C_setTypes)) then begin
      setLength(mustContain,0);
      setLength(mightContain,list0^.size);
      for i:=0 to length(mightContain)-1 do mightContain[i]:=compound0^.value[i];
      result:=newSetLiteral;
      recurseBuildPermutations(mustContain,mightContain);
    end;
  end;

FUNCTION factorize_impl intFuncSignature;
  {$define DIVIDE_THROUGH:=while n mod p=0 do begin n:=n div p; listResult^.appendInt(p); end}
  VAR n,p:int64;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin
      n:=int0^.value;
      if (n=1) or (n=0) then exit(newListLiteral^.appendInt(n));
      result:=newListLiteral;
      if n<0 then begin
        n:=-n;
        listResult^.appendInt(-1);
      end;
      p:=2; DIVIDE_THROUGH;
      p:=3; DIVIDE_THROUGH;
      p:=5; DIVIDE_THROUGH;
      p:=7;
      while (p*p<n) and (context.adapters^.noErrors) do begin
        DIVIDE_THROUGH; inc(p,4); // n*30 +  7
        DIVIDE_THROUGH; inc(p,2); // n*30 + 11
        DIVIDE_THROUGH; inc(p,4); // n*30 + 13
        DIVIDE_THROUGH; inc(p,2); // n*30 + 17
        DIVIDE_THROUGH; inc(p,4); // n*30 + 19
        DIVIDE_THROUGH; inc(p,6); // n*30 + 23
        DIVIDE_THROUGH; inc(p,2); // n*30 + 29
        DIVIDE_THROUGH; inc(p,6); // n*30 + 31
      end;
      if n>1 then listResult^.appendInt(n);
    end;
  end;

FUNCTION primes_impl intFuncSignature;
  FUNCTION sievePrimes(CONST pMax:int64):P_listLiteral;
    VAR isPrime:array of boolean;
        i,p:longint;
    begin
      if pMax<2 then exit(newListLiteral);
      setLength(isPrime,pMax+1);
      isPrime[0]:=false;
      isPrime[1]:=false;
      for i:=2 to length(isPrime)-1 do
        isPrime[i]:=true;
      p:=2;
      while (p*p<length(isPrime)) and context.adapters^.noErrors do begin
        i:=p*p;
        while i<length(isPrime) do begin
          isPrime[i]:=false;
          inc(i,p);
        end;
        inc(p);
        while (p<length(isPrime)) and not(isPrime[p]) do inc(p);
      end;
      result:=newListLiteral;
      for i:=2 to length(isPrime)-1 do if isPrime[i] then result^.appendInt(i);
      setLength(isPrime,0);
    end;

  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int)
    then result:=sievePrimes(int0^.value)
    else result:=nil;
  end;

FUNCTION digits_impl intFuncSignature;
  VAR base:int64=10;
  FUNCTION digitsOf(i:int64):P_listLiteral;
    VAR digit:array[0..63] of int64;
        digitCount:longint=0;
        k:longint;
    begin
      if i<0 then begin
        context.adapters^.raiseError('Cannot determine digits of negative integer '+intToStr(i),tokenLocation);
        exit(newListLiteral);
      end;
      if i=0 then begin
        digit[0]:=0;
        digitCount:=1;
      end else while i>0 do begin
        digit[digitCount]:=i mod base;
        i:=i div base;
        inc(digitCount);
      end;
      result:=newListLiteral(digitCount);
      for k:=digitCount-1 downto 0 do result^.appendInt(digit[k]);
    end;

  VAR j:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=2) and
      (arg0^.literalType in [lt_int,lt_emptyList,lt_intList,lt_emptySet,lt_intSet]) and
      ((params^.size<2) or (arg1^.literalType=lt_int))  then begin
      if params^.size=2 then base:=int1^.value;
      if base<=1 then begin
        context.adapters^.raiseError('Cannot determine digits with base '+arg1^.toString,tokenLocation);
        exit(nil);
      end;
      if arg0^.literalType=lt_int then exit(digitsOf(int0^.value));
      result:=collection0^.newOfSameType;
      for j:=0 to list0^.size-1 do collResult^.append(digitsOf(P_intLiteral(list0^.value[j])^.value),false);
    end;
  end;

FUNCTION arctan2_impl intFuncSignature;
  VAR x,y:T_myFloat;
  begin
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType in [lt_int,lt_real]) and
       (arg1^.literalType in [lt_int,lt_real]) then begin
      if arg0^.literalType=lt_int then x:=int0^.value
                                  else x:=real0^.value;
      if arg1^.literalType=lt_int then y:=int1^.value
                                  else y:=real1^.value;
      result:=newRealLiteral(arctan2(x,y));
    end else result:=nil;
  end;

INITIALIZATION
  registerRule(MATH_NAMESPACE,'pi'          ,@pi_imp           ,true,ak_nullary   ,'pi;//Returns pi');
  BUILTIN_MAX:=
  registerRule(MATH_NAMESPACE,'max'         ,@max_imp          ,true,ak_variadic_1,'max(L);//Returns the greatest element out of list L#max(x,y,...);//Returns the greatest element out of the given parameters');
  registerRule(MATH_NAMESPACE,'argMax'      ,@argMax_imp       ,true,ak_unary     ,'argMax(L);//Returns the index of the greatest element out of list L (or the first index if ambiguous)');
  BUILTIN_MIN:=
  registerRule(MATH_NAMESPACE,'min'         ,@min_imp          ,true,ak_variadic_1,'min(L);//Returns the smallest element out of list L#min(x,y,...);//Returns the smallest element out of the given parameters');
  registerRule(MATH_NAMESPACE,'argMin'      ,@argMin_imp       ,true,ak_unary     ,'argMin(L);//Returns the index of the smallest element out of list L (or the first index if ambiguous)');
  registerRule(MATH_NAMESPACE,'isNan'       ,@isNan_impl       ,true,ak_unary     ,'isNan(n);//Returns true if n is a number representing the value Not-A-Number');
  registerRule(MATH_NAMESPACE,'isInfinite'  ,@isInfinite_impl  ,true,ak_unary     ,'isInfinite(n);//Returns true if n is a number representing an infinite value');
  registerRule(MATH_NAMESPACE,'subSets'     ,@subSets_impl     ,true,ak_unary     ,'subSets(S);//Returns all distinct subsets of S');
  registerRule(MATH_NAMESPACE,'permutations',@permutations_impl,true,ak_unary     ,'permutations(L:list);//Returns a list of all permutations of S');
  registerRule(MATH_NAMESPACE,'factorize'   ,@factorize_impl   ,true,ak_unary     ,'factorize(i:int);//Returns a list of all prime factors of i');
  registerRule(MATH_NAMESPACE,'primes'      ,@primes_impl      ,true,ak_unary     ,'primes(pMax:int);//Returns prime numbers up to pMax');
  registerRule(MATH_NAMESPACE,'digits'      ,@digits_impl      ,true,ak_variadic_1,'digits(i>=0);//Returns the digits of i (base 10)#digits(i>=0,base>1);//Returns the digits of i for a custom base');
  registerRule(MATH_NAMESPACE,'arctan2'     ,@arctan2_impl     ,true,ak_binary    ,'arctan2(x,y);//Calculates arctan(x/y) and returns an angle in the correct quadrant');
end.
