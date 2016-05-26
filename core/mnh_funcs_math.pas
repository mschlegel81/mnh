UNIT mnh_funcs_math;
INTERFACE
{$WARN 5024 OFF}
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,math,mnh_out_adapters,mnh_contexts;
VAR BUILTIN_MIN,
    BUILTIN_MAX:T_intFuncCallback;
IMPLEMENTATION
{$MACRO ON}
{$define arg0:=params^.value(0)}
{$define real0:=P_realLiteral(params^.value(0))}
{$define int0:=P_intLiteral(params^.value(0))}
{$define bool0:=P_boolLiteral(params^.value(0))}
{$define str0:=P_stringLiteral(params^.value(0))}
{$define list0:=P_listLiteral(params^.value(0))}

FUNCTION max_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR x:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then x:=arg0
    else x:=params;
    if (x<>nil) and (x^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
      result:=P_listLiteral(x)^.value(0);
      for i:=1 to P_listLiteral(x)^.size-1 do if P_scalarLiteral(P_listLiteral(x)^.value(i))^.isInRelationTo(tt_comparatorGrt,P_scalarLiteral(result)) then result:=P_listLiteral(x)^.value(i);
      result^.rereference;
    end;
  end;

FUNCTION argMax_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR x,xMin:P_scalarLiteral;
      L:P_listLiteral;
      i,iMin:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0<>nil) and (arg0^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
      L:=list0;
      iMin:=0;
      xMin:=P_scalarLiteral(L^.value(0));
      for i:=1 to L^.size-1 do begin
        x:=P_scalarLiteral(L^.value(i));
        if x^.isInRelationTo(tt_comparatorGrt,xMin) then begin
          iMin:=i;
          xMin:=x;
        end;
      end;
      result:=newIntLiteral(iMin);
    end;
  end;

FUNCTION min_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR x:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then x:=arg0
    else x:=params;
    if (x<>nil) and (x^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
      result:=P_listLiteral(x)^.value(0);
      for i:=1 to P_listLiteral(x)^.size-1 do if P_scalarLiteral(P_listLiteral(x)^.value(i))^.isInRelationTo(tt_comparatorLss,P_scalarLiteral(result)) then result:=P_listLiteral(x)^.value(i);
      result^.rereference;
    end;
  end;

FUNCTION argMin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR x,xMin:P_scalarLiteral;
      L:P_listLiteral;
      i,iMin:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0<>nil) and (arg0^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
      L:=list0;
      iMin:=0;
      xMin:=P_scalarLiteral(L^.value(0));
      for i:=1 to L^.size-1 do begin
        x:=P_scalarLiteral(L^.value(i));
        if x^.isInRelationTo(tt_comparatorLss,xMin) then begin
          iMin:=i;
          xMin:=x;
        end;
      end;
      result:=newIntLiteral(iMin);
    end;
  end;

FUNCTION isNan_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR i:longint;
      L:P_listLiteral;
      x:P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and
       (arg0^.literalType in [lt_real,lt_int,lt_realList,lt_intList,lt_numList,lt_emptyList]) then begin
      case arg0^.literalType of
        lt_real: exit(newBoolLiteral(isNan(real0^.value)));
        lt_int:  exit(newBoolLiteral(false));
        lt_intList: begin
          result:=newListLiteral;
          for i:=0 to list0^.size-1 do
            P_listLiteral(result)^.appendBool(false);
        end;
        else begin
          L:=list0;
          result:=newListLiteral;
          for i:=0 to L^.size-1 do begin
            x:=L^.value(i);
            P_listLiteral(result)^.appendBool((x^.literalType=lt_real) and isNan(P_realLiteral(x)^.value));
          end;
        end;
      end;
    end;
  end;

FUNCTION isInfinite_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR i:longint;
      L:P_listLiteral;
      x:P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and
       (arg0^.literalType in [lt_real,lt_int,lt_realList,lt_intList,lt_numList,lt_emptyList]) then begin
      case arg0^.literalType of
        lt_real: exit(newBoolLiteral(isInfinite(real0^.value)));
        lt_int:  exit(newBoolLiteral(false));
        lt_intList: begin
          result:=newListLiteral;
          for i:=0 to list0^.size-1 do
            P_listLiteral(result)^.appendBool(false);
        end;
        else begin
          L:=list0;
          result:=newListLiteral;
          for i:=0 to L^.size-1 do begin
            x:=L^.value(i);
            P_listLiteral(result)^.appendBool((x^.literalType=lt_real) and isInfinite(P_realLiteral(x)^.value));
          end;
        end;
      end;
    end;
  end;

FUNCTION subSets_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR sets:specialize G_literalKeyMap<byte>;
  PROCEDURE recurseBuildSets(CONST mustContain,mightContain:T_arrayOfLiteral);
    VAR newMust,newMight:T_arrayOfLiteral;
        newSet:P_listLiteral;
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
        newSet:=newListLiteral;
        for i:=0 to length(mustContain)-1 do newSet^.append(mustContain[i],true);
        newSet^.sort;
        if sets.get(newSet,0)=0 then sets.put(newSet,1)
                                else disposeLiteral(newSet);
      end;
    end;
  VAR mustContain,mightContain:T_arrayOfLiteral;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType in C_validListTypes then begin
        sets.create;
        setLength(mustContain,0);
        setLength(mightContain,list0^.size);
        for i:=0 to length(mightContain)-1 do mightContain[i]:=list0^.value(i);
        recurseBuildSets(mustContain,mightContain);
        mustContain:=sets.keySet;
        result:=newListLiteral;
        for i:=0 to length(mustContain)-1 do P_listLiteral(result)^.append(mustContain[i],false);
        sets.destroy;
      end else begin
        result:=newListLiteral^.append(newListLiteral                     ,false)
                              ^.append(newOneElementListLiteral(arg0,true),false);
      end;
    end;
  end;

FUNCTION factorize_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR n,d:int64;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin
      n:=int0^.value;
      if (n=1) or (n=0) then exit(newListLiteral^.appendInt(n));
      result:=newListLiteral;
      if n<0 then begin
        n:=-n;
        P_listLiteral(result)^.appendInt(-1);
      end;
      d:=2;
      while d<=n do begin
        while n mod d=0 do begin
          n:=n div d;
          P_listLiteral(result)^.appendInt(d);
        end;
        inc(d);
      end;
      if n>1 then P_listLiteral(result)^.appendInt(n);
    end;
  end;

FUNCTION primes_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION sievePrimes(CONST pMax:int64):P_listLiteral;
    VAR isPrime:array of boolean;
        i,p:longint;
    begin
      if pMax<2 then exit(newListLiteral);
      setLength(isPrime,pMax+1);
      isPrime[0]:=false;
      isPrime[1]:=false;
      for i:=2 to length(isPrime)-1 do isPrime[i]:=true;
      p:=2;
      while p*p<length(isPrime) do begin
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

INITIALIZATION
  registerRule(MATH_NAMESPACE,'max',@max_imp,'max(L);#Returns the greatest element out of list L#max(x,y,...);#Returns the greatest element out of the given parameters');
  registerRule(MATH_NAMESPACE,'argMax',@argMax_imp,'argMax(L);#Returns the index of the greatest element out of list L (or the first index if ambiguous)');
  registerRule(MATH_NAMESPACE,'min',@min_imp,'min(L);#Returns the smallest element out of list L#min(x,y,...);#Returns the smallest element out of the given parameters');
  registerRule(MATH_NAMESPACE,'argMin',@argMin_imp,'argMin(L);#Returns the index of the smallest element out of list L (or the first index if ambiguous)');
  registerRule(MATH_NAMESPACE,'isNan',@isNan_impl,'isNan(n);#Returns true if n is a number representing the value Not-A-Number');
  registerRule(MATH_NAMESPACE,'isInfinite',@isInfinite_impl,'isInfinite(n);#Returns true if n is a number representing an infinite value');
  registerRule(MATH_NAMESPACE,'subSets',@subSets_impl,'subSets(S);#Returns all distinct subsets of S');
  registerRule(MATH_NAMESPACE,'factorize',@factorize_impl,'factorize(i:int);#Returns a list of all prime factors of i');
  registerRule(MATH_NAMESPACE,'primes',@primes_impl,'primes(pMax:int);#Returns prime numbers up to pMax');
  BUILTIN_MIN:=@min_imp;
  BUILTIN_MAX:=@max_imp;
end.
