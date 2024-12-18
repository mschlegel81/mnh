UNIT funcs_math;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     math,
     bigint,
     myGenerics,
     basicTypes,mnh_constants,
     litVar,
     funcs,
     mnh_messages,
     out_adapters,
     recyclers,
     contexts;

TYPE KahanBabushkaKleinSummation=object
       sum,cs,ccs,c,cc:double;
       PROCEDURE init;
       PROCEDURE addTerm(CONST k:double);
       FUNCTION getSum:double;
     end;

VAR BUILTIN_MIN,
    BUILTIN_MAX:P_intFuncCallback;
IMPLEMENTATION
USES mySys,heaps,complex;
{$i func_defines.inc}
PROCEDURE KahanBabushkaKleinSummation.init;
  begin
    sum:=0;
    cs:=0;
    ccs:=0;
    c:=0;
    cc:=0;
  end;

PROCEDURE KahanBabushkaKleinSummation.addTerm(CONST k: double);
  VAR t:double;
  begin
    t:=sum+k;
    if abs(sum)>=abs(k)
    then c:=(sum-t)+k
    else c:=(k-t)+sum;
    sum:=t;
    t:=cs+c;
    if abs(cs)>=abs(c)
    then cc:=(cs-t)+c
    else cc:=(c-t)+cs;
    cs:=t;
    ccs+=cc;
  end;

FUNCTION KahanBabushkaKleinSummation.getSum: double;
  begin
    result:=sum+cs+ccs;
  end;

 FUNCTION sqrt_imp intFuncSignature;
  VAR intRoot:int64;
      bigRoot:T_bigInt;
      fltRoot:T_myFloat;
      isSquare:boolean;
  begin
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_smallint:begin
        fltRoot:=sqrt(P_smallIntLiteral(arg0)^.value);
        intRoot:=trunc(fltRoot);
        isSquare:=P_smallIntLiteral(arg0)^.value=intRoot*intRoot;
        if isSquare then result:=recycler^.newIntLiteral (intRoot)
                    else result:=recycler^.newRealLiteral(fltRoot);
      end;
      lt_bigint: begin
        bigRoot:=P_bigIntLiteral(arg0)^.value.iSqrt(false,RM_DOWN,isSquare);
        if isSquare then result:=recycler^.newIntLiteral(bigRoot)
        else begin
          bigRoot.clear;
          result:=recycler^.newRealLiteral(sqrt(P_bigIntLiteral(arg0)^.floatValue));
        end;
      end;
      lt_real: result:=recycler^.newRealLiteral(sqrt(P_realLiteral(arg0)^.value))
      else result:=genericVectorization('sqrt',params,tokenLocation,context,recycler);
    end else result:=nil;
  end;

FUNCTION isqrt_imp intFuncSignature;
  VAR intRoot:int64;
      bigRoot:T_bigInt;
      isSquare:boolean;
      roundingMode:T_roundingMode=RM_DEFAULT;
  begin
    if (params<>nil) and (params^.size>=1) and (params^.size<=2) then begin
      if params^.size=2 then begin
        if arg1^.literalType = lt_smallint then begin
          if P_smallIntLiteral(arg1)^.value<0 then roundingMode:=RM_DOWN else
          if P_smallIntLiteral(arg1)^.value>0 then roundingMode:=RM_UP;
        end else exit(nil);
      end;
      case arg0^.literalType of
        lt_smallint:begin
          case roundingMode of
            RM_UP:      intRoot:=ceil64(sqrt(P_smallIntLiteral(arg0)^.value));
            RM_DOWN:    intRoot:=trunc (sqrt(P_smallIntLiteral(arg0)^.value));
            RM_DEFAULT: intRoot:=round (sqrt(P_smallIntLiteral(arg0)^.value));
          end;
          result:=recycler^.newListLiteral(2)^
            .appendInt (recycler, intRoot)^
            .appendBool(recycler, P_smallIntLiteral(arg0)^.value=intRoot*intRoot);
        end;
        lt_bigint: begin
          bigRoot:=P_bigIntLiteral(arg0)^.value.iSqrt(true,roundingMode,isSquare);
          result:=recycler^.newListLiteral(2)^
            .append    (recycler, recycler^.newIntLiteral(bigRoot),false)^
            .appendBool(recycler, isSquare);
        end;
        else result:=genericVectorization('isqrt',params,tokenLocation,context,recycler);
      end;
    end else result:=nil;
  end;

FUNCTION sin_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.newRealLiteral(system.sin(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('sin',params,tokenLocation,context,recycler);
  end;

FUNCTION arcsin_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.newRealLiteral(arcsin(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('arcsin',params,tokenLocation,context,recycler);
  end;

FUNCTION cos_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.newRealLiteral(system.cos(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('cos',params,tokenLocation,context,recycler);
  end;

FUNCTION arccos_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.newRealLiteral(arccos(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('arccos',params,tokenLocation,context,recycler);
  end;

FUNCTION tan_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.newRealLiteral(math.tan(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('tan',params,tokenLocation,context,recycler);
  end;

FUNCTION arctan_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.newRealLiteral(arctan(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('arctan',params,tokenLocation,context,recycler);
  end;

FUNCTION exp_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.newRealLiteral(exp(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('exp',params,tokenLocation,context,recycler);
  end;

FUNCTION ln_imp intFuncSignature;
  VAR X:T_bigInt;
      r:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_smallint: result:=recycler^.newRealLiteral(system.ln(P_smallIntLiteral(arg0)^.value));
      lt_real    : result:=recycler^.newRealLiteral(system.ln(real0^.value));
      lt_bigint  : begin
        if P_bigIntLiteral(arg0)^.value.isNegative
        then result:=nanLit.rereferenced
        else begin
          r:=P_bigIntLiteral(arg0)^.value.relevantBits-512;
          if r<0
          then result:=recycler^.newRealLiteral(system.ln(P_bigIntLiteral(arg0)^.value.toFloat))
          else begin
            x.create(P_bigIntLiteral(arg0)^.value);
            x.shiftRight(r);
            result:=recycler^.newRealLiteral(system.ln(x.toFloat)+system.ln(2)*r);
            x.clear;
          end;
        end;
      end;
      lt_list..lt_emptySet,lt_expression: result:=genericVectorization('ln',params,tokenLocation,context,recycler);
    end;
  end;

FUNCTION abs_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then
    case arg0^.literalType of
      lt_smallint: if P_smallIntLiteral(arg0)^.value<0
                   then result:=recycler^.newIntLiteral(-P_smallIntLiteral(arg0)^.value)
                   else result:=arg0^.rereferenced;
      lt_bigint : if P_bigIntLiteral(arg0)^.value.isNegative
                  then result:=recycler^.newIntLiteral(P_bigIntLiteral(arg0)^.value.negated)
                  else result:=arg0^.rereferenced;
      lt_real: if P_realLiteral(arg0)^.value<0
               then result:=recycler^.newRealLiteral(-P_realLiteral(arg0)^.value)
               else result:=arg0^.rereferenced;
      else result:=genericVectorization('abs',params,tokenLocation,context,recycler);
    end;
  end;

FUNCTION sqr_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then case arg0^.literalType of
      lt_smallint : result:=recycler^.newIntLiteral(system.sqr(int64(P_smallIntLiteral (arg0)^.value)));
      lt_bigint   : result:=recycler^.newIntLiteral(P_bigIntLiteral(arg0)^.value*
                                          P_bigIntLiteral(arg0)^.value);
      lt_real: result:=recycler^.newRealLiteral(system.sqr(P_realLiteral(arg0)^.value));
      else result:=genericVectorization('sqr',params,tokenLocation,context,recycler);
    end;
  end;

FUNCTION customRound(CONST x:P_literal; CONST relevantDigits:longint; CONST roundingMode:T_roundingMode;
                     CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  CONST funcName:array[T_roundingMode] of string=('round',  //RM_DEFAULT,
                                                  'ceil',   //RM_UP,
                                                  'floor'); //RM_DOWN
  FUNCTION floor64(CONST d:T_myFloat):int64; begin result:=trunc(d); if frac(d)<0 then dec(result); end;
  FUNCTION ceil64 (CONST d:T_myFloat):int64; begin result:=trunc(d); if frac(d)>0 then inc(result); end;
  FUNCTION myRound(CONST x:T_myFloat; CONST y:int64):P_literal; inline;
    VAR pot:T_myFloat;
        i:int64;
    begin
      result:=nil;
      pot:=1;
      i:=0;
      while (i<y) and (i< 20) do begin pot:=pot*10;  inc(i); end;
      while (i>y) and (i>-20) do begin pot:=pot*0.1; dec(i); end;
      case roundingMode of
        RM_DEFAULT: result:=recycler^.newRealLiteral(round  (x*pot)/pot);
        RM_UP     : result:=recycler^.newRealLiteral(ceil64 (x*pot)/pot);
        RM_DOWN   : result:=recycler^.newRealLiteral(floor64(x*pot)/pot);
      end;
    end;

  FUNCTION myRound(CONST x:P_abstractIntLiteral; CONST y:int64):P_literal; inline;
    VAR i   :longint=0;
        pot :DigitType=1;
        xv  :int64;
    begin
      if y>=0 then exit(x^.rereferenced);
      while (i>y) and (i>-19) do begin pot:=pot*10; dec(i); end;
      if not((x^.literalType=lt_smallint) or P_bigIntLiteral(x)^.value.canBeRepresentedAsInt64()) then begin
        raiseNotApplicableError(funcName[roundingMode],x,location,context,' because it is a big integer');
        exit(nil);
      end;
      xv:=x^.intValue;
      result:=recycler^.newIntLiteral((xv div pot) * pot);
    end;

  VAR big:T_bigInt;
      f:T_myFloat;
  begin
    result:=nil;
    if relevantDigits=0 then begin
      result:=nil;
      case x^.literalType of
        lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction(funcName[roundingMode],location,context,recycler);
        lt_error,lt_smallint,lt_bigint: result:=x^.rereferenced;
        lt_real: if not(isNan(P_realLiteral(x)^.value)) and not(isInfinite(P_realLiteral(x)^.value))
                 then begin
                   f:=P_realLiteral(x)^.value;
                   if (-2147483646.0<f) and (f<2147483646.0)
                   then case roundingMode of
                     RM_DEFAULT: result:=recycler^.newIntLiteral(round  (f));
                     RM_UP     : result:=recycler^.newIntLiteral(ceil64 (f));
                     RM_DOWN   : result:=recycler^.newIntLiteral(floor64(f));
                   end else begin
                     big.fromFloat(P_realLiteral(x)^.value,roundingMode);
                     result:=recycler^.newIntLiteral(big);
                   end;
                 end;
        else raiseNotApplicableError(funcName[roundingMode],x,location,context);
      end;
    end else begin
      result:=nil;
      case x^.literalType of
        lt_error: result:=x^.rereferenced;
        lt_smallint,lt_bigint : result:=myRound(P_abstractIntLiteral(x),relevantDigits);
        lt_real: if not(isNan(P_realLiteral(x)^.value)) and not(isInfinite(P_realLiteral(x)^.value))
                 then result:=myRound(P_realLiteral(x)^.value,relevantDigits)
                 else raiseNotApplicableError(funcName[roundingMode],x,location,context);
        else raiseNotApplicableError(funcName[roundingMode],x,location,context);
      end;
    end;
  end;

FUNCTION round_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real,lt_expression])
    then result:=customRound(arg0,0             ,RM_DEFAULT,tokenLocation,context,recycler) else
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real,lt_expression]) and (arg1^.literalType in [lt_smallint,lt_bigint])
    then result:=customRound(arg0,int1^.intValue,RM_DEFAULT,tokenLocation,context,recycler)
    else result:=genericVectorization('round',params,tokenLocation,context,recycler);
  end;

FUNCTION ceil_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real,lt_expression])
    then result:=customRound(arg0,0             ,RM_UP,tokenLocation,context,recycler) else
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real,lt_expression]) and (arg1^.literalType in [lt_smallint,lt_bigint])
    then result:=customRound(arg0,int1^.intValue,RM_UP,tokenLocation,context,recycler)
    else result:=genericVectorization('ceil',params,tokenLocation,context,recycler);
  end;

FUNCTION floor_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real,lt_expression])
    then result:=customRound(arg0,0             ,RM_DOWN,tokenLocation,context,recycler) else
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real,lt_expression]) and (arg1^.literalType in [lt_smallint,lt_bigint])
    then result:=customRound(arg0,int1^.intValue,RM_DOWN,tokenLocation,context,recycler)
    else result:=genericVectorization('floor',params,tokenLocation,context,recycler);
  end;

FUNCTION sign_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_bigint: result:=recycler^.newIntLiteral(P_bigIntLiteral (arg0)^.value.sign);
      lt_smallint: if P_smallIntLiteral(arg0)^.value=0 then result:=arg0^.rereferenced
                   else if P_smallIntLiteral(arg0)^.value>0 then result:=recycler^.newIntLiteral(1)
                   else result:=recycler^.newIntLiteral(-1);
      lt_real: result:=recycler^.newIntLiteral(sign(P_realLiteral(arg0)^.value));
      else result:=genericVectorization('sign',params,tokenLocation,context,recycler);
    end;
  end;

FUNCTION pi_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then result:=recycler^.newRealLiteral(pi) else result:=nil;
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
    if x^.literalType in [lt_emptyList,lt_emptySet] then exit(newVoidLiteral);
    if x^.literalType in C_scalarTypes+C_mapTypes then exit(x^.rereferenced);
    it:=P_collectionLiteral(x)^.tempIterableList;
    result:=it[0];
    for x in it do if not(x^.leqForSorting(result)) then result:=x;
    result^.rereference;
  end;

FUNCTION leq_for_min(CONST a,b:P_literal):boolean; inline;
  begin
    if (b^.literalType=lt_real) and isNan(P_realLiteral(b)^.value) then exit(true);
    if (a^.literalType=lt_real) and isNan(P_realLiteral(a)^.value) then exit(false);
    result:=a^.leqForSorting(b);
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
    if x^.literalType in [lt_emptyList,lt_emptySet] then exit(newVoidLiteral);
    if x^.literalType in C_scalarTypes+C_mapTypes then exit(x^.rereferenced);
    it:=P_collectionLiteral(x)^.tempIterableList;
    result:=it[0];
    for x in it do if leq_for_min(x,result) then result:=x;
    result^.rereference;
  end;

FUNCTION argMax_imp intFuncSignature;
  VAR x,xMax:P_literal;
      L:P_listLiteral;
      i,imax:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0<>nil) and (arg0^.literalType in C_listTypes-[lt_emptyList]) then begin
      L:=list0;
      imax:=0;
      xMax:=L^.value[0];
      for i:=1 to L^.size-1 do begin
        x:=L^.value[i];
        if not(x^.leqForSorting(xMax)) then begin
          imax:=i;
          xMax:=x;
        end;
      end;
      result:=recycler^.newIntLiteral(imax);
    end;
  end;

FUNCTION argMin_imp intFuncSignature;
  VAR x,xMin:P_literal;
      L:P_listLiteral;
      i,iMin:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0<>nil) and (arg0^.literalType in C_listTypes-[lt_emptyList]) then begin
      L:=list0;
      iMin:=0;
      xMin:=L^.value[0];
      for i:=1 to L^.size-1 do begin
        x:=L^.value[i];
        if leq_for_min(x,xMin) then begin
          iMin:=i;
          xMin:=x;
        end;
      end;
      result:=recycler^.newIntLiteral(iMin);
    end;
  end;

{$define nan_or_inf_impl:=begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_real: exit(newBoolLiteral(PREDICATE(real0^.value)));
      lt_smallint,lt_bigint:  exit(newBoolLiteral(false));
      else result:=genericVectorization(FUNC_NAME,params,tokenLocation,context,recycler);
    end;
  end}

FUNCTION isNan_impl      {$define PREDICATE:=isNan}      {$define FUNC_NAME:='isNan'}      intFuncSignature; nan_or_inf_impl;
FUNCTION isInfinite_impl {$define PREDICATE:=isInfinite} {$define FUNC_NAME:='isInfinite'} intFuncSignature; nan_or_inf_impl;
{$undef nan_or_inf_impl}
{$undef PREDICATE}
{$undef FUNC_NAME}

FUNCTION subSets_impl intFuncSignature;
  TYPE T_multiset=array of record value:P_literal; multiplicity:longint; end;
       T_freqMap=specialize G_literalKeyMap<longint>;
  VAR resultSets:P_listLiteral;
      acceptOnlySetsOfSize:longint=-1;
      memoryPanic:boolean=false;

  PROCEDURE recurseBuildSets(CONST mustContain: T_arrayOfLiteral; CONST mightContain:T_multiset);
    VAR newMust :T_arrayOfLiteral;
        newMight:T_multiset;
        newSet:P_collectionLiteral;
        i:longint;
    begin
      if not(memoryCleaner .isMemoryInComfortZone) or memoryPanic then begin
        memoryPanic:=true;
        exit;
      end;
      if length(mightContain)>0 then begin
        setLength(newMight,length(mightContain)-1); for i:=0 to length(newMight)-1 do newMight[i]:=mightContain[i+1];
        setLength(newMust ,length(mustContain )  ); for i:=0 to length(newMust )-1 do newMust [i]:=mustContain[i];
        recurseBuildSets(newMust,newMight);
        for i:=1 to mightContain[0].multiplicity do if not(memoryPanic) then begin
          setLength(newMust,length(newMust)+1);
          newMust[length(newMust)-1]:=mightContain[0].value;
          recurseBuildSets(newMust,newMight);
        end;
        setLength(newMust,0);
        setLength(newMight,0);
      end else begin
        if (acceptOnlySetsOfSize<>-1) and (acceptOnlySetsOfSize<>length(mustContain)) then exit;
        if arg0^.literalType in C_setTypes
        then newSet:=recycler^.newSetLiteral (length(mustContain))
        else newSet:=recycler^.newListLiteral(length(mustContain));
        for i:=0 to length(mustContain)-1 do newSet^.append(recycler,mustContain[i],true);
        resultSets^.append(recycler,newSet,false);
      end;
    end;

  VAR mightContain:T_multiset;
  PROCEDURE buildFreqMap(CONST list:P_listLiteral);
    VAR freqMap:T_freqMap;
        freqList:T_freqMap.KEY_VALUE_LIST;
        freqEntry:T_freqMap.P_CACHE_ENTRY;
        i:longint;
    begin
      freqMap.create;
      for i:=0 to list^.size-1 do begin
        freqEntry:=freqMap.getEntry(list^.value[i]);
        if freqEntry=nil
        then freqMap.put(list^.value[i],1)
        else inc(freqEntry^.value);
      end;
      freqList:=freqMap.keyValueList;
      setLength(mightContain,length(freqList));
      for i:=0 to length(mightContain)-1 do begin
        mightContain[i].value:=freqList[i].key;
        mightContain[i].multiplicity:=freqList[i].value;
      end;
      setLength(freqList,0);
      freqMap.destroy;
    end;

  PROCEDURE buildFreqMap(CONST s:P_setLiteral);
    VAR iter:T_arrayOfLiteral;
        i:longint;
    begin
      iter:=s^.tempIterableList;
      setLength(mightContain,length(iter));
      for i:=0 to length(mightContain)-1 do begin
        mightContain[i].value:=iter[i];
        mightContain[i].multiplicity:=1;
      end;
    end;

  VAR mustContain :T_arrayOfLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=2) and ((params^.size=1) or (arg1^.literalType in [lt_smallint,lt_bigint])) then begin
      if params^.size=2 then acceptOnlySetsOfSize:=int1^.intValue;
      if (arg0^.literalType in C_listTypes) or (arg0^.literalType in C_setTypes) then begin
        setLength(mustContain,0);
        initialize(mightContain);
        if arg0^.literalType in C_listTypes
        then buildFreqMap(list0)
        else buildFreqMap(set0);

        resultSets:=recycler^.newListLiteral();
        recurseBuildSets(mustContain,mightContain);
        if memoryPanic then begin
          if resultSets<>nil then recycler^.disposeLiteral(resultSets);
          result:=nil;
        end else result:=resultSets;
      end else begin
        result:=recycler^.newListLiteral
                  ^.append(recycler, recycler^.newListLiteral                                              ,false)
                  ^.append(recycler, recycler^.newListLiteral^.append(recycler, arg0,true),false);
      end;
    end;
  end;

FUNCTION permutations_impl intFuncSignature;
  VAR mustContain,mightContain,iter:T_arrayOfLiteral;
      i:longint;
      memoryPanic:boolean=false;
  PROCEDURE recurseBuildPermutations(CONST mustContain,mightContain:T_arrayOfLiteral);
    VAR newMust,newMight:T_arrayOfLiteral;
        newList:P_listLiteral;
        i,j,k:longint;
    begin
      if memoryPanic or not(memoryCleaner.isMemoryInComfortZone) then begin
        memoryPanic:=true;
        exit;
      end;
      if length(mightContain)>0 then begin
        setLength(newMust ,length(mustContain )+1);
        for k:=0 to length(mustContain)-1 do newMust[k]:=mustContain[k];
        setLength(newMight,length(mightContain)-1);
        for i:=0 to length(mightContain)-1 do if not(memoryPanic) then begin
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
        newList:=recycler^.newListLiteral(length(mustContain));
        for i:=0 to length(mustContain)-1 do newList^.append(recycler,mustContain[i],true);
        setResult^.append(recycler, newList,false);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and ((arg0^.literalType in C_listTypes) or (arg0^.literalType in C_setTypes)) then begin
      setLength(mustContain,0);
      iter:=collection0^.tempIterableList;
      setLength(mightContain,length(iter));
      for i:=0 to length(mightContain)-1 do mightContain[i]:=iter[i];
      result:=recycler^.newSetLiteral(length(iter));
      recurseBuildPermutations(mustContain,mightContain);
      if memoryPanic then begin
        recycler^.disposeLiteral(result);
        result:=nil;
      end;
    end;
  end;

FUNCTION factorize_impl intFuncSignature;
  VAR factors:T_factorizationResult;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint]) then begin
      if arg0^.literalType=lt_smallint
      then begin
        factors:=factorizeSmall(P_smallIntLiteral(arg0)^.value);
      end else begin
        factors:=bigint.factorize(P_bigIntLiteral(arg0)^.value,@context^.continueEvaluation);
      end;
      result:=recycler^.newListLiteral(length(factors.smallFactors)+length(factors.bigFactors));
      for i:=0 to length(factors.smallFactors)-1 do listResult^.appendInt(recycler,factors.smallFactors[i]);
      for i:=0 to length(factors.bigFactors)-1 do listResult^.append(recycler,recycler^.newIntLiteral(factors.bigFactors[i]),false);
      setLength(factors.smallFactors,0);
      setLength(factors.bigFactors,0);
      listResult^.sort;
    end else result:=genericVectorization('factorize',params,tokenLocation,context,recycler);
  end;

FUNCTION isPrime_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_bigint  : result:=newBoolLiteral(    not(P_bigIntLiteral  (arg0)^.value.isNegative) and
                                          isPrime(P_bigIntLiteral  (arg0)^.value));
      lt_smallint: result:=newBoolLiteral(isPrime(P_smallIntLiteral(arg0)^.value));
      else         result:=genericVectorization('isPrime',params,tokenLocation,context,recycler);
    end;
  end;

FUNCTION primes_impl intFuncSignature;
  FUNCTION sievePrimes(CONST pMax:int64):P_listLiteral;
    VAR isPrime:array of boolean;
        i,p:longint;
    begin
      if (pMax<2) or (pMax>2147117569) then exit(recycler^.newListLiteral);
      setLength(isPrime,pMax+1);
      isPrime[0]:=false;
      isPrime[1]:=false;
      for i:=2 to length(isPrime)-1 do
        isPrime[i]:=true;
      p:=2;
      while (p*p<length(isPrime)) do begin
        i:=p*p;
        while i<length(isPrime) do begin
          isPrime[i]:=false;
          inc(i,p);
        end;
        inc(p);
        while (p<length(isPrime)) and not(isPrime[p]) do inc(p);
      end;
      result:=recycler^.newListLiteral;
      for i:=2 to length(isPrime)-1 do if isPrime[i] then result^.appendInt(recycler,i);
      setLength(isPrime,0);
    end;

  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_smallint)
    then result:=sievePrimes(int0^.intValue)
    else result:=nil;
  end;

FUNCTION digits_impl intFuncSignature;
  VAR bigBase:T_bigInt;
      smallBase:longint=10;

  FUNCTION smallDigitsOf(CONST i:T_bigInt):P_listLiteral;
    VAR digits:T_arrayOfLongint;
        k:longint;
    begin
      digits:=i.getDigits(smallBase);
      result:=recycler^.newListLiteral(length(digits));
      for k:=length(digits)-1 downto 0 do result^.appendInt(recycler,digits[k]);
      setLength(digits,0);
    end;

  FUNCTION smallDigitsOf(i:longint):P_listLiteral;
    VAR digits:T_arrayOfLongint;
        digit:longint;
        k:longint=0;
    begin
      if i=0 then exit(P_listLiteral(recycler^.newListLiteral(1)^.appendInt(recycler,0)));
      if i<0 then i:=-i;
      setLength(digits,32);
      while (i>0) do begin
        digit:=i mod smallBase;
        i    :=i div smallBase;
        digits[k]:=digit;
        inc(k);
      end;
      setLength(digits,k);
      result:=recycler^.newListLiteral(k);
      for k:=length(digits)-1 downto 0 do result^.appendInt(recycler,digits[k]);
      setLength(digits,0);
    end;

  FUNCTION digitsOf(CONST i:T_bigInt):P_listLiteral;
    VAR digits:T_arrayOfBigint;
        k:longint;
    begin
      digits:=bigDigits(i,bigBase);
      result:=recycler^.newListLiteral(length(digits));
      for k:=length(digits)-1 downto 0 do result^.append(recycler,recycler^.newIntLiteral(digits[k]),false);
      setLength(digits,0);
    end;

  VAR temp:T_bigInt;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=2) and
      (arg0^.literalType in [lt_smallint,lt_bigint,lt_emptyList,lt_intList]) and
      ((params^.size<2) or (arg1^.literalType in [lt_smallint,lt_bigint]))  then begin
      if params^.size=2 then begin
        if arg1^.literalType=lt_bigint then begin
          bigBase:=P_bigIntLiteral(arg1)^.value;
          if (bigBase.compare(1) in [CR_LESSER,CR_EQUAL]) then begin
            context^.raiseError('Cannot determine digits with base '+arg1^.toString+'; must be >=2',tokenLocation);
            exit(nil);
          end;
          if bigBase.canBeRepresentedAsInt32()
          then smallBase:=bigBase.toInt
          else smallBase:=-1;
        end else begin
          smallBase:=P_smallIntLiteral(arg1)^.value;
          if (smallBase<=1) then begin
            context^.raiseError('Cannot determine digits with base '+arg1^.toString+'; must be >=2',tokenLocation);
            exit(nil);
          end;
        end;
      end;

      if smallBase>0 then case arg0^.literalType of
        lt_smallint: result:=smallDigitsOf(P_smallIntLiteral(arg0)^.value);
        lt_bigint  : result:=smallDigitsOf(P_bigIntLiteral  (arg0)^.value);
        lt_emptyList: result:=arg0^.rereferenced;
        lt_intList: begin
          result:=recycler^.newListLiteral(list0^.size);
          for i:=0 to list0^.size-1 do case list0^.value[i]^.literalType of
            lt_bigint  : listResult^.append(recycler,smallDigitsOf(P_bigIntLiteral  (list0^.value[i])^.value),false);
            lt_smallint: listResult^.append(recycler,smallDigitsOf(P_smallIntLiteral(list0^.value[i])^.value),false);
          end;
        end;
      end else case arg0^.literalType of
        lt_bigint: result:=digitsOf(P_bigIntLiteral(arg0)^.value);
        lt_smallint: begin
          temp.fromInt(P_smallIntLiteral(arg0)^.value);
          result:=digitsOf(temp);
          temp.clear;
        end;
        lt_emptyList: result:=arg0^.rereferenced;
        lt_intList: begin
          result:=recycler^.newListLiteral(list0^.size);
          for i:=0 to list0^.size-1 do case list0^.value[i]^.literalType of
            lt_bigint  : listResult^.append(recycler,digitsOf(P_bigIntLiteral  (list0^.value[i])^.value),false);
            lt_smallint: begin
              temp.fromInt(P_smallIntLiteral(list0^.value[i])^.value);
              listResult^.append(recycler,digitsOf(temp),false);
              temp.clear;
            end;
          end;
        end;
      end;
    end else result:=genericVectorization('digits',params,tokenLocation,context,recycler);
  end;

FUNCTION composeDigits_imp intFuncSignature;
  VAR garbage:T_arrayOfBigint;
      zeroUsed:boolean=false;
      zero:T_bigInt;
  PROCEDURE markAsGarbage(VAR x:T_bigInt);
    begin
      setLength(garbage,length(garbage)+1);
      garbage[length(garbage)-1]:=x;
    end;

  PROCEDURE clearGarbage;
    VAR i:longint;
    begin
      for i:=0 to length(garbage)-1 do garbage[i].clear;
    end;

  FUNCTION newTempZero:T_bigInt;
    begin
      if zeroUsed then exit(zero);
      zero.createZero;
      markAsGarbage(zero);
      result:=zero;
    end;

  VAR base:T_bigInt;
      Shift:int64=0;
      digits:T_arrayOfBigint;
      intPart :T_bigInt;
      fracPart,invFloatBase:T_myFloat;
      k  :longint;
  begin
    result:=nil;
    setLength(garbage,0);
    if (params<>nil) and (params^.size>=1) and (params^.size<=3) and
       (arg0^.literalType in [lt_emptyList,lt_intList]) then begin
      if params^.size>=2 then begin
        case arg1^.literalType of
          lt_bigint: base:=P_bigIntLiteral(arg1)^.value;
          lt_smallint: begin
            base.fromInt(P_smallIntLiteral(arg1)^.value);
            markAsGarbage(base);
          end;
          else exit(nil);
        end;
      end else begin
        base.fromInt(10);
        markAsGarbage(base);
      end;
      if params^.size=3 then begin
        if (arg2^.literalType in [lt_smallint,lt_bigint]) and (int2^.isBetween(-maxLongint,maxLongint))
        then Shift:=int2^.intValue
        else begin
          context^.raiseError('Shift argument is out of bounds',tokenLocation);
          clearGarbage;
          exit(nil);
        end;
      end;
      if arg0^.literalType=lt_emptyList then begin
        clearGarbage;
        exit(recycler^.newIntLiteral(0));
      end;

      if list0^.size>Shift then begin
        setLength(digits,list0^.size-Shift);
        for k:=0 to min(length(digits),list0^.size)-1 do begin
          if list0^.value[k]^.literalType=lt_smallint
          then begin
            digits[k].fromInt(P_smallIntLiteral(list0^.value[k])^.value);
            markAsGarbage(digits[k]);
          end else digits[k]:=P_bigIntLiteral(list0^.value[k])^.value;
        end;
        for k:=list0^.size to length(digits)-1 do digits[k]:=newTempZero;
        intPart:=newFromBigDigits(digits,base);
      end else intPart.createZero;
      if Shift>0 then begin
        fracPart:=0;
        invFloatBase:=1/base.toFloat;
        for k:=list0^.size-1 downto list0^.size-Shift do begin
          if k>=0 then fracPart+=P_abstractIntLiteral(list0^.value[k])^.floatValue;
          fracPart*=invFloatBase;
        end;
        result:=recycler^.newRealLiteral(intPart.toFloat+fracPart);
        intPart.clear;
      end else result:=recycler^.newIntLiteral(intPart);
      clearGarbage;
    end;
  end;

FUNCTION arctan2_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType in [lt_smallint,lt_bigint,lt_real]) and
       (arg1^.literalType in [lt_smallint,lt_bigint,lt_real]) then begin
      result:=recycler^.newRealLiteral(arctan2(P_numericLiteral(arg0)^.floatValue,P_numericLiteral(arg1)^.floatValue));
    end else result:=genericVectorization('arctan2',params,tokenLocation,context,recycler);
  end;

FUNCTION gcd_impl intFuncSignature;
  FUNCTION smallGcd(x,y:int64):int64; inline;
    begin
      result:=x;
      while (y<>0) do begin
        x:=result mod y; result:=y; y:=x;
      end;
    end;

  VAR bigR,bigTemp:T_bigInt;
      r:int64=0;
      workingSmall:boolean;
      k:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) then begin
      for k:=0 to params^.size-1 do if not(params^.value[k]^.literalType in [lt_smallint,lt_bigint]) then exit(genericVectorization('gcd',params,tokenLocation,context,recycler));
      if params^.size=1 then exit(arg0^.rereferenced);

      if arg0^.literalType=lt_smallint then begin
        r:=P_smallIntLiteral(arg0)^.value;
        workingSmall:=true;
      end else begin
        bigR.create(P_bigIntLiteral(arg0)^.value);
        workingSmall:=false;
      end;

      for k:=1 to params^.size-1 do case params^.value[k]^.literalType of
        lt_bigint: if workingSmall
        then r:=P_bigIntLiteral(params^.value[k])^.value.greatestCommonDivider(r)
        else begin
          bigTemp:=bigR.greatestCommonDivider(P_bigIntLiteral(params^.value[k])^.value);
          bigR.clear;
          if bigTemp.canBeRepresentedAsInt64 then begin
            r:=bigTemp.toInt;
            workingSmall:=true;
            bigTemp.clear;
          end else bigR:=bigTemp;
        end;
        lt_smallint: if workingSmall
        then r:=smallGcd(r,P_smallIntLiteral(params^.value[k])^.value)
        else begin
          r:=bigR.greatestCommonDivider(P_smallIntLiteral(params^.value[k])^.value);
          bigR.clear;
          workingSmall:=true;
        end;
      end;
      if workingSmall then result:=recycler^.newIntLiteral(r)
                      else result:=recycler^.newIntLiteral(bigR);
    end;
  end;

FUNCTION hammingWeight_impl intFuncSignature;
  VAR i:longint;
      r:longint=0;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_bigint: result:=recycler^.newIntLiteral(P_bigIntLiteral(arg0)^.value.hammingWeight);
      lt_smallint: begin
        i:=P_smallIntLiteral(arg0)^.value;
        while i>0 do begin
          if odd(i) then inc(r);
          i:=i shr 1;
        end;
        result:=recycler^.newIntLiteral(r);
      end;
      else result:=genericVectorization('hammingWeight',params,tokenLocation,context,recycler);
    end;
  end;

PROCEDURE ensure(CONST L:P_literal; OUT v:T_bigInt; OUT created:boolean);
  begin
    if L^.literalType=lt_bigint then begin
      v:=P_bigIntLiteral(L)^.value;
      created:=false;
    end else begin
      v.fromInt(P_smallIntLiteral(L)^.value);
      created:=true;
    end;
  end;

FUNCTION isPositiveInt(CONST L:P_literal; CONST allowZero:boolean):boolean;
  begin
    result:=(L^.literalType=lt_smallint)
        and ((P_smallIntLiteral(L)^.value>0) or allowZero and (P_smallIntLiteral(L)^.value=0)) or
            (L^.literalType=lt_bigint)
        and (not(P_bigIntLiteral(L)^.value.isNegative) and allowZero or not(P_bigIntLiteral(L)^.value.isZero));
  end;

FUNCTION powMod_impl intFuncSignature;
  FUNCTION smallPowMod(CONST base:int64; power:int64; CONST modul:int64):int64; inline;
    VAR f:int64;
    begin
      result:=1;
      f:=base mod modul;
      while power>0 do begin
        if odd(power) then begin
          result:=(result * f) mod modul;
        end;
        f:=(f*f) mod modul;
        power:=power shr 1;
      end;
    end;

  VAR bx,by,bz:T_bigInt;
      ux:boolean=false;
      uy:boolean=false;
      uz:boolean=false;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       isPositiveInt(arg0,true) and
       isPositiveInt(arg1,true) and
       isPositiveInt(arg2,false) then begin
      if (arg0^.literalType=lt_smallint) and
         (arg1^.literalType=lt_smallint) and
         (arg2^.literalType=lt_smallint)
      then exit(recycler^.newIntLiteral(smallPowMod(P_smallIntLiteral(arg0)^.value,
                                          P_smallIntLiteral(arg1)^.value,
                                          P_smallIntLiteral(arg2)^.value)));
      ensure(arg0,bx,ux);
      ensure(arg1,by,uy);
      ensure(arg2,bz,uz);
      result:=recycler^.newIntLiteral(bx.powMod(by,bz));
      if ux then bx.clear;
      if uy then by.clear;
      if uz then bz.clear;
    end else result:=genericVectorization('powMod',params,tokenLocation,context,recycler);
  end;

FUNCTION modularInverse_impl intFuncSignature;
  VAR intResult:T_bigInt;
      validResult:boolean;
      bx,by:T_bigInt;
      ux,uy:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       isPositiveInt(arg0,false) and
       isPositiveInt(arg1,false) then begin
      ensure(arg0,bx,ux);
      ensure(arg1,by,uy);
      intResult:=bx.modularInverse(by,validResult);
      if validResult
      then result:=recycler^.newIntLiteral(intResult)
      else begin
        intResult.clear;
        result:=recycler^.newRealLiteral(Nan);
      end;
      if ux then bx.clear;
      if uy then by.clear;
    end else result:=genericVectorization('modularInverse',params,tokenLocation,context,recycler);
  end;

FUNCTION bitShift_impl intFuncSignature;
  VAR res:T_bigInt;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_smallint,lt_bigint]) and (arg1^.literalType=lt_smallint) then begin
      if arg0^.literalType=lt_bigint
      then res.create (P_bigIntLiteral  (arg0)^.value)
      else res.fromInt(P_smallIntLiteral(arg0)^.value);
      res.shiftRight(int1^.intValue);
      result:=recycler^.newIntLiteral(res);
    end else result:=genericVectorization('shiftRight',params,tokenLocation,context,recycler);
  end;

FUNCTION divMod_impl intFuncSignature;
  VAR q ,r :T_bigInt;
      temp :T_bigInt;
  begin
    if (params<>nil) and (params^.size=2) then
    case arg1^.literalType of
      lt_smallint: begin
        if P_smallIntLiteral(arg1)^.value=0
        then exit(recycler^.newListLiteral(2)^.appendReal(recycler,Nan)^.appendReal(recycler,Nan));
        case arg0^.literalType of
          lt_smallint:
            exit(recycler^.newListLiteral(2)^
               .appendInt(recycler,int0^.intValue div int1^.intValue)^
               .appendInt(recycler,int0^.intValue mod int1^.intValue));
          lt_bigint: begin
            temp.fromInt(P_smallIntLiteral(arg1)^.value);
            P_bigIntLiteral(arg0)^.value.divMod(temp,q,r);
            temp.clear;
            exit(recycler^.newListLiteral(2)^
              .append(recycler,recycler^.newIntLiteral(q),false)^
              .append(recycler,recycler^.newIntLiteral(r),false));
          end;
        end;
      end;
      lt_bigint: begin
        if P_bigIntLiteral(arg1)^.value.isZero
        then exit(recycler^.newListLiteral(2)^.appendReal(recycler,Nan)^.appendReal(recycler,Nan));
        case arg0^.literalType of
          lt_smallint: begin
            temp.fromInt(P_smallIntLiteral(arg0)^.value);
            temp.divMod(P_bigIntLiteral(arg1)^.value,q,r);
            exit(recycler^.newListLiteral(2)^
              .append(recycler,recycler^.newIntLiteral(q),false)^
              .append(recycler,recycler^.newIntLiteral(r),false));
          end;
          lt_bigint: begin
            P_bigIntLiteral(arg0)^.value.divMod(P_bigIntLiteral(arg1)^.value,q,r);
            exit(recycler^.newListLiteral(2)^
              .append(recycler,recycler^.newIntLiteral(q),false)^
              .append(recycler,recycler^.newIntLiteral(r),false));
          end;
        end;
      end;
    end;
    result:=genericVectorization('divMod',params,tokenLocation,context,recycler);
  end;

FUNCTION euklideanNorm_impl intFuncSignature;
  VAR k:longint;
      total:double=0;
  begin
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_numList,lt_intList,lt_realList:begin
          for k:=0 to list0^.size-1 do total+=system.sqr(P_numericLiteral(list0^.value[k])^.floatValue);
          result:=recycler^.newRealLiteral(sqrt(total));
        end;
        lt_smallint,lt_bigint,lt_real: result:=abs_imp(params,tokenLocation,context,recycler);
        else result:=genericVectorization('euklideanNorm',params,tokenLocation,context,recycler);
      end;
    end else result:=nil;
  end;

FUNCTION integrate_impl intFuncSignature;
  TYPE T_subrange=record
         area: T_arrayOfDouble;
         x0,dx:double;
         y:array[0..4] of T_arrayOfDouble;
       end;
       T_subrangeHeap=specialize T_binaryHeap<T_subrange>;
  VAR subrangeHeap:T_subrangeHeap;
      f:P_expressionLiteral;
      returnType:T_literalType=lt_error;
      pointsRemaining:longint;
      errorTolerance:double=0;
  FUNCTION evalF(CONST x:double):T_arrayOfDouble; inline;
    VAR evResult:T_evaluationResult;
        parameterList:T_listLiteral;
        i:longint;
    begin
      dec(pointsRemaining);
      parameterList.create(1);
      parameterList.appendReal(recycler,x);

      evResult:=f^.evaluate(tokenLocation,context,recycler,@parameterList);
      if (evResult.literal<>nil) and (evResult.literal^.literalType in [lt_smallint,lt_bigint,lt_real,lt_numList,lt_realList,lt_intList]) then begin
        returnType:=evResult.literal^.literalType;
        if evResult.literal^.literalType in [lt_smallint,lt_bigint,lt_real] then begin
          result:=P_numericLiteral(evResult.literal)^.floatValue;
        end else begin
          setLength(result,P_listLiteral(evResult.literal)^.size);
          for i:=0 to length(result)-1 do result[i]:=P_numericLiteral(P_listLiteral(evResult.literal)^.value[i])^.floatValue;
        end;
      end else begin
        result:=nanLit.floatValue;
        if evResult.reasonForStop=rr_patternMismatch then P_context(context)^.raiseCannotApplyError('expression '+f^.toString(50),@parameterList,tokenLocation)
        else if (evResult.literal<>nil) then P_context(context)^.raiseError('Expression does not return a numeric but a '+evResult.literal^.typeString,tokenLocation);
      end;
      parameterList.cleanup(recycler);
      parameterList.destroy;
      if evResult.literal<>nil then recycler^.disposeLiteral(evResult.literal);
    end;

  PROCEDURE evaluatePart(CONST x0,dx:double; f0,f2,f4:T_arrayOfDouble);
    FUNCTION anyInvalid(CONST a:T_arrayOfDouble):boolean; inline;
      VAR d:double;
      begin
        for d in a do if isNan(d) or isInfinite(d) then exit(true);
        result:=false;
      end;
    CONST highOrderWeights:array[0..31,0..4] of double=(( 0.07777777777777777, 0.35555555555555556, 0.13333333333333333, 0.3555555555555556 , 0.07777777777777777),
                                                        ( 0                  , 0.66666666666666666,-0.33333333333333333, 0.6666666666666666 , 0                  ),
                                                        ( 0.16666666666666666, 0                  , 0.6666666666666667 , 0                  , 0.16666666666666666),
                                                        ( 0.66666666666666666,-1.33333333333333333, 1.66666666666666666, 0                  , 0                  ),
                                                        ( 0.05555555555555555, 0.44444444444444444, 0                  , 0.44444444444444444, 0.05555555555555555),
                                                        ( 0.11111111111111111, 0.33333333333333333, 0                  , 0.55555555555555555, 0                  ),
                                                        (-0.16666666666666666, 0.88888888888888888, 0                  , 0                  , 0.27777777777777777),
                                                        (-1                  , 2                  , 0                  , 0                  , 0                  ),
                                                        ( 0.16666666666666666, 0                  , 0.6666666666666667 , 0                  , 0.16666666666666666),
                                                        ( 0.22222222222222222, 0                  , 0.33333333333333333, 0.44444444444444444, 0                  ),
                                                        ( 0.16666666666666666, 0                  , 0.6666666666666667 , 0                  , 0.16666666666666666),
                                                        ( 0                  , 0                  , 1                  , 0                  , 0                  ),
                                                        ( 0.27777777777777777, 0                  , 0                  , 0.8888888888888889 ,-0.16666666666666666),
                                                        ( 0.33333333333333333, 0                  , 0                  , 0.66666666666666666, 0                  ),
                                                        ( 0.5                , 0                  , 0                  , 0                  , 0.5                ),
                                                        ( 1                  , 0                  , 0                  , 0                  , 0                  ),
                                                        ( 0                  , 0.66666666666666666,-0.33333333333333333, 0.66666666666666666, 0                  ),
                                                        ( 0                  , 0.66666666666666666,-0.33333333333333333, 0.66666666666666666, 0                  ),
                                                        ( 0                  , 0.4444444444444445 , 0.33333333333333333, 0                  , 0.22222222222222222),
                                                        ( 0                  , 0                  , 1                  , 0                  , 0                  ),
                                                        ( 0                  , 0.55555555555555555, 0                  , 0.33333333333333333, 0.11111111111111111),
                                                        ( 0                  , 0.5                , 0                  , 0.5                , 0                  ),
                                                        ( 0                  , 0.66666666666666666, 0                  , 0                  , 0.33333333333333333),
                                                        ( 0                  , 1                  , 0                  , 0                  , 0                  ),
                                                        ( 0                  , 0                  , 1.66666666666666666,-1.33333333333333333, 0.66666666666666666),
                                                        ( 0                  , 0                  , 1                  , 0                  , 0                  ),
                                                        ( 0                  , 0                  , 1                  , 0                  , 0                  ),
                                                        ( 0                  , 0                  , 1                  , 0                  , 0                  ),
                                                        ( 0                  , 0                  , 0                  , 2                  ,-1                  ),
                                                        ( 0                  , 0                  , 0                  , 1                  , 0                  ),
                                                        ( 0                  , 0                  , 0                  , 0                  , 1                  ),
                                                        ( 0                  , 0                  , 0                  , 0                  , 0                  ));
    CONST lowOrderWeights:array[0..7,0..2] of double=((0.16666666666666666,0.6666666666666667,0.16666666666666666),
                                                      (0                  ,1                 ,0                  ),
                                                      (0.5                ,0                 ,0.5                ),
                                                      (1                  ,0                 ,0                  ),
                                                      (0                  ,1                 ,0                  ),
                                                      (0                  ,1                 ,0                  ),
                                                      (0                  ,0                 ,1                  ),
                                                      (0                  ,0                 ,0                  ));
    VAR subrange:T_subrange;
        s0:byte=0;
        s1:byte=0;
        lowerOrderEstimate:T_arrayOfDouble;
        resultSize:longint=maxLongint;
        i,k:longint;
        w:double;
    begin
      initialize(subrange);
      subrange.x0:=x0;
      subrange.dx:=dx;
      //complete point list:----------------------------------------------------
      subrange.y[0]:=f0;
      subrange.y[1]:=evalF(x0+0.25*dx);
      subrange.y[2]:=f2;
      subrange.y[3]:=evalF(x0+0.75*dx);
      subrange.y[4]:=f4;
      //----------------------------------------------------:complete point list
      //determine integration weights:------------------------------------------
      if anyInvalid(subrange.y[0]) then begin inc(s0  ); inc(s1   ); end;
      if anyInvalid(subrange.y[1]) then begin            inc(s1, 2); end;
      if anyInvalid(subrange.y[2]) then begin inc(s0,2); inc(s1, 4); end;
      if anyInvalid(subrange.y[3]) then begin            inc(s1, 8); end;
      if anyInvalid(subrange.y[4]) then begin inc(s0,4); inc(s1,16); end;
      //------------------------------------------:determine integration weights
      //calculate approximations:-----------------------------------------------
      for k:=0 to 4 do begin
        i:=length(subrange.y[k]);
        if i<resultSize then resultSize:=i;
      end;
      setLength(lowerOrderEstimate,resultSize); for k:=0 to resultSize-1 do lowerOrderEstimate[k]:=0;
      setLength(subrange.area     ,resultSize); for k:=0 to resultSize-1 do subrange.area     [k]:=0;
      for i:=0 to 4 do begin
        w:=dx*highOrderWeights[s0,i];
        if w<>0 then for k:=0 to resultSize-1 do subrange.area[k]+=w*subrange.y[i,k];
        if not(odd(i)) then begin
          w:=dx*lowOrderWeights[s1,i shr 1];
          if w<>0 then for k:=0 to resultSize-1 do lowerOrderEstimate[k]+=w*subrange.y[i,k];
        end;
      end;
      //-----------------------------------------------:calculate approximations
      //calculate error estimate:-----------------------------------------------
      w:=0;
      for k:=0 to resultSize-1 do w+=system.sqr(lowerOrderEstimate[k]-subrange.area[k]);
      //Safeguard: make sure that only finite errors are posted
      if isNan(w) or isInfinite(w) or (w<0) then w:=0;
      //-----------------------------------------------:calculate error estimate
      subrangeHeap.insert(subrange,w);
    end;

  FUNCTION anyNonzero(CONST a:T_arrayOfDouble):boolean;
    VAR d:double;
    begin
      for d in a do if d<>0 then exit(true);
      result:=false;
    end;

  FUNCTION performIntegration(x0,x1:double):T_arrayOfDouble;
    VAR subrange:T_subrange;
        subranges:T_subrangeHeap.T_payloadArray;
        dx:double;
        i:longint;
        aggregator:array of KahanBabushkaKleinSummation;
    begin
      subrangeHeap.createWithNumericPriority(nil); //priority will be explicitly passed to heap
      if isInfinite(x1) and isInfinite(x0) then begin
        x0:=-1;
        x1:= 1;
        dx:=1;
        while not(isInfinite(x1)) and anyNonzero(evalF(x1)) do begin dx*=2; x1+=dx; end;
        x1-=dx;
        dx:=1;
        while not(isInfinite(x0)) and anyNonzero(evalF(x0)) do begin dx*=2; x0-=dx; end;
        x0+=dx;
      end else if isInfinite(x1) then begin
        x1:=x0+1; dx:=1;
        while not(isInfinite(x1)) and anyNonzero(evalF(x1)) do begin dx*=2; x1+=dx; end;
        x1-=dx;
      end else if isInfinite(x0) then begin
        x0:=x1-1; dx:=1;
        while not(isInfinite(x0)) and anyNonzero(evalF(x0)) do begin dx*=2; x0-=dx; end;
        x0+=dx;
      end;
      if isInfinite(x0) then x0:=-1E256;
      if isInfinite(x1) then x1:= 1E256;
      dx:=(x1-x0);

      i:=pointsRemaining-floor(sqrt(pointsRemaining));
      evaluatePart(x0,dx,
        evalF(x0),
        evalF(x0+0.5*dx),
        evalF(x1));

      while (pointsRemaining>0) and ((pointsRemaining>i) or (subrangeHeap.maxPrio>errorTolerance)) do begin
        subrange:=subrangeHeap.extractHighestPrio;
        evaluatePart(subrange.x0                ,subrange.dx*0.5,subrange.y[0],subrange.y[1],subrange.y[2]);
        evaluatePart(subrange.x0+subrange.dx*0.5,subrange.dx*0.5,subrange.y[2],subrange.y[3],subrange.y[4]);
      end;

      subranges:=subrangeHeap.getAll;
      setLength(aggregator,length(subranges[0].area));
      for i:=0 to length(aggregator)-1 do aggregator[i].init;

      for subrange in subranges do
        for i:=0 to min(length(aggregator),length(subrange.area))-1 do
          aggregator[i].addTerm(subrange.area[i]);
      setLength(result,length(aggregator));
      for i:=0 to length(result)-1 do result[i]:=aggregator[i].getSum;
      setLength(aggregator,0);
      setLength(subranges,0);
      subrangeHeap.destroy;
    end;

  VAR floatResult:T_arrayOfDouble;
      k:longint;
  begin
    if (params<>nil) and (params^.size>=4) and (params^.size<=5) and
      (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(1)) and
      (arg1^.literalType in [lt_smallint,lt_bigint,lt_real]) and
      (arg2^.literalType in [lt_smallint,lt_bigint,lt_real]) and
      (arg3^.literalType = lt_smallint) and (P_smallIntLiteral(arg3)^.value>0) and
      ((params^.size<=4) or (arg4^.literalType=lt_real) and (P_realLiteral(arg4)^.value>=0))
    then begin
      if params^.size>4 then errorTolerance:=system.sqr(P_realLiteral(arg4)^.value); //Square the tolerance, because the errors are squared as well
      f :=       P_expressionLiteral(arg0);
      pointsRemaining:=int3^.intValue;
      floatResult:=performIntegration(P_numericLiteral(arg1)^.floatValue,
                                      P_numericLiteral(arg2)^.floatValue);
      if returnType in C_listTypes then begin
        result:=recycler^.newListLiteral(length(floatResult));
        for k:=0 to length(floatResult)-1 do listResult^.appendReal(recycler,floatResult[k]);
      end else begin
        result:=recycler^.newRealLiteral(floatResult[0]);
      end;
    end else result:=nil;
  end;

FUNCTION bitXor intFuncSignature;
  CONST mask:array[1..32] of longint=($1,$3,$7,$F,$1F,$3F,$7F,$ff,$1ff,$3ff,$7ff,$FFF,$1FFF,$3FFF,$7FFF,$FFFF,$1FFFF,$3FFFF,$7FFFF,$FFFFF,$1FFFFF,$3FFFFF,$7FFFFF,$FFFFFF,$1FFFFFF,$3FFFFFF,$7FFFFFF,$FFFFFFF,$1FFFFFFF,$3FFFFFFF,$7FFFFFFF,-1);
  begin
    if (params<>nil) and (params^.size=3) then begin
      if (arg0^.literalType=lt_smallint) and (arg1^.literalType=lt_smallint) and (arg2^.literalType=lt_smallint) and (int2^.intValue>0) and (int2^.intValue<32)
      then result:=recycler^.newIntLiteral(mask[P_smallIntLiteral(arg2)^.value] and (P_smallIntLiteral(arg0)^.value xor P_smallIntLiteral(arg1)^.value))
      else result:=genericVectorization('bitXor',params,tokenLocation,context,recycler);
    end else result:=nil;
  end;

FUNCTION canConvertToArrayOfComplex(CONST L:P_literal; OUT arr:T_arrayOfComplex):boolean;
  VAR i:longint;
  begin
    case L^.literalType of
      lt_intList,
      lt_realList,
      lt_numList: begin
        setLength(arr,P_listLiteral(L)^.size);
        for i:=0 to length(arr)-1 do arr[i]:=P_numericLiteral(P_listLiteral(L)^.value[i])^.floatValue;
        result:=true;
      end;
      lt_list: begin
        setLength(arr,P_listLiteral(L)^.size);
        for i:=0 to length(arr)-1 do case P_listLiteral(L)^.value[i]^.literalType of
          lt_smallint,lt_bigint,lt_real: arr[i]:=P_numericLiteral(P_listLiteral(L)^.value[i])^.floatValue;
          lt_intList,
          lt_realList,
          lt_numList: if P_listLiteral(P_listLiteral(L)^.value[i])^.size=2 then begin
            arr[i].re:=P_numericLiteral(P_listLiteral(P_listLiteral(L)^.value[i])^.value[0])^.floatValue;
            arr[i].im:=P_numericLiteral(P_listLiteral(P_listLiteral(L)^.value[i])^.value[1])^.floatValue;
          end else exit(false);
          else exit(false);
        end;
        result:=true;
      end;
      else result:=false;
    end;
  end;

FUNCTION arrayOfComplexToLiteral(CONST arr:T_arrayOfComplex; CONST recycler:P_recycler):P_listLiteral;
  VAR i:longint;
  begin
    result:=recycler^.newListLiteral(length(arr));
    for i:=0 to length(arr)-1 do result^.append(recycler,
      recycler^.newListLiteral^.appendReal(recycler,arr[i].re)^.appendReal(recycler,arr[i].im),false);
  end;

FUNCTION DFT_impl intFuncSignature;
  VAR x,y:T_arrayOfComplex;
  begin
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_emptyList then exit(recycler^.newListLiteral(0));
      if canConvertToArrayOfComplex(arg0,x) then y:=DiscreteFourierTransform(x,false) else exit(nil);
      result:=arrayOfComplexToLiteral(y,recycler);
      setLength(x,0);
      setLength(y,0);
    end else result:=nil;
  end;

FUNCTION iDFT_impl intFuncSignature;
  VAR x,y:T_arrayOfComplex;
  begin
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_emptyList then exit(recycler^.newListLiteral(0));
      if canConvertToArrayOfComplex(arg0,x) then y:=DiscreteFourierTransform(x,true) else exit(nil);
      result:=arrayOfComplexToLiteral(y,recycler);
      setLength(x,0);
      setLength(y,0);
    end else result:=nil;
  end;

FUNCTION FFT_impl intFuncSignature;
  VAR x,y:T_arrayOfComplex;
  begin
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_emptyList then exit(recycler^.newListLiteral(0));
      if canConvertToArrayOfComplex(arg0,x) then y:=FastFourierTransform(x,false) else exit(nil);
      result:=arrayOfComplexToLiteral(y,recycler);
      setLength(x,0);
      setLength(y,0);
    end else result:=nil;
  end;

FUNCTION iFFT_impl intFuncSignature;
  VAR x,y:T_arrayOfComplex;
  begin
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_emptyList then exit(recycler^.newListLiteral(0));
      if canConvertToArrayOfComplex(arg0,x) then y:=FastFourierTransform(x,true) else exit(nil);
      result:=arrayOfComplexToLiteral(y,recycler);
      setLength(x,0);
      setLength(y,0);
    end else result:=nil;
  end;

FUNCTION kMeans_impl intFuncSignature;
  TYPE T_vector=array of double;
  VAR raw_data:array of record
                 x:T_vector;
                 c:longint;
                 invalid:boolean;
               end;
      centers :array of record
                 x:T_vector;
                 count:longint;
               end;
      k:longint=0;
      dim:longint=0;
  FUNCTION ZERO_VECTOR:T_vector;
    VAR i:longint;
    begin
      setLength(result,dim);
      for i:=0 to dim-1 do result[i]:=0;
    end;

  FUNCTION toVector(CONST L:P_listLiteral):T_vector;
    VAR i:longint;
    begin
      setLength(result,dim);
      for i:=0 to dim-1 do result[i]:=P_numericLiteral(L^.value[i])^.floatValue;
    end;

  FUNCTION toNumList(CONST v:T_vector):P_listLiteral;
    VAR i:longint;
    begin
      result:=recycler^.newListLiteral(length(v));
      for i:=0 to length(v)-1 do result^.appendReal(recycler,v[i]);
    end;

  FUNCTION vector_inc(VAR v:T_vector; CONST toAdd:T_vector):boolean; inline;
    VAR i:longint;
    begin
      result:=true;
      for i:=0 to dim-1 do result:=result and not(isInfinite(toAdd[i])) and not(isNan(toAdd[i]));
      if result then for i:=0 to dim-1 do v[i]+=toAdd[i];
    end;

  PROCEDURE vector_mult(VAR v:T_vector; CONST factor:double); inline;
    VAR i:longint;
    begin
      for i:=0 to dim-1 do v[i]*=factor;
    end;

  FUNCTION classify(CONST v:T_vector):longint; {$ifndef debugMode} inline; {$endif}
    VAR i,classIndex:longint;
        dist   :double;
        closest:double=infinity;
    begin
      for classIndex:=0 to k-1 do begin
        dist:=0; for i:=0 to dim-1 do dist+=system.sqr(centers[classIndex].x[i]-v[i]);
        if (classIndex=0) or (dist<closest) then begin
          result:=classIndex;
          closest:=dist;
        end;
      end;
      assert((result>=0) and (result<k));
    end;

  FUNCTION anyInvalid(CONST v:T_vector):boolean;  {$ifndef debugMode} inline; {$endif}
    VAR i:longint;
    begin
      result:=false;
      for i:=0 to dim-1 do result:=result or isNan(v[i]) or isInfinite(v[i]);
    end;

  VAR max_steps:longint=100;
      min_class_size:longint=1;

  FUNCTION canFillRawData:boolean;
    VAR i:longint;
        entry:P_literal;
    begin
      setLength(raw_data,list0^.size);
      result:=true;
      for i:=0 to list0^.size-1 do if result then begin
        entry:=list0^.value[i];
        if entry^.literalType in [lt_numList,lt_intList,lt_realList] then begin
          if dim=0
          then dim:=P_listLiteral(entry)^.size
          else if dim<>P_listLiteral(entry)^.size then result:=false;
          raw_data[i].x:=toVector(P_listLiteral(entry));
          raw_data[i].c:=context^.getGlobals^.prng.intRandom(k);
          raw_data[i].invalid:=anyInvalid(raw_data[i].x);
        end else result:=false;
      end;
      if not(result) then begin
        for i:=0 to length(raw_data)-1 do with raw_data[i] do setLength(x,0);
        setLength(raw_data,0);
        context^.raiseError('kMeans requires a list of numeric lists of the same size as input.',tokenLocation);
      end;
      if length(raw_data)=0 then max_steps:=0;
    end;

  PROCEDURE updateCenters;
    VAR j, i: longint;
    begin
      for j:=0 to k-1 do begin
        centers[j].count:=0;
        centers[j].x:=ZERO_VECTOR;
      end;
      for i:=0 to length(raw_data)-1 do if not raw_data[i].invalid then begin
        j:=raw_data[i].c;
        assert((j>=0) and (j<k),'Invalid class index: '+intToStr(j));
        if vector_inc(centers[j].x,raw_data[i].x)
        then      inc(centers[j].count);
      end;
    end;

  FUNCTION canInitCenters:boolean;
    VAR entry: P_literal;
        i,j:longint;
    begin
      setLength(centers,k);
      result:=true;
      if arg1^.literalType=lt_smallint then updateCenters
      else begin
        for i:=0 to list1^.size-1 do if result then begin
          entry:=list1^.value[i];
          if entry^.literalType in [lt_numList,lt_intList,lt_realList] then begin
            if dim=0
            then dim:=P_listLiteral(entry)^.size
            else if dim<>P_listLiteral(entry)^.size then result:=false;
            centers[i].x:=toVector(P_listLiteral(entry));
            centers[i].count:=1;
          end else result:=false;
        end;
        if result then for i:=0 to length(raw_data)-1 do begin
          j:=classify(raw_data[i].x);
          raw_data[i].c:=j;
        end;
      end;
      if not(result) then begin
        for i:=0 to length(raw_data)-1 do with raw_data[i] do setLength(x,0);
        setLength(raw_data,0);
        for i:=0 to length(raw_data)-1 do with centers[i] do setLength(x,0);
        setLength(centers,0);
        context^.raiseError('kMeans requires a list of numeric lists of the same size as input. The inital list of centers must contain numeric lists of the same size.',tokenLocation);
      end;
      if dim=0 then max_steps:=0;
    end;

  FUNCTION classifyAll:P_mapLiteral;
    PROCEDURE redistribute(CONST emptyClassIndex:longint);
      VAR j:longint;
          largestClassIndex:longint=0;
          splitAxis:longint;
          discriminant:double;
      begin
        for j:=0 to k-1 do if centers[j].count>centers[largestClassIndex].count then largestClassIndex:=j;
        splitAxis:=context^.getGlobals^.prng.intRandom(dim);
        discriminant:=centers[largestClassIndex].x[splitAxis]/centers[largestClassIndex].count;

        centers[largestClassIndex].count:=0; centers[largestClassIndex].x:=ZERO_VECTOR;
        centers[emptyClassIndex  ].count:=0; centers[emptyClassIndex  ].x:=ZERO_VECTOR;

        for j:=0 to length(raw_data)-1 do with raw_data[j] do if c=largestClassIndex then begin
          if x[splitAxis]<discriminant
          then c:=  emptyClassIndex
          else c:=largestClassIndex;
          if vector_inc(centers[c].x,x)
          then      inc(centers[c].count);
        end;
      end;

    VAR i,j:longint;
        anyClassChanged:boolean;
        tempList:P_listLiteral;
        loopCount:longint=0;
    begin
      if max_steps>0 then repeat
        inc(loopCount);
        //1. Normalize centers
        anyClassChanged:=false;
        for i:=0 to k-1 do begin
          if (centers[i].count<min_class_size) or anyInvalid(centers[i].x) then begin
            redistribute(i);
            anyClassChanged:=true;
          end;
          vector_mult(centers[i].x,1/centers[i].count);
        end;
        //2. Reclassify...
        for i:=0 to length(raw_data)-1 do begin
          j:=classify(raw_data[i].x);
          if j<>raw_data[i].c then begin
            raw_data[i].c:=j;
            anyClassChanged:=true;
          end;
        end;
        //3. Update centers
        updateCenters;

      until not(anyClassChanged) or not(context^.continueEvaluation) or (loopCount>=max_steps);
      result:=recycler^.newMapLiteral(2);
      tempList:=recycler^.newListLiteral(length(raw_data));
      for i:=0 to length(raw_data)-1 do begin
        tempList^.appendInt(recycler,raw_data[i].c);
        setLength(raw_data[i].x,0);
      end;
      setLength(raw_data,0);
      result^.put(recycler,'class',tempList,false);

      tempList:=recycler^.newListLiteral(k);
      for j:=0 to length(centers)-1 do if centers[j].count>0 then begin
        vector_mult(centers[j].x,1/centers[j].count);
        tempList^.append(recycler,toNumList(centers[j].x),false);
        setLength(centers[j].x,0);
      end;
      setLength(centers,0);
      result^.put(recycler,'centers',tempList,false);
    end;

  begin
    if (params<>nil) and (params^.size>=2) and (params^.size<=4) and (arg0^.literalType in C_listTypes) and
       ((arg1^.literalType=lt_smallint) and (int1^.intValue>=2) or (arg1^.literalType in C_listTypes) and (list1^.size>=2)) and
       ((params^.size<3) or (arg2^.literalType=lt_smallint)) and
       ((params^.size<4) or (arg3^.literalType=lt_smallint) and (int3^.intValue>=0))
    then begin
      if arg1^.literalType=lt_smallint
      then k:=int1 ^.intValue
      else k:=list1^.size;
      if params^.size>=3 then max_steps     :=int2^.intValue;
      if params^.size =4 then min_class_size:=int3^.intValue;
      if canFillRawData and canInitCenters
      then result:=classifyAll
      else result:=nil;
    end else result:=nil;
  end;

FUNCTION convolve1D_impl intFuncSignature;
  VAR allowGrowth:boolean=false;
      f,g,fcg:T_arrayOfDouble;
      i,j,k:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.size<=3) and
       (arg0^.literalType in [lt_intList,lt_realList,lt_numList]) and
       (arg1^.literalType in [lt_intList,lt_realList,lt_numList]) and
       ((params^.size=2) or (arg2^.literalType=lt_boolean))  then begin
      if params^.size=3 then allowGrowth:=bool2^.value;
      f:=toVector(list0,tokenLocation,context);
      g:=toVector(list1,tokenLocation,context);

      if allowGrowth then begin
        // fcg[i] = sum_j f[j]*g[i-j]
        //          by f: 0<= j <=    length(f)-1
        //          by g: i>= j >=1+i-length(g)
        // 1+i_max-length(g) = length(f)-1
        //   i_max           = length(g)+length(f)-2
        setLength(fcg,length(f)+length(g)-1);
        for i:=0 to length(fcg)-1 do begin
          fcg[i]:=0;
          for j:=max(0,1+i-length(g)) to min(length(f)-1,i) do fcg[i]+=f[j]*g[i-j];
        end;
      end else begin
        k:=length(g) div 2;
        // fcg[i] = sum_j f[j]*g[i-j+k]
        //          by f: 0  <= j <=length(f)-1
        //          by g: i+k>= j >=1+i+k-length(g)
        setLength(fcg,length(f));
        for i:=0 to length(fcg)-1 do begin
          fcg[i]:=0;
          for j:=max(0,1+k+i-length(g)) to min(length(f)-1,i+k) do fcg[i]+=f[j]*g[k+i-j];
        end;
      end;
      result:=vectorToList(fcg,recycler);
      setLength(fcg,0);
    end;
  end;

FUNCTION sum_impl intFuncSignature;
  VAR aggregators:array of KahanBabushkaKleinSummation;
      resultIsScalar:boolean=true;
      er: T_evaluationResult;

  PROCEDURE addAggregator;
    begin
      setLength(aggregators,length(aggregators)+1);
      aggregators[length(aggregators)-1].init;
    end;

  FUNCTION canAggregate(CONST v:P_literal):boolean;
    VAR k:longint;
    begin
      case v^.literalType of
        lt_smallint, lt_bigint, lt_real:
          begin
            if length(aggregators)=0 then addAggregator;
            for k:=0 to length(aggregators)-1 do aggregators[k].addTerm(P_numericLiteral(v)^.floatValue);
            result:=true;
          end;
        lt_realList, lt_intList, lt_numList:
          begin
            if resultIsScalar then begin
              if length(aggregators)=0 then addAggregator;
              setLength(aggregators,P_listLiteral(v)^.size);
              for k:=1 to length(aggregators)-1 do aggregators[k]:=aggregators[0];
              resultIsScalar:=false;
            end else begin
              if P_listLiteral(v)^.size<>length(aggregators) then begin
                context^.raiseError('Trying to sum up lists of different sizes.',tokenLocation);
                exit(false);
              end;
            end;
            for k:=0 to length(aggregators)-1 do
              aggregators[k].addTerm(P_numericLiteral(P_listLiteral(v)^.value[k])^.floatValue);
          end;
        else begin
          context^.raiseError('Trying to sum up invalid types. Only numeric scalars and numeric lists are allowed.',tokenLocation);
          result:=false;
        end;
      end;
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_expression: if P_expressionLiteral(arg0)^.typ in C_iterableExpressionTypes then begin
          er:=evaluteExpression(P_expressionLiteral(arg0),tokenLocation,context,recycler);
          while not(er.reasonForStop in [rr_ok,rr_okWithReturn]) and context^.continueEvaluation do begin
            if canAggregate(er.literal)
            then begin
              recycler^.disposeLiteral(er.literal);
              er:=evaluteExpression(P_expressionLiteral(arg0),tokenLocation,context,recycler);
            end else begin
              recycler^.disposeLiteral(er.literal);
              exit(nil);
            end;
          end;
        end;
        lt_intList,lt_numList,lt_realList, lt_emptyList:
          for i:=0 to list0^.size-1 do canAggregate(list0^.value[i]);
        lt_list:
          for i:=0 to list0^.size-1 do if not canAggregate(list0^.value[i]) then exit(nil);
        else
          exit(nil);
      end;
      if length(aggregators)=0 then result:=recycler^.newRealLiteral(0)
      else if resultIsScalar then result:=recycler^.newRealLiteral(aggregators[0].getSum)
      else begin
        result:=recycler^.newListLiteral(length(aggregators));
        for i:=0 to length(aggregators)-1 do listResult^.appendReal(recycler,aggregators[i].getSum);
      end;
      setLength(aggregators,0);
    end;
  end;

INITIALIZATION
  //Unary Numeric -> real
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'sqrt'  ,@sqrt_imp  ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'isqrt' ,@isqrt_imp ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'sin'   ,@sin_imp   ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'arcsin',@arcsin_imp,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'cos'   ,@cos_imp   ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'arccos',@arccos_imp,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'tan'   ,@tan_imp   ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'arctan',@arctan_imp,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'exp'   ,@exp_imp   ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'ln'    ,@ln_imp    ,ak_unary);
  //Unary Numeric -> same (i.e. I -> I, R -> R)
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'abs',@abs_imp,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'sqr',@sqr_imp,ak_unary);
  //Unary Numeric -> Integer
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'sign' ,@sign_imp ,ak_unary     );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'ceil' ,@ceil_imp ,ak_variadic_1);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'floor',@floor_imp,ak_variadic_1);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'round',@round_imp,ak_variadic_1);

  builtinFunctionMap.registerRule(MATH_NAMESPACE,'pi'          ,@pi_imp           ,ak_nullary   );
  BUILTIN_MAX:=
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'max'         ,@max_imp          ,ak_variadic_1);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'argMax'      ,@argMax_imp       ,ak_unary     );
  BUILTIN_MIN:=
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'min'         ,@min_imp          ,ak_variadic_1);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'argMin'      ,@argMin_imp       ,ak_unary     );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'isNan'       ,@isNan_impl       ,ak_unary     );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'isInfinite'  ,@isInfinite_impl  ,ak_unary     );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'subSets'     ,@subSets_impl     ,ak_variadic_1);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'permutations',@permutations_impl,ak_unary     );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'factorize'   ,@factorize_impl   ,ak_unary     );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'isPrime'     ,@isPrime_impl     ,ak_unary     );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'primes'      ,@primes_impl      ,ak_unary     );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'digits'      ,@digits_impl      ,ak_variadic_1);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'composeDigits' ,@composeDigits_imp  ,ak_variadic_1);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'arctan2'       ,@arctan2_impl       ,ak_binary    );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'gcd'           ,@gcd_impl           ,ak_variadic_1);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'hammingWeight' ,@hammingWeight_impl ,ak_unary     );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'powMod'        ,@powMod_impl        ,ak_ternary   );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'modularInverse',@modularInverse_impl,ak_binary    );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'shiftRight'    ,@bitShift_impl      ,ak_binary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'divMod'        ,@divMod_impl        ,ak_binary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'euklideanNorm' ,@euklideanNorm_impl ,ak_unary );
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'integrate'     ,@integrate_impl     ,ak_quartary);

  builtinFunctionMap.registerRule(MATH_NAMESPACE,'bitXor',@bitXor,ak_ternary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'DFT' ,@DFT_impl ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'iDFT',@iDFT_impl,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'FFT' ,@FFT_impl ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'iFFT',@iFFT_impl,ak_unary);

  builtinFunctionMap.registerRule(MATH_NAMESPACE,'kMeans'    ,@kMeans_impl,ak_variadic_2);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'convolve1D',@convolve1D_impl,ak_variadic_2);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'sum',@sum_impl,ak_unary);
end.
