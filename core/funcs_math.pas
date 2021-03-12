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
VAR BUILTIN_MIN,
    BUILTIN_MAX:P_intFuncCallback;
IMPLEMENTATION
USES mySys;
{$i func_defines.inc}
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
        if isSquare then result:=recycler^.literalRecycler.newIntLiteral (intRoot)
                    else result:=recycler^.literalRecycler.newRealLiteral(fltRoot);
      end;
      lt_bigint: begin
        bigRoot:=P_bigIntLiteral(arg0)^.value.iSqrt(false,RM_DOWN,isSquare);
        if isSquare then result:=recycler^.literalRecycler.newIntLiteral(bigRoot)
        else begin
          bigRoot.clear;
          result:=recycler^.literalRecycler.newRealLiteral(sqrt(P_bigIntLiteral(arg0)^.floatValue));
        end;
      end;
      lt_real: result:=recycler^.literalRecycler.newRealLiteral(sqrt(P_realLiteral(arg0)^.value))
      else result:=genericVectorization('sqrt',params,tokenLocation,context,recycler);
    end else result:=nil;
  end;

FUNCTION isqrt_imp intFuncSignature;
  VAR intRoot:int64;
      bigRoot:T_bigInt;
      isSquare:boolean;
  begin
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_smallint:begin
        intRoot:=trunc(sqrt(P_smallIntLiteral(arg0)^.value));
        result:=recycler^.literalRecycler.newListLiteral(2)^
          .appendInt (@recycler^.literalRecycler, intRoot)^
          .appendBool(@recycler^.literalRecycler, P_smallIntLiteral(arg0)^.value=intRoot*intRoot);
      end;
      lt_bigint: begin
        bigRoot:=P_bigIntLiteral(arg0)^.value.iSqrt(true,RM_DOWN,isSquare);
        result:=recycler^.literalRecycler.newListLiteral(2)^
          .append    (@recycler^.literalRecycler, recycler^.literalRecycler.newIntLiteral(bigRoot),false)^
          .appendBool(@recycler^.literalRecycler, isSquare);
      end;
      else result:=genericVectorization('isqrt',params,tokenLocation,context,recycler);
    end else result:=nil;
  end;

FUNCTION sin_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.literalRecycler.newRealLiteral(sin(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('sin',params,tokenLocation,context,recycler);
  end;

FUNCTION arcsin_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.literalRecycler.newRealLiteral(arcsin(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('arcsin',params,tokenLocation,context,recycler);
  end;

FUNCTION cos_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.literalRecycler.newRealLiteral(cos(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('cos',params,tokenLocation,context,recycler);
  end;

FUNCTION arccos_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.literalRecycler.newRealLiteral(arccos(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('arccos',params,tokenLocation,context,recycler);
  end;

FUNCTION tan_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.literalRecycler.newRealLiteral(tan(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('tan',params,tokenLocation,context,recycler);
  end;

FUNCTION arctan_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.literalRecycler.newRealLiteral(arctan(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('arctan',params,tokenLocation,context,recycler);
  end;

FUNCTION exp_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint,lt_real])
    then result:=recycler^.literalRecycler.newRealLiteral(exp(P_numericLiteral(arg0)^.floatValue))
    else result:=genericVectorization('exp',params,tokenLocation,context,recycler);
  end;

FUNCTION ln_imp intFuncSignature;
  VAR X:T_bigInt;
      r:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_smallint: result:=recycler^.literalRecycler.newRealLiteral(ln(P_smallIntLiteral(arg0)^.value));
      lt_real    : result:=recycler^.literalRecycler.newRealLiteral(ln(real0^.value));
      lt_bigint  : begin
        if P_bigIntLiteral(arg0)^.value.isNegative
        then result:=nanLit.rereferenced
        else begin
          r:=P_bigIntLiteral(arg0)^.value.relevantBits-512;
          if r<0
          then result:=recycler^.literalRecycler.newRealLiteral(ln(P_bigIntLiteral(arg0)^.value.toFloat))
          else begin
            x.create(P_bigIntLiteral(arg0)^.value);
            x.shiftRight(r);
            result:=recycler^.literalRecycler.newRealLiteral(ln(x.toFloat)+ln(2)*r);
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
                   then result:=recycler^.literalRecycler.newIntLiteral(-P_smallIntLiteral(arg0)^.value)
                   else result:=arg0^.rereferenced;
      lt_bigint : if P_bigIntLiteral(arg0)^.value.isNegative
                  then result:=recycler^.literalRecycler.newIntLiteral(P_bigIntLiteral(arg0)^.value.negated)
                  else result:=arg0^.rereferenced;
      lt_real: if P_realLiteral(arg0)^.value<0
               then result:=recycler^.literalRecycler.newRealLiteral(-P_realLiteral(arg0)^.value)
               else result:=arg0^.rereferenced;
      else result:=genericVectorization('abs',params,tokenLocation,context,recycler);
    end;
  end;

FUNCTION sqr_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then case arg0^.literalType of
      lt_smallint : result:=recycler^.literalRecycler.newIntLiteral(sqr(int64(P_smallIntLiteral (arg0)^.value)));
      lt_bigint   : result:=recycler^.literalRecycler.newIntLiteral(P_bigIntLiteral(arg0)^.value*
                                          P_bigIntLiteral(arg0)^.value);
      lt_real: result:=recycler^.literalRecycler.newRealLiteral(sqr(P_realLiteral(arg0)^.value));
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
        RM_DEFAULT: result:=recycler^.literalRecycler.newRealLiteral(round  (x*pot)/pot);
        RM_UP     : result:=recycler^.literalRecycler.newRealLiteral(ceil64 (x*pot)/pot);
        RM_DOWN   : result:=recycler^.literalRecycler.newRealLiteral(floor64(x*pot)/pot);
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
      result:=recycler^.literalRecycler.newIntLiteral((xv div pot) * pot);
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
                     RM_DEFAULT: result:=recycler^.literalRecycler.newIntLiteral(round  (f));
                     RM_UP     : result:=recycler^.literalRecycler.newIntLiteral(ceil64 (f));
                     RM_DOWN   : result:=recycler^.literalRecycler.newIntLiteral(floor64(f));
                   end else begin
                     big.fromFloat(P_realLiteral(x)^.value,roundingMode);
                     result:=recycler^.literalRecycler.newIntLiteral(big);
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
      lt_bigint: result:=recycler^.literalRecycler.newIntLiteral(P_bigIntLiteral (arg0)^.value.sign);
      lt_smallint: if P_smallIntLiteral(arg0)^.value=0 then result:=arg0^.rereferenced
                   else if P_smallIntLiteral(arg0)^.value>0 then result:=recycler^.literalRecycler.newIntLiteral(1)
                   else result:=recycler^.literalRecycler.newIntLiteral(-1);
      lt_real: result:=recycler^.literalRecycler.newIntLiteral(sign(P_realLiteral(arg0)^.value));
      else result:=genericVectorization('sign',params,tokenLocation,context,recycler);
    end;
  end;

FUNCTION pi_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then result:=recycler^.literalRecycler.newRealLiteral(pi) else result:=nil;
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
    it:=P_collectionLiteral(x)^.tempIteratableList;
    result:=it[0];
    for x in it do if not(x^.leqForSorting(result)) then result:=x;
    result^.rereference;
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
    it:=P_collectionLiteral(x)^.tempIteratableList;
    result:=it[0];
    for x in it do if x^.leqForSorting(result) then result:=x;
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
      result:=recycler^.literalRecycler.newIntLiteral(imax);
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
        if x^.leqForSorting(xMin) then begin
          iMin:=i;
          xMin:=x;
        end;
      end;
      result:=recycler^.literalRecycler.newIntLiteral(iMin);
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
        then newSet:=newSetLiteral (length(mustContain))
        else newSet:=recycler^.literalRecycler.newListLiteral(length(mustContain));
        for i:=0 to length(mustContain)-1 do newSet^.append(@recycler^.literalRecycler,mustContain[i],true);
        resultSets^.append(@recycler^.literalRecycler,newSet,false);
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
      iter:=s^.tempIteratableList;
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

        resultSets:=recycler^.literalRecycler.newListLiteral();
        recurseBuildSets(mustContain,mightContain);
        if memoryPanic then begin
          if resultSets<>nil then recycler^.literalRecycler.disposeLiteral(resultSets);
          result:=nil;
        end else result:=resultSets;
      end else begin
        result:=recycler^.literalRecycler.newListLiteral
                  ^.append(@recycler^.literalRecycler, recycler^.literalRecycler.newListLiteral                                              ,false)
                  ^.append(@recycler^.literalRecycler, recycler^.literalRecycler.newListLiteral^.append(@recycler^.literalRecycler, arg0,true),false);
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
        newList:=recycler^.literalRecycler.newListLiteral(length(mustContain));
        for i:=0 to length(mustContain)-1 do newList^.append(@recycler^.literalRecycler,mustContain[i],true);
        setResult^.append(@recycler^.literalRecycler, newList,false);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and ((arg0^.literalType in C_listTypes) or (arg0^.literalType in C_setTypes)) then begin
      setLength(mustContain,0);
      iter:=collection0^.tempIteratableList;
      setLength(mightContain,length(iter));
      for i:=0 to length(mightContain)-1 do mightContain[i]:=iter[i];
      result:=newSetLiteral(length(iter));
      recurseBuildPermutations(mustContain,mightContain);
      if memoryPanic then begin
        recycler^.literalRecycler.disposeLiteral(result);
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
        factors.smallFactors:=factorizeSmall(P_smallIntLiteral(arg0)^.value);
        setLength(factors.bigFactors,0);
      end else begin
        factors:=bigint.factorize(P_bigIntLiteral(arg0)^.value,@context^.continueEvaluation);
      end;
      result:=recycler^.literalRecycler.newListLiteral(length(factors.smallFactors)+length(factors.bigFactors));
      for i:=0 to length(factors.smallFactors)-1 do listResult^.appendInt(@recycler^.literalRecycler,factors.smallFactors[i]);
      for i:=0 to length(factors.bigFactors)-1 do listResult^.append(@recycler^.literalRecycler,recycler^.literalRecycler.newIntLiteral(factors.bigFactors[i]),false);
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
      if (pMax<2) or (pMax>2147117569) then exit(recycler^.literalRecycler.newListLiteral);
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
      result:=recycler^.literalRecycler.newListLiteral;
      for i:=2 to length(isPrime)-1 do if isPrime[i] then result^.appendInt(@recycler^.literalRecycler,i);
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
      result:=recycler^.literalRecycler.newListLiteral(length(digits));
      for k:=length(digits)-1 downto 0 do result^.appendInt(@recycler^.literalRecycler,digits[k]);
      setLength(digits,0);
    end;

  FUNCTION smallDigitsOf(i:longint):P_listLiteral;
    VAR digits:T_arrayOfLongint;
        digit:longint;
        k:longint=0;
    begin
      if i=0 then exit(P_listLiteral(recycler^.literalRecycler.newListLiteral(1)^.appendInt(@recycler^.literalRecycler,0)));
      if i<0 then i:=-i;
      setLength(digits,32);
      while (i>0) do begin
        digit:=i mod smallBase;
        i    :=i div smallBase;
        digits[k]:=digit;
        inc(k);
      end;
      setLength(digits,k);
      result:=recycler^.literalRecycler.newListLiteral(k);
      for k:=length(digits)-1 downto 0 do result^.appendInt(@recycler^.literalRecycler,digits[k]);
      setLength(digits,0);
    end;

  FUNCTION digitsOf(CONST i:T_bigInt):P_listLiteral;
    VAR digits:T_arrayOfBigint;
        k:longint;
    begin
      digits:=bigDigits(i,bigBase);
      result:=recycler^.literalRecycler.newListLiteral(length(digits));
      for k:=length(digits)-1 downto 0 do result^.append(@recycler^.literalRecycler,recycler^.literalRecycler.newIntLiteral(digits[k]),false);
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
          result:=recycler^.literalRecycler.newListLiteral(list0^.size);
          for i:=0 to list0^.size-1 do case list0^.value[i]^.literalType of
            lt_bigint  : listResult^.append(@recycler^.literalRecycler,smallDigitsOf(P_bigIntLiteral  (list0^.value[i])^.value),false);
            lt_smallint: listResult^.append(@recycler^.literalRecycler,smallDigitsOf(P_smallIntLiteral(list0^.value[i])^.value),false);
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
          result:=recycler^.literalRecycler.newListLiteral(list0^.size);
          for i:=0 to list0^.size-1 do case list0^.value[i]^.literalType of
            lt_bigint  : listResult^.append(@recycler^.literalRecycler,digitsOf(P_bigIntLiteral  (list0^.value[i])^.value),false);
            lt_smallint: begin
              temp.fromInt(P_smallIntLiteral(list0^.value[i])^.value);
              listResult^.append(@recycler^.literalRecycler,digitsOf(temp),false);
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
        exit(recycler^.literalRecycler.newIntLiteral(0));
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
        result:=recycler^.literalRecycler.newRealLiteral(intPart.toFloat+fracPart);
        intPart.clear;
      end else result:=recycler^.literalRecycler.newIntLiteral(intPart);
      clearGarbage;
    end;
  end;

FUNCTION arctan2_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType in [lt_smallint,lt_bigint,lt_real]) and
       (arg1^.literalType in [lt_smallint,lt_bigint,lt_real]) then begin
      result:=recycler^.literalRecycler.newRealLiteral(arctan2(P_numericLiteral(arg0)^.floatValue,P_numericLiteral(arg1)^.floatValue));
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
      if workingSmall then result:=recycler^.literalRecycler.newIntLiteral(r)
                      else result:=recycler^.literalRecycler.newIntLiteral(bigR);
    end;
  end;

FUNCTION hammingWeight_impl intFuncSignature;
  VAR i:longint;
      r:longint=0;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then case arg0^.literalType of
      lt_bigint: result:=recycler^.literalRecycler.newIntLiteral(P_bigIntLiteral(arg0)^.value.hammingWeight);
      lt_smallint: begin
        i:=P_smallIntLiteral(arg0)^.value;
        while i>0 do begin
          if odd(i) then inc(r);
          i:=i shr 1;
        end;
        result:=recycler^.literalRecycler.newIntLiteral(r);
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
      then exit(recycler^.literalRecycler.newIntLiteral(smallPowMod(P_smallIntLiteral(arg0)^.value,
                                          P_smallIntLiteral(arg1)^.value,
                                          P_smallIntLiteral(arg2)^.value)));
      ensure(arg0,bx,ux);
      ensure(arg1,by,uy);
      ensure(arg2,bz,uz);
      result:=recycler^.literalRecycler.newIntLiteral(bx.powMod(by,bz));
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
      then result:=recycler^.literalRecycler.newIntLiteral(intResult)
      else begin
        intResult.clear;
        result:=recycler^.literalRecycler.newRealLiteral(Nan);
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
      result:=recycler^.literalRecycler.newIntLiteral(res);
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
        then exit(recycler^.literalRecycler.newListLiteral(2)^.appendReal(@recycler^.literalRecycler,Nan)^.appendReal(@recycler^.literalRecycler,Nan));
        case arg0^.literalType of
          lt_smallint:
            exit(recycler^.literalRecycler.newListLiteral(2)^
               .appendInt(@recycler^.literalRecycler,int0^.intValue div int1^.intValue)^
               .appendInt(@recycler^.literalRecycler,int0^.intValue mod int1^.intValue));
          lt_bigint: begin
            temp.fromInt(P_smallIntLiteral(arg1)^.value);
            P_bigIntLiteral(arg0)^.value.divMod(temp,q,r);
            temp.clear;
            exit(recycler^.literalRecycler.newListLiteral(2)^
              .append(@recycler^.literalRecycler,recycler^.literalRecycler.newIntLiteral(q),false)^
              .append(@recycler^.literalRecycler,recycler^.literalRecycler.newIntLiteral(r),false));
          end;
        end;
      end;
      lt_bigint: begin
        if P_bigIntLiteral(arg1)^.value.isZero
        then exit(recycler^.literalRecycler.newListLiteral(2)^.appendReal(@recycler^.literalRecycler,Nan)^.appendReal(@recycler^.literalRecycler,Nan));
        case arg0^.literalType of
          lt_smallint: begin
            temp.fromInt(P_smallIntLiteral(arg0)^.value);
            temp.divMod(P_bigIntLiteral(arg1)^.value,q,r);
            exit(recycler^.literalRecycler.newListLiteral(2)^
              .append(@recycler^.literalRecycler,recycler^.literalRecycler.newIntLiteral(q),false)^
              .append(@recycler^.literalRecycler,recycler^.literalRecycler.newIntLiteral(r),false));
          end;
          lt_bigint: begin
            P_bigIntLiteral(arg0)^.value.divMod(P_bigIntLiteral(arg1)^.value,q,r);
            exit(recycler^.literalRecycler.newListLiteral(2)^
              .append(@recycler^.literalRecycler,recycler^.literalRecycler.newIntLiteral(q),false)^
              .append(@recycler^.literalRecycler,recycler^.literalRecycler.newIntLiteral(r),false));
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
          for k:=0 to list0^.size-1 do total+=sqr(P_numericLiteral(list0^.value[k])^.floatValue);
          result:=recycler^.literalRecycler.newRealLiteral(sqrt(total));
        end;
        lt_smallint,lt_bigint,lt_real: result:=abs_imp(params,tokenLocation,context,recycler);
        else result:=genericVectorization('euklideanNorm',params,tokenLocation,context,recycler);
      end;
    end else result:=nil;
  end;

INITIALIZATION
  //Unary Numeric -> real
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'sqrt'  ,@sqrt_imp  ,ak_unary{$ifdef fullVersion},'sqrt(n);//Returns the square root of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'isqrt' ,@isqrt_imp ,ak_unary{$ifdef fullVersion},'isqrt(n);//Returns [floor(sqrt(n)),isSquare(n)]'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'sin'   ,@sin_imp   ,ak_unary{$ifdef fullVersion},'sin(n);//Returns the sine of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'arcsin',@arcsin_imp,ak_unary{$ifdef fullVersion},'arcsin(n);//Returns the arcsine of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'cos'   ,@cos_imp   ,ak_unary{$ifdef fullVersion},'cos(n);//Returns the cosine of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'arccos',@arccos_imp,ak_unary{$ifdef fullVersion},'arccos(n);//Returns the arccosine of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'tan'   ,@tan_imp   ,ak_unary{$ifdef fullVersion},'tan(n);//Returns the tangent of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'arctan',@arctan_imp,ak_unary{$ifdef fullVersion},'arctan(n);//Returns the arctangent of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'exp'   ,@exp_imp   ,ak_unary{$ifdef fullVersion},'exp(n);//Returns the exponential of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'ln'    ,@ln_imp    ,ak_unary{$ifdef fullVersion},'ln(n);//Returns the natural logarithm of numeric or expression parameter n'{$endif});
  //Unary Numeric -> same (i.e. I -> I, R -> R)
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'abs',@abs_imp,ak_unary{$ifdef fullVersion},'abs(n);//Returns the absolute value of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'sqr',@sqr_imp,ak_unary{$ifdef fullVersion},'sqr(n);//Returns the square of numeric or expression parameter n'{$endif});
  //Unary Numeric -> Integer
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'sign' ,@sign_imp ,ak_unary     {$ifdef fullVersion},'sign(n);//Returns the sign of numeric or expression parameter n'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'ceil' ,@ceil_imp ,ak_variadic_1{$ifdef fullVersion},'ceil(x);//Returns the smallest integer >=x#ceil(x,k);//Does the same but with k digits precision'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'floor',@floor_imp,ak_variadic_1{$ifdef fullVersion},'floor(x);//Returns the largest integer <=x#floor(x,k);//Does the same but with k digits precision'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'round',@round_imp,ak_variadic_1{$ifdef fullVersion},'round(x);//Returns the value of x, rounded to the nearest integer#round(x,k);//Returns the value of x rounded to k-digits precision'{$endif});

  builtinFunctionMap.registerRule(MATH_NAMESPACE,'pi'          ,@pi_imp           ,ak_nullary   {$ifdef fullVersion},'pi;//Returns pi'{$endif});
  BUILTIN_MAX:=
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'max'         ,@max_imp          ,ak_variadic_1{$ifdef fullVersion},'max(L);//Returns the greatest element out of list L#max(x,y,...);//Returns the greatest element out of the given parameters'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'argMax'      ,@argMax_imp       ,ak_unary     {$ifdef fullVersion},'argMax(L);//Returns the index of the greatest element out of list L (or the first index if ambiguous)'{$endif});
  BUILTIN_MIN:=
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'min'         ,@min_imp          ,ak_variadic_1{$ifdef fullVersion},'min(L);//Returns the smallest element out of list L#min(x,y,...);//Returns the smallest element out of the given parameters'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'argMin'      ,@argMin_imp       ,ak_unary     {$ifdef fullVersion},'argMin(L);//Returns the index of the smallest element out of list L (or the first index if ambiguous)'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'isNan'       ,@isNan_impl       ,ak_unary     {$ifdef fullVersion},'isNan(n);//Returns true if n is a number representing the value Not-A-Number'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'isInfinite'  ,@isInfinite_impl  ,ak_unary     {$ifdef fullVersion},'isInfinite(n);//Returns true if n is a number representing an infinite value'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'subSets'     ,@subSets_impl     ,ak_variadic_1{$ifdef fullVersion},'subSets(S);//Returns all distinct subsets of S#'+
                                                                              'subSets(S,k:Int);//Returns all distinct subsets of S having k elements'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'permutations',@permutations_impl,ak_unary     {$ifdef fullVersion},'permutations(L:List);//Returns a list of all permutations of S'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'factorize'   ,@factorize_impl   ,ak_unary     {$ifdef fullVersion},'factorize(i:Int);//Returns a list of all prime factors of i'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'isPrime'     ,@isPrime_impl     ,ak_unary     {$ifdef fullVersion},'isPrime(i:Int);//Returns true if i is a prime, false otherwise#//result is guaranteed to be correct for i<3.317E23'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'primes'      ,@primes_impl      ,ak_unary     {$ifdef fullVersion},'primes(pMax:Int);//Returns prime numbers up to pMax'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'digits'      ,@digits_impl      ,ak_variadic_1{$ifdef fullVersion},'digits(i>=0);//Returns the digits of i (base 10)#digits(i>=0,base>1);//Returns the digits of i for a custom base'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'composeDigits' ,@composeDigits_imp  ,ak_variadic_1{$ifdef fullVersion},'composeDigits(digits:IntList);//Returns a number constructed from digits (base 10)#'+
                                                                                  'composeDigits(digits:IntList,base:Int);//Returns a number constructed from digits with given base #'+
                                                                                  'composeDigits(digits:IntList,base:Int,shift:Int);//Returns a number constructed from digits with given base and shift'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'arctan2'       ,@arctan2_impl       ,ak_binary    {$ifdef fullVersion},'arctan2(x,y);//Calculates arctan(x/y) and returns an angle in the correct quadrant'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'gcd'           ,@gcd_impl           ,ak_variadic_1{$ifdef fullVersion},'gcd(x:Int,...);//Returns the greatest common divider of all arguments (only integers accepted)'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'hammingWeight' ,@hammingWeight_impl ,ak_unary     {$ifdef fullVersion},'hammingWeight(x:Int);//Returns the hamming weight (i.e. number of true bits) in x'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'powMod'        ,@powMod_impl        ,ak_ternary   {$ifdef fullVersion},'powMod(x>=0,y>=0,z>=0);//Returns x^y mod z'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'modularInverse',@modularInverse_impl,ak_binary    {$ifdef fullVersion},'modularInverse(x>0,m>0);//Returns the modular inverse of x with respect to modul m or NaN if no modular inverse exists'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'shiftRight'    ,@bitShift_impl      ,ak_binary{$ifdef fullVersion},'bitShift(x:Int,bitsToShift:Int);//Shifts integer x right by the given number of bits#//If bitsToShift<0 a shift-left is performed'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'divMod'        ,@divMod_impl        ,ak_binary{$ifdef fullVersion},'divMod(x:Int,y:Int);//Returns a pair [x div y, x mod y]'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'euklideanNorm' ,@euklideanNorm_impl ,ak_unary {$ifdef fullVersion},'euklideanNorm(v:NumericList);//returns the Euklidean norm of vector v'{$endif});
end.
