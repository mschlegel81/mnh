UNIT mnh_funcs_math;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,
     math,
     bigint,
     myGenerics,
     mnh_basicTypes,mnh_constants,
     mnh_litVar,
     mnh_funcs,
     mnh_out_adapters,
     mnh_contexts;
VAR BUILTIN_MIN,
    BUILTIN_MAX:P_intFuncCallback;
IMPLEMENTATION
{$i mnh_func_defines.inc}
{$define UNARY_NUM_TO_REAL:=FUNCTION recurse(CONST x:P_literal):P_literal;
  VAR iter:T_arrayOfLiteral;
      y:P_literal;
  begin
    result:=nil;
    case x^.literalType of
      lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction(ID_MACRO,tokenLocation,@context);
      lt_int : try result:=newRealLiteral(CALL_MACRO(P_intLiteral (x)^.value.toFloat)); except result:=newRealLiteral(Nan) end;
      lt_real: try result:=newRealLiteral(CALL_MACRO(P_realLiteral(x)^.value        )); except result:=newRealLiteral(Nan) end;
      lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
      lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: begin
        result:=P_collectionLiteral(x)^.newOfSameType(true);
        iter  :=P_collectionLiteral(x)^.iteratableList;
        for y in iter do collResult^.append(recurse(y),false);
        disposeLiteral(iter);
        if collResult^.containsError then begin
          raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
          disposeLiteral(result);
        end;
      end;
      else raiseNotApplicableError(ID_MACRO,x,tokenLocation,context.adapters^);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1)
  then result:=recurse(arg0);
end}

FUNCTION sqrt_imp intFuncSignature;
{$define CALL_MACRO:=sqrt}
{$define ID_MACRO:='sqrt'}
UNARY_NUM_TO_REAL;

FUNCTION sin_imp intFuncSignature;
{$define CALL_MACRO:=sin}
{$define ID_MACRO:='sin'}
UNARY_NUM_TO_REAL;

FUNCTION arcsin_imp intFuncSignature;
{$define CALL_MACRO:=arcsin}
{$define ID_MACRO:='arcsin'}
UNARY_NUM_TO_REAL;

FUNCTION cos_imp intFuncSignature;
{$define CALL_MACRO:=cos}
{$define ID_MACRO:='cos'}
UNARY_NUM_TO_REAL;

FUNCTION arccos_imp intFuncSignature;
{$define CALL_MACRO:=arccos}
{$define ID_MACRO:='arccos'}
UNARY_NUM_TO_REAL;

FUNCTION tan_imp intFuncSignature;
{$define CALL_MACRO:=tan}
{$define ID_MACRO:='tan'}
UNARY_NUM_TO_REAL;

FUNCTION arctan_imp intFuncSignature;
{$define CALL_MACRO:=arctan}
{$define ID_MACRO:='arctan'}
UNARY_NUM_TO_REAL;

FUNCTION exp_imp intFuncSignature;
{$define CALL_MACRO:=exp}
{$define ID_MACRO:='exp'}
UNARY_NUM_TO_REAL;

FUNCTION ln_imp intFuncSignature;
{$define CALL_MACRO:=ln}
{$define ID_MACRO:='ln'}
UNARY_NUM_TO_REAL;

{$undef CALL_MACRO}
{$undef ID_MACRO}
{$undef UNARY_NUM_TO_REAL}

FUNCTION not_imp intFuncSignature;
  FUNCTION not_rec(CONST x:P_literal):P_literal;
    VAR y:P_literal;
        iter:T_arrayOfLiteral;
        zero:T_bigint;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction('not',tokenLocation,@context);
        lt_boolean: result:=newBoolLiteral(not(P_boolLiteral(x)^.value));
        lt_int:     begin
                      zero.createZero;
                      result:=newIntLiteral (P_intLiteral (x)^.value.bitXor(zero));
                      zero.destroy;
                    end;
        lt_list,lt_booleanList,lt_intList,lt_emptyList,
        lt_set ,lt_booleanSet ,lt_intSet ,lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType(true);
          iter:=P_collectionLiteral(x)^.iteratableList;
          for y in iter do P_collectionLiteral(result)^.append(not_rec(y),false);
          disposeLiteral(iter);
          if collResult^.containsError then begin
            raiseNotApplicableError('not',x,tokenLocation,context.adapters^);
            disposeLiteral(result);
          end;
        end;
        else raiseNotApplicableError('not',x,tokenLocation,context.adapters^);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=not_rec(arg0);
  end;

FUNCTION abs_imp intFuncSignature;
  FUNCTION recurse(CONST x:P_literal):P_literal;
    VAR sub:P_literal;
        iter:T_arrayOfLiteral;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction('abs',tokenLocation,@context);
        lt_error: begin result:=x; result^.rereference; end;
        lt_int : if P_intLiteral(x)^.value.isNegative
                 then result:=newIntLiteral(P_intLiteral(x)^.value.negated)
                 else result:=x^.rereferenced;
        lt_real: if P_realLiteral(x)^.value<0
                 then result:=newRealLiteral(-P_realLiteral(x)^.value)
                 else result:=x^.rereferenced;
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType(true);
          iter  :=P_collectionLiteral(x)^.iteratableList;
          for sub in iter do collResult^.append(recurse(sub),false);
          disposeLiteral(iter);
          if collResult^.containsError then begin
            raiseNotApplicableError('abs',x,tokenLocation,context.adapters^);
            disposeLiteral(result);
          end;
        end;
        else raiseNotApplicableError('abs',x,tokenLocation,context.adapters^);
      end;
    end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1)
  then result:=recurse(arg0);
end;

FUNCTION sqr_imp intFuncSignature;
  FUNCTION recurse(CONST x:P_literal):P_literal;
    VAR sub:P_literal;
        iter:T_arrayOfLiteral;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction('sqr',tokenLocation,@context);
        lt_error: begin result:=x; result^.rereference; end;
        lt_int : result:=newIntLiteral (P_intLiteral (x)^.value.mult(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(sqr(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType(true);
          iter  :=P_collectionLiteral(x)^.iteratableList;
          for sub in iter do collResult^.append(recurse(sub),false);
          disposeLiteral(iter);
          if collResult^.containsError then begin
            raiseNotApplicableError('sqr',x,tokenLocation,context.adapters^);
            disposeLiteral(result);
          end;
        end;
        else raiseNotApplicableError('sqr',x,tokenLocation,context.adapters^);
      end;
    end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1)
  then result:=recurse(arg0);
end;

FUNCTION customRound(CONST x:P_literal; CONST relevantDigits:longint; CONST roundingMode:T_roundingMode;
                     CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal;
  CONST funcName:array[T_roundingMode] of string=('round',  //RM_DEFAULT,
                                                  'ceil',   //RM_UP,
                                                  'floor'); //RM_DOWN
  FUNCTION myRound(CONST x:T_myFloat; CONST y:int64):P_literal; inline;
      VAR pot:T_myFloat;
          i:int64;
      begin
        pot:=1;
        i:=0;
        while (i<y) and (i< 20) do begin pot:=pot*10;  inc(i); end;
        while (i>y) and (i>-20) do begin pot:=pot*0.1; dec(i); end;
        case roundingMode of
          RM_DEFAULT: result:=newRealLiteral(round  (x*pot)/pot);
          RM_UP     : result:=newRealLiteral(ceil64 (x*pot)/pot);
          RM_DOWN   : result:=newRealLiteral(floor64(x*pot)/pot);
        end;
      end;

  FUNCTION myRound(CONST x:P_intLiteral; CONST y:int64):P_literal; inline;
    VAR i   :longint=0;
        pot :digitType=1;
        xv  :int64;
    begin
      if y>=0 then exit(x^.rereferenced);
      while (i>y) and (i>-19) do begin pot:=pot*10; dec(i); end;
      if not(x^.value.canBeRepresentedAsInt64()) then begin
        raise Exception.create('Unimplemented');
      end;
      xv:=x^.value.toInt;
      xv  :=xv div pot;
      result:=newIntLiteral((xv div pot) * pot);
    end;

  VAR big:T_bigint;
      sub:P_literal;
      iter:T_arrayOfLiteral;
  begin
    result:=nil;
    if relevantDigits=0 then begin
      result:=nil;
      case x^.literalType of
        lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction(funcName[roundingMode],location,@context);
        lt_error,lt_int: result:=x^.rereferenced;
        lt_real: if not(isNan(P_realLiteral(x)^.value)) and not(isInfinite(P_realLiteral(x)^.value))
                 then begin
                   big.fromFloat(P_realLiteral(x)^.value,roundingMode);
                   result:=newIntLiteral(big);
                 end;
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType(true);
          iter  :=P_collectionLiteral(x)^.iteratableList;
          for sub in iter do collResult^.append(customRound(sub,0,roundingMode,location,context),false);
          disposeLiteral(iter);
          if collResult^.containsError then begin
            raiseNotApplicableError(funcName[roundingMode],x,location,context.adapters^);
            disposeLiteral(result);
          end;
        end;
        else raiseNotApplicableError(funcName[roundingMode],x,location,context.adapters^);
      end;
    end else begin
      result:=nil;
      case x^.literalType of
        lt_error: result:=x^.rereferenced;
        lt_int : result:=myRound(P_intLiteral(x),relevantDigits);
        lt_real: if not(isNan(P_realLiteral(x)^.value)) and not(isInfinite(P_realLiteral(x)^.value))
                 then result:=myRound(P_realLiteral(x)^.value,relevantDigits)
                 else raiseNotApplicableError(funcName[roundingMode],x,location,context.adapters^);
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList:
          begin
            result:=newListLiteral;
            iter:=P_collectionLiteral(x)^.iteratableList;
            for sub in iter do P_listLiteral(result)^.append(customRound(sub,relevantDigits,roundingMode,location,context),false);
            disposeLiteral(iter);
            if collResult^.containsError then begin
              disposeLiteral(result);
              raiseNotApplicableError(funcName[roundingMode],x,location,context.adapters^);
            end;
          end;

        lt_set,lt_intSet,lt_realSet,lt_numSet,lt_emptySet:  begin
            result:=newSetLiteral;
            iter:=P_collectionLiteral(x)^.iteratableList;
            for sub in iter do P_setLiteral(result)^.append(customRound(sub,relevantDigits,roundingMode,location,context),false);
            disposeLiteral(iter);
            if collResult^.containsError then begin
              disposeLiteral(result);
              raiseNotApplicableError(funcName[roundingMode],x,location,context.adapters^);
            end;
          end;

        else raiseNotApplicableError(funcName[roundingMode],x,location,context.adapters^);
      end;
    end;
  end;

{
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=recurse1(arg0) else
    if (params<>nil) and (params^.size=2) then result:=recurse2(arg0,arg1);
  end}

FUNCTION round_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)                                then result:=customRound(arg0,0                ,RM_DEFAULT,tokenLocation,context) else
    if (params<>nil) and (params^.size=2) and (arg1^.literalType=lt_int) then result:=customRound(arg0,int1^.value.toInt,RM_DEFAULT,tokenLocation,context);
  end;

FUNCTION floor64(CONST d:T_myFloat):int64; begin result:=trunc(d); if frac(d)<0 then dec(result); end;
FUNCTION ceil64 (CONST d:T_myFloat):int64; begin result:=trunc(d); if frac(d)>0 then inc(result); end;

FUNCTION ceil_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)                                then result:=customRound(arg0,0                ,RM_UP,tokenLocation,context) else
    if (params<>nil) and (params^.size=2) and (arg1^.literalType=lt_int) then result:=customRound(arg0,int1^.value.toInt,RM_UP,tokenLocation,context);
  end;

FUNCTION floor_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)                                then result:=customRound(arg0,0                ,RM_DOWN,tokenLocation,context) else
    if (params<>nil) and (params^.size=2) and (arg1^.literalType=lt_int) then result:=customRound(arg0,int1^.value.toInt,RM_DOWN,tokenLocation,context);
  end;

FUNCTION sign_imp intFuncSignature;
  FUNCTION sign_rec(CONST x:P_literal):P_literal;
    VAR iter:T_arrayOfLiteral;
        sub:P_literal;
    begin
      result:=nil;
      case x^.literalType of
        lt_expression: result:=P_expressionLiteral(x)^.applyBuiltinFunction('sign',tokenLocation,@context);
        lt_error: result:=x^.rereferenced;
        lt_int : result:=newIntLiteral(P_intLiteral (x)^.value.sign);
        lt_real: result:=newIntLiteral(sign(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType(true);
          iter:=P_collectionLiteral(x)^.iteratableList;
          for sub in iter do collResult^.append(sign_rec(sub),false);
          disposeLiteral(iter);
          if collResult^.containsError then begin
            disposeLiteral(result);
            raiseNotApplicableError('sign',x,tokenLocation,context.adapters^);
          end;
        end;
        else raiseNotApplicableError('sign',x,tokenLocation,context.adapters^);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=sign_rec(arg0);
  end;

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
      xMax:=L^.value[0];
      for i:=1 to L^.size-1 do begin
        x:=L^.value[i];
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
      iter:T_arrayOfLiteral;
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
          result:=collection0^.newOfSameType(true);
          iter:=collection0^.iteratableList;
          for x in iter do collResult^.appendBool((x^.literalType=lt_real) and PREDICATE(P_realLiteral(x)^.value));
          disposeLiteral(iter);
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
      acceptOnlySetsOfSize:longint=-1;
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
        if (acceptOnlySetsOfSize<>-1) and (acceptOnlySetsOfSize<>length(mustContain)) then exit;
        newSet:=collection0^.newOfSameType(false);
        for i:=0 to length(mustContain)-1 do newSet^.append(mustContain[i],true);
        if sets.get(newSet,0)=0 then sets.put(newSet,1)
                                else disposeLiteral(newSet);
      end;
    end;

  FUNCTION directBuildSets(mightContain:T_arrayOfLiteral):boolean;
    VAR maxInt,prevInt,currInt:qword;
        bits:bitpacked array [0..8*sizeOf(qword)-1] of boolean;
        k,trueCount:longint;
        newSet:P_collectionLiteral;
        prevDummy:byte;

    PROCEDURE next; inline;
      VAR lo,lz:qword;
      begin
        lo:=currInt and -currInt;
        lz:=(currInt+lo) and not(currInt);
        currInt:=currInt or lz;
        currInt:=currInt and not(lz-1);
        currInt:=currInt or ((lz div lo) shr 1)-1;
      end;

    begin
      initialize(bits);
      if acceptOnlySetsOfSize=0 then begin
        if collection0^.literalType in C_setTypes
        then newSet:=newSetLiteral
        else newSet:=newListLiteral;
        sets.put(newSet,1);
        exit(true);
      end;
      if (acceptOnlySetsOfSize>length(mightContain)) or (length(mightContain)=0) then exit(true);
      if (length(mightContain)>=length(bits)-1)                                  then exit(false);

      maxInt:=1;
      for k:=1 to length(mightContain)-1 do maxInt:=maxInt+maxInt+1;
      if (acceptOnlySetsOfSize>0) then begin
        currInt:=qword(1) shl acceptOnlySetsOfSize-1;
        prevInt:=0;
        while (prevInt<currInt) and (currInt<=maxInt) do begin
          move(currInt,bits,sizeOf(qword));
          if collection0^.literalType in C_setTypes
          then newSet:=newSetLiteral (acceptOnlySetsOfSize)
          else newSet:=newListLiteral(acceptOnlySetsOfSize);
          for k:=0 to length(bits)-1 do if bits[k] then newSet^.append(mightContain[k],true);
          if not(sets.putNew(newSet,1,prevDummy)) then disposeLiteral(newSet);

          prevInt:=currInt;
          next;
        end;
      end else begin
        currInt:=0;
        while (currInt<=maxInt) do begin
          trueCount:=PopCnt(currInt);
          move(currInt,bits,sizeOf(qword));
          if collection0^.literalType in C_setTypes
          then newSet:=newSetLiteral (trueCount)
          else newSet:=newListLiteral(trueCount);
          for k:=0 to length(bits)-1 do if bits[k] then newSet^.append(mightContain[k],true);
          if not(sets.putNew(newSet,1,prevDummy)) then disposeLiteral(newSet);
          inc(currInt);
        end;
      end;
      result:=true;
    end;

  VAR mustContain,mightContain:T_arrayOfLiteral;
      i:longint;
      tempList:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=2) and ((params^.size=1) or (arg1^.literalType=lt_int)) then begin
      if params^.size=2 then acceptOnlySetsOfSize:=int1^.value.toInt;
      if (arg0^.literalType in C_listTypes) or (arg0^.literalType in C_setTypes) then begin
        sets.create;
        setLength(mustContain,0);
        if arg0^.literalType in C_listTypes then begin
          tempList:=P_listLiteral(list0^.clone);
          tempList^.sort;
          mightContain:=tempList^.iteratableList;
          disposeLiteral(tempList);
        end else mightContain:=set0^.iteratableList;

        if not(directBuildSets(            mightContain)) then
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
  VAR mustContain,mightContain,iter:T_arrayOfLiteral;
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
      iter:=compound0^.iteratableList;
      setLength(mightContain,length(iter));
      for i:=0 to length(mightContain)-1 do mightContain[i]:=iter[i];
      result:=newSetLiteral;
      recurseBuildPermutations(mustContain,mightContain);
      disposeLiteral(iter);
    end;
  end;

FUNCTION factorize_impl intFuncSignature;
  {$define DIVIDE_THROUGH:=while n mod p=0 do begin n:=n div p; listResult^.appendInt(p); end}
  VAR n,p:int64;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin
      n:=int0^.value.toInt;
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
      while (p*p<=n) and (context.adapters^.noErrors) do begin
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
    then result:=sievePrimes(int0^.value.toInt)
    else result:=nil;
  end;

FUNCTION digits_impl intFuncSignature;
  VAR base:int64=10;
  FUNCTION digitsOf(CONST i:T_bigint):P_listLiteral;
    VAR digits:T_arrayOfLongint;
        k:longint;
    begin
      digits:=i.getDigits(base);
      result:=newListLiteral(length(digits));
      for k:=length(digits)-1 downto 0 do result^.appendInt(digits[k]);
    end;

  VAR j:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=2) and
      (arg0^.literalType in [lt_int,lt_emptyList,lt_intList]) and
      ((params^.size<2) or (arg1^.literalType=lt_int))  then begin
      if params^.size=2 then base:=int1^.value.toInt;
      if base<=1 then begin
        context.adapters^.raiseError('Cannot determine digits with base '+arg1^.toString,tokenLocation);
        exit(nil);
      end;
      if arg0^.literalType=lt_int then exit(digitsOf(int0^.value));
      result:=collection0^.newOfSameType(true);
      for j:=0 to list0^.size-1 do collResult^.append(digitsOf(P_intLiteral(list0^.value[j])^.value),false);
    end;
  end;

FUNCTION composeDigits_imp intFuncSignature;
  VAR base:int64=10;
      Shift:int64=0;

      k  :longint;
      dig:int64    =0;
      ir :int64    =0;
      fr :T_myFloat=0;
      invalidatedIntResult:boolean=false;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=3) and
       (arg0^.literalType in [lt_emptyList,lt_intList]) then begin
      if params^.size>=2 then begin
        if arg1^.literalType<>lt_int then exit(nil) else base:=int1^.value.toInt;
        if (base<=1) or not(int1^.value.canBeRepresentedAsInt64()) then begin
          context.adapters^.raiseError('Cannot compose digits with base '+arg1^.toString,tokenLocation);
          exit(nil);
        end;
      end;
      if params^.size=3 then begin
        if arg2^.literalType<>lt_int then exit(nil) else Shift:=int2^.value.toInt;
      end;
      if arg0^.literalType=lt_emptyList then exit(newIntLiteral(0));

      {$Q-}{$R-}
      for k:=0 to list0^.size-1 do begin
        dig:=P_intLiteral(list0^.value[k])^.value.toInt;
        if (dig>=base) or (dig<0) then context.adapters^.raiseError('Digits must be in range 0..base-1',tokenLocation);
        ir:=ir*base+dig;
        fr:=fr*base+dig;
        invalidatedIntResult:=invalidatedIntResult or (ir<0);
      end;
      if Shift<0 then for k:=-1 downto Shift do begin
        ir:=ir*base+dig;
        fr:=fr*base+dig;
        invalidatedIntResult:=invalidatedIntResult or (ir<0);
      end else if Shift>0 then begin
        dig:=1;
        for k:=1 to Shift do fr:=fr/base;
        invalidatedIntResult:=true;
      end;
      {$Q+}{$R+}
      if invalidatedIntResult then exit(newRealLiteral(fr))
                              else exit(newIntLiteral (ir));
    end;
  end;

FUNCTION arctan2_impl intFuncSignature;
  VAR x,y:T_myFloat;
  begin
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType in [lt_int,lt_real]) and
       (arg1^.literalType in [lt_int,lt_real]) then begin
      if arg0^.literalType=lt_int then x:=int0^.value.toFloat
                                  else x:=real0^.value;
      if arg1^.literalType=lt_int then y:=int1^.value.toFloat
                                  else y:=real1^.value;
      result:=newRealLiteral(arctan2(x,y));
    end else result:=nil;
  end;

INITIALIZATION
  //Unary Numeric -> real
  registerRule(MATH_NAMESPACE,'sqrt'  ,@sqrt_imp  ,ak_unary,'sqrt(n);//Returns the square root of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'sin'   ,@sin_imp   ,ak_unary,'sin(n);//Returns the sine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arcsin',@arcsin_imp,ak_unary,'arcsin(n);//Returns the arcsine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'cos'   ,@cos_imp   ,ak_unary,'cos(n);//Returns the cosine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arccos',@arccos_imp,ak_unary,'arccos(n);//Returns the arccosine of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'tan'   ,@tan_imp   ,ak_unary,'tan(n);//Returns the tangent of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'arctan',@arctan_imp,ak_unary,'arctan(n);//Returns the arctangent of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'exp'   ,@exp_imp   ,ak_unary,'exp(n);//Returns the exponential of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'ln'    ,@ln_imp    ,ak_unary,'ln(n);//Returns the natural logarithm of numeric or expression parameter n');
  //Unary Boolean -> boolean
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'not',@not_imp,ak_unary,'not(b:boolean);#not(b:booleanList);//Returns the negated value of b#not(i:int);#not(i:intList);//Returns the bitwise negated value of i');
  //Unary Numeric -> same (i.e. I -> I, R -> R)
  registerRule(MATH_NAMESPACE,'abs',@abs_imp,ak_unary,'abs(n);//Returns the absolute value of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'sqr',@sqr_imp,ak_unary,'sqr(n);//Returns the square of numeric or expression parameter n');
  //Unary Numeric -> Integer
  registerRule(MATH_NAMESPACE,'sign' ,@sign_imp ,ak_unary     ,'sign(n);//Returns the sign of numeric or expression parameter n');
  registerRule(MATH_NAMESPACE,'ceil' ,@ceil_imp ,ak_variadic_1,'ceil(x);//Returns the smallest integer >=x#ceil(x,k);//Does the same but with k digits precision');
  registerRule(MATH_NAMESPACE,'floor',@floor_imp,ak_variadic_1,'floor(x);//Returns the largest integer <=x#floor(x,k);//Does the same but with k digits precision');
  registerRule(MATH_NAMESPACE,'round',@round_imp,ak_variadic_1,'round(x);//Returns the value of x, rounded to the nearest integer#round(x,k);//Returns the value of x rounded to k-digits precision');

  registerRule(MATH_NAMESPACE,'pi'          ,@pi_imp           ,ak_nullary   ,'pi;//Returns pi');
  BUILTIN_MAX:=
  registerRule(MATH_NAMESPACE,'max'         ,@max_imp          ,ak_variadic_1,'max(L);//Returns the greatest element out of list L#max(x,y,...);//Returns the greatest element out of the given parameters');
  registerRule(MATH_NAMESPACE,'argMax'      ,@argMax_imp       ,ak_unary     ,'argMax(L);//Returns the index of the greatest element out of list L (or the first index if ambiguous)');
  BUILTIN_MIN:=
  registerRule(MATH_NAMESPACE,'min'         ,@min_imp          ,ak_variadic_1,'min(L);//Returns the smallest element out of list L#min(x,y,...);//Returns the smallest element out of the given parameters');
  registerRule(MATH_NAMESPACE,'argMin'      ,@argMin_imp       ,ak_unary     ,'argMin(L);//Returns the index of the smallest element out of list L (or the first index if ambiguous)');
  registerRule(MATH_NAMESPACE,'isNan'       ,@isNan_impl       ,ak_unary     ,'isNan(n);//Returns true if n is a number representing the value Not-A-Number');
  registerRule(MATH_NAMESPACE,'isInfinite'  ,@isInfinite_impl  ,ak_unary     ,'isInfinite(n);//Returns true if n is a number representing an infinite value');
  registerRule(MATH_NAMESPACE,'subSets'     ,@subSets_impl     ,ak_variadic_1,'subSets(S);//Returns all distinct subsets of S#'+
                                                                                 'subSets(S,k:int);//Returns all distinct subsets of S having k elements');
  registerRule(MATH_NAMESPACE,'permutations',@permutations_impl,ak_unary     ,'permutations(L:list);//Returns a list of all permutations of S');
  registerRule(MATH_NAMESPACE,'factorize'   ,@factorize_impl   ,ak_unary     ,'factorize(i:int);//Returns a list of all prime factors of i');
  registerRule(MATH_NAMESPACE,'primes'      ,@primes_impl      ,ak_unary     ,'primes(pMax:int);//Returns prime numbers up to pMax');
  registerRule(MATH_NAMESPACE,'digits'      ,@digits_impl      ,ak_variadic_1,'digits(i>=0);//Returns the digits of i (base 10)#digits(i>=0,base>1);//Returns the digits of i for a custom base');
  registerRule(MATH_NAMESPACE,'composeDigits',@composeDigits_imp,ak_variadic_1,'composeDigits(digits:intList);//Returns a number constructed from digits (base 10)#'+
                                                                                  'composeDigits(digits:intList,base:int);//Returns a number constructed from digits with given base #'+
                                                                                  'composeDigits(digits:intList,base:int,shift:int);//Returns a number constructed from digits with given base and shift');
  registerRule(MATH_NAMESPACE,'arctan2'     ,@arctan2_impl     ,ak_binary    ,'arctan2(x,y);//Calculates arctan(x/y) and returns an angle in the correct quadrant');
end.
