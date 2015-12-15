UNIT mnh_funcs_math;
INTERFACE
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
    then x:=params^.value(0)
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
    if (params<>nil) and (params^.size=1) and (params^.value(0)<>nil) and (params^.value(0)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
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
    then x:=params^.value(0)
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
    if (params<>nil) and (params^.size=1) and (params^.value(0)<>nil) and (params^.value(0)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
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
       (params^.value(0)^.literalType in [lt_real,lt_int,lt_realList,lt_intList,lt_numList,lt_emptyList]) then begin
      case params^.value(0)^.literalType of
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
       (params^.value(0)^.literalType in [lt_real,lt_int,lt_realList,lt_intList,lt_numList,lt_emptyList]) then begin
      case params^.value(0)^.literalType of
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

FUNCTION isInRange_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR r0,r1:T_myFloat;
  FUNCTION inRange(CONST L:P_literal):boolean; inline;
    VAR i:int64;
        r:T_myFloat;
    begin
      if l^.literalType=lt_real then begin
        r:=P_realLiteral(l)^.value;
        result:=not(isNan(r)) and not(isInfinite(r)) and (r0<=r) and (r<=r1);
      end else begin
        i:=P_intLiteral(l)^.value;
        result:=(r0<=i) and (i<=r1);
      end;
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (params^.value(0)^.literalType in [lt_real,lt_int,lt_realList,lt_intList,lt_numList,lt_emptyList]) and
       (params^.value(1)^.literalType in [lt_real,lt_int]) and
       (params^.value(2)^.literalType in [lt_real,lt_int]) then begin
      if params^.value(1)^.literalType=lt_real
        then r0:=P_realLiteral(params^.value(1))^.value
        else r0:=P_intLiteral (params^.value(1))^.value;
      if params^.value(2)^.literalType=lt_real
        then r1:=P_realLiteral(params^.value(2))^.value
        else r1:=P_intLiteral (params^.value(2))^.value;
      if params^.value(0)^.literalType in [lt_real,lt_int] then exit(newBoolLiteral(inRange(params^.value(0))))
      else begin
        result:=newListLiteral;
        for i:=0 to list0^.size-1 do
          P_listLiteral(result)^.appendBool(inRange(list0^.value(i)));
      end;
    end;
  end;


INITIALIZATION
  registerRule(MATH_NAMESPACE,'max',@max_imp,'max(L);#Returns the greatest element out of list L#max(x,y,...);#Returns the greatest element out of the given parameters');
  registerRule(MATH_NAMESPACE,'argMax',@argMax_imp,'argMax(L);#Returns the index of the greatest element out of list L (or the first index if ambiguous)');
  registerRule(MATH_NAMESPACE,'min',@min_imp,'min(L);#Returns the smallest element out of list L#min(x,y,...);#Returns the smallest element out of the given parameters');
  registerRule(MATH_NAMESPACE,'argMin',@argMin_imp,'argMin(L);#Returns the index of the smallest element out of list L (or the first index if ambiguous)');
  registerRule(MATH_NAMESPACE,'isNan',@isNan_impl,'isNan(n);#Returns true if n is a number representing the value Not-A-Number');
  registerRule(MATH_NAMESPACE,'isInfinite',@isInfinite_impl,'isInfinite(n);#Returns true if n is a number representing an infinite value');
  registerRule(MATH_NAMESPACE,'isInRange',@isInRange_impl,'isInRange(x,x0,x1);#Returns true, if x0<=x<=x1 and x is neither Not-A-Number nor infinite');
  BUILTIN_MIN:=@min_imp;
  BUILTIN_MAX:=@max_imp;
end.
