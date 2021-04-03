UNIT funcs_interpolators;
INTERFACE
USES serializationUtil,
     mnh_constants,
     myGenerics,
     basicTypes,
     out_adapters,
     recyclers,
     contexts,
     litVar,subrules;

TYPE
  T_interpolator=object(T_expression)
    protected
      underlyingValues:P_listLiteral;
      FUNCTION getParameterNames(CONST literalRecycler:P_literalRecycler):P_listLiteral; virtual;
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual; abstract;
      FUNCTION getEquivalentInlineExpression(CONST context:P_context; CONST recycler:P_recycler):P_inlineExpression; virtual;
    public
      CONSTRUCTOR createInterpolator(CONST id_:string; CONST values:P_listLiteral; CONST location:T_tokenLocation);
      DESTRUCTOR destroy; virtual;

      FUNCTION evaluateToBoolean(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST allowRaiseError:boolean; CONST a:P_literal; CONST b:P_literal):boolean; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal; CONST b:P_literal):T_evaluationResult; virtual;
      FUNCTION evaluate         (CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST parameters:P_listLiteral):T_evaluationResult; virtual;
      FUNCTION arity:T_arityInfo; virtual;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      FUNCTION toString(CONST lengthLimit: longint=maxLongint): ansistring; virtual;
      PROCEDURE cleanup(CONST literalRecycler: P_literalRecycler); virtual;

      FUNCTION referencesAnyUserPackage: boolean; virtual;
      FUNCTION writeToStream(CONST literalRecycler: P_literalRecycler; CONST locationOfSerializeCall: T_tokenLocation; CONST adapters: P_messages; CONST stream: P_outputStreamWrapper): boolean; virtual;
  end;

IMPLEMENTATION
USES funcs,tokens,sysutils,math;
FUNCTION T_interpolator.getParameterNames(CONST literalRecycler: P_literalRecycler): P_listLiteral;
  begin
    result:=P_listLiteral(literalRecycler^.newListLiteral^.appendString(literalRecycler,'i'));
  end;

FUNCTION T_interpolator.getEquivalentInlineExpression(CONST context: P_context; CONST recycler: P_recycler): P_inlineExpression;
  VAR first:P_token;
  begin
    first:=P_recycler(recycler)^.newToken(getLocation,getId,tt_literal,rereferenced);
    first^.next:=getParametersForPseudoFuncPtr(1,false,getLocation,P_context(context),recycler);
    new(result,createFromInline(first,P_context(context),recycler));
  end;

CONSTRUCTOR T_interpolator.createInterpolator(CONST id_:string; CONST values: P_listLiteral; CONST location: T_tokenLocation);
  begin
    inherited create(id_,et_builtin,location);
    values^.rereference;
    underlyingValues:=values;
  end;

DESTRUCTOR T_interpolator.destroy;
  begin
    assert(underlyingValues=nil);
    inherited;
  end;

FUNCTION T_interpolator.evaluateToBoolean(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: pointer; CONST allowRaiseError: boolean; CONST a: P_literal; CONST b: P_literal): boolean;
  begin
    if allowRaiseError then context^.raiseError('Interpolators always return numeric values!',location);
    result:=false;
  end;

FUNCTION T_interpolator.evaluateToLiteral(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: pointer; CONST a: P_literal; CONST b: P_literal): T_evaluationResult;
  VAR aSub:P_literal;
  begin
    result.reasonForStop:=rr_ok;
    result.literal:=nil;
    case a^.literalType of
      lt_bigint,lt_smallint,lt_real:
        result.literal:=P_literalRecycler(recycler)^.newRealLiteral(getSingleInterpolatedValue(P_numericLiteral(a)^.floatValue));
      lt_emptyList,lt_intList,lt_numList,lt_realList:
        begin
          result.literal:=P_literalRecycler(recycler)^.newListLiteral(P_listLiteral(a)^.size);
          for aSub in P_listLiteral(a)^.tempIteratableList do
            P_listLiteral(result.literal)^.appendReal(P_literalRecycler(recycler),getSingleInterpolatedValue(P_numericLiteral(aSub)^.floatValue));
        end;
      else result.reasonForStop:=rr_fail;
    end;
  end;

FUNCTION T_interpolator.evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: pointer; CONST parameters: P_listLiteral): T_evaluationResult;
  begin
    if (parameters^.size=1)
    then result:=evaluateToLiteral(location,context,recycler,parameters^.value[0],nil)
    else begin
      P_context(context)^.raiseCannotApplyError('interpolator '+getId,parameters,location);
      result.reasonForStop:=rr_patternMismatch;
      result.literal:=nil;
    end;
  end;

FUNCTION T_interpolator.arity: T_arityInfo;
  begin
    result.maxPatternLength:=1;
    result.minPatternLength:=1;
  end;

FUNCTION T_interpolator.canApplyToNumberOfParameters(CONST parCount: longint): boolean;
  begin
    result:=parCount=1;
  end;

FUNCTION T_interpolator.toString(CONST lengthLimit: longint): ansistring;
  begin
    result:=getId+'(';
    result+=underlyingValues^.toString(lengthLimit-length(result)-1)+')';
  end;

PROCEDURE T_interpolator.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    if underlyingValues<>nil then literalRecycler^.disposeLiteral(underlyingValues);
    underlyingValues:=nil;
  end;

FUNCTION T_interpolator.referencesAnyUserPackage: boolean;
  begin
    result:=false;
  end;

FUNCTION T_interpolator.writeToStream(CONST literalRecycler: P_literalRecycler; CONST locationOfSerializeCall: T_tokenLocation; CONST adapters: P_messages; CONST stream: P_outputStreamWrapper): boolean;
  begin
    stream^.logWrongTypeError;
    if adapters<>nil
    then adapters^.raiseSimpleError('Cannot serialize builtin generator expression.',locationOfSerializeCall)
    else raise Exception.create(    'Cannot serialize builtin generator expression.');
    result:=false;
  end;

TYPE
  P_linearInterpolator=^T_linearInterpolator;
  T_linearInterpolator=object(T_interpolator)
    protected
      value:T_arrayOfDouble;
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual;
    public
      CONSTRUCTOR create(CONST values:P_listLiteral; CONST location: T_tokenLocation);
      PROCEDURE cleanup(CONST literalRecycler: P_literalRecycler); virtual;
  end;

FUNCTION T_linearInterpolator.getSingleInterpolatedValue(CONST floatIdx: double): double;
  VAR i0:longint;
      rest:double;
  begin
    i0:=trunc(floatIdx);
    if (i0<0) then result:=value[0]
    else if (i0>=length(value)-1) then result:=value[length(value)-1]
    else begin
      rest:=floatIdx-i0;
      result:=value[i0]*(1-rest)+value[i0+1]*rest;
    end;
  end;

CONSTRUCTOR T_linearInterpolator.create(CONST values: P_listLiteral; CONST location: T_tokenLocation);
  VAR i:longint;
  begin
    inherited createInterpolator('linearInterpolator',values,location);
    assert(values^.literalType in [lt_numList,lt_realList,lt_intList]);
    setLength(value,values^.size);
    for i:=0 to values^.size-1 do value[i]:=P_numericLiteral(values^.value[i])^.floatValue;
  end;

PROCEDURE T_linearInterpolator.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    setLength(value,0);
    inherited;
  end;

{$i func_defines.inc}
FUNCTION linearInterpolator_imp intFuncSignature;
  begin
    result:=nil;
    if (params^.size=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList]) then begin
      new(P_linearInterpolator(result),create(list0,tokenLocation));
    end;
  end;

TYPE
  P_cSplineInterpolator=^T_cSplineInterpolator;
  T_cSplineInterpolator=object(T_interpolator)
    protected
      toInterpolate:T_arrayOfDouble;
      M:T_arrayOfDouble;

      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual;
    public
      CONSTRUCTOR create(CONST values:P_listLiteral; CONST location: T_tokenLocation);
      PROCEDURE cleanup(CONST literalRecycler: P_literalRecycler); virtual;
  end;

FUNCTION T_cSplineInterpolator.getSingleInterpolatedValue(CONST floatIdx: double): double;
  VAR i0:longint;
      cub0,
      cub1,
      off ,
      lin ,
      rest:double;
  begin
    i0:=trunc(floatIdx);
    if (i0<0) then i0:=0
    else if (i0>=length(M)-1) then i0:=length(M)-2;
    rest:=floatIdx-i0;
    cub0:=M[i0  ]*(1/6);
    cub1:=M[i0+1]*(1/6);
    off :=toInterpolate[i0]-M[i0]*(1/6);
    lin :=toInterpolate[i0+1]-toInterpolate[i0]-(M[i0+1]-M[i0])*(1/6);
    result:=off+(lin+cub1*sqr(  rest))*   rest+
                     cub0*sqr(1-rest) *(1-rest);
  end;

CONSTRUCTOR T_cSplineInterpolator.create(CONST values: P_listLiteral; CONST location: T_tokenLocation);
  VAR n,i:longint;
     C:T_arrayOfDouble;
  begin
    inherited createInterpolator('cSplineInterpolator',values,location);
    assert(values^.literalType in [lt_numList,lt_realList,lt_intList]);
    setLength(toInterpolate,values^.size);
    for i:=0 to values^.size-1 do toInterpolate[i]:=P_numericLiteral(values^.value[i])^.floatValue;
    if length(toInterpolate)=1 then append(toInterpolate,toInterpolate[0]);
    if length(toInterpolate)=2 then append(toInterpolate,toInterpolate[1]);
    n:=length(toInterpolate);
    setLength(M,n);
    setLength(C,n);
    dec(n);
    M[0]:=toInterpolate[0]*0.125;
    M[n]:=toInterpolate[n]*(-0.5);
    C[0]:=1/4;
    for i:=1 to n-1 do begin
      M[i]:=(toInterpolate[i-1]-toInterpolate[i]*2+toInterpolate[i+1])*6;
      C[i]:=1/(4-C[i-1]);
    end;
    M[0]:=M[0]*0.25;
    for i:=1 to n       do M[i]:=(M[i]-M[i-1])*C[i];
    for i:=n-1 downto 0 do M[i]:=M[i]-M[i+1]*C[i];
  end;

PROCEDURE T_cSplineInterpolator.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    setLength(M,0);
    setLength(toInterpolate,0);
    inherited;
  end;

FUNCTION cSplineInterpolator_imp intFuncSignature;
  begin
    result:=nil;
    if (params^.size=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList]) then begin
      new(P_cSplineInterpolator(result),create(list0,tokenLocation));
    end;
  end;

TYPE
  P_bSplineApproximator=^T_bSplineApproximator;
  T_bSplineApproximator=object(T_interpolator)
    protected
      toInterpolate:T_arrayOfDouble;
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual;
    public
      CONSTRUCTOR create(CONST values:P_listLiteral; CONST location: T_tokenLocation);
      PROCEDURE cleanup(CONST literalRecycler: P_literalRecycler); virtual;
  end;

FUNCTION T_bSplineApproximator.getSingleInterpolatedValue(CONST floatIdx: double): double;
  VAR i:longint;
      t,it:double;
  FUNCTION limitIdx(CONST j:longint):longint; inline;
    begin
      if j<0
      then result:=0
      else if j>=length(toInterpolate)
           then result:=length(toInterpolate)-1
           else result:=j;
    end;

  begin
    i:=floor(floatIdx); t:=floatIdx-i; it:=1-t;
    result:=(toInterpolate[limitIdx(i-1)]*(it*it*it)
            +toInterpolate[limitIdx(i  )]*((3*t*t*t)-(6*t*t)+4)
            +toInterpolate[limitIdx(i+1)]*((-3*t*t*t)+(3*t*t)+(3*t)+1)
            +toInterpolate[limitIdx(i+2)]*(t*t*t))/6;
  end;

CONSTRUCTOR T_bSplineApproximator.create(CONST values: P_listLiteral; CONST location: T_tokenLocation);
  VAR i:longint;
  begin
    inherited createInterpolator('bezierSpline',values,location);
    assert(values^.literalType in [lt_numList,lt_realList,lt_intList]);
    setLength(toInterpolate,values^.size);
    for i:=0 to values^.size-1 do toInterpolate[i]:=P_numericLiteral(values^.value[i])^.floatValue;
  end;

PROCEDURE T_bSplineApproximator.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    setLength(toInterpolate,0);
    inherited;
  end;

FUNCTION bSplineApproximator_imp intFuncSignature;
  begin
    result:=nil;
    if (params^.size=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList]) then begin
      new(P_bSplineApproximator(result),create(list0,tokenLocation));
    end;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newLinearInterpolator',@linearInterpolator_imp ,ak_unary{$ifdef fullVersion},'linearInterpolator(L:NumericList);//returns an linear interpolator, which returns values out of L by their index'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newSplineInterpolator',@cSplineInterpolator_imp,ak_unary{$ifdef fullVersion},'newSplineInterpolator(L:NumericList);//returns an C-Spline interpolator, which returns values out of L by their index'{$endif});
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newBezierSpline'      ,@bSplineApproximator_imp,ak_unary{$ifdef fullVersion},'newBezierSpline(L:NumericList);//returns an Bezier approximator'{$endif});

end.
