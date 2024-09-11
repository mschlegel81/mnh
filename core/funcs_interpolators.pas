UNIT funcs_interpolators;
INTERFACE
USES mnh_constants,
     myGenerics,
     basicTypes,
     out_adapters,
     recyclers,
     contexts,
     litVar,subrules;

TYPE
  T_1D_interpolator=object(T_builtinObject)
    protected
      //TODO: Enable cyclic/periodic interpolation
      underlyingValues:P_listLiteral;
      accessByIndex:boolean;
      xValues,
      yValues:T_arrayOfDouble;
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual; abstract;
      FUNCTION getEquivalentInlineExpression(CONST context:P_context; CONST recycler:P_recycler):P_inlineExpression; virtual;
      FUNCTION findIndexForX(CONST x:double):longint;
    public
      CONSTRUCTOR createInterpolator(CONST id_:string; CONST values:P_listLiteral; CONST location:T_tokenLocation; CONST context:P_context; CONST allowUnordered:boolean=false);
      DESTRUCTOR destroy; virtual;
      FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_literalRecycler; CONST parameters:P_listLiteral=nil):T_evaluationResult; virtual;
      FUNCTION arity:T_arityInfo; virtual;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      FUNCTION toString(CONST lengthLimit: longint=maxLongint): ansistring; virtual;
      PROCEDURE cleanup(CONST literalRecycler: P_literalRecycler); virtual;
      FUNCTION getParameterNames(CONST literalRecycler:P_literalRecycler):P_listLiteral; virtual;
  end;

IMPLEMENTATION
USES complex,funcs,tokens,sysutils,math,mnh_messages;
FUNCTION T_1D_interpolator.getParameterNames(CONST literalRecycler: P_literalRecycler): P_listLiteral;
  begin
    result:=P_listLiteral(literalRecycler^.newListLiteral^.appendString(literalRecycler,'i'));
  end;

FUNCTION T_1D_interpolator.getEquivalentInlineExpression(CONST context: P_context; CONST recycler: P_recycler): P_inlineExpression;
  VAR first:P_token;
  begin
    first:=P_recycler(recycler)^.newToken(getLocation,getId,tt_literal,rereferenced);
    first^.next:=getParametersForPseudoFuncPtr(1,false,getLocation,P_context(context),recycler);
    new(result,createFromInline(first,P_context(context),recycler));
  end;

FUNCTION T_1D_interpolator.findIndexForX(CONST x:double):longint;
  VAR i0,i1,ic:longint;
  begin
    if accessByIndex
    then result:=floor(x)
    else begin
      i0:=0;
      i1:=length(xValues)-1;
      while i0<>i1 do begin
        ic:=(i0+i1) shr 1;
        if ic=i0 then exit(ic);
        if   xValues[ic]<=x
        then i0:=ic
        else if xValues[ic]>x
        then i1:=ic
        else exit(i0);
      end;
      result:=i0;
    end;
  end;

CONSTRUCTOR T_1D_interpolator.createInterpolator(CONST id_:string; CONST values: P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context; CONST allowUnordered:boolean=false);
  VAR i:longint;
      ok:boolean=true;
  begin
    inherited create(id_,location);
    values^.rereference;
    underlyingValues:=values;

    if values^.literalType in [lt_numList,lt_realList,lt_intList] then begin
      accessByIndex:=true;
      setLength(xValues,0);
      yValues:=toVector(values,location,context);
    end else if values^.literalType=lt_list then begin
      accessByIndex:=false;
      setLength(xValues,values^.size);
      setLength(yValues,values^.size);
      for i:=0 to values^.size-1 do if ok then begin
        if (values^.value[i]^.literalType in [lt_numList,lt_realList,lt_intList])
        and (P_listLiteral(values^.value[i])^.size=2) then begin
          xValues[i]:=P_numericLiteral(P_listLiteral(values^.value[i])^.value[0])^.floatValue;
          yValues[i]:=P_numericLiteral(P_listLiteral(values^.value[i])^.value[1])^.floatValue;
        end else begin
          context^.raiseError('All list entries must be pairs of numbers; entry is: '+P_listLiteral(values^.value[i])^.typeString,location);
          ok:=false;
        end;
      end;
      if ok and not(allowUnordered) then begin
        for i:=1 to length(xValues)-1 do ok:=ok and (xValues[i]>xValues[i-1]);
        if not(ok) then context^.raiseError('x-Values for interpolator must be sorted.',location);
      end;
    end else context^.raiseError('Cannot create interpolator based on '+values^.typeString,location);
  end;

DESTRUCTOR T_1D_interpolator.destroy;
  begin
    assert(underlyingValues=nil);
    inherited;
  end;

FUNCTION T_1D_interpolator.evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_literalRecycler; CONST parameters:P_listLiteral=nil):T_evaluationResult;
  VAR aSub: P_literal;
  begin
    result.reasonForStop:=rr_ok;
    if (parameters<>nil) and (parameters^.size=1) then case (parameters^.value[0]^.literalType) of
      lt_bigint,lt_smallint,lt_real:
        result.literal:=P_literalRecycler(recycler)^.newRealLiteral(getSingleInterpolatedValue(P_numericLiteral(parameters^.value[0])^.floatValue));
      lt_emptyList,lt_intList,lt_numList,lt_realList:
        begin
          result.literal:=P_literalRecycler(recycler)^.newListLiteral(P_listLiteral(parameters^.value[0])^.size);
          for aSub in P_listLiteral(parameters^.value[0])^.tempIterableList do
            P_listLiteral(result.literal)^.appendReal(P_literalRecycler(recycler),getSingleInterpolatedValue(P_numericLiteral(aSub)^.floatValue));
        end;
      else begin P_context(context)^.raiseCannotApplyError('interpolator '+getId,parameters,location);
        result.reasonForStop:=rr_patternMismatch;
        result.literal:=nil;
      end;
    end else begin
      P_context(context)^.raiseCannotApplyError('interpolator '+getId,parameters,location);
      result.reasonForStop:=rr_patternMismatch;
      result.literal:=nil;
    end;
  end;

FUNCTION T_1D_interpolator.arity: T_arityInfo;
  begin
    result.maxPatternLength:=1;
    result.minPatternLength:=1;
  end;

FUNCTION T_1D_interpolator.canApplyToNumberOfParameters(CONST parCount: longint): boolean;
  begin
    result:=parCount=1;
  end;

FUNCTION T_1D_interpolator.toString(CONST lengthLimit: longint): ansistring;
  begin
    result:=getId+'(';
    result+=underlyingValues^.toString(lengthLimit-length(result)-1)+')';
  end;

PROCEDURE T_1D_interpolator.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    if underlyingValues<>nil then literalRecycler^.disposeLiteral(underlyingValues);
    setLength(xValues,0);
    setLength(yValues,0);
    underlyingValues:=nil;
  end;

TYPE
  P_linearInterpolator=^T_linearInterpolator;
  T_linearInterpolator=object(T_1D_interpolator)
    protected
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual;
    public
      CONSTRUCTOR create(CONST values:P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
      PROCEDURE cleanup(CONST literalRecycler: P_literalRecycler); virtual;
  end;

FUNCTION T_linearInterpolator.getSingleInterpolatedValue(CONST floatIdx: double): double;
  VAR i0:longint;
      rest:double;
  begin
    if accessByIndex then begin
      if floatIdx>maxLongint then i0:=length(yValues)-1
      else if floatIdx<0     then i0:=0
      else                        i0:=trunc(floatIdx);
      if (i0<0) then result:=yValues[0]
      else if (i0>=length(yValues)-1) then result:=yValues[length(yValues)-1]
      else begin
        rest:=floatIdx-i0;
        result:=yValues[i0]*(1-rest)+yValues[i0+1]*rest;
      end;
    end else begin
      i0:=findIndexForX(floatIdx);
      if (i0<0) then result:=yValues[0]
      else if (i0>=length(yValues)-1) then result:=yValues[length(yValues)-1]
      else result:=(floatIdx-xValues[i0])/
                   (xValues[i0+1]-xValues[i0])*
                   (yValues[i0+1]-yValues[i0])+yValues[i0];
    end;
  end;

CONSTRUCTOR T_linearInterpolator.create(CONST values: P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
  begin
    inherited createInterpolator('linearInterpolator',values,location,context);
    if length(yValues)<=1 then accessByIndex:=true;
    assert(values^.literalType in [lt_numList,lt_realList,lt_intList,lt_list]);
  end;

PROCEDURE T_linearInterpolator.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    inherited;
  end;

{$i func_defines.inc}
FUNCTION linearInterpolator_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList,lt_list]) then begin
      new(P_linearInterpolator(result),create(list0,tokenLocation,context));
      if not(context^.continueEvaluation) then recycler^.disposeLiteral(result);
    end;
  end;

TYPE
  P_cSplineInterpolator=^T_cSplineInterpolator;
  T_cSplineInterpolator=object(T_1D_interpolator)
    protected
      M:T_arrayOfDouble;
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual;
    private
      PROCEDURE calculateCoefficients;
    public
      CONSTRUCTOR create(CONST values:P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
      CONSTRUCTOR createForFourierAnalysis(CONST xValues_,yValues_:T_arrayOfDouble; CONST location: T_tokenLocation; CONST context:P_context);
      PROCEDURE cleanup(CONST literalRecycler: P_literalRecycler); virtual;
  end;

FUNCTION T_cSplineInterpolator.getSingleInterpolatedValue(CONST floatIdx: double): double;
  VAR i0:longint;
      cub0,
      cub1,
      off ,
      lin ,
      t0,t1:double;
      ih0:double;
  begin
    if accessByIndex then begin
      if floatIdx>maxLongint then i0:=length(M)-2
      else if floatIdx<0     then i0:=0
      else                        i0:=trunc(floatIdx);
      i0:=trunc(floatIdx);
      if (i0<0) then i0:=0
      else if (i0>=length(M)-1) then i0:=length(M)-2;
      t0:=floatIdx-i0;
      t1:=1-t0;
      cub0:=M[i0  ];
      cub1:=M[i0+1];
      off :=yValues[i0]-M[i0];
      lin :=yValues[i0+1]-yValues[i0]-(M[i0+1]-M[i0]);
      result:=off+(lin+cub1*t0*t0)*t0+cub0*t1*t1*t1;
    end else begin
      i0:=findIndexForX(floatIdx);
      if (i0>=length(M)-1) then i0:=length(M)-2;
      t0:=floatIdx-xValues[i0];
      t1:=xValues[i0+1]-floatIdx;
      ih0:=1/(xValues[i0+1]-xValues[i0]);

      cub0:=M[i0  ]*ih0;
      cub1:=M[i0+1]*ih0;
      off :=yValues[i0]-M[i0]*system.sqr(xValues[i0+1]-xValues[i0]);
      lin :=(yValues[i0+1]-yValues[i0])*ih0-(M[i0+1]-M[i0])*(xValues[i0+1]-xValues[i0]);
      result:=off+(lin+cub1*t0*t0)*t0+cub0*t1*t1*t1;
    end;
  end;

PROCEDURE T_cSplineInterpolator.calculateCoefficients;
  VAR n,i:longint;
      v:array of array[-1..1] of double;
      factor:double;
  begin
    n:=length(yValues);
    setLength(M,n);
    dec(n);
    //To solve:
    // With h[i]= x[i+1]-x[i]
    // |   1                                         |   |M0|   | 0                       |
    // | h0/6  (h0+h1)/3    h1/6                     |   |M1|   | (y2-y1)/h1 - (y1-y0)/h0 |
    // |          h1/6   (h1+h2)/3    h2/6           | * |M2| = | (y3-y2)/h2 - (y2-y1)/h1 |
    // |                    h2/6   (h2+h3)/3    h3/6 |   |M3|   | (y4-y3)/h3 - (y3-y2)/h2 |
    // |                                          1  |   |M4|   | 0                       |
    //---------------------------------------------------------------------------------------
    //Fill matrix:---------------------------------------------//
    // | v[0,0]=1                      | w[0]=0|               //
    // | v[1,-1] v[1, 0] v[1,1]        | w[1]  |               //
    // |         v[2,-1] v[2,0] v[2,1] | w[2]  |               //
    // |                            ...| ...   |               //
    //------------------------------------------               //
    setLength(v,n+1);                                          //
    v[0,-1]:=0; v[0,0]:=1; v[0,1]:=0; M[0]:=0;                 //
    v[n,-1]:=0; v[n,0]:=1; v[n,1]:=0; M[n]:=0;                 //
    if accessByIndex then begin                                //
      for i:=1 to n-1 do begin                                 //
        v[i,-1]:=1;                                            //
        v[i, 0]:=4;                                            //
        v[i, 1]:=1;                                            //
        M[i]   :=((yValues[i+1]-yValues[i  ])-                 //
                  (yValues[i  ]-yValues[i-1]))*6;              //
      end;                                                     //
    end else begin                                             //
      for i:=1 to n-1 do begin                                 //
        v[i,-1]:=(xValues[i  ]-xValues[i-1]);                  //
        v[i, 0]:=(xValues[i+1]-xValues[i-1])*2;                //
        v[i, 1]:=(xValues[i+1]-xValues[i  ]);                  //
        M[i]   :=((yValues[i+1]-yValues[i  ])/                 //
                  (xValues[i+1]-xValues[i  ])-                 //
                  (yValues[i  ]-yValues[i-1])/                 //
                  (xValues[i  ]-xValues[i-1]))*6;              //
      end;                                                     //
    end;                                                       //
    //-----------------------------------------------:Fill matrix
    //Eliminiate subdiagonal:------------//
    // | 1                  | w'[0]=0|   //
    // | 0 1 v[1,1]         | w'[1]  |   //
    // |   0 1       v[2,1] | w'[2]  |   //
    // |                 ...| ...    |   //
    //--------------------------------   //
    M[0]/=v[0,0];                        //
    v[0,0]:=1;                           //
    for i:=1 to n-1 do begin             //
      factor:=v[i,-1];                   //
      v[i,-1]:=0;                        //
      v[i, 0]-=factor*v[i-1,1];          //
      M[i]   -=factor*M[i-1];            //
      factor:=1/v[i,0];                  //
      v[i,0]:=1;                         //
      v[i,1]*=factor;                    //
      M[i]  *=factor;                    //
    end;                                 //
    //--------------:Eliminiate subdiagonal
    for i:=n-1 downto 0 do M[i]:=M[i]-M[i+1]*v[i,1];
    for i:=0 to n do M[i]*=1/6;
  end;

CONSTRUCTOR T_cSplineInterpolator.create(CONST values: P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
  begin
    inherited createInterpolator('cSplineInterpolator',values,location,context);
    if length(yValues)<3 then begin
      context^.raiseError('Spline interpolation requires at least 3 points!',location);
      exit;
    end;
    calculateCoefficients;
  end;

CONSTRUCTOR T_cSplineInterpolator.createForFourierAnalysis(CONST xValues_,yValues_:T_arrayOfDouble; CONST location: T_tokenLocation; CONST context:P_context);
  CONST x_shifts:array[0..2] of double=(-2*pi,0,2*pi);
        twoPi=2*pi;
        iTwoPi=1/(2*pi);
  VAR i:longint;
      j:longint=0;
      x_shift:double;

      pairs:T_arrayOfComplex;
      tmp:T_Complex;
  begin

    setLength(pairs,length(xValues_));
    //Project all samples into interval [0, 2*pi) - potentially requires reordering
    for i:=0 to length(xValues_)-1 do begin
      pairs[i].re:=xValues_[i];
      pairs[i].im:=yValues_[i];
      if (pairs[i].re<0) or (pairs[i].re>=2*pi) then begin
        x_shift:=pairs[i].re*iTwoPi;
        pairs[i].re:=twoPi*(x_shift-floor(x_shift));
      end;
      for j:=0 to i-1 do if pairs[i].re<pairs[j].re then begin
        tmp:=pairs[i]; pairs[i]:=pairs[j]; pairs[j]:=tmp;
      end;
    end;
    //filter out samples which are "too close"
    j:=0;
    for i:=0 to length(pairs)-1 do begin
      if (j>0) and (pairs[i].re-pairs[j-1].re<1E-6)
      then pairs[j-1]:=(pairs[j-1]+pairs[i])*0.5
      else begin
        pairs[j]:=pairs[i];
        inc(j);
      end;
    end;
    setLength(pairs,j);

    j:=0;
    setLength(xValues,length(pairs)*3);
    setLength(yValues,length(pairs)*3);
    for x_shift in x_shifts do
    for i:=0 to length(pairs)-1 do if (pairs[i].re>=0) and (pairs[i].im<2*pi) then begin
      xValues[j]:=pairs[i].re+x_shift;
      yValues[j]:=pairs[i].im;
      inc(j);
    end;
    setLength(xValues,j);
    setLength(yValues,j);
    calculateCoefficients;
  end;

PROCEDURE T_cSplineInterpolator.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    setLength(M,0);
    inherited;
  end;

FUNCTION cSplineInterpolator_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList,lt_list]) then begin
      new(P_cSplineInterpolator(result),create(list0,tokenLocation,context));
      if not(context^.continueEvaluation) then recycler^.disposeLiteral(result);
    end;
  end;

TYPE
  P_bSplineApproximator=^T_bSplineApproximator;
  T_bSplineApproximator=object(T_1D_interpolator)
    protected
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual;
    public
      CONSTRUCTOR create(CONST values:P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
  end;

FUNCTION T_bSplineApproximator.getSingleInterpolatedValue(CONST floatIdx: double): double;
  VAR i,k:longint;
      t,it:double;
  FUNCTION limitIdx(CONST j:longint):longint; inline;
    begin
      if j<0
      then result:=0
      else if j>=length(yValues)
           then result:=length(yValues)-1
           else result:=j;
    end;
  VAR X:array[-2..3] of double;
      w0,w1:array[0..3] of double;
  begin
    if accessByIndex then begin
      if (floatIdx>maxLongint) or (floatIdx<1-maxLongint) then exit(Nan);

      i:=floor(floatIdx); t:=floatIdx-i; it:=1-t;
      result:=(yValues[limitIdx(i-1)]*(it*it*it)
              +yValues[limitIdx(i  )]*((3*t*t*t)-(6*t*t)+4)
              +yValues[limitIdx(i+1)]*((-3*t*t*t)+(3*t*t)+(3*t)+1)
              +yValues[limitIdx(i+2)]*(t*t*t))/6;
    end else begin
      i:=findIndexForX(floatIdx);
      for k:=-2 to 3 do begin
        if i+k<0
        then X[k]:=xValues[0]+(i+k)*(xValues[1]-xValues[0])
        else if i+k>=length(xValues)
        then X[k]:=xValues[length(xValues)-1]+(i+k)*(xValues[length(xValues)-1]-xValues[length(xValues)-2])
        else X[k]:=xValues[i+k];
      end;
                                       it:=(floatIdx-X[  0])/(X[  1]-X[  0]); w0[0]:= 1-it;        w0[  1]:=it;
      w1[0]:=0; for k:=0 to 1 do begin it:=(floatIdx-X[k-1])/(X[k+1]-X[k-1]); w1[k]+=(1-it)*w0[k]; w1[k+1]:=it*w0[k]; end;
      w0[0]:=0; for k:=0 to 2 do begin it:=(floatIdx-X[k-2])/(X[k+1]-X[k-2]); w0[k]+=(1-it)*w1[k]; w0[k+1]:=it*w1[k]; end;
      result:=yValues[limitIdx(i-1)]*w0[0]
             +yValues[limitIdx(i  )]*w0[1]
             +yValues[limitIdx(i+1)]*w0[2]
             +yValues[limitIdx(i+2)]*w0[3];
    end;
  end;

CONSTRUCTOR T_bSplineApproximator.create(CONST values: P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
  begin
    inherited createInterpolator('bSpline',values,location,context);
    if length(yValues)<2 then context^.raiseError('BSpline requires at least 2 points.',location);
  end;

FUNCTION bSplineApproximator_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList,lt_list]) then begin
      new(P_bSplineApproximator(result),create(list0,tokenLocation,context));
      if not(context^.continueEvaluation) then recycler^.disposeLiteral(result);
    end;
  end;

TYPE
  P_localInterpolator=^T_localInterpolator;
  T_localInterpolator=object(T_1D_interpolator)
    protected
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual;
    public
      CONSTRUCTOR create(CONST values:P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
  end;

FUNCTION T_localInterpolator.getSingleInterpolatedValue(CONST floatIdx: double): double;
  VAR i,k:longint;
      t:double;
      x,y:array[-1..2] of double;
      a,b,c,d,
      s0,s1       :double;
  begin
    if accessByIndex then begin
      if (floatIdx>maxLongint) or (floatIdx<1-maxLongint) then exit(Nan);
      i:=floor(floatIdx); t:=floatIdx-i;
      for k:=-1 to 2 do begin
        if      i+k<0                then y[k]:=yValues[                0]+(i+k)                  *(yValues[                1]-yValues[                0])
        else if i+k>=length(yValues) then y[k]:=yValues[length(yValues)-1]+(i+k-length(yValues)+1)*(yValues[length(yValues)-1]-yValues[length(yValues)-2])
                                     else y[k]:=yValues[i+k];

      end;
      result:= y[-1]*(t*(-0.5+(1-t*0.5)*t))
              +y[ 0]*(1+t*t*(-2.5+(3*t)*0.5))
              +y[+1]*(t*(0.5+(2-(3*t)*0.5)*t))
              +y[+2]*((-0.5+t*0.5)*t*t);
    end else begin
      i:=findIndexForX(floatIdx);
      if (i>=1) and (i<length(yValues)-2) then begin
        for k:=-1 to 2 do begin y[k]:=yValues[i+k]; x[k]:=xValues[i+k]; end;
        s0:=(y[1]-y[-1])/(x[1]-x[-1])*(x[1]-x[0]);
        s1:=(y[2]-y[ 0])/(x[2]-x[ 0])*(x[1]-x[0]);
      end else if i=0 then begin
        for k:=0 to 2 do begin y[k]:=yValues[i+k]; x[k]:=xValues[i+k]; end;
        s0:=(y[1]-y[0]);
        s1:=(y[2]-y[0])/(x[2]-x[0]) *(x[1]-x[0]);
      end else if i=length(yValues)-2 then begin
        for k:=-1 to 1 do begin y[k]:=yValues[i+k]; x[k]:=xValues[i+k]; end;
        s0:=(y[1]-y[-1])/(x[1]-x[-1])*(x[1]-x[0]);
        s1:=(y[1]-y[0]);
      end;
      t:=(floatIdx-x[0])/(x[1]-x[0]);
      if      t<0 then result:=y[0]+ t   *s0
      else if t>1 then result:=y[1]+(t-1)*s1
      else begin
        a:=y[0];
        b:=s0;
        c:=y[1]-a-b;
        d:=s1-b-2*c;
        c-=d;
        result:=a+t*(b+t*(c+t*d));
      end;
    end;
  end;

CONSTRUCTOR T_localInterpolator.create(CONST values: P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
  begin
    inherited createInterpolator('localInterpolator',values,location,context);
    if length(yValues)<2 then context^.raiseError('localInterpolator requires at least 2 points.',location);
  end;

FUNCTION localInterpolator_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList,lt_list]) then begin
      new(P_localInterpolator(result),create(list0,tokenLocation,context));
      if not(context^.continueEvaluation) then recycler^.disposeLiteral(result);
    end;
  end;

TYPE
  P_fourierSeries=^T_fourierSeries;
  T_fourierSeries=object(T_1D_interpolator)
    protected
      includeXcos,
      includeYsin:boolean;
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual;
    public
      CONSTRUCTOR create(CONST values:P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
  end;

FUNCTION T_fourierSeries.getSingleInterpolatedValue(CONST floatIdx: double): double;
  VAR i:longint;
  begin
    result:=0;
    if includeYsin then for i:=1 to length(yValues)-1 do if yValues[i]<>0 then result+=yValues[i]*system.sin(floatIdx*i);
    if includeXcos then for i:=0 to length(xValues)-1 do if xValues[i]<>0 then result+=xValues[i]*system.cos(floatIdx*i);
  end;

CONSTRUCTOR T_fourierSeries.create(CONST values: P_listLiteral; CONST location: T_tokenLocation; CONST context: P_context);
  VAR i:longint;
  begin
    inherited createInterpolator('fourierSeries',values,location,context,true);
    includeYsin:=false;
    includeXcos:=false;
    for i:=1 to length(yValues)-1 do includeYsin:=includeYsin or (yValues[i]<>0);
    for i:=0 to length(xValues)-1 do includeXcos:=includeXcos or (xValues[i]<>0);
  end;

FUNCTION fourierSeries_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList,lt_list]) then begin
      new(P_fourierSeries(result),create(list0,tokenLocation,context));
      if not(context^.continueEvaluation) then recycler^.disposeLiteral(result);
    end;
  end;

FUNCTION calcFourierCoeff_im intFuncSignature;
  VAR maxWaveNumber:longint=-1;
      values_to_sample,fft_output:T_arrayOfComplex;
      k:longint;
  FUNCTION makeSamplesFromNonequidistant:boolean;
    FUNCTION hasLargePrimeFactors(n:longint):boolean;
      begin
        while n mod 2=0 do n:=n div 2;
        while n mod 3=0 do n:=n div 3;
        while n mod 5=0 do n:=n div 5;
        result:=n>1;
      end;

    VAR allX,allY:T_arrayOfDouble;
        spline:T_cSplineInterpolator;
        k,sampleCount:longint;
        tuple: P_literal;
    begin
      setLength(allX,list0^.size);
      setLength(allY,list0^.size);
      for k:=0 to list0^.size-1 do begin
        tuple:=list0^.value[k];
        if (tuple^.literalType in [lt_numList,lt_realList,lt_intList]) and (P_listLiteral(tuple)^.size=2) then begin
          allX[k]:=P_numericLiteral(P_listLiteral(tuple)^.value[0])^.floatValue;
          allY[k]:=P_numericLiteral(P_listLiteral(tuple)^.value[1])^.floatValue;
        end else exit(false);
      end;
      spline.createForFourierAnalysis(allX,allY,tokenLocation,context);
      if not context^.continueEvaluation then exit(false);
      //In the case of equidistant points, all in range 0..2pi, this reverts to plain FFT
      sampleCount:=list0^.size;
      if sampleCount>4*maxWaveNumber then begin
        sampleCount:=4*maxWaveNumber;
        while hasLargePrimeFactors(sampleCount) do inc(sampleCount);
      end;
      setLength(values_to_sample,sampleCount);
      for k:=0 to sampleCount-1 do values_to_sample[k]:=spline.getSingleInterpolatedValue(2*pi*k/sampleCount);
      spline.destroy;
      result:=true;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList,lt_list])
    then begin
      if (params^.size=2) and (arg1^.literalType=lt_smallint) and (int1^.intValue>=0) then maxWaveNumber:=int1^.intValue;
      if arg0^.literalType in [lt_numList,lt_realList,lt_intList]
      then begin
        if maxWaveNumber=-1 then maxWaveNumber:=list0^.size-list0^.size div 2;
        setLength(values_to_sample,list0^.size);
        for k:=0 to list0^.size-1 do values_to_sample[k]:=P_numericLiteral(list0^.value[k])^.floatValue;
      end else begin
        if (maxWaveNumber=-1) or not makeSamplesFromNonequidistant then exit(nil);
      end;
      fft_output:=FastFourierTransform(values_to_sample,false);
      result:=recycler^.newListLiteral(maxWaveNumber+1);

      for k:=0 to maxWaveNumber do begin
        listResult^.append(recycler,
                           recycler^.newListLiteral(2)
                                   ^.appendReal(recycler,fft_output[k].re* 2/length(fft_output))
                                   ^.appendReal(recycler,fft_output[k].im*-2/length(fft_output)),
                           false);
      end;
      setLength(values_to_sample,0);
      setLength(fft_output      ,0);
    end;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newLinearInterpolator',@linearInterpolator_imp ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newSplineInterpolator',@cSplineInterpolator_imp,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newBSpline'           ,@bSplineApproximator_imp,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newFourierSeries'     ,@fourierSeries_imp      ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'calculateFourierCoefficients',@calcFourierCoeff_im,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newLocalInterpolator' ,@localInterpolator_imp,ak_unary);
  registerValidStringType('LinearInterpolator');
  registerValidStringType('CSplineInterpolator');
  registerValidStringType('BSpline');
  registerValidStringType('LocalInterpolator');
  registerValidStringType('FourierSeries');
end.
