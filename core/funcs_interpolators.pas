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
  //TODO Make interpolators serializable
  //TODO Enable analytic integration for interpolators
  T_interpolator=object(T_builtinExpression)
    protected
      underlyingValues:P_listLiteral;
      accessByIndex:boolean;
      xValues,
      yValues:T_arrayOfDouble;
      FUNCTION getParameterNames(CONST literalRecycler:P_literalRecycler):P_listLiteral; virtual;
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

      FUNCTION referencesAnyUserPackage: boolean; virtual;
  end;

IMPLEMENTATION
USES funcs,tokens,sysutils,math,mnh_messages;
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

FUNCTION T_interpolator.findIndexForX(CONST x:double):longint;
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

CONSTRUCTOR T_interpolator.createInterpolator(CONST id_:string; CONST values: P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context; CONST allowUnordered:boolean=false);
  VAR i:longint;
      ok:boolean=true;
  begin
    inherited create(id_,et_builtinStateful,location);
    values^.rereference;
    underlyingValues:=values;

    if values^.literalType in [lt_numList,lt_realList,lt_intList] then begin
      accessByIndex:=true;
      setLength(xValues,0);
      setLength(yValues,values^.size);
      for i:=0 to values^.size-1 do yValues[i]:=P_numericLiteral(values^.value[i])^.floatValue;
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

DESTRUCTOR T_interpolator.destroy;
  begin
    assert(underlyingValues=nil);
    inherited;
  end;

FUNCTION T_interpolator.evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_literalRecycler; CONST parameters:P_listLiteral=nil):T_evaluationResult;
  VAR aSub: P_literal;
  begin
    result.reasonForStop:=rr_ok;
    if (parameters<>nil) and (parameters^.size=1) then case (parameters^.value[0]^.literalType) of
      lt_bigint,lt_smallint,lt_real:
        result.literal:=P_literalRecycler(recycler)^.newRealLiteral(getSingleInterpolatedValue(P_numericLiteral(parameters^.value[0])^.floatValue));
      lt_emptyList,lt_intList,lt_numList,lt_realList:
        begin
          result.literal:=P_literalRecycler(recycler)^.newListLiteral(P_listLiteral(parameters^.value[0])^.size);
          for aSub in P_listLiteral(parameters^.value[0])^.tempIteratableList do
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
    setLength(xValues,0);
    setLength(yValues,0);
    underlyingValues:=nil;
  end;

FUNCTION T_interpolator.referencesAnyUserPackage: boolean;
  begin
    result:=false;
  end;

TYPE
  P_linearInterpolator=^T_linearInterpolator;
  T_linearInterpolator=object(T_interpolator)
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
  T_cSplineInterpolator=object(T_interpolator)
    protected
      M:T_arrayOfDouble;
      FUNCTION getSingleInterpolatedValue(CONST floatIdx:double):double; virtual;
      FUNCTION getFourierCoefficients(CONST maxWaveNumber:longint; CONST literalRecycler: P_literalRecycler):P_listLiteral;
    public
      CONSTRUCTOR create(CONST values:P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
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
      off :=yValues[i0]-M[i0]*sqr(xValues[i0+1]-xValues[i0]);
      lin :=(yValues[i0+1]-yValues[i0])*ih0-(M[i0+1]-M[i0])*(xValues[i0+1]-xValues[i0]);
      result:=off+(lin+cub1*t0*t0)*t0+cub0*t1*t1*t1;
    end;
  end;

FUNCTION T_cSplineInterpolator.getFourierCoefficients(CONST maxWaveNumber:longint; CONST literalRecycler: P_literalRecycler):P_listLiteral;
  TYPE T_normalizedCoeff=record a0,a1,a2,a3:double; end;
       T_fourierIntegralBasis=record s0,s1,s2,s3,
                                     c0,c1,c2,c3:double; end;
  VAR ik:array[1..3] of double;
  FUNCTION calcFourierIntegralBasis(CONST x:double; CONST k:longint):T_fourierIntegralBasis;
    VAR
      kx, skx, ckx: double;
      kx1, kx2: double;
    begin
      kx:=k*x;
      skx:=sin(kx)*ik[1];
      ckx:=cos(kx)*ik[1];
      kx1:=sqr(kx)-2;
      kx2:=sqr(kx)-6;
      result.s0:=(-       ckx          )      ;
      result.s1:=(-kx    *ckx+      skx)*ik[1];
      result.s2:=(-   kx1*ckx+2*kx *skx)*ik[2];
      result.s3:=(-kx*kx2*ckx+3*kx1*skx)*ik[3];
      result.c0:=(        skx          )      ;
      result.c1:=( kx    *skx+      ckx)*ik[1];
      result.c2:=(    kx1*skx+2*kx *ckx)*ik[2];
      result.c3:=( kx*kx2*skx+3*kx1*ckx)*ik[3];
    end;

  FUNCTION sectionIntegral(CONST coeff:T_normalizedCoeff; CONST t0,t1:double):double;
    begin
      result:=coeff.a0   *(    t1      -    t0)
             +coeff.a1/2 *(sqr(t1)     -sqr(t0))
             +coeff.a2/3 *(sqr(t1)*t1  -sqr(t0)*t0)
             +coeff.a3/4 *(sqr(sqr(t1))-sqr(sqr(t0)));
    end;

  PROCEDURE sectionIntegral(CONST coeff:T_normalizedCoeff; CONST B0,B1:T_fourierIntegralBasis; VAR sinusTotal,cosinusTotal:double);
    begin
      sinusTotal  +=coeff.a0*(B1.s0-B0.s0)+
                    coeff.a1*(B1.s1-B0.s1)+
                    coeff.a2*(B1.s2-B0.s2)+
                    coeff.a3*(B1.s3-B0.s3);
      cosinusTotal+=coeff.a0*(B1.c0-B0.c0)+
                    coeff.a1*(B1.c1-B0.c1)+
                    coeff.a2*(B1.c2-B0.c2)+
                    coeff.a3*(B1.c3-B0.c3);
    end;

  FUNCTION calcNormalizedCoeff(CONST sectionIndex:longint):T_normalizedCoeff;
    VAR h, cub0, cub1, off, lin, x0, x1: double;
    begin
      //if (sectionIndex>=length(M)-1) then sectionIndex:=length(M)-2;
      x0:=xValues[sectionIndex];
      x1:=xValues[sectionIndex+1];
      h:=1/(x1-x0);
      cub0:=M[sectionIndex  ]*h;
      cub1:=M[sectionIndex+1]*h;
      off := yValues[sectionIndex]-M[sectionIndex]*sqr(x1-x0);
      lin :=(yValues[sectionIndex+1]-yValues[sectionIndex])*h-(M[sectionIndex+1]-M[sectionIndex])*(x1-x0);

      result.a0:= off-lin*x0-cub1*  x0*x0*x0+  cub0*x1*x1*x1;
      result.a1:=(    lin   +cub1*3*x0*x0   -3*cub0*x1*x1);
      result.a2:=(          -cub1*3*x0      +3*cub0*x1);
      result.a3:=(           cub1           -  cub0);
    end;

  VAR normalizedCoeff:array of T_normalizedCoeff;
      evaluationPoints:T_arrayOfDouble;
      k:longint;
      j:longint;
      i0,i1:longint;

      sinusTotal  :double=0;
      cosinusTotal:double=0;
      startBasis,endBasis:T_fourierIntegralBasis;
  begin
    if accessByIndex   then exit(nil);
    if maxWaveNumber<0 then exit(literalRecycler^.newListLiteral(0));

    //find first and last relevant section
    i0:=0;                 while (i0<length(xValues)-2) and (xValues[i0+1]<0 ) do inc(i0);
    i1:=length(xValues)-2; while (i1>0                ) and (xValues[i1]>2*pi) do dec(i1);
    if i0>i1 then exit(nil);

    //Prepare points in time
    setLength(evaluationPoints,i1-i0+2);
    for j:=0 to i1-i0+1 do evaluationPoints[j]:=xValues[i0+j];
    evaluationPoints[0                         ]:=0;
    evaluationPoints[length(evaluationPoints)-1]:=2*pi;

    //Prepare normal form coefficients
    setLength(normalizedCoeff,i1-i0+1);
    for j:=0 to i1-i0 do normalizedCoeff[j]:=calcNormalizedCoeff(i0+j);

    result:=literalRecycler^.newListLiteral;
    //Prepare 0th integral
    for j:=0 to length(normalizedCoeff)-1 do cosinusTotal+=sectionIntegral(normalizedCoeff[j],evaluationPoints[j],evaluationPoints[j+1]);

    result^.append(literalRecycler,literalRecycler^.newListLiteral^.appendReal(literalRecycler,cosinusTotal/(2*pi))^.appendReal(literalRecycler,sinusTotal),false);
    //prepare remaining integrals
    for k:=1 to maxWaveNumber do begin
      ik[1]:=1/k; ik[2]:=ik[1]*ik[1]; ik[3]:=ik[1]*ik[2];
      endBasis:=calcFourierIntegralBasis(evaluationPoints[0],k);
      sinusTotal  :=0;
      cosinusTotal:=0;

      for j:=0 to length(normalizedCoeff)-1 do begin
        startBasis:=endBasis;
        endBasis  :=calcFourierIntegralBasis(evaluationPoints[j+1],k);
        sectionIntegral(normalizedCoeff[j],startBasis,endBasis,sinusTotal,cosinusTotal);
      end;
      result^.append(literalRecycler,literalRecycler^.newListLiteral^.appendReal(literalRecycler,cosinusTotal/pi)^.appendReal(literalRecycler,sinusTotal/pi),false);
    end;

    //cleanup
    setLength(normalizedCoeff,0);
    setLength(evaluationPoints,0);
  end;

CONSTRUCTOR T_cSplineInterpolator.create(CONST values: P_listLiteral; CONST location: T_tokenLocation; CONST context:P_context);
  VAR n,i:longint;
      v:array of array[-1..1] of double;
      factor:double;
  begin
    inherited createInterpolator('cSplineInterpolator',values,location,context);
    if length(yValues)<3 then begin
      context^.raiseError('Spline interpolation requires at least 3 points!',location);
      exit;
    end;

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
  T_bSplineApproximator=object(T_interpolator)
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
  T_localInterpolator=object(T_interpolator)
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
    inherited createInterpolator('local',values,location,context);
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
  T_fourierSeries=object(T_interpolator)
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
    if includeYsin then for i:=1 to length(yValues)-1 do result+=yValues[i]*sin(floatIdx*i);
    if includeXcos then for i:=0 to length(xValues)-1 do result+=xValues[i]*cos(floatIdx*i);
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
  VAR spline:T_cSplineInterpolator;
      maxWaveNumber:longint=-1;
  FUNCTION calculateDFT:P_listLiteral;
    VAR k,i:longint;
        x:T_arrayOfDouble;
        iter:T_arrayOfLiteral;
        h:double;
        sinSum,cosSum:double;
    begin
      //Prepare input data
      iter:=list0^.tempIteratableList;
      setLength(x,length(iter));
      for k:=0 to length(iter)-1 do x[k]:=P_numericLiteral(iter[k])^.floatValue;
      setLength(iter,0);
      h:=2*pi/length(x);
      //calculate coefficients
      result:=recycler^.newListLiteral(maxWaveNumber+1);
      for k:=0 to maxWaveNumber do begin
        sinSum:=0;
        cosSum:=0;
        for i:=0 to length(x)-1 do begin
          sinSum+=x[i]*sin(h*i*k);
          cosSum+=x[i]*cos(h*i*k);
        end;
        if k=0
        then result^.append(recycler,recycler^.newListLiteral^.appendReal(recycler,  cosSum/length(x))^.appendReal(recycler,0),false)
        else result^.append(recycler,recycler^.newListLiteral^.appendReal(recycler,2*cosSum/length(x))^.appendReal(recycler,2*sinSum/length(x)),false);
      end;
      setLength(x,0);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType in [lt_numList,lt_realList,lt_intList,lt_list])
    then begin
      if (params^.size=2) and (arg1^.literalType=lt_smallint) and (int1^.intValue>=0) then maxWaveNumber:=int1^.intValue;
      if arg0^.literalType in [lt_numList,lt_realList,lt_intList]
      then begin
        if maxWaveNumber=-1 then maxWaveNumber:=list0^.size-list0^.size div 2;
        result:=calculateDFT;
      end
      else begin
        if maxWaveNumber=-1 then exit(nil);
        spline.create(list0,tokenLocation,context);
        result:=spline.getFourierCoefficients(maxWaveNumber,recycler);
        spline.cleanup(recycler);
        spline.destroy;
      end;
    end;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newLinearInterpolator',@linearInterpolator_imp ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newSplineInterpolator',@cSplineInterpolator_imp,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newBSpline'           ,@bSplineApproximator_imp,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newFourierSeries'     ,@fourierSeries_imp      ,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'calculateFourierCoefficients',@calcFourierCoeff_im,ak_unary);
  builtinFunctionMap.registerRule(MATH_NAMESPACE,'newLocalInterpolator' ,@localInterpolator_imp,ak_unary);

end.
