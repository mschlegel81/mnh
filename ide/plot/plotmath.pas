UNIT plotMath;
INTERFACE
USES sysutils,
     Graphics,
     myGenerics,
     myStringUtil,
     basicTypes,
     plotstyles,
     litVar,
     BGRACanvas, BGRABitmapTypes, BGRADefaultBitmap,
     types;
CONST
  MIN_VALUE_FOR:array[false..true] of double=(-1E100, 1E-100);
  MAX_VALUE_FOR:array[false..true] of double=( 1E100,  1E100);

TYPE
  T_point = array[0..1] of double;
  P_point = ^T_point;
  FUNCTION pointOf(CONST x,y:double):T_point; inline;
TYPE
  T_dataRow = object
    private
      dat:P_point;
      alloc:longint;
      FUNCTION getPoint(CONST index:longint):T_point;               inline;
      PROCEDURE setPoint(CONST index:longint; CONST value:T_point); inline;
      PROCEDURE setSize(CONST newSize:longint);
    public
      PROCEDURE init(CONST initialSize:longint=0);
      PROCEDURE free;
      PROPERTY point[index:longint]:T_point read getPoint write setPoint; default;
      PROPERTY size:longint read alloc write setSize;
      PROCEDURE cloneTo(OUT other:T_dataRow);
      FUNCTION toNewLiteralForExport(CONST literalRecycler:P_literalRecycler):P_listLiteral;
  end;

  T_rowToPaintArr = record
    lineStrips :array of array of TPoint;
    polygons   :array of array of TPoint;
  end;

  T_boundingBox = array['x'..'y', 0..1] of double;

  T_ticInfo = record
    pos  : double;
    major: boolean;
    txt  : ansistring;
  end;

  T_ticInfos=array ['x'..'y'] of array of T_ticInfo;
  T_axisTrafo=object
    private
      logs:boolean;
      rangeByAutosize,rangeByUser:array[0..1] of double;
      screenRange:array[0..1] of longint;

      factor,offset:double;
      PROCEDURE prepare;
      PROCEDURE setLogScale(CONST value:boolean);
      PROCEDURE setWorldMin(CONST value:double);
      PROCEDURE setWorldMax(CONST value:double);
      PROCEDURE setWorldRange(CONST x0,x1:double);
      PROCEDURE setScreenMin(CONST value:longint);
      PROCEDURE setScreenMax(CONST value:longint);
      FUNCTION isValidMinimum(CONST x:double):boolean;
      FUNCTION isValidMaximum(CONST x:double):boolean;
    public
      autoscale:boolean;
      PROCEDURE reset;

      PROPERTY logscale:boolean read logs write setLogScale;
      PROPERTY worldMin:double read rangeByUser[0] write setWorldMin;
      PROPERTY worldMax:double read rangeByUser[1] write setWorldMax;
      PROPERTY screenMin:longint read screenRange[0] write setScreenMin;
      PROPERTY screenMax:longint read screenRange[1] write setScreenMax;
      FUNCTION screenExtend:longint;
      FUNCTION apply       (CONST x:double):double;
      FUNCTION applyInverse(CONST x:double):double;

      PROCEDURE zoom(CONST screenCoordinate:longint; CONST zoomFactor:double);
      PROCEDURE pan(CONST screenDelta:longint);
      FUNCTION sampleIsInRange(CONST x:double):boolean;
      FUNCTION equals(CONST other:T_axisTrafo):boolean;
  end;

  P_plotPrimitive=^T_plotPrimitive;
  T_plotPrimitiveList=array of P_plotPrimitive;

  T_scalingOptionElement=(soe_x0,soe_x1,soe_y0,soe_y1,soe_fontsize,soe_autoscaleFactor,soe_preserveAspect,soe_autoscaleX,soe_autoscaleY,soe_logscaleX,soe_logscaleY,soe_axisStyleX,soe_axisStyleY);
  T_scalingOptionElements=set of T_scalingOptionElement;
  T_scalingOptions=object
    preserveAspect  : boolean;
    relativeFontSize: double;
    autoscaleFactor : double;
    axisTrafo:array['x'..'y'] of T_axisTrafo;
    axisStyle:array['x'..'y'] of T_gridStyle;

    PROCEDURE setDefaults;
    PROCEDURE updateForPlot(CONST Canvas:TBGRACanvas; CONST primitives:T_plotPrimitiveList; VAR grid:T_ticInfos);
    FUNCTION transformRow(CONST row:T_dataRow; CONST yBaseLine:longint; CONST style:T_style):T_rowToPaintArr;
    FUNCTION getRefinementSteps(CONST row:T_dataRow; CONST samplesToDistribute:longint):T_arrayOfLongint;
    FUNCTION screenToReal(CONST x,y:integer):T_point;
    FUNCTION realToScreen(CONST p:T_point):T_point;
    FUNCTION absoluteFontSize(CONST xRes,yRes:longint):longint;
    FUNCTION getOptionString:string;
    FUNCTION getOptionDiffString(CONST before:T_scalingOptions):string;
    PROCEDURE modifyOptions(CONST o:T_scalingOptions; CONST modified:T_scalingOptionElements);
    FUNCTION equals(CONST other:T_scalingOptions):boolean;
  end;

  T_plotPrimitive=object
    pseudoIndex:longint;
    PROCEDURE updateBoundingBox(CONST scaling:T_scalingOptions; VAR boundingBox:T_boundingBox); virtual; abstract;
    PROCEDURE render(CONST opt:T_scalingOptions; CONST screenBox:T_boundingBox;  CONST yBaseLine:longint; CONST target:TBGRACanvas); virtual; abstract;
    FUNCTION toStatementForExport(CONST firstRow:boolean; CONST literalRecycler:P_literalRecycler; VAR globalRowData:T_listLiteral):string; virtual; abstract;
    FUNCTION getFullClone:P_plotPrimitive; virtual; abstract;
    DESTRUCTOR destroy; virtual; abstract;
  end;

  P_sampleRow=^T_sampleRow;
  T_sampleRow = object(T_plotPrimitive)
    style: T_style;
    sample: T_dataRow;
    CONSTRUCTOR create(CONST row:T_dataRow);

    PROCEDURE updateBoundingBox(CONST scaling:T_scalingOptions; VAR boundingBox:T_boundingBox); virtual;
    PROCEDURE render(CONST opt:T_scalingOptions; CONST screenBox:T_boundingBox; CONST yBaseLine:longint; CONST target:TBGRACanvas); virtual;
    FUNCTION toStatementForExport(CONST firstRow:boolean; CONST literalRecycler:P_literalRecycler; VAR globalRowData:T_listLiteral):string; virtual;
    FUNCTION getFullClone:P_plotPrimitive; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  T_customTextAnchor=(cta_topLeft   ,cta_top   ,cta_topRight   ,
                      cta_centerLeft,cta_center,cta_centerRight,
                      cta_bottomLeft,cta_bottom,cta_bottomRight);

  P_customText=^T_customText;
  T_customText=object(T_plotPrimitive)
    text      :T_arrayOfString;
    fontName  :string;
    fontSize  :double;
    p         :T_point;
    anchor    :T_customTextAnchor;
    foreground:T_color;
    background:T_color;
    transparentBackground:boolean;
    absolutePosition:boolean;

    CONSTRUCTOR create(CONST x,y:double; CONST txt:T_arrayOfString);
    PROCEDURE setAnchor(CONST s:string);
    PROCEDURE setForeground(CONST r,g,b:double; CONST alpha:double=1);
    PROCEDURE setBackground(CONST r,g,b:double; CONST alpha:double=1);

    PROCEDURE updateBoundingBox(CONST scaling:T_scalingOptions; VAR boundingBox:T_boundingBox); virtual;
    PROCEDURE render(CONST opt:T_scalingOptions; CONST screenBox:T_boundingBox;  CONST yBaseLine:longint; CONST target:TBGRACanvas); virtual;
    FUNCTION toStatementForExport(CONST firstRow:boolean; CONST literalRecycler:P_literalRecycler; VAR globalRowData:T_listLiteral):string; virtual;
    FUNCTION getFullClone:P_plotPrimitive; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_rasterImage=^T_rasterImage;
  T_rasterImage= object(T_plotPrimitive)
    width:longint;
    scale,offsetX,offsetY:double;
    sourceMap: TBGRADefaultBitmap;
    CONSTRUCTOR create(CONST scale_,offsetX_,offsetY_:double);
    PROCEDURE updateBoundingBox(CONST scaling:T_scalingOptions; VAR boundingBox:T_boundingBox); virtual;
    PROCEDURE render(CONST opt:T_scalingOptions; CONST screenBox:T_boundingBox; CONST yBaseLine:longint; CONST target:TBGRACanvas); virtual;
    FUNCTION toStatementForExport(CONST firstRow:boolean; CONST literalRecycler:P_literalRecycler; VAR globalRowData:T_listLiteral):string; virtual;
    FUNCTION getFullClone:P_plotPrimitive; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  OPERATOR *(CONST x:T_point; CONST y:double):T_point;
  OPERATOR +(CONST x,y:T_point):T_point;
  OPERATOR -(CONST x,y:T_point):T_point;

  FUNCTION boundingBoxOf(CONST x0,y0,x1,y1:double):T_boundingBox;

VAR globalTextRenderingCs:TRTLCriticalSection;
IMPLEMENTATION
USES math;
FUNCTION intersect(CONST b1,b2:T_boundingBox):boolean;
  begin
    result:=(max(b1['x',0],b2['x',0])<=min(b1['x',1],b2['x',1])) and
            (max(b1['y',0],b2['y',0])<=min(b1['y',1],b2['y',1]));
  end;

FUNCTION boundingBoxOf(CONST x0,y0,x1,y1:double):T_boundingBox;
  begin
    result['x',0]:=min(x0,x1);
    result['x',1]:=max(x0,x1);
    result['y',0]:=min(y0,y1);
    result['y',1]:=max(y0,y1);
  end;

TYPE P_lineBuilder=^T_lineBuilder;

     { T_lineBuilder }

     T_lineBuilder=object
       row:T_rowToPaintArr;
       rowFill:longint;
       buffer:array of TPoint;
       bufferFill:longint;
       CONSTRUCTOR createLineBuilder;
       DESTRUCTOR destroy; virtual;
       FUNCTION get:T_rowToPaintArr; virtual;
       PROCEDURE add(CONST nextPoint:T_point); virtual;
       FUNCTION skip:boolean; virtual;
       FUNCTION newInstance:P_lineBuilder; virtual;
     end;

     P_bSplineBuilder=^T_bSplineBuilder;

     { T_bSplineBuilder }

     T_bSplineBuilder=object(T_lineBuilder)
       toApproximate:array of T_point;
       approxFill   :longint;

       PROCEDURE flush;
       CONSTRUCTOR createBSplineBuilder;
       DESTRUCTOR destroy; virtual;
       FUNCTION get:T_rowToPaintArr; virtual;
       PROCEDURE add(CONST nextPoint:T_point); virtual;
       FUNCTION skip:boolean; virtual;
       FUNCTION newInstance:P_lineBuilder; virtual;
     end;

     P_cSplineBuilder=^T_cSplineBuilder;

     { T_cSplineBuilder }

     T_cSplineBuilder=object(T_lineBuilder)
       toInterpolate:array of T_point;
       interpFill:longint;

       PROCEDURE flush;
       CONSTRUCTOR createCSplineBuilder;
       DESTRUCTOR destroy; virtual;
       FUNCTION get:T_rowToPaintArr; virtual;
       PROCEDURE add(CONST nextPoint:T_point); virtual;
       FUNCTION skip:boolean; virtual;
       FUNCTION newInstance:P_lineBuilder; virtual;
     end;

     P_tubeBuilder=^T_tubeBuilder;

     { T_tubeBuilder }

     T_tubeBuilder=object(T_lineBuilder)
       builders:array [0..1] of P_lineBuilder;
       tgt:byte;

       CONSTRUCTOR createTubeBuilder(CONST baseBuilder:P_lineBuilder);
       DESTRUCTOR destroy; virtual;
       FUNCTION get:T_rowToPaintArr; virtual;
       PROCEDURE add(CONST nextPoint:T_point); virtual;
       FUNCTION skip:boolean; virtual;
       FUNCTION newInstance:P_lineBuilder; virtual;
     end;

FUNCTION pointOf(CONST x,y:double):T_point;
  begin
    result[0]:=x;
    result[1]:=y;
  end;

OPERATOR *(CONST x:T_point; CONST y:double):T_point;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
  end;

OPERATOR +(CONST x,y:T_point):T_point;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
  end;

OPERATOR -(CONST x,y:T_point):T_point;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
  end;

{ T_tubeBuilder }

CONSTRUCTOR T_tubeBuilder.createTubeBuilder(CONST baseBuilder: P_lineBuilder);
  begin
    builders[0]:=baseBuilder;
    builders[1]:=baseBuilder^.newInstance;
    tgt:=0;
  end;

DESTRUCTOR T_tubeBuilder.destroy;
  begin
    dispose(builders[0],destroy);
    dispose(builders[1],destroy);
    inherited;
  end;

FUNCTION T_tubeBuilder.get: T_rowToPaintArr;
  begin
    skip;
    setLength(row.lineStrips,rowFill);
    result:=row;
  end;

PROCEDURE T_tubeBuilder.add(CONST nextPoint: T_point);
  begin
    builders[tgt]^.add(nextPoint);
    tgt:=1-tgt;
  end;

FUNCTION T_tubeBuilder.skip:boolean;
  VAR i,k:longint;
      j:longint=0;
  begin
    if builders[0]^.skip and builders[1]^.skip then begin

      if rowFill+1>=length(row.lineStrips) then setLength(row.lineStrips,round(length(row.lineStrips)*1.2)+2);

      k:=length(row.polygons);
      setLength(row.polygons,k+1);
      setLength(row.polygons[k],length(builders[0]^.row.lineStrips[0])+
                                length(builders[1]^.row.lineStrips[0]));

      setLength(row.lineStrips[rowFill],length(builders[0]^.row.lineStrips[0]));
      for i:=0 to length(row.lineStrips[rowFill])-1 do begin
        row.lineStrips[rowFill][i]:=builders[0]^.row.lineStrips[0][i];
        row.polygons[k][j]:=builders[0]^.row.lineStrips[0][i];
        inc(j);
      end;
      inc(rowFill);

      setLength(row.lineStrips[rowFill],length(builders[1]^.row.lineStrips[0]));
      for i:=0 to length(row.lineStrips[rowFill])-1 do row.lineStrips[rowFill][i]:=builders[1]^.row.lineStrips[0][i];
      for i:=length(row.lineStrips[rowFill])-1 downto 0 do begin
        row.polygons[k][j]:=builders[1]^.row.lineStrips[0][i];
        inc(j);
      end;
      inc(rowFill);

      result:=true;
    end else result:=false;
    builders[0]^.rowFill:=0; builders[0]^.bufferFill:=0;
    builders[1]^.rowFill:=0; builders[1]^.bufferFill:=0;

    tgt:=1-tgt;
  end;

FUNCTION T_tubeBuilder.newInstance: P_lineBuilder;
  begin
    new(P_tubeBuilder(result),createTubeBuilder(builders[0]^.newInstance));
  end;

CONSTRUCTOR T_rasterImage.create(CONST scale_,offsetX_,offsetY_:double);
  begin
    width:=1;
    scale:=scale_;
    offsetX:=offsetX_;
    offsetY:=offsetY_;
    sourceMap:=nil;
  end;

PROCEDURE T_rasterImage.updateBoundingBox(CONST scaling: T_scalingOptions; VAR boundingBox: T_boundingBox);
  VAR r0,r1:double;
  begin
    if (scaling.axisTrafo['x'].autoscale) then begin
      if scaling.axisTrafo['x'].logs then r0:=1 else r0:=offsetX; r1:=width*scale+offsetX;
      if r0<boundingBox['x',0] then boundingBox['x',0]:=r0;
      if r1>boundingBox['x',1] then boundingBox['x',1]:=r1;
    end;
    if (scaling.axisTrafo['y'].autoscale) then begin
      if scaling.axisTrafo['y'].logs then r0:=1 else r0:=offsetY;r1:=sourceMap.height*scale+offsetY;
      if r0<boundingBox['y',0] then boundingBox['y',0]:=r0;
      if r1>boundingBox['y',1] then boundingBox['y',1]:=r1;
    end;
  end;

PROCEDURE T_rasterImage.render(CONST opt: T_scalingOptions; CONST screenBox: T_boundingBox; CONST yBaseLine: longint; CONST target: TBGRACanvas);
  VAR dest, source: TRect;
      x0:longint=0;
      y0:longint=0;
      x1,y1:longint;
      tx0,tx1,ty0,ty1:double;
  begin
    x1:=sourceMap.width;
    y1:=sourceMap.height;

    tx0:=opt.axisTrafo['x'].apply(offsetX+x0*scale);
    tx1:=opt.axisTrafo['x'].apply(offsetX+x1*scale);
    ty0:=opt.axisTrafo['y'].apply(offsetY+y1*scale);
    ty1:=opt.axisTrafo['y'].apply(offsetY+y0*scale);
    if (abs(tx1-tx0)>target.width*2) or (abs(ty1-ty0)>target.height*2) then begin
      repeat inc(x0); tx0:=opt.axisTrafo['x'].apply(offsetX+x0*scale); until tx0>0;              dec(x0); tx0:=opt.axisTrafo['x'].apply(offsetX+x0*scale);
      repeat dec(x1); tx1:=opt.axisTrafo['x'].apply(offsetX+x1*scale); until tx1<=target.width;  inc(x1); tx1:=opt.axisTrafo['x'].apply(offsetX+x1*scale);
      repeat dec(y1); ty0:=opt.axisTrafo['y'].apply(offsetY+y1*scale); until ty0>0;              inc(y1); ty0:=opt.axisTrafo['y'].apply(offsetY+y1*scale);
      repeat inc(y0); ty1:=opt.axisTrafo['y'].apply(offsetY+y0*scale); until ty1<=target.height; dec(y0); ty1:=opt.axisTrafo['y'].apply(offsetY+y0*scale);
    end;

    dest.create(round(tx0),
                round(ty0),
                round(tx1),
                round(ty1));
    source.create(x0,y0,x1,y1);

    if dest.width>2*source.width
    then target.AntialiasingMode:=amOff
    else target.AntialiasingMode:=amOn;
    target.CopyRect(dest,sourceMap,source);

    target.AntialiasingMode:=amOn;
  end;

FUNCTION T_rasterImage.toStatementForExport(CONST firstRow: boolean; CONST literalRecycler: P_literalRecycler; VAR globalRowData: T_listLiteral): string;
  VAR myRowIndex:longint;
      ix,iy:longint;
      data:P_listLiteral;
      pixel: TBGRAPixel;
  begin
    myRowIndex:=globalRowData.size;
    result:='plotRasterImage(ROW['+intToStr(myRowIndex)+']*(1/255),'+intToStr(width);
    if (scale<>1) or (offsetX<>0) or (offsetY<>0)
    then result+=','+myFloatToStr(scale);
    if (offsetX<>0) or (offsetY<>0)
    then result+=','+myFloatToStr(offsetX);
    if offsetY<>0
    then result+=','+myFloatToStr(offsetY);
    result+=');';
    data:=literalRecycler^.newListLiteral(sourceMap.width*sourceMap.height);
    for iy:=0 to sourceMap.height-1 do
    for ix:=0 to sourceMap.width-1 do begin
      pixel:=sourceMap.GetPixel256(ix,iy,0,0,rfBox,false);
      if pixel.alpha=255 then begin
        if (pixel.RED=pixel.GREEN) and (pixel.RED=pixel.BLUE) then begin
          data^.appendInt(literalRecycler,pixel.RED);
        end else begin
          data^.append(literalRecycler,
                       literalRecycler^.newListLiteral(3)^
            .appendInt(literalRecycler,pixel.RED)^
            .appendInt(literalRecycler,pixel.GREEN)^
            .appendInt(literalRecycler,pixel.BLUE),false);
        end;
      end else begin
        data^.append(literalRecycler,
                     literalRecycler^.newListLiteral(4)^
          .appendInt(literalRecycler,pixel.RED)^
          .appendInt(literalRecycler,pixel.GREEN)^
          .appendInt(literalRecycler,pixel.BLUE)^
          .appendInt(literalRecycler,pixel.alpha),false);
      end;
    end;
    globalRowData.append(literalRecycler,data,false);
  end;

FUNCTION T_rasterImage.getFullClone: P_plotPrimitive;
  VAR rasterImage:P_rasterImage;
      iy,ix:longint;
      pixel: TBGRAPixel;
  begin
    new(rasterImage,create(scale,offsetX,offsetY));
    rasterImage^.width:=width;
    rasterImage^.pseudoIndex:=pseudoIndex;
    rasterImage^.sourceMap:=TBGRADefaultBitmap.create(sourceMap.width,sourceMap.height);
    sourceMap.CopyPropertiesTo(rasterImage^.sourceMap);
    for iy:=0 to sourceMap.height-1 do
    for ix:=0 to sourceMap.width-1 do begin
      pixel:=sourceMap.GetPixel256(ix,iy,0,0,rfBox,false);
      rasterImage^.sourceMap.setPixel(ix,iy,pixel.ToColor);
      rasterImage^.sourceMap.AlphaPixel(ix,iy,pixel.alpha);
    end;
    result:=rasterImage;
  end;

DESTRUCTOR T_rasterImage.destroy;
  begin
    if sourceMap<>nil then FreeAndNil(sourceMap);
  end;

PROCEDURE T_bSplineBuilder.flush;
  FUNCTION N(CONST i,p:longint; CONST u:double):double;
    begin
      if p=0 then begin
        if (i<=u) and (u<i+1) then result:=1 else result:=0;
      end else
        result:=(u-i    )/p*N(i  ,p-1,u)+
                (i+p+1-u)/p*N(i+1,p-1,u);
    end;

  VAR support:array[0..3] of T_point;

  FUNCTION interpolate(CONST u:double):T_point;
    begin
      result:=support[0]*N(-3,3,u)+
              support[1]*N(-2,3,u)+
              support[2]*N(-1,3,u)+
              support[3]*N( 0,3,u);
    end;

  PROCEDURE addInterpolated;
    PROCEDURE addPointBetween(CONST u0,u1:double; CONST p0,p1:T_point);
      VAR um:double;
          pm:T_point;
      begin
        um:=(u0+u1)*0.5;
        pm:=interpolate(um);
        if sqr(pm[0]-p0[0])+sqr(pm[1]-p0[1])>100 then addPointBetween(u0,um,p0,pm);
        inherited add(pm);
        if sqr(pm[0]-p1[0])+sqr(pm[1]-p1[1])>100 then addPointBetween(um,u1,pm,p1);
      end;

    VAR p0,p1:T_point;
    begin
      p0:=interpolate(0);
      p1:=interpolate(1);
      inherited add(p0);
      if sqr(p0[0]-p1[0])+sqr(p0[1]-p1[1])>100 then addPointBetween(0,1,p0,p1);
    end;

  VAR i:longint;
  begin
    if approxFill=1
    then inherited add(toApproximate[0])
    else if approxFill=2 then begin
      inherited add(toApproximate[0]);
      inherited add(toApproximate[1]);
    end else if approxFill>=2 then begin
      for i:=-2 to approxFill-2 do begin
        if i  <0            then support[0]:=toApproximate[0           ] else support[0]:=toApproximate[i  ];
        if i+1<0            then support[1]:=toApproximate[0           ] else support[1]:=toApproximate[i+1];
        if i+2>approxFill-1 then support[2]:=toApproximate[approxFill-1] else support[2]:=toApproximate[i+2];
        if i+3>approxFill-1 then support[3]:=toApproximate[approxFill-1] else support[3]:=toApproximate[i+3];
        addInterpolated;
      end;
      inherited add(interpolate(1));
    end;
    approxFill:=0;
  end;

PROCEDURE T_cSplineBuilder.flush;
  VAR M:array of T_point=();
      C:array of double=();
      i,n:longint;
      cub0,cub1, off,lin :T_point;

  FUNCTION interpolate(CONST t:double):T_point;
    begin
      result:=off+(lin+cub1*sqr(t))*t+cub0*sqr(1-t)*(1-t);
    end;

  PROCEDURE addInterpolated;
    PROCEDURE addPointBetween(CONST u0,u1:double; CONST p0,p1:T_point);
      VAR um:double;
          pm:T_point;
      begin
        um:=(u0+u1)*0.5;
        pm:=interpolate(um);
        if sqr(pm[0]-p0[0])+sqr(pm[1]-p0[1])>100 then addPointBetween(u0,um,p0,pm);
        inherited add(pm);
        if sqr(pm[0]-p1[0])+sqr(pm[1]-p1[1])>100 then addPointBetween(um,u1,pm,p1);
      end;

    VAR p0,p1:T_point;
    begin
      p0:=interpolate(0);
      p1:=interpolate(1);
      inherited add(p0);
      if sqr(p0[0]-p1[0])+sqr(p0[1]-p1[1])>100 then addPointBetween(0,1,p0,p1);
    end;

  begin
    //TODO: add only points if not "too close"
    if interpFill =1
    then inherited add(toInterpolate[0])
    else if interpFill=2 then begin
      inherited add(toInterpolate[0]);
      inherited add(toInterpolate[1]);
    end else if interpFill>2 then begin
      n:=interpFill;
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

      for i:=0 to n-1 do begin
        cub0:=M[i  ]*(1/6);
        cub1:=M[i+1]*(1/6);
        off :=toInterpolate[i]-M[i]*(1/6);
        lin :=toInterpolate[i+1]-toInterpolate[i]-(M[i+1]-M[i])*(1/6);
        addInterpolated;
      end;
      inherited add(toInterpolate[n]);
    end;
    interpFill:=0;
  end;

CONSTRUCTOR T_lineBuilder.createLineBuilder;
  begin
    setLength(buffer,100);
    bufferFill:=0;
    setLength(row.lineStrips,0);
    setLength(row.polygons  ,0);
    rowFill:=0;
  end;

CONSTRUCTOR T_bSplineBuilder.createBSplineBuilder;
  begin
    inherited createLineBuilder;
    setLength(toApproximate,100);
    approxFill:=0;
  end;

CONSTRUCTOR T_cSplineBuilder.createCSplineBuilder;
  begin
    inherited createLineBuilder;
    setLength(toInterpolate,100);
    interpFill:=0;
  end;

DESTRUCTOR T_bSplineBuilder.destroy;
  begin
    setLength(toApproximate,0);
    inherited destroy;
  end;

DESTRUCTOR T_cSplineBuilder.destroy;
  begin
    setLength(toInterpolate,0);
    inherited destroy;
  end;

DESTRUCTOR T_lineBuilder.destroy;
  begin setLength(buffer,0); end;

FUNCTION T_lineBuilder.get: T_rowToPaintArr;
  begin
    if bufferFill>0 then skip;
    setLength(row.lineStrips,rowFill);
    result:=row;
  end;

FUNCTION T_bSplineBuilder.get: T_rowToPaintArr;
  begin
    flush;
    result:=inherited get;
  end;

FUNCTION T_cSplineBuilder.get: T_rowToPaintArr;
  begin
    flush;
    result:=inherited get;
  end;

PROCEDURE T_lineBuilder.add(CONST nextPoint: T_point);
  VAR p:TPoint;
  begin
    if bufferFill>=length(buffer) then setLength(buffer,round(length(buffer)*1.2)+1);
    p.X:=round(nextPoint[0]);
    p.Y:=round(nextPoint[1]);
    buffer[bufferFill]:=p;
    inc(bufferFill);
  end;

PROCEDURE T_bSplineBuilder.add(CONST nextPoint: T_point);
  begin
    if approxFill>=length(toApproximate) then setLength(toApproximate,round(length(toApproximate)*1.2)+1);
    toApproximate[approxFill]:=nextPoint;
    inc(approxFill);
  end;

PROCEDURE T_cSplineBuilder.add(CONST nextPoint: T_point);
  begin
    if interpFill>=length(toInterpolate) then setLength(toInterpolate,round(length(toInterpolate)*1.2)+1);
    toInterpolate[interpFill]:=nextPoint;
    inc(interpFill);
  end;

FUNCTION T_lineBuilder.skip: boolean;
  VAR i:longint;
  begin
    if bufferFill>0 then begin
      if rowFill>=length(row.lineStrips) then setLength(row.lineStrips,round(length(row.lineStrips)*1.2)+2);
      setLength(row.lineStrips[rowFill],bufferFill);
      for i:=0 to bufferFill-1 do row.lineStrips[rowFill][i]:=buffer[i];
      inc(rowFill);
      bufferFill:=0;
      result:=true;
    end else result:=false;
  end;

FUNCTION T_lineBuilder.newInstance: P_lineBuilder;
  begin
    new(result,createLineBuilder);
  end;

FUNCTION T_bSplineBuilder.skip: boolean;
  begin
    flush;
    result:=inherited skip;
  end;

FUNCTION T_bSplineBuilder.newInstance: P_lineBuilder;
  begin
    new(P_bSplineBuilder(result),createBSplineBuilder);
  end;

FUNCTION T_cSplineBuilder.skip: boolean;
  begin
    flush;
    result:=inherited skip;
  end;

FUNCTION T_cSplineBuilder.newInstance: P_lineBuilder;
  begin
    new(P_cSplineBuilder(result),createCSplineBuilder);
  end;

FUNCTION T_dataRow.getPoint(CONST index: longint): T_point;
  begin
    result:=dat[index];
  end;

PROCEDURE T_dataRow.setPoint(CONST index: longint; CONST value: T_point);
  begin
    if index>=alloc then setSize(index+1);
    dat[index]:=value;
  end;

PROCEDURE T_dataRow.setSize(CONST newSize: longint);
  begin
    alloc:=newSize;
    ReAllocMem(dat,sizeOf(T_point)*alloc);
  end;

PROCEDURE T_dataRow.init(CONST initialSize: longint);
  begin
    alloc:=initialSize;
    getMem(dat,sizeOf(T_point)*alloc);
  end;

PROCEDURE T_dataRow.free;
  begin
    freeMem(dat,sizeOf(T_point)*alloc);
    alloc:=0;
  end;

PROCEDURE T_dataRow.cloneTo(OUT other: T_dataRow);
  begin
    other.init(alloc);
    move(dat^,other.dat^,sizeOf(T_point)*alloc);
  end;

FUNCTION numToString(num:double):string;
  VAR inum:int64;
  begin
    if (num>=-9E18) and (num<9E18) then begin
      inum:=round(num);
      if num=inum then result:=intToStr(inum)
                  else result:=myFloatToStr(num);
    end else result:=myFloatToStr(num);
  end;

FUNCTION T_dataRow.toNewLiteralForExport(CONST literalRecycler:P_literalRecycler):P_listLiteral;
  FUNCTION simplify(CONST num:double):P_numericLiteral;
    VAR inum:int64;
    begin
      if (num>=-9E18) and (num<9E18) then begin
        inum:=round(num);
        if num=inum then result:=literalRecycler^.newIntLiteral(inum)
                    else result:=literalRecycler^.newRealLiteral(num);
      end else result:=literalRecycler^.newRealLiteral(num);
    end;

  VAR i:longint=0;
      simple:boolean=true;
  begin
    while (i<alloc) and simple do begin simple:=simple and (dat[i,0]=i); inc(i); end;
    result:=literalRecycler^.newListLiteral(alloc);
    if simple
    then for i:=0 to alloc-1 do result^.append(literalRecycler,simplify(dat[i,1]),false)
    else for i:=0 to alloc-1 do result^.append(literalRecycler,literalRecycler^.newListLiteral(2)^
                                                    .append(literalRecycler,simplify(dat[i,0]),false)^
                                                    .append(literalRecycler,simplify(dat[i,1]),false),false);
  end;

CONSTRUCTOR T_customText.create(CONST x, y: double; CONST txt: T_arrayOfString);
  CONST BLACK:T_color=(0,0,0,255);
        WHITE:T_color=(255,255,255,255);
  begin
    p[0]:=x;
    p[1]:=y;
    text:=txt;
    fontName:='';
    anchor:=cta_center;
    fontSize:=Nan;
    foreground:=BLACK;
    background:=WHITE;
    transparentBackground:=true;
    absolutePosition:=false;
  end;

DESTRUCTOR T_customText.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(text)-1 do text[i]:='';
    setLength(text,0);
    fontName:='';
  end;

FUNCTION absFontSize(CONST xRes,yRes:longint; CONST relativeSize:double):longint;
  begin
    result:=round(relativeSize*sqrt(sqr(xRes)+sqr(yRes))/500);
  end;

PROCEDURE T_customText.render(CONST opt: T_scalingOptions;
  CONST screenBox: T_boundingBox; CONST yBaseLine: longint;
  CONST target: TBGRACanvas);
  VAR screenLoc:T_point;
      x,y,
      i,k,
      textWidth,textHeight:longint;
      oldFont:TBGRAFont;
  begin
    if length(text)=0 then exit;

    if absolutePosition then begin
      try
        x:=round(opt.axisTrafo['x'].screenMin+p[0]*opt.axisTrafo['x'].screenExtend);
        y:=round(opt.axisTrafo['y'].screenMin-p[1]*opt.axisTrafo['y'].screenExtend);
      except
        exit;
      end;
    end else begin
      screenLoc:=opt.realToScreen(p);
      for i:=0 to 1 do if isNan(screenLoc[i]) or (screenLoc[i]<-2147483648) or (screenLoc[i]>2147483647) then exit;
      x:=round(screenLoc[0]);
      y:=round(screenLoc[1]);
    end;
    enterCriticalSection(globalTextRenderingCs);
    try
      oldFont     :=target.Font;

      if (fontName<>'') and (fontName<>target.Font.name) then target.Font.name:=fontName;
      if isNan(fontSize) then target.Font.height:=absFontSize(target.width,target.height,opt.relativeFontSize)
                         else target.Font.height:=absFontSize(target.width,target.height,            fontSize);
      target.Font.BGRAColor:=foreground;
      if transparentBackground then target.Brush.style:=bsClear
      else begin
        target.Brush.style:=bsSolid;
        target.Brush.BGRAColor:=background;
      end;

      textWidth :=0;
      textHeight:=0;
      for k:=0 to length(text)-1 do begin
        try
          i:=target.textWidth(text[k]);
        except
          i:=0;
        end;
        if i>textWidth then textWidth:=i;
        try
          inc(textHeight,target.textHeight(text[k]));
        except
        end;
      end;

      case anchor of
        cta_bottomLeft,cta_bottom,cta_bottomRight:dec(y,textHeight      );
        cta_centerLeft,cta_center,cta_centerRight:dec(y,textHeight shr 1);
      end;
      case anchor of
        cta_topRight,cta_centerRight,cta_bottomRight:dec(x,textWidth      );
        cta_top     ,cta_center     ,cta_bottom     :dec(x,textWidth shr 1);
      end;
      target.Pen.style:=psClear;
      if (x<=target.width) and (x+textWidth>=0) and (y<=target.height) and (y+textHeight>=0) then begin
        if (length(text)>1) and not(transparentBackground) then target.FillRect(x,y,x+textWidth,y+textHeight);
        for k:=0 to length(text)-1 do begin
          target.textOut(x,
                         y,
                         text[k]);
          inc(y,target.textHeight(text[k]));
        end;
      end;
      target.Font:=oldFont;
    finally
      leaveCriticalSection(globalTextRenderingCs);
    end;
  end;

PROCEDURE T_customText.setAnchor(CONST s: string);
  VAR alX:(Left,center,Right)=center;
      alY:(top,middle,Bottom)=middle;
      cs:T_charSet=[];
      c:char;
  begin
    for c in s do include(cs,upCase(c));
    if ('T' in cs) then alY:=top;
    if ('B' in cs) then alY:=Bottom;
    if ('L' in cs) then alX:=Left;
    if ('R' in cs) then alX:=Right;
    case alY of
      top: case alX of
        Left  : anchor:=cta_topLeft;
        center: anchor:=cta_top;
        Right : anchor:=cta_topRight;
      end;
      middle: case alX of
        Left  : anchor:=cta_centerLeft;
        center: anchor:=cta_center;
        Right : anchor:=cta_centerRight;
      end;
      Bottom: case alX of
        Left  : anchor:=cta_bottomLeft;
        center: anchor:=cta_bottom;
        Right : anchor:=cta_bottomRight;
      end;
    end;
  end;

PROCEDURE T_customText.setForeground(CONST r, g, b: double; CONST alpha:double=1);
  begin
    foreground[cc_red  ]:=round(255*max(0,min(1,r)));
    foreground[cc_green]:=round(255*max(0,min(1,g)));
    foreground[cc_blue ]:=round(255*max(0,min(1,b)));
    foreground[cc_alpha]:=round(255*max(0,min(1,alpha)));
  end;

PROCEDURE T_customText.setBackground(CONST r, g, b: double; CONST alpha:double=1);
  begin
    transparentBackground:=false;
    background[cc_red  ]:=round(255*max(0,min(1,r)));
    background[cc_green]:=round(255*max(0,min(1,g)));
    background[cc_blue ]:=round(255*max(0,min(1,b)));
    background[cc_alpha]:=round(255*max(0,min(1,alpha)));
  end;

PROCEDURE T_customText.updateBoundingBox(CONST scaling: T_scalingOptions;
  VAR boundingBox: T_boundingBox);
  begin
    //TODO Maybe custom text with relative positions must influence the bounding box...
  end;

FUNCTION T_customText.toStatementForExport(CONST firstRow: boolean;
  CONST literalRecycler: P_literalRecycler; VAR globalRowData: T_listLiteral
  ): string;
  CONST anchorText:array[T_customTextAnchor] of string=('"TL"','"T"','"TR"','"L"','"C"','"R"','"BL"','"B"','"BR"');
  VAR dummy:boolean;
  begin
    if absolutePosition
    then result:='drawTextAbsolute('
    else result:='drawText(';
    result+=numToString(p[0])+','+numToString(p[1])+','+
            escapeString(join(text,C_lineBreakChar),es_pickShortest,se_testPending,dummy);
    if not(isNan(fontSize))
    then result+=','+numToString(fontSize);
    result+=','+anchorText[anchor];
    if fontName<>''
    then result+=','+escapeString(fontName,es_pickShortest,se_testPending,dummy);
    if not(transparentBackground) or
       (foreground[cc_red]<>0) or
       (foreground[cc_green]<>0) or
       (foreground[cc_blue]<>0) or
       (foreground[cc_alpha]<>255)
    then result+=',['+numToString(foreground[cc_red  ]/255)+','+
                      numToString(foreground[cc_green]/255)+','+
                      numToString(foreground[cc_blue ]/255)+']';
    if not(transparentBackground)
    then result+=',['+numToString(background[cc_red  ]/255)+','+
                      numToString(background[cc_green]/255)+','+
                      numToString(background[cc_blue ]/255)+']';
    result+=');';
  end;

FUNCTION T_customText.getFullClone: P_plotPrimitive;
  VAR clonedText:T_arrayOfString=();
      i:longint;
      customText:P_customText;
  begin
    setLength(clonedText,length(text));
    for i:=0 to length(clonedText)-1 do clonedText[i]:=text[i];
    new(customText,create(p[0],p[1],clonedText));
    customText^.fontName             :=fontName;
    customText^.anchor               :=anchor;
    customText^.fontSize             :=fontSize;
    customText^.foreground           :=foreground;
    customText^.background           :=background;
    customText^.transparentBackground:=transparentBackground;
    customText^.absolutePosition     :=absolutePosition;
    customText^.pseudoIndex          :=pseudoIndex;
    result:=customText;
  end;

PROCEDURE T_scalingOptions.setDefaults;
  VAR axis:char;
  begin
    for axis:='x' to 'y' do begin
      axisTrafo[axis].reset;
      axisStyle[axis]:=[gse_tics,gse_coarseGrid,gse_fineGrid];
    end;
    preserveAspect:=true;
    relativeFontSize:=10;
    autoscaleFactor:=1;
  end;

PROCEDURE T_scalingOptions.updateForPlot(CONST Canvas: TBGRACanvas; CONST primitives:T_plotPrimitiveList; VAR grid: T_ticInfos);

  VAR validSampleCount:longint=0;
  FUNCTION getSamplesBoundingBox:T_boundingBox;
    VAR axis:char;
        primitive:P_plotPrimitive;
    begin
      for axis:='x' to 'y' do
      if axisTrafo[axis].autoscale then begin
        result[axis,0]:= infinity;
        result[axis,1]:=-infinity;
      end else begin
        result[axis,0]:=axisTrafo[axis].rangeByUser[0];
        result[axis,1]:=axisTrafo[axis].rangeByUser[1];
      end;
      for primitive in primitives do begin
        primitive^.updateBoundingBox(self,result);
        inc(validSampleCount);
      end;
    end;

    PROCEDURE autoscaleByX(VAR range:T_boundingBox);
      VAR center, extend: double;
      begin
        center:=(range['y',1]+range['y',0])*0.5;
        extend:=(range['x',1]-range['x',0])/axisTrafo['x'].screenExtend*axisTrafo['y'].screenExtend*0.5;
        range['y',0]:=center-extend;
        range['y',1]:=center+extend;
      end;

    PROCEDURE autoscaleByY(VAR range:T_boundingBox);
      VAR center, extend: double;
      begin
        center:=(range['x',1]+range['x',0])*0.5;
        extend:=(range['y',1]-range['y',0])/axisTrafo['y'].screenExtend*axisTrafo['x'].screenExtend*0.5;
        range['x', 0]:=center-extend;
        range['x', 1]:=center+extend;
      end;

  PROCEDURE prepareRanges;
    VAR axis:char;
        boundingBox:T_boundingBox;
        i:longint;
        tmp:double;
    begin
      if axisTrafo['x'].autoscale
      or axisTrafo['y'].autoscale
      then boundingBox:=getSamplesBoundingBox
      else for axis:='x' to 'y' do for i:=0 to 1 do boundingBox[axis,i]:=axisTrafo[axis].rangeByUser[i];
      //Stretch bounding box, potentially transform to logscale:----------------------------------
      for axis:='x' to 'y' do begin
        if (validSampleCount>=1) then for i:=0 to 1 do begin
          if isNan(boundingBox[axis,i]) or isInfinite(boundingBox[axis,i])
          then boundingBox[axis,i]:=axisTrafo[axis].rangeByUser[i];
        end;
        for i:=0 to 1 do if axisTrafo[axis].logs then boundingBox[axis,i]:=ln(boundingBox[axis,i]);
        tmp:=(boundingBox[axis,1]-boundingBox[axis,0])*(1-autoscaleFactor);
        boundingBox[axis,0]:=boundingBox[axis,0]-tmp;
        boundingBox[axis,1]:=boundingBox[axis,1]+tmp;
      end;
      //----------------------------------:Stretch bounding box, potentially transform to logscale
      //Adapt bounding box to respect preservation of aspect ratio:-------------------------------
      if preserveAspect and (axisTrafo['x'].logscale = axisTrafo['y'].logscale) then begin
        if (axisTrafo['x'].autoscale or axisTrafo['y'].autoscale) then begin
          if axisTrafo['x'].autoscale then begin
            if axisTrafo['y'].autoscale then begin
              if (boundingBox['x',1]-boundingBox['x',0])/axisTrafo['x'].screenExtend>
                 (boundingBox['y',1]-boundingBox['y',0])/axisTrafo['y'].screenExtend
              then autoscaleByX(boundingBox)
              else autoscaleByY(boundingBox);
            end else autoscaleByX(boundingBox);
          end else if axisTrafo['y'].autoscale then autoscaleByY(boundingBox);
        end else begin
          if (boundingBox['x',1]-boundingBox['x',0])/axisTrafo['x'].screenExtend>
             (boundingBox['y',1]-boundingBox['y',0])/axisTrafo['y'].screenExtend
          then autoscaleByX(boundingBox)
          else autoscaleByY(boundingBox);
        end;
      end;
      //-------------------------------:Adapt bounding box to respect preservation of aspect ratio
      for axis:='x' to 'y' do begin
        if axisTrafo[axis].logscale then for i:=0 to 1 do boundingBox[axis,i]:=exp(boundingBox[axis,i]);
        axisTrafo[axis].setWorldRange(boundingBox[axis,0],boundingBox[axis,1]);
      end;
    end;

  PROCEDURE initTics(CONST axis: char);

    FUNCTION niceText(CONST value, scale: longint): string;
      CONST suf: array[0..5] of string = ('', '0', '00', '000', '0000', '00000');
      begin
        if value = 0 then exit('0');
        if (scale>=-5) and (scale<0) then begin
          result:=intToStr(value);
          while length(result)<-scale do result:='0'+result;
          exit(copy(result, 1, length(result)+scale)+'.'+copy(result, length(result)+1+scale, -scale));
        end else if (scale>=0) and (scale<5)
        then exit(intToStr(value)+suf[scale])
        else exit(intToStr(value)+'E'+intToStr(scale));
      end;

    PROCEDURE addTic(CONST realPos: double; CONST realTxt: ansistring; CONST isMajorTic: boolean);
      begin
        if axisTrafo[axis].sampleIsInRange(realPos) then begin
          setLength(grid[axis], length(grid[axis])+1);
          with grid[axis][length(grid[axis])-1] do begin
            pos:=realPos;
            major:=isMajorTic;
            if major then txt:=realTxt
                     else txt:='';
          end;
        end;
      end;

    FUNCTION pot10(y: int64): double; inline;
      VAR p10: double;
      begin
        if y>=0 then p10:=10 else begin
          p10:=0.1;
          y:=-y;
        end;
        result:=1;
        while y>0 do begin
          if odd(y) then result:=result*p10;
          p10:=p10*p10;
          y:=y shr 1;
        end;
      end;

    PROCEDURE initLinearTics(CONST power, majorTicRest, minorTicRest: longint);
      VAR j: longint;
      begin
        setLength(grid[axis], 0);
        for j:=floor(axisTrafo[axis].rangeByAutosize[0]*pot10(-power)) to
                ceil(axisTrafo[axis].rangeByAutosize[1]*pot10(-power)) do
        if j mod minorTicRest = 0 then addTic(pot10(power)*j, niceText(j div 10, power+1), (j mod majorTicRest) = 0);
      end;

    PROCEDURE initLogTics(CONST ppot:longint);
      VAR i:longint;
          j:byte;
      begin
        setLength(grid[axis], 0);
        if ppot=-1 then begin
          for i:=floor(ln(axisTrafo[axis].rangeByAutosize[0])/ln(10)) to
                  ceil(ln(axisTrafo[axis].rangeByAutosize[1])/ln(10)) do for j:=1 to 9 do
            addTic(pot10(i)*j,niceText(j,i),true);
        end else if ppot=0 then begin
          for i:=floor(ln(axisTrafo[axis].rangeByAutosize[0])/ln(10)) to
                  ceil(ln(axisTrafo[axis].rangeByAutosize[1])/ln(10)) do for j:=1 to 9 do
            addTic(pot10(i)*j,niceText(j,i),j in [1,2,5]);
        end else if ppot=1 then begin
          for i:=floor(ln(axisTrafo[axis].rangeByAutosize[0])/ln(10)) to
                  ceil(ln(axisTrafo[axis].rangeByAutosize[1])/ln(10)) do for j:=1 to 9 do
            addTic(pot10(i)*j,niceText(j,i),j=1);
        end else begin
          for i:=floor(ln(axisTrafo[axis].rangeByAutosize[0])/ln(10)) to
                  ceil(ln(axisTrafo[axis].rangeByAutosize[1])/ln(10)) do
            if (i mod (ppot-1)=0) then addTic(pot10(i),niceText(1,i),i mod ppot=0);
        end;
      end;

    FUNCTION majorTicCount:longint;
      VAR t:T_ticInfo;
      begin
        result:=0;
        for t in grid[axis] do if t.major then inc(result);
      end;

    VAR i:longint;
        j:byte;
        k:longint;
        logRange:double;
    begin
      if axisTrafo[axis].logscale then begin
        //Tics: ... 0.5 0.6 0.7 0.8 0.9 1 2 3 4 5 ...
        k:=0;
        for i:=floor(ln(axisTrafo[axis].rangeByAutosize[0])/ln(10)) to
                ceil(ln(axisTrafo[axis].rangeByAutosize[1])/ln(10)) do if k<=15 then for j:=1 to 9 do
        if axisTrafo[axis].sampleIsInRange(pot10(i)*j) then inc(k);
        if k<15 then begin
          initLogTics(-1);
          exit;
        end;
        //Tics: ... 0.5 1 2 5 10 20 50 ...
        k:=0;
        for i:=floor(ln(axisTrafo[axis].rangeByAutosize[0])/ln(10)) to
                ceil(ln(axisTrafo[axis].rangeByAutosize[1])/ln(10)) do if k<=15 then for j:=1 to 5 do
        if (j in [1,2,5]) and axisTrafo[axis].sampleIsInRange(pot10(i)*j) then inc(k);
        if k<15 then begin
          initLogTics(0);
          exit;
        end;
        //Tics: ... 0.01 0.1 1 10 100 ...
        for j:=1 to 60 do begin
          k:=0;
          for i:=floor(ln(axisTrafo[axis].rangeByAutosize[0])/ln(10)) to
                  ceil(ln(axisTrafo[axis].rangeByAutosize[1])/ln(10)) do
          if (i mod j = 0) and axisTrafo[axis].sampleIsInRange(pot10(i)*j) then inc(k);
          if k<15 then begin
            initLogTics(j);
            exit;
          end;
        end;
      end else begin
        if (preserveAspect and (axisTrafo['x'].logscale=axisTrafo['y'].logscale))
        then logRange:=0.5*(log10(axisTrafo['x'].rangeByAutosize[1]-axisTrafo['x'].rangeByAutosize[0])
                           +log10(axisTrafo['y'].rangeByAutosize[1]-axisTrafo['y'].rangeByAutosize[0]))
        else logRange:=log10(axisTrafo[axis].rangeByAutosize[1]-axisTrafo[axis].rangeByAutosize[0]);
        try
          i:=round(logRange-1.8);
          logRange:=logRange-1.8-i;
        except
          i:=1;
        end;
        if logRange<0.2
        then initLinearTics(i,10,1)
        else initLinearTics(i,50,10);
      end;
    end;

  VAR ticSampleText:string;
  FUNCTION updateBorders:boolean;
    VAR newX0,newY0:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      try
        if gse_tics in axisStyle['y']
        then newX0:=Canvas.textWidth(ticSampleText)+5
        else newX0:=0;
        if gse_tics in axisStyle['x']
        then newY0:=Canvas.height-Canvas.textHeight(ticSampleText)-5
        else newY0:=Canvas.height;
      finally
        leaveCriticalSection(globalTextRenderingCs);
      end;

      result:=(axisTrafo['x'].screenRange[0]<newX0-20) or
              (axisTrafo['x'].screenRange[0]>newX0   ) or
              (axisTrafo['y'].screenRange[0]>newY0+20) or
              (axisTrafo['y'].screenRange[0]<newY0);

      axisTrafo['x'].screenRange[0]:=newX0;
      axisTrafo['x'].screenRange[1]:=Canvas.width;
      axisTrafo['x'].prepare;

      axisTrafo['y'].screenRange[0]:=newY0;
      axisTrafo['y'].screenRange[1]:=0;
      axisTrafo['y'].prepare;
    end;

  FUNCTION longestTic:string;
    VAR axis:char;
        tic:T_ticInfo;
    begin
      result:='';
      for axis:='y' downto 'x' do begin
        for tic in grid[axis] do if length(tic.txt)>length(result) then result:=tic.txt;
        if result<>'' then exit(result);
      end;
    end;

  PROCEDURE transformTics;
    VAR a:char;
        i:longint;
    begin
      for a:='x' to 'y' do for i:=0 to length(grid[a])-1 do grid[a,i].pos:=axisTrafo[a].apply(grid[a,i].pos);
    end;

  VAR axis:char;
  begin
    enterCriticalSection(globalTextRenderingCs);
    try
      Canvas.Font.height:=absoluteFontSize(Canvas.width,Canvas.height);
      ticSampleText:='.0E12';
      updateBorders;
      prepareRanges;
      for axis:='x' to 'y' do if axisStyle[axis]<>[] then initTics(axis);
      ticSampleText:=longestTic;
      if updateBorders then begin
        prepareRanges;
        for axis:='x' to 'y' do if axisStyle[axis]<>[] then initTics(axis);
      end;
      transformTics;
    finally
      leaveCriticalSection(globalTextRenderingCs);
    end;
  end;

FUNCTION T_scalingOptions.transformRow(CONST row: T_dataRow; CONST yBaseLine:longint; CONST style:T_style): T_rowToPaintArr;
  FUNCTION isPointValid(CONST p:T_point; OUT transformed:T_point):boolean;
    begin
      transformed[0]:=axisTrafo['x'].apply(p[0]);
      result:=not(isNan(transformed[0])) and (transformed[0]>=-2147483648) and (transformed[0]<=2147483647);
      if result then begin
        transformed[1]:=axisTrafo['y'].apply(p[1]);
        result:=not(isNan(transformed[1])) and (transformed[1]>=-2147483648) and (transformed[1]<=2147483647);
      end;
    end;

  VAR i,j:longint;
      prev:T_point;
      prevValid:boolean=false;
      tr:T_point=(Nan,Nan);
      pointIsValid:boolean;
      lineBuilder:P_lineBuilder;
  begin
    case style.builderStyle of
      bs_bSpline : new(P_bSplineBuilder(lineBuilder),createBSplineBuilder);
      bs_cSpline : new(P_cSplineBuilder(lineBuilder),createCSplineBuilder);
     else          new(P_lineBuilder   (lineBuilder),createLineBuilder);
    end;
    if style.feedStyle=fs_tube then  new(P_tubeBuilder(lineBuilder),createTubeBuilder(lineBuilder));

    case style.feedStyle of
      fs_direct,fs_tube : for i:=0 to row.size-1 do if isPointValid(row[i],tr) then lineBuilder^.add(tr) else lineBuilder^.skip;
      fs_stepLeft : for i:=0 to row.size-1 do begin
        pointIsValid:=isPointValid(row[i],tr);
        if pointIsValid and prevValid then begin
          lineBuilder^.add(pointOf(tr[0],prev[1])); //move x first
          lineBuilder^.add(tr);
        end else lineBuilder^.skip;
        prev:=tr;
        prevValid:=pointIsValid;
      end;
      fs_stepRight: for i:=0 to row.size-1 do begin
        pointIsValid:=isPointValid(row[i],tr);
        if pointIsValid and prevValid then begin
          lineBuilder^.add(pointOf(prev[0],tr[1])); //move y first
          lineBuilder^.add(tr);
        end else lineBuilder^.skip;
        prev:=tr;
        prevValid:=pointIsValid;
      end;
      fs_pair: begin
        i:=0;
        while i<row.size-1 do begin
          if isPointValid(row[i],prev) and isPointValid(row[i+1],tr)
          then begin
            lineBuilder^.add(prev);
            lineBuilder^.add(tr);
            lineBuilder^.skip;
          end;
          inc(i,2);
        end;
      end;
      fs_bar: for i:=0 to row.size-1 do begin
        pointIsValid:=isPointValid(row[i],tr);
        if pointIsValid and prevValid then begin
          lineBuilder^.add(pointOf(prev[0]*0.95+tr[0]*0.05,yBaseLine));
          lineBuilder^.add(pointOf(prev[0]*0.05+tr[0]*0.95,tr[1]    ));
          lineBuilder^.skip;
        end;
        prev:=tr;
        prevValid:=pointIsValid;
      end;
    end;
    result:=lineBuilder^.get;

    if style.polygonStyle=ps_direct then begin
      result.polygons:=result.lineStrips;
      setLength(result.lineStrips,0);
    end;
    if (style.polygonStyle=ps_additional) and (style.feedStyle<>fs_tube) then begin
      setLength(result.polygons,length(result.lineStrips));
      for i:=0 to length(result.polygons)-1 do begin
        setLength(result.polygons[i],length(result.lineStrips[i])+2);
        result.polygons[i][0].X:=result.lineStrips[i][length(result.lineStrips[i])-1].X;
        result.polygons[i][0].Y:=yBaseLine;
        result.polygons[i][1].X:=result.lineStrips[i][0].X;
        result.polygons[i][1].Y:=yBaseLine;
        for j:=0 to length(result.lineStrips[i])-1 do result.polygons[i][j+2]:=result.lineStrips[i][j];
      end;
    end;

    dispose(lineBuilder,destroy);
  end;

FUNCTION T_scalingOptions.getRefinementSteps(CONST row:T_dataRow; CONST samplesToDistribute:longint):T_arrayOfLongint;
  FUNCTION heightOfTriangle(CONST x,y,z:T_point):double; inline;
    VAR a,b:T_point;
        t:double;
    begin
      a:=realToScreen(z)-realToScreen(x);
      b:=realToScreen(y)-realToScreen(x);
      t:=(a[0]*b[0]+a[1]*b[1])/(sqr(a[0])+sqr(a[1]));
      result:=sqrt(sqr(a[0]*t-b[0])+
                   sqr(a[1]*t-b[1]));
    end;

  VAR triangleHeights:array of double=();
      tmp:double=0;
      i:longint;
      k:longint=0;
  begin
    //1. Determine heights of triangles; replace Nan and infinite values by average of valid values
    setLength(triangleHeights,row.size-2);
    for i:=0 to row.size-3 do begin
      triangleHeights[i]:=heightOfTriangle( row[i],row[i+1],row[i+2]);
      if not(isNan(triangleHeights[i])) and not(isInfinite(triangleHeights[i])) then begin
        tmp+=triangleHeights[i];
        k  +=1;
      end;
    end;
    tmp:=tmp/k;
    if k=0
    then for i:=0 to length(triangleHeights)-1 do                                                                       triangleHeights[i]:=1
    else for i:=0 to length(triangleHeights)-1 do if isNan(triangleHeights[i]) or (isInfinite(triangleHeights[i])) then triangleHeights[i]:=tmp;
    //2. Distribute triangle heights.
    k:=length(triangleHeights);
    setLength(triangleHeights,k+1);
    triangleHeights[k]:=triangleHeights[k-1];
    for i:=k downto 1 do triangleHeights[i]+=triangleHeights[i-1];
    triangleHeights[0]*=2;
    //3. Normalize and return
    tmp:=0;
    for k:=0 to length(triangleHeights)-1 do tmp+=triangleHeights[k];
    tmp:=samplesToDistribute/tmp;
    //Note: initialize(result) leads to memory leak
    setLength(result,length(triangleHeights));
    if isNan(tmp) or isInfinite(tmp)
    then for k:=0 to length(result)-1 do result[k]:=1
    else for k:=0 to length(result)-1 do result[k]:=round(triangleHeights[k]*tmp);
  end;

FUNCTION T_scalingOptions.screenToReal(CONST x, y: integer): T_point;
  begin
    result[0]:=axisTrafo['x'].applyInverse(x);
    result[1]:=axisTrafo['y'].applyInverse(y);
  end;

FUNCTION T_scalingOptions.realToScreen(CONST p:T_point):T_point;
  begin
    result[0]:=axisTrafo['x'].apply(p[0]);
    result[1]:=axisTrafo['y'].apply(p[1]);
  end;

FUNCTION T_scalingOptions.absoluteFontSize(CONST xRes, yRes: longint): longint;
  begin
    result:=absFontSize(xRes,yRes,relativeFontSize);
  end;

FUNCTION T_scalingOptions.getOptionString:string;
  begin
    result:='';
    result:=                            '"x0"=>'+numToString(axisTrafo['x'].worldMin);
    result+=BoolToStr(result='','',',')+'"x1"=>'+numToString(axisTrafo['x'].worldMax);
    result+=BoolToStr(result='','',',')+'"y0"=>'+numToString(axisTrafo['y'].worldMin);
    result+=BoolToStr(result='','',',')+'"y1"=>'+numToString(axisTrafo['y'].worldMax);
    result+=BoolToStr(result='','',',')+'"fontsize"=>'+numToString(relativeFontSize);
    result+=BoolToStr(result='','',',')+'"preserveAspect"=>'+boolLit[preserveAspect].toString;
    result+=BoolToStr(result='','',',')+'"autoscaleX"=>'+boolLit[axisTrafo['x'].autoscale].toString;
    result+=BoolToStr(result='','',',')+'"autoscaleY"=>'+boolLit[axisTrafo['y'].autoscale].toString;
    result+=BoolToStr(result='','',',')+'"logscaleX"=>' +boolLit[axisTrafo['x'].logscale ].toString;
    result+=BoolToStr(result='','',',')+'"logscaleY"=>' +boolLit[axisTrafo['y'].logscale ].toString;
    result+=BoolToStr(result='','',',')+'"autoscaleFactor"=>'+numToString(autoscaleFactor);
    result+=BoolToStr(result='','',',')+'"axisStyleX"=>'+intToStr(byte(axisStyle['x']));
    result+=BoolToStr(result='','',',')+'"axisStyleY"=>'+intToStr(byte(axisStyle['y']));
    if result<>'' then result:='setOptions(['+result+'].toMap);';
  end;

FUNCTION T_scalingOptions.getOptionDiffString(CONST before:T_scalingOptions):string;
  begin
    result:='';
    if axisTrafo['x'].worldMin <>before.axisTrafo['x'].worldMin  then result:=                            '"x0"=>'+numToString(axisTrafo['x'].worldMin);
    if axisTrafo['x'].worldMax <>before.axisTrafo['x'].worldMax  then result+=BoolToStr(result='','',',')+'"x1"=>'+numToString(axisTrafo['x'].worldMax);
    if axisTrafo['y'].worldMin <>before.axisTrafo['y'].worldMin  then result+=BoolToStr(result='','',',')+'"y0"=>'+numToString(axisTrafo['y'].worldMin);
    if axisTrafo['y'].worldMax <>before.axisTrafo['y'].worldMax  then result+=BoolToStr(result='','',',')+'"y1"=>'+numToString(axisTrafo['y'].worldMax);
    if relativeFontSize        <>before.relativeFontSize         then result+=BoolToStr(result='','',',')+'"fontsize"=>'+numToString(relativeFontSize);
    if preserveAspect          <>before.preserveAspect           then result+=BoolToStr(result='','',',')+'"preserveAspect"=>'+boolLit[preserveAspect].toString;
    if axisTrafo['x'].autoscale<>before.axisTrafo['x'].autoscale then result+=BoolToStr(result='','',',')+'"autoscaleX"=>'+boolLit[axisTrafo['x'].autoscale].toString;
    if axisTrafo['y'].autoscale<>before.axisTrafo['y'].autoscale then result+=BoolToStr(result='','',',')+'"autoscaleY"=>'+boolLit[axisTrafo['y'].autoscale].toString;
    if axisTrafo['x'].logscale <>before.axisTrafo['x'].logscale  then result+=BoolToStr(result='','',',')+'"logscaleX"=>' +boolLit[axisTrafo['x'].logscale ].toString;
    if axisTrafo['y'].logscale <>before.axisTrafo['y'].logscale  then result+=BoolToStr(result='','',',')+'"logscaleY"=>' +boolLit[axisTrafo['y'].logscale ].toString;
    if autoscaleFactor         <>before.autoscaleFactor          then result+=BoolToStr(result='','',',')+'"autoscaleFactor"=>'+numToString(autoscaleFactor);
    if axisStyle['x']          <>before.axisStyle['x']           then result+=BoolToStr(result='','',',')+'"axisStyleX"=>'+intToStr(byte(axisStyle['x']));
    if axisStyle['y']          <>before.axisStyle['y']           then result+=BoolToStr(result='','',',')+'"axisStyleY"=>'+intToStr(byte(axisStyle['y']));
    if result<>'' then result:='setOptions(['+result+'].toMap);';
  end;

PROCEDURE T_scalingOptions.modifyOptions(CONST o:T_scalingOptions; CONST modified:T_scalingOptionElements);
  begin
    if soe_x0              in modified then axisTrafo['x'].worldMin :=o.axisTrafo['x'].worldMin ;
    if soe_x1              in modified then axisTrafo['x'].worldMax :=o.axisTrafo['x'].worldMax ;
    if soe_y0              in modified then axisTrafo['y'].worldMin :=o.axisTrafo['y'].worldMin ;
    if soe_y1              in modified then axisTrafo['y'].worldMax :=o.axisTrafo['y'].worldMax ;
    if soe_fontsize        in modified then relativeFontSize        :=o.relativeFontSize        ;
    if soe_autoscaleFactor in modified then autoscaleFactor         :=o.autoscaleFactor         ;
    if soe_preserveAspect  in modified then preserveAspect          :=o.preserveAspect          ;
    if soe_autoscaleX      in modified then axisTrafo['x'].autoscale:=o.axisTrafo['x'].autoscale;
    if soe_autoscaleY      in modified then axisTrafo['y'].autoscale:=o.axisTrafo['y'].autoscale;
    if soe_logscaleX       in modified then axisTrafo['x'].logscale :=o.axisTrafo['x'].logscale ;
    if soe_logscaleY       in modified then axisTrafo['y'].logscale :=o.axisTrafo['y'].logscale ;
    if soe_axisStyleX      in modified then axisStyle['x']          :=o.axisStyle['x']          ;
    if soe_axisStyleY      in modified then axisStyle['y']          :=o.axisStyle['y']          ;
  end;

FUNCTION T_scalingOptions.equals(CONST other:T_scalingOptions):boolean;
  begin
    result:=(preserveAspect=other.preserveAspect) and
            (relativeFontSize=other.relativeFontSize) and
            (autoscaleFactor=other.autoscaleFactor) and
            (axisTrafo['x'].equals(other.axisTrafo['x'])) and
            (axisTrafo['y'].equals(other.axisTrafo['y'])) and
            (axisStyle['x']=other.axisStyle['x']) and
            (axisStyle['y']=other.axisStyle['y']);
  end;

CONSTRUCTOR T_sampleRow.create(CONST row: T_dataRow);
  begin
    style.init();
    sample:=row;
  end;

PROCEDURE T_sampleRow.updateBoundingBox(CONST scaling: T_scalingOptions;
  VAR boundingBox: T_boundingBox);
  VAR i: longint;
      point: T_point;
      x,y:double;
  begin
    for i:=0 to sample.size-1 do begin
      point:=sample[i];
      x:=point[0]; if (x<MIN_VALUE_FOR[scaling.axisTrafo['x'].logs]) or (x>MAX_VALUE_FOR[scaling.axisTrafo['x'].logs]) then continue;
      y:=point[1]; if (y<MIN_VALUE_FOR[scaling.axisTrafo['y'].logs]) or (x>MAX_VALUE_FOR[scaling.axisTrafo['y'].logs]) then continue;
      if not(scaling.axisTrafo['x'].autoscale) and not(scaling.axisTrafo['x'].sampleIsInRange(x)) or
         not(scaling.axisTrafo['y'].autoscale) and not(scaling.axisTrafo['y'].sampleIsInRange(y)) then continue;
      if (x<boundingBox['x',0]) then boundingBox['x',0]:=x;
      if (x>boundingBox['x',1]) then boundingBox['x',1]:=x;
      if (y<boundingBox['y',0]) then boundingBox['y',0]:=y;
      if (y>boundingBox['y',1]) then boundingBox['y',1]:=y;
    end;
  end;

PROCEDURE T_sampleRow.render(CONST opt: T_scalingOptions; CONST screenBox: T_boundingBox; CONST yBaseLine: longint; CONST target: TBGRACanvas);
  VAR scaleAndColor:T_scaleAndColor;
      screenRow:T_rowToPaintArr;

  PROCEDURE drawStraightLines;
    VAR i:longint;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      for i:=0 to length(screenRow.lineStrips)-1 do
        target.Polyline(screenRow.lineStrips[i]);
    end;

  PROCEDURE drawPolygons;
    VAR i:longint;
    begin
      if length(screenRow.lineStrips)=0 then begin
        target.Pen.style:=psSolid;
        target.Pen.BGRAColor:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
      end else begin
        target.Pen.style:=psClear;
      end;
      target.Brush.style:=scaleAndColor.solidStyle;
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      for i:=0 to length(screenRow.polygons)-1 do
        target.Polygon(screenRow.polygons[i]);
    end;

  PROCEDURE drawBoxes;
    VAR i:longint;
        withPen:boolean=false;
    begin
      if scaleAndColor.lineWidth<=0
      then target.Pen.style:=psClear
      else begin
        target.Pen.style:=psSolid;
        target.Pen.BGRAColor:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
        withPen:=true;
      end;
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      for i:=0 to length(screenRow.lineStrips)-1 do
        if withPen
        then target.Rectangle(screenRow.lineStrips[i][0].X,
                             screenRow.lineStrips[i][0].Y,
                             screenRow.lineStrips[i][1].X,
                             screenRow.lineStrips[i][1].Y)
        else target.FillRect(screenRow.lineStrips[i][0].X,
                             screenRow.lineStrips[i][0].Y,
                             screenRow.lineStrips[i][1].X,
                             screenRow.lineStrips[i][1].Y);

    end;

  PROCEDURE drawEllipses;
    PROCEDURE drawEllipse(CONST x0,y0,x1,y1:longint);
      VAR points:array[0..100] of TPoint;
          cx,cy,rx,ry:double;
          i:longint;
      begin
        if not(intersect(screenBox,boundingBoxOf(x0,y0,x1,y1))) or ((scaleAndColor.solidStyle=bsClear) and (scaleAndColor.lineWidth<1)) then exit;
        if (abs(x1-x0)>target.width) or (abs(y1-y0)>target.height) then begin
          cx:=(x0+x1)*0.5; rx:=(x1-x0)*0.5;
          cy:=(y0+y1)*0.5; ry:=(y1-y0)*0.5;
          for i:=0 to 100 do begin
            points[i].x:=round(cx+rx*cos(0.02*pi*i));
            points[i].y:=round(cy+ry*sin(0.02*pi*i));
          end;
          target.Polygon(points);
        end else begin
          target.Ellipse(x0,y0,x1,y1);
        end;
      end;
    VAR i:longint;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      if (scaleAndColor.lineWidth<=0) then target.Pen.style:=psClear;
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      i:=0;
      for i:=0 to length(screenRow.lineStrips)-1 do begin
        drawEllipse(screenRow.lineStrips[i][0].X,
                    screenRow.lineStrips[i][0].Y,
                    screenRow.lineStrips[i][1].X,
                    screenRow.lineStrips[i][1].Y);
      end;
      if (scaleAndColor.lineWidth<=0) then target.Pen.style:=psSolid;
    end;

  PROCEDURE drawPluses;
    VAR i:longint;
        p:TPoint;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecSquare;
      for i:=0 to length(screenRow.lineStrips)-1 do for p in screenRow.lineStrips[i] do begin
        target.MoveTo(p.x-scaleAndColor.symbolWidth,
                      p.y);
        target.LineTo(p.x+scaleAndColor.symbolWidth,
                      p.y);
        target.MoveTo(p.x,
                      p.y-scaleAndColor.symbolWidth);
        target.LineTo(p.x,
                      p.y+scaleAndColor.symbolWidth);
      end;
    end;

  PROCEDURE drawCrosses;
    VAR i:longint;
        p:TPoint;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecSquare;
      for i:=0 to length(screenRow.lineStrips)-1 do for p in screenRow.lineStrips[i] do begin
        target.MoveTo(p.x-scaleAndColor.symbolRadius,
                      p.y-scaleAndColor.symbolRadius);
        target.LineTo(p.x+scaleAndColor.symbolRadius,
                      p.y+scaleAndColor.symbolRadius);
        target.MoveTo(p.x+scaleAndColor.symbolRadius,
                      p.y-scaleAndColor.symbolRadius);
        target.LineTo(p.x-scaleAndColor.symbolRadius,
                      p.y+scaleAndColor.symbolRadius);
      end;
    end;

  PROCEDURE drawDots;
    VAR i:longint;
        p:TPoint;
    begin
      target.Pen.style:=psClear;
      target.Brush.style:=bsSolid;
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      if scaleAndColor.symbolWidth>=1 then begin
        for i:=0 to length(screenRow.lineStrips)-1 do for p in screenRow.lineStrips[i] do
          target.Ellipse(p.x-scaleAndColor.symbolWidth,
                         p.y-scaleAndColor.symbolWidth,
                         p.x+scaleAndColor.symbolWidth+1,
                         p.y+scaleAndColor.symbolWidth+1);
      end else begin
        for i:=0 to length(screenRow.lineStrips)-1 do for p in screenRow.lineStrips[i] do
          target.Pixels[p.x,p.y]:=scaleAndColor.lineColor;
      end;
    end;

  PROCEDURE drawImpulses;
    VAR i:longint;
        p: TPoint;
    begin
      target.Pen.style     :=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width    :=scaleAndColor.lineWidth;
      target.Pen.EndCap   :=pecSquare;
      for i:=0 to length(screenRow.lineStrips)-1 do for p in screenRow.lineStrips[i] do begin
        target.MoveTo(p.x, yBaseLine     );
        target.LineTo(p);
      end;
    end;

  {$ifdef debugMode}
  VAR startTicks: qword;
  {$endif}

  begin
    {$ifdef debugMode}
    startTicks:=GetTickCount64;
    {$endif}
    screenRow:=opt.transformRow(sample,yBaseLine,style);
    {$ifdef debugMode}
    writeln('     -- row transformation: ',GetTickCount64-startTicks,' ticks');
    startTicks:=GetTickCount64;
    {$endif}
    scaleAndColor:=style.getLineScaleAndColor(target.width,target.height);

    if ds_continuous in style.drawStyles then begin
      drawPolygons;
      drawStraightLines;
    end;
    if ds_plus     in style.drawStyles then drawPluses;
    if ds_cross    in style.drawStyles then drawCrosses;
    if ds_dot      in style.drawStyles then drawDots;
    if ds_impulse  in style.drawStyles then drawImpulses;
    if ds_box      in style.drawStyles then drawBoxes;
    if ds_ellipse  in style.drawStyles then drawEllipses;

    {$ifdef debugMode}
    writeln('     -- plot execution: ',GetTickCount64-startTicks,' in style: "',style.toString,'"');
    {$endif}

  end;

DESTRUCTOR T_sampleRow.destroy;
  begin
    sample.free;
  end;

FUNCTION T_sampleRow.toStatementForExport(CONST firstRow: boolean;CONST literalRecycler: P_literalRecycler; VAR globalRowData: T_listLiteral): string;
  VAR myRowIndex:longint;
  begin
    myRowIndex:=globalRowData.size;
    if firstRow then result:='plot@(ROW['+intToStr(myRowIndex)+']);'
             else result:='addPlot@(ROW['+intToStr(myRowIndex)+']);';
    globalRowData.append(literalRecycler,
                         literalRecycler^.newListLiteral(2)^
                               .append      (literalRecycler,sample.toNewLiteralForExport(literalRecycler),false)^
                               .appendString(literalRecycler,style.toString),false);
  end;

FUNCTION T_sampleRow.getFullClone: P_plotPrimitive;
  VAR sampleRow:P_sampleRow;
      row:T_dataRow;
  begin
    sample.cloneTo(row);
    new(sampleRow,create(row));
    sampleRow^.style:=style;
    sampleRow^.pseudoIndex:=pseudoIndex;
    result:=sampleRow;
  end;

PROCEDURE T_axisTrafo.prepare;
  VAR x0,x1:double;
  begin
    //t(x) = (x-worldRangeMin)/(worldRangeMax-worldRangeMin)*(screenRangeMax-screenRangeMin)+screenRangeMin;
    //     = x/(worldRangeMax-worldRangeMin)*(screenRangeMax-screenRangeMin) + screenRangeMin - worldRangeMin/(worldRangeMax-worldRangeMin)*(screenRangeMax-screenRangeMin);
    //       x*factor                                                        + offset
    if logscale then begin
      x0:=ln(rangeByAutosize[0])/ln(10);
      x1:=ln(rangeByAutosize[1])/ln(10);
    end else begin
      x0:=rangeByAutosize[0];
      x1:=rangeByAutosize[1];
    end;
    factor:=(screenRange[1]-screenRange[0])/(x1-x0);
    offset:=screenRange[0]-factor*x0;
    if logscale then factor:=factor/ln(10);
  end;

PROCEDURE T_axisTrafo.setLogScale(CONST value: boolean);
  begin
    logs:=value;
    if not(isValidMinimum(rangeByUser[0])) then rangeByUser[0]:=MIN_VALUE_FOR[value];
    if not(isValidMaximum(rangeByUser[1])) then rangeByUser[1]:=MAX_VALUE_FOR[value];
    if value then begin
      if rangeByUser[1]<rangeByUser[0]*10 then rangeByUser[1]:=rangeByUser[0]*10;
    end else begin
      if rangeByUser[1]<rangeByUser[0]+abs(rangeByUser[0])*0.00001 then rangeByUser[1]:=rangeByUser[0]+abs(rangeByUser[0])*0.00001;
    end;
  end;

PROCEDURE T_axisTrafo.setWorldMin(CONST value: double);
  begin
    if not(isValidMinimum(value)) then exit;
    rangeByUser[0]:=value;
  end;

PROCEDURE T_axisTrafo.setWorldMax(CONST value: double);
  begin
    if not(isValidMaximum(value)) then exit;
    rangeByUser[1]:=value;
  end;

PROCEDURE T_axisTrafo.setWorldRange(CONST x0, x1: double);
  VAR xc:double;
  begin
    if logscale then begin
      if x1<x0*10 then begin
        xc:=sqrt(x0*x1);
        rangeByAutosize[0]:=xc/sqrt(10);
        rangeByAutosize[1]:=xc*sqrt(10);
      end else begin
        rangeByAutosize[0]:=x0;
        rangeByAutosize[1]:=x1;
      end;
    end else begin
      if x1<=x0 then begin
        xc:=(x0+x1)*0.5;
        rangeByAutosize[0]:=xc-5E-9;
        rangeByAutosize[1]:=xc+5E-9;
      end else begin
        rangeByAutosize[0]:=x0;
        rangeByAutosize[1]:=x1;
      end;
    end;
    prepare;
  end;

PROCEDURE T_axisTrafo.setScreenMin(CONST value: longint);
  begin
    screenRange[0]:=value;
    prepare;
  end;

PROCEDURE T_axisTrafo.setScreenMax(CONST value: longint);
  begin
    screenRange[1]:=value;
    prepare;
  end;

FUNCTION T_axisTrafo.isValidMinimum(CONST x: double): boolean;
  begin
    result:=not(isNan(x) or isInfinite(x) or (x<MIN_VALUE_FOR[logscale]));
  end;

FUNCTION T_axisTrafo.isValidMaximum(CONST x: double): boolean;
  begin
    result:=not(isNan(x) or isInfinite(x) or (x>MAX_VALUE_FOR[logscale]));
  end;

PROCEDURE T_axisTrafo.reset;
  begin
    logs:=false;
    autoscale:=true;
    rangeByUser[0]:=-1;
    rangeByUser[1]:=1;
    rangeByAutosize:=rangeByUser;
    screenRange[0]:=0;
    screenRange[1]:=100;
    prepare;
  end;

FUNCTION T_axisTrafo.screenExtend: longint;
  begin
    result:=abs(screenRange[1]-screenRange[0]);
  end;

FUNCTION T_axisTrafo.apply(CONST x: double): double;
  begin
    if logscale then begin
      if x<MIN_VALUE_FOR[true]
      then   result:=Nan
      else   result:=ln(x)*factor+offset
    end else result:=   x *factor+offset;
  end;

FUNCTION T_axisTrafo.applyInverse(CONST x: double): double;
  begin
    if logscale then result:=exp((x-offset)/factor)
                else result:=    (x-offset)/factor;
  end;

PROCEDURE T_axisTrafo.zoom(CONST screenCoordinate: longint; CONST zoomFactor: double);
  VAR hold:double;
  begin
    if autoscale then begin
      autoscale:=false;
      rangeByUser:=rangeByAutosize;
    end;
    hold:=applyInverse(screenCoordinate);
    if logscale then begin
      hold    :=ln(hold);
      rangeByUser[0]:=ln(rangeByUser[0]);
      rangeByUser[1]:=ln(rangeByUser[1]);
    end;
    rangeByUser[0]:=(rangeByUser[0]-hold)*zoomFactor+hold;
    rangeByUser[1]:=(rangeByUser[1]-hold)*zoomFactor+hold;
    if logscale then begin
      rangeByUser[0]:=exp(rangeByUser[0]);
      rangeByUser[1]:=exp(rangeByUser[1]);
    end;
    rangeByAutosize:=rangeByUser;
    prepare;
  end;

PROCEDURE T_axisTrafo.pan(CONST screenDelta: longint);
  VAR worldDelta:double;
  begin
    if autoscale then begin
      autoscale:=false;
      rangeByUser:=rangeByAutosize;
    end;
    worldDelta:=screenDelta/factor;
    if logscale then begin
      worldDelta:=exp(worldDelta);
      rangeByUser[0]:=rangeByUser[0]*worldDelta;
      rangeByUser[1]:=rangeByUser[1]*worldDelta;
    end else begin
      rangeByUser[0]:=rangeByUser[0]+worldDelta;
      rangeByUser[1]:=rangeByUser[1]+worldDelta;
    end;
    rangeByAutosize:=rangeByUser;
    prepare;
  end;

FUNCTION T_axisTrafo.sampleIsInRange(CONST x: double): boolean;
  begin
    result:=not(isNan(x)) and (x>=rangeByAutosize[0]) and (x<=rangeByAutosize[1]);
  end;

FUNCTION T_axisTrafo.equals(CONST other:T_axisTrafo):boolean;
  begin
    result:=(autoscale = other.autoscale) and
            (logs=other.logs) and
            (rangeByUser[0]=other.rangeByUser[0]) and
            (rangeByUser[1]=other.rangeByUser[1]);
  end;

INITIALIZATION
  initCriticalSection(globalTextRenderingCs);
FINALIZATION
  doneCriticalSection(globalTextRenderingCs);

end.

