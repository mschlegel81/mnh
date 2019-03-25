UNIT plotMath;
INTERFACE
USES sysutils, math,
     Graphics, base64,
     myGenerics,
     myStringUtil,
     basicTypes,
     plotstyles,
     litVar;
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
      FUNCTION toMnhString:string;
  end;

  T_rowToPaint = array of record
    x,y:longint;
    valid:boolean;
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
  end;

  T_sampleRow = object
    style: T_style;
    sample: T_dataRow;
    CONSTRUCTOR create(CONST index: longint; CONST row:T_dataRow);
    DESTRUCTOR destroy;
    FUNCTION toPlotStatement(CONST firstRow:boolean):string;
  end;

  T_allSamples=array of T_sampleRow;
  T_scalingOptions=object
    preserveAspect  : boolean;
    relativeFontSize: double;
    autoscaleFactor : double;
    axisTrafo:array['x'..'y'] of T_axisTrafo;
    axisStyle:array['x'..'y'] of T_gridStyle;

    PROCEDURE setDefaults;
    PROCEDURE updateForPlot(CONST Canvas:TCanvas; CONST aimWidth,aimHeight:longint; CONST samples:T_allSamples; VAR grid:T_ticInfos);
    FUNCTION transformRow(CONST row:T_dataRow; CONST scalingFactor:byte; CONST subPixelDx,subPixelDy:double):T_rowToPaint;
    FUNCTION screenToReal(CONST x,y:integer):T_point;
    FUNCTION absoluteFontSize(CONST xRes,yRes:longint):longint;
    FUNCTION getOptionString:string;
    FUNCTION getOptionDiffString(CONST before:T_scalingOptions):string;
  end;

  T_customTextAnchor=(cta_topLeft   ,cta_top   ,cta_topRight   ,
                      cta_centerLeft,cta_center,cta_centerRight,
                      cta_bottomLeft,cta_bottom,cta_bottomRight);

  P_customText=^T_customText;
  T_customText=object
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
    FUNCTION clone:P_customText;
    DESTRUCTOR destroy;
    PROCEDURE renderText(CONST xRes,yRes:longint; CONST opt:T_scalingOptions; CONST target:TCanvas);
    PROCEDURE setAnchor(CONST s:string);
    PROCEDURE setForeground(CONST r,g,b:double);
    PROCEDURE setBackground(CONST r,g,b:double);
    FUNCTION toTextStatement:string;
  end;

VAR globalTextRenderingCs:TRTLCriticalSection;
IMPLEMENTATION
FUNCTION pointOf(CONST x,y:double):T_point;
  begin
    result[0]:=x;
    result[1]:=y;
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

FUNCTION T_dataRow.toMnhString:string;
  FUNCTION simplify(CONST num:double):P_numericLiteral;
    VAR inum:int64;
    begin
      if (num>=-9E18) and (num<9E18) then begin
        inum:=round(num);
        if num=inum then result:=newIntLiteral(inum)
                    else result:=newRealLiteral(num);
      end else result:=newRealLiteral(num);
    end;

  VAR i:longint=0;
      simple:boolean=true;
      dataList:P_listLiteral;
      compressedForm:string;
      dummyLocation:T_tokenLocation;
  begin
    while (i<alloc) and simple do begin simple:=simple and (dat[i,0]=i); inc(i); end;
    dataList:=newListLiteral(alloc);
    if simple
    then for i:=0 to alloc-1 do dataList^.append(simplify(dat[i,1]),false)
    else for i:=0 to alloc-1 do dataList^.append(newListLiteral(2)^.append(simplify(dat[i,0]),false)^.append(simplify(dat[i,1]),false),false);
    compressedForm:=escapeString(EncodeStringBase64(compressString(serialize(dataList,dummyLocation,nil),1)),es_javaStyle,simple)+'.base64decode.decompress.deserialize';
    result:=dataList^.toString();
    disposeLiteral(dataList);
    if length(compressedForm)<length(result) then result:=compressedForm;
  end;

CONSTRUCTOR T_customText.create(CONST x, y: double; CONST txt: T_arrayOfString);
  CONST BLACK:T_color=(0,0,0);
        WHITE:T_color=(255,255,255);
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

FUNCTION T_customText.clone:P_customText;
  VAR clonedText:T_arrayOfString;
      i:longint;
  begin
    setLength(clonedText,length(text));
    for i:=0 to length(clonedText)-1 do clonedText[i]:=text[i];
    new(result,create(p[0],p[1],clonedText));
    result^.fontName             :=fontName;
    result^.anchor               :=anchor;
    result^.fontSize             :=fontSize;
    result^.foreground           :=foreground;
    result^.background           :=background;
    result^.transparentBackground:=transparentBackground;
    result^.absolutePosition     :=absolutePosition;
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
    result:=round(relativeSize*sqrt(sqr(xRes)+sqr(yRes))/1000);
  end;

PROCEDURE T_customText.renderText(CONST xRes, yRes: longint; CONST opt: T_scalingOptions; CONST target: TCanvas);
  VAR tempRow:T_dataRow;
      toPaint:T_rowToPaint;
      x,y,
      i,k,
      textWidth,textHeight:longint;
      oldFont:TFont;
  begin
    if length(text)=0 then exit;

    if absolutePosition then begin
      try
        x:=     round(p[0]*xRes);
        y:=yRes-round(p[1]*yRes);
      except
        exit;
      end;
    end else begin
      tempRow.init(1);
      tempRow[0]:=p;
      toPaint:=opt.transformRow(tempRow,1,0,0);
      x:=toPaint[0].x;
      y:=toPaint[0].y;
      if not(toPaint[0].valid) then exit;
      tempRow.free;
    end;
    enterCriticalSection(globalTextRenderingCs);
    try
      oldFont     :=target.Font;

      if (fontName<>'') and (fontName<>target.Font.name) then target.Font.name:=fontName;
      if isNan(fontSize) then target.Font.size:=absFontSize(xRes,yRes,opt.relativeFontSize)
                         else target.Font.size:=absFontSize(xRes,yRes,            fontSize);
      target.Font.color:=foreground;
      if transparentBackground then target.Brush.style:=bsClear
      else begin
        target.Brush.style:=bsSolid;
        target.Brush.color:=background;
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
      if (x<=xRes) and (x+textWidth>=0) and (y<=yRes) and (y+textHeight>=0) then begin
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

PROCEDURE T_customText.setForeground(CONST r, g, b: double);
  begin
    foreground[cc_red  ]:=round(255*max(0,min(1,r)));
    foreground[cc_green]:=round(255*max(0,min(1,g)));
    foreground[cc_blue ]:=round(255*max(0,min(1,b)));
  end;

PROCEDURE T_customText.setBackground(CONST r, g, b: double);
  begin
    transparentBackground:=false;
    background[cc_red  ]:=round(255*max(0,min(1,r)));
    background[cc_green]:=round(255*max(0,min(1,g)));
    background[cc_blue ]:=round(255*max(0,min(1,b)));
  end;

FUNCTION T_customText.toTextStatement:string;
  CONST anchorText:array[T_customTextAnchor] of string=('"TL"','"T"','"TR"','"L"','"C"','"R"','"BL"','"B"','"BR"');
  VAR dummy:boolean;
  begin
    if absolutePosition
    then result:='drawTextAbsolute('
    else result:='drawText(';
    result+=numToString(p[0])+','+numToString(p[1])+','+
            escapeString(join(text,C_lineBreakChar),es_pickShortest,dummy);
    if isNan(fontSize) then result+=',Nan' else result+=','+numToString(fontSize);
    result+=','+anchorText[anchor];
    result+=','+escapeString(fontName,es_pickShortest,dummy);
    result+=',['+numToString(foreground[cc_red  ]*255)+','+
                 numToString(foreground[cc_green]*255)+','+
                 numToString(foreground[cc_blue ]*255)+']';
    if not(transparentBackground) then
    result+=',['+numToString(background[cc_red  ]*255)+','+
                 numToString(background[cc_green]*255)+','+
                 numToString(background[cc_blue ]*255)+']';
    result+=');';
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

PROCEDURE T_scalingOptions.updateForPlot(CONST Canvas: TCanvas; CONST aimWidth,aimHeight: longint; CONST samples: T_allSamples; VAR grid: T_ticInfos);

  VAR validSampleCount:longint=0;
  FUNCTION getSamplesBoundingBox:T_boundingBox;
    VAR axis:char;
        row:T_sampleRow;
        point:T_point;
        x,y:double;
        i:longint;
    begin
      for axis:='x' to 'y' do
      if axisTrafo[axis].autoscale then begin
        result[axis,0]:= infinity;
        result[axis,1]:=-infinity;
      end else begin
        result[axis,0]:=axisTrafo[axis].rangeByUser[0];
        result[axis,1]:=axisTrafo[axis].rangeByUser[1];
      end;
      for row in samples do for i:=0 to row.sample.size-1 do begin
        point:=row.sample[i];
        x:=point[0]; if (x<MIN_VALUE_FOR[axisTrafo['x'].logs]) or (x>MAX_VALUE_FOR[axisTrafo['x'].logs]) then continue;
        y:=point[1]; if (y<MIN_VALUE_FOR[axisTrafo['y'].logs]) or (x>MAX_VALUE_FOR[axisTrafo['y'].logs]) then continue;
        if not(axisTrafo['x'].autoscale) and not(axisTrafo['x'].sampleIsInRange(x)) or
           not(axisTrafo['y'].autoscale) and not(axisTrafo['y'].sampleIsInRange(y)) then continue;
        if (x<result['x',0]) then result['x',0]:=x;
        if (x>result['x',1]) then result['x',1]:=x;
        if (y<result['y',0]) then result['y',0]:=y;
        if (y>result['y',1]) then result['y',1]:=y;
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
      VAR screenPos: double;
      begin
        if axisTrafo[axis].sampleIsInRange(realPos) then begin
          screenPos:=axisTrafo[axis].apply(realPos);
          setLength(grid[axis], length(grid[axis])+1);
          with grid[axis][length(grid[axis])-1] do begin
            pos:=screenPos;
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
        except
          i:=1;
        end;
        initLinearTics(i,10,1);
        if majorTicCount>10 then begin
          initLinearTics(i,50,10);
        end;
      end;
    end;

  VAR ticSampleText:string;
  FUNCTION updateBorders:boolean;
    VAR newX0,newY0:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      try
        if gse_tics in axisStyle['y']
        then newX0:=Canvas.Font.GetTextWidth(ticSampleText)+5
        else newX0:=0;
        if gse_tics in axisStyle['x']
        then newY0:=aimHeight-Canvas.Font.GetTextHeight(ticSampleText)-5
        else newY0:=aimHeight;
      finally
        leaveCriticalSection(globalTextRenderingCs);
      end;

      result:=(axisTrafo['x'].screenRange[0]<newX0-20) or
              (axisTrafo['x'].screenRange[0]>newX0   ) or
              (axisTrafo['y'].screenRange[0]>newY0+20) or
              (axisTrafo['y'].screenRange[0]<newY0);

      axisTrafo['x'].screenRange[0]:=newX0;
      axisTrafo['x'].screenRange[1]:=aimWidth;
      axisTrafo['y'].screenRange[0]:=newY0;
      axisTrafo['y'].screenRange[1]:=0;
      axisTrafo['x'].prepare;
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

  VAR axis:char;
  begin
    enterCriticalSection(globalTextRenderingCs);
    try
      Canvas.Font.size:=absoluteFontSize(aimWidth,aimHeight);
    finally
      leaveCriticalSection(globalTextRenderingCs);
    end;
    ticSampleText:='.0E12';
    updateBorders;
    prepareRanges;
    for axis:='x' to 'y' do if axisStyle[axis]<>[] then initTics(axis);
    ticSampleText:=longestTic;
    if updateBorders then begin
      prepareRanges;
      for axis:='x' to 'y' do if axisStyle[axis]<>[] then initTics(axis);
    end;
  end;

FUNCTION T_scalingOptions.transformRow(CONST row: T_dataRow; CONST scalingFactor:byte; CONST subPixelDx,subPixelDy:double): T_rowToPaint;
  VAR i:longint;
      tx,ty:double;
  begin
    setLength(result,row.size);
    for i:=0 to row.size-1 do begin
      tx:=axisTrafo['x'].apply(row[i][0])*scalingFactor+subPixelDx;
      result[i].valid:=not(isNan(tx)) and (tx>=-2147483648) and (tx<=2147483647);
      if not(result[i].valid) then continue;
      ty:=axisTrafo['y'].apply(row[i][1])*scalingFactor+subPixelDy;
      result[i].valid:=not(isNan(ty)) and (ty>=-2147483648) and (ty<=2147483647);
      if not(result[i].valid) then continue;
      result[i].x:=round(tx);
      result[i].y:=round(ty);
    end;
  end;

FUNCTION T_scalingOptions.screenToReal(CONST x, y: integer): T_point;
  begin
    result[0]:=axisTrafo['x'].applyInverse(x);
    result[1]:=axisTrafo['y'].applyInverse(y);
  end;

FUNCTION T_scalingOptions.absoluteFontSize(CONST xRes, yRes: longint): longint;
  begin
    result:=absFontSize(xRes,yRes,relativeFontSize);
  end;

FUNCTION T_scalingOptions.getOptionString:string;
  VAR defaults:T_scalingOptions;
  begin
    defaults.setDefaults;
    result:=getOptionDiffString(defaults);
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

CONSTRUCTOR T_sampleRow.create(CONST index: longint; CONST row: T_dataRow);
  begin
    style.create(index);
    sample:=row;
  end;

DESTRUCTOR T_sampleRow.destroy;
  begin
    style.destroy;
    sample.free;
  end;

FUNCTION T_sampleRow.toPlotStatement(CONST firstRow:boolean):string;
  VAR dummy:boolean;
  begin
    if firstRow then result:='plot(' else result:='addPlot(';
    result+=sample.toMnhString+','+escapeString(style.toString,es_pickShortest,dummy)+');';
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

INITIALIZATION
  initCriticalSection(globalTextRenderingCs);
FINALIZATION
  doneCriticalSection(globalTextRenderingCs);

end.
