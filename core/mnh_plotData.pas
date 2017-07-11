UNIT mnh_plotData;
INTERFACE
USES mnh_basicTypes, sysutils, math, mnh_constants,Interfaces, Classes, ExtCtrls, Graphics, types;
TYPE
  T_plotStyle=(ps_none,
               ps_straight,
               ps_stepLeft,
               ps_stepRight,
               ps_filled,
               ps_bar,
               ps_box,
               ps_dot,
               ps_plus,
               ps_cross,
               ps_impulse);
  T_plotStyles=set of T_plotStyle;

CONST
  C_lineStyles:T_plotStyles=[ps_straight..ps_stepRight];
  C_styleName: array[T_plotStyle] of array[0..1] of string=
     {ps_none,     }  (('',''),
     {ps_straight, }   ('line'     , 'l'),
     {ps_stepLeft, }   ('stepLeft' , '' ),
     {ps_stepRight,}   ('stepRight', '' ),
     {ps_filled,   }   ('fill'     , 'f'),
     {ps_bar,      }   ('bar'      , '' ),
     {ps_box,      }   ('box'      , '' ),
     {ps_dot,      }   ('dot'      , '.'),
     {ps_plus,     }   ('plus'     , '+'),
     {ps_cross,    }   ('cross'    , 'x'),
     {ps_impulse); }   ('impulse'  , 'i'));

TYPE
  T_colorChannel = (cc_red, cc_green, cc_blue);
  T_color = array[T_colorChannel] of byte;
  T_scaleAndColor = record
    lineWidth:longint;
    symbolRadius:longint;
    symbolWidth:longint;
    lineColor:longint;
    solidColor:longint;
  end;
  T_point = array[0..1] of double;
  T_dataRow = array of T_point;
  T_style = object
    style: T_plotStyles;
    color: T_color;
    styleModifier: double;
    CONSTRUCTOR create(CONST index: longint);
    DESTRUCTOR destroy;
    PROCEDURE parseStyle(CONST styleString: ansistring);
    FUNCTION toString:ansistring;
    FUNCTION getLineScaleAndColor(CONST scalingFactor:double):T_scaleAndColor;
  end;

  T_boundingBox = array['x'..'y', 0..1] of double;
  T_sampleRow = object
    style: T_style;
    sample: T_dataRow;
    CONSTRUCTOR create(CONST index: longint; CONST row:T_dataRow);
    PROCEDURE getBoundingBox(CONST logX, logY, autoscaleX, autoscaleY: boolean; VAR box: T_boundingBox);
    DESTRUCTOR destroy;
  end;

CONST
  C_tics = 1;
  C_grid = 2;
  C_finerGrid = C_grid+4; //=6
  C_ticsAndGrid = C_tics+C_grid; //=3
  C_ticsAndFinerGrid = C_tics+C_finerGrid; //=7

TYPE
  T_scalingOptions=record
    range: array['x'..'y', 0..1] of double;
    axisStyle: array['x'..'y'] of byte;
    autoscale, logscale: array['x'..'y'] of boolean;
    preserveAspect: boolean;
    relativeFontSize: double;
    autoscaleFactor: double;
  end;

  P_plot =^T_plot;
  T_plot = object
    TYPE T_ticInfo = record
      pos: double;
      major: boolean;
      txt: ansistring;
    end;
    private
      cs: TRTLCriticalSection;
      screenWidth, screenHeight: longint;
      xOffset, yOffset: longint;
      scaledXOffset,scaledYOffset:double;

      tic: array['x'..'y'] of array of T_ticInfo;
      scalingOptions:T_scalingOptions;
      row: array of T_sampleRow;
      renderImage:TImage;

      FUNCTION olx(CONST x: double): double;
      FUNCTION oex(CONST x: double): double;
      FUNCTION oly(CONST y: double): double;
      FUNCTION oey(CONST y: double): double;
      PROCEDURE setScalingOptions(CONST value:T_scalingOptions);
      FUNCTION  getScalingOptions:T_scalingOptions;
      FUNCTION longtestYTic: ansistring;
      PROCEDURE setScreenSize(CONST width, height: longint; CONST skipTics:boolean=false);
    public
      FUNCTION olxy(CONST xy:T_dataRow):T_dataRow;
      PROPERTY options:T_scalingOptions read getScalingOptions write setScalingOptions;

      CONSTRUCTOR createWithDefaults;
      PROCEDURE setDefaults;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE addRow(CONST styleOptions: string; CONST rowData:T_dataRow);
      PROCEDURE removeRows(CONST numberOfRowsToRemove:longint);

      FUNCTION realToScreen(CONST p: T_point): T_point;
      FUNCTION realToScreen(CONST x, y: double): T_point;
      FUNCTION realToScreen(CONST axis: char; CONST p: double): double;
      FUNCTION screenToReal(CONST x, y: longint): T_point;
      FUNCTION isSampleValid(CONST sample: T_point): boolean;

      PROCEDURE zoomOnPoint(CONST pixelX, pixelY: longint; CONST factor: double; VAR plotImage: TImage);
      PROCEDURE panByPixels(CONST pixelDX, pixelDY: longint; VAR plotImage: TImage);

      PROCEDURE renderInternally(CONST width,height:longint);
      PROCEDURE renderPlot(VAR plotImage: TImage);
      PROCEDURE renderPlot(VAR plotImage: TImage; CONST supersampling:longint);
      PROCEDURE renderToFile(CONST fileName:string; CONST width,height,supersampling:longint);
      FUNCTION renderToString(CONST width,height,supersampling:longint):ansistring;

      PROCEDURE CopyFrom(VAR p:T_plot);
  end;

IMPLEMENTATION
VAR MAJOR_TIC_STYLE, MINOR_TIC_STYLE:T_style;

CONSTRUCTOR T_sampleRow.create(CONST index: longint; CONST row:T_dataRow);
  VAR i:longint;
  begin
    style.create(index);
    setLength(sample, length(row));
    for i:=0 to length(row)-1 do sample[i]:=row[i];
  end;

PROCEDURE T_sampleRow.getBoundingBox(CONST logX, logY, autoscaleX, autoscaleY: boolean; VAR box: T_boundingBox);
  VAR i:longint;
      x,y:double;
  begin
    for i:=0 to length(sample)-1 do begin
      x:=sample[i,0]; if (x<=1E-100) and logX then x:=Nan;
      y:=sample[i,1]; if (y<=1E-100) and logY then y:=Nan;
      if isNan(x) or (isInfinite(x)) or not(autoscaleX) and ((x<box['x',0]) or (x>box['x',1])) or
         isNan(y) or (isInfinite(y)) or not(autoscaleY) and ((y<box['y',0]) or (y>box['y',1])) then continue;
      if (x<box['x',0]) then box['x',0]:=x;
      if (x>box['x',1]) then box['x',1]:=x;
      if (y<box['y',0]) then box['y',0]:=y;
      if (y>box['y',1]) then box['y',1]:=y;
    end;
  end;

DESTRUCTOR T_sampleRow.destroy;
  begin
    setLength(sample, 0);
  end;

CONST
  C_defaultColor: array[0..5] of record
      name: string;
      color: array[T_colorChannel] of byte
    end =
    ((name: 'black' ; color:(  0,  0,  0)),
     (name: 'red'   ; color:(255,  0,  0)),
     (name: 'blue'  ; color:(  0,  0,255)),
     (name: 'green' ; color:(  0,128,  0)),
     (name: 'purple'; color:(192,  0,192)),
     (name: 'orange'; color:(255, 96,  0)));

CONSTRUCTOR T_style.create(CONST index: longint);
  begin
    style:=[ps_straight];
    color:=C_defaultColor[index mod length(C_defaultColor)].color;
    styleModifier:=1;
  end;

DESTRUCTOR T_style.destroy;
  begin
  end;

PROCEDURE T_style.parseStyle(CONST styleString: ansistring);
  FUNCTION parseColorOption(colorOption: shortString; OUT r, g, b: byte): boolean;
    PROCEDURE HSV2RGB(H,S,V: single; OUT r,g,b: byte);
      VAR hi,p,q,t: byte;
      begin
        while H<0 do H:=H+1;
        while H>1 do H:=H-1;
        hi:=trunc(H*6);
        H:=H*6-hi;
        V:=V*255;
        p:=round(V*(1-S));
        q:=round(V*(1-S*H));
        t:=round(V*(1-S*(1-H)));
        case hi of
          0, 6: begin r:=round(V); g:=t; b:=p; end;
          1:    begin r:=q; g:=round(V); b:=p; end;
          2:    begin r:=p; g:=round(V); b:=t; end;
          3:    begin r:=p; g:=q; b:=round(V); end;
          4:    begin r:=t; g:=p; b:=round(V); end;
          5:    begin r:=round(V); g:=p; b:=q; end;
        end;
      end;

    VAR rStr: string = '';
        gStr: string = '';
        bStr: string = '';
        i: longint;
        isHSV: boolean;
    begin
      result:=false;
      for i:=0 to length(C_defaultColor)-1 do
      if colorOption = C_defaultColor[i].name then begin
          color:=C_defaultColor[i].color;
          result:=true;
        end;
      if not(result) and ((copy(colorOption, 1, 3) = 'RGB') or (copy(colorOption, 1, 3) = 'HSV')) then begin
        isHSV:=(copy(colorOption, 1, 3) = 'HSV');
        colorOption:=copy(colorOption, 4, length(colorOption)-3);
        i:=pos(',', colorOption);
        if i>0 then begin
          rStr:=copy(colorOption, 1, i-1);
          colorOption:=copy(colorOption, i+1, length(colorOption));
          i:=pos(',', colorOption);
        end;
        if i>0 then begin
          gStr:=copy(colorOption, 1, i-1);
          colorOption:=copy(colorOption, i+1, length(colorOption));
          i:=pos(',', colorOption);
        end;
        if i>0 then begin
          bStr:=copy(colorOption, 1, i-1);
          colorOption:=copy(colorOption, i+1, length(colorOption));
          i:=pos(',', colorOption);
        end else bStr:=colorOption;
        if (rStr<>'') and (gStr<>'') and (bStr<>'') then begin
          if isHSV then HSV2RGB(strToFloatDef(rStr,0),
                    max(0,min(1,strToFloatDef(gStr,0))),
                    max(0,min(1,strToFloatDef(bStr,0))), r, g, b)
          else begin
            r:=round(255*max(0, min(1, strToFloatDef(rStr, 0))));
            g:=round(255*max(0, min(1, strToFloatDef(gStr, 0))));
            b:=round(255*max(0, min(1, strToFloatDef(bStr, 0))));
          end;
          result:=true;
        end;
      end;
      if not(result) and (copy(colorOption, 1, 3) = 'HUE') then begin
        colorOption:=copy(colorOption, 4, length(colorOption)-3);
        HSV2RGB(strToFloatDef(colorOption, 0), 1, 1, r, g, b);
        result:=true;
      end;
      if not(result) and (copy(colorOption, 1, 4) = 'GREY') then begin
        colorOption:=copy(colorOption, 5, length(colorOption)-4);
        r:=round(255*max(0, min(1, strToFloatDef(colorOption, 0))));
        g:=r;
        b:=r;
        result:=true;
      end;
    end;

  VAR part, options: ansistring;
      sp: longint;
      size: T_myFloat;
      mightBeColor: boolean;
      ps:T_plotStyle;
  begin
    style:=[];
    options:=trim(styleString);
    repeat
      sp:=pos(' ', options);
      if sp<=0 then begin
        part:=options;
        options:='';
      end else begin
        part:=copy(options, 1, sp-1);
        options:=trim(copy(options, sp+1, length(options)));
      end;
      mightBeColor:=true;
      if part<>'.' then begin
      size:=strToFloatDef(part, Nan);
        if not(isNan(size)) then begin
          styleModifier:=size;
          mightBeColor:=false;
        end;
      end;
      for ps in T_plotStyle do
      if (part = C_styleName[ps,0]) or (part = C_styleName[ps,1]) then begin
        if ps in C_lineStyles
        then style:=style-C_lineStyles+[ps]
        else include(style,ps);
        mightBeColor:=false;
      end;
      if mightBeColor then parseColorOption(part, color[cc_red], color[cc_green], color[cc_blue]);
    until options = '';
    if style = [] then style:=[ps_straight];
  end;

FUNCTION T_style.toString:ansistring;
  VAR s:T_plotStyle;
  begin
    result:=floatToStr(styleModifier)+' ';
    for s in style do result:=result+C_styleName[s,0]+' ';
    result:=result+'RGB'+floatToStr(color[cc_red]/255)
                  +','  +floatToStr(color[cc_green]/255)
                  +','  +floatToStr(color[cc_blue]);
  end;

FUNCTION T_style.getLineScaleAndColor(CONST scalingFactor:double):T_scaleAndColor;
  FUNCTION toByte(CONST d:double):byte;
    begin
      if d>255 then result:=255 else if d<0 then result:=0 else result:=round(d);
    end;

  VAR ideal:double;
  begin
    result.solidColor:=color [cc_red] or (color [cc_green] shl 8) or (color [cc_blue] shl 16);
    //line width
    ideal:=styleModifier*scalingFactor;
    if ideal>1 then begin
      result.lineWidth:=round(ideal);
      result.lineColor:=result.solidColor;
    end else begin
      result.lineWidth:=1;
      result.lineColor:=toByte(color[cc_red  ]*ideal + 255*(1-ideal))
                    or (toByte(color[cc_green]*ideal + 255*(1-ideal)) shl  8)
                    or (toByte(color[cc_blue ]*ideal + 255*(1-ideal)) shl 16);
    end;
    result.symbolWidth :=round(scalingFactor*3        *styleModifier);
    result.symbolRadius:=round(scalingFactor*3/sqrt(2)*styleModifier);
  end;

CONSTRUCTOR T_plot.createWithDefaults;
  begin
    system.initCriticalSection(cs);
    renderImage:=nil;
    setDefaults;
  end;

PROCEDURE T_plot.setDefaults;
  VAR axis: char;
  begin
    system.enterCriticalSection(cs);
    screenWidth:=200;
    screenHeight:=200;
    xOffset:=20;
    yOffset:=200-20;
    with scalingOptions do begin
      for axis:='x' to 'y' do begin
        range[axis, 0]:=-1.5;
        range[axis, 1]:=1.5;
        logscale[axis]:=false;
        axisStyle[axis]:=C_ticsAndFinerGrid;
        autoscale[axis]:=true;
      end;
      preserveAspect:=true;
      relativeFontSize:=10;
      autoscaleFactor:=1;
    end;
    clear;
    system.leaveCriticalSection(cs);
  end;

DESTRUCTOR T_plot.destroy;
  begin
    system.enterCriticalSection(cs);
    clear;
    if renderImage<>nil then renderImage.destroy;
    system.leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

PROCEDURE T_plot.clear;
  VAR i: longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(row)-1 do row[i].destroy;
    setLength(row, 0);
    system.leaveCriticalSection(cs);
  end;

FUNCTION niceText(CONST value, scale: longint): string;
  CONST suf: array[0..3] of string = ('', '0', '00', '000');
  begin
    if value = 0 then exit('0');
    if (scale>=-3) and (scale<0) then begin
      result:=intToStr(value);
      while length(result)<-scale do result:='0'+result;
      exit(copy(result, 1, length(result)+scale)+'.'+copy(result, length(result)+1+scale, -scale));
    end else if (scale>=0) and (scale<3)
    then exit(intToStr(value)+suf[scale])
    else exit(intToStr(value)+'E'+intToStr(scale));
  end;

PROCEDURE T_plot.setScreenSize(CONST width, height: longint; CONST skipTics:boolean=false);
  PROCEDURE getRanges;
    VAR axis: char;
        i: longint;
        boundingBox: T_boundingBox;
        tmp:double;

    PROCEDURE autoscaleByX;
      VAR center, extend: double;
      begin
        with scalingOptions do begin
          center:=(range['y',1]+range['y',0])*0.5;
          extend:=(range['x',1]-range['x',0])/(screenWidth-xOffset)*yOffset*0.5;
          range['y',0]:=center-extend;
          range['y',1]:=center+extend;
        end;
      end;

    PROCEDURE autoscaleByY;
      VAR center, extend: double;
      begin
        with scalingOptions do begin
          center:=(range['x',1]+range['x',0])*0.5;
          extend:=(range['y',1]-range['y',0])/yOffset*(screenWidth-xOffset)*0.5;
          range['x', 0]:=center-extend;
          range['x', 1]:=center+extend;
        end;
      end;

    PROCEDURE harmonizeScale(CONST xStretch,yStretch:double);
      VAR center, extend: double;
          xCorrection,yCorrection:double;
      begin
        if abs(xStretch/yStretch-1)>0.01 then begin
          xCorrection:=sqrt(yStretch/xStretch);
          yCorrection:=1/xCorrection;
          with scalingOptions do begin
            center:=(range['x',1]+range['x',0])*0.5;
            extend:=(range['x',1]-range['x',0])*0.5;
            range['x', 0]:=center-extend*xCorrection;
            range['x', 1]:=center+extend*xCorrection;
            center:=(range['y',1]+range['y',0])*0.5;
            extend:=(range['y',1]-range['y',0])*0.5;
            range['y', 0]:=center-extend*yCorrection;
            range['y', 1]:=center+extend*yCorrection;
          end;
        end;
      end;

    begin with scalingOptions do begin
      //Determine current bounding box:-----------------------------------------------------------
      for axis:='x' to 'y' do
      if autoscale[axis] then begin
        boundingBox[axis,0]:= infinity;
        boundingBox[axis,1]:=-infinity;
      end else begin
        boundingBox[axis,0]:=oex(range[axis,0]);
        boundingBox[axis,1]:=oex(range[axis,1]);
      end;
      for i:=0 to length(row)-1 do row[i].getBoundingBox(logscale['x'], logscale['y'],autoscale['x'],autoscale['y'],boundingBox);
      for axis:='x' to 'y' do if autoscale[axis] then for i:=0 to 1 do begin
        if isNan(boundingBox[axis,i]) or isInfinite(boundingBox[axis,i]) then boundingBox[axis,i]:=         range[axis,i]
        else if logscale[axis]                                           then boundingBox[axis,i]:=ln(boundingBox[axis,i])/ln(10);
        range[axis,i]:=boundingBox[axis,i];
      end;
      if autoscaleFactor<>1 then for axis:='x' to 'y' do if autoscale[axis] then begin
        tmp:=(range[axis,1]-range[axis,0])*(1-autoscaleFactor);
        range[axis,0]:=range[axis,0]-tmp;
        range[axis,1]:=range[axis,1]+tmp;
      end;
      //-----------------------------------------------------------:Determine current bounding box
      //Adapt bounding box to respect preservation of aspect ratio:-------------------------------
      if preserveAspect and (logscale['x'] = logscale['y']) then begin
        if (autoscale['x'] or autoscale['y']) then begin
          if autoscale['x'] then begin
            if autoscale['y'] then begin
              if (range['x',1]-range['x',0])/(screenWidth-xOffset)>
                 (range['y',1]-range['y',0])/yOffset
              then autoscaleByX
              else autoscaleByY;
            end else autoscaleByX;
          end else if autoscale['y'] then autoscaleByY;
        end else harmonizeScale((range['x',1]-range['x',0])/(screenWidth-xOffset),
                                (range['y',1]-range['y',0])/yOffset);
      end;
      //-------------------------------:Adapt bounding box to respect preservation of aspect ratio
      for axis:='x' to 'y' do begin
        if range[axis,1]<range[axis,0] then begin
          tmp:=range[axis,0];
          range[axis,0]:=range[axis,1];
          range[axis,1]:=tmp;
        end;
        if range[axis,1]<range[axis,0]+2E-60 then begin
          tmp:=(range[axis,0]+range[axis,1])*0.5;
          range[axis,0]:=tmp-1E-60;
          range[axis,1]:=tmp+1E-60;
        end;
      end;
    end; end;

  PROCEDURE initTics(CONST axis: char);
    FUNCTION isOnScreen(CONST screenPos:double):boolean;
      begin
        result:=not(isNan(screenPos)) and
                not(isInfinite(screenPos)) and
                not(screenPos<0) and
                not((axis = 'y') and (screenPos>screenHeight)) and
                not((axis = 'x') and (screenPos>screenWidth ));
      end;

    PROCEDURE addTic(CONST realPos: double; CONST realTxt: ansistring; CONST isMajorTic: boolean);
      VAR screenPos: double;
      begin
        screenPos:=realToScreen(axis, realPos);
        if isOnScreen(screenPos) then begin
          setLength(tic[axis], length(tic[axis])+1);
          with tic[axis][length(tic[axis])-1] do begin
            pos:=screenPos;
            major:=isMajorTic;
            if major then txt:=realTxt
                     else txt:='';
          end;
        end;
      end;

    FUNCTION pot10(y: int64): T_myFloat; inline;
      VAR p10: T_myFloat;
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
        setLength(tic[axis], 0);
        with scalingOptions do
        for j:=floor(range[axis,0]*pot10(-power)) to
                ceil(range[axis,1]*pot10(-power)) do
        if j mod minorTicRest = 0 then addTic(pot10(power)*j, niceText(j div 10, power+1), (j mod majorTicRest) = 0);
      end;

    PROCEDURE initLogTics(CONST ppot:longint);
      VAR i:longint;
          j:byte;
      begin with scalingOptions do begin
        setLength(tic[axis], 0);
        if ppot=-1 then begin
          for i:=floor(range[axis, 0]) to ceil(range[axis, 1]) do for j:=1 to 9 do
            addTic(pot10(i)*j,niceText(j,i),true);
        end else if ppot=0 then begin
          for i:=floor(range[axis, 0]) to ceil(range[axis, 1]) do for j:=1 to 9 do
            addTic(pot10(i)*j,niceText(j,i),j in [1,2,5]);
        end else if ppot=1 then begin
          for i:=floor(range[axis, 0]) to ceil(range[axis, 1]) do for j:=1 to 9 do
            addTic(pot10(i)*j,niceText(j,i),j=1);
        end else begin
          for i:=floor(range[axis, 0]) to ceil(range[axis, 1]) do
            if (i mod (ppot-1)=0) then addTic(pot10(i),niceText(1,i),i mod ppot=0);
        end;
      end; end;

    VAR i:longint;
        j:byte;
        k:longint;
        logRange:double;
    begin with scalingOptions do begin
      if logscale[axis] then begin
        //Tics: ... 0.5 0.6 0.7 0.8 0.9 1 2 3 4 5 ...
        k:=0;
        for i:=floor(range[axis, 0]) to ceil(range[axis, 1]) do for j:=1 to 9 do
        if isOnScreen(realToScreen(axis,pot10(i)*j)) then inc(k);
        if k<15 then begin
          initLogTics(-1);
          exit;
        end;
        //Tics: ... 0.5 1 2 5 10 20 50 ...
        k:=0;
        for i:=floor(range[axis, 0]) to ceil(range[axis, 1]) do for j:=1 to 5 do
        if (j in [1,2,5]) and isOnScreen(realToScreen(axis,pot10(i)*j)) then inc(k);
        if k<15 then begin
          initLogTics(0);
          exit;
        end;
        //Tics: ... 0.01 0.1 1 10 100 ...
        for j:=1 to 60 do begin
          k:=0;
          for i:=floor(range[axis, 0]) to ceil(range[axis, 1]) do
          if (i mod j = 0) and isOnScreen(realToScreen(axis,pot10(i))) then inc(k);
          if k<15 then begin
            initLogTics(j);
            exit;
          end;
        end;
      end else begin
        if (preserveAspect)
        then logRange:=0.5*(log10(range['x',1]-range['x',0])
                           +log10(range['y',1]-range['y',0]))
        else logRange:=log10(range[axis,1]-range[axis,0]);
        try
          i:=round(logRange-1.8);
        except
          i:=1;
        end;
        initLinearTics(i,10,1);
      end;
    end; end;

  begin
    system.enterCriticalSection(cs);
    screenWidth:=width;
    screenHeight:=height;
    if (scalingOptions.axisStyle['y'] and C_tics)=0 then xOffset:=0;
    if (scalingOptions.axisStyle['x'] and C_tics)=0 then yOffset:=height;
    getRanges;
    if not(skipTics) then begin
      initTics('x');
      initTics('y');
    end;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_plot.longtestYTic: ansistring;
  VAR i: longint;
  begin
    result:='';
    for i:=0 to length(tic['y'])-1 do with tic['y', i] do
    if length(txt)>length(result) then result:=txt;
    if result = '' then result:='.0E';
  end;

PROCEDURE T_plot.addRow(CONST styleOptions: string; CONST rowData: T_dataRow);
  VAR index:longint;
  begin
    system.enterCriticalSection(cs);
    index:=length(row);
    setLength(row, index+1);
    row[index].create(index,rowData);
    if trim(styleOptions)<>'' then row[index].style.parseStyle(styleOptions);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.removeRows(CONST numberOfRowsToRemove:longint);
  VAR i0,i:longint;
  begin
    if numberOfRowsToRemove<=0 then exit;
    system.enterCriticalSection(cs);
    i0:=length(row)-numberOfRowsToRemove; if i0<0 then i0:=0;
    for i:=i0 to length(row)-1 do row[i].destroy;
    setLength(row,i0);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_plot.realToScreen(CONST p: T_point): T_point;
  begin with scalingOptions do begin
    result[0]:=(olx(p[0])-range['x',0])/(range['x',1]-range['x',0])* (screenWidth-xOffset)+xOffset;
    result[1]:=(oly(p[1])-range['y',0])/(range['y',1]-range['y',0])* (-yOffset)+yOffset;
  end; end;

FUNCTION T_plot.realToScreen(CONST x, y: double): T_point;
  begin with scalingOptions do begin
    result[0]:=(olx(x)-range['x',0])/(range['x',1]-range['x',0])* (screenWidth-xOffset)+xOffset;
    result[1]:=(oly(y)-range['y',0])/(range['y',1]-range['y',0])* (-yOffset)+yOffset;
  end; end;

FUNCTION T_plot.screenToReal(CONST x, y: longint): T_point;
  begin with scalingOptions do begin
    result[0]:=oex((x-scaledXOffset)/(screenWidth-scaledXOffset)*(range['x',1]-range['x',0])+range['x',0]);
    result[1]:=oey((y-scaledYOffset)/(-scaledYOffset)           *(range['y',1]-range['y',0])+range['y',0]);
  end; end;

FUNCTION T_plot.realToScreen(CONST axis: char; CONST p: double): double;
  begin
    if axis = 'x' then result:=realToScreen(p,1)[0]
                  else result:=realToScreen(1,p)[1];
  end;

FUNCTION T_plot.olx(CONST x: double): double;
  begin
    if scalingOptions.logscale['x'] then begin
      if x<1E-323 then exit(Nan);
      result:=ln(x)/ln(10);
      if isNan(result) or isInfinite(result) then result:=Nan;
    end else result:=x;
  end;

FUNCTION T_plot.oex(CONST x: double): double;
  begin
    if scalingOptions.logscale['x'] then result:=exp(x*ln(10))
                                    else result:=x;
  end;

FUNCTION T_plot.oly(CONST y: double): double;
  begin
    if scalingOptions.logscale['y'] then begin
      if y<1E-323 then exit(Nan);
      result:=ln(y)/ln(10);
      if isNan(result) or isInfinite(result) then result:=Nan;
    end else result:=y;
  end;

FUNCTION T_plot.oey(CONST y: double): double;
  begin
    if scalingOptions.logscale['y'] then result:=exp(y*ln(10))
                                    else result:=y;
  end;

FUNCTION T_plot.olxy(CONST xy:T_dataRow):T_dataRow;
  VAR cAxis:char;
      iAxis:longint;
      i:longint;
  begin
    setLength(result,length(xy));
    for cAxis:='x' to 'y' do begin
      iAxis:=ord(cAxis)-ord('x');
      if scalingOptions.logscale[cAxis] then for i:=0 to length(xy)-1 do begin
        if xy[i,iAxis]<1E-323 then result[i,iAxis]:=Nan
                              else result[i,iAxis]:=ln(xy[i,iAxis])/ln(10);
        if isNan(result[i,iAxis]) or isInfinite(result[i,iAxis]) then result[i,iAxis]:=Nan;
      end else for i:=0 to length(xy)-1 do result[i,iAxis]:=xy[i,iAxis];
    end;
  end;

FUNCTION T_plot.isSampleValid(CONST sample: T_point): boolean;
  begin
    with scalingOptions do
    result:=not(isNan(sample[0])) and not(isInfinite(sample[0])) and (not(logscale['x']) or (sample[0]>=1E-100)) and
            not(isNan(sample[1])) and not(isInfinite(sample[1])) and (not(logscale['y']) or (sample[1]>=1E-100));
  end;

PROCEDURE T_plot.setScalingOptions(CONST value: T_scalingOptions);
  VAR axis:char;
      i:longint;
  begin
    system.enterCriticalSection(cs);
    with scalingOptions do begin
      range:=value.range;
      autoscale:=value.autoscale;
      for axis:='x' to 'y' do if value.axisStyle[axis] in [0, C_tics, C_grid, C_finerGrid, C_ticsAndGrid, C_ticsAndFinerGrid]
           then axisStyle[axis]:=value.axisStyle[axis];
      for axis:='x' to 'y' do if logscale[axis]<>value.logscale[axis] then begin
        for i:=0 to 1 do range[axis,i]:=oex(range[axis,i]);
        logscale[axis]:=value.logscale[axis];
        for i:=0 to 1 do range[axis,i]:=olx(range[axis,i]);
      end;
      preserveAspect:=(logscale['x'] = logscale['y']) and value.preserveAspect;
      relativeFontSize:=value.relativeFontSize;
      autoscaleFactor:=value.autoscaleFactor;
    end;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_plot.getScalingOptions: T_scalingOptions;
  begin
    system.enterCriticalSection(cs);
    result:=scalingOptions;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.zoomOnPoint(CONST pixelX, pixelY: longint; CONST factor: double; VAR plotImage: TImage);
  VAR holdX, holdY: double;
      rectA, rectB: TRect;
  begin with scalingOptions do begin
    system.enterCriticalSection(cs);
    autoscale['x']:=false;
    autoscale['y']:=false;
    holdX:=(pixelX-scaledXOffset)/(screenWidth-scaledXOffset)*
      (range['x', 1]-range['x', 0])+range['x', 0];
    holdY:=(pixelY-scaledYOffset)/(-scaledYOffset)*
      (range['y', 1]-range['y', 0])+range['y', 0];
    range['x', 0]:=(range['x', 0]-holdX)*factor+holdX;
    range['x', 1]:=(range['x', 1]-holdX)*factor+holdX;
    range['y', 0]:=(range['y', 0]-holdY)*factor+holdY;
    range['y', 1]:=(range['y', 1]-holdY)*factor+holdY;

    rectA.top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;

    rectB.top:=round((-pixelY)*factor+pixelY);
    rectB.Left:=round((-pixelX)*factor+pixelX);
    rectB.Right:=round((plotImage.width-pixelX)*factor+pixelX);
    rectB.Bottom:=round((plotImage.height-pixelY)*factor+pixelY);

    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
    system.leaveCriticalSection(cs);
  end; end;

PROCEDURE T_plot.panByPixels(CONST pixelDX, pixelDY: longint; VAR plotImage: TImage);
  VAR worldDX, worldDY: double;
      rectA, rectB: TRect;
  begin with scalingOptions do begin
    system.enterCriticalSection(cs);
    autoscale['x']:=false;
    autoscale['y']:=false;
    worldDX:=pixelDX/(screenWidth-scaledXOffset)*(range['x', 1]-range['x', 0]);
    worldDY:=pixelDY/(-scaledYOffset)*(range['y', 1]-range['y', 0]);
    range['x', 0]:=range['x', 0]+worldDX;
    range['x', 1]:=range['x', 1]+worldDX;
    range['y', 0]:=range['y', 0]+worldDY;
    range['y', 1]:=range['y', 1]+worldDY;

    rectA.top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.top:=0+pixelDY;
    rectB.Left:=0+pixelDX;
    rectB.Right:=plotImage.width+pixelDX;
    rectB.Bottom:=plotImage.height+pixelDY;

    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
    system.leaveCriticalSection(cs);
  end; end;

PROCEDURE T_plot.renderPlot(VAR plotImage: TImage);
  PROCEDURE drawGridAndRows(CONST target: TCanvas; CONST scalingFactor: longint);
    VAR
      rowId, i, x, y, yBaseLine:longint;
      lastX: longint = 0;
      lastY: longint = 0;
      lastWasValid, currentIsValid: boolean;
      sample: T_point;
      patternIdx: byte;
      scaleAndColor:T_scaleAndColor;

    PROCEDURE drawPatternRect(x0, y0, x1, y1: longint);
      VAR x, y, locY: longint;
      begin
        if (x0<0) then x0:=0 else if x0>screenWidth*scalingFactor then
          x0:=screenWidth*scalingFactor;
        if (x1<0) then x1:=0 else if x1>screenWidth*scalingFactor then
          x1:=screenWidth*scalingFactor;
        if (y0<0) then y0:=0 else if y0>screenHeight*scalingFactor then
          y0:=screenHeight*scalingFactor;
        if (y1<0) then y1:=0 else if y1>screenHeight*scalingFactor then
          y1:=screenHeight*scalingFactor;
        if x1<x0 then begin
          x :=x1; y :=y1;
          x1:=x0; y1:=y0;
          x0:=x;  y0:=y;
        end;
        for x:=x0 to x1 do
          begin
          if (x1>x0) then
            locY:=round(y0+(y1-y0)*(x-x0)/(x1-x0))
          else
            locY:=round(0.5*(y0+y1));
          if locY>yBaseLine then
            begin
            for y:=yBaseLine to locY do
              if (x and 1)+2*(y and 1) = patternIdx then
                target.Pixels[x, y]:=scaleAndColor.solidColor;
            end
          else
            for y:=yBaseLine downto locY do
              if (x and 1)+2*(y and 1) = patternIdx then
                target.Pixels[x, y]:=scaleAndColor.solidColor;
          end;
      end;

    begin
      setScreenSize(plotImage.width,plotImage.height);
      //Clear:------------------------------------------------------------------
      target.Brush.style:=bsSolid;
      target.Brush.color:=clWhite;
      target.Pen.style:=psClear;
      target.Pen.EndCap:=pecSquare;
      target.FillRect(0, 0, plotImage.width-1, plotImage.height-1);
      target.clear;
      //------------------------------------------------------------------:Clear
      //coordinate grid:========================================================
      target.Pen.style:=psSolid;
      //minor grid:-------------------------------------------------------------
      scaleAndColor:=MINOR_TIC_STYLE.getLineScaleAndColor(sqrt(sqr(plotImage.width)+sqr(plotImage.height))/1000);
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      if (scalingOptions.axisStyle['y'] and C_finerGrid) = C_finerGrid then
      for i:=0 to length(tic['y'])-1 do with tic['y'][i] do if not(major) then begin
        y:=round(pos*scalingFactor);
        target.line(0, y, screenWidth*scalingFactor, y);
      end;
      if (scalingOptions.axisStyle['x'] and C_finerGrid) = C_finerGrid then
      for i:=0 to length(tic['x'])-1 do with tic['x'][i] do if not(major) then begin
        x:=round(pos*scalingFactor);
        target.line(x, 0, x, screenHeight*scalingFactor);
      end;
      //-------------------------------------------------------------:minor grid
      //major grid:-------------------------------------------------------------
      scaleAndColor:=MAJOR_TIC_STYLE.getLineScaleAndColor(sqrt(sqr(plotImage.width)+sqr(plotImage.height))/1000);
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      if (scalingOptions.axisStyle['y'] and C_grid) = C_grid then
      for i:=0 to length(tic['y'])-1 do with tic['y'][i] do if major then begin
        y:=round(pos*scalingFactor);
        target.line(0, y, screenWidth*scalingFactor, y);
      end;
      if (scalingOptions.axisStyle['x'] and C_grid) = C_grid then
      for i:=0 to length(tic['x'])-1 do with tic['x'][i] do if major then begin
        x:=round(pos*scalingFactor);
        target.line(x, 0, x, screenHeight*scalingFactor);
      end;
      //-------------------------------------------------------------:major grid
      //========================================================:coordinate grid
      //row data:===============================================================
      if scalingOptions.logscale['y'] then yBaseLine:=yOffset
                                      else yBaseLine:=round(realToScreen('y', 0)*scalingFactor);
      if yBaseLine<0 then yBaseLine:=0
      else if yBaseLine>=target.height then yBaseLine:=target.height-1;
      for rowId:=0 to length(row)-1 do begin
        {$ifdef debugMode}
        writeln('Drawing row #',rowId,' with style: ',row[rowId].style.toString);
        {$endif}
        scaleAndColor:=row[rowId].style.getLineScaleAndColor(sqrt(sqr(plotImage.width)+sqr(plotImage.height))/1000);
        patternIdx:=rowId and 3;

        if ps_straight in row[rowId].style.style then begin
          target.Pen.style:=psSolid;
          target.Pen.color:=scaleAndColor.lineColor;
          target.Pen.width:=scaleAndColor.lineWidth;
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(row[rowId].sample)-1 do begin
            sample:=row[rowId].sample[i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then begin
              sample:=realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then begin
                target.LineTo(x, y);
                if ps_filled in row[rowId].style.style then
                  drawPatternRect(lastX, lastY, x, y);
              end else
                target.MoveTo(x, y);
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if ps_stepLeft in row[rowId].style.style then begin
          target.Pen.style:=psSolid;
          target.Pen.color:=scaleAndColor.lineColor;
          target.Pen.width:=scaleAndColor.lineWidth;
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(row[rowId].sample)-1 do
            begin
            sample:=row[rowId].sample[i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then
                begin
                target.LineTo(lastX, y);
                target.LineTo(x, y);
                if ps_filled in row[rowId].style.style then
                  drawPatternRect(lastX, y, x, y);
                end
              else
                target.MoveTo(x, y);
              lastX:=x;
              lastY:=y;
              end;
            lastWasValid:=currentIsValid;
            end;
          end
        else if ps_stepRight in row[rowId].style.style then begin
          target.Pen.style:=psSolid;
          target.Pen.color:=scaleAndColor.lineColor;
          target.Pen.width:=scaleAndColor.lineWidth;
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(row[rowId].sample)-1 do
            begin
            sample:=row[rowId].sample[i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then
                begin
                target.LineTo(x, lastY);
                target.LineTo(x, y);
                if ps_filled in row[rowId].style.style then
                  drawPatternRect(lastX, lastY, x, lastY);
                end
              else
                target.MoveTo(x, y);
              lastX:=x;
              lastY:=y;
              end;
            lastWasValid:=currentIsValid;
            end;
          end
        else if ps_bar in row[rowId].style.style then begin
          target.Pen.style:=psSolid;
          target.Pen.color:=scaleAndColor.lineColor;
          target.Pen.width:=scaleAndColor.lineWidth;
          target.Pen.EndCap:=pecRound;

          lastWasValid:=false;
          for i:=0 to length(row[rowId].sample)-1 do begin
            sample:=row[rowId].sample[i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then begin
              sample:=realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then  begin
                drawPatternRect(round(lastX*0.95+x*0.05), lastY,
                                round(lastX*0.05+x*0.95), lastY);
                target.line(round(lastX*0.95+x*0.05), yBaseLine,
                            round(lastX*0.95+x*0.05), lastY);
                target.line(round(lastX*0.95+x*0.05), lastY,
                            round(lastX*0.05+x*0.95), lastY);
                target.line(round(lastX*0.05+x*0.95), yBaseLine,
                            round(lastX*0.05+x*0.95), lastY);
              end;
              lastX:=x;
              lastY:=y;
              lastWasValid:=currentIsValid;
            end;
          end;
        end else if ps_box in row[rowId].style.style then begin
          target.Pen.style:=psClear;
          target.Brush.style:=bsSolid;
          target.Brush.color:=scaleAndColor.solidColor;
          lastWasValid:=false;
          i:=0;
          while i+1<length(row[rowId].sample) do
            begin
            sample:=row[rowId].sample[i];
            if isSampleValid(sample) then
              begin
              sample:=realToScreen(sample);
              lastX:=round(sample[0]*scalingFactor);
              lastY:=round(sample[1]*scalingFactor);
              sample:=row[rowId].sample[i+1];
              if isSampleValid(sample) then
                begin
                sample:=realToScreen(sample);
                x:=round(sample[0]*scalingFactor);
                y:=round(sample[1]*scalingFactor);
                target.FillRect(lastX, lastY, x, y);
                end;
              end;
            inc(i, 2);
            end;
        end;
        if ps_dot in row[rowId].style.style then begin
          target.Pen.style:=psClear;
          target.Brush.style:=bsSolid;
          target.Brush.color:=scaleAndColor.solidColor;
          if scaleAndColor.symbolWidth>=1 then
          for i:=0 to length(row[rowId].sample)-1 do begin
            sample:=row[rowId].sample[i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then begin
              sample:=realToScreen(sample);
              target.Ellipse(round(sample[0]*scalingFactor-scaleAndColor.symbolWidth),
                             round(sample[1]*scalingFactor-scaleAndColor.symbolWidth),
                             round(sample[0]*scalingFactor+scaleAndColor.symbolWidth),
                             round(sample[1]*scalingFactor+scaleAndColor.symbolWidth));
            end;
          end else for i:=0 to length(row[rowId].sample)-1 do begin
            sample:=row[rowId].sample[i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then begin
              sample:=realToScreen(sample);
              target.Pixels[round(sample[0]*scalingFactor),
                            round(sample[1]*scalingFactor)]:=scaleAndColor.lineColor;
            end;
          end;
        end;
        if ps_plus in row[rowId].style.style then begin
          target.Pen.style:=psSolid;
          target.Pen.color:=scaleAndColor.lineColor;
          target.Pen.width:=scaleAndColor.lineWidth;
          target.Pen.EndCap:=pecSquare;
          for i:=0 to length(row[rowId].sample)-1 do
            begin
            sample:=row[rowId].sample[i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              target.line(round(sample[0]*scalingFactor-scaleAndColor.symbolWidth),
                          round(sample[1]*scalingFactor),
                          round(sample[0]*scalingFactor+scaleAndColor.symbolWidth),
                          round(sample[1]*scalingFactor));
              target.line(round(sample[0]*scalingFactor),
                          round(sample[1]*scalingFactor-scaleAndColor.symbolWidth),
                          round(sample[0]*scalingFactor),
                          round(sample[1]*scalingFactor+scaleAndColor.symbolWidth));
              end;
            end;
          end;
        if ps_cross in row[rowId].style.style then begin
          target.Pen.style:=psSolid;
          target.Pen.color:=scaleAndColor.lineColor;
          target.Pen.width:=scaleAndColor.lineWidth;
          target.Pen.EndCap:=pecSquare;
          for i:=0 to length(row[rowId].sample)-1 do begin
            sample:=row[rowId].sample[i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then begin
              sample:=realToScreen(sample);
              target.line(round(sample[0]*scalingFactor-scaleAndColor.symbolRadius),
                          round(sample[1]*scalingFactor-scaleAndColor.symbolRadius),
                          round(sample[0]*scalingFactor+scaleAndColor.symbolRadius),
                          round(sample[1]*scalingFactor+scaleAndColor.symbolRadius));
              target.line(round(sample[0]*scalingFactor+scaleAndColor.symbolRadius),
                          round(sample[1]*scalingFactor-scaleAndColor.symbolRadius),
                          round(sample[0]*scalingFactor-scaleAndColor.symbolRadius),
                          round(sample[1]*scalingFactor+scaleAndColor.symbolRadius));
            end;
          end;
        end;
        if ps_impulse in row[rowId].style.style then begin
          target.Pen.style:=psSolid;
          target.Pen.color:=scaleAndColor.lineColor;
          target.Pen.width:=scaleAndColor.lineWidth;
          target.Pen.EndCap:=pecSquare;
          for i:=0 to length(row[rowId].sample)-1 do
            begin
            sample:=row[rowId].sample[i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              target.line(round(sample[0]*scalingFactor), yBaseLine,
                round(sample[0]*scalingFactor),
                round(sample[1]*scalingFactor));
              end;
            end;
          end;
        end;
      //===============================================================:row data
    end;

  PROCEDURE drawCoordSys(CONST target: TCanvas);
    VAR i, x, y: longint;
    begin
      //coordinate system:======================================================
      //clear border:-----------------------------------------------------------
      target.Brush.style:=bsSolid;
      target.Brush.color:=clWhite;
      target.Pen.style:=psClear;
      target.Pen.width:=1;
      target.Pen.EndCap:=pecSquare;
      if (scalingOptions.axisStyle['y'] and C_ticsAndGrid) > 0 then target.FillRect(0,0, xOffset, screenHeight);
      if (scalingOptions.axisStyle['x'] and C_ticsAndGrid) > 0 then target.FillRect(xOffset, yOffset,screenWidth, screenHeight);
      //-----------------------------------------------------------:clear border
      //axis:-------------------------------------------------------------------
      target.Pen.style:=psSolid;
      target.Pen.color:=clBlack;
      target.Pen.width:=1;
      if (scalingOptions.axisStyle['y'] and C_ticsAndGrid) > 0 then target.line(xOffset, 0, xOffset, yOffset);
      if (scalingOptions.axisStyle['x'] and C_ticsAndGrid) > 0 then target.line(screenWidth, yOffset, xOffset, yOffset);
      //-------------------------------------------------------------------:axis
      //tics:-------------------------------------------------------------------
      if (scalingOptions.axisStyle['y'] and C_ticsAndGrid) > 0 then for i:=0 to length(tic['y'])-1 do with tic['y'][i] do if major then begin
        y:=round(pos);
        target.line(xOffset-5, y, xOffset, y);
        if (scalingOptions.axisStyle['y'] and C_tics)>0 then target.textOut(xOffset-5-target.TextWidth(txt), y-target.TextHeight(txt) shr 1, txt);
      end;
      if (scalingOptions.axisStyle['x'] and C_ticsAndGrid) >0 then for i:=0 to length(tic['x'])-1 do with tic['x'][i] do if major then begin
        x:=round(pos);
        target.line(x, yOffset+5, x, yOffset);
        if (scalingOptions.axisStyle['x'] and C_tics)>0 then target.textOut(x-target.TextWidth(txt) shr 1, yOffset+5, txt);
      end;
      //-------------------------------------------------------------------:tics
      //======================================================:coordinate system
    end;

  FUNCTION setTextSize(CONST xTicHeight, yTicWidth: longint): boolean;
    begin with scalingOptions do begin
      result:=false;
      if ((axisStyle['y'] and C_tics)>0) and (xOffset<>yTicWidth+5) then begin
        xOffset:=yTicWidth+5;
        result:=true;
      end;
      if ((axisStyle['x'] and C_tics)>0) and (yOffset<>screenHeight-(xTicHeight+5)) then begin
        yOffset:=screenHeight-(xTicHeight+5);
        result:=true;
      end;
      if result then setScreenSize(screenWidth, screenHeight);
    end; end;

  begin
    system.enterCriticalSection(cs);
    setScreenSize(plotImage.width,plotImage.height);
    plotImage.Canvas.Font.size:=round(sqrt(sqr(plotImage.width)+sqr(plotImage.height))/1000*scalingOptions.relativeFontSize);
    setTextSize(plotImage.Canvas.TextHeight(longtestYTic),plotImage.Canvas.TextWidth(longtestYTic));
    drawGridAndRows(plotImage.Canvas, 1);
    drawCoordSys(plotImage.Canvas);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.renderInternally(CONST width,height:longint);
  begin
    if renderImage=nil then begin
      renderImage:=TImage.create(nil);
      renderImage.SetInitialBounds(0,0,width,height);
    end else if (renderImage.width<>width) or (renderImage.height<>height) then begin
      renderImage.destroy;
      renderImage:=TImage.create(nil);
      renderImage.SetInitialBounds(0,0,width,height);
    end;
    renderPlot(renderImage);
  end;

PROCEDURE scale(source: TImage; VAR dest: TImage; CONST factor: double);
  VAR ARect: TRect;
      X, Y: integer;
  begin
    X:=round(source.width*factor);
    Y:=round(source.height*factor);
    ARect:=Rect(0, 0, X, Y);
    dest.Canvas.AntialiasingMode:=amOn;
    dest.Canvas.StretchDraw(ARect, source.picture.Bitmap);
  end;

PROCEDURE T_plot.renderPlot(VAR plotImage: TImage; CONST supersampling:longint);
  begin
    if supersampling<=1 then begin
      renderPlot(plotImage);
      scaledXOffset:=xOffset;
      scaledYOffset:=yOffset;
      exit;
    end;
    renderInternally(plotImage.width*supersampling,plotImage.height*supersampling);
    scale(renderImage,plotImage,1/supersampling);

    //set screen size to fix GUI interaction (panning, zooming, etc.)
    scaledXOffset:=xOffset/supersampling;
    scaledYOffset:=yOffset/supersampling;
    screenHeight:=screenHeight div supersampling;
    screenWidth:=screenWidth div supersampling;
  end;

PROCEDURE T_plot.renderToFile(CONST fileName: string; CONST width, height, supersampling: longint);
  VAR storeImage: TImage;
  begin
    system.enterCriticalSection(cs);
    try
      renderInternally(width*supersampling,height*supersampling);
      storeImage:=TImage.create(nil);
      storeImage.SetInitialBounds(0, 0, width, height);
      scale(renderImage,storeImage,1/supersampling);
      storeImage.picture.PNG.saveToFile(ChangeFileExt(fileName, '.png'));
      storeImage.free;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_plot.renderToString(CONST width, height, supersampling: longint): ansistring;
  VAR storeImage: TImage;
      memStream: TStringStream;
  begin
    system.enterCriticalSection(cs);
    renderInternally(width*supersampling,height*supersampling);
    storeImage:=TImage.create(nil);
    storeImage.SetInitialBounds(0, 0, width, height);
    scale(renderImage,storeImage,1/supersampling);
    memStream := TStringStream.create('');
    storeImage.picture.PNG.saveToStream(memStream);
    memStream.position:=0;
    result:=memStream.DataString;
    memStream.free;
    storeImage.free;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.CopyFrom(VAR p:T_plot);
  VAR axis:char;
      i:longint;
  begin
    system.enterCriticalSection(cs);
    system.enterCriticalSection(p.cs);

    screenWidth :=p.screenWidth;
    screenHeight:=p.screenHeight;
    xOffset:=p.xOffset;
    yOffset:=p.yOffset;
    for axis:='x' to 'y' do begin
      setLength(tic[axis],length(p.tic[axis]));
      for i:=0 to length(tic[axis])-1 do tic[axis,i]:=p.tic[axis,i];
    end;
    scalingOptions:=p.scalingOptions;
    for i:=0 to length(row)-1 do row[i].destroy;
    setLength(row,length(p.row));
    for i:=0 to length(row)-1 do begin
      row[i].create(i,p.row[i].sample);
      row[i].style:=p.row[i].style;
    end;
    system.leaveCriticalSection(p.cs);
    system.leaveCriticalSection(cs);
  end;

INITIALIZATION
  MAJOR_TIC_STYLE.create(0); MAJOR_TIC_STYLE.styleModifier:=0.2;
  MINOR_TIC_STYLE.create(0); MINOR_TIC_STYLE.styleModifier:=0.1;
FINALIZATION
  MAJOR_TIC_STYLE.destroy;
  MINOR_TIC_STYLE.destroy;
end.
