UNIT mnh_plotData;

INTERFACE

USES myFiles, SysUtils, Math, mnh_litvar, mnh_tokens, mnh_constants,
  mnh_out_adapters, mnh_tokloc,
  mnh_funcs,
  Interfaces, ExtCtrls, Graphics, types;
TYPE
  T_colorChannel = (cc_red, cc_green, cc_blue);

CONST
  PLOT_NAMESPACE='plot';
  C_lineStyle_none = 0;
  C_lineStyle_straight = 1;
  C_lineStyle_stepLeft = 2;
  C_lineStyle_stepRight = 3;
  C_lineStyle_filled = 4;
  C_lineStyle_bar = 8;
  C_lineStyle_box = 9;
  C_symbolStyle_dot = 16;
  C_symbolStyle_plus = 32;
  C_symbolStyle_cross = 64;
  C_symbolStyle_impulse = 128;

  C_anyLineStyle = 15;

  C_styleName: array[0..9] of record
      idx: byte;
      Name: array[0..1] of string;
    end = ((idx: C_lineStyle_straight; Name: ('line', 'l')),
    (idx: C_lineStyle_stepLeft; Name: ('stepLeft', '')),
    (idx: C_lineStyle_stepRight; Name: ('stepRight', '')),
    (idx: C_lineStyle_filled; Name: ('fill', 'f')),
    (idx: C_lineStyle_bar; Name: ('bar', '')),
    (idx: C_lineStyle_box; Name: ('box', '')),
    (idx: C_symbolStyle_dot; Name: ('dot', '.')),
    (idx: C_symbolStyle_plus; Name: ('plus', '+')),
    (idx: C_symbolStyle_cross; Name: ('cross', 'x')),
    (idx: C_symbolStyle_impulse; Name: ('impulse', 'i')));

TYPE
  T_point = array[0..1] of double;
  T_dataRow = array of T_point;

  { T_style }

  T_style = object(T_serializable)
    //persistent:
    title: string;
    style: byte;
    color: array[T_colorChannel] of byte;
    styleModifier: double;
    CONSTRUCTOR create(CONST index: byte);
    DESTRUCTOR destroy;
    PROCEDURE parseStyle(CONST styleString: ansistring);
    FUNCTION loadFromFile(VAR F: T_File): boolean;
      virtual; overload;
    //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(VAR F: T_File); virtual; overload;
    //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei

    FUNCTION getTColor: longint;
    FUNCTION getIntLineWidth(CONST scalingFactor: double): longint;
    FUNCTION getSymbolWidth: double;
    FUNCTION getSymbolRad: double;
    FUNCTION wantStraightLines: boolean;
    FUNCTION wantLeftSteps: boolean;
    FUNCTION wantRightSteps: boolean;
    FUNCTION wantFill: boolean;
    FUNCTION wantBars: boolean;
    FUNCTION wantBoxes: boolean;
    FUNCTION wantDot: boolean;
    FUNCTION wantPlus: boolean;
    FUNCTION wantCross: boolean;
    FUNCTION wantImpulses: boolean;
  end;

  { T_sampleRow }

  T_boundingBox = array['x'..'y', 0..1] of double;

  T_sampleWithTime = record
    x, y, t: double;
  end;

  T_sampleRow = object(T_serializable)
    //nonpersistent:
    computed: record  temp: array of T_sampleWithTime;
      xRule, yRule: P_expressionLiteral;
      t0, t1: double;
      maxSamples: longint;
    end;
    //persistent:
    style: T_style;
    sample: T_dataRow;
    CONSTRUCTOR create(CONST index: byte);
    PROCEDURE getBoundingBox(CONST logX, logY: boolean; VAR box: T_boundingBox);
    PROCEDURE setRules(CONST forXOrNil, forY: P_expressionLiteral; CONST t0, t1: double; CONST maxSamples: longint; VAR recycler:T_tokenRecycler);
    PROCEDURE computeSamplesInActivePlot(CONST secondPass: boolean; VAR recycler:T_tokenRecycler);
    PROCEDURE addSample(CONST x, y: double);
    DESTRUCTOR destroy;
    FUNCTION loadFromFile(VAR F: T_File): boolean;
      virtual; overload;
    //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(VAR F: T_File); virtual; overload;
    //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
  end;

{ T_plot }
CONST
  C_tics = 1;
  C_grid = 2;
  C_finerGrid = C_grid+4; //=6
  C_ticsAndGrid = C_tics+C_grid; //=3
  C_ticsAndFinerGrid = C_tics+C_finerGrid; //=7

TYPE
  T_ticInfo = record
    pos: double;
    major: boolean;
    txt: ansistring;
  end;

  T_plot = object(T_serializable)
    //private
      //non-persistent:
      screenWidth, screenHeight: longint;
      xOffset, yOffset: longint;

      tic: array['x'..'y'] of array of T_ticInfo;
      //persistent:
      range: array['x'..'y', 0..1] of double;
      axisStyle: array['x'..'y'] of byte;
      autoscale, logscale: array['x'..'y'] of boolean;
      preserveAspect: boolean;
      row: array of T_sampleRow;

      FUNCTION olx(CONST x: double): double;
      FUNCTION oex(CONST x: double): double;
      FUNCTION oly(CONST y: double): double;
      FUNCTION oey(CONST y: double): double;
    //public
      CONSTRUCTOR createWithDefaults;
      PROCEDURE setDefaults;
      DESTRUCTOR destroy;
      PROCEDURE addSampleRow(CONST sampleRow: T_sampleRow);
      PROCEDURE Clear;
      FUNCTION loadFromFile(VAR F: T_File): boolean; virtual; overload;
      //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
      PROCEDURE saveToFile(VAR F: T_File); virtual; overload;
      //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
      PROCEDURE setScreenSize(CONST Width, Height: longint);
      FUNCTION longtestYTic: ansistring;
      FUNCTION setTextSize(CONST xTicHeight, yTicWidth: longint): boolean;
      FUNCTION addRow(CONST styleOptions: string): longint;

      FUNCTION wantTics(CONST axis: char): boolean;
      FUNCTION realToScreen(CONST p: T_point): T_point;
      FUNCTION realToScreen(CONST x, y: double): T_point;
      FUNCTION realToScreen(CONST axis: char; CONST p: double): double;
      FUNCTION screenToReal(CONST x, y: longint): T_point;
      FUNCTION est_curvature(CONST s0, s1, s2: T_point): double;
      FUNCTION isSampleValid(CONST sample: T_point): boolean;
      //interfacing:
      PROCEDURE setAutoscale(CONST autoX, autoY: boolean);
      FUNCTION getAutoscale: P_listLiteral;
      PROCEDURE setRange(CONST x0, y0, x1, y1: double);
      FUNCTION getRange: P_listLiteral;
      PROCEDURE setAxisStyle(CONST x, y: longint);
      FUNCTION getAxisStyle: P_listLiteral;
      PROCEDURE setLogscale(CONST logX, logY: boolean);
      FUNCTION getLogscale: P_listLiteral;
      PROCEDURE setPreserveAspect(CONST flag: boolean);
      FUNCTION getPreserveAspect: P_boolLiteral;

      PROCEDURE zoomOnPoint(CONST pixelX, pixelY: longint; CONST factor: double; VAR plotImage: TImage);
      PROCEDURE panByPixels(CONST pixelDX, pixelDY: longint; VAR plotImage: TImage);

      PROCEDURE renderPlot(VAR plotImage: TImage; CONST supersampling: longint);
  end;

VAR
  activePlot: T_plot;
  plotCS:TRTLCriticalSection;

FUNCTION plot(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
FUNCTION addPlot(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
FUNCTION setAutoscale(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
FUNCTION setLogscale(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
FUNCTION setPlotRange(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
FUNCTION setAxisStyle(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
FUNCTION setPreserveAspect(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;

IMPLEMENTATION

FUNCTION fReal(CONST X: P_literal): double; inline;
  begin
    if x = nil then result:=NAN else case X^.literalType of
        lt_real: begin
          result:=P_realLiteral(x)^.value;
          if IsInfinite(result) then
            result:=NaN;
          end;
        lt_int: result:=P_intLiteral(x)^.value;
      else result:=NaN;
        end;
  end;

{ T_sampleRow }

CONSTRUCTOR T_sampleRow.create(CONST index: byte);
  begin
    style.create(index);
    setLength(sample, 0);
    with computed do
      begin
      computed.xRule:=nil;
      computed.yRule:=nil;
      computed.t0:=0;
      computed.t1:=1;
      end;
  end;

PROCEDURE T_sampleRow.getBoundingBox(CONST logX, logY: boolean; VAR box: T_boundingBox);
  VAR
    i: longint;
    x, y: double;
  begin
    for i:=0 to length(sample)-1 do
      begin
      x:=sample[i, 0];
      if logX and (x<=1E-100) then
        x:=NaN;
      y:=sample[i, 1];
      if logY and (y<=1E-100) then
        y:=NaN;
      if IsNan(box['x', 0]) or (not (IsNan(x)) and not (IsInfinite(x)) and
        (x<box['x', 0])) then
        box['x', 0]:=x;
      if IsNan(box['x', 1]) or (not (IsNan(x)) and not (IsInfinite(x)) and
        (x>box['x', 1])) then
        box['x', 1]:=x;
      if IsNan(box['y', 0]) or (not (IsNan(y)) and not (IsInfinite(y)) and
        (y<box['y', 0])) then
        box['y', 0]:=y;
      if IsNan(box['y', 1]) or (not (IsNan(y)) and not (IsInfinite(y)) and
        (y>box['y', 1])) then
        box['y', 1]:=y;
      end;
  end;

PROCEDURE T_sampleRow.setRules(CONST forXOrNil, forY: P_expressionLiteral; CONST t0, t1: double; CONST maxSamples: longint; VAR recycler:T_tokenRecycler);
  begin
    if maxSamples>100
    then computed.maxSamples:=maxSamples
    else computed.maxSamples:=100;
    computed.xRule:=forXOrNil;
    computed.yRule:=forY;
    if forXOrNil<>nil then forXOrNil^.rereference;
    if forY     <>nil then forY     ^.rereference;
    if t1>t0 then begin
      computed.t0:=t0;
      computed.t1:=t1;
    end else begin
      computed.t0:=t1;
      computed.t1:=t0;
    end;
    computeSamplesInActivePlot(false,recycler);
  end;

PROCEDURE T_sampleRow.computeSamplesInActivePlot(CONST secondPass: boolean; VAR recycler:T_tokenRecycler);
  CONST
    initialSampleCount = 100;

  VAR
    xRule, yRule: P_expressionLiteral;

  FUNCTION getSampleWithTime(CONST atTime: double; VAR recycler:T_tokenRecycler): T_sampleWithTime; inline;
    VAR
      pt: T_realLiteral;
      L: P_literal;
    begin
      result.t:=atTime;
      pt.create(atTime);
      if xRule = nil then
        result.x:=atTime
      else
        begin
        L:=P_subrule(xRule^.value)^.directEvaluateUnary(@pt,xRule, 0,recycler);
        result.x:=fReal(L);
        if L<>nil then disposeLiteral(L);
        end;
        begin
        L:=P_subrule(yRule^.value)^.directEvaluateUnary(@pt,yRule, 0,recycler);
        result.y:=fReal(L);
        if L<>nil then disposeLiteral(L);
        end;
      pt.destroy;
    end;

  VAR
    refineTemp: array of T_sampleWithTime;

  PROCEDURE addRefine(CONST sample: T_sampleWithTime); inline;
    begin
      setLength(refineTemp, length(refineTemp)+1);
      refineTemp[length(refineTemp)-1]:=sample;
    end;

  FUNCTION mergeTemp: boolean;
    VAR
      tempCopy: array of T_sampleWithTime;
      i, j, k: longint;
    begin
      if length(refineTemp) = 0 then
        exit(false);

      setLength(tempCopy, length(computed.temp));
      for i:=0 to length(tempCopy)-1 do
        tempCopy[i]:=computed.temp[i];

      setLength(computed.temp, length(tempCopy)+length(refineTemp));
      i:=0;
      j:=0;
      k:=0;
      while (i<length(refineTemp)) and (j<length(tempCopy)) do
        if refineTemp[i].t<tempCopy[j].t then
          begin
          computed.temp[k]:=refineTemp[i];
          Inc(i);
          Inc(k);
          end
        else
          begin
          computed.temp[k]:=tempCopy[j];
          Inc(j);
          Inc(k);
          end;
      while (i<length(refineTemp)) do
        begin
        computed.temp[k]:=refineTemp[i];
        Inc(i);
        Inc(k);
        end;
      while (j<length(tempCopy)) do
        begin
        computed.temp[k]:=tempCopy[j];
        Inc(j);
        Inc(k);
        end;
      setLength(tempCopy, 0);
      setLength(refineTemp, 0);
      result:=true;
    end;

  FUNCTION curvature(CONST i: longint): double;
    VAR
      p0, p1, p2: T_point;
    begin
      with computed do
        begin
        if (i<=0) or (i>=length(temp)-1) then exit(0);
        p0:=activePlot.realToScreen(temp[i-1].x, temp[i-1].y);
        p1:=activePlot.realToScreen(temp[i].x, temp[i].y);
        p2:=activePlot.realToScreen(temp[i+1].x, temp[i+1].y);
        if (p0[0]<0) and  (p1[0]<0) and  (p2[0]<0) or
          (p0[0]>activePlot.screenWidth) and
          (p1[0]>activePlot.screenWidth) and  (p2[0]>activePlot.screenWidth) or
          (p0[1]<0) and  (p1[1]<0) and  (p2[1]<0) or
          (p0[1]>activePlot.screenHeight) and
          (p1[1]>activePlot.screenHeight) and  (p2[1]>activePlot.screenHeight) then exit(0);
        end;
      result:=sqr(p0[0]-2*p1[0]+p2[0])+sqr(p0[1]-2*p1[1]+p2[1]);
      if IsNan(result) or IsInfinite(result) then
        result:=0;
    end;

  PROCEDURE copyTempToData;
    VAR
      i: longint;
    begin
      with computed do
        begin
        setLength(sample, length(temp));
        for i:=0 to length(temp)-1 do
          begin
          if IsInfinite(temp[i].x) then
            sample[i, 0]:=NaN
          else
            sample[i, 0]:=temp[i].x;
          if IsInfinite(temp[i].y) then
            sample[i, 1]:=NaN
          else
            sample[i, 1]:=temp[i].y;
          end;
        end;
    end;

  VAR
    i: longint;
    curvatureThreshold: double;
  begin
    if (computed.yRule<>nil) and (length(sample) = 0) and not (secondPass) then
      begin
      if computed.xRule = nil then
        xRule:=nil
      else
        xRule:=computed.xRule;
      yRule:=computed.yRule;

      setLength(computed.temp, initialSampleCount+1);
      with computed do
        for i:=0 to initialSampleCount do
          if errorLevel<el3_evalError then
            computed.temp[i] :=
              getSampleWithTime(t0+(t1-t0)*i/initialSampleCount,recycler);

      copyTempToData;
      end
    else if (computed.yRule<>nil) and (length(sample)>0) and secondPass then
      begin
      if computed.xRule = nil then
        xRule:=nil
      else
        xRule:=computed.xRule;
      yRule:=computed.yRule;

      curvatureThreshold:=0;
      for i:=1 to length(computed.temp)-1 do
        curvatureThreshold:=curvatureThreshold+curvature(i);
      curvatureThreshold:=1E3*curvatureThreshold/(length(computed.temp)-1);
      setLength(refineTemp, 0);

      repeat
        i:=0;
        for i:=0 to length(computed.temp)-1 do
          if (errorLevel<el3_evalError) and
            ((curvature(i)>curvatureThreshold) or
            (curvature(i+1)>curvatureThreshold)) then
            addRefine(getSampleWithTime(
              (computed.temp[i].t+computed.temp[i+1].t)*0.5,recycler));

        if not (mergeTemp) then
          curvatureThreshold:=curvatureThreshold*0.9;
      until (length(computed.temp)>=computed.maxSamples) or
        (errorLevel>=el3_evalError);

      copyTempToData;
      setLength(computed.temp, 0);

      if computed.xRule<>nil then
        disposeLiteral(computed.xRule);
      disposeLiteral(computed.yRule);
      computed.xRule:=nil;
      computed.yRule:=nil;
    end;
  end;

PROCEDURE T_sampleRow.addSample(CONST x, y: double);
  begin
    setLength(sample, length(sample)+1);
    if IsInfinite(x) then
      sample[length(sample)-1, 0]:=NaN
    else
      sample[length(sample)-1, 0]:=x;
    if IsInfinite(y) then
      sample[length(sample)-1, 1]:=NaN
    else
      sample[length(sample)-1, 1]:=y;
  end;

DESTRUCTOR T_sampleRow.destroy;
  begin
    setLength(sample, 0);
  end;

FUNCTION T_sampleRow.loadFromFile(VAR F: T_File): boolean;
  VAR
    i1, i: longint;
  begin
    if not (style.loadFromFile(F)) then
      exit(false);
    i1:=f.readLongint;
    if i1<0 then
      exit(false);
    setLength(sample, i1);
    for i:=0 to i1-1 do
      begin
      sample[i, 0]:=f.readDouble;
      sample[i, 1]:=f.readDouble;
      end;
    result:=f.allOkay;
  end;

PROCEDURE T_sampleRow.saveToFile(VAR F: T_File);
  VAR
    i: longint;
  begin
    style.saveToFile(F);
    f.writeLongint(length(sample));
    for i:=0 to length(sample)-1 do
      begin
      f.writeDouble(sample[i, 0]);
      f.writeDouble(sample[i, 1]);
      end;
  end;

{ T_style }

CONST
  C_defaultColor: array[0..5] of record
      Name: string;
      color: array[T_colorChannel] of byte
    end =
    ((Name: 'black'; color: (0, 0, 0)),
    (Name: 'red'; color: (255, 0, 0)),
    (Name: 'blue'; color: (0, 0, 255)),
    (Name: 'green'; color: (0, 128, 0)),
    (Name: 'purple'; color: (192, 0, 192)),
    (Name: 'orange'; color: (255, 96, 0)));

CONSTRUCTOR T_style.create(CONST index: byte);
  begin
    title:='';
    style:=C_lineStyle_straight;
    color:=C_defaultColor[index mod length(C_defaultColor)].color;
    styleModifier:=1;
  end;

DESTRUCTOR T_style.destroy;
  begin
  end;

PROCEDURE T_style.parseStyle(CONST styleString: ansistring);

  FUNCTION parseColorOption(colorOption: string; OUT r, g, b: byte): boolean;

    PROCEDURE HSV2RGB(H, S, V: single; OUT r, g, b: byte);
      VAR
        hi, p, q, t: byte;
      begin
        while H<0 do
          H:=H+1;
        while H>1 do
          H:=H-1;
        hi:=trunc(H*6);
        H:=H*6-hi;
        V:=V*255;
        p:=round(V*(1-S));
        q:=round(V*(1-S*H));
        t:=round(V*(1-S*(1-H)));
        case hi of
          0, 6: begin
            r:=round(V);
            g:=t;
            b:=p;
            end;
          1: begin
            r:=q;
            g:=round(V);
            b:=p;
            end;
          2: begin
            r:=p;
            g:=round(V);
            b:=t;
            end;
          3: begin
            r:=p;
            g:=q;
            b:=round(V);
            end;
          4: begin
            r:=t;
            g:=p;
            b:=round(V);
            end;
          5: begin
            r:=round(V);
            g:=p;
            b:=q;
            end;
          end;
      end;

    VAR
      rStr: string = '';
      gStr: string = '';
      bStr: string = '';
      i: longint;
      isHSV: boolean;
    begin
      result:=false;
      for i:=0 to length(C_defaultColor)-1 do
        if colorOption = C_defaultColor[i].Name then
          begin
          color:=C_defaultColor[i].color;
          result:=true;
          end;
      if not (result) and ((copy(colorOption, 1, 3) = 'RGB') or
        (copy(colorOption, 1, 3) = 'HSV')) then
        begin
        isHSV:=(copy(colorOption, 1, 3) = 'HSV');
        colorOption:=copy(colorOption, 4, length(colorOption)-3);
        i:=pos(',', colorOption);
        if i>0 then
          begin
          rStr:=copy(colorOption, 1, i-1);
          colorOption:=copy(colorOption, i+1, length(colorOption));
          i:=pos(',', colorOption);
          end;
        if i>0 then
          begin
          gStr:=copy(colorOption, 1, i-1);
          colorOption:=copy(colorOption, i+1, length(colorOption));
          i:=pos(',', colorOption);
          end;
        if i>0 then
          begin
          bStr:=copy(colorOption, 1, i-1);
          colorOption:=copy(colorOption, i+1, length(colorOption));
          i:=pos(',', colorOption);
          end
        else
          bStr:=colorOption;
        if (rStr<>'') and (gStr<>'') and (bStr<>'') then
          begin
          if isHSV then
            HSV2RGB(
              strToFloatDef(rStr, 0),
              max(0, min(1, strToFloatDef(gStr, 0))),
              max(0, min(1, strToFloatDef(bStr, 0))), r, g, b)
          else
            begin
            r:=round(255*max(0, min(1, strToFloatDef(rStr, 0))));
            g:=round(255*max(0, min(1, strToFloatDef(gStr, 0))));
            b:=round(255*max(0, min(1, strToFloatDef(bStr, 0))));
            end;
          result:=true;
          end;
        end;
      if not (result) and (copy(colorOption, 1, 3) = 'HUE') then
        begin
        colorOption:=copy(colorOption, 4, length(colorOption)-3);
        HSV2RGB(strToFloatDef(colorOption, 0), 1, 1, r, g, b);
        result:=true;
        end;
      if not (result) and (copy(colorOption, 1, 4) = 'GREY') then
        begin
        colorOption:=copy(colorOption, 5, length(colorOption)-4);
        r:=round(255*max(0, min(1, strToFloatDef(colorOption, 0))));
        g:=r;
        b:=r;
        result:=true;
        end;
    end;

  VAR
    part, options: ansistring;
    sp: longint;
    i: longint;
    size: T_myFloat;
    mightBeColor: boolean;
  begin
    style:=0;
    options:=Trim(styleString);
    repeat
      sp:=pos(' ', options);
      if sp<=0 then
        begin
        part:=options;
        options:='';
        end
      else
        begin
        part:=copy(options, 1, sp-1);
        options:=trim(copy(options, sp+1, length(options)));
        end;
      mightBeColor:=true;
      size:=StrToFloatDef(part, NaN);
      if not (IsNan(size)) then
        begin
        styleModifier:=size;
        mightBeColor:=false;
        end;
      for i:=0 to length(C_styleName)-1 do
        with C_styleName[i] do
          if (part = Name[0]) or (part = Name[1]) then
            begin
            if idx in[C_lineStyle_box, C_lineStyle_bar, C_lineStyle_straight,
              C_lineStyle_stepRight, C_lineStyle_stepLeft] then
              style:=style and not (C_anyLineStyle) or idx
            else
              style:=style or idx;
            mightBeColor:=false;
            end;
      if mightBeColor then
        parseColorOption(part, color[cc_red], color[cc_green], color[cc_blue]);
    until options = '';
    if style = 0 then
      style:=C_lineStyle_straight;
  end;

FUNCTION T_style.loadFromFile(VAR F: T_File): boolean;
  VAR
    cc: T_colorChannel;
  begin
    title:=f.readAnsiString;
    style:=f.readByte;
    for cc:=cc_red to cc_blue do
      color[cc]:=f.readByte;
    styleModifier:=f.readDouble;
    result:=f.allOkay;
  end;

PROCEDURE T_style.saveToFile(VAR F: T_File);
  VAR
    cc: T_colorChannel;
  begin
    f.writeAnsiString(title);
    f.writeByte(style);
    for cc:=cc_red to cc_blue do
      f.writeByte(color [cc]);
    f.writeDouble(styleModifier);
  end;

FUNCTION T_style.getTColor: longint;
  begin
    result:=color [cc_red] or (color [cc_green] shl 8) or (color [cc_blue] shl 16);
  end;

FUNCTION T_style.getIntLineWidth(CONST scalingFactor: double): longint;
  begin
    result:=trunc(styleModifier*scalingFactor);
    if (random<frac(styleModifier*scalingFactor)) then
      Inc(result);
    if result<0 then
      result:=0;
  end;

FUNCTION T_style.getSymbolWidth: double;
  begin
    result:=3*styleModifier;
  end;

FUNCTION T_style.getSymbolRad: double;
  begin
    result:=3/sqrt(2)*styleModifier;
  end;

FUNCTION T_style.wantStraightLines: boolean;
  begin
    result:=(style and C_anyLineStyle) in [C_lineStyle_straight,
      C_lineStyle_straight or C_lineStyle_filled];
  end;

FUNCTION T_style.wantLeftSteps: boolean;
  begin
    result:=(style and C_anyLineStyle) in [C_lineStyle_stepLeft,
      C_lineStyle_stepLeft or C_lineStyle_filled];
  end;

FUNCTION T_style.wantRightSteps: boolean;
  begin
    result:=(style and C_anyLineStyle) in [C_lineStyle_stepRight,
      C_lineStyle_stepRight or C_lineStyle_filled];
  end;

FUNCTION T_style.wantFill: boolean;
  begin
    result:=(style and C_lineStyle_filled) = C_lineStyle_filled;
  end;

FUNCTION T_style.wantBars: boolean;
  begin
    result:=style = C_lineStyle_bar;
  end;

FUNCTION T_style.wantBoxes: boolean;
  begin
    result:=style = C_lineStyle_box;
  end;

FUNCTION T_style.wantDot: boolean;
  begin
    result:=(style and C_symbolStyle_dot) = C_symbolStyle_dot;
  end;

FUNCTION T_style.wantPlus: boolean;
  begin
    result:=(style and C_symbolStyle_plus) = C_symbolStyle_plus;
  end;

FUNCTION T_style.wantCross: boolean;
  begin
    result:=(style and C_symbolStyle_cross) = C_symbolStyle_cross;
  end;

FUNCTION T_style.wantImpulses: boolean;
  begin
    result:=(style and C_symbolStyle_impulse) = C_symbolStyle_impulse;
  end;

CONSTRUCTOR T_plot.createWithDefaults;
  begin
    setDefaults;
  end;

PROCEDURE T_plot.setDefaults;
  VAR axis: char;
  begin
    screenWidth:=200;
    screenHeight:=200;
    xOffset:=20;
    yOffset:=200-20;
    for axis:='x' to 'y' do begin
      range[axis, 0]:=-1.5;
      range[axis, 1]:=1.5;
      logscale[axis]:=false;
      axisStyle[axis]:=C_ticsAndFinerGrid;
      autoscale[axis]:=true;
    end;
    preserveAspect:=true;
    Clear;
  end;

DESTRUCTOR T_plot.destroy;
  begin
    Clear;
  end;

PROCEDURE T_plot.addSampleRow(CONST sampleRow: T_sampleRow);
  begin
    setLength(row, length(row)+1);
    row[length(row)-1]:=sampleRow;
  end;

PROCEDURE T_plot.Clear;
  VAR i: longint;
  begin
    for i:=0 to length(row)-1 do row[i].destroy;
    setLength(row, 0);
  end;

FUNCTION T_plot.loadFromFile(VAR F: T_File): boolean;
  VAR axis: char;
      i, iMax: longint;
  begin
    for axis:='x' to 'y' do begin
      for i:=0 to 1 do range[axis, i]:=f.readDouble;
      axisStyle[axis]:=f.readByte;
      autoscale[axis]:=f.readBoolean;
      logscale[axis]:=f.readBoolean;
    end;
    preserveAspect:=f.readBoolean;
    iMax:=f.readLongint;
    if not (F.allOkay) or (iMax<0) then exit(false);
    Clear;
    setLength(row, iMax);
    result:=true;
    for i:=0 to iMax-1 do begin
      row[i].create(i);
      result:=result and row [i].loadFromFile(f);
    end;
  end;

PROCEDURE T_plot.saveToFile(VAR F: T_File);
  VAR axis: char;
      i: longint;
  begin
    for axis:='x' to 'y' do
      begin
      for i:=0 to 1 do
        f.writeDouble(range [axis, i]);
      f.writeByte(axisStyle [axis]);
      f.writeBoolean(autoscale [axis]);
      f.writeBoolean(logscale [axis]);
      end;
    f.writeBoolean(preserveAspect);
    f.writeLongint(length(row));
    for i:=0 to length(row)-1 do
      row[i].saveToFile(f);
  end;

PROCEDURE T_plot.setScreenSize(CONST Width, Height: longint);

  PROCEDURE getRanges;
    VAR
      axis: char;
      i: longint;
      center, extend: double;
      boundingBox: T_boundingBox;
      recycler:T_tokenRecycler;
    begin
      for axis:='x' to 'y' do
        for i:=0 to 1 do
          boundingBox[axis, i]:=Nan;
      for i:=0 to length(row)-1 do
        row[i].getBoundingBox(logscale ['x'], logscale ['y'], boundingBox);
      for axis:='x' to 'y' do
        for i:=0 to 1 do
          begin
          if isNAN(boundingBox [axis, i]) then
            boundingBox[axis, i]:=range [axis, i]
          else if logscale [axis] then
            boundingBox[axis, i]:=ln(boundingBox [axis, i])/ln(10);
          if autoscale [axis] then
            range[axis, i]:=boundingBox [axis, i];
          end;

      if preserveAspect and (autoscale ['x'] or autoscale ['y']) and
        (logscale ['x'] = logscale ['y']) then
        if autoscale ['x'] then
          begin
          if autoscale ['y'] then
            begin
            if (range ['x', 1]-range ['x', 0])/(screenWidth-xOffset)>
              (range ['y', 1]-range ['y', 0])/yOffset then
              begin
              center:=(range ['y', 1]+range ['y', 0])*0.5;
              extend:=(range ['x', 1]-range ['x', 0])/(screenWidth-xOffset)*
                yOffset*0.5;
              range['y', 0]:=center-extend;
              range['y', 1]:=center+extend;
              end
            else
              begin
              center:=(range ['x', 1]+range ['x', 0])*0.5;
              extend:=(range ['y', 1]-range ['y', 0])/yOffset*
                (screenWidth-xOffset)*0.5;
              range['x', 0]:=center-extend;
              range['x', 1]:=center+extend;
              end;
            end
          else
            begin
            center:=(range ['y', 1]+range ['y', 0])*0.5;
            extend:=(range ['x', 1]-range ['x', 0])/(screenWidth-xOffset)*
              yOffset*0.5;
            range['y', 0]:=center-extend;
            range['y', 1]:=center+extend;
            end;
          end
        else if autoscale ['y'] then
          begin
          center:=(range ['x', 1]+range ['x', 0])*0.5;
          extend:=(range ['y', 1]-range ['y', 0])/yOffset*
            (screenWidth-xOffset)*0.5;
          range['x', 0]:=center-extend;
          range['x', 1]:=center+extend;
          end;
      for axis:='x' to 'y' do
        begin
        if range [axis, 1]<range [axis, 0] then
          begin
          center:=range [axis, 0];
          range[axis, 0]:=range [axis, 1];
          range[axis, 1]:=center;
          end;
        if range [axis, 1]<range [axis, 0]+2E-60 then
          begin
          center:=(range [axis, 0]+range [axis, 1])*0.5;
          range[axis, 0]:=center-1E-60;
          range[axis, 1]:=center+1E-60;
          end;
        end;
      recycler.create;
      for i:=0 to length(row)-1 do
        row[i].computeSamplesInActivePlot(true,recycler);
      recycler.destroy;
    end;

  PROCEDURE initTics(CONST axis: char);

    PROCEDURE addTic(CONST realPos: double; CONST realTxt: ansistring; CONST isMajorTic: boolean);
      VAR
        screenPos: double;
      begin
        screenPos:=realToScreen(axis, realPos);
        if not (IsNan(screenPos)) and not (IsInfinite(screenPos)) and  not (screenPos<0) and not ((axis = 'y') and
          (screenPos>screenHeight)) and not ((axis = 'x') and
          (screenPos>screenWidth)) then
          begin
          setLength(tic [axis], length(tic [axis])+1);
          with tic [axis] [length(tic [axis])-1] do
            begin
            pos:=screenPos;
            major:=isMajorTic;
            if major then
              txt:=realTxt
            else
              txt:='';
            end;
          end;
      end;

    FUNCTION pot10(y: int64): T_myFloat; inline;
      VAR
        p10: T_myFloat;
      begin
        if y>=0 then
          p10:=10
        else
          begin
          p10:=0.1;
          y:=-y;
          end;
        result:=1;
        while y>0 do
          begin
          if odd(y) then
            result:=result*p10;
          p10:=p10*p10;
          y:=y shr 1;
          end;
      end;

    FUNCTION niceText(CONST value, scale: longint): string;
      CONST
        suf: array[0..3] of string = ('', '0', '00', '000');
      begin
        if value = 0 then
          exit('0');
        if (scale>=-3) and (scale<0) then
          begin
          result:=IntToStr(value);
          while length(result)<-scale do
            result:='0'+result;
          exit(copy(result, 1, length(result)+scale)+'.'+
            copy(result, length(result)+1+scale, -scale));
          end
        else if (scale>=0) and (scale<3) then
          exit(IntToStr(value)+suf [scale])
        else
          exit(IntToStr(value)+'E'+IntToStr(scale));
      end;

    FUNCTION tooManyTotalTics: boolean;
      CONST
        distanceTol = 5;
      VAR
        i: longint;
        lastPos: double;
      begin
        lastPos:=-1E50;
        for i:=0 to length(tic [axis])-1 do
          begin
          if abs(tic [axis] [i].pos-lastPos)<distanceTol then
            exit(true);
          lastPos:=tic [axis] [i].pos;
          end;
        result:=false;
      end;

    FUNCTION tooManyMajorTics: boolean;
      VAR
        i: longint;
        distanceTol: longint;
        lastPos: double;
      begin
        if axis = 'y' then
          distanceTol:=(screenHeight-yOffset)
        else
          distanceTol:=xOffset;
        lastPos:=-1E50;
        for i:=0 to length(tic [axis])-1 do
          if tic [axis] [i].major then
            begin
            if abs(tic [axis] [i].pos-lastPos)<distanceTol then
              exit(true);
            lastPos:=tic [axis] [i].pos;
            end;
        result:=false;
      end;

    PROCEDURE initLinearTics(CONST power, majorTicRest, minorTicRest: longint);
      VAR
        j: longint;
      begin
        setLength(tic [axis], 0);
        for j:=floor(range [axis, 0]*pot10(-power)) to
          ceil(range [axis, 1]*pot10(-power)) do
          if j mod minorTicRest = 0 then
            addTic(pot10(power)*j, niceText(j div 10, power+1),
              (j mod majorTicRest) = 0);
      end;

    VAR
      i, j: longint;
    begin
      if logscale [axis] then
        begin
        setLength(tic [axis], 0);
        for i:=floor(range [axis, 0]) to ceil(range [axis, 1]) do
          for j:=1 to 9 do
            addTic(pot10(i)*j, niceText(j, i), true);
        if not (tooManyMajorTics) and not (tooManyTotalTics) then
          exit;

        setLength(tic [axis], 0);
        for i:=floor(range [axis, 0]) to ceil(range [axis, 1]) do
          for j:=1 to 9 do
            addTic(pot10(i)*j, niceText(j, i), (j = 1) or (j = 2) or (j = 5));
        if not (tooManyMajorTics) and not (tooManyTotalTics) then
          exit;

        setLength(tic [axis], 0);
        for i:=floor(range [axis, 0]) to ceil(range [axis, 1]) do
          for j:=1 to 9 do
            addTic(pot10(i)*j, niceText(j, i), j = 1);

        if not (tooManyMajorTics) then
          begin
          if tooManyTotalTics then
            begin
            setLength(tic [axis], 0);
            for i:=floor(range [axis, 0]) to ceil(range [axis, 1]) do
              begin
              addTic(pot10(i), niceText(1, i), true);
              addTic(pot10(i)*2, niceText(2, i), false);
              addTic(pot10(i)*5, niceText(5, i), false);
              end;
            end
          else
            exit;
          exit;
          end;

        for j:=2 to 10 do
          begin
          if not (tooManyMajorTics) then
            exit
          else
            setLength(tic [axis], 0);
          for i:=floor(range [axis, 0]) to ceil(range [axis, 1]) do
            addTic(pot10(i), niceText(1, i), (i mod j) = 0);
          end;
        end
      else
        begin
        i:=floor(ln(range [axis, 1]-range [axis, 0])/ln(10))-3;
        repeat
          initLinearTics(i, 10, 1);
          if not (tooManyMajorTics) then
            begin
            if tooManyTotalTics then
              initLinearTics(i, 10, 2)
            else
              exit;
            if tooManyTotalTics then
              initLinearTics(i, 10, 5)
            else
              exit;
            end;
          initLinearTics(i, 20, 1);
          if not (tooManyMajorTics) then
            begin
            if tooManyTotalTics then
              initLinearTics(i, 20, 2)
            else
              exit;
            if tooManyTotalTics then
              initLinearTics(i, 20, 5)
            else
              exit;
            if tooManyTotalTics then
              initLinearTics(i, 20, 10)
            else
              exit;
            end;
          initLinearTics(i, 50, 1);
          if not (tooManyMajorTics) then
            begin
            if tooManyTotalTics then
              initLinearTics(i, 50, 2)
            else
              exit;
            if tooManyTotalTics then
              initLinearTics(i, 50, 5)
            else
              exit;
            if tooManyTotalTics then
              initLinearTics(i, 50, 10)
            else
              exit;
            end;
          Inc(i);
        until false;
        end;
    end;

  begin
    screenWidth:=Width;
    screenHeight:=Height;
    if not wantTics('y') then
      xOffset:=0;
    if not wantTics('x') then
      yOffset:=Height;
    getRanges;
    initTics('x');
    initTics('y');
  end;

FUNCTION T_plot.longtestYTic: ansistring;
  VAR
    i: longint;
  begin
    result:='';
    for i:=0 to length(tic ['y'])-1 do
      with tic ['y', i] do
        if length(txt)>length(result) then
          result:=txt;
    if result = '' then
      result:='.0E';
  end;

FUNCTION T_plot.setTextSize(CONST xTicHeight, yTicWidth: longint): boolean;
  begin
    result:=false;
    if wantTics('y') and (xOffset<>yTicWidth+5) then
      begin
      xOffset:=yTicWidth+5;
      result:=true;
      end;
    if wantTics('x') and (yOffset<>screenHeight-(xTicHeight+5)) then
      begin
      yOffset:=screenHeight-(xTicHeight+5);
      result:=true;
      end;
    if result then
      setScreenSize(screenWidth, screenHeight);
  end;

FUNCTION T_plot.addRow(CONST styleOptions: string): longint;
  begin
    result:=length(row);
    setLength(row, result+1);
    row[result].create(result);
    if Trim(styleOptions)<>'' then
      row[result].style.parseStyle(styleOptions);
  end;

FUNCTION T_plot.wantTics(CONST axis: char): boolean;
  begin
    result:=(axisStyle [axis] and C_tics)>0;
  end;

FUNCTION T_plot.realToScreen(CONST p: T_point): T_point;
  begin
    result:=realToScreen(p [0], p [1]);
  end;

FUNCTION T_plot.realToScreen(CONST x, y: double): T_point;
  begin
    result[0]:=(olx(x)-range ['x', 0])/(range ['x', 1]-range ['x', 0])*
      (screenWidth-xOffset)+xOffset;
    result[1]:=(oly(y)-range ['y', 0])/(range ['y', 1]-range ['y', 0])*
      (-yOffset)+yOffset;
  end;

FUNCTION T_plot.screenToReal(CONST x, y: longint): T_point;
  begin
    result[0]:=oex((x-xOffset)/(screenWidth-xOffset)*
      (range ['x', 1]-range ['x', 0])+range ['x', 0]);
    result[1]:=oey((y-yOffset)/(-yOffset)*
      (range ['y', 1]-range ['y', 0])+range ['y', 0]);
  end;

FUNCTION T_plot.realToScreen(CONST axis: char; CONST p: double): double;
  begin
    if axis = 'x' then
      result:=realToScreen(p, 1) [0]
    else
      result:=realToScreen(1, p) [1];
  end;

FUNCTION T_plot.est_curvature(CONST s0, s1, s2: T_point): double;
  VAR
    p0, p1, p2: T_point;
  begin
    p0:=realToScreen(s0);
    p1:=realToScreen(s1);
    p2:=realToScreen(s2);
    result:=sqr(p0 [0]-2*p1 [0]+p2 [0])+sqr(p0 [1]-2*p1 [1]+p2 [1]);
    if IsNan(result) or IsInfinite(result) then
      result:=0;
  end;

FUNCTION T_plot.olx(CONST x: double): double;
  begin
    if logscale ['x'] then
      begin
      if x<1E-324 then exit(-324);
      result:=ln(x)/ln(10);
      if IsNan(result) or IsInfinite(result) then
        result:=-324;
      end
    else
      result:=x;
  end;

FUNCTION T_plot.oex(CONST x: double): double;
  begin
    if logscale ['x'] then
      result:=exp(x*ln(10))
    else
      result:=x;
  end;

FUNCTION T_plot.oly(CONST y: double): double;
  begin
    if logscale ['y'] then
      begin
      if y<1E-324 then exit(-324);
      result:=ln(y)/ln(10);
      if IsNan(result) or IsInfinite(result) then
        result:=-324;
      end
    else
      result:=y;
  end;

FUNCTION T_plot.oey(CONST y: double): double;
  begin
    if logscale ['y'] then
      result:=exp(y*ln(10))
    else
      result:=y;
  end;

FUNCTION T_plot.isSampleValid(CONST sample: T_point): boolean;
  begin
    result:=not (IsNan(sample [0])) and not (IsInfinite(sample [0])) and
      (not (logscale ['x']) or (sample [0]>=1E-100)) and not
      (IsNan(sample [1])) and not (IsInfinite(sample [1])) and
      (not (logscale ['y']) or (sample [1]>=1E-100));
  end;

PROCEDURE T_plot.setAutoscale(CONST autoX, autoY: boolean);
  begin
    autoscale['x']:=autoX;
    autoscale['y']:=autoY;
  end;

FUNCTION T_plot.getAutoscale: P_listLiteral;
  begin
    result:=newListLiteral;
    result^.append(newBoolLiteral(autoscale ['x']), false);
    result^.append(newBoolLiteral(autoscale ['y']), false);
  end;

PROCEDURE T_plot.setRange(CONST x0, y0, x1, y1: double);
  begin
    autoscale['x']:=false;
    autoscale['y']:=false;
    range['x', 0]:=olx(x0);
    range['x', 1]:=olx(x1);
    range['y', 0]:=oly(y0);
    range['y', 1]:=oly(y1);
  end;

FUNCTION T_plot.getRange: P_listLiteral;
  begin
    result:=newListLiteral;
    result^.append(newListLiteral, false);
    result^.append(newListLiteral, false);
    P_listLiteral(result^.value(0))^.append(newRealLiteral(oex(range ['x', 0])), false);
    P_listLiteral(result^.value(0))^.append(newRealLiteral(oex(range ['x', 1])), false);
    P_listLiteral(result^.value(1))^.append(newRealLiteral(oey(range ['y', 0])), false);
    P_listLiteral(result^.value(1))^.append(newRealLiteral(oey(range ['y', 1])), false);
  end;

PROCEDURE T_plot.setAxisStyle(CONST x, y: longint);
  begin
    if (x>=0) and (x<255) and
      (byte(x) in [0, C_tics, C_grid, C_finerGrid, C_ticsAndGrid, C_ticsAndFinerGrid]) and
      (y>=0) and (y<255) and
      (byte(y) in [0, C_tics, C_grid, C_finerGrid, C_ticsAndGrid, C_ticsAndFinerGrid]) then
      begin
      axisStyle['x']:=x;
      axisStyle['y']:=y;
      end;
  end;

FUNCTION T_plot.getAxisStyle: P_listLiteral;
  begin
    result:=newListLiteral;
    result^.append(newIntLiteral(axisStyle ['x']), false);
    result^.append(newIntLiteral(axisStyle ['y']), false);
  end;

PROCEDURE T_plot.setLogscale(CONST logX, logY: boolean);
  begin
    range['x', 0]:=oex(range ['x', 0]);
    range['x', 1]:=oex(range ['x', 1]);
    range['y', 0]:=oey(range ['y', 0]);
    range['y', 1]:=oey(range ['y', 1]);
    logscale['x']:=logX;
    logscale['y']:=logY;
    range['x', 0]:=olx(range ['x', 0]);
    range['x', 1]:=olx(range ['x', 1]);
    range['y', 0]:=oly(range ['y', 0]);
    range['y', 1]:=oly(range ['y', 1]);
    if logscale ['x']<>logscale ['y'] then
      preserveAspect:=false;
  end;

FUNCTION T_plot.getLogscale: P_listLiteral;
  begin
    result:=newListLiteral;
    result^.append(newBoolLiteral(logscale ['x']), false);
    result^.append(newBoolLiteral(logscale ['y']), false);
  end;

PROCEDURE T_plot.setPreserveAspect(CONST flag: boolean);
  begin
    preserveAspect:=(logscale ['x'] = logscale ['y']) and flag;
  end;

FUNCTION T_plot.getPreserveAspect: P_boolLiteral;
  begin
    result:=newBoolLiteral(preserveAspect);
  end;

PROCEDURE T_plot.zoomOnPoint(CONST pixelX, pixelY: longint; CONST factor: double; VAR plotImage: TImage);
  VAR holdX, holdY: double;
    rectA, rectB: TRect;
  begin
    autoscale['x']:=false;
    autoscale['y']:=false;
    holdX:=(pixelX-xOffset)/(screenWidth-xOffset)*
      (range ['x', 1]-range ['x', 0])+range ['x', 0];
    holdY:=(pixelY-yOffset)/(-yOffset)*
      (range ['y', 1]-range ['y', 0])+range ['y', 0];
    range['x', 0]:=(range ['x', 0]-holdX)*factor+holdX;
    range['x', 1]:=(range ['x', 1]-holdX)*factor+holdX;
    range['y', 0]:=(range ['y', 0]-holdY)*factor+holdY;
    range['y', 1]:=(range ['y', 1]-holdY)*factor+holdY;

    rectA.Top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.Width;
    rectA.Bottom:=plotImage.Height;

    rectB.Top:=round((-pixelY)*factor+pixelY);
    rectB.Left:=round((-pixelX)*factor+pixelX);
    rectB.Right:=round((plotImage.Width-pixelX)*factor+pixelX);
    rectB.Bottom:=round((plotImage.Height-pixelY)*factor+pixelY);

    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
  end;

PROCEDURE T_plot.panByPixels(CONST pixelDX, pixelDY: longint; VAR plotImage: TImage);
  VAR worldDX, worldDY: double;
    rectA, rectB: TRect;
  begin
    autoscale['x']:=false;
    autoscale['y']:=false;
    worldDX:=pixelDX/(screenWidth-xOffset)*(range ['x', 1]-range ['x', 0]);
    worldDY:=pixelDY/(-yOffset)*(range ['y', 1]-range ['y', 0]);
    range['x', 0]:=range ['x', 0]+worldDX;
    range['x', 1]:=range ['x', 1]+worldDX;
    range['y', 0]:=range ['y', 0]+worldDY;
    range['y', 1]:=range ['y', 1]+worldDY;

    rectA.Top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.Width;
    rectA.Bottom:=plotImage.Height;

    rectB.Top:=0+pixelDY;
    rectB.Left:=0+pixelDX;
    rectB.Right:=plotImage.Width+pixelDX;
    rectB.Bottom:=plotImage.Height+pixelDY;

    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
  end;

PROCEDURE T_plot.renderPlot(VAR plotImage: TImage; CONST supersampling: longint);

  PROCEDURE drawGridAndRows(CONST target: TCanvas; CONST scalingFactor: longint);
    VAR
      rowId, i, x, y, yBaseLine, lastX, lastY: longint;
      symSize: double;
      lastWasValid, currentIsValid: boolean;
      sample: T_point;
      patternIdx: byte;
      rowColor: longint;

    PROCEDURE drawPatternRect(x0, y0, x1, y1: longint);
      VAR
        x, y, locY: longint;
      begin
        if (x0<0) then x0:=0 else if x0>screenWidth*scalingFactor then
          x0:=screenWidth*scalingFactor;
        if (x1<0) then x1:=0 else if x1>screenWidth*scalingFactor then
          x1:=screenWidth*scalingFactor;
        if (y0<0) then y0:=0 else if y0>screenHeight*scalingFactor then
          y0:=screenHeight*scalingFactor;
        if (y1<0) then y1:=0 else if y1>screenHeight*scalingFactor then
          y1:=screenHeight*scalingFactor;
        if x1<x0 then
          begin
          x:=x1;
          x1:=x0;
          x0:=x;
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
                target.Pixels[x, y]:=rowColor;
            end
          else
            for y:=yBaseLine downto locY do
              if (x and 1)+2*(y and 1) = patternIdx then
                target.Pixels[x, y]:=rowColor;
          end;
      end;

    begin
      //Clear:------------------------------------------------------------------
      target.Brush.Style:=bsSolid;
      target.Brush.Color:=clWhite;
      target.Pen.Style:=psClear;
      target.Pen.EndCap:=pecSquare;
      target.FillRect(0, 0, target.Width-1, target.Height-1);
      target.Clear;
      //------------------------------------------------------------------:Clear
      //coordinate grid:========================================================
      target.Pen.Style:=psSolid;
      target.Pen.Width:=scalingFactor;
      //minor grid:-------------------------------------------------------------
      target.Pen.Color:=$DDDDDD;
      if (axisStyle ['y'] and C_finerGrid) = C_finerGrid then
        for i:=0 to length(tic ['y'])-1 do with tic ['y'] [i] do if not (major) then
              begin
              y:=round(pos*scalingFactor);
              target.Line(0, y, screenWidth*scalingFactor, y);
              end;
      if (axisStyle ['x'] and C_finerGrid) = C_finerGrid then
        for i:=0 to length(tic ['x'])-1 do with tic ['x'] [i] do if not (major) then
              begin
              x:=round(pos*scalingFactor);
              target.Line(x, 0, x, screenHeight*scalingFactor);
              end;
      //-------------------------------------------------------------:minor grid
      //major grid:-------------------------------------------------------------
      target.Pen.Color:=$BBBBBB;
      if (axisStyle ['y'] and C_grid) = C_grid then
        for i:=0 to length(tic ['y'])-1 do with tic ['y'] [i] do if major then
              begin
              y:=round(pos*scalingFactor);
              target.Line(0, y, screenWidth*scalingFactor, y);
              end;
      if (axisStyle ['x'] and C_grid) = C_grid then
        for i:=0 to length(tic ['x'])-1 do with tic ['x'] [i] do if major then
              begin
              x:=round(pos*scalingFactor);
              target.Line(x, 0, x, screenHeight*scalingFactor);
              end;
      //-------------------------------------------------------------:major grid
      //========================================================:coordinate grid
      //row data:===============================================================
      if logscale ['y'] then
        yBaseLine:=round(realToScreen('y', 1)*scalingFactor)
      else
        yBaseLine:=round(realToScreen('y', 0)*scalingFactor);
      if yBaseLine<0 then
        yBaseLine:=0
      else if yBaseLine>=target.Height then
        yBaseLine:=target.Height-1;
      for rowId:=0 to length(row)-1 do
        begin
        rowColor:=row [rowId].style.getTColor;
        patternIdx:=rowId and 3;

        if row [rowId].style.wantStraightLines then
          begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=row [rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(row [rowId].sample)-1 do
            begin
            sample:=row [rowId].sample [i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              x:=round(sample [0]*scalingFactor);
              y:=round(sample [1]*scalingFactor);
              if lastWasValid then
                begin
                target.LineTo(x, y);
                if row [rowId].style.wantFill then
                  drawPatternRect(lastX, lastY, x, y);
                end
              else
                target.MoveTo(x, y);
              lastX:=x;
              lastY:=y;
              end;
            lastWasValid:=currentIsValid;
            end;
          end
        else if row [rowId].style.wantLeftSteps then
          begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=row [rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(row [rowId].sample)-1 do
            begin
            sample:=row [rowId].sample [i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              x:=round(sample [0]*scalingFactor);
              y:=round(sample [1]*scalingFactor);
              if lastWasValid then
                begin
                target.LineTo(lastX, y);
                target.LineTo(x, y);
                if row [rowId].style.wantFill then
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
        else if row [rowId].style.wantRightSteps then
          begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=row [rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(row [rowId].sample)-1 do
            begin
            sample:=row [rowId].sample [i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              x:=round(sample [0]*scalingFactor);
              y:=round(sample [1]*scalingFactor);
              if lastWasValid then
                begin
                target.LineTo(x, lastY);
                target.LineTo(x, y);
                if row [rowId].style.wantFill then
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
        else if row [rowId].style.wantBars then
          begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=row [rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;

          lastWasValid:=false;
          for i:=0 to length(row [rowId].sample)-1 do
            begin
            sample:=row [rowId].sample [i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              x:=round(sample [0]*scalingFactor);
              y:=round(sample [1]*scalingFactor);
              if lastWasValid then
                begin
                drawPatternRect(round(lastX*0.95+x*0.05), lastY,
                  round(lastX*0.05+x*0.95), lastY);
                target.Line(round(lastX*0.95+x*0.05), yBaseLine,
                  round(lastX*0.95+x*0.05), lastY);
                target.Line(round(lastX*0.95+x*0.05), lastY,
                  round(lastX*0.05+x*0.95), lastY);
                target.Line(round(lastX*0.05+x*0.95), yBaseLine,
                  round(lastX*0.05+x*0.95), lastY);
                end;
              lastX:=x;
              lastY:=y;
              lastWasValid:=currentIsValid;
              end;
            end;

          end
        else if row [rowId].style.wantBoxes then
          begin
          target.Pen.Style:=psClear;
          target.Brush.Style:=bsSolid;
          target.Brush.Color:=rowColor;
          lastWasValid:=false;
          i:=0;
          while i+1<length(row [rowId].sample) do
            begin
            sample:=row [rowId].sample [i];
            if isSampleValid(sample) then
              begin
              sample:=realToScreen(sample);
              lastX:=round(sample [0]*scalingFactor);
              lastY:=round(sample [1]*scalingFactor);
              sample:=row [rowId].sample [i+1];
              if isSampleValid(sample) then
                begin
                sample:=realToScreen(sample);
                x:=round(sample [0]*scalingFactor);
                y:=round(sample [1]*scalingFactor);
                target.FillRect(lastX, lastY, x, y);
                end;
              end;
            Inc(i, 2);
            end;
          end;
        if row [rowId].style.wantDot then
          begin
          target.Pen.Style:=psClear;
          target.Brush.Style:=bsSolid;
          target.Brush.Color:=rowColor;
          symSize:=row [rowId].style.getSymbolWidth*scalingFactor;

          for i:=0 to length(row [rowId].sample)-1 do
            begin
            sample:=row [rowId].sample [i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              //target.Pixels[round(sample[0]*scalingFactor),round(sample[1]*scalingFactor)]:=rowColor;
              target.Ellipse(round(sample [0]*scalingFactor-symSize),
                round(sample [1]*scalingFactor-symSize),
                round(sample [0]*scalingFactor+symSize),
                round(sample [1]*scalingFactor+symSize));
              end;
            end;
          end;
        if row [rowId].style.wantPlus then
          begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=row [rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecSquare;
          symSize:=row [rowId].style.getSymbolWidth*scalingFactor;
          for i:=0 to length(row [rowId].sample)-1 do
            begin
            sample:=row [rowId].sample [i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              target.Line(round(sample [0]*scalingFactor-symSize),
                round(sample [1]*scalingFactor),
                round(sample [0]*scalingFactor+symSize),
                round(sample [1]*scalingFactor));
              target.Line(round(sample [0]*scalingFactor), round(
                sample [1]*scalingFactor-symSize),
                round(sample [0]*scalingFactor),
                round(sample [1]*scalingFactor+symSize));
              end;
            end;
          end;
        if row [rowId].style.wantCross then
          begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=row [rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecSquare;
          symSize:=row [rowId].style.getSymbolRad*scalingFactor;
          for i:=0 to length(row [rowId].sample)-1 do
            begin
            sample:=row [rowId].sample [i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              target.Line(round(sample [0]*scalingFactor-symSize),
                round(sample [1]*scalingFactor-symSize),
                round(sample [0]*scalingFactor+symSize),
                round(sample [1]*scalingFactor+symSize));
              target.Line(round(sample [0]*scalingFactor+symSize),
                round(sample [1]*scalingFactor-symSize),
                round(sample [0]*scalingFactor-symSize),
                round(sample [1]*scalingFactor+symSize));
              end;
            end;
          end;
        if row [rowId].style.wantImpulses then
          begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=row [rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecSquare;
          for i:=0 to length(row [rowId].sample)-1 do
            begin
            sample:=row [rowId].sample [i];
            currentIsValid:=isSampleValid(sample);
            if currentIsValid then
              begin
              sample:=realToScreen(sample);
              target.Line(round(sample [0]*scalingFactor), yBaseLine,
                round(sample [0]*scalingFactor),
                round(sample [1]*scalingFactor));
              end;
            end;
          end;
        end;
      //===============================================================:row data
    end;

  PROCEDURE scale(Source: TImage; VAR dest: TImage; CONST Factor: double);
    VAR
      ARect: TRect;
      X, Y: integer;
    begin
      X:=Round(Source.Width*Factor);
      Y:=Round(Source.Height*Factor);
      Arect:=Rect(0, 0, X, Y);
      dest.Canvas.AntialiasingMode:=amOn;
      dest.Canvas.StretchDraw(ARect, Source.Picture.Bitmap);
    end;

  PROCEDURE drawCoordSys(CONST target: TCanvas);
    VAR
      i, x, y: longint;
    begin
      //coordinate system:======================================================
      //clear border:-----------------------------------------------------------
      target.Brush.Style:=bsSolid;
      target.Brush.Color:=clWhite;
      target.Pen.Style:=psClear;
      target.Pen.Width:=1;
      target.Pen.EndCap:=pecSquare;

      if wantTics('y') then
        target.FillRect(0, 0, xOffset, screenHeight);
      if wantTics('x') then
        target.FillRect(xOffset, yOffset,
          screenWidth, screenHeight);
      //-----------------------------------------------------------:clear border
      //axis:-------------------------------------------------------------------
      target.Pen.Style:=psSolid;
      target.Pen.Color:=clBlack;
      target.Pen.Width:=1;
      if wantTics('y') then
        target.Line(xOffset, 0,
          xOffset, yOffset);
      if wantTics('x') then
        target.Line(screenWidth, yOffset,
          xOffset, yOffset);
      //-------------------------------------------------------------------:axis
      //tics:-------------------------------------------------------------------
      if wantTics('y') then for i:=0 to length(tic ['y'])-1 do
          with tic ['y'] [i] do if major then
              begin
              y:=round(pos);
              target.Line(xOffset-5, y, xOffset, y);
              target.TextOut(xOffset-5-target.TextWidth(txt), y-
                target.TextHeight(txt) shr 1, txt);
              end;
      if wantTics('x') then for i:=0 to length(tic ['x'])-1 do
          with tic ['x'] [i] do if major then
              begin
              x:=round(pos);
              target.Line(x, yOffset+5, x, yOffset);
              target.TextOut(x-target.TextWidth(txt) shr 1, yOffset+5, txt);
              end;
      //-------------------------------------------------------------------:tics
      //======================================================:coordinate system
    end;

  VAR
    renderImage: TImage;
  begin
    repeat
    until not (setTextSize(plotImage.Canvas.TextHeight(longtestYTic),
        plotImage.Canvas.TextWidth(longtestYTic)));

    if supersampling<=1 then
      begin
      drawGridAndRows(plotImage.Canvas, 1);
      drawCoordSys(plotImage.Canvas);
      end
    else
      begin
      renderImage:=TImage.create(plotImage);
      renderImage.SetInitialBounds(0, 0, screenWidth*supersampling,
        screenHeight*supersampling);
      drawGridAndRows(renderImage.Canvas, supersampling);
      scale(renderImage, plotImage, 1/supersampling);
      renderImage.Free;
      drawCoordSys(plotImage.Canvas);
      end;
  end;

FUNCTION addPlot(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  VAR options: ansistring = '';
      sizeWithoutOptions: longint;
      rowId, i, iMax: longint;
      X, Y: P_listLiteral;

  FUNCTION isValidExpression(CONST p:P_literal; VAR recycler:T_tokenRecycler):boolean;
    VAR s:P_subrule;
        par:T_realLiteral;
        res:P_literal;
    begin
      if p^.literalType<>lt_expression then exit(false);
      s:=P_expressionLiteral(p)^.value;
      par.create(random);
      res:=s^.directEvaluateUnary(@par,P_expressionLiteral(s),0,recycler);
      if res=nil then begin
        raiseError(el3_evalError,'Expression to plot '+p^.toString+' is not a valid unary FUNCTION.',tokenLocation);
        result:=false;
      end else if not(res^.literalType in [lt_int,lt_real]) then begin
        raiseError(el3_evalError,'Expression to plot '+p^.toString+' returns '+C_typeString[res^.literalType]+'; should be int or real.',tokenLocation);
        result:=false;
      end else result:=true;
      if res<>nil then disposeLiteral(res);
      par.destroy;
    end;

  VAR recycler:T_tokenRecycler;

  begin
    system.EnterCriticalsection(plotCS);
    result:=nil;
    if (params<>nil) and (params^.size>=1) then begin
      if (params^.value(params^.size-1)^.literalType = lt_string) then begin
        options:=P_stringLiteral(params^.value(params^.size-1))^.value;
        sizeWithoutOptions:=params^.size-1;
      end else begin
        options:='';
        sizeWithoutOptions:=params^.size;
      end;
      if (sizeWithoutOptions = 1) and
         (params^.value(0)^.literalType = lt_list) then begin
        rowId:=activePlot.addRow(options);
        X:=P_listLiteral(params^.value(0));
        for i:=0 to X^.size-1 do
          if (X^.value(i)^.literalType in [lt_intList, lt_realList, lt_numList]) then begin
            Y:=P_listLiteral(X^.value(i));
            if Y^.size = 2
            then activePlot.row[rowId].addSample(fReal(Y^.value(0)), fReal(Y^.value(1)))
            else activePlot.row[rowId].addSample(Nan, Nan);
          end else activePlot.row[rowId].addSample(Nan, Nan);
        result:=newVoidLiteral;
      end else if (sizeWithoutOptions = 1) and
                  (params^.value(0)^.literalType in [lt_intList, lt_realList, lt_numList]) then
        begin
        rowId:=activePlot.addRow(options);
        X:=P_listLiteral(params^.value(0));
        for i:=0 to X^.size-1 do
          activePlot.row[rowId].addSample(i, fReal(X^.value(i)));
        result:=newVoidLiteral;
        end
      else if (sizeWithoutOptions = 2) and
              (params^.value(0)^.literalType in [lt_intList, lt_realList, lt_numList]) and
              (params^.value(1)^.literalType in [lt_intList, lt_realList, lt_numList]) then begin
        rowId:=activePlot.addRow(options);
        X:=P_listLiteral(params^.value(0));
        Y:=P_listLiteral(params^.value(1));
        iMax:=Min(X^.size, Y^.size);
        for i:=0 to iMax-1 do activePlot.row[rowId].addSample(fReal(X^.value(i)), fReal(Y^.value(i)));
        result:=newVoidLiteral;
      end else if (sizeWithoutOptions = 4) and
                  (params^.value(0)^.literalType = lt_expression) and
                  (params^.value(1)^.literalType in [lt_int, lt_real]) and
                  (params^.value(2)^.literalType in [lt_int, lt_real]) and
                  (params^.value(3)^.literalType = lt_int) then begin
        recycler.create;
        if not(isValidExpression(params^.value(0),recycler)) then begin
          recycler.destroy;
          system.LeaveCriticalsection(plotCS);
          exit(nil);
        end;
        rowId:=activePlot.addRow(options);
        activePlot.row[rowId].setRules(
          nil,
          P_expressionLiteral(params^.value(0)),
          fReal(params^.value(1)),
          fReal(params^.value(2)),
          round(fReal(params^.value(3))),
          recycler);
        if not(activePlot.autoscale['x']) and not (activePlot.autoscale['y']) then activePlot.row[rowId].computeSamplesInActivePlot(true,recycler);
        recycler.destroy;
        result:=newVoidLiteral;
      end else if (sizeWithoutOptions = 5) and
                  (params^.value(0)^.literalType = lt_expression) and
                  (params^.value(1)^.literalType = lt_expression) and
                  (params^.value(2)^.literalType in [lt_int, lt_real]) and
                  (params^.value(3)^.literalType in [lt_int, lt_real]) and
                  (params^.value(4)^.literalType = lt_int) then begin
        recycler.create;
        if not(isValidExpression(params^.value(0),recycler)) then begin recycler.destroy; system.LeaveCriticalsection(plotCS); exit(nil); end;
        if not(isValidExpression(params^.value(1),recycler)) then begin recycler.destroy; system.LeaveCriticalsection(plotCS); exit(nil); end;
        rowId:=activePlot.addRow(options);
        activePlot.row[rowId].setRules(
          P_expressionLiteral(params^.value(0)),
          P_expressionLiteral(params^.value(1)),
          fReal(params^.value(2)),
          fReal(params^.value(3)),
          round(fReal(params^.value(4))),
          recycler);
        if not(activePlot.autoscale['x']) and not (activePlot.autoscale['y']) then activePlot.row[rowId].computeSamplesInActivePlot(true,recycler);
        recycler.destroy;
        result:=newVoidLiteral;
      end else raiseError(el3_evalError,'Functions plot and addPlot cannot be applied to parameter list'+
                                                params^.toParameterListString(true),
                                                tokenLocation);
    end else raiseError(el3_evalError,'Functions plot and addPlot cannot be applied to empty parameter list',tokenLocation);
    system.LeaveCriticalsection(plotCS);
  end;

FUNCTION plot(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    system.EnterCriticalsection(plotCS);
    activePlot.Clear;
    if (params<>nil) and (params^.size = 1) and (params^.value(0)^.literalType = lt_emptyList)
    then result:=newVoidLiteral
    else result:=addPlot(params, tokenLocation);
    system.LeaveCriticalsection(plotCS);
  end;

FUNCTION setAutoscale(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
      (params^.value(0)^.literalType = lt_booleanList) and
      (P_listLiteral(params^.value(0))^.size = 2) then begin
      system.EnterCriticalsection(plotCS);
      activePlot.setAutoscale(
        P_boolLiteral(P_listLiteral(params^.value(0))^.value(0))^.value,
        P_boolLiteral(P_listLiteral(params^.value(0))^.value(1))^.value);
      result:=newVoidLiteral;
      system.LeaveCriticalsection(plotCS);
    end else
      raiseError(el3_evalError,
        'Function setPlotAutoscale expects a list of 2 booleans as parameter.',
        tokenLocation);
  end;

FUNCTION getAutoscale(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    result:=activePlot.getAutoscale;
  end;

FUNCTION setLogscale(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
      (params^.value(0)^.literalType = lt_booleanList) and
      (P_listLiteral(params^.value(0))^.size = 2) then begin
      system.EnterCriticalsection(plotCS);
      activePlot.setLogscale(P_boolLiteral(
        P_listLiteral(params^.value(0))^.value(0))^.value,
        P_boolLiteral(P_listLiteral(params^.value(0))^.value(1))^.value);
      result:=newVoidLiteral;
      system.LeaveCriticalsection(plotCS);
    end else
      raiseError(el3_evalError,
        'Function setPlotLogscale expects a list of 2 booleans as parameter.',
        tokenLocation);
  end;

FUNCTION getLogscale(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    result:=activePlot.getLogscale;
  end;

FUNCTION setPlotRange(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  VAR
    x, y: P_literal;
    x0, y0, x1, y1: double;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
      (params^.value(0)^.literalType = lt_list) and
      (P_listLiteral(params^.value(0))^.size = 2) then begin
      x:=P_listLiteral(params^.value(0))^.value(0);
      y:=P_listLiteral(params^.value(0))^.value(1);
      if (x^.literalType in [lt_intList, lt_realList, lt_numList]) and
        (P_listLiteral(x)^.size = 2) and (y^.literalType in
        [lt_intList, lt_realList, lt_numList]) and (P_listLiteral(y)^.size = 2) then begin
        x0:=fReal(P_listLiteral(x)^.value(0));
        x1:=fReal(P_listLiteral(x)^.value(1));
        y0:=fReal(P_listLiteral(y)^.value(0));
        y1:=fReal(P_listLiteral(y)^.value(1));
        if not (IsNan(x0)) and not (IsInfinite(x0)) and not (IsNan(x1)) and  not (IsInfinite(x1)) and not (IsNan(y0)) and not (IsInfinite(y0)) and  not (IsNan(y1)) and not (IsInfinite(y1)) then begin
          system.EnterCriticalsection(plotCS);
          activePlot.setRange(x0, y0, x1, y1);
          result:=newVoidLiteral;
          system.LeaveCriticalsection(plotCS);
        end else raiseError(el3_evalError,'Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
      end else raiseError(el3_evalError,'Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
    end else raiseError(el3_evalError, 'Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
  end;

FUNCTION getPlotRange(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    result:=activePlot.getRange;
  end;

FUNCTION setAxisStyle(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
      (params^.value(0)^.literalType = lt_intList) and
      (P_listLiteral(params^.value(0))^.size = 2) then begin
      system.EnterCriticalsection(plotCS);
      activePlot.setAxisStyle(
        P_intLiteral(P_listLiteral(params^.value(0))^.value(0))^.value,
        P_intLiteral(P_listLiteral(params^.value(0))^.value(1))^.value);
      result:=newVoidLiteral;
      system.LeaveCriticalsection(plotCS);
    end else raiseError(el3_evalError, 'Function setPlotAxisStyle expects a list of 2 integers as parameter.', tokenLocation);
  end;

FUNCTION getAxisStyle(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    result:=activePlot.getAxisStyle;
  end;

FUNCTION setPreserveAspect(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
      (params^.value(0)^.literalType = lt_boolean) then begin
      system.EnterCriticalsection(plotCS);
      activePlot.setPreserveAspect(P_boolLiteral(params^.value(0))^.value);
      result:=newVoidLiteral;
      system.LeaveCriticalsection(plotCS);
    end else raiseError(el3_evalError,'Function setPlotPreserveAspect expects a boolean as parameter.', tokenLocation);
  end;

FUNCTION getPreserveAspect(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    result:=activePlot.getPreserveAspect;
  end;

FUNCTION renderToFile_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  VAR
    plotImage, storeImage: TImage;
    filename: ansistring;
    Width, Height, supersampling: longint;
    rect: TRect;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=3) and
      (params^.value(0)^.literalType = lt_string) and
      (params^.value(1)^.literalType = lt_int) and
      (params^.value(2)^.literalType = lt_int) and
      ((params^.size = 3) or (params^.size = 4) and
      (params^.value(3)^.literalType = lt_int)) then begin
      system.EnterCriticalsection(plotCS);
      filename:=P_stringLiteral(params^.value(0))^.value;
      Width:=P_intLiteral(params^.value(1))^.value;
      Height:=P_intLiteral(params^.value(2))^.value;
      if params^.size>3 then supersampling:=P_intLiteral(params^.value(3))^.value
                        else supersampling:=1;
      if (filename = '') or (Width<1) or (Height<1) or (supersampling<1) then begin
        raiseError(el3_evalError,
          'Function renderToFileImpl expects parameters (filename,width,height,[supersampling]).',
          tokenLocation);
        system.LeaveCriticalsection(plotCS);
        exit(nil);
      end;

      plotImage:=TImage.create(nil);
      plotImage.SetInitialBounds(0, 0, Width, Height);
      activePlot.setScreenSize(Width, Height);
      activePlot.renderPlot(plotImage, supersampling);

      storeImage:=TImage.create(plotImage);
      storeImage.SetInitialBounds(0, 0, Width, Height);
      rect.Top:=0;
      rect.Left:=0;
      rect.Right:=Width;
      rect.Bottom:=Height;
      storeImage.Canvas.CopyRect(rect, plotImage.Canvas, rect);
      storeImage.Picture.PNG.SaveToFile(ChangeFileExt(filename, '.png'));
      storeImage.Free;

      plotImage.Free;
      result:=newVoidLiteral;
      system.LeaveCriticalsection(plotCS);
    end else
      raiseError(el3_evalError,
        'Function renderToFileImpl expects parameters (filename,width,height,[supersampling]).',
        tokenLocation);
  end;


INITIALIZATION
  system.InitCriticalSection(plotCS);
  activePlot.createWithDefaults;
  mnh_funcs.registerRule(PLOT_NAMESPACE,'plot', @plot,
    'plot(list,[options]); //plots flat numeric list or xy-list'+
    '#plot(xList,yList,[options]); //plots flat numeric list or xy-list'+
    '#plot(yExpression,t0,t1,samples,[options]); //plots yExpression versus t in [t0,t1]'+
    '#plot(xExpression,yExpression,t0,t1,samples,[options]); //plots yExpression versus xExpression for t in [t0,t1]'+
    '#options are optional and given in the form of a string, the individual option items being delimited by spaces'+
    '#valid options are:'+'#Style/size modifier: any real number'+
    '#Styles:'+'#  line; l;'+'#  stepLeft;'+'#  stepRight;'+
    '#  fill;'+'#  bar;'+'#  box;'+'#  dot; .;'+'#  plus; +;'+
    '#  cross; x;'+'#  impulse; i;'+'#Colors:'+'#  black;'+
    '#  red;'+'#  blue;'+'#  green;'+'#  purple;'+
    '#  orange;'+'#  RGB$,$,$; //With three real numbers in range [0,1]'+
    '#  HSV$,$,$; //With three real numbers in range [0,1]'+
    '#  HUE$; //With one real number '+
    '#  GREY$; //With one real number in range [0,1]');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'addPlot', @addPlot,
    'addPlot(list,[options]); //adds plot of flat numeric list or xy-list'+
    '#addPlot(xList,yList,[options]); //adds plot of flat numeric list or xy-list'+
    '#addPlot(yExpression,t0,t1,samples,[options]); //adds plot of yExpression versus t in [t0,t1]'+'#addPlot(xExpression,yExpression,t0,t1,samples,[options]); //adds plot of yExpression versus xExpression for t in [t0,t1]');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setPlotAutoscale', @setAutoscale,
    'setPlotAutoscale([forX,forY]);#Sets autoscale per axis and returns true#Expects a tuple of two booleans as parameter.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getPlotAutoscale', @getAutoscale,
    'getPlotAutoscale;#Returns the current autoscale settings per axis as a tuple of two booleans.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setPlotLogscale', @setLogscale,
    'setPlotLogscale([forX,forY]);#Sets log-scale per axis#Expects a tuple of two booleans as parameter.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getPlotLogscale', @getLogscale,
    'getPlotLogscale;#Returns the current log-scale settings per axis as a tuple of two booleans.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setPlotRange', @setPlotRange,
    'setPlotRange([[x0,x1],[y0,y1]]);#Sets the plot-range for the next plot.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getPlotRange', @getPlotRange,
    'getPlotRange;#Returns the plot-range of the last plot as a nested list: [[x0,x1],[y0,y1]]');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setPlotAxisStyle', @setAxisStyle,
    'setPlotAxisStyle([sx,sy]);#Sets the axis style for the next plot. #valid options are:'+
    '#  0; //no tics, no grid#  1; //tics, no gris'+
    '#  2; //no tics, coarse grid#  3; //tics, and coarse grid'+
    '#  6; //no tics, finer grid#  7; //tics and finer grid');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getPlotAxisStyle', @getAxisStyle,
    'getPlotAxisStyle([sx,sy]);#Returns the current axis-style as a tuple of two integers.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setPlotPreserveAspect', @setPreserveAspect,
    'setPlotPreserveAspect(b:boolean);#Sets or un-sets preservation of aspect ratio for the next plot.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getPlotPreserveAspect', @getPreserveAspect,
    'getPlotPreserveAspect;#Returns a boolean indicating whether the aspect ratio will be preserverd for the next plot');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'renderToFile', @renderToFile_impl,
    'renderToFile(filename,width,height,[supersampling]);#Renders the current plot to a file.');
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
    exOverflow, exUnderflow, exPrecision]);

FINALIZATION
  EnterCriticalsection(plotCS);
  activePlot.destroy;
  LeaveCriticalsection(plotCS);
  DoneCriticalsection(plotCS);
end.
