UNIT mnh_plotData;
INTERFACE
USES myFiles,sysutils,math,mnh_litvar;
TYPE
  T_colorChannel=(cc_red,cc_green,cc_blue);

CONST
  C_lineStyle_none     =  0;
  C_lineStyle_straight =  1;
  C_lineStyle_stepLeft =  2;
  C_lineStyle_stepRight=  3;
  C_lineStyle_filled   =  4;
  C_lineStyle_bar      =  8;
  C_lineStyle_box      =  9;
  C_symbolStyle_dot    = 16;
  C_symbolStyle_plus   = 32;
  C_symbolStyle_cross  = 64;
  C_symbolStyle_impulse=128;

  C_anyLineStyle=3;

  C_styleName:array[0..9] of record
    idx:byte;
    name:array[0..1] of string;
  end=((idx:C_lineStyle_straight ; name:('line','l')),
       (idx:C_lineStyle_stepLeft ; name:('stepLeft','')),
       (idx:C_lineStyle_stepRight; name:('stepRight','')),
       (idx:C_lineStyle_filled   ; name:('fill','f')),
       (idx:C_lineStyle_bar      ; name:('bar','')),
       (idx:C_lineStyle_box      ; name:('box','')),
       (idx:C_symbolStyle_dot    ; name:('dot','.')),
       (idx:C_symbolStyle_plus   ; name:('plus','+')),
       (idx:C_symbolStyle_cross  ; name:('cross','x')),
       (idx:C_symbolStyle_impulse; name:('impulse','i')));

TYPE
  T_point=array[0..1] of double;
  T_dataRow=array of T_point;

  { T_style }

  T_style=object(T_serializable)
    //persistent:
    title:string;
    style:byte;
    color:array[T_colorChannel] of byte;
    styleModifier:double;
    CONSTRUCTOR create(CONST index:byte);
    DESTRUCTOR destroy;
    PROCEDURE parseStyle(CONST styleString:ansistring);
    FUNCTION  loadFromFile(VAR F:T_File):boolean; virtual; overload; //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(VAR F:T_File);           virtual; overload; //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei

    FUNCTION getTColor:longint;
    FUNCTION getIntLineWidth:longint;
    FUNCTION getSymbolWidth:double;
    FUNCTION getSymbolRad  :double;
    FUNCTION wantStraightLines:boolean;
    FUNCTION wantLeftSteps:boolean;
    FUNCTION wantRightSteps:boolean;
    FUNCTION wantFill:boolean;
    FUNCTION wantBars:boolean;
    FUNCTION wantBoxes:boolean;
    FUNCTION wantDot:boolean;
    FUNCTION wantPlus:boolean;
    FUNCTION wantCross:boolean;
    FUNCTION wantImpulses:boolean;
  end;

  { T_sampleRow }

  T_boundingBox=array['x'..'y',0..1] of double;

  T_sampleRow=object(T_serializable)
    //persistent:
    style:T_style;
    sample:T_dataRow;
    CONSTRUCTOR create(CONST index:byte);
    PROCEDURE getBoundingBox(CONST logX,logY:boolean; VAR box:T_boundingBox);
    PROCEDURE addSample(CONST x,y:double);
    DESTRUCTOR destroy;
    FUNCTION  loadFromFile(VAR F:T_File):boolean; virtual; overload; //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(VAR F:T_File);           virtual; overload; //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
  end;

  { T_plot }
CONST
  C_tics=1;
  C_grid=2;
  C_finerGrid=C_grid+4;
  C_ticsAndGrid=C_tics+C_grid;
  C_ticsAndFinerGrid=C_tics+C_finerGrid;

TYPE
  T_ticInfo=record
    pos:double;
    major:boolean;
    txt:ansistring;
  end;

  T_plot=object(T_serializable)
    //non-persistent:
    screenWidth,screenHeight:longint;
    xOffset,yOffset:longint;

    tic:array['x'..'y'] of array of T_ticInfo;
    //persistent:
    range:array['x'..'y',0..1] of double;
    axisStyle:array['x'..'y'] of byte;
    autoscale,logscale:array['x'..'y'] of boolean;
    preserveAspect:boolean;
    row:array of T_sampleRow;

    CONSTRUCTOR createWithDefaults;
    PROCEDURE setDefaults;
    DESTRUCTOR destroy;
    PROCEDURE addSampleRow(CONST sampleRow:T_sampleRow);
    PROCEDURE clear;
    FUNCTION  loadFromFile(VAR F:T_File):boolean; virtual; overload; //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(VAR F:T_File);           virtual; overload; //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
    PROCEDURE setScreenSize(CONST width,height:longint);
    FUNCTION longtestYTic:ansistring;
    FUNCTION setTextSize(CONST xTicHeight,yTicWidth:longint):boolean;
    FUNCTION addRow(CONST styleOptions:string):longint;
    //pure query:
    FUNCTION wantTics(CONST axis:char):boolean;
    FUNCTION realToScreen(CONST p :T_point):T_point;
    FUNCTION realToScreen(CONST x,y:double):T_point;
    FUNCTION realToScreen(CONST axis:char; CONST p:double):double;
    FUNCTION olx(CONST x:double):double;
    FUNCTION oex(CONST x:double):double;
    FUNCTION oly(CONST y:double):double;
    FUNCTION oey(CONST y:double):double;
    //interfacing:
    PROCEDURE setAutoscale(CONST autoX,autoY:boolean);
    FUNCTION  getAutoscale:P_listLiteral;
    PROCEDURE setRange(CONST x0,y0,x1,y1:double);
    FUNCTION  getRange:P_listLiteral;
    PROCEDURE setAxisStyle(CONST x,y:longint);
    FUNCTION  getAxisStyle:P_listLiteral;
    PROCEDURE setLogscale(CONST logX,logY:boolean);
    FUNCTION  getLogscale:P_listLiteral;
    PROCEDURE setPreserveAspect(CONST flag:boolean);
    FUNCTION  getPreserveAspect:P_boolLiteral;
  end;

VAR activePlot:T_plot;
IMPLEMENTATION

{ T_sampleRow }

CONSTRUCTOR T_sampleRow.create(CONST index: byte);
  begin
    style.create(index);
    SetLength(sample,0);
  end;

PROCEDURE T_sampleRow.getBoundingBox(CONST logX, logY: boolean;
  VAR box: T_boundingBox);
  VAR i:longint;
      x,y:double;
  begin
    for i:=0 to length(sample)-1 do begin
      x:=sample[i,0]; if logX and (x<0) then x:=NaN;
      y:=sample[i,1]; if logY and (y<0) then y:=NaN;
      if IsNan(box['x',0]) or (not(IsNan(x)) and (x<box['x',0])) then box['x',0]:=x;
      if IsNan(box['x',1]) or (not(IsNan(x)) and (x>box['x',1])) then box['x',1]:=x;
      if IsNan(box['y',0]) or (not(IsNan(y)) and (y<box['y',0])) then box['y',0]:=y;
      if IsNan(box['y',1]) or (not(IsNan(y)) and (y>box['y',1])) then box['y',1]:=y;
    end;
  end;

PROCEDURE T_sampleRow.addSample(CONST x, y: double);
begin
  setLength(sample,length(sample)+1);
  sample[length(sample)-1,0]:=x;
  sample[length(sample)-1,1]:=y;
end;

DESTRUCTOR T_sampleRow.destroy;
  begin
    SetLength(sample,0);
  end;

FUNCTION T_sampleRow.loadFromFile(VAR F: T_File): boolean;
  VAR i1,i:longint;
  begin
    if not(style.loadFromFile(F)) then exit(false);
    i1:=f.readLongint;
    if i1<0 then exit(false);
    setLength(sample,i1);
    for i:=0 to i1-1 do begin
      sample[i,0]:=f.readDouble;
      sample[i,1]:=f.readDouble;
    end;
    result:=f.allOkay;
  end;

PROCEDURE T_sampleRow.saveToFile(VAR F: T_File);
  VAR i:longint;
  begin
    style.saveToFile(F);
    f.writeLongint(length(sample));
    for i:=0 to length(sample)-1 do begin
      f.writeDouble(sample[i,0]);
      f.writeDouble(sample[i,1]);
    end;
  end;

 { T_style }

CONST C_defaultColor:array[0..5] of record name:string; color:array[T_colorChannel] of byte end=
     ((name:'black'; color:(  0,  0,  0)),
      (name:'red';   color:(255,  0,  0)),
      (name:'blue';  color:(  0,  0,255)),
      (name:'green'; color:(  0,128,  0)),
      (name:'purple';color:(192,  0,192)),
      (name:'orange';color:(255, 96,  0)));

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
  FUNCTION parseColorOption(colorOption: string; OUT r,g,b:byte):boolean;
    PROCEDURE HSV2RGB(H,S,V:single; OUT r,g,b:byte);
      VAR hi,p,q,t:byte;
      begin
        while H<0 do H:=H+1;
        while H>1 do H:=H-1;
        hi:=trunc(H*6); H:=H*6-hi;
        V:=V*255;
        p:=round(V*(1-S      ));
        q:=round(V*(1-S*   H ));
        t:=round(V*(1-S*(1-H)));
        case hi of
          0,6: begin r:=round(V); g:=t; b:=p; end;
          1  : begin r:=q; g:=round(V); b:=p; end;
          2  : begin r:=p; g:=round(V); b:=t; end;
          3  : begin r:=p; g:=q; b:=round(V); end;
          4  : begin r:=t; g:=p; b:=round(V); end;
          5  : begin r:=round(V); g:=p; b:=q; end;
        end;
      end;

    VAR rStr:string='';
        gStr:string='';
        bStr:string='';
        i:longint;
        isHSV:boolean;
    begin
      result:=false;
      for i:=0 to length(C_defaultColor)-1 do if colorOption=C_defaultColor[i].name then begin
        color:=C_defaultColor[i].color;
        result:=true;
      end;
      if not(result) and ((copy(colorOption,1,3)='RGB') or (copy(colorOption,1,3)='HSV')) then begin
        isHSV:=(copy(colorOption,1,3)='HSV');
        colorOption:=copy(colorOption,4,length(colorOption)-3);
        i:=pos(',',colorOption);
        if i>0 then begin
          rStr:=copy(colorOption,1,i-1);
          colorOption:=copy(colorOption,i+1,length(colorOption));
          i:=pos(',',colorOption);
        end;
        if i>0 then begin
          gStr:=copy(colorOption,1,i-1);
          colorOption:=copy(colorOption,i+1,length(colorOption));
          i:=pos(',',colorOption);
        end;
        if i>0 then begin
          bStr:=copy(colorOption,1,i-1);
          colorOption:=copy(colorOption,i+1,length(colorOption));
          i:=pos(',',colorOption);
        end else bStr:=colorOption;
        if (rStr<>'') and (gStr<>'') and (bStr<>'') then begin
          if isHSV then HSV2RGB(
            strToFloatDef(rStr,0),
            max(0,min(1,strToFloatDef(gStr,0))),
            max(0,min(1,strToFloatDef(bStr,0))),r,g,b)
          else begin
            r:=round(255*max(0,min(1,strToFloatDef(rStr,0))));
            g:=round(255*max(0,min(1,strToFloatDef(gStr,0))));
            b:=round(255*max(0,min(1,strToFloatDef(bStr,0))));
          end;
          result:=true;
        end;
      end;
      if not(result) and (copy(colorOption,1,3)='HUE') then begin
        colorOption:=copy(colorOption,4,length(colorOption)-3);
        HSV2RGB(strToFloatDef(colorOption,0),1,1,r,g,b);
        result:=true;
      end;
      if not(result) and (copy(colorOption,1,4)='GREY') then begin
        colorOption:=copy(colorOption,5,length(colorOption)-4);
        r:=round(255*max(0,min(1,strToFloatDef(colorOption,0))));
        g:=r;
        b:=r;
        result:=true;
      end;
    end;

  VAR part,options:ansistring;
      sp:longint;
      i:longint;
      size:extended;
      mightBeColor:boolean;
  begin
    style:=0;
    options:=Trim(styleString);
    repeat
      sp:=pos(' ',options);
      if sp<=0 then begin
        part:=options;
        options:=''
      end else begin
        part:=copy(options,1,sp-1);
        options:=trim(copy(options,sp+1,length(options)));
      end;
      mightBeColor:=true;
      size:=StrToFloatDef(part,NaN);
      if not(IsNan(size)) then begin
        styleModifier:=size;
        mightBeColor:=false;
      end;
      for i:=0 to length(C_styleName)-1 do with C_styleName[i] do if (part=name[0]) or (part=name[1]) then begin
        style:=style or idx;
        mightBeColor:=false;
      end;
      if mightBeColor then parseColorOption(part,color[cc_red],color[cc_green],color[cc_blue]);
    until options='';
    if style=0 then style:=C_lineStyle_straight;
  end;

FUNCTION T_style.loadFromFile(VAR F: T_File): boolean;
  VAR cc:T_colorChannel;
  begin
    title:=f.readAnsiString;
    style:=f.readByte;
    for cc:=cc_red to cc_blue do color[cc]:=f.readByte;
    styleModifier:=f.readDouble;
    result:=f.allOkay;
  end;

PROCEDURE T_style.saveToFile(VAR F: T_File);
  VAR cc:T_colorChannel;
  begin
    f.writeAnsiString(title);
    f.writeByte(style);
    for cc:=cc_red to cc_blue do f.writeByte(color[cc]);
    f.writeDouble(styleModifier);
  end;

FUNCTION T_style.getTColor: longint;
begin
  result:=color[cc_red] or (color[cc_green] shl 8) or (color[cc_blue] shl 16);
end;

FUNCTION T_style.getIntLineWidth: longint;
  begin
    result:=trunc(styleModifier);
    if (random<frac(styleModifier)) then inc(result);
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
  result:=(style and C_anyLineStyle)=C_lineStyle_straight;
end;

FUNCTION T_style.wantLeftSteps: boolean;
begin
  result:=(style and C_anyLineStyle)=C_lineStyle_stepLeft;
end;

FUNCTION T_style.wantRightSteps: boolean;
begin
  result:=(style and C_anyLineStyle)=C_lineStyle_stepRight;
end;

FUNCTION T_style.wantFill: boolean;
begin
  result:=(style and C_lineStyle_filled)=C_lineStyle_filled;
end;

FUNCTION T_style.wantBars: boolean;
begin
  result:=style=C_lineStyle_bar;
end;

FUNCTION T_style.wantBoxes: boolean;
begin
  result:=style=C_lineStyle_box;
end;

FUNCTION T_style.wantDot: boolean;
begin
  result:=(style and C_symbolStyle_dot)=C_symbolStyle_dot;
end;

FUNCTION T_style.wantPlus: boolean;
begin
  result:=(style and C_symbolStyle_plus)=C_symbolStyle_plus;
end;

FUNCTION T_style.wantCross: boolean;
begin
  result:=(style and C_symbolStyle_cross)=C_symbolStyle_cross;
end;

FUNCTION T_style.wantImpulses: boolean;
begin
  result:=(style and C_symbolStyle_impulse)=C_symbolStyle_impulse;
end;

CONSTRUCTOR T_plot.createWithDefaults;
  begin
    setDefaults;
  end;

PROCEDURE T_plot.setDefaults;
  VAR axis:char;
  begin
    screenWidth:=200;
    screenHeight:=200;
    xOffset:=20;
    yOffset:=200-20;
    for axis:='x' to 'y' do begin
      range[axis,0]:=-1.5;
      range[axis,1]:= 1.5;
      logscale[axis]:=false;
      axisStyle[axis]:=C_tics;
      autoscale[axis]:=true;
    end;
    preserveAspect:=true;
    clear;
  end;

DESTRUCTOR T_plot.destroy;
  begin clear; end;

PROCEDURE T_plot.addSampleRow(CONST sampleRow: T_sampleRow);
  begin
    setLength(row,length(row)+1);
    row[length(row)-1]:=sampleRow;
  end;

PROCEDURE T_plot.clear;
  VAR i:longint;
  begin
    for i:=0 to length(row)-1 do row[i].destroy;
    setLength(row,0);
  end;

FUNCTION T_plot.loadFromFile(VAR F: T_File): boolean;
  VAR axis:char;
      i,iMax:longint;
  begin
    for axis:='x' to 'y' do begin
      for i:=0 to 1 do range[axis,i]:=f.readDouble;
      axisStyle[axis]:=f.readByte;
      autoscale[axis]:=f.readBoolean;
      logscale[axis]:=f.readBoolean;
    end;
    preserveAspect:=f.readBoolean;
    iMax:=f.readLongint;
    if not(F.allOkay) or (iMax<0) then exit(false);
    clear;
    setLength(row,iMax);
    result:=true;
    for i:=0 to iMax-1 do begin
      row[i].create(i);
      result:=result and row[i].loadFromFile(f);
    end;
  end;

PROCEDURE T_plot.saveToFile(VAR F: T_File);
  VAR axis:char;
      i:longint;
  begin
    for axis:='x' to 'y' do begin
      for i:=0 to 1 do f.writeDouble(range[axis,i]);
      f.writeByte(axisStyle[axis]);
      f.writeBoolean(autoscale[axis]);
      f.writeBoolean(logscale[axis]);
    end;
    f.writeBoolean(preserveAspect);
    f.writeLongint(length(row));
    for i:=0 to length(row)-1 do row[i].saveToFile(f);
  end;

PROCEDURE T_plot.setScreenSize(CONST width, height: longint);
  PROCEDURE getRanges;
    VAR axis:char;
        i:longint;
        center,extend:double;
        boundingBox:T_boundingBox;
    begin
      for axis:='x' to 'y' do for i:=0 to 1 do boundingBox[axis,i]:=Nan;
      for i:=0 to length(row)-1 do row[i].getBoundingBox(logscale['x'],logscale['y'],boundingBox);
      for axis:='x' to 'y' do
        for i:=0 to 1 do begin
          if isNAN(boundingBox[axis,i])
          then boundingBox[axis,i]:=range[axis,i]
          else if logscale[axis]
               then boundingBox[axis,i]:=ln(boundingBox[axis,i])/ln(10);
          if autoscale[axis] then range[axis,i]:=boundingBox[axis,i];
        end;

      if preserveAspect and (autoscale['x'] or autoscale['y']) and (logscale['x']=logscale['y']) then begin
        if autoscale['x'] then begin
          if autoscale['y'] then begin
            if (range['x',1]-range['x',0])/(screenWidth-xOffset)>(range['y',1]-range['y',0])/yOffset then begin
              center:=(range['y',1]+range['y',0])*0.5;
              extend:=(range['x',1]-range['x',0])/(screenWidth-xOffset)*yOffset*0.5;
              range['y',0]:=center-extend;
              range['y',1]:=center+extend;
            end else begin
              center:=(range['x',1]+range['x',0])*0.5;
              extend:=(range['y',1]-range['y',0])/yOffset*(screenWidth-xOffset)*0.5;
              range['x',0]:=center-extend;
              range['x',1]:=center+extend;
            end;
          end else begin
            center:=(range['y',1]+range['y',0])*0.5;
            extend:=(range['x',1]-range['x',0])/(screenWidth-xOffset)*yOffset*0.5;
            range['y',0]:=center-extend;
            range['y',1]:=center+extend;
          end;
        end else if autoscale['y'] then begin
          center:=(range['x',1]+range['x',0])*0.5;
          extend:=(range['y',1]-range['y',0])/yOffset*(screenWidth-xOffset)*0.5;
          range['x',0]:=center-extend;
          range['x',1]:=center+extend;
        end;
      end;
    end;

  PROCEDURE initTics(CONST axis:char);
    PROCEDURE addTic(CONST realPos:double; CONST realTxt:ansistring; CONST isMajorTic:boolean);
      begin

        setLength(tic[axis],length(tic[axis])+1);
        with tic[axis][length(tic[axis])-1] do begin
          pos:=realToScreen(axis,realPos);
          major:=isMajorTic;
          if major then txt:=realTxt else txt:='';
        end;
      end;

    FUNCTION pot10(y:int64):extended; inline;
      VAR p10:extended;
      begin
        if y>=0 then begin
          p10:=10;
        end else begin
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

    FUNCTION niceText(CONST value,scale:longint):string;
      CONST suf:array[0..3] of string=('','0','00','000');
      begin
        if value=0 then exit('0');
        if (scale >=-3) and (scale<0) then begin
          result:=IntToStr(value);
          while length(result)<-scale do result:='0'+result;
          exit(copy(result,1,length(result)  +scale)+'.'+
               copy(result,  length(result)+1+scale,-scale));
        end else if (scale>=0) and (scale<3) then exit(IntToStr(value)+suf[scale])
        else exit(IntToStr(value)+'E'+IntToStr(scale));
      end;

    FUNCTION notToManyTics:boolean;
      VAR i,majors:longint;
      begin
        //too many total tics?
        if length(tic[axis])>100 then exit(false);
        //too many major tics?
        majors:=0;
        for i:=0 to length(tic[axis])-1 do if tic[axis][i].major then inc(majors);
        if majors>20 then exit(false);
        //labels are too close?
        {$WARNING unimplemented}
      end;

    VAR i,j:longint;
    begin
      setLength(tic[axis],0);
      if logscale[axis] then begin
        for i:=floor(range[axis,0]) to ceil(range[axis,1]) do
          for j:=1 to 9 do addTic(pot10(i)*j,niceText(j,i),j=1);
        if length(tic[axis])<50 then exit else setLength(tic[axis],0);
        for i:=floor(range[axis,0]) to ceil(range[axis,1]) do begin
          addTic(pot10(i)  ,niceText(1,i),true);
          addTic(pot10(i)*2,niceText(1,i),false);
          addTic(pot10(i)*5,niceText(1,i),false);
        end;
        if length(tic[axis])<50 then exit else setLength(tic[axis],0);
        for i:=floor(range[axis,0]) to ceil(range[axis,1]) do addTic(pot10(i)  ,niceText(1,i),(i mod 3)=0);
      end else begin
        i:=floor(ln(range[axis,1]-range[axis,0])/ln(10))-1;
        for j:=floor(range[axis,0]*pot10(-i)) to
               ceil (range[axis,1]*pot10(-i)) do begin
          addTic(pot10(i)*j,niceText(j,i),(j mod 10)=0);
        end;
      end;
    end;

  begin
    screenWidth:=width;
    screenHeight:=height;
    if not wantTics('y') then xOffset:=0;
    if not wantTics('x') then yOffset:=0;
    getRanges;
    initTics('x');
    initTics('y');
  end;

FUNCTION T_plot.longtestYTic: ansistring;
  VAR i:longint;
  begin
    result:='';
    for i:=0 to length(tic['y'])-1 do with tic['y',i] do if length(txt)>length(result) then result:=txt;
    if result='' then result:='.0E';
  end;

FUNCTION T_plot.setTextSize(CONST xTicHeight, yTicWidth: longint): boolean;
  begin
    result:=false;
    if wantTics('y') and
     (xOffset<>yTicWidth +5) then begin
      xOffset:=yTicWidth +5;
      result:=true;
    end;
    if wantTics('x') and
     (yOffset<>screenHeight-(xTicHeight+5)) then begin
      yOffset:=screenHeight-(xTicHeight+5);
      result:=true;
    end;
    if result then setScreenSize(screenWidth,screenHeight);
  end;

FUNCTION T_plot.addRow(CONST styleOptions: string): longint;
  begin
    result:=length(row);
    SetLength(row,result+1);
    row[result].create(result);
    if Trim(styleOptions)<>'' then row[result].style.parseStyle(styleOptions);
  end;

FUNCTION T_plot.wantTics(CONST axis: char): boolean;
  begin
    result:=(axisStyle[axis] and C_tics)>0;
  end;

FUNCTION T_plot.realToScreen(CONST p: T_point): T_point;
  begin
    result:=realToScreen(p[0],p[1]);
  end;

FUNCTION T_plot.realToScreen(CONST x, y: double): T_point;
  begin
    if logscale['x']
    then result[0]:=(ln(x)/ln(10)-range['x',0])/(range['x',1]-range['x',0])*(screenWidth-xOffset)+xOffset
    else result[0]:=(   x        -range['x',0])/(range['x',1]-range['x',0])*(screenWidth-xOffset)+xOffset;
    if logscale['y']
    then result[1]:=(ln(y)/ln(10)-range['y',0])/(range['y',1]-range['y',0])*(-yOffset)+yOffset
    else result[1]:=(   y        -range['y',0])/(range['y',1]-range['y',0])*(-yOffset)+yOffset;
  end;

FUNCTION T_plot.realToScreen(CONST axis: char; CONST p: double): double;
  begin
    if axis='x' then result:=realToScreen(p,1)[0]
                else result:=realToScreen(1,p)[1];
  end;

FUNCTION T_plot.olx(CONST x: double): double;
begin
  if logscale['x'] then begin
    result:=ln(x)/ln(10);
    if IsNan(result) or IsInfinite(result) then result:=-324;
  end else result:=x;
end;

FUNCTION T_plot.oex(CONST x: double): double;
begin
  if logscale['x'] then result:=exp(x*ln(10)) else result:=x;
end;

FUNCTION T_plot.oly(CONST y: double): double;
begin
  if logscale['y'] then begin
    result:=ln(y)/ln(10);
    if IsNan(result) or IsInfinite(result) then result:=-324;
  end else result:=y;
end;

FUNCTION T_plot.oey(CONST y: double): double;
begin
  if logscale['y'] then result:=exp(y*ln(10)) else result:=y;
end;

PROCEDURE T_plot.setAutoscale(CONST autoX, autoY: boolean);
begin
  autoscale['x']:=autoX;
  autoscale['y']:=autoY;
end;

FUNCTION T_plot.getAutoscale: P_listLiteral;
begin
  result:=newListLiteral;
  result^.append(newBoolLiteral(autoscale['x']),false);
  result^.append(newBoolLiteral(autoscale['y']),false);
end;

PROCEDURE T_plot.setRange(CONST x0, y0, x1, y1: double);
begin
  autoscale['x']:=false;
  autoscale['y']:=false;
  range['x',0]:=olx(x0);
  range['x',1]:=olx(x1);
  range['y',0]:=oly(y0);
  range['y',1]:=oly(y1);
end;

FUNCTION T_plot.getRange: P_listLiteral;
begin
  result:=newListLiteral;
  result^.append(newListLiteral,false);
  result^.append(newListLiteral,false);
  P_listLiteral(result^.value(0))^.append(newRealLiteral(oex(range['x',0])),false);
  P_listLiteral(result^.value(0))^.append(newRealLiteral(oex(range['x',1])),false);
  P_listLiteral(result^.value(1))^.append(newRealLiteral(oey(range['y',0])),false);
  P_listLiteral(result^.value(1))^.append(newRealLiteral(oey(range['y',1])),false);
end;

PROCEDURE T_plot.setAxisStyle(CONST x, y: longint);
begin
  if (x>=0) and (x<255) and (byte(x) in [C_tics, C_grid, C_finerGrid, C_ticsAndGrid, C_ticsAndFinerGrid]) and
     (y>=0) and (y<255) and (byte(y) in [C_tics, C_grid, C_finerGrid, C_ticsAndGrid, C_ticsAndFinerGrid]) then begin
    axisStyle['x']:=x;
    axisStyle['y']:=y;
  end;
end;

FUNCTION T_plot.getAxisStyle: P_listLiteral;
begin
  result:=newListLiteral;
  result^.append(newIntLiteral(axisStyle['x']),false);
  result^.append(newIntLiteral(axisStyle['y']),false);
end;

PROCEDURE T_plot.setLogscale(CONST logX, logY: boolean);
begin
  range['x',0]:=oex(range['x',0]);
  range['x',1]:=oex(range['x',1]);
  range['y',0]:=oey(range['y',0]);
  range['y',1]:=oey(range['y',1]);
  logscale['x']:=logX;
  logscale['y']:=logY;
  range['x',0]:=olx(range['x',0]);
  range['x',1]:=olx(range['x',1]);
  range['y',0]:=oly(range['y',0]);
  range['y',1]:=oly(range['y',1]);
  if logscale['x']<>logscale['y'] then preserveAspect:=false;
end;

FUNCTION T_plot.getLogscale: P_listLiteral;
begin
  result:=newListLiteral;
  result^.append(newBoolLiteral(logscale['x']),false);
  result^.append(newBoolLiteral(logscale['y']),false);
end;

PROCEDURE T_plot.setPreserveAspect(CONST flag: boolean);
begin
  preserveAspect:=(logscale['x']=logscale['y']) and flag;
end;

FUNCTION T_plot.getPreserveAspect: P_boolLiteral;
begin
  result:=newBoolLiteral(preserveAspect);
end;

INITIALIZATION
  activePlot.createWithDefaults;
FINALIZATION
  activePlot.destroy;
end.
