UNIT mnh_plotData;
INTERFACE
USES myFiles,sysutils,math,myFloat;
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
    FUNCTION wantStraightLines:boolean;
  end;

  { T_sampleRow }

  T_boundingBox=array['x'..'y',0..1] of double;

  T_sampleRow=object(T_serializable)
    //non-persistent:
    sampleTime:array of double;
    //persistent:
    style:T_style;
    sample:T_dataRow;
    computed:boolean;
    CONSTRUCTOR create(CONST index:byte);
    PROCEDURE setComputed;
    FUNCTION isComputed:boolean;
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
    DESTRUCTOR destroy;
    PROCEDURE addSampleRow(CONST sampleRow:T_sampleRow);
    PROCEDURE clear;
    FUNCTION  loadFromFile(VAR F:T_File):boolean; virtual; overload; //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(VAR F:T_File);           virtual; overload; //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
    PROCEDURE setScreenSize(CONST width,height:longint);
    FUNCTION longtestYTic:ansistring;
    FUNCTION setTextSize(CONST xTicHeight,yTicWidth:longint):boolean;
    //pure query:
    FUNCTION wantTics(CONST axis:char):boolean;
    FUNCTION realToScreen(CONST p :T_point):T_point;
    FUNCTION realToScreen(CONST x,y:double):T_point;
    FUNCTION realToScreen(CONST axis:char; CONST p:double):double;
  end;

VAR activePlot:T_plot;
IMPLEMENTATION

{ T_sampleRow }

constructor T_sampleRow.create(const index: byte);
  begin
    style.create(index);
    SetLength(sample,0);
    SetLength(sampleTime,0);
    computed:=false;
  end;

procedure T_sampleRow.setComputed;
  begin
    computed:=true;
  end;

function T_sampleRow.isComputed: boolean;
  begin
    result:=computed;
  end;

procedure T_sampleRow.getBoundingBox(const logX, logY: boolean;
  var box: T_boundingBox);
  VAR i:longint;
      x,y:double;
  begin
    for i:=0 to length(sample)-1 do begin
      x:=sample[i,0]; if computed or logX and (x<0) then x:=NaN;
      y:=sample[i,1]; if             logY and (y<0) then y:=NaN;
      if IsNan(box['x',0]) or (not(IsNan(x)) and (x<box['x',0])) then box['x',0]:=x;
      if IsNan(box['x',1]) or (not(IsNan(x)) and (x>box['x',1])) then box['x',1]:=x;
      if IsNan(box['y',0]) or (not(IsNan(y)) and (y<box['y',0])) then box['y',0]:=y;
      if IsNan(box['y',1]) or (not(IsNan(y)) and (y>box['y',1])) then box['y',1]:=y;
    end;
  end;

procedure T_sampleRow.addSample(const x, y: double);
begin
  setLength(sample,length(sample)+1);
  sample[length(sample)-1,0]:=x;
  sample[length(sample)-1,1]:=y;
end;

destructor T_sampleRow.destroy;
  begin
    SetLength(sample,0);
    SetLength(sampleTime,0);
  end;

function T_sampleRow.loadFromFile(var F: T_File): boolean;
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
    computed:=f.readBoolean;
    result:=f.allOkay;
  end;

procedure T_sampleRow.saveToFile(var F: T_File);
  VAR i:longint;
  begin
    style.saveToFile(F);
    f.writeLongint(length(sample));
    for i:=0 to length(sample)-1 do begin
      f.writeDouble(sample[i,0]);
      f.writeDouble(sample[i,1]);
    end;
    f.writeBoolean(computed);
  end;

 { T_style }

CONST C_defaultColor:array[0..5] of record name:string; color:array[T_colorChannel] of byte end=
     ((name:'black'; color:(  0,  0,  0)),
      (name:'red';   color:(255,  0,  0)),
      (name:'blue';  color:(  0,  0,255)),
      (name:'green'; color:(  0,128,  0)),
      (name:'purple';color:(192,  0,192)),
      (name:'orange';color:(255, 96,  0)));

constructor T_style.create(const index: byte);
  begin
    title:='';
    style:=C_lineStyle_straight;
    color:=C_defaultColor[index mod length(C_defaultColor)].color;
    styleModifier:=1;
  end;

destructor T_style.destroy;
  begin
  end;

procedure T_style.parseStyle(const styleString: ansistring);
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
      cr,cg,cb:byte;

  begin
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
        if style=C_lineStyle_straight then style:=idx else style:=style or idx;
        mightBeColor:=false;
      end;

      if mightBeColor then parseColorOption(part,color[cc_red],color[cc_green],color[cc_blue]);
    until options='';
  end;

function T_style.loadFromFile(var F: T_File): boolean;
  VAR cc:T_colorChannel;
      plausibleStyle:boolean=false;
  begin
    title:=f.readAnsiString;
    style:=f.readByte;
    for cc:=cc_red to cc_blue do color[cc]:=f.readByte;
    styleModifier:=f.readDouble;
    result:=f.allOkay;
  end;

procedure T_style.saveToFile(var F: T_File);
  VAR cc:T_colorChannel;
  begin
    f.writeAnsiString(title);
    f.writeByte(style);
    for cc:=cc_red to cc_blue do f.writeByte(color[cc]);
    f.writeDouble(styleModifier);
  end;

function T_style.getTColor: longint;
begin
  result:=color[cc_red] or (color[cc_green] shl 8) or (color[cc_blue] shl 16);
end;

function T_style.wantStraightLines: boolean;
begin
  result:=(style and C_anyLineStyle)=C_lineStyle_straight;
end;

constructor T_plot.createWithDefaults;
  VAR axis:char;
      i:longint;
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
    end;
    preserveAspect:=true;
    clear;

    setLength(row,1);
    row[0].create(1);

    for i:=-150 to 150 do row[0].addSample(i/100,sin(2*i/100));
  end;

destructor T_plot.destroy;
  begin clear; end;

procedure T_plot.addSampleRow(const sampleRow: T_sampleRow);
  begin
    setLength(row,length(row)+1);
    row[length(row)-1]:=sampleRow;
  end;

procedure T_plot.clear;
  VAR i:longint;
  begin
    for i:=0 to length(row)-1 do with row[i] do setLength(sample,0);
    setLength(row,0);
  end;

function T_plot.loadFromFile(var F: T_File): boolean;
begin
  {$WARNING unimplemented}
end;

procedure T_plot.saveToFile(var F: T_File);
begin
  {$WARNING unimplemented}
end;

procedure T_plot.setScreenSize(const width, height: longint);
  PROCEDURE getRanges;
    VAR axis:char;
        i:longint;
        center,extend:double;
        boundingBox:T_boundingBox;
    begin
      for axis:='x' to 'y' do for i:=0 to 1 do boundingBox[axis,i]:=Nan;
      for i:=0 to length(row)-1 do row[i].getBoundingBox(logscale['x'],logscale['y'],boundingBox);
      for axis:='x' to 'y' do
        for i:=0 to 1 do
          if boundingBox[axis,i]=Nan
          then boundingBox[axis,i]:=range[axis,i]
          else if logscale[axis]
               then boundingBox[axis,i]:=ln(boundingBox[axis,i])/ln(10);

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
          while length(result)<2 do result:='0'+result;
          exit(copy(result,1,length(result)  +scale)+'.'+
               copy(result,  length(result)+1+scale,-scale));
        end else if (scale>=0) and (scale<3) then exit(IntToStr(value)+suf[scale])
        else exit(IntToStr(value)+'E'+IntToStr(scale));
      end;

    FUNCTION notToManyTics:boolean;
      VAR i,majors:longint;
          lastPos:double;
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

function T_plot.longtestYTic: ansistring;
  VAR i:longint;
  begin
    result:='';
    for i:=0 to length(tic['y'])-1 do with tic['y',i] do if length(txt)>length(result) then result:=txt;
    if result='' then result:='.0E';
  end;

function T_plot.setTextSize(const xTicHeight, yTicWidth: longint): boolean;
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

function T_plot.wantTics(const axis: char): boolean;
  begin
    result:=(axisStyle[axis] and C_tics)>0;
  end;

function T_plot.realToScreen(const p: T_point): T_point;
  begin
    result:=realToScreen(p[0],p[1]);
  end;

function T_plot.realToScreen(const x, y: double): T_point;
  begin
    if logscale['x']
    then result[0]:=(ln(x)/ln(10)-range['x',0])/(range['x',1]-range['x',0])*(screenWidth-xOffset)+xOffset
    else result[0]:=(   x        -range['x',0])/(range['x',1]-range['x',0])*(screenWidth-xOffset)+xOffset;
    if logscale['y']
    then result[1]:=(ln(y)/ln(10)-range['y',0])/(range['y',1]-range['y',0])*(-yOffset)+yOffset
    else result[1]:=(   y        -range['y',0])/(range['y',1]-range['y',0])*(-yOffset)+yOffset;
  end;

function T_plot.realToScreen(const axis: char; const p: double): double;
  begin
    if axis='x' then result:=realToScreen(p,1)[0]
                else result:=realToScreen(1,p)[1];
  end;

INITIALIZATION
  activePlot.createWithDefaults;
FINALIZATION
  activePlot.destroy;
end.
