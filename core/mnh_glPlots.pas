UNIT mnh_glPlots;
INTERFACE
USES dos,sysutils,math,gl,glut,mnh_funcs, mnh_litvar, mnh_tokloc,mnh_constants;
TYPE
  T_graphDataSeries= array of array ['x'..'y',false..true] of single;

  T_plotStyle=(ps_naive,ps_line,ps_dot,ps_plus,ps_cross,ps_impulse);

  T_graphData=record
    data :T_graphDataSeries;
    style:set of T_plotStyle;    //default= ps_line
    color:array[0..2] of byte;        //default: first free from table
    styleModifier:single; //line width, point size or symbol size factor; default= 1
  end;

  T_rectData=record
    p:array[0..1,'x'..'y',false..true] of single;
    color:array[0..2] of byte;
  end;


  { T_plot }

  T_plot=object
    graph:array of T_graphData;
    rect:array of T_rectData;
    graphWasAdded:boolean; //if so, limits have to be redetermined
    useAntiAliasing:boolean;

    grid:array['x'..'y'] of array of record
      greyLevel:single;
      position:single;
      ticText:string;
    end;

    sampleRange:array['x'..'y',0..1,false..true] of double;
    logScale   :array['x'..'y'] of boolean;
    screenRes  :array['x'..'y'] of longint;
    preserveAspectRatio:boolean;

    CONSTRUCTOR create;
    DESTRUCTOR  destroy;
    PROCEDURE   updateViewport;
    PROCEDURE   rescale   (newWidth,newHeight:longint);
    PROCEDURE   moveCenter(dx,dy:double);
    PROCEDURE   rezoom    (factorX,factorY,invariantX,invariantY:double);
    PROCEDURE   checkRanges;
    PROCEDURE   addGraph;
    PROCEDURE   addRect(x0,y0,x1,y1:Extended; colorOption:string);
    PROCEDURE   addPoint(x,y:Extended);
    PROCEDURE   setOptions(options:string);
    PROCEDURE   dropGraph;
    PROCEDURE   drawGrid;
    PROCEDURE   drawGraphs;
    PROCEDURE   clear;
    PROCEDURE   screenCoordsToRealCoords(screenX,screenY:longint; OUT realX,realY:double);
  end;

//PROCEDURE drawGrid(VAR img:T_FloatMap; gridColor:T_Vec3; gridCover:double; scaler:T_Scaler);

VAR plot:T_plot;

PROCEDURE activatePlots;
PROCEDURE waitForPlotClose;
IMPLEMENTATION
VAR initialized:longint=0;
    idleSleepTime:longint=0;
    redisplayAtSomePoint:boolean=false;
    renderThreadID:TThreadID;
    lastActivation:double;
    showHelp:boolean=true;
    showGrid:boolean=true;


PROCEDURE waitForPlotClose;
  begin
    while (initialized>0) and (glutGet(GLUT_WINDOW_FORMAT_ID)<>0) do sleep(100);
  end;

PROCEDURE gWrite(x,y:float; s:string);
  VAR i:longint;
  begin
    glRasterpos2f(x,y);
    for i:=1 to length(s) do glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12,ord(s[i]));
  end;

FUNCTION safeLog10(value:double):double; inline;
  CONST invLog10=1/ln(10);
  begin
    if     (value<1E-60) or IsNan(value) then result:=-60
    else if (value>1E60) or IsInfinite(value) then result:=60
    else result:=ln(value)*invLog10;
  end;

FUNCTION safeExp10(value:double):double; inline;
  CONST log10=ln(10);
  begin
    if (value<-60) or IsNan(value) then result:=1E-60
    else if (value>60) or IsInfinite(value) then result:=1E60
    else result:=exp(log10*value);
  end;

CONSTRUCTOR T_plot.create;
  begin
    setLength(graph,0);
    graphWasAdded:=false;
    useAntiAliasing:=false;
    preserveAspectRatio:=true;
    sampleRange['x',0,false]:=0;
    sampleRange['x',0,true ]:=0;
    sampleRange['x',1,false]:=1;
    sampleRange['x',1,true ]:=1;
    sampleRange['y',0,false]:=0;
    sampleRange['y',0,true ]:=0;
    sampleRange['y',1,false]:=1;
    sampleRange['y',1,true ]:=1;
    logScale['x']:=false;
    logScale['y']:=false;
    screenRes['x']:=500;
    screenRes['y']:=500;
    setLength(grid['x'],0);
    setLength(grid['y'],0);    
  end;

DESTRUCTOR T_plot.destroy;
  begin end;

PROCEDURE T_plot.updateViewport;
  CONST aspectTolerance=1.001;
  VAR zoom,
      range,
      center:array['x'..'y'] of double;
      axis:char;
  begin
    if (logScale['x']=logScale['y']) and preserveAspectRatio then begin
      for axis:='x' to 'y' do begin
        range[axis] :=(sampleRange[axis,1,logScale[axis]]-sampleRange[axis,0,logScale[axis]]);
        zoom[axis]  :=range[axis]/screenRes[axis];
        center[axis]:=(sampleRange[axis,1,logScale[axis]]+sampleRange[axis,0,logScale[axis]])*0.5;
      end;
      if zoom['x']>aspectTolerance*zoom['y'] then begin
        zoom['y'] :=zoom['x'];
        range['y']:=zoom['y']*screenRes['y'];
        sampleRange['y',0,logScale['y']]:=center['y']-0.5*range['y'];
        sampleRange['y',1,logScale['y']]:=center['y']+0.5*range['y'];
      end else if zoom['y']>aspectTolerance*zoom['x'] then begin
        zoom['x'] :=zoom['y'];
        range['x']:=zoom['x']*screenRes['x'];
        sampleRange['x',0,logScale['x']]:=center['x']-0.5*range['x'];
        sampleRange['x',1,logScale['x']]:=center['x']+0.5*range['x'];
      end;
    end;
    if initialized>0 then begin
      glLoadIdentity;
      glOrtho(sampleRange['x',0,logScale['x']],
              sampleRange['x',1,logScale['x']],
              sampleRange['y',0,logScale['y']],
              sampleRange['y',1,logScale['y']], -10.0, 10.0);
      glMatrixMode(GL_MODELVIEW);
      glutPostRedisplay;
    end;
    setLength(grid['x'],0);
    setLength(grid['y'],0);
  end;

PROCEDURE T_plot.rescale   (newWidth,newHeight:longint);
  begin
    screenRes['x']:=newWidth;
    screenRes['y']:=newHeight;

    glViewport(0, 0,newWidth,newHeight);
    updateViewport;
  end;

PROCEDURE T_plot.moveCenter(dx,dy:double);
//  VAR delta:array['x'..'y'] of double;
//      axis:char;
  begin
    dx:= dx/screenRes['x']*(sampleRange['x',1,logScale['x']]-sampleRange['x',0,logScale['x']]);
    dy:=-dy/screenRes['y']*(sampleRange['y',1,logScale['y']]-sampleRange['y',0,logScale['y']]);
    sampleRange['x',0,logScale['x']]:=sampleRange['x',0,logScale['x']]-dx;
    sampleRange['x',1,logScale['x']]:=sampleRange['x',1,logScale['x']]-dx;
    sampleRange['y',0,logScale['y']]:=sampleRange['y',0,logScale['y']]-dy;
    sampleRange['y',1,logScale['y']]:=sampleRange['y',1,logScale['y']]-dy;
    updateViewport;
  end;

PROCEDURE T_plot.rezoom(factorX,factorY,invariantX,invariantY:double);
  VAR center,stretch,factor,invariant:double;
      axis:char;
  begin
    for axis:='x' to 'y' do begin
      if axis='x'
        then factor:=factorX
        else factor:=factorY;
      if axis='x'
        then invariant:=  invariantX/screenRes['x']
        else invariant:=1-invariantY/screenRes['y'];
      center :=(sampleRange[axis,1,logScale[axis]]*invariant+sampleRange[axis,0,logScale[axis]]*(1-invariant));
      stretch:=(sampleRange[axis,1,logScale[axis]]-sampleRange[axis,0,logScale[axis]]);
      stretch:=stretch*factor;
      sampleRange[axis,0,logScale[axis]]:=center-stretch*invariant;
      sampleRange[axis,1,logScale[axis]]:=center+stretch*(1-invariant);
    end;
    updateViewport;
  end;

PROCEDURE T_plot.checkRanges;
  VAR i,j:longint;
      axis:char;
      log:boolean;
  begin
    for i:=0 to length(graph)-1 do for j:=0 to length(graph[i].data)-1 do begin
      if graphWasAdded then begin
        for axis:='x' to 'y' do
        for log:=false to true do begin
          sampleRange[axis,0,log]:=graph[i].data[j,axis,log];
          sampleRange[axis,1,log]:=graph[i].data[j,axis,log];
        end;
        graphWasAdded:=false;
      end;
      for axis:='x' to 'y' do for log:=false to true do begin
        if sampleRange[axis,0,log]> graph[i].data[j,axis,log] then
           sampleRange[axis,0,log]:=graph[i].data[j,axis,log];
        if sampleRange[axis,1,log]< graph[i].data[j,axis,log] then
           sampleRange[axis,1,log]:=graph[i].data[j,axis,log];
      end;
    end;
    for i:=0 to length(rect)-1 do with rect[i] do
    for j:=0 to 1 do for axis:='x' to 'y' do for log:=false to true do begin
      if sampleRange[axis,0,log]> p[j,axis,log] then
         sampleRange[axis,0,log]:=p[j,axis,log];
      if sampleRange[axis,1,log]< p[j,axis,log] then
         sampleRange[axis,1,log]:=p[j,axis,log];
    end;
    graphWasAdded:=false;
    updateViewport;
  end;

PROCEDURE T_plot.addGraph;
  VAR newGraphIndex:longint;
  begin
    newGraphIndex:=length(graph);
    setLength(graph,newGraphIndex+1);
    setLength(graph[newGraphIndex].data,0);
    graph[newGraphIndex].style:=[ps_naive];
    graph[newGraphIndex].color[0]:=0;
    graph[newGraphIndex].color[1]:=0;
    graph[newGraphIndex].color[2]:=0;
    graph[newGraphIndex].styleModifier:=1;
    graphWasAdded:=true;
  end;

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

  CONST C_colorOptions:array [0..3] of record
    id:string;
    r,g,b:byte;
  end=((id:'black'; r:0; g:0; b:0),
       (id:'red';   r:255; g:0; b:0),
       (id:'green'; r:0; g:190; b:0),
       (id:'blue';  r:0; g:0; b:255));
  VAR rStr:string='';
      gStr:string='';
      bStr:string='';
      i:longint;
      isHSV:boolean;
  begin
    result:=false;
    for i:=0 to length(C_colorOptions)-1 do if colorOption=C_colorOptions[i].id then begin
      r:=C_colorOptions[i].r;
      g:=C_colorOptions[i].g;
      b:=C_colorOptions[i].b;
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

PROCEDURE T_plot.addRect(x0, y0, x1, y1: Extended; colorOption: string);
  begin
    setLength(rect,length(rect)+1);
    with rect[length(rect)-1] do begin
      p[0,'x',false]:=x0;
      p[1,'x',false]:=x1;
      p[0,'y',false]:=y0;
      p[1,'y',false]:=y1;
      p[0,'x',true]:=safeLog10(x0);
      p[1,'x',true]:=safeLog10(x1);
      p[0,'y',true]:=safeLog10(y0);
      p[1,'y',true]:=safeLog10(y1);
      parseColorOption(colorOption,color[0],color[1],color[2]);
    end;
  end;

PROCEDURE T_plot.addPoint(x,y:Extended);
  VAR i,j:longint;
  begin
    if not(IsInfinite(x)) and not(isNan(x)) and
       not(IsInfinite(y)) and not(isNan(y)) then begin
      i:=length(graph)-1;
      j:=length(graph[i].data);
      setLength(graph[i].data,j+1);
      graph[i].data[j,'x',false]:=x;
      graph[i].data[j,'y',false]:=y;
      graph[i].data[j,'x',true]:=safeLog10(x);
      graph[i].data[j,'y',true]:=safeLog10(y);
    end;
  end;

PROCEDURE T_plot.setOptions(options: string);
  CONST C_styleOptions:array [0..4] of record
          id:string;
          style:T_plotStyle;
        end=((id:'l'; style:ps_line),
             (id:'.'; style:ps_dot),
             (id:'+'; style:ps_plus),
             (id:'x'; style:ps_cross),
             (id:'i'; style:ps_impulse));
  VAR part:ansistring;
      sp:longint;
      i:longint;
      size:extended;
      mightBeColor:boolean;
      cr,cg,cb:byte;
  begin
    options:=Trim(options);
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
        graph[length(graph)-1].styleModifier:=size;
        mightBeColor:=false;
      end;

      for i:=0 to length(C_styleOptions)-1 do with C_styleOptions[i] do
        if part=id then begin
          if graph[length(graph)-1].style=[ps_naive]
            then graph[length(graph)-1].style:=[style]
            else graph[length(graph)-1].style:=graph[length(graph)-1].style + [style];
          mightBeColor:=false;
        end;

      if mightBeColor and parseColorOption(part,cr,cg,cb) then begin
        graph[length(graph)-1].color[0]:=cr;
        graph[length(graph)-1].color[1]:=cg;
        graph[length(graph)-1].color[2]:=cb;
      end;
    until options='';
  end;

//PROCEDURE T_plot.setColor(r,g,b:single);
//  VAR intColor:longint;
//  begin
//    if r<0 then r:=0 else if r>1 then r:=1;
//    if g<0 then g:=0 else if g>1 then g:=1;
//    if b<0 then b:=0 else if b>1 then b:=1;
//
//    graph[length(graph)-1].color[0]:=round(255*r);
//    graph[length(graph)-1].color[1]:=round(255*g);
//    graph[length(graph)-1].color[2]:=round(255*b);
//  end;

PROCEDURE T_plot.dropGraph;
  begin
    setLength(graph,length(graph)-1);
  end;

PROCEDURE T_plot.drawGrid;
  PROCEDURE prepareGrid;
    CONST logSubGrid:array[1..9] of double=(ln(1)/ln(10),ln(2)/ln(10),ln(3)/ln(10),ln(4)/ln(10),ln(5)/ln(10),ln(6)/ln(10),ln(7)/ln(10),ln(8)/ln(10),ln(9)/ln(10));    
    VAR i,j:longint;

        cover_5,
        cover_1,
        currentCover,
        range,
        origin,
        step:double;
        axis:char;
    begin
      for axis:='x' to 'y' do if logscale[axis] then begin
        setLength(grid[axis],0);
        origin:=sampleRange[axis,0,logScale[axis]];
        range :=sampleRange[axis,1,logScale[axis]]-origin;
        step  :=exp(floor(ln(range)/ln(2))*ln(2));
        cover_1 :=min(1,max(0,(ln(range/step)-ln( 2))/(ln(1)-ln( 2))));
        cover_5 :=min(1,max(0,(ln(range/step)-ln( 4))/(ln(2)-ln( 4))));
        origin:=floor(origin/step)*step;
        step:=step*0.25;
        if step>0.26 then
        for i:=0 to 200 do begin
          if      i mod 4=0 then begin currentCover:=1;       end
          else if i mod 2=0 then begin currentCover:=cover_5; end
          else                   begin currentCover:=cover_1; end;
          if (currentCover>0) and
             (origin+step*i>sampleRange[axis,0,logScale[axis]]) and
             (origin+step*i<sampleRange[axis,1,logScale[axis]]) then begin
            setLength(grid[axis],length(grid[axis])+1);
            with grid[axis][length(grid[axis])-1] do begin
              greyLevel:=1-0.5*currentCover;
              position:=origin+step*i;
              if i mod 2=0 then ticText:='1E'+IntToStr(round(origin+step*i))
                           else ticText:='';
            end;
          end;
        end else for i:=0 to 20 do for j:=1 to 9 do begin
          if j=1                 then currentCover:=1
          else if (j=2) or (j=5) then currentCover:=cover_5
          else                        currentCover:=cover_1;
          if (currentCover>0) and
             (origin+(i+logSubGrid[j])>sampleRange[axis,0,logScale[axis]]) and
             (origin+(i+logSubGrid[j])<sampleRange[axis,1,logScale[axis]]) then begin
            setLength(grid[axis],length(grid[axis])+1);
            with grid[axis][length(grid[axis])-1] do begin
              greyLevel:=1-0.5*currentCover;
              position:=origin+(i+logSubGrid[j]);
              if j=1 then ticText:='1E'+IntToStr(round(origin+i))
                     else ticText:=intToStr(j)+'E'+IntToStr(round(origin+i))
            end;
          end;
        end;
      end else begin
        setLength(grid[axis],0);
        origin:=sampleRange[axis,0,logScale[axis]];
        range :=sampleRange[axis,1,logScale[axis]]-origin;
        step  :=exp(floor(ln(range)/ln(10))*ln(10));
        cover_1 :=min(1,max(0,(ln(range/step)-ln( 5))/(ln(1)-ln( 5))));
        cover_5 :=min(1,max(0,(ln(range/step)-ln(10))/(ln(5)-ln(10))));
        origin:=floor(origin/step)*step;
        step:=step*0.1;
        for i:=0 to 200 do begin
          if      i mod 10=0 then begin currentCover:=1;       end
          else if i mod  5=0 then begin currentCover:=cover_5; end
          else                    begin currentCover:=cover_1; end;
          if (currentCover>0) and
             (origin+step*i>sampleRange[axis,0,logScale[axis]]) and
             (origin+step*i<sampleRange[axis,1,logScale[axis]]) then begin
            setLength(grid[axis],length(grid[axis])+1);
            with grid[axis][length(grid[axis])-1] do begin
              greyLevel:=1-0.5*currentCover;
              position:=origin+step*i;
              if (i mod 5=0) and (currentCover>0.5)
                 then ticText:=FloatToStr(origin+step*i)
                 else ticText:='';
            end;
          end;
        end;
      end;
    end;

  VAR i:longint;
      ticAxis:double;
  begin
    glDisable(GL_LINE_SMOOTH);
    glLineWidth(1);
    glPointSize(1);
    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if length(grid['x'])=0 then prepareGrid;
    glBegin(GL_LINES);
    for i:=0 to length(grid['x'])-1 do with grid['x'][i] do begin
      glColor4f(0,0,0,1-greylevel);
      glVertex3f(position,sampleRange['y',0,logScale['y']],-1-greyLevel);
      glVertex3f(position,sampleRange['y',1,logScale['y']],-1-greyLevel);
    end;
    for i:=0 to length(grid['y'])-1 do with grid['y'][i] do begin
      glColor4f(0,0,0,1-greylevel);
      glVertex3f(sampleRange['x',0,logScale['x']],position,-1-greyLevel);
      glVertex3f(sampleRange['x',1,logScale['x']],position,-1-greyLevel);
    end;
    glEnd;
    glColor3f(0,0,0);
    ticAxis:=sampleRange['y',0,logscale['y']]+(sampleRange['y',1,logscale['y']]-sampleRange['y',0,logscale['y']])*10/screenRes['y'];
    for i:=0 to length(grid['x'])-1 do with grid['x'][i] do gWrite(position,ticAxis,ticText);
    ticAxis:=sampleRange['x',0,logscale['x']]+(sampleRange['x',1,logscale['x']]-sampleRange['x',0,logscale['x']])*10/screenRes['x'];
    for i:=0 to length(grid['y'])-1 do with grid['y'][i] do gWrite(ticAxis,position,ticText);
  end;

PROCEDURE T_plot.drawGraphs;
  VAR graphIndex,i:longint;
      baseLine:double;
      threePixelsX,threePixelsY:double;
  begin
    
    threePixelsX:=(sampleRange['x',1,logscale['x']]-sampleRange['x',0,logscale['x']])*3/screenRes['x'];
    threePixelsY:=(sampleRange['y',1,logscale['y']]-sampleRange['y',0,logscale['y']])*3/screenRes['y'];
    glBegin(GL_QUADS);
    for graphIndex:=0 to length(rect)-1 do with rect[graphIndex] do begin
      glColor3f(color[0]/255,
                color[1]/255,
                color[2]/255);
      glVertex3f(p[0,'x',logscale['x']],p[0,'y',logscale['y']],-3);
      glVertex3f(p[1,'x',logscale['x']],p[0,'y',logscale['y']],-3);
      glVertex3f(p[1,'x',logscale['x']],p[1,'y',logscale['y']],-3);
      glVertex3f(p[0,'x',logscale['x']],p[1,'y',logscale['y']],-3);
    end;
    glEnd;
    if showGrid then plot.drawGrid;

    if useAntiAliasing then begin
      glEnable(GL_LINE_SMOOTH);
      glEnable(GL_POINT_SMOOTH);
    end else
      glDisable(GL_POINT_SMOOTH);
    for graphIndex:=0 to length(graph)-1 do with graph[graphIndex] do begin
      glColor3f(color[0]/255,
                color[1]/255,
                color[2]/255);
      glLineWidth(styleModifier);
      glPointSize(styleModifier);
      if (ps_line in style) or (ps_naive in style) then begin
        glBegin(GL_LINE_STRIP);
          for i:=0 to length(data)-1 do begin
            glVertex2f(data[i,'x',logScale['x']],data[i,'y',logScale['y']]);
          end;
        glEnd;
      end;
      if ps_dot in style then begin
        glBegin(GL_POINTS);
          for i:=0 to length(data)-1 do begin
            glVertex2f(data[i,'x',logScale['x']],data[i,'y',logScale['y']]);
          end;
        glEnd;
      end;
      if ps_plus in style then begin
        glBegin(GL_LINES);
          for i:=0 to length(data)-1 do begin
            glVertex2f(data[i,'x',logScale['x']]-styleModifier*threePixelsX,data[i,'y',logScale['y']]);
            glVertex2f(data[i,'x',logScale['x']]+styleModifier*threePixelsX,data[i,'y',logScale['y']]);
            glVertex2f(data[i,'x',logScale['x']],data[i,'y',logScale['y']]-styleModifier*threePixelsY);
            glVertex2f(data[i,'x',logScale['x']],data[i,'y',logScale['y']]+styleModifier*threePixelsY);
          end;
        glEnd;
      end;
      if ps_cross in style then begin
        glBegin(GL_LINES);
          for i:=0 to length(data)-1 do begin
            glVertex2f(data[i,'x',logScale['x']]-styleModifier*threePixelsX,data[i,'y',logScale['y']]-styleModifier*threePixelsY);
            glVertex2f(data[i,'x',logScale['x']]+styleModifier*threePixelsX,data[i,'y',logScale['y']]+styleModifier*threePixelsY);
            glVertex2f(data[i,'x',logScale['x']]+styleModifier*threePixelsX,data[i,'y',logScale['y']]-styleModifier*threePixelsY);
            glVertex2f(data[i,'x',logScale['x']]-styleModifier*threePixelsX,data[i,'y',logScale['y']]+styleModifier*threePixelsY);
          end;
        glEnd;
      end;
      if ps_impulse in style then begin
        if logScale['y'] then baseLine:=sampleRange['y',0,true]
                         else begin
          baseLine:=min(sampleRange['y',1,false],max(sampleRange['y',0,false],0));
        end;
        glBegin(GL_LINES);
          for i:=0 to length(data)-1 do begin
            glVertex2f(data[i,'x',logScale['x']],data[i,'y',logScale['y']]);
            glVertex2f(data[i,'x',logScale['x']],baseLine);
          end;
        glEnd;
      end;
    end;
  end;

PROCEDURE T_plot.clear;
  begin
    setLength(graph,0);
    setLength(rect,0);
  end;

PROCEDURE T_plot.screenCoordsToRealCoords(screenX,screenY:longint; OUT realX,realY:double);
  begin
    //screen to relative:
    realX:=  screenX/screenRes['x'];
    realY:=1-screenY/screenRes['y'];

    //relative to absolute
    realX:=(1-realX)*sampleRange['x',0,logScale['x']]+realX*sampleRange['x',1,logScale['x']];
    realY:=(1-realY)*sampleRange['y',0,logScale['y']]+realY*sampleRange['y',1,logScale['y']];

    //exponentiate if necessary
    if logScale['x'] then realX:=safeExp10(realX);
    if logScale['y'] then realY:=safeExp10(realY);
  end;

PROCEDURE reshape(xRes,yRes:longint); cdecl;
  begin
    plot.rescale(xres,yres);
  end;

PROCEDURE idle; cdecl;
  begin
    if redisplayAtSomePoint and (now-lastActivation>1/(24*60*60*10)) then begin
      glutPostRedisplay;
      redisplayAtSomePoint:=false;
    end;
    if idleSleepTime<1000 then inc(idleSleepTime,10);
    sleep(idleSleepTime);
  end;

PROCEDURE keyboard(key:byte; x,y:longint); cdecl;
  begin
    idleSleepTime:=0;
    case chr(key) of
      '+': plot.rezoom(1/1.1,1/1.1,x,y);
      '-': plot.rezoom(  1.1,  1.1,x,y);
      'x','y': begin plot.logScale[chr(key)]:=not(plot.logScale[chr(key)]); plot.updateViewport; end;
      'a','A': begin plot.useAntiAliasing:=not(plot.useAntiAliasing); glutPostRedisplay; end;
      'l','L': begin plot.preserveAspectRatio:=not(plot.preserveAspectRatio); plot.graphWasAdded:=true; plot.checkRanges; end;
      'h','H': begin showHelp:=not(showHelp); glutPostRedisplay; end;
      'g','G': begin showGrid:=not(showGrid); glutPostRedisplay; end;
    end;
  end;

PROCEDURE draw; cdecl;
  CONST helpLines:array [0..7] of string=
   ('[+/-] Zoom in/out',
    '[x/y] Toggle logscale x/y',
    '[A] Toggle anti-aliasing',
    '[L] Toggle aspect-ratio lock',
    '[H] Show/hide this help',
    '[G] Show/hide grid',
    'Draw with left mouse button to move',
    'Draw with right mouse button to scale');

  PROCEDURE displayHelp;
    PROCEDURE gWrite(x,y:float; s:string);
      VAR i:longint;
      begin
        glRasterpos2f(x,y);
        for i:=1 to length(s) do glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12,ord(s[i]));
      end;

    VAR x0,y0,dy:double;
        i:longint;
    begin
      x0:=plot.sampleRange['x',0,plot.logscale['x']]+(plot.sampleRange['x',1,plot.logscale['x']]-plot.sampleRange['x',0,plot.logscale['x']])*10/plot.screenRes['x'];
      dy:=                                          -(plot.sampleRange['y',1,plot.logscale['y']]-plot.sampleRange['y',0,plot.logscale['y']])*16/plot.screenRes['y'];
      y0:=plot.sampleRange['y',1,plot.logscale['y']]+dy;
      glColor3f(0,0,0);
      for i:=0 to length(helpLines)-1 do
        gWrite(x0,y0+i*dy,helpLines[i]);
    end;

  begin
    if plot.graphWasAdded then plot.checkRanges;
    glClearColor(1,1,1,0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    plot.drawGraphs;
    if showHelp then displayHelp;
    glutswapbuffers;
  end;

VAR mouseX,mouseY,mouseButton,mouseDownX,mouseDownY:longint;

PROCEDURE mousePress(button,state,x,y:longint); cdecl;
  begin
    idleSleepTime:=0;
    mouseX:=x;
    mouseY:=y;
    mouseDownX:=x;
    mouseDownY:=y;
    mouseButton:=button;
  end;

PROCEDURE mouseMove(x,y:longint); cdecl;
  begin
    idleSleepTime:=0;
    if mouseButton=GLUT_LEFT_BUTTON then plot.moveCenter(x-mouseX,y-mouseY);
    if mouseButton=GLUT_RIGHT_BUTTON then begin
      plot.preserveAspectRatio:=false;
      if abs(mouseDownX-x)>abs(mouseDownY-y)
        then plot.rezoom(exp((mouseX-x)*0.01),1,plot.screenRes['x']/2,plot.screenRes['y']/2 )
        else plot.rezoom(1,exp((y-mouseY)*0.01),plot.screenRes['x']/2,plot.screenRes['y']/2 );
    end;
    mouseX:=x;
    mouseY:=y;
  end;

PROCEDURE mouseMovePassive(x,y:longint); cdecl;
  VAR rx,ry:double;
      newTitle:ShortString;
  begin
    idleSleepTime:=0;
    plot.screenCoordsToRealCoords(x,y,rx,ry);
    newTitle:=FloatToStr(rx)+', '+FloatToStr(ry)+#0;
    glutSetWindowTitle(@newTitle[1]);
  end;

PROCEDURE initOpenGlPlots;
begin
  glutInitWindowSize(400,400);
  glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGBA + GLUT_DEPTH);
  glutCreateWindow('Calculator plots');
  glClearColor(1.0, 1.0, 1.0, 0.0);
  glOrtho(0, 2, -1, 1, -10.0, 10.0);
  glMatrixMode(GL_MODELVIEW);
  glutDisplayFunc(@draw);
  glutKeyboardFunc(@keyboard);
  glutReshapeFunc(@reshape);
  glutMouseFunc(@mousePress);
  glutMotionFunc(@mouseMove);
  glutPassiveMotionFunc(@mouseMovePassive);
  glutIdleFunc(@idle);
  glutSetCursor(GLUT_CURSOR_CROSSHAIR);
  glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glEnable(GL_DEPTH_TEST);

end;


FUNCTION plottingThread(p:pointer):PtrInt;
  begin
    initOpenGlPlots;
    interlockedIncrement(initialized);
    glutmainloop;
    result:=0;
  end;

PROCEDURE activatePlots;
  begin
    if initialized=0 then begin
      renderThreadID:=BeginThread(@plottingThread,nil);
      repeat sleep(10) until initialized>0;
    end else if glutGet(GLUT_WINDOW_FORMAT_ID)=0 then begin
      KillThread(renderThreadID);
      initialized:=0;
      activatePlots;
    end else begin
      //glutShowWindow;
      redisplayAtSomePoint:=true;
    end;
    idleSleepTime:=0;
    lastActivation:=now;
  end;

FUNCTION int_addplot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR LX,LY:P_listLiteral;
      point:array of array[0..1] of extended;
      options:P_stringLiteral;
      i:longint;

      errorOccurred:boolean=false;
  FUNCTION canConvertOneToPoints:boolean;
    VAR i:longint;
        L:P_listLiteral;
    begin
      result:=true;
      setLength(point,0);
      L:=P_listLiteral(params^.value(0));
      if L^.literalType in [lt_realList,lt_intList,lt_numList] then begin
        setLength(point,L^.size);
        for i:=0 to L^.size-1 do begin
          point[i,0]:=i;
          if L^.value(i)^.literalType=lt_int
          then point[i,1]:=P_intLiteral(L^.value(i))^.value
          else point[i,1]:=P_realLiteral(L^.value(i))^.value;
        end;
      end else begin


      end;


    end;

  begin
    result:=nil;
    plot.addGraph;
    if (params<>nil) then begin
      if (params^.size>1) and (params^.value(params^.size-1)^.literalType=lt_string)
        then options:=P_stringLiteral(params^.value(params^.size-1))
        else options:=nil;

      if ((options=nil) and (params^.size=1)) or ((options<>nil) and (params^.size=2))
      and (params^.value(0)^.literalType in [lt_list,lt_realList,lt_intList,lt_numList]) then begin
        LX:=P_listLiteral(params^.value(0));
        if LX^.literalType in [lt_intList, lt_realList, lt_numList] then begin
          for i:=0 to P_listLiteral(LX)^.size-1 do begin
            if (P_listLiteral(LX)^.value(i)^.literalType=lt_int)
            then rx:=P_intLiteral (P_listLiteral(LX)^.value(i))^.value
            else rx:=P_realLiteral(P_listLiteral(LX)^.value(i))^.value;
          end;
          plot.addPoint(i,rx);
          result:=newBoolLiteral(true);
        end else begin
          for i:=0 to LX^.size-1 do if not(errorOccurred) then case LX^.value(i)^.literalType of
            lt_int: plot.addPoint(i,P_intLiteral(LX^.value(i))^.value);
            lt_intList: if P_listLiteral(LX^.value(i))^.size;
            type_intList: if length(LX^.lVal[i]^.iVal)=2
              then plot.addPoint(LX^.lVal[i]^.iVal[0],LX^.lVal[i]^.iVal[1])
              else errorOccurred:=true;
            type_realScalar: plot.addPoint(i,LX^.lVal[i]^.rVal[0]);
            type_realList: if length(LX^.lVal[i]^.rVal)=2
              then plot.addPoint(LX^.lVal[i]^.rVal[0],LX^.lVal[i]^.rVal[1])
              else result:=nil;
            type_object: if length(LX^.lVal[i]^.lVal)=2 then begin
              case LX^.lval[i]^.lVal[0]^.typeByte of
                type_intScalar : case LX^.lval[i]^.lVal[1]^.typeByte of
                  type_intScalar : plot.addPoint(LX^.lVal[i]^.lVal[0]^.iVal[0],LX^.lVal[i]^.lVal[1]^.iVal[0]);
                  type_realScalar: plot.addPoint(LX^.lVal[i]^.lVal[0]^.iVal[0],LX^.lVal[i]^.lVal[1]^.rVal[0]);
                  else result:=nil;
                end;
                type_realScalar: case LX^.lval[i]^.lVal[1]^.typeByte of
                  type_intScalar : plot.addPoint(LX^.lVal[i]^.lVal[0]^.rVal[0],LX^.lVal[i]^.lVal[1]^.iVal[0]);
                  type_realScalar: plot.addPoint(LX^.lVal[i]^.lVal[0]^.rVal[0],LX^.lVal[i]^.lVal[1]^.rVal[0]);
                  else result:=nil;
                end;
                else result:=nil;
              end;
            end else result:=nil;
            else result:=nil;
          end;
        end;
      end else if (((options=nil) and (length(params^.lVal)=2)) or ((options<>nil) and (length(params^.lVal)=3)))
        and (params^.lVal[0]^.typeByte in [type_object,type_realList,type_intList])
        and (params^.lVal[1]^.typeByte in [type_object,type_realList,type_intList])
        and (params^.lVal[0]^.listLength=params^.lVal[1]^.listLength)  then begin
        LX:=params^.lVal[0];
        LY:=params^.lVal[1];
        result:=C_boolLit[true];
        setLength(temp,params^.lVal[0]^.listLength);
        case LX^.typeByte of
          type_realList: for i:=0 to length(temp)-1 do temp[i,0]:=LX^.rVal[i];
          type_intList: for i:=0 to length(temp)-1 do temp[i,0]:=LX^.iVal[i];
          type_object: for i:=0 to length(temp)-1 do case LX^.lVal[i]^.typeByte of
            type_intScalar: temp[i,0]:=LX^.lVal[i]^.iVal[0];
            type_realScalar: temp[i,0]:=LX^.lVal[i]^.rVal[0];
            else result:=nil;
          end;
        end;
        case LY^.typeByte of
          type_realList: for i:=0 to length(temp)-1 do temp[i,1]:=LY^.rVal[i];
          type_intList: for i:=0 to length(temp)-1 do temp[i,1]:=LY^.iVal[i];
          type_object: for i:=0 to length(temp)-1 do case LY^.lVal[i]^.typeByte of
            type_intScalar: temp[i,1]:=LY^.lVal[i]^.iVal[0];
            type_realScalar: temp[i,1]:=LY^.lVal[i]^.rVal[0];
            else result:=nil;
          end;
        end;
        if result<>nil then for i:=0 to length(temp)-1 do plot.addPoint(temp[i,0],temp[i,1]);
      end;
    end;
    if result<>nil then begin
      result^.reReference;
      if options<>nil then plot.setOptions(options^.sVal[0]);
      activatePlots;
    end else plot.dropGraph;
  end;

FUNCTION int_plot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    plot.clear;
    result:=int_addplot(primaryCall,param,evaluationEnvironment);
  end;

FUNCTION int_plotRange(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    if (param=nil) or (length(param^.lVal)=0) then begin
      new(result,create(type_object,2));
      new(result^.lVal[0],create(type_object,3));
      new(result^.lVal[0]^.lVal[0],createReal(plot.sampleRange['x',0,plot.logScale['x']]));
      new(result^.lVal[0]^.lVal[1],createReal(plot.sampleRange['x',1,plot.logScale['x']]));
      result^.lVal[0]^.lVal[2]:=C_boolLit[plot.logScale['x']];
      result^.lVal[0]^.lVal[2]^.reReference;
      new(result^.lVal[1],create(type_object,3));
      new(result^.lVal[1]^.lVal[0],createReal(plot.sampleRange['y',0,plot.logScale['y']]));
      new(result^.lVal[1]^.lVal[1],createReal(plot.sampleRange['y',1,plot.logScale['y']]));
      result^.lVal[1]^.lVal[2]:=C_boolLit[plot.logScale['y']];
      result^.lVal[1]^.lVal[2]^.reReference;
    end else result:=nil;
  end;

FUNCTION int_drawRect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR rp:array[0..3] of extended;
      i:longint;
  begin
    result:=nil;
    if (param<>nil) and (length(param^.lVal)=5) and (param^.lVal[4]^.typeByte=type_stringScalar) then begin
      i:=0;
      while i<=3 do begin
        case (param^.lVal[i]^.typeByte) of
          type_intScalar :rp[i]:=param^.lVal[i]^.iVal[0];
          type_realScalar:rp[i]:=param^.lVal[i]^.rVal[0];
          else i:=99;
        end;
        inc(i);
      end;
      if (i=4) then begin
        plot.addRect(rp[0],rp[1],rp[2],rp[3],param^.lVal[4]^.sVal[0]);
        result:=C_boolLit[true];
        result^.reReference;
        activatePlots;
      end;
    end;
  end;


INITIALIZATION
  plot.create;
  glutInit(@argc, argv);
  lastActivation:=now;

end.
