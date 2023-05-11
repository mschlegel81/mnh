UNIT plotstyles;
INTERFACE
USES sysutils,math,FPCanvas,BGRABitmapTypes;
TYPE
  T_feedStyle   =(fs_direct, fs_stepLeft, fs_stepRight, fs_tube, fs_pair, fs_bar);
  T_builderStyle=(bs_straight, bs_bSpline, bs_cSpline);
  T_polygonStyle=(ps_noPoly, ps_direct, ps_additional);
  T_brushStyle  =(bs_noBrush, bs_solidBrush, bs_transparentBrush);
  T_drawStyle   =(ds_continuous, ds_plus, ds_cross, ds_dot, ds_impulse, ds_box, ds_ellipse);
  T_drawStyles  =set of T_drawStyle;

  T_colorChannel = (cc_red, cc_green, cc_blue,cc_alpha);
  T_color = array[T_colorChannel] of byte;

  T_plotStyle=(ps_none,               // defaults to ps_straight
               ps_straight,           // fs_direct    + bs_straight + ps_*                     + bs_*
               ps_bspline,            // fs_*         + bs_bSpline  + ps_*                     + bs_*
               ps_cosspline,          // fs_*         + bs_cSpline  + ps_*                     + bs_*
               ps_stepLeft,           // fs_stepLeft  + bs_straight + ps_*                     + bs_*
               ps_stepRight,          // fs_stepRight + bs_straight + ps_*                     + bs_*
               ps_filled,             // fs_*         + bs_*        + (default to ps_baseLine) + bs_solidBrush
               ps_fillSolid,          // fs_*         + bs_*        + (default to ps_baseLine) + bs_transparentBrush
               ps_bar,                // ?
               ps_box,                // ?
               ps_tube,               // fs_tube      + bs_*        + ps_*                     + bs_*
               ps_dot,                //
               ps_plus,               //
               ps_cross,              //
               ps_impulse,            //
               ps_polygon,            //
               ps_ellipse);           // ?

  T_plotStyles=set of T_plotStyle;

CONST
  C_lineStyles:T_plotStyles=[ps_straight..ps_stepRight];
  C_stylesRequiringDiscreteSteps:T_plotStyles=[ps_stepLeft,ps_stepRight,ps_bar,ps_box,ps_dot,ps_plus,ps_cross,ps_impulse,ps_ellipse];
  C_styleName: array[T_plotStyle] of array[0..1] of string=
     {ps_none      }  (('',''),
     {ps_straight  }   ('line'     , 'l'),
     {ps_bspline   }   ('bspline'  , 'b'),
     {ps_cosspline }   ('cspline'  , 'c'),
     {ps_stepLeft  }   ('stepLeft' , '' ),
     {ps_stepRight }   ('stepRight', '' ),
     {ps_filled    }   ('fill'     , 'f'),
     {ps_fillSolid }   ('fillSolid', 'fs'),
     {ps_bar       }   ('bar'      , '' ),
     {ps_box       }   ('box'      , '' ),
     {ps_tube      }   ('tube'     , '' ),
     {ps_dot       }   ('dot'      , '.'),
     {ps_plus      }   ('plus'     , '+'),
     {ps_cross     }   ('cross'    , 'x'),
     {ps_impulse   }   ('impulse'  , 'i'),
     {ps_polygon   }   ('polygon'  , 'p'),
     {ps_ellipse   }   ('ellipse'  , 'e'));
TYPE
  T_scaleAndColor = record
    lineWidth   ,
    symbolRadius,
    symbolWidth :longint;
    lineColor   ,
    solidColor  :TBGRAPixel;
    solidStyle  :TFPBrushStyle;
  end;

  T_style = object
    private
      defaultColor:boolean       ;
      style:T_plotStyles;
      PROCEDURE parseStyle(CONST styleString: ansistring);
      PROCEDURE setDefaults(CONST index:longint);
    public
      color: T_color;
      styleModifier: double;

      PROCEDURE init();
      FUNCTION toString:ansistring;
      FUNCTION getLineScaleAndColor(CONST xRes,yRes:longint):T_scaleAndColor;

      FUNCTION feedStyle   :T_feedStyle   ;
      FUNCTION builderStyle:T_builderStyle;
      FUNCTION polygonStyle:T_polygonStyle ;
      FUNCTION brushStyle  :T_brushStyle  ;
      FUNCTION drawStyles  :T_drawStyles  ;
  end;

  T_arrayOfStyle = array of T_style;
  T_gridStyleElement=(gse_tics,gse_coarseGrid,gse_fineGrid);
  T_gridStyle=set of T_gridStyleElement;

OPERATOR :=(CONST gridStyle:T_gridStyle):byte;
OPERATOR :=(CONST b:byte):T_gridStyle;
OPERATOR :=(CONST c:T_color):TBGRAPixel;
FUNCTION getStyles(CONST index:longint; CONST styleString:string):T_arrayOfStyle;
IMPLEMENTATION
USES myGenerics,mySys,myStringUtil;
VAR styleCS:TRTLCriticalSection;
    styleMap:specialize G_stringKeyMap<T_arrayOfStyle>;
CONST GSE_BYTE:array[T_gridStyleElement] of byte=(1,2,4);
OPERATOR :=(CONST gridStyle:T_gridStyle):byte;
  VAR se:T_gridStyleElement;
  begin
    result:=0;
    for se in gridStyle do inc(result,GSE_BYTE[se]);
  end;

OPERATOR :=(CONST b:byte):T_gridStyle;
  VAR se:T_gridStyleElement;
  begin
    result:=[];
    for se in T_gridStyleElement do if (GSE_BYTE[se] and b)>0 then include(result,se);
  end;

OPERATOR :=(CONST c:T_color):TBGRAPixel;
  begin
    result.FromRGB(c[cc_red],c[cc_green],c[cc_blue],c[cc_alpha]);
  end;

CONST C_defaultColor: array[0..7] of record
        name: string;
        color: T_color;
      end =
      ((name: 'black' ; color:(  0,  0,  0,255)),
       (name: 'red'   ; color:(255,  0,  0,255)),
       (name: 'blue'  ; color:(  0,  0,255,255)),
       (name: 'green' ; color:(  0,128,  0,255)),
       (name: 'purple'; color:(192,  0,192,255)),
       (name: 'orange'; color:(255, 96,  0,255)),
       (name: 'yellow'; color:(255,255,  0,255)),
       (name: 'cyan'  ; color:(  0,128,128,255)));

PROCEDURE T_style.init();
  begin
    defaultColor:=true;
    style:=[];
    color:=C_defaultColor[0].color;
    styleModifier:=1;
  end;

PROCEDURE T_style.parseStyle(CONST styleString: ansistring);
  FUNCTION parseColorOption(colorOption: shortstring; OUT color:T_color): boolean;
    FUNCTION HSV2RGB(H,S,V: single):T_color;
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
        result[cc_alpha]:=255;
        case hi of
          0, 6: begin result[cc_red]:=round(V); result[cc_green]:=t;        result[cc_blue]:=p; end;
          1:    begin result[cc_red]:=q;        result[cc_green]:=round(V); result[cc_blue]:=p; end;
          2:    begin result[cc_red]:=p;        result[cc_green]:=round(V); result[cc_blue]:=t; end;
          3:    begin result[cc_red]:=p;        result[cc_green]:=q;        result[cc_blue]:=round(V); end;
          4:    begin result[cc_red]:=t;        result[cc_green]:=p;        result[cc_blue]:=round(V); end;
          5:    begin result[cc_red]:=round(V); result[cc_green]:=p;        result[cc_blue]:=q; end;
        end;
      end;

    VAR colorParts:T_arrayOfString;
        isHSV: boolean;
        i:longint;
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
        colorParts:=split(colorOption,',');
        if (length(colorParts)>=3) and (length(colorParts)<=4) then begin
          if isHSV then color:=HSV2RGB(strToFloatDef(colorParts[0],0),
                           max(0,min(1,strToFloatDef(colorParts[1],0))),
                           max(0,min(1,strToFloatDef(colorParts[2],0))))
          else begin
            color[cc_red  ]:=round(255*max(0, min(1, strToFloatDef(colorParts[0], 0))));
            color[cc_green]:=round(255*max(0, min(1, strToFloatDef(colorParts[1], 0))));
            color[cc_blue ]:=round(255*max(0, min(1, strToFloatDef(colorParts[2], 0))));
          end;
          if length(colorParts)=4
          then color[cc_alpha]:=round(255*max(0, min(1, strToFloatDef(colorParts[3], 0))))
          else color[cc_alpha]:=255;
          result:=true;
        end;
      end;
      if not(result) and (copy(colorOption, 1, 3) = 'HUE') then begin
        colorOption:=copy(colorOption, 4, length(colorOption)-3);
        color:=HSV2RGB(strToFloatDef(colorOption, 0), 1, 1);
        result:=true;
      end;
      if not(result) and (copy(colorOption, 1, 4) = 'GREY') then begin
        colorOption:=copy(colorOption, 5, length(colorOption)-4);
        color[cc_red  ]:=round(255*max(0, min(1, strToFloatDef(colorOption, 0))));
        color[cc_green]:=color[cc_red];
        color[cc_blue ]:=color[cc_red];
        color[cc_alpha]:=255;
        result:=true;
      end;
    end;

  VAR part, options: ansistring;
      sp: longint;
      size: double;
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
      if part<>'' then begin
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
        if mightBeColor then begin
          if parseColorOption(part, color) then defaultColor:=false;
        end;
      end;
    until options = '';
    if (style-[ps_filled,ps_fillSolid])=[] then include(style,ps_straight);
  end;

PROCEDURE T_style.setDefaults(CONST index:longint);
  begin
    if defaultColor then color:=C_defaultColor[index mod length(C_defaultColor)].color;
  end;

FUNCTION T_style.toString:ansistring;
  VAR s:T_plotStyle;
  begin
    result:='';
    if styleModifier<>1 then result:=floatToStr(styleModifier)+' ';
    for s in style do result+=C_styleName[s,0]+' ';
    result+='RGB'+floatToStrF(color[cc_red  ]/255,ffGeneral,3,4)
           +','  +floatToStrF(color[cc_green]/255,ffGeneral,3,4)
           +','  +floatToStrF(color[cc_blue ]/255,ffGeneral,3,4);
    if color[cc_alpha]<>255 then result+=','+floatToStrF(color[cc_alpha]/255,ffGeneral,3,4);
  end;

FUNCTION T_style.getLineScaleAndColor(CONST xRes,yRes:longint):T_scaleAndColor;
  FUNCTION toByte(CONST d:double):byte;
    begin
      if d>255 then result:=255 else if d<0 then result:=0 else result:=round(d);
    end;

  VAR scalingFactor,idealLineWidth:double;
  begin
    if (ps_filled in style) and not(ps_fillSolid in style)
    then result.solidColor.FromRGB(color [cc_red],color [cc_green],color [cc_blue],toByte(color[cc_alpha]*0.4))
    else result.solidColor:=color;

    result.solidStyle:=bsClear;
    if (ps_filled in style) or (ps_fillSolid in style) then result.solidStyle:=bsSolid;

    scalingFactor:=sqrt(sqr(xRes)+sqr(yRes))/1000;
    idealLineWidth:=styleModifier*scalingFactor;
    if      idealLineWidth<1/255 then begin result.lineWidth:=0;                     idealLineWidth:=0; end
    else if idealLineWidth<1     then       result.lineWidth:=1
                                 else begin result.lineWidth:=round(idealLineWidth); idealLineWidth:=1; end;
    result.lineColor.FromRGB(color[cc_red],color[cc_green],color[cc_blue],toByte(idealLineWidth*color[cc_alpha]));
    result.symbolWidth :=round(scalingFactor*3        *styleModifier);
    result.symbolRadius:=round(scalingFactor*3/sqrt(2)*styleModifier);
  end;

FUNCTION T_style.feedStyle   :T_feedStyle   ;
  begin
    if [ps_ellipse,ps_box]*style<>[] then result:=fs_pair
    else if ps_tube      in style    then result:=fs_tube
    else if ps_stepLeft  in style    then result:=fs_stepLeft
    else if ps_stepRight in style    then result:=fs_stepRight
    else if ps_bar       in style    then result:=fs_bar
                                     else result:=fs_direct;
  end;

FUNCTION T_style.builderStyle:T_builderStyle;
  begin
    if ps_bspline in style        then result:=bs_bSpline
    else if ps_cosspline in style then result:=bs_cSpline
                                  else result:=bs_straight;
  end;

FUNCTION T_style.polygonStyle :T_polygonStyle ;
  begin
    if   ps_polygon in style                   then result:=ps_direct
    else if ps_tube in style                   then result:=ps_additional
    else if [ps_filled,ps_fillSolid]*style<>[] then result:=ps_additional
                                               else result:=ps_noPoly;
  end;

FUNCTION T_style.brushStyle  :T_brushStyle  ;
  begin
    if ps_filled in style         then result:=bs_transparentBrush
    else if ps_fillSolid in style then result:=bs_solidBrush
                                  else result:=bs_noBrush;
  end;

FUNCTION T_style.drawStyles  :T_drawStyles   ;
  begin
    result:=[];
    if ps_ellipse in style then result+=[ds_ellipse];
    if [ps_box,ps_bar]*style<>[] then result+=[ds_box];
    if ps_impulse in style then result+=[ds_impulse];
    if ps_dot     in style then result+=[ds_dot];
    if ps_cross   in style then result+=[ds_cross];
    if ps_plus    in style then result+=[ds_plus];
    if (ps_straight in style) or (ps_tube in style) or (result=[]) then result+=[ds_continuous];
  end;

FUNCTION splitIntoConsistentStyles(style:T_style):T_arrayOfStyle;
  CONST inconsistent:array[0..6] of record
          s1,s2:T_plotStyles;
        end=((s1:[ps_tube];                 s2:[ps_ellipse,ps_box]),
             (s1:[ps_stepLeft];             s2:[ps_ellipse,ps_box]),
             (s1:[ps_stepRight];            s2:[ps_ellipse,ps_box]),
             (s1:[ps_bar];                  s2:[ps_ellipse,ps_box]),
             (s1:[ps_dot,ps_plus,ps_cross]; s2:[ps_ellipse,ps_box]),
             (s1:[ps_dot,ps_plus,ps_cross]; s2:[ps_cosspline,ps_bspline]),
             (s1:[ps_cosspline];            s2:[ps_bspline]));

  VAR s1,s2:T_style;
      rest:T_arrayOfStyle;
      i,i0,k:longint;
  begin
    //inconsistent fillings
    if (ps_filled in style.style) and (ps_fillSolid in style.style) then style.style:=style.style-[ps_filled];

    for k:=0 to length(inconsistent)-1 do if (style.style*inconsistent[k].s1<>[]) and (style.style*inconsistent[k].s2<>[]) then begin
      s1:=style; s1.style:=s1.style-inconsistent[k].s1;
      s2:=style; s2.style:=s2.style-inconsistent[k].s2;
      result:=splitIntoConsistentStyles(s1);
      rest  :=splitIntoConsistentStyles(s2);
      i0:=length(result);
      setLength(result,i0+length(rest));
      for i:=0 to length(rest)-1 do result[i+i0]:=rest[i];
      exit(result);
    end;
    setLength(result,1);
    result[0]:=style;
  end;

PROCEDURE clearStyles;
  begin
    enterCriticalSection(styleCS);
    try
      styleMap.clear;
    finally
      leaveCriticalSection(styleCS);
    end;
  end;

FUNCTION getStyles(CONST index:longint; CONST styleString:string):T_arrayOfStyle;
  FUNCTION copyOf(CONST x:T_arrayOfStyle):T_arrayOfStyle;
    VAR i:longint;
    begin
      //Note: initialize(result) leads to memory leak
      setLength(result,length(x));
      for i:=0 to length(x)-1 do result[i]:=x[i];
    end;

  VAR s:T_style;
      i:longint;
  begin
    enterCriticalSection(styleCS);
    try
      if not(styleMap.containsKey(styleString,result)) then begin
        s.init();
        s.parseStyle(styleString);
        result:=splitIntoConsistentStyles(s);
        styleMap.put(styleString,result);
      end;
      result:=copyOf(result);
      for i:=0 to length(result)-1 do result[i].setDefaults(index);
    finally
      leaveCriticalSection(styleCS);
    end;
  end;

PROCEDURE disposeStyle(VAR v:T_arrayOfStyle);
  begin
    setLength(v,0);
  end;

INITIALIZATION
  initialize(styleCS);
  initCriticalSection(styleCS);
  styleMap.create(@disposeStyle);
  memoryCleaner.registerCleanupMethod(1,@clearStyles);
FINALIZATION
  enterCriticalSection(styleCS);
  styleMap.destroy;
  leaveCriticalSection(styleCS);
  doneCriticalSection(styleCS);

end.

