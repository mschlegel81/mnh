UNIT plotstyles;
INTERFACE
USES sysutils,math,FPCanvas,BGRABitmapTypes;
TYPE
  T_plotStyle=(ps_none,
               ps_straight,
               ps_bspline,
               ps_cosspline,
               ps_stepLeft,
               ps_stepRight,
               ps_filled,
               ps_fillSolid,
               ps_bar,
               ps_box,
               ps_tube,
               ps_dot,
               ps_plus,
               ps_cross,
               ps_impulse,
               ps_polygon,
               ps_ellipse);
  T_plotStyles=set of T_plotStyle;
  T_colorChannel = (cc_red, cc_green, cc_blue,cc_alpha);
  T_color = array[T_colorChannel] of byte;

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

  T_styleTemplateDefault =(std_defaultColor);
  T_styleTemplateDefaults=set of T_styleTemplateDefault;
  T_style = object
    private
      PROCEDURE parseStyle(CONST styleString: ansistring);
      PROCEDURE setDefaults(CONST index:longint);
    public
      defaults:T_styleTemplateDefaults;
      style: T_plotStyles;
      color: T_color;
      styleModifier: double;
      PROCEDURE init();
      FUNCTION toString:ansistring;
      FUNCTION getLineScaleAndColor(CONST xRes,yRes:longint):T_scaleAndColor;
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
    style:=[];
    defaults:=[low(T_styleTemplateDefault)..high(T_styleTemplateDefault)];
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
          if parseColorOption(part, color) then defaults-=[std_defaultColor];
        end;
      end;
    until options = '';
    if (style-[ps_filled,ps_fillSolid])=[] then include(style,ps_straight);
  end;

PROCEDURE T_style.setDefaults(CONST index:longint);
  begin
    if std_defaultColor in defaults then color:=C_defaultColor[index mod length(C_defaultColor)].color;
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

FUNCTION splitIntoConsistentStyles(style:T_style):T_arrayOfStyle;
{$MACRO ON}
{$define recursion:=begin
  result:=splitIntoConsistentStyles(s1);
  rest  :=splitIntoConsistentStyles(s2);
  i0:=length(result);
  setLength(result,i0+length(rest));
  for i:=0 to length(rest)-1 do result[i+i0]:=rest[i]
end}
  VAR s1,s2:T_style;
      rest:T_arrayOfStyle;
      i,i0:longint;
  begin
    if (ps_filled in style.style) and (ps_fillSolid in style.style) then begin
      s1:=style; s1.style:=s1.style-[ps_filled];
      s2:=style; s2.style:=s2.style-[ps_fillSolid];
      recursion;
    end else if (ps_tube in style.style) and (style.style*C_stylesRequiringDiscreteSteps<>[]) then begin
      s1:=style; s1.style:=s1.style-[ps_tube];
      s2:=style; s2.style:=s2.style-C_stylesRequiringDiscreteSteps;
      recursion;
    end else if (ps_bspline in style.style) and (ps_cosspline in style.style) then begin
      s1:=style; s1.style:=s1.style-[ps_bspline];
      s2:=style; s2.style:=s2.style-[ps_cosspline];
      recursion;
    end else if (ps_bspline in style.style) and (style.style*C_stylesRequiringDiscreteSteps<>[]) then begin
      s1:=style; s1.style:=s1.style-[ps_bspline];
      s2:=style; s2.style:=s2.style-C_stylesRequiringDiscreteSteps;
      recursion;
    end else if (ps_cosspline in style.style) and (style.style*C_stylesRequiringDiscreteSteps<>[]) then begin
      s1:=style; s1.style:=s1.style-[ps_cosspline];
      s2:=style; s2.style:=s2.style-C_stylesRequiringDiscreteSteps;
      recursion;
    end else begin
      setLength(result,1);
      result[0]:=style;
    end;
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
  VAR s:T_style;
  begin
    enterCriticalSection(styleCS);
    try
      if not(styleMap.containsKey(styleString,result)) then begin
        s.init();
        s.parseStyle(styleString);
        result:=splitIntoConsistentStyles(s);
        styleMap.put(styleString,result);
      end;
      for s in result do s.setDefaults(index);
    finally
      leaveCriticalSection(styleCS);
    end;
  end;

INITIALIZATION
  initialize(styleCS);
  initCriticalSection(styleCS);
  styleMap.create();
  memoryCleaner.registerCleanupMethod(@clearStyles);
FINALIZATION
  enterCriticalSection(styleCS);
  styleMap.destroy;
  leaveCriticalSection(styleCS);
  doneCriticalSection(styleCS);

end.

