UNIT plotstyles;
INTERFACE
USES sysutils,math,FPCanvas;
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
  T_colorChannel = (cc_red, cc_green, cc_blue);
  T_color = array[T_colorChannel] of byte;

CONST
  C_lineStyles:T_plotStyles=[ps_straight..ps_stepRight];
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
  SINGLE_SAMPLE_INDEX=4;

TYPE
  T_scaleAndColor = record
    lineWidth   ,
    symbolRadius,
    symbolWidth ,
    lineColor   ,
    solidColor  :longint;
    solidStyle  :TFPBrushStyle;
  end;

  T_styleTemplateDefault =(std_defaultColor,std_defaultTransparentIdx);
  T_styleTemplateDefaults=set of T_styleTemplateDefault;

  T_style = object
    private
      transparentIndex:byte;
      PROCEDURE parseStyle(CONST styleString: ansistring);
      PROCEDURE setDefaults(CONST index:longint; VAR transparentCount:longint);
    public
      defaults:T_styleTemplateDefaults;
      style: T_plotStyles;
      color: T_color;
      styleModifier: double;
      PROCEDURE init();
      FUNCTION toString:ansistring;
      FUNCTION getLineScaleAndColor(CONST xRes,yRes:longint; CONST sampleIndex:byte):T_scaleAndColor;
  end;

  T_gridStyleElement=(gse_tics,gse_coarseGrid,gse_fineGrid);
  T_gridStyle=set of T_gridStyleElement;

OPERATOR :=(CONST gridStyle:T_gridStyle):byte;
OPERATOR :=(CONST b:byte):T_gridStyle;
OPERATOR :=(CONST c:T_color):longint;
FUNCTION getStyle(CONST index:longint; CONST styleString:string; VAR transparentCount:longint):T_style;
IMPLEMENTATION
USES myGenerics,mySys;
VAR styleCS:TRTLCriticalSection;
    styleMap:specialize G_stringKeyMap<T_style>;
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

OPERATOR :=(CONST c:T_color):longint;
  begin
    result:=c[cc_red  ]
        or (c[cc_green] shl  8)
        or (c[cc_blue ] shl 16);
  end;

CONST C_defaultColor: array[0..7] of record
        name: string;
        color: T_color;
      end =
      ((name: 'black' ; color:(  0,  0,  0)),
       (name: 'red'   ; color:(255,  0,  0)),
       (name: 'blue'  ; color:(  0,  0,255)),
       (name: 'green' ; color:(  0,128,  0)),
       (name: 'purple'; color:(192,  0,192)),
       (name: 'orange'; color:(255, 96,  0)),
       (name: 'yellow'; color:(255,255,  0)),
       (name: 'cyan'  ; color:(  0,128,128)));

PROCEDURE T_style.init();
  begin
    transparentIndex:=0;
    style:=[];
    defaults:=[low(T_styleTemplateDefault)..high(T_styleTemplateDefault)];
    color:=C_defaultColor[0].color;
    styleModifier:=1;
  end;

PROCEDURE T_style.parseStyle(CONST styleString: ansistring);
  FUNCTION parseColorOption(colorOption: shortstring; OUT r, g, b: byte): boolean;
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
          if parseColorOption(part, color[cc_red], color[cc_green], color[cc_blue]) then defaults-=[std_defaultColor]
          else begin
            if (copy(part,1,2)='TI') then begin
              transparentIndex:=strToIntDef(copy(part,3,length(part)-2),-1) and 255;
              defaults-=[std_defaultTransparentIdx];
            end;
          end;
        end;
      end;
    until options = '';
    if (style-[ps_filled,ps_fillSolid])=[] then include(style,ps_straight);
  end;

PROCEDURE T_style.setDefaults(CONST index:longint; VAR transparentCount:longint);
  begin
    if ((ps_filled  in style) or
        (ps_bar     in style) or
        (ps_tube    in style) or
        (ps_polygon in style)) and not(ps_fillSolid in style) then begin
      if std_defaultTransparentIdx in defaults
      then transparentIndex:=transparentCount and 255;
      inc(transparentCount);
    end;
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
                 +','  +floatToStrF(color[cc_blue ]/255,ffGeneral,3,4)+' TI'+intToStr(transparentIndex and 3);
  end;

FUNCTION T_style.getLineScaleAndColor(CONST xRes,yRes:longint; CONST sampleIndex:byte):T_scaleAndColor;
  FUNCTION toByte(CONST d:double):byte;
    begin
      if d>255 then result:=255 else if d<0 then result:=0 else result:=round(d);
    end;
  FUNCTION roundToInt(CONST x:double):longint;
    CONST subLineWidthDelta:array[0..3,0..3] of byte=(
          {0.0  }(0,0,0,0),
          {0.25 }(0,1,0,0),
          {0.5  }(0,1,1,0),
          {0.75 }(0,1,1,1));
    VAR intPart:longint;
        fracPart4:longint;
    begin
      intPart:=trunc(x);
      fracPart4:=trunc(frac(x)*4);
      result:=intPart+subLineWidthDelta[fracPart4,sampleIndex and 3];
    end;

  VAR scalingFactor,ideal:double;
  begin
    result.solidColor:=color [cc_red] or (color [cc_green] shl 8) or (color [cc_blue] shl 16);
    result.solidStyle:=bsClear;
    if ps_filled in style then begin
      if sampleIndex=SINGLE_SAMPLE_INDEX then case transparentIndex and 3 of
        0: result.solidStyle:=bsFDiagonal ;
        1: result.solidStyle:=bsBDiagonal ;
        2: result.solidStyle:=bsHorizontal;
        3: result.solidStyle:=bsVertical  ;
      end else begin
        if (sampleIndex-transparentIndex) and 3=0
        then result.solidStyle:=bsSolid
        else result.solidStyle:=bsClear;
      end;
    end;
    if ps_fillSolid in style then result.solidStyle:=bsSolid;
    scalingFactor:=sqrt(sqr(xRes)+sqr(yRes))/1000;
    result.lineColor:=result.solidColor;
    ideal:=styleModifier*scalingFactor;
    if (sampleIndex<>SINGLE_SAMPLE_INDEX) or (ideal>0.9375) then begin
      result.lineWidth:=roundToInt(ideal);
    end else begin
      if ideal=0 then result.lineWidth:=0
                 else result.lineWidth:=1;
      result.lineColor:=toByte(color[cc_red  ]*ideal + 255*(1-ideal))
                    or (toByte(color[cc_green]*ideal + 255*(1-ideal)) shl  8)
                    or (toByte(color[cc_blue ]*ideal + 255*(1-ideal)) shl 16);
    end;
    result.symbolWidth :=round(scalingFactor*3        *styleModifier);
    result.symbolRadius:=round(scalingFactor*3/sqrt(2)*styleModifier);
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

FUNCTION getStyle(CONST index:longint; CONST styleString:string; VAR transparentCount:longint):T_style;
  begin
    enterCriticalSection(styleCS);
    try
      if not(styleMap.containsKey(styleString,result)) then begin
        result.init();
        result.parseStyle(styleString);
        styleMap.put(styleString,result);
      end;
      result.setDefaults(index,transparentCount);
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

