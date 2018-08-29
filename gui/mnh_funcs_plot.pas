UNIT mnh_funcs_plot;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,math,
     myGenerics,
     myStringUtil,
     mnh_basicTypes,
     mnh_constants,
     mnh_out_adapters,
     mnh_litVar,
     mnh_contexts,
     mnh_funcs,
     mnh_messages,
     mnh_plotData,plotstyles,plotMath;
TYPE F_generateRow=FUNCTION(CONST f:P_expressionLiteral; CONST t0,t1:T_myFloat; CONST samples:longint; CONST location:T_tokenLocation; VAR context:T_threadContext):T_dataRow;
FUNCTION newDataRow(CONST y:P_listLiteral; CONST x:P_listLiteral=nil):T_dataRow;
VAR generateRow:F_generateRow;
IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION fReal(CONST X: P_literal): double; inline;
  begin
    if x = nil then result:=Nan else case X^.literalType of
      lt_real: begin
        result:=P_realLiteral(x)^.value;
        if isInfinite(result) then
          result:=Nan;
        end;
      lt_smallint,lt_bigint: result:=P_abstractIntLiteral(x)^.floatValue;
      else result:=Nan;
    end;
  end;

FUNCTION newDataRow(CONST y:P_listLiteral; CONST x:P_listLiteral=nil):T_dataRow;
  VAR i,imax:longint;
      xy:P_listLiteral;
  begin
    if x=nil then case y^.literalType of
      lt_list: begin
        result.init(y^.size);
        with y^ do for i:=0 to size-1 do if value[i]^.literalType in [lt_intList,lt_realList,lt_numList] then begin
          xy:=P_listLiteral(value[i]);
          if xy^.size=2 then result[i]:=pointOf(fReal(xy^.value[0]),fReal(xy^.value[1]))
                        else result[i]:=pointOf(Nan,Nan);
        end else result[i]:=pointOf(Nan,Nan);
      end;
      lt_intList, lt_realList, lt_numList: begin
        result.init(y^.size);
        with y^ do for i:=0 to size-1 do result[i]:=pointOf(i,fReal(value[i]));
      end;
    end else begin
      imax:=min(X^.size, Y^.size);
      result.init(imax);
      for i:=0 to imax-1 do result[i]:=pointOf(fReal(X^.value[i]),fReal(Y^.value[i]));
    end;
  end;

FUNCTION addPlot intFuncSignature;
  VAR options: ansistring = '';
      sizeWithoutOptions: longint;
  FUNCTION addRowMessage(CONST dataRow:T_dataRow):P_addRowMessage;
    begin
      new(result,create(options,dataRow));
    end;

  begin
    if not(context.checkSideEffects('addPlot',tokenLocation,[se_alterPlotState])) then exit(nil);
    result:=nil;
    if (params<>nil) and (params^.size>=1) then begin
      if (params^.value[params^.size-1]^.literalType = lt_string) then begin
        options:=P_stringLiteral(params^.value[params^.size-1])^.value;
        sizeWithoutOptions:=params^.size-1;
      end else begin
        options:='';
        sizeWithoutOptions:=params^.size;
      end;
      if (sizeWithoutOptions = 1) and (arg0^.literalType in [lt_list,lt_intList, lt_realList, lt_numList]) then begin
        context.messages.globalMessages^.postCustomMessage(addRowMessage(newDataRow(list0)),true);
        exit(newVoidLiteral);
      end;
      if (sizeWithoutOptions = 2) and
         (arg0^.literalType in [lt_intList, lt_realList, lt_numList]) and
         (arg1^.literalType in [lt_intList, lt_realList, lt_numList]) and
         (list0^.size=list1^.size) then begin
        context.messages.globalMessages^.postCustomMessage(addRowMessage(newDataRow(list1,list0)),true);
        exit(newVoidLiteral);
      end;
      if (sizeWithoutOptions = 4) and
         (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(1)) and
         (arg1^.literalType in [lt_smallint,lt_bigint,lt_real]) and
         (arg2^.literalType in [lt_smallint,lt_bigint,lt_real]) and (arg2^.isInRelationTo(tt_comparatorGrt,arg1)) and
         (arg3^.literalType in [lt_smallint,lt_bigint]) and (int3^.isBetween(2,maxLongint)) then begin
        context.messages.globalMessages^.postCustomMessage(addRowMessage(generateRow(P_expressionLiteral(arg0),
                                                                fReal(arg1),
                                                                fReal(arg2),
                                                                int3^.intValue,
                                                                tokenLocation,
                                                                context)),true);
        exit(newVoidLiteral);
      end;
    end;
  end;

FUNCTION plot intFuncSignature;
  begin
    if not(context.checkSideEffects('plot',tokenLocation,[se_alterPlotState])) then exit(nil);
    context.messages.globalMessages^.postSingal(mt_plot_clear,C_nilTokenLocation);
    if (params=nil) or (params^.size=0) or (params^.size = 1) and (arg0^.literalType = lt_emptyList)
    then result:=newVoidLiteral
    else result:=addPlot(params, tokenLocation,context);
  end;

FUNCTION getOptions intFuncSignature;
  VAR opt:T_scalingOptions;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      opt:=getOptionsViaAdapters(context.messages);
      result:=newMapLiteral^
        .put('x0'             ,opt.axisTrafo['x'].worldMin)^
        .put('x1'             ,opt.axisTrafo['x'].worldMax)^
        .put('y0'             ,opt.axisTrafo['y'].worldMin)^
        .put('y1'             ,opt.axisTrafo['y'].worldMax)^
        .put('fontsize'       ,opt.relativeFontSize)^
        .put('preserveAspect' ,opt.preserveAspect  )^
        .put('autoscaleX'     ,opt.axisTrafo['x'].autoscale)^
        .put('autoscaleY'     ,opt.axisTrafo['y'].autoscale)^
        .put('autoscaleFactor',opt.autoscaleFactor )^
        .put('logscaleX'      ,opt.axisTrafo['x'].logscale)^
        .put('logscaleY'      ,opt.axisTrafo['y'].logscale)^
        .put('axisStyleX'     ,byte(opt.axisStyle['x']))^
        .put('axisStyleY'     ,byte(opt.axisStyle['y']));
    end;
  end;

FUNCTION setOptions intFuncSignature;
  VAR opt:T_scalingOptions;
      allOkay:boolean=true;
  PROCEDURE matchKey(CONST key:string; CONST value:P_literal);
    PROCEDURE fail;
      begin
        context.messages.globalMessages^.postTextMessage(mt_el2_warning,tokenLocation,
          'invalid plot option ; key="'+key+'" is not known or not compatible with value '+value^.toString);
        allOkay:=false;
      end;

    VAR f:double;
    begin
      if (key='x0'            ) and (value^.literalType in [lt_smallint,lt_bigint,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.axisTrafo['x'].worldMin:=f;
      end else
      if (key='x1'            ) and (value^.literalType in [lt_smallint,lt_bigint,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.axisTrafo['x'].worldMax:=f;
      end else
      if (key='y0'            ) and (value^.literalType in [lt_smallint,lt_bigint,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.axisTrafo['y'].worldMin:=f;
      end else
      if (key='y1'            ) and (value^.literalType in [lt_smallint,lt_bigint,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.axisTrafo['y'].worldMax:=f;
      end else
      if (key='fontsize'      ) and (value^.literalType in [lt_smallint,lt_bigint,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.relativeFontSize:=f;
      end else
      if (key='autoscaleFactor') and (value^.literalType in [lt_smallint,lt_bigint,lt_real]) then begin
        f:=fReal(value); if isNan(f) or (f<1E-3) then fail else opt.autoscaleFactor:=f;
      end else
      if (key='preserveAspect') and (value^.literalType=lt_boolean) then begin
        opt.preserveAspect:=P_boolLiteral(value)^.value;
      end else
      if (key='autoscaleX'    ) and (value^.literalType=lt_boolean) then begin
        opt.axisTrafo['x'].autoscale:=P_boolLiteral(value)^.value;
      end else
      if (key='autoscaleY'    ) and (value^.literalType=lt_boolean) then begin
        opt.axisTrafo['y'].autoscale:=P_boolLiteral(value)^.value;
      end else
      if (key='logscaleX'     ) and (value^.literalType=lt_boolean) then begin
        opt.axisTrafo['x'].logscale:=P_boolLiteral(value)^.value;
      end else
      if (key='logscaleY'     ) and (value^.literalType=lt_boolean) then begin
        opt.axisTrafo['y'].logscale:=P_boolLiteral(value)^.value;
      end else
      if (key='axisStyleX'    ) and (value^.literalType in [lt_smallint,lt_bigint]) then begin
        opt.axisStyle['x']:=P_abstractIntLiteral(value)^.intValue and 7;
      end else
      if (key='axisStyleY'    ) and (value^.literalType in [lt_smallint,lt_bigint]) then begin
        opt.axisStyle['y']:=P_abstractIntLiteral(value)^.intValue and 7;
      end else fail;
    end;

  VAR pair:P_literal;
      iter:T_arrayOfLiteral;
      postOptionsMessage:P_plotOptionsMessage;
  begin
    if not(context.checkSideEffects('setOptions',tokenLocation,[se_alterPlotState])) then exit(nil);
    result:=nil;
    opt:=getOptionsViaAdapters(context.messages);
    if (params<>nil) and (params^.size=1) and ((arg0^.literalType=lt_map) or (arg0^.literalType in C_listTypes+C_setTypes) and (list0^.isKeyValueCollection)) then begin
      iter:=compound0^.iteratableList;
      for pair in iter do if P_listLiteral(pair)^.value[0]^.literalType<>lt_string then begin
        disposeLiteral(iter);
        exit(nil);
      end;
      for pair in iter do
        matchKey(P_stringLiteral(P_listLiteral(pair)^.value[0])^.value,P_listLiteral(pair)^.value[1]);
      disposeLiteral(iter);
      result:=newBoolLiteral(allOkay);
    end else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType in [lt_real,lt_smallint,lt_bigint,lt_boolean]) then begin
      matchKey(str0^.value,arg1);
      result:=newBoolLiteral(allOkay);
    end else allOkay:=false;
    if allOkay then begin
      new(postOptionsMessage,createPostRequest(opt));
      context.messages.globalMessages^.postCustomMessage(postOptionsMessage,true);
    end;
  end;

FUNCTION resetOptions_impl intFuncSignature;
  VAR opt:T_scalingOptions;
      postOptionsMessage:P_plotOptionsMessage;
  begin
    if not(context.checkSideEffects('resetOptions',tokenLocation,[se_alterPlotState])) then exit(nil);
    if (params=nil) or (params^.size=0) then begin
      opt.setDefaults;
      new(postOptionsMessage,createPostRequest(opt));
      context.messages.globalMessages^.postCustomMessage(postOptionsMessage);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION renderToFile_impl intFuncSignature;
  VAR fileName: ansistring;
      width, height, quality: longint;
      renderRequest:P_plotRenderRequest;
  begin
    if not(context.checkSideEffects('renderToFile',tokenLocation,[se_writeFile])) then exit(nil);
    result:=nil;
    if (params<>nil) and (params^.size>=3) and
      (arg0^.literalType = lt_string) and
      (arg1^.literalType in [lt_smallint,lt_bigint]) and
      (arg2^.literalType in [lt_smallint,lt_bigint]) and
      ((params^.size = 3) or (params^.size = 4) and
      (arg3^.literalType in [lt_smallint,lt_bigint])) then begin
      fileName:=str0^.value;
      width :=int1^.intValue;
      height:=int2^.intValue;
      if params^.size>3 then quality:=int3^.intValue
                        else quality:=0;
      if (str0^.value = '') or (width<1) or (height<1) or (quality<PLOT_QUALITY_LOW) or (quality>PLOT_QUALITY_HIGH) then exit(nil);
      try
        fileName:=ChangeFileExt(str0^.value,'.png');
        new(renderRequest,createRenderToFileRequest(fileName,width,height,quality));
        context.messages.globalMessages^.postCustomMessage(renderRequest,true);
      except
        on e:Exception do begin
          context.messages.raiseError('Error on renderToFile: '+e.message,tokenLocation);
          exit(nil);
        end;
      end;
      result:=newVoidLiteral;
    end;
  end;

FUNCTION renderToString_impl intFuncSignature;
  VAR width, height, quality: longint;
      renderRequest:P_plotRenderRequest;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and
      (arg0^.literalType in [lt_smallint,lt_bigint]) and
      (arg1^.literalType in [lt_smallint,lt_bigint]) and
      ((params^.size = 2) or (params^.size = 3) and
      (arg2^.literalType in [lt_smallint,lt_bigint])) then begin
      width:=int0^.intValue;
      height:=int1^.intValue;
      if params^.size>2 then quality:=int2^.intValue
                        else quality:=0;
      if  (width<1) or (height<1) or (quality<PLOT_QUALITY_LOW) or (quality>PLOT_QUALITY_HIGH) then exit(nil);
      new(renderRequest,createRenderToStringRequest(width,height,quality));
      context.messages.globalMessages^.postCustomMessage(renderRequest^.rereferenced);
      result:=newStringLiteral(renderRequest^.getStringWaiting(context.messages));
      disposeMessage(renderRequest);
    end;
  end;

FUNCTION removePlot_imp intFuncSignature;
  VAR toDrop:longint=1;
      dropPlotMessage:P_plotDropRowRequest;
  begin
    if not(context.checkSideEffects('removePlot',tokenLocation,[se_alterPlotState])) then exit(nil);
    if (params=nil) or (params^.size=0) or
       (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint]) and (int0^.isBetween(1,maxLongint)) then begin
      if (params<>nil) and (params^.size=1) then toDrop:=int0^.intValue;
      new(dropPlotMessage,create(toDrop));
      context.messages.globalMessages^.postCustomMessage(dropPlotMessage);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION drawTextRelativeOrAbsolute(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext; CONST abspos:boolean):P_literal;
  VAR txt:T_customText;
      lines:T_arrayOfString;
      i:longint;

      hasCol   :boolean=false;
      hasAnchor:boolean=false;
      postRequest:P_addTextMessage;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=3) and
       (arg0^.literalType in [lt_smallint,lt_bigint,lt_real]) and
       (arg1^.literalType in [lt_smallint,lt_bigint,lt_real]) and
       (arg2^.literalType in [lt_string,lt_stringList,lt_emptyList]) then begin
      initialize(lines);
      case arg2^.literalType of
        lt_string: lines:=split(str2^.value,C_lineBreakChar);
        lt_emptyList: lines:=C_EMPTY_STRING_ARRAY;
        lt_stringList: begin
          setLength(lines,0);
          for i:=0 to list2^.size-1 do append(lines,P_stringLiteral(list2^.value[i])^.value);
        end;
      end;

      txt.create(fReal(arg0),fReal(arg1),lines);
      txt.absolutePosition:=abspos;
      for i:=3 to params^.size-1 do begin
        case params^.value[i]^.literalType of
          lt_real,lt_smallint,lt_bigint: txt.fontSize:=fReal(params^.value[i]);
          lt_string: if hasAnchor then txt.fontName:=P_stringLiteral(params^.value[i])^.value
                                  else begin
                                    txt.setAnchor(P_stringLiteral(params^.value[i])^.value);
                                    hasAnchor:=true;
                                  end;
          lt_intList,lt_realList,
          lt_numList: if P_listLiteral(params^.value[i])^.size=3 then begin
            if hasCol then txt.setBackground(fReal(P_listLiteral(params^.value[i])^.value[0]),
                                             fReal(P_listLiteral(params^.value[i])^.value[1]),
                                             fReal(P_listLiteral(params^.value[i])^.value[2]))
                      else begin
                        txt.setForeground(fReal(P_listLiteral(params^.value[i])^.value[0]),
                                          fReal(P_listLiteral(params^.value[i])^.value[1]),
                                          fReal(P_listLiteral(params^.value[i])^.value[2]));
                        hasCol:=true;
                      end;
          end else exit(nil);
        end;
      end;
      new(postRequest,create(txt));
      context.messages.globalMessages^.postCustomMessage(postRequest,true);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION drawText_imp    intFuncSignature; begin result:=nil; if context.checkSideEffects('drawText'        ,tokenLocation,[se_alterPlotState]) then result:=drawTextRelativeOrAbsolute(params,tokenLocation,context,false); end;
FUNCTION drawTextAbs_imp intFuncSignature; begin result:=nil; if context.checkSideEffects('drawTextAbsolute',tokenLocation,[se_alterPlotState]) then result:=drawTextRelativeOrAbsolute(params,tokenLocation,context,true); end;

INITIALIZATION
  mnh_funcs.registerRule(PLOT_NAMESPACE,'plot', @plot, ak_variadic,
    'plot(list,[options]); //plots flat numeric list or xy-list'+
    '#plot(xList,yList,[options]); //plots flat numeric list or xy-list'+
    '#plot(f:expression(1),t0,t1>t0,samples>=2,[options]); //plots f versus t in [t0,t1]'+
    '#options are optional and given in the form of a string, the individual option items being delimited by spaces'+
    '#valid options are:'+'#Style/size modifier: any real number'+
    '#Styles:'+
    '#  line; l;'+
    '#  stepLeft;'+
    '#  stepRight;'+
    '#  fill; f;'+
    '#  fillSolid; fs;'+
    '#  bar;'+
    '#  box;'+
    '#  tube;'+
    '#  dot; .;'+
    '#  plus; +;'+
    '#  cross; x;'+
    '#  impulse; i;'+
    '#  polygon; p;'+
    '#Colors:'+'#  black;'+
    '#  red;'+'#  blue;'+'#  green;'+'#  purple;'+
    '#  orange;'+'#  RGB$,$,$; //With three real numbers in range [0,1]'+
    '#  HSV$,$,$; //With three real numbers in range [0,1]'+
    '#  HUE$; //With one real number '+
    '#  GREY$; //With one real number in range [0,1]'+
    '#Transparency Index:'+'  #  TI$;// with an integer $');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'addPlot', @addPlot, ak_variadic_1,
    'addPlot(list,[options]); //adds plot of flat numeric list or xy-list'+
    '#addPlot(xList,yList,[options]); //adds plot of flat numeric list or xy-list'+
    '#addPlot(f:expression(1),t0,t1>t0,samples>=2,[options]); //adds plot of f versus t in [t0,t1]');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getOptions',@getOptions, ak_nullary,
    'getOptions;//returns plot options as a key-value-list.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setOptions',@setOptions, ak_variadic_1,
    'setOptions(set:keyValueList);//Sets options via a key value list of the same form as returned by plot.getOptions#'+
    'setOptions(key:string,value);//Sets a single plot option');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'resetOptions',@resetOptions_impl, ak_nullary,
    'resetOptions;//Sets the default plot options');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'renderToFile', @renderToFile_impl, ak_variadic_3,
    'renderToFile(filename<>'',width>=1,height>=1,[quality in [0..3]]);//Renders the current plot to a file.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'renderToString', @renderToString_impl, ak_variadic_2,
    'renderToString(width,height,[quality in [0..3]]);//Renders the current plot to a string.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'removePlot',@removePlot_imp, ak_nullary,
    'removePlot;//Removes the last row from the plot#removePlot(n>=1);//Removed the last n rows from the plot');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'drawText',@drawText_imp, ak_variadic_3,
    'drawText(x,y,text);//Draws custom text#'+
    'drawText(x,y,text,size:Numeric,anchor in ["TL","T","TR","CL","C","CR","BL","B","BR"],font:String,textCol:IntList(3),backgroundCol:IntList(3));//Draws text with custom options. Custom parameters are optional');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'drawTextAbsolute',@drawTextAbs_imp, ak_variadic_3,
    'drawTextAbsolute(x,y,text);//Draws custom text at absolute position#'+
    'drawTextAbsolute(x,y,text,size:Numeric,anchor in ["TL","T","TR","CL","C","CR","BL","B","BR"],font:String,textCol:IntList(3),backgroundCol:IntList(3));//Draws text with custom options. Custom parameters are optional');
end.
