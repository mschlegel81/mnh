UNIT mnh_funcs_plot;
INTERFACE
{$WARN 5024 OFF}
USES sysutils, mnh_funcs,mnh_litVar,mnh_basicTypes,mnh_out_adapters,mnh_plotData,math,mnh_constants,mnh_contexts,myGenerics;
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
      lt_int: result:=P_intLiteral(x)^.value;
      else result:=Nan;
    end;
  end;

FUNCTION newDataRow(CONST y:P_listLiteral; CONST x:P_listLiteral=nil):T_dataRow;
  VAR i,imax:longint;
      xy:P_listLiteral;
  PROCEDURE addSample(CONST px,py:double);
    VAR k:longint;
    begin
      k:=length(result);
      setLength(result,k+1);
      result[k,0]:=px;
      result[k,1]:=py;
    end;

  begin
    setLength(result,0);
    if x=nil then case y^.literalType of
      lt_list:
        with y^ do for i:=0 to size-1 do if value[i]^.literalType in [lt_intList,lt_realList,lt_numList] then begin
          xy:=P_listLiteral(value[i]);
          if xy^.size=2 then addSample(fReal(xy^.value[0]), fReal(xy^.value[1]))
                        else addSample(Nan,Nan);
        end else addSample(Nan,Nan);
      lt_intList, lt_realList, lt_numList:
        with y^ do for i:=0 to size-1 do addSample(i,fReal(value[i]));
    end else begin
      imax:=min(X^.size, Y^.size);
      for i:=0 to imax-1 do addSample(fReal(X^.value[i]), fReal(Y^.value[i]));
    end;
  end;

FUNCTION addPlot intFuncSignature;
  VAR options: ansistring = '';
      sizeWithoutOptions: longint;
  begin
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
        context.adapters^.plot^.addRow(options,newDataRow(list0));
        context.adapters^.logDeferredPlot;
        exit(newVoidLiteral);
      end;
      if (sizeWithoutOptions = 2) and
         (arg0^.literalType in [lt_intList, lt_realList, lt_numList]) and
         (arg1^.literalType in [lt_intList, lt_realList, lt_numList]) and
         (list0^.size=list1^.size) then begin
        context.adapters^.plot^.addRow(options,newDataRow(list1,list0));
        context.adapters^.logDeferredPlot;
        exit(newVoidLiteral);
      end;
      if (sizeWithoutOptions = 4) and
         (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(1)) and
         (arg1^.literalType in [lt_int,lt_real]) and
         (arg2^.literalType in [lt_int,lt_real]) and (arg2^.isInRelationTo(tt_comparatorGrt,arg1)) and
         (arg3^.literalType=lt_int) and (int3^.value>=2) then begin
        context.adapters^.plot^.addRow(options,generateRow(P_expressionLiteral(arg0),
                                                          fReal(arg1),
                                                          fReal(arg2),
                                                          int3^.value,
                                                          tokenLocation,
                                                          context));
        if context.adapters^.noErrors then context.adapters^.logDeferredPlot;
        exit(newVoidLiteral);
      end;
    end;
  end;

FUNCTION plot intFuncSignature;
  begin
    context.adapters^.plot^.clear;
    if (params=nil) or (params^.size=0) or (params^.size = 1) and (arg0^.literalType = lt_emptyList)
    then result:=newVoidLiteral
    else result:=addPlot(params, tokenLocation,context);
  end;

FUNCTION getOptions intFuncSignature;
  VAR opt:T_scalingOptions;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      opt:=context.adapters^.plot^.options;
      result:=newMapLiteral^
        .put('x0'             ,opt.range['x',0]    )^
        .put('x1'             ,opt.range['x',1]    )^
        .put('y0'             ,opt.range['y',0]    )^
        .put('y1'             ,opt.range['y',1]    )^
        .put('fontsize'       ,opt.relativeFontSize)^
        .put('preserveAspect' ,opt.preserveAspect  )^
        .put('autoscaleX'     ,opt.autoscale['x']  )^
        .put('autoscaleY'     ,opt.autoscale['y']  )^
        .put('autoscaleFactor',opt.autoscaleFactor )^
        .put('logscaleX'      ,opt.logscale['x']   )^
        .put('logscaleY'      ,opt.logscale['y']   )^
        .put('axisStyleX'     ,opt.axisStyle['x']  )^
        .put('axisStyleY'     ,opt.axisStyle['y']  );
    end;
  end;

FUNCTION setOptions intFuncSignature;
  VAR opt:T_scalingOptions;
      allOkay:boolean=true;
  PROCEDURE matchKey(CONST key:string; CONST value:P_literal);
    PROCEDURE fail;
      begin
        context.adapters^.raiseWarning('invalid plot option ; key="'+key+'" is not known or not compatible with value '+value^.toString,tokenLocation);
        allOkay:=false;
      end;

    VAR f:double;
    begin
      if (key='x0'            ) and (value^.literalType in [lt_int,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.range['x',0]:=f;
      end else
      if (key='x1'            ) and (value^.literalType in [lt_int,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.range['x',1]:=f;
      end else
      if (key='y0'            ) and (value^.literalType in [lt_int,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.range['y',0]:=f;
      end else
      if (key='y1'            ) and (value^.literalType in [lt_int,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.range['y',1]:=f;
      end else
      if (key='fontsize'      ) and (value^.literalType in [lt_int,lt_real]) then begin
        f:=fReal(value); if isNan(f) then fail else opt.relativeFontSize:=f;
      end else
      if (key='autoscaleFactor') and (value^.literalType in [lt_int,lt_real]) then begin
        f:=fReal(value); if isNan(f) or (f<1E-3) then fail else opt.autoscaleFactor:=f;
      end else
      if (key='preserveAspect') and (value^.literalType=lt_boolean) then begin
        opt.preserveAspect:=P_boolLiteral(value)^.value;
      end else
      if (key='autoscaleX'    ) and (value^.literalType=lt_boolean) then begin
        opt.autoscale['x']:=P_boolLiteral(value)^.value;
      end else
      if (key='autoscaleY'    ) and (value^.literalType=lt_boolean) then begin
        opt.autoscale['y']:=P_boolLiteral(value)^.value;
      end else
      if (key='logscaleX'     ) and (value^.literalType=lt_boolean) then begin
        opt.logscale['x']:=P_boolLiteral(value)^.value;
      end else
      if (key='logscaleY'     ) and (value^.literalType=lt_boolean) then begin
        opt.logscale['y']:=P_boolLiteral(value)^.value;
      end else
      if (key='axisStyleX'    ) and (value^.literalType=lt_int) then begin
        opt.axisStyle['x']:=P_intLiteral(value)^.value and 7;
      end else
      if (key='axisStyleY'    ) and (value^.literalType=lt_int) then begin
        opt.axisStyle['y']:=P_intLiteral(value)^.value and 7;
      end else fail;
    end;

  VAR pair:P_literal;
      iter:T_arrayOfLiteral;
  begin
    result:=nil;
    opt:=context.adapters^.plot^.options;
    if (params<>nil) and (params^.size=1) and ((arg0^.literalType=lt_map) or (arg0^.literalType in C_listTypes+C_setTypes) and (list0^.isKeyValueCollection)) then begin
      iter:=compound0^.iteratableList;
      for pair in iter do if P_listLiteral(pair)^[0]^.literalType<>lt_string then begin
        disposeLiteral(iter);
        exit(nil);
      end;
      for pair in iter do
        matchKey(P_stringLiteral(P_listLiteral(pair)^[0])^.value,P_listLiteral(pair)^[1]);
      disposeLiteral(iter);
      result:=newBoolLiteral(allOkay);
    end else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType in [lt_real,lt_int,lt_boolean]) then begin
      matchKey(str0^.value,arg1);
      result:=newBoolLiteral(allOkay);
    end else allOkay:=false;
    if allOkay then begin
      context.adapters^.plot^.options:=opt;
      context.adapters^.logPlotSettingsChanged;
    end;
  end;

FUNCTION resetOptions_impl intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      context.adapters^.plot^.setDefaults;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION renderToFile_impl intFuncSignature;
  VAR fileName: ansistring;
      width, height, supersampling: longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=3) and
      (arg0^.literalType = lt_string) and
      (arg1^.literalType = lt_int) and
      (arg2^.literalType = lt_int) and
      ((params^.size = 3) or (params^.size = 4) and
      (arg3^.literalType = lt_int)) then begin
      fileName:=str0^.value;
      width:=int1^.value;
      height:=int2^.value;
      if params^.size>3 then supersampling:=int3^.value
                        else supersampling:=1;
      if (fileName = '') or (width<1) or (height<1) or (supersampling<1) then begin
        context.adapters^.raiseError(
          'Function renderToFile expects parameters (filename,width,height,[supersampling]).',
          tokenLocation);
        exit(nil);
      end;
      try
        fileName:=ChangeFileExt(fileName,'.png');
        context.adapters^.plot^.renderToFile(fileName,width,height,supersampling);
        context.adapters^.logPlotFileCreated(expandFileName(ChangeFileExt(fileName,'.png')),tokenLocation);
      except
        on e:Exception do begin
          context.adapters^.raiseError('Error on renderToFile: '+e.message,tokenLocation);
          exit(nil);
        end;
      end;
      result:=newVoidLiteral;
    end;
  end;

FUNCTION renderToString_impl intFuncSignature;
  VAR width, height, supersampling: longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and
      (arg0^.literalType = lt_int) and
      (arg1^.literalType = lt_int) and
      ((params^.size = 2) or (params^.size = 3) and
      (arg2^.literalType = lt_int)) then begin
      width:=int0^.value;
      height:=int1^.value;
      if params^.size>2 then supersampling:=int2^.value
                        else supersampling:=1;
      if  (width<1) or (height<1) or (supersampling<1) then begin
        context.adapters^.raiseError(
          'Function renderToString expects parameters (filename,width,height,[supersampling]).',
          tokenLocation);
        exit(nil);
      end;
      result:=newStringLiteral(context.adapters^.plot^.renderToString(width,height,supersampling));
    end;
  end;

FUNCTION removePlot_imp intFuncSignature;
  VAR toDrop:longint=1;
  begin
    if (params=nil) or (params^.size=0) or
       (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) and (int0^.value>0) then begin
      if (params<>nil) and (params^.size=1) then toDrop:=int0^.value;
      context.adapters^.plot^.removeRows(toDrop);
      context.adapters^.logDeferredPlot;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION display_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      context.adapters^.logInstantPlot;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

INITIALIZATION
  mnh_funcs.registerRule(PLOT_NAMESPACE,'plot', @plot, [se_writingInternal], ak_variadic,
    'plot(list,[options]); //plots flat numeric list or xy-list'+
    '#plot(xList,yList,[options]); //plots flat numeric list or xy-list'+
    '#plot(f:expression(1),t0,t1>t0,samples>=2,[options]); //plots f versus t in [t0,t1]'+
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
  mnh_funcs.registerRule(PLOT_NAMESPACE,'addPlot', @addPlot, [se_writingInternal], ak_variadic_1,
    'addPlot(list,[options]); //adds plot of flat numeric list or xy-list'+
    '#addPlot(xList,yList,[options]); //adds plot of flat numeric list or xy-list'+
    '#addPlot(f:expression(1),t0,t1>t0,samples>=2,[options]); //adds plot of f versus t in [t0,t1]');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getOptions',@getOptions, [se_readingInternal], ak_nullary,
    'getOptions;//returns plot options as a key-value-list.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setOptions',@setOptions, [se_writingInternal], ak_variadic_1,
    'setOptions(set:keyValueList);//Sets options via a key value list of the same form as returned by plot.getOptions#'+
    'setOptions(key:string,value);//Sets a single plot option');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'resetOptions',@resetOptions_impl, [se_writingInternal], ak_nullary,
    'resetOptions;//Sets the default plot options');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'renderToFile', @renderToFile_impl, [se_readingInternal,se_writingExternal], ak_variadic_3,
    'renderToFile(filename,width,height,[supersampling]);//Renders the current plot to a file.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'renderToString', @renderToString_impl, [se_readingInternal], ak_variadic_2,
    'renderToString(width,height,[supersampling]);//Renders the current plot to a string.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'removePlot',@removePlot_imp, [se_writingInternal], ak_nullary,
    'removePlot;//Removes the last row from the plot#removePlot(n>=1);//Removed the last n rows from the plot');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'display',@display_imp, [se_readingInternal,se_outputViaAdapter], ak_nullary,
    'display;//Displays the plot as soon as possible, even during evaluation.');

end.
