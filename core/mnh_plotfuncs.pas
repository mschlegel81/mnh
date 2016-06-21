UNIT mnh_plotFuncs;
INTERFACE
{$WARN 5024 OFF}
USES sysutils, mnh_funcs,mnh_litVar,mnh_tokLoc,mnh_out_adapters,mnh_plotData,math,mnh_constants,mnh_contexts;

IMPLEMENTATION
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

FUNCTION addPlot(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR options: ansistring = '';
      sizeWithoutOptions: longint;
      i,imax: longint;
      X,Y:    P_listLiteral;

      row:    T_dataRow;

  PROCEDURE addSample(CONST px,py:double);
    VAR k:longint;
    begin
      k:=length(row);
      setLength(row,k+1);
      row[k,0]:=px;
      row[k,1]:=py;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) then begin
      setLength(row,0);
      if (params^.value(params^.size-1)^.literalType = lt_string) then begin
        options:=P_stringLiteral(params^.value(params^.size-1))^.value;
        sizeWithoutOptions:=params^.size-1;
      end else begin
        options:='';
        sizeWithoutOptions:=params^.size;
      end;
      if (sizeWithoutOptions = 1) and (params^.value(0)^.literalType = lt_list) then begin
        with P_listLiteral(params^.value(0))^ do
        for i:=0 to size-1 do if value(i)^.literalType in [lt_intList,lt_realList,lt_numList] then begin
          Y:=P_listLiteral(value(i));
          if Y^.size=2 then addSample(fReal(Y^.value(0)), fReal(Y^.value(1)))
                       else addSample(Nan,Nan);
        end else addSample(Nan,Nan);
        context.adapters^.plot.addRow(options,row);
        context.adapters^.raiseCustomMessage(mt_plotCreatedWithDeferredDisplay,'',tokenLocation);
        setLength(row,0);
        exit(newVoidLiteral);
      end;
      if (sizeWithoutOptions = 1) and (params^.value(0)^.literalType in [lt_intList, lt_realList, lt_numList]) then begin
        with P_listLiteral(params^.value(0))^ do
        for i:=0 to size-1 do addSample(i,fReal(value(i)));
        context.adapters^.plot.addRow(options,row);
        context.adapters^.raiseCustomMessage(mt_plotCreatedWithDeferredDisplay,'',tokenLocation);
        setLength(row,0);
        exit(newVoidLiteral);
      end;
      if (sizeWithoutOptions = 2) and
         (params^.value(0)^.literalType in [lt_intList, lt_realList, lt_numList]) and
         (params^.value(1)^.literalType in [lt_intList, lt_realList, lt_numList]) then begin
        X:=P_listLiteral(params^.value(0));
        Y:=P_listLiteral(params^.value(1));
        imax:=min(X^.size, Y^.size);
        for i:=0 to imax-1 do addSample(fReal(X^.value(i)), fReal(Y^.value(i)));
        context.adapters^.plot.addRow(options,row);
        context.adapters^.raiseCustomMessage(mt_plotCreatedWithDeferredDisplay,'',tokenLocation);
        setLength(row,0);
        exit(newVoidLiteral);
      end;
    end;
  end;

FUNCTION plot(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    context.adapters^.plot.clear;
    context.adapters^.raiseCustomMessage(mt_plotCreatedWithDeferredDisplay,'',tokenLocation);
    if (params=nil) or (params^.size=0) or (params^.size = 1) and (params^.value(0)^.literalType = lt_emptyList)
    then result:=newVoidLiteral
    else result:=addPlot(params, tokenLocation,context);
  end;

FUNCTION getOptions(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR opt:T_scalingOptions;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      opt:=context.adapters^.plot.options;
      result:=newListLiteral^
        .append(newListLiteral^.appendString('x0'            )^.appendReal(opt.range['x',0]),false)^
        .append(newListLiteral^.appendString('x1'            )^.appendReal(opt.range['x',1]),false)^
        .append(newListLiteral^.appendString('y0'            )^.appendReal(opt.range['y',0]),false)^
        .append(newListLiteral^.appendString('y1'            )^.appendReal(opt.range['y',1]),false)^
        .append(newListLiteral^.appendString('fontsize'      )^.appendReal(opt.relativeFontSize),false)^
        .append(newListLiteral^.appendString('preserveAspect')^.appendBool(opt.preserveAspect),false)^
        .append(newListLiteral^.appendString('autoscaleX'    )^.appendBool(opt.autoscale['x']),false)^
        .append(newListLiteral^.appendString('autoscaleY'    )^.appendBool(opt.autoscale['y']),false)^
        .append(newListLiteral^.appendString('logscaleX'     )^.appendBool(opt.logscale['x']),false)^
        .append(newListLiteral^.appendString('logscaleY'     )^.appendBool(opt.logscale['y']),false)^
        .append(newListLiteral^.appendString('axisStyleX'    )^.appendInt(opt.axisStyle['x']),false)^
        .append(newListLiteral^.appendString('axisStyleY'    )^.appendInt(opt.axisStyle['y']),false);
    end;
  end;

FUNCTION setOptions(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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

  VAR pair:P_listLiteral;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_keyValueList) then begin
      opt:=context.adapters^.plot.options;
      for i:=0 to P_listLiteral(params^.value(0))^.size-1 do begin
        pair:=P_listLiteral(P_listLiteral(params^.value(0))^.value(i));
        matchKey(P_stringLiteral(pair^.value(0))^.value,
                                 pair^.value(1));
      end;
      result:=newBoolLiteral(allOkay);
    end else if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) and (params^.value(1)^.literalType in [lt_real,lt_int,lt_boolean]) then begin
      opt:=context.adapters^.plot.options;
      matchKey(P_stringLiteral(params^.value(0))^.value,params^.value(1));
      result:=newBoolLiteral(allOkay);
    end else allOkay:=false;
    if allOkay then begin
      context.adapters^.plot.options:=opt;
      context.adapters^.raiseCustomMessage(mt_plotSettingsChanged,'',tokenLocation);
    end;
  end;

FUNCTION renderToFile_impl(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR fileName: ansistring;
      width, height, supersampling: longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=3) and
      (params^.value(0)^.literalType = lt_string) and
      (params^.value(1)^.literalType = lt_int) and
      (params^.value(2)^.literalType = lt_int) and
      ((params^.size = 3) or (params^.size = 4) and
      (params^.value(3)^.literalType = lt_int)) then begin
      fileName:=P_stringLiteral(params^.value(0))^.value;
      width:=P_intLiteral(params^.value(1))^.value;
      height:=P_intLiteral(params^.value(2))^.value;
      if params^.size>3 then supersampling:=P_intLiteral(params^.value(3))^.value
                        else supersampling:=1;
      if (fileName = '') or (width<1) or (height<1) or (supersampling<1) then begin
        context.adapters^.raiseError(
          'Function renderToFile expects parameters (filename,width,height,[supersampling]).',
          tokenLocation);
        exit(nil);
      end;
      context.adapters^.plot.renderToFile(fileName,width,height,supersampling);
      context.adapters^.raiseCustomMessage(mt_plotFileCreated,expandFileName( ChangeFileExt(fileName, '.png')),tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION renderToString_impl(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR width, height, supersampling: longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and
      (params^.value(0)^.literalType = lt_int) and
      (params^.value(1)^.literalType = lt_int) and
      ((params^.size = 2) or (params^.size = 3) and
      (params^.value(2)^.literalType = lt_int)) then begin
      width:=P_intLiteral(params^.value(0))^.value;
      height:=P_intLiteral(params^.value(1))^.value;
      if params^.size>2 then supersampling:=P_intLiteral(params^.value(2))^.value
                        else supersampling:=1;
      if  (width<1) or (height<1) or (supersampling<1) then begin
        context.adapters^.raiseError(
          'Function renderToString expects parameters (filename,width,height,[supersampling]).',
          tokenLocation);
        exit(nil);
      end;
      result:=newStringLiteral(context.adapters^.plot.renderToString(width,height,supersampling));
    end;
  end;

FUNCTION display_imp(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params=nil) or (params^.size=0) then begin
      context.adapters^.hasMessageOfType[mt_plotCreatedWithDeferredDisplay]:=false;
      context.adapters^.raiseCustomMessage(mt_plotCreatedWithInstantDisplay,'',tokenLocation);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

INITIALIZATION
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
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getOptions',@getOptions,
    'getOptions;#returns plot options as a key-value-list.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setOptions',@setOptions,
    'setOptions(set:keyValueList);#Sets options via a key value list of the same form as returned by plot.getOptions#'+
    'setOptions(key:string,value);#Sets a single plot option');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'renderToFile', @renderToFile_impl,
    'renderToFile(filename,width,height,[supersampling]);#Renders the current plot to a file.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'renderToString', @renderToString_impl,
    'renderToString(width,height,[supersampling]);#Renders the current plot to a string.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'display',@display_imp,
    'display;#Displays the plot as soon as possible, even during evaluation.');

end.
