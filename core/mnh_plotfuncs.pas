UNIT mnh_plotFuncs;
INTERFACE
USES sysutils, mnh_funcs,mnh_litVar,mnh_tokLoc,mnh_out_adapters,mnh_plotData,math,mnh_constants;

FUNCTION plot(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION addPlot(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION setAutoscale(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION setLogscale(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION setPlotRange(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION setAxisStyle(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION setPreserveAspect(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;

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

FUNCTION addPlot(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR options: ansistring = '';
      sizeWithoutOptions: longint;
      i,iMax: longint;
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
        adapters.plot.addRow(options,row);
        adapters.raiseCustomMessage(mt_plotCreatedWithDeferredDisplay,'',tokenLocation);
        setLength(row,0);
        exit(newVoidLiteral);
      end;
      if (sizeWithoutOptions = 1) and (params^.value(0)^.literalType in [lt_intList, lt_realList, lt_numList]) then begin
        with P_listLiteral(params^.value(0))^ do
        for i:=0 to size-1 do addSample(i,fReal(value(i)));
        adapters.plot.addRow(options,row);
        adapters.raiseCustomMessage(mt_plotCreatedWithDeferredDisplay,'',tokenLocation);
        setLength(row,0);
        exit(newVoidLiteral);
      end;
      if (sizeWithoutOptions = 2) and
         (params^.value(0)^.literalType in [lt_intList, lt_realList, lt_numList]) and
         (params^.value(1)^.literalType in [lt_intList, lt_realList, lt_numList]) then begin
        X:=P_listLiteral(params^.value(0));
        Y:=P_listLiteral(params^.value(1));
        iMax:=min(X^.size, Y^.size);
        for i:=0 to iMax-1 do addSample(fReal(X^.value(i)), fReal(Y^.value(i)));
        adapters.plot.addRow(options,row);
        adapters.raiseCustomMessage(mt_plotCreatedWithDeferredDisplay,'',tokenLocation);
        setLength(row,0);
        exit(newVoidLiteral);
      end;
    end;
  end;

FUNCTION plot(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    adapters.plot.clear;
    adapters.raiseCustomMessage(mt_plotCreatedWithDeferredDisplay,'',tokenLocation);
    if (params=nil) or (params^.size=0) or (params^.size = 1) and (params^.value(0)^.literalType = lt_emptyList)
    then result:=newVoidLiteral
    else result:=addPlot(params, tokenLocation,adapters);
  end;

FUNCTION setAutoscale(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR o:T_scalingOptions;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
       (params^.value(0)^.literalType = lt_booleanList) and
       (P_listLiteral(params^.value(0))^.size = 2) then begin
      o:=adapters.plot.options;
      o.autoscale['x']:=P_boolLiteral(P_listLiteral(params^.value(0))^.value(0))^.value;
      o.autoscale['y']:=P_boolLiteral(P_listLiteral(params^.value(0))^.value(1))^.value;
      adapters.plot.options:=o;
      adapters.raiseCustomMessage(mt_plotSettingsChanged,'',tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION getAutoscale(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR o:T_scalingOptions;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      o:=adapters.plot.options;
      result:=newListLiteral^
             .appendBool(o.autoscale['x'])^
             .appendBool(o.autoscale['y']);
    end;
  end;

FUNCTION setLogscale(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR o:T_scalingOptions;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
       (params^.value(0)^.literalType = lt_booleanList) and
       (P_listLiteral(params^.value(0))^.size = 2) then begin
      o:=adapters.plot.options;
      o.logscale['x']:=P_boolLiteral(P_listLiteral(params^.value(0))^.value(0))^.value;
      o.logscale['y']:=P_boolLiteral(P_listLiteral(params^.value(0))^.value(1))^.value;
      adapters.plot.options:=o;
      adapters.raiseCustomMessage(mt_plotSettingsChanged,'',tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION getLogscale(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR o:T_scalingOptions;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      o:=adapters.plot.options;
      result:=newListLiteral^
             .appendBool(o.logscale['x'])^
             .appendBool(o.logscale['y']);
    end;
  end;

FUNCTION setPlotRange(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR x, y: P_literal;
      o:T_scalingOptions;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
      (params^.value(0)^.literalType = lt_list) and
      (P_listLiteral(params^.value(0))^.size = 2) then begin
      x:=P_listLiteral(params^.value(0))^.value(0);
      y:=P_listLiteral(params^.value(0))^.value(1);
      if (x^.literalType in [lt_intList, lt_realList, lt_numList]) and (P_listLiteral(x)^.size = 2) and (y^.literalType in [lt_intList, lt_realList, lt_numList]) and (P_listLiteral(y)^.size = 2) then begin
        o:=adapters.plot.options;
        o.range['x',0]:=fReal(P_listLiteral(x)^.value(0));
        o.range['x',1]:=fReal(P_listLiteral(x)^.value(1));
        o.range['y',0]:=fReal(P_listLiteral(y)^.value(0));
        o.range['y',1]:=fReal(P_listLiteral(y)^.value(1));
        adapters.plot.options:=o;
        adapters.raiseCustomMessage(mt_plotSettingsChanged,'',tokenLocation);
        result:=newVoidLiteral;
      end;
    end;
  end;

FUNCTION getPlotRange(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR o:T_scalingOptions;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      o:=adapters.plot.options;
      result:=newListLiteral^
             .append(newListLiteral^
                    .appendReal(o.range['x',0])^
                    .appendReal(o.range['x',1]),false,adapters)^
             .append(newListLiteral^
                    .appendReal(o.range['y',0])^
                    .appendReal(o.range['y',1]),false,adapters);
    end;
  end;

FUNCTION setAxisStyle(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR o:T_scalingOptions;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
       (params^.value(0)^.literalType = lt_intList) and
       (P_listLiteral(params^.value(0))^.size = 2) then begin
      o:=adapters.plot.options;
      o.axisStyle['x']:=P_intLiteral(P_listLiteral(params^.value(0))^.value(0))^.value and 255;
      o.axisStyle['y']:=P_intLiteral(P_listLiteral(params^.value(0))^.value(0))^.value and 255;
      adapters.plot.options:=o;
      adapters.raiseCustomMessage(mt_plotSettingsChanged,'',tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION getAxisStyle(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR o:T_scalingOptions;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      o:=adapters.plot.options;
      result:=newListLiteral^
             .appendInt(o.axisStyle['x'])^
             .appendInt(o.axisStyle['y']);
    end;
  end;

FUNCTION setPreserveAspect(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR o:T_scalingOptions;
  begin
    result:=nil;
    if (params<>nil) and (params^.size = 1) and
       (params^.value(0)^.literalType = lt_boolean) then begin
      o:=adapters.plot.options;
      o.preserveAspect:=P_boolLiteral(params^.value(0))^.value;
      adapters.plot.options:=o;
      adapters.raiseCustomMessage(mt_plotSettingsChanged,'',tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION getPreserveAspect(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    if (params=nil) or (params^.size=0)
    then result:=newBoolLiteral(adapters.plot.options.preserveAspect)
    else result:=nil;
  end;

FUNCTION renderToFile_impl(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
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
        adapters.raiseError(
          'Function renderToFileImpl expects parameters (filename,width,height,[supersampling]).',
          tokenLocation);
        exit(nil);
      end;
      adapters.plot.renderToFile(fileName,width,height,supersampling);
      adapters.raiseCustomMessage(mt_plotFileCreated,expandFileName( ChangeFileExt(fileName, '.png')),tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION display_imp(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    if (params=nil) or (params^.size=0) then begin
      adapters.hasMessageOfType[mt_plotCreatedWithDeferredDisplay]:=false;
      adapters.raiseCustomMessage(mt_plotCreatedWithInstantDisplay,'',tokenLocation);
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
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setAutoscale', @setAutoscale,
    'setAutoscale([forX,forY]);#Sets autoscale per axis and returns true#Expects a tuple of two booleans as parameter.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getAutoscale', @getAutoscale,
    'getAutoscale;#Returns the current autoscale settings per axis as a tuple of two booleans.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setLogscale', @setLogscale,
    'setLogscale([forX,forY]);#Sets log-scale per axis#Expects a tuple of two booleans as parameter.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getLogscale', @getLogscale,
    'getLogscale;#Returns the current log-scale settings per axis as a tuple of two booleans.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setRange', @setPlotRange,
    'setRange([[x0,x1],[y0,y1]]);#Sets the plot-range for the next plot.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getRange', @getPlotRange,
    'getRange;#Returns the plot-range of the last plot as a nested list: [[x0,x1],[y0,y1]]');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setAxisStyle', @setAxisStyle,
    'setAxisStyle([sx,sy]);#Sets the axis style for the next plot. #valid options are:'+
    '#  0; //no tics, no grid#  1; //tics, no gris'+
    '#  2; //no tics, coarse grid#  3; //tics, and coarse grid'+
    '#  6; //no tics, finer grid#  7; //tics and finer grid');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getAxisStyle', @getAxisStyle,
    'getAxisStyle([sx,sy]);#Returns the current axis-style as a tuple of two integers.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'setPreserveAspect', @setPreserveAspect,
    'setPreserveAspect(b:boolean);#Sets or un-sets preservation of aspect ratio for the next plot.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'getPreserveAspect', @getPreserveAspect,
    'getPreserveAspect;#Returns a boolean indicating whether the aspect ratio will be preserverd for the next plot');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'renderToFile', @renderToFile_impl,
    'renderToFile(filename,width,height,[supersampling]);#Renders the current plot to a file.');
  mnh_funcs.registerRule(PLOT_NAMESPACE,'display',@display_imp,
    'display;#Displays the plot as soon as possible, even during evaluation.');

end.


