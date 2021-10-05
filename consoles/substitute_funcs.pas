UNIT substitute_funcs;
INTERFACE

IMPLEMENTATION
USES funcs,mnh_constants,contexts,basicTypes,litVar,recyclers;
FUNCTION returnFalse(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    result:=newBoolLiteral(false);
  end;

FUNCTION requireGui(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    context^.messages^.logGuiNeeded;
    result:=nil;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(GUI_NAMESPACE ,'anyFormShowing',   @returnFalse,ak_variadic);
  builtinFunctionMap.registerRule(GUI_NAMESPACE ,'showDialog',       @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(GUI_NAMESPACE ,'showTable',        @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(GUI_NAMESPACE ,'showVariable',     @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'formatHtmlPage',   @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'addAnimationFrame',@requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'addPlot',          @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'plotRasterImage',  @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'clearAnimation',   @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'display',          @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'drawText',         @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'drawTextAbsolute', @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'getOptions',       @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'plot',             @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'plotClosed',       @returnFalse,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'postDisplay',      @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'removePlot',       @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'removeText',       @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'renderToFile',     @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'renderToString',   @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'resetOptions',     @requireGui ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'setOptions',       @requireGui ,ak_variadic);

end.
