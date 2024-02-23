UNIT substitute_funcs;
INTERFACE

IMPLEMENTATION
USES funcs,mnh_constants,contexts,basicTypes,litVar,recyclers;
FUNCTION returnFalse(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    result:=newBoolLiteral(false);
  end;

FUNCTION requireFullVersion(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    context^.messages^.logGuiNeeded;
    result:=nil;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(GUI_NAMESPACE ,'anyFormShowing',   @returnFalse,ak_variadic);
  builtinFunctionMap.registerRule(GUI_NAMESPACE ,'showDialog',       @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(GUI_NAMESPACE ,'showTable',        @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(GUI_NAMESPACE ,'showVariable',     @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'formatHtmlPage',   @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'addAnimationFrame',@requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'addPlot',          @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'plotRasterImage',  @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'clearAnimation',   @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'display',          @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'drawText',         @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'drawTextAbsolute', @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'getOptions',       @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'plot',             @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'plotClosed',       @returnFalse,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'plotImageSize',    @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'postDisplay',      @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'removePlot',       @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'renderToFile',     @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'renderToString',   @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'resetOptions',     @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'setOptions',       @requireFullVersion ,ak_variadic);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'renderToRawData',  @requireFullVersion ,ak_variadic);
end.
