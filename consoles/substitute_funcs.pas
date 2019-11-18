UNIT substitute_funcs;
INTERFACE

IMPLEMENTATION
USES funcs,mnh_constants,contexts,basicTypes,litVar,recyclers;
FUNCTION returnFalse(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal;
  begin
    result:=newBoolLiteral(false);
  end;

FUNCTION requireGui(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal;
  begin
    context.messages^.logGuiNeeded;
    result:=nil;
  end;

INITIALIZATION
  registerRule(GUI_NAMESPACE ,'anyFormShowing',   @returnFalse,ak_variadic);
  registerRule(GUI_NAMESPACE ,'showDialog',       @requireGui ,ak_variadic);
  registerRule(GUI_NAMESPACE ,'showTable',        @requireGui ,ak_variadic);
  registerRule(GUI_NAMESPACE ,'showVariable',     @requireGui ,ak_variadic);
  registerRule(HTTP_NAMESPACE,'formatHtmlPage',   @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'addAnimationFrame',@requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'addPlot',          @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'clearAnimation',   @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'display',          @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'drawText',         @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'drawTextAbsolute', @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'getOptions',       @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'plot',             @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'plotClosed',       @returnFalse,ak_variadic);
  registerRule(PLOT_NAMESPACE,'postDisplay',      @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'removePlot',       @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'removeText',       @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'renderToFile',     @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'renderToString',   @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'resetOptions',     @requireGui ,ak_variadic);
  registerRule(PLOT_NAMESPACE,'setOptions',       @requireGui ,ak_variadic);
  registerRule(IMIG_NAMESPACE,'calculateThumbnail'      ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'closeImage'              ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'displayImage'            ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'executeWorkflow'         ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'getScreenSize'           ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'imageJpgRawData'         ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'imageSize'               ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'listManipulations'       ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'loadImage'               ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'randomIfs'               ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'renderPlotToCurrentImage',@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'resizeImage'             ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'saveImage'               ,@requireGui,ak_variadic);
  registerRule(IMIG_NAMESPACE,'validateWorkflow'        ,@requireGui,ak_variadic);

end.
