
TYPE
  //TODO: Maybe refactor this, so that a new plot adapter becomes part of the custom form.
  P_plotConnectorMeta=^T_plotConnectorMeta;
  T_plotConnectorMeta=object(T_guiElementMeta)
    plotConfig:record
      mouseMoved,
      mouseClicked:P_expressionLiteral;
    end;
    plotState:record
      mouseMovedParam:P_listLiteral;
      mouseClickedParam:P_listLiteral;
    end;
    relatedPlotAdapter:P_guiPlotSystem;

    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST plot:P_guiPlotSystem);
    PROCEDURE update; virtual;
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean; virtual;
    FUNCTION getControl:TControl; virtual;
    PROCEDURE plotMouseMoved(CONST realPoint:T_point);
    PROCEDURE plotMouseClicked(CONST realPoint:T_point);
    PROCEDURE plotRescaled(Sender:TObject);
    PROCEDURE connect; virtual;
    PROCEDURE disconnect; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION getName:string; virtual;
  end;

CONSTRUCTOR T_plotConnectorMeta.create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST plot:P_guiPlotSystem);
  VAR tmp:P_literal;
  begin
    inherited create(def,location,context,[dmk_type,dmk_action,dmk_mouseClicked,dmk_mouseMoved]);
    relatedPlotAdapter:=plot;
    if plot=nil then context.raiseError('No plot available for connecting',location);
    with plotConfig do begin
      mouseMoved:=nil;
      mouseClicked:=nil;
    end;
    with plotState do begin
      mouseMovedParam:=nil;
      mouseClickedParam:=nil;
    end;

    tmp:=mapGet(def,key[dmk_mouseMoved]);
    if tmp<>nil then begin
      if (tmp^.literalType=lt_expression)
      then plotConfig.mouseMoved:=P_expressionLiteral(tmp)
      else context.raiseError('mouseMoved is: '+tmp^.typeString+'; must be nullary or binary expression',location);
    end;

    tmp:=mapGet(def,key[dmk_mouseClicked]);
    if tmp<>nil then begin
      if (tmp^.literalType=lt_expression)
      then plotConfig.mouseClicked:=P_expressionLiteral(tmp)
      else context.raiseError('mouseClicked is: '+tmp^.typeString+'; must be nullary or binary expression',location);
    end;

    if plot<>nil then connect;
  end;

PROCEDURE T_plotConnectorMeta.update;
  begin

  end;

FUNCTION T_plotConnectorMeta.evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean;
  VAR tmp:P_literal;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit(false);
    try
      result:=inherited evaluate(location,context,recycler);
      if plotState.mouseMovedParam<>nil then begin
        if plotConfig.mouseMoved^.canApplyToNumberOfParameters(2)
        then tmp:=plotConfig.mouseMoved^.evaluate(location,@context,@recycler,plotState.mouseMovedParam).literal
        else tmp:=plotConfig.mouseMoved^.evaluate(location,@context,@recycler,nil).literal;
        if tmp<>nil then disposeLiteral(tmp);
        disposeLiteral(plotState.mouseMovedParam);
        result:=true;
      end;

      if plotState.mouseClickedParam<>nil then begin
        if plotConfig.mouseClicked^.canApplyToNumberOfParameters(2)
        then tmp:=plotConfig.mouseClicked^.evaluate(location,@context,@recycler,plotState.mouseClickedParam).literal
        else tmp:=plotConfig.mouseClicked^.evaluate(location,@context,@recycler,nil).literal;
        if tmp<>nil then disposeLiteral(tmp);
        disposeLiteral(plotState.mouseClickedParam);
        result:=true;
      end;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

FUNCTION T_plotConnectorMeta.getControl:TControl;
  begin
    result:=nil;
  end;

PROCEDURE T_plotConnectorMeta.plotMouseMoved(CONST realPoint:T_point);
  begin
    if (plotConfig.mouseMoved=nil) or (tryEnterCriticalsection(elementCs)=0) then exit;
    try
      with plotState do begin
        if mouseMovedParam<>nil then disposeLiteral(mouseMovedParam);
        mouseMovedParam:=newListLiteral(2);
        mouseMovedParam^.appendReal(realPoint[0])^.appendReal(realPoint[1]);
      end;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

PROCEDURE T_plotConnectorMeta.plotMouseClicked(CONST realPoint:T_point);
  begin
    if (plotConfig.mouseClicked=nil) or (tryEnterCriticalsection(elementCs)=0) then exit;
    try
      with plotState do begin
        if mouseClickedParam<>nil then disposeLiteral(mouseClickedParam);
        mouseClickedParam:=newListLiteral(2);
        mouseClickedParam^.appendReal(realPoint[0])^.appendReal(realPoint[1]);
      end;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

PROCEDURE T_plotConnectorMeta.plotRescaled(Sender:TObject);
  begin
    postAction(nil);
  end;

PROCEDURE T_plotConnectorMeta.connect;
  VAR plotForm:TplotForm;
  begin
    plotForm:=relatedPlotAdapter^.plotFormForConnecting;
    if (config.action<>nil)
    then plotForm.onPlotRescale:=@plotRescaled
    else plotForm.onPlotRescale:=nil;
    if (plotConfig.mouseClicked<>nil)
    then plotForm.onPlotMouseClick:=@plotMouseClicked
    else plotForm.onPlotMouseClick:=nil;
    if (plotConfig.mouseMoved<>nil)
    then plotForm.onPlotMouseMove:=@plotMouseMoved
    else plotForm.onPlotMouseMove:=nil;
  end;

PROCEDURE T_plotConnectorMeta.disconnect;
  begin
    relatedPlotAdapter^.disconnect;
  end;

FUNCTION T_plotConnectorMeta.getName:string;
  begin
    result:='T_plotConnectorMeta';
  end;

DESTRUCTOR T_plotConnectorMeta.destroy;
  begin
    disconnect;
    inherited destroy;
    with plotConfig do begin
      if mouseMoved  <>nil then disposeLiteral(mouseMoved  );
      if mouseClicked<>nil then disposeLiteral(mouseClicked);
    end;
    with plotState do begin
      if mouseMovedParam  <>nil then disposeLiteral(mouseMovedParam  );
      if mouseClickedParam<>nil then disposeLiteral(mouseClickedParam);
    end;
  end;

