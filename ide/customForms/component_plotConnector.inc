
TYPE
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

    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler; CONST plot:P_guiPlotSystem);
    PROCEDURE update; virtual;
    FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean; virtual;
    FUNCTION getControl:TControl; virtual;
    PROCEDURE plotMouseMoved(CONST realPoint:T_point);
    PROCEDURE plotMouseClicked(CONST realPoint:T_point);
    PROCEDURE plotRescaled(Sender:TObject);
    PROCEDURE connect; virtual;
    PROCEDURE disconnect; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION getName:string; virtual;
  end;

CONSTRUCTOR T_plotConnectorMeta.create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler; CONST plot:P_guiPlotSystem);
  VAR tmp:P_literal;
  begin
    inherited create(def,location,context,recycler,[dmk_type,dmk_action,dmk_mouseClicked,dmk_mouseMoved]);
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

    tmp:=mapGet(def,key[dmk_mouseMoved],recycler);
    if tmp<>nil then begin
      if (tmp^.literalType=lt_expression)
      then plotConfig.mouseMoved:=P_expressionLiteral(tmp)
      else context.raiseError('mouseMoved is: '+tmp^.typeString+'; must be nullary or binary expression',location);
    end;

    tmp:=mapGet(def,key[dmk_mouseClicked],recycler);
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

FUNCTION T_plotConnectorMeta.evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean;
  VAR tmp:P_literal;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit(false);
    try
      result:=inherited evaluate(location,context,recycler);
      if plotState.mouseMovedParam<>nil then begin
        if plotConfig.mouseMoved^.canApplyToNumberOfParameters(2)
        then tmp:=plotConfig.mouseMoved^.evaluate(location,context,recycler,plotState.mouseMovedParam).literal
        else tmp:=plotConfig.mouseMoved^.evaluate(location,context,recycler,nil).literal;
        if tmp<>nil then recycler^.disposeLiteral(tmp);
        recycler^.disposeLiteral(plotState.mouseMovedParam);
        result:=true;
      end;

      if plotState.mouseClickedParam<>nil then begin
        if plotConfig.mouseClicked^.canApplyToNumberOfParameters(2)
        then tmp:=plotConfig.mouseClicked^.evaluate(location,context,recycler,plotState.mouseClickedParam).literal
        else tmp:=plotConfig.mouseClicked^.evaluate(location,context,recycler,nil).literal;
        if tmp<>nil then recycler^.disposeLiteral(tmp);
        recycler^.disposeLiteral(plotState.mouseClickedParam);
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
  VAR recycler:P_recycler;
  begin
    if (plotConfig.mouseMoved=nil) or (tryEnterCriticalsection(elementCs)=0) then exit;
    try
      recycler:=newRecycler;
      with plotState do begin
        if mouseMovedParam<>nil then recycler^.disposeLiteral(mouseMovedParam);
        mouseMovedParam:=recycler^.newListLiteral(2);
        mouseMovedParam^.appendReal(recycler,realPoint[0])^.appendReal(recycler,realPoint[1]);
      end;
    finally
      leaveCriticalSection(elementCs);
      freeRecycler(recycler);
    end;
  end;

PROCEDURE T_plotConnectorMeta.plotMouseClicked(CONST realPoint:T_point);
  VAR recycler:P_recycler;
  begin
    if (plotConfig.mouseClicked=nil) or (tryEnterCriticalsection(elementCs)=0) then exit;
    try
      with plotState do begin
        recycler:=newRecycler;
        if mouseClickedParam<>nil then recycler^.disposeLiteral(mouseClickedParam);
        mouseClickedParam:=recycler^.newListLiteral(2);
        mouseClickedParam^.appendReal(recycler,realPoint[0])^.appendReal(recycler,realPoint[1]);
        freeRecycler(recycler);
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
      formWasFreshlyCreated: boolean;
  begin
    plotForm:=relatedPlotAdapter^.plotFormForConnecting(false,formWasFreshlyCreated);
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
  VAR recycler:P_recycler;
  begin
    disconnect;
    inherited destroy;
    recycler:=newRecycler;
    with plotConfig do begin
      if mouseMoved  <>nil then recycler^.disposeLiteral(mouseMoved  );
      if mouseClicked<>nil then recycler^.disposeLiteral(mouseClicked);
    end;
    with plotState do begin
      if mouseMovedParam  <>nil then recycler^.disposeLiteral(mouseMovedParam  );
      if mouseClickedParam<>nil then recycler^.disposeLiteral(mouseClickedParam);
    end;
    freeRecycler(recycler);
  end;

