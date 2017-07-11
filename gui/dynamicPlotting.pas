UNIT dynamicPlotting;
INTERFACE
USES sysutils,
     myGenerics,
     mnh_constants,
     mnh_basicTypes,
     mnh_litVar,mnh_contexts,mnh_funcs;

PROCEDURE initializeDynamicPlotting;
PROCEDURE postMouseMove(CONST newX,newY:double);
PROCEDURE postRescale;
PROCEDURE postPlotClosed;
FUNCTION isPlotInteractive:boolean;
IMPLEMENTATION
TYPE T_requestKind=(rk_none,rk_kill,rk_mouseMove,rk_resize);
     T_request=record
       kind:T_requestKind;
       real0,real1:double;
     end;
CONST NO_REQUEST  :T_request=(kind:rk_none; real0:0; real1:0);
      KILL_REQUEST:T_request=(kind:rk_kill; real0:0; real1:0);
TYPE T_dynamicPlotEvents=array[rk_mouseMove..rk_resize] of P_expressionLiteral;
CONST blankEvents:T_dynamicPlotEvents=(nil,nil);

VAR initialized:boolean=false;
    request:specialize G_safeVar<T_request>;
    dynamicPlotLoopRunning: specialize G_safeVar<boolean>;
    setupCs:TRTLCriticalSection;
    dynPlotContext:P_threadContext;
    dynPlotLocation:T_tokenLocation;
    events:T_dynamicPlotEvents;

PROCEDURE postMouseMove(CONST newX, newY: double);
  VAR r:T_request;
  begin
    if not(initialized) then exit;
    r.kind:=rk_mouseMove;
    r.real0:=newX;
    r.real1:=newY;
    request.value:=r;
  end;

PROCEDURE postRescale;
  CONST r:T_request=(kind:rk_resize; real0:0; real1:0);
  begin
    if not(initialized) then exit;
    request.value:=r;
  end;

PROCEDURE postPlotClosed;
  begin
    if not(initialized) then exit;
    request.value:=KILL_REQUEST;
  end;

FUNCTION isPlotInteractive: boolean;
  begin
    result:=dynamicPlotLoopRunning.value;
  end;

FUNCTION dynamicPlotThread(p:pointer):ptrint;
  VAR processing:T_request;
      rk        :T_requestKind;
      sleepTime :longint=0;

  PROCEDURE evaluate(CONST f:P_expressionLiteral; par:P_listLiteral);
    VAR evaluationResult:P_literal=nil;
    begin
      if f<>nil then begin
        evaluationResult:=f^.evaluate(dynPlotLocation,dynPlotContext,par);
        sleepTime:=0;
      end;
      if par<>nil then disposeLiteral(par);
      if evaluationResult<>nil then disposeLiteral(evaluationResult);
    end;

  begin
    processing:=request.value;
    while (processing.kind<>rk_kill) and (dynPlotContext^.adapters^.noErrors) do begin
      request.value:=NO_REQUEST;
      case processing.kind of
        rk_none: begin
          if sleepTime<100 then inc(sleepTime);
          sleep(sleepTime);
        end;
        rk_mouseMove: evaluate(events[processing.kind],P_listLiteral(newListLiteral(2)^.appendReal(processing.real0)^.appendReal(processing.real1)));
        else          evaluate(events[processing.kind],nil);
      end;
      processing:=request.value;
    end;
    dynPlotContext^.doneEvaluating;
    dispose(dynPlotContext,destroy);
    for rk:=low(events) to high(events) do
      if events[rk]<>nil then disposeLiteral(events[rk]);
    dynamicPlotLoopRunning.value:=false;
    result:=0;
  end;

{$i mnh_func_defines.inc}
FUNCTION dynamicPlot_impl intFuncSignature;
  VAR allOkay:boolean=true;
      newEvents:T_dynamicPlotEvents;

  PROCEDURE applyParameters;
    VAR e:P_expressionLiteral;
        anyEvent:boolean=false;
    begin
      if dynamicPlotLoopRunning.value then begin
        request.value:=KILL_REQUEST;
        while dynamicPlotLoopRunning.value do begin
          ThreadSwitch;
          sleep(1);
        end;
      end;
      dynPlotContext:=context.getNewAsyncContext;
      dynPlotLocation:=tokenLocation;
      events:=newEvents;
      for e in events do if e<>nil then begin
        e^.rereference;
        anyEvent:=true;
      end;
      request.value:=NO_REQUEST;
      if anyEvent then begin
        beginThread(@dynamicPlotThread);
        dynamicPlotLoopRunning.value:=true;
      end;
    end;

  PROCEDURE matchKey(CONST key:string; CONST value:P_expressionLiteral);
    begin
      if not(allOkay) then exit;
      if key='mouseMove' then begin
        if value^.canApplyToNumberOfParameters(2)
        then newEvents[rk_mouseMove]:=value
        else allOkay:=false;
      end else if key='rescale' then begin
        if value^.canApplyToNumberOfParameters(0)
        then newEvents[rk_resize]:=value
        else allOkay:=false;
      end else allOkay:=false;
      if not(allOkay) then context.adapters^.raiseWarning('Dont know what to do with entry ["'+key+'", '+value^.toString(30)+']',tokenLocation);
    end;

  VAR iter:T_arrayOfLiteral;
      pair:P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and ((arg0^.literalType=lt_map) or (arg0^.literalType in C_listTypes+C_setTypes) and (list0^.isKeyValueCollection)) then begin
      iter:=compound0^.iteratableList;
      for pair in iter do
      if (pair^.literalType<>lt_list) or
         (P_listLiteral(pair)^.size<>2) or
         (P_listLiteral(pair)^[0]^.literalType<>lt_string) or
         (P_listLiteral(pair)^[1]^.literalType<>lt_expression) then begin
        disposeLiteral(iter);
        exit(nil);
      end;
      enterCriticalSection(setupCs);
      newEvents:=blankEvents;
      for pair in iter do matchKey(P_stringLiteral(P_listLiteral(pair)^[0])^.value,P_expressionLiteral(P_listLiteral(pair)^[1]));
      disposeLiteral(iter);
      if allOkay then applyParameters;
      result:=newBoolLiteral(allOkay);
      leaveCriticalSection(setupCs);
      while context.adapters^.noErrors and dynamicPlotLoopRunning.value do begin
        sleep(10);
        ThreadSwitch;
      end;
    end;
  end;

PROCEDURE initializeDynamicPlotting;
  begin
    if initialized then exit;
    initialized:=true;
    request.create(NO_REQUEST);
    dynamicPlotLoopRunning.create(false);
    initCriticalSection(setupCs);
    dynPlotContext:=nil;
    events:=blankEvents;

    registerRule(PLOT_NAMESPACE,'dynamicPlot',@dynamicPlot_impl,[se_writingInternal],ak_unary,
      'dynamicPlot(events:map);//Sets up dynamic plotting with the given events#'+
      '//expected map structure (all entries optional):#'+
      '//  [["mouseMove",expression(2)],#'+
      '//   ["rescale"  ,expression(0)]].toMap');
  end;

FINALIZATION
  if initialized then begin
    initialized:=false;
    request.destroy;
    dynamicPlotLoopRunning.destroy;
    doneCriticalSection(setupCs);
  end;
end.
