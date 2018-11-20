UNIT listProcessing;
INTERFACE
USES sysutils,
     math,
     mnh_constants, mnh_basicTypes,
     mnh_settings,
     mnh_litVar,valueStore,mnh_subrules,
     mnh_aggregators,mnh_contexts;
CONST FUTURE_RECYCLER_MAX_SIZE=16;
TYPE
  T_futureLiteralState=(fls_pending,fls_evaluating,fls_done);

  P_futureLiteral=^T_futureLiteral;
  T_futureLiteral=object(T_builtinGeneratorExpression)
    private
      criticalSection:TRTLCriticalSection;
      func:P_expressionLiteral;
      param:P_listLiteral;
      resultValue:P_literal;
      state:T_futureLiteralState;
      isBlocking:boolean;
    public
      CONSTRUCTOR create(CONST func_:P_expressionLiteral; CONST param_:P_listLiteral; CONST loc:T_tokenLocation; CONST blocking:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; {$WARN 5024 OFF}CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      PROCEDURE executeInContext(CONST context:P_threadContext);
  end;

PROCEDURE processListSerial(CONST inputIterator:P_expressionLiteral; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                            CONST eachLocation:T_tokenLocation;
                            VAR context:T_threadContext);
PROCEDURE processListParallel(CONST inputIterator:P_expressionLiteral; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                              CONST eachLocation:T_tokenLocation;
                              VAR context:T_threadContext);
FUNCTION processMapSerial(CONST inputIterator,expr:P_expressionLiteral;
                          CONST mapLocation:T_tokenLocation;
                          VAR context:T_threadContext):P_listLiteral;
FUNCTION processMapParallel(CONST inputIterator,expr:P_expressionLiteral;
                            CONST mapLocation:T_tokenLocation;
                            VAR context:T_threadContext; CONST iteratorSource:P_literal):P_listLiteral;
PROCEDURE processFilterParallel(CONST inputIterator,filterExpression:P_expressionLiteral;
                                CONST filterLocation:T_tokenLocation;
                                VAR context:T_threadContext;
                                CONST output:P_compoundLiteral; CONST iteratorSource:P_literal);
PROCEDURE aggregate(CONST inputIterator:P_expressionLiteral; CONST aggregator:P_aggregator; CONST location:T_tokenLocation; VAR context:T_threadContext);
PROCEDURE enqueueFutureTask(CONST future:P_futureLiteral; VAR context:T_threadContext);

VAR newIterator:FUNCTION (CONST input:P_literal):P_expressionLiteral;
IMPLEMENTATION
TYPE
  P_eachTask=^T_eachTask;
  T_eachTask=object(T_queueTask)
    eachPayload:record
      eachIndex:longint;
      eachRule:P_expressionLiteral;
      eachParameter:P_literal;
    end;
    nextToAggregate:P_eachTask;
    evaluationResult:T_evaluationResult;
    CONSTRUCTOR createEachTask();
    PROCEDURE dropEachParameter;
    PROCEDURE defineAndEnqueue(CONST taskEnv:P_threadContext; CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal);
    PROCEDURE evaluate; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION canGetResult:boolean;
  end;

  P_mapTask=^T_mapTask;
  T_mapTask=object(T_queueTask)
    mapPayload:record
      mapRule:P_expressionLiteral;
      mapParameter:T_arrayOfLiteral;
    end;
    mapResult:T_arrayOfLiteral;
    nextToAggregate:P_mapTask;
    CONSTRUCTOR createMapTask(CONST expr:P_expressionLiteral);
    PROCEDURE defineAndEnqueue(CONST taskEnv:P_threadContext; CONST x:T_arrayOfLiteral);
    PROCEDURE evaluate; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION canGetResult:boolean;
  end;

  P_filterTask=^T_filterTask;
  T_filterTask=object(T_mapTask)
    CONSTRUCTOR createFilterTask(CONST expr:P_expressionLiteral);
    PROCEDURE evaluate; virtual;
  end;

  P_futureTask=^T_futureTask;
  T_futureTask=object(T_queueTask)
    payload:P_futureLiteral;
    CONSTRUCTOR create(CONST future:P_futureLiteral);
    PROCEDURE   evaluate; virtual;
  end;

PROCEDURE processListSerial(CONST inputIterator:P_expressionLiteral; CONST rulesList: T_expressionList; CONST aggregator: P_aggregator; CONST eachLocation: T_tokenLocation; VAR context: T_threadContext);
  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      indexLiteral:P_abstractIntLiteral;
      x:P_literal;
      proceed:boolean=true;
  begin
    x:=inputIterator^.evaluateToLiteral(eachLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin
      indexLiteral:=newIntLiteral(eachIndex);
      for rule in rulesList do if proceed then begin
        aggregator^.addToAggregation(
          rule^.evaluateToLiteral(eachLocation,@context,x,indexLiteral),
          true,
          eachLocation,
          @context);
        proceed:=context.messages.continueEvaluation and not(aggregator^.earlyAbort);
      end;
      inc(eachIndex);
      disposeLiteral(indexLiteral);
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(eachLocation,@context).literal;
    end;
    if x<>nil then disposeLiteral(x);
  end;

PROCEDURE processListParallel(CONST inputIterator:P_expressionLiteral;
  CONST rulesList: T_expressionList; CONST aggregator: P_aggregator;
  CONST eachLocation: T_tokenLocation; VAR context: T_threadContext);

  VAR firstToAggregate:P_eachTask=nil;
      lastToAggregate:P_eachTask=nil;
      recycling:record
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_eachTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
    VAR toAggregate:P_eachTask;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        aggregator^.addToAggregation(toAggregate^.evaluationResult,true,eachLocation,@context);
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  PROCEDURE createTask(CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal); {$ifndef debugMode} inline; {$endif}
    VAR task:P_eachTask;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        task:=dat[fill];
      end else new(task,createEachTask());
      if firstToAggregate=nil then begin
        firstToAggregate:=task;
        lastToAggregate:=task;
      end else begin
        lastToAggregate^.nextToAggregate:=task;
        lastToAggregate:=task;
      end;
      task^.defineAndEnqueue(context.getFutureEnvironment,expr,idx,x);
    end;

  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      x:P_literal;
      proceed:boolean=true;
  begin
    recycling.fill:=0;
    x:=inputIterator^.evaluateToLiteral(eachLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin
      for rule in rulesList do if proceed then begin
        createTask(rule,eachIndex,x);
        if context.getGlobals^.taskQueue.getQueuedCount>settings.cpuCount*2 then begin
          if not(canAggregate) then context.getGlobals^.taskQueue.activeDeqeue;
          proceed:=proceed and not(aggregator^.earlyAbort);
        end;
        proceed:=proceed and context.messages.continueEvaluation;
      end;
      inc(eachIndex);
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(eachLocation,@context).literal;
    end;
    if x<>nil then disposeLiteral(x);

    while firstToAggregate<>nil do
    if not(canAggregate)
    then context.getGlobals^.taskQueue.activeDeqeue;
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
  end;

FUNCTION processMapSerial(CONST inputIterator,expr:P_expressionLiteral;
                          CONST mapLocation:T_tokenLocation;
                          VAR context:T_threadContext):P_listLiteral;
  VAR x:P_literal;
      isExpressionNullary:boolean;
  begin
    isExpressionNullary:=not(expr^.canApplyToNumberOfParameters(1));
    result:=newListLiteral();
    x:=inputIterator^.evaluateToLiteral(mapLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and (context.messages.continueEvaluation) do begin
      if isExpressionNullary
      then result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context  ).literal,false)
      else result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context,x).literal,false);
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(mapLocation,@context).literal;
    end;
    if x<>nil then disposeLiteral(x);
  end;

FUNCTION processMapParallel(CONST inputIterator,expr:P_expressionLiteral;
                            CONST mapLocation:T_tokenLocation;
                            VAR context:T_threadContext; CONST iteratorSource:P_literal):P_listLiteral;

  VAR firstToAggregate:P_mapTask=nil;
      lastToAggregate:P_mapTask=nil;
      resultLiteral:P_listLiteral;
  VAR recycling:record
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_mapTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
    VAR toAggregate:P_mapTask;
        resultElement:P_literal;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        for resultElement in toAggregate^.mapResult do
        resultLiteral^.append(resultElement,false);
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  PROCEDURE createTask(CONST x:T_arrayOfLiteral); {$ifndef debugMode} inline; {$endif}
    VAR task:P_mapTask;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        task:=dat[fill];
      end else new(task,createMapTask(expr));
      task^.defineAndEnqueue(context.getFutureEnvironment,x);
      if firstToAggregate=nil then begin
        firstToAggregate:=task;
        lastToAggregate:=task;
      end else begin
        lastToAggregate^.nextToAggregate:=task;
        lastToAggregate:=task;
      end;
    end;

  VAR nextToEnqueue:T_arrayOfLiteral;
      enqueueFill:longint=0;
      elementsProcessed:longint=0;
  FUNCTION enqueueValue(CONST x:P_literal):boolean;
    begin
      nextToEnqueue[enqueueFill]:=x;
      inc(enqueueFill);
      inc(elementsProcessed);
      if enqueueFill>=length(nextToEnqueue) then begin
        createTask(nextToEnqueue);
        setLength(nextToEnqueue,max(length(nextToEnqueue),ceil(elementsProcessed/(2*settings.cpuCount))));
        enqueueFill:=0;
        if   context.getGlobals^.taskQueue.getQueuedCount>settings.cpuCount*2
        then context.getGlobals^.taskQueue.activeDeqeue;
        result:=true;
      end else result:=false;
    end;

  VAR x:P_literal;
      isExpressionNullary:boolean;

  begin
    isExpressionNullary:=not(expr^.canApplyToNumberOfParameters(1));
    resultLiteral:=newListLiteral();
    recycling.fill:=0;

    if iteratorSource^.literalType in C_compoundTypes
    then setLength(nextToEnqueue,ceil(P_compoundLiteral(iteratorSource)^.size/settings.cpuCount))
    else setLength(nextToEnqueue,1);

    x:=inputIterator^.evaluateToLiteral(mapLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and (context.messages.continueEvaluation) do begin
      if isExpressionNullary
      then begin if enqueueValue(nil) then canAggregate; end
      else begin if enqueueValue(x  ) then canAggregate; end;
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(mapLocation,@context).literal;
    end;
    setLength(nextToEnqueue,enqueueFill);
    createTask(nextToEnqueue);
    if x<>nil then disposeLiteral(x);

    while firstToAggregate<>nil do if not(canAggregate) then context.getGlobals^.taskQueue.activeDeqeue;
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
    result:=resultLiteral;
  end;

PROCEDURE processFilterParallel(CONST inputIterator,filterExpression:P_expressionLiteral;
                                CONST filterLocation:T_tokenLocation;
                                VAR context:T_threadContext;
                                CONST output:P_compoundLiteral; CONST iteratorSource:P_literal);
  VAR firstToAggregate:P_filterTask=nil;
      lastToAggregate:P_filterTask=nil;
      recycling:record
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_filterTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
    VAR toAggregate:P_filterTask;
        resultElement:P_literal;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=P_filterTask(toAggregate^.nextToAggregate);
        if output^.literalType in C_mapTypes then begin
          for resultElement in toAggregate^.mapResult do
            P_mapLiteral(output)^.put(P_listLiteral(resultElement)^.value[0],P_listLiteral(resultElement)^.value[1],true);
        end else begin
          for resultElement in toAggregate^.mapResult do
            P_collectionLiteral(output)^.append(resultElement,true);
        end;
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  PROCEDURE createTask(CONST x:T_arrayOfLiteral); {$ifndef debugMode} inline; {$endif}
    VAR task:P_filterTask;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        task:=dat[fill];
      end else new(task,createFilterTask(filterExpression));
      task^.defineAndEnqueue(context.getFutureEnvironment,x);
      if firstToAggregate=nil then begin
        firstToAggregate:=task;
        lastToAggregate:=task;
      end else begin
        lastToAggregate^.nextToAggregate:=task;
        lastToAggregate:=task;
      end;
    end;

  VAR nextToEnqueue:T_arrayOfLiteral;
      enqueueFill:longint=0;
      elementsProcessed:longint=0;
  FUNCTION enqueueValue(CONST x:P_literal):boolean;
    begin
      nextToEnqueue[enqueueFill]:=x;
      inc(enqueueFill);
      inc(elementsProcessed);
      if enqueueFill>=length(nextToEnqueue) then begin
        createTask(nextToEnqueue);
        setLength(nextToEnqueue,max(length(nextToEnqueue),ceil(elementsProcessed/(2*settings.cpuCount))));
        enqueueFill:=0;
        if   context.getGlobals^.taskQueue.getQueuedCount>settings.cpuCount*2
        then context.getGlobals^.taskQueue.activeDeqeue;
        result:=true;
      end else result:=false;
    end;

  VAR x:P_literal;
  begin
    recycling.fill:=0;

    if iteratorSource^.literalType in C_compoundTypes
    then setLength(nextToEnqueue,ceil(P_compoundLiteral(iteratorSource)^.size/settings.cpuCount))
    else setLength(nextToEnqueue,1);

    x:=inputIterator^.evaluateToLiteral(filterLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and (context.messages.continueEvaluation) do begin
      if enqueueValue(x) then canAggregate;
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(filterLocation,@context).literal;
    end;
    if x<>nil then disposeLiteral(x);
    setLength(nextToEnqueue,enqueueFill);
    createTask(nextToEnqueue);
    while firstToAggregate<>nil do if not(canAggregate) then context.getGlobals^.taskQueue.activeDeqeue;
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
  end;

PROCEDURE aggregate(CONST inputIterator: P_expressionLiteral; CONST aggregator: P_aggregator; CONST location: T_tokenLocation; VAR context: T_threadContext);
  VAR x:T_evaluationResult;
  begin
    x:=inputIterator^.evaluateToLiteral(location,@context);
    while (x.literal<>nil) and (x.literal^.literalType<>lt_void) and context.messages.continueEvaluation and not(aggregator^.earlyAbort) do begin
      aggregator^.addToAggregation(
        x,
        false,
        location,
        @context);
      disposeLiteral(x.literal);
      x:=inputIterator^.evaluateToLiteral(location,@context);
    end;
    if x.literal<>nil then disposeLiteral(x.literal);
  end;

PROCEDURE enqueueFutureTask(CONST future:P_futureLiteral; VAR context:T_threadContext);
  VAR task:P_futureTask;
  begin
    new(task,create(future));
    task^.defineAndEnqueue(context.getFutureEnvironment);
  end;

CONSTRUCTOR T_futureTask.create(CONST future: P_futureLiteral);
  begin
    inherited create(true);
    payload:=future;
  end;

PROCEDURE T_futureTask.evaluate;
  begin
    context^.beginEvaluation;
    try
      payload^.executeInContext(context);
      disposeLiteral(payload);
    finally
      context^.finalizeTaskAndDetachFromParent;
    end;
  end;

CONSTRUCTOR T_filterTask.createFilterTask(CONST expr: P_expressionLiteral);
  begin
    createMapTask(expr);
  end;

PROCEDURE T_filterTask.evaluate;
  VAR k:longint;
  begin
    context^.beginEvaluation;
    try
      if context^.messages.continueEvaluation then with mapPayload do begin
        setLength(mapResult,length(mapParameter));
        for k:=0 to length(mapParameter)-1 do begin
          if mapRule^.evaluateToBoolean(mapRule^.getLocation,context,true,mapParameter[k])
          then mapResult[k]:=mapParameter[k]
          else mapResult[k]:=nil;
        end;
      end;
    finally
      context^.finalizeTaskAndDetachFromParent;
    end;
  end;

CONSTRUCTOR T_mapTask.createMapTask(CONST expr: P_expressionLiteral);
  begin
    create(false);
    mapPayload.mapRule:=expr;
    mapPayload.mapParameter:=nil;
  end;

PROCEDURE T_mapTask.defineAndEnqueue(CONST taskEnv:P_threadContext; CONST x:T_arrayOfLiteral);
  VAR k:longint;
  begin
    if mapPayload.mapParameter<>nil then disposeLiteral(mapPayload.mapParameter);
    mapPayload.mapParameter:=x;
    for k:=0 to length(mapPayload.mapParameter)-1 do if mapPayload.mapParameter[k]<>nil then mapPayload.mapParameter[k]^.rereference;
    nextToAggregate:=nil;
    inherited defineAndEnqueue(taskEnv);
  end;

PROCEDURE T_mapTask.evaluate;
  VAR k:longint;
  begin
    context^.beginEvaluation;
    try
      if context^.messages.continueEvaluation then with mapPayload do begin
        setLength(mapResult,length(mapParameter));
        for k:=0 to length(mapParameter)-1 do
        mapResult[k]:=mapRule^.evaluateToLiteral(mapRule^.getLocation,context,mapParameter[k]).literal;
      end;
    finally
      context^.finalizeTaskAndDetachFromParent;
    end;
  end;

DESTRUCTOR T_mapTask.destroy;
  begin
    if mapPayload.mapParameter<>nil then disposeLiteral(mapPayload.mapParameter);
    inherited destroy;
  end;

CONSTRUCTOR T_eachTask.createEachTask();
  begin
    create(false);
  end;

PROCEDURE T_eachTask.dropEachParameter;
  begin
    enterCriticalSection(taskCs);
    if eachPayload.eachParameter<>nil then disposeLiteral(eachPayload.eachParameter);
    leaveCriticalSection(taskCs);
  end;

PROCEDURE T_eachTask.defineAndEnqueue(CONST taskEnv:P_threadContext; CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal);
  begin
    enterCriticalSection(taskCs);
    with eachPayload do begin
      eachIndex       :=idx;
      eachRule        :=expr;
      if x=nil
      then eachParameter:=nil
      else eachParameter:=x^.rereferenced;
      evaluationResult:=NIL_EVAL_RESULT;
    end;
    nextToAggregate:=nil;
    inherited defineAndEnqueue(taskEnv);
    leaveCriticalSection(taskCs);
  end;

PROCEDURE T_eachTask.evaluate;
  VAR indexLiteral:P_abstractIntLiteral;
  begin
    enterCriticalSection(taskCs);
    context^.beginEvaluation;
    leaveCriticalSection(taskCs);
    try
      if context^.messages.continueEvaluation then with eachPayload do begin
        indexLiteral:=newIntLiteral(eachIndex);
        evaluationResult:=eachRule^.evaluateToLiteral(eachRule^.getLocation,context,eachParameter,indexLiteral);
        disposeLiteral(indexLiteral);
      end;
    finally
      enterCriticalSection(taskCs);
      dropEachParameter;
      context^.finalizeTaskAndDetachFromParent;
      leaveCriticalSection(taskCs);
    end;
  end;

DESTRUCTOR T_eachTask.destroy;
  begin
    enterCriticalSection(taskCs);
    dropEachParameter;
    leaveCriticalSection(taskCs);
    inherited destroy;
  end;

FUNCTION T_eachTask.canGetResult:boolean;
  begin
    result:=context^.state=fts_finished;
  end;

FUNCTION T_mapTask.canGetResult:boolean;
  begin
    result:=context^.state=fts_finished;
  end;

CONSTRUCTOR T_futureLiteral.create(CONST func_:P_expressionLiteral; CONST param_:P_listLiteral; CONST loc:T_tokenLocation; CONST blocking:boolean);
  begin
    inherited create(loc,et_builtinFuture);
    initCriticalSection(criticalSection);
    isBlocking:=blocking;
    func :=func_;                      func ^.rereference;
    param:=param_;  if param<>nil then param^.rereference;
    resultValue:=nil;
    state:=fls_pending;
  end;

DESTRUCTOR T_futureLiteral.destroy;
  begin
    enterCriticalSection(criticalSection);
    while state=fls_evaluating do begin
      leaveCriticalSection(criticalSection);
      ThreadSwitch;
      sleep(1);
      enterCriticalSection(criticalSection);
    end;
    disposeLiteral(func);
    if param<>nil then disposeLiteral(param);
    if resultValue<>nil then disposeLiteral(resultValue);
    leaveCriticalSection(criticalSection);
    doneCriticalSection(criticalSection);
  end;

FUNCTION T_futureLiteral.toString(CONST lengthLimit:longint=maxLongint):string;
  VAR remaining:longint;
  begin
    if isBlocking then result:='future(' else result:='async(';
    remaining:=lengthLimit-length(result)-1;
    result:=result+func^.toString(remaining);
    remaining:=lengthLimit-length(result)-1;
    result:=result+toParameterListString(param,true,remaining)+')';
  end;

FUNCTION T_futureLiteral.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  begin
    enterCriticalSection(criticalSection);
    if isBlocking then begin
      if state=fls_pending then executeInContext(context)
      else while state<>fls_done do begin
        leaveCriticalSection(criticalSection);
        ThreadSwitch;
        sleep(1);
        enterCriticalSection(criticalSection);
      end;
    end;
    result.triggeredByReturn:=false;
    if resultValue=nil then result.literal:=newVoidLiteral
                       else result.literal:=resultValue^.rereferenced;
    leaveCriticalSection(criticalSection);
  end;

PROCEDURE T_futureLiteral.executeInContext(CONST context:P_threadContext);
  begin
    enterCriticalSection(criticalSection);
    if state=fls_pending then begin
      state:=fls_evaluating;
      leaveCriticalSection(criticalSection);
    end else begin
      leaveCriticalSection(criticalSection);
      exit;
    end;

    resultValue:=func^.evaluate(getLocation,context,param).literal;
    if (resultValue=nil) and (context^.messages.continueEvaluation)
    then context^.raiseCannotApplyError('future/async payload '+func^.toString(20),param,getLocation);

    enterCriticalSection(criticalSection);
    state:=fls_done;
    leaveCriticalSection(criticalSection);
  end;

end.

