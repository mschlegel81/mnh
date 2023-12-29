UNIT listProcessing;
INTERFACE
USES sysutils,
     mnh_constants, basicTypes,
     out_adapters,
     mnh_settings,
     litVar,subrules,
     recyclers,
     aggregators,contexts;
CONST FUTURE_RECYCLER_MAX_SIZE=64;
TYPE
  T_futureLiteralState=(fls_pending,fls_evaluating,fls_done);

  P_chainTask=^T_chainTask;
  T_chainTask=object(T_queueTask)
    protected
      isCancelled:boolean;
    public
      nextToAggregate:P_chainTask;
      CONSTRUCTOR createChainTask(CONST taskEnv:P_context);
      PROCEDURE cancelAllInAggregationChain;
      FUNCTION canGetResult:boolean;
  end;

  P_mapTask=^T_mapTask;
  T_mapTask=object(T_chainTask)
    mapPayload:record
      mapFunc:P_expressionLiteral;
      mapParameter:P_literal;
      location:T_tokenLocation;
    end;
    mapTaskResult:P_literal;
    CONSTRUCTOR createMapTask(CONST taskEnv:P_context; CONST func:P_expressionLiteral);
    FUNCTION define(CONST x:P_literal; CONST mapLocation:T_tokenLocation):P_mapTask;
    PROCEDURE evaluate(CONST recycler:P_recycler); virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_filterTask=^T_filterTask;
  T_filterTask=object(T_chainTask)
    filterPayload:record
      filterFunc:P_expressionLiteral;
      filterParameter:P_literal;
      location:T_tokenLocation;
    end;
    mapTaskResult:P_literal;
    CONSTRUCTOR createFilterTask(CONST taskEnv:P_context; CONST func:P_expressionLiteral);
    FUNCTION define(CONST x:P_literal; CONST filterLocation:T_tokenLocation):P_filterTask;
    PROCEDURE evaluate(CONST recycler:P_recycler); virtual;
    DESTRUCTOR destroy; virtual;
  end;

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
      PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: P_literalRecycler; CONST parameters: P_listLiteral=nil): T_evaluationResult; virtual;
      PROCEDURE executeInContext(CONST context:P_context; CONST recycler:P_recycler);
      PROPERTY isFuture:boolean read isBlocking;
      FUNCTION isDone:boolean;
      FUNCTION getBultinGeneratorType: T_builtinGeneratorType; virtual;
  end;

PROCEDURE processListSerial  (CONST input:P_literal; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                              CONST eachLocation:T_tokenLocation;
                              CONST context:P_context; CONST recycler:P_recycler);
PROCEDURE processListParallel(CONST input:P_literal; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                              CONST eachLocation:T_tokenLocation;
                              CONST context:P_context; CONST recycler:P_recycler);
FUNCTION processMapSerial(CONST input:P_compoundLiteral; CONST expr:P_expressionLiteral;
                          CONST mapLocation:T_tokenLocation;
                          CONST context:P_context; CONST recycler:P_recycler):P_listLiteral;
FUNCTION processFilterSerial  (CONST input:P_compoundLiteral; CONST filterExpression:P_expressionLiteral;
                               CONST filterLocation:T_tokenLocation;
                               CONST context:P_context; CONST recycler:P_recycler):P_compoundLiteral;
PROCEDURE aggregate(CONST input:P_literal; CONST aggregator:P_aggregator; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
PROCEDURE enqueueFutureTask(CONST future:P_futureLiteral; CONST context:P_context; CONST recycler:P_recycler);

VAR newIterator:FUNCTION (CONST literalRecycler:P_literalRecycler; CONST input:P_literal; CONST location:T_tokenLocation):P_expressionLiteral;
IMPLEMENTATION
USES mySys,tokenArray;

TYPE
  T_eachPayload=record
    eachIndex:longint;
    eachRule:P_expressionLiteral;
    eachParameter:P_literal;
  end;

  P_eachTask=^T_eachTask;
  T_eachTask=object(T_chainTask)
    payloads:T_eachPayload;
    evaluationResult:T_evaluationResult;
    CONSTRUCTOR createEachTask(CONST taskEnv:P_context);
    FUNCTION define(CONST payload_:T_eachPayload):P_eachTask;
    PROCEDURE evaluate(CONST recycler:P_recycler); virtual;
  end;

  P_futureTask=^T_futureTask;
  T_futureTask=object(T_queueTask)
    payload:P_futureLiteral;
    CONSTRUCTOR create(CONST taskEnv:P_context; CONST future:P_futureLiteral);
    PROCEDURE   evaluate(CONST recycler:P_recycler); virtual;
  end;

PROCEDURE processListSerial(CONST input:P_literal; CONST rulesList: T_expressionList; CONST aggregator: P_aggregator; CONST eachLocation: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
{$MACRO ON}
{$define processX:=begin
  indexLiteral:=recycler^.newIntLiteral(eachIndex);
  for rule in rulesList do if proceed then begin
    addToAggregator(
      evaluteExpression(rule,eachLocation,context,recycler,x,indexLiteral),
      true,
      eachLocation,
      context,recycler);
    proceed:=context^.continueEvaluation and not(aggregator^.earlyAbort);
  end;
  inc(eachIndex);
  recycler^.disposeLiteral(indexLiteral);
end}

  VAR addToAggregator: PROCEDURE (er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler) of object;
      eachIndex:longint=0;
      indexLiteral:P_abstractIntLiteral;
      iter:T_arrayOfLiteral;
      x:P_literal;
      proceed:boolean=true;
      rule:P_expressionLiteral;
  begin
    {$ifdef fullVersion}
    if tco_stackTrace in context^.threadOptions then context^.callStackPush(eachLocation,'each',stepForward(eachLocation,5),nil);
    {$endif}

    addToAggregator:=@aggregator^.addToAggregation;
    if (input^.literalType=lt_expression) and (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then begin

      x:=P_expressionLiteral(input)^.evaluate(eachLocation,context,recycler,nil).literal;
      while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin
        processX;
        recycler^.disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluate(eachLocation,context,recycler,nil).literal;
      end;
      if x<>nil then recycler^.disposeLiteral(x);
    end else if input^.literalType in C_compoundTypes then begin
      iter:=P_compoundLiteral(input)^.forcedIteratableList(recycler);
      for x in iter do if proceed then processX;
      recycler^.disposeLiterals(iter);
    end else begin
      x:=input;
      processX;
    end;
    {$ifdef fullVersion}
    if tco_stackTrace in context^.threadOptions
    then context^.callStackPop(nil);
    {$endif}
  end;

PROCEDURE processListParallel(CONST input:P_literal; CONST rulesList: T_expressionList; CONST aggregator: P_aggregator; CONST eachLocation: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  VAR taskChain:T_taskChain;
      firstToAggregate:P_chainTask=nil;
      lastToAggregate:P_chainTask=nil;
      addToAggregator: PROCEDURE (er:T_evaluationResult; CONST doDispose:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler) of object;
      myQueuedTasks:longint=0;
      recycling:record
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_chainTask;
        fill:longint;
      end;

  FUNCTION canAggregate:longint; {$ifndef debugMode} inline; {$endif}
    VAR toAggregate:P_chainTask;
    begin
      result:=0;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        assert(firstToAggregate^.getContext<>nil);
        assert(firstToAggregate^.getContext^.valueScope=nil,'valueScope must be nil at this point');
        inc(result);
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        dec(myQueuedTasks);
        addToAggregator(P_eachTask(toAggregate)^.evaluationResult,true,eachLocation,context,recycler);
        with recycling do if fill<length(dat) then begin
          toAggregate^.nextToAggregate:=nil;
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR processed:longint=0;
      toQueueLimit:longint;
  PROCEDURE createTask(CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal); {$ifndef debugMode} inline; {$endif}
    VAR nextToEnqueue:T_eachPayload;
        newTask:P_eachTask;
    begin
      with nextToEnqueue do begin
        eachIndex    :=idx;
        eachRule     :=expr;
        eachParameter:=x^.rereferenced;
      end;
      inc(processed);
      with recycling do if fill>0 then begin
        dec(fill);
        newTask:=P_eachTask(dat[fill]);
        newTask^.reattach(recycler);
      end else new(newTask,createEachTask(context^.getFutureEnvironment(recycler)));
      if firstToAggregate=nil
      then firstToAggregate:=newTask
      else lastToAggregate^.nextToAggregate:=newTask;
      lastToAggregate:=newTask;
      if taskChain.enqueueOrExecute(newTask^.define(nextToEnqueue),recycler) then begin
        myQueuedTasks+=taskChain.handDownThreshold;
        canAggregate;
        taskChain     .handDownThreshold:=toQueueLimit-myQueuedTasks;;
        if   taskChain.handDownThreshold> toQueueLimit
        then taskChain.handDownThreshold:=toQueueLimit;
      end;
    end;

  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      x:P_literal;
      iter:T_arrayOfLiteral;
      proceed:boolean=true;

{$define processX:=
  begin
  for rule in rulesList do if proceed then begin
    createTask(rule,eachIndex,x);
    proceed:=proceed and not(aggregator^.earlyAbort)
                     and context^.messages^.continueEvaluation;
  end;
  inc(eachIndex);
end}

  begin
    {$ifdef fullVersion}
    if tco_stackTrace in context^.threadOptions then context^.callStackPush(eachLocation,'pEach',stepForward(eachLocation,6),nil);
    {$endif}
    addToAggregator:=@aggregator^.addToAggregation;
    recycling.fill:=0;
    toQueueLimit:=TASKS_TO_QUEUE_PER_CPU*settings.cpuCount;
    if toQueueLimit<4 then toQueueLimit:=4;
    taskChain.create(toQueueLimit,context^);
    processed:=0;
    if (input^.literalType=lt_expression) and (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then begin
      x:=P_expressionLiteral(input)^.evaluate(eachLocation,context,recycler,nil).literal;
      while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin
        processX;
        recycler^.disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluate(eachLocation,context,recycler,nil).literal;
      end;
      if x<>nil then recycler^.disposeLiteral(x);
    end else if input^.literalType in C_compoundTypes then begin
      iter:=P_compoundLiteral(input)^.forcedIteratableList(recycler);
      for x in iter do if proceed then processX;
      recycler^.disposeLiterals(iter);
    end else begin
      x:=input;
      processX;
    end;
    taskChain.flush;
    taskChain.destroy;

    if not(proceed) and (firstToAggregate<>nil) then firstToAggregate^.cancelAllInAggregationChain;
    while (firstToAggregate<>nil) and context^.getGlobals^.taskQueue.activeDeqeue(recycler) do canAggregate;
    while (firstToAggregate<>nil)                                                           do canAggregate;
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
    {$ifdef fullVersion}
    if tco_stackTrace in context^.threadOptions
    then context^.callStackPop(nil);
    {$endif}
  end;

FUNCTION processMapSerial(CONST input:P_compoundLiteral; CONST expr:P_expressionLiteral;
                          CONST mapLocation:T_tokenLocation;
                          CONST context:P_context; CONST recycler:P_recycler):P_listLiteral;
  VAR x:P_literal;
      iter:T_arrayOfLiteral;
      isExpressionNullary:boolean;
  begin
    isExpressionNullary:=expr^.arity.maxPatternLength=0;
    result:=recycler^.newListLiteral();
    iter:=input^.forcedIteratableList(recycler);
    if isExpressionNullary
    then begin for x in iter do if (context^.continueEvaluation) then result^.append(recycler,expr^.evaluate(           mapLocation,context,recycler,nil).literal,false); end
    else begin for x in iter do if (context^.continueEvaluation) then result^.append(recycler,evaluteExpressionMap(expr,mapLocation,context,recycler,x  ).literal,false); end;
    recycler^.disposeLiterals(iter);
  end;

FUNCTION processFilterSerial  (CONST input:P_compoundLiteral; CONST filterExpression:P_expressionLiteral;
                               CONST filterLocation:T_tokenLocation;
                               CONST context:P_context; CONST recycler:P_recycler):P_compoundLiteral;
  VAR iter:T_arrayOfLiteral;
      x:P_literal;
  begin
    case input^.literalType of
      lt_emptyList,lt_emptySet,lt_emptyMap                                       : exit(P_compoundLiteral(input^.rereferenced));
      lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList: result:=recycler^.newListLiteral;
      lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet : result:=recycler^.newSetLiteral(0);
      else result:=recycler^.newListLiteral;
    end;
    iter:=input^.forcedIteratableList(recycler);
    for x in iter do if context^.continueEvaluation and evaluteExpressionFilter(filterExpression,filterLocation,context,recycler,x)
                     then P_collectionLiteral(result)^.append(recycler,x,true);
    recycler^.disposeLiterals(iter);
    if input^.literalType=lt_map then begin
      x:=result;
      result:=P_listLiteral(result)^.toMap(recycler,filterLocation,context);
      recycler^.disposeLiteral(x);
    end;
  end;

PROCEDURE aggregate(CONST input:P_literal; CONST aggregator: P_aggregator; CONST location: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler);
  VAR x:T_evaluationResult;
      iter:T_arrayOfLiteral;
      aggregate:F_addToAggregationCall;
  begin
    {$ifdef fullVersion}
    if tco_stackTrace in context^.threadOptions then context^.callStackPush(location,'agg',stepForward(location,4),nil);
    {$endif}
    aggregate:=@aggregator^.addToAggregation;
    if (input^.literalType=lt_expression) and (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then begin
      x:=P_expressionLiteral(input)^.evaluate(location,context,recycler,nil);
      while (x.literal<>nil) and (x.literal^.literalType<>lt_void) and context^.messages^.continueEvaluation and not(aggregator^.earlyAbort) do begin
        aggregate(x,false,location,context,recycler);
        recycler^.disposeLiteral(x.literal);
        x:=P_expressionLiteral(input)^.evaluate(location,context,recycler,nil);
      end;
      if x.literal<>nil then recycler^.disposeLiteral(x.literal);
    end else if input^.literalType in C_compoundTypes then begin
      iter:=P_compoundLiteral(input)^.forcedIteratableList(recycler);
      x.reasonForStop:=rr_ok;
      for x.literal in iter do if (x.literal^.literalType<>lt_void) and context^.messages^.continueEvaluation and not(aggregator^.earlyAbort) then
        aggregate(x,false,location,context,recycler);
      recycler^.disposeLiterals(iter);
    end else begin
      x.reasonForStop:=rr_ok;
      x.literal:=input;
      aggregate(x,false,location,context,recycler);
    end;
    {$ifdef fullVersion}
    if tco_stackTrace in context^.threadOptions
    then context^.callStackPop(nil);
    {$endif}
  end;

PROCEDURE enqueueFutureTask(CONST future:P_futureLiteral; CONST context:P_context; CONST recycler:P_recycler);
  VAR task:P_futureTask;
  begin
    new(task,create(context^.getFutureEnvironment(recycler),future));
    task^.defineAndEnqueueOrEvaluate(recycler);
  end;

CONSTRUCTOR T_futureTask.create(CONST taskEnv:P_context; CONST future: P_futureLiteral);
  begin
    inherited create(true,taskEnv);
    payload:=future;
  end;

PROCEDURE T_futureTask.evaluate(CONST recycler:P_recycler);
  begin
    context^.beginEvaluation;
    payload^.executeInContext(context,recycler);
    recycler^.disposeLiteral(payload);
    context^.finalizeTaskAndDetachFromParent(recycler);
  end;

CONSTRUCTOR T_mapTask.createMapTask(CONST taskEnv:P_context; CONST func:P_expressionLiteral);
  begin
    inherited createChainTask(taskEnv);
    mapPayload.mapFunc:=func;
    mapPayload.mapParameter:=nil;
  end;

CONSTRUCTOR T_filterTask.createFilterTask(CONST taskEnv: P_context; CONST func:P_expressionLiteral);
  begin
    inherited createChainTask(taskEnv);
    filterPayload.filterFunc:=func;
    filterPayload.filterParameter:=nil;
  end;

FUNCTION T_mapTask.define(CONST x:P_literal; CONST mapLocation:T_tokenLocation):P_mapTask;
  begin
    mapPayload.mapParameter:=x;
    mapPayload.location:=mapLocation;
    nextToAggregate:=nil;
    nextToEvaluate:=nil;
    result:=@self;
  end;

FUNCTION T_filterTask.define(CONST x: P_literal; CONST filterLocation: T_tokenLocation): P_filterTask;
  begin
    filterPayload.filterParameter:=x;
    filterPayload.location:=filterLocation;
    nextToAggregate:=nil;
    nextToEvaluate:=nil;
    result:=@self;
  end;

PROCEDURE T_filterTask.evaluate(CONST recycler: P_recycler);
  begin
    context^.beginEvaluation;
    if not(isCancelled) and context^.messages^.continueEvaluation then with filterPayload do begin
      if evaluteExpressionFilter(filterFunc,location,context,recycler,filterParameter)
      then mapTaskResult:=filterParameter^.rereferenced
      else mapTaskResult:=nil;
    end else mapTaskResult:=nil;
    if filterPayload.filterParameter<>nil then recycler^.disposeLiteral(filterPayload.filterParameter);
    context^.finalizeTaskAndDetachFromParent(recycler);
  end;

DESTRUCTOR T_filterTask.destroy;
  begin
    assert(filterPayload.filterParameter=nil);
    inherited destroy;
  end;

PROCEDURE T_mapTask.evaluate(CONST recycler:P_recycler);
  begin
    context^.beginEvaluation;
    if not(isCancelled) and (context^.continueEvaluation)
    then with mapPayload do mapTaskResult:=evaluteExpressionMap(mapFunc,location,context,recycler,mapParameter).literal
    else mapTaskResult:=nil;
    if mapPayload.mapParameter<>nil
    then recycler^.disposeLiteral(mapPayload.mapParameter);
    context^.finalizeTaskAndDetachFromParent(recycler);
  end;

DESTRUCTOR T_mapTask.destroy;
  begin
    assert(mapPayload.mapParameter=nil);
    inherited destroy;
  end;

CONSTRUCTOR T_eachTask.createEachTask(CONST taskEnv:P_context);
  begin
    inherited createChainTask(taskEnv);
  end;

FUNCTION T_eachTask.define(CONST payload_:T_eachPayload):P_eachTask;
  begin
    payloads:=payload_;
    nextToAggregate:=nil;
    nextToEvaluate:=nil;
    result:=@self;
  end;

PROCEDURE T_eachTask.evaluate(CONST recycler:P_recycler);
  VAR indexLiteral:P_abstractIntLiteral;
  begin
    context^.beginEvaluation;
    evaluationResult.literal:=nil;
    if not(isCancelled) and context^.messages^.continueEvaluation then with payloads do begin
      indexLiteral:=recycler^.newIntLiteral(eachIndex);
      evaluationResult:=evaluteExpression(eachRule,eachRule^.getLocation,context,recycler,eachParameter,indexLiteral);
      recycler^.disposeLiteral(indexLiteral);
    end;
    if payloads.eachParameter<>nil then recycler^.disposeLiteral(payloads.eachParameter);
    context^.finalizeTaskAndDetachFromParent(recycler);
  end;

CONSTRUCTOR T_chainTask.createChainTask(CONST taskEnv:P_context);
  begin
    inherited create(false,taskEnv);
    isCancelled:=false;
    nextToAggregate:=nil;
  end;

PROCEDURE T_chainTask.cancelAllInAggregationChain;
  VAR current:P_chainTask;
  begin
    enterCriticalSection(taskCs);
    current:=@self;
    while current<>nil do begin
      current^.isCancelled:=true;
      current:=current^.nextToAggregate;
    end;
    leaveCriticalSection(taskCs);
  end;

FUNCTION T_chainTask.canGetResult: boolean;
  begin
    result:=context^.state=fts_finished;
  end;

CONSTRUCTOR T_futureLiteral.create(CONST func_:P_expressionLiteral; CONST param_:P_listLiteral; CONST loc:T_tokenLocation; CONST blocking:boolean);
  begin
    inherited create(loc,et_builtinAsyncOrFuture);
    initCriticalSection(criticalSection);
    isBlocking:=blocking;
    func :=func_;                      func ^.rereference;
    param:=param_;  if param<>nil then param^.rereference;
    resultValue:=nil;
    state:=fls_pending;
  end;

PROCEDURE T_futureLiteral.cleanup(CONST literalRecycler:P_literalRecycler);
  begin
    enterCriticalSection(criticalSection);
    while state=fls_evaluating do begin
      leaveCriticalSection(criticalSection);
      ThreadSwitch;
      sleep(1);
      enterCriticalSection(criticalSection);
    end;
    literalRecycler^.disposeLiteral(func);
    if param<>nil then literalRecycler^.disposeLiteral(param);
    if resultValue<>nil then literalRecycler^.disposeLiteral(resultValue);
    leaveCriticalSection(criticalSection);
  end;

DESTRUCTOR T_futureLiteral.destroy;
  begin
    doneCriticalSection(criticalSection);
    inherited;
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

FUNCTION T_futureLiteral.evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: P_literalRecycler; CONST parameters: P_listLiteral=nil): T_evaluationResult;
  begin
    enterCriticalSection(criticalSection);
    try
      if isBlocking then begin
        if state=fls_pending then executeInContext(P_context(context),P_recycler(recycler))
        else while state<>fls_done do begin
          leaveCriticalSection(criticalSection);
          ThreadSwitch;
          sleep(1);
          enterCriticalSection(criticalSection);
        end;
      end;
      result.reasonForStop:=rr_ok;
      if resultValue=nil then result.literal:=newVoidLiteral
                         else result.literal:=resultValue^.rereferenced;
    finally
      leaveCriticalSection(criticalSection);
    end;
  end;

PROCEDURE T_futureLiteral.executeInContext(CONST context:P_context; CONST recycler:P_recycler);
  begin
    enterCriticalSection(criticalSection);
    if state=fls_pending then begin
      state:=fls_evaluating;
      leaveCriticalSection(criticalSection);
    end else begin
      leaveCriticalSection(criticalSection);
      exit;
    end;
    resultValue:=func^.evaluate(getLocation,context,recycler,param).literal;
    if (resultValue=nil) and (context^.messages^.continueEvaluation)
    then context^.raiseCannotApplyError('future/async payload '+func^.toString(20),param,getLocation);

    enterCriticalSection(criticalSection);
    state:=fls_done;
    leaveCriticalSection(criticalSection);
  end;

FUNCTION T_futureLiteral.isDone:boolean;
  begin
    enterCriticalSection(criticalSection);
    result:=(state=fls_done);
    leaveCriticalSection(criticalSection);
  end;

FUNCTION T_futureLiteral.getBultinGeneratorType: T_builtinGeneratorType; begin result:=bgt_future; end;

end.

