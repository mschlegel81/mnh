UNIT listProcessing;
INTERFACE
USES sysutils,
     myGenerics,
     mnh_constants, mnh_basicTypes,
     {$ifdef fullVersion}mnh_settings,{$endif}
     mnh_litVar,valueStore,mnh_subrules,
     mnh_aggregators,mnh_contexts;
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
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
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
                            VAR context:T_threadContext):P_listLiteral;
PROCEDURE processFilterParallel(CONST inputIterator,filterExpression:P_expressionLiteral;
                                CONST filterLocation:T_tokenLocation;
                                VAR context:T_threadContext;
                                CONST output:P_compoundLiteral);
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
    CONSTRUCTOR createEachTask(CONST environment:T_queueTaskEnvironment);
    PROCEDURE dropEachParameter;
    PROCEDURE define(CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal);
    PROCEDURE evaluate(VAR context:T_threadContext); virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_mapTask=^T_mapTask;
  T_mapTask=object(T_queueTask)
    mapPayload:record
      mapRule:P_expressionLiteral;
      mapParameter:P_literal;
    end;
    nextToAggregate:P_mapTask;
    CONSTRUCTOR createMapTask(CONST environment:T_queueTaskEnvironment; CONST expr:P_expressionLiteral);
    PROCEDURE define(CONST x:P_literal);
    PROCEDURE evaluate(VAR context:T_threadContext); virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_filterTask=^T_filterTask;
  T_filterTask=object(T_mapTask)
    CONSTRUCTOR createFilterTask(CONST environment:T_queueTaskEnvironment; CONST expr:P_expressionLiteral);
    PROCEDURE evaluate(VAR context:T_threadContext); virtual;
  end;

  P_futureTask=^T_futureTask;
  T_futureTask=object(T_queueTask)
    payload:P_futureLiteral;
    CONSTRUCTOR create(CONST environment:T_queueTaskEnvironment; CONST future:P_futureLiteral);
    PROCEDURE   evaluate(VAR context:T_threadContext); virtual;
    DESTRUCTOR  destroy; virtual;
  end;

PROCEDURE processListSerial(CONST inputIterator:P_expressionLiteral;
  CONST rulesList: T_expressionList; CONST aggregator: P_aggregator;
  CONST eachLocation: T_tokenLocation; VAR context: T_threadContext);
  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      x:P_literal;
      proceed:boolean=true;
  begin
    x:=inputIterator^.evaluateToLiteral(eachLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin
      context.valueStore^.scopePush(false);
      context.valueStore^.createVariable(EACH_INDEX_IDENTIFIER,eachIndex,true);
      for rule in rulesList do if proceed then begin
        aggregator^.addToAggregation(
          rule^.evaluateToLiteral(eachLocation,@context,x),
          true,
          eachLocation,
          @context);
        proceed:=context.threadLocalMessages.continueEvaluation and not(aggregator^.earlyAbort);
      end;
      context.valueStore^.scopePop;
      inc(eachIndex);
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

  PROCEDURE enqueueForAggregation(CONST task:P_eachTask); inline;
    begin
      if firstToAggregate=nil then begin
        firstToAggregate:=task;
        lastToAggregate:=task;
      end else begin
        lastToAggregate^.nextToAggregate:=task;
        lastToAggregate:=task;
      end;
    end;
  VAR recycling:record
        dat:array[0..31] of P_eachTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; inline;
    VAR toAggregate:P_eachTask;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        aggregator^.addToAggregation(toAggregate^.getResult,true,eachLocation,@context);
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR environment:T_queueTaskEnvironment;

  FUNCTION createTask(CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal):P_eachTask; inline;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,createEachTask(environment));
      result^.define(expr,idx,x);
    end;

  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      aimEnqueueCount:longint;
      x:P_literal;
      proceed:boolean=true;
      dequeueContext:P_threadContext;
  begin
    dequeueContext:=contextPool.newContext(@context);
    recycling.fill:=0;
    environment:=context.getFutureEnvironment;
    aimEnqueueCount:=workerThreadCount*2+1;
    x:=inputIterator^.evaluateToLiteral(eachLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin

      for rule in rulesList do if proceed then begin
        enqueueForAggregation(createTask(rule,eachIndex,x));
        if environment.taskQueue^.getQueuedCount>aimEnqueueCount then begin
          if not(canAggregate) then environment.taskQueue^.activeDeqeue(dequeueContext^);
          //if there is not enough pending after dequeuing, increase aimEnqueueCount
          if environment.taskQueue^.getQueuedCount<workerThreadCount then inc(aimEnqueueCount,workerThreadCount);
        end;
        proceed:=context.threadLocalMessages.continueEvaluation and not(aggregator^.earlyAbort);
      end;

      inc(eachIndex);
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(eachLocation,@context).literal;
    end;
    if x<>nil then disposeLiteral(x);

    while firstToAggregate<>nil do
    if not(canAggregate)
    then environment.taskQueue^
         .activeDeqeue(dequeueContext^);
    contextPool.disposeContext(dequeueContext);
    dispose(environment.values,destroy);
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
    while (x<>nil) and (x^.literalType<>lt_void) and (context.threadLocalMessages.continueEvaluation) do begin
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
                            VAR context:T_threadContext):P_listLiteral;

  VAR firstToAggregate:P_mapTask=nil;
      lastToAggregate:P_mapTask=nil;
      resultLiteral:P_listLiteral;

  PROCEDURE enqueueForAggregation(CONST task:P_mapTask); inline;
    begin
      if firstToAggregate=nil then begin
        firstToAggregate:=task;
        lastToAggregate:=task;
      end else begin
        lastToAggregate^.nextToAggregate:=task;
        lastToAggregate:=task;
      end;
    end;
  VAR recycling:record
        dat:array[0..31] of P_mapTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; inline;
    VAR toAggregate:P_mapTask;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        resultLiteral^.append(toAggregate^.getResult.literal,false);
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR environment:T_queueTaskEnvironment;

  FUNCTION createTask(CONST x:P_literal):P_mapTask; inline;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,createMapTask(environment,expr));
      result^.define(x);
    end;

  VAR x:P_literal;
      aimEnqueueCount:longint;
      isExpressionNullary:boolean;
      dequeueContext:P_threadContext;
  begin
    dequeueContext:=contextPool.newContext(@context);
    isExpressionNullary:=not(expr^.canApplyToNumberOfParameters(1));
    resultLiteral:=newListLiteral();
    recycling.fill:=0;
    environment:=context.getFutureEnvironment;
    aimEnqueueCount:=workerThreadCount*2+1;
    x:=inputIterator^.evaluateToLiteral(mapLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and (context.threadLocalMessages.continueEvaluation) do begin
      if isExpressionNullary
      then enqueueForAggregation(createTask(nil))
      else enqueueForAggregation(createTask(x  ));
      if environment.taskQueue^.getQueuedCount>aimEnqueueCount then begin
        if not(canAggregate) then environment.taskQueue^.activeDeqeue(dequeueContext^);
        //if there is not enough pending after dequeuing, increase aimEnqueueCount
        if environment.taskQueue^.getQueuedCount<workerThreadCount then inc(aimEnqueueCount,workerThreadCount);
      end;
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(mapLocation,@context).literal;
    end;
    if x<>nil then disposeLiteral(x);

    while firstToAggregate<>nil do if not(canAggregate) then environment.taskQueue^.activeDeqeue(dequeueContext^);
    dispose(environment.values,destroy);
    contextPool.disposeContext(dequeueContext);
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
    result:=resultLiteral;
  end;

PROCEDURE processFilterParallel(CONST inputIterator,filterExpression:P_expressionLiteral;
                                CONST filterLocation:T_tokenLocation;
                                VAR context:T_threadContext;
                                CONST output:P_compoundLiteral);
  VAR firstToAggregate:P_filterTask=nil;
      lastToAggregate:P_filterTask=nil;

  PROCEDURE enqueueForAggregation(CONST task:P_filterTask); inline;
    begin
      if firstToAggregate=nil then begin
        firstToAggregate:=task;
        lastToAggregate:=task;
      end else begin
        lastToAggregate^.nextToAggregate:=task;
        lastToAggregate:=task;
      end;
    end;
  VAR recycling:record
        dat:array[0..31] of P_filterTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; inline;
    VAR toAggregate:P_filterTask;
        value:P_literal;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=P_filterTask(firstToAggregate^.nextToAggregate);
        value:=toAggregate^.getResult.literal;
        if value<>nil then begin;
          if output^.literalType in C_mapTypes then begin
            P_mapLiteral(output)^.put(P_listLiteral(value)^.value[0],P_listLiteral(value)^.value[1],true);
          end else begin
            P_collectionLiteral(output)^.append(value,true);
          end;
        end;
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR environment:T_queueTaskEnvironment;

  FUNCTION createTask(CONST x:P_literal):P_filterTask; inline;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,createFilterTask(environment,filterExpression));
      result^.define(x);
    end;

  VAR x:P_literal;
      aimEnqueueCount:longint;
      dequeueContext:P_threadContext;
  begin
    dequeueContext:=contextPool.newContext(@context);
    recycling.fill:=0;
    environment:=context.getFutureEnvironment;
    aimEnqueueCount:=workerThreadCount*2+1;
    x:=inputIterator^.evaluateToLiteral(filterLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and (context.threadLocalMessages.continueEvaluation) do begin
      enqueueForAggregation(createTask(x  ));
      if environment.taskQueue^.getQueuedCount>aimEnqueueCount then begin
        if not(canAggregate) then environment.taskQueue^.activeDeqeue(dequeueContext^);
        //if there is not enough pending after dequeuing, increase aimEnqueueCount
        if environment.taskQueue^.getQueuedCount<workerThreadCount then inc(aimEnqueueCount,workerThreadCount);
      end;
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(filterLocation,@context).literal;
    end;
    if x<>nil then disposeLiteral(x);

    while firstToAggregate<>nil do if not(canAggregate) then environment.taskQueue^.activeDeqeue(dequeueContext^);
    dispose(environment.values,destroy);
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
    contextPool.disposeContext(dequeueContext);
  end;

PROCEDURE aggregate(CONST inputIterator: P_expressionLiteral; CONST aggregator: P_aggregator; CONST location: T_tokenLocation; VAR context: T_threadContext);
  VAR x:T_evaluationResult;
  begin
    x:=inputIterator^.evaluateToLiteral(location,@context);
    while (x.literal<>nil) and (x.literal^.literalType<>lt_void) and context.threadLocalMessages.continueEvaluation and not(aggregator^.earlyAbort) do begin
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
  VAR env:T_queueTaskEnvironment;
      task:P_futureTask;
  begin
    env:=context.getFutureEnvironment;
    new(task,create(env,future));
    env.taskQueue^.enqueue(task,@context);
  end;

CONSTRUCTOR T_futureTask.create(CONST environment: T_queueTaskEnvironment; CONST future: P_futureLiteral);
  begin
    inherited create(environment,true);
    payload:=future;
  end;

PROCEDURE T_futureTask.evaluate(VAR context: T_threadContext);
  begin
    enterCriticalSection(taskCs);
    state:=fts_evaluating;
    leaveCriticalSection(taskCs);
    try
      context.workerContextInit(env);
      payload^.executeInContext(@context);
      disposeLiteral(payload);
      context.workerContextDone;
    finally
      enterCriticalSection(taskCs);
      state:=fts_ready;
      leaveCriticalSection(taskCs);
    end;
  end;

DESTRUCTOR T_futureTask.destroy;
  begin
    enterCriticalSection(taskCs);
    dispose(env.values,destroy);
    leaveCriticalSection(taskCs);
    inherited destroy;
  end;

CONSTRUCTOR T_filterTask.createFilterTask(CONST environment: T_queueTaskEnvironment; CONST expr: P_expressionLiteral);
  begin
    createMapTask(environment,expr);
  end;

PROCEDURE T_filterTask.evaluate(VAR context: T_threadContext);
  begin
    enterCriticalSection(taskCs);
    state:=fts_evaluating;
    leaveCriticalSection(taskCs);
    try
      if context.threadLocalMessages.continueEvaluation then with mapPayload do begin
        context.workerContextInit(env);
        evaluationResult.triggeredByReturn:=false;
        if mapRule^.evaluateToBoolean(mapRule^.getLocation,@context,true,mapParameter)
        then evaluationResult.literal:=mapParameter
        else evaluationResult.literal:=nil;
        context.workerContextDone;
      end;
    finally
      enterCriticalSection(taskCs);
      state:=fts_ready;
      leaveCriticalSection(taskCs);
    end;
  end;

CONSTRUCTOR T_mapTask.createMapTask(CONST environment: T_queueTaskEnvironment; CONST expr: P_expressionLiteral);
  begin
    create(environment,false);
    mapPayload.mapRule:=expr;
    mapPayload.mapParameter:=nil;
  end;

PROCEDURE T_mapTask.define(CONST x: P_literal);
  begin
    enterCriticalSection(taskCs);
    if mapPayload.mapParameter<>nil then disposeLiteral(mapPayload.mapParameter);
    if x<>nil then mapPayload.mapParameter:=x^.rereferenced;
    nextToAggregate:=nil;
    env.taskQueue^.enqueue(@self,env.callingContext);
    leaveCriticalSection(taskCs);
  end;

PROCEDURE T_mapTask.evaluate(VAR context: T_threadContext);
  begin
    enterCriticalSection(taskCs);
    state:=fts_evaluating;
    leaveCriticalSection(taskCs);
    try
      if context.threadLocalMessages.continueEvaluation then with mapPayload do begin
        context.workerContextInit(env);
        evaluationResult:=mapRule^.evaluateToLiteral(mapRule^.getLocation,@context,mapParameter);
        context.workerContextDone;
      end;
    finally
      enterCriticalSection(taskCs);
      state:=fts_ready;
      leaveCriticalSection(taskCs);
    end;
  end;

DESTRUCTOR T_mapTask.destroy;
  begin
    enterCriticalSection(taskCs);
    if mapPayload.mapParameter<>nil then disposeLiteral(mapPayload.mapParameter);
    leaveCriticalSection(taskCs);
    inherited destroy;
  end;

CONSTRUCTOR T_eachTask.createEachTask(CONST environment: T_queueTaskEnvironment);
  begin
    create(environment,false);
  end;

PROCEDURE T_eachTask.dropEachParameter;
  begin
    enterCriticalSection(taskCs);
    if eachPayload.eachParameter<>nil then disposeLiteral(eachPayload.eachParameter);
    leaveCriticalSection(taskCs);
  end;

PROCEDURE T_eachTask.define(CONST expr: P_expressionLiteral; CONST idx: longint; CONST x: P_literal);
  begin
    enterCriticalSection(taskCs);
    with eachPayload do begin
      eachIndex       :=idx;
      eachRule        :=expr;
      if x=nil
      then eachParameter:=nil
      else eachParameter:=x^.rereferenced;
      state           :=fts_pending;
      evaluationResult:=NIL_EVAL_RESULT;
    end;
    nextToAggregate:=nil;
    env.taskQueue^.enqueue(@self,env.callingContext);
    leaveCriticalSection(taskCs);
  end;

PROCEDURE T_eachTask.evaluate(VAR context: T_threadContext);
  VAR idxLit:P_abstractIntLiteral;
  begin
    enterCriticalSection(taskCs);
    state:=fts_evaluating;
    leaveCriticalSection(taskCs);
    try
      if context.threadLocalMessages.continueEvaluation then with eachPayload do begin
        context.workerContextInit(env);
        context.valueStore^.scopePush(false);
        idxLit:=newIntLiteral(eachIndex);
        context.valueStore^.createVariable(EACH_INDEX_IDENTIFIER,idxLit,true);
        idxLit^.unreference;
        evaluationResult:=eachRule^.evaluateToLiteral(eachRule^.getLocation,@context,eachParameter);
        context.valueStore^.scopePop;
        context.workerContextDone;
      end;
    finally
      enterCriticalSection(taskCs);
      dropEachParameter;
      state:=fts_ready;
      leaveCriticalSection(taskCs);
    end;
  end;

DESTRUCTOR T_eachTask.destroy;
  begin
    dropEachParameter;
    inherited destroy;
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
    if (resultValue=nil) and (context^.threadLocalMessages.continueEvaluation)
    then context^.raiseCannotApplyError('future/async payload '+func^.toString(20),param,getLocation);

    enterCriticalSection(criticalSection);
    state:=fls_done;
    leaveCriticalSection(criticalSection);
  end;

end.
