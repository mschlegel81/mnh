UNIT listProcessing;
INTERFACE
USES sysutils,
     mnh_constants, mnh_basicTypes,
     mnh_settings,
     mnh_litVar,valueStore,mnh_subrules,
     mnh_aggregators,mnh_contexts;
CONST FUTURE_RECYCLER_MAX_SIZE=64;
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
    CONSTRUCTOR createEachTask();
    PROCEDURE dropEachParameter;
    PROCEDURE defineAndEnqueue(CONST taskEnv:P_threadContext; CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal);
    PROCEDURE evaluate; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_mapTask=^T_mapTask;
  T_mapTask=object(T_queueTask)
    mapPayload:record
      mapRule:P_expressionLiteral;
      mapParameter:P_literal;
    end;
    nextToAggregate:P_mapTask;
    CONSTRUCTOR createMapTask(CONST expr:P_expressionLiteral);
    PROCEDURE defineAndEnqueue(CONST taskEnv:P_threadContext; CONST x:P_literal);
    PROCEDURE evaluate; virtual;
    DESTRUCTOR destroy; virtual;
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
      scopePush(context.valueScope,ACCESS_READWRITE);
      context.valueScope^.createVariable(EACH_INDEX_IDENTIFIER,eachIndex,true);
      for rule in rulesList do if proceed then begin
        aggregator^.addToAggregation(
          rule^.evaluateToLiteral(eachLocation,@context,x),
          true,
          eachLocation,
          @context);
        proceed:=context.messages.continueEvaluation and not(aggregator^.earlyAbort);
      end;
      scopePop(context.valueScope);
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

  PROCEDURE enqueueForAggregation(CONST task:P_eachTask); {$ifndef debugMode} inline; {$endif}
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
        aggregator^.addToAggregation(toAggregate^.getResult,true,eachLocation,@context);
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  FUNCTION createTask(CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal):P_eachTask; {$ifndef debugMode} inline; {$endif}
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,createEachTask());
      result^.defineAndEnqueue(context.getFutureEnvironment,expr,idx,x);
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
        enqueueForAggregation(createTask(rule,eachIndex,x));
        if context.getGlobals^.taskQueue.getQueuedCount>=length(recycling.dat) then begin
          if not(canAggregate) then context.getGlobals^.taskQueue.activeDeqeue;
        end;
        proceed:=context.messages.continueEvaluation and not(aggregator^.earlyAbort);
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
                            VAR context:T_threadContext):P_listLiteral;

  VAR firstToAggregate:P_mapTask=nil;
      lastToAggregate:P_mapTask=nil;
      resultLiteral:P_listLiteral;

  PROCEDURE enqueueForAggregation(CONST task:P_mapTask); {$ifndef debugMode} inline; {$endif}
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
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_mapTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
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

  FUNCTION createTask(CONST x:P_literal):P_mapTask; {$ifndef debugMode} inline; {$endif}
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,createMapTask(expr));
      result^.defineAndEnqueue(context.getFutureEnvironment,x);
    end;

  VAR x:P_literal;
      isExpressionNullary:boolean;
  begin
    isExpressionNullary:=not(expr^.canApplyToNumberOfParameters(1));
    resultLiteral:=newListLiteral();
    recycling.fill:=0;
    x:=inputIterator^.evaluateToLiteral(mapLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and (context.messages.continueEvaluation) do begin
      if isExpressionNullary
      then enqueueForAggregation(createTask(nil))
      else enqueueForAggregation(createTask(x  ));
      if context.getGlobals^.taskQueue.getQueuedCount>=length(recycling.dat) then begin
        if not(canAggregate) then context.getGlobals^.taskQueue.activeDeqeue;
      end;
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(mapLocation,@context).literal;
    end;
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
                                CONST output:P_compoundLiteral);
  VAR firstToAggregate:P_filterTask=nil;
      lastToAggregate:P_filterTask=nil;

  PROCEDURE enqueueForAggregation(CONST task:P_filterTask); {$ifndef debugMode} inline; {$endif}
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
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_filterTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
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

  FUNCTION createTask(CONST x:P_literal):P_filterTask; {$ifndef debugMode} inline; {$endif}
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,createFilterTask(filterExpression));
      result^.defineAndEnqueue(context.getFutureEnvironment,x);
    end;

  VAR x:P_literal;
  begin
    recycling.fill:=0;
    x:=inputIterator^.evaluateToLiteral(filterLocation,@context).literal;
    while (x<>nil) and (x^.literalType<>lt_void) and (context.messages.continueEvaluation) do begin
      enqueueForAggregation(createTask(x));
      if context.getGlobals^.taskQueue.getQueuedCount>=length(recycling.dat) then begin
        if not(canAggregate) then context.getGlobals^.taskQueue.activeDeqeue;
      end;
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(filterLocation,@context).literal;
    end;
    if x<>nil then disposeLiteral(x);

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
    env^.beginEvaluation;
    try
      payload^.executeInContext(env);
      disposeLiteral(payload);
    finally
      env^.finalizeTaskAndDetachFromParent;
    end;
  end;

CONSTRUCTOR T_filterTask.createFilterTask(CONST expr: P_expressionLiteral);
  begin
    createMapTask(expr);
  end;

PROCEDURE T_filterTask.evaluate;
  begin
    env^.beginEvaluation;
    try
      if env^.messages.continueEvaluation then with mapPayload do begin
        evaluationResult.triggeredByReturn:=false;
        if mapRule^.evaluateToBoolean(mapRule^.getLocation,env,true,mapParameter)
        then evaluationResult.literal:=mapParameter
        else evaluationResult.literal:=nil;
      end;
    finally
      enterCriticalSection(taskCs);
      env^.finalizeTaskAndDetachFromParent;
      leaveCriticalSection(taskCs);
    end;
  end;

CONSTRUCTOR T_mapTask.createMapTask(CONST expr: P_expressionLiteral);
  begin
    create(false);
    mapPayload.mapRule:=expr;
    mapPayload.mapParameter:=nil;
  end;

PROCEDURE T_mapTask.defineAndEnqueue(CONST taskEnv:P_threadContext; CONST x:P_literal);
  begin
    enterCriticalSection(taskCs);
    if mapPayload.mapParameter<>nil then disposeLiteral(mapPayload.mapParameter);
    if x<>nil then mapPayload.mapParameter:=x^.rereferenced;
    nextToAggregate:=nil;
    inherited defineAndEnqueue(taskEnv);
    leaveCriticalSection(taskCs);
  end;

PROCEDURE T_mapTask.evaluate;
  begin
    env^.beginEvaluation;
    try
      if env^.messages.continueEvaluation then with mapPayload do begin
        evaluationResult:=mapRule^.evaluateToLiteral(mapRule^.getLocation,env,mapParameter);
      end;
    finally
      enterCriticalSection(taskCs);
      env^.finalizeTaskAndDetachFromParent;
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
  VAR oldScope:P_valueScope;
  begin
    enterCriticalSection(taskCs);
    env^.beginEvaluation;
    leaveCriticalSection(taskCs);
    try
      if env^.messages.continueEvaluation then with eachPayload do begin
        oldScope:=env^.valueScope;
        scopePush(env^.valueScope,ACCESS_READWRITE);
        env^.valueScope^.createVariable(EACH_INDEX_IDENTIFIER,eachIndex,true);
        evaluationResult:=eachRule^.evaluateToLiteral(eachRule^.getLocation,env,eachParameter);
        while (env^.valueScope<>oldScope) and (env^.valueScope<>nil) do scopePop(env^.valueScope);
      end;
    finally
      enterCriticalSection(taskCs);
      dropEachParameter;
      env^.finalizeTaskAndDetachFromParent;
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
    if (resultValue=nil) and (context^.messages.continueEvaluation)
    then context^.raiseCannotApplyError('future/async payload '+func^.toString(20),param,getLocation);

    enterCriticalSection(criticalSection);
    state:=fls_done;
    leaveCriticalSection(criticalSection);
  end;

end.
