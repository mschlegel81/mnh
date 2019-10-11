UNIT listProcessing;
INTERFACE
USES sysutils,
     math,
     mnh_constants, basicTypes,
     out_adapters,
     mnh_settings,
     litVar,subrules,
     recyclers,
     aggregators,contexts;
CONST FUTURE_RECYCLER_MAX_SIZE=16;
      ENQUEUE_QUOTIENT=2;
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
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; {$WARN 5024 OFF}CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
      PROCEDURE executeInContext(CONST context:P_context; VAR recycler:T_recycler);
  end;

PROCEDURE processListSerial  (CONST input:P_literal; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                              CONST eachLocation:T_tokenLocation;
                              VAR context:T_context; VAR recycler:T_recycler);
PROCEDURE processListParallel(CONST input:P_literal; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                              CONST eachLocation:T_tokenLocation;
                              VAR context:T_context; VAR recycler:T_recycler);
FUNCTION processMapSerial  (CONST input:P_literal; CONST expr:P_expressionLiteral;
                            CONST mapLocation:T_tokenLocation;
                            VAR context:T_context; VAR recycler:T_recycler):P_listLiteral;
FUNCTION processMapParallel(CONST input:P_literal; CONST expr:P_expressionLiteral;
                            CONST mapLocation:T_tokenLocation;
                            VAR context:T_context; VAR recycler:T_recycler):P_listLiteral;
FUNCTION processFilterSerial  (CONST input:P_compoundLiteral; CONST filterExpression:P_expressionLiteral;
                               CONST filterLocation:T_tokenLocation;
                               VAR context:T_context; VAR recycler:T_recycler):P_compoundLiteral;
FUNCTION processFilterParallel(CONST input:P_compoundLiteral; CONST filterExpression:P_expressionLiteral;
                               CONST filterLocation:T_tokenLocation;
                               VAR context:T_context; VAR recycler:T_recycler):P_compoundLiteral;
PROCEDURE aggregate(CONST input:P_literal; CONST aggregator:P_aggregator; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler);
PROCEDURE enqueueFutureTask(CONST future:P_futureLiteral; VAR context:T_context; VAR recycler:T_recycler);

VAR newIterator:FUNCTION (CONST input:P_literal):P_expressionLiteral;
IMPLEMENTATION
USES mySys,tokenArray;
TYPE
  T_eachPayload=record
    eachIndex:longint;
    eachRule:P_expressionLiteral;
    eachParameter:P_literal;
  end;
  T_eachPayloads=array of T_eachPayload;
  T_evaluationResults=array of T_evaluationResult;

  P_eachTask=^T_eachTask;
  T_eachTask=object(T_queueTask)
    payloads:T_eachPayloads;
    nextToAggregate:P_eachTask;
    results:T_evaluationResults;
    tasksPerSecond:double;
    CONSTRUCTOR createEachTask();
    PROCEDURE dropEachParameter;
    PROCEDURE defineAndEnqueueOrEvaluate(CONST taskEnv:P_context; CONST payloads_:T_eachPayloads; VAR recycler:T_recycler);
    PROCEDURE dropResults;
    PROCEDURE evaluate(VAR recycler:T_recycler); virtual;
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
    tasksPerSecond:double;
    CONSTRUCTOR createMapTask(CONST expr:P_expressionLiteral);
    PROCEDURE defineAndEnqueueOrEvaluate(CONST taskEnv:P_context; CONST x:T_arrayOfLiteral; VAR recycler:T_recycler);
    PROCEDURE evaluate(VAR recycler:T_recycler); virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION canGetResult:boolean;
    PROCEDURE clearMapPayload;
  end;

  P_filterTask=^T_filterTask;
  T_filterTask=object(T_mapTask)
    CONSTRUCTOR createFilterTask(CONST expr:P_expressionLiteral);
    PROCEDURE evaluate(VAR recycler:T_recycler); virtual;
  end;

  P_futureTask=^T_futureTask;
  T_futureTask=object(T_queueTask)
    payload:P_futureLiteral;
    CONSTRUCTOR create(CONST future:P_futureLiteral);
    PROCEDURE   evaluate(VAR recycler:T_recycler); virtual;
  end;

PROCEDURE processListSerial(CONST input:P_literal; CONST rulesList: T_expressionList; CONST aggregator: P_aggregator; CONST eachLocation: T_tokenLocation; VAR context: T_context; VAR recycler:T_recycler);
{$MACRO ON}
{$define processX:=
begin
  indexLiteral:=newIntLiteral(eachIndex);
  for rule in rulesList do if proceed then begin
    aggregator^.addToAggregation(
      rule^.evaluateToLiteral(eachLocation,@context,@recycler,x,indexLiteral),
      true,
      eachLocation,
      @context,recycler);
    proceed:=context.messages^.continueEvaluation and not(aggregator^.earlyAbort);
  end;
  inc(eachIndex);
  disposeLiteral(indexLiteral);
end}

  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      indexLiteral:P_abstractIntLiteral;
      iter:T_arrayOfLiteral;
      x:P_literal;
      proceed:boolean=true;
  begin
    if (input^.literalType=lt_expression) and (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then begin
      x:=P_expressionLiteral(input)^.evaluateToLiteral(eachLocation,@context,@recycler,nil,nil).literal;
      while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin
        processX;
        disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluateToLiteral(eachLocation,@context,@recycler,nil,nil).literal;
      end;
      if x<>nil then disposeLiteral(x);
    end else if input^.literalType in C_compoundTypes then begin
      iter:=P_compoundLiteral(input)^.iteratableList;
      for x in iter do if proceed then processX;
      disposeLiteral(iter);
    end else begin
      x:=input;
      processX;
    end;
  end;

PROCEDURE processListParallel(CONST input:P_literal;
  CONST rulesList: T_expressionList; CONST aggregator: P_aggregator;
  CONST eachLocation: T_tokenLocation; VAR context: T_context; VAR recycler:T_recycler);

  VAR earlyAborting:boolean=false;
      firstToAggregate:P_eachTask=nil;
      lastToAggregate:P_eachTask=nil;
      tasksPerSecond:double=1E-3;
      recycling:record
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_eachTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
    VAR toAggregate:P_eachTask;
        r:T_evaluationResult;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        for r in toAggregate^.results do aggregator^.addToAggregation(r,true,eachLocation,@context,recycler);
        tasksPerSecond:=tasksPerSecond*0.5+
           toAggregate^.tasksPerSecond*0.5;
        with recycling do if fill<length(dat) then begin
          toAggregate^.dropResults;
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR nextToEnqueue:T_eachPayloads;
      enqueueFill:longint=0;
      processed:longint;

  FUNCTION defineAndEnqueueTask:P_eachTask;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
        result^.clearContext;
      end else new(result,createEachTask());
      if firstToAggregate=nil then begin
        firstToAggregate:=result;
        lastToAggregate:=result;
      end else begin
        lastToAggregate^.nextToAggregate:=result;
        lastToAggregate:=result;
      end;
      result^.defineAndEnqueueOrEvaluate(context.getFutureEnvironment(recycler),nextToEnqueue,recycler);
    end;

  PROCEDURE finalizePending(CONST enqueue:boolean);
    VAR k:longint;
    begin
      setLength(nextToEnqueue,enqueueFill);
      if enqueueFill=0 then exit;
      if enqueue then begin
        defineAndEnqueueTask;
      end else begin
        for k:=0 to length(nextToEnqueue)-1 do disposeLiteral(nextToEnqueue[k].eachParameter);
      end;
      setLength(nextToEnqueue,0);
    end;

  PROCEDURE createTask(CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal); {$ifndef debugMode} inline; {$endif}
    begin
      with nextToEnqueue[enqueueFill] do begin
        eachIndex    :=idx;
        eachRule     :=expr;
        eachParameter:=x^.rereferenced;
      end;
      inc(enqueueFill);
      if enqueueFill>=length(nextToEnqueue) then begin
        inc(processed,enqueueFill);
        defineAndEnqueueTask;
        if isMemoryInComfortZone then begin
          if earlyAborting
          then setLength(nextToEnqueue,min(    8,round(max(1,0.1*tasksPerSecond))))
          else setLength(nextToEnqueue,min(16384,round(max(1,    tasksPerSecond))));
        end else begin
          tasksPerSecond:=1E-6;
          setLength(nextToEnqueue,1);
          context.getGlobals^.taskQueue.activeDeqeue(recycler);
        end;
        enqueueFill:=0;
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
    canAggregate;
    proceed:=proceed and not(aggregator^.earlyAbort)
                     and context.messages^.continueEvaluation;
  end;
  inc(eachIndex);
end}

  begin
    recycling.fill:=0;
    earlyAborting:=aggregator^.isEarlyAbortingAggregator;
    for rule in rulesList do earlyAborting:=earlyAborting or P_inlineExpression(rule)^.containsReturnToken;
    processed:=0;
    setLength(nextToEnqueue,1);
    if (input^.literalType=lt_expression) and (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then begin
      x:=P_expressionLiteral(input)^.evaluateToLiteral(eachLocation,@context,@recycler,nil,nil).literal;
      while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin
        processX;
        disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluateToLiteral(eachLocation,@context,@recycler,nil,nil).literal;
      end;
      if x<>nil then disposeLiteral(x);
    end else if input^.literalType in C_compoundTypes then begin
      iter:=P_compoundLiteral(input)^.iteratableList;
      for x in iter do if proceed then processX;
      disposeLiteral(iter);
    end else begin
      x:=input;
      processX;
    end;
    finalizePending(proceed);
    while (firstToAggregate<>nil) and context.getGlobals^.taskQueue.activeDeqeue(recycler) do canAggregate;
    while  firstToAggregate<>nil                                                           do canAggregate;
    with recycling do while fill>0 do begin
      dec(fill);
      dat[fill]^.clearContext;
      dispose(dat[fill],destroy);
    end;
  end;

FUNCTION processMapSerial(CONST input:P_literal; CONST expr:P_expressionLiteral;
                          CONST mapLocation:T_tokenLocation;
                          VAR context:T_context; VAR recycler:T_recycler):P_listLiteral;
  VAR x:P_literal;
      iter:T_arrayOfLiteral;
      isExpressionNullary:boolean;
  begin
    isExpressionNullary:=not(expr^.canApplyToNumberOfParameters(1));
    result:=newListLiteral();
    if (input^.literalType=lt_expression) and (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then begin
      x:=P_expressionLiteral(input)^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal;
      if isExpressionNullary then while (x<>nil) and (x^.literalType<>lt_void) and (context.messages^.continueEvaluation) do begin
        result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context,@recycler,nil,nil).literal,false);
        disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal;
      end else while (x<>nil) and (x^.literalType<>lt_void) and (context.messages^.continueEvaluation) do begin
        result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context,@recycler,x  ,nil).literal,false);
        disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal;
      end;
      if x<>nil then disposeLiteral(x);
    end else if input^.literalType in C_compoundTypes then begin
      iter:=P_compoundLiteral(input)^.iteratableList;
      if isExpressionNullary
      then begin for x in iter do if (context.continueEvaluation) then result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context,@recycler,nil,nil).literal,false); end
      else begin for x in iter do if (context.continueEvaluation) then result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context,@recycler,x  ,nil).literal,false); end;
      disposeLiteral(iter);
    end else begin
      if isExpressionNullary
      then result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context,@recycler,nil  ,nil).literal,false)
      else result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context,@recycler,input,nil).literal,false);
    end;
  end;

FUNCTION processMapParallel(CONST input:P_literal; CONST expr:P_expressionLiteral;
                            CONST mapLocation:T_tokenLocation;
                            VAR context:T_context; VAR recycler:T_recycler):P_listLiteral;

  VAR firstToAggregate:P_mapTask=nil;
      lastToAggregate:P_mapTask=nil;
      resultLiteral:P_listLiteral;
      tasksPerSecond:double=1E-3;
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
        for resultElement in toAggregate^.mapResult do resultLiteral^.append(resultElement,false);
        tasksPerSecond:=tasksPerSecond*0.5+
           toAggregate^.tasksPerSecond*0.5;
        with recycling do if fill<length(dat) then begin
          setLength(toAggregate^.mapResult,0);
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  PROCEDURE createTask(CONST x:T_arrayOfLiteral);
    VAR task:P_mapTask;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        task:=dat[fill];
        task^.clearContext;
      end else new(task,createMapTask(expr));
      task^.defineAndEnqueueOrEvaluate(context.getFutureEnvironment(recycler),x,recycler);
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
  FUNCTION enqueueValue(CONST x:P_literal):boolean; {$ifndef debugMode} inline; {$endif}
    begin
      nextToEnqueue[enqueueFill]:=x;
      if x<>nil then x^.rereference;
      inc(enqueueFill);
      inc(elementsProcessed);
      if enqueueFill>=length(nextToEnqueue) then begin
        createTask(nextToEnqueue);
        if isMemoryInComfortZone
        then setLength(nextToEnqueue,min(16384,round(max(1,    tasksPerSecond))))
        else setLength(nextToEnqueue,1);
        enqueueFill:=0;
        result:=true;
      end else result:=false;
    end;

  VAR x:P_literal;
      isExpressionNullary:boolean;
      iter:T_arrayOfLiteral;
  begin
    isExpressionNullary:=not(expr^.canApplyToNumberOfParameters(1));
    resultLiteral:=newListLiteral();
    recycling.fill:=0;

    setLength(nextToEnqueue,1);

    if (input^.literalType=lt_expression) and (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then begin
      x:=P_expressionLiteral(input)^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal;
      if isExpressionNullary then while (x<>nil) and (x^.literalType<>lt_void) and (context.messages^.continueEvaluation) do begin
        if enqueueValue(nil) then canAggregate;
        disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal;
      end else while (x<>nil) and (x^.literalType<>lt_void) and (context.messages^.continueEvaluation) do begin
        if enqueueValue(x  ) then canAggregate;
        disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal;
      end;
      if x<>nil then disposeLiteral(x);
    end else if input^.literalType in C_compoundTypes then begin
      iter:=P_compoundLiteral(input)^.iteratableList;
      if isExpressionNullary then begin for x in iter do if context.continueEvaluation then begin if enqueueValue(nil) then canAggregate; end end
                             else       for x in iter do if context.continueEvaluation then begin if enqueueValue(x  ) then canAggregate; end;
      disposeLiteral(iter);
    end else begin
      if isExpressionNullary
      then begin if enqueueValue(nil  ) then canAggregate; end
      else begin if enqueueValue(input) then canAggregate; end;
    end;

    setLength(nextToEnqueue,enqueueFill);
    if enqueueFill>0 then createTask(nextToEnqueue);

    while (firstToAggregate<>nil) and context.getGlobals^.taskQueue.activeDeqeue(recycler) do canAggregate;
    while  firstToAggregate<>nil                                                           do canAggregate;
    with recycling do while fill>0 do begin
      dec(fill);
      dat[fill]^.clearContext;
      dispose(dat[fill],destroy);
    end;
    result:=resultLiteral;
  end;

FUNCTION processFilterSerial  (CONST input:P_compoundLiteral; CONST filterExpression:P_expressionLiteral;
                               CONST filterLocation:T_tokenLocation;
                               VAR context:T_context; VAR recycler:T_recycler):P_compoundLiteral;
  VAR iter:T_arrayOfLiteral;
      x:P_literal;
  begin
    case input^.literalType of
      lt_emptyList,lt_emptySet,lt_emptyMap                                       : exit(P_compoundLiteral(input^.rereferenced));
      lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList: result:=newListLiteral;
      lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet : result:=newSetLiteral(0);
      else result:=newListLiteral;
    end;
    iter:=input^.iteratableList;
    for x in iter do if context.continueEvaluation and filterExpression^.evaluateToBoolean(filterLocation,@context,@recycler,true,x,nil) then P_collectionLiteral(result)^.append(x,true);
    disposeLiteral(iter);
    if input^.literalType=lt_map then begin
      x:=result;
      result:=P_listLiteral(result)^.toMap(filterLocation,@context);
      disposeLiteral(x);
    end;
  end;

FUNCTION processFilterParallel(CONST input:P_compoundLiteral; CONST filterExpression:P_expressionLiteral;
                               CONST filterLocation:T_tokenLocation;
                               VAR context:T_context; VAR recycler:T_recycler):P_compoundLiteral;
  VAR firstToAggregate:P_filterTask=nil;
      lastToAggregate:P_filterTask=nil;
      tasksPerSecond:double=1E-3;
      recycling:record
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_filterTask;
        fill:longint;
      end;
      output:P_collectionLiteral;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
    VAR toAggregate:P_filterTask;
        resultElement:P_literal;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=P_filterTask(toAggregate^.nextToAggregate);
        for resultElement in toAggregate^.mapResult do output^.append(resultElement,true);
        tasksPerSecond:=tasksPerSecond*0.5+
           toAggregate^.tasksPerSecond*0.5;
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  PROCEDURE createTask(CONST x:T_arrayOfLiteral);
    VAR task:P_filterTask;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        task:=dat[fill];
        task^.clearContext;
      end else new(task,createFilterTask(filterExpression));
      task^.defineAndEnqueueOrEvaluate(context.getFutureEnvironment(recycler),x,recycler);
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
  FUNCTION enqueueValue(CONST x:P_literal):boolean; {$ifndef debugMode} inline; {$endif}
    begin
      nextToEnqueue[enqueueFill]:=x^.rereferenced;
      inc(enqueueFill);
      inc(elementsProcessed);
      if enqueueFill>=length(nextToEnqueue) then begin
        createTask(nextToEnqueue);
        if isMemoryInComfortZone
        then setLength(nextToEnqueue,min(16384,round(max(1,tasksPerSecond))))
        else setLength(nextToEnqueue,1);
        enqueueFill:=0;
        result:=true;
      end else result:=false;
    end;

  VAR iter:T_arrayOfLiteral;
      x:P_literal;
  begin
    case input^.literalType of
      lt_emptyList,lt_emptySet,lt_emptyMap                                       : exit(P_compoundLiteral(input^.rereferenced));
      lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList: output:=newListLiteral;
      lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet : output:=newSetLiteral(input^.size);
      lt_map                                                                     : output:=newListLiteral;
    end;
    recycling.fill:=0;
    setLength(nextToEnqueue,1);
    iter:=input^.iteratableList;
    for x in iter do if context.continueEvaluation and enqueueValue(x) then canAggregate;
    disposeLiteral(iter);
    setLength(nextToEnqueue,enqueueFill);
    if enqueueFill>0 then createTask(nextToEnqueue);

    while (firstToAggregate<>nil) and context.getGlobals^.taskQueue.activeDeqeue(recycler) do canAggregate;
    while  firstToAggregate<>nil                                                           do canAggregate;
    with recycling do while fill>0 do begin
      dec(fill);
      dat[fill]^.clearContext;
      dispose(dat[fill],destroy);
    end;
    if input^.literalType=lt_map
    then begin
      result:=output^.toMap(filterLocation,@context);
    end else result:=output;
  end;

PROCEDURE aggregate(CONST input:P_literal; CONST aggregator: P_aggregator; CONST location: T_tokenLocation; VAR context: T_context; VAR recycler:T_recycler);
  VAR x:T_evaluationResult;
      iter:T_arrayOfLiteral;
  begin
    if (input^.literalType=lt_expression) and (P_expressionLiteral(input)^.typ in C_iteratableExpressionTypes) then begin
      x:=P_expressionLiteral(input)^.evaluateToLiteral(location,@context,@recycler,nil,nil);
      while (x.literal<>nil) and (x.literal^.literalType<>lt_void) and context.messages^.continueEvaluation and not(aggregator^.earlyAbort) do begin
        aggregator^.addToAggregation(x,false,location,@context,recycler);
        disposeLiteral(x.literal);
        x:=P_expressionLiteral(input)^.evaluateToLiteral(location,@context,@recycler,nil,nil);
      end;
      if x.literal<>nil then disposeLiteral(x.literal);
    end else if input^.literalType in C_compoundTypes then begin
      iter:=P_compoundLiteral(input)^.iteratableList;
      x.triggeredByReturn:=false;
      for x.literal in iter do if (x.literal^.literalType<>lt_void) and context.messages^.continueEvaluation and not(aggregator^.earlyAbort) then
        aggregator^.addToAggregation(x,false,location,@context,recycler);
      disposeLiteral(iter);
    end else begin
      x.triggeredByReturn:=false;
      x.literal:=input;
      aggregator^.addToAggregation(x,false,location,@context,recycler);
    end;
  end;

PROCEDURE enqueueFutureTask(CONST future:P_futureLiteral; VAR context:T_context; VAR recycler:T_recycler);
  VAR task:P_futureTask;
  begin
    new(task,create(future));
    task^.defineAndEnqueueOrEvaluate(context.getFutureEnvironment(recycler),recycler);
  end;

CONSTRUCTOR T_futureTask.create(CONST future: P_futureLiteral);
  begin
    inherited create(true);
    payload:=future;
  end;

PROCEDURE T_futureTask.evaluate(VAR recycler:T_recycler);
  begin
    context^.beginEvaluation;
    try
      payload^.executeInContext(context,recycler);
      disposeLiteral(payload);
    finally
      context^.finalizeTaskAndDetachFromParent(@recycler);
    end;
  end;

CONSTRUCTOR T_filterTask.createFilterTask(CONST expr: P_expressionLiteral);
  begin
    createMapTask(expr);
  end;

PROCEDURE T_filterTask.evaluate(VAR recycler:T_recycler);
  VAR k:longint;
      j:longint=0;
      wallClockAtStart:double;
  begin
    enterCriticalSection(taskCs);
    wallClockAtStart:=context^.wallclockTime;
    context^.beginEvaluation;
    leaveCriticalSection(taskCs);
    try
      if context^.messages^.continueEvaluation then with mapPayload do begin
        setLength(mapResult,length(mapParameter));
        for k:=0 to length(mapParameter)-1 do if context^.messages^.continueEvaluation then begin
          if mapRule^.evaluateToBoolean(mapRule^.getLocation,context,@recycler,true,mapParameter[k],nil)
          then begin
            mapResult[j]:=mapParameter[k];
            inc(j);
          end;
        end;
        setLength(mapResult,j);
      end;
    finally
      enterCriticalSection(taskCs);
      tasksPerSecond:=min(1E6,length(mapPayload.mapParameter)/(context^.wallclockTime-wallClockAtStart));
      clearMapPayload;
      context^.finalizeTaskAndDetachFromParent(@recycler);
      leaveCriticalSection(taskCs);
    end;
  end;

CONSTRUCTOR T_mapTask.createMapTask(CONST expr: P_expressionLiteral);
  begin
    create(false);
    mapPayload.mapRule:=expr;
    setLength(mapPayload.mapParameter,0);
  end;

PROCEDURE T_mapTask.defineAndEnqueueOrEvaluate(CONST taskEnv:P_context; CONST x:T_arrayOfLiteral; VAR recycler:T_recycler);
  VAR k:longint;
  begin
    clearMapPayload;
    setLength(mapPayload.mapParameter,length(x));
    for k:=0 to length(x)-1 do mapPayload.mapParameter[k]:=x[k];
    nextToAggregate:=nil;
    inherited defineAndEnqueueOrEvaluate(taskEnv,recycler);
  end;

PROCEDURE T_mapTask.evaluate(VAR recycler:T_recycler);
  VAR k:longint;
      j:longint=0;
      lit:P_literal;
      wallClockAtStart:double;
  begin
    enterCriticalSection(taskCs);
    wallClockAtStart:=context^.wallclockTime;
    context^.beginEvaluation;
    leaveCriticalSection(taskCs);
    try
      if context^.messages^.continueEvaluation then with mapPayload do begin
        setLength(mapResult,length(mapParameter));
        for k:=0 to length(mapParameter)-1 do if context^.messages^.continueEvaluation then begin
          lit:=mapRule^.evaluateToLiteral(mapRule^.getLocation,context,@recycler,mapParameter[k],nil).literal;
          if (lit<>nil) then begin
            if lit^.literalType=lt_void then disposeLiteral(lit)
            else begin
              mapResult[j]:=lit;
              inc(j);
            end;
          end;
        end;
        setLength(mapResult,j);
      end;
    finally
      enterCriticalSection(taskCs);
      tasksPerSecond:=min(1E6,length(mapPayload.mapParameter)/(context^.wallclockTime-wallClockAtStart));
      clearMapPayload;
      context^.finalizeTaskAndDetachFromParent(@recycler);
      leaveCriticalSection(taskCs);
    end;
  end;

DESTRUCTOR T_mapTask.destroy;
  begin
    enterCriticalSection(taskCs);
    try
      clearMapPayload;
      clearContext;
    finally
      leaveCriticalSection(taskCs);
    end;
    inherited destroy;
  end;

CONSTRUCTOR T_eachTask.createEachTask();
  begin
    create(false);
  end;

PROCEDURE T_eachTask.dropEachParameter;
  VAR k:longint;
  begin
    enterCriticalSection(taskCs);
    try
      for k:=0 to length(payloads)-1 do if payloads[k].eachParameter<>nil then disposeLiteral(payloads[k].eachParameter);
    finally
      leaveCriticalSection(taskCs);
    end;
  end;

PROCEDURE T_eachTask.dropResults;
  begin
    enterCriticalSection(taskCs);
    try
      setLength(results,0);
    finally
      leaveCriticalSection(taskCs);
    end;
  end;

PROCEDURE T_eachTask.defineAndEnqueueOrEvaluate(CONST taskEnv:P_context; CONST payloads_:T_eachPayloads; VAR recycler:T_recycler);
  begin
    enterCriticalSection(taskCs);
    try
      tasksPerSecond:=1;
      payloads:=payloads_;
      setLength(results,0);
      nextToAggregate:=nil;
      inherited defineAndEnqueueOrEvaluate(taskEnv,recycler);
    finally
      leaveCriticalSection(taskCs);
    end;
  end;

PROCEDURE T_eachTask.evaluate(VAR recycler:T_recycler);
  VAR indexLiteral:P_abstractIntLiteral;
      localReturn:boolean=false;
      k:longint=0;
      payload:T_eachPayload;
      wallClockAtStart:double;
  begin
    enterCriticalSection(taskCs);
    wallClockAtStart:=context^.wallclockTime;
    context^.beginEvaluation;
    leaveCriticalSection(taskCs);
    try
      setLength(results,length(payloads));
      for payload in payloads do if not(localReturn) and context^.messages^.continueEvaluation then with payload do begin
        indexLiteral:=newIntLiteral(eachIndex);
        results[k]:=eachRule^.evaluateToLiteral(eachRule^.getLocation,context,@recycler,eachParameter,indexLiteral);
        localReturn:=localReturn or results[k].triggeredByReturn;
        inc(k);
        disposeLiteral(indexLiteral);
      end;
    finally
      setLength(results,k);
      enterCriticalSection(taskCs);
      tasksPerSecond:=min(1E6,length(payloads)/(context^.wallclockTime-wallClockAtStart));
      dropEachParameter;
      context^.finalizeTaskAndDetachFromParent(@recycler);
      leaveCriticalSection(taskCs);
    end;
  end;

DESTRUCTOR T_eachTask.destroy;
  begin
    enterCriticalSection(taskCs);
    try
      dropEachParameter;
      clearContext;
      setLength(payloads,0);
      setLength(results,0);
    finally
      leaveCriticalSection(taskCs);
    end;
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

PROCEDURE T_mapTask.clearMapPayload;
  VAR k:longint;
  begin
    for k:=0 to length(mapPayload.mapParameter)-1 do if mapPayload.mapParameter[k]<>nil then disposeLiteral(mapPayload.mapParameter[k]);
    setLength(mapPayload.mapParameter,0);
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

FUNCTION T_futureLiteral.evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult;
  begin
    enterCriticalSection(criticalSection);
    try
      if isBlocking then begin
        if state=fls_pending then executeInContext(P_context(context),P_recycler(recycler)^)
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
    finally
      leaveCriticalSection(criticalSection);
    end;
  end;

PROCEDURE T_futureLiteral.executeInContext(CONST context:P_context; VAR recycler:T_recycler);
  begin
    enterCriticalSection(criticalSection);
    if state=fls_pending then begin
      state:=fls_evaluating;
      leaveCriticalSection(criticalSection);
    end else begin
      leaveCriticalSection(criticalSection);
      exit;
    end;
    resultValue:=func^.evaluate(getLocation,context,@recycler,param).literal;
    if (resultValue=nil) and (context^.messages^.continueEvaluation)
    then context^.raiseCannotApplyError('future/async payload '+func^.toString(20),param,getLocation);

    enterCriticalSection(criticalSection);
    state:=fls_done;
    leaveCriticalSection(criticalSection);
  end;

end.

