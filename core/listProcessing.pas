UNIT listProcessing;
INTERFACE
USES mnh_constants, mnh_basicTypes,
     {$ifdef fullVersion}mnh_settings,{$endif}
     mnh_litVar,valueStore,
     mnh_aggregators,mnh_contexts;

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
PROCEDURE aggregate(CONST inputIterator:P_expressionLiteral; CONST aggregator:P_aggregator; CONST location:T_tokenLocation; VAR context:T_threadContext);

VAR newIterator:FUNCTION (CONST input:P_literal):P_expressionLiteral;
IMPLEMENTATION

PROCEDURE processListSerial(CONST inputIterator:P_expressionLiteral;
  CONST rulesList: T_expressionList; CONST aggregator: P_aggregator;
  CONST eachLocation: T_tokenLocation; VAR context: T_threadContext);
  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      x:P_literal;
      proceed:boolean=true;
  begin
    x:=inputIterator^.evaluateToLiteral(eachLocation,@context);
    while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin
      context.valueStore^.scopePush(false);
      context.valueStore^.createVariable(EACH_INDEX_IDENTIFIER,eachIndex,true);
      for rule in rulesList do if proceed then begin
        aggregator^.addToAggregation(
          rule^.evaluateToLiteral(eachLocation,@context,x),
          true,
          eachLocation,
          @context);
        proceed:=context.adapters^.noErrors and not(aggregator^.earlyAbort);
      end;
      context.valueStore^.scopePop;
      inc(eachIndex);
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(eachLocation,@context);
    end;
    if x<>nil then disposeLiteral(x);
  end;

PROCEDURE processListParallel(CONST inputIterator:P_expressionLiteral;
  CONST rulesList: T_expressionList; CONST aggregator: P_aggregator;
  CONST eachLocation: T_tokenLocation; VAR context: T_threadContext);

  VAR firstToAggregate:P_futureTask=nil;
      lastToAggregate:P_futureTask=nil;

  PROCEDURE enqueueForAggregation(CONST task:P_futureTask); inline;
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
        dat:array[0..31] of P_futureTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; inline;
    VAR toAggregate:P_futureTask;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        aggregator^.addToAggregation(toAggregate^.getResultAsLiteral,true,eachLocation,@context);
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR environment:T_futureTaskEnvironment;

  FUNCTION createTask(CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal):P_futureTask; inline;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,create);
      result^.define(expr,expr^.getLocation,idx,x,environment);
    end;

  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      aimEnqueueCount:longint;
      x:P_literal;
      proceed:boolean=true;
      dequeueContext:T_threadContext;
  begin
    dequeueContext.createWorkerContext(context.adapters);
    recycling.fill:=0;
    environment:=context.getFutureEnvironment;
    aimEnqueueCount:=workerThreadCount*2+1;
    x:=inputIterator^.evaluateToLiteral(eachLocation,@context);
    while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin

      for rule in rulesList do if proceed then begin
        enqueueForAggregation(createTask(rule,eachIndex,x));
        if environment.taskQueue^.getQueuedCount>aimEnqueueCount then begin
          if not(canAggregate) then environment.taskQueue^.activeDeqeue(dequeueContext);
          //if there is not enough pending after dequeuing, increase aimEnqueueCount
          if environment.taskQueue^.getQueuedCount<workerThreadCount then inc(aimEnqueueCount,workerThreadCount);
        end;
        proceed:=context.adapters^.noErrors and not(aggregator^.earlyAbort);
      end;

      inc(eachIndex);
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(eachLocation,@context);
    end;
    if x<>nil then disposeLiteral(x);

    while firstToAggregate<>nil do if not(canAggregate) then environment.taskQueue^.activeDeqeue(dequeueContext);
    dispose(environment.values,destroy);
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
    dequeueContext.destroy;
  end;

FUNCTION processMapSerial(CONST inputIterator,expr:P_expressionLiteral;
                          CONST mapLocation:T_tokenLocation;
                          VAR context:T_threadContext):P_listLiteral;
  VAR x:P_literal;
      isExpressionNullary:boolean;
  begin
    isExpressionNullary:=not(expr^.canApplyToNumberOfParameters(1));
    result:=newListLiteral();
    x:=inputIterator^.evaluateToLiteral(mapLocation,@context);
    while (x<>nil) and (x^.literalType<>lt_void) and (context.adapters^.noErrors) do begin
      if isExpressionNullary
      then result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context  ),false)
      else result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context,x),false);
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(mapLocation,@context);
    end;
    if x<>nil then disposeLiteral(x);
  end;

FUNCTION processMapParallel(CONST inputIterator,expr:P_expressionLiteral;
                            CONST mapLocation:T_tokenLocation;
                            VAR context:T_threadContext):P_listLiteral;

  VAR firstToAggregate:P_futureTask=nil;
      lastToAggregate:P_futureTask=nil;
      resultLiteral:P_listLiteral;

  PROCEDURE enqueueForAggregation(CONST task:P_futureTask); inline;
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
        dat:array[0..31] of P_futureTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; inline;
    VAR toAggregate:P_futureTask;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        resultLiteral^.append(toAggregate^.getResultAsLiteral,false);
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR environment:T_futureTaskEnvironment;

  FUNCTION createTask(CONST x:P_literal):P_futureTask; inline;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,create);
      result^.define(expr,expr^.getLocation,-1,x,environment);
    end;

  VAR x:P_literal;
      aimEnqueueCount:longint;
      isExpressionNullary:boolean;
      dequeueContext:T_threadContext;
  begin
    dequeueContext.createWorkerContext(context.adapters);
    isExpressionNullary:=not(expr^.canApplyToNumberOfParameters(1));
    resultLiteral:=newListLiteral();
    recycling.fill:=0;
    environment:=context.getFutureEnvironment;
    aimEnqueueCount:=workerThreadCount*2+1;
    x:=inputIterator^.evaluateToLiteral(mapLocation,@context);
    while (x<>nil) and (x^.literalType<>lt_void) and (context.adapters^.noErrors) do begin
      if isExpressionNullary
      then enqueueForAggregation(createTask(nil))
      else enqueueForAggregation(createTask(x  ));
      if environment.taskQueue^.getQueuedCount>aimEnqueueCount then begin
        if not(canAggregate) then environment.taskQueue^.activeDeqeue(dequeueContext);
        //if there is not enough pending after dequeuing, increase aimEnqueueCount
        if environment.taskQueue^.getQueuedCount<workerThreadCount then inc(aimEnqueueCount,workerThreadCount);
      end;
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(mapLocation,@context);
    end;
    if x<>nil then disposeLiteral(x);

    while firstToAggregate<>nil do if not(canAggregate) then environment.taskQueue^.activeDeqeue(dequeueContext);
    dispose(environment.values,destroy);
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
    result:=resultLiteral;
    dequeueContext.destroy;
  end;

PROCEDURE aggregate(CONST inputIterator: P_expressionLiteral; CONST aggregator: P_aggregator; CONST location: T_tokenLocation; VAR context: T_threadContext);
  VAR x:P_literal;
  begin
    x:=inputIterator^.evaluateToLiteral(location,@context);
    while (x<>nil) and (x^.literalType<>lt_void) and context.adapters^.noErrors and not(aggregator^.earlyAbort) do begin
      aggregator^.addToAggregation(
        x,
        false,
        location,
        @context);
      disposeLiteral(x);
      x:=inputIterator^.evaluateToLiteral(location,@context);
    end;
    if x<>nil then disposeLiteral(x);
  end;

end.
