UNIT listProcessing;
INTERFACE
USES mnh_constants, mnh_basicTypes,
     {$ifdef fullVersion}mnh_settings,{$endif}
     mnh_litVar,valueStore,
     mnh_aggregators,mnh_contexts;
TYPE
  P_iterator=^T_iterator;
  T_iterator=object
    FUNCTION next(CONST context:P_threadContext):P_literal; virtual; abstract;
    DESTRUCTOR destroy; virtual; abstract;
  end;

PROCEDURE processListSerial(CONST inputIterator:P_iterator; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                            CONST eachLocation:T_tokenLocation;
                            VAR context:T_threadContext);
PROCEDURE processListParallel(CONST inputIterator:P_iterator; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                              CONST eachLocation:T_tokenLocation;
                              VAR context:T_threadContext);
FUNCTION processMapSerial(CONST inputIterator:P_iterator; CONST expr:P_expressionLiteral;
                          VAR context:T_threadContext):P_listLiteral;
FUNCTION processMapParallel(CONST inputIterator:P_iterator; CONST expr:P_expressionLiteral;
                            VAR context:T_threadContext):P_listLiteral;
PROCEDURE aggregate(CONST inputIterator:P_iterator; CONST aggregator:P_aggregator; CONST location:T_tokenLocation; VAR context:T_threadContext);

FUNCTION newIterator(CONST input:P_literal):P_iterator;
IMPLEMENTATION
TYPE
  P_listIterator=^T_listIterator;
  T_listIterator=object(T_iterator)
    index:longint;
    values:T_arrayOfLiteral;
    CONSTRUCTOR create(CONST v:P_compoundLiteral);
    FUNCTION next(CONST context:P_threadContext):P_literal; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_generator=^T_generator;
  T_generator=object(T_iterator)
    generator:P_expressionLiteral;
    CONSTRUCTOR create(CONST g:P_expressionLiteral);
    FUNCTION next(CONST context:P_threadContext):P_literal; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_singleValueIterator=^T_singleValueIterator;
  T_singleValueIterator=object(T_iterator)
    didDeliver:boolean;
    value:P_literal;
    CONSTRUCTOR create(CONST v:P_literal);
    FUNCTION next(CONST context:P_threadContext):P_literal; virtual;
    DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_listIterator.create(CONST v: P_compoundLiteral);
  begin
    index:=0;
    values:=v^.iteratableList;
  end;

FUNCTION T_listIterator.next(CONST context:P_threadContext): P_literal;
  begin
    if index>=length(values)
    then result:=nil
    else result:=values[index]^.rereferenced;
    inc(index);
  end;

DESTRUCTOR T_listIterator.destroy;
  begin
    disposeLiteral(values);
  end;

CONSTRUCTOR T_generator.create(CONST g: P_expressionLiteral);
  begin
    generator:=g;
    generator^.rereference;
  end;

FUNCTION T_generator.next(CONST context:P_threadContext): P_literal;
  begin
    result:=generator^.evaluateToLiteral(generator^.getLocation,context);
  end;

DESTRUCTOR T_generator.destroy;
  begin
    disposeLiteral(generator);
  end;

CONSTRUCTOR T_singleValueIterator.create(CONST v: P_literal);
  begin
    didDeliver:=false;
    value:=v^.rereferenced;
  end;

FUNCTION T_singleValueIterator.next(CONST context: P_threadContext): P_literal;
  begin
    if didDeliver then exit(nil);
    result:=value^.rereferenced;
    didDeliver:=true;
  end;

DESTRUCTOR T_singleValueIterator.destroy;
  begin
    disposeLiteral(value);
  end;

FUNCTION newIterator(CONST input:P_literal):P_iterator;
  begin
    if input^.literalType in C_compoundTypes then new(P_listIterator(result),create(P_compoundLiteral(input)))
    else if (input^.literalType=lt_expression) and
            (P_expressionLiteral(input)^.canApplyToNumberOfParameters(0)) and
            (P_expressionLiteral(input)^.isStateful) then new(P_generator(result),create(P_expressionLiteral(input)))
    else new(P_singleValueIterator(result),create(input));
  end;

FUNCTION newGeneratorIterator(CONST g:P_expressionLiteral):P_iterator;
  VAR it:P_generator;
  begin
    new(it,create(g));
    result:=it;
  end;

PROCEDURE processListSerial(CONST inputIterator:P_iterator;
  CONST rulesList: T_expressionList; CONST aggregator: P_aggregator;
  CONST eachLocation: T_tokenLocation; VAR context: T_threadContext);
  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      x:P_literal;
      proceed:boolean=true;
  begin
    x:=inputIterator^.next(@context);
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
      x:=inputIterator^.next(@context);
    end;
    if x<>nil then disposeLiteral(x);
  end;

PROCEDURE processListParallel(CONST inputIterator:P_iterator;
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

  VAR values:P_valueStore;
      taskQueue:P_taskQueue;
      initialDepth:longint;
      initialAllow:T_sideEffects;

  FUNCTION createTask(CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal):P_futureTask; inline;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,create);
      result^.define(expr,expr^.getLocation,idx,x,@context,values,initialDepth,initialAllow);
      taskQueue^.enqueue(result,@context);
    end;

  VAR rule:P_expressionLiteral;
      eachIndex:longint=0;
      aimEnqueueCount:longint;
      x:P_literal;
      proceed:boolean=true;
  begin
    recycling.fill:=0;
    taskQueue:=context.getParent^.getTaskQueue;
    values:=context.valueStore^.readOnlyClone;
    initialDepth:=context.callDepth;
    initialAllow:=context.sideEffectWhitelist;
    aimEnqueueCount:=workerThreadCount*2+1;
    x:=inputIterator^.next(@context);
    while (x<>nil) and (x^.literalType<>lt_void) and proceed do begin

      for rule in rulesList do if proceed then begin
        enqueueForAggregation(createTask(rule,eachIndex,x));
        if taskQueue^.getQueuedCount>aimEnqueueCount then begin
          if not(canAggregate) then taskQueue^.activeDeqeue(context);
          //if there is not enough pending after dequeuing, increase aimEnqueueCount
          if taskQueue^.getQueuedCount<workerThreadCount then inc(aimEnqueueCount,workerThreadCount);
        end;
        proceed:=context.adapters^.noErrors and not(aggregator^.earlyAbort);
      end;

      inc(eachIndex);
      disposeLiteral(x);
      x:=inputIterator^.next(@context);
    end;
    if x<>nil then disposeLiteral(x);

    while firstToAggregate<>nil do if not(canAggregate) then taskQueue^.activeDeqeue(context);
    dispose(values,destroy);
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
  end;

FUNCTION processMapSerial(CONST inputIterator:P_iterator; CONST expr:P_expressionLiteral;
                          VAR context:T_threadContext):P_listLiteral;
  VAR x:P_literal;
  begin
    result:=newListLiteral();
    x:=inputIterator^.next(@context);
    while (x<>nil) and (x^.literalType<>lt_void) and (context.adapters^.noErrors) do begin
      result^.append(expr^.evaluateToLiteral(expr^.getLocation,@context,x),false);
      disposeLiteral(x);
      x:=inputIterator^.next(@context);
    end;
    if x<>nil then disposeLiteral(x);
  end;

FUNCTION processMapParallel(CONST inputIterator:P_iterator; CONST expr:P_expressionLiteral;
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

  VAR values:P_valueStore;
      taskQueue:P_taskQueue;
      initialDepth:longint;
      initialAllow:T_sideEffects;

  FUNCTION createTask(CONST x:P_literal):P_futureTask; inline;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,create);
      result^.define(expr,expr^.getLocation,-1,x,@context,values,initialDepth,initialAllow);
      taskQueue^.enqueue(result,@context);
    end;

  VAR x:P_literal;
      aimEnqueueCount:longint;
  begin
    resultLiteral:=newListLiteral();
    recycling.fill:=0;
    taskQueue:=context.getParent^.getTaskQueue;
    values:=context.valueStore^.readOnlyClone;
    initialDepth:=context.callDepth;
    initialAllow:=context.sideEffectWhitelist;
    aimEnqueueCount:=workerThreadCount*2+1;
    x:=inputIterator^.next(@context);
    while (x<>nil) and (x^.literalType<>lt_void) and (context.adapters^.noErrors) do begin
      enqueueForAggregation(createTask(x));
      if taskQueue^.getQueuedCount>aimEnqueueCount then begin
        if not(canAggregate) then taskQueue^.activeDeqeue(context);
        //if there is not enough pending after dequeuing, increase aimEnqueueCount
        if taskQueue^.getQueuedCount<workerThreadCount then inc(aimEnqueueCount,workerThreadCount);
      end;
      disposeLiteral(x);
      x:=inputIterator^.next(@context);
    end;
    if x<>nil then disposeLiteral(x);

    while firstToAggregate<>nil do if not(canAggregate) then taskQueue^.activeDeqeue(context);
    dispose(values,destroy);
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
    result:=resultLiteral;
  end;

PROCEDURE aggregate(CONST inputIterator: P_iterator; CONST aggregator: P_aggregator; CONST location: T_tokenLocation; VAR context: T_threadContext);
  VAR x:P_literal;
  begin
    x:=inputIterator^.next(@context);
    while (x<>nil) and (x^.literalType<>lt_void) and context.adapters^.noErrors and not(aggregator^.earlyAbort) do begin
      aggregator^.addToAggregation(
        x,
        false,
        location,
        @context);
      disposeLiteral(x);
      x:=inputIterator^.next(@context);
    end;
    if x<>nil then disposeLiteral(x);
  end;

end.
