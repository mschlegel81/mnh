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

  P_futureMapLiteral=^T_futureMapLiteral;
  T_futureMapLiteral=object(T_futureLiteral)
    nextToAggregate:P_futureMapLiteral;
    CONSTRUCTOR create(CONST func_:P_expressionLiteral; CONST param_:P_listLiteral; CONST loc:T_tokenLocation);
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

VAR newIterator:FUNCTION (CONST input:P_literal; CONST location:T_tokenLocation):P_expressionLiteral;
IMPLEMENTATION
USES mySys,tokenArray;
TYPE
  T_eachPayload=record
    eachIndex:longint;
    eachRule:P_expressionLiteral;
    eachParameter:P_literal;
  end;

  P_eachTask=^T_eachTask;
  T_eachTask=object(T_queueTask)
    payloads:T_eachPayload;
    nextToAggregate:P_eachTask;
    evaluationResult:T_evaluationResult;
    CONSTRUCTOR createEachTask(CONST taskEnv:P_context);
    PROCEDURE defineAndEnqueueOrEvaluate(CONST payload_:T_eachPayload; VAR recycler:T_recycler);
    PROCEDURE evaluate(VAR recycler:T_recycler); virtual;
    FUNCTION canGetResult:boolean;
  end;

  P_mapTask=^T_mapTask;
  T_mapTask=object(T_queueTask)
    mapPayload:record
      mapRule:P_expressionLiteral;
      mapParameter:P_literal;
      location:T_tokenLocation;
    end;
    mapResult:P_literal;
    nextToAggregate:P_mapTask;
    CONSTRUCTOR createMapTask(CONST taskEnv:P_context; CONST expr:P_expressionLiteral);
    PROCEDURE defineAndEnqueueOrEvaluate(CONST x:P_literal; VAR recycler:T_recycler; CONST mapLocation:T_tokenLocation);
    PROCEDURE evaluate(VAR recycler:T_recycler); virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION canGetResult:boolean;
  end;

  P_filterTask=^T_filterTask;
  T_filterTask=object(T_mapTask)
    CONSTRUCTOR createFilterTask(CONST taskEnv:P_context; CONST expr:P_expressionLiteral);
    PROCEDURE evaluate(VAR recycler:T_recycler); virtual;
  end;

  P_futureTask=^T_futureTask;
  T_futureTask=object(T_queueTask)
    payload:P_futureLiteral;
    CONSTRUCTOR create(CONST taskEnv:P_context; CONST future:P_futureLiteral);
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
      recycling:record
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_eachTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
    VAR toAggregate:P_eachTask;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        assert(firstToAggregate^.getContext<>nil);
        assert(firstToAggregate^.getContext^.valueScope=nil,'valueScope must be nil at this point '+intToStr(ptrint(pointer(firstToAggregate))));
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        aggregator^.addToAggregation(toAggregate^.evaluationResult,true,eachLocation,@context,recycler);
        with recycling do if fill<length(dat) then begin
          toAggregate^.nextToAggregate:=nil;
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR processed:longint=0;
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
        newTask:=dat[fill];
        newTask^.reattach(recycler);
      end else new(newTask,createEachTask(context.getFutureEnvironment(recycler)));
      if firstToAggregate=nil
      then firstToAggregate:=newTask
      else lastToAggregate^.nextToAggregate:=newTask;
      lastToAggregate:=newTask;
      newTask^.defineAndEnqueueOrEvaluate(nextToEnqueue,recycler);
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
    while context.continueEvaluation and (firstToAggregate<>nil) and context.getGlobals^.taskQueue.activeDeqeue(recycler) do canAggregate;
    while context.continueEvaluation and (firstToAggregate<>nil)                                                          do canAggregate;
    with recycling do while fill>0 do begin
      dec(fill);
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
        result^.append(expr^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal,false);
        disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal;
      end else while (x<>nil) and (x^.literalType<>lt_void) and (context.messages^.continueEvaluation) do begin
        result^.append(expr^.evaluateToLiteral(mapLocation,@context,@recycler,x  ,nil).literal,false);
        disposeLiteral(x);
        x:=P_expressionLiteral(input)^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal;
      end;
      if x<>nil then disposeLiteral(x);
    end else if input^.literalType in C_compoundTypes then begin
      iter:=P_compoundLiteral(input)^.iteratableList;
      if isExpressionNullary
      then begin for x in iter do if (context.continueEvaluation) then result^.append(expr^.evaluateToLiteral(mapLocation,@context,@recycler,nil,nil).literal,false); end
      else begin for x in iter do if (context.continueEvaluation) then result^.append(expr^.evaluateToLiteral(mapLocation,@context,@recycler,x  ,nil).literal,false); end;
      disposeLiteral(iter);
    end else begin
      if isExpressionNullary
      then result^.append(expr^.evaluateToLiteral(mapLocation,@context,@recycler,nil  ,nil).literal,false)
      else result^.append(expr^.evaluateToLiteral(mapLocation,@context,@recycler,input,nil).literal,false);
    end;
  end;

FUNCTION processMapParallel(CONST input:P_literal; CONST expr:P_expressionLiteral;
                            CONST mapLocation:T_tokenLocation;
                            VAR context:T_context; VAR recycler:T_recycler):P_listLiteral;

  VAR firstToAggregate:P_mapTask=nil;
      lastToAggregate:P_mapTask=nil;
      resultLiteral:P_listLiteral;
  VAR recycling:record
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_mapTask;
        fill:longint;
      end;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
    VAR toAggregate:P_mapTask;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        assert(firstToAggregate^.getContext<>nil);
        assert(firstToAggregate^.getContext^.valueScope=nil,'valueScope must be nil at this point '+intToStr(ptrint(pointer(firstToAggregate))));
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=firstToAggregate^.nextToAggregate;
        assert(toAggregate^.mapResult<>nil);
        resultLiteral^.append(toAggregate^.mapResult,false);
        with recycling do if fill<length(dat) then begin
          toAggregate^.mapResult:=nil;
          toAggregate^.nextToAggregate:=nil;
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  FUNCTION enqueueValue(CONST x:P_literal):boolean; {$ifndef debugMode} inline; {$endif}
    VAR task:P_mapTask;
    begin
      if x<>nil then x^.rereference;
      result:=true;

      with recycling do if fill>0 then begin
        dec(fill);
        task:=dat[fill];
        task^.reattach(recycler);
      end else new(task,createMapTask(context.getFutureEnvironment(recycler),expr));
      if firstToAggregate=nil then begin
        firstToAggregate:=task;
        lastToAggregate:=task;
      end else begin
        lastToAggregate^.nextToAggregate:=task;
        lastToAggregate:=task;
      end;
      task^.defineAndEnqueueOrEvaluate(x,recycler,mapLocation);
    end;

  VAR x:P_literal;
      isExpressionNullary:boolean;
      iter:T_arrayOfLiteral;
  begin
    isExpressionNullary:=not(expr^.canApplyToNumberOfParameters(1));
    resultLiteral:=newListLiteral();
    recycling.fill:=0;

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

    while context.continueEvaluation and (firstToAggregate<>nil) and context.getGlobals^.taskQueue.activeDeqeue(recycler) do canAggregate;
    while context.continueEvaluation and (firstToAggregate<>nil)                                                          do canAggregate;
    with recycling do while fill>0 do begin
      dec(fill);
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
      recycling:record
        dat:array[0..FUTURE_RECYCLER_MAX_SIZE-1] of P_filterTask;
        fill:longint;
      end;
      output:P_collectionLiteral;

  FUNCTION canAggregate:boolean; {$ifndef debugMode} inline; {$endif}
    VAR toAggregate:P_filterTask;
        value:P_literal;
    begin
      result:=false;
      while (firstToAggregate<>nil) and (firstToAggregate^.canGetResult) do begin
        result:=true;
        toAggregate:=firstToAggregate;
        firstToAggregate:=P_filterTask(toAggregate^.nextToAggregate);
        value:=toAggregate^.mapResult;
        if value<>nil then output^.append(value,true);
        with recycling do if fill<length(dat) then begin
          toAggregate^.nextToAggregate:=nil;
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR elementsProcessed:longint=0;
  FUNCTION enqueueValue(CONST x:P_literal):boolean; {$ifndef debugMode} inline; {$endif}
    VAR task:P_filterTask;
    begin
      inc(elementsProcessed);
      result:=true;

      with recycling do if fill>0 then begin
        dec(fill);
        task:=dat[fill];
        task^.reattach(recycler);
      end else new(task,createFilterTask(context.getFutureEnvironment(recycler),filterExpression));
      if firstToAggregate=nil then begin
        firstToAggregate:=task;
        lastToAggregate:=task;
      end else begin
        lastToAggregate^.nextToAggregate:=task;
        lastToAggregate:=task;
      end;
      task^.defineAndEnqueueOrEvaluate(x^.rereferenced,recycler,filterLocation);
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
    iter:=input^.iteratableList;
    for x in iter do if context.continueEvaluation and enqueueValue(x) then canAggregate;
    disposeLiteral(iter);

    while context.continueEvaluation and (firstToAggregate<>nil) and context.getGlobals^.taskQueue.activeDeqeue(recycler) do canAggregate;
    while context.continueEvaluation and (firstToAggregate<>nil)                                                          do canAggregate;
    with recycling do while fill>0 do begin
      dec(fill);
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
    new(task,create(context.getFutureEnvironment(recycler),future));
    task^.defineAndEnqueueOrEvaluate(recycler);
  end;

CONSTRUCTOR T_futureMapLiteral.create(CONST func_: P_expressionLiteral; CONST param_: P_listLiteral; CONST loc: T_tokenLocation);
  begin
    inherited create(func_,param_,loc,true);
    nextToAggregate:=nil;
  end;

CONSTRUCTOR T_futureTask.create(CONST taskEnv:P_context; CONST future: P_futureLiteral);
  begin
    inherited create(true,taskEnv);
    payload:=future;
  end;

PROCEDURE T_futureTask.evaluate(VAR recycler:T_recycler);
  begin
    context^.beginEvaluation;
    payload^.executeInContext(context,recycler);
    disposeLiteral(payload);
    context^.finalizeTaskAndDetachFromParent(@recycler);
  end;

CONSTRUCTOR T_filterTask.createFilterTask(CONST taskEnv:P_context; CONST expr: P_expressionLiteral);
  begin
    createMapTask(taskEnv,expr);
  end;

PROCEDURE T_filterTask.evaluate(VAR recycler:T_recycler);
  begin
    context^.beginEvaluation;
    if context^.messages^.continueEvaluation then with mapPayload do begin
      if mapRule^.evaluateToBoolean(location,context,@recycler,true,mapParameter,nil)
      then mapResult:=mapParameter
      else mapResult:=nil;
    end;
    if mapPayload.mapParameter<>nil then disposeLiteral(mapPayload.mapParameter);
    context^.finalizeTaskAndDetachFromParent(@recycler);
  end;

CONSTRUCTOR T_mapTask.createMapTask(CONST taskEnv:P_context; CONST expr: P_expressionLiteral);
  begin
    create(false,taskEnv);
    mapPayload.mapRule:=expr;
    mapPayload.mapParameter:=nil;
  end;

PROCEDURE T_mapTask.defineAndEnqueueOrEvaluate(CONST x:P_literal; VAR recycler:T_recycler; CONST mapLocation:T_tokenLocation);
  begin
    mapPayload.mapParameter:=x;
    mapPayload.location:=mapLocation;
    nextToAggregate:=nil;
    inherited defineAndEnqueueOrEvaluate(recycler);
  end;

PROCEDURE T_mapTask.evaluate(VAR recycler:T_recycler);
  begin
    context^.beginEvaluation;
    if context^.messages^.continueEvaluation then with mapPayload do begin
      mapResult:=mapRule^.evaluateToLiteral(location,context,@recycler,mapParameter,nil).literal;
    end;
    if mapPayload.mapParameter<>nil then disposeLiteral(mapPayload.mapParameter);
    context^.finalizeTaskAndDetachFromParent(@recycler);
  end;

DESTRUCTOR T_mapTask.destroy;
  begin
    if mapPayload.mapParameter<>nil then disposeLiteral(mapPayload.mapParameter);
    inherited destroy;
  end;

CONSTRUCTOR T_eachTask.createEachTask(CONST taskEnv:P_context);
  begin
    create(false,taskEnv);
  end;

PROCEDURE T_eachTask.defineAndEnqueueOrEvaluate(CONST payload_:T_eachPayload; VAR recycler:T_recycler);
  begin
    payloads:=payload_;
    nextToAggregate:=nil;
    inherited defineAndEnqueueOrEvaluate(recycler);
  end;

PROCEDURE T_eachTask.evaluate(VAR recycler:T_recycler);
  VAR indexLiteral:P_abstractIntLiteral;
  begin
    context^.beginEvaluation;
    evaluationResult.literal:=nil;
    if context^.messages^.continueEvaluation then with payloads do begin
      indexLiteral:=newIntLiteral(eachIndex);
      evaluationResult:=eachRule^.evaluateToLiteral(eachRule^.getLocation,context,@recycler,eachParameter,indexLiteral);
      disposeLiteral(indexLiteral);
    end;
    if payloads.eachParameter<>nil then disposeLiteral(payloads.eachParameter);
    context^.finalizeTaskAndDetachFromParent(@recycler);
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

