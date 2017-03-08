UNIT listProcessing;
INTERFACE
USES mnh_constants, mnh_basicTypes,
     {$ifdef fullVersion}mnh_settings,{$endif}
     mnh_litVar,valueStore,
     mnh_aggregators,mnh_contexts;

PROCEDURE processListSerial(CONST inputList:T_arrayOfLiteral; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                            CONST eachLocation:T_tokenLocation;
                            VAR context:T_threadContext);
PROCEDURE processListParallel(CONST inputList:T_arrayOfLiteral; CONST rulesList:T_expressionList; CONST aggregator:P_aggregator;
                              CONST eachLocation:T_tokenLocation;
                              VAR context:T_threadContext);
IMPLEMENTATION
PROCEDURE processListSerial(CONST inputList: T_arrayOfLiteral;
  CONST rulesList: T_expressionList; CONST aggregator: P_aggregator;
  CONST eachLocation: T_tokenLocation; VAR context: T_threadContext);
  VAR rule:P_expressionLiteral;
      eachIndex:longint;
  begin
    for eachIndex:=0 to length(inputList)-1 do if context.adapters^.noErrors then begin
      context.valueStore.scopePush(false);
      context.valueStore.createVariable(EACH_INDEX_IDENTIFIER,eachIndex,true);
      for rule in rulesList do if not(aggregator^.earlyAbort) then
        aggregator^.addToAggregation(
          rule^.evaluateToLiteral(eachLocation,@context,inputList[eachIndex]),
          true,
          eachLocation,
          context.adapters);
      context.valueStore.scopePop;
    end;
  end;

PROCEDURE processListParallel(CONST inputList: T_arrayOfLiteral;
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
        aggregator^.addToAggregation(toAggregate^.getResultAsLiteral,true,eachLocation,context.adapters);
        with recycling do if fill<length(dat) then begin
          dat[fill]:=toAggregate;
          inc(fill);
        end else dispose(toAggregate,destroy);
      end;
    end;

  VAR values:P_valueStore;
      taskQueue:P_taskQueue;

  FUNCTION createTask(CONST expr:P_expressionLiteral; CONST idx:longint; CONST x:P_literal):P_futureTask; inline;
    begin
      with recycling do if fill>0 then begin
        dec(fill);
        result:=dat[fill];
      end else new(result,create);
      result^.define(expr,expr^.getLocation,idx,x,@context,values);
      taskQueue^.enqueue(result,@context);
    end;

  VAR rule:P_expressionLiteral;
      eachIndex:longint;
      aimEnqueueCount:longint;
  begin
    recycling.fill:=0;
    taskQueue:=context.getParent^.getTaskQueue;
    values:=context.valueStore.readOnlyClone;
    aimEnqueueCount:=workerThreadCount*2+1;
    for eachIndex:=0 to length(inputList)-1 do if context.adapters^.noErrors then
    for rule in rulesList do if not(aggregator^.earlyAbort) then begin
      enqueueForAggregation(createTask(rule,eachIndex,inputList[eachIndex]));
      if taskQueue^.getQueuedCount>aimEnqueueCount then begin
        if not(canAggregate) then taskQueue^.activeDeqeue(context);
        //if there is not enough pending after dequeuing, increase aimEnqueueCount
        if taskQueue^.getQueuedCount<workerThreadCount then inc(aimEnqueueCount,workerThreadCount);
      end;
    end;
    while firstToAggregate<>nil do if not(canAggregate) then taskQueue^.activeDeqeue(context);
    dispose(values,destroy);
    with recycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);
    end;
  end;

end.
