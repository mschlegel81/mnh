{$ifdef include_interface}
  T_futureTaskState=(fts_pending, //set on construction
                     fts_errorOnInitialization, //set on construction
                     fts_evaluating, //set on dequeue
                     fts_ready); //set after evaluation

  P_futureTask=^T_futureTask;
  T_futureTask=object
    eachIndex:longint;
    task:P_token;
    state:T_futureTaskState;
    next:P_futureTask;
    scope:P_evaluationContext;

    {$ifdef fullVersion}
    variableName:T_idString;
    variableValue:P_literal;
    {$endif}

    CONSTRUCTOR createTask(CONST expr:P_subrule; CONST idx:longint; CONST x:P_literal; CONST context:P_evaluationContext);
    PROCEDURE   evaluate(CONST callDepth:word; VAR context:T_evaluationContext);
    FUNCTION    getResultAsLiteral(VAR context:T_evaluationContext):P_literal;
    PROCEDURE   free(VAR context:T_evaluationContext);
    DESTRUCTOR  proFormaDestroy;
  end;

  { T_taskQueue }

  T_taskQueue=object
    first,last:P_futureTask;
    queuedCount:longint;
    cs:system.TRTLCriticalSection;
    destructionPending:boolean;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION  enqueue(CONST expr:P_subrule; CONST idx:longint; CONST x:P_literal; CONST context:P_evaluationContext):P_futureTask;
    FUNCTION  dequeue:P_futureTask;
    PROCEDURE activeDeqeue(CONST callDepth:word; VAR context:T_evaluationContext);
  end;

{$endif}
{$ifdef include_implementation}
VAR poolThreadsRunning:longint=0;

FUNCTION threadPoolThread(p:pointer):ptrint;
  //means that 0.511 seconds have passed since the last activity
  CONST SLEEP_TIME_TO_QUIT=73;
  VAR sleepTime:longint;
      currentTask:P_futureTask;
      tempcontext:T_evaluationContext;
  begin
    sleepTime:=0;
    tempcontext.createNormalContext(nil);
    repeat
      currentTask:=pendingTasks.dequeue;
      if currentTask=nil then begin
        inc(sleepTime);
        sleep(sleepTime div 5);
      end else begin
        currentTask^.evaluate(0,tempcontext);
        sleepTime:=0;
      end;
    until (sleepTime>=SLEEP_TIME_TO_QUIT) or (pendingTasks.destructionPending);
    tempcontext.destroy;
    result:=0;
    interlockedDecrement(poolThreadsRunning);
  end;

PROCEDURE ensurePoolThreads();
  begin
    if (poolThreadsRunning<workerThreadCount) {$ifdef fullVersion} and not(currentlyDebugging) {$endif} then begin
      InterLockedIncrement(poolThreadsRunning);
      beginThread(@threadPoolThread);
    end;
  end;

CONSTRUCTOR T_futureTask.createTask(CONST expr: P_subrule; CONST idx: longint; CONST x: P_literal; CONST context: P_evaluationContext);
  VAR dummy:P_token;
      param:T_listLiteral;
  begin
    scope:=context;
    eachIndex:=idx;
    param.create;
    param.append(x,true);
    if expr^.replaces(@param,nil,task,dummy,context^,false)
    then state:=fts_pending
    else state:=fts_errorOnInitialization;
    {$ifdef FULLVERSION}
    variableName:=expr^.pattern.sig[0].id;
    variableValue:=x;
    x^.rereference;
    {$endif}
    param.destroy;
    next:=nil;
  end;

PROCEDURE T_futureTask.evaluate(CONST callDepth: word; VAR context: T_evaluationContext);
  VAR idxLit:P_intLiteral;
  begin
    if (scope<>@context) then context.adopt(scope);
    context.valueStore.scopePush(false);
    idxLit:=newIntLiteral(eachIndex);
    context.valueStore.createVariable(EACH_INDEX_IDENTIFIER,idxLit,true);
    idxLit^.unreference;
    if context.adapters^.noErrors then begin
      {$ifdef FULLVERSION}
      if currentlyDebugging then context.callStackPushInEach(task^.location,variableValue,variableName);
      disposeLiteral(variableValue);
      {$endif}
      reduceExpression(task,callDepth,context);
      {$ifdef FULLVERSION}
      if currentlyDebugging then context.callStackPop();
      {$endif}
    end else begin
      context.cascadeDisposeToken(task);
      task:=nil;
    end;
    context.valueStore.scopePop;
    if (scope<>@context) then context.adopt(nil);
    repeat state:=fts_ready until state=fts_ready;
  end;

FUNCTION T_futureTask.getResultAsLiteral(VAR context: T_evaluationContext): P_literal;
  VAR loc:T_tokenLocation;
      sleepTime:longint=0;
  begin
    while state in [fts_pending,fts_evaluating] do begin
      ThreadSwitch;
      sleep(sleepTime);
      if sleepTime<100 then inc(sleepTime);
    end;
    if (context.adapters^.noErrors) then begin
      if (task<>nil) and (task^.next=nil) and (task^.tokType=tt_literal) then begin
        result:=task^.data;
        result^.rereference;
      end else begin
        result:=newErrorLiteral;
        if task<>nil then loc:=task^.location else begin
          loc.package:=nil;
          loc.column:=0;
          loc.line:=0;
        end;
        context.adapters^.raiseError('Cannot evaluate future task. Make sure the function can be resolved without errors, returning only a literal.',loc);
        context.adapters^.raiseError('The resulting expression is: '+tokensToString(task,50),loc);
      end;
    end else result:=newErrorLiteral;
    context.cascadeDisposeToken(task);
  end;

PROCEDURE T_futureTask.free(VAR context: T_evaluationContext);
  begin
    while state=fts_evaluating do begin ThreadSwitch; sleep(1); end;
    context.cascadeDisposeToken(task);
  end;

DESTRUCTOR T_futureTask.proFormaDestroy;
  begin
  end;


CONSTRUCTOR T_taskQueue.create;
  begin
    system.initCriticalSection(cs);
    destructionPending:=false;
    first:=nil;
    last:=nil;
    queuedCount:=0;
  end;

DESTRUCTOR T_taskQueue.destroy;
  begin
    while poolThreadsRunning>0 do begin
      destructionPending:=true;
      sleep(5);
    end;
    system.doneCriticalSection(cs);
  end;

FUNCTION T_taskQueue.enqueue(CONST expr: P_subrule; CONST idx: longint; CONST x: P_literal; CONST context: P_evaluationContext): P_futureTask;
  begin
    new(result,createTask(expr,idx,x,context));
    if result^.state=fts_pending then begin
      system.enterCriticalSection(cs);
      if first=nil then begin
        queuedCount:=1;
        first:=result;
        last:=result;
      end else begin
        inc(queuedCount);
        last^.next:=result;
        last:=result;
      end;
      ensurePoolThreads();
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_taskQueue.dequeue: P_futureTask;
  begin
    system.enterCriticalSection(cs);
    if first=nil then result:=nil
    else begin
      dec(queuedCount);
      result:=first;
      first:=first^.next;
      result^.state:=fts_evaluating;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_taskQueue.activeDeqeue(CONST callDepth: word; VAR context: T_evaluationContext);
  VAR task:P_futureTask;
  begin
    task:=dequeue;
    if task<>nil then task^.evaluate(callDepth,context);
  end;

{$endif}
