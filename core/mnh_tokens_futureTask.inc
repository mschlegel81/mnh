{$ifdef include_interface}
  T_futureTaskState=(fts_pending, //set on construction
                     fts_errorOnInitialization, //set on construction
                     fts_evaluating, //set on dequeue
                     fts_ready); //set after evaluation

  P_futureTask=^T_futureTask;

  { T_futureTask }

  T_futureTask=object
    task:P_token;
    state:T_futureTaskState;
    next:P_futureTask;
    scope:P_evaluationContext;

    CONSTRUCTOR createTask(CONST expr:P_subrule; CONST x:P_literal; VAR context:T_evaluationContext);
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
    FUNCTION  enqueue(CONST expr:P_subrule; CONST x:P_literal; VAR context:T_evaluationContext):P_futureTask;
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
        tempcontext.adopt(currentTask^.scope);
        currentTask^.evaluate(0,tempcontext);
        sleepTime:=0;
      end;
    until (sleepTime>=SLEEP_TIME_TO_QUIT) or (pendingTasks.destructionPending);
    tempcontext.destroy;
    result:=0;
    InterLockedDecrement(poolThreadsRunning);
  end;

PROCEDURE ensurePoolThreads;
  begin
    if (poolThreadsRunning<MAX_NUMBER_OF_SECONDARY_WORKER_THREADS) then begin
      InterLockedIncrement(poolThreadsRunning);
      beginThread(@threadPoolThread);
    end;
  end;

PROCEDURE limitPoolThreads(CONST newLimit:longint);
  begin
    if newLimit>=0 then MAX_NUMBER_OF_SECONDARY_WORKER_THREADS:=newLimit;
    while poolThreadsRunning>MAX_NUMBER_OF_SECONDARY_WORKER_THREADS do sleep(100);
  end;

CONSTRUCTOR T_futureTask.createTask(CONST expr: P_subrule; CONST x: P_literal; VAR context:T_evaluationContext);
  VAR dummy:P_token;
      param:T_listLiteral;
  begin
    scope:=@context;
    param.create;
    param.append(x,true,context.adapters^);
    if expr^.replaces(@param,nil,task,dummy,context)
    then state:=fts_pending
    else state:=fts_errorOnInitialization;
    param.destroy;
    next:=nil;
  end;

PROCEDURE T_futureTask.evaluate(CONST callDepth: word; VAR context:T_evaluationContext);
  begin
    if context.adapters^.noErrors then begin
      reduceExpression(task,callDepth,context);
    end else begin
      context.cascadeDisposeToken(task);
      task:=nil;
    end;
    repeat state:=fts_ready until state=fts_ready;
  end;

FUNCTION T_futureTask.getResultAsLiteral(VAR context:T_evaluationContext): P_literal;
  VAR loc:T_tokenLocation;
  begin
    while state in [fts_pending,fts_evaluating] do sleep(1);
    if (context.adapters^.noErrors) then begin
      if (task<>nil) and (task^.next=nil) and (task^.tokType=tt_literal) then begin
        result:=task^.data;
        result^.rereference;
      end else begin
        result:=newErrorLiteral;
        if task<>nil then loc:=task^.location else loc:=C_nilTokenLocation;
        context.adapters^.raiseError('Cannot evaluate future task. Make sure the function can be resolved without errors, returning only a literal.',loc);
        context.adapters^.raiseError('The resulting expression is: '+tokensToString(task,maxLongint),loc);
      end;
    end else result:=newErrorLiteral;
  end;

PROCEDURE T_futureTask.free(VAR context:T_evaluationContext);
  begin
    while state=fts_evaluating do sleep(1);
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

FUNCTION T_taskQueue.enqueue(CONST expr:P_subrule; CONST x:P_literal; VAR context:T_evaluationContext):P_futureTask;
  begin
    new(result,createTask(expr,x,context));
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
      ensurePoolThreads;
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION  T_taskQueue.dequeue:P_futureTask;
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

PROCEDURE T_taskQueue.activeDeqeue(CONST callDepth:word; VAR context:T_evaluationContext);
  VAR task:P_futureTask;
  begin
    task:=dequeue;
    if task<>nil then task^.evaluate(callDepth,context);
  end;

{$endif}
