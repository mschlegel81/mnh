{$ifdef include_interface}
  T_futureTaskState=(fts_pending, //set on construction
                     fts_evaluating, //set on dequeue
                     fts_ready); //set after evaluation

  P_futureTask=^T_futureTask;
  T_futureTask=object
    private
      eachIndex:longint;
      eachRule:P_subrule;
      eachParameter:P_literal;
      eachLocation:T_tokenLocation;
      scope:P_evaluationContext;
      next:P_futureTask;

      evaluationResult:P_literal;
    public
      state:T_futureTaskState;

    CONSTRUCTOR createTask(CONST expr:P_subrule; CONST location:T_tokenLocation; CONST idx:longint; CONST x:P_literal; CONST context:P_evaluationContext);
    PROCEDURE   evaluate(CONST callDepth:word; VAR context:T_evaluationContext; CONST calledFromWorkerThread:boolean);
    FUNCTION    getResultAsLiteral:P_literal;
    DESTRUCTOR  destroy;
  end;

  { T_taskQueue }

  T_taskQueue=object
    first,last:P_futureTask;
    queuedCount:longint;
    cs:system.TRTLCriticalSection;
    destructionPending:boolean;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION  enqueue(CONST expr:P_subrule; CONST location:T_tokenLocation; CONST idx:longint; CONST x:P_literal; CONST context:P_evaluationContext):P_futureTask;
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
        currentTask^.evaluate(0,tempcontext,true);
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

CONSTRUCTOR T_futureTask.createTask(CONST expr:P_subrule; CONST location:T_tokenLocation; CONST idx:longint; CONST x:P_literal; CONST context:P_evaluationContext);
  begin
    eachIndex:=idx;
    eachRule:=expr;
    eachParameter:=x;
    eachLocation:=location;
    scope:=context;
    next:=nil;
    state:=fts_pending;
    evaluationResult:=nil;
  end;

PROCEDURE T_futureTask.evaluate(CONST callDepth: word; VAR context: T_evaluationContext; CONST calledFromWorkerThread:boolean);
  VAR idxLit:P_intLiteral;
  begin
    if calledFromWorkerThread then context.adopt(scope);
    context.valueStore.scopePush(false);
    idxLit:=newIntLiteral(eachIndex);
    context.valueStore.createVariable(EACH_INDEX_IDENTIFIER,idxLit,true);
    idxLit^.unreference;
    if context.adapters^.noErrors
    then evaluationResult:=eachRule^.directEvaluateUnary(eachParameter,eachLocation,context,callDepth)
    else evaluationResult:=nil;
    context.valueStore.scopePop;
    if calledFromWorkerThread then context.adopt(nil);
    repeat state:=fts_ready until state=fts_ready;
  end;

FUNCTION T_futureTask.getResultAsLiteral: P_literal;
  VAR sleepTime:longint=0;
  begin
    while state in [fts_pending,fts_evaluating] do begin
      ThreadSwitch;
      sleep(sleepTime);
      if sleepTime<100 then inc(sleepTime);
    end;
    result:=evaluationResult;
  end;

DESTRUCTOR T_futureTask.destroy;
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

FUNCTION T_taskQueue.enqueue(CONST expr:P_subrule; CONST location:T_tokenLocation; CONST idx:longint; CONST x:P_literal; CONST context:P_evaluationContext):P_futureTask;
  begin
    new(result,createTask(expr,location,idx,x,context));
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
    if task<>nil then task^.evaluate(callDepth,context,false);
  end;

{$endif}
