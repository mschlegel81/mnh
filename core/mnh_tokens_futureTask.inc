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

    CONSTRUCTOR createTask(CONST expr:P_subrule; CONST x:P_literal; VAR recycler:T_tokenRecycler);
    PROCEDURE   evaluate(CONST callDepth:word; VAR recycler:T_tokenRecycler);
    FUNCTION    getResultAsLiteral:P_literal;
    PROCEDURE   free(VAR recycler:T_tokenRecycler);
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
    FUNCTION  enqueue(CONST expr:P_subrule; CONST x:P_literal; VAR recycler:T_tokenRecycler):P_futureTask;
    FUNCTION  dequeue:P_futureTask;
    PROCEDURE activeDeqeue(CONST callDepth:word; VAR recycler:T_tokenRecycler);
  end;

{$endif}
{$ifdef include_implementation}
VAR poolThreadsRunning:longint=0;


FUNCTION threadPoolThread(p:pointer):ptrint;
  VAR sleepTime:longint;
      currentTask:P_futureTask;
      tempRecycler:T_tokenRecycler;
  begin
    sleepTime:=0;
    tempRecycler.create;
    repeat
      currentTask:=pendingTasks.dequeue;
      if currentTask=nil then begin
        inc(sleepTime);
        sleep(sleepTime div 5);
      end else begin
        currentTask^.evaluate(0,tempRecycler);
        sleepTime:=0;
      end;
    until (sleepTime>=50) or (pendingTasks.destructionPending);
    tempRecycler.destroy;
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

CONSTRUCTOR T_futureTask.createTask(CONST expr: P_subrule; CONST x: P_literal; VAR recycler:T_tokenRecycler);
  VAR dummy:P_token;
      param:T_listLiteral;
  begin
    param.create;
    param.append(x,true);
    if expr^.replaces(@param,nil,task,dummy,recycler)
    then state:=fts_pending
    else state:=fts_errorOnInitialization;
    param.destroy;
    next:=nil;
  end;

PROCEDURE T_futureTask.evaluate(CONST callDepth: word; VAR recycler:T_tokenRecycler);
  begin
    if errorLevel<3 then begin
      reduceExpression(task,callDepth,recycler);
    end else begin
      recycler.cascadeDisposeToken(task);
      task:=nil;
    end;
    repeat state:=fts_ready until state=fts_ready;
  end;

FUNCTION T_futureTask.getResultAsLiteral: P_literal;
  begin
    while state in [fts_pending,fts_evaluating] do sleep(1);
    if (task<>nil) and (task^.next=nil) and (task^.tokType=tt_literal) and (task^.data<>nil)
    then begin
      result:=task^.data;
      result^.rereference;
    end
    else result:=newErrorLiteral;
  end;

PROCEDURE T_futureTask.free(VAR recycler:T_tokenRecycler);
  begin
    while state=fts_evaluating do sleep(1);
    recycler.cascadeDisposeToken(task);
  end;

DESTRUCTOR T_futureTask.proFormaDestroy;
  begin
  end;


CONSTRUCTOR T_taskQueue.create;
  begin
    system.InitCriticalSection(cs);
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
    system.DoneCriticalsection(cs);
  end;

FUNCTION T_taskQueue.enqueue(CONST expr:P_subrule; CONST x:P_literal; VAR recycler:T_tokenRecycler):P_futureTask;
  begin
    new(result,createTask(expr,x,recycler));
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

PROCEDURE T_taskQueue.activeDeqeue(CONST callDepth:word; VAR recycler:T_tokenRecycler);
  VAR task:P_futureTask;
  begin
    task:=dequeue;
    if task<>nil then task^.evaluate(callDepth,recycler);
  end;

{$endif}
