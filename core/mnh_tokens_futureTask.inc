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
    
    CONSTRUCTOR createTask(CONST expr:P_subrule; CONST x:P_literal);
    PROCEDURE   evaluate(CONST callDepth:word);
    FUNCTION    getResultAsLiteral:P_literal;
    DESTRUCTOR  destroy;
  end;

  { T_taskQueue }

  T_taskQueue=object
    first,last:P_futureTask;
    queuedCount:longint;
    cs:system.TRTLCriticalSection;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION  enqueue(CONST expr:P_subrule; CONST x:P_literal;CONST calldepth:word):P_futureTask;
    FUNCTION  dequeue:P_futureTask;
    PROCEDURE activeDeqeue(CONST calldepth:word);
  end;

{$endif}
{$ifdef include_implementation}
VAR poolThreadsRunning:longint=0;
    idlePoolThreads:longint=0;

FUNCTION threadPoolThread(p:pointer):ptrint;
  VAR sleepTime:longint;
      currentTask:P_futureTask;
  begin
    sleepTime:=0;
    repeat
      currentTask:=pendingTasks.dequeue;
      if currentTask=nil then begin
        InterLockedIncrement(idlePoolThreads);
        inc(sleepTime);
        sleep(sleepTime);
        InterLockedDecrement(idlePoolThreads);
      end else begin
        currentTask^.evaluate(0);
        sleepTime:=0;
      end;
    until sleepTime>=10;
    result:=0;
    InterLockedDecrement(poolThreadsRunning);
  end;

PROCEDURE ensurePoolThreads;
  begin
    if (poolThreadsRunning<3) then begin
      InterLockedIncrement(poolThreadsRunning);
      BeginThread(@threadPoolThread);
    end;
  end;

constructor T_futureTask.createTask(const expr: P_subrule; const x: P_literal);
  VAR dummy:P_token;
      param:T_listLiteral;
  begin
    param.create;
    param.append(x,true);
    if expr^.replaces(@param,nil,task,dummy)
    then state:=fts_pending
    else state:=fts_errorOnInitialization;
    param.destroy;
    next:=nil;
  end;

procedure T_futureTask.evaluate(const callDepth: word);
  begin
    if errorLevel<=el3_evalError then begin
      reduceExpression(task,callDepth);
    end else begin
      cascadeDisposeToken(task);
      task:=nil;
    end;
    repeat state:=fts_ready until state=fts_ready;
  end;

function T_futureTask.getResultAsLiteral: P_literal;
  begin
    while state in [fts_pending,fts_evaluating] do sleep(1);
    if (task<>nil) and (task^.next=nil) and (task^.tokType=tt_literal) and (task^.data<>nil)
    then begin
      result:=task^.data;
      result^.rereference;
    end
    else result:=newErrorLiteral;
  end;

destructor T_futureTask.destroy;
  begin
    while state=fts_evaluating do sleep(1);
    cascadeDisposeToken(task);
  end;


CONSTRUCTOR T_taskQueue.create;
  begin
    system.InitCriticalSection(cs);
    first:=nil;
    last:=nil;
    queuedCount:=0;
  end;

DESTRUCTOR T_taskQueue.destroy;
  begin
    system.DoneCriticalsection(cs);
  end;

FUNCTION T_taskQueue.enqueue(CONST expr:P_subrule; CONST x:P_literal;CONST calldepth:word):P_futureTask;
  begin
    new(result,createTask(expr,x));
    if result^.state=fts_pending then begin
      system.EnterCriticalsection(cs);
      if first=nil then begin
        queuedCount:=1;
        first:=Result;
        last:=Result;
      end else begin
        inc(queuedCount);
        last^.next:=Result;
        last:=result;
      end;
      ensurePoolThreads;
      system.LeaveCriticalsection(cs);
    end;
  end;

FUNCTION  T_taskQueue.dequeue:P_futureTask;
  begin
    system.EnterCriticalsection(cs);
    if first=nil then result:=nil
    else begin
      dec(queuedCount);
      result:=first;
      first:=first^.next;
      result^.state:=fts_evaluating;
    end;
    system.LeaveCriticalsection(cs);
  end;

PROCEDURE T_taskQueue.activeDeqeue(CONST calldepth:word);
  VAR task:P_futureTask;
  begin
    task:=dequeue;
    if task<>nil then task^.evaluate(calldepth);
  end;

{$endif}
