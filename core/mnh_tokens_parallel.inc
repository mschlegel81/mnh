{$ifdef include_interface}


  T_evaluationTaskState=(ets_freeSlot,ets_pending,ets_evaluating,ets_done);

  P_evaluationTask=^T_evaluationTask;

  { T_evaluationTask }

  T_evaluationTask=object
    indexInTaskList:longint;
    state:T_evaluationTaskState;
    rule:P_subrule;
    param,evaluationResult:P_literal;
    CONSTRUCTOR create(const rule_: P_subrule; const param_: P_literal);
    DESTRUCTOR destroy;
    FUNCTION canAssignIndexInTaskList:boolean;
    FUNCTION evaluate(CONST callDepth:word):boolean; inline;
    FUNCTION freeReturningResult(CONST callDepth:word):P_literal;
    FUNCTION toString:ansistring;
  end;


  P_workerThread=^T_workerThread;
  T_workerThread=object
    alive:boolean;

    CONSTRUCTOR create();
    DESTRUCTOR destroy;
    PROCEDURE wakeUp;
  end;


{$endif}
{$ifdef include_implementation}
PROCEDURE initWorkerThreadsAndTasks;
  VAR i:longint;
  begin
    for i:=0 to length(evaluationTask)-1 do evaluationTask[i]:=nil;
    for i:=0 to length(workerThread)-1 do workerThread[i].create();
  end;
  
PROCEDURE finalizeWorkerThreadsAndTasks;
  VAR i:longint;
  begin
    for i:=0 to length(evaluationTask)-1 do evaluationTask[i]:=nil;
    for i:=0 to length(workerThread)-1 do workerThread[i].destroy;
  end;

{ T_evaluationTask }

constructor T_evaluationTask.create(const rule_: P_subrule; const param_: P_literal);
  begin
    indexInTaskList:=-1;
    rule:=rule_;
    param:=param_;
    state:=ets_pending;
    evaluationResult:=nil;;
  end;

destructor T_evaluationTask.destroy;
  begin
    while state=ets_evaluating do sleep(1);
    if evaluationResult<>nil then disposeLiteral(evaluationResult);
  end;

FUNCTION T_evaluationTask.canAssignIndexInTaskList:boolean;
  VAR i:longint;
  begin
    if (ThreadID<>mainThread) or (indexInTaskList<>-1) or (state<>ets_pending) then exit(false);
    for i:=0 to length(evaluationTask)-1 do if evaluationTask[i]=nil then begin
      indexInTaskList:=i;
      evaluationTask[i]:=@self;
      workerThread[i mod length(workerThread)].wakeUp;
      exit(true);
    end;
    result:=false;
  end;

FUNCTION T_evaluationTask.evaluate(const callDepth: word):boolean;
  begin
    if (errorLevel<el3_evalError) and (state=ets_pending) then begin
      repeat state:=ets_evaluating; until state=ets_evaluating;
      if param=nil then evaluationResult:=rule^.directEvaluateNullary(callDepth)
                   else evaluationResult:=rule^.directEvaluateUnary(param,callDepth);
      repeat state:=ets_done; until state=ets_done;
      result:=true;
    end else result:=false;
  end;

function T_evaluationTask.freeReturningResult(const callDepth: word): P_literal;
  begin
    if state=ets_pending then evaluate(callDepth);
    while (errorLevel<el3_evalError) and (state<>ets_done) do sleep(1);
    if indexInTaskList<>-1 then evaluationTask[indexInTaskList]:=nil;
    result:=evaluationResult;
    rule:=nil;
    param:=nil;
    evaluationResult:=nil;
  end;

FUNCTION T_evaluationTask.toString:ansistring;
  begin
    result:=rule^.toString;
    if param<>nil then result:=result+'('+param^.toString+')';
  end;

FUNCTION workerThreadThread(p:pointer):ptrint;
  CONST dieAfterIdleCount=100;
  VAR idleCount:longint=0;
      taskIdx:longint;
      anyProcessed:boolean;
  begin
    with P_workerThread(p)^ do repeat alive:=true; until alive;
    repeat
      anyProcessed:=false;
      for taskIdx:=0 to length(evaluationTask)-1 do
      if (evaluationTask[taskIdx]<>nil) and evaluationTask[taskIdx]^.evaluate(STACK_DEPTH_LIMIT+1)
      then anyProcessed:=true;

      if anyProcessed then idleCount:=0
      else begin
        sleep(1);
        inc(idleCount);
      end;
    until idleCount>dieAfterIdleCount;
    with P_workerThread(p)^ do repeat alive:=false; until not(alive);
  end;
  
CONSTRUCTOR T_workerThread.create();
  begin
    alive:=false;
  end;

DESTRUCTOR T_workerThread.destroy;
  begin
    while alive do sleep(1);
  end;

PROCEDURE T_workerThread.wakeUp;
  begin
    if not(alive) then begin
      alive:=true;
      BeginThread(@workerThreadThread,@self);
    end;
  end;

{$endif}
