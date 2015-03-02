{$ifdef include_interface}
  T_futureTaskState=(fts_pending,fts_errorOnInitialization,fts_evaluating,fts_ready);

  P_futureTask=^T_futureTask;
  T_futureTask=object
    task:P_token;
    state:T_futureTaskState;
    
    CONSTRUCTOR createTask(CONST expr:P_subrule; CONST x:P_literal);
    CONSTRUCTOR createTaskInErrorState;
    PROCEDURE   evaluate(CONST callDepth:word);
    FUNCTION    getResult_blocking(CONST callDepth:word):P_literal;
    DESTRUCTOR  destroy;
  end;
{$endif}
{$ifdef include_implementation}
FUNCTION futureTaskEvalThread(p:pointer):ptrint;
  begin
    P_futureTask(p)^.evaluate(STACK_DEPTH_LIMIT);
    result:=0;
  end;

CONSTRUCTOR T_futureTask.createTask(CONST expr:P_subrule; CONST x:P_literal);
  VAR dummy:P_token;
      param:T_listLiteral;
  begin
    param.create;
    param.append(x,true);
    if expr^.replaces(@param,task,dummy) 
    then state:=fts_pending
    else state:=fts_errorOnInitialization;
    param.destroy;
    beginThread(@futureTaskEvalThread,@self);
  end;

CONSTRUCTOR T_futureTask.createTaskInErrorState;
  begin
    state:=fts_errorOnInitialization;
    task:=nil;
  end;

PROCEDURE T_futureTask.evaluate(CONST callDepth:word);
  begin
    if state<>fts_pending then exit;
    repeat state:=fts_evaluating until state=fts_evaluating;
    reduceExpression(task,callDepth+1);
    repeat state:=fts_ready until state=fts_ready; 
  end;

FUNCTION T_futureTask.getResult_blocking(CONST callDepth:word):P_literal;
  begin
    if state=fts_pending then evaluate(callDepth);
    while state=fts_evaluating do sleep(1);
    if (task<>nil) and (task^.next=nil) and (task^.tokType=tt_literal) and (task^.data<>nil)
    then begin
      result:=task^.data;
      result^.rereference;
    end  
    else result:=newErrorLiteral;
  end;
  
DESTRUCTOR  T_futureTask.destroy;  
  begin
    while state=fts_evaluating do sleep(1);
    cascadeDisposeToken(task);
  end;

{$endif}
