{$ifdef includeInterface}
P_guiTask=^T_guiTask;
T_guiTask=object
  next:P_guiTask;
  CONSTRUCTOR create;
  DESTRUCTOR destroy; virtual;
  PROCEDURE execute; virtual; abstract;
end;

T_guiTaskQueue=object
  private
    taskQueueCs:TRTLCriticalSection;
    firstTask,lastTask:P_guiTask;
  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE enqueueTask(CONST t:P_guiTask);
    FUNCTION executeTask:boolean;
end;

P_openEditorTask=^T_openEditorTask;
T_openEditorTask=object(T_guiTask)
  fileName:string;
  CONSTRUCTOR create(CONST filename_:string);
  DESTRUCTOR destroy; virtual;
  PROCEDURE execute; virtual;
end;
{$endif}
{$ifdef includeImplementation}
VAR guiTaskQueue:T_guiTaskQueue;

CONSTRUCTOR T_guiTask.create; begin next:=nil; end;
DESTRUCTOR T_guiTask.destroy; begin end;

CONSTRUCTOR T_guiTaskQueue.create;
  begin
    system.initCriticalSection(taskQueueCs);
    firstTask:=nil;
    lastTask:=nil;
  end;

DESTRUCTOR T_guiTaskQueue.destroy;
  VAR n:P_guiTask;
  begin
    system.enterCriticalSection(taskQueueCs);
    while firstTask<>nil do begin
      n:=firstTask;
      firstTask:=firstTask^.next;
      dispose(n,destroy);
    end;
    system.leaveCriticalSection(taskQueueCs);
    system.doneCriticalSection(taskQueueCs);
  end;

PROCEDURE T_guiTaskQueue.enqueueTask(CONST t:P_guiTask);
  begin
    system.enterCriticalSection(taskQueueCs);
    if lastTask=nil then begin
      lastTask:=t;
      firstTask:=t;
    end else begin
      lastTask^.next:=t;
      lastTask:=lastTask^.next;
    end;
    system.leaveCriticalSection(taskQueueCs);
  end;

FUNCTION T_guiTaskQueue.executeTask:boolean;
  VAR todo:P_guiTask;
  begin
    system.enterCriticalSection(taskQueueCs);
    if firstTask<>nil then begin
      todo:=firstTask;
      firstTask:=firstTask^.next;
      if todo=lastTask then lastTask:=nil;
      todo^.execute;
      dispose(todo,destroy);
      result:=true;
    end else result:=false;
    system.leaveCriticalSection(taskQueueCs);
  end;

CONSTRUCTOR T_openEditorTask.create(CONST filename_:string);
  begin
    inherited create;
    fileName:=filename_;
  end;

DESTRUCTOR T_openEditorTask.destroy;
  begin
    inherited destroy;
  end;

PROCEDURE T_openEditorTask.execute;
  begin
    MnhForm.FormDropFiles(nil,fileName);;
  end;
{$endif}
