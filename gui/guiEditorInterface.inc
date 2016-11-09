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
  filename:string;
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
    system.EnterCriticalsection(taskQueueCs);
    while firstTask<>nil do begin
      n:=firstTask;
      firstTask:=firstTask^.next;
      dispose(n,destroy);
    end;
    system.LeaveCriticalsection(taskQueueCs);
    system.DoneCriticalsection(taskQueueCs);
  end;
  
PROCEDURE T_guiTaskQueue.enqueueTask(CONST t:P_guiTask);
  begin
    system.EnterCriticalsection(taskQueueCs);
    if lastTask=nil then begin
      lastTask:=t;
      firstTask:=t;
    end else begin
      lastTask^.next:=t;
      lastTask:=lastTask^.next;
    end;
    system.LeaveCriticalsection(taskQueueCs);
  end;

FUNCTION T_guiTaskQueue.executeTask:boolean;
  VAR toDo:P_guiTask;
  begin
    system.EnterCriticalsection(taskQueueCs);
    if firstTask<>nil then begin
      toDo:=firstTask;
      firstTask:=firstTask^.next;
      if toDo=lastTask then lastTask:=nil;
      toDo^.execute;
      dispose(toDo,destroy);
      result:=true;
    end else result:=false;
    system.LeaveCriticalsection(taskQueueCs);
  end;

CONSTRUCTOR T_openEditorTask.create(CONST filename_:string);
  begin
    inherited create;
    filename:=filename_;
  end;

DESTRUCTOR T_openEditorTask.destroy;
  begin
    inherited destroy;
  end;

PROCEDURE T_openEditorTask.execute;
  begin
    MnhForm.FormDropFiles(nil,filename);;
  end;
{$endif}
