{$ifdef includeInterface}
T_runnerModel=object(T_serializable)
  private
    evaluation:T_standardEvaluation;
    scriptEval:T_ideScriptEvaluation;
    debugMode_:boolean;
    PROCEDURE setDebugMode(CONST value:boolean);
    FUNCTION executionDir(CONST provider:P_editorMeta):string;
  public
    persistentRunOptions:record
      preferShebang:boolean;
      mnhExecutionOptions: T_mnhExecutionOptions;
      customFolder:ansistring;
    end;
    externalRun:T_commandLineParameters;
    stackTracing:boolean;
    profiling:boolean;
    CONSTRUCTOR create(CONST mainForm:T_mnhIdeForm);
    DESTRUCTOR destroy;
    FUNCTION areEditorsLocked:boolean;
    PROPERTY debugMode:boolean read debugMode_ write setDebugMode;
    FUNCTION isMainEvaluationPaused:boolean;
    FUNCTION canRunMain:boolean;
    FUNCTION canRunScript:boolean;
    PROCEDURE customRun(CONST mainCall:boolean);
    PROCEDURE runExternally();
    PROCEDURE ensureEditScripts;
    PROCEDURE doDebuggerAction(CONST newState:T_debuggerState);
    PROCEDURE haltEvaluation;
    PROCEDURE postHalt;
    FUNCTION anyRunning(CONST considerScriptEval:boolean=true):boolean;
    PROCEDURE flushMessages;
    FUNCTION getStateLabel:string;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    FUNCTION ensureStdOutForm:T_mnhComponentForm;
  end;
{$endif}
{$ifdef includeImplementation}
PROCEDURE T_runnerModel.setDebugMode(CONST value: boolean);
  begin
    if value=debugMode_ then exit;
    debugMode_:=value;
    if evaluation.isRunning then evaluation.postHalt;
  end;

FUNCTION T_runnerModel.executionDir(CONST provider:P_editorMeta):string;
  begin
    if persistentRunOptions.customFolder=''
    then result:=provider^.getFolder
    else result:=persistentRunOptions.customFolder;
  end;

CONSTRUCTOR T_runnerModel.create(CONST mainForm:T_mnhIdeForm);
  begin
    debugMode_:=false;
    stackTracing:=false;
    persistentRunOptions.mnhExecutionOptions.create;
    externalRun.clear;
    evaluation.create(mainForm);
    scriptEval.create(mainForm);
  end;

DESTRUCTOR T_runnerModel.destroy;
  begin
    evaluation.destroy;
    scriptEval.destroy;
  end;

FUNCTION T_runnerModel.areEditorsLocked: boolean;
  begin
    result:=(debugMode_ and evaluation.isRunning) or scriptEval.isRunning;
  end;

FUNCTION T_runnerModel.isMainEvaluationPaused:boolean;
  begin
    result:=evaluation.isPaused;
  end;

FUNCTION T_runnerModel.canRunMain: boolean;
  VAR meta:P_editorMeta;
  begin
    if evaluation.isRunning then exit(false);
    meta:=workspace.currentEditor;
    result:=(meta<>nil) and (meta^.language=LANG_MNH);
  end;

FUNCTION T_runnerModel.canRunScript:boolean;
  begin
    result:=not(scriptEval.isRunning);
  end;

PROCEDURE T_runnerModel.customRun(CONST mainCall: boolean);
  VAR contextType:T_evaluationContextType;
  begin
    if not(canRunMain) then exit;
    //environment and options
    if debugMode then begin
      workspace.updateEditorsByGuiStatus;
      evaluation.stepper^.setBreakpoints(workspace.getAllBreakpoints);
    end;
    if profiling then begin
      if debugMode_ then contextType:=ect_debuggingAndProfiling
                    else contextType:=ect_profiling;
    end else begin
      if debugMode_        then contextType:=ect_debugging
      else if stackTracing then contextType:=ect_stackTracing
                           else contextType:=ect_normal;
    end;
    if workspace.autosaveBeforeEachExecution then workspace.postSaveRequest;
    evaluation.getStdOut^.outputBehavior:=ideSettings.outputBehavior;
    if mainCall then evaluation.callMain(newFixatedFileProxy(workspace.currentEditor^.getPath),externalRun.mainParameters,contextType,executionDir(workspace.currentEditor))
                else evaluation.evaluate(newFixatedFileProxy(workspace.currentEditor^.getPath),                           contextType,executionDir(workspace.currentEditor));
  end;

PROCEDURE T_runnerModel.runExternally();
  VAR callParameters:T_arrayOfString;
      pid:longint;
  begin
    externalRun.mnhExecutionOptions.flags+=[clf_PAUSE_ALWAYS];
    callParameters:=externalRun.getCommandLineArgumentsArray;
    if workspace.autosaveBeforeEachExecution then workspace.postSaveRequest;
    postIdeMessage('Starting evaluation using external executor '+externalRun.mnhExecutionOptions.executor+' '+join(callParameters,' '),false);
    runCommandAsyncOrPipeless(externalRun.mnhExecutionOptions.executor,callParameters,true,pid,executionDir(workspace.currentEditor));
  end;

PROCEDURE T_runnerModel.ensureEditScripts;
  begin
    if canRunScript then scriptEval.ensureEditScripts();
  end;

PROCEDURE T_runnerModel.doDebuggerAction(CONST newState: T_debuggerState);
  begin
    evaluation.stepper^.setState(newState);
    mainForm.onDebuggerEvent; //sets editors to readonly...
  end;

PROCEDURE T_runnerModel.haltEvaluation;
  begin
    evaluation.haltEvaluation;
    scriptEval.haltEvaluation;
  end;

PROCEDURE T_runnerModel.postHalt;
  begin
    evaluation.postHalt;
    scriptEval.postHalt;
  end;

FUNCTION T_runnerModel.anyRunning(CONST considerScriptEval:boolean=true):boolean;
  begin
    result:=evaluation.isRunning or considerScriptEval and scriptEval.isRunning;
  end;

PROCEDURE T_runnerModel.flushMessages;
  begin
    evaluation.flushMessages;
    scriptEval.flushMessages;
  end;

FUNCTION T_runnerModel.getStateLabel:string;
  begin
    result:=evaluation.stateString;
  end;

FUNCTION T_runnerModel.getSerialVersion:dword;
  begin
    result:=123237;
  end;

FUNCTION T_runnerModel.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    {$ifdef debugMode}
    writeln('Loading T_runnerModel @',stream.streamPos);
    {$endif}
    if not stream.allOkay then exit(false);
    debugMode_  :=stream.readBoolean();
    stackTracing:=stream.readBoolean();
    profiling   :=stream.readBoolean();
    with persistentRunOptions do begin
      preferShebang:=stream.readBoolean;
      mnhExecutionOptions.loadFromStream(stream);
      customFolder:=stream.readAnsiString;
    end;
    result:=stream.allOkay;
    if not(result) then begin
      debugMode_:=false;
      stackTracing:=false;
      profiling:=false;
      with persistentRunOptions do begin
        mnhExecutionOptions.clear;
        customFolder:='';
      end;
    end;
  end;

PROCEDURE T_runnerModel.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    {$ifdef debugMode}
    writeln('Saving T_runnerModel @',stream.streamPos);
    {$endif}
    stream.writeBoolean(debugMode_  );
    stream.writeBoolean(stackTracing);
    stream.writeBoolean(profiling   );
    with persistentRunOptions do begin
      stream.writeBoolean(preferShebang);
      mnhExecutionOptions.saveToStream(stream);
      stream.writeAnsiString(customFolder);
    end;
  end;

FUNCTION T_runnerModel.ensureStdOutForm:T_mnhComponentForm;
  begin
    result:=evaluation.getStdOut^.ensureOutputForm;
  end;

{$endif}
