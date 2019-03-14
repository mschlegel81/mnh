{$ifdef includeInterface}
T_runnerModel=object
  private
    lastStart:record
      mainCall:boolean;
      parameters:string;
    end;
    debugLine:record
      editor:TSynEdit;
      line:longint;
    end;
    debugMode_:boolean;
    PROCEDURE setDebugMode(CONST value:boolean);
  public
    stackTracing:boolean;
    firstCallAfterActivation:boolean;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION areEditorsLocked:boolean;
    PROPERTY debugMode:boolean read debugMode_ write setDebugMode;
    FUNCTION canRun(CONST quickMode:boolean=false):boolean;
    PROCEDURE customRun(CONST mainCall,profiling:boolean; CONST mainParameters:string='');
    PROCEDURE runExternally(CONST mainParameters:string='');
    PROCEDURE rerun(CONST profiling:boolean);
    PROCEDURE InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
    PROCEDURE doDebuggerAction(CONST newState:T_debuggerState);
    PROCEDURE markDebugLine(CONST editor:TSynEdit; CONST line:longint);
    PROCEDURE haltEvaluation;
  end;
{$endif}
{$ifdef includeImplementation}
PROCEDURE T_runnerModel.setDebugMode(CONST value: boolean);
  begin
    if value=debugMode_ then exit;
    debugMode_:=value;
    if (runEvaluator.evaluationRunning) and not(runEvaluator.getRunnerStateInfo.state=es_editRunning) then runEvaluator.haltEvaluation;
  end;

CONSTRUCTOR T_runnerModel.create;
  begin
    debugMode_:=false;
    stackTracing:=false;
    with lastStart do begin mainCall:=false; parameters:=''; end;
  end;

DESTRUCTOR T_runnerModel.destroy;
  begin

  end;

FUNCTION T_runnerModel.areEditorsLocked: boolean;
  begin
    result:=(debugMode_ and runEvaluator.evaluationRunning) or (runEvaluator.getRunnerStateInfo.state=es_editRunning);
  end;

FUNCTION T_runnerModel.canRun(CONST quickMode:boolean=false): boolean;
  VAR meta:P_editorMeta;
  begin
    if runEvaluator.evaluationRunning then exit(false);
    if quickMode then exit(true);
    meta:=workspace.currentEditor;
    result:=(meta<>nil) and (meta^.language=LANG_MNH);
  end;

PROCEDURE T_runnerModel.customRun(CONST mainCall, profiling: boolean; CONST mainParameters: string);
  VAR m:P_editorMeta;
      contextType:T_evaluationContextType;
  begin
    if not(canRun) then exit;
    //adapter reset
    guiAdapters.clear;
    guiOutAdapter.flushClear;
    plotSystem.resetOnEvaluationStart(false);
    guiOutAdapter.flushToGui(false);
    //dynamic forms reset
    resetPlot(mainCall);
    resetTableForms;
    resetTreeForms;
    //environment and options
    if debugMode then begin
      workspace.updateEditorsByGuiStatus;
      runEvaluator.globals.stepper^.setBreakpoints(workspace.getAllBreakpoints);
    end;
    if profiling then begin
      if debugMode_ then contextType:=ect_debuggingAndProfiling
                    else contextType:=ect_profiling;
    end else begin
      if debugMode_        then contextType:=ect_debugging
      else if stackTracing then contextType:=ect_stackTracing
                           else contextType:=ect_normal;
    end;

    if mainCall then runEvaluator.callMain(workspace.currentEditor,mainParameters,contextType,firstCallAfterActivation)
                else runEvaluator.evaluate(workspace.currentEditor,               contextType,firstCallAfterActivation);
    firstCallAfterActivation:=false;
    lastStart.mainCall:=mainCall;
    lastStart.parameters:=mainParameters;
  end;

PROCEDURE T_runnerModel.runExternally(CONST mainParameters:string='');
  VAR callParameters:T_arrayOfString;
      flag:T_cmdLineFlag;
      executor:string='';
      folder:string;
  begin
    callParameters:=FLAG_PAUSE_ALWAYS;
    with settings.externalRunOptions do begin
      for flag in flags do
        append(callParameters,FLAG_TEXT[flag]);
      if verbosity<>'' then append(callParameters,'-v'+verbosity);
      if customFolder=''
      then folder:=ExtractFileDir(workspace.currentEditor^.getPath)
      else folder:=customFolder;
      if callLightFlavour then executor:=settings.lightFlavourLocation
                          else executor:=paramStr(0);
    end;

    append(callParameters,workspace.currentEditor^.getPath);
    append(callParameters,splitCommandLine(trim(mainParameters)));
    runCommandAsyncOrPipeless(executor,callParameters,true,folder);
  end;

PROCEDURE T_runnerModel.rerun(CONST profiling:boolean);
  begin
    customRun(lastStart.mainCall,profiling,lastStart.parameters);
  end;

PROCEDURE T_runnerModel.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
    Special:=runEvaluator.globals.isPaused and runEvaluator.evaluationRunning and (Sender=debugLine.editor) and (line=debugLine.line);
  end;

PROCEDURE T_runnerModel.doDebuggerAction(CONST newState: T_debuggerState);
  VAR meta:P_editorMeta;
  begin
    runEvaluator.globals.stepper^.setState(newState);
    mainForm.onDebuggerEvent;
    meta:=workspace.currentEditor;
    if meta<>nil then with meta^ do begin
      editor.Gutter.MarksPart.visible:=true;
      editor.readonly:=areEditorsLocked or strictlyReadOnly;
    end;
  end;

PROCEDURE T_runnerModel.markDebugLine(CONST editor:TSynEdit; CONST line:longint);
  begin
    debugLine.editor:=editor;
    debugLine.line  :=line;
  end;

PROCEDURE T_runnerModel.haltEvaluation;
  begin
    runEvaluator.haltEvaluation;
    if debugMode_ then runEvaluator.globals.stepper^.haltEvaluation;
  end;
{$endif}
