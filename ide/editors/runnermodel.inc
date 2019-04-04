{$ifdef includeInterface}
T_runnerModel=object(T_serializable)
  private
    evaluation:T_standardEvaluation;
    scriptEval:T_ideScriptEvaluation;

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
    externalRunOptions:record
      flags:set of T_cmdLineFlag;
      callLightFlavour:boolean;
      verbosity,customFolder:ansistring;
    end;
    stackTracing:boolean;
    profiling:boolean;
    CONSTRUCTOR create(CONST mainForm:T_mnhIdeForm; CONST stdOutAdapter:P_synOutAdapter);
    DESTRUCTOR destroy;
    FUNCTION areEditorsLocked:boolean;
    PROPERTY debugMode:boolean read debugMode_ write setDebugMode;
    FUNCTION isMainEvaluationPaused:boolean;
    FUNCTION canRun:boolean;
    PROCEDURE customRun(CONST mainCall:boolean; CONST mainParameters:string='');
    PROCEDURE runExternally(CONST mainParameters:string='');
    PROCEDURE rerun();
    PROCEDURE ensureEditScripts;
    PROCEDURE InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
    PROCEDURE doDebuggerAction(CONST newState:T_debuggerState);
    PROCEDURE markDebugLine(CONST editor:TSynEdit; CONST line:longint);
    PROCEDURE haltEvaluation;
    PROCEDURE postHalt;
    FUNCTION anyRunning(CONST considerScriptEval:boolean=true):boolean;
    PROCEDURE flushMessages;
    FUNCTION getStateLabel:string;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;
{$endif}
{$ifdef includeImplementation}
PROCEDURE T_runnerModel.setDebugMode(CONST value: boolean);
  begin
    if value=debugMode_ then exit;
    debugMode_:=value;
    if evaluation.isRunning then evaluation.postHalt;
  end;

CONSTRUCTOR T_runnerModel.create(CONST mainForm:T_mnhIdeForm; CONST stdOutAdapter:P_synOutAdapter);
  begin
    debugMode_:=false;
    stackTracing:=false;
    with lastStart do begin mainCall:=false; parameters:=''; end;

    evaluation.create(stdOutAdapter,mainForm);
    scriptEval.create(stdOutAdapter,mainForm);
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

FUNCTION T_runnerModel.canRun: boolean;
  VAR meta:P_editorMeta;
  begin
    if evaluation.isRunning or scriptEval.isRunning then exit(false);
    meta:=workspace.currentEditor;
    result:=(meta<>nil) and (meta^.language=LANG_MNH);
  end;

PROCEDURE T_runnerModel.customRun(CONST mainCall: boolean; CONST mainParameters: string);
  VAR contextType:T_evaluationContextType;
  begin
    if not(canRun) then exit;
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
    if mainCall then evaluation.callMain(workspace.currentEditor,mainParameters,contextType)
                else evaluation.evaluate(workspace.currentEditor,               contextType);
    lastStart.mainCall:=mainCall;
    lastStart.parameters:=mainParameters;
  end;

PROCEDURE T_runnerModel.runExternally(CONST mainParameters: string);
  VAR callParameters:T_arrayOfString;
      flag:T_cmdLineFlag;
      executor:string='';
      folder:string;
  begin
    callParameters:=FLAG_PAUSE_ALWAYS;
    with externalRunOptions do begin
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

PROCEDURE T_runnerModel.rerun();
  begin
    customRun(lastStart.mainCall,lastStart.parameters);
  end;

PROCEDURE T_runnerModel.ensureEditScripts;
  begin
    if canRun then scriptEval.ensureEditScripts();
  end;

PROCEDURE T_runnerModel.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
    Special:=evaluation.isPaused and evaluation.isRunning and (Sender=debugLine.editor) and (line=debugLine.line);
  end;

PROCEDURE T_runnerModel.doDebuggerAction(CONST newState: T_debuggerState);
  begin
    evaluation.stepper^.setState(newState);
    mainForm.onDebuggerEvent; //sets editors to readonly...
  end;

PROCEDURE T_runnerModel.markDebugLine(CONST editor: TSynEdit;
  CONST line: longint);
  begin
    debugLine.editor:=editor;
    debugLine.line  :=line;
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
    if not(evaluation.isRunning) then scriptEval.flushMessages;
  end;

FUNCTION T_runnerModel.getStateLabel:string;
  begin
    result:=evaluation.stateString;
  end;

FUNCTION T_runnerModel.getSerialVersion:dword;
  begin
    result:=123235;
  end;

FUNCTION T_runnerModel.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    if not stream.allOkay then exit(false);
    debugMode_  :=stream.readBoolean();
    stackTracing:=stream.readBoolean();
    profiling   :=stream.readBoolean();
    with externalRunOptions do begin
      stream.read(flags,sizeOf(flags));
      callLightFlavour:=stream.readBoolean;
      verbosity   :=stream.readAnsiString;
      customFolder:=stream.readAnsiString;
    end;
    result:=stream.allOkay;
    if not(result) then begin
      debugMode_:=false;
      stackTracing:=false;
      profiling:=false;
      with externalRunOptions do begin
        flags:=[];
        callLightFlavour:=false;
        verbosity:='';
        customFolder:='';
      end;
    end;
  end;

PROCEDURE T_runnerModel.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    stream.writeBoolean(debugMode_  );
    stream.writeBoolean(stackTracing);
    stream.writeBoolean(profiling   );
    with externalRunOptions do begin
      stream.write(flags,sizeOf(flags));
      stream.writeBoolean   (callLightFlavour);
      stream.writeAnsiString(verbosity   );
      stream.writeAnsiString(customFolder);
    end;
  end;

{$endif}
