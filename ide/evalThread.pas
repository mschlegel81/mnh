UNIT evalThread;
INTERFACE
USES sysutils,Classes,
     Forms,
     SynEdit,
     ideLayoutUtil,
     myGenerics,myStringUtil,
     basicTypes,fileWrappers,
     mnh_messages,
     out_adapters,
     litVar,
     funcs,contexts,
     subrules,
     packages,
     debugging,
     cmdLineInterpretation,
     recyclers,
     mnh_plotForm, mnh_plotData,
     mnh_imig_form,
     synOutAdapter,
     variableTreeViews,
     mnh_tables,
     mnhCustomForm,
     guiOutAdapters,
     editScripts,
     outputFormUnit;

TYPE
  T_evaluationKind = (ek_normal,ek_quick,ek_editScript);
  T_evaluationState= (es_pending, es_running,es_debugRunning,es_debugHalted,es_finished,es_stoppedByUser);

CONST
  C_QuickEvalPseudoPackageName='<quick>';
  C_evaluationState:array [T_evaluationState] of record
    txt: string;
    showTime: byte; //0: no, 1: yes, difference, 2: yes, absolute
  end={es_pending}     ((txt: 'Pending execution...'; showTime:0),
      {es_running}      (txt: 'Evaluating ';          showTime:1),
      {es_debugRunning} (txt: 'Debugging ';           showTime:1),
      {es_debugHalted}  (txt: 'HALTED ';              showTime:1),
      {es_finished}     (txt: 'Finished after ';      showTime:2),
      {es_stoppedByUser}(txt: 'Abortet after ';       showTime:2));
TYPE
  P_abstractEvaluation = ^T_abstractEvaluation;
  T_abstractEvaluation = object
    private
      //global
      evaluationCs:TRTLCriticalSection;
      evaluationKind:T_evaluationKind;
      state:T_evaluationState;
      stoppedByUser:boolean;

      package:T_package;
      evalTime:double;

      messages:T_guiMessagesDistributor;
      globals:T_evaluationGlobals; //needed for debugging
      PROCEDURE execute(CONST recycler:P_recycler); virtual; abstract;
    protected
      CONSTRUCTOR init(CONST kind:T_evaluationKind);
      PROCEDURE executeInNewThread(CONST debugging:boolean=false);
    public
      DESTRUCTOR destroy; virtual;
      PROCEDURE haltEvaluation;
      PROCEDURE postHalt;
      FUNCTION isRunning:boolean;
      FUNCTION flushMessages:T_messageTypeSet;
      FUNCTION stateString:string;
  end;

  T_standardEvaluation = object(T_abstractEvaluation)
    private
      evalRequest:record
        provider:P_codeProvider;
        parameters:T_arrayOfString;
        contextType:T_evaluationContextType;
        folder:string;
        callMain:boolean;
      end;
      stdOutAdapter:P_lazyInitializedOutAdapter;
      PROCEDURE execute(CONST recycler:P_recycler); virtual;
    public
      CONSTRUCTOR create(CONST mainForm:T_mnhIdeForm);
      PROCEDURE evaluate(CONST provider:P_codeProvider; CONST contextType:T_evaluationContextType; CONST executeInFolder:string);
      PROCEDURE callMain(CONST provider:P_codeProvider; CONST params: T_arrayOfString; CONST contextType:T_evaluationContextType; CONST executeInFolder:string);
      //Debugger support:
      FUNCTION isPaused:boolean;
      FUNCTION stepper:P_debuggingStepper;
      PROPERTY getStdOut:P_lazyInitializedOutAdapter read stdOutAdapter;
  end;

  T_reevaluationWithGui = object(T_standardEvaluation)
    private
      PROCEDURE execute(CONST recycler:P_recycler); virtual;
    public
      CONSTRUCTOR create();
  end;

  T_quickEvaluation = object(T_abstractEvaluation)
    private
      parentProvider:P_codeProvider;
      toEvaluate:T_arrayOfString;
      PROCEDURE execute(CONST recycler:P_recycler); virtual;
    public
      CONSTRUCTOR create(CONST quickStdout:P_eagerInitializedOutAdapter);
      FUNCTION postEvaluation(CONST parent:P_codeProvider; CONST evaluateInParent:boolean; CONST input:T_arrayOfString):boolean;
      DESTRUCTOR destroy; virtual;
      PROCEDURE postOutputMessage(CONST kind:T_messageType; CONST txt:string);
  end;

  T_ideScriptEvaluation = object (T_abstractEvaluation)
    private
      utilityScriptList:T_scriptMetaArray;
      evalRequest:P_editScriptTask;
      stdOutAdapter:P_lazyInitializedOutAdapter;
      PROCEDURE execute(CONST recycler:P_recycler); virtual;
    public
      CONSTRUCTOR create(CONST mainForm:T_mnhIdeForm);
      DESTRUCTOR destroy; virtual;
      PROCEDURE ensureEditScripts();
      PROCEDURE runUtilScript    (CONST scriptIndex:longint; CONST L:T_arrayOfString; CONST inputLang:string; CONST editorFileName:string);
      PROPERTY getScripts:T_scriptMetaArray read utilityScriptList;
  end;

CONST
  C_runningStates:set of T_evaluationState=[es_running,es_debugRunning,es_debugHalted];

IMPLEMENTATION
USES mnh_constants,
     tokenArray,
     mySys,
     profilingView,
     mnh_imig,
     mnh_settings,
     messageFormatting,
     eventsComponent;
FUNCTION newPlotAdapter      (CONST caption:string; CONST headlessMode:boolean=false):P_plotSystem ;
  begin
    if headlessMode then new(P_plotSystem   (result),create(nil,false))
                    else new(P_guiPlotSystem(result),create(caption));
  end;
FUNCTION newImigAdapter      (CONST caption:string; CONST headlessMode:boolean=false):P_imageSystem  ;
  begin
    if headlessMode then new(P_imageSystem   (result),create(nil))
                    else new(P_guiImageSystem(result),create(caption));

  end;
FUNCTION newTableAdapter     (CONST caption:string      ):P_tableAdapter;       begin new(result,create(caption)); end;
FUNCTION newTreeAdapter      (CONST caption:string      ):P_treeAdapter;        begin new(result,create(caption)); end;
FUNCTION newStdOutAdapter    (CONST caption:string; CONST running:TIsRunningFunc; CONST typesToInclude:T_messageTypeSet):P_lazyInitializedOutAdapter;        begin new(result,create(running,caption,typesToInclude)); end;
FUNCTION newCustomFormAdapter(CONST plot:P_guiPlotSystem):P_customFormAdapter;  begin new(result,createCustomFormAdapter(plot)); end;
FUNCTION newGuiEventsAdapter (CONST guiForm:T_mnhIdeForm):P_guiEventsAdapter;   begin new(result,create(guiForm)); end;
FUNCTION newProfilingAdapter (CONST ideAdapter:boolean  ):P_profileAdapter;     begin new(result,create(ideAdapter)); end;

CONSTRUCTOR T_abstractEvaluation.init(CONST kind: T_evaluationKind);
  begin
    initCriticalSection(evaluationCs);
    package.create(newVirtualFileCodeProvider('?',C_EMPTY_STRING_ARRAY),nil);
    messages.createGuiMessagesDistributor();
    globals.create(@messages);
    evaluationKind:=kind;
    evalTime:=0;
    state:=es_finished;
  end;

DESTRUCTOR T_abstractEvaluation.destroy;
  VAR timeout:double;
      enteredCs:boolean=false;
  begin
    haltEvaluation;
    timeout:=now+1/(24*60*60); //one second timeout
    while (now<timeout) and not(enteredCs) do enteredCs:=enteredCs or (tryEnterCriticalsection(evaluationCs)<>0);
    assert(enteredCs);
    package.destroy;
    globals.destroy;
    messages.destroy;
    if enteredCs then begin
      leaveCriticalSection(evaluationCs);
      doneCriticalSection(evaluationCs);
    end;
  end;

DESTRUCTOR T_quickEvaluation.destroy;
  begin
    if (parentProvider<>nil) and (parentProvider^.disposeOnPackageDestruction) then dispose(parentProvider,destroy);
    setLength(toEvaluate,0);
    inherited destroy;
  end;

PROCEDURE T_quickEvaluation.postOutputMessage(CONST kind:T_messageType; CONST txt:string);
  begin
    globals.primaryContext.messages^.postTextMessage(kind,C_nilSearchTokenLocation,txt);
  end;

CONSTRUCTOR T_standardEvaluation.create(CONST mainForm:T_mnhIdeForm);
  VAR plot:P_guiPlotSystem;
  begin
    inherited init(ek_normal);
    stdOutAdapter:=newStdOutAdapter('',@isRunning,C_textMessages);
    messages.addOutAdapter(stdOutAdapter,true);
    plot:=P_guiPlotSystem(newPlotAdapter      (''));
    messages.addOutAdapter(newCustomFormAdapter(           plot),true);
    messages.addOutAdapter(                                plot ,true);
    messages.addOutAdapter(newImigAdapter      ('MNH image')    ,true);
    messages.addOutAdapter(newTableAdapter     ('MNH table')    ,true);
    messages.addOutAdapter(newTreeAdapter      ('MNH tree view'),true);
    messages.addOutAdapter(newGuiEventsAdapter (mainForm)       ,true);
    messages.addOutAdapter(newProfilingAdapter (true)           ,true);
    {$ifdef debugMode}
    messages.addConsoleOutAdapter(C_textMessages,commandLine.getConsoleMode(),@defaultConsoleFormatter);
    {$endif}
  end;

CONSTRUCTOR T_quickEvaluation.create(CONST quickStdout:P_eagerInitializedOutAdapter);
  VAR plot:P_guiPlotSystem;
  begin
    inherited init(ek_quick);
    messages.addOutAdapter(quickStdout,false);
    plot:=P_guiPlotSystem(newPlotAdapter      ('Quick plot'));
    messages.addOutAdapter(newCustomFormAdapter(                  plot),true);
    messages.addOutAdapter(                                       plot ,true);
    messages.addOutAdapter(newImigAdapter      ('Quick image')         ,true);
    messages.addOutAdapter(newTableAdapter     ('Quick table')         ,true);
    messages.addOutAdapter(newTreeAdapter      ('Quick tree view')     ,true);
  end;

CONST C_messagesForwardedToOutput:T_messageTypeSet=[mt_clearConsole,mt_printdirect,mt_printline,mt_el2_warning,mt_el2_userWarning,mt_el3_evalError,mt_el3_trace,mt_el3_userDefined,mt_el4_systemError];
CONSTRUCTOR T_ideScriptEvaluation.create(CONST mainForm:T_mnhIdeForm);
  begin
    inherited init(ek_editScript);
    stdOutAdapter:=newStdOutAdapter('Output (GUI script)',@isRunning,C_messagesForwardedToOutput);
    messages.addOutAdapter(stdOutAdapter,true);
    messages.addOutAdapter(newGuiEventsAdapter (mainForm)              ,true);
    package.replaceCodeProvider(newFileCodeProvider(utilityScriptFileName));
    setLength(utilityScriptList,0);
  end;

DESTRUCTOR T_ideScriptEvaluation.destroy;
  VAR script:P_scriptMeta;
  begin
    for script in utilityScriptList do dispose(script,destroy);
    setLength(utilityScriptList,0);
    inherited destroy;
  end;

CONSTRUCTOR T_reevaluationWithGui.create();
  VAR plot:P_guiPlotSystem;
      console:P_redirectionAwareConsoleOutAdapter;
      defaultConsoleFormatter:P_defaultConsoleFormatter;
  begin
    inherited init(ek_normal);
    commandLine.applyAndReturnOk(@messages,true);
    if not(clf_QUIET in commandLine.mnhExecutionOptions.flags) then begin
      new(defaultConsoleFormatter,create);
      new(console,create(stringToMessageTypeSet(commandLine.mnhExecutionOptions.verbosityString),commandLine.getConsoleMode,defaultConsoleFormatter));
      dispose(defaultConsoleFormatter,destroy);
      messages.addOutAdapter(console,true);
      //Do not show profiling info as text; is shown as GUI component
      console^.enableMessageType(false,[mt_profile_call_info]);
    end;
    if (clf_HEADLESS in commandLine.mnhExecutionOptions.flags) then begin
      messages.addOutAdapter(newPlotAdapter('-',true),true);
      messages.addOutAdapter(newImigAdapter('-',true),true);
    end else begin
      plot:=P_guiPlotSystem (newPlotAdapter      ('MNH plot: '+commandLine.scriptName));
      messages.addOutAdapter(newCustomFormAdapter(           plot),true);
      messages.addOutAdapter(newTableAdapter     ('MNH table: '+commandLine.scriptName)    ,true);
      messages.addOutAdapter(newTreeAdapter      ('MNH tree view: '+commandLine.scriptName),true);
      messages.addOutAdapter(                                plot ,true);
      messages.addOutAdapter(newImigAdapter      ('MNH image'),true);
    end;
    messages.addOutAdapter(newProfilingAdapter (false)          ,true);
    system.enterCriticalSection(evaluationCs);
    try
      if (clf_PROFILE in commandLine.mnhExecutionOptions.flags)
      then evalRequest.contextType:=ect_profiling
      else evalRequest.contextType:=ect_normal;
      if (clf_HEADLESS in commandLine.mnhExecutionOptions.flags)
      then globals.primaryContext.setAllowedSideEffectsReturningPrevious(C_allSideEffects-[se_input]);
      evalRequest.callMain:=true;
      evalRequest.parameters:=commandLine.mainParameters;
      if commandLine.getFileToInterpretFromCommandLine<>''
      then package.replaceCodeProvider(newFileCodeProvider(commandLine.getFileToInterpretFromCommandLine))
      else package.replaceCodeProvider(newVirtualFileCodeProvider(CMD_LINE_PSEUDO_FILENAME,commandLine.getCommandToInterpretFromCommandLine));
      state:=es_pending;
      executeInNewThread(false);
    finally
      system.leaveCriticalSection(evaluationCs);
    end;
  end;

TYPE
  T_evaluationThread=class(T_basicThread)
    protected
      eval:P_abstractEvaluation;
      PROCEDURE execute; override;
    public
      CONSTRUCTOR create(CONST evaluation:P_abstractEvaluation);
  end;

PROCEDURE T_evaluationThread.execute;
  VAR recycler:P_recycler;
  begin
    with eval^ do begin
      evalTime:=now;
      recycler:=newRecycler;
      execute(recycler);
      freeRecycler(recycler);
      enterCriticalSection(evaluationCs);
      try
        if stoppedByUser
        then state:=es_stoppedByUser
        else state:=es_finished;
        evalTime:=now-evalTime;
        messages.postSingal(mt_endOfEvaluation,C_nilSearchTokenLocation);
      finally
        leaveCriticalSection(evaluationCs);
      end;
    end;
  end;

CONSTRUCTOR T_evaluationThread.create(CONST evaluation: P_abstractEvaluation);
  begin
    eval:=evaluation;
    inherited create();
  end;

PROCEDURE T_abstractEvaluation.executeInNewThread(CONST debugging:boolean);
  begin
    enterCriticalSection(evaluationCs);
    try
      if state=es_pending then begin
        stoppedByUser:=false;
        if debugging then state:=es_debugRunning
                     else state:=es_running;
        T_evaluationThread.create(@self);
      end;
    finally
      leaveCriticalSection(evaluationCs);
    end;
  end;

PROCEDURE T_reevaluationWithGui.execute(CONST recycler: P_recycler);
  begin
    globals.resetForEvaluation(@package,@package.reportVariables,commandLine.mnhExecutionOptions.allowedSideEffects,evalRequest.contextType,evalRequest.parameters,recycler);
    if commandLine.getFileToInterpretFromCommandLine=''
    then package.load(lu_forDirectExecution,globals,recycler,commandLine.mainParameters)
    else package.load(lu_forCallingMain    ,globals,recycler,commandLine.mainParameters);
    globals.afterEvaluation(recycler,packageTokenLocation(@package));
    messages.setExitCode;
  end;

PROCEDURE T_ideScriptEvaluation.ensureEditScripts();
  begin
    system.enterCriticalSection(evaluationCs);
    package.replaceCodeProvider(newFileCodeProvider(utilityScriptFileName));
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(evaluationCs);
      exit;
    end;
    try
      evalRequest:=nil;
      state:=es_pending;
      executeInNewThread(false);
    finally
      system.leaveCriticalSection(evaluationCs);
    end;
  end;

PROCEDURE T_ideScriptEvaluation.runUtilScript(CONST scriptIndex: longint; CONST L: T_arrayOfString; CONST inputLang: string; CONST editorFileName: string);
  begin
    system.enterCriticalSection(evaluationCs);
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(evaluationCs);
      exit;
    end;
    try
      new(evalRequest,create(utilityScriptList[scriptIndex],editorFileName,L,inputLang));
      state:=es_pending;
      executeInNewThread(false);
    finally
      system.leaveCriticalSection(evaluationCs);
    end;
  end;

PROCEDURE T_ideScriptEvaluation.execute(CONST recycler: P_recycler);
  PROCEDURE setupEdit;
    begin
      messages.clear();
      globals.resetForEvaluation(nil,nil,C_allSideEffects,ect_normal,C_EMPTY_STRING_ARRAY,recycler);
    end;

  PROCEDURE doneEdit;
    begin
      package.finalize(@globals.primaryContext,recycler,true);
      globals.afterEvaluation(recycler,packageTokenLocation(@package));
      if evalRequest<>nil
      then messages.postCustomMessage(evalRequest^.withSuccessFlag(messages.collectedMessageTypes*C_errorMessageTypes[3]=[]))
      else messages.postSingal(mt_guiEditScriptsLoaded,C_nilSearchTokenLocation);
      evalRequest:=nil;
    end;

  PROCEDURE ensureEditScripts_impl();
    VAR subRule:P_subruleExpression;
        script:P_scriptMeta;
        scriptType:T_scriptType;
        isValid:boolean;
        t0:double;
    begin
      t0:=now;
      if not(package.codeChanged) then exit;
      setupEdit;
      for script in utilityScriptList do dispose(script,destroy);
      setLength(utilityScriptList,0);
      {$ifdef debugMode} writeln(stdErr,'        DEBUG: Loading script package: ',package.getPath); {$endif}
      package.load(lu_forImport,globals,recycler,C_EMPTY_STRING_ARRAY);
      if globals.primaryContext.continueEvaluation then begin
        for scriptType in T_scriptType do
        for subRule in package.getSubrulesByAttribute(C_scriptTypeMeta[scriptType].nameAttribute) do begin
          {$ifdef debugMode} writeln(stdErr,'        DEBUG: Found script: ',subRule^.getId); {$endif}
          new(script,create(subRule,isValid,globals.primaryContext.messages));
          if isValid then begin
            setLength(utilityScriptList,length(utilityScriptList)+1);
            utilityScriptList[length(utilityScriptList)-1]:=script;
          end else dispose(script,destroy);
        end;
      end;
      doneEdit;
      postIdeMessage('Prepared custom GUI scripts in '+myTimeToStr(now-t0)+'s',false);
    end;

  PROCEDURE executeEditScript_impl;
    VAR t0:double;
    begin
      t0:=now;
      setupEdit;
      evalRequest^.execute(globals,recycler);
      doneEdit;
      postIdeMessage('Evaluation of IDE request finished in '+myTimeToStr(now-t0)+'s',false);
    end;

begin
  if evalRequest=nil
  then ensureEditScripts_impl
  else executeEditScript_impl;
end;

FUNCTION T_quickEvaluation.postEvaluation(CONST parent: P_codeProvider; CONST evaluateInParent: boolean; CONST input: T_arrayOfString):boolean;
  begin
    system.enterCriticalSection(evaluationCs);
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(evaluationCs);
      exit(false);
    end;
    try
      if evaluateInParent
      then parentProvider:=parent
      else begin
        if (parent<>nil) and (parent^.disposeOnPackageDestruction) then begin
          parentProvider:=parent;
          dispose(parentProvider,destroy);
        end;
        parentProvider:=nil;
      end;
      setLength(toEvaluate,0);
      append(toEvaluate,input);
      state:=es_pending;
      executeInNewThread();
      result:=true;
    finally
      system.leaveCriticalSection(evaluationCs);
    end;
  end;

PROCEDURE T_quickEvaluation.execute(CONST recycler: P_recycler);
  begin
    package.replaceCodeProvider(newVirtualFileCodeProvider(C_QuickEvalPseudoPackageName,toEvaluate));
    globals.resetForEvaluation(@package,nil,C_allSideEffects,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
    package.load(lu_forDirectExecution,globals,recycler,C_EMPTY_STRING_ARRAY,nil,parentProvider);
    globals.afterEvaluation(recycler,packageTokenLocation(@package));
  end;

PROCEDURE T_standardEvaluation.evaluate(CONST provider: P_codeProvider; CONST contextType: T_evaluationContextType; CONST executeInFolder:string);
  begin
    system.enterCriticalSection(evaluationCs);
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(evaluationCs);
      exit;
    end;
    try
      evalRequest.contextType:=contextType;
      evalRequest.parameters:=C_EMPTY_STRING_ARRAY;
      evalRequest.callMain:=false;
      evalRequest.folder:=executeInFolder;
      state:=es_pending;
      if provider<>package.getCodeProvider then package.clear(true);
      package.replaceCodeProvider(provider);
      executeInNewThread(contextType in [ect_debugging,ect_debuggingAndProfiling]);
    finally
      system.leaveCriticalSection(evaluationCs);
    end;
  end;

PROCEDURE T_standardEvaluation.callMain(CONST provider: P_codeProvider; CONST params: T_arrayOfString; CONST contextType: T_evaluationContextType; CONST executeInFolder:string);
  begin
    system.enterCriticalSection(evaluationCs);
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(evaluationCs);
      exit;
    end;
    try
      evalRequest.contextType:=contextType;
      evalRequest.parameters:=params;
      evalRequest.callMain:=true;
      evalRequest.folder:=executeInFolder;
      state:=es_pending;
      if provider<>package.getCodeProvider then package.clear(true);
      package.replaceCodeProvider(provider);
      executeInNewThread(contextType in [ect_debugging,ect_debuggingAndProfiling]);
    finally
      system.leaveCriticalSection(evaluationCs);
    end;
  end;

PROCEDURE T_standardEvaluation.execute(CONST recycler: P_recycler);
  CONST C_loadMode:array[false..true] of T_packageLoadUsecase=(lu_forDirectExecution,lu_forCallingMain);
  begin
    package.clear(true);
    globals.resetForEvaluation(@package,@package.reportVariables,C_allSideEffects,evalRequest.contextType,evalRequest.parameters,recycler);
    SetCurrentDir(evalRequest.folder);
    package.load(C_loadMode[evalRequest.callMain],globals,recycler,evalRequest.parameters);
    globals.afterEvaluation(recycler,packageTokenLocation(@package));
    recyclers.cleanupRecyclerPools;
  end;

FUNCTION T_abstractEvaluation.isRunning: boolean;
  begin
    enterCriticalSection(evaluationCs);
    try
      result:=state in C_runningStates;
    finally
      leaveCriticalSection(evaluationCs);
    end;
  end;

FUNCTION T_abstractEvaluation.flushMessages:T_messageTypeSet;
  begin
    if tryEnterCriticalsection(evaluationCs)<>0 then
    try
      result:=messages.flushToGui;
    finally
      leaveCriticalSection(evaluationCs);
    end;
  end;

FUNCTION T_abstractEvaluation.stateString:string;
  begin
    if (state=es_debugRunning) and      globals.isPaused  then state:=es_debugHalted else
    if (state=es_debugHalted ) and  not(globals.isPaused) then state:=es_debugRunning;
    result:=C_evaluationState[state].txt;
    case C_evaluationState[state].showTime of
      1: result+=myTimeToStr(now-evalTime,false);
      2: result+=myTimeToStr(    evalTime);
    end;
  end;

FUNCTION T_standardEvaluation.isPaused:boolean;
  begin
    enterCriticalSection(evaluationCs);
    try
      if (state=es_debugRunning) and      globals.isPaused  then state:=es_debugHalted else
      if (state=es_debugHalted ) and  not(globals.isPaused) then state:=es_debugRunning;
      result:=globals.isPaused;
    finally
      leaveCriticalSection(evaluationCs);
    end;
  end;

FUNCTION T_standardEvaluation.stepper:P_debuggingStepper;
  begin
    result:=globals.stepper;
  end;

PROCEDURE T_abstractEvaluation.haltEvaluation;
  begin
    postHalt;
    while state in C_runningStates do begin
      sleep(1); ThreadSwitch;
    end;
  end;

PROCEDURE T_abstractEvaluation.postHalt;
  VAR recycler:P_recycler;
  begin
    recycler:=newRecycler;
    system.enterCriticalSection(evaluationCs);
    try
      if eco_debugging in globals.options then globals.stepper^.haltEvaluation;
      globals.stopWorkers(recycler);
      stoppedByUser:=true;
    finally
      system.leaveCriticalSection(evaluationCs);
      freeRecycler(recycler);
    end;
  end;

end.
