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
      PROCEDURE execute(VAR recycler:T_recycler); virtual; abstract;
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
      PROCEDURE execute(VAR recycler:T_recycler); virtual;
    public
      CONSTRUCTOR create(CONST mainForm:T_mnhIdeForm);
      PROCEDURE evaluate(CONST provider:P_codeProvider; CONST contextType:T_evaluationContextType; CONST executeInFolder:string);
      PROCEDURE callMain(CONST provider:P_codeProvider; params: ansistring; CONST contextType:T_evaluationContextType; CONST executeInFolder:string);
      //Debugger support:
      FUNCTION isPaused:boolean;
      FUNCTION stepper:P_debuggingStepper;
      PROPERTY getStdOut:P_lazyInitializedOutAdapter read stdOutAdapter;
  end;

  T_reevaluationWithGui = object(T_standardEvaluation)
    private
      PROCEDURE execute(VAR recycler:T_recycler); virtual;
    public
      CONSTRUCTOR create();
  end;

  T_quickEvaluation = object(T_abstractEvaluation)
    private
      parentProvider:P_codeProvider;
      toEvaluate:T_arrayOfString;
      PROCEDURE execute(VAR recycler:T_recycler); virtual;
    public
      CONSTRUCTOR create(CONST quickStdout:P_eagerInitializedOutAdapter);
      FUNCTION postEvaluation(CONST parent:P_codeProvider; CONST evaluateInParent:boolean; CONST input:T_arrayOfString):boolean;
      DESTRUCTOR destroy; virtual;
  end;

  T_ideScriptEvaluation = object (T_abstractEvaluation)
    private
      utilityScriptList:T_scriptMetaArray;
      evalRequest:P_editScriptTask;
      stdOutAdapter:P_lazyInitializedOutAdapter;
      PROCEDURE execute(VAR recycler:T_recycler); virtual;
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
     profilingView;
FUNCTION newPlotAdapter      (CONST caption:string      ):P_guiPlotSystem;      begin new(result,create(caption)); end;
FUNCTION newImigAdapter      (CONST caption:string      ):P_guiImageSystem; begin new(result,create(caption)); end;
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
  begin
    haltEvaluation;
    enterCriticalSection(evaluationCs);
    package.destroy;
    globals.destroy;
    messages.destroy;
    leaveCriticalSection(evaluationCs);
    doneCriticalSection(evaluationCs);
  end;

DESTRUCTOR T_quickEvaluation.destroy;
  begin
    if (parentProvider<>nil) and (parentProvider^.disposeOnPackageDestruction) then dispose(parentProvider,destroy);
    setLength(toEvaluate,0);
    inherited destroy;
  end;

CONSTRUCTOR T_standardEvaluation.create(CONST mainForm:T_mnhIdeForm);
  VAR plot:P_guiPlotSystem;
  begin
    inherited init(ek_normal);
    stdOutAdapter:=newStdOutAdapter('Output',@isRunning,C_textMessages);
    stdOutAdapter^.parentMessages:=@messages;
    messages.addOutAdapter(stdOutAdapter,true);
    plot:=                 newPlotAdapter      ('MNH plot');
    messages.addOutAdapter(newCustomFormAdapter(           plot),true);
    messages.addOutAdapter(                                plot ,true);
    messages.addOutAdapter(newImigAdapter      ('MNH image')    ,true);
    messages.addOutAdapter(newTableAdapter     ('MNH table')    ,true);
    messages.addOutAdapter(newTreeAdapter      ('MNH tree view'),true);
    messages.addOutAdapter(newGuiEventsAdapter (mainForm)       ,true);
    messages.addOutAdapter(newProfilingAdapter (true)           ,true);
    {$ifdef debugMode}
    messages.addConsoleOutAdapter('v');
    {$endif}
  end;

CONSTRUCTOR T_quickEvaluation.create(CONST quickStdout:P_eagerInitializedOutAdapter);
  VAR plot:P_guiPlotSystem;
  begin
    inherited init(ek_quick);
    messages.addOutAdapter(quickStdout,false);
    quickStdout^.parentMessages:=@messages;
    plot:=                 newPlotAdapter      ('Quick plot') ;
    messages.addOutAdapter(newCustomFormAdapter(                  plot),true);
    messages.addOutAdapter(                                       plot ,true);
    messages.addOutAdapter(newImigAdapter      ('Quick image')         ,true);
    messages.addOutAdapter(newTableAdapter     ('Quick table')         ,true);
    messages.addOutAdapter(newTreeAdapter      ('Quick tree view')     ,true);
  end;

CONST C_messagesForwardedToOutput:T_messageTypeSet=[mt_clearConsole,mt_printdirect,mt_printline,mt_el2_warning,mt_el2_userWarning,mt_el3_evalError,mt_el3_userDefined,mt_el4_systemError];
CONSTRUCTOR T_ideScriptEvaluation.create(CONST mainForm:T_mnhIdeForm);
  begin
    inherited init(ek_editScript);
    stdOutAdapter:=newStdOutAdapter('Output (GUI script)',@isRunning,C_messagesForwardedToOutput);
    stdOutAdapter^.parentMessages:=@messages;
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
  begin
    inherited init(ek_normal);
    new(console,create(cmdLineInterpretation.verbosityString));
    messages.addOutAdapter(console,true);
    //Do not show profiling info as text; is shown as GUI component
    console^.enableMessageType(false,[mt_profile_call_info]);
    plot:=                 newPlotAdapter      ('MNH plot');
    messages.addOutAdapter(newCustomFormAdapter(           plot),true);
    messages.addOutAdapter(                                plot ,true);
    messages.addOutAdapter(newImigAdapter      ('MNH image')    ,true);
    messages.addOutAdapter(newTableAdapter     ('MNH table')    ,true);
    messages.addOutAdapter(newTreeAdapter      ('MNH tree view'),true);
    messages.addOutAdapter(newProfilingAdapter (false)          ,true);
    system.enterCriticalSection(evaluationCs);
    if cmdLineInterpretation.profilingRun
    then evalRequest.contextType:=ect_profiling
    else evalRequest.contextType:=ect_normal;
    if cmdLineInterpretation.headless
    then globals.primaryContext.setAllowedSideEffectsReturningPrevious(C_allSideEffects-[se_inputViaAsk]);
    evalRequest.callMain:=true;
    evalRequest.parameters:=cmdLineInterpretation.mainParameters;
    if getFileToInterpretFromCommandLine<>''
    then package.replaceCodeProvider(newFileCodeProvider(getFileToInterpretFromCommandLine))
    else package.replaceCodeProvider(newVirtualFileCodeProvider(CMD_LINE_PSEUDO_FILENAME,getCommandToInterpretFromCommandLine));
    state:=es_pending;
    executeInNewThread(false);
    system.leaveCriticalSection(evaluationCs);
  end;

VAR evaluationThreadsRunning:longint=0;
FUNCTION evaluationThread(p:pointer):ptrint;
  VAR recycler:T_recycler;
  begin
    with P_abstractEvaluation(p)^ do begin
      evalTime:=now;
      recycler.initRecycler;
      execute(recycler);
      recycler.cleanup;
      enterCriticalSection(evaluationCs);
      if stoppedByUser
      then state:=es_stoppedByUser
      else state:=es_finished;
      evalTime:=now-evalTime;
      messages.postSingal(mt_endOfEvaluation,C_nilTokenLocation);
      leaveCriticalSection(evaluationCs);
    end;
    interlockedDecrement(evaluationThreadsRunning);
    result:=0;
  end;

PROCEDURE T_abstractEvaluation.executeInNewThread(CONST debugging:boolean);
  begin
    enterCriticalSection(evaluationCs);
    try
      if state=es_pending then begin
        stoppedByUser:=false;
        if debugging then state:=es_debugRunning
                     else state:=es_running;
        interLockedIncrement(evaluationThreadsRunning);
        beginThread(@evaluationThread,@self);
      end;
    finally
      leaveCriticalSection(evaluationCs);
    end;
  end;

PROCEDURE T_reevaluationWithGui.execute(VAR recycler: T_recycler);
  begin
    globals.resetForEvaluation(@package,@package.reportVariables,evalRequest.contextType,evalRequest.parameters,recycler);
    if getFileToInterpretFromCommandLine=''
    then package.load(lu_forDirectExecution,globals,recycler,mainParameters,nil,nil)
    else package.load(lu_forCallingMain    ,globals,recycler,mainParameters,nil,nil);
    globals.afterEvaluation(recycler);
    messages.setExitCode;
  end;

PROCEDURE T_ideScriptEvaluation.ensureEditScripts();
  begin
    system.enterCriticalSection(evaluationCs);
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

PROCEDURE T_ideScriptEvaluation.execute(VAR recycler: T_recycler);
  PROCEDURE setupEdit;
    begin
      messages.clear();
      globals.resetForEvaluation(nil,nil,ect_normal,C_EMPTY_STRING_ARRAY,recycler);
    end;

  PROCEDURE doneEdit;
    begin
      package.finalize(globals.primaryContext,recycler);
      globals.afterEvaluation(recycler);
      if evalRequest<>nil
      then messages.postCustomMessage(evalRequest^.withSuccessFlag(messages.collectedMessageTypes*C_errorMessageTypes[3]=[]))
      else messages.postSingal(mt_guiEditScriptsLoaded,C_nilTokenLocation);
      evalRequest:=nil;
    end;

  PROCEDURE ensureEditScripts_impl();
    VAR subRule:P_subruleExpression;
        script:P_scriptMeta;
        scriptType:T_scriptType;
        isValid:boolean;
    begin
      if not(package.codeChanged) then exit;
      setupEdit;
      for script in utilityScriptList do dispose(script,destroy);
      setLength(utilityScriptList,0);
      {$ifdef debugMode} writeln(stdErr,'        DEBUG: Loading script package: ',package.getPath); {$endif}
      package.load(lu_forImport,globals,recycler,C_EMPTY_STRING_ARRAY,nil,nil);
      if globals.primaryContext.messages^.continueEvaluation then begin
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
    end;

  PROCEDURE executeEditScript_impl;
    begin
      setupEdit;
      evalRequest^.execute(globals,recycler);
      doneEdit;
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

PROCEDURE T_quickEvaluation.execute(VAR recycler: T_recycler);
  VAR lexer:T_lexer;
      stmt :T_enhancedStatement;
  begin
    if parentProvider=nil then begin
      package.replaceCodeProvider(newVirtualFileCodeProvider('<quick>',toEvaluate));
      globals.resetForEvaluation(@package,nil,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
      package.load(lu_forDirectExecution,globals,recycler,C_EMPTY_STRING_ARRAY,nil,nil);
      globals.afterEvaluation(recycler);
    end else begin
      package.replaceCodeProvider(parentProvider);
      globals.resetForEvaluation(@package,nil,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
      package.load(lu_forImport,globals,recycler,C_EMPTY_STRING_ARRAY,nil,nil);
      messages.postSingal(mt_clearConsole,C_nilTokenLocation);
      lexer.create(toEvaluate,packageTokenLocation(@package),@package);
      stmt:=lexer.getNextStatement(globals.primaryContext.messages,recycler{$ifdef fullVersion},nil{$endif});
      while (globals.primaryContext.messages^.continueEvaluation) and (stmt.firstToken<>nil) do begin
        package.interpret(stmt,lu_forDirectExecution,globals,recycler,nil,nil);
        stmt:=lexer.getNextStatement(globals.primaryContext.messages,recycler{$ifdef fullVersion},nil{$endif});
      end;
      if (stmt.firstToken<>nil) then recycler.cascadeDisposeToken(stmt.firstToken);
      lexer.destroy;
      globals.afterEvaluation(recycler);
    end;
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

PROCEDURE T_standardEvaluation.callMain(CONST provider: P_codeProvider; params: ansistring; CONST contextType: T_evaluationContextType; CONST executeInFolder:string);
  begin
    system.enterCriticalSection(evaluationCs);
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(evaluationCs);
      exit;
    end;
    try
      evalRequest.contextType:=contextType;
      evalRequest.parameters:=splitCommandLine(trim(params));
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

PROCEDURE T_standardEvaluation.execute(VAR recycler: T_recycler);
  CONST C_loadMode:array[false..true] of T_packageLoadUsecase=(lu_forDirectExecution,lu_forCallingMain);
  begin
    globals.resetForEvaluation(@package,@package.reportVariables,evalRequest.contextType,evalRequest.parameters,recycler);
    SetCurrentDir(evalRequest.folder);
    package.load(C_loadMode[evalRequest.callMain],globals,recycler,evalRequest.parameters,nil,nil);
    globals.afterEvaluation(recycler);
    package.clear(true);
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
    enterCriticalSection(evaluationCs);
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
  VAR recycler:T_recycler;
  begin
    recycler.initRecycler;
    system.enterCriticalSection(evaluationCs);
    try
      globals.primaryContext.messages^.setStopFlag;
      if eco_debugging in globals.options then globals.stepper^.haltEvaluation;
      globals.stopWorkers(recycler);
      stoppedByUser:=true;
    finally
      system.leaveCriticalSection(evaluationCs);
      recycler.cleanup;
    end;
  end;

end.
