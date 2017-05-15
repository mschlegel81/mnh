UNIT mnh_contexts;
INTERFACE
USES //FPC/LCL libraries
     sysutils,
     //3rd party libraries
     EpikTimer, RegExpr,
     //my libraries
     myGenerics,{$ifndef fullVersion}mySys,{$endif}
     //MNH:
     mnh_constants, mnh_basicTypes,
     {$ifdef fullVersion}mnh_settings,{$endif}
     mnh_out_adapters,mnh_litVar,
     mnh_tokens,
     tokenStack,valueStore,
     mnh_profiling{$ifdef fullVersion},mnh_debugging{$endif};
TYPE
  T_evaluationContextOption =(eco_spawnWorker,eco_profiling,eco_createDetachedTask,eco_timing,eco_debugging,eco_beepOnError);
  T_threadContextOption     =(tco_spawnWorker,tco_profiling,tco_createDetachedTask,tco_timing,tco_debugging,tco_notifyParentOfAsyncTaskEnd);
  T_evaluationContextOptions=set of T_evaluationContextOption;
  T_threadContextOptions    =set of T_threadContextOption;
  T_evaluationContextType   =(ect_normal{$ifdef fullVersion},ect_profiling,ect_debugging{$endif},ect_silentlyRunAlone);
  T_sideEffect=(se_inputViaAsk,
                se_outputViaAdapter,
                se_sound,
                se_sleep,
                se_detaching,
                se_server,
                se_readingInternal,
                se_writingInternal,
                se_readingExternal,
                se_writingExternal,
                se_executingExternal,
                se_scriptDependent,
                se_executableDependent,
                se_versionDependent);
  T_sideEffects=set of T_sideEffect;

CONST
  C_sideEffectName:array[T_sideEffect] of string=(
                'input',
                'output',
                'sound',
                'sleep',
                'detaching',
                'server',
                'readingInternal',
                'writingInternal',
                'readingExternal',
                'writingExternal',
                'executingExternal',
                'scriptDependent',
                'executableDependent',
                'versionDependent');

  C_defaultOptions:T_evaluationContextOptions=[eco_spawnWorker,eco_createDetachedTask];
  C_equivalentOption:array[tco_spawnWorker..tco_debugging] of T_evaluationContextOption=(eco_spawnWorker,eco_profiling,eco_createDetachedTask,eco_timing,eco_debugging);

  C_allSideEffects:T_sideEffects=[low(T_sideEffect)..high(T_sideEffect)];

TYPE
  T_regexMap=specialize G_stringKeyMap<TRegExpr>;
  P_regexMap=^T_regexMap;
  P_evaluationContext=^T_evaluationContext;
  P_threadContext=^T_threadContext;
  P_taskQueue=^T_taskQueue;
  T_threadContext=object
    private
      //privileges and obligations
      options:T_threadContextOptions;
      //call stack
      parent:P_evaluationContext;
      callingContext:P_threadContext;
      {$ifdef fullVersion}
      callStack :T_callStack;
      {$endif}
      allowedSideEffects:T_sideEffects;
      CONSTRUCTOR createThreadContext(CONST parent_:P_evaluationContext; CONST outAdapters:P_adapters=nil);
      CONSTRUCTOR createWorkerContext;
    public
      regexCache:P_regexMap;
      recycler  :T_tokenRecycler;
      valueStore:P_valueStore;
      adapters  :P_adapters;
      callDepth:longint;
      DESTRUCTOR destroy;

      FUNCTION wallclockTime(CONST forceInit:boolean=false):double;
      PROCEDURE timeBaseComponent(CONST component: T_profileCategory);

      PROCEDURE doneEvaluating;
      FUNCTION getNewAsyncContext:P_threadContext;

      PROCEDURE attachWorkerContext(CONST valueScope:P_valueStore; CONST callingContext_:P_threadContext);
      PROCEDURE detachWorkerContext;

      FUNCTION enterTryStatementReturningPreviousAdapters:P_adapters;
      PROCEDURE leaveTryStatementReassumingPreviousAdapters(CONST previousAdapters: P_adapters; CONST tryBodyFailed: boolean);

      PROCEDURE raiseCannotApplyError(CONST ruleWithType:string; CONST parameters:P_listLiteral; CONST location:T_tokenLocation; CONST suffix:T_arrayOfString; CONST missingMain:boolean=false);

      {$ifdef fullVersion}
      PROCEDURE callStackPush(CONST callerLocation:T_tokenLocation; CONST callee:P_objectWithIdAndLocation);
      PROCEDURE callStackPush(CONST package:P_objectWithPath; CONST category:T_profileCategory; VAR calls:T_packageProfilingCalls);
      PROCEDURE callStackPop();
      PROCEDURE callStackPrint(CONST targetAdapters:P_adapters=nil);
      PROCEDURE callStackClear();
      FUNCTION stepping(CONST first:P_token; CONST stack:P_tokenStack):boolean; {$ifndef DEBUGMODE} inline; {$endif}
      PROCEDURE reportVariables(VAR variableReport: T_variableReport);
      {$endif}

      PROPERTY threadOptions:T_threadContextOptions read options;
      PROCEDURE reduceExpression(VAR first:P_token); inline;
      FUNCTION cascadeDisposeToLiteral(VAR p:P_token):P_literal;
      PROPERTY getParent:P_evaluationContext read parent;
      PROPERTY sideEffectWhitelist:T_sideEffects read allowedSideEffects;
      FUNCTION setAllowedSideEffectsReturningPrevious(CONST se:T_sideEffects):T_sideEffects;
  end;

  T_evaluationContext=object
    private
      wallClock:TEpikTimer;
      {$ifdef fullVersion}
      profiler:P_profiler;
      debuggingStepper:P_debuggingStepper;
      {$endif}
      timingInfo:record
        startOfProfiling:double;
        timeSpent:array[pc_importing..pc_interpretation] of double;
      end;
      detachedAsyncChildCount:longint;
      disposeAdaptersOnDestruction:boolean;
      contextAdapters:P_adapters;
      options :T_evaluationContextOptions;
      primaryThreadContext:P_threadContext;
      taskQueue:P_taskQueue;
      allowedSideEffects:T_sideEffects;
      PROCEDURE setupThreadContext(CONST context:P_threadContext);
    public
      CONSTRUCTOR create(CONST outAdapters:P_adapters);
      CONSTRUCTOR createAndResetSilentContext(CONST package:P_objectWithPath; CONST customSideEffecWhitelist:T_sideEffects);
      DESTRUCTOR destroy;
      PROCEDURE resetForEvaluation(CONST package:P_objectWithPath; CONST doProfiling,doDebugging,silentMode:boolean);
      PROCEDURE afterEvaluation;
      PROPERTY evaluationOptions:T_evaluationContextOptions read options;
      PROPERTY threadContext:P_threadContext read primaryThreadContext;
      PROPERTY adapters:P_adapters read contextAdapters;
      {$ifdef fullVersion}
      FUNCTION stepper:P_debuggingStepper;
      FUNCTION isPaused:boolean;
      {$endif}
      FUNCTION getTaskQueue:P_taskQueue;
  end;

  T_futureTaskState=(fts_pending, //set on construction
                     fts_evaluating, //set on dequeue
                     fts_ready); //set after evaluation

  P_futureTask=^T_futureTask;
  T_futureTask=object
    private
      payload:record
        eachIndex:longint;
        eachRule:P_expressionLiteral;
        eachParameter:P_literal;
        eachLocation:T_tokenLocation;
        scope:P_threadContext;
        valueScope:P_valueStore;
        evaluationResult:P_literal;
        state:T_futureTaskState;
      end;
      taskCs:TRTLCriticalSection;
      nextToEvaluate:P_futureTask;
      PROCEDURE dropEachParameter;
      PROCEDURE evaluate(VAR context:T_threadContext; CONST calledFromWorkerThread:boolean);
    public
      nextToAggregate:P_futureTask;
      CONSTRUCTOR create;
      PROCEDURE   define(CONST expr:P_expressionLiteral; CONST location:T_tokenLocation; CONST idx:longint; CONST x:P_literal; CONST context:P_threadContext; CONST values:P_valueStore);
      FUNCTION    canGetResult:boolean;
      FUNCTION    getResultAsLiteral:P_literal;
      DESTRUCTOR  destroy;
  end;

  T_taskQueue=object
    private
      first,last:P_futureTask;
      queuedCount:longint;
      cs:system.TRTLCriticalSection;
      destructionPending:boolean;
      poolThreadsRunning:longint;
    public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE enqueue(CONST task:P_futureTask; CONST context:P_threadContext);
    FUNCTION  dequeue:P_futureTask;
    PROCEDURE activeDeqeue(VAR context:T_threadContext);
    PROPERTY getQueuedCount:longint read queuedCount;
  end;

VAR reduceExpressionCallback:PROCEDURE(VAR first:P_token; VAR context:T_threadContext);
    subruleReplacesCallback :FUNCTION(CONST subrulePointer:pointer; CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_threadContext; CONST useUncurryingFallback:boolean):boolean;
{$ifndef fullVersion}
FUNCTION workerThreadCount:longint;
{$endif}
IMPLEMENTATION
VAR globalLock:TRTLCriticalSection;
{$ifndef fullVersion}
FUNCTION workerThreadCount:longint;
  begin
    result:=getNumberOfCPUs-1;
  end;
{$endif}

CONSTRUCTOR T_threadContext.createThreadContext(CONST parent_:P_evaluationContext; CONST outAdapters:P_adapters=nil);
  begin
    recycler  .create;
    new(valueStore,create);
    {$ifdef fullVersion}
    callStack .create;
    {$endif}
    parent        :=parent_;
    adapters      :=outAdapters;
    callingContext:=nil;
    regexCache    :=nil;
    allowedSideEffects:=C_allSideEffects;
    callDepth:=0;
    if adapters=nil then adapters:=parent^.adapters;
  end;

CONSTRUCTOR T_threadContext.createWorkerContext;
  begin
    recycler  .create;
    new(valueStore,create);
    {$ifdef fullVersion}
    callStack .create;
    {$endif}
    allowedSideEffects:=C_allSideEffects;
    regexCache    :=nil;
    parent        :=nil;
    adapters      :=nil;
    callingContext:=nil;
  end;

DESTRUCTOR T_threadContext.destroy;
  begin
    {$ifdef debugMode}{$ifdef fullVersion}
    if callStack.size>0 then raise Exception.create('Non-empty callstack on T_threadContext.doneEvaluating');
    {$endif}{$endif}
    dispose(valueStore,destroy);
    recycler  .destroy;
    if regexCache<>nil then dispose(regexCache,destroy);
  end;

CONSTRUCTOR T_evaluationContext.create(CONST outAdapters:P_adapters);
  begin
    wallClock:=nil;
    {$ifdef fullVersion}
    profiler:=nil;
    debuggingStepper:=nil;
    {$endif}
    taskQueue:=nil;
    contextAdapters:=outAdapters;
    disposeAdaptersOnDestruction:=false;
    detachedAsyncChildCount:=0;
    allowedSideEffects:=C_allSideEffects;
    new(primaryThreadContext,createThreadContext(@self,adapters));
  end;

CONSTRUCTOR T_evaluationContext.createAndResetSilentContext(CONST package:P_objectWithPath; CONST customSideEffecWhitelist:T_sideEffects);
  VAR tempAdapters:P_adapters;
  begin
    new(tempAdapters,create);
    create(tempAdapters);
    taskQueue:=nil;
    disposeAdaptersOnDestruction:=true;
    allowedSideEffects:=customSideEffecWhitelist;
    resetForEvaluation(package,false,false,true);
  end;

DESTRUCTOR T_evaluationContext.destroy;
  begin
    if wallClock<>nil then FreeAndNil(wallClock);
    {$ifdef fullVersion}
    if profiler<>nil then dispose(profiler,destroy);
    if debuggingStepper<>nil then dispose(debuggingStepper,destroy);
    {$endif}
    dispose(primaryThreadContext,destroy);
    if taskQueue<>nil then dispose(taskQueue,destroy);
    if disposeAdaptersOnDestruction then dispose(contextAdapters,destroy);
  end;

PROCEDURE T_evaluationContext.resetForEvaluation(CONST package:P_objectWithPath; CONST doProfiling,doDebugging,silentMode:boolean);
  VAR pc:T_profileCategory;
  begin
    //set options
    options:=[eco_spawnWorker,eco_createDetachedTask,eco_beepOnError];
    if adapters^.doShowTimingInfo then options:=options+[eco_timing];
    if doProfiling then options:=options+[eco_profiling];
    if doDebugging then options:=options+[eco_debugging]-[eco_createDetachedTask,eco_spawnWorker];
    if silentMode  then options:=options-[eco_beepOnError,eco_debugging,eco_timing,eco_profiling];
    if silentMode  then allowedSideEffects:=allowedSideEffects-[se_inputViaAsk];
    {$ifdef fullVersion}
    //prepare or dispose profiler:
    if eco_profiling in options then begin
      if profiler=nil then new(profiler,create)
                      else profiler^.clear;
    end else if profiler<>nil then begin
      dispose(profiler,destroy);
      profiler:=nil;
    end;
    //prepare or dispose stepper:
    if eco_debugging in options then begin
      if debuggingStepper=nil then new(debuggingStepper,create(adapters));
      debuggingStepper^.resetForDebugging(package);
    end else if debuggingStepper<>nil then begin
      dispose(debuggingStepper,destroy);
      debuggingStepper:=nil;
    end;

    {$endif}
    //prepare primary context
    setupThreadContext(primaryThreadContext);

    detachedAsyncChildCount:=0;
    //timing:
    if (eco_timing in options) or (eco_profiling in options) then begin
      if wallClock=nil then wallClock:=TEpikTimer.create(nil);
      with timingInfo do for pc:=low(timeSpent) to high(timeSpent) do timeSpent[pc]:=0;
      wallClock.clear;
      wallClock.start;
      timingInfo.startOfProfiling:=wallClock.elapsed;
    end;
  end;

PROCEDURE T_evaluationContext.afterEvaluation;
  PROCEDURE logTimingInfo;
    CONST CATEGORY_DESCRIPTION:array[T_profileCategory] of string=(
      'Importing time      ',
      'Tokenizing time     ',
      'Declaration time    ',
      'Interpretation time ',
      'Unaccounted for     ',
      'Total               ');

    VAR timeValue :array[T_profileCategory] of double;
        timeString:array[T_profileCategory] of string;
        cat:T_profileCategory;

        timeUnit:string;
        longest:longint=0;
        formatString:ansistring;
        timingMessage:T_arrayOfString;

    FUNCTION fmt(CONST d:double):string; begin result:=formatFloat(formatString,d); if length(result)>longest then longest:=length(result); end;
    FUNCTION fmt(CONST s:string):string; begin result:=StringOfChar(' ',longest-length(s))+s+timeUnit; end;
    begin
      timeValue[pc_total]:=wallClock.elapsed-timingInfo.startOfProfiling;
      timeValue[pc_unknown]:=timeValue[pc_total];
      for cat:=low(timingInfo.timeSpent) to high(timingInfo.timeSpent) do begin
        timeValue[cat]:=timingInfo.timeSpent[cat];
        timeValue[pc_unknown]:=timeValue[pc_unknown]-timeValue[cat];
      end;
      if timeValue[pc_total]<1 then begin
        for cat:=low(timeValue) to high(timeValue) do timeValue[cat]:=timeValue[cat]*1000;
        timeUnit:='ms';
        formatString:='0.000';
      end else begin
        timeUnit:='s';
        formatString:='0.000000';
      end;
      for cat:=low(T_profileCategory) to high(T_profileCategory) do timeString[cat]:=fmt(timeValue[cat]);
      for cat:=low(T_profileCategory) to high(T_profileCategory) do timeString[cat]:=CATEGORY_DESCRIPTION[cat]+fmt(timeString[cat]);
      timingMessage:=C_EMPTY_STRING_ARRAY;
      for cat:=low(T_profileCategory) to high(T_profileCategory) do begin
        if cat=high(T_profileCategory) then append(timingMessage,StringOfChar('-',length(timeString[cat])));
        append(timingMessage,timeString[cat]);
      end;
      adapters^.logTimingInfo(timingMessage);
    end;

  begin
    adapters^.stopEvaluation;
    while detachedAsyncChildCount>0 do begin
      ThreadSwitch;
      sleep(1);
    end;
    adapters^.logEndOfEvaluation;
    if (adapters^.doShowTimingInfo) and (wallClock<>nil) then logTimingInfo;
    {$ifdef fullVersion}
    if (eco_profiling in options) and (profiler<>nil) then profiler^.logInfo(adapters);
    {$endif}
    if (eco_beepOnError in options) and adapters^.triggersBeep then beep;
  end;

PROCEDURE T_evaluationContext.setupThreadContext(CONST context:P_threadContext);
  VAR threadOptions:T_threadContextOptions=[];
      threadOption :T_threadContextOption;
  begin
    for threadOption:=low(C_equivalentOption) to high(C_equivalentOption) do if C_equivalentOption[threadOption] in options then include(threadOptions,threadOption);
    context^.options:=threadOptions;
    context^.parent:=@self;
    {$ifdef fullVersion}
    context^.callStack.clear;
    {$endif}
    context^.valueStore^.clear;
    context^.adapters:=adapters;
    context^.callDepth:=0;
    context^.allowedSideEffects:=allowedSideEffects;
  end;

{$ifdef fullVersion}
FUNCTION T_evaluationContext.stepper:P_debuggingStepper;
  begin
    if debuggingStepper=nil then new(debuggingStepper,create(adapters));
    result:=debuggingStepper;
  end;

FUNCTION T_evaluationContext.isPaused:boolean;
  begin
    result:=(debuggingStepper<>nil) and debuggingStepper^.paused;
  end;
{$endif}

FUNCTION T_evaluationContext.getTaskQueue:P_taskQueue;
  begin
    if taskQueue=nil then begin
      enterCriticalSection(globalLock);
      if taskQueue=nil then new(taskQueue,create);
      leaveCriticalSection(globalLock);
    end;
    result:=taskQueue;
  end;

FUNCTION T_threadContext.wallclockTime(CONST forceInit:boolean=false):double;
  begin
    if parent^.wallClock=nil then begin
      if forceInit then begin
        enterCriticalSection(globalLock);
        if parent^.wallClock=nil then begin
          parent^.wallClock:=TEpikTimer.create(nil);
          parent^.wallClock.clear;
          parent^.wallClock.start;
        end;
        leaveCriticalSection(globalLock);
      end else exit(0);
    end;
    result:=parent^.wallClock.elapsed;
  end;

PROCEDURE T_threadContext.timeBaseComponent(CONST component: T_profileCategory);
  begin
    with parent^.timingInfo do timeSpent[component]:=wallclockTime-timeSpent[component];
  end;

PROCEDURE T_threadContext.doneEvaluating;
  begin
    if tco_notifyParentOfAsyncTaskEnd in options then interlockedDecrement(parent^.detachedAsyncChildCount);
    {$ifdef fullVersion}
    {$ifdef debugMode}
    if callStack.size>0 then raise Exception.create('Non-empty callstack on T_threadContext.doneEvaluating');
    {$endif}
    callStack.clear;
    {$endif}
    valueStore^.clear;
  end;

FUNCTION T_threadContext.getNewAsyncContext:P_threadContext;
  begin
    if not(tco_createDetachedTask in options) then exit(nil);
    interLockedIncrement(parent^.detachedAsyncChildCount);
    new(result,createThreadContext(nil,adapters));
    parent^.setupThreadContext(result);
    result^.options:=options+[tco_notifyParentOfAsyncTaskEnd];
  end;

PROCEDURE T_threadContext.attachWorkerContext(CONST valueScope:P_valueStore; CONST callingContext_:P_threadContext);
  begin
    callingContext:=callingContext_;
    parent        :=callingContext^.parent;
    adapters      :=callingContext^.adapters;
    options       :=callingContext^.options;
    allowedSideEffects:=callingContext^.allowedSideEffects;
    valueStore^.clear;
    valueStore^.parentStore:=valueScope;
    {$ifdef fullVersion}
    callStack.clear;
    {$endif}
    callDepth:=0;
  end;

PROCEDURE T_threadContext.detachWorkerContext;
  begin
    {$ifdef debugMode}
    if not(valueStore^.isEmpty) and (adapters^.noErrors) then raise Exception.create('valueStore must be empty on detach');
    {$endif}
    parent:=nil;
    callingContext:=nil;
    valueStore^.clear;
    valueStore^.parentStore:=nil;
    {$ifdef fullVersion}
    callStack.clear;
    {$endif}
  end;

FUNCTION T_threadContext.enterTryStatementReturningPreviousAdapters: P_adapters;
  begin
    result:=adapters;
    adapters:=result^.collectingClone;
  end;

PROCEDURE T_threadContext.leaveTryStatementReassumingPreviousAdapters(CONST previousAdapters: P_adapters; CONST tryBodyFailed: boolean);
  begin
    previousAdapters^.copyDataFromCollectingCloneDisposing(adapters,tryBodyFailed);
    adapters:=previousAdapters;
  end;

{$ifdef fullVersion}
PROCEDURE T_threadContext.callStackPush(CONST callerLocation: T_tokenLocation; CONST callee: P_objectWithIdAndLocation);
  begin
    if options*[tco_debugging,tco_profiling]=[] then exit;
    callStack.push(wallclockTime,callerLocation,callee);
  end;

PROCEDURE T_threadContext.callStackPush(CONST package:P_objectWithPath; CONST category:T_profileCategory; VAR calls:T_packageProfilingCalls);
  begin
    if options*[tco_debugging,tco_profiling]=[] then exit;
    if calls[category]=nil then new(calls[category],create(package,category));
    callStack.push(wallclockTime,calls[category]^.getLocation,calls[category]);
  end;

PROCEDURE T_threadContext.callStackPop;
  begin
    if options*[tco_debugging,tco_profiling]=[] then exit;
    callStack.pop(wallclockTime,parent^.profiler);
  end;
{$endif}
{$ifdef fullVersion}
PROCEDURE T_threadContext.reportVariables(VAR variableReport: T_variableReport);
  begin
    if callingContext<>nil then callingContext^.reportVariables(variableReport);
    valueStore^.reportVariables(variableReport);
  end;
{$endif}

PROCEDURE T_threadContext.reduceExpression(VAR first:P_token); begin reduceExpressionCallback(first,self); end;

FUNCTION T_threadContext.cascadeDisposeToLiteral(VAR p:P_token):P_literal;
  begin
    if adapters^.noErrors and (p<>nil) and (p^.tokType=tt_literal) and (p^.next=nil) then begin
      result:=P_literal(p^.data)^.rereferenced;
      recycler.disposeToken(p);
    end else begin
      result:=nil;
      recycler.cascadeDisposeToken(p);
    end;
  end;

FUNCTION T_threadContext.setAllowedSideEffectsReturningPrevious(CONST se:T_sideEffects):T_sideEffects;
  begin
    result:=allowedSideEffects;
    allowedSideEffects:=se;
  end;

{$ifdef fullVersion}
PROCEDURE T_threadContext.callStackPrint(CONST targetAdapters:P_adapters=nil);
  VAR p:P_threadContext;
      a:P_adapters;
  begin
    if options*[tco_debugging,tco_profiling]=[] then exit;
    a:=targetAdapters;
    if a=nil then a:=adapters;
    if a=nil then exit;
    p:=callingContext;
    callStack.print(a^);
    if p<>nil then p^.callStackPrint(a);
  end;

PROCEDURE T_threadContext.callStackClear;
  begin
    callStack.clear;
  end;
{$endif}

PROCEDURE T_threadContext.raiseCannotApplyError(CONST ruleWithType:string; CONST parameters:P_listLiteral; CONST location:T_tokenLocation; CONST suffix:T_arrayOfString; CONST missingMain:boolean=false);
  VAR totalMessage:T_arrayOfString;
  begin
    totalMessage:='Cannot apply '+ruleWithType+' to parameter list '+parameterListTypeString(parameters)+':  '+toParameterListString(parameters,true,100);
    if length(suffix)>0 then append(totalMessage,suffix);
    adapters^.raiseError(totalMessage,location);
    if missingMain then adapters^.logMissingMain;
  end;

{$ifdef fullVersion}
FUNCTION T_threadContext.stepping(CONST first:P_token; CONST stack:P_tokenStack):boolean; inline;
  begin
    if parent^.debuggingStepper=nil then exit(false);
    parent^.debuggingStepper^.stepping(first,stack,@callStack);
    result:=true;
  end;
{$endif}

FUNCTION threadPoolThread(p:pointer):ptrint;
  //means that 0.511 seconds have passed since the last activity
  CONST SLEEP_TIME_TO_QUIT=73;
  VAR sleepTime:longint;
      currentTask:P_futureTask;
      tempcontext:T_threadContext;
  begin
    sleepTime:=0;
    tempcontext.createWorkerContext;
    with P_evaluationContext(p)^ do begin
      tempcontext.adapters:=adapters;
      repeat
        currentTask:=taskQueue^.dequeue;
        if currentTask=nil then begin
          inc(sleepTime);
          ThreadSwitch;
          sleep(sleepTime div 5);
        end else begin
          currentTask^.evaluate(tempcontext,true);
          sleepTime:=0;
        end;
      until (sleepTime>=SLEEP_TIME_TO_QUIT) or (taskQueue^.destructionPending) or not(adapters^.noErrors);
      tempcontext.destroy;
      result:=0;
      interlockedDecrement(taskQueue^.poolThreadsRunning);
    end;
  end;

CONSTRUCTOR T_futureTask.create;
  begin;
    initCriticalSection(taskCs);
    payload.eachParameter:=nil;
  end;

PROCEDURE T_futureTask.define(CONST expr:P_expressionLiteral; CONST location:T_tokenLocation; CONST idx:longint; CONST x:P_literal; CONST context:P_threadContext; CONST values:P_valueStore);
  begin
    enterCriticalSection(taskCs);
    with payload do begin
      eachIndex       :=idx;
      eachRule        :=expr;
      eachParameter   :=x^.rereferenced;
      eachLocation    :=location;
      scope           :=context;
      valueScope      :=values;
      state           :=fts_pending;
      evaluationResult:=nil;
    end;
    nextToAggregate:=nil;
    nextToEvaluate:=nil;
    leaveCriticalSection(taskCs);
  end;
PROCEDURE T_futureTask.dropEachParameter;
  begin
    if payload.eachParameter<>nil then begin
      disposeLiteral(payload.eachParameter);
      payload.eachParameter:=nil;
    end;
  end;

PROCEDURE T_futureTask.evaluate(VAR context: T_threadContext; CONST calledFromWorkerThread:boolean);
  VAR idxLit:P_intLiteral;
  begin
    enterCriticalSection(taskCs);
    payload.state:=fts_evaluating;
    leaveCriticalSection(taskCs);
    try
      if context.adapters^.noErrors then with payload do begin
        if calledFromWorkerThread then context.attachWorkerContext(valueScope,scope);
        if (eachIndex>=0) then begin
          context.valueStore^.scopePush(false);
          idxLit:=newIntLiteral(eachIndex);
          context.valueStore^.createVariable(EACH_INDEX_IDENTIFIER,idxLit,true);
          idxLit^.unreference;
        end;
        evaluationResult:=eachRule^.evaluateToLiteral(eachLocation,@context,eachParameter);
        if (eachIndex>=0) then context.valueStore^.scopePop;
        if calledFromWorkerThread then context.detachWorkerContext;
      end;
    finally
      enterCriticalSection(taskCs);
      dropEachParameter;
      payload.state:=fts_ready;
      leaveCriticalSection(taskCs);
    end;
  end;

FUNCTION T_futureTask.canGetResult:boolean;
  begin
    enterCriticalSection(taskCs);
    result:=payload.state=fts_ready;
    leaveCriticalSection(taskCs);
  end;

FUNCTION T_futureTask.getResultAsLiteral: P_literal;
  VAR sleepTime:longint=0;
  begin
    enterCriticalSection(taskCs);
    while payload.state in [fts_pending,fts_evaluating] do begin
      leaveCriticalSection(taskCs);
      ThreadSwitch;
      sleep(sleepTime);
      if sleepTime<100 then inc(sleepTime);
      enterCriticalSection(taskCs);
    end;
    result:=payload.evaluationResult;
    leaveCriticalSection(taskCs);
  end;

DESTRUCTOR T_futureTask.destroy;
  begin
    dropEachParameter;
    doneCriticalSection(taskCs);
  end;

CONSTRUCTOR T_taskQueue.create;
  begin
    system.initCriticalSection(cs);
    destructionPending:=false;
    first:=nil;
    last:=nil;
    queuedCount:=0;
  end;

DESTRUCTOR T_taskQueue.destroy;
  begin
    while poolThreadsRunning>0 do begin
      destructionPending:=true;
      sleep(1);
      ThreadSwitch;
    end;
    system.doneCriticalSection(cs);
  end;

PROCEDURE T_taskQueue.enqueue(CONST task:P_futureTask; CONST context:P_threadContext);
  PROCEDURE ensurePoolThreads();
    begin
      if (poolThreadsRunning<workerThreadCount) then begin
        interLockedIncrement(poolThreadsRunning);
        beginThread(@threadPoolThread,context^.parent);
      end;
    end;
  begin
    system.enterCriticalSection(cs);
    if first=nil then begin
      queuedCount:=1;
      first:=task;
      last:=task;
    end else begin
      inc(queuedCount);
      last^.nextToEvaluate:=task;
      last:=task;
    end;
    ensurePoolThreads();
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_taskQueue.dequeue: P_futureTask;
  begin
    system.enterCriticalSection(cs);
    if first=nil then result:=nil
    else begin
      dec(queuedCount);
      result:=first;
      first:=first^.nextToEvaluate;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_taskQueue.activeDeqeue(VAR context: T_threadContext);
  VAR task:P_futureTask;
  begin
    task:=dequeue;
    if task<>nil then task^.evaluate(context,false);
  end;

INITIALIZATION
  initCriticalSection(globalLock);
FINALIZATION
  doneCriticalSection(globalLock);

end.
