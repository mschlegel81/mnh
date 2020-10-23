UNIT contexts;
INTERFACE
USES //FPC/LCL libraries
     sysutils,
     //3rd party libraries
     EpikTimer,
     //my libraries
     myGenerics,mySys,myStringUtil,
     //MNH:
     mnh_constants,
     basicTypes,
     mnh_settings,
     mnh_messages,
     out_adapters,
     litVar,
     recyclers,
     tokens,
     valueStore,
     profiling{$ifdef fullVersion},tokenStack,debugging,debuggingVar{$endif};
TYPE
  T_evaluationContextOption =(eco_spawnWorker,eco_profiling,eco_createDetachedTask,eco_timing,eco_debugging,eco_stackTrace,eco_beepOnError);
  T_threadContextOption     =(tco_spawnWorker,tco_profiling,tco_createDetachedTask,tco_timing,tco_debugging,tco_stackTrace);
  T_evaluationContextOptions=set of T_evaluationContextOption;
  T_threadContextOptions    =set of T_threadContextOption;
  T_evaluationContextType   =(ect_normal{$ifdef fullVersion},ect_profiling,ect_debugging,ect_debuggingAndProfiling,ect_stackTracing{$endif},ect_silent);
  T_reduceResult            =(rr_fail,rr_ok,rr_okWithReturn);

CONST
  C_evaluationContextOptions:array[T_evaluationContextType] of T_evaluationContextOptions=(
  {ect_normal}                [eco_spawnWorker,eco_createDetachedTask,eco_beepOnError],
  {$ifdef fullVersion}
  {ect_profiling}             [eco_spawnWorker,eco_createDetachedTask,eco_beepOnError,eco_stackTrace,eco_profiling],
  {ect_debugging}             [                eco_createDetachedTask,eco_beepOnError,eco_stackTrace,              eco_debugging],
  {ect_debuggingAndProfiling} [                eco_createDetachedTask,eco_beepOnError,eco_stackTrace,eco_profiling,eco_debugging],
  {ect_stackTracing}          [eco_spawnWorker,eco_createDetachedTask,eco_beepOnError,eco_stackTrace],
  {$endif}
  {ect_silent}                [eco_spawnWorker,eco_createDetachedTask]);

  C_equivalentOption:array[tco_spawnWorker..tco_stackTrace] of T_evaluationContextOption=(eco_spawnWorker,eco_profiling,eco_createDetachedTask,eco_timing,eco_debugging,eco_stackTrace);

TYPE
  P_evaluationGlobals=^T_evaluationGlobals;
  P_context=^T_context;

  T_contextRecycler=object
    private
      recyclerCS:TRTLCriticalSection;
      contexts:array[0..127] of P_context;
      fill:longint;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE cleanup;
      PROCEDURE disposeContext(VAR context:P_context);
      FUNCTION newContext(VAR recycler:T_recycler; CONST parentThread:P_context; CONST allowAccessToParentScope:boolean):P_context;
  end;

  T_taskState=(fts_pending, //set on construction
               fts_evaluating, //set on dequeue
               fts_finished); //set after evaluation

  T_context=object(T_abstractContext)
    private
      //helper:
      contextCS:TRTLCriticalSection;
      //config:
      options:T_threadContextOptions;
      //state:
      allowedSideEffects:T_sideEffects;
      {$ifdef fullVersion}
      callStack :T_callStack;
      {$endif}
      related:record
        evaluation :P_evaluationGlobals;
        parent     :P_context;
        childCount :longint;
      end;
      CONSTRUCTOR create();
      DESTRUCTOR destroy;
      PROCEDURE dropChildContext;
      PROCEDURE addChildContext;
      PROCEDURE setThreadOptions(CONST globalOptions:T_evaluationContextOptions);
    public
      state:T_taskState;
      callDepth:longint;
      messages:P_messages;
      valueScope:P_valueScope;
      {$ifdef fullVersion}
      parentCustomForm:pointer;
      {$endif}
      //Globals
      PROPERTY getGlobals:P_evaluationGlobals read related.evaluation;
      FUNCTION wallclockTime:double;
      //Side effects
      FUNCTION checkSideEffects(CONST id:string; CONST location:T_tokenLocation; CONST functionSideEffects:T_sideEffects):boolean; inline;
      PROPERTY sideEffectWhitelist:T_sideEffects read allowedSideEffects;
      FUNCTION setAllowedSideEffectsReturningPrevious(CONST se:T_sideEffects):T_sideEffects;
      //Messaging
      PROCEDURE raiseCannotApplyError(CONST ruleWithType:string; CONST parameters:P_listLiteral; CONST location:T_tokenLocation; CONST suffix:string=''; CONST missingMain:boolean=false);
      //Evaluation:
      FUNCTION reduceExpression(VAR first:P_token; VAR recycler:T_recycler):T_reduceResult; inline;
      FUNCTION reduceToLiteral(VAR first:P_token; VAR recycler:T_recycler):T_evaluationResult; inline;
      //Multithreading:
      FUNCTION getFutureEnvironment(VAR recycler:T_recycler):P_context;
      FUNCTION getNewAsyncContext(VAR recycler:T_recycler; CONST local:boolean):P_context;
      PROCEDURE beginEvaluation;
      PROCEDURE reattachToParent(VAR recycler:T_recycler);
      PROCEDURE finalizeTaskAndDetachFromParent(CONST recyclerOrNil:P_recycler);

      //Misc.:
      PROPERTY threadOptions:T_threadContextOptions read options;
      PROCEDURE raiseError(CONST text:string; CONST location:T_searchTokenLocation; CONST kind:T_messageType=mt_el3_evalError); virtual;
      FUNCTION continueEvaluation:boolean; virtual;
      {$ifdef fullVersion}
      PROCEDURE callStackPush(CONST callerLocation:T_tokenLocation; CONST callee:P_objectWithIdAndLocation; CONST callParameters:P_variableTreeEntryCategoryNode); {$ifndef debugMode} inline; {$endif}
      PROCEDURE callStackPushCategory(CONST package:P_objectWithPath; CONST category:T_profileCategory; VAR calls:T_packageProfilingCalls); {$ifndef debugMode} inline; {$endif}
      PROCEDURE callStackPop(CONST first:P_token); {$ifndef debugMode} inline; {$endif}
      FUNCTION stepping(CONST first:P_token; CONST stack:P_tokenStack):boolean; {$ifndef debugMode} inline; {$endif}
      PROCEDURE reportVariables(VAR variableReport: T_variableTreeEntryCategoryNode);
      {$endif}
  end;

  P_queueTask=^T_queueTask;
  {T_queueTask is a task contained in a task queue. It has everything it requires for evaluation except a recycler}
  T_queueTask=object
    protected
      context:P_context;
      taskCs:TRTLCriticalSection;
      nextToEvaluate:P_queueTask;
    private
      isVolatile    :boolean;
    public
      PROCEDURE   evaluate(VAR recycler:T_recycler); virtual; abstract;
      CONSTRUCTOR create(CONST volatile:boolean; CONST newEnvironment:P_context);
      PROCEDURE   defineAndEnqueueOrEvaluate(VAR recycler:T_recycler);
      PROCEDURE   reattach(VAR recycler:T_recycler);
      DESTRUCTOR  destroy; virtual;
      PROPERTY    getContext:P_context read context;
  end;

  P_subQueue=^T_subQueue;
  {T_subqueue is a simple queue; contains a linked list of T_queueTask}
  T_subQueue=object
    private
      first,last:P_queueTask;
      queuedCount,depth:longint;
      FUNCTION  enqueue(CONST task:P_queueTask):boolean;
      FUNCTION  dequeue(OUT isEmptyAfter:boolean):P_queueTask;
    public
      CONSTRUCTOR create(CONST level:longint);
      DESTRUCTOR destroy;
      PROPERTY getQueuedCount:longint read queuedCount;
  end;

  P_taskQueue=^T_taskQueue;
  T_taskChain=object(T_subQueue)
    private
      taskQueue:P_taskQueue;
    public
      handDownThreshold:longint;
      CONSTRUCTOR create(CONST handDownThreshold_:longint; VAR myContext:T_context);
      FUNCTION enqueueOrExecute(CONST task:P_queueTask; VAR recycler:T_recycler):boolean;
      PROCEDURE flush;
  end;

  T_taskQueue=object
    private
      subQueue:array of P_subQueue;
      cs:system.TRTLCriticalSection;
      destructionPending:boolean;
      poolThreadsRunning:longint;
      FUNCTION  dequeue:P_queueTask;
      PROCEDURE cleanupQueues;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION  activeDeqeue(VAR recycler:T_recycler):boolean;
      PROCEDURE enqueue(CONST task:P_queueTask; CONST context:P_context);
  end;

  P_detachedEvaluationPart=^T_detachedEvaluationPart;
  T_detachedEvaluationPart=object
    protected
      globals:P_evaluationGlobals;
    public
      CONSTRUCTOR create(CONST evaluation:P_evaluationGlobals; CONST location:T_tokenLocation);
      DESTRUCTOR destroy; virtual;
      PROCEDURE stopOnFinalization; virtual; abstract;
  end;

  T_evaluationGlobals=object
    private
      detached:record
        access:TRTLCriticalSection;
        parts :T_setOfPointer;
        finalizing:boolean;
      end;
      wallClock:record
        timer:TEpikTimer;
        timerStartInSystime:double;
        timerCorrectionFactor:double;
        lastCorrection:double;
      end;
      globalOptions :T_evaluationContextOptions;
      globalMessages:P_messages;
      mainParameters:T_arrayOfString;
      {$ifdef fullVersion}
      profiler:P_profiler;
      debuggingStepper:P_debuggingStepper;
      {$endif}
      timingInfo:record
        startOfProfiling:double;
        timeSpent:array[pc_importing..pc_interpretation] of double;
      end;
    public
      primaryContext:T_context;
      taskQueue:T_taskQueue;
      prng:T_xosPrng;
      CONSTRUCTOR create(CONST outAdapters:P_messages);
      DESTRUCTOR destroy;
      PROCEDURE resetForEvaluation({$ifdef fullVersion}CONST package:P_objectWithPath; CONST packageVar_:F_fillCategoryNode; {$endif}
                                  CONST sideEffectProfile:T_sideEffects;
                                  CONST evaluationContextType:T_evaluationContextType; CONST mainParams:T_arrayOfString; VAR recycler:T_recycler);
      PROCEDURE startFinalization;
      PROCEDURE stopWorkers(VAR recycler:T_recycler);
      PROCEDURE afterEvaluation(VAR recycler:T_recycler);
      PROPERTY options:T_evaluationContextOptions read globalOptions;
      PROCEDURE timeBaseComponent(CONST component: T_profileCategory);

      PROCEDURE resolveMainParameter(VAR first:P_token);

      {$ifdef fullVersion}
      FUNCTION stepper:P_debuggingStepper;
      FUNCTION isPaused:boolean;
      {$endif}
  end;

VAR reduceExpressionCallback:FUNCTION(VAR first:P_token; VAR context:T_context; VAR recycler:T_recycler):T_reduceResult;
    subruleReplacesCallback :FUNCTION(CONST subrulePointer:pointer; CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT output:T_tokenRange; VAR context:T_context; VAR recycler:T_recycler):boolean;
    suppressBeep:boolean=false;
    contextPool:T_contextRecycler;
    {$ifdef fullVersion}
    postIdeMessage:PROCEDURE (CONST messageText:string; CONST warn:boolean);
    {$endif}
IMPLEMENTATION
CONSTRUCTOR T_taskChain.create(CONST handDownThreshold_: longint;
  VAR myContext: T_context);
  begin
    inherited create(0);
    handDownThreshold:=handDownThreshold_;
    taskQueue:=@(myContext.related.evaluation^.taskQueue);
  end;

FUNCTION T_taskChain.enqueueOrExecute(CONST task:P_queueTask; VAR recycler:T_recycler):boolean;
  begin
    if handDownThreshold<=0 then begin
      task^.evaluate(recycler);
      exit(true);
    end;
    inherited enqueue(task);
    if queuedCount>=handDownThreshold then begin
      flush;
      result:=true;
    end else result:=false;
  end;

PROCEDURE T_taskChain.flush;
  begin
    if first<>nil then taskQueue^.enqueue(first,first^.context);
    first:=nil;
    last :=nil;
    queuedCount:=0;
  end;

CONSTRUCTOR T_detachedEvaluationPart.create(CONST evaluation: P_evaluationGlobals; CONST location:T_tokenLocation);
  begin
    globals:=evaluation;
    enterCriticalSection(globals^.detached.access);
    try
      if globals^.detached.finalizing
      then globals^.primaryContext.raiseError('Forbidden in @after rule.',location,mt_el3_evalError)
      else globals^.detached.parts.put(@self);
    finally
      leaveCriticalSection(globals^.detached.access);
    end;
  end;

DESTRUCTOR T_detachedEvaluationPart.destroy;
  begin
    enterCriticalSection(globals^.detached.access);
    try
      globals^.detached.parts.drop(@self);
    finally
      leaveCriticalSection(globals^.detached.access);
    end;
  end;

CONSTRUCTOR T_contextRecycler.create;
  begin
    initCriticalSection(recyclerCS);
    fill:=0;
    initialize(contexts);
    memoryCleaner.registerObjectForCleanup(@cleanup);
  end;

PROCEDURE T_contextRecycler.cleanup;
  VAR k:longint;
  begin
    enterCriticalSection(recyclerCS);
    try
    for k:=0 to fill-1 do begin
      dispose(contexts[k],destroy);
    end;
    finally
      fill:=0;
      leaveCriticalSection(recyclerCS);
    end;
  end;

DESTRUCTOR T_contextRecycler.destroy;
  begin
    cleanup;
    doneCriticalSection(recyclerCS);
    memoryCleaner.unregisterObjectForCleanup(@cleanup);
  end;

PROCEDURE T_contextRecycler.disposeContext(VAR context: P_context);
  begin
    if (tryEnterCriticalsection(recyclerCS)=0)
    then dispose(context,destroy)
    else begin
      try
        if (fill>=length(contexts))
        then dispose(context,destroy)
        else begin
          contexts[fill]:=context;
          inc(fill);
        end;
      finally
        leaveCriticalSection(recyclerCS);
      end;
    end;
    context:=nil;
  end;

FUNCTION T_contextRecycler.newContext(VAR recycler:T_recycler; CONST parentThread:P_context; CONST allowAccessToParentScope:boolean):P_context;
  begin
    if (tryEnterCriticalsection(recyclerCS)<>0) then begin
      try
        if (fill>0) then begin
          dec(fill);
          result:=contexts[fill];
        end else new(result,create);
      finally
        leaveCriticalSection(recyclerCS);
      end;
    end else new(result,create);
    with result^ do begin
      options           :=parentThread^.options;
      allowedSideEffects:=parentThread^.allowedSideEffects;
      {$ifdef fullVersion}
      callStack.clear;
      {$endif}

      related.evaluation:=parentThread^.related.evaluation;
      related.parent    :=parentThread;
      related.childCount:=0;
      parentThread^.addChildContext;

      callDepth:=parentThread^.callDepth+2;
      messages:=parentThread^.messages;

      if allowAccessToParentScope
      then valueScope:=recycler.newValueScopeAsChildOf(parentThread^.valueScope)
      else valueScope:=nil;

      {$ifdef fullVersion}
      parentCustomForm:=nil;
      {$endif}
      state:=fts_pending;
    end;
  end;

CONSTRUCTOR T_evaluationGlobals.create(CONST outAdapters:P_messages);
  begin
    with detached do begin
      initCriticalSection(access);
      parts.create;
      finalizing:=false;
    end;
    globalMessages:=outAdapters;
    primaryContext.create();
    prng.create;
    prng.randomize;
    wallClock.timer:=nil;
    wallClock.timerCorrectionFactor:=1;
    {$ifdef fullVersion}
    profiler:=nil;
    debuggingStepper:=nil;
    {$endif}
    taskQueue.create;

    primaryContext.related.evaluation:=@self;
    primaryContext.related.parent:=nil;
    primaryContext.related.childCount:=0;

    primaryContext.messages:=outAdapters;
    primaryContext.allowedSideEffects:=C_allSideEffects;
    mainParameters:=C_EMPTY_STRING_ARRAY;
  end;

DESTRUCTOR T_evaluationGlobals.destroy;
  begin
    with detached do begin
      doneCriticalSection(access);
      parts.destroy;
    end;
    prng.destroy;
    if wallClock.timer<>nil then FreeAndNil(wallClock.timer);
    {$ifdef fullVersion}
    if profiler<>nil then dispose(profiler,destroy);
    profiler:=nil;
    if debuggingStepper<>nil then dispose(debuggingStepper,destroy);
    debuggingStepper:=nil;
    {$endif}
    taskQueue.destroy;
    primaryContext.destroy;
  end;

PROCEDURE T_evaluationGlobals.resetForEvaluation({$ifdef fullVersion}CONST package:P_objectWithPath; CONST packageVar_:F_fillCategoryNode; {$endif}
                                                 CONST sideEffectProfile:T_sideEffects;
                                                 CONST evaluationContextType:T_evaluationContextType; CONST mainParams:T_arrayOfString; VAR recycler:T_recycler);
  VAR pc:T_profileCategory;
      i:longint;
  begin
    detached.parts.clear;
    detached.finalizing:=false;
    setLength(mainParameters,length(mainParams));
    for i:=0 to length(mainParams)-1 do mainParameters[i]:=mainParams[i];
    //global options
    globalOptions:=C_evaluationContextOptions[evaluationContextType];
    if primaryContext.messages^.isCollecting(mt_timing_info)
    then globalOptions:=globalOptions+[eco_timing];
    {$ifdef fullVersion}
    //prepare or dispose profiler:
    if eco_profiling in globalOptions then begin
      if profiler=nil then new(profiler,create)
                      else profiler^.clear;
    end else if profiler<>nil then begin
      dispose(profiler,destroy);
      profiler:=nil;
    end;
    //prepare or dispose stepper:
    if eco_debugging in globalOptions then begin
      if debuggingStepper=nil then new(debuggingStepper,create(@primaryContext.messages));
      debuggingStepper^.resetForDebugging(package,packageVar_);
    end else if debuggingStepper<>nil then begin
      dispose(debuggingStepper,destroy);
      debuggingStepper:=nil;
    end;
    {$endif}
    //timing:
    with timingInfo do for pc:=low(timeSpent) to high(timeSpent) do timeSpent[pc]:=0;
    //Ensure there is a wallclock if it is needed
    if (wallClock.timer=nil) and ((eco_profiling in globalOptions) or (eco_timing in globalOptions))
    then wallClock.timer:=TEpikTimer.create(nil);

    if wallClock.timer<>nil then begin;
      wallClock.timer.clear;
      wallClock.timer.start;
      wallClock.timerStartInSystime:=now;
      wallClock.lastCorrection:=now;
      timingInfo.startOfProfiling:=wallClock.timer.elapsed*wallClock.timerCorrectionFactor;
    end;
    //evaluation state
    if evaluationContextType=ect_silent
    then primaryContext.allowedSideEffects:=sideEffectProfile-[se_input]
    else primaryContext.allowedSideEffects:=sideEffectProfile;
    primaryContext.setThreadOptions(globalOptions);
    recycler.disposeScope(primaryContext.valueScope);
    primaryContext.valueScope:=nil;
    primaryContext.messages^.clear();
    primaryContext.messages^.setUserDefinedExitCode(0);
    with primaryContext.related do begin
      evaluation:=@self;
      parent:=nil;
      childCount:=0;
    end;
    {$ifdef fullVersion}
    primaryContext.messages^.postSingal(mt_startOfEvaluation,C_nilTokenLocation);
    primaryContext.callStack.clear;
    primaryContext.parentCustomForm:=nil;
    {$endif}
    primaryContext.state:=fts_evaluating;
  end;

PROCEDURE T_evaluationGlobals.startFinalization;
  VAR p:pointer;
  begin
    with detached do begin
      enterCriticalSection(access);
      try
        finalizing:=true;
        for p in parts.values do P_detachedEvaluationPart(p)^.stopOnFinalization;
        while (parts.size>0) do begin
          leaveCriticalSection(access);
          sleep(1);
          ThreadSwitch;
          enterCriticalSection(access);
        end;
      finally
        leaveCriticalSection(access);
      end;
    end;
  end;

PROCEDURE T_evaluationGlobals.stopWorkers(VAR recycler: T_recycler);
  CONST TIME_OUT_AFTER=10/(24*60*60); //=10 seconds
  VAR timeout:double;
  begin
    timeout:=now+TIME_OUT_AFTER;
    primaryContext.messages^.setStopFlag;
    while (now<timeout) and ((primaryContext.related.childCount>0) or (length(taskQueue.subQueue)>0)) do begin
      while taskQueue.activeDeqeue(recycler) do begin end;
      ThreadSwitch;
      sleep(1);
    end;
  end;

PROCEDURE T_evaluationGlobals.afterEvaluation(VAR recycler:T_recycler);
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
      timeValue[pc_total]:=wallClock.timer.elapsed*wallClock.timerCorrectionFactor-timingInfo.startOfProfiling;
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
      primaryContext.messages^.postTextMessage(mt_timing_info,C_nilTokenLocation,timingMessage);
    end;

  begin
    stopWorkers(recycler);
    primaryContext.messages^.postSingal(mt_endOfEvaluation,C_nilTokenLocation);
    if (primaryContext.messages^.isCollecting(mt_timing_info)) and (wallClock.timer<>nil) then logTimingInfo;
    {$ifdef fullVersion}
    if (eco_profiling in globalOptions) and (profiler<>nil) then profiler^.logInfo(primaryContext.messages);
    {$endif}
    if not(suppressBeep) and (eco_beepOnError in globalOptions) and primaryContext.messages^.triggersBeep then beep;
    while primaryContext.valueScope<>nil do recycler.scopePop(primaryContext.valueScope);
    primaryContext.finalizeTaskAndDetachFromParent(@recycler);
  end;

{$ifdef fullVersion}
FUNCTION T_evaluationGlobals.stepper:P_debuggingStepper;
  begin
    if debuggingStepper=nil then new(debuggingStepper,create(primaryContext.messages));
    result:=debuggingStepper;
  end;

FUNCTION T_evaluationGlobals.isPaused:boolean;
  begin
    result:=(debuggingStepper<>nil) and debuggingStepper^.paused;
  end;
{$endif}

FUNCTION T_context.wallclockTime: double;
  CONST updateCorrectionInterval=10/(24*60*60); //update this every 10 seconds
  begin
    if related.evaluation^.wallClock.timer=nil then begin
      enterCriticalSection(related.evaluation^.primaryContext.contextCS);
      try
        if related.evaluation^.wallClock.timer=nil then begin
          related.evaluation^.wallClock.timer:=TEpikTimer.create(nil);
          related.evaluation^.wallClock.timer.clear;
          related.evaluation^.wallClock.timer.start;
          related.evaluation^.wallClock.timerStartInSystime:=now;
          related.evaluation^.wallClock.lastCorrection:=now;
        end;
      finally
        leaveCriticalSection(related.evaluation^.primaryContext.contextCS);
      end;
    end;
    if now-related.evaluation^.wallClock.lastCorrection>updateCorrectionInterval then begin
      related.evaluation^.wallClock.timerCorrectionFactor:=
        //|    time passed in systime, seconds                         |/|time passed according to timer|
        (now-related.evaluation^.wallClock.timerStartInSystime)*24*60*60/related.evaluation^.wallClock.timer.elapsed;
      related.evaluation^.wallClock.lastCorrection:=now;
    end;
    result:=related.evaluation^.wallClock.timer.elapsed*
            related.evaluation^.wallClock.timerCorrectionFactor;
  end;

PROCEDURE T_evaluationGlobals.timeBaseComponent(CONST component: T_profileCategory);
  begin
    with timingInfo do timeSpent[component]:=primaryContext.wallclockTime-timeSpent[component];
  end;

FUNCTION T_context.getNewAsyncContext(VAR recycler:T_recycler; CONST local: boolean): P_context;
  begin
    if not(tco_createDetachedTask in options) then exit(nil);
    result:=contextPool.newContext(recycler,@self,local);
    result^.messages:=related.evaluation^.globalMessages;
  end;

{$ifdef fullVersion}
PROCEDURE T_context.callStackPush(CONST callerLocation: T_tokenLocation;
  CONST callee: P_objectWithIdAndLocation;
  CONST callParameters: P_variableTreeEntryCategoryNode);
  begin
    if (tco_stackTrace in options) then callStack.push(wallclockTime,callParameters,callerLocation,callee);
  end;

PROCEDURE T_context.callStackPushCategory(CONST package: P_objectWithPath; CONST category: T_profileCategory; VAR calls: T_packageProfilingCalls);
  begin
    if tco_stackTrace in options then begin
      if calls[category]=nil then new(calls[category],create(package,category));
      callStack.push(wallclockTime,nil,calls[category]^.getLocation,calls[category]);
    end;
  end;

PROCEDURE T_context.callStackPop(CONST first: P_token);
  VAR loc:T_tokenLocation;
  begin
    if (tco_stackTrace in options) then begin
      loc:=callStack.pop(wallclockTime,related.evaluation^.profiler);
      if (loc.package<>nil) and (first<>nil) then first^.location:=loc;
    end;
  end;

FUNCTION T_context.stepping(CONST first: P_token; CONST stack: P_tokenStack): boolean;
  begin
    if (related.evaluation=nil) or (related.evaluation^.debuggingStepper=nil) then exit(false);
    if first<>nil then related.evaluation^.debuggingStepper^.stepping(first,stack,@callStack,@reportVariables);
    result:=true;
  end;

PROCEDURE T_context.reportVariables(VAR variableReport: T_variableTreeEntryCategoryNode);
  begin
    if related.parent<>nil then related.parent^.reportVariables(variableReport);
    if valueScope<>nil then valueScope^.reportVariables(variableReport);
  end;
{$endif}

FUNCTION T_context.reduceExpression(VAR first: P_token; VAR recycler:T_recycler): T_reduceResult;
  begin result:=reduceExpressionCallback(first,self,recycler); end;

FUNCTION T_context.reduceToLiteral(VAR first: P_token; VAR recycler:T_recycler): T_evaluationResult;
  begin
    result.triggeredByReturn:=reduceExpressionCallback(first,self,recycler)=rr_okWithReturn;
    if messages^.continueEvaluation and (first<>nil) and (first^.tokType=tt_literal) and (first^.next=nil) then begin
      result.literal:=P_literal(first^.data)^.rereferenced;
      recycler.disposeToken(first);
    end else begin
      result.literal:=nil;
      recycler.cascadeDisposeToken(first);
    end;
  end;

FUNCTION T_context.setAllowedSideEffectsReturningPrevious(CONST se: T_sideEffects): T_sideEffects;
  begin
    result:=allowedSideEffects;
    allowedSideEffects:=se;
  end;

FUNCTION T_context.getFutureEnvironment(VAR recycler:T_recycler) : P_context;
  begin
    result:=contextPool.newContext(recycler,@self,true);
  end;

PROCEDURE T_context.beginEvaluation;
  begin
    assert(state=fts_pending,'Wrong state before calling T_threadContext.beginEvaluation');
    state:=fts_evaluating;
  end;

PROCEDURE T_context.reattachToParent(VAR recycler:T_recycler);
  begin
    assert(state=fts_finished,'Wrong state before calling T_threadContext.reattachToParent');
    assert(valueScope=nil,'valueScope must be nil on T_threadContext.reattachToParent');
    enterCriticalSection(contextCS);
    valueScope:=recycler.newValueScopeAsChildOf(related.parent^.valueScope);
    state:=fts_pending;
    related.parent^.addChildContext;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_context.finalizeTaskAndDetachFromParent(CONST recyclerOrNil:P_recycler);
  begin
    enterCriticalSection(contextCS);
    with related do if childCount>0 then begin
      while childCount>0 do begin
        leaveCriticalSection(contextCS);
        ThreadSwitch; sleep(1);
        enterCriticalSection(contextCS);
      end;
    end;
    try
      if related.parent<>nil then related.parent^.dropChildContext;
      {$ifdef fullVersion}
      callStack.clear;
      parentCustomForm:=nil;
      {$endif}
      if recyclerOrNil=nil then     noRecycler_disposeScope(valueScope)
                           else recyclerOrNil^.disposeScope(valueScope);
      assert(valueScope=nil,'valueScope must be nil at this point');
      state:=fts_finished;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_context.raiseError(CONST text: string; CONST location: T_searchTokenLocation; CONST kind: T_messageType);
  {$ifdef fullVersion}
  VAR message:P_errorMessage;
  begin
    new(message,create(kind,location,split(text,C_lineBreakChar)));
    callStack.ensureTraceInError(message^);
    messages^.postCustomMessage(message,true);
  end;
  {$else}
  begin messages^.raiseSimpleError(text,location,kind); end;
  {$endif}

FUNCTION T_context.continueEvaluation: boolean;
  begin
    result:=messages^.continueEvaluation;
  end;

PROCEDURE T_evaluationGlobals.resolveMainParameter(VAR first:P_token);
  VAR parameterIndex:longint;
      s:string;
      newValue:P_literal=nil;
  begin
    if first^.tokType=tt_parameterIdentifier then begin
      if copy(first^.txt,1,1)='$' then begin
        parameterIndex:=strToIntDef(copy(first^.txt,2,length(first^.txt)-1),-1);
        if parameterIndex<0 then begin
          if first^.txt=ALL_PARAMETERS_TOKEN_TEXT then begin
            newValue:=newListLiteral(length(mainParameters)+1);
            P_listLiteral(newValue)^.appendString(first^.location.package^.getPath);
            for s in mainParameters do P_listLiteral(newValue)^.appendString(s);
          end else begin
            primaryContext.raiseError('Invalid parameter identifier',first^.location);
            exit;
          end;
        end else if parameterIndex=0 then
          newValue:=newStringLiteral(first^.location.package^.getPath)
        else if parameterIndex<=length(mainParameters) then
          newValue:=newStringLiteral(mainParameters[parameterIndex-1])
        else
          newValue:=newVoidLiteral;
        first^.data:=newValue;
        first^.tokType:=tt_literal;
      end else primaryContext.raiseError('Invalid parameter identifier',first^.location);
    end;
  end;

PROCEDURE T_context.raiseCannotApplyError(CONST ruleWithType: string; CONST parameters: P_listLiteral; CONST location: T_tokenLocation; CONST suffix: string; CONST missingMain: boolean); VAR message:string;
  begin
    message:='Cannot apply '+ruleWithType+' to parameter list '+parameterListTypeString(parameters)+':  '+toParameterListString(parameters,true,100);
    if length(suffix)>0 then message+=C_lineBreakChar+suffix;
    if missingMain
    then raiseError(message,location,mt_el3_noMatchingMain)
    else raiseError(message,location);
  end;

FUNCTION T_context.checkSideEffects(CONST id: string; CONST location: T_tokenLocation; CONST functionSideEffects: T_sideEffects): boolean;
  VAR messageText:string='';
      eff:T_sideEffect;
      violations:T_sideEffects;
  begin
    violations:=functionSideEffects-allowedSideEffects;
    if violations=[] then exit(true);
    for eff in violations do begin
      if messageText<>'' then messageText:=messageText+', ';
      messageText:=messageText+C_sideEffectName[eff];
    end;
    messageText:='Cannot apply '+id+' because of side effect(s): ['+messageText+']';
    raiseError(messageText,location);
    result:=false;
  end;

FUNCTION threadPoolThread(p:pointer):ptrint;
  CONST MS_IDLE_BEFORE_QUIT=500;
  VAR sleepCount:longint;
      currentTask:P_queueTask;
      recycler:T_recycler;
      stopBecauseOfMemoryUsage:boolean=false;
  begin
    recycler.initRecycler;
    sleepCount:=0;
    with P_evaluationGlobals(p)^ do begin
      repeat
        currentTask:=taskQueue.dequeue;
        if currentTask=nil then begin
          inc(sleepCount);
          ThreadSwitch;
          sleep(1);
        end else begin
          if currentTask^.isVolatile then begin
            currentTask^.evaluate(recycler);
            dispose(currentTask,destroy);
          end else begin
            currentTask^.evaluate(recycler);
          end;
          sleepCount:=0;
        end;
        stopBecauseOfMemoryUsage:=(not(isMemoryInComfortZone) and (taskQueue.poolThreadsRunning>1));
      until (sleepCount>=MS_IDLE_BEFORE_QUIT) or    //nothing to do
            (taskQueue.destructionPending) or
            not(primaryContext.messages^.continueEvaluation) or //error ocurred
            stopBecauseOfMemoryUsage; //memory panic with more than 1 pool thread running
      {$ifdef fullVersion}
      if stopBecauseOfMemoryUsage
      then postIdeMessage('Worker thread stopped because of memory panic',true)
      else postIdeMessage('Worker thread stopped',false);
      {$endif}

      result:=0;
      interlockedDecrement(taskQueue.poolThreadsRunning);
    end;
    recycler.cleanup;
  end;

CONSTRUCTOR T_context.create();
  begin
    initCriticalSection(contextCS);
    state:=fts_pending;
    {$ifdef fullVersion}
    callStack.create;
    {$endif}
    valueScope:=nil;
    related.childCount:=0;
    related.parent:=nil;
    related.evaluation:=nil;
    messages:=nil;
  end;

PROCEDURE T_context.dropChildContext;
  begin
    interlockedDecrement(related.childCount);
  end;

PROCEDURE T_context.addChildContext;
  begin
    interLockedIncrement(related.childCount);
  end;

PROCEDURE T_context.setThreadOptions(CONST globalOptions: T_evaluationContextOptions);
  VAR threadOption :T_threadContextOption;
  begin
    options:=[];
    for threadOption:=low(C_equivalentOption) to high(C_equivalentOption) do if C_equivalentOption[threadOption] in globalOptions then include(options,threadOption);
  end;

DESTRUCTOR T_context.destroy;
  begin
    enterCriticalSection(contextCS);
    {$ifdef fullVersion}
    callStack.destroy;
    {$endif}
    if valueScope<>nil then noRecycler_disposeScope(valueScope);
    related.childCount:=0;
    related.parent:=nil;
    related.evaluation:=nil;
    leaveCriticalSection(contextCS);
    doneCriticalSection(contextCS);
  end;

CONSTRUCTOR T_queueTask.create(CONST volatile:boolean; CONST newEnvironment:P_context);
  begin;
    assert(newEnvironment<>nil,'T_queueTask.create - newEnvironemnt must not be nil');
    isVolatile:=volatile;
    context:=newEnvironment;
    initCriticalSection(taskCs);
  end;

PROCEDURE T_queueTask.defineAndEnqueueOrEvaluate(VAR recycler:T_recycler);
  begin
    nextToEvaluate  :=nil;
    if not(isVolatile) and not(isMemoryInComfortZone)
    then evaluate(recycler)
    else context^.related.evaluation^.taskQueue.enqueue(@self,context);
  end;

PROCEDURE T_queueTask.reattach(VAR recycler:T_recycler);
  begin
    context^.reattachToParent(recycler);
  end;

DESTRUCTOR T_queueTask.destroy;
  begin
    if context<>nil then contextPool.disposeContext(context);
    doneCriticalSection(taskCs);
  end;

CONSTRUCTOR T_taskQueue.create;
  begin
    system.initCriticalSection(cs);
    memoryCleaner.registerObjectForCleanup(@cleanupQueues);
    destructionPending:=false;
    setLength(subQueue,0);
  end;

DESTRUCTOR T_taskQueue.destroy;
  begin
    memoryCleaner.unregisterObjectForCleanup(@cleanupQueues);
    while poolThreadsRunning>0 do begin
      destructionPending:=true;
      sleep(1);
      ThreadSwitch;
    end;
    cleanupQueues;
    system.doneCriticalSection(cs);
  end;

CONSTRUCTOR T_subQueue.create(CONST level: longint);
  begin
    first:=nil;
    last :=nil;
    queuedCount:=0;
    depth:=level;
  end;

DESTRUCTOR T_subQueue.destroy;
  begin
  end;

FUNCTION T_subQueue.enqueue(CONST task: P_queueTask):boolean;
  begin
    inc(queuedCount);
    if first=nil
    then first:=task
    else last^.nextToEvaluate:=task;
    last:=task;
    while last^.nextToEvaluate<>nil do begin
      inc(queuedCount);
      last:=last^.nextToEvaluate;
    end;
    result:=true;
  end;

FUNCTION T_subQueue.dequeue(OUT isEmptyAfter: boolean): P_queueTask;
  begin
    result:=first;
    first:=first^.nextToEvaluate;
    dec(queuedCount);
    isEmptyAfter:=(queuedCount=0) or (first=nil);
  end;

PROCEDURE T_taskQueue.enqueue(CONST task:P_queueTask; CONST context:P_context);
  PROCEDURE ensurePoolThreads();
    VAR aimPoolThreads:longint;
        spawnCount:longint=0;
    begin
      if isMemoryInComfortZone
      then aimPoolThreads:=settings.cpuCount
      else aimPoolThreads:=1;
      while poolThreadsRunning<aimPoolThreads do begin
        spawnCount+=1;
        interLockedIncrement(poolThreadsRunning);
        beginThread(@threadPoolThread,context^.related.evaluation);
      end;
      {$ifdef fullVersion}
      if spawnCount>0 then postIdeMessage('Spawned '+intToStr(spawnCount)+' new worker thread(s)',false);
      {$endif}
    end;

  FUNCTION ensureQueue:P_subQueue;
    VAR i:longint;
        s:P_subQueue;
    begin
      for i:=0 to length(subQueue)-1 do if subQueue[i]^.depth=context^.callDepth then exit(subQueue[i]);
      //create new sub-queue
      i:=length(subQueue);
      setLength(subQueue,i+1);
      new(result,create(context^.callDepth));
      subQueue[i]:=result;
      //bubble up
      while (i>0) and (subQueue[i-1]^.depth>subQueue[i]^.depth) do begin
        s            :=subQueue[i-1];
        subQueue[i-1]:=subQueue[i  ];
        subQueue[i  ]:=s;
        dec(i);
      end;
    end;

  begin
    system.enterCriticalSection(cs);
    try
      ensureQueue^.enqueue(task);
      ensurePoolThreads();
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_taskQueue.dequeue: P_queueTask;
  VAR emptyAfter:boolean;
      k:longint;
  begin
    system.enterCriticalSection(cs);
    try
      result:=nil;
      k:=length(subQueue)-1;
      while (k>=0) and (subQueue[k]^.queuedCount=0) do dec(k);
      if (k<0)
      then begin
        result:=nil;
        for k:=0 to length(subQueue)-1 do dispose(subQueue[k],destroy);
        setLength(subQueue,0);
      end else begin
        result:=subQueue[k]^.dequeue(emptyAfter);
      end;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_taskQueue.activeDeqeue(VAR recycler:T_recycler):boolean;
  VAR task:P_queueTask=nil;
  begin
    task:=dequeue;
    if task=nil
    then result:=false
    else begin
      result:=true;
      task^.context^.options-=[tco_spawnWorker];
      if task^.isVolatile then begin
        task^.evaluate(recycler);
        dispose(task,destroy);
      end else task^.evaluate(recycler);
    end;
  end;

PROCEDURE T_taskQueue.cleanupQueues;
  VAR k:longint;
  begin
    system.enterCriticalSection(cs);
    try
      k:=length(subQueue)-1;
      while (k>=0) and (subQueue[k]^.queuedCount=0) do begin
        dispose(subQueue[k],destroy);
        setLength(subQueue,k);
      end;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE cleanupContextPool;
  begin
    contextPool.cleanup;
  end;

INITIALIZATION
  contextPool.create;
  memoryCleaner.registerCleanupMethod(@cleanupContextPool);
FINALIZATION
  contextPool.destroy;

end.
