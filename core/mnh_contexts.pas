UNIT mnh_contexts;
INTERFACE
USES //FPC/LCL libraries
     sysutils,
     //3rd party libraries
     EpikTimer, RegExpr,
     //my libraries
     myGenerics,mySys,myStringUtil,
     //MNH:
     mnh_constants, mnh_basicTypes,
     {$ifdef fullVersion}mnh_settings,{$endif}
     mnh_messages,
     mnh_out_adapters,mnh_litVar,
     mnh_tokens,
     valueStore,
     mnh_profiling{$ifdef fullVersion},tokenStack,mnh_debugging,mnh_debuggingVar{$endif};
TYPE
  T_evaluationContextOption =(eco_spawnWorker,eco_profiling,eco_createDetachedTask,eco_timing,eco_debugging,eco_stackTrace,eco_beepOnError);
  T_threadContextOption     =(tco_spawnWorker,tco_profiling,tco_createDetachedTask,tco_timing,tco_debugging,tco_stackTrace,tco_notifyParentOfAsyncTaskEnd);
  T_evaluationContextOptions=set of T_evaluationContextOption;
  T_threadContextOptions    =set of T_threadContextOption;
  T_evaluationContextType   =(ect_normal{$ifdef fullVersion},ect_profiling,ect_debugging,ect_debuggingAndProfiling,ect_stackTracing{$endif},ect_silent);
  T_reduceResult            =(rr_fail,rr_ok,rr_okWithReturn);

CONST
  C_evaluationContextOptions:array[T_evaluationContextType] of T_evaluationContextOptions=(
  {ect_normal}                [eco_spawnWorker,eco_createDetachedTask,eco_beepOnError],
  {$ifdef fullVersion}
  {ect_profiling}             [eco_spawnWorker,eco_createDetachedTask,eco_beepOnError,eco_stackTrace,eco_profiling],
  {ect_debugging}             [                                       eco_beepOnError,eco_stackTrace,              eco_debugging],
  {ect_debuggingAndProfiling} [                                       eco_beepOnError,eco_stackTrace,eco_profiling,eco_debugging],
  {ect_stackTracing}          [eco_spawnWorker,eco_createDetachedTask,eco_beepOnError,eco_stackTrace],
  {$endif}
  {ect_silent}                [eco_spawnWorker,eco_createDetachedTask]);

  C_defaultOptions:T_evaluationContextOptions=[eco_spawnWorker,eco_createDetachedTask];
  C_equivalentOption:array[tco_spawnWorker..tco_stackTrace] of T_evaluationContextOption=(eco_spawnWorker,eco_profiling,eco_createDetachedTask,eco_timing,eco_debugging,eco_stackTrace);

TYPE

  P_evaluationContext=^T_evaluationContext;
  P_threadContext=^T_threadContext;
  P_taskQueue=^T_taskQueue;

  T_queueTaskEnvironment=record
    callingContext:P_threadContext;
    values        :P_valueStore;
    taskQueue     :P_taskQueue;
    initialDepth  :longint;
    initialAllow  :T_sideEffects;
  end;

  T_contextRecycler=object
    private
      recyclerCS:TRTLCriticalSection;
      contexts:array[0..63] of P_threadContext;
      fill:longint;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE disposeContext(VAR context:P_threadContext);
      FUNCTION newContext(CONST parentThread:P_threadContext):P_threadContext;
  end;

  { T_threadContext }

  T_threadContext=object
    private
      contextCS:TRTLCriticalSection;
      options:T_threadContextOptions;
      allowedSideEffects:T_sideEffects;
      related:record
        evaluation     :P_evaluationContext;
        parent         :P_threadContext;
        children       :array of P_threadContext;
      end;
      callStack :T_callStack;
      CONSTRUCTOR create();
      PROCEDURE initFor(CONST evaluation:P_evaluationContext; CONST parentThread:P_threadContext);
      DESTRUCTOR destroy;
      PROCEDURE dropChildContext(CONST context:P_threadContext);
      PROCEDURE addChildContext(CONST context:P_threadContext);
      PROCEDURE setThreadOptions(CONST globalOptions:T_evaluationContextOptions);
    public
      callDepth:longint;
      threadLocalMessages:T_threadLocalMessages;
      globalMessages:P_messageConnector;
      recycler  :T_tokenRecycler;
      valueStore:P_valueStore;
      {$ifdef fullVersion}
      parentCustomForm:pointer;
      {$endif}
      //Globals
      PROPERTY getGlobals:P_evaluationContext read related.evaluation;
      FUNCTION wallclockTime(CONST forceInit:boolean=false):double;
      //Side effects
      FUNCTION checkSideEffects(CONST id:string; CONST location:T_tokenLocation; CONST functionSideEffects:T_sideEffects):boolean;
      PROPERTY sideEffectWhitelist:T_sideEffects read allowedSideEffects;
      FUNCTION setAllowedSideEffectsReturningPrevious(CONST se:T_sideEffects):T_sideEffects;
      FUNCTION getNewEndToken(CONST blocking:boolean; CONST location:T_tokenLocation):P_token; {$ifndef debugMode} inline; {$endif}
      //Messaging
      PROCEDURE raiseCannotApplyError(CONST ruleWithType:string; CONST parameters:P_listLiteral; CONST location:T_tokenLocation; CONST suffix:string=''; CONST missingMain:boolean=false);
      //Evaluation:
      FUNCTION reduceExpression(VAR first:P_token):T_reduceResult; inline;
      FUNCTION reduceToLiteral(VAR first:P_token):T_evaluationResult; inline;
      //Multithreading:
      FUNCTION getFutureEnvironment:T_queueTaskEnvironment;
      FUNCTION getNewAsyncContext(CONST local:boolean):P_threadContext;
      PROCEDURE workerContextInit(CONST environment:T_queueTaskEnvironment);
      PROCEDURE workerContextDone;
      //Misc.:
      PROPERTY threadOptions:T_threadContextOptions read options;
      PROCEDURE callStackPush(CONST callerLocation:T_tokenLocation; CONST callee:P_objectWithIdAndLocation; CONST callParameters:P_variableTreeEntryCategoryNode);
      PROCEDURE callStackPush(CONST package:P_objectWithPath; CONST category:T_profileCategory; VAR calls:T_packageProfilingCalls);
      PROCEDURE callStackPop(CONST first:P_token);
  end;

  T_queueTaskState=(fts_pending, //set on construction
                    fts_evaluating, //set on dequeue
                    fts_ready); //set after evaluation
  P_queueTask=^T_queueTask;
  T_queueTask=object
    protected
      taskCs:TRTLCriticalSection;
      env   :T_queueTaskEnvironment;
      state :T_queueTaskState;
      evaluationResult:T_evaluationResult;
    private
      nextToEvaluate:P_queueTask;
      isVolatile    :boolean;
    public
      PROCEDURE evaluate(VAR context:T_threadContext); virtual; abstract;
      CONSTRUCTOR create(CONST environment:T_queueTaskEnvironment; CONST volatile:boolean);
      PROCEDURE reset;
      FUNCTION    canGetResult:boolean;
      FUNCTION    getResult:T_evaluationResult;
      DESTRUCTOR  destroy; virtual;
  end;

  T_taskQueue=object
    private
      subQueues:array of record
        priority:longint;
        first,last:P_queueTask;
      end;
      highestPrio     :longint;
      highestPrioIndex:longint;

      queuedCount:longint;
      cs:system.TRTLCriticalSection;
      destructionPending:boolean;
      poolThreadsRunning:longint;
    public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION  dequeue:P_queueTask;
    PROCEDURE activeDeqeue(VAR context:T_threadContext);
    PROPERTY getQueuedCount:longint read queuedCount;
    PROCEDURE enqueue(CONST task:P_queueTask; CONST context:P_threadContext);
  end;

  T_evaluationContext=object(T_threadContext)
    private
      taskQueue:T_taskQueue;
      wallClock:TEpikTimer;
      globalOptions :T_evaluationContextOptions;
      mainParameters:T_arrayOfString;
      {$ifdef fullVersion}
      profiler:P_profiler;
      debuggingStepper:P_debuggingStepper;
      {$endif}
      timingInfo:record
        startOfProfiling:double;
        timeSpent:array[pc_importing..pc_interpretation] of double;
      end;
      disposeAdaptersOnDestruction:boolean;
    public
      prng:T_xosPrng;
      CONSTRUCTOR create(CONST outAdapters:P_messageConnector);
    //  CONSTRUCTOR createAndResetSilentContext({$ifdef fullVersion}CONST package:P_objectWithPath;{$endif} CONST mainParams:T_arrayOfString; CONST customSideEffecWhitelist:T_sideEffects);
      DESTRUCTOR destroy;
      PROCEDURE resetForEvaluation({$ifdef fullVersion}CONST package:P_objectWithPath; {$endif} CONST evaluationContextType:T_evaluationContextType; CONST mainParams:T_arrayOfString; CONST enforceWallClock:boolean=false);
      PROCEDURE stopWorkers;
      PROCEDURE afterEvaluation;

      PROCEDURE timeBaseComponent(CONST component: T_profileCategory);

    //  PROPERTY evaluationOptions:T_evaluationContextOptions read options;
    //  PROPERTY threadContext:P_threadContext read primaryThreadContext;
    //  PROPERTY adapters:P_messageConnector read contextAdapters;
    //  {$ifdef fullVersion}
    //  FUNCTION stepper:P_debuggingStepper;
    //  FUNCTION isPaused:boolean;
    //  {$endif}
    //  FUNCTION getTaskQueue:P_taskQueue;
  end;

VAR reduceExpressionCallback:FUNCTION(VAR first:P_token; VAR context:T_threadContext):T_reduceResult;
    subruleReplacesCallback :FUNCTION(CONST subrulePointer:pointer; CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_threadContext):boolean;
    suppressBeep:boolean=false;
    contextPool:T_contextRecycler;
{$ifndef fullVersion}
FUNCTION workerThreadCount:longint;
{$endif}
IMPLEMENTATION
VAR globalLock:TRTLCriticalSection;

{ T_contextRecycler }

CONSTRUCTOR T_contextRecycler.create;
  begin
    initCriticalSection(recyclerCS);
    fill:=0;
    initialize(contexts);
  end;

DESTRUCTOR T_contextRecycler.destroy;
  VAR k:longint;
  begin
    enterCriticalSection(recyclerCS);
    for k:=0 to fill-1 do dispose(contexts[k],destroy);
    fill:=0;
    leaveCriticalSection(recyclerCS);
    doneCriticalSection(recyclerCS);
  end;

PROCEDURE T_contextRecycler.disposeContext(VAR context: P_threadContext);
  begin
    enterCriticalSection(recyclerCS);
    if fill<length(contexts) then begin
      contexts[fill]:=context;
      inc(fill);
    end else dispose(context,destroy);
    leaveCriticalSection(recyclerCS);
  end;

FUNCTION T_contextRecycler.newContext(CONST parentThread:P_threadContext):P_threadContext;
  begin
    enterCriticalSection(recyclerCS);
    if fill>0 then begin
      dec(fill);
      result:=contexts[fill];
    end else new(result,create());
    result^.initFor(parentThread^.related.evaluation,parentThread);
    leaveCriticalSection(recyclerCS);
  end;

{$ifndef fullVersion}
FUNCTION workerThreadCount:longint;
  begin
    result:=getNumberOfCPUs-1;
  end;
{$endif}

//CONSTRUCTOR T_threadContext.createThreadContext(CONST parent_:P_evaluationContext; CONST outAdapters:P_messageConnector=nil);
//  begin
//    recycler  .create;
//    new(valueStore,create);
//    {$ifdef fullVersion}
//    callStack .create;
//    parentCustomForm:=nil;
//    {$endif}
//    parent        :=parent_;
//    adapters.create(@callStack.ensureTraceInError,outAdapters);
//    callingContext:=nil;
//    Cache    :=nil;
//    allowedSideEffects:=C_allSideEffects;
//    callDepth:=0;
//    dequeueContext_:=nil;
//  end;
//
//CONSTRUCTOR T_threadContext.createWorkerContext(CONST adapters_:P_messageConnector);
//  begin
//    recycler  .create;
//    new(valueStore,create);
//    {$ifdef fullVersion}
//    callStack .create;
//    parentCustomForm:=nil;
//    {$endif}
//    allowedSideEffects:=C_allSideEffects;
//    regexCache    :=nil;
//    parent        :=nil;
//    adapters.create(@callStack.ensureTraceInError,adapters_);
//    callingContext:=nil;
//    dequeueContext_:=nil;
//  end;
//
//DESTRUCTOR T_threadContext.destroy;
//  begin
//    {$ifdef debugMode}{$ifdef fullVersion}
//    if callStack.size>0 then raise Exception.create('Non-empty callstack on T_threadContext.doneEvaluating');
//    parentCustomForm:=nil;
//    {$endif}{$endif}
//    if valueStore<>nil then dispose(valueStore,destroy);
//    recycler  .destroy;
//    if regexCache<>nil then dispose(regexCache,destroy);
//    if dequeueContext_<>nil then dispose(dequeueContext_,destroy);
//  end;

CONSTRUCTOR T_evaluationContext.create(CONST outAdapters:P_messageConnector);
  begin
    inherited create();
    prng.create;
    prng.randomize;
    wallClock:=nil;
    {$ifdef fullVersion}
    profiler:=nil;
    debuggingStepper:=nil;
    {$endif}
    taskQueue.create;

    related.evaluation:=@self;
    related.parent:=nil;
    setLength(related.children,0);
    globalMessages:=outAdapters;
    disposeAdaptersOnDestruction:=false;
    allowedSideEffects:=C_allSideEffects;
    mainParameters:=C_EMPTY_STRING_ARRAY;
  end;
//
//CONSTRUCTOR T_evaluationContext.createAndResetSilentContext({$ifdef fullVersion}CONST package:P_objectWithPath; {$endif}CONST mainParams:T_arrayOfString; CONST customSideEffecWhitelist:T_sideEffects);
//  VAR tempAdapters:P_messageConnector;
//  begin
//    new(tempAdapters,create);
//    create(tempAdapters);
//    taskQueue:=nil;
//    disposeAdaptersOnDestruction:=true;
//    allowedSideEffects:=customSideEffecWhitelist;
//    mainParameters:=C_EMPTY_STRING_ARRAY;
//    resetForEvaluation({$ifdef fullVersion}package,{$endif}ect_silent,mainParams);
//  end;
//
DESTRUCTOR T_evaluationContext.destroy;
  begin
    prng.destroy;
    if wallClock<>nil then FreeAndNil(wallClock);
    {$ifdef fullVersion}
    if profiler<>nil then dispose(profiler,destroy);
    if debuggingStepper<>nil then dispose(debuggingStepper,destroy);
    {$endif}
    taskQueue.destroy;
    if disposeAdaptersOnDestruction then dispose(globalMessages,destroy);
    inherited destroy;
  end;
//
PROCEDURE T_evaluationContext.resetForEvaluation({$ifdef fullVersion}CONST package:P_objectWithPath; {$endif}CONST evaluationContextType:T_evaluationContextType; CONST mainParams:T_arrayOfString; CONST enforceWallClock:boolean=false);
  VAR pc:T_profileCategory;
      i:longint;
  begin
    setLength(mainParameters,length(mainParams));
    for i:=0 to length(mainParams)-1 do mainParameters[i]:=mainParams[i];
    //set options
    globalOptions:=C_evaluationContextOptions[evaluationContextType];
    if globalMessages^.isCollecting(mt_timing_info) then globalOptions:=globalOptions+[eco_timing];
    if evaluationContextType=ect_silent then allowedSideEffects:=C_allSideEffects-[se_inputViaAsk]
                                        else allowedSideEffects:=C_allSideEffects;
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
      if debuggingStepper=nil then new(debuggingStepper,create(globalMessages));
      debuggingStepper^.resetForDebugging(package);
    end else if debuggingStepper<>nil then begin
      dispose(debuggingStepper,destroy);
      debuggingStepper:=nil;
    end;

    {$endif}
    setThreadOptions(globalOptions);
    setLength(related.children,0);
    //timing:
    if (eco_timing in globalOptions) or (eco_profiling in globalOptions) or enforceWallClock then begin
      if wallClock=nil then wallClock:=TEpikTimer.create(nil);
      with timingInfo do for pc:=low(timeSpent) to high(timeSpent) do timeSpent[pc]:=0;
      wallClock.clear;
      wallClock.start;
      timingInfo.startOfProfiling:=wallClock.elapsed;
    end;
  end;
//
PROCEDURE T_evaluationContext.stopWorkers;
  CONST TIME_OUT_AFTER=10/(24*60*60); //=10 seconds
  VAR timeout:double;
      dequeueContext:P_threadContext;
  begin
    timeout:=now+TIME_OUT_AFTER;
    threadLocalMessages.setStopFlag;
    while (now<timeout) and ((length(related.children)>0) or (taskQueue.queuedCount>0)) do begin
      dequeueContext:=contextPool.newContext(@self);
      taskQueue.activeDeqeue(dequeueContext^);
      contextPool.disposeContext(dequeueContext);
      ThreadSwitch;
      sleep(1);
    end;
  end;
//
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
      globalMessages^.postTextMessage(mt_timing_info,C_nilTokenLocation,timingMessage);
    end;

  begin
    stopWorkers;
    globalMessages^.postSingal(mt_endOfEvaluation,C_nilTokenLocation);
    if (globalMessages^.isCollecting(mt_timing_info)) and (wallClock<>nil) then logTimingInfo;
    {$ifdef fullVersion}
    if (eco_profiling in globalOptions) and (profiler<>nil) then profiler^.logInfo(globalMessages);
    {$endif}
    //if not(suppressBeep) and (eco_beepOnError in globalMessages) and adapters^.triggersBeep then beep;
  end;
//
//PROCEDURE T_evaluationContext.setupThreadContext(CONST context:P_threadContext);
//  VAR threadOptions:T_threadContextOptions=[];
//      threadOption :T_threadContextOption;
//  begin
//    for threadOption:=low(C_equivalentOption) to high(C_equivalentOption) do if C_equivalentOption[threadOption] in options then include(threadOptions,threadOption);
//    context^.options:=threadOptions;
//    context^.parent:=@self;
//    {$ifdef fullVersion}
//    context^.callStack.clear;
//    context^.parentCustomForm:=nil;
//    {$endif}
//    context^.valueStore^.clear;
//    context^.adapters:=adapters;
//    context^.callDepth:=0;
//    context^.allowedSideEffects:=allowedSideEffects;
//  end;
//
//{$ifdef fullVersion}
//FUNCTION T_evaluationContext.stepper:P_debuggingStepper;
//  begin
//    if debuggingStepper=nil then new(debuggingStepper,create(adapters));
//    result:=debuggingStepper;
//  end;
//
//FUNCTION T_evaluationContext.isPaused:boolean;
//  begin
//    result:=(debuggingStepper<>nil) and debuggingStepper^.paused;
//  end;
//{$endif}
//
//FUNCTION T_evaluationContext.getTaskQueue:P_taskQueue;
//  begin
//    if taskQueue=nil then begin
//      enterCriticalSection(globalLock);
//      if taskQueue=nil then new(taskQueue,create);
//      leaveCriticalSection(globalLock);
//    end;
//    result:=taskQueue;
//  end;
//
FUNCTION T_threadContext.wallclockTime(CONST forceInit: boolean): double;
  begin
    if related.evaluation^.wallClock=nil then begin
      if forceInit then begin
        enterCriticalSection(globalLock);
        if related.evaluation^.wallClock=nil then begin
          related.evaluation^.wallClock:=TEpikTimer.create(nil);
          related.evaluation^.wallClock.clear;
          related.evaluation^.wallClock.start;
        end;
        leaveCriticalSection(globalLock);
      end else exit(0);
    end;
    result:=related.evaluation^.wallClock.elapsed;
  end;
//
PROCEDURE T_evaluationContext.timeBaseComponent(CONST component: T_profileCategory);
  begin
    with timingInfo do timeSpent[component]:=wallclockTime-timeSpent[component];
  end;
//
//
FUNCTION T_threadContext.getNewAsyncContext(CONST local: boolean
  ): P_threadContext;
  begin
    if not(tco_createDetachedTask in options) then exit(nil);
    result:=contextPool.newContext(@self);
    result^.options:=options+[tco_notifyParentOfAsyncTaskEnd];
    if local then result^.valueStore^.parentStore:=valueStore;
  end;
//
//
//{$ifdef fullVersion}
PROCEDURE T_threadContext.callStackPush(CONST callerLocation: T_tokenLocation;
  CONST callee: P_objectWithIdAndLocation;
  CONST callParameters: P_variableTreeEntryCategoryNode);
  begin
    if not(tco_stackTrace in options) then exit;
    callStack.push(wallclockTime,callParameters,callerLocation,callee);
  end;

PROCEDURE T_threadContext.callStackPush(CONST package:P_objectWithPath; CONST category:T_profileCategory; VAR calls:T_packageProfilingCalls);
  begin
    if not(tco_stackTrace in options) then exit;
    if calls[category]=nil then new(calls[category],create(package,category));
    callStack.push(wallclockTime,nil,calls[category]^.getLocation,calls[category]);
  end;

PROCEDURE T_threadContext.callStackPop(CONST first: P_token);
  VAR loc:T_tokenLocation;
  begin
    if not(tco_stackTrace in options) then exit;
    loc:=callStack.pop(wallclockTime,related.evaluation^.profiler);
    if (loc.package<>nil) and (first<>nil) then first^.location:=loc;
  end;

//PROCEDURE T_threadContext.reportVariables(VAR variableReport: T_variableTreeEntryCategoryNode);
//  begin
//    if callingContext<>nil then callingContext^.reportVariables(variableReport);
//    valueStore^.reportVariables(variableReport);
//  end;
//{$endif}
//
FUNCTION T_threadContext.reduceExpression(VAR first: P_token): T_reduceResult;
  begin result:=reduceExpressionCallback(first,self); end;

FUNCTION T_threadContext.reduceToLiteral(VAR first: P_token
  ): T_evaluationResult;
  begin
    result.triggeredByReturn:=reduceExpressionCallback(first,self)=rr_okWithReturn;
    if threadLocalMessages.continueEvaluation and (first<>nil) and (first^.tokType=tt_literal) and (first^.next=nil) then begin
      result.literal:=P_literal(first^.data)^.rereferenced;
      recycler.disposeToken(first);
    end else begin
      result.literal:=nil;
      recycler.cascadeDisposeToken(first);
    end;
  end;

FUNCTION T_threadContext.setAllowedSideEffectsReturningPrevious(
  CONST se: T_sideEffects): T_sideEffects;
  begin
    result:=allowedSideEffects;
    allowedSideEffects:=se;
  end;

//PROCEDURE T_threadContext.setSideEffectsByEndToken(CONST token:P_token);
//  begin
//    {$ifdef debugMode}
//    if (not(token^.tokType in [tt_endRule,tt_endExpression])) then raise Exception.create('Invalid parameter for setSideEffectsByEndToken; not an end-token but '+safeTokenToString(token));
//    {$endif}
//    move(token^.data,allowedSideEffects,sizeOf(pointer));
//  end;
//
FUNCTION T_threadContext.getNewEndToken(CONST blocking: boolean;
  CONST location: T_tokenLocation): P_token;
  VAR data:pointer=nil;
  begin
    move(allowedSideEffects,data,sizeOf(data));
    if blocking then result:=recycler.newToken(location,'',tt_endRule,data)
                else result:=recycler.newToken(location,'',tt_endExpression,data);
  end;

FUNCTION T_threadContext.getFutureEnvironment: T_queueTaskEnvironment;
  begin
    result.callingContext:=@self;
    result.values:=valueStore^.readOnlyClone;
    result.taskQueue:=@related.evaluation^.taskQueue;
    result.initialDepth:=callDepth;
    result.initialAllow:=sideEffectWhitelist;
  end;

PROCEDURE T_threadContext.workerContextInit(
  CONST environment: T_queueTaskEnvironment);
  begin
    with related do begin
      parent:=environment.callingContext;
      evaluation:=parent^.related.evaluation;
      parent^.addChildContext(@self);
    end;
    options:=related.parent^.options;
    threadLocalMessages.clear;
    allowedSideEffects:=environment.initialAllow;
    valueStore^.clear;
    valueStore^.parentStore:=environment.values;
    {$ifdef fullVersion}
    callStack.clear;
    {$endif}
    callDepth:=environment.initialDepth;

  end;

{PROCEDURE T_threadContext.detachWorkerContext;
  begin
    {$ifdef debugMode}
    if not(valueStore^.isEmpty) and (threadLocalMessages.continueEvaluation) then raise Exception.create('valueStore must be empty on detach');
    {$endif}
    parent:=nil;
    callingContext:=nil;
    threadLocalMessages.clear;
    valueStore^.clear;
    valueStore^.parentStore:=nil;
    {$ifdef fullVersion}
    callStack.clear;
    {$endif}
  end;}

PROCEDURE T_threadContext.workerContextDone;
  begin
    threadLocalMessages.escalateErrors;
    {$ifdef fullVersion}
    {$ifdef debugMode}
    if callStack.size>0 then raise Exception.create('Non-empty callstack on T_threadContext.doneEvaluating');
    {$endif}
    callStack.clear;
    {$endif}
    valueStore^.clear;
    if related.parent<>nil then related.parent^.dropChildContext(@self);
    related.parent:=nil;
    related.evaluation:=nil;
  end;

//PROCEDURE T_threadContext.resolveMainParameter(VAR first:P_token);
//  VAR parameterIndex:longint;
//      s:string;
//      newValue:P_literal=nil;
//  begin
//    if first^.tokType=tt_parameterIdentifier then begin
//      if copy(first^.txt,1,1)='$' then begin
//        parameterIndex:=strToIntDef(copy(first^.txt,2,length(first^.txt)-1),-1);
//        if parameterIndex<0 then begin
//          if first^.txt=ALL_PARAMETERS_TOKEN_TEXT then begin
//            newValue:=newListLiteral(length(parent^.mainParameters)+1);
//            P_listLiteral(newValue)^.appendString(first^.location.package^.getPath);
//            for s in parent^.mainParameters do P_listLiteral(newValue)^.appendString(s);
//          end else begin
//            adapters^.raiseError('Invalid parameter identifier',first^.location);
//            exit;
//          end;
//        end else if parameterIndex=0 then
//          newValue:=newStringLiteral(first^.location.package^.getPath)
//        else if parameterIndex<=length(parent^.mainParameters) then
//          newValue:=newStringLiteral(parent^.mainParameters[parameterIndex-1])
//        else
//          newValue:=newVoidLiteral;
//        first^.data:=newValue;
//        first^.tokType:=tt_literal;
//      end else adapters^.raiseError('Invalid parameter identifier',first^.location);
//    end;
//  end;
//
//{$ifdef fullVersion}
//PROCEDURE T_threadContext.callStackPrint(CONST targetAdapters:P_adapters=nil);
//  VAR p:P_threadContext;
//      a:P_adapters;
//  begin
//    if not(tco_stackTrace in options) then exit;
//    a:=targetAdapters;
//    if a=nil then a:=adapters;
//    if a=nil then exit;
//    p:=callingContext;
//    callStack.print(a^);
//    if p<>nil then p^.callStackPrint(a);
//  end;
//
//PROCEDURE T_threadContext.callStackClear;
//  begin
//    callStack.clear;
//  end;
//{$endif}
//
PROCEDURE T_threadContext.raiseCannotApplyError(CONST ruleWithType: string;
  CONST parameters: P_listLiteral; CONST location: T_tokenLocation;
  CONST suffix: string; CONST missingMain: boolean);
  VAR message:string;
  begin
    message:='Cannot apply '+ruleWithType+' to parameter list '+parameterListTypeString(parameters)+':  '+toParameterListString(parameters,true,100);
    if length(suffix)>0 then message+=C_lineBreakChar+suffix;
    if missingMain
    then threadLocalMessages.raiseError(message,location,mt_el3_noMatchingMain)
    else threadLocalMessages.raiseError(message,location);
  end;

FUNCTION T_threadContext.checkSideEffects(CONST id: string;
  CONST location: T_tokenLocation; CONST functionSideEffects: T_sideEffects
  ): boolean;
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
    threadLocalMessages.raiseError(messageText,location);
    result:=false;
  end;

//{$ifdef fullVersion}
//FUNCTION T_threadContext.stepping(CONST first:P_token; CONST stack:P_tokenStack):boolean; {$ifndef debugMode} inline; {$endif}
//  begin
//    if (parent=nil) or (parent^.debuggingStepper=nil) then exit(false);
//    if first<>nil then parent^.debuggingStepper^.stepping(first,stack,@callStack);
//    result:=true;
//  end;
//{$endif}

FUNCTION threadPoolThread(p:pointer):ptrint;
  //means that 0.511 seconds have passed since the last activity
  CONST SLEEP_TIME_TO_QUIT=73;
  VAR sleepTime:longint;
      currentTask:P_queueTask;
      tempcontext:P_threadContext;
  begin
    sleepTime:=0;
    tempcontext:=contextPool.newContext(nil);
    with P_evaluationContext(p)^ do begin
      repeat
        currentTask:=taskQueue.dequeue;
        if currentTask=nil then begin
          inc(sleepTime);
          ThreadSwitch;
          sleep(sleepTime div 5);
        end else begin
          if currentTask^.isVolatile then begin
            currentTask^.evaluate(tempcontext^);
            dispose(currentTask,destroy);
          end else currentTask^.evaluate(tempcontext^);
          sleepTime:=0;
        end;
      until (sleepTime>=SLEEP_TIME_TO_QUIT) or (taskQueue.destructionPending) or not(tempcontext^.threadLocalMessages.continueEvaluation);
      contextPool.disposeContext(tempcontext);
      result:=0;
      interlockedDecrement(taskQueue.poolThreadsRunning);
    end;
  end;

CONSTRUCTOR T_threadContext.create();
  begin
    initCriticalSection(contextCS);
    enterCriticalSection(contextCS);
    callStack.create;
    new(valueStore,create);
    recycler.create;
    setLength(related.children,0);
    related.parent:=nil;
    related.evaluation:=nil;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_threadContext.initFor(CONST evaluation: P_evaluationContext;
  CONST parentThread: P_threadContext);
  begin
    enterCriticalSection(contextCS);
    if parentThread=nil then begin
      setThreadOptions(evaluation^.globalOptions);
      related.parent:=nil;
      related.evaluation:=evaluation;
      threadLocalMessages.setParent(nil);
      callDepth:=parentThread^.callDepth+1;
      globalMessages:=evaluation^.globalMessages;
    end else begin
      options:=parentThread^.options;
      related.parent:=parentThread;
      related.evaluation:=parentThread^.related.evaluation;
      parentThread^.addChildContext(@self);
      callDepth:=0;
      globalMessages:=parentThread^.globalMessages;
    end;
    callStack.clear;
    valueStore^.clear;
    parentCustomForm:=nil;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_threadContext.dropChildContext(CONST context: P_threadContext);
  VAR k:longint=0;
  begin
    enterCriticalSection(contextCS);
    with related do
    while k<length(children) do
    if children[k]=context then begin
      children[k]:=children[length(children)-1];
      setLength(children,length(children)-1);
      context^.threadLocalMessages.setParent(nil);
    end else inc(k);
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_threadContext.addChildContext(CONST context: P_threadContext);
  VAR k:longint=0;
  begin
    enterCriticalSection(contextCS);
    with related do begin
      for k:=0 to length(children)-1 do if children[k]=context then begin
        leaveCriticalSection(contextCS);
        exit;
      end;
      k:=length(children);
      setLength(children,k+1);
      children[k]:=context;
      context^.threadLocalMessages.setParent(@threadLocalMessages);
    end;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_threadContext.setThreadOptions( CONST globalOptions: T_evaluationContextOptions);
  VAR threadOption :T_threadContextOption;
  begin
    for threadOption:=low(C_equivalentOption) to high(C_equivalentOption) do if C_equivalentOption[threadOption] in globalOptions then include(options,threadOption);
  end;

DESTRUCTOR T_threadContext.destroy;
  begin
    doneCriticalSection(contextCS);
  end;

CONSTRUCTOR T_queueTask.create(CONST environment:T_queueTaskEnvironment; CONST volatile:boolean);
  begin;
    initCriticalSection(taskCs);
    env       :=environment;
    isVolatile:=volatile;
  end;

PROCEDURE T_queueTask.reset;
  begin
    state :=fts_pending;
    evaluationResult:=NIL_EVAL_RESULT;
    nextToEvaluate  :=nil;
  end;

FUNCTION T_queueTask.canGetResult:boolean;
  begin
    enterCriticalSection(taskCs);
    result:=state=fts_ready;
    leaveCriticalSection(taskCs);
  end;

FUNCTION T_queueTask.getResult:T_evaluationResult;
  VAR sleepTime:longint=0;
  begin
    enterCriticalSection(taskCs);
    while state in [fts_pending,fts_evaluating] do begin
      leaveCriticalSection(taskCs);
      ThreadSwitch;
      sleep(sleepTime);
      if sleepTime<100 then inc(sleepTime);
      enterCriticalSection(taskCs);
    end;
    result:=evaluationResult;
    leaveCriticalSection(taskCs);
  end;

DESTRUCTOR T_queueTask.destroy;
  begin
    doneCriticalSection(taskCs);
  end;

CONSTRUCTOR T_taskQueue.create;
  begin
    system.initCriticalSection(cs);
    destructionPending:=false;
    highestPrio:=-1;
    highestPrioIndex:=-1;
    setLength(subQueues,0);
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

PROCEDURE T_taskQueue.enqueue(CONST task:P_queueTask; CONST context:P_threadContext);
  PROCEDURE ensurePoolThreads();
    begin
      if (poolThreadsRunning<workerThreadCount) then begin
        interLockedIncrement(poolThreadsRunning);
        beginThread(@threadPoolThread,context^.related.evaluation);
      end;
    end;

  VAR subQueueIndex:longint=0;
  begin
    enterCriticalSection(task^.taskCs);
    task^.nextToEvaluate:=nil;
    task^.state:=fts_pending;
    task^.evaluationResult:=NIL_EVAL_RESULT;
    leaveCriticalSection(task^.taskCs);

    system.enterCriticalSection(cs);
    if length(subQueues)=0 then begin
      setLength(subQueues,1);
      subQueues[0].priority:=context^.callDepth;
      subQueues[0].first:=nil;
      highestPrio:=subQueues[0].priority;
      highestPrioIndex:=0;
      queuedCount:=0;
    end else begin
      while (subQueueIndex<length(subQueues)) and (subQueues[subQueueIndex].priority<>context^.callDepth) do inc(subQueueIndex);
      if subQueueIndex=length(subQueues) then begin
        setLength(subQueues,subQueueIndex+1);
        subQueues[subQueueIndex].priority:=context^.callDepth;
        subQueues[subQueueIndex].first:=nil;
        if subQueues[subQueueIndex].priority>highestPrio then begin
          highestPrio:=subQueues[subQueueIndex].priority;
          highestPrioIndex:=     subQueueIndex;
        end;
      end;
    end;
    inc(queuedCount);
    with subQueues[subQueueIndex] do begin
      if first=nil then begin
        first:=task;
        last:=task;
      end else begin
        last^.nextToEvaluate:=task;
        last:=task;
      end;
    end;
    ensurePoolThreads();
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_taskQueue.dequeue: P_queueTask;
  VAR k:longint;
  begin
    system.enterCriticalSection(cs);
    if highestPrioIndex<0
    then result:=nil
    else begin
      with subQueues[highestPrioIndex] do begin
        dec(queuedCount);
        result:=first;
        first:=first^.nextToEvaluate;
      end;
      //drop subqueue if empty
      if subQueues[highestPrioIndex].first=nil then begin
        k:=length(subQueues)-1;
        subQueues[highestPrioIndex]:=subQueues[k];
        setLength(subQueues,k);
        //find new highest prio
        highestPrio:=-1;
        highestPrioIndex:=-1;
        for k:=0 to length(subQueues)-1 do if subQueues[k].priority>highestPrio then begin
          highestPrio:=subQueues[k].priority;
          highestPrioIndex:=     k;
        end;
      end;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_taskQueue.activeDeqeue(VAR context: T_threadContext);
  VAR task:P_queueTask=nil;
  begin
    task:=dequeue;
    if task<>nil then begin
      if task^.isVolatile then begin
        task^.evaluate(context);
        dispose(task,destroy);
      end else task^.evaluate(context);
    end;
  end;

INITIALIZATION
  initialize(globalLock);
  initCriticalSection(globalLock);
  contextPool.create;
FINALIZATION
  doneCriticalSection(globalLock);
  contextPool.destroy;

end.
