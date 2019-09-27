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
  TASKS_TO_QUEUE_PER_CPU=2;
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
      FUNCTION newContext(VAR recycler:T_recycler; CONST parentThread:P_context; CONST parentScopeAccess:AccessLevel):P_context;
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
  T_queueTask=object
    protected
      context:P_context;
      taskCs:TRTLCriticalSection;
    private
      nextToEvaluate:P_queueTask;
      isVolatile    :boolean;
    public
      PROCEDURE   evaluate(VAR recycler:T_recycler); virtual; abstract;
      CONSTRUCTOR create(CONST volatile:boolean);
      PROCEDURE   defineAndEnqueueOrEvaluate(CONST newEnvironment:P_context; VAR recycler:T_recycler);
      PROCEDURE   clearContext;
      DESTRUCTOR  destroy; virtual;
  end;

  T_taskQueue=object
    private
      first,last:P_queueTask;
      queuedCount:longint;

      cs:system.TRTLCriticalSection;
      destructionPending:boolean;
      poolThreadsRunning:longint;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION  dequeue:P_queueTask;
      FUNCTION  activeDeqeue(VAR recycler:T_recycler):boolean;
      PROPERTY getQueuedCount:longint read queuedCount;
      PROCEDURE enqueue(CONST task:P_queueTask; CONST context:P_context);
  end;

  T_evaluationGlobals=object
    private
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
      primaryContext:T_context;
      taskQueue:T_taskQueue;
      prng:T_xosPrng;
      CONSTRUCTOR create(CONST outAdapters:P_messages);
      DESTRUCTOR destroy;
      PROCEDURE resetForEvaluation({$ifdef fullVersion}CONST package:P_objectWithPath; CONST packageVar_:F_fillCategoryNode; {$endif}
                                  CONST evaluationContextType:T_evaluationContextType; CONST mainParams:T_arrayOfString; VAR recycler:T_recycler);
      PROCEDURE stopWorkers(VAR recycler:T_recycler);
      PROCEDURE afterEvaluation(VAR recycler:T_recycler);
      PROPERTY options:T_evaluationContextOptions read globalOptions;
      PROCEDURE timeBaseComponent(CONST component: T_profileCategory);

      PROCEDURE resolveMainParameter(VAR first:P_token);

      FUNCTION queuedFuturesCount:longint;

      {$ifdef fullVersion}
      FUNCTION stepper:P_debuggingStepper;
      FUNCTION isPaused:boolean;
      {$endif}
  end;

VAR reduceExpressionCallback:FUNCTION(VAR first:P_token; VAR context:T_context; VAR recycler:T_recycler):T_reduceResult;
    subruleReplacesCallback :FUNCTION(CONST subrulePointer:pointer; CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_context; VAR recycler:T_recycler):boolean;
    suppressBeep:boolean=false;
    contextPool:T_contextRecycler;
IMPLEMENTATION

CONSTRUCTOR T_contextRecycler.create;
  begin
    initCriticalSection(recyclerCS);
    fill:=0;
    initialize(contexts);
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

FUNCTION T_contextRecycler.newContext(VAR recycler:T_recycler; CONST parentThread:P_context; CONST parentScopeAccess:AccessLevel):P_context;
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
      options           :=parentThread^.options-[tco_spawnWorker];
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

      if parentScopeAccess=ACCESS_BLOCKED
      then valueScope:=nil
      else valueScope:=recycler.newValueScopeAsChildOf(parentThread^.valueScope,parentScopeAccess);

      {$ifdef fullVersion}
      parentCustomForm:=nil;
      {$endif}
      state:=fts_pending;
    end;
  end;

CONSTRUCTOR T_evaluationGlobals.create(CONST outAdapters:P_messages);
  begin
    primaryContext.create();
    prng.create;
    prng.randomize;
    wallClock:=nil;
    {$ifdef fullVersion}
    profiler:=nil;
    debuggingStepper:=nil;
    {$endif}
    taskQueue.create;

    primaryContext.related.evaluation:=@self;
    primaryContext.related.parent:=nil;
    primaryContext.related.childCount:=0;

    primaryContext.messages:=outAdapters;
    disposeAdaptersOnDestruction:=false;
    primaryContext.allowedSideEffects:=C_allSideEffects;
    mainParameters:=C_EMPTY_STRING_ARRAY;
  end;

DESTRUCTOR T_evaluationGlobals.destroy;
  begin
    prng.destroy;
    if wallClock<>nil then FreeAndNil(wallClock);
    {$ifdef fullVersion}
    if profiler<>nil then dispose(profiler,destroy);
    profiler:=nil;
    if debuggingStepper<>nil then dispose(debuggingStepper,destroy);
    debuggingStepper:=nil;
    {$endif}
    taskQueue.destroy;
    if disposeAdaptersOnDestruction then dispose(primaryContext.messages,destroy);
    primaryContext.destroy;
  end;

PROCEDURE T_evaluationGlobals.resetForEvaluation({$ifdef fullVersion}CONST package:P_objectWithPath; CONST packageVar_:F_fillCategoryNode; {$endif}
                                                 CONST evaluationContextType:T_evaluationContextType; CONST mainParams:T_arrayOfString; VAR recycler:T_recycler);
  VAR pc:T_profileCategory;
      i:longint;
  begin
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
    if (wallClock=nil) and ((eco_profiling in globalOptions) or (eco_timing in globalOptions))
    then wallClock:=TEpikTimer.create(nil);

    if wallClock<>nil then begin;
      wallClock.clear;
      wallClock.start;
      timingInfo.startOfProfiling:=wallClock.elapsed;
    end;
    //evaluation state
    if evaluationContextType=ect_silent
    then primaryContext.allowedSideEffects:=C_allSideEffects-[se_inputViaAsk]
    else primaryContext.allowedSideEffects:=C_allSideEffects;
    primaryContext.setThreadOptions(globalOptions);
    recycler.disposeScope(primaryContext.valueScope);
    primaryContext.valueScope:=nil;
    primaryContext.messages^.clear();
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

PROCEDURE T_evaluationGlobals.stopWorkers(VAR recycler:T_recycler);
  CONST TIME_OUT_AFTER=10/(24*60*60); //=10 seconds
  VAR timeout:double;
  begin
    timeout:=now+TIME_OUT_AFTER;
    primaryContext.messages^.setStopFlag;
    while (now<timeout) and ((primaryContext.related.childCount>0) or (taskQueue.queuedCount>0)) do begin
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
      primaryContext.messages^.postTextMessage(mt_timing_info,C_nilTokenLocation,timingMessage);
    end;

  begin
    stopWorkers(recycler);
    primaryContext.messages^.postSingal(mt_endOfEvaluation,C_nilTokenLocation);
    if (primaryContext.messages^.isCollecting(mt_timing_info)) and (wallClock<>nil) then logTimingInfo;
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
  begin
    if related.evaluation^.wallClock=nil then begin
      enterCriticalSection(related.evaluation^.primaryContext.contextCS);
      try
        if related.evaluation^.wallClock=nil then begin
          related.evaluation^.wallClock:=TEpikTimer.create(nil);
          related.evaluation^.wallClock.clear;
          related.evaluation^.wallClock.start;
        end;
      finally
        leaveCriticalSection(related.evaluation^.primaryContext.contextCS);
      end;
    end;
    result:=related.evaluation^.wallClock.elapsed;
  end;

PROCEDURE T_evaluationGlobals.timeBaseComponent(CONST component: T_profileCategory);
  begin
    with timingInfo do timeSpent[component]:=primaryContext.wallclockTime-timeSpent[component];
  end;

FUNCTION T_context.getNewAsyncContext(VAR recycler:T_recycler; CONST local: boolean): P_context;
  VAR parentAccess:AccessLevel;
  begin
    if not(tco_createDetachedTask in options) then exit(nil);
    if local then parentAccess:=ACCESS_READONLY
             else parentAccess:=ACCESS_BLOCKED;
    result:=contextPool.newContext(recycler,@self,parentAccess);
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

FUNCTION T_context.setAllowedSideEffectsReturningPrevious(
  CONST se: T_sideEffects): T_sideEffects;
  begin
    result:=allowedSideEffects;
    allowedSideEffects:=se;
  end;

FUNCTION T_context.getFutureEnvironment(VAR recycler:T_recycler) : P_context;
  begin
    enterCriticalSection(contextCS);
    try
      result:=contextPool.newContext(recycler,@self,ACCESS_READONLY);
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_context.beginEvaluation;
  begin
    {$ifdef debugMode}
    if state<>fts_pending then raise Exception.create('Wrong state before calling T_threadContext.beginEvaluation');
    {$endif}
    enterCriticalSection(contextCS);
    try
      state:=fts_evaluating;
    finally
      leaveCriticalSection(contextCS);
    end;
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
      state:=fts_finished;
      if related.parent<>nil then related.parent^.dropChildContext;
      related.parent:=nil;
      {$ifdef fullVersion}
      callStack.clear;
      parentCustomForm:=nil;
      {$endif}
      if recyclerOrNil=nil then     noRecycler_disposeScope(valueScope)
                           else recyclerOrNil^.disposeScope(valueScope);
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

FUNCTION T_evaluationGlobals.queuedFuturesCount:longint;
  begin result:=taskQueue.queuedCount; end;

PROCEDURE T_context.raiseCannotApplyError(CONST ruleWithType: string;
  CONST parameters: P_listLiteral; CONST location: T_tokenLocation;
  CONST suffix: string; CONST missingMain: boolean);
  VAR message:string;
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
            currentTask^.clearContext;
            dispose(currentTask,destroy);
          end else currentTask^.evaluate(recycler);
          sleepCount:=0;
        end;
      until (sleepCount>=MS_IDLE_BEFORE_QUIT) or    //nothing to do
            (taskQueue.destructionPending) or
            not(primaryContext.messages^.continueEvaluation) or //error ocurred
            (not(isMemoryInComfortZone) and (taskQueue.poolThreadsRunning>1)); //memory panic with more than 1 pool thread running
      result:=0;
      interlockedDecrement(taskQueue.poolThreadsRunning);
    end;
    recycler.cleanup;
  end;

CONSTRUCTOR T_context.create();
  begin
    initCriticalSection(contextCS);
    enterCriticalSection(contextCS);
    state:=fts_pending;
    {$ifdef fullVersion}
    callStack.create;
    {$endif}
    valueScope:=nil;
    related.childCount:=0;
    related.parent:=nil;
    related.evaluation:=nil;
    messages:=nil;
    leaveCriticalSection(contextCS);
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
    related.childCount:=0;
    related.parent:=nil;
    related.evaluation:=nil;
    leaveCriticalSection(contextCS);
    doneCriticalSection(contextCS);
  end;

CONSTRUCTOR T_queueTask.create(CONST volatile:boolean);
  begin;
    isVolatile:=volatile;
    context:=nil;
    initCriticalSection(taskCs);
  end;

PROCEDURE T_queueTask.defineAndEnqueueOrEvaluate(CONST newEnvironment:P_context; VAR recycler:T_recycler);
  begin
    enterCriticalSection(taskCs);
    try
      {$ifdef debugMode}
      if newEnvironment=nil then raise Exception.create('T_queueTask.defineAndEnqueue - newEnvironemnt must not be nil');
      {$endif}
      nextToEvaluate  :=nil;
      if context<>nil then contextPool.disposeContext(context);
      context:=newEnvironment;
      if not(isVolatile) and ((context^.related.evaluation^.taskQueue.queuedCount>TASKS_TO_QUEUE_PER_CPU*settings.cpuCount) or not(isMemoryInComfortZone))
      then evaluate(recycler)
      else context^.related.evaluation^.taskQueue.enqueue(@self,newEnvironment);
    finally
      leaveCriticalSection(taskCs);
    end;
  end;

PROCEDURE T_queueTask.clearContext;
  begin
    enterCriticalSection(taskCs);
    try
      if context<>nil then contextPool.disposeContext(context);
    finally
      leaveCriticalSection(taskCs);
    end;
  end;

DESTRUCTOR T_queueTask.destroy;
  VAR recycler:T_recycler;
  begin
    if context<>nil then begin
      recycler.initRecycler;
      contextPool.disposeContext(context);
      recycler.cleanup;
    end;
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

PROCEDURE T_taskQueue.enqueue(CONST task:P_queueTask; CONST context:P_context);
  PROCEDURE ensurePoolThreads();
    VAR aimPoolThreads:longint;
    begin
      if isMemoryInComfortZone
      then aimPoolThreads:=settings.cpuCount-1
      else aimPoolThreads:=1;
      if poolThreadsRunning<aimPoolThreads then begin
        interLockedIncrement(poolThreadsRunning);
        beginThread(@threadPoolThread,context^.related.evaluation);
      end;
    end;
  begin
    system.enterCriticalSection(cs);
    try
      inc(queuedCount);
      if first=nil then begin
        first:=task;
        last:=task;
      end else begin
        last^.nextToEvaluate:=task;
        last:=task;
      end;
      ensurePoolThreads();
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_taskQueue.dequeue: P_queueTask;
  begin
    system.enterCriticalSection(cs);
    try
      if queuedCount<=0
      then result:=nil
      else begin
        dec(queuedCount);
        result:=first;
        first:=first^.nextToEvaluate;
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
      if task^.isVolatile then begin
        task^.evaluate(recycler);
        task^.clearContext;
        dispose(task,destroy);
      end else task^.evaluate(recycler);
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
