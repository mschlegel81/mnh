UNIT mnh_contexts;
INTERFACE
USES //FPC/LCL libraries
     sysutils,
     //3rd party libraries
     EpikTimer,
     //my libraries
     myGenerics,
     //MNH:
     mnh_constants, mnh_basicTypes,
     mnh_out_adapters,mnh_litVar,
     mnh_tokens,
     tokenStack,valueStore,tokenRecycler,
     mnh_profiling{$ifdef fullVersion},mnh_debugging{$endif};
TYPE
  T_evaluationContextOption =(eco_ask,eco_spawnWorker,eco_profiling,eco_createDetachedTask,eco_timing,eco_debugging,eco_beepOnError);
  T_threadContextOption     =(tco_ask,tco_spawnWorker,tco_profiling,tco_createDetachedTask,tco_notifyParentOfAsyncTaskEnd);
  T_evaluationContextOptions=set of T_evaluationContextOption;
  T_threadContextOptions    =set of T_threadContextOption;
  T_evaluationContextType   =(ect_normal{$ifdef fullVersion},ect_profiling,ect_debugging{$endif},ect_silentlyRunAlone);

CONST
  C_defaultOptions:T_evaluationContextOptions=[eco_ask,eco_spawnWorker,eco_createDetachedTask];
  C_equivalentOption:array[tco_ask..tco_createDetachedTask] of T_evaluationContextOption=(eco_ask,eco_spawnWorker,eco_profiling,eco_createDetachedTask);

TYPE
  P_evaluationContext=^T_evaluationContext;
  P_threadContext=^T_threadContext;
  T_threadContext=object
    private
      //privileges and obligations
      options:T_threadContextOptions;
      //call stack
      parent:P_evaluationContext;
      callingContext:P_threadContext;
      callStack :T_callStack;
      CONSTRUCTOR createThreadContext(CONST parent_:P_evaluationContext; CONST outAdapters:P_adapters=nil);
    public
      recycler  :T_tokenRecycler;
      valueStore:T_valueStore;
      adapters  :P_adapters;
      CONSTRUCTOR createWorkerContext;
      DESTRUCTOR destroy;

      FUNCTION wallclockTime(CONST forceInit:boolean=false):double;
      PROCEDURE timeBaseComponent(CONST component: T_profileCategory);

      PROCEDURE doneEvaluating;
      FUNCTION getNewAsyncContext:P_threadContext;

      PROCEDURE attachWorkerContext(CONST valueScope:P_valueStore; CONST callingContext_:P_threadContext);
      PROCEDURE detachWorkerContext;

      FUNCTION enterTryStatementReturningPreviousAdapters:P_adapters;
      PROCEDURE leaveTryStatementReassumingPreviousAdapters(CONST previousAdapters: P_adapters; CONST tryBodyFailed: boolean);

      PROCEDURE callStackPush(CONST callerLocation:T_tokenLocation; CONST callee:P_objectWithIdAndLocation; CONST callParameters:P_listLiteral; CONST expressionLiteral:P_expressionLiteral);
      PROCEDURE callStackPush(CONST package:P_objectWithPath; CONST category:T_profileCategory; VAR calls:T_packageProfilingCalls);
      PROCEDURE callStackPop();
      PROCEDURE callStackPrint(CONST targetAdapters:P_adapters=nil);
      PROCEDURE callStackClear();

      PROCEDURE raiseCannotApplyError(CONST ruleWithType:string; CONST parameters:P_listLiteral; CONST location:T_tokenLocation; CONST suffix:T_arrayOfString; CONST missingMain:boolean=false);

      {$ifdef fullVersion}
      FUNCTION stepping(CONST first:P_token; CONST stack:P_tokenStack):boolean; inline;
      PROCEDURE reportVariables(VAR variableReport: T_variableReport);
      {$endif}

      PROPERTY threadOptions:T_threadContextOptions read options;
      PROCEDURE reduceExpression(VAR first:P_token; CONST callDepth:word); inline;
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
      PROCEDURE setupThreadContext(CONST context:P_threadContext);
    public
      CONSTRUCTOR create(CONST outAdapters:P_adapters);
      CONSTRUCTOR createAndResetSilentContext(CONST package:P_objectWithPath);
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
  end;

VAR reduceExpressionCallback:PROCEDURE(VAR first:P_token; CONST callDepth:word; VAR context:T_threadContext);
    subruleReplacesCallback :FUNCTION(CONST subrulePointer:pointer; CONST param:P_listLiteral; CONST callLocation:T_tokenLocation; OUT firstRep,lastRep:P_token; VAR context:T_threadContext; CONST useUncurryingFallback:boolean):boolean;
IMPLEMENTATION
VAR globalLock:TRTLCriticalSection;
CONSTRUCTOR T_threadContext.createThreadContext(CONST parent_:P_evaluationContext; CONST outAdapters:P_adapters=nil);
  begin
    recycler  .create;
    valueStore.create;
    callStack .create;
    parent        :=parent_;
    adapters      :=outAdapters;
    callingContext:=nil;
    if adapters=nil then adapters:=parent^.adapters;
  end;

CONSTRUCTOR T_threadContext.createWorkerContext;
  begin
    recycler  .create;
    valueStore.create;
    callStack .create;
    parent        :=nil;
    adapters      :=nil;
    callingContext:=nil;
  end;

DESTRUCTOR T_threadContext.destroy;
  begin
    valueStore.destroy;
    recycler  .destroy;
  end;

CONSTRUCTOR T_evaluationContext.create(CONST outAdapters:P_adapters);
  begin
    wallClock:=nil;
    {$ifdef fullVersion}
    profiler:=nil;
    debuggingStepper:=nil;
    {$endif}
    contextAdapters:=outAdapters;
    disposeAdaptersOnDestruction:=false;
    detachedAsyncChildCount:=0;
    new(primaryThreadContext,createThreadContext(@self,adapters));
  end;

CONSTRUCTOR T_evaluationContext.createAndResetSilentContext(CONST package:P_objectWithPath);
  VAR tempAdapters:P_adapters;
  begin
    new(tempAdapters,create);
    create(tempAdapters);
    disposeAdaptersOnDestruction:=true;
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
    if disposeAdaptersOnDestruction then dispose(contextAdapters,destroy);
  end;

PROCEDURE T_evaluationContext.resetForEvaluation(CONST package:P_objectWithPath; CONST doProfiling,doDebugging,silentMode:boolean);
  VAR pc:T_profileCategory;
  begin
    //set options
    options:=[eco_ask,eco_spawnWorker,eco_createDetachedTask,eco_beepOnError];
    if adapters^.doShowTimingInfo then options:=options+[eco_timing];
    if doProfiling then options:=options+[eco_profiling];
    if doDebugging then options:=options+[eco_debugging]-[eco_createDetachedTask,eco_spawnWorker];
    if silentMode  then options:=options-[eco_ask,eco_beepOnError,eco_debugging,eco_timing,eco_profiling];
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
      if debuggingStepper=nil then new(debuggingStepper,create);
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
    context^.callStack.clear;
    context^.valueStore.clear;
    context^.adapters:=adapters;
  end;

{$ifdef fullVersion}
FUNCTION T_evaluationContext.stepper:P_debuggingStepper;
  begin
    if debuggingStepper=nil then new(debuggingStepper,create);
    result:=debuggingStepper;
  end;

FUNCTION T_evaluationContext.isPaused:boolean;
  begin
    result:=(debuggingStepper<>nil) and debuggingStepper^.paused;
  end;
{$endif}

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
    callStack.clear;
    valueStore.clear;
  end;

FUNCTION T_threadContext.getNewAsyncContext:P_threadContext;
  begin
    if not(tco_createDetachedTask in options) then exit(nil);
    interLockedIncrement(parent^.detachedAsyncChildCount);
    new(result,createThreadContext(nil,adapters));
    result^.options:=options+[tco_notifyParentOfAsyncTaskEnd];
  end;

PROCEDURE T_threadContext.attachWorkerContext(CONST valueScope:P_valueStore; CONST callingContext_:P_threadContext);
  begin
    callingContext:=callingContext_;
    parent        :=callingContext^.parent;
    adapters      :=callingContext^.adapters;
    options       :=callingContext^.options;
    valueStore.clear;
    valueStore.parentStore:=valueScope;
    callStack.clear;
  end;

PROCEDURE T_threadContext.detachWorkerContext;
  begin
    {$ifdef debugMode}
    if not(valueStore.isEmpty) and (adapters^.noErrors) then raise Exception.create('valueStore must be empty on detach');
    {$endif}
    parent:=nil;
    callingContext:=nil;
    valueStore.clear;
    valueStore.parentStore:=nil;
    callStack.clear;
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

PROCEDURE T_threadContext.callStackPush(CONST callerLocation: T_tokenLocation; CONST callee: P_objectWithIdAndLocation; CONST callParameters: P_listLiteral; CONST expressionLiteral:P_expressionLiteral);
  begin
    callStack.push(wallclockTime,callerLocation,callee,callParameters,expressionLiteral);
  end;

PROCEDURE T_threadContext.callStackPush(CONST package:P_objectWithPath; CONST category:T_profileCategory; VAR calls:T_packageProfilingCalls);
  begin
    if calls[category]=nil then new(calls[category],create(package,category));
    callStack.push(wallclockTime,calls[category]^.getLocation,calls[category],nil,nil);
  end;

PROCEDURE T_threadContext.callStackPop;
  begin
    callStack.pop({$ifdef fullVersion}wallclockTime,parent^.profiler{$endif});
  end;
{$ifdef fullVersion}
PROCEDURE T_threadContext.reportVariables(VAR variableReport: T_variableReport);
  begin
    if callingContext<>nil then callingContext^.reportVariables(variableReport);
    valueStore.reportVariables(variableReport);
  end;
{$endif}

PROCEDURE T_threadContext.reduceExpression(VAR first:P_token; CONST callDepth:word); begin reduceExpressionCallback(first,callDepth,self); end;


PROCEDURE T_threadContext.callStackPrint(CONST targetAdapters:P_adapters=nil);
  VAR p:P_threadContext;
      a:P_adapters;
  begin
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

INITIALIZATION
  initCriticalSection(globalLock);
FINALIZATION
  doneCriticalSection(globalLock);

end.
