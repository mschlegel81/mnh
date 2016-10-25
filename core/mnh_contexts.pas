UNIT mnh_contexts;
INTERFACE
USES sysutils,mnh_constants,mnh_tokens,mnh_basicTypes, mnh_out_adapters,mnh_litVar,myGenerics,EpikTimer{$ifdef fullVersion},myStringUtil{$endif};
TYPE
  T_valueStoreMarker=(vsm_none,vsm_nonBlockingVoid,vsm_blockingVoid,vsm_nonBlockingFirst,vsm_blockingFirst);
CONST
  C_voidOfBlocking:array[false..true] of T_valueStoreMarker=(vsm_nonBlockingVoid,vsm_blockingVoid);
  C_firstOfVoid   :array[vsm_nonBlockingVoid..vsm_blockingVoid] of T_valueStoreMarker=(vsm_nonBlockingFirst,vsm_blockingFirst);
TYPE
  T_valueStore=object
    private
      cs:TRTLCriticalSection;
      data:array of record
        marker:T_valueStoreMarker;
        v:P_namedVariable;
      end;
      PROCEDURE createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean);
      FUNCTION getVariable(CONST id:T_idString; OUT blockEncountered:boolean):P_namedVariable;
      PROCEDURE scopePush(CONST blocking:boolean);
      PROCEDURE scopePop;

    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      {$ifdef fullVersion}
      //For debugging:
      PROCEDURE reportVariables(VAR variableReport:T_variableReport);
      PROCEDURE writeStore;
      {$endif}
  end;

  T_contextOption=(cp_ask,
                   cp_spawnWorker,
                   cp_timing,              //not granted to child
                   {$ifdef fullVersion}
                   cp_profile,             //granted to child
                   cp_debug,
                   {$endif}
                   cp_createDetachedTask,
                   cp_queryParentValueStore,
                   cp_clearTimerOnStart,
                   cp_clearAdaptersOnStart,
                   cp_logEndOfEvaluation,
                   cp_beepOnError,
                   cp_notifyParentOfAsyncTaskEnd,
                   cp_disposeAdaptersOnDestruction);
  T_contextOptions=set of T_contextOption;
  T_contextType=(ct_normal{$ifdef fullVersion},ct_profiling,ct_debugging{$endif},ct_silentlyRunAlone);

CONST
  C_defaultOptions:array[T_contextType] of T_contextOptions=(
  {ct_normal}          [cp_ask,cp_timing,cp_spawnWorker,                    cp_createDetachedTask,cp_clearTimerOnStart,cp_clearAdaptersOnStart,cp_logEndOfEvaluation],
  {$ifdef fullVersion}
  {ct_profiling}       [cp_ask,cp_timing,cp_spawnWorker,cp_profile,         cp_createDetachedTask,cp_clearTimerOnStart,cp_clearAdaptersOnStart,cp_logEndOfEvaluation],
  {ct_debugging}       [cp_ask,cp_timing,               cp_profile,cp_debug,cp_createDetachedTask,cp_clearTimerOnStart,cp_clearAdaptersOnStart,cp_logEndOfEvaluation],
  {$endif}
  {ct_silentlyRunAlone}[                 cp_spawnWorker,                    cp_createDetachedTask,cp_clearTimerOnStart,cp_clearAdaptersOnStart]);

TYPE
  T_profileCategory=(pc_importing,pc_tokenizing,pc_declaration,pc_interpretation,pc_unknown,pc_total);

  T_profilingEntry=record
    id:T_idString;
    location:string;
    timeSpent_inclusive,
    timeSpent_exclusive:double;
    callCount:longint;
  end;

  T_callStack=array of record
    callerLocation:T_tokenLocation;
    callee:P_objectWithIdAndLocation;
    callParameters:P_listLiteral;
    calleeLiteral:P_expressionLiteral;
    {$ifdef fullVersion}
    timeForProfiling_inclusive,
    timeForProfiling_exclusive:double;
    {$endif}
  end;
  {$ifdef fullVersion}
  T_debuggingSnapshot=record
    location:T_tokenLocation;
    tokenStack:pointer;
    first:pointer;
    callStack:T_callStack;
  end;
  {$endif}

  T_profilingMap=specialize G_stringKeyMap<T_profilingEntry>;

  P_evaluationContext=^T_evaluationContext;
  T_evaluationContext=object
    private
      {$ifdef debugMode}
      evaluationIsRunning:boolean;
      {$endif}
      //privileges and obligations
      options:T_contextOptions;
      //links to other objects
      parentContext:P_evaluationContext;
      asyncChildCount:longint;
      initialAdapters:P_adapters;
      currentAdapters:P_adapters;
      //token recycling
      recycler:record
        dat:array[0..2047] of P_token;
        fill:longint;
      end;
      //local variables
      valueStore:T_valueStore;
      //call stack
      callStack:T_callStack;
      //timing and profiling
      wallClock: specialize G_lazyVar<TEpikTimer>;
      timingInfo:record
        startOfProfiling:double;
        timeSpent:array[pc_importing..pc_interpretation] of double;
      end;
      {$ifdef fullVersion}
      profilingAndDebuggingCriticalSection:TRTLCriticalSection;
      profilingMap:T_profilingMap;
      debuggingStepper:record
        breakpoints:array of T_searchTokenLocation;
        state:(breakSoonest,
               breakOnLineChange,
               breakOnStepOut,
               breakOnStepIn,
               runUntilBreakpoint,
               dontBreakAtAll,
               waitingForGUI);
        lastBreakLine:T_tokenLocation;
        lastBreakLevel:longint;
        snapshot:T_debuggingSnapshot;
      end;

      PROCEDURE addToProfilingMap(CONST id:T_idString; CONST location:ansistring; CONST dt_inclusive,dt_exclusive:double);
      {$endif}

    public
      CONSTRUCTOR createContext(CONST outAdapters:P_adapters; CONST contextType:T_contextType);
      PROCEDURE resetOptions(CONST contextType:T_contextType);
      DESTRUCTOR destroy;

      PROCEDURE resetForEvaluation(CONST package:P_objectWithPath);
      PROCEDURE afterEvaluation;

      //Basic property queries:
      PROPERTY adapters:P_adapters read currentAdapters;
      FUNCTION hasOption(CONST option:T_contextOption):boolean; inline;
      PROCEDURE removeOption(CONST option:T_contextOption);
      PROCEDURE addOption(CONST option:T_contextOption);
      //Delegation routines:
      PROCEDURE notifyAsyncTaskEnd;
      FUNCTION getNewAsyncContext:P_evaluationContext;
      FUNCTION enterTryStatementReturningPreviousAdapters:P_adapters;
      PROCEDURE leaveTryStatementReassumingPreviousAdapters(CONST previousAdapters:P_adapters; CONST tryBodyFailed:boolean);
      PROCEDURE attachWorkerContext(CONST newParent:P_evaluationContext);
      PROCEDURE detachWorkerContext(CONST expectedParent:P_evaluationContext);
      //Recycler routines:
      FUNCTION disposeToken(p:P_token):P_token; inline;
      PROCEDURE cascadeDisposeToken(VAR p:P_token);
      FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer=nil):P_token; inline;
      FUNCTION newToken(CONST original:T_token):P_token; inline;
      FUNCTION newToken(CONST original:P_token):P_token; inline;
      //Local scope routines:
      PROCEDURE scopePush(CONST blocking:boolean); inline;
      PROCEDURE scopePop; inline;
      PROCEDURE createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean); inline;
      FUNCTION getVariableValue(CONST id:T_idString):P_literal; inline;
      PROCEDURE setVariableValue(CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation);
      FUNCTION mutateVariableValue(CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
      //call stack routines
      PROCEDURE callStackPush(CONST callerLocation:T_tokenLocation; CONST callee:P_objectWithIdAndLocation; CONST callParameters:P_listLiteral; CONST expressionLiteral:P_expressionLiteral);
      PROCEDURE callStackPop();
      {$ifdef fullVersion}
      PROCEDURE reportVariables(VAR variableReport:T_variableReport);
      {$endif}
      PROCEDURE printCallStack(CONST messageType:T_messageType);
      PROCEDURE clearCallStack;
      //clock routines
      FUNCTION wallclockTime:double;
      FUNCTION wantBasicTiming:boolean;
      PROCEDURE timeBaseComponent(CONST component:T_profileCategory);

      {$ifdef fullVersion}
      PROCEDURE stepping(CONST first:P_token; CONST stack:pointer);
      //GUI interaction
      PROCEDURE haltEvaluation;
      PROCEDURE clearBreakpoints;
      PROCEDURE addBreakpoint(CONST fileName:string; CONST line:longint);
      PROCEDURE doContinue;
      PROCEDURE doStepInto;
      PROCEDURE doStepOut;
      PROCEDURE doStep;
      PROCEDURE doMicrostep;
      PROCEDURE doStop;
      FUNCTION paused:boolean;

      FUNCTION getDebuggingSnapshot:T_debuggingSnapshot;
      {$endif}
  end;


VAR showProfilingTableCallback:PROCEDURE(CONST L:P_listLiteral)=nil;
IMPLEMENTATION
CONSTRUCTOR T_valueStore.create;
  begin
    system.initCriticalSection(cs);
    setLength(data,0);
  end;

DESTRUCTOR T_valueStore.destroy;
  begin
    clear;
    system.doneCriticalSection(cs);
  end;

PROCEDURE T_valueStore.clear;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(data)-1 do if data[i].v<>nil then dispose(data[i].v,destroy);
    setLength(data,0);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePush(CONST blocking:boolean);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(data);
    setLength(data,i+1);
    with data[i] do begin
      v:=nil;
      marker:=C_voidOfBlocking[blocking];
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePop;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(data);
    repeat
      dec(i);
      with data[i] do if v<>nil then begin
        dispose(v,destroy);
        v:=nil;
      end;
    until data[i].marker<>vsm_none;
    setLength(data,i);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_valueStore.getVariable(CONST id:T_idString; OUT blockEncountered:boolean):P_namedVariable;
  VAR i:longint;
  begin
    blockEncountered:=false;
    result:=nil;
    for i:=length(data)-1 downto 0 do with data[i] do
    if (v<>nil) and (v^.getId=id) then begin
      result:=v;
      exit(result);
    end else if marker in [vsm_blockingFirst,vsm_blockingVoid] then begin
      blockEncountered:=true;
      exit(nil);
    end;
  end;

PROCEDURE T_valueStore.createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(data);
    with data[i-1] do if marker in [vsm_blockingVoid,vsm_nonBlockingVoid] then begin
      marker:=C_firstOfVoid[marker];
      new(v,create(id,value,readonly));
      system.leaveCriticalSection(cs);
      exit;
    end;
    setLength(data,i+1);
    with data[i] do begin
      marker:=vsm_none;
      new(v,create(id,value,readonly));
    end;
    system.leaveCriticalSection(cs);
  end;

{$ifdef fullVersion}
PROCEDURE T_valueStore.reportVariables(VAR variableReport:T_variableReport);
  VAR i :longint;
      i0:longint=0;
      up:longint=0;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(data)-1 do case data[i].marker of
      vsm_nonBlockingVoid,vsm_nonBlockingFirst: inc(up);
      vsm_blockingVoid,vsm_blockingFirst: begin up:=1; i0:=i; end;
    end;
    for i:=i0 to length(data)-1 do begin
      if data[i].marker<>vsm_none then dec(up);
      with data[i] do if v<>nil then begin
        if up=0 then variableReport.addVariable(v,'local')
                else variableReport.addVariable(v,'local (+'+intToStr(up)+')');
      end;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.writeStore;
  FUNCTION vts(CONST v:P_namedVariable):string;
    begin
      if v=nil then result:='<nil>'
               else result:=v^.toString(100);
    end;

  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(data)-1 do writeln(data[i].marker,' ',vts(data[i].v));
    system.leaveCriticalSection(cs);
  end;

{$endif}
FUNCTION initTimer:TEpikTimer;
  begin
    result:=TEpikTimer.create(nil);
    result.clear;
    result.start;
  end;

PROCEDURE disposeTimer(t:TEpikTimer);
  begin
    t.destroy;
  end;

{$ifdef fullVersion}
PROCEDURE T_evaluationContext.addToProfilingMap(CONST id: T_idString; CONST location: ansistring; CONST dt_inclusive,dt_exclusive:double);
  VAR profilingEntry:T_profilingEntry;
  begin
    if parentContext<>nil then begin
      parentContext^.addToProfilingMap(id,location,dt_inclusive,dt_exclusive);
      exit;
    end;
    enterCriticalSection(profilingAndDebuggingCriticalSection);
    if profilingMap.containsKey(location,profilingEntry)
    then with profilingEntry do begin
      timeSpent_exclusive:=timeSpent_exclusive+dt_exclusive;
      timeSpent_inclusive:=timeSpent_inclusive+dt_inclusive;
      inc(callCount);
    end else begin
      profilingEntry.id:=id;
      profilingEntry.location:=location;
      profilingEntry.timeSpent_exclusive:=dt_exclusive;
      profilingEntry.timeSpent_inclusive:=dt_inclusive;
      profilingEntry.callCount:=1;
    end;
    profilingMap.put(location,profilingEntry);
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;
{$endif}

CONSTRUCTOR T_evaluationContext.createContext(CONST outAdapters: P_adapters; CONST contextType: T_contextType);
  VAR i:longint;
  begin
    options :=C_defaultOptions[contextType];
    parentContext:=nil;
    with recycler do begin
      for i:=0 to length(dat)-1 do dat[i]:=nil;
      fill:=0;
    end;
    valueStore.create;
    initialAdapters:=outAdapters;
    currentAdapters:=outAdapters;
    setLength(callStack,0);
    wallClock.create(@initTimer,@disposeTimer);
    {$ifdef fullVersion}
    system.initCriticalSection(profilingAndDebuggingCriticalSection);
    profilingMap.create();
    {$endif}
    {$ifdef debugMode}
    evaluationIsRunning:=false;
    {$endif}
  end;

PROCEDURE T_evaluationContext.resetOptions(CONST contextType: T_contextType);
  begin
    {$ifdef debugMode}
    if evaluationIsRunning then raise Exception.create('Options must not be changed during evaluation!');
    {$endif}
    options:=C_defaultOptions[contextType];
  end;

DESTRUCTOR T_evaluationContext.destroy;
  begin
    with recycler do begin
      while fill>0 do begin
        dec(fill);
        try
          dispose(dat[fill],destroy);
        except
          dat[fill]:=nil;
        end;
      end;
    end;
    valueStore.destroy;
    clearCallStack;
    wallClock.destroy;
    if cp_disposeAdaptersOnDestruction in options then dispose(adapters,destroy);
    {$ifdef fullVersion}
    profilingMap.destroy;
    system.doneCriticalSection(profilingAndDebuggingCriticalSection);
    {$endif}
  end;

PROCEDURE T_evaluationContext.resetForEvaluation(CONST package: P_objectWithPath);
  VAR pc:T_profileCategory;
  begin
    {$ifdef debugMode}
    if evaluationIsRunning then raise Exception.create('Evaluation already is running!');
    evaluationIsRunning:=true;
    {$endif}

    valueStore.clear;
    clearCallStack;
    {$ifdef fullVersion}
    profilingMap.clear;
    {$endif}
    if cp_clearTimerOnStart in options then begin
      if {$ifdef fullVersion} (cp_profile in options) or {$endif} initialAdapters^.doShowTimingInfo then begin
        wallClock.value.clear;
        wallClock.value.start;
        with timingInfo do begin
          for pc:=low(timeSpent) to high(timeSpent) do timeSpent[pc]:=0;
          startOfProfiling:=wallClock.value.elapsed;
        end;
      end;
      {$ifdef fullVersion}
      if (cp_debug in options) then begin
        debuggingStepper.state:=runUntilBreakpoint;
        debuggingStepper.lastBreakLevel:=-1;
        debuggingStepper.lastBreakLine.line:=-1;
        debuggingStepper.lastBreakLine.package:=package;
      end;
      {$endif}
    end;
    currentAdapters:=initialAdapters;
    if cp_clearAdaptersOnStart in options then begin
      initialAdapters^.clearAll;
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
      '                    ');

    VAR timeValue :array[T_profileCategory] of double;
        timeString:array[T_profileCategory] of string;
        cat:T_profileCategory;

        timeUnit:string;
        longest:longint=0;
        formatString:ansistring;

    FUNCTION fmt(CONST d:double):string; begin result:=formatFloat(formatString,d); if length(result)>longest then longest:=length(result); end;
    FUNCTION fmt(CONST s:string):string; begin result:=StringOfChar(' ',longest-length(s))+s+timeUnit; end;
    begin
      timeValue[pc_total]:=wallClock.value.elapsed-timingInfo.startOfProfiling;
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
      for cat:=low(T_profileCategory) to high(T_profileCategory) do begin
        if cat=high(T_profileCategory) then
        adapters^.raiseCustomMessage(mt_timing_info,StringOfChar('-',length(timeString[cat])),C_nilTokenLocation);
        adapters^.raiseCustomMessage(mt_timing_info,                        timeString[cat]  ,C_nilTokenLocation);
      end;
    end;
  {$ifdef fullVersion}
  PROCEDURE logProfilingInfo;
    FUNCTION nicestTime(CONST seconds:double):string;
      begin
         result:=formatFloat('0.000',seconds*1E3)+C_invisibleTabChar+'ms';
      end;

    VAR profilingData:T_profilingMap.VALUE_TYPE_ARRAY;
        data:P_listLiteral;
        swapTemp:T_profilingEntry;
        lines:T_arrayOfString;
        i,j:longint;

    FUNCTION showTableMessage:T_storedMessage;
      begin
        result.messageType:=mt_displayTable;
        result.location:=C_nilTokenLocation;
        result.simpleMessage:='';
        result.multiMessage:=C_EMPTY_STRING_ARRAY;
      end;

    begin
      profilingData:=profilingMap.valueSet;
      for j:=0 to length(profilingData)-1 do for i:=0 to j-1 do if profilingData[i].timeSpent_inclusive<profilingData[j].timeSpent_inclusive then begin
        swapTemp:=profilingData[i];
        profilingData[i]:=profilingData[j];
        profilingData[j]:=swapTemp;
      end;
      setLength(lines,length(profilingData)+1);
      if Assigned(showProfilingTableCallback) then
      data:=newListLiteral(length(profilingData)+1)^.append(newListLiteral(5)^
            .appendString('Location')^
            .appendString('id')^
            .appendString('count')^
            .appendString('inclusive (ms)')^
            .appendString('exclusive (ms)'),false);
      lines[0]:='Location'+C_tabChar+
                'id'+C_tabChar+
                'count'+C_tabChar+
                'inclusive'+C_invisibleTabChar+' time'+C_tabChar+
                'exclusive'+C_invisibleTabChar+' time';

      for i:=0 to length(profilingData)-1 do with profilingData[i] do begin
        lines[i+1]:=location+C_tabChar+
                    id+C_tabChar+
                    intToStr(callCount)+C_tabChar+
                    nicestTime(timeSpent_inclusive)+C_tabChar+
                    nicestTime(timeSpent_exclusive);
        if Assigned(showProfilingTableCallback)
        then data^.append(newListLiteral(5)^
                          .appendString(location)^
                          .appendString(id)^
                          .appendInt(callCount)^
                          .appendReal(timeSpent_inclusive*1E3)^
                          .appendReal(timeSpent_exclusive*1E3),false);
      end;
      lines:=formatTabs(lines);
      if Assigned(showProfilingTableCallback) then begin
        showProfilingTableCallback(data);
        disposeLiteral(data);
        for j:=0 to adapters^.adapterCount-1 do if adapters^.getAdapter(j)^.adapterType=at_gui then adapters^.getAdapter(j)^.append(showTableMessage);
      end;
      for i:=0 to length(lines)-1 do adapters^.raiseCustomMessage(mt_timing_info,lines[i],C_nilTokenLocation);
    end;
  {$endif}

  begin
    {$ifdef debugMode}
    if not(evaluationIsRunning) then raise Exception.create('Evaluation is not running!');
    evaluationIsRunning:=false;
    {$endif}

    valueStore.clear;
    clearCallStack;
    if cp_logEndOfEvaluation in options then begin
      initialAdapters^.stopEvaluation;
      while asyncChildCount>0 do begin
        ThreadSwitch;
        sleep(1);
      end;
      initialAdapters^.logEndOfEvaluation;
      if wantBasicTiming then logTimingInfo;
      {$ifdef fullVersion}
      if (cp_profile in options) and adapters^.doShowTimingInfo then logProfilingInfo;
      {$endif}
      if (cp_beepOnError in options) and adapters^.triggersBeep then beep;
    end;
    if cp_notifyParentOfAsyncTaskEnd in options then parentContext^.notifyAsyncTaskEnd;
  end;

FUNCTION T_evaluationContext.hasOption(CONST option: T_contextOption): boolean;
  begin result:=option in options; end;

PROCEDURE T_evaluationContext.removeOption(CONST option:T_contextOption);
  begin
    {$ifdef debugMode}
    if evaluationIsRunning then raise Exception.create('Options must not be changed during evaluation!');
    {$endif}
    options:=options-[option];
  end;

PROCEDURE T_evaluationContext.addOption(CONST option:T_contextOption);
  begin
    {$ifdef debugMode}
    if evaluationIsRunning then raise Exception.create('Options must not be changed during evaluation!');
    {$endif}
    options:=options+[option];
  end;

PROCEDURE T_evaluationContext.notifyAsyncTaskEnd;
  begin
    interlockedDecrement(asyncChildCount);
  end;

FUNCTION T_evaluationContext.getNewAsyncContext:P_evaluationContext;
  begin
    if not(cp_createDetachedTask in options) then exit(nil);
    if parentContext=nil then begin
      InterLockedIncrement(asyncChildCount);
      new(result,createContext(initialAdapters,ct_normal));
      result^.parentContext:=@self;
      result^.options:=options-[cp_timing,cp_queryParentValueStore];
      result^.options:=[cp_notifyParentOfAsyncTaskEnd];
    end else begin
      result:=parentContext^.getNewAsyncContext();
    end;
  end;

FUNCTION T_evaluationContext.enterTryStatementReturningPreviousAdapters: P_adapters;
  begin
    result:=currentAdapters;
    currentAdapters:=result^.collectingClone;
  end;

PROCEDURE T_evaluationContext.leaveTryStatementReassumingPreviousAdapters(CONST previousAdapters: P_adapters; CONST tryBodyFailed: boolean);
  begin
    previousAdapters^.copyDataFromCollectingCloneDisposing(currentAdapters,tryBodyFailed);
    currentAdapters:=previousAdapters;
  end;

PROCEDURE T_evaluationContext.attachWorkerContext(CONST newParent: P_evaluationContext);
  begin
    initialAdapters:=newParent^.initialAdapters;
    currentAdapters:=newParent^.currentAdapters;
    parentContext:=newParent;
    options:=parentContext^.options-[cp_timing]+[cp_queryParentValueStore];
  end;

PROCEDURE T_evaluationContext.detachWorkerContext(CONST expectedParent: P_evaluationContext);
  begin
    {$ifdef debugMode}
    if parentContext<>expectedParent then raise Exception.create('Detaching excepts another context');
    {$endif}
    parentContext:=nil;
    options:=options-[cp_queryParentValueStore];
  end;


FUNCTION T_evaluationContext.disposeToken(p: P_token): P_token;
  begin with recycler do begin
    if p=nil then exit(nil);
    result:=p^.next;
    if (fill>=length(dat))
    then dispose(p,destroy)
    else begin
      p^.undefine;
      dat[fill]:=p;
      inc(fill);
    end;
  end; end;

PROCEDURE T_evaluationContext.cascadeDisposeToken(VAR p: P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_evaluationContext.newToken(CONST tokenLocation: T_tokenLocation; CONST tokenText: ansistring; CONST tokenType: T_tokenType; CONST ptr: pointer): P_token;
  begin with recycler do begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end; end;

FUNCTION T_evaluationContext.newToken(CONST original: T_token): P_token;
  begin with recycler do begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end; end;

FUNCTION T_evaluationContext.newToken(CONST original: P_token): P_token;
  begin with recycler do begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original^);
    result^.next:=nil;
  end; end;

PROCEDURE T_evaluationContext.scopePush(CONST blocking: boolean);
  begin
    valueStore.scopePush(blocking);
  end;

PROCEDURE T_evaluationContext.scopePop;
  begin
    valueStore.scopePop;
  end;

PROCEDURE T_evaluationContext.createVariable(CONST id: T_idString; CONST value: P_literal; CONST readonly: boolean);
  begin
    valueStore.createVariable(id,value,readonly);
  end;

FUNCTION T_evaluationContext.getVariableValue(CONST id: T_idString): P_literal;
  VAR named:P_namedVariable;
      blocked:boolean;
  begin
    system.enterCriticalSection(valueStore.cs);
    named:=valueStore.getVariable(id,blocked);
    if named<>nil then result:=named^.getValue
    else if not(blocked) and (parentContext<>nil) and (cp_queryParentValueStore in options) then result:=parentContext^.getVariableValue(id)
    else result:=nil;
    system.leaveCriticalSection(valueStore.cs);
  end;

PROCEDURE T_evaluationContext.setVariableValue(CONST id:T_idString; CONST value:P_literal; CONST location:T_tokenLocation);
  VAR named:P_namedVariable;
      blocked:boolean;
  begin
    system.enterCriticalSection(valueStore.cs);
    named:=valueStore.getVariable(id,blocked);
    if named<>nil then named^.setValue(value)
    else if not(blocked) and (parentContext<>nil) and (cp_queryParentValueStore in options) then parentContext^.setVariableValue(id,value,location)
    else adapters^.raiseError('Cannot assign value to unknown local variable '+id,location);
    system.leaveCriticalSection(valueStore.cs);
  end;

FUNCTION T_evaluationContext.mutateVariableValue(CONST id:T_idString; CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  VAR named:P_namedVariable;
      blocked:boolean;
  begin
    system.enterCriticalSection(valueStore.cs);
    named:=valueStore.getVariable(id,blocked);
    if named<>nil then result:=named^.mutate(mutation,RHS,location,adapters^)
    else if not(blocked) and (parentContext<>nil) and (cp_queryParentValueStore in options) then result:=parentContext^.mutateVariableValue(id,mutation,RHS,location)
    else begin
      adapters^.raiseError('Cannot mutate unknown local variable '+id,location);
      result:=nil;
    end;
    system.leaveCriticalSection(valueStore.cs);
  end;

PROCEDURE T_evaluationContext.callStackPush(CONST callerLocation: T_tokenLocation; CONST callee: P_objectWithIdAndLocation; CONST callParameters: P_listLiteral; CONST expressionLiteral:P_expressionLiteral);
  VAR topIdx:longint;
  begin
    topIdx:=length(callStack);
    setLength(callStack,topIdx+1);
    callStack[topIdx].callerLocation:=callerLocation;
    callStack[topIdx].callee        :=callee;
    callStack[topIdx].callParameters:=callParameters;
    callStack[topIdx].calleeLiteral :=expressionLiteral;
    if callParameters<>nil then callParameters^.rereference;
    if expressionLiteral<>nil then expressionLiteral^.rereference;
    {$ifdef fullVersion}
    if cp_profile in options then begin
      if topIdx>0 then with callStack[topIdx-1] do timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
      callStack[topIdx].timeForProfiling_exclusive:=wallclockTime;
      callStack[topIdx].timeForProfiling_inclusive:=wallclockTime;
    end;
    {$endif}
  end;

PROCEDURE T_evaluationContext.callStackPop;
  VAR topIdx:longint;
  begin
    topIdx:=length(callStack)-1;
    if topIdx<0 then exit;
    {$ifdef fullVersion}
    if cp_profile in options then begin
      with callStack[topIdx] do begin
        timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
        timeForProfiling_inclusive:=wallclockTime-timeForProfiling_inclusive;
        addToProfilingMap(callee^.getId,callee^.getLocation,timeForProfiling_inclusive, timeForProfiling_exclusive);
       end;
      if topIdx>0 then with callStack[topIdx-1] do timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
    end;
    {$endif}
    with callStack[topIdx] do begin
      if callParameters<>nil then disposeLiteral(callParameters);
      if calleeLiteral <>nil then disposeLiteral(calleeLiteral);
    end;
    setLength(callStack,topIdx);
  end;
{$ifdef fullVersion}
PROCEDURE T_evaluationContext.reportVariables(
  VAR variableReport: T_variableReport);
  begin
    if parentContext<>nil then parentContext^.reportVariables(variableReport);
    valueStore.reportVariables(variableReport);
  end;
{$endif}

PROCEDURE T_evaluationContext.printCallStack(CONST messageType: T_messageType);
  VAR i:longint;
      p:P_evaluationContext;
  begin
    p:=parentContext;
    if adapters=nil then exit;
    for i:=length(callStack)-1 downto 0 do with callStack[i] do
    adapters^.raiseCustomMessage(messageType,callee^.getId+' '+toParameterListString(callParameters,true,50),callerLocation);
    if p<>nil then p^.printCallStack(messageType);
  end;

PROCEDURE T_evaluationContext.clearCallStack;
  begin
    //If the evaluation was not finished cleanly, the call stack may not be empty
    //Make sure that the elements are profiled correctly and references to expressions are dropped
    while length(callStack)>0 do callStackPop();
  end;

FUNCTION T_evaluationContext.wallclockTime: double;
  begin
    if parentContext<>nil then exit(parentContext^.wallclockTime);
    result:=wallClock.value.elapsed;
  end;

FUNCTION T_evaluationContext.wantBasicTiming: boolean;
  begin
    result:=(cp_timing in options) and (adapters^.doShowTimingInfo);
  end;

PROCEDURE T_evaluationContext.timeBaseComponent(
  CONST component: T_profileCategory);
  begin
    timingInfo.timeSpent[component]:=wallclockTime-timingInfo.timeSpent[component];
  end;
{$ifdef fullVersion}
PROCEDURE T_evaluationContext.stepping(CONST first: P_token; CONST stack: pointer);
  FUNCTION isEqualLine(CONST loc1,loc2:T_tokenLocation):boolean; inline;
    begin
      result:=(loc1.package=loc2.package) and
              (loc1.line   =loc2.line);
    end;

  FUNCTION isEqualLine(CONST loc1:T_tokenLocation; CONST loc2:T_searchTokenLocation):boolean; inline;
    begin
      result:=(loc1.package^.getPath=loc2.fileName) and
              (loc1.line            =loc2.line);
    end;

  FUNCTION breakpointEncountered:boolean;
    VAR i:longint;
    begin
      result:=false;
      with debuggingStepper do for i:=0 to length(breakpoints)-1 do if isEqualLine(first^.location,breakpoints[i]) then exit(true);
    end;

  PROCEDURE prepareSnapshot;
    VAR i:longint;
    begin
      debuggingStepper.snapshot.location:=first^.location;
      debuggingStepper.snapshot.tokenStack:=stack;
      debuggingStepper.snapshot.first:=first;
      setLength(debuggingStepper.snapshot.callStack,length(callStack));
      for i:=0 to length(callStack)-1 do debuggingStepper.snapshot.callStack[i]:=callStack[i];
    end;

  VAR lineChanged:boolean;
  begin
    if not(cp_debug in options) or (debuggingStepper.state=dontBreakAtAll) then exit;
    if parentContext<>nil then parentContext^.stepping(first,stack);
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    wallClock.value.stop;
    with debuggingStepper do begin
      if state=breakSoonest then state:=waitingForGUI
      else begin
        lineChanged:=not(isEqualLine(lastBreakLine,first^.location));
        if (state=breakOnLineChange ) and ((length(callStack)< lastBreakLevel) or lineChanged) or
           (state=breakOnStepOut    ) and  (length(callStack)< lastBreakLevel) or
           (state=breakOnStepIn     ) and  (length(callStack)> lastBreakLevel) or
           (state=runUntilBreakpoint) and ((length(callStack)<>lastBreakLevel) or lineChanged) and breakpointEncountered
        then state:=waitingForGUI;
      end;

      if state=waitingForGUI then begin
        lastBreakLine:=first^.location;
        lastBreakLevel:=length(callStack);
        prepareSnapshot;
      end;
      while state=waitingForGUI do begin
        system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
        ThreadSwitch;
        sleep(1);
        system.enterCriticalSection(profilingAndDebuggingCriticalSection);
      end;
    end;
    wallClock.value.start;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.haltEvaluation;
  begin
    adapters^.haltEvaluation;
    if cp_debug in options then begin
      system.enterCriticalSection(profilingAndDebuggingCriticalSection);
      debuggingStepper.state:=dontBreakAtAll;
      system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
    end;
  end;

PROCEDURE T_evaluationContext.clearBreakpoints;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    setLength(debuggingStepper.breakpoints,0);
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.addBreakpoint(CONST fileName: string; CONST line: longint);
  VAR i:longint;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    with debuggingStepper do begin
      i:=length(breakpoints);
      setLength(breakpoints,i+1);
      breakpoints[i].fileName:=fileName;
      breakpoints[i].line    :=line;
    end;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.doContinue;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    debuggingStepper.state:=runUntilBreakpoint;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.doStepInto;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    debuggingStepper.state:=breakOnStepIn;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.doStepOut;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    debuggingStepper.state:=breakOnStepOut;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.doStep;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    debuggingStepper.state:=breakOnLineChange;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.doMicrostep;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    debuggingStepper.state:=breakSoonest;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.doStop;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    debuggingStepper.state:=dontBreakAtAll;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

FUNCTION T_evaluationContext.paused:boolean;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    result:=debuggingStepper.state=waitingForGUI;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

FUNCTION T_evaluationContext.getDebuggingSnapshot:T_debuggingSnapshot;
  begin
    system.enterCriticalSection(profilingAndDebuggingCriticalSection);
    result:=debuggingStepper.snapshot;
    system.leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;
{$endif}

end.
