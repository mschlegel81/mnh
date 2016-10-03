UNIT mnh_contexts;
INTERFACE
USES sysutils,mnh_constants,mnh_tokens,mnh_basicTypes, mnh_out_adapters,mnh_litVar,myGenerics,EpikTimer,myStringUtil;
TYPE
  T_valueStoreMarker=(vsm_none,vsm_nonBlockingVoid,vsm_blockingVoid,vsm_nonBlockingFirst,vsm_blockingFirst);
CONST
  C_voidOfBlocking:array[false..true] of T_valueStoreMarker=(vsm_nonBlockingVoid,vsm_blockingVoid);
  C_firstOfVoid   :array[vsm_nonBlockingVoid..vsm_blockingVoid] of T_valueStoreMarker=(vsm_nonBlockingFirst,vsm_blockingFirst);
TYPE
  T_valueStore=object
    cs:TRTLCriticalSection;
    data:array of record
      marker:T_valueStoreMarker;
      v:P_namedVariable;
    end;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clear;
    PROCEDURE scopePush(CONST blocking:boolean);
    PROCEDURE scopePop;
    FUNCTION getVariable(CONST id:T_idString):P_namedVariable;
    PROCEDURE createVariable(CONST id:T_idString; CONST value:P_literal; CONST readonly:boolean);
    {$ifdef FULLVERSION}
    //For debugging:
    PROCEDURE reportVariables(VAR variableReport:T_variableReport);
    {$endif}
  end;

  T_contextPrivilege=(cp_ask,
                      cp_spawnWorker,
                      cp_profile,             //granted to child
                      cp_timing,              //not granted to child
                      {$ifdef fullVersion}
                      cp_debug,
                      {$endif}
                      cp_createDetachedTask,
                      cp_queryParentValueStore,
                      cp_clearAdaptersAndTimerOnStartOfEvaluation,
                      cp_logEndOfEvaluation,
                      cp_notifyParentOfAsyncTaskEnd,
                      cp_pushProfliningInfoToParent,
                      cp_disposeAdaptersOnDestruction);
  T_contextPrivilegeSet=set of T_contextPrivilege;
  T_contextType=(ct_normal,ct_profiling{$ifdef fullVersion},ct_debugging{$endif},ct_silentlyRunAlone);

CONST
  C_initialPrivileges:array[T_contextType] of T_contextPrivilegeSet=(
  {ct_normal}          [cp_ask,cp_timing,cp_spawnWorker,                    cp_createDetachedTask,cp_clearAdaptersAndTimerOnStartOfEvaluation,cp_logEndOfEvaluation],
  {ct_profiling}       [cp_ask,cp_timing,cp_spawnWorker,cp_profile,         cp_createDetachedTask,cp_clearAdaptersAndTimerOnStartOfEvaluation,cp_logEndOfEvaluation],
  {$ifdef fullVersion}
  {ct_debugging}       [cp_ask,cp_timing,               cp_profile,cp_debug,cp_createDetachedTask,cp_clearAdaptersAndTimerOnStartOfEvaluation,cp_logEndOfEvaluation],
  {$endif}
  {ct_silentlyRunAlone}[                 cp_spawnWorker,                    cp_createDetachedTask,cp_clearAdaptersAndTimerOnStartOfEvaluation]);

TYPE
  T_profileCategory=(pc_importing,pc_tokenizing,pc_declaration,pc_interpretation,pc_unknown,pc_total);

  T_profilingEntry=record
    id:T_idString;
    location:string;
    timeSpent:double;
    callCount:longint;
  end;

  T_callStack=array of record
    callerLocation:T_tokenLocation;
    callee:P_objectWithIdAndLocation;
    callParameters:P_listLiteral;
    timeForProfiling:double;
  end;
  {$ifdef fullVersion}
  T_debuggingSnapshot=record
    location:T_tokenLocation;
    state:string;
    callStack:T_callStack;
  end;
  {$endif}

  T_profilingMap=specialize G_stringKeyMap<T_profilingEntry>;

  P_evaluationContext=^T_evaluationContext;

  { T_evaluationContext }

  T_evaluationContext=object
    private
      //privileges and obligations
      privileges:T_contextPrivilegeSet;
      //links to other objects
      parentContext:P_evaluationContext;
      asyncChildCount:longint;
      mainPackage:P_objectWithPath;
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
      profilingAndDebuggingCriticalSection:TRTLCriticalSection;
      profilingMap:T_profilingMap;
      {$ifdef fullVersion}
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
      {$endif}

      PROCEDURE addToProfilingMap(CONST id:T_idString; CONST location:ansistring; CONST dt:double);
    public
      CONSTRUCTOR createContext(CONST outAdapters:P_adapters; CONST contextType:T_contextType);
      PROCEDURE resetPrivileges(CONST contextType:T_contextType);
      DESTRUCTOR destroy;

      PROCEDURE resetForEvaluation(CONST package:P_objectWithPath);
      PROCEDURE afterEvaluation;

      //Basic property queries:
      PROPERTY adapters:P_adapters read currentAdapters;
      FUNCTION may(CONST privilege:T_contextPrivilege):boolean;
      //Delegation routines:
      PROCEDURE notifyAsyncTaskEnd;
      FUNCTION canGetNewAsyncContext (OUT childContext:T_evaluationContext):boolean;
      FUNCTION enterTryStatementReturningPreviousAdapters:P_adapters;
      PROCEDURE leaveTryStatementReassumingPreviousAdapters(CONST previousAdapters:P_adapters; CONST tryBodyFailed:boolean);
      PROCEDURE attachWorkerContext(CONST newParent:P_evaluationContext);
      PROCEDURE detachWorkerContext;
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
      FUNCTION getVariable(CONST id:T_idString):P_namedVariable;
      FUNCTION getVariableValue(CONST id:T_idString):P_literal; inline;
      //call stack routines
      PROCEDURE callStackPush(CONST callerLocation:T_tokenLocation; CONST callee:P_objectWithIdAndLocation; CONST callParameters:P_listLiteral);
      PROCEDURE callStackPop();
      {$ifdef FULLVERSION}
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

VAR tokenStackToStringCallback:FUNCTION(CONST stack:pointer; CONST first:P_token; CONST lengthLimit:longint):ansistring;
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

FUNCTION T_valueStore.getVariable(CONST id:T_idString):P_namedVariable;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    result:=nil;
    for i:=length(data)-1 downto 0 do with data[i] do
    if (v<>nil) and (v^.getId=id) then begin
      result:=v;
      system.leaveCriticalSection(cs);
      exit(result);
    end else if marker in [vsm_blockingFirst,vsm_blockingVoid] then begin
      system.leaveCriticalSection(cs);
      exit(nil);
    end;
    system.leaveCriticalSection(cs);
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

{$ifdef FULLVERSION}
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

PROCEDURE T_evaluationContext.addToProfilingMap(CONST id: T_idString;
  CONST location: ansistring; CONST dt: double);
  VAR profilingEntry:T_profilingEntry;
  begin
    enterCriticalSection(profilingAndDebuggingCriticalSection);
    if profilingMap.containsKey(location,profilingEntry)
    then with profilingEntry do begin
      timeSpent:=timeSpent+dt;
      inc(callCount);
    end else begin
      profilingEntry.id:=id;
      profilingEntry.location:=location;
      profilingEntry.timeSpent:=dt;
      profilingEntry.callCount:=1;
    end;
    profilingMap.put(location,profilingEntry);
    leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

CONSTRUCTOR T_evaluationContext.createContext(CONST outAdapters: P_adapters;
  CONST contextType: T_contextType);
  VAR i:longint;
  begin
    privileges :=C_initialPrivileges[contextType];
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
    initCriticalSection(profilingAndDebuggingCriticalSection);
    profilingMap.create();
  end;

PROCEDURE T_evaluationContext.resetPrivileges(CONST contextType: T_contextType);
  begin
    privileges:=C_initialPrivileges[contextType];
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
    if cp_disposeAdaptersOnDestruction in privileges then dispose(adapters,destroy);
    profilingMap.destroy;
    doneCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.resetForEvaluation(CONST package: P_objectWithPath);
  VAR pc:T_profileCategory;
  begin
    valueStore.clear;
    clearCallStack;
    profilingMap.clear;
    if cp_clearAdaptersAndTimerOnStartOfEvaluation in privileges then begin
      initialAdapters^.clearAll;
      currentAdapters:=initialAdapters;
      if (cp_profile in privileges) or initialAdapters^.doShowTimingInfo then begin
        wallClock.value.clear;
        wallClock.value.start;
        with timingInfo do begin
          for pc:=low(timeSpent) to high(timeSpent) do timeSpent[pc]:=0;
          startOfProfiling:=wallClock.value.elapsed;
        end;
      end;
      {$ifdef fullVersion}
      if (cp_debug in privileges) then begin
        debuggingStepper.state:=runUntilBreakpoint;
        debuggingStepper.lastBreakLevel:=-1;
        debuggingStepper.lastBreakLine.line:=-1;
        debuggingStepper.lastBreakLine.package:=package;
      end;
      {$endif}
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

  PROCEDURE logProfilingInfo;
    FUNCTION nicestTime(CONST seconds:double):string;
      begin
        if seconds>=10         then result:=formatFloat('0.000',seconds)+C_invisibleTabChar+'s'
        else if seconds>=10E-3 then result:=formatFloat('0.000',seconds*1E3)+C_invisibleTabChar+'ms'
        else                        result:=formatFloat('0.000',seconds*1E6)+C_invisibleTabChar+'µs';
      end;
    VAR profilingData:T_profilingMap.VALUE_TYPE_ARRAY;
        swapTemp:T_profilingEntry;
        lines:T_arrayOfString;
        i,j:longint;
    begin
      profilingData:=profilingMap.valueSet;
      for j:=0 to length(profilingData)-1 do for i:=0 to j-1 do if profilingData[i].timeSpent<profilingData[i].timeSpent then begin
        swapTemp:=profilingData[i];
        profilingData[i]:=profilingData[j];
        profilingData[j]:=swapTemp;
      end;
      setLength(lines,length(profilingData)+1);
      lines[0]:='Location'+C_tabChar+
                'id'+C_tabChar+
                'count'+C_tabChar+
                'time';

      for i:=0 to length(profilingData)-1 do with profilingData[i] do
      lines[i+1]:=location+C_tabChar+
                  id+C_tabChar+
                  intToStr(callCount)+C_tabChar+
                  nicestTime(timeSpent);
      lines:=formatTabs(lines);
      for i:=0 to length(lines)-1 do adapters^.raiseCustomMessage(mt_timing_info,lines[i],C_nilTokenLocation);
    end;

  begin
    valueStore.clear;
    clearCallStack;
    if cp_logEndOfEvaluation in privileges then begin
      initialAdapters^.stopEvaluation;
      while asyncChildCount>0 do begin
        ThreadSwitch;
        sleep(1);
      end;
      initialAdapters^.logEndOfEvaluation;
      if wantBasicTiming then logTimingInfo;
      if (cp_profile in privileges) and adapters^.doShowTimingInfo then logProfilingInfo;
    end;
    if cp_notifyParentOfAsyncTaskEnd in privileges then parentContext^.notifyAsyncTaskEnd;
  end;

FUNCTION T_evaluationContext.may(CONST privilege: T_contextPrivilege): boolean;
  begin result:=privilege in privileges; end;

PROCEDURE T_evaluationContext.notifyAsyncTaskEnd;
  begin
    interlockedDecrement(asyncChildCount);
  end;

FUNCTION T_evaluationContext.canGetNewAsyncContext(OUT
  childContext: T_evaluationContext): boolean;
  begin
    if not(cp_createDetachedTask in privileges) then exit(false);
    if parentContext=nil then begin
      InterLockedIncrement(asyncChildCount);
      childContext.createContext(initialAdapters,ct_normal);
      childContext.parentContext:=@self;
      childContext.privileges:=privileges-[cp_timing,cp_queryParentValueStore];
      childContext.privileges:=[cp_notifyParentOfAsyncTaskEnd];
      if cp_profile in childContext.privileges then childContext.privileges:=childContext.privileges+[cp_pushProfliningInfoToParent];
    end else begin
      result:=parentContext^.canGetNewAsyncContext(childContext);
    end;
  end;

FUNCTION T_evaluationContext.enterTryStatementReturningPreviousAdapters: P_adapters;
  begin
    result:=currentAdapters;
    currentAdapters:=result^.collectingClone;
  end;

PROCEDURE T_evaluationContext.leaveTryStatementReassumingPreviousAdapters(
  CONST previousAdapters: P_adapters; CONST tryBodyFailed: boolean);
  begin
    previousAdapters^.copyDataFromCollectingCloneDisposing(currentAdapters,tryBodyFailed);
    currentAdapters:=previousAdapters;
  end;

PROCEDURE T_evaluationContext.attachWorkerContext(
  CONST newParent: P_evaluationContext);
  begin
    initialAdapters:=newParent^.initialAdapters;
    currentAdapters:=newParent^.currentAdapters;
    parentContext:=newParent;
    privileges:=parentContext^.privileges-[cp_timing]+[cp_queryParentValueStore];
    if cp_profile in privileges then privileges:=privileges+[cp_pushProfliningInfoToParent];
  end;

PROCEDURE T_evaluationContext.detachWorkerContext;
  VAR profilingEntries:T_profilingMap.VALUE_TYPE_ARRAY;
      i:longint;
  begin
    if cp_pushProfliningInfoToParent in privileges then begin
      enterCriticalSection(profilingAndDebuggingCriticalSection);
      profilingEntries:=profilingMap.valueSet;
      for i:=0 to length(profilingEntries)-1 do with profilingEntries[i] do parentContext^.addToProfilingMap(id,location,timeSpent);
      leaveCriticalSection(profilingAndDebuggingCriticalSection);
    end;
    valueStore.clear;
    clearCallStack;
    profilingMap.clear;
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

FUNCTION T_evaluationContext.newToken(CONST tokenLocation: T_tokenLocation;
  CONST tokenText: ansistring; CONST tokenType: T_tokenType; CONST ptr: pointer
  ): P_token;
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

PROCEDURE T_evaluationContext.createVariable(CONST id: T_idString;
  CONST value: P_literal; CONST readonly: boolean);
  begin
    valueStore.createVariable(id,value,readonly);
  end;

FUNCTION T_evaluationContext.getVariable(CONST id: T_idString): P_namedVariable;
  VAR c:T_contextPrivilege;
  begin
    result:=valueStore.getVariable(id);
    if (result=nil) and (parentContext<>nil) and (cp_queryParentValueStore in privileges) then result:=parentContext^.getVariable(id);
    if result=nil then begin
      writeln(' - getVariable failed; context privileges are:');
      for c:=low(T_contextPrivilege) to high(T_contextPrivilege)  do begin
        if c in privileges then writeln('   - ',c);
      end;
    end;
  end;

FUNCTION T_evaluationContext.getVariableValue(CONST id: T_idString): P_literal;
  VAR named:P_namedVariable;
  begin
    named:=getVariable(id);
    if named=nil then result:=nil
                 else begin
      result:=named^.getValue;
      if result=nil then writeln('Something strange happened - encountered a named variable without literal');
    end;
  end;

PROCEDURE T_evaluationContext.callStackPush(CONST callerLocation: T_tokenLocation; CONST callee: P_objectWithIdAndLocation; CONST callParameters: P_listLiteral);
  VAR topIdx:longint;
  begin
    topIdx:=length(callStack);
    setLength(callStack,topIdx+1);
    callStack[topIdx].callerLocation:=callerLocation;
    callStack[topIdx].callee        :=callee;
    callStack[topIdx].callParameters:=callParameters;
    if callParameters<>nil then callParameters^.rereference;
    if cp_profile in privileges then begin
      if topIdx>0 then with callStack[topIdx-1] do timeForProfiling:=wallclockTime-timeForProfiling;
      callStack[topIdx].timeForProfiling:=wallclockTime;
    end;
  end;

PROCEDURE T_evaluationContext.callStackPop;
  VAR topIdx:longint;
  begin
    topIdx:=length(callStack)-1;
    if topIdx<0 then exit;
    if cp_profile in privileges then begin
      with callStack[topIdx] do begin
        timeForProfiling:=wallclockTime-timeForProfiling;
        addToProfilingMap(callee^.getId,callee^.getLocation,timeForProfiling);
      end;
      if topIdx>0 then with callStack[topIdx-1] do timeForProfiling:=wallclockTime-timeForProfiling;
    end;
    with callStack[topIdx] do if callParameters<>nil then disposeLiteral(callParameters);
    setLength(callStack,topIdx);
  end;
{$ifdef FULLVERSION}
PROCEDURE T_evaluationContext.reportVariables(
  VAR variableReport: T_variableReport);
  begin
    if parentContext<>nil then parentContext^.reportVariables(variableReport);
    valueStore.reportVariables(variableReport);
  end;
{$endif}

PROCEDURE T_evaluationContext.printCallStack(CONST messageType: T_messageType);
  VAR i:longint;
  begin
    for i:=length(callStack)-1 downto 0 do with callStack[i] do
    if callParameters=nil
    then adapters^.raiseCustomMessage(messageType,callee^.getId+' ()'                                             ,callerLocation)
    else adapters^.raiseCustomMessage(messageType,callee^.getId+' '+callParameters^.toParameterListString(true,50),callerLocation);
    if parentContext<>nil then parentContext^.printCallStack(messageType);
  end;

PROCEDURE T_evaluationContext.clearCallStack;
  VAR i:longint;
  begin
    //If the evaluation was not finished cleanly, the call stack may not be empty
    //Make sure that the current top-element is profiled correctly
    i:=length(callStack)-1;
    if i<0 then exit;
    if cp_profile in privileges then begin
      with callStack[i] do begin
        timeForProfiling:=wallclockTime-timeForProfiling;
        addToProfilingMap(callee^.getId,callee^.getLocation,timeForProfiling);
      end;
    end;
    for i:=0 to length(callStack)-1 do with callStack[i] do if callParameters<>nil then disposeLiteral(callParameters);
    setLength(callStack,0);
  end;

FUNCTION T_evaluationContext.wallclockTime: double;
  begin
    if parentContext<>nil then exit(parentContext^.wallclockTime);
    result:=wallClock.value.elapsed;
  end;

FUNCTION T_evaluationContext.wantBasicTiming: boolean;
  begin
    result:=(cp_timing in privileges) and (adapters^.doShowTimingInfo);
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
      debuggingStepper.snapshot.state:=tokenStackToStringCallback(stack,first,100);
      setLength(debuggingStepper.snapshot.callStack,length(callStack));
      for i:=0 to length(callStack)-1 do debuggingStepper.snapshot.callStack[i]:=callStack[i];
    end;

  VAR lineChanged:boolean;
  begin
    if not(cp_debug in privileges) or (debuggingStepper.state=dontBreakAtAll) then exit;
    enterCriticalSection(profilingAndDebuggingCriticalSection);
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
    leaveCriticalSection(profilingAndDebuggingCriticalSection);
  end;

PROCEDURE T_evaluationContext.haltEvaluation;
  begin
    adapters^.haltEvaluation;
    if cp_debug in privileges then begin
      enterCriticalSection(profilingAndDebuggingCriticalSection);
      debuggingStepper.state:=dontBreakAtAll;
      leaveCriticalSection(profilingAndDebuggingCriticalSection);
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
