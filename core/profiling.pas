UNIT profiling;
INTERFACE
USES sysutils,
     //my libraries
     {$ifdef fullVersion}
     myGenerics,
     mnh_messages,out_adapters,
     {$endif}
     //MNH:
     basicTypes,
     messageFormatting;

TYPE
  T_profileCategory=(pc_importing,pc_tokenizing,pc_declaration,pc_interpretation,pc_unknown,pc_total);

  P_packageProfilingCall=^T_packageProfilingCall;
  T_packageProfilingCalls=array[T_profileCategory] of P_packageProfilingCall;
  T_packageProfilingCall=object(T_objectWithIdAndLocation)
    package: P_objectWithPath;
    category:T_profileCategory;
    CONSTRUCTOR create(CONST package_: P_objectWithPath;
                       CONST category_:T_profileCategory);
    DESTRUCTOR destroy;
    FUNCTION getId:T_idString; virtual;
    FUNCTION getLocation:T_tokenLocation; virtual;
  end;

  {$ifdef fullVersion}
  T_callerMap=specialize G_stringKeyMap<T_profilingInfo>;
  T_stringMap=specialize G_stringKeyMap<string>;

  P_calleeEntry=^T_calleeEntry;
  T_calleeEntry=object
    id:T_idString;
    calleeLocation:T_tokenLocation;
    callerMap:T_callerMap;

    CONSTRUCTOR create(CONST id_:T_idString;CONST loc:T_tokenLocation);
    DESTRUCTOR destroy;
    PROCEDURE add(CONST callerLocation: ansistring; CONST dt_inclusive,dt_exclusive:double);
    FUNCTION toProfilingListEntry(CONST locationToIdMap:T_stringMap):T_profilingListEntry;
    FUNCTION getCaleeList(CONST locationToIdMap:T_stringMap):T_callerList;
  end;

  T_profilingMap=specialize G_stringKeyMap<P_calleeEntry>;

  P_profiler=^T_profiler;
  T_profiler=object
    private
      cs:TRTLCriticalSection;
      callee2callers:T_profilingMap;
      caller2callees:T_profilingMap;
      line2funcLoc  :T_stringMap;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE add(CONST id: T_idString; CONST callerFuncLocation,callerLocation,calleeLocation: T_tokenLocation; CONST dt_inclusive,dt_exclusive:double);
      PROCEDURE logInfo(CONST adapters:P_messages);
  end;
  {$endif}

FUNCTION blankProfilingCalls:T_packageProfilingCalls;
IMPLEMENTATION
CONST categoryText:array[T_profileCategory] of string=(':importing',':tokenizing',':declarations',':evaluation',':unknown',':total');
FUNCTION blankProfilingCalls:T_packageProfilingCalls;
  VAR p:T_profileCategory;
  begin
    for p in T_profileCategory do result[p]:=nil;
  end;
{$ifdef fullVersion}
FUNCTION fixLocation(CONST location:T_searchTokenLocation):T_searchTokenLocation;
  begin
    result:=location;
    if (location.line<0) and (location.column=0) then begin
      result.line:=1;
      result.column:=1;
    end;
  end;

FUNCTION fixLocation(CONST s:string):T_searchTokenLocation;
  begin
    result:=fixLocation(guessLocationFromString(s,false));
  end;

PROCEDURE disposeEntry(VAR entry:P_calleeEntry);
  begin
    dispose(entry,destroy);
  end;

CONSTRUCTOR T_calleeEntry.create(CONST id_: T_idString; CONST loc: T_tokenLocation);
  begin
    id:=id_;
    calleeLocation:=loc;
    callerMap.create();
  end;

DESTRUCTOR T_calleeEntry.destroy;
  begin
    callerMap.destroy;
  end;

PROCEDURE T_calleeEntry.add(CONST callerLocation: ansistring;
  CONST dt_inclusive, dt_exclusive: double);
  VAR callerEntry:T_profilingInfo;
  begin
    if callerMap.containsKey(callerLocation,callerEntry) then begin
      inc(callerEntry.callCount);
      callerEntry.timeSpent_inclusive+=dt_inclusive;
      callerEntry.timeSpent_exclusive+=dt_exclusive;
    end else begin
      callerEntry.callCount          :=1;
      callerEntry.timeSpent_inclusive:=dt_inclusive;
      callerEntry.timeSpent_exclusive:=dt_exclusive;
    end;
    callerMap.put(callerLocation,callerEntry);
  end;

OPERATOR +(CONST x,y:T_profilingInfo):T_profilingInfo;
  begin
    result.callCount          :=x.callCount          +y.callCount;
    result.timeSpent_inclusive:=x.timeSpent_inclusive+y.timeSpent_inclusive;
    result.timeSpent_exclusive:=x.timeSpent_exclusive+y.timeSpent_exclusive;
  end;

FUNCTION T_calleeEntry.toProfilingListEntry(CONST locationToIdMap:T_stringMap): T_profilingListEntry;
  VAR entries:T_callerMap.KEY_VALUE_LIST;
      k:longint;
  begin
    result.aggTime.callCount:=0;
    result.aggTime.timeSpent_exclusive:=0;
    result.aggTime.timeSpent_inclusive:=0;
    result.id:=id;
    result.calleeLocation:=fixLocation(T_searchTokenLocation(calleeLocation));
    entries:=callerMap.entrySet;
    setLength(result.callers,length(entries));
    for k:=0 to length(entries)-1 do begin
      result.callers[k].location:=fixLocation(entries[k].key);
      result.callers[k].time    :=entries[k].value;
      result.aggTime+=            entries[k].value;
      if not(locationToIdMap.containsKey(entries[k].key,result.callers[k].id))
      then result.callers[k].id:='?';
    end;
    setLength(result.callees,0);
  end;

FUNCTION T_calleeEntry.getCaleeList(CONST locationToIdMap:T_stringMap):T_callerList;
  VAR entries:T_callerMap.KEY_VALUE_LIST;
      k:longint;
  begin
    entries:=callerMap.entrySet;
    setLength(result,length(entries));
    for k:=0 to length(entries)-1 do begin
      result[k].location:=fixLocation(entries[k].key);
      result[k].time    :=entries[k].value;
      if not(locationToIdMap.containsKey(entries[k].key,result[k].id))
      then result[k].id:='?';
    end;
  end;

CONSTRUCTOR T_profiler.create;
  begin
    callee2callers.create(@disposeEntry);
    caller2callees.create(@disposeEntry);
    line2funcLoc.create();
    initCriticalSection(cs);
  end;

DESTRUCTOR T_profiler.destroy;
  begin
    enterCriticalSection(cs);
    try
      callee2callers.destroy;
      caller2callees.destroy;
      line2funcLoc.destroy;
    finally
      leaveCriticalSection(cs);
    end;
    doneCriticalSection(cs);
  end;

PROCEDURE T_profiler.clear;
  begin
    enterCriticalSection(cs);
    try
      callee2callers.clear;
      caller2callees.clear;
      line2funcLoc.clear;
    finally
      leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_profiler.add(CONST id: T_idString; CONST callerFuncLocation,callerLocation,calleeLocation: T_tokenLocation; CONST dt_inclusive, dt_exclusive: double);
  VAR profilingEntry:P_calleeEntry;
  begin
    enterCriticalSection(cs);
    try
      if not callee2callers.containsKey(calleeLocation,profilingEntry) then begin
        new(profilingEntry,create(id,calleeLocation));
        callee2callers.put(calleeLocation,profilingEntry);
      end;
      profilingEntry^.add(callerLocation,dt_inclusive,dt_exclusive);

      if not caller2callees.containsKey(callerFuncLocation,profilingEntry) then begin
        new(profilingEntry,create('?',callerFuncLocation));
        caller2callees.put(callerFuncLocation,profilingEntry);
      end;
      profilingEntry^.add(calleeLocation,dt_inclusive,dt_exclusive);

      line2funcLoc.put(callerLocation,callerFuncLocation);
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_profiler.logInfo(CONST adapters:P_messages);
  VAR callee2callersList:T_profilingMap.VALUE_TYPE_ARRAY;
      profEntry:T_profilingMap.KEY_VALUE_PAIR;
      lineEntry:T_stringMap.KEY_VALUE_PAIR;

      callerInfo:P_calleeEntry;
      locationToIdMap:T_stringMap;
      message:P_profileMessage;
      k:longint;
  begin
    enterCriticalSection(cs);
    try
      callee2callersList:=callee2callers.valueSet;
      locationToIdMap.create();
      for profEntry in callee2callers.entrySet do locationToIdMap.put(profEntry.key,profEntry.value^.id);
      for lineEntry in line2funcLoc.entrySet do if callee2callers.containsKey(lineEntry.value,callerInfo) then
        locationToIdMap.put(lineEntry.key,callerInfo^.id);

      new(message,create);
      setLength(message^.content,length(callee2callersList));
      for k:=0 to length(callee2callersList)-1 do begin
        message^.content[k]:=callee2callersList[k]^.toProfilingListEntry(locationToIdMap);
        if caller2callees.containsKey(message^.content[k].calleeLocation,callerInfo) then
          message^.content[k].callees:=callerInfo^.getCaleeList(locationToIdMap);
      end;
      adapters^.postCustomMessage(message,true);
      locationToIdMap.destroy;
    finally
      leaveCriticalSection(cs);
    end;
  end;
{$endif}

CONSTRUCTOR T_packageProfilingCall.create(CONST package_: P_objectWithPath; CONST category_: T_profileCategory);
  begin
    package:=package_;
    category:=category_;
  end;

DESTRUCTOR T_packageProfilingCall.destroy;
  begin end;

FUNCTION T_packageProfilingCall.getId: T_idString;
  begin
    result:=categoryText[category];
  end;

FUNCTION T_packageProfilingCall.getLocation: T_tokenLocation;
  begin
    result:=packageTokenLocation(package);
    result.column:=0;
    result.line:=-1-ord(category);
  end;

end.
