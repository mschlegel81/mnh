UNIT profiling;
INTERFACE
USES sysutils,
     //my libraries
     {$ifdef fullVersion}
     myGenerics,myStringUtil,
     mnh_messages,out_adapters,
     {$endif}
     //MNH:
     basicTypes;

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
  T_callerEntry=record
    timeSpent_inclusive,
    timeSpent_exclusive:double;
    callCount:longint;
  end;
  T_callerMap=specialize G_stringKeyMap<T_callerEntry>;

  P_calleeEntry=^T_calleeEntry;
  T_calleeEntry=object
    id:T_idString;
    calleeLocation:string;
    callerMap:T_callerMap;

    //aggregated values
    timeSpent_inclusive,
    timeSpent_exclusive:double;
    callCount:longint;

    CONSTRUCTOR create(CONST id_:T_idString;CONST loc:string);
    DESTRUCTOR destroy;
    PROCEDURE aggregateValues;
    FUNCTION toString(CONST withHeader:boolean):T_arrayOfString;
    PROCEDURE add(CONST callerLocation: ansistring; CONST dt_inclusive,dt_exclusive:double);
  end;

  T_profilingMap=specialize G_stringKeyMap<P_calleeEntry>;

  P_profiler=^T_profiler;
  T_profiler=object
    private
      cs:TRTLCriticalSection;
      map:T_profilingMap;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE add(CONST id: T_idString; CONST callerLocation,calleeLocation: ansistring; CONST dt_inclusive,dt_exclusive:double);
      PROCEDURE logInfo(CONST adapters:P_messages);
  end;
  {$endif}

FUNCTION blankProfilingCalls:T_packageProfilingCalls;
VAR mnhSysPseudopackagePrefix :string='';
IMPLEMENTATION
FUNCTION blankProfilingCalls:T_packageProfilingCalls;
  VAR p:T_profileCategory;
  begin
    for p in T_profileCategory do result[p]:=nil;
  end;
{$ifdef fullVersion}
PROCEDURE disposeEntry(VAR entry:P_calleeEntry);
  begin
    dispose(entry,destroy);
  end;

CONSTRUCTOR T_calleeEntry.create(CONST id_: T_idString; CONST loc: string);
  begin
    id:=id_;
    calleeLocation:=loc;
    callerMap.create();
  end;

DESTRUCTOR T_calleeEntry.destroy;
  begin
    callerMap.destroy;
  end;

PROCEDURE T_calleeEntry.aggregateValues;
  VAR callerEntry:T_callerEntry;
  begin
    timeSpent_inclusive:=0;
    timeSpent_exclusive:=0;
    callCount          :=0;
    for callerEntry in callerMap.valueSet do begin
      timeSpent_inclusive+=callerEntry.timeSpent_inclusive;
      timeSpent_exclusive+=callerEntry.timeSpent_exclusive;
      callCount          +=callerEntry.callCount          ;
    end;
  end;

FUNCTION T_calleeEntry.toString(CONST withHeader: boolean): T_arrayOfString;
  FUNCTION nicestTime(CONST seconds:double):string;
    begin
       result:=formatFloat('0.000',seconds*1E3);
    end;

  FUNCTION profiledLocation(CONST location:string):string;
    begin
      if startsWith(location,mnhSysPseudopackagePrefix)
      then result:='(builtin)'
      else result:=location;
    end;

  VAR callers:T_callerMap.KEY_VALUE_LIST;
      temp   :T_callerMap.KEY_VALUE_PAIR;
      firstCaller:boolean=true;
      i,j:longint;
      shortId:string;
  begin
    callers:=callerMap.entrySet;
    for j:=0 to length(callers)-1 do for i:=0 to j-1 do
    if callers[i].value.timeSpent_inclusive<callers[j].value.timeSpent_inclusive then begin
      temp:=callers[i];
      callers[i]:=callers[j];
      callers[j]:=temp;
    end;
    j:=length(callers)+1;
    if withHeader then inc(j);
    setLength(result,j);
    j:=0;
    if length(id)>50 then shortId:=copy(id,1,47)+'...' else shortId:=id;
    if withHeader then begin
      result[0]:='id'       +C_tabChar+
                 'location' +C_tabChar+
                 'count'    +C_tabChar+
                 'inclusive ms'+C_tabChar+
                 'exclusive ms';
      inc(j);
    end;
    result[j]:=shortId                         +C_tabChar+
               profiledLocation(calleeLocation)+C_tabChar+
               intToStr  (callCount)           +C_tabChar+
               nicestTime(timeSpent_inclusive) +C_tabChar+
               nicestTime(timeSpent_exclusive);
    inc(j);
    for temp in callers do begin
      result[j]:=BoolToStr(firstCaller,C_shiftInChar+'called at',' ')    +C_tabChar+
                 profiledLocation(temp.key)                +C_tabChar+
                 intToStr  (temp.value.callCount)          +C_tabChar+
                 nicestTime(temp.value.timeSpent_inclusive)+C_tabChar+
                 nicestTime(temp.value.timeSpent_exclusive);
      firstCaller:=false;
      inc(j);
    end;
  end;

PROCEDURE T_calleeEntry.add(CONST callerLocation: ansistring; CONST dt_inclusive, dt_exclusive: double);
  VAR callerEntry:T_callerEntry;
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

CONSTRUCTOR T_profiler.create;
  begin
    map.create(@disposeEntry);
    initCriticalSection(cs);
  end;

DESTRUCTOR T_profiler.destroy;
  begin
    enterCriticalSection(cs);
    try
      map.destroy;
    finally
      leaveCriticalSection(cs);
    end;
    doneCriticalSection(cs);
  end;

PROCEDURE T_profiler.clear;
  begin
    enterCriticalSection(cs);
    try
      map.clear;
    finally
      leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_profiler.add(CONST id: T_idString; CONST callerLocation,calleeLocation: ansistring; CONST dt_inclusive, dt_exclusive: double);
  VAR profilingEntry:P_calleeEntry;
  begin
    enterCriticalSection(cs);
    try
      if not map.containsKey(calleeLocation,profilingEntry) then begin
        new(profilingEntry,create(id,calleeLocation));
        map.put             (calleeLocation,profilingEntry);
      end;
      profilingEntry^.add(callerLocation,dt_inclusive,dt_exclusive);
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_profiler.logInfo(CONST adapters:P_messages);
  VAR profilingData:T_profilingMap.VALUE_TYPE_ARRAY;
      temp:P_calleeEntry;
      lines:T_arrayOfString;
      i,j:longint;
      firstEntry:boolean=true;
      txt:string;
  begin
    enterCriticalSection(cs);
    try
      profilingData:=map.valueSet;
      for temp in profilingData do temp^.aggregateValues;
      for j:=0 to length(profilingData)-1 do for i:=0 to j-1 do
      if profilingData[i]^.timeSpent_inclusive<profilingData[j]^.timeSpent_inclusive then begin
        temp:=profilingData[i];
        profilingData[i]:=profilingData[j];
        profilingData[j]:=temp;
      end;
      lines:=C_EMPTY_STRING_ARRAY;
      for temp in profilingData do begin
        append(lines,temp^.toString(firstEntry));
        firstEntry:=false;
      end;
      lines:=formatTabs(lines);
      adapters^.postTextMessage(mt_printline,C_nilTokenLocation,'Profiling output:');
      for txt in lines do begin
        if (length(txt)=0) or (txt[1]=' ') or (copy(txt,1,9)='called at')
        then adapters^.postTextMessage(mt_profile_call_info2,C_nilTokenLocation,txt)
        else adapters^.postTextMessage(mt_profile_call_info1,C_nilTokenLocation,txt);
      end;
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
  CONST categoryText:array[T_profileCategory] of string=('importing','tokenizing','declarations','evaluation','unknown','total');
  begin
    result:=':'+categoryText[category];
  end;

FUNCTION T_packageProfilingCall.getLocation: T_tokenLocation;
  begin
    result:=packageTokenLocation(package);
    result.column:=0;
    result.line:=-1-ord(category);
  end;

end.
