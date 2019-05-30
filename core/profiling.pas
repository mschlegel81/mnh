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

  T_profilingListEntry=record
    id:T_idString;
    calleeLocation:string;
    callers:T_callerMap.KEY_VALUE_LIST;

    //aggregated values
    timeSpent_inclusive,
    timeSpent_exclusive:double;
    callCount:longint;
  end;

  P_calleeEntry=^T_calleeEntry;
  T_calleeEntry=object
    id:T_idString;
    calleeLocation:string;
    callerMap:T_callerMap;

    CONSTRUCTOR create(CONST id_:T_idString;CONST loc:string);
    DESTRUCTOR destroy;
    PROCEDURE add(CONST callerLocation: ansistring; CONST dt_inclusive,dt_exclusive:double);
    FUNCTION toProfilingListEntry:T_profilingListEntry;
  end;

  T_profilingMap=specialize G_stringKeyMap<P_calleeEntry>;
  T_profilingList=array of T_profilingListEntry;

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

  P_profileMessage=^T_profileMessage;
  T_profileMessage=object(T_payloadMessage)
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      content:T_profilingList;
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      FUNCTION toString({$ifdef fullVersion}CONST forGui:boolean{$endif}):T_arrayOfString; virtual;
  end;

PROCEDURE sortProfilingList(VAR list:T_profilingList; CONST sortIndex:byte);
PROCEDURE sortCallerList(VAR list:T_callerMap.KEY_VALUE_LIST; CONST sortIndex:byte);
  {$endif}

FUNCTION blankProfilingCalls:T_packageProfilingCalls;
VAR mnhSysPseudopackagePrefix :string='';
IMPLEMENTATION
CONST categoryText:array[T_profileCategory] of string=(':importing',':tokenizing',':declarations',':evaluation',':unknown',':total');
FUNCTION blankProfilingCalls:T_packageProfilingCalls;
  VAR p:T_profileCategory;
  begin
    for p in T_profileCategory do result[p]:=nil;
  end;
{$ifdef fullVersion}
PROCEDURE sortProfilingList(VAR list:T_profilingList; CONST sortIndex:byte);
  FUNCTION lesser(CONST a,b:T_profilingListEntry):boolean;
    begin
      result:=false;
      case sortIndex of
        0: result:=a.id<b.id;
        1: result:=a.id>b.id;
        2: result:=a.calleeLocation<b.calleeLocation;
        3: result:=a.calleeLocation>b.calleeLocation;
        4: result:=a.callCount<b.callCount;
        5: result:=a.callCount>b.callCount;
        6: result:=a.timeSpent_inclusive<b.timeSpent_inclusive;
        7: result:=a.timeSpent_inclusive>b.timeSpent_inclusive;
        8: result:=a.timeSpent_exclusive<b.timeSpent_exclusive;
        9: result:=a.timeSpent_exclusive>b.timeSpent_exclusive;
      end;
    end;

  VAR i,j:longint;
      tmp:T_profilingListEntry;
  begin
    for i:=1 to length(list)-1 do
    for j:=0 to i-1 do if lesser(list[i],list[j]) then begin
      tmp:=list[i]; list[i]:=list[j]; list[j]:=tmp;
    end;
  end;

PROCEDURE sortCallerList(VAR list:T_callerMap.KEY_VALUE_LIST; CONST sortIndex:byte);
  FUNCTION lesser(CONST a,b:T_callerMap.KEY_VALUE_PAIR):boolean;
    begin
      result:=false;
      case sortIndex of
        0: result:=a.key<b.key;
        1: result:=a.key>b.key;
        2: result:=a.value.callCount<b.value.callCount;
        3: result:=a.value.callCount>b.value.callCount;
        4: result:=a.value.timeSpent_inclusive<b.value.timeSpent_inclusive;
        5: result:=a.value.timeSpent_inclusive>b.value.timeSpent_inclusive;
        6: result:=a.value.timeSpent_exclusive<b.value.timeSpent_exclusive;
        7: result:=a.value.timeSpent_exclusive>b.value.timeSpent_exclusive;
      end;
    end;

  VAR i,j:longint;
      tmp:T_callerMap.KEY_VALUE_PAIR;
  begin
    for i:=1 to length(list)-1 do
    for j:=0 to i-1 do if lesser(list[i],list[j]) then begin
      tmp:=list[i]; list[i]:=list[j]; list[j]:=tmp;
    end;
  end;

FUNCTION T_profileMessage.internalType: shortstring;
begin result:='T_profileMessage'; end;

CONSTRUCTOR T_profileMessage.create;
  begin
    inherited create(mt_profile_call_info);
  end;

DESTRUCTOR T_profileMessage.destroy;
  begin
    setLength(content,0);
  end;

FUNCTION T_profileMessage.toString(CONST forGui: boolean): T_arrayOfString;
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

  VAR j,k:longint;
      shortId:string;
  begin
    for k:=0 to length(content)-1 do sortCallerList(content[k].callers,4);
    sortProfilingList(content,6);
    result:='id'          +C_tabChar+
            'location'    +C_tabChar+
            'count'       +C_tabChar+
            'inclusive ms'+C_tabChar+
            'exclusive ms';
    for k:=0 to length(content)-1 do with content[k] do begin
      if length(id)>50 then shortId:=copy(id,1,47)+'...' else shortId:=id;
      append(result,shortId                         +C_tabChar+
                    profiledLocation(calleeLocation)+C_tabChar+
                    intToStr  (callCount)           +C_tabChar+
                    nicestTime(timeSpent_inclusive) +C_tabChar+
                    nicestTime(timeSpent_exclusive));
      for j:=0 to length(callers)-1 do begin
        append(result,BoolToStr(j=0,C_shiftInChar+'called at',' ')+C_tabChar+
                  profiledLocation(callers[j].key)                +C_tabChar+
                  intToStr  (callers[j].value.callCount)          +C_tabChar+
                  nicestTime(callers[j].value.timeSpent_inclusive)+C_tabChar+
                  nicestTime(callers[j].value.timeSpent_exclusive));
      end;
    end;
    formatTabs(result);
  end;

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

PROCEDURE T_calleeEntry.add(CONST callerLocation: ansistring;
  CONST dt_inclusive, dt_exclusive: double);
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

FUNCTION T_calleeEntry.toProfilingListEntry: T_profilingListEntry;
  VAR caller:T_callerMap.KEY_VALUE_PAIR;
      anyCat:boolean=false;
      s:string;
  begin
    result.callCount:=0;
    result.timeSpent_exclusive:=0;
    result.timeSpent_inclusive:=0;
    result.id:=id;
    result.calleeLocation:=calleeLocation;
    result.callers:=callerMap.entrySet;
    for caller in result.callers do begin
      result.callCount          +=caller.value.callCount;
      result.timeSpent_exclusive+=caller.value.timeSpent_exclusive;
      result.timeSpent_inclusive+=caller.value.timeSpent_inclusive;
    end;
    for s in categoryText do if s=id then anyCat:=true;
    if anyCat then begin
      result.calleeLocation:='';
      setLength(result.callers,0);
    end;
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
      message:P_profileMessage;
      k:longint;
  begin
    enterCriticalSection(cs);
    try
       profilingData:=map.valueSet;
       new(message,create);
       setLength(message^.content,length(profilingData));
       for k:=0 to length(profilingData)-1 do message^.content[k]:=profilingData[k]^.toProfilingListEntry;
       adapters^.postCustomMessage(message,true);
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
