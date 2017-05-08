UNIT mnh_profiling;
INTERFACE
USES sysutils,
     //my libraries
     myGenerics,myStringUtil,
     //MNH:
     mnh_basicTypes,
     mnh_out_adapters,
     mnh_litVar;

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

  T_profilingEntry=record
    id:T_idString;
    location:string;
    timeSpent_inclusive,
    timeSpent_exclusive:double;
    callCount:longint;
  end;
  T_profilingMap=specialize G_stringKeyMap<T_profilingEntry>;

  P_profiler=^T_profiler;
  T_profiler=object
    private
      cs:TRTLCriticalSection;
      map:T_profilingMap;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE add(CONST id: T_idString; CONST location: ansistring; CONST dt_inclusive,dt_exclusive:double);
      PROCEDURE logInfo(CONST adapters:P_adapters);
  end;

FUNCTION blankProfilingCalls:T_packageProfilingCalls;
VAR showProfilingTableCallback:PROCEDURE(CONST L:P_listLiteral)=nil;
IMPLEMENTATION
FUNCTION blankProfilingCalls:T_packageProfilingCalls;
  VAR p:T_profileCategory;
  begin
    for p in T_profileCategory do result[p]:=nil;
  end;

CONSTRUCTOR T_profiler.create;
  begin
    map.create();
    initCriticalSection(cs);
  end;

DESTRUCTOR T_profiler.destroy;
  begin
    enterCriticalSection(cs);
    map.destroy;
    leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

PROCEDURE T_profiler.clear;
  begin
    enterCriticalSection(cs);
    map.clear;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_profiler.add(CONST id: T_idString; CONST location: ansistring; CONST dt_inclusive, dt_exclusive: double);
  VAR profilingEntry:T_profilingEntry;
  begin
    enterCriticalSection(cs);
    if map.containsKey(location,profilingEntry)
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
    map.put(location,profilingEntry);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_profiler.logInfo(CONST adapters:P_adapters);
  FUNCTION nicestTime(CONST seconds:double):string;
    begin
       result:=formatFloat('0.000',seconds*1E3)+C_invisibleTabChar+'ms';
    end;

  VAR profilingData:T_profilingMap.VALUE_TYPE_ARRAY;
      {$ifdef fullVersion} data:P_listLiteral;{$endif}
      swapTemp:T_profilingEntry;
      lines:T_arrayOfString;
      i,j:longint;

  begin
    enterCriticalSection(cs);
    profilingData:=map.valueSet;
    for j:=0 to length(profilingData)-1 do for i:=0 to j-1 do if profilingData[i].timeSpent_inclusive<profilingData[j].timeSpent_inclusive then begin
      swapTemp:=profilingData[i];
      profilingData[i]:=profilingData[j];
      profilingData[j]:=swapTemp;
    end;
    setLength(lines,length(profilingData)+1);
    lines[0]:='Location'+C_tabChar+
              'id'+C_tabChar+
              'count'+C_tabChar+
              'inclusive'+C_invisibleTabChar+' time'+C_tabChar+
              'exclusive'+C_invisibleTabChar+' time';

    for i:=0 to length(profilingData)-1 do with profilingData[i] do
      lines[i+1]:=location+C_tabChar+
                  id+C_tabChar+
                  intToStr(callCount)+C_tabChar+
                  nicestTime(timeSpent_inclusive)+C_tabChar+
                  nicestTime(timeSpent_exclusive);
    lines:=formatTabs(lines);
    {$ifdef fullVersion}
    if Assigned(showProfilingTableCallback) then begin
      data:=newListLiteral(length(profilingData)+1);
      data^.append(newListLiteral(5)^
           .appendString('Location')^
           .appendString('id')^
           .appendString('count')^
           .appendString('inclusive (ms)')^
           .appendString('exclusive (ms)'),false);
      for i:=0 to length(profilingData)-1 do with profilingData[i] do
        data^.append(newListLiteral(5)^
                    .appendString(location)^
                    .appendString(id)^
                    .appendInt(callCount)^
                    .appendReal(timeSpent_inclusive*1E3)^
                    .appendReal(timeSpent_exclusive*1E3),false);
      showProfilingTableCallback(data);
      disposeLiteral(data);
      adapters^.logDisplayTable;
    end;
    {$endif}

    for i:=0 to length(lines)-1 do adapters^.logTimingInfo(lines[i]);
    leaveCriticalSection(cs);
  end;

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
