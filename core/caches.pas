UNIT caches;
INTERFACE
USES sysutils,
     myGenerics,mySys,
     basicTypes,
     litVar;
CONST MAX_ACCEPTED_COLLISIONS=10;
      MIN_BIN_COUNT=4;

TYPE
  T_cacheEntry = record
    key: P_listLiteral;
    keyHash: T_hashInt;
    value: P_literal;
    lastUse: longint;
    goodUntil: double;
  end;

  P_cache = ^T_cache;
  T_cache = object
  private
    criticalSection:TRTLCriticalSection;
    fill: longint;
    useCounter:longint;
    cached: array of record
      data:array of T_cacheEntry;
    end;
    PROCEDURE polish;
  public
    timeout:double;
    CONSTRUCTOR create(ruleCS:TRTLCriticalSection);
    DESTRUCTOR destroy;
    PROCEDURE put(CONST key: P_listLiteral; CONST value: P_literal; CONST recycler:P_literalRecycler); inline;
    FUNCTION get(CONST key: P_listLiteral): P_literal;
    FUNCTION getIfNotLocked(CONST key: P_listLiteral): P_literal;
    PROCEDURE clear;
    FUNCTION getMap:P_mapLiteral;
  end;

IMPLEMENTATION
USES recyclers;
CONSTRUCTOR T_cache.create(ruleCS:TRTLCriticalSection);
  begin
    criticalSection:=ruleCS;
    timeout:=1E20;
    fill := 0;
    useCounter:=0;
    setLength(cached,MIN_BIN_COUNT);
    memoryCleaner.registerObjectForCleanup(2,@polish);
  end;

DESTRUCTOR T_cache.destroy;
  begin
    memoryCleaner.unregisterObjectForCleanup(@polish);
    clear;
  end;

PROCEDURE T_cache.polish;
  VAR binIdx,i,j: longint;
      dropThreshold:longint;
      now_: TDateTime;
  begin
    now_:=now;
    enterCriticalSection(criticalSection);
    try
      dropThreshold:=useCounter - fill shr 1;
      for binIdx:=0 to length(cached)-1 do
      with cached[binIdx] do begin
        j:=0;
        for i := 0 to length(data)-1 do
        if (data[i].lastUse>dropThreshold) and (data[i].goodUntil>now_) then begin
          data[j] := data[i];
          inc(j);
        end else begin
          globalLiteralRecycler.disposeLiteral(data[i].key);
          globalLiteralRecycler.disposeLiteral(data[i].value);
          dec(fill);
        end;
        setLength(data, j);
      end;
    finally
      leaveCriticalSection(criticalSection);
    end;
  end;

PROCEDURE T_cache.put(CONST key: P_listLiteral; CONST value: P_literal; CONST recycler:P_literalRecycler);
  PROCEDURE grow;
    VAR redistribute:array of T_cacheEntry=();
        i,j,k:longint;
    begin
      setLength(redistribute,fill);
      k:=0;
      for i:=0 to length(cached)-1 do for j:=0 to length(cached[i].data)-1 do begin
        redistribute[k]:=cached[i].data[j];
        inc(k);
      end;
      fill:=length(redistribute);
      setLength(cached,length(cached)*2);
      for i:=0 to length(cached)-1 do with cached[i] do setLength(data,0);
      for k:=0 to length(redistribute)-1 do begin
        i:=redistribute[k].keyHash and (length(cached)-1);
        with cached[i] do begin
          j:=length(data);
          setLength(data,j+1);
          data[j]:=redistribute[k];
        end;
      end;
      setLength(redistribute,0);
      k:=0; j:=-1;
      for i:=0 to length(cached)-1 do begin
        if length(cached[i].data)>k then begin
          k:=length(cached[i].data);
          j:=i;
        end;
      end;
    end;

  VAR i:longint;
      hash:T_hashInt;
      binIdx: T_hashInt;
  begin
    enterCriticalSection(criticalSection);
    try
      hash:=key^.hash;
      binIdx:=hash and (length(cached)-1);
      with cached[binIdx] do begin
        i := 0;
        while (i<length(data)) and not((data[i].keyHash=hash) and key^.equals(data[i].key)) do inc(i);
        if (i>=length(data)) then begin
          setLength(data, i+1);
          inc(fill);
          data[i].key      :=P_listLiteral(key^.rereferenced);
          data[i].keyHash  :=hash;
          data[i].value    :=value^.rereferenced;
        end else if timeout<1E20 then begin
          recycler^.disposeLiteral(data[i].value);
          data[i].value    :=value^.rereferenced;
        end;
        data[i].lastUse  :=useCounter;
        data[i].goodUntil:=now+timeout;
      end;
      inc(useCounter);
      if (fill>MAX_ACCEPTED_COLLISIONS*length(cached)) then grow;
    finally
      leaveCriticalSection(criticalSection);
    end;
  end;

FUNCTION T_cache.get(CONST key: P_listLiteral): P_literal;
  VAR i: longint;
      hash:T_hashInt;
      binIdx: T_hashInt;
      now_: TDateTime;
  begin
    now_:=now;
    enterCriticalSection(criticalSection);
    try
      hash:=key^.hash;
      binIdx:=hash and (length(cached)-1);
      with cached[binIdx] do begin
        i := 0;
        while (i<length(data)) and not((data[i].keyHash=hash) and key^.equals(data[i].key) and (data[i].goodUntil>now_)) do inc(i);
        if i>=length(data) then result:=nil
        else begin
          inc(useCounter);
          data[i].lastUse:=useCounter;
          result:=data[i].value;
        end;
      end;
    finally
      leaveCriticalSection(criticalSection);
    end;
  end;

FUNCTION T_cache.getIfNotLocked(CONST key: P_listLiteral): P_literal;
  VAR i: longint;
      hash:T_hashInt;
      binIdx: T_hashInt;
      now_: TDateTime;
  begin
    if tryEnterCriticalsection(criticalSection)=0 then exit(nil) else begin
      now_:=now;
      try
        hash:=key^.hash;
        binIdx:=hash and (length(cached)-1);
        with cached[binIdx] do begin
          i := 0;
          while (i<length(data)) and not((data[i].keyHash=hash) and key^.equals(data[i].key) and (data[i].goodUntil>now_)) do inc(i);
          if i>=length(data) then result:=nil
          else begin
            inc(useCounter);
            data[i].lastUse:=useCounter;
            result:=data[i].value;
          end;
        end;
      finally
        leaveCriticalSection(criticalSection);
      end;
    end;
  end;

PROCEDURE T_cache.clear;
  VAR i, j: longint;
  begin
    enterCriticalSection(criticalSection);
    try
      for i := 0 to (length(cached)-1) do with cached[i] do begin
        for j := 0 to length(data)-1 do with data[j] do begin
          globalLiteralRecycler.disposeLiteral(key);
          globalLiteralRecycler.disposeLiteral(value);
        end;
        setLength(data, 0);
      end;
      setLength(cached,MIN_BIN_COUNT);
      fill := 0;
      useCounter:=0;
    finally
      leaveCriticalSection(criticalSection);
    end;
  end;

FUNCTION T_cache.getMap:P_mapLiteral;
  VAR i:longint;
      entry: T_cacheEntry;
  begin
    result:=globalLiteralRecycler.newMapLiteral(fill);
    for i:=0 to length(cached)-1 do for entry in cached[i].data do
      result^.put(@globalLiteralRecycler,entry.key,entry.value,true);
  end;

end.
