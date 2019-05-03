UNIT caches;
INTERFACE
USES sysutils,
     myGenerics,mySys,
     basicTypes,
     litVar;
CONST MAX_ACCEPTED_COLLISIONS=10;
      MIN_BIN_COUNT=1;

TYPE
  T_cacheEntry = record
    key: P_listLiteral;
    keyHash: T_hashInt;
    value: P_literal;
    lastUse: longint;
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
    PROCEDURE resortBin(CONST binIdx:longint);
    PROCEDURE polish;
  public
    CONSTRUCTOR create(ruleCS:TRTLCriticalSection);
    DESTRUCTOR destroy;
    PROCEDURE put(CONST key: P_listLiteral; CONST value: P_literal);
    FUNCTION get(CONST key: P_listLiteral): P_literal;
    PROCEDURE clear;
  end;

IMPLEMENTATION
VAR allCaches:T_arrayOfPointer;
    allCacheCs:TRTLCriticalSection;
PROCEDURE polishAllCaches;
  VAR i:longint;
  begin
    enterCriticalSection(allCacheCs);
    try
      for i:=0 to length(allCaches)-1 do with P_cache(allCaches[i])^ do if system.tryEnterCriticalsection(criticalSection)<>0 then begin
        try
          polish;
        finally
          system.leaveCriticalSection(criticalSection);
        end;
      end;
    finally
      leaveCriticalSection(allCacheCs);
    end;
  end;

CONSTRUCTOR T_cache.create(ruleCS:TRTLCriticalSection);
  begin
    criticalSection:=ruleCS;
    fill := 0;
    useCounter:=0;
    setLength(cached,MIN_BIN_COUNT);
    enterCriticalSection(allCacheCs);
    try
      append(allCaches,pointer(@self));
    finally
      leaveCriticalSection(allCacheCs);
    end;
  end;

DESTRUCTOR T_cache.destroy;
  begin
    enterCriticalSection(allCacheCs);
    dropValues(allCaches,@self);
    leaveCriticalSection(allCacheCs);
    clear;
  end;

PROCEDURE T_cache.resortBin(CONST binIdx:longint);
  VAR i,j:longint;
      swapTmp:T_cacheEntry;
  begin
    with cached[binIdx] do
    for i:=1 to length(data)-1 do for j:=0 to i-1 do
    if data[i].lastUse>data[j].lastUse then begin
      swapTmp:=data[i];
      data[i]:=data[j];
      data[j]:=swapTmp;
    end;
  end;

PROCEDURE T_cache.polish;
  VAR binIdx,i,j: longint;
      dropThreshold:longint;
  begin
    dropThreshold:=useCounter - fill shr 1;
    for binIdx:=0 to length(cached)-1 do
    with cached[binIdx] do begin
      j:=0;
      for i := 0 to length(data)-1 do
      if (data[i].lastUse>dropThreshold) then begin
        data[j] := data[i];
        inc(j);
      end else begin
        disposeLiteral(data[i].key);
        disposeLiteral(data[i].value);
        dec(fill);
      end;
      setLength(data, j);
      resortBin(binIdx);
    end;
  end;

PROCEDURE T_cache.put(CONST key: P_listLiteral; CONST value: P_literal);
  PROCEDURE grow;
    VAR redistribute:array of T_cacheEntry;
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
        resortBin(i);
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
    hash:=key^.hash;
    binIdx:=hash and (length(cached)-1);
    with cached[binIdx] do begin
      i := 0;
      while (i<length(data)) and not((data[i].keyHash=hash) and key^.equals(data[i].key)) do inc(i);
      if (i<length(data))
      then exit
      else setLength(data, i+1);
      inc(fill);
      data[i].key     :=P_listLiteral(key^.rereferenced);
      data[i].keyHash :=hash;
      data[i].value   :=value^.rereferenced;
      data[i].lastUse :=useCounter;
    end;
    inc(useCounter);
    if (fill>MAX_ACCEPTED_COLLISIONS*length(cached)) then grow;
  end;

FUNCTION T_cache.get(CONST key: P_listLiteral): P_literal;
  VAR i: longint;
      hash:T_hashInt;
      binIdx: T_hashInt;
  begin
    hash:=key^.hash;
    binIdx:=hash and (length(cached)-1);
    with cached[binIdx] do begin
      i := 0;
      while (i<length(data)) and not((data[i].keyHash=hash) and key^.equals(data[i].key)) do inc(i);
      if i>=length(data) then result:=nil
      else begin
        inc(useCounter);
        data[i].lastUse:=useCounter;
        result:=data[i].value;
      end;
    end;
  end;

PROCEDURE T_cache.clear;
  VAR i, j: longint;
  begin
    for i := 0 to (length(cached)-1) do with cached[i] do begin
      for j := 0 to length(data)-1 do with data[j] do begin
        disposeLiteral(key);
        disposeLiteral(value);
      end;
      setLength(data, 0);
    end;
    setLength(cached,MIN_BIN_COUNT);
    fill := 0;
    useCounter:=0;
  end;

INITIALIZATION
  allCaches:=C_EMPTY_POINTER_ARRAY;
  initialize(allCacheCs);
  initCriticalSection(allCacheCs);
  memoryCleaner.registerCleanupMethod(@polishAllCaches);
FINALIZATION
  doneCriticalSection(allCacheCs);

end.
