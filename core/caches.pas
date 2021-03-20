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
    PROCEDURE polish;
  public
    CONSTRUCTOR create(ruleCS:TRTLCriticalSection);
    DESTRUCTOR destroy;
    PROCEDURE put(CONST key: P_listLiteral; CONST value: P_literal); inline;
    FUNCTION get(CONST key: P_listLiteral): P_literal;
    PROCEDURE clear;
  end;

IMPLEMENTATION
USES recyclers;
CONSTRUCTOR T_cache.create(ruleCS:TRTLCriticalSection);
  begin
    criticalSection:=ruleCS;
    fill := 0;
    useCounter:=0;
    setLength(cached,MIN_BIN_COUNT);
    memoryCleaner.registerObjectForCleanup(@polish);
  end;

DESTRUCTOR T_cache.destroy;
  begin
    memoryCleaner.unregisterObjectForCleanup(@polish);
    clear;
  end;

PROCEDURE T_cache.polish;
  VAR binIdx,i,j: longint;
      dropThreshold:longint;
      recycler:P_recycler;
  begin
    recycler:=newRecycler;
    enterCriticalSection(criticalSection);
    try
      dropThreshold:=useCounter - fill shr 1;
      for binIdx:=0 to length(cached)-1 do
      with cached[binIdx] do begin
        j:=0;
        for i := 0 to length(data)-1 do
        if (data[i].lastUse>dropThreshold) then begin
          data[j] := data[i];
          inc(j);
        end else begin
          recycler^.disposeLiteral(data[i].key);
          recycler^.disposeLiteral(data[i].value);
          dec(fill);
        end;
        setLength(data, j);
      end;
    finally
      leaveCriticalSection(criticalSection);
      freeRecycler(recycler);
    end;
  end;

PROCEDURE T_cache.put(CONST key: P_listLiteral; CONST value: P_literal);
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
          data[i].key     :=P_listLiteral(key^.rereferenced);
          data[i].keyHash :=hash;
          data[i].value   :=value^.rereferenced;
          data[i].lastUse :=useCounter;
        end;
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
  begin
    enterCriticalSection(criticalSection);
    try
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
    finally
      leaveCriticalSection(criticalSection);
    end;
  end;

PROCEDURE T_cache.clear;
  VAR i, j: longint;
      recycler:P_recycler;
  begin
    recycler:=newRecycler;
    enterCriticalSection(criticalSection);
    try
      for i := 0 to (length(cached)-1) do with cached[i] do begin
        for j := 0 to length(data)-1 do with data[j] do begin
          recycler^.disposeLiteral(key);
          recycler^.disposeLiteral(value);
        end;
        setLength(data, 0);
      end;
      setLength(cached,MIN_BIN_COUNT);
      fill := 0;
      useCounter:=0;
    finally
      leaveCriticalSection(criticalSection);
      freeRecycler(recycler);
    end;
  end;

end.
