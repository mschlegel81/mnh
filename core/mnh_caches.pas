UNIT mnh_caches;
INTERFACE
USES mnh_basicTypes, myGenerics, mnh_litVar, mnh_out_adapters, sysutils, mnh_constants,mySys{$ifdef fullVersion},mnh_settings{$endif};
CONST MAX_ACCEPTED_COLLISIONS=10;
      MIN_BIN_COUNT=1;
      POLISH_FREQUENCY=32;

TYPE
  T_cacheEntry = record
    key: P_listLiteral;
    keyHash: T_hashInt;
    value: P_literal;
    useCount: longint;
  end;

  P_cache = ^T_cache;

  T_cache = object
  private
    criticalSection:TRTLCriticalSection;
    fill: longint;
    putCounter:longint;
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
VAR globalMemoryLimit:int64={$ifdef Windows}
                              {$ifdef CPU32}
                              1000000000;
                              {$else}
                              int64(maxLongint)*2;
                              {$endif}
                            {$else}
                            1000000000;
                            {$endif}
    allCaches:T_arrayOfPointer;

PROCEDURE polishAllCaches;
  VAR i:longint;
  begin
    for i:=0 to length(allCaches)-1 do with P_cache(allCaches[i])^ do if system.TryEnterCriticalsection(criticalSection)<>0 then begin
      polish;
      system.leaveCriticalSection(criticalSection);
    end;
  end;

CONSTRUCTOR T_cache.create(ruleCS:TRTLCriticalSection);
  begin
    criticalSection:=ruleCS;
    {$ifdef fullVersion}globalMemoryLimit:=settings.value^.memoryLimit;{$endif}
    fill := 0;
    setLength(cached,MIN_BIN_COUNT);
    append(allCaches,@self);
  end;

DESTRUCTOR T_cache.destroy;
  begin
    dropValues(allCaches,@self);
    clear;
  end;

PROCEDURE T_cache.resortBin(CONST binIdx:longint);
  VAR i,j:longint;
      swapTmp:T_cacheEntry;
  begin
    with cached[binIdx] do
    for i:=1 to length(data)-1 do for j:=0 to i-1 do
    if data[i].useCount>data[j].useCount then begin
      swapTmp:=data[i];
      data[i]:=data[j];
      data[j]:=swapTmp;
    end;
  end;

PROCEDURE T_cache.polish;
  VAR binIdx,i,j: longint;
  begin
    for binIdx:=0 to length(cached)-1 do
    with cached[binIdx] do begin
      j:=0;
      for i := 0 to length(data)-1 do
      if (data[i].useCount>0) then begin
        data[j] := data[i];
        data[j].useCount := data[j].useCount shr 1;
        inc(j);
      end else begin
        disposeLiteral(data[i].key);
        disposeLiteral(data[i].value);
        dec(fill);
      end;
      setLength(data, j);
      resortBin(binIdx);
    end;
    putCounter:=0;
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
      data[i].key     :=key;   key  ^.rereference;
      data[i].keyHash :=hash;
      data[i].value   :=value; value^.rereference;
      data[i].useCount:= 0;
    end;
    inc(putCounter);
    if (MemoryUsed>globalMemoryLimit) then polishAllCaches
    else if (fill>MAX_ACCEPTED_COLLISIONS*length(cached)) then grow;
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
        inc(data[i].useCount);
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
    putCounter:=0;
  end;

INITIALIZATION
  allCaches:=C_EMPTY_POINTER_ARRAY;

end.
