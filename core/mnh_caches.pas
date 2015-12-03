UNIT mnh_caches;

INTERFACE

USES mnh_litVar, mnh_out_adapters, sysutils, mnh_constants,mySys;
CONST MAX_ACCEPTED_COLLISIONS=10;
      MIN_BIN_COUNT=128;
      POLISH_FREQUENCY=64;
      MEM_LIMIT:int64={$ifdef CPU32}
                      1000000000;
                      {$else}
                      int64(maxLongint)*2;
                      {$endif}

TYPE
  T_cacheEntry = record
    key: P_listLiteral;
    value: P_literal;
    useCount: longint;
  end;

  P_cache = ^T_cache;

  T_cache = object
  private
    fill: longint;
    putCounter:longint;
    cached: array of record
      data:array of T_cacheEntry;
    end;
  public
    CONSTRUCTOR create();
    DESTRUCTOR destroy;
    PROCEDURE put(CONST key: P_listLiteral; CONST value: P_literal);
    FUNCTION get(CONST key: P_listLiteral): P_literal;
    PROCEDURE clear;
  end;

IMPLEMENTATION
CONSTRUCTOR T_cache.create();
  begin
    fill := 0;
    setLength(cached,MIN_BIN_COUNT);
  end;

DESTRUCTOR T_cache.destroy;
  begin
    clear;
  end;

PROCEDURE T_cache.put(CONST key: P_listLiteral; CONST value: P_literal);
  PROCEDURE resortBin(CONST binIdx:longint);
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

  PROCEDURE polish;
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
        i:=redistribute[k].key^.hash and (length(cached)-1);
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
      binIdx: T_hashInt;
  begin
    binIdx:=key^.hash and (length(cached)-1);
    with cached[binIdx] do begin
      i := 0;
      while (i<length(data)) and not (key^.equals(data[i].key)) do inc(i);
      if (i<length(data))
      then exit
      else setLength(data, i+1);
      inc(fill);
      data[i].key  :=key;   key  ^.rereference;
      data[i].value:=value; value^.rereference;
      data[i].useCount:= 0;
    end;
    inc(putCounter);
    if (putCounter>POLISH_FREQUENCY*length(cached)) or (MemoryUsed>MEM_LIMIT) then polish
    else if (fill>MAX_ACCEPTED_COLLISIONS*length(cached)) then grow;
  end;


FUNCTION T_cache.get(CONST key: P_listLiteral): P_literal;
  VAR i: longint;
      binIdx: T_hashInt;
  begin
    binIdx:=key^.hash and (length(cached)-1);
    with cached[binIdx] do begin
      i := 0;
      while (i<length(data)) and not (key^.equals(data[i].key)) do inc(i);
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

end.
