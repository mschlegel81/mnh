UNIT mnh_caches;

INTERFACE

USES mnh_litVar, mnh_out_adapters, sysutils, mnh_constants,mySys;
CONST MAX_ACCEPTED_COLLISIONS=10;
      POLISH_FREQUENCY=64;
      MEM_LIMIT:int64=2000000000;
                      //{$ifdef CPU32}
                      //1073741824
                      //{$else}
                      //maxlongint;
                      //{$endif}

TYPE
  T_cacheEntry = record
    key: P_listLiteral;
    value: P_literal;
    useCount: longint;
  end;

  P_cache = ^T_cache;

  T_cache = object
  private
    memoryDistressSignalPending:boolean;
    fill: longint;
    putCounter:longint;
    cached: array of record
      data:array of T_cacheEntry;
    end;
  public
    CONSTRUCTOR create();
    DESTRUCTOR destroy;
    //FUNCTION getBinIdx(CONST key: P_listLiteral):longint;
    PROCEDURE put(CONST key: P_listLiteral; CONST value: P_literal);
    FUNCTION get(CONST key: P_listLiteral): P_literal;
    PROCEDURE clear;
  end;

PROCEDURE clearAllCaches;

IMPLEMENTATION

VAR
  allCaches: array of P_cache;

PROCEDURE clearAllCaches;
  VAR i: longint;
  begin
    for i:=0 to length(allCaches)-1 do
      allCaches[i]^.clear;
  end;

CONSTRUCTOR T_cache.create();
  begin
    fill := 0;
    setLength(cached,1024);
    setLength(allCaches, length(allCaches)+1);
    allCaches[length(allCaches)-1] := @self;
  end;

DESTRUCTOR T_cache.destroy;
  VAR i: longint;
  begin
    clear;
    i := 0;
    while (i<length(allCaches)) and (allCaches [i]<>@self) do inc(i);
    if (i<length(allCaches)) then begin
      allCaches[i] := allCaches [length(allCaches)-1];
      setLength(allCaches, length(allCaches)-1);
    end;
  end;


//FUNCTION T_cache.getBinIdx(CONST key: P_listLiteral):longint;
//  begin
//    result:=key^.hash and (length(cached)-1);
//  end;

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
        swapTmp:T_cacheEntry;
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

  VAR i,binIdx: longint;
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
      binIdx:longint;
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
    setLength(cached,1);
    fill := 0;
    putCounter:=0;
  end;

end.
