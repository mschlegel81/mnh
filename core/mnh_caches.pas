UNIT mnh_caches;

INTERFACE

USES mnh_litvar, mnh_out_adapters, SysUtils, mnh_constants;
CONST CACHE_MOD   = 2047; //must be 2^n-1 because of bitwise operation used instead of mod
      POLISH_FREQ = 16;

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
    cached: array[0..CACHE_MOD] of record
      putsSinceLastPolish:longint;
      data:array of T_cacheEntry;
    end;
    PROCEDURE polishBin(CONST binIdx:longint);
  public
    CONSTRUCTOR create();
    DESTRUCTOR destroy;
    FUNCTION getBinIdx(CONST key: P_listLiteral):longint;
    PROCEDURE put(CONST key: P_listLiteral; CONST binIdx:longint; CONST value: P_literal);
    FUNCTION get(CONST key: P_listLiteral; CONST binIdx:longint): P_literal;
    PROCEDURE Clear;
  end;

PROCEDURE clearAllCaches;

IMPLEMENTATION

VAR
  allCaches: array of P_cache;

PROCEDURE clearAllCaches;
  VAR i: longint;
  begin
    for i := 0 to length(allCaches)-1 do
      allCaches[i]^.Clear;
  end;

CONSTRUCTOR T_cache.create();
  begin
    fill := 0;
    setLength(allCaches, length(allCaches)+1);
    allCaches[length(allCaches)-1] := @self;
  end;

DESTRUCTOR T_cache.destroy;
  VAR i: longint;
  begin
    Clear;
    i := 0;
    while (i<length(allCaches)) and (allCaches [i]<>@self) do Inc(i);
    if (i<length(allCaches)) then begin
      allCaches[i] := allCaches [length(allCaches)-1];
      setLength(allCaches, length(allCaches)-1);
    end;
  end;

PROCEDURE T_cache.polishBin(CONST binIdx:longint);
  VAR i,j: longint;
      swapTmp:T_cacheEntry;
  begin
    j := 0;
    with cached[binIdx] do begin
      for i := 0 to length(cached [binIdx].data)-1 do
      if (data[i].useCount<>0) then begin
        if i<>j then data[j] := data[i];
        data[j].useCount := data[j].useCount shr 1;
        Inc(j);
      end else begin
        disposeLiteral(data[i].key);
        disposeLiteral(data[i].value);
        dec(fill);
      end;
      if j=length(data) then writeln('uneffective polish of bin ',binIdx);
      setLength(data, j);
      for i:=1 to length(data)-1 do for j:=0 to i-1 do
      if data[i].useCount>data[j].useCount then begin
        swapTmp:=data[i];
        data[i]:=data[j];
        data[j]:=swapTmp;
      end;
      putsSinceLastPolish:=0;
    end;
  end;

FUNCTION T_cache.getBinIdx(CONST key: P_listLiteral):longint;
  begin
    result:=key^.hash and CACHE_MOD;
  end;

PROCEDURE T_cache.put(CONST key: P_listLiteral; CONST binIdx:longint; CONST value: P_literal);
  VAR i: longint;
  begin
    with cached[binIdx] do begin
      i := 0;
      while (i<length(data)) and not (key^.equals(data[i].key)) do Inc(i);
      if (i<length(data))
      then exit
      else setLength(data, i+1);
      inc(fill);
      data[i].key  :=key;   key  ^.rereference;
      data[i].value:=value; value^.rereference;
      data[i].useCount:= 1;
      inc(putsSinceLastPolish);
      if (putsSinceLastPolish>POLISH_FREQ) and
         (length(data)       >POLISH_FREQ) then polishBin(binIdx);
    end;
  end;


FUNCTION T_cache.get(CONST key: P_listLiteral; CONST binIdx:longint): P_literal;
  VAR i: longint;
  begin
    with cached[binIdx] do begin
      i := 0;
      while (i<length(data)) and not (key^.equals(data[i].key)) do Inc(i);
      if i>=length(data) then result:=nil
      else begin
        Inc(data[i].useCount);
        result:=data[i].value;
      end;
    end;
  end;

PROCEDURE T_cache.Clear;
  VAR i, j: longint;
  begin
    for i := 0 to CACHE_MOD do with cached[i] do begin
      for j := 0 to length(data)-1 do with data[j] do begin
        disposeLiteral(key);
        disposeLiteral(value);
      end;
      setLength(data, 0);
      putsSinceLastPolish:=0;
    end;
    fill := 0;
  end;

end.
