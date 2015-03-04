UNIT mnh_caches;

INTERFACE

USES mnh_litvar, mnh_out_adapters, SysUtils, mnh_constants;
CONST CACHE_MOD   = 2047; //must be 2^n-1 because of bitwise operation used instead of mod
      POLISH_FREQ = 20470;

TYPE
  T_cacheEntry = record
    key: P_listLiteral;
    value: P_literal;
    useCount: longint;
  end;

  P_cache = ^T_cache;

  T_cache = object
  private
    putsSinceLastPolish:longint;

    fill: longint;
    cached: array[0..CACHE_MOD] of array of T_cacheEntry;
    PROCEDURE polish;
  public
    CONSTRUCTOR create();
    DESTRUCTOR destroy;
    PROCEDURE put(CONST key: P_listLiteral; CONST value: P_literal);
    FUNCTION get(CONST key: P_listLiteral): P_literal;
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
    putsSinceLastPolish:=0;
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

PROCEDURE T_cache.polish;
  VAR binIdx,i,j: longint;
      swapTmp:T_cacheEntry;
  begin
    fill := 0;
    for binIdx := 0 to CACHE_MOD do begin
      j := 0;
      for i := 0 to length(cached [binIdx])-1 do
      if (cached [binIdx, i].useCount>0) then begin
        if i<>j then cached[binIdx,j] := cached [binIdx, i];
        cached[binIdx,j].useCount := cached[binIdx,j].useCount shr 1;
        Inc(j);
      end else begin
        disposeLiteral(cached [binIdx, i].key);
        disposeLiteral(cached [binIdx, i].value);
      end;
      setLength(cached [binIdx], j);
      Inc(fill, j);
      for i:=1 to length(cached[binIdx])-1 do for j:=0 to i-1 do
      if cached[binIdx,i].useCount>cached[binIdx,j].useCount then begin
        swapTmp:=cached[binIdx,i];
        cached[binIdx,i]:=cached[binIdx,j];
        cached[binIdx,j]:=swapTmp;
      end;
    end;
    putsSinceLastPolish:=0;
  end;

PROCEDURE T_cache.put(CONST key: P_listLiteral; CONST value: P_literal);
  VAR binIdx, i: longint;
  begin
    binIdx := key^.hash and CACHE_MOD;
    i := 0;
    while (i<length(cached [binIdx])) and not (key^.equals(cached [binIdx, i].key)) do Inc(i);
    if (i<length(cached [binIdx])) then begin
      disposeLiteral(cached [binIdx, i].key);
      disposeLiteral(cached [binIdx, i].value);
      Dec(fill);
    end  else setLength(cached [binIdx], i+1);
    inc(fill);
    cached[binIdx, i].key := key;
    key^.rereference;
    cached[binIdx, i].value := value;
    value^.rereference;
    cached[binIdx, i].useCount := 0;
    inc(putsSinceLastPolish);
  end;

FUNCTION T_cache.get(CONST key: P_listLiteral): P_literal;
  VAR binIdx, i: longint;
  begin
    binIdx := key^.hash and CACHE_MOD;
    i := 0;
    while (i<length(cached [binIdx])) and not (key^.equals(cached [binIdx, i].key)) do Inc(i);
    if i>=length(cached [binIdx]) then begin
      result:=nil;
    end else begin
      Inc(cached [binIdx, i].useCount);
      result := cached [binIdx, i].value;
      if putsSinceLastPolish>POLISH_FREQ then polish;
    end;
  end;

PROCEDURE T_cache.Clear;
  VAR i, j: longint;
  begin
    for i := 0 to CACHE_MOD do begin
      for j := 0 to length(cached [i])-1 do with cached [i, j] do begin
        disposeLiteral(key);
        disposeLiteral(value);
      end;
      setLength(cached [i], 0);
    end;
    fill := 0;
    putsSinceLastPolish:=0;
  end;

end.
