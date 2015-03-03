UNIT mnh_caches;

INTERFACE

USES mnh_litvar, mnh_out_adapters, SysUtils, mnh_constants, EpikTimer;
CONST CACHE_MOD = 4095; //must be 2^n-1 because of bitwise operation used instead of mod

TYPE
  T_cacheEntry = record
    key: P_listLiteral;
    value: P_literal;
    useTime, useCount, cost: longint;
  end;

  P_cache = ^T_cache;

  T_cache = object
  private
    timer:TEpikTimer;
    timeTally: longint;
    fill: longint;
    cached: array[0..CACHE_MOD] of array of T_cacheEntry;
    PROCEDURE polish(CONST costLimit:double);
  public
    CONSTRUCTOR create();
    DESTRUCTOR destroy;
    PROCEDURE put(CONST key: P_listLiteral; CONST value: P_literal; CONST costInSeconds:double);
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
    timer:=TEpikTimer.create(nil);
    timer.Clear;
    timer.Start;
    fill := 0;
    timeTally := 0;
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
    timer.Free;
  end;

PROCEDURE T_cache.polish(CONST costLimit:double);
  VAR binIdx,i,j,timeLimit: longint;
  begin

    timeLimit := timeTally-(fill shr 1);
    fill := 0;
    for binIdx := 0 to CACHE_MOD do begin
      j := 0;
      for i := 0 to length(cached [binIdx])-1 do
      if (cached [binIdx, i].useTime>timeLimit) or
         (double(cached[binIdx,i].cost)*
                 cached[binIdx,i].useCount>=costLimit) then begin
        if i<>j then cached[binIdx,j] := cached [binIdx, i];
        cached[binIdx,j].useCount := cached[binIdx,j].useCount shr 1;
        Inc(j);
      end else begin
        disposeLiteral(cached [binIdx, i].key);
        disposeLiteral(cached [binIdx, i].value);
      end;
      setLength(cached [binIdx], j);
      Inc(fill, j);
    end;
  end;

PROCEDURE T_cache.put(CONST key: P_listLiteral; CONST value: P_literal; CONST costInSeconds:double);
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
    cached[binIdx, i].useTime := timeTally; Inc(timeTally);
    if      costInSeconds<=1E-6 then cached[binIdx, i].cost := 1
    else if costInSeconds>2147  then cached[binIdx, i].cost := 2147000000
                                else cached[binIdx, i].cost := round(1E6*costInSeconds);
  end;

FUNCTION T_cache.get(CONST key: P_listLiteral): P_literal;
  VAR binIdx, i: longint;
  begin
    timer.Clear;
    timer.Start();
    binIdx := key^.hash and CACHE_MOD;
    i := 0;
    while (i<length(cached [binIdx])) and not (key^.equals(cached [binIdx, i].key)) do Inc(i);
    if i>=length(cached [binIdx]) then result:=nil
    else begin
      Inc(cached [binIdx, i].useCount);
      cached[binIdx, i].useTime := timeTally; Inc(timeTally);
      result := cached [binIdx, i].value;

      if 1E6*timer.Elapsed()>cached[binIdx,i].cost then polish(double(cached[binIdx,i].cost)*cached[binIdx, i].useCount);
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
    timeTally := 0;
    timer.Clear;
    timer.Start;
  end;

end.
