UNIT mnh_caches;

INTERFACE

USES mnh_litvar, mnh_out_adapters, SysUtils, mnh_constants;
CONST CACHE_MOD = 2047; //must be 2^n-1 because of bitwise operation used instead of mod
      MAX_CACHE_FILL = (CACHE_MOD+1)*4;
      CLEANUP_PARAM = (CACHE_MOD+1)*2;
TYPE
  T_cacheEntry = record
    key: P_listLiteral;
    value: P_literal;
    useTime, useCount, cost: longint;
  end;

  P_cache = ^T_cache;

  T_cache = object
  private
    timeTally: longint;
    fill: longint;
    criticalSection:TRTLCriticalSection;
    cached: array[0..CACHE_MOD] of array of T_cacheEntry;
  public
    CONSTRUCTOR create();
    DESTRUCTOR destroy;
    PROCEDURE polish;
    PROCEDURE put(CONST key: P_listLiteral; CONST value: P_literal; CONST costFactor: longint);
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
    InitCriticalSection(criticalSection);
    fill := 0;
    timeTally := 0;
    setLength(allCaches, length(allCaches)+1);
    allCaches[length(allCaches)-1] := @self;
  end;

DESTRUCTOR T_cache.destroy;
  VAR i: longint;
  begin
    EnterCriticalsection(criticalSection);
    Clear;
    i := 0;
    while (i<length(allCaches)) and (allCaches [i]<>@self) do Inc(i);
    if (i<length(allCaches)) then begin
      allCaches[i] := allCaches [length(allCaches)-1];
      setLength(allCaches, length(allCaches)-1);
    end;
    LeaveCriticalsection(criticalSection);
    DoneCriticalsection(criticalSection);
  end;

PROCEDURE T_cache.polish;
  VAR avgCost: double;
      binIdx,i,j,timeLimit: longint;
  begin
   if (fill>MAX_CACHE_FILL) then begin
     avgCost := 0;
     for i := 0 to CACHE_MOD do for j := 0 to length(cached [i])-1 do with cached [i, j] do avgCost := avgCost+double(cost)*useCount;
     avgCost := avgCost/fill;
     timeLimit := timeTally-CLEANUP_PARAM;
     fill := 0;
     for binIdx := 0 to CACHE_MOD do begin
       j := 0;
       for i := 0 to length(cached [binIdx])-1 do
       if (cached [binIdx, i].useTime>timeLimit) or
          (double(cached [binIdx, i].cost)*cached [binIdx, i].useCount>=avgCost) then begin
         if i<>j then cached[binIdx, j] := cached [binIdx, i];
         cached[binIdx, j].useCount := 0;
         Inc(j);
       end else begin
         disposeLiteral(cached [binIdx, i].key);
         disposeLiteral(cached [binIdx, i].value);
       end;
       setLength(cached [binIdx], j);
       Inc(fill, j);
     end;
   end;
 end;

PROCEDURE T_cache.put(CONST key: P_listLiteral; CONST value: P_literal; CONST costFactor: longint);
  VAR binIdx, i: longint;
  begin
    EnterCriticalsection(criticalSection);
    binIdx := key^.hash and CACHE_MOD;
    i := 0;
    while (i<length(cached [binIdx])) and not (key^.equals(cached [binIdx, i].key)) do Inc(i);
    if (i<length(cached [binIdx])) then begin
      disposeLiteral(cached [binIdx, i].key);
      disposeLiteral(cached [binIdx, i].value);
      Dec(fill);
    end  else setLength(cached [binIdx], i+1);
    cached[binIdx, i].key := key;
    key^.rereference;
    cached[binIdx, i].value := value;
    value^.rereference;
    cached[binIdx, i].useCount := 0;
    cached[binIdx, i].useTime := timeTally; Inc(timeTally);
    if costFactor<=0 then cached[binIdx, i].cost := 1
                     else cached[binIdx, i].cost := 10*costFactor;
    LeaveCriticalsection(criticalSection);
  end;

FUNCTION T_cache.get(CONST key: P_listLiteral): P_literal;
  VAR binIdx, i: longint;
  begin
    EnterCriticalsection(criticalSection);
    binIdx := key^.hash and CACHE_MOD;
    i := 0;
    while (i<length(cached [binIdx])) and not (key^.equals(cached [binIdx, i].key)) do Inc(i);
    if i>=length(cached [binIdx]) then result:=nil
    else begin
      Inc(cached [binIdx, i].useCount);
      cached[binIdx, i].useTime := timeTally; Inc(timeTally);
      result := cached [binIdx, i].value;
    end;
    LeaveCriticalsection(criticalSection);
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
  end;

end.
