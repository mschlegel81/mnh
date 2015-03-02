UNIT mnh_caches;

INTERFACE

USES mnh_litvar, mnh_out_adapters, SysUtils, mnh_constants;

CONST CACHE_MOD=1023;

TYPE
  T_cacheEntry = record
    key: P_listLiteral;
    Value: P_literal;
    use: longint;
  end;

  P_cache = ^T_cache;

  T_cache = object
  private
    useTally: longint;
    fill: longint;
    cached: array[0..CACHE_MOD] of array of T_cacheEntry;
  public
    CONSTRUCTOR Create();
    DESTRUCTOR Destroy;
    PROCEDURE put(CONST key: P_listLiteral; CONST Value: P_literal);
    FUNCTION get(CONST key: P_listLiteral): P_literal;
    PROCEDURE Clear;
  end;

PROCEDURE clearAllCaches;

IMPLEMENTATION

VAR
  allCaches: array of P_cache;

PROCEDURE clearAllCaches;
  VAR
    i: longint;
  begin
    for i := 0 to length(allCaches) - 1 do
      allCaches[i]^.Clear;
  end;

CONSTRUCTOR T_cache.Create();
  begin
    useTally := 0;
    fill := 0;
    setLength(allCaches, length(allCaches) + 1);
    allCaches[length(allCaches) - 1] := @self;
  end;

DESTRUCTOR T_cache.Destroy;
  VAR
    i: longint;
  begin
    Clear;
    i := 0;
    while (i < length(allCaches)) and (allCaches[i] <> @self) do
      Inc(i);
    if (i < length(allCaches)) then
      begin
      allCaches[i] := allCaches[length(allCaches) - 1];
      setLength(allCaches, length(allCaches) - 1);
      end;
  end;

PROCEDURE T_cache.put(CONST key: P_listLiteral; CONST Value: P_literal);
  VAR
    binIdx, i, j, tallyLimit: longint;
  begin
    binIdx := key^.hash and CACHE_MOD;
    i := 0;
    while (i < length(cached[binIdx])) and not (key^.equals(cached[binIdx, i].key)) do
      Inc(i);
    if (i < length(cached[binIdx])) then
      begin
      disposeLiteral(cached[binIdx, i].key);
      disposeLiteral(cached[binIdx, i].Value);
      Dec(fill);
      end
    else
      setLength(cached[binIdx], i + 1);
    cached[binIdx, i].key := key;
    key^.rereference;
    cached[binIdx, i].Value := Value;
    Value^.rereference;
    cached[binIdx, i].use := useTally;
    Inc(useTally);
    Inc(fill);

    if fill > cache_mod * 2 then
      begin
      tallyLimit := useTally - cache_mod;
      fill := 0;
      for binIdx := 0 to length(cached) - 1 do
        begin
        j := 0;
        for i := 0 to length(cached[binIdx]) - 1 do
          if cached[binIdx, i].use >= tallyLimit then
            begin
            if i <> j then
              cached[binIdx, j] := cached[binIdx, i];
            Inc(j);
            end
          else
            begin
            disposeLiteral(cached[binIdx, i].key);
            disposeLiteral(cached[binIdx, i].Value);
            end;
        setLength(cached[binIdx], j);
        Inc(fill, j);
        end;
      end;
  end;

FUNCTION T_cache.get(CONST key: P_listLiteral): P_literal;
  VAR
    binIdx, i: longint;
    tmp: T_cacheEntry;
  begin
    binIdx := key^.hash and CACHE_MOD;
    i := 0;
    while (i < length(cached[binIdx])) and not (key^.equals(cached[binIdx, i].key)) do
      Inc(i);
    if i >= length(cached[binIdx]) then
      exit(nil);
    cached[binIdx, i].use := useTally;
    Inc(useTally);
    result := cached[binIdx, i].Value;
    while (i > 0) do
      begin
      tmp := cached[binIdx, i];
      cached[binIdx, i] := cached[binIdx, i - 1];
      cached[binIdx, i - 1] := tmp;
      Dec(i);
      end;
  end;

PROCEDURE T_cache.Clear;
  VAR
    i, j: longint;
  begin
    for i := 0 to length(cached) - 1 do
      begin
      for j := 0 to length(cached[i]) - 1 do
        with cached[i, j] do
          begin
          disposeLiteral(key);
          disposeLiteral(Value);
          use := 0;
          end;
      setLength(cached[i], 0);
      end;
    useTally := 0;
  end;

end.
