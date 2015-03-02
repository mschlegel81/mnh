UNIT mnh_caches;
INTERFACE
USES mnh_litvar;
TYPE
  T_cacheEntry=record
    key:P_listLiteral;
    value:P_literal;
    use:longint;
  end;  
  
  P_cache=^T_cache;
  T_cache=object
    private
      useTally:longint;
      cache_mod:longint;
      fill:longint;
      cached:array of array of T_cacheEntry;
    public
      CONSTRUCTOR create(CONST cacheLimit:longint);
      DESTRUCTOR destroy;
      PROCEDURE put(CONST key:P_listLiteral; CONST value:P_literal);
      FUNCTION  get(CONST key:P_listLiteral):P_literal;
      PROCEDURE clear;
  end;
  
PROCEDURE clearAllCaches;
IMPLEMENTATION
VAR allCaches:array of P_cache;

PROCEDURE clearAllCaches;
  VAR i:longint;
  begin
    for i:=0 to length(allCaches)-1 do allCaches[i]^.clear;
  end;
  
CONSTRUCTOR T_cache.create(CONST cacheLimit:longint);
  begin
    useTally:=0;
    cache_mod:=1;
    while cache_mod*4<cacheLimit do inc(cache_mod,cache_mod);
    dec(cache_mod);
    
    fill:=0;
    setLength(cached,CACHE_MOD+1);
    
    setLength(allCaches,length(allCaches)+1);
    allCaches[length(allCaches)-1]:=@self;
  end;
  
DESTRUCTOR T_cache.destroy;
  VAR i:longint;
  begin
    clear;
    
    i:=0;
    while (i<length(allCaches)) and (allCaches[i]<>@self) do inc(i);
    if (i<length(allCaches)) then begin
      allCaches[i]:=allCaches[length(allCaches)-1];
      setLength(allCaches,length(allCaches)-1);
    end;
  end;

PROCEDURE T_cache.put(CONST key:P_listLiteral; CONST value:P_literal);
  VAR binIdx,i,j,tallyLimit:longint;
  begin
    binIdx:=key^.hash and CACHE_MOD;
    i:=0;
    while (i<length(cached[binIdx])) and not(key^.equals(cached[binIdx,i].key)) do inc(i);
    if (i<length(cached[binIdx])) then begin
      disposeLiteral(cached[binIdx,i].key);
      disposeLiteral(cached[binIdx,i].value);
      dec(fill);
    end else setLength(cached[binIdx],i+1);
    cached[binIdx,i].key  :=key;   key^.  rereference;
    cached[binIdx,i].value:=value; value^.rereference;
    cached[binIdx,i].use  :=useTally; inc(useTally);
    inc(fill);
    
    if fill>cache_mod*4 then begin      
      tallyLimit:=useTally-2*cache_mod;
      fill:=0;
      for binIdx:=0 to length(cached)-1 do begin
        j:=0;
        for i:=0 to length(cached[binIdx])-1 do if cached[binIdx,i].use>=tallyLimit then begin
          if i<>j then cached[binIdx,j]:=cached[binIdx,i];
          inc(j);
        end else begin
          disposeLiteral(cached[binIdx,i].key);
          disposeLiteral(cached[binIdx,i].value);
        end;
        setLength(cached[binIdx],j);        
        inc(fill,j);
      end;
    end;
  end;

FUNCTION T_cache.get(CONST key:P_listLiteral):P_literal;
  VAR binIdx,i:longint;
      tmp:T_cacheEntry;
  begin
    binIdx:=key^.hash and CACHE_MOD;
    i:=0;
    while (i<length(cached[binIdx])) and not(key^.equals(cached[binIdx,i].key)) do inc(i);    
    if i>=length(cached[binIdx]) then exit(nil);
    cached[binIdx,i].use:=useTally; inc(useTally);
    result:=cached[binIdx,i].value;
    while (i>0) do begin    
      tmp:=cached[binIdx,i];
      cached[binIdx,i]:=cached[binIdx,i-1];
      cached[binIdx,i-1]:=tmp;
      dec(i);
    end;
  end;
  
PROCEDURE T_cache.clear;
  VAR i,j:longint;
  begin
    for i:=0 to length(cached)-1 do begin
      for j:=0 to length(cached[i])-1 do with cached[i,j] do begin
        disposeLiteral(key);
        disposeLiteral(value);
        use:=0;
      end;
      setLength(cached[i],0);
    end;
    setLength(cached,0);
    useTally:=0;
  end;

end.
