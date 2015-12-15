UNIT myGenerics;

{mode objfpc}{H+}

INTERFACE
USES sysutils;
TYPE
  GENERIC G_list<ENTRY_TYPE>=object
    TYPE
      ENTRY_TYPE_ARRAY=array of ENTRY_TYPE;
    private VAR
      entry:ENTRY_TYPE_ARRAY;
      sortedUntilIndex:longint;
      isUnique:boolean;
      cs:system.TRTLCriticalSection;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION contains(CONST value:ENTRY_TYPE):boolean;
      FUNCTION indexOf(CONST value:ENTRY_TYPE):longint;
      PROCEDURE add(CONST value:ENTRY_TYPE);
      PROCEDURE remValue(CONST value:ENTRY_TYPE);
      PROCEDURE remValues(CONST values:ENTRY_TYPE_ARRAY);
      PROCEDURE remIndex(CONST index:longint);
      PROCEDURE addAll(CONST values:ENTRY_TYPE_ARRAY);
      PROCEDURE clear;
      PROCEDURE sort;
      PROCEDURE unique;
      FUNCTION size:longint;
      FUNCTION getEntry(CONST index:longint):ENTRY_TYPE;
      PROPERTY element[index:longint]:ENTRY_TYPE read getEntry; default;
      FUNCTION elementArray:ENTRY_TYPE_ARRAY;
  end;

  T_listOfString=specialize G_list<ansistring>;
  T_listOfIntegers=specialize G_list<longint>;

  GENERIC G_sparseArray<ENTRY_TYPE>=object
    private
      TYPE INDEXED_ENTRY=record index:longint; value:ENTRY_TYPE; end;
      VAR map:array of array of INDEXED_ENTRY;
      hashMask:longint;
      entryCount:longint;
      FUNCTION indexInMap(CONST mapIdx,searchIdx:longint):longint;
      PROCEDURE rehash(CONST grow:boolean);
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE add(CONST index:longint; CONST value:ENTRY_TYPE);
      FUNCTION containsIndex(CONST index:longint; OUT value:ENTRY_TYPE):boolean;
      FUNCTION remove(CONST index:longint):boolean;
      FUNCTION size:longint;
      FUNCTION getEntry(CONST iterator:longint):ENTRY_TYPE;
      FUNCTION getIndex(CONST iterator:longint):longint;
  end;

  T_arrayOfString=array of ansistring;
  OPERATOR :=(x:ansistring):T_arrayOfString;
  PROCEDURE append(VAR x:T_arrayOfString; CONST y:ansistring);
  PROCEDURE appendIfNew(VAR x:T_arrayOfString; CONST y:ansistring);
  PROCEDURE append(VAR x:T_arrayOfString; CONST y:T_arrayOfString);
  PROCEDURE dropFirst(VAR x:T_arrayOfString; CONST dropCount:longint);
  FUNCTION C_EMPTY_STRING_ARRAY:T_arrayOfString;
  { G_stringKeyMap }
TYPE
  GENERIC G_stringKeyMap<VALUE_TYPE>=object
    TYPE VALUE_TYPE_ARRAY=array of VALUE_TYPE;
         KEY_VALUE_PAIR=record
           hash:longint;
           key:ansistring;
           value:VALUE_TYPE;
         end;
         KEY_VALUE_LIST=array of KEY_VALUE_PAIR;
    private VAR
      cs:TRTLCriticalSection;
      entryCount:longint;
      rebalanceFac:double;
      bitMask:longint;
      bucket:array of KEY_VALUE_LIST;
      PROCEDURE rehash(grow:boolean);
    public
      CONSTRUCTOR create(rebalanceFactor:double);
      CONSTRUCTOR create();
      DESTRUCTOR destroy;
      FUNCTION containsKey(CONST key:ansistring; OUT value:VALUE_TYPE):boolean;
      FUNCTION containsKey(CONST key:ansistring):boolean;
      FUNCTION get(CONST key:ansistring):VALUE_TYPE;
      PROCEDURE put(CONST key:ansistring; CONST value:VALUE_TYPE);
      PROCEDURE dropKey(CONST key:ansistring);
      PROCEDURE clear;
      FUNCTION keySet:T_arrayOfString;
      FUNCTION valueSet:VALUE_TYPE_ARRAY;
      FUNCTION entrySet:KEY_VALUE_LIST;
      FUNCTION size:longint;
      FUNCTION dropAny:VALUE_TYPE;
  end;

  { G_safeVar }

  GENERIC G_safeVar<ENTRY_TYPE>=object
    private VAR
      v :ENTRY_TYPE;
      saveCS:TRTLCriticalSection;
      FUNCTION getValue:ENTRY_TYPE;
      PROCEDURE setValue(CONST newValue:ENTRY_TYPE);
    public
      CONSTRUCTOR create(CONST intialValue:ENTRY_TYPE);
      DESTRUCTOR destroy;
      PROPERTY value:ENTRY_TYPE read getValue write setValue;
  end;

  { G_safeArray }

  GENERIC G_safeArray<ENTRY_TYPE>=object
    TYPE ENTRY_TYPE_ARRAY=array of ENTRY_TYPE;
    private VAR
      data :ENTRY_TYPE_ARRAY;
      saveCS:TRTLCriticalSection;
      FUNCTION getValue(index:longint):ENTRY_TYPE;
      PROCEDURE setValue(index:longint; newValue:ENTRY_TYPE);
    public
      CONSTRUCTOR create();
      DESTRUCTOR destroy;
      PROCEDURE clear;
      FUNCTION size:longint;
      PROPERTY value[index:longint]:ENTRY_TYPE read getValue write setValue; default;
      PROCEDURE append(CONST newValue:ENTRY_TYPE);
      PROCEDURE appendAll(CONST newValue:ENTRY_TYPE_ARRAY);
      PROCEDURE lock;
      PROCEDURE unlock;
  end;

  { G_lazyVar }

  GENERIC G_lazyVar<ENTRY_TYPE>=object
    TYPE T_obtainer=FUNCTION():ENTRY_TYPE;
         T_disposer=PROCEDURE(x:ENTRY_TYPE);
    private
      valueObtained:boolean;
      obtainer:T_obtainer;
      disposer:T_disposer;
      v :ENTRY_TYPE;
      FUNCTION getValue:ENTRY_TYPE;
    public
      CONSTRUCTOR create(CONST o:T_obtainer; CONST d:T_disposer);
      DESTRUCTOR destroy;
      PROPERTY value:ENTRY_TYPE read getValue;
  end;

FUNCTION hashOfAnsiString(CONST x:ansistring):longint; inline;

IMPLEMENTATION
OPERATOR :=(x:ansistring):T_arrayOfString;
  begin
    setLength(result,1);
    result[0]:=x;
  end;

PROCEDURE append(VAR x:T_arrayOfString; CONST y:ansistring);
  begin
    setLength(x,length(x)+1);
    x[length(x)-1]:=y;
  end;

PROCEDURE appendIfNew(VAR x:T_arrayOfString; CONST y:ansistring);
  VAR i:longint;
  begin
    i:=0;
    while (i<length(x)) and (x[i]<>y) do inc(i);
    if i>=length(x) then begin
      setLength(x,length(x)+1);
      x[length(x)-1]:=y;
    end;
  end;

PROCEDURE append(VAR x:T_arrayOfString; CONST y:T_arrayOfString);
  VAR i,i0:longint;
  begin
    i0:=length(x);
    setLength(x,i0+length(y));
    for i:=0 to length(y)-1 do x[i+i0]:=y[i];
  end;

PROCEDURE dropFirst(VAR x:T_arrayOfString; CONST dropCount:longint);
  VAR i,dc:longint;
  begin
    if dropCount<=0 then exit;
    if dropCount>length(x) then dc:=length(x) else dc:=dropCount;
    for i:=0 to length(x)-dc-1 do x[i]:=x[i+dc];
    setLength(x,length(x)-dc);
  end;

FUNCTION C_EMPTY_STRING_ARRAY:T_arrayOfString; begin setLength(result,0); end;

FUNCTION hashOfAnsiString(CONST x:ansistring):longint; inline;
  VAR i:longint;
  begin
    {$Q-}
    result:=length(x);
    for i:=1 to length(x) do result:=result*31+ord(x[i]);
    {$Q+}
  end;

{ G_safeArray }

CONSTRUCTOR G_safeArray.create;
  begin
    system.initCriticalSection(saveCS);
    clear;
  end;

FUNCTION G_safeArray.getValue(index: longint): ENTRY_TYPE;
  begin
    system.enterCriticalSection(saveCS);
    result:=data[index];
    system.leaveCriticalSection(saveCS);
  end;

PROCEDURE G_safeArray.setValue(index: longint; newValue: ENTRY_TYPE);
  begin
    system.enterCriticalSection(saveCS);
    if index=length(data) then append(newValue)
                          else data[index]:=newValue;
    system.leaveCriticalSection(saveCS);
  end;

PROCEDURE G_safeArray.clear;
  begin
    system.enterCriticalSection(saveCS);
    setLength(data,0);
    system.leaveCriticalSection(saveCS);
  end;

FUNCTION G_safeArray.size: longint;
  begin
    system.enterCriticalSection(saveCS);
    result:=length(data);
    system.leaveCriticalSection(saveCS);
  end;

PROCEDURE G_safeArray.append(CONST newValue: ENTRY_TYPE);
  begin
    system.enterCriticalSection(saveCS);
    setLength(data,length(data)+1);
    data[length(data)-1]:=newValue;
    system.leaveCriticalSection(saveCS);
  end;

PROCEDURE G_safeArray.appendAll(CONST newValue: ENTRY_TYPE_ARRAY);
  VAR i,i0:longint;
  begin
    system.enterCriticalSection(saveCS);
    i0:=length(data);
    setLength(data,i0+length(newValue));
    for i:=0 to length(newValue)-1 do data[i0+i]:=newValue[i];
    system.leaveCriticalSection(saveCS);
  end;


PROCEDURE G_safeArray.lock;
  begin
    system.enterCriticalSection(saveCS);
  end;

PROCEDURE G_safeArray.unlock;
  begin
    system.leaveCriticalSection(saveCS);
  end;

DESTRUCTOR G_safeArray.destroy;
  begin
    clear;
    system.doneCriticalSection(saveCS);
  end;

{ G_lazyVar }

FUNCTION G_lazyVar.getValue: ENTRY_TYPE;
  begin
    if not(valueObtained) then begin
      v:=obtainer();
      valueObtained:=true;
    end;
    result:=v;
  end;

CONSTRUCTOR G_lazyVar.create(CONST o:T_obtainer; CONST d:T_disposer);
  begin
    obtainer:=o;
    disposer:=d;
    valueObtained:=false;
  end;

DESTRUCTOR G_lazyVar.destroy;
  begin
    if valueObtained and (disposer<>nil) then disposer(v);
  end;

{ G_safeVar }

CONSTRUCTOR G_safeVar.create(CONST intialValue: ENTRY_TYPE);
  begin
    system.initCriticalSection(saveCS);
    v:=intialValue;
  end;

FUNCTION G_safeVar.getValue: ENTRY_TYPE;
  begin
    system.enterCriticalSection(saveCS);
    result:=v;
    system.leaveCriticalSection(saveCS);
  end;

PROCEDURE G_safeVar.setValue(CONST newValue: ENTRY_TYPE);
  begin
    system.enterCriticalSection(saveCS);
    v:=newValue;
    system.leaveCriticalSection(saveCS);
  end;

DESTRUCTOR G_safeVar.destroy;
  begin
    system.doneCriticalSection(saveCS);
  end;

CONSTRUCTOR G_list.create;
  begin system.initCriticalSection(cs); clear; end;

DESTRUCTOR G_list.destroy;
  begin clear; system.doneCriticalSection(cs); end;

FUNCTION G_list.contains(CONST value:ENTRY_TYPE):boolean;
  begin result:=indexOf(value)>=0; end;

FUNCTION G_list.indexOf(CONST value:ENTRY_TYPE):longint;
  VAR i0,i1:longint;
  begin
    system.enterCriticalSection(cs);
    i0:=0;
    i1:=sortedUntilIndex-1;
    while i1>=i0 do begin
      result:=(i0+i1) shr 1;
      if      entry[result]<value then i0:=result+1
      else if entry[result]>value then i1:=result-1
      else begin
        system.leaveCriticalSection(cs);
        exit(result);
      end;
    end;
    for i0:=sortedUntilIndex to length(entry)-1 do
      if entry[i0]=value then begin
        system.leaveCriticalSection(cs);
        exit(i0);
      end;
    result:=-1;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE G_list.add(CONST value:ENTRY_TYPE);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(entry);
    setLength(entry,i+1);
    entry[i]:=value;
    isUnique:=false;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE G_list.remIndex(CONST index:longint);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    if (index>=0) and (index<length(entry)) then begin
      if index<sortedUntilIndex then dec(sortedUntilIndex);
      for i:=index to length(entry)-2 do entry[i]:=entry[i+1];
      setLength(entry,length(entry)-1);
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE G_list.remValue(CONST value:ENTRY_TYPE);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=indexOf(value);
    while i>=0 do begin
      remIndex(i);
      i:=indexOf(value);
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE G_list.remValues(CONST values:ENTRY_TYPE_ARRAY);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(values)-1 do remValue(values[i]);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE G_list.addAll(CONST values:ENTRY_TYPE_ARRAY);
  VAR i,i0:longint;
  begin
    system.enterCriticalSection(cs);
    i0:=length(entry);
    setLength(entry,length(entry)+length(values));
    for i:=0 to length(values)-1 do entry[i0+i]:=values[i];
    isUnique:=false;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE G_list.clear;
  begin
    system.enterCriticalSection(cs);
    setLength(entry,0);
    sortedUntilIndex:=0;
    isUnique:=true;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE G_list.sort;
  VAR scale    :longint;
      i,j0,j1,k:longint;
      temp     :ENTRY_TYPE_ARRAY;
  begin
    system.enterCriticalSection(cs);
    if sortedUntilIndex<length(entry) then begin
      scale:=1;
      setLength(temp,length(entry)-sortedUntilIndex);
      while scale<length(temp) do begin
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while i<length(temp) do begin
          j0:=sortedUntilIndex+i;
          j1:=sortedUntilIndex+i+scale;
          k :=i;
          while (j0<sortedUntilIndex+i+scale) and (j1<sortedUntilIndex+i+scale+scale) and (j1<length(entry)) do begin
            if entry[j0]<=entry[j1]
              then begin temp[k]:=entry[j0]; inc(k); inc(j0); end
              else begin temp[k]:=entry[j1]; inc(k); inc(j1); end;
          end;
          while (j0<sortedUntilIndex+i+scale)       and (j0<length(entry)) do begin temp[k]:=entry[j0]; inc(k); inc(j0); end;
          while (j1<sortedUntilIndex+i+scale+scale) and (j1<length(entry)) do begin temp[k]:=entry[j1]; inc(k); inc(j1); end;
          inc(i,scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale,scale);
        if (scale<length(temp)) then begin
          //The following is equivalent to the above with swapped roles of "list" and "temp".
          //while making the code a little more complicated it avoids unnecessary copys.
          //merge lists of size [scale] to lists of size [scale+scale]:---------------
          i:=0;
          while i<length(temp) do begin
            j0:=i;
            j1:=i+scale;
            k :=sortedUntilIndex+i;
            while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp)) do begin
              if temp[j0]<=temp[j1]
                then begin entry[k]:=temp[j0]; inc(k); inc(j0); end
                else begin entry[k]:=temp[j1]; inc(k); inc(j1); end;
            end;
            while (j0<i+scale)       and (j0<length(temp)) do begin entry[k]:=temp[j0]; inc(k); inc(j0); end;
            while (j1<i+scale+scale) and (j1<length(temp)) do begin entry[k]:=temp[j1]; inc(k); inc(j1); end;
            inc(i,scale+scale);
          end;
          //---------------:merge lists of size [scale] to lists of size [scale+scale]
          inc(scale,scale);
        end else for k:=0 to length(temp)-1 do entry[sortedUntilIndex+k]:=temp[k];
      end;
      setLength(temp,length(entry));
      for k:=0 to length(entry)-1 do temp[k]:=entry[k];
      j0:=0;
      j1:=sortedUntilIndex;
      k:=0;
      while (j0<sortedUntilIndex) and (j1<length(entry)) do begin
        if temp[j0]<=temp[j1]
          then begin entry[k]:=temp[j0]; inc(k); inc(j0); end
          else begin entry[k]:=temp[j1]; inc(k); inc(j1); end;
      end;
      while (j0<sortedUntilIndex) do begin entry[k]:=temp[j0]; inc(k); inc(j0); end;
      while (j1<length(temp))     do begin entry[k]:=temp[j1]; inc(k); inc(j1); end;
      sortedUntilIndex:=length(entry);
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE G_list.unique;
  VAR i,j:longint;
  begin
    system.enterCriticalSection(cs);
    if not(isUnique) then begin
      sort;
      j:=1;
      for i:=1 to length(entry)-1 do if entry[i]<>entry[i-1] then begin
        entry[j]:=entry[i]; inc(j);
      end;
      setLength(entry,j);
      sortedUntilIndex:=length(entry);
    end;
    isUnique:=true;
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_list.size:longint;
  begin
    system.enterCriticalSection(cs);
    result:=length(entry);
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_list.getEntry(CONST index:longint):ENTRY_TYPE;
  begin
    system.enterCriticalSection(cs);
    result:=entry[index];
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_list.elementArray:ENTRY_TYPE_ARRAY;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    setLength(result,length(entry));
    for i:=0 to length(result)-1 do result[i]:=entry[i];
    system.leaveCriticalSection(cs);
  end;


{CONSTRUCTOR G_hashMap.create(hashFunc:HASH_FUNC; rebalanceFactor:double);
  begin
    hash:=hashFunc;
    setLength(bucket,1);
    bitMask:=0;
    rebalanceFac:=rebalanceFactor;
    entryCount:=0;
  end;

CONSTRUCTOR G_hashMap.create(hashFunc:HASH_FUNC);
  begin
    if hashFunc=nil then Raise Exception.create ('Hash func is nil!');
    hash:=hashFunc;
    setLength(bucket,1);
    bitMask:=0;
    rebalanceFac:=4;
    entryCount:=0;
  end;

PROCEDURE G_hashMap.rehash(grow:boolean);
  VAR i,i0,j,k,c0,c1,newMask:longint;
      temp:array of KEY_VALUE_PAIR;
  begin
    if grow then begin
      i0:=length(bucket);
      setLength(bucket,i0+i0);
      newMask:=i0+i0-1;
      for i:=0 to i0-1 do begin
        temp:=bucket[i];
        setLength(bucket[i+i0],length(bucket[i]));
        c0:=0;
        c1:=0;
        for j:=0 to length(temp)-1 do begin
          k:=temp[j].hash and newMask;
          if k=i then begin
            bucket[i][c0]:=temp[j];
            inc(c0);
          end else begin
            bucket[k][c1]:=temp[j];
            inc(c1);
          end;
        end;
        setLength(bucket[i   ],c0);
        setLength(bucket[i+i0],c1);
      end;
      bitMask:=newMask;
    end else begin
      i0:=length(bucket) shr 1;
      newMask:=i0-1;
      for i:=0 to i0-1 do
      for j:=0 to length(bucket[i0+i])-1 do begin
        setLength(bucket[i],length(bucket[i])+1);
        bucket[i][length(bucket[i])-1]:=bucket[i0+i][j];
      end;
    end;
  end;


DESTRUCTOR G_hashMap.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(bucket)-1 do setLength(bucket[i],0);
    setLength(bucket,0);
  end;

FUNCTION G_hashMap.containsKey(key:KEY_TYPE; OUT value:VALUE_TYPE):boolean;
  VAR i,j:longint;
  begin
    i:=hash(key) and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if (j<length(bucket[i])) then begin
      value:=bucket[i][j].value;
      result:=true;
    end else result:=false;
  end;

PROCEDURE G_hashMap.put(key:KEY_TYPE; value:VALUE_TYPE);
  VAR i,j,h:longint;
  begin
    h:=hash(key);
    i:=h and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if j>=length(bucket[i]) then begin
      setLength(bucket[i],j+1);
      bucket[i][j].key:=key;
      bucket[i][j].hash:=h;
      bucket[i][j].value:=value;
      inc(entryCount);
      if entryCount>length(bucket)*rebalanceFac then rehash(true);
    end else begin
      bucket[i][j].value:=value;
    end;
  end;

PROCEDURE G_hashMap.dropKey(key:KEY_TYPE);
  VAR i,j:longint;
  begin
    i:=hash(key) and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if j<length(bucket[i]) then begin
      while j<length(bucket[i])-1 do begin
        bucket[i][j]:=bucket[i][j+1];
        inc(j);
      end;
      setLength(bucket[i],length(bucket[i])-1);
      dec(entryCount);
      if entryCount<0.4*length(bucket)*rebalanceFac then rehash(false);
    end;
  end;

PROCEDURE G_hashMap.clear;
  VAR i:longint;
  begin
    for i:=0 to length(bucket)-1 do setLength(bucket[i],0);
    setLength(bucket,1);
    bitMask:=0;
    entryCount:=0;
  end;

FUNCTION G_hashMap.keySet:KEY_TYPE_ARRAY;
  VAR k,i,j:longint;
  begin
    setLength(result,entryCount);
    k:=0;
    for i:=0 to length(bucket)-1 do
    for j:=0 to length(bucket[i])-1 do begin
      result[k]:=bucket[i][j].key;
      inc(k);
    end;
  end;

FUNCTION G_hashMap.valueSet:VALUE_TYPE_ARRAY;
  VAR k,i,j:longint;
  begin
    setLength(result,entryCount);
    k:=0;
    for i:=0 to length(bucket)-1 do
    for j:=0 to length(bucket[i])-1 do begin
      result[k]:=bucket[i][j].value;
      inc(k);
    end;
  end;

FUNCTION G_hashMap.size:longint;
  begin
    result:=entryCount;
  end;}

CONSTRUCTOR G_sparseArray.create;
  begin
    hashMask:=0;
    entryCount:=0;
    setLength(map,1);
    setLength(map[0],0);
  end;

DESTRUCTOR G_sparseArray.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(map)-1 do setLength(map[i],0);
    setLength(map,0);
  end;

FUNCTION G_sparseArray.indexInMap(CONST mapIdx,searchIdx:longint):longint;
  VAR i0:longint;
  begin
    for i0:=0 to length(map[mapIdx])-1 do if map[mapIdx,i0].index=searchIdx then exit(i0);
    result:=-1;
  end;

PROCEDURE G_sparseArray.add(CONST index:longint; CONST value:ENTRY_TYPE);
  VAR hash:longint;
      i:longint;
  begin
    hash:=index and hashMask;
    i:=indexInMap(hash,index);
    if i<0 then begin
      i:=length(map[hash]);
      setLength(map[hash],i+1);
      map[hash,i].index:=index;
      inc(entryCount);
      map[hash,i].value:=value;
      if length(map) shl 4<entryCount then rehash(true);
    end else map[hash,i].value:=value;
  end;

FUNCTION G_sparseArray.containsIndex(CONST index:longint; OUT value:ENTRY_TYPE):boolean;
  VAR hash:longint;
      i:longint;
  begin
    hash:=index and hashMask;
    i:=indexInMap(hash,index);
    if i<0 then result:=false
    else begin
      result:=true;
      value:=map[hash,i].value;
    end;
  end;

FUNCTION G_sparseArray.remove(CONST index:longint):boolean;
  VAR hash:longint;
      i,j:longint;
  begin
    hash:=index and hashMask;
    i:=indexInMap(hash,index);
    if i<0 then result:=false
    else begin
      for j:=i to length(map[hash])-2 do map[hash][j]:=map[hash][j+1];
      setLength(map[hash],length(map[hash])-1);
      dec(entryCount);
      result:=true;
      if length(map) shl 3>entryCount then rehash(false);
    end;
  end;

PROCEDURE G_sparseArray.rehash(CONST grow:boolean);
  VAR i,k,j0,j1,oldLen:longint;
  begin
    if (length(map)>1) and not(grow) then begin
      //merge
      k:=length(map) shr 1;
      for i:=0 to k-1 do begin
        j0:=length(map[i]);
        setLength(map[i],length(map[i])+length(map[i+k]));
        for j1:=0 to length(map[i+k])-1 do map[i][j1+j0]:=map[i+k][j1];
        setLength(map[i+k],0);
      end;
      setLength(map,k);
      hashMask:=k-1;
    end else if grow then begin
      //split
      oldLen:=length(map);
      setLength(map,oldLen+oldLen);
      hashMask:=(oldLen+oldLen)-1;
      for i:=0 to oldLen-1 do begin
        j0:=0;
        setLength(map[i+oldLen],length(map[i])); j1:=0;
        for k:=0 to length(map[i])-1 do begin
          if (hashMask and map[i][k].index)=i
            then begin map[i       ][j0]:=map[i][k]; inc(j0); end
            else begin map[i+oldLen][j1]:=map[i][k]; inc(j1); end;
        end;
        setLength(map[i],j0);
        setLength(map[i+oldLen],j1);
      end;
    end;
  end;

FUNCTION G_sparseArray.size:longint;
  begin result:=entryCount;end;

FUNCTION G_sparseArray.getIndex(CONST iterator:longint):longint;
  VAR i,k:longint;
  begin
    k:=iterator;
    i:=0;
    while (i<length(map)) and (k>=length(map[i])) do begin
      dec(k,length(map[i]));
      inc(i);
    end;
    if (i<length(map)) then result:=map[i,k].index
                       else result:=-1;
  end;

FUNCTION G_sparseArray.getEntry(CONST iterator:longint):ENTRY_TYPE;
  VAR i,k:longint;
  begin
    k:=iterator;
    i:=0;
    while (i<length(map)) and (k>=length(map[i])) do begin
      dec(k,length(map[i]));
      inc(i);
    end;
    if (i<length(map)) then result:=map[i,k].value
                       else result:=map[0,0].value;
  end;

PROCEDURE G_stringKeyMap.rehash(grow: boolean);
  VAR i,i0,j,k,c0,c1,newMask:longint;
      temp:array of KEY_VALUE_PAIR;
  begin
    if grow then begin
      i0:=length(bucket);
      setLength(bucket,i0+i0);
      newMask:=i0+i0-1;
      for i:=0 to i0-1 do begin
        temp:=bucket[i];
        setLength(bucket[i+i0],length(bucket[i]));
        c0:=0;
        c1:=0;
        for j:=0 to length(temp)-1 do begin
          k:=temp[j].hash and newMask;
          if k=i then begin
            bucket[i][c0]:=temp[j];
            inc(c0);
          end else begin
            bucket[k][c1]:=temp[j];
            inc(c1);
          end;
        end;
        setLength(bucket[i   ],c0);
        setLength(bucket[i+i0],c1);
      end;
      bitMask:=newMask;
    end else begin
      i0:=length(bucket) shr 1;
      newMask:=i0-1;
      for i:=0 to i0-1 do
      for j:=0 to length(bucket[i0+i])-1 do begin
        setLength(bucket[i],length(bucket[i])+1);
        bucket[i][length(bucket[i])-1]:=bucket[i0+i][j];
      end;
    end;
  end;

CONSTRUCTOR G_stringKeyMap.create(rebalanceFactor: double);
  begin
    system.initCriticalSection(cs);
    rebalanceFac:=rebalanceFactor;
    setLength(bucket,1);
    setLength(bucket[0],0);
    bitMask:=0;
    entryCount:=0;
  end;

PROCEDURE G_stringKeyMap.clear;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(bucket)-1 do setLength(bucket[i],0);
    setLength(bucket,1);
    bitMask:=0;
    entryCount:=0;
    system.leaveCriticalSection(cs);
  end;

CONSTRUCTOR G_stringKeyMap.create;
  begin
    create(4);
  end;

DESTRUCTOR G_stringKeyMap.destroy;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(bucket)-1 do setLength(bucket[i],0);
    setLength(bucket,0);
    system.leaveCriticalSection(cs);
    system.doneCriticalSection(cs);
  end;

FUNCTION G_stringKeyMap.containsKey(CONST key: ansistring; OUT value: VALUE_TYPE): boolean;
  VAR i,j:longint;
  begin
    system.enterCriticalSection(cs);
    i:=hashOfAnsiString(key) and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if (j<length(bucket[i])) then begin
      value:=bucket[i][j].value;
      result:=true;
    end else result:=false;
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_stringKeyMap.containsKey(CONST key:ansistring):boolean;
  VAR i,j:longint;
  begin
    system.enterCriticalSection(cs);
    i:=hashOfAnsiString(key) and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    result:=(j<length(bucket[i]));
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_stringKeyMap.get(CONST key: ansistring): VALUE_TYPE;
  begin
    containsKey(key,result);
  end;

PROCEDURE G_stringKeyMap.put(CONST key: ansistring; CONST value: VALUE_TYPE);
  VAR i,j,h:longint;
  begin
    system.enterCriticalSection(cs);
    h:=hashOfAnsiString(key);
    i:=h and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if j>=length(bucket[i]) then begin
      setLength(bucket[i],j+1);
      bucket[i][j].key:=key;
      bucket[i][j].hash:=h;
      bucket[i][j].value:=value;
      inc(entryCount);
      if entryCount>length(bucket)*rebalanceFac then rehash(true);
    end else begin
      bucket[i][j].value:=value;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE G_stringKeyMap.dropKey(CONST key: ansistring);
  VAR i,j:longint;
  begin
    system.enterCriticalSection(cs);
    i:=hashOfAnsiString(key) and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if j<length(bucket[i]) then begin
      while j<length(bucket[i])-1 do begin
        bucket[i][j]:=bucket[i][j+1];
        inc(j);
      end;
      setLength(bucket[i],length(bucket[i])-1);
      dec(entryCount);
      if entryCount<0.4*length(bucket)*rebalanceFac then rehash(false);
    end;
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_stringKeyMap.dropAny: VALUE_TYPE;
  VAR i,j:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(bucket)-1 do begin
      j:=length(bucket[i]);
      if j>0 then begin
        dec(j);
        result:=bucket[i][j].value;
        setLength(bucket[i],j);
        dec(entryCount);
        system.leaveCriticalSection(cs);
        exit(result);
      end;
    end;
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_stringKeyMap.keySet: T_arrayOfString;
  VAR k,i,j:longint;
  begin
    system.enterCriticalSection(cs);
    setLength(result,entryCount);
    k:=0;
    for i:=0 to length(bucket)-1 do
    for j:=0 to length(bucket[i])-1 do begin
      result[k]:=bucket[i][j].key;
      inc(k);
    end;
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_stringKeyMap.valueSet: VALUE_TYPE_ARRAY;
  VAR k,i,j:longint;
  begin
    system.enterCriticalSection(cs);
    setLength(result,entryCount);
    k:=0;
    for i:=0 to length(bucket)-1 do
    for j:=0 to length(bucket[i])-1 do begin
      result[k]:=bucket[i][j].value;
      inc(k);
    end;
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_stringKeyMap.entrySet: KEY_VALUE_LIST;
  VAR k,i,j:longint;
  begin
    system.enterCriticalSection(cs);
    setLength(result,entryCount);
    k:=0;
    for i:=0 to length(bucket)-1 do
    for j:=0 to length(bucket[i])-1 do begin
      result[k]:=bucket[i][j];
      inc(k);
    end;
    system.leaveCriticalSection(cs);
  end;

FUNCTION G_stringKeyMap.size: longint;
  begin
    system.enterCriticalSection(cs);
    result:=entryCount;
    system.leaveCriticalSection(cs);
  end;

end.

