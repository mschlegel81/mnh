{$ifdef include_interface}
  T_literalRecycler=object
    PROCEDURE freeMemory(CONST hard:boolean); virtual; abstract;
    PROCEDURE disposeLiteral(VAR l:P_literal); virtual; abstract;
    PROCEDURE disposeLiterals(VAR l: T_arrayOfLiteral);
    PROCEDURE disposeEntries(VAR l: T_arrayOfKeyValuePair);
    FUNCTION newBigIntLiteral(value: T_bigInt): P_abstractIntLiteral; virtual; abstract;
    FUNCTION newIntLiteral   (CONST value: int64         ): P_abstractIntLiteral; virtual; abstract;
    FUNCTION newIntLiteral   (CONST value: T_bigInt      ): P_abstractIntLiteral; virtual; abstract;
    FUNCTION newStringLiteral(CONST value: ansistring; CONST enforceNewString:boolean=false): P_stringLiteral; virtual; abstract;
    FUNCTION newRealLiteral  (CONST value: T_myFloat     ): P_realLiteral; virtual; abstract;
    FUNCTION newListLiteral  (CONST initialSize:longint=2): P_listLiteral; virtual; abstract;
    FUNCTION listLiteralOf   (CONST a:P_literal;
                              CONST b:P_literal=nil)      : P_listLiteral;
    FUNCTION newSetLiteral(CONST expectedSize:longint)    : P_setLiteral; virtual; abstract;
  end;

  T_globalLiteralRecycler=object(T_literalRecycler)
    private
      smallIntLiterals:record cs:TRTLCriticalSection; dat:array of P_smallIntLiteral; fill:longint; end;
      bigIntLiterals  :record cs:TRTLCriticalSection; dat:array of P_bigIntLiteral;   fill:longint; end;
      realLiterals    :record cs:TRTLCriticalSection; dat:array of P_realLiteral;     fill:longint; end;
      stringLiterals  :record cs:TRTLCriticalSection; dat:array of P_stringLiteral;   fill:longint; end;
      listLiterals    :record cs:TRTLCriticalSection; dat:array of P_listLiteral;     fill:longint; end;
      setLiterals     :record cs:TRTLCriticalSection; dat:array of P_setLiteral;      fill:longint; end;
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
    public
      PROCEDURE freeMemory(CONST hard:boolean); virtual;
      PROCEDURE disposeLiteral(VAR l:P_literal); virtual;
      FUNCTION newBigIntLiteral(value: T_bigInt): P_abstractIntLiteral; virtual;
      FUNCTION newIntLiteral   (CONST value: int64         ): P_abstractIntLiteral; virtual;
      FUNCTION newIntLiteral   (CONST value: T_bigInt      ): P_abstractIntLiteral; virtual;
      FUNCTION newStringLiteral(CONST value: ansistring; CONST enforceNewString:boolean=false): P_stringLiteral; virtual;
      FUNCTION newRealLiteral  (CONST value: T_myFloat     ): P_realLiteral; virtual;
      FUNCTION newListLiteral  (CONST initialSize:longint=2): P_listLiteral; virtual;
      FUNCTION newSetLiteral(CONST expectedSize:longint)    : P_setLiteral; virtual;
    end;

  T_localLiteralRecycler=object(T_literalRecycler)
    private
      smallIntLiterals:record dat:array [0..2047] of P_smallIntLiteral; fill:longint; end;
      bigIntLiterals  :record dat:array [0..511 ] of P_bigIntLiteral;   fill:longint; end;
      realLiterals    :record dat:array [0..2047] of P_realLiteral;     fill:longint; end;
      stringLiterals  :record dat:array [0..511 ] of P_stringLiteral;   fill:longint; end;
      listLiterals    :record dat:array [0..1023] of P_listLiteral;     fill:longint; end;
      setLiterals     :record dat:array [0..1023] of P_setLiteral;      fill:longint; end;
    public
      PROCEDURE freeMemory(CONST hard:boolean); virtual;
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      PROCEDURE disposeLiteral(VAR l:P_literal); virtual;
      FUNCTION newBigIntLiteral(value: T_bigInt): P_abstractIntLiteral; virtual;
      FUNCTION newIntLiteral   (CONST value: int64         ): P_abstractIntLiteral; virtual;
      FUNCTION newIntLiteral   (CONST value: T_bigInt      ): P_abstractIntLiteral; virtual;
      FUNCTION newStringLiteral(CONST value: ansistring; CONST enforceNewString:boolean=false): P_stringLiteral; virtual;
      FUNCTION newRealLiteral  (CONST value: T_myFloat     ): P_realLiteral; virtual;
      FUNCTION newListLiteral  (CONST initialSize:longint=2): P_listLiteral; virtual;
      FUNCTION newSetLiteral(CONST expectedSize:longint)    : P_setLiteral; virtual;
      PROCEDURE exchangeLiterals(CONST pushOnly:boolean=false);
    end;

FUNCTION newBoolLiteral  (CONST value: boolean       ): P_boolLiteral;
FUNCTION newVoidLiteral                               : P_voidLiteral;
FUNCTION newMapLiteral(CONST expectedSize:longint)    : P_mapLiteral;
VAR emptyStringSingleton: T_stringLiteral;
CONST maxSingletonInt=1000;
VAR boolLit       : array[false..true] of T_boolLiteral;
    charLit       : array[#0..#255] of T_stringLiteral;
    nanLit,
    infLit,
    negInfLit     : T_realLiteral;
    intLit : array[-100..maxSingletonInt] of T_smallIntLiteral;
    voidLit: T_voidLiteral;

    globalLiteralRecycler:T_globalLiteralRecycler;
{$endif}
{$ifdef include_implementation}
FUNCTION newBoolLiteral(CONST value: boolean)      : P_boolLiteral; begin result:=P_boolLiteral(boolLit[value].rereferenced); end;
FUNCTION newVoidLiteral                            : P_voidLiteral; begin result:=P_voidLiteral(voidLit       .rereferenced); end;
FUNCTION newMapLiteral (CONST expectedSize: longint): P_mapLiteral; begin new(result,create(expectedSize)); end;

PROCEDURE T_globalLiteralRecycler.freeMemory(CONST hard:boolean);
  VAR threshold:longint;
  begin
    with smallIntLiterals do begin enterCriticalSection(cs); if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); leaveCriticalSection(cs); end;
    with bigIntLiterals   do begin enterCriticalSection(cs); if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); leaveCriticalSection(cs); end;
    with realLiterals     do begin enterCriticalSection(cs); if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); leaveCriticalSection(cs); end;
    with stringLiterals   do begin enterCriticalSection(cs); if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); leaveCriticalSection(cs); end;
    with listLiterals     do begin enterCriticalSection(cs); if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); leaveCriticalSection(cs); end;
    with setLiterals      do begin enterCriticalSection(cs); if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); leaveCriticalSection(cs); end;
  end;

PROCEDURE T_localLiteralRecycler.freeMemory(CONST hard:boolean);
  VAR threshold:longint;
  begin
    if not(hard) then exchangeLiterals(true);
    with smallIntLiterals do begin if hard then threshold:=0 else threshold:=length(dat) shr 1; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; end;
    with bigIntLiterals   do begin if hard then threshold:=0 else threshold:=length(dat) shr 1; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; end;
    with realLiterals     do begin if hard then threshold:=0 else threshold:=length(dat) shr 1; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; end;
    with stringLiterals   do begin if hard then threshold:=0 else threshold:=length(dat) shr 1; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; end;
    with listLiterals     do begin if hard then threshold:=0 else threshold:=length(dat) shr 1; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; end;
    with setLiterals      do begin if hard then threshold:=0 else threshold:=length(dat) shr 1; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; end;
  end;

CONSTRUCTOR T_globalLiteralRecycler.create;
  begin
    with smallIntLiterals do begin fill:=0; setLength(dat,0); initCriticalSection(cs); end;
    with bigIntLiterals   do begin fill:=0; setLength(dat,0); initCriticalSection(cs); end;
    with realLiterals     do begin fill:=0; setLength(dat,0); initCriticalSection(cs); end;
    with stringLiterals   do begin fill:=0; setLength(dat,0); initCriticalSection(cs); end;
    with listLiterals     do begin fill:=0; setLength(dat,0); initCriticalSection(cs); end;
    with setLiterals      do begin fill:=0; setLength(dat,0); initCriticalSection(cs); end;
  end;

CONSTRUCTOR T_localLiteralRecycler.create;
  begin
    smallIntLiterals.fill:=0;
    bigIntLiterals  .fill:=0;
    realLiterals    .fill:=0;
    stringLiterals  .fill:=0;
    listLiterals    .fill:=0;
    setLiterals     .fill:=0;
  end;

DESTRUCTOR T_globalLiteralRecycler.destroy;
  begin
    with smallIntLiterals do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); doneCriticalSection(cs); end;
    with bigIntLiterals   do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); doneCriticalSection(cs); end;
    with realLiterals     do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); doneCriticalSection(cs); end;
    with stringLiterals   do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); doneCriticalSection(cs); end;
    with listLiterals     do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); doneCriticalSection(cs); end;
    with setLiterals      do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); doneCriticalSection(cs); end;
  end;

DESTRUCTOR T_localLiteralRecycler.destroy;
  begin
    with smallIntLiterals do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); end;
    with bigIntLiterals   do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); end;
    with realLiterals     do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); end;
    with stringLiterals   do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); end;
    with listLiterals     do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); end;
    with setLiterals      do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); end;
  end;

PROCEDURE T_globalLiteralRecycler.disposeLiteral(VAR l: P_literal);
  begin
    if l^.unreference<=0 then
    case l^.literalType of
      lt_smallint:
        with smallIntLiterals do if tryEnterCriticalsection(cs)=0 then dispose(l,destroy) else try
          if (fill>=length(dat)) then begin
            if length(dat)=0
            then setLength(dat,32)
            else setLength(dat,length(dat)*2);
          end;
          dat[fill]:=P_smallIntLiteral(l);
          inc(fill);
        finally
          leaveCriticalSection(cs);
        end;
      lt_bigint:
        with bigIntLiterals do begin
          l^.cleanup(@self);
          if tryEnterCriticalsection(cs)=0 then dispose(l,destroy) else try
            if (fill>=length(dat)) then begin
              if length(dat)=0
              then setLength(dat,32)
              else setLength(dat,length(dat)*2);
            end;
            dat[fill]:=P_bigIntLiteral(l);
            inc(fill);
          finally
            leaveCriticalSection(cs);
          end;
        end;
      lt_real:
        with realLiterals do if tryEnterCriticalsection(cs)=0 then dispose(l,destroy) else try
          if (fill>=length(dat)) then begin
            if length(dat)=0
            then setLength(dat,32)
            else setLength(dat,length(dat)*2);
          end;
          dat[fill]:=P_realLiteral(l);
          inc(fill);
        finally
          leaveCriticalSection(cs);
        end;
      lt_string:
        with stringLiterals do begin
          l^.cleanup(@self);
          if tryEnterCriticalsection(cs)=0 then dispose(l,destroy) else try
            if (fill>=length(dat)) then begin
              if length(dat)=0
              then setLength(dat,32)
              else setLength(dat,length(dat)*2);
            end;
            dat[fill]:=P_stringLiteral(l);
            inc(fill);
          finally
            leaveCriticalSection(cs);
          end;
        end;
      lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_emptyList:
        with listLiterals do begin
          l^.cleanup(@self);
          if tryEnterCriticalsection(cs)=0 then dispose(l,destroy) else try
            if (fill>=length(dat)) then begin
              if length(dat)=0
              then setLength(dat,32)
              else setLength(dat,length(dat)*2);
            end;
            dat[fill]:=P_listLiteral(l);
            inc(fill);
          finally
            leaveCriticalSection(cs);
          end;
        end;
      lt_set, lt_booleanSet, lt_intSet, lt_realSet, lt_numSet, lt_stringSet, lt_emptySet:
        with setLiterals do begin
          l^.cleanup(@self);
          if tryEnterCriticalsection(cs)=0 then dispose(l,destroy) else try
            if (fill>=length(dat)) then begin
              if length(dat)=0
              then setLength(dat,32)
              else setLength(dat,length(dat)*2);
            end;
            dat[fill]:=P_setLiteral(l);
            inc(fill);
          finally
            leaveCriticalSection(cs);
          end;
        end;
      else begin l^.cleanup(@self); dispose(l,destroy); end;
    end;
    l:=nil;
  end;

PROCEDURE T_localLiteralRecycler.disposeLiteral(VAR l: P_literal);
  begin
    if l^.unreference<=0 then
    case l^.literalType of
      lt_smallint:
        with smallIntLiterals do begin
          if (fill>=length(dat)) then dispose(l,destroy) else begin
            dat[fill]:=P_smallIntLiteral(l);
            inc(fill);
          end;
        end;
      lt_bigint:
        with bigIntLiterals do begin
          l^.cleanup(@self);
          if (fill>=length(dat)) then dispose(l,destroy) else begin
            dat[fill]:=P_bigIntLiteral(l);
            inc(fill);
          end;
        end;
      lt_real:
        with realLiterals do begin
          if (fill>=length(dat)) then dispose(l,destroy) else begin
            dat[fill]:=P_realLiteral(l);
            inc(fill);
          end;
        end;
      lt_string:
        with stringLiterals do begin
          l^.cleanup(@self);
          if (fill>=length(dat)) then dispose(l,destroy) else begin
            dat[fill]:=P_stringLiteral(l);
            inc(fill);
          end;
        end;
      lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_emptyList:
        with listLiterals do begin
          l^.cleanup(@self);
          if (fill>=length(dat)) then dispose(l,destroy) else begin
            dat[fill]:=P_listLiteral(l);
            inc(fill);
          end;
        end;
      lt_set, lt_booleanSet, lt_intSet, lt_realSet, lt_numSet, lt_stringSet, lt_emptySet:
        with setLiterals do begin
          l^.cleanup(@self);
          if (fill>=length(dat)) then dispose(l,destroy) else begin
            dat[fill]:=P_setLiteral(l);
            inc(fill);
          end;
        end;
      else begin l^.cleanup(@self); dispose(l,destroy); end;
    end;
    l:=nil;
  end;

PROCEDURE T_literalRecycler.disposeLiterals(VAR l: T_arrayOfLiteral);
  VAR i:longint;
  begin
    for i:=0 to length(l)-1 do disposeLiteral(l[i]);
    setLength(l,0);
  end;

PROCEDURE T_literalRecycler.disposeEntries(VAR l: T_arrayOfKeyValuePair);
  VAR pair:T_keyValuePair;
      tmp:P_literal;
  begin
    for pair in l do with pair do begin
      tmp:=key;   disposeLiteral(tmp);
      tmp:=value; disposeLiteral(tmp);
    end;
    setLength(l,0);
  end;

FUNCTION T_globalLiteralRecycler.newBigIntLiteral(value: T_bigInt): P_abstractIntLiteral;
  VAR iv:int64;
  begin
    if value.isBetween(low(intLit),high(intLit)) then begin
      iv:=value.toInt;
      value.clear;
      exit(P_smallIntLiteral(intLit[iv].rereferenced));
    end;
    with bigIntLiterals do if tryEnterCriticalsection(cs)=0 then new(P_bigIntLiteral(result),create(value)) else try
      if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        P_bigIntLiteral(result)^.val:=value;
        result^.numberOfReferences:=1;
        value.clear;
      end else begin
        new(P_bigIntLiteral(result),create(value));
      end;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_localLiteralRecycler.newBigIntLiteral(value: T_bigInt): P_abstractIntLiteral;
  VAR iv:int64;
  begin
    if value.isBetween(low(intLit),high(intLit)) then begin
      iv:=value.toInt;
      value.clear;
      exit(P_smallIntLiteral(intLit[iv].rereferenced));
    end;
    with bigIntLiterals do if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
      P_bigIntLiteral(result)^.val:=value;
      result^.numberOfReferences:=1;
      value.clear;
    end else begin
      new(P_bigIntLiteral(result),create(value));
    end;
  end;

FUNCTION T_globalLiteralRecycler.newIntLiteral(CONST value: int64): P_abstractIntLiteral;
  begin
    if (value>=low(intLit)) and (value<=high(intLit))
    then exit(P_smallIntLiteral(intLit[value].rereferenced));
    if (value>=-maxLongint) and (value<=maxLongint)
    then begin
      with smallIntLiterals do if tryEnterCriticalsection(cs)=0 then new(P_smallIntLiteral(result),create(value)) else try
        if (fill>0) then begin
          dec(fill);
          result:=dat[fill];
          P_smallIntLiteral(result)^.val:=value;
          P_smallIntLiteral(result)^.numberOfReferences:=1;
        end else begin
          new(P_smallIntLiteral(result),create(value));
        end;
      finally
        leaveCriticalSection(cs);
      end;
    end else with bigIntLiterals do if tryEnterCriticalsection(cs)=0 then new(P_bigIntLiteral(result),create(value)) else try
      if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        P_bigIntLiteral(result)^.value.fromInt(value);
        result^.numberOfReferences:=1;
      end else begin
        new(P_bigIntLiteral(result),create(value));
      end;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_localLiteralRecycler.newIntLiteral(CONST value: int64): P_abstractIntLiteral;
  begin
    if (value>=low(intLit)) and (value<=high(intLit))
    then exit(P_smallIntLiteral(intLit[value].rereferenced));
    if (value>=-maxLongint) and (value<=maxLongint)
    then begin
      with smallIntLiterals do if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        P_smallIntLiteral(result)^.val:=value;
        P_smallIntLiteral(result)^.numberOfReferences:=1;
      end else begin
        new(P_smallIntLiteral(result),create(value));
      end;
    end else with bigIntLiterals do if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
      P_bigIntLiteral(result)^.value.fromInt(value);
      result^.numberOfReferences:=1;
    end else begin
      new(P_bigIntLiteral(result),create(value));
    end;
  end;

FUNCTION T_globalLiteralRecycler.newIntLiteral(CONST value: T_bigInt): P_abstractIntLiteral;
  VAR iv:int64;
  begin
    if value.isBetween(low(intLit),high(intLit)) then begin
      iv:=value.toInt;
      value.clear;
      exit(P_smallIntLiteral(intLit[iv].rereferenced));
    end;
    if (value.canBeRepresentedAsInt32)
    then begin
      with smallIntLiterals do if tryEnterCriticalsection(cs)=0 then  new(P_smallIntLiteral(result),create(value.toInt)) else try
        if (fill>0) then begin
          dec(fill);
          result:=dat[fill];
          P_smallIntLiteral(result)^.val:=value.toInt;
          P_smallIntLiteral(result)^.numberOfReferences:=1
        end else begin
          new(P_smallIntLiteral(result),create(value.toInt));
        end;
      finally
        leaveCriticalSection(cs);
      end;
      value.clear;
    end else with bigIntLiterals do if tryEnterCriticalsection(cs)=0 then new(P_bigIntLiteral(result),create(value)) else try
      if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        P_bigIntLiteral(result)^.val:=value;
        result^.numberOfReferences:=1;
      end else begin
        new(P_bigIntLiteral(result),create(value));
      end;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_localLiteralRecycler.newIntLiteral(CONST value: T_bigInt): P_abstractIntLiteral;
  VAR iv:int64;
  begin
    if value.isBetween(low(intLit),high(intLit)) then begin
      iv:=value.toInt;
      value.clear;
      exit(P_smallIntLiteral(intLit[iv].rereferenced));
    end;
    if (value.canBeRepresentedAsInt32)
    then begin
      with smallIntLiterals do if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        P_smallIntLiteral(result)^.val:=value.toInt;
        P_smallIntLiteral(result)^.numberOfReferences:=1
      end else begin
        new(P_smallIntLiteral(result),create(value.toInt));
      end;
      value.clear;
    end else with bigIntLiterals do if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
      P_bigIntLiteral(result)^.val:=value;
      result^.numberOfReferences:=1;
    end else begin
      new(P_bigIntLiteral(result),create(value));
    end;
  end;

FUNCTION T_globalLiteralRecycler.newStringLiteral(CONST value: ansistring; CONST enforceNewString: boolean): P_stringLiteral;
  begin
    if not(enforceNewString) then begin
      if length(value)=1 then exit(P_stringLiteral(charLit[value[1]]   .rereferenced));
      if length(value)=0 then exit(P_stringLiteral(emptyStringSingleton.rereferenced));
    end;
    with stringLiterals do if tryEnterCriticalsection(cs)=0 then new(result,create(value)) else try
      if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        result^.val:=value;
        result^.numberOfReferences:=1;
        result^.enc:=se_testPending;
      end else begin
        new(result,create(value));
      end;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_localLiteralRecycler.newStringLiteral(CONST value: ansistring; CONST enforceNewString: boolean): P_stringLiteral;
  begin
    if not(enforceNewString) then begin
      if length(value)=1 then exit(P_stringLiteral(charLit[value[1]]   .rereferenced));
      if length(value)=0 then exit(P_stringLiteral(emptyStringSingleton.rereferenced));
    end;
    with stringLiterals do if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
      result^.val:=value;
      result^.numberOfReferences:=1;
      result^.enc:=se_testPending;
    end else begin
      new(result,create(value));
    end;
  end;

FUNCTION T_globalLiteralRecycler.newRealLiteral(CONST value: T_myFloat): P_realLiteral;
  begin
    with realLiterals do if tryEnterCriticalsection(cs)=0 then new(result,create(value)) else try
      if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        result^.val:=value;
        result^.numberOfReferences:=1;
      end else begin
        new(result,create(value));
      end;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_localLiteralRecycler.newRealLiteral(CONST value: T_myFloat): P_realLiteral;
  begin
    with realLiterals do if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
      result^.val:=value;
      result^.numberOfReferences:=1;
    end else begin
      new(result,create(value));
    end;
  end;

FUNCTION T_globalLiteralRecycler.newListLiteral(CONST initialSize: longint): P_listLiteral;
  begin
    with listLiterals do if tryEnterCriticalsection(cs)=0 then new(result,create(initialSize)) else try
      if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        result^.numberOfReferences:=1;
        if length(result^.dat)<initialSize then setLength(result^.dat,initialSize);
        result^.fill:=0;
      end else begin
        new(result,create(initialSize));
      end;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_localLiteralRecycler.newListLiteral(CONST initialSize: longint): P_listLiteral;
  begin
    with listLiterals do if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
      result^.numberOfReferences:=1;
      if length(result^.dat)<initialSize then setLength(result^.dat,initialSize);
      result^.fill:=0;
    end else begin
      new(result,create(initialSize));
    end;
  end;

FUNCTION T_globalLiteralRecycler.newSetLiteral(CONST expectedSize:longint)    : P_setLiteral;
  begin
    with setLiterals do if tryEnterCriticalsection(cs)=0 then new(result,create(expectedSize)) else try
      if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        result^.numberOfReferences:=1;
        result^.dat.prepareForSize(expectedSize);
      end else begin
        new(result,create(expectedSize));
      end;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_localLiteralRecycler.newSetLiteral(CONST expectedSize:longint)    : P_setLiteral;
  begin
    with setLiterals do if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
      result^.numberOfReferences:=1;
      result^.dat.prepareForSize(expectedSize);
    end else begin
      new(result,create(expectedSize));
    end;
  end;

FUNCTION T_literalRecycler.listLiteralOf(CONST a: P_literal; CONST b: P_literal): P_listLiteral;
  VAR initialSize:longint=2;
  begin
    if b=nil then dec(initialSize);
    result:=newListLiteral(initialSize);
    result^.append(@self,a,true);
    if b<>nil then result^.append(@self,b,true);
  end;

PROCEDURE T_localLiteralRecycler.exchangeLiterals(CONST pushOnly:boolean=false);

  PROCEDURE pushSmallInt;
    CONST RETAIN_COUNT=length(smallIntLiterals.dat) shr 1;
    VAR i:longint;
    begin with globalLiteralRecycler.smallIntLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        i:=fill+smallIntLiterals.fill;
        if length(dat)<i then setLength(dat,i);
        for i:=RETAIN_COUNT to smallIntLiterals.fill-1 do begin
          dat[fill]:=smallIntLiterals.dat[i];
          inc(fill);
        end;
        smallIntLiterals.fill:=RETAIN_COUNT;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pullSmallInt;
    CONST RETAIN_COUNT=length(smallIntLiterals.dat) shr 1;
    begin with globalLiteralRecycler.smallIntLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        while (fill>0) and (smallIntLiterals.fill<RETAIN_COUNT) do begin
          dec(fill);
          smallIntLiterals.dat[smallIntLiterals.fill]:=dat[fill];
          inc(smallIntLiterals.fill);
        end;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pushBigInt;
    CONST RETAIN_COUNT=length(bigIntLiterals.dat) shr 1;
    VAR i:longint;
    begin with globalLiteralRecycler.bigIntLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        i:=fill+bigIntLiterals.fill;
        if length(dat)<i then setLength(dat,i);
        for i:=RETAIN_COUNT to bigIntLiterals.fill-1 do begin
          dat[fill]:=bigIntLiterals.dat[i];
          inc(fill);
        end;
        bigIntLiterals.fill:=RETAIN_COUNT;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pullBigint;
    CONST RETAIN_COUNT=length(bigIntLiterals.dat) shr 1;
    begin with globalLiteralRecycler.bigIntLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        while (fill>0) and (bigIntLiterals.fill<RETAIN_COUNT) do begin
          dec(fill);
          bigIntLiterals.dat[bigIntLiterals.fill]:=dat[fill];
          inc(bigIntLiterals.fill);
        end;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pushReal;
    CONST RETAIN_COUNT=length(realLiterals.dat) shr 1;
    VAR i:longint;
    begin with globalLiteralRecycler.realLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        i:=fill+realLiterals.fill;
        if length(dat)<i then setLength(dat,i);
        for i:=RETAIN_COUNT to realLiterals.fill-1 do begin
          dat[fill]:=realLiterals.dat[i];
          inc(fill);
        end;
        realLiterals.fill:=RETAIN_COUNT;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pullReal;
    CONST RETAIN_COUNT=length(realLiterals.dat) shr 1;
    begin with globalLiteralRecycler.realLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        while (fill>0) and (realLiterals.fill<RETAIN_COUNT) do begin
          dec(fill);
          realLiterals.dat[realLiterals.fill]:=dat[fill];
          inc(realLiterals.fill);
        end;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pushString;
    CONST RETAIN_COUNT=length(stringLiterals.dat) shr 1;
    VAR i:longint;
    begin with globalLiteralRecycler.stringLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        i:=fill+stringLiterals.fill;
        if length(dat)<i then setLength(dat,i);
        for i:=RETAIN_COUNT to stringLiterals.fill-1 do begin
          dat[fill]:=stringLiterals.dat[i];
          inc(fill);
        end;
        stringLiterals.fill:=RETAIN_COUNT;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pullString;
    CONST RETAIN_COUNT=length(stringLiterals.dat) shr 1;
    begin with globalLiteralRecycler.stringLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        while (fill>0) and (stringLiterals.fill<RETAIN_COUNT) do begin
          dec(fill);
          stringLiterals.dat[stringLiterals.fill]:=dat[fill];
          inc(stringLiterals.fill);
        end;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pushList;
    CONST RETAIN_COUNT=length(listLiterals.dat) shr 1;
    VAR i:longint;
    begin with globalLiteralRecycler.listLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        i:=fill+listLiterals.fill;
        if length(dat)<i then setLength(dat,i);
        for i:=RETAIN_COUNT to listLiterals.fill-1 do begin
          dat[fill]:=listLiterals.dat[i];
          inc(fill);
        end;
        listLiterals.fill:=RETAIN_COUNT;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pullList;
    CONST RETAIN_COUNT=length(listLiterals.dat) shr 1;
    begin with globalLiteralRecycler.listLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        while (fill>0) and (listLiterals.fill<RETAIN_COUNT) do begin
          dec(fill);
          listLiterals.dat[listLiterals.fill]:=dat[fill];
          inc(listLiterals.fill);
        end;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pushSet;
    CONST RETAIN_COUNT=length(setLiterals.dat) shr 1;
    VAR i:longint;
    begin with globalLiteralRecycler.setLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        i:=fill+setLiterals.fill;
        if length(dat)<i then setLength(dat,i);
        for i:=RETAIN_COUNT to setLiterals.fill-1 do begin
          dat[fill]:=setLiterals.dat[i];
          inc(fill);
        end;
        setLiterals.fill:=RETAIN_COUNT;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  PROCEDURE pullSet;
    CONST RETAIN_COUNT=length(setLiterals.dat) shr 1;
    begin with globalLiteralRecycler.setLiterals do begin
      if tryEnterCriticalsection(cs)=0 then exit else
      try
        while (fill>0) and (setLiterals.fill<length(setLiterals.dat) shr 1) do begin
          dec(fill);
          setLiterals.dat[setLiterals.fill]:=dat[fill];
          inc(setLiterals.fill);
        end;
      finally
        leaveCriticalSection(cs);
      end;
    end; end;

  begin
    if (smallIntLiterals.fill<0.25*length(smallIntLiterals.dat)) and not(pushOnly) then pullSmallInt else if (smallIntLiterals.fill>0.75*length(smallIntLiterals.dat)) then pullSmallInt;
    if (bigIntLiterals  .fill<0.25*length(bigIntLiterals  .dat)) and not(pushOnly) then pullBigint   else if (bigIntLiterals  .fill>0.75*length(bigIntLiterals  .dat)) then pushBigInt;
    if (realLiterals    .fill<0.25*length(realLiterals    .dat)) and not(pushOnly) then pullReal     else if (realLiterals    .fill>0.75*length(realLiterals    .dat)) then pushReal;
    if (stringLiterals  .fill<0.25*length(stringLiterals  .dat)) and not(pushOnly) then pullString   else if (stringLiterals  .fill>0.75*length(stringLiterals  .dat)) then pushString;
    if (listLiterals    .fill<0.25*length(listLiterals    .dat)) and not(pushOnly) then pullList     else if (listLiterals    .fill>0.75*length(listLiterals    .dat)) then pushList;
    if (setLiterals     .fill<0.25*length(setLiterals     .dat)) and not(pushOnly) then pullSet      else if (setLiterals     .fill>0.75*length(setLiterals     .dat)) then pushSet;
  end;

{$endif}
{$ifdef include_initialization}
initialize(boolLit);
boolLit[false].create(false);
boolLit[true ].create(true);
voidLit.create();
emptyStringSingleton.create('');
for i:=low(intLit) to high(intLit) do intLit[i].create(i);
for i:=0 to 255 do charLit[chr(i)].create(chr(i));
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
nanLit   .create(Nan);
infLit   .create(infinity);
negInfLit.create(-infinity);
globalLiteralRecycler.create;
{$endif}
{$ifdef include_finalization}
globalLiteralRecycler.destroy;
boolLit[false].destroy;
boolLit[true ].destroy;
voidLit.destroy;
emptyStringSingleton.destroy;
for i:=low(intLit) to high(intLit) do intLit[i].destroy;
for i:=0 to 255 do charLit[chr(i)].destroy;
nanLit   .destroy;
infLit   .destroy;
negInfLit.destroy;
{$endif}
