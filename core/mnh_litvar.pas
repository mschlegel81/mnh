UNIT mnh_litVar;
{$WARN 5024 OFF}
{$Q-}
INTERFACE
{$WARN 3018 OFF}{$WARN 3019 OFF}
USES myGenerics, mnh_constants, mnh_out_adapters, sysutils, math, myStringUtil, mnh_basicTypes, typinfo, serializationUtil, Classes,LazUTF8;

TYPE
  PP_literal = ^P_literal;
  P_literal = ^T_literal;
  T_arrayOfLiteral=array of P_literal;
  T_literal = object(T_objectWithIdAndLocation)
  private
    numberOfReferences: longint;
  public
    literalType:T_literalType;
    CONSTRUCTOR init(CONST lt:T_literalType);
    DESTRUCTOR destroy; virtual;
    PROCEDURE rereference;
    FUNCTION rereferenced:P_literal;
    FUNCTION unreference: longint;
    FUNCTION getReferenceCount: longint;

    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION typeString:string; virtual;

    FUNCTION getId:T_idString; virtual;
    FUNCTION getLocation:T_tokenLocation; virtual;
  end;

  P_scalarLiteral = ^T_scalarLiteral;
  T_scalarLiteral = object(T_literal)
    FUNCTION stringForm: ansistring; virtual;
  end;

  P_voidLiteral = ^T_voidLiteral;
  T_voidLiteral = object(T_scalarLiteral)
    private
      CONSTRUCTOR create();
    public
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
  end;

  P_boolLiteral = ^T_boolLiteral;
  T_boolLiteral = object(T_scalarLiteral)
  private
    val: boolean;
    CONSTRUCTOR create(CONST value: boolean);
  public
    PROPERTY value:boolean read val;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_intLiteral = ^T_intLiteral;
  T_intLiteral = object(T_scalarLiteral)
  private
    val: int64;
    CONSTRUCTOR create(CONST value: int64);
  public
    PROPERTY value:int64 read val;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_realLiteral = ^T_realLiteral;
  T_realLiteral = object(T_scalarLiteral)
  private
    val: T_myFloat;
    CONSTRUCTOR create(CONST value: T_myFloat);
  public
    PROPERTY value:T_myFloat read val;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_stringLiteral = ^T_stringLiteral;
  T_stringLiteral = object(T_scalarLiteral)
  private
    val: ansistring;
    CONSTRUCTOR create(CONST value: ansistring);
  public
    DESTRUCTOR destroy; virtual;
    PROPERTY value:ansistring read val;
    FUNCTION softCast: P_scalarLiteral;
    FUNCTION trim: P_stringLiteral;
    FUNCTION trimLeft: P_stringLiteral;
    FUNCTION trimRight: P_stringLiteral;
    FUNCTION upper: P_stringLiteral;
    FUNCTION lower: P_stringLiteral;
    FUNCTION unbrace: P_stringLiteral;
    FUNCTION escape: P_stringLiteral;
    PROCEDURE append(CONST suffix:ansistring);
    //from T_scalarLiteral:
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_compoundLiteral  = ^T_compoundLiteral;
  P_listLiteral      = ^T_listLiteral    ;
  P_setLiteral       = ^T_setLiteral     ;
  P_mapLiteral       = ^T_mapLiteral     ;

  P_expressionLiteral = ^T_expressionLiteral;
  T_expressionLiteral = object(T_scalarLiteral)
    FUNCTION evaluate         (CONST parameters:P_listLiteral; CONST location:T_tokenLocation; CONST context:pointer):P_literal; virtual; abstract;
    FUNCTION evaluateToBoolean(CONST a,b:P_literal;            CONST location:T_tokenLocation; CONST context:pointer):boolean;   virtual; abstract;
    FUNCTION arity:longint; virtual; abstract;
    FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual; abstract;
  end;

  generic G_literalKeyMap<VALUE_TYPE>= object
    TYPE CACHE_ENTRY=record
           key:P_literal;
           keyHash:T_hashInt;
           value:VALUE_TYPE;
         end;
         KEY_VALUE_LIST=array of CACHE_ENTRY;
         MY_TYPE=specialize G_literalKeyMap<VALUE_TYPE>;
    VAR dat:array of KEY_VALUE_LIST;
        fill:longint;
    CONSTRUCTOR create();
    CONSTRUCTOR createClone(VAR map:MY_TYPE);
    DESTRUCTOR destroy;
    PROCEDURE rehash(CONST grow:boolean);
    PROCEDURE put(CONST key:P_literal; CONST value:VALUE_TYPE);
    FUNCTION putNew(CONST entry:CACHE_ENTRY; OUT previousValue:VALUE_TYPE):boolean;
    FUNCTION putNew(CONST key:P_literal; CONST value:VALUE_TYPE; OUT previousValue:VALUE_TYPE):boolean;
    FUNCTION get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
    FUNCTION drop(CONST key:P_literal):CACHE_ENTRY;
    FUNCTION keyValueList:KEY_VALUE_LIST;
    FUNCTION keySet:T_arrayOfLiteral;
  end;

  P_literalKeyBooleanValueMap=^T_literalKeyBooleanValueMap;
  T_literalKeyBooleanValueMap=specialize G_literalKeyMap<boolean>;
  P_literalKeyLiteralValueMap=^T_literalKeyLiteralValueMap;
  T_literalKeyLiteralValueMap=specialize G_literalKeyMap<P_literal>;
  P_stringKeyLiteralValueMap=^T_stringKeyLiteralValueMap;
  T_stringKeyLiteralValueMap=specialize G_stringKeyMap<P_literal>;

  T_compoundLiteral=object(T_literal)
    private
      FUNCTION getValue(CONST index:longint):P_literal; virtual; abstract;
      PROCEDURE modifyType(CONST L:P_literal); inline;
    public
    containsError:boolean;
    FUNCTION toSet :P_setLiteral;
    FUNCTION toList:P_listLiteral;
    FUNCTION toMap(CONST location:T_tokenLocation; VAR adapters:T_adapters):P_mapLiteral;
    PROPERTY value[CONST index:longint]:P_literal read getValue; default;
    FUNCTION get     (CONST accessor:P_literal):P_literal; virtual; abstract;
    FUNCTION getInner(CONST accessor:P_literal):P_literal; virtual; abstract;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION typeString:string; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION size:longint;                        virtual; abstract;
    FUNCTION contains(CONST L:P_literal):boolean; virtual; abstract;
    FUNCTION clone:P_compoundLiteral; virtual; abstract;
    FUNCTION iteratableList:T_arrayOfLiteral; virtual; abstract;
  end;

  P_collectionLiteral=^T_collectionLiteral;
  T_collectionLiteral=object(T_compoundLiteral)
    FUNCTION isKeyValueCollection:boolean; virtual; abstract;
    FUNCTION newOfSameType:P_collectionLiteral; virtual; abstract;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION appendAll   (CONST L:P_compoundLiteral                ):P_collectionLiteral; virtual;
    FUNCTION append      (CONST L:P_literal; CONST incRefs: boolean):P_collectionLiteral; virtual; abstract;
    FUNCTION appendString(CONST s:ansistring):P_collectionLiteral;
    FUNCTION appendBool  (CONST b:boolean   ):P_collectionLiteral;
    FUNCTION appendInt   (CONST i:int64     ):P_collectionLiteral;
    FUNCTION appendReal  (CONST r:T_myFloat ):P_collectionLiteral;
  end;

  T_listLiteral=object(T_collectionLiteral)
    private
      dat:T_arrayOfLiteral;
      fill:longint;
      FUNCTION getValue(CONST index:longint):P_literal; virtual;
    public
      CONSTRUCTOR create(CONST initialSize:longint);
      DESTRUCTOR destroy; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION equals(CONST other: P_literal): boolean; virtual;
      FUNCTION isKeyValuePair:boolean;
      FUNCTION isKeyValueCollection:boolean; virtual;

      FUNCTION newOfSameType:P_collectionLiteral; virtual;
      FUNCTION size:longint;        virtual;
      FUNCTION contains(CONST other:P_literal):boolean; virtual;
      FUNCTION listConstructorToString(CONST lengthLimit:longint=maxLongint):string;
      FUNCTION get     (CONST accessor:P_literal):P_literal; virtual;
      FUNCTION getInner(CONST accessor:P_literal):P_literal; virtual;
      FUNCTION appendConstructing(CONST L: P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST doRangeAppend:boolean):P_compoundLiteral;
      FUNCTION append(CONST L: P_literal; CONST incRefs: boolean):P_collectionLiteral; virtual;
      FUNCTION clone:P_compoundLiteral; virtual;
      FUNCTION iteratableList:T_arrayOfLiteral; virtual;

      PROCEDURE sort;
      PROCEDURE sortBySubIndex(CONST innerIndex:longint; CONST location:T_tokenLocation; VAR adapters: T_adapters);
      PROCEDURE customSort(CONST leqExpression: P_expressionLiteral; CONST location: T_tokenLocation; CONST context:pointer; VAR adapters: T_adapters);
      FUNCTION  sortPerm: P_listLiteral;
      PROCEDURE unique;

      FUNCTION head:    P_literal;     FUNCTION head    (CONST headSize : longint):P_listLiteral;
      FUNCTION tail:    P_listLiteral; FUNCTION tail    (CONST headSize : longint):P_listLiteral;
      FUNCTION leading: P_listLiteral; FUNCTION leading (CONST trailSize: longint):P_listLiteral;
      FUNCTION trailing:P_literal;     FUNCTION trailing(CONST trailSize: longint):P_listLiteral;
      FUNCTION transpose(CONST filler:P_literal): P_listLiteral;
    end;

  T_setLiteral=object(T_collectionLiteral)
    private
      dat:T_literalKeyBooleanValueMap;
      manifestation:P_listLiteral;
      manifestationCs:TRTLCriticalSection;
      CONSTRUCTOR create;
      FUNCTION getValue(CONST index:longint):P_literal; virtual;
      PROCEDURE manifest;
      PROCEDURE dropManifestation;
    public
      DESTRUCTOR destroy; virtual;
      FUNCTION isKeyValueCollection:boolean; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION equals(CONST other: P_literal): boolean; virtual;
      FUNCTION newOfSameType:P_collectionLiteral; virtual;
      FUNCTION size:longint;        virtual;
      FUNCTION contains(CONST other:P_literal):boolean; virtual;
      FUNCTION get     (CONST accessor:P_literal):P_literal; virtual;
      FUNCTION getInner(CONST accessor:P_literal):P_literal; virtual;
      FUNCTION append(CONST L: P_literal; CONST incRefs: boolean):P_collectionLiteral; virtual;
      FUNCTION appendAll(CONST L:P_compoundLiteral              ):P_collectionLiteral; virtual;
      FUNCTION clone:P_compoundLiteral; virtual;
      FUNCTION iteratableList:T_arrayOfLiteral; virtual;
      FUNCTION getManifestation:P_listLiteral;
    end;

  T_mapLiteral=object(T_compoundLiteral)
    private
      dat:T_literalKeyLiteralValueMap;
      manifestation:P_listLiteral;
      manifestationCs:TRTLCriticalSection;
      CONSTRUCTOR create;
      FUNCTION getValue(CONST index:longint):P_literal; virtual;
      PROCEDURE manifest;
      PROCEDURE dropManifestation;
    public
      DESTRUCTOR destroy; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION equals(CONST other: P_literal): boolean; virtual;
      FUNCTION size:longint;        virtual;
      FUNCTION contains(CONST other:P_literal):boolean; virtual;
      FUNCTION get     (CONST accessor:P_literal):P_literal; virtual;
      FUNCTION getInner(CONST accessor:P_literal):P_literal; virtual;
      FUNCTION clone:P_compoundLiteral; virtual;
      FUNCTION iteratableList:T_arrayOfLiteral; virtual;

      PROCEDURE drop(CONST L:P_scalarLiteral);
      FUNCTION put(CONST key,                  newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
      FUNCTION put(CONST key,                  newValue:ansistring                      ):P_mapLiteral;
      FUNCTION put(CONST key:ansistring; CONST newValue:int64                           ):P_mapLiteral;
      FUNCTION put(CONST key:ansistring; CONST newValue:T_myFloat                       ):P_mapLiteral;
      FUNCTION put(CONST key:ansistring; CONST newValue:boolean                         ):P_mapLiteral;
      FUNCTION put(CONST key:ansistring; CONST newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
      FUNCTION put(CONST key:P_literal;  CONST newValue:int64    ; CONST incRefs:boolean):P_mapLiteral;
      FUNCTION putAll(CONST map:P_mapLiteral):P_mapLiteral;
      FUNCTION getManifestation:P_listLiteral;
  end;

  T_subruleApplyOpCallback = FUNCTION(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation): P_expressionLiteral;

  T_format=object
    category:(fmtCat_decimal,
              fmtCat_scientific,
              fmtCat_fixedPoint,
              fmtCat_general,
              fmtCat_currency,
              fmtCat_number,
              fmtCat_string,
              fmtCat_hex);
    intFmt,realFmt,strFmt:string;

    CONSTRUCTOR create(CONST formatString:ansistring);
    PROCEDURE formatAppend(VAR txt:ansistring; CONST l:P_literal);
    DESTRUCTOR destroy;
  end;

VAR
  subruleApplyOpCallback: T_subruleApplyOpCallback;

FUNCTION exp(CONST x:double):double; inline;

PROCEDURE disposeLiteral(VAR l: P_literal); inline;
PROCEDURE disposeLiteral(VAR l: T_arrayOfLiteral); inline;
FUNCTION newBoolLiteral          (CONST value: boolean       ): P_boolLiteral;       inline;
FUNCTION newIntLiteral           (CONST value: int64         ): P_intLiteral;        inline;
FUNCTION newRealLiteral          (CONST value: T_myFloat     ): P_realLiteral;       inline;
FUNCTION newStringLiteral        (CONST value: ansistring    ): P_stringLiteral;     inline;
FUNCTION newListLiteral          (CONST initialSize:longint=0): P_listLiteral;       inline;
FUNCTION newSetLiteral                                        : P_setLiteral;        inline;
FUNCTION newMapLiteral                                        : P_mapLiteral;        inline;
FUNCTION newVoidLiteral: P_voidLiteral; inline;
FUNCTION newErrorLiteral:P_literal; inline;

FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
FUNCTION parseNumber(CONST input: ansistring; CONST offset:longint; CONST suppressOutput: boolean; OUT parsedLength: longint): P_scalarLiteral; inline;

FUNCTION messagesToLiteralForSandbox(CONST messages:T_storedMessages):P_listLiteral;

FUNCTION newLiteralFromStream(CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
PROCEDURE writeLiteralToStream(CONST L:P_literal; CONST stream:P_outputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters);

FUNCTION serialize(CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST asExpression:boolean):ansistring;
FUNCTION deserialize(CONST source:ansistring; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
FUNCTION toParameterListString(CONST list:P_listLiteral; CONST isFinalized: boolean; CONST lengthLimit:longint=maxLongint): ansistring;
FUNCTION parameterListTypeString(CONST list:P_listLiteral):string;

FUNCTION setUnion    (CONST params:P_listLiteral):P_setLiteral;
FUNCTION setIntersect(CONST params:P_listLiteral):P_setLiteral;
FUNCTION setMinus    (CONST params:P_listLiteral):P_setLiteral;
VAR emptyStringSingleton: T_stringLiteral;
IMPLEMENTATION
VAR
  errLit        : T_literal;
  boolLit       : array[false..true] of T_boolLiteral;
  intLit        : array[-100..4000] of T_intLiteral;
  charLit       : array[#0..#255] of T_stringLiteral;
  voidLit       : T_voidLiteral;

FUNCTION messagesToLiteralForSandbox(CONST messages:T_storedMessages):P_listLiteral;
  FUNCTION headByMessageType(CONST messageType:T_messageType):P_collectionLiteral;
    begin
      if C_messageTypeMeta[messageType].prefix=''
      then result:=newListLiteral(3)^.appendString(copy(getEnumName(TypeInfo(messageType),ord(messageType)),4,1000))
      else result:=newListLiteral(3)^.appendString(C_messageTypeMeta[messageType].prefix);
    end;

  VAR i:longint;
  begin
    result:=newListLiteral();
    for i:=0 to length(messages)-1 do with messages[i] do if not(C_messageTypeMeta[messageType].ignoredBySandbox) then
      result^.append(
         headByMessageType(messageType)^
        .appendString(ansistring(location))^
        .appendString(join(messageText,C_lineBreakChar)),false);
  end;

FUNCTION exp(CONST x:double):double; inline;
  begin
    {$ifdef CPU32}
    result:=system.exp(x);
    {$else}
    if      x<-745.133219101925 then result:=0
    else if x> 709.782712893375 then result:=infinity
                                else result:=system.exp(x);
    {$endif}
  end;

PROCEDURE disposeLiteral(VAR l: P_literal);
  begin
    if l^.unreference<=0 then dispose(l, destroy);
    l:=nil;
  end;

PROCEDURE disposeLiteral(VAR l: T_arrayOfLiteral); inline;
  VAR lit:P_literal;
  begin
    for lit in l do if lit^.unreference<=0 then dispose(lit,destroy);
    setLength(l,0);
  end;

PROCEDURE disposeLiteralWithoutResettingPointer(l:P_literal); inline;
  begin
    if l^.unreference<=0 then dispose(l,destroy);
  end;

FUNCTION newIntLiteral(CONST value: int64): P_intLiteral;
  begin
    if (value>=low(intLit)) and (value<=high(intLit))
    then result:=P_intLiteral(intLit[value].rereferenced)
    else new(result, create(value));
  end;

FUNCTION newStringLiteral(CONST value: ansistring): P_stringLiteral;
  begin
    if length(value)<=1 then begin
      if length(value)=1 then result:=P_stringLiteral(charLit[value[1]]   .rereferenced)
                         else result:=P_stringLiteral(emptyStringSingleton.rereferenced);
    end else new(result, create(value));
  end;

FUNCTION newRealLiteral(CONST value: T_myFloat)     : P_realLiteral;       begin new(result,create(value));       end;
FUNCTION newListLiteral(CONST initialSize:longint=0): P_listLiteral;       begin new(result,create(initialSize)); end;
FUNCTION newSetLiteral                              : P_setLiteral;        begin new(result,create);              end;
FUNCTION newMapLiteral                              : P_mapLiteral;        begin new(result,create);              end;
FUNCTION newVoidLiteral                             : P_voidLiteral;       begin result:=P_voidLiteral(voidLit       .rereferenced); end;
FUNCTION newErrorLiteral                            : P_literal;           begin result:=              errLit        .rereferenced ; end;
FUNCTION newBoolLiteral(CONST value: boolean)       : P_boolLiteral;       begin result:=P_boolLiteral(boolLit[value].rereferenced); end;

FUNCTION myFloatToStr(CONST x: T_myFloat): string;
  begin
    result:=floatToStr(x);
    if (pos('E', uppercase(result))<=0) and //occurs in exponents
       (pos('N', uppercase(result))<=0) and //occurs in "Nan or Inf"
       (pos('.', result)<=0) then
       result:=result+'.0';
  end;

FUNCTION parseNumber(CONST input: ansistring; CONST offset:longint; CONST suppressOutput: boolean; OUT parsedLength: longint): P_scalarLiteral;
  VAR i: longint;
  begin
    result:=nil;
    parsedLength:=0;
    if (length(input)>=offset) and (input [offset] in ['0'..'9', '-', '+']) then begin
      i:=offset;
      while (i<length(input)) and (input [i+1] in ['0'..'9']) do inc(i);
      parsedLength:=i+1-offset;
      //Only digits on indexes [1..i]; accept decimal point and following digts
      if (i<length(input)) and (input [i+1] = '.') then begin
        inc(i);
        if (i<length(input)) and (input [i+1] = '.') then dec(i);
      end;
      while (i<length(input)) and (input [i+1] in ['0'..'9']) do  inc(i);
      //Accept exponent marker and following exponent
      if (i<length(input)) and (input [i+1] in ['e', 'E']) then begin
        inc(i);
        if (i<length(input)) and (input [i+1] in ['+', '-']) then inc(i);
      end;
      while (i<length(input)) and (input [i+1] in ['0'..'9']) do inc(i);
      if i+1-offset>parsedLength then begin
        parsedLength:=i+1-offset;
        if suppressOutput then exit(nil);
        result:=newRealLiteral(strToFloatDef(copy(input, offset, parsedLength), Nan));
      end else begin
        if suppressOutput then exit(nil);
        result:=newIntLiteral(StrToInt64Def(copy(input, offset, parsedLength), 0));
      end;
    end;
  end;

//=====================================================================================================================

CONSTRUCTOR G_literalKeyMap.create();
  VAR i:longint;
  begin
    setLength(dat,1);
    for i:=0 to length(dat)-1 do setLength(dat[i],0);
    fill:=0;
  end;

CONSTRUCTOR G_literalKeyMap.createClone(VAR map:MY_TYPE);
  VAR i,j:longint;
  begin
    fill:=0;
    setLength(dat,length(map.dat));
    for i:=0 to length(dat)-1 do begin
      setLength(dat[i],length(map.dat[i]));
      inc(fill,length(dat[i]));
      for j:=0 to length(dat[i])-1 do dat[i,j]:=map.dat[i,j];
    end;
  end;

DESTRUCTOR G_literalKeyMap.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do setLength(dat[i],0);
    setLength(dat,0);
  end;

PROCEDURE G_literalKeyMap.rehash(CONST grow:boolean);
  VAR i,i0,j,k,c0,c1:longint;
      temp:KEY_VALUE_LIST;
  begin
    if grow then begin
      i0:=length(dat);
      setLength(dat,i0+i0);
      for i:=0 to i0-1 do begin
        temp:=dat[i];
        setLength(dat[i+i0],length(dat[i]));
        c0:=0;
        c1:=0;
        for j:=0 to length(temp)-1 do begin
          k:=temp[j].keyHash and (length(dat)-1);
          if k=i then begin
            dat[i][c0]:=temp[j];
            inc(c0);
          end else begin
            dat[k][c1]:=temp[j];
            inc(c1);
          end;
        end;
        setLength(dat[i   ],c0);
        setLength(dat[i+i0],c1);
        setLength(temp,0);
      end;
    end else if length(dat)>1 then begin
      i0:=length(dat) shr 1;
      for i:=0 to i0-1 do
      for j:=0 to length(dat[i0+i])-1 do begin
        setLength(dat[i],length(dat[i])+1);
        dat[i][length(dat[i])-1]:=dat[i0+i][j];
      end;
      setLength(dat,i0);
    end;
  end;

PROCEDURE G_literalKeyMap.put(CONST key:P_literal; CONST value:VALUE_TYPE);
  VAR hash:T_hashInt;
      binIdx:longint;
      j:longint;
  begin
    hash:=key^.hash;
    binIdx:=hash and (length(dat)-1);
    j:=0;
    while (j<length(dat[binIdx])) and not(dat[binIdx,j].key^.equals(key)) do inc(j);
    if j>=length(dat[binIdx]) then begin
      setLength(dat[binIdx],j+1);
      dat[binIdx,j].key:=key;
      dat[binIdx,j].keyHash:=hash;
      inc(fill);
    end;
    dat[binIdx,j].value:=value;
    if fill>length(dat)*4 then rehash(true);
  end;

FUNCTION G_literalKeyMap.putNew(CONST entry:CACHE_ENTRY; OUT previousValue:VALUE_TYPE):boolean;
  VAR binIdx:longint;
      j:longint;
  begin
    initialize(previousValue);
    binIdx:=entry.keyHash and (length(dat)-1);
    j:=0;
    while (j<length(dat[binIdx])) and not(dat[binIdx,j].key^.equals(entry.key)) do inc(j);
    if j>=length(dat[binIdx]) then begin
      setLength(dat[binIdx],j+1);
      dat[binIdx,j].key:=entry.key;
      dat[binIdx,j].keyHash:=entry.keyHash;
      result:=true;
      inc(fill);
    end else begin
      result:=false;
      previousValue:=dat[binIdx,j].value;
    end;
    dat[binIdx,j].value:=entry.value;
    if fill>length(dat)*4 then rehash(true);
  end;

FUNCTION G_literalKeyMap.putNew(CONST key:P_literal; CONST value:VALUE_TYPE; OUT previousValue:VALUE_TYPE):boolean;
  VAR hash:T_hashInt;
      binIdx:longint;
      j:longint;
  begin
    initialize(previousValue);
    hash:=key^.hash;
    binIdx:=hash and (length(dat)-1);
    j:=0;
    while (j<length(dat[binIdx])) and not(dat[binIdx,j].key^.equals(key)) do inc(j);
    if j>=length(dat[binIdx]) then begin
      setLength(dat[binIdx],j+1);
      dat[binIdx,j].key:=key;
      dat[binIdx,j].keyHash:=hash;
      result:=true;
      inc(fill);
    end else begin
      result:=false;
      previousValue:=dat[binIdx,j].value;
    end;
    dat[binIdx,j].value:=value;
    if fill>length(dat)*4 then rehash(true);
  end;

FUNCTION G_literalKeyMap.get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
  VAR hash:T_hashInt;
      binIdx:longint;
      j:longint;
  begin
    hash:=key^.hash;
    binIdx:=hash and (length(dat)-1);
    result:=fallbackIfNotFound;
    for j:=0 to length(dat[binIdx])-1 do if (dat[binIdx,j].keyHash=hash) and (dat[binIdx,j].key^.equals(key)) then exit(dat[binIdx,j].value);
  end;

FUNCTION G_literalKeyMap.drop(CONST key:P_literal):CACHE_ENTRY;
  VAR hash:T_hashInt;
      binIdx:longint;
      j,i:longint;
  begin
    result.key:=nil;
    hash:=key^.hash;
    binIdx:=hash and (length(dat)-1);
    for j:=0 to length(dat[binIdx])-1 do if (dat[binIdx,j].keyHash=hash) and dat[binIdx,j].key^.equals(key) then begin
      result:=dat[binIdx,j];
      i:=length(dat[binIdx])-1;
      if (j<i) then dat[binIdx,j]:=dat[binIdx,i];
      setLength(dat[binIdx],i);
      dec(fill);
      if fill<length(dat)*3 then rehash(false);
      exit(result);
    end;
  end;

FUNCTION G_literalKeyMap.keyValueList:KEY_VALUE_LIST;
  VAR i,j,k:longint;
  begin
    setLength(result,fill);
    k:=0;
    for i:=0 to length(dat)-1 do for j:=0 to length(dat[i])-1 do begin
      if k>=length(result) then setLength(result,k+1);
      result[k]:=dat[i,j];
      inc(k);
    end;
    if k<length(result) then setLength(result,k);
  end;

FUNCTION G_literalKeyMap.keySet:T_arrayOfLiteral;
  VAR i,j,k:longint;
  begin
    setLength(result,fill);
    k:=0;
    for i:=0 to length(dat)-1 do for j:=0 to length(dat[i])-1 do begin
      try
        result[k]:=dat[i,j].key;
      except
        raise Exception.create('Trying to set result ['+intToStr(k)+'] in list of length '+intToStr(length(result)));
      end;
      inc(k);
    end;
  end;
//=====================================================================================================================
CONSTRUCTOR T_literal.init(CONST lt:T_literalType); begin literalType:=lt; numberOfReferences:=1; end;

PROCEDURE T_literal.rereference;
  begin
    interLockedIncrement(numberOfReferences);
  end;

FUNCTION T_literal.rereferenced:P_literal;
  begin
    interLockedIncrement(numberOfReferences);
    result:=@self;
  end;

FUNCTION T_literal.unreference: longint;
  begin
    interlockedDecrement(numberOfReferences);
    result:=numberOfReferences;
  end;

FUNCTION T_literal.getReferenceCount: longint;
  begin
    result:=numberOfReferences;
  end;

FUNCTION T_literal.getId:T_idString;            begin result:=''; end;
FUNCTION T_literal.getLocation:T_tokenLocation; begin result.package:=nil; result.column:=-1; result.line:=-1; end;
//CONSTRUCTORS:=================================================================
CONSTRUCTOR T_voidLiteral.create();                              begin inherited init(lt_void);                   end;
CONSTRUCTOR T_boolLiteral      .create(CONST value: boolean);    begin inherited init(lt_boolean);    val:=value; end;
CONSTRUCTOR T_intLiteral       .create(CONST value: int64);      begin inherited init(lt_int);        val:=value; end;
CONSTRUCTOR T_realLiteral      .create(CONST value: T_myFloat);  begin inherited init(lt_real);       val:=value; end;
CONSTRUCTOR T_stringLiteral    .create(CONST value: ansistring); begin inherited init(lt_string);     val:=value; end;
CONSTRUCTOR T_listLiteral.create(CONST initialSize: longint);
  begin
    inherited init(lt_emptyList);
    setLength(dat, initialSize);
    fill:=0;
  end;

CONSTRUCTOR T_setLiteral.create;
  begin
    inherited init(lt_emptySet);
    dat.create();
    system.initCriticalSection(manifestationCs);
    manifestation:=nil;
  end;

CONSTRUCTOR T_mapLiteral.create;
  begin
    inherited init(lt_emptyMap);
    dat.create();
    system.initCriticalSection(manifestationCs);
    manifestation:=nil;
  end;
//=================================================================:CONSTRUCTORS
//DESTRUCTORS:==================================================================
DESTRUCTOR T_literal.destroy; begin end;
DESTRUCTOR T_stringLiteral.destroy; begin val:=''; end;
DESTRUCTOR T_listLiteral.destroy;
  VAR i:longint;
  begin
    for i:=0 to fill-1 do disposeLiteral(dat[i]);
    setLength(dat,0);
    fill:=0;
  end;

DESTRUCTOR T_setLiteral.destroy;
  VAR entries:T_arrayOfLiteral;
      i:longint;
  begin
    dropManifestation;
    entries:=dat.keySet;
    for i:=0 to length(entries)-1 do disposeLiteral(entries[i]);
    dat.destroy;
    system.doneCriticalSection(manifestationCs);
  end;

DESTRUCTOR T_mapLiteral.destroy;
  VAR entries:T_literalKeyLiteralValueMap.KEY_VALUE_LIST;
      i:longint;
  begin
    dropManifestation;
    entries:=dat.keyValueList;
    for i:=0 to length(entries)-1 do begin
      disposeLiteral(entries[i].key);
      disposeLiteral(entries[i].value);
    end;
    dat.destroy;
    system.doneCriticalSection(manifestationCs);
  end;
//==================================================================:DESTRUCTORS
FUNCTION T_listLiteral.getValue(CONST index: longint): P_literal;
  begin
    result:=dat[index];
  end;

FUNCTION T_setLiteral.getValue(CONST index: longint): P_literal;
  begin
    enterCriticalSection(manifestationCs);
    manifest;
    result:=manifestation^.dat[index];
    leaveCriticalSection(manifestationCs);
  end;

FUNCTION T_mapLiteral.getValue(CONST index: longint): P_literal;
  begin
    enterCriticalSection(manifestationCs);
    manifest;
    result:=manifestation^.dat[index];
    leaveCriticalSection(manifestationCs);
  end;

PROCEDURE T_setLiteral.manifest;
  VAR L:P_literal;
  begin
    enterCriticalSection(manifestationCs);
    if manifestation=nil then begin
      manifestation:=newListLiteral(size);
      for l in dat.keySet do manifestation^.append(l,true);
      manifestation^.sort;
    end;
    leaveCriticalSection(manifestationCs);
  end;

PROCEDURE T_mapLiteral.manifest;
  VAR e:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    enterCriticalSection(manifestationCs);
    if manifestation=nil then begin
      manifestation:=newListLiteral(size);
      for e in dat.keyValueList do manifestation^.append(newListLiteral(2)^.append(e.key,true)^.append(e.value,true),false);
      manifestation^.sort;
    end;
    leaveCriticalSection(manifestationCs);
  end;

PROCEDURE T_setLiteral.dropManifestation;
  begin
    enterCriticalSection(manifestationCs);
    if manifestation<>nil then disposeLiteral(manifestation);
    leaveCriticalSection(manifestationCs);
  end;

PROCEDURE T_mapLiteral.dropManifestation;
  begin
    enterCriticalSection(manifestationCs);
    if manifestation<>nil then disposeLiteral(manifestation);
    leaveCriticalSection(manifestationCs);
  end;

FUNCTION T_setLiteral.getManifestation:P_listLiteral;
  begin
    enterCriticalSection(manifestationCs);
    manifest;
    result:=manifestation;
    leaveCriticalSection(manifestationCs);
  end;

FUNCTION T_mapLiteral.getManifestation:P_listLiteral;
  begin
    enterCriticalSection(manifestationCs);
    manifest;
    result:=manifestation;
    leaveCriticalSection(manifestationCs);
  end;

FUNCTION T_collectionLiteral.appendString(CONST s: ansistring): P_collectionLiteral; begin result:=P_collectionLiteral(append(newStringLiteral(s),false)); end;
FUNCTION T_collectionLiteral.appendBool  (CONST b: boolean   ): P_collectionLiteral; begin result:=P_collectionLiteral(append(newBoolLiteral  (b),false)); end;
FUNCTION T_collectionLiteral.appendInt   (CONST i: int64     ): P_collectionLiteral; begin result:=P_collectionLiteral(append(newIntLiteral   (i),false)); end;
FUNCTION T_collectionLiteral.appendReal  (CONST r: T_myFloat ): P_collectionLiteral; begin result:=P_collectionLiteral(append(newRealLiteral  (r),false)); end;
FUNCTION T_collectionLiteral.appendAll   (CONST L: P_compoundLiteral): P_collectionLiteral;
  VAR x:P_literal;
  begin
    for x in L^.iteratableList do append(x,false);
    result:=@self
  end;

FUNCTION T_setLiteral.appendAll(CONST L:P_compoundLiteral):P_collectionLiteral;
  VAR x:P_literal;
      E:T_literalKeyBooleanValueMap.CACHE_ENTRY;
      prevBool:boolean; //dummy
  begin
    if L^.literalType in C_setTypes then begin
      for E in P_setLiteral(L)^.dat.keyValueList do
      if dat.putNew(E,prevBool) then E.key^.rereference;
    end else for x in L^.iteratableList do append(x,false);
    result:=@self;
  end;

//?.size:=======================================================================
FUNCTION T_listLiteral.size: longint; begin result:=fill;     end;
FUNCTION T_mapLiteral.size: longint; begin result:=dat.fill; end;
FUNCTION T_setLiteral.size: longint; begin result:=dat.fill; end;
//=======================================================================:?.size
FUNCTION T_listLiteral.head: P_literal;
  begin
    if fill=0
    then result:=@self
    else result:=dat[0];
    result^.rereference;
  end;

FUNCTION T_listLiteral.head(CONST headSize: longint): P_listLiteral;
  VAR i,imax:longint;
  begin
    imax:=headSize;
    if imax>fill then imax:=fill;
    if imax<0 then imax:=0;
    result:=newListLiteral;
    setLength(result^.dat,imax);
    for i:=0 to imax-1 do result^.append(dat[i],true);
  end;

FUNCTION T_listLiteral.tail: P_listLiteral;
  begin result:=tail(1); end;

FUNCTION T_listLiteral.tail(CONST headSize: longint): P_listLiteral;
  VAR i,iMin:longint;
  begin
    iMin:=headSize;
    if iMin>fill then iMin:=fill
    else if iMin<0 then iMin:=0;
    result:=newListLiteral;
    setLength(result^.dat,fill-iMin);
    for i:=iMin to fill-1 do result^.append(dat[i],true);
  end;

FUNCTION T_listLiteral.trailing: P_literal;
  begin
    if fill=0
    then result:=@self
    else result:=dat[fill-1];
    result^.rereference;
  end;

FUNCTION T_listLiteral.trailing(CONST trailSize: longint): P_listLiteral;
  begin result:=tail(fill-trailSize); end;

FUNCTION T_listLiteral.leading: P_listLiteral;
  begin result:=head(fill-1); end;

FUNCTION T_listLiteral.leading(CONST trailSize: longint): P_listLiteral;
  begin result:=head(fill-trailSize); end;

FUNCTION T_listLiteral.transpose(CONST filler: P_literal): P_listLiteral;
  VAR innerSize:longint=-1;
      i,j:longint;
      innerList:P_listLiteral;
      doneRow:boolean;
  begin
    if literalType=lt_emptyList then exit(P_listLiteral(rereferenced));
    for i:=0 to fill-1 do
    if (dat[i]^.literalType in C_listTypes)
    then innerSize:=max(innerSize,P_listLiteral(dat[i])^.size)
    else innerSize:=max(innerSize,1);

    result:=newListLiteral;
    for i:=0 to innerSize-1 do if not(result^.containsError) then begin
      if filler=nil then begin
        innerList:=newListLiteral;
        doneRow:=false;
        for j:=0 to fill-1 do if not(result^.containsError) then begin
          if (dat[j]^.literalType in C_listTypes) and (P_listLiteral(dat[j])^.fill>i) then
          begin innerList^.append(P_listLiteral(dat[j])^.dat[i],true); result^.containsError:=doneRow or result^.containsError; end
          else if (dat[j]^.literalType in C_scalarTypes) and (i=0) then
          begin innerList^.append(              dat[j]         ,true); result^.containsError:=doneRow or result^.containsError; end
          else doneRow:=true;
        end;
      end else begin
        innerList:=newListLiteral(fill);
        for j:=0 to fill-1 do if not(result^.containsError) then begin
          if (dat[j]^.literalType in C_listTypes) and (P_listLiteral(dat[j])^.fill>i)
          then                                                          innerList^.append(P_listLiteral(dat[j])^.dat[i],true)
          else if (dat[j]^.literalType in C_scalarTypes) and (i=0) then innerList^.append(              dat[j]         ,true)
          else                                                          innerList^.append(filler                       ,true);
        end;
      end;
      result^.append(innerList,false);
    end;
  end;

//?.toString:===================================================================
FUNCTION T_literal          .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:='<ERR>';           end;
FUNCTION T_voidLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=LITERAL_TEXT_VOID;        end;
FUNCTION T_boolLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=LITERAL_BOOL_TEXT[val];   end;
FUNCTION T_intLiteral       .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=intToStr(val);     end;
FUNCTION T_realLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=myFloatToStr(val); end;
FUNCTION T_stringLiteral    .toString(CONST lengthLimit:longint=maxLongint): ansistring;
  begin
    if lengthLimit>=length(val)+2 then result:=escapeString(val,es_pickShortest)
                                  else result:=escapeString(UTF8Copy(val,1,lengthLimit-5)+'...',es_pickShortest);
  end;

FUNCTION T_compoundLiteral.toString(CONST lengthLimit: longint): ansistring;
  VAR i,remainingLength: longint;
  begin
    if size = 0 then result:='[]'
    else begin
      remainingLength:=lengthLimit-1;
      result:='['+value[0]^.toString(remainingLength);
      for i:=1 to size-1 do if remainingLength>0 then begin
        remainingLength:=lengthLimit-length(result);
        result:=result+','+value[i]^.toString(remainingLength);
      end else begin
        result:=result+',... ';
        break;
      end;
      result:=result+']';
    end;
    if literalType in C_setTypes then result:=result+'.toSet';
    if literalType in C_mapTypes then result:=result+'.toMap';
  end;

FUNCTION T_listLiteral.listConstructorToString(CONST lengthLimit: longint): string;
  begin
    result:=toString(lengthLimit-1);
    setLength(result,length(result)-1);
  end;

//===================================================================:?.toString
FUNCTION toParameterListString(CONST list:P_listLiteral; CONST isFinalized: boolean; CONST lengthLimit:longint=maxLongint): ansistring;
  VAR i,remainingLength: longint;
  begin
    if list<>nil then with list^ do begin
      if fill = 0 then if isFinalized then exit('()')
                                      else exit('(');
      remainingLength:=lengthLimit-1;
      result:='('+dat[0]^.toString(lengthLimit);
      for i:=1 to fill-1 do if remainingLength>0 then begin
        remainingLength:=lengthLimit-length(result);
        result:=result+','+dat[i]^.toString;
      end else begin
        result:=result+',... ';
        break;
      end;
      if isFinalized then result:=result+')'
                     else result:=result+',';
    end else if isFinalized then exit('()')
                            else exit('(');
  end;

//?.stringForm:=================================================================
FUNCTION T_scalarLiteral.stringForm: ansistring; begin result:=toString; end;
FUNCTION T_stringLiteral.stringForm: ansistring; begin result:=val;      end;
//=================================================================:?.stringForm
//?.isInRelationTo:=============================================================
FUNCTION T_literal.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  begin
    result:=false;
  end;

FUNCTION T_boolLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR ovl: boolean;
  begin
    case relation of
      tt_operatorIn      : exit((other^.literalType in C_containingTypes[lt_boolean]) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    if other^.literalType<>lt_boolean then exit(false);
    ovl:=P_boolLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq, tt_comparatorLeq, tt_comparatorGeq])
         or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_intLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR ovi: int64;
      ovr: T_myFloat;
  begin
    case relation of
      tt_operatorIn      : exit((other^.literalType in C_containingTypes[lt_int]) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    case other^.literalType of
      lt_int: begin
        ovi:=P_intLiteral(other)^.val;
        result:=(val=ovi) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
             or (val<ovi) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (val>ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        ovr:=P_realLiteral(other)^.val;
        result:=(val=ovr) and (relation in [tt_comparatorEq, tt_comparatorLeq, tt_comparatorGeq])
             or (val<ovr) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (val>ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result:=false;
    end;
  end;

FUNCTION T_realLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR ovi: int64;
      ovr: T_myFloat;
  begin
    case relation of
      tt_operatorIn      : exit((other^.literalType in C_containingTypes[lt_real]) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    case other^.literalType of
      lt_int: begin
        ovi:=P_intLiteral(other)^.val;
        result:=(val=ovi) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
             or (val<ovi) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (val>ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        ovr:=P_realLiteral(other)^.val;
        result:=(val=ovr) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
             or (val<ovr) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (val>ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result:=false;
    end;
  end;

FUNCTION T_stringLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR ovl: ansistring;
  begin
    case relation of
      tt_operatorIn      : exit((other^.literalType in C_containingTypes[lt_string]) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    if other^.literalType<>lt_string then exit(false);
    ovl:=P_stringLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq, tt_comparatorLeq, tt_comparatorGeq])
         or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;


FUNCTION T_compoundLiteral.isInRelationTo(CONST relation: T_tokenType;
  CONST other: P_literal): boolean;
  begin
    if not(other^.literalType in C_compoundTypes) then exit(false);
    case relation of
      tt_operatorIn: result:=P_compoundLiteral(other)^.contains(@self);
      tt_comparatorListEq,tt_comparatorNeq: begin
        if (literalType in C_emptyCompoundTypes) and (other^.literalType in C_emptyCompoundTypes)
        then result:=true
        else result:=equals(other);
        if relation=tt_comparatorNeq then result:=not(result);
      end;
      else result:=false;
    end;
  end;
//=============================================================:?.isInRelationTo
FUNCTION T_listLiteral.newOfSameType: P_collectionLiteral; begin result:=newListLiteral; end;
FUNCTION T_setLiteral.newOfSameType: P_collectionLiteral; begin result:=newSetLiteral; end;
//?.negate:=====================================================================
FUNCTION T_literal.negate(CONST minusLocation: T_tokenLocation; VAR adapters: T_adapters): P_literal;
  begin result:=@self; rereference; end;
FUNCTION T_stringLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newVoidLiteral; adapters.raiseError('Cannot negate string.', minusLocation); end;
FUNCTION T_boolLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newVoidLiteral; adapters.raiseError('Cannot negate boolean.', minusLocation); end;
FUNCTION T_intLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newIntLiteral(-value); end;
FUNCTION T_realLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newRealLiteral(-value); end;
FUNCTION T_collectionLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters: T_adapters): P_literal;
  VAR res: P_collectionLiteral;
      i  : longint;
  begin
    res:=newOfSameType;
    for i:=0 to size-1 do res^.append(value[i]^.negate(minusLocation,adapters),false);
    result:=res;
  end;
//=====================================================================:?.negate
FUNCTION T_literal.typeString:           string; begin result:=C_typeString[literalType]; end;
FUNCTION T_compoundLiteral.typeString: string; begin result:=C_typeString[literalType]+'('+intToStr(size)+')';  end;

FUNCTION parameterListTypeString(CONST list:P_listLiteral):string;
  VAR i:longint;
  begin
    if (list=nil) or (list^.fill<=0) then exit('()');
    with list^ do begin
      result:='('+dat[0]^.typeString;
      for i:=1 to fill-1 do result:=result+', '+dat[i]^.typeString;
      result:=result+')';
    end;
  end;

//?.hash:=======================================================================
FUNCTION T_literal.hash: T_hashInt; begin result:=longint(literalType); end;
FUNCTION T_boolLiteral.hash: T_hashInt; begin result:=longint(lt_boolean); if val then inc(result); end;
FUNCTION T_intLiteral .hash: T_hashInt; begin {$R-} result:=longint(lt_int) xor longint(val); {$R+} end;
FUNCTION T_realLiteral.hash: T_hashInt;
  begin
    {$Q-}{$R-}
    result:=0;
    move(val, result, sizeOf(result));
    result:=result xor longint(lt_real);
    {$Q+}{$R+}
  end;

FUNCTION T_stringLiteral.hash: T_hashInt;
  VAR i: longint;
  begin
    {$Q-}{$R-}
    result:=T_hashInt(lt_string)+T_hashInt(length(val));
    for i:=1 to length(val) do result:=result*31+ord(val[i]);
    {$Q+}{$R+}
  end;

FUNCTION T_listLiteral.hash: T_hashInt;
  VAR i:longint;
  begin
    {$Q-}{$R-}
    result:=T_hashInt(lt_list)+T_hashInt(fill);
    for i:=0 to fill-1 do result:=result*31+dat[i]^.hash;
    {$Q+}{$R+}
  end;

FUNCTION T_setLiteral.hash: T_hashInt;
  VAR entry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    {$Q-}{$R-}
    result:=T_hashInt(lt_set)+T_hashInt(dat.fill);
    result:=result*31;
    for entry in dat.keyValueList do result:=result+entry.keyHash;
    {$Q+}{$R+}
  end;

FUNCTION T_mapLiteral.hash: T_hashInt;
  VAR entry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    {$Q-}{$R-}
    result:=T_hashInt(lt_map)+T_hashInt(dat.fill);
    result:=result*31;
    for entry in dat.keyValueList do result:=result+entry.keyHash+entry.value^.hash*37;
    {$Q+}{$R+}
  end;

//=======================================================================:?.hash
//?.equals:=====================================================================
FUNCTION T_literal.equals(CONST other: P_literal): boolean;
  begin result:=(@self = other) or (other^.literalType = literalType) and (other^.toString=toString);  end;

FUNCTION T_intLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other) or (other^.literalType = lt_int) and (P_intLiteral(other)^.val = val);
  end;

FUNCTION T_realLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other)
           or (other^.literalType = lt_real) and ((P_realLiteral(other)^.val = val)
                                               or isNan(P_realLiteral(other)^.val) and isNan(val)
                                               or isInfinite(P_realLiteral(other)^.val) and isInfinite(val));
  end;

FUNCTION T_stringLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other) or (other^.literalType = lt_string) and (P_stringLiteral(other)^.val = val);
  end;

FUNCTION T_compoundLiteral.equals(CONST other: P_literal): boolean;
  VAR i:longint;
  begin
    if (@self = other) then exit(true);
    if (other^.literalType<>literalType) or (P_compoundLiteral(other)^.size<>size) then exit(false);
    result:=true;
    for i:=0 to size-1 do if not(value[i]^.equals(P_compoundLiteral(other)^[i])) then exit(false);
  end;

FUNCTION T_listLiteral.equals(CONST other:P_literal):boolean;
  VAR i:longint;
  begin
    if (@self = other) then exit(true);
    if (other^.literalType<>literalType) or (P_compoundLiteral(other)^.size<>size) then exit(false);
    result:=true;
    for i:=0 to fill-1 do if not(dat[i]^.equals(P_listLiteral(other)^.dat[i])) then exit(false);
  end;

FUNCTION T_setLiteral.equals(CONST other:P_literal):boolean;
  VAR iter:T_arrayOfLiteral;
      sub:P_literal;
  begin
    if (@self = other) then exit(true);
    if (other^.literalType<>literalType) or (P_compoundLiteral(other)^.size<>size) then exit(false);
    result:=true;
    iter:=dat.keySet;
    for sub in iter do if not(P_setLiteral(other)^.dat.get(sub,false)) then exit(false);
  end;

FUNCTION T_mapLiteral.equals(CONST other:P_literal):boolean;
  VAR entries:T_literalKeyLiteralValueMap.KEY_VALUE_LIST;
      entry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
      otherValue:P_literal;
  begin
    if (@self = other) then exit(true);
    if (other^.literalType<>literalType) or (P_compoundLiteral(other)^.size<>size) then exit(false);
    result:=true;
    entries:=dat.keyValueList;
    for entry in entries do begin
      otherValue:=P_mapLiteral(other)^.dat.get(entry.key,nil);
      if (otherValue=nil) or not(entry.value^.equals(otherValue)) then exit(false);
    end;
  end;
//=====================================================================:?.equals

//?.contains:===================================================================
FUNCTION T_listLiteral.contains(CONST other: P_literal): boolean;
  VAR i:longint;
  begin
    if not(literalType in C_containingTypes[other^.literalType]) then exit(false);
    for i:=0 to fill-1 do if dat[i]^.equals(other) then exit(true);
    result:=false;
  end;

FUNCTION T_setLiteral.contains(CONST other: P_literal): boolean;
  begin
    if not(literalType in C_containingTypes[other^.literalType]) then exit(false);
    result:=dat.get(other,false);
  end;

FUNCTION T_mapLiteral.contains(CONST other: P_literal): boolean;
  VAR key,val,mapValue:P_literal;
  begin
    if not(literalType in C_containingTypes[other^.literalType]) or
       not((other^.literalType in C_listTypes) and (P_listLiteral(other)^.isKeyValuePair)) then exit(false);
    key:=P_listLiteral(other)^.dat[0];
    val:=P_listLiteral(other)^.dat[1];
    mapValue:=dat.get(key,nil);
    result:=(mapValue<>nil) and (mapValue^.equals(val));
  end;
//===================================================================:?.contains
//?.isKeyValuePair:=============================================================
FUNCTION T_listLiteral.isKeyValuePair: boolean; begin result:=(fill=2); end;
//=============================================================:?.isKeyValuePair
FUNCTION T_listLiteral.isKeyValueCollection: boolean;
  VAR i:longint;
  begin
    if fill<1 then exit(true);
    result:=true;
    for i:=0 to fill-1 do if not((dat[i]^.literalType in C_listTypes) and P_listLiteral(dat[i])^.isKeyValuePair) then exit(false);
  end;

FUNCTION T_setLiteral.isKeyValueCollection: boolean;
  VAR L:P_literal;
  begin
    if dat.fill<1 then exit(true);
    result:=true;
    for L in dat.keySet do if not((L^.literalType in C_listTypes) and P_listLiteral(L)^.isKeyValuePair) then exit(false);
  end;

FUNCTION T_listLiteral.get(CONST accessor:P_literal):P_literal;
  VAR i,j:longint;
      iter:T_arrayOfLiteral;
      idx:P_literal;
  begin
    result:=nil;
    case accessor^.literalType of
      lt_int: begin
        i:=P_intLiteral(accessor)^.val;
        if (i>=0) and (i<fill) then exit(dat[i]^.rereferenced)
                               else exit(newVoidLiteral);
      end;
      lt_intList, lt_emptyList: begin
        result:=newListLiteral(P_listLiteral(accessor)^.fill);
        for j:=0 to P_listLiteral(accessor)^.fill-1 do begin
          i:=P_intLiteral(P_listLiteral(accessor)^.dat[j])^.val;
          if (i>=0) and (i<fill) then P_listLiteral(result)^.append(dat[i],true);
        end;
        exit(result);
      end;
      lt_intSet, lt_emptySet: begin
        result:=newSetLiteral;
        iter:=P_setLiteral(accessor)^.iteratableList;
        for idx in iter do begin
          i:=P_intLiteral(idx)^.val;
          if (i>=0) and (i<fill) then P_setLiteral(result)^.append(dat[i],true);
        end;
        disposeLiteral(iter);
        exit(result);
      end;
      lt_booleanList: if (P_listLiteral(accessor)^.fill=fill) then begin
        result:=newListLiteral(fill);
        for i:=0 to fill-1 do if P_boolLiteral(P_listLiteral(accessor)^.dat[i])^.val then P_listLiteral(result)^.append(dat[i],true);
        exit(result);
      end;
    end;
    if isKeyValueCollection then begin
      for i:=0 to fill-1 do if accessor^.equals(P_listLiteral(dat[i])^[0])
                                      then exit(P_listLiteral(dat[i])^[1]^.rereferenced);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION T_setLiteral.get(CONST accessor:P_literal):P_literal;
  VAR iter:T_arrayOfLiteral;
      x:P_literal;
  begin
    result:=nil;
    if isKeyValueCollection then begin
      iter:=iteratableList;
      for x in iter do if (result=nil) and accessor^.equals(P_listLiteral(x)^[0])
                                               then result:=P_listLiteral(x)^[1]^.rereferenced;
      if result=nil then result:=newVoidLiteral;
      disposeLiteral(iter);
    end else result:=nil;
  end;

FUNCTION T_mapLiteral.get(CONST accessor:P_literal):P_literal;
  begin
    result:=dat.get(accessor,nil);
    if result=nil then result:=newVoidLiteral
                  else result^.rereference;
  end;

FUNCTION T_listLiteral.getInner(CONST accessor:P_literal):P_literal;
  VAR i:longint;
  begin
    result:=newListLiteral(fill);
    if literalType=lt_list then
    for i:=0 to fill-1 do
    if dat[i]^.literalType in C_compoundTypes
    then P_listLiteral(result)^.append(P_compoundLiteral(dat[i])^.get(accessor),false)
    else begin
      disposeLiteral(result);
      exit(nil);
    end;
  end;

FUNCTION T_setLiteral.getInner(CONST accessor:P_literal):P_literal;
  VAR iter:T_arrayOfLiteral;
      sub:P_literal;
  begin
    iter:=iteratableList;
    result:=newSetLiteral;
    for sub in iter do if sub^.literalType in C_compoundTypes
    then P_setLiteral(result)^.append(P_compoundLiteral(sub)^.get(accessor),false)
    else begin
      disposeLiteral(result);
      disposeLiteral(iter);
      exit(nil);
    end;
    disposeLiteral(iter);
  end;

FUNCTION T_mapLiteral.getInner(CONST accessor:P_literal):P_literal;
  VAR validCase :boolean=false;
      wantKeys  :boolean=false;
      wantValues:boolean=false;
      subAsSet  :boolean=false;
      entry     :T_literalKeyLiteralValueMap.CACHE_ENTRY;
      sub       :P_collectionLiteral;
  begin
    case accessor^.literalType of
      lt_int: begin
        wantKeys  :=(P_intLiteral(accessor)^.val=0);
        wantValues:=(P_intLiteral(accessor)^.val=1);
        validCase :=true;
      end;
      lt_intList, lt_intSet,lt_emptyList,lt_emptySet: begin
        wantKeys  :=P_compoundLiteral(accessor)^.contains(@intLit[0]);
        wantValues:=P_compoundLiteral(accessor)^.contains(@intLit[1]);
        validCase :=true;
        subAsSet  :=accessor^.literalType in C_setTypes;
      end;
      lt_booleanList: if P_listLiteral(accessor)^.size=2 then begin
        wantKeys  :=P_boolLiteral(P_listLiteral(accessor)^[0])^.val;
        wantValues:=P_boolLiteral(P_listLiteral(accessor)^[1])^.val;
        validCase :=true;
      end;
    end;
    if not(validCase) then begin
      result:=nil;
      exit(result);
    end;
    result:=newListLiteral(dat.fill);
    if wantKeys then begin
      if wantValues then for entry in dat.keyValueList do begin
        if subAsSet then sub:=newSetLiteral
                    else sub:=newListLiteral(2);
        sub^.append(entry.key  ,true);
        sub^.append(entry.value,true);
        P_listLiteral(result)^.append(sub,false);
      end else for entry in dat.keyValueList do
        P_listLiteral(result)^.append(entry.key,true);
    end else if wantValues then for entry in dat.keyValueList do
      P_listLiteral(result)^.append(entry.value,true);
  end;
//?.leqForSorting:==============================================================
FUNCTION T_literal.leqForSorting(CONST other: P_literal): boolean;
  begin
    if (other^.literalType = lt_expression)
    then result:=toString<=other^.toString
    else result:=literalType<=other^.literalType;
  end;

FUNCTION T_boolLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    if other^.literalType = lt_boolean
    then result:=value<=P_boolLiteral(other)^.value
    else result:=(literalType<=other^.literalType);
  end;

FUNCTION T_intLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    case other^.literalType of
      lt_int:  result:=val<=P_intLiteral (other)^.val;
      lt_real: result:=val<=P_realLiteral(other)^.val;
    else result:=(literalType<=other^.literalType); end;
  end;

FUNCTION T_realLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    case other^.literalType of
      lt_int:  result:=val< P_intLiteral (other)^.val;
      lt_real: result:=val<=P_realLiteral(other)^.val;
    else result:=(literalType<=other^.literalType);  end;
  end;

FUNCTION T_stringLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    if (other^.literalType = lt_string)
    then result:=val<=P_stringLiteral(other)^.val
    else result:=(literalType<=other^.literalType);
  end;

FUNCTION T_compoundLiteral.leqForSorting(CONST other: P_literal): boolean;
  VAR i: longint;
  begin
    if (other^.literalType in C_compoundTypes) then begin
      if      size<P_compoundLiteral(other)^.size then exit(true)
      else if size>P_compoundLiteral(other)^.size then exit(false)
      else for i:=0 to size-1 do if value[i]^.leqForSorting(P_compoundLiteral(other)^[i]) then begin
        if not(P_compoundLiteral(other)^[i]^.leqForSorting(value[i])) then exit(true);
      end else exit(false);
      exit(true);
    end else result:=literalType<=other^.literalType;
  end;

//?.leqForSorting:==============================================================
FUNCTION T_stringLiteral.softCast: P_scalarLiteral;
  VAR
    len: longint;
    otherVal: ansistring;
  begin
    if lowercase(val) = LITERAL_BOOL_TEXT[false] then
      exit(newBoolLiteral(false));
    if lowercase(val) = LITERAL_BOOL_TEXT[true] then
      exit(newBoolLiteral(true));
    result:=parseNumber(val, 1, false, len);
    if (result<>nil) then
      if (len = length(val)) then
        exit(result)
      else
        disposeLiteral(result);
    otherVal:=unescapeString(sysutils.trim(value),1, len);
    if len = length(sysutils.trim(value)) then
      exit(newStringLiteral(otherVal));
    result:=@self;
    rereference;
  end;

FUNCTION T_stringLiteral.trim: P_stringLiteral;
  VAR rs: ansistring;
  begin
    rs:=sysutils.trim(val);
    if rs = val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.trimLeft: P_stringLiteral;
  VAR rs: ansistring;
  begin
    rs:=sysutils.trimLeft(val);
    if rs = val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.trimRight: P_stringLiteral;
  VAR rs: ansistring;
  begin
    rs:=sysutils.trimRight(val);
    if rs = val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.upper: P_stringLiteral;
  VAR rs: string;
  begin
    rs:=UTF8UpperCase(val);
    if rs = val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.lower: P_stringLiteral;
  VAR rs: string;
  begin
    rs:=UTF8LowerCase(val);
    if rs = val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.unbrace: P_stringLiteral;
  VAR rs: string;
  begin
    rs:=myStringUtil.unbrace(val);
    if rs = val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.escape: P_stringLiteral;
  begin
    result:=newStringLiteral(escapeString(val,es_pickShortest));
  end;

PROCEDURE T_stringLiteral.append(CONST suffix:ansistring);
  begin
    val:=val+suffix;
  end;

PROCEDURE T_compoundLiteral.modifyType(CONST L: P_literal);
  begin
    case literalType of
      lt_error        : containsError:=true;
      lt_booleanList  : if L^.literalType<>lt_boolean then literalType:=lt_list;
      lt_booleanSet   : if L^.literalType<>lt_boolean then literalType:=lt_set;
      lt_intList      : case L^.literalType of
                          lt_int: begin end;
                          lt_real: literalType:=lt_numList;
  			  else     literalType:=lt_list;
  	                end;
      lt_intSet       : case L^.literalType of
                          lt_int: begin end;
                          lt_real: literalType:=lt_numSet;
  			  else     literalType:=lt_set;
  	                end;
      lt_realList     : case L^.literalType of
  	                  lt_real: begin end;
  	                  lt_int: literalType:=lt_numList;
  			  else    literalType:=lt_list;
                        end;
      lt_realSet      : case L^.literalType of
  	                  lt_real: begin end;
  	                  lt_int: literalType:=lt_numSet;
  			  else    literalType:=lt_set;
                        end;
      lt_numList      : if not(L^.literalType in [lt_int,lt_real]) then literalType:=lt_list;
      lt_numSet       : if not(L^.literalType in [lt_int,lt_real]) then literalType:=lt_set;
      lt_stringList   : if L^.literalType<>lt_string then literalType:=lt_list;
      lt_stringSet    : if L^.literalType<>lt_string then literalType:=lt_set;
      lt_emptyList    : case L^.literalType of
                          lt_boolean:literalType:=lt_booleanList;
                          lt_int:    literalType:=lt_intList;
                          lt_real:   literalType:=lt_realList;
                          lt_string: literalType:=lt_stringList;
                          else       literalType:=lt_list;
                        end;
      lt_emptySet     : case L^.literalType of
                          lt_boolean:literalType:=lt_booleanSet;
                          lt_int:    literalType:=lt_intSet;
                          lt_real:   literalType:=lt_realSet;
                          lt_string: literalType:=lt_stringSet;
                          else       literalType:=lt_set;
                        end;
      lt_emptyMap     : literalType:=lt_map;
    end;
    containsError:=containsError or (L^.literalType in C_compoundTypes) and (P_compoundLiteral(L)^.containsError);
  end;

FUNCTION T_compoundLiteral.toSet: P_setLiteral;
  begin
    if literalType in C_setTypes then exit(P_setLiteral(rereferenced));
    result:=P_setLiteral(newSetLiteral^.appendAll(@self));
  end;

FUNCTION T_compoundLiteral.toList: P_listLiteral;
  begin
    if literalType in C_listTypes then exit(P_listLiteral(rereferenced));
    result:=P_listLiteral(newListLiteral^.appendAll(@self));
  end;

FUNCTION T_compoundLiteral.toMap(CONST location:T_tokenLocation; VAR adapters:T_adapters): P_mapLiteral;
  VAR iter:T_arrayOfLiteral;
      pair:P_literal;
  begin
    if literalType in C_mapTypes then exit(P_mapLiteral(rereferenced));
    iter:=iteratableList;
    result:=newMapLiteral;
    for pair in iter do if (pair^.literalType in C_listTypes) and (P_listLiteral(pair)^.isKeyValuePair) then begin
      result^.put(P_listLiteral(pair)^[0],
                  P_listLiteral(pair)^[1],true);
    end else begin
      result^.containsError:=true;
      adapters.raiseError('Literal of type '+pair^.typeString+' cannot be intrerpreted as key-value-pair',location);
    end;
    disposeLiteral(iter);
  end;

FUNCTION T_listLiteral.appendConstructing(CONST L: P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST doRangeAppend:boolean):P_compoundLiteral;
  VAR last: P_literal;
      i0, i1: int64;
      c0, c1: char;
      newLen: longint;
  begin
    result:=@self;
    if not(doRangeAppend) then begin
      append(L, true);
      exit;
    end;
    if fill=0 then begin
      adapters^.raiseError('Cannot append range to empty list', location);
      exit;
    end;
    last:=dat[fill-1];
    if (last^.literalType = lt_int) and (L^.literalType = lt_int) then begin
      i0:=P_intLiteral(last)^.val;
      i1:=P_intLiteral(L)^.val;
      newLen:=fill+abs(i1-i0)+1;
      if newLen>length(dat) then setLength(dat,newLen);
      while (i0<i1) and adapters^.noErrors do begin
        inc(i0);
        appendInt(i0);
      end;
      while (i0>i1) and adapters^.noErrors do begin
        dec(i0);
        appendInt(i0);
      end;
    end else if (last^.literalType = lt_string) and
      (length(P_stringLiteral(last)^.val) = 1) and (L^.literalType = lt_string) and
      (length(P_stringLiteral(L   )^.val) = 1) then begin
      c0:=P_stringLiteral(last)^.val [1];
      c1:=P_stringLiteral(L)^.val [1];
      newLen:=fill+abs(ord(c1)-ord(c0))+1;
      if newLen>length(dat) then setLength(dat,newLen);
      while c0<c1 do begin
        inc(c0);
        appendString(c0);
      end;
      while c0>c1 do begin
        dec(c0);
        appendString(c0);
      end;
    end else begin
      literalType:=lt_list;
      adapters^.raiseError('Invalid range expression '+
        last^.toString+'..'+L^.toString, location);
    end;
  end;

FUNCTION T_listLiteral.append(CONST L: P_literal; CONST incRefs: boolean): P_collectionLiteral;
  begin
    result:=@self;
    if (L=nil) or (L^.literalType=lt_void) then exit;
    if length(dat)<=fill then setLength(dat,round(fill*1.1)+1);
    dat[fill]:=L;
    inc(fill);
    if incRefs then L^.rereference;
    modifyType(L);
  end;

FUNCTION T_setLiteral.append(CONST L: P_literal; CONST incRefs: boolean): P_collectionLiteral;
  VAR prevBool:boolean;
  begin
    result:=@self;
    if (L=nil) or (L^.literalType=lt_void) then exit;
    if dat.putNew(L,true,prevBool) then begin
      if incRefs then L^.rereference;
      modifyType(L);
      dropManifestation;
    end else if not(incRefs) then disposeLiteralWithoutResettingPointer(L);
  end;

FUNCTION T_mapLiteral.put(CONST key,newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
  VAR prevValue:P_literal;
  begin
    if dat.putNew(key,newValue,prevValue) then begin
      if incRefs then key^.rereference;
      literalType:=lt_map;
      dropManifestation;
    end else begin
      disposeLiteral(prevValue);
    end;
    if incRefs then newValue^.rereference;
    result:=@self;
  end;

FUNCTION T_mapLiteral.put(CONST key,newValue:ansistring                 ):P_mapLiteral; begin result:=put(newStringLiteral(key), newStringLiteral(newValue),false); end;
FUNCTION T_mapLiteral.put(CONST key:ansistring; CONST newValue:int64    ):P_mapLiteral; begin result:=put(newStringLiteral(key), newIntLiteral   (newValue),false); end;
FUNCTION T_mapLiteral.put(CONST key:ansistring; CONST newValue:T_myFloat):P_mapLiteral; begin result:=put(newStringLiteral(key), newRealLiteral  (newValue),false); end;
FUNCTION T_mapLiteral.put(CONST key:ansistring; CONST newValue:boolean  ):P_mapLiteral; begin result:=put(newStringLiteral(key), newBoolLiteral  (newValue),false); end;
FUNCTION T_mapLiteral.put(CONST key:ansistring; CONST newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
  begin
    if incRefs then newValue^.rereference;
    result:=put(newStringLiteral(key),
                newValue,false);
  end;

FUNCTION T_mapLiteral.put(CONST key:P_literal; CONST newValue:int64; CONST incRefs:boolean):P_mapLiteral;
  begin
    if incRefs then key^.rereference;
    result:=put(key,
                newIntLiteral(newValue),false);
  end;


FUNCTION T_mapLiteral.putAll(CONST map:P_mapLiteral):P_mapLiteral;
  VAR prevValue:P_literal;
      E:T_literalKeyLiteralValueMap.CACHE_ENTRY;
      someNew:boolean=false;
  begin
    for E in map^.dat.keyValueList do begin
      if dat.putNew(E,prevValue) then begin
        E.key^.rereference;
        literalType:=lt_map;
        someNew:=true;
      end else begin
        disposeLiteral(prevValue);
      end;
      E.value^.rereference;
    end;
    if someNew then dropManifestation;
    result:=@self;
  end;

PROCEDURE T_mapLiteral.drop(CONST L: P_scalarLiteral);
  VAR dropped:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    dropped:=dat.drop(L);
    if dropped.key=nil then exit;
    dropManifestation;
    disposeLiteral(dropped.key);
    disposeLiteral(dropped.value);
  end;

PROCEDURE T_listLiteral.sort;
  VAR temp: T_arrayOfLiteral;
      scale: longint;
      i, j0, j1, k: longint;
  begin
    if (fill<=1) then exit;
    scale:=1;
    setLength(temp, fill);
    while scale<fill do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<fill do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<fill) do
          if dat[j0]^.leqForSorting(dat[j1]) then begin temp[k]:=dat[j0]; inc(k); inc(j0); end
                                             else begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<fill) do begin temp[k]:=dat[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<fill) do begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<fill) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while i<fill do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<fill) do
            if temp [j0]^.leqForSorting(temp[j1]) then begin dat[k]:=temp[j0]; inc(k); inc(j0); end
                                                  else begin dat[k]:=temp[j1]; inc(k); inc(j1); end;
          while (j0<i+scale)       and (j0<fill)    do begin dat[k]:=temp[j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<fill)    do begin dat[k]:=temp[j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to fill-1 do dat[k]:=temp[k];
    end;
    setLength(temp, 0);
  end;

PROCEDURE T_listLiteral.sortBySubIndex(CONST innerIndex: longint;
  CONST location: T_tokenLocation; VAR adapters: T_adapters);
  VAR temp: T_arrayOfLiteral;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(a,b:P_literal):boolean; inline;
    begin
      if (a^.literalType in C_listTypes) and (P_listLiteral(a)^.fill>innerIndex) and
         (b^.literalType in C_listTypes) and (P_listLiteral(b)^.fill>innerIndex)
      then result:=P_listLiteral(a)^.dat[innerIndex]^.leqForSorting(P_listLiteral(b)^.dat[innerIndex])
      else begin
        result:=false;
        adapters.raiseError('Invalid sorting index '+intToStr(innerIndex)+' for elements '+a^.toString(50)+' and '+b^.toString(50),location);
      end;
    end;

  begin
    if fill<=1 then exit;
    scale:=1;
    setLength(temp, fill);
    while (scale<fill) and adapters.noErrors do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while (i<fill) and adapters.noErrors do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<fill) do
          if isLeq(dat[j0],dat[j1])          then begin temp[k]:=dat[j0]; inc(k); inc(j0); end
                                             else begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<fill) do begin temp[k]:=dat[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<fill) do begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      if not(adapters.noErrors) then exit;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<fill) and adapters.noErrors then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while (i<fill) and adapters.noErrors do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<fill) do
            if isLeq(temp[j0],temp[j1])        then begin dat[k]:=temp[j0]; inc(k); inc(j0); end
                                               else begin dat[k]:=temp[j1]; inc(k); inc(j1); end;
          while (j0<i+scale)       and (j0<fill) do begin dat[k]:=temp[j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<fill) do begin dat[k]:=temp[j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to fill-1 do dat[k]:=temp[k];
    end;
    setLength(temp, 0);
  end;

PROCEDURE T_listLiteral.customSort(CONST leqExpression: P_expressionLiteral; CONST location: T_tokenLocation; CONST context:pointer; VAR adapters: T_adapters);
  VAR temp: T_arrayOfLiteral;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(a,b:P_literal):boolean; inline; begin result:=leqExpression^.evaluateToBoolean(a,b,location,context); end;

  begin
    if fill<=1 then exit;
    scale:=1;
    setLength(temp, fill);
    while (scale<fill) and adapters.noErrors do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while (i<fill) and adapters.noErrors do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<fill) do
          if isLeq(dat[j0],dat[j1])          then begin temp[k]:=dat[j0]; inc(k); inc(j0); end
                                             else begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<fill) do begin temp[k]:=dat[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<fill) do begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      if not(adapters.noErrors) then exit;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<fill) and adapters.noErrors then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while (i<fill) and adapters.noErrors do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<fill) do
            if isLeq(temp [j0],temp [j1])      then begin dat[k]:=temp[j0]; inc(k); inc(j0); end
                                               else begin dat[k]:=temp[j1]; inc(k); inc(j1); end;
          while (j0<i+scale)       and (j0<fill) do begin dat[k]:=temp[j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<fill) do begin dat[k]:=temp[j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to fill-1 do dat[k]:=temp[k];
    end;
    setLength(temp, 0);
  end;

FUNCTION T_listLiteral.sortPerm: P_listLiteral;
  VAR temp1, temp2: array of record
        v: P_literal;
        index: longint;
      end;
      scale: longint;
      i,j0,j1,k: longint;
  begin
    if fill = 0 then exit(newListLiteral);
    setLength(temp1, fill);
    setLength(temp2, fill);
    for i:=0 to fill-1 do with temp1[i] do begin
      v:=P_scalarLiteral(dat[i]);
      index:=i;
    end;
    scale:=1;
    while scale<length(temp1) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<length(temp1) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do
          if temp1[j0].v^.leqForSorting(temp1[j1].v)  then begin temp2[k]:=temp1[j0]; inc(k); inc(j0); end
                                                      else begin temp2[k]:=temp1[j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<length(temp1)) do begin temp2[k]:=temp1[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(temp1)) do begin temp2[k]:=temp1[j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<length(temp1)) then begin
        i:=0;
        while i<length(temp1) do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do
            if temp2[j0].v^.leqForSorting(temp2[j1].v)  then begin temp1[k]:=temp2[j0]; inc(k); inc(j0); end
                                                        else begin temp1[k]:=temp2[j1]; inc(k); inc(j1); end;
          while (j0<i+scale)       and (j0<length(temp1)) do begin temp1[k]:=temp2[j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(temp1)) do begin temp1[k]:=temp2[j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to length(temp1)-1 do temp1[k]:=temp2[k];
    end;
    setLength(temp2, 0);
    result:=newListLiteral;
    setLength(result^.dat,length(temp1));
    for i:=0 to length(temp1)-1 do result^.appendInt(temp1[i].index);
    setLength(temp1, 0);
  end;

PROCEDURE T_listLiteral.unique;
  VAR i,j:longint;
  begin
    if fill<=0 then exit;
    sort;
    j:=0;
    for i:=1 to fill-1 do
    if dat[i]^.equals(dat[j]) then begin
      disposeLiteral(dat[i]);
    end else begin
      inc(j);
      dat[j]:=dat[i];
    end;
    fill:=j+1;
  end;

FUNCTION T_listLiteral.clone: P_compoundLiteral;
  VAR i:longint;
  begin
    result:=newListLiteral(fill);
    for i:=0 to fill-1 do P_listLiteral(result)^.dat[i]:=dat[i]^.rereferenced;
    P_listLiteral(result)^.fill       :=fill;
    P_listLiteral(result)^.literalType:=literalType;
  end;

FUNCTION T_setLiteral.clone: P_compoundLiteral;
  VAR bin,i:longint;
  begin
    result:=newSetLiteral;
    setLength(P_setLiteral(result)^.dat.dat,
                             length(dat.dat));
    for bin:=0 to length(dat.dat)-1 do begin
      setLength(P_setLiteral(result)^.dat.dat[bin],
                               length(dat.dat[bin]));
      for i:=0 to length(dat.dat[bin])-1 do begin
        P_setLiteral(result)^.dat.dat[bin,i]:=
                              dat.dat[bin,i];
        dat.dat[bin,i].key^.rereference;
      end;
    end;
  end;

FUNCTION T_mapLiteral.clone: P_compoundLiteral;
  VAR bin,i:longint;
  begin
    result:=newMapLiteral;
    setLength(P_mapLiteral(result)^.dat.dat,
                             length(dat.dat));
    for bin:=0 to length(dat.dat)-1 do begin
      setLength(P_mapLiteral(result)^.dat.dat[bin],
                               length(dat.dat[bin]));
      for i:=0 to length(dat.dat[bin])-1 do begin
        P_mapLiteral(result)^.dat.dat[bin,i]:=
                              dat.dat[bin,i];
        dat.dat[bin,i].key  ^.rereference;
        dat.dat[bin,i].value^.rereference;
      end;
    end;
  end;

FUNCTION T_listLiteral.iteratableList: T_arrayOfLiteral;
  VAR i:longint;
  begin
    setLength(result,fill);
    for i:=0 to fill-1 do result[i]:=dat[i]^.rereferenced;
  end;

FUNCTION T_setLiteral.iteratableList: T_arrayOfLiteral;
  VAR L:P_literal;
  begin
    enterCriticalSection(manifestationCs);
    if manifestation<>nil then begin
      result:=manifestation^.iteratableList;
      leaveCriticalSection(manifestationCs);
      exit(result);
    end else begin
      leaveCriticalSection(manifestationCs);
      result:=dat.keySet;
      for L in result do L^.rereference;
    end;
  end;

FUNCTION T_mapLiteral.iteratableList: T_arrayOfLiteral;
  VAR e:T_literalKeyLiteralValueMap.KEY_VALUE_LIST;
      i:longint;
  begin
    enterCriticalSection(manifestationCs);
    if manifestation<>nil then begin
      result:=manifestation^.iteratableList;
      leaveCriticalSection(manifestationCs);
      exit(result);
    end else begin
      leaveCriticalSection(manifestationCs);
      e:=dat.keyValueList;
      setLength(result,length(e));
      for i:=0 to length(e)-1 do result[i]:=newListLiteral(2)^.append(e[i].key,true)^.append(e[i].value,true);
    end;
  end;

FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  FUNCTION newErrorLiteral(CONST errorMessage:ansistring):P_literal;
    begin
      result:=errLit.rereferenced;
      adapters.raiseError(errorMessage,tokenLocation);
    end;

  FUNCTION defaultErrorLiteral:P_literal;
    begin
      result:=errLit.rereferenced;
      adapters.raiseError('Operator '+C_tokenInfo[op].defaultId+' cannot be applied to operands of type '+LHS^.typeString+' and '+RHS^.typeString,tokenLocation);
    end;

  {$MACRO ON}
  {$define defaultLHScases:=
    lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation));
    lt_void:       exit(RHS^.rereferenced);
    lt_error:      exit(LHS^.rereferenced)}
  {$define defaultRhsCases:=
    lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation));
    lt_void:       exit(LHS^.rereferenced);
    lt_error:      exit(RHS^.rereferenced)}
  {$define S_x_L_recursion:=
    begin
      result:=P_collectionLiteral(RHS)^.newOfSameType;
      for i:=0 to P_compoundLiteral(RHS)^.size-1 do P_collectionLiteral(result)^.append(
        function_id(                  LHS      ,
                    P_compoundLiteral(RHS)^[i]),false);
      exit(result);
    end}
  {$define L_x_S_recursion:=
    begin
      result:=P_collectionLiteral(LHS)^.newOfSameType;
      for i:=0 to P_collectionLiteral(LHS)^.size-1 do P_collectionLiteral(result)^.append(
        function_id(P_collectionLiteral(LHS)^[i],
                                        RHS           ),false);
      exit(result);
    end}
  {$define L_x_L_recursion:=
    if (LHS^.literalType in C_listTypes) and (RHS^.literalType in C_listTypes) and (P_compoundLiteral(LHS)^.size=P_compoundLiteral(RHS)^.size) then begin
      result:=newListLiteral(P_listLiteral(LHS)^.fill);
      for i:=0 to P_listLiteral(LHS)^.fill-1 do P_listLiteral(result)^.append(
        function_id(P_listLiteral(LHS)^.dat[i],
                    P_listLiteral(RHS)^.dat[i]),false);
      exit(result);
    end else if (LHS^.literalType in C_setTypes) and (RHS^.literalType in C_setTypes) then begin
      result:=newSetLiteral;
      for i:=0 to P_setLiteral(LHS)^.size-1 do
      for j:=0 to P_setLiteral(RHS)^.size-1 do P_setLiteral(result)^.append(
        function_id(P_setLiteral(LHS)^[i],
                    P_setLiteral(RHS)^[j]),false);
      exit(result);
    end}

  FUNCTION perform_comparator(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_comparator}
    VAR i,j:longint;
    begin
      if RHS^.literalType in C_comparableTypes[LHS^.literalType] then
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean..lt_string: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean..lt_string: if RHS^.literalType in C_comparableTypes[LHS^.literalType] then exit(newBoolLiteral(LHS^.isInRelationTo(op,RHS)));
          lt_list..lt_emptySet : if RHS^.literalType in C_comparableTypes[LHS^.literalType] then S_x_L_recursion;
        end;
        lt_list..lt_emptySet: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean..lt_string: if RHS^.literalType in C_comparableTypes[LHS^.literalType] then L_x_S_recursion;
          lt_list..lt_emptySet : if RHS^.literalType in C_comparableTypes[LHS^.literalType] then L_x_L_recursion;
        end;
      end;
      result:=newErrorLiteral('Incompatible comparands '+LHS^.typeString+' and '+RHS^.typeString);
    end;

  FUNCTION perform_and(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_and}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean: exit(newBoolLiteral(P_boolLiteral(LHS)^.val and P_boolLiteral(RHS)^.val));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_booleanList,lt_booleanSet:  S_x_L_recursion;
        end;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int: exit(newIntLiteral(P_intLiteral(LHS)^.val and P_intLiteral(RHS)^.val));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_intList,lt_intSet: S_x_L_recursion;
        end;
        lt_set ,lt_emptySet ,
        lt_list,lt_emptyList: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean,lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet,lt_intList,lt_intSet: L_x_L_recursion;
        end;
        lt_booleanList,lt_booleanSet: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet: L_x_L_recursion;
        end;
        lt_intList,lt_intSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_intList,lt_intSet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_or(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_or}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean: exit(newBoolLiteral(P_boolLiteral(LHS)^.val or P_boolLiteral(RHS)^.val));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_booleanList,lt_booleanSet:  S_x_L_recursion;
        end;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int: exit(newIntLiteral(P_intLiteral(LHS)^.val or P_intLiteral(RHS)^.val));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_intList,lt_intSet: S_x_L_recursion;
        end;
        lt_set ,lt_emptySet ,
        lt_list,lt_emptyList: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean,lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet,lt_intList,lt_intSet: L_x_L_recursion;
        end;
        lt_booleanList,lt_booleanSet: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet: L_x_L_recursion;
        end;
        lt_intList,lt_intSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_intList,lt_intSet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_xor(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_xor}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean: exit(newBoolLiteral(P_boolLiteral(LHS)^.val xor P_boolLiteral(RHS)^.val));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_booleanList,lt_booleanSet:  S_x_L_recursion;
        end;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int: exit(newIntLiteral(P_intLiteral(LHS)^.val xor P_intLiteral(RHS)^.val));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_intList,lt_intSet: S_x_L_recursion;
        end;
        lt_set ,lt_emptySet ,
        lt_list,lt_emptyList: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean,lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet,lt_intList,lt_intSet: L_x_L_recursion;
        end;
        lt_booleanList,lt_booleanSet: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet: L_x_L_recursion;
        end;
        lt_intList,lt_intSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_intList,lt_intSet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_plus(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_plus}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    {$Q-}exit(newIntLiteral (P_intLiteral(LHS)^.val+P_intLiteral (RHS)^.val));{$Q+}
          lt_real:   exit(newRealLiteral(P_intLiteral(LHS)^.val+P_realLiteral(RHS)^.val));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.val+P_intLiteral (RHS)^.val));
          lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.val+P_realLiteral(RHS)^.val));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_string: case RHS^.literalType of
          defaultRhsCases;
          lt_string: exit(newStringLiteral(P_stringLiteral(LHS)^.val+P_stringLiteral(RHS)^.val));
          lt_list,lt_stringList,lt_emptyList,
          lt_set ,lt_stringSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_stringList,lt_stringSet: case RHS^.literalType of
          defaultRhsCases;
          lt_string: L_x_S_recursion;
          lt_list,lt_stringList,
          lt_set ,lt_stringSet , lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real,lt_string: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_stringList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_minus(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_minus}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    {$Q-}exit(newIntLiteral (P_intLiteral(LHS)^.val-P_intLiteral (RHS)^.val));{$Q+}
          lt_real:   exit(newRealLiteral(P_intLiteral(LHS)^.val-P_realLiteral(RHS)^.val));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.val-P_intLiteral (RHS)^.val));
          lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.val-P_realLiteral(RHS)^.val));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_mult(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_mult}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    {$Q-}exit(newIntLiteral (P_intLiteral(LHS)^.val*P_intLiteral (RHS)^.val));{$Q+}
          lt_real:   exit(newRealLiteral(P_intLiteral(LHS)^.val*P_realLiteral(RHS)^.val));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.val*P_intLiteral (RHS)^.val));
          lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.val*P_realLiteral(RHS)^.val));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_divReal(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_divReal}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    exit(newRealLiteral(P_intLiteral(LHS)^.val/P_intLiteral (RHS)^.val));
          lt_real:   exit(newRealLiteral(P_intLiteral(LHS)^.val/P_realLiteral(RHS)^.val));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.val/P_intLiteral (RHS)^.val));
          lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.val/P_realLiteral(RHS)^.val));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_divInt(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_divInt}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int: if P_intLiteral(RHS)^.val<>0 then exit(newIntLiteral(P_intLiteral(LHS)^.val div P_intLiteral(RHS)^.val))
                                               else exit(newRealLiteral(Nan));
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_numList,
        lt_intSet ,lt_numSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int: L_x_S_recursion;
          lt_list,lt_intList,
          lt_set ,lt_intSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRhsCases;
          lt_int: L_x_S_recursion;
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_mod(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_mod}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int: if P_intLiteral(RHS)^.val<>0 then exit(newIntLiteral(P_intLiteral(LHS)^.val mod P_intLiteral(RHS)^.val))
                                               else exit(newRealLiteral(Nan));
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_numList,
        lt_intSet ,lt_numSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int: L_x_S_recursion;
          lt_list,lt_intList,
          lt_set ,lt_intSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRhsCases;
          lt_int: L_x_S_recursion;
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_pot(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_pot}
    FUNCTION pot_int_int(x, y: int64): P_scalarLiteral;
      VAR temp: int64;
          tx, rx: T_myFloat;
      begin
        if y>=0 then begin
          {$Q-}
          temp:=1;
          while y>0 do begin
            if odd(y) then temp:=temp*x;
            x:=int64(x)*int64(x);
            y:=y shr 1;
          end;
          {$Q+}
          result:=newIntLiteral(temp);
        end else begin
          rx:=1/x;
          tx:=1;
          y:=-y;
          while y>0 do begin
            if odd(y) then tx:=tx*rx;
            rx:=rx*rx;
            y:=y shr 1;
          end;
          result:=newRealLiteral(tx);
        end;
      end;

    FUNCTION pot_real_int(x: T_myFloat; y: longint): T_myFloat;
      begin
        if y<0 then begin
          y:=-y;
          x:=1/x;
        end;
        result:=1;
        while y>0 do begin
          if odd(y) then result:=result*x;
          x:=x*x;
          y:=y shr 1;
        end;
      end;

    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    exit(pot_int_int(P_intLiteral(LHS)^.val,P_intLiteral (RHS)^.val));
          lt_real:   exit(newRealLiteral(exp(ln(P_intLiteral(LHS)^.val)*P_realLiteral(RHS)^.val)));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRhsCases;
          lt_int:    exit(newRealLiteral(pot_real_int(P_realLiteral(LHS)^.val,P_intLiteral(RHS)^.val)));
          lt_real:   exit(newRealLiteral(exp(ln(P_realLiteral(LHS)^.val)*P_realLiteral(RHS)^.val)));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRhsCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_strConcat(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_strConcat}
    VAR i,j:longint;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean..lt_string: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean..lt_string: exit(newStringLiteral(P_scalarLiteral(LHS)^.stringForm+P_scalarLiteral(RHS)^.stringForm));
          lt_list..lt_emptySet:  S_x_L_recursion;
        end;
        lt_list..lt_emptySet: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean..lt_string: L_x_S_recursion;
          lt_list..lt_emptySet:  L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_concat(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean..lt_string: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean..lt_string: exit(newListLiteral(2)^
                                      .append(LHS,true)^
                                      .append(RHS,true));
          lt_list..lt_emptyList: exit(newListLiteral(P_listLiteral(RHS)^.size+1)^
                                      .append   (LHS,true)^
                                      .appendAll(P_listLiteral(RHS)));
          lt_set ..lt_emptySet : exit(newSetLiteral^
                                      .append   (LHS,true)^
                                      .appendAll(P_setLiteral(RHS)));
        end;
        lt_list..lt_emptyList: case RHS^.literalType of
          defaultRhsCases;
          lt_boolean..lt_string: exit(newListLiteral(P_listLiteral(LHS)^.size+1)^
                                      .appendAll(P_listLiteral(LHS))^
                                      .append(RHS,true));
          lt_list..lt_emptyMap:  exit(newListLiteral(P_listLiteral(LHS)^.size+P_listLiteral(RHS)^.size)^
                                      .appendAll(P_listLiteral(LHS))^
                                      .appendAll(P_compoundLiteral(RHS)));
        end;
        lt_set ..lt_emptySet : case RHS^.literalType of
          defaultRhsCases;
          lt_boolean..lt_string: exit(newSetLiteral^
                                      .appendAll(P_setLiteral(LHS))^
                                      .append(RHS,true));
          lt_list..lt_emptyList: exit(newListLiteral^
                                      .appendAll(P_setLiteral (LHS))^
                                      .appendAll(P_listLiteral(RHS)));
          lt_set ..lt_emptyMap : exit(newSetLiteral^
                                      .appendAll(P_setLiteral     (LHS))^
                                      .appendAll(P_compoundLiteral(RHS)));
        end;
        lt_map..lt_emptyMap: case RHS^.literalType of
          defaultRhsCases;
          lt_map..lt_emptyMap:  exit(newMapLiteral^
                                     .putAll(P_mapLiteral(LHS))^
                                     .putAll(P_mapLiteral(RHS)));
          lt_list..lt_emptySet: exit(P_collectionLiteral(RHS)^.newOfSameType^
                                     .appendAll(P_mapLiteral(LHS))^
                                     .appendAll(P_setLiteral(RHS)));
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  begin
    case op of
      tt_comparatorEq,
      tt_comparatorNeq,
      tt_comparatorLeq,
      tt_comparatorGeq,
      tt_comparatorLss,
      tt_comparatorGrt:    result:=perform_comparator(LHS,RHS);
      tt_operatorIn,
      tt_comparatorListEq: exit(newBoolLiteral(LHS^.isInRelationTo(op,RHS)));
      tt_operatorAnd,
      tt_operatorLazyAnd:  result:=perform_and      (LHS,RHS);
      tt_operatorOr,
      tt_operatorLazyOr:   result:=perform_or       (LHS,RHS);
      tt_operatorXor:      result:=perform_xor      (LHS,RHS);
      tt_operatorPlus:     result:=perform_plus     (LHS,RHS);
      tt_operatorMinus:    result:=perform_minus    (LHS,RHS);
      tt_operatorMult:     result:=perform_mult     (LHS,RHS);
      tt_operatorDivReal:  result:=perform_divReal  (LHS,RHS);
      tt_operatorDivInt:   result:=perform_divInt   (LHS,RHS);
      tt_operatorMod:      result:=perform_mod      (LHS,RHS);
      tt_operatorPot:      result:=perform_pot      (LHS,RHS);
      tt_operatorStrConcat:result:=perform_strConcat(LHS,RHS);
      tt_operatorConcat:   result:=perform_concat   (LHS,RHS);
      tt_operatorOrElse:   if LHS^.literalType=lt_void
                           then exit(RHS^.rereferenced)
                           else exit(LHS^.rereferenced);
      else begin
        adapters.raiseError('Invalid operator '+C_tokenInfo[op].defaultId,tokenLocation);
        exit(errLit.rereferenced);
      end;
    end;
    if (result^.literalType in C_compoundTypes) and (P_compoundLiteral(result)^.containsError) then begin
      disposeLiteral(result);
      result:=defaultErrorLiteral;
    end;
  end;

FUNCTION setUnion(CONST params:P_listLiteral):P_setLiteral;
  VAR i:longint;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^[i]^.literalType in C_compoundTypes) then exit(nil);
    if params^.size=1 then exit(P_compoundLiteral(params^[0])^.toSet());
    result:=newSetLiteral;
    for i:=0 to params^.size-1 do result^.appendAll(P_compoundLiteral(params^[i]));
  end;

FUNCTION setIntersect(CONST params:P_listLiteral):P_setLiteral;
  TYPE T_occurenceCount=specialize G_literalKeyMap<longint>;
  VAR counterSet:T_occurenceCount;
      prevInt, //dummy
      i,j:longint;
      entry:T_occurenceCount.CACHE_ENTRY;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^[i]^.literalType in C_compoundTypes) then exit(nil);
    if params^.size=1 then exit(P_compoundLiteral(params^[0])^.toSet());

    counterSet.create;
    for i:=0 to params^.size-1 do with P_compoundLiteral(params^[i])^ do
      for j:=0 to size-1 do counterSet.putNew(value[j],counterSet.get(value[j],0)+1,prevInt);
    result:=newSetLiteral;
    i:=params^.size;
    for entry in counterSet.keyValueList do if (entry.value=i) then result^.append(entry.key,true);
    counterSet.destroy;
  end;

FUNCTION setMinus(CONST params:P_listLiteral):P_setLiteral;
  VAR i:longint;
      LHS,RHS:P_compoundLiteral;
      L:P_literal;
  begin
    if not((params<>nil) and
           (params^.size=2) and
           (params^[0]^.literalType in C_compoundTypes) and
           (params^[1]^.literalType in C_compoundTypes))
    then exit(nil);
    LHS:=P_compoundLiteral(params^[0]);
    RHS:=P_compoundLiteral(params^[1]);
    result:=newSetLiteral;
    for i:=0 to LHS^.size-1 do result^.dat.put (LHS^[i],true);
    for i:=0 to RHS^.size-1 do result^.dat.drop(RHS^[i]);
    for L in result^.dat.keySet do begin
      L^.rereference;
      result^.modifyType(L);
    end;
  end;

CONSTRUCTOR T_format.create(CONST formatString: ansistring);
  begin
    if length(formatString)>0 then case formatString[length(formatString)] of
      'd','D': begin
        category:=fmtCat_decimal;
        intFmt :=formatString;
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      'e','E': begin
        category:=fmtCat_scientific;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'f','F': begin
        category:=fmtCat_fixedPoint;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'g','G': begin
        category:=fmtCat_general;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'm','M': begin
        category:=fmtCat_currency;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'n','N': begin
        category:=fmtCat_number;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      's','S': begin
        category:=fmtCat_string;
        strFmt :=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      'x','X': begin
        category:=fmtCat_hex;
        intFmt :=formatString;
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      else begin
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
    end;
  end;

PROCEDURE T_format.formatAppend(VAR txt:ansistring; CONST l:P_literal);
  begin
    case category of
      fmtCat_scientific, fmtCat_fixedPoint, fmtCat_general, fmtCat_currency, fmtCat_number: case l^.literalType of
        lt_real: begin txt:=txt+sysutils.format(realFmt,[P_realLiteral(l)^.val]); exit; end;
        lt_int : begin txt:=txt+sysutils.format(realFmt,[extended(P_intLiteral(l)^.val)]); exit; end;
      end;
      fmtCat_decimal, fmtCat_hex:
      if l^.literalType=lt_int then begin
        txt:=txt+sysutils.format(intFmt,[P_intLiteral(l)^.val]);
        exit;
      end;
    end;
    if l^.literalType in C_scalarTypes
    then txt:=txt+sysutils.format(strFmt,[P_scalarLiteral(l)^.stringForm])
    else txt:=txt+sysutils.format(strFmt,[l^.toString]);
  end;

DESTRUCTOR T_format.destroy;
  begin
    intFmt:='';
    realFmt:='';
    strFmt:='';
  end;

FUNCTION newLiteralFromStream(CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
  VAR reusableLiterals:array of P_literal;
      encodingMethod:byte=0;
      {$ifdef debugMode}
      start:double;
      {$endif}
  PROCEDURE errorOrException(CONST message:string);
    begin
      if adapters<>nil then adapters^.raiseError(message,location)
                       else raise Exception.create(message);
    end;

  FUNCTION typeStringOrNone(CONST t:T_literalType):string;
    begin
      if (t>=low(T_literalType)) and (t<=high(T_literalType)) then result:=C_typeString[t] else result:='';
    end;

  FUNCTION literalFromStream255:P_literal;
    VAR literalType:T_literalType;
        reusableIndex:longint;
        literalByte:byte;
        listSize:longint;
        i:longint;
        mapKey,mapValue:P_literal;
    begin
      literalByte:=stream^.readByte;
      if literalByte=255 then begin
        reusableIndex:=stream^.readNaturalNumber;
        if (reusableIndex<length(reusableLiterals)) then begin
          result:=reusableLiterals[reusableIndex];
          result^.rereference;
        end else begin
          result:=newVoidLiteral;
          stream^.logWrongTypeError;
          errorOrException('Read invalid reuse index '+intToStr(reusableIndex)+'! Abort.');
        end;
        exit(result);
      end;
      literalType:=T_literalType(literalByte);
      case literalType of
        lt_boolean:result:=newBoolLiteral  (stream^.readBoolean   );
        lt_int    :result:=newIntLiteral   (stream^.readInt64     );
        lt_real   :result:=newRealLiteral  (stream^.readDouble    );
        lt_string :result:=newStringLiteral(stream^.readAnsiString);
        lt_booleanList,lt_booleanSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral
          else result:=newListLiteral(listSize);
          for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendBool(stream^.readBoolean);
        end;
        lt_intList,lt_intSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral
          else result:=newListLiteral(listSize);
          for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendInt(stream^.readInt64);
        end;
        lt_realList,lt_realSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral
          else result:=newListLiteral(listSize);
          for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendReal(stream^.readDouble);
        end;
        lt_stringList,lt_stringSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral
          else result:=newListLiteral(listSize);
          for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendString(stream^.readAnsiString);
        end;
        lt_emptyList: result:=newListLiteral(0);
        lt_emptySet:  result:=newSetLiteral;
        lt_emptyMap:  result:=newMapLiteral;
        lt_list,lt_set,
        lt_numList,lt_numSet:begin
          listSize:=stream^.readNaturalNumber;
          case literalType of
            lt_set,lt_numSet: result:=newSetLiteral;
            else              result:=newListLiteral(listSize);
          end;
          for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.append(literalFromStream255(),false);
        end;
        lt_map: begin
          listSize:=stream^.readNaturalNumber;
          result:=newMapLiteral;
          for i:=0 to listSize-1 do if stream^.allOkay then begin
            mapKey  :=literalFromStream255();
            mapValue:=literalFromStream255();
            P_mapLiteral(result)^.put(mapKey,mapValue,false);
          end;
        end;
        else begin
          errorOrException('Read invalid literal type '+typeStringOrNone(literalType)+' ('+intToStr(literalByte)+') ! Abort.');
          stream^.logWrongTypeError;
          exit(newVoidLiteral);
        end;
      end;
      if (result^.literalType<>literalType) and (adapters<>nil) then errorOrException('Deserializaion result has other type ('+typeStringOrNone(result^.literalType)+') than expected ('+typeStringOrNone(literalType)+').');
      if not(stream^.allOkay) then errorOrException('Unknown error during deserialization.');
      if ((literalType=lt_string) or (literalType in C_compoundTypes)) and (length(reusableLiterals)<2097151) then begin
        setLength(reusableLiterals,length(reusableLiterals)+1);
        reusableLiterals[length(reusableLiterals)-1]:=result;
      end;
    end;

  begin
    {$ifdef debugMode}start:=now;{$endif}

    setLength(reusableLiterals,0);
    encodingMethod:=stream^.readByte;
    case encodingMethod of
      255: result:=literalFromStream255;
      else begin
        errorOrException('Invalid literal encoding type '+intToStr(encodingMethod));
        result:=newVoidLiteral;
      end;
    end;

    setLength(reusableLiterals,0);
    {$ifdef debugMode}
    writeln(stdErr,'Read literal in ',(now-start)*24*60*60:0:3,'s');
    {$endif}
  end;

PROCEDURE writeLiteralToStream(CONST L:P_literal; CONST stream:P_outputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR reusableMap:specialize G_literalKeyMap<longint>;
      {$ifdef debugMode}
      start:double;
      {$endif}
      previousMapValueDummy:longint;
      mapEntry:T_literalKeyLiteralValueMap.CACHE_ENTRY;

 PROCEDURE writeLiteral(CONST L:P_literal);
    VAR i:longint;
        reusableIndex:longint;
    begin
      reusableIndex:=reusableMap.get(L,2097151);
      if reusableIndex<2097151 then begin
        stream^.writeByte(255);
        stream^.writeNaturalNumber(reusableIndex);
        exit;
      end;
      stream^.writeByte(byte(L^.literalType));
      case L^.literalType of
        lt_boolean:stream^.writeBoolean   (P_boolLiteral  (L)^.val);
        lt_int:    stream^.writeInt64     (P_intLiteral   (L)^.val);
        lt_real:   stream^.writeDouble    (P_realLiteral  (L)^.val);
        lt_string: stream^.writeAnsiString(P_stringLiteral(L)^.val);
        lt_booleanList,lt_booleanSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          for i:=0 to P_compoundLiteral(L)^.size-1 do if (adapters=nil) or (adapters^.noErrors) then stream^.writeBoolean(P_boolLiteral(P_compoundLiteral(L)^.value[i])^.val);
        end;
        lt_intList,lt_intSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          for i:=0 to P_compoundLiteral(L)^.size-1 do if (adapters=nil) or (adapters^.noErrors) then stream^.writeInt64(P_intLiteral(P_compoundLiteral(L)^.value[i])^.val);
        end;
        lt_realList,lt_realSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          for i:=0 to P_compoundLiteral(L)^.size-1 do if (adapters=nil) or (adapters^.noErrors) then stream^.writeDouble(P_realLiteral(P_compoundLiteral(L)^.value[i])^.val);
        end;
        lt_stringList,lt_stringSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          for i:=0 to P_compoundLiteral(L)^.size-1 do if (adapters=nil) or (adapters^.noErrors) then stream^.writeAnsiString(P_stringLiteral(P_compoundLiteral(L)^.value[i])^.val);
        end;
        lt_emptyList,lt_emptySet,lt_emptyMap: begin end; //completely defined by type
        lt_list,lt_set,
        lt_numList,lt_numSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          for i:=0 to P_compoundLiteral(L)^.size-1 do if (adapters=nil) or (adapters^.noErrors) then writeLiteral(P_compoundLiteral(L)^.value[i]);
        end;
        lt_map: begin
          stream^.writeNaturalNumber(P_mapLiteral(L)^.size);
          for mapEntry in P_mapLiteral(L)^.dat.keyValueList do begin
            writeLiteral(mapEntry.key);
            writeLiteral(mapEntry.value);
          end;
        end
        else begin
          if adapters<>nil then adapters^.raiseError  ('Cannot represent '+L^.typeString+' literal in binary form!',location)
                           else raise Exception.create('Cannot represent '+L^.typeString+' literal in binary form!');
        end;
      end;
      if (reusableMap.fill<2097151) and ((L^.literalType=lt_string) or (L^.literalType in C_compoundTypes)) then
        reusableMap.putNew(L,reusableMap.fill,previousMapValueDummy);
    end;

  begin
    {$ifdef debugMode}start:=now;{$endif}
    reusableMap.create();
    stream^.writeByte(255);
    writeLiteral(L);
    reusableMap.destroy;
    {$ifdef debugMode}
    writeln(stdErr,'Wrote literal in ',(now-start)*24*60*60:0:3,'s');
    {$endif}
  end;

FUNCTION serialize(CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters):ansistring;
  FUNCTION representReal(CONST realValue:T_myFloat):ansistring;
    CONST p52:int64=1 shl 52;
    VAR r:double;
        bits:bitpacked array[0..sizeOf(double)*8-1] of boolean;
        isNegative:boolean;
        significand,exponent:int64;
    begin
      //ensure representation as IEEE745 double
      result:=myFloatToStr(realValue);
      if (isNan(realValue) or isInfinite(realValue) or (realValue=strToFloatDef(result,Nan))) then exit(result);

      r:=realValue;
      {$WARN 5057 OFF}
      move(r,bits,sizeOf(double));
      isNegative:=bits[length(bits)-1];
      bits[length(bits)-1]:=false;
      move(bits,significand,min(sizeOf(double),sizeOf(int64)));
      exponent:=significand;
      significand:=p52+(significand and (p52-1));
      exponent:=(exponent shr 52)-1023-52;

      if isNegative then result:='-' else result:='';
      result:=result+intToStr(significand);
      if exponent<0 then result:=result+'*2^'  +intToStr(exponent)
                    else result:=result+'*2.0^'+intToStr(exponent);
      {$WARN 5057 ON}
    end;

  VAR i:longint;
  begin
    case L^.literalType of
      lt_int, lt_boolean, lt_string, lt_expression: result:=L^.toString;
      lt_real: result:=representReal(P_realLiteral(L)^.val);
      lt_list..lt_emptyMap: begin
        result:='[';
        if (P_compoundLiteral(L)^.size>0)      then result:=result+    serialize(P_compoundLiteral(L)^.value[0],location,adapters);
        for i:=1 to P_compoundLiteral(L)^.size-1 do result:=result+','+serialize(P_compoundLiteral(L)^.value[i],location,adapters);
        result:=result+']';
      end;
      else begin
        adapters^.raiseError('Literal of type '+L^.typeString+' ('+L^.toString+') cannot be serialized',location);
        exit('');
      end;
    end;
  end;

FUNCTION serialize(CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST asExpression:boolean):ansistring;
  VAR wrapper:T_outputStreamWrapper;
      stream:TStringStream;
  begin
    if asExpression then exit(serialize(L,location,adapters));
    stream:= TStringStream.create('');
    wrapper.create(stream);
    writeLiteralToStream(L,@wrapper,location,adapters);
    stream.position:=0;
    result:=stream.DataString;
    wrapper.destroy; //implicitly destroys stream
  end;

FUNCTION deserialize(CONST source:ansistring; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
  VAR wrapper:T_inputStreamWrapper;
      stream:TStringStream;
  begin
    stream:=TStringStream.create(source);
    wrapper.create(stream);
    stream.position:=0;
    result:=newLiteralFromStream(@wrapper,location,adapters);
    wrapper.destroy; //implicitly destroys stream
  end;

VAR i: longint;

INITIALIZATION
  errLit.init(lt_error);
  boolLit[false].create(false);
  boolLit[true ].create(true);
  voidLit.create();
  emptyStringSingleton.create('');
  for i:=low(intLit) to high(intLit) do intLit[i].create(i);
  for i:=0 to 255 do charLit[chr(i)].create(chr(i));
  DefaultFormatSettings.DecimalSeparator:='.';
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  randomize;

FINALIZATION
  errLit.destroy;
  boolLit[false].destroy;
  boolLit[true ].destroy;
  voidLit.destroy;
  emptyStringSingleton.destroy;
  for i:=low(intLit) to high(intLit) do intLit[i].destroy;
  for i:=0 to 255 do charLit[chr(i)].destroy;
end.
