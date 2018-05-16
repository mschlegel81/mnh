UNIT mnh_litVar;
{$Q-}
INTERFACE
USES sysutils, math, typinfo,
     Classes,LazUTF8,
     myGenerics, myStringUtil, serializationUtil, bigint,
     mnh_constants,
     mnh_basicTypes,
     mnh_out_adapters;

CONST HASH_GROWTH_THRESHOLD_FACTOR=2;
      HASH_SHRINK_THRESHOLD_FACTOR=0.5;
TYPE T_expressionType=(et_builtin          ,
                       et_builtinIteratable,
                       et_builtinFuture    ,
                       et_subrule          ,
                       et_inline           ,
                       et_subruleIteratable,
                       et_inlineIteratable ,
                       et_subruleStateful  ,
                       et_inlineStateful   ,
                       et_eachBody         ,
                       et_whileBody        );

CONST C_builtinExpressionTypes:set of T_expressionType=[et_builtin,et_builtinIteratable,et_builtinFuture];
      C_subruleExpressionTypes:set of T_expressionType=[et_subrule,et_subruleIteratable,et_subruleStateful];
      C_inlineExpressionTypes:set of T_expressionType =[et_inline,et_inlineIteratable,et_inlineStateful];
      C_statefulExpressionTypes:set of T_expressionType=[et_builtinIteratable,et_builtinFuture,
                                                         et_subruleIteratable,et_subruleStateful,
                                                         et_inlineIteratable ,et_inlineStateful];
      C_iteratableExpressionTypes:set of T_expressionType=[et_builtinIteratable,
                                                           et_subruleIteratable,
                                                           et_inlineIteratable];
      C_expressionTypeString:array[T_expressionType] of string=(
        'builtin',
        'builtin iteratable',
        'builtin future',
        'subrule',
        'inline',
        'subrule iteratable',
        'inline iteratable',
        'subrule stateful',
        'inline stateful',
        'eachBody',
        'whileBody');

TYPE
  P_typedef=^T_typedef;
  P_literal = ^T_literal;
  PP_literal = ^P_literal;
  P_setOfPointer=^T_setOfPointer;
  T_arrayOfLiteral=array of P_literal;
  T_literal = object(T_objectWithIdAndLocation)
  private
    numberOfReferences: longint;
  public
    customType:P_typedef;
    literalType:T_literalType;
    CONSTRUCTOR init(CONST lt:T_literalType);
    DESTRUCTOR destroy; virtual;
    PROCEDURE rereference; inline;
    FUNCTION rereferenced:P_literal; inline;
    FUNCTION unreference: longint; inline;
    PROPERTY getReferenceCount: longint read numberOfReferences;

    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION typeString:string; virtual;

    FUNCTION getId:T_idString; virtual;
    FUNCTION getLocation:T_tokenLocation; virtual;
  end;

  P_voidLiteral = ^T_voidLiteral;
  T_voidLiteral = object(T_literal)
    private
      CONSTRUCTOR create();
    public
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
  end;

  P_boolLiteral = ^T_boolLiteral;
  T_boolLiteral = object(T_literal)
  private
    val: boolean;
    CONSTRUCTOR create(CONST value: boolean);
  public
    PROPERTY value:boolean read val;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_intLiteral = ^T_intLiteral;
  T_intLiteral = object(T_literal)
  private
    val: T_bigInt;
    CONSTRUCTOR create(CONST value: T_bigInt);
    CONSTRUCTOR create(CONST value: int64);
    FUNCTION clone:P_intLiteral;
  public
    DESTRUCTOR destroy; virtual;
    PROPERTY value:T_bigInt read val;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_realLiteral = ^T_realLiteral;
  T_realLiteral = object(T_literal)
  private
    val: T_myFloat;
    CONSTRUCTOR create(CONST value: T_myFloat);
  public
    PROPERTY value:T_myFloat read val;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_stringLiteral = ^T_stringLiteral;
  T_stringLiteral = object(T_literal)
  private
    enc: T_stringEncoding;
    val: ansistring;
    CONSTRUCTOR create(CONST value: ansistring);
  public
    DESTRUCTOR destroy; virtual;
    PROPERTY value:ansistring read val;
    FUNCTION softCast: P_literal;
    FUNCTION trim: P_stringLiteral;
    FUNCTION trimLeft: P_stringLiteral;
    FUNCTION trimRight: P_stringLiteral;
    FUNCTION upper: P_stringLiteral;
    FUNCTION lower: P_stringLiteral;
    FUNCTION unbrace: P_stringLiteral;
    FUNCTION escape: P_stringLiteral;
    FUNCTION getEncoding: T_stringEncoding;
    PROCEDURE append(CONST suffix:ansistring);
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  T_evaluationResult=record
    literal:P_literal;
    triggeredByReturn:boolean;
  end;

  P_compoundLiteral  = ^T_compoundLiteral;
  P_listLiteral      = ^T_listLiteral    ;
  P_setLiteral       = ^T_setLiteral     ;
  P_mapLiteral       = ^T_mapLiteral     ;
  P_expressionLiteral = ^T_expressionLiteral;
  T_expressionList = array of P_expressionLiteral;
  T_expressionLiteral = object(T_literal)
    private
      expressionType:T_expressionType;
      declaredAt:T_tokenLocation;
      myHash:T_hashInt;
    public
      CONSTRUCTOR create(CONST eType:T_expressionType; CONST location:T_tokenLocation);
      PROPERTY typ:T_expressionType read expressionType;
      FUNCTION evaluateToBoolean(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):boolean;            virtual; abstract;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual; abstract;
      FUNCTION evaluate         (CONST location:T_tokenLocation; CONST context:pointer; CONST parameters:P_listLiteral):T_evaluationResult;               virtual; abstract;
      FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; CONST threadContext:pointer):P_expressionLiteral; virtual; abstract;
      FUNCTION arity:longint; virtual; abstract;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual; abstract;

      PROCEDURE makeStateful  (CONST adapters:P_adapters; CONST location:T_tokenLocation);
      PROCEDURE makeIteratable(CONST adapters:P_adapters; CONST location:T_tokenLocation);

      FUNCTION getParentId:T_idString; virtual; abstract;
      PROCEDURE validateSerializability(CONST adapters:P_adapters); virtual; abstract;
      FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal; virtual;
      FUNCTION typeString:string; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION getLocation:T_tokenLocation; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
      FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
      FUNCTION clone(CONST location:T_tokenLocation; CONST context:pointer):P_expressionLiteral; virtual; abstract;
  end;

  T_typedef=object
    private
      name:T_idString;
      super:P_typedef;
      builtinsuper:T_literalTypeSet;
      ducktyperule:P_expressionLiteral;
      ducktyping:boolean;
      FUNCTION cloneLiteral(CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:pointer; CONST forUncasting:boolean):P_literal;
    public
      CONSTRUCTOR create(CONST id:T_idString; CONST builtinType:T_literalTypeSet; CONST super_:P_typedef; CONST typerule:P_expressionLiteral; CONST ducktyping_:boolean);
      DESTRUCTOR destroy;
      FUNCTION matchesLiteral(CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:pointer):boolean;
      FUNCTION cast(CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:pointer; CONST adapters:P_adapters):P_literal;
      FUNCTION uncast(CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:pointer; CONST adapters:P_adapters):P_literal;
      PROPERTY getName:T_idString read name;
      PROPERTY getSuper:P_typedef read super;
      PROPERTY getWhitelist:T_literalTypeSet read builtinsuper;
      PROPERTY isDucktyping:boolean read ducktyping;
  end;

  generic G_literalKeyMap<VALUE_TYPE>= object
    TYPE CACHE_ENTRY=record
           key:P_literal;
           keyHash:T_hashInt;
           value:VALUE_TYPE;
         end;
         P_CACHE_ENTRY=^CACHE_ENTRY;
         KEY_VALUE_LIST=array of CACHE_ENTRY;
         MY_TYPE=specialize G_literalKeyMap<VALUE_TYPE>;
    VAR dat:array of KEY_VALUE_LIST;
        fill:longint;
    CONSTRUCTOR create();
    CONSTRUCTOR createClone(VAR map:MY_TYPE);
    DESTRUCTOR destroy;
    PROCEDURE rehash(CONST grow:boolean);
    PROCEDURE put(CONST key:P_literal; CONST value:VALUE_TYPE);
    PROCEDURE rehashForExpectedSize(CONST expectedFill:longint);
    FUNCTION putNew(CONST entry:CACHE_ENTRY; OUT previousValue:VALUE_TYPE):boolean;
    FUNCTION putNew(CONST key:P_literal; CONST value:VALUE_TYPE; OUT previousValue:VALUE_TYPE):boolean;
    FUNCTION get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
    FUNCTION getEntry(CONST key:P_literal):P_CACHE_ENTRY;
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
      myHash:T_hashInt;
    public
    containsError:boolean;
    FUNCTION toSet :P_setLiteral;
    FUNCTION toList:P_listLiteral;
    FUNCTION toMap(CONST location:T_tokenLocation; VAR adapters:T_adapters):P_mapLiteral;
    FUNCTION get     (CONST accessor:P_literal):P_literal; virtual; abstract;
    FUNCTION getInner(CONST accessor:P_literal):P_literal; virtual; abstract;
    FUNCTION typeString:string; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION size:longint;                        virtual; abstract;
    FUNCTION contains(CONST L:P_literal):boolean; virtual; abstract;
    FUNCTION clone:P_compoundLiteral; virtual; abstract;
    FUNCTION iteratableList:T_arrayOfLiteral; virtual; abstract;
  end;

  P_collectionLiteral=^T_collectionLiteral;
  T_collectionLiteral=object(T_compoundLiteral)
    private
      ints,reals,strings,booleans,others:longint;
    public
    FUNCTION isKeyValueCollection:boolean; virtual; abstract;
    FUNCTION newOfSameType(CONST initSize:boolean):P_collectionLiteral; virtual; abstract;
    FUNCTION appendAll   (CONST L:P_compoundLiteral                ):P_collectionLiteral; virtual;
    FUNCTION append      (CONST L:P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false):P_collectionLiteral; virtual; abstract;
    FUNCTION appendString(CONST s:ansistring):P_collectionLiteral;
    FUNCTION appendBool  (CONST b:boolean   ):P_collectionLiteral;
    FUNCTION appendInt   (CONST i:int64     ):P_collectionLiteral;
    FUNCTION appendReal  (CONST r:T_myFloat ):P_collectionLiteral;
  end;

  T_listLiteral=object(T_collectionLiteral)
    private
      dat:PP_literal;
      alloc,fill:longint;
      PROCEDURE modifyType(CONST L:P_literal); inline;
    public
      PROPERTY value:PP_literal read dat;
      CONSTRUCTOR create(CONST initialSize:longint);
      DESTRUCTOR destroy; virtual;
      FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION equals(CONST other: P_literal): boolean; virtual;
      FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal; virtual;
      FUNCTION isKeyValuePair:boolean;
      FUNCTION isKeyValueCollection:boolean; virtual;
      FUNCTION newOfSameType(CONST initSize:boolean):P_collectionLiteral; virtual;
      FUNCTION size:longint;        virtual;
      FUNCTION contains(CONST other:P_literal):boolean; virtual;
      FUNCTION listConstructorToString(CONST lengthLimit:longint=maxLongint):string;
      FUNCTION get     (CONST accessor:P_literal):P_literal; virtual;
      FUNCTION getInner(CONST accessor:P_literal):P_literal; virtual;
      FUNCTION appendConstructing(CONST L: P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST doRangeAppend:boolean):P_compoundLiteral;
      FUNCTION append(CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false):P_collectionLiteral; virtual;
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
      PROCEDURE removeElement(CONST index:longint);
    end;

  T_setLiteral=object(T_collectionLiteral)
    private
      dat:T_literalKeyBooleanValueMap;
      CONSTRUCTOR create;
      PROCEDURE modifyType(CONST L:P_literal); inline;
    public
      DESTRUCTOR destroy; virtual;
      FUNCTION isKeyValueCollection:boolean; virtual;
      FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION equals(CONST other: P_literal): boolean; virtual;
      FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal; virtual;
      FUNCTION newOfSameType(CONST initSize:boolean):P_collectionLiteral; virtual;
      FUNCTION size:longint;        virtual;
      FUNCTION contains(CONST other:P_literal):boolean; virtual;
      FUNCTION get     (CONST accessor:P_literal):P_literal; virtual;
      FUNCTION getInner(CONST accessor:P_literal):P_literal; virtual;
      FUNCTION append(CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false):P_collectionLiteral; virtual;
      FUNCTION appendAll(CONST L:P_compoundLiteral              ):P_collectionLiteral; virtual;
      FUNCTION clone:P_compoundLiteral; virtual;
      FUNCTION iteratableList:T_arrayOfLiteral; virtual;
      PROCEDURE drop(CONST L:P_literal);
    end;

  T_mapLiteral=object(T_compoundLiteral)
    private
      dat:T_literalKeyLiteralValueMap;
      CONSTRUCTOR create;
    public
      DESTRUCTOR destroy; virtual;
      FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION equals(CONST other: P_literal): boolean; virtual;
      FUNCTION size:longint;        virtual;
      FUNCTION contains(CONST other:P_literal):boolean; virtual;
      FUNCTION get     (CONST accessor:P_literal):P_literal; virtual;
      FUNCTION getInner(CONST accessor:P_literal):P_literal; virtual;
      FUNCTION clone:P_compoundLiteral; virtual;
      FUNCTION iteratableList:T_arrayOfLiteral; virtual;
      FUNCTION keyIteratableList:T_arrayOfLiteral;

      PROCEDURE drop(CONST L:P_literal);
      FUNCTION put(CONST key,                  newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
      FUNCTION put(CONST key,                  newValue:ansistring                      ):P_mapLiteral;
      FUNCTION put(CONST key:ansistring; CONST newValue:int64                           ):P_mapLiteral;
      FUNCTION put(CONST key:ansistring; CONST newValue:T_myFloat                       ):P_mapLiteral;
      FUNCTION put(CONST key:ansistring; CONST newValue:boolean                         ):P_mapLiteral;
      FUNCTION put(CONST key:ansistring; CONST newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
      FUNCTION put(CONST key:P_literal;  CONST newValue:int64    ; CONST incRefs:boolean):P_mapLiteral;
      FUNCTION putAll(CONST map:P_mapLiteral):P_mapLiteral;
  end;

CONST
  NIL_EVAL_RESULT:T_evaluationResult=(literal:nil; triggeredByReturn:false);

VAR
  subruleApplyOpCallback: FUNCTION(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST threadContext:pointer):P_literal;
  resolveOperatorCallback: FUNCTION (CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; CONST threadContext:pointer): P_literal;
FUNCTION exp(CONST x:double):double; inline;

PROCEDURE disposeLiteral(VAR l: P_literal); {$ifndef debugMode} inline; {$endif}
PROCEDURE disposeLiteral(VAR l: T_arrayOfLiteral); inline;
FUNCTION newBoolLiteral  (CONST value: boolean       ): P_boolLiteral;       inline;
FUNCTION newIntLiteral(value: T_bigInt): P_intLiteral;
FUNCTION newIntLiteral   (CONST value: int64         ): P_intLiteral;        inline;
FUNCTION newRealLiteral  (CONST value: T_myFloat     ): P_realLiteral;       inline;
FUNCTION newStringLiteral(CONST value: ansistring; CONST enforceNewString:boolean=false): P_stringLiteral;     inline;
FUNCTION newSingletonString(CONST value: ansistring): P_stringLiteral;     inline;
FUNCTION newListLiteral  (CONST initialSize:longint=2): P_listLiteral;       inline;
FUNCTION newListLiteral  (CONST a:P_literal;
                          CONST b:P_literal=nil)      : P_listLiteral; inline;
FUNCTION newSetLiteral(CONST expectedSize:longint=0)  : P_setLiteral;        inline;
FUNCTION newMapLiteral                                : P_mapLiteral;        inline;
FUNCTION newVoidLiteral                               : P_voidLiteral; inline;

FUNCTION parseNumber(CONST input: ansistring; CONST offset:longint; CONST suppressOutput: boolean; OUT parsedLength: longint): P_literal; inline;

FUNCTION messagesToLiteralForSandbox(CONST messages:T_storedMessages):P_listLiteral;

FUNCTION newLiteralFromStream(CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
PROCEDURE writeLiteralToStream(CONST L:P_literal; CONST stream:P_outputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters);
FUNCTION serializeToStringList(CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST maxLineLength:longint=128):T_arrayOfString;

FUNCTION serialize(CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters):ansistring;
FUNCTION deserialize(CONST source:ansistring; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
FUNCTION toParameterListString(CONST list:P_listLiteral; CONST isFinalized: boolean; CONST lengthLimit:longint=maxLongint): ansistring;
FUNCTION parameterListTypeString(CONST list:P_listLiteral):string;

FUNCTION setUnion    (CONST params:P_listLiteral):P_setLiteral;
FUNCTION setIntersect(CONST params:P_listLiteral):P_setLiteral;
FUNCTION setMinus    (CONST params:P_listLiteral):P_setLiteral;
FUNCTION mapMerge    (CONST params:P_listLiteral; CONST location:T_tokenLocation; CONST contextPointer:pointer):P_mapLiteral;
FUNCTION mutateVariable(VAR toMutate:P_literal; CONST mutation:T_tokenType; CONST parameters:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer):P_literal;

VAR emptyStringSingleton: T_stringLiteral;
VAR boolLit       : array[false..true] of T_boolLiteral;
    charLit       : array[#0..#255] of T_stringLiteral;
CONST maxSingletonInt=4000;
IMPLEMENTATION
VAR
  intLit : array[-100..maxSingletonInt] of T_intLiteral;
  voidLit: T_voidLiteral;
  stringSingletons:specialize G_stringKeyMap<P_literal>;
  singletonCs:TRTLCriticalSection;

FUNCTION messagesToLiteralForSandbox(CONST messages:T_storedMessages):P_listLiteral;
  FUNCTION headByMessageType(CONST messageType:T_messageType):P_collectionLiteral;
    begin
      if C_messageTypeMeta[messageType].prefix=''
      then result:=newListLiteral(3)^.appendString(copy(getEnumName(TypeInfo(messageType),ord(messageType)),4,1000))
      else result:=newListLiteral(3)^.appendString(trim(C_messageTypeMeta[messageType].prefix));
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

FUNCTION newIntLiteral(value: T_bigInt): P_intLiteral;
  VAR iv:int64;
  begin
    if value.isBetween(low(intLit),high(intLit)) then begin
      iv:=value.toInt;
      value.destroy;
      exit(P_intLiteral(intLit[iv].rereferenced));
    end;
    new(result,create(value));
  end;

FUNCTION newIntLiteral(CONST value: int64): P_intLiteral;
  begin
    if (value>=low(intLit)) and (value<=high(intLit))
    then result:=P_intLiteral(intLit[value].rereferenced)
    else new(result, create(value));
  end;

FUNCTION newStringLiteral(CONST value: ansistring; CONST enforceNewString:boolean=false): P_stringLiteral;
  begin
    if (length(value)<=1) and not(enforceNewString) then begin
      if length(value)=1 then result:=P_stringLiteral(charLit[value[1]]   .rereferenced)
                         else result:=P_stringLiteral(emptyStringSingleton.rereferenced);
    end else new(result, create(value));
  end;

FUNCTION newSingletonString(CONST value: ansistring): P_stringLiteral;     inline;
  VAR r:P_literal;
  begin
    enterCriticalSection(singletonCs);
    if stringSingletons.containsKey(value,r) then begin
      leaveCriticalSection(singletonCs);
      exit(P_stringLiteral(r^.rereferenced));
    end;
    new(result,create(value));
    result^.rereference;
    stringSingletons.put(value,result);
    leaveCriticalSection(singletonCs);
  end;

FUNCTION newRealLiteral(CONST value: T_myFloat)     : P_realLiteral;       begin new(result,create(value));       end;
FUNCTION newListLiteral(CONST initialSize:longint=2): P_listLiteral;       begin new(result,create(initialSize)); end;
FUNCTION newListLiteral(CONST a:P_literal; CONST b:P_literal=nil): P_listLiteral;
  VAR initialSize:longint=2;
  begin
    if b=nil then dec(initialSize);
    new(result,create(initialSize));
                   result^.append(a,true);
    if b<>nil then result^.append(b,true);
  end;

FUNCTION newSetLiteral(CONST expectedSize:longint=0): P_setLiteral;        begin new(result,create); if expectedSize>0 then result^.dat.rehashForExpectedSize(expectedSize); end;
FUNCTION newMapLiteral                              : P_mapLiteral;        begin new(result,create);              end;
FUNCTION newVoidLiteral                             : P_voidLiteral;       begin result:=P_voidLiteral(voidLit       .rereferenced); end;
FUNCTION newBoolLiteral(CONST value: boolean)       : P_boolLiteral;       begin result:=P_boolLiteral(boolLit[value].rereferenced); end;

FUNCTION myFloatToStr(CONST x: T_myFloat): string;
  FUNCTION exponentRepresentation:shortString;
    VAR ePos:longint;
        exponentPart:shortString;
    begin
      str(x,result);
      result:=trim(result);
      ePos:=pos('E',result);
      if ePos<=0 then exit(result);
      //Split:
      exponentPart:=copy(result,ePos,length(result)+1-ePos);
      result      :=copy(result,1,ePos);
      //shorten exponentPart:
      ePos:=1;
      if exponentPart[2]='+' then begin
        move(exponentPart[3],exponentPart[2],6);
        dec(exponentPart[0]);
      end;
      while not(exponentPart[ePos] in ['0'..'9']) do inc(ePos);
      while exponentPart[ePos]='0' do begin
        move(exponentPart[ePos+1],exponentPart[ePos],5);
        dec(exponentPart[0]);
      end;
      //shorten significand part:
      while (strToFloatDef(copy(result,1,ord(result[0])-1)+exponentPart,Nan)=x) do dec(result[0]);
      //compose:
      result:=result+exponentPart;
    end;

  FUNCTION simpleRepresentation:shortString;
    begin
      str(x:50:50,result);
      result:=trim(result);
      while (result[ord(result[0])]<>'.')
        and ((strToFloatDef(copy(result,1,ord(result[0])-1),Nan)=x)) do dec(result[0]);
    end;

  VAR altRes:shortString;
  begin
    //Special representation for special values:
    if isNan(x) then exit(LITERAL_NAN_TEXT);
    if isInfinite(x) then begin
      if x>0 then exit(    LITERAL_INF_TEXT)
             else exit('-'+LITERAL_INF_TEXT);
    end;
    //Default representation (preferred if accurate)
    result:=floatToStr(x);
    if strToFloatDef(result,Nan)=x then begin
      if (pos('.', result)<=0) then result:=result+'.0';
      exit(result);
    end;
    //alternative representations
    if (abs(x)>1E15) then exit(exponentRepresentation);
    if (abs(x)<1E9) and (abs(x)>1E-9) then exit(simpleRepresentation);
    result:=simpleRepresentation;
    altRes:=exponentRepresentation;
    if length(altRes)<length(result) then exit(altRes);
  end;

FUNCTION parseNumber(CONST input: ansistring; CONST offset:longint; CONST suppressOutput: boolean; OUT parsedLength: longint): P_literal;
  VAR i: longint;
      allZeroes:boolean=true;
      atLeastOneDigit:boolean=false;
      big:T_bigInt;
      intResult:int64;
  begin
    result:=nil;
    parsedLength:=0;
    if (length(input)>=offset) and (input [offset] in ['0'..'9', '-', '+']) then begin
      i:=offset;
      allZeroes:=input[offset] in ['+','-','0'];
      atLeastOneDigit:=input[offset] in ['0'..'9'];
      while (i<length(input)) and (input [i+1] in ['0'..'9']) do begin
        inc(i);
        atLeastOneDigit:=true;
        allZeroes:=allZeroes and (input[i]='0');
      end;
      if atLeastOneDigit then parsedLength:=i+1-offset
                         else exit(nil);
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
        intResult:=StrToInt64Def(copy(input, offset, parsedLength), 0);
        if (intResult=0) and not(allZeroes) then begin
          big.fromString(copy(input, offset, parsedLength));
          result:=newIntLiteral(big);
        end else result:=newIntLiteral(intResult);
      end;
    end;
  end;

CONSTRUCTOR T_typedef.create(CONST id: T_idString; CONST builtinType:T_literalTypeSet; CONST super_: P_typedef; CONST typerule: P_expressionLiteral; CONST ducktyping_:boolean);
  begin
    name        :=id;
    super       :=super_;
    builtinsuper:=builtinType;
    ducktyperule:=typerule;
    ducktyping  :=ducktyping_;
  end;

DESTRUCTOR T_typedef.destroy;
  begin
    disposeLiteral(ducktyperule);
  end;

FUNCTION T_typedef.matchesLiteral(CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:pointer):boolean;
  VAR t:P_typedef;
  begin
    result:=false;
    if L^.customType<>nil then begin
      t:=@self;
      while(t<>nil) do
      if L^.customType=t
      then exit(true) else t:=t^.super;
    end;
    if ducktyping then result:=ducktyperule^.evaluateToBoolean(location,threadContext,L);
  end;

FUNCTION T_typedef.cloneLiteral(CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:pointer; CONST forUncasting:boolean):P_literal;
  begin
    result:=nil;
    case L^.literalType of
      lt_boolean   : if forUncasting
                     then result:=newBoolLiteral(P_boolLiteral(L)^.val)
                     else new(P_boolLiteral(result),create(P_boolLiteral(L)^.val));
      lt_int       : result:=P_intLiteral(L)^.clone;
      lt_real      : result:=newRealLiteral(P_realLiteral(L)^.val);
      lt_string    : result:=newStringLiteral(P_stringLiteral(L)^.val,not(forUncasting));
      lt_expression: begin
        if L^.numberOfReferences<=1
        then result:=L^.rereferenced
        else result:=P_expressionLiteral(L)^.clone(location,threadContext);
      end;
      lt_list..lt_emptyMap: begin
        if L^.numberOfReferences<=1
        then result:=L^.rereferenced
        else result:=P_compoundLiteral(L)^.clone;
      end;
    end;
  end;

FUNCTION T_typedef.cast(CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:pointer; CONST adapters:P_adapters):P_literal;
  begin
    if L^.customType=@self then exit(L^.rereferenced);
    if ducktyperule^.evaluateToBoolean(location,threadContext,L) then begin
      result:=cloneLiteral(L,location,threadContext,false);
      if result<>nil then result^.customType:=@self;
    end else result:=nil;
    if result=nil then adapters^.raiseError('Cannot cast literal to custom type '+name,location);
  end;

FUNCTION T_typedef.uncast(CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:pointer; CONST adapters:P_adapters):P_literal;
  begin
    if L^.customType=nil then exit(L^.rereferenced);
    result:=cloneLiteral(L,location,threadContext,true);
    if result<>nil then result^.customType:=nil;
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
    if fill>length(dat)*HASH_GROWTH_THRESHOLD_FACTOR then rehash(true);
  end;

PROCEDURE G_literalKeyMap.rehashForExpectedSize(CONST expectedFill:longint);
  VAR targetSize:longint;
  begin
    if fill=0 then begin
      targetSize:=length(dat);
      while expectedFill>=targetSize*HASH_GROWTH_THRESHOLD_FACTOR do inc(targetSize,targetSize);
      setLength(dat,targetSize);
    end else while expectedFill>length(dat)*HASH_GROWTH_THRESHOLD_FACTOR do rehash(true);
  end;

FUNCTION G_literalKeyMap.putNew(CONST entry:CACHE_ENTRY; OUT previousValue:VALUE_TYPE):boolean;
  VAR binIdx:longint;
      j:longint;
  begin
    initialize(previousValue);
    binIdx:=entry.keyHash and (length(dat)-1);
    j:=0;
    while (j<length(dat[binIdx])) and not((entry.keyHash=dat[binIdx,j].keyHash) and dat[binIdx,j].key^.equals(entry.key)) do inc(j);
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
    if fill>length(dat)*HASH_GROWTH_THRESHOLD_FACTOR then rehash(true);
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
    while (j<length(dat[binIdx])) and not((hash=dat[binIdx,j].keyHash) and dat[binIdx,j].key^.equals(key)) do inc(j);
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
    if fill>length(dat)*HASH_GROWTH_THRESHOLD_FACTOR then rehash(true);
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

FUNCTION G_literalKeyMap.getEntry(CONST key:P_literal):P_CACHE_ENTRY;
  VAR hash:T_hashInt;
      binIdx:longint;
      j:longint;
  begin
    hash:=key^.hash;
    binIdx:=hash and (length(dat)-1);
    for j:=0 to length(dat[binIdx])-1 do if (dat[binIdx,j].keyHash=hash) and (dat[binIdx,j].key^.equals(key)) then exit(@(dat[binIdx,j]));
    result:=nil;
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
      if fill<length(dat)*HASH_SHRINK_THRESHOLD_FACTOR then rehash(false);
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
      {$ifdef debugMode}
      try
      {$endif}
        result[k]:=dat[i,j].key;
      {$ifdef debugMode}
      except
        raise Exception.create('Trying to set result ['+intToStr(k)+'] in list of length '+intToStr(length(result)));
      end;
      {$endif}
      inc(k);
    end;
  end;

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
    result:=interlockedDecrement(numberOfReferences);
  end;

FUNCTION T_literal.getId:T_idString;            begin result:=''; end;
FUNCTION T_literal.getLocation:T_tokenLocation; begin result.package:=nil; result.column:=-1; result.line:=-1; end;

FUNCTION T_expressionLiteral.getLocation:T_tokenLocation; begin result:=declaredAt; end;
//CONSTRUCTORS:=================================================================
{$MACRO ON}
{$define inline_init:=numberOfReferences:=1; customType:=nil; literalType:=}
CONSTRUCTOR T_literal.init(CONST lt: T_literalType); begin literalType:=lt; numberOfReferences:=1; end;
CONSTRUCTOR T_voidLiteral.create();                              begin {inherited init}inline_init(lt_void);                end;
CONSTRUCTOR T_boolLiteral      .create(CONST value: boolean);    begin {inherited init}inline_init(lt_boolean); val:=value; end;
CONSTRUCTOR T_intLiteral       .create(CONST value: int64);      begin {inherited init}inline_init(lt_int);     val.fromInt(value); end;
CONSTRUCTOR T_intLiteral       .create(CONST value: T_bigInt);   begin {inherited init}inline_init(lt_int);     val:=value; end;
CONSTRUCTOR T_realLiteral      .create(CONST value: T_myFloat);  begin {inherited init}inline_init(lt_real);    val:=value; end;
CONSTRUCTOR T_stringLiteral    .create(CONST value: ansistring); begin {inherited init}inline_init(lt_string);  val:=value; enc:=se_testPending; end;
CONSTRUCTOR T_expressionLiteral.create(CONST eType: T_expressionType; CONST location:T_tokenLocation);
  begin
    inline_init(lt_expression);
    myHash:=0;
    expressionType:=eType;
    declaredAt:=location;
  end;

FUNCTION T_intLiteral.clone:P_intLiteral;
  VAR valCopy:T_bigInt;
  begin
    valCopy.create(val);
    new(result,create(valCopy));
  end;

CONSTRUCTOR T_listLiteral.create(CONST initialSize: longint);
  begin
    inline_init(lt_emptyList);
    myHash  :=0;
    ints    :=0;
    reals   :=0;
    strings :=0;
    booleans:=0;
    others  :=0;
    getMem(dat,sizeOf(P_literal)*initialSize);
    alloc:=initialSize;
    fill:=0;
  end;

CONSTRUCTOR T_setLiteral.create;
  begin
    inline_init(lt_emptySet);
    myHash  :=0;
    ints    :=0;
    reals   :=0;
    strings :=0;
    booleans:=0;
    others  :=0;
    dat.create();
  end;

CONSTRUCTOR T_mapLiteral.create;
  begin
    inline_init(lt_emptyMap);
    myHash:=0;
    dat.create();
  end;
//=================================================================:CONSTRUCTORS
//DESTRUCTORS:==================================================================
DESTRUCTOR T_literal.destroy; begin end;
DESTRUCTOR T_intLiteral.destroy; begin val.destroy; end;
DESTRUCTOR T_stringLiteral.destroy; begin val:=''; end;
DESTRUCTOR T_listLiteral.destroy;
  VAR i:longint;
  begin
    for i:=0 to fill-1 do disposeLiteral(dat[i]);
    freeMem(dat,sizeOf(P_literal)*alloc);
    alloc:=0;
    fill:=0;
  end;

DESTRUCTOR T_setLiteral.destroy;
  VAR entries:T_arrayOfLiteral;
      i:longint;
  begin
    entries:=dat.keySet;
    for i:=0 to length(entries)-1 do disposeLiteral(entries[i]);
    dat.destroy;
  end;

DESTRUCTOR T_mapLiteral.destroy;
  VAR entries:T_literalKeyLiteralValueMap.KEY_VALUE_LIST;
      i:longint;
  begin
    entries:=dat.keyValueList;
    for i:=0 to length(entries)-1 do begin
      disposeLiteral(entries[i].key);
      disposeLiteral(entries[i].value);
    end;
    dat.destroy;
  end;
//==================================================================:DESTRUCTORS
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
      if dat.putNew(E,prevBool) then begin
        E.key^.rereference;
        modifyType(E.key);
      end;
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
    result:=newListLiteral(imax);
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
    result:=newListLiteral(fill-iMin);
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

CONST listType:array[false..true,false..true,false..true,false..true] of T_literalType=
              ((((lt_emptyList  ,lt_stringList),    //                     + string?
                 (lt_realList   ,lt_list      )),   //              + real + string?
                ((lt_intList    ,lt_list      ),    //          int        + string?
                 (lt_numList    ,lt_list      ))),  //          int + real + string?
               (((lt_booleanList,lt_list      ),    //boolean              + string?
                 (lt_list       ,lt_list      )),   //boolean       + real + string?
                ((lt_list       ,lt_list      ),    //boolean + int        + string?
                 (lt_list       ,lt_list      )))); //boolean + int + real + string?

PROCEDURE T_listLiteral.removeElement(CONST index:longint);
  VAR i:longint;
  begin
    if (index<0) or (index>=fill) then exit;
    myHash:=0;
    case dat[index]^.literalType of
      lt_boolean: dec(booleans);
      lt_int    : dec(ints);
      lt_real   : dec(reals);
      lt_string : dec(strings);
      else        dec(others);
    end;
    if fill=0 then literalType:=lt_emptyList
    else if others>0 then literalType:=lt_list
    else literalType:=listType[booleans>0,ints>0,reals>0,strings>0];
    disposeLiteral(dat[index]);
    for i:=index to fill-2 do dat[i]:=dat[i+1];
    dec(fill);
  end;

//?.toString:===================================================================
FUNCTION T_literal          .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:='<ERR>';           end;
FUNCTION T_voidLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=LITERAL_TEXT_VOID;        end;
FUNCTION T_boolLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=LITERAL_BOOL_TEXT[val]; if customType<>nil then result+='.to'+customType^.name; end;
FUNCTION T_intLiteral       .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=val.toString;      if customType<>nil then result:='to'+customType^.name+'('+result+')'; end;
FUNCTION T_realLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=myFloatToStr(val); if customType<>nil then result:='to'+customType^.name+'('+result+')'; end;
FUNCTION T_stringLiteral    .toString(CONST lengthLimit:longint=maxLongint): ansistring;
  begin
    if lengthLimit>=length(val)+2 then result:=escapeString(val,es_pickShortest)
                                  else result:=escapeString(UTF8Copy(val,1,lengthLimit-5)+'...',es_pickShortest);
    if customType<>nil then result+='.to'+customType^.name;
  end;

FUNCTION T_listLiteral.toString(CONST lengthLimit: longint): ansistring;
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
    if customType<>nil then result+='.to'+customType^.name;
  end;

FUNCTION T_setLiteral.toString(CONST lengthLimit: longint): ansistring;
  VAR i,remainingLength: longint;
      iter:T_arrayOfLiteral;
  begin
    if size = 0 then result:='[]'
    else begin
      iter:=iteratableList;;
      remainingLength:=lengthLimit-1;
      result:='['+iter[0]^.toString(remainingLength);
      for i:=1 to size-1 do if remainingLength>0 then begin
        remainingLength:=lengthLimit-length(result);
        result:=result+','+iter[i]^.toString(remainingLength);
      end else begin
        result:=result+',... ';
        break;
      end;
      disposeLiteral(iter);
      result:=result+']';
    end;
    result:=result+'.toSet';
    if customType<>nil then result+='.to'+customType^.name;
  end;

FUNCTION T_mapLiteral.toString(CONST lengthLimit: longint): ansistring;
  VAR i,remainingLength: longint;
      iter:T_arrayOfLiteral;
  begin
    if size = 0 then result:='[]'
    else begin
      iter:=iteratableList;;
      remainingLength:=lengthLimit-1;
      result:='['+iter[0]^.toString(remainingLength);
      for i:=1 to size-1 do if remainingLength>0 then begin
        remainingLength:=lengthLimit-length(result);
        result:=result+','+iter[i]^.toString(remainingLength);
      end else begin
        result:=result+',... ';
        break;
      end;
      disposeLiteral(iter);
      result:=result+']';
    end;
    result:=result+'.toMap';
    if customType<>nil then result+='.to'+customType^.name;
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

//?.isInRelationTo:=============================================================
FUNCTION T_literal.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  begin
    result:=false;
  end;

FUNCTION T_boolLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR ovl: boolean;
  begin
    case relation of
      tt_operatorIn      : exit((other^.literalType in C_typeInfo[lt_boolean].containedIn) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    if other^.literalType<>lt_boolean then exit(false);
    ovl:=P_boolLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq, tt_comparatorLeq, tt_comparatorGeq])
         or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_intLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR cr:T_comparisonResult;
  begin
    case relation of
      tt_operatorIn      : exit((other^.literalType in C_typeInfo[lt_int].containedIn) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    case other^.literalType of
      lt_int: begin
        cr:=val.compare(P_intLiteral(other)^.val);
        result:=(cr=CR_EQUAL  ) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
             or (cr=CR_LESSER ) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (cr=CR_GREATER) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        cr:=val.compare(P_realLiteral(other)^.val);
        result:=(cr=CR_EQUAL  ) and (relation in [tt_comparatorEq, tt_comparatorLeq, tt_comparatorGeq])
             or (cr=CR_LESSER ) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (cr=CR_GREATER) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result:=false;
    end;
  end;

FUNCTION T_realLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR cr:T_comparisonResult;
      ovr: T_myFloat;
  begin
    case relation of
      tt_operatorIn      : exit((other^.literalType in C_typeInfo[lt_real].containedIn) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    case other^.literalType of
      lt_int: begin
        cr:=C_FLIPPED[P_intLiteral(other)^.val.compare(val)];
        result:=(cr=CR_EQUAL  ) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
             or (cr=CR_LESSER ) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (cr=CR_GREATER) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
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
      tt_operatorIn      : exit((other^.literalType in C_typeInfo[lt_string].containedIn) and (P_compoundLiteral(other)^.contains(@self)));
      tt_comparatorListEq: exit(equals(other));
    end;
    if other^.literalType<>lt_string then exit(false);
    ovl:=P_stringLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq, tt_comparatorLeq, tt_comparatorGeq])
         or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_expressionLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  begin
    result:=(relation=tt_comparatorListEq) and equals(other);
  end;

FUNCTION T_compoundLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
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
FUNCTION T_listLiteral.newOfSameType(CONST initSize:boolean): P_collectionLiteral; begin if initSize then result:=newListLiteral(fill) else result:=newListLiteral(); end;
FUNCTION T_setLiteral.newOfSameType(CONST initSize:boolean): P_collectionLiteral; begin result:=newSetLiteral; if initSize then P_setLiteral(result)^.dat.rehashForExpectedSize(dat.fill); end;
//?.negate:=====================================================================
FUNCTION T_literal.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
  begin result:=@self; rereference; end;
FUNCTION T_stringLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
  begin result:=newVoidLiteral; adapters.raiseError('Cannot negate string.', minusLocation); end;
FUNCTION T_boolLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
  begin result:=newVoidLiteral; adapters.raiseError('Cannot negate boolean.', minusLocation); end;
FUNCTION T_expressionLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
  begin
    result:=subruleApplyOpCallback(@intLit[-1],tt_operatorMult,@self,minusLocation,threadContext);
  end;

FUNCTION T_intLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
  begin result:=newIntLiteral(value.negated); end;
FUNCTION T_realLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
  begin result:=newRealLiteral(-value); end;
FUNCTION T_listLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
  VAR res: P_listLiteral;
      i  : longint;
  begin
    res:=newListLiteral(size);
    for i:=0 to size-1 do res^.append(dat[i]^.negate(minusLocation,adapters,threadContext),false);
    result:=res;
  end;
FUNCTION T_setLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
  VAR res: P_setLiteral;
      iter:T_arrayOfLiteral;
      x:P_literal;
  begin
    res:=newSetLiteral(dat.fill);
    iter:=iteratableList;
    for x in iter do res^.append(x^.negate(minusLocation,adapters,threadContext),false);
    disposeLiteral(iter);
    result:=res;
  end;
//=====================================================================:?.negate
FUNCTION T_literal          .typeString:string;
  begin
    if customType<>nil
    then result:=customType^.name
    else result:=C_typeInfo[literalType].name;
  end;

FUNCTION T_compoundLiteral  .typeString:string;
  begin
    if customType<>nil
    then result:=customType^.name
    else result:=C_typeInfo[literalType].name+'('+intToStr(size)+')';
  end;

FUNCTION T_expressionLiteral.typeString:string;
  begin
    if customType<>nil then exit(customType^.name);
    if expressionType in C_iteratableExpressionTypes then exit(C_typeCheckInfo[tc_typeCheckIteratableExpression].name)
    else if expressionType in C_statefulExpressionTypes
    then result:=C_typeCheckInfo[tc_typeCheckStatefulExpression].name
    else result:=C_typeInfo     [literalType                   ].name;
    result:=result+'('+intToStr(arity)+')';
  end;

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
FUNCTION T_boolLiteral.hash: T_hashInt; begin result:=longint(lt_boolean) xor T_hashInt(customType); if val then inc(result); end;
FUNCTION T_intLiteral .hash: T_hashInt; begin {$R-} result:=longint(lt_int) xor val.lowDigit xor T_hashInt(customType); if val.isNegative then inc(result); {$R+} end;
FUNCTION T_realLiteral.hash: T_hashInt;
  begin
    {$Q-}{$R-}
    result:=0;
    move(val, result, sizeOf(result));
    result:=result xor longint(lt_real) xor T_hashInt(customType);
    {$Q+}{$R+}
  end;

FUNCTION T_stringLiteral.hash: T_hashInt;
  VAR i: longint;
  begin
    {$Q-}{$R-}
    result:=T_hashInt(lt_string)+T_hashInt(length(val))+T_hashInt(customType);
    for i:=1 to length(val) do result:=result*31+ord(val[i]);
    {$Q+}{$R+}
  end;

FUNCTION T_expressionLiteral.hash: T_hashInt;
  VAR i:longint;
      s:string;
  begin
    if myHash>0 then exit(myHash);
    {$Q-}{$R-}
    s:= toString;
    result:=T_hashInt(lt_expression)+T_hashInt(length(s))+T_hashInt(customType);
    for i:=1 to length(s) do result:=result*31+ord(s[i]);
    {$Q+}{$R+}
    if result=0 then result:=1;
    myHash:=result;
  end;

FUNCTION T_listLiteral.hash: T_hashInt;
  VAR i:longint;
  begin
    if myHash>0 then exit(myHash);
    {$Q-}{$R-}
    result:=T_hashInt(lt_list)+T_hashInt(fill)+T_hashInt(customType);
    for i:=0 to fill-1 do result:=result*31+dat[i]^.hash;
    {$Q+}{$R+}
    if result=0 then result:=1;
    myHash:=result;
  end;

FUNCTION T_setLiteral.hash: T_hashInt;
  VAR entry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    if myHash>0 then exit(myHash);
    {$Q-}{$R-}
    result:=T_hashInt(lt_set)+T_hashInt(dat.fill)+T_hashInt(customType);
    result:=result*31;
    for entry in dat.keyValueList do result:=result+entry.keyHash;
    {$Q+}{$R+}
    if result=0 then result:=1;
    myHash:=result;
  end;

FUNCTION T_mapLiteral.hash: T_hashInt;
  VAR entry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    if myHash>0 then exit(myHash);
    {$Q-}{$R-}
    result:=T_hashInt(lt_map)+T_hashInt(dat.fill)+T_hashInt(customType);
    result:=result*31;
    for entry in dat.keyValueList do result:=result+entry.keyHash+entry.value^.hash*37;
    {$Q+}{$R+}
    if result=0 then result:=1;
    myHash:=result;
  end;

//=======================================================================:?.hash
PROCEDURE T_expressionLiteral.makeStateful  (CONST adapters:P_adapters; CONST location:T_tokenLocation);
  begin
    if expressionType in C_statefulExpressionTypes then exit;
    case expressionType of
      et_subrule: expressionType:=et_subruleStateful;
      et_inline : expressionType:=et_inlineStateful;
      else if adapters<>nil then adapters^.raiseError(C_expressionTypeString[expressionType]+' cannot be stateful',location);
    end;
  end;

PROCEDURE T_expressionLiteral.makeIteratable(CONST adapters:P_adapters; CONST location:T_tokenLocation);
  begin
    if expressionType in C_iteratableExpressionTypes then exit;
    if not(canApplyToNumberOfParameters(0)) or not(expressionType in C_statefulExpressionTypes) then begin
      if adapters<>nil then adapters^.raiseError('Only nullary stateful expressions may be iteratable.',location);
      exit;
    end;
    case expressionType of
      et_subruleStateful: expressionType:=et_subruleIteratable;
      et_inlineStateful : expressionType:=et_inlineIteratable;
    else if adapters<>nil then adapters^.raiseError('Only nullary stateful expressions may be iteratable.',location);
    end;
  end;

//?.equals:=====================================================================
FUNCTION T_literal.equals(CONST other: P_literal): boolean;
  begin result:=(@self = other) or (other^.literalType = literalType) and (other^.toString=toString);  end;

FUNCTION T_intLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other) or (other^.literalType = lt_int) and (P_intLiteral(other)^.val.equals(val));
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

FUNCTION T_expressionLiteral.equals(CONST other: P_literal): boolean;
  begin
    if @self=other then exit(true);
    if (other^.literalType<>lt_expression) or (P_expressionLiteral(other)^.expressionType<>expressionType)
                                           or (P_expressionLiteral(other)^.customType    <>customType) then exit(false);
    case typ of
      et_subrule ,
      et_inline  ,
      et_eachBody,
      et_whileBody:result:=toString()=other^.toString();
      else result:=false; //only equal if same
    end;
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
    if not(literalType in C_typeInfo[other^.literalType].containedIn) then exit(false);
    for i:=0 to fill-1 do if dat[i]^.equals(other) then exit(true);
    result:=false;
  end;

FUNCTION T_setLiteral.contains(CONST other: P_literal): boolean;
  begin
    if not(literalType in C_typeInfo[other^.literalType].containedIn) then exit(false);
    result:=dat.get(other,false);
  end;

FUNCTION T_mapLiteral.contains(CONST other: P_literal): boolean;
  VAR key,val,mapValue:P_literal;
  begin
    if not(literalType in C_typeInfo[other^.literalType].containedIn) or
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
        i:=P_intLiteral(accessor)^.val.toInt;
        if (i>=0) and (i<fill) then exit(dat[i]^.rereferenced)
                               else exit(newVoidLiteral);
      end;
      lt_intList, lt_emptyList: begin
        result:=newListLiteral(P_listLiteral(accessor)^.fill);
        for j:=0 to P_listLiteral(accessor)^.fill-1 do begin
          i:=P_intLiteral(P_listLiteral(accessor)^.dat[j])^.val.toInt;
          if (i>=0) and (i<fill) then P_listLiteral(result)^.append(dat[i],true);
        end;
        exit(result);
      end;
      lt_intSet, lt_emptySet: begin
        result:=newSetLiteral;
        P_setLiteral(result)^.dat.rehashForExpectedSize(P_setLiteral(accessor)^.size);
        iter:=P_setLiteral(accessor)^.iteratableList;
        for idx in iter do begin
          i:=P_intLiteral(idx)^.val.toInt;
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
      for i:=0 to fill-1 do if accessor^.equals(P_listLiteral(dat[i])^.value[0])
                                      then exit(P_listLiteral(dat[i])^.value[1]^.rereferenced);
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
      for x in iter do if (result=nil) and accessor^.equals(P_listLiteral(x)^.value[0])
                                               then result:=P_listLiteral(x)^.value[1]^.rereferenced;
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
    result:=newSetLiteral(length(iter));
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
        wantKeys  :=(P_intLiteral(accessor)^.val.toInt=0);
        wantValues:=(P_intLiteral(accessor)^.val.toInt=1);
        validCase :=true;
      end;
      lt_intList, lt_intSet,lt_emptyList,lt_emptySet: begin
        wantKeys  :=P_compoundLiteral(accessor)^.contains(@intLit[0]);
        wantValues:=P_compoundLiteral(accessor)^.contains(@intLit[1]);
        validCase :=true;
        subAsSet  :=accessor^.literalType in C_setTypes;
      end;
      lt_booleanList: if P_listLiteral(accessor)^.size=2 then begin
        wantKeys  :=P_boolLiteral(P_listLiteral(accessor)^.value[0])^.val;
        wantValues:=P_boolLiteral(P_listLiteral(accessor)^.value[1])^.val;
        validCase :=true;
      end;
    end;
    result:=nil;
    if validCase then begin
      if wantKeys then begin
        if wantValues then for entry in dat.keyValueList do begin
          result:=newListLiteral(dat.fill);
          if subAsSet then sub:=newSetLiteral(2)
                      else sub:=newListLiteral(2);
          sub^.append(entry.key  ,true);
          sub^.append(entry.value,true);
          P_listLiteral(result)^.append(sub,false);
        end else begin
          result:=newSetLiteral(dat.fill);
          for entry in dat.keyValueList do P_setLiteral(result)^.append(entry.key,true);
        end;
      end else if wantValues then begin
        result:=newListLiteral(dat.fill);
        for entry in dat.keyValueList do
        P_listLiteral(result)^.append(entry.value,true);
      end else result:=newListLiteral();
    end;
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
      lt_int:  result:=val.compare(P_intLiteral (other)^.val) in [CR_LESSER,CR_EQUAL];
      lt_real: result:=val.compare(P_realLiteral(other)^.val) in [CR_LESSER,CR_EQUAL];
    else result:=(literalType<=other^.literalType); end;
  end;

FUNCTION T_realLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    case other^.literalType of
      lt_int:  result:=P_intLiteral(other)^.val.compare(val) in [CR_EQUAL,CR_GREATER];
      lt_real: if isNan(val) then result:=not(isNan(P_realLiteral(other)^.val))
               else if isNan(P_realLiteral(other)^.val) then result:=false
               else result:=val<=P_realLiteral(other)^.val;
    else result:=(literalType<=other^.literalType);  end;
  end;

FUNCTION T_stringLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    if (other^.literalType = lt_string)
    then result:=val<=P_stringLiteral(other)^.val
    else result:=(literalType<=other^.literalType);
  end;

FUNCTION T_listLiteral.leqForSorting(CONST other: P_literal): boolean;
  VAR i: longint;
  begin
    if (other^.literalType in C_listTypes) then begin
      if      size<P_compoundLiteral(other)^.size then exit(true)
      else if size>P_compoundLiteral(other)^.size then exit(false)
      else for i:=0 to size-1 do if value[i]^.leqForSorting(P_listLiteral(other)^.value[i]) then begin
        if not(P_listLiteral(other)^.value[i]^.leqForSorting(value[i])) then exit(true);
      end else exit(false);
      exit(true);
    end else result:=literalType<=other^.literalType;
  end;

FUNCTION T_setLiteral.leqForSorting(CONST other: P_literal): boolean;
  VAR myList,otherList:P_listLiteral;
  begin
    if (other^.literalType in C_setTypes) then begin
      if      size<P_compoundLiteral(other)^.size then exit(true)
      else if size>P_compoundLiteral(other)^.size then exit(false)
      else begin
        myList   :=                     toList; myList   ^.sort;
        otherList:=P_setLiteral(other)^.toList; otherList^.sort;
        result:=myList^.leqForSorting(otherList);
        disposeLiteral(myList);
        disposeLiteral(otherList);
      end;
    end else result:=literalType<=other^.literalType;
  end;

FUNCTION T_mapLiteral.leqForSorting(CONST other: P_literal): boolean;
  VAR myList,otherList:P_listLiteral;
  begin
    if (other^.literalType in C_mapTypes) then begin
      if      size<P_compoundLiteral(other)^.size then exit(true)
      else if size>P_compoundLiteral(other)^.size then exit(false)
      else begin
        myList   :=                     toList; myList   ^.sort;
        otherList:=P_mapLiteral(other)^.toList; otherList^.sort;
        result:=myList^.leqForSorting(otherList);
        disposeLiteral(myList);
        disposeLiteral(otherList);
      end;
    end else result:=literalType<=other^.literalType;
  end;

//?.leqForSorting:==============================================================
FUNCTION T_stringLiteral.softCast: P_literal;
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

FUNCTION T_stringLiteral.getEncoding: T_stringEncoding;
  begin
    if enc=se_testPending then enc:=encoding(val);
    result:=enc;
  end;

PROCEDURE T_stringLiteral.append(CONST suffix:ansistring);
  begin
    {$ifdef debugMode}
    if numberOfReferences>1 then raise Exception.create('You must not append suffixes to strings which are used elsewhere!');
    {$endif}
    val:=val+suffix;
    enc:=se_testPending;
  end;

PROCEDURE T_listLiteral.modifyType(CONST L: P_literal);
  begin
    case L^.literalType of
      lt_error     : begin inc(others); containsError:=true; literalType:=lt_list; end;
      lt_boolean   : begin inc(booleans); if literalType in [lt_emptyList,lt_booleanList] then literalType:=lt_booleanList                                      else literalType:=lt_list; end;
      lt_int       : begin inc(ints);     case literalType of lt_emptyList,lt_intList: literalType:=lt_intList; lt_realList,lt_numList:literalType:=lt_numList; else literalType:=lt_list; end; end;
      lt_real      : begin inc(reals);    case literalType of lt_emptyList,lt_realList:literalType:=lt_realList;lt_intList ,lt_numList:literalType:=lt_numList; else literalType:=lt_list; end; end;
      lt_string    : begin inc(strings);  if literalType in [lt_emptyList,lt_stringList] then literalType:=lt_stringList                                        else literalType:=lt_list; end;
      lt_expression,
      lt_void      : begin inc(others); literalType:=lt_list; end;
      else           begin inc(others); literalType:=lt_list; containsError:=containsError or P_compoundLiteral(L)^.containsError; end;
    end;
    myHash:=0;
  end;

PROCEDURE T_setLiteral.modifyType(CONST L: P_literal);
  begin
    case L^.literalType of
      lt_error     : begin inc(others); containsError:=true; literalType:=lt_set; end;
      lt_boolean   : begin inc(booleans); if literalType in [lt_emptySet,lt_booleanSet] then literalType:=lt_booleanSet                                   else literalType:=lt_set; end;
      lt_int       : begin inc(ints);     case literalType of lt_emptySet,lt_intSet: literalType:=lt_intSet; lt_realSet,lt_numSet:literalType:=lt_numSet; else literalType:=lt_set; end; end;
      lt_real      : begin inc(reals);    case literalType of lt_emptySet,lt_realSet:literalType:=lt_realSet;lt_intSet ,lt_numSet:literalType:=lt_numSet; else literalType:=lt_set; end; end;
      lt_string    : begin inc(strings);  if literalType in [lt_emptySet,lt_stringSet] then literalType:=lt_stringSet                                     else literalType:=lt_set; end;
      lt_expression,
      lt_void      : begin inc(others); literalType:=lt_set; end;
      else           begin inc(others); literalType:=lt_set; containsError:=containsError or P_compoundLiteral(L)^.containsError; end;
    end;
    myHash:=0;
  end;

FUNCTION T_compoundLiteral.toSet: P_setLiteral;
  begin
    if literalType in C_setTypes then exit(P_setLiteral(rereferenced));
    result:=P_setLiteral(newSetLiteral(size)^.appendAll(@self));
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
      result^.put(P_listLiteral(pair)^.value[0],
                  P_listLiteral(pair)^.value[1],true);
    end else begin
      result^.containsError:=true;
      adapters.raiseError('Literal of type '+pair^.typeString+' cannot be interpreted as key-value-pair',location);
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
      i0:=P_intLiteral(last)^.val.toInt;
      i1:=P_intLiteral(L   )^.val.toInt;
      newLen:=fill+abs(i1-i0)+1;
      if newLen>alloc then begin
        ReAllocMem(dat,sizeOf(P_literal)*newLen);
        alloc:=newLen;
      end;
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
      if newLen>alloc then begin
        ReAllocMem(dat,sizeOf(P_literal)*newLen);
        alloc:=newLen;
      end;
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

FUNCTION T_listLiteral.append(CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false): P_collectionLiteral;
  begin
    result:=@self;
    if (L=nil) or ((L^.literalType=lt_void) and not(forceVoidAppend)) then exit;
    if alloc>fill then begin end else begin
      alloc:=round(fill*1.25)+2;
      ReAllocMem(dat,sizeOf(P_literal)*alloc);
    end;
    dat[fill]:=L;
    inc(fill);
    if incRefs then L^.rereference;
    modifyType(L);
  end;

FUNCTION T_setLiteral.append(CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false): P_collectionLiteral;
  VAR prevBool:boolean;
  begin
    result:=@self;
    if (L=nil) or ((L^.literalType=lt_void) and not(forceVoidAppend)) then exit;
    if dat.putNew(L,true,prevBool) then begin
      if incRefs then L^.rereference;
      modifyType(L);
    end else if not(incRefs) then disposeLiteralWithoutResettingPointer(L);
  end;

FUNCTION T_mapLiteral.put(CONST key,newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
  VAR prevValue:P_literal;
  begin
    if dat.putNew(key,newValue,prevValue) then begin
      if incRefs then key^.rereference;
      literalType:=lt_map;
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
  begin
    for E in map^.dat.keyValueList do begin
      if dat.putNew(E,prevValue) then begin
        E.key^.rereference;
        literalType:=lt_map;
      end else begin
        disposeLiteral(prevValue);
      end;
      E.value^.rereference;
    end;
    result:=@self;
  end;

PROCEDURE T_mapLiteral.drop(CONST L: P_literal);
  VAR dropped:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    dropped:=dat.drop(L);
    if dropped.key=nil then exit;
    disposeLiteral(dropped.key);
    disposeLiteral(dropped.value);
    if dat.fill<=0 then literalType:=lt_map;
  end;

PROCEDURE T_setLiteral.drop(CONST L:P_literal);
  VAR dropped:T_literalKeyBooleanValueMap.CACHE_ENTRY;
  CONST setType:array[false..true,false..true,false..true,false..true] of T_literalType=
                ((((lt_emptySet  ,lt_stringSet),    //                     + string?
                   (lt_realSet   ,lt_set      )),   //              + real + string?
                  ((lt_intSet    ,lt_set      ),    //          int        + string?
                   (lt_numSet    ,lt_set      ))),  //          int + real + string?
                 (((lt_booleanSet,lt_set      ),    //boolean              + string?
                   (lt_set       ,lt_set      )),   //boolean       + real + string?
                  ((lt_set       ,lt_set      ),    //boolean + int        + string?
                   (lt_set       ,lt_set      )))); //boolean + int + real + string?
  begin
    dropped:=dat.drop(L);
    if dropped.key=nil then exit;
    disposeLiteral(dropped.key);
    case L^.literalType of
      lt_boolean: dec(booleans);
      lt_int    : dec(ints);
      lt_real   : dec(reals);
      lt_string : dec(strings);
      else        dec(others);
    end;
    if dat.fill=0 then literalType:=lt_emptySet
    else if others>0 then literalType:=lt_set
    else literalType:=setType[booleans>0,ints>0,reals>0,strings>0];
  end;

PROCEDURE T_listLiteral.sort;
  VAR temp: T_arrayOfLiteral;
      scale: longint;
      i, j0, j1, k: longint;
  begin
    if (fill<=1) then exit;
    myHash:=0;
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
    myHash:=0;
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
  FUNCTION isLeq(CONST a,b:P_literal):boolean; inline; begin result:=leqExpression^.evaluateToBoolean(location,context,a,b); end;

  begin
    if fill<=1 then exit;
    myHash:=0;
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
    myHash:=0;
    setLength(temp1, fill);
    setLength(temp2, fill);
    for i:=0 to fill-1 do with temp1[i] do begin
      v:=dat[i];
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
    result:=newListLiteral(length(temp1));
    for i:=0 to length(temp1)-1 do result^.appendInt(temp1[i].index);
    setLength(temp1, 0);
  end;

PROCEDURE T_listLiteral.unique;
  VAR i,j:longint;
  begin
    if fill<=0 then exit;
    myHash:=0;
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
    result:=newSetLiteral();
    setLength(P_setLiteral(result)^.dat.dat,
                             length(dat.dat));
    P_setLiteral(result)^.dat.fill:=dat.fill;
    for bin:=0 to length(dat.dat)-1 do begin
      setLength(P_setLiteral(result)^.dat.dat[bin],
                               length(dat.dat[bin]));
      for i:=0 to length(dat.dat[bin])-1 do begin
        P_setLiteral(result)^.dat.dat[bin,i]:=
                              dat.dat[bin,i];
        dat.dat[bin,i].key^.rereference;
      end;
    end;
    result^.literalType:=literalType;
  end;

FUNCTION T_mapLiteral.clone: P_compoundLiteral;
  VAR bin,i:longint;
  begin
    result:=newMapLiteral;
    result^.literalType:=literalType;
    P_mapLiteral(result)^.dat.fill:=dat.fill;
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
    result:=dat.keySet;
    for L in result do L^.rereference;
  end;

FUNCTION T_mapLiteral.iteratableList: T_arrayOfLiteral;
  VAR e:T_literalKeyLiteralValueMap.KEY_VALUE_LIST;
      i:longint;
  begin
    e:=dat.keyValueList;
    setLength(result,length(e));
    for i:=0 to length(e)-1 do result[i]:=newListLiteral(2)^.append(e[i].key,true)^.append(e[i].value,true);
  end;

FUNCTION T_mapLiteral.keyIteratableList:T_arrayOfLiteral;
  VAR L:P_literal;
  begin
    result:=dat.keySet;
    for L in result do L^.rereference;
  end;

FUNCTION setUnion(CONST params:P_listLiteral):P_setLiteral;
  VAR i:longint;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^.value[i]^.literalType in C_compoundTypes) then exit(nil);
    if params^.size=1 then exit(P_compoundLiteral(params^.value[0])^.toSet());
    result:=newSetLiteral(P_compoundLiteral(params^.value[0])^.size);
    for i:=0 to params^.size-1 do result^.appendAll(P_compoundLiteral(params^.value[i]));
  end;

FUNCTION setIntersect(CONST params:P_listLiteral):P_setLiteral;
  TYPE T_occurenceCount=specialize G_literalKeyMap<word>;
  CONST bit:array[0..15] of word=(1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768);
  VAR counterSet:T_occurenceCount;
      prevMask:word; //dummy
      i:longint;
      maxSubsetSize:longint=0;
      entry:T_occurenceCount.CACHE_ENTRY;
      iter:T_arrayOfLiteral;
      x:P_literal;
      acceptMask:word=0;

  FUNCTION resultByRecursion:P_setLiteral;
    VAR innerCallParam,outerCallParam:P_listLiteral;
        i:longint;
    begin
      outerCallParam:=newListLiteral();
      innerCallParam:=newListLiteral(16);
      for i:=0 to params^.size-1 do begin
        innerCallParam^.append(params^.dat[i],true);
        if innerCallParam^.size>=16 then begin
          outerCallParam^.append(setIntersect(innerCallParam),false);
          disposeLiteral(innerCallParam);
          innerCallParam:=newListLiteral(16);
        end;
      end;
      if innerCallParam^.size>=1 then outerCallParam^.append(setIntersect(innerCallParam),false);
      disposeLiteral(innerCallParam);
      result:=setIntersect(outerCallParam);
      disposeLiteral(outerCallParam);
    end;

  begin
    if not((params<>nil) and (params^.size>=1))                                            then exit(nil);
    for i:=0 to params^.size-1 do if not(params^.value[i]^.literalType in C_compoundTypes) then exit(nil);
    if params^.size>16 then exit(resultByRecursion);
    if params^.size=1  then exit(P_compoundLiteral(params^.value[0])^.toSet());

    counterSet.create;
    for i:=0 to params^.size-1 do if P_compoundLiteral(params^.value[i])^.size>maxSubsetSize then maxSubsetSize:=P_compoundLiteral(params^.value[i])^.size;
    counterSet.rehashForExpectedSize(maxSubsetSize);
    for i:=0 to params^.size-1 do begin
      iter:=P_compoundLiteral(params^.value[i])^.iteratableList;
      for x in iter do counterSet.putNew(x,counterSet.get(x,0) or bit[i],prevMask);
      disposeLiteral(iter);
      inc(acceptMask,bit[i]);
    end;
    result:=newSetLiteral;
    result^.dat.rehashForExpectedSize(maxSubsetSize);
    for entry in counterSet.keyValueList do if (entry.value=acceptMask) then result^.append(entry.key,true);
    counterSet.destroy;
  end;

FUNCTION setMinus(CONST params:P_listLiteral):P_setLiteral;
  VAR L:P_literal;
      iter:T_arrayOfLiteral;
  begin
    if not((params<>nil) and
           (params^.size=2) and
           (params^.value[0]^.literalType in C_compoundTypes) and
           (params^.value[1]^.literalType in C_compoundTypes))
    then exit(nil);
    result:=newSetLiteral;
    iter:=P_compoundLiteral(params^.value[0])^.iteratableList;
    result^.dat.rehashForExpectedSize(length(iter));
    for L in iter do result^.dat.put(L,true);
    disposeLiteral(iter);
    iter:=P_compoundLiteral(params^.value[1])^.iteratableList;
    for L in iter do result^.dat.drop(L);
    disposeLiteral(iter);
    for L in result^.dat.keySet do begin
      L^.rereference;
      result^.modifyType(L);
    end;
  end;

FUNCTION mapMerge(CONST params:P_listLiteral; CONST location:T_tokenLocation; CONST contextPointer:pointer):P_mapLiteral;
  VAR map1,map2:P_mapLiteral;
      merger:P_expressionLiteral;
      entry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
      L,M:P_literal;
  begin
    if (params=nil) or (params^.size<>3) or
       not(params^.value[0]^.literalType in [lt_map,lt_emptyMap]) or
       not(params^.value[1]^.literalType in [lt_map,lt_emptyMap]) or
       (params^.value[2]^.literalType<>lt_expression) then exit(nil);
    map1:=P_mapLiteral(params^.value[0]);
    map2:=P_mapLiteral(params^.value[1]);
    merger:=P_expressionLiteral(params^.value[2]);
    if not(merger^.canApplyToNumberOfParameters(2)) then exit(nil);

    if map2^.size=0 then exit(P_mapLiteral(map1^.rereferenced));
    result:=P_mapLiteral(map1^.clone);

    for entry in map2^.dat.keyValueList do begin
      L:=result^.dat.get(entry.key,nil);
      if L=nil then begin
        result^.dat.putNew(entry,L);
        entry.key  ^.rereference;
        entry.value^.rereference;
      end else begin
        M:=merger^.evaluateToLiteral(location,contextPointer,L,entry.value).literal;
        if M=nil then begin
          disposeLiteral(result);
          exit(nil);
        end;
        disposeLiteral(L);
        result^.dat.putNew(entry.key,M,L);
      end;
    end;
    if result^.dat.fill>0 then result^.literalType:=lt_map
                          else result^.literalType:=lt_emptyMap;
  end;

FUNCTION mutateVariable(VAR toMutate:P_literal; CONST mutation:T_tokenType; CONST parameters:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer):P_literal;
  VAR returnValue:P_literal=nil;
  PROCEDURE return(CONST L:P_literal); inline;
    begin
      if returnValue<>nil then disposeLiteral(returnValue);
      returnValue:=L;
    end;

  FUNCTION simpleMutate(VAR toMutate:P_literal; CONST op:T_tokenType; CONST RHS:P_literal):boolean;
    VAR newValue:P_literal;
    begin
      newValue:=resolveOperatorCallback(toMutate,op,RHS,location,threadContext);
      if newValue<>nil then begin
        result:=newValue^.literalType<>toMutate^.literalType;
        disposeLiteral(toMutate);
        toMutate:=newValue;
        return(toMutate^.rereferenced);
      end else result:=false;
    end;

  FUNCTION assign(VAR toMutate:P_literal; CONST newValue:P_literal):boolean;
    begin
      result:=newValue^.literalType<>toMutate^.literalType;
      disposeLiteral(toMutate);
      toMutate:=newValue^.rereferenced;
      return(toMutate^.rereferenced);
    end;

  FUNCTION mutateStringAppend(VAR toMutate:P_literal; CONST RHS:P_literal):boolean;
    begin
      if (toMutate^.literalType=lt_string) and (toMutate^.numberOfReferences=1) and (RHS^.literalType in [lt_boolean..lt_string]) then begin
        if RHS^.literalType=lt_string
        then P_stringLiteral(toMutate)^.append(P_stringLiteral(RHS)^.val)
        else P_stringLiteral(toMutate)^.append(RHS^.toString());
        result:=false;
      end else result:=simpleMutate(toMutate,tt_operatorStrConcat,RHS);
      return(toMutate^.rereferenced);
    end;

  FUNCTION mutateListAppend(VAR toMutate:P_literal; CONST RHS:P_literal; CONST alt:boolean):boolean;
    begin
      if (toMutate^.literalType in C_setTypes+C_listTypes) and (toMutate^.numberOfReferences=1) then begin
        if (RHS^.literalType in C_scalarTypes) or (alt)
        then P_collectionLiteral(toMutate)^.append(RHS, true)
        else P_collectionLiteral(toMutate)^.appendAll(P_compoundLiteral(RHS));
        result:=false;
      end else if alt then result:=simpleMutate(toMutate,tt_operatorConcatAlt,RHS)
                      else result:=simpleMutate(toMutate,tt_operatorConcat   ,RHS);
      return(newVoidLiteral);
    end;

  PROCEDURE ensureExclusiveAccess(VAR toMutate:P_compoundLiteral); inline;
    VAR old:P_compoundLiteral;
    begin
      if toMutate^.numberOfReferences<=1 then begin
        toMutate^.myHash:=0;
        exit;
      end;
      if toMutate^.customType<>nil then adapters.raiseNote('Mutating resets type of variable from '+toMutate^.customType^.name+' to '+C_typeInfo[toMutate^.literalType].name,location);
      old:=toMutate;
      toMutate:=old^.clone;
      old^.unreference;
    end;

  FUNCTION mutateDrop(VAR toMutate:P_literal; CONST RHS:P_literal):boolean;
    begin
      result:=false;
      if (toMutate^.literalType in C_setTypes) then begin
        ensureExclusiveAccess(P_setLiteral(toMutate));
        P_setLiteral(toMutate)^.drop(RHS);
      end else if (toMutate^.literalType in C_mapTypes) then begin
        ensureExclusiveAccess(P_mapLiteral(toMutate));
        P_mapLiteral(toMutate)^.drop(RHS);
      end else if (toMutate^.literalType in C_listTypes) and (RHS^.literalType=lt_int) then begin
        ensureExclusiveAccess(P_listLiteral(toMutate));
        P_listLiteral(toMutate)^.removeElement(P_intLiteral(RHS)^.val.toInt);
      end else adapters.raiseError('Cannot drop from literal of type '+toMutate^.typeString,location);
      return(newVoidLiteral);
    end;

  FUNCTION mutateNested(VAR toMutate:P_literal; CONST nestedMutation:T_tokenType; accessor:P_listLiteral; RHS:P_literal):boolean;
    VAR elementToMutate:P_literal=nil;
        listIndex:int64;
        accessorTail:P_listLiteral;
        mapEntry:T_literalKeyLiteralValueMap.P_CACHE_ENTRY;
        prevType:T_literalType;
    begin
      if accessor^.size=0 then case nestedMutation of
        tt_mut_nested_assign  : exit(assign            (toMutate,                   RHS));
        tt_mut_nestedPlus     : exit(simpleMutate      (toMutate,tt_operatorPlus   ,RHS));
        tt_mut_nestedMinus    : exit(simpleMutate      (toMutate,tt_operatorMinus  ,RHS));
        tt_mut_nestedMult     : exit(simpleMutate      (toMutate,tt_operatorMult   ,RHS));
        tt_mut_nestedDiv      : exit(simpleMutate      (toMutate,tt_operatorDivReal,RHS));
        tt_mut_nestedStrConcat: exit(mutateStringAppend(toMutate,                   RHS));
        tt_mut_nestedAppend   : exit(mutateListAppend  (toMutate,                   RHS,false));
        tt_mut_nestedAppendAlt: exit(mutateListAppend  (toMutate,                   RHS,true));
        tt_mut_nestedDrop     : exit(mutateDrop        (toMutate,                   RHS));
      end;
      accessorTail:=accessor^.tail;
      result:=false;
      if toMutate^.literalType in C_listTypes then begin
        if accessor^.value[0]^.literalType=lt_int then begin
          listIndex:=P_intLiteral(accessor^.value[0])^.val.toInt;
          if (listIndex>=0) and (listIndex<P_listLiteral(toMutate)^.fill) then begin
            ensureExclusiveAccess(P_listLiteral(toMutate));
            prevType:=P_listLiteral(toMutate)^.dat[listIndex]^.literalType;
            if mutateNested(P_listLiteral(toMutate)^.dat[listIndex],nestedMutation,accessorTail,RHS) then begin
              if prevType<>P_listLiteral(toMutate)^.dat[listIndex]^.literalType then begin
                case prevType of
                  lt_boolean: dec(P_listLiteral(toMutate)^.booleans);
                  lt_int:     dec(P_listLiteral(toMutate)^.ints);
                  lt_real:    dec(P_listLiteral(toMutate)^.reals);
                  lt_string:  dec(P_listLiteral(toMutate)^.strings);
                  else        dec(P_listLiteral(toMutate)^.others);
                end;
                case P_listLiteral(toMutate)^.dat[listIndex]^.literalType of
                  lt_boolean: inc(P_listLiteral(toMutate)^.booleans);
                  lt_int:     inc(P_listLiteral(toMutate)^.ints);
                  lt_real:    inc(P_listLiteral(toMutate)^.reals);
                  lt_string:  inc(P_listLiteral(toMutate)^.strings);
                  else        inc(P_listLiteral(toMutate)^.others);
                end;
                if P_listLiteral(toMutate)^.fill=0 then P_listLiteral(toMutate)^.literalType:=lt_emptyList
                else if P_listLiteral(toMutate)^.others>0 then P_listLiteral(toMutate)^.literalType:=lt_list
                else P_listLiteral(toMutate)^.literalType:=listType[P_listLiteral(toMutate)^.booleans>0,P_listLiteral(toMutate)^.ints>0,P_listLiteral(toMutate)^.reals>0,P_listLiteral(toMutate)^.strings>0];
              end;
            end;
          end else if listIndex=P_listLiteral(toMutate)^.fill then begin
            ensureExclusiveAccess(P_listLiteral(toMutate));
            elementToMutate:=newVoidLiteral;
            mutateNested(elementToMutate,nestedMutation,accessorTail,RHS);
            P_listLiteral(toMutate)^.append(elementToMutate,false);
          end else adapters.raiseError('List index out of bounds',location)
        end else adapters.raiseError('List elements must be qualified by their index',location);
      end else if toMutate^.literalType in C_mapTypes then begin
        ensureExclusiveAccess(P_mapLiteral(toMutate));
        mapEntry:=P_mapLiteral(toMutate)^.dat.getEntry(accessor^.value[0]);
        if (mapEntry<>nil) then begin
          mutateNested(mapEntry^.value,nestedMutation,accessorTail,RHS);
        end else begin
          elementToMutate:=newVoidLiteral;
          mutateNested(elementToMutate,nestedMutation,accessorTail,RHS);
          P_mapLiteral(toMutate)^.put(accessor^.value[0]^.rereferenced,elementToMutate,false);
        end;
      end else adapters.raiseError('Cannot apply nested mutation to literal of type '+toMutate^.typeString,location);
      disposeLiteral(accessorTail);
    end;

  PROCEDURE mutateNested;
    VAR accessor:P_listLiteral;
        RHS:P_literal;
        temp:P_listLiteral;
    begin
      accessor:=P_listLiteral(parameters)^.leading;
      RHS     :=P_listLiteral(parameters)^.trailing;
      if (RHS^.literalType=lt_void) then begin
        if mutation=tt_mut_nested_assign then begin
          // M[]:=void -> M:=void;
          if (accessor^.size=0) then assign(toMutate,RHS)
          // M[key  ]:=void -> M >> key
          else if (accessor^.size=1) then mutateDrop(toMutate,accessor^.value[0])
          // M[k1,k2]:=void -> M[k1] >> k2
          else begin
            disposeLiteral(RHS);
            temp:=accessor;
            accessor:=temp^.leading;
            RHS:=temp^.trailing;
            disposeLiteral(temp);
            mutateNested(toMutate,tt_mut_nestedDrop,accessor,RHS);
          end;
        end;
      end else mutateNested(toMutate,mutation,accessor,RHS);
      disposeLiteral(accessor);
      disposeLiteral(RHS);
    end;

  begin
    case mutation of
      tt_assign,
      tt_mutate,
      tt_assignNewBlockLocal,
      tt_assignExistingBlockLocal: assign            (toMutate,                   parameters);
      tt_mut_assignPlus          : simpleMutate      (toMutate,tt_operatorPlus   ,parameters);
      tt_mut_assignMinus         : simpleMutate      (toMutate,tt_operatorMinus  ,parameters);
      tt_mut_assignMult          : simpleMutate      (toMutate,tt_operatorMult   ,parameters);
      tt_mut_assignDiv           : simpleMutate      (toMutate,tt_operatorDivReal,parameters);
      tt_mut_assignStrConcat     : mutateStringAppend(toMutate,                   parameters);
      tt_mut_assignAppend        : mutateListAppend  (toMutate,                   parameters,false);
      tt_mut_assignAppendAlt     : mutateListAppend  (toMutate,                   parameters,true);
      tt_mut_assignDrop          : mutateDrop        (toMutate,                   parameters);

      tt_mut_nested_assign,
      tt_mut_nestedPlus,
      tt_mut_nestedMinus,
      tt_mut_nestedMult,
      tt_mut_nestedDiv,
      tt_mut_nestedStrConcat,
      tt_mut_nestedAppend,
      tt_mut_nestedAppendAlt,
      tt_mut_nestedDrop: if not(parameters^.literalType in C_listTypes) or not(toMutate^.literalType in C_compoundTypes) then begin
        adapters.raiseError('Nested mutation expects a compound literal on the left hand side and a list literal on the right hand side',location);
        adapters.raiseError('RHS: '+toMutate^.typeString+' '+toMutate^.toString(50),location);
        adapters.raiseError('LHS: '+parameters^.typeString+' '+parameters^.toString(50),location);
        exit(nil);
      end else mutateNested;
      else begin
        adapters.raiseError('Unimplemented mutation '+C_tokenInfo[mutation].defaultId,location);
      end;
    end;
    result:=returnValue;
  end;

FUNCTION newLiteralFromStream(CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
  VAR reusableLiterals:{$ifdef Windows}array[0..2097151] of P_literal;{$else}^P_literal;{$endif}
      reusableFill:longint=0;
      encodingMethod:byte=0;
  PROCEDURE errorOrException(CONST message:string);
    begin
      if adapters<>nil then adapters^.raiseError(message,location)
                       else raise Exception.create(message);
    end;

  FUNCTION typeStringOrNone(CONST t:T_literalType):string;
    begin
      if (t>=low(T_literalType)) and (t<=high(T_literalType)) then result:=C_typeInfo[t].name else result:='';
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
        if (reusableIndex<reusableFill) then begin
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
          then result:=newSetLiteral(listSize)
          else result:=newListLiteral(listSize);
          for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendBool(stream^.readBoolean);
        end;
        lt_intList,lt_intSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral(listSize)
          else result:=newListLiteral(listSize);
          for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendInt(stream^.readInt64);
        end;
        lt_realList,lt_realSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral(listSize)
          else result:=newListLiteral(listSize);
          for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendReal(stream^.readDouble);
        end;
        lt_stringList,lt_stringSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral(listSize)
          else result:=newListLiteral(listSize);
          for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendString(stream^.readAnsiString);
        end;
        lt_emptyList: result:=newListLiteral(0);
        lt_emptySet:  result:=newSetLiteral;
        lt_emptyMap:  result:=newMapLiteral;
        lt_void    :  result:=newVoidLiteral;
        lt_list,lt_set,
        lt_numList,lt_numSet:begin
          listSize:=stream^.readNaturalNumber;
          case literalType of
            lt_set,lt_numSet: result:=newSetLiteral(listSize);
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
            P_mapLiteral(result)^.dat.put(mapKey,mapValue);
          end;
          result^.literalType:=lt_map;
        end;
        else begin
          errorOrException('Read invalid literal type '+typeStringOrNone(literalType)+' ('+intToStr(literalByte)+') ! Abort.');
          stream^.logWrongTypeError;
          exit(newVoidLiteral);
        end;
      end;
      if (result^.literalType<>literalType) and (adapters<>nil) then errorOrException('Deserializaion result has other type ('+typeStringOrNone(result^.literalType)+') than expected ('+typeStringOrNone(literalType)+').');
      if not(stream^.allOkay) then errorOrException('Unknown error during deserialization.');
      if ((literalType=lt_string) or (literalType in C_compoundTypes)) and (reusableFill<2097151) then begin
        reusableLiterals[reusableFill]:=result;
        inc(reusableFill);
      end;
    end;

  FUNCTION literalFromStream254:P_literal;
    VAR literalType:T_literalType;
        reusableIndex:longint;
        listSize:longint;
        i:longint;
        mapKey,mapValue:P_literal;
        tempInt:T_bigInt;
    begin
      reusableIndex:=stream^.readNaturalNumber;
      if reusableIndex<=byte(high(T_literalType)) then literalType:=T_literalType(byte(reusableIndex))
      else begin
        dec(reusableIndex,byte(high(T_literalType))+1);
        if (reusableIndex<reusableFill) then begin
          result:=reusableLiterals[reusableIndex];
          result^.rereference;
        end else begin
          result:=newVoidLiteral;
          stream^.logWrongTypeError;
          errorOrException('Read invalid reuse index '+intToStr(reusableIndex)+'! Abort.');
        end;
        exit(result);
      end;
      case literalType of
        lt_boolean  : result:=newBoolLiteral  (stream^.readBoolean   );
        lt_int      : begin
                        tempInt.readFromStream(stream);
                        result:=newIntLiteral(tempInt);
                      end;
        lt_real     : result:=newRealLiteral  (stream^.readDouble    );
        lt_string   : result:=newStringLiteral(stream^.readAnsiString);
        lt_emptyList: result:=newListLiteral;
        lt_emptySet : result:=newSetLiteral;
        lt_emptyMap : result:=newMapLiteral;
        lt_void     : result:=newVoidLiteral;
        lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList,
        lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral(listSize)
          else result:=newListLiteral(listSize);
          case literalType of
            lt_booleanList,lt_booleanSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendBool(stream^.readBoolean);
            lt_intList,lt_intSet:
              for i:=0 to listSize-1 do if stream^.allOkay then begin
                tempInt.readFromStream(stream);
                P_collectionLiteral(result)^.append(newIntLiteral(tempInt),false);
              end;
            lt_realList,lt_realSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendReal(stream^.readDouble);
            lt_stringList,lt_stringSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendString(stream^.readAnsiString);
            else
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.append(literalFromStream254(),false);
          end;
          if P_collectionLiteral(result)^.size<>listSize then errorOrException('Invalid collection. Expected size of '+intToStr(listSize)+' but got '+result^.typeString);
        end;
        lt_map: begin
          result:=newMapLiteral;
          listSize:=stream^.readNaturalNumber;
          for i:=0 to listSize-1 do if stream^.allOkay then begin
            mapKey  :=literalFromStream254();
            mapValue:=literalFromStream254();
            P_mapLiteral(result)^.dat.put(mapKey,mapValue);
          end;
          result^.literalType:=lt_map;
        end;
        else begin
          errorOrException('Read invalid literal type '+typeStringOrNone(literalType)+' ('+intToStr(reusableIndex)+') ! Abort.');
          stream^.logWrongTypeError;
          exit(newVoidLiteral);
        end;
      end;
      if (result^.literalType<>literalType) then errorOrException('Deserializaion result has other type ('+typeStringOrNone(result^.literalType)+') than expected ('+typeStringOrNone(literalType)+').');
      if not(stream^.allOkay) then errorOrException('Unknown error during deserialization.');
      if ((literalType=lt_string) or (literalType in C_compoundTypes)) and (reusableFill<2097151) then begin
        reusableLiterals[reusableFill]:=result;
        inc(reusableFill);
      end;
    end;

  begin
    {$ifndef Windows}
    getMem(reusableLiterals,sizeOf(P_literal)*2097151);
    {$endif}
    encodingMethod:=stream^.readByte;
    case encodingMethod of
      255: result:=literalFromStream255;
      254: result:=literalFromStream254;
      else begin
        errorOrException('Invalid literal encoding type '+intToStr(encodingMethod));
        result:=newVoidLiteral;
      end;
    end;
    {$ifndef Windows}
    freeMem(reusableLiterals,sizeOf(P_literal)*2097151);
    {$endif}
  end;

PROCEDURE writeLiteralToStream(CONST L:P_literal; CONST stream:P_outputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR reusableMap:specialize G_literalKeyMap<longint>;
      previousMapValueDummy:longint;
      mapEntry:T_literalKeyLiteralValueMap.CACHE_ENTRY;

 PROCEDURE writeLiteral(CONST L:P_literal);
    VAR reusableIndex:longint;
        x:P_literal;
        iter:T_arrayOfLiteral;
    begin
      reusableIndex:=reusableMap.get(L,2097151);
      if reusableIndex<2097151 then begin
        inc(reusableIndex,byte(high(T_literalType))+1);
        stream^.writeNaturalNumber(reusableIndex);
        exit;
      end;
      stream^.writeNaturalNumber(byte(L^.literalType));
      case L^.literalType of
        lt_boolean:stream^.writeBoolean   (P_boolLiteral  (L)^.val);
        lt_int:    P_intLiteral(L)^.val.writeToStream(stream);
        lt_real:   stream^.writeDouble    (P_realLiteral  (L)^.val);
        lt_string: stream^.writeAnsiString(P_stringLiteral(L)^.val);
        lt_booleanList,lt_booleanSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_compoundLiteral(L)^.iteratableList;
          for x in iter do if (adapters=nil) or (adapters^.noErrors) then stream^.writeBoolean(P_boolLiteral(x)^.val);
          disposeLiteral(iter);
        end;
        lt_intList,lt_intSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_compoundLiteral(L)^.iteratableList;
          for x in iter do if (adapters=nil) or (adapters^.noErrors) then P_intLiteral(x)^.val.writeToStream(stream);
          disposeLiteral(iter);
        end;
        lt_realList,lt_realSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_compoundLiteral(L)^.iteratableList;
          for x in iter do if (adapters=nil) or (adapters^.noErrors) then stream^.writeDouble(P_realLiteral(x)^.val);
          disposeLiteral(iter);
        end;
        lt_stringList,lt_stringSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_compoundLiteral(L)^.iteratableList;
          for x in iter do if (adapters=nil) or (adapters^.noErrors) then stream^.writeAnsiString(P_stringLiteral(x)^.val);
          disposeLiteral(iter);
        end;
        lt_void, lt_emptyList,lt_emptySet,lt_emptyMap: begin end; //completely defined by type
        lt_list,lt_set,
        lt_numList,lt_numSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_compoundLiteral(L)^.iteratableList;
          for x in iter do if (adapters=nil) or (adapters^.noErrors) then writeLiteral(x);
          disposeLiteral(iter);
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
    reusableMap.create();
    stream^.writeByte(254);
    writeLiteral(L);
    reusableMap.destroy;
  end;

FUNCTION serializeToStringList(CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST maxLineLength:longint=128):T_arrayOfString;
  VAR indent:longint=0;
      prevLines:T_arrayOfString;
      nextLine:ansistring;

  PROCEDURE appendSeparator;
    begin
      nextLine:=nextLine+',';
    end;

  PROCEDURE appendPart(CONST part:string);
    begin
      if length(nextLine)+length(part)>maxLineLength then begin
        append(prevLines,nextLine);
        nextLine:=StringOfChar(' ',indent);
      end;
      nextLine:=nextLine+part;
    end;

  PROCEDURE ser(CONST L:P_literal; CONST outerIndex:longint);
    VAR iter:T_arrayOfLiteral;
        k:longint;
        sortedTemp:P_listLiteral=nil;
    begin
      case L^.literalType of
        lt_boolean,lt_int,lt_string,lt_real,lt_void: appendPart(L^.toString);
        lt_expression: begin
          P_expressionLiteral(L)^.validateSerializability(adapters);
          if (adapters=nil) or (adapters^.noErrors) then appendPart(L^.toString);
        end;
        lt_list..lt_emptyList,
        lt_set ..lt_emptySet,
        lt_map ..lt_emptyMap:
        begin
          if outerIndex>0 then begin
            append(prevLines,nextLine);
            nextLine:=StringOfChar(' ',indent);
          end;
          appendPart('[');
          inc(indent);
          if L^.literalType in [lt_set..lt_emptySet,lt_map..lt_emptyMap] then begin
            sortedTemp:=P_compoundLiteral(L)^.toList;
            sortedTemp^.sort;
            iter:=sortedTemp^.iteratableList;
            disposeLiteral(sortedTemp);
          end else iter:=P_compoundLiteral(L)^.iteratableList;
          for k:=0 to length(iter)-1 do if (adapters=nil) or (adapters^.noErrors) then begin
            ser(iter[k],k);
            if k<length(iter)-1 then appendSeparator;
          end;
          disposeLiteral(iter);
          dec(indent);
          case L^.literalType of
            lt_list..lt_emptyList: appendPart(']');
            lt_set ..lt_emptySet : appendPart('].toSet');
            lt_map ..lt_emptyMap : appendPart('].toMap');
          end;
          if L^.customType<>nil then appendPart('.to'+L^.customType^.name);
        end;
        else if adapters<>nil then adapters^.raiseError('Literal of type '+L^.typeString+' ('+L^.toString+') cannot be serialized',location);
      end;
    end;

  begin
    setLength(prevLines,0);
    nextLine:='';
    ser(L,0);
    if length(nextLine)>0 then append(prevLines,nextLine);
    result:=prevLines;
  end;

FUNCTION serialize(CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters):ansistring;
  VAR wrapper:T_outputStreamWrapper;
      stream:TStringStream;
  begin
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
  initialize(boolLit);
  boolLit[false].create(false);
  boolLit[true ].create(true);
  voidLit.create();
  emptyStringSingleton.create('');
  for i:=low(intLit) to high(intLit) do intLit[i].create(i);
  for i:=0 to 255 do charLit[chr(i)].create(chr(i));
  DefaultFormatSettings.DecimalSeparator:='.';
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  initialize(singletonCs);
  initCriticalSection(singletonCs);
  stringSingletons.create(@disposeLiteral);

FINALIZATION
  boolLit[false].destroy;
  boolLit[true ].destroy;
  voidLit.destroy;
  emptyStringSingleton.destroy;
  for i:=low(intLit) to high(intLit) do intLit[i].destroy;
  for i:=0 to 255 do charLit[chr(i)].destroy;
  doneCriticalSection(singletonCs);
end.
