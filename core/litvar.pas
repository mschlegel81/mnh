UNIT litVar;
{$Q-}
INTERFACE
USES myGenerics, myStringUtil, serializationUtil, bigint,
     mnh_constants,
     basicTypes,
     mnh_messages,
     out_adapters;

CONST HASH_GROWTH_THRESHOLD_FACTOR=1.5;
      HASH_SHRINK_THRESHOLD_FACTOR=1.2/HASH_GROWTH_THRESHOLD_FACTOR;
TYPE T_expressionType=(et_builtin          ,
                       et_builtinIteratable,
                       et_builtinAsyncOrFuture    ,
                       et_subrule          ,
                       et_inline           ,
                       et_subruleIteratable,
                       et_inlineIteratable ,
                       et_subruleStateful  ,
                       et_inlineStateful   ,
                       et_eachBody         ,
                       et_whileBody        ,
                       et_builtinStateful);

CONST C_builtinExpressionTypes:set of T_expressionType=[et_builtin,et_builtinStateful,et_builtinIteratable,et_builtinAsyncOrFuture];
      C_subruleExpressionTypes:set of T_expressionType=[et_subrule,et_subruleIteratable,et_subruleStateful];
      C_inlineExpressionTypes:set of T_expressionType =[et_inline,et_inlineIteratable,et_inlineStateful];
      C_statefulExpressionTypes:set of T_expressionType=[et_builtinStateful,et_builtinIteratable,et_builtinAsyncOrFuture,
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
        'whileBody',
        'builtin stateful');

TYPE
  P_typedef=^T_typedef;
  T_typeMap=specialize G_stringKeyMap<P_typedef>;
  P_literal = ^T_literal;
  PP_literal = ^P_literal;
  P_setOfPointer=^T_setOfPointer;
  T_arrayOfLiteral=array of P_literal;
  T_keyValuePair=record
    key,value:P_literal;
  end;
  T_arrayOfKeyValuePair=array of T_keyValuePair;
  P_literalRecycler=^T_literalRecycler;
  T_literal = object(T_objectWithIdAndLocation)
  private
    numberOfReferences: longint;
  public
    literalType:T_literalType;
    CONSTRUCTOR init(CONST lt:T_literalType);
    DESTRUCTOR destroy; virtual;
    PROCEDURE rereference; inline;
    FUNCTION rereferenced:P_literal; inline;
    FUNCTION unreference: longint; inline;
    PROPERTY getReferenceCount: longint read numberOfReferences;

    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION typeString:string; virtual;

    FUNCTION getId:T_idString; virtual;
    FUNCTION getLocation:T_tokenLocation; virtual;
    PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
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
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_numericLiteral=^T_numericLiteral;
  T_numericLiteral=object(T_literal)
    FUNCTION floatValue:T_myFloat; virtual; abstract;
  end;

  P_abstractIntLiteral=^T_abstractIntLiteral;
  T_abstractIntLiteral=object(T_numericLiteral)
    FUNCTION intValue:int64;                                              virtual; abstract;
    FUNCTION isBetween(CONST lowInclusive,highInclusive:longint):boolean; virtual; abstract;
    PROCEDURE writeToStream(CONST stream:P_outputStreamWrapper);          virtual; abstract;
    FUNCTION toHexString:string;                                          virtual; abstract;
  end;

  P_bigIntLiteral = ^T_bigIntLiteral;
  T_bigIntLiteral = object(T_abstractIntLiteral)
  private
    val: T_bigInt;
    CONSTRUCTOR create(CONST value: T_bigInt);
    CONSTRUCTOR create(CONST value: int64);
  public
    DESTRUCTOR destroy; virtual;
    PROPERTY value:T_bigInt read val;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION intValue:int64;                                              virtual;
    FUNCTION floatValue:T_myFloat;                                        virtual;
    FUNCTION isBetween(CONST lowInclusive,highInclusive:longint):boolean; virtual;
    PROCEDURE writeToStream(CONST stream:P_outputStreamWrapper);          virtual;
    FUNCTION toHexString:string;                                          virtual;
    PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
  end;

  P_smallIntLiteral = ^T_smallIntLiteral;
  T_smallIntLiteral = object(T_abstractIntLiteral)
  private
    val: longint;
    CONSTRUCTOR create(CONST value: longint);
  public
    PROPERTY value:longint read val;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION intValue:int64;                                              virtual;
    FUNCTION floatValue:T_myFloat;                                        virtual;
    FUNCTION isBetween(CONST lowInclusive,highInclusive:longint):boolean; virtual;
    PROCEDURE writeToStream(CONST stream:P_outputStreamWrapper);          virtual;
    FUNCTION toHexString:string;                                          virtual;
  end;

  P_realLiteral = ^T_realLiteral;
  T_realLiteral = object(T_numericLiteral)
  private
    val: T_myFloat;
    CONSTRUCTOR create(CONST value: T_myFloat);
  public
    PROPERTY value:T_myFloat read val;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION floatValue:T_myFloat;                           virtual;
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
    FUNCTION softCast(CONST literalRecycler:P_literalRecycler): P_literal;
    FUNCTION getEncoding: T_stringEncoding;
    PROCEDURE append(CONST suffix:ansistring);
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
  end;

  T_reduceResult=(rr_fail,rr_ok,rr_okWithReturn,rr_patternMismatch);
  T_evaluationResult=record
    literal:P_literal;
    reasonForStop:T_reduceResult;
  end;
  P_typableLiteral   = ^T_typableLiteral;
  T_typableLiteral=object(T_literal)
    customType:P_typedef;
  end;

  T_arityInfo=record minPatternLength,maxPatternLength:longint; end;

  P_compoundLiteral  = ^T_compoundLiteral;
  P_listLiteral      = ^T_listLiteral    ;
  P_setLiteral       = ^T_setLiteral     ;
  P_mapLiteral       = ^T_mapLiteral     ;
  P_expressionLiteral = ^T_expressionLiteral;
  T_expressionList = array of P_expressionLiteral;
  T_expressionLiteral = object(T_typableLiteral)
    private
      expressionType:T_expressionType;
      declaredAt:T_tokenLocation;
      myHash:T_hashInt;
    public
      CONSTRUCTOR create(CONST eType:T_expressionType; CONST location:T_tokenLocation);
      PROPERTY typ:T_expressionType read expressionType;
      FUNCTION evaluateToBoolean(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST allowRaiseError:boolean; CONST a:P_literal; CONST b:P_literal):boolean; virtual; abstract;
      FUNCTION evaluateToDouble (CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST allowRaiseError:boolean; CONST a:P_literal; CONST b:P_literal):double;  virtual; abstract;
      FUNCTION evaluateToLiteral(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal; CONST b:P_literal):T_evaluationResult; virtual; abstract;
      FUNCTION evaluate         (CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST parameters:P_listLiteral):T_evaluationResult;               virtual; abstract;
      FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer):P_expressionLiteral; virtual; abstract;
      FUNCTION arity:T_arityInfo; virtual; abstract;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual; abstract;

      PROCEDURE makeStateful  (CONST context:P_abstractContext; CONST location:T_tokenLocation);
      PROCEDURE makeIteratable(CONST context:P_abstractContext; CONST location:T_tokenLocation);

      FUNCTION getParentId:T_idString; virtual; abstract;
      PROCEDURE validateSerializability(CONST adapters:P_messages); virtual; abstract;
      FUNCTION typeString:string; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION getLocation:T_tokenLocation; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
      FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
      FUNCTION clone(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_expressionLiteral; virtual; abstract;
      FUNCTION writeToStream(CONST literalRecycler:P_literalRecycler; CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean; virtual; abstract;
      FUNCTION referencesAnyUserPackage:boolean; virtual; abstract;
      FUNCTION mustBeDroppedBeforePop:boolean; virtual;
  end;

  T_typedef=object(T_objectWithIdAndLocation)
    private
      name:T_idString;
      super:P_typedef;
      builtinsuper:T_typeCheck;
      builtinsuperModifier:longint;
      ducktyperule:P_expressionLiteral;
      ducktyping:boolean;
      alwaysTrue:boolean;
      FUNCTION cloneLiteral(CONST literalRecycler:P_literalRecycler; CONST L:P_typableLiteral; CONST location:T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer):P_typableLiteral;
    public
      CONSTRUCTOR create(CONST id:T_idString; CONST builtinCheck:T_typeCheck; CONST builtinCheckPar:longint; CONST super_:P_typedef; CONST typerule:P_expressionLiteral; CONST ducktyping_,alwaysTrue_:boolean);
      DESTRUCTOR destroy;
      FUNCTION matchesLiteral(CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer):boolean;
      FUNCTION cast(CONST literalRecycler:P_literalRecycler; CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:P_abstractContext;  CONST recycler:pointer):P_typableLiteral;
      FUNCTION uncast(CONST literalRecycler:P_literalRecycler; CONST L:P_literal; CONST location:T_tokenLocation; CONST threadContext:P_abstractContext; CONST adapters:P_messages; CONST recycler:pointer):P_literal;
      PROPERTY getName:T_idString read name;
      PROPERTY getSuper:P_typedef read super;
      PROPERTY builtinTypeCheck:T_typeCheck read builtinsuper;
      PROPERTY builtinSuperParameter:longint read builtinsuperModifier;
      PROPERTY isDucktyping:boolean read ducktyping;
      PROPERTY isAlwaysTrue:boolean read alwaysTrue;
      FUNCTION getId:T_idString; virtual;
      FUNCTION getLocation:T_tokenLocation; virtual;
      FUNCTION getDocTxt:string;
      PROPERTY getDuckTypeRule:P_expressionLiteral read ducktyperule;
      PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler);
  end;

  GENERIC G_literalKeyMap<VALUE_TYPE>= object
    TYPE CACHE_ENTRY=record
           key:P_literal;
           keyHash:T_hashInt;
           value:VALUE_TYPE;
         end;
         P_CACHE_ENTRY=^CACHE_ENTRY;
         KEY_VALUE_LIST=array of CACHE_ENTRY;
         BIN_ENTRY=record
           binFill:longint;
           arr:KEY_VALUE_LIST;
         end;
         P_BIN_ENTRY=^BIN_ENTRY;
         MY_TYPE=specialize G_literalKeyMap<VALUE_TYPE>;
    VAR bin:array of BIN_ENTRY;
        fill:longint;
    CONSTRUCTOR create(CONST expectedSize:longint=0);
    CONSTRUCTOR createClone(VAR map:MY_TYPE);
    DESTRUCTOR destroy;
    PROCEDURE rehash(CONST grow:boolean);
    PROCEDURE put(CONST key:P_literal; CONST value:VALUE_TYPE);
    FUNCTION putNew(CONST newEntry:CACHE_ENTRY; OUT previousValue:VALUE_TYPE):boolean;
    FUNCTION putNew(CONST key:P_literal; CONST value:VALUE_TYPE; OUT previousValue:VALUE_TYPE):boolean;
    FUNCTION get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
    FUNCTION getEntry(CONST key:P_literal):P_CACHE_ENTRY;
    FUNCTION containsKey(CONST entry:CACHE_ENTRY):boolean;
    FUNCTION drop(CONST key:P_literal):CACHE_ENTRY;
    FUNCTION keyValueList:KEY_VALUE_LIST;
    FUNCTION keySet:T_arrayOfLiteral;
    {$ifdef debugMode}
    PROCEDURE writeDump;
    {$endif}
  end;

  P_literalKeyBooleanValueMap=^T_literalKeyBooleanValueMap;
  T_literalKeyBooleanValueMap=specialize G_literalKeyMap<boolean>;
  P_literalKeyLiteralValueMap=^T_literalKeyLiteralValueMap;
  T_literalKeyLiteralValueMap=specialize G_literalKeyMap<P_literal>;
  P_stringKeyLiteralValueMap=^T_stringKeyLiteralValueMap;
  T_stringKeyLiteralValueMap=specialize G_stringKeyMap<P_literal>;

  T_compoundLiteral=object(T_typableLiteral)
    private
      myHash:T_hashInt;
    public
    FUNCTION toSet (CONST literalRecycler:P_literalRecycler):P_setLiteral;
    FUNCTION toList(CONST literalRecycler:P_literalRecycler):P_listLiteral;
    FUNCTION toMap (CONST literalRecycler:P_literalRecycler; CONST location:T_tokenLocation; CONST context:P_abstractContext):P_mapLiteral;
    FUNCTION get     (CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal; virtual; abstract;
    FUNCTION getInner(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal; virtual; abstract;
    FUNCTION typeString:string; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION size:longint;                        virtual; abstract;
    FUNCTION contains(CONST L:P_literal):boolean; virtual; abstract;
    FUNCTION clone(CONST literalRecycler:P_literalRecycler):P_compoundLiteral; virtual; abstract;
    FUNCTION forcedIteratableList(CONST literalRecycler:P_literalRecycler):T_arrayOfLiteral; virtual; abstract;
  end;

  P_collectionLiteral=^T_collectionLiteral;
  T_collectionLiteral=object(T_compoundLiteral)
    private
      ints,reals,strings,booleans,others:longint;
    public
    FUNCTION isKeyValueCollection:boolean; virtual; abstract;
    FUNCTION newOfSameType(CONST literalRecycler:P_literalRecycler; CONST initSize:boolean):P_collectionLiteral; virtual; abstract;
    FUNCTION appendAll   (CONST literalRecycler:P_literalRecycler; CONST L:P_compoundLiteral):P_collectionLiteral; virtual;
    FUNCTION append      (CONST literalRecycler:P_literalRecycler; CONST L:P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false):P_collectionLiteral; virtual; abstract;
    FUNCTION appendString(CONST literalRecycler:P_literalRecycler; CONST s:ansistring):P_collectionLiteral;
    FUNCTION appendBool  (CONST literalRecycler:P_literalRecycler; CONST b:boolean   ):P_collectionLiteral;
    FUNCTION appendInt   (CONST literalRecycler:P_literalRecycler; CONST i:int64     ):P_collectionLiteral;
    FUNCTION appendReal  (CONST literalRecycler:P_literalRecycler; CONST r:T_myFloat ):P_collectionLiteral;
    FUNCTION tempIteratableList:T_arrayOfLiteral; virtual; abstract;
    FUNCTION forcedIteratableList(CONST literalRecycler:P_literalRecycler):T_arrayOfLiteral; virtual;
  end;

  T_listLiteral=object(T_collectionLiteral)
    private
      dat:T_arrayOfLiteral;
      fill:longint;
      PROCEDURE modifyType(CONST L:P_literal); {$ifndef profilingFlavour}inline;{$endif}
    public
      PROPERTY value:T_arrayOfLiteral read dat;
      CONSTRUCTOR create(CONST initialSize:longint);
      DESTRUCTOR destroy; virtual;
      FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION equals(CONST other: P_literal): boolean; virtual;
      FUNCTION isKeyValuePair:boolean;
      FUNCTION isKeyValueCollection:boolean; virtual;
      FUNCTION newOfSameType(CONST literalRecycler:P_literalRecycler; CONST initSize:boolean):P_collectionLiteral; virtual;
      FUNCTION size:longint;        virtual;
      FUNCTION contains(CONST other:P_literal):boolean; virtual;
      FUNCTION listConstructorToString(CONST lengthLimit:longint=maxLongint):string;
      FUNCTION get     (CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal; virtual;
      FUNCTION getInner(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal; virtual;
      FUNCTION appendConstructing(CONST literalRecycler:P_literalRecycler; CONST L: P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST doRangeAppend:boolean):P_compoundLiteral;
      FUNCTION append(CONST literalRecycler:P_literalRecycler; CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false):P_collectionLiteral; virtual;
      FUNCTION clone(CONST literalRecycler:P_literalRecycler):P_compoundLiteral; virtual;
      FUNCTION tempIteratableList:T_arrayOfLiteral; virtual;

      PROCEDURE sort;
      PROCEDURE sortBySubIndex(CONST innerIndex:longint; CONST location:T_tokenLocation; CONST context:P_abstractContext);
      PROCEDURE customSort(CONST leqExpression: P_expressionLiteral; CONST location: T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer);
      FUNCTION  sortPerm(CONST literalRecycler:P_literalRecycler): P_listLiteral;
      PROCEDURE unique(CONST literalRecycler:P_literalRecycler);

      FUNCTION head(CONST literalRecycler:P_literalRecycler                          ):P_literal;
      FUNCTION head(CONST literalRecycler:P_literalRecycler; CONST headSize : longint):P_listLiteral;
      FUNCTION tail(CONST literalRecycler:P_literalRecycler                          ): P_listLiteral;
      FUNCTION tail(CONST literalRecycler:P_literalRecycler; CONST headSize : longint):P_listLiteral;
      FUNCTION leading(CONST literalRecycler:P_literalRecycler                          ): P_listLiteral;
      FUNCTION leading(CONST literalRecycler:P_literalRecycler; CONST trailSize: longint):P_listLiteral;
      FUNCTION trailing(CONST literalRecycler:P_literalRecycler                          ):P_literal;
      FUNCTION trailing(CONST literalRecycler:P_literalRecycler; CONST trailSize: longint):P_listLiteral;
      FUNCTION transpose(CONST literalRecycler:P_literalRecycler; CONST filler:P_literal): P_listLiteral;
      PROCEDURE removeElement(CONST literalRecycler:P_literalRecycler; CONST index:longint);
      PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
    end;

  T_setLiteral=object(T_collectionLiteral)
    private
      dat:T_literalKeyBooleanValueMap;
      CONSTRUCTOR create(CONST expectedSize:longint);
      CONSTRUCTOR createClone(VAR original:T_setLiteral);
      PROCEDURE modifyType(CONST L:P_literal); {$ifndef profilingFlavour}inline;{$endif}
    public
      DESTRUCTOR destroy; virtual;
      FUNCTION isKeyValueCollection:boolean; virtual;
      FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION equals(CONST other: P_literal): boolean; virtual;
      FUNCTION newOfSameType(CONST literalRecycler:P_literalRecycler; CONST initSize:boolean):P_collectionLiteral; virtual;
      FUNCTION size:longint; virtual;
      FUNCTION contains(CONST other:P_literal):boolean; virtual;
      FUNCTION get     (CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal; virtual;
      FUNCTION getInner(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal; virtual;
      FUNCTION append(CONST literalRecycler:P_literalRecycler; CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false):P_collectionLiteral; virtual;
      FUNCTION appendAll(CONST literalRecycler:P_literalRecycler; CONST L:P_compoundLiteral):P_collectionLiteral; virtual;
      FUNCTION clone(CONST literalRecycler:P_literalRecycler):P_compoundLiteral; virtual;
      FUNCTION tempIteratableList:T_arrayOfLiteral; virtual;
      PROCEDURE drop(CONST literalRecycler:P_literalRecycler; CONST L:P_literal);
      PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
    end;

  T_mapLiteral=object(T_compoundLiteral)
    private
      dat:T_literalKeyLiteralValueMap;
      CONSTRUCTOR create(CONST expectedSize:longint);
      CONSTRUCTOR createClone(VAR original:T_mapLiteral);
    public
      DESTRUCTOR destroy; virtual;
      FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
      FUNCTION hash: T_hashInt; virtual;
      FUNCTION equals(CONST other: P_literal): boolean; virtual;
      FUNCTION size:longint; virtual;
      FUNCTION contains(CONST other:P_literal):boolean; virtual;
      FUNCTION get     (CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal; virtual;
      FUNCTION getInner(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal; virtual;
      FUNCTION clone(CONST literalRecycler:P_literalRecycler):P_compoundLiteral; virtual;
      FUNCTION entryList:T_arrayOfKeyValuePair;
      FUNCTION keyIteratableList:T_arrayOfLiteral;

      PROCEDURE drop(CONST literalRecycler:P_literalRecycler; CONST L:P_literal);
      FUNCTION put(CONST literalRecycler:P_literalRecycler; CONST key,                  newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
      FUNCTION put(CONST literalRecycler:P_literalRecycler; CONST key,                  newValue:ansistring                      ):P_mapLiteral;
      FUNCTION put(CONST literalRecycler:P_literalRecycler; CONST key:ansistring; CONST newValue:int64                           ):P_mapLiteral;
      FUNCTION put(CONST literalRecycler:P_literalRecycler; CONST key:ansistring; CONST newValue:T_myFloat                       ):P_mapLiteral;
      FUNCTION put(CONST literalRecycler:P_literalRecycler; CONST key:ansistring; CONST newValue:boolean                         ):P_mapLiteral;
      FUNCTION put(CONST literalRecycler:P_literalRecycler; CONST key:ansistring; CONST newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
      FUNCTION put(CONST literalRecycler:P_literalRecycler; CONST key:P_literal;  CONST newValue:int64    ; CONST incRefs:boolean):P_mapLiteral;
      FUNCTION putAll(CONST literalRecycler:P_literalRecycler; CONST map:P_mapLiteral):P_mapLiteral;
      FUNCTION transpose(CONST literalRecycler:P_literalRecycler): P_listLiteral;
      PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
      FUNCTION forcedIteratableList(CONST literalRecycler:P_literalRecycler):T_arrayOfLiteral; virtual;
      FUNCTION underlyingMap:P_literalKeyLiteralValueMap;
      PROCEDURE ensureType;
  end;

  T_literalRecycler=object
    private
      smallIntLiterals:record dat:array of P_smallIntLiteral; fill:longint; end;
      bigIntLiterals  :record dat:array of P_bigIntLiteral;   fill:longint; end;
      realLiterals    :record dat:array of P_realLiteral;     fill:longint; end;
      stringLiterals  :record dat:array of P_stringLiteral;   fill:longint; end;
      listLiterals    :record dat:array of P_listLiteral;     fill:longint; end;
    public
      PROCEDURE freeMemory(CONST hard:boolean);
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE disposeLiteral(VAR l:P_literal);
      PROCEDURE disposeLiteral(VAR l: T_arrayOfLiteral);
      PROCEDURE disposeLiteral(VAR l: T_arrayOfKeyValuePair);
      FUNCTION newBoolLiteral  (CONST value: boolean       ): P_boolLiteral;
      FUNCTION newBigIntLiteral(value: T_bigInt): P_abstractIntLiteral;
      FUNCTION newIntLiteral   (CONST value: int64         ): P_abstractIntLiteral;
      FUNCTION newIntLiteral   (CONST value: T_bigInt      ): P_abstractIntLiteral;
      FUNCTION newStringLiteral(CONST value: ansistring; CONST enforceNewString:boolean=false): P_stringLiteral;
      FUNCTION newRealLiteral  (CONST value: T_myFloat     ): P_realLiteral;
      FUNCTION newListLiteral  (CONST initialSize:longint=2): P_listLiteral;
      FUNCTION newListLiteral  (CONST a:P_literal;
                                CONST b:P_literal=nil)      : P_listLiteral;
    end;

CONST
  NIL_EVAL_RESULT:T_evaluationResult=(literal:nil; reasonForStop:rr_ok);
  NO_ARITY_INFO  :T_arityInfo       =(minPatternLength:-1; maxPatternLength:-2);
VAR
  resolveOperatorCallback: FUNCTION (CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:pointer): P_literal;
  readExpressionFromStreamCallback: FUNCTION(CONST literalRecycler:P_literalRecycler; CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):P_expressionLiteral;
FUNCTION commonArity(CONST x,y:T_arityInfo):T_arityInfo;
FUNCTION exp(CONST x:double):double; inline;

FUNCTION newBoolLiteral  (CONST value: boolean       ): P_boolLiteral;
FUNCTION newVoidLiteral                               : P_voidLiteral;
FUNCTION newSetLiteral(CONST expectedSize:longint)    : P_setLiteral;
FUNCTION newMapLiteral(CONST expectedSize:longint)    : P_mapLiteral;

FUNCTION myFloatToStr(CONST x: T_myFloat): string;
FUNCTION parseNumber(CONST input: ansistring; CONST offset:longint; CONST suppressOutput: boolean; CONST literalRecycler:P_literalRecycler; OUT parsedLength: longint): P_literal; inline;

FUNCTION newLiteralFromStream(CONST literalRecycler:P_literalRecycler; CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):P_literal;
PROCEDURE writeLiteralToStream(CONST literalRecycler:P_literalRecycler; CONST L:P_literal; CONST stream:P_outputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages);
FUNCTION serializeToStringList(CONST L:P_literal; CONST location:T_searchTokenLocation; CONST adapters:P_messages; CONST maxLineLength:longint=128):T_arrayOfString;

FUNCTION serialize(CONST literalRecycler:P_literalRecycler; CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_messages):ansistring;
FUNCTION deserialize(CONST literalRecycler:P_literalRecycler; CONST source:ansistring; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):P_literal;
FUNCTION toParameterListString(CONST list:P_listLiteral; CONST isFinalized: boolean; CONST lengthLimit:longint=maxLongint): ansistring;
FUNCTION parameterListTypeString(CONST list:P_listLiteral):string;

FUNCTION setUnion    (CONST literalRecycler:P_literalRecycler; CONST params:P_listLiteral):P_setLiteral;
FUNCTION setIntersect(CONST literalRecycler:P_literalRecycler; CONST params:P_listLiteral):P_setLiteral;
FUNCTION setMinus    (CONST literalRecycler:P_literalRecycler; CONST params:P_listLiteral):P_setLiteral;
FUNCTION mapMerge    (CONST literalRecycler:P_literalRecycler; CONST params:P_listLiteral; CONST location:T_tokenLocation; CONST contextPointer:P_abstractContext; CONST recycler:pointer):P_mapLiteral;
FUNCTION mutateVariable(CONST literalRecycler:P_literalRecycler; VAR toMutate:P_literal; CONST mutation:T_tokenType; CONST parameters:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
FUNCTION divideInts(CONST literalRecycler:P_literalRecycler; CONST LHS,RHS:P_abstractIntLiteral):P_numericLiteral;
FUNCTION typeCheckAccept(CONST valueToCheck:P_literal; CONST check:T_typeCheck; CONST modifier:longint=-1):boolean; inline;

VAR emptyStringSingleton: T_stringLiteral;
VAR boolLit       : array[false..true] of T_boolLiteral;
    charLit       : array[#0..#255] of T_stringLiteral;
    nanLit,
    infLit,
    negInfLit     : T_realLiteral;
CONST maxSingletonInt=1000;
IMPLEMENTATION
USES sysutils, math,
     Classes,LazUTF8;
CONST C_listType:array[false..true,false..true,false..true,false..true] of T_literalType=
         ((((lt_emptyList  ,lt_stringList),    //                     + string?
            (lt_realList   ,lt_list      )),   //              + real + string?
           ((lt_intList    ,lt_list      ),    //          int        + string?
            (lt_numList    ,lt_list      ))),  //          int + real + string?
          (((lt_booleanList,lt_list      ),    //boolean              + string?
            (lt_list       ,lt_list      )),   //boolean       + real + string?
           ((lt_list       ,lt_list      ),    //boolean + int        + string?
            (lt_list       ,lt_list      )))); //boolean + int + real + string?
CONST C_setType:array[false..true,false..true,false..true,false..true] of T_literalType=
         ((((lt_emptySet  ,lt_stringSet),    //                     + string?
            (lt_realSet   ,lt_set      )),   //              + real + string?
           ((lt_intSet    ,lt_set      ),    //          int        + string?
            (lt_numSet    ,lt_set      ))),  //          int + real + string?
          (((lt_booleanSet,lt_set      ),    //boolean              + string?
            (lt_set       ,lt_set      )),   //boolean       + real + string?
           ((lt_set       ,lt_set      ),    //boolean + int        + string?
            (lt_set       ,lt_set      )))); //boolean + int + real + string?

VAR
  intLit : array[-100..maxSingletonInt] of T_smallIntLiteral;
  voidLit: T_voidLiteral;

FUNCTION typeCheckAccept(CONST valueToCheck:P_literal; CONST check:T_typeCheck; CONST modifier:longint=-1):boolean;
  begin
    if not(valueToCheck^.literalType in C_typeCheckInfo[check].matching) then exit(false);
    if modifier<0 then case check of
      tc_typeCheckStatelessExpression : result:=not(P_expressionLiteral(valueToCheck)^.typ in C_statefulExpressionTypes);
      tc_typeCheckStatefulExpression  : result:=   (P_expressionLiteral(valueToCheck)^.typ in C_statefulExpressionTypes);
      tc_typeCheckIteratableExpression: result:=    P_expressionLiteral(valueToCheck)^.typ in C_iteratableExpressionTypes;
      tc_typeCheckIteratable          : result:=   (valueToCheck^.literalType<>lt_expression) or
                                                   (P_expressionLiteral(valueToCheck)^.typ in C_iteratableExpressionTypes);
      else result:=true;
    end else case check of
      tc_any: result:=true;
      tc_typeCheckList,       tc_typeCheckSet,       tc_typeCheckCollection,
      tc_typeCheckBoolList,   tc_typeCheckBoolSet,   tc_typeCheckBoolCollection,
      tc_typeCheckIntList,    tc_typeCheckIntSet,    tc_typeCheckIntCollection,
      tc_typeCheckRealList,   tc_typeCheckRealSet,   tc_typeCheckRealCollection,
      tc_typeCheckStringList, tc_typeCheckStringSet, tc_typeCheckStringCollection,
      tc_typeCheckNumList,    tc_typeCheckNumSet,    tc_typeCheckNumCollection,
      tc_typeCheckMap:                 result:=P_compoundLiteral  (valueToCheck)^.size =                       modifier ;
      tc_typeCheckExpression:          result:=P_expressionLiteral(valueToCheck)^.canApplyToNumberOfParameters(modifier);
      tc_typeCheckStatelessExpression: result:=not(P_expressionLiteral(valueToCheck)^.typ in C_statefulExpressionTypes) and
                                                   P_expressionLiteral(valueToCheck)^.canApplyToNumberOfParameters(modifier);
      tc_typeCheckStatefulExpression : result:=   (P_expressionLiteral(valueToCheck)^.typ in C_statefulExpressionTypes) and
                                                   P_expressionLiteral(valueToCheck)^.canApplyToNumberOfParameters(modifier);
      else result:=false;
    end;
  end;

FUNCTION divideInts(CONST literalRecycler:P_literalRecycler; CONST LHS,RHS:P_abstractIntLiteral):P_numericLiteral;
  VAR bigRHS,quotient,rest:T_bigInt;
  begin
    if RHS^.literalType=lt_bigint then begin
      if LHS^.literalType=lt_bigint then begin
        if P_bigIntLiteral(RHS)^.val.isZero then begin
          if      P_bigIntLiteral(LHS)^.val.isZero     then result:=P_realLiteral(   nanLit.rereferenced)
          else if P_bigIntLiteral(LHS)^.val.isNegative then result:=P_realLiteral(negInfLit.rereferenced)
          else                                              result:=P_realLiteral(   infLit.rereferenced);
        end else begin
          P_bigIntLiteral(LHS)^.val.divMod(P_bigIntLiteral(RHS)^.value,quotient,rest);
          if rest.isZero
          then begin
            rest.clear;
            result:=literalRecycler^.newIntLiteral(quotient);
          end else begin
            rest.clear;
            quotient.clear;
            result:=literalRecycler^.newRealLiteral(P_bigIntLiteral(LHS)^.val.toFloat/P_bigIntLiteral(RHS)^.val.toFloat);
          end;
        end;
      end else result:=literalRecycler^.newRealLiteral(P_smallIntLiteral(LHS)^.val/P_bigIntLiteral(RHS)^.val.toFloat);
        //LHS is small, RHS is big, so |LHS/RHS| < 1
    end else begin
      if LHS^.literalType=lt_bigint then begin
        if P_smallIntLiteral(RHS)^.val=0 then begin
          if      P_bigIntLiteral(LHS)^.val.isZero     then result:=P_realLiteral(   nanLit.rereferenced)
          else if P_bigIntLiteral(LHS)^.val.isNegative then result:=P_realLiteral(negInfLit.rereferenced)
          else                                              result:=P_realLiteral(   infLit.rereferenced);
        end else begin
          bigRHS.fromInt(P_smallIntLiteral(RHS)^.val);
          P_bigIntLiteral(LHS)^.val.divMod(bigRHS,quotient,rest);
          bigRHS.clear;
          if rest.isZero
          then begin
            rest.clear;
            result:=literalRecycler^.newIntLiteral(quotient);
          end else begin
            rest.clear;
            quotient.clear;
            result:=literalRecycler^.newRealLiteral(P_bigIntLiteral(LHS)^.val.toFloat/P_smallIntLiteral(RHS)^.val);
          end;
        end;
      end else begin
        if P_smallIntLiteral(RHS)^.val=0 then begin
          if      P_smallIntLiteral(LHS)^.val>0 then result:=P_realLiteral(   infLit.rereferenced)
          else if P_smallIntLiteral(LHS)^.val<0 then result:=P_realLiteral(negInfLit.rereferenced)
          else                                       result:=P_realLiteral(   nanLit.rereferenced);
        end else
          if P_smallIntLiteral(LHS)^.val mod P_smallIntLiteral(RHS)^.val=0
          then result:=literalRecycler^.newIntLiteral (P_smallIntLiteral(LHS)^.val div P_smallIntLiteral(RHS)^.val)
          else result:=literalRecycler^.newRealLiteral(P_smallIntLiteral(LHS)^.val  /  P_smallIntLiteral(RHS)^.val);
      end;
    end;
  end;

FUNCTION commonArity(CONST x,y:T_arityInfo):T_arityInfo;
  begin
    if (x.maxPatternLength<0) then result:=y else
    if (y.maxPatternLength<0) then result:=x else begin
      result.minPatternLength:=min(x.minPatternLength,y.minPatternLength);
      result.maxPatternLength:=max(x.maxPatternLength,y.maxPatternLength);
    end;
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

FUNCTION myFloatToStr(CONST x: T_myFloat): string;
  FUNCTION exponentRepresentation:shortstring;
    VAR ePos:longint;
        exponentPart:shortstring;
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

  FUNCTION simpleRepresentation:shortstring;
    begin
      str(x:50:50,result);
      result:=trim(result);
      while (result[ord(result[0])]<>'.')
        and ((strToFloatDef(copy(result,1,ord(result[0])-1),Nan)=x)) do dec(result[0]);
    end;

  VAR altRes:shortstring;
  begin
    DefaultFormatSettings.DecimalSeparator:='.';
    //Special representation for special values:
    if isNan(x) then exit(LITERAL_NAN_TEXT);
    if isInfinite(x) then begin
      if x>0 then exit(    LITERAL_INF_TEXT)
             else exit('-'+LITERAL_INF_TEXT);
    end;
    //Default representation (preferred if accurate)
    result:=floatToStr(x);
    if strToFloatDef(result,Nan)=x then begin
      if (pos('.', result)<=0) and (pos('E', result)<=0) and (pos('e', result)<=0) then result:=result+'.0';
      exit(result);
    end;
    //alternative representations
    if (abs(x)>1E15) then exit(exponentRepresentation);
    if (abs(x)<1E9) and (abs(x)>1E-9) then exit(simpleRepresentation);
    result:=simpleRepresentation;
    altRes:=exponentRepresentation;
    if length(altRes)<length(result) then exit(altRes);
  end;

FUNCTION parseNumber(CONST input: ansistring; CONST offset:longint; CONST suppressOutput: boolean; CONST literalRecycler:P_literalRecycler; OUT parsedLength: longint): P_literal;
  VAR i: longint;
      allZeroes:boolean=true;
      atLeastOneDigit:boolean=false;
      big:T_bigInt;
      intResult:int64;
  begin
    DefaultFormatSettings.DecimalSeparator:='.';
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
        result:=literalRecycler^.newRealLiteral(strToFloatDef(copy(input, offset, parsedLength), Nan));
      end else begin
        if suppressOutput then exit(nil);
        intResult:=StrToInt64Def(copy(input, offset, parsedLength), 0);
        if (intResult=0) and not(allZeroes) then begin
          big.fromString(copy(input, offset, parsedLength));
          result:=literalRecycler^.newBigIntLiteral(big);
        end else result:=literalRecycler^.newIntLiteral(intResult);
      end;
    end;
  end;

PROCEDURE T_literalRecycler.freeMemory(CONST hard:boolean);
  VAR threshold:longint;
  begin
    with smallIntLiterals do begin if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); end;
    with bigIntLiterals   do begin if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); end;
    with realLiterals     do begin if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); end;
    with stringLiterals   do begin if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); end;
    with listLiterals     do begin if hard then threshold:=0 else threshold:=fill div 2; while fill>threshold do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill<=threshold); setLength(dat,threshold); end;
  end;

CONSTRUCTOR T_literalRecycler.create;
  begin
    with smallIntLiterals do begin fill:=0; setLength(dat,0); end;
    with bigIntLiterals   do begin fill:=0; setLength(dat,0); end;
    with realLiterals     do begin fill:=0; setLength(dat,0); end;
    with stringLiterals   do begin fill:=0; setLength(dat,0); end;
    with listLiterals     do begin fill:=0; setLength(dat,0); end;
  end;

DESTRUCTOR T_literalRecycler.destroy;
  begin
    with smallIntLiterals do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); end;
    with bigIntLiterals   do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); end;
    with realLiterals     do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); end;
    with stringLiterals   do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); end;
    with listLiterals     do begin while fill>0 do begin dec(fill); try dispose(dat[fill],destroy); except dat[fill]:=nil; end; end; assert(fill=0); setLength(dat,0); end;
  end;

PROCEDURE T_literalRecycler.disposeLiteral(VAR l: P_literal);
  begin
    if l^.unreference<=0 then
    case l^.literalType of
      lt_smallint:
        with smallIntLiterals do begin
          if (fill>=length(dat)) then begin
            if length(dat)=0
            then setLength(dat,32)
            else setLength(dat,length(dat)*2);
          end;
          dat[fill]:=P_smallIntLiteral(l);
          inc(fill);
        end;
      lt_bigint:
        with bigIntLiterals do begin
          l^.cleanup(@self);
          if (fill>=length(dat)) then begin
            if length(dat)=0
            then setLength(dat,32)
            else setLength(dat,length(dat)*2);
          end;
          dat[fill]:=P_bigIntLiteral(l);
          inc(fill);
        end;
      lt_real:
        with realLiterals do begin
          if (fill>=length(dat)) then begin
            if length(dat)=0
            then setLength(dat,32)
            else setLength(dat,length(dat)*2);
          end;
          dat[fill]:=P_realLiteral(l);
          inc(fill);
        end;
      lt_string:
        with stringLiterals do begin
          l^.cleanup(@self);
          if (fill>=length(dat)) then begin
            if length(dat)=0
            then setLength(dat,32)
            else setLength(dat,length(dat)*2);
          end;
          dat[fill]:=P_stringLiteral(l);
          inc(fill);
        end;
      lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_emptyList:
        with listLiterals do begin
          l^.cleanup(@self);
          if (fill>=length(dat)) then begin
            if length(dat)=0
            then setLength(dat,32)
            else setLength(dat,length(dat)*2);
          end;
          dat[fill]:=P_listLiteral(l);
          inc(fill);
        end;
      else begin l^.cleanup(@self); dispose(l,destroy); end;
    end;
    l:=nil;
  end;

PROCEDURE T_literalRecycler.disposeLiteral(VAR l: T_arrayOfLiteral);
  VAR i:longint;
  begin
    for i:=0 to length(l)-1 do disposeLiteral(l[i]);
    setLength(l,0);
  end;

PROCEDURE T_literalRecycler.disposeLiteral(VAR l: T_arrayOfKeyValuePair);
  VAR pair:T_keyValuePair;
      tmp:P_literal;
  begin
    for pair in l do with pair do begin
      tmp:=key;   disposeLiteral(tmp);
      tmp:=value; disposeLiteral(tmp);
    end;
    setLength(l,0);
  end;

FUNCTION T_literalRecycler.newBoolLiteral(CONST value: boolean): P_boolLiteral;
  begin
    result:=P_boolLiteral(boolLit[value].rereferenced);
  end;

FUNCTION T_literalRecycler.newBigIntLiteral(value: T_bigInt): P_abstractIntLiteral;
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

FUNCTION T_literalRecycler.newIntLiteral(CONST value: int64): P_abstractIntLiteral;
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

FUNCTION T_literalRecycler.newIntLiteral(CONST value: T_bigInt): P_abstractIntLiteral;
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

FUNCTION T_literalRecycler.newStringLiteral(CONST value: ansistring; CONST enforceNewString: boolean): P_stringLiteral;
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

FUNCTION T_literalRecycler.newRealLiteral(CONST value: T_myFloat): P_realLiteral;
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

FUNCTION T_literalRecycler.newListLiteral(CONST initialSize: longint): P_listLiteral;
  begin
    with listLiterals do if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
      result^.numberOfReferences:=1;
      setLength(result^.dat,initialSize);
      result^.fill:=0;
    end else begin
      new(result,create(initialSize));
    end;
  end;

FUNCTION T_literalRecycler.newListLiteral(CONST a: P_literal; CONST b: P_literal): P_listLiteral;
  VAR initialSize:longint=2;
  begin
    if b=nil then dec(initialSize);
    with listLiterals do if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
      result^.numberOfReferences:=1;
      setLength(result^.dat,initialSize);
      result^.fill:=0;
    end else begin
      new(result,create(initialSize));
    end;
    result^.append(@self,a,true);
    if b<>nil then result^.append(@self,b,true);
  end;

FUNCTION newVoidLiteral                            : P_voidLiteral; begin result:=P_voidLiteral(voidLit       .rereferenced); end;
FUNCTION newBoolLiteral(CONST value: boolean)      : P_boolLiteral; begin result:=P_boolLiteral(boolLit[value].rereferenced); end;
FUNCTION newSetLiteral(CONST expectedSize: longint): P_setLiteral;  begin new(result,create(expectedSize)); end;
FUNCTION newMapLiteral(CONST expectedSize: longint): P_mapLiteral;  begin new(result,create(expectedSize)); end;

CONSTRUCTOR T_typedef.create(CONST id: T_idString;
  CONST builtinCheck: T_typeCheck; CONST builtinCheckPar: longint;
  CONST super_: P_typedef; CONST typerule: P_expressionLiteral;
  CONST ducktyping_, alwaysTrue_: boolean);
  begin
    name        :=id;
    super       :=super_;
    builtinsuper        :=builtinCheck;
    builtinsuperModifier:=builtinCheckPar;
    ducktyperule:=typerule;
    ducktyping  :=ducktyping_;
    alwaysTrue  :=alwaysTrue_ and (super=nil);
  end;

PROCEDURE T_typedef.cleanup(CONST literalRecycler:P_literalRecycler);
  begin
    literalRecycler^.disposeLiteral(ducktyperule);
  end;

DESTRUCTOR T_typedef.destroy;
  begin
    assert(ducktyperule=nil);
  end;

FUNCTION T_typedef.matchesLiteral(CONST L: P_literal;
  CONST location: T_tokenLocation; CONST threadContext: P_abstractContext;
  CONST recycler: pointer): boolean;
  VAR T:P_typedef;
  begin
    result:=false;
    if (L^.literalType in C_typables) then begin
      T:=P_typableLiteral(L)^.customType;
      while(t<>nil) do begin
        if T=@self
        then exit(true)
        else T:=T^.super;
      end;
    end;
    if ducktyping then result:=typeCheckAccept(L,builtinsuper,builtinsuperModifier) and (alwaysTrue or ducktyperule^.evaluateToBoolean(location,threadContext,recycler,false,L,nil));
  end;

FUNCTION T_typedef.cloneLiteral(CONST literalRecycler:P_literalRecycler; CONST L: P_typableLiteral; CONST location: T_tokenLocation; CONST threadContext: P_abstractContext; CONST recycler: pointer): P_typableLiteral;
  begin
    result:=nil;
    case L^.literalType of
      lt_expression: begin
        if P_expressionLiteral(l)^.typ in [et_builtin,et_builtinAsyncOrFuture,et_builtinIteratable] then begin
          result:=P_expressionLiteral(L)^.clone(location,threadContext,recycler);
        end else begin
          if L^.numberOfReferences<=1
          then result:=P_typableLiteral(L^.rereferenced)
          else result:=P_expressionLiteral(L)^.clone(location,threadContext,recycler);
        end;
      end;
      lt_list..lt_emptyMap: begin
        if L^.numberOfReferences<=1
        then result:=P_typableLiteral(L^.rereferenced)
        else result:=P_compoundLiteral(L)^.clone(literalRecycler);
      end;
    end;
  end;

FUNCTION T_typedef.cast(CONST literalRecycler:P_literalRecycler;CONST L: P_literal; CONST location: T_tokenLocation; CONST threadContext: P_abstractContext; CONST recycler: pointer): P_typableLiteral;
  begin
    result:=nil;
    if not(L^.literalType in C_typables) then begin
      threadContext^.raiseError('Cannot cast primitive scalar',location);
      exit(nil);
    end;
    if P_typableLiteral(L)^.customType=@self then exit(P_typableLiteral(L^.rereferenced));
    if alwaysTrue or ducktyperule^.evaluateToBoolean(location,threadContext,recycler,false,L,nil) then begin
      result:=cloneLiteral(literalRecycler,P_typableLiteral(L),location,threadContext,recycler);
      if result<>nil then result^.customType:=@self;
    end else if (super<>nil) then begin
      result:=super^.cast(literalRecycler,L,location,threadContext,recycler);
      if (result<>nil) then begin
        if alwaysTrue or ducktyperule^.evaluateToBoolean(location,threadContext,recycler,false,result,nil)
        then result^.customType:=@self
        else literalRecycler^.disposeLiteral(result);
      end;
    end;
    if (result=nil) then threadContext^.raiseError('Cannot cast literal to custom type '+name,location);
  end;

FUNCTION T_typedef.uncast(CONST literalRecycler:P_literalRecycler; CONST L: P_literal; CONST location: T_tokenLocation; CONST threadContext: P_abstractContext; CONST adapters: P_messages; CONST recycler: pointer): P_literal;
  begin
    if not(L^.literalType in C_typables) or (P_typableLiteral(L)^.customType=nil) then exit(L^.rereferenced);
    result:=cloneLiteral(literalRecycler,P_typableLiteral(L),location,threadContext,recycler);
    if result<>nil then P_typableLiteral(result)^.customType:=nil;
  end;

FUNCTION T_typedef.getId: T_idString;
  begin
    result:=name;
  end;

FUNCTION T_typedef.getLocation: T_tokenLocation;
  begin
    result:=ducktyperule^.getLocation;
  end;

FUNCTION T_typedef.getDocTxt:string;
  begin
    result:=ECHO_MARKER+ducktyperule^.getId+';'+C_tabChar+COMMENT_PREFIX+'declared '+ansistring(getLocation);
  end;

//=====================================================================================================================

CONSTRUCTOR G_literalKeyMap.create(CONST expectedSize:longint=0);
  VAR k:longint;
  begin
    k:=4;
    while expectedSize>=k*HASH_GROWTH_THRESHOLD_FACTOR do inc(k,k);
    setLength(bin,k);
    for k:=0 to length(bin)-1 do begin
      bin[k].binFill:=0;
      setLength(bin[k].arr,2);
    end;
    fill:=0;
  end;

CONSTRUCTOR G_literalKeyMap.createClone(VAR map:MY_TYPE);
  VAR i,j:longint;
  begin
    fill:=map.fill;
    setLength(bin,length(map.bin));
    for i:=0 to length(bin)-1 do begin
      bin[i].binFill :=map.bin[i].binFill;
      setLength(bin[i].arr,bin[i].binFill);
      for j:=0 to bin[i].binFill-1 do bin[i].arr[j]:=
                                  map.bin[i].arr[j];
    end;
  end;

DESTRUCTOR G_literalKeyMap.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(bin)-1 do with bin[i] do setLength(arr,0);
    setLength(bin,0);
  end;

PROCEDURE G_literalKeyMap.rehash(CONST grow:boolean);
  VAR i,i0,j,c0,c1:longint;
      hashMask:dword;
  begin
    if grow then begin
      i0:=length(bin);
      setLength(bin,i0+i0);
      hashMask:=length(bin)-1;
      for i:=0 to i0-1 do begin
        setLength(bin[i+i0].arr,bin[i].binFill);
        c0:=0;
        c1:=0;
        for j:=0 to bin[i].binFill-1 do begin
          if (bin[i].arr[j].keyHash and hashMask=i)
          then begin bin[i   ].arr[c0]:=bin[i].arr[j]; inc(c0); end
          else begin bin[i+i0].arr[c1]:=bin[i].arr[j]; inc(c1); end;
        end;
        bin[i   ].binFill:=c0; setLength(bin[i   ].arr,max(2,bin[i   ].binFill));
        bin[i+i0].binFill:=c1; setLength(bin[i+i0].arr,max(2,bin[i+i0].binFill));
      end;
    end else if length(bin)>1 then begin
      i0:=length(bin) shr 1;
      for i:=0 to i0-1 do begin
        setLength(bin[i].arr,bin[i].binFill+bin[i+i0].binFill);
        for j:=0 to bin[i+i0].binFill-1 do begin
          bin[i].arr[bin[i].binFill]:=bin[i+i0].arr[j];
          inc(bin[i].binFill);
        end;
        setLength(bin[i+i0].arr,0);
      end;
      setLength(bin,i0);
    end;
  end;

PROCEDURE G_literalKeyMap.put(CONST key:P_literal; CONST value:VALUE_TYPE);
  VAR hash:T_hashInt;
      j:longint=0;
  begin
    hash:=key^.hash;
    with bin[hash and (length(bin)-1)] do begin
      while (j<binFill) and not((arr[j].keyHash=hash) and (arr[j].key^.equals(key))) do inc(j);
      if j>=binFill then begin
        if j>=length(arr) then setLength(arr,(length(arr)*5+4) shr 2);
        arr[j].key    :=key;
        arr[j].keyHash:=hash;
        inc(binFill);
        inc(fill);
      end;
      arr[j].value:=value;
    end;
    if fill>length(bin)*HASH_GROWTH_THRESHOLD_FACTOR then rehash(true);
  end;

FUNCTION G_literalKeyMap.putNew(CONST newEntry:CACHE_ENTRY; OUT previousValue:VALUE_TYPE):boolean;
  VAR j:longint=0;
  begin
    initialize(previousValue);
    with bin[newEntry.keyHash and (length(bin)-1)] do begin
      while (j<binFill) and not((newEntry.keyHash=arr[j].keyHash) and arr[j].key^.equals(newEntry.key)) do inc(j);
      if j>=binFill then begin
        if j>=length(arr) then setLength(arr,(length(arr)*5+4) shr 2);
        arr[j].key    :=newEntry.key;
        arr[j].keyHash:=newEntry.keyHash;
        arr[j].value  :=newEntry.value;
        inc(binFill);
        inc(fill);
        result:=true;
      end else begin
        previousValue:=arr[j].value;
        arr[j].value:=newEntry.value;
        result:=false;
      end;
    end;
    if fill>length(bin)*HASH_GROWTH_THRESHOLD_FACTOR then rehash(true);
  end;

FUNCTION G_literalKeyMap.putNew(CONST key:P_literal; CONST value:VALUE_TYPE; OUT previousValue:VALUE_TYPE):boolean;
  VAR hash:T_hashInt;
      j:longint=0;
  begin
    initialize(previousValue);
    hash:=key^.hash;
    with bin[hash and (length(bin)-1)] do begin
      while (j<binFill) and not((hash=arr[j].keyHash) and arr[j].key^.equals(key)) do inc(j);
      if j>=binFill then begin
        if j>=length(arr) then setLength(arr,(length(arr)*5+4) shr 2);
        arr[j].key    :=key;
        arr[j].keyHash:=hash;
        arr[j].value  :=value;
        inc(binFill);
        inc(fill);
        result:=true;
      end else begin
        previousValue:=arr[j].value;
        arr[j].value:=value;
        result:=false;
      end;
    end;
    if fill>length(bin)*HASH_GROWTH_THRESHOLD_FACTOR then rehash(true);
  end;

FUNCTION G_literalKeyMap.get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
  VAR hash:T_hashInt;
      j:longint;
  begin
    hash:=key^.hash;
    result:=fallbackIfNotFound;
    with bin[hash and (length(bin)-1)] do
    for j:=0 to binFill-1 do if (arr[j].keyHash=hash) and (arr[j].key^.equals(key)) then exit(arr[j].value);
  end;

FUNCTION G_literalKeyMap.getEntry(CONST key:P_literal):P_CACHE_ENTRY;
  VAR hash:T_hashInt;
      j:longint;
  begin
    hash:=key^.hash;
    with bin[hash and (length(bin)-1)] do
    for j:=0 to binFill-1 do if (arr[j].keyHash=hash) and (arr[j].key^.equals(key)) then exit(@arr[j]);
    result:=nil;
  end;

FUNCTION G_literalKeyMap.containsKey(CONST entry:CACHE_ENTRY):boolean;
  VAR j:longint;
  begin
    with bin[entry.keyHash and (length(bin)-1)] do
    for j:=0 to binFill-1 do if (arr[j].keyHash=entry.keyHash) and (arr[j].key^.equals(entry.key)) then exit(true);
    result:=false;
  end;

FUNCTION G_literalKeyMap.drop(CONST key:P_literal):CACHE_ENTRY;
  VAR hash:T_hashInt;
      j,i:longint;
  begin
    result.key:=nil;
    hash:=key^.hash;
    with bin[hash and (length(bin)-1)] do
    for j:=0 to binFill-1 do if (arr[j].keyHash=hash) and arr[j].key^.equals(key) then begin
      result:=arr[j];
      i:=binFill-1;
      if (j<i) then arr[j]:=arr[i];
      dec(binFill);
      dec(fill);
      if fill<length(bin)*HASH_SHRINK_THRESHOLD_FACTOR then rehash(false);
      exit(result);
    end;
  end;

FUNCTION G_literalKeyMap.keyValueList:KEY_VALUE_LIST;
  VAR i,j:longint;
      k  :longint=0;
  begin
    setLength(result,fill);
    for i:=0 to length(bin)-1 do
    with bin[i] do
    for j:=0 to binFill-1 do begin
      result[k]:=arr[j];
      inc(k);
    end;
  end;

FUNCTION G_literalKeyMap.keySet:T_arrayOfLiteral;
  VAR i,j,k:longint;
  begin
    setLength(result,fill);
    k:=0;
    for i:=0 to length(bin)-1 do with bin[i] do for j:=0 to binFill-1 do begin
      {$ifdef debugMode}
      try
      {$endif}
        result[k]:=arr[j].key;
      {$ifdef debugMode}
      except
        raise Exception.create('Trying to set result ['+intToStr(k)+'] in list of length '+intToStr(length(result)));
      end;
      {$endif}
      inc(k);
    end;
  end;

{$ifdef debugMode}
PROCEDURE G_literalKeyMap.writeDump;
  VAR i,j:longint;
      f:longint=0;
  begin
    for i:=0 to length(bin)-1 do begin
      if i=0 then write('[[') else write(' [');
      with bin[i] do begin
        f+=binFill;
        for j:=0 to binFill-1 do begin
          if j>0 then write(', ');
          write(arr[j].key^.toString())
        end;
      end;
      if i=length(bin)-1 then writeln(']]') else writeln('],');
    end;
    if f<>fill then writeln('FILL MISMATCH; fill=',fill,'; sum of binFill=',f);
  end;
{$endif}

PROCEDURE T_literal.rereference;
  begin
    interLockedIncrement(numberOfReferences);
  end;

FUNCTION T_literal.rereferenced: P_literal;
  begin
    interLockedIncrement(numberOfReferences);
    result:=@self;
  end;

FUNCTION T_literal.unreference: longint;
  begin
    result:=interlockedDecrement(numberOfReferences);
  end;

FUNCTION T_literal.getId: T_idString;            begin result:=''; end;
FUNCTION T_literal.getLocation: T_tokenLocation; begin result.package:=nil; result.column:=-1; result.line:=-1; end;

PROCEDURE T_literal.cleanup(CONST literalRecycler: P_literalRecycler);
  begin end;

FUNCTION T_expressionLiteral.getLocation:T_tokenLocation; begin result:=declaredAt; end;
//CONSTRUCTORS:=================================================================
{$MACRO ON}
{$define inline_init:=numberOfReferences:=1; literalType:=}
CONSTRUCTOR T_literal.init(CONST lt: T_literalType); begin literalType:=lt; numberOfReferences:=1; end;
CONSTRUCTOR T_voidLiteral.create();                              begin {inherited init}inline_init(lt_void);                end;
CONSTRUCTOR T_boolLiteral      .create(CONST value: boolean);    begin {inherited init}inline_init(lt_boolean); val:=value; end;
CONSTRUCTOR T_bigIntLiteral    .create(CONST value: int64);      begin {inherited init}inline_init(lt_bigint);  val.fromInt(value); end;
CONSTRUCTOR T_smallIntLiteral  .create(CONST value: longint);    begin {inherited init}inline_init(lt_smallint);val:=value; end;
CONSTRUCTOR T_bigIntLiteral    .create(CONST value: T_bigInt);   begin {inherited init}inline_init(lt_bigint);  val:=value; end;
CONSTRUCTOR T_realLiteral      .create(CONST value: T_myFloat);  begin {inherited init}inline_init(lt_real);    val:=value; end;
CONSTRUCTOR T_stringLiteral    .create(CONST value: ansistring); begin {inherited init}inline_init(lt_string);  val:=value; enc:=se_testPending; end;
{$define inline_init:=numberOfReferences:=1; customType:=nil;literalType:=}
CONSTRUCTOR T_expressionLiteral.create(CONST eType: T_expressionType; CONST location:T_tokenLocation);
  begin
    inline_init(lt_expression);
    myHash:=0;
    expressionType:=eType;
    declaredAt:=location;
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
    setLength(dat,initialSize);
    fill:=0;
  end;

CONSTRUCTOR T_setLiteral.create(CONST expectedSize:longint);
  begin
    inline_init(lt_emptySet);
    myHash  :=0;
    ints    :=0;
    reals   :=0;
    strings :=0;
    booleans:=0;
    others  :=0;
    dat.create(expectedSize);
  end;

CONSTRUCTOR T_setLiteral.createClone(VAR original:T_setLiteral);
  VAR key:P_literal;
  begin
    inline_init(original.literalType);
    myHash  :=original.myHash  ;
    ints    :=original.ints    ;
    reals   :=original.reals   ;
    strings :=original.strings ;
    booleans:=original.booleans;
    others  :=original.others  ;
    dat.createClone(original.dat);
    for key in dat.keySet do key^.rereference;
  end;

CONSTRUCTOR T_mapLiteral.create(CONST expectedSize:longint);
  begin
    inline_init(lt_emptyMap);
    myHash:=0;
    dat.create(expectedSize);
  end;

CONSTRUCTOR T_mapLiteral.createClone(VAR original:T_mapLiteral);
  VAR entry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    inline_init(original.literalType);
    myHash:=original.myHash;
    dat.createClone(original.dat);
    for entry in dat.keyValueList do begin
      entry.key  ^.rereference;
      entry.value^.rereference;
    end;
  end;

//=================================================================:CONSTRUCTORS
//DESTRUCTORS:==================================================================
DESTRUCTOR T_literal.destroy; begin end;
DESTRUCTOR T_bigIntLiteral.destroy; begin end;
DESTRUCTOR T_stringLiteral.destroy; begin end;
DESTRUCTOR T_listLiteral.destroy;
  begin
    assert(length(dat)=0);
  end;

PROCEDURE T_listLiteral.cleanup(CONST literalRecycler: P_literalRecycler);
  VAR i:longint;
  begin
    for i:=0 to fill-1 do literalRecycler^.disposeLiteral(dat[i]);
    setLength(dat,0);
    fill    :=0;
    myHash  :=0;
    ints    :=0;
    reals   :=0;
    strings :=0;
    booleans:=0;
    others  :=0;
    customType:=nil;
    literalType:=lt_emptyList;
  end;

DESTRUCTOR T_setLiteral.destroy;
  begin
    assert(length(dat.bin)=0);
  end;

PROCEDURE T_setLiteral.cleanup(CONST literalRecycler:P_literalRecycler);
  VAR entries:T_arrayOfLiteral;
      i:longint;
  begin
    entries:=dat.keySet;
    for i:=0 to length(entries)-1 do literalRecycler^.disposeLiteral(entries[i]);
    dat.destroy;
    myHash  :=0;
    ints    :=0;
    reals   :=0;
    strings :=0;
    booleans:=0;
    others  :=0;
    customType:=nil;
    literalType:=lt_emptySet;
  end;

DESTRUCTOR T_mapLiteral.destroy;
  begin
    assert(length(dat.bin)=0);
  end;

PROCEDURE T_mapLiteral.cleanup(CONST literalRecycler:P_literalRecycler);
  VAR entries:T_literalKeyLiteralValueMap.KEY_VALUE_LIST;
      i:longint;
  begin
    entries:=dat.keyValueList;
    for i:=0 to length(entries)-1 do begin
      literalRecycler^.disposeLiteral(entries[i].key);
      literalRecycler^.disposeLiteral(entries[i].value);
    end;
    dat.destroy;
    customType:=nil;
    literalType:=lt_emptyMap;
    myHash:=0;
  end;

//==================================================================:DESTRUCTORS
FUNCTION T_collectionLiteral.appendString(CONST literalRecycler:P_literalRecycler; CONST s: ansistring): P_collectionLiteral; begin result:=P_collectionLiteral(append(literalRecycler,literalRecycler^.newStringLiteral(s),false)); end;
FUNCTION T_collectionLiteral.appendBool  (CONST literalRecycler:P_literalRecycler; CONST b: boolean   ): P_collectionLiteral; begin result:=P_collectionLiteral(append(literalRecycler,boolLit[b].rereferenced,false)); end;
FUNCTION T_collectionLiteral.appendInt   (CONST literalRecycler:P_literalRecycler; CONST i: int64     ): P_collectionLiteral; begin result:=P_collectionLiteral(append(literalRecycler,literalRecycler^.newIntLiteral   (i),false)); end;
FUNCTION T_collectionLiteral.appendReal  (CONST literalRecycler:P_literalRecycler; CONST r: T_myFloat ): P_collectionLiteral; begin result:=P_collectionLiteral(append(literalRecycler,literalRecycler^.newRealLiteral  (r),false)); end;
FUNCTION T_collectionLiteral.appendAll   (CONST literalRecycler:P_literalRecycler; CONST L: P_compoundLiteral): P_collectionLiteral;
  VAR x:P_literal;
  begin
    for x in L^.forcedIteratableList(literalRecycler) do append(literalRecycler,x,false);
    result:=@self
  end;

FUNCTION T_setLiteral.appendAll(CONST literalRecycler:P_literalRecycler; CONST L:P_compoundLiteral):P_collectionLiteral;
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
    end else for x in L^.forcedIteratableList(literalRecycler) do append(literalRecycler,x,false);
    result:=@self;
  end;

//?.size:=======================================================================
FUNCTION T_listLiteral.size: longint; begin result:=fill;     end;
FUNCTION T_mapLiteral.size: longint; begin result:=dat.fill; end;
FUNCTION T_setLiteral.size: longint; begin result:=dat.fill; end;
//=======================================================================:?.size
FUNCTION T_listLiteral.head(CONST literalRecycler:P_literalRecycler): P_literal;
  begin
    if fill=0
    then result:=@self
    else result:=dat[0];
    result^.rereference;
  end;

FUNCTION T_listLiteral.head(CONST literalRecycler:P_literalRecycler; CONST headSize: longint): P_listLiteral;
  VAR i,imax:longint;
  begin
    imax:=headSize;
    if imax>fill then imax:=fill;
    if imax<0 then imax:=0;
    result:=literalRecycler^.newListLiteral(imax);
    for i:=0 to imax-1 do result^.append(literalRecycler,dat[i],true);
  end;

FUNCTION T_listLiteral.tail(CONST literalRecycler:P_literalRecycler): P_listLiteral;
  begin result:=tail(literalRecycler,1); end;

FUNCTION T_listLiteral.tail(CONST literalRecycler:P_literalRecycler; CONST headSize: longint): P_listLiteral;
  VAR i,iMin:longint;
  begin
    iMin:=headSize;
    if iMin>fill then iMin:=fill
    else if iMin<0 then iMin:=0;
    result:=literalRecycler^.newListLiteral(fill-iMin);
    for i:=iMin to fill-1 do result^.append(literalRecycler,dat[i],true);
  end;

FUNCTION T_listLiteral.trailing(CONST literalRecycler:P_literalRecycler): P_literal;
  begin
    if fill=0
    then result:=@self
    else result:=dat[fill-1];
    result^.rereference;
  end;

FUNCTION T_listLiteral.trailing(CONST literalRecycler:P_literalRecycler; CONST trailSize: longint): P_listLiteral;
  begin result:=tail(literalRecycler,fill-trailSize); end;

FUNCTION T_listLiteral.leading(CONST literalRecycler:P_literalRecycler): P_listLiteral;
  begin result:=head(literalRecycler,fill-1); end;

FUNCTION T_listLiteral.leading(CONST literalRecycler:P_literalRecycler; CONST trailSize: longint): P_listLiteral;
  begin result:=head(literalRecycler,fill-trailSize); end;

FUNCTION T_listLiteral.transpose(CONST literalRecycler:P_literalRecycler; CONST filler: P_literal): P_listLiteral;
  VAR innerSize:longint=-1;
      i,j:longint;
      innerList:P_listLiteral;
      doneRow:boolean;
      containsError:boolean=false;
  begin
    if literalType=lt_emptyList then exit(P_listLiteral(rereferenced));
    for i:=0 to fill-1 do
    if (dat[i]^.literalType in C_listTypes)
    then innerSize:=max(innerSize,P_listLiteral(dat[i])^.size)
    else innerSize:=max(innerSize,1);

    result:=literalRecycler^.newListLiteral;
    for i:=0 to innerSize-1 do if not(containsError) then begin
      if filler=nil then begin
        innerList:=literalRecycler^.newListLiteral;
        doneRow:=false;
        for j:=0 to fill-1 do if not(containsError) then begin
          if (dat[j]^.literalType in C_listTypes) and (P_listLiteral(dat[j])^.fill>i) then
          begin innerList^.append(literalRecycler,P_listLiteral(dat[j])^.dat[i],true); containsError:=doneRow or containsError; end
          else if (dat[j]^.literalType in C_scalarTypes) and (i=0) then
          begin innerList^.append(literalRecycler,              dat[j]         ,true); containsError:=doneRow or containsError; end
          else doneRow:=true;
        end;
      end else begin
        innerList:=literalRecycler^.newListLiteral(fill);
        for j:=0 to fill-1 do if not(containsError) then begin
          if (dat[j]^.literalType in C_listTypes) and (P_listLiteral(dat[j])^.fill>i)
          then                                                          innerList^.append(literalRecycler,P_listLiteral(dat[j])^.dat[i],true)
          else if (dat[j]^.literalType in C_scalarTypes) and (i=0) then innerList^.append(literalRecycler,              dat[j]         ,true)
          else                                                          innerList^.append(literalRecycler,filler                       ,true);
        end;
      end;
      result^.append(literalRecycler,innerList,false);
    end;
    if containsError then literalRecycler^.disposeLiteral(result);
  end;

FUNCTION T_mapLiteral.transpose(CONST literalRecycler:P_literalRecycler): P_listLiteral;
  VAR keyList,valueList:P_listLiteral;
      entry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    keyList  :=literalRecycler^.newListLiteral(dat.fill);
    valueList:=literalRecycler^.newListLiteral(dat.fill);
    for entry in dat.keyValueList do begin
      keyList  ^.append(literalRecycler,entry.key  ,true);
      valueList^.append(literalRecycler,entry.value,true);
    end;
    result:=P_listLiteral(literalRecycler^.newListLiteral(2)^
      .append(literalRecycler,keyList,false)^
      .append(literalRecycler,valueList,false));
  end;

PROCEDURE T_listLiteral.removeElement(CONST literalRecycler:P_literalRecycler; CONST index:longint);
  VAR i:longint;
  begin
    if (index<0) or (index>=fill) then exit;
    myHash:=0;
    case dat[index]^.literalType of
      lt_boolean: dec(booleans);
      lt_smallint,
      lt_bigint : dec(ints);
      lt_real   : dec(reals);
      lt_string : dec(strings);
      else        dec(others);
    end;
    if fill=0 then literalType:=lt_emptyList
    else if others>0 then literalType:=lt_list
    else literalType:=C_listType[booleans>0,ints>0,reals>0,strings>0];
    literalRecycler^.disposeLiteral(dat[index]);
    for i:=index to fill-2 do dat[i]:=dat[i+1];
    dec(fill);
  end;

//?.toString:===================================================================
FUNCTION T_literal          .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:='<ERR>';           end;
FUNCTION T_voidLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=LITERAL_TEXT_VOID;        end;
FUNCTION T_boolLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=LITERAL_BOOL_TEXT[val]; end;
FUNCTION T_bigIntLiteral    .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=val.toString;      end;
FUNCTION T_smallIntLiteral  .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=intToStr(val);     end;
FUNCTION T_realLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=myFloatToStr(val); end;
FUNCTION T_stringLiteral    .toString(CONST lengthLimit:longint=maxLongint): ansistring;
  VAR dummy:boolean;
  begin
    if lengthLimit>=length(val)+2 then result:=escapeString(val                                ,es_pickShortest,getEncoding,dummy)
                                  else result:=escapeString(UTF8Copy(val,1,lengthLimit-5)+'...',es_pickShortest,getEncoding,dummy);
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
      iter:=tempIteratableList;;
      remainingLength:=lengthLimit-1;
      result:='['+iter[0]^.toString(remainingLength);
      for i:=1 to size-1 do if remainingLength>0 then begin
        remainingLength:=lengthLimit-length(result);
        result:=result+','+iter[i]^.toString(remainingLength);
      end else begin
        result:=result+',... ';
        break;
      end;
      result:=result+']';
    end;
    result:=result+'.toSet';
    if customType<>nil then result+='.to'+customType^.name;
  end;

FUNCTION T_mapLiteral.toString(CONST lengthLimit: longint): ansistring;
  VAR i,remainingLength: longint;
      iter:T_arrayOfKeyValuePair;
  begin
    if size = 0 then result:='[]'
    else begin
      iter:=entryList;
      remainingLength:=lengthLimit-1;
      result:='['+iter[0].key^.toString(remainingLength)+' => '+iter[0].value^.toString(remainingLength);
      for i:=1 to size-1 do if remainingLength>0 then begin
        remainingLength:=lengthLimit-length(result);
        result:=result+','+iter[i].key^.toString(remainingLength)+'=>'+iter[i].value^.toString(remainingLength);
      end else begin
        result:=result+',... ';
        break;
      end;
      for i:=0 to length(iter)-1 do begin
        iter[i].key^.unreference;
        iter[i].value^.unreference;
      end;
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

FUNCTION T_smallIntLiteral.toHexString:string;
  VAR digits:longint=0;
      v:longint=0;
  begin
    v:=val;
    while v>0 do begin
      inc(digits);
      v:=v shr 4;
    end;
    result:=IntToHex(val,digits);
  end;

FUNCTION T_bigIntLiteral.toHexString:string;
  begin
    result:=val.toHexString;
  end;

PROCEDURE T_bigIntLiteral.cleanup(CONST literalRecycler: P_literalRecycler);
  begin val.clear; end;

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
        result:=result+','+dat[i]^.toString(remainingLength);
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
      tt_operatorIn      : exit(    (other^.literalType in C_typeInfo[lt_boolean].containedIn) and   (P_compoundLiteral(other)^.contains(@self)) );
      tt_operatorNotIn   : exit(not((other^.literalType in C_typeInfo[lt_boolean].containedIn) and   (P_compoundLiteral(other)^.contains(@self))));
      tt_comparatorListEq: exit(equals(other));
    end;
    if other^.literalType<>lt_boolean then exit(false);
    ovl:=P_boolLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq, tt_comparatorLeq, tt_comparatorGeq])
         or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_bigIntLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR cr:T_comparisonResult;
  begin
    case relation of
      tt_operatorIn      : exit(    (other^.literalType in C_typeInfo[lt_bigint].containedIn) and (P_compoundLiteral(other)^.contains(@self)) );
      tt_operatorNotIn   : exit(not((other^.literalType in C_typeInfo[lt_bigint].containedIn) and (P_compoundLiteral(other)^.contains(@self))));
      tt_comparatorListEq: exit(equals(other));
    end;
    case other^.literalType of
      lt_smallint: begin
        cr:=val.compare(P_smallIntLiteral(other)^.val);
        result:=(cr=CR_EQUAL  ) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
             or (cr=CR_LESSER ) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (cr=CR_GREATER) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_bigint: begin
        cr:=val.compare(P_bigIntLiteral(other)^.val);
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

FUNCTION T_smallIntLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR cr:T_comparisonResult;
      i:longint;
      f:T_myFloat;
  begin
    case relation of
      tt_operatorIn      : exit(    (other^.literalType in C_typeInfo[lt_smallint].containedIn) and (P_compoundLiteral(other)^.contains(@self)) );
      tt_operatorNotIn   : exit(not((other^.literalType in C_typeInfo[lt_smallint].containedIn) and (P_compoundLiteral(other)^.contains(@self))));
      tt_comparatorListEq: exit(equals(other));
    end;
    case other^.literalType of
      lt_smallint: begin
        i:=P_smallIntLiteral(other)^.val;
        result:=(val=i) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
             or (val<i) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (val>i) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_bigint: begin
        cr:=C_FLIPPED[P_bigIntLiteral(other)^.val.compare(val)];
        result:=(cr=CR_EQUAL  ) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
             or (cr=CR_LESSER ) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (cr=CR_GREATER) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        f:=P_realLiteral(other)^.val;
        result:=(val=f) and (relation in [tt_comparatorEq, tt_comparatorLeq, tt_comparatorGeq])
             or (val<f) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (val>f) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result:=false;
    end;
  end;

FUNCTION T_realLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR cr:T_comparisonResult;
      ovr: T_myFloat;
      i:longint;
  begin
    case relation of
      tt_operatorIn      : exit(    (other^.literalType in C_typeInfo[lt_real].containedIn) and (P_compoundLiteral(other)^.contains(@self)) );
      tt_operatorNotIn   : exit(not((other^.literalType in C_typeInfo[lt_real].containedIn) and (P_compoundLiteral(other)^.contains(@self))));
      tt_comparatorListEq: exit(equals(other));
    end;
    case other^.literalType of
      lt_smallint: begin
        i:=P_smallIntLiteral(other)^.val;
        result:=(val=i) and (relation in [tt_comparatorEq,  tt_comparatorLeq, tt_comparatorGeq])
             or (val<i) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
             or (val>i) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_bigint: begin
        cr:=C_FLIPPED[P_bigIntLiteral(other)^.val.compare(val)];
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
      tt_operatorIn      : exit(    (other^.literalType in C_typeInfo[lt_string].containedIn) and (P_compoundLiteral(other)^.contains(@self)) );
      tt_operatorNotIn   : exit(not((other^.literalType in C_typeInfo[lt_string].containedIn) and (P_compoundLiteral(other)^.contains(@self))));
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

FUNCTION T_expressionLiteral.mustBeDroppedBeforePop:boolean;
  begin
    result:=false;
  end;

FUNCTION T_compoundLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  begin
    if not(other^.literalType in C_compoundTypes) then exit(false);
    case relation of
      tt_operatorIn   : result:=    P_compoundLiteral(other)^.contains(@self) ;
      tt_operatorNotIn: result:=not(P_compoundLiteral(other)^.contains(@self));
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
FUNCTION T_listLiteral.newOfSameType(CONST literalRecycler:P_literalRecycler; CONST initSize: boolean): P_collectionLiteral;
  begin
    if initSize
    then result:=literalRecycler^.newListLiteral(fill)
    else result:=literalRecycler^.newListLiteral();
  end;

FUNCTION T_setLiteral.newOfSameType(CONST literalRecycler:P_literalRecycler; CONST initSize:boolean): P_collectionLiteral;
  begin
    if initSize
    then result:=newSetLiteral(dat.fill)
    else result:=newSetLiteral(0);
  end;

FUNCTION T_literal.typeString:string;
  begin
    result:=C_typeInfo[literalType].name;
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
    result:=result+'('+intToStr(arity.minPatternLength)+')';
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
FUNCTION T_boolLiteral.hash: T_hashInt; begin result:=longint(lt_boolean); if val then inc(result); end;
FUNCTION T_smallIntLiteral.hash: T_hashInt; begin {$Q-}{$R-} result:=T_hashInt((T_hashInt(val)*T_hashInt(31))) shr 3; {$R+}{$Q+} end;
FUNCTION T_bigIntLiteral  .hash: T_hashInt; begin result:=val.hash; end;
FUNCTION T_realLiteral.hash: T_hashInt;
  begin
    {$Q-}{$R-}
    result:=0;
    move(val, result, sizeOf(result));
    {$Q+}{$R+}
  end;

FUNCTION T_stringLiteral.hash: T_hashInt;
  VAR i: longint;
  begin
    {$Q-}{$R-}
    result:=T_hashInt(length(val));
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
    result:=T_hashInt(length(s))+T_hashInt(customType);
    for i:=1 to length(s) do result:=result*31+ord(s[i]);
    {$Q+}{$R+}
    if result=0 then result:=1;
    if not(expressionType in C_statefulExpressionTypes) then myHash:=result;
  end;

FUNCTION T_listLiteral.hash: T_hashInt;
  VAR i:longint;
  begin
    if myHash>0 then exit(myHash);
    {$Q-}{$R-}
    result:=T_hashInt(fill)+T_hashInt(customType);
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
    result:=T_hashInt(dat.fill)+T_hashInt(customType);
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
    result:=T_hashInt(dat.fill)+T_hashInt(customType);
    result:=result*31;
    for entry in dat.keyValueList do result:=result+entry.keyHash+entry.value^.hash*37;
    {$Q+}{$R+}
    if result=0 then result:=1;
    myHash:=result;
  end;

//=======================================================================:?.hash
PROCEDURE T_expressionLiteral.makeStateful  (CONST context:P_abstractContext; CONST location:T_tokenLocation);
  begin
    if expressionType in C_statefulExpressionTypes then exit;
    case expressionType of
      et_subrule: expressionType:=et_subruleStateful;
      et_inline : expressionType:=et_inlineStateful;
      else if context<>nil then context^.raiseError(C_expressionTypeString[expressionType]+' cannot be stateful',location);
    end;
  end;

PROCEDURE T_expressionLiteral.makeIteratable(CONST context:P_abstractContext;  CONST location:T_tokenLocation);
  begin
    if expressionType in C_iteratableExpressionTypes then exit;
    if not(canApplyToNumberOfParameters(0)) or not(expressionType in C_statefulExpressionTypes) then begin
      if context<>nil then context^.raiseError('Only nullary stateful expressions may be iteratable.',location);
      exit;
    end;
    case expressionType of
      et_subruleStateful: expressionType:=et_subruleIteratable;
      et_inlineStateful : expressionType:=et_inlineIteratable;
    else if context<>nil then context^.raiseError('Only nullary stateful expressions may be iteratable.',location);
    end;
  end;

//?.equals:=====================================================================
FUNCTION T_literal.equals(CONST other: P_literal): boolean;
  begin result:=(@self = other) or (other^.literalType = literalType) and (other^.toString=toString);  end;

FUNCTION T_bigIntLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other) or (other^.literalType = lt_bigint) and (P_bigIntLiteral(other)^.val.equals(val))
                            or (other^.literalType = lt_smallint) and (val.compare(P_smallIntLiteral(other)^.val)=CR_EQUAL);
  end;

FUNCTION T_smallIntLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other) or (other^.literalType = lt_smallint) and (P_smallIntLiteral(other)^.val=val)
                            or (other^.literalType = lt_bigint) and (P_bigIntLiteral(other)^.val.compare(val)=CR_EQUAL);
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

FUNCTION T_listLiteral.get(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal;
  VAR i,j:longint;
      iter:T_arrayOfLiteral;
      idx:P_literal;
  begin
    result:=nil;
    case accessor^.literalType of
      lt_bigint,lt_smallint: begin
        if            P_abstractIntLiteral(accessor)^.isBetween(0,fill-1)
        then exit(dat[P_abstractIntLiteral(accessor)^.intValue]^.rereferenced)
        else exit(newVoidLiteral);
      end;
      lt_intList, lt_emptyList: begin
        result:=literalRecycler^.newListLiteral(P_listLiteral(accessor)^.fill);
        for j:=0 to P_listLiteral(accessor)^.fill-1 do begin
          if                                                     P_abstractIntLiteral(P_listLiteral(accessor)^.dat[j])^.isBetween(0,fill-1)
          then P_listLiteral(result)^.append(literalRecycler,dat[P_abstractIntLiteral(P_listLiteral(accessor)^.dat[j])^.intValue],true);
        end;
        exit(result);
      end;
      lt_intSet, lt_emptySet: begin
        result:=newSetLiteral(P_setLiteral(accessor)^.size);
        iter:=P_setLiteral(accessor)^.tempIteratableList;
        for idx in iter do begin
          if                                                    P_abstractIntLiteral(idx)^.isBetween(0,fill-1)
          then P_setLiteral(result)^.append(literalRecycler,dat[P_abstractIntLiteral(idx)^.intValue],true);
        end;
        exit(result);
      end;
      lt_booleanList: if (P_listLiteral(accessor)^.fill=fill) then begin
        result:=literalRecycler^.newListLiteral(fill);
        for i:=0 to fill-1 do if P_boolLiteral(P_listLiteral(accessor)^.dat[i])^.val then P_listLiteral(result)^.append(literalRecycler,dat[i],true);
        exit(result);
      end;
    end;
    if isKeyValueCollection then begin
      for i:=0 to fill-1 do if accessor^.equals(P_listLiteral(dat[i])^.value[0])
                                      then exit(P_listLiteral(dat[i])^.value[1]^.rereferenced);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION T_setLiteral.get(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal;
  VAR iter:T_arrayOfLiteral;
      x:P_literal;
  begin
    result:=nil;
    if isKeyValueCollection then begin
      iter:=tempIteratableList;
      for x in iter do if (result=nil) and accessor^.equals(P_listLiteral(x)^.value[0])
                                               then result:=P_listLiteral(x)^.value[1]^.rereferenced;
      if result=nil then result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION T_mapLiteral.get(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal;
  begin
    result:=dat.get(accessor,nil);
    if result=nil then result:=newVoidLiteral
                  else result^.rereference;
  end;

FUNCTION T_listLiteral.getInner(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal;
  VAR i:longint;
  begin
    result:=literalRecycler^.newListLiteral(fill);
    if literalType=lt_list then
    for i:=0 to fill-1 do
    if dat[i]^.literalType in C_compoundTypes
    then P_listLiteral(result)^.append(literalRecycler,P_compoundLiteral(dat[i])^.get(literalRecycler,accessor),false)
    else begin
      literalRecycler^.disposeLiteral(result);
      exit(nil);
    end;
  end;

FUNCTION T_setLiteral.getInner(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal;
  VAR iter:T_arrayOfLiteral;
      sub:P_literal;
  begin
    iter:=tempIteratableList;
    result:=newSetLiteral(length(iter));
    for sub in iter do if sub^.literalType in C_compoundTypes
    then P_setLiteral(result)^.append(literalRecycler,P_compoundLiteral(sub)^.get(literalRecycler,accessor),false)
    else begin
      literalRecycler^.disposeLiteral(result);
      exit(nil);
    end;
  end;

FUNCTION T_mapLiteral.getInner(CONST literalRecycler:P_literalRecycler; CONST accessor:P_literal):P_literal;
  VAR validCase :boolean=false;
      wantKeys  :boolean=false;
      wantValues:boolean=false;
      subAsSet  :boolean=false;
      entry     :T_literalKeyLiteralValueMap.CACHE_ENTRY;
      sub       :P_collectionLiteral;
  begin
    case accessor^.literalType of
      lt_bigint: begin
        wantKeys  :=P_bigIntLiteral(accessor)^.val.isZero;
        wantValues:=P_bigIntLiteral(accessor)^.val.isOne;
        validCase :=true;

      end;
      lt_smallint: begin
        wantKeys  :=(P_smallIntLiteral(accessor)^.val=0);
        wantValues:=(P_smallIntLiteral(accessor)^.val=1);
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
          result:=literalRecycler^.newListLiteral(dat.fill);
          if subAsSet then sub:=newSetLiteral(2)
                      else sub:=literalRecycler^.newListLiteral(2);
          sub^.append(literalRecycler,entry.key  ,true);
          sub^.append(literalRecycler,entry.value,true);
          P_listLiteral(result)^.append(literalRecycler,sub,false);
        end else begin
          result:=newSetLiteral(dat.fill);
          for entry in dat.keyValueList do P_setLiteral(result)^.append(literalRecycler,entry.key,true);
        end;
      end else if wantValues then begin
        result:=literalRecycler^.newListLiteral(dat.fill);
        for entry in dat.keyValueList do
        P_listLiteral(result)^.append(literalRecycler,entry.value,true);
      end else result:=literalRecycler^.newListLiteral();
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

FUNCTION T_smallIntLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    case other^.literalType of
      lt_real    : result:=not(isNan(  P_realLiteral    (other)^.val)) and
                          (val<=P_realLiteral    (other)^.val);
      lt_smallint: result:=val<=P_smallIntLiteral(other)^.val;
      lt_bigint  : result:=P_bigIntLiteral(other)^.val.compare(val) in [CR_GREATER,CR_EQUAL];
    else result:=(literalType<=other^.literalType); end;
  end;

FUNCTION T_bigIntLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    case other^.literalType of
      lt_real    : result:=not(isNan(  P_realLiteral    (other)^.val)) and
                          (val.compare(P_realLiteral    (other)^.val) in [CR_LESSER,CR_EQUAL]);
      lt_bigint  : result:=val.compare(P_bigIntLiteral  (other)^.val) in [CR_LESSER,CR_EQUAL];
      lt_smallint: result:=val.compare(P_smallIntLiteral(other)^.val) in [CR_LESSER,CR_EQUAL];
    else result:=(literalType<=other^.literalType); end;
  end;

FUNCTION T_realLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    case other^.literalType of
      lt_bigint  : result:=P_bigIntLiteral(other)^.val.compare(val) in [CR_EQUAL,CR_GREATER];
      lt_smallint: result:=isNan(val) or (val<=P_smallIntLiteral(other)^.val);
      lt_real    : if isNan(val) then result:=not(isNan(P_realLiteral(other)^.val))
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

PROCEDURE T_stringLiteral.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    setLength(val,0);
    enc:=se_testPending;
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
  begin
    if (other^.literalType in C_setTypes) then begin
      if      size<P_compoundLiteral(other)^.size then exit(true)
      else if size>P_compoundLiteral(other)^.size then exit(false)
      else begin
        result:=false;
        //TODO: Is there a nice solution?
      end;
    end else result:=literalType<=other^.literalType;
  end;

FUNCTION T_mapLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    if (other^.literalType in C_mapTypes) then begin
      if      size<P_compoundLiteral(other)^.size then exit(true)
      else if size>P_compoundLiteral(other)^.size then exit(false)
      else begin
        result:=false;
        //TODO: Is there a nice solution?
      end;
    end else result:=literalType<=other^.literalType;
  end;

//?.leqForSorting:==============================================================
FUNCTION T_smallIntLiteral.intValue:int64; begin result:=val; end;
FUNCTION T_bigIntLiteral  .intValue:int64; begin result:=val.toInt; end;

FUNCTION T_smallIntLiteral.floatValue:T_myFloat; begin result:=val; end;
FUNCTION T_bigIntLiteral  .floatValue:T_myFloat; begin result:=val.toFloat; end;
FUNCTION T_realLiteral    .floatValue:T_myFloat; begin result:=val; end;

FUNCTION T_smallIntLiteral.isBetween(CONST lowInclusive,highInclusive:longint):boolean;
  begin result:=(lowInclusive<=val) and (val<=highInclusive); end;
FUNCTION T_bigIntLiteral  .isBetween(CONST lowInclusive,highInclusive:longint):boolean;
  begin result:=val.isBetween(lowInclusive,highInclusive) end;

PROCEDURE T_smallIntLiteral.writeToStream(CONST stream:P_outputStreamWrapper);
  begin writeLongintToStream(val,stream); end;
PROCEDURE T_bigIntLiteral  .writeToStream(CONST stream:P_outputStreamWrapper);
  begin val.writeToStream(stream); end;

FUNCTION T_stringLiteral.softCast(CONST literalRecycler:P_literalRecycler): P_literal;
  VAR
    len: longint;
    otherVal: ansistring;
  begin
    if lowercase(val) = LITERAL_BOOL_TEXT[false] then
      exit(boolLit[false].rereferenced);
    if lowercase(val) = LITERAL_BOOL_TEXT[true] then
      exit(boolLit[true].rereferenced);
    result:=parseNumber(val, 1, false,literalRecycler, len);
    if (result<>nil) then
      if (len = length(val)) then
        exit(result)
      else
        literalRecycler^.disposeLiteral(result);
    otherVal:=unescapeString(sysutils.trim(value),1, len);
    if len = length(sysutils.trim(value)) then
      exit(literalRecycler^.newStringLiteral(otherVal));
    result:=@self;
    rereference;
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
      lt_boolean   : inc(booleans);
      lt_bigint,
      lt_smallint  : inc(ints);
      lt_real      : inc(reals);
      lt_string    : inc(strings);
      else           inc(others);
    end;
    if others>0
    then literalType:=lt_list
    else literalType:=C_listType[booleans>0,ints>0,reals>0,strings>0];
    myHash:=0;
  end;

PROCEDURE T_setLiteral.modifyType(CONST L: P_literal);
  begin
    case L^.literalType of
      lt_boolean   : inc(booleans);
      lt_bigint,
      lt_smallint  : inc(ints);
      lt_real      : inc(reals);
      lt_string    : inc(strings);
      else           inc(others);
    end;
    if others>0
    then literalType:=lt_set
    else literalType:=C_setType[booleans>0,ints>0,reals>0,strings>0];
    myHash:=0;
  end;

FUNCTION T_compoundLiteral.toSet(CONST literalRecycler:P_literalRecycler): P_setLiteral;
  begin
    if literalType in C_setTypes then exit(P_setLiteral(rereferenced));
    result:=P_setLiteral(newSetLiteral(size)^.appendAll(literalRecycler,@self));
  end;

FUNCTION T_compoundLiteral.toList(CONST literalRecycler:P_literalRecycler): P_listLiteral;
  begin
    if literalType in C_listTypes then exit(P_listLiteral(rereferenced));
    result:=P_listLiteral(literalRecycler^.newListLiteral^.appendAll(literalRecycler,@self));
  end;

FUNCTION T_compoundLiteral.toMap(CONST literalRecycler:P_literalRecycler; CONST location:T_tokenLocation; CONST context:P_abstractContext): P_mapLiteral;
  VAR iter:T_arrayOfLiteral;
      pair:P_literal;
  begin
    if literalType in C_mapTypes then exit(P_mapLiteral(rereferenced));
    iter:=P_collectionLiteral(@self)^.tempIteratableList;
    result:=newMapLiteral(length(iter));
    for pair in iter do if (pair^.literalType in C_listTypes) and (P_listLiteral(pair)^.isKeyValuePair) then begin
      result^.put(literalRecycler,
                  P_listLiteral(pair)^.value[0],
                  P_listLiteral(pair)^.value[1],true);
    end else begin
      context^.raiseError('Literal of type '+pair^.typeString+' cannot be interpreted as key-value-pair',location);
      literalRecycler^.disposeLiteral(result);
      result:=nil;
      break;
    end;
    if result=nil then result:=newMapLiteral(0);
  end;

FUNCTION T_listLiteral.appendConstructing(CONST literalRecycler:P_literalRecycler; CONST L: P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST doRangeAppend:boolean):P_compoundLiteral;
  VAR last: P_literal;
      i0, i1: int64;
      c0, c1: char;
      newLen: longint;
  begin
    result:=@self;
    if not(doRangeAppend) then begin
      append(literalRecycler,L, true);
      exit;
    end;
    if fill=0 then begin
      context^.raiseError('Cannot append range to empty list', location);
      exit;
    end;
    last:=dat[fill-1];
    if (last^.literalType in [lt_bigint,lt_smallint]) and P_abstractIntLiteral(last)^.isBetween(-maxLongint,maxLongint) and
       (L   ^.literalType in [lt_bigint,lt_smallint]) and P_abstractIntLiteral(L   )^.isBetween(-maxLongint,maxLongint) then begin
      i0:=P_abstractIntLiteral(last)^.intValue;
      i1:=P_abstractIntLiteral(L   )^.intValue;
      newLen:=fill+abs(i1-i0)+1;
      if newLen>length(dat) then setLength(dat,newLen);
      while (i0<i1) do begin
        inc(i0);
        appendInt(literalRecycler,i0);
      end;
      while (i0>i1) do begin
        dec(i0);
        appendInt(literalRecycler,i0);
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
        appendString(literalRecycler,c0);
      end;
      while c0>c1 do begin
        dec(c0);
        appendString(literalRecycler,c0);
      end;
    end else begin
      literalType:=lt_list;
      context^.raiseError('Invalid range expression '+last^.toString+'..'+L^.toString, location);
    end;
  end;

FUNCTION T_listLiteral.append(CONST literalRecycler:P_literalRecycler; CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false): P_collectionLiteral;
  begin
    result:=@self;
    if (L=nil) or ((L^.literalType=lt_void) and not(forceVoidAppend)) then exit;
    if length(dat)<=fill then setLength(dat,round(fill*1.25)+2);
    dat[fill]:=L;
    inc(fill);
    if incRefs then L^.rereference;
    modifyType(L);
  end;

FUNCTION T_setLiteral.append(CONST literalRecycler:P_literalRecycler; CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false): P_collectionLiteral;
  VAR prevBool:boolean;
      temp:P_literal;
  begin
    result:=@self;
    if (L=nil) or ((L^.literalType=lt_void) and not(forceVoidAppend)) then exit;
    if dat.putNew(L,true,prevBool) then begin
      if incRefs then L^.rereference;
      modifyType(L);
    end else if not(incRefs) then begin
      temp:=L;
      literalRecycler^.disposeLiteral(temp);
    end;
  end;

FUNCTION T_mapLiteral.put(CONST literalRecycler:P_literalRecycler; CONST key,newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
  VAR prevValue:P_literal;
  begin
    if dat.putNew(key,newValue,prevValue) then begin
      if incRefs then key^.rereference;
      literalType:=lt_map;
    end else begin
      literalRecycler^.disposeLiteral(prevValue);
    end;
    if incRefs then newValue^.rereference;
    result:=@self;
  end;

FUNCTION T_mapLiteral.put(CONST literalRecycler:P_literalRecycler; CONST key,newValue:ansistring                 ):P_mapLiteral; begin result:=put(literalRecycler,literalRecycler^.newStringLiteral(key), literalRecycler^.newStringLiteral(newValue),false); end;
FUNCTION T_mapLiteral.put(CONST literalRecycler:P_literalRecycler; CONST key:ansistring; CONST newValue:int64    ):P_mapLiteral; begin result:=put(literalRecycler,literalRecycler^.newStringLiteral(key), literalRecycler^.newIntLiteral   (newValue),false); end;
FUNCTION T_mapLiteral.put(CONST literalRecycler:P_literalRecycler; CONST key:ansistring; CONST newValue:T_myFloat):P_mapLiteral; begin result:=put(literalRecycler,literalRecycler^.newStringLiteral(key), literalRecycler^.newRealLiteral  (newValue),false); end;
FUNCTION T_mapLiteral.put(CONST literalRecycler:P_literalRecycler; CONST key:ansistring; CONST newValue:boolean  ):P_mapLiteral; begin result:=put(literalRecycler,literalRecycler^.newStringLiteral(key), boolLit[newValue].rereferenced,false); end;
FUNCTION T_mapLiteral.put(CONST literalRecycler:P_literalRecycler; CONST key:ansistring; CONST newValue:P_literal; CONST incRefs:boolean):P_mapLiteral;
  begin
    if incRefs then newValue^.rereference;
    result:=put(literalRecycler,
                literalRecycler^.newStringLiteral(key),
                newValue,false);
  end;

FUNCTION T_mapLiteral.put(CONST literalRecycler:P_literalRecycler; CONST key:P_literal; CONST newValue:int64; CONST incRefs:boolean):P_mapLiteral;
  begin
    if incRefs then key^.rereference;
    result:=put(literalRecycler,
                key,
                literalRecycler^.newIntLiteral(newValue),false);
  end;

FUNCTION T_mapLiteral.putAll(CONST literalRecycler:P_literalRecycler; CONST map:P_mapLiteral):P_mapLiteral;
  VAR prevValue:P_literal;
      E:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    for E in map^.dat.keyValueList do begin
      if dat.putNew(E,prevValue) then begin
        E.key^.rereference;
        literalType:=lt_map;
      end else begin
        literalRecycler^.disposeLiteral(prevValue);
      end;
      E.value^.rereference;
    end;
    result:=@self;
  end;

PROCEDURE T_mapLiteral.drop(CONST literalRecycler:P_literalRecycler; CONST L: P_literal);
  VAR dropped:T_literalKeyLiteralValueMap.CACHE_ENTRY;
  begin
    dropped:=dat.drop(L);
    if dropped.key=nil then exit;
    literalRecycler^.disposeLiteral(dropped.key);
    literalRecycler^.disposeLiteral(dropped.value);
    if dat.fill<=0 then literalType:=lt_map;
  end;

PROCEDURE T_setLiteral.drop(CONST literalRecycler:P_literalRecycler; CONST L:P_literal);
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
    literalRecycler^.disposeLiteral(dropped.key);
    case L^.literalType of
      lt_boolean: dec(booleans);
      lt_smallint,
      lt_bigint : dec(ints);
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

PROCEDURE T_listLiteral.sortBySubIndex(CONST innerIndex: longint; CONST location: T_tokenLocation; CONST context:P_abstractContext);
  VAR temp: T_arrayOfLiteral;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(a,b:P_literal):boolean; inline;
    begin
      if (a^.literalType in C_listTypes) and (b^.literalType in C_listTypes) then begin
        if (P_listLiteral(a)^.fill>innerIndex) then begin
          if (P_listLiteral(b)^.fill>innerIndex)
          then result:=P_listLiteral(a)^.dat[innerIndex]^.leqForSorting(P_listLiteral(b)^.dat[innerIndex])
          else result:=P_listLiteral(a)^.dat[innerIndex]^.leqForSorting(@voidLit)
        end else begin
          if (P_listLiteral(b)^.fill>innerIndex)
          then result:=voidLit.leqForSorting(P_listLiteral(b)^.dat[innerIndex])
          else result:=false;
        end;
      end else begin
        result:=false;
        if context<>nil then context^.raiseError('Invalid sorting index '+intToStr(innerIndex)+' for elements '+a^.toString(50)+' and '+b^.toString(50),location);
      end;
    end;

  begin
    if fill<=1 then exit;
    myHash:=0;
    scale:=1;
    setLength(temp, fill);
    while (scale<fill) and ((context=nil) or (context^.continueEvaluation)) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while (i<fill) and ((context=nil) or (context^.continueEvaluation)) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<fill) do
          if isLeq(dat[j0],dat[j1])          then begin temp[k]:=dat[j0]; inc(k); inc(j0); end
                                             else begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<fill) do begin temp[k]:=dat[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<fill) do begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      if not(((context=nil) or (context^.continueEvaluation))) then exit;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<fill) and ((context=nil) or (context^.continueEvaluation)) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while (i<fill) and ((context=nil) or (context^.continueEvaluation)) do begin
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

PROCEDURE T_listLiteral.customSort(CONST leqExpression: P_expressionLiteral; CONST location: T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer);
  VAR temp: T_arrayOfLiteral;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(CONST a,b:P_literal):boolean; inline; begin result:=leqExpression^.evaluateToBoolean(location,context,recycler,true,a,b); end;

  begin
    if fill<=1 then exit;
    myHash:=0;
    scale:=1;
    setLength(temp, fill);
    while (scale<fill) and context^.continueEvaluation do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while (i<fill) and context^.continueEvaluation do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<fill) do
          if isLeq(dat[j0],dat[j1])          then begin temp[k]:=dat[j0]; inc(k); inc(j0); end
                                             else begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<fill) do begin temp[k]:=dat[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<fill) do begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      if not(context^.continueEvaluation) then exit;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<fill) and context^.continueEvaluation then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while (i<fill) and context^.continueEvaluation do begin
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

FUNCTION T_listLiteral.sortPerm(CONST literalRecycler:P_literalRecycler): P_listLiteral;
  VAR temp1, temp2: array of record
        v: P_literal;
        index: longint;
      end;
      scale: longint;
      i,j0,j1,k: longint;
  begin
    if fill = 0 then exit(literalRecycler^.newListLiteral);
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
    result:=literalRecycler^.newListLiteral(length(temp1));
    for i:=0 to length(temp1)-1 do result^.appendInt(literalRecycler,temp1[i].index);
    setLength(temp1, 0);
  end;

PROCEDURE T_listLiteral.unique(CONST literalRecycler:P_literalRecycler);
  VAR i,j:longint;
  begin
    if fill<=0 then exit;
    myHash:=0;
    sort;
    j:=0;
    for i:=1 to fill-1 do
    if dat[i]^.equals(dat[j]) then begin
      literalRecycler^.disposeLiteral(dat[i]);
    end else begin
      inc(j);
      dat[j]:=dat[i];
    end;
    fill:=j+1;
  end;

FUNCTION T_listLiteral.clone(CONST literalRecycler:P_literalRecycler): P_compoundLiteral;
  VAR i:longint;
  begin
    result:=literalRecycler^.newListLiteral(fill);
    for i:=0 to fill-1 do P_listLiteral(result)^.dat[i]:=dat[i]^.rereferenced;
    P_listLiteral(result)^.fill       :=fill;
    P_listLiteral(result)^.literalType:=literalType;
    P_listLiteral(result)^.ints       :=ints    ;
    P_listLiteral(result)^.reals      :=reals   ;
    P_listLiteral(result)^.strings    :=strings ;
    P_listLiteral(result)^.booleans   :=booleans;
    P_listLiteral(result)^.others     :=others  ;
  end;

FUNCTION T_setLiteral.clone(CONST literalRecycler:P_literalRecycler): P_compoundLiteral;
  begin
    new(P_setLiteral(result),createClone(self));
  end;

FUNCTION T_mapLiteral.clone(CONST literalRecycler:P_literalRecycler): P_compoundLiteral;
  begin
    new(P_mapLiteral(result),createClone(self));
  end;

FUNCTION T_listLiteral.tempIteratableList: T_arrayOfLiteral;
  VAR i:longint;
  begin
    setLength(result,fill);
    for i:=0 to fill-1 do result[i]:=dat[i];
  end;

FUNCTION T_setLiteral.tempIteratableList: T_arrayOfLiteral;
  begin
    result:=dat.keySet;
  end;

FUNCTION T_mapLiteral.entryList:T_arrayOfKeyValuePair;
  VAR e:T_literalKeyLiteralValueMap.KEY_VALUE_LIST;
      i:longint;
  begin
    e:=dat.keyValueList;
    setLength(result,length(e));
    for i:=0 to length(e)-1 do begin
      result[i].key  :=e[i].key  ^.rereferenced;
      result[i].value:=e[i].value^.rereferenced;
    end;
  end;

FUNCTION T_collectionLiteral.forcedIteratableList(CONST literalRecycler:P_literalRecycler):T_arrayOfLiteral;
  VAR L:P_literal;
  begin
    result:=tempIteratableList;
    for L in result do L^.rereference;
  end;

FUNCTION T_mapLiteral.forcedIteratableList(CONST literalRecycler:P_literalRecycler):T_arrayOfLiteral;
  VAR e:T_literalKeyLiteralValueMap.KEY_VALUE_LIST;
      i:longint;
  begin
    e:=dat.keyValueList;
    setLength(result,length(e));
    for i:=0 to length(e)-1 do result[i]:=literalRecycler^.newListLiteral(e[i].key,e[i].value);
  end;

FUNCTION T_mapLiteral.underlyingMap:P_literalKeyLiteralValueMap;
  begin
    result:=@dat;
  end;

PROCEDURE T_mapLiteral.ensureType;
  begin
    if dat.fill=0 then literalType:=lt_emptyMap else literalType:=lt_map;
  end;

FUNCTION T_mapLiteral.keyIteratableList:T_arrayOfLiteral;
  VAR L:P_literal;
  begin
    result:=dat.keySet;
    for L in result do L^.rereference;
  end;

FUNCTION setUnion(CONST literalRecycler:P_literalRecycler; CONST params:P_listLiteral):P_setLiteral;
  VAR i:longint;
      expectedSetSize:longint=0;
      part:P_compoundLiteral;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^.value[i]^.literalType in C_compoundTypes) then exit(nil) else inc(expectedSetSize,P_compoundLiteral(params^.value[i])^.size);
    if params^.size=1 then exit(P_compoundLiteral(params^.value[0])^.toSet(literalRecycler));
    result:=newSetLiteral(expectedSetSize);
    for i:=0 to params^.size-1 do begin
      part:=P_compoundLiteral(params^.value[i]);
      case part^.literalType of
        lt_emptyList,lt_emptyMap,lt_emptySet: begin end;
        else result^.appendAll(literalRecycler,P_collectionLiteral(part));
      end;
    end;
  end;

FUNCTION setIntersect(CONST literalRecycler:P_literalRecycler; CONST params:P_listLiteral):P_setLiteral;
  TYPE T_occurenceCount=specialize G_literalKeyMap<word>;
  CONST bit:array[0..15] of word=(1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768);
  VAR counterSet:T_occurenceCount;
      prevMask:word; //dummy
      i:longint;
      maxSubsetSize:longint=0;
      smallestSetIndex:longint=0;
      entry:T_occurenceCount.CACHE_ENTRY;
      iter:T_arrayOfLiteral;
      x:P_literal;
      acceptMask:word=0;
      allSets:boolean=true;

  FUNCTION resultByRecursion:P_setLiteral;
    VAR innerCallParam,outerCallParam:P_listLiteral;
        i:longint;
    begin
      outerCallParam:=literalRecycler^.newListLiteral();
      innerCallParam:=literalRecycler^.newListLiteral(16);
      for i:=0 to params^.size-1 do begin
        innerCallParam^.append(literalRecycler,params^.dat[i],true);
        if innerCallParam^.size>=16 then begin
          outerCallParam^.append(literalRecycler,setIntersect(literalRecycler,innerCallParam),false);
          literalRecycler^.disposeLiteral(innerCallParam);
          innerCallParam:=literalRecycler^.newListLiteral(16);
        end;
      end;
      if innerCallParam^.size>=1 then outerCallParam^.append(literalRecycler,setIntersect(literalRecycler,innerCallParam),false);
      literalRecycler^.disposeLiteral(innerCallParam);
      result:=setIntersect(literalRecycler,outerCallParam);
      literalRecycler^.disposeLiteral(outerCallParam);
    end;

  FUNCTION resultByContains:P_setLiteral;
    VAR iter:T_literalKeyBooleanValueMap.KEY_VALUE_LIST;
        elem:T_literalKeyBooleanValueMap.CACHE_ENTRY;
        resultMap:P_literalKeyBooleanValueMap;
        inAll:boolean;
        previousValueDummy:boolean;
        k:longint;
    begin
      iter:=P_setLiteral(params^.value[smallestSetIndex])^.dat.keyValueList;
      result:=newSetLiteral(length(iter));
      resultMap:=@(result^.dat);
      for elem in iter do begin
        inAll:=true;
        for k:=0 to params^.size-1 do inAll:=inAll and ((k=smallestSetIndex) or P_setLiteral(params^.value[k])^.dat.containsKey(elem));
        if inAll then begin
          resultMap^.putNew(elem,previousValueDummy);
          result^.modifyType(elem.key);
          elem.key^.rereference;
        end;
      end;
    end;

  begin
    if not((params<>nil) and (params^.size>=1))
    then exit(nil);
    for i:=0 to params^.size-1 do
      if not(params^.value[i]^.literalType in C_collectionTypes)
      then exit(nil)
      else begin
        allSets:=allSets and (params^.value[i]^.literalType in C_setTypes);
        if P_compoundLiteral(params^.value[i])^.size<P_compoundLiteral(params^.value[smallestSetIndex])^.size
        then smallestSetIndex:=i;
        if P_compoundLiteral(params^.value[i])^.size>maxSubsetSize then maxSubsetSize:=P_compoundLiteral(params^.value[i])^.size;
      end;
    if params^.size=1 then exit(P_compoundLiteral(params^.value[0])^.toSet(literalRecycler));
    if allSets then exit(resultByContains);
    if params^.size>16 then exit(resultByRecursion);

    counterSet.create(maxSubsetSize div 2);
    for i:=0 to params^.size-1 do begin
      iter:=P_collectionLiteral(params^.value[i])^.tempIteratableList;
      for x in iter do counterSet.putNew(x,counterSet.get(x,0) or bit[i],prevMask);
      inc(acceptMask,bit[i]);
    end;
    result:=newSetLiteral(maxSubsetSize);
    for entry in counterSet.keyValueList do if (entry.value=acceptMask) then  begin
      result^.dat.putNew(entry,allSets);
      result^.modifyType(entry.key);
      entry.key^.rereference;
    end;
    counterSet.destroy;
  end;

FUNCTION setMinus(CONST literalRecycler:P_literalRecycler; CONST params:P_listLiteral):P_setLiteral;
  VAR L:P_literal;
      iter:T_arrayOfLiteral;
      s:P_setLiteral;
  begin
    if not((params<>nil) and
           (params^.size=2) and
           (params^.value[0]^.literalType in C_collectionTypes) and
           (params^.value[1]^.literalType in C_collectionTypes))
    then exit(nil);
    iter:=P_collectionLiteral(params^.value[0])^.tempIteratableList;
    result:=newSetLiteral(length(iter));
    if params^.value[1]^.literalType in C_setTypes then begin
      s:=P_setLiteral(params^.value[1]);
      for L in iter do if not(s^.contains(L)) then result^.append(literalRecycler,L,true);
    end else begin
      for L in iter do result^.dat.put(L,true);
      iter:=P_collectionLiteral(params^.value[1])^.tempIteratableList;
      for L in iter do result^.dat.drop(L);
      for L in result^.dat.keySet do begin
        L^.rereference;
        result^.modifyType(L);
      end;
    end;
  end;

FUNCTION mapMerge(CONST literalRecycler:P_literalRecycler; CONST params:P_listLiteral; CONST location:T_tokenLocation; CONST contextPointer:P_abstractContext; CONST recycler:pointer):P_mapLiteral;
  VAR map1,map2:P_mapLiteral;
      merger:P_expressionLiteral;
      entry:T_literalKeyLiteralValueMap.CACHE_ENTRY;
      pTargetEntry:T_literalKeyLiteralValueMap.P_CACHE_ENTRY;
      M:P_literal;
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
    new(result,createClone(map1^));

    for entry in map2^.dat.keyValueList do begin
      pTargetEntry:=result^.dat.getEntry(entry.key);
      if pTargetEntry=nil then begin
        result^.dat.putNew(entry,M);
        entry.key  ^.rereference;
        entry.value^.rereference;
      end else begin
        M:=merger^.evaluateToLiteral(location,contextPointer,recycler,pTargetEntry^.value,entry.value).literal;
        if M=nil then begin
          literalRecycler^.disposeLiteral(result);
          exit(nil);
        end;
        literalRecycler^.disposeLiteral(pTargetEntry^.value);
        pTargetEntry^.value:=M;
      end;
    end;
    if result^.dat.fill>0 then result^.literalType:=lt_map
                          else result^.literalType:=lt_emptyMap;
  end;

FUNCTION mutateVariable(CONST literalRecycler:P_literalRecycler; VAR toMutate:P_literal; CONST mutation:T_tokenType; CONST parameters:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer):P_literal;
  VAR returnValue:P_literal=nil;
  PROCEDURE return(CONST L:P_literal); inline;
    begin
      if returnValue<>nil then literalRecycler^.disposeLiteral(returnValue);
      returnValue:=L;
    end;

  FUNCTION simpleMutate(VAR toMutate:P_literal; CONST op:T_tokenType; CONST RHS:P_literal):boolean;
    VAR newValue:P_literal;
    begin
      newValue:=resolveOperatorCallback(toMutate,op,RHS,location,context,recycler);
      if newValue<>nil then begin
        result:=newValue^.literalType<>toMutate^.literalType;
        literalRecycler^.disposeLiteral(toMutate);
        toMutate:=newValue;
        return(toMutate^.rereferenced);
      end else result:=false;
    end;

  FUNCTION assign(VAR toMutate:P_literal; CONST newValue:P_literal):boolean;
    begin
      result:=newValue^.literalType<>toMutate^.literalType;
      literalRecycler^.disposeLiteral(toMutate);
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
        then P_collectionLiteral(toMutate)^.append(literalRecycler,RHS, true)
        else P_collectionLiteral(toMutate)^.appendAll(literalRecycler,P_compoundLiteral(RHS));
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
      old:=toMutate;
      toMutate:=old^.clone(literalRecycler);
      old^.unreference;
    end;

  FUNCTION mutateDrop(VAR toMutate:P_literal; CONST RHS:P_literal):boolean;
    begin
      result:=false;
      if (toMutate^.literalType in C_setTypes) then begin
        ensureExclusiveAccess(P_setLiteral(toMutate));
        P_setLiteral(toMutate)^.drop(literalRecycler,RHS);
      end else if (toMutate^.literalType in C_mapTypes) then begin
        ensureExclusiveAccess(P_mapLiteral(toMutate));
        P_mapLiteral(toMutate)^.drop(literalRecycler,RHS);
      end else if (toMutate^.literalType in C_listTypes) and (RHS^.literalType in [lt_bigint,lt_smallint]) then begin
        ensureExclusiveAccess(P_listLiteral(toMutate));
        P_listLiteral(toMutate)^.removeElement(literalRecycler,P_abstractIntLiteral(RHS)^.intValue);
      end else context^.raiseError('Cannot drop from literal of type '+toMutate^.typeString,location);
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
      accessorTail:=accessor^.tail(literalRecycler);
      result:=false;
      if toMutate^.literalType in C_listTypes then begin
        if accessor^.value[0]^.literalType in [lt_bigint,lt_smallint] then begin
          listIndex:=P_abstractIntLiteral(accessor^.value[0])^.intValue;
          if (listIndex>=0) and (listIndex<P_listLiteral(toMutate)^.fill) then begin
            ensureExclusiveAccess(P_listLiteral(toMutate));
            prevType:=P_listLiteral(toMutate)^.dat[listIndex]^.literalType;
            if mutateNested(P_listLiteral(toMutate)^.dat[listIndex],nestedMutation,accessorTail,RHS) then begin
              if prevType<>P_listLiteral(toMutate)^.dat[listIndex]^.literalType then begin
                case prevType of
                  lt_boolean: dec(P_listLiteral(toMutate)^.booleans);
                  lt_smallint,
                  lt_bigint:  dec(P_listLiteral(toMutate)^.ints);
                  lt_real:    dec(P_listLiteral(toMutate)^.reals);
                  lt_string:  dec(P_listLiteral(toMutate)^.strings);
                  else        dec(P_listLiteral(toMutate)^.others);
                end;
                case P_listLiteral(toMutate)^.dat[listIndex]^.literalType of
                  lt_boolean: inc(P_listLiteral(toMutate)^.booleans);
                  lt_smallint,
                  lt_bigint:  inc(P_listLiteral(toMutate)^.ints);
                  lt_real:    inc(P_listLiteral(toMutate)^.reals);
                  lt_string:  inc(P_listLiteral(toMutate)^.strings);
                  else        inc(P_listLiteral(toMutate)^.others);
                end;
                P_listLiteral(toMutate)^.myHash:=0;
                if P_listLiteral(toMutate)^.fill=0 then P_listLiteral(toMutate)^.literalType:=lt_emptyList
                else if P_listLiteral(toMutate)^.others>0 then P_listLiteral(toMutate)^.literalType:=lt_list
                else P_listLiteral(toMutate)^.literalType:=C_listType[P_listLiteral(toMutate)^.booleans>0,P_listLiteral(toMutate)^.ints>0,P_listLiteral(toMutate)^.reals>0,P_listLiteral(toMutate)^.strings>0];
              end;
            end;
          end else if listIndex=P_listLiteral(toMutate)^.fill then begin
            ensureExclusiveAccess(P_listLiteral(toMutate));
            elementToMutate:=newVoidLiteral;
            mutateNested(elementToMutate,nestedMutation,accessorTail,RHS);
            P_listLiteral(toMutate)^.append(literalRecycler,elementToMutate,false);
          end else context^.raiseError('List index out of bounds',location)
        end else context^.raiseError('List elements must be qualified by their index',location);
      end else if toMutate^.literalType in C_mapTypes then begin
        ensureExclusiveAccess(P_mapLiteral(toMutate));
        mapEntry:=P_mapLiteral(toMutate)^.dat.getEntry(accessor^.value[0]);
        if (mapEntry<>nil) then begin
          mutateNested(mapEntry^.value,nestedMutation,accessorTail,RHS);
        end else begin
          elementToMutate:=newVoidLiteral;
          mutateNested(elementToMutate,nestedMutation,accessorTail,RHS);
          P_mapLiteral(toMutate)^.put(literalRecycler,accessor^.value[0]^.rereferenced,elementToMutate,false);
        end;
      end else context^.raiseError('Cannot apply nested mutation to literal of type '+toMutate^.typeString,location);
      literalRecycler^.disposeLiteral(accessorTail);
    end;

  PROCEDURE mutateNested;
    VAR accessor:P_listLiteral;
        RHS:P_literal;
        temp:P_listLiteral;
    begin
      accessor:=P_listLiteral(parameters)^.leading(literalRecycler);
      RHS     :=P_listLiteral(parameters)^.trailing(literalRecycler);
      if (RHS^.literalType=lt_void) then begin
        if mutation=tt_mut_nested_assign then begin
          // M[]:=void -> M:=void;
          if (accessor^.size=0) then assign(toMutate,RHS)
          // M[key  ]:=void -> M >> key
          else if (accessor^.size=1) then mutateDrop(toMutate,accessor^.value[0])
          // M[k1,k2]:=void -> M[k1] >> k2
          else begin
            literalRecycler^.disposeLiteral(RHS);
            temp:=accessor;
            accessor:=temp^.leading(literalRecycler);
            RHS:=temp^.trailing(literalRecycler);
            literalRecycler^.disposeLiteral(temp);
            mutateNested(toMutate,tt_mut_nestedDrop,accessor,RHS);
          end;
        end;
      end else mutateNested(toMutate,mutation,accessor,RHS);
      literalRecycler^.disposeLiteral(accessor);
      literalRecycler^.disposeLiteral(RHS);
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
        context^.raiseError('Nested mutation expects a compound literal on the left hand side and a list literal on the right hand side'+C_lineBreakChar+
                            'RHS: '+toMutate^.typeString+' '+toMutate^.toString(50)+C_lineBreakChar+
                            'LHS: '+parameters^.typeString+' '+parameters^.toString(50),location);
        exit(nil);
      end else mutateNested;
      else begin
        context^.raiseError('Unimplemented mutation '+C_tokenDefaultId[mutation],location);
      end;
    end;
    result:=returnValue;
  end;

FUNCTION newLiteralFromStream(CONST literalRecycler:P_literalRecycler; CONST stream:P_inputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):P_literal;
  VAR reusableLiterals:PP_literal;
      reusableFill:longint=0;
      encodingMethod:byte=0;
  PROCEDURE errorOrException(CONST message:string);
    begin
      if adapters<>nil then adapters^.raiseSimpleError(message,location)
                       else raise Exception.create(message);
    end;

  FUNCTION typeStringOrNone(CONST t:T_literalType):string;
    begin
      if (t>=low(T_literalType)) and (t<=high(T_literalType)) then result:=C_typeInfo[t].name else result:='';
    end;

  FUNCTION literalFromStream254:P_literal;
    VAR literalType:T_literalType;
        reusableIndex:longint;
        listSize:longint;
        i:longint;
        mapKey,mapValue:P_literal;
        tempInt:T_bigInt;

    FUNCTION byteToType(CONST b:byte):T_literalType;
      begin
        case b of
          1: result:=lt_boolean    ;
          2: result:=lt_smallint   ;
          3: result:=lt_real       ;
          4: result:=lt_string     ;
          6: result:=lt_list       ;
          7: result:=lt_booleanList;
          8: result:=lt_intList    ;
          9: result:=lt_realList   ;
         10: result:=lt_numList    ;
         11: result:=lt_stringList ;
         12: result:=lt_emptyList  ;
         13: result:=lt_set        ;
         14: result:=lt_booleanSet ;
         15: result:=lt_intSet     ;
         16: result:=lt_realSet    ;
         17: result:=lt_numSet     ;
         18: result:=lt_stringSet  ;
         19: result:=lt_emptySet   ;
         20: result:=lt_map        ;
         21: result:=lt_emptyMap   ;
         22: result:=lt_void       ;
        else result:=lt_error;
        end;
      end;

    FUNCTION isSameType(CONST a,b:T_literalType):boolean;
      begin
        result:=(a=b) or (a in [lt_smallint,lt_bigint])
                     and (b in [lt_smallint,lt_bigint]);
      end;

    begin
      reusableIndex:=stream^.readNaturalNumber;
      if reusableIndex<=22 then literalType:=byteToType(byte(reusableIndex))
      else begin
        dec(reusableIndex,23);
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
        lt_boolean  : result:=boolLit[stream^.readBoolean].rereferenced;
        lt_smallint : begin
                        tempInt.readFromStream(stream);
                        result:=literalRecycler^.newBigIntLiteral(tempInt);
                      end;
        lt_real     : result:=literalRecycler^.newRealLiteral  (stream^.readDouble    );
        lt_string   : result:=literalRecycler^.newStringLiteral(stream^.readAnsiString);
        lt_emptyList: result:=literalRecycler^.newListLiteral;
        lt_emptySet : result:=newSetLiteral(0);
        lt_emptyMap : result:=newMapLiteral(0);
        lt_void     : result:=newVoidLiteral;
        lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList,
        lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral(listSize)
          else result:=literalRecycler^.newListLiteral(listSize);
          case literalType of
            lt_booleanList,lt_booleanSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendBool(literalRecycler,stream^.readBoolean);
            lt_intList,lt_intSet:
              for i:=0 to listSize-1 do if stream^.allOkay then begin
                tempInt.readFromStream(stream);
                P_collectionLiteral(result)^.append(literalRecycler,literalRecycler^.newBigIntLiteral(tempInt),false);
              end;
            lt_realList,lt_realSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendReal(literalRecycler,stream^.readDouble);
            lt_stringList,lt_stringSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendString(literalRecycler,stream^.readAnsiString);
            else
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.append(literalRecycler,literalFromStream254(),false);
          end;
          if P_collectionLiteral(result)^.size<>listSize then errorOrException('Invalid collection. Expected size of '+intToStr(listSize)+' but got '+result^.typeString);
        end;
        lt_map: begin
          result:=newMapLiteral(0);
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
      if not(isSameType(result^.literalType,literalType)) then errorOrException('Deserializaion result has other type ('+typeStringOrNone(result^.literalType)+') than expected ('+typeStringOrNone(literalType)+').');
      if not(stream^.allOkay) then errorOrException('Unknown error during deserialization.');
      if ((literalType=lt_string) or (literalType in C_compoundTypes)) and (reusableFill<2097151) then begin
        reusableLiterals[reusableFill]:=result;
        inc(reusableFill);
      end;
    end;

  FUNCTION literalFromStream253:P_literal;
    VAR literalType:T_literalType;
        customTypeName:T_idString='';
        customType:P_typedef=nil;
        reusableIndex:longint=-1;
        listSize:longint;
        i:longint;
        mapKey,mapValue:P_literal;
        tempInt:T_bigInt;

      FUNCTION byteToType(CONST b:byte):T_literalType;
        begin
          case b of
            2, 3: result:=lt_boolean    ;
            4, 5: result:=lt_bigint     ;
            6, 7: result:=lt_real       ;
            8, 9: result:=lt_string     ;
           12,13: result:=lt_list       ;
           14,15: result:=lt_booleanList;
           16,17: result:=lt_intList    ;
           18,19: result:=lt_realList   ;
           20,21: result:=lt_numList    ;
           22,23: result:=lt_stringList ;
           24,25: result:=lt_emptyList  ;
           26,27: result:=lt_set        ;
           28,29: result:=lt_booleanSet ;
           30,31: result:=lt_intSet     ;
           32,33: result:=lt_realSet    ;
           34,35: result:=lt_numSet     ;
           36,37: result:=lt_stringSet  ;
           38,39: result:=lt_emptySet   ;
           40,41: result:=lt_map        ;
           42,43: result:=lt_emptyMap   ;
           44,45: result:=lt_void       ;
          else result:=lt_error;
          end;
        end;

      FUNCTION isSameType(CONST a,b:T_literalType):boolean;
        begin
          result:=(a=b) or (a in [lt_smallint,lt_bigint])
                       and (b in [lt_smallint,lt_bigint]);
        end;

    begin
      reusableIndex:=stream^.readNaturalNumber;
      if reusableIndex<=45 then begin
        literalType:=byteToType(reusableIndex);
        if odd(reusableIndex)
        then customTypeName:=stream^.readAnsiString
        else customTypeName:='';
      end
      else begin
        dec(reusableIndex,46);
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
        lt_boolean  : if customTypeName=''
                      then result:=boolLit[stream^.readBoolean].rereferenced
                      else new(P_boolLiteral(result),create(stream^.readBoolean));
        lt_bigint   : begin
                        tempInt.readFromStream(stream);
                        result:=literalRecycler^.newBigIntLiteral(tempInt);
                      end;
        lt_real     : result:=literalRecycler^.newRealLiteral  (stream^.readDouble    );
        lt_string   : result:=literalRecycler^.newStringLiteral(stream^.readAnsiString,customTypeName<>'');
        lt_emptyList: result:=literalRecycler^.newListLiteral;
        lt_emptySet : result:=newSetLiteral(0);
        lt_emptyMap : result:=newMapLiteral(0);
        lt_void     : result:=newVoidLiteral;
        lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList,
        lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral(listSize)
          else result:=literalRecycler^.newListLiteral(listSize);
          case literalType of
            lt_booleanList,lt_booleanSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendBool(literalRecycler,stream^.readBoolean);
            lt_intList,lt_intSet:
              for i:=0 to listSize-1 do if stream^.allOkay then begin
                tempInt.readFromStream(stream);
                P_collectionLiteral(result)^.append(literalRecycler,literalRecycler^.newBigIntLiteral(tempInt),false);
              end;
            lt_realList,lt_realSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendReal(literalRecycler,stream^.readDouble);
            lt_stringList,lt_stringSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendString(literalRecycler,stream^.readAnsiString);
            else
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.append(literalRecycler,literalFromStream253(),false);
          end;
          if P_collectionLiteral(result)^.size<>listSize then errorOrException('Invalid collection. Expected size of '+intToStr(listSize)+' but got '+result^.typeString);
        end;
        lt_map: begin
          result:=newMapLiteral(0);
          listSize:=stream^.readNaturalNumber;
          for i:=0 to listSize-1 do if stream^.allOkay then begin
            mapKey  :=literalFromStream253();
            mapValue:=literalFromStream253();
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
      if not(isSameType(result^.literalType,literalType)) then errorOrException('Deserializaion result has other type ('+typeStringOrNone(result^.literalType)+') than expected ('+typeStringOrNone(literalType)+').');
      if customTypeName<>'' then begin
        if literalType in C_typables then begin
          if typeMap.containsKey(customTypeName,customType) then begin
            P_compoundLiteral(result)^.customType:=customType;
          end else errorOrException('Read unknown custom type '+customTypeName);
        end else errorOrException('Read invalid custom for literal of type '+C_typeInfo[literalType].name);
      end;

      if not(stream^.allOkay) then errorOrException('Unknown error during deserialization.');
      if ((literalType=lt_string) or (literalType in C_compoundTypes) or (customTypeName<>'')) and (reusableFill<2097151) then begin
        reusableLiterals[reusableFill]:=result;
        inc(reusableFill);
      end;
    end;

  FUNCTION nextIntFromStream:P_abstractIntLiteral;
    VAR markerByte:byte;
        big:T_bigInt;
        small:longint;
    begin
      markerByte:=stream^.readByte;
      if markerByte>=253 then begin
        big.readFromStream(markerByte,stream);
        new(P_bigIntLiteral(result),create(big));
      end else begin
        small:=readLongintFromStream(markerByte,stream);
        result:=literalRecycler^.newIntLiteral(small);
      end;
    end;

  FUNCTION literalFromStream252:P_literal;
    VAR literalType:T_literalType;
        customTypeName:T_idString='';
        customType:P_typedef=nil;
        reusableIndex:longint=-1;
        listSize:longint;
        i:longint;
        mapKey,mapValue:P_literal;

      FUNCTION byteToType(CONST b:byte):T_literalType;
        begin
          result:=T_literalType(b shr 1);
        end;

    begin
      reusableIndex:=stream^.readNaturalNumber;
      if reusableIndex<=47 then begin
        literalType:=byteToType(reusableIndex);
        if odd(reusableIndex)
        then customTypeName:=stream^.readAnsiString
        else customTypeName:='';
      end
      else begin
        dec(reusableIndex,48);
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
        lt_boolean  : result:=boolLit[stream^.readBoolean].rereferenced;
        lt_smallint,lt_bigint: result:=nextIntFromStream;
        lt_real     : result:=literalRecycler^.newRealLiteral  (stream^.readDouble    );
        lt_string   : result:=literalRecycler^.newStringLiteral(stream^.readAnsiString,customTypeName<>'');
        lt_emptyList: result:=literalRecycler^.newListLiteral;
        lt_emptySet : result:=newSetLiteral(0);
        lt_emptyMap : result:=newMapLiteral(0);
        lt_void     : result:=newVoidLiteral;
        lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList,
        lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral(listSize)
          else result:=literalRecycler^.newListLiteral(listSize);
          case literalType of
            lt_booleanList,lt_booleanSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendBool(literalRecycler,stream^.readBoolean);
            lt_intList,lt_intSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.append(literalRecycler,nextIntFromStream,false);
            lt_realList,lt_realSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendReal(literalRecycler,stream^.readDouble);
            lt_stringList,lt_stringSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendString(literalRecycler,stream^.readAnsiString);
            else
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.append(literalRecycler,literalFromStream252(),false);
          end;
          if P_collectionLiteral(result)^.size<>listSize then errorOrException('Invalid collection. Expected size of '+intToStr(listSize)+' but got '+result^.typeString);
        end;
        lt_map: begin
          result:=newMapLiteral(0);
          listSize:=stream^.readNaturalNumber;
          for i:=0 to listSize-1 do if stream^.allOkay then begin
            mapKey  :=literalFromStream252();
            mapValue:=literalFromStream252();
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
      if customTypeName<>'' then begin
        if literalType in C_typables then begin
          if typeMap.containsKey(customTypeName,customType) then begin
            P_typableLiteral(result)^.customType:=customType;
          end else errorOrException('Read unknown custom type '+customTypeName);
        end else errorOrException('Read invalid custom for literal of type '+C_typeInfo[literalType].name);
      end;

      if not(stream^.allOkay) then errorOrException('Unknown error during deserialization.');
      if ((literalType=lt_string) or (literalType in C_compoundTypes) or (customTypeName<>'')) and (reusableFill<2097151) then begin
        reusableLiterals[reusableFill]:=result;
        inc(reusableFill);
      end;
    end;

  FUNCTION literalFromStream251:P_literal;
    VAR literalType:T_literalType;
        customTypeName:T_idString='';
        customType:P_typedef=nil;
        reusableIndex:longint=-1;
        listSize:longint;
        i:longint;
        mapKey,mapValue:P_literal;

      FUNCTION byteToType(CONST b:byte):T_literalType;
        begin
          result:=T_literalType(b shr 1);
        end;

    begin
      reusableIndex:=stream^.readNaturalNumber;
      if reusableIndex<=47 then begin
        literalType:=byteToType(reusableIndex);
        if odd(reusableIndex)
        then customTypeName:=stream^.readAnsiString
        else customTypeName:='';
      end
      else begin
        dec(reusableIndex,48);
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
        lt_boolean  : result:=boolLit[stream^.readBoolean].rereferenced;
        lt_smallint,lt_bigint: result:=nextIntFromStream;
        lt_real     : result:=literalRecycler^.newRealLiteral  (stream^.readDouble    );
        lt_string   : result:=literalRecycler^.newStringLiteral(stream^.readAnsiString,customTypeName<>'');
        lt_emptyList: result:=literalRecycler^.newListLiteral;
        lt_emptySet : result:=newSetLiteral(0);
        lt_emptyMap : result:=newMapLiteral(0);
        lt_void     : result:=newVoidLiteral;
        lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList,
        lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet: begin
          listSize:=stream^.readNaturalNumber;
          if literalType in C_setTypes
          then result:=newSetLiteral(listSize)
          else result:=literalRecycler^.newListLiteral(listSize);
          case literalType of
            lt_booleanList,lt_booleanSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendBool(literalRecycler,stream^.readBoolean);
            lt_intList,lt_intSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.append(literalRecycler,nextIntFromStream,false);
            lt_realList,lt_realSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendReal(literalRecycler,stream^.readDouble);
            lt_stringList,lt_stringSet:
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.appendString(literalRecycler,stream^.readAnsiString);
            else
              for i:=0 to listSize-1 do if stream^.allOkay then P_collectionLiteral(result)^.append(literalRecycler,literalFromStream251(),false);
          end;
          if P_collectionLiteral(result)^.size<>listSize then errorOrException('Invalid collection. Expected size of '+intToStr(listSize)+' but got '+result^.typeString);
        end;
        lt_map: begin
          result:=newMapLiteral(0);
          listSize:=stream^.readNaturalNumber;
          for i:=0 to listSize-1 do if stream^.allOkay then begin
            mapKey  :=literalFromStream251();
            mapValue:=literalFromStream251();
            P_mapLiteral(result)^.dat.put(mapKey,mapValue);
          end;
          result^.literalType:=lt_map;
        end;
        lt_expression: begin
          result:=readExpressionFromStreamCallback(literalRecycler,stream,location,adapters,typeMap);
          if result=nil then result:=newVoidLiteral;
        end;
        else begin
          errorOrException('Read invalid literal type '+typeStringOrNone(literalType)+' ('+intToStr(reusableIndex)+') ! Abort.');
          stream^.logWrongTypeError;
          exit(newVoidLiteral);
        end;
      end;
      if (result^.literalType<>literalType) then errorOrException('Deserializaion result has other type ('+typeStringOrNone(result^.literalType)+') than expected ('+typeStringOrNone(literalType)+').');
      if customTypeName<>'' then begin
        if literalType in C_typables then begin
          if typeMap.containsKey(customTypeName,customType) then begin
            P_compoundLiteral(result)^.customType:=customType;
          end else errorOrException('Read unknown custom type '+customTypeName);
        end else errorOrException('Read invalid custom for literal of type '+C_typeInfo[literalType].name);
      end;

      if not(stream^.allOkay) then errorOrException('Unknown error during deserialization.');
      if ((literalType in [lt_string,lt_expression]) or (literalType in C_compoundTypes) or (customTypeName<>'')) and (reusableFill<2097151) then begin
        reusableLiterals[reusableFill]:=result;
        inc(reusableFill);
      end;
    end;

  begin
    getMem(reusableLiterals,sizeOf(P_literal)*2097151);
    encodingMethod:=stream^.readByte;
    case encodingMethod of
      254: result:=literalFromStream254;
      253: result:=literalFromStream253;
      252: result:=literalFromStream252;
      251: result:=literalFromStream251;
      else begin
        errorOrException('Invalid literal encoding type '+intToStr(encodingMethod));
        result:=newVoidLiteral;
      end;
    end;
    freeMem(reusableLiterals,sizeOf(P_literal)*2097151);
  end;

PROCEDURE writeLiteralToStream(CONST literalRecycler:P_literalRecycler; CONST L:P_literal; CONST stream:P_outputStreamWrapper; CONST location:T_tokenLocation; CONST adapters:P_messages);
  VAR reusableMap:specialize G_literalKeyMap<longint>;
      previousMapValueDummy:longint;
      mapEntry:T_literalKeyLiteralValueMap.CACHE_ENTRY;

  PROCEDURE writeLiteral(CONST L:P_literal);
    VAR reusableIndex:longint;
        x:P_literal;
        iter:T_arrayOfLiteral;
    FUNCTION typeByte:byte;
      begin
        result:=byte(L^.literalType)*2;
        if (L^.literalType in C_typables) and (P_typableLiteral(L)^.customType<>nil) then inc(result);
      end;

    begin
      reusableIndex:=reusableMap.get(L,2097151);
      if reusableIndex<2097151 then begin
        inc(reusableIndex,2*byte(high(T_literalType))+2);
        stream^.writeNaturalNumber(reusableIndex);
        exit;
      end;
      stream^.writeNaturalNumber(typeByte);
      if (L^.literalType in C_typables) and (P_typableLiteral(L)^.customType<>nil) then stream^.writeAnsiString(P_typableLiteral(L)^.customType^.name);
      case L^.literalType of
        lt_boolean : stream^.writeBoolean   (P_boolLiteral  (L)^.val);
        lt_smallint,lt_bigint  : P_abstractIntLiteral(L)^.writeToStream(stream);
        lt_real:   stream^.writeDouble    (P_realLiteral  (L)^.val);
        lt_string: stream^.writeAnsiString(P_stringLiteral(L)^.val);
        lt_booleanList,lt_booleanSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_collectionLiteral(L)^.tempIteratableList;
          for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then stream^.writeBoolean(P_boolLiteral(x)^.val);
        end;
        lt_intList,lt_intSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_collectionLiteral(L)^.tempIteratableList;
          for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then P_abstractIntLiteral(x)^.writeToStream(stream);
        end;
        lt_realList,lt_realSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_collectionLiteral(L)^.tempIteratableList;
          for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then stream^.writeDouble(P_realLiteral(x)^.val);
        end;
        lt_stringList,lt_stringSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_collectionLiteral(L)^.tempIteratableList;
          for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then stream^.writeAnsiString(P_stringLiteral(x)^.val);
        end;
        lt_void, lt_emptyList,lt_emptySet,lt_emptyMap: begin end; //completely defined by type
        lt_list,lt_set,
        lt_numList,lt_numSet:begin
          stream^.writeNaturalNumber(P_compoundLiteral(L)^.size);
          iter:=P_collectionLiteral(L)^.tempIteratableList;
          for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then writeLiteral(x);
        end;
        lt_map: begin
          stream^.writeNaturalNumber(P_mapLiteral(L)^.size);
          for mapEntry in P_mapLiteral(L)^.dat.keyValueList do begin
            writeLiteral(mapEntry.key);
            writeLiteral(mapEntry.value);
          end;
        end;
        lt_expression: begin
          if not(P_expressionLiteral(L)^.writeToStream(literalRecycler,location,adapters,stream)) then begin
            if adapters<>nil then adapters^.raiseSimpleError('Cannot represent '+L^.typeString+' literal in binary form!',location)
                             else raise Exception.create    ('Cannot represent '+L^.typeString+' literal in binary form!');
          end;
        end
        else begin
          if adapters<>nil then adapters^.raiseSimpleError('Cannot represent '+L^.typeString+' literal in binary form!',location)
                           else raise Exception.create    ('Cannot represent '+L^.typeString+' literal in binary form!');
        end;
      end;
      if (reusableMap.fill<2097151) and ((L^.literalType in [lt_string,lt_expression]) or (L^.literalType in C_typables)) then
        reusableMap.putNew(L,reusableMap.fill,previousMapValueDummy);
    end;

  begin
    reusableMap.create();
    stream^.writeByte(251);
    writeLiteral(L);
    reusableMap.destroy;
  end;

FUNCTION serializeToStringList(CONST L:P_literal; CONST location:T_searchTokenLocation; CONST adapters:P_messages; CONST maxLineLength:longint=128):T_arrayOfString;
  VAR indent:longint=0;
      prevLines:T_arrayOfString;
      nextLine:ansistring;
      literalRecycler:P_literalRecycler;

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
        lt_boolean,lt_smallint,lt_bigint,lt_string,lt_real,lt_void: appendPart(L^.toString);
        lt_expression: begin
          P_expressionLiteral(L)^.validateSerializability(adapters);
          if (adapters=nil) or (adapters^.continueEvaluation) then appendPart(L^.toString);
        end;
        lt_list..lt_emptyList,
        lt_set ..lt_emptySet:
        begin
          if outerIndex>0 then begin
            append(prevLines,nextLine);
            nextLine:=StringOfChar(' ',indent);
          end;
          appendPart('[');
          inc(indent);
          if L^.literalType in [lt_set..lt_emptySet,lt_map..lt_emptyMap] then begin
            sortedTemp:=P_compoundLiteral(L)^.toList(literalRecycler);
            sortedTemp^.sort;
            iter:=sortedTemp^.tempIteratableList;
          end else iter:=P_collectionLiteral(L)^.tempIteratableList;
          for k:=0 to length(iter)-1 do if (adapters=nil) or (adapters^.continueEvaluation) then begin
            ser(iter[k],k);
            if k<length(iter)-1 then appendSeparator;
          end;
          if sortedTemp<>nil then literalRecycler^.disposeLiteral(sortedTemp);
          dec(indent);
          case L^.literalType of
            lt_list..lt_emptyList: appendPart(']');
            lt_set ..lt_emptySet : appendPart('].toSet');
          end;
          if (L^.literalType in C_typables) and (P_typableLiteral(L)^.customType<>nil) then appendPart('.to'+P_typableLiteral(L)^.customType^.name);
        end;
        lt_map ..lt_emptyMap:
        begin
          if outerIndex>0 then begin
            append(prevLines,nextLine);
            nextLine:=StringOfChar(' ',indent);
          end;
          appendPart('[');
          inc(indent);
          sortedTemp:=P_compoundLiteral(L)^.toList(literalRecycler);
          sortedTemp^.sort;
          iter:=sortedTemp^.tempIteratableList;
          for k:=0 to length(iter)-1 do if (adapters=nil) or (adapters^.continueEvaluation) then begin
            ser(P_listLiteral(iter[k])^.value[0],0);
            nextLine+='=>';
            ser(P_listLiteral(iter[k])^.value[1],1);
            if k<length(iter)-1 then begin
              appendSeparator;
              append(prevLines,nextLine);
              nextLine:=StringOfChar(' ',indent);
            end;
          end;
          literalRecycler^.disposeLiteral(sortedTemp);
          dec(indent);
          appendPart('].toMap');
          if (L^.literalType in C_typables) and (P_typableLiteral(L)^.customType<>nil) then appendPart('.to'+P_typableLiteral(L)^.customType^.name);
        end;

        else if adapters<>nil then adapters^.raiseSimpleError('Literal of type '+L^.typeString+' ('+L^.toString+') cannot be serialized',location);
      end;
    end;

  begin
    new(literalRecycler,create);
    setLength(prevLines,0);
    nextLine:='';
    ser(L,0);
    if length(nextLine)>0 then append(prevLines,nextLine);
    result:=prevLines;
    dispose(literalRecycler,destroy);
  end;

FUNCTION serialize(CONST literalRecycler:P_literalRecycler; CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_messages):ansistring;
  VAR wrapper:T_outputStreamWrapper;
      stream:TStringStream;
  begin
    stream:= TStringStream.create('');
    wrapper.create(stream);
    writeLiteralToStream(literalRecycler,L,@wrapper,location,adapters);
    stream.position:=0;
    result:=stream.DataString;
    wrapper.destroy; //implicitly destroys stream
  end;

FUNCTION deserialize(CONST literalRecycler:P_literalRecycler; CONST source:ansistring; CONST location:T_tokenLocation; CONST adapters:P_messages; VAR typeMap:T_typeMap):P_literal;
  VAR wrapper:T_inputStreamWrapper;
      stream:TStringStream;
  begin
    stream:=TStringStream.create(source);
    wrapper.create(stream);
    stream.position:=0;
    result:=newLiteralFromStream(literalRecycler,@wrapper,location,adapters,typeMap);
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
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  nanLit   .create(Nan);
  infLit   .create(infinity);
  negInfLit.create(-infinity);

FINALIZATION
  boolLit[false].destroy;
  boolLit[true ].destroy;
  voidLit.destroy;
  emptyStringSingleton.destroy;
  for i:=low(intLit) to high(intLit) do intLit[i].destroy;
  for i:=0 to 255 do charLit[chr(i)].destroy;
  nanLit   .destroy;
  infLit   .destroy;
  negInfLit.destroy;
end.
