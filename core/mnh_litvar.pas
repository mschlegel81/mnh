UNIT mnh_litVar;

INTERFACE

USES mnh_constants, mnh_out_adapters, sysutils, math, myStringUtil, mnh_tokLoc;

TYPE
  T_hashInt=dword;

  PP_literal = ^P_literal;
  P_literal = ^T_literal;

  T_literal = object
  private
    numberOfReferences: longint;
    cachedStringForm: ansistring;
  public
    CONSTRUCTOR init;
    PROCEDURE rereference;
    FUNCTION unreference: longint;
    FUNCTION getReferenceCount: longint;

    DESTRUCTOR destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION toShorterString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_scalarLiteral = ^T_scalarLiteral;
  T_scalarLiteral = object(T_literal)
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION opAnd      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opOr       (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opXor      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opPlus     (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMinus    (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMult     (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opDivReal  (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opPot      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opDivInt   (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMod      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
  end;

  P_voidLiteral = ^T_voidLiteral;
  T_voidLiteral = object(T_scalarLiteral)
    CONSTRUCTOR create();
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
  end;

  P_boolLiteral = ^T_boolLiteral;
  T_boolLiteral = object(T_scalarLiteral)
  private
    val: boolean;
  public
    CONSTRUCTOR create(CONST value: boolean);
    FUNCTION value: boolean;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION opAnd      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opOr       (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opXor      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_intLiteral = ^T_intLiteral;

  T_intLiteral = object(T_scalarLiteral)
  private
    val: int64;
  public
    CONSTRUCTOR create(CONST value: int64);
    FUNCTION value: int64;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION opAnd      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opOr       (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opXor      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opPlus     (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMinus    (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMult     (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opDivReal  (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opPot      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opDivInt   (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMod      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_realLiteral = ^T_realLiteral;

  T_realLiteral = object(T_scalarLiteral)
  private
    val: T_myFloat;
  public
    CONSTRUCTOR create(CONST value: T_myFloat);
    FUNCTION value: T_myFloat;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION opPlus     (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMinus    (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMult     (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opDivReal  (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opPot      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_stringLiteral = ^T_stringLiteral;

  T_stringLiteral = object(T_scalarLiteral)
  private
    cachedHash:T_hashInt;
    val: ansistring;
  public
    CONSTRUCTOR create(CONST value: ansistring);
    FUNCTION value: ansistring;
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
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION opPlus     (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    //from T_literal:
    DESTRUCTOR destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION toShorterString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_expressionLiteral = ^T_expressionLiteral;

  T_expressionLiteral = object(T_scalarLiteral)
  private
    val: pointer;
  public
    CONSTRUCTOR create(CONST value: pointer);
    FUNCTION value: pointer;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION opAnd      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opOr       (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opXor      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opPlus     (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMinus    (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMult     (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opDivReal  (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opPot      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opDivInt   (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opMod      (CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    FUNCTION opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; virtual;
    //from T_literal:
    DESTRUCTOR destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_listLiteral = ^T_listLiteral;

  { T_listLiteral }
  T_listLiteral = object(T_literal)
  private
    element: array of P_literal;
    cachedHash: T_hashInt;
    strictType: T_literalType;
    nextAppendIsRange: boolean;
  public
    CONSTRUCTOR create;
    FUNCTION toParameterListString(CONST isFinalized: boolean): ansistring;
    FUNCTION append(CONST L: P_literal; CONST incRefs: boolean; VAR adapters:T_adapters):P_listLiteral;
    FUNCTION appendString(CONST s:ansistring):P_listLiteral;
    FUNCTION appendBool  (CONST b:boolean):P_listLiteral;
    FUNCTION appendInt   (CONST i:int64):P_listLiteral;
    FUNCTION appendReal  (CONST r:T_myFloat):P_listLiteral;
    FUNCTION appendAll(CONST L: P_listLiteral):P_listLiteral;
    PROCEDURE appendConstructing(CONST L: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters);
    PROCEDURE setRangeAppend;
    FUNCTION size: longint;
    FUNCTION head:P_literal;
    FUNCTION head(CONST headSize:longint):P_listLiteral;
    FUNCTION tail:P_listLiteral;
    FUNCTION tail(CONST headSize:longint):P_listLiteral;
    FUNCTION trailing:P_literal;
    FUNCTION trailing(CONST trailSize:longint):P_listLiteral;
    FUNCTION leading:P_listLiteral;
    FUNCTION leading(CONST trailSize:longint):P_listLiteral;

    FUNCTION value(index: longint): P_literal;
    PROCEDURE sort;
    PROCEDURE customSort(CONST leqExpression:P_expressionLiteral; VAR adapters:T_adapters);
    FUNCTION sortPerm: P_listLiteral;
    PROCEDURE unique;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION isKeyValuePair: boolean;
    FUNCTION clone:P_listLiteral;
    //from T_literal:
    DESTRUCTOR destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION listConstructorToString:ansistring;
    FUNCTION toShorterString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
  end;

  P_namedVariable=^T_namedVariable;
  T_namedVariable=object
    private
      id:ansistring;
      value:P_literal;
    public
      CONSTRUCTOR create(CONST initialId:ansistring; CONST initialValue:P_literal);
      DESTRUCTOR destroy;
      PROCEDURE setValue(CONST newValue:P_literal);
      FUNCTION mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
      FUNCTION getId:ansistring;
      FUNCTION getValue:P_literal;
      FUNCTION toString:ansistring;
  end;

  GENERIC G_literalKeyMap<VALUE_TYPE>=object
    CONST CACHE_MOD=2047;
    TYPE CACHE_ENTRY=record
           key:P_literal;
           value:VALUE_TYPE;
         end;
         KEY_VALUE_LIST=array of CACHE_ENTRY;
    VAR dat:array[0..CACHE_MOD] of KEY_VALUE_LIST;
    CONSTRUCTOR create();
    DESTRUCTOR destroy;
    FUNCTION put(CONST key:P_literal; CONST value:VALUE_TYPE):boolean;
    FUNCTION get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
    FUNCTION keyValueList:KEY_VALUE_LIST;
  end;

  T_disposeSubruleCallback = PROCEDURE(VAR p: pointer);
  T_subruleApplyOpCallback = FUNCTION(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation): pointer;
  T_pointerToStringCallback = FUNCTION(CONST p: pointer): string;
  T_evaluateCompatorCallback = FUNCTION (CONST subruleLiteral:P_expressionLiteral; CONST LHSComparand,RHScomparand:P_literal; VAR adapters:T_adapters):boolean;

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
  disposeSubruleCallback: T_disposeSubruleCallback;
  subruleToStringCallback: T_pointerToStringCallback;
  subruleApplyOpCallback: T_subruleApplyOpCallback;
  evaluateCompatorCallback: T_evaluateCompatorCallback;

PROCEDURE disposeLiteral(VAR l: P_literal); inline;
FUNCTION newBoolLiteral(CONST value: boolean): P_boolLiteral; inline;
FUNCTION newIntLiteral(CONST value: int64): P_intLiteral; inline;
FUNCTION newRealLiteral(CONST value: T_myFloat): P_realLiteral; inline;
FUNCTION newStringLiteral(CONST value: ansistring): P_stringLiteral; inline;
FUNCTION newStringLiteral(CONST value,stringForm: ansistring): P_stringLiteral; inline;
FUNCTION newExpressionLiteral(CONST value: pointer): P_expressionLiteral; inline;
FUNCTION newListLiteral: P_listLiteral; inline;
FUNCTION newOneElementListLiteral(CONST value: P_literal; CONST incRefs: boolean; VAR adapters:T_adapters): P_listLiteral; inline;
FUNCTION newErrorLiteral: P_scalarLiteral; inline;
FUNCTION newErrorLiteralRaising(CONST errorMessage: ansistring; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
FUNCTION newErrorLiteralRaising(CONST x, y: T_literalType; CONST op: T_tokenType; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
FUNCTION newVoidLiteral: P_voidLiteral; inline;
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
FUNCTION parseNumber(CONST input: ansistring; CONST offset:longint; CONST suppressOutput: boolean; OUT parsedLength: longint): P_scalarLiteral; inline;

FUNCTION mapPut(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION mapGet(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION mapDrop(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;

IMPLEMENTATION
VAR
  boolLit: array[false..true] of T_boolLiteral;
  intLit: array[-127..128] of T_intLiteral;
  errLit: T_scalarLiteral;
  voidLit: T_voidLiteral;

PROCEDURE disposeLiteral(VAR l: P_literal);
  begin
    if l = nil then begin
      writeln(stdErr, 'disposing NIL literal ?!?');
      exit;
    end;
    if l^.unreference<=0 then dispose(l, destroy);
    l:=nil;
  end;

FUNCTION newBoolLiteral(CONST value: boolean): P_boolLiteral;
  begin
    result:=@boolLit [value];
    result^.rereference;
  end;

FUNCTION newIntLiteral(CONST value: int64): P_intLiteral;
  begin
    if (value>=-127) and (value<=128) then begin
      result:=@intLit [value];
      result^.rereference;
    end else begin
      new(result, create(value));
    end;
  end;

FUNCTION newRealLiteral(CONST value: T_myFloat): P_realLiteral;
  begin
    new(result, create(value));
  end;

FUNCTION newStringLiteral(CONST value: ansistring): P_stringLiteral;
  begin
    new(result, create(value));
  end;

FUNCTION newStringLiteral(CONST value,stringForm: ansistring): P_stringLiteral; inline;
  begin
    new(result, create(value));
    result^.cachedStringForm:=stringForm;
  end;

FUNCTION newExpressionLiteral(CONST value: pointer): P_expressionLiteral;
  begin
    new(result, create(value));
  end;

FUNCTION newListLiteral: P_listLiteral;
  begin
    new(result, create);
  end;

FUNCTION newOneElementListLiteral(CONST value: P_literal; CONST incRefs: boolean; VAR adapters:T_adapters): P_listLiteral;
  begin
    result:=newListLiteral;
    result^.append(value, incRefs,adapters);
  end;

FUNCTION newErrorLiteral: P_scalarLiteral;
  begin
    result:=@errLit;
    errLit.rereference;
  end;

FUNCTION newErrorLiteralRaising(CONST errorMessage: ansistring; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    result:=@errLit;
    errLit.rereference;
    adapters.raiseError(errorMessage, tokenLocation);
  end;

FUNCTION newErrorLiteralRaising(CONST x, y: T_literalType; CONST op: T_tokenType; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    result:=@errLit;
    errLit.rereference;
    adapters.raiseError('Operator '+C_tokenString [op]+ ' is not supported for types '+C_typeString [x]+' and '+ C_typeString [y], tokenLocation);
  end;

FUNCTION newVoidLiteral: P_voidLiteral; inline;
  begin
    result:=@voidLit;
    voidLit.rereference;
  end;

FUNCTION myFloatToStr(CONST x: T_myFloat): string;
  begin
    result:=FloatToStr(x);
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
        P_realLiteral(result)^.cachedStringForm:=copy(input, offset, parsedLength);
      end else begin
        if suppressOutput then exit(nil);
        result:=newIntLiteral(StrToInt64Def(copy(input, offset, parsedLength), 0));
        P_intLiteral(result)^.cachedStringForm:=copy(input, offset, parsedLength);
      end;
    end;
  end;

//=====================================================================================================================

CONSTRUCTOR G_literalKeyMap.create();
  VAR i:longint;
  begin
    for i:=0 to CACHE_MOD do setLength(dat[i],0);
  end;

DESTRUCTOR G_literalKeyMap.destroy;
  VAR i:longint;
  begin
    for i:=0 to CACHE_MOD do setLength(dat[i],0);
  end;

FUNCTION G_literalKeyMap.put(CONST key:P_literal; CONST value:VALUE_TYPE):boolean;
  VAR binIdx:longint;
      j:longint;
  begin
    binIdx:=key^.hash and CACHE_MOD;
    j:=0;
    while (j<length(dat[binIdx])) and not(dat[binIdx,j].key^.equals(key)) do inc(j);
    if j>=length(dat[binIdx]) then begin
      setLength(dat[binIdx],j+1);
      result:=true;
      dat[binIdx,j].key:=key;
    end else result:=false;
    dat[binIdx,j].value:=value;
  end;

FUNCTION G_literalKeyMap.get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
  VAR binIdx:longint;
      j:longint;
  begin
    binIdx:=key^.hash and CACHE_MOD;
    result:=fallbackIfNotFound;
    for j:=0 to length(dat[binIdx])-1 do if dat[binIdx,j].key^.equals(key) then exit(dat[binIdx,j].value);
  end;

FUNCTION G_literalKeyMap.keyValueList:KEY_VALUE_LIST;
  VAR i,j,k:longint;
  begin
    k:=0;
    for i:=0 to CACHE_MOD do for j:=0 to length(dat[i])-1 do begin
      setLength(result,k+1);
      result[k]:=dat[i,j];
      inc(k);
    end;
  end;

//=====================================================================================================================
CONSTRUCTOR T_literal.init; begin numberOfReferences:=1; cachedStringForm:=''; end;

PROCEDURE T_literal.rereference;
  begin
    InterLockedIncrement(numberOfReferences);
  end;

FUNCTION T_literal.unreference: longint;
  begin
    InterLockedDecrement(numberOfReferences);
    result:=numberOfReferences;
  end;

FUNCTION T_literal.getReferenceCount: longint;
  begin
    result:=numberOfReferences;
  end;

//CONSTRUCTORS:=================================================================
CONSTRUCTOR T_voidLiteral.create();                              begin inherited init; end;
CONSTRUCTOR T_boolLiteral      .create(CONST value: boolean);    begin inherited init; val:=value; end;
CONSTRUCTOR T_intLiteral       .create(CONST value: int64);      begin inherited init; val:=value; end;
CONSTRUCTOR T_realLiteral      .create(CONST value: T_myFloat);  begin inherited init; val:=value; end;
CONSTRUCTOR T_stringLiteral    .create(CONST value: ansistring); begin inherited init; val:=value; cachedHash:=0; end;
CONSTRUCTOR T_expressionLiteral.create(CONST value: pointer);    begin inherited init; val:=value; end;
CONSTRUCTOR T_listLiteral.create;
  begin
    inherited init;
    cachedHash:=0;
    setLength(element, 0);
    strictType:=lt_emptyList;
    nextAppendIsRange:=false;
  end;
//=================================================================:CONSTRUCTORS
//DESTRUCTORS:==================================================================
DESTRUCTOR T_literal.destroy; begin end;

DESTRUCTOR T_stringLiteral.destroy; begin val:=''; end;

DESTRUCTOR T_expressionLiteral.destroy;
  begin
    disposeSubruleCallback(val);
  end;

DESTRUCTOR T_listLiteral.destroy;
  VAR i: longint;
  begin
    for i:=0 to length(element)-1 do if element [i]<>nil then disposeLiteral(element[i]);
    setLength(element, 0);
    strictType:=lt_emptyList;
    nextAppendIsRange:=false;
  end;
//==================================================================:DESTRUCTORS
//?.literalType:================================================================
FUNCTION T_literal          .literalType: T_literalType; begin result:=lt_error;      end;
FUNCTION T_scalarLiteral    .literalType: T_literalType; begin result:=lt_error;      end;
FUNCTION T_voidLiteral      .literalType: T_literalType; begin result:=lt_void;       end;
FUNCTION T_boolLiteral      .literalType: T_literalType; begin result:=lt_boolean;    end;
FUNCTION T_intLiteral       .literalType: T_literalType; begin result:=lt_int;        end;
FUNCTION T_realLiteral      .literalType: T_literalType; begin result:=lt_real;       end;
FUNCTION T_stringLiteral    .literalType: T_literalType; begin result:=lt_string;     end;
FUNCTION T_expressionLiteral.literalType: T_literalType; begin result:=lt_expression; end;
FUNCTION T_listLiteral      .literalType: T_literalType; begin result:=strictType; end;
//================================================================:?.literalType
//?.value:======================================================================
FUNCTION T_intLiteral       .value: int64;      begin result:=val; end;
FUNCTION T_realLiteral      .value: T_myFloat;  begin result:=val; end;
FUNCTION T_stringLiteral    .value: ansistring; begin result:=val; end;
FUNCTION T_boolLiteral      .value: boolean;    begin result:=val; end;
FUNCTION T_expressionLiteral.value: pointer;    begin result:=val; end;
FUNCTION T_listLiteral      .value(index: longint): P_literal;
  begin
    result:=element [index];
  end;
//======================================================================:?.value

FUNCTION T_listLiteral.size: longint;
  begin
    result:=length(element);
  end;

FUNCTION T_listLiteral.head:P_literal;
  begin
    if length(element)=0
    then result:=@self
    else result:=element[0];
    result^.rereference;
  end;

FUNCTION T_listLiteral.head(CONST headSize:longint):P_listLiteral;
  VAR i,iMax:longint;
  begin
    iMax:=headSize;
    if iMax>length(element) then iMax:=length(element);
    result:=newListLiteral;
    for i:=0 to iMax-1 do result^.append(element[i],true,nullAdapter);
  end;

FUNCTION T_listLiteral.tail:P_listLiteral;
  begin result:=tail(1); end;

FUNCTION T_listLiteral.tail(CONST headSize:longint):P_listLiteral;
  VAR i,iMin:longint;
  begin
    iMin:=headSize;
    if iMin>length(element) then iMin:=length(element);
    result:=newListLiteral;
    for i:=iMin to length(element)-1 do result^.append(element[i],true,nullAdapter);
  end;

FUNCTION T_listLiteral.trailing:P_literal;
  begin
    if length(element)=0
    then result:=@self
    else result:=element[length(element)-1];
    result^.rereference;
  end;

FUNCTION T_listLiteral.trailing(CONST trailSize:longint):P_listLiteral;
  begin result:=tail(length(element)-trailSize); end;

FUNCTION T_listLiteral.leading:P_listLiteral;
  begin result:=head(length(element)-1); end;

FUNCTION T_listLiteral.leading  (CONST trailSize:longint):P_listLiteral;
  begin result:=head(length(element)-trailSize); end;
//?.toString:===================================================================
FUNCTION T_literal          .toString: ansistring; begin result:='<ERR>';                      end;
FUNCTION T_voidLiteral      .toString: ansistring; begin result:=C_voidText;                   end;
FUNCTION T_boolLiteral      .toString: ansistring; begin result:=C_boolText[val];              end;
FUNCTION T_intLiteral       .toString: ansistring; begin if cachedStringForm<>'' then exit(cachedStringForm); result:=intToStr(val);                cachedStringForm:=result; end;
FUNCTION T_realLiteral      .toString: ansistring; begin if cachedStringForm<>'' then exit(cachedStringForm); result:=myFloatToStr(val);            cachedStringForm:=result; end;
FUNCTION T_stringLiteral    .toString: ansistring; begin if cachedStringForm<>'' then exit(cachedStringForm); result:=escapeString(val);            cachedStringForm:=result; end;
FUNCTION T_expressionLiteral.toString: ansistring; begin if cachedStringForm<>'' then exit(cachedStringForm); result:=subruleToStringCallback(val); cachedStringForm:=result; end;
FUNCTION T_listLiteral      .toString: ansistring;
  VAR i: longint;
  begin
    if cachedStringForm<>'' then exit(cachedStringForm);
    if length(element) = 0 then result:='[]'
    else begin
      result:='['+element[0]^.toString;
      for i:=1 to length(element)-1 do
        result:=result+','+element[i]^.toString;
      result:=result+']';
    end;
    cachedStringForm:=result;
  end;

FUNCTION T_listLiteral.listConstructorToString:ansistring;
  VAR i:longint;
  begin
    if length(element) = 0 then result:='['
    else begin
      result:='['+element[0]^.toString;
      for i:=1 to length(element)-1 do
        result:=result+','+element[i]^.toString;
      if nextAppendIsRange then result:=result+'..'
                           else result:=result+',';
    end;
  end;

//===================================================================:?.toString
FUNCTION T_listLiteral.toParameterListString(CONST isFinalized: boolean): ansistring;
  VAR
    i: longint;
  begin
    if length(element) = 0 then if isFinalized then exit('()')
                                               else exit('(');
    result:=element [0]^.toString;
    for i:=1 to length(element)-1 do result:=result+','+element [i]^.toString;
    if isFinalized then result:='('+result+')'
    else result:='('+result+',';
  end;
//?.toShorterString:============================================================
FUNCTION T_literal.toShorterString: ansistring; begin result:=toString; end;

FUNCTION T_stringLiteral.toShorterString: ansistring;
  begin
    if length(val)>13 then result:=escapeString(copy(val, 1, 5)+'...'+copy(val, length(val)-5, 5))
    else result:=toString;
  end;

FUNCTION T_listLiteral.toShorterString: ansistring;
  VAR i: longint;
  begin
    if length(element) = 0 then result:='[]'
    else if length(element)<5 then begin
      result:='['+element [0]^.toShorterString;
      for i:=1 to length(element)-1 do result:=result+','+element [i]^.toShorterString;
      result:=result+']';
    end else result:='['+element [                0]^.toShorterString+','+
                           element [                1]^.toShorterString+',...,'+
                           element [length(element)-2]^.toShorterString+','+
                           element [length(element)-1]^.toShorterString+']';
  end;
//============================================================:?.toShorterString
//?.stringForm:=================================================================
FUNCTION T_scalarLiteral.stringForm: ansistring; begin result:=toString; end;
FUNCTION T_stringLiteral.stringForm: ansistring; begin result:=val;      end;
//=================================================================:?.stringForm
//?.isInRelationTo:=============================================================
FUNCTION T_scalarLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  begin
    result:=false;
  end;

FUNCTION T_boolLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  VAR ovl: boolean;
  begin
    if other^.literalType<>lt_boolean then exit(false);
    ovl:=P_boolLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
           or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
           or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_intLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  VAR ovi: int64;
      ovr: T_myFloat;
  begin
    case other^.literalType of
      lt_int: begin
        ovi:=P_intLiteral(other)^.val;
        result:=(val=ovi) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
               or (val<ovi) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
               or (val>ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        ovr:=P_realLiteral(other)^.val;
        result:=(val=ovr) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
               or (val<ovr) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
               or (val>ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result:=false;
    end;
  end;

FUNCTION T_realLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  VAR ovi: int64;
      ovr: T_myFloat;
  begin
    case other^.literalType of
      lt_int: begin
        ovi:=P_intLiteral(other)^.val;
        result:=(val=ovi) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
               or (val<ovi) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
               or (val>ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        ovr:=P_realLiteral(other)^.val;
        result:=(val=ovr) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
               or (val<ovr) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
               or (val>ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result:=false;
    end;
  end;

FUNCTION T_stringLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  VAR ovl: ansistring;
  begin
    if other^.literalType<>lt_string then exit(false);
    ovl:=P_stringLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
         or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_expressionLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  begin
    result:=false;
  end;
//=============================================================:?.isInRelationTo
//?.negate:=====================================================================
FUNCTION T_literal.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=@self; rereference; end;
FUNCTION T_stringLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newErrorLiteralRaising('Cannot negate string.', minusLocation,adapters); end;
FUNCTION T_boolLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newErrorLiteralRaising('Cannot negate boolean.', minusLocation,adapters); end;
FUNCTION T_intLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newIntLiteral(-value); end;
FUNCTION T_realLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newRealLiteral(-value); end;
FUNCTION T_expressionLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  begin result:=newErrorLiteralRaising('Cannot negate expression. Please use "-1*..." instead.', minusLocation,adapters); end;
FUNCTION T_listLiteral.negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
  VAR res: P_listLiteral;
      i: longint;
  begin
    res:=newListLiteral;
    for i:=0 to length(element)-1 do res^.append(element [i]^.negate(minusLocation,adapters), false,adapters);
    result:=res;
  end;
//=====================================================================:?.negate
//?.operate:====================================================================
FUNCTION T_scalarLiteral.opAnd      (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorAnd      ,tokenLocation); end;
FUNCTION T_scalarLiteral.opOr       (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorOr       ,tokenLocation); end;
FUNCTION T_scalarLiteral.opXor      (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorXor      ,tokenLocation); end;
FUNCTION T_scalarLiteral.opPlus     (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorPlus     ,tokenLocation); end;
FUNCTION T_scalarLiteral.opMinus    (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorMinus    ,tokenLocation); end;
FUNCTION T_scalarLiteral.opMult     (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorMult     ,tokenLocation); end;
FUNCTION T_scalarLiteral.opDivReal  (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorDivReal  ,tokenLocation); end;
FUNCTION T_scalarLiteral.opPot      (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorPot      ,tokenLocation); end;
FUNCTION T_scalarLiteral.opDivInt   (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorDivInt   ,tokenLocation); end;
FUNCTION T_scalarLiteral.opMod      (CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorMod      ,tokenLocation); end;
FUNCTION T_scalarLiteral.opStrConcat(CONST other:P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral; begin result:=newErrorLiteral; end; //Raising(literalType,other^.literalType,tt_operatorStrConcat,tokenLocation); end;

FUNCTION T_boolLiteral.opAnd(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorAnd, other, tokenLocation));
      lt_boolean   : result:=newBoolLiteral(val and P_boolLiteral(other)^.val);
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorAnd, tokenLocation,adapters);
    end;
  end;

FUNCTION T_boolLiteral.opOr(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorOr, other, tokenLocation));
      lt_boolean   : result:=newBoolLiteral(val or P_boolLiteral(other)^.val);
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorOr, tokenLocation,adapters);
    end;
  end;

FUNCTION T_boolLiteral.opXor(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorXor, other, tokenLocation));
      lt_boolean   : result:=newBoolLiteral(val xor P_boolLiteral(other)^.val);
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorXor, tokenLocation,adapters);
    end;
  end;

FUNCTION T_boolLiteral.opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorStrConcat, other, tokenLocation));
      else           result:=newStringLiteral(stringForm+other^.stringForm);
    end;
  end;

FUNCTION T_intLiteral.opAnd(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int       : result:=newIntLiteral(val and P_intLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorAnd, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorAnd, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opOr(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int       : result:=newIntLiteral(val or P_intLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorOr, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorOr, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opXor(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int       : result:=newIntLiteral(val xor P_intLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorXor, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorXor, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opPlus(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        result:=newIntLiteral(val+P_intLiteral(other)^.val);
      lt_real:       result:=newRealLiteral(val+P_realLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPlus , other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorPlus, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opMinus(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        result:=newIntLiteral(val-P_intLiteral(other)^.val);
      lt_real:       result:=newRealLiteral(val-P_realLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMinus, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorMinus, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opMult(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        result:=newIntLiteral(val*P_intLiteral(other)^.val);
      lt_real:       result:=newRealLiteral(val*P_realLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMult, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorMult, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opDivReal(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        result:=newRealLiteral(val/P_intLiteral(other)^.val);
      lt_real:       result:=newRealLiteral(val/P_realLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivReal, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorDivReal, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opPot(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  FUNCTION pot_int_int(x, y: int64): P_scalarLiteral;
    VAR temp: int64;
        tx, rx: T_myFloat;
    begin
      if y>=0 then begin
        temp:=1;
        while y>0 do begin
          if odd(y) then temp:=temp*x;
          x:=int64(x)*int64(x);
          y:=y shr 1;
        end;
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

  begin
    case other^.literalType of
      lt_int:        result:=pot_int_int(val, P_intLiteral(other)^.val);
      lt_real:       result:=newRealLiteral(exp(ln(val)*P_realLiteral(other)^.val));
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPot, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorPot, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opDivInt(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        try
                       result:=newIntLiteral(val div P_intLiteral(other)^.val);
                     except
                       result:=newRealLiteral(Nan);
                     end;
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivInt, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorDivInt, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opMod(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        try
                       result:=newIntLiteral(val mod P_intLiteral(other)^.val)
                     except
                       result:=newRealLiteral(Nan);
                     end;
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMod, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorMod, tokenLocation,adapters);
    end;
  end;

FUNCTION T_intLiteral.opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorStrConcat, other, tokenLocation));
      else           result:=newStringLiteral(stringForm+other^.stringForm);
    end;
  end;

FUNCTION T_realLiteral.opPlus(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        result:=newRealLiteral(val+P_intLiteral (other)^.val);
      lt_real:       result:=newRealLiteral(val+P_realLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPlus, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorPlus, tokenLocation,adapters);
    end;
  end;

FUNCTION T_realLiteral.opMinus(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        result:=newRealLiteral(val-P_intLiteral (other)^.val);
      lt_real:       result:=newRealLiteral(val-P_realLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMinus, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorMinus, tokenLocation,adapters);
    end;
  end;

FUNCTION T_realLiteral.opMult(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        result:=newRealLiteral(val*P_intLiteral (other)^.val);
      lt_real:       result:=newRealLiteral(val*P_realLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMult, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorMult, tokenLocation,adapters);
    end;
  end;

FUNCTION T_realLiteral.opDivReal(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_int:        result:=newRealLiteral(val/P_intLiteral (other)^.val);
      lt_real:       result:=newRealLiteral(val/P_realLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivReal, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorDivReal, tokenLocation,adapters);
    end;
  end;

FUNCTION T_realLiteral.opPot(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
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

  begin
    case other^.literalType of
      lt_int:        result:=newRealLiteral(pot_real_int(val, P_intLiteral(other)^.val));
      lt_real:       result:=newRealLiteral(exp(ln(val)*P_realLiteral(other)^.val));
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPot, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorPot, tokenLocation,adapters);
    end;
  end;

FUNCTION T_realLiteral.opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorStrConcat, other, tokenLocation));
      else           result:=newStringLiteral(stringForm+other^.stringForm);
    end;
  end;

FUNCTION T_stringLiteral.opPlus(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_string:     result:=newStringLiteral(val+P_stringLiteral(other)^.val);
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPlus, other, tokenLocation));
      else           result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorPlus, tokenLocation,adapters);
    end;
  end;

FUNCTION T_stringLiteral.opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    case other^.literalType of
      lt_expression: result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorStrConcat, other, tokenLocation));
      else           result:=newStringLiteral(stringForm+other^.stringForm);
    end;
  end;

FUNCTION T_expressionLiteral.opAnd(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_boolean,lt_int,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorAnd, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorAnd, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opOr(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_boolean,lt_int,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorOr, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorOr, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opXor(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_boolean,lt_int,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorXor, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorXor, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opPlus(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_real,lt_int,lt_string,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPlus, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorPlus, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opMinus(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_real,lt_int,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMinus, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorMinus, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opMult(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_real,lt_int,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMult, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorMult, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opDivReal(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_real,lt_int,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivReal, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorDivReal, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opPot(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_real,lt_int,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPot, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorPot, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opDivInt(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_int,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivInt, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorDivInt, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opMod(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    if other^.literalType in [lt_int,lt_expression]
    then result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMod, other, tokenLocation))
    else result:=newErrorLiteralRaising(literalType, other^.literalType, tt_operatorMod, tokenLocation,adapters);
  end;

FUNCTION T_expressionLiteral.opStrConcat(CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
  begin
    result:=newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorStrConcat, other, tokenLocation));
  end;

//====================================================================:?.operate
//?.hash:=======================================================================
FUNCTION T_literal    .hash: T_hashInt; begin result:=$FFFFFFFF; end;
FUNCTION T_boolLiteral.hash: T_hashInt; begin result:=longint(lt_boolean); if val then inc(result); end;
FUNCTION T_intLiteral .hash: T_hashInt; begin result:=longint(lt_int) xor longint(val); end;
FUNCTION T_realLiteral.hash: T_hashInt;
  begin
    {$Q-}
    result:=0;
    move(val, result, sizeOf(result));
    result:=result xor longint(lt_real);
    {$Q+}
  end;

FUNCTION T_stringLiteral.hash: T_hashInt;
  VAR i: longint;
  begin
    if cachedHash<>0 then exit(cachedHash);
    {$Q-}
    result:=T_hashInt(lt_string)+T_hashInt(length(val));
    for i:=1 to length(val) do result:=result*31+ord(val[i]);
    cachedHash:=result;
    {$Q+}
  end;

FUNCTION T_expressionLiteral.hash: T_hashInt;
  VAR i:longint;
      s:string;
  begin
    {$Q-}
    s:= toString;
    result:=T_hashInt(lt_expression)+T_hashInt(length(s));
    for i:=1 to length(s) do result:=result*31+ord(s[i]);
    {$Q+}
  end;

FUNCTION T_listLiteral.hash: T_hashInt;
  VAR i: longint;
  begin
    if cachedHash<>0 then exit(cachedHash);
    {$Q-}
    result:=T_hashInt(lt_list)+T_hashInt(length(element));
    for i:=0 to length(element)-1 do result:=result*31+element [i]^.hash;
    cachedHash:=0;
    {$Q+}
  end;
//=======================================================================:?.hash
//?.equals:=====================================================================
FUNCTION T_literal.equals(CONST other: P_literal): boolean;
  begin result:=(@self = other);  end;

FUNCTION T_intLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other)
           or (other^.literalType = lt_int) and (P_intLiteral(other)^.value = val);
  end;

FUNCTION T_realLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other)
           or (other^.literalType = lt_real) and ((P_realLiteral(other)^.value = val)
                                               or isNan(P_realLiteral(other)^.value) and isNan(val)
                                               or isInfinite(P_realLiteral(other)^.value) and isInfinite(val));
  end;

FUNCTION T_stringLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other)
           or (other^.literalType = lt_string) and (P_stringLiteral(other)^.value = val);
  end;

FUNCTION T_expressionLiteral.equals(CONST other: P_literal): boolean;
  begin
    result:=(@self = other)
           or (other^.literalType = lt_expression) and (P_expressionLiteral(other)^.toString = toString);
  end;

FUNCTION T_listLiteral.equals(CONST other: P_literal): boolean;
  VAR i: longint;
  begin
    if (@self = other) then exit(true);
    if (other^.literalType<>literalType) or (P_listLiteral(other)^.size<>size) then exit(false);
    for i:=0 to length(element)-1 do if not (element [i]^.equals(P_listLiteral(other)^.element [i])) then exit(false);
    result:=true;
  end;
//=====================================================================:?.equals
//?.leqForSorting:==============================================================
FUNCTION T_literal.leqForSorting(CONST other: P_literal): boolean;
  begin
    result:=literalType<=other^.literalType;
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
      lt_int:  result:=val<=P_intLiteral(other)^.val;
      lt_real: result:=val<=P_realLiteral(other)^.val;
    else result:=(literalType<=other^.literalType); end;
  end;

FUNCTION T_realLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    case other^.literalType of
      lt_int:  result:=val<=P_intLiteral(other)^.val;
      lt_real: result:=val<=P_realLiteral(other)^.val;
    else result:=(literalType<=other^.literalType);  end;
  end;

FUNCTION T_stringLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    if (other^.literalType = lt_string)
    then result:=val<=P_stringLiteral(other)^.val
    else result:=(literalType<=other^.literalType);
  end;

FUNCTION T_expressionLiteral.leqForSorting(CONST other: P_literal): boolean;
  begin
    if (other^.literalType = lt_expression)
    then result:=toString<=other^.toString
    else result:=literalType<=other^.literalType;
  end;


FUNCTION T_listLiteral.leqForSorting(CONST other: P_literal): boolean;
  VAR i: longint;
  begin
    if (other^.literalType in C_validListTypes) then
      begin
      if length(element)<length(P_listLiteral(other)^.element) then exit(true)
      else if length(element)>length(P_listLiteral(other)^.element) then exit(false)
      else for i:=0 to length(element)-1 do if element [i]^.leqForSorting(P_listLiteral(other)^.element [i]) then
            begin
            if not(P_listLiteral(other)^.element [i]^.leqForSorting(element [i])) then exit(true);
            end
          else exit(false);
      exit(true);
      end
    else result:=literalType<=other^.literalType;
  end;

//?.leqForSorting:==============================================================
FUNCTION T_stringLiteral.softCast: P_scalarLiteral;
  VAR
    len: longint;
    otherVal: ansistring;
  begin
    if lowercase(val) = C_boolText [false] then
      exit(newBoolLiteral(false));
    if lowercase(val) = C_boolText [true] then
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
    rs:=uppercase(val);
    if rs = val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.lower: P_stringLiteral;
  VAR rs: string;
  begin
    rs:=lowercase(val);
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
    result:=newStringLiteral(escapeString(val));
  end;

PROCEDURE T_stringLiteral.append(CONST suffix:ansistring);
  begin
    val:=val+suffix;
  end;

FUNCTION T_listLiteral.append(CONST L: P_literal; CONST incRefs: boolean; VAR adapters:T_adapters):P_listLiteral;
  begin
    result:=@self;
    if L = nil then begin
      adapters.raiseError('Trying to append NIL literal to list', C_nilTokenLocation);
      exit;
    end;
    if L^.literalType=lt_void then exit;
    cachedHash:=0;
    cachedStringForm:='';
    setLength(element, length(element)+1);
    element[length(element)-1]:=L;
    if incRefs then L^.rereference;
    case strictType of
      lt_list: if L^.literalType in [lt_error,lt_listWithError,lt_void] then strictType:=lt_listWithError;
      lt_booleanList  : case L^.literalType of
  	                  lt_boolean: begin end;
  			  lt_error,lt_listWithError,lt_void : strictType:=lt_listWithError;
  			  lt_list..lt_flatList,lt_expression: strictType:=lt_list;
  	                  else                                strictType:=lt_flatList;
                        end;
      lt_intList      : case L^.literalType of
                          lt_int: begin end;
  	                  lt_error,lt_listWithError,lt_void : strictType:=lt_listWithError;
  			  lt_list..lt_flatList,lt_expression: strictType:=lt_list;
                          lt_real:                            strictType:=lt_numList;
  			  else                                strictType:=lt_flatList;
  	                end;
      lt_realList     : case L^.literalType of
  	                  lt_real: begin end;
  			  lt_error,lt_listWithError,lt_void : strictType:=lt_listWithError;
  			  lt_list..lt_flatList,lt_expression: strictType:=lt_list;
  	                  lt_int:                             strictType:=lt_numList;
  			  else                                strictType:=lt_flatList;
                        end;
      lt_numList      : case L^.literalType of
                          lt_int,lt_real: begin end;
  	                  lt_error,lt_listWithError,lt_void : strictType:=lt_listWithError;
  	                  lt_list..lt_flatList,lt_expression: strictType:=lt_list;
  	                  else                                strictType:=lt_flatList;
  	                end;
      lt_stringList   : case L^.literalType of
	                  lt_string: begin end;
  	                  lt_error,lt_listWithError,lt_void : strictType:=lt_listWithError;
  	                  lt_list..lt_flatList,lt_expression: strictType:=lt_list;
  	                  else                                strictType:=lt_flatList;
  	                end;
      lt_emptyList    : case L^.literalType of
  	                  lt_error,lt_listWithError,lt_void: strictType:=lt_listWithError;
                          lt_boolean:                        strictType:=lt_booleanList;
                          lt_int:                            strictType:=lt_intList;
                          lt_real:                           strictType:=lt_realList;
                          lt_string:                         strictType:=lt_stringList;
                          lt_expression:                     strictType:=lt_list;
                          lt_list..lt_flatList: if P_listLiteral(L)^.isKeyValuePair
                                                then strictType:=lt_keyValueList
                                                else strictType:=lt_list;
                        end;
      lt_keyValueList: if not((L^.literalType in C_validListTypes) and (P_listLiteral(L)^.isKeyValuePair)) then strictType:=lt_list;
      lt_flatList:     case L^.literalType of
  	                 lt_error,lt_listWithError,lt_void: strictType:=lt_listWithError;
  	                 lt_list..lt_flatList:              strictType:=lt_list;
                       end;
    end;
  end;

FUNCTION T_listLiteral.appendString(CONST s: ansistring):P_listLiteral;
  begin
    result:=append(newStringLiteral(s),false,nullAdapter);
  end;

FUNCTION T_listLiteral.appendBool(CONST b: boolean):P_listLiteral;
  begin
    result:=append(newBoolLiteral(b),false,nullAdapter);
  end;

FUNCTION T_listLiteral.appendInt(CONST i: int64):P_listLiteral;
  begin
    result:=append(newIntLiteral(i),false,nullAdapter);
  end;

FUNCTION T_listLiteral.appendReal(CONST r: T_myFloat):P_listLiteral;
  begin
    result:=append(newRealLiteral(r),false,nullAdapter);
  end;

FUNCTION T_listLiteral.appendAll(CONST L: P_listLiteral):P_listLiteral;
  VAR i: longint;
  begin
    for i:=0 to length(L^.element)-1 do append(L^.element [i], true,nullAdapter);
    result:=@self;
  end;

PROCEDURE T_listLiteral.appendConstructing(CONST L: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters);
  VAR
    last: P_literal;
    i0, i1: int64;
    c0, c1: char;
  begin
    if not (nextAppendIsRange) then begin
      append(L, true,adapters);
      exit;
    end;
    nextAppendIsRange:=false;

    if length(element) = 0 then begin
      strictType:=lt_listWithError;
      adapters.raiseError('Cannot append range to empty list', tokenLocation);
      exit;
    end;
    last:=element [length(element)-1];
    if (last^.literalType = lt_int) and (L^.literalType = lt_int) then
      begin
      i0:=P_intLiteral(last)^.val;
      i1:=P_intLiteral(L)^.val;
      while (i0<i1) and adapters.noErrors do
        begin
        inc(i0);
        appendInt(i0);
        end;
      while (i0>i1) and adapters.noErrors do
        begin
        dec(i0);
        appendInt(i0);
        end;
      end
    else if (last^.literalType = lt_string) and
      (length(P_stringLiteral(last)^.val) = 1) and (L^.literalType = lt_string) and
      (length(P_stringLiteral(L)^.val) = 1) then
      begin
      c0:=P_stringLiteral(last)^.val [1];
      c1:=P_stringLiteral(L)^.val [1];
      while c0<c1 do
        begin
        inc(c0);
        appendString(c0);
        end;
      while c0>c1 do
        begin
        dec(c0);
        appendString(c0);
        end;
      end
    else begin
      strictType:=lt_listWithError;
      adapters.raiseError('Invalid range expression '+
        last^.toString+'..'+L^.toString, tokenLocation);
    end;
  end;

PROCEDURE T_listLiteral.setRangeAppend;
  begin
    nextAppendIsRange:=true;
  end;

PROCEDURE T_listLiteral.sort;
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  begin
    if (length(element)<=1) then exit;
    cachedHash:=0;
    cachedStringForm:='';
    scale:=1;
    setLength(temp, length(element));
    while scale<length(element) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<length(element) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do
          if element [j0]^.leqForSorting(element [j1])  then begin temp[k]:=element [j0]; inc(k); inc(j0); end
                                                        else begin temp[k]:=element [j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<length(element)) do begin temp[k]:=element [j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(element)) do begin temp[k]:=element [j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<length(element)) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while i<length(element) do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do
            if temp [j0]^.leqForSorting(temp [j1])        then begin element[k]:=temp [j0]; inc(k); inc(j0); end
                                                          else begin element[k]:=temp [j1]; inc(k); inc(j1); end;
          while (j0<i+scale) and (j0<length(element))       do begin element[k]:=temp [j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(element)) do begin element[k]:=temp [j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to length(element)-1 do element[k]:=temp [k];
    end;
    setLength(temp, 0);
  end;

PROCEDURE T_listLiteral.customSort(CONST leqExpression:P_expressionLiteral; VAR adapters:T_adapters);
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(a,b:P_literal):boolean; inline; begin result:=evaluateCompatorCallback(leqExpression,a,b,adapters); end;

  begin
    if length(element)<=1 then exit;
    cachedHash:=0;
    cachedStringForm:='';
    scale:=1;
    setLength(temp, length(element));
    while (scale<length(element)) and adapters.noErrors do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while (i<length(element)) and adapters.noErrors do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do
          if isLeq(element [j0],element [j1])           then begin temp[k]:=element [j0]; inc(k); inc(j0); end
                                                        else begin temp[k]:=element [j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<length(element)) do begin temp[k]:=element [j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(element)) do begin temp[k]:=element [j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      if not(adapters.noErrors) then exit;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<length(element)) and adapters.noErrors then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while (i<length(element)) and adapters.noErrors do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do
            if isLeq(temp [j0],temp [j1])                 then begin element[k]:=temp [j0]; inc(k); inc(j0); end
                                                          else begin element[k]:=temp [j1]; inc(k); inc(j1); end;
          while (j0<i+scale) and (j0<length(element))       do begin element[k]:=temp [j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(element)) do begin element[k]:=temp [j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to length(element)-1 do element[k]:=temp [k];
    end;
    setLength(temp, 0);
  end;

FUNCTION T_listLiteral.sortPerm: P_listLiteral;
  VAR
    temp1, temp2: array of record  v: P_literal;
      index: longint;
      end;
    scale: longint;
    i, j0, j1, k: longint;

  begin
    if length(element) = 0 then exit(newListLiteral);

    setLength(temp1, length(element));
    setLength(temp2, length(element));
    for i:=0 to length(element)-1 do with temp1 [i] do begin
      v:=P_scalarLiteral(element [i]);
      index:=i;
    end;
    scale:=1;
    while scale<length(temp1) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<length(temp1) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do
          if temp1 [j0].v^.leqForSorting(temp1 [j1].v) then begin temp2[k]:=temp1 [j0]; inc(k); inc(j0); end
                                                       else begin temp2[k]:=temp1 [j1]; inc(k); inc(j1); end;
        while (j0<i+scale) and (j0<length(temp1))        do begin temp2[k]:=temp1 [j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(temp1))  do begin temp2[k]:=temp1 [j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<length(temp1)) then begin
        i:=0;
        while i<length(temp1) do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do
            if temp2 [j0].v^.leqForSorting(temp2 [j1].v) then begin temp1[k]:=temp2 [j0]; inc(k); inc(j0); end
                                                         else begin temp1[k]:=temp2 [j1]; inc(k); inc(j1); end;
          while (j0<i+scale)  and (j0<length(temp1))       do begin temp1[k]:=temp2 [j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(temp1))  do begin temp1[k]:=temp2 [j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to length(temp1)-1 do temp1[k]:=temp2 [k];
    end;
    setLength(temp2, 0);
    result:=newListLiteral;
    for i:=0 to length(temp1)-1 do result^.appendInt(temp1 [i].index);
    setLength(temp1, 0);
  end;

PROCEDURE T_listLiteral.unique;
  VAR i, j: longint;
  begin
    if (length(element)<=1) then exit;
    sort;
    i:=0;
    for j:=1 to length(element)-1 do
    if (element[i]^.equals(element[j])) then disposeLiteral(element [j])
    else begin
      inc(i);
      element[i]:=element [j];
    end;
    setLength(element, i+1);
  end;

FUNCTION T_listLiteral.isKeyValuePair: boolean;
  begin
    result:=(length(element)=2)
        and (element[0]^.literalType=lt_string);
  end;

FUNCTION T_listLiteral.clone:P_listLiteral;
  VAR i:longint;
  begin
    result:=newListLiteral;
    setLength(result^.element,length(element));
    for i:=0 to length(element)-1 do begin
      result^.element[i]:=element[i];
      element[i]^.rereference;
    end;
    result^.strictType:=strictType;
    result^.nextAppendIsRange:=nextAppendIsRange;
    result^.cachedHash:=cachedHash;
    result^.cachedStringForm:=cachedStringForm;
  end;

FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;

  FUNCTION equals(CONST LHS, RHS: P_literal): boolean;
    VAR i: longint;
    begin
      if LHS = RHS then exit(true);
      case LHS^.literalType of
        lt_int, lt_real, lt_boolean, lt_string, lt_expression:
          if RHS^.literalType in [lt_int, lt_real, lt_boolean, lt_string, lt_expression]
          then exit(P_scalarLiteral(LHS)^.isInRelationTo(tt_comparatorEq,P_scalarLiteral(RHS)))
          else exit(false);
        lt_list..lt_flatList:
          if (RHS^.literalType in C_validListTypes) and (length(P_listLiteral(LHS)^.element) =length(P_listLiteral(RHS)^.element))
          then begin
            result:=true;
            i:=0;
            while result and (i<length(P_listLiteral(LHS)^.element)) do begin
              result:=result and equals(P_listLiteral(LHS)^.element [i],
                P_listLiteral(RHS)^.element [i]);
              inc(i);
            end;
          end else exit(false);
        else exit(false);
      end;
    end;

  FUNCTION isContained(CONST LHS, RHS: P_literal): boolean;
    VAR i: longint;
    begin
      result:=false;
      if RHS^.literalType in C_validListTypes then begin
        i:=0;
        while (i<length(P_listLiteral(RHS)^.element)) and not (result) do begin
          result:=result or equals(LHS, P_listLiteral(RHS)^.element [i]);
          inc(i);
        end;
      end;
    end;

  FUNCTION areInRelEqual(CONST LHS,RHS:P_literal):boolean; inline;
    begin
      case LHS^.literalType of
        lt_boolean: result:=(RHS^.literalType=lt_boolean) and (P_boolLiteral  (LHS)^.val=P_boolLiteral  (RHS)^.val);
        lt_int:     result:=(RHS^.literalType=lt_int)     and (P_intLiteral   (LHS)^.val=P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_intLiteral   (LHS)^.val=P_realLiteral  (RHS)^.val);
        lt_real:    result:=(RHS^.literalType=lt_int)     and (P_realLiteral  (LHS)^.val=P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_realLiteral  (LHS)^.val=P_realLiteral  (RHS)^.val);
        lt_string:  result:=(RHS^.literalType=lt_string)  and (P_stringLiteral(LHS)^.val=P_stringLiteral(RHS)^.val);
      end;
    end;

  FUNCTION areInRelNeq(CONST LHS,RHS:P_literal):boolean; inline;
    begin
      case LHS^.literalType of
        lt_boolean: result:=(RHS^.literalType=lt_boolean) and (P_boolLiteral  (LHS)^.val<>P_boolLiteral  (RHS)^.val);
        lt_int:     result:=(RHS^.literalType=lt_int)     and (P_intLiteral   (LHS)^.val<>P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_intLiteral   (LHS)^.val<>P_realLiteral  (RHS)^.val);
        lt_real:    result:=(RHS^.literalType=lt_int)     and (P_realLiteral  (LHS)^.val<>P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_realLiteral  (LHS)^.val<>P_realLiteral  (RHS)^.val);
        lt_string:  result:=(RHS^.literalType=lt_string)  and (P_stringLiteral(LHS)^.val<>P_stringLiteral(RHS)^.val);
      end;
    end;

  FUNCTION areInRelGeq(CONST LHS,RHS:P_literal):boolean; inline;
    begin
      case LHS^.literalType of
        lt_boolean: result:=(RHS^.literalType=lt_boolean) and (P_boolLiteral  (LHS)^.val>=P_boolLiteral  (RHS)^.val);
        lt_int:     result:=(RHS^.literalType=lt_int)     and (P_intLiteral   (LHS)^.val>=P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_intLiteral   (LHS)^.val>=P_realLiteral  (RHS)^.val);
        lt_real:    result:=(RHS^.literalType=lt_int)     and (P_realLiteral  (LHS)^.val>=P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_realLiteral  (LHS)^.val>=P_realLiteral  (RHS)^.val);
        lt_string:  result:=(RHS^.literalType=lt_string)  and (P_stringLiteral(LHS)^.val>=P_stringLiteral(RHS)^.val);
      end;
    end;

  FUNCTION areInRelLeq(CONST LHS,RHS:P_literal):boolean; inline;
    begin
      case LHS^.literalType of
        lt_boolean: result:=(RHS^.literalType=lt_boolean) and (P_boolLiteral  (LHS)^.val<=P_boolLiteral  (RHS)^.val);
        lt_int:     result:=(RHS^.literalType=lt_int)     and (P_intLiteral   (LHS)^.val<=P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_intLiteral   (LHS)^.val<=P_realLiteral  (RHS)^.val);
        lt_real:    result:=(RHS^.literalType=lt_int)     and (P_realLiteral  (LHS)^.val<=P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_realLiteral  (LHS)^.val<=P_realLiteral  (RHS)^.val);
        lt_string:  result:=(RHS^.literalType=lt_string)  and (P_stringLiteral(LHS)^.val<=P_stringLiteral(RHS)^.val);
      end;
    end;

  FUNCTION areInRelLesser(CONST LHS,RHS:P_literal):boolean; inline;
    begin
      case LHS^.literalType of
        lt_boolean: result:=(RHS^.literalType=lt_boolean) and (P_boolLiteral  (LHS)^.val<P_boolLiteral  (RHS)^.val);
        lt_int:     result:=(RHS^.literalType=lt_int)     and (P_intLiteral   (LHS)^.val<P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_intLiteral   (LHS)^.val<P_realLiteral  (RHS)^.val);
        lt_real:    result:=(RHS^.literalType=lt_int)     and (P_realLiteral  (LHS)^.val<P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_realLiteral  (LHS)^.val<P_realLiteral  (RHS)^.val);
        lt_string:  result:=(RHS^.literalType=lt_string)  and (P_stringLiteral(LHS)^.val<P_stringLiteral(RHS)^.val);
      end;
    end;

  FUNCTION areInRelGreater(CONST LHS,RHS:P_literal):boolean; inline;
    begin
      case LHS^.literalType of
        lt_boolean: result:=(RHS^.literalType=lt_boolean) and (P_boolLiteral  (LHS)^.val>P_boolLiteral  (RHS)^.val);
        lt_int:     result:=(RHS^.literalType=lt_int)     and (P_intLiteral   (LHS)^.val>P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_intLiteral   (LHS)^.val>P_realLiteral  (RHS)^.val);
        lt_real:    result:=(RHS^.literalType=lt_int)     and (P_realLiteral  (LHS)^.val>P_intLiteral   (RHS)^.val) or
                            (RHS^.literalType=lt_real)    and (P_realLiteral  (LHS)^.val>P_realLiteral  (RHS)^.val);
        lt_string:  result:=(RHS^.literalType=lt_string)  and (P_stringLiteral(LHS)^.val>P_stringLiteral(RHS)^.val);
      end;
    end;

  {$MACRO ON}
  {$define checkedExit:=
    if result^.literalType = lt_listWithError then begin
      disposeLiteral(result);
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType,op, tokenLocation,adapters);
    end;
    exit(result)}
  {$define invalidLengthExit:=exit(newErrorLiteralRaising('Invalid list lengths '+intToStr(i)+' and '+intToStr(i1)+' given for operator '+C_tokenString [op], tokenLocation,adapters))}

  VAR i, i1, j: longint;
      key: ansistring;
  begin
    //HANDLE S x S -> S OPERATORS:---------------------------------------------
    case op of
      tt_comparatorEq,tt_comparatorNeq,tt_comparatorLeq,tt_comparatorGeq,tt_comparatorLss,tt_comparatorGrt,
      tt_operatorAnd, tt_operatorOr, tt_operatorXor,
      tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal,
      tt_operatorDivInt, tt_operatorMod, tt_operatorPot, tt_operatorStrConcat:
      case LHS^.literalType of
        lt_boolean, lt_int, lt_real, lt_string:
          case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string:
              case op of
                tt_comparatorEq:      exit(newBoolLiteral(areInRelEqual  (LHS,RHS)));
                tt_comparatorNeq:     exit(newBoolLiteral(areInRelNeq    (LHS,RHS)));
                tt_comparatorLeq:     exit(newBoolLiteral(areInRelLeq    (LHS,RHS)));
                tt_comparatorGeq:     exit(newBoolLiteral(areInRelGeq    (LHS,RHS)));
                tt_comparatorLss:     exit(newBoolLiteral(areInRelLesser (LHS,RHS)));
                tt_comparatorGrt:     exit(newBoolLiteral(areInRelGreater(LHS,RHS)));
                tt_operatorAnd:       exit(P_scalarLiteral(LHS)^.opAnd      (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorOr:        exit(P_scalarLiteral(LHS)^.opOr       (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorXor:       exit(P_scalarLiteral(LHS)^.opXor      (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorPlus:      exit(P_scalarLiteral(LHS)^.opPlus     (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorMinus:     exit(P_scalarLiteral(LHS)^.opMinus    (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorMult:      exit(P_scalarLiteral(LHS)^.opMult     (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorDivReal:   exit(P_scalarLiteral(LHS)^.opDivReal  (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorDivInt:    exit(P_scalarLiteral(LHS)^.opDivInt   (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorMod:       exit(P_scalarLiteral(LHS)^.opMod      (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorPot:       exit(P_scalarLiteral(LHS)^.opPot      (P_scalarLiteral(RHS),tokenLocation,adapters));
                tt_operatorStrConcat: exit(P_scalarLiteral(LHS)^.opStrConcat(P_scalarLiteral(RHS),tokenLocation,adapters));
              end;
            //scalar X scalar
            lt_list,lt_keyValueList: begin
              //scalar X nested list
              result:=newListLiteral;
              for i:=0 to length(P_listLiteral(RHS)^.element)-1 do
                P_listLiteral(result)^.append(
                  resolveOperator(LHS, op, P_listLiteral(RHS)^.element[i],
                  tokenLocation,adapters),
                  false,adapters);
              checkedExit;
            end;
            lt_booleanList..lt_emptyList,lt_flatList: begin
              //scalar X flat list
              result:=newListLiteral;
              case op of
                tt_comparatorEq:       for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelEqual  (LHS,P_listLiteral(RHS)^.element[i]));
                tt_comparatorNeq:      for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelNeq    (LHS,P_listLiteral(RHS)^.element[i]));
                tt_comparatorLeq:      for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelLeq    (LHS,P_listLiteral(RHS)^.element[i]));
                tt_comparatorGeq:      for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelGeq    (LHS,P_listLiteral(RHS)^.element[i]));
                tt_comparatorLss:      for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelLesser (LHS,P_listLiteral(RHS)^.element[i]));
                tt_comparatorGrt:      for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelGreater(LHS,P_listLiteral(RHS)^.element[i]));
                tt_operatorAnd:        for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opAnd      (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorOr:         for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opOr       (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorXor:        for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opXor      (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorPlus:       for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opPlus     (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorMinus:      for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opMinus    (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorMult:       for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opMult     (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorDivReal:    for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opDivReal  (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorDivInt:     for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opDivInt   (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorMod:        for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opMod      (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorPot:        for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opPot      (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                tt_operatorStrConcat:  for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.opStrConcat(P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
              end;
              checkedExit;
            end;
          end;
          lt_list,lt_keyValueList: case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string: begin
              //nested list X scalar
              result:=newListLiteral;
              for i:=0 to length(P_listLiteral(LHS)^.element)-1 do
                P_listLiteral(result)^.append(
                  resolveOperator(P_listLiteral(LHS)^.element [i], op,
                  RHS, tokenLocation,adapters),
                  false,adapters);
              checkedExit;
            end;
            lt_list..lt_flatList: begin
              //nested list X flat/nested list
              i :=length(P_listLiteral(LHS)^.element);
              i1:=length(P_listLiteral(RHS)^.element);
              if i = i1 then begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.element [i], op,
                    P_listLiteral(RHS)^.element [i], tokenLocation,adapters),
                    false,adapters);
                checkedExit;
              end else invalidLengthExit;
            end;
          end;
          lt_booleanList..lt_emptyList,lt_flatList: case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string: begin
              //flat list X scalar
              result:=newListLiteral;
              case op of
                tt_comparatorEq:       for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelEqual  (P_listLiteral(LHS)^.element [i],RHS));
                tt_comparatorNeq:      for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelNeq    (P_listLiteral(LHS)^.element [i],RHS));
                tt_comparatorLeq:      for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelLeq    (P_listLiteral(LHS)^.element [i],RHS));
                tt_comparatorGeq:      for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelGeq    (P_listLiteral(LHS)^.element [i],RHS));
                tt_comparatorLss:      for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelLesser (P_listLiteral(LHS)^.element [i],RHS));
                tt_comparatorGrt:      for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.appendBool(areInRelGreater(P_listLiteral(LHS)^.element [i],RHS));
                tt_operatorAnd:        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opAnd      (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorOr:         for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opOr       (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorXor:        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opXor      (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorPlus:       for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opPlus     (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorMinus:      for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opMinus    (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorMult:       for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opMult     (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorDivReal:    for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opDivReal  (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorDivInt:     for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opDivInt   (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorMod:        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opMod      (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorPot:        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opPot      (P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
                tt_operatorStrConcat:  for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opStrConcat(P_scalarLiteral(RHS),tokenLocation,adapters),false,adapters);
              end;
              checkedExit;
            end;
            lt_list,lt_keyValueList: begin
              //flat list X nested list
              i:=length(P_listLiteral(LHS)^.element);
              i1:=length(P_listLiteral(RHS)^.element);
              if i = i1 then begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.element [i], op,
                    P_listLiteral(RHS)^.element [i], tokenLocation,adapters),
                    false,adapters);
                checkedExit;
              end else invalidLengthExit;
            end;
            lt_booleanList..lt_emptyList,lt_flatList: begin
              //flat list X flat list
              i:=length(P_listLiteral(LHS)^.element);
              i1:=length(P_listLiteral(RHS)^.element);
              if i = i1 then begin
                result:=newListLiteral;
                case op of
                  tt_comparatorEq:       for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelEqual  (P_listLiteral(LHS)^.element[i],P_listLiteral(RHS)^.element[i]));
                  tt_comparatorNeq:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelNeq    (P_listLiteral(LHS)^.element[i],P_listLiteral(RHS)^.element[i]));
                  tt_comparatorLeq:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelLeq    (P_listLiteral(LHS)^.element[i],P_listLiteral(RHS)^.element[i]));
                  tt_comparatorGeq:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelGeq    (P_listLiteral(LHS)^.element[i],P_listLiteral(RHS)^.element[i]));
                  tt_comparatorLss:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelLesser (P_listLiteral(LHS)^.element[i],P_listLiteral(RHS)^.element[i]));
                  tt_comparatorGrt:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelGreater(P_listLiteral(LHS)^.element[i],P_listLiteral(RHS)^.element[i]));
                  tt_operatorAnd:        for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opAnd      (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorOr:         for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opOr       (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorXor:        for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opXor      (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorPlus:       for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opPlus     (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorMinus:      for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opMinus    (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorMult:       for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opMult     (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorDivReal:    for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opDivReal  (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorDivInt:     for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opDivInt   (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorMod:        for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opMod      (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorPot:        for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opPot      (P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                  tt_operatorStrConcat:  for i:=0 to i1-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.opStrConcat(P_scalarLiteral(P_listLiteral(RHS)^.element [i]),tokenLocation,adapters),false,adapters);
                end;
                checkedExit;
              end else invalidLengthExit;
            end;
          end;
      end;
      tt_operatorConcat: begin
        result:=newListLiteral;
        if (LHS^.literalType in C_validScalarTypes)
        then P_listLiteral(result)^.append   (LHS,true,adapters)
        else P_listLiteral(result)^.appendAll(P_listLiteral(LHS));
        if (RHS^.literalType in C_validScalarTypes)
        then P_listLiteral(result)^.append   (RHS,true,adapters)
        else P_listLiteral(result)^.appendAll(P_listLiteral(RHS));
        checkedExit;
      end;
      tt_comparatorListEq: exit(newBoolLiteral(equals(LHS, RHS)));
      tt_operatorIn: exit(newBoolLiteral(isContained(LHS, RHS)));
      tt_operatorExtractL0: if LHS^.literalType in C_validListTypes then case RHS^.literalType of
        lt_int: begin
          i1:=length(P_listLiteral(LHS)^.element);
          i:=P_intLiteral(RHS)^.val;
          if (i>=0) and (i<i1) then begin
            result:=P_listLiteral(LHS)^.element [i];
            result^.rereference;
            checkedExit;
          end else exit(newListLiteral);
        end;
        lt_intList: begin
          result:=newListLiteral;
          i1:=length(P_listLiteral(LHS)^.element);
          for j:=0 to length(P_listLiteral(RHS)^.element)-1 do begin
            i:=P_intLiteral(P_listLiteral(RHS)^.element [j])^.val;
            if (i>=0) and (i<i1) then P_listLiteral(result)^.append(P_listLiteral(LHS)^.element [i], true,adapters);
          end;
          checkedExit;
        end;
        lt_booleanList: begin
          result:=newListLiteral;
          i1:=length(P_listLiteral(LHS)^.element);
          if i1 = length(P_listLiteral(RHS)^.element) then
          for i:=0 to length(P_listLiteral(RHS)^.element)-1 do
          if P_boolLiteral(P_listLiteral(RHS)^.element [i])^.val then
            P_listLiteral(result)^.append(P_listLiteral(LHS)^.element [i], true,adapters);
          checkedExit;
        end;
        lt_string: if LHS^.literalType in [lt_keyValueList,lt_emptyList] then begin
          key:=P_stringLiteral(RHS)^.val;
          for i:=0 to length(P_listLiteral(LHS)^.element)-1 do
          if P_stringLiteral(P_listLiteral(P_listLiteral(LHS)^.element [i])^.element [0])^.val = key then begin
            result:=P_listLiteral(P_listLiteral(LHS)^.element [i])^.element [1];
            result^.rereference;
            checkedExit;
          end;
          exit(newListLiteral);
        end else exit(newErrorLiteralRaising('Operator % with a string as second operand can only be applied to key-value-lists!', tokenLocation,adapters));
        lt_stringList: if LHS^.literalType in [lt_keyValueList,lt_emptyList] then begin
          result:=newListLiteral;
          for j:=0 to P_listLiteral(RHS)^.size-1 do begin
            key:=P_stringLiteral(P_listLiteral(RHS)^.element [j])^.value;
            i:=0; while i<length(P_listLiteral(LHS)^.element) do
            if P_stringLiteral(P_listLiteral(P_listLiteral(LHS)^.element [i])^.element [0])^.value = key then begin
              P_listLiteral(result)^.append(P_listLiteral(P_listLiteral(LHS)^.element [i])^.element [1], true,adapters);
              i:=length(P_listLiteral(LHS)^.element);
            end else inc(i);
          end;
          checkedExit;
        end else exit(newErrorLiteralRaising('Operator % with a stringList as second operand can only be applied to key-value-lists!', tokenLocation,adapters));
        lt_emptyList: exit(newListLiteral);
      end;
      tt_operatorExtractL1: if LHS^.literalType in C_validListTypes then begin
        result:=newListLiteral;
        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do
          P_listLiteral(result)^.append(resolveOperator(
            P_listLiteral(LHS)^.element [i], tt_operatorExtractL0,
            RHS, tokenLocation,adapters), false,adapters);
        checkedExit;
      end;
      tt_operatorExtractL2: if LHS^.literalType in C_validListTypes then begin
        result:=newListLiteral;
        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do
          P_listLiteral(result)^.append(resolveOperator(
            P_listLiteral(LHS)^.element [i], tt_operatorExtractL1,
            RHS, tokenLocation,adapters), false,adapters);
        checkedExit;
      end;
      tt_operatorExtractL3: if LHS^.literalType in C_validListTypes then begin
        result:=newListLiteral;
        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do
          P_listLiteral(result)^.append(resolveOperator(
            P_listLiteral(LHS)^.element [i], tt_operatorExtractL2,
            RHS, tokenLocation,adapters), false,adapters);
        checkedExit;
      end;
    end;
    //---------------------------------------------:HANDLE S x S -> S OPERATORS
    //HANDLE ERROR, VOID AND EXPRESSION LITERALS:-------------------------------
    case LHS^.literalType of
      lt_void:                   begin RHS^.rereference; exit(RHS); end;
      lt_error,lt_listWithError: begin LHS^.rereference; exit(LHS); end;
      lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, op, RHS, tokenLocation)));
    end;
    case RHS^.literalType of
      lt_void:                   begin LHS^.rereference; exit(LHS); end;
      lt_error,lt_listWithError: begin RHS^.rereference; exit(RHS); end;
      lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, op, RHS, tokenLocation)));
    end;
    //-------------------------------:HANDLE ERROR, VOID AND EXPRESSION LITERALS
    result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, op, tokenLocation,adapters);
  end;

CONSTRUCTOR T_namedVariable.create(CONST initialId:ansistring; CONST initialValue:P_literal);
  begin
    id:=initialId;
    value:=initialValue;
    if value<>nil then value^.rereference;
  end;

DESTRUCTOR T_namedVariable.destroy;
  begin
    if value<>nil then disposeLiteral(value);
  end;

PROCEDURE T_namedVariable.setValue(CONST newValue:P_literal);
  begin
    disposeLiteral(value);
    value:=newValue;
    value^.rereference;
  end;

FUNCTION T_namedVariable.mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  CONST MAPPED_OP:array[tt_cso_assignPlus..tt_cso_assignDiv] of T_tokenType=(tt_operatorPlus,tt_operatorMinus,tt_operatorMult,tt_operatorDivReal);
  VAR oldValue:P_literal;
  begin
    oldValue:=value;
    case mutation of
      tt_cso_assignPlus..tt_cso_assignDiv: begin
        result:=resolveOperator(oldValue, MAPPED_OP[mutation], RHS, location,adapters);
        disposeLiteral(oldValue);
        value:=result;
        result^.rereference;
      end;
      tt_cso_assignStrConcat: begin
        if (oldValue^.literalType=lt_string) and (oldValue^.getReferenceCount=1) and (RHS^.literalType in [lt_boolean..lt_string]) then begin
          P_stringLiteral(oldValue)^.append(P_scalarLiteral(RHS)^.stringForm);
          result:=oldValue;
          result^.rereference;
        end else begin
          result:=resolveOperator(oldValue, tt_operatorStrConcat, RHS, location,adapters);
          disposeLiteral(oldValue);
          value:=result;
          result^.rereference;
        end;
      end;
      tt_cso_assignAppend: begin
        if (oldValue^.literalType in C_validListTypes) and (oldValue^.getReferenceCount=1) then begin
          if (RHS^.literalType in C_validScalarTypes)
          then P_listLiteral(oldValue)^.append(RHS, true,adapters)
          else P_listLiteral(oldValue)^.appendAll(P_listLiteral(RHS));
          result:=oldValue;
          result^.rereference;
        end else begin
          result:=resolveOperator(oldValue, tt_operatorConcat   , RHS, location,adapters);
          result^.rereference;
          disposeLiteral(oldValue);
          value:=result;
        end;
      end;
    end;
  end;

FUNCTION T_namedVariable.getId:ansistring;
  begin
    result:=id;
  end;

FUNCTION T_namedVariable.getValue:P_literal;
  begin
    result:=value;
    result^.rereference;
  end;

FUNCTION T_namedVariable.toString:ansistring;
  begin
    result:=id+'='+value^.toString;
  end;

FUNCTION mapPut(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR map,keyValuePair:P_listLiteral;
      key:P_stringLiteral;
      value:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (length(params^.element)=3) and
       (params^.element[0]^.literalType in [lt_keyValueList,lt_emptyList]) and
       (params^.element[1]^.literalType=lt_string) then begin
      map:=P_listLiteral(params^.element[0]);
      if map^.numberOfReferences=1
      then map^.rereference
      else map:=map^.clone;
      key:=P_stringLiteral(params^.element[1]);
      value:=params^.element[2];
      for i:=0 to length(map^.element)-1 do begin
        keyValuePair:=P_listLiteral(map^.element[i]);
        if keyValuePair^.element[0]^.equals(key) then begin
          disposeLiteral(keyValuePair^.element[1]);
          keyValuePair^.element[1]:=value;
          value^.rereference;
          exit(map);
        end;
      end;
      map^.append(
        newListLiteral^
       .append(key  ,true,adapters)^
       .append(value,true,adapters),false,adapters);
      result:=map;
    end;
  end;

FUNCTION mapGet(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR map,keyValuePair:P_listLiteral;
      key:P_stringLiteral;
      fallback:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and ((length(params^.element)=2) or (length(params^.element)=3)) and
       (params^.element[0]^.literalType in [lt_keyValueList,lt_emptyList]) and
       (params^.element[1]^.literalType=lt_string) then begin
      map:=P_listLiteral(params^.element[0]);
      key:=P_stringLiteral(params^.element[1]);
      if length(params^.element)=3 then fallback:=params^.element[2]
                                   else fallback:=nil;
      for i:=0 to length(map^.element)-1 do begin
        keyValuePair:=P_listLiteral(map^.element[i]);
        if keyValuePair^.element[0]^.equals(key) then begin
          result:=keyValuePair^.element[1];
          result^.rereference;
          exit(result);
        end;
      end;
      if fallback=nil then exit(newListLiteral);
      result:=fallback;
      fallback^.rereference;
    end;
  end;

FUNCTION mapDrop(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR map,keyValuePair:P_listLiteral;
      key:P_stringLiteral;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (length(params^.element)=2) and
       (params^.element[0]^.literalType in [lt_keyValueList,lt_emptyList]) and
       (params^.element[1]^.literalType=lt_string) then begin
      map:=P_listLiteral(params^.element[0]);
      result:=newListLiteral;
      key:=P_stringLiteral(params^.element[1]);
      for i:=0 to length(map^.element)-1 do begin
        keyValuePair:=P_listLiteral(map^.element[i]);
        if not(keyValuePair^.element[0]^.equals(key)) then P_listLiteral(result)^.append(keyValuePair,true,adapters);
      end;
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
    if l^.literalType in C_validScalarTypes
    then txt:=txt+sysutils.format(strFmt,[P_scalarLiteral(l)^.stringForm])
    else txt:=txt+sysutils.format(strFmt,[l^.toString]);
  end;

DESTRUCTOR T_format.destroy;
  begin
  end;

VAR
  i: longint;

INITIALIZATION
  boolLit[false].create(false);
  boolLit[true].create(true);
  errLit.init;
  voidLit.create();
  for i:=-127 to 128 do intLit[i].create(i);
  DefaultFormatSettings.DecimalSeparator:='.';
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  randomize;

FINALIZATION
  boolLit[false].destroy;
  boolLit[true].destroy;
  errLit.destroy;
  voidLit.destroy;
  for i:=-127 to 128 do intLit[i].destroy;
end.
