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
  T_literal = object
  private
    numberOfReferences: longint;
    CONSTRUCTOR init(CONST lt:T_literalType);
    DESTRUCTOR destroy; virtual;
  public
    literalType:T_literalType;
    PROCEDURE rereference;
    FUNCTION unreference: longint;
    FUNCTION getReferenceCount: longint;

    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION typeString:string; virtual;
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
    FUNCTION value: boolean;
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
    FUNCTION value: int64;
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
    FUNCTION value: T_myFloat;
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
    DESTRUCTOR destroy; virtual;
  public
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
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_listLiteral = ^T_listLiteral;
  P_expressionLiteral = ^T_expressionLiteral;
  T_expressionLiteral = object(T_scalarLiteral)
  private
    val: pointer;
    CONSTRUCTOR create(CONST value: pointer);
    DESTRUCTOR destroy; virtual;
  public
    FUNCTION value: pointer;
    FUNCTION evaluate(CONST parameters:P_listLiteral; CONST location:T_tokenLocation; CONST context:pointer):P_literal;
    FUNCTION arity:longint;
    FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION typeString:string; virtual;
  end;

  GENERIC G_literalKeyMap<VALUE_TYPE>= object
    TYPE CACHE_ENTRY=record
           key:P_literal;
           value:VALUE_TYPE;
         end;
         KEY_VALUE_LIST=array of CACHE_ENTRY;
         MY_TYPE=specialize G_literalKeyMap<VALUE_TYPE>;
    VAR dat:array of KEY_VALUE_LIST;
        fill:longint;
    CONSTRUCTOR create();
    CONSTRUCTOR createClone(VAR map:MY_TYPE);
    DESTRUCTOR destroy;
    PROCEDURE rehashGrowing;
    FUNCTION put(CONST key:P_literal; CONST value:VALUE_TYPE):boolean;
    FUNCTION get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
    PROCEDURE drop(CONST key:P_literal);
    FUNCTION keyValueList:KEY_VALUE_LIST;
    FUNCTION keySet:T_arrayOfLiteral;
  end;

  P_literalKeyBooleanValueMap=^T_literalKeyBooleanValueMap;
  T_literalKeyBooleanValueMap=specialize G_literalKeyMap<boolean>;
  P_stringKeyLiteralValueMap=^T_stringKeyLiteralValueMap;
  T_stringKeyLiteralValueMap=specialize G_stringKeyMap<P_literal>;

  T_listLiteral = object(T_literal)
  private
    dat: array of P_literal;
    datFill:longint;
    nextAppendIsRange: boolean;

    lockCs:TRTLCriticalSection;
    setMap     :P_literalKeyBooleanValueMap;
    keyValueMap:P_stringKeyLiteralValueMap;

    PROCEDURE modifyType(CONST L:P_literal); inline;
  public
    CONSTRUCTOR create;
    FUNCTION append(CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false):P_listLiteral;
    FUNCTION appendString(CONST s:ansistring):P_listLiteral;
    FUNCTION appendBool  (CONST b:boolean):P_listLiteral;
    FUNCTION appendInt   (CONST i:int64):P_listLiteral;
    FUNCTION appendReal  (CONST r:T_myFloat):P_listLiteral;
    FUNCTION appendAll   (CONST L: P_listLiteral):P_listLiteral;
    PROCEDURE appendConstructing(CONST L: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters);
    PROCEDURE setRangeAppend;
    PROCEDURE dropIndexes;

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
    PROCEDURE sortBySubIndex(CONST innerIndex:longint; CONST location:T_tokenLocation; VAR adapters: T_adapters);
    PROCEDURE customSort(CONST leqExpression:P_expressionLiteral; CONST location:T_tokenLocation; VAR adapters:T_adapters);
    FUNCTION sortPerm: P_listLiteral;
    PROCEDURE toSet;
    PROCEDURE toKeyValueList;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION isKeyValuePair: boolean;
    FUNCTION clone:P_listLiteral;
    //from T_literal:
    DESTRUCTOR destroy; virtual;
    FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;
    FUNCTION listConstructorToString(CONST lengthLimit:longint=maxLongint):ansistring;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION contains(CONST other: P_literal): boolean;
    FUNCTION get(CONST other:P_literal; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
    FUNCTION getInner(CONST other:P_literal; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
    FUNCTION typeString:string; virtual;
    FUNCTION transpose:P_listLiteral;
  end;

  P_namedVariable=^T_namedVariable;
  T_namedVariable=object
    private
      id:T_idString;
      value:P_literal;
      readonly:boolean;
    public
      CONSTRUCTOR create(CONST initialId:T_idString; CONST initialValue:P_literal; CONST isReadOnly:boolean);
      DESTRUCTOR destroy;
      PROCEDURE setValue(CONST newValue:P_literal);
      FUNCTION mutate(CONST mutation:T_cStyleOperator; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
      FUNCTION getId:T_idString;
      FUNCTION getValue:P_literal;
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):ansistring;
  end;

 {$ifdef fullVersion}
  T_variableReport=object
    dat:array of record
          id:ansistring;
          value:P_literal;
          location:string;
        end;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addVariable(CONST id:ansistring; CONST value:P_literal; CONST location:string; CONST retainExistent:boolean=false);
    PROCEDURE addVariable(CONST namedVar:P_namedVariable; CONST location:string);
    PROCEDURE addSubReport(VAR sub:T_variableReport; CONST pseudoLocation:string);
  end;
  {$endif}

  T_disposeSubruleCallback = PROCEDURE(VAR p: pointer);
  T_subruleApplyOpCallback = FUNCTION(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation): pointer;
  T_pointerToStringCallback = FUNCTION(CONST p: pointer; CONST lengthLimit:longint): string;
  T_pointerToIntCallback = FUNCTION(CONST p: pointer): longint;
  T_pointerAndIntToBooleanCallback = FUNCTION(CONST p: pointer; CONST i:longint): boolean;
  T_evaluateCompatorCallback = FUNCTION(CONST subruleLiteral:P_expressionLiteral; CONST LHSComparand,RHScomparand:P_literal; CONST callLocation:T_tokenLocation; VAR adapters:T_adapters):boolean;
  T_evaluateSubruleCallback = FUNCTION(CONST subruleLiteral:P_expressionLiteral; CONST location:T_tokenLocation; CONST parameters:P_listLiteral; CONST context:pointer):P_literal;
  T_expressionToTokensCallback = FUNCTION(CONST subruleLiteral:P_expressionLiteral):P_listLiteral;

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
  subruleToArityCallback: T_pointerToIntCallback;
  subruleAcceptParCountCallback: T_pointerAndIntToBooleanCallback;
  subruleApplyOpCallback: T_subruleApplyOpCallback;
  evaluateCompatorCallback: T_evaluateCompatorCallback;
  evaluateSubruleCallback:T_evaluateSubruleCallback;
  expressionToTokensCallback:T_expressionToTokensCallback;

FUNCTION exp(CONST x:double):double; inline;

PROCEDURE disposeLiteral(VAR l: P_literal); inline;
FUNCTION newBoolLiteral(CONST value: boolean): P_boolLiteral; inline;
FUNCTION newIntLiteral(CONST value: int64): P_intLiteral; inline;
FUNCTION newRealLiteral(CONST value: T_myFloat): P_realLiteral; inline;
FUNCTION newStringLiteral(CONST value: ansistring): P_stringLiteral; inline;
FUNCTION newExpressionLiteral(CONST value: pointer): P_expressionLiteral; inline;
FUNCTION newListLiteral(CONST initialSize:longint=0): P_listLiteral; inline;
FUNCTION newOneElementListLiteral(CONST value: P_literal; CONST incRefs: boolean): P_listLiteral; inline;
FUNCTION newErrorLiteral: P_scalarLiteral; inline;
FUNCTION newErrorLiteralRaising(CONST errorMessage: ansistring; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
FUNCTION newErrorLiteralRaising(CONST x, y: T_literalType; CONST op: T_tokenType; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
FUNCTION newVoidLiteral: P_voidLiteral; inline;
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
FUNCTION parseNumber(CONST input: ansistring; CONST offset:longint; CONST suppressOutput: boolean; OUT parsedLength: longint): P_scalarLiteral; inline;

FUNCTION mapPut      (CONST params:P_listLiteral):P_listLiteral;
FUNCTION mapGet      (CONST params:P_listLiteral):P_literal;
FUNCTION mapDrop     (CONST params:P_listLiteral):P_listLiteral;
FUNCTION setUnion    (CONST params:P_listLiteral):P_listLiteral;
FUNCTION setIntersect(CONST params:P_listLiteral):P_listLiteral;
FUNCTION setMinus    (CONST params:P_listLiteral):P_listLiteral;

FUNCTION messagesToLiteralForSandbox(CONST messages:T_storedMessages):P_listLiteral;

FUNCTION newLiteralFromStream(VAR stream:T_streamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
PROCEDURE writeLiteralToStream(CONST L:P_literal; VAR stream:T_streamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters);

FUNCTION serialize(CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST asExpression:boolean):ansistring;
FUNCTION deserialize(CONST Source:ansistring; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
FUNCTION toParameterListString(CONST list:P_listLiteral; CONST isFinalized: boolean; CONST lengthLimit:longint=maxLongint): ansistring;
FUNCTION parameterListTypeString(CONST list:P_listLiteral):string;
IMPLEMENTATION
VAR
  boolLit: array[false..true] of T_boolLiteral;
  intLit: array[-100..4000] of T_intLiteral;
  emptyStringLit: T_stringLiteral;
  charLit: array[#0..#255] of T_stringLiteral;
  errLit: T_scalarLiteral;
  voidLit: T_voidLiteral;

FUNCTION messagesToLiteralForSandbox(CONST messages:T_storedMessages):P_listLiteral;
  FUNCTION headByMessageType(CONST messageType:T_messageType):P_listLiteral;
    begin
      if C_messageTypeMeta[messageType].prefix=''
      then result:=newListLiteral(3)^.appendString(copy(getEnumName(TypeInfo(messageType),ord(messageType)),4,1000))
      else result:=newListLiteral(3)^.appendString(C_messageTypeMeta[messageType].prefix);
    end;

  VAR i,j:longint;
  begin
    result:=newListLiteral;
    for i:=0 to length(messages)-1 do with messages[i] do if not(C_messageTypeMeta[messageType].ignoredBySandbox) then begin
      if length(multiMessage)>0 then begin
        simpleMessage:=multiMessage[0];
        for j:=1 to length(multiMessage)-1 do simpleMessage:=simpleMessage+C_lineBreakChar+multiMessage[j];
      end;
      result^.append(
         headByMessageType(messageType)^
        .appendString(ansistring(location))^
        .appendString(simpleMessage),false);
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

PROCEDURE disposeLiteral(VAR l: P_literal);
  begin
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
    if (value>=low(intLit)) and (value<=high(intLit)) then begin
      result:=@intLit[value];
      result^.rereference;
    end else new(result, create(value));
  end;

FUNCTION newRealLiteral(CONST value: T_myFloat): P_realLiteral;
  begin
    new(result, create(value));
  end;

FUNCTION newStringLiteral(CONST value: ansistring): P_stringLiteral;
  begin
    if length(value)<=1 then begin
      if length(value)=1 then result:=@charLit[value[1]]
                         else result:=@emptyStringLit;
      result^.rereference;
    end else new(result, create(value));
  end;

FUNCTION newExpressionLiteral(CONST value: pointer): P_expressionLiteral;
  begin
    new(result, create(value));
  end;

FUNCTION newListLiteral(CONST initialSize:longint=0): P_listLiteral;
  begin
    new(result, create);
    if initialSize>0 then setLength(result^.dat,initialSize);
  end;

FUNCTION newOneElementListLiteral(CONST value: P_literal; CONST incRefs: boolean): P_listLiteral;
  begin
    result:=newListLiteral;
    result^.append(value, incRefs);
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
    adapters.raiseError('Operator '+C_tokenInfo[op].defaultId+ ' is not supported for types '+C_typeString [x]+' and '+ C_typeString [y], tokenLocation);
  end;

FUNCTION newVoidLiteral: P_voidLiteral; inline;
  begin
    result:=@voidLit;
    voidLit.rereference;
  end;

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

{$ifdef fullVersion}
CONSTRUCTOR T_variableReport.create;
  begin
    setLength(dat,0);
  end;

DESTRUCTOR T_variableReport.destroy;
  begin
    setLength(dat,0);
  end;

PROCEDURE T_variableReport.addVariable(CONST id: ansistring; CONST value: P_literal; CONST location: string; CONST retainExistent:boolean=false);
  VAR i,j:longint;
  begin
    if not(retainExistent) then begin
      j:=0;
      for i:=0 to length(dat)-1 do if dat[i].id<>id then begin
        dat[j]:=dat[i];
        inc(j);
      end;
      setLength(dat,j);
    end;
    setLength(dat,length(dat)+1);
    dat[length(dat)-1].id:=id;
    dat[length(dat)-1].value:=value;
    dat[length(dat)-1].location:=location;
  end;

PROCEDURE T_variableReport.addVariable(CONST namedVar: P_namedVariable; CONST location: string);
  begin
    addVariable(namedVar^.id,namedVar^.value,location);
  end;

PROCEDURE T_variableReport.addSubReport(VAR sub:T_variableReport; CONST pseudoLocation:string);
  VAR i,i1:longint;
  begin
    i1:=length(sub.dat)-1;
    for i:=i1 downto 0 do
    if i=0 then addVariable('par.: '+sub.dat[i].id,sub.dat[i].value,pseudoLocation,true)
           else addVariable('par.: '+sub.dat[i].id,sub.dat[i].value,''            ,true);
  end;
{$endif}
//=====================================================================================================================

CONSTRUCTOR G_literalKeyMap.create();
  VAR i:longint;
  begin
    setLength(dat,16);
    for i:=0 to length(dat)-1 do setLength(dat[i],0);
    fill:=0;
  end;

CONSTRUCTOR G_literalKeyMap.createClone(VAR map:MY_TYPE);
  VAR i,j:longint;
  begin
    setLength(dat,length(map.dat));
    for i:=0 to length(dat)-1 do begin
      setLength(dat[i],length(map.dat[i]));
      for j:=0 to length(dat[i])-1 do dat[i,j]:=map.dat[i,j];
    end;
    fill:=map.fill;
  end;

DESTRUCTOR G_literalKeyMap.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do setLength(dat[i],0);
    setLength(dat,0);
  end;

PROCEDURE G_literalKeyMap.rehashGrowing;
  VAR oldLen:longint;
      temp:KEY_VALUE_LIST;
      c0,c1,i,j,k:longint;
  begin
    oldLen:=length(dat);
    setLength(dat,oldLen*2);
    for i:=0 to oldLen-1 do begin
      temp:=dat[i];
      setLength(dat[i+oldLen],length(dat[i]));
      c0:=0;
      c1:=0;
      for j:=0 to length(temp)-1 do begin
        k:=temp[j].key^.hash and (length(dat)-1);
        if k=i then begin
          dat[i][c0]:=temp[j];
          inc(c0);
        end else begin
          dat[k][c1]:=temp[j];
          inc(c1);
        end;
      end;
      setLength(dat[i       ],c0);
      setLength(dat[i+oldLen],c1);
      setLength(temp,0);
    end;
  end;

FUNCTION G_literalKeyMap.put(CONST key:P_literal; CONST value:VALUE_TYPE):boolean;
  VAR binIdx:longint;
      j:longint;
  begin
    binIdx:=key^.hash and (length(dat)-1);
    j:=0;
    while (j<length(dat[binIdx])) and not(dat[binIdx,j].key^.equals(key)) do inc(j);
    if j>=length(dat[binIdx]) then begin
      setLength(dat[binIdx],j+1);
      result:=true;
      dat[binIdx,j].key:=key;
      inc(fill);
    end else result:=false;
    dat[binIdx,j].value:=value;
    if fill>length(dat)*4 then rehashGrowing;
  end;

FUNCTION G_literalKeyMap.get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
  VAR binIdx:longint;
      j:longint;
  begin
    binIdx:=key^.hash and (length(dat)-1);
    result:=fallbackIfNotFound;
    for j:=0 to length(dat[binIdx])-1 do if dat[binIdx,j].key^.equals(key) then exit(dat[binIdx,j].value);
  end;

PROCEDURE G_literalKeyMap.drop(CONST key:P_literal);
  VAR binIdx:longint;
      j,i:longint;
  begin
    binIdx:=key^.hash and (length(dat)-1);
    for j:=0 to length(dat[binIdx])-1 do if dat[binIdx,j].key^.equals(key) then begin
      i:=length(dat[binIdx])-1;
      if (j<i) then dat[binIdx,j]:=dat[binIdx,i];
      setLength(dat[binIdx],i);
      exit;
    end;
  end;

FUNCTION G_literalKeyMap.keyValueList:KEY_VALUE_LIST;
  VAR i,j,k:longint;
  begin
    setLength(result,0);
    k:=0;
    for i:=0 to length(dat)-1 do for j:=0 to length(dat[i])-1 do begin
      setLength(result,k+1);
      result[k]:=dat[i,j];
      inc(k);
    end;
  end;

FUNCTION G_literalKeyMap.keySet:T_arrayOfLiteral;
  VAR i,j,k:longint;
  begin
    setLength(result,0);
    k:=0;
    for i:=0 to length(dat)-1 do for j:=0 to length(dat[i])-1 do begin
      setLength(result,k+1);
      result[k]:=dat[i,j].key;
      inc(k);
    end;
  end;

//=====================================================================================================================
CONSTRUCTOR T_literal.init(CONST lt:T_literalType); begin literalType:=lt; numberOfReferences:=1; end;

PROCEDURE T_literal.rereference;
  begin
    InterLockedIncrement(numberOfReferences);
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

//CONSTRUCTORS:=================================================================
CONSTRUCTOR T_voidLiteral.create();                              begin inherited init(lt_void); end;
CONSTRUCTOR T_boolLiteral      .create(CONST value: boolean);    begin inherited init(lt_boolean); val:=value; end;
CONSTRUCTOR T_intLiteral       .create(CONST value: int64);      begin inherited init(lt_int); val:=value; end;
CONSTRUCTOR T_realLiteral      .create(CONST value: T_myFloat);  begin inherited init(lt_real); val:=value; end;
CONSTRUCTOR T_stringLiteral    .create(CONST value: ansistring); begin inherited init(lt_string); val:=value; end;
CONSTRUCTOR T_expressionLiteral.create(CONST value: pointer);    begin inherited init(lt_expression); val:=value; end;
CONSTRUCTOR T_listLiteral.create;
  begin
    inherited init(lt_emptyList);
    setLength(dat, 0);
    datFill:=0;
    nextAppendIsRange:=false;
    setMap:=nil;
    keyValueMap:=nil;
    system.initCriticalSection(lockCs);
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
    dropIndexes;
    for i:=0 to datFill-1 do if dat[i]<>nil then disposeLiteral(dat[i]);
    setLength(dat,0);
    system.doneCriticalSection(lockCs);
  end;
//==================================================================:DESTRUCTORS
//?.value:======================================================================
FUNCTION T_intLiteral       .value: int64;      begin result:=val; end;
FUNCTION T_realLiteral      .value: T_myFloat;  begin result:=val; end;
FUNCTION T_stringLiteral    .value: ansistring; begin result:=val; end;
FUNCTION T_boolLiteral      .value: boolean;    begin result:=val; end;
FUNCTION T_expressionLiteral.value: pointer;    begin result:=val; end;
FUNCTION T_listLiteral      .value(index: longint): P_literal;
  begin
    result:=dat[index];
  end;
//======================================================================:?.value

FUNCTION T_listLiteral.size: longint;
  begin
    result:=datFill;
  end;

FUNCTION T_listLiteral.head:P_literal;
  begin
    if datFill=0
    then result:=@self
    else result:=dat[0];
    result^.rereference;
  end;

FUNCTION T_listLiteral.head(CONST headSize: longint): P_listLiteral;
  VAR i,imax:longint;
  begin
    imax:=headSize;
    if imax>datFill then imax:=datFill;
    if imax<0 then imax:=0;
    result:=newListLiteral;
    setLength(result^.dat,imax);
    for i:=0 to imax-1 do result^.append(dat[i],true);
  end;

FUNCTION T_listLiteral.tail:P_listLiteral;
  begin result:=tail(1); end;

FUNCTION T_listLiteral.tail(CONST headSize:longint):P_listLiteral;
  VAR i,iMin:longint;
  begin
    iMin:=headSize;
    if iMin>datFill then iMin:=datFill
    else if iMin<0 then iMin:=0;
    result:=newListLiteral;
    setLength(result^.dat,datFill-iMin);
    for i:=iMin to datFill-1 do result^.append(dat[i],true);
  end;

FUNCTION T_listLiteral.trailing:P_literal;
  begin
    if datFill=0
    then result:=@self
    else result:=dat[datFill-1];
    result^.rereference;
  end;

FUNCTION T_listLiteral.trailing(CONST trailSize:longint):P_listLiteral;
  begin result:=tail(datFill-trailSize); end;

FUNCTION T_listLiteral.leading:P_listLiteral;
  begin result:=head(datFill-1); end;

FUNCTION T_listLiteral.leading  (CONST trailSize:longint):P_listLiteral;
  begin result:=head(datFill-trailSize); end;
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

FUNCTION T_expressionLiteral.toString(CONST lengthLimit:longint=maxLongint): ansistring; begin result:=subruleToStringCallback(val,lengthLimit); end;
FUNCTION T_listLiteral      .toString(CONST lengthLimit:longint=maxLongint): ansistring;
  VAR i,remainingLength: longint;
  begin
    if datFill = 0 then result:='[]'
    else begin
      remainingLength:=lengthLimit-1;
      result:='['+dat[0]^.toString(remainingLength);
      for i:=1 to datFill-1 do if remainingLength>0 then begin
        remainingLength:=lengthLimit-length(result);
        result:=result+','+dat[i]^.toString(remainingLength);
      end else begin
        result:=result+',... ';
        break;
      end;
      result:=result+']';
    end;
  end;

FUNCTION T_listLiteral.listConstructorToString(CONST lengthLimit:longint=maxLongint):ansistring;
  VAR i,remainingLength:longint;
  begin
    if datFill = 0 then result:='['
    else begin
      remainingLength:=lengthLimit-1;
      result:='['+dat[0]^.toString;
      for i:=1 to datFill-1 do if remainingLength>0 then begin
        remainingLength:=lengthLimit-length(result);
        result:=result+','+dat[i]^.toString(remainingLength);
      end else begin
        result:=result+',... ';
        break;
      end;
      if nextAppendIsRange then result:=result+'..'
                           else result:=result+',';
    end;
  end;

//===================================================================:?.toString
FUNCTION toParameterListString(CONST list:P_listLiteral; CONST isFinalized: boolean; CONST lengthLimit:longint=maxLongint): ansistring;
  VAR i,remainingLength: longint;
  begin
    if list<>nil then with list^ do begin
      if datFill = 0 then if isFinalized then exit('()')
                                         else exit('(');
      remainingLength:=lengthLimit-1;
      result:='('+dat[0]^.toString(lengthLimit);
      for i:=1 to datFill-1 do if remainingLength>0 then begin
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
    if (relation=tt_operatorIn) and (other^.literalType in C_validListTypes) and (P_listLiteral(other)^.contains(@self)) then exit(true);
    if other^.literalType<>lt_boolean then exit(false);
    ovl:=P_boolLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
           or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
           or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_intLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR ovi: int64;
      ovr: T_myFloat;
  begin
    if (relation=tt_operatorIn) and (other^.literalType in C_validListTypes) and (P_listLiteral(other)^.contains(@self)) then exit(true);
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

FUNCTION T_realLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR ovi: int64;
      ovr: T_myFloat;
  begin
    if (relation=tt_operatorIn) and (other^.literalType in C_validListTypes) and (P_listLiteral(other)^.contains(@self)) then exit(true);
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

FUNCTION T_stringLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR ovl: ansistring;
  begin
    if (relation=tt_operatorIn) and (other^.literalType in C_validListTypes) and (P_listLiteral(other)^.contains(@self)) then exit(true);
    if other^.literalType<>lt_string then exit(false);
    ovl:=P_stringLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
         or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_expressionLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR myTxt,otherTxt:ansistring;
  begin
    if (relation=tt_operatorIn) and (other^.literalType in C_validListTypes) and (P_listLiteral(other)^.contains(@self)) then exit(true);
    if other^.literalType<>lt_expression then exit(false);
    if (relation in [tt_comparatorEq,tt_comparatorListEq]) and equals(other) then exit(true);
    myTxt   :=toString;
    otherTxt:=other^.toString;
    result:=(myTxt=otherTxt) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
         or (myTxt<otherTxt) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
         or (myTxt>otherTxt) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_listLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean;
  VAR i:longint;
  begin
    if not(other^.literalType in C_validListTypes) then exit(false);
    if (relation=tt_operatorIn) and (P_listLiteral(other)^.contains(@self)) then exit(true);
    if not(relation in [tt_comparatorListEq,tt_comparatorNeq]) then exit(false);
    result:=datFill=P_listLiteral(other)^.datFill;
    for i:=0 to datFill-1 do result:=result and dat[i]^.isInRelationTo(tt_comparatorListEq,P_listLiteral(other)^.dat[i]);
    if relation=tt_comparatorNeq then result:=not(result);
  end;
//=============================================================:?.isInRelationTo
//?.negate:=====================================================================
FUNCTION T_literal.negate(CONST minusLocation: T_tokenLocation; VAR adapters: T_adapters): P_literal;
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
    for i:=0 to datFill-1 do res^.append(dat[i]^.negate(minusLocation,adapters), false);
    result:=res;
  end;
//=====================================================================:?.negate
FUNCTION T_literal.typeString: string;
  begin
    result:=C_typeString[literalType];
  end;

FUNCTION T_expressionLiteral.typeString: string;
  begin
    result:=C_typeString[literalType]+'('+intToStr(arity)+')';
  end;

FUNCTION T_listLiteral.typeString:string;
  begin
    result:=C_typeString[literalType]+'('+intToStr(datFill)+')';
  end;

FUNCTION parameterListTypeString(CONST list:P_listLiteral):string;
  VAR i:longint;
  begin
    if (list=nil) or (list^.datFill<=0) then exit('()');
    with list^ do begin
      result:='('+dat[0]^.typeString;
      for i:=1 to datFill-1 do result:=result+', '+dat[i]^.typeString;
      result:=result+')';
    end;
  end;

FUNCTION T_listLiteral.transpose:P_listLiteral;
  VAR innerSize:longint=-1;
      i,j:longint;
      innerList:P_listLiteral;

  PROCEDURE removeTrailingVoids;
    begin
      while (innerList^.datFill>0) and (innerList^.dat[innerList^.datFill-1]^.literalType=lt_void) do begin
        disposeLiteral(innerList^.dat[innerList^.datFill-1]);
        dec(innerList^.datFill);
      end;
      setLength(innerList^.dat,innerList^.datFill);
    end;

  begin
    if literalType=lt_emptyList then begin result:=@self; result^.rereference; end;
    for i:=0 to datFill-1 do
    if (dat[i]^.literalType in C_validListTypes)
    then innerSize:=max(innerSize,P_listLiteral(dat[i])^.size)
    else innerSize:=max(innerSize,1);

    result:=newListLiteral;
    for i:=0 to innerSize-1 do begin
      innerList:=newListLiteral(datFill);
      for j:=0 to datFill-1 do begin
        if (dat[j]^.literalType in C_validListTypes) and (P_listLiteral(dat[j])^.datFill>i)
        then                                                               innerList^.append(P_listLiteral(dat[j])^.dat[i],true,false)
        else if (dat[j]^.literalType in C_validScalarTypes) and (i=0) then innerList^.append(              dat[j]         ,true,false)
        else                                                               innerList^.append(newVoidLiteral,false,true);
      end;
      removeTrailingVoids;
      result^.append(innerList,false);
    end;
  end;

//?.hash:=======================================================================
FUNCTION T_literal.hash: T_hashInt; begin result:=$ffffffff; end;
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

FUNCTION T_expressionLiteral.hash: T_hashInt;
  VAR i:longint;
      s:string;
  begin
    {$Q-}{$R-}
    s:= toString;
    result:=T_hashInt(lt_expression)+T_hashInt(length(s));
    for i:=1 to length(s) do result:=result*31+ord(s[i]);
    {$Q+}{$R+}
  end;

FUNCTION T_listLiteral.hash: T_hashInt;
  VAR i: longint;
  begin
    {$Q-}{$R-}
    result:=T_hashInt(lt_list)+T_hashInt(datFill);
    for i:=0 to datFill-1 do result:=result*31+dat[i]^.hash;
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

FUNCTION T_listLiteral.equals(CONST other: P_literal): boolean;
  VAR i: longint;
  begin
    if (@self = other) then exit(true);
    if (other^.literalType<>literalType) or (P_listLiteral(other)^.datFill<>datFill) then exit(false);
    for i:=0 to datFill-1 do if not dat[i]^.equals(P_listLiteral(other)^.dat[i]) then exit(false);
    result:=true;
  end;

//=====================================================================:?.equals

FUNCTION T_expressionLiteral.evaluate(CONST parameters:P_listLiteral; CONST location:T_tokenLocation; CONST context:pointer):P_literal;
  begin
    result:=evaluateSubruleCallback(@self,location,parameters,context);
  end;

FUNCTION T_expressionLiteral.arity:longint;
  begin
    result:=subruleToArityCallback(val);
  end;

FUNCTION T_expressionLiteral.canApplyToNumberOfParameters(CONST parCount:longint):boolean;
  begin
    result:=subruleAcceptParCountCallback(val,parCount);
  end;

FUNCTION T_listLiteral.contains(CONST other: P_literal): boolean;
  VAR i:longint;
  begin
    system.enterCriticalSection(lockCs);
    if setMap<>nil then begin
      result:=setMap^.get(other,false);
      system.leaveCriticalSection(lockCs);
      exit(result);
    end;
    system.leaveCriticalSection(lockCs);
    result:=false;
    for i:=0 to datFill-1 do if dat[i]^.equals(other) then exit(true);
  end;

FUNCTION T_listLiteral.get(CONST other: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters: T_adapters): P_literal;
{$MACRO ON}
{$define checkedExit:=
  if result^.literalType = lt_listWithError then begin
    disposeLiteral(result);
    result:=newErrorLiteral;
  end;
  exit(result)}
  VAR i,i1,j:longint;
      key:ansistring;
      L:P_literal;
  begin
    result:=nil;
    case other^.literalType of
      lt_int: begin
        i1:=datFill;
        i:=P_intLiteral(other)^.val;
        if (i>=0) and (i<i1) then begin
          result:=dat[i];
          result^.rereference;
          checkedExit;
        end else exit(newVoidLiteral);
      end;
      lt_intList: begin
        result:=newListLiteral;
        setLength(P_listLiteral(result)^.dat,P_listLiteral(other)^.datFill);
        i1:=datFill;
        for j:=0 to P_listLiteral(other)^.datFill-1 do begin
          i:=P_intLiteral(P_listLiteral(other)^.dat[j])^.val;
          if (i>=0) and (i<i1) then P_listLiteral(result)^.append(dat[i], true);
        end;
        checkedExit;
      end;
      lt_booleanList: begin
        result:=newListLiteral;
        i1:=datFill;
        setLength(P_listLiteral(result)^.dat,i1);
        if i1 = P_listLiteral(other)^.datFill then
        for i:=0 to P_listLiteral(other)^.datFill-1 do
        if P_boolLiteral(P_listLiteral(other)^.dat[i])^.val then
          P_listLiteral(result)^.append(dat[i], true);
        checkedExit;
      end;
      lt_string: if literalType in [lt_keyValueList,lt_emptyList] then begin
        system.enterCriticalSection(lockCs);
        if keyValueMap<>nil then begin
          if not(keyValueMap^.containsKey(P_stringLiteral(other)^.val,result)) then result:=@voidLit;
          system.leaveCriticalSection(lockCs);
          result^.rereference;
          exit(result);
        end else begin
          system.leaveCriticalSection(lockCs);
          key:=P_stringLiteral(other)^.val;
          for i:=0 to datFill-1 do
          if P_stringLiteral(P_listLiteral(dat[i])^.dat[0])^.val = key then begin
            result:=P_listLiteral(dat[i])^.dat[1];
            result^.rereference;
            checkedExit;
          end;
          exit(newVoidLiteral);
        end;
      end else begin
        adapters.raiseError('get with a string as second parameter can only be applied to key-value-lists!', tokenLocation);
        exit(newErrorLiteral);
      end;
      lt_stringList: if literalType in [lt_keyValueList,lt_emptyList] then begin
        result:=newListLiteral;
        setLength(P_listLiteral(result)^.dat,P_listLiteral(other)^.size);
        system.enterCriticalSection(lockCs);
        if keyValueMap<>nil then begin
          for j:=0 to P_listLiteral(other)^.size-1 do
            if keyValueMap^.containsKey(P_stringLiteral(P_listLiteral(other)^.dat[j])^.val,L)
            then P_listLiteral(result)^.append(L,true);
          system.leaveCriticalSection(lockCs);
        end else begin
          system.leaveCriticalSection(lockCs);
          for j:=0 to P_listLiteral(other)^.size-1 do begin
            key:=P_stringLiteral(P_listLiteral(other)^.dat[j])^.value;
            i:=0; while i<datFill do
            if P_stringLiteral(P_listLiteral(dat[i])^.dat[0])^.value = key then begin
              P_listLiteral(result)^.append(P_listLiteral(dat[i])^.dat[1], true);
              i:=datFill;
            end else inc(i);
          end;
        end;
        checkedExit;
      end else begin
        adapters.raiseError('get with a stringList as second parameter can only be applied to key-value-lists!', tokenLocation);
        exit(newErrorLiteral);
      end;
      lt_emptyList: exit(newListLiteral);
    end;
    if result=nil then exit(newErrorLiteral);
  end;

FUNCTION T_listLiteral.getInner(CONST other: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters: T_adapters): P_literal;
  VAR i:longint;
  begin
    result:=newListLiteral;
    for i:=0 to datFill-1 do
    if dat[i]^.literalType in C_validListTypes
    then P_listLiteral(result)^.append(P_listLiteral(dat[i])^.get(other,tokenLocation,adapters),false)
    else begin
      disposeLiteral(result);
      exit(newErrorLiteral);
    end;
    checkedExit;
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
      lt_int:  result:=val<=P_intLiteral (other)^.val;
      lt_real: result:=val<=P_realLiteral(other)^.val;
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
    if (other^.literalType in C_validListTypes) then begin
      if datFill<P_listLiteral(other)^.datFill then exit(true)
      else if datFill>P_listLiteral(other)^.datFill then exit(false)
      else for i:=0 to datFill-1 do if dat[i]^.leqForSorting(P_listLiteral(other)^.dat[i]) then begin
        if not(P_listLiteral(other)^.dat[i]^.leqForSorting(dat[i])) then exit(true);
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

PROCEDURE T_listLiteral.modifyType(CONST L:P_literal); inline;
  begin
    case literalType of
      lt_list: if L^.literalType in [lt_error,lt_listWithError] then literalType:=lt_listWithError;
      lt_booleanList  : case L^.literalType of
  	                  lt_boolean: begin end;
  			  lt_error,lt_listWithError         : literalType:=lt_listWithError;
  			  lt_list..lt_flatList,lt_expression: literalType:=lt_list;
  	                  else                                literalType:=lt_flatList;
                        end;
      lt_intList      : case L^.literalType of
                          lt_int: begin end;
  	                  lt_error,lt_listWithError         : literalType:=lt_listWithError;
  			  lt_list..lt_flatList,lt_expression: literalType:=lt_list;
                          lt_real:                            literalType:=lt_numList;
  			  else                                literalType:=lt_flatList;
  	                end;
      lt_realList     : case L^.literalType of
  	                  lt_real: begin end;
  			  lt_error,lt_listWithError         : literalType:=lt_listWithError;
  			  lt_list..lt_flatList,lt_expression: literalType:=lt_list;
  	                  lt_int:                             literalType:=lt_numList;
  			  else                                literalType:=lt_flatList;
                        end;
      lt_numList      : case L^.literalType of
                          lt_int,lt_real: begin end;
  	                  lt_error,lt_listWithError         : literalType:=lt_listWithError;
  	                  lt_list..lt_flatList,lt_expression: literalType:=lt_list;
  	                  else                                literalType:=lt_flatList;
  	                end;
      lt_stringList   : case L^.literalType of
	                  lt_string: begin end;
  	                  lt_error,lt_listWithError         : literalType:=lt_listWithError;
  	                  lt_list..lt_flatList,lt_expression: literalType:=lt_list;
  	                  else                                literalType:=lt_flatList;
  	                end;
      lt_emptyList    : case L^.literalType of
  	                  lt_error,lt_listWithError: literalType:=lt_listWithError;
                          lt_boolean:                literalType:=lt_booleanList;
                          lt_int:                    literalType:=lt_intList;
                          lt_real:                   literalType:=lt_realList;
                          lt_string:                 literalType:=lt_stringList;
                          lt_expression:             literalType:=lt_list;
                          lt_list..lt_flatList: if P_listLiteral(L)^.isKeyValuePair
                                                then literalType:=lt_keyValueList
                                                else literalType:=lt_list;
                        end;
      lt_keyValueList: if not((L^.literalType in C_validListTypes) and (P_listLiteral(L)^.isKeyValuePair)) then literalType:=lt_list;
      lt_flatList:     case L^.literalType of
  	                 lt_error,lt_listWithError: literalType:=lt_listWithError;
  	                 lt_list..lt_flatList:      literalType:=lt_list;
                       end;
    end;
  end;

FUNCTION T_listLiteral.append(CONST L: P_literal; CONST incRefs: boolean; CONST forceVoidAppend:boolean=false): P_listLiteral;
  begin
    result:=@self;
    if L = nil then begin
      raise Exception.create('Trying to append NIL literal to list');
      exit;
    end;
    if (L^.literalType=lt_void) and not(forceVoidAppend) then exit;
    if length(dat)<=datFill then setLength(dat,datFill+16);
    dat[datFill]:=L;
    inc(datFill);
    if incRefs then L^.rereference;
    modifyType(L);
    system.enterCriticalSection(lockCs);
    if (setMap<>nil) and (setMap^.put(L,true)) then begin
      dispose(setMap,destroy);
      setMap:=nil;
      //if the list is not unique, it cannot be a map
      if (keyValueMap<>nil) then begin
        dispose(keyValueMap,destroy);
        keyValueMap:=nil;
      end;
    end;
    if (keyValueMap<>nil) then begin
      if (literalType<>lt_keyValueList) or (keyValueMap^.containsKey(P_stringLiteral(P_listLiteral(L)^.value(0))^.val)) then begin
        dispose(keyValueMap,destroy);
        keyValueMap:=nil;
      end else
        keyValueMap^.put(P_stringLiteral(P_listLiteral(L)^.value(0))^.val,
                                         P_listLiteral(L)^.value(1));
    end;
    system.leaveCriticalSection(lockCs);
  end;

FUNCTION T_listLiteral.appendString(CONST s: ansistring): P_listLiteral;
  begin
    result:=append(newStringLiteral(s),false);
  end;

FUNCTION T_listLiteral.appendBool(CONST b: boolean): P_listLiteral;
  begin
    result:=append(@boolLit[b],true);
  end;

FUNCTION T_listLiteral.appendInt(CONST i: int64): P_listLiteral;
  begin
    result:=append(newIntLiteral(i),false);
  end;

FUNCTION T_listLiteral.appendReal(CONST r: T_myFloat): P_listLiteral;
  begin
    result:=append(newRealLiteral(r),false);
  end;

FUNCTION T_listLiteral.appendAll(CONST L: P_listLiteral): P_listLiteral;
  VAR i: longint;
  begin
    for i:=0 to L^.datFill-1 do append(L^.dat[i], true);
    result:=@self;
  end;

PROCEDURE T_listLiteral.appendConstructing(CONST L: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters: T_adapters);
  VAR
    last: P_literal;
    i0, i1: int64;
    c0, c1: char;
    newLen: longint;
  begin
    if not (nextAppendIsRange) then begin
      append(L, true);
      exit;
    end;
    nextAppendIsRange:=false;

    if datFill = 0 then begin
      literalType:=lt_listWithError;
      adapters.raiseError('Cannot append range to empty list', tokenLocation);
      exit;
    end;
    last:=dat[datFill-1];
    if (last^.literalType = lt_int) and (L^.literalType = lt_int) then begin
      i0:=P_intLiteral(last)^.val;
      i1:=P_intLiteral(L)^.val;
      newLen:=datFill+abs(i1-i0)+1;
      if newLen>length(dat) then setLength(dat,newLen);
      while (i0<i1) and adapters.noErrors do begin
        inc(i0);
        appendInt(i0);
      end;
      while (i0>i1) and adapters.noErrors do begin
        dec(i0);
        appendInt(i0);
      end;
    end else if (last^.literalType = lt_string) and
      (length(P_stringLiteral(last)^.val) = 1) and (L^.literalType = lt_string) and
      (length(P_stringLiteral(L   )^.val) = 1) then begin
      c0:=P_stringLiteral(last)^.val [1];
      c1:=P_stringLiteral(L)^.val [1];
      newLen:=datFill+abs(ord(c1)-ord(c0))+1;
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
      literalType:=lt_listWithError;
      adapters.raiseError('Invalid range expression '+
        last^.toString+'..'+L^.toString, tokenLocation);
    end;
  end;

PROCEDURE T_listLiteral.setRangeAppend;
  begin
    nextAppendIsRange:=true;
  end;

PROCEDURE T_listLiteral.dropIndexes;
  begin
    system.enterCriticalSection(lockCs);
    if setMap     <>nil then dispose(setMap     ,destroy); setMap     :=nil;
    if keyValueMap<>nil then dispose(keyValueMap,destroy); keyValueMap:=nil;
    system.leaveCriticalSection(lockCs);
  end;

PROCEDURE T_listLiteral.sort;
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  begin
    if (datFill<=1) then exit;
    scale:=1;
    setLength(temp, datFill);
    while scale<datFill do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<datFill do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<datFill) do
          if dat[j0]^.leqForSorting(dat[j1])    then begin temp[k]:=dat[j0]; inc(k); inc(j0); end
                                                else begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<datFill) do begin temp[k]:=dat[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<datFill) do begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<datFill) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while i<datFill do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<datFill) do
            if temp [j0]^.leqForSorting(temp [j1]) then begin dat[k]:=temp [j0]; inc(k); inc(j0); end
                                                   else begin dat[k]:=temp [j1]; inc(k); inc(j1); end;
          while (j0<i+scale) and (j0<datFill)       do  begin dat[k]:=temp [j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<datFill) do  begin dat[k]:=temp [j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to datFill-1 do dat[k]:=temp[k];
    end;
    setLength(temp, 0);
  end;

PROCEDURE T_listLiteral.sortBySubIndex(CONST innerIndex:longint; CONST location:T_tokenLocation; VAR adapters: T_adapters);
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(a,b:P_literal):boolean; inline;
    begin
      if (a^.literalType in C_validListTypes) and (P_listLiteral(a)^.datFill>innerIndex) and
         (b^.literalType in C_validListTypes) and (P_listLiteral(b)^.datFill>innerIndex)
      then result:=P_listLiteral(a)^.dat[innerIndex]^.leqForSorting(P_listLiteral(b)^.dat[innerIndex])
      else begin
        result:=false;
        adapters.raiseError('Invalid sorting index '+intToStr(innerIndex)+' for elements '+a^.toString(50)+' and '+b^.toString(50),location);
      end;
    end;

  begin
    if datFill<=1 then exit;
    scale:=1;
    setLength(temp, datFill);
    while (scale<datFill) and adapters.noErrors do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while (i<datFill) and adapters.noErrors do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<datFill) do
          if isLeq(dat[j0],dat[j1]     )        then begin temp[k]:=dat[j0]; inc(k); inc(j0); end
                                                else begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<datFill) do begin temp[k]:=dat[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<datFill) do begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      if not(adapters.noErrors) then exit;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<datFill) and adapters.noErrors then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while (i<datFill) and adapters.noErrors do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<datFill) do
            if isLeq(temp[j0],temp[j1])           then begin dat[k]:=temp[j0]; inc(k); inc(j0); end
                                                  else begin dat[k]:=temp[j1]; inc(k); inc(j1); end;
          while (j0<i+scale) and (j0<datFill)       do begin dat[k]:=temp[j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<datFill) do begin dat[k]:=temp[j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to datFill-1 do dat[k]:=temp [k];
    end;
    setLength(temp, 0);
  end;

PROCEDURE T_listLiteral.customSort(CONST leqExpression: P_expressionLiteral; CONST location:T_tokenLocation; VAR adapters: T_adapters);
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(a,b:P_literal):boolean; inline; begin result:=evaluateCompatorCallback(leqExpression,a,b,location,adapters); end;

  begin
    if datFill<=1 then exit;
    scale:=1;
    setLength(temp, datFill);
    while (scale<datFill) and adapters.noErrors do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while (i<datFill) and adapters.noErrors do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<datFill) do
          if isLeq(dat[j0],dat[j1]     )        then begin temp[k]:=dat[j0]; inc(k); inc(j0); end
                                                else begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        while (j0<i+scale)       and (j0<datFill) do begin temp[k]:=dat[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<datFill) do begin temp[k]:=dat[j1]; inc(k); inc(j1); end;
        inc(i, scale+scale);
      end;
      if not(adapters.noErrors) then exit;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale, scale);
      if (scale<datFill) and adapters.noErrors then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while (i<datFill) and adapters.noErrors do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<datFill) do
            if isLeq(temp [j0],temp [j1])         then begin dat[k]:=temp [j0]; inc(k); inc(j0); end
                                                  else begin dat[k]:=temp [j1]; inc(k); inc(j1); end;
          while (j0<i+scale) and (j0<datFill)       do begin dat[k]:=temp [j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<datFill) do begin dat[k]:=temp [j1]; inc(k); inc(j1); end;
          inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale, scale);
      end else for k:=0 to datFill-1 do dat[k]:=temp [k];
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
    if datFill = 0 then exit(newListLiteral);

    setLength(temp1, datFill);
    setLength(temp2, datFill);
    for i:=0 to datFill-1 do with temp1 [i] do begin
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
    setLength(result^.dat,length(temp1));
    for i:=0 to length(temp1)-1 do result^.appendInt(temp1 [i].index);
    setLength(temp1, 0);
  end;

PROCEDURE T_listLiteral.toSet;
  VAR i, j: longint;
  begin
    system.enterCriticalSection(lockCs);
    if setMap=nil then begin
      new(setMap,create());
      if keyValueMap<>nil then begin
        //If a key-value-map is present, the list is uniqe
        for i:=0 to datFill-1 do setMap^.put(dat[i],true);
      end else begin
        j:=0;
        for i:=0 to datFill-1 do
        if setMap^.get(dat[i],false)
        then disposeLiteral(dat[i])
        else begin
          setMap^.put(dat[i],true);
          dat[j]:=dat[i];
          inc(j);
        end;
        setLength(dat,j);
        datFill:=j;
      end;
    end;
    system.leaveCriticalSection(lockCs);
  end;

PROCEDURE T_listLiteral.toKeyValueList;
  VAR i,j:longint;
      key:ansistring;
      val:P_literal;
  begin
    system.enterCriticalSection(lockCs);
    if (keyValueMap=nil) and (literalType in [lt_keyValueList,lt_emptyList]) then begin
      new(keyValueMap,create());
      j:=0;
      for i:=0 to datFill-1 do begin
        key:=P_stringLiteral(P_listLiteral(dat[i])^.dat[0])^.val;
        val:=                P_listLiteral(dat[i])^.dat[1];
        if not(keyValueMap^.containsKey(key)) then begin
          keyValueMap^.put(key,val);
          dat[j]:=dat[i];
          inc(j);
        end else begin
          if setMap<>nil then setMap^.drop(dat[i]);
          disposeLiteral(dat[i]);
        end;
      end;
      setLength(dat,j);
      datFill:=j;
    end;
    system.leaveCriticalSection(lockCs);
  end;

FUNCTION T_listLiteral.isKeyValuePair: boolean;
  begin
    result:=(datFill=2)
        and (dat[0]^.literalType=lt_string);
  end;

FUNCTION T_listLiteral.clone: P_listLiteral;
  VAR i:longint;
  begin
    {$ifdef debugMode}
    writeln(stdErr,'Cloning ',typeString,'; refCount=',numberOfReferences,'; set/map-backing: ',setMap<>nil,'/',keyValueMap<>nil);
    {$endif}
    result:=newListLiteral;
    setLength(result^.dat,datFill);
    result^.datFill:=datFill;
    for i:=0 to datFill-1 do begin
      result^.dat[i]:=dat[i];
      dat[i]^.rereference;
    end;
    result^.literalType:=literalType;
    result^.nextAppendIsRange:=nextAppendIsRange;
    system.enterCriticalSection(lockCs);
    if keyValueMap<>nil then new(result^.keyValueMap,createClone(keyValueMap^));
    if setMap     <>nil then new(result^.setMap     ,createClone(setMap     ^));
    system.leaveCriticalSection(lockCs);
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
          if (RHS^.literalType in C_validListTypes) and (P_listLiteral(LHS)^.datFill = P_listLiteral(RHS)^.datFill)
          then begin
            result:=true;
            i:=0;
            while result and (i<P_listLiteral(LHS)^.datFill) do begin
              result:=result and equals(P_listLiteral(LHS)^.dat[i],
                P_listLiteral(RHS)^.dat[i]);
              inc(i);
            end;
          end else exit(false);
        else exit(false);
      end;
    end;

  FUNCTION isContained(CONST LHS, RHS: P_literal): boolean; inline;
    begin
      result:=false;
      result:=(RHS^.literalType in C_validListTypes) and P_listLiteral(RHS)^.contains(LHS);
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

  FUNCTION opAnd(CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      case LHS^.literalType of
        lt_boolean:
          case RHS^.literalType of
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorAnd, RHS, tokenLocation)));
            lt_boolean   : exit(newBoolLiteral(P_boolLiteral(LHS)^.val and P_boolLiteral(RHS)^.val));
          end;
        lt_int:
          case RHS^.literalType of
            lt_int       : exit(newIntLiteral(P_intLiteral(LHS)^.val and P_intLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorAnd, RHS, tokenLocation)));
          end;
        lt_expression:
          if RHS^.literalType in [lt_boolean,lt_int,lt_expression] then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorAnd, RHS, tokenLocation)));
      end;
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorAnd, tokenLocation,adapters);
    end;

  FUNCTION opOr(CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      case LHS^.literalType of
        lt_boolean:
          case RHS^.literalType of
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorOr, RHS, tokenLocation)));
            lt_boolean   : exit(newBoolLiteral(P_boolLiteral(LHS)^.val or P_boolLiteral(RHS)^.val));
          end;
        lt_int:
          case RHS^.literalType of
            lt_int       : exit(newIntLiteral(P_intLiteral(LHS)^.val or P_intLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorOr, RHS, tokenLocation)));
          end;
        lt_expression:
          if RHS^.literalType in [lt_boolean,lt_int,lt_expression] then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorOr, RHS, tokenLocation)));
      end;
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorOr, tokenLocation,adapters);
    end;

  FUNCTION opXor(CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      case LHS^.literalType of
        lt_boolean:
          case RHS^.literalType of
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorXor, RHS, tokenLocation)));
            lt_boolean   : exit(newBoolLiteral(P_boolLiteral(LHS)^.val xor P_boolLiteral(RHS)^.val));
          end;
        lt_int:
          case RHS^.literalType of
            lt_int       : exit(newIntLiteral(P_intLiteral(LHS)^.val xor P_intLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorXor, RHS, tokenLocation)));
          end;
        lt_expression:
          if RHS^.literalType in [lt_boolean,lt_int,lt_expression]
          then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorXor, RHS, tokenLocation)));
      end;
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorXor, tokenLocation,adapters);
    end;

  FUNCTION opPlus(CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      case LHS^.literalType of
        lt_int:
          {$O-}{$Q-}
          case RHS^.literalType of
            lt_int:        exit(newIntLiteral(P_intLiteral(LHS)^.val+P_intLiteral(RHS)^.val));
            lt_real:       exit(newRealLiteral(P_intLiteral(LHS)^.val+P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorPlus , RHS, tokenLocation)));
          end;
          {$O+}{$Q+}
        lt_real:
          case RHS^.literalType of
            lt_int:        exit(newRealLiteral(P_realLiteral(LHS)^.val+P_intLiteral (RHS)^.val));
            lt_real:       exit(newRealLiteral(P_realLiteral(LHS)^.val+P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorPlus, RHS, tokenLocation)));
          end;
        lt_string:
          case RHS^.literalType of
            lt_string:     exit(newStringLiteral(P_stringLiteral(LHS)^.val+P_stringLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorPlus, RHS, tokenLocation)));
          end;
        lt_expression:
          if RHS^.literalType in [lt_int,lt_real,lt_string,lt_expression]
          then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorPlus, RHS, tokenLocation)));
      end;
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorPlus, tokenLocation,adapters);
    end;

  FUNCTION opMinus    (CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      case LHS^.literalType of
        {$O-}{$Q-}
        lt_int:
          case RHS^.literalType of
            lt_int:        exit(newIntLiteral(P_intLiteral(LHS)^.val-P_intLiteral(RHS)^.val));
            lt_real:       exit(newRealLiteral(P_intLiteral(LHS)^.val-P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMinus, RHS, tokenLocation)));
          end;
        {$O+}{$Q+}
        lt_real:
          case RHS^.literalType of
            lt_int:        exit(newRealLiteral(P_realLiteral(LHS)^.val-P_intLiteral (RHS)^.val));
            lt_real:       exit(newRealLiteral(P_realLiteral(LHS)^.val-P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMinus, RHS, tokenLocation)));
          end;
        lt_expression:
          if RHS^.literalType in [lt_int,lt_real,lt_expression]
          then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMinus, RHS, tokenLocation)));
      end;
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorMinus, tokenLocation,adapters);
    end;

  FUNCTION opMult     (CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      case LHS^.literalType of
        lt_int:
          {$O-}{$Q-}
          case RHS^.literalType of
            lt_int:        exit(newIntLiteral(P_intLiteral(LHS)^.val*P_intLiteral(RHS)^.val));
            lt_real:       exit(newRealLiteral(P_intLiteral(LHS)^.val*P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMult, RHS, tokenLocation)));
          end;
          {$O+}{$Q+}
        lt_real:
          case RHS^.literalType of
            lt_int:        exit(newRealLiteral(P_realLiteral(LHS)^.val*P_intLiteral (RHS)^.val));
            lt_real:       exit(newRealLiteral(P_realLiteral(LHS)^.val*P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMult, RHS, tokenLocation)));
          end;
        lt_expression:
          if RHS^.literalType in [lt_int,lt_real,lt_expression]
          then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMult, RHS, tokenLocation)));
      end;
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorMult, tokenLocation,adapters);
    end;

  FUNCTION opDivReal  (CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      case LHS^.literalType of
        lt_int:
          case RHS^.literalType of
            lt_int:        exit(newRealLiteral(P_intLiteral(LHS)^.val/P_intLiteral (RHS)^.val));
            lt_real:       exit(newRealLiteral(P_intLiteral(LHS)^.val/P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorDivReal, RHS, tokenLocation)));
          end;
        lt_real:
          case RHS^.literalType of
            lt_int:        exit(newRealLiteral(P_realLiteral(LHS)^.val/P_intLiteral (RHS)^.val));
            lt_real:       exit(newRealLiteral(P_realLiteral(LHS)^.val/P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorDivReal, RHS, tokenLocation)));
          end;
        lt_expression:
          if RHS^.literalType in [lt_int,lt_real,lt_expression]
          then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorDivReal, RHS, tokenLocation)));
      end;
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorDivReal, tokenLocation,adapters);
    end;

  FUNCTION opDivInt(CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      case LHS^.literalType of
        lt_int:
          case RHS^.literalType of
            lt_int: if P_intLiteral(RHS)^.val<>0 then exit(newIntLiteral(P_intLiteral(LHS)^.val div P_intLiteral(RHS)^.val))
                                                 else exit(newRealLiteral(Nan));
                           //try
                           //  exit(newIntLiteral(P_intLiteral(LHS)^.val div P_intLiteral(RHS)^.val));
                           //except
                           //  exit(newRealLiteral(Nan));
                           //end;
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorDivInt, RHS, tokenLocation)));
          end;
        lt_expression:
          if RHS^.literalType in [lt_int,lt_expression]
          then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorDivInt, RHS, tokenLocation)));
      end;
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorDivInt, tokenLocation,adapters);
    end;

  FUNCTION opMod(CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      case LHS^.literalType of
        lt_int:
          case RHS^.literalType of
            lt_int: if P_intLiteral(RHS)^.val<>0 then exit(newIntLiteral(P_intLiteral(LHS)^.val mod P_intLiteral(RHS)^.val))
                                                 else exit(newRealLiteral(Nan));
                           //try
                           //  exit(newIntLiteral(P_intLiteral(LHS)^.val mod P_intLiteral(RHS)^.val))
                           //except
                           //  exit(newRealLiteral(Nan));
                           //end;
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMod, RHS, tokenLocation)));
          end;
        lt_expression:
          if RHS^.literalType in [lt_int,lt_expression]
          then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMod, RHS, tokenLocation)));
      end;
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorMod, tokenLocation,adapters);
    end;

  FUNCTION opPot(CONST LHS,RHS:P_literal):P_scalarLiteral;
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

  begin
    case LHS^.literalType of
      lt_int:
        case RHS^.literalType of
          lt_int:        exit(pot_int_int(P_intLiteral(LHS)^.val, P_intLiteral(RHS)^.val));
          lt_real:       exit(newRealLiteral(exp(ln(P_intLiteral(LHS)^.val)*P_realLiteral(RHS)^.val)));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorPot, RHS, tokenLocation)));
        end;
      lt_real:
        case RHS^.literalType of
          lt_int:        exit(newRealLiteral(pot_real_int(P_realLiteral(LHS)^.val, P_intLiteral(RHS)^.val)));
          lt_real:       exit(newRealLiteral(exp(ln(P_realLiteral(LHS)^.val)*P_realLiteral(RHS)^.val)));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorPot, RHS, tokenLocation)));
        end;
      lt_expression:
        if RHS^.literalType in [lt_int,lt_real,lt_expression]
        then exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorPot, RHS, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, tt_operatorPot, tokenLocation,adapters);
  end;

  FUNCTION opStrConcat(CONST LHS,RHS:P_literal):P_scalarLiteral;
    begin
      if (LHS^.literalType=lt_expression) or (RHS^.literalType=lt_expression)
      then result:=newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorStrConcat, RHS, tokenLocation))
      else result:=newStringLiteral(P_scalarLiteral(LHS)^.stringForm+P_scalarLiteral(RHS)^.stringForm);
    end;

  {$define checkedExit:=
    if result^.literalType = lt_listWithError then begin
      disposeLiteral(result);
      result:=newErrorLiteralRaising(LHS^.literalType, RHS^.literalType,op, tokenLocation,adapters);
    end;
    exit(result)}
  {$define invalidLengthExit:=exit(newErrorLiteralRaising('Invalid list lengths '+intToStr(i)+' and '+intToStr(i1)+' given for operator '+C_tokenInfo[op].defaultId, tokenLocation,adapters))}

  VAR i, i1: longint;
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
                tt_operatorAnd:       exit(opAnd      (LHS,RHS));
                tt_operatorOr:        exit(opOr       (LHS,RHS));
                tt_operatorXor:       exit(opXor      (LHS,RHS));
                tt_operatorPlus:      exit(opPlus     (LHS,RHS));
                tt_operatorMinus:     exit(opMinus    (LHS,RHS));
                tt_operatorMult:      exit(opMult     (LHS,RHS));
                tt_operatorDivReal:   exit(opDivReal  (LHS,RHS));
                tt_operatorDivInt:    exit(opDivInt   (LHS,RHS));
                tt_operatorMod:       exit(opMod      (LHS,RHS));
                tt_operatorPot:       exit(opPot      (LHS,RHS));
                tt_operatorStrConcat: exit(opStrConcat(LHS,RHS));
              end;
            //scalar X scalar
            lt_list,lt_keyValueList: begin
              //scalar X nested list
              result:=newListLiteral;
              setLength(P_listLiteral(result)^.dat,P_listLiteral(RHS)^.datFill);
              for i:=0 to P_listLiteral(RHS)^.datFill-1 do
                P_listLiteral(result)^.append(
                  resolveOperator(LHS, op, P_listLiteral(RHS)^.dat[i],
                  tokenLocation,adapters),
                  false);
              checkedExit;
            end;
            lt_booleanList..lt_emptyList,lt_flatList: begin
              //scalar X flat list
              result:=newListLiteral;
              setLength(P_listLiteral(result)^.dat,P_listLiteral(RHS)^.datFill);
              case op of
                tt_comparatorEq:       for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelEqual  (LHS,P_listLiteral(RHS)^.dat[i]));
                tt_comparatorNeq:      for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelNeq    (LHS,P_listLiteral(RHS)^.dat[i]));
                tt_comparatorLeq:      for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelLeq    (LHS,P_listLiteral(RHS)^.dat[i]));
                tt_comparatorGeq:      for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelGeq    (LHS,P_listLiteral(RHS)^.dat[i]));
                tt_comparatorLss:      for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelLesser (LHS,P_listLiteral(RHS)^.dat[i]));
                tt_comparatorGrt:      for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelGreater(LHS,P_listLiteral(RHS)^.dat[i]));
                tt_operatorAnd:        for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opAnd      (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorOr:         for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opOr       (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorXor:        for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opXor      (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorPlus:       for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opPlus     (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorMinus:      for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opMinus    (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorMult:       for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opMult     (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorDivReal:    for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opDivReal  (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorDivInt:     for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opDivInt   (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorMod:        for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opMod      (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorPot:        for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opPot      (LHS,P_listLiteral(RHS)^.dat[i]),false);
                tt_operatorStrConcat:  for i:=0 to P_listLiteral(RHS)^.datFill-1 do P_listLiteral(result)^.append(opStrConcat(LHS,P_listLiteral(RHS)^.dat[i]),false);
              end;
              checkedExit;
            end;
          end;
          lt_list,lt_keyValueList: case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string: begin
              //nested list X scalar
              result:=newListLiteral;
              setLength(P_listLiteral(result)^.dat,P_listLiteral(LHS)^.datFill);
              for i:=0 to P_listLiteral(LHS)^.datFill-1 do
                P_listLiteral(result)^.append(
                  resolveOperator(P_listLiteral(LHS)^.dat[i], op,
                  RHS, tokenLocation,adapters),
                  false);
              checkedExit;
            end;
            lt_list..lt_flatList: begin
              //nested list X flat/nested list
              i :=P_listLiteral(LHS)^.datFill;
              i1:=P_listLiteral(RHS)^.datFill;
              if i = i1 then begin
                result:=newListLiteral;
                setLength(P_listLiteral(result)^.dat,i1);
                for i:=0 to i1-1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.dat[i], op,
                    P_listLiteral(RHS)^.dat[i], tokenLocation,adapters),
                    false);
                checkedExit;
              end else invalidLengthExit;
            end;
          end;
          lt_booleanList..lt_emptyList,lt_flatList: case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string: begin
              //flat list X scalar
              result:=newListLiteral;
              setLength(P_listLiteral(result)^.dat,P_listLiteral(LHS)^.datFill);
              case op of
                tt_comparatorEq:       for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelEqual  (P_listLiteral(LHS)^.dat[i],RHS));
                tt_comparatorNeq:      for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelNeq    (P_listLiteral(LHS)^.dat[i],RHS));
                tt_comparatorLeq:      for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelLeq    (P_listLiteral(LHS)^.dat[i],RHS));
                tt_comparatorGeq:      for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelGeq    (P_listLiteral(LHS)^.dat[i],RHS));
                tt_comparatorLss:      for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelLesser (P_listLiteral(LHS)^.dat[i],RHS));
                tt_comparatorGrt:      for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.appendBool(areInRelGreater(P_listLiteral(LHS)^.dat[i],RHS));
                tt_operatorAnd:        for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opAnd      (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorOr:         for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opOr       (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorXor:        for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opXor      (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorPlus:       for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opPlus     (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorMinus:      for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opMinus    (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorMult:       for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opMult     (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorDivReal:    for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opDivReal  (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorDivInt:     for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opDivInt   (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorMod:        for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opMod      (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorPot:        for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opPot      (P_listLiteral(LHS)^.dat[i],RHS),false);
                tt_operatorStrConcat:  for i:=0 to P_listLiteral(LHS)^.datFill-1 do P_listLiteral(result)^.append(opStrConcat(P_listLiteral(LHS)^.dat[i],RHS),false);
              end;
              checkedExit;
            end;
            lt_list,lt_keyValueList: begin
              //flat list X nested list
              i :=P_listLiteral(LHS)^.datFill;
              i1:=P_listLiteral(RHS)^.datFill;
              if i = i1 then begin
                result:=newListLiteral;
                setLength(P_listLiteral(result)^.dat,i1);
                for i:=0 to i1-1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.dat[i], op,
                    P_listLiteral(RHS)^.dat[i], tokenLocation,adapters),
                    false);
                checkedExit;
              end else invalidLengthExit;
            end;
            lt_booleanList..lt_emptyList,lt_flatList: begin
              //flat list X flat list
              i :=P_listLiteral(LHS)^.datFill;
              i1:=P_listLiteral(RHS)^.datFill;
              if i = i1 then begin
                result:=newListLiteral;
                setLength(P_listLiteral(result)^.dat,i1);
                case op of
                  tt_comparatorEq:       for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelEqual  (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]));
                  tt_comparatorNeq:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelNeq    (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]));
                  tt_comparatorLeq:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelLeq    (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]));
                  tt_comparatorGeq:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelGeq    (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]));
                  tt_comparatorLss:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelLesser (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]));
                  tt_comparatorGrt:      for i:=0 to i1-1 do P_listLiteral(result)^.appendBool(areInRelGreater(P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]));
                  tt_operatorAnd:        for i:=0 to i1-1 do P_listLiteral(result)^.append(opAnd      (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorOr:         for i:=0 to i1-1 do P_listLiteral(result)^.append(opOr       (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorXor:        for i:=0 to i1-1 do P_listLiteral(result)^.append(opXor      (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorPlus:       for i:=0 to i1-1 do P_listLiteral(result)^.append(opPlus     (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorMinus:      for i:=0 to i1-1 do P_listLiteral(result)^.append(opMinus    (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorMult:       for i:=0 to i1-1 do P_listLiteral(result)^.append(opMult     (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorDivReal:    for i:=0 to i1-1 do P_listLiteral(result)^.append(opDivReal  (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorDivInt:     for i:=0 to i1-1 do P_listLiteral(result)^.append(opDivInt   (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorMod:        for i:=0 to i1-1 do P_listLiteral(result)^.append(opMod      (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorPot:        for i:=0 to i1-1 do P_listLiteral(result)^.append(opPot      (P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                  tt_operatorStrConcat:  for i:=0 to i1-1 do P_listLiteral(result)^.append(opStrConcat(P_listLiteral(LHS)^.dat[i],P_listLiteral(RHS)^.dat[i]),false);
                end;
                checkedExit;
              end else invalidLengthExit;
            end;
          end;
      end;
      tt_operatorConcat: begin
        result:=newListLiteral;
        if (LHS^.literalType in C_validScalarTypes)
        then P_listLiteral(result)^.append   (LHS,true)
        else P_listLiteral(result)^.appendAll(P_listLiteral(LHS));
        if (RHS^.literalType in C_validScalarTypes)
        then P_listLiteral(result)^.append   (RHS,true)
        else P_listLiteral(result)^.appendAll(P_listLiteral(RHS));
        checkedExit;
      end;
      tt_comparatorListEq: exit(newBoolLiteral(equals(LHS, RHS)));
      tt_operatorIn: exit(newBoolLiteral(isContained(LHS, RHS)));
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

CONSTRUCTOR T_namedVariable.create(CONST initialId:T_idString; CONST initialValue:P_literal; CONST isReadOnly:boolean);
  begin
    id:=initialId;
    value:=initialValue;
    readonly:=isReadOnly;
    if value<>nil then value^.rereference;
  end;

DESTRUCTOR T_namedVariable.destroy;
  begin
    if value<>nil then disposeLiteral(value);
  end;

PROCEDURE T_namedVariable.setValue(CONST newValue:P_literal);
  begin
    if readonly then raise Exception.create('Mutation of constant "'+id+'" is not allowed.');
    disposeLiteral(value);
    value:=newValue;
    value^.rereference;
  end;

FUNCTION T_namedVariable.mutate(CONST mutation:T_cStyleOperator; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  CONST MAPPED_OP:array[tt_cso_assignPlus..tt_cso_assignDiv] of T_tokenType=(tt_operatorPlus,tt_operatorMinus,tt_operatorMult,tt_operatorDivReal);
  VAR oldValue:P_literal;
  begin
    if readonly then adapters.raiseError('Mutation of constant "'+id+'" is not allowed.',location);
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
          then P_listLiteral(oldValue)^.append(RHS, true)
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

FUNCTION T_namedVariable.getId:T_idString;
  begin
    result:=id;
  end;

FUNCTION T_namedVariable.getValue:P_literal;
  begin
    result:=value;
    result^.rereference;
  end;

FUNCTION T_namedVariable.toString(CONST lengthLimit:longint=maxLongint):ansistring;
  begin
    result:=id+'='+value^.toString(lengthLimit-1-length(id));
  end;

FUNCTION mapPut(CONST params:P_listLiteral):P_listLiteral;
  VAR map,keyValuePair:P_listLiteral;
      keyLit:P_stringLiteral;
      key:string;
      value:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.datFill=3) and
       (params^.dat[0]^.literalType in [lt_keyValueList,lt_emptyList]) and
       (params^.dat[1]^.literalType=lt_string) then begin
      map:=P_listLiteral(params^.dat[0]);
      if map^.numberOfReferences=1
      then map^.rereference
      else map:=map^.clone;
      system.enterCriticalSection(map^.lockCs);
      if (map^.keyValueMap=nil) then map^.toKeyValueList;
      keyLit:=P_stringLiteral(params^.dat[1]);
      key:=keyLit^.val;
      value:=params^.dat[2];
      if map^.keyValueMap^.containsKey(key) then begin
        for i:=0 to map^.datFill-1 do begin
          keyValuePair:=P_listLiteral(map^.dat[i]);
          if keyValuePair^.dat[0]^.equals(keyLit) then begin
            disposeLiteral(keyValuePair^.dat[1]);
            keyValuePair^.dat[1]:=value;
            value^.rereference;
            break;
          end;
        end;
      end else map^.append(newListLiteral(2)^
                           .append(keyLit,true)^
                           .append(value ,true),false);
      system.leaveCriticalSection(map^.lockCs);
      result:=map;
    end;
  end;

FUNCTION mapGet(CONST params:P_listLiteral):P_literal;
  VAR map,keyValuePair,keyList,fallbackList,resultList:P_listLiteral;
      key:P_stringLiteral;
      fallback:P_literal;
      nextElement:P_literal;
      i:longint;
      back:P_stringKeyLiteralValueMap;
      tempBack:boolean=false;
  begin
    result:=nil;
    if (params<>nil) and ((params^.datFill=2) or (params^.datFill=3)) and
       (params^.dat[0]^.literalType in [lt_keyValueList,lt_emptyList]) and
       (params^.dat[1]^.literalType in [lt_string,lt_stringList,lt_emptyList]) then begin
      map:=P_listLiteral(params^.dat[0]);
      if params^.datFill=3 then fallback:=params^.dat[2]
                           else fallback:=nil;
      if params^.dat[1]^.literalType in [lt_stringList,lt_emptyList] then begin
        keyList:=P_listLiteral(params^.dat[1]);
        system.enterCriticalSection(map^.lockCs);
        back:=map^.keyValueMap;
        if back=nil then begin
          tempBack:=true;
          new(back,create);
          for i:=0 to map^.datFill-1 do begin
            keyValuePair:=P_listLiteral(map^.dat[i]);
            back^.put(P_stringLiteral(keyValuePair^.dat[0])^.val,keyValuePair^.dat[1]);
          end;
        end;
        resultList:=newListLiteral;
        if (fallback<>nil) and (fallback^.literalType in C_validListTypes) and (P_listLiteral(fallback)^.size=keyList^.size) then begin
          fallbackList:=P_listLiteral(fallback);
          for i:=0 to keyList^.size-1 do if back^.containsKey(P_stringLiteral(keyList^.dat[i])^.val,nextElement)
          then resultList^.append(nextElement         ,true)
          else resultList^.append(fallbackList^.dat[i],true);
        end else begin
          for i:=0 to keyList^.size-1 do begin
            if back^.containsKey(P_stringLiteral(keyList^.dat[i])^.val,nextElement)
            then resultList^.append(nextElement,true);
          end;
        end;
        if tempBack then dispose(back,destroy);
        system.leaveCriticalSection(map^.lockCs);
        result:=resultList;
      end else begin
        system.enterCriticalSection(map^.lockCs);
        back:=map^.keyValueMap;
        key:=P_stringLiteral(params^.dat[1]);
        if back=nil then begin
          system.leaveCriticalSection(map^.lockCs);
          for i:=0 to map^.datFill-1 do begin
            keyValuePair:=P_listLiteral(map^.dat[i]);
            if keyValuePair^.dat[0]^.equals(key) then begin
              result:=keyValuePair^.dat[1];
              result^.rereference;
              exit(result);
            end;
          end;
        end else begin
          if back^.containsKey(P_stringLiteral(key)^.val,result)
          then begin
            result^.rereference;
            system.leaveCriticalSection(map^.lockCs);
            exit(result);
          end;
        end;
        if fallback=nil then exit(newVoidLiteral);
        result:=fallback;
        fallback^.rereference;
      end;
    end;
  end;

FUNCTION mapDrop(CONST params:P_listLiteral):P_listLiteral;
  VAR map,keyValuePair:P_listLiteral;
      key:string;
      keyLit:P_stringLiteral;
      i,j:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.datFill=2) and
       (params^.dat[0]^.literalType in [lt_keyValueList,lt_emptyList]) and
       (params^.dat[1]^.literalType=lt_string) then begin
      map:=P_listLiteral(params^.dat[0]);
      system.enterCriticalSection(map^.lockCs);
      result:=map^.clone;
      if result^.keyValueMap=nil then result^.toKeyValueList;
      keyLit:=P_stringLiteral(params^.dat[1]);
      key:=keyLit^.val;
      if result^.keyValueMap^.containsKey(key) then begin
        result^.keyValueMap^.dropKey(key);
        for i:=0 to result^.datFill-1 do begin
          keyValuePair:=P_listLiteral(map^.dat[i]);
          if keyValuePair^.value(0)^.equals(keyLit) then begin
            disposeLiteral(keyValuePair);
            for j:=i to result^.datFill-2 do result^.dat[i]:=result^.dat[i+1];
            result^.dat[result^.datFill-1]:=nil;
            dec(result^.datFill);
            break;
          end;
        end;
      end;
      system.leaveCriticalSection(map^.lockCs);
    end;
  end;

FUNCTION setUnion(CONST params:P_listLiteral):P_listLiteral;
  VAR i,j:longint;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^.value(i)^.literalType in C_validListTypes) then exit(nil);
    if params^.size=1 then begin
      if (P_listLiteral(params^.value(0))^.getReferenceCount=1) then begin
        result:=P_listLiteral(params^.value(0));
        result^.rereference;
      end else result:=P_listLiteral(params^.value(0))^.clone;
      result^.toSet;
      exit(result);
    end;
    result:=newListLiteral();
    new(result^.setMap,create);
    for i:=0 to params^.size-1 do
    with P_listLiteral(params^.value(i))^ do
    for j:=0 to size-1 do if result^.setMap^.put(value(j),true) then begin
      if length(result^.dat)<=result^.datFill then setLength(result^.dat,result^.datFill+16);
      result^.dat[result^.datFill]:=value(j);
      inc(result^.datFill);
      value(j)^.rereference;
      result^.modifyType(value(j));
    end;
  end;

FUNCTION setIntersect(CONST params:P_listLiteral):P_listLiteral;
  TYPE T_occurenceCount=specialize G_literalKeyMap<longint>;
  VAR resultSet:T_occurenceCount;
      resultList:T_occurenceCount.KEY_VALUE_LIST;
      i,j:longint;
  begin
    if not((params<>nil) and (params^.size>=1)) then exit(nil);
    for i:=0 to params^.size-1 do if not(params^.value(i)^.literalType in C_validListTypes) then exit(nil);
    if params^.size=1 then begin
      if (P_listLiteral(params^.value(0))^.getReferenceCount=1) then begin
        result:=P_listLiteral(params^.value(0));
        result^.rereference;
      end else result:=P_listLiteral(params^.value(0))^.clone;
      result^.toSet;
      exit(result);
    end;

    resultSet.create;
    for i:=0 to params^.size-1 do
      with P_listLiteral(params^.value(i))^ do
        for j:=0 to size-1 do if resultSet.get(value(j),0)=i then resultSet.put(value(j),i+1);

    i:=params^.size;
    resultList:=resultSet.keyValueList;
    result:=newListLiteral;
    new(result^.setMap,create);
    for j:=0 to length(resultList)-1 do if resultList[j].value=i then begin
      if length(result^.dat)<=result^.datFill then setLength(result^.dat,result^.datFill+16);
      result^.dat[result^.datFill]:=resultList[j].key;
      inc(result^.datFill);
      resultList[j].key^.rereference;
      result^.modifyType(resultList[j].key)
    end;
    setLength(resultList,0);
    resultSet.destroy;
  end;

FUNCTION setMinus(CONST params:P_listLiteral):P_listLiteral;
  VAR i:longint;
      LHS,RHS:P_listLiteral;
      kvl:T_literalKeyBooleanValueMap.KEY_VALUE_LIST;
  begin
    if not((params<>nil) and
           (params^.size=2) and
           (params^.value(0)^.literalType in C_validListTypes) and
           (params^.value(1)^.literalType in C_validListTypes))
    then exit(nil);

    LHS:=P_listLiteral(params^.value(0));
    RHS:=P_listLiteral(params^.value(1));
    new(result,create);
    new(result^.setMap,create);
    for i:=0 to LHS^.datFill-1 do result^.setMap^.put (LHS^.dat[i],true);
    for i:=0 to RHS^.datFill-1 do result^.setMap^.drop(RHS^.dat[i]);
    kvl:=result^.setMap^.keyValueList;
    setLength(result^.dat,length(kvl));
    result^.datFill     :=length(kvl);
    for i:=0 to length(kvl)-1 do begin
      result^.dat[i]:=kvl[i].key;
      result^.dat[i]^.rereference;
    end;
    setLength(kvl,0);
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
    intFmt:='';
    realFmt:='';
    strFmt:='';
  end;

FUNCTION newLiteralFromStream(VAR stream:T_streamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
  VAR reusableLiterals:array of P_literal;
  PROCEDURE errorOrException(CONST message:string);
    begin
      if adapters<>nil then adapters^.raiseError(message,location)
                       else raise Exception.create(message);
    end;

  FUNCTION typeStringOrNone(CONST t:T_literalType):string;
    begin
      if (t>=low(T_literalType)) and (t<=high(T_literalType)) then result:=C_typeString[t] else result:='';
    end;

  FUNCTION literalFromStream:P_literal;
    VAR literalType:T_literalType;
        reusableIndex:longint;
        literalByte:byte;
        listSize:longint;
        i:longint;
    begin
      literalByte:=stream.readByte;
      if literalByte=255 then begin
        reusableIndex:=stream.readNaturalNumber;
        if (reusableIndex<length(reusableLiterals)) then begin
          result:=reusableLiterals[reusableIndex];
          result^.rereference;
        end else begin
          result:=newErrorLiteral;
          stream.logWrongTypeError;
          errorOrException('Read invalid reuse index '+intToStr(reusableIndex)+'! Abort.');
        end;
        exit(result);
      end;
      literalType:=T_literalType(literalByte);
      case literalType of
        lt_error:result:=newErrorLiteral;
        lt_boolean:result:=newBoolLiteral(stream.readBoolean);
        lt_int:result:=newIntLiteral(stream.readInt64);
        lt_real:result:=newRealLiteral(stream.readDouble);
        lt_string:result:=newStringLiteral(stream.readAnsiString);
        lt_list,
        lt_booleanList,
        lt_intList,
        lt_realList,
        lt_numList,
        lt_stringList,
        lt_emptyList,
        lt_keyValueList,
        lt_flatList,
        lt_listWithError:begin
          listSize:=stream.readNaturalNumber;
          result:=newListLiteral;
          setLength(P_listLiteral(result)^.dat,listSize);
          for i:=0 to listSize-1 do if stream.allOkay then P_listLiteral(result)^.append(literalFromStream(),false);
          if (result^.literalType<>literalType) and (adapters<>nil) then adapters^.raiseWarning('List has other type than expected.',location);
        end;
        lt_void:result:=newVoidLiteral;
        else begin
          errorOrException('Read invalid literal type '+typeStringOrNone(literalType)+' ('+intToStr(literalByte)+') ! Abort.');
          stream.logWrongTypeError;
          exit(newErrorLiteral);
        end;
      end;
      if not(literalType in [lt_boolean,lt_void,lt_error]) and (length(reusableLiterals)<2097151) then begin
        setLength(reusableLiterals,length(reusableLiterals)+1);
        reusableLiterals[length(reusableLiterals)-1]:=result;
      end;
    end;

  begin
    setLength(reusableLiterals,0);
    result:=literalFromStream;
    setLength(reusableLiterals,0);
  end;

PROCEDURE writeLiteralToStream(CONST L:P_literal; VAR stream:T_streamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR reusableMap:specialize G_literalKeyMap<longint>;

 PROCEDURE writeLiteral(CONST L:P_literal);
    VAR i:longint;
        reusableIndex:longint;
    begin
      if (L^.literalType=lt_expression) then begin
        if adapters<>nil then adapters^.raiseError('Cannot represent expression literal in binary form!',location);
        exit;
      end;
      reusableIndex:=reusableMap.get(L,2097151);
      if reusableIndex<2097151 then begin
        stream.writeByte(255);
        stream.writeNaturalNumber(reusableIndex);
        exit;
      end;
      stream.writeByte(byte(L^.literalType));
      case L^.literalType of
        lt_boolean:stream.writeBoolean(P_boolLiteral(L)^.val);
        lt_int:stream.writeInt64(P_intLiteral(L)^.val);
        lt_real:stream.writeDouble(P_realLiteral(L)^.val);
        lt_string:stream.writeAnsiString(P_stringLiteral(L)^.val);
        lt_list,
        lt_booleanList,
        lt_intList,
        lt_realList,
        lt_numList,
        lt_stringList,
        lt_emptyList,
        lt_keyValueList,
        lt_flatList,
        lt_listWithError:begin
          stream.writeNaturalNumber(P_listLiteral(L)^.size);
          for i:=0 to P_listLiteral(L)^.size-1 do if (adapters=nil) or (adapters^.noErrors) then writeLiteral(P_listLiteral(L)^.value(i));
        end;
      end;
      if (reusableMap.fill<2097151) and not(L^.literalType in [lt_boolean,lt_void,lt_error]) then begin
        reusableMap.put(L,reusableMap.fill);
      end;
    end;

  begin
    reusableMap.create();
    writeLiteral(L);
    reusableMap.destroy;
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
      {$WARN 5057 ON}
      exponent:=significand;
      significand:=p52+(significand and (p52-1));
      exponent:=(exponent shr 52)-1023-52;

      if isNegative then result:='-' else result:='';
      result:=result+intToStr(significand);
      if exponent<0 then result:=result+'*2^'  +intToStr(exponent)
                    else result:=result+'*2.0^'+intToStr(exponent);
    end;

  VAR i:longint;
  begin
    case L^.literalType of
      lt_int, lt_boolean, lt_string, lt_expression: result:=L^.toString;
      lt_real: result:=representReal(P_realLiteral(L)^.val);
      lt_list..lt_flatList: begin
        result:='[';
        if (P_listLiteral(L)^.datFill>0)      then result:=result+    serialize(P_listLiteral(L)^.dat[0],location,adapters);
        for i:=1 to P_listLiteral(L)^.datFill-1 do result:=result+','+serialize(P_listLiteral(L)^.dat[i],location,adapters);
        result:=result+']';
      end;
      else begin
        adapters^.raiseError('Literal of type '+L^.typeString+' ('+L^.toString+') cannot be serialized',location);
        exit('');
      end;
    end;
  end;

FUNCTION serialize(CONST L:P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters; CONST asExpression:boolean):ansistring;
  VAR wrapper:T_streamWrapper;
      stream:TStringStream;
  begin
    if asExpression then exit(serialize(L,location,adapters));
    stream:= TStringStream.create('');
    wrapper.create(stream);
    writeLiteralToStream(L,wrapper,location,adapters);
    stream.position:=0;
    result:=stream.DataString;
    wrapper.destroy; //implicitly destroys stream
  end;

FUNCTION deserialize(CONST Source:ansistring; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
  VAR wrapper:T_streamWrapper;
      stream:TStringStream;
  begin
    stream:=TStringStream.create(Source);
    wrapper.create(stream);
    stream.position:=0;
    result:=newLiteralFromStream(wrapper,location,adapters);
    wrapper.destroy; //implicitly destroys stream
  end;

VAR i: longint;

INITIALIZATION
  boolLit[false].create(false);
  boolLit[true].create(true);
  errLit.init(lt_error);
  voidLit.create();
  emptyStringLit.create('');
  for i:=low(intLit) to high(intLit) do intLit[i].create(i);
  for i:=0 to 255 do charLit[chr(i)].create(chr(i));
  DefaultFormatSettings.DecimalSeparator:='.';
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  randomize;

FINALIZATION
  boolLit[false].destroy;
  boolLit[true].destroy;
  errLit.destroy;
  voidLit.destroy;
  emptyStringLit.destroy;
  for i:=low(intLit) to high(intLit) do intLit[i].destroy;
  for i:=0 to 255 do charLit[chr(i)].destroy;

end.
