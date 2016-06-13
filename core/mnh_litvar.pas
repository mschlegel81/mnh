// MIT License
//
// Copyright (c) 2016 Martin Schlegel
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

UNIT mnh_litVar;
{$ifdef fullVersion}
{$WARN 3018 OFF}{$WARN 3019 OFF}{$WARN 5024 OFF}
{$endif}
INTERFACE
USES mnh_constants, mnh_out_adapters, sysutils, math, myStringUtil, mnh_tokLoc, typinfo, serializationUtil;
TYPE
  PP_literal = ^P_literal;
  P_literal = ^T_literal;
  T_arrayOfLiteral=array of P_literal;
  T_literal = object
  private
    numberOfReferences: longint;
    CONSTRUCTOR init;
    DESTRUCTOR destroy; virtual;
  public
    PROCEDURE rereference;
    FUNCTION unreference: longint;
    FUNCTION getReferenceCount: longint;

    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
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
    FUNCTION literalType: T_literalType; virtual;
  end;

  P_voidLiteral = ^T_voidLiteral;
  T_voidLiteral = object(T_scalarLiteral)
    private
      CONSTRUCTOR create();
    public
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
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
    CONSTRUCTOR create(CONST value: int64);
  public
    FUNCTION value: int64;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
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
    CONSTRUCTOR create(CONST value: T_myFloat);
  public
    FUNCTION value: T_myFloat;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
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
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
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
    FUNCTION evaluate(CONST parameters:P_listLiteral; CONST context:pointer):P_literal;
    FUNCTION arity:longint;
    //from T_scalarLiteral:
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION typeString:string; virtual;
  end;

  GENERIC G_literalKeyMap<VALUE_TYPE>=object
    TYPE CACHE_ENTRY=record
           key:P_literal;
           value:VALUE_TYPE;
         end;
         KEY_VALUE_LIST=array of CACHE_ENTRY;
    VAR dat:array of KEY_VALUE_LIST;
        fill:longint;
    CONSTRUCTOR create();
    DESTRUCTOR destroy;
    PROCEDURE rehashGrowing;
    FUNCTION put(CONST key:P_literal; CONST value:VALUE_TYPE):boolean;
    FUNCTION get(CONST key:P_literal; CONST fallbackIfNotFound:VALUE_TYPE):VALUE_TYPE;
    FUNCTION keyValueList:KEY_VALUE_LIST;
    FUNCTION keySet:T_arrayOfLiteral;
  end;

  P_literalKeyLongintValueMap=^T_literalKeyLongintValueMap;
  T_literalKeyLongintValueMap=specialize G_literalKeyMap<longint>;
  P_literalKeyLiteralValueMap=^T_literalKeyLiteralValueMap;
  T_literalKeyLiteralValueMap=specialize G_literalKeyMap<P_literal>;

  T_listLiteral = object(T_literal)
  private
    dat: array of P_literal;
    datFill:longint;
    cachedHash: T_hashInt;
    strictType: T_literalType;
    nextAppendIsRange: boolean;

    indexBacking:record
      setBack:P_literalKeyLongintValueMap;
      mapBack:P_literalKeyLiteralValueMap;
    end;

  public
    CONSTRUCTOR create;
    FUNCTION toParameterListString(CONST isFinalized: boolean): ansistring;
    FUNCTION append(CONST L: P_literal; CONST incRefs: boolean):P_listLiteral;
    FUNCTION appendString(CONST s:ansistring):P_listLiteral;
    FUNCTION appendBool  (CONST b:boolean):P_listLiteral;
    FUNCTION appendInt   (CONST i:int64):P_listLiteral;
    FUNCTION appendReal  (CONST r:T_myFloat):P_listLiteral;
    FUNCTION appendAll(CONST L: P_listLiteral):P_listLiteral;
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
    PROCEDURE customSort(CONST leqExpression:P_expressionLiteral; VAR adapters:T_adapters);
    FUNCTION sortPerm: P_listLiteral;
    PROCEDURE unique;
    PROCEDURE toKeyValueList;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION isKeyValuePair: boolean;
    FUNCTION clone:P_listLiteral;
    //from T_literal:
    DESTRUCTOR destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION listConstructorToString:ansistring;
    FUNCTION negate(CONST minusLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal; virtual;
    FUNCTION hash: T_hashInt; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_literal): boolean; virtual;
    FUNCTION contains(CONST other: P_literal): boolean;
    FUNCTION get(CONST other:P_literal; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
    FUNCTION getInner(CONST other:P_literal; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
    FUNCTION typeString:string; virtual;
    FUNCTION parameterListTypeString:string;
  end;

  P_namedVariable=^T_namedVariable;
  T_namedVariable=object
    private
      id:ansistring;
      value:P_literal;
      readonly:boolean;
    public
      CONSTRUCTOR create(CONST initialId:ansistring; CONST initialValue:P_literal; CONST isReadOnly:boolean);
      DESTRUCTOR destroy;
      PROCEDURE setValue(CONST newValue:P_literal; VAR adapters:T_adapters);
      FUNCTION mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
      FUNCTION getId:ansistring;
      FUNCTION getValue:P_literal;
      FUNCTION toString:ansistring;
  end;

  T_variableReport=object
    dat:array of record
          id:ansistring;
          value:P_literal;
          location:string;
        end;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addVariable(CONST id:ansistring; CONST value:P_literal; CONST location:string);
    PROCEDURE addVariable(CONST namedVar:P_namedVariable; CONST location:string);
  end;

  T_disposeSubruleCallback = PROCEDURE(VAR p: pointer);
  T_subruleApplyOpCallback = FUNCTION(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation): pointer;
  T_pointerToStringCallback = FUNCTION(CONST p: pointer): string;
  T_pointerToIntCallback = FUNCTION(CONST p: pointer): longint;
  T_evaluateCompatorCallback = FUNCTION (CONST subruleLiteral:P_expressionLiteral; CONST LHSComparand,RHScomparand:P_literal; VAR adapters:T_adapters):boolean;
  T_evaluateSubruleCallback = FUNCTION(CONST subruleLiteral:P_expressionLiteral;  CONST parameters:P_listLiteral; CONST context:pointer):P_literal;

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
  subruleApplyOpCallback: T_subruleApplyOpCallback;
  evaluateCompatorCallback: T_evaluateCompatorCallback;
  evaluateSubruleCallback:T_evaluateSubruleCallback;

FUNCTION exp(CONST x:double):double; inline;

PROCEDURE disposeLiteral(VAR l: P_literal); inline;
FUNCTION newBoolLiteral(CONST value: boolean): P_boolLiteral; inline;
FUNCTION newIntLiteral(CONST value: int64): P_intLiteral; inline;
FUNCTION newRealLiteral(CONST value: T_myFloat): P_realLiteral; inline;
FUNCTION newStringLiteral(CONST value: ansistring): P_stringLiteral; inline;
FUNCTION newExpressionLiteral(CONST value: pointer): P_expressionLiteral; inline;
FUNCTION newListLiteral: P_listLiteral; inline;
FUNCTION newOneElementListLiteral(CONST value: P_literal; CONST incRefs: boolean): P_listLiteral; inline;
FUNCTION newErrorLiteral: P_scalarLiteral; inline;
FUNCTION newErrorLiteralRaising(CONST errorMessage: ansistring; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
FUNCTION newErrorLiteralRaising(CONST x, y: T_literalType; CONST op: T_tokenType; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_scalarLiteral;
FUNCTION newVoidLiteral: P_voidLiteral; inline;
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters): P_literal;
FUNCTION parseNumber(CONST input: ansistring; CONST offset:longint; CONST suppressOutput: boolean; OUT parsedLength: longint): P_scalarLiteral; inline;

FUNCTION mapPut(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION mapGet(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
FUNCTION mapDrop(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;

FUNCTION messagesToLiteral(CONST messages:T_storedMessages; CONST messageTypeBlackList:T_messageTypeSet=[]):P_listLiteral;
IMPLEMENTATION
VAR
  boolLit: array[false..true] of T_boolLiteral;
  intLit: array[-127..128] of T_intLiteral;
  errLit: T_scalarLiteral;
  voidLit: T_voidLiteral;

FUNCTION messagesToLiteral(CONST messages:T_storedMessages; CONST messageTypeBlackList:T_messageTypeSet=[]):P_listLiteral;
  FUNCTION headByMessageType(CONST messageType:T_messageType):P_listLiteral;
    begin
      if C_errorLevelTxt[messageType]=''
      then result:=newListLiteral^.appendString(copy(getEnumName(TypeInfo(messageType),ord(messageType)),4,1000))
      else result:=newListLiteral^.appendString(C_errorLevelTxt[messageType]);
    end;

  VAR i,j:longint;
  begin
    result:=newListLiteral;
    for i:=0 to length(messages)-1 do with messages[i] do if not(messageType in messageTypeBlackList) then begin
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

FUNCTION newExpressionLiteral(CONST value: pointer): P_expressionLiteral;
  begin
    new(result, create(value));
  end;

FUNCTION newListLiteral: P_listLiteral;
  begin
    new(result, create);
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

CONSTRUCTOR T_variableReport.create;
  begin
    setLength(dat,0);
  end;

DESTRUCTOR T_variableReport.destroy;
  begin
    setLength(dat,0);
  end;

PROCEDURE T_variableReport.addVariable(CONST id: ansistring; CONST value: P_literal; CONST location: string);
  VAR i,j:longint;
  begin
    j:=0;
    for i:=0 to length(dat)-1 do if dat[i].id<>id then begin
      dat[i]:=dat[j];
      inc(j);
    end;
    setLength(dat,j);

    setLength(dat,length(dat)+1);
    dat[length(dat)-1].id:=id;
    dat[length(dat)-1].value:=value;
    dat[length(dat)-1].location:=location;
  end;

PROCEDURE T_variableReport.addVariable(CONST namedVar: P_namedVariable; CONST location: string);
  begin
    addVariable(namedVar^.id,namedVar^.value,location);
  end;

//=====================================================================================================================

CONSTRUCTOR G_literalKeyMap.create();
  VAR i:longint;
  begin
    setLength(dat,256);
    for i:=0 to length(dat)-1 do setLength(dat[i],0);
    fill:=0;
  end;

DESTRUCTOR G_literalKeyMap.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do setLength(dat[i],0);
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
CONSTRUCTOR T_literal.init; begin numberOfReferences:=1; end;

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
    setLength(dat, 0);
    datFill:=0;
    strictType:=lt_emptyList;
    nextAppendIsRange:=false;
    with indexBacking do begin
      setBack:=nil;
      mapBack:=nil;
    end;
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
    for i:=0 to datFill-1 do if dat[i]<>nil then disposeLiteral(dat[i]);
    setLength(dat, 0);
    datFill:=0;
    strictType:=lt_emptyList;
    nextAppendIsRange:=false;
    dropIndexes;
  end;
//==================================================================:DESTRUCTORS
//?.literalType:================================================================
FUNCTION T_literal.literalType: T_literalType; begin result:=lt_error;      end;
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
FUNCTION T_literal.toString: ansistring; begin result:='<ERR>';                      end;
FUNCTION T_voidLiteral      .toString: ansistring; begin result:=C_voidText;                   end;
FUNCTION T_boolLiteral      .toString: ansistring; begin result:=C_boolText[val];              end;
FUNCTION T_intLiteral       .toString: ansistring; begin result:=intToStr(val);                end;
FUNCTION T_realLiteral      .toString: ansistring; begin result:=myFloatToStr(val);            end;
FUNCTION T_stringLiteral    .toString: ansistring; begin result:=escapeString(val);            end;
FUNCTION T_expressionLiteral.toString: ansistring; begin result:=subruleToStringCallback(val); end;
FUNCTION T_listLiteral      .toString: ansistring;
  VAR i: longint;
  begin
    if datFill = 0 then result:='[]'
    else begin
      result:='['+dat[0]^.toString;
      for i:=1 to datFill-1 do
        result:=result+','+dat[i]^.toString;
      result:=result+']';
    end;
  end;

FUNCTION T_listLiteral.listConstructorToString:ansistring;
  VAR i:longint;
  begin
    if datFill = 0 then result:='['
    else begin
      result:='['+dat[0]^.toString;
      for i:=1 to datFill-1 do
        result:=result+','+dat[i]^.toString;
      if nextAppendIsRange then result:=result+'..'
                           else result:=result+',';
    end;
  end;

//===================================================================:?.toString
FUNCTION T_listLiteral.toParameterListString(CONST isFinalized: boolean): ansistring;
  VAR
    i: longint;
  begin
    if datFill = 0 then if isFinalized then exit('()')
                                       else exit('(');
    result:=dat[0]^.toString;
    for i:=1 to datFill-1 do result:=result+','+dat[i]^.toString;
    if isFinalized then result:='('+result+')'
    else result:='('+result+',';
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

FUNCTION T_listLiteral.parameterListTypeString:string;
  VAR i:longint;
  begin
    if datFill<=0 then exit('()');
    result:='('+dat[0]^.typeString;
    for i:=1 to datFill-1 do result:=result+', '+dat[i]^.typeString;
    result:=result+')';
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
    if cachedHash<>0 then exit(cachedHash);
    {$Q-}{$R-}
    result:=T_hashInt(lt_string)+T_hashInt(length(val));
    for i:=1 to length(val) do result:=result*31+ord(val[i]);
    cachedHash:=result;
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
    if cachedHash<>0 then exit(cachedHash);
    {$Q-}{$R-}
    result:=T_hashInt(lt_list)+T_hashInt(datFill);
    for i:=0 to datFill-1 do result:=result*31+dat[i]^.hash;
    cachedHash:=0;
    {$Q+}{$R+}
  end;
//=======================================================================:?.hash
//?.equals:=====================================================================
FUNCTION T_literal.equals(CONST other: P_literal): boolean;
  begin result:=(@self = other) or (other^.literalType = literalType) and (other^.toString=toString);  end;

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

FUNCTION T_listLiteral.equals(CONST other: P_literal): boolean;
  VAR i: longint;
  begin
    if (@self = other) then exit(true);
    if (other^.literalType<>literalType) or (P_listLiteral(other)^.size<>size) then exit(false);
    for i:=0 to datFill-1 do if not (dat[i]^.equals(P_listLiteral(other)^.dat[i])) then exit(false);
    result:=true;
  end;

//=====================================================================:?.equals

FUNCTION T_expressionLiteral.evaluate(CONST parameters:P_listLiteral; CONST context:pointer):P_literal;
  begin
    result:=evaluateSubruleCallback(@self,parameters,context);
  end;

FUNCTION T_expressionLiteral.arity:longint;
  begin
    result:=subruleToArityCallback(val);
  end;

FUNCTION T_listLiteral.contains(CONST other: P_literal): boolean;
  VAR i:longint;
  begin
    with indexBacking do if setBack<>nil then exit(setBack^.get(other,-1)>=0);
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
        if i1 = P_listLiteral(other)^.datFill then
        for i:=0 to P_listLiteral(other)^.datFill-1 do
        if P_boolLiteral(P_listLiteral(other)^.dat[i])^.val then
          P_listLiteral(result)^.append(dat[i], true);
        checkedExit;
      end;
      lt_string: if literalType in [lt_keyValueList,lt_emptyList] then begin
        if indexBacking.mapBack<>nil then begin
          result:=indexBacking.mapBack^.get(other,nil);
          if result=nil then exit(newVoidLiteral);
          result^.rereference;
          exit(result);
        end else begin
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
        if indexBacking.mapBack<>nil then begin
          for j:=0 to P_listLiteral(other)^.size-1 do begin
            L:=indexBacking.mapBack^.get(P_listLiteral(other)^.dat[j],nil);
            if L<>nil then begin
              P_listLiteral(result)^.append(L,true);
              break;
            end;
          end;
        end else begin
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

FUNCTION T_listLiteral.append(CONST L: P_literal; CONST incRefs: boolean): P_listLiteral;
  begin
    result:=@self;
    if L = nil then begin
      raise Exception.create('Trying to append NIL literal to list');
      exit;
    end;
    if L^.literalType=lt_void then exit;
    cachedHash:=0;
    if length(dat)>=datFill then setLength(dat,datFill+16);
    dat[datFill]:=L;
    inc(datFill);
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
    dropIndexes;
  end;

FUNCTION T_listLiteral.appendString(CONST s: ansistring): P_listLiteral;
  begin
    result:=append(newStringLiteral(s),false);
  end;

FUNCTION T_listLiteral.appendBool(CONST b: boolean): P_listLiteral;
  begin
    result:=append(newBoolLiteral(b),false);
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
  begin
    if not (nextAppendIsRange) then begin
      append(L, true);
      exit;
    end;
    nextAppendIsRange:=false;

    if datFill = 0 then begin
      strictType:=lt_listWithError;
      adapters.raiseError('Cannot append range to empty list', tokenLocation);
      exit;
    end;
    last:=dat[datFill-1];
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

PROCEDURE T_listLiteral.dropIndexes;
  begin
    with indexBacking do begin
      if setBack<>nil then dispose(setBack,destroy);
      if mapBack<>nil then dispose(mapBack,destroy);
      setBack:=nil;
      mapBack:=nil;
    end;
  end;

PROCEDURE T_listLiteral.sort;
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  begin
    if (datFill<=1) then exit;
    cachedHash:=0;
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
    dropIndexes;
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
        adapters.raiseError('Invalid sorting index '+intToStr(innerIndex)+' for elements '+a^.toString+' and '+b^.toString,location);
      end;
    end;

  begin
    if datFill<=1 then exit;
    cachedHash:=0;
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
    dropIndexes;
  end;

PROCEDURE T_listLiteral.customSort(CONST leqExpression: P_expressionLiteral; VAR adapters: T_adapters);
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(a,b:P_literal):boolean; inline; begin result:=evaluateCompatorCallback(leqExpression,a,b,adapters); end;

  begin
    if datFill<=1 then exit;
    cachedHash:=0;
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
    dropIndexes;
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
    for i:=0 to length(temp1)-1 do result^.appendInt(temp1 [i].index);
    setLength(temp1, 0);
  end;

PROCEDURE T_listLiteral.unique;
  VAR i, j: longint;
  begin
    with indexBacking do begin
      if setBack<>nil then exit;
      if mapBack<>nil then begin
        dispose(mapBack,destroy);
        mapBack:=nil;
      end;
      sort;
      j:=0;
      new(setBack,create);
      for i:=0 to datFill-1 do
      if setBack^.get(dat[i],-1)=-1
      then begin
        setBack^.put(dat[i],i);
        dat[j]:=dat[i];
        inc(j);
      end else disposeLiteral(dat[i]);
      setLength(dat,j);
      datFill:=j;
    end;
  end;

PROCEDURE T_listLiteral.toKeyValueList;
  VAR i,j:longint;
      key,val:P_literal;
  begin
    with indexBacking do begin
      if (strictType<>lt_keyValueList) or (mapBack<>nil) then exit;
      if setBack<>nil then begin
        dispose(setBack,destroy);
        setBack:=nil;
      end;
      j:=0;
      new(mapBack,create());
      for i:=0 to datFill-1 do begin
        key:=P_listLiteral(dat[i])^.dat[0];
        val:=P_listLiteral(dat[i])^.dat[1];
        if mapBack^.get(key,nil)=nil then begin
          mapBack^.put(key,val);
          dat[j]:=dat[i];
          inc(j);
        end else disposeLiteral(dat[i]);
      end;
      setLength(dat,j);
      datFill:=j;
    end;
  end;

FUNCTION T_listLiteral.isKeyValuePair: boolean;
  begin
    result:=(datFill=2)
        and (dat[0]^.literalType=lt_string);
  end;

FUNCTION T_listLiteral.clone: P_listLiteral;
  VAR i:longint;
  begin
    result:=newListLiteral;
    setLength(result^.dat,datFill);
    result^.datFill:=datFill;
    for i:=0 to datFill-1 do begin
      result^.dat[i]:=dat[i];
      dat[i]^.rereference;
    end;
    result^.strictType:=strictType;
    result^.nextAppendIsRange:=nextAppendIsRange;
    result^.cachedHash:=cachedHash;
    with indexBacking do begin
      if mapBack<>nil then result^.unique;
      if setBack<>nil then result^.toKeyValueList;
    end;
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
          case RHS^.literalType of
            lt_int:        exit(newIntLiteral(P_intLiteral(LHS)^.val+P_intLiteral(RHS)^.val));
            lt_real:       exit(newRealLiteral(P_intLiteral(LHS)^.val+P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorPlus , RHS, tokenLocation)));
          end;
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
        lt_int:
          case RHS^.literalType of
            lt_int:        exit(newIntLiteral(P_intLiteral(LHS)^.val-P_intLiteral(RHS)^.val));
            lt_real:       exit(newRealLiteral(P_intLiteral(LHS)^.val-P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMinus, RHS, tokenLocation)));
          end;
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
          case RHS^.literalType of
            lt_int:        exit(newIntLiteral(P_intLiteral(LHS)^.val*P_intLiteral(RHS)^.val));
            lt_real:       exit(newRealLiteral(P_intLiteral(LHS)^.val*P_realLiteral(RHS)^.val));
            lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, tt_operatorMult, RHS, tokenLocation)));
          end;
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
            lt_int:        try
                             exit(newIntLiteral(P_intLiteral(LHS)^.val div P_intLiteral(RHS)^.val));
                           except
                             exit(newRealLiteral(Nan));
                           end;
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
            lt_int:        try
                             exit(newIntLiteral(P_intLiteral(LHS)^.val mod P_intLiteral(RHS)^.val))
                           except
                             exit(newRealLiteral(Nan));
                           end;
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

CONSTRUCTOR T_namedVariable.create(CONST initialId:ansistring; CONST initialValue:P_literal; CONST isReadOnly:boolean);
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

PROCEDURE T_namedVariable.setValue(CONST newValue:P_literal; VAR adapters:T_adapters);
  begin
    if readonly then raise Exception.create('Mutation of constant "'+id+'" is not allowed.');
    disposeLiteral(value);
    value:=newValue;
    value^.rereference;
  end;

FUNCTION T_namedVariable.mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR adapters:T_adapters):P_literal;
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
    if (params<>nil) and (params^.datFill=3) and
       (params^.dat[0]^.literalType in [lt_keyValueList,lt_emptyList]) and
       (params^.dat[1]^.literalType=lt_string) then begin
      map:=P_listLiteral(params^.dat[0]);
      if map^.numberOfReferences=1
      then map^.rereference
      else map:=map^.clone;
      key:=P_stringLiteral(params^.dat[1]);
      value:=params^.dat[2];
      for i:=0 to map^.datFill-1 do begin
        keyValuePair:=P_listLiteral(map^.dat[i]);
        if keyValuePair^.dat[0]^.equals(key) then begin
          disposeLiteral(keyValuePair^.dat[1]);
          keyValuePair^.dat[1]:=value;
          value^.rereference;
          exit(map);
        end;
      end;
      map^.append(
        newListLiteral^
       .append(key  ,true)^
       .append(value,true),false);
      result:=map;
    end;
  end;

FUNCTION mapGet(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR map,keyValuePair,keyList,fallbackList,resultList:P_listLiteral;
      key:P_stringLiteral;
      fallback:P_literal;
      i:longint;
      back:P_literalKeyLiteralValueMap;
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
        back:=map^.indexBacking.mapBack;
        if back=nil then begin
          tempBack:=true;
          new(back,create);
          for i:=0 to map^.datFill-1 do begin
            keyValuePair:=P_listLiteral(map^.dat[i]);
            back^.put(keyValuePair^.dat[0],keyValuePair^.dat[1]);
          end;
        end;
        resultList:=newListLiteral;
        if (fallback<>nil) and (fallback^.literalType in C_validListTypes) and (P_listLiteral(fallback)^.size=keyList^.size) then begin
          fallbackList:=P_listLiteral(fallback);
          for i:=0 to keyList^.size-1 do resultList^.append(back^.get(keyList^.dat[i],fallbackList^.dat[i]),true);
        end else begin
          for i:=0 to keyList^.size-1 do begin
            result:=back^.get(keyList^.dat[i],fallback);
            if result<>nil then resultList^.append(result,true);
          end;
        end;
        if tempBack then dispose(back,destroy);
        result:=resultList;
      end else begin
        back:=map^.indexBacking.mapBack;
        key:=P_stringLiteral(params^.dat[1]);
        if back=nil then begin
          for i:=0 to map^.datFill-1 do begin
            keyValuePair:=P_listLiteral(map^.dat[i]);
            if keyValuePair^.dat[0]^.equals(key) then begin
              result:=keyValuePair^.dat[1];
              result^.rereference;
              exit(result);
            end;
          end;
        end else begin
          result:=back^.get(key,nil);
          if result<>nil then begin
            result^.rereference;
            exit(result);
          end;
        end;
        if fallback=nil then exit(newListLiteral);
        result:=fallback;
        fallback^.rereference;
      end;
    end;
  end;

FUNCTION mapDrop(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR map,keyValuePair:P_listLiteral;
      key:P_stringLiteral;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.datFill=2) and
       (params^.dat[0]^.literalType in [lt_keyValueList,lt_emptyList]) and
       (params^.dat[1]^.literalType=lt_string) then begin
      map:=P_listLiteral(params^.dat[0]);
      result:=newListLiteral;
      key:=P_stringLiteral(params^.dat[1]);
      for i:=0 to map^.datFill-1 do begin
        keyValuePair:=P_listLiteral(map^.dat[i]);
        if not(keyValuePair^.dat[0]^.equals(key)) then P_listLiteral(result)^.append(keyValuePair,true);
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

FUNCTION newLiteralFromStream(VAR stream:T_streamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
  VAR literalType:T_literalType;
      listSize:longint;
      i:longint;
  begin
    literalType:=T_literalType(stream.readByte);
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
        listSize:=stream.readLongint;
        if listSize<0 then begin
          if adapters<>nil then adapters^.raiseError('Read negative list size! Abort.',location);
          stream.logWrongTypeError;
          exit(newErrorLiteral);
        end;
        result:=newListLiteral;
        setLength(P_listLiteral(result)^.dat,listSize);
        for i:=0 to listSize-1 do if stream.allOkay then P_listLiteral(result)^.append(newLiteralFromStream(stream,location,adapters),false);
        if (result^.literalType<>literalType) and (adapters<>nil) then adapters^.raiseWarning('List has other type than expected.',location);
      end;
      lt_void:result:=newVoidLiteral;
      else begin
        if adapters<>nil then adapters^.raiseError('Read invalid literal type! Abort.',location);
        stream.logWrongTypeError;
        exit(newErrorLiteral);
      end;
    end;
  end;

PROCEDURE writeLiteralToStream(CONST L:P_literal; VAR stream:T_streamWrapper; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR i:longint;
  begin
    if (L^.literalType=lt_expression) then begin
      if adapters<>nil then adapters^.raiseError('Cannot represent expression literal in binary form!',location);
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
        stream.writeLongint(P_listLiteral(L)^.size);
        for i:=0 to P_listLiteral(L)^.size-1 do if (adapters=nil) or (adapters^.noErrors) then writeLiteralToStream(P_listLiteral(L)^.value(i),stream,location,adapters);
      end;
    end;
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
