UNIT mnh_litvar;

INTERFACE

USES mnh_constants, mnh_out_adapters, SysUtils, Math, mnh_stringUtil, mnh_tokloc;

CONST
  C_boolText: array[False..True] of string = ('false', 'true');

TYPE
  PP_literal = ^P_literal;
  P_literal = ^T_literal;

  T_literal = OBJECT
  private
    numberOfReferences: longint;
  public
    CONSTRUCTOR init;
    DESTRUCTOR Destroy; virtual;
    PROCEDURE rereference;
    FUNCTION unreference: longint;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION toShorterString:ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
  end;

  P_scalarLiteral = ^T_scalarLiteral;

  { T_scalarLiteral }

  T_scalarLiteral = OBJECT(T_literal)
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
  end;

  P_boolLiteral = ^T_boolLiteral;

  T_boolLiteral = OBJECT(T_scalarLiteral)
  private
    val: boolean;
  public
    CONSTRUCTOR Create(CONST Value: boolean);
    DESTRUCTOR Destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    FUNCTION Value: boolean;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
  end;

  P_intLiteral = ^T_intLiteral;

  T_intLiteral = OBJECT(T_scalarLiteral)
  private
    val: int64;
  public
    CONSTRUCTOR Create(CONST Value: int64);
    DESTRUCTOR Destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    FUNCTION Value: int64;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
  end;

  P_realLiteral = ^T_realLiteral;

  T_realLiteral = OBJECT(T_scalarLiteral)
  private
    val: extended;
  public
    CONSTRUCTOR Create(CONST Value: extended);
    DESTRUCTOR Destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    FUNCTION Value: extended;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
  end;

  P_stringLiteral = ^T_stringLiteral;

  T_stringLiteral = OBJECT(T_scalarLiteral)
  private
    val: ansistring;
  public
    CONSTRUCTOR Create(CONST Value: ansistring);
    DESTRUCTOR Destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION toShorterString:ansistring; virtual;
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    FUNCTION Value: ansistring;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION softCast: P_scalarLiteral;
    FUNCTION trim: P_stringLiteral;
    FUNCTION upper: P_stringLiteral;
    FUNCTION lower: P_stringLiteral;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
  end;

  P_expressionLiteral = ^T_expressionLiteral;

  T_expressionLiteral = OBJECT(T_scalarLiteral)
  private
    val: pointer;
  public
    CONSTRUCTOR Create(CONST Value: pointer);
    DESTRUCTOR Destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    FUNCTION Value: pointer;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
  end;

  P_listLiteral = ^T_listLiteral;

  { T_listLiteral }

  T_listLiteral = OBJECT(T_literal)
  private
    strictType: T_literalType;
    element: array of P_literal;
    nextAppendIsRange: boolean;
  public
    CONSTRUCTOR Create;
    PROCEDURE destroyChildren;
    DESTRUCTOR Destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION toShorterString:ansistring; virtual;
    FUNCTION toParameterListString(CONST isFinalized: boolean): ansistring;
    PROCEDURE append(CONST L: P_literal; CONST incRefs: boolean);
    PROCEDURE appendAll(CONST L: P_listLiteral);
    PROCEDURE appendConstructing(CONST L: P_literal; CONST tokenLocation: T_tokenLocation);
    PROCEDURE setRangeAppend;
    FUNCTION size: longint;
    FUNCTION Value(index: longint): P_literal;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    PROCEDURE sort;
    FUNCTION sortPerm: P_listLiteral;
    PROCEDURE unique;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION isKeyValueList:boolean;
  end;

TYPE
  T_disposeSubruleCallback = PROCEDURE(VAR p: pointer);
  T_subruleApplyOpCallback = FUNCTION(CONST LHS: P_literal;
    CONST op: T_tokenType; CONST RHS: P_literal;
    CONST location: T_tokenLocation): pointer;
  T_pointerToStringCallback = FUNCTION(CONST p: pointer): string;

VAR
  disposeSubruleCallback: T_disposeSubruleCallback;
  subruleToStringCallback: T_pointerToStringCallback;
  subruleApplyOpCallback: T_subruleApplyOpCallback;

PROCEDURE disposeLiteral(VAR l: P_literal); inline;
FUNCTION newBoolLiteral(CONST Value: boolean): P_boolLiteral; inline;
FUNCTION newIntLiteral(CONST Value: int64): P_intLiteral; inline;
FUNCTION newRealLiteral(CONST Value: extended): P_realLiteral; inline;
FUNCTION newStringLiteral(CONST Value: ansistring): P_stringLiteral; inline;
FUNCTION newExpressionLiteral(CONST Value: pointer): P_expressionLiteral; inline;
FUNCTION newListLiteral: P_listLiteral; inline;
FUNCTION newOneElementListLiteral(CONST Value: P_literal;
  CONST incRefs: boolean): P_listLiteral; inline;
FUNCTION newErrorLiteral: P_scalarLiteral; inline;
FUNCTION newErrorLiteralRaising(CONST errorMessage: ansistring;
  CONST tokenLocation: T_tokenLocation): P_scalarLiteral; inline;
FUNCTION newErrorLiteralRaising(CONST x, y: T_literalType; CONST op: T_tokenType;
  CONST tokenLocation: T_tokenLocation): P_scalarLiteral; inline;
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType;
  CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation): P_literal; inline;
FUNCTION parseNumber(CONST input: ansistring; CONST suppressOutput: boolean;
  OUT parsedLength: longint): P_scalarLiteral; inline;

IMPLEMENTATION

VAR
  boolLit: array[False..True] of T_boolLiteral;
  intLit: array[0..127] of P_intLiteral;
  errLit: T_scalarLiteral;

PROCEDURE disposeLiteral(VAR l: P_literal);
  begin
    if l = nil then
      writeln(stderr, 'disposing NIL literal ?!?');
    if l^.unreference <= 0 then
      dispose(l, Destroy);
    l := nil;
  end;

FUNCTION newBoolLiteral(CONST Value: boolean): P_boolLiteral;
  begin
    result := @boolLit[Value];
    result^.rereference;
  end;

FUNCTION newIntLiteral(CONST Value: int64): P_intLiteral;
  begin
    if (Value >= 0) and (Value < length(intLit)) then
      begin
      if intLit[Value] = nil then
        new(intLit[Value], Create(Value));
      result := intLit[Value];
      result^.rereference;
      end
    else
      begin
      new(result, Create(Value));
      isMemoryFree('allocating new integer literal');
      end;
  end;

FUNCTION newRealLiteral(CONST Value: extended): P_realLiteral;
  begin
    new(result, Create(Value));
    isMemoryFree('allocating new real literal');
  end;

FUNCTION newStringLiteral(CONST Value: ansistring): P_stringLiteral;
  begin
    new(result, Create(Value));
    isMemoryFree('allocating new string literal');
  end;

FUNCTION newExpressionLiteral(CONST Value: pointer): P_expressionLiteral;
  begin
    new(result, Create(Value));
    isMemoryFree('allocating new expression literal');
  end;

FUNCTION newListLiteral: P_listLiteral;
  begin
    new(result, Create);
    isMemoryFree('allocating new list literal');
  end;

FUNCTION newOneElementListLiteral(CONST Value: P_literal;
  CONST incRefs: boolean): P_listLiteral;
  begin
    result := newListLiteral;
    result^.append(Value, incRefs);
    isMemoryFree('allocating new one-element-list literal');
  end;

FUNCTION newErrorLiteral: P_scalarLiteral;
  begin
    result := @errLit;
    errLit.rereference;
  end;

FUNCTION newErrorLiteralRaising(CONST errorMessage: ansistring;
  CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    result := @errLit;
    errLit.rereference;
    raiseError(el2_warning, errorMessage, tokenLocation);
  end;

FUNCTION newErrorLiteralRaising(CONST x, y: T_literalType; CONST op: T_tokenType;
  CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    result := @errLit;
    errLit.rereference;
    raiseError(el2_warning, 'Operator ' + C_tokenString[op] +
      ' is not supported for types ' + C_typeString[x] + ' and ' +
      C_typeString[y], tokenLocation);
  end;

FUNCTION myFloatToStr(CONST x: extended): string;
  begin
    result := FloatToStr(x);
    if (pos('E', UpperCase(result)) <= 0) and //occurs in exponents
      (pos('N', UpperCase(result)) <= 0) and //occurs in "Nan or Inf"
      (pos('.', result) <= 0) then
      result := result + '.0';
  end;

FUNCTION parseNumber(CONST input: ansistring; CONST suppressOutput: boolean;
  OUT parsedLength: longint): P_scalarLiteral;
  VAR
    i: longint;
  begin
    result := nil;
    parsedLength := 0;
    if (length(input) >= 1) and (input[1] in ['0'..'9', '-', '+']) then
      begin
      i := 1;
      while (i < length(input)) and (input[i + 1] in ['0'..'9']) do
        Inc(i);
      parsedLength := i;
      //Only digits on indexes [1..i]; accept decimal point and following digts
      if (i < length(input)) and (input[i + 1] = '.') then
        begin
        Inc(i);
        if (i < length(input)) and (input[i + 1] = '.') then
          Dec(i);
        end;
      while (i < length(input)) and (input[i + 1] in ['0'..'9']) do
        Inc(i);
      //Accept exponent marker and following exponent
      if (i < length(input)) and (input[i + 1] in ['e', 'E']) then
        begin
        Inc(i);
        if (i < length(input)) and (input[i + 1] in ['+', '-']) then
          Inc(i);
        end;
      while (i < length(input)) and (input[i + 1] in ['0'..'9']) do
        Inc(i);
      if i > parsedLength then
        begin
        parsedLength := i;
        if suppressOutput then
          exit(nil);
        result := newRealLiteral(StrToFloatDef(copy(input, 1, parsedLength), NAN));
        end
      else
        begin
        if suppressOutput then
          exit(nil);
        result := newIntLiteral(StrToInt64Def(copy(input, 1, parsedLength), 0));
        end;
      end;
  end;

//=====================================================================================================================
CONSTRUCTOR T_literal.init;
  begin
    numberOfReferences := 1;
  end;

CONSTRUCTOR T_boolLiteral.Create(CONST Value: boolean);
  begin
    INHERITED init;
    val := Value;
  end;

CONSTRUCTOR T_intLiteral.Create(CONST Value: int64);
  begin
    INHERITED init;
    val := Value;
  end;

CONSTRUCTOR T_realLiteral.Create(CONST Value: extended);
  begin
    INHERITED init;
    val := Value;
  end;

CONSTRUCTOR T_stringLiteral.Create(CONST Value: ansistring);
  begin
    INHERITED init;
    val := Value;
  end;

CONSTRUCTOR T_expressionLiteral.Create(CONST Value: pointer);
  begin
    INHERITED init;
    val := Value;
  end;

constructor T_listLiteral.Create;
  begin
    INHERITED init;
    setLength(element, 0);
    strictType := lt_uncheckedList;
    nextAppendIsRange := False;
  end;

DESTRUCTOR T_literal.Destroy;
  begin
  end;

DESTRUCTOR T_boolLiteral.Destroy;
  begin
  end;

DESTRUCTOR T_intLiteral.Destroy;
  begin
  end;

DESTRUCTOR T_realLiteral.Destroy;
  begin
  end;

DESTRUCTOR T_stringLiteral.Destroy;
  begin
  end;

DESTRUCTOR T_expressionLiteral.Destroy;
  begin
    disposeSubruleCallback(val);
  end;

procedure T_listLiteral.destroyChildren;
  VAR
    i: longint;
  begin
    for i := 0 to length(element) - 1 do
      if element[i] <> nil then
        disposeLiteral(element[i]);
    setLength(element, 0);
    strictType := lt_uncheckedList;
    nextAppendIsRange := False;
  end;

destructor T_listLiteral.Destroy;
  begin
    destroyChildren;
  end;

PROCEDURE T_literal.rereference;
  begin
    InterLockedIncrement(numberOfReferences);
  end;

FUNCTION T_literal.unreference: longint;
  begin
    InterLockedDecrement(numberOfReferences);
    result := numberOfReferences;
  end;

FUNCTION T_literal.literalType: T_literalType;
  begin
    result := lt_error;
  end;

FUNCTION T_scalarLiteral.literalType: T_literalType;
  begin
    result := lt_error;
  end;

FUNCTION T_boolLiteral.literalType: T_literalType;
  begin
    result := lt_boolean;
  end;

FUNCTION T_intLiteral.literalType: T_literalType;
  begin
    result := lt_int;
  end;

FUNCTION T_realLiteral.literalType: T_literalType;
  begin
    result := lt_real;
  end;

FUNCTION T_stringLiteral.literalType: T_literalType;
  begin
    result := lt_string;
  end;

FUNCTION T_expressionLiteral.literalType: T_literalType;
  begin
    result := lt_expression;
  end;

function T_listLiteral.literalType: T_literalType;
  VAR
    i: longint;
    containsError: boolean = False;
    allInt: boolean = True;
    allReal: boolean = True;
    allNum: boolean = True;
    allBool: boolean = True;
    allStr: boolean = True;
    allScalar: boolean = True;
  begin
    if strictType <> lt_uncheckedList then
      exit(strictType);
    if length(element) > 0 then
      begin
      for i := 0 to length(element) - 1 do
        if element[i] = nil then
          containsError := True
        else
          case element[i]^.literalType of
            lt_error, lt_listWithError: containsError := True;
            lt_boolean: begin
              allInt := False;
              allReal := False;
              allNum := False;
              allStr := False;
              end;
            lt_int: begin
              allReal := False;
              allBool := False;
              allStr := False;
              end;
            lt_real: begin
              allInt := False;
              allBool := False;
              allStr := False;
              end;
            lt_string: begin
              allInt := False;
              allReal := False;
              allNum := False;
              allBool := False;
              end;
            lt_list..lt_flatList: begin
              allScalar := False;
              allInt := False;
              allReal := False;
              allNum := False;
              allBool := False;
              allStr := False;
              end;
            else begin
              allInt := False;
              allReal := False;
              allNum := False;
              allBool := False;
              allStr := False;
              end;
            end;
      if containsError then
        strictType := lt_listWithError
      else if allInt then
          strictType := lt_intList
        else if allReal then
            strictType := lt_realList
          else if allNum then
              strictType := lt_numList
            else if allBool then
                strictType := lt_booleanList
              else if allStr then
                  strictType := lt_stringList
                else if allScalar then
                    strictType := lt_flatList
                  else
                    strictType := lt_list;
      end
    else
      strictType := lt_list;
    result := strictType;
  end;

FUNCTION T_intLiteral.Value: int64;
  begin
    result := val;
  end;

FUNCTION T_realLiteral.Value: extended;
  begin
    result := val;
  end;

FUNCTION T_stringLiteral.Value: ansistring;
  begin
    result := val;
  end;

FUNCTION T_boolLiteral.Value: boolean;
  begin
    result := val;
  end;

FUNCTION T_expressionLiteral.Value: pointer;
  begin
    result := val;
  end;

function T_listLiteral.size: longint;
  begin
    result := length(element);
  end;

function T_listLiteral.Value(index: longint): P_literal;
  begin
    result := element[index];
  end;


FUNCTION T_literal.toString: ansistring;
  begin
    result := '<ERR>';
  end;

FUNCTION T_literal.toShorterString:ansistring; begin result:=toString; end;

FUNCTION T_scalarLiteral.toString: ansistring;
  begin
    result := '<ERR>';
  end;

FUNCTION T_boolLiteral.toString: ansistring;
  begin
    result := C_boolText[val];
  end;

FUNCTION T_intLiteral.toString: ansistring;
  begin
    result := IntToStr(val);
  end;

FUNCTION T_realLiteral.toString: ansistring;
  begin
    result := myFloatToStr(val);
  end;

FUNCTION T_stringLiteral.toString: ansistring;
  begin
    result := escapeString(val);
  end;

FUNCTION T_stringLiteral.toShorterString:ansistring;
  begin
    if length(val)>13 then result:=escapeString(copy(val,1,5)+'...'+copy(val,length(val)-5,5))
                      else result:=toString;
  end;

FUNCTION T_expressionLiteral.toString: ansistring;
  begin
    result := subruleToStringCallback(val);
  end;

function T_listLiteral.toString: ansistring;
  VAR
    i: longint;
  begin
    if length(element) = 0 then
      result := '[]'
    else
      begin
      result := '[' + element[0]^.toString;
      for i := 1 to length(element) - 1 do
        result := result + ',' + element[i]^.toString;
      result := result + ']';
      end;
  end;

function T_listLiteral.toShorterString: ansistring;
  VAR i: longint;
  begin
    if length(element) = 0 then
      result := '[]'
    else if length(element)<5 then begin
      result := '[' + element[0]^.toShorterString;
      for i := 1 to length(element) - 1 do
        result := result + ',' + element[i]^.toShorterString;
      result := result + ']';
    end else begin
      result := '[' + element[0]^.toShorterString + ',' +
                      element[1]^.toShorterString + ',...,' +
                      element[length(element)-2]^.toShorterString + ','+
                      element[length(element)-1]^.toShorterString + ']';
    end;
  end;

function T_listLiteral.toParameterListString(const isFinalized: boolean
  ): ansistring;
  VAR
    i: longint;
  begin
    if length(element) = 0 then begin
      if isFinalized then exit('()')
                     else exit('(  ');
    end;
    result:=element[0]^.toShorterString;
    for i:=1 to length(element)-1 do
      result:=result+','+element[i]^.toShorterString;
    if isFinalized then result := '(' + result + ')'
                   else result := '(' + result + ',  ';
  end;

FUNCTION T_scalarLiteral.stringForm: ansistring;
  begin
    result := toString;
  end;

FUNCTION T_boolLiteral.stringForm: ansistring;
  begin
    result := toString;
  end;

FUNCTION T_intLiteral.stringForm: ansistring;
  begin
    result := toString;
  end;

FUNCTION T_realLiteral.stringForm: ansistring;
  begin
    result := toString;
  end;

FUNCTION T_stringLiteral.stringForm: ansistring;
  begin
    result := val;
  end;

FUNCTION T_expressionLiteral.stringForm: ansistring;
  begin
    result := toString;
  end;

FUNCTION T_scalarLiteral.isInRelationTo(CONST relation: T_tokenType;
  CONST other: P_scalarLiteral): boolean;
  begin
    result := False;
  end;

FUNCTION T_scalarLiteral.leqForSorting(CONST other: P_scalarLiteral): boolean;
  begin
    result := literalType <= other^.literalType;
  end;

FUNCTION T_boolLiteral.isInRelationTo(CONST relation: T_tokenType;
  CONST other: P_scalarLiteral): boolean;
  VAR
    ovl: boolean;
  begin
    if other^.literalType <> lt_boolean then
      exit(False);
    ovl := P_boolLiteral(other)^.val;
    result := (val = ovl) and (relation in [tt_comparatorEq,
      tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq]) or
      (val < ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq,
      tt_comparatorLss]) or (val > ovl) and
      (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_boolLiteral.leqForSorting(CONST other: P_scalarLiteral): boolean;
  begin
    if other^.literalType = lt_boolean then
      result := isInRelationTo(tt_comparatorLeq, other)
    else
      result := (literalType <= other^.literalType);
  end;

FUNCTION T_intLiteral.isInRelationTo(CONST relation: T_tokenType;
  CONST other: P_scalarLiteral): boolean;
  VAR
    ovi: int64;
    ovr: extended;
  begin
    case other^.literalType of
      lt_int: begin
        ovi := P_intLiteral(other)^.val;
        result := (val = ovi) and
          (relation in [tt_comparatorEq, tt_comparatorListEq,
          tt_comparatorLeq, tt_comparatorGeq]) or (val < ovi) and
          (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
          (val > ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq,
          tt_comparatorGrt]);
        end;
      lt_real: begin
        ovr := P_realLiteral(other)^.val;
        result := (val = ovr) and
          (relation in [tt_comparatorEq, tt_comparatorListEq,
          tt_comparatorLeq, tt_comparatorGeq]) or (val < ovr) and
          (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
          (val > ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq,
          tt_comparatorGrt]);
        end;
      else result := False;
      end;
  end;

FUNCTION T_intLiteral.leqForSorting(CONST other: P_scalarLiteral): boolean;
  begin
    if (other^.literalType in [lt_int, lt_real]) then
      result := isInRelationTo(tt_comparatorLeq, other)
    else
      result := (literalType <= other^.literalType);
  end;

FUNCTION T_realLiteral.isInRelationTo(CONST relation: T_tokenType;
  CONST other: P_scalarLiteral): boolean;
  VAR
    ovi: int64;
    ovr: extended;
  begin
    case other^.literalType of
      lt_int: begin
        ovi := P_intLiteral(other)^.val;
        result := (val = ovi) and
          (relation in [tt_comparatorEq, tt_comparatorListEq,
          tt_comparatorLeq, tt_comparatorGeq]) or (val < ovi) and
          (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
          (val > ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq,
          tt_comparatorGrt]);
        end;
      lt_real: begin
        ovr := P_realLiteral(other)^.val;
        result := (val = ovr) and
          (relation in [tt_comparatorEq, tt_comparatorListEq,
          tt_comparatorLeq, tt_comparatorGeq]) or (val < ovr) and
          (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
          (val > ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq,
          tt_comparatorGrt]);
        end;
      else result := False;
      end;
  end;

FUNCTION T_realLiteral.leqForSorting(CONST other: P_scalarLiteral): boolean;
  begin
    if (other^.literalType in [lt_int, lt_real]) then
      result := isInRelationTo(tt_comparatorLeq, other)
    else
      result := (literalType <= other^.literalType);
  end;

FUNCTION T_stringLiteral.isInRelationTo(CONST relation: T_tokenType;
  CONST other: P_scalarLiteral): boolean;
  VAR
    ovl: ansistring;
  begin
    if other^.literalType <> lt_string then
      exit(False);
    ovl := P_stringLiteral(other)^.val;
    result := (val = ovl) and (relation in [tt_comparatorEq,
      tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq]) or
      (val < ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq,
      tt_comparatorLss]) or (val > ovl) and
      (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_stringLiteral.leqForSorting(CONST other: P_scalarLiteral): boolean;
  begin
    if (other^.literalType = lt_string) then
      result := isInRelationTo(tt_comparatorLeq, other)
    else
      result := (literalType <= other^.literalType);
  end;

FUNCTION T_stringLiteral.softCast: P_scalarLiteral;
  VAR
    len: longint;
    otherVal: ansistring;
  begin
    if lowercase(val) = C_boolText[False] then
      exit(newBoolLiteral(False));
    if lowercase(val) = C_boolText[True] then
      exit(newBoolLiteral(True));
    result := parseNumber(val, False, len);
    if (result <> nil) then
      if (len = length(val)) then
        exit(result)
      else
        disposeLiteral(result);
    otherVal := unescapeString(SysUtils.trim(Value), len);
    if len = length(SysUtils.trim(Value)) then
      exit(newStringLiteral(otherVal));
    result := @self;
    rereference;
  end;

FUNCTION T_stringLiteral.trim: P_stringLiteral;
  VAR
    rs: ansistring;
  begin
    rs := SysUtils.trim(val);
    if rs = val then
      begin
      result := @self;
      rereference;
      end
    else
      result := newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.upper: P_stringLiteral;
  VAR
    rs: string;
  begin
    rs := uppercase(val);
    if rs = val then
      begin
      result := @self;
      rereference;
      end
    else
      result := newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.lower: P_stringLiteral;
  VAR
    rs: string;
  begin
    rs := lowercase(val);
    if rs = val then
      begin
      result := @self;
      rereference;
      end
    else
      result := newStringLiteral(rs);
  end;

FUNCTION T_expressionLiteral.isInRelationTo(CONST relation: T_tokenType;
  CONST other: P_scalarLiteral): boolean;
  begin
    result := False;
  end;

FUNCTION T_expressionLiteral.leqForSorting(CONST other: P_scalarLiteral): boolean;
  begin
    result := literalType <= other^.literalType;
  end;

FUNCTION T_scalarLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral;
  CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    result := @errLit;
    errLit.rereference;
  end;

FUNCTION T_boolLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral;
  CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    if other^.literalType = lt_expression then
      result := newExpressionLiteral(subruleApplyOpCallback(@self,
        op, other, tokenLocation))
    else
      case op of
        tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq,
        tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq: result :=
            newBoolLiteral(isInRelationTo(op, other));
        tt_operatorAnd: if other^.literalType = lt_boolean then
            result := newBoolLiteral(val and P_boolLiteral(other)^.val)
          else
            result := newErrorLiteralRaising(literalType, other^.literalType,
              op, tokenLocation);
        tt_operatorOr: if other^.literalType = lt_boolean then
            result := newBoolLiteral(val or P_boolLiteral(other)^.val)
          else
            result := newErrorLiteralRaising(literalType, other^.literalType,
              op, tokenLocation);
        tt_operatorXor: if other^.literalType = lt_boolean then
            result := newBoolLiteral(val xor P_boolLiteral(other)^.val)
          else
            result := newErrorLiteralRaising(literalType, other^.literalType,
              op, tokenLocation);
        tt_operatorStrConcat: result := newStringLiteral(stringForm + other^.stringForm);
        else result := newErrorLiteralRaising(literalType, other^.literalType,
            op, tokenLocation);
        end;
  end;

FUNCTION T_intLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral;
  CONST tokenLocation: T_tokenLocation): P_scalarLiteral;

  FUNCTION pot_int_int(x, y: int64): P_scalarLiteral;
    VAR
      temp: int64;
      tx, rx: extended;
    begin
      if y >= 0 then
        begin
        temp := 1;
        while y > 0 do
          begin
          if odd(y) then
            temp := temp * x;
          x := int64(x) * int64(x);
          y := y shr 1;
          end;
        result := newIntLiteral(temp);
        end
      else
        begin
        rx := 1 / x;
        tx := 1;
        y := -y;
        while y > 0 do
          begin
          if odd(y) then
            tx := tx * rx;
          rx := rx * rx;
          y := y shr 1;
          end;
        result := newRealLiteral(tx);
        end;
    end;

  begin
    if other^.literalType = lt_expression then
      result := newExpressionLiteral(subruleApplyOpCallback(@self,
        op, other, tokenLocation))
    else
      case op of
        tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq,
        tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq: result :=
            newBoolLiteral(isInRelationTo(op, other));
        tt_operatorAnd: if other^.literalType = lt_int then
            result := newIntLiteral(val and P_intLiteral(other)^.val)
          else
            result := newErrorLiteralRaising(literalType, other^.literalType,
              op, tokenLocation);
        tt_operatorOr: if other^.literalType = lt_int then
            result := newIntLiteral(val or P_intLiteral(other)^.val)
          else
            result := newErrorLiteralRaising(literalType, other^.literalType,
              op, tokenLocation);
        tt_operatorXor: if other^.literalType = lt_int then
            result := newIntLiteral(val xor P_intLiteral(other)^.val)
          else
            result := newErrorLiteralRaising(literalType, other^.literalType,
              op, tokenLocation);
        tt_operatorPlus: case other^.literalType of
            lt_int: result := newIntLiteral(val + P_intLiteral(other)^.val);
            lt_real: result := newRealLiteral(val + P_realLiteral(other)^.val);
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorMinus: case other^.literalType of
            lt_int: result := newIntLiteral(val - P_intLiteral(other)^.val);
            lt_real: result := newRealLiteral(val - P_realLiteral(other)^.val);
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorMult: case other^.literalType of
            lt_int: result := newIntLiteral(val * P_intLiteral(other)^.val);
            lt_real: result := newRealLiteral(val * P_realLiteral(other)^.val);
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorDivReal: case other^.literalType of
            lt_int: result := newRealLiteral(val / P_intLiteral(other)^.val);
            lt_real: result := newRealLiteral(val / P_realLiteral(other)^.val);
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorDivInt: if other^.literalType = lt_int then
              try
              result :=
                newIntLiteral(val div P_intLiteral(other)^.val);
              except
              raiseError(
                el1_note, 'WARN: Integer division by zero; returning Nan',
                tokenLocation);
              result := newRealLiteral(Nan);
              end
          else
            result := newErrorLiteralRaising(literalType, other^.literalType,
              op, tokenLocation);
        tt_operatorMod: if other^.literalType = lt_int then
              try
              result :=
                newIntLiteral(val mod P_intLiteral(other)^.val)
              except
              raiseError(
                el1_note, 'WARN: Integer division by zero; returning Nan',
                tokenLocation);
              result := newRealLiteral(Nan);
              end
          else
            result := newErrorLiteralRaising(literalType, other^.literalType,
              op, tokenLocation);
        tt_operatorPot: case other^.literalType of
            lt_int: result := pot_int_int(val, P_intLiteral(other)^.val);
            lt_real: result := newRealLiteral(exp(ln(val) * P_realLiteral(other)^.val));
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorStrConcat: result := newStringLiteral(stringForm + other^.stringForm);
        else result := newErrorLiteralRaising(literalType, other^.literalType,
            op, tokenLocation);
        end;
  end;

FUNCTION T_realLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral;
  CONST tokenLocation: T_tokenLocation): P_scalarLiteral;

  FUNCTION pot_real_int(x: extended; y: longint): extended;
    begin
      if y < 0 then
        begin
        y := -y;
        x := 1 / x;
        end;
      result := 1;
      while y > 0 do
        begin
        if odd(y) then
          result := result * x;
        x := x * x;
        y := y shr 1;
        end;
    end;

  begin
    if other^.literalType = lt_expression then
      result := newExpressionLiteral(subruleApplyOpCallback(@self,
        op, other, tokenLocation))
    else
      case op of
        tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq,
        tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq: result :=
            newBoolLiteral(isInRelationTo(op, other));
        tt_operatorPlus: case other^.literalType of
            lt_int: result := newRealLiteral(val + P_intLiteral(other)^.val);
            lt_real: result := newRealLiteral(val + P_realLiteral(other)^.val);
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorMinus: case other^.literalType of
            lt_int: result := newRealLiteral(val - P_intLiteral(other)^.val);
            lt_real: result := newRealLiteral(val - P_realLiteral(other)^.val);
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorMult: case other^.literalType of
            lt_int: result := newRealLiteral(val * P_intLiteral(other)^.val);
            lt_real: result := newRealLiteral(val * P_realLiteral(other)^.val);
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorDivReal: case other^.literalType of
            lt_int: result := newRealLiteral(val / P_intLiteral(other)^.val);
            lt_real: result := newRealLiteral(val / P_realLiteral(other)^.val);
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorPot: case other^.literalType of
            lt_int: result :=
                newRealLiteral(pot_real_int(val, P_intLiteral(other)^.val));
            lt_real: result := newRealLiteral(exp(ln(val) * P_realLiteral(other)^.val));
            else result := newErrorLiteralRaising(literalType,
                other^.literalType, op, tokenLocation);
            end;
        tt_operatorStrConcat: result := newStringLiteral(stringForm + other^.stringForm);
        else result := newErrorLiteralRaising(literalType, other^.literalType,
            op, tokenLocation);
        end;
  end;

FUNCTION T_stringLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral;
  CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    if other^.literalType = lt_expression then
      result := newExpressionLiteral(subruleApplyOpCallback(@self,
        op, other, tokenLocation))
    else
      case op of
        tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq,
        tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq: result :=
            newBoolLiteral(isInRelationTo(op, other));
        tt_operatorPlus: if other^.literalType = lt_string then
            result := newStringLiteral(val + P_stringLiteral(other)^.val)
          else
            result := newErrorLiteralRaising(literalType, other^.literalType,
              op, tokenLocation);
        tt_operatorStrConcat: result := newStringLiteral(stringForm + other^.stringForm);
        else result := newErrorLiteralRaising(literalType, other^.literalType,
            op, tokenLocation);
        end;
  end;

FUNCTION T_expressionLiteral.operate(CONST op: T_tokenType;
  CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    result := newExpressionLiteral(subruleApplyOpCallback(@self, op,
      other, tokenLocation));
  end;

procedure T_listLiteral.append(const L: P_literal; const incRefs: boolean);
  begin
    if L = nil then
      begin
      raiseError(el3_evalError, 'Trying to append NIL literal to list',
        C_nilTokenLocation);
      exit;
      end;
    setLength(element, length(element) + 1);
    element[length(element) - 1] := L;
    if incRefs then
      L^.rereference;
    strictType := lt_uncheckedList;
  end;

procedure T_listLiteral.appendAll(const L: P_listLiteral);
  VAR
    i: longint;
  begin
    for i := 0 to length(L^.element) - 1 do
      append(L^.element[i], True);
  end;

procedure T_listLiteral.appendConstructing(const L: P_literal;
  const tokenLocation: T_tokenLocation);
  VAR
    last: P_literal;
    i0, i1: int64;
    c0, c1: char;
  begin
    if not (nextAppendIsRange) then
      begin
      append(L, True);
      exit;
      end;
    nextAppendIsRange := False;

    if length(element) = 0 then
      begin
      strictType := lt_listWithError;
      raiseError(el3_evalError, 'Cannot append range to empty list', tokenLocation);
      exit;
      end;
    last := element[length(element) - 1];
    if (last^.literalType = lt_int) and (L^.literalType = lt_int) then
      begin
      i0 := P_intLiteral(last)^.val;
      i1 := P_intLiteral(L)^.val;
      while (i0 < i1) and (errorLevel < el3_evalError) do
        begin
        Inc(i0);
        append(newIntLiteral(i0), False);
        end;
      while (i0 > i1) and (errorLevel < el3_evalError) do
        begin
        Dec(i0);
        append(newIntLiteral(i0), False);
        end;
      end
    else if (last^.literalType = lt_string) and
        (length(P_stringLiteral(last)^.val) = 1) and (L^.literalType = lt_string) and
        (length(P_stringLiteral(L)^.val) = 1) then
        begin
        c0 := P_stringLiteral(last)^.val[1];
        c1 := P_stringLiteral(L)^.val[1];
        while c0 < c1 do
          begin
          Inc(c0);
          append(newStringLiteral(c0), False);
          end;
        while c0 > c1 do
          begin
          Dec(c0);
          append(newStringLiteral(c0), False);
          end;
        end
      else
        begin
        strictType := lt_listWithError;
        raiseError(el3_evalError, 'Invalid range expression ' +
          last^.toString + '..' + L^.toString, tokenLocation);
        end;
  end;

procedure T_listLiteral.setRangeAppend;
  begin
    nextAppendIsRange := True;
  end;

procedure T_listLiteral.sort;
  VAR
    temp: array of P_scalarLiteral;
    scale: longint;
    i, j0, j1, k: longint;
  begin
    scale := 1;
    setLength(temp, length(element));
    while scale < length(element) do
      begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i := 0;
      while i < length(element) do
        begin
        j0 := i;
        j1 := i + scale;
        k := i;
        while (j0 < i + scale) and (j1 < i + scale + scale) and (j1 < length(element)) do
          if P_scalarLiteral(element[j0])^.leqForSorting(P_scalarLiteral(element[j1]))
          then
            begin
            temp[k] := P_scalarLiteral(element[j0]);
            Inc(k);
            Inc(j0);
            end
          else
            begin
            temp[k] := P_scalarLiteral(element[j1]);
            Inc(k);
            Inc(j1);
            end;
        while (j0 < i + scale) and (j0 < length(element)) do
          begin
          temp[k] := P_scalarLiteral(element[j0]);
          Inc(k);
          Inc(j0);
          end;
        while (j1 < i + scale + scale) and (j1 < length(element)) do
          begin
          temp[k] := P_scalarLiteral(element[j1]);
          Inc(k);
          Inc(j1);
          end;
        Inc(i, scale + scale);
        end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      Inc(scale, scale);
      if (scale < length(element)) then
        begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i := 0;
        while i < length(element) do
          begin
          j0 := i;
          j1 := i + scale;
          k := i;
          while (j0 < i + scale) and (j1 < i + scale + scale) and
            (j1 < length(element)) do
            if temp[j0]^.leqForSorting(temp[j1]) then
              begin
              element[k] := temp[j0];
              Inc(k);
              Inc(j0);
              end
            else
              begin
              element[k] := temp[j1];
              Inc(k);
              Inc(j1);
              end;
          while (j0 < i + scale) and (j0 < length(element)) do
            begin
            element[k] := temp[j0];
            Inc(k);
            Inc(j0);
            end;
          while (j1 < i + scale + scale) and (j1 < length(element)) do
            begin
            element[k] := temp[j1];
            Inc(k);
            Inc(j1);
            end;
          Inc(i, scale + scale);
          end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        Inc(scale, scale);
        end
      else
        for k := 0 to length(element) - 1 do
          element[k] := temp[k];
      end;
    setLength(temp, 0);
  end;

function T_listLiteral.sortPerm: P_listLiteral;
  VAR
    temp1, temp2: array of record
      v: P_scalarLiteral;
      index: longint;
      end;
    scale: longint;
    i, j0, j1, k: longint;

  begin
    setLength(temp1, length(element));
    setLength(temp2, length(element));
    for i := 0 to length(element) - 1 do
      WITH temp1[i] do
        begin
        v := P_scalarLiteral(element[i]);
        index := i;
        end;
    scale := 1;
    while scale < length(temp1) do
      begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i := 0;
      while i < length(temp1) do
        begin
        j0 := i;
        j1 := i + scale;
        k := i;
        while (j0 < i + scale) and (j1 < i + scale + scale) and (j1 < length(temp1)) do
          if temp1[j0].v^.leqForSorting(temp1[j1].v) then
            begin
            temp2[k] := temp1[j0];
            Inc(k);
            Inc(j0);
            end
          else
            begin
            temp2[k] := temp1[j1];
            Inc(k);
            Inc(j1);
            end;
        while (j0 < i + scale) and (j0 < length(temp1)) do
          begin
          temp2[k] := temp1[j0];
          Inc(k);
          Inc(j0);
          end;
        while (j1 < i + scale + scale) and (j1 < length(temp1)) do
          begin
          temp2[k] := temp1[j1];
          Inc(k);
          Inc(j1);
          end;
        Inc(i, scale + scale);
        end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      Inc(scale, scale);
      if (scale < length(temp1)) then
        begin
        i := 0;
        while i < length(temp1) do
          begin
          j0 := i;
          j1 := i + scale;
          k := i;
          while (j0 < i + scale) and (j1 < i + scale + scale) and (j1 < length(temp1)) do
            if temp2[j0].v^.leqForSorting(temp2[j1].v) then
              begin
              temp1[k] := temp2[j0];
              Inc(k);
              Inc(j0);
              end
            else
              begin
              temp1[k] := temp2[j1];
              Inc(k);
              Inc(j1);
              end;
          while (j0 < i + scale) and (j0 < length(temp1)) do
            begin
            temp1[k] := temp2[j0];
            Inc(k);
            Inc(j0);
            end;
          while (j1 < i + scale + scale) and (j1 < length(temp1)) do
            begin
            temp1[k] := temp2[j1];
            Inc(k);
            Inc(j1);
            end;
          Inc(i, scale + scale);
          end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        Inc(scale, scale);
        end
      else
        for k := 0 to length(temp1) - 1 do
          temp1[k] := temp2[k];
      end;
    setLength(temp2, 0);
    result := newListLiteral;
    for i := 0 to length(temp1) - 1 do
      result^.append(newIntLiteral(temp1[i].index), False);
    setLength(temp1, 0);
  end;

procedure T_listLiteral.unique;
  VAR
    i, j: longint;
  begin
    if literalType in [lt_list, lt_listWithError, lt_uncheckedList] then
      exit;
    sort;
    i := 0;
    for j := 1 to length(element) - 1 do
      if (P_scalarLiteral(element[i])^.isInRelationTo(tt_comparatorEq,
        P_scalarLiteral(element[j]))) then
        disposeLiteral(element[j])
      else
        begin
        Inc(i);
        element[i] := element[j];
        end;
    setLength(element, i + 1);
  end;


function T_listLiteral.isKeyValueList: boolean;
  VAR i:longint;
  begin
    if (literalType<>lt_list) or (length(element)<=0) then exit(false);
    for i:=0 to length(element)-1 do if not(
      (element[i]^.literalType in [lt_list..lt_listWithError]) and
      (P_listLiteral(element[i])^.size=2) and
      (P_listLiteral(element[i])^.Value(0)^.literalType=lt_string))
    then exit(false);
    result:=true;
  end;

FUNCTION T_literal.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin
    result := @self;
    rereference;
  end;

FUNCTION T_scalarLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin
    result := @self;
    rereference;
  end;

FUNCTION T_stringLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin
    result := newErrorLiteralRaising('Cannot negate string.', minusLocation);
  end;

FUNCTION T_boolLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin
    result := newErrorLiteralRaising('Cannot negate boolean.', minusLocation);
  end;

FUNCTION T_intLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin
    result := newIntLiteral(-Value);
  end;

FUNCTION T_realLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin
    result := newRealLiteral(-Value);
  end;

FUNCTION T_expressionLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin
    result := newErrorLiteralRaising(
      'Cannot negate expression. Please use "-1*..." instead.', minusLocation);
  end;

function T_listLiteral.negate(const minusLocation: T_tokenLocation): P_literal;
  VAR
    res: P_listLiteral;
    i: longint;
  begin
    res := newListLiteral;
    for i := 0 to length(element) - 1 do
      res^.append(element[i]^.negate(minusLocation), False);
    result := res;
  end;

FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType;
  CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation): P_literal;

  FUNCTION equals(CONST LHS, RHS: P_literal): boolean;
    VAR
      i: longint;
    begin
      if LHS = RHS then
        exit(True);
      case LHS^.literalType of
        lt_int, lt_real, lt_boolean, lt_string, lt_expression: if RHS^.literalType in
            [lt_int, lt_real, lt_boolean, lt_string, lt_expression] then
            exit(P_scalarLiteral(LHS)^.isInRelationTo(tt_comparatorEq,
              P_scalarLiteral(RHS)))
          else
            exit(False);
        lt_list..lt_flatList: if (RHS^.literalType in [lt_list..lt_flatList]) and
            (length(P_listLiteral(LHS)^.element) =
            length(P_listLiteral(RHS)^.element)) then
            begin
            result := True;
            i := 0;
            while result and (i < length(P_listLiteral(LHS)^.element)) do
              begin
              result := result and equals(P_listLiteral(LHS)^.element[i],
                P_listLiteral(RHS)^.element[i]);
              Inc(i);
              end;
            end
          else
            exit(False);
        else exit(False);
        end;
    end;

  FUNCTION isContained(CONST LHS, RHS: P_literal): boolean;
    VAR
      i: longint;
    begin
      result := False;
      if RHS^.literalType in [lt_list..lt_flatList] then
        begin
        i := 0;
        while (i < length(P_listLiteral(RHS)^.element)) and not (result) do
          begin
          result := result or equals(LHS, P_listLiteral(RHS)^.element[i]);
          Inc(i);
          end;
        end;
    end;

  VAR
    i, i1, j: longint;
    key:ansistring;
  begin
    //writeln('resolving operator ',op);
    //writeln('             LHS = @',ptrint(LHS));
    //writeln('             RHS = @',ptrint(RHS));
    //HANDLE ERROR LITERALS:---------------------------------------------------
    if (LHS^.literalType = lt_error) then
      begin
      LHS^.rereference;
      exit(LHS);
      end;
    if (RHS^.literalType = lt_error) then
      begin
      RHS^.rereference;
      exit(RHS);
      end;
    //---------------------------------------------------:HANDLE ERROR LITERALS
    //HANDLE EXPRESSION LITERALS:----------------------------------------------
    if (LHS^.literalType = lt_expression) or (RHS^.literalType = lt_expression) then
      exit(newExpressionLiteral(subruleApplyOpCallback(LHS, op, RHS, tokenLocation)));
    //----------------------------------------------:HANDLE EXPRESSION LITERALS
    //HANDLE S x S -> S OPERATORS:---------------------------------------------
    if (op in [tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq,
      tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_operatorAnd,
      tt_operatorOr, tt_operatorXor, tt_operatorPlus, tt_operatorMinus,
      tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt,
      tt_operatorMod, tt_operatorPot, tt_operatorStrConcat]) then
      case LHS^.literalType of
        lt_boolean, lt_int, lt_real, lt_string, lt_expression: case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string, lt_expression:
              exit(P_scalarLiteral(LHS)^.operate(op, P_scalarLiteral(RHS), tokenLocation));
            //scalar X scalar

            lt_list: begin
              //scalar X nested list
              result := newListLiteral;
              for i := 0 to length(P_listLiteral(RHS)^.element) - 1 do
                P_listLiteral(result)^.append(
                  resolveOperator(LHS, op, P_listLiteral(RHS)^.element[i],
                  tokenLocation),
                  False);
              exit(result);
              end;
            lt_booleanList, lt_intList, lt_realList, lt_numList,
            lt_stringList, lt_flatList: begin
              //scalar X flat list
              result := newListLiteral;
              for i := 0 to length(P_listLiteral(RHS)^.element) - 1 do
                P_listLiteral(result)^.append(
                  P_scalarLiteral(LHS)^.operate(op, P_scalarLiteral(
                  P_listLiteral(RHS)^.element[i]), tokenLocation),
                  False);
              exit(result);
              end;
            else exit(newErrorLiteralRaising(LHS^.literalType,
                RHS^.literalType, op, tokenLocation));
            end;
        lt_list: case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string, lt_expression: begin
              //nested list X scalar
              result := newListLiteral;
              for i := 0 to length(P_listLiteral(LHS)^.element) - 1 do
                P_listLiteral(result)^.append(
                  resolveOperator(P_listLiteral(LHS)^.element[i], op,
                  RHS, tokenLocation),
                  False);
              exit(result);
              end;
            lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList,
            lt_stringList, lt_flatList: begin
              //nested list X flat/nested list
              i := length(P_listLiteral(LHS)^.element);
              i1 := length(P_listLiteral(RHS)^.element);
              if i = i1 then
                begin
                result := newListLiteral;
                for i := 0 to i1 - 1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.element[i], op,
                    P_listLiteral(RHS)^.element[i], tokenLocation), False);
                exit(result);
                end
              else
                exit(newErrorLiteralRaising('Invalid list lengths ' +
                  IntToStr(i) + ' and ' + IntToStr(i1) + ' given for operator ' +
                  C_tokenString[op], tokenLocation));
              end;
            else exit(newErrorLiteralRaising(LHS^.literalType,
                RHS^.literalType, op, tokenLocation));
            end;
        lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList,
        lt_flatList: case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string, lt_expression: begin
              //flat list X scalar
              result := newListLiteral;
              for i := 0 to length(P_listLiteral(LHS)^.element) - 1 do
                P_listLiteral(result)^.append(
                  P_scalarLiteral(P_listLiteral(LHS)^.element[i])^.operate(
                  op, P_scalarLiteral(RHS), tokenLocation),
                  False);
              exit(result);
              end;
            lt_list: begin
              //flat list X nested list
              i := length(P_listLiteral(LHS)^.element);
              i1 := length(P_listLiteral(RHS)^.element);
              if i = i1 then
                begin
                result := newListLiteral;
                for i := 0 to i1 - 1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.element[i], op,
                    P_listLiteral(RHS)^.element[i], tokenLocation), False);
                exit(result);
                end
              else
                exit(newErrorLiteralRaising('Invalid list lengths ' +
                  IntToStr(i) + ' and ' + IntToStr(i1) + ' given for operator ' +
                  C_tokenString[op], tokenLocation));
              end;
            lt_booleanList, lt_intList, lt_realList, lt_numList,
            lt_stringList, lt_flatList: begin
              //flat list X flat list
              i := length(P_listLiteral(LHS)^.element);
              i1 := length(P_listLiteral(RHS)^.element);
              if i = i1 then
                begin
                result := newListLiteral;
                for i := 0 to i1 - 1 do
                  P_listLiteral(result)^.append(
                    P_scalarLiteral(P_listLiteral(LHS)^.element[i])^.operate(op,
                    P_scalarLiteral(P_listLiteral(RHS)^.element[i]),
                    tokenLocation), False);
                exit(result);
                end
              else
                exit(newErrorLiteralRaising('Invalid list lengths ' +
                  IntToStr(i) + ' and ' + IntToStr(i1) + ' given for operator ' +
                  C_tokenString[op], tokenLocation));
              end;
            else exit(newErrorLiteralRaising(LHS^.literalType,
                RHS^.literalType, op, tokenLocation));
            end;
        else exit(newErrorLiteralRaising(LHS^.literalType, RHS^.literalType,
            op, tokenLocation));
        end;
    //---------------------------------------------:HANDLE S x S -> S OPERATORS
    case op of
      tt_operatorConcat: begin
        result := newListLiteral;
        if (LHS^.literalType in [lt_error, lt_boolean, lt_int, lt_real,
          lt_string, lt_expression]) then
          P_listLiteral(result)^.append(LHS, True)
        else
          P_listLiteral(result)^.appendAll(P_listLiteral(LHS));
        if (RHS^.literalType in [lt_error, lt_boolean, lt_int, lt_real,
          lt_string, lt_expression]) then
          P_listLiteral(result)^.append(RHS, True)
        else
          P_listLiteral(result)^.appendAll(P_listLiteral(RHS));
        exit(result);
        end;
      tt_comparatorListEq: exit(newBoolLiteral(equals(LHS, RHS)));
      tt_operatorIn: exit(newBoolLiteral(isContained(LHS, RHS)));
      tt_operatorExtractL0: if LHS^.literalType in [lt_list..lt_flatList] then
          case RHS^.literalType of
            lt_int: begin
              i1 := length(P_listLiteral(LHS)^.element);
              i := P_intLiteral(RHS)^.val;
              if (i >= 0) and (i < i1) then
                begin
                result := P_listLiteral(LHS)^.element[i];
                result^.rereference;
                exit(result);
                end
              else
                exit(newListLiteral);
              end;
            lt_intList: begin
              result := newListLiteral;
              i1 := length(P_listLiteral(LHS)^.element);
              for j := 0 to length(P_listLiteral(RHS)^.element) - 1 do
                begin
                i := P_intLiteral(P_listLiteral(RHS)^.element[j])^.val;
                if (i >= 0) and (i < i1) then
                  P_listLiteral(result)^.append(P_listLiteral(LHS)^.element[i], True);
                end;
              exit(result);
              end;
            lt_booleanList: begin
              result := newListLiteral;
              i1 := length(P_listLiteral(LHS)^.element);
              if i1 = length(P_listLiteral(RHS)^.element) then
                for i := 0 to length(P_listLiteral(RHS)^.element) - 1 do
                  if P_boolLiteral(P_listLiteral(RHS)^.element[i])^.val then
                    P_listLiteral(result)^.append(P_listLiteral(LHS)^.element[i], True);
              exit(result);
              end;
            lt_string: if P_listLiteral(LHS)^.isKeyValueList then begin
              key:=P_stringLiteral(RHS)^.Value;
              for i:=0 to length(P_listLiteral(LHS)^.element)-1 do
              if P_stringLiteral(P_listLiteral(P_listLiteral(LHS)^.element[i])^.element[0])^.Value = key then begin
                result:=P_listLiteral(P_listLiteral(LHS)^.element[i])^.element[1];
                result^.rereference;
                exit(result);
              end;
              exit(newListLiteral);
            end else exit(newErrorLiteralRaising('Operator % with a string as second operand can only be applied to key-value-lists!',tokenLocation));
            lt_stringList: if P_listLiteral(LHS)^.isKeyValueList then begin
              result:=newListLiteral;
              for j:=0 to P_listLiteral(RHS)^.size-1 do begin
                key:=P_stringLiteral(P_listLiteral(RHS)^.element[j])^.Value;
                i:=0; while i<length(P_listLiteral(LHS)^.element)-1 do
                if P_stringLiteral(P_listLiteral(P_listLiteral(LHS)^.element[i])^.element[0])^.Value = key then begin
                  P_listLiteral(result)^.append(P_listLiteral(P_listLiteral(LHS)^.element[i])^.element[1],true);
                  i:=length(P_listLiteral(LHS)^.element);
                end else inc(i);
              end;
              exit(result);
            end else exit(newErrorLiteralRaising('Operator % with a stringList as second operand can only be applied to key-value-lists!',tokenLocation));

            lt_list, lt_flatList: if P_listLiteral(RHS)^.size = 0 then
                exit(newListLiteral)
              else
                exit(newErrorLiteralRaising(LHS^.literalType,
                  RHS^.literalType, op, tokenLocation));
            else exit(newErrorLiteralRaising(LHS^.literalType,
                RHS^.literalType, op, tokenLocation));
            end
        else
          exit(newErrorLiteralRaising(LHS^.literalType, RHS^.literalType,
            op, tokenLocation));
      tt_operatorExtractL1: if LHS^.literalType in [lt_list..lt_flatList] then
          begin
          result := newListLiteral;
          for i := 0 to length(P_listLiteral(LHS)^.element) - 1 do
            P_listLiteral(result)^.append(resolveOperator(
              P_listLiteral(LHS)^.element[i], tt_operatorExtractL0,
              RHS, tokenLocation), False);
          exit(result);
          end
        else
          exit(newErrorLiteralRaising(LHS^.literalType, RHS^.literalType,
            op, tokenLocation));
      tt_operatorExtractL2: if LHS^.literalType in [lt_list..lt_flatList] then
          begin
          result := newListLiteral;
          for i := 0 to length(P_listLiteral(LHS)^.element) - 1 do
            P_listLiteral(result)^.append(resolveOperator(
              P_listLiteral(LHS)^.element[i], tt_operatorExtractL1,
              RHS, tokenLocation), False);
          exit(result);
          end
        else
          exit(newErrorLiteralRaising(LHS^.literalType, RHS^.literalType,
            op, tokenLocation));
      tt_operatorExtractL3: if LHS^.literalType in [lt_list..lt_flatList] then
          begin
          result := newListLiteral;
          for i := 0 to length(P_listLiteral(LHS)^.element) - 1 do
            P_listLiteral(result)^.append(resolveOperator(
              P_listLiteral(LHS)^.element[i], tt_operatorExtractL2,
              RHS, tokenLocation), False);
          exit(result);
          end
        else
          exit(newErrorLiteralRaising(LHS^.literalType, RHS^.literalType,
            op, tokenLocation));
      end;
  end;

FUNCTION T_literal.hash: longint;
  begin
    result := -1;
  end;

FUNCTION T_scalarLiteral.hash: longint;
  begin
    result := longint(lt_error);
  end;

FUNCTION T_boolLiteral.hash: longint;
  begin
    result := longint(lt_boolean);
    if val then Inc(result);
  end;

FUNCTION T_intLiteral.hash: longint;
  begin
    result := longint(lt_int) xor longint(val);
  end;

FUNCTION T_realLiteral.hash: longint;
  begin
    {$Q-}
    move(val, result, 4);
    result := result xor longint(lt_real);
    {$Q+}
  end;

FUNCTION T_stringLiteral.hash: longint;
  VAR
    i: longint;
  begin
    {$Q-}
    result := longint(lt_string) + length(val);
    for i := 1 to length(val) do
      result := result * 31 + Ord(val[i]);
    {$Q+}
  end;

FUNCTION T_expressionLiteral.hash: longint;
  begin
    {$Q-}
    result := longint(lt_expression) + longint(val);
    {$Q+}
  end;

FUNCTION T_listLiteral.hash: longint;
  VAR
    i: longint;
  begin
    {$Q-}
    result := longint(lt_list) + length(element);
    for i := 0 to length(element) - 1 do
      result := result * 31 + element[i]^.hash;
    {$Q+}
  end;

FUNCTION T_literal.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other);
  end;

FUNCTION T_scalarLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other);
  end; //this can only be the error literal, a singleton

FUNCTION T_boolLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other);
  end; //this can only be true or false, both singleons

FUNCTION T_intLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other) or (other^.literalType = lt_int) and
      (P_intLiteral(other)^.Value = val);
  end;

FUNCTION T_realLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other) or (other^.literalType = lt_real) and
      ((P_realLiteral(other)^.Value = val) or
       IsNan(P_realLiteral(other)^.Value) and isNan(val) or
       IsInfinite(P_realLiteral(other)^.Value) and IsInfinite(val));
  end;

FUNCTION T_stringLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other) or (other^.literalType = lt_string) and
      (P_stringLiteral(other)^.Value = val);
  end;

FUNCTION T_expressionLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other) or (other^.literalType = lt_expression) and
      (P_expressionLiteral(other)^.Value = val);
  end;

FUNCTION T_listLiteral.equals(CONST other: P_literal): boolean;
  VAR
    i: longint;
  begin
    if (@self = other) then
      exit(True);
    if (other^.literalType <> literalType) or (P_listLiteral(other)^.size <> size) then
      exit(False);
    for i := 0 to length(element) - 1 do
      if not (element[i]^.equals(P_listLiteral(other)^.element[i])) then
        exit(False);
    result := True;
  end;

VAR
  i: longint;

INITIALIZATION
  boolLit[False].Create(False);
  boolLit[True].Create(True);
  errLit.init;
  for i := 0 to length(intLit) - 1 do
    intLit[i] := nil;
  DefaultFormatSettings.DecimalSeparator := '.';
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  randomize;

FINALIZATION
  boolLit[False].Destroy;
  boolLit[True].Destroy;
  errLit.Destroy;
  for i := 0 to length(intLit) - 1 do
    if intLit[i] <> nil then
      dispose(intLit[i], Destroy);
end.
