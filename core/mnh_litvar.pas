UNIT mnh_litvar;

INTERFACE

USES mnh_constants, mnh_out_adapters, SysUtils, Math, mnh_stringUtil, mnh_tokloc, mnh_fileWrappers;
CONST
  C_boolText: array[false..true] of string = ('false', 'true');

TYPE
  PP_literal = ^P_literal;
  P_literal = ^T_literal;

  T_literal = object
  private
    numberOfReferences: longint;
  public
    CONSTRUCTOR init;
    PROCEDURE rereference;
    FUNCTION unreference: longint;

    DESTRUCTOR destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION toShorterString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_scalarLiteral = ^T_scalarLiteral;
  T_scalarLiteral = object(T_literal)
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
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
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
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
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
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
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    //from T_literal:
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_stringLiteral = ^T_stringLiteral;

  T_stringLiteral = object(T_scalarLiteral)
  private
    val: ansistring;
  public
    CONSTRUCTOR create(CONST value: ansistring);
    FUNCTION value: ansistring;
    FUNCTION softCast: P_scalarLiteral;
    FUNCTION trim: P_stringLiteral;
    FUNCTION upper: P_stringLiteral;
    FUNCTION lower: P_stringLiteral;
    //from T_scalarLiteral:
    FUNCTION stringForm: ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean; virtual;
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    //from T_literal:
    DESTRUCTOR destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION toShorterString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
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
    FUNCTION operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; virtual;
    //from T_literal:
    DESTRUCTOR destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
  end;

  P_listLiteral = ^T_listLiteral;

  { T_listLiteral }

  T_listLiteral = object(T_literal)
  private
    strictType: T_literalType;
    element: array of P_literal;
    nextAppendIsRange: boolean;
  public
    CONSTRUCTOR create;
    FUNCTION toParameterListString(CONST isFinalized: boolean): ansistring;
    PROCEDURE append(CONST L: P_literal; CONST incRefs: boolean);
    PROCEDURE appendAll(CONST L: P_listLiteral);
    PROCEDURE appendConstructing(CONST L: P_literal; CONST tokenLocation: T_tokenLocation);
    PROCEDURE setRangeAppend;
    FUNCTION size: longint;
    FUNCTION value(index: longint): P_literal;
    PROCEDURE sort;
    PROCEDURE customSort(CONST leqExpression:P_expressionLiteral);
    FUNCTION sortPerm: P_listLiteral;
    PROCEDURE unique;
    FUNCTION leqForSorting(CONST other: P_literal): boolean; virtual;
    FUNCTION isKeyValueList: boolean;
    FUNCTION clone:P_listLiteral;
    //from T_literal:
    DESTRUCTOR destroy; virtual;
    FUNCTION literalType: T_literalType; virtual;
    FUNCTION toString: ansistring; virtual;
    FUNCTION toShorterString: ansistring; virtual;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal; virtual;
    FUNCTION hash: longint; virtual;
    FUNCTION equals(CONST other: P_literal): boolean; virtual;
  end;


TYPE
  T_disposeSubruleCallback = PROCEDURE(VAR p: pointer);
  T_subruleApplyOpCallback = FUNCTION(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation): pointer;
  T_pointerToStringCallback = FUNCTION(CONST p: pointer): string;
  T_evaluateCompatorCallback = FUNCTION (CONST subruleLiteral:P_expressionLiteral; CONST LHSComparand,RHScomparand:P_literal):boolean;

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
FUNCTION newExpressionLiteral(CONST value: pointer): P_expressionLiteral; inline;
FUNCTION newListLiteral: P_listLiteral; inline;
FUNCTION newOneElementListLiteral(CONST value: P_literal; CONST incRefs: boolean): P_listLiteral; inline;
FUNCTION newErrorLiteral: P_scalarLiteral; inline;
FUNCTION newErrorLiteralRaising(CONST errorMessage: ansistring; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; inline;
FUNCTION newErrorLiteralRaising(CONST x, y: T_literalType; CONST op: T_tokenType; CONST tokenLocation: T_tokenLocation): P_scalarLiteral; inline;
FUNCTION newVoidLiteral: P_voidLiteral; inline;
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation): P_literal; inline;
FUNCTION parseNumber(CONST input: ansistring; CONST suppressOutput: boolean; OUT parsedLength: longint): P_scalarLiteral; inline;

IMPLEMENTATION

VAR
  boolLit: array[false..true] of T_boolLiteral;
  intLit: array[-1000..1000] of T_intLiteral;
  errLit: T_scalarLiteral;
  voidLit: T_voidLiteral;

PROCEDURE disposeLiteral(VAR l: P_literal);
  begin
    if l = nil then begin
      writeln(stderr, 'disposing NIL literal ?!?');
      exit;
    end;
    if l^.unreference<=0 then dispose(l, destroy);
    l := nil;
  end;

FUNCTION newBoolLiteral(CONST value: boolean): P_boolLiteral;
  begin
    result := @boolLit [value];
    result^.rereference;
  end;

FUNCTION newIntLiteral(CONST value: int64): P_intLiteral;
  begin
    if (value>=-1000) and (value<=1000) then begin
      result := @intLit [value];
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
    result := newListLiteral;
    result^.append(value, incRefs);
  end;

FUNCTION newErrorLiteral: P_scalarLiteral;
  begin
    result := @errLit;
    errLit.rereference;
  end;

FUNCTION newErrorLiteralRaising(CONST errorMessage: ansistring; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    result := @errLit;
    errLit.rereference;
    raiseError(el3_evalError, errorMessage, tokenLocation);
  end;

FUNCTION newErrorLiteralRaising(CONST x, y: T_literalType; CONST op: T_tokenType; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    result := @errLit;
    errLit.rereference;
    raiseError(el3_evalError, 'Operator '+C_tokenString [op]+ ' is not supported for types '+C_typeString [x]+' and '+ C_typeString [y], tokenLocation);
  end;

FUNCTION newVoidLiteral: P_voidLiteral; inline;
  begin
    result:=@voidLit;
    voidLit.rereference;
  end;

FUNCTION myFloatToStr(CONST x: T_myFloat): string;
  begin
    result := FloatToStr(x);
    if (pos('E', UpperCase(result))<=0) and //occurs in exponents
      (pos('N', UpperCase(result))<=0) and //occurs in "Nan or Inf"
      (pos('.', result)<=0) then
      result := result+'.0';
  end;

FUNCTION parseNumber(CONST input: ansistring; CONST suppressOutput: boolean; OUT parsedLength: longint): P_scalarLiteral;
  VAR
    i: longint;
  begin
    result := nil;
    parsedLength := 0;
    if (length(input)>=1) and (input [1] in ['0'..'9', '-', '+']) then
      begin
      i := 1;
      while (i<length(input)) and (input [i+1] in ['0'..'9']) do
        Inc(i);
      parsedLength := i;
      //Only digits on indexes [1..i]; accept decimal point and following digts
      if (i<length(input)) and (input [i+1] = '.') then
        begin
        Inc(i);
        if (i<length(input)) and (input [i+1] = '.') then
          Dec(i);
        end;
      while (i<length(input)) and (input [i+1] in ['0'..'9']) do
        Inc(i);
      //Accept exponent marker and following exponent
      if (i<length(input)) and (input [i+1] in ['e', 'E']) then
        begin
        Inc(i);
        if (i<length(input)) and (input [i+1] in ['+', '-']) then
          Inc(i);
        end;
      while (i<length(input)) and (input [i+1] in ['0'..'9']) do
        Inc(i);
      if i>parsedLength then
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
CONSTRUCTOR T_literal.init; begin numberOfReferences := 1; end;

PROCEDURE T_literal.rereference;
  begin
    InterLockedIncrement(numberOfReferences);
  end;

FUNCTION T_literal.unreference: longint;
  begin
    InterLockedDecrement(numberOfReferences);
    result := numberOfReferences;
  end;

//CONSTRUCTORS:=================================================================
CONSTRUCTOR T_voidLiteral.create();                              begin inherited init;               end;
CONSTRUCTOR T_boolLiteral      .create(CONST value: boolean);    begin inherited init; val := value; end;
CONSTRUCTOR T_intLiteral       .create(CONST value: int64);      begin inherited init; val := value; end;
CONSTRUCTOR T_realLiteral      .create(CONST value: T_myFloat);  begin inherited init; val := value; end;
CONSTRUCTOR T_stringLiteral    .create(CONST value: ansistring); begin inherited init; val := value; end;
CONSTRUCTOR T_expressionLiteral.create(CONST value: pointer);    begin inherited init; val := value; end;
CONSTRUCTOR T_listLiteral.create;
  begin
    inherited init;
    setLength(element, 0);
    strictType := lt_uncheckedList;
    nextAppendIsRange := false;
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
    for i := 0 to length(element)-1 do if element [i]<>nil then disposeLiteral(element[i]);
    setLength(element, 0);
    strictType := lt_uncheckedList;
    nextAppendIsRange := false;
  end;
//==================================================================:DESTRUCTORS
//?.literalType:================================================================
FUNCTION T_literal          .literalType: T_literalType; begin result := lt_error;      end;
FUNCTION T_scalarLiteral    .literalType: T_literalType; begin result := lt_error;      end;
FUNCTION T_voidLiteral      .literalType: T_literalType; begin result := lt_void;       end;
FUNCTION T_boolLiteral      .literalType: T_literalType; begin result := lt_boolean;    end;
FUNCTION T_intLiteral       .literalType: T_literalType; begin result := lt_int;        end;
FUNCTION T_realLiteral      .literalType: T_literalType; begin result := lt_real;       end;
FUNCTION T_stringLiteral    .literalType: T_literalType; begin result := lt_string;     end;
FUNCTION T_expressionLiteral.literalType: T_literalType; begin result := lt_expression; end;
FUNCTION T_listLiteral      .literalType: T_literalType;
  CONST ERROR_FREE_BITS= 1;
        SCALAR_BITS    = ERROR_FREE_BITS + 2;
        NUMERIC_BITS   = SCALAR_BITS + 4;
        INT_BITS       = NUMERIC_BITS+ 8;
        REAL_BITS      = NUMERIC_BITS+ 16;
        BOOL_BITS      = SCALAR_BITS + 32;
        STR_BITS       = SCALAR_BITS + 64;

  VAR i: longint;
      bitMask:byte=255;
  begin
    if strictType<>lt_uncheckedList then exit(strictType);
    if length(element)>0 then begin
      for i := 0 to length(element)-1 do
        if element [i] = nil then bitMask:=0
        else case element [i]^.literalType of
          lt_error, lt_listWithError: bitMask:=0;
          lt_boolean                : bitMask:=bitMask and BOOL_BITS;
          lt_int                    : bitMask:=bitMask and INT_BITS;
          lt_real                   : bitMask:=bitMask and REAL_BITS;
          lt_string                 : bitMask:=bitMask and STR_BITS;
          lt_list..lt_flatList      : bitMask:=bitMask and ERROR_FREE_BITS;
          else                        bitMask:=bitMask and SCALAR_BITS;
        end;
      if (bitMask and ERROR_FREE_BITS) <> ERROR_FREE_BITS then begin
        strictType := lt_listWithError;
        raiseError(el3_evalError, 'List-with-error encountered.', C_nilTokenLocation);
      end
      else if (bitMask and INT_BITS)     = INT_BITS     then strictType := lt_intList
      else if (bitMask and REAL_BITS)    = REAL_BITS    then strictType := lt_realList
      else if (bitMask and NUMERIC_BITS) = NUMERIC_BITS then strictType := lt_numList
      else if (bitMask and BOOL_BITS)    = BOOL_BITS    then strictType := lt_booleanList
      else if (bitMask and STR_BITS)     = STR_BITS     then strictType := lt_stringList
      else if (bitMask and SCALAR_BITS)  = SCALAR_BITS  then strictType := lt_flatList
      else                                                   strictType := lt_list;
    end else strictType := lt_emptyList;
    result := strictType;
  end;
//================================================================:?.literalType
//?.value:======================================================================
FUNCTION T_intLiteral       .value: int64;      begin result := val; end;
FUNCTION T_realLiteral      .value: T_myFloat;  begin result := val; end;
FUNCTION T_stringLiteral    .value: ansistring; begin result := val; end;
FUNCTION T_boolLiteral      .value: boolean;    begin result := val; end;
FUNCTION T_expressionLiteral.value: pointer;    begin result := val; end;
FUNCTION T_listLiteral      .value(index: longint): P_literal;
  begin
    result := element [index];
  end;
//======================================================================:?.value

FUNCTION T_listLiteral.size: longint;
  begin
    result := length(element);
  end;
//?.toString:===================================================================
FUNCTION T_literal          .toString: ansistring; begin result := '<ERR>';                      end;
FUNCTION T_voidLiteral      .toString: ansistring; begin result :='void';                        end;
FUNCTION T_boolLiteral      .toString: ansistring; begin result := C_boolText[val];              end;
FUNCTION T_intLiteral       .toString: ansistring; begin result := IntToStr(val);                end;
FUNCTION T_realLiteral      .toString: ansistring; begin result := myFloatToStr(val);            end;
FUNCTION T_stringLiteral    .toString: ansistring; begin result := escapeString(val);            end;
FUNCTION T_expressionLiteral.toString: ansistring; begin result := subruleToStringCallback(val); end;
FUNCTION T_listLiteral      .toString: ansistring;
  VAR i: longint;
  begin
    if length(element) = 0 then result := '[]'
    else begin
      result:='['+element[0]^.toString;
      for i := 1 to length(element)-1 do
        result:=result+','+element[i]^.toString;
      result := result+']';
    end;
  end;
//===================================================================:?.toString
FUNCTION T_listLiteral.toParameterListString(CONST isFinalized: boolean): ansistring;
  VAR
    i: longint;
  begin
    if length(element) = 0 then if isFinalized then exit('()')
                                               else exit('(');
    result := element [0]^.toShorterString;
    for i := 1 to length(element)-1 do result:=result+','+element [i]^.toShorterString;
    if isFinalized then result := '('+result+')'
    else result := '('+result+',  ';
  end;
//?.toShorterString:============================================================
FUNCTION T_literal.toShorterString: ansistring; begin result := toString; end;

FUNCTION T_stringLiteral.toShorterString: ansistring;
  begin
    if length(val)>13 then result := escapeString(copy(val, 1, 5)+'...'+copy(val, length(val)-5, 5))
    else result := toString;
  end;

FUNCTION T_listLiteral.toShorterString: ansistring;
  VAR i: longint;
  begin
    if length(element) = 0 then result := '[]'
    else if length(element)<5 then begin
      result := '['+element [0]^.toShorterString;
      for i := 1 to length(element)-1 do result := result+','+element [i]^.toShorterString;
      result := result+']';
    end else result := '['+element [                0]^.toShorterString+','+
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
    result := false;
  end;

FUNCTION T_boolLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  VAR ovl: boolean;
  begin
    if other^.literalType<>lt_boolean then exit(false);
    ovl := P_boolLiteral(other)^.val;
    result := (val=ovl) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
           or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
           or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_intLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  VAR ovi: int64;
      ovr: T_myFloat;
  begin
    case other^.literalType of
      lt_int: begin
        ovi := P_intLiteral(other)^.val;
        result := (val=ovi) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
               or (val<ovi) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
               or (val>ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        ovr := P_realLiteral(other)^.val;
        result := (val=ovr) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
               or (val<ovr) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
               or (val>ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result := false;
    end;
  end;

FUNCTION T_realLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  VAR ovi: int64;
      ovr: T_myFloat;
  begin
    case other^.literalType of
      lt_int: begin
        ovi := P_intLiteral(other)^.val;
        result := (val=ovi) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
               or (val<ovi) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
               or (val>ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        ovr := P_realLiteral(other)^.val;
        result := (val=ovr) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
               or (val<ovr) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
               or (val>ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result := false;
    end;
  end;

FUNCTION T_stringLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  VAR ovl: ansistring;
  begin
    if other^.literalType<>lt_string then exit(false);
    ovl := P_stringLiteral(other)^.val;
    result := (val=ovl) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
           or (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
           or (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_expressionLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  begin
    result := false;
  end;
//=============================================================:?.isInRelationTo
//?.negate:=====================================================================
FUNCTION T_literal.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin result := @self; rereference; end;
FUNCTION T_stringLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin result := newErrorLiteralRaising('Cannot negate string.', minusLocation); end;
FUNCTION T_boolLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin result := newErrorLiteralRaising('Cannot negate boolean.', minusLocation); end;
FUNCTION T_intLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin result := newIntLiteral(-value); end;
FUNCTION T_realLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin result := newRealLiteral(-value); end;
FUNCTION T_expressionLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  begin result := newErrorLiteralRaising('Cannot negate expression. Please use "-1*..." instead.', minusLocation); end;
FUNCTION T_listLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal;
  VAR
    res: P_listLiteral;
    i: longint;
  begin
    res := newListLiteral;
    for i := 0 to length(element)-1 do res^.append(element [i]^.negate(minusLocation), false);
    result := res;
  end;
//=====================================================================:?.negate
//?.operate:====================================================================
FUNCTION T_scalarLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin result := @self; errLit.rereference; end;
{$MACRO ON}
{$define operate_end:=
    if other^.literalType = lt_expression
      then result := newExpressionLiteral(subruleApplyOpCallback(@self, op, other, tokenLocation))
      else result := newErrorLiteralRaising(literalType, other^.literalType, op, tokenLocation);}
FUNCTION T_boolLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    case op of
      tt_comparatorEq..tt_comparatorListEq:                   exit(newBoolLiteral(isInRelationTo(op, other)));
      tt_operatorAnd: if other^.literalType = lt_boolean then exit(newBoolLiteral(val and P_boolLiteral(other)^.val));
      tt_operatorOr:  if other^.literalType = lt_boolean then exit(newBoolLiteral(val or  P_boolLiteral(other)^.val));
      tt_operatorXor: if other^.literalType = lt_boolean then exit(newBoolLiteral(val xor P_boolLiteral(other)^.val));
      tt_operatorStrConcat:                                   exit(newStringLiteral(stringForm+other^.stringForm));
    end;
    operate_end;
  end;

FUNCTION T_intLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  FUNCTION pot_int_int(x, y: int64): P_scalarLiteral;
    VAR temp: int64;
        tx, rx: T_myFloat;
    begin
      if y>=0 then begin
        temp := 1;
        while y>0 do begin
          if odd(y) then temp := temp*x;
          x := int64(x)*int64(x);
          y := y shr 1;
        end;
        result := newIntLiteral(temp);
      end else begin
        rx := 1/x;
        tx := 1;
        y := -y;
        while y>0 do begin
          if odd(y) then tx := tx*rx;
          rx := rx*rx;
          y := y shr 1;
        end;
        result := newRealLiteral(tx);
      end;
    end;

  begin
    case op of
      tt_comparatorEq..tt_comparatorListEq: exit(newBoolLiteral(isInRelationTo(op, other)));
      tt_operatorAnd: if other^.literalType = lt_int then exit(newIntLiteral(val and P_intLiteral(other)^.val));
      tt_operatorOr:  if other^.literalType = lt_int then exit(newIntLiteral(val or  P_intLiteral(other)^.val));
      tt_operatorXor: if other^.literalType = lt_int then exit(newIntLiteral(val xor P_intLiteral(other)^.val));
      tt_operatorPlus:    case other^.literalType of lt_int:  exit(newIntLiteral (val+P_intLiteral (other)^.val));
                                                     lt_real: exit(newRealLiteral(val+P_realLiteral(other)^.val)); end;
      tt_operatorMinus:   case other^.literalType of lt_int:  exit(newIntLiteral (val-P_intLiteral (other)^.val));
                                                     lt_real: exit(newRealLiteral(val-P_realLiteral(other)^.val)); end;
      tt_operatorMult:    case other^.literalType of lt_int:  exit(newIntLiteral (val*P_intLiteral (other)^.val));
                                                     lt_real: exit(newRealLiteral(val*P_realLiteral(other)^.val)); end;
      tt_operatorDivReal: case other^.literalType of lt_int:  exit(newRealLiteral(val/P_intLiteral (other)^.val));
                                                     lt_real: exit(newRealLiteral(val/P_realLiteral(other)^.val)); end;
      tt_operatorPot:     case other^.literalType of lt_int : exit(pot_int_int(val, P_intLiteral(other)^.val));
                                                     lt_real: exit(newRealLiteral(exp(ln(val)*P_realLiteral(other)^.val))); end;
      tt_operatorDivInt: if other^.literalType = lt_int then begin
        try
          result := newIntLiteral(val div P_intLiteral(other)^.val);
        except
          raiseError(el1_note, 'WARN: Integer division by zero; returning Nan', tokenLocation);
          result := newRealLiteral(Nan);
        end;
        exit(result);
      end;
      tt_operatorMod: if other^.literalType = lt_int then begin
        try
          result := newIntLiteral(val mod P_intLiteral(other)^.val)
        except
          raiseError(el1_note, 'WARN: Integer division by zero (in modulo); returning Nan', tokenLocation);
          result := newRealLiteral(Nan);
        end;
        exit(result);
      end;
      tt_operatorStrConcat: exit(newStringLiteral(stringForm+other^.stringForm));
    end;
    operate_end;
  end;

FUNCTION T_realLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;

  FUNCTION pot_real_int(x: T_myFloat; y: longint): T_myFloat;
    begin
      if y<0 then begin
        y := -y;
        x := 1/x;
      end;
      result := 1;
      while y>0 do begin
        if odd(y) then result := result*x;
        x := x*x;
        y := y shr 1;
      end;
    end;

  begin
    case op of
      tt_comparatorEq..tt_comparatorListEq: exit(newBoolLiteral(isInRelationTo(op, other)));
      tt_operatorPlus:     case other^.literalType of lt_int:  exit(newRealLiteral(val+P_intLiteral (other)^.val));
                                                      lt_real: exit(newRealLiteral(val+P_realLiteral(other)^.val)); end;
      tt_operatorMinus:    case other^.literalType of lt_int:  exit(newRealLiteral(val-P_intLiteral (other)^.val));
                                                      lt_real: exit(newRealLiteral(val-P_realLiteral(other)^.val)); end;
      tt_operatorMult:     case other^.literalType of lt_int:  exit(newRealLiteral(val*P_intLiteral (other)^.val));
                                                      lt_real: exit(newRealLiteral(val*P_realLiteral(other)^.val)); end;
      tt_operatorDivReal:  case other^.literalType of lt_int:  exit(newRealLiteral(val/P_intLiteral (other)^.val));
                                                      lt_real: exit(newRealLiteral(val/P_realLiteral(other)^.val)); end;
      tt_operatorPot:      case other^.literalType of lt_int:  exit(newRealLiteral(pot_real_int(val, P_intLiteral(other)^.val)));
                                                      lt_real: exit(newRealLiteral(exp(ln(val)*P_realLiteral(other)^.val))); end;
      tt_operatorStrConcat: exit(newStringLiteral(stringForm+other^.stringForm));
    end;
    operate_end;
  end;

FUNCTION T_stringLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    case op of
      tt_comparatorEq..tt_comparatorListEq: exit(newBoolLiteral(isInRelationTo(op, other)));
      tt_operatorPlus: if other^.literalType = lt_string then exit(newStringLiteral(val+P_stringLiteral(other)^.val));
      tt_operatorStrConcat: exit(newStringLiteral(stringForm+other^.stringForm));
    end;
    operate_end;
  end;
{$undef operate_end}

FUNCTION T_expressionLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    result := newExpressionLiteral(subruleApplyOpCallback(@self, op, other, tokenLocation));
  end;
//====================================================================:?.operate
//?.hash:=======================================================================
FUNCTION T_literal    .hash: longint; begin result := -1; end;
FUNCTION T_boolLiteral.hash: longint; begin result := longint(lt_boolean); if val then Inc(result); end;
FUNCTION T_intLiteral .hash: longint; begin result := longint(lt_int) xor longint(val); end;
FUNCTION T_realLiteral.hash: longint;
  begin
    {$Q-}
    move(val, result, 4);
    result := result xor longint(lt_real);
    {$Q+}
  end;

FUNCTION T_stringLiteral.hash: longint;
  VAR i: longint;
  begin
    {$Q-}
    result := longint(lt_string)+length(val);
    for i := 1 to length(val) do result := result*31+Ord(val[i]);
    {$Q+}
  end;

FUNCTION T_expressionLiteral.hash: longint;
  VAR i:longint;
      s:string;
  begin
    {$Q-}
    s:= toString;
    result := longint(lt_expression)+length(s);
    for i := 1 to length(s) do result := result*31+Ord(s[i]);
    {$Q+}
  end;

FUNCTION T_listLiteral.hash: longint;
  VAR i: longint;
  begin
    {$Q-}
    result := longint(lt_list)+length(element);
    for i := 0 to length(element)-1 do result := result*31+element [i]^.hash;
    {$Q+}
  end;
//=======================================================================:?.hash
//?.equals:=====================================================================
FUNCTION T_literal.equals(CONST other: P_literal): boolean;
  begin result := (@self = other);  end;

FUNCTION T_intLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other)
           or (other^.literalType = lt_int) and (P_intLiteral(other)^.value = val);
  end;

FUNCTION T_realLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other)
           or (other^.literalType = lt_real) and ((P_realLiteral(other)^.value = val)
                                               or IsNan(P_realLiteral(other)^.value) and isNan(val)
                                               or IsInfinite(P_realLiteral(other)^.value) and IsInfinite(val));
  end;

FUNCTION T_stringLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other)
           or (other^.literalType = lt_string) and (P_stringLiteral(other)^.value = val);
  end;

FUNCTION T_expressionLiteral.equals(CONST other: P_literal): boolean;
  begin
    result := (@self = other)
           or (other^.literalType = lt_expression) and (P_expressionLiteral(other)^.toString = toString);
  end;

FUNCTION T_listLiteral.equals(CONST other: P_literal): boolean;
  VAR i: longint;
  begin
    if (@self = other) then exit(true);
    if (other^.literalType<>literalType) or (P_listLiteral(other)^.size<>size) then exit(false);
    for i := 0 to length(element)-1 do if not (element [i]^.equals(P_listLiteral(other)^.element [i])) then exit(false);
    result := true;
  end;
//=====================================================================:?.equals
//?.leqForSorting:==============================================================
FUNCTION T_literal.leqForSorting(CONST other: P_Literal): boolean;
  begin
    result := literalType<=other^.literalType;
  end;

FUNCTION T_boolLiteral.leqForSorting(CONST other: P_Literal): boolean;
  begin
    if other^.literalType = lt_boolean
    then result := value<=P_boolLiteral(other)^.value
    else result := (literalType<=other^.literalType);
  end;

FUNCTION T_intLiteral.leqForSorting(CONST other: P_Literal): boolean;
  begin
    case other^.literalType of
      lt_int:  result := val<=P_intLiteral(other)^.val;
      lt_real: result := val<=P_realLiteral(other)^.val;
    else result := (literalType<=other^.literalType); end;
  end;

FUNCTION T_realLiteral.leqForSorting(CONST other: P_Literal): boolean;
  begin
    case other^.literalType of
      lt_int:  result := val<=P_intLiteral(other)^.val;
      lt_real: result := val<=P_realLiteral(other)^.val;
    else result := (literalType<=other^.literalType);  end;
  end;

FUNCTION T_stringLiteral.leqForSorting(CONST other: P_Literal): boolean;
  begin
    if (other^.literalType = lt_string)
    then result := val<=P_stringLiteral(other)^.val
    else result := (literalType<=other^.literalType);
  end;

FUNCTION T_expressionLiteral.leqForSorting(CONST other: P_Literal): boolean;
  begin
    if (other^.literalType = lt_expression)
    then result := toString<=other^.toString
    else result := literalType<=other^.literalType;
  end;


FUNCTION T_listLiteral.leqForSorting(CONST other: P_literal): boolean;
  VAR i: longint;
  begin
    if (other^.literalType in C_validListTypes) then
      begin
      if length(element)<length(P_listLiteral(other)^.element) then exit(true)
      else if length(element)>length(P_listLiteral(other)^.element) then exit(false)
      else for i := 0 to length(element)-1 do if element [i]^.leqForSorting(P_listLiteral(other)^.element [i]) then
            begin
            if not(P_listLiteral(other)^.element [i]^.leqForSorting(element [i])) then exit(true);
            end
          else exit(false);
      exit(true);
      end
    else result := literalType<=other^.literalType;
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
    result := parseNumber(val, false, len);
    if (result<>nil) then
      if (len = length(val)) then
        exit(result)
      else
        disposeLiteral(result);
    otherVal := unescapeString(SysUtils.trim(value), len);
    if len = length(SysUtils.trim(value)) then
      exit(newStringLiteral(otherVal));
    result := @self;
    rereference;
  end;

FUNCTION T_stringLiteral.trim: P_stringLiteral;
  VAR
    rs: ansistring;
  begin
    rs := SysUtils.trim(val);
    if rs = val then begin
      result := @self;
      rereference;
    end else result := newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.upper: P_stringLiteral;
  VAR
    rs: string;
  begin
    rs := uppercase(val);
    if rs = val then begin
      result := @self;
      rereference;
    end else result := newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.lower: P_stringLiteral;
  VAR
    rs: string;
  begin
    rs := lowercase(val);
    if rs = val then begin
      result := @self;
      rereference;
    end else result := newStringLiteral(rs);
  end;

PROCEDURE T_listLiteral.append(CONST L: P_literal; CONST incRefs: boolean);
  begin
    if L^.literalType=lt_void then exit;
    if L = nil then begin
      raiseError(el3_evalError, 'Trying to append NIL literal to list',
        C_nilTokenLocation);
      exit;
    end;
    setLength(element, length(element)+1);
    element[length(element)-1] := L;
    if incRefs then L^.rereference;
    strictType := lt_uncheckedList;
  end;

PROCEDURE T_listLiteral.appendAll(CONST L: P_listLiteral);
  VAR i: longint;
  begin
    for i := 0 to length(L^.element)-1 do append(L^.element [i], true);
  end;

PROCEDURE T_listLiteral.appendConstructing(CONST L: P_literal;
  CONST tokenLocation: T_tokenLocation);
  VAR
    last: P_literal;
    i0, i1: int64;
    c0, c1: char;
  begin
    if not (nextAppendIsRange) then begin
      append(L, true);
      exit;
    end;
    nextAppendIsRange := false;

    if length(element) = 0 then begin
      strictType := lt_listWithError;
      raiseError(el3_evalError, 'Cannot append range to empty list', tokenLocation);
      exit;
    end;
    last := element [length(element)-1];
    if (last^.literalType = lt_int) and (L^.literalType = lt_int) then
      begin
      i0 := P_intLiteral(last)^.val;
      i1 := P_intLiteral(L)^.val;
      while (i0<i1) and (errorLevel<el3_evalError) do
        begin
        Inc(i0);
        append(newIntLiteral(i0), false);
        end;
      while (i0>i1) and (errorLevel<el3_evalError) do
        begin
        Dec(i0);
        append(newIntLiteral(i0), false);
        end;
      end
    else if (last^.literalType = lt_string) and
      (length(P_stringLiteral(last)^.val) = 1) and (L^.literalType = lt_string) and
      (length(P_stringLiteral(L)^.val) = 1) then
      begin
      c0 := P_stringLiteral(last)^.val [1];
      c1 := P_stringLiteral(L)^.val [1];
      while c0<c1 do
        begin
        Inc(c0);
        append(newStringLiteral(c0), false);
        end;
      while c0>c1 do
        begin
        Dec(c0);
        append(newStringLiteral(c0), false);
        end;
      end
    else begin
      strictType := lt_listWithError;
      raiseError(el3_evalError, 'Invalid range expression '+
        last^.toString+'..'+L^.toString, tokenLocation);
    end;
  end;

PROCEDURE T_listLiteral.setRangeAppend;
  begin
    nextAppendIsRange := true;
  end;

PROCEDURE T_listLiteral.sort;
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  begin
    if length(element)<=1 then exit;
    scale := 1;
    setLength(temp, length(element));
    while scale<length(element) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i := 0;
      while i<length(element) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do
          if element [j0]^.leqForSorting(element [j1])  then begin temp[k] := element [j0]; Inc(k); Inc(j0); end
                                                        else begin temp[k] := element [j1]; Inc(k); Inc(j1); end;
        while (j0<i+scale)       and (j0<length(element)) do begin temp[k] := element [j0]; Inc(k); Inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(element)) do begin temp[k] := element [j1]; Inc(k); Inc(j1); end;
        Inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      Inc(scale, scale);
      if (scale<length(element)) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i := 0;
        while i<length(element) do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do
            if temp [j0]^.leqForSorting(temp [j1])        then begin element[k] := temp [j0]; Inc(k); Inc(j0); end
                                                          else begin element[k] := temp [j1]; Inc(k); Inc(j1); end;
          while (j0<i+scale) and (j0<length(element))       do begin element[k] := temp [j0]; Inc(k); Inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(element)) do begin element[k] := temp [j1]; Inc(k); Inc(j1); end;
          Inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        Inc(scale, scale);
      end else for k := 0 to length(element)-1 do element[k] := temp [k];
    end;
    setLength(temp, 0);
  end;

PROCEDURE T_listLiteral.customSort(CONST leqExpression:P_expressionLiteral);
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(a,b:P_literal):boolean; inline; begin result:=evaluateCompatorCallback(leqExpression,a,b); end;

  begin
    if length(element)<=1 then exit;
    scale := 1;
    setLength(temp, length(element));
    while (scale<length(element)) and (errorLevel<el3_evalError) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i := 0;
      while (i<length(element)) and (errorLevel<el3_evalError) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do
          if isLeq(element [j0],element [j1])           then begin temp[k] := element [j0]; Inc(k); Inc(j0); end
                                                        else begin temp[k] := element [j1]; Inc(k); Inc(j1); end;
        while (j0<i+scale)       and (j0<length(element)) do begin temp[k] := element [j0]; Inc(k); Inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(element)) do begin temp[k] := element [j1]; Inc(k); Inc(j1); end;
        Inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      Inc(scale, scale);
      if (scale<length(element)) and (errorLevel<el3_evalError) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i := 0;
        while (i<length(element)) and (errorLevel<el3_evalError) do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do
            if isLeq(temp [j0],temp [j1])                 then begin element[k] := temp [j0]; Inc(k); Inc(j0); end
                                                          else begin element[k] := temp [j1]; Inc(k); Inc(j1); end;
          while (j0<i+scale) and (j0<length(element))       do begin element[k] := temp [j0]; Inc(k); Inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(element)) do begin element[k] := temp [j1]; Inc(k); Inc(j1); end;
          Inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        Inc(scale, scale);
      end else for k := 0 to length(element)-1 do element[k] := temp [k];
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
    for i := 0 to length(element)-1 do with temp1 [i] do begin
      v := P_scalarLiteral(element [i]);
      index := i;
    end;
    scale := 1;
    while scale<length(temp1) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i := 0;
      while i<length(temp1) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do
          if temp1 [j0].v^.leqForSorting(temp1 [j1].v) then begin temp2[k] := temp1 [j0]; Inc(k); Inc(j0); end
                                                       else begin temp2[k] := temp1 [j1]; Inc(k); Inc(j1); end;
        while (j0<i+scale) and (j0<length(temp1))        do begin temp2[k] := temp1 [j0]; Inc(k); Inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(temp1))  do begin temp2[k] := temp1 [j1]; Inc(k); Inc(j1); end;
        Inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      Inc(scale, scale);
      if (scale<length(temp1)) then begin
        i := 0;
        while i<length(temp1) do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do
            if temp2 [j0].v^.leqForSorting(temp2 [j1].v) then begin temp1[k] := temp2 [j0]; Inc(k); Inc(j0); end
                                                         else begin temp1[k] := temp2 [j1]; Inc(k); Inc(j1); end;
          while (j0<i+scale)  and (j0<length(temp1))       do begin temp1[k] := temp2 [j0]; Inc(k); Inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(temp1))  do begin temp1[k] := temp2 [j1]; Inc(k); Inc(j1); end;
          Inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        Inc(scale, scale);
      end else for k := 0 to length(temp1)-1 do temp1[k] := temp2 [k];
    end;
    setLength(temp2, 0);
    result := newListLiteral;
    for i := 0 to length(temp1)-1 do result^.append(newIntLiteral(temp1 [i].index), false);
    setLength(temp1, 0);
  end;

PROCEDURE T_listLiteral.unique;
  VAR
    i, j: longint;
  begin
    if length(element)<=1 then exit;
    sort;
    i := 0;
    for j := 1 to length(element)-1 do
      if (element [i]^.leqForSorting(element [j])) and
        (element [j]^.leqForSorting(element [i])) then disposeLiteral(element [j])
      else
        begin
        Inc(i);
        element[i] := element [j];
        end;
    setLength(element, i+1);
  end;


FUNCTION T_listLiteral.isKeyValueList: boolean;
  VAR i: longint;
  begin
    if (literalType<>lt_list) or (length(element)<=0) then exit(length(element)=0);
    for i := 0 to length(element)-1 do if not(
        (element [i]^.literalType in C_validListTypes) and
        (P_listLiteral(element [i])^.size = 2) and
        (P_listLiteral(element [i])^.value(0)^.literalType = lt_string)) then exit(false);
    result := true;
  end;

FUNCTION T_listLiteral.clone:P_listLiteral;
  begin
    result:=newListLiteral;
    result^.appendAll(@self);
    if nextAppendIsRange then result^.setRangeAppend;
  end;

FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation): P_literal;

  FUNCTION equals(CONST LHS, RHS: P_literal): boolean;
    VAR
      i: longint;
    begin
      if LHS = RHS then
        exit(true);
      case LHS^.literalType of
        lt_int, lt_real, lt_boolean, lt_string, lt_expression: if RHS^.literalType in
            [lt_int, lt_real, lt_boolean, lt_string, lt_expression] then
            exit(P_scalarLiteral(LHS)^.isInRelationTo(tt_comparatorEq,
              P_scalarLiteral(RHS)))
          else
            exit(false);
        lt_list..lt_flatList: if (RHS^.literalType in C_validListTypes) and
                                 (length(P_listLiteral(LHS)^.element) =length(P_listLiteral(RHS)^.element)) then begin
            result := true;
            i := 0;
            while result and (i<length(P_listLiteral(LHS)^.element)) do begin
              result := result and equals(P_listLiteral(LHS)^.element [i],
                P_listLiteral(RHS)^.element [i]);
              Inc(i);
            end;
          end else exit(false);
        else exit(false);
      end;
    end;

  FUNCTION isContained(CONST LHS, RHS: P_literal): boolean;
    VAR i: longint;
    begin
      result := false;
      if RHS^.literalType in C_validListTypes then begin
        i := 0;
        while (i<length(P_listLiteral(RHS)^.element)) and not (result) do begin
          result := result or equals(LHS, P_listLiteral(RHS)^.element [i]);
          Inc(i);
        end;
      end;
    end;

  VAR
    i, i1, j: longint;
    key: ansistring;
  begin
    if (LHS^.literalType=lt_void) then begin RHS^.rereference; exit(RHS); end;
    if (RHS^.literalType=lt_void) then begin LHS^.rereference; exit(LHS); end;
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
            lt_boolean, lt_int, lt_real, lt_string, lt_expression: exit(P_scalarLiteral(LHS)^.operate(op, P_scalarLiteral(RHS), tokenLocation));
            //scalar X scalar

            lt_list: begin
              //scalar X nested list
              result := newListLiteral;
              for i := 0 to length(P_listLiteral(RHS)^.element)-1 do
                P_listLiteral(result)^.append(
                  resolveOperator(LHS, op, P_listLiteral(RHS)^.element [i],
                  tokenLocation),
                  false);
              exit(result);
              end;
            lt_booleanList, lt_intList, lt_realList, lt_numList,
            lt_stringList, lt_flatList: begin
              //scalar X flat list
              result := newListLiteral;
              for i := 0 to length(P_listLiteral(RHS)^.element)-1 do
                P_listLiteral(result)^.append(
                  P_scalarLiteral(LHS)^.operate(op, P_scalarLiteral(
                  P_listLiteral(RHS)^.element [i]), tokenLocation),
                  false);
              exit(result);
              end;
          else exit(newErrorLiteralRaising(LHS^.literalType,
              RHS^.literalType, op, tokenLocation));
            end;
        lt_list: case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string, lt_expression: begin
              //nested list X scalar
              result := newListLiteral;
              for i := 0 to length(P_listLiteral(LHS)^.element)-1 do
                P_listLiteral(result)^.append(
                  resolveOperator(P_listLiteral(LHS)^.element [i], op,
                  RHS, tokenLocation),
                  false);
              exit(result);
              end;
            lt_list..lt_flatList: begin
              //nested list X flat/nested list
              i := length(P_listLiteral(LHS)^.element);
              i1 := length(P_listLiteral(RHS)^.element);
              if i = i1 then
                begin
                result := newListLiteral;
                for i := 0 to i1-1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.element [i], op,
                    P_listLiteral(RHS)^.element [i], tokenLocation), false);
                exit(result);
                end
              else
                exit(newErrorLiteralRaising('Invalid list lengths '+
                  IntToStr(i)+' and '+IntToStr(i1)+' given for operator '+
                  C_tokenString [op], tokenLocation));
              end;
          else exit(newErrorLiteralRaising(LHS^.literalType,
              RHS^.literalType, op, tokenLocation));
            end;
        lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList,
        lt_flatList: case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string, lt_expression: begin
              //flat list X scalar
              result := newListLiteral;
              for i := 0 to length(P_listLiteral(LHS)^.element)-1 do
                P_listLiteral(result)^.append(
                  P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.operate(
                  op, P_scalarLiteral(RHS), tokenLocation),
                  false);
              exit(result);
            end;
            lt_list: begin
              //flat list X nested list
              i := length(P_listLiteral(LHS)^.element);
              i1 := length(P_listLiteral(RHS)^.element);
              if i = i1 then begin
                result := newListLiteral;
                for i := 0 to i1-1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.element [i], op,
                    P_listLiteral(RHS)^.element [i], tokenLocation), false);
                exit(result);
              end else
                exit(newErrorLiteralRaising('Invalid list lengths '+
                  IntToStr(i)+' and '+IntToStr(i1)+' given for operator '+
                  C_tokenString [op], tokenLocation));
            end;
            lt_booleanList..lt_flatList: begin
              //flat list X flat list
              i := length(P_listLiteral(LHS)^.element);
              i1 := length(P_listLiteral(RHS)^.element);
              if i = i1 then
                begin
                result := newListLiteral;
                for i := 0 to i1-1 do
                  P_listLiteral(result)^.append(
                    P_scalarLiteral(P_listLiteral(LHS)^.element [i])^.operate(op,
                    P_scalarLiteral(P_listLiteral(RHS)^.element [i]),
                    tokenLocation), false);
                exit(result);
                end
              else
                exit(newErrorLiteralRaising('Invalid list lengths '+
                  IntToStr(i)+' and '+IntToStr(i1)+' given for operator '+
                  C_tokenString [op], tokenLocation));
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
        if (LHS^.literalType in [lt_error, lt_boolean, lt_int, lt_real, lt_string, lt_expression])
        then P_listLiteral(result)^.append(LHS, true)
        else P_listLiteral(result)^.appendAll(P_listLiteral(LHS));
        if (RHS^.literalType in [lt_error, lt_boolean, lt_int, lt_real, lt_string, lt_expression])
        then P_listLiteral(result)^.append(RHS, true)
        else P_listLiteral(result)^.appendAll(P_listLiteral(RHS));
        exit(result);
      end;
      tt_comparatorListEq: exit(newBoolLiteral(equals(LHS, RHS)));
      tt_operatorIn: exit(newBoolLiteral(isContained(LHS, RHS)));
      tt_operatorExtractL0: if LHS^.literalType in C_validListTypes then
          case RHS^.literalType of
            lt_int: begin
              i1 := length(P_listLiteral(LHS)^.element);
              i := P_intLiteral(RHS)^.val;
              if (i>=0) and (i<i1) then
                begin
                result := P_listLiteral(LHS)^.element [i];
                result^.rereference;
                exit(result);
                end
              else
                exit(newListLiteral);
              end;
            lt_intList: begin
              result := newListLiteral;
              i1 := length(P_listLiteral(LHS)^.element);
              for j := 0 to length(P_listLiteral(RHS)^.element)-1 do
                begin
                i := P_intLiteral(P_listLiteral(RHS)^.element [j])^.val;
                if (i>=0) and (i<i1) then
                  P_listLiteral(result)^.append(P_listLiteral(LHS)^.element [i], true);
                end;
              exit(result);
              end;
            lt_booleanList: begin
              result := newListLiteral;
              i1 := length(P_listLiteral(LHS)^.element);
              if i1 = length(P_listLiteral(RHS)^.element) then
                for i := 0 to length(P_listLiteral(RHS)^.element)-1 do
                  if P_boolLiteral(P_listLiteral(RHS)^.element [i])^.val then
                    P_listLiteral(result)^.append(P_listLiteral(LHS)^.element [i], true);
              exit(result);
              end;
            lt_string: if P_listLiteral(LHS)^.isKeyValueList then
                begin
                key := P_stringLiteral(RHS)^.value;
                for i := 0 to length(P_listLiteral(LHS)^.element)-1 do
                  if P_stringLiteral(P_listLiteral(P_listLiteral(LHS)^.element [i])^.element [0])^.value = key then
                    begin
                    result := P_listLiteral(P_listLiteral(LHS)^.element [i])^.element [1];
                    result^.rereference;
                    exit(result);
                    end;
                exit(newListLiteral);
                end
              else exit(newErrorLiteralRaising('Operator % with a string as second operand can only be applied to key-value-lists!', tokenLocation));
            lt_stringList: if P_listLiteral(LHS)^.isKeyValueList then
                begin
                result := newListLiteral;
                for j := 0 to P_listLiteral(RHS)^.size-1 do
                  begin
                  key := P_stringLiteral(P_listLiteral(RHS)^.element [j])^.value;
                  i := 0; while i<length(P_listLiteral(LHS)^.element) do
                    if P_stringLiteral(P_listLiteral(P_listLiteral(LHS)^.element [i])^.element [0])^.value = key then
                      begin
                      P_listLiteral(result)^.append(P_listLiteral(P_listLiteral(LHS)^.element [i])^.element [1], true);
                      i := length(P_listLiteral(LHS)^.element);
                      end
                    else Inc(i);
                  end;
                exit(result);
                end
              else exit(newErrorLiteralRaising('Operator % with a stringList as second operand can only be applied to key-value-lists!', tokenLocation));
            lt_emptyList: exit(newListLiteral);
            lt_list, lt_flatList:
                exit(newErrorLiteralRaising(LHS^.literalType,
                  RHS^.literalType, op, tokenLocation));
          else exit(newErrorLiteralRaising(LHS^.literalType,
              RHS^.literalType, op, tokenLocation));
            end
        else
          exit(newErrorLiteralRaising(LHS^.literalType, RHS^.literalType,
            op, tokenLocation));
      tt_operatorExtractL1: if LHS^.literalType in C_validListTypes then begin
          result := newListLiteral;
          for i := 0 to length(P_listLiteral(LHS)^.element)-1 do
            P_listLiteral(result)^.append(resolveOperator(
              P_listLiteral(LHS)^.element [i], tt_operatorExtractL0,
              RHS, tokenLocation), false);
          exit(result);
        end else exit(newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, op, tokenLocation));
      tt_operatorExtractL2: if LHS^.literalType in C_validListTypes then begin
          result := newListLiteral;
          for i := 0 to length(P_listLiteral(LHS)^.element)-1 do
            P_listLiteral(result)^.append(resolveOperator(
              P_listLiteral(LHS)^.element [i], tt_operatorExtractL1,
              RHS, tokenLocation), false);
          exit(result);
        end else exit(newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, op, tokenLocation));
      tt_operatorExtractL3: if LHS^.literalType in C_validListTypes then begin
          result := newListLiteral;
          for i := 0 to length(P_listLiteral(LHS)^.element)-1 do
            P_listLiteral(result)^.append(resolveOperator(
              P_listLiteral(LHS)^.element [i], tt_operatorExtractL2,
              RHS, tokenLocation), false);
          exit(result);
        end else exit(newErrorLiteralRaising(LHS^.literalType, RHS^.literalType, op, tokenLocation));
      end;
  end;

VAR
  i: longint;

INITIALIZATION
  boolLit[false].create(false);
  boolLit[true].create(true);
  errLit.init;
  voidLit.create();
  for i := -1000 to 1000 do intLit[i].create(i);
  DefaultFormatSettings.DecimalSeparator := '.';
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  randomize;

FINALIZATION
  {$ifdef debugMode}
  writeln(stdErr,'Finalizing mnh_litvar');
  {$endif}
  boolLit[false].destroy;
  boolLit[true].destroy;
  errLit.destroy;
  voidLit.destroy;
  for i := -1000 to 1000 do intLit[i].destroy;
  {$ifdef debugMode}
  writeln(stdErr,'mnh_litvar finalized');
  {$endif}
end.
