UNIT mnh_litvar;

INTERFACE

USES mnh_constants, mnh_out_adapters, SysUtils, Math, myStringutil, mnh_tokloc, mnh_fileWrappers;
CONST
  C_boolText: array[false..true] of string = ('false', 'true');

TYPE
  PP_literal = ^P_literal;
  P_literal = ^T_literal;

  T_literal = object
  private
    trueType:T_literalType;
    numberOfReferences: longint;
    intValue:Int64;
    realValue:T_myFloat;
    stringValue:ansistring;
    expressionValue:pointer;
    listValue:array of P_literal;
    nextAppendIsRange: boolean;
    CONSTRUCTOR init;
    CONSTRUCTOR createVoid;
    CONSTRUCTOR createBoolean(CONST value:boolean);
    CONSTRUCTOR createInt(CONST value:int64);
    CONSTRUCTOR createString(CONST value:ansistring);
    CONSTRUCTOR createExpression(CONST value:pointer);
  public
    CONSTRUCTOR createList;
    CONSTRUCTOR createReal(CONST value:T_myFloat);
    DESTRUCTOR destroy;

    PROCEDURE rereference;
    FUNCTION unreference: longint;
    FUNCTION getReferenceCount: longint;

    FUNCTION literalType: T_literalType;
    FUNCTION toString: ansistring;
    FUNCTION toShorterString: ansistring;
    FUNCTION negate(CONST minusLocation: T_tokenLocation): P_literal;
    FUNCTION hash: longint;
    FUNCTION equals(CONST other: P_literal): boolean;
    FUNCTION leqForSorting(CONST other: P_literal): boolean;

    FUNCTION getIntValue:int64;
    FUNCTION getRealValue:T_myFloat;
    FUNCTION getStringValue:ansistring;
    FUNCTION getBoolValue:boolean;
    FUNCTION getExpressionValue:pointer;
    FUNCTION size:longint;
    FUNCTION value(CONST index: longint): P_literal;

    FUNCTION opAnd      (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opOr       (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opXor      (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opPlus     (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opMinus    (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opMult     (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opDivReal  (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opPot      (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opDivInt   (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opMod      (CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;
    FUNCTION opStrConcat(CONST other: T_Literal; CONST tokenLocation: T_tokenLocation): P_Literal;

    FUNCTION stringForm: ansistring;
    FUNCTION softCast: P_literal;
    FUNCTION isInRelationTo(CONST relation: T_tokenType; CONST other: T_Literal): boolean;

    FUNCTION trim:P_literal;
    FUNCTION trimLeft:P_literal;
    FUNCTION trimRight:P_literal;
    FUNCTION upper:P_literal;
    FUNCTION lower:P_literal;
    FUNCTION unbrace:P_literal;
    FUNCTION escape:P_literal;
    PROCEDURE appendString(CONST appendix:ansistring);

    FUNCTION toParameterListString(CONST isFinalized: boolean): ansistring;
    PROCEDURE append(CONST L: P_literal; CONST incRefs: boolean);
    PROCEDURE appendAll(CONST L: P_Literal);
    PROCEDURE appendConstructing(CONST L: P_literal; CONST tokenLocation: T_tokenLocation);
    PROCEDURE setRangeAppend;
    FUNCTION head:P_literal;
    FUNCTION head(CONST headSize:longint):P_Literal;
    FUNCTION tail:P_Literal;
    FUNCTION tail(CONST headSize:longint):P_Literal;
    FUNCTION trailing:P_literal;
    FUNCTION trailing(CONST trailSize:longint):P_Literal;
    FUNCTION leading:P_Literal;
    FUNCTION leading(CONST trailSize:longint):P_Literal;

    PROCEDURE sort;
    PROCEDURE customSort(CONST leqExpression:P_Literal);
    FUNCTION sortPerm: P_Literal;
    PROCEDURE unique;
    FUNCTION isKeyValueList:boolean;
  end;

  T_namedVariable=object
    private
      id:ansistring;
      value:P_literal;
    public
      CONSTRUCTOR create(CONST initialId:ansistring; CONST initialValue:P_literal);
      DESTRUCTOR destroy;
      PROCEDURE setValue(CONST newValue:P_literal);
      FUNCTION mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
      FUNCTION getId:ansistring;
      FUNCTION getValue:P_literal;
  end;

  T_disposeSubruleCallback = PROCEDURE(VAR p: pointer);
  T_subruleApplyOpCallback = FUNCTION(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation): pointer;
  T_pointerToStringCallback = FUNCTION(CONST p: pointer): string;
  T_evaluateCompatorCallback = FUNCTION (CONST subruleLiteral,LHSComparand,RHScomparand:P_literal):boolean;

VAR
  disposeSubruleCallback: T_disposeSubruleCallback;
  subruleToStringCallback: T_pointerToStringCallback;
  subruleApplyOpCallback: T_subruleApplyOpCallback;
  evaluateCompatorCallback: T_evaluateCompatorCallback;

PROCEDURE disposeLiteral(VAR l: P_literal); inline;
FUNCTION newBoolLiteral(CONST value: boolean): P_Literal; inline;
FUNCTION newIntLiteral(CONST value: int64): P_Literal; inline;
FUNCTION newRealLiteral(CONST value: T_myFloat): P_Literal; inline;
FUNCTION newStringLiteral(CONST value: ansistring): P_Literal; inline;
FUNCTION newExpressionLiteral(CONST value: pointer): P_Literal; inline;
FUNCTION newListLiteral: P_Literal; inline;
FUNCTION newOneElementListLiteral(CONST value: P_literal; CONST incRefs: boolean): P_Literal; inline;
FUNCTION newErrorLiteralSilent:P_literal; inline;
FUNCTION newErrorLiteralRaising(CONST level:T_errorLevel; CONST errorMessage: ansistring; CONST tokenLocation: T_tokenLocation): P_Literal; inline;
FUNCTION newVoidLiteral: P_Literal; inline;
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation): P_literal; inline;
FUNCTION parseNumber(CONST input: ansistring; CONST suppressOutput: boolean; OUT parsedLength: longint): P_Literal; inline;

IMPLEMENTATION

VAR
  boolLit: array[false..true] of T_Literal;
  intLit: array[-127..128] of T_Literal;
  errLit: T_Literal;
  voidLit: T_Literal;

PROCEDURE disposeLiteral(VAR l: P_literal);
  begin
    if l = nil then begin
      writeln(stderr, 'disposing NIL literal ?!?');
      exit;
    end;
    if l^.unreference<=0 then dispose(l, destroy);
    l:=nil;
  end;

FUNCTION newBoolLiteral(CONST value: boolean): P_Literal;
  begin
    result:=@boolLit [value];
    result^.rereference;
  end;

FUNCTION newIntLiteral(CONST value: int64): P_Literal;
  begin
    if (value>=-127) and (value<=128) then begin
      result:=@intLit [value];
      result^.rereference;
    end else begin
      new(result,createInt(value));
    end;
  end;

FUNCTION newRealLiteral(CONST value: T_myFloat): P_Literal;
  begin
    new(result, createReal(value));
  end;

FUNCTION newStringLiteral(CONST value: ansistring): P_Literal;
  begin
    new(result, createString(value));
  end;

FUNCTION newExpressionLiteral(CONST value: pointer): P_Literal;
  begin
    new(result, createExpression(value));
  end;

FUNCTION newListLiteral: P_Literal;
  begin
    new(result, createList);
  end;

FUNCTION newOneElementListLiteral(CONST value: P_literal; CONST incRefs: boolean): P_Literal;
  begin
    new(result,createList);
    result^.append(value, incRefs);
  end;

FUNCTION newErrorLiteralSilent:P_literal; inline;
  begin
    result:=@errLit;
    errLit.rereference;
  end;

FUNCTION newErrorLiteralRaising(CONST level:T_errorLevel; CONST errorMessage: ansistring; CONST tokenLocation: T_tokenLocation): P_Literal; inline;
  begin
    result:=@errLit;
    errLit.rereference;
    raiseError(level, errorMessage, tokenLocation);
  end;

FUNCTION newErrorLiteralRaising(CONST errorMessage: ansistring; CONST tokenLocation: T_tokenLocation): P_Literal;
  begin
    result:=@errLit;
    errLit.rereference;
    raiseError(el3_evalError, errorMessage, tokenLocation);
  end;

FUNCTION newErrorLiteralRaising(CONST x, y: T_literalType; CONST op: T_tokenType; CONST tokenLocation: T_tokenLocation): P_Literal;
  begin
    result:=@errLit;
    errLit.rereference;
    raiseError(el3_evalError, 'Operator '+C_tokenString [op]+ ' is not supported for types '+C_typeString [x]+' and '+ C_typeString [y], tokenLocation);
  end;

FUNCTION newVoidLiteral: P_Literal; inline;
  begin
    result:=@voidLit;
    voidLit.rereference;
  end;

FUNCTION myFloatToStr(CONST x: T_myFloat): string;
  begin
    result:=FloatToStr(x);
    if (pos('E', UpperCase(result))<=0) and //occurs in exponents
      (pos('N', UpperCase(result))<=0) and //occurs in "Nan or Inf"
      (pos('.', result)<=0) then
      result:=result+'.0';
  end;

FUNCTION parseNumber(CONST input: ansistring; CONST suppressOutput: boolean; OUT parsedLength: longint): P_Literal;
  VAR
    i: longint;
  begin
    result:=nil;
    parsedLength:=0;
    if (length(input)>=1) and (input [1] in ['0'..'9', '-', '+']) then
      begin
      i:=1;
      while (i<length(input)) and (input [i+1] in ['0'..'9']) do
        Inc(i);
      parsedLength:=i;
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
        parsedLength:=i;
        if suppressOutput then
          exit(nil);
        result:=newRealLiteral(StrToFloatDef(copy(input, 1, parsedLength), NAN));
        end
      else
        begin
        if suppressOutput then
          exit(nil);
        result:=newIntLiteral(StrToInt64Def(copy(input, 1, parsedLength), 0));
        end;
      end;
  end;

//=====================================================================================================================
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
CONSTRUCTOR T_literal.init;
  begin
    numberOfReferences:=1;
    trueType:=lt_error;
    intValue:=0;
    realValue:=Nan;
    stringValue:='';
    expressionValue:=nil;
    setLength(listValue,0);
    nextAppendIsRange:=false;
  end;

CONSTRUCTOR T_literal.createVoid;                               begin init; trueType:=lt_void;                                  end;
CONSTRUCTOR T_literal.createBoolean   (CONST value:boolean);    begin init; trueType:=lt_boolean;    if value then intValue:=1; end;
CONSTRUCTOR T_literal.createInt       (CONST value:int64);      begin init; trueType:=lt_int;        intValue:=value;           end;
CONSTRUCTOR T_literal.createReal      (CONST value:T_myFloat);  begin init; trueType:=lt_real;       realValue:=value;          end;
CONSTRUCTOR T_literal.createString    (CONST value:ansistring); begin init; trueType:=lt_string;     stringValue:=value;        end;
CONSTRUCTOR T_literal.createExpression(CONST value:pointer);    begin init; trueType:=lt_expression; expressionValue:=value;    end;
CONSTRUCTOR T_literal.createList;                               begin init; trueType:=lt_emptyList;                             end;
//=================================================================:CONSTRUCTORS
//DESTRUCTORS:==================================================================
DESTRUCTOR T_literal.destroy;
  VAR i: longint;
  begin
    case trueType of
      lt_list..lt_listWithError:begin
        for i:=0 to length(listValue)-1 do if listValue[i]<>nil then disposeLiteral(listValue[i]);
        setLength(listValue, 0);
        trueType:=lt_emptyList;
        nextAppendIsRange:=false;
      end;
      lt_string:
        stringValue:='';
      lt_expression:
        disposeSubruleCallback(expressionValue);
    end;
  end;
//==================================================================:DESTRUCTORS
//?.literalType:================================================================
FUNCTION T_literal.literalType: T_literalType; begin result:=trueType; end;
//================================================================:?.literalType

FUNCTION T_Literal.getIntValue:int64;                begin result:=intValue; end;
FUNCTION T_Literal.getRealValue:T_myFloat;           begin result:=realValue; end;
FUNCTION T_Literal.getStringValue:ansistring;        begin result:=stringValue; end;
FUNCTION T_Literal.getBoolValue:boolean;             begin result:=odd(intValue); end;
FUNCTION T_Literal.getExpressionValue:pointer;       begin result:=expressionValue; end;
FUNCTION T_Literal.size:longint;                     begin result:=length(listValue); end;
FUNCTION T_Literal.value(CONST index: longint): P_literal; begin result:=listValue[index]; end;


FUNCTION T_Literal.head:P_literal;
  begin
    if length(listValue)=0
    then result:=@self
    else result:=listValue[0];
    result^.rereference;
  end;

FUNCTION T_literal.head(CONST headSize:longint):P_literal;
  VAR i,iMax:longint;
  begin
    iMax:=headSize;
    if iMax>length(listValue) then iMax:=length(listValue);
    result:=newListLiteral;
    for i:=0 to iMax-1 do result^.append(listValue[i],true);
  end;

FUNCTION T_literal.tail:P_literal;
  begin result:=tail(1); end;

FUNCTION T_literal.tail(CONST headSize:longint):P_literal;
  VAR i,iMin:longint;
  begin
    iMin:=headSize;
    if iMin>length(listValue) then iMin:=length(listValue);
    result:=newListLiteral;
    for i:=iMin to length(listValue)-1 do result^.append(listValue[i],true);
  end;

FUNCTION T_literal.trailing:P_Literal;
  begin
    if length(listValue)=0
    then result:=@self
    else result:=listValue[length(listValue)-1];
    result^.rereference;
  end;

FUNCTION T_literal.trailing(CONST trailSize:longint):P_literal;
  begin result:=tail(length(listValue)-trailSize); end;

FUNCTION T_literal.leading:P_literal;
  begin result:=head(length(listValue)-1); end;

FUNCTION T_literal.leading  (CONST trailSize:longint):P_literal;
  begin result:=head(length(listValue)-trailSize); end;

FUNCTION T_literal.toString: ansistring;
  VAR i: longint;
  begin
    case trueType of
      lt_boolean:
        result:=C_boolText[odd(intValue)];
      lt_int:
        result:=IntToStr(intValue);
      lt_real:
        result:=myFloatToStr(realValue);
      lt_string:
        result:=escapeString(stringValue);
      lt_expression:
        result:=subruleToStringCallback(expressionValue);
      lt_list..lt_flatList:
        if length(listValue) = 0 then result:='[]'
        else begin
          result:='['+listValue[0]^.toString;
          for i:=1 to length(listValue)-1 do
            result:=result+','+listValue[i]^.toString;
          result:=result+']';
        end;
      lt_error,
      lt_listWithError:
        result:='<ERR>';
      lt_void:
        result:=C_typeString[lt_void];
    end;
  end;

FUNCTION T_Literal.toParameterListString(CONST isFinalized: boolean): ansistring;
  VAR
    i: longint;
  begin
    if length(listValue) = 0 then if isFinalized then exit('()')
                                               else exit('(');
    result:=listValue [0]^.toShorterString;
    for i:=1 to length(listValue)-1 do result:=result+','+listValue [i]^.toShorterString;
    if isFinalized then result:='('+result+')'
    else result:='('+result+', ';
  end;

FUNCTION T_literal.toShorterString: ansistring;
  VAR i:longint;
  begin
    case trueType of
      lt_string:
        if length(stringValue)>13
        then result:=escapeString(copy(stringValue, 1, 5)+'...'+copy(stringValue, length(stringValue)-5, 5))
        else result:=toString;
      lt_list..lt_flatList:
        if length(listValue) = 0 then result:='[]'
        else if length(listValue)<5 then begin
          result:='['+listValue [0]^.toShorterString;
          for i:=1 to length(listValue)-1 do result:=result+','+listValue [i]^.toShorterString;
          result:=result+']';
        end else result:='['+listValue [                  0]^.toShorterString+','+
                             listValue [                  1]^.toShorterString+',...,'+
                             listValue [length(listValue)-2]^.toShorterString+','+
                             listValue [length(listValue)-1]^.toShorterString+']';
      else result:=toString;
    end;
  end;

FUNCTION T_literal.stringForm:ansistring;
  begin
    if trueType=lt_string then result:=stringValue
                          else result:=toString;
  end;

FUNCTION T_Literal.isInRelationTo(CONST relation: T_tokenType; CONST other: T_Literal): boolean;
  FUNCTION stringsAreInRelation(CONST LHS,RHS:ansistring):boolean; inline;
    begin
      result:=(LHS=RHS) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
           or (LHS<RHS) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
           or (LHS>RHS) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
    end;

  VAR i:longint;
  begin
    case trueType of
      lt_boolean:
        begin
          if other.trueType<>lt_boolean then exit(false);
          intValue:=intValue and 1;
          result:=(intValue=other.intValue and 1) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
               or (intValue<other.intValue and 1) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
               or (intValue>other.intValue and 1) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
        end;
      lt_int:
        case other.trueType of
          lt_int: begin
            result:=(intValue=other.intValue) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
                 or (intValue<other.intValue) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
                 or (intValue>other.intValue) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
          end;
          lt_real: begin
            result:=(intValue=other.realValue) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
                 or (intValue<other.realValue) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
                 or (intValue>other.realValue) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
          end;
          else result:=false;
        end;
      lt_real:
        case other.trueType of
          lt_int: begin
            result:=(realValue=other.intValue) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
                 or (realValue<other.intValue) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
                 or (realValue>other.intValue) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
          end;
          lt_real: begin
            result:=(realValue=other.realValue) and (relation in [tt_comparatorEq, tt_comparatorListEq, tt_comparatorLeq, tt_comparatorGeq])
                 or (realValue<other.realValue) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss])
                 or (realValue>other.realValue) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
          end;
          else result:=false;
        end;
      lt_string:
        begin
          if other.trueType<>lt_string then exit(false);
          result:=stringsAreInRelation(stringValue,other.stringValue);
        end;
      lt_expression:
        begin
          if other.trueType<>lt_expression then exit(false);
          result:=stringsAreInRelation(subruleToStringCallback(expressionValue),subruleToStringCallback(other.expressionValue));
        end;
      lt_list..lt_flatList:
        if (relation=tt_comparatorListEq) and (length(listValue)=length(other.listValue)) then begin
          for i:=0 to length(listValue)-1 do if not(listValue[i]^.isInRelationTo(tt_comparatorListEq,other.listValue[i]^)) then exit(false);
          result:=true;
        end else result:=false;
      else result:=false;
    end;
  end;

FUNCTION T_literal.negate(CONST minusLocation: T_tokenLocation): P_literal;
  VAR minusOne:P_literal;
      i:longint;
  begin
    case trueType of
      lt_boolean,lt_string:
        result:=newErrorLiteralRaising('Cannot negate '+C_typeString[trueType]+'.', minusLocation);
      lt_int:
        result:=newIntLiteral(-intValue);
      lt_real:
        result:=newRealLiteral(-realValue);
      lt_expression:
        begin
          minusOne:=newIntLiteral(-1);
          result:=newExpressionLiteral(subruleApplyOpCallback(minusOne,tt_operatorMult,@self,minusLocation));
          disposeLiteral(minusOne);
          if errorLevel>=el3_evalError then begin
            disposeLiteral(result);
            result:=@errLit;
            errLit.rereference;
          end;
        end;
      lt_list..lt_flatList:
        begin
          result:=newListLiteral;
          for i:=0 to length(listValue)-1 do result^.append(listValue[i]^.negate(minusLocation),false);
          if errorLevel>=el3_evalError then begin
            disposeLiteral(result);
            result:=@errLit;
            errLit.rereference;
          end;
        end;
      else begin
        result:=@self; rereference;
      end;
    end;
  end;

//?.operate:====================================================================
FUNCTION T_literal.opAnd(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    case trueType of
      lt_boolean:
        case other.trueType of
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorAnd, @other, tokenLocation)));
          lt_boolean   : exit(newBoolLiteral(odd(intValue and other.intValue)));
        end;
      lt_int:
        case other.trueType of
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorAnd, @other, tokenLocation)));
          lt_int       : exit(newIntLiteral(intValue and other.intValue));
        end;
      lt_expression:
        if (other.trueType in [lt_int,lt_boolean,lt_expression])
        then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorAnd, @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType, other.trueType, tt_operatorAnd, tokenLocation);
  end;

FUNCTION T_literal.opOr(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    case trueType of
      lt_boolean:
        case other.trueType of
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorOr, @other, tokenLocation)));
          lt_boolean   : exit(newBoolLiteral(odd(intValue or other.intValue)));
        end;
      lt_int:
        case other.trueType of
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorOr, @other, tokenLocation)));
          lt_int       : exit(newIntLiteral(intValue or other.intValue));
        end;
      lt_expression:
        if (other.trueType in [lt_int,lt_boolean,lt_expression])
        then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorOr, @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType, other.trueType, tt_operatorOr, tokenLocation);
  end;

FUNCTION T_literal.opXor(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    case trueType of
      lt_boolean:
        case other.trueType of
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorXor, @other, tokenLocation)));
          lt_boolean   : exit(newBoolLiteral(odd(intValue xor other.intValue)));
        end;
      lt_int:
        case other.trueType of
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorXor, @other, tokenLocation)));
          lt_int       : exit(newIntLiteral(intValue xor other.intValue));
        end;
      lt_expression:
        if (other.trueType in [lt_int,lt_boolean,lt_expression])
        then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorXor, @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType, other.trueType, tt_operatorXor, tokenLocation);
  end;

FUNCTION T_literal.opPlus(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    case trueType of
      lt_int:
        case other.trueType of
          lt_int: exit(newIntLiteral(intValue+other.intValue));
          lt_real: exit(newRealLiteral(intValue+other.realValue));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPlus , @other, tokenLocation)));
        end;
      lt_real:
        case other.trueType of
          lt_int: exit(newRealLiteral(realValue+other.intValue));
          lt_real: exit(newRealLiteral(realValue+other.realValue));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPlus , @other, tokenLocation)));
        end;
      lt_string:
        case other.trueType of
          lt_string: exit(newStringLiteral(stringValue+other.stringValue));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPlus , @other, tokenLocation)));
        end;
      lt_expression:
        if other.trueType in [lt_int,lt_real,lt_string,lt_expression] then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPlus , @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType, other.trueType, tt_operatorPlus, tokenLocation);
  end;

FUNCTION T_literal.opMinus(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    case trueType of
      lt_int:
        case other.trueType of
          lt_int: exit(newIntLiteral(intValue-other.intValue));
          lt_real: exit(newRealLiteral(intValue-other.realValue));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMinus , @other, tokenLocation)));
        end;
      lt_real:
        case other.trueType of
          lt_int: exit(newRealLiteral(realValue-other.intValue));
          lt_real: exit(newRealLiteral(realValue-other.realValue));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMinus , @other, tokenLocation)));
        end;
      lt_expression:
        if other.trueType in [lt_int,lt_real,lt_expression] then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMinus , @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType, other.trueType, tt_operatorMinus, tokenLocation);
  end;

FUNCTION T_literal.opMult(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    case trueType of
      lt_int:
        case other.trueType of
          lt_int: exit(newIntLiteral(intValue*other.intValue));
          lt_real: exit(newRealLiteral(intValue*other.realValue));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMult , @other, tokenLocation)));
        end;
      lt_real:
        case other.trueType of
          lt_int: exit(newRealLiteral(realValue*other.intValue));
          lt_real: exit(newRealLiteral(realValue*other.realValue));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMult , @other, tokenLocation)));
        end;
      lt_expression:
        if other.trueType in [lt_int,lt_real,lt_expression] then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMult , @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType, other.trueType, tt_operatorMult, tokenLocation);
  end;

FUNCTION T_literal.opDivReal(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    case trueType of
      lt_int:
        case other.trueType of
          lt_int: exit(newRealLiteral(intValue/other.intValue));
          lt_real: exit(newRealLiteral(intValue/other.realValue));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivReal , @other, tokenLocation)));
        end;
      lt_real:
        case other.trueType of
          lt_int: exit(newRealLiteral(realValue/other.intValue));
          lt_real: exit(newRealLiteral(realValue/other.realValue));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivReal , @other, tokenLocation)));
        end;
      lt_expression:
        if other.trueType in [lt_int,lt_real,lt_expression] then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivReal , @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType, other.trueType, tt_operatorDivReal, tokenLocation);

  end;

FUNCTION T_literal.opPot(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  FUNCTION pot_int_int(x, y: int64): P_literal;
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
    case trueType of
      lt_int:
        case other.trueType of
          lt_int:        exit(pot_int_int(intValue,other.intValue));
          lt_real:       exit(newRealLiteral(exp(ln(intValue)*other.realValue)));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPot, @other, tokenLocation)));
        end;
      lt_real:
        case other.trueType of
          lt_int:        exit(newRealLiteral(pot_real_int(realValue, other.intValue)));
          lt_real:       exit(newRealLiteral(exp(ln(realValue)*other.realValue)));
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPot, @other, tokenLocation)));
        end;
      lt_expression:
        if other.trueType in [lt_int,lt_real,lt_expression] then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorPot, @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType,other.trueType,tt_operatorPot,tokenLocation);
  end;

FUNCTION T_literal.opDivInt(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    case trueType of
      lt_int:
        case other.trueType of
          lt_int:
            begin
              try
                result:=newIntLiteral(intValue div other.intValue);
              except
                raiseError(el1_note, 'WARN: Integer division by zero; returning Nan', tokenLocation);
                result:=newRealLiteral(Nan);
              end;
              exit(result);
            end;
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivInt, @other, tokenLocation)));
        end;
      lt_expression:
        if other.trueType in [lt_int,lt_expression] then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorDivInt, @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType,other.trueType,tt_operatorDivInt,tokenLocation);
  end;

FUNCTION T_literal.opMod(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    case trueType of
      lt_int:
        case other.trueType of
          lt_int:
            begin
              try
                result:=newIntLiteral(intValue mod other.intValue);
              except
                raiseError(el1_note, 'WARN: Integer division by zero(in modulo); returning Nan', tokenLocation);
                result:=newRealLiteral(Nan);
              end;
              exit(result);
            end;
          lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMod, @other, tokenLocation)));
        end;
      lt_expression:
        if other.trueType in [lt_int,lt_expression] then exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorMod, @other, tokenLocation)));
    end;
    result:=newErrorLiteralRaising(trueType,other.trueType,tt_operatorMod,tokenLocation);
  end;

FUNCTION T_literal.opStrConcat(CONST other: T_literal; CONST tokenLocation: T_tokenLocation): P_literal;
  begin
    if (trueType=lt_expression) or (other.trueType=lt_expression) then
      exit(newExpressionLiteral(subruleApplyOpCallback(@self, tt_operatorStrConcat, @other, tokenLocation)));
    result:=newStringLiteral(stringForm+other.stringForm);
  end;

//?.hash:=======================================================================
FUNCTION T_literal.hash: longint;
  CONST prime=31;
  VAR i:longint=0;
      s:ansistring;
  begin
    {$Q-}
    result:=longint(trueType);
    case trueType of
      lt_boolean: result:=result*prime+(intValue and 1);
      lt_int: result:=result*prime+intValue;
      lt_real: begin
        move(realValue, i, 4);
        result:=result*prime+i;
      end;
      lt_string, lt_expression: begin
        if trueType=lt_string then s:=stringValue
                              else s:=subruleToStringCallback(expressionValue);
        result:=result*prime+length(s);
        for i:=1 to length(s) do result:=result*prime+ord(s[i]);
      end;
      lt_list..lt_flatList: begin
        result:=result*prime+length(listValue);
        for i:=0 to length(listValue)-1 do result:=result*prime+listValue[i]^.hash;
      end;
    end;
    {$Q+}
  end;

FUNCTION T_literal.equals(CONST other: P_literal): boolean;
  begin result:=(trueType=other^.trueType) and isInRelationTo(tt_comparatorListEq,other^) end;

FUNCTION T_literal.leqForSorting(CONST other: P_Literal): boolean;
  VAR i:longint;
      minLen:longint;
  begin
    case trueType of
      lt_boolean: if (other^.trueType=lt_boolean) then exit(intValue and 1 <= other^.intValue and 1);
      lt_int:
        case other^.trueType of
          lt_int: exit(intValue<=other^.intValue);
          lt_real: exit(intValue<=other^.realValue);
        end;
      lt_real:
        case other^.trueType of
          lt_int: exit(realValue<=other^.intValue);
          lt_real: exit(realValue<=other^.realValue);
        end;
      lt_string: if (other^.trueType=lt_string) then exit(stringValue <= other^.stringValue);
      lt_expression: if (other^.trueType=lt_expression) then exit(toString <= other^.toString);
      lt_list..lt_flatList: if other^.trueType in C_validListTypes then begin
        minLen:=length(other^.listValue);
        if length(listValue)<minLen then minLen:=length(listValue);
        for i:=0 to minLen-1 do
          if listValue[i]^.leqForSorting(other^.listValue[i]) then begin
            if not(other^.listValue[i]^.leqForSorting(listValue[i])) then exit(true);
          end else exit(false);
        exit(length(listValue)<=length(other^.listValue));
      end;
    end;
    result:=trueType<=other^.trueType;
  end;

FUNCTION T_Literal.softCast: P_Literal;
  VAR i: longint;
      otherVal: ansistring;
  begin
    case trueType of
      lt_string: begin
        if lowercase(stringValue) = C_boolText [false] then
          exit(newBoolLiteral(false));
        if lowercase(stringValue) = C_boolText [true] then
          exit(newBoolLiteral(true));
        result:=parseNumber(stringValue, false, i);
        if (result<>nil) then
          if (i = length(stringValue)) then
            exit(result)
          else
            disposeLiteral(result);
        otherVal:=unescapeString(SysUtils.trim(stringValue), i);
        if i = length(SysUtils.trim(stringValue)) then
          exit(newStringLiteral(otherVal));
        result:=@self;
        rereference;
      end;
      lt_list,
      lt_stringList,
      lt_flatList: begin
        result:=newListLiteral;
        for i:=0 to length(listValue)-1 do result^.append(listValue[i]^.softCast,false);
      end;
      else begin result:=@self; rereference; end;
    end;
  end;

FUNCTION T_Literal.trim: P_literal;
  VAR rs: ansistring;
  begin
    rs:=SysUtils.trim(stringValue);
    if rs = stringValue then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_literal.trimLeft: P_literal;
  VAR rs: ansistring;
  begin
    rs:=SysUtils.TrimLeft(stringValue);
    if rs = stringValue then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_literal.trimRight: P_literal;
  VAR rs: ansistring;
  begin
    rs:=SysUtils.TrimRight(stringValue);
    if rs = stringValue then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;


FUNCTION T_literal.upper: P_literal;
  VAR rs: string;
  begin
    rs:=uppercase(stringValue);
    if rs = stringValue then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_literal.lower: P_literal;
  VAR rs: string;
  begin
    rs:=lowercase(stringValue);
    if rs = stringValue then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_literal.unbrace: P_literal;
  VAR rs: string;
  begin
    rs:=myStringutil.unbrace(stringValue);
    if rs = stringValue then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_literal.escape: P_literal;
  begin
    result:=newStringLiteral(escapeString(stringValue));
  end;

PROCEDURE T_literal.appendString(CONST appendix:ansistring);
  begin
    stringValue:=stringValue+appendix;
  end;

PROCEDURE T_literal.append(CONST L: P_literal; CONST incRefs: boolean);
  begin
    if L = nil then begin
      raiseError(el3_evalError, 'Trying to append NIL literal to list', C_nilTokenLocation);
      exit;
    end;
    if L^.trueType=lt_void then exit;
    setLength(listValue, length(listValue)+1);
    listValue[length(listValue)-1]:=L;
    if incRefs then L^.rereference;
    case trueType of
      lt_list:
        if L^.trueType in [lt_error,lt_listWithError,lt_void] then trueType:=lt_listWithError;
      lt_booleanList:
        case L^.trueType of
  	  lt_error,lt_listWithError,lt_void:
            trueType:=lt_listWithError;
  	  lt_list..lt_flatList:
            trueType:=lt_list;
  	  lt_boolean:
            begin end;
  	  else trueType:=lt_flatList;
  	end;
      lt_intList:
        case L^.trueType of
  	  lt_error,lt_listWithError,lt_void:
            trueType:=lt_listWithError;
  	  lt_list..lt_flatList:
            trueType:=lt_list;
  	  lt_int:
            begin end;
  	  lt_real:
            trueType:=lt_numList;
  	  else trueType:=lt_flatList;
  	end;
      lt_realList:
        case L^.trueType of
  	  lt_error,lt_listWithError,lt_void:
            trueType:=lt_listWithError;
          lt_list..lt_flatList:
            trueType:=lt_list;
  	  lt_real:
            begin end;
  	  lt_int:
            trueType:=lt_numList;
  	  else trueType:=lt_flatList;
        end;
      lt_numList:
        case L^.trueType of
  	  lt_error,lt_listWithError,lt_void:
            trueType:=lt_listWithError;
  	  lt_list..lt_flatList:
            trueType:=lt_list;
  	  lt_int,lt_real:
            begin end;
  	  else trueType:=lt_flatList;
  	end;
      lt_stringList:
        case L^.trueType of
  	  lt_error,lt_listWithError,lt_void:
            trueType:=lt_listWithError;
  	  lt_list..lt_flatList:
            trueType:=lt_list;
  	  lt_string:
            begin end;
  	  else trueType:=lt_flatList;
  	end;
      lt_emptyList:
        case L^.trueType of
  	  lt_error,lt_listWithError,lt_void:
            trueType:=lt_listWithError;
  	  lt_boolean:
            trueType:=lt_booleanList;
          lt_int:
            trueType:=lt_intList;
          lt_real:
            trueType:=lt_realList;
          lt_string:
            trueType:=lt_stringList;
          lt_expression:
            trueType:=lt_flatList;
          lt_list..lt_flatList:
            trueType:=lt_list;
  	end;
      lt_flatList:
        case L^.trueType of
  	  lt_error,lt_listWithError,lt_void:
            trueType:=lt_listWithError;
  	  lt_list..lt_flatList:
            trueType:=lt_list;
  	  else begin end;
  	end;
      lt_listWithError: begin end;
    end;
  end;

PROCEDURE T_literal.appendAll(CONST L: P_literal);
  VAR i: longint;
  begin
    for i:=0 to length(L^.listValue)-1 do append(L^.listValue [i], true);
  end;

PROCEDURE T_literal.appendConstructing(CONST L: P_literal; CONST tokenLocation: T_tokenLocation);
  VAR last: P_literal;
      i0, i1: int64;
      c0, c1: char;
  begin
    if not(nextAppendIsRange) then begin
      append(L, true);
      exit;
    end;
    nextAppendIsRange:=false;

    if length(listValue) = 0 then begin
      trueType:=lt_listWithError;
      raiseError(el3_evalError, 'Cannot append range to empty list', tokenLocation);
      exit;
    end;
    last:=listValue[length(listValue)-1];
    if (last^.trueType = lt_int) and (L^.trueType = lt_int) then begin
      i0:=last^.intValue;
      i1:=L^.intValue;
      while (i0<i1) and (errorLevel<el3_evalError) do begin
        Inc(i0);
        append(newIntLiteral(i0), false);
      end;
      while (i0>i1) and (errorLevel<el3_evalError) do begin
        Dec(i0);
        append(newIntLiteral(i0), false);
      end;
    end else if (last^.trueType = lt_string) and (length(last^.stringValue) = 1) and
                (L^   .trueType = lt_string) and (length(L^   .stringValue) = 1) then begin
      c0:=last^.stringValue [1];
      c1:=L^.stringValue [1];
      while c0<c1 do begin
        Inc(c0);
        append(newStringLiteral(c0), false);
      end;
      while c0>c1 do begin
        Dec(c0);
        append(newStringLiteral(c0), false);
      end;
    end else begin
      trueType:=lt_listWithError;
      raiseError(el3_evalError, 'Invalid range expression '+
        last^.toString+'..'+L^.toString, tokenLocation);
    end;
  end;

PROCEDURE T_literal.setRangeAppend;
  begin
    nextAppendIsRange:=true;
  end;

PROCEDURE T_literal.sort;
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  begin
    if length(listValue)<=1 then exit;
    scale:=1;
    setLength(temp, length(listValue));
    while scale<length(listValue) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<length(listValue) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(listValue)) do
          if listValue [j0]^.leqForSorting(listValue [j1])  then begin temp[k]:=listValue [j0]; Inc(k); Inc(j0); end
                                                        else begin temp[k]:=listValue [j1]; Inc(k); Inc(j1); end;
        while (j0<i+scale)       and (j0<length(listValue)) do begin temp[k]:=listValue [j0]; Inc(k); Inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(listValue)) do begin temp[k]:=listValue [j1]; Inc(k); Inc(j1); end;
        Inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      Inc(scale, scale);
      if (scale<length(listValue)) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while i<length(listValue) do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(listValue)) do
            if temp [j0]^.leqForSorting(temp [j1])        then begin listValue[k]:=temp [j0]; Inc(k); Inc(j0); end
                                                          else begin listValue[k]:=temp [j1]; Inc(k); Inc(j1); end;
          while (j0<i+scale) and (j0<length(listValue))       do begin listValue[k]:=temp [j0]; Inc(k); Inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(listValue)) do begin listValue[k]:=temp [j1]; Inc(k); Inc(j1); end;
          Inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        Inc(scale, scale);
      end else for k:=0 to length(listValue)-1 do listValue[k]:=temp [k];
    end;
    setLength(temp, 0);
  end;

PROCEDURE T_literal.customSort(CONST leqExpression:P_literal);
  VAR temp: array of P_literal;
      scale: longint;
      i, j0, j1, k: longint;
  FUNCTION isLeq(a,b:P_literal):boolean; inline; begin result:=evaluateCompatorCallback(leqExpression,a,b); end;

  begin
    if length(listValue)<=1 then exit;
    scale:=1;
    setLength(temp, length(listValue));
    while (scale<length(listValue)) and (errorLevel<el3_evalError) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while (i<length(listValue)) and (errorLevel<el3_evalError) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(listValue)) do
          if isLeq(listValue [j0],listValue [j1])           then begin temp[k]:=listValue [j0]; Inc(k); Inc(j0); end
                                                        else begin temp[k]:=listValue [j1]; Inc(k); Inc(j1); end;
        while (j0<i+scale)       and (j0<length(listValue)) do begin temp[k]:=listValue [j0]; Inc(k); Inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(listValue)) do begin temp[k]:=listValue [j1]; Inc(k); Inc(j1); end;
        Inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      Inc(scale, scale);
      if (scale<length(listValue)) and (errorLevel<el3_evalError) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //while making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while (i<length(listValue)) and (errorLevel<el3_evalError) do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(listValue)) do
            if isLeq(temp [j0],temp [j1])                 then begin listValue[k]:=temp [j0]; Inc(k); Inc(j0); end
                                                          else begin listValue[k]:=temp [j1]; Inc(k); Inc(j1); end;
          while (j0<i+scale) and (j0<length(listValue))       do begin listValue[k]:=temp [j0]; Inc(k); Inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(listValue)) do begin listValue[k]:=temp [j1]; Inc(k); Inc(j1); end;
          Inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        Inc(scale, scale);
      end else for k:=0 to length(listValue)-1 do listValue[k]:=temp [k];
    end;
    setLength(temp, 0);
  end;

FUNCTION T_literal.sortPerm: P_literal;
  VAR
    temp1, temp2: array of record
      v: P_literal;
      index: longint;
    end;
    scale: longint;
    i, j0, j1, k: longint;

  begin
    if length(listValue) = 0 then exit(newListLiteral);

    setLength(temp1, length(listValue));
    setLength(temp2, length(listValue));
    for i:=0 to length(listValue)-1 do with temp1 [i] do begin
      v:=listValue[i];
      index:=i;
    end;
    scale:=1;
    while scale<length(temp1) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<length(temp1) do begin
        j0:=i; j1:=i+scale; k:=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do
          if temp1 [j0].v^.leqForSorting(temp1 [j1].v) then begin temp2[k]:=temp1 [j0]; Inc(k); Inc(j0); end
                                                       else begin temp2[k]:=temp1 [j1]; Inc(k); Inc(j1); end;
        while (j0<i+scale) and (j0<length(temp1))        do begin temp2[k]:=temp1 [j0]; Inc(k); Inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(temp1))  do begin temp2[k]:=temp1 [j1]; Inc(k); Inc(j1); end;
        Inc(i, scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      Inc(scale, scale);
      if (scale<length(temp1)) then begin
        i:=0;
        while i<length(temp1) do begin
          j0:=i; j1:=i+scale; k:=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do
            if temp2 [j0].v^.leqForSorting(temp2 [j1].v) then begin temp1[k]:=temp2 [j0]; Inc(k); Inc(j0); end
                                                         else begin temp1[k]:=temp2 [j1]; Inc(k); Inc(j1); end;
          while (j0<i+scale)  and (j0<length(temp1))       do begin temp1[k]:=temp2 [j0]; Inc(k); Inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(temp1))  do begin temp1[k]:=temp2 [j1]; Inc(k); Inc(j1); end;
          Inc(i, scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        Inc(scale, scale);
      end else for k:=0 to length(temp1)-1 do temp1[k]:=temp2 [k];
    end;
    setLength(temp2, 0);
    result:=newListLiteral;
    for i:=0 to length(temp1)-1 do result^.append(newIntLiteral(temp1 [i].index), false);
    setLength(temp1, 0);
  end;

PROCEDURE T_literal.unique;
  VAR
    i, j: longint;
  begin
    if length(listValue)<=1 then exit;
    sort;
    i:=0;
    for j:=1 to length(listValue)-1 do
      if (listValue [i]^.leqForSorting(listValue [j])) and
        (listValue [j]^.leqForSorting(listValue [i])) then disposeLiteral(listValue [j])
      else
        begin
        Inc(i);
        listValue[i]:=listValue [j];
        end;
    setLength(listValue, i+1);
  end;


FUNCTION T_literal.isKeyValueList: boolean;
  VAR i: longint;
  begin
    if (trueType<>lt_list) or (length(listValue)<=0) then exit(length(listValue)=0);
    for i:=0 to length(listValue)-1 do if not(
        (listValue[i]^.trueType in [lt_list,lt_flatList,lt_stringList]) and
        (length(listValue[i]^.listValue) = 2) and
        ((listValue [i])^.listValue[0]^.trueType = lt_string)) then exit(false);
    result:=true;
  end;

FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation): P_literal;

  //FUNCTION equals(CONST LHS, RHS: P_literal): boolean;
  //  VAR
  //    i: longint;
  //  begin
  //    if LHS = RHS then
  //      exit(true);
  //    case LHS^.trueType of
  //      lt_int, lt_real, lt_boolean, lt_string, lt_expression: if RHS^.trueType in
  //          [lt_int, lt_real, lt_boolean, lt_string, lt_expression] then
  //          exit(P_scalarLiteral(LHS)^.isInRelationTo(tt_comparatorEq,
  //            P_scalarLiteral(RHS)))
  //        else
  //          exit(false);
  //      lt_list..lt_flatList: if (RHS^.trueType in C_validListTypes) and
  //                               (length(P_listLiteral(LHS)^.listValue) =length(P_listLiteral(RHS)^.listValue)) then begin
  //          result:=true;
  //          i:=0;
  //          while result and (i<length(P_listLiteral(LHS)^.listValue)) do begin
  //            result:=result and equals(P_listLiteral(LHS)^.listValue [i],
  //              P_listLiteral(RHS)^.listValue [i]);
  //            Inc(i);
  //          end;
  //        end else exit(false);
  //      else exit(false);
  //    end;
  //  end;

  FUNCTION isContained(CONST LHS, RHS: P_literal): boolean;
    VAR i: longint;
    begin
      result:=false;
      if RHS^.trueType in C_validListTypes then begin
        i:=0;
        while (i<length(RHS^.listValue)) and not (result) do begin
          result:=result or LHS^.isInRelationTo(tt_comparatorListEq, RHS^.listValue[i]^);
          Inc(i);
        end;
      end;
    end;

  VAR
    i, i1, j: longint;
    key: ansistring;
  begin
    //HANDLE S x S -> S OPERATORS:---------------------------------------------
    case op of
      tt_comparatorEq,tt_comparatorNeq,tt_comparatorLeq,tt_comparatorGeq,tt_comparatorLss,tt_comparatorGrt:
      case LHS^.trueType of
        lt_boolean, lt_int, lt_real, lt_string, lt_expression:
          case RHS^.trueType of
            lt_boolean, lt_int, lt_real, lt_string: exit(newBoolLiteral(LHS^.isInRelationTo(op,RHS^)));
            //scalar X scalar
            lt_list: begin
              //scalar X nested list
              result:=newListLiteral;
              for i:=0 to length(RHS^.listValue)-1 do
                result^.append(
                  resolveOperator(LHS, op, RHS^.listValue [i],
                  tokenLocation),
                  false);
              exit(result);
            end;
            lt_booleanList..lt_flatList: begin
              //scalar X flat list
              result:=newListLiteral;
              for i:=0 to length(RHS^.listValue)-1 do
                result^.append(
                  newBoolLiteral(LHS^.isInRelationTo(op, RHS^.listValue[i]^)),
                  false);
              exit(result);
            end;
          end;
          lt_list: case RHS^.trueType of
            lt_boolean, lt_int, lt_real, lt_string: begin
              //nested list X scalar
              result:=newListLiteral;
              for i:=0 to length(LHS^.listValue)-1 do
                result^.append(
                  resolveOperator(LHS^.listValue [i], op,
                  RHS, tokenLocation),
                  false);
              exit(result);
            end;
            lt_list..lt_flatList: begin
              //nested list X flat/nested list
              i:=length(LHS^.listValue);
              i1:=length(RHS^.listValue);
              if i = i1 then begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  result^.append(resolveOperator(
                    LHS^.listValue [i], op,
                    RHS^.listValue [i], tokenLocation), false);
                exit(result);
              end else
                exit(newErrorLiteralRaising('Invalid list lengths '+
                  IntToStr(i)+' and '+IntToStr(i1)+' given for operator '+
                  C_tokenString [op], tokenLocation));
            end;
          end;
          lt_booleanList..lt_flatList: case RHS^.trueType of
            lt_boolean, lt_int, lt_real, lt_string: begin
              //flat list X scalar
              result:=newListLiteral;
              for i:=0 to length(LHS^.listValue)-1 do
                result^.append(
                  newBoolLiteral(LHS^.listValue[i]^.isInRelationTo(op, RHS^)),
                  false);
              exit(result);
            end;
            lt_list: begin
              //flat list X nested list
              i:=length(LHS^.listValue);
              i1:=length(RHS^.listValue);
              if i = i1 then begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  result^.append(resolveOperator(
                    LHS^.listValue [i], op,
                    RHS^.listValue [i], tokenLocation), false);
                exit(result);
              end else
                exit(newErrorLiteralRaising('Invalid list lengths '+
                  IntToStr(i)+' and '+IntToStr(i1)+' given for operator '+
                  C_tokenString [op], tokenLocation));
            end;
            lt_booleanList..lt_flatList: begin
              //flat list X flat list
              i:=length(LHS^.listValue);
              i1:=length(RHS^.listValue);
              if i = i1 then
                begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  result^.append(
                    newBoolLiteral(
                      LHS^.listValue[i]^.isInRelationTo(op,
                      RHS^.listValue[i]^)),
                    false);
                exit(result);
                end
              else
                exit(newErrorLiteralRaising('Invalid list lengths '+
                  IntToStr(i)+' and '+IntToStr(i1)+' given for operator '+
                  C_tokenString [op], tokenLocation));
              end;
          end;
      end;
      tt_operatorAnd, tt_operatorOr, tt_operatorXor,
      tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal,
      tt_operatorDivInt, tt_operatorMod, tt_operatorPot, tt_operatorStrConcat:
      case LHS^.trueType of
        lt_boolean, lt_int, lt_real, lt_string:
          case RHS^.trueType of
            lt_boolean, lt_int, lt_real, lt_string:
              case op of
                tt_operatorAnd:       exit(LHS^.opAnd      (RHS^,tokenLocation));
                tt_operatorOr:        exit(LHS^.opOr       (RHS^,tokenLocation));
                tt_operatorXor:       exit(LHS^.opXor      (RHS^,tokenLocation));
                tt_operatorPlus:      exit(LHS^.opPlus     (RHS^,tokenLocation));
                tt_operatorMinus:     exit(LHS^.opMinus    (RHS^,tokenLocation));
                tt_operatorMult:      exit(LHS^.opMult     (RHS^,tokenLocation));
                tt_operatorDivReal:   exit(LHS^.opDivReal  (RHS^,tokenLocation));
                tt_operatorDivInt:    exit(LHS^.opDivInt   (RHS^,tokenLocation));
                tt_operatorMod:       exit(LHS^.opMod      (RHS^,tokenLocation));
                tt_operatorPot:       exit(LHS^.opPot      (RHS^,tokenLocation));
                tt_operatorStrConcat: exit(LHS^.opStrConcat(RHS^,tokenLocation));
              end;
            //scalar X scalar
            lt_list: begin
              //scalar X nested list
              result:=newListLiteral;
              for i:=0 to length(RHS^.listValue)-1 do
                result^.append(
                  resolveOperator(LHS, op, RHS^.listValue[i],
                  tokenLocation),
                  false);
              exit(result);
            end;
            lt_booleanList..lt_flatList: begin
              //scalar X flat list
              result:=newListLiteral;
              case op of
                tt_operatorAnd:        for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opAnd      (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorOr:         for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opOr       (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorXor:        for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opXor      (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorPlus:       for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opPlus     (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorMinus:      for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opMinus    (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorMult:       for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opMult     (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorDivReal:    for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opDivReal  (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorDivInt:     for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opDivInt   (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorMod:        for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opMod      (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorPot:        for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opPot      (RHS^.listValue[i]^,tokenLocation),false);
                tt_operatorStrConcat:  for i:=0 to length(RHS^.listValue)-1 do result^.append(LHS^.opStrConcat(RHS^.listValue[i]^,tokenLocation),false);
              end;
              exit(result);
            end;
          end;
          lt_list: case RHS^.trueType of
            lt_boolean, lt_int, lt_real, lt_string: begin
              //nested list X scalar
              result:=newListLiteral;
              for i:=0 to length(LHS^.listValue)-1 do
                result^.append(
                  resolveOperator(LHS^.listValue [i], op,
                  RHS, tokenLocation),
                  false);
              exit(result);
            end;
            lt_list..lt_flatList: begin
              //nested list X flat/nested list
              i:=length(LHS^.listValue);
              i1:=length(RHS^.listValue);
              if i = i1 then begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  result^.append(resolveOperator(
                    LHS^.listValue [i], op,
                    RHS^.listValue [i], tokenLocation), false);
                exit(result);
              end else
                exit(newErrorLiteralRaising('Invalid list lengths '+
                  IntToStr(i)+' and '+IntToStr(i1)+' given for operator '+
                  C_tokenString [op], tokenLocation));
            end;
          end;
          lt_booleanList..lt_flatList: case RHS^.trueType of
            lt_boolean, lt_int, lt_real, lt_string: begin
              //flat list X scalar
              result:=newListLiteral;
              case op of
                tt_operatorAnd:        for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opAnd      (RHS^,tokenLocation),false);
                tt_operatorOr:         for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opOr       (RHS^,tokenLocation),false);
                tt_operatorXor:        for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opXor      (RHS^,tokenLocation),false);
                tt_operatorPlus:       for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opPlus     (RHS^,tokenLocation),false);
                tt_operatorMinus:      for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opMinus    (RHS^,tokenLocation),false);
                tt_operatorMult:       for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opMult     (RHS^,tokenLocation),false);
                tt_operatorDivReal:    for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opDivReal  (RHS^,tokenLocation),false);
                tt_operatorDivInt:     for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opDivInt   (RHS^,tokenLocation),false);
                tt_operatorMod:        for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opMod      (RHS^,tokenLocation),false);
                tt_operatorPot:        for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opPot      (RHS^,tokenLocation),false);
                tt_operatorStrConcat:  for i:=0 to length(LHS^.listValue)-1 do result^.append(LHS^.listValue[i]^.opStrConcat(RHS^,tokenLocation),false);
              end;
              exit(result);
            end;
            lt_list: begin
              //flat list X nested list
              i:=length(LHS^.listValue);
              i1:=length(RHS^.listValue);
              if i = i1 then begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  result^.append(resolveOperator(
                    LHS^.listValue [i], op,
                    RHS^.listValue [i], tokenLocation), false);
                exit(result);
              end else
                exit(newErrorLiteralRaising('Invalid list lengths '+
                  IntToStr(i)+' and '+IntToStr(i1)+' given for operator '+
                  C_tokenString [op], tokenLocation));
            end;
            lt_booleanList..lt_flatList: begin
              //flat list X flat list
              i:=length(LHS^.listValue);
              i1:=length(RHS^.listValue);
              if i = i1 then begin
                result:=newListLiteral;
                case op of
                  tt_operatorAnd:        for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opAnd      (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorOr:         for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opOr       (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorXor:        for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opXor      (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorPlus:       for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opPlus     (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorMinus:      for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opMinus    (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorMult:       for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opMult     (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorDivReal:    for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opDivReal  (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorDivInt:     for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opDivInt   (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorMod:        for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opMod      (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorPot:        for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opPot      (RHS^.listValue[i]^,tokenLocation),false);
                  tt_operatorStrConcat:  for i:=0 to i1-1 do result^.append(LHS^.listValue[i]^.opStrConcat(RHS^.listValue[i]^,tokenLocation),false);
                end;
                exit(result);
              end else
                exit(newErrorLiteralRaising('Invalid list lengths '+
                  IntToStr(i)+' and '+IntToStr(i1)+' given for operator '+
                  C_tokenString [op], tokenLocation));
            end;
          end;
      end;
      tt_operatorConcat: begin
        result:=newListLiteral;
        if (LHS^.trueType in [lt_boolean..lt_expression])
        then result^.append(LHS, true)
        else result^.appendAll(LHS);
        if (RHS^.trueType in [lt_boolean..lt_expression])
        then result^.append(RHS, true)
        else result^.appendAll(RHS);
        exit(result);
      end;
      tt_comparatorListEq: exit(newBoolLiteral(LHS^.isInRelationTo(tt_comparatorListEq,RHS^)));
      tt_operatorIn: exit(newBoolLiteral(isContained(LHS, RHS)));
      tt_operatorExtractL0: if LHS^.trueType in C_validListTypes then case RHS^.trueType of
        lt_int: begin
          i1:=length(LHS^.listValue);
          i:=RHS^.intValue;
          if (i>=0) and (i<i1) then begin
            result:=LHS^.listValue [i];
            result^.rereference;
            exit(result);
          end else exit(newListLiteral);
        end;
        lt_intList: begin
          result:=newListLiteral;
          i1:=length(LHS^.listValue);
          for j:=0 to length(RHS^.listValue)-1 do begin
            i:=RHS^.listValue [j]^.intValue;
            if (i>=0) and (i<i1) then result^.append(LHS^.listValue [i], true);
          end;
          exit(result);
        end;
        lt_booleanList: begin
          result:=newListLiteral;
          i1:=length(LHS^.listValue);
          if i1 = length(RHS^.listValue) then
          for i:=0 to length(RHS^.listValue)-1 do
          if odd(RHS^.listValue[i]^.intValue) then
            result^.append(LHS^.listValue [i], true);
          exit(result);
        end;
        lt_string: if LHS^.isKeyValueList then begin
          key:=RHS^.stringValue;
          for i:=0 to length(LHS^.listValue)-1 do
          if LHS^.listValue [i]^.listValue[0]^.stringValue = key then begin
            result:=LHS^.listValue[i]^.listValue [1];
            result^.rereference;
            exit(result);
          end;
          exit(newListLiteral);
        end else exit(newErrorLiteralRaising('Operator % with a string as second operand can only be applied to key-value-lists!', tokenLocation));
        lt_stringList: if LHS^.isKeyValueList then begin
          result:=newListLiteral;
          for j:=0 to length(RHS^.listValue)-1 do begin
            key:=RHS^.listValue[j]^.stringValue;
            i:=0; while i<length(LHS^.listValue) do
            if LHS^.listValue[i]^.listValue [0]^.stringValue = key then begin
              result^.append(LHS^.listValue[i]^.listValue [1], true);
              i:=length(LHS^.listValue);
            end else Inc(i);
          end;
          exit(result);
        end else exit(newErrorLiteralRaising('Operator % with a stringList as second operand can only be applied to key-value-lists!', tokenLocation));
        lt_emptyList: exit(newListLiteral);
      end;
      tt_operatorExtractL1: if LHS^.trueType in C_validListTypes then begin
        result:=newListLiteral;
        for i:=0 to length(LHS^.listValue)-1 do
          result^.append(resolveOperator(
            LHS^.listValue [i], tt_operatorExtractL0,
            RHS, tokenLocation), false);
        exit(result);
      end;
      tt_operatorExtractL2: if LHS^.trueType in C_validListTypes then begin
        result:=newListLiteral;
        for i:=0 to length(LHS^.listValue)-1 do
          result^.append(resolveOperator(
            LHS^.listValue [i], tt_operatorExtractL1,
            RHS, tokenLocation), false);
        exit(result);
      end;
      tt_operatorExtractL3: if LHS^.trueType in C_validListTypes then begin
        result:=newListLiteral;
        for i:=0 to length(LHS^.listValue)-1 do
          result^.append(resolveOperator(
            LHS^.listValue [i], tt_operatorExtractL2,
            RHS, tokenLocation), false);
        exit(result);
      end;
    end;
    //---------------------------------------------:HANDLE S x S -> S OPERATORS
    //HANDLE ERROR, VOID AND EXPRESSION LITERALS:-------------------------------
    case LHS^.trueType of
      lt_void:                   begin RHS^.rereference; exit(RHS); end;
      lt_error,lt_listWithError: begin LHS^.rereference; exit(LHS); end;
      lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, op, RHS, tokenLocation)));
    end;
    case RHS^.trueType of
      lt_void:                   begin LHS^.rereference; exit(LHS); end;
      lt_error,lt_listWithError: begin RHS^.rereference; exit(RHS); end;
      lt_expression: exit(newExpressionLiteral(subruleApplyOpCallback(LHS, op, RHS, tokenLocation)));
    end;
    //-------------------------------:HANDLE ERROR, VOID AND EXPRESSION LITERALS
    result:=newErrorLiteralRaising(LHS^.trueType, RHS^.trueType, op, tokenLocation);
  end;

CONSTRUCTOR T_namedVariable.create(CONST initialId:ansistring; CONST initialValue:P_literal);
  begin
    id:=initialId;
    value:=initialValue;
    value^.rereference;
  end;

DESTRUCTOR T_namedVariable.destroy;
  begin
    disposeLiteral(value);
  end;

PROCEDURE T_namedVariable.setValue(CONST newValue:P_literal);
  begin
    disposeLiteral(value);
    value:=newValue;
    value^.rereference;
  end;

FUNCTION T_namedVariable.mutate(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):P_literal;
  CONST MAPPED_OP:array[tt_cso_assignPlus..tt_cso_assignDiv] of T_tokenType=(tt_operatorPlus,tt_operatorMinus,tt_operatorMult,tt_operatorDivReal);
  VAR oldValue:P_literal;
  begin
    oldValue:=value;
    case mutation of
      tt_cso_assignPlus..tt_cso_assignDiv: begin
        result:=resolveOperator(oldValue, MAPPED_OP[mutation], RHS, location);
        disposeLiteral(oldValue);
        value:=result;
        result^.rereference;
      end;
      tt_cso_assignStrConcat: begin
        if (oldValue^.trueType=lt_string) and (oldValue^.getReferenceCount=1) and (RHS^.trueType in [lt_boolean..lt_string]) then begin
          oldValue^.appendString(RHS^.stringForm);
          result:=oldValue;
          result^.rereference;
        end else begin
          result:=resolveOperator(oldValue, tt_operatorStrConcat, RHS, location);
          disposeLiteral(oldValue);
          value:=result;
          result^.rereference;
        end;
      end;
      tt_cso_assignAppend: begin
        if (oldValue^.trueType in C_validListTypes) and (oldValue^.getReferenceCount=1) then begin
          if (RHS^.trueType in [lt_boolean..lt_expression])
          then oldValue^.append(RHS, true)
          else oldValue^.appendAll(RHS);
          result:=oldValue;
          result^.rereference;
        end else begin
          result:=resolveOperator(oldValue, tt_operatorConcat   , RHS, location);
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

VAR
  i: longint;

INITIALIZATION
  boolLit[false].createBoolean(false);
  boolLit[true].createBoolean(true);
  errLit.init;
  voidLit.createVoid();
  for i:=-127 to 128 do intLit[i].createInt(i);
  DefaultFormatSettings.DecimalSeparator:='.';
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
  for i:=-127 to 128 do intLit[i].destroy;
  {$ifdef debugMode}
  writeln(stdErr,'mnh_litvar finalized');
  {$endif}
end.
