UNIT mnh_operators;
INTERFACE
USES math,
     myGenerics,
     bigint,
     mnh_constants,
     mnh_basicTypes,
     mnh_out_adapters,
     mnh_contexts,
     mnh_litVar,
     mnh_funcs;

{$i mnh_func_defines.inc}
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
VAR intFuncForOperator:array[tt_comparatorEq..tt_operatorConcatAlt] of P_intFuncCallback;
IMPLEMENTATION
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; VAR adapters:T_adapters; CONST threadContext:pointer): P_literal;
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

  {$define defaultLHScases:=
    lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation,threadContext));
    lt_void:       exit(RHS^.rereferenced);
    lt_error:      exit(LHS^.rereferenced)}
  {$define defaultRHSCases:=
    lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation,threadContext));
    lt_void:       exit(LHS^.rereferenced);
    lt_error:      exit(RHS^.rereferenced)}
  {$define S_x_L_recursion:=
    begin
      result:=P_collectionLiteral(RHS)^.newOfSameType(true);
      rhsIt:=P_collectionLiteral(RHS)^.iteratableList;
      for rhsX in rhsIt do P_collectionLiteral(result)^.append(function_id(LHS,rhsX),false);
      disposeLiteral(rhsIt);
      exit(result);
    end}
  {$define L_x_S_recursion:=
    begin
      result:=P_collectionLiteral(LHS)^.newOfSameType(true);
      lhsIt:=P_collectionLiteral(LHS)^.iteratableList;
      for lhsX in lhsIt do P_collectionLiteral(result)^.append(function_id(lhsX,RHS),false);
      disposeLiteral(lhsIt);
      exit(result);
    end}
  {$define L_x_L_recursion:=
    if (LHS^.literalType in C_listTypes) and (RHS^.literalType in C_listTypes) and (P_compoundLiteral(LHS)^.size=P_compoundLiteral(RHS)^.size) then begin
      result:=newListLiteral(P_listLiteral(LHS)^.size);
      for i:=0 to P_listLiteral(LHS)^.size-1 do P_listLiteral(result)^.append(
        function_id(P_listLiteral(LHS)^.value[i],
                    P_listLiteral(RHS)^.value[i]),false);
      exit(result);
    end else if (LHS^.literalType in C_setTypes) and (RHS^.literalType in C_setTypes) then begin
      result:=newSetLiteral;
      lhsIt:=P_collectionLiteral(LHS)^.iteratableList;
      rhsIt:=P_collectionLiteral(RHS)^.iteratableList;
      for lhsX in lhsIt do for rhsX in rhsIt do P_setLiteral(result)^.append(function_id(lhsX,rhsX),false);
      disposeLiteral(lhsIt);
      disposeLiteral(rhsIt);
      exit(result);
    end}

  FUNCTION perform_comparator(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_comparator}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean..lt_string: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean..lt_string: if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(newBoolLiteral(LHS^.isInRelationTo(op,RHS)));
          lt_list..lt_emptySet : if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then S_x_L_recursion;
        end;
        lt_list..lt_emptySet: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean..lt_string: if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then L_x_S_recursion;
          lt_list..lt_emptySet : if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then L_x_L_recursion;
        end;
      end;
      result:=newErrorLiteral('Incompatible comparands '+LHS^.typeString+' and '+RHS^.typeString);
    end;

  FUNCTION perform_and(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_and}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean: exit(newBoolLiteral(P_boolLiteral(LHS)^.value and P_boolLiteral(RHS)^.value));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_booleanList,lt_booleanSet:  S_x_L_recursion;
        end;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int: exit(newIntLiteral(P_intLiteral(LHS)^.value.bitAnd(P_intLiteral(RHS)^.value,-1)));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_intList,lt_intSet: S_x_L_recursion;
        end;
        lt_set ,lt_emptySet ,
        lt_list,lt_emptyList: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean,lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet,lt_intList,lt_intSet: L_x_L_recursion;
        end;
        lt_booleanList,lt_booleanSet: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet: L_x_L_recursion;
        end;
        lt_intList,lt_intSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_intList,lt_intSet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_or(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_or}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean: exit(newBoolLiteral(P_boolLiteral(LHS)^.value or P_boolLiteral(RHS)^.value));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_booleanList,lt_booleanSet:  S_x_L_recursion;
        end;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int: exit(newIntLiteral(P_intLiteral(LHS)^.value.bitOr(P_intLiteral(RHS)^.value,-1)));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_intList,lt_intSet: S_x_L_recursion;
        end;
        lt_set ,lt_emptySet ,
        lt_list,lt_emptyList: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean,lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet,lt_intList,lt_intSet: L_x_L_recursion;
        end;
        lt_booleanList,lt_booleanSet: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet: L_x_L_recursion;
        end;
        lt_intList,lt_intSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_intList,lt_intSet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_xor(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_xor}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean: exit(newBoolLiteral(P_boolLiteral(LHS)^.value xor P_boolLiteral(RHS)^.value));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_booleanList,lt_booleanSet:  S_x_L_recursion;
        end;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int: exit(newIntLiteral(P_intLiteral(LHS)^.value.bitXor(P_intLiteral(RHS)^.value,-1)));
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_intList,lt_intSet: S_x_L_recursion;
        end;
        lt_set ,lt_emptySet ,
        lt_list,lt_emptyList: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean,lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet,lt_intList,lt_intSet: L_x_L_recursion;
        end;
        lt_booleanList,lt_booleanSet: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet: L_x_L_recursion;
        end;
        lt_intList,lt_intSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int: L_x_S_recursion;
          lt_set,lt_emptySet,lt_list,lt_emptyList,lt_intList,lt_intSet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_plus(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_plus}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    {$ifndef debugMode}
                     if (P_intLiteral(LHS)^.value.canBeRepresentedAsInt62) and
                        (P_intLiteral(RHS)^.value.canBeRepresentedAsInt62) then
                     exit(newIntLiteral(P_intLiteral(LHS)^.value.toInt+
                                        P_intLiteral(RHS)^.value.toInt)) else
                     {$endif}
                     exit(newIntLiteral (P_intLiteral(LHS)^.value.plus(P_intLiteral(RHS)^.value)));
          lt_real:   exit(newRealLiteral(P_intLiteral(LHS)^.value.toFloat+P_realLiteral(RHS)^.value));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.value+P_intLiteral (RHS)^.value.toFloat));
          lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.value+P_realLiteral(RHS)^.value));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_string: case RHS^.literalType of
          defaultRHSCases;
          lt_string: exit(newStringLiteral(P_stringLiteral(LHS)^.value+P_stringLiteral(RHS)^.value));
          lt_list,lt_stringList,lt_emptyList,
          lt_set ,lt_stringSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_stringList,lt_stringSet: case RHS^.literalType of
          defaultRHSCases;
          lt_string: L_x_S_recursion;
          lt_list,lt_stringList,
          lt_set ,lt_stringSet , lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real,lt_string: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_stringList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_minus(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_minus}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    {$ifndef debugMode}
                     if (P_intLiteral(LHS)^.value.canBeRepresentedAsInt62) and
                        (P_intLiteral(RHS)^.value.canBeRepresentedAsInt62) then
                     exit(newIntLiteral(P_intLiteral(LHS)^.value.toInt-
                                        P_intLiteral(RHS)^.value.toInt)) else
                     {$endif}
                     exit(newIntLiteral (P_intLiteral(LHS)^.value.minus(P_intLiteral (RHS)^.value)));
          lt_real:   exit(newRealLiteral(P_intLiteral(LHS)^.value.toFloat-P_realLiteral(RHS)^.value));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.value-P_intLiteral (RHS)^.value.toFloat));
          lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.value-P_realLiteral(RHS)^.value));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_mult(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_mult}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    {$ifndef debugMode}
                     if (P_intLiteral(LHS)^.value.canBeRepresentedAsInt32) and
                        (P_intLiteral(RHS)^.value.canBeRepresentedAsInt32) then
                     exit(newIntLiteral(P_intLiteral(LHS)^.value.toInt*
                                        P_intLiteral(RHS)^.value.toInt)) else
                     {$endif}
                     exit(newIntLiteral (P_intLiteral(LHS)^.value.mult(P_intLiteral (RHS)^.value)));
          lt_real:   exit(newRealLiteral(P_intLiteral(LHS)^.value.toFloat*P_realLiteral(RHS)^.value));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.value*P_intLiteral (RHS)^.value.toFloat));
          lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.value*P_realLiteral(RHS)^.value));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_divReal(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_divReal}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    exit(newRealLiteral(P_intLiteral(LHS)^.value.toFloat/P_intLiteral (RHS)^.value.toFloat));
          lt_real:   exit(newRealLiteral(P_intLiteral(LHS)^.value.toFloat/P_realLiteral(RHS)^.value));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.value/P_intLiteral (RHS)^.value.toFloat));
          lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.value/P_realLiteral(RHS)^.value));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_divInt(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_divInt}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int: if P_intLiteral(RHS)^.value.isZero
                  then exit(newRealLiteral(Nan))
                  else exit(newIntLiteral(P_intLiteral(LHS)^.value.divide(P_intLiteral(RHS)^.value)));
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_numList,
        lt_intSet ,lt_numSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int: L_x_S_recursion;
          lt_list,lt_intList,
          lt_set ,lt_intSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRHSCases;
          lt_int: L_x_S_recursion;
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_mod(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_mod}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int: if P_intLiteral(RHS)^.value.isZero
                  then exit(newRealLiteral(Nan))
                  else exit(newIntLiteral(P_intLiteral(LHS)^.value.modulus(P_intLiteral(RHS)^.value)));
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_numList,
        lt_intSet ,lt_numSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int: L_x_S_recursion;
          lt_list,lt_intList,
          lt_set ,lt_intSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRHSCases;
          lt_int: L_x_S_recursion;
          lt_list,lt_intList,lt_emptyList,
          lt_set ,lt_intSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_pot(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_pot}
    FUNCTION pot_int_int(CONST x, y: T_bigInt): P_scalarLiteral;
      VAR exponent:int64;
          tx, rx: T_myFloat;
      begin
        if not(y.canBeRepresentedAsInt32) then begin
          adapters.raiseError('Huge exponents are unimplemented',tokenLocation);
          exit(newVoidLiteral);
        end;
        exponent:=y.toInt;
        if exponent>=0
        then result:=newIntLiteral(x.pow(exponent))
        else begin
          rx:=x.toFloat;
          tx:=1;
          exponent:=-exponent;
          while exponent>0 do begin
            if odd(exponent) then tx:=tx*rx;
            rx:=rx*rx;
            exponent:=exponent shr 1;
          end;
          result:=newRealLiteral(1/tx);
        end;
      end;

    FUNCTION pot_real_int(x: T_myFloat; CONST y: T_bigInt): P_scalarLiteral;
      VAR exponent:int64;
          resultVal:T_myFloat=1;
      begin
        if not(y.canBeRepresentedAsInt32) then begin
          adapters.raiseError('Huge exponents are unimplemented',tokenLocation);
          exit(newVoidLiteral);
        end;
        exponent:=y.toInt;
        if exponent<0 then begin
          exponent:=-exponent;
          x:=1/x;
        end;
        while exponent>0 do begin
          if odd(exponent) then resultVal*=x;
          x:=x*x;
          exponent:=exponent shr 1;
        end;
        result:=newRealLiteral(resultVal);
      end;

    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_int: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    exit(pot_int_int(P_intLiteral(LHS)^.value,P_intLiteral (RHS)^.value));
          lt_real:   exit(newRealLiteral(exp(ln(P_intLiteral(LHS)^.value.toFloat)*P_realLiteral(RHS)^.value)));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_real: case RHS^.literalType of
          defaultRHSCases;
          lt_int:    exit(pot_real_int(P_realLiteral(LHS)^.value,P_intLiteral(RHS)^.value));
          lt_real:   exit(newRealLiteral(exp(ln(P_realLiteral(LHS)^.value)*P_realLiteral(RHS)^.value)));
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: S_x_L_recursion;
        end;
        lt_intList,lt_realList,lt_numList,
        lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
        lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
          defaultRHSCases;
          lt_int,lt_real: L_x_S_recursion;
          lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
          lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: L_x_L_recursion;
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_strConcat(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    {$define function_id:=perform_strConcat}
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_boolean..lt_string: case RHS^.literalType of
          defaultRHSCases;
          lt_boolean..lt_string: exit(newStringLiteral(P_scalarLiteral(LHS)^.stringForm+P_scalarLiteral(RHS)^.stringForm));
          lt_list..lt_emptySet:  S_x_L_recursion;
        end;
        lt_list..lt_emptySet: case RHS^.literalType of
          defaultRHSCases;
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
          defaultRHSCases;
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
          defaultRHSCases;
          lt_boolean..lt_string: exit(newListLiteral(P_listLiteral(LHS)^.size+1)^
                                      .appendAll(P_listLiteral(LHS))^
                                      .append(RHS,true));
          lt_list..lt_emptyMap:  exit(newListLiteral(P_listLiteral(LHS)^.size+P_listLiteral(RHS)^.size)^
                                      .appendAll(P_listLiteral(LHS))^
                                      .appendAll(P_compoundLiteral(RHS)));
        end;
        lt_set ..lt_emptySet : case RHS^.literalType of
          defaultRHSCases;
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
          defaultRHSCases;
          lt_map..lt_emptyMap:  exit(newMapLiteral^
                                     .putAll(P_mapLiteral(LHS))^
                                     .putAll(P_mapLiteral(RHS)));
          lt_list..lt_emptySet: exit(P_collectionLiteral(RHS)^.newOfSameType(true)^
                                     .appendAll(P_mapLiteral(LHS))^
                                     .appendAll(P_setLiteral(RHS)));
        end;
      end;
      result:=defaultErrorLiteral;
    end;

  FUNCTION perform_concatAlt(CONST LHS:P_literal; CONST RHS:P_literal):P_literal;
    begin
      case LHS^.literalType of
        defaultLHScases;
        lt_list..lt_emptyList: case RHS^.literalType of
          lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation,threadContext));
          lt_error:      exit(RHS^.rereferenced)
          else
          exit(newListLiteral(P_listLiteral(LHS)^.size+1)^
               .appendAll(P_listLiteral(LHS))^
               .append(RHS,true,true));
        end;
        lt_set ..lt_emptySet : case RHS^.literalType of
          lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation,threadContext));
          lt_error:      exit(RHS^.rereferenced)
          else exit(newSetLiteral^
               .appendAll(P_setLiteral(LHS))^
               .append(RHS,true,true));
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
      tt_operatorConcatAlt:result:=perform_concatAlt(LHS,RHS);
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

{$define funcForOp:=begin if (params<>nil) and (params^.size=2) then result:=resolveOperator(arg0,OP,arg1,tokenLocation,context.adapters^,@context) else result:=nil; end}
{$define OP:=tt_comparatorEq     } FUNCTION funcFor_comparatorEq      intFuncSignature; funcForOp;
{$define OP:=tt_comparatorNeq    } FUNCTION funcFor_comparatorNeq     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorLeq    } FUNCTION funcFor_comparatorLeq     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorGeq    } FUNCTION funcFor_comparatorGeq     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorLss    } FUNCTION funcFor_comparatorLss     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorGrt    } FUNCTION funcFor_comparatorGrt     intFuncSignature; funcForOp;
{$define OP:=tt_comparatorListEq } FUNCTION funcFor_comparatorListEq  intFuncSignature; funcForOp;
{$define OP:=tt_operatorAnd      } FUNCTION funcFor_operatorAnd       intFuncSignature; funcForOp;
{$define OP:=tt_operatorOr       } FUNCTION funcFor_operatorOr        intFuncSignature; funcForOp;
{$define OP:=tt_operatorXor      } FUNCTION funcFor_operatorXor       intFuncSignature; funcForOp;
FUNCTION funcFor_operatorLazyAnd intFuncSignature;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_boolean) then begin
      if bool0^.value then result:=arg1 else result:=arg0;
      result^.rereference;
    end else result:=nil;
  end;

FUNCTION funcFor_operatorLazyOr  intFuncSignature;
  begin
    if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_boolean) then begin
      if bool0^.value then result:=arg0 else result:=arg1;
      result^.rereference;
    end else result:=nil;
  end;
{$define OP:=tt_operatorPlus     } FUNCTION funcFor_operatorPlus      intFuncSignature; funcForOp;
{$define OP:=tt_operatorMinus    } FUNCTION funcFor_operatorMinus     intFuncSignature; funcForOp;
{$define OP:=tt_operatorMult     } FUNCTION funcFor_operatorMult      intFuncSignature; funcForOp;
{$define OP:=tt_operatorDivReal  } FUNCTION funcFor_operatorDivReal   intFuncSignature; funcForOp;
{$define OP:=tt_operatorDivInt   } FUNCTION funcFor_operatorDivInt    intFuncSignature; funcForOp;
{$define OP:=tt_operatorMod      } FUNCTION funcFor_operatorMod       intFuncSignature; funcForOp;
{$define OP:=tt_operatorPot      } FUNCTION funcFor_operatorPot       intFuncSignature; funcForOp;
{$define OP:=tt_operatorStrConcat} FUNCTION funcFor_operatorStrConcat intFuncSignature; funcForOp;
FUNCTION funcFor_operatorOrElse intFuncSignature;
  begin
    if (params<>nil) and ((params^.size=1) or (params^.size=2)) then begin
      result:=arg0;
      result^.rereference;
    end else result:=nil;
  end;
{$define OP:=tt_operatorConcat   } FUNCTION funcFor_operatorConcat intFuncSignature; funcForOp;
{$define OP:=tt_operatorIn       } FUNCTION funcFor_operatorIn     intFuncSignature; funcForOp;
{$undef OP}
{$undef funcForOp}
INITIALIZATION
  mnh_litVar.resolveOperator:=@resolveOperator;
  intFuncForOperator[tt_comparatorEq     ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::='     ,@funcFor_comparatorEq     ,ak_binary,'//Function wrapper for operator =');
  intFuncForOperator[tt_comparatorNeq    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::!='    ,@funcFor_comparatorNeq    ,ak_binary,'//Function wrapper for operator !=');
  intFuncForOperator[tt_comparatorLeq    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::<='    ,@funcFor_comparatorLeq    ,ak_binary,'//Function wrapper for operator <=');
  intFuncForOperator[tt_comparatorGeq    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::>='    ,@funcFor_comparatorGeq    ,ak_binary,'//Function wrapper for operator >=');
  intFuncForOperator[tt_comparatorLss    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::<'     ,@funcFor_comparatorLss    ,ak_binary,'//Function wrapper for operator <');
  intFuncForOperator[tt_comparatorGrt    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::>'     ,@funcFor_comparatorGrt    ,ak_binary,'//Function wrapper for operator >');
  intFuncForOperator[tt_comparatorListEq ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::=='    ,@funcFor_comparatorListEq ,ak_binary,'//Function wrapper for operator ==');
  intFuncForOperator[tt_operatorAnd      ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::and'   ,@funcFor_operatorAnd      ,ak_binary,'//Function wrapper for operator and');
  intFuncForOperator[tt_operatorOr       ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::or'    ,@funcFor_operatorOr       ,ak_binary,'//Function wrapper for operator or');
  intFuncForOperator[tt_operatorXor      ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::xor'   ,@funcFor_operatorXor      ,ak_binary,'//Function wrapper for operator xor');
  intFuncForOperator[tt_operatorLazyAnd  ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::AND'   ,@funcFor_operatorLazyAnd  ,ak_binary,'//Function wrapper for operator AND#');
  intFuncForOperator[tt_operatorLazyOr   ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::OR'    ,@funcFor_operatorLazyOr   ,ak_binary,'//Function wrapper for operator OR'    );
  intFuncForOperator[tt_operatorPlus     ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::+'     ,@funcFor_operatorPlus     ,ak_binary,'//Function wrapper for operator +'     );
  intFuncForOperator[tt_operatorMinus    ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::-'     ,@funcFor_operatorMinus    ,ak_binary,'//Function wrapper for operator -'     );
  intFuncForOperator[tt_operatorMult     ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::*'     ,@funcFor_operatorMult     ,ak_binary,'//Function wrapper for operator *'     );
  intFuncForOperator[tt_operatorDivReal  ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::/'     ,@funcFor_operatorDivReal  ,ak_binary,'//Function wrapper for operator /'     );
  intFuncForOperator[tt_operatorDivInt   ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::div'   ,@funcFor_operatorDivInt   ,ak_binary,'//Function wrapper for operator div'   );
  intFuncForOperator[tt_operatorMod      ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::mod'   ,@funcFor_operatorMod      ,ak_binary,'//Function wrapper for operator mod'   );
  intFuncForOperator[tt_operatorPot      ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::^'     ,@funcFor_operatorPot      ,ak_binary,'//Function wrapper for operator ^'     );
  intFuncForOperator[tt_operatorStrConcat]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::&'     ,@funcFor_operatorStrConcat,ak_binary,'//Function wrapper for operator &'     );
  intFuncForOperator[tt_operatorOrElse   ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::orElse',@funcFor_operatorOrElse   ,ak_binary,'//Function wrapper for operator orElse');
  intFuncForOperator[tt_operatorConcat   ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::|'     ,@funcFor_operatorConcat   ,ak_binary,'//Function wrapper for operator |'     );
  intFuncForOperator[tt_operatorIn       ]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,'::in'    ,@funcFor_operatorIn       ,ak_binary,'//Function wrapper for operator in'    );

end.
