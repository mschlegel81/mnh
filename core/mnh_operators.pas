UNIT mnh_operators;
INTERFACE
USES math,
     sysutils,
     myGenerics,
     bigint,
     mnh_constants,
     mnh_basicTypes,
     mnh_out_adapters,
     mnh_tokenArray,
     mnh_contexts,
     mnh_litVar,
     mnh_tokens,
     mnh_funcs;

{$i mnh_func_defines.inc}
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; CONST context:pointer): P_literal;
CONST allOperators:T_tokenTypeSet=[tt_comparatorEq..tt_operatorConcatAlt];
      overridableOperators:T_tokenTypeSet=[
      tt_comparatorEq     ,
      tt_comparatorNeq    ,
      tt_comparatorLeq    ,
      tt_comparatorGeq    ,
      tt_comparatorLss    ,
      tt_comparatorGrt    ,
      tt_operatorAnd      ,
      tt_operatorOr       ,
      tt_operatorXor      ,
      tt_operatorPlus     ,
      tt_operatorMinus    ,
      tt_operatorMult     ,
      tt_operatorDivReal  ,
      tt_operatorDivInt   ,
      tt_operatorMod      ,
      tt_operatorPot      ,
      tt_operatorStrConcat];

CONST operatorName:array[tt_comparatorEq..tt_operatorConcatAlt] of string=
      ('COMPARATOR_EQ',
       'COMPARATOR_NEQ',
       'COMPARATOR_LEQ',
       'COMPARATOR_GEQ',
       'COMPARATOR_LSS',
       'COMPARATOR_GRT',
       'COMPARATOR_LISTEQ ',
       'OPERATOR_IN',
       'OPERATOR_AND',
       'OPERATOR_OR',
       'OPERATOR_XOR',
       'OPERATOR_LAZYAND',
       'OPERATOR_LAZYOR',
       'OPERATOR_PLUS',
       'OPERATOR_MINUS',
       'OPERATOR_MULT',
       'OPERATOR_DIVREAL',
       'OPERATOR_DIVINT',
       'OPERATOR_MOD',
       'OPERATOR_POT',
       'OPERATOR_STRCONCAT',
       'OPERATOR_ORELSE',
       'OPERATOR_CONCAT',
       'OPERATOR_CONCATALT');

//FUNCTION comparator_eq      intFuncSignature;
//FUNCTION comparator_Neq     intFuncSignature;
//FUNCTION comparator_Leq     intFuncSignature;
//FUNCTION comparator_Geq     intFuncSignature;
//FUNCTION comparator_Lss     intFuncSignature;
//FUNCTION comparator_Grt     intFuncSignature;
//FUNCTION comparator_ListEq  intFuncSignature;
//FUNCTION operator_And       intFuncSignature;
//FUNCTION operator_Or        intFuncSignature;
//FUNCTION operator_Xor       intFuncSignature;
//FUNCTION operator_And       intFuncSignature;
//FUNCTION operator_Or        intFuncSignature;
//FUNCTION operator_Plus      intFuncSignature;
//FUNCTION operator_Minus     intFuncSignature;
//FUNCTION operator_Mult      intFuncSignature;
//FUNCTION operator_DivReal   intFuncSignature;
//FUNCTION operator_DivInt    intFuncSignature;
//FUNCTION operator_Mod       intFuncSignature;
//FUNCTION operator_Pot       intFuncSignature;
FUNCTION operator_StrConcat intFuncSignature;
//FUNCTION operator_OrElse    intFuncSignature;
//FUNCTION operator_Concat    intFuncSignature;
//FUNCTION operator_ConcatAlt intFuncSignature;
//FUNCTION operator_In        intFuncSignature;

IMPLEMENTATION
TYPE P_op=FUNCTION(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
VAR OP_IMPL:array[tt_comparatorEq..tt_operatorConcatAlt] of P_op;

FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; CONST context:pointer): P_literal;
  VAR parList:P_listLiteral;
      rule   :P_abstractRule =nil;
      ruleOut:P_token=nil;
      dummy  :P_token=nil;
  begin
    rule:=P_abstractPackage(tokenLocation.package)^.customOperatorRule[op];
    if (rule<>nil) then begin
      if P_threadContext(context)^.callDepth>=STACK_DEPTH_LIMIT then begin
        P_threadContext(context)^.adapters^.raiseError('Stack overflow in overridden comparator',tokenLocation);
        exit(newVoidLiteral);
      end;
      parList:=newListLiteral(2);
      parList^.append(LHS,true)^.append(RHS,true);
      inc(P_threadContext(context)^.callDepth);
      if rule^.replaces(tt_localUserRule,tokenLocation,parList,ruleOut,dummy,context) then begin
        result:=P_threadContext(context)^.reduceToLiteral(ruleOut).literal;
        dec(P_threadContext(context)^.callDepth);
        disposeLiteral(parList);
        if result=nil
        then exit(newVoidLiteral)
        else exit(result);
      end;
      dec(P_threadContext(context)^.callDepth);
      disposeLiteral(parList);
    end;
    result:=OP_IMPL[op](LHS,RHS,tokenLocation,P_threadContext(context)^);
    if result=nil then begin
      P_threadContext(context)^.adapters^.raiseError('Incompatible operators '+LHS^.typeString+' and '+RHS^.typeString+' for operator '+C_tokenInfo[op].defaultId,tokenLocation);
      result:=newVoidLiteral;
    end else if (result^.literalType in C_compoundTypes) and (P_compoundLiteral(result)^.containsError) then begin
      P_threadContext(context)^.adapters^.raiseError('Incompatible operators '+LHS^.typeString+' and '+RHS^.typeString+' for operator '+C_tokenInfo[op].defaultId,tokenLocation);
      disposeLiteral(result);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION perform_listEq(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  begin
    result:=newBoolLiteral(LHS^.isInRelationTo(tt_comparatorListEq,RHS));
  end;

FUNCTION comparator_ListEq intFuncSignature;
  begin
    result:=nil;
    if params^.size=2 then exit(newBoolLiteral(arg0^.isInRelationTo(tt_comparatorListEq,arg1)))
                      else exit(newBoolLiteral(false));
  end;

FUNCTION perform_OrElse(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  begin
    if LHS^.literalType=lt_void then exit(RHS^.rereferenced) else exit(LHS^.rereferenced);
  end;

FUNCTION operator_OrElse   intFuncSignature;
  begin
    result:=nil;
    if params^.size=0 then exit(newVoidLiteral)
    else exit(arg0^.rereferenced);
  end;

FUNCTION perform_In(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  begin
    exit(newBoolLiteral(LHS^.isInRelationTo(tt_operatorIn,RHS)));
  end;

FUNCTION operator_In       intFuncSignature;
  begin
    result:=nil;
    if params^.size=2 then exit(newBoolLiteral(arg0^.isInRelationTo(tt_operatorIn,arg1)))
                      else exit(newBoolLiteral(false));
  end;

{$define defaultLHScases:=
  lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation,@context));
  lt_void:       exit(RHS^.rereferenced);
  lt_error:      exit(LHS^.rereferenced)}
{$define defaultRHSCases:=
  lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation,@context));
  lt_void:       exit(LHS^.rereferenced);
  lt_error:      exit(RHS^.rereferenced)}
{$define generic_recursions:=
    FUNCTION recurse_SL:P_literal;
    VAR rhsIt:T_arrayOfLiteral;
        rhsX :P_literal;
    begin
      result:=P_collectionLiteral(RHS)^.newOfSameType(true);
      rhsIt:=P_collectionLiteral(RHS)^.iteratableList;
      for rhsX in rhsIt do P_collectionLiteral(result)^.append(function_id(LHS,rhsX,tokenLocation,context),false);
      disposeLiteral(rhsIt);
    end;

  FUNCTION recurse_LS:P_literal;
    VAR lhsIt:T_arrayOfLiteral;
        lhsX :P_literal;
    begin
      result:=P_collectionLiteral(LHS)^.newOfSameType(true);
      lhsIt:=P_collectionLiteral(LHS)^.iteratableList;
      for lhsX in lhsIt do P_collectionLiteral(result)^.append(function_id(lhsX,RHS,tokenLocation,context),false);
      disposeLiteral(lhsIt);
    end;

  FUNCTION recurse_LL:P_literal;
    VAR i:longint;
        lhsIt,rhsIt:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
    begin
      if (LHS^.literalType in C_listTypes) and (RHS^.literalType in C_listTypes) and (P_compoundLiteral(LHS)^.size=P_compoundLiteral(RHS)^.size) then begin
        result:=newListLiteral(P_listLiteral(LHS)^.size);
        for i:=0 to P_listLiteral(LHS)^.size-1 do P_listLiteral(result)^.append(
          function_id(P_listLiteral(LHS)^.value[i],
                      P_listLiteral(RHS)^.value[i],tokenLocation,context),false);
      end else if (LHS^.literalType in C_setTypes) and (RHS^.literalType in C_setTypes) then begin
        result:=newSetLiteral;
        lhsIt:=P_collectionLiteral(LHS)^.iteratableList;
        rhsIt:=P_collectionLiteral(RHS)^.iteratableList;
        for lhsX in lhsIt do for rhsX in rhsIt do P_setLiteral(result)^.append(function_id(lhsX,rhsX,tokenLocation,context),false);
        disposeLiteral(lhsIt);
        disposeLiteral(rhsIt);
      end else result:=nil;
    end}

{$define comparator_implementation:=
FUNCTION function_id(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  generic_recursions;
  begin
    if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then
    case LHS^.literalType of
      defaultLHScases;
      lt_boolean..lt_string: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(newBoolLiteral(LHS^.isInRelationTo(op,RHS)));
        lt_list..lt_emptySet : if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(recurse_SL);
      end;
      lt_list..lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(recurse_LS);
        lt_list..lt_emptySet : if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;

FUNCTION outerFunc_id intFuncSignature;
  VAR rule   :P_abstractRule =nil;
      ruleOut:P_token=nil;
      dummy  :P_token=nil;
  begin
    rule:=P_abstractPackage(tokenLocation.package)^.customOperatorRule[op];
    if (rule<>nil) then begin
      if context.callDepth>=STACK_DEPTH_LIMIT then begin
        context.adapters^.raiseError('Stack overflow in overridden comparator',tokenLocation);
        exit(nil);
      end;
      inc(context.callDepth);
      if rule^.replaces(tt_localUserRule,tokenLocation,params,ruleOut,dummy,@context) then begin
        result:=context.reduceToLiteral(ruleOut).literal;
        dec(context.callDepth);
        exit(result);
      end else dec(context.callDepth);
    end;
    if (params<>nil) and (params^.size=2)
    then begin
      result:=function_id(arg0,arg1,tokenLocation,context);
      if result=nil then context.adapters^.raiseError('Incompatible comparands '+arg0^.typeString+' and '+arg1^.typeString,tokenLocation);
    end else if (params<>nil) and (params^.size=1)
    then exit(arg0^.rereferenced)
    else if (params=nil) or (params^.size=0) then exit(newVoidLiteral);
  end}

{$define function_id:=perform_eq}
{$define outerFunc_id:=comparator_eq}
{$define op:=tt_comparatorEq}
comparator_implementation;

{$define function_id:=perform_Neq}
{$define outerFunc_id:=comparator_Neq}
{$define op:=tt_comparatorNeq}
comparator_implementation;
{$define function_id:=perform_Leq}
{$define outerFunc_id:=comparator_Leq}
{$define op:=tt_comparatorLeq}
comparator_implementation;
{$define function_id:=perform_Geq}
{$define outerFunc_id:=comparator_Geq}
{$define op:=tt_comparatorGeq}
comparator_implementation;
{$define function_id:=perform_Lss}
{$define outerFunc_id:=comparator_Lss}
{$define op:=tt_comparatorLss}
comparator_implementation;
{$define function_id:=perform_Grt}
{$define outerFunc_id:=comparator_Grt}
{$define op:=tt_comparatorGrt}
comparator_implementation;
{$undef comparator_implementation}

{$define boolIntOperator:=FUNCTION function_id(CONST LHS:P_literal; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_boolean: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean: exit(newBoolLiteral(P_boolLiteral(LHS)^.value boolOp P_boolLiteral(RHS)^.value));
        lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_booleanList,lt_booleanSet: exit(recurse_SL);
      end;
      lt_int: case RHS^.literalType of
        defaultRHSCases;
        lt_int: exit(newIntLiteral(P_intLiteral(LHS)^.value.bitOp(P_intLiteral(RHS)^.value,-1)));
        lt_set,lt_emptySet,lt_list,lt_emptyList,lt_map,lt_emptyMap,lt_intList,lt_intSet: exit(recurse_SL);
      end;
      lt_set ,lt_emptySet ,
      lt_list,lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean,lt_int: exit(recurse_LS);
        lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet,lt_intList,lt_intSet: exit(recurse_LL);
      end;
      lt_booleanList,lt_booleanSet: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean: exit(recurse_LS);
        lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet: exit(recurse_LL);
      end;
      lt_intList,lt_intSet: case RHS^.literalType of
        defaultRHSCases;
        lt_int: exit(recurse_LS);
        lt_set,lt_emptySet,lt_list,lt_emptyList,lt_intList,lt_intSet: exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;

FUNCTION outerFunc_id intFuncSignature;
  VAR rule   :P_abstractRule=nil;
      ruleOut:P_token=nil;
      dummy  :P_token=nil;
  begin
    rule:=P_abstractPackage(tokenLocation.package)^.customOperatorRule[op];
    if (rule<>nil) then begin
      if context.callDepth>=STACK_DEPTH_LIMIT then begin
        context.adapters^.raiseError('Stack overflow in overridden comparator',tokenLocation);
        exit(nil);
      end;
      inc(context.callDepth);
      if rule^.replaces(tt_localUserRule,tokenLocation,params,ruleOut,dummy,@context) then begin
        result:=context.reduceToLiteral(ruleOut).literal;
        dec(context.callDepth);
        exit(result);
      end else dec(context.callDepth);
    end;
    if (params<>nil) and (params^.size=2)
    then begin
      result:=function_id(arg0,arg1,tokenLocation,context);
      if result=nil then context.adapters^.raiseError('Incompatible operators '+arg0^.typeString+' and '+arg1^.typeString+' for operator '+C_tokenInfo[op].defaultId,tokenLocation);
    end else if (params<>nil) and (params^.size=1)
    then exit(arg0^.rereferenced)
    else if (params=nil) or (params^.size=0) then exit(newVoidLiteral);
  end}

{$define function_id:=perform_and}
{$define outerFunc_id:=operator_and}
{$define op:=tt_operatorAnd}
{$define boolOp:=and}
{$define bitOp:=bitAnd}
boolIntOperator;
{$define function_id:=perform_or}
{$define outerFunc_id:=operator_or}
{$define op:=tt_operatorOr}
{$define boolOp:=or}
{$define bitOp:=bitOr}
boolIntOperator;
{$define function_id:=perform_xor}
{$define outerFunc_id:=operator_xor}
{$define op:=tt_operatorXor}
{$define boolOp:=xor}
{$define bitOp:=bitXor}
boolIntOperator;
{$undef boolIntOperator}

{$define genericOuter:=FUNCTION outerFunc_id intFuncSignature;
  VAR rule   :P_abstractRule=nil;
      ruleOut:P_token=nil;
      dummy  :P_token=nil;
  begin
    rule:=P_abstractPackage(tokenLocation.package)^.customOperatorRule[op];
    if (rule<>nil) then begin
      if context.callDepth>=STACK_DEPTH_LIMIT then begin
        context.adapters^.raiseError('Stack overflow in overridden comparator',tokenLocation);
        exit(nil);
      end;
      inc(context.callDepth);
      if rule^.replaces(tt_localUserRule,tokenLocation,params,ruleOut,dummy,@context) then begin
        result:=context.reduceToLiteral(ruleOut).literal;
        dec(context.callDepth);
        exit(result);
      end else dec(context.callDepth);
    end;
    if (params<>nil) and (params^.size=2) then begin
      result:=function_id(arg0,arg1,tokenLocation,context);
      if result=nil then context.adapters^.raiseError('Incompatible operators '+arg0^.typeString+' and '+arg1^.typeString+' for operator '+C_tokenInfo[op].defaultId,tokenLocation);
    end else if (params<>nil) and (params^.size=1)
    then exit(arg0^.rereferenced)
    else if (params=nil) or (params^.size=0) then exit(newVoidLiteral);
  end}
{$define function_id:=perform_plus}
{$define outerFunc_id:=operator_plus}
{$define op:=tt_operatorPlus}
FUNCTION perform_plus(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  generic_recursions;
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
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.value+P_intLiteral (RHS)^.value.toFloat));
        lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.value+P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_string: case RHS^.literalType of
        defaultRHSCases;
        lt_string: exit(newStringLiteral(P_stringLiteral(LHS)^.value+P_stringLiteral(RHS)^.value));
        lt_list,lt_stringList,lt_emptyList,
        lt_set ,lt_stringSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_intList,lt_realList,lt_numList,
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_int,lt_real: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_LL);
      end;
      lt_stringList,lt_stringSet: case RHS^.literalType of
        defaultRHSCases;
        lt_string: exit(recurse_LS);
        lt_list,lt_stringList,
        lt_set ,lt_stringSet , lt_emptySet: exit(recurse_LL);
      end;
      lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_int,lt_real,lt_string: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,lt_stringList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet: exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_minus}
{$define outerFunc_id:=operator_minus}
{$define op:=tt_operatorMinus}
FUNCTION perform_minus(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  generic_recursions;
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
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.value-P_intLiteral (RHS)^.value.toFloat));
        lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.value-P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_intList,lt_realList,lt_numList,
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_int,lt_real: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_LL);
      end;
      lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_int,lt_real: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_mult}
{$define outerFunc_id:=operator_mult}
{$define op:=tt_operatorMult}
FUNCTION perform_mult(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  generic_recursions;
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
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet:
          {$ifndef debugMode}
          if P_intLiteral(LHS)^.value.isOne then exit(RHS^.rereferenced) else
          {$endif}
          exit(recurse_SL);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.value*P_intLiteral (RHS)^.value.toFloat));
        lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.value*P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_intList,lt_realList,lt_numList,
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_int : {$ifndef debugMode}
                 if P_intLiteral(RHS)^.value.isOne then exit(LHS^.rereferenced) else
                 {$endif}
                 exit(recurse_LS);
        lt_real: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_LL);
      end;
      lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_int,lt_real: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_divReal}
{$define outerFunc_id:=operator_divReal}
{$define op:=tt_operatorDivReal}
FUNCTION perform_divReal(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_int: case RHS^.literalType of
        defaultRHSCases;
        lt_int:    exit(newRealLiteral(P_intLiteral(LHS)^.value.toFloat/P_intLiteral (RHS)^.value.toFloat));
        lt_real:   exit(newRealLiteral(P_intLiteral(LHS)^.value.toFloat/P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_int:    exit(newRealLiteral(P_realLiteral(LHS)^.value/P_intLiteral (RHS)^.value.toFloat));
        lt_real:   exit(newRealLiteral(P_realLiteral(LHS)^.value/P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_intList,lt_realList,lt_numList,
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_int,lt_real: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_LL);
      end;
      lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_int,lt_real: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_divInt}
{$define outerFunc_id:=operator_divInt}
{$define op:=tt_operatorDivInt}
FUNCTION perform_divInt(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_int: case RHS^.literalType of
        defaultRHSCases;
        lt_int: if P_intLiteral(RHS)^.value.isZero
                then exit(newRealLiteral(Nan))
                else exit(newIntLiteral(P_intLiteral(LHS)^.value.divide(P_intLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_emptyList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_intList,lt_numList,
      lt_intSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_int: {$ifndef debugMode}
                if P_intLiteral(RHS)^.value.isOne then exit(LHS^.rereferenced) else
                {$endif}
                exit(recurse_LS);
        lt_list,lt_intList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_LL);
      end;
      lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_int: exit(recurse_LS);
        lt_list,lt_intList,lt_emptyList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_mod}
{$define outerFunc_id:=operator_mod}
{$define op:=tt_operatorMod}
FUNCTION perform_mod(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_int: case RHS^.literalType of
        defaultRHSCases;
        lt_int: if P_intLiteral(RHS)^.value.isZero
                then exit(newRealLiteral(Nan))
                else exit(newIntLiteral(P_intLiteral(LHS)^.value.modulus(P_intLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_emptyList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_intList,lt_numList,
      lt_intSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_int: exit(recurse_LS);
        lt_list,lt_intList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_LL);
      end;
      lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_int: exit(recurse_LS);
        lt_list,lt_intList,lt_emptyList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;

genericOuter;

{$define function_id:=perform_pot}
{$define outerFunc_id:=operator_pot}
{$define op:=tt_operatorPot}
FUNCTION perform_pot(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  FUNCTION pot_int_int(CONST x, y: T_bigInt): P_literal;
    VAR exponent:int64;
        tx, rx: T_myFloat;
    begin
      if not(y.canBeRepresentedAsInt32) then begin
        context.adapters^.raiseError('Huge exponents are unimplemented',tokenLocation);
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

  FUNCTION pot_real_int(x: T_myFloat; CONST y: T_bigInt): P_literal;
    VAR exponent:int64;
        resultVal:T_myFloat=1;
    begin
      if not(y.canBeRepresentedAsInt32) then begin
        context.adapters^.raiseError('Huge exponents are unimplemented',tokenLocation);
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

    generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_int: case RHS^.literalType of
        defaultRHSCases;
        lt_int:    exit(pot_int_int(P_intLiteral(LHS)^.value,P_intLiteral (RHS)^.value));
        lt_real:   exit(newRealLiteral(exp(ln(P_intLiteral(LHS)^.value.toFloat)*P_realLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_int:    exit(pot_real_int(P_realLiteral(LHS)^.value,P_intLiteral(RHS)^.value));
        lt_real:   exit(newRealLiteral(exp(ln(P_realLiteral(LHS)^.value)*P_realLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_SL);
      end;
      lt_intList,lt_realList,lt_numList,
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_int,lt_real: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_LL);
      end;
      lt_list,lt_set,lt_emptyList,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_int,lt_real: exit(recurse_LS);
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_strConcat}
{$define outerFunc_id:=operator_strConcat}
{$define OP:=tt_operatorStrConcat}
FUNCTION perform_strConcat(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_string: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_real: exit(newStringLiteral(P_stringLiteral(LHS)^.value+RHS^.toString));
        lt_string:           exit(newStringLiteral(P_stringLiteral(LHS)^.value+P_stringLiteral(RHS)^.value));
        lt_list..lt_emptySet:  exit(recurse_SL);
      end;
      lt_boolean..lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_real: exit(newStringLiteral(LHS^.toString+RHS^.toString));
        lt_string:           exit(newStringLiteral(LHS^.toString+P_stringLiteral(RHS)^.value));
        lt_list..lt_emptySet:  exit(recurse_SL);
      end;
      lt_list..lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: exit(recurse_LS);
        lt_list..lt_emptySet:  exit(recurse_LL);
      end;
    end;
    result:=nil;
  end;

genericOuter;
{$define genericOuter:=FUNCTION outerFunc_id intFuncSignature;
  begin
    if (params<>nil) and (params^.size=2)
    then begin
      result:=function_id(arg0,arg1,tokenLocation,context);
      if result=nil then context.adapters^.raiseError('Incompatible operators '+arg0^.typeString+' and '+arg1^.typeString+' for operator '+C_tokenInfo[op].defaultId,tokenLocation);
    end else if (params<>nil) and (params^.size=1)
    then exit(arg0^.rereferenced)
    else if (params=nil) or (params^.size=0) then exit(newVoidLiteral);
  end}

{$define function_id:=perform_concat}
{$define outerFunc_id:=operator_concat}
{$define op:=tt_operatorConcat}
FUNCTION perform_concat(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
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
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_concatAlt}
{$define outerFunc_id:=operator_concatAlt}
{$define op:=tt_operatorConcatAlt}
FUNCTION perform_concatAlt(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_list..lt_emptyList: case RHS^.literalType of
        lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation,@context));
        lt_error:      exit(RHS^.rereferenced)
        else
        exit(newListLiteral(P_listLiteral(LHS)^.size+1)^
             .appendAll(P_listLiteral(LHS))^
             .append(RHS,true,true));
      end;
      lt_set ..lt_emptySet : case RHS^.literalType of
        lt_expression: exit(subruleApplyOpCallback(LHS, op, RHS, tokenLocation,@context));
        lt_error:      exit(RHS^.rereferenced)
        else exit(newSetLiteral^
             .appendAll(P_setLiteral(LHS))^
             .append(RHS,true,true));
      end;
    end;
    result:=nil;
  end;
genericOuter;
{$undef OP}

PROCEDURE registerOperator(CONST op:T_tokenType; CONST func:P_intFuncCallback; CONST internal:P_op);
  begin
    OP_IMPL[op]:=internal;
    intFuncForOperator[op]:=registerRule(DEFAULT_BUILTIN_NAMESPACE,operatorName[op],func,ak_binary,
      operatorName[op]+'(x,y);//Function wrapper for operator '+C_tokenInfo[op].defaultId);
  end;

INITIALIZATION
  mnh_litVar.resolveOperatorCallback:=@resolveOperator;
  registerOperator(tt_comparatorEq     ,@comparator_eq     ,@perform_eq);
  registerOperator(tt_comparatorNeq    ,@comparator_Neq    ,@perform_neq);
  registerOperator(tt_comparatorLeq    ,@comparator_Leq    ,@perform_leq);
  registerOperator(tt_comparatorGeq    ,@comparator_Geq    ,@perform_geq);
  registerOperator(tt_comparatorLss    ,@comparator_Lss    ,@perform_lss);
  registerOperator(tt_comparatorGrt    ,@comparator_Grt    ,@perform_grt);
  registerOperator(tt_comparatorListEq ,@comparator_ListEq ,@perform_listEq);
  registerOperator(tt_operatorLazyAnd  ,@operator_And      ,@perform_and      );
  registerOperator(tt_operatorLazyOr   ,@operator_Or       ,@perform_or       );
  registerOperator(tt_operatorAnd      ,@operator_And      ,@perform_and      );
  registerOperator(tt_operatorOr       ,@operator_Or       ,@perform_or       );
  registerOperator(tt_operatorXor      ,@operator_Xor      ,@perform_xor      );
  registerOperator(tt_operatorPlus     ,@operator_Plus     ,@perform_plus     );
  registerOperator(tt_operatorMinus    ,@operator_Minus    ,@perform_minus    );
  registerOperator(tt_operatorMult     ,@operator_Mult     ,@perform_mult     );
  registerOperator(tt_operatorDivReal  ,@operator_DivReal  ,@perform_divReal  );
  registerOperator(tt_operatorDivInt   ,@operator_DivInt   ,@perform_divInt   );
  registerOperator(tt_operatorMod      ,@operator_Mod      ,@perform_mod      );
  registerOperator(tt_operatorPot      ,@operator_Pot      ,@perform_pot      );
  registerOperator(tt_operatorStrConcat,@operator_StrConcat,@perform_strConcat);
  registerOperator(tt_operatorOrElse   ,@operator_OrElse   ,@perform_OrElse   );
  registerOperator(tt_operatorConcat   ,@operator_Concat   ,@perform_concat   );
  registerOperator(tt_operatorConcatAlt,@operator_ConcatAlt,@perform_concatAlt);
  registerOperator(tt_operatorIn       ,@operator_In       ,@perform_In       );

end.
