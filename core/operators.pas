UNIT operators;
INTERFACE
USES mnh_constants,
     basicTypes,
     mnh_messages,
     out_adapters,
     tokenArray,
     contexts,
     litVar,
     subrules,
     recyclers,
     funcs;

{$i func_defines.inc}
FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer): P_literal;
FUNCTION resolveUnaryOperator(CONST op: T_tokenType; CONST operand: P_literal; CONST tokenLocation: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): P_literal;
FUNCTION operator_StrConcat intFuncSignature;
FUNCTION isUnaryOperatorId(CONST id:T_idString):boolean;

IMPLEMENTATION
USES sysutils,
     bigint;

TYPE P_op   =FUNCTION(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
     P_unary=FUNCTION(CONST x:P_literal; CONST opLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
VAR OP_IMPL:array[tt_comparatorEq..tt_operatorConcatAlt] of P_op;
    UN_IMPL:array[tt_unaryOpNegate..tt_unaryOpMinus] of P_unary;
FUNCTION isUnaryOperatorId(CONST id:T_idString):boolean;
  VAR o:T_tokenType;
  begin
    for o in unaryOperators do if operatorName[o]=id then exit(true);
    result:=false;
  end;

FUNCTION unaryNoOp(CONST x:P_literal;{$WARN 5024 OFF} CONST opLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    result:=x^.rereferenced;
  end;

FUNCTION unaryNoOp_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1)
    then result:=arg0^.rereferenced
    else result:=nil;
  end;

FUNCTION logicalNegationOf(CONST x:P_literal; CONST opLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  VAR y,yNeg:P_literal;
      iter:T_arrayOfLiteral;
      containsError:boolean=false;
  begin
    result:=nil;
    case x^.literalType of
      lt_expression: result:=subruleApplyOpImpl(nil,tt_unaryOpNegate,x,opLocation,context,recycler);
      lt_boolean: result:=newBoolLiteral(not(P_boolLiteral(x)^.value));
      lt_list,lt_booleanList,lt_emptyList,
      lt_set ,lt_booleanSet ,lt_emptySet: begin
        result:=P_collectionLiteral(x)^.newOfSameType(recycler,true);
        iter:=P_collectionLiteral(x)^.tempIteratableList;
        for y in iter do begin
          yNeg:=logicalNegationOf(y,opLocation,context,recycler);
          if yNeg=nil
          then containsError:=true
          else P_collectionLiteral(result)^.append(recycler,yNeg,false);
        end;
        if containsError then begin
          raiseNotApplicableError('! (logical negation)',x,opLocation,context);
          recycler^.disposeLiteral(result);
        end;
      end;
      else raiseNotApplicableError('! (logical negation)',x,opLocation,context);
    end;
    if result=nil then result:=newVoidLiteral;
  end;

FUNCTION logicalNegationOf_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1)
    then result:=logicalNegationOf(arg0,tokenLocation,context,recycler)
    else result:=nil;
  end;

FUNCTION arithmeticNegationOf(CONST x:P_literal; CONST opLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  VAR y,yNeg:P_literal;
      iter:T_arrayOfLiteral;
      containsError:boolean=false;
  begin
    result:=nil;
    case x^.literalType of
      lt_expression: result:=subruleApplyOpImpl(nil,tt_unaryOpMinus,x,opLocation,context,recycler);
      lt_bigint    : result:=recycler^.newIntLiteral (P_bigIntLiteral (x)^.value.negated);
      lt_smallint  : result:=recycler^.newIntLiteral(-P_smallIntLiteral(x)^.value);
      lt_real      : result:=recycler^.newRealLiteral(-P_realLiteral(x)^.value);
      lt_list,lt_realList,lt_intList,lt_numList,lt_emptyList,
      lt_set ,lt_realSet ,lt_intSet ,lt_numSet ,lt_emptySet: begin
        result:=P_collectionLiteral(x)^.newOfSameType(recycler,true);
        iter:=P_collectionLiteral(x)^.tempIteratableList;
        for y in iter do begin
          yNeg:=arithmeticNegationOf(y,opLocation,context,recycler);
          if yNeg=nil
          then containsError:=true
          else P_collectionLiteral(result)^.append(recycler,yNeg,false);
        end;
        if containsError then begin
          raiseNotApplicableError('- (arithmetic negation)',x,opLocation,context);
          recycler^.disposeLiteral(result);
        end;
      end;
      else raiseNotApplicableError('- (arithmetic negation)',x,opLocation,context);
    end;
    if result=nil then result:=newVoidLiteral;
  end;

FUNCTION arithmeticNegationOf_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1)
    then result:=arithmeticNegationOf(arg0,tokenLocation,context,recycler)
    else result:=nil;
  end;

FUNCTION resolveUnaryOperator(CONST op: T_tokenType; CONST operand: P_literal; CONST tokenLocation: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): P_literal;
  VAR rule   :P_abstractRule =nil;
      {$ifdef fullVersion}
      wrapped:P_expressionLiteral;
      {$endif}
  begin
    result:=nil;
    rule:=P_abstractPackage(tokenLocation.package)^.customOperatorRule[op];
    if (rule<>nil) then result:=rule^.evaluateToLiteral(tokenLocation,operand,nil,recycler,context);
    if result=nil then begin
      {$ifdef fullVersion}
      if (tco_stackTrace in P_context(context)^.threadOptions)
        and builtinFunctionMap.canGetIntrinsicRuleAsExpression(operatorName[op], wrapped) then begin
        P_context(context)^.callStackPush(tokenLocation,wrapped,nil);
        result:=UN_IMPL[op](operand,tokenLocation,context,recycler);
        P_context(context)^.callStackPop(nil);
      end else
      {$endif}
      result:=UN_IMPL[op](operand,tokenLocation,context,recycler);
    end;
    if result=nil then begin
      context^.raiseError('Incompatible operand '+operand^.typeString+' for operator '+C_tokenDefaultId[op],tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION resolveOperator(CONST LHS: P_literal; CONST op: T_tokenType; CONST RHS: P_literal; CONST tokenLocation: T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer): P_literal;
  VAR rule   :P_abstractRule =nil;
  begin
    result:=nil;
    rule:=P_abstractPackage(tokenLocation.package)^.customOperatorRule[op];
    if (rule<>nil) then result:=rule^.evaluateToLiteral(tokenLocation,LHS,RHS,P_recycler(recycler),context);
    if result=nil then begin
      {$ifdef fullVersion}
      if tco_stackTrace in P_context(context)^.threadOptions then begin
        P_context(context)^.callStackPush(tokenLocation,builtinFunctionMap.getIntrinsicRuleAsExpression(intFuncForOperator[op],false),nil);
        result:=OP_IMPL[op](LHS,RHS,tokenLocation,P_context(context),P_recycler(recycler));
        P_context(context)^.callStackPop(nil);
      end else
      {$endif}
      result:=OP_IMPL[op](LHS,RHS,tokenLocation,P_context(context),P_recycler(recycler));
    end;
    if result=nil then begin
      P_context(context)^.raiseError('Incompatible operands '+LHS^.typeString+' and '+RHS^.typeString+' for operator '+C_tokenDefaultId[op],tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION perform_listEq(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    result:=newBoolLiteral(LHS^.isInRelationTo(tt_comparatorListEq,RHS));
  end;

FUNCTION comparator_ListEq intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2)
    then exit(newBoolLiteral(arg0^.isInRelationTo(tt_comparatorListEq,arg1)))
    else exit(newBoolLiteral(false));
  end;

FUNCTION perform_OrElse(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    if LHS^.literalType=lt_void then exit(RHS^.rereferenced) else exit(LHS^.rereferenced);
  end;

FUNCTION operator_OrElse   intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then exit(newVoidLiteral)
    else exit(arg0^.rereferenced);
  end;

FUNCTION perform_In(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    exit(newBoolLiteral(LHS^.isInRelationTo(tt_operatorIn,RHS)));
  end;

FUNCTION operator_In       intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2)
    then exit(newBoolLiteral(arg0^.isInRelationTo(tt_operatorIn,arg1)))
    else exit(newBoolLiteral(false));
  end;

FUNCTION perform_NotIn(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    exit(newBoolLiteral(LHS^.isInRelationTo(tt_operatorNotIn,RHS)));
  end;

FUNCTION operator_NotIn       intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2)
    then exit(newBoolLiteral(arg0^.isInRelationTo(tt_operatorNotIn,arg1)))
    else exit(newBoolLiteral(false));
  end;

{$define defaultLHScases:=
  lt_expression: exit(subruleApplyOpImpl(LHS, op, RHS, tokenLocation,context,recycler));
  lt_void:       exit(RHS^.rereferenced);
  lt_map,lt_emptyMap: exit(nil)}
{$define defaultRHSCases:=
  lt_expression: exit(subruleApplyOpImpl(LHS, op, RHS, tokenLocation,context,recycler));
  lt_void:       exit(LHS^.rereferenced);
  lt_map,lt_emptyMap: exit(nil)}
{$define generic_recursions:=FUNCTION recurse_scalar_collection:P_literal;
    VAR rhsIt:T_arrayOfLiteral;
        i:longint;
        resultElements:T_arrayOfLiteral;
    begin
      rhsIt:=P_collectionLiteral(RHS)^.tempIteratableList;
      setLength(resultElements,length(rhsIt));
      for i:=0 to length(rhsIt)-1 do begin
        resultElements[i]:=function_id(LHS,rhsIt[i],tokenLocation,context,recycler);
        if resultElements[i]=nil then begin
          setLength(resultElements,i);
          recycler^.disposeLiterals(resultElements);
          exit(nil);
        end;
      end;
      result:=P_collectionLiteral(RHS)^.newOfSameType(recycler,false);
      P_collectionLiteral(result)^.setContents(resultElements,recycler);
    end;

  FUNCTION recurse_collection_scalar:P_literal;
    VAR lhsIt:T_arrayOfLiteral;
        i:longint;
        resultElements:T_arrayOfLiteral;
    begin
      lhsIt:=P_collectionLiteral(LHS)^.tempIteratableList;
      setLength(resultElements,length(lhsIt));
      for i:=0 to length(lhsIt)-1 do begin
        resultElements[i]:=function_id(lhsIt[i],RHS,tokenLocation,context,recycler);
        if resultElements[i]=nil then begin
          setLength(resultElements,i);
          recycler^.disposeLiterals(resultElements);
          exit(nil);
        end;
      end;
      result:=P_collectionLiteral(LHS)^.newOfSameType(recycler,false);
      P_collectionLiteral(result)^.setContents(resultElements,recycler);
    end;

  FUNCTION recurse_list_list:P_literal;
    VAR i:longint;
        resultElements:T_arrayOfLiteral;
    begin
      if  (P_listLiteral(LHS)^.size=P_listLiteral(RHS)^.size) then begin
        setLength(resultElements,P_listLiteral(LHS)^.size);
        for i:=0 to P_listLiteral(LHS)^.size-1 do begin
          resultElements[i]:=function_id(P_listLiteral(LHS)^.value[i],
                                         P_listLiteral(RHS)^.value[i],tokenLocation,context,recycler);
          if resultElements[i]=nil then begin
            setLength(resultElements,i);
            recycler^.disposeLiterals(resultElements);
            exit(nil);
          end;
        end;
        result:=recycler^.newListLiteral(0);
        P_listLiteral(result)^.setContents(resultElements,recycler);
      end else result:=nil;
    end;

  FUNCTION recurse_set_set:P_literal;
    VAR lhsIt,rhsIt,resultElements:T_arrayOfLiteral;
        lhsX ,rhsX :P_literal;
        k:longint=0;
    begin
      if (LHS^.literalType in C_setTypes) and (RHS^.literalType in C_setTypes) then begin
        lhsIt:=P_collectionLiteral(LHS)^.tempIteratableList;
        rhsIt:=P_collectionLiteral(RHS)^.tempIteratableList;
        setLength(resultElements,length(lhsIt)*length(rhsIt));
        for lhsX in lhsIt do for rhsX in rhsIt do begin
          resultElements[k]:=function_id(lhsX,rhsX,tokenLocation,context,recycler);
          if resultElements[k]=nil then begin
            setLength(resultElements,k);
            recycler^.disposeLiterals(resultElements);
            exit(nil);
          end;
          inc(k);
        end;
      end else result:=nil;
      result:=recycler^.newSetLiteral(k);
      P_setLiteral(result)^.setContents(resultElements,recycler);
    end}

{$define comparator_implementation:=
FUNCTION function_id(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal; inline;
  generic_recursions;
  begin
    if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then
    case LHS^.literalType of
      defaultLHScases;
      lt_boolean..lt_string: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(newBoolLiteral(LHS^.isInRelationTo(op,RHS)));
        lt_list..lt_emptySet : if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(recurse_scalar_collection);
      end;
      lt_list..lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(recurse_collection_scalar);
        lt_list..lt_emptyList: if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(recurse_list_list);
      end;
      lt_set..lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(recurse_collection_scalar);
        lt_set..lt_emptySet :  if RHS^.literalType in C_typeInfo[LHS^.literalType].comparableTo then exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;

FUNCTION outerFunc_id intFuncSignature;
  VAR rule:P_abstractRule;
  begin
    result:=nil;
    rule:=P_abstractPackage(tokenLocation.package)^.customOperatorRule[op];
    if (rule<>nil) then result:=rule^.evaluateToLiteral(tokenLocation,params,recycler,context);
    if result<>nil then exit(result);
    if (params<>nil) and (params^.size=2)
    then begin
      result:=function_id(arg0,arg1,tokenLocation,context,recycler);
      if result=nil then context^.raiseError('Incompatible comparands '+arg0^.typeString+' and '+arg1^.typeString,tokenLocation);
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

{$define boolIntOperator:=FUNCTION function_id(CONST LHS:P_literal; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_boolean: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean: exit(newBoolLiteral(P_boolLiteral(LHS)^.value boolOp P_boolLiteral(RHS)^.value));
        lt_set,lt_emptySet,lt_list,lt_emptyList,lt_booleanList,lt_booleanSet: exit(recurse_scalar_collection);
      end;
      lt_smallint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: exit(recycler^.newIntLiteral(P_smallIntLiteral(LHS)^.value boolOp P_smallIntLiteral(RHS)^.value));
        lt_bigint  : exit(recycler^.newIntLiteral(P_bigIntLiteral(RHS)^.value.bitOp(P_smallIntLiteral(LHS)^.value)));
        lt_set,lt_emptySet,lt_list,lt_emptyList,lt_intList,lt_intSet: exit(recurse_scalar_collection);
      end;
      lt_bigint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: exit(recycler^.newIntLiteral   (P_bigIntLiteral(LHS)^.value.bitOp(P_smallIntLiteral(RHS)^.value)));
        lt_bigint:   exit(recycler^.newBigIntLiteral(P_bigIntLiteral(LHS)^.value.bitOp(P_bigIntLiteral  (RHS)^.value)));
        lt_set,lt_emptySet,lt_list,lt_emptyList,lt_intList,lt_intSet: exit(recurse_scalar_collection);
      end;
      lt_list,lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean,lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_list,lt_emptyList,lt_booleanList,lt_intList: exit(recurse_list_list);
      end;
      lt_set ,lt_emptySet : case RHS^.literalType of
        defaultRHSCases;
        lt_boolean,lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_set,lt_emptySet,lt_booleanSet,lt_intSet: exit(recurse_set_set);
      end;
      lt_booleanList: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean: exit(recurse_collection_scalar);
        lt_list,lt_emptyList,lt_booleanList: exit(recurse_list_list);
      end;
      lt_booleanSet: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean: exit(recurse_collection_scalar);
        lt_set,lt_emptySet,lt_booleanSet: exit(recurse_set_set);
      end;
      lt_intList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_list,lt_emptyList,lt_intList: exit(recurse_list_list);
      end;
      lt_intSet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_set,lt_emptySet,lt_intSet: exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;

FUNCTION outerFunc_id intFuncSignature;
  VAR rule   :P_abstractRule=nil;
  begin
    result:=nil;
    rule:=P_abstractPackage(tokenLocation.package)^.customOperatorRule[op];
    if (rule<>nil) then result:=rule^.evaluateToLiteral(tokenLocation,params,recycler,context);
    if result<>nil then exit(result);
    if (params<>nil) and (params^.size=2)
    then begin
      result:=function_id(arg0,arg1,tokenLocation,context,recycler);
      if result=nil then context^.raiseError('Incompatible operands '+arg0^.typeString+' and '+arg1^.typeString+' for operator '+C_tokenDefaultId[op],tokenLocation);
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
  begin
    result:=nil;
    rule:=P_abstractPackage(tokenLocation.package)^.customOperatorRule[op];
    if (rule<>nil) then result:=rule^.evaluateToLiteral(tokenLocation,params,recycler,context);
    if result<>nil then exit(result);
    if (params<>nil) and (params^.size=2) then begin
      result:=function_id(arg0,arg1,tokenLocation,context,recycler);
      if result=nil then context^.raiseError('Incompatible operands '+arg0^.typeString+' and '+arg1^.typeString+' for operator '+C_tokenDefaultId[op],tokenLocation);
    end else if (params<>nil) and (params^.size=1)
    then exit(arg0^.rereferenced)
    else if (params=nil) or (params^.size=0) then exit(newVoidLiteral);
  end}
{$define function_id:=perform_plus}
{$define outerFunc_id:=operator_plus}
{$define op:=tt_operatorPlus}
FUNCTION perform_plus(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  generic_recursions;

  VAR resultElements:T_arrayOfLiteral;
      tmp:T_myFloat;
      i:longint;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_smallint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: exit(recycler^.newIntLiteral(int64(       P_smallIntLiteral(LHS)^.value)+int64(P_smallIntLiteral(RHS)^.value)));
        lt_bigint:   exit(recycler^.newIntLiteral (            P_bigIntLiteral  (RHS)^.value +      P_smallIntLiteral(LHS)^.value));
        lt_real:     exit(recycler^.newRealLiteral(            P_smallIntLiteral(LHS)^.floatValue+P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_bigint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: exit(recycler^.newIntLiteral (            P_bigIntLiteral(LHS)^.value+ P_smallIntLiteral(RHS)^.value));
        lt_bigint:   exit(recycler^.newIntLiteral (            P_bigIntLiteral(LHS)^.value+ P_bigIntLiteral  (RHS)^.value));
        lt_real:     exit(recycler^.newRealLiteral(            P_bigIntLiteral(LHS)^.floatValue+P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,
        lt_bigint: exit(recycler^.newRealLiteral(P_realLiteral(LHS)^.value+P_abstractIntLiteral(RHS)^.floatValue));
        lt_real:   exit(recycler^.newRealLiteral(P_realLiteral(LHS)^.value+P_realLiteral       (RHS)^.value));
        lt_intList,lt_realList,lt_numList: begin
          setLength(resultElements,P_listLiteral(RHS)^.size);
          tmp:=P_realLiteral(LHS)^.floatValue;
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(tmp+P_numericLiteral(P_listLiteral(RHS)^.value[i])^.floatValue);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_list,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_string: case RHS^.literalType of
        defaultRHSCases;
        lt_string: exit(recycler^.newStringLiteral(P_stringLiteral(LHS)^.value+P_stringLiteral(RHS)^.value));
        lt_list,lt_stringList,lt_emptyList,
        lt_set ,lt_stringSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_realList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: begin
          setLength(resultElements,P_listLiteral(LHS)^.size);
          tmp:=P_numericLiteral(RHS)^.floatValue;
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(P_realLiteral(P_listLiteral(LHS)^.value[i])^.value+tmp);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_intList,lt_realList,lt_numList: if P_listLiteral(LHS)^.size=P_listLiteral(RHS)^.size then begin
          setLength(resultElements,P_listLiteral(LHS)^.size);
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(P_realLiteral(P_listLiteral(LHS)^.value[i])^.value+P_numericLiteral(P_listLiteral(RHS)^.value[i])^.floatValue);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_list: exit(recurse_list_list);
      end;
      lt_intList,lt_numList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList: exit(recurse_list_list);
      end;
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_set_set);
      end;
      lt_stringList: case RHS^.literalType of
        defaultRHSCases;
        lt_string: exit(recurse_collection_scalar);
        lt_list,lt_stringList: exit(recurse_list_list);
      end;
      lt_stringSet: case RHS^.literalType of
        defaultRHSCases;
        lt_string: exit(recurse_collection_scalar);
        lt_set ,lt_stringSet , lt_emptySet: exit(recurse_set_set);
      end;
      lt_list,lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real,lt_string: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList,lt_stringList,lt_emptyList: exit(recurse_list_list);
      end;
      lt_set,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real,lt_string: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet: exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_minus}
{$define outerFunc_id:=operator_minus}
{$define op:=tt_operatorMinus}
FUNCTION perform_minus(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  generic_recursions;

  VAR resultElements:T_arrayOfLiteral;
      tmp:T_myFloat;
      i:longint;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_smallint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: exit(recycler^.newIntLiteral (int64(       P_smallIntLiteral(LHS)^.value)-int64(P_smallIntLiteral(RHS)^.value)));
        lt_bigint:   exit(recycler^.newIntLiteral (             P_smallIntLiteral(LHS)^.value-P_bigIntLiteral(RHS)^.value));
        lt_real:     exit(recycler^.newRealLiteral(             P_smallIntLiteral(LHS)^.value-P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_bigint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: exit(recycler^.newIntLiteral (             P_bigIntLiteral(LHS)^.value        -P_smallIntLiteral(RHS)^.value));
        lt_bigint  : exit(recycler^.newIntLiteral (             P_bigIntLiteral(LHS)^.value        -P_bigIntLiteral  (RHS)^.value));
        lt_real    : exit(recycler^.newRealLiteral(             P_bigIntLiteral(LHS)^.value.toFloat-P_realLiteral    (RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,
        lt_bigint: exit(recycler^.newRealLiteral(P_realLiteral(LHS)^.value-P_abstractIntLiteral(RHS)^.floatValue));
        lt_real:   exit(recycler^.newRealLiteral(P_realLiteral(LHS)^.value-P_realLiteral       (RHS)^.value));
        lt_intList,lt_realList,lt_numList: begin
          setLength(resultElements,P_listLiteral(RHS)^.size);
          tmp:=P_realLiteral(LHS)^.floatValue;
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(tmp-P_numericLiteral(P_listLiteral(RHS)^.value[i])^.floatValue);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_list,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_realList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: begin
          setLength(resultElements,P_listLiteral(LHS)^.size);
          tmp:=P_numericLiteral(RHS)^.floatValue;
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(P_realLiteral(P_listLiteral(LHS)^.value[i])^.value-tmp);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_intList,lt_realList,lt_numList: if P_listLiteral(LHS)^.size=P_listLiteral(RHS)^.size then begin
          setLength(resultElements,P_listLiteral(LHS)^.size);
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(P_realLiteral(P_listLiteral(LHS)^.value[i])^.value-P_numericLiteral(P_listLiteral(RHS)^.value[i])^.floatValue);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_list: exit(recurse_list_list);
      end;
      lt_intList,lt_numList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList: exit(recurse_list_list);
      end;
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_set_set);
      end;
      lt_list,lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: exit(recurse_list_list);
      end;
      lt_set,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_mult}
{$define outerFunc_id:=operator_mult}
{$define op:=tt_operatorMult}
FUNCTION perform_mult(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  generic_recursions;

  VAR resultElements:T_arrayOfLiteral;
      tmp:T_myFloat;
      i:longint;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_smallint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: exit(recycler^.newIntLiteral (int64(P_smallIntLiteral(LHS)^.value)*int64(P_smallIntLiteral(RHS)^.value)));
        lt_bigint  : exit(recycler^.newIntLiteral( P_bigIntLiteral(RHS)^.value * P_smallIntLiteral(LHS)^.value));
        lt_real    : exit(recycler^.newRealLiteral(P_smallIntLiteral(LHS)^.floatValue*P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_bigint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: exit(recycler^.newIntLiteral (P_bigIntLiteral(LHS)^.value*P_smallIntLiteral(RHS)^.value));
        lt_bigint  : exit(recycler^.newIntLiteral (P_bigIntLiteral(LHS)^.value*P_bigIntLiteral (RHS)^.value));
        lt_real    : exit(recycler^.newRealLiteral(P_bigIntLiteral(LHS)^.floatValue*P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,
        lt_bigint: exit(recycler^.newRealLiteral(P_realLiteral(LHS)^.value*P_abstractIntLiteral(RHS)^.floatValue));
        lt_real:   exit(recycler^.newRealLiteral(P_realLiteral(LHS)^.value*P_realLiteral       (RHS)^.value));
        lt_intList,lt_realList,lt_numList: begin
          setLength(resultElements,P_listLiteral(RHS)^.size);
          tmp:=P_realLiteral(LHS)^.floatValue;
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(tmp*P_numericLiteral(P_listLiteral(RHS)^.value[i])^.floatValue);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_list,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_realList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: begin
          setLength(resultElements,P_listLiteral(LHS)^.size);
          tmp:=P_numericLiteral(RHS)^.floatValue;
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(P_realLiteral(P_listLiteral(LHS)^.value[i])^.value*tmp);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_intList,lt_realList,lt_numList: if P_listLiteral(LHS)^.size=P_listLiteral(RHS)^.size then begin
          setLength(resultElements,P_listLiteral(LHS)^.size);
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(P_realLiteral(P_listLiteral(LHS)^.value[i])^.value*P_numericLiteral(P_listLiteral(RHS)^.value[i])^.floatValue);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_list: exit(recurse_list_list);
      end;
      lt_intList,lt_numList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList: exit(recurse_list_list);
      end;
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_set_set);
      end;
      lt_list,lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: exit(recurse_list_list);
      end;
      lt_set,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_divReal}
{$define outerFunc_id:=operator_divReal}
{$define op:=tt_operatorDivReal}
FUNCTION perform_divReal(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  generic_recursions;

  VAR resultElements:T_arrayOfLiteral;
    tmp:T_myFloat;
    i:longint;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_smallint,lt_bigint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(divideInts(recycler,P_abstractIntLiteral(LHS),P_abstractIntLiteral(RHS)));
        lt_real:   exit(recycler^.newRealLiteral(P_abstractIntLiteral(LHS)^.floatValue/P_realLiteral(RHS)^.value));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,
        lt_bigint: exit(recycler^.newRealLiteral(P_realLiteral(LHS)^.value/P_abstractIntLiteral(RHS)^.floatValue));
        lt_real:   exit(recycler^.newRealLiteral(P_realLiteral(LHS)^.value/P_realLiteral       (RHS)^.value));
        lt_intList,lt_realList,lt_numList: begin
          setLength(resultElements,P_listLiteral(RHS)^.size);
          tmp:=P_realLiteral(LHS)^.floatValue;
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(tmp/P_numericLiteral(P_listLiteral(RHS)^.value[i])^.floatValue);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_list,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_realList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: begin
          setLength(resultElements,P_listLiteral(LHS)^.size);
          tmp:=P_numericLiteral(RHS)^.floatValue;
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(P_realLiteral(P_listLiteral(LHS)^.value[i])^.value/tmp);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_intList,lt_realList,lt_numList: if P_listLiteral(LHS)^.size=P_listLiteral(RHS)^.size then begin
          setLength(resultElements,P_listLiteral(LHS)^.size);
          for i:=0 to length(resultElements)-1 do resultElements[i]:=recycler^.newRealLiteral(P_realLiteral(P_listLiteral(LHS)^.value[i])^.value/P_numericLiteral(P_listLiteral(RHS)^.value[i])^.floatValue);
          result:=recycler^.newListLiteral(length(resultElements));
          P_listLiteral(result)^.setContents(resultElements,recycler);
          exit(result);
        end;
        lt_list: exit(recurse_list_list);
      end;
      lt_intList,lt_numList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList: exit(recurse_list_list);
      end;
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_set_set);
      end;
      lt_list,lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: exit(recurse_list_list);
      end;
      lt_set,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_divInt}
{$define outerFunc_id:=operator_divInt}
{$define op:=tt_operatorDivInt}
FUNCTION perform_divInt(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_smallint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: if P_smallIntLiteral(RHS)^.value=0
                     then exit(nanLit.rereferenced)
                     else exit(recycler^.newIntLiteral(P_smallIntLiteral(LHS)^.value div P_smallIntLiteral(RHS)^.value));
        lt_bigint  : if P_bigIntLiteral(RHS)^.value.isZero
                     then exit(nanLit.rereferenced)
                     else exit(recycler^.newIntLiteral(bigint.divide(P_smallIntLiteral(LHS)^.value,P_bigIntLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_emptyList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_bigint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: if P_smallIntLiteral(RHS)^.value=0
                     then exit(nanLit.rereferenced)
                     else exit(recycler^.newIntLiteral(bigint.divide(P_bigIntLiteral(LHS)^.value,P_smallIntLiteral(RHS)^.value)));
        lt_bigint  : if P_bigIntLiteral(RHS)^.value.isZero
                     then exit(nanLit.rereferenced)
                     else exit(recycler^.newIntLiteral(P_bigIntLiteral(LHS)^.value.divide(P_bigIntLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_emptyList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_intList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_list,lt_intList: exit(recurse_list_list);
      end;
      lt_intSet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_set_set);
      end;
      lt_list,lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_emptyList: exit(recurse_list_list);
      end;
      lt_set,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_mod}
{$define outerFunc_id:=operator_mod}
{$define op:=tt_operatorMod}
FUNCTION perform_mod(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_smallint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: if P_smallIntLiteral(RHS)^.value=0
                     then exit(nanLit.rereferenced)
                     else exit(recycler^.newIntLiteral(P_smallIntLiteral(LHS)^.value mod P_smallIntLiteral(RHS)^.value));
        lt_bigint  : if P_bigIntLiteral(RHS)^.value.isZero
                     then exit(nanLit.rereferenced)
                     else exit(recycler^.newIntLiteral(bigint.modulus(P_smallIntLiteral(LHS)^.value,P_bigIntLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_emptyList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_bigint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: if P_smallIntLiteral(RHS)^.value=0
                     then exit(nanLit.rereferenced)
                     else exit(recycler^.newIntLiteral(P_bigIntLiteral(LHS)^.value mod P_smallIntLiteral(RHS)^.value));
        lt_bigint  : if P_bigIntLiteral(RHS)^.value.isZero
                     then exit(nanLit.rereferenced)
                     else exit(recycler^.newIntLiteral(P_bigIntLiteral(LHS)^.value.modulus(P_bigIntLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_emptyList,
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_intList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_list,lt_intList: exit(recurse_list_list);
      end;
      lt_intSet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_set_set);
      end;
      lt_list,lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_emptyList: exit(recurse_list_list);
      end;
      lt_set,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_emptySet: exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;

genericOuter;

{$define function_id:=perform_pot}
{$define outerFunc_id:=operator_pot}
{$define op:=tt_operatorPot}
FUNCTION perform_pot(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  FUNCTION pot_int_int(CONST x:T_bigInt; exponent: longint): P_literal;
    VAR tx, rx: T_myFloat;
    begin
      if exponent>=0
      then result:=recycler^.newIntLiteral(x.pow(exponent))
      else begin
        rx:=x.toFloat;
        tx:=1;
        exponent:=-exponent;
        while exponent>0 do begin
          if odd(exponent) then tx:=tx*rx;
          rx:=rx*rx;
          exponent:=exponent shr 1;
        end;
        result:=recycler^.newRealLiteral(1/tx);
      end;
    end;

  FUNCTION pot_int_int(CONST smallX:longint; exponent: longint): P_literal;
    VAR tx, rx: T_myFloat;
        x:T_bigInt;
    begin
      if exponent>=0
      then begin
        x.fromInt(smallX);
        result:=recycler^.newIntLiteral(x.pow(exponent));
        x.clear;
      end else begin
        rx:=smallX;
        tx:=1;
        exponent:=-exponent;
        while exponent>0 do begin
          if odd(exponent) then tx:=tx*rx;
          rx:=rx*rx;
          exponent:=exponent shr 1;
        end;
        result:=recycler^.newRealLiteral(1/tx);
      end;
    end;

  FUNCTION pot_real_int(x: T_myFloat; exponent:longint): P_literal;
    VAR resultVal:T_myFloat=1;
    begin
      if exponent<0 then begin
        exponent:=-exponent;
        x:=1/x;
      end;
      while exponent>0 do begin
        if odd(exponent) then resultVal*=x;
        x:=x*x;
        exponent:=exponent shr 1;
      end;
      result:=recycler^.newRealLiteral(resultVal);
    end;

    generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_smallint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint:    exit(pot_int_int(P_smallIntLiteral(LHS)^.value,P_smallIntLiteral (RHS)^.value));
        lt_bigint: if P_bigIntLiteral(RHS)^.value.canBeRepresentedAsInt32
                   then exit(pot_int_int(P_smallIntLiteral(LHS)^.value,P_bigIntLiteral(RHS)^.value.toInt))
                   else begin
                     context^.raiseError('Huge exponents are unimplemented',tokenLocation);
                     exit(newVoidLiteral);
                   end;
        lt_real:   exit(recycler^.newRealLiteral(exp(ln(P_smallIntLiteral(LHS)^.value)*P_realLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_bigint: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint:    exit(pot_int_int(P_bigIntLiteral(LHS)^.value,P_smallIntLiteral (RHS)^.value));
        lt_bigint: if P_bigIntLiteral(RHS)^.value.canBeRepresentedAsInt32
                   then exit(pot_int_int(P_bigIntLiteral(LHS)^.value,P_bigIntLiteral(RHS)^.value.toInt))
                   else begin
                     context^.raiseError('Huge exponents are unimplemented',tokenLocation);
                     exit(newVoidLiteral);
                   end;
        lt_real:   exit(recycler^.newRealLiteral(exp(ln(P_bigIntLiteral(LHS)^.floatValue)*P_realLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint: exit(pot_real_int(P_realLiteral(LHS)^.value,P_smallIntLiteral(RHS)^.value));
        lt_bigint: if P_bigIntLiteral(RHS)^.value.canBeRepresentedAsInt32
                   then exit(pot_real_int(P_realLiteral(LHS)^.value,P_bigIntLiteral(RHS)^.value.toInt))
                   else begin
                     context^.raiseError('Huge exponents are unimplemented',tokenLocation);
                     exit(newVoidLiteral);
                   end;
        lt_real:   exit(recycler^.newRealLiteral(exp(ln(P_realLiteral(LHS)^.value)*P_realLiteral(RHS)^.value)));
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList,
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_scalar_collection);
      end;
      lt_intList,lt_realList,lt_numList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList: exit(recurse_list_list);
      end;
      lt_intSet ,lt_realSet ,lt_numSet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_set_set);
      end;
      lt_list,lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_list,lt_intList,lt_realList,lt_numList,lt_emptyList: exit(recurse_list_list);
      end;
      lt_set,lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_smallint,lt_bigint,lt_real: exit(recurse_collection_scalar);
        lt_set ,lt_intSet ,lt_realSet ,lt_numSet ,lt_emptySet: exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_strConcat}
{$define outerFunc_id:=operator_strConcat}
{$define OP:=tt_operatorStrConcat}
FUNCTION perform_strConcat(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  generic_recursions;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_string: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_real: exit(recycler^.newStringLiteral(P_stringLiteral(LHS)^.value+RHS^.toString));
        lt_string:           exit(recycler^.newStringLiteral(P_stringLiteral(LHS)^.value+P_stringLiteral(RHS)^.value));
        lt_list..lt_emptySet:  exit(recurse_scalar_collection);
      end;
      lt_boolean..lt_real: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_real: exit(recycler^.newStringLiteral(LHS^.toString+RHS^.toString));
        lt_string:           exit(recycler^.newStringLiteral(LHS^.toString+P_stringLiteral(RHS)^.value));
        lt_list..lt_emptySet:  exit(recurse_scalar_collection);
      end;
      lt_list..lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: exit(recurse_collection_scalar);
        lt_list..lt_emptyList:  exit(recurse_list_list);
      end;
      lt_set..lt_emptySet: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: exit(recurse_collection_scalar);
        lt_set..lt_emptySet:  exit(recurse_set_set);
      end;
    end;
    result:=nil;
  end;

genericOuter;
{$define genericOuter:=FUNCTION outerFunc_id intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2)
    then begin
      result:=function_id(arg0,arg1,tokenLocation,context,recycler);
      if result=nil then context^.raiseError('Incompatible operands '+arg0^.typeString+' and '+arg1^.typeString+' for operator '+C_tokenDefaultId[op],tokenLocation);
    end else if (params<>nil) and (params^.size=1)
    then exit(arg0^.rereferenced)
    else if (params=nil) or (params^.size=0) then exit(newVoidLiteral);
  end}

{$define function_id:=perform_concat}
{$define outerFunc_id:=operator_concat}
{$define op:=tt_operatorConcat}
FUNCTION perform_concat(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_boolean..lt_string: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: exit(recycler^.newListLiteral(2)^
                                    .append(recycler,LHS,true)^
                                    .append(recycler,RHS,true));
        lt_list..lt_emptyList: exit(recycler^.newListLiteral(P_listLiteral(RHS)^.size+1)^
                                    .append   (recycler,LHS,true)^
                                    .appendAll(recycler,P_listLiteral(RHS)));
        lt_set ..lt_emptySet : exit(recycler^.newSetLiteral(P_setLiteral(RHS)^.size)^
                                    .append   (recycler,LHS,true)^
                                    .appendAll(recycler,P_setLiteral(RHS)));
      end;
      lt_list..lt_emptyList: case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: exit(recycler^.newListLiteral(P_listLiteral(LHS)^.size+1)^
                                    .appendAll(recycler,P_listLiteral(LHS))^
                                    .append   (recycler,RHS,true));
        lt_list..lt_emptySet:  exit(recycler^.newListLiteral(P_listLiteral(LHS)^.size+P_listLiteral(RHS)^.size)^
                                    .appendAll(recycler,P_listLiteral(LHS))^
                                    .appendAll(recycler,P_setLiteral(RHS)));
      end;
      lt_set ..lt_emptySet : case RHS^.literalType of
        defaultRHSCases;
        lt_boolean..lt_string: exit(recycler^.newSetLiteral(P_setLiteral(LHS)^.size)^
                                    .appendAll(recycler,P_setLiteral(LHS))^
                                    .append   (recycler,RHS,true));
        lt_list..lt_emptyList: exit(recycler^.newListLiteral^
                                    .appendAll(recycler,P_setLiteral (LHS))^
                                    .appendAll(recycler,P_listLiteral(RHS)));
        lt_set ..lt_emptySet : exit(recycler^.newSetLiteral(P_setLiteral(LHS)^.size+P_setLiteral(RHS)^.size)^
                                    .appendAll(recycler,P_setLiteral(LHS))^
                                    .appendAll(recycler,P_setLiteral(RHS)));
      end;
    end;
    result:=nil;
  end;
genericOuter;

{$define function_id:=perform_concatAlt}
{$define outerFunc_id:=operator_concatAlt}
{$define op:=tt_operatorConcatAlt}
FUNCTION perform_concatAlt(CONST LHS,RHS:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    case LHS^.literalType of
      defaultLHScases;
      lt_list..lt_emptyList:
        exit(recycler^.newListLiteral(P_listLiteral(LHS)^.size+1)^
             .appendAll(recycler,P_listLiteral(LHS))^
             .append   (recycler,RHS,true,true));
      lt_set ..lt_emptySet :
        exit(recycler^.newSetLiteral(P_setLiteral(LHS)^.size)^
             .appendAll(recycler,P_setLiteral(LHS))^
             .append   (recycler,RHS,true,true));
    end;
    result:=nil;
  end;
genericOuter;
{$undef OP}

PROCEDURE registerOperator(CONST op:T_tokenType; CONST func:P_intFuncCallback; CONST internal:P_op);
  begin
    OP_IMPL[op]:=internal;
    intFuncForOperator[op]:=builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,operatorName[op],func,ak_binary{$ifdef fullVersion},
      operatorName[op]+'(x,y);//Function wrapper for operator '+C_tokenDefaultId[op]{$endif});
  end;

PROCEDURE registerUnary(CONST op:T_tokenType; CONST func:P_intFuncCallback; CONST internal:P_unary);
  begin
    UN_IMPL[op]:=internal;
    builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,operatorName[op],func,ak_unary{$ifdef fullVersion},
      operatorName[op]+'(x,y);//Function wrapper for '+C_tokenDoc[op].helpText{$endif});
  end;

INITIALIZATION
  litVar.resolveOperatorCallback:=@resolveOperator;
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
  registerOperator(tt_operatorNotIn    ,@operator_NotIn    ,@perform_NotIn    );
  registerUnary(tt_unaryOpNegate,@logicalNegationOf_impl   ,@logicalNegationOf   );
  registerUnary(tt_unaryOpPlus  ,@unaryNoOp_impl           ,@unaryNoOp           );
  registerUnary(tt_unaryOpMinus ,@arithmeticNegationOf_impl,@arithmeticNegationOf);

end.
