UNIT funcs_types;
INTERFACE
USES funcs;
{$WARN 5024 OFF}
VAR BUILTIN_TOSET,BUILTIN_TOLIST:P_intFuncCallback;

IMPLEMENTATION
USES sysutils,
     bigint,
     mnh_constants,
     basicTypes,
     mnh_messages,
     litVar,
     recyclers,
     contexts;
{$i func_defines.inc}

FUNCTION softCast_imp intFuncSignature;
  FUNCTION softCastRecurse(CONST x:P_literal):P_literal;
    VAR iter:T_arrayOfLiteral;
        sub :P_literal;
    begin
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.softCast(recycler);
        lt_list..lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType(recycler,true);
          iter:=P_collectionLiteral(x)^.tempIteratableList;
          for sub in iter do collResult^.append(recycler,softCastRecurse(sub),false);
        end;
        else begin
          x^.rereference;
          result:=x;
        end;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=softCastRecurse(arg0);
  end;

FUNCTION toBuiltin_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0)
    then exit(newVoidLiteral)
    else if (params^.size=1) then begin
      if not(arg0^.literalType in C_typables) or (P_typableLiteral(arg0)^.customType=nil)
      then exit(arg0^.rereferenced)
      else exit(P_typableLiteral(arg0)^.customType^.uncast(recycler,arg0,tokenLocation,context,context^.messages,recycler));
    end else result:=nil;
  end;

FUNCTION toString_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_string then begin
        result:=arg0;
        result^.rereference;
      end else result:=recycler^.newStringLiteral(arg0^.toString);
    end;
  end;

FUNCTION toBoolean_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_boolean: begin result:=arg0; result^.rereference; exit(result); end;
        lt_string: if lowercase(str0^.value) = LITERAL_BOOL_TEXT[false] then exit(newBoolLiteral(false))
              else if lowercase(str0^.value) = LITERAL_BOOL_TEXT[true]  then exit(newBoolLiteral(true));
        lt_bigint: if P_bigIntLiteral(arg0)^.value.isZero then exit(newBoolLiteral(false))
              else if P_bigIntLiteral(arg0)^.value.isOne  then exit(newBoolLiteral(true));
        lt_smallint: if P_smallIntLiteral(arg0)^.value=0 then exit(newBoolLiteral(false))
                else if P_smallIntLiteral(arg0)^.value=1 then exit(newBoolLiteral(true));
        lt_real: if real0^.value=0 then exit(newBoolLiteral(false))
            else if real0^.value=1 then exit(newBoolLiteral(true));
      end;
      context^.raiseError(arg0^.toString+' cannot be cast to boolean',tokenLocation);
    end;
  end;

FUNCTION toInt_imp intFuncSignature;
  VAR len:longint;
      i:int64;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_smallint,
        lt_bigint:
          exit(arg0^.rereferenced);
        lt_boolean:
          if bool0^.value
          then exit(recycler^.newIntLiteral(1))
          else exit(recycler^.newIntLiteral(0));
        lt_real:
          if real0^.value=round(real0^.value)
          then exit(recycler^.newIntLiteral(round(real0^.value)));
        lt_string: begin
          result:=parseNumber(str0^.value,1,false,recycler,len);
          if (result=nil) or (result^.literalType in [lt_smallint,lt_bigint]) then exit(result);
          //parsed a real number
          if P_realLiteral(result)^.value=round(P_realLiteral(result)^.value) then begin
            i:=round(P_realLiteral(result)^.value);
            recycler^.disposeLiteral(result);
            exit(recycler^.newIntLiteral(i));
          end else begin
            recycler^.disposeLiteral(result);
            result:=nil;
          end;
        end;
      end;
      context^.raiseError(arg0^.toString+' cannot be cast to int',tokenLocation);
    end;
  end;

FUNCTION toReal_imp intFuncSignature;
  VAR len:longint;
      x:T_myFloat;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_real: begin result:=arg0; result^.rereference; exit(result); end;
        lt_boolean: if bool0^.value then exit(recycler^.newIntLiteral(1)) else exit(recycler^.newIntLiteral(0));
        lt_smallint,lt_bigint: exit(recycler^.newRealLiteral(int0^.floatValue));
        lt_string: begin
          result:=parseNumber(str0^.value,1,false,recycler,len);
          if (result=nil) or (result^.literalType=lt_real) then exit(result);
          //parsed an integer
          x:=P_abstractIntLiteral(result)^.floatValue;
          recycler^.disposeLiteral(result);
          exit(recycler^.newRealLiteral(x));
        end;
      end;
      context^.raiseError(arg0^.toString+' cannot be cast to real',tokenLocation);
    end;
  end;

FUNCTION toList_imp intFuncSignature;
  VAR valueToAppend:P_literal;
      iterator:P_expressionLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes) then begin
        iterator:=P_expressionLiteral(arg0);
        result:=recycler^.newListLiteral();
        valueToAppend:=iterator^.evaluateToLiteral(tokenLocation,context,recycler,nil,nil).literal;
        while (valueToAppend<>nil) and (valueToAppend^.literalType<>lt_void) do begin
          listResult^.append(recycler,valueToAppend,false);
          valueToAppend:=iterator^.evaluateToLiteral(tokenLocation,context,recycler,nil,nil).literal;
        end;
      end else if arg0^.literalType in C_scalarTypes
      then result:=recycler^.newListLiteral(1)^.append(recycler,arg0,true)
      else result:=compound0^.toList(recycler);
    end;
  end;

FUNCTION toSet_imp intFuncSignature;
  VAR valueToAppend:P_literal;
      iterator:P_expressionLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.typ in C_iteratableExpressionTypes) then begin
        iterator:=P_expressionLiteral(arg0);
        result:=newSetLiteral(1);
        valueToAppend:=iterator^.evaluateToLiteral(tokenLocation,context,recycler,nil,nil).literal;
        while (valueToAppend<>nil) and (valueToAppend^.literalType<>lt_void) do begin
          setResult^.append(recycler,valueToAppend,false);
          valueToAppend:=iterator^.evaluateToLiteral(tokenLocation,context,recycler,nil,nil).literal;
        end;
      end else
      if arg0^.literalType in C_scalarTypes
      then result:=newSetLiteral(1)^.append(recycler,arg0,true)
      else result:=compound0^.toSet(recycler);
    end;
  end;

FUNCTION toMap_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_compoundTypes) then begin
      result:=compound0^.toMap(recycler,tokenLocation,context);
    end;
  end;

{$MACRO ON}
{$define GENERIC_TYPE_CHECK:=FUNCTION FUNC_ID intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then exit(newBoolLiteral(false));
    if (params<>nil) and (params^.size=1) then
    result:=newBoolLiteral((arg0^.literalType in C_typeCheckInfo[TYPE_CHECK].matching) and typeCheckAccept(arg0,TYPE_CHECK))
    else if (C_typeCheckInfo[TYPE_CHECK].modifiable) and (params^.size=2) and (arg1^.literalType in [lt_smallint,lt_bigint]) then
    result:=newBoolLiteral((arg0^.literalType in C_typeCheckInfo[TYPE_CHECK].matching) and typeCheckAccept(arg0,TYPE_CHECK,int1^.intValue));
  end}

{$define TYPE_CHECK:=tc_typeCheckScalar          } {$define FUNC_ID:=isScalar           } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckList            } {$define FUNC_ID:=isList             } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckSet             } {$define FUNC_ID:=isSet              } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckCollection      } {$define FUNC_ID:=isCollection       } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckBoolean         } {$define FUNC_ID:=isBoolean          } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckBoolList        } {$define FUNC_ID:=isBooleanList      } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckBoolSet         } {$define FUNC_ID:=isBooleanSet       } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckBoolCollection  } {$define FUNC_ID:=isBooleanCollection} GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckInt             } {$define FUNC_ID:=isInt              } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckIntList         } {$define FUNC_ID:=isIntList          } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckIntSet          } {$define FUNC_ID:=isIntSet           } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckIntCollection   } {$define FUNC_ID:=isIntCollection    } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckReal            } {$define FUNC_ID:=isReal             } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckRealList        } {$define FUNC_ID:=isRealList         } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckRealSet         } {$define FUNC_ID:=isRealSet          } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckRealCollection  } {$define FUNC_ID:=isRealCollection   } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckString          } {$define FUNC_ID:=isString           } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckStringList      } {$define FUNC_ID:=isStringList       } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckStringSet       } {$define FUNC_ID:=isStringSet        } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckStringCollection} {$define FUNC_ID:=isStringCollection } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckNumeric         } {$define FUNC_ID:=isNumeric          } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckNumList         } {$define FUNC_ID:=isNumericList      } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckNumSet          } {$define FUNC_ID:=isNumericSet       } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckNumCollection   } {$define FUNC_ID:=isNumericCollection} GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckMap             } {$define FUNC_ID:=isMap              } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckExpression      } {$define FUNC_ID:=isExpression       } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckStatelessExpression } {$define FUNC_ID:=isStatelessExpression } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckStatefulExpression  } {$define FUNC_ID:=isStatefulExpression  } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckIteratableExpression} {$define FUNC_ID:=isIteratableExpression} GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tc_typeCheckIteratable          } {$define FUNC_ID:=isIteratable          } GENERIC_TYPE_CHECK;

FUNCTION isVoid intFuncSignature;
  begin
    result:=newBoolLiteral((params=nil) or (params^.size=0));
  end;

FUNCTION typeOf_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then exit(recycler^.newStringLiteral(arg0^.typeString))
    else exit(recycler^.newStringLiteral(parameterListTypeString(params)));
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'softCast'      ,@softCast_imp  ,ak_unary{$ifdef fullVersion},'softCast(X);#Returns a simplified version of X, trying to parse integers, real values and booleans'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'toBuiltin'     ,@toBuiltin_imp ,ak_unary{$ifdef fullVersion},'toBuiltin(X);#Returns X without custom type info'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,TO_STRING_RULE_ID,@toString_imp  ,ak_unary{$ifdef fullVersion},TO_STRING_RULE_ID+'(X);#Casts X to string'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'toBoolean'     ,@toBoolean_imp ,ak_unary{$ifdef fullVersion},'toBoolean(X);#Casts X to boolean or throws an error if not possible'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'toInt'         ,@toInt_imp     ,ak_unary{$ifdef fullVersion},'toInt(X);#Casts X to int or throws an error if not possible'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'toReal'        ,@toReal_imp    ,ak_unary{$ifdef fullVersion},'toReal(X);#Casts X to real or throws an error if not possible'{$endif});
  BUILTIN_TOLIST:=
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'toList'        ,@toList_imp    ,ak_unary{$ifdef fullVersion},'toList(X);#Casts X to list or wraps a scalar in a list'{$endif});
  BUILTIN_TOSET:=
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'toSet'         ,@toSet_imp     ,ak_unary{$ifdef fullVersion},'toSet(X);#Casts X to set or wraps a scalar in a set'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'toMap'         ,@toMap_imp     ,ak_unary{$ifdef fullVersion},'toMap(X:Collection);#Casts X to map or throws an error if not possible'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'typeOf'             ,@typeOf_imp         ,ak_unary     {$ifdef fullVersion},'typeOf(x); //Returns a description of x''s type'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isVoid'             ,@isVoid             ,ak_unary     {$ifdef fullVersion},'isVoid(x); //Returns true if x is void (or no arguments were given)'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isScalar'           ,@isScalar           ,ak_unary     {$ifdef fullVersion},'isScalar(x); //Returns true if x is a scalar'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isList'             ,@isList             ,ak_variadic_1{$ifdef fullVersion},'isList(x); //Returns true if x is a list. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isSet'              ,@isSet              ,ak_variadic_1{$ifdef fullVersion},'isSet(x); //Returns true if x is a set. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isCollection'       ,@isCollection       ,ak_variadic_1{$ifdef fullVersion},'isCollection(x); //Returns true if x is a collection. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isBoolean'          ,@isBoolean          ,ak_unary     {$ifdef fullVersion},'isBoolean(x); //Returns true if x is a boolean'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isBooleanList'      ,@isBooleanList      ,ak_variadic_1{$ifdef fullVersion},'isBooleanList(x); //Returns true if x is a booleanList. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isBooleanSet'       ,@isBooleanSet       ,ak_variadic_1{$ifdef fullVersion},'isBooleanSet(x); //Returns true if x is a booleanSet. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isBooleanCollection',@isBooleanCollection,ak_variadic_1{$ifdef fullVersion},'isBooleanCollection(x); //Returns true if x is a booleanCollection. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isInt'              ,@isInt              ,ak_unary     {$ifdef fullVersion},'isInt(x); //Returns true if x is a int'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isIntList'          ,@isIntList          ,ak_variadic_1{$ifdef fullVersion},'isIntList(x); //Returns true if x is a intList. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isIntSet'           ,@isIntSet           ,ak_variadic_1{$ifdef fullVersion},'isIntSet(x); //Returns true if x is a intSet. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isIntCollection'    ,@isIntCollection    ,ak_variadic_1{$ifdef fullVersion},'isIntCollection(x); //Returns true if x is a intCollection. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isReal'             ,@isReal             ,ak_unary     {$ifdef fullVersion},'isReal(x); //Returns true if x is a real'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isRealList'         ,@isRealList         ,ak_variadic_1{$ifdef fullVersion},'isRealList(x); //Returns true if x is a realList. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isRealSet'          ,@isRealSet          ,ak_variadic_1{$ifdef fullVersion},'isRealSet(x); //Returns true if x is a realSet. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isRealCollection'   ,@isRealCollection   ,ak_variadic_1{$ifdef fullVersion},'isRealCollection(x); //Returns true if x is a realCollection. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isString'           ,@isString           ,ak_unary     {$ifdef fullVersion},'isString(x); //Returns true if x is a string'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isStringList'       ,@isStringList       ,ak_variadic_1{$ifdef fullVersion},'isStringList(x); //Returns true if x is a stringList. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isStringSet'        ,@isStringSet        ,ak_variadic_1{$ifdef fullVersion},'isStringSet(x); //Returns true if x is a stringSet. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isStringCollection' ,@isStringCollection ,ak_variadic_1{$ifdef fullVersion},'isStringCollection(x); //Returns true if x is a stringCollection. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isNumeric'          ,@isNumeric          ,ak_unary     {$ifdef fullVersion},'isNumeric(x); //Returns true if x is a numeric'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isNumericList'      ,@isNumericList      ,ak_variadic_1{$ifdef fullVersion},'isNumericList(x); //Returns true if x is a numericList. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isNumericSet'       ,@isNumericSet       ,ak_variadic_1{$ifdef fullVersion},'isNumericSet(x); //Returns true if x is a numericSet. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isNumericCollection',@isNumericCollection,ak_variadic_1{$ifdef fullVersion},'isNumericCollection(x); //Returns true if x is a numericCollection. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isMap'              ,@isMap              ,ak_variadic_1{$ifdef fullVersion},'isMap(x); //Returns true if x is a map. Specify an additional int parameter to additionally check the size.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isExpression'       ,@isExpression       ,ak_variadic_1{$ifdef fullVersion},'isExpression(x); //Returns true if x is a expression. Specify an additional int parameter k to additionally check if the expression can be applied to k parameters.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isStatelessExpression' ,@isStatelessExpression ,ak_variadic_1{$ifdef fullVersion},'isStatelessExpression(x); //Returns true if x is a stateless expression. Specify an additional int parameter k to additionally check if the expression can be applied to k parameters.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isStatefulExpression'  ,@isStatefulExpression  ,ak_variadic_1{$ifdef fullVersion},'isStatefulExpression(x); //Returns true if x is a stateful expression. Specify an additional int parameter k to additionally check if the expression can be applied to k parameters.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isIteratableExpression',@isIteratableExpression,ak_unary     {$ifdef fullVersion},'isIteratableExpression(x); //Returns true if x is an iteratable expression.'{$endif});
  builtinFunctionMap.registerRule(TYPECAST_NAMESPACE,'isIteratable'          ,@isIteratable          ,ak_unary     {$ifdef fullVersion},'isIteratable(x); //Returns true if x is an iteratable expression, a collection or a map.'{$endif});

end.
