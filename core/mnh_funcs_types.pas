UNIT mnh_funcs_types;
INTERFACE
{$WARN 5024 OFF}
USES mnh_basicTypes,mnh_litVar,mnh_constants, mnh_funcs,sysutils,mnh_out_adapters,mnh_contexts,myGenerics;

IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION softCast_imp intFuncSignature;
  FUNCTION softCastRecurse(CONST x:P_literal):P_literal;
    VAR iter:T_arrayOfLiteral;
        sub :P_literal;
    begin
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.softCast;
        lt_list..lt_emptySet: begin
          result:=P_collectionLiteral(x)^.newOfSameType;
          iter:=P_collectionLiteral(x)^.iteratableList;
          for sub in iter do collResult^.append(softCastRecurse(sub),false);
          disposeLiteral(iter);
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

FUNCTION toString_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_string then begin
        result:=arg0;
        result^.rereference;
      end else result:=newStringLiteral(arg0^.toString);
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
        lt_int: if int0^.value=0 then exit(newBoolLiteral(false))
           else if int0^.value=1 then exit(newBoolLiteral(true));
        lt_real: if real0^.value=0 then exit(newBoolLiteral(false))
            else if real0^.value=1 then exit(newBoolLiteral(true));
      end;
      context.adapters^.raiseError(arg0^.toString+' cannot be cast to boolean',tokenLocation);
    end;
  end;

FUNCTION toInt_imp intFuncSignature;
  VAR len:longint;
      i:int64;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case arg0^.literalType of
        lt_int: begin result:=arg0; result^.rereference; exit(result); end;
        lt_boolean: if bool0^.value then exit(newIntLiteral(1)) else exit(newIntLiteral(0));
        lt_real: if real0^.value=round(real0^.value) then exit(newIntLiteral(round(real0^.value)));
        lt_string: begin
          result:=parseNumber(str0^.value,1,false,len);
          if (result=nil) or (result^.literalType=lt_int) then exit(result);
          //parsed a real number
          if P_realLiteral(result)^.value=round(P_realLiteral(result)^.value) then begin
            i:=round(P_realLiteral(result)^.value);
            disposeLiteral(result);
            exit(newIntLiteral(i));
          end else begin
            disposeLiteral(result);
            result:=nil;
          end;
        end;
      end;
      context.adapters^.raiseError(arg0^.toString+' cannot be cast to int',tokenLocation);
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
        lt_boolean: if bool0^.value then exit(newIntLiteral(1)) else exit(newIntLiteral(0));
        lt_int: exit(newRealLiteral(int0^.value));
        lt_string: begin
          result:=parseNumber(str0^.value,1,false,len);
          if (result=nil) or (result^.literalType=lt_real) then exit(result);
          //parsed an integer
          x:=P_intLiteral(result)^.value;
          disposeLiteral(result);
          exit(newRealLiteral(x));
        end;
      end;
      context.adapters^.raiseError(arg0^.toString+' cannot be cast to real',tokenLocation);
    end;
  end;

FUNCTION toList_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType in C_scalarTypes
      then result:=newListLiteral(1)^.append(arg0,true)
      else result:=compound1^.toList;
    end;
  end;

FUNCTION toSet_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType in C_scalarTypes
      then result:=newSetLiteral^.append(arg0,true)
      else result:=compound1^.toSet;
    end;
  end;

FUNCTION toMap_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in C_compoundTypes) then begin
      result:=compound1^.toMap(tokenLocation,context.adapters^);
    end;
  end;


{$MACRO ON}
{$define GENERIC_TYPE_CHECK:=FUNCTION FUNC_ID intFuncSignature;
  begin
    result:=nil;              ;
    if (params<>nil) and (params^.size=1) then
    result:=newBoolLiteral(arg0^.literalType in C_matchingTypes[TYPE_CHECK])
    else if (TYPE_CHECK in C_modifieableTypeChecks) and (params^.size=2) and (arg1^.literalType=lt_int) then
    result:=newBoolLiteral((arg0^.literalType in C_matchingTypes[TYPE_CHECK])
       and (   (arg0^.literalType<>lt_expression    ) or (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(int1^.value)))
       and (not(arg0^.literalType in C_compoundTypes) or (P_compoundLiteral  (arg0)^.size =int1^.value)));
  end}

{$define TYPE_CHECK:=tt_typeCheckScalar          } {$define FUNC_ID:=isScalar           } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckList            } {$define FUNC_ID:=isList             } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckSet             } {$define FUNC_ID:=isSet              } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckCollection      } {$define FUNC_ID:=isCollection       } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckBoolean         } {$define FUNC_ID:=isBoolean          } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckBoolList        } {$define FUNC_ID:=isBooleanList      } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckBoolSet         } {$define FUNC_ID:=isBooleanSet       } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckBoolCollection  } {$define FUNC_ID:=isBooleanCollection} GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckInt             } {$define FUNC_ID:=isInt              } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckIntList         } {$define FUNC_ID:=isIntList          } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckIntSet          } {$define FUNC_ID:=isIntSet           } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckIntCollection   } {$define FUNC_ID:=isIntCollection    } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckReal            } {$define FUNC_ID:=isReal             } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckRealList        } {$define FUNC_ID:=isRealList         } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckRealSet         } {$define FUNC_ID:=isRealSet          } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckRealCollection  } {$define FUNC_ID:=isRealCollection   } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckString          } {$define FUNC_ID:=isString           } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckStringList      } {$define FUNC_ID:=isStringList       } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckStringSet       } {$define FUNC_ID:=isStringSet        } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckStringCollection} {$define FUNC_ID:=isStringCollection } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckNumeric         } {$define FUNC_ID:=isNumeric          } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckNumList         } {$define FUNC_ID:=isNumericList      } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckNumSet          } {$define FUNC_ID:=isNumericSet       } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckNumCollection   } {$define FUNC_ID:=isNumericCollection} GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckMap             } {$define FUNC_ID:=isMap              } GENERIC_TYPE_CHECK;
{$define TYPE_CHECK:=tt_typeCheckExpression      } {$define FUNC_ID:=isExpression       } GENERIC_TYPE_CHECK;

FUNCTION isVoid intFuncSignature;
  begin
    result:=newBoolLiteral((params=nil) or (params^.size=0));
  end;

FUNCTION typeOf_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then exit(newStringLiteral(arg0^.typeString))
    else exit(newStringLiteral(parameterListTypeString(params)));
  end;

INITIALIZATION
  registerRule(TYPECAST_NAMESPACE,'softCast'      ,@softCast_imp  ,true,ak_unary,'softCast(X);#Returns a simplified version of X, trying to parse integers, real values and booleans');
  registerRule(TYPECAST_NAMESPACE,'toString'      ,@toString_imp  ,true,ak_unary,'toString(X);#Casts X to string');
  registerRule(TYPECAST_NAMESPACE,'toBoolean'     ,@toBoolean_imp ,true,ak_unary,'toBoolean(X);#Casts X to boolean or throws an error if not possible');
  registerRule(TYPECAST_NAMESPACE,'toInt'         ,@toInt_imp     ,true,ak_unary,'toInt(X);#Casts X to int or throws an error if not possible');
  registerRule(TYPECAST_NAMESPACE,'toReal'        ,@toReal_imp    ,true,ak_unary,'toReal(X);#Casts X to real or throws an error if not possible');
  registerRule(TYPECAST_NAMESPACE,'toList'        ,@toList_imp    ,true,ak_unary,'toList(X);#Casts X to list or wraps a scalar in a list');
  registerRule(TYPECAST_NAMESPACE,'toSet'         ,@toSet_imp     ,true,ak_unary,'toSet(X);#Casts X to set or wraps a scalar in a set');
  registerRule(TYPECAST_NAMESPACE,'toMap'         ,@toMap_imp     ,true,ak_unary,'toMap(X:collection);#Casts X to map or throws an error if not possible');
  registerRule(TYPECAST_NAMESPACE,'isScalar'           ,@isScalar           ,true,ak_unary     ,'isScalar(x); //Returns true if x is a scalar');
  registerRule(TYPECAST_NAMESPACE,'isList'             ,@isList             ,true,ak_variadic_1,'isList(x); //Returns true if x is a list. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isSet'              ,@isSet              ,true,ak_variadic_1,'isSet(x); //Returns true if x is a set. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isCollection'       ,@isCollection       ,true,ak_variadic_1,'isCollection(x); //Returns true if x is a collection. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isBoolean'          ,@isBoolean          ,true,ak_unary     ,'isBoolean(x); //Returns true if x is a boolean');
  registerRule(TYPECAST_NAMESPACE,'isBooleanList'      ,@isBooleanList      ,true,ak_variadic_1,'isBooleanList(x); //Returns true if x is a booleanList. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isBooleanSet'       ,@isBooleanSet       ,true,ak_variadic_1,'isBooleanSet(x); //Returns true if x is a booleanSet. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isBooleanCollection',@isBooleanCollection,true,ak_variadic_1,'isBooleanCollection(x); //Returns true if x is a booleanCollection. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isInt'              ,@isInt              ,true,ak_unary     ,'isInt(x); //Returns true if x is a int');
  registerRule(TYPECAST_NAMESPACE,'isIntList'          ,@isIntList          ,true,ak_variadic_1,'isIntList(x); //Returns true if x is a intList. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isIntSet'           ,@isIntSet           ,true,ak_variadic_1,'isIntSet(x); //Returns true if x is a intSet. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isIntCollection'    ,@isIntCollection    ,true,ak_variadic_1,'isIntCollection(x); //Returns true if x is a intCollection. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isReal'             ,@isReal             ,true,ak_unary     ,'isReal(x); //Returns true if x is a real');
  registerRule(TYPECAST_NAMESPACE,'isRealList'         ,@isRealList         ,true,ak_variadic_1,'isRealList(x); //Returns true if x is a realList. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isRealSet'          ,@isRealSet          ,true,ak_variadic_1,'isRealSet(x); //Returns true if x is a realSet. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isRealCollection'   ,@isRealCollection   ,true,ak_variadic_1,'isRealCollection(x); //Returns true if x is a realCollection. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isString'           ,@isString           ,true,ak_unary     ,'isString(x); //Returns true if x is a string');
  registerRule(TYPECAST_NAMESPACE,'isStringList'       ,@isStringList       ,true,ak_variadic_1,'isStringList(x); //Returns true if x is a stringList. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isStringSet'        ,@isStringSet        ,true,ak_variadic_1,'isStringSet(x); //Returns true if x is a stringSet. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isStringCollection' ,@isStringCollection ,true,ak_variadic_1,'isStringCollection(x); //Returns true if x is a stringCollection. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isNumeric'          ,@isNumeric          ,true,ak_unary     ,'isNumeric(x); //Returns true if x is a numeric');
  registerRule(TYPECAST_NAMESPACE,'isNumericList'      ,@isNumericList      ,true,ak_variadic_1,'isNumericList(x); //Returns true if x is a numericList. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isNumericSet'       ,@isNumericSet       ,true,ak_variadic_1,'isNumericSet(x); //Returns true if x is a numericSet. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isNumericCollection',@isNumericCollection,true,ak_variadic_1,'isNumericCollection(x); //Returns true if x is a numericCollection. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isMap'              ,@isMap              ,true,ak_variadic_1,'isMap(x); //Returns true if x is a map. Specify an additional int parameter to additionally check the size.');
  registerRule(TYPECAST_NAMESPACE,'isExpression'       ,@isExpression       ,true,ak_variadic_1,'isExpression(x); //Returns true if x is a expression. Specify an additional int parameter k to additionally check if the expression can be applied to k parameters.');
end.

