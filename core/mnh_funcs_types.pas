UNIT mnh_funcs_types;
INTERFACE
{$WARN 5024 OFF}
USES mnh_basicTypes,mnh_litVar,mnh_constants, mnh_funcs,sysutils,mnh_out_adapters,mnh_contexts,myGenerics;

IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION softCast_imp intFuncSignature;
  FUNCTION softCastRecurse(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.softCast;
        lt_list..lt_listWithError: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do
            lResult^.append(softCastRecurse(P_listLiteral(x)^.value(i)),false);
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

{$MACRO ON}
{$define FUNC_ID:=isScalar}
{$define TYPE_CHECK:=tt_typeCheckScalar}
{$define GENERIC_TYPE_CHECK:=
FUNCTION FUNC_ID intFuncSignature;
  begin
    result:=nil;              ;
    if (params<>nil) and (params^.size=1) then
    result:=newBoolLiteral(arg0^.literalType in C_matchingTypes[TYPE_CHECK])
    else if (TYPE_CHECK in C_modifieableTypeChecks) and (params^.size=2) and (arg1^.literalType=lt_int) then
    result:=newBoolLiteral((arg0^.literalType in C_matchingTypes[TYPE_CHECK])
       and (   (arg0^.literalType<>lt_expression     ) or (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(P_intLiteral(arg1)^.value)))
       and (not(arg0^.literalType in C_validListTypes) or (P_listLiteral      (arg0)^.size =P_intLiteral(arg1)^.value)));
  end}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isBoolean}
{$define TYPE_CHECK:=tt_typeCheckBoolean}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isInt}
{$define TYPE_CHECK:=tt_typeCheckInt}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isReal}
{$define TYPE_CHECK:=tt_typeCheckReal}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isNumeric}
{$define TYPE_CHECK:=tt_typeCheckNumeric}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isString}
{$define TYPE_CHECK:=tt_typeCheckString}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isList}
{$define TYPE_CHECK:=tt_typeCheckList}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isBoolList}
{$define TYPE_CHECK:=tt_typeCheckBoolList}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isIntList}
{$define TYPE_CHECK:=tt_typeCheckIntList}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isRealList}
{$define TYPE_CHECK:=tt_typeCheckRealList}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isStringList}
{$define TYPE_CHECK:=tt_typeCheckStringList}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isNumList}
{$define TYPE_CHECK:=tt_typeCheckNumList}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isKeyValueList}
{$define TYPE_CHECK:=tt_typeCheckKeyValueList}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isFlatList}
{$define TYPE_CHECK:=tt_typeCheckKeyValueList}
GENERIC_TYPE_CHECK;

{$define FUNC_ID:=isExpression}
{$define TYPE_CHECK:=tt_typeCheckExpression}
GENERIC_TYPE_CHECK;

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
  registerRule(TYPECAST_NAMESPACE,'isScalar'      ,@isScalar      ,true,ak_unary,'isScalar(x);//Returns true if x is a scalar.');
  registerRule(TYPECAST_NAMESPACE,'isBoolean'     ,@isBoolean     ,true,ak_unary,'isBoolean(x);//Returns true if x is a boolean.');
  registerRule(TYPECAST_NAMESPACE,'isInt'         ,@isInt         ,true,ak_unary,'isInt(x);//Returns true if x is an int.');
  registerRule(TYPECAST_NAMESPACE,'isReal'        ,@isReal        ,true,ak_unary,'isReal(x);//Returns true if x is numeric.');
  registerRule(TYPECAST_NAMESPACE,'isNumeric'     ,@isNumeric     ,true,ak_unary,'isNumeric(x);//Returns true if x is a real.');
  registerRule(TYPECAST_NAMESPACE,'isString'      ,@isString      ,true,ak_unary,'isString(x);//Returns true if x is a string.');
  registerRule(TYPECAST_NAMESPACE,'isList'        ,@isList        ,true,ak_unary,'isList(x);//Returns true if x is a list. Specify an additional int parameter to additionally check the length.');
  registerRule(TYPECAST_NAMESPACE,'isBoolList'    ,@isBoolList    ,true,ak_unary,'isBoolList(x);//Returns true if x is a boolList. Specify an additional int parameter to additionally check the length.');
  registerRule(TYPECAST_NAMESPACE,'isIntList'     ,@isIntList     ,true,ak_unary,'isIntListx);//Returns true if x is an intList. Specify an additional int parameter to additionally check the length.');
  registerRule(TYPECAST_NAMESPACE,'isRealList'    ,@isRealList    ,true,ak_unary,'isRealList(x);//Returns true if x is a realList. Specify an additional int parameter to additionally check the length.');
  registerRule(TYPECAST_NAMESPACE,'isStringList'  ,@isStringList  ,true,ak_unary,'isStringList(x);//Returns true if x is a stringList. Specify an additional int parameter to additionally check the length.');
  registerRule(TYPECAST_NAMESPACE,'isNumList'     ,@isNumList     ,true,ak_unary,'isNumList(x);//Returns true if x is a numList. Specify an additional int parameter to additionally check the length.');
  registerRule(TYPECAST_NAMESPACE,'isFlatList'    ,@isFlatList    ,true,ak_unary,'isFlatList(x);//Returns true if x is a flat list. Specify an additional int parameter to additionally check the length.');
  registerRule(TYPECAST_NAMESPACE,'isKeyValueList',@isKeyValueList,true,ak_unary,'isKeyValueList(x);//Returns true if x is a keyValueList. Specify an additional int parameter to additionally check the length.');
  registerRule(TYPECAST_NAMESPACE,'isExpression'  ,@isExpression  ,true,ak_unary,'isExpression(x);//Returns true if x is an expression. Specify an additional int parameter k to additionally check if the expression can be applied to k parameters.');
  registerRule(TYPECAST_NAMESPACE,'isVoid'        ,@isVoid        ,true,ak_unary,'isVoid(x);//Returns true if no parameters are passed.');
  registerRule(TYPECAST_NAMESPACE,'typeOf'        ,@typeOf_imp    ,true,ak_unary,'typeOf(x);#Returns a string representation of the type of x');

end.

