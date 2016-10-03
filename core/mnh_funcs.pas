UNIT mnh_funcs;
INTERFACE
USES sysutils,myGenerics,mnh_constants,mnh_litVar,mnh_out_adapters,mnh_basicTypes,mnh_contexts,
     myStringUtil,Classes{$ifdef fullVersion},mnh_doc{$endif};
TYPE
  F_PARAM_TOKLOC_CONTEXT_TO_LITERAL=FUNCTION(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;

  P_intFuncCallback=^T_intFuncCallback;
  T_intFuncCallback=object(T_objectWithIdAndLocation)
    location:T_tokenLocation;
    id:T_idString;
    call:F_PARAM_TOKLOC_CONTEXT_TO_LITERAL;
    CONSTRUCTOR create(CONST namespace:T_namespace; CONST unqualifiedId:T_idString; CONST func:F_PARAM_TOKLOC_CONTEXT_TO_LITERAL);
    DESTRUCTOR destroy;
    FUNCTION getId:T_idString; virtual;
    FUNCTION getLocation:T_tokenLocation; virtual;
  end;

  P_mnhSystemPseudoPackage=^T_mnhSystemPseudoPackage;
  T_mnhSystemPseudoPackage=object(T_objectWithPath)
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getPath:ansistring; virtual;
  end;

VAR
  intrinsicRuleMap:specialize G_stringKeyMap<P_intFuncCallback>;
  print_cs        :system.TRTLCriticalSection;

FUNCTION registerRule(CONST namespace:T_namespace; CONST name:T_idString; CONST ptr:F_PARAM_TOKLOC_CONTEXT_TO_LITERAL; CONST explanation:ansistring; CONST fullNameOnly:boolean=false):P_intFuncCallback;
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST typ:T_literalType; CONST messageTail:ansistring; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters);
PROCEDURE setMnhParameters(CONST p:T_arrayOfString);
IMPLEMENTATION
VAR mnhParameters:P_listLiteral=nil;
    uniqueBuiltinFunctions:array of P_intFuncCallback;
    mnhSystemPseudoPackage:P_mnhSystemPseudoPackage;

FUNCTION registerRule(CONST namespace: T_namespace; CONST name:T_idString; CONST ptr: F_PARAM_TOKLOC_CONTEXT_TO_LITERAL; CONST explanation: ansistring; CONST fullNameOnly: boolean):P_intFuncCallback;
  begin
    new(result,create(namespace,name,ptr));
    if not(fullNameOnly) then
    intrinsicRuleMap.put(                                                  name,result);
    intrinsicRuleMap.put(C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+name,result);
    {$ifdef fullVersion}registerDoc(C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+name,explanation,fullNameOnly);{$endif}
  end;

PROCEDURE raiseNotApplicableError(CONST functionName: ansistring; CONST typ: T_literalType; CONST messageTail: ansistring; CONST tokenLocation: T_tokenLocation; VAR adapters: T_adapters);
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function '+functionName+' cannot be applied to type '+C_typeString[typ]+messageTail;
    adapters.raiseError(complaintText,tokenLocation);
  end;

PROCEDURE setMnhParameters(CONST p: T_arrayOfString);
  VAR i:longint;
  begin
    if mnhParameters<>nil then disposeLiteral(mnhParameters);
    mnhParameters:=newListLiteral;
    for i:=0 to length(p)-1 do mnhParameters^.appendString(p[i]);
  end;

{$undef INNER_FORMATTING}
{$WARN 5024 OFF}
FUNCTION clearPrint_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params<>nil) and (params^.size>0) then exit(nil);
    system.enterCriticalSection(print_cs);
    context.adapters^.clearPrint();
    system.leaveCriticalSection(print_cs);
    result:=newVoidLiteral;
  end;

FUNCTION print_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR stringToPrint:ansistring='';
      i:longint;
  begin
    if params<>nil then for i:=0 to params^.size-1 do case params^.value(i)^.literalType of
      lt_boolean,
      lt_int,
      lt_real,
      lt_string,
      lt_expression: stringToPrint:=stringToPrint + P_scalarLiteral(params^.value(i))^.stringForm;
      lt_list..lt_listWithError: stringToPrint:=stringToPrint + params^.value(i)^.toString;
    end;
    system.enterCriticalSection(print_cs);
    context.adapters^.printOut(formatTabs(split(stringToPrint)));
    system.leaveCriticalSection(print_cs);
    result:=newVoidLiteral;
  end;

FUNCTION typeOf_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then exit(newStringLiteral(params^.value(0)^.typeString))
    else exit(newStringLiteral(params^.parameterListTypeString));
  end;

FUNCTION mnhParameters_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params=nil) or (params^.size=0) then begin
      if mnhParameters=nil then mnhParameters:=newListLiteral;
      result:=mnhParameters;
      mnhParameters^.rereference;
    end else result:=nil;
  end;

FUNCTION serialize_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params<>nil) and (params^.size=1)
    then result:=newStringLiteral(serialize(params^.value(0),tokenLocation,context.adapters,false,false))
    else if (params<>nil) and (params^.size=2) and (params^.value(1)^.literalType=lt_boolean)
    then result:=newStringLiteral(serialize(params^.value(0),tokenLocation,context.adapters,false,P_boolLiteral(params^.value(1))^.value))
    else result:=nil;
  end;

FUNCTION deserialize_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string)
    then result:=deserialize(P_stringLiteral(params^.value(0))^.value,tokenLocation,context.adapters,2)
    else result:=nil;
  end;

FUNCTION bits_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR bits:bitpacked array [0..63] of boolean;
      k:longint;
  begin
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_int) then begin
      initialize(bits);
      move(P_intLiteral(params^.value(0))^.value,bits,8);
      result:=newListLiteral;
      for k:=0 to 63 do P_listLiteral(result)^.appendBool(bits[k]);
    end else result:=nil;
  end;

{$MACRO ON}
{$define FUNC_ID:=isScalar}
{$define TYPE_CHECK:=tt_typeCheckScalar}
{$define GENERIC_TYPE_CHECK:=
FUNCTION FUNC_ID (CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;              ;
    if (params<>nil) and (params^.size=1) then
    result:=newBoolLiteral(params^.value(0)^.literalType in C_matchingTypes[TYPE_CHECK])
    else if (TYPE_CHECK in C_modifieableTypeChecks) and (params^.size=2) and (params^.value(1)^.literalType=lt_int) then
    result:=newBoolLiteral((params^.value(0)^.literalType in C_matchingTypes[TYPE_CHECK])
       and (   (params^.value(0)^.literalType<>lt_expression     ) or (P_expressionLiteral(params^.value(0))^.arity=P_intLiteral(params^.value(1))^.value))
       and (not(params^.value(0)^.literalType in C_validListTypes) or (P_listLiteral      (params^.value(0))^.size =P_intLiteral(params^.value(1))^.value)));
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

{$define FUNC_ID:=isExpression}
{$define TYPE_CHECK:=tt_typeCheckExpression}
GENERIC_TYPE_CHECK;

CONSTRUCTOR T_mnhSystemPseudoPackage.create;
  begin end;

DESTRUCTOR T_mnhSystemPseudoPackage.destroy;
  begin end;

FUNCTION T_mnhSystemPseudoPackage.getPath: ansistring;
  begin
    result:='[MNH]';
  end;

{ T_intFuncCallback }

CONSTRUCTOR T_intFuncCallback.create(CONST namespace:T_namespace; CONST unqualifiedId:T_idString; CONST func:F_PARAM_TOKLOC_CONTEXT_TO_LITERAL);
  begin
    id:=C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+unqualifiedId;
    call:=func;
    location.package:=mnhSystemPseudoPackage;
    location.column:=1;
    location.line:=length(uniqueBuiltinFunctions)+1;
    setLength(uniqueBuiltinFunctions,location.line);
    uniqueBuiltinFunctions[location.line-1]:=@self;
  end;

DESTRUCTOR T_intFuncCallback.destroy;
  begin end;

FUNCTION T_intFuncCallback.getId: T_idString;
  begin
    result:=id;
  end;

FUNCTION T_intFuncCallback.getLocation: T_tokenLocation;
  begin
    result:=location;
  end;

PROCEDURE finalizeUniqueBuiltinFunctions;
  VAR i:longint;
  begin
    for i:=0 to length(uniqueBuiltinFunctions)-1 do dispose(uniqueBuiltinFunctions[i],destroy);
    setLength(uniqueBuiltinFunctions,0);;
  end;

INITIALIZATION
  intrinsicRuleMap.create;
  setLength(uniqueBuiltinFunctions,0);
  new(mnhSystemPseudoPackage,create);

  registerRule(SYSTEM_BUILTIN_NAMESPACE,'clearPrint',@clearPrint_imp,'clearPrint(...);#Clears the output and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'print',@print_imp,'print(...);#Prints out the given parameters and returns void#if tabs and line breaks are part of the output, a default pretty-printing is used');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'typeOf',@typeOf_imp,'typeOf(x);#Returns a string representation of the type of x');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'mnhParameters',@mnhParameters_imp,'mnhParameters;#Returns the command line parameters/switches passed on program startup');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'serialize',@serialize_impl,'serialize(x);#Returns a string representing x. Strings will NOT(!) be compressed.#serialize(x,compressStrings:boolean);#Returns a string representing x.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deserialize',@deserialize_impl,'deserialize(s:string);#Returns the literal represented by s which was created using serialize(x)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'bits',@bits_impl,'bits(i:int);#Returns the bits of i');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isScalar',@isScalar              ,'isScalar(x);//Returns true if x is a scalar.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isBoolean',@isBoolean            ,'isBoolean(x);//Returns true if x is a boolean.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isInt',@isInt                    ,'isInt(x);//Returns true if x is an int.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isReal',@isReal                  ,'isReal(x);//Returns true if x is numeric.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isNumeric',@isNumeric            ,'isNumeric(x);//Returns true if x is a real.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isString',@isString              ,'isString      (x);//Returns true if x is a string.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isList',@isList                  ,'isList        (x);//Returns true if x is a list. Specify an additional int parameter to additionally check the length.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isBoolList',@isBoolList          ,'isBoolList    (x);//Returns true if x is a boolList. Specify an additional int parameter to additionally check the length.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isIntList',@isIntList            ,'isIntList     (x);//Returns true if x is an intList. Specify an additional int parameter to additionally check the length.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isRealList',@isRealList          ,'isRealList    (x);//Returns true if x is a realList. Specify an additional int parameter to additionally check the length.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isStringList',@isStringList      ,'isStringList  (x);//Returns true if x is a stringList. Specify an additional int parameter to additionally check the length.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isNumList',@isNumList            ,'isNumList     (x);//Returns true if x is a numList. Specify an additional int parameter to additionally check the length.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isKeyValueList',@isKeyValueList  ,'isKeyValueList(x);//Returns true if x is a keyValueList. Specify an additional int parameter to additionally check the length.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isExpression',@isExpression      ,'isExpression  (x);//Returns true if x is an expression. Specify an additional int parameter to additionally check the arity.');
  system.initCriticalSection(print_cs);
FINALIZATION
  if mnhParameters<>nil then disposeLiteral(mnhParameters);
  intrinsicRuleMap.destroy;
  dispose(mnhSystemPseudoPackage,destroy);
  finalizeUniqueBuiltinFunctions;
  system.doneCriticalSection(print_cs);

end.
