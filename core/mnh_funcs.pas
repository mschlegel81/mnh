UNIT mnh_funcs;
INTERFACE
{$i mnh_func_defines.inc}
USES sysutils,myGenerics,mnh_constants,mnh_litVar,mnh_out_adapters,mnh_basicTypes,mnh_contexts,
     myStringUtil,Classes{$ifdef fullVersion},mnh_doc{$endif};
TYPE
  P_intFuncCallback=FUNCTION intFuncSignature;

  P_identifiedInternalFunction=^T_identifiedInternalFunction;
  T_identifiedInternalFunction=object(T_objectWithIdAndLocation)
    location:T_tokenLocation;
    id:T_idString;
    CONSTRUCTOR create(CONST namespace:T_namespace; CONST unqualifiedId:T_idString);
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

FUNCTION registerRule(CONST namespace:T_namespace; CONST name:T_idString; CONST ptr:P_intFuncCallback; CONST explanation:ansistring; CONST fullNameOnly:boolean=false):P_intFuncCallback;
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST typ:T_literalType; CONST messageTail:ansistring; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters);
PROCEDURE setMnhParameters(CONST p:T_arrayOfString);
IMPLEMENTATION

VAR mnhParameters:P_listLiteral=nil;
    mnhSystemPseudoPackage:P_mnhSystemPseudoPackage;

FUNCTION registerRule(CONST namespace: T_namespace; CONST name:T_idString; CONST ptr: P_intFuncCallback; CONST explanation: ansistring; CONST fullNameOnly: boolean):P_intFuncCallback;
  begin
    result:=ptr;
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
FUNCTION clearPrint_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size>0) then exit(nil);
    system.enterCriticalSection(print_cs);
    context.adapters^.clearPrint();
    system.leaveCriticalSection(print_cs);
    result:=newVoidLiteral;
  end;

FUNCTION getStringToPrint(CONST params:P_listLiteral):ansistring; inline;
  VAR i:longint;
  begin
    result:='';
    if params<>nil then for i:=0 to params^.size-1 do case params^.value(i)^.literalType of
      lt_boolean,
      lt_int,
      lt_real,
      lt_string,
      lt_expression: result:=result + P_scalarLiteral(params^.value(i))^.stringForm;
      lt_list..lt_listWithError: result:=result + params^.value(i)^.toString;
    end;
  end;

FUNCTION print_imp intFuncSignature;
  begin
    system.enterCriticalSection(print_cs);
    context.adapters^.printOut(formatTabs(split(getStringToPrint(params))));
    system.leaveCriticalSection(print_cs);
    result:=newVoidLiteral;
  end;

FUNCTION note_imp intFuncSignature;
  begin
    context.adapters^.raiseCustomMessage(mt_el1_userNote,getStringToPrint(params),tokenLocation);
    result:=newVoidLiteral;
  end;

FUNCTION warn_imp intFuncSignature;
  begin
    context.adapters^.raiseCustomMessage(mt_el2_userWarning,getStringToPrint(params),tokenLocation);
    result:=newVoidLiteral;
  end;

FUNCTION fail_impl intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then context.adapters^.raiseCustomMessage(mt_el3_userDefined,'Fail.',tokenLocation)
    else begin
      context.adapters^.raiseCustomMessage(mt_el3_userDefined,getStringToPrint(params),tokenLocation);
      result:=newVoidLiteral;
    end;
    result:=nil;
  end;

FUNCTION mnhParameters_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      if mnhParameters=nil then mnhParameters:=newListLiteral;
      result:=mnhParameters;
      mnhParameters^.rereference;
    end else result:=nil;
  end;

FUNCTION serialize_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1)
    then result:=newStringLiteral(serialize(arg0,tokenLocation,context.adapters,false))
    else result:=nil;
  end;

FUNCTION deserialize_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=deserialize(P_stringLiteral(arg0)^.value,tokenLocation,context.adapters)
    else result:=nil;
  end;

FUNCTION bits_impl intFuncSignature;
  VAR bits:bitpacked array [0..63] of boolean;
      k:longint;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int) then begin
      initialize(bits);
      move(P_intLiteral(arg0)^.value,bits,8);
      result:=newListLiteral;
      for k:=0 to 63 do lResult^.appendBool(bits[k]);
    end else result:=nil;
  end;


CONSTRUCTOR T_mnhSystemPseudoPackage.create;
  begin end;

DESTRUCTOR T_mnhSystemPseudoPackage.destroy;
  begin end;

FUNCTION T_mnhSystemPseudoPackage.getPath: ansistring;
  begin
    result:='[MNH]';
  end;

CONSTRUCTOR T_identifiedInternalFunction.create(CONST namespace:T_namespace; CONST unqualifiedId:T_idString);
  begin
    id:=C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+unqualifiedId;
    location.package:=mnhSystemPseudoPackage;
    location.column:=1;
    location.line:=hash(unqualifiedId);
  end;

DESTRUCTOR T_identifiedInternalFunction.destroy;
  begin end;

FUNCTION T_identifiedInternalFunction.getId: T_idString;
  begin
    result:=id;
  end;

FUNCTION T_identifiedInternalFunction.getLocation: T_tokenLocation;
  begin
    result:=location;
  end;

INITIALIZATION
  intrinsicRuleMap.create;
  new(mnhSystemPseudoPackage,create);

  registerRule(SYSTEM_BUILTIN_NAMESPACE,'clearPrint',@clearPrint_imp,'clearPrint(...);//Clears the output and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'print',@print_imp,'print(...);//Prints out the given parameters and returns void#if tabs and line breaks are part of the output, a default pretty-printing is used');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'note',@note_imp,'note(...);//Raises a note of out the given parameters and returns void');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'warn',@warn_imp,'warn(...);//Raises a warning of out the given parameters and returns void');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fail',@fail_impl,'fail;//Raises an exception without a message#fail(...);//Raises an exception with the given message');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'mnhParameters',@mnhParameters_imp,'mnhParameters;#Returns the command line parameters/switches passed on program startup');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'serialize',@serialize_impl,'serialize(x);#Returns a string representing x.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deserialize',@deserialize_impl,'deserialize(s:string);#Returns the literal represented by s which was created using serialize(x)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'bits',@bits_impl,'bits(i:int);#Returns the bits of i');
  system.initCriticalSection(print_cs);
FINALIZATION
  if mnhParameters<>nil then disposeLiteral(mnhParameters);
  intrinsicRuleMap.destroy;
  dispose(mnhSystemPseudoPackage,destroy);
  system.doneCriticalSection(print_cs);

end.
