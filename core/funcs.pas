UNIT funcs;
INTERFACE
{$i func_defines.inc}
USES myGenerics,
     mnh_constants,
     basicTypes,
     mnh_messages,
     out_adapters,
     litVar,
     recyclers,
     contexts;
TYPE
  T_arityKind=(ak_nullary,
               ak_unary,
               ak_binary,
               ak_ternary,
               ak_quartary,
               ak_variadic,
               ak_variadic_1,
               ak_variadic_2,
               ak_variadic_3,
               ak_variadic_4);

  P_intFuncCallback=FUNCTION intFuncSignature;
  P_builtinFunctionMetaData=^T_builtinFunctionMetaData;
  T_builtinFunctionMetaData=object
    arityKind      :T_arityKind;
    namespace      :T_namespace;
    unqualifiedId  :T_idString;
    sideEffects    :T_sideEffects;
    functionPointer:P_intFuncCallback;
    wrappedFunction:P_expressionLiteral;
    CONSTRUCTOR create(CONST arityKind_      :T_arityKind;
                       CONST namespace_      :T_namespace;
                       CONST unqualifiedId_  :T_idString;
                       CONST sideEffects_    :T_sideEffects;
                       CONST functionPointer_:P_intFuncCallback);
    DESTRUCTOR destroy;
    FUNCTION qualifiedId:string;
  end;

  T_builtinFunctionMetaDatas=array of P_builtinFunctionMetaData;

  { T_functionMap }

  T_functionMap=object
    private
      functionMapCs:    TRTLCriticalSection;
      uniqueMetaDatas:  array of P_builtinFunctionMetaData;
      mapByName:        specialize G_stringKeyMap<P_builtinFunctionMetaData>;
      mapByFuncPointer: specialize G_pointerKeyMap<P_builtinFunctionMetaData>;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION registerRule(CONST namespace:T_namespace; CONST name:T_idString; CONST ptr:P_intFuncCallback; CONST aritiyKind:T_arityKind; CONST sideEffects:T_sideEffects=[]; CONST fullNameOnly:boolean=false):P_intFuncCallback;
      FUNCTION reregisterRule(CONST namespace:T_namespace; CONST name:T_idString; CONST ptr:P_intFuncCallback):P_intFuncCallback;
      FUNCTION containsFunctionForId(CONST id:string; OUT ptr:P_intFuncCallback):boolean;
      FUNCTION getFunctionForId(CONST id:string):P_intFuncCallback;
      FUNCTION getSideEffects(CONST ptr:P_intFuncCallback):T_sideEffects;
      FUNCTION canGetIntrinsicRuleAsExpression(CONST id:string; OUT wrapped:P_expressionLiteral):boolean;
      FUNCTION getIntrinsicRuleAsExpression(CONST ptr:P_intFuncCallback; CONST doRereference:boolean):P_expressionLiteral;
      FUNCTION getMeta(CONST ptr:P_intFuncCallback):P_builtinFunctionMetaData;
      FUNCTION containsKey(CONST id:string):boolean;
      FUNCTION getAllIds:T_arrayOfString;
  end;

CONST
  C_arityKind:array[T_arityKind] of record fixedParameters:longint; variadic:boolean end=
                          {ak_nullary   }((fixedParameters:0;       variadic:false),
                          {ak_unary     } (fixedParameters:1;       variadic:false),
                          {ak_binary    } (fixedParameters:2;       variadic:false),
                          {ak_ternary   } (fixedParameters:3;       variadic:false),
                          {ak_quartary  } (fixedParameters:4;       variadic:false),
                          {ak_variadic  } (fixedParameters:0;       variadic:true ),
                          {ak_variadic_1} (fixedParameters:1;       variadic:true ),
                          {ak_variadic_2} (fixedParameters:2;       variadic:true ),
                          {ak_variadic_3} (fixedParameters:3;       variadic:true ),
                          {ak_variadic_4} (fixedParameters:4;       variadic:true ));

PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST L:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST messageTail:ansistring='');
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST x,y:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST messageTail:ansistring='');
FUNCTION genericVectorization(CONST functionId:T_idString; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
PROCEDURE registerValidStringType(CONST id:T_idString);
FUNCTION isStringTypeValid(CONST id:T_idString):boolean;
OPERATOR =(CONST x,y:T_builtinFunctionMetaData):boolean;
VAR builtinFunctionMap:T_functionMap;
    makeBuiltinExpressionCallback:FUNCTION(CONST meta:P_builtinFunctionMetaData):P_expressionLiteral;
    intFuncForOperator:array[tt_comparatorEq..tt_operatorConcatAlt] of P_intFuncCallback;
    failFunction    :P_intFuncCallback;
    validStringTypes:T_arrayOfString;
IMPLEMENTATION
USES sysutils,Classes,
     myStringUtil
     {$ifdef fullVersion},
     mnh_doc{$endif};

PROCEDURE registerValidStringType(CONST id: T_idString);
  begin
    appendIfNew(validStringTypes,id);
  end;

FUNCTION isStringTypeValid(CONST id: T_idString): boolean;
  begin
    result:=arrContains(validStringTypes,id);
  end;

OPERATOR =(CONST x,y:T_builtinFunctionMetaData):boolean;
  begin
    result:=(x.namespace=y.namespace) and (x.unqualifiedId=y.unqualifiedId);
  end;

TYPE formatTabsOption=(ft_always,ft_never,ft_onlyIfTabsAndLinebreaks);

CONSTRUCTOR T_functionMap.create;
  begin
    initCriticalSection(functionMapCs);
    setLength(uniqueMetaDatas,0);
    mapByName.create(2);
    mapByFuncPointer.create(2);
  end;

DESTRUCTOR T_functionMap.destroy;
  VAR i:longint;
  begin
    enterCriticalSection(functionMapCs);
    mapByFuncPointer.destroy;
    mapByName       .destroy;
    if length(uniqueMetaDatas)>0 then begin
      for i:=0 to length(uniqueMetaDatas)-1 do dispose(uniqueMetaDatas[i],destroy);
      setLength(uniqueMetaDatas,0);
    end;
    leaveCriticalSection(functionMapCs);
    doneCriticalSection(functionMapCs);
  end;

FUNCTION T_functionMap.registerRule(CONST namespace: T_namespace; CONST name: T_idString; CONST ptr: P_intFuncCallback; CONST aritiyKind: T_arityKind; CONST sideEffects: T_sideEffects; CONST fullNameOnly: boolean): P_intFuncCallback;
  VAR meta,prevMeta:P_builtinFunctionMetaData;
      reusingExistingFunction:boolean;
  begin
    reusingExistingFunction:=mapByFuncPointer.containsKey(ptr,prevMeta);

    new(meta,create(aritiyKind,namespace,name,sideEffects,ptr));
    assert(not(mapByName.containsKey(meta^.qualifiedId)));
    setLength(uniqueMetaDatas,length(uniqueMetaDatas)+1);
    uniqueMetaDatas[length(uniqueMetaDatas)-1]:=meta;

    if not(fullNameOnly)
    then mapByName  .put(name             ,meta);
    mapByName       .put(meta^.qualifiedId,meta);
    if not(reusingExistingFunction) then mapByFuncPointer.put(ptr,meta);
    {$ifdef fullVersion}registerDoc(meta^.qualifiedId,fullNameOnly);{$endif}
    result:=ptr;
  end;

FUNCTION T_functionMap.reregisterRule(CONST namespace: T_namespace; CONST name: T_idString; CONST ptr: P_intFuncCallback): P_intFuncCallback; VAR meta:P_builtinFunctionMetaData;
  VAR metaByPointer:P_builtinFunctionMetaData;
  begin
    if mapByName.containsKey(C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+name,meta) then begin
      mapByFuncPointer.containsKey(meta^.functionPointer,metaByPointer);

      {$ifdef debugMode}
      writeln(stdErr,'reregistering rule '+meta^.qualifiedId+' ('+C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+name+') old pointer: '+
                     IntToHex(ptrint(meta^.functionPointer),32)+' new pointer: ',IntToHex(ptrint(ptr),32));
      {$endif}
      assert(meta=metaByPointer);
      //Entries by name stay unchanged
      //Entried by pointer must be updated
      mapByFuncPointer.dropKey(meta^.functionPointer);
      mapByFuncPointer.put(ptr,meta);
      meta^.functionPointer:=ptr;
      result:=ptr;
    end else raise Exception.create('Cannot reregister rule which is not registered yet! Name: '+C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+name);
  end;

FUNCTION T_functionMap.containsFunctionForId(CONST id: string; OUT ptr: P_intFuncCallback): boolean;
  VAR meta:P_builtinFunctionMetaData;
  begin
    if mapByName.containsKey(id,meta) then begin
      ptr:=meta^.functionPointer;
      result:=true;
    end else begin
      ptr:=nil;
      result:=false;
    end;
  end;

FUNCTION T_functionMap.getFunctionForId(CONST id: string): P_intFuncCallback;
  VAR meta:P_builtinFunctionMetaData;
  begin
    result:=nil;
    if mapByName.containsKey(id,meta)
    then result:=meta^.functionPointer
    else assert(false,'No function available for id '+id);
  end;

FUNCTION T_functionMap.getSideEffects(CONST ptr: P_intFuncCallback): T_sideEffects;
  VAR meta:P_builtinFunctionMetaData;
  begin
    if mapByFuncPointer.containsKey(ptr,meta)
    then result:=meta^.sideEffects
    else result:=[];
  end;

FUNCTION T_functionMap.canGetIntrinsicRuleAsExpression(CONST id: string; OUT wrapped:P_expressionLiteral): boolean;
  VAR meta:P_builtinFunctionMetaData;
  begin
    enterCriticalSection(functionMapCs);
    try
      if mapByName.containsKey(id,meta)
      then begin
        if meta^.wrappedFunction=nil
        then meta^.wrappedFunction:=makeBuiltinExpressionCallback(meta);
        wrapped:=P_expressionLiteral(meta^.wrappedFunction^.rereferenced);
        result:=true;
      end else result:=false;
    finally
      leaveCriticalSection(functionMapCs);
    end;
  end;

FUNCTION T_functionMap.getIntrinsicRuleAsExpression(CONST ptr:P_intFuncCallback; CONST doRereference:boolean):P_expressionLiteral;
  VAR meta:P_builtinFunctionMetaData;
  begin
    enterCriticalSection(functionMapCs);
    try
      if mapByFuncPointer.containsKey(ptr,meta)
      then begin
        if meta^.wrappedFunction=nil
        then meta^.wrappedFunction:=makeBuiltinExpressionCallback(meta);
        result:=meta^.wrappedFunction;
        if doRereference then result^.rereference;
      end else assert(false,'No function available for given pointer '+IntToHex(ptrint(ptr),32));
    finally
      leaveCriticalSection(functionMapCs);
    end;
  end;

FUNCTION T_functionMap.getMeta(CONST ptr:P_intFuncCallback):P_builtinFunctionMetaData;
  begin
    if not(mapByFuncPointer.containsKey(ptr,result)) then result:=nil;
  end;

FUNCTION T_functionMap.containsKey(CONST id:string):boolean;
  begin
    result:=mapByName.containsKey(id);
  end;

FUNCTION T_functionMap.getAllIds:T_arrayOfString;
  begin
    result:=mapByName.keySet;
  end;

CONSTRUCTOR T_builtinFunctionMetaData.create(CONST arityKind_: T_arityKind; CONST namespace_: T_namespace; CONST unqualifiedId_: T_idString; CONST sideEffects_: T_sideEffects; CONST functionPointer_: P_intFuncCallback);
  begin
    arityKind      :=arityKind_;
    namespace      :=namespace_;
    unqualifiedId  :=unqualifiedId_;
    sideEffects    :=sideEffects_;
    functionPointer:=functionPointer_;
    wrappedFunction:=nil;
  end;

DESTRUCTOR T_builtinFunctionMetaData.destroy;
  begin
    if wrappedFunction<>nil then dispose(wrappedFunction,destroy);
  end;

FUNCTION T_builtinFunctionMetaData.qualifiedId: string;
  begin
    result:=C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+unqualifiedId;
  end;

PROCEDURE raiseNotApplicableError(CONST functionName: ansistring; CONST L:P_literal; CONST tokenLocation: T_tokenLocation; CONST context:P_context; CONST messageTail: ansistring='');
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function '+functionName+' cannot be applied to type '+L^.typeString+messageTail;
    context^.raiseError(complaintText,tokenLocation);
  end;

PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST x,y:P_literal; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST messageTail:ansistring='');
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function '+functionName+' cannot be applied to parameters of type '+x^.typeString+' and '+y^.typeString+messageTail;
    context^.raiseError(complaintText,tokenLocation);
  end;

FUNCTION genericVectorization(CONST functionId:T_idString; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  VAR anyList:boolean=false;
      firstSet:longint=-1;
      allOkay:boolean=true;
      consensusLength:longint=0;
  PROCEDURE checkLength(CONST L:P_literal; CONST idx:longint); inline;
    VAR s:longint;
    begin
      if L^.literalType in C_listTypes then begin
        s:=P_listLiteral(L)^.size;
        if not(anyList) then begin consensusLength:=s; anyList:=true; end
                        else if    consensusLength<>s then allOkay:=false;
      end else if L^.literalType in C_setTypes then firstSet:=idx;
    end;

  FUNCTION getListSubParameters(CONST index:longint):P_listLiteral; inline;
    VAR k:longint;
        x:P_literal;
    begin
      result:=recycler^.newListLiteral(params^.size);
      for k:=0 to params^.size-1 do begin
        x:=params^.value[k];
        if x^.literalType in C_listTypes
        then result^.append(recycler,P_listLiteral(x)^.value[index],true)
        else result^.append(recycler,x                             ,true);
      end;
    end;

  VAR setIter:T_arrayOfLiteral;
  FUNCTION getSetSubParameters(CONST index:longint):P_listLiteral; inline;
    VAR k:longint;
    begin
      result:=recycler^.newListLiteral(params^.size);
      for k:=0 to params^.size-1 do
      if k=firstSet then result^.append(recycler,setIter  [index],true)
                    else result^.append(recycler,params^.value[k],true);
    end;

  VAR i:longint;
      k:longint=0;
      p :P_listLiteral;
      fp:P_literal;
      f :P_intFuncCallback;
      resultElements:T_arrayOfLiteral;
  begin
    if params=nil then exit(nil);
    if (params^.size=1) and (arg0^.literalType=lt_expression) then
      exit(P_expressionLiteral(arg0)^.applyBuiltinFunction(functionId,tokenLocation,context,recycler));
    for i:=0 to params^.size-1 do checkLength(params^.value[i],i);
    if not(allOkay) or not(anyList xor (firstSet>=0)) then exit(nil);
    if not(builtinFunctionMap.containsFunctionForId(functionId,f)) then raise Exception.create('genericVectorization cannot be applied to unknown function "'+functionId+'"');
    if anyList then begin
      setLength(resultElements,consensusLength);
      for i:=0 to consensusLength-1 do if allOkay then begin
        p:=getListSubParameters(i);
        fp:=f(p,tokenLocation,context,recycler);
        recycler^.disposeLiteral(p);
        if fp=nil then allOkay:=false
        else if not(context^.continueEvaluation) then begin
          allOkay:=false;
          recycler^.disposeLiteral(fp);
        end else begin
          resultElements[k]:=fp;
          inc(k);
        end;
      end;
      if allOkay then begin
        setLength(resultElements,k);
        result:=recycler^.newListLiteral(0);
        P_listLiteral(result)^.setContents(resultElements,recycler);
      end else begin
        setLength(resultElements,k);
        recycler^.disposeLiterals(resultElements);
        exit(nil);
      end;
    end else if firstSet>=0 then begin
      setIter:=P_setLiteral(params^.value[firstSet])^.tempIterableList;
      result:=recycler^.newSetLiteral(length(setIter));
      for i:=0 to length(setIter)-1 do if allOkay then begin
        p:=getSetSubParameters(i);
        fp:=f(p,tokenLocation,context,recycler);
        recycler^.disposeLiteral(p);
        if fp=nil then allOkay:=false
        else if not(context^.continueEvaluation) then begin
          allOkay:=false;
          recycler^.disposeLiteral(fp);
        end else begin
          if fp^.literalType in (C_collectionTypes)
          then begin
            P_setLiteral(result)^.appendAll(recycler,P_collectionLiteral(fp));
            recycler^.disposeLiteral(fp);
          end else P_setLiteral(result)^.append(recycler,fp,false);
          end;
        end;
    end else result:=nil;
    if not(allOkay) then begin
      recycler^.disposeLiteral(result);
      result:=nil;
    end;
  end;

{$WARN 5024 OFF}
{$define FUNC_ID:='clearPrint'}
{$define SIDE_EFFECTS:=[se_output]}
FUNCTION clearPrint_imp intFuncSignature;
  begin
    CHECK_SIDE;
    context^.messages^.postSingal(mt_clearConsole,C_nilSearchTokenLocation);
    result:=newVoidLiteral;
  end;

FUNCTION getStringToPrint(CONST params:P_listLiteral; CONST doFormatTabs:formatTabsOption):T_arrayOfString; {$ifndef debugMode} inline; {$endif}
  VAR i:longint;
      resultParts:T_arrayOfString=();
      tabOrBreak:boolean=false;
  begin
    if params<>nil then begin
      setLength(resultParts,params^.size);
      for i:=0 to params^.size-1 do begin
        case params^.value[i]^.literalType of
          lt_boolean,lt_smallint,lt_bigint,lt_real,lt_expression:
            resultParts[i]:=params^.value[i]^.toString;
          lt_string:
            resultParts[i]:=P_stringLiteral(params^.value[i])^.value;
          lt_list..lt_emptyMap:
            resultParts[i]:=params^.value[i]^.toString;
        end;
      end;
    end else setLength(resultParts,0);
    if doFormatTabs=ft_never then exit(join(resultParts,''));
    setLength(result,1);
    result[0]:=join(resultParts,'');
    tabOrBreak:=(doFormatTabs=ft_always);
    i:=1;
    while (not(tabOrBreak)) and (i<=length(result[0])) do begin
      tabOrBreak:=tabOrBreak or (result[0][i] in [C_lineBreakChar,C_tabChar,C_invisibleTabChar]);
      inc(i);
    end;
    if tabOrBreak then result:=formatTabs(split(result[0]));
  end;

{$define FUNC_ID:='print'}
FUNCTION print_imp intFuncSignature;
  begin
    CHECK_SIDE;
    context^.messages^.postTextMessage(mt_printline,tokenLocation,getStringToPrint(params,ft_onlyIfTabsAndLinebreaks));
    result:=newVoidLiteral;
  end;

{$define FUNC_ID:='printDirect'}
FUNCTION printDirect_imp intFuncSignature;
  begin
    CHECK_SIDE;
    context^.messages^.postTextMessage(mt_printdirect,C_nilSearchTokenLocation,getStringToPrint(params,ft_never));
    result:=newVoidLiteral;
  end;

{$define FUNC_ID:='log'}
FUNCTION log_imp intFuncSignature;
  begin
    CHECK_SIDE;
    context^.messages^.postTextMessage(mt_log,tokenLocation,getStringToPrint(params,ft_always));
    result:=newVoidLiteral;
  end;

{$define FUNC_ID:='note'}
FUNCTION note_imp intFuncSignature;
  begin
    CHECK_SIDE;
    context^.messages^.postTextMessage(mt_el1_userNote,tokenLocation,getStringToPrint(params,ft_always));
    result:=newVoidLiteral;
  end;

{$define FUNC_ID:='warn'}
FUNCTION warn_imp intFuncSignature;
  begin
    CHECK_SIDE;
    context^.messages^.postTextMessage(mt_el2_userWarning,tokenLocation,getStringToPrint(params,ft_always));
    result:=newVoidLiteral;
  end;

FUNCTION fail_impl intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then context^.raiseError('Fail',tokenLocation,mt_el3_userDefined)
    else begin
      context^.raiseError(join(getStringToPrint(params,ft_always),C_lineBreakChar),tokenLocation,mt_el3_userDefined);
      result:=newVoidLiteral;
    end;
    result:=nil;
  end;

FUNCTION assert_impl intFuncSignature;
  VAR failParam:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_boolean) then begin
      if not(bool0^.value) then begin
        failParam:=params^.tail(recycler);
        result:=fail_impl(failParam,tokenLocation,context,recycler);
        recycler^.disposeLiteral(failParam);
      end else result:=newVoidLiteral;
    end;
  end;

{$define FUNC_ID:='halt'}
{$define SIDE_EFFECTS:=[se_alterContextState]}
FUNCTION halt_impl intFuncSignature;
  begin
    CHECK_SIDE;
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      context^.messages^.setStopFlag;
      result:=newVoidLiteral;
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_smallint) then begin
      context^.messages^.setUserDefinedExitCode(P_smallIntLiteral(arg0)^.value);
      context^.messages^.setStopFlag;
      result:=newVoidLiteral;
    end;
  end;

FUNCTION allBuiltinFunctions intFuncSignature;
  VAR meta:P_builtinFunctionMetaData;
  begin
    if (params<>nil) and (params^.size>0) then exit(nil);
    result:=recycler^.newSetLiteral(length(builtinFunctionMap.uniqueMetaDatas));
    for meta in builtinFunctionMap.uniqueMetaDatas do setResult^.appendString(recycler,meta^.qualifiedId);
  end;

INITIALIZATION
  initialize(validStringTypes);
  setLength(validStringTypes,0);
  builtinFunctionMap.create;
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'clearPrint'   ,@clearPrint_imp   ,ak_nullary ,[se_output]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'print'        ,@print_imp        ,ak_variadic,[se_output]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'printDirect'  ,@printDirect_imp  ,ak_variadic,[se_output]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'log'          ,@log_imp          ,ak_variadic,[se_output]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'note'         ,@note_imp         ,ak_variadic,[se_output]);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'warn'         ,@warn_imp         ,ak_variadic,[se_output]);
  failFunction:=
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'fail'         ,@fail_impl        ,ak_variadic);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'assert'       ,@assert_impl      ,ak_variadic_1);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'halt'         ,@halt_impl        ,ak_variadic  ,[se_alterContextState]);
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'listBuiltin'  ,@allBuiltinFunctions,ak_nullary);
FINALIZATION
  builtinFunctionMap.destroy;

end.
