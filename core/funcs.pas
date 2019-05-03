UNIT funcs;
INTERFACE
{$i func_defines.inc}
USES sysutils,Classes,
     myGenerics,myStringUtil,
     mnh_constants,
     basicTypes,
     mnh_messages,
     out_adapters,
     litVar,
     recyclers,
     contexts
     {$ifdef fullVersion},
     mnh_doc{$endif};
TYPE
  T_arityKind=(ak_nullary,
               ak_unary,
               ak_binary,
               ak_ternary,
               ak_variadic,
               ak_variadic_1,
               ak_variadic_2,
               ak_variadic_3);
CONST
  C_arityKind:array[T_arityKind] of record fixedParameters:longint; variadic:boolean end=
                          {ak_nullary   }((fixedParameters:0;       variadic:false),
                          {ak_unary     } (fixedParameters:1;       variadic:false),
                          {ak_binary    } (fixedParameters:2;       variadic:false),
                          {ak_ternary   } (fixedParameters:3;       variadic:false),
                          {ak_variadic  } (fixedParameters:0;       variadic:true ),
                          {ak_variadic_1} (fixedParameters:1;       variadic:true ),
                          {ak_variadic_2} (fixedParameters:2;       variadic:true ),
                          {ak_variadic_3} (fixedParameters:3;       variadic:true ));

TYPE
  P_intFuncCallback=FUNCTION intFuncSignature;

  P_builtinFunctionMetaData=^T_builtinFunctionMetaData;
  T_builtinFunctionMetaData=object
    arityKind    :T_arityKind;
    namespace    :T_namespace;
    unqualifiedId:T_idString;
    FUNCTION qualifiedId:string;
  end;
  T_intrinsicRuleMap=specialize G_stringKeyMap<P_intFuncCallback>;

VAR
  intFuncForOperator:array[tt_comparatorEq..tt_operatorConcatAlt] of P_intFuncCallback;
  intrinsicRuleMap:T_intrinsicRuleMap;
  builtinMetaMap  :specialize G_pointerKeyMap<T_builtinFunctionMetaData>;
  print_cs        :system.TRTLCriticalSection;
  makeBuiltinExpressionCallback:FUNCTION(CONST f: P_intFuncCallback; CONST meta:T_builtinFunctionMetaData):P_expressionLiteral;
FUNCTION registerRule(CONST namespace:T_namespace; CONST name:T_idString; CONST ptr:P_intFuncCallback; CONST aritiyKind:T_arityKind;{$WARN 5024 OFF}{$ifdef fullVersion}CONST explanation:ansistring;{$endif}CONST fullNameOnly:boolean=false):P_intFuncCallback;
FUNCTION reregisterRule(CONST namespace:T_namespace; CONST name:T_idString; CONST ptr:P_intFuncCallback; CONST fullNameOnly:boolean=false):P_intFuncCallback;
FUNCTION getMeta(CONST p:pointer):T_builtinFunctionMetaData;
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST L:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_context; CONST messageTail:ansistring='');
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST x,y:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_context; CONST messageTail:ansistring='');
FUNCTION genericVectorization(CONST functionId:T_idString; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal;
FUNCTION getIntrinsicRuleAsExpression(CONST p:pointer):P_expressionLiteral;

IMPLEMENTATION
VAR builtinExpressionMap:specialize G_pointerKeyMap<P_expressionLiteral>;

TYPE formatTabsOption=(ft_always,ft_never,ft_onlyIfTabsAndLinebreaks);

FUNCTION registerRule(CONST namespace: T_namespace; CONST name:T_idString; CONST ptr: P_intFuncCallback; CONST aritiyKind:T_arityKind; {$ifdef fullVersion}CONST explanation: ansistring;{$endif} CONST fullNameOnly: boolean):P_intFuncCallback;
  VAR meta:T_builtinFunctionMetaData;
  begin
    result:=ptr;
    if not(fullNameOnly) then
    intrinsicRuleMap.put(                                                  name,result);
    intrinsicRuleMap.put(C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+name,result);
    meta.arityKind:=aritiyKind;
    meta.namespace:=namespace;
    meta.unqualifiedId:=name;
    builtinMetaMap.put(ptr,meta);
    {$ifdef fullVersion}registerDoc(C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+name,explanation,fullNameOnly);{$endif}
  end;

FUNCTION reregisterRule(CONST namespace:T_namespace; CONST name:T_idString; CONST ptr:P_intFuncCallback; CONST fullNameOnly:boolean=false):P_intFuncCallback;
  VAR meta:T_builtinFunctionMetaData;
      previous:P_intFuncCallback;
  begin
    result:=ptr;
    if intrinsicRuleMap.containsKey(C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+name,previous) then begin
      if builtinMetaMap.containsKey(previous)
      then meta:=builtinMetaMap.get(previous)
      else raise Exception.create('Error during cloning meta.');
      builtinMetaMap.put(ptr,meta);
    end;
    if not(fullNameOnly) then
    intrinsicRuleMap.put(                                                  name,result);
    intrinsicRuleMap.put(C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+name,result);
  end;

FUNCTION getMeta(CONST p:pointer):T_builtinFunctionMetaData;
  begin
    result:=builtinMetaMap.get(p);
  end;

FUNCTION T_builtinFunctionMetaData.qualifiedId:string;
  begin
    result:=C_namespaceString[namespace]+ID_QUALIFY_CHARACTER+unqualifiedId;
  end;

PROCEDURE raiseNotApplicableError(CONST functionName: ansistring; CONST L:P_literal; CONST tokenLocation: T_tokenLocation; VAR context:T_context; CONST messageTail: ansistring='');
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function '+functionName+' cannot be applied to type '+L^.typeString+messageTail;
    context.raiseError(complaintText,tokenLocation);
  end;

PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST x,y:P_literal; CONST tokenLocation:T_tokenLocation; VAR context:T_context; CONST messageTail:ansistring='');
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function '+functionName+' cannot be applied to parameters of type '+x^.typeString+' and '+y^.typeString+messageTail;
    context.raiseError(complaintText,tokenLocation);
  end;

FUNCTION genericVectorization(CONST functionId:T_idString; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal;
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
      result:=newListLiteral(params^.size);
      for k:=0 to params^.size-1 do begin
        x:=params^.value[k];
        if x^.literalType in C_listTypes
        then result^.append(P_listLiteral(x)^.value[index],true)
        else result^.append(x                             ,true);
      end;
    end;

  VAR setIter:T_arrayOfLiteral;
  FUNCTION getSetSubParameters(CONST index:longint):P_listLiteral; inline;
    VAR k:longint;
    begin
      result:=newListLiteral(params^.size);
      for k:=0 to params^.size-1 do
      if k=firstSet then result^.append(setIter  [index],true)
                    else result^.append(params^.value[k],true);
    end;

  VAR i:longint;
      p :P_listLiteral;
      fp:P_literal;
      f :P_intFuncCallback;
  begin
    if params=nil then exit(nil);
    if (params^.size=1) and (arg0^.literalType=lt_expression) then
      exit(P_expressionLiteral(arg0)^.applyBuiltinFunction(functionId,tokenLocation,@context,@recycler));
    for i:=0 to params^.size-1 do checkLength(params^.value[i],i);
    if not(allOkay) or not(anyList xor (firstSet>=0)) then exit(nil);
    if not(intrinsicRuleMap.containsKey(functionId,f)) then raise Exception.create('genericVectorization cannot be applied to unknown function "'+functionId+'"');
    if anyList then begin
      result:=newListLiteral(consensusLength);
      for i:=0 to consensusLength-1 do if allOkay then begin
        p:=getListSubParameters(i);
        fp:=f(p,tokenLocation,context,recycler);
        disposeLiteral(p);
        if fp=nil then allOkay:=false
        else if not(context.continueEvaluation) then begin
          allOkay:=false;
          disposeLiteral(fp);
        end else P_listLiteral(result)^.append(fp,false);
      end;
    end else if firstSet>=0 then begin
      result:=newSetLiteral();
      setIter:=P_setLiteral(params^.value[firstSet])^.iteratableList;
      for i:=0 to length(setIter)-1 do if allOkay then begin
        p:=getSetSubParameters(i);
        fp:=f(p,tokenLocation,context,recycler);
        disposeLiteral(p);
        if fp=nil then allOkay:=false
        else if not(context.continueEvaluation) then begin
          allOkay:=false;
          disposeLiteral(fp);
        end else begin
          if fp^.literalType in (C_setTypes+C_listTypes)
          then begin
            P_setLiteral(result)^.appendAll(P_compoundLiteral(fp));
            disposeLiteral(fp);
          end else P_setLiteral(result)^.append(fp,false);
        end;
      end;
      disposeLiteral(setIter);
    end else result:=nil;
    if not(allOkay) then disposeLiteral(result);
  end;

{$WARN 5024 OFF}
FUNCTION clearPrint_imp intFuncSignature;
  begin
    system.enterCriticalSection(print_cs);
    try
      context.messages^.postSingal(mt_clearConsole,C_nilTokenLocation);
    finally
      system.leaveCriticalSection(print_cs);
    end;
    result:=newVoidLiteral;
  end;

FUNCTION getStringToPrint(CONST params:P_listLiteral; CONST doFormatTabs:formatTabsOption):T_arrayOfString; {$ifndef debugMode} inline; {$endif}
  VAR i:longint;
      resultParts:T_arrayOfString;
      tabOrBreak:boolean=false;
  begin
    if params<>nil then begin
      setLength(resultParts,params^.size);
      for i:=0 to params^.size-1 do begin
        case params^.value[i]^.literalType of
          lt_boolean,lt_smallint,lt_bigint,lt_real,lt_expression:
            resultParts[i]:=params^.value[i]^.toString();
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

FUNCTION print_imp intFuncSignature;
  begin
    if not(context.checkSideEffects('print',tokenLocation,[se_output])) then exit(nil);
    system.enterCriticalSection(print_cs);
    try
      context.messages^.postTextMessage(mt_printline,C_nilTokenLocation,getStringToPrint(params,ft_onlyIfTabsAndLinebreaks));
    finally
      system.leaveCriticalSection(print_cs);
    end;
    result:=newVoidLiteral;
  end;

FUNCTION printDirect_imp intFuncSignature;
  begin
    if not(context.checkSideEffects('printDirect',tokenLocation,[se_output])) then exit(nil);
    system.enterCriticalSection(print_cs);
    try
      context.messages^.postTextMessage(mt_printdirect,C_nilTokenLocation,getStringToPrint(params,ft_never));
    finally
      system.leaveCriticalSection(print_cs);
    end;
    result:=newVoidLiteral;
  end;

FUNCTION note_imp intFuncSignature;
  begin
    if not(context.checkSideEffects('note',tokenLocation,[se_output])) then exit(nil);
    context.messages^.postTextMessage(mt_el1_userNote,tokenLocation,getStringToPrint(params,ft_always));
    result:=newVoidLiteral;
  end;

FUNCTION warn_imp intFuncSignature;
  begin
    if not(context.checkSideEffects('warn',tokenLocation,[se_output])) then exit(nil);
    context.messages^.postTextMessage(mt_el2_userWarning,tokenLocation,getStringToPrint(params,ft_always));
    result:=newVoidLiteral;
  end;

FUNCTION fail_impl intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then context.raiseError('Fail',tokenLocation,mt_el3_userDefined)
    else begin
      context.raiseError(join(getStringToPrint(params,ft_always),C_lineBreakChar),tokenLocation,mt_el3_userDefined);
      result:=newVoidLiteral;
    end;
    result:=nil;
  end;

FUNCTION getIntrinsicRuleAsExpression(CONST p:pointer):P_expressionLiteral;
  begin
    if builtinExpressionMap.containsKey(p,result) then exit(P_expressionLiteral(result^.rereferenced));
    result:=makeBuiltinExpressionCallback(P_intFuncCallback(p),getMeta(p));
    result^.rereference;
    builtinExpressionMap.put(p,result);
  end;

PROCEDURE disposeIdentifiedInternalFunction(VAR p:P_expressionLiteral);
  begin
    dispose(p,destroy);
  end;

INITIALIZATION
  intrinsicRuleMap.create;
  builtinMetaMap.create;
  builtinExpressionMap.create(@disposeIdentifiedInternalFunction);

  registerRule(SYSTEM_BUILTIN_NAMESPACE,'clearPrint'   ,@clearPrint_imp   ,ak_nullary {$ifdef fullVersion},'clearPrint;//Clears the output and returns void.'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'print'        ,@print_imp        ,ak_variadic{$ifdef fullVersion},'print(...);//Prints out the given parameters and returns void#//if tabs and line breaks are part of the output, a default pretty-printing is used'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printDirect'  ,@printDirect_imp  ,ak_variadic{$ifdef fullVersion},'printDirect(...);//Prints out the given string without pretty printing or line breaks'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'note'         ,@note_imp         ,ak_variadic{$ifdef fullVersion},'note(...);//Raises a note of out the given parameters and returns void'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'warn'         ,@warn_imp         ,ak_variadic{$ifdef fullVersion},'warn(...);//Raises a warning of out the given parameters and returns void'{$endif});
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fail'         ,@fail_impl        ,ak_variadic{$ifdef fullVersion},'fail;//Raises an exception without a message#fail(...);//Raises an exception with the given message'{$endif});
  system.initCriticalSection(print_cs);
FINALIZATION
  builtinExpressionMap.destroy;
  builtinMetaMap.destroy;
  intrinsicRuleMap.destroy;
  system.doneCriticalSection(print_cs);

end.
