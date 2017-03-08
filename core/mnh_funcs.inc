{$ifdef include_implementation}
VAR builtinLocation_group,
    builtinLocation_try:T_identifiedInternalFunction;

{$i mnh_func_defines.inc}


FUNCTION try_imp intFuncSignature;
  VAR oldAdapters:P_adapters;
      errorCase:boolean;
      messages:P_literal=nil;
  begin
    result:=nil;
    if (params^.size>=1) and (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(0)) and
      ((params^.size=1) or (params^.size=2)) then begin
      context.callStackPush(tokenLocation,@builtinLocation_try,params,nil);
      oldAdapters:=context.enterTryStatementReturningPreviousAdapters;
      result:=P_subrule(arg0)^.evaluateToLiteral(tokenLocation,@context);
      if context.adapters^.noErrors
      then errorCase:=false
      else begin
        if result<>nil then disposeLiteral(result);
        messages:=messagesToLiteralForSandbox(P_collectingOutAdapter(context.adapters^.getAdapter(0))^.storedMessages);
        errorCase:=true;
      end;
      context.leaveTryStatementReassumingPreviousAdapters(oldAdapters,errorCase);
      if errorCase then begin
        if params^.size=2 then begin
          if arg1^.literalType=lt_expression then begin
            if P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)
            then result:=P_subrule(arg1)^.evaluateToLiteral(tokenLocation,@context,messages)
            else result:=P_subrule(arg1)^.evaluateToLiteral(tokenLocation,@context);
          end else begin
            result:=arg1;
            result^.rereference;
          end;
        end else result:=newVoidLiteral;
        disposeLiteral(messages);
      end;
      context.callStackPop();
    end;
  end;

TYPE P_asyncTask=^T_asyncTask;
     T_asyncTask=record
       task:P_token;
       context:P_threadContext;
     end;

FUNCTION doAsync(p:pointer):ptrint;
  begin
    result:=0;
    with P_asyncTask(p)^ do begin
      reduceExpression(task,context^);
      context^.recycler.cascadeDisposeToken(task);
      context^.doneEvaluating;
      dispose(context,destroy);
    end;
    freeMem(p,sizeOf(T_asyncTask));
  end;

FUNCTION async_imp intFuncSignature;
  VAR p:P_asyncTask;
      childContext:P_threadContext;
      parameters:P_listLiteral=nil;
      dummy:P_token;
  begin
    result:=nil;
    if (params^.size>=1) and (arg0^.literalType=lt_expression) and
       ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) then begin
      childContext:=context.getNewAsyncContext;
      if childContext<>nil then begin
        getMem(p,sizeOf(T_asyncTask));
        p^.context:=childContext;
        if params^.size=2 then parameters:=list1;
        if not(P_subrule(arg0)^.replaces(parameters,tokenLocation,p^.task,dummy,context,false)) then begin
          freeMem(p,sizeOf(T_asyncTask));
          childContext^.doneEvaluating;
          dispose(childContext,destroy);
          exit(nil);
        end;
        beginThread(@doAsync,p);
        result:=newVoidLiteral;
      end else begin
        context.adapters^.raiseError('Creation of asynchronous tasks is forbidden for the current context',tokenLocation);
      end;
    end;
  end;



FUNCTION sandBoxExecute_imp intFuncSignature;
  VAR src:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_string,lt_stringList]) then begin
      if arg0^.literalType=lt_string
      then src:=P_stringLiteral(arg0)^.value
      else begin
        setLength(src,0);
        for i:=0 to P_listLiteral(arg0)^.size-1 do append(src,P_stringLiteral(P_listLiteral(arg0)^[i])^.value);
      end;
      result:=messagesToLiteralForSandbox(runAlone(src));
    end;
  end;

FUNCTION group_imp intFuncSignature;
  TYPE T_groupMap=specialize G_literalKeyMap<P_literal>;
  VAR listToGroup:P_listLiteral;
      keyList:T_arrayOfLiteral;
      aggregator:P_expressionLiteral;
      groupMap:T_groupMap;
      groupList :T_groupMap.KEY_VALUE_LIST;
      groupEntry:T_groupMap.CACHE_ENTRY;

  PROCEDURE makeKeysByIndex(CONST index:longint);
    VAR i:longint;
        dummy:P_literal;
    begin
      dummy:=newErrorLiteral;
      setLength(keyList,listToGroup^.size);
      for i:=0 to length(keyList)-1 do begin
        if (listToGroup^[i]^.literalType in C_listTypes) and (P_listLiteral(listToGroup^[i])^.size>index)
        then keyList[i]:=P_listLiteral(listToGroup^[i])^[index]^.rereferenced
        else keyList[i]:=dummy;
      end;
      disposeLiteral(dummy);
    end;

  PROCEDURE addToAggregation(CONST groupKey:P_literal; CONST L:P_literal); inline;
    VAR newLit:P_literal;
        resultLiteral:P_literal;
    begin
      resultLiteral:=groupMap.get(groupKey,nil);

      if aggregator=nil then begin
        if resultLiteral=nil then resultLiteral:=newListLiteral;
        P_listLiteral(resultLiteral)^.append(L,true);
      end else begin
        if resultLiteral=nil then begin
          resultLiteral:=L; L^.rereference;
        end else begin
          newLit:=P_subrule(aggregator)^.evaluateToLiteral(tokenLocation,@context,resultLiteral,L);
          if newLit<>nil then begin
            disposeLiteral(resultLiteral);
            resultLiteral:=newLit;
          end else begin
            context.adapters^.raiseError('Error performing aggregation in group-construct with aggregator '+aggregator^.toString,tokenLocation);
            exit;
          end;
        end;
      end;
      groupMap.put(groupKey,resultLiteral);
    end;

  VAR inputIndex:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.size<=3) and
       (arg0^.literalType in C_listTypes) and
      ((arg1^.literalType in C_listTypes) and (list1^.size=list0^.size) or (arg1^.literalType=lt_int)) and
      ((params^.size=2) or (arg2^.literalType=lt_expression))
    then begin
      listToGroup:=P_listLiteral(arg0);

      if arg1^.literalType=lt_int
      then makeKeysByIndex(P_intLiteral(arg1)^.value)
      else keyList:=list1^.iteratableList;

      if (params^.size=3) then aggregator:=P_expressionLiteral(arg2)
                          else aggregator:=nil;
      if aggregator<>nil then context.callStackPush(tokenLocation,@builtinLocation_group,params,nil);
      groupMap.create();
      for inputIndex:=0 to length(keyList)-1 do if context.adapters^.noErrors then
        addToAggregation(keyList[inputIndex],listToGroup^[inputIndex]);
      disposeLiteral(keyList);

      groupList:=groupMap.keyValueList;
      result:=newListLiteral(length(groupList));
      for groupEntry in groupList do listResult^.append(groupEntry.value,false);
      groupMap.destroy;
      if aggregator<>nil then context.callStackPop();
    end;
  end;

FUNCTION obtainPackage(CONST callerPackage:P_package; CONST locationForWarning:T_tokenLocation; CONST filenameOrId:string; VAR context:T_threadContext; CONST registerAsSecondaryPackage:boolean; OUT freshlyCreated:boolean):P_package;
  VAR fileName:string='';
      id      :string='';
      p       :P_package;
  begin
    freshlyCreated:=false;
    if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
    then fileName:=expandFileName(filenameOrId)
    else id      :=filenameOrId;
    for p in callerPackage^.mainPackage^.secondaryPackages do
      if ({$ifdef Windows}uppercase{$endif}(id      )={$ifdef Windows}uppercase{$endif}(p^.codeProvider^.id    ))
      or ({$ifdef Windows}uppercase{$endif}(fileName)={$ifdef Windows}uppercase{$endif}(expandFileName(p^.getPath)))
      then exit(p);
    if id<>'' then fileName:=locateSource(extractFilePath(callerPackage^.getPath),id);
    if (fileName='') or not(fileExists(fileName)) then begin
      context.adapters^.raiseWarning('Cannot locate package with id or path "'+id+'"',locationForWarning);
      exit(nil);
    end;
    new(result,create(newFileCodeProvider(fileName),callerPackage^.mainPackage));
    result^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
    freshlyCreated:=true;
    if registerAsSecondaryPackage then begin
      setLength(callerPackage^.mainPackage^.secondaryPackages,
         length(callerPackage^.mainPackage^.secondaryPackages)+1);
                callerPackage^.mainPackage^.secondaryPackages[
         length(callerPackage^.mainPackage^.secondaryPackages)-1]:=result;
    end;
  end;

FUNCTION inspect_imp intFuncSignature;
  VAR i:longint;
      id:string;
      tempPackage:P_package;
      source:T_arrayOfString;
      needDispose:boolean=false;
      loadMessages:P_collectionLiteral=nil;

  PROCEDURE loadTemporaryPackage;
    VAR prevAdapters:P_adapters;
        collector   :P_collectingOutAdapter;
        mt:T_messageType;
    begin
      prevAdapters:=context.enterTryStatementReturningPreviousAdapters;
      collector:=P_collectingOutAdapter(context.adapters^.getAdapter(at_sandboxAdapter));
      for mt in T_messageType do collector^.enableMessageType(C_messageTypeMeta[mt].mClass in [mc_echo,mc_warning],[mt]);
      tempPackage^.load(lu_forCodeAssistance,context,C_EMPTY_STRING_ARRAY);
      loadMessages:=messagesToLiteralForSandbox(collector^.storedMessages);
      context.leaveTryStatementReassumingPreviousAdapters(prevAdapters,true);
    end;

  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=P_package(tokenLocation.package)^.inspect
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      id:=P_stringLiteral(arg0)^.value;

      tempPackage:=obtainPackage(P_package(tokenLocation.package),tokenLocation,id,context,false,needDispose);
      if tempPackage<>nil then begin
        if needDispose then loadTemporaryPackage;
        result:=tempPackage^.inspect;
        if loadMessages<>nil then mapResult^.put('messages',loadMessages,false);
        if needDispose then dispose(tempPackage,destroy);
        exit(result);
      end;
      result:=newListLiteral;
    end else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_stringList) then begin
      setLength(source,list1^.size);
      for i:=0 to list1^.size-1 do source[i]:=P_stringLiteral(list1^[i])^.value;
      new(tempPackage,create(newVirtualFileCodeProvider(str0^.value,source),nil));
      loadTemporaryPackage;
      result:=tempPackage^.inspect^.put('messages',loadMessages,false);
      dispose(tempPackage,destroy);
    end;
  end;

FUNCTION import_imp intFuncSignature;
  VAR p:P_package;
      freshlyCreated:boolean;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      p:=obtainPackage(P_package(tokenLocation.package),tokenLocation,str0^.value,context,true,freshlyCreated);
      if freshlyCreated then p^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
      if p=nil then result:=newListLiteral
               else result:=p^.getDynamicUseMeta(context);
    end else result:=nil;
  end;

FUNCTION clearRuleCache_imp intFuncSignature;
  VAR ruleIdToken:T_token;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      ruleIdToken.create;
      ruleIdToken.define(tokenLocation,P_stringLiteral(arg0)^.value,tt_identifier);
      ruleIdToken.resolveRuleId(tokenLocation.package,context.adapters);
      if ruleIdToken.tokType in [tt_customTypeCheck,tt_customTypeRule,tt_localUserRule,tt_importedUserRule] then P_rule(ruleIdToken.data)^.clearCache;
      ruleIdToken.destroy;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION clearPackageCache_imp intFuncSignature;
  VAR i:longint;
      id:string;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.clearPackageCache(false);
      result:=newVoidLiteral;
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      id:=P_stringLiteral(arg0)^.value;
      with P_package(tokenLocation.package)^ do
        for i:=0 to length(packageUses)-1 do begin
          if (packageUses[i].id=id) or (packageUses[i].path=id) then begin
            packageUses[i].pack^.clearPackageCache(false);
            if result=nil then result:=newVoidLiteral;
          end;
        end;
      if result=nil then context.adapters^.raiseError('Cannot locate package with id or path "'+id+'"',tokenLocation);
    end;
  end;

FUNCTION clearAllCaches_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.clearPackageCache(true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION writeAllDataStores_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.writeDataStores(context.adapters^,true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION writeDataStores_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.writeDataStores(context.adapters^,false);
      result:=newVoidLiteral;
    end else result:=nil;
  end;
{$endif}
{$ifdef include_initialization}
  builtinLocation_try.create(SYSTEM_BUILTIN_NAMESPACE,'try');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'try',@try_imp,false,ak_variadic,
               'try(E:expression(0));//Evaluates E and returns the result if successful or void if failed.#'+
               'try(E:expression(0),except(1):expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed with the errors as first paramter ($0).#'+
               'try(E:expression(0),except:expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed without paramters.#'+
               'try(E:expression(0),except);//Evaluates E and returns the result if successful. Otherwise <except> (any type except expression) is returned.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'async',@async_imp,false,ak_variadic_1,'async(E:expression);//Calls E asynchronously (without parameters) and returns void.#'+
               'async(E:expression,par:list);//Calls E@par and asynchronously and returns void.#//Asynchronous tasks are killed at the end of (synchonous) evaluation.');
  builtinLocation_group.create(DEFAULT_BUILTIN_NAMESPACE,'group');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'group'         ,@group_imp         ,true,ak_variadic_2,'group(list,grouping);//Re-groups list by grouping (which is a sub-index or a list)#group(list,grouping,aggregator:expression);//Groups by grouping using aggregator on a per group basis');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sandBoxExecute',@sandBoxExecute_imp,false,ak_unary,'sandBoxExecute(source);//Executes the source (string or stringList) and returns created messages');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'inspect'          ,@inspect_imp          ,false,ak_variadic_1,'inspect;//Returs information on the current package#inspect(idOrPath:string);//Returns information on an imported package or a specified MNH source file#inspect(path:string,source:stringList);//Returns information of a package with given path and source');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'import'           ,@import_imp           ,false,ak_unary     ,'import(idOrPath:string);//Returns information on rules of an imported package or a specified MNH source file');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearRuleCache'   ,@clearRuleCache_imp   ,false,ak_unary     ,'clearRuleCache(id:string);//Clears memoized rule caches for a specified rule and returns void.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearPackageCache',@clearPackageCache_imp,false,ak_unary     ,'clearPackageCache(id:string);//Clears memoized rule caches for a used package specified by id or path and returns void.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearAllCaches'   ,@clearAllCaches_imp   ,false,ak_nullary   ,'clearAllCaches;//Clears all memoized rule caches and returns void.');

  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeAllDataStores',@writeAllDataStores_imp,false,ak_nullary,'writeAllDataStores;//Writes back all modified datastores (including stores in imported packages).');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeDataStores'   ,@writeDataStores_imp   ,false,ak_nullary,'writeDataStores;//Writes back all modified datastores of the current package (i.e. excluding stores in imported packages).');

{$endif}
{$ifdef include_finalization}
  builtinLocation_group .destroy;
  builtinLocation_try   .destroy;
{$endif}
