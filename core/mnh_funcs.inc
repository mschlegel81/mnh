{$ifdef include_implementation}
VAR packageCs:TRTLCriticalSection;
{$i mnh_func_defines.inc}
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

FUNCTION obtainPackage(CONST callerPackage:P_package; CONST locationForWarning:T_tokenLocation; CONST filenameOrId:string; VAR context:T_threadContext; CONST registerAsSecondaryPackage:boolean; OUT freshlyCreated:boolean):P_package;
  VAR fileName:string='';
      id      :string='';
      p       :P_package;
  begin
    enterCriticalSection(packageCs);
    freshlyCreated:=false;
    if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
    then fileName:=expandFileName(filenameOrId)
    else id      :=filenameOrId;
    for p in callerPackage^.mainPackage^.secondaryPackages do
      if ({$ifdef Windows}uppercase{$endif}(id      )={$ifdef Windows}uppercase{$endif}(p^.getCodeProvider^.id    ))
      or ({$ifdef Windows}uppercase{$endif}(fileName)={$ifdef Windows}uppercase{$endif}(expandFileName(p^.getPath)))
      then begin
        if p^.codeChanged or (p^.readyForUsecase<>lu_forImport) then p^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
        leaveCriticalSection(packageCs);
        exit(p);
      end;
    if id<>'' then fileName:=locateSource(extractFilePath(callerPackage^.getPath),id);
    if (fileName='') or not(fileExists(fileName)) then begin
      context.adapters^.raiseWarning('Cannot locate package with id or path "'+id+'"',locationForWarning);
      leaveCriticalSection(packageCs);
      exit(nil);
    end;
    new(result,create(newFileCodeProvider(fileName),callerPackage^.mainPackage));
    result^.load(lu_forImport,context,C_EMPTY_STRING_ARRAY);
    freshlyCreated:=true;
    if registerAsSecondaryPackage then begin
      with callerPackage^.mainPackage^ do begin
        setLength(secondaryPackages,
           length(secondaryPackages)+1);
                  secondaryPackages[
           length(secondaryPackages)-1]:=result;
      end;
      with callerPackage^ do begin
        setLength(dynamicallyUsed,
           length(dynamicallyUsed)+1);
                  dynamicallyUsed[
           length(dynamicallyUsed)-1]:=result;
      end;
    end;
    leaveCriticalSection(packageCs);
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
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sandBoxExecute',@sandBoxExecute_imp,false,ak_unary,'sandBoxExecute(source);//Executes the source (string or stringList) and returns created messages');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'inspect'          ,@inspect_imp          ,false,ak_variadic_1,'inspect;//Returs information on the current package#inspect(idOrPath:string);//Returns information on an imported package or a specified MNH source file#inspect(path:string,source:stringList);//Returns information of a package with given path and source');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'import'           ,@import_imp           ,false,ak_unary     ,'import(idOrPath:string);//Returns information on rules of an imported package or a specified MNH source file');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearRuleCache'   ,@clearRuleCache_imp   ,false,ak_unary     ,'clearRuleCache(id:string);//Clears memoized rule caches for a specified rule and returns void.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearPackageCache',@clearPackageCache_imp,false,ak_unary     ,'clearPackageCache(id:string);//Clears memoized rule caches for a used package specified by id or path and returns void.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'clearAllCaches'   ,@clearAllCaches_imp   ,false,ak_nullary   ,'clearAllCaches;//Clears all memoized rule caches and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeAllDataStores',@writeAllDataStores_imp,false,ak_nullary,'writeAllDataStores;//Writes back all modified datastores (including stores in imported packages).');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeDataStores'   ,@writeDataStores_imp   ,false,ak_nullary,'writeDataStores;//Writes back all modified datastores of the current package (i.e. excluding stores in imported packages).');
  initialize(packageCs);
  initCriticalSection(packageCs);
{$endif}
{$ifdef include_finalization}
  doneCriticalSection(packageCs);
{$endif}

