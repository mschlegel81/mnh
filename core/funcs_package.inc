{$ifdef include_implementation}
{$i func_defines.inc}
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
        for i:=0 to P_listLiteral(arg0)^.size-1 do append(src,P_stringLiteral(P_listLiteral(arg0)^.value[i])^.value);
      end;
      result:=messagesToLiteralForSandbox(sandbox^.execute(src,recycler,context.getGlobals^.prng.dwordRandom),C_textMessages-[mt_timing_info]);
    end;
  end;

FUNCTION try_imp intFuncSignature;
  FUNCTION performTry(CONST toTry:P_expressionLiteral; CONST inCaseOfFailure:P_literal):P_literal;
    VAR messagesLiteral:P_literal;
        tryMessages:T_messagesErrorHolder;
        oldMessages:P_messages;
    begin
      oldMessages:=context.messages;
      tryMessages.createErrorHolder(context.messages,[mt_el3_evalError..mt_el3_userDefined]);
      context.messages:=@tryMessages;

      result:=toTry^.evaluateToLiteral(tokenLocation,@context,@recycler,nil,nil).literal;
      if (result=nil) or (tryMessages.getFlags*[FlagError,FlagFatalError]<>[]) then begin
        if (result<>nil) then disposeLiteral(result);
        //Error handling
        if inCaseOfFailure=nil then begin
          tryMessages.clear;
          result:=newVoidLiteral
        end else if inCaseOfFailure^.literalType=lt_expression then begin
          if P_expressionLiteral(inCaseOfFailure)^.canApplyToNumberOfParameters(1) then begin
            messagesLiteral:=messagesToLiteralForSandbox(tryMessages.storedMessages(false),C_errorMessageTypes[4]+C_errorMessageTypes[3]);
            tryMessages.clear;
            context.messages:=oldMessages;
            result:=P_expressionLiteral(inCaseOfFailure)^.evaluateToLiteral(tokenLocation,@context,@recycler,messagesLiteral,nil).literal;
            disposeLiteral(messagesLiteral);
          end else begin
            tryMessages.clear;
            context.messages:=oldMessages;
            result:=P_expressionLiteral(inCaseOfFailure)^.evaluateToLiteral(tokenLocation,@context,@recycler,nil,nil).literal;
          end;
        end else begin
          tryMessages.clear;
          result:=inCaseOfFailure^.rereferenced;
        end;
      end;
      tryMessages.destroy;
      context.messages:=oldMessages;
    end;

  begin
    result:=nil;
    if (params^.size>=1) and (arg0^.literalType=lt_expression) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(0)) then begin
      if (params^.size=1) then begin
        result:=performTry(P_expressionLiteral(arg0),nil );
      end else if (params^.size=2) then begin
        result:=performTry(P_expressionLiteral(arg0),arg1);
      end;
    end;
  end;

FUNCTION runScript_imp intFuncSignature;
  VAR scriptParams:T_arrayOfString;
      i:longint;
      connect:longint=0;
      enforceDeterminism:boolean=false;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.size<=4) and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType in [lt_emptyList,lt_stringList])
    then begin
      for i:=2 to params^.size-1 do case params^.value[i]^.literalType of
        lt_smallint: begin
          connect:=P_smallIntLiteral(params^.value[i])^.value;
          if (connect<0) or (connect>3) then exit(nil);
        end;
        lt_boolean: enforceDeterminism:=P_boolLiteral(params^.value[i])^.value;
        else exit(nil);
      end;
      setLength(scriptParams,list1^.size);
      for i:=0 to list1^.size-1 do scriptParams[i]:=P_stringLiteral(list1^.value[i])^.value;
      result:=sandbox^.runScript(str0^.value,scriptParams,tokenLocation,@context,recycler,byte(connect),enforceDeterminism);
    end;
  end;

FUNCTION inspect_imp intFuncSignature;
  VAR i:longint;
      id:string;
      tempPackage:P_package=nil;
      source:T_arrayOfString;
      loadMessages:P_collectionLiteral=nil;
      inspectedPackageIsTemporary:boolean=true;

  PROCEDURE obtainPackage(CONST callerPackage:P_package; CONST locationForWarning:T_tokenLocation; CONST filenameOrId:string; VAR context:T_context);
    VAR fileName:string='';
        id      :string='';
        loadedPackage:P_package;
    begin
      for loadedPackage in callerPackage^.mainPackage^.secondaryPackages do
      if (loadedPackage^.getId=filenameOrId) or
         (loadedPackage^.getPath=filenameOrId) or
         (SameFileName(loadedPackage^.getPath,filenameOrId)) then begin
        tempPackage:=loadedPackage;
        inspectedPackageIsTemporary:=false;
        exit;
      end;

      if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
      then fileName:=expandFileName(filenameOrId)
      else id      :=filenameOrId;
      if id<>'' then fileName:=locateSource(extractFilePath(callerPackage^.getPath),id);
      if (fileName='') or not(fileExists(fileName)) then begin
        context.messages^.postTextMessage(mt_el2_warning,locationForWarning, 'Cannot locate package with id or path "'+id+'"');
        tempPackage:=nil;
        exit;
      end;
      if context.checkSideEffects('inspect',tokenLocation,[se_readFile]) then new(tempPackage,create(newFileCodeProvider(fileName),nil));
    end;

  PROCEDURE loadTemporaryPackage;
    VAR msg:T_storedMessages;
        k:longint;
    begin
      msg:=sandbox^.loadForCodeAssistance(tempPackage^,recycler);
      loadMessages:=messagesToLiteralForSandbox( msg,C_errorsAndWarnings);
      for k:=0 to length(msg)-1 do disposeMessage(msg[k]);
      setLength(msg,0);
    end;

  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=P_abstractPackage(tokenLocation.package)^.inspect(true,@context,recycler)
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      id:=P_stringLiteral(arg0)^.value;

      obtainPackage(P_package(tokenLocation.package),tokenLocation,id,context);
      if tempPackage<>nil then begin
        if inspectedPackageIsTemporary then loadTemporaryPackage;
        result:=tempPackage^.inspect(not(inspectedPackageIsTemporary),@context,recycler);
        if loadMessages<>nil then mapResult^.put('messages',loadMessages,false);
        if inspectedPackageIsTemporary then dispose(tempPackage,destroy);
        exit(result);
      end;
      result:=newListLiteral;
    end else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_stringList) then begin
      setLength(source,list1^.size);
      for i:=0 to list1^.size-1 do source[i]:=P_stringLiteral(list1^.value[i])^.value;
      new(tempPackage,create(newVirtualFileCodeProvider(str0^.value,source),nil));
      loadTemporaryPackage;
      result:=tempPackage^.inspect(false,@context,recycler)^.put('messages',loadMessages,false);
      dispose(tempPackage,destroy);
    end;
  end;

FUNCTION inspectAll_imp intFuncSignature;
  VAR main,p:P_package;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      main:=P_package(tokenLocation.package)^.mainPackage;
      result:=newMapLiteral;
      for p in main^.secondaryPackages do
      mapResult^.put(p   ^.getPath,p   ^.inspect(true,@context,recycler),false);
      mapResult^.put(main^.getPath,main^.inspect(true,@context,recycler),false);
    end;
  end;

FUNCTION writeAllDataStores_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.writeDataStores(context.messages,true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION writeDataStores_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      P_package(tokenLocation.package)^.writeDataStores(context.messages,false);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

{$endif}
{$ifdef include_initialization}
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sandBoxExecute',@sandBoxExecute_imp,ak_unary,'sandBoxExecute(source);//Executes the source (string or stringList) and returns created messages');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'try',@try_imp,ak_variadic,
             'try(E:Expression(0));//Evaluates E and returns the result if successful or void if failed.#'+
             'try(E:Expression(0),except(1):Expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed with the errors as first paramter ($0).#'+
             'try(E:Expression(0),except:Expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed without paramters.#'+
             'try(E:Expression(0),except);//Evaluates E and returns the result if successful. Otherwise <except> (any type except expression) is returned.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'runScript'     ,@runScript_imp     ,ak_binary,'runScript(filenameOrId:String,parameters:StringList);//executes the given script - in the current program instance but strictly isolated#'+
                                                                                           'runScript(filenameOrId:String,parameters:StringList,connectionLevel);//executes the given script with connectionLevel#  // 0: isolated#  // 1: print#  // 2: print and warnings#  // 3: print, warnings and errors#  // Add an additional boolean parameter true to enforce determinism');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'inspect'          ,@inspect_imp           ,ak_variadic_1,'inspect;//Returs information on the current package#inspect(idOrPath:String);//Returns information on an imported package or a specified MNH source file#inspect(path:String,source:StringList);//Returns information of a package with given path and source');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'inspectAll'       ,@inspectAll_imp        ,ak_nullary,'inspectAll;//Returs information on all currently loaded packages');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeAllDataStores',@writeAllDataStores_imp,ak_nullary,'writeAllDataStores;//Writes back all modified datastores (including stores in imported packages).');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeDataStores'   ,@writeDataStores_imp   ,ak_nullary,'writeDataStores;//Writes back all modified datastores of the current package (i.e. excluding stores in imported packages).');
{$endif}

