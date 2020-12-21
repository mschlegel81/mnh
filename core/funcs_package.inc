{$ifdef include_implementation}
{$i func_defines.inc}
FUNCTION sandBoxExecute_imp intFuncSignature;
  VAR src:T_arrayOfString;
      sideEffectWhitelist:T_sideEffects=[];
      i:longint;
      messages:T_storedMessages;
      ExitCode:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=2) and (arg0^.literalType in [lt_string,lt_stringList]) then begin
      if arg0^.literalType=lt_string
      then src:=P_stringLiteral(arg0)^.value
      else begin
        setLength(src,0);
        for i:=0 to P_listLiteral(arg0)^.size-1 do append(src,P_stringLiteral(P_listLiteral(arg0)^.value[i])^.value);
      end;
      if (params^.size=2) and not(canInterpretAsSideEffectList(arg1,true,tokenLocation,context,sideEffectWhitelist))
      then exit(nil);
      messages:=sandbox^.execute(src,sideEffectWhitelist,recycler,ExitCode,context.getGlobals^.prng.dwordRandom);
      result:=messagesToLiteralForSandbox(messages,C_textMessages-[mt_timing_info],ExitCode);
      setLength(messages,0);
    end;
  end;

FUNCTION try_imp intFuncSignature;
  FUNCTION performTry(CONST toTry:P_expressionLiteral; CONST toTryParameters:P_listLiteral; CONST inCaseOfFailure:P_literal):P_literal;
    VAR messagesLiteral:P_literal;
        tryMessages:T_messagesErrorHolder;
        oldMessages:P_messages;
    begin
      oldMessages:=context.messages;
      tryMessages.createErrorHolder(context.messages,[mt_el3_evalError..mt_el3_userDefined]);
      context.messages:=@tryMessages;

      result:=toTry^.evaluate(tokenLocation,@context,@recycler,toTryParameters).literal;
      if ((result=nil) or (tryMessages.getFlags*[FlagError,FlagFatalError]<>[])) and oldMessages^.continueEvaluation then begin
        if (result<>nil) then disposeLiteral(result);
        //Error handling
        if inCaseOfFailure=nil then begin
          tryMessages.clear;
          result:=newVoidLiteral
        end else if inCaseOfFailure^.literalType=lt_expression then begin
          if P_expressionLiteral(inCaseOfFailure)^.canApplyToNumberOfParameters(1) then begin
            messagesLiteral:=messagesToLiteralForSandbox(tryMessages.storedMessages(false),C_errorMessageTypes[4]+C_errorMessageTypes[3],SUPPRESS_EXIT_CODE);
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
    if (params^.size>=1) and (arg0^.literalType=lt_expression) then begin
      if (params^.size=1) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(0))
      then result:=performTry(P_expressionLiteral(arg0),nil,nil)
      else if (params^.size=2) and (arg1^.literalType in C_listTypes) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(list1^.size))
      then result:=performTry(P_expressionLiteral(arg0),list1,nil)
      else if (params^.size=2) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(0))
      then result:=performTry(P_expressionLiteral(arg0),nil,arg1)
      else if (params^.size=3) and (arg1^.literalType in C_listTypes) and (P_expressionLiteral(arg0)^.canApplyToNumberOfParameters(list1^.size))
      then result:=performTry(P_expressionLiteral(arg0),list1,arg2);
    end;
  end;

CONST RUNSCRIPT_KEY_NAME         ='name';
      RUNSCRIPT_KEY_SOURCE       ='source';
      RUNSCRIPT_KEY_PARAMETERS   ='parameters';
      RUNSCRIPT_KEY_SIDE_EFFECTS ='side effects';
      RUNSCRIPT_KEY_ISOLATION    ='connection level';
      RUNSCRIPT_KEY_DETERMINISTIC='deterministic';

FUNCTION runScript_imp intFuncSignature;
  VAR iter:T_arrayOfLiteral;
      entry:P_literal;
      key,value:P_literal;

  VAR scriptName:string='';
  PROCEDURE parseName;
    begin
      if value^.literalType=lt_string
      then scriptName:=P_stringLiteral(value)^.value
      else context.raiseError('Script name must be a string',tokenLocation);
    end;

  FUNCTION listToStrings(CONST L:P_literal):T_arrayOfString;
    VAR j:longint;
    begin
      setLength(result,P_listLiteral(L)^.size);
      for j:=0 to P_listLiteral(L)^.size-1 do result[j]:=P_stringLiteral(P_listLiteral(L)^.value[j])^.value;
    end;

  VAR scriptSource:T_arrayOfString;
  PROCEDURE parseSource;
    begin
      if value^.literalType=lt_stringList
      then scriptSource:=listToStrings(value)
      else context.raiseError('Script source must be a (non empty) string list',tokenLocation);
    end;

  VAR scriptParams:T_arrayOfString;
  PROCEDURE parseParameters;
    begin
      if value^.literalType in [lt_stringList,lt_emptyList]
      then scriptParams:=listToStrings(value)
      else if value^.literalType<>lt_emptySet then context.raiseError('Script parameters must be a string list',tokenLocation);
    end;

  VAR sideEffects:T_sideEffects;
  PROCEDURE parseSideEffects;
    begin
      canInterpretAsSideEffectList(value,true,tokenLocation,context,sideEffects);
    end;

  VAR connect:longint=0;
  PROCEDURE parseIsolation;
    begin
      if (value^.literalType=lt_smallint)
      and (P_smallIntLiteral(value)^.value>=0) and (P_smallIntLiteral(value)^.value<=3)
      then connect:=P_smallIntLiteral(value)^.value
      else context.raiseError('Invalid connection level '+value^.toString(),tokenLocation);
    end;

  VAR enforceDeterminism:boolean=false;
  PROCEDURE parseDeterministic;
    begin
      if boolLit[false].equals(value) then exit;
      if boolLit[true].equals(value)
      then enforceDeterminism:=true
      else context.raiseError('Expected a boolean',tokenLocation);
    end;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_map) then begin
      scriptSource:=C_EMPTY_STRING_ARRAY;
      scriptParams:=C_EMPTY_STRING_ARRAY;
      iter:=map0^.iteratableList;
      for entry in iter do begin
        assert(entry^.literalType in C_listTypes);
        assert(P_listLiteral(entry)^.size=2);
        key  :=P_listLiteral(entry)^.value[0];
        value:=P_listLiteral(entry)^.value[1];
        if key^.literalType=lt_string then begin
          if P_stringLiteral(key)^.value=RUNSCRIPT_KEY_NAME          then parseName else
          if P_stringLiteral(key)^.value=RUNSCRIPT_KEY_SOURCE        then parseSource else
          if P_stringLiteral(key)^.value=RUNSCRIPT_KEY_PARAMETERS    then parseParameters else
          if P_stringLiteral(key)^.value=RUNSCRIPT_KEY_SIDE_EFFECTS  then parseSideEffects else
          if P_stringLiteral(key)^.value=RUNSCRIPT_KEY_DETERMINISTIC then parseDeterministic else
          if P_stringLiteral(key)^.value=RUNSCRIPT_KEY_ISOLATION     then parseIsolation else
          context.raiseError('Invalid map entry with key '+key^.toString(),tokenLocation);
        end else context.raiseError('Invalid map entry with key '+key^.toString(),tokenLocation);
      end;
      disposeLiteral(iter);

      if scriptName='' then context.raiseError('No script name given.',tokenLocation);

      if context.continueEvaluation then result:=
        sandbox^.runScript(scriptName,
                           scriptSource,
                           scriptParams,
                           sideEffects,
                           tokenLocation,
                           @context,
                           recycler,
                           byte(connect),
                           enforceDeterminism);
    end;
  end;

FUNCTION helpOnMain_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0)
    then result:=newStringLiteral(P_package(tokenLocation.package)^.getHelpOnMain)
    else result:=nil;
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
      if (callerPackage<>nil) and (callerPackage^.mainPackage<>nil) and (callerPackage<>@BLANK_ABSTRACT_PACKAGE) and (callerPackage<>@MNH_PSEUDO_PACKAGE) then begin
        for loadedPackage in callerPackage^.mainPackage^.secondaryPackages do
        if (loadedPackage^.getId=filenameOrId) or
           (loadedPackage^.getPath=filenameOrId) or
           (SameFileName(loadedPackage^.getPath,filenameOrId)) then begin
          tempPackage:=loadedPackage;
          inspectedPackageIsTemporary:=false;
        end;
      end;
      if tempPackage=nil then begin
        if lowercase(extractFileExt(filenameOrId))=SCRIPT_EXTENSION
        then fileName:=expandFileName(filenameOrId)
        else id      :=filenameOrId;
        if id<>'' then fileName:=locateSource(extractFilePath(callerPackage^.getPath),id);
        if (fileName='') or not(fileExists(fileName)) then begin
          context.messages^.postTextMessage(mt_el2_warning,locationForWarning, 'Cannot locate package with id or path "'+id+'"');
          tempPackage:=nil;
        end else
        if context.checkSideEffects('inspect',tokenLocation,[se_readFile]) then new(tempPackage,create(newFileCodeProvider(fileName),nil));
      end;
    end;

  PROCEDURE loadTemporaryPackage({$ifdef fullVersion}OUT functionCallInfos:P_callAndIdInfos{$endif});
    VAR msg:T_storedMessages;
        k:longint;
    begin
      msg:=sandbox^.loadForCodeAssistance(tempPackage^,recycler{$ifdef fullVersion},functionCallInfos{$endif});
      loadMessages:=messagesToLiteralForSandbox(msg,C_errorsAndWarnings,SUPPRESS_EXIT_CODE);
      for k:=0 to length(msg)-1 do disposeMessage(msg[k]);
      setLength(msg,0);
    end;

  {$ifdef fullVersion}
  VAR functionCallInfos:P_callAndIdInfos=nil;
  {$endif}
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=P_abstractPackage(tokenLocation.package)^.inspect(true,@context,recycler{$ifdef fullVersion},functionCallInfos{$endif})
    else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      id:=P_stringLiteral(arg0)^.value;
      obtainPackage(P_package(tokenLocation.package),tokenLocation,id,context);
      if tempPackage<>nil then begin
        if inspectedPackageIsTemporary then loadTemporaryPackage({$ifdef fullVersion}functionCallInfos{$endif});
        result:=tempPackage^.inspect(not(inspectedPackageIsTemporary),@context,recycler{$ifdef fullVersion},functionCallInfos{$endif});
        if loadMessages<>nil then mapResult^.put('messages',loadMessages,false);
        if inspectedPackageIsTemporary
        then dispose(tempPackage,destroy);
      end else result:=newListLiteral;
    end else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_stringList) then begin
      setLength(source,list1^.size);
      for i:=0 to list1^.size-1 do source[i]:=P_stringLiteral(list1^.value[i])^.value;
      new(tempPackage,create(newVirtualFileCodeProvider(str0^.value,source),nil));
      loadTemporaryPackage({$ifdef fullVersion}functionCallInfos{$endif});
      result:=tempPackage^.inspect(false,@context,recycler{$ifdef fullVersion},functionCallInfos{$endif})^.put('messages',loadMessages,false);
      dispose(tempPackage,destroy);
    end;
  end;

FUNCTION inspectAll_imp intFuncSignature;
  VAR main,p:P_package;
      {$ifdef fullVersion}
      functionCallInfos:P_callAndIdInfos=nil;
      {$endif}
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      main:=P_package(tokenLocation.package)^.mainPackage;
      result:=newMapLiteral(0);
      for p in main^.secondaryPackages do
      mapResult^.put(p   ^.getPath,p   ^.inspect(true,@context,recycler{$ifdef fullVersion},functionCallInfos{$endif}),false);
      mapResult^.put(main^.getPath,main^.inspect(true,@context,recycler{$ifdef fullVersion},functionCallInfos{$endif}),false);
    end;
  end;

FUNCTION writeAllDataStores_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      result:=newBoolLiteral(P_package(tokenLocation.package)^.writeDataStores(context.messages,true));
    end else result:=nil;
  end;

FUNCTION writeDataStores_imp intFuncSignature;
  begin
    if (params=nil) or (params^.size=0) then begin
      result:=newBoolLiteral(P_package(tokenLocation.package)^.writeDataStores(context.messages,false));
    end else result:=nil;
  end;

{$endif}
{$ifdef include_initialization}
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'sandBoxExecute',@sandBoxExecute_imp,ak_unary{$ifdef fullVersion},'sandBoxExecute(source,sideEffectWhitelist:StringCollection);//Executes the source (String or StringList) and returns created messages#//Optionally you can specify allowed side effects'{$endif});
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'try',@try_imp,ak_variadic{$ifdef fullVersion},
             'try(E:Expression(0));//Evaluates E and returns the result if successful or void if failed.#'+
             'try(E:Expression,parameters:List);//Evaluates E with the given parameters - the parameter list length must match the arity of E#'+
             'try(E:Expression(0),except(1):Expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed with the errors as first paramter ($0).#'+
             'try(E:Expression(0),except:Expression);//Evaluates E and returns the result if successful. Otherwise <except> is executed without paramters.#'+
             'try(E:Expression(0),except);//Evaluates E and returns the result if successful. Otherwise <except> (any type except expression) is returned.'{$endif});
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'runScript'     ,@runScript_imp     ,ak_binary{$ifdef fullVersion},'runScript(config:Map);//executes the given script config#'+
                                                     '//expected keys: "'+RUNSCRIPT_KEY_NAME         +'" (String, mandatory)#'+
                                                     '//               "'+RUNSCRIPT_KEY_SOURCE       +'" (StringList)#'+
                                                     '//               "'+RUNSCRIPT_KEY_PARAMETERS   +'" (StringList)#'+
                                                     '//               "'+RUNSCRIPT_KEY_SIDE_EFFECTS +'" (StringCollection)#'+
                                                     '//               "'+RUNSCRIPT_KEY_ISOLATION    +'" 0: isolated, 1: print, 2: print and warnings, 3: print, warnings and errors#'+
                                                     '//               "'+RUNSCRIPT_KEY_DETERMINISTIC+'" (Boolean, true to enforce determinism of PRGNs)'{$endif});
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'inspect'          ,@inspect_imp           ,ak_variadic_1{$ifdef fullVersion},'inspect;//Returs information on the current package#inspect(idOrPath:String);//Returns information on an imported package or a specified MNH source file#inspect(path:String,source:StringList);//Returns information of a package with given path and source'{$endif});
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'inspectAll'       ,@inspectAll_imp        ,ak_nullary{$ifdef fullVersion},'inspectAll;//Returs information on all currently loaded packages'{$endif});
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeAllDataStores',@writeAllDataStores_imp,ak_nullary{$ifdef fullVersion},'writeAllDataStores;//Writes back all modified datastores (including stores in imported packages) and returns true if any store was updated.'{$endif});
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeDataStores'   ,@writeDataStores_imp   ,ak_nullary{$ifdef fullVersion},'writeDataStores;//Writes back all modified datastores of the current package (i.e. excluding stores in imported packages) and returns true if any store was updated.'{$endif});
  builtinFunctionMap.registerRule(DEFAULT_BUILTIN_NAMESPACE,'helpOnMain'       ,@helpOnMain_imp        ,ak_nullary{$ifdef fullVersion},'helpOnMain;//Returns help text for the current package''s main method.'{$endif});
{$endif}
{$ifdef include_finalization}
{$endif}

