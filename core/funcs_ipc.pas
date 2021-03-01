UNIT funcs_ipc;
INTERFACE
USES basicTypes;
IMPLEMENTATION
USES sysutils, Classes, simpleipc, //RTL
     myGenerics,serializationUtil,
     {$ifdef UNIX}strutils,myStringUtil,{$endif}
     mnh_constants,
     mnh_messages,
     out_adapters,
     litVar,
     contexts,
     tokenArray,
     recyclers,
     funcs;
TYPE
  P_myIpcServer=^T_myIpcServer;
  T_myIpcServer=object(T_detachedEvaluationPart)
    serverCs:TRTLCriticalSection;
    localRequest: P_literal;
    localResponse: P_literal;

    serverId:string;
    feedbackLocation:T_tokenLocation;
    servingExpressionOrNil:P_expressionLiteral;
    servingContextOrNil   :P_context;
    ipcMessageConnector   :P_messages;
    hasKillRequest:boolean;
    CONSTRUCTOR create(CONST serverId_:string; CONST location:T_tokenLocation; CONST expression:P_expressionLiteral; CONST context:P_context; CONST ipcMessageConnect:P_messages; CONST globals_:P_evaluationGlobals);
    DESTRUCTOR destroy; virtual;
    PROCEDURE stopOnFinalization; virtual;
    FUNCTION processLocally(CONST request:P_literal):P_literal;
  end;

VAR checkingClient:TSimpleIPCClient=nil;
    localServerCs:TRTLCriticalSection;
    localServers:specialize G_stringKeyMap<P_myIpcServer>;

FUNCTION serverIsAssociatedWithPackage(s:P_myIpcServer; package:pointer):boolean;
  begin
    result:=s^.feedbackLocation.package=package;
    if result then s^.hasKillRequest:=true;
  end;

FUNCTION cleanPath(CONST s:string):string;
  begin
    {$ifdef UNIX}
    result:=cleanString(replaceOne(ansiReplaceStr('$'+s,'/','_'),'$_',''),['a'..'z','A'..'Z','0'..'9','_'],'_');
    {$else}
    result:=s;
    {$endif}
  end;

FUNCTION isServerRunning(CONST serverId:string):boolean;
  begin
    enterCriticalSection(localServerCs);
    if localServers.containsKey(serverId)
    then result:=true
    else begin
      if not(Assigned(checkingClient)) then checkingClient:=TSimpleIPCClient.create(nil);
      checkingClient.serverId:=cleanPath(serverId);
      result:=checkingClient.ServerRunning;
    end;
    leaveCriticalSection(localServerCs);
  end;

FUNCTION newServer(CONST serverId:string=''):TSimpleIPCServer;
  FUNCTION getNewServerId:string;
    CONST millisecondsPerDay=24*60*60*1000;
    VAR intResult:int64;
        threadString:string;
    begin
      threadString:=IntToHex(ThreadID,16);
      intResult:=round(now*millisecondsPerDay);
      result:=threadString+IntToHex(intResult,12);
      while isServerRunning(result) do begin
        inc(intResult);
        result:=threadString+IntToHex(intResult,12);
      end;
    end;

  begin
    result:=TSimpleIPCServer.create(nil);
    if serverId<>'' then result.serverId:=cleanPath(serverId)
                    else result.serverId:=getNewServerId;
    result.global:=true;
    result.StartServer;
  end;

PROCEDURE disposeServer(VAR server:TSimpleIPCServer);
  begin
    try
      server.StopServer;
    finally
      FreeAndNil(server);
    end;
  end;

VAR messageHashTally:T_hashInt=0;
PROCEDURE sendMessage(senderServerId,receiverServerId:string; CONST statusOk:boolean; CONST payload:P_literal;
                      CONST location:T_tokenLocation; CONST adapters:P_messages; VAR messageHash:T_hashInt);
  VAR streamWrapper:T_outputStreamWrapper;
      memoryStream:TMemoryStream;
      client:TSimpleIPCClient;
      sendStatusOk:boolean;
      serializationOk:boolean=true;
  begin
    senderServerId  :=cleanPath(senderServerId);
    receiverServerId:=cleanPath(receiverServerId);
    if messageHash=0 then begin
      {$Q-}{$R-}
      interLockedIncrement(messageHashTally);
      messageHash:=messageHashTally;
      messageHash:=messageHash*31+T_hashInt(round(now*86400000));
      messageHash:=messageHash*31+T_hashInt(ThreadID);
      messageHash:=messageHash*31+T_hashInt(payload^.hash);
      messageHash:=messageHash*31+hashOfAnsiString(receiverServerId);
      {$Q+}{$R+}
      if messageHash=0 then messageHash:=1;
    end;

    client:=TSimpleIPCClient.create(nil);
    client.serverId:=receiverServerId;
    if not(client.ServerRunning) then begin
      if adapters<>nil then adapters^.raiseSimpleError('Cannot send IPC message to unreachable server: '+receiverServerId,location);
      client.free;
      exit;
    end;
    memoryStream:=TMemoryStream.create;
    streamWrapper.create(memoryStream);
    streamWrapper.writeAnsiString(senderServerId);
    streamWrapper.writeDWord(messageHash);
    sendStatusOk:=statusOk and ((adapters=nil) or (adapters^.continueEvaluation)) and (payload<>nil);
    streamWrapper.writeBoolean(sendStatusOk);
    try
      if sendStatusOk then writeLiteralToStream(payload,@streamWrapper,location,adapters);
    except
      serializationOk:=false;
    end;
    if serializationOk then begin
      memoryStream.position:=0;
      if not(client.ServerRunning) then begin
        if adapters<>nil then adapters^.raiseSimpleError('Cannot send IPC message to unreachable server: '+receiverServerId,location);
        client.free;
        exit;
      end;
      client.active:=true;
      client.sendMessage(0,memoryStream);
    end;
    client.free;
    streamWrapper.destroy;
  end;

FUNCTION readMessage(VAR receiver:TSimpleIPCServer;
                     OUT senderId:string;
                     OUT messageHash:T_hashInt;
                     OUT statusOk:boolean;
                     OUT payload:P_literal;
                     CONST location:T_tokenLocation; CONST adapters:P_messages):boolean;
  VAR streamWrapper:T_inputStreamWrapper;
      memoryStream:TMemoryStream;
      typeMap:T_typeMap;
  begin
    if not(receiver.PeekMessage(0,true)) then begin
      payload:=nil;
      exit(false);
    end;
    memoryStream:=TMemoryStream.create;
    receiver.GetMessageData(memoryStream);
    streamWrapper.create(memoryStream);
    memoryStream.position:=0;
    senderId:=streamWrapper.readAnsiString;
    messageHash:=streamWrapper.readDWord;
    statusOk:=streamWrapper.readBoolean;
    typeMap:=P_abstractPackage(location.package)^.getTypeMap;
    if statusOk then payload:=newLiteralFromStream(@streamWrapper,location,adapters,typeMap)
                else payload:=nil;
    typeMap.destroy;
    streamWrapper.destroy;
    result:=true;
  end;

FUNCTION ipcServerThread(p:pointer):ptrint;
  VAR sleepTime:longint=0;
      //Caution: Server must be started and stopped in the same thread!
      server:TSimpleIPCServer;
      recentRequests:array[0..63] of T_hashInt;
      recentRequestOffset:byte;
      recycler:T_recycler;

  FUNCTION processThisRequest(CONST hash:T_hashInt):boolean;
    VAR i:longint;
    begin
      for i:=0 to length(recentRequests)-1 do if recentRequests[i]=hash then exit(false);
      result:=true;
      recentRequests[recentRequestOffset]:=hash;
      recentRequestOffset:=(recentRequestOffset+1) and 63;
    end;

  FUNCTION serve:boolean;
    VAR request,response:record
          senderId:string;
          statusOk:boolean;
          payload:P_literal;
          messageHash:T_hashInt;
        end;
    begin with P_myIpcServer(p)^ do begin
      if (servingContextOrNil=nil) or (servingExpressionOrNil=nil) then exit(false);
      result:=false;
      request.payload:=nil;
      enterCriticalSection(serverCs);
      try
        if (localRequest<>nil) and (localResponse=nil) then begin
          localResponse:=servingExpressionOrNil^.evaluateToLiteral(feedbackLocation,servingContextOrNil,@recycler,localRequest,nil).literal;
          result:=true;
        end;
      finally
        leaveCriticalSection(serverCs);
      end;
      if result then exit(true);

      //Even unique-instance-marker-servers should fetch messages from time to time
      if readMessage(server,request.senderId,request.messageHash,request.statusOk,request.payload,feedbackLocation,@servingContextOrNil^.messages) and
         processThisRequest(request.messageHash)then begin
        //execute:-----------------------------------------------
        response.senderId:=server.serverId;
        if request.statusOk then begin
          response.payload:=servingExpressionOrNil^.evaluateToLiteral(feedbackLocation,servingContextOrNil,@recycler,request.payload,nil).literal;
          response.statusOk:=servingContextOrNil^.messages^.continueEvaluation;
        end else begin
          servingContextOrNil^.messages^.postTextMessage(mt_el2_warning,feedbackLocation,'IPC server received request with error status - answering with error status');
          response.payload :=nil;
          response.statusOk:=false;
        end;
        if request.payload<>nil then disposeLiteral(request.payload);
        //------------------------------------------------:execute
        //respond:------------------------------------------------
        try
          sendMessage(response.senderId,request.senderId,response.statusOk,response.payload,feedbackLocation,nil,response.messageHash);
          if response.payload<>nil then disposeLiteral(response.payload);
        finally
        end;
        //------------------------------------------------:respond
        result:=true;
      end else begin
        if request.payload<>nil then disposeLiteral(request.payload);
        result:=false;
      end;
    end; end;
  VAR serverCreated:boolean=false;
  begin
    recycler.initRecycler;
    with P_myIpcServer(p)^ do begin
      for recentRequestOffset:=0 to length(recentRequests)-1 do recentRequests[recentRequestOffset]:=0;
      recentRequestOffset:=0;
      try
        server:=newServer(serverId);
        if servingContextOrNil<>nil then servingContextOrNil^.messages^.postTextMessage(mt_el1_note,feedbackLocation,'IPC server started. '+serverId);
        serverCreated:=true;
      except on e:Exception do
        begin
          ipcMessageConnector^.raiseSimpleError(e.message,feedbackLocation,mt_el4_systemError);
          serverCreated:=false;
          hasKillRequest:=true;
        end;
      end;
      while not(hasKillRequest) and (ipcMessageConnector^.continueEvaluation) do begin
        if serve then sleepTime:=0
                 else begin
                   if sleepTime<500 then inc(sleepTime);
                   sleep(sleepTime shr 2);
                 end;
      end;
      if serverCreated then try
        disposeServer(server);
      except on e:Exception do
        ipcMessageConnector^.raiseSimpleError(e.message,feedbackLocation,mt_el4_systemError);
      end;
      if servingContextOrNil<>nil then servingContextOrNil^.messages^.postTextMessage(mt_el1_note,feedbackLocation,'IPC server stopped. '+serverId);
    end;
    dispose(P_myIpcServer(p),destroy);
    result:=0;
    recycler.cleanup;
  end;

CONSTRUCTOR T_myIpcServer.create(CONST serverId_:string; CONST location: T_tokenLocation; CONST expression: P_expressionLiteral; CONST context: P_context; CONST ipcMessageConnect:P_messages; CONST globals_:P_evaluationGlobals);
  begin
    inherited create(globals_,location);
    initCriticalSection(serverCs);
    serverId:=serverId_;
    feedbackLocation:=location;
    servingExpressionOrNil:=expression;
    servingContextOrNil:=context;
    ipcMessageConnector:=ipcMessageConnect;
    enterCriticalSection(localServerCs);
    localServers.put(serverId,@self);
    leaveCriticalSection(localServerCs);
    hasKillRequest:=false;
    localRequest:=nil;
    localResponse:=nil;
  end;

DESTRUCTOR T_myIpcServer.destroy;
  VAR recycler:T_recycler;
  begin
    enterCriticalSection(serverCs);
    try
      recycler.initRecycler;
      if servingContextOrNil<>nil then begin
        servingContextOrNil^.finalizeTaskAndDetachFromParent(nil);
        contextPool.disposeContext(servingContextOrNil);
      end;
      if servingExpressionOrNil<>nil then disposeLiteral(servingExpressionOrNil);
      enterCriticalSection(localServerCs);
      localServers.dropKey(serverId);
      leaveCriticalSection(localServerCs);
    finally
      leaveCriticalSection(serverCs);
      doneCriticalSection(serverCs);
      recycler.cleanup;
      inherited destroy;
    end;
  end;

PROCEDURE T_myIpcServer.stopOnFinalization;
  begin
    hasKillRequest:=true;
  end;

FUNCTION T_myIpcServer.processLocally(CONST request:P_literal):P_literal;
  FUNCTION aliveAndWell:boolean; inline;
    begin
      result:=not(hasKillRequest) and
              (servingContextOrNil<>nil) and
              (servingContextOrNil^.continueEvaluation);
    end;

  begin
    enterCriticalSection(serverCs);
    while aliveAndWell and (localRequest<>nil) do begin
      leaveCriticalSection(serverCs);
      ThreadSwitch;
      enterCriticalSection(serverCs);
    end;
    if aliveAndWell then begin
      localRequest:=request;
      leaveCriticalSection(serverCs);
      while aliveAndWell and (localResponse=nil) do begin
        leaveCriticalSection(serverCs);
        ThreadSwitch;
        enterCriticalSection(serverCs);
      end;
      result:=localResponse;
      localResponse:=nil;
      localRequest :=nil;
      leaveCriticalSection(serverCs);
    end else exit(nil);
  end;

{$i func_defines.inc}

FUNCTION assertUniqueInstance_impl intFuncSignature;
  VAR markerServer:P_myIpcServer;
      normalizedPath:string;
  begin
    result:=nil;
    if ((params=nil) or (params^.size=0)) and context.checkSideEffects('assertUniqueInstance',tokenLocation,[se_alterContextState,se_accessIpc,se_server,se_detaching]) then begin
      enterCriticalSection(localServerCs);
      try
        normalizedPath:=cleanPath(expandFileName(tokenLocation.package^.getPath));
        if localServers.containsKey(normalizedPath,markerServer) then begin
          if (markerServer^.feedbackLocation.package=tokenLocation.package)
          then result:=newVoidLiteral
          else context.messages^.raiseSimpleError('There already is an instance of this script running',tokenLocation);
        end else begin
          if isServerRunning(normalizedPath)
          then context.messages^.raiseSimpleError('There already is an instance of this script running',tokenLocation)
          else begin
            new(markerServer,create(normalizedPath,tokenLocation,nil,nil,context.messages,context.getGlobals));
            beginThread(@ipcServerThread,markerServer);
            result:=newVoidLiteral;
          end;
        end;
      except on e:Exception do
        context.messages^.raiseSimpleError(e.message,tokenLocation,mt_el4_systemError);
      end;
      leaveCriticalSection(localServerCs);
    end;
  end;

FUNCTION startIpcServer_impl intFuncSignature;
  VAR ipcServer:P_myIpcServer;
      childContext:P_context;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and context.checkSideEffects('startIpcServer',tokenLocation,[se_alterContextState,se_server,se_detaching])  and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      if isServerRunning(str0^.value) then begin
        context.raiseError('There already is an IPC server with ID "'+str0^.value+'" running',tokenLocation);
      end else begin
        childContext:=context.getNewAsyncContext(recycler,false);
        if childContext<>nil then begin
          new(ipcServer,create(str0^.value,tokenLocation,P_expressionLiteral(arg1^.rereferenced),childContext,childContext^.messages,context.getGlobals));
          beginThread(@ipcServerThread,ipcServer);
          result:=newVoidLiteral;
        end else context.raiseError('startIpcServer is not allowed in this context because delegation is disabled.',tokenLocation);
      end;
    end;
  end;

FUNCTION sendIpcRequest_impl intFuncSignature;
  VAR temporaryReceiver:TSimpleIPCServer;
      fetchedResult:boolean;
      aliveCheckCounter:longint=0;
      localServer:P_myIpcServer;
      isLocal:boolean;
      response:record
        senderId:string;
        statusOk:boolean;
        payload:P_literal;
      end;
      messageHash:T_hashInt=0;
      responseHash:T_hashInt;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and context.checkSideEffects('sendIpcRequest',tokenLocation,[se_accessIpc]) and
       (arg0^.literalType=lt_string) then begin
      enterCriticalSection(localServerCs);
      isLocal:=localServers.containsKey(str0^.value,localServer);
      leaveCriticalSection(localServerCs);
      if isLocal
      then result:=localServer^.processLocally(arg1)
      else begin
        temporaryReceiver:=newServer();
        sendMessage(temporaryReceiver.serverId,str0^.value,true,arg1,tokenLocation,context.messages,messageHash);
        repeat
          fetchedResult:=readMessage(temporaryReceiver,response.senderId,responseHash,response.statusOk,response.payload,tokenLocation,context.messages);
          if not(fetchedResult) then begin
            inc(aliveCheckCounter);
            if (aliveCheckCounter>100) then begin
              if not(isServerRunning(str0^.value))
              then context.raiseError('IPC server "'+str0^.value+'" died before answering.',tokenLocation)
              else sendMessage(temporaryReceiver.serverId,str0^.value,true,arg1,tokenLocation,context.messages,messageHash);
              aliveCheckCounter:=0;
            end else sleep(1);
          end;
        until fetchedResult or not(context.messages^.continueEvaluation);
        if fetchedResult and context.messages^.continueEvaluation then begin
          if response.payload<>nil then result:=response.payload
                                   else result:=newVoidLiteral;
        end;
        disposeServer(temporaryReceiver);
      end;
    end;
  end;

FUNCTION isIpcServerRunning_impl intFuncSignature;
  begin
    if not(context.checkSideEffects('isIpcServerRunning',tokenLocation,[se_accessIpc])) then exit(nil);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then
      result:=newBoolLiteral(isServerRunning(str0^.value))
    else if (params=nil) or (params^.size=0) then
      result:=newBoolLiteral(isServerRunning(expandFileName(tokenLocation.package^.getPath)));
  end;

INITIALIZATION
  initialize(localServerCs);
  initCriticalSection(localServerCs);
  localServers.create();
  builtinFunctionMap.registerRule(IPC_NAMESPACE,'assertUniqueInstance',@assertUniqueInstance_impl,ak_nullary   {$ifdef fullVersion},'assertUniqueInstance;//Returns with an error if there already is an instance of this script running.'{$endif});
  builtinFunctionMap.registerRule(IPC_NAMESPACE,'startIpcServer'      ,@startIpcServer_impl      ,ak_binary    {$ifdef fullVersion},'startIpcServer(id:String,serve:Expression(1));//Creates an IPC server'{$endif});
  builtinFunctionMap.registerRule(IPC_NAMESPACE,'sendIpcRequest'      ,@sendIpcRequest_impl      ,ak_binary    {$ifdef fullVersion},'sendIpcRequest(serverId:String,request);//Delegates a given request to an IPC server'{$endif});
  builtinFunctionMap.registerRule(IPC_NAMESPACE,'isIpcServerRunning'  ,@isIpcServerRunning_impl  ,ak_variadic_1{$ifdef fullVersion},'isIpcServerRunning(serverId:String);//Returns true if the given IPC server is running and false otherwise#isIpcServerRunning;//Returns true if this script is already running and called assertUniqueInstance'{$endif});
FINALIZATION
  doneCriticalSection(localServerCs);
  localServers.destroy;
  if Assigned(checkingClient) then checkingClient.free;
end.
