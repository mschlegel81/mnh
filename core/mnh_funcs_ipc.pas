UNIT mnh_funcs_ipc;
INTERFACE
USES sysutils, Classes, simpleipc, //RTL
     myGenerics,serializationUtil, //my tools
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_contexts,mnh_litVar,
     mnh_funcs;

PROCEDURE onPackageFinalization(CONST package:P_objectWithPath);
FUNCTION isServerRunning(CONST serverId:string):boolean;
IMPLEMENTATION
TYPE
  P_myIpcServer=^T_myIpcServer;
  T_myIpcServer=object
    serverId:string;
    feedbackLocation:T_tokenLocation;
    servingExpression:P_expressionLiteral;
    servingContext:P_threadContext;
    hasKillRequest:boolean;
    CONSTRUCTOR create(CONST serverId_:string; CONST location:T_tokenLocation; CONST expression:P_expressionLiteral; CONST context:P_threadContext);
    DESTRUCTOR destroy;
  end;

VAR checkingClient:TSimpleIPCClient=nil;
    registry:specialize G_instanceRegistry<P_myIpcServer>;

FUNCTION serverIsAssociatedWithPackage(s:P_myIpcServer; package:pointer):boolean;
  begin
    result:=s^.feedbackLocation.package=package;
    if result then s^.hasKillRequest:=true;
  end;

PROCEDURE onPackageFinalization(CONST package:P_objectWithPath);
  begin
    while registry.anyMatch(@serverIsAssociatedWithPackage,package) do sleep(1);
  end;

FUNCTION isServerRunning(CONST serverId:string):boolean;
  begin
    registry.enterCs;
    if not(Assigned(checkingClient)) then checkingClient:=TSimpleIPCClient.create(nil);
    checkingClient.serverId:=serverId;
    result:=checkingClient.ServerRunning;
    registry.leaveCs;
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
    registry.enterCs;
    result:=TSimpleIPCServer.create(nil);
    if serverId<>'' then result.serverId:=serverId
                    else result.serverId:=getNewServerId;
    result.Global:=true;
    result.StartServer;
    registry.leaveCs;
  end;

PROCEDURE disposeServer(VAR server:TSimpleIPCServer);
  begin
    server.StopServer;
    FreeAndNil(server);
  end;

PROCEDURE sendMessage(CONST senderServerId,receiverServerId:string; CONST statusOk:boolean; CONST payload:P_literal;
                      CONST location:T_tokenLocation; CONST adapters:P_adapters; VAR messageHash:T_hashInt);
  VAR streamWrapper:T_outputStreamWrapper;
      memoryStream:TMemoryStream;
      client:TSimpleIPCClient;
      sendStatusOk:boolean;
      serializationOk:boolean=true;
  begin
    if messageHash=0 then begin
      {$Q-}{$R-}
      messageHash:=T_hashInt(round(now*1000000));
      messageHash:=messageHash*31+T_hashInt(ThreadID);
      messageHash:=messageHash*31+T_hashInt(payload^.hash);
      messageHash:=messageHash*31+hashOfAnsiString(receiverServerId);
      {$Q+}{$R+}
      if messageHash=0 then messageHash:=1;
    end;

    client:=TSimpleIPCClient.create(nil);
    client.serverId:=receiverServerId;
    if not(client.ServerRunning) then begin
      if adapters<>nil then adapters^.raiseError('Cannot send IPC message to unreachable server: '+receiverServerId,location);
      client.free;
      exit;
    end;
    memoryStream:=TMemoryStream.create;
    streamWrapper.create(memoryStream);
    streamWrapper.writeAnsiString(senderServerId);
    streamWrapper.writeDWord(messageHash);
    sendStatusOk:=statusOk and ((adapters=nil) or (adapters^.noErrors)) and (payload<>nil);
    streamWrapper.writeBoolean(sendStatusOk);
    try
      if sendStatusOk then writeLiteralToStream(payload,@streamWrapper,location,adapters);
    except
      serializationOk:=false;
    end;
    if serializationOk then begin
      memoryStream.position:=0;
      if not(client.ServerRunning) then begin
        if adapters<>nil then adapters^.raiseError('Cannot send IPC message to unreachable server: '+receiverServerId,location);
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
                     CONST location:T_tokenLocation; CONST adapters:P_adapters):boolean;
  VAR streamWrapper:T_inputStreamWrapper;
      memoryStream:TMemoryStream;
  begin
    if not(receiver.PeekMessage(1,true)) then exit(false);
    memoryStream:=TMemoryStream.create;
    receiver.GetMessageData(memoryStream);
    streamWrapper.create(memoryStream);
    memoryStream.position:=0;
    senderId:=streamWrapper.readAnsiString;
    messageHash:=streamWrapper.readDWord;
    statusOk:=streamWrapper.readBoolean;
    if statusOk then payload:=newLiteralFromStream(@streamWrapper,location,adapters)
                else payload:=nil;
    streamWrapper.destroy;
    result:=true;
  end;

FUNCTION ipcServerThread(p:pointer):ptrint;
  VAR sleepTime:longint=0;
      //Caution: Server must be started and stopped in the same thread!
      server:TSimpleIPCServer;
      recentRequests:array[0..63] of T_hashInt;
      recentRequestOffset:byte;

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
        adaptersOrNil:P_adapters;
    begin with P_myIpcServer(p)^ do begin
      if servingContext=nil then adaptersOrNil:=nil
                            else adaptersOrNil:=servingContext^.adapters;
      //Even unique-instance-marker-servers should fetch messages from time to time
      if readMessage(server,request.senderId,request.messageHash,request.statusOk,request.payload,feedbackLocation,adaptersOrNil) and
         processThisRequest(request.messageHash) and
         (servingContext<>nil) and
         (servingExpression<>nil) then begin
        //execute:-----------------------------------------------
        response.senderId:=server.serverId;
        if request.statusOk then begin
          response.payload:=servingExpression^.evaluateToLiteral(feedbackLocation,servingContext,request.payload);
          response.statusOk:=servingContext^.adapters^.noErrors;
        end else begin
          if adaptersOrNil<>nil then adaptersOrNil^.raiseWarning('IPC server received request with error status - answering with error status',feedbackLocation);
          response.payload :=nil;
          response.statusOk:=false;
        end;
        if request.payload<>nil then disposeLiteral(request.payload);
        //------------------------------------------------:execute
        //respond:------------------------------------------------
        try
          sendMessage(response.senderId,request.senderId,response.statusOk,response.payload,feedbackLocation,nil,response.messageHash);
        finally
        end;
        //------------------------------------------------:respond
        result:=true;
      end else result:=false;
    end; end;

  begin
    with P_myIpcServer(p)^ do begin
      for recentRequestOffset:=0 to length(recentRequests)-1 do recentRequests[recentRequestOffset]:=0;
      recentRequestOffset:=0;
      if servingContext<>nil then servingContext^.adapters^.raiseNote('IPC server started. '+serverId,feedbackLocation);
      server:=newServer(serverId);
      while not(hasKillRequest) and ((servingContext=nil) or (servingContext^.adapters^.noErrors)) do begin
        if serve then sleepTime:=0
                 else begin
                   if sleepTime<100 then inc(sleepTime);
                   sleep(sleepTime);
                 end;
      end;
      disposeServer(server);
      if servingContext<>nil then servingContext^.adapters^.raiseNote('IPC server stopped. '+serverId,feedbackLocation);
    end;
    dispose(P_myIpcServer(p),destroy);
    result:=0;
  end;

CONSTRUCTOR T_myIpcServer.create(CONST serverId_:string; CONST location: T_tokenLocation; CONST expression: P_expressionLiteral; CONST context: P_threadContext);
  begin
    serverId:=serverId_;
    feedbackLocation:=location;
    servingExpression:=expression;
    servingContext:=context;
    registry.onCreation(@self);
    hasKillRequest:=false;
  end;

DESTRUCTOR T_myIpcServer.destroy;
  begin
    if servingContext<>nil then begin
      servingContext^.doneEvaluating;
      dispose(servingContext,destroy);
    end;
    if servingExpression<>nil then disposeLiteral(servingExpression);
    registry.onDestruction(@self);
  end;

{$i mnh_func_defines.inc}

FUNCTION assertUniqueInstance_impl intFuncSignature;
  VAR markerServer:P_myIpcServer;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      registry.enterCs;
      if isServerRunning(tokenLocation.package^.getPath)
      then context.adapters^.raiseError('There already is an instance of this script running',tokenLocation)
      else begin
        new(markerServer,create(tokenLocation.package^.getPath,tokenLocation,nil,nil));
        beginThread(@ipcServerThread,markerServer);
        result:=newVoidLiteral;
      end;
      registry.leaveCs;
    end;
  end;

FUNCTION startIpcServer_impl intFuncSignature;
  VAR ipcServer:P_myIpcServer;
      childContext:P_threadContext;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType=lt_expression) and (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) then begin
      registry.enterCs;
      if isServerRunning(str0^.value) then begin
        context.adapters^.raiseError('There already is an IPC server with ID "'+str0^.value+'" running',tokenLocation);
      end else begin
        childContext:=context.getNewAsyncContext;
        if childContext<>nil then begin
          new(ipcServer,create(str0^.value,tokenLocation,P_expressionLiteral(arg1^.rereferenced),childContext));
          beginThread(@ipcServerThread,ipcServer);
          result:=newVoidLiteral;
        end else context.adapters^.raiseError('startIpcServer is not allowed in this context because delegation is disabled.',tokenLocation);
      end;
      registry.leaveCs;
    end;
  end;

FUNCTION sendIpcRequest_impl intFuncSignature;
  VAR temporaryReceiver:TSimpleIPCServer;
      fetchedResult:boolean;
      aliveCheckCounter:longint=0;

      response:record
        senderId:string;
        statusOk:boolean;
        payload:P_literal;
      end;
      messageHash:T_hashInt=0;
      responseHash:T_hashInt;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType=lt_string) then begin
      temporaryReceiver:=newServer();
      sendMessage(temporaryReceiver.serverId,str0^.value,true,arg1,tokenLocation,context.adapters,messageHash);
      repeat
        fetchedResult:=readMessage(temporaryReceiver,response.senderId,responseHash,response.statusOk,response.payload,tokenLocation,context.adapters);
        if not(fetchedResult) then begin
          inc(aliveCheckCounter);
          if (aliveCheckCounter>100) then begin
            if not(isServerRunning(str0^.value))
            then context.adapters^.raiseError('IPC server "'+str0^.value+'" died before answering.',tokenLocation)
            else sendMessage(temporaryReceiver.serverId,str0^.value,true,arg1,tokenLocation,context.adapters,messageHash);
            aliveCheckCounter:=0;
          end else sleep(1);
        end;
      until fetchedResult or not(context.adapters^.noErrors);
      if fetchedResult and context.adapters^.noErrors then begin
        if response.payload<>nil then result:=response.payload
                                 else result:=newVoidLiteral;
      end;
      disposeServer(temporaryReceiver);
    end;
  end;

FUNCTION isIpcServerRunning_impl intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then
      result:=newBoolLiteral(isServerRunning(str0^.value));
  end;

INITIALIZATION
  registry.create;
  registerRule(IPC_NAMESPACE,'assertUniqueInstance',@assertUniqueInstance_impl,[se_writingInternal,se_server,se_detaching],ak_nullary,'assertUniqueInstance;//Returns with an error if there already is an instance of this script running.');
  registerRule(IPC_NAMESPACE,'startIpcServer'      ,@startIpcServer_impl      ,[se_writingInternal,se_server],ak_binary ,'startIpcServer(id:string,serve:expression(1));//Creates an IPC server');
  registerRule(IPC_NAMESPACE,'sendIpcRequest'      ,@sendIpcRequest_impl      ,[se_readingExternal,se_writingExternal],ak_binary ,'sendIpcRequest(serverId:string,request);//Delegates a given request to an IPC server');
  registerRule(IPC_NAMESPACE,'isIpcServerRunning'  ,@isIpcServerRunning_impl  ,[se_readingExternal],ak_unary  ,'isIpcServerRunning(serverId:string);//Returns true if the given IPC server is running and false otherwise');
FINALIZATION
  registry.destroy;
  if Assigned(checkingClient) then checkingClient.free;
end.
