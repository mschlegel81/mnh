UNIT mnh_funcs_ipc;
INTERFACE
USES sysutils, Classes, simpleipc, //RTL
     myGenerics,serializationUtil, //my tools
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_contexts,mnh_litVar,
     mnh_funcs;

TYPE
  T_ipcMessage=record
    respondTo:string;
    statusOk:boolean;
    body:P_literal;
  end;

  T_myIpcComunicator=object
    private
      feedbackLocation:T_tokenLocation;
      oneWayServer:TSimpleIPCServer;
      oneWayClient:TSimpleIPCClient;
      FUNCTION receive(CONST adapters:P_adapters):T_ipcMessage;
      PROCEDURE send(CONST receiver: string; VAR message: T_ipcMessage; CONST adapters:P_adapters);
    public
      CONSTRUCTOR create(CONST location:T_tokenLocation);
      DESTRUCTOR destroy;
  end;

  T_myIpcClient=object(T_myIpcComunicator)
    CONSTRUCTOR create(CONST location:T_tokenLocation);
    DESTRUCTOR destroy;
    FUNCTION ipcGet(CONST receiver: string; CONST request:P_literal; CONST adapters:P_adapters):P_literal;
  end;

  P_myIpcServer=^T_myIpcServer;
  T_myIpcServer=object(T_myIpcComunicator)
    servingExpression:P_expressionLiteral;
    servingContext:P_threadContext;
    hasKillRequest:boolean;
    CONSTRUCTOR create(CONST serverId:string; CONST location:T_tokenLocation; CONST expression:P_expressionLiteral; CONST context:P_threadContext);
    DESTRUCTOR destroy;
    FUNCTION serve:boolean;
  end;

PROCEDURE onPackageFinalization(CONST package:P_objectWithPath);
FUNCTION isServerRunning(CONST serverId:string):boolean;
IMPLEMENTATION
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
    checkingClient.ServerID:=serverId;
    result:=checkingClient.ServerRunning;
    registry.leaveCs;
  end;

FUNCTION ipcServerThread(p:pointer):ptrint;
  VAR sleepTime:longint=0;
  begin
    with P_myIpcServer(p)^ do while not(hasKillRequest) and ((servingContext=nil) or (servingContext^.adapters^.noErrors)) do begin
      if serve then sleepTime:=0
               else begin
                 if sleepTime<100 then inc(sleepTime);
                 sleep(sleepTime);
               end;
    end;
    dispose(P_myIpcServer(p),destroy);
    result:=0;
  end;

CONSTRUCTOR T_myIpcServer.create(CONST serverId:string; CONST location: T_tokenLocation; CONST expression: P_expressionLiteral; CONST context: P_threadContext);
  begin
    inherited create(location);
    oneWayServer:=TSimpleIPCServer.create(nil);
    oneWayServer.ServerID:=serverId;
    oneWayServer.Global:=true;
    oneWayServer.StartServer;
    servingExpression:=expression;
    servingContext:=context;
    registry.onCreation(@self);
    hasKillRequest:=false;
  end;

DESTRUCTOR T_myIpcServer.destroy;
  begin
    inherited destroy;
    if servingContext<>nil then begin
      servingContext^.doneEvaluating;
      dispose(servingContext,destroy);
    end;
    if servingExpression<>nil then disposeLiteral(servingExpression);
    registry.onDestruction(@self);
  end;

FUNCTION T_myIpcServer.serve:boolean;
  VAR hasRequest:boolean;
      message:T_ipcMessage;
      responseBody:P_literal;
  begin
    //fetch:-------------------------------------------------
    hasRequest:=oneWayServer.PeekMessage(1,true);
    if not(hasRequest) or (servingExpression=nil) then exit(false);
    //-------------------------------------------------:fetch
    //decode:------------------------------------------------
    message:=receive(servingContext^.adapters);
    //------------------------------------------------:decode
    //execute:-----------------------------------------------
    if message.statusOk then begin
      responseBody:=servingExpression^.evaluateToLiteral(feedbackLocation,servingContext,message.body);
      if message.body<>nil then disposeLiteral(message.body);
      message.body:=responseBody;
      message.statusOk:=servingContext^.adapters^.noErrors;
    end else begin
      servingContext^.adapters^.raiseWarning('IPC server received request with error status - answering with error status',feedbackLocation);
      if message.body<>nil then disposeLiteral(message.body);
    end;
    //------------------------------------------------:execute
    //respond:------------------------------------------------
    try
      send(message.respondTo,message,nil);
    finally
    end;
    //------------------------------------------------:respond
    result:=true;
  end;

CONSTRUCTOR T_myIpcClient.create(CONST location: T_tokenLocation);
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
    inherited create(location);
    oneWayServer:=TSimpleIPCServer.create(nil);
    oneWayServer.ServerID:=getNewServerId;
    oneWayServer.Global:=true;
    oneWayServer.StartServer;
    registry.leaveCs;
  end;

DESTRUCTOR T_myIpcClient.destroy;
begin
  inherited destroy;
end;

FUNCTION T_myIpcClient.ipcGet(CONST receiver: string; CONST request: P_literal; CONST adapters: P_adapters): P_literal;
  VAR message:T_ipcMessage;
      hasResponse:boolean=false;
      aliveCheckCounter:longint=0;
  begin
    //SEND:--------------------------------------------------
    message.respondTo:=oneWayServer.ServerID;
    message.statusOk:=(request<>nil) and (adapters^.noErrors);
    if message.statusOk then message.body:=request
                        else message.body:=nil;
    send(receiver,message,adapters);
    //--------------------------------------------------:SEND
    //RECEIVE: wait/fetch:-----------------------------------
    while (adapters^.noErrors) and not(hasResponse) do begin
      hasResponse:=oneWayServer.PeekMessage(1,true);
      if not(hasResponse) then begin
        inc(aliveCheckCounter);
        if (aliveCheckCounter>100) then begin
          if not(oneWayClient.ServerRunning) then adapters^.raiseError('IPC server "'+receiver+'" died before answering.',feedbackLocation);
        end else sleep(1);
      end;
    end;
    if not(hasResponse) then exit(nil);
    //--------------------------------------------:wait/fetch
    //decode:------------------------------------------------
    message:=receive(adapters);
    if message.statusOk then result:=message.body
    else begin
      if message.body<>nil then disposeLiteral(message.body);
      result:=nil;
      adapters^.raiseError('IPC get returned with errorstatus.',feedbackLocation);
    end;
    //---------------------------------------:decode :RECEIVE
  end;

DESTRUCTOR T_myIpcComunicator.destroy;
  begin
    if Assigned(oneWayServer) then begin
      if oneWayServer.Active then oneWayServer.StopServer;
      oneWayServer.free;
    end;
    if Assigned(oneWayClient) then
      oneWayClient.free;
  end;

PROCEDURE T_myIpcComunicator.send(CONST receiver: string; VAR message:T_ipcMessage; CONST adapters:P_adapters);
  VAR streamWrapper:T_outputStreamWrapper;
      memoryStream:TStringStream;
  begin
    if not(Assigned(oneWayClient)) then oneWayClient:=TSimpleIPCClient.create(nil);
    oneWayClient.ServerID:=receiver;
    if not(oneWayClient.ServerRunning) then begin
      if adapters<>nil then adapters^.raiseError('Cannot send IPC message to unreachable server: '+receiver,feedbackLocation);
      exit;
    end;
    memoryStream:=TStringStream.create('');
    streamWrapper.create(memoryStream);
    streamWrapper.writeAnsiString(message.respondTo);
    message.statusOk:=message.statusOk and ((adapters=nil) or (adapters^.noErrors)) and (message.body<>nil);
    streamWrapper.writeBoolean(message.statusOk);
    try
      if message.statusOk then writeLiteralToStream(message.body,@streamWrapper,feedbackLocation,adapters);
    except
      message.statusOk:=false;
    end;
    memoryStream.position:=0;
    oneWayClient.Active:=true;
    oneWayClient.SendStringMessage(memoryStream.DataString);
    oneWayClient.Active:=false;
    streamWrapper.destroy;
  end;


FUNCTION T_myIpcComunicator.receive(CONST adapters: P_adapters): T_ipcMessage;
  VAR streamWrapper:T_inputStreamWrapper;
      memoryStream:TStringStream;
  begin
    initialize(result);
    memoryStream:=TStringStream.create(oneWayServer.StringMessage);
    streamWrapper.create(memoryStream);
    memoryStream.position:=0;
    result.respondTo:=streamWrapper.readAnsiString;
    result.statusOk:=streamWrapper.readBoolean;
    if result.statusOk then result.body:=newLiteralFromStream(@streamWrapper,feedbackLocation,adapters)
                       else result.body:=nil;
    streamWrapper.destroy;
  end;

CONSTRUCTOR T_myIpcComunicator.create(CONST location: T_tokenLocation);
  begin
    feedbackLocation:=location;
    oneWayServer:=nil;
    oneWayClient:=nil;
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
  VAR ipcClient:T_myIpcClient;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       (arg0^.literalType=lt_string) then begin
      ipcClient.create(tokenLocation);
      result:=ipcClient.ipcGet(str0^.value,arg1,context.adapters);
      ipcClient.destroy;
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
  registerRule(IPC_NAMESPACE,'assertUniqueInstance',@assertUniqueInstance_impl,true,ak_nullary,'assertUniqueInstance;//Returns with an error if there already is an instance of this script running.');
  registerRule(IPC_NAMESPACE,'startIpcServer'      ,@startIpcServer_impl,false,ak_binary,'startIpcServer(id:string,serve:expression(1));//Creates an IPC server');
  registerRule(IPC_NAMESPACE,'sendIpcRequest'      ,@sendIpcRequest_impl,false,ak_binary,'sendIpcRequest(serverId:string,request);//Delegates a given request to an IPC server');
  registerRule(IPC_NAMESPACE,'isIpcServerRunning'  ,@isIpcServerRunning_impl,false,ak_unary,'isIpcServerRunning(serverId:string);//Returns true if the given IPC server is running and false otherwise');
FINALIZATION
  registry.destroy;
  if Assigned(checkingClient) then checkingClient.free;

end.
