UNIT funcs_server;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,math,fphttpclient,lclintf,
     Classes,
     synautil,
     myStringUtil,myGenerics,httpUtil,
     basicTypes,mnh_constants,
     mnh_messages,
     out_adapters,
     litVar,
     contexts,
     recyclers,
     funcs,
     synsock;

IMPLEMENTATION
USES strutils,mySys;
TYPE
  T_microserver=class(T_detachedEvaluationPart)
    protected
      serverCs:TRTLCriticalSection;
      ip:ansistring;
      timeout:double;
      servingExpression:P_expressionLiteral;
      feedbackLocation:T_tokenLocation;
      up:boolean;
      context:P_context;
      httpListener:T_httpListener;

      log:record
        creationTime,serveTime,execTime:double;
        requestCount:longint;
      end;
      PROCEDURE execute; override;
    public
      CONSTRUCTOR create(CONST ip_:string; CONST servingExpression_:P_expressionLiteral; CONST timeout_:double; CONST feedbackLocation_:T_tokenLocation; CONST context_: P_context);
      DESTRUCTOR destroy; override;
      PROCEDURE logExecution(CONST totalTime,execTime:double);
  end;

  T_microserverRequest=class(T_detachedEvaluationPart)
    protected
      connection:T_httpConnectionForRequest;
      context:P_context;
      servingExpression:P_expressionLiteral;
      feedbackLocation:T_tokenLocation;
      recycler:P_recycler;
      myParent:T_microserver;
      creationTime:double;
      PROCEDURE execute; override;
    public
      CONSTRUCTOR createMicroserverRequest(CONST conn:TSocket; CONST parent:T_microserver);
      DESTRUCTOR destroy; override;
  end;

  T_httpMethod=(hm_get,hm_put,hm_post,hm_delete);

VAR localServerCs:TRTLCriticalSection;
    localServers:specialize G_stringKeyMap<T_microserver>;

{$i func_defines.inc}

FUNCTION wrapTextInHttp_impl intFuncSignature;
  CONST serverInfo='MNH5 via Synapse';
  VAR header:T_httpHeader;
      i:longint=0;
      iter:T_arrayOfKeyValuePair;
      invalidHeader:boolean=false;
  begin
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      if  params^.size=1                                    then exit(recycler^.newStringLiteral(wrapTextInHttp(str0^.value,serverInfo)));
      if (params^.size=2) and (arg1^.literalType=lt_string) then exit(recycler^.newStringLiteral(wrapTextInHttp(str0^.value,serverInfo,str1^.value)));
      if (params^.size=3) and (arg1^.literalType=lt_smallint) and (arg2^.literalType in [lt_map,lt_emptyMap]) then begin
        iter:=map2^.entryList;
        setLength(header,length(iter));
        for i:=0 to length(iter)-1 do with iter[i] do begin
          if key  ^.literalType=lt_string then header[i].key  :=P_stringLiteral(key  )^.value else invalidHeader:=true;
          if value^.literalType=lt_string then header[i].value:=P_stringLiteral(value)^.value else header[i].value:=value^.toString();
          key^.unreference;
          value^.unreference;
        end;
        setHeaderDefaults(header,length(str0^.value));
        if invalidHeader then begin
          context^.raiseError('Invalid header map; all keys must be strings',tokenLocation);
          exit(nil);
        end else exit(recycler^.newStringLiteral(wrapTextInHttp(str0^.value,P_smallIntLiteral(arg1)^.value,header)));
      end;
      result:=nil;
    end else result:=nil;
  end;

FUNCTION httpError_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_smallint)
    then result:=recycler^.newStringLiteral('HTTP/1.0 '+arg0^.toString+C_carriageReturnChar+C_lineBreakChar)
    else if (params=nil) or (params^.size=0)
    then result:=recycler^.newStringLiteral('HTTP/1.0 404'+C_carriageReturnChar+C_lineBreakChar)
    else result:=nil;
  end;

FUNCTION startServer_impl intFuncSignature;
  VAR microserver:T_microserver;
      timeout:double;
      servingExpression:P_expressionLiteral;
      childContext:P_context;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType=lt_expression) and
       (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) and
       (arg2^.literalType in [lt_smallint,lt_bigint,lt_real])  and
       context^.checkSideEffects('startHttpServer',tokenLocation,[se_alterContextState,se_server,se_detaching]) then begin

      timeout:=P_numericLiteral(arg2)^.floatValue/(24*60*60);
      servingExpression:=P_expressionLiteral(arg1);
      servingExpression^.rereference;
      childContext:=context^.getNewAsyncContext(recycler,false);
      if childContext<>nil then begin
        microserver:=T_microserver.create(str0^.value,servingExpression,timeout,tokenLocation,childContext);
        if microserver.httpListener.getLastListenerSocketError=0
        then microserver.start
        else begin
          context^.raiseError('Error in socket creation ('+microserver.ip+')',tokenLocation);
          FreeAndNil(microserver);
          exit(nil);
        end;
        result:=recycler^.newStringLiteral(microserver.ip);
      end else context^.raiseError('startServer is not allowed in this context because delegation is disabled.',tokenLocation);
    end;
  end;

CONSTRUCTOR T_microserverRequest.createMicroserverRequest(CONST conn: TSocket; CONST parent: T_microserver);
  begin
    creationTime:=parent.context^.wallclockTime;
    servingExpression:=P_expressionLiteral(parent.servingExpression^.rereferenced);
    feedbackLocation:=parent.feedbackLocation;
    connection.create(conn,@(parent.httpListener));
    recycler:=newRecycler;
    context:=parent.context^.getNewAsyncContext(recycler,true);
    myParent:=parent;
    FreeOnTerminate:=true;
    inherited create(parent.context^.getGlobals,feedbackLocation,false);
  end;

PROCEDURE T_microserverRequest.execute;
  VAR requestMap:P_mapLiteral;

  PROCEDURE fillRequestLiteral;
    VAR headerMap:P_mapLiteral;
        i:longint;
    begin
      headerMap:=recycler^.newMapLiteral(8);
      requestMap:=recycler^.newMapLiteral(8)
        ^.put(recycler,'request',
          recycler^.newMapLiteral(3)^.put(recycler,'method',C_httpRequestMethodName[connection.getMethod])
                          ^.put(recycler,'path',connection.getRequest)
                          ^.put(recycler,'protocol',connection.getProtocol),false)
        ^.put(recycler,'header',headerMap,false)
        ^.put(recycler,'body',connection.getBody);
      for i:=0 to length(connection.getHeader)-1 do headerMap^.put(recycler,connection.getHeader[i].key,connection.getHeader[i].value);
    end;

  VAR response:P_literal;
      executionTime:double;
  begin
    fillRequestLiteral;
    executionTime:=context^.wallclockTime;
    response:=evaluteExpression(servingExpression,feedbackLocation,context,recycler,requestMap).literal;
    executionTime:=context^.wallclockTime-executionTime;
    recycler^.disposeLiteral(requestMap);
    if (response<>nil) then begin
      if response^.literalType=lt_string
      then connection.sendStringAndClose(P_stringLiteral(response)^.value)
      else connection.sendStringAndClose(response^.toString);
      recycler^.disposeLiteral(response);
    end else begin
      context^.messages^.postTextMessage(mt_el2_warning,feedbackLocation,'Microserver response is nil!');
      connection.sendStringAndClose(HTTP_404_RESPONSE);
    end;
    context^.finalizeTaskAndDetachFromParent(recycler);
    contextPool.disposeContext(context);
    myParent.logExecution(myParent.context^.wallclockTime-creationTime,executionTime);
    connection.destroy;
    Terminate;
  end;

DESTRUCTOR T_microserverRequest.destroy;
  begin
    recycler^.disposeLiteral(servingExpression);
    freeRecycler(recycler);
    inherited destroy;
  end;

CONSTRUCTOR T_microserver.create(CONST ip_: string; CONST servingExpression_: P_expressionLiteral; CONST timeout_: double; CONST feedbackLocation_: T_tokenLocation; CONST context_: P_context);
  begin
    inherited create(context_^.getGlobals,feedbackLocation_,true);
    with log do begin
      creationTime:=context_^.wallclockTime;
      execTime:=0;
      serveTime:=0;
      requestCount:=0;
    end;
    initCriticalSection(serverCs);
    ip:=cleanIp(ip_);
    if isNan(timeout_) or isInfinite(timeout_) or (timeout_<0)
    then timeout:=0
    else timeout:=timeout_;
    servingExpression:=servingExpression_;
    feedbackLocation:=feedbackLocation_;
    context:=context_;
    httpListener.create(ip,GLOBAL_THREAD_LIMIT);
    enterCriticalSection(localServerCs);
    localServers.put(ip,self);
    leaveCriticalSection(localServerCs);
  end;

DESTRUCTOR T_microserver.destroy;
  VAR recycler:P_recycler;
  begin
    recycler:=newRecycler;
    recycler^.disposeLiteral(servingExpression);
    context^.finalizeTaskAndDetachFromParent(recycler);
    contextPool.disposeContext(context);
    httpListener.destroy;
    freeRecycler(recycler);
    doneCriticalSection(serverCs);
    enterCriticalSection(localServerCs);
    localServers.dropKey(ip);
    leaveCriticalSection(localServerCs);
    inherited destroy;
  end;

PROCEDURE T_microserver.execute;
  VAR lastActivity:double;
  FUNCTION timedOut:boolean;
    begin
      result:=(timeout> 0) and (now-lastActivity>timeout);
    end;

  PROCEDURE postShutdownMessage;
    VAR msg:T_arrayOfString;
    begin
      msg:='http Microserver stopped. '+httpListener.toString;
      enterCriticalSection(serverCs);
      with log do begin
        append(msg,'Uptime           : '+myTimeToStr((context^.wallclockTime-creationTime)/(24*60*60)));
        append(msg,'                   '+myFloatToStr((context^.wallclockTime-creationTime)*1000)+'ms');
        append(msg,'Request count    : '+intToStr(requestCount));
        append(msg,'Avg. request time: '+myFloatToStr(1000*serveTime/requestCount)+'ms');
        append(msg,'Avg. exec time   : '+myFloatToStr(1000*execTime/requestCount)+'ms');
      end;
      leaveCriticalSection(serverCs);
      context^.messages^.postTextMessage(mt_el1_note,feedbackLocation,msg);
    end;

  CONST minSleepTime=0;
        maxSleepTime=50;
  VAR sleepTime:longint=minSleepTime;
      requestSocket:TSocket;
  begin
    FreeOnTerminate:=true;
    context^.messages^.postTextMessage(mt_el1_note,feedbackLocation,'http Microserver started. '+httpListener.toString);
    up:=true;
    lastActivity:=now;
    threadStartsSleeping;
    repeat
      requestSocket:=httpListener.getRawRequestSocket(sleepTime);
      if requestSocket<>INVALID_SOCKET then begin
        {$ifdef debugMode}
        writeln(stdErr,'Recieved HTTP request at ',ip,'; globalRunningThreads: ',getGlobalRunningThreads,'; memory in comfort zone: ',memoryCleaner.isMemoryInComfortZone);
        {$endif}

        //Protect against memory over-use by request bombing
        while not(memoryCleaner.isMemoryInComfortZone) or
              (getGlobalRunningThreads>=GLOBAL_THREAD_LIMIT) do sleep(10);
        sleepTime:=minSleepTime;
        lastActivity:=now;
        threadStopsSleeping;
        T_microserverRequest.createMicroserverRequest(requestSocket,self);
        threadStartsSleeping;
      end else begin
        inc(sleepTime);
        if sleepTime>maxSleepTime then sleepTime:=maxSleepTime;
      end;
    until timedOut or Terminated or not(context^.messages^.continueEvaluation);
    postShutdownMessage;
    up:=false;
    threadStopsSleeping;
    Terminate;
  end;

PROCEDURE T_microserver.logExecution(CONST totalTime,execTime:double);
  begin
    enterCriticalSection(serverCs);
    inc(log.requestCount);
    log.serveTime+=totalTime;
    log.execTime +=execTime;
    leaveCriticalSection(serverCs);
  end;

FUNCTION extractParameters_impl intFuncSignature;
  VAR parameters:P_mapLiteral;
  PROCEDURE addParameterPair(CONST pair:string);

    VAR keyAndValue:T_arrayOfString;
        i:longint;
        value:P_stringLiteral;
        castValue:P_literal;
    begin
      keyAndValue:=split(pair,'=');
      while length(keyAndValue)<2 do append(keyAndValue,'');
      for i:=0 to length(keyAndValue)-1 do begin
        keyAndValue[i]:=percentDecode(ansiReplaceStr(keyAndValue[i],'+',' '));
      end;
      value:=recycler^.newStringLiteral(keyAndValue[1]);
      castValue:=value^.softCast(recycler);
      recycler^.disposeLiteral(value);
      parameters^.put(recycler,keyAndValue[0],castValue,false);
    end;

  VAR parts:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      parts:=split(str0^.value,'?');
      while (length(parts)<2) do append(parts,'');
      parameters:=recycler^.newMapLiteral(0);
      if length(parts[1])>0 then begin
        parts:=split(parts[1],'&');
        for i:=0 to length(parts)-1 do addParameterPair(parts[i]);
      end;
      exit(parameters);
    end;
  end;

FUNCTION extractRawParameters_impl intFuncSignature;
  VAR parts:T_arrayOfString;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      parts:=split(str0^.value,'?');
      if length(parts)>1
      then exit(recycler^.newStringLiteral(parts[1]))
      else exit(recycler^.newStringLiteral(''));
    end;
  end;

FUNCTION extractPath_impl intFuncSignature;
  VAR parts:T_arrayOfString;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      parts:=split(str0^.value,'?');
      if length(parts)>0
      then exit(recycler^.newStringLiteral(parts[0]))
      else exit(recycler^.newStringLiteral(''));
    end;
  end;

FUNCTION encodeRequest_impl intFuncSignature;
  FUNCTION getString(CONST L:P_literal):string;
    begin
      if L^.literalType=lt_string then result:=P_stringLiteral(L)^.value
                                  else result:=L^.toString();
    end;

  VAR address:string='';
      path:string='';
      parameters:string='';
      i:longint;
      iter:T_arrayOfKeyValuePair;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string) and (arg2^.literalType in [lt_emptyList,lt_map,lt_string,lt_emptyMap]) then begin
      address:=str0^.value;
      path:=str1^.value;
      if not(startsWith(path,'/')) then path:='/'+path;
      case arg2^.literalType of
        lt_string: parameters:=percentEncode(str2^.value);
        lt_map: begin
          iter:=map2^.entryList;
          for i:=0 to length(iter)-1 do begin
            if i>0 then parameters:=parameters+'&';
            parameters:=parameters+percentEncode(getString(iter[i].key))
                              +'='+percentEncode(getString(iter[i].value));
            iter[i].key  ^.unreference;
            iter[i].value^.unreference;
          end;
        end;
      end;
      if parameters<>'' then parameters:='?'+parameters;
      result:=recycler^.newStringLiteral(address+path+parameters);
    end;
  end;

VAR httpRequestCs:TRTLCriticalSection;

FUNCTION httpGetPutPost(CONST method:T_httpMethod; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  CONST methodName:array[T_httpMethod] of string=('httpGet','httpPut','httpPost','httpDelete');
  VAR resultText:ansistring='';
      requestText:ansistring='';
      requestStream:TStringStream=nil;
      requestHeader:array of record key,value:string; end;
      client:TFPHTTPClient;
      k:longint;
      allOkay:boolean=true;

  PROCEDURE constructRequestHeader(CONST input:P_mapLiteral);
    VAR entry:T_keyValuePair;
        k:longint=0;
    begin
      setLength(requestHeader,input^.size);
      for entry in input^.entryList do with entry do begin
        if (key^.literalType<>lt_string) or (value^.literalType<>lt_string) then begin
          allOkay:=false;
          exit;
        end;
        requestHeader[k].key  :=P_stringLiteral(key)^.value;
        requestHeader[k].value:=P_stringLiteral(value)^.value;
        inc(k);
      end;
    end;

  FUNCTION responseLiteral:P_literal;
    VAR headerMap:P_mapLiteral;
        i:longint;
        s:string;
        key:string;
    begin
      headerMap:=recycler^.newMapLiteral(0);
      result:=recycler^.newMapLiteral(0)
      ^.put(recycler,'body',resultText)
      ^.put(recycler,'code',client.ResponseStatusCode)
      ^.put(recycler,'status',client.ResponseStatusText)
      ^.put(recycler,'header',headerMap,false);
      for i:=0 to client.ResponseHeaders.count-1 do begin
        s:=client.ResponseHeaders[i];
        key:=fetch(s,':');
        headerMap^.put(recycler,key,trim(s));
      end;
    end;

  VAR isHttpsRequest:boolean;
  begin
    setLength(requestHeader,0);
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=3) and (arg0^.literalType=lt_string) then begin
      requestText:=str0^.value;
      isHttpsRequest:=startsWith(lowercase(requestText),'https');
      for k:=1 to params^.size-1 do case params^.value[k]^.literalType of
        lt_string: begin
          if requestStream=nil
          then requestStream:=TStringStream.create(P_stringLiteral(params^.value[k])^.value)
          else allOkay:=false;
        end;
        lt_map: begin
          if length(requestHeader)=0
          then constructRequestHeader(P_mapLiteral(params^.value[k]))
          else allOkay:=false;
        end;
        else allOkay:=false;
      end;
    end else allOkay:=false;
    if allOkay then begin
      if isHttpsRequest then enterCriticalSection(httpRequestCs);
      try
        client:=TFPHTTPClient.create(nil);
        client.AllowRedirect:=true;
        client.RequestBody:=requestStream;
        for k:=0 to length(requestHeader)-1 do client.AddHeader(requestHeader[k].key,requestHeader[k].value);
        case method of
          hm_put:   resultText:=client.put   (requestText);
          hm_post:  resultText:=client.Post  (requestText);
          hm_delete:resultText:=client.delete(requestText);
          else      resultText:=client.get   (requestText);
        end;
      except
        on E : Exception do begin
          resultText:='';
          context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,methodName[method]+' failed with: '+E.message);
        end;
      end;
      if isHttpsRequest then leaveCriticalSection(httpRequestCs);
      result:=responseLiteral;
      client.free;
    end;
    if requestStream<>nil then requestStream.free;
    for k:=0 to length(requestHeader)-1 do with requestHeader[k] do begin
      key:='';
      value:='';
    end;
    setLength(requestHeader,0);
  end;

FUNCTION httpGet_imp    intFuncSignature; begin if context^.checkSideEffects('httpGet'   ,tokenLocation,[se_accessHttp]) then result:=httpGetPutPost(hm_get   ,params,tokenLocation,context,recycler) else result:=nil; end;
FUNCTION httpPut_imp    intFuncSignature; begin if context^.checkSideEffects('httpPut'   ,tokenLocation,[se_accessHttp]) then result:=httpGetPutPost(hm_put   ,params,tokenLocation,context,recycler) else result:=nil; end;
FUNCTION httpPost_imp   intFuncSignature; begin if context^.checkSideEffects('httpPost'  ,tokenLocation,[se_accessHttp]) then result:=httpGetPutPost(hm_post  ,params,tokenLocation,context,recycler) else result:=nil; end;
FUNCTION httpDelete_imp intFuncSignature; begin if context^.checkSideEffects('httpDelete',tokenLocation,[se_accessHttp]) then result:=httpGetPutPost(hm_delete,params,tokenLocation,context,recycler) else result:=nil; end;

FUNCTION openUrl_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context^.checkSideEffects('openUrl',tokenLocation,[se_executingExternal])
    then result:=newBoolLiteral(OpenURL(str0^.value))
    else result:=nil;
  end;

FUNCTION isServerRunning_imp intFuncSignature;
  VAR serverIsRunning:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      enterCriticalSection(localServerCs);
      serverIsRunning:=localServers.containsKey(str0^.value);
      leaveCriticalSection(localServerCs);
      //TODO: Try to connect to server port
      result:=newBoolLiteral(serverIsRunning);
    end;
  end;

INITIALIZATION
  {$WARN 5058 OFF}
  initCriticalSection(localServerCs);
  localServers.create();
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'startHttpServer'     ,@startServer_impl         ,ak_ternary   ,[se_alterContextState,se_server,se_detaching]);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'wrapTextInHttp'      ,@wrapTextInHttp_impl      ,ak_variadic_1);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'httpError'           ,@httpError_impl           ,ak_variadic  );
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'extractParameters'   ,@extractParameters_impl   ,ak_unary     );
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'extractRawParameters',@extractRawParameters_impl,ak_unary     );
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'extractPath'         ,@extractPath_impl         ,ak_unary     );
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'encodeRequest'       ,@encodeRequest_impl       ,ak_ternary   );
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'httpGet'             ,@httpGet_imp              ,ak_unary     ,[se_accessHttp]);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'httpPut'             ,@httpPut_imp              ,ak_unary     ,[se_accessHttp]);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'httpPost'            ,@httpPost_imp             ,ak_unary     ,[se_accessHttp]);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'httpDelete'          ,@httpDelete_imp           ,ak_unary     ,[se_accessHttp]);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'openUrl'             ,@openUrl_imp              ,ak_unary     ,[se_executingExternal]);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'isServerRunning'     ,@isServerRunning_imp      ,ak_unary     );

  initialize(httpRequestCs);
  initCriticalSection(httpRequestCs);
FINALIZATION
  doneCriticalSection(httpRequestCs);
  doneCriticalSection(localServerCs);
  localServers.destroy;

end.
