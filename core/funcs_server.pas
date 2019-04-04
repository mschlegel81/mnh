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
     funcs;

PROCEDURE onPackageFinalization(CONST package:P_objectWithPath);
IMPLEMENTATION
TYPE
  P_microserver=^T_microserver;
  T_microserver=object
    ip:ansistring;
    timeout:double;
    servingExpression:P_expressionLiteral;
    feedbackLocation:T_tokenLocation;
    hasKillRequest:boolean;
    up:boolean;
    context:P_context;
    socket:T_socketPair;

    CONSTRUCTOR create(CONST ip_:string; CONST servingExpression_:P_expressionLiteral; CONST timeout_:double; CONST feedbackLocation_:T_tokenLocation; CONST context_: P_context);
    DESTRUCTOR destroy;
    PROCEDURE serve;
    PROCEDURE killQuickly;
  end;

  T_httpMethod=(hm_get,hm_put,hm_post,hm_delete);
{$i func_defines.inc}

VAR registry:specialize G_instanceRegistry<P_microserver>;

PROCEDURE sendKillRequest(s:P_microserver); begin s^.hasKillRequest:=true; end;
PROCEDURE killAllServers; begin registry.forEach(@sendKillRequest); end;

FUNCTION serverIsAssociatedWithPackage(s:P_microserver; package:pointer):boolean;
  begin
    result:=s^.feedbackLocation.package=package;
    if result then s^.hasKillRequest:=true;
  end;

PROCEDURE onPackageFinalization(CONST package:P_objectWithPath);
  begin
    while registry.anyMatch(@serverIsAssociatedWithPackage,package) do sleep(1);
  end;

FUNCTION wrapTextInHttp_impl intFuncSignature;
  CONST serverInfo='MNH5 via Synapse';
  begin
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      if params^.size=1                                     then exit(newStringLiteral(wrapTextInHttp(str0^.value,serverInfo)));
      if (params^.size=2) and (arg1^.literalType=lt_string) then exit(newStringLiteral(wrapTextInHttp(str0^.value,serverInfo,str1^.value)));
      result:=nil;
    end else result:=nil;
  end;

FUNCTION httpError_impl intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_smallint)
    then result:=newStringLiteral('HTTP/1.0 '+arg0^.toString+C_carriageReturnChar+C_lineBreakChar)
    else if (params=nil) or (params^.size=0)
    then result:=newStringLiteral('HTTP/1.0 404'+C_carriageReturnChar+C_lineBreakChar)
    else result:=nil;
  end;

FUNCTION microserverThread(p:pointer):ptrint;
  begin
    P_microserver(p)^.serve;
    dispose(P_microserver(p),destroy);
    result:=0;
  end;

FUNCTION startServer_impl intFuncSignature;
  VAR microserver:P_microserver;
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
       context.checkSideEffects('startHttpServer',tokenLocation,[se_alterContextState,se_server,se_detaching]) then begin

      timeout:=P_numericLiteral(arg2)^.floatValue/(24*60*60);
      servingExpression:=P_expressionLiteral(arg1);
      servingExpression^.rereference;
      childContext:=context.getNewAsyncContext(recycler,false);
      if childContext<>nil then begin
        new(microserver,create(str0^.value,servingExpression,timeout,tokenLocation,childContext));
        if microserver^.socket.getLastListenerSocketError=0 then begin
          beginThread(@microserverThread,microserver);
          repeat ThreadSwitch; sleep(1); until microserver^.up;
        end else begin
          context.raiseError('Error in socket creation ('+microserver^.ip+')',tokenLocation);
          dispose(microserver,destroy);
        end;
        result:=newVoidLiteral;
      end else context.raiseError('startServer is not allowed in this context because delegation is disabled.',tokenLocation);
    end;
  end;

PROCEDURE killServerWithIp(s:P_microserver; ip:pointer);
  begin
    if s^.ip=PAnsiString(ip)^ then s^.killQuickly;
  end;

CONSTRUCTOR T_microserver.create(CONST ip_: string; CONST servingExpression_: P_expressionLiteral; CONST timeout_: double; CONST feedbackLocation_: T_tokenLocation; CONST context_: P_context);
  begin
    ip:=cleanIp(ip_);
    registry.forEach(@killServerWithIp,@ip);
    if isNan(timeout_) or isInfinite(timeout_) or (timeout_<0)
    then timeout:=0
    else timeout:=timeout_;
    servingExpression:=servingExpression_;
    feedbackLocation:=feedbackLocation_;
    context:=context_;
    hasKillRequest:=false;
    socket.create(ip);
    registry.onCreation(@self);
  end;

DESTRUCTOR T_microserver.destroy;
  VAR recycler:T_recycler;
  begin
    recycler.initRecycler;
    disposeLiteral(servingExpression);
    context^.finalizeTaskAndDetachFromParent(nil);
    contextPool.disposeContext(context);
    socket.destroy;
    registry.onDestruction(@self);
    recycler.cleanup;
  end;

PROCEDURE T_microserver.serve;
  VAR lastActivity:double;
  FUNCTION timedOut:boolean;
    begin
      result:=(timeout> 0) and (now-lastActivity>timeout) or
              (timeout<=0) and hasKillRequest;
    end;

  CONST minSleepTime=0;
        maxSleepTime=100;

  VAR request:T_httpRequest;
      response:P_literal;
      requestMap:P_mapLiteral;
      sleepTime:longint=minSleepTime;
      start:double;
      statistics:record
        serveCount:longint;
        serveTime:double;
        socketTime:double;
      end;
      finalMessage:T_arrayOfString;
      recycler:T_recycler;
  PROCEDURE fillRequestLiteral;
    VAR headerMap:P_mapLiteral;
        i:longint;
    begin
      headerMap:=newMapLiteral;
      requestMap:=newMapLiteral
        ^.put('request',
          newMapLiteral^.put('method',C_httpRequestMethodName[request.method])
                       ^.put('path',request.request)
                       ^.put('protocol',request.protocol),false)
        ^.put('header',headerMap,false)
        ^.put('body',request.body);
      for i:=0 to length(request.header)-1 do with request.header[i] do headerMap^.put(key,value);
    end;

  begin
    recycler.initRecycler;
    with statistics do begin
      serveCount:=0;
      serveTime:=0;
      socketTime:=0;
    end;
    context^.messages^.postTextMessage(mt_el1_note,feedbackLocation,'http Microserver started. '+socket.toString);
    up:=true;
    lastActivity:=now;
    repeat
      start:=context^.wallclockTime;
      request:=socket.getRequest(sleepTime);
      if request.method<>htrm_no_request then begin
        statistics.socketTime:=statistics.socketTime+(context^.wallclockTime-start);
        inc(statistics.serveCount);

        start:=context^.wallclockTime;
        sleepTime:=minSleepTime;
        lastActivity:=now;
        fillRequestLiteral;
        response:=servingExpression^.evaluateToLiteral(feedbackLocation,context,@recycler,requestMap,nil).literal;
        disposeLiteral(requestMap);
        statistics.serveTime:=statistics.serveTime+(context^.wallclockTime-start);
        start:=context^.wallclockTime;
        if (response<>nil) then begin
          if response^.literalType=lt_string
          then socket.sendString(P_stringLiteral(response)^.value)
          else socket.sendString(response^.toString);
          disposeLiteral(response);
        end else begin
          context^.messages^.postTextMessage(mt_el2_warning,feedbackLocation,'Microserver response is nil!');
          socket.sendString(HTTP_404_RESPONSE);
        end;
        statistics.socketTime:=statistics.socketTime+(context^.wallclockTime-start);
      end else begin
        inc(sleepTime);
        if sleepTime>maxSleepTime then sleepTime:=maxSleepTime;
      end;
    until timedOut or hasKillRequest or not(context^.messages^.continueEvaluation);
    finalMessage:='http Microserver stopped. '+socket.toString;
    append(finalMessage,'  served '+intToStr(statistics.serveCount)+' requests');
    append(finalMessage,'  evaluation time '+floatToStr(statistics.serveTime)+' seconds');
    append(finalMessage,'  socket time '+floatToStr(statistics.socketTime)+' seconds');
    context^.messages^.postTextMessage(mt_el1_note,feedbackLocation,finalMessage);
    up:=false;
    recycler.cleanup;
  end;

PROCEDURE T_microserver.killQuickly;
  begin
    timeout:=0;
    hasKillRequest:=true;
    while up do sleep(1);
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
        keyAndValue[i]:=percentDecode(replaceAll(keyAndValue[i],'+',' '));
      end;
      value:=newStringLiteral(keyAndValue[1]);
      castValue:=value^.softCast;
      disposeLiteral(value);
      parameters^.put(keyAndValue[0],castValue,false);
    end;

  VAR parts:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      parts:=split(str0^.value,'?');
      while (length(parts)<2) do append(parts,'');
      parameters:=newMapLiteral;
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
      if length(parts)>1 then exit(newStringLiteral(parts[1])) else exit(newStringLiteral(''));
    end;
  end;

FUNCTION extractPath_impl intFuncSignature;
  VAR parts:T_arrayOfString;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      parts:=split(str0^.value,'?');
      if length(parts)>0 then exit(newStringLiteral(parts[0])) else exit(newStringLiteral(''));
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
      iter:T_arrayOfLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string) and (arg2^.literalType in [lt_emptyList,lt_map,lt_string,lt_emptyMap]) then begin
      address:=str0^.value;
      path:=str1^.value;
      if not(startsWith(path,'/')) then path:='/'+path;
      case arg2^.literalType of
        lt_string: parameters:=percentEncode(str2^.value);
        lt_map: begin
          iter:=map2^.iteratableList;
          for i:=0 to length(iter)-1 do begin
            if i>0 then parameters:=parameters+'&';
            parameters:=parameters+percentEncode(getString(P_listLiteral(iter[i])^.value[0]))
                              +'='+percentEncode(getString(P_listLiteral(iter[i])^.value[1]));
          end;
          disposeLiteral(iter);
        end;
      end;
      if parameters<>'' then parameters:='?'+parameters;
      result:=newStringLiteral(address+path+parameters);
    end;
  end;

FUNCTION httpGetPutPost(CONST method:T_httpMethod; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_context):P_literal;
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
      headerMap:=newMapLiteral;
      result:=newMapLiteral^.put('body',resultText)
      ^.put('code',client.ResponseStatusCode)
      ^.put('status',client.ResponseStatusText)
      ^.put('header',headerMap,false);
      for i:=0 to client.ResponseHeaders.count-1 do begin
        s:=client.ResponseHeaders[i];
        key:=fetch(s,':');
        headerMap^.put(key,trim(s));
      end;
    end;

  begin
    setLength(requestHeader,0);
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.size<=3) and (arg0^.literalType=lt_string) then begin
      requestText:=str0^.value;
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
          context.messages^.postTextMessage(mt_el2_warning,tokenLocation,methodName[method]+' failed with: '+E.message);
        end;
      end;
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

FUNCTION httpGet_imp    intFuncSignature; begin if context.checkSideEffects('httpGet'   ,tokenLocation,[se_accessHttp]) then result:=httpGetPutPost(hm_get   ,params,tokenLocation,context) else result:=nil; end;
FUNCTION httpPut_imp    intFuncSignature; begin if context.checkSideEffects('httpPut'   ,tokenLocation,[se_accessHttp]) then result:=httpGetPutPost(hm_put   ,params,tokenLocation,context) else result:=nil; end;
FUNCTION httpPost_imp   intFuncSignature; begin if context.checkSideEffects('httpPost'  ,tokenLocation,[se_accessHttp]) then result:=httpGetPutPost(hm_post  ,params,tokenLocation,context) else result:=nil; end;
FUNCTION httpDelete_imp intFuncSignature; begin if context.checkSideEffects('httpDelete',tokenLocation,[se_accessHttp]) then result:=httpGetPutPost(hm_delete,params,tokenLocation,context) else result:=nil; end;

FUNCTION openUrl_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context.checkSideEffects('openUrl',tokenLocation,[se_executingExternal])
    then result:=newBoolLiteral(OpenURL(str0^.value))
    else result:=nil;
  end;

FUNCTION stopAllHttpServers_impl intFuncSignature;
  VAR allKilled:boolean;
  begin
    if (params=nil) or (params^.size=0) and context.checkSideEffects('stopAllHttpServers',tokenLocation,[se_alterContextState,se_server]) then begin
      killAllServers;
      repeat
        ThreadSwitch;
        sleep(1);
        allKilled:=registry.registeredCount=0;
      until allKilled;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

INITIALIZATION
  {$WARN 5058 OFF}
  registry.create;
  registerRule(HTTP_NAMESPACE,'startHttpServer'     ,@startServer_impl         ,ak_ternary   ,'startHttpServer(urlAndPort:String,requestToResponseFunc:Expression(1),timeoutInSeconds:Numeric);//Starts a new microserver-instance');
  registerRule(HTTP_NAMESPACE,'wrapTextInHttp'      ,@wrapTextInHttp_impl      ,ak_variadic_1,'wrapTextInHttp(s:String);//Wraps s in an http-response (type: "text/html")#wrapTextInHttp(s:String,type:String);//Wraps s in an http-response of given type.');
  registerRule(HTTP_NAMESPACE,'httpError'           ,@httpError_impl           ,ak_variadic  ,'httpError;//Returns http-representation of error 404.#httpError(code:Int);//Returns http-representation of given error code.');
  registerRule(HTTP_NAMESPACE,'extractParameters'   ,@extractParameters_impl   ,ak_unary     ,'extractParameters(request:String);//Returns the parameters of an http request as a keyValueList');
  registerRule(HTTP_NAMESPACE,'extractRawParameters',@extractRawParameters_impl,ak_unary     ,'extractRawParameters(request:String);//Returns the parameter part of an http request as a string');
  registerRule(HTTP_NAMESPACE,'extractPath'         ,@extractPath_impl         ,ak_unary     ,'extractPath(request:String);//Returns the path part of an http request as a string');
  registerRule(HTTP_NAMESPACE,'encodeRequest'       ,@encodeRequest_impl       ,ak_ternary   ,'encodeRequest(address:String,path:String,parameters:String);#encodeRequest(address:String,path:String,parameters:keyValueList);//Returns an http request from the given components');
  registerRule(HTTP_NAMESPACE,'httpGet'             ,@httpGet_imp              ,ak_unary     ,'httpGet(URL:String);#httpGet(URL:String,body:String,header:Map);//Retrieves the contents of the given URL and returns them as a map ["body"=>...,"code"=>...,"status"=>...,"header"=>...]');
  registerRule(HTTP_NAMESPACE,'httpPut'             ,@httpPut_imp              ,ak_unary     ,'httpPut(URL:String);#httpPut(URL:String,body:String,header:Map);//Performs an http-PUT on the given URL and returns the response as a map ["body"=>...,"code"=>...,"status"=>...,"header"=>...]');
  registerRule(HTTP_NAMESPACE,'httpPost'            ,@httpPost_imp             ,ak_unary     ,'httpPost(URL:String);#httpPost(URL:String,body:String,header:Map);//Performs an http-POST on the given URL and returns the response as a map ["body"=>...,"code"=>...,"status"=>...,"header"=>...]');
  registerRule(HTTP_NAMESPACE,'httpDelete'          ,@httpDelete_imp           ,ak_unary     ,'httpDelete(URL:String);#httpDelete(URL:String,body:String,header:Map);//Performs an http-DELETE on the given URL and returns the response ["body"=>...,"code"=>...,"status"=>...,"header"=>...]');
  registerRule(HTTP_NAMESPACE,'openUrl'             ,@openUrl_imp              ,ak_unary     ,'openUrl(URL:String);//Opens the URL in the default browser');
  registerRule(HTTP_NAMESPACE,'stopAllHttpServers'  ,@stopAllHttpServers_impl  ,ak_nullary   ,'stopAllHttpServers;//Stops all currently running httpServers and waits for shutdown');

FINALIZATION
  registry.destroy;

end.
