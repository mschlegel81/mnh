UNIT mnh_funcs_server;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,math,fphttpclient,lclintf,
     myStringUtil,myGenerics,httpUtil,
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_litVar,
     mnh_contexts,
     mnh_funcs;

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
    context:P_threadContext;
    socket:T_socketPair;

    CONSTRUCTOR create(CONST ip_:string; CONST servingExpression_:P_expressionLiteral; CONST timeout_:double; CONST feedbackLocation_:T_tokenLocation; CONST context_: P_threadContext);
    DESTRUCTOR destroy;
    PROCEDURE serve;
    PROCEDURE killQuickly;
  end;

  T_httpMethod=(hm_get,hm_put,hm_post,hm_delete);
{$i mnh_func_defines.inc}

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
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int)
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
      childContext:P_threadContext;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType=lt_expression) and
       (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(3)) and
       (arg2^.literalType in [lt_int,lt_real]) then begin

      if arg2^.literalType=lt_int then timeout:=int2^.value/(24*60*60)
                                  else timeout:=real2^.value/(24*60*60);
      servingExpression:=P_expressionLiteral(arg1);
      servingExpression^.rereference;
      childContext:=context.getNewAsyncContext;
      if childContext<>nil then begin
        new(microserver,create(str0^.value,servingExpression,timeout,tokenLocation,childContext));
        if microserver^.socket.getLastListenerSocketError=0 then begin
          beginThread(@microserverThread,microserver);
          repeat ThreadSwitch; sleep(1); until microserver^.up;
        end else begin
          context.adapters^.raiseError('Error in socket creation ('+microserver^.ip+')',tokenLocation);
          dispose(microserver,destroy);
        end;
        result:=newVoidLiteral;
      end else context.adapters^.raiseError('startServer is not allowed in this context because delegation is disabled.',tokenLocation);
    end;
  end;

PROCEDURE killServerWithIp(s:P_microserver; ip:pointer);
  begin
    if s^.ip=PAnsiString(ip)^ then s^.killQuickly;
  end;

CONSTRUCTOR T_microserver.create(CONST ip_: string; CONST servingExpression_: P_expressionLiteral; CONST timeout_: double; CONST feedbackLocation_: T_tokenLocation; CONST context_: P_threadContext);
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
  begin
    disposeLiteral(servingExpression);
    context^.doneEvaluating;
    dispose(context,destroy);
    socket.destroy;
    registry.onDestruction(@self);
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

  VAR request:T_requestTriplet;
      response:P_literal;
      requestLiteral:T_listLiteral;
      sleepTime:longint=minSleepTime;
      start:double;
      statistics:record
        serveCount:longint;
        serveTime:double;
        socketTime:double;
      end;
      finalMessage:T_arrayOfString;
  begin
    with statistics do begin
      serveCount:=0;
      serveTime:=0;
      socketTime:=0;
    end;
    context^.adapters^.raiseNote('http Microserver started. '+socket.toString,feedbackLocation);
    up:=true;
    lastActivity:=now;
    repeat
      start:=context^.wallclockTime(true);
      request:=socket.getRequest(sleepTime);
      if request.method<>htrm_no_request then begin
        statistics.socketTime:=statistics.socketTime+(context^.wallclockTime-start);
        inc(statistics.serveCount);

        start:=context^.wallclockTime;
        sleepTime:=minSleepTime;
        lastActivity:=now;
        requestLiteral.create(3);
        requestLiteral.appendString(C_httpRequestMethodName[request.method])^
                      .appendString(request.request)^
                      .appendString(request.protocol);
        response:=servingExpression^.evaluate(feedbackLocation,context,@requestLiteral);
        requestLiteral.destroy;
        statistics.serveTime:=statistics.serveTime+(context^.wallclockTime-start);
        start:=context^.wallclockTime;
        if (response<>nil) then begin
          if response^.literalType in C_scalarTypes
          then socket.SendString(P_scalarLiteral(response)^.stringForm)
          else socket.SendString(response^.toString);
          disposeLiteral(response);
        end else begin
          context^.adapters^.raiseWarning('Microserver response is nil!', feedbackLocation);
          socket.SendString(HTTP_404_RESPONSE);
        end;
        statistics.socketTime:=statistics.socketTime+(context^.wallclockTime-start);
      end else begin
        inc(sleepTime);
        if sleepTime>maxSleepTime then sleepTime:=maxSleepTime;
      end;
    until timedOut or hasKillRequest or not(context^.adapters^.noErrors);
    finalMessage:='http Microserver stopped. '+socket.toString;
    append(finalMessage,'  served '+intToStr(statistics.serveCount)+' requests');
    append(finalMessage,'  evaluation time '+floatToStr(statistics.serveTime)+' seconds');
    append(finalMessage,'  socket time '+floatToStr(statistics.socketTime)+' seconds');
    context^.adapters^.raiseNote(finalMessage,feedbackLocation);
    up:=false;
  end;

PROCEDURE T_microserver.killQuickly;
  begin
    timeout:=0;
    hasKillRequest:=true;
    while up do sleep(1);
  end;

FUNCTION percentCode(CONST c:byte):string;
  CONST hexDigit:array[0..15] of char=('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  begin
    result:='%'+hexDigit[c shr 4]+hexDigit[c and 15];
  end;

FUNCTION extractParameters_impl intFuncSignature;
  VAR parameters:P_mapLiteral;
  PROCEDURE addParameterPair(CONST pair:string);

    VAR keyAndValue:T_arrayOfString;
        c:byte;
        i:longint;
        value:P_stringLiteral;
        castValue:P_scalarLiteral;
    begin
      keyAndValue:=split(pair,'=');
      while length(keyAndValue)<2 do append(keyAndValue,'');
      for i:=0 to length(keyAndValue)-1 do
      for c:=0 to 255 do keyAndValue[i]:=replaceAll(keyAndValue[i],percentCode(c),chr(c));
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

  FUNCTION percentEncode(CONST s:string):string;
    VAR c:char;
    begin
      result:='';
      for c in s do if c in ['A'..'Z','a'..'z','0'..'9','-','_'] then result:=result+c else result:=result+percentCode(ord(c));
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

FUNCTION httpGetPutPost(CONST method:T_httpMethod; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  CONST methodName:array[T_httpMethod] of string=('httpGet','httpPut','httpPost','httpDelete');
  VAR resultText:ansistring='';
      requestText:ansistring='';
      encodedRequest:P_literal;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      requestText:=str0^.value;
    end else begin
      encodedRequest:=encodeRequest_impl(params,tokenLocation,context);
      if encodedRequest=nil then exit(nil)
                            else requestText:=P_stringLiteral(encodedRequest)^.value;
    end;
    try
      case method of
        hm_put:   resultText:=TFPCustomHTTPClient.SimplePut   (requestText);
        hm_post:  resultText:=TFPCustomHTTPClient.SimplePost  (requestText);
        hm_delete:resultText:=TFPCustomHTTPClient.SimpleDelete(requestText);
        else      resultText:=TFPCustomHTTPClient.SimpleGet   (requestText);
      end;
    except
      on E : Exception do begin
        resultText:='';
        context.adapters^.raiseWarning(methodName[method]+' failed with:'+E.message,tokenLocation);
      end;
    end;
    result:=newStringLiteral(resultText);
  end;

FUNCTION httpGet_imp    intFuncSignature; begin result:=httpGetPutPost(hm_get   ,params,tokenLocation,context); end;
FUNCTION httpPut_imp    intFuncSignature; begin result:=httpGetPutPost(hm_put   ,params,tokenLocation,context); end;
FUNCTION httpPost_imp   intFuncSignature; begin result:=httpGetPutPost(hm_post  ,params,tokenLocation,context); end;
FUNCTION httpDelete_imp intFuncSignature; begin result:=httpGetPutPost(hm_delete,params,tokenLocation,context); end;

FUNCTION openUrl_imp intFuncSignature;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
    then result:=newBoolLiteral(OpenURL(str0^.value))
    else result:=nil;
  end;

FUNCTION stopAllHttpServers_impl intFuncSignature;
  VAR allKilled:boolean;
  begin
    if (params=nil) or (params^.size=0) then begin
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
  registerRule(HTTP_NAMESPACE,'startHttpServer'     ,@startServer_impl         ,[se_alterContextState,se_server,se_detaching],ak_ternary   ,'startHttpServer(urlAndPort:string,requestToResponseFunc:expression(3),timeoutInSeconds:numeric);//Starts a new microserver-instance');
  registerRule(HTTP_NAMESPACE,'wrapTextInHttp'      ,@wrapTextInHttp_impl      ,[],ak_variadic_1,'wrapTextInHttp(s:string);//Wraps s in an http-response (type: "text/html")#wrapTextInHttp(s:string,type:string);//Wraps s in an http-response of given type.');
  registerRule(HTTP_NAMESPACE,'httpError'           ,@httpError_impl           ,[],ak_variadic  ,'httpError;//Returns http-representation of error 404.#httpError(code:int);//Returns http-representation of given error code.');
  registerRule(HTTP_NAMESPACE,'extractParameters'   ,@extractParameters_impl   ,[],ak_unary     ,'extractParameters(request:string);//Returns the parameters of an http request as a keyValueList');
  registerRule(HTTP_NAMESPACE,'extractRawParameters',@extractRawParameters_impl,[],ak_unary     ,'extractRawParameters(request:string);//Returns the parameter part of an http request as a string');
  registerRule(HTTP_NAMESPACE,'extractPath'         ,@extractPath_impl         ,[],ak_unary     ,'extractPath(request:string);//Returns the path part of an http request as a string');
  registerRule(HTTP_NAMESPACE,'encodeRequest'       ,@encodeRequest_impl       ,[],ak_ternary   ,'encodeRequest(address:string,path:string,parameters:string);#encodeRequest(address:string,path:string,parameters:keyValueList);//Returns an http request from the given components');
  registerRule(HTTP_NAMESPACE,'httpGet'             ,@httpGet_imp              ,[se_accessHttp],ak_unary     ,'httpGet(URL:string);//Retrieves the contents of the given URL and returns them as a string#httpGet(address:string,path:string,parameters:string);#httpGet(address:string,path:string,parameters:keyValueList);');
  registerRule(HTTP_NAMESPACE,'httpPut'             ,@httpPut_imp              ,[se_accessHttp],ak_unary     ,'httpPut(URL:string);#httpPut(address:string,path:string,parameters:string);#httpPut(address:string,path:string,parameters:keyValueList);');
  registerRule(HTTP_NAMESPACE,'httpPost'            ,@httpPost_imp             ,[se_accessHttp],ak_unary     ,'httpPost(URL:string);#httpPost(address:string,path:string,parameters:string);#httpPost(address:string,path:string,parameters:keyValueList);');
  registerRule(HTTP_NAMESPACE,'httpDelete'          ,@httpDelete_imp           ,[se_accessHttp],ak_unary     ,'httpDelete(URL:string);#httpDelete(address:string,path:string,parameters:string);#httpDelete(address:string,path:string,parameters:keyValueList);');
  registerRule(HTTP_NAMESPACE,'openUrl'             ,@openUrl_imp              ,[se_executingExternal],ak_unary     ,'openUrl(URL:string);//Opens the URL in the default browser');
  registerRule(HTTP_NAMESPACE,'stopAllHttpServers'  ,@stopAllHttpServers_impl  ,[se_alterContextState,se_server],ak_nullary   ,'stopAllHttpServers;//Stops all currently running httpServers and waits for shutdown');

FINALIZATION
  registry.destroy;

end.
