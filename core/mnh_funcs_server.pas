UNIT mnh_funcs_server;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,math,mnh_constants,mnh_funcs,httpUtil,mnh_contexts,mnh_litVar,mnh_basicTypes,mnh_out_adapters,mnh_packages,myStringUtil,myGenerics;
TYPE
  P_microserver=^T_microserver;
  T_microserver=object
    ip:string;
    timeout:double;
    servingExpression:P_expressionLiteral;
    feedbackLocation:T_tokenLocation;
    hasKillRequest:boolean;
    up:boolean;
    context:P_evaluationContext;
    socket:T_socketPair;

    CONSTRUCTOR create(CONST ip_:string; CONST servingExpression_:P_expressionLiteral; CONST timeout_:double; CONST feedbackLocation_:T_tokenLocation; CONST context_: P_evaluationContext);
    DESTRUCTOR destroy;
    PROCEDURE serve;
    PROCEDURE killQuickly;
  end;

IMPLEMENTATION
{$i mnh_func_defines.inc}

VAR serverCS:system.TRTLCriticalSection;
    currentUpServers:array of P_microserver;

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
      childContext:P_evaluationContext;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType=lt_expression) and
       (P_expressionLiteral(arg1)^.canApplyToNumberOfParameters(1)) and
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

CONSTRUCTOR T_microserver.create(CONST ip_: string; CONST servingExpression_: P_expressionLiteral; CONST timeout_: double; CONST feedbackLocation_: T_tokenLocation; CONST context_: P_evaluationContext);
  VAR i:longint;
  begin
    system.enterCriticalSection(serverCS);
    ip:=cleanIp(ip_);
    if isNan(timeout_) or isInfinite(timeout_) or (timeout_<0)
    then timeout:=0
    else timeout:=timeout_;
    servingExpression:=servingExpression_;
    feedbackLocation:=feedbackLocation_;
    context:=context_;
    hasKillRequest:=false;
    for i:=0 to length(currentUpServers)-1 do if currentUpServers[i]^.ip=ip then currentUpServers[i]^.killQuickly;
    setLength(currentUpServers,length(currentUpServers)+1);
    currentUpServers[length(currentUpServers)-1]:=@self;
    socket.create(ip);
    system.leaveCriticalSection(serverCS);
  end;

DESTRUCTOR T_microserver.destroy;
  VAR i,j:longint;
  begin
    system.enterCriticalSection(serverCS);
    socket.destroy;
    j:=0;
    for i:=0 to length(currentUpServers)-1 do if currentUpServers[i]<>@self then begin
      currentUpServers[j]:=currentUpServers[i];
      inc(j);
    end;
    setLength(currentUpServers,j);
    system.leaveCriticalSection(serverCS);
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

  VAR request:ansistring;
      response:P_literal;
      requestLiteral:T_listLiteral;
      sleepTime:longint=minSleepTime;

  begin
    context^.resetForEvaluation(nil);
    context^.adapters^.raiseNote('Microserver started. '+socket.toString,feedbackLocation);
    up:=true;
    lastActivity:=now;
    repeat
      request:=socket.getRequest(sleepTime);
      if request<>'' then begin
        sleepTime:=minSleepTime;
        lastActivity:=now;
        requestLiteral.create;
        requestLiteral.appendString(request);
        response:=servingExpression^.evaluate(@requestLiteral,feedbackLocation,context);
        requestLiteral.destroy;
        if (response<>nil) then begin
          if response^.literalType in C_validScalarTypes
          then socket.SendString(P_scalarLiteral(response)^.stringForm)
          else socket.SendString(P_scalarLiteral(response)^.toString);
          disposeLiteral(response);
        end else begin
          context^.adapters^.raiseWarning('Microserver response is nil!', feedbackLocation);
          socket.SendString(HTTP_404_RESPONSE);
        end;
      end else begin
        sleep(sleepTime);
        inc(sleepTime);
        if sleepTime>maxSleepTime then sleepTime:=maxSleepTime;
      end;
    until timedOut or not(context^.adapters^.noErrors);
    disposeLiteral(servingExpression);
    context^.adapters^.raiseNote('Microserver stopped. '+socket.toString,feedbackLocation);
    context^.afterEvaluation;
    dispose(context,destroy);
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
  VAR parameters:P_listLiteral;
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
      parameters^.append(
        newListLiteral^
          .appendString(keyAndValue[0])^
          .append(castValue,false),
        false);
    end;

  VAR parts:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      parts:=split(str0^.value,'?');
      while (length(parts)<2) do append(parts,'');
      parameters:=newListLiteral;
      if length(parts[1])>0 then begin
        parts:=split(parts[1],'&');
        for i:=0 to length(parts)-1 do addParameterPair(parts[i]);
        parameters^.toKeyValueList(false);
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
  VAR address:string='';
      path:string='';
      parameters:string='';
      i:longint;
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

  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_string) and (arg2^.literalType in [lt_emptyList,lt_keyValueList,lt_string]) then begin
      address:=str0^.value;
      path:=str1^.value;
      if not(startsWith(path,'/')) then path:='/'+path;
      case arg2^.literalType of
        lt_string: parameters:=percentEncode(str2^.value);
        lt_keyValueList: for i:=0 to list2^.size-1 do begin
          if i>0 then parameters:=parameters+'&';
          parameters:=parameters+percentEncode(P_stringLiteral(P_listLiteral(list2^.value(i))^.value(0))^.value)
                            +'='+percentEncode(getString      (P_listLiteral(list2^.value(i))^.value(1))       );
        end;

      end;
      if parameters<>'' then parameters:='?'+parameters;
      result:=newStringLiteral(address+path+parameters);
    end;
  end;


INITIALIZATION
  {$WARN 5058 OFF}
  system.initCriticalSection(serverCS);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'startHttpServer',@startServer_impl,'startHttpServer(urlAndPort:string,requestToResponseFunc:expression(1),timeoutInSeconds:numeric);#Starts a new microserver-instance');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'wrapTextInHttp',@wrapTextInHttp_impl,'wrapTextInHttp(s:string);#Wraps s in an http-response (type: "text/html")#wrapTextInHttp(s:string,type:string);#Wraps s in an http-response of given type.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'httpError',@httpError_impl,'httpError;#Returns http-representation of error 404.#httpError(code:int);#Returns http-representation of given error code.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'extractParameters',@extractParameters_impl,'extractParameters(request:string);#Returns the parameters of an http request as a keyValueList');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'extractRawParameters',@extractRawParameters_impl,'extractRawParameters(request:string);#Returns the parameter part of an http request as a string');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'extractPath',@extractPath_impl,'extractPath(request:string);#Returns http-representation of error 404.#Returns the path part of an http request as a string');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'encodeRequest',@encodeRequest_impl,'encodeRequest(address:string,path:string,parameters:string);#encodeRequest(address:string,path:string,parameters:keyValueList);#Returns an http request from the given components');

FINALIZATION
  doneCriticalSection(serverCS);

end.
