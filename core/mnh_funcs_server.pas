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
    adapters:P_adapters;

    CONSTRUCTOR create(CONST ip_:string; CONST servingExpression_:P_expressionLiteral; CONST timeout_:double; CONST feedbackLocation_:T_tokenLocation; CONST parentContext:T_evaluationContext);
    DESTRUCTOR destroy;
    PROCEDURE serve;
    PROCEDURE killQuickly;
  end;

PROCEDURE killActiveServers;
IMPLEMENTATION
{$MACRO ON}
{$define str0:=P_stringLiteral(params^.value(0))}
{$define str1:=P_stringLiteral(params^.value(1))}
{$define int0:=P_intLiteral(params^.value(0))}
{$define int2:=P_intLiteral(params^.value(2))}
{$define real2:=P_realLiteral(params^.value(2))}
{$define arg0:=params^.value(0)}
{$define arg1:=params^.value(1)}
{$define arg2:=params^.value(2)}

VAR serverCS:system.TRTLCriticalSection;
    currentUpServers:array of P_microserver;
PROCEDURE killActiveServers;
  VAR i:longint;
      allDone:boolean;
  begin
    enterCriticalSection(serverCS);
    for i:=0 to length(currentUpServers)-1 do currentUpServers[i]^.hasKillRequest:=true;
    allDone:=length(currentUpServers)<=0;
    leaveCriticalSection(serverCS);
    while not(allDone) do begin
      sleep(100);
      enterCriticalSection(serverCS);
      for i:=0 to length(currentUpServers)-1 do currentUpServers[i]^.hasKillRequest:=true;
      allDone:=length(currentUpServers)<=0;
      leaveCriticalSection(serverCS);
    end;
  end;

FUNCTION wrapTextInHttp_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  CONST serverInfo='MNH5 via Synapse';
  begin
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string) then begin
      if params^.size=1                                     then exit(newStringLiteral(wrapTextInHttp(str0^.value,serverInfo)));
      if (params^.size=2) and (arg1^.literalType=lt_string) then exit(newStringLiteral(wrapTextInHttp(str0^.value,serverInfo,str1^.value)));
      result:=nil;
    end else result:=nil;
  end;

FUNCTION httpError_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_int)
    then result:=newStringLiteral('HTTP/1.0 '+arg0^.toString+C_carriageReturnChar+C_lineBreakChar)
    else if (params=nil) or (params^.size=0)
    then result:=newStringLiteral('HTTP/1.0 404'+C_carriageReturnChar+C_lineBreakChar)
    else result:=nil;
  end;

FUNCTION startServer_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR microserver:P_microserver;
      timeout:double;
      servingExpression:P_expressionLiteral;
      servingSubrule:P_subrule;
  begin
    if not(context.allowDelegation) then begin
      context.adapters^.raiseError('startServer is not allowed in this context because delegation is disabled.',tokenLocation);
      exit(newVoidLiteral);
    end;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType=lt_expression) and
       (P_expressionLiteral(arg1)^.arity<=4) and
       (arg2^.literalType in [lt_int,lt_real]) then begin
      {$ifdef fullVersion}
      if currentlyDebugging then begin
        context.adapters^.raiseError('Cannot start microserver in debug mode. Sorry.',tokenLocation);
        exit(nil);
      end;
      {$endif}
      if arg2^.literalType=lt_int then timeout:=int2^.value/(24*60*60)
                                  else timeout:=real2^.value/(24*60*60);
      servingExpression:=P_expressionLiteral(arg1);
      if servingExpression^.arity<4 then begin
        new(servingSubrule,clone(servingExpression^.value));
        servingExpression:=newExpressionLiteral(servingSubrule);
        servingSubrule^.increaseArity(4);
      end else servingExpression^.rereference;
      new(microserver,create(str0^.value,servingExpression,timeout,tokenLocation,context));
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION microserverThread(p:pointer):ptrint;
  begin
    P_microserver(p)^.serve;
    dispose(P_microserver(p),destroy);
    result:=0;
  end;

CONSTRUCTOR T_microserver.create(CONST ip_: string; CONST servingExpression_: P_expressionLiteral; CONST timeout_: double; CONST feedbackLocation_: T_tokenLocation; CONST parentContext: T_evaluationContext);
  VAR i:longint;
  begin
    system.enterCriticalSection(serverCS);
    ip:=cleanIp(ip_);
    if isNan(timeout_) or isInfinite(timeout_) or (timeout_<0)
    then timeout:=0
    else timeout:=timeout_;
    servingExpression:=servingExpression_;
    feedbackLocation:=feedbackLocation_;
    adapters:=parentContext.adapters;
    hasKillRequest:=false;
    for i:=0 to length(currentUpServers)-1 do if currentUpServers[i]^.ip=ip then currentUpServers[i]^.killQuickly;
    setLength(currentUpServers,length(currentUpServers)+1);
    currentUpServers[length(currentUpServers)-1]:=@self;
    beginThread(@microserverThread,@self);
    system.leaveCriticalSection(serverCS);
  end;

DESTRUCTOR T_microserver.destroy;
  VAR i,j:longint;
  begin
    system.enterCriticalSection(serverCS);
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
      socket:T_socketPair;
      context:T_evaluationContext;

  PROCEDURE initRequestLiteral(CONST request:string);
    VAR parts:T_arrayOfString;
        parameters:P_listLiteral;
        i:longint;

    PROCEDURE addParameterPair(CONST pair:string);
      FUNCTION code(CONST c:byte):string;
        CONST hexDigit:array[0..15] of char=('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
        begin
          result:='%'+hexDigit[c shr 4]+hexDigit[c and 15];
        end;

      VAR keyAndValue:T_arrayOfString;
          c:byte;
          i:longint;
          value:P_stringLiteral;
          castValue:P_scalarLiteral;
      begin
        keyAndValue:=split(pair,'=');
        while length(keyAndValue)<2 do append(keyAndValue,'');
        for i:=0 to length(keyAndValue)-1 do
        for c:=0 to 255 do keyAndValue[i]:=replaceAll(keyAndValue[i],code(c),chr(c));
         value:=newStringLiteral(keyAndValue[1]);
        castValue:=value^.softCast;
        disposeLiteral(value);
        parameters^.append(
          newListLiteral^
            .appendString(keyAndValue[0])^
            .append(castValue,false),
          false);
      end;

    begin
      requestLiteral.create;
      requestLiteral.appendString(request);
      parts:=split(request,'?');
      while (length(parts)<2) do append(parts,'');
      requestLiteral.appendString(parts[0]);
      requestLiteral.appendString(parts[1]);
      parameters:=newListLiteral;
      if length(parts[1])>0 then begin
        parts:=split(parts[1],'&');
        for i:=0 to length(parts)-1 do addParameterPair(parts[i]);
        parameters^.toKeyValueList;
      end;
      requestLiteral.append(parameters,false);
    end;

  begin
    socket.create(ip);
    context.createNormalContext(adapters);
    context.adapters^.raiseNote('Microserver started. '+socket.toString,feedbackLocation);
    up:=true;
    lastActivity:=now;
    repeat
      request:=socket.getRequest(sleepTime);
      if request<>'' then begin
        sleepTime:=minSleepTime;
        lastActivity:=now;
        initRequestLiteral(request);
        response:=servingExpression^.evaluate(@requestLiteral,@context);
        requestLiteral.destroy;
        if (response<>nil) then begin
          if response^.literalType in C_validScalarTypes
          then socket.SendString(P_scalarLiteral(response)^.stringForm)
          else socket.SendString(P_scalarLiteral(response)^.toString);
          disposeLiteral(response);
        end else begin
          context.adapters^.raiseWarning('Microserver response is nil!', feedbackLocation);
          socket.SendString(HTTP_404_RESPONSE);
        end;
      end else begin
        sleep(sleepTime);
        inc(sleepTime);
        if sleepTime>maxSleepTime then sleepTime:=maxSleepTime;
      end;
    until timedOut or not(context.adapters^.noErrors);
    disposeLiteral(servingExpression);
    socket.destroy;
    context.adapters^.raiseNote('Microserver stopped. '+socket.toString,feedbackLocation);
    context.destroy;
    up:=false;
  end;

PROCEDURE T_microserver.killQuickly;
  begin
    timeout:=0;
    hasKillRequest:=true;
    while up do sleep(1);
  end;

INITIALIZATION
  {$WARN 5058 OFF}
  system.initCriticalSection(serverCS);
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'startHttpServer',@startServer_impl,'startHttpServer(urlAndPort:string,requestToResponseFunc:expression(1),timeoutInSeconds:numeric);#Starts a new microserver-instance');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'wrapTextInHttp',@wrapTextInHttp_impl,'wrapTextInHttp(s:string);#Wraps s in an http-response (type: "text/html")#wrapTextInHttp(s:string,type:string);#Wraps s in an http-response of given type.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'httpError',@httpError_impl,'httpError;#Returns http-representation of error 404.#httpError(code:int);#Returns http-representation of given error code.');
  {$ifdef fullVersion}
  killServersCallback:=@killActiveServers;
  {$endif}
FINALIZATION
  doneCriticalSection(serverCS);

end.
