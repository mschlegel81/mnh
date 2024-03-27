UNIT funcs_ftp;

{$mode objfpc}{$H+}

INTERFACE
USES sysutils,lclintf,
     Classes,
     synautil,
     myStringUtil,myGenerics,
     basicTypes,mnh_constants,
     mnh_messages,
     out_adapters,
     litVar,
     contexts,
     recyclers,
     funcs,
     synsock;

CONST FTP_TYPE_STRING='FTPconnection';
IMPLEMENTATION
USES ftpsend,subrules,fileWrappers;
TYPE
  P_ftpConnection=^T_ftpConnection;
  T_ftpConnection=object(T_builtinObject)
    protected
      connectionCs:TRTLCriticalSection;
      connection:TFTPSend;
      enableLogging:boolean;
      FUNCTION getEquivalentInlineExpression(CONST context:P_context; CONST recycler:P_recycler):P_inlineExpression; virtual;
    public
      CONSTRUCTOR create(CONST host, port, user, pass: string; CONST context:P_context; CONST location:T_tokenLocation);
      DESTRUCTOR destroy; virtual;

      //From T_Expression:
      FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_literalRecycler; CONST parameters:P_listLiteral=nil):T_evaluationResult; virtual;
      FUNCTION applyBuiltinFunction(CONST intrinsicRuleId:string; CONST funcLocation:T_tokenLocation; CONST threadContext:P_abstractContext; CONST recycler:P_literalRecycler):P_expressionLiteral; virtual;
      FUNCTION arity:T_arityInfo; virtual;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean; virtual;
      FUNCTION clone(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_literalRecycler):P_expressionLiteral; virtual;
      FUNCTION mustBeDroppedBeforePop:boolean; virtual;
      FUNCTION getParameterNames(CONST literalRecycler:P_literalRecycler):P_listLiteral; virtual;
      //From T_builtinExpression
      FUNCTION toString(CONST lengthLimit:longint=maxLongint): ansistring; virtual;

      PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
  end;

FUNCTION T_ftpConnection.getEquivalentInlineExpression(CONST context: P_context; CONST recycler: P_recycler): P_inlineExpression;
  begin
    context^.raiseError('Invalid operation on FTP connection.',getLocation,mt_el3_evalError);
    result:=nil;
  end;

CONSTRUCTOR T_ftpConnection.create(CONST host, port, user, pass: string; CONST context:P_context; CONST location:T_tokenLocation);
  VAR loginSuccessful:boolean;
  begin
    inherited create(FTP_TYPE_STRING,location);

    initCriticalSection(connectionCs);
    connection:=TFTPSend.create;
    connection.messages:=context^.messages;
    enableLogging:=true;
    with connection do begin
      Username := user;
      Password := pass;
      TargetHost := host;
      TargetPort := port;
      loginSuccessful:=Login;
    end;
    if not loginSuccessful then begin
      context^.messages^.postTextMessage(mt_el2_warning,location,'FTP login with the given credentials was rejected. Last status: '+intToStr(connection.ResultCode));
      FreeAndNil(connection);
    end;

  end;

DESTRUCTOR T_ftpConnection.destroy;
  begin
    doneCriticalSection(connectionCs);
    inherited;
  end;

FUNCTION T_ftpConnection.evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: P_literalRecycler; CONST parameters: P_listLiteral): T_evaluationResult;
  begin
    enterCriticalSection(connectionCs);
    if connection=nil
    then result.literal:=newVoidLiteral
    else result.literal:=recycler^.newStringLiteral('ftp://'+connection.TargetHost+':'+connection.TargetPort);
    result.reasonForStop:=rr_ok;
    leaveCriticalSection(connectionCs);
  end;

FUNCTION T_ftpConnection.applyBuiltinFunction(CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; CONST threadContext: P_abstractContext; CONST recycler: P_literalRecycler): P_expressionLiteral;
  begin
    result:=nil;
  end;

FUNCTION T_ftpConnection.arity: T_arityInfo;
  begin
    result.maxPatternLength:=0;
    result.minPatternLength:=0;
  end;

FUNCTION T_ftpConnection.canApplyToNumberOfParameters(CONST parCount: longint): boolean;
  begin
    result:=parCount=0;
  end;

FUNCTION T_ftpConnection.clone(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: P_literalRecycler): P_expressionLiteral;
  begin
    context^.raiseError('FTP connections should never be cloned',location);
    result:=P_expressionLiteral(rereferenced);
  end;

FUNCTION T_ftpConnection.mustBeDroppedBeforePop: boolean;
  begin
    result:=true;
  end;

FUNCTION T_ftpConnection.getParameterNames(CONST literalRecycler: P_literalRecycler): P_listLiteral;
  begin
    result:=literalRecycler^.newListLiteral();
  end;

FUNCTION T_ftpConnection.toString(CONST lengthLimit: longint): ansistring;
  begin
    result:=getId;
  end;

PROCEDURE T_ftpConnection.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    enterCriticalSection(connectionCs);
    if connection<>nil then begin
      connection.Logout;
      FreeAndNil(connection);
    end;
    leaveCriticalSection(connectionCs);
  end;

{$i func_defines.inc}
FUNCTION ftp_connect_impl intFuncSignature;
  CONST default_user='anonymous';
        default_pass='user@mnh.com';
  VAR host  :string='';   //required
      ftpPort:longint=21;
      user  :string='';
      pass  :string='';
      portRead:boolean=false;
      k:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) then begin
      for k:=0 to params^.size-1 do begin
        case params^.value[k]^.literalType of
          lt_string: begin
            if host  ='' then host  :=P_stringLiteral(params^.value[k])^.value else
            if user  ='' then user  :=P_stringLiteral(params^.value[k])^.value else
            if pass  ='' then pass  :=P_stringLiteral(params^.value[k])^.value else exit(nil);
          end;
          lt_smallint: begin
            if portRead then exit(nil);
            ftpPort:=P_smallIntLiteral(params^.value[k])^.value;
            portRead:=true;
          end;
          else exit(nil);
        end;
      end;
      if user  ='' then user:=default_user;
      if pass  ='' then pass:=default_pass;
      new(P_ftpConnection(result),create(host,intToStr(ftpPort),user,pass,context,tokenLocation));
    end;
  end;

FUNCTION areValidFtpParameters(CONST L:P_listLiteral; CONST location:T_tokenLocation; CONST context:P_context; OUT FTP:P_ftpConnection; OUT outerResult:P_literal):boolean;
  VAR p0:P_literal;
  begin
    outerResult:=nil;
    FTP:=nil;
    if (L<>nil) and (L^.size>=1) then begin
      p0:=L^.value[0];
      result:=(p0^.literalType=lt_expression) and (P_expressionLiteral(p0)^.typ=et_builtinObject) and (p0^.typeString=FTP_TYPE_STRING);
      if result then begin
        FTP:=P_ftpConnection(p0);
        if FTP^.connection=nil then begin
          result:=false;
          context^.messages^.postTextMessage(mt_el2_warning,location,'The given FTP connection is not valid. Probably an error ocurred during connecting.');
          outerResult:=newVoidLiteral;
          FTP:=nil;
        end else begin
          enterCriticalSection(FTP^.connectionCs);
          if FTP^.enableLogging then FTP^.connection.messages:=context^.messages;
        end;
      end;
    end else exit(false);
  end;

FUNCTION ftp_listing_impl intFuncSignature;
  VAR k:longint;
      FTP:P_ftpConnection;
      tmpMap:P_mapLiteral;
  begin
    result:=nil;
    if areValidFtpParameters(params,tokenLocation,context,FTP,result) and (params^.size=2) and (arg1^.literalType=lt_string) then with FTP^.connection do begin
      result:=recycler^.newListLiteral(0);
      if not list(str1^.value,false) then begin
        context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'FTP directory listing failed with status code '+intToStr(ResultCode));
      end else for k:=0 to FtpList.count -1 do  begin
        tmpMap:=recycler^.newMapLiteral(4)
                  ^.put(recycler,'filename' ,FtpList[k].fileName)
                  ^.put(recycler,'directory',FtpList[k].directory)
                  ^.put(recycler,'size'     ,FtpList[k].filesize)
                  ^.put(recycler,'time'     ,FtpList[k].FileTime);
        listResult^.append(recycler,tmpMap,false);
      end;
      leaveCriticalSection(FTP^.connectionCs);
    end;
  end;

FUNCTION ftp_download_impl intFuncSignature;
  VAR FTP:P_ftpConnection;
      success:boolean;
  begin
    result:=nil;
    if areValidFtpParameters(params,tokenLocation,context,FTP,result) and (params^.size=3) and (arg1^.literalType=lt_string) and (arg2^.literalType=lt_string) then with FTP^.connection do begin
      DirectFileName := str2^.value;
      DirectFile:=true;
      ensurePath(str2^.value);
      success:=RetrieveFile(str1^.value, false);
      result := newBoolLiteral(success);
      if not(success) then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'FTP request failed with status code '+intToStr(ResultCode));
      leaveCriticalSection(FTP^.connectionCs);
    end;
  end;

FUNCTION ftp_upload_impl intFuncSignature;
  VAR FTP:P_ftpConnection;
      success:boolean;
      fullPath, nameOnly, directory: string;
  begin
    result:=nil;
    if areValidFtpParameters(params,tokenLocation,context,FTP,result) and (params^.size=3) and (arg1^.literalType=lt_string) and (arg2^.literalType=lt_string) then with FTP^.connection do begin
      fullPath:=str2^.value;
      nameOnly:=extractFileName(fullPath);
      directory:=ExtractFileDir (fullPath);
      DirectFileName := str1^.value;
      DirectFile:=true;
      success:=ChangeWorkingDir(directory) and StoreFile(nameOnly, false);
      result := newBoolLiteral(success);
      if not(success) then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'FTP request failed with status code '+intToStr(ResultCode));
      leaveCriticalSection(FTP^.connectionCs);
    end;
  end;

FUNCTION ftp_makeDir_impl intFuncSignature;
  VAR FTP:P_ftpConnection;
      success:boolean;
  begin
    result:=nil;
    if areValidFtpParameters(params,tokenLocation,context,FTP,result) and (params^.size=2) and (arg1^.literalType=lt_string) then with FTP^.connection do begin
      success := CreateDir(str1^.value);
      result := newBoolLiteral(success);
      if not(success) then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'FTP request failed with status code '+intToStr(ResultCode));
      leaveCriticalSection(FTP^.connectionCs);
    end;
  end;

FUNCTION ftp_deleteDir_impl intFuncSignature;
  VAR FTP:P_ftpConnection;
      success:boolean;
      nameOnly, directory: string;
  begin
    result:=nil;
    if areValidFtpParameters(params,tokenLocation,context,FTP,result) and (params^.size=2) and (arg1^.literalType=lt_string) then with FTP^.connection do begin
      nameOnly:=extractFileName(str1^.value);
      directory:=ExtractFileDir(str1^.value);
      success:=ChangeWorkingDir(directory) and DeleteDir(nameOnly);
      result := newBoolLiteral(success);
      if not(success) then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'FTP request failed with status code '+intToStr(ResultCode));
      leaveCriticalSection(FTP^.connectionCs);
    end;
  end;

FUNCTION ftp_deleteFile_impl intFuncSignature;
  VAR FTP:P_ftpConnection;
      success:boolean;
      nameOnly, directory: string;
  begin
    result:=nil;
    if areValidFtpParameters(params,tokenLocation,context,FTP,result) and (params^.size=2) and (arg1^.literalType=lt_string) then with FTP^.connection do begin
      nameOnly:=extractFileName(str1^.value);
      directory:=ExtractFileDir(str1^.value);
      success:=ChangeWorkingDir(directory) and DeleteFile(nameOnly);
      result := newBoolLiteral(success);
      if not(success) then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'FTP request failed with status code '+intToStr(ResultCode));
      leaveCriticalSection(FTP^.connectionCs);
    end;
  end;

FUNCTION ftp_setTimeout_impl intFuncSignature;
  VAR FTP:P_ftpConnection;
  begin
    if areValidFtpParameters(params,tokenLocation,context,FTP,result) and (params^.size=2) and (arg1^.literalType=lt_smallint) then with FTP^.connection do begin
      timeout:=int1^.intValue;
      result:=arg0^.rereferenced;
      leaveCriticalSection(FTP^.connectionCs);
    end;
  end;

FUNCTION ftp_setLogging_impl intFuncSignature;
  VAR FTP:P_ftpConnection;
  begin
    if areValidFtpParameters(params,tokenLocation,context,FTP,result) and (params^.size=2) and (arg1^.literalType=lt_boolean) then with FTP^ do begin
      enableLogging:=bool1^.value;
      result:=arg0^.rereferenced;
      leaveCriticalSection(connectionCs);
    end;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(FTP_NAMESPACE,'ftpConnect'   ,@ftp_connect_impl   ,ak_variadic_1);
  builtinFunctionMap.registerRule(FTP_NAMESPACE,'getListing'   ,@ftp_listing_impl   ,ak_binary);
  builtinFunctionMap.registerRule(FTP_NAMESPACE,'ftpDownload'  ,@ftp_download_impl  ,ak_ternary);
  builtinFunctionMap.registerRule(FTP_NAMESPACE,'ftpUpload'    ,@ftp_upload_impl    ,ak_ternary);
  builtinFunctionMap.registerRule(FTP_NAMESPACE,'ftpMakeDir'   ,@ftp_makeDir_impl   ,ak_binary);
  builtinFunctionMap.registerRule(FTP_NAMESPACE,'ftpDeleteDir' ,@ftp_deleteDir_impl ,ak_binary);
  builtinFunctionMap.registerRule(FTP_NAMESPACE,'ftpDeleteFile',@ftp_deleteFile_impl,ak_binary);
  builtinFunctionMap.registerRule(FTP_NAMESPACE,'setTimeout'   ,@ftp_setTimeout_impl,ak_binary);
  builtinFunctionMap.registerRule(FTP_NAMESPACE,'setLogging'   ,@ftp_setLogging_impl,ak_binary);

end.

