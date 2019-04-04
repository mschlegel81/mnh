UNIT ipcModel;
INTERFACE
USES sysutils,myGenerics,myStringUtil,Forms,simpleipc;
CONST UNIQUE_EDITOR_IPC_ID='MNH5-editingGuiInstance';

PROCEDURE initIpcServer(CONST mainForm:TForm);
FUNCTION getFilesToOpen:T_arrayOfString;
FUNCTION sendParametersToOtherInstance(CONST parameters:T_arrayOfString):boolean;
IMPLEMENTATION
VAR uniqueEditorInstanceIpcServer:TSimpleIPCServer;
PROCEDURE initIpcServer(CONST mainForm:TForm);
  begin
    uniqueEditorInstanceIpcServer:=TSimpleIPCServer.create(mainForm);
    uniqueEditorInstanceIpcServer.serverId:=UNIQUE_EDITOR_IPC_ID;
    uniqueEditorInstanceIpcServer.global:=true;
    uniqueEditorInstanceIpcServer.StartServer;
  end;

FUNCTION getFilesToOpen:T_arrayOfString;
  begin
    if uniqueEditorInstanceIpcServer.PeekMessage(1,true)
    then result:=split(uniqueEditorInstanceIpcServer.StringMessage,C_lineBreakChar)
    else result:=C_EMPTY_STRING_ARRAY;
  end;

FUNCTION sendParametersToOtherInstance(CONST parameters:T_arrayOfString):boolean;
  VAR client:TSimpleIPCClient;
      i:longint;
  begin
    client:=TSimpleIPCClient.create(nil);
    client.serverId:=UNIQUE_EDITOR_IPC_ID;
    if client.ServerRunning then begin
      result:=true;
      if length(parameters)>0 then begin
        client.active:=true;
        {$WARN 5092 OFF}
        for i:=0 to length(parameters)-1 do parameters[i]:=expandFileName(parameters[i]);
        client.SendStringMessage(join(parameters,C_lineBreakChar));
      end;
    end else result:=false;
    client.free;
  end;

end.

