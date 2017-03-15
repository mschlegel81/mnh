UNIT mnh_funcs_ipc;
INTERFACE
USES mnh_constants, mnh_basicTypes,
     mnh_litVar,
     mnh_contexts,
     mnh_funcs,simpleipc;
IMPLEMENTATION
{$i mnh_func_defines.inc}
VAR ipcClient:TSimpleIPCClient;
    ipcServer:array of TSimpleIPCServer;
    ipcCs:TRTLCriticalSection;

FUNCTION isSingleInstance_imp intFuncSignature;
  VAR client:TSimpleIPCClient;
  begin
    client:=TSimpleIPCClient.create(nil);
  end;

INITIALIZATION
  InitCriticalSection(ipcCs);
FINALIZATION
  DoneCriticalsection(ipcCs);
end.
