{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$ifdef UNIX} cthreads, {$else}
  {$ifdef debugMode}heaptrc,{$endif}{$endif}
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms,
  simpleipc,
  myStringUtil,
  mySys,
  mnh_cmdLineInterpretation,
  mnh_gui_main,
  mnh_gui_outputOnly;

FUNCTION sendParametersToOtherInstance:boolean;
  VAR client:TSimpleIPCClient;
      i:longint;
  begin
    client:=TSimpleIPCClient.create(nil);
    client.serverId:=UNIQUE_EDITOR_IPC_ID;
    if client.ServerRunning then begin
      result:=true;
      if length(filesToOpenInEditor)>0 then begin
        client.active:=true;
        for i:=0 to length(filesToOpenInEditor)-1 do filesToOpenInEditor[i]:=expandFileName(filesToOpenInEditor[i]);
        client.SendStringMessage(join(filesToOpenInEditor,C_lineBreakChar));
      end;
    end else result:=false;
    client.free;
  end;

{$R *.res}

begin
  if wantMainLoopAfterParseCmdLine then begin
    hideConsole;

    Application.title:='MNH5 - GUI';
    RequireDerivedFormResource := true;
    Application.initialize;
    if reEvaluationWithGUIrequired
    then Application.CreateForm(ToutputOnlyForm, outputOnlyForm)
    else if sendParametersToOtherInstance then begin
      showConsole;
      halt;
    end else Application.CreateForm(TMnhForm, MnhForm);
    Application.run;
    showConsole;
  end;
end.
