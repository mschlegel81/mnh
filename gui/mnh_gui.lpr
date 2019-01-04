{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$ifdef UNIX} cthreads,{$endif}
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms,
  mySys,
  cmdLineInterpretation,
  ipcModel,
  mnh_gui_main,
  mnh_gui_outputOnly,
  mnh_plotForm;

{$R *.res}

begin
  cmdLineInterpretation.plotAdapters:=@mnh_plotForm.plotSystem;
  if wantMainLoopAfterParseCmdLine then begin
    {$ifndef debugMode}
    hideConsole;
    {$endif}
    Application.title:='MNH5 - GUI';
    RequireDerivedFormResource := true;
    Application.initialize;
    if reEvaluationWithGUIrequired
    then Application.CreateForm(ToutputOnlyForm, outputOnlyForm)
    else if sendParametersToOtherInstance(filesToOpenInEditor)
    then halt
    else Application.CreateForm(TMnhForm, MnhForm);
    Application.run;
    showConsole;
    if pauseAtEnd or pauseOnError and ((ExitCode<>0) or profilingRun) then readln;
  end;
end.
