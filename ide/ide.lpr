PROGRAM ide;

{$mode objfpc}{$H+}
{apptype console} // to read debug output on windows

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Forms, Interfaces
  { you can add units after this }, ideMain,
  mnh_plotForm,
  cmdLineInterpretation,
  ipcModel,
  mySys,
  saveFile; //needed for proper initialization

{$R *.res}

begin
  Application.title:='MNH5 - GUI';
  RequireDerivedFormResource := true;
  Application.initialize;
  if wantMainLoopAfterParseCmdLine then begin
    {$ifndef debugMode}
    hideConsole;
    {$endif}
    if reEvaluationWithGUIrequired
    then halt //TODO: Implement output only form...
    else if sendParametersToOtherInstance(filesToOpenInEditor)
    then halt
    else Application.CreateForm(TIdeMainForm, IdeMainForm);
    Application.run;
    showConsole;
    if pauseAtEnd or pauseOnError and ((ExitCode<>0) or profilingRun) then pauseOnce;
  end;
end.

