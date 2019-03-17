PROGRAM ide;

{$mode objfpc}{$H+}
{$apptype console} // to read debug output on windows

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, ideMain,
  mnh_gui_settings, mnh_plotForm, codeAssistance,
  guiOutAdapters, editorMetaBase, editorMeta, fileWrappers, contexts,
  out_adapters, packages, mnh_constants, mnh_settings,cmdLineInterpretation,
  ipcModel,mySys;

{$R *.res}

begin
  cmdLineInterpretation.plotAdapters:=@mnh_plotForm.plotSystem;
  Application.title:='MNH5 - GUI';
  RequireDerivedFormResource := true;
  Application.initialize;
  if wantMainLoopAfterParseCmdLine then begin
    {$ifndef debugMode}
    hideConsole;
    {$endif}
    if reEvaluationWithGUIrequired
    then halt //Application.CreateForm(ToutputOnlyForm, outputOnlyForm)
    else if sendParametersToOtherInstance(filesToOpenInEditor)
    then halt
    else Application.CreateForm(TIdeMainForm, IdeMainForm);
    Application.run;
    showConsole;
    if pauseAtEnd or pauseOnError and ((ExitCode<>0) or profilingRun) then pauseOnce;
  end;
end.

