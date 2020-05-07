{$ifdef Windows}{$MAXSTACKSIZE 10000000}{$endif}
PROGRAM ide;

{$mode objfpc}{$H+}
{$apptype console} // to read debug output on windows

USES
  {$ifdef UNIX}
  cthreads,
  {$endif}
  Forms, Interfaces,
  LCLTranslator,
  idemain, reevaluationForms,
  cmdLineInterpretation,
  ipcModel, mnh_doc,
  askDialog,funcs,mnh_constants, evaluation,
  mySys,
  mnh_settings,
  saveFile; //needed for proper initialization

{$R *.res}

begin
  Application.Scaled:=true;
  Application.title:='MNH5 - GUI';
  RequireDerivedFormResource := true;
  Application.initialize;
  SetDefaultLang('en');
  if wantMainLoopAfterParseCmdLine then begin
    {$ifndef debugMode}
    hideConsole;
    {$endif}
    initAskForm;
    if clf_GUI in commandLine.mnhExecutionOptions.flags
    then Application.CreateForm(TreevaluationForm,reevaluationForm)
    else if sendParametersToOtherInstance(commandLine.filesToOpenInEditor)
    then halt
    else Application.CreateForm(TIdeMainForm, IdeMainForm);
    Application.run;
    memoryCleaner.stop;
    showConsole;
    commandLine.pauseIfConfigured(ExitCode<>0);
  end;
end.

