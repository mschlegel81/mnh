{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
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
  saveFile; //needed for proper initialization

{$R *.res}

begin
  Application.title:='MNH5 - GUI';
  RequireDerivedFormResource := true;
  Application.initialize;
  SetDefaultLang('en');
  if wantMainLoopAfterParseCmdLine then begin
    {$ifndef debugMode}
    hideConsole;
    {$endif}
    initAskForm;
    if commandLine.reEvaluationWithGUIrequired
    then Application.CreateForm(TreevaluationForm,reevaluationForm)
    else if sendParametersToOtherInstance(commandLine.filesToOpenInEditor)
    then halt
    else Application.CreateForm(TIdeMainForm, IdeMainForm);
    Application.run;
    memoryCleaner.stop;
    showConsole;
    if commandLine.pauseAtEnd or commandLine.pauseOnError then pauseOnce;
  end;
end.

