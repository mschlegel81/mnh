{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$ifdef UNIX} cthreads,{$endif}
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms,
  mySys,
  mnh_cmdLineInterpretation,
  {$ifdef imig}
  mnh_imig_form,
  {$endif}
  ipcModel,
  mnh_gui_main,
  mnh_gui_outputOnly,
  mnh_plotForm;

{$R *.res}

begin
  mnh_cmdLineInterpretation.plotAdapters:=@mnh_plotForm.plotSystem;
  {$ifdef imig}
  mnh_cmdLineInterpretation.imigAdapters:=@mnh_imig_form.imigSystem;
  {$endif}
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
  end;
end.
