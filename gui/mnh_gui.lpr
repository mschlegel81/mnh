{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$ifdef UNIX} cthreads,{$endif}
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms,
  mySys,
  mnh_cmdLineInterpretation,
  ipcModel,
  mnh_gui_main,
  mnh_gui_outputOnly,
  mnh_plotForm, openFile;

{$R *.res}

begin
  mnh_cmdLineInterpretation.plotAdapters:=@mnh_plotForm.plotSystem;
  if wantMainLoopAfterParseCmdLine then begin
    hideConsole;
    Application.title:='MNH5 - GUI';
    RequireDerivedFormResource := true;
    Application.initialize;
    if reEvaluationWithGUIrequired
    then Application.CreateForm(ToutputOnlyForm, outputOnlyForm)
    else if sendParametersToOtherInstance(filesToOpenInEditor) then begin
      showConsole;
      halt;
    end else Application.CreateForm(TMnhForm, MnhForm);
  Application.CreateForm(TopenFileDialog, openFileDialog);
    Application.run;
    showConsole;
  end;
end.
