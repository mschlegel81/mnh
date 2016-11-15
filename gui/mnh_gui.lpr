{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$ifdef UNIX} cthreads, {$else}
  {$ifdef debugMode}heaptrc,{$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mnh_gui_main,mnh_gui_settings,closeDialog,askDialog,mnh_plotForm, newCentralPackageDialog, mnh_tables, //actual Forms
  mnh_cmdLineInterpretation, mnh_basicTypes, mnh_funcs_files,
  mnh_funcs_server, mnh_packages, mnh_settings, openDemoDialog, mySys, mnh_gui_outputOnly;

{$R *.res}

begin
  if wantMainLoopAfterParseCmdLine then begin
    hideConsole;

    Application.title:='MNH5 - GUI';
    RequireDerivedFormResource := true;
    Application.initialize;
    if reEvaluationWithGUIrequired
    then begin
      Application.CreateForm(ToutputOnlyForm, outputOnlyForm);
      outputOnlyForm.Hide;
    end
    else Application.CreateForm(TMnhForm, MnhForm);
    Application.run;
    showConsole;
  end;
end.
