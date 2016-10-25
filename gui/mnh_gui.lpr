{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$ifdef UNIX} cthreads, {$else}
  {$ifdef debugMode}heaptrc,{$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mnh_gui_main,mnh_gui_settings,closeDialog,askDialog,mnh_plotForm, newCentralPackageDialog, mnh_tables, //actual Forms
  mnh_cmdLineInterpretation, mnh_basicTypes, mnh_funcs_files,
  mnh_funcs_server, mnh_packages, mnh_settings, openDemoDialog, mySys,
mnh_workspaces;

{$R *.res}

begin
  if wantMainLoopAfterParseCmdLine then begin
    hideConsole;

    mnh_gui_main.lateInitialization;
    Application.title:='MNH5 - GUI';
    RequireDerivedFormResource := true;
    Application.initialize;

    Application.CreateForm(TMnhForm, MnhForm);
    Application.run;
    mnh_gui_main.doFinalization;
    showConsole;
  end;
end.
