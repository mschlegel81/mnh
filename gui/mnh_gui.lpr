{$MAXSTACKSIZE 100000000}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mnh_gui_settings,
  mnh_gui_main;

{$R *.res}

BEGIN
  RequireDerivedFormResource := True;
  Application.Initialize;

  Application.CreateForm(TMnhForm, MnhForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.Run;
END.
