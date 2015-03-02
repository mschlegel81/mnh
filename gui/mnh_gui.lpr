program mnh_gui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mnh_gui_settings, mnh_gui_main, mnh_plots;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  //Application.CreateForm(TplotForm, plotForm);
  Application.CreateForm(TMnhForm, MnhForm);
  Application.CreateForm(TSettingsForm, SettingsForm);

  Application.Run;
end.

