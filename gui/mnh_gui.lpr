{$MAXSTACKSIZE 100000000}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mnh_gui_settings, mnh_gui_main, closeDialog, askDialog, mnh_cmdLineInterpretation;



{$R *.res}

begin
  parseCmdLine;

  mnh_gui_main.lateInitialization;
  Application.Title:='MNH5 - GUI';
  RequireDerivedFormResource := True;
  Application.Initialize;

  Application.CreateForm(TMnhForm, MnhForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TcloseDialogForm, closeDialogForm);
  Application.CreateForm(TaskForm, askForm);
  Application.Run;
end.
