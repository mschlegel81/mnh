{$ifdef windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$IFDEF UNIX} cthreads, cmem, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mnh_gui_settings, mnh_gui_main, closeDialog, askDialog,
  mnh_cmdLineInterpretation, mnh_tokLoc, mnh_funcs, mnh_funcs_list,
  mnh_funcs_math, mnh_funcs_mnh, mnh_funcs_regex, mnh_funcs_strings,
  mnh_funcs_system, mnh_litVar, mnh_packages, mnh_out_adapters,
  consoleAsk, mnh_constants, mnh_doc, mnh_html,
  SynHighlighterMnh, mnh_evalThread, mySys,
  mnh_plotData,mnh_plotFuncs, mnh_plotForm, newCentralPackageDialog, mnh_tables;

{$R *.res}

begin
  parseCmdLine;
  hideConsole;

  mnh_gui_main.lateInitialization;
  Application.Title:='MNH5 - GUI';
  RequireDerivedFormResource := True;
  Application.Initialize;

  Application.CreateForm(TMnhForm, MnhForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TcloseDialogForm, closeDialogForm);
  Application.CreateForm(TaskForm, askForm);
  Application.CreateForm(TplotForm, plotForm);
  Application.CreateForm(TnewCentralPackageForm, newCentralPackageForm);
  Application.CreateForm(TtableForm, tableForm);
  Application.Run;
  showConsole;
end.
