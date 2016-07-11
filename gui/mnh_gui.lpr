{$ifdef windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$IFDEF UNIX} cthreads, cmem,{$ENDIF}
  {$ifdef DEBUGMODE}heaptrc,{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, mnh_gui_settings, mnh_gui_main, closeDialog, askDialog,
  mnh_cmdLineInterpretation, mnh_funcs, mnh_funcs_list,
  mnh_funcs_math, mnh_funcs_mnh, mnh_funcs_regex, mnh_funcs_strings,
  mnh_funcs_system, mnh_litVar, mnh_packages, mnh_out_adapters,
  consoleAsk, mnh_constants, mnh_doc, mnh_html,
  SynHighlighterMnh, mnh_evalThread, mySys,
  mnh_plotData,mnh_plotFuncs, mnh_plotForm, newCentralPackageDialog, mnh_tables;

{$R *.res}

begin
  parseCmdLine;
  {$ifndef imig}hideConsole;{$endif}

  mnh_gui_main.lateInitialization;
  Application.title:='MNH5 - GUI';
  RequireDerivedFormResource := true;
  Application.initialize;

  Application.CreateForm(TMnhForm, MnhForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TcloseDialogForm, closeDialogForm);
  Application.CreateForm(TaskForm, askForm);
  Application.CreateForm(TplotForm, plotForm);
  Application.CreateForm(TnewCentralPackageForm, newCentralPackageForm);
  Application.CreateForm(TtableForm, tableForm);
  Application.run;
  showConsole;
end.
