{$ifdef windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$IFDEF UNIX} cthreads, cmem,{$else}
  {$ifdef DEB_UGMODE}heaptrc,{$endif}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mnh_gui_main,mnh_gui_settings,closeDialog,askDialog,mnh_plotForm, newCentralPackageDialog, mnh_tables, //actual Forms
  mySys, mnh_cmdLineInterpretation, openDemoDialog;

{$R *.res}

begin
  if wantMainLoopAfterParseCmdLine then begin
    hideConsole;

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
    Application.CreateForm(TopenDemoDialogForm, openDemoDialogForm);
    Application.run;
    mnh_gui_main.doFinalization;
    showConsole;
  end;
end.
