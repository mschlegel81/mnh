{$ifdef windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$IFDEF UNIX} cthreads, {$else}
  {$ifdef DEBUGMODE}heaptrc,{$endif}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mnh_gui_main,
  mySys, mnh_cmdLineInterpretation,
  mnh_funcs_server, mnh_packages, mnh_settings;

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
