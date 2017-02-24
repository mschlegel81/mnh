{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$ifdef UNIX} cthreads, {$else}
  {$ifdef debugMode}heaptrc,{$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mySys,
  mnh_cmdLineInterpretation,
  mnh_gui_main,
  mnh_gui_outputOnly;

{$R *.res}

begin
  if wantMainLoopAfterParseCmdLine then begin
    hideConsole;

    Application.title:='MNH5 - GUI';
    RequireDerivedFormResource := true;
    Application.initialize;
    if reEvaluationWithGUIrequired
    then Application.CreateForm(ToutputOnlyForm, outputOnlyForm)
    else Application.CreateForm(TMnhForm, MnhForm);
    Application.run;
    showConsole;
  end;
end.
