PROGRAM mnh_light;
USES {$ifdef UNIX}cthreads,{$endif}
     sysutils, mySys, mnh_settings,
     cmdLineInterpretation,
     commandLineParameters,
     substitute_funcs;

{$R *.res}

begin
  if wantMainLoopAfterParseCmdLine then begin
    if clf_GUI in commandLine.mnhExecutionOptions.flags
    then begin
      beep;
      writeln(#27'[91;5mThis script cannot be run in light version'#27'[0m');
      ExitCode:=5;
    end else displayHelp(nil);
  end;
  commandLine.pauseIfConfigured(ExitCode<>0);
end.

