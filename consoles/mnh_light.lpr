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
      commandLine.mnhExecutionOptions.flags-=[clf_PAUSE_ALWAYS,clf_PAUSE_ON_ERR];
      if fileExists(settings.fullFlavourLocation)
      then runDetachedCommand(settings.fullFlavourLocation,myCommandLineParameters)
      else begin
        writeln('Delegate to full version is required but file "',settings.fullFlavourLocation,'" is invalid');
        writeln('Reinstall to fix this problem');
        ExitCode:=5;
      end;
    end else displayHelp(nil);
  end;
  memoryCleaner.stop;
  commandLine.pauseIfConfigured(ExitCode<>0);
end.

