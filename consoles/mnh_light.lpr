{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_light;
USES {$ifdef UNIX}cthreads,{$endif}
     sysutils, mySys, mnh_settings,
     cmdLineInterpretation,
     commandLineParameters;

{$R *.res}

begin
  if wantMainLoopAfterParseCmdLine then begin
    if commandLine.reEvaluationWithGUIrequired
    then begin
      commandLine.pauseAtEnd  :=false;
      commandLine.pauseOnError:=false;
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
  if commandLine.pauseAtEnd then pauseOnce;
end.

