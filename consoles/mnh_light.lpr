{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_light;
USES {$ifdef UNIX}cthreads,{$endif}
     mnh_cmdLineInterpretation;

{$R *.res}

begin
  if wantMainLoopAfterParseCmdLine then displayHelp;
end.

