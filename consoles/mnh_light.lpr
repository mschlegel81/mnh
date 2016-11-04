{$ifdef Windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_light;
USES {$ifdef UNIX}cmem, cthreads,{$else}
     {$ifdef debugMode}heaptrc,{$endif}{$endif}
     mnh_cmdLineInterpretation;

begin
  if wantMainLoopAfterParseCmdLine then displayHelp;
end.

