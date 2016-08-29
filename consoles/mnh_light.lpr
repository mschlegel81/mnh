{$ifdef WINDOWS}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_light;
USES {$ifdef UNIX}cmem, cthreads,{$else}
     {$ifdef DEBUGMODE}heaptrc,{$endif}{$endif}
     mnh_constants,myGenerics,mnh_cmdLineInterpretation, mnh_packages, mnh_contexts, sysutils,mnh_out_adapters;

PROCEDURE interactiveMode;
  VAR hasExitSignal:boolean=false;
      consolePackage:T_package;
  PROCEDURE readInputFromConsole;
    VAR nextInput:ansistring;
    begin
      write('>'); readln(nextInput);
      nextInput:=trim(nextInput);
      if uppercase(nextInput)='EXIT' then begin
        hasExitSignal:=true;
        exit;
      end else if nextInput='\' then consolePackage.clearSource
      else consolePackage.appendSource(nextInput);
    end;

  VAR i:longint;
      context:T_evaluationContext;
  begin
    consolePackage.create(nil);
    for i:=0 to length(LOGO)-1 do writeln(LOGO[i]);
    writeln;
    writeln('No command line parameters were given. You are in interactive mode.');
    writeln('Type "exit" (case insensitive) to quit.');
    writeln('Type \ to clear and restart.');
    context.createNormalContext(P_adapters(@consoleAdapters));

    readInputFromConsole;
    while not(hasExitSignal) do begin
      consolePackage.load(lu_interactiveMode,context,C_EMPTY_STRING_ARRAY);
      readInputFromConsole;
    end;
    context.destroy;
    consolePackage.destroy;
  end;

begin
  if wantMainLoopAfterParseCmdLine then interactiveMode;
end.

