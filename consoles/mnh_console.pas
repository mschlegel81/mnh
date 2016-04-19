{$MAXSTACKSIZE 100000000}
PROGRAM mnh_console;
USES mnh_cmdLineInterpretation, mnh_packages, mnh_contexts, sysutils, mnh_constants, mnh_out_adapters;

PROCEDURE interactiveMode;
  VAR hasExitSignal:boolean=false;
  PROCEDURE readInputFromConsole;
    VAR nextInput:ansistring;
    begin
      write('>'); readln(nextInput);
      nextInput:=trim(nextInput);
      if uppercase(nextInput)='exit' then begin
        hasExitSignal:=true;
        exit;
      end else if nextInput='\' then environment.mainPackageProvider^.clear
      else
      environment.mainPackageProvider^.appendLine(nextInput);
    end;

  VAR i:longint;
      context:T_evaluationContext;
  begin
    for i:=0 to length(LOGO)-1 do writeln(LOGO[i]);
    writeln;
    writeln('No command line parameters were given. You are in interactive mode.');
    writeln('Type "exit" (case insensitive) to quit.');
    writeln('Type \ to clear and restart.');
    context.createNormalContext(P_adapters(@consoleAdapters));

    readInputFromConsole;
    while not(hasExitSignal) do begin
      reloadMainPackage(lu_interactiveMode,context);
      readInputFromConsole;
    end;
    context.destroy;
  end;

begin
  parseCmdLine;
  interactiveMode;
end.
