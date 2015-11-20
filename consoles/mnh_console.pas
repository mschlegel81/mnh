{$MAXSTACKSIZE 100000000}
PROGRAM mnh_console;
USES mnh_cmdLineInterpretation, mnh_tokens, sysutils, mnh_constants, mnh_out_adapters;

PROCEDURE interactiveMode;
  VAR hasExitSignal:boolean=false;
  PROCEDURE readInputFromConsole;
    VAR nextInput:ansistring;
    begin
      environment.mainPackageProvider^.clear;
      repeat
        write('>'); readln(nextInput);
        nextInput:=trim(nextInput);
        if uppercase(nextInput)='EXIT' then begin
          hasExitSignal:=true;
          exit;
        end;
        if (length(nextInput)>0) and (nextInput[length(nextInput)]='\') then begin
          environment.mainPackageProvider^.appendLine(copy(nextInput,1,length(nextInput)-1));
        end else begin
          environment.mainPackageProvider^.appendLine(nextInput);
          exit;
        end;
      until false;
    end;
  VAR i:longint;
      context:T_evaluationContext;
  begin
    for i:=0 to length(LOGO)-1 do writeln(LOGO[i]);
    writeln;
    writeln('No command line parameters were given. You are in interactive mode.');
    writeln('Type "exit" to quit.');
    writeln('end a line with a \ to continue the input.');
    context.createNormalContext(P_adapters(@consoleAdapters));

    readInputFromConsole;
    while not(hasExitSignal) do begin
      reloadMainPackage(lu_forDirectExecution,context);
      readInputFromConsole;
    end;
    context.destroy;
  end;

begin
  parseCmdLine;
  interactiveMode;
end.
