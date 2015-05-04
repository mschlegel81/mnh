{$MAXSTACKSIZE 100000000}
PROGRAM mnh_console;
USES mnh_cmdLineInterpretation, mnh_tokens, sysutils;

PROCEDURE interactiveMode;
  VAR time:double;
      hasExitSignal:boolean=false;
  PROCEDURE readInputFromConsole;
    VAR nextInput:ansistring;
    begin
      mainPackageProvider.clear;
      repeat
        write('>'); readln(nextInput);
        nextInput:=trim(nextInput);
        if uppercase(nextInput)='EXIT' then begin
          hasExitSignal:=true;
          exit;
        end;
        if (length(nextInput)>0) and (nextInput[length(nextInput)]='\') then begin
          mainPackageProvider.appendLine(copy(nextInput,1,length(nextInput)-1));
        end else begin
          mainPackageProvider.appendLine(nextInput);
          exit;
        end;
      until false;
    end;

  begin
    writeln;
    writeln('No command line parameters were given. You are in interactive mode.');
    writeln('Type "exit" to quit.');
    writeln('end a line with a \ to continue the input.');

    readInputFromConsole;
    while not(hasExitSignal) do begin
      time:=now;
      reloadMainPackage(lu_forDirectExecution);
      if displayTime then writeln('time: ',(now-time)*24*60*60:0:3,'sec');
      readInputFromConsole;
    end;
  end;

begin
  parseCmdLine;
  interactiveMode;
  {$ifdef debugMode} writeln(stdErr,'mnh_console.pas - quit'); {$endif}
end.
