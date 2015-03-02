PROGRAM mnh_console;
USES mnh_tokens, mnh_out_adapters, mnh_constants, fileWrappers,sysutils;
VAR input_adapter:T_directInputWrapper;
    nextInput:ansistring;



PROCEDURE inputDeclEcho(CONST s:ansistring);
  begin
    writeln('dec>',s);
  end;
  
PROCEDURE inputExprEcho(CONST s:ansistring);
  begin
    writeln('in >',s);
  end;

PROCEDURE exprOut(CONST s:ansistring);
  begin
    writeln('out>',s);
  end;
  
PROCEDURE stdOut(CONST s:ansistring);
  begin
    writeln(s);
  end;
  
PROCEDURE errOut(CONST s:ansistring);
  begin
    writeln(s);
  end;

 
CONST my_out_adapter:T_outAdapter=(inputDeclEcho:@inputDeclEcho; 
                                   inputExprEcho:@inputExprEcho;
                                   exprOut      :@exprOut;
                                   errorOut     :@plainStdErrOut;
                                   printOut     :@plainConsoleOut;
                                   tablePrintOut:nil;
                                   plotOut      :nil;
                                   maxErrorLevel:el0_allOkay);
PROCEDURE setup;
  begin
    input_adapter.create;
    outAdapter:=my_out_adapter;
  end;

VAR time:double;
begin
  setup;
  write('>');
  readln(nextInput);
  input_adapter.setInput(nextInput);
  if (uppercase(trim(nextInput))<>'EXIT') then begin
    initMainPackage(@input_adapter);
    write('>'); readln(nextInput);
    while (uppercase(trim(nextInput))<>'EXIT') do begin
      input_adapter.setInput(nextInput);
      time:=now;
      if isReloadOfAllPackagesIndicated then reloadAllPackages
                                        else reloadMainPackage;
      writeln('time: ',(now-time)*24*60*60:0:3,'sec');
      write('>'); readln(nextInput);
    end;
  end;
  input_adapter.destroy;
end.