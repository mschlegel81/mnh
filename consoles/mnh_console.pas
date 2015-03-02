PROGRAM mnh_console;
USES mnh_tokens, mnh_out_adapters, mnh_constants, mnh_fileWrappers,sysutils;
VAR input_adapter:P_directInputWrapper;
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
 
PROCEDURE setup;
  begin
    writeln('MNH (V5) console; by Martin Schlegel');
    writeln;
    writeln('compiled on: ',{$I %DATE%});
    writeln('         at: ',{$I %TIME%});
    writeln('FPC version: ',{$I %FPCVERSION%});
    writeln('Target CPU : ',{$I %FPCTARGET%});  
    writeln;    
    mnh_out_adapters.inputDeclEcho:=@inputDeclEcho; 
    mnh_out_adapters.inputExprEcho:=@inputExprEcho;
    mnh_out_adapters.exprOut      :=@exprOut;   
  end;

PROCEDURE interactiveMode;  
  VAR time:double;
  begin
    writeln('No command line parameters were given. You are in interactive mode.');
    writeln('Type "exit" to quit.');
    new(input_adapter,create);
    initMainPackage(input_adapter);
    write('>'); readln(nextInput);
    while (uppercase(trim(nextInput))<>'EXIT') do begin
      input_adapter^.setInput(nextInput);
      time:=now;
      if isReloadOfAllPackagesIndicated then reloadAllPackages
                                        else reloadMainPackage;
      writeln('time: ',(now-time)*24*60*60:0:3,'sec');
      write('>'); readln(nextInput);
    end;
  end;

PROCEDURE fileMode;
  VAR i:longint;
      time:double;
  begin
    for i:=1 to paramCount do begin
      time:=now;
      initMainPackage(paramStr(i));
      reloadMainPackage;
      writeln('time: ',(now-time)*24*60*60:0:3,'sec');
      clearAllPackages;
    end;
  end;

begin
  setup;
  if paramCount=0 then interactiveMode
                  else fileMode;
end.