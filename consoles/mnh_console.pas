{$MAXSTACKSIZE 100000000}
PROGRAM mnh_console;
USES mnh_tokens, mnh_out_adapters, mnh_constants, mnh_fileWrappers,sysutils, mnh_stringutil, mnh_tokLoc, mnh_funcs{$ifdef plots}, mnh_plotData{$endif};

//by command line parameters:---------------
VAR minErrorLevel:T_errorLevel=el2_warning;
    fileToInterpret:ansistring='';
    parameters:array of ansistring;
//---------------:by command line parameters

PROCEDURE inputDeclEcho(CONST s:ansistring); begin writeln('in_>',s); end;
PROCEDURE inputExprEcho(CONST s:ansistring); begin writeln('in >',s); end;
PROCEDURE exprOut      (CONST s:ansistring); begin writeln('out>',s); end;
PROCEDURE filteredStdErrOut(CONST error:T_storedError);
  begin
    with error do if errorLevel>=minErrorLevel then writeln(stdErr,C_errorLevelTxt[errorLevel],errorMessage,' @',ansistring(errorLocation));
  end;
  
PROCEDURE displayVersionInfo;
  begin
    writeln('MNH (V5) console; by Martin Schlegel');
    writeln('compiled on: ',{$I %DATE%});
    writeln('         at: ',{$I %TIME%});
    writeln('FPC version: ',{$I %FPCVERSION%});
    writeln('Target CPU : ',{$I %FPCTARGET%});    
  end;
  
PROCEDURE parseCmdLine;
  PROCEDURE displayHelp;
    begin
      displayVersionInfo;
      writeln('Accepted parameters: ');
      writeln('  [-h] [+echo/-echo] [-el#] [filename [parameters]]');
      writeln('  filename: if present the file is interpreted; parameters are passed if present');
      writeln('            if not present, interactive mode is entered');
      writeln('  +echo: force echo on (default for interactive mode)');
      writeln('  -echo: force echo off (default for interpretation mode)');
      writeln('  -el# : set minimum error level for output; valid values: [0..5], default=2');
      writeln('  -h   : display this help and quit');
      halt;
    end;
    
  VAR echo:(e_forcedOn,e_default,e_forcedOff)=e_default;
      i,pel:longint;      
  begin
    setLength(parameters,0);
    for i:=1 to paramCount do begin
      if      paramstr(i)='+echo' then echo:=e_forcedOn
      else if paramstr(i)='-echo' then echo:=e_forcedOff
      else if startsWith(paramStr(i),'-h') then displayHelp
      else if startsWith(paramStr(i),'-el') then begin
        pel:=strToIntDef(copy(paramstr(i),4,length(paramstr(i))-3),-1);
        if (pel<0) or (pel>ord(el5_systemError)) then begin
          writeln('Invalid minimum error level given!');
          writeln('Parameter: ',paramStr(i),'; extracted level: ',copy(paramstr(i),4,length(paramstr(i))-3));
          writeln('Allowed values: 0, 1, 2, 3, 4, 5');
          halt;
        end else minErrorLevel:=T_errorLevel(pel);
      end else if fileToInterpret='' then begin
        if fileExists(paramstr(i)) then fileToInterpret:=paramStr(i) else begin
          writeln('Invalid filename given!');
          writeln('Parameter: ',paramStr(i));
          writeln('File does not exist.');
        end;
      end else begin
        setLength(parameters,length(parameters)+1);
        parameters[length(parameters)-1]:=paramStr(i);
      end;    
    end;
    //-----------------------------------------------------
    if (echo=e_forcedOn) or (echo=e_default) and (fileToInterpret='') then begin
      mnh_out_adapters.inputDeclEcho:=@inputDeclEcho; 
      mnh_out_adapters.inputExprEcho:=@inputExprEcho;
      mnh_out_adapters.exprOut      :=@exprOut;             
    end else begin
      mnh_out_adapters.inputDeclEcho:=nil; 
      mnh_out_adapters.inputExprEcho:=nil;
      mnh_out_adapters.exprOut      :=nil;      
    end;
    mnh_out_adapters.errorOut:=@filteredStdErrOut;
  end;

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
        if nextInput[length(nextInput)]='\' then begin
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
    
    readInputFromConsole;
    while not(hasExitSignal) do begin      
      time:=now;
      reloadMainPackage(true);
      writeln('time: ',(now-time)*24*60*60:0:3,'sec');
      readInputFromConsole;
    end;
  end;

PROCEDURE fileMode;
  begin
    mainPackageProvider.setPath(fileToInterpret);
    reloadMainPackage(false);    
    callMainInMain(parameters);
  end;
  
begin
  mnh_funcs.mnh_console_executable:=paramstr(0);
  parseCmdLine;
  if fileToInterpret='' then interactiveMode
                        else fileMode;
end.