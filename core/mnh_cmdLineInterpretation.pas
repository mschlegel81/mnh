UNIT mnh_cmdLineInterpretation;
INTERFACE
USES mnh_constants,mnh_out_adapters,mnh_funcs,consoleAsk{$ifdef plots},mnh_plotData{$endif},mnh_tokens,mnh_tokLoc,mnh_stringutil,sysutils;
PROCEDURE parseCmdLine;
IMPLEMENTATION
//by command line parameters:---------------
VAR minErrorLevel:T_errorLevel=el2_warning;
    fileToInterpret:ansistring='';
    parameters:array of ansistring;
    wantHelpDisplay:boolean=false;
//---------------:by command line parameters

PROCEDURE inputDeclEcho(CONST s:ansistring); begin writeln('in_>',s); end;
PROCEDURE inputExprEcho(CONST s:ansistring); begin writeln('in >',s); end;
PROCEDURE exprOut      (CONST s:ansistring); begin writeln('OUT>',s); end;
PROCEDURE filteredStdErrOut(CONST error:T_storedError);
  begin
    with error do if errorLevel>=minErrorLevel then writeln(stdErr,C_errorLevelTxt[errorLevel],errorMessage,' @',ansistring(errorLocation));
  end;

PROCEDURE parseCmdLine;
  PROCEDURE displayHelp;
    begin
      writeln('MNH (V5) console; by Martin Schlegel');
      writeln('compiled on: ',{$I %DATE%});
      writeln('         at: ',{$I %TIME%});
      writeln('FPC version: ',{$I %FPCVERSION%});
      writeln('Target CPU : ',{$I %FPCTARGET%});
      writeln;
      writeln('Accepted parameters: ');
      writeln('  [-h] [+echo/-echo] [-el#] [filename [parameters]]');
      writeln('  filename: if present the file is interpreted; parameters are passed if present');
      writeln('            if not present, interactive mode is entered');
      writeln('  +echo: force echo on (default for interactive mode)');
      writeln('  -echo: force echo off (default for interpretation mode)');
      writeln('  -el# : set minimum error level for output; valid values: [0..5], default=2');
      writeln('  -h   : display this help and quit');
      writeln('  -det : force deterministic "random" numbers');    
    end;

  PROCEDURE fileMode;
    begin
      mainThread:=threadId;
      mainPackageProvider.setPath(fileToInterpret);
      if wantHelpDisplay then begin
        displayHelp;
        printMainPackageDocText;
        halt;
      end;
      callMainInMain(parameters);
      halt;
    end;

  VAR echo:(e_forcedOn,e_default,e_forcedOff)=e_default;
      i,pel:longint;
  begin
    setLength(parameters,0);
    for i:=1 to paramCount do begin
      if      paramstr(i)='+echo' then echo:=e_forcedOn
      else if paramstr(i)='-echo' then echo:=e_forcedOff
      else if paramstr(i)='-det'  then randseed:=0
      else if startsWith(paramStr(i),'-h') then wantHelpDisplay:=true
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
          halt;
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
    if fileToInterpret<>'' then fileMode;
    if wantHelpDisplay then begin
      displayHelp;      
      halt;
    end;
  end;

end.