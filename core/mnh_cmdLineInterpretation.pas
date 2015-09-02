UNIT mnh_cmdLineInterpretation;
INTERFACE
USES mnh_constants,mnh_out_adapters,mnh_funcs,consoleAsk{$ifdef fullVersion},mnh_plotData{$endif},mnh_tokens,mnh_tokLoc,myStringutil,sysutils,myGenerics,
     mnh_doc,lclintf;
PROCEDURE parseCmdLine;
VAR displayTime:boolean=false;
IMPLEMENTATION
//by command line parameters:---------------
VAR fileToInterpret:ansistring='';
    parameters:T_arrayOfString;
    wantHelpDisplay:boolean=false;
//---------------:by command line parameters

PROCEDURE parseCmdLine;
  PROCEDURE makeAndShowDoc;
    begin
      findAndDocumentAllPackages;
      OpenURL('file:///'+replaceAll(ExpandFileName(htmlRoot+'\index.html'),'\','/'));
    end;

  PROCEDURE displayVersionInfo;
    begin writeln('MNH5',
                  {$ifdef fullVersion}'(full'{$else}'(light'{$endif},
                  {$ifdef debugMode}',debug)'{$else}')'{$endif},
                  {$I %DATE%},
                  ' ',{$I %TIME%},
                  ' FPC',{$I %FPCVERSION%},
                  ' for ',{$I %FPCTARGET%});
    end;

  PROCEDURE displayHelp;
    begin
      writeln('MNH5 ',{$ifdef fullVersion}'(full'{$else}'(light'{$endif},
                      {$ifdef debugMode}',debug)'{$else}')'{$endif},' by Martin Schlegel');
      writeln('compiled on: ',{$I %DATE%});
      writeln('         at: ',{$I %TIME%});
      writeln('FPC version: ',{$I %FPCVERSION%});
      writeln('Target CPU : ',{$I %FPCTARGET%});
      writeln;
      writeln('Accepted parameters: ');
      writeln('  [-h/-version] [+echo/-echo] [-el#] [-det] [filename [parameters]]');
      writeln('  filename: if present the file is interpreted; parameters are passed if present');
      writeln('            if not present, interactive mode is entered');
      writeln('  +echo   : force echo on (default for interactive mode)');
      writeln('  -echo   : force echo off (default for interpretation mode)');
      writeln('  +time   : force time on (default for interactive mode)');
      writeln('  -time   : force time off (default for interpretation mode)');
      writeln('  -el#    : set minimum error level for output; valid values: [0..5], default=2');
      writeln('  -h      : display this help and quit');
      writeln('  -det    : force deterministic "random" numbers');
      writeln('  -version: show version info and exit');
      writeln('  -doc    : regenerate and show documentation');
    end;

  PROCEDURE fileMode;
    VAR startTime:double;
    begin
	  startTime:=now;
      mainPackageProvider.setPath(fileToInterpret);
      if wantHelpDisplay then begin
        displayHelp;
        printMainPackageDocText;
        halt;
      end;
      callMainInMain(parameters);
    if displayTime then writeln('time: ',(now-startTime)*24*60*60:0:3,'sec');
      mnh_out_adapters.haltWithAdaptedSystemErrorLevel;
    end;

  VAR echo:(e_forcedOn,e_default,e_forcedOff)=e_default;
      time:(t_forcedOn,t_default,t_forcedOff)=t_default;
      i,pel:longint;
  begin
    setLength(parameters,0);
    for i:=1 to paramCount do begin
    if fileToInterpret='' then begin
        if      paramstr(i)='+echo' then echo:=e_forcedOn
        else if paramstr(i)='-echo' then echo:=e_forcedOff
        else if paramstr(i)='+time' then time:=t_forcedOn
        else if paramstr(i)='-time' then time:=t_forcedOff
        else if paramstr(i)='-det'  then randseed:=0
        else if startsWith(paramStr(i),'-h') then wantHelpDisplay:=true
        else if startsWith(paramStr(i),'-version') then begin displayVersionInfo; halt; end
        else if startsWith(paramStr(i),'-codeHash') then begin write({$ifdef fullVersion}'F'{$else}'L'{$endif},{$I %FPCTARGET%}); {$include code_hash.inc} halt; end
        else if startsWith(paramStr(i),'-doc') then begin makeAndShowDoc; halt; end
        else if startsWith(paramStr(i),'-el') then begin
          pel:=strToIntDef(copy(paramstr(i),4,length(paramstr(i))-3),-1);
          if (pel<0) or (pel>5) then begin
            writeln('Invalid minimum error level given!');
            writeln('Parameter: ',paramStr(i),'; extracted level: ',copy(paramstr(i),4,length(paramstr(i))-3));
            writeln('Allowed values: 0, 1, 2, 3, 4, 5');
            halt;
          end else consoleOutAdapter.minErrorLevel:=T_messageTypeOrErrorLevel(ord(el0_allOkay)+ pel);
        end else begin
          if fileExists(paramstr(i)) then fileToInterpret:=paramStr(i) else begin
            writeln('Invalid filename given!');
            writeln('Parameter: ',paramStr(i));
            writeln('File does not exist.');
            halt;
          end;
        end;
	  end else begin
        setLength(parameters,length(parameters)+1);
        parameters[length(parameters)-1]:=paramStr(i);
	  end;
    end;
    //-----------------------------------------------------
    if (echo=e_forcedOn) or (echo=e_default) and (fileToInterpret='') then begin
      consoleOutAdapter.echoOn:=true;
    end else begin
      consoleOutAdapter.echoOn:=false;
    end;
    displayTime:=((time=t_forcedOn) or (echo=e_default) and (fileToInterpret=''));

    if fileToInterpret<>'' then fileMode;
    if wantHelpDisplay then begin
      displayHelp;
      halt;
    end;
  end;

end.
