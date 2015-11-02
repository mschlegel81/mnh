UNIT mnh_cmdLineInterpretation;
INTERFACE
USES mnh_constants,mnh_out_adapters,mnh_funcs,consoleAsk{$ifdef fullVersion},mnh_plotData{$endif},mnh_tokens,mnh_tokLoc,myStringUtil,sysutils,myGenerics,
     mnh_doc,lclintf,mySys,mnh_html;
PROCEDURE parseCmdLine;
VAR displayTime:boolean=false;
IMPLEMENTATION
//by command line parameters:---------------
VAR fileOrCommandToInterpret:ansistring='';
    parameters:T_arrayOfString;
    wantHelpDisplay:boolean=false;
    directExecutionMode:boolean=false;
//---------------:by command line parameters

PROCEDURE parseCmdLine;
  PROCEDURE makeAndShowDoc;
    VAR i:longint;
    begin
      for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then with outAdapter[i]^.outputBehaviour do begin
        doEchoInput:=false;
        doEchoDeclaration:=false;
        doShowExpressionOut:=false;
        doShowTimingInfo:=false;
        minErrorLevel:=6;
      end;
      findAndDocumentAllPackages;
      OpenURL('file:///'+replaceAll(expandFileName(htmlRoot+'\index.html'),'\','/'));
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
      writeln('  [-h/-version] [+echo/-echo] [-el#] [-det] [(-cmd commandToExecute) | (filename [parameters])]');
      writeln('  filename : if present the file is interpreted; parameters are passed if present');
      writeln('             if not present, interactive mode is entered');
      writeln('  +echo    : force echo on (default for interactive mode)');
      writeln('  -echo    : force echo off (default for interpretation mode)');
      writeln('  +time    : force time on (default for interactive mode)');
      writeln('  -time    : force time off (default for interpretation mode)');
      writeln('  -el#     : set minimum error level for output; valid values: [0..5], default=2');
      writeln('  -h       : display this help and quit');
      writeln('  -det     : force deterministic "random" numbers');
      writeln('  -version : show version info and exit');
      writeln('  -codeHash: show codeHash and exit');
      writeln('  -cmd     : directly execute the following command');
      writeln('  -doc     : regenerate and show documentation');
      writeln('  -html:<filename> : write html output to the given file');
    end;

  PROCEDURE tryToRunSetup;
    CONST setupFile='setup.mnh';
    VAR i:longint;
    begin
      if not(fileExists(setupFile)) then exit;
      for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then with outAdapter[i]^.outputBehaviour do begin
        doEchoInput:=false;
        doEchoDeclaration:=false;
        doShowExpressionOut:=false;
        doShowTimingInfo:=false;
        minErrorLevel:=2;
      end;
      mainPackageProvider.setPath(setupFile);
      setLength(parameters,0);
      callMainInMain(parameters);
    end;

  PROCEDURE fileMode;
    VAR startTime:double;
    begin
      startTime:=now;
      mainPackageProvider.setPath(fileOrCommandToInterpret);
      if wantHelpDisplay then begin
        displayHelp;
        printMainPackageDocText;
        halt;
      end;
      callMainInMain(parameters);
      if displayTime then writeln('time: ',myTimeToStr(now-startTime));
      mnh_out_adapters.haltWithAdaptedSystemErrorLevel;
    end;

  PROCEDURE doDirect;
    VAR startTime:double;
    begin
      startTime:=now;
      mainPackageProvider.clear;
      mainPackageProvider.appendLine(fileOrCommandToInterpret);
      reloadMainPackage(lu_forDirectExecution);
      if displayTime then writeln('time: ',myTimeToStr(now-startTime));
      mnh_out_adapters.haltWithAdaptedSystemErrorLevel;
    end;

  VAR echo:(e_forcedOn,e_default,e_forcedOff)=e_default;
      time:(t_forcedOn,t_default,t_forcedOff)=t_default;
      i    :longint;
      minEL:longint=2;
      htmlAdapter:P_htmlOutAdapter;
  begin
    setLength(parameters,0);
    for i:=1 to paramCount do begin
    if (fileOrCommandToInterpret='') or directExecutionMode then begin
        if      paramStr(i)='+echo' then echo:=e_forcedOn
        else if paramStr(i)='-echo' then echo:=e_forcedOff
        else if paramStr(i)='+time' then time:=t_forcedOn
        else if paramStr(i)='-time' then time:=t_forcedOff
        else if paramStr(i)='-det'  then randseed:=0
        else if paramStr(i)='-cmd'  then directExecutionMode:=true
        else if startsWith(paramStr(i),'-html:') then begin
          new(htmlAdapter,create(copy(paramStr(i),7,length(paramStr(i))-6)));
          addOutAdapter(htmlAdapter);
        end else if startsWith(paramStr(i),'-h') then wantHelpDisplay:=true
        else if startsWith(paramStr(i),'-version') then begin displayVersionInfo; halt; end
        else if startsWith(paramStr(i),'-codeHash') then begin write({$ifdef fullVersion}'F'{$else}'L'{$endif},
                                                                     {$ifdef debugMode}  'D'{$else}'O'{$endif},
                                                                     {$I %FPCTARGET%}); {$include code_hash.inc} halt; end
        else if startsWith(paramStr(i),'-doc') then begin makeAndShowDoc; halt; end
        else if startsWith(paramStr(i),'-el') then begin
          minEL:=strToIntDef(copy(paramStr(i),4,length(paramStr(i))-3),-1);
          if (minEL<0) or (minEL>5) then begin
            writeln('Invalid minimum error level given!');
            writeln('Parameter: ',paramStr(i),'; extracted level: ',copy(paramStr(i),4,length(paramStr(i))-3));
            writeln('Allowed values: 0, 1, 2, 3, 4, 5');
            halt;
          end;;
        end else if directExecutionMode then begin
          fileOrCommandToInterpret:=fileOrCommandToInterpret+' '+paramStr(i);
        end else begin
          if fileExists(paramStr(i)) then fileOrCommandToInterpret:=paramStr(i) else begin
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
    displayTime:=((time=t_forcedOn) or (echo=e_default) and (fileOrCommandToInterpret=''));
    if (echo=e_forcedOn) or (echo=e_default) and ((fileOrCommandToInterpret='') or directExecutionMode) then begin
      for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then with outAdapter[i]^.outputBehaviour do begin
        doEchoInput:=true;
        doEchoDeclaration:=true;
        doShowExpressionOut:=true;
        doShowTimingInfo:=displayTime;
        minErrorLevel:=minEL;
      end;
    end else begin
      for i:=0 to length(outAdapter)-1 do if outAdapter[i]<>nil then with outAdapter[i]^.outputBehaviour do begin
        doEchoInput:=false;
        doEchoDeclaration:=false;
        doShowExpressionOut:=false;
        doShowTimingInfo:=displayTime;
        minErrorLevel:=minEL;
      end;
    end;
    if fileOrCommandToInterpret<>'' then begin
       if directExecutionMode then doDirect
                              else fileMode;
    end;
    if wantHelpDisplay then begin
      displayHelp;
      halt;
    end;
    tryToRunSetup;
  end;

end.
