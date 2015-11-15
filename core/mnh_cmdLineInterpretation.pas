UNIT mnh_cmdLineInterpretation;
INTERFACE
USES mnh_constants,mnh_out_adapters,mnh_funcs,consoleAsk{$ifdef fullVersion},mnh_plotData{$endif},mnh_tokens,mnh_tokLoc,myStringUtil,sysutils,myGenerics,
     mnh_doc,lclintf,mySys,mnh_html;
FUNCTION parseCmdLine:T_tokenLocation;
VAR consoleAdapters:T_adapters;
IMPLEMENTATION
//by command line parameters:---------------
VAR fileOrCommandToInterpret:ansistring='';
    parameters:T_arrayOfString;
    wantHelpDisplay:boolean=false;
    directExecutionMode:boolean=false;
//---------------:by command line parameters

FUNCTION parseCmdLine:T_tokenLocation;
  PROCEDURE makeAndShowDoc;
    begin
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
      consoleAdapters.printOut('MNH5 '+{$ifdef fullVersion}'(full'{$else}'(light'{$endif}+
                      {$ifdef debugMode}',debug)'{$else}')'{$endif}+' by Martin Schlegel');
      consoleAdapters.printOut('compiled on: '+{$I %DATE%});
      consoleAdapters.printOut('         at: '+{$I %TIME%});
      consoleAdapters.printOut('FPC version: '+{$I %FPCVERSION%});
      consoleAdapters.printOut('Target CPU : '+{$I %FPCTARGET%});
      consoleAdapters.printOut('');
      consoleAdapters.printOut('Accepted parameters: ');
      consoleAdapters.printOut('  [-h/-version] [+echo/-echo] [-el#] [-det] [(-cmd commandToExecute) | (filename [parameters])]');
      consoleAdapters.printOut('  filename          if present the file is interpreted; parameters are passed if present');
      consoleAdapters.printOut('                    if not present, interactive mode is entered');
      consoleAdapters.printOut('  +echo             force echo on (default for interactive mode)');
      consoleAdapters.printOut('  -echo             force echo off (default for interpretation mode)');
      consoleAdapters.printOut('  +time             force time on (default for interactive mode)');
      consoleAdapters.printOut('  -time             force time off (default for interpretation mode)');
      consoleAdapters.printOut('  -el#              set minimum error level for output; valid values: [0..5], default=2');
      consoleAdapters.printOut('  -h                display this help and quit');
      consoleAdapters.printOut('  -det              force deterministic "random" numbers');
      consoleAdapters.printOut('  -version          show version info and exit');
      consoleAdapters.printOut('  -codeHash         show codeHash and exit');
      consoleAdapters.printOut('  -cmd              directly execute the following command');
      consoleAdapters.printOut('  -doc              regenerate and show documentation');
      consoleAdapters.printOut('  -out:<filename>   write output to the given file; if the extension is .html, ');
      consoleAdapters.printOut('                    an html document will be generated, otherwise simple text.');
      {$ifdef fullVersion}
      consoleAdapters.printOut('  -open<location>  Open the given file at location');
      {$endif}
    end;

  PROCEDURE tryToRunSetup;
    CONST setupFile='install.mnh';
    VAR context:T_evaluationContext;
    begin
      context.create(P_adapters(@consoleAdapters));
      if not(fileExists(setupFile)) then exit;
      mainPackageProvider.setPath(setupFile);
      setLength(parameters,0);
      callMainInMain(parameters,context);
      context.destroy;
    end;

  PROCEDURE fileMode;
    VAR context:T_evaluationContext;
    begin
      mainPackageProvider.setPath(fileOrCommandToInterpret);
      if wantHelpDisplay then begin
        displayHelp;
        printMainPackageDocText(consoleAdapters);
        halt;
      end;
      context.create(P_adapters(@consoleAdapters));
      callMainInMain(parameters,context);
      context.destroy;
      halt;
    end;

  PROCEDURE doDirect;
    VAR context:T_evaluationContext;
    begin
      context.create(P_adapters(@consoleAdapters));
      mainPackageProvider.clear;
      mainPackageProvider.appendLine(fileOrCommandToInterpret);
      reloadMainPackage(lu_forDirectExecution,context);
      context.destroy;
      halt;
    end;

  PROCEDURE addOutfile(CONST fileName:ansistring);
    begin
      if uppercase(extractFileExt(fileName))='.HTML'
      then addHtmlOutAdapter(consoleAdapters,fileName)
      else consoleAdapters.addFileOutAdapter(fileName);
    end;

  VAR echo:(e_forcedOn,e_default,e_forcedOff)=e_default;
      time:(t_forcedOn,t_default,t_forcedOff)=t_default;
      i    :longint;
      minEL:longint=2;
  begin
    result:=C_nilTokenLocation;
    setLength(parameters,0);
    for i:=1 to paramCount do begin
    if (fileOrCommandToInterpret='') or directExecutionMode then begin
        if      paramStr(i)='+echo' then echo:=e_forcedOn
        else if paramStr(i)='-echo' then echo:=e_forcedOff
        else if paramStr(i)='+time' then time:=t_forcedOn
        else if paramStr(i)='-time' then time:=t_forcedOff
        else if paramStr(i)='-det'  then randseed:=0
        else if paramStr(i)='-cmd'  then directExecutionMode:=true
        else if startsWith(paramStr(i),'-out:') then addOutfile(copy(paramStr(i),6,length(paramStr(i))-5))
        else if startsWith(paramStr(i),'-open@') then result:=guessLocationFromString(paramStr(i),true)
        else if startsWith(paramStr(i),'-h') then wantHelpDisplay:=true
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

    consoleAdapters.doEchoInput        :=(echo=e_forcedOn) or (echo=e_default) and ((fileOrCommandToInterpret='') or directExecutionMode);
    consoleAdapters.doEchoDeclaration  :=(echo=e_forcedOn) or (echo=e_default) and ((fileOrCommandToInterpret='') or directExecutionMode);
    consoleAdapters.doShowExpressionOut:=(echo=e_forcedOn) or (echo=e_default) and ((fileOrCommandToInterpret='') or directExecutionMode);
    consoleAdapters.doShowTimingInfo   :=(time=t_forcedOn) or (time=t_default) and ((fileOrCommandToInterpret='') or directExecutionMode);
    consoleAdapters.minErrorLevel:=minEL;

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

INITIALIZATION
  consoleAdapters.create;
  consoleAdapters.addConsoleOutAdapter;
FINALIZATION
  consoleAdapters.destroy;

end.
