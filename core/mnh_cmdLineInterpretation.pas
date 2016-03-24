UNIT mnh_cmdLineInterpretation;
INTERFACE
USES mnh_constants,mnh_out_adapters,mnh_funcs,consoleAsk{$ifdef fullVersion},mnh_doc{$endif},mnh_packages,
     mnh_tokLoc,myStringUtil,sysutils,myGenerics,mnh_contexts,
     lclintf,mySys,mnh_html;
FUNCTION parseCmdLine:T_tokenLocation;
PROCEDURE makeAndShowDoc;
VAR consoleAdapters:T_adapters;
IMPLEMENTATION
//by command line parameters:---------------
VAR fileOrCommandToInterpret:ansistring='';
    mnhParameters:T_arrayOfString;
    mainParameters:T_arrayOfString;
    wantHelpDisplay:boolean=false;
    directExecutionMode:boolean=false;
//---------------:by command line parameters
PROCEDURE makeAndShowDoc;
  begin
    {$ifdef fullVersion}
    findAndDocumentAllPackages;
    OpenURL('file:///'+replaceAll(expandFileName(getHtmlRoot+'\index.html'),'\','/'));
    {$else}
    writeln('Generation of the documentation is only implemented in the full version.');
    {$endif}
  end;

FUNCTION parseCmdLine:T_tokenLocation;
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
      {$ifdef fullVersion}
      consoleAdapters.printOut('  -doc              regenerate and show documentation');
      {$endif}
      consoleAdapters.printOut('  -out:<filename>   write output to the given file; if the extension is .html, ');
      consoleAdapters.printOut('                    an html document will be generated, otherwise simple text.');
      {$ifdef fullVersion}
      consoleAdapters.printOut('  -open<location>  Open the given file at location');
      {$endif}
    end;

  PROCEDURE fileMode;
    VAR context:T_evaluationContext;
    begin
      environment.mainPackageProvider^.setPath(fileOrCommandToInterpret);
      fileOrCommandToInterpret:='';
      if wantHelpDisplay then begin
        environment.mainPackageProvider^.load;
        printMainPackageDocText(consoleAdapters);
        halt;
      end;
      context.createNormalContext(P_adapters(@consoleAdapters));
      callMainInMain(mainParameters,context);
      context.destroy;
      halt;
    end;

  PROCEDURE doDirect;
    VAR context:T_evaluationContext;
    begin
      context.createNormalContext(P_adapters(@consoleAdapters));
      environment.mainPackageProvider^.clear;
      environment.mainPackageProvider^.appendLine(fileOrCommandToInterpret);
      reloadMainPackage(lu_forDirectExecution,context);
      context.destroy;
      halt;
    end;

  PROCEDURE addParameter(VAR list:T_arrayOfString; CONST index:longint);
    begin
      setLength(list,length(list)+1);
      list[length(list)-1]:=paramStr(index);
    end;

  VAR echo:(e_forcedOn,e_default,e_forcedOff)=e_default;
      time:(t_forcedOn,t_default,t_forcedOff)=t_default;
      i    :longint;
      minEL:longint=2;
  begin
    result:=C_nilTokenLocation;
    setLength(mainParameters,0);
    setLength(mnhParameters,0);
    for i:=1 to paramCount do begin
    if (fileOrCommandToInterpret='') or directExecutionMode then begin
        if      paramStr(i)='+echo' then begin echo:=e_forcedOn;           addParameter(mnhParameters,i); end
        else if paramStr(i)='-echo' then begin echo:=e_forcedOff;          addParameter(mnhParameters,i); end
        else if paramStr(i)='+time' then begin time:=t_forcedOn;           addParameter(mnhParameters,i); end
        else if paramStr(i)='-time' then begin time:=t_forcedOff;          addParameter(mnhParameters,i); end
        else if paramStr(i)='-det'  then begin randseed:=0;                addParameter(mnhParameters,i); end
        else if paramStr(i)='-cmd'  then begin directExecutionMode:=true;  addParameter(mnhParameters,i); end
        else if startsWith(paramStr(i),'-out:') then begin
          addOutfile(consoleAdapters, copy(paramStr(i),6,length(paramStr(i))-5),false);
          addParameter(mnhParameters,i);
        end
        {$ifdef fullVersion}
        else if startsWith(paramStr(i),'-open@') then result:=guessLocationFromString(paramStr(i),true)
        {$endif}
        else if startsWith(paramStr(i),'-h') then wantHelpDisplay:=true
        else if startsWith(paramStr(i),'-version') then begin displayVersionInfo; halt; end
        else if startsWith(paramStr(i),'-codeHash') then begin write({$ifdef fullVersion}'F'{$else}'L'{$endif},
                                                                     {$ifdef debugMode}  'D'{$else}'O'{$endif},
                                                                     {$I %FPCTARGET%}); {$include code_hash.inc} halt; end
        else if startsWith(paramStr(i),'-doc') then begin makeAndShowDoc; halt; end
        else if startsWith(paramStr(i),'-el') then begin
          minEL:=strToIntDef(copy(paramStr(i),4,length(paramStr(i))-3),-1);
          addParameter(mnhParameters,i);
          if (minEL<0) or (minEL>5) then begin
            writeln('Invalid minimum error level given!');
            writeln('Parameter: ',paramStr(i),'; extracted level: ',copy(paramStr(i),4,length(paramStr(i))-3));
            writeln('Allowed values: 0, 1, 2, 3, 4, 5');
            halt;
          end;
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
      end else addParameter(mainParameters,i);
    end;
    setMnhParameters(mnhParameters);
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
  end;

INITIALIZATION
  consoleAdapters.create;
  consoleAdapters.addConsoleOutAdapter;
FINALIZATION
  consoleAdapters.destroy;

end.
