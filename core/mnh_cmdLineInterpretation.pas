UNIT mnh_cmdLineInterpretation;
INTERFACE
USES mnh_constants,mnh_out_adapters,mnh_funcs,consoleAsk{$ifdef fullVersion},mnh_doc,mnh_funcs_server{$endif},mnh_packages,
     myStringUtil,sysutils,myGenerics,mnh_contexts,
     lclintf,mnh_html,mnh_funcs_mnh;
FUNCTION wantMainLoopAfterParseCmdLine:boolean;
{$ifdef fullVersion}
PROCEDURE makeAndShowDoc(CONST includePackageDoc:boolean);
{$endif}
FUNCTION getFileOrCommandToInterpretFromCommandLine:ansistring;
PROCEDURE setupOutputBehaviourFromCommandLineOptions(VAR adapters:T_adapters; CONST guiAdapterOrNil:P_abstractOutAdapter);

VAR mainParameters:T_arrayOfString;
    wantConsoleAdapter:boolean=true;
    {$ifdef fullVersion}
    profilingRun:boolean=false;
    reEvaluationWithGUIrequired:boolean=false;
    filesToOpenInEditor:T_arrayOfString;
    {$endif}
IMPLEMENTATION
//by command line parameters:---------------
VAR fileOrCommandToInterpret:ansistring='';
    deferredAdapterCreations:array of record
      nameAndOption:string;
      appending:boolean;
    end;
//---------------:by command line parameters
FUNCTION getFileOrCommandToInterpretFromCommandLine:ansistring;
  begin
    result:=fileOrCommandToInterpret;
  end;

PROCEDURE setupOutputBehaviourFromCommandLineOptions(VAR adapters:T_adapters; CONST guiAdapterOrNil:P_abstractOutAdapter);
  VAR i:longint;
  begin
    for i:=0 to length(deferredAdapterCreations)-1 do with deferredAdapterCreations[i] do addOutfile(adapters,nameAndOption,appending);
    if guiAdapterOrNil<>nil then guiAdapterOrNil^.outputBehaviour:=defaultOutputBehavior;
  end;

{$ifdef fullVersion}
PROCEDURE makeAndShowDoc(CONST includePackageDoc:boolean);
  begin
    prepareDocumentation(includePackageDoc);
    if includePackageDoc then OpenURL('file:///'+replaceAll(expandFileName(getHtmlRoot+'/packages.html'),'\','/')+'#defined')
                         else OpenURL('file:///'+replaceAll(expandFileName(getHtmlRoot+'/index.html'   ),'\','/'));
  end;
{$endif}

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
  VAR consoleAdapters:T_adapters;
      mnhParameters:T_arrayOfString;
      wantHelpDisplay:boolean=false;
      directExecutionMode:boolean=false;
      echo:(e_forcedOn,e_default,e_forcedOff)=e_default;
      time:(t_forcedOn,t_default,t_forcedOff)=t_default;
      minEL:longint=3;

  PROCEDURE displayVersionInfo;
    begin writeln('MNH5',
                  {$ifdef fullVersion}'(full'{$else}'(light'{$endif},
                  {$ifdef imig}',IMIG'{$else}''{$endif},
                  {$ifdef debugMode}',debug)'{$else}')'{$endif},
                  {$I %DATE%},
                  ' ',{$I %TIME%},
                  ' FPC',{$I %FPCVERSION%},
                  ' for ',{$I %FPCTARGET%},{$ifdef imig}'+IMIG '{$else}' '{$endif},{$I %FPCTargetOS%});
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
      consoleAdapters.printOut('  [-h/-version] [+echo/-echo] [-el#] [(-cmd commandToExecute) | (filename [parameters])]');
      consoleAdapters.printOut('  filename          if present the file is interpreted; parameters are passed if present');
      consoleAdapters.printOut('                    if not present, interactive mode is entered');
      consoleAdapters.printOut('  +echo             force echo on (default for interactive mode)');
      consoleAdapters.printOut('  -echo             force echo off (default for interpretation mode)');
      consoleAdapters.printOut('  +time             force time on (default for interactive mode)');
      consoleAdapters.printOut('  -time             force time off (default for interpretation mode)');
      consoleAdapters.printOut('  -el#              set minimum error level for output; valid values: [0..5], default=2');
      consoleAdapters.printOut('  -h                display this help and quit');
      consoleAdapters.printOut('  -version          show version info and exit');
      consoleAdapters.printOut('  -codeHash         show codeHash and exit');
      consoleAdapters.printOut('  -cmd              directly execute the following command');
      consoleAdapters.printOut('  -info             show info; same as -cmd mnhInfo.print');
      {$ifdef fullVersion}
      consoleAdapters.printOut('  -profile          do a profiling run - implies +time');
      consoleAdapters.printOut('  -doc              regenerate and show documentation');
      consoleAdapters.printOut('  -edit <filename>  opens file(s) in editor instead of interpreting it directly');
      {$endif}
      consoleAdapters.printOut('  -out <filename>[(options)] write output to the given file; if the extension is .html, ');
      consoleAdapters.printOut('     an html document will be generated, otherwise simple text.');
      consoleAdapters.printOut('     options can consist of multiple characters interpreted as:');
      consoleAdapters.printOut('        p    : show print out');
      consoleAdapters.printOut('        P    : suppress print out');
      consoleAdapters.printOut('        i    : show input echo');
      consoleAdapters.printOut('        I    : suppress input echo');
      consoleAdapters.printOut('        d    : show declaration echo');
      consoleAdapters.printOut('        D    : suppress declaration echo');
      consoleAdapters.printOut('        o    : show output echo');
      consoleAdapters.printOut('        O    : suppress output echo');
      consoleAdapters.printOut('        t    : show timing info');
      consoleAdapters.printOut('        T    : suppress timing info');
      consoleAdapters.printOut('        1..5 : override minimum error level');
      consoleAdapters.printOut('        v    : be verbose; same as pidot1');
      consoleAdapters.printOut('     if no options are given, the global output settings will be used');
      consoleAdapters.printOut('  +out <filename>   As -out but appending to the file if existing.');
      consoleAdapters.printOut('  -quiet            disable console output');
    end;

  PROCEDURE doDirect;
    VAR context:T_evaluationContext;
        package:P_package;
    begin
      {$ifdef fullVersion}
      if profilingRun then context.createContext(P_adapters(@consoleAdapters),ct_profiling) else
      {$endif}
      context.createContext(P_adapters(@consoleAdapters),ct_normal);
      package:=packageFromCode(fileOrCommandToInterpret,'<cmd_line>');
      context.removeOption(cp_clearAdaptersOnStart);
      context.addOption(cp_beepOnError);
      context.resetForEvaluation(package);
      package^.load(lu_forDirectExecution,context,C_EMPTY_STRING_ARRAY);
      context.afterEvaluation;
      dispose(package,destroy);
      context.destroy;
      consoleAdapters.setExitCode;
    end;

  PROCEDURE fileMode;
    VAR context:T_evaluationContext;
        package:T_package;
    begin
      package.create(nil);
      package.setSourcePath(fileOrCommandToInterpret);

      if wantHelpDisplay then begin
        package.loadForDocumentation;
        package.printHelpOnMain(consoleAdapters);
        package.destroy;
        exit;
      end;
      {$ifdef fullVersion}
      if profilingRun then context.createContext(P_adapters(@consoleAdapters),ct_profiling) else
      {$endif}
      context.createContext(P_adapters(@consoleAdapters),ct_normal);
      context.removeOption(cp_clearAdaptersOnStart);
      context.addOption(cp_beepOnError);
      context.resetForEvaluation(@package);
      package.load(lu_forCallingMain,context,mainParameters);
      context.afterEvaluation;
      package.destroy;
      {$ifdef fullVersion}
      if context.adapters^.hasNeedGUIerror then begin
        reEvaluationWithGUIrequired:=true;
        context.destroy;
        exit;
      end;
      {$endif}
      context.destroy;
      consoleAdapters.setExitCode;
    end;

  PROCEDURE addParameter(VAR list:T_arrayOfString; CONST index:longint);
    begin
      setLength(list,length(list)+1);
      list[length(list)-1]:=paramStr(index);
    end;

  VAR i:longint;
      quitImmediate:boolean=false;
      nextAppendMode:boolean;

  begin
    consoleAdapters.create;
    setLength(mainParameters,0);
    setLength(mnhParameters,0);
    setLength(deferredAdapterCreations,0);
    i:=1;
    while i<=paramCount do begin
      if (fileOrCommandToInterpret='') or directExecutionMode then begin
        if      paramStr(i)='+echo' then begin echo:=e_forcedOn;           addParameter(mnhParameters,i); end
        else if paramStr(i)='-echo' then begin echo:=e_forcedOff;          addParameter(mnhParameters,i); end
        else if paramStr(i)='+time' then begin time:=t_forcedOn;           addParameter(mnhParameters,i); end
        else if paramStr(i)='-time' then begin time:=t_forcedOff;          addParameter(mnhParameters,i); end
        else if paramStr(i)='-cmd'  then begin directExecutionMode:=true;  addParameter(mnhParameters,i); end
        {$ifdef fullVersion}
        else if (paramStr(i)='-edit') then while i<paramCount do begin
          inc(i);
          append(filesToOpenInEditor,paramStr(i));
        end
        else if (paramStr(i)='-profile') then profilingRun:=true
        {$endif}
        else if ((paramStr(i)='-out') or (paramStr(i)='+out')) and (i<paramCount) then begin
          nextAppendMode:=paramStr(i)='+out';
          addParameter(mnhParameters,i);
          inc(i);
          setLength(deferredAdapterCreations,length(deferredAdapterCreations)+1);
          with deferredAdapterCreations[length(deferredAdapterCreations)-1] do begin
            appending:=nextAppendMode;
            nameAndOption:=paramStr(i);
          end;
          addParameter(mnhParameters,i);
        end
        else if startsWith(paramStr(i),'-quiet') then wantConsoleAdapter:=false
        else if startsWith(paramStr(i),'-h') then wantHelpDisplay:=true
        else if startsWith(paramStr(i),'-version') then begin displayVersionInfo; quitImmediate:=true; end
        else if startsWith(paramStr(i),'-info')    then begin writeln(getMnhInfo); quitImmediate:=true; end
        else if startsWith(paramStr(i),'-codeHash') then begin writeln({$ifdef fullVersion}'F'{$else}'L'{$endif},
                                                                       {$ifdef IMIG}'I'{$else}''{$endif},
                                                                       {$ifdef debugMode}'D'{$else}'O'{$endif},
                                                                       {$I %FPCTargetOS%},':',CODE_HASH); quitImmediate:=true; end
        {$ifdef fullVersion}
        else if startsWith(paramStr(i),'-doc') then begin makeAndShowDoc(false); quitImmediate:=true; end
        {$endif}
        else if startsWith(paramStr(i),'-el') then begin
          minEL:=strToIntDef(copy(paramStr(i),4,length(paramStr(i))-3),-1);
          addParameter(mnhParameters,i);
          if (minEL<=0) or (minEL>5) then begin
            writeln('Invalid minimum error level given!');
            writeln('Parameter: ',paramStr(i),'; extracted level: ',copy(paramStr(i),4,length(paramStr(i))-3));
            writeln('Allowed values: 1, 2, 3, 4, 5');
            exit(false);
          end;
        end else if directExecutionMode then begin
          fileOrCommandToInterpret:=fileOrCommandToInterpret+' '+paramStr(i);
        end else begin
          if fileExists(paramStr(i)) then fileOrCommandToInterpret:=paramStr(i) else begin
            writeln('Invalid filename given!');
            writeln('Parameter: ',paramStr(i));
            writeln('File does not exist.');
            exit(false);
          end;
        end;
      end else addParameter(mainParameters,i);
      inc(i);
    end;
    setMnhParameters(mnhParameters);

    {$ifdef fullVersion}
    if profilingRun then time:=t_forcedOn;
    {$endif}

    if fileOrCommandToInterpret=''
    then defaultOutputBehavior:=C_defaultOutputBehavior_interactive
    else defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;
    defaultOutputBehavior.minErrorLevel:=minEL;
    case echo of
      e_forcedOn: begin
        defaultOutputBehavior.doEchoDeclaration  :=true;
        defaultOutputBehavior.doEchoInput        :=true;
        defaultOutputBehavior.doShowExpressionOut:=true;
      end;
      e_forcedOff: begin
        defaultOutputBehavior.doEchoDeclaration  :=false;
        defaultOutputBehavior.doEchoInput        :=false;
        defaultOutputBehavior.doShowExpressionOut:=false;
      end;
    end;
    case time of
      t_forcedOn:  defaultOutputBehavior.doShowTimingInfo:=true;
      t_forcedOff: defaultOutputBehavior.doShowTimingInfo:=false;
    end;
    //-----------------------------------------------------
    if wantConsoleAdapter then consoleAdapters.addConsoleOutAdapter;
    setupOutputBehaviourFromCommandLineOptions(consoleAdapters,nil);
    if fileOrCommandToInterpret<>'' then begin
       if directExecutionMode then doDirect
                              else fileMode;
       quitImmediate:=true;
    end;
    if wantHelpDisplay then begin
      displayHelp;
      quitImmediate:=true;
    end;
    {$ifdef fullVersion}quitImmediate:=quitImmediate and (length(filesToOpenInEditor)=0) and not(reEvaluationWithGUIrequired);{$endif}
    result:=not(quitImmediate);
    consoleAdapters.destroy;
  end;

INITIALIZATION
  {$ifdef fullVersion}filesToOpenInEditor:=C_EMPTY_STRING_ARRAY;{$endif}

end.
