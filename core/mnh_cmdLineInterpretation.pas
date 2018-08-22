UNIT mnh_cmdLineInterpretation;
INTERFACE
USES sysutils,{$ifdef fullVersion}{$ifdef debugMode}lclintf,{$endif}{$endif}
     myStringUtil,myGenerics,{$ifdef fullVersion}mySys,{$endif}
     mnh_constants,
     mnh_fileWrappers,
     mnh_messages,
     mnh_out_adapters,consoleAsk,{$ifdef fullVersion}mnh_doc,mnh_settings,{$endif}
     mnh_funcs_mnh,
     mnh_contexts,
     mnh_packages,
     mnh_evaluation;

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
FUNCTION getFileOrCommandToInterpretFromCommandLine:ansistring;
PROCEDURE setupOutputBehaviourFromCommandLineOptions(VAR adapters:T_messageConnector; CONST guiAdapterOrNil:P_abstractOutAdapter);
PROCEDURE displayHelp;

VAR mainParameters:T_arrayOfString;
    wantConsoleAdapter:boolean=true;
    {$ifdef fullVersion}
    plotAdapters:P_abstractOutAdapter=nil;
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
    headless:boolean=false;
//---------------:by command line parameters
{$ifdef fullVersion}
PROCEDURE addFileToOpen(CONST pathOrPattern:string);
  VAR info:T_fileInfo;
  begin
    if containsPlaceholder(pathOrPattern) then begin
      for info in findFileInfo(pathOrPattern) do if (info.size>0) and not(aDirectory in info.attributes) then append(filesToOpenInEditor,info.filePath);
    end else append(filesToOpenInEditor,pathOrPattern);
  end;
{$endif}

FUNCTION getFileOrCommandToInterpretFromCommandLine:ansistring;
  begin
    result:=fileOrCommandToInterpret;
  end;

PROCEDURE setupOutputBehaviourFromCommandLineOptions(VAR adapters:T_messageConnector; CONST guiAdapterOrNil:P_abstractOutAdapter);
  VAR i:longint;
  begin
    for i:=0 to length(deferredAdapterCreations)-1 do with deferredAdapterCreations[i] do adapters.addOutfile(nameAndOption,appending);
    if guiAdapterOrNil<>nil then guiAdapterOrNil^.outputBehavior:=defaultOutputBehavior{$ifdef fullVersion}+C_messagesAlwaysProcessedInGuiMode{$endif};
  end;

PROCEDURE displayHelp;
  VAR s:string;
  begin
    writeln('MNH5 '+{$ifdef fullVersion}'(full'{$else}'(light'{$endif}+
                    {$ifdef debugMode}',debug)'{$else}')'{$endif}+' by Martin Schlegel');
    writeln('compiled on: '+{$I %DATE%});
    writeln('         at: '+{$I %TIME%});
    writeln('FPC version: '+{$I %FPCVERSION%});
    writeln('Target CPU : '+{$I %FPCTARGET%});
    writeln('');
    for s in LOGO do writeln(s);
    writeln('');
    writeln('Accepted parameters: ');
    writeln('  [mnh_options] [(-cmd commandToExecute) | (filename [parameters])]');
    writeln('  filename          if present the file is interpreted; parameters are passed if present');
    writeln('                    if not present, interactive mode is entered');
    writeln('  -v[options]       verbosity. options can consist of multiple characters.');
    writeln('                    Lowercase indicates enabling, uppercase indicates disabling.');
    writeln('                       p/P  : print out');
    writeln('                       i/I  : input echo');
    writeln('                       d/D  : declaration echo');
    writeln('                       o/O  : output echo');
    writeln('                       e/E  : all echo; same as ido');
    writeln('                       t/T  : timing info');
    writeln('                       n/N  : notes (error level 1 only)');
    writeln('                       w/W  : warnings (error level 2 only)');
    writeln('                       u/U  : user defined notes, warnings and errors');
    writeln('                       1..4 : override minimum error level');
    writeln('                       v/V  : be verbose; same as pidot1 (uppercase means disabling all output)');
    writeln('  -h                display this help or help on the input file if present and quit');
    writeln('  -headless         forbid input via ask (scripts using ask will crash)');
    writeln('  -cmd              directly execute the following command');
    writeln('  -info             show info; same as -cmd mnhInfo.print');
    {$ifdef fullVersion}
    writeln('  -install          update packes and demos, ensure installation directory and file associations');
    writeln('  -uninstall        remove packes and demos, remove installation directory and file associations');
    writeln('  -profile          do a profiling run - implies -vt');
    writeln('  -edit <filename>  opens file(s) in editor instead of interpreting directly');
    {$endif}
    writeln('  -out <filename>[(options)] write output to the given file; Options are verbosity options');
    writeln('     if no options are given, the global output settings will be used');
    writeln('  +out <filename>[(options)]  As -out but appending to the file if existing.');
    writeln('  -quiet            disable console output');
    writeln('  -silent           suppress beeps');
  end;

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
  VAR consoleAdapters:T_messageConnector;
      mnhParameters:T_arrayOfString;
      wantHelpDisplay:boolean=false;
      directExecutionMode:boolean=false;
  {$ifdef fullVersion}
  CONST contextType:array[false..true] of T_evaluationContextType=(ect_normal,ect_profiling);
  {$endif}
  PROCEDURE doDirect;
    VAR globals:T_evaluationGlobals;
        package:P_package;
    begin
      {$ifdef fullVersion}memoryComfortThreshold:=settings.value^.memoryLimit;{$endif}
      globals.create(@consoleAdapters);
      if headless then globals.primaryContext.setAllowedSideEffectsReturningPrevious(C_allSideEffects-[se_inputViaAsk]);
      package:=packageFromCode(fileOrCommandToInterpret,'<cmd_line>');
      globals.resetForEvaluation({$ifdef fullVersion}package,contextType[profilingRun]{$else}ect_normal{$endif},C_EMPTY_STRING_ARRAY);
      package^.load(lu_forDirectExecution,globals,C_EMPTY_STRING_ARRAY);
      globals.afterEvaluation;
      dispose(package,destroy);
      globals.destroy;
      consoleAdapters.setExitCode;
    end;

  PROCEDURE fileMode;
    VAR globals:T_evaluationGlobals;
        package:T_package;
    begin
      {$ifdef fullVersion}memoryComfortThreshold:=settings.value^.memoryLimit;{$endif}
      package.create(newFileCodeProvider(expandFileName(fileOrCommandToInterpret)),nil);
      globals.create(@consoleAdapters);
      {$ifdef fullVersion}
      consoleAdapters.addOutAdapter(plotAdapters,false);
      {$endif}
      globals.resetForEvaluation({$ifdef fullVersion}@package,contextType[profilingRun]{$else}ect_normal{$endif},mainParameters);
      if wantHelpDisplay then begin
        package.load(lu_forCodeAssistance,globals,C_EMPTY_STRING_ARRAY);
        writeln(package.getHelpOnMain);
        package.destroy;
        wantHelpDisplay:=false;
        globals.destroy;
        exit;
      end;
      if headless then globals.primaryContext.setAllowedSideEffectsReturningPrevious(C_allSideEffects-[se_inputViaAsk]);
      package.load(lu_forCallingMain,globals,mainParameters);
      {$ifdef fullVersion} if not(FlagGUINeeded in globals.primaryContext.messages.getFlags) then {$endif}
      globals.afterEvaluation;
      package.destroy;
      {$ifdef fullVersion}
      if (FlagGUINeeded in globals.primaryContext.messages.getFlags) then begin
        reEvaluationWithGUIrequired:=true;
        globals.destroy;
        exit;
      end;
      {$endif}
      globals.destroy;
      consoleAdapters.setExitCode;
    end;

  PROCEDURE addParameter(VAR list:T_arrayOfString; CONST index:longint);
    begin
      setLength(list,length(list)+1);
      list[length(list)-1]:=paramStr(index);
    end;

  CONST DEF_VERBOSITY_STRING='&^\';
  VAR i:longint;
      quitImmediate:boolean=false;
      nextAppendMode:boolean;
      verbosityString:string=DEF_VERBOSITY_STRING;

  begin
    consoleAdapters.create;
    setLength(mainParameters,0);
    setLength(mnhParameters,0);
    setLength(deferredAdapterCreations,0);
    i:=1;
    while i<=paramCount do begin
      if (fileOrCommandToInterpret='') or directExecutionMode then begin
        if startsWith(paramStr(i),'-v') then verbosityString:=copy(paramStr(i),3,length(paramStr(i))-2)
        {$ifdef fullVersion}
        else if (paramStr(i)='-install') then begin
          writeln('Updating scripts and demos');
          ensureDemos;
          {$ifdef Windows}
          writeln('Updating file associations');
          sandbox^.runInstallScript;
          {$endif}
          writeln('Saving settings');
          saveSettings;
          halt(0);
        end
        else if (paramStr(i)='-uninstall') then begin
          {$ifdef Windows}
          writeln('Removing file associations');
          sandbox^.runUninstallScript;
          mySys.deleteMyselfOnExit;
          {$endif}
          writeln('Removing configuration directory');
          RemoveDir(configDir);
          halt(0);
        end else if (paramStr(i)='-edit') then while i<paramCount do begin
          inc(i);
          addFileToOpen(paramStr(i));
        end
        else if (paramStr(i)='-profile') then profilingRun:=true
        {$ifdef debugMode}
        else if (paramStr(i)='-doc') then begin
          while i<paramCount do begin
            inc(i);
            append(filesToOpenInEditor,paramStr(i));
          end;
          if length(filesToOpenInEditor)>0
          then makeHtmlFromTemplate(filesToOpenInEditor[0])
          else makeHtmlFromTemplate();
          writeln(expandFileName(getHtmlRoot+'/index.html'   ));
          exit(false);
        end
        {$endif}
        {$endif}
        else if paramStr(i)='-cmd'  then begin directExecutionMode:=true;  addParameter(mnhParameters,i); end
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
        else if startsWith(paramStr(i),'-silent') then suppressBeep:=true
        else if startsWith(paramStr(i),'-headless') then headless:=true
        else if startsWith(paramStr(i),'-h') then wantHelpDisplay:=true
        else if startsWith(paramStr(i),'-info')    then begin writeln(getMnhInfo); quitImmediate:=true; end
        else if directExecutionMode then begin
          fileOrCommandToInterpret:=fileOrCommandToInterpret+' '+paramStr(i);
        end else begin
          if fileExists(paramStr(i)) then fileOrCommandToInterpret:=paramStr(i) else begin
            if startsWith(paramStr(i),'-') or startsWith(paramStr(i),'+')
            then writeln('Invalid parameter/switch given!')
            else begin
              writeln('Invalid filename given!');
              writeln('File does not exist.');
            end;
            writeln('Parameter: ',paramStr(i));
            exit(false);
          end;
        end;
      end else addParameter(mainParameters,i);
      inc(i);
    end;

    if fileOrCommandToInterpret=''
    then defaultOutputBehavior:=C_defaultOutputBehavior_interactive
    else defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;
    if verbosityString<>DEF_VERBOSITY_STRING then begin
      if verbosityString='' then verbosityString:='v';
      defaultOutputBehavior:=verbosityString;
    end;
    {$ifdef fullVersion}
    if profilingRun then defaultOutputBehavior:=defaultOutputBehavior+[mt_timing_info,mt_profile_call_info];
    {$endif}
    //-----------------------------------------------------
    wantConsoleAdapter:=wantConsoleAdapter or wantHelpDisplay;
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
