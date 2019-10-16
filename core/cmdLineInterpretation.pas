UNIT cmdLineInterpretation;
INTERFACE
USES sysutils,
     myGenerics,mySys,
     mnh_constants,
     fileWrappers,
     mnh_messages,
     out_adapters,{$ifdef fullVersion}mnh_plotData,mnh_doc,{$endif}mnh_settings,
     funcs_mnh,
     contexts,
     packages,
     recyclers,
     commandLineParameters;

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
CONST CMD_LINE_PSEUDO_FILENAME='<cmd_line>';

VAR commandLine:T_commandLineParameters;
PROCEDURE pauseOnce;
IMPLEMENTATION
//GLUE USES: Do not remove!
USES evaluation,
     funcs_files,
     funcs_format,
     funcs_ipc,
     funcs_list,
     funcs_math,
     funcs_regex,
     funcs_server,
     funcs_strings,
     funcs_system,
     funcs_types,
     builtinGenerators,
     consoleAsk,
     basicTypes;

PROCEDURE pauseOnce;
  begin
    commandLine.pauseAtEnd:=false;
    commandLine.pauseOnError:=false;
    if ExitCode=0 then write('Evaluation finished. Press enter to exit ')
                  else write('Evaluation finished with exit code ',ExitCode,'. Press enter to exit ');
    readln;
  end;

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
  VAR consoleAdapters:T_messagesDistributor;
      memCheckerStarted:boolean=false;
      {$ifdef UNIX}
      hasAnyMnhParameter:boolean=false;
      {$endif}

  {$ifdef fullVersion}
  CONST contextType:array[false..true] of T_evaluationContextType=(ect_normal,ect_profiling);
  {$endif}

  PROCEDURE executePackage(package:P_package; CONST loadMode:T_packageLoadUsecase);
    VAR globals:T_evaluationGlobals;
        recycler:T_recycler;
    begin
      recycler.initRecycler;
      globals.create(@consoleAdapters);
      {$ifdef fullVersion} consoleAdapters.addOutAdapter(newPlotSystemWithoutDisplay,true); {$endif}
      globals.resetForEvaluation({$ifdef fullVersion}package,nil,contextType[commandLine.profilingRun]{$else}ect_normal{$endif},commandLine.mainParameters,recycler);
      if commandLine.wantHelpDisplay then begin
        package^.load(lu_forCodeAssistance,globals,recycler,C_EMPTY_STRING_ARRAY{$ifdef fullVersion},nil,nil{$endif});
        consoleAdapters.postTextMessage(mt_printline,C_nilTokenLocation,package^.getHelpOnMain);
        dispose(package,destroy);
        commandLine.wantHelpDisplay:=false;
        globals.destroy;
        recycler.cleanup;
        if commandLine.pauseAtEnd then pauseOnce;
        exit;
      end;
      if commandLine.headless then globals.primaryContext.setAllowedSideEffectsReturningPrevious(C_allSideEffects-[se_inputViaAsk]);
      package^.load(loadMode,globals,recycler,commandLine.mainParameters{$ifdef fullVersion},nil,nil{$endif});
      if not(FlagGUINeeded in globals.primaryContext.messages^.getFlags) then globals.afterEvaluation(recycler);
      dispose(package,destroy);
      if (FlagGUINeeded in globals.primaryContext.messages^.getFlags) then begin
        commandLine.reEvaluationWithGUIrequired:=true;
        globals.destroy;
        recycler.cleanup;
        exit;
      end;
      globals.destroy;
      recycler.cleanup;
      consoleAdapters.setExitCode;
      if commandLine.pauseAtEnd or commandLine.pauseOnError and ((ExitCode<>0) or (consoleAdapters.triggersBeep)) then pauseOnce;
    end;

  PROCEDURE executeCommand;
    begin
      executePackage(packageFromCode(commandLine.fileOrCommandToInterpret,CMD_LINE_PSEUDO_FILENAME),lu_forDirectExecution);
    end;

  PROCEDURE executeScript;
    VAR package:P_package;
    begin
      commandLine.fileOrCommandToInterpret:=expandFileName(commandLine.fileOrCommandToInterpret);
      new(package,create(newFileCodeProvider(commandLine.fileOrCommandToInterpret),nil));
      executePackage(package,lu_forCallingMain);
    end;

  VAR quitImmediate:boolean=false;
  begin
    consoleAdapters.createDistributor();
    commandLine.initFromCommandLine;

    if commandLine.fileOrCommandToInterpret=''
    then defaultOutputBehavior:=C_defaultOutputBehavior_interactive
    else defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;
    if commandLine.verbosityString<>DEF_VERBOSITY_STRING then begin
      if commandLine.verbosityString='' then commandLine.verbosityString:='v';
      defaultOutputBehavior:=commandLine.verbosityString;
    end;
    {$ifdef fullVersion}
    if commandLine.profilingRun then defaultOutputBehavior:=defaultOutputBehavior+[mt_profile_call_info];
    {$endif}
    //-----------------------------------------------------
    if commandLine.applyAndReturnOk(@consoleAdapters,nil) then begin
      if (commandLine.fileOrCommandToInterpret<>'') and not(commandLine.reEvaluationWithGUIrequired) then begin
         startMemChecker(settings.memoryLimit);
         memCheckerStarted:=true;
         if commandLine.directExecutionMode then executeCommand
                                            else executeScript;
         quitImmediate:=true;
      end;
      if commandLine.wantHelpDisplay then begin
        displayHelp(@consoleAdapters);
        quitImmediate:=true;
      end;
      quitImmediate:=quitImmediate
        {$ifdef fullVersion}and (length(commandLine.filesToOpenInEditor)=0) {$endif}
        and not(commandLine.reEvaluationWithGUIrequired);
      result:=not(quitImmediate);
      if result and not(memCheckerStarted) then startMemChecker(settings.memoryLimit);
    end else result:=false;
    consoleAdapters.destroy;
  end;

end.
