UNIT cmdLineInterpretation;
INTERFACE
USES sysutils,
     myGenerics,mySys,
     mnh_constants,
     fileWrappers,
     mnh_messages,
     out_adapters,{$ifdef fullVersion}mnh_plotData,mnh_doc,mnh_imig,{$endif}mnh_settings,
     funcs_mnh,
     contexts,
     packages,
     recyclers,
     commandLineParameters;

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
CONST CMD_LINE_PSEUDO_FILENAME='<cmd_line>';
    imigAdapters:P_abstractOutAdapter=nil;

VAR commandLine:T_commandLineParameters;
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
     funcs_xml,
     builtinGenerators,
     consoleAsk,
     basicTypes;

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
  VAR consoleAdapters:T_messagesDistributor;
      memCheckerStarted:boolean=false;

  {$ifdef fullVersion}
  CONST contextType:array[false..true] of T_evaluationContextType=(ect_normal,ect_profiling);
  {$endif}

  PROCEDURE executePackage(package:P_package; CONST loadMode:T_packageLoadUsecase);
    VAR globals:T_evaluationGlobals;
        recycler:T_recycler;
    begin
      recycler.initRecycler;
      globals.create(@consoleAdapters);
      {$ifdef fullVersion}
      consoleAdapters.addOutAdapter(newPlotSystemWithoutDisplay,true);
      consoleAdapters.addOutAdapter(newImigSystemWithoutDisplay,true);
      {$endif}
      globals.resetForEvaluation({$ifdef fullVersion}package,nil,{$endif}commandLine.mnhExecutionOptions.allowedSideEffects,{$ifdef fullVersion}contextType[clf_PROFILE in commandLine.mnhExecutionOptions.flags]{$else}ect_normal{$endif},commandLine.mainParameters,recycler);
      if clf_SHOW_HELP in commandLine.mnhExecutionOptions.flags then begin
        package^.load(lu_forCodeAssistance,globals,recycler,C_EMPTY_STRING_ARRAY);
        consoleAdapters.postTextMessage(mt_printline,C_nilTokenLocation,package^.getHelpOnMain);
        dispose(package,destroy);
        Exclude(commandLine.mnhExecutionOptions.flags,clf_SHOW_HELP);
        globals.destroy;
        recycler.cleanup;
        commandLine.pauseIfConfigured(false);
        exit;
      end;
      if (clf_HEADLESS in commandLine.mnhExecutionOptions.flags) then globals.primaryContext.setAllowedSideEffectsReturningPrevious(C_allSideEffects-[se_input]);
      package^.load(loadMode,globals,recycler,commandLine.mainParameters);
      if not(FlagGUINeeded in globals.primaryContext.messages^.getFlags) then globals.afterEvaluation(recycler);
      dispose(package,destroy);
      if (FlagGUINeeded in globals.primaryContext.messages^.getFlags) then begin
        include(commandLine.mnhExecutionOptions.flags,clf_GUI);
        globals.destroy;
        recycler.cleanup;
        exit;
      end;
      globals.destroy;
      recycler.cleanup;
      consoleAdapters.setExitCode;
      commandLine.pauseIfConfigured((ExitCode<>0) or (consoleAdapters.triggersBeep));
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
    //-----------------------------------------------------
    if commandLine.applyAndReturnOk(@consoleAdapters) then begin
      if (commandLine.fileOrCommandToInterpret<>'') and not(clf_GUI in commandLine.mnhExecutionOptions.flags) then begin
         startMemChecker(settings.memoryLimit);
         memCheckerStarted:=true;
         if commandLine.mnhExecutionOptions.executeCommand
         then executeCommand
         else executeScript;
         quitImmediate:=true;
      end;
      if (clf_SHOW_HELP in commandLine.mnhExecutionOptions.flags) then begin
        displayHelp(@consoleAdapters);
        quitImmediate:=true;
      end;
      quitImmediate:=quitImmediate
        {$ifdef fullVersion}and (length(commandLine.filesToOpenInEditor)=0) {$endif}
        and not(clf_GUI in commandLine.mnhExecutionOptions.flags);
      result:=not(quitImmediate);
      if result and not(memCheckerStarted) then startMemChecker(settings.memoryLimit);
    end else result:=false;
    consoleAdapters.destroy;
  end;

end.
