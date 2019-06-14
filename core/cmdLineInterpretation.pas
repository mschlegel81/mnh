UNIT cmdLineInterpretation;
INTERFACE
USES sysutils,
     myStringUtil,myGenerics,mySys,
     mnh_constants,
     fileWrappers,
     mnh_messages,
     out_adapters,{$ifdef fullVersion}mnh_plotData,mnh_doc,{$endif}mnh_settings,
     funcs_mnh,
     contexts,
     packages,
     recyclers;

CONST
  FLAG_GUI          ='-GUI';
  FLAG_QUIET        ='-quiet';
  FLAG_SILENT       ='-silent';
  FLAG_HEADLESS     ='-headless';
  FLAG_PAUSE_ON_ERR ='-pauseOnError';
  FLAG_PAUSE_ALWAYS ='-pauseAfter';
  FLAG_PROFILE      ='-profile';

  FLAG_SHOW_HELP    ='-h';
  FLAG_EXEC_CMD     ='-cmd';
  FLAG_SHOW_INFO    ='-info';

  FLAG_TEXT:array[T_cmdLineFlag] of string=(FLAG_GUI,FLAG_QUIET,FLAG_SILENT,FLAG_HEADLESS,FLAG_PROFILE,FLAG_PAUSE_ALWAYS);

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
{$ifdef fullVersion}
FUNCTION getFileToInterpretFromCommandLine:ansistring;
FUNCTION getCommandToInterpretFromCommandLine:ansistring;
{$else}
PROCEDURE displayHelp(CONST adapters:P_messages);
{$endif}
PROCEDURE setupOutputBehaviourFromCommandLineOptions(CONST adapters:P_messagesDistributor; CONST guiAdapterOrNil:P_abstractOutAdapter);

CONST CMD_LINE_PSEUDO_FILENAME='<cmd_line>';
VAR mainParameters:T_arrayOfString;
    wantConsoleAdapter:boolean=true;
    reEvaluationWithGUIrequired:boolean=false;
    pauseAtEnd:boolean=false;
    pauseOnError:boolean=false;
    headless:boolean=false;
    {$ifdef fullVersion}
    profilingRun:boolean=false;
    filesToOpenInEditor:T_arrayOfString;
    {$endif}

CONST DEF_VERBOSITY_STRING='';
VAR verbosityString:string=DEF_VERBOSITY_STRING;

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

//by command line parameters:---------------
VAR fileOrCommandToInterpret:ansistring='';
    directExecutionMode:boolean=false;
    deferredAdapterCreations:array of record
      nameAndOption:string;
      appending:boolean;
    end;
//---------------:by command line parameters
PROCEDURE pauseOnce;
  begin
    pauseAtEnd:=false;
    pauseOnError:=false;
    if ExitCode=0 then write('Evaluation finished. Press enter to exit ')
                  else write('Evaluation finished with exit code ',ExitCode,'. Press enter to exit ');
    readln;
  end;
{$ifdef fullVersion}
PROCEDURE addFileToOpen(CONST pathOrPattern:string);
  VAR info:T_fileInfo;
  begin
    if containsPlaceholder(pathOrPattern) then begin
      for info in findFileInfo(pathOrPattern) do if (info.size>0) and not(aDirectory in info.attributes) then append(filesToOpenInEditor,expandFileName(info.filePath));
    end else append(filesToOpenInEditor,expandFileName(pathOrPattern));
  end;

FUNCTION getCommandToInterpretFromCommandLine:ansistring; begin if     directExecutionMode  then result:=fileOrCommandToInterpret else result:=''; end;
{$endif}
FUNCTION getFileToInterpretFromCommandLine   :ansistring; begin if not(directExecutionMode) then result:=fileOrCommandToInterpret else result:=''; end;

PROCEDURE setupOutputBehaviourFromCommandLineOptions(CONST adapters:P_messagesDistributor; CONST guiAdapterOrNil:P_abstractOutAdapter);
  VAR i:longint;
      scriptFileName:string;
  begin
    scriptFileName:=getFileToInterpretFromCommandLine;
    if scriptFileName<>'' then scriptFileName:=ChangeFileExt(scriptFileName,'');
    for i:=0 to length(deferredAdapterCreations)-1 do with deferredAdapterCreations[i] do adapters^.addOutfile(replaceAll(nameAndOption,'?',scriptFileName),appending);
    if guiAdapterOrNil<>nil then guiAdapterOrNil^.outputBehavior:=defaultOutputBehavior;
  end;

PROCEDURE displayHelp(CONST adapters:P_messages);
  PROCEDURE wl(CONST s:string);
    begin
      if (adapters=nil) or not(adapters^.isCollecting(mt_printline))
      then writeln(s)
      else adapters^.postTextMessage(mt_printline,C_nilTokenLocation,s);
    end;

  VAR s:string;
  begin
    wl('MNH5 '+{$ifdef fullVersion}'(full'{$else}'(light'{$endif}+
                    {$ifdef debugMode}',debug)'{$else}')'{$endif}+' by Martin Schlegel');
    wl('compiled on: '+{$I %DATE%});
    wl('         at: '+{$I %TIME%});
    wl('FPC version: '+{$I %FPCVERSION%});
    wl('Target CPU : '+{$I %FPCTARGET%});
    wl('');
    for s in LOGO do wl(s);
    wl('');
    wl('Accepted parameters: ');
    wl('  [mnh_options] [('+FLAG_EXEC_CMD+' commandToExecute) | (filename [parameters])]');
    wl('  filename          if present the file is interpreted; parameters are passed if present');
    wl('  -v[options]       verbosity. options can consist of multiple characters.');
    wl('                    Lowercase indicates enabling, uppercase indicates disabling.');
    wl('                       p/P  : print out');
    wl('                       i/I  : input echo');
    wl('                       d/D  : declaration echo');
    wl('                       o/O  : output echo');
    wl('                       e/E  : all echo; same as ido');
    wl('                       t/T  : timing info');
    wl('                       n/N  : notes (error level 1 only)');
    wl('                       w/W  : warnings (error level 2 only)');
    wl('                       u/U  : user defined notes, warnings and errors');
    wl('                       1..4 : override minimum error level');
    wl('                       v/V  : be verbose; same as pidot1 (uppercase means disabling all output)');
    wl('  '+FLAG_GUI+'              force evaluation with GUI');
    wl('  '+FLAG_SHOW_HELP+'                display this help or help on the input file if present and quit');
    wl('  '+FLAG_HEADLESS+'         forbid input via ask (scripts using ask will crash)');
    wl('  '+FLAG_EXEC_CMD+'              directly execute the following command');
    wl('  '+FLAG_SHOW_INFO+'             show info; same as '+FLAG_EXEC_CMD+' mnhInfo.print');
    wl('  '+FLAG_PROFILE+'          do a profiling run - implies -vt');
    wl('  -edit <filename>  opens file(s) in editor instead of interpreting directly');
    wl('  -out <filename>[(options)] write output to the given file; Options are verbosity options');
    wl('                    if no options are given, the global output settings will be used');
    wl('  +out <filename>[(options)]  As -out but appending to the file if existing.');
    wl('  '+FLAG_QUIET+'            disable console output');
    wl('  '+FLAG_SILENT+'           suppress beeps');
    wl('  '+FLAG_PAUSE_ALWAYS+'       pauses after script execution');
    wl('  '+FLAG_PAUSE_ON_ERR+'     pauses after script execution if an error ocurred');
  end;

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
  VAR consoleAdapters:T_messagesDistributor;
      wantHelpDisplay:boolean=false;
      parsingState:(pst_initial,pst_parsingOutFileRewrite,pst_parsingOutFileAppend,pst_parsingFileToEdit)=pst_initial;
      quitImmediate:boolean=false;
      memCheckerStarted:boolean=false;
      {$ifdef UNIX}
      hasAnyMnhParameter:boolean=false;
      {$endif}
      cmdLineParsingErrors:T_arrayOfString;

  {$ifdef fullVersion}
  CONST contextType:array[false..true] of T_evaluationContextType=(ect_normal,ect_profiling);
  {$endif}

  PROCEDURE logCmdLineParsingError(CONST s:string);
    begin
      append(cmdLineParsingErrors,s);
    end;

  {Return true when parsed successfully}
  FUNCTION parseSingleMnhParameter(CONST param:string):boolean;
    VAR app:string;
    begin
      result:=false;
      if fileOrCommandToInterpret<>'' then exit(false);
      case parsingState of
        pst_initial: begin
          if startsWith(param,'-v') then begin
            app:=copy(param,3,length(param)-2);
            if app='' then app:='v';
            verbosityString+=app;
            exit(true);
          end;
          if (param=FLAG_PROFILE) then begin
            {$ifdef fullVersion}
              profilingRun:=true;
            {$else}
              reEvaluationWithGUIrequired:=true;
            {$endif}
            exit(true);
          end;
          if param=FLAG_GUI          then begin reEvaluationWithGUIrequired:=true ;      exit(true); end;
          if param=FLAG_QUIET        then begin wantConsoleAdapter         :=false;      exit(true); end;
          if param=FLAG_SILENT       then begin suppressBeep               :=true ;      exit(true); end;
          if param=FLAG_HEADLESS     then begin headless                   :=true ;      exit(true); end;
          if param=FLAG_PAUSE_ON_ERR then begin pauseOnError               :=true ;      exit(true); end;
          if param=FLAG_PAUSE_ALWAYS then begin pauseAtEnd                 :=true ;      exit(true); end;
          if param='-out'            then begin parsingState:=pst_parsingOutFileRewrite; exit(true); end;
          if param='+out'            then begin parsingState:=pst_parsingOutFileAppend;  exit(true); end;
        end;
        pst_parsingOutFileAppend,pst_parsingOutFileRewrite: begin
          setLength(deferredAdapterCreations,length(deferredAdapterCreations)+1);
          with deferredAdapterCreations[length(deferredAdapterCreations)-1] do begin
            appending:=(parsingState=pst_parsingOutFileAppend);
            nameAndOption:=param;
          end;
          parsingState:=pst_initial;
          exit(true);
        end;
      end;
    end;

  FUNCTION parseShebangParameters(fileName:string):boolean;
    VAR parameters:T_arrayOfString;
        k:longint;
    begin
      {$ifdef UNIX}
      //To prevent repeated parsing of the same shebang under Linux/UNIX systems
      if hasAnyMnhParameter then exit(true);
      {$endif}
      parameters:=parseShebang(fileName);
      for k:=1 to length(parameters)-1 do
      if not(parseSingleMnhParameter(parameters[k])) then begin
        logCmdLineParsingError('Invalid parameter/switch given by shebang: "'+parameters[k]+'"');
        exit(false);
      end;
      result:=true;
    end;

  FUNCTION parseMnhCommand(CONST param:string):boolean;
    VAR s:string;
    begin
      result:=false;
      if directExecutionMode then begin
        fileOrCommandToInterpret+=' '+param;
        exit(true);
      end;
      if fileOrCommandToInterpret<>'' then exit(false);
      case parsingState of
        pst_initial: begin
          if (param='-edit') then begin
            parsingState:=pst_parsingFileToEdit;
            exit(true);
          end;
          {$ifdef fullVersion} {$ifdef debugMode}
          if (param='-doc') then begin
            makeHtmlFromTemplate(nil,nil);
            writeln(expandFileName(getHtmlRoot+'/index.html'));
            halt(0);
          end;
          {$endif} {$endif}
          if param=FLAG_EXEC_CMD  then begin directExecutionMode:=true;                              exit(true); end;
          if param=FLAG_SHOW_HELP then begin wantHelpDisplay:=true;                                  exit(true); end;
          if param=FLAG_SHOW_INFO then begin for s in getMnhInfo do writeln(s); quitImmediate:=true; exit(true); end;
        end;
        pst_parsingFileToEdit: begin
          {$ifdef fullVersion}
            addFileToOpen(param);
          {$else}
            reEvaluationWithGUIrequired:=true;
          {$endif}
          exit(true);
        end;
      end;
    end;

  PROCEDURE executePackage(package:P_package; CONST loadMode:T_packageLoadUsecase);
    VAR globals:T_evaluationGlobals;
        recycler:T_recycler;
    begin
      recycler.initRecycler;
      globals.create(@consoleAdapters);
      {$ifdef fullVersion} consoleAdapters.addOutAdapter(newPlotSystemWithoutDisplay,true); {$endif}
      globals.resetForEvaluation({$ifdef fullVersion}package,nil,contextType[profilingRun]{$else}ect_normal{$endif},mainParameters,recycler);
      if wantHelpDisplay then begin
        package^.load(lu_forCodeAssistance,globals,recycler,C_EMPTY_STRING_ARRAY);
        consoleAdapters.postTextMessage(mt_printline,C_nilTokenLocation,package^.getHelpOnMain);
        dispose(package,destroy);
        wantHelpDisplay:=false;
        globals.destroy;
        recycler.cleanup;
        if pauseAtEnd then pauseOnce;
        exit;
      end;
      if headless then globals.primaryContext.setAllowedSideEffectsReturningPrevious(C_allSideEffects-[se_inputViaAsk]);
      package^.load(loadMode,globals,recycler,mainParameters);
      if not(FlagGUINeeded in globals.primaryContext.messages^.getFlags) then globals.afterEvaluation(recycler);
      dispose(package,destroy);
      if (FlagGUINeeded in globals.primaryContext.messages^.getFlags) then begin
        reEvaluationWithGUIrequired:=true;
        globals.destroy;
        recycler.cleanup;
        exit;
      end;
      globals.destroy;
      recycler.cleanup;
      consoleAdapters.setExitCode;
      if pauseAtEnd or pauseOnError and ((ExitCode<>0) or (consoleAdapters.triggersBeep) {$ifdef fullVersion} or profilingRun {$endif}) then pauseOnce;
    end;

  PROCEDURE executeCommand;
    begin
      executePackage(packageFromCode(fileOrCommandToInterpret,CMD_LINE_PSEUDO_FILENAME),lu_forDirectExecution);
    end;

  PROCEDURE executeScript;
    VAR package:P_package;
    begin
      fileOrCommandToInterpret:=expandFileName(fileOrCommandToInterpret);
      new(package,create(newFileCodeProvider(fileOrCommandToInterpret),nil));
      executePackage(package,lu_forCallingMain);
    end;

  PROCEDURE addParameter(VAR list:T_arrayOfString; CONST index:longint);
    begin
      setLength(list,length(list)+1);
      list[length(list)-1]:=paramStr(index);
    end;

  VAR i:longint;
      s:string;
  begin
    cmdLineParsingErrors:=C_EMPTY_STRING_ARRAY;
    //TODO: Collect all ocurring errors and display them on a valid adapter (if possible)
    //TODO: Set exit code if command line parsing fails
    consoleAdapters.createDistributor();
    setLength(mainParameters,0);
    setLength(deferredAdapterCreations,0);
    i:=1;
    while (i<=paramCount) do begin
      if parseMnhCommand        (paramStr(i)) then inc(i) else
      if parseSingleMnhParameter(paramStr(i)) then begin
        inc(i);
        {$ifdef UNIX}
        hasAnyMnhParameter:=true;
        {$endif}
      end else begin
        if (fileOrCommandToInterpret='') then begin
          begin
            if fileExists(paramStr(i)) then begin
              if not(parseShebangParameters(paramStr(i))) then exit(false);
              fileOrCommandToInterpret:=paramStr(i);
            end else begin
              if startsWith(paramStr(i),'-') or startsWith(paramStr(i),'+')
              then logCmdLineParsingError('Invalid parameter/switch given!')
              else begin
                logCmdLineParsingError('Invalid filename given!');
                logCmdLineParsingError('File does not exist.');
              end;
              logCmdLineParsingError('Parameter: '+paramStr(i));
            end;
          end;
        end else addParameter(mainParameters,i);
        inc(i);
      end;
    end;

    if fileOrCommandToInterpret=''
    then defaultOutputBehavior:=C_defaultOutputBehavior_interactive
    else defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;
    if verbosityString<>DEF_VERBOSITY_STRING then begin
      if verbosityString='' then verbosityString:='v';
      defaultOutputBehavior:=verbosityString;
    end;
    {$ifdef fullVersion}
    if profilingRun then defaultOutputBehavior:=defaultOutputBehavior+[mt_profile_call_info];
    {$endif}
    //-----------------------------------------------------
    wantConsoleAdapter:=wantConsoleAdapter or wantHelpDisplay or (length(cmdLineParsingErrors)>0);
    if wantConsoleAdapter then consoleAdapters.addConsoleOutAdapter;
    setupOutputBehaviourFromCommandLineOptions(@consoleAdapters,nil);
    if length(cmdLineParsingErrors)>0 then begin
      if wantHelpDisplay then displayHelp(@consoleAdapters);

      if consoleAdapters.isCollecting(mt_el4_systemError)
      then consoleAdapters.postTextMessage(mt_el4_systemError,C_nilTokenLocation,cmdLineParsingErrors)
      else for s in cmdLineParsingErrors do writeln(s);
      ExitCode:=1;
      result:=false;
    end else begin
      if (fileOrCommandToInterpret<>'') and not(reEvaluationWithGUIrequired) then begin
         startMemChecker(settings.memoryLimit);
         memCheckerStarted:=true;
         if directExecutionMode then executeCommand
                                else executeScript;
         quitImmediate:=true;
      end;
      if wantHelpDisplay then begin
        displayHelp(@consoleAdapters);
        quitImmediate:=true;
      end;
      quitImmediate:=quitImmediate
        {$ifdef fullVersion}and (length(filesToOpenInEditor)=0) {$endif}
        and not(reEvaluationWithGUIrequired);
      result:=not(quitImmediate);
      if result and not(memCheckerStarted) then startMemChecker(settings.memoryLimit);
    end;
    consoleAdapters.destroy;
  end;

INITIALIZATION
  {$ifdef fullVersion}filesToOpenInEditor:=C_EMPTY_STRING_ARRAY;{$endif}

end.
