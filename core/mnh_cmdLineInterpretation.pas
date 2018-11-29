UNIT mnh_cmdLineInterpretation;
INTERFACE
USES sysutils,
     myStringUtil,myGenerics,mySys,
     mnh_constants,
     mnh_fileWrappers,
     mnh_messages,
     mnh_out_adapters,consoleAsk,{$ifdef fullVersion}mnh_doc,{$endif}mnh_settings,
     mnh_funcs_mnh,
     mnh_contexts,
     packages,
     recyclers,
     mnh_evaluation;

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
{$ifdef fullVersion}
FUNCTION getFileToInterpretFromCommandLine:ansistring;
FUNCTION getCommandToInterpretFromCommandLine:ansistring;
{$endif}
PROCEDURE setupOutputBehaviourFromCommandLineOptions(CONST adapters:P_messagesDistributor; CONST guiAdapterOrNil:P_abstractOutAdapter);
PROCEDURE displayHelp;

CONST CMD_LINE_PSEUDO_FILENAME='<cmd_line>';
VAR mainParameters:T_arrayOfString;
    wantConsoleAdapter:boolean=true;
    reEvaluationWithGUIrequired:boolean=false;
    {$ifdef fullVersion}
    plotAdapters:P_abstractOutAdapter=nil;
    profilingRun:boolean=false;
    filesToOpenInEditor:T_arrayOfString;
    {$endif}
IMPLEMENTATION
//by command line parameters:---------------
VAR fileOrCommandToInterpret:ansistring='';
    directExecutionMode:boolean=false;
    deferredAdapterCreations:array of record
      nameAndOption:string;
      appending:boolean;
    end;
//---------------:by command line parameters
{$ifdef fullVersion}
PROCEDURE addFileToOpen(CONST pathOrPattern:string);
  VAR info:T_fileInfo;
  begin
    if containsPlaceholder(pathOrPattern) then begin
      for info in findFileInfo(pathOrPattern) do if (info.size>0) and not(aDirectory in info.attributes) then append(filesToOpenInEditor,info.filePath);
    end else append(filesToOpenInEditor,pathOrPattern);
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
    writeln('  -GUI              force evaluation with GUI');
    writeln('  -h                display this help or help on the input file if present and quit');
    writeln('  -headless         forbid input via ask (scripts using ask will crash)');
    writeln('  -cmd              directly execute the following command');
    writeln('  -info             show info; same as -cmd mnhInfo.print');
    writeln('  -profile          do a profiling run - implies -vt');
    writeln('  -edit <filename>  opens file(s) in editor instead of interpreting directly');
    writeln('  -out <filename>[(options)] write output to the given file; Options are verbosity options');
    writeln('     if no options are given, the global output settings will be used');
    writeln('  +out <filename>[(options)]  As -out but appending to the file if existing.');
    writeln('  -quiet            disable console output');
    writeln('  -silent           suppress beeps');
  end;

FUNCTION wantMainLoopAfterParseCmdLine:boolean;
CONST DEF_VERBOSITY_STRING='';
  VAR consoleAdapters:T_messagesDistributor;
      wantHelpDisplay:boolean=false;
      headless:boolean=false;
      parsingState:(pst_initial,pst_parsingOutFileRewrite,pst_parsingOutFileAppend,pst_parsingFileToEdit)=pst_initial;
      verbosityString:string=DEF_VERBOSITY_STRING;
      quitImmediate:boolean=false;
      {$ifdef UNIX}
      hasAnyMnhParameter:boolean=false;
      {$endif}

  {$ifdef fullVersion}
  CONST contextType:array[false..true] of T_evaluationContextType=(ect_normal,ect_profiling);
  {$endif}

  {Return true when parsed successfully}
  FUNCTION parseSingleMnhParameter(CONST param:string):boolean;
    VAR app:string;
    begin
      result:=false;
      case parsingState of
        pst_initial: begin
          if startsWith(param,'-v') then begin
            app:=copy(param,3,length(param)-2);
            if app='' then app:='v';
            verbosityString+=app;
            exit(true);
          end;
          if (param='-profile') then begin
            {$ifdef fullVersion}
              profilingRun:=true;
            {$else}
              reEvaluationWithGUIrequired:=true;
            {$endif}
            exit(true);
          end;
          if (param='-GUI') then begin
            reEvaluationWithGUIrequired:=true;
            exit(true);
          end;
          if param='-quiet'    then begin wantConsoleAdapter:=false;               exit(true); end;
          if param='-silent'   then begin suppressBeep      :=true ;               exit(true); end;
          if param='-headless' then begin headless          :=true ;               exit(true); end;
          if param='-out'      then begin parsingState:=pst_parsingOutFileRewrite; exit(true); end;
          if param='+out'      then begin parsingState:=pst_parsingOutFileAppend;  exit(true); end;
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

  FUNCTION parseShebangParameters:boolean;
    VAR parameters:T_arrayOfString;
        k:longint;
    begin
     {$ifdef UNIX}
     //To prevent repeated parsing of the same shebang under Linux/UNIX systems
     if hasAnyMnhParameter then exit(true);
     {$endif}
      parameters:=parseShebang(fileOrCommandToInterpret);
      for k:=1 to length(parameters)-1 do
      if not(parseSingleMnhParameter(parameters[k])) then begin
        writeln('Invalid parameter/switch given by shebang: "',parameters[k],'"');
        exit(false);
      end;
      result:=true;
    end;

  FUNCTION parseMnhCommand(CONST param:string):boolean;
    begin
      result:=false;
      if directExecutionMode then begin
        fileOrCommandToInterpret+=' '+param;
        exit(true);
      end;
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
          if param='-cmd'           then begin directExecutionMode:=true;                exit(true); end;
          if startsWith(param,'-h') then begin wantHelpDisplay:=true;                    exit(true); end;
          if param='-info'          then begin writeln(getMnhInfo); quitImmediate:=true; exit(true); end;
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
      memoryComfortThreshold:=settings.memoryLimit;
      globals.create(@consoleAdapters);
      {$ifdef fullVersion} consoleAdapters.addOutAdapter(plotAdapters,false); {$endif}
      globals.resetForEvaluation({$ifdef fullVersion}@package,contextType[profilingRun]{$else}ect_normal{$endif},mainParameters);
      if wantHelpDisplay then begin
        package^.load(lu_forCodeAssistance,globals,recycler,C_EMPTY_STRING_ARRAY);
        writeln(package^.getHelpOnMain);
        dispose(package,destroy);
        wantHelpDisplay:=false;
        globals.destroy;
        recycler.cleanup;
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
    end;

  PROCEDURE executeCommand;
    begin
      {$ifdef fullVersion} if reEvaluationWithGUIrequired then exit; {$endif}
      executePackage(packageFromCode(fileOrCommandToInterpret,CMD_LINE_PSEUDO_FILENAME),lu_forDirectExecution);
    end;

  PROCEDURE executeScript;
    VAR package:P_package;
    begin
      {$ifdef fullVersion} if reEvaluationWithGUIrequired then exit; {$endif}
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
  begin
    consoleAdapters.createDistributor();
    setLength(mainParameters,0);
    setLength(deferredAdapterCreations,0);
    i:=1;
    while (i<=paramCount) {$ifndef fullVersion} and not(reEvaluationWithGUIrequired) {$endif} do begin
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
              fileOrCommandToInterpret:=paramStr(i);
              if not(parseShebangParameters) then exit(false);
            end else begin
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
    setupOutputBehaviourFromCommandLineOptions(@consoleAdapters,nil);
    if (fileOrCommandToInterpret<>'') and not(reEvaluationWithGUIrequired) then begin
       if directExecutionMode then executeCommand
                              else executeScript;
       quitImmediate:=true;
    end;
    if wantHelpDisplay then begin
      displayHelp;
      quitImmediate:=true;
    end;

    quitImmediate:=quitImmediate
      {$ifdef fullVersion}and (length(filesToOpenInEditor)=0) {$endif}
      and not(reEvaluationWithGUIrequired);
    result:=not(quitImmediate);
    consoleAdapters.destroy;
  end;

INITIALIZATION
  {$ifdef fullVersion}filesToOpenInEditor:=C_EMPTY_STRING_ARRAY;{$endif}

end.
