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
    if guiAdapterOrNil<>nil then guiAdapterOrNil^.outputBehavior:=defaultOutputBehavior;
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
      writeln('MNH5 '+{$ifdef fullVersion}'(full'{$else}'(light'{$endif}+
                      {$ifdef debugMode}',debug)'{$else}')'{$endif}+' by Martin Schlegel');
      writeln('compiled on: '+{$I %DATE%});
      writeln('         at: '+{$I %TIME%});
      writeln('FPC version: '+{$I %FPCVERSION%});
      writeln('Target CPU : '+{$I %FPCTARGET%});
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
      writeln('                       1..5 : override minimum error level');
      writeln('                       v/V  : be verbose; same as pidot1 (uppercase means disabling all output)');
      writeln('  -h                display this help or help on the input file if present and quit');
      writeln('  -version          show version info and exit');
      writeln('  -codeHash         show codeHash and exit');
      writeln('  -cmd              directly execute the following command');
      writeln('  -info             show info; same as -cmd mnhInfo.print');
      {$ifdef fullVersion}
      writeln('  -profile          do a profiling run - implies +time');
      writeln('  -doc              regenerate and show documentation');
      writeln('  -edit <filename>  opens file(s) in editor instead of interpreting it directly');
      {$endif}
      writeln('  -out <filename>[(options)] write output to the given file; if the extension is .html, ');
      writeln('     an html document will be generated, otherwise simple text. Options are verbosity options');
      writeln('     if no options are given, the global output settings will be used');
      writeln('  +out <filename>[(options)]  As -out but appending to the file if existing.');
      writeln('  -quiet            disable console output');
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
        writeln(package.getHelpOnMain);
        package.destroy;
        wantHelpDisplay:=false;
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
      {$ifdef fullVersion} if not(context.adapters^.hasNeedGUIerror) then {$endif}
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
        if startsWith(paramStr(i),'-version') then begin displayVersionInfo; quitImmediate:=true; end
        else if startsWith(paramStr(i),'-v') then verbosityString:=copy(paramStr(i),3,length(paramStr(i))-2)
        {$ifdef fullVersion}
        else if (paramStr(i)='-edit') then while i<paramCount do begin
          inc(i);
          append(filesToOpenInEditor,paramStr(i));
        end
        else if (paramStr(i)='-profile') then profilingRun:=true
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
        else if startsWith(paramStr(i),'-h') then wantHelpDisplay:=true
        else if startsWith(paramStr(i),'-info')    then begin writeln(getMnhInfo); quitImmediate:=true; end
        else if startsWith(paramStr(i),'-codeHash') then begin writeln({$ifdef fullVersion}'F'{$else}'L'{$endif},
                                                                       {$ifdef imig}'I'{$else}''{$endif},
                                                                       {$ifdef debugMode}'D'{$else}'O'{$endif},
                                                                       {$I %FPCTargetOS%},':',CODE_HASH); quitImmediate:=true; end
        {$ifdef fullVersion}
        else if startsWith(paramStr(i),'-doc') then begin makeAndShowDoc(false); quitImmediate:=true; end
        {$endif}
        else if directExecutionMode then begin
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

    if fileOrCommandToInterpret=''
    then defaultOutputBehavior:=C_defaultOutputBehavior_interactive
    else defaultOutputBehavior:=C_defaultOutputBehavior_fileMode;
    if verbosityString<>DEF_VERBOSITY_STRING then begin
      if verbosityString='' then verbosityString:='v';
      defaultOutputBehavior:=verbosityString;
    end;
    {$ifdef fullVersion}
    if profilingRun then defaultOutputBehavior:=defaultOutputBehavior+[mt_timing_info];
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
