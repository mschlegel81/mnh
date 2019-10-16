UNIT commandLineParameters;
INTERFACE
USES myGenerics,mnh_settings,out_adapters
     {$ifdef fullVersion}
     ,funcs
     {$endif};
TYPE
  T_commandLineParameters=object
      fileOrCommandToInterpret:ansistring;
      verbosityString:string;
      directExecutionMode,
      pauseAtEnd,
      pauseOnError,
      wantHelpDisplay,
      headless,
      reEvaluationWithGUIrequired:boolean;
      mainParameters:T_arrayOfString;

      {$ifdef fullVersion}
      profilingRun:boolean;
      filesToOpenInEditor:T_arrayOfString;
      {$endif}

      suppressBeep,
      wantMnhInfo:boolean;

      PROCEDURE clear;
      PROCEDURE initFromCommandLine;
      PROCEDURE initFromShebang(CONST shebangLine:string);
      FUNCTION applyAndReturnOk(CONST adapters: P_messagesDistributor; CONST guiAdapterOrNil: P_abstractOutAdapter):boolean;

      FUNCTION getCommandToInterpretFromCommandLine: ansistring;
      FUNCTION getFileToInterpretFromCommandLine: ansistring;
      {$ifdef fullVersion}
      FUNCTION getShebang(CONST requires:T_specialFunctionRequirements):ansistring;
      {$endif}
  private
      cmdLineParsingErrors:T_arrayOfString;
      wantConsoleAdapter:boolean;
      deferredAdapterCreations:array of record
        nameAndOption:string;
        appending:boolean;
      end;
      parsingState:(pst_initial,pst_parsingOutFileRewrite,pst_parsingOutFileAppend,pst_parsingFileToEdit);
    PROCEDURE addParameter(VAR list: T_arrayOfString; CONST index: longint);
    PROCEDURE logCmdLineParsingError(CONST s: string);
    FUNCTION parseMnhCommand(CONST param: string): boolean;
    FUNCTION parseShebangParameters(fileName: string): boolean;
    FUNCTION parseSingleMnhParameter(CONST param: string): boolean;
  end;
CONST
  DEF_VERBOSITY_STRING='';
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

PROCEDURE displayHelp(CONST adapters:P_messages);
IMPLEMENTATION
USES sysutils,
     myStringUtil,
     contexts,
     funcs_mnh,
     fileWrappers,
     mySys,
     mnh_messages,
     basicTypes,
     mnh_constants;
FUNCTION parseShebang(CONST scriptFileName:string):T_arrayOfString;
  VAR firstFileLine:ansistring;
      handle:textFile;
  begin
    if fileExists(scriptFileName) then begin
      try
        assign(handle,scriptFileName);
        reset(handle);
        readln(handle,firstFileLine);
        close(handle);
      except
        firstFileLine:='';
      end;
      if (copy(firstFileLine,1,2)='#!')
      then result:=splitCommandLine(copy(firstFileLine,3,length(firstFileLine)-2))
      else result:=C_EMPTY_STRING_ARRAY;
      {$ifdef debugMode}
      if length(result)>0
      then for firstFileLine in result do writeln(stdErr,'        SHEBANG: |',firstFileLine,'|')
      else                                writeln(stdErr,'        SHEBANG: none found');
      {$endif}
    end else result:=C_EMPTY_STRING_ARRAY;
  end;

PROCEDURE T_commandLineParameters.clear;
  begin
    setLength(cmdLineParsingErrors,0);
    parsingState:=pst_initial;

    setLength(mainParameters,0);
    verbosityString:=DEF_VERBOSITY_STRING;
    setLength(deferredAdapterCreations,0);
    fileOrCommandToInterpret:='';
    directExecutionMode:=false;
    wantConsoleAdapter:=true;
    reEvaluationWithGUIrequired:=false;
    pauseAtEnd:=false;
    pauseOnError:=false;
    headless:=false;
    wantHelpDisplay:=false;

    {$ifdef fullVersion}
    profilingRun:=false;
    setLength(filesToOpenInEditor,0);
    {$endif}
    suppressBeep:=false;
  end;

PROCEDURE T_commandLineParameters.initFromCommandLine;
  VAR i:longint;
  begin
    clear;
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
              parseShebangParameters(paramStr(i));
              fileOrCommandToInterpret:=expandFileName(paramStr(i));
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
  end;

PROCEDURE T_commandLineParameters.logCmdLineParsingError(CONST s: string);
  begin
    append(cmdLineParsingErrors,s);
  end;

PROCEDURE T_commandLineParameters.addParameter(VAR list: T_arrayOfString;
  CONST index: longint);
  begin
    setLength(list,length(list)+1);
    list[length(list)-1]:=paramStr(index);
  end;

FUNCTION T_commandLineParameters.parseShebangParameters(fileName: string): boolean;
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

{Return true when parsed successfully}
FUNCTION T_commandLineParameters.parseSingleMnhParameter(CONST param: string
  ): boolean;
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

FUNCTION T_commandLineParameters.parseMnhCommand(CONST param: string): boolean;
  {$ifdef fullVersion}
  PROCEDURE addFileToOpen(CONST pathOrPattern:string);
    VAR info:T_fileInfo;
    begin
      if containsPlaceholder(pathOrPattern) then begin
        for info in findFileInfo(pathOrPattern) do if (info.size>0) and not(aDirectory in info.attributes) then append(filesToOpenInEditor,expandFileName(info.filePath));
      end else append(filesToOpenInEditor,expandFileName(pathOrPattern));
    end;
  {$endif}
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
        if param=FLAG_EXEC_CMD  then begin directExecutionMode:=true;exit(true); end;
        if param=FLAG_SHOW_HELP then begin wantHelpDisplay:=true;    exit(true); end;
        if param=FLAG_SHOW_INFO then begin wantMnhInfo:=true;        exit(true); end;
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

PROCEDURE T_commandLineParameters.initFromShebang(CONST shebangLine: string);
  VAR k:longint;
      parameters:T_arrayOfString;
  begin
    clear;
    parameters:=splitCommandLine(copy(shebangLine,3,length(shebangLine)-2));
    for k:=1 to length(parameters)-1 do
    if not(parseSingleMnhParameter(parameters[k])) then
      logCmdLineParsingError('Invalid parameter/switch given by shebang: "'+parameters[k]+'"');
  end;

FUNCTION T_commandLineParameters.getFileToInterpretFromCommandLine: ansistring;
  begin if not(directExecutionMode) then result:=fileOrCommandToInterpret else result:=''; end;
{$ifdef fullVersion}
FUNCTION T_commandLineParameters.getShebang(CONST requires:T_specialFunctionRequirements): ansistring;
  VAR k:longint;
  begin
    reEvaluationWithGUIrequired:=reEvaluationWithGUIrequired or (sfr_needs_gui in requires);
    suppressBeep:=suppressBeep and not(sfr_beeps in requires);
    headless:=headless and not(sfr_asks in requires);
    result:='#!';
    if (settings.lightFlavourLocation='') or
       reEvaluationWithGUIrequired or
       (sfr_needs_full_version in requires)
    then result+=settings.fullFlavourLocation
    else result+=settings.lightFlavourLocation;
    if verbosityString<>DEF_VERBOSITY_STRING then result+=' -v'+verbosityString;
    if reEvaluationWithGUIrequired           then result+=' '+FLAG_GUI;
    if not(wantConsoleAdapter)               then result+=' '+FLAG_QUIET;
    if suppressBeep                          then result+=' '+FLAG_SILENT;
    if headless                              then result+=' '+FLAG_HEADLESS;
    if pauseOnError                          then result+=' '+FLAG_PAUSE_ON_ERR;
    if pauseAtEnd                            then result+=' '+FLAG_PAUSE_ALWAYS;
    for k:=0 to length(deferredAdapterCreations)-1 do begin
      if deferredAdapterCreations[k].appending
      then result+=' +out '+deferredAdapterCreations[k].nameAndOption
      else result+=' -out '+deferredAdapterCreations[k].nameAndOption;
    end;
  end;
{$endif}

FUNCTION T_commandLineParameters.getCommandToInterpretFromCommandLine: ansistring;
  begin if directExecutionMode then result:=fileOrCommandToInterpret else result:=''; end;

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

FUNCTION T_commandLineParameters.applyAndReturnOk(CONST adapters: P_messagesDistributor; CONST guiAdapterOrNil: P_abstractOutAdapter): boolean;
  VAR i:longint;
      scriptFileName:string;
  VAR s:ansistring;
  begin
    result:=true;
    if wantMnhInfo then begin
      for s in getMnhInfo do writeln(s);
      exit(false);
    end;
    wantConsoleAdapter:=wantConsoleAdapter or wantHelpDisplay or (length(cmdLineParsingErrors)>0);
    if wantConsoleAdapter then adapters^.addConsoleOutAdapter;

     contexts.suppressBeep:=suppressBeep;
    scriptFileName:=getFileToInterpretFromCommandLine;
    if scriptFileName<>'' then scriptFileName:=ChangeFileExt(scriptFileName,'');

    for i:=0 to length(deferredAdapterCreations)-1 do with deferredAdapterCreations[i] do adapters^.addOutfile(replaceAll(nameAndOption,'?',scriptFileName),appending);
    if guiAdapterOrNil<>nil then guiAdapterOrNil^.outputBehavior:=defaultOutputBehavior;

    if length(cmdLineParsingErrors)>0 then begin
      if wantHelpDisplay then displayHelp(adapters);

      if adapters^.isCollecting(mt_el4_systemError)
      then adapters^.postTextMessage(mt_el4_systemError,C_nilTokenLocation,cmdLineParsingErrors)
      else for s in cmdLineParsingErrors do writeln(s);
      ExitCode:=1;
      result:=false;
    end;
  end;

end.

