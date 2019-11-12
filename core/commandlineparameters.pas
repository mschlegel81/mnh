UNIT commandLineParameters;
INTERFACE
USES myGenerics,mnh_settings,out_adapters,serializationUtil
     {$ifdef fullVersion}
     ,funcs
     {$endif};
TYPE

  { T_parsingState }

  T_parsingState=object
    cmdLineParsingErrors:T_arrayOfString;
    parsingState:(pst_initial,
                  pst_parsingOutFileRewrite,
                  pst_parsingOutFileAppend,
                  pst_parsingFileToEdit,
                  pst_parsingScriptParameters);
    {$ifdef UNIX}
    hasAnyMnhParameter:boolean;
    {$endif}
    PROCEDURE clear;
    PROCEDURE logCmdLineParsingError(CONST s: string);
  end;

  { T_mnhExecutionOptions }

  T_mnhExecutionOptions=object(T_serializable)
    verbosityString:string;
    flags:set of T_cmdLineFlag;
    executor:string;
    deferredAdapterCreations:array of record
      nameAndOption:string;
      appending:boolean;
    end;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clear;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE copyFrom(CONST other:T_mnhExecutionOptions);
    FUNCTION parseSingleMnhParameter(CONST param: string; VAR parsingState:T_parsingState): boolean;
    {$ifdef fullVersion}
    PROCEDURE initFromShebang(CONST shebangLine:string; CONST requires:T_specialFunctionRequirements);
    FUNCTION getShebang:ansistring;
    FUNCTION getCommandLineArgumentsArray:T_arrayOfString;
    {$endif}
    FUNCTION parseShebangParameters(CONST fileName: string; VAR parsingState:T_parsingState): boolean;
    FUNCTION executeCommand:boolean;

    FUNCTION isCallLightFlavour:boolean;
    PROCEDURE setCallLightFlavour(CONST b:boolean);
    PROPERTY callLightFlavour:boolean read isCallLightFlavour write setCallLightFlavour;
  end;

  { T_commandLineParameters }

  T_commandLineParameters=object
      mnhExecutionOptions:T_mnhExecutionOptions;

      fileOrCommandToInterpret:ansistring;
      mainParameters:T_arrayOfString;
      {$ifdef fullVersion}
      filesToOpenInEditor:T_arrayOfString;
      {$endif}

      PROCEDURE clear;
      PROCEDURE initFromCommandLine;
      FUNCTION applyAndReturnOk(CONST adapters: P_messagesDistributor; CONST initAdaptersForGui:boolean=false):boolean;

      FUNCTION getCommandToInterpretFromCommandLine: ansistring;
      FUNCTION getFileToInterpretFromCommandLine: ansistring;
      PROCEDURE pauseIfConfigured(CONST errorOcurred:boolean);
      {$ifdef fullVersion}
      PROCEDURE initFromIde(CONST scriptName,parameters:string);
      FUNCTION getCommandLineArgumentsArray:T_arrayOfString;
      {$endif}
    private
      parsingState:T_parsingState;
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
  FLAG_TEXT:array[T_cmdLineFlag] of string=(FLAG_GUI         ,
                                            FLAG_QUIET       ,
                                            FLAG_SILENT      ,
                                            FLAG_HEADLESS    ,
                                            FLAG_PAUSE_ON_ERR,
                                            FLAG_PAUSE_ALWAYS,
                                            FLAG_PROFILE     ,
                                            FLAG_SHOW_HELP   ,
                                            FLAG_EXEC_CMD    ,
                                            FLAG_SHOW_INFO);

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

PROCEDURE T_parsingState.clear;
  begin
    setLength(cmdLineParsingErrors,0);
    parsingState:=pst_initial;
    {$ifdef UNIX}
    hasAnyMnhParameter:=false;
    {$endif}
  end;

PROCEDURE T_parsingState.logCmdLineParsingError(CONST s: string);
  begin
    append(cmdLineParsingErrors,s);
  end;

CONSTRUCTOR T_mnhExecutionOptions.create;
  begin clear; end;

DESTRUCTOR T_mnhExecutionOptions.destroy;
  begin clear; end;

PROCEDURE T_mnhExecutionOptions.clear;
  begin
    verbosityString:=DEF_VERBOSITY_STRING;
    setLength(deferredAdapterCreations,0);
    flags:=[];
    executor:=settings.fullFlavourLocation;
    if (executor='') or not(fileExists(executor))
    then executor:=settings.lightFlavourLocation;
  end;

FUNCTION T_mnhExecutionOptions.getSerialVersion:dword;
  begin result:=23413; end;

FUNCTION T_mnhExecutionOptions.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    clear;
    stream.read(flags,sizeOf(flags));
    callLightFlavour:=stream.readBoolean;
    verbosityString:=stream.readAnsiString;
    result:=stream.allOkay;
  end;

PROCEDURE T_mnhExecutionOptions.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    stream.write(flags,sizeOf(flags));
    stream.writeBoolean(callLightFlavour);
    stream.writeAnsiString(verbosityString);
  end;

PROCEDURE T_mnhExecutionOptions.copyFrom(CONST other: T_mnhExecutionOptions);
  VAR k:longint;
  begin
    clear;
    flags:=other.flags;
    executor:=other.executor;
    verbosityString:=other.verbosityString;
    setLength(deferredAdapterCreations,length(other.deferredAdapterCreations));
    for k:=0 to length(deferredAdapterCreations)-1 do begin
      deferredAdapterCreations[k].appending    :=other.deferredAdapterCreations[k].appending;
      deferredAdapterCreations[k].nameAndOption:=other.deferredAdapterCreations[k].nameAndOption;
    end;
  end;

FUNCTION T_mnhExecutionOptions.parseSingleMnhParameter(CONST param: string; VAR parsingState: T_parsingState): boolean;
  VAR app:string;
  begin
    result:=false;
    case parsingState.parsingState of
      pst_initial: begin
        //verbosity:
        if startsWith(param,'-v') then begin
          app:=copy(param,3,length(param)-2);
          if app='' then app:='v';
          verbosityString+=app;
          exit(true);
        end;
        //flags:
        if param=FLAG_PROFILE      then begin include(flags,{$ifdef fullVersion}clf_PROFILE{$else}clf_GUI{$endif}); exit(true); end;
        if param=FLAG_GUI          then begin include(flags,clf_GUI         ); exit(true); end;
        if param=FLAG_QUIET        then begin include(flags,clf_QUIET       ); exit(true); end;
        if param=FLAG_SILENT       then begin include(flags,clf_SILENT      ); exit(true); end;
        if param=FLAG_HEADLESS     then begin include(flags,clf_HEADLESS    ); exit(true); end;
        if param=FLAG_PAUSE_ON_ERR then begin include(flags,clf_PAUSE_ON_ERR); exit(true); end;
        if param=FLAG_PAUSE_ALWAYS then begin include(flags,clf_PAUSE_ALWAYS); exit(true); end;
        if param=FLAG_EXEC_CMD     then begin include(flags,clf_EXEC_CMD    ); exit(true); end;
        if param=FLAG_SHOW_HELP    then begin include(flags,clf_SHOW_HELP   ); exit(true); end;
        if param=FLAG_SHOW_INFO    then begin include(flags,clf_SHOW_INFO   ); exit(true); end;
        //state changes:
        if param='-out'            then begin parsingState.parsingState:=pst_parsingOutFileRewrite; exit(true); end;
        if param='+out'            then begin parsingState.parsingState:=pst_parsingOutFileAppend;  exit(true); end;
      end;
      pst_parsingOutFileAppend,pst_parsingOutFileRewrite: begin
        setLength(deferredAdapterCreations,length(deferredAdapterCreations)+1);
        with deferredAdapterCreations[length(deferredAdapterCreations)-1] do begin
          appending:=(parsingState.parsingState=pst_parsingOutFileAppend);
          nameAndOption:=param;
        end;
        parsingState.parsingState:=pst_initial;
        exit(true);
      end;
    end;
    result:=false;
  end;

PROCEDURE T_commandLineParameters.clear;
  begin
    parsingState.clear;
    fileOrCommandToInterpret:='';
    setLength(mainParameters,0);
    {$ifdef fullVersion}
    setLength(filesToOpenInEditor,0);
    {$endif}
  end;

PROCEDURE T_commandLineParameters.initFromCommandLine;
  {$ifdef fullVersion}
  PROCEDURE addFileToOpen(CONST pathOrPattern:string);
    VAR info:T_fileInfo;
    begin
      if containsPlaceholder(pathOrPattern) then begin
        for info in findFileInfo(pathOrPattern) do if (info.size>0) and not(aDirectory in info.attributes) then append(filesToOpenInEditor,expandFileName(info.filePath));
      end else append(filesToOpenInEditor,expandFileName(pathOrPattern));
    end;
  {$endif}
  VAR i:longint;
  begin
    //    result:=false;
    //    if directExecutionMode then begin
    //      fileOrCommandToInterpret+=' '+param;
    //      exit(true);
    //    end;
    //    if fileOrCommandToInterpret<>'' then exit(false);
    //    case parsingState of
    //      pst_initial: begin
    //        if (param='-edit') then begin
    //          parsingState:=pst_parsingFileToEdit;
    //          exit(true);
    //        end;
    //      end;
    //    end;
    //
    clear;
    i:=1;
    while (i<=paramCount) do begin
      if mnhExecutionOptions.parseSingleMnhParameter(paramStr(i),parsingState)
      then inc(i) else case parsingState.parsingState of
        pst_initial: begin
          if (paramStr(i)='-edit')
          then parsingState.parsingState:=pst_parsingFileToEdit
          else begin
            fileOrCommandToInterpret:=paramStr(i);
            if not(mnhExecutionOptions.executeCommand) then begin
              if fileExists(fileOrCommandToInterpret) then begin
                mnhExecutionOptions.parseShebangParameters(fileOrCommandToInterpret,parsingState);
                fileOrCommandToInterpret:=expandFileName(fileOrCommandToInterpret);
              end else parsingState.logCmdLineParsingError('Given script does not exist: '+fileOrCommandToInterpret);
            end;
            parsingState.parsingState:=pst_parsingScriptParameters;
          end;
          inc(i);
        end;
        pst_parsingFileToEdit: begin
          {$ifdef fullVersion}
            addFileToOpen(paramStr(i));
          {$else}
            include(mnhExecutionOptions.flags,clf_GUI);
          {$endif}
          inc(i);
        end;
        pst_parsingScriptParameters: begin
          if mnhExecutionOptions.executeCommand
          then fileOrCommandToInterpret+=' '+paramStr(i)
          else append(mainParameters,paramStr(i));
          inc(i);
        end;
        else parsingState.logCmdLineParsingError('Out of context: '+paramStr(i));
      end;
    end;
  end;

FUNCTION T_mnhExecutionOptions.parseShebangParameters(CONST fileName: string;
  VAR parsingState: T_parsingState): boolean;
  VAR parameters:T_arrayOfString;
      k:longint;
  begin
    {$ifdef UNIX}
    //To prevent repeated parsing of the same shebang under Linux/UNIX systems
    if hasAnyMnhParameter then exit(true);
    {$endif}
    parameters:=parseShebang(fileName);
    for k:=1 to length(parameters)-1 do
    if not(parseSingleMnhParameter(parameters[k],parsingState)) then begin
      parsingState.logCmdLineParsingError('Invalid parameter/switch given by shebang: "'+parameters[k]+'"');
      exit(false);
    end;
    result:=true;
  end;

FUNCTION T_mnhExecutionOptions.executeCommand: boolean;
  begin
    result:=clf_EXEC_CMD in flags;
  end;

FUNCTION T_mnhExecutionOptions.isCallLightFlavour: boolean;
  begin
    result:=executor=settings.lightFlavourLocation;
  end;

PROCEDURE T_mnhExecutionOptions.setCallLightFlavour(CONST b: boolean);
  begin
    if b and fileExists(settings.lightFlavourLocation)
    then executor:=settings.lightFlavourLocation
    else executor:=settings.fullFlavourLocation;
  end;

FUNCTION T_commandLineParameters.getFileToInterpretFromCommandLine: ansistring;
  begin if mnhExecutionOptions.executeCommand then result:='' else result:=fileOrCommandToInterpret; end;

PROCEDURE T_commandLineParameters.pauseIfConfigured(CONST errorOcurred:boolean);
  begin
    if (clf_PAUSE_ALWAYS in mnhExecutionOptions.flags) or (clf_PAUSE_ON_ERR in mnhExecutionOptions.flags) and errorOcurred
    then begin
      {$ifdef fullVersion}
      showConsole;
      {$endif}
      Exclude(mnhExecutionOptions.flags,clf_PAUSE_ALWAYS);
      Exclude(mnhExecutionOptions.flags,clf_PAUSE_ON_ERR);
      if ExitCode=0 then write('Evaluation finished. Press enter to exit ')
                    else write('Evaluation finished with exit code ',ExitCode,'. Press enter to exit ');
      readln;
    end;
  end;

{$ifdef fullVersion}
PROCEDURE T_commandLineParameters.initFromIde(CONST scriptName,parameters:string);
  begin
    fileOrCommandToInterpret:=scriptName;
    if trim(parameters)=''
    then setLength(mainParameters,0)
    else mainParameters:=splitCommandLine(parameters);
  end;

FUNCTION T_commandLineParameters.getCommandLineArgumentsArray:T_arrayOfString;
  begin
    result:=mnhExecutionOptions.getCommandLineArgumentsArray;
    append(result,fileOrCommandToInterpret);
    append(result,mainParameters);
  end;
{$endif}

FUNCTION T_commandLineParameters.getCommandToInterpretFromCommandLine: ansistring;
  begin if mnhExecutionOptions.executeCommand then result:=fileOrCommandToInterpret else result:=''; end;

{$ifdef fullVersion}
PROCEDURE T_mnhExecutionOptions.initFromShebang(CONST shebangLine: string; CONST requires: T_specialFunctionRequirements);
  VAR k:longint;
      parameters:T_arrayOfString;
      parsingState:T_parsingState;
  begin
    parsingState.clear;
    clear;
    if shebangLine<>'' then begin
      parameters:=splitCommandLine(copy(shebangLine,3,length(shebangLine)-2));
      executor:=parameters[0];
      for k:=1 to length(parameters)-1 do parseSingleMnhParameter(parameters[k],parsingState);
    end;
    if sfr_needs_gui in requires then include(flags,clf_GUI);
    if sfr_beeps     in requires then Exclude(flags,clf_SILENT);
    if sfr_asks      in requires then Exclude(flags,clf_HEADLESS);
    setCallLightFlavour(not((clf_GUI in flags) or
                            (sfr_needs_full_version in requires) or
                            (executor=settings.fullFlavourLocation)));
  end;

FUNCTION T_mnhExecutionOptions.getShebang: ansistring;
  VAR k:longint;
      f:T_cmdLineFlag;
  begin
    result:='#!'+executor;
    if verbosityString<>DEF_VERBOSITY_STRING then result+=' -v'+verbosityString;
    for f in flags do result+=' '+FLAG_TEXT[f];
    for k:=0 to length(deferredAdapterCreations)-1 do begin
      if deferredAdapterCreations[k].appending
      then result+=' +out '+deferredAdapterCreations[k].nameAndOption
      else result+=' -out '+deferredAdapterCreations[k].nameAndOption;
    end;
  end;

FUNCTION T_mnhExecutionOptions.getCommandLineArgumentsArray: T_arrayOfString;
  VAR k:longint;
      f:T_cmdLineFlag;
  begin
    setLength(result,0);
    if verbosityString<>DEF_VERBOSITY_STRING then append(result,' -v'+verbosityString);
    for f in flags do append(result, FLAG_TEXT[f]);
    for k:=0 to length(deferredAdapterCreations)-1 do begin
      if deferredAdapterCreations[k].appending
      then append(result,'+out')
      else append(result,'-out');
      append(result,deferredAdapterCreations[k].nameAndOption);
    end;
  end;

{$endif}

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

FUNCTION T_commandLineParameters.applyAndReturnOk(CONST adapters: P_messagesDistributor; CONST initAdaptersForGui: boolean): boolean;
  VAR i:longint;
      scriptFileName:string;
      s:ansistring;
  begin
    result:=true;
    if not(initAdaptersForGui) and (clf_SHOW_INFO in mnhExecutionOptions.flags) then begin
      for s in getMnhInfo do writeln(s);
      exit(false);
    end;
    if (clf_SHOW_HELP in mnhExecutionOptions.flags) or (length(parsingState.cmdLineParsingErrors)>0) then Exclude(mnhExecutionOptions.flags,clf_QUIET);
    if not(clf_QUIET in mnhExecutionOptions.flags) and not(initAdaptersForGui) then adapters^.addConsoleOutAdapter;
    contexts.suppressBeep:=suppressBeep;

    scriptFileName:=getFileToInterpretFromCommandLine;
    if scriptFileName<>'' then scriptFileName:=ChangeFileExt(scriptFileName,'');
    for i:=0 to length(mnhExecutionOptions.deferredAdapterCreations)-1 do with mnhExecutionOptions.deferredAdapterCreations[i] do adapters^.addOutfile(replaceAll(nameAndOption,'?',scriptFileName),appending);

    if not(initAdaptersForGui) and (length(parsingState.cmdLineParsingErrors)>0) then begin
      if (clf_SHOW_HELP in mnhExecutionOptions.flags) then displayHelp(adapters);
      if adapters^.isCollecting(mt_el4_systemError)
      then adapters^.postTextMessage(mt_el4_systemError,C_nilTokenLocation,parsingState.cmdLineParsingErrors)
      else for s in parsingState.cmdLineParsingErrors do writeln(s);
      ExitCode:=1;
      result:=false;
    end;
  end;

end.

