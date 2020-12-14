UNIT commandLineParameters;
INTERFACE
USES myGenerics,mnh_settings,out_adapters,serializationUtil,mnh_constants
     {$ifdef fullVersion}
     ,funcs
     {$endif};
TYPE
  T_parsingState=object
    cmdLineParsingErrors:T_arrayOfString;
    parsingState:(pst_initial,
                  pst_parsingOutFileRewrite,
                  pst_parsingOutFileAppend,
                  pst_parsingSideEffectProfile,
                  pst_parsingFileToEdit,
                  pst_parsingScriptParameters);
    PROCEDURE clear;
    PROCEDURE logCmdLineParsingError(CONST s: string);
  end;

  T_mnhExecutionOptions=object(T_serializable)
    verbosityString:string;
    flags:set of T_cmdLineFlag;
    executor:string;
    deferredAdapterCreations:array of T_textFileAdapterSpecification;
    sideEffectProfile:longint;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clear;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE copyFrom(CONST other:T_mnhExecutionOptions);
    FUNCTION parseSingleMnhParameter(CONST param: string; VAR parsingState:T_parsingState): boolean;
    {$ifdef fullVersion}
    PROCEDURE initFromShebang(CONST shebangLine:string; CONST requires:T_sideEffects);
    FUNCTION getShebang:ansistring;
    FUNCTION getCommandLineArgumentsArray:T_arrayOfString;
    {$endif}
    FUNCTION parseShebangParameters(CONST fileName: string; VAR parsingState:T_parsingState): boolean;
    FUNCTION executeCommand:boolean;

    FUNCTION isCallLightFlavour:boolean;
    PROCEDURE setCallLightFlavour(CONST b:boolean);
    PROPERTY callLightFlavour:boolean read isCallLightFlavour write setCallLightFlavour;
    FUNCTION allowedSideEffects:T_sideEffects;
  end;

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
      FUNCTION getConsoleMode:T_consoleOutMode;
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
  FLAG_STDOUT       ='-stdout';
  FLAG_STDERR       ='-stderr';
  FLAG_EDIT         ='-edit';
  FLAG_TEXT:array[T_cmdLineFlag] of string=(FLAG_GUI         ,
                                            FLAG_QUIET       ,
                                            FLAG_SILENT      ,
                                            FLAG_HEADLESS    ,
                                            FLAG_PAUSE_ON_ERR,
                                            FLAG_PAUSE_ALWAYS,
                                            FLAG_PROFILE     ,
                                            FLAG_SHOW_HELP   ,
                                            FLAG_EXEC_CMD    ,
                                            FLAG_SHOW_INFO   ,
                                            FLAG_STDOUT,
                                            FLAG_STDERR);

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
     messageFormatting;

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
    end else result:=C_EMPTY_STRING_ARRAY;
  end;

PROCEDURE T_parsingState.clear;
  begin
    setLength(cmdLineParsingErrors,0);
    parsingState:=pst_initial;
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
    sideEffectProfile:=0;
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
    sideEffectProfile:=other.sideEffectProfile;
    setLength(deferredAdapterCreations,length(other.deferredAdapterCreations));
    for k:=0 to length(deferredAdapterCreations)-1 do
      deferredAdapterCreations[k].copy(other.deferredAdapterCreations[k]);
  end;

FUNCTION T_mnhExecutionOptions.parseSingleMnhParameter(CONST param: string; VAR parsingState: T_parsingState): boolean;
  PROCEDURE addAdapter();
    VAR specification:T_textFileAdapterSpecification;
        i:longint;
        messageTypesToCollect:T_messageTypeSet;
    begin
      //Create specification
      messageTypesToCollect:=stringToMessageTypeSet(verbosityString);
      specification.forceNewFile:=parsingState.parsingState=pst_parsingOutFileRewrite;
      specification.setFilenameAndOptions(param,messageTypesToCollect);
      new(P_logFormatter(specification.formatter),create);
      //Try to merge into existing adapters
      for i:=0 to length(deferredAdapterCreations)-1 do if specification.canMergeInto(deferredAdapterCreations[i],messageTypesToCollect) then exit;
      //If merging did not work: create additional adapter
      i:=length(deferredAdapterCreations);
      setLength(deferredAdapterCreations,i+1);
      deferredAdapterCreations[i]:=specification;
    end;

  VAR app:string;
      i:longint;
      sideEffectProfileIndex:longint=-1;
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
        if param=FLAG_EDIT         then begin parsingState.parsingState:=pst_parsingFileToEdit; exit(true); end;
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
        if param=FLAG_STDERR       then begin include(flags,clf_FORCE_STDERR); Exclude(flags,clf_FORCE_STDOUT); exit(true); end;
        if param=FLAG_STDOUT       then begin include(flags,clf_FORCE_STDOUT); Exclude(flags,clf_FORCE_STDERR); exit(true); end;
        //state changes:
        if param='-out'            then begin parsingState.parsingState:=pst_parsingOutFileRewrite;     exit(true); end;
        if param='+out'            then begin parsingState.parsingState:=pst_parsingOutFileAppend;      exit(true); end;
        if param='-restrict'       then begin parsingState.parsingState:=pst_parsingSideEffectProfile;  exit(true); end;
      end;
      pst_parsingOutFileAppend,pst_parsingOutFileRewrite: begin
        addAdapter();
        parsingState.parsingState:=pst_initial;
        exit(true);
      end;
      pst_parsingSideEffectProfile: begin
        for i:=0 to length(C_sideEffectProfile)-1 do if C_sideEffectProfile[i].name=param then sideEffectProfileIndex:=i;
        if sideEffectProfileIndex=-1 then begin
          app:='';
          for i:=0 to length(C_sideEffectProfile)-1 do if C_sideEffectProfile[i].name<>'' then app+=', '+C_sideEffectProfile[i].name;
          parsingState.logCmdLineParsingError('Invalid profile "'+param+'". Must be one of '+copy(app,3,length(app)-2));
        end else begin
          sideEffectProfile:=sideEffectProfileIndex;
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
    clear;
    i:=1;
    while (i<=paramCount) do begin
      if mnhExecutionOptions.parseSingleMnhParameter(paramStr(i),parsingState)
      then inc(i) else case parsingState.parsingState of
        pst_initial: begin
          fileOrCommandToInterpret:=paramStr(i);
          if not(mnhExecutionOptions.executeCommand) then begin
            if fileExists(fileOrCommandToInterpret) then begin
              mnhExecutionOptions.parseShebangParameters(fileOrCommandToInterpret,parsingState);
              fileOrCommandToInterpret:=expandFileName(fileOrCommandToInterpret);
              if parsingState.parsingState=pst_parsingFileToEdit then begin
                {$ifdef fullVersion}
                  addFileToOpen(paramStr(i));
                {$else}
                  include(mnhExecutionOptions.flags,clf_GUI);
                {$endif}
                fileOrCommandToInterpret:='';
              end;
            end else parsingState.logCmdLineParsingError('Given script does not exist: '+fileOrCommandToInterpret);
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

FUNCTION T_mnhExecutionOptions.parseShebangParameters(CONST fileName: string; VAR parsingState: T_parsingState): boolean;
  VAR parameters:T_arrayOfString;
      k:longint;
  begin
    parameters:=parseShebang(fileName);
    for k:=1 to length(parameters)-1 do
    if not(parseSingleMnhParameter(parameters[k],parsingState)) then begin
      parsingState.logCmdLineParsingError('Invalid parameter/switch given by shebang: "'+parameters[k]+'"');
      exit(false);
    end;
    if parsingState.parsingState=pst_initial then parsingState.parsingState:=pst_parsingScriptParameters;
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

FUNCTION T_mnhExecutionOptions.allowedSideEffects:T_sideEffects;
  begin
    result:=C_sideEffectProfile[sideEffectProfile].allowed;
  end;

FUNCTION T_commandLineParameters.getFileToInterpretFromCommandLine: ansistring;
  begin if mnhExecutionOptions.executeCommand then result:='' else result:=fileOrCommandToInterpret; end;

FUNCTION T_commandLineParameters.getConsoleMode:T_consoleOutMode;
  begin
    if      clf_FORCE_STDERR in mnhExecutionOptions.flags then result:=com_stderr_only
    else if clf_FORCE_STDOUT in mnhExecutionOptions.flags then result:=com_stdout_only
                                                          else result:=com_normal;
  end;

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
PROCEDURE T_mnhExecutionOptions.initFromShebang(CONST shebangLine: string; CONST requires: T_sideEffects);
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

    if requires*C_sideEffectsRequiringGui<>[] then include(flags,clf_GUI);
    if se_sound in requires then Exclude(flags,clf_SILENT);
    if se_input in requires then Exclude(flags,clf_HEADLESS);
    setCallLightFlavour(not((clf_GUI in flags) or
                            (requires*C_sideEffectsRequiringGui<>[]) or
                            (executor=settings.fullFlavourLocation)));
  end;

FUNCTION T_mnhExecutionOptions.getShebang: ansistring;
  VAR k:longint;
      f:T_cmdLineFlag;
  begin
    result:='#!'+executor;
    if verbosityString<>DEF_VERBOSITY_STRING then result+=' -v'+verbosityString;
    for f in flags do result+=' '+FLAG_TEXT[f];
    if sideEffectProfile<>0 then result+=' -restrict '+C_sideEffectProfile[sideEffectProfile].name;
    for k:=0 to length(deferredAdapterCreations)-1 do begin
      if deferredAdapterCreations[k].forceNewFile
      then result+=' -out '
      else result+=' +out ';
      result+=deferredAdapterCreations[k].getFilenameAndOptions;
    end;
  end;

FUNCTION T_mnhExecutionOptions.getCommandLineArgumentsArray: T_arrayOfString;
  VAR k:longint;
      f:T_cmdLineFlag;
  begin
    setLength(result,0);
    if verbosityString<>DEF_VERBOSITY_STRING then append(result,'-v'+verbosityString);
    for f in flags do append(result,FLAG_TEXT[f]);
    if sideEffectProfile<>0 then begin
      append(result,'-restrict ');
      append(result,C_sideEffectProfile[sideEffectProfile].name);
    end;
    for k:=0 to length(deferredAdapterCreations)-1 do begin
      if deferredAdapterCreations[k].forceNewFile
      then append(result,'-out')
      else append(result,'+out');
      append(result,deferredAdapterCreations[k].getFilenameAndOptions);
    end;
  end;

{$endif}

PROCEDURE displayHelp(CONST adapters:P_messages);
  PROCEDURE wl(CONST s:string);
    begin
      if (adapters=nil) or not(adapters^.isCollecting(mt_printline))
      then writeln(s)
      else adapters^.postTextMessage(mt_printline,C_nilSearchTokenLocation,s);
    end;

  VAR s:string;
      i:longint;
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
    wl('  '+FLAG_HEADLESS+'         forbid user input');
    wl('  '+FLAG_EXEC_CMD+'              directly execute the following command');
    wl('  '+FLAG_SHOW_INFO+'             show info; same as '+FLAG_EXEC_CMD+' mnhInfo.print');
    wl('  '+FLAG_PROFILE+'          do a profiling run - implies -vt');
    wl('  '+FLAG_EDIT+' <filename>  opens file(s) in editor instead of interpreting directly');
    wl('  -out <filename>[(options)] write output to the given file; Options are verbosity options');
    wl('                    if no options are given, the global output settings will be used');
    wl('  +out <filename>[(options)]  As -out but appending to the file if existing.');
    wl('  -restrict <profileName>     Restricts the allowed side effects. Available restriction profiles:');
    for i:=0 to length(C_sideEffectProfile)-1 do if C_sideEffectProfile[i].name<>'' then
    wl('                                '+C_sideEffectProfile[i].name);
    wl('  '+FLAG_QUIET+'            disable console output');
    wl('  '+FLAG_STDOUT+'          write all console output to stdout');
    wl('  '+FLAG_STDERR+'          write all console output to stderr');
    wl('  '+FLAG_SILENT+'           suppress beeps');
    wl('  '+FLAG_PAUSE_ALWAYS+'       pauses after script execution');
    wl('  '+FLAG_PAUSE_ON_ERR+'     pauses after script execution if an error ocurred');
  end;

FUNCTION T_commandLineParameters.applyAndReturnOk(CONST adapters: P_messagesDistributor; CONST initAdaptersForGui: boolean): boolean;
  VAR i:longint;
      scriptFileName:string;
      s:ansistring;
      consoleMessageTypes:T_messageTypeSet;
  begin
    result:=true;
    if not(initAdaptersForGui) and (clf_SHOW_INFO in mnhExecutionOptions.flags) then begin
      for s in getMnhInfo do writeln(s);
      exit(false);
    end;
    if (clf_SHOW_HELP in mnhExecutionOptions.flags) or (length(parsingState.cmdLineParsingErrors)>0) then Exclude(mnhExecutionOptions.flags,clf_QUIET);
    consoleMessageTypes:=stringToMessageTypeSet(mnhExecutionOptions.verbosityString);
    if not(clf_QUIET in mnhExecutionOptions.flags) and not(initAdaptersForGui) then begin
      {$ifdef fullVersion}
      if (clf_PROFILE in mnhExecutionOptions.flags) then include(consoleMessageTypes,mt_profile_call_info);
      {$endif}
      adapters^.addConsoleOutAdapter(consoleMessageTypes,getConsoleMode,@defaultConsoleFormatter);
    end;
    contexts.suppressBeep:=clf_SILENT in mnhExecutionOptions.flags;

    scriptFileName:=getFileToInterpretFromCommandLine;
    if scriptFileName<>'' then scriptFileName:=ChangeFileExt(scriptFileName,'');
    for i:=0 to length(mnhExecutionOptions.deferredAdapterCreations)-1 do begin
      mnhExecutionOptions.deferredAdapterCreations[i].applyScriptName(scriptFileName);
      adapters^.addOutfile(mnhExecutionOptions.deferredAdapterCreations[i]);
    end;

    if not(initAdaptersForGui) and (length(parsingState.cmdLineParsingErrors)>0) then begin
      if (clf_SHOW_HELP in mnhExecutionOptions.flags) then displayHelp(adapters);
      if adapters^.isCollecting(mt_el4_systemError)
      then adapters^.postTextMessage(mt_el4_systemError,C_nilSearchTokenLocation,parsingState.cmdLineParsingErrors)
      else for s in parsingState.cmdLineParsingErrors do writeln(s);
      ExitCode:=1;
      result:=false;
    end;
  end;

end.

