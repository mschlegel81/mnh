UNIT commandLineParameters;
INTERFACE
USES myGenerics,mnh_settings,out_adapters,serializationUtil,mnh_constants,mnh_messages
     {$ifdef fullVersion}
     ,funcs
     {$endif};
TYPE
  T_parsingState=object
    cmdLineParsingErrors:T_arrayOfString;
    parsingState:(pst_initial,
                  pst_parsingOutFileRewrite,
                  pst_parsingOutFileAppend,
                  pst_parsingLogFileRewrite,
                  pst_parsingLogFileAppend,
                  pst_parsingLogDateFormat,
                  pst_parsingLogLocationLength,
                  pst_parsingSideEffectProfile,
                  pst_parsingFileToEdit,
                  pst_parsingScriptParameters);
    PROCEDURE clear;
    PROCEDURE logCmdLineParsingError(CONST s: string);
  end;

  P_mnhExecutionOptions=^T_mnhExecutionOptions;
  T_mnhExecutionOptions=object(T_serializable)
    verbosityString:string;
    customLogLocationLength:longint;
    customDateFormat:string;
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
  FLAG_PRINT_AS_LOG ='-convertPrintToLog';
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
                                            FLAG_STDERR,
                                            FLAG_PRINT_AS_LOG);
  MARK_OUT_APPEND ='+out';
  MARK_LOG_APPEND ='+log';
  MARK_OUT_REWRITE='-out';
  MARK_LOG_REWRITE='-log';
  MARK_RESTRICT   ='-restrict';
  MARK_DATE_FMT   ='-logDateFmt';
  MARK_LOCLEN     ='-logLocationLength';

FUNCTION getFormatterFor(CONST deferredAdapterCreation:T_textFileAdapterSpecification):P_messageFormatProvider;
PROCEDURE displayHelp(CONST adapters:P_messages);
IMPLEMENTATION
USES sysutils,
     myStringUtil,
     contexts,
     funcs_mnh,
     fileWrappers,
     mySys,
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
    customDateFormat:='';
    customLogLocationLength:=maxLongint;
    if (executor='') or not(fileExists(executor))
    then executor:=settings.lightFlavourLocation;
  end;

FUNCTION T_mnhExecutionOptions.getSerialVersion:dword;
  begin result:=23414; end;

FUNCTION T_mnhExecutionOptions.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
      adapterCount:longint;
  begin
    clear;
    stream.read(flags,sizeOf(flags));
    callLightFlavour:=stream.readBoolean;
    verbosityString:=stream.readAnsiString;

    customLogLocationLength:=stream.readLongint   ;
    customDateFormat:=stream.readAnsiString;
    sideEffectProfile      :=stream.readLongint   ;
    adapterCount:=stream.readNaturalNumber;
    result:=stream.allOkay;
    if result then begin
      setLength(deferredAdapterCreations,adapterCount);
      for i:=0 to length(deferredAdapterCreations)-1 do result:=result and deferredAdapterCreations[i].loadFromStream(stream);
    end;
    if not(result) then clear;
  end;

PROCEDURE T_mnhExecutionOptions.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
  begin
    stream.write(flags,sizeOf(flags));
    stream.writeBoolean(callLightFlavour);
    stream.writeAnsiString(verbosityString);

    stream.writeLongint(customLogLocationLength);
    stream.writeAnsiString(customDateFormat);
    stream.writeLongint(sideEffectProfile);
    stream.writeNaturalNumber(length(deferredAdapterCreations));
    for i:=0 to length(deferredAdapterCreations)-1 do deferredAdapterCreations[i].saveToStream(stream);
  end;

PROCEDURE T_mnhExecutionOptions.copyFrom(CONST other: T_mnhExecutionOptions);
  VAR k:longint;
  begin
    clear;
    flags:=other.flags;
    executor:=other.executor;
    verbosityString:=other.verbosityString;
    customLogLocationLength:=other.customLogLocationLength;
    customDateFormat:=other.customDateFormat;
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
      specification.setDefaults;
      specification.forceNewFile:=parsingState.parsingState in [pst_parsingOutFileRewrite,pst_parsingLogFileRewrite];
      specification.setFilenameAndOptions(param,messageTypesToCollect);

      if parsingState.parsingState in [pst_parsingLogFileAppend,pst_parsingLogFileRewrite]
      then begin
        specification.useLogFormatter:=true;
        specification.handlePrintAsLog:=clf_TREAT_PRINT_AS_LOG in flags;
        specification.logDateFormat:=customDateFormat;
        specification.logLocationLen:=customLogLocationLength;
      end
      else specification.useLogFormatter:=false;

      //Try to merge into existing adapters
      for i:=0 to length(deferredAdapterCreations)-1 do if specification.canMergeInto(deferredAdapterCreations[i],messageTypesToCollect) then exit;
      //If merging did not work: create additional adapter
      i:=length(deferredAdapterCreations);
      setLength(deferredAdapterCreations,i+1);
      deferredAdapterCreations[i]:=specification;
    end;

  PROCEDURE parseDateFormat;
    begin
      try
        FormatDateTime(param,now);
        customDateFormat:=param;
      except
        parsingState.logCmdLineParsingError('Invalid date/time format "'+param+'"');
      end;
    end;

  PROCEDURE parseLogLocationLength;
    begin
      customLogLocationLength:=strToIntDef(param,7-maxLongint);
      if customLogLocationLength=7-maxLongint
      then parsingState.logCmdLineParsingError('Invalid location length "'+param+'"');
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
        if param=FLAG_PRINT_AS_LOG then begin include(flags,clf_TREAT_PRINT_AS_LOG); exit(true); end;
        //state changes:
        if param=MARK_OUT_REWRITE  then begin parsingState.parsingState:=pst_parsingOutFileRewrite;       exit(true); end;
        if param=MARK_OUT_APPEND   then begin parsingState.parsingState:=pst_parsingOutFileAppend;        exit(true); end;
        if param=MARK_LOG_REWRITE  then begin parsingState.parsingState:=pst_parsingLogFileRewrite;       exit(true); end;
        if param=MARK_LOG_APPEND   then begin parsingState.parsingState:=pst_parsingLogFileAppend;        exit(true); end;
        if param=MARK_RESTRICT     then begin parsingState.parsingState:=pst_parsingSideEffectProfile;    exit(true); end;
        if param=MARK_DATE_FMT     then begin parsingState.parsingState:=pst_parsingLogDateFormat;        exit(true); end;
        if param=MARK_LOCLEN       then begin parsingState.parsingState:=pst_parsingLogLocationLength; exit(true); end;
      end;
      pst_parsingLogDateFormat: begin
        parseDateFormat;
        parsingState.parsingState:=pst_initial;
        exit(true);
      end;
      pst_parsingLogLocationLength: begin
        parseLogLocationLength;
        parsingState.parsingState:=pst_initial;
        exit(true);
      end;
      pst_parsingOutFileAppend,pst_parsingOutFileRewrite,
      pst_parsingLogFileAppend,pst_parsingLogFileRewrite: begin
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
              if i=1 //If the script is the first parameter, then no parameters were given by the command line and parameters from shebang must be taken into account
              then mnhExecutionOptions.parseShebangParameters(fileOrCommandToInterpret,parsingState)
              else if parsingState.parsingState=pst_initial then parsingState.parsingState:=pst_parsingScriptParameters;
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
  VAR s:string;
  begin
    result:='#!'+executor;
    for s in getCommandLineArgumentsArray do begin
      if (pos(' ',s)>=1) or (length(s)=0)
      then result+=' "'+s+'"'
      else result+=' ' +s;
    end;
  end;

FUNCTION T_mnhExecutionOptions.getCommandLineArgumentsArray: T_arrayOfString;
  PROCEDURE appendAdapterSpecification(CONST s:T_textFileAdapterSpecification);
    begin
      if s.useLogFormatter then begin
        append(result,MARK_DATE_FMT);
        if s.logDateFormat=''
        then append(result,'""')
        else append(result,s.logDateFormat);
        append(result,MARK_LOCLEN);
        append(result,intToStr(s.logLocationLen));
        if s.forceNewFile
        then append(result,MARK_LOG_REWRITE)
        else append(result,MARK_LOG_APPEND);
      end else begin
        if s.forceNewFile
        then append(result,MARK_OUT_REWRITE)
        else append(result,MARK_OUT_APPEND);
      end;
      append(result,s.getFilenameAndOptions);
    end;

  VAR k:longint;
      f:T_cmdLineFlag;
      anyHandlePrintAsLog:boolean=false;
  begin
    setLength(result,0);
    if verbosityString<>DEF_VERBOSITY_STRING then append(result,'-v'+verbosityString);
    for f in flags do if f<>clf_TREAT_PRINT_AS_LOG then append(result,FLAG_TEXT[f]);
    if sideEffectProfile<>0 then begin
      append(result,MARK_RESTRICT);
      append(result,C_sideEffectProfile[sideEffectProfile].name);
    end;
    for k:=0 to length(deferredAdapterCreations)-1 do
    if deferredAdapterCreations[k].handlePrintAsLog
    then anyHandlePrintAsLog:=true
    else appendAdapterSpecification(deferredAdapterCreations[k]);
    if anyHandlePrintAsLog then begin
      append(result,FLAG_PRINT_AS_LOG);
      for k:=0 to length(deferredAdapterCreations)-1 do
      if deferredAdapterCreations[k].handlePrintAsLog
      then appendAdapterSpecification(deferredAdapterCreations[k]);
    end;
  end;

{$endif}

PROCEDURE displayHelp(CONST adapters:P_messages);
  VAR linesToPrint:T_arrayOfString;
  PROCEDURE printOut;
    VAR s:string;
    begin
      if (adapters=nil) or not(adapters^.isCollecting(mt_printline))
      then for s in linesToPrint do writeln(s)
      else adapters^.postTextMessage(mt_printline,C_nilSearchTokenLocation,linesToPrint);
    end;
  VAR i:longint;
  begin
    linesToPrint:='MNH5 '+{$ifdef fullVersion}'(full'{$else}'(light'{$endif}+{$ifdef debugMode}',debug)'{$else}')'{$endif}+' by Martin Schlegel';
    append(linesToPrint,'compiled: '+{$I %DATE%}+' '+{$I %TIME%}+' with FPC'+{$I %FPCVERSION%}+' for '+{$I %FPCTARGET%});
    append(linesToPrint,'');
    append(linesToPrint,LOGO);
    append(linesToPrint,'');
    append(linesToPrint,'Accepted parameters: ');
    append(linesToPrint,'  [mnh_options] [('+FLAG_EXEC_CMD+' commandToExecute) | (filename [parameters])]');
    printOut;
    linesToPrint:=' '+C_tabChar+'filename'+C_tabChar+'if present the file is interpreted; parameters are passed if present';
    append(linesToPrint,C_tabChar+'-v[options]'+C_tabChar+'verbosity. options can consist of multiple characters.');
    append(linesToPrint,C_tabChar+C_tabChar+'Lowercase indicates enabling, uppercase indicates disabling.');
    append(linesToPrint,C_tabChar+C_tabChar+'  p/P  : print out');
    append(linesToPrint,C_tabChar+C_tabChar+'  i/I  : input echo');
    append(linesToPrint,C_tabChar+C_tabChar+'  d/D  : declaration echo');
    append(linesToPrint,C_tabChar+C_tabChar+'  o/O  : output echo');
    append(linesToPrint,C_tabChar+C_tabChar+'  e/E  : all echo; same as ido');
    append(linesToPrint,C_tabChar+C_tabChar+'  t/T  : timing info');
    append(linesToPrint,C_tabChar+C_tabChar+'  n/N  : notes (error level 1 only)');
    append(linesToPrint,C_tabChar+C_tabChar+'  w/W  : warnings (error level 2 only)');
    append(linesToPrint,C_tabChar+C_tabChar+'  u/U  : user defined notes, warnings and errors');
    append(linesToPrint,C_tabChar+C_tabChar+'  1..4 : override minimum error level');
    append(linesToPrint,C_tabChar+C_tabChar+'  v/V  : be verbose; same as pidot1 (uppercase means disabling all output)');
    append(linesToPrint,C_tabChar+FLAG_GUI      +C_tabChar+'force evaluation with GUI');
    append(linesToPrint,C_tabChar+FLAG_SHOW_HELP+C_tabChar+'display this help or help on the input file if present and quit');
    append(linesToPrint,C_tabChar+FLAG_HEADLESS +C_tabChar+'forbid user input');
    append(linesToPrint,C_tabChar+FLAG_EXEC_CMD +C_tabChar+'directly execute the following command');
    append(linesToPrint,C_tabChar+FLAG_SHOW_INFO+C_tabChar+'show info; same as '+FLAG_EXEC_CMD+' mnhInfo.print');
    append(linesToPrint,C_tabChar+FLAG_PROFILE  +C_tabChar+'do a profiling run - implies -vt');
    append(linesToPrint,C_tabChar+FLAG_EDIT+' <filename>'+C_tabChar+'opens file(s) in editor instead of interpreting directly');
    append(linesToPrint,C_tabChar+FLAG_QUIET       +C_tabChar+'disable console output');
    append(linesToPrint,C_tabChar+FLAG_STDOUT      +C_tabChar+'write all console output to stdout');
    append(linesToPrint,C_tabChar+FLAG_STDERR      +C_tabChar+'write all console output to stderr');
    append(linesToPrint,C_tabChar+FLAG_SILENT      +C_tabChar+'suppress beeps');
    append(linesToPrint,C_tabChar+FLAG_PAUSE_ALWAYS+C_tabChar+'pauses after script execution');
    append(linesToPrint,C_tabChar+FLAG_PAUSE_ON_ERR+C_tabChar+'pauses after script execution if an error ocurred');
    append(linesToPrint,C_tabChar+MARK_OUT_REWRITE+' <filename>[(options)]'+C_tabChar+'write output to the given file; Options are verbosity options');
    append(linesToPrint,C_tabChar+MARK_OUT_APPEND +' <filename>[(options)]'+C_tabChar+'as '+MARK_OUT_REWRITE+' but appending to the file if existing.');
    append(linesToPrint,C_tabChar+MARK_LOG_REWRITE+' <filename>[(options)]'+C_tabChar+'as '+MARK_OUT_REWRITE+' but with a log formatter');
    append(linesToPrint,C_tabChar+MARK_LOG_APPEND +' <filename>[(options)]'+C_tabChar+'as '+MARK_OUT_APPEND+' but with a log formatter');
    append(linesToPrint,C_tabChar+C_tabChar+'If "stdOut" or "stdErr" is given as a filename, output will be redirected to the appropriate console.');
    append(linesToPrint,C_tabChar+FLAG_PRINT_AS_LOG+C_tabChar+'convert all print output to log. Applies to all logs defined afterwards.');
    append(linesToPrint,C_tabChar+MARK_DATE_FMT+' <dateFormat>'+C_tabChar+'date/time format for log output. Applies to all logs defined afterwards.');
    append(linesToPrint,C_tabChar+C_tabChar+'Use a date/time format of "" to suppress time output');
    append(linesToPrint,C_tabChar+MARK_LOCLEN+' <length>'+C_tabChar+'location length for log output. Applies to all logs defined afterwards');
    append(linesToPrint,C_tabChar+C_tabChar+'A length of zero is interpreted as "full location"');
    append(linesToPrint,C_tabChar+MARK_RESTRICT+' <profileName>'+C_tabChar+'Restricts the allowed side effects. Available restriction profiles:');
    for i:=0 to length(C_sideEffectProfile)-1 do if C_sideEffectProfile[i].name<>'' then
    append(linesToPrint,C_tabChar+C_tabChar+'  '+C_sideEffectProfile[i].name);
    linesToPrint:=formatTabs(linesToPrint);
    printOut;
  end;

FUNCTION getFormatterFor(CONST deferredAdapterCreation:T_textFileAdapterSpecification):P_messageFormatProvider;
  begin
    if deferredAdapterCreation.useLogFormatter then begin
      new(P_logFormatter(result),create);
      with P_logFormatter(result)^ do begin
        timeFormat       :=deferredAdapterCreation.logDateFormat;
        maxLocationLength:=deferredAdapterCreation.logLocationLen;
        handlePrintAsLog :=deferredAdapterCreation.handlePrintAsLog;
      end;
    end else new(P_defaultConsoleFormatter(result),create);
  end;

FUNCTION T_commandLineParameters.applyAndReturnOk(CONST adapters: P_messagesDistributor; CONST initAdaptersForGui: boolean): boolean;
  VAR i:longint;
      scriptFileName:string;
      s:ansistring;
      consoleMessageTypes:T_messageTypeSet;
      tempFormatter:P_messageFormatProvider;

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
      mnhExecutionOptions.deferredAdapterCreations[i].finalizeBeforeApplication(scriptFileName);
      tempFormatter:=getFormatterFor(mnhExecutionOptions.deferredAdapterCreations[i]);
      adapters^.addOutfile(mnhExecutionOptions.deferredAdapterCreations[i],tempFormatter);
      dispose(tempFormatter,destroy);
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

