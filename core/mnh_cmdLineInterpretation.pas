UNIT mnh_cmdLineInterpretation;
INTERFACE
USES mnh_constants,mnh_out_adapters,mnh_funcs,consoleAsk{$ifdef fullVersion},mnh_doc,mnh_funcs_server{$endif},mnh_packages,
     myStringUtil,sysutils,myGenerics,mnh_contexts,
     lclintf,mnh_html;
PROCEDURE parseCmdLine;
PROCEDURE makeAndShowDoc(CONST includePackageDoc:boolean);
FUNCTION getFileOrCommandToInterpretFromCommandLine:ansistring;
PROCEDURE setupOutputBehaviour(VAR adapters:T_adapters);

VAR consoleAdapters:T_adapters;
    reEvaluationWithGUIrequired:boolean=false;
    mainParameters:T_arrayOfString;
    wantConsoleAdapter:boolean=true;
    {$ifdef fullVersion}
    filesToOpenInEditor:T_arrayOfString;
    {$endif}
IMPLEMENTATION
//by command line parameters:---------------
VAR fileOrCommandToInterpret:ansistring='';
    mnhParameters:T_arrayOfString;
    wantHelpDisplay:boolean=false;
    directExecutionMode:boolean=false;

    echo:(e_forcedOn,e_default,e_forcedOff)=e_default;
    time:(t_forcedOn,t_default,t_forcedOff)=t_default;
    minEL:longint={$ifdef IMIG}0{$else}2{$endif};
//---------------:by command line parameters
PROCEDURE setupOutputBehaviour(VAR adapters:T_adapters);
  begin
    adapters.doEchoInput        :=(echo=e_forcedOn) or (echo=e_default) and ((fileOrCommandToInterpret='') or directExecutionMode);
    adapters.doEchoDeclaration  :=(echo=e_forcedOn) or (echo=e_default) and ((fileOrCommandToInterpret='') or directExecutionMode);
    adapters.doShowExpressionOut:=(echo=e_forcedOn) or (echo=e_default) and ((fileOrCommandToInterpret='') or directExecutionMode);
    adapters.doShowTimingInfo   :=(time=t_forcedOn) or (time=t_default) and ((fileOrCommandToInterpret='') or directExecutionMode);
    adapters.minErrorLevel:=minEL;
  end;

FUNCTION getFileOrCommandToInterpretFromCommandLine:ansistring;
  begin
    result:=fileOrCommandToInterpret;
  end;

PROCEDURE makeAndShowDoc(CONST includePackageDoc:boolean);
  begin
    {$ifdef fullVersion}
    prepareDocumentation(includePackageDoc);
    if includePackageDoc then OpenURL('file:///'+replaceAll(expandFileName(getHtmlRoot+'/packages.html'),'\','/')+'#defined')
                         else OpenURL('file:///'+replaceAll(expandFileName(getHtmlRoot+'/index.html'   ),'\','/'));
    {$else}
    writeln('Generation of the documentation is only implemented in the full (i.e. non-light) version.');
    {$endif}
  end;

PROCEDURE parseCmdLine;
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
      consoleAdapters.printOut('MNH5 '+{$ifdef fullVersion}'(full'{$else}'(light'{$endif}+
                      {$ifdef debugMode}',debug)'{$else}')'{$endif}+' by Martin Schlegel');
      consoleAdapters.printOut('compiled on: '+{$I %DATE%});
      consoleAdapters.printOut('         at: '+{$I %TIME%});
      consoleAdapters.printOut('FPC version: '+{$I %FPCVERSION%});
      consoleAdapters.printOut('Target CPU : '+{$I %FPCTARGET%});
      consoleAdapters.printOut('');
      consoleAdapters.printOut('Accepted parameters: ');
      consoleAdapters.printOut('  [-h/-version] [+echo/-echo] [-el#] [(-cmd commandToExecute) | (filename [parameters])]');
      consoleAdapters.printOut('  filename          if present the file is interpreted; parameters are passed if present');
      consoleAdapters.printOut('                    if not present, interactive mode is entered');
      consoleAdapters.printOut('  +echo             force echo on (default for interactive mode)');
      consoleAdapters.printOut('  -echo             force echo off (default for interpretation mode)');
      consoleAdapters.printOut('  +time             force time on (default for interactive mode)');
      consoleAdapters.printOut('  -time             force time off (default for interpretation mode)');
      consoleAdapters.printOut('  -el#              set minimum error level for output; valid values: [0..5], default=2');
      consoleAdapters.printOut('  -h                display this help and quit');
      consoleAdapters.printOut('  -version          show version info and exit');
      consoleAdapters.printOut('  -codeHash         show codeHash and exit');
      consoleAdapters.printOut('  -cmd              directly execute the following command');
      {$ifdef fullVersion}
      consoleAdapters.printOut('  -doc              regenerate and show documentation');
      consoleAdapters.printOut('  -edit <filename>  opens file(s) in editor instead of interpreting it directly');
      {$endif}
      consoleAdapters.printOut('  -out <filename>   write output to the given file; if the extension is .html, ');
      consoleAdapters.printOut('                    an html document will be generated, otherwise simple text.');
      consoleAdapters.printOut('  +out <filename>   As -out but appending to the file if existing.');
      consoleAdapters.printOut('  -quiet            disable console output');
    end;

  PROCEDURE doDirect;
    VAR context:T_evaluationContext;
        package:P_package;
    begin
      context.createNormalContext(P_adapters(@consoleAdapters));
      package:=packageFromCode(fileOrCommandToInterpret,'<cmd_line>');
      package^.load(lu_forDirectExecution,context,C_EMPTY_STRING_ARRAY);
      dispose(package,destroy);
      context.destroy;
      consoleAdapters.setExitCode;
      halt(ExitCode);
    end;

  PROCEDURE fileMode;
    VAR context:T_evaluationContext;
        package:T_package;
    begin
      package.create(nil);
      package.setSourcePath(fileOrCommandToInterpret);

      if wantHelpDisplay then begin
        package.loadForDocumentation;
        package.printHelpOnMain(consoleAdapters);
        package.destroy;
        halt;
      end;
      context.createNormalContext(P_adapters(@consoleAdapters));
      package.load(lu_forCallingMain,context,mainParameters);
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
      halt(ExitCode);
    end;

  PROCEDURE addParameter(VAR list:T_arrayOfString; CONST index:longint);
    begin
      setLength(list,length(list)+1);
      list[length(list)-1]:=paramStr(index);
    end;

  VAR i:longint;
      quitImmediate:boolean=false;
  begin
    setLength(mainParameters,0);
    setLength(mnhParameters,0);
    i:=1;
    while i<=paramCount do begin
      if (fileOrCommandToInterpret='') or directExecutionMode then begin
        if      paramStr(i)='+echo' then begin echo:=e_forcedOn;           addParameter(mnhParameters,i); end
        else if paramStr(i)='-echo' then begin echo:=e_forcedOff;          addParameter(mnhParameters,i); end
        else if paramStr(i)='+time' then begin time:=t_forcedOn;           addParameter(mnhParameters,i); end
        else if paramStr(i)='-time' then begin time:=t_forcedOff;          addParameter(mnhParameters,i); end
        else if paramStr(i)='-cmd'  then begin directExecutionMode:=true;  addParameter(mnhParameters,i); end
        {$ifdef fullVersion}
        else if (paramStr(i)='-edit') then while i<paramCount do begin
          inc(i);
          append(filesToOpenInEditor,paramStr(i));
        end
        {$endif}
        else if (paramStr(i)='-out') and (i<paramCount) then begin
          addParameter(mnhParameters,i);
          inc(i);
          addOutfile(consoleAdapters, paramStr(i),false);
          addParameter(mnhParameters,i);
        end
        else if (paramStr(i)='+out') and (i<paramCount) then begin
          addParameter(mnhParameters,i);
          inc(i);
          addOutfile(consoleAdapters, paramStr(i));
          addParameter(mnhParameters,i);
        end
        else if startsWith(paramStr(i),'-quiet') then wantConsoleAdapter:=false
        else if startsWith(paramStr(i),'-h') then wantHelpDisplay:=true
        else if startsWith(paramStr(i),'-version') then begin displayVersionInfo; quitImmediate:=true; end
        else if startsWith(paramStr(i),'-codeHash') then begin write({$ifdef fullVersion}'F'{$else}'L'{$endif},
                                                                     {$ifdef IMIG}'I'{$else}''{$endif},
                                                                     {$ifdef debugMode}'D'{$else}'O'{$endif},
                                                                     {$I %FPCTargetOS%}); {$include code_hash.inc} quitImmediate:=true; end
        {$ifdef fullVersion}
        else if startsWith(paramStr(i),'-doc') then begin makeAndShowDoc(false); quitImmediate:=true; end
        {$endif}
        else if startsWith(paramStr(i),'-el') then begin
          minEL:=strToIntDef(copy(paramStr(i),4,length(paramStr(i))-3),-1);
          addParameter(mnhParameters,i);
          if (minEL<0) or (minEL>5) then begin
            writeln('Invalid minimum error level given!');
            writeln('Parameter: ',paramStr(i),'; extracted level: ',copy(paramStr(i),4,length(paramStr(i))-3));
            writeln('Allowed values: 0, 1, 2, 3, 4, 5');
            halt;
          end;
        end else if directExecutionMode then begin
          fileOrCommandToInterpret:=fileOrCommandToInterpret+' '+paramStr(i);
        end else begin
          if fileExists(paramStr(i)) then fileOrCommandToInterpret:=paramStr(i) else begin
            writeln('Invalid filename given!');
            writeln('Parameter: ',paramStr(i));
            writeln('File does not exist.');
            halt;
          end;
        end;
      end else addParameter(mainParameters,i);
      inc(i);
    end;
    setMnhParameters(mnhParameters);
    //-----------------------------------------------------
    if wantConsoleAdapter then consoleAdapters.addConsoleOutAdapter;
    setupOutputBehaviour(consoleAdapters);

    if fileOrCommandToInterpret<>'' then begin
       if directExecutionMode then doDirect
                              else fileMode;
    end;
    if wantHelpDisplay then begin
      displayHelp;
      quitImmediate:=true;
    end;
    {$ifdef fullVersion}quitImmediate:=quitImmediate and (length(filesToOpenInEditor)=0);{$endif}
    if quitImmediate then halt;
  end;

INITIALIZATION
  {$ifdef fullVersion}filesToOpenInEditor:=C_EMPTY_STRING_ARRAY;{$endif}
  consoleAdapters.create;

FINALIZATION
  consoleAdapters.destroy;

end.
