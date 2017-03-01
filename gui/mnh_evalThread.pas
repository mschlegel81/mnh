UNIT mnh_evalThread;
INTERFACE
USES FileUtil,sysutils,Classes,LazUTF8,
     myGenerics,myStringUtil,
     mnh_constants,mnh_basicTypes,mnh_fileWrappers,
     mnh_out_adapters,
     mnh_litVar,
     mnh_tokens,valueStore,
     mnh_funcs,mnh_contexts,
     mnh_subrules,
     mnh_packages,mnh_doc,
     mnh_cmdLineInterpretation;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_callMain,er_reEvaluateWithGUI,er_ensureEditScripts,er_runEditScript,er_die);
  T_evaluationState=(es_dead,es_idle,es_running,es_debugRunning,es_debugHalted,es_editEnsuring,es_editRunning);
  T_scriptType=(st_edit,st_insert,st_util);
CONST
  C_runningStates:set of T_evaluationState=[es_running,es_debugRunning,es_debugHalted,es_editEnsuring,es_editRunning];
  C_scriptTypeMeta:array[T_scriptType] of record nameAttribute:string; validResultType:T_literalTypeSet; end=
    {st_edit} ((nameAttribute:'editScript'  ; validResultType:[lt_emptyList,lt_stringList]),
    {st_insert}(nameAttribute:'insertScript'; validResultType:[lt_string]),
    {st_util}  (nameAttribute:'utility';      validResultType:[lt_emptyList,lt_stringList,lt_void]));
TYPE
  T_runnerStateInfo=record
    request:T_evalRequest;
    state  :T_evaluationState;
    message:string;
    hasPendingEditResult:boolean;
  end;

  T_tokenInfo=record
    tokenText, tokenExplanation:ansistring;
    location,
    startLoc,endLoc:T_searchTokenLocation;
  end;

  P_scriptMeta=^T_scriptMeta;
  T_scriptMetaArray=array of P_scriptMeta;
  T_scriptMeta=object
    private
      name:string;
      editRule:P_subrule;
      outputLanguage:string;
      createNewEditor:boolean;
      scriptType:T_scriptType;
      CONSTRUCTOR create(CONST rule:P_subrule; OUT isValid:boolean; VAR adapters:T_adapters);
      DESTRUCTOR destroy;
    public
      PROPERTY getName:string read name;
      PROPERTY getScriptType:T_scriptType read scriptType;
  end;

  P_editScriptTask=^T_editScriptTask;
  T_editScriptTask=object
    private
      script:P_scriptMeta;
      inputEditIndex:longint;
      input,
      output:P_literal;
      outputLanguage:string;
      done  :boolean;
      CONSTRUCTOR create(CONST script_:P_scriptMeta; CONST inputIndex:longint; CONST inputEditFile:string; CONST input_:TStrings; CONST inputLang:string);
      DESTRUCTOR destroy;
      PROCEDURE execute(VAR context:T_threadContext);
    public
      FUNCTION getOutput:P_literal;
      FUNCTION getOutputLanguage:string;
      FUNCTION wantNewEditor:boolean;
      FUNCTION inputIdx:longint;
      FUNCTION wantOutput:boolean;
      FUNCTION wantInsert:boolean;
  end;

  P_evaluator=^T_evaluator;
  T_evaluator=object
    private
      //"final" properties
      cs:TRTLCriticalSection;
      thread:TThreadFunc;
      adapter:P_adapters;
      //request variables
      request:T_evalRequest;

      //internal state variables
      state:T_evaluationState;
      package:T_package;

      PROCEDURE ensureThread;
      PROCEDURE threadStopped;
      PROCEDURE preEval; virtual;
      PROCEDURE postEval; virtual;
      FUNCTION pendingRequest:T_evalRequest;
    public
      context:T_evaluationContext;
      CONSTRUCTOR create(CONST adapters:P_adapters; threadFunc:TThreadFunc);
      DESTRUCTOR destroy; virtual;
      PROCEDURE haltEvaluation; virtual;

      FUNCTION evaluationRunning: boolean;
      FUNCTION evaluationRunningOrPending: boolean;
      FUNCTION getCodeProvider:P_codeProvider;
      PROCEDURE reportVariables(VAR report:T_variableReport);
  end;

  P_runEvaluator=^T_runEvaluator;
  T_runEvaluator=object(T_evaluator)
    private
      //request variables
      profilingRequested:boolean;
      debuggingRequested:boolean;
      mainParameters:T_arrayOfString;
      //internal state variables
      startOfEvaluation:double;
      endOfEvaluationText:ansistring;
      utilityScriptPackage:P_package;
      utilityScriptList:T_scriptMetaArray;
      currentEdit:P_editScriptTask;
      editAdapters:P_adapters;

      FUNCTION parametersForMainCall:T_arrayOfString;
      PROCEDURE preEval; virtual;
      PROCEDURE postEval; virtual;
    public
      CONSTRUCTOR create(CONST adapters:P_adapters; threadFunc:TThreadFunc);
      DESTRUCTOR destroy; virtual;
      PROCEDURE reEvaluateWithGUI;
      PROCEDURE evaluate         (CONST provider:P_codeProvider; CONST profiling,debugging:boolean);
      PROCEDURE callMain         (CONST provider:P_codeProvider; params: ansistring; CONST profiling,debugging:boolean);
      PROCEDURE ensureEditScripts();
      PROCEDURE runUtilScript    (CONST scriptIndex,editorIndex:longint; CONST L:TStrings; CONST inputLang:string; CONST editorFileName:string);
      FUNCTION getRunnerStateInfo:T_runnerStateInfo;

      FUNCTION getCurrentEdit:P_editScriptTask;
      PROCEDURE freeCurrentEdit;
      PROCEDURE haltEvaluation; virtual;
      FUNCTION getScripts:T_scriptMetaArray;
  end;

  P_assistanceEvaluator=^T_assistanceEvaluator;
  T_assistanceEvaluator=object(T_evaluator)
    private
      localErrors,externalErrors:T_storedMessages;
      stateCounter:longint;
      userRules,
      completionList:T_setOfString;
      PROCEDURE preEval; virtual;
      PROCEDURE postEval; virtual;
    public
      CONSTRUCTOR create(CONST adapters:P_adapters; threadFunc:TThreadFunc);
      DESTRUCTOR destroy; virtual;

      PROCEDURE evaluate(CONST provider:P_codeProvider);

      FUNCTION isErrorLocation(CONST lineIndex,tokenStart,tokenEnd:longint):byte;
      FUNCTION getErrorHints:T_arrayOfString;
      FUNCTION getStateCounter:longint;
      FUNCTION isUserRule(CONST id:string):boolean;
      FUNCTION resolveImport(CONST id:string):string;
      PROCEDURE extendCompletionList(VAR list:T_setOfString);
      PROCEDURE explainIdentifier(CONST fullLine:ansistring; CONST CaretY,CaretX:longint; VAR info:T_tokenInfo);
  end;

VAR runEvaluator:T_runEvaluator;
    assistancEvaluator:T_assistanceEvaluator;

PROCEDURE initUnit(CONST guiAdapters:P_adapters; CONST useAssistance:boolean);
OPERATOR =(CONST x,y:T_runnerStateInfo):boolean;
FUNCTION utilityScriptFileName:string;

PROCEDURE earlyFinalization;
IMPLEMENTATION
VAR unitIsInitialized:boolean=false;
    assistanceIsInitialized:boolean=false;
    silentAdapters:T_adapters;
    intrinsicRulesForCompletion:T_setOfString;

OPERATOR =(CONST x,y:T_runnerStateInfo):boolean;
  begin
    result:=(x.state=y.state) and (x.request=y.request) and (x.message=y.message);
  end;

FUNCTION utilityScriptFileName:string;
  begin
    result:=configDir+'packages'+DirectorySeparator+'guiScripts.mnh';
    if not(fileExists(result)) then ensureDemos;
  end;
{$WARN 5024 OFF}
FUNCTION main(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=250;
  VAR sleepTime:longint=0;
      r:T_evalRequest;

  PROCEDURE setupEdit(OUT context:T_evaluationContext);
    begin
      P_runEvaluator(p)^.editAdapters^.clearAll;
      context.create(P_runEvaluator(p)^.editAdapters);
      context.resetForEvaluation(nil,false,false,true);
    end;

  PROCEDURE doneEdit(VAR context:T_evaluationContext);
    VAR collector:P_collectingOutAdapter;
    begin
      context.afterEvaluation;
      if (context.adapters^.hasPrintOut) or
         (context.adapters^.hasNonSilentError) then begin
        collector:=P_collectingOutAdapter(context.adapters^.getAdapter(0));
        {$ifdef debugMode}
        writeln('Raising ',length(collector^.storedMessages),' stored messages');
        {$endif}
        P_runEvaluator(p)^.adapter^.clearPrint;
        P_runEvaluator(p)^.adapter^.raiseStoredMessages(collector^.storedMessages);
      end;
      context.destroy;
    end;

  PROCEDURE ensureEditScripts_impl();
    VAR subRule:P_subrule;
        script:P_scriptMeta;
        editContext:T_evaluationContext;
        scriptType:T_scriptType;
        isValid:boolean;
    begin with P_runEvaluator(p)^ do begin
      setupEdit(editContext);
      if utilityScriptPackage=nil then begin
        {$ifdef debugMode} writeln('Creating script package'); {$endif}
        new(utilityScriptPackage,create(newFileCodeProvider(utilityScriptFileName),nil));
      end else if not(utilityScriptPackage^.codeChanged) then exit;
      for script in utilityScriptList do dispose(script,destroy);
      setLength(utilityScriptList,0);
      {$ifdef debugMode} writeln('Loading script package: ',utilityScriptPackage^.getPath); {$endif}
      utilityScriptPackage^.load(lu_forImport,editContext.threadContext^,C_EMPTY_STRING_ARRAY);
      if editContext.adapters^.noErrors then begin
        for scriptType in T_scriptType do
        for subRule in utilityScriptPackage^.getSubrulesByAttribute(C_scriptTypeMeta[scriptType].nameAttribute) do begin
          {$ifdef debugMode} writeln('Found script: ',subRule^.getId); {$endif}
          new(script,create(subRule,isValid,editAdapters^));
          if isValid then begin
            setLength(utilityScriptList,length(utilityScriptList)+1);
            utilityScriptList[length(utilityScriptList)-1]:=script;
          end else dispose(script,destroy);
        end;
      end;
      doneEdit(editContext);
    end; end;

  PROCEDURE executeEditScript_impl;
    VAR editContext:T_evaluationContext;
    begin
      setupEdit(editContext);
      P_runEvaluator(p)^.currentEdit^.execute(editContext.threadContext^);
      doneEdit(editContext);
    end;

  begin with P_runEvaluator(p)^ do begin
    {$ifdef debugMode} writeln(stdErr,'Thread ',ThreadID,' handles main evaluation loop');{$endif}
    result:=0;
    repeat
      r:=pendingRequest;
      if r in [er_evaluate,er_callMain,er_reEvaluateWithGUI,er_ensureEditScripts,er_runEditScript] then begin
        {$ifdef debugMode} writeln(stdErr,'Thread ',ThreadID,' handles main evaluation request ',r);{$endif}
        sleepTime:=0;
        preEval;
        case r of
          er_evaluate: package.load(lu_forDirectExecution,context.threadContext^,C_EMPTY_STRING_ARRAY);
          er_callMain: package.load(lu_forCallingMain    ,context.threadContext^,parametersForMainCall);
          er_reEvaluateWithGUI: begin
            package.replaceCodeProvider(newFileCodeProvider(getFileOrCommandToInterpretFromCommandLine));
            package.load(lu_forCallingMain,context.threadContext^,parametersForMainCall);
          end;
          er_ensureEditScripts: ensureEditScripts_impl;
          er_runEditScript: executeEditScript_impl;
        end;
        postEval;
        {$ifdef debugMode} writeln(stdErr,'Thread ',ThreadID,' finished main evaluation request ',r);{$endif}
      end else begin
        if sleepTime<MAX_SLEEP_TIME then inc(sleepTime);
        sleep(sleepTime);
      end;
    until (pendingRequest=er_die);
    threadStopped;
    {$ifdef debugMode} writeln(stdErr,'Thread ',ThreadID,' stopped main evaluation loop');{$endif}
  end; end;

FUNCTION docMain(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=100;
  begin with P_assistanceEvaluator(p)^ do begin
    {$ifdef debugMode} writeln(stdErr,'Thread ',ThreadID,' handles assistance evaluation loop');{$endif}
    result:=0;
    repeat
      if (pendingRequest in [er_evaluate,er_callMain,er_reEvaluateWithGUI])  then begin
        {$ifdef debugMode} writeln(stdErr,'Thread ',ThreadID,' handles assistance evaluation request');{$endif}
        preEval;
        package.load(lu_forCodeAssistance,context.threadContext^,C_EMPTY_STRING_ARRAY);
        postEval;
        {$ifdef debugMode} writeln(stdErr,'Thread ',ThreadID,' finished assistance evaluation request');{$endif}
      end;
      ThreadSwitch;
      sleep(MAX_SLEEP_TIME);
      ThreadSwitch;
    until (pendingRequest=er_die);
    threadStopped;
    {$ifdef debugMode} writeln(stdErr,'Thread ',ThreadID,' stopped assistance evaluation loop');{$endif}
  end; end;

CONSTRUCTOR T_scriptMeta.create(CONST rule: P_subrule; OUT isValid:boolean; VAR adapters:T_adapters);
  VAR t:T_scriptType;
  begin
    editRule:=rule;
    outputLanguage:=editRule^.getAttribute('language').value;
    createNewEditor:=editRule^.hasAttribute('newEdit');
    isValid:=false;
    for t in T_scriptType do if rule^.hasAttribute(C_scriptTypeMeta[t].nameAttribute) then begin
      scriptType:=t;
      name:=editRule^.getAttribute(C_scriptTypeMeta[t].nameAttribute).value;
      isValid:=true;
    end;
    if name='' then name:=editRule^.getId;
    if (scriptType=st_insert) and createNewEditor then begin
      isValid:=false;
      adapters.raiseError('Invalid attribute @newEdit for insert script',rule^.getLocation);
    end;
    if (scriptType=st_insert) and (outputLanguage<>'') then begin
      isValid:=false;
      adapters.raiseError('Invalid attribute @language for insert script',rule^.getLocation);
    end;
    if scriptType in [st_util,st_insert] then begin
      if not(rule^.acceptsSingleLiteral(lt_string)) then begin
        isValid:=false;
        adapters.raiseError('Scripts @'+C_scriptTypeMeta[scriptType].nameAttribute+' must accept a single string, the editor name',rule^.getLocation);
      end;
    end else begin
      if not(rule^.acceptsSingleLiteral(lt_stringList)) then begin
        isValid:=false;
        adapters.raiseError('Scripts @'+C_scriptTypeMeta[scriptType].nameAttribute+' must accept a single stringList, the editor contents',rule^.getLocation);
      end;
    end;
  end;

DESTRUCTOR T_scriptMeta.destroy;
  begin
  end;

CONSTRUCTOR T_editScriptTask.create(CONST script_:P_scriptMeta; CONST inputIndex:longint; CONST inputEditFile:string; CONST input_:TStrings; CONST inputLang:string);
  VAR i:longint;
  begin
    script:=script_;
    inputEditIndex:=inputIndex;
    if script^.scriptType=st_edit then begin
      input:=newListLiteral(input_.count);
      for i:=0 to input_.count-1 do P_listLiteral(input)^.appendString(input_[i]);
    end else input:=newStringLiteral(inputEditFile);
    output:=nil;
    outputLanguage:=script^.outputLanguage;
    if outputLanguage='' then begin
      if script^.scriptType=st_edit
      then outputLanguage:=inputLang
      else outputLanguage:='txt';
    end;
    done:=false;
  end;

DESTRUCTOR T_editScriptTask.destroy;
  begin
    if output<>nil then disposeLiteral(output);
  end;

PROCEDURE T_editScriptTask.execute(VAR context:T_threadContext);
  begin
    output:=script^.editRule^.directEvaluateUnary(input,script^.editRule^.getLocation,context,0);
    disposeLiteral(input);
    if (output<>nil) and not(output^.literalType in C_scriptTypeMeta[script^.scriptType].validResultType) then begin
      context.adapters^.raiseError('Script failed due to invalid result type '+C_typeString[output^.literalType],script^.editRule^.getLocation);
      disposeLiteral(output);
    end;
    done:=true;
  end;

FUNCTION T_editScriptTask.getOutput:P_literal; begin result:=output; end;
FUNCTION T_editScriptTask.getOutputLanguage:string; begin result:=outputLanguage; end;
FUNCTION T_editScriptTask.wantNewEditor:boolean; begin result:=(script^.createNewEditor) and (output<>nil) and (output^.literalType=lt_stringList); end;
FUNCTION T_editScriptTask.inputIdx:longint; begin result:=inputEditIndex; end;
FUNCTION T_editScriptTask.wantOutput:boolean; begin result:=((output<>nil) and (output^.literalType=lt_stringList)) and ((script^.scriptType=st_edit) or script^.createNewEditor); end;
FUNCTION T_editScriptTask.wantInsert:boolean; begin result:=((output<>nil) and (output^.literalType=lt_string)) and (script^.scriptType=st_insert); end;

PROCEDURE T_evaluator.ensureThread;
  begin
    system.enterCriticalSection(cs);
    if state=es_dead then begin
      beginThread(thread,@self);
      state:=es_idle;
    end;
    system.leaveCriticalSection(cs);
  end;

CONSTRUCTOR T_evaluator.create(CONST adapters: P_adapters; threadFunc: TThreadFunc);
  begin
    system.initCriticalSection(cs);
    request:=er_none;
    state:=es_dead;
    thread:=threadFunc;
    package.create(newVirtualFileCodeProvider('?',C_EMPTY_STRING_ARRAY),nil);
    adapter:=adapters;
    context.create(adapter);
  end;

CONSTRUCTOR T_runEvaluator.create(CONST adapters:P_adapters; threadFunc:TThreadFunc);
  VAR collector:P_collectingOutAdapter;
  begin
    inherited create(adapters,threadFunc);
    utilityScriptPackage:=nil;
    setLength(utilityScriptList,0);
    currentEdit:=nil;
    new(editAdapters,create);
    new(collector,create(at_sandboxAdapter,C_defaultOutputBehavior_fileMode));
    editAdapters^.addOutAdapter(collector,true);
    {$ifdef debugMode}editAdapters^.addConsoleOutAdapter('v')^.enableMessageType(false,[mt_clearConsole]);{$endif}
    endOfEvaluationText:='compiled on: '+{$I %DATE%}+' at: '+{$I %TIME%}+' with FPC'+{$I %FPCVERSION%}+' for '+{$I %FPCTARGET%};
  end;

CONSTRUCTOR T_assistanceEvaluator.create(CONST adapters: P_adapters; threadFunc: TThreadFunc);
  begin
    inherited create(adapters,threadFunc);
    stateCounter:=0;
    setLength(localErrors,0);
    setLength(externalErrors,0);
    userRules.create;
    completionList.create;
  end;

DESTRUCTOR T_evaluator.destroy;
  begin
    system.enterCriticalSection(cs);
    if state in C_runningStates then adapter^.haltEvaluation;
    repeat
      request:=er_die;
      system.leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      system.enterCriticalSection(cs);
    until state=es_dead;
    package.destroy;
    context.destroy;
    system.leaveCriticalSection(cs);
    system.doneCriticalSection(cs);
  end;

DESTRUCTOR T_runEvaluator.destroy;
  VAR i:longint;
  begin
    if currentEdit<>nil then dispose(currentEdit,destroy);
    if utilityScriptPackage<>nil then dispose(utilityScriptPackage,destroy);
    for i:=0 to length(utilityScriptList)-1 do dispose(utilityScriptList[i],destroy);
    dispose(editAdapters,destroy);
    inherited destroy;
    setLength(mainParameters,0);
  end;

DESTRUCTOR T_assistanceEvaluator.destroy;
  begin
    inherited destroy;
    userRules.destroy;
    completionList.destroy;
  end;

PROCEDURE T_evaluator.haltEvaluation;
  begin
    system.enterCriticalSection(cs);
    killServersCallback;
    context.adapters^.haltEvaluation;
    while not(adapter^.hasHaltMessage) do begin
      system.leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      system.enterCriticalSection(cs);
    end;
    request:=er_none;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.haltEvaluation;
  begin
    system.enterCriticalSection(cs);
    editAdapters^.haltEvaluation;
    inherited haltEvaluation;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.reEvaluateWithGUI;
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) or (request<>er_none) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    debuggingRequested:=false;
    profilingRequested:=mnh_cmdLineInterpretation.profilingRun;
    mainParameters:=mnh_cmdLineInterpretation.mainParameters;
    request:=er_reEvaluateWithGUI;
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.evaluate(CONST provider:P_codeProvider; CONST profiling,debugging:boolean);
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    profilingRequested:=profiling;
    debuggingRequested:=debugging;
    request:=er_evaluate;
    package.replaceCodeProvider(provider);
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_assistanceEvaluator.evaluate(CONST provider:P_codeProvider);
  begin
    system.enterCriticalSection(cs);
    ensureThread;
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    request:=er_evaluate;
    package.replaceCodeProvider(provider);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.callMain(CONST provider:P_codeProvider; params: ansistring; CONST profiling,debugging:boolean);
  VAR sp:longint;
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) or (request<>er_none) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    profilingRequested:=profiling;
    debuggingRequested:=debugging;
    setLength(mainParameters,0);
    params:=trim(params);
    while params<>'' do begin
      sp:=pos(' ',params);
      if sp<=0 then begin
        append(mainParameters,params);
        params:='';
      end else begin
        append(mainParameters,copy(params,1,sp-1));
        params:=trim(copy(params,sp+1,length(params)));
      end;
    end;
    package.replaceCodeProvider(provider);
    request:=er_callMain;
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.ensureEditScripts();
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) or (currentEdit<>nil) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    request:=er_ensureEditScripts;
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.runUtilScript(CONST scriptIndex,editorIndex:longint; CONST L:TStrings; CONST inputLang:string; CONST editorFileName:string);
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) or (currentEdit<>nil) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    request:=er_runEditScript;
    new(currentEdit,create(utilityScriptList[scriptIndex],editorIndex,editorFileName,L,inputLang));
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_runEvaluator.getCurrentEdit:P_editScriptTask;
  begin
    system.enterCriticalSection(cs);
    result:=currentEdit;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.freeCurrentEdit;
  begin
    system.enterCriticalSection(cs);
    if currentEdit<>nil then dispose(currentEdit,destroy);
    currentEdit:=nil;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_runEvaluator.getScripts:T_scriptMetaArray;
  begin
    result:=utilityScriptList;
  end;

FUNCTION T_evaluator.evaluationRunning: boolean;
  begin
    result:=state in C_runningStates;
  end;

FUNCTION T_evaluator.evaluationRunningOrPending: boolean;
  begin
    result:=(state in C_runningStates) or (request in [er_evaluate,er_callMain,er_reEvaluateWithGUI]);
  end;

FUNCTION T_evaluator.getCodeProvider: P_codeProvider;
  begin
    system.enterCriticalSection(cs);
    result:=package.getCodeProvider;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_assistanceEvaluator.explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo);
  PROCEDURE appendBuiltinRuleInfo(CONST prefix:string='');
    VAR doc:P_intrinsicFunctionDocumentation;
    begin
      ensureBuiltinDocExamples;
      if (length(info.tokenText)>1) and (info.tokenText[1]='.')
      then doc:=functionDocMap.get(copy(info.tokenText,2,length(info.tokenText)-1))
      else doc:= functionDocMap.get(info.tokenText);
      if doc=nil then exit;
      info.tokenExplanation:=info.tokenExplanation+prefix+'Builtin rule'+C_lineBreakChar+doc^.getPlainText(C_lineBreakChar)+';';
    end;

  VAR tokens:T_tokenArray;
      tokenToExplain:T_token;
      loc:T_tokenLocation;
      i:longint;
      comments,attributes:T_arrayOfString;
  begin
    if (CaretY=info.startLoc.line) and (CaretX>=info.startLoc.column) and (CaretX<info.endLoc.column) then exit;
    system.enterCriticalSection(cs);
    while (state in C_runningStates) do begin
      system.leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      system.enterCriticalSection(cs);
    end;

    tokens.create;
    loc.line:=1;
    loc.column:=1;
    loc.package:=@package;
    adapter^.clearAll;
    tokens.tokenizeAll(fullLine,loc,@package,adapter^,false);
    comments  :=C_EMPTY_STRING_ARRAY;
    attributes:=C_EMPTY_STRING_ARRAY;
    tokens.step(@package,comments,attributes,adapter^);

    while not(tokens.atEnd) and (tokens.current.location.column<CaretX) do tokens.step(@package,comments,attributes,adapter^);
    if tokens.atEnd then begin
      tokenToExplain:=tokens.lastToken;
      info.startLoc:=tokenToExplain.location;
      info.endLoc:=info.startLoc;
      info.endLoc.column:=maxLongint;
    end else begin
      tokenToExplain:=tokens.current;
      info.startLoc:=tokenToExplain.location;
      info.endLoc:=loc;
    end;
    info.location:=info.startLoc;
    info.tokenText:=safeTokenToString(@tokenToExplain);
    tokens.getRawTokensUndefining;
    tokens.destroy;

    if tokenToExplain.tokType=tt_identifier then tokenToExplain.resolveRuleId(@package,nil);

    info.tokenExplanation:=replaceAll(C_tokenInfo[tokenToExplain.tokType].helpText,'#',C_lineBreakChar);
    for i:=0 to length(C_specialWordInfo)-1 do
      if C_specialWordInfo[i].txt=info.tokenText then
      info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar+replaceAll(C_specialWordInfo[i].helpText,'#',C_lineBreakChar);

    case tokenToExplain.tokType of
      tt_intrinsicRule: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        appendBuiltinRuleInfo;
      end;
      tt_importedUserRule,tt_localUserRule,tt_customTypeRule, tt_customTypeCheck: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+replaceAll(P_rule(tokenToExplain.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(tokenToExplain.data)^.getLocationOfDeclaration;
        if intrinsicRuleMap.containsKey(tokenToExplain.txt) then appendBuiltinRuleInfo('hides ');
      end;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.reportVariables(VAR report: T_variableReport);
  begin
    system.enterCriticalSection(cs);
    package.reportVariables(report);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_runEvaluator.getRunnerStateInfo:T_runnerStateInfo;
  begin
    system.enterCriticalSection(cs);
    result.state:=state;
    if (context.isPaused) then result.state:=es_debugHalted;
    result.request:=request;
    result.message:=endOfEvaluationText;
    result.hasPendingEditResult:=(currentEdit<>nil) and (currentEdit^.done);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    result:=0;
    for i:=0 to length(localErrors)-1 do with localErrors[i] do
    if (result=0) and (lineIndex=location.line-1) and ((location.column<0) or (tokenStart<=location.column-1) and (tokenEnd>location.column-1)) then begin
      if C_messageTypeMeta[messageType].level>2 then result:=2 else result:=1;
    end;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.getErrorHints:T_arrayOfString;
  VAR k:longint;
  PROCEDURE addErrors(CONST list:T_storedMessages);
    VAR i:longint;
        s:string;
    begin
      for i:=0 to length(list)-1 do with list[i] do for s in messageText do begin
        if k>=length(result) then setLength(result,k+1);
        result[k]:=C_messageClassMeta[C_messageTypeMeta[messageType].mClass].guiMarker+C_messageTypeMeta[messageType].prefix+' '+ansistring(location)+' '+(s);
        inc(k);
      end;
    end;

  begin
    system.enterCriticalSection(cs);
    setLength(result,length(localErrors)+length(externalErrors));
    k:=0;
    addErrors(localErrors);
    addErrors(externalErrors);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_evaluator.pendingRequest: T_evalRequest;
  begin
    system.enterCriticalSection(cs);
    if state=es_idle
    then result:=request
    else result:=er_none;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.threadStopped;
  begin
    system.enterCriticalSection(cs);
    state:=es_dead;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.preEval;
  begin
    system.enterCriticalSection(cs);
    state:=es_running;
    case request of
      er_ensureEditScripts: state:=es_editEnsuring;
      er_runEditScript    : state:=es_editRunning;
    end;
    request:=er_none;
    if state in [es_editEnsuring,es_editRunning]
    then adapter^.clearErrors;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.preEval;
  begin
    system.enterCriticalSection(cs);
    inherited preEval;
    startOfEvaluation:=now;
    context.resetForEvaluation(@package,profilingRequested,debuggingRequested,false);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_assistanceEvaluator.preEval;
  begin
    system.enterCriticalSection(cs);
    inherited preEval;
    adapter^.clearAll;
    context.resetForEvaluation(@package,false,false,true);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.postEval;
  begin
    system.enterCriticalSection(cs);
    if not(state in [es_editEnsuring,es_editRunning]) then context.afterEvaluation;
    state:=es_idle;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.postEval;
  begin
    system.enterCriticalSection(cs);
    inherited postEval;
    if adapter^.hasHaltMessage(false)
    then endOfEvaluationText:='Aborted after '+myTimeToStr(now-startOfEvaluation)
    else endOfEvaluationText:='Done in '+myTimeToStr(now-startOfEvaluation);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_assistanceEvaluator.postEval;
  PROCEDURE updateCompletionList;
    VAR s:string;
    begin
      completionList.clear;
      completionList.put(intrinsicRulesForCompletion);
      package.updateLists(userRules);
      completionList.put(userRules);
      for s in userRules.values do if pos(ID_QUALIFY_CHARACTER,s)<=0 then completionList.put(ID_QUALIFY_CHARACTER+s);
    end;

  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    inherited postEval;
    updateCompletionList;

    setLength(localErrors,0);
    setLength(externalErrors,0);
    with P_collectingOutAdapter(adapter^.getAdapter(0))^ do
    for i:=0 to length(storedMessages)-1 do with storedMessages[i] do
    if C_messageTypeMeta[messageType].level>=2 then begin
      if location.fileName=package.getPath
      then begin
        setLength(localErrors,length(localErrors)+1);
        localErrors[length(localErrors)-1]:=storedMessages[i];
      end else begin
        setLength(externalErrors,length(externalErrors)+1);
        externalErrors[length(externalErrors)-1]:=storedMessages[i];
      end;
    end;

    inc(stateCounter);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_runEvaluator.parametersForMainCall: T_arrayOfString;
  begin
    system.enterCriticalSection(cs);
    result:=mainParameters;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.getStateCounter: longint;
  begin
    system.enterCriticalSection(cs);
    result:=stateCounter;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.isUserRule(CONST id:string):boolean;
  begin
    system.enterCriticalSection(cs);
    result:=userRules.contains(id);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.resolveImport(CONST id: string): string;
  begin
    result:=package.getSecondaryPackageById(id);
  end;

PROCEDURE T_assistanceEvaluator.extendCompletionList(VAR list: T_setOfString);
  begin
    system.enterCriticalSection(cs);
    list.put(completionList.values);
    leaveCriticalSection(cs);
  end;

PROCEDURE initUnit(CONST guiAdapters:P_adapters; CONST useAssistance:boolean);
  PROCEDURE initIntrinsicRuleList;
    VAR ids:T_arrayOfString;
        i:longint;
        tt:T_tokenType;
    begin
      ids:=mnh_funcs.intrinsicRuleMap.keySet;
      intrinsicRulesForCompletion.create;
      for i:=0 to length(ids)-1 do begin
        if pos(ID_QUALIFY_CHARACTER,ids[i])<=0 then begin
          intrinsicRulesForCompletion.put(ids[i]);
          intrinsicRulesForCompletion.put(ID_QUALIFY_CHARACTER+ids[i]);
        end else begin
          intrinsicRulesForCompletion.put(ids[i]);
          intrinsicRulesForCompletion.put(split(ids[i],ID_QUALIFY_CHARACTER)[0]);
          intrinsicRulesForCompletion.put(split(ids[i],ID_QUALIFY_CHARACTER)[1]);
          intrinsicRulesForCompletion.put(ID_QUALIFY_CHARACTER+split(ids[i],ID_QUALIFY_CHARACTER)[0]);
          intrinsicRulesForCompletion.put(ID_QUALIFY_CHARACTER+split(ids[i],ID_QUALIFY_CHARACTER)[1]);
        end;
      end;
      for tt:=low(T_tokenType) to high(T_tokenType) do if isIdentifier(C_tokenInfo[tt].defaultId,true) then
        intrinsicRulesForCompletion.put(replaceAll(C_tokenInfo[tt].defaultId,'.',''));
      for i:=low(C_specialWordInfo) to high(C_specialWordInfo) do
        intrinsicRulesForCompletion.put(C_specialWordInfo[i].txt);
    end;

  VAR collector:P_collectingOutAdapter;
  begin
    runEvaluator.create(guiAdapters,@main);
    if useAssistance then begin
      silentAdapters.create;
      new(collector,create(at_unknown,C_collectAllOutputBehavior));
      silentAdapters.addOutAdapter(collector,true);
      assistancEvaluator.create(@silentAdapters,@docMain);
      assistanceIsInitialized:=true;
    end;
    initIntrinsicRuleList;
    unitIsInitialized:=true;
  end;

PROCEDURE earlyFinalization;
  begin
    if unitIsInitialized then begin
      runEvaluator.destroy;
      if assistanceIsInitialized then begin
        assistancEvaluator.destroy;
        silentAdapters.destroy;
      end;
    end;
    unitIsInitialized:=false;
  end;

FINALIZATION
  earlyFinalization;
end.
