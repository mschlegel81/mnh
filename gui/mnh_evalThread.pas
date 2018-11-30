UNIT mnh_evalThread;
INTERFACE
USES sysutils,Classes,
     mySys,
     myGenerics,myStringUtil,
     mnh_constants,basicTypes,mnh_fileWrappers,
     mnh_messages,
     mnh_out_adapters,
     mnh_litVar,
     funcs,contexts,
     mnh_tokenArray,
     subrules,
     packages,mnh_doc,
     cmdLineInterpretation,
     recyclers,
     mnh_debuggingVar;
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

  P_scriptMeta=^T_scriptMeta;
  T_scriptMetaArray=array of P_scriptMeta;
  T_scriptMeta=object
    private
      name:string;
      editRule:P_subruleExpression;
      outputLanguage:string;
      createNewEditor:boolean;
      scriptType:T_scriptType;
      CONSTRUCTOR create(CONST rule:P_subruleExpression; OUT isValid:boolean; CONST messages:P_messages);
      DESTRUCTOR destroy;
    public
      PROPERTY getName:string read name;
  end;

  P_editScriptTask=^T_editScriptTask;
  T_editScriptTask=object(T_payloadMessage)
    private
      script:P_scriptMeta;
      inputEditIndex:longint;
      input,
      output:P_literal;
      outputLanguage:string;
      done  :boolean;
      succeeded:boolean;
      CONSTRUCTOR create(CONST script_:P_scriptMeta; CONST inputIndex:longint; CONST inputEditFile:string; CONST input_:TStrings; CONST inputLang:string);
      PROCEDURE execute(VAR globals:T_evaluationGlobals; VAR recycler:T_recycler);
    public
      DESTRUCTOR destroy; virtual;
      FUNCTION getOutput:P_literal;
      FUNCTION getOutputLanguage:string;
      FUNCTION wantNewEditor:boolean;
      FUNCTION inputIdx:longint;
      FUNCTION wantOutput:boolean;
      FUNCTION wantInsert:boolean;
      PROPERTY successful:boolean read succeeded;
      FUNCTION withSuccessFlag(CONST trueWhenOk:boolean):P_editScriptTask;
  end;

  P_evaluator=^T_evaluator;
  T_evaluator=object
    private
      //"final" properties
      cs:TRTLCriticalSection;
      thread:TThreadFunc;
      messages:P_messages;
      //request variables
      request:T_evalRequest;

      //internal state variables
      state:T_evaluationState;
      package:T_package;

      haltedByUser:boolean;

      PROCEDURE ensureThread;
      PROCEDURE threadStopped;
      PROCEDURE preEval(VAR recycler:T_recycler); virtual;
      PROCEDURE postEval(VAR recycler:T_recycler); virtual;
      FUNCTION pendingRequest:T_evalRequest;
    public
      globals:T_evaluationGlobals;
      CONSTRUCTOR create(CONST messages_:P_messages; threadFunc:TThreadFunc);
      DESTRUCTOR destroy; virtual;
      PROCEDURE haltEvaluation; virtual;

      FUNCTION evaluationRunning: boolean;
      FUNCTION evaluationRunningOrPending: boolean;
      FUNCTION getCodeProvider:P_codeProvider;
      FUNCTION reportVariables:P_variableTreeEntryCategoryNode;
  end;

  P_runEvaluator=^T_runEvaluator;
  T_runEvaluator=object(T_evaluator)
    private
      //request variables
      requestedContextType:T_evaluationContextType;
      mainParameters:T_arrayOfString;
      //internal state variables
      startOfEvaluation:double;
      endOfEvaluationText:ansistring;
      utilityScriptPackage:P_package;
      utilityScriptList:T_scriptMetaArray;
      currentEdit:P_editScriptTask;
      editAdapters:P_messagesErrorHolder;

      FUNCTION parametersForMainCall:T_arrayOfString;
      PROCEDURE preEval(VAR recycler:T_recycler); virtual;
      PROCEDURE postEval(VAR recycler:T_recycler); virtual;
    public
      CONSTRUCTOR create(CONST messages_:P_messages; threadFunc:TThreadFunc);
      DESTRUCTOR destroy; virtual;
      PROCEDURE reEvaluateWithGUI;
      PROCEDURE evaluate         (CONST provider:P_codeProvider; CONST contextType:T_evaluationContextType; CONST firstRun:boolean);
      FUNCTION getPackageForPostEvaluation(CONST provider:P_codeProvider; CONST firstRun:boolean):P_package;
      PROCEDURE callMain         (CONST provider:P_codeProvider; params: ansistring; CONST contextType:T_evaluationContextType; CONST firstRun:boolean);
      PROCEDURE ensureEditScripts();
      PROCEDURE runUtilScript    (CONST scriptIndex,editorIndex:longint; CONST L:TStrings; CONST inputLang:string; CONST editorFileName:string);
      FUNCTION getRunnerStateInfo:T_runnerStateInfo;

      FUNCTION getCurrentEdit:P_editScriptTask;
      PROCEDURE freeCurrentEdit;
      FUNCTION getScripts:T_scriptMetaArray;
  end;

VAR runEvaluator:T_runEvaluator;

PROCEDURE initUnit(CONST guiAdapters:P_messages);
OPERATOR =(CONST x,y:T_runnerStateInfo):boolean;
FUNCTION utilityScriptFileName:string;

PROCEDURE earlyFinalization;
IMPLEMENTATION
VAR unitIsInitialized:boolean=false;

OPERATOR =(CONST x,y:T_runnerStateInfo):boolean;
  begin
    result:=(x.state=y.state) and (x.request=y.request);
  end;

FUNCTION utilityScriptFileName:string;
  begin
    result:=configDir+'packages'+DirectorySeparator+'guiScripts.mnh';
    if not(fileExists(result)) then ensureDemosAndPackages(nil,nil);
  end;
{$WARN 5024 OFF}
FUNCTION main(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=250;
  VAR sleepTime:longint=0;
      r:T_evalRequest;
      recycler:T_recycler;

  PROCEDURE setupEdit(OUT globals:T_evaluationGlobals);
    begin
      P_runEvaluator(p)^.editAdapters^.clear;
      globals.create(P_runEvaluator(p)^.editAdapters);
      globals.resetForEvaluation(nil,ect_normal,C_EMPTY_STRING_ARRAY);
    end;

  PROCEDURE doneEdit(VAR globals:T_evaluationGlobals);
    VAR successful:boolean=true;
    begin
      {$ifdef debugMode} writeln(stdErr,'        DEBUG: mnhEvalThread - doneEdit'); {$endif}
      P_runEvaluator(p)^.utilityScriptPackage^.finalize(globals.primaryContext,recycler);
      globals.afterEvaluation(recycler);
      if ([mt_printdirect,mt_printline]*globals.primaryContext.messages^.collectedMessageTypes<>[]) or
         ([FlagError,FlagFatalError   ]*globals.primaryContext.messages^.getFlags<>[]) then begin
        P_runEvaluator(p)^.messages^.postSingal(mt_clearConsole,C_nilTokenLocation);
        P_runEvaluator(p)^.messages^.postCustomMessages(P_messagesErrorHolder(globals.primaryContext.messages)^.storedMessages(false));
        successful:=false;
      end;
      if P_runEvaluator(p)^.currentEdit<>nil
      then P_runEvaluator(p)^.messages^.postCustomMessage(P_runEvaluator(p)^.currentEdit^.withSuccessFlag(successful))
      else P_runEvaluator(p)^.messages^.postSingal(mt_endOfEvaluation,C_nilTokenLocation);
      globals.destroy;
      P_runEvaluator(p)^.currentEdit:=nil;
    end;

  PROCEDURE ensureEditScripts_impl();
    VAR subRule:P_subruleExpression;
        script:P_scriptMeta;
        editContext:T_evaluationGlobals;
        scriptType:T_scriptType;
        isValid:boolean;
    begin with P_runEvaluator(p)^ do begin
      setupEdit(editContext);
      if utilityScriptPackage=nil then begin
        {$ifdef debugMode} writeln(stdErr,'        DEBUG: Creating script package'); {$endif}
        new(utilityScriptPackage,create(newFileCodeProvider(utilityScriptFileName),nil));
      end else if not(utilityScriptPackage^.codeChanged) then exit;
      for script in utilityScriptList do dispose(script,destroy);
      setLength(utilityScriptList,0);
      {$ifdef debugMode} writeln(stdErr,'        DEBUG: Loading script package: ',utilityScriptPackage^.getPath); {$endif}
      utilityScriptPackage^.load(lu_forImport,editContext,recycler,C_EMPTY_STRING_ARRAY);
      if editContext.primaryContext.messages^.continueEvaluation then begin
        for scriptType in T_scriptType do
        for subRule in utilityScriptPackage^.getSubrulesByAttribute(C_scriptTypeMeta[scriptType].nameAttribute) do begin
          {$ifdef debugMode} writeln(stdErr,'        DEBUG: Found script: ',subRule^.getId); {$endif}
          new(script,create(subRule,isValid,editContext.primaryContext.messages));
          if isValid then begin
            setLength(utilityScriptList,length(utilityScriptList)+1);
            utilityScriptList[length(utilityScriptList)-1]:=script;
          end else dispose(script,destroy);
        end;
      end;
      doneEdit(editContext);
    end; end;

  PROCEDURE executeEditScript_impl;
    VAR editContext:T_evaluationGlobals;
    begin
      setupEdit(editContext);
      P_runEvaluator(p)^.currentEdit^.execute(editContext,recycler);
      doneEdit(editContext);
    end;

  begin with P_runEvaluator(p)^ do begin
    result:=0;
    recycler.initRecycler;
    repeat
      r:=pendingRequest;
      if r in [er_evaluate,er_callMain,er_reEvaluateWithGUI,er_ensureEditScripts,er_runEditScript] then begin
        sleepTime:=0;
        preEval(recycler);
        case r of
          er_evaluate: package.load(lu_forDirectExecution,globals,recycler,C_EMPTY_STRING_ARRAY);
          er_callMain: package.load(lu_forCallingMain    ,globals,recycler,parametersForMainCall);
          er_reEvaluateWithGUI: begin
            if getFileToInterpretFromCommandLine<>''
            then begin
              package.replaceCodeProvider(newFileCodeProvider                                   (getFileToInterpretFromCommandLine));
              package.load(lu_forCallingMain,globals,recycler,parametersForMainCall);
            end else begin
              package.replaceCodeProvider(newVirtualFileCodeProvider(CMD_LINE_PSEUDO_FILENAME,getCommandToInterpretFromCommandLine));
              package.load(lu_forDirectExecution,globals,recycler,parametersForMainCall);
            end;
          end;
          er_ensureEditScripts: ensureEditScripts_impl;
          er_runEditScript: executeEditScript_impl;
        end;
        postEval(recycler);
      end else begin
        if sleepTime<MAX_SLEEP_TIME then inc(sleepTime);
        sleep(sleepTime);
      end;
    until (pendingRequest=er_die);
    threadStopped;
    recycler.cleanup;
  end; end;

CONSTRUCTOR T_scriptMeta.create(CONST rule: P_subruleExpression; OUT isValid:boolean; CONST messages:P_messages);
  VAR t:T_scriptType;
  begin
    editRule:=rule;
    outputLanguage:=editRule^.metaData.getAttribute('language').value;
    createNewEditor:=editRule^.metaData.hasAttribute('newEdit');
    isValid:=false;
    for t in T_scriptType do if rule^.metaData.hasAttribute(C_scriptTypeMeta[t].nameAttribute) then begin
      scriptType:=t;
      name:=editRule^.metaData.getAttribute(C_scriptTypeMeta[t].nameAttribute).value;
      isValid:=true;
    end;
    if name='' then name:=editRule^.getId;
    if (scriptType=st_insert) and createNewEditor then begin
      isValid:=false;
      messages^.raiseSimpleError('Invalid attribute @newEdit for insert script',rule^.getLocation);
    end;
    if (scriptType=st_insert) and (outputLanguage<>'') then begin
      isValid:=false;
      messages^.raiseSimpleError('Invalid attribute @language for insert script',rule^.getLocation);
    end;
    if scriptType in [st_util,st_insert] then begin
      if not(rule^.acceptsSingleLiteral(lt_string)) then begin
        isValid:=false;
        messages^.raiseSimpleError('Scripts @'+C_scriptTypeMeta[scriptType].nameAttribute+' must accept a single string, the editor name',rule^.getLocation);
      end;
    end else begin
      if not(rule^.acceptsSingleLiteral(lt_stringList)) then begin
        isValid:=false;
        messages^.raiseSimpleError('Scripts @'+C_scriptTypeMeta[scriptType].nameAttribute+' must accept a single stringList, the editor contents',rule^.getLocation);
      end;
    end;
  end;

DESTRUCTOR T_scriptMeta.destroy;
  begin
  end;

CONSTRUCTOR T_editScriptTask.create(CONST script_:P_scriptMeta; CONST inputIndex:longint; CONST inputEditFile:string; CONST input_:TStrings; CONST inputLang:string);
  VAR i:longint;
  begin
    inherited create(mt_guiEdit_done);
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
    inherited destroy;
  end;

PROCEDURE T_editScriptTask.execute(VAR globals:T_evaluationGlobals; VAR recycler:T_recycler);
  begin
    output:=script^.editRule^.evaluateToLiteral(script^.editRule^.getLocation,@globals.primaryContext,@recycler,input,nil).literal;
    disposeLiteral(input);
    if (output<>nil) and not(output^.literalType in C_scriptTypeMeta[script^.scriptType].validResultType) then begin
      globals.primaryContext.messages^.raiseSimpleError('Script failed due to invalid result type '+output^.typeString,script^.editRule^.getLocation);
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
FUNCTION T_editScriptTask.withSuccessFlag(CONST trueWhenOk:boolean):P_editScriptTask;
  begin
    succeeded:=trueWhenOk;
    result:=@self;
  end;

PROCEDURE T_evaluator.ensureThread;
  begin
    system.enterCriticalSection(cs);
    if state=es_dead then begin
      beginThread(thread,@self);
      state:=es_idle;
    end;
    system.leaveCriticalSection(cs);
  end;

CONSTRUCTOR T_evaluator.create(CONST messages_:P_messages; threadFunc: TThreadFunc);
  begin
    system.initCriticalSection(cs);
    request:=er_none;
    state:=es_dead;
    thread:=threadFunc;
    package.create(newVirtualFileCodeProvider('?',C_EMPTY_STRING_ARRAY),nil);
    messages:=messages_;
    globals.create(messages);
  end;

CONSTRUCTOR T_runEvaluator.create(CONST messages_:P_messages; threadFunc:TThreadFunc);
  begin
    inherited create(messages_,threadFunc);
    utilityScriptPackage:=nil;
    setLength(utilityScriptList,0);
    currentEdit:=nil;
    new(editAdapters,createErrorHolder(messages,C_textMessages));
    endOfEvaluationText:='compiled on: '+{$I %DATE%}+' at: '+{$I %TIME%}+' with FPC'+{$I %FPCVERSION%}+' for '+{$I %FPCTARGET%};
  end;

DESTRUCTOR T_evaluator.destroy;
  begin
    system.enterCriticalSection(cs);
    if state in C_runningStates then globals.primaryContext.messages^.setStopFlag;
    repeat
      request:=er_die;
      system.leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      system.enterCriticalSection(cs);
    until state=es_dead;
    package.destroy;
    globals.destroy;
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

PROCEDURE T_evaluator.haltEvaluation;
  VAR recycler:T_recycler;
  begin
    recycler.initRecycler;
    system.enterCriticalSection(cs);
    haltedByUser:=true;
    globals.primaryContext.messages^.setStopFlag;
    globals.stepper^.haltEvaluation;
    globals.stopWorkers(recycler);
    request:=er_none;
    system.leaveCriticalSection(cs);
    recycler.cleanup;
  end;

PROCEDURE T_runEvaluator.reEvaluateWithGUI;
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) or (request<>er_none) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    if cmdLineInterpretation.profilingRun
    then requestedContextType:=ect_profiling
    else requestedContextType:=ect_normal;
    mainParameters:=cmdLineInterpretation.mainParameters;
    request:=er_reEvaluateWithGUI;
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.evaluate(CONST provider:P_codeProvider; CONST contextType:T_evaluationContextType; CONST firstRun:boolean);
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    requestedContextType:=contextType;
    request:=er_evaluate;
    if firstRun then package.clear(true);
    package.replaceCodeProvider(provider);
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_runEvaluator.getPackageForPostEvaluation(CONST provider: P_codeProvider; CONST firstRun:boolean): P_package;
  begin
    system.enterCriticalSection(cs);
    while state in C_runningStates do begin
      system.leaveCriticalSection(cs);
      sleep(1); ThreadSwitch;
      system.enterCriticalSection(cs);
    end;
    if firstRun then package.clear(true);
    package.replaceCodeProvider(provider);
    result:=@package;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.callMain(CONST provider: P_codeProvider; params: ansistring; CONST contextType:T_evaluationContextType; CONST firstRun:boolean);
  VAR sp:longint;
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) or (request<>er_none) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    requestedContextType:=contextType;
    setLength(mainParameters,0);
    {$WARN 5092 OFF}
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
    if firstRun then package.clear(true);
    package.replaceCodeProvider(provider);
    request:=er_callMain;
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.ensureEditScripts;
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

FUNCTION T_runEvaluator.getCurrentEdit: P_editScriptTask;
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

FUNCTION T_runEvaluator.getScripts: T_scriptMetaArray;
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

FUNCTION T_evaluator.reportVariables:P_variableTreeEntryCategoryNode;
  begin
    system.enterCriticalSection(cs);
    new(result,create(dvc_global));
    package.reportVariables(result^);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_runEvaluator.getRunnerStateInfo: T_runnerStateInfo;
  begin
    system.enterCriticalSection(cs);
    result.state:=state;
    if (globals.isPaused) then result.state:=es_debugHalted;
    if (requestedContextType in [ect_debugging,ect_debuggingAndProfiling]) and (result.state=es_running) then result.state:=es_debugRunning;
    result.request:=request;
    case result.state of
      es_running     : result.message:='Evaluating... '+myTimeToStr(now-startOfEvaluation);
      es_debugRunning: result.message:='Debugging'+StringOfChar('.',round((now-startOfEvaluation)*24*60*60) mod 4);
      es_debugHalted : result.message:='Debugging [HALTED]';
      es_editEnsuring,
      es_editRunning : result.message:='Edit script... '+myTimeToStr(now-startOfEvaluation);
      else             result.message:=endOfEvaluationText;
    end;
    result.hasPendingEditResult:=(currentEdit<>nil) and (currentEdit^.done);
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

PROCEDURE T_evaluator.preEval(VAR recycler:T_recycler);
  begin
    system.enterCriticalSection(cs);
    state:=es_running;
    haltedByUser:=false;
    case request of
      er_ensureEditScripts: state:=es_editEnsuring;
      er_runEditScript    : state:=es_editRunning;
    end;
    request:=er_none;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.preEval(VAR recycler:T_recycler);
  begin
    system.enterCriticalSection(cs);
    inherited preEval(recycler);
    startOfEvaluation:=now;
    globals.resetForEvaluation(@package,requestedContextType,mainParameters);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.postEval(VAR recycler:T_recycler);
  begin
    system.enterCriticalSection(cs);
    if not(state in [es_editEnsuring,es_editRunning]) then begin
      globals.afterEvaluation(recycler);
      memoryCleaner.callCleanupMethods;
    end;
    state:=es_idle;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.postEval(VAR recycler:T_recycler);
  begin
    system.enterCriticalSection(cs);
    inherited postEval(recycler);
    if haltedByUser
    then endOfEvaluationText:='Aborted after '+myTimeToStr(now-startOfEvaluation)
    else endOfEvaluationText:='Done in '+myTimeToStr(now-startOfEvaluation);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_runEvaluator.parametersForMainCall: T_arrayOfString;
  begin
    system.enterCriticalSection(cs);
    result:=mainParameters;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE initUnit(CONST guiAdapters:P_messages);
  begin
    runEvaluator.create(guiAdapters,@main);
    unitIsInitialized:=true;
  end;

PROCEDURE earlyFinalization;
  begin
    if unitIsInitialized then runEvaluator.destroy;
    unitIsInitialized:=false;
  end;

FINALIZATION
  earlyFinalization;
end.
