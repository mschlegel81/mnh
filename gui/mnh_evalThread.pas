UNIT mnh_evalThread;
INTERFACE
USES FileUtil,sysutils,myGenerics,mnh_packages,mnh_out_adapters,Classes,mnh_constants,mnh_basicTypes,mnh_funcs,mnh_litVar,
     myStringUtil,mnh_tokens,mnh_contexts,mnh_doc,mnh_cmdLineInterpretation, LazUTF8, mnh_fileWrappers;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_callMain,er_reEvaluateWithGUI,er_ensureEditScripts,er_runEditScript,er_die);
  T_evaluationState=(es_dead,es_idle,es_running,es_debugRunning,es_debugHalted,es_editEnsuring,es_editRunning);
CONST
  C_runningStates:set of T_evaluationState=[es_running,es_debugRunning,es_debugHalted,es_editEnsuring,es_editRunning];
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

  P_editScriptMeta=^T_editScriptMeta;
  T_editScriptMeta=object
    private
      name:string;
      editRule:P_subrule;
      outputLanguage:string;
      createNewEditor:boolean;
      CONSTRUCTOR create(CONST rule:P_subrule);
      DESTRUCTOR destroy;
    public
      FUNCTION getName:string;
  end;

  P_editScriptTask=^T_editScriptTask;
  T_editScriptTask=object
    private
      script:P_editScriptMeta;
      inputEditIndex:longint;
      input :P_listLiteral;
      output:P_literal;
      outputLanguage:string;
      done  :boolean;
      CONSTRUCTOR create(CONST script_:P_editScriptMeta; CONST inputIndex:longint; CONST input_:TStrings; CONST inputLang:string);
      DESTRUCTOR destroy;
      PROCEDURE execute(VAR context:T_evaluationContext);
    public
      FUNCTION getOutput:P_literal;
      FUNCTION getOutputLanguage:string;
      FUNCTION wantNewEditor:boolean;
      FUNCTION inputIdx:longint;
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
      PROCEDURE haltEvaluation;

      FUNCTION evaluationRunning: boolean;
      FUNCTION evaluationRunningOrPending: boolean;
      FUNCTION getCodeProvider:P_codeProvider;
      PROCEDURE reportVariables(VAR report:T_variableReport);
  end;

  P_runEvaluator=^T_runEvaluator;
  T_runEvaluator=object(T_evaluator)
    private
      //request variables
      requestedContextType:T_contextType;
      mainParameters:T_arrayOfString;
      //internal state variables
      startOfEvaluation:double;
      endOfEvaluationText:ansistring;

      editScriptPackage:P_package;
      editScriptList:array of P_editScriptMeta;
      currentEdit:P_editScriptTask;

      FUNCTION parametersForMainCall:T_arrayOfString;
      PROCEDURE preEval; virtual;
      PROCEDURE postEval; virtual;
    public
      CONSTRUCTOR create(CONST adapters:P_adapters; threadFunc:TThreadFunc);
      DESTRUCTOR destroy; virtual;
      PROCEDURE reEvaluateWithGUI(CONST contextType:T_contextType);
      PROCEDURE evaluate         (CONST path:ansistring; CONST L: TStrings; CONST contextType:T_contextType);
      PROCEDURE callMain         (CONST path:ansistring; CONST L: TStrings; params: ansistring; CONST contextType:T_contextType);
      PROCEDURE ensureEditScripts();
      PROCEDURE runEditScript    (CONST scriptIndex,editorIndex:longint; CONST L:TStrings; CONST inputLang:string);
      FUNCTION getRunnerStateInfo:T_runnerStateInfo;

      FUNCTION getCurrentEdit:P_editScriptTask;
      PROCEDURE freeCurrentEdit;

      FUNCTION getEditScriptNames:T_arrayOfString;
  end;

  P_assistanceEvaluator=^T_assistanceEvaluator;
  T_assistanceEvaluator=object(T_evaluator)
    private
      localErrors,externalErrors:T_storedMessages;
      stateCounter:longint;
      userRules,
      completionList:T_listOfString;
      PROCEDURE postEval; virtual;
    public
      CONSTRUCTOR create(CONST adapters:P_adapters; threadFunc:TThreadFunc);
      DESTRUCTOR destroy; virtual;

      PROCEDURE evaluate(CONST path:ansistring; CONST L: TStrings);

      FUNCTION isErrorLocation(CONST lineIndex,tokenStart,tokenEnd:longint):byte;
      FUNCTION getErrorHints:T_arrayOfString;
      FUNCTION getStateCounter:longint;
      FUNCTION isUserRule(CONST id:string):boolean;
      FUNCTION resolveImport(CONST id:string):string;
      PROCEDURE extendCompletionList(VAR list:T_listOfString);
      PROCEDURE explainIdentifier(CONST fullLine:ansistring; CONST CaretY,CaretX:longint; VAR info:T_tokenInfo);
      FUNCTION getAllUsedFiles:T_arrayOfString;
  end;

VAR runEvaluator:T_runEvaluator;
    assistancEvaluator:T_assistanceEvaluator;

PROCEDURE initUnit(CONST guiAdapters:P_adapters);
OPERATOR =(CONST x,y:T_runnerStateInfo):boolean;
FUNCTION editScriptFileName:string;
IMPLEMENTATION
VAR unitIsInitialized:boolean=false;
    silentAdapters:T_adapters;
    intrinsicRulesForCompletion:T_listOfString;

OPERATOR =(CONST x,y:T_runnerStateInfo):boolean;
  begin
    result:=(x.state=y.state) and (x.request=y.request) and (x.message=y.message);
  end;

FUNCTION editScriptFileName:string;
  begin
    result:=configDir+'packages'+DirectorySeparator+'editScripts.mnh';
    if not(fileExists(result)) then ensureDemos;
  end;


{$WARN 5024 OFF}
FUNCTION main(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=250;
  VAR sleepTime:longint=0;
      r:T_evalRequest;

  PROCEDURE setupEdit(OUT collector:P_collectingOutAdapter; OUT context:T_evaluationContext);
    VAR adapters:P_adapters;
    begin
      new(collector,create(at_sandboxAdapter,C_defaultOutputBehavior_fileMode));
      new(adapters,create);
      adapters^.addOutAdapter(collector,true);
      context.createContext(adapters,ct_normal);
      context.removeOption(cp_timing);
      context.removeOption(cp_spawnWorker);
      context.removeOption(cp_createDetachedTask);
      context.resetForEvaluation(nil);
    end;

  PROCEDURE doneEdit(VAR collector:P_collectingOutAdapter; VAR context:T_evaluationContext);
    VAR i:longint;
        adapters:P_adapters;
    begin
      context.afterEvaluation;
      if (context.adapters^.hasMessageOfType[mt_printline]) or
         (context.adapters^.hasMessageOfType[mt_el3_evalError]) or
         (context.adapters^.hasMessageOfType[mt_el3_userDefined]) or
         (context.adapters^.hasMessageOfType[mt_el4_parsingError]) or
         (context.adapters^.hasMessageOfType[mt_el5_systemError]) then begin
        P_runEvaluator(p)^.context.adapters^.clearPrint;
        for i:=0 to length(collector^.storedMessages)-1 do P_runEvaluator(p)^.context.adapters^.raiseCustomMessage(collector^.storedMessages[i]);
      end;
      adapters:=context.adapters;
      context.destroy;
      dispose(adapters,destroy);
    end;

  PROCEDURE ensureEditScripts_impl();
    VAR subRule:P_subrule;
        script:P_editScriptMeta;
        editContext:T_evaluationContext;
        collector:P_collectingOutAdapter;
    begin with P_runEvaluator(p)^ do begin
      setupEdit(collector,editContext);
      if editScriptPackage=nil then begin
        {$ifdef debugMode} writeln('Creating edit script package'); {$endif}
        new(editScriptPackage,create(nil));
        editScriptPackage^.setSourcePath(editScriptFileName);
      end else if not(editScriptPackage^.getCodeProvider^.fileHasChanged) then exit;
      for script in editScriptList do dispose(Script,destroy);
      setLength(editScriptList,0);
      {$ifdef debugMode} writeln('Loading edit script package: ',editScriptPackage^.getPath); {$endif}
      editScriptPackage^.getCodeProvider^.load;
      editScriptPackage^.load(lu_forImport,editContext,C_EMPTY_STRING_ARRAY);
      if editContext.adapters^.noErrors then begin
        for subRule in editScriptPackage^.getSubrulesByAttribute('editScript') do begin
          {$ifdef debugMode} writeln('Found edit script: ',subRule^.getId); {$endif}
          new(script,create(subRule));
          setLength(editScriptList,length(editScriptList)+1);
          editScriptList[length(editScriptList)-1]:=script;
        end;
      end;
      doneEdit(collector,editContext);
    end; end;

  PROCEDURE executeEditScript_impl;
    VAR editContext:T_evaluationContext;
        collector:P_collectingOutAdapter;
    begin
      setupEdit(collector,editContext);
      P_runEvaluator(p)^.currentEdit^.execute(editContext);
      doneEdit(collector,editContext);
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
          er_evaluate: package.load(lu_forDirectExecution,context,C_EMPTY_STRING_ARRAY);
          er_callMain: package.load(lu_forCallingMain    ,context,parametersForMainCall);
          er_reEvaluateWithGUI: begin
            package.setSourcePath(getFileOrCommandToInterpretFromCommandLine);
            package.load(lu_forCallingMain,context,parametersForMainCall);
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
        package.load(lu_forCodeAssistance,context,C_EMPTY_STRING_ARRAY);
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

CONSTRUCTOR T_editScriptMeta.create(CONST rule: P_subrule);
  begin
    editRule:=rule;
    outputLanguage:=editRule^.getAttribute('language').value;
    createNewEditor:=editRule^.hasAttribute('newEdit');
    name:=editRule^.getAttribute('editScript').value;
    if name='' then name:=editRule^.getId;
  end;

DESTRUCTOR T_editScriptMeta.destroy;
  begin
  end;

FUNCTION T_editScriptMeta.getName: string;
  begin
    result:=name;
  end;

CONSTRUCTOR T_editScriptTask.create(CONST script_:P_editScriptMeta; CONST inputIndex:longint; CONST input_:TStrings; CONST inputLang:string);
  VAR i:longint;
  begin
    script:=script_;
    inputEditIndex:=inputIndex;
    input:=newListLiteral(input_.count);
    for i:=0 to input_.count-1 do input^.appendString(input_[i]);
    output:=nil;
    outputLanguage:=script^.outputLanguage;
    if outputLanguage='' then outputLanguage:=inputLang;
    done:=false;
  end;

DESTRUCTOR T_editScriptTask.destroy;
  begin
    if output<>nil then disposeLiteral(output);
  end;

PROCEDURE T_editScriptTask.execute(VAR context:T_evaluationContext);
  begin
    output:=script^.editRule^.directEvaluateUnary(input,script^.editRule^.getLocation,context,0);
    done:=true;
  end;

FUNCTION T_editScriptTask.getOutput:P_literal; begin result:=output; end;
FUNCTION T_editScriptTask.getOutputLanguage:string; begin result:=outputLanguage; end;
FUNCTION T_editScriptTask.wantNewEditor:boolean; begin result:=(script^.createNewEditor) and (output<>nil) and (output^.literalType=lt_stringList); end;
FUNCTION T_editScriptTask.inputIdx:longint; begin result:=inputEditIndex; end;

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
    package.create(nil);
    adapter:=adapters;
    context.createContext(adapter,ct_normal);
  end;

CONSTRUCTOR T_runEvaluator.create(CONST adapters:P_adapters; threadFunc:TThreadFunc);
  begin
    inherited create(adapters,threadFunc);
    editScriptPackage:=nil;
    setLength(editScriptList,0);
    currentEdit:=nil;
    endOfEvaluationText:='compiled on: '+{$I %DATE%}+' at: '+{$I %TIME%}+' with FPC'+{$I %FPCVERSION%}+' for '+{$I %FPCTARGET%};
  end;

CONSTRUCTOR T_assistanceEvaluator.create(CONST adapters: P_adapters; threadFunc: TThreadFunc);
  begin
    inherited create(adapters,threadFunc);
    context.resetOptions(ct_silentlyRunAlone);
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
    if editScriptPackage<>nil then dispose(editScriptPackage,destroy);
    for i:=0 to length(editScriptList)-1 do dispose(editScriptList[i],destroy);
    setLength(editScriptList,0);
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
    context.haltEvaluation;
    while not(adapter^.hasMessageOfType[mt_el5_haltMessageReceived]) do begin
      system.leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      system.enterCriticalSection(cs);
    end;
    request:=er_none;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.reEvaluateWithGUI(CONST contextType:T_contextType);
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) or (request<>er_none) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    requestedContextType:=contextType;
    mainParameters:=mnh_cmdLineInterpretation.mainParameters;
    request:=er_reEvaluateWithGUI;
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.evaluate(CONST path: ansistring; CONST L: TStrings; CONST contextType:T_contextType);
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    requestedContextType:=contextType;
    request:=er_evaluate;
    package.setSourceUTF8AndPath(L,path);
    ensureThread;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_assistanceEvaluator.evaluate(CONST path: ansistring; CONST L: TStrings);
  begin
    system.enterCriticalSection(cs);
    ensureThread;
    if (state in C_runningStates) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    request:=er_evaluate;
    package.setSourceUTF8AndPath(L,path);
    system.leaveCriticalSection(cs);
  end;


PROCEDURE T_runEvaluator.callMain(CONST path: ansistring; CONST L: TStrings; params: ansistring; CONST contextType:T_contextType);
  VAR sp:longint;
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) or (request<>er_none) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    requestedContextType:=contextType;
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
    package.setSourceUTF8AndPath(L,path);
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

PROCEDURE T_runEvaluator.runEditScript(CONST scriptIndex,editorIndex:longint; CONST L:TStrings; CONST inputLang:string);
  begin
    system.enterCriticalSection(cs);
    if (state in C_runningStates) or (currentEdit<>nil) then begin
      system.leaveCriticalSection(cs);
      exit;
    end;
    request:=er_runEditScript;
    new(currentEdit,create(editScriptList[scriptIndex],editorIndex,L,inputLang));
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

FUNCTION T_runEvaluator.getEditScriptNames:T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,length(editScriptList));
    for i:=0 to length(result)-1 do result[i]:=editScriptList[i]^.name;
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

    if tokenToExplain.tokType=tt_identifier then package.resolveRuleId(tokenToExplain,nil);

    info.tokenExplanation:=replaceAll(C_tokenInfo[tokenToExplain.tokType].helpText,'#',C_lineBreakChar);
    for i:=0 to length(C_specialWordInfo)-1 do
      if C_specialWordInfo[i].txt=info.tokenText then
      info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar+replaceAll(C_specialWordInfo[i].helpText,'#',C_lineBreakChar);

    case tokenToExplain.tokType of
      tt_intrinsicRule: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        appendBuiltinRuleInfo;
      end;
      tt_importedUserRule: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Imported rule'+C_lineBreakChar+replaceAll(P_rule(tokenToExplain.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(tokenToExplain.data)^.getLocationOfDeclaration;
        if intrinsicRuleMap.containsKey(tokenToExplain.txt) then appendBuiltinRuleInfo('hides ');
      end;
      tt_localUserRule: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Local rule'+C_lineBreakChar+replaceAll(P_rule(tokenToExplain.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(tokenToExplain.data)^.getLocationOfDeclaration;
        if intrinsicRuleMap.containsKey(tokenToExplain.txt) then appendBuiltinRuleInfo('hides ');
      end;
      tt_customTypeRule, tt_customTypeCheck: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Custom type'+C_lineBreakChar+replaceAll(P_rule(tokenToExplain.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(tokenToExplain.data)^.getLocationOfDeclaration;
        if intrinsicRuleMap.containsKey(tokenToExplain.txt) then appendBuiltinRuleInfo('hides ');
      end;
    end;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.getAllUsedFiles:T_arrayOfString;
  begin
    system.enterCriticalSection(cs);
    result:=package.getPackageFileNameList;
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
    if (context.hasOption(cp_debug)) and (context.paused) then result.state:=es_debugHalted;
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
    begin
      for i:=0 to length(list)-1 do with list[i] do begin
        result[k]:=UTF8_ZERO_WIDTH_SPACE+C_messageTypeMeta[messageType].prefix+' '+ansistring(location)+' '+(simpleMessage);
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
    if context.hasOption(cp_debug)
    then state:=es_debugRunning
    else state:=es_running;
    case request of
      er_ensureEditScripts: state:=es_editEnsuring;
      er_runEditScript    : state:=es_editRunning;
      er_reEvaluateWithGUI: context.removeOption(cp_clearAdaptersOnStart);
    end;
    request:=er_none;
    if not(state in [es_editEnsuring,es_editRunning]) then context.resetForEvaluation(@package);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.preEval;
  begin
    system.enterCriticalSection(cs);
    context.resetOptions(requestedContextType);
    inherited preEval;
    startOfEvaluation:=now;
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
    if adapter^.hasMessageOfType[mt_el5_haltMessageReceived]
    then endOfEvaluationText:='Aborted after '+myTimeToStr(now-startOfEvaluation)
    else endOfEvaluationText:='Done in '+myTimeToStr(now-startOfEvaluation);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_assistanceEvaluator.postEval;
  PROCEDURE updateCompletionList;
    VAR i:longint;
    begin
      completionList.clear;
      completionList.addAll(intrinsicRulesForCompletion.elementArray);
      package.updateLists(userRules);
      completionList.addAll(userRules.elementArray);
      for i:=0 to userRules.size-1 do if pos(ID_QUALIFY_CHARACTER,userRules[i])<=0 then completionList.add(ID_QUALIFY_CHARACTER+userRules[i]);
      completionList.unique;
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

PROCEDURE T_assistanceEvaluator.extendCompletionList(VAR list: T_listOfString);
  begin
    system.enterCriticalSection(cs);
    list.addAll(completionList.elementArray);
    leaveCriticalSection(cs);
  end;

PROCEDURE initUnit(CONST guiAdapters:P_adapters);
  PROCEDURE initIntrinsicRuleList;
    VAR ids:T_arrayOfString;
        i:longint;
        tt:T_tokenType;
    begin
      ids:=mnh_funcs.intrinsicRuleMap.keySet;
      intrinsicRulesForCompletion.create;
      for i:=0 to length(ids)-1 do begin
        if pos(ID_QUALIFY_CHARACTER,ids[i])<=0 then begin
          intrinsicRulesForCompletion.add(ids[i]);
          intrinsicRulesForCompletion.add(ID_QUALIFY_CHARACTER+ids[i]);
        end else begin
          intrinsicRulesForCompletion.add(ids[i]);
          intrinsicRulesForCompletion.add(split(ids[i],ID_QUALIFY_CHARACTER)[0]);
          intrinsicRulesForCompletion.add(split(ids[i],ID_QUALIFY_CHARACTER)[1]);
          intrinsicRulesForCompletion.add(ID_QUALIFY_CHARACTER+split(ids[i],ID_QUALIFY_CHARACTER)[0]);
          intrinsicRulesForCompletion.add(ID_QUALIFY_CHARACTER+split(ids[i],ID_QUALIFY_CHARACTER)[1]);
        end;
      end;
      for tt:=low(T_tokenType) to high(T_tokenType) do if isIdentifier(C_tokenInfo[tt].defaultId,true) then
        intrinsicRulesForCompletion.add(replaceAll(C_tokenInfo[tt].defaultId,'.',''));
      for i:=low(C_specialWordInfo) to high(C_specialWordInfo) do
        intrinsicRulesForCompletion.add(C_specialWordInfo[i].txt);
      intrinsicRulesForCompletion.unique;
    end;

  VAR collector:P_collectingOutAdapter;
  begin
    runEvaluator.create(guiAdapters,@main);
    silentAdapters.create;
    new(collector,create(at_unknown,C_collectAllOutputBehavior));
    silentAdapters.addOutAdapter(collector,true);
    assistancEvaluator.create(@silentAdapters,@docMain);
    initIntrinsicRuleList;
    unitIsInitialized:=true;
  end;

FINALIZATION
  if unitIsInitialized then begin
    runEvaluator.destroy;
    assistancEvaluator.destroy;
    silentAdapters.destroy;
  end;
end.
