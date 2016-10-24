UNIT mnh_evalThread;
INTERFACE
USES FileUtil,sysutils,myGenerics,mnh_packages,mnh_out_adapters,Classes,mnh_constants,mnh_basicTypes,mnh_funcs,mnh_litVar,
     myStringUtil,mnh_tokens,mnh_contexts,mnh_doc,mnh_cmdLineInterpretation, LazUTF8, mnh_fileWrappers;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_callMain,er_reEvaluateWithGUI,er_die);
  T_evaluationState=(es_dead,es_idle,es_running);

  T_tokenInfo=record
    tokenText, tokenExplanation:ansistring;
    location,
    startLoc,endLoc:T_searchTokenLocation;
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
      FUNCTION parametersForMainCall:T_arrayOfString;
      PROCEDURE preEval; virtual;
      PROCEDURE postEval; virtual;
    public
      CONSTRUCTOR create(CONST adapters:P_adapters; threadFunc:TThreadFunc);
      DESTRUCTOR destroy; virtual;
      PROCEDURE reEvaluateWithGUI(CONST contextType:T_contextType);
      PROCEDURE evaluate(CONST path:ansistring; CONST L: TStrings; CONST contextType:T_contextType);
      PROCEDURE callMain(CONST path:ansistring; CONST L: TStrings; params: ansistring; CONST contextType:T_contextType);
      FUNCTION getEndOfEvaluationText:ansistring;
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
  end;

VAR runEvaluator:T_runEvaluator;
    assistancEvaluator:T_assistanceEvaluator;

PROCEDURE initUnit(CONST guiAdapters:P_adapters);

IMPLEMENTATION
VAR unitIsInitialized:boolean=false;
    silentAdapters:T_adapters;
    intrinsicRulesForCompletion:T_listOfString;

{$WARN 5024 OFF}
FUNCTION main(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=250;
  VAR sleepTime:longint=0;
      r:T_evalRequest;
  begin with P_runEvaluator(p)^ do begin
    {$ifdef debugMode} writeln(stdErr,'Thread ',ThreadID,' handles main evaluation loop');{$endif}
    result:=0;
    repeat
      r:=pendingRequest;
      if r in [er_evaluate,er_callMain,er_reEvaluateWithGUI] then begin
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


PROCEDURE T_evaluator.ensureThread;
  begin
    enterCriticalSection(cs);
    if state=es_dead then begin
      beginThread(thread,@self);
      state:=es_idle;
    end;
    leaveCriticalSection(cs);
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
    enterCriticalSection(cs);
    if state=es_running then adapter^.haltEvaluation;
    repeat
      request:=er_die;
      leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      enterCriticalSection(cs);
    until state=es_dead;
    package.destroy;
    context.destroy;
    leaveCriticalSection(cs);
    system.doneCriticalSection(cs);
  end;

DESTRUCTOR T_runEvaluator.destroy;
  begin
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
    enterCriticalSection(cs);
    context.haltEvaluation;
    while not(adapter^.hasMessageOfType[mt_el5_haltMessageReceived]) do begin
      leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      enterCriticalSection(cs);
    end;
    request:=er_none;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.reEvaluateWithGUI(CONST contextType:T_contextType);
  begin
    enterCriticalSection(cs);
    if (state=es_running) or (request<>er_none) then begin
      leaveCriticalSection(cs);
      exit;
    end;
    requestedContextType:=contextType;
    mainParameters:=mnh_cmdLineInterpretation.mainParameters;
    request:=er_reEvaluateWithGUI;
    ensureThread;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.evaluate(CONST path: ansistring; CONST L: TStrings; CONST contextType:T_contextType);
  begin
    enterCriticalSection(cs);
    ensureThread;
    if (state=es_running) then begin
      leaveCriticalSection(cs);
      exit;
    end;
    requestedContextType:=contextType;
    request:=er_evaluate;
    package.setSourceUTF8AndPath(L,path);
    leaveCriticalSection(cs);
  end;

PROCEDURE T_assistanceEvaluator.evaluate(CONST path: ansistring; CONST L: TStrings);
  begin
    enterCriticalSection(cs);
    ensureThread;
    if (state=es_running) then begin
      leaveCriticalSection(cs);
      exit;
    end;
    request:=er_evaluate;
    package.setSourceUTF8AndPath(L,path);
    leaveCriticalSection(cs);
  end;


PROCEDURE T_runEvaluator.callMain(CONST path: ansistring; CONST L: TStrings; params: ansistring; CONST contextType:T_contextType);
  VAR sp:longint;
  begin
    enterCriticalSection(cs);
    if (state=es_running) or (request<>er_none) then begin
      leaveCriticalSection(cs);
      exit;
    end;
    requestedContextType:=contextType;
    setLength(mainParameters,0);
    params:=trim(UTF8ToSys(params));
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
    leaveCriticalSection(cs);
  end;

FUNCTION T_evaluator.evaluationRunning: boolean;
  begin
    result:=state=es_running;
  end;

FUNCTION T_evaluator.evaluationRunningOrPending: boolean;
  begin
    result:=(state=es_running) or (request in [er_evaluate,er_callMain,er_reEvaluateWithGUI]);
  end;

FUNCTION T_evaluator.getCodeProvider: P_codeProvider;
  begin
    enterCriticalSection(cs);
    result:=package.getCodeProvider;
    leaveCriticalSection(cs);
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
      lastComment:ansistring='';
  begin
    if (CaretY=info.startLoc.line) and (CaretX>=info.startLoc.column) and (CaretX<info.endLoc.column) then exit;
    enterCriticalSection(cs);
    while (state=es_running) do begin
      leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      enterCriticalSection(cs);
    end;

    tokens.create;
    loc.line:=1;
    loc.column:=1;
    loc.package:=@package;
    adapter^.clearAll;
    tokens.tokenizeAll(fullLine,loc,@package,adapter^,false);
    tokens.step(@package,lastComment,adapter^);

    while not(tokens.atEnd) and (tokens.current.location.column<CaretX) do tokens.step(@package,lastComment,adapter^);
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
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.reportVariables(VAR report: T_variableReport);
  begin
    enterCriticalSection(cs);
    package.reportVariables(report);
    leaveCriticalSection(cs);
  end;

FUNCTION T_runEvaluator.getEndOfEvaluationText: ansistring;
  begin
    enterCriticalSection(cs);
    result:=endOfEvaluationText;
    leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
  VAR i:longint;
  begin
    enterCriticalSection(cs);
    result:=0;
    for i:=0 to length(localErrors)-1 do with localErrors[i] do
    if (result=0) and (lineIndex=location.line-1) and ((location.column<0) or (tokenStart<=location.column-1) and (tokenEnd>location.column-1)) then begin
      if C_messageTypeMeta[messageType].level>2 then result:=2 else result:=1;
    end;
    leaveCriticalSection(cs);
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
    enterCriticalSection(cs);
    setLength(result,length(localErrors)+length(externalErrors));
    k:=0;
    addErrors(localErrors);
    addErrors(externalErrors);
    leaveCriticalSection(cs);
  end;


FUNCTION T_evaluator.pendingRequest: T_evalRequest;
  begin
    enterCriticalSection(cs);
    if state=es_idle
    then result:=request
    else result:=er_none;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.threadStopped;
  begin
    enterCriticalSection(cs);
    state:=es_dead;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.preEval;
  begin
    enterCriticalSection(cs);
    request:=er_none;
    state:=es_running;
    if pendingRequest=er_reEvaluateWithGUI then context.removeOption(cp_clearAdaptersOnStart);
    context.resetForEvaluation(@package);
    leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.preEval;
  begin
    enterCriticalSection(cs);
    context.resetOptions(requestedContextType);
    inherited preEval;
    startOfEvaluation:=now;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.postEval;
  begin
    enterCriticalSection(cs);
    context.afterEvaluation;
    state:=es_idle;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_runEvaluator.postEval;
  begin
    enterCriticalSection(cs);
    inherited postEval;
    if adapter^.hasMessageOfType[mt_el5_haltMessageReceived]
    then endOfEvaluationText:='Aborted after '+myTimeToStr(now-startOfEvaluation)
    else endOfEvaluationText:='Done in '+myTimeToStr(now-startOfEvaluation);
    leaveCriticalSection(cs);
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
    enterCriticalSection(cs);
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
    leaveCriticalSection(cs);
  end;

FUNCTION T_runEvaluator.parametersForMainCall: T_arrayOfString;
  begin
    enterCriticalSection(cs);
    result:=mainParameters;
    leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.getStateCounter: longint;
  begin
    enterCriticalSection(cs);
    result:=stateCounter;
    leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.isUserRule(CONST id:string):boolean;
  begin
    enterCriticalSection(cs);
    result:=userRules.contains(id);
    leaveCriticalSection(cs);
  end;

FUNCTION T_assistanceEvaluator.resolveImport(CONST id: string): string;
  begin
    result:=package.getSecondaryPackageById(id);
  end;

PROCEDURE T_assistanceEvaluator.extendCompletionList(VAR list: T_listOfString);
  begin
    enterCriticalSection(cs);
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
