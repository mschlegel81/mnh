UNIT mnh_evalThread;
INTERFACE
USES FileUtil,sysutils,myGenerics,mnh_packages,mnh_out_adapters,Classes,mnh_constants,mnh_tokLoc,mnh_funcs,mnh_litVar,
     myStringUtil,mnh_tokens,mnh_contexts,mnh_doc,mnh_cmdLineInterpretation, LazUTF8, mnh_fileWrappers;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_evaluateInteractive,er_callMain,er_reEvaluateWithGUI,er_die);
  T_evaluationState=(es_dead,es_idle,es_running);
  T_tokenInfo=record
    tokenText, tokenExplanation:ansistring;
    location:T_searchTokenLocation;
  end;

PROCEDURE ad_evaluate(CONST path:ansistring; CONST L:TStrings; CONST interactive:boolean);
PROCEDURE ad_callMain(CONST path:ansistring; CONST L: TStrings; params: ansistring);
PROCEDURE ad_haltEvaluation;
FUNCTION ad_evaluationRunning:boolean;
PROCEDURE ad_killEvaluationLoopSoftly;
PROCEDURE ad_explainIdentifier(CONST id:ansistring; VAR info:T_tokenInfo);
PROCEDURE ad_reEvaluateWithGUI;

VAR evaluationState    :specialize G_safeVar<T_evaluationState>;
    endOfEvaluationText:specialize G_safeVar<ansistring>;
    intrinsicRules,
    userRules,
    completionList:T_listOfString;

PROCEDURE initIntrinsicRuleList;
PROCEDURE initUnit;

VAR guiOutAdapters:P_adapters;
    guiPackage:T_package;
IMPLEMENTATION
VAR pendingRequest   :specialize G_safeVar<T_evalRequest>;
    unitIsInitialized:boolean=false;
    parametersForMainCall:T_arrayOfString;
    intrinsicRulesForCompletion:T_listOfString;
{$WARN 5024 OFF}
FUNCTION main(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=250;
  VAR sleepTime:longint=0;
      startOfEvaluation:double;
      mainEvaluationContext:T_evaluationContext;

  PROCEDURE updateCompletionList;
    begin
      completionList.clear;
      completionList.addAll(userRules.elementArray);
      completionList.addAll(intrinsicRulesForCompletion.elementArray);
      completionList.unique;
    end;

  PROCEDURE preEval;
    begin
      pendingRequest.value:=er_none;
      evaluationState.value:=es_running;
      startOfEvaluation:=now;
    end;

  PROCEDURE postEval;
    begin
      guiPackage.updateLists(userRules);
      updateCompletionList;
      evaluationState.value:=es_idle;
      if mainEvaluationContext.adapters^.hasMessageOfType[mt_el5_haltMessageReceived]
      then endOfEvaluationText.value:='Aborted after '+myTimeToStr(now-startOfEvaluation)
      else endOfEvaluationText.value:='Done in '+myTimeToStr(now-startOfEvaluation);
      while not(mainEvaluationContext.adapters^.hasMessageOfType[mt_endOfEvaluation])
      do mainEvaluationContext.adapters^.logEndOfEvaluation;
      sleepTime:=0;
    end;

  begin
    mainEvaluationContext.createNormalContext(guiOutAdapters);
    result:=0;
    evaluationState.value:=es_idle;
    updateCompletionList;
    repeat
      if (evaluationState.value=es_idle) and (pendingRequest.value=er_evaluate) then begin
        preEval;
        guiPackage.load(lu_forDirectExecution,mainEvaluationContext,C_EMPTY_STRING_ARRAY);
        postEval;
      end else if (evaluationState.value=es_idle) and (pendingRequest.value=er_evaluateInteractive) then begin
        preEval;
        guiPackage.load(lu_interactiveMode,mainEvaluationContext,C_EMPTY_STRING_ARRAY);
        postEval;
      end else if (evaluationState.value=es_idle) and (pendingRequest.value=er_callMain) then begin
        preEval;
        guiPackage.load(lu_forCallingMain,mainEvaluationContext,parametersForMainCall);
        postEval;
      end else if (evaluationState.value=es_idle) and (pendingRequest.value=er_reEvaluateWithGUI) then begin
        preEval;
        guiPackage.setSourcePath(getFileOrCommandToInterpretFromCommandLine);
        setupOutputBehaviour(mainEvaluationContext.adapters^);
        guiPackage.load(lu_forCallingMain,mainEvaluationContext,parametersForMainCall);
        postEval;
      end else begin
        if sleepTime<MAX_SLEEP_TIME then inc(sleepTime);
        if pendingRequest.value=er_none then sleep(sleepTime);
      end;
    until (pendingRequest.value=er_die);
    evaluationState.value:=es_dead;
    mainEvaluationContext.destroy;
  end;

PROCEDURE ad_reEvaluateWithGUI;
  begin
    pendingRequest.value:=er_reEvaluateWithGUI;
    if evaluationState.value=es_dead then begin
      beginThread(@main);
      sleep(10);
    end;
  end;

PROCEDURE ad_evaluate(CONST path:ansistring; CONST L: TStrings; CONST interactive:boolean);
  begin
    guiPackage.setSourceUTF8AndPath(L,path);
    if interactive
    then pendingRequest.value:=er_evaluateInteractive
    else pendingRequest.value:=er_evaluate;
    if evaluationState.value=es_dead then begin
      beginThread(@main);
      sleep(10);
    end;
  end;

PROCEDURE ad_callMain(CONST path:ansistring; CONST L: TStrings; params: ansistring);
  VAR sp:longint;
  begin
    setLength(parametersForMainCall,0);
    params:=trim(UTF8ToSys(params));
    while params<>'' do begin
      sp:=pos(' ',params);
      if sp<=0 then begin
        append(parametersForMainCall,params);
        params:='';
      end else begin
        append(parametersForMainCall,copy(params,1,sp-1));
        params:=trim(copy(params,sp+1,length(params)));
      end;
    end;
    guiPackage.setSourceUTF8AndPath(L,path);
    pendingRequest.value:=er_callMain;
    if evaluationState.value=es_dead then begin
      beginThread(@main);
      sleep(10);
    end;
  end;

PROCEDURE ad_haltEvaluation;
  begin
    if evaluationState.value=es_running then stepper.doStop;
    while not(guiOutAdapters^.hasMessageOfType[mt_el5_haltMessageReceived]) do guiOutAdapters^.haltEvaluation;
    repeat pendingRequest.value:=er_none; until pendingRequest.value=er_none;
  end;

FUNCTION ad_evaluationRunning: boolean;
  begin
    result:=evaluationState.value=es_running;
  end;

PROCEDURE ad_killEvaluationLoopSoftly;
  begin
    if evaluationState.value=es_running then guiOutAdapters^.haltEvaluation;
    repeat pendingRequest.value:=er_die; sleep(1); until evaluationState.value=es_dead;
  end;

PROCEDURE ad_explainIdentifier(CONST id:ansistring; VAR info:T_tokenInfo);
  PROCEDURE appendBuiltinRuleInfo(CONST prefix:string='');
    begin
      ensureBuiltinDocExamples;
      info.tokenExplanation:=info.tokenExplanation+prefix+'Builtin rule'+C_lineBreakChar+functionDocMap.get(id)^.getPlainText(C_lineBreakChar)+';';
    end;

  VAR reservedWordClass: T_reservedWordClass;
      ns:T_namespace;
      packageReference: T_packageReference;
      hasBuiltinNamespace:boolean;
      token:T_token;
  begin
    info.tokenText:=id;
    info.location.fileName:='';
    info.location.column:=0;
    info.location.line:=0;
    reservedWordClass:=isReservedWord(id);
    case reservedWordClass of
      rwc_specialLiteral:   begin info.tokenExplanation:='Reserved word: special literal';   exit; end;
      rwc_specialConstruct: begin info.tokenExplanation:='Reserved word: special construct'; exit; end;
      rwc_operator:         begin info.tokenExplanation:='Reserved word: operator';          exit; end;
      rwc_modifier:         begin info.tokenExplanation:='Reserved word: modifier';          exit; end;
    end;
    if id='USE' then begin
      info.tokenExplanation:='Context specific reserved word: as first token in a package it marks the use-clause';
      exit;
    end;
    info.tokenExplanation:='';

    hasBuiltinNamespace:=false;
    for ns:=low(T_namespace) to high(T_namespace) do if C_namespaceString[ns]=id then begin
      info.tokenExplanation:='Builtin package name';
      hasBuiltinNamespace:=true;
    end;

    if not(hasBuiltinNamespace) then begin
      packageReference:=guiPackage.getPackageReferenceForId(id,nil);
      if packageReference.id<>'' then info.tokenExplanation:='Imported package ('+packageReference.path+')';
    end;

    token.create;
    token.define(packageTokenLocation(@guiPackage),id,tt_identifier,@guiPackage);
    guiPackage.resolveRuleId(token,nil);
    case token.tokType of
      tt_intrinsicRule,tt_intrinsicRule_pon: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        appendBuiltinRuleInfo;
      end;
      tt_importedUserRule,tt_importedUserRule_pon: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Imported rule'+C_lineBreakChar+replaceAll(P_rule(token.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(token.data)^.getLocationOfDeclaration;
        if intrinsicRuleMap.containsKey(id) then appendBuiltinRuleInfo('hides ');
      end;
      tt_localUserRule,tt_localUserRule_pon: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Local rule'+C_lineBreakChar+replaceAll(P_rule(token.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(token.data)^.getLocationOfDeclaration;
        if intrinsicRuleMap.containsKey(id) then appendBuiltinRuleInfo('hides ');
      end;
    end;
  end;

PROCEDURE initIntrinsicRuleList;
  VAR ids:T_arrayOfString;
      i:longint;
      list:T_listOfString;
  begin
    ids:=mnh_funcs.intrinsicRuleMap.keySet;
    intrinsicRules.clear;
    for i:=0 to length(ids)-1 do begin
      if pos(C_ID_QUALIFY_CHARACTER,ids[i])<=0 then begin
        intrinsicRules.add(    ids[i]);
        intrinsicRulesForCompletion.add(ids[i]);
        intrinsicRulesForCompletion.add(C_ID_QUALIFY_CHARACTER+ids[i]);
      end else begin
        intrinsicRules.add(split(ids[i],C_ID_QUALIFY_CHARACTER)[0]);
        intrinsicRules.add(split(ids[i],C_ID_QUALIFY_CHARACTER)[1]);
        intrinsicRulesForCompletion.add(ids[i]);
        intrinsicRulesForCompletion.add(split(ids[i],C_ID_QUALIFY_CHARACTER)[0]);
        intrinsicRulesForCompletion.add(split(ids[i],C_ID_QUALIFY_CHARACTER)[1]);
        intrinsicRulesForCompletion.add(C_ID_QUALIFY_CHARACTER+split(ids[i],C_ID_QUALIFY_CHARACTER)[0]);
        intrinsicRulesForCompletion.add(C_ID_QUALIFY_CHARACTER+split(ids[i],C_ID_QUALIFY_CHARACTER)[1]);
      end;
    end;
    list:=reservedWordsByClass(rwc_not_reserved);
    for i:=0 to list.size-1 do intrinsicRulesForCompletion.add(list[i]);
    list.destroy;
    intrinsicRules.unique;
    intrinsicRulesForCompletion.unique;
  end;

PROCEDURE initUnit;
  begin
    guiPackage.create(nil);
    pendingRequest.create(er_none);
    evaluationState.create(es_dead);
    endOfEvaluationText.create('');
    intrinsicRules.create;
    intrinsicRulesForCompletion.create;
    initIntrinsicRuleList;
    userRules.create;
    completionList.create;
    beginThread(@main);
    unitIsInitialized:=true;
  end;

FINALIZATION
  if unitIsInitialized then begin
    ad_killEvaluationLoopSoftly;
    pendingRequest.destroy;
    evaluationState.destroy;
    endOfEvaluationText.destroy;
    intrinsicRulesForCompletion.destroy;
    intrinsicRules.destroy;
    userRules.destroy;
    completionList.destroy;
    guiPackage.destroy;
  end;
end.
