UNIT mnh_evalThread;
INTERFACE
USES sysutils,myGenerics,mnh_tokens,mnh_out_adapters,Classes,mnh_constants,mnh_tokLoc,mnh_funcs,mnh_litVar,myStringUtil;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_callMain,er_die);
  T_evaluationState=(es_dead,es_idle,es_running);
  T_tokenInfo=record
    tokenText, tokenExplanation:ansistring;
    location:T_tokenLocation;
  end;

PROCEDURE ad_clearFile;
PROCEDURE ad_evaluate(CONST L:TStrings);
PROCEDURE ad_callMain(CONST L:TStrings; params:ansistring);
PROCEDURE ad_haltEvaluation;
PROCEDURE ad_setFile(CONST path:string; CONST L:TStrings);
PROCEDURE ad_saveFile(CONST path:string; CONST L:TStrings);
FUNCTION ad_currentFile:string;
FUNCTION ad_evaluationRunning:boolean;
PROCEDURE ad_killEvaluationLoopSoftly;
PROCEDURE ad_explainIdentifier(CONST id:ansistring; VAR info:T_tokenInfo);
FUNCTION ad_needReload:boolean;
FUNCTION ad_needSave(CONST L: TStrings):boolean;
PROCEDURE ad_doReload(CONST L:TStrings);

VAR evaluationState    :specialize G_safeVar<T_evaluationState>;
    endOfEvaluationText:specialize G_safeVar<ansistring>;
    intrinsicRules,
    userRules,
    completionList:T_listOfString;

PROCEDURE initIntrinsicRuleList;
PROCEDURE initUnit;

VAR guiOutAdapters:P_adapters;

IMPLEMENTATION
VAR pendingRequest   :specialize G_safeVar<T_evalRequest>;
    unitIsInitialized:boolean=false;
    parametersForMainCall:T_arrayOfString;

FUNCTION main(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=250;
  VAR sleepTime:longint=0;
      startOfEvaluation:double;
      mainEvaluationContext:T_evaluationContext;

  PROCEDURE updateCompletionList;
    VAR list:T_listOfString;
        i:longint;
    begin
      completionList.clear;
      list:=reservedWordsByClass(rwc_not_reserved);
      for i:=0 to list.size-1 do completionList.add(list[i]);
      list.destroy;
      completionList.addAll(userRules.elementArray);
      completionList.addAll(intrinsicRules.elementArray);
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
      getMainPackage^.updateLists(userRules);
      updateCompletionList;
      evaluationState.value:=es_idle;
      if mainEvaluationContext.adapters^.hasMessageOfType[mt_el5_haltMessageReceived]
      then endOfEvaluationText.value:='Aborted after '+myTimeToStr(now-startOfEvaluation)
      else endOfEvaluationText.value:='Done in '+myTimeToStr(now-startOfEvaluation);
      while not(mainEvaluationContext.adapters^.hasMessageOfType[mt_endOfEvaluation])
      do mainEvaluationContext.adapters^.raiseCustomMessage(mt_endOfEvaluation,'',C_nilTokenLocation);
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
        reloadMainPackage(lu_forDirectExecution,mainEvaluationContext);
        postEval;
      end else if (evaluationState.value=es_idle) and (pendingRequest.value=er_callMain) then begin
        preEval;
        callMainInMain(parametersForMainCall,mainEvaluationContext);
        postEval;
      end else begin
        if sleepTime<MAX_SLEEP_TIME then inc(sleepTime);
        if pendingRequest.value=er_none then sleep(sleepTime);
      end;
    until (pendingRequest.value=er_die);
    evaluationState.value:=es_dead;
    mainEvaluationContext.destroy;
  end;

PROCEDURE ad_clearFile;
  begin
    if evaluationState.value=es_running then guiOutAdapters^.haltEvaluation;
    while evaluationState.value=es_running do sleep(1);
    mainPackageProvider.clear;
  end;

PROCEDURE ad_evaluate(CONST L: TStrings);
  begin
    mainPackageProvider.setLines(L);
    pendingRequest.value:=er_evaluate;
    if evaluationState.value=es_dead then begin
      beginThread(@main);
      sleep(10);
    end;
  end;

PROCEDURE ad_callMain(CONST L: TStrings; params: ansistring);
  VAR sp:longint;
  begin
    setLength(parametersForMainCall,0);
    params:=trim(params);
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

    mainPackageProvider.setLines(L);
    pendingRequest.value:=er_callMain;
    if evaluationState.value=es_dead then begin
      beginThread(@main);
      sleep(10);
    end;
  end;

PROCEDURE ad_haltEvaluation;
  begin
    if evaluationState.value=es_running then stepper.onAbort;
    while not(guiOutAdapters^.hasMessageOfType[mt_el5_haltMessageReceived]) do guiOutAdapters^.haltEvaluation;
    repeat pendingRequest.value:=er_none; until pendingRequest.value=er_none;
  end;

PROCEDURE ad_setFile(CONST path: string; CONST L: TStrings);
  VAR i:longint;
      LL:T_arrayOfString;
  begin
    ad_haltEvaluation;
    if path<>mainPackageProvider.getPath then begin
      mainPackageProvider.setPath(path);
      mainPackageProvider.load;
      L.clear;
      LL:=mainPackageProvider.getLines;
      for i:=0 to length(LL)-1 do L.append(LL[i]);
      setLength(LL,0);
      getMainPackage^.clear;
    end;
  end;

PROCEDURE ad_saveFile(CONST path:string; CONST L:TStrings);
  begin
    L.saveToFile(path);
    if path<>mainPackageProvider.getPath then begin
      mainPackageProvider.setPath(path);
    end;
    mainPackageProvider.setLines(L);
    mainPackageProvider.save;
  end;

FUNCTION ad_currentFile: string;
  begin
    result:=mainPackageProvider.getPath;
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
  VAR reservedWordClass: T_reservedWordClass;
      ns:T_namespace;
      packageReference: T_packageReference;
      hasBuiltinNamespace:boolean;
      token:T_token;
  begin
    info.tokenText:=id;
    info.location:=C_nilTokenLocation;
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
      packageReference:=getMainPackage^.getPackageReferenceForId(id,guiOutAdapters^);
      if packageReference.id<>'' then info.tokenExplanation:='Imported package ('+packageReference.path+')';
    end;

    token.create;
    token.define(C_nilTokenLocation,id,tt_identifier,getMainPackage);
    getMainPackage^.resolveRuleId(token,nil);
    case token.tokType of
      tt_intrinsicRule: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Builtin rule'+C_lineBreakChar+replaceAll(intrinsicRuleExplanationMap.get(id),'#',C_lineBreakChar);
      end;
      tt_importedUserRule:begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Imported rule'+C_lineBreakChar+replaceAll(P_rule(token.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(token.data)^.getLocationOfDeclaration;
      end;
      tt_localUserRule: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Local rule'+C_lineBreakChar+replaceAll(P_rule(token.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(token.data)^.getLocationOfDeclaration;
      end;
    end;
  end;

FUNCTION ad_needReload: boolean;
  begin
    result:=not(ad_evaluationRunning) and (ad_currentFile<>'') and mainPackageProvider.fileHasChanged;
  end;

FUNCTION ad_needSave(CONST L: TStrings):boolean;
  VAR i:longint;
  begin
    mainPackageProvider.setLines(L);
    if mainPackageProvider.getPath='' then begin
      result:=false;
      for i:=0 to L.count-1 do result:=result or (trim(L[i])<>'');
    end else result:=mainPackageProvider.fileIsOutOfSync;
  end;

PROCEDURE ad_doReload(CONST L: TStrings);
  VAR lines:T_arrayOfString;
      i:longint;
  begin
    L.clear;
    if mainPackageProvider.fileHasChanged then mainPackageProvider.load;
    lines:=mainPackageProvider.getLines;
    for i:=0 to length(lines)-1 do L.append(lines[i]);
  end;

PROCEDURE initIntrinsicRuleList;
  VAR ids:T_arrayOfString;
      i:longint;
  begin
    ids:=mnh_funcs.intrinsicRuleMap.keySet;
    intrinsicRules.clear;
    for i:=0 to length(ids)-1 do begin
      intrinsicRules.add(ids[i]);
      if pos('.',ids[i])<=0 then intrinsicRules.add('.'+ids[i]);
    end;
    intrinsicRules.unique;
  end;

PROCEDURE initUnit;
  begin
    pendingRequest.create(er_none);
    evaluationState.create(es_dead);
    endOfEvaluationText.create('');
    intrinsicRules.create;
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
    intrinsicRules.destroy;
    userRules.destroy;
    completionList.destroy;
  end;
end.
