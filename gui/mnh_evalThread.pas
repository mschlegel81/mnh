UNIT mnh_evalThread;
INTERFACE
USES sysutils,myGenerics,mnh_tokens,mnh_out_adapters,Classes,mnh_constants,mnh_tokLoc,mnh_funcs,mnh_litVar;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_docRun,er_callMain,er_die);
  T_evaluationState=(es_dead,es_idle,es_running);
  T_tokenInfo=record
    tokenText, tokenExplanation:ansistring;
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
FUNCTION ad_getTokenInfo(CONST line:ansistring; CONST column:longint):T_tokenInfo;
FUNCTION ad_needReload:boolean;
FUNCTION ad_needSave(CONST L: TStrings):boolean;
PROCEDURE ad_doReload(CONST L:TStrings);

VAR evaluationState    :specialize G_safeVar<T_evaluationState>;
    startOfEvaluation  :specialize G_safeVar<double>;
    endOfEvaluationText:specialize G_safeVar<ansistring>;
    intrinsicRules,
    userRules,
    completionList:T_listOfString;

PROCEDURE initIntrinsicRuleList;
PROCEDURE initUnit;

IMPLEMENTATION
VAR pendingRequest   :specialize G_safeVar<T_evalRequest>;
    unitIsInitialized:boolean=false;
    parametersForMainCall:T_arrayOfString;

FUNCTION main(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=250;
  VAR sleepTime:longint=0;
  PROCEDURE updateCompletionList;
    begin
      completionList.clear;
      completionList.add(C_tokenString[tt_modifier_memoized]);
      completionList.add(C_nanText);
      completionList.add(C_infText);
      completionList.add(C_voidText);
      completionList.add(C_tokenString[tt_aggregatorConstructor]);
      completionList.add(C_tokenString[tt_operatorXor]   );
      completionList.add(C_tokenString[tt_operatorOr]    );
      completionList.add(C_tokenString[tt_operatorMod]   );
      completionList.add(C_tokenString[tt_operatorIn]    );
      completionList.add(C_tokenString[tt_operatorDivInt]);
      completionList.add(C_tokenString[tt_operatorAnd]   );
      completionList.add(C_tokenString[tt_modifier_private] );
      completionList.add(C_tokenString[tt_modifier_memoized]);
      completionList.add(C_tokenString[tt_modifier_mutable]);
      completionList.add(C_tokenString[tt_modifier_synchronized]);
      completionList.add(C_tokenString[tt_modifier_local]);
      completionList.add(C_tokenString[tt_procedureBlockBegin]);
      completionList.add(C_tokenString[tt_procedureBlockEnd]);
      completionList.add(C_tokenString[tt_each]             );
      completionList.add(C_tokenString[tt_parallelEach]     );
      completionList.add(C_boolText[true]);
      completionList.add(C_boolText[false]);
      completionList.addAll(userRules.elementArray);
      completionList.addAll(intrinsicRules.elementArray);
      completionList.unique;
    end;

  PROCEDURE preEval;
    begin
      pendingRequest.value:=er_none;
      evaluationState.value:=es_running;
      startOfEvaluation.value:=now;
    end;

  PROCEDURE postEval;
    begin
      getMainPackage^.updateLists(userRules);
      updateCompletionList;
      evaluationState.value:=es_idle;
      if hasHaltMessage
      then endOfEvaluationText.value:='Aborted after '+formatFloat('0.000',(now-startOfEvaluation.value)*(24*60*60))+'s'
      else endOfEvaluationText.value:='Done in '+formatFloat('0.000',(now-startOfEvaluation.value)*(24*60*60))+'s';
      sleepTime:=0;
    end;

  begin
    result:=0;
    evaluationState.value:=es_idle;
    updateCompletionList;
    repeat
      if (evaluationState.value=es_idle) and (pendingRequest.value=er_evaluate) then begin
        preEval;
        reloadMainPackage(lu_forDirectExecution);
        raiseError(el0_allOkay,'reloadMainPackage done',C_nilTokenLocation);
        postEval;
      end else if (evaluationState.value=es_idle) and (pendingRequest.value=er_callMain) then begin
        preEval;
        callMainInMain(parametersForMainCall);
        postEval;
      end else begin
        if sleepTime<MAX_SLEEP_TIME then inc(sleepTime);
        if pendingRequest.value=er_none then sleep(sleepTime);
      end;
    until (pendingRequest.value=er_die);
    evaluationState.value:=es_dead;
  end;

PROCEDURE ad_clearFile;
  begin
    if evaluationState.value=es_running then haltEvaluation;
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
    if evaluationState.value=es_running then stepper.doAbort;
    pendingRequest.value:=er_none;
    while evaluationState.value=es_running do begin hasHaltMessage:=true;  pendingRequest.value:=er_none; sleep(1); end;
    raiseError(el0_allOkay,'Evaluation halted.',C_nilTokenLocation);
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
    if evaluationState.value=es_running then haltEvaluation;
    repeat pendingRequest.value:=er_die; sleep(1); until evaluationState.value=es_dead;
  end;

FUNCTION ad_getTokenInfo(CONST line: ansistring; CONST column: longint): T_tokenInfo;
  VAR token:T_token;
  begin
    result.tokenText:='';
    result.tokenExplanation:='';
    if evaluationState.value<>es_running then begin
      token:=getTokenAt(line,column);
      result.tokenText:=token.txt;
      result.tokenExplanation:='';//C_tokenInfoString[token.tokType];
      if (token.tokType=tt_intrinsicRulePointer) then begin
        result.tokenExplanation:=intrinsicRuleExplanationMap.get(token.txt);
      end else if (token.tokType in [tt_localUserRulePointer,tt_importedUserRulePointer]) then begin
        result.tokenExplanation:=P_rule(token.data)^.getDocTxt;
      end else if (token.tokType=tt_identifier) then begin
        if token.txt='USE' then begin
          result.tokenExplanation:=result.tokenExplanation+'#Identifier has context sepecific interpretation'
                                                          +'#As first token in a package, it marks the use-clause (importing packages)';
        end
      end else if (token.tokType=tt_literal) then begin
        if (token.txt=C_boolText[true]) or (token.txt=C_boolText[false]) then result.tokenExplanation:='boolean literal'
        else if (token.txt=C_nanText) then result.tokenExplanation:='numeric literal (Not-A-Number)'
        else if (token.txt=C_infText) then result.tokenExplanation:='numeric literal (Infinity)'
        else if (token.txt=C_voidText) then result.tokenExplanation:='void literal'
        else if (token.txt[1] in ['"','''']) then result.tokenExplanation:='string literal'
        else if (pos('.',token.txt)>0) or (pos('E',uppercase(token.txt))>0) then result.tokenExplanation:='real literal'
        else result.tokenExplanation:='integer literal';
      end;
    end;
  end;

FUNCTION ad_needReload: boolean;
  begin
    result:=mainPackageProvider.fileHasChanged;
  end;

FUNCTION ad_needSave(CONST L: TStrings):boolean;
  begin
    mainPackageProvider.setLines(L);
    result:=mainPackageProvider.fileIsOutOfSync;
  end;

PROCEDURE ad_doReload(CONST L: TStrings);
  VAR lines:T_arrayOfString;
      i:longint;
  begin
    ad_haltEvaluation;
    L.clear;
    mainPackageProvider.load;
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
    startOfEvaluation.create(now);
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
    startOfEvaluation.destroy;
    endOfEvaluationText.destroy;
    intrinsicRules.destroy;
    userRules.destroy;
    completionList.destroy;
  end;
end.
