UNIT mnh_evalThread;
INTERFACE
USES sysutils,myGenerics,mnh_tokens,mnh_out_adapters,classes,mnh_fileWrappers,mnh_constants,mnh_tokloc,mnh_funcs;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_die);
  T_evaluationState=(es_dead,es_idle,es_running);
  T_tokenInfo=record
    tokenText, tokenExplanation:ansistring;
    declaredInLine:longint;
    declaredInFile:ansistring;
  end;

PROCEDURE ad_clearFile;
PROCEDURE ad_evaluate(CONST L:TStrings);
PROCEDURE ad_haltEvaluation;
PROCEDURE ad_setFile(CONST path:string; CONST L:TStrings);
FUNCTION ad_currentFile:string;
FUNCTION ad_evaluationRunning:Boolean;
PROCEDURE ad_killEvaluationLoopSoftly;
FUNCTION ad_getTokenInfo(CONST line:ansistring; CONST column:longint):T_tokenInfo;
FUNCTION ad_needReload:boolean;
PROCEDURE ad_doReload(CONST L:TStrings);

VAR evaluationState    :specialize G_safeVar<T_evaluationState>;
    startOfEvaluation  :specialize G_safeVar<double>;
    endOfEvaluationText:specialize G_safeVar<AnsiString>;
    startOfEvaluationCallback:PROCEDURE;

    intrinsicRules,
    localUserRules,
    importedUserRules:T_listOfString;

IMPLEMENTATION
VAR pendingRequest   :specialize G_safeVar<T_evalRequest>;

FUNCTION main(p:pointer):ptrint;
  VAR idleCount:longint=0;
  begin
    evaluationState.value:=es_idle;
    repeat
      if (evaluationState.value=es_idle) and (pendingRequest.value=er_evaluate) then begin
        idleCount:=0;
        pendingRequest.value:=er_none;
        evaluationState.value:=es_running;
        startOfEvaluation.value:=now;
        startOfEvaluationCallback();
        reloadMainPackage;
        raiseError(el0_allOkay,'reloadMainPackage done',C_nilTokenLocation);
        getMainPackage^.updateLists(localUserRules,importedUserRules);
        evaluationState.value:=es_idle;
        if hasMessage(el5_systemError,HALT_MESSAGE)
        then endOfEvaluationText.value:='Aborted after '+formatFloat('0.000',(now-startOfEvaluation.value)*(24*60*60))+'s'
        else endOfEvaluationText.value:='Done in '+formatFloat('0.000',(now-startOfEvaluation.value)*(24*60*60))+'s';
      end else inc(idleCount);
      sleep(10);
    until (pendingRequest.value<>er_evaluate) and (idleCount>100) or (pendingRequest.value=er_die);
    evaluationState.value:=es_dead;
  end;

procedure ad_clearFile;
  begin
    if evaluationState.value=es_running then haltEvaluation;
    while evaluationState.value=es_running do sleep(1);
    mainPackageProvider.clear;
  end;

procedure ad_evaluate(const L: TStrings);
  begin
    mainPackageProvider.setLines(L);
    pendingRequest.value:=er_evaluate;
    if evaluationState.value=es_dead then begin
      beginThread(@main);
      sleep(10);
    end;
  end;

procedure ad_haltEvaluation;
  begin
    if evaluationState.value=es_running then haltEvaluation;
    pendingRequest.value:=er_none;
    while evaluationState.value=es_running do sleep(1);
    raiseError(el0_allOkay,'Evaluation halted.',C_nilTokenLocation);
  end;

procedure ad_setFile(const path: string; const L: TStrings);
  VAR i:longint;
      LL:T_stringList;
  begin
    ad_haltEvaluation;
    if path<>mainPackageProvider.getPath then begin
      mainPackageProvider.setPath(path);
      if mainPackageProvider.fileHasChanged then begin
        mainPackageProvider.load;
        L.Clear;
        LL:=mainPackageProvider.getLines;
        for i:=0 to length(LL)-1 do L.Append(LL[i]);
        setLength(LL,0);
      end;
    end;
  end;

function ad_currentFile: string;
  begin
    result:=mainPackageProvider.getPath;
  end;

function ad_evaluationRunning: Boolean;
  begin
    result:=evaluationState.value=es_running;
  end;

PROCEDURE ad_killEvaluationLoopSoftly;
  begin
    if evaluationState.value=es_running then haltEvaluation;
    repeat pendingRequest.value:=er_die; sleep(1); until evaluationState.value=es_dead;
  end;

function ad_getTokenInfo(const line: ansistring; const column: longint): T_tokenInfo;
VAR token:T_token;
    loc:T_tokenLocation;
begin
  result.tokenText:='';
  result.tokenExplanation:='';
  result.declaredInFile:='';
  result.declaredInLine:=-1;
  if evaluationState.value<>es_running then begin
    token:=getTokenAt(line,column);
    result.tokenText:=token.txt;
    result.tokenExplanation:=C_tokenInfoString[token.tokType];
    if (token.tokType=tt_userRulePointer) then begin
      loc:=P_rule(token.data)^.getLocationOfDeclaration;
      result.declaredInLine:=loc.line;
      result.declaredInFile:=loc.provider^.getPath;
      result.tokenExplanation:=result.tokenExplanation+'#@Line '+IntToStr(loc.line);
      if trim(result.declaredInFile)<>'' then
        result.tokenExplanation:=result.tokenExplanation+'#in '+result.declaredInFile;
    end else if (token.tokType=tt_identifier) then begin
      if token.txt='USE' then begin
        result.tokenExplanation:=result.tokenExplanation+'#Identifier has context sepecific interpretation'
                                                        +'#As first token in a package, it marks the use-clause (importing packages)';
      end else if token.txt='CACHE' then begin
        result.tokenExplanation:=result.tokenExplanation+'#Identifier has context specific interpretation'
                                                        +'#In conjunction with a further identifier it enables caching for a specific rule.'
      end;
    end else if (token.tokType=tt_literal) then begin
      if (token.txt='true') or (token.txt='false') then result.tokenExplanation:='boolean literal'
      else if (token.txt='Nan') then result.tokenExplanation:='numeric literal (Not-A-Number)'
      else if (token.txt='Inf') then result.tokenExplanation:='numeric literal (Infinity)'
      else if (token.txt[1] in ['"','''']) then result.tokenExplanation:='string literal'
      else if (pos('.',token.txt)>0) or (pos('E',UpperCase(token.txt))>0) then result.tokenExplanation:='real literal'
      else result.tokenExplanation:='integer literal';
    end;
  end;
end;

function ad_needReload: boolean;
  begin
    result:=mainPackageProvider.fileHasChanged;
  end;

procedure ad_doReload(const L: TStrings);
  VAR lines:T_stringList;
      i:longint;
  begin
    ad_haltEvaluation;
    L.Clear;
    mainPackageProvider.load;
    lines:=mainPackageProvider.getLines;
    for i:=0 to length(lines)-1 do L.Append(lines[i]);
  end;

PROCEDURE initIntrinsicRuleList;
  VAR ids:T_arrayOfString;
      i:longint;
  begin
    ids:=mnh_funcs.intrinsicRuleMap.keySet;
    intrinsicRules.clear;
    for i:=0 to length(ids)-1 do begin
      intrinsicRules.add(ids[i]);
      intrinsicRules.add('mnh.'+ids[i]);
    end;
    intrinsicRules.unique;
  end;


INITIALIZATION
  pendingRequest.create(er_none);
  evaluationState.create(es_dead);
  startOfEvaluation.create(now);
  endOfEvaluationText.create('');
  intrinsicRules.create;
  initIntrinsicRuleList;
  localUserRules.create;
  importedUserRules.create;
  beginThread(@main);

FINALIZATION
  ad_killEvaluationLoopSoftly;
  pendingRequest.destroy;
  evaluationState.destroy;
  startOfEvaluation.destroy;
  endOfEvaluationText.destroy;
  intrinsicRules.destroy;
  localUserRules.destroy;
  importedUserRules.destroy;

end.
