UNIT mnh_evalThread;
INTERFACE
USES sysutils,myGenerics,mnh_tokens,mnh_out_adapters,classes,mnh_fileWrappers,mnh_constants,mnh_tokloc;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_die);
  T_evaluationState=(es_dead,es_idle,es_running);

PROCEDURE ad_clearFile;
PROCEDURE ad_evaluate(CONST L:TStrings);
PROCEDURE ad_haltEvaluation;
PROCEDURE ad_setFile(CONST path:string; CONST L:TStrings);
FUNCTION ad_currentFile:string;
FUNCTION ad_evaluationRunning:Boolean;
FUNCTION ad_getIdInfo(CONST id:string):T_idInfo;
PROCEDURE ad_killEvaluationLoopSoftly;
VAR evaluationState    :specialize G_safeVar<T_evaluationState>;
    startOfEvaluation  :specialize G_safeVar<double>;
    endOfEvaluationText:specialize G_safeVar<AnsiString>;
    startOfEvaluationCallback:PROCEDURE;
IMPLEMENTATION
VAR pendingRequest   :specialize G_safeVar<T_evalRequest>;
    infoMapCs:TRTLCriticalSection;
    infoMap:specialize G_stringKeyMap<T_idInfo>;

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
        EnterCriticalsection(infoMapCS);
        infoMap.clear;
        LeaveCriticalsection(infoMapCS);
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
    while evaluationState.value=es_running do sleep(1);
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

function ad_getIdInfo(const id: string): T_idInfo;
  VAR t:T_token;
      p:P_package;
      loc:T_tokenLocation;
  begin
    EnterCriticalsection(infoMapCS);
    if not(infoMap.containsKey(id,result)) then begin
      p:=getMainPackage;
      if p<>nil then begin
        t.create;
        t.txt:=id;
        t.tokType:=tt_identifier;
        p^.resolveRuleId(t,true);
        with result do begin
          if t.tokType=tt_intrinsicRulePointer then begin
            isBuiltIn:=true;
            isUserDefined:=false;
            filename:='';
            fileLine:=-1;
          end else if t.tokType=tt_userRulePointer then begin
            isBuiltIn:=false;
            loc:=P_rule(t.data)^.getLocationOfDeclaration;
            filename:=loc.provider^.getPath;
            fileLine:=loc.line;
            isUserDefined:=true;
          end else begin
            isBuiltIn:=false;
            isUserDefined:=false;
            filename:=p^.locationOfUsedPackage(id);
            fileLine:=-1;
          end;
        end;
        infoMap.put(id,result);
        t.destroy;
      end else with result do begin
        isBuiltIn:=false;
        isUserDefined:=false;
        filename:='';
        fileLine:=-1;
      end;
    end;
    LeaveCriticalsection(infoMapCS);
  end;

PROCEDURE ad_killEvaluationLoopSoftly;
  begin
    if evaluationState.value=es_running then haltEvaluation;
    repeat pendingRequest.value:=er_die; sleep(1); until evaluationState.value=es_dead;
  end;

INITIALIZATION
  InitCriticalSection(infoMapCS);
  infoMap.create();
  pendingRequest.create(er_none);
  evaluationState.create(es_dead);
  startOfEvaluation.create(now);
  endOfEvaluationText.create('');
  beginThread(@main);

FINALIZATION
  ad_killEvaluationLoopSoftly;
  pendingRequest.destroy;
  evaluationState.destroy;
  startOfEvaluation.destroy;
  endOfEvaluationText.destroy;
  infoMap.destroy;
  DoneCriticalsection(infoMapCS);
  
end.
