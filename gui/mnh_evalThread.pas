UNIT mnh_evalThread;
INTERFACE
USES sysutils,myGenerics,mnh_tokens,mnh_out_adapters,classes,mnh_fileWrappers,mnh_constants,mnh_tokloc;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_die);
  T_evaluationState=(es_dead,es_idle,es_running);
TYPE T_idInfo=record
       isUserDefined:boolean;
       isBuiltIn:boolean;
       filename:ansistring;
       fileLine:longint;
     end;

PROCEDURE ad_copyLinesToWrapper(CONST L:TStrings);
PROCEDURE ad_clearFile;
PROCEDURE ad_evaluate;
PROCEDURE ad_haltEvaluation;
PROCEDURE ad_setFile(CONST path:string; CONST L:TStrings);
FUNCTION ad_currentFile:string;
FUNCTION ad_evaluationRunning:Boolean;
FUNCTION ad_getIdInfo(CONST id:string):T_idInfo;

PROCEDURE ad_killEvaluationLoopSoftly;
IMPLEMENTATION
VAR pendingRequest   :specialize G_safeVar<T_evalRequest>;
    evaluationState  :specialize G_safeVar<T_evaluationState>;
    startOfEvaluation:specialize G_safeVar<double>;
VAR infoMap:specialize G_stringKeyMap<T_idInfo>;
    infoMapCS:TRTLCriticalSection;

FUNCTION main(p:pointer):ptrint;
  begin
    evaluationState.value:=es_idle;
    repeat
      if (evaluationState.value=es_idle) and (pendingRequest.value=er_evaluate) then begin
        writeln('EVALUATION TRIGGERED');
        pendingRequest.value:=er_none;
        evaluationState.value:=es_running;
        startOfEvaluation.value:=now;
        reloadMainPackage;
        //EnterCriticalsection(infoMapCS);
        //infoMap.clear;
        //LeaveCriticalsection(infoMapCS);
        evaluationState.value:=es_idle;
      end;
      sleep(10);
    until pendingRequest.value<>er_evaluate;
    evaluationState.value:=es_dead;
  end;

procedure ad_copyLinesToWrapper(const L: TStrings);
  begin
    mainPackageProvider.setLines(L);
  end;

procedure ad_clearFile;
  begin
    if evaluationState.value=es_running then haltEvaluation;
    while evaluationState.value=es_running do sleep(1);
    mainPackageProvider.clear;
  end;

procedure ad_evaluate;
  begin
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
            filename:=locateSource(id);
            fileLine:=-1;
          end;
        end;
        infoMap.put(id,result);
        t.destroy;
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
  beginThread(@main);

FINALIZATION
  ad_killEvaluationLoopSoftly;
  infoMap.destroy;
  pendingRequest.destroy;
  evaluationState.destroy;
  startOfEvaluation.destroy;
  DoneCriticalsection(infoMapCS);
  
end.
