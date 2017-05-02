UNIT runnerModel;
INTERFACE
USES mnh_evalThread;
VAR debugMode:boolean;
FUNCTION areEditorsLocked:boolean;

IMPLEMENTATION
function areEditorsLocked: boolean;
  begin
    result:=(debugMode and runEvaluator.evaluationRunning) or (runEvaluator.getRunnerStateInfo.state=es_editRunning);
  end;

end.
