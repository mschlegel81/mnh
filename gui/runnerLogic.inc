
//PROCEDURE haltEvaluation(CONST debugging:boolean);
//  begin
//    runEvaluator.haltEvaluation;
//    if debugging then begin
//      MnhForm.breakPointHandlingPending:=true;
//      MnhForm.lastReportedRunnerInfo.state:=es_dead;
//    end;
//  end;

//PROCEDURE doDebuggerAction(CONST newState:T_debuggerState);
//  begin
//    runEvaluator.context.stepper^.setState(newState);
//    MnhForm.updateDebugParts;
//    MnhForm.breakPointHandlingPending:=true;
//    MnhForm.lastReportedRunnerInfo.state:=es_dead;
//  end;
