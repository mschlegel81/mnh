VAR lastStart:record
      mainCall:boolean;
      parameters:string;
    end=(mainCall:false; parameters:'');

FUNCTION canRun:boolean;
  begin
    result:=not(runEvaluator.evaluationRunningOrPending) and hasEditor and (getEditor^.language=LANG_MNH);
  end;

//PROCEDURE customRun(CONST mainCall,debugging,profiling:boolean; CONST mainParameters:string='');
//  VAR e:P_editorMeta;
//  begin
//    if not(canRun) then exit;
//    guiOutAdapter.flushClear;
//    MnhForm.UpdateTimeTimerTimer(nil);
//    if settings.value^.doResetPlotOnEvaluation then begin
//      guiAdapters.plot^.setDefaults;
//      if plotFormIsInitialized then plotForm.pullPlotSettingsToGui();
//    end;
//    resetTableForms;
//    getEditor^.setWorkingDir;
//    MnhForm.underCursor.tokenText:='';
//    if debugging then begin
//      for e in editorMetaData do e^.editor.readonly:=true;
//      MnhForm.updateDebugParts;
//      MnhForm.breakPointHandlingPending:=true;
//    end;
//    MnhForm.UpdateTimeTimer.interval:=1;
//    if mainCall then runEvaluator.callMain(getEditor,lastStart.parameters,profiling,debugging)
//                else runEvaluator.evaluate(getEditor,                     profiling,debugging);
//
//    lastStart.mainCall:=mainCall;
//    lastStart.parameters:=mainParameters;
//  end;

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
