VAR lastStart:record
      mainCall:boolean;
      parameters:string;
    end=(mainCall:false; parameters:'');

FUNCTION canRun:boolean;
  begin
    result:=not(runEvaluator.evaluationRunningOrPending) and MnhForm.hasEditor and (MnhForm.getEditor^.language=LANG_MNH);
  end;

PROCEDURE customRun(CONST mainCall,debugging,profiling:boolean; CONST mainParameters:string='');
  VAR e:P_editorMeta;
  begin
    if not(canRun) then exit;
    guiOutAdapter.flushClear;
    MnhForm.UpdateTimeTimerTimer(nil);
    MnhForm.doConditionalPlotReset;
    resetTableForms;
    MnhForm.getEditor^.setWorkingDir;
    MnhForm.underCursor.tokenText:='';
    if debugging then begin
      for e in MnhForm.editorMeta do e^.startDebugging;
      MnhForm.updateDebugParts;
      MnhForm.breakPointHandlingPending:=true;
    end;
    MnhForm.UpdateTimeTimer.interval:=1;
    if mainCall then runEvaluator.callMain(MnhForm.getEditor,lastStart.parameters,profiling,debugging)
                else runEvaluator.evaluate(MnhForm.getEditor,                     profiling,debugging);

    lastStart.mainCall:=mainCall;
    lastStart.parameters:=mainParameters;
  end;
