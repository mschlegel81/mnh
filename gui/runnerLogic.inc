{$ifdef includeInterface}
PROCEDURE miHaltEvalutaionClick(Sender: TObject);
PROCEDURE MenuItem4Click(Sender: TObject);
PROCEDURE doStartEvaluation(CONST reEvaluating:boolean);
PROCEDURE miEvaluateNowClick(Sender: TObject);
PROCEDURE miRunCustomEditScript(Sender: TObject);
PROCEDURE miRunCustomUtilScript(Sender: TObject);
FUNCTION preferredContextType:T_contextType;
PROCEDURE doConditionalPlotReset;
{$endif}
{$ifdef includeImplementation}
PROCEDURE TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
  end;

PROCEDURE TMnhForm.doStartEvaluation(CONST reEvaluating: boolean);
  VAR i:longint;
  begin
    guiOutAdapter.flushClear;
    UpdateTimeTimerTimer(self);
    doConditionalPlotReset;
    resetTableForms;
    if not(reEvaluating) then editorMeta[inputPageControl.activePageIndex].setWorkingDir;
    underCursor.tokenText:='';
    if miDebug.Checked or reEvaluating and profilingRun then begin
      if reEvaluating and profilingRun then runEvaluator.context.clearBreakpoints;
      for i:=0 to length(editorMeta)-1 do editorMeta[i].startDebugging;
      updateDebugParts;
    end;
    breakPointHandlingPending:=true;
    UpdateTimeTimer.interval:=1;
  end;

PROCEDURE TMnhForm.MenuItem4Click(Sender: TObject);
  begin
    if inputPageControl.activePageIndex<0 then exit;
    askForm.initWithQuestion('Please give command line parameters');
    if askForm.ShowModal=mrOk then begin
      doStartEvaluation(false);
      lastStart.mainCall:=true;
      lastStart.parameters:=askForm.getLastAnswerReleasing(nil);
      with editorMeta[inputPageControl.activePageIndex] do runEvaluator.callMain(pseudoName,editor.lines,lastStart.parameters,preferredContextType);
    end else askForm.getLastAnswerReleasing(nil);
  end;

PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    if not(runEvaluator.evaluationRunningOrPending) then begin
      doStartEvaluation(false);
      lastStart.mainCall:=false;
      with editorMeta[inputPageControl.activePageIndex] do runEvaluator.evaluate(pseudoName,editor.lines,preferredContextType);
    end;
  end;

PROCEDURE TMnhForm.miRunCustomEditScript(Sender: TObject);
  VAR i,k:longint;
  begin
    k:=inputPageControl.activePageIndex;
    if (k<0) or (k>=length(editorMeta)) then exit;
    for i:=0 to length(editorMeta)-1 do editorMeta[i].editor.readonly:=true;
    runEvaluator.runEditScript(TMenuItem(Sender).Tag,k,editorMeta[k].editor.lines,editorMeta[k].languageName);
  end;

PROCEDURE TMnhForm.miRunCustomUtilScript(Sender: TObject);
  VAR i,k:longint;
  begin
    k:=inputPageControl.activePageIndex;
    if (k<0) or (k>=length(editorMeta)) then exit;
    for i:=0 to length(editorMeta)-1 do editorMeta[i].editor.readonly:=true;
    runEvaluator.runUtilScript(TMenuItem(Sender).Tag,editorMeta[k].pseudoName());
  end;

FUNCTION TMnhForm.preferredContextType:T_contextType;
  begin
    if miDebug.Checked then result:=ct_debugging
    else if reEvaluationWithGUIrequired and profilingRun or miProfile.Checked
    then result:=ct_profiling
    else result:=ct_normal;
  end;

PROCEDURE TMnhForm.doConditionalPlotReset;
  begin
    if settings.value^.doResetPlotOnEvaluation then begin
      guiAdapters.plot.setDefaults;
      if plotFormIsInitialized then plotForm.pullPlotSettingsToGui();
    end;
  end;
{$endif}
