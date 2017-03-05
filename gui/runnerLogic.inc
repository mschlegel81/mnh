{$ifdef includeInterface}
PROCEDURE miHaltEvalutaionClick(Sender: TObject);
PROCEDURE MenuItem4Click(Sender: TObject);
PROCEDURE doStartEvaluation;
PROCEDURE miEvaluateNowClick(Sender: TObject);
PROCEDURE miRunCustomUtilScript(Sender: TObject);
PROCEDURE doConditionalPlotReset;
{$endif}
{$ifdef includeImplementation}
PROCEDURE TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
  end;

PROCEDURE TMnhForm.doStartEvaluation;
  VAR i:longint;
  begin
    guiOutAdapter.flushClear;
    UpdateTimeTimerTimer(self);
    doConditionalPlotReset;
    resetTableForms;
    editorMeta[inputPageControl.activePageIndex]^.setWorkingDir;
    underCursor.tokenText:='';
    if miDebug.Checked then begin
      for i:=0 to length(editorMeta)-1 do editorMeta[i]^.startDebugging;
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
      doStartEvaluation;
      lastStart.mainCall:=true;
      lastStart.parameters:=askForm.getLastAnswerReleasing(nil);
      runEvaluator.callMain((editorMeta[inputPageControl.activePageIndex]),lastStart.parameters,miProfile.Checked,miDebug.Checked);
    end else askForm.getLastAnswerReleasing(nil);
  end;

PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    if not(runEvaluator.evaluationRunningOrPending) then begin
      doStartEvaluation;
      lastStart.mainCall:=false;
      runEvaluator.evaluate((editorMeta[inputPageControl.activePageIndex]),miProfile.Checked,miDebug.Checked);
    end;
  end;

PROCEDURE TMnhForm.miRunCustomUtilScript(Sender: TObject);
  VAR i,k:longint;
  begin
    k:=inputPageControl.activePageIndex;
    if (k<0) or (k>=length(editorMeta)) then exit;
    for i:=0 to length(editorMeta)-1 do editorMeta[i]^.editor.readonly:=true;
    with editorMeta[k]^ do runEvaluator.runUtilScript(TMenuItem(Sender).Tag,k,editor.lines,languageName,pseudoName());
  end;

PROCEDURE TMnhForm.doConditionalPlotReset;
  begin
    if settings.value^.doResetPlotOnEvaluation then begin
      guiAdapters.plot^.setDefaults;
      if plotFormIsInitialized then plotForm.pullPlotSettingsToGui();
    end;
  end;
{$endif}
