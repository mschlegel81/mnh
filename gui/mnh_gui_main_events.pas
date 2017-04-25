{$ifdef includeInterface}
PROCEDURE miHtmlExportClick(Sender: TObject);
PROCEDURE miUtilityScriptRootClick(Sender: TObject);
PROCEDURE tbRunClick(Sender: TObject);
PROCEDURE miHaltEvalutaionClick(Sender: TObject);
PROCEDURE MenuItem4Click(Sender: TObject);
PROCEDURE miEvaluateNowClick(Sender: TObject);
{$endif}
{$ifdef includeImplementation}
PROCEDURE TMnhForm.miHtmlExportClick(Sender: TObject);
  begin
    SaveDialog.FilterIndex:=2;
    if SaveDialog.execute then getEditor^.exportToHtml(SaveDialog.fileName);
  end;

PROCEDURE TMnhForm.miUtilityScriptRootClick(Sender: TObject);
  begin
    updateMainMenuItems(false,true);
  end;

PROCEDURE TMnhForm.tbRunClick(Sender: TObject);
  begin
    if canRun then customRun(lastStart.mainCall,true,miDebug.Checked,lastStart.parameters)
    else if runEvaluator.context.isPaused then runEvaluator.context.stepper^.doContinue
                                          else runEvaluator.context.stepper^.doMicrostep;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
  end;

PROCEDURE TMnhForm.MenuItem4Click(Sender: TObject);
  begin
    if not(canRun) then exit;
    askForm.initWithQuestion('Please give command line parameters');
    if askForm.ShowModal=mrOk
    then customRun(true,miDebug.Checked,miProfile.Checked,askForm.getLastAnswerReleasing(nil))
    else                                                  askForm.getLastAnswerReleasing(nil);
  end;

PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    customRun(false,miDebug.Checked,miProfile.Checked);
  end;
{$endif}
