UNIT mnh_gui_outputOnly;

{$mode objfpc}{$H+}

INTERFACE

USES
  //basic
  sysutils, FileUtil,
  //LCL
  Classes, SynEdit, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  //MNH
  mnh_constants,
  mnh_funcs, mnh_cmdLineInterpretation, mnh_contexts, mnh_settings, mnh_out_adapters,
  mnh_evalThread, mnhFormHandler, mnh_plotForm, mnh_tables, askDialog, guiOutAdapters, SynHighlighterMnh, editorMetaBase;

TYPE
  ToutputOnlyForm = class(T_abstractMnhForm)
    outputEdit: TSynEdit;
    Timer1: TTimer;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE OutputEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE Timer1Timer(Sender: TObject);

    PROCEDURE onEditFinished(CONST data:pointer; CONST successful:boolean); override;
    PROCEDURE onBreakpoint  (CONST data:pointer);                           override;
    PROCEDURE onDebuggerEvent;                                              override;
    PROCEDURE onEndOfEvaluation; override;
    PROCEDURE triggerFastPolling; override;
  private
    outputHighlighter:TSynMnhSyn;
  end;

VAR
  outputOnlyForm: ToutputOnlyForm;

IMPLEMENTATION
{$R *.lfm}
PROCEDURE ToutputOnlyForm.Timer1Timer(Sender: TObject);
  CONST MAX_INTERVAL=50;
  VAR currentRunnerInfo:T_runnerStateInfo;
  begin
    currentRunnerInfo:=runEvaluator.getRunnerStateInfo;
    guiOutAdapter.flushToGui;
    if guiAdapters.isDeferredPlotLogged and not(currentRunnerInfo.state in C_runningStates) then plotForm.doPlot();
    if askForm.displayPending then askForm.Show;
    if Timer1.interval<MAX_INTERVAL then Timer1.interval:=Timer1.interval+1;
    if plotFormIsInitialized and plotForm.timerTick then Timer1.interval:=plotForm.wantTimerInterval
    else if Timer1.interval>MAX_INTERVAL then Timer1.interval:=MAX_INTERVAL;
    if not(currentRunnerInfo.state in C_runningStates) and not(anyFormShowing) then close;
  end;

PROCEDURE ToutputOnlyForm.onEditFinished(CONST data: pointer; CONST successful: boolean);
begin
end;

PROCEDURE ToutputOnlyForm.onBreakpoint(CONST data: pointer);
begin
end;

PROCEDURE ToutputOnlyForm.onDebuggerEvent;
begin
end;

PROCEDURE ToutputOnlyForm.onEndOfEvaluation;
  begin
    caption:='MNH - '+getFileOrCommandToInterpretFromCommandLine+' - done';
  end;

PROCEDURE ToutputOnlyForm.triggerFastPolling;
  begin
    Timer1.interval:=1;
  end;

PROCEDURE ToutputOnlyForm.FormCreate(Sender: TObject);
  begin
    initGuiOutAdapters(outputOnlyForm,outputEdit,false);
    setupOutputBehaviourFromCommandLineOptions(guiAdapters,@guiOutAdapter);
    reregisterRule(SYSTEM_BUILTIN_NAMESPACE,'ask', @ask_impl);
    SynHighlighterMnh.initLists;
    mnh_evalThread.initUnit(@guiAdapters);
    setupCallbacks;

    outputHighlighter:=TSynMnhSyn.create(nil,msf_output);
    outputEdit.highlighter:=outputHighlighter;
    outputEdit.Font.name:=settings.value^.editorFontname;
    outputEdit.Font.size:=settings.value^.fontSize;
    if settings.value^.antialiasedFonts
    then outputEdit.Font.quality:=fqCleartypeNatural
    else outputEdit.Font.quality:=fqNonAntialiased;

    mnh_out_adapters.gui_started:=true;
    caption:='MNH - '+getFileOrCommandToInterpretFromCommandLine+' - evaluating';
    {$ifdef debugMode}
    if wantConsoleAdapter then guiAdapters.addConsoleOutAdapter^.enableMessageType(false,[mt_clearConsole,mt_echo_input,mt_echo_output,mt_echo_declaration,mt_echo_continued]);
    {$endif}
    setupEditorMetaBase(self,outputHighlighter,nil);
    editorMetaBase.editorFont:=outputEdit.Font;
  end;

PROCEDURE ToutputOnlyForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    if runEvaluator.evaluationRunning then runEvaluator.haltEvaluation;
  end;

PROCEDURE ToutputOnlyForm.FormDestroy(Sender: TObject);
  begin
    mnh_evalThread.earlyFinalization;
    Timer1.enabled:=false;
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    outputHighlighter.destroy;
  end;

VAR firstShow:boolean=true;
PROCEDURE ToutputOnlyForm.FormShow(Sender: TObject);
  begin
    if firstShow then begin
      runEvaluator.reEvaluateWithGUI;
      firstShow:=false;
      Hide;
    end else registerForm(self,ft_main);
  end;

PROCEDURE ToutputOnlyForm.OutputEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(self,ssShift in Shift);
    if ((key=187) or (key=107)) and (ssCtrl in Shift) then outputEdit.Font.size:=outputEdit.Font.size+1;
    if ((key=189) or (key=109)) and (ssCtrl in Shift) then outputEdit.Font.size:=outputEdit.Font.size-1;
  end;

end.

