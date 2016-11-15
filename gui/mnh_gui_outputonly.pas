UNIT mnh_gui_outputOnly;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mnh_constants, mnh_funcs, mnh_cmdLineInterpretation, mnh_contexts, mnh_settings, mnh_out_adapters,
  mnh_evalThread, mnhFormHandler, mnh_plotForm, mnh_tables, askDialog, guiOutAdapters, SynHighlighterMnh;

TYPE

  { ToutputOnlyForm }

  ToutputOnlyForm = class(T_abstractMnhForm)
    OutputEdit: TSynEdit;
    Timer1: TTimer;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE Timer1Timer(Sender: TObject);

    PROCEDURE onEndOfEvaluation; override;
    PROCEDURE onReloadRequired(CONST fileName:string); override;
  private
    outputHighlighter:TSynMnhSyn;
    { private declarations }
  public
    { public declarations }
  end;

VAR
  outputOnlyForm: ToutputOnlyForm;

IMPLEMENTATION

{$R *.lfm}

{ ToutputOnlyForm }

PROCEDURE ToutputOnlyForm.Timer1Timer(Sender: TObject);
  CONST MIN_INTERVALL=1;
        MAX_INTERVALL=1000;

  VAR flushPerformed:boolean=false;
      currentRunnerInfo:T_runnerStateInfo;

  begin
    currentRunnerInfo:=runEvaluator.getRunnerStateInfo;
    flushPerformed:=guiOutAdapter.flushToGui(OutputEdit);
    if guiAdapters.hasMessageOfType[mt_plotCreatedWithDeferredDisplay] and not(currentRunnerInfo.state in C_runningStates) then plotForm.doPlot();
    if askForm.displayPending then begin
      askForm.Show;
      flushPerformed:=true;
    end;
    if flushPerformed then Timer1.interval:=MIN_INTERVALL else begin
      Timer1.interval:=Timer1.interval+1;
      if Timer1.interval>MAX_INTERVALL then Timer1.interval:=MAX_INTERVALL;
    end;
    if not(currentRunnerInfo.state in C_runningStates) and not(guiAdapters.hasMessageOfType[mt_el3_evalError]) and not(anyFormShowing) then close;
  end;

PROCEDURE ToutputOnlyForm.onEndOfEvaluation;
  begin
  end;

PROCEDURE ToutputOnlyForm.onReloadRequired(CONST fileName: string);
  begin
  end;

PROCEDURE ToutputOnlyForm.FormCreate(Sender: TObject);
  begin
    registerForm(self,true,true);
    initGuiOutAdapters(outputOnlyForm);
    mnh_plotForm.guiAdapters:=@guiAdapters;
    guiAdapters.addOutAdapter(@guiOutAdapter,false);
    setupOutputBehaviourFromCommandLineOptions(guiAdapters,@guiOutAdapter);
    registerRule(SYSTEM_BUILTIN_NAMESPACE,'ask', @ask_impl,'');
    SynHighlighterMnh.initLists;
    mnh_evalThread.initUnit(@guiAdapters);
    setupCallbacks;

    outputHighlighter:=TSynMnhSyn.create(nil,msf_output);
    OutputEdit.highlighter:=outputHighlighter;
    guiOutAdapter.flushClear;
    mnh_out_adapters.gui_started:=true;
    if profilingRun then runEvaluator.reEvaluateWithGUI(ct_profiling)
                    else runEvaluator.reEvaluateWithGUI(ct_normal);
    caption:='MNH - '+getFileOrCommandToInterpretFromCommandLine;
    {$ifdef debugMode}
    if wantConsoleAdapter then guiAdapters.addConsoleOutAdapter^.enableMessageType(false,[mt_clearConsole]);
    {$endif}
    Hide;
  end;

PROCEDURE ToutputOnlyForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
begin
  if runEvaluator      .evaluationRunning then runEvaluator      .haltEvaluation;
  if assistancEvaluator.evaluationRunning then assistancEvaluator.haltEvaluation;
end;

PROCEDURE ToutputOnlyForm.FormDestroy(Sender: TObject);
  begin
    Timer1.enabled:=false;
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    outputHighlighter.destroy;
  end;

end.

