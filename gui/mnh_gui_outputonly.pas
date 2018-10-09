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
  mnh_funcs, mnh_cmdLineInterpretation, mnh_contexts, mnh_settings,
  mnh_messages, mnh_out_adapters,
  mnh_debugging,
  mnh_evalThread, mnhFormHandler, mnh_plotForm, mnh_tables, askDialog, guiOutAdapters, SynHighlighterMnh, editorMetaBase,synOutAdapter;

TYPE
  ToutputOnlyForm = class(T_abstractMnhForm)
    Timer1: TTimer;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE Timer1Timer(Sender: TObject);

    PROCEDURE onEditFinished(CONST data:P_editScriptTask   ); override;
    PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); override;
    PROCEDURE onDebuggerEvent;                                override;
    PROCEDURE onEndOfEvaluation;                              override;
    PROCEDURE triggerFastPolling;                             override;
    PROCEDURE activeFileChanged(CONST newCaption:string; CONST isMnhFile:boolean; CONST isPseudoFile:boolean); override;
    private
      consoleAdapters:T_redirectionAwareConsoleOutAdapter;
      outputHighlighter: TSynMnhSyn;
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
    if askForm.displayPending then askForm.Show;
    if Timer1.interval<MAX_INTERVAL then Timer1.interval:=Timer1.interval+1;
    guiEventsAdapter.flushToGui;
    if plotFormIsInitialized and plotForm.timerTick then Timer1.interval:=plotForm.wantTimerInterval
    else if Timer1.interval>MAX_INTERVAL then Timer1.interval:=MAX_INTERVAL;
    if not(currentRunnerInfo.state in C_runningStates) and not(anyFormShowing) then close;
  end;

PROCEDURE ToutputOnlyForm.onEditFinished(CONST data:P_editScriptTask);  begin end;
PROCEDURE ToutputOnlyForm.onBreakpoint(CONST data:P_debuggingSnapshot); begin end;
PROCEDURE ToutputOnlyForm.onDebuggerEvent;                              begin end;

PROCEDURE ToutputOnlyForm.onEndOfEvaluation;
  begin
    caption:='MNH - done';
  end;

PROCEDURE ToutputOnlyForm.triggerFastPolling;
  begin
    Timer1.interval:=1;
  end;

PROCEDURE ToutputOnlyForm.activeFileChanged(CONST newCaption:string; CONST isMnhFile:boolean; CONST isPseudoFile:boolean);
  begin
  end;

PROCEDURE ToutputOnlyForm.FormCreate(Sender: TObject);
  begin
    initGuiOutAdapters(outputOnlyForm,nil);
    consoleAdapters.create(C_defaultOutputBehavior_fileMode);
    setupOutputBehaviourFromCommandLineOptions(guiAdapters,@consoleAdapters);
    if wantConsoleAdapter then guiAdapters.addOutAdapter(@consoleAdapters,false);
    reregisterRule(SYSTEM_BUILTIN_NAMESPACE,'ask', @ask_impl);
    SynHighlighterMnh.initLists;
    mnh_evalThread.initUnit(@guiAdapters);

    editorMetaBase.editorFont:=TFont.create;

    outputHighlighter:=TSynMnhSyn.create(nil,msf_output);
    editorMetaBase.editorFont.name:=settings.editorFontname;
    editorMetaBase.editorFont.size:=settings.fontSize;
    if settings.antialiasedFonts
    then editorMetaBase.editorFont.quality:=fqCleartypeNatural
    else editorMetaBase.editorFont.quality:=fqNonAntialiased;

    mnh_out_adapters.gui_started:=true;
    caption:='MNH';
    setupEditorMetaBase(outputHighlighter,nil);
  end;

PROCEDURE ToutputOnlyForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    while currentlyExporting do begin ThreadSwitch; sleep(1); end;
    if runEvaluator.evaluationRunning then runEvaluator.haltEvaluation;
  end;

PROCEDURE ToutputOnlyForm.FormDestroy(Sender: TObject);
  begin
    mnh_evalThread.earlyFinalization;
    editorMetaBase.editorFont.free;
    Timer1.enabled:=false;
    guiAdapters.removeOutAdapter(@consoleAdapters);
    outputHighlighter.destroy;
    consoleAdapters.destroy;
  end;

VAR firstShow:boolean=true;
PROCEDURE ToutputOnlyForm.FormShow(Sender: TObject);
  begin
    if firstShow then begin
      runEvaluator.reEvaluateWithGUI;
      firstShow:=false;
      Hide;
    end;
  end;

end.

