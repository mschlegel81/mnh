UNIT reevaluationForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ideLayoutUtil,askDialog,evalThread,mnh_constants,out_adapters,mnh_plotForm,editorMetaBase;

TYPE

  { TreevaluationForm }

  TreevaluationForm = class(TForm)
    timer: TTimer;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    subTimerCounter:byte;
    runner:T_reevaluationWithGui;
  public

  end;

VAR
  reevaluationForm: TreevaluationForm;

IMPLEMENTATION

{$R *.lfm}

{ TreevaluationForm }

PROCEDURE TreevaluationForm.TimerTimer(Sender: TObject);
  PROCEDURE slowUpdates; inline;
    begin
      performSlowUpdates;
      {$ifdef debugMode}
      writeln(runner.stateString);
      {$endif}
      if not(runner.isRunning) and not(hasAnyForm) then close;
    end;

  PROCEDURE fastUpdates; inline;
    begin
      performFastUpdates;
      runner.flushMessages;
      if askForm.displayPending then askForm.Show;
    end;

  begin
    fastUpdates;
    inc(subTimerCounter);
    if subTimerCounter>100 then begin
      slowUpdates;
      subTimerCounter:=0;
    end;
  end;

PROCEDURE TreevaluationForm.FormCreate(Sender: TObject);
  begin
    {$ifdef debugMode}
    writeln('Reevaluting with GUI');
    {$endif}
    initializePlotForm(nil);
    setupEditorMetaBase(nil);
    gui_started:=true;
    subTimerCounter:=0;
    runner.create();
    timer.enabled:=true;
  end;

PROCEDURE TreevaluationForm.FormDestroy(Sender: TObject);
  begin
    timer.enabled:=false;
    runner.destroy;
  end;

PROCEDURE TreevaluationForm.FormShow(Sender: TObject);
  begin
    visible:=false;
  end;

end.

