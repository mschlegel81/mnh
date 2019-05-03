UNIT reevaluationForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ideLayoutUtil,askDialog,evalThread,mnh_constants,out_adapters,mnh_plotForm,editorMetaBase;

TYPE
  TreevaluationForm = class(TForm)
    timer: TTimer;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    fastUpdating,
    slowUpdating:boolean;
    subTimerCounter:byte;
    runner:T_reevaluationWithGui;
  public

  end;

VAR
  reevaluationForm: TreevaluationForm;

IMPLEMENTATION

{$R *.lfm}
PROCEDURE TreevaluationForm.TimerTimer(Sender: TObject);
  PROCEDURE slowUpdates; inline;
    begin
      if not(slowUpdating) then begin
        slowUpdating:=true;
        performSlowUpdates;
        {$ifdef debugMode}
        writeln(runner.stateString);
        {$endif}
        slowUpdating:=false;
      end;
      if not(runner.isRunning) and not(hasAnyForm) then close;
    end;

  PROCEDURE fastUpdates; inline;
    begin
      if not(fastUpdating) then begin
        fastUpdating:=true;
        performFastUpdates;
        runner.flushMessages;
        if askForm.displayPending then askForm.Show;
        fastUpdating:=false;
      end;
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
    fastUpdating:=false;
    slowUpdating:=false;

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
    while fastUpdating or slowUpdating do sleep(1);
    runner.destroy;
  end;

PROCEDURE TreevaluationForm.FormShow(Sender: TObject);
  begin
    visible:=false;
  end;

end.

