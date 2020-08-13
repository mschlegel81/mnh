UNIT reevaluationForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  ideLayoutUtil,askDialog,evalThread,mnh_constants,out_adapters,mnh_plotForm,editorMetaBase;

TYPE
  TreevaluationForm = class(TForm)
    miStop: TMenuItem;
    TrayPopup: TPopupMenu;
    timer: TTimer;
    TrayIcon: TTrayIcon;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE miStopClick(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
    PROCEDURE TrayIconClick(Sender: TObject);
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
USES cmdLineInterpretation,mnh_settings,mySys,mnh_doc;
{$R *.lfm}
PROCEDURE TreevaluationForm.TimerTimer(Sender: TObject);
  PROCEDURE slowUpdates; inline;
    begin
      if not(slowUpdating) then begin
        slowUpdating:=true;
        performSlowUpdates(runner.isRunning);
        {$ifdef debugMode}
        writeln(runner.stateString);
        {$endif}
        slowUpdating:=false;
      end;
      if not(runner.isRunning) and (not(hasAnyForm) or (clf_HEADLESS in commandLine.mnhExecutionOptions.flags)) then close;
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
    if subTimerCounter>50 then begin
      slowUpdates;
      subTimerCounter:=0;
    end;
  end;

PROCEDURE TreevaluationForm.TrayIconClick(Sender: TObject);
  begin
    TrayIcon.ShowBalloonHint;
  end;

PROCEDURE TreevaluationForm.FormCreate(Sender: TObject);
  begin
    {$ifdef debugMode}
    writeln('Reevaluting with GUI');
    {$endif}
    fastUpdating:=false;
    slowUpdating:=false;
    memoryCleaner.registerCleanupMethod(@finalizeFunctionDocMap);

    initializePlotForm(nil);
    setupEditorMetaBase(nil);
    gui_started:=REEVALUATION;
    subTimerCounter:=0;
    ideSettings.loadFromFile(ideSettingsFilename);
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
    //TODO: make tray menu extensible by script
    TrayIcon.visible:=true;
    TrayIcon.Hint:=commandLine.fileOrCommandToInterpret;
    TrayIcon.BalloonTitle:='mnh5';
    TrayIcon.BalloonHint:='Running: '+commandLine.fileOrCommandToInterpret;
  end;

PROCEDURE TreevaluationForm.miStopClick(Sender: TObject);
  CONST oneSecond=1/(24*60*60);
  VAR timeout:double;
  begin
    timer.enabled:=false;
    commandLine.mnhExecutionOptions.flags:=commandLine.mnhExecutionOptions.flags-[clf_PAUSE_ALWAYS,clf_PAUSE_ON_ERR];
    runner.postHalt;
    timeout:=now+oneSecond;
    while runner.isRunning and (now<timeout) do sleep(1);
    if runner.isRunning then halt(14) //We tried to play nice, now we force stop
                        else close;
  end;

end.

