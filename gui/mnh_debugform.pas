UNIT mnh_debugForm;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, SynEdit, Forms, Controls,
  ComCtrls, ButtonPanel, Menus,mnh_out_adapters,SynHighlighterMnh,mnh_evalThread;

CONST
  ROLLING_LINE_COUNT=40;

TYPE

  { TDebugForm }

  TDebugForm = class(TForm)
    MainMenu1: TMainMenu;
    miStep: TMenuItem;
    miMultistep: TMenuItem;
    miCancel: TMenuItem;
    debugEdit: TSynEdit;
    highlighter:TSynMnhSyn;

    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE miCancelClick(Sender: TObject);
    PROCEDURE miMultistepClick(Sender: TObject);
    PROCEDURE miStepClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
    PROCEDURE rollingAppend(CONST line:ansistring);
  end;

VAR
  DebugForm: TDebugForm;

IMPLEMENTATION

{$R *.lfm}

{ TDebugForm }


PROCEDURE TDebugForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    if ad_evaluationRunning then stepper.setFreeRun;
  end;

PROCEDURE TDebugForm.FormCreate(Sender: TObject);
  begin
    highlighter:=TSynMnhSyn.create(nil,false);
    debugEdit.Highlighter:=highlighter;
  end;

PROCEDURE TDebugForm.FormKeyPress(Sender: TObject; VAR key: char);
begin
  case key of
    's','S': miStepClick(Sender);
    'm','M': miMultistepClick(Sender);
    'c','C': miCancelClick(Sender);
  end;
end;

PROCEDURE TDebugForm.miCancelClick(Sender: TObject);
  begin
    ad_haltEvaluation;
  end;

PROCEDURE TDebugForm.miMultistepClick(Sender: TObject);
  begin
    stepper.doMultiStep(ROLLING_LINE_COUNT-1);
  end;

PROCEDURE TDebugForm.miStepClick(Sender: TObject);
  begin
    stepper.doStep;
  end;

PROCEDURE TDebugForm.rollingAppend(CONST line: ansistring);
  VAR i:longint;
  begin
    writeln(line);
    if debugEdit.lines.count<ROLLING_LINE_COUNT
    then debugEdit.lines.append(line)
    else begin
      for i:=0 to ROLLING_LINE_COUNT-2 do debugEdit.lines[i]:=debugEdit.lines[i+1];
      debugEdit.lines[ROLLING_LINE_COUNT-1]:=line;
    end;
  end;

end.

