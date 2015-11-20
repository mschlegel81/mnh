UNIT mnh_debugForm;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, SynEdit, Forms, Controls,
  Menus,mnh_out_adapters,SynHighlighterMnh,mnh_evalThread,SynEditKeyCmds;

CONST
  ROLLING_LINE_COUNT=200;

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
    procedure FormDestroy(Sender: TObject);
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
  StopDebuggingCallback: PROCEDURE = nil;
  DebuggingStepCallback: PROCEDURE = nil;

IMPLEMENTATION

{$R *.lfm}

{ TDebugForm }


PROCEDURE TDebugForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    if ad_evaluationRunning then stepper.setFreeRun;
    if StopDebuggingCallback<>nil then StopDebuggingCallback;
  end;

PROCEDURE TDebugForm.FormCreate(Sender: TObject);
  begin
    highlighter:=TSynMnhSyn.create(nil,msf_debugger);
    debugEdit.highlighter:=highlighter;
  end;

procedure TDebugForm.FormDestroy(Sender: TObject);
  begin
    highlighter.destroy;
    debugEdit.Highlighter:=nil;
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
    if DebuggingStepCallback<>nil then DebuggingStepCallback;
  end;

PROCEDURE TDebugForm.miMultistepClick(Sender: TObject);
  begin
    stepper.doMultiStep(ROLLING_LINE_COUNT div 3);
    if DebuggingStepCallback<>nil then DebuggingStepCallback;
  end;

PROCEDURE TDebugForm.miStepClick(Sender: TObject);
  begin
    stepper.doStep;
    if DebuggingStepCallback<>nil then DebuggingStepCallback;
  end;

PROCEDURE TDebugForm.rollingAppend(CONST line: ansistring);
  VAR i:longint;
  begin
    if debugEdit.lines.count<ROLLING_LINE_COUNT
    then debugEdit.lines.append(line)
    else begin
      for i:=0 to ROLLING_LINE_COUNT-2 do debugEdit.lines[i]:=debugEdit.lines[i+1];
      debugEdit.lines[ROLLING_LINE_COUNT-1]:=line;
    end;
    debugEdit.ExecuteCommand(ecEditorBottom,' ',nil);
    debugEdit.ExecuteCommand(ecLineStart,' ',nil);

  end;

end.

