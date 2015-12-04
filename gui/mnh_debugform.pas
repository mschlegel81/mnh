UNIT mnh_debugForm;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, SynEdit, Forms, Controls, Menus, ComCtrls, Grids,
  mnh_out_adapters, SynHighlighterMnh, mnh_evalThread, SynEditKeyCmds,myStringUtil;

CONST
  ROLLING_LINE_COUNT=200;

TYPE

  { TDebugForm }

  TDebugForm = class(TForm)
    debugEdit: TSynEdit;
    variableEdit: TSynEdit;
    MainMenu1: TMainMenu;
    miRunForBreak: TMenuItem;
    miVerboseRun: TMenuItem;
    miStep: TMenuItem;
    miMultistep: TMenuItem;
    miCancel: TMenuItem;
    highlighter,
    varHighlighter:TSynMnhSyn;
    PageControl1: TPageControl;
    BreakpointsGrid: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;

    PROCEDURE BreakpointsGridKeyUp(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE miCancelClick(Sender: TObject);
    PROCEDURE miMultistepClick(Sender: TObject);
    PROCEDURE miRunForBreakClick(Sender: TObject);
    PROCEDURE miStepClick(Sender: TObject);
    PROCEDURE miVerboseRunClick(Sender: TObject);
  private
    rollOffset:longint;
    roll:array[0..ROLLING_LINE_COUNT-1] of ansistring;
    { private declarations }
  public
    { public declarations }
    PROCEDURE updateBreakpointGrid;
    PROCEDURE rollingAppend(CONST line:ansistring);
    PROCEDURE variablesPut(CONST line:ansistring);
    PROCEDURE updateFromRoll;
    PROCEDURE clearRoll;
  end;

VAR
  DebugForm: TDebugForm;
  StopDebuggingCallback: PROCEDURE = nil;
  DebuggingStepCallback: PROCEDURE = nil;
  showMainFormCallback : PROCEDURE = nil;
IMPLEMENTATION

{$R *.lfm}

{ TDebugForm }


PROCEDURE TDebugForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    if ad_evaluationRunning then stepper.setSignal(ds_run);
    if StopDebuggingCallback<>nil then StopDebuggingCallback;
  end;

PROCEDURE TDebugForm.BreakpointsGridKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  VAR i:longint;
  begin
    FormKeyUp(Sender,key,Shift);
    if key=46 then exit;
    if BreakpointsGrid.Selection.Bottom>=BreakpointsGrid.Selection.top then
    for i:=BreakpointsGrid.Selection.top-1 downto BreakpointsGrid.Selection.Bottom-1 do stepper.removeBreakpoint(i);
    updateBreakpointGrid;
  end;

PROCEDURE TDebugForm.FormCreate(Sender: TObject);
  begin
    highlighter:=TSynMnhSyn.create(nil,msf_debugger);
    debugEdit.highlighter:=highlighter;
    varHighlighter:=TSynMnhSyn.create(nil,msf_input);
    variableEdit.highlighter:=highlighter;
  end;

PROCEDURE TDebugForm.FormDestroy(Sender: TObject);
  begin
    highlighter.destroy;
    varHighlighter.destroy;
    debugEdit.highlighter:=nil;
  end;

PROCEDURE TDebugForm.FormKeyPress(Sender: TObject; VAR key: char);
  begin
    case key of
      's','S': miStepClick(Sender);
      'm','M': miMultistepClick(Sender);
      'c','C': miCancelClick(Sender);
      'r','R': if miRunForBreak.Enabled then miRunForBreakClick(Sender);
      'v','V': if miVerboseRun.Enabled then miVerboseRunClick(Sender);
    end;
  end;

PROCEDURE TDebugForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (Shift=[ssCtrl]) and (showMainFormCallback<>nil) then showMainFormCallback();
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

PROCEDURE TDebugForm.miRunForBreakClick(Sender: TObject);
  begin
    stepper.setSignal(ds_runUntilBreak);
    if DebuggingStepCallback<>nil then DebuggingStepCallback;
  end;

PROCEDURE TDebugForm.miStepClick(Sender: TObject);
  begin
    stepper.doStep;
    if DebuggingStepCallback<>nil then DebuggingStepCallback;
  end;

PROCEDURE TDebugForm.miVerboseRunClick(Sender: TObject);
  begin
    stepper.setSignal(ds_verboseRunUntilBreak);
    if DebuggingStepCallback<>nil then DebuggingStepCallback;
  end;

PROCEDURE TDebugForm.rollingAppend(CONST line: ansistring);
  VAR i:longint;
  begin
    roll[rollOffset]:=line;
    inc(rollOffset);
    if rollOffset>=ROLLING_LINE_COUNT then rollOffset:=0;
  end;

PROCEDURE TDebugForm.clearRoll;
  VAR i:longint;
  begin
    for i:=0 to ROLLING_LINE_COUNT-1 do roll[i]:='';
    debugEdit.lines.clear;
    rollOffset:=0;
  end;

PROCEDURE TDebugForm.updateFromRoll;
  VAR i:longint;
  begin
    BeginFormUpdate;
    debugEdit.BeginUpdate(false);
    highlighter.BeginUpdate;
    debugEdit.lines.clear;
    for i:=rollOffset to rollOffset+ROLLING_LINE_COUNT-1 do if roll[i mod ROLLING_LINE_COUNT]<>'' then debugEdit.lines.append(roll[i mod ROLLING_LINE_COUNT]);
    debugEdit.ExecuteCommand(ecEditorBottom,' ',nil);
    debugEdit.ExecuteCommand(ecLineStart,' ',nil);
    highlighter.EndUpdate;
    debugEdit.EndUpdate;
    EndFormUpdate;
  end;

PROCEDURE TDebugForm.updateBreakpointGrid;
  VAR i:longint;
  begin
    BreakpointsGrid.RowCount:=1+length(stepper.breakpoints);
    for i:=0 to length(stepper.breakpoints)-1 do begin
      BreakpointsGrid.Cells[0,i+1]:=         stepper.breakpoints[i].fileName;
      BreakpointsGrid.Cells[1,i+1]:=intToStr(stepper.breakpoints[i].line);
    end;
    PageControl1.ActivePageIndex:=2;
    miVerboseRun .Enabled:=length(stepper.breakpoints)>0;
    miRunForBreak.Enabled:=length(stepper.breakpoints)>0;
    if not(showing) then Show;
  end;

PROCEDURE TDebugForm.variablesPut(CONST line:ansistring);
  begin
    if line=C_carriageReturnChar
    then variableEdit.lines.clear
    else variableEdit.lines.append(line);
  end;

end.

