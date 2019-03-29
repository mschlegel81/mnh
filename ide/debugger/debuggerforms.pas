UNIT debuggerForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, ComCtrls, SynEdit,
  ideLayoutUtil, debugging;

TYPE

  { TDebuggerForm }

  TDebuggerForm = class(T_mnhComponentForm)
    DebuggerIcons: TImageList;
    DebuggerToolbar: TToolBar;
    SynEdit1: TSynEdit;
    tbRunContinue: TToolButton;
    tbStepIn: TToolButton;
    tbStep: TToolButton;
    tbMicroStep: TToolButton;
    tbStepOut: TToolButton;
    tbHalt: TToolButton;
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private
  public
  end;

VAR
  currentSnapshot     : P_debuggingSnapshot=nil;

PROCEDURE ensureDebuggerForm;
IMPLEMENTATION

PROCEDURE ensureDebuggerForm;
  begin
    if not(hasFormOfType(icDebugger,true)) then dockNewForm(TDebuggerForm.create(Application));
    if currentSnapshot=nil then exit;

    //TODO: Update model in debuggerVarForms

  end;

{$R *.lfm}

{ TDebuggerForm }

FUNCTION TDebuggerForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icDebugger;
  end;

PROCEDURE TDebuggerForm.performSlowUpdate;
  begin

  end;

PROCEDURE TDebuggerForm.performFastUpdate;
  begin

  end;

end.

