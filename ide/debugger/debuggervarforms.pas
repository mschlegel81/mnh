UNIT debuggerVarForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Grids,ideLayoutUtil,debuggingVar;

TYPE
  TDebuggerVarForm = class(T_mnhComponentForm)
    Splitter1: TSplitter;
    StackGrid: TStringGrid;
    VariablesTree: TTreeView;
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private

  public

  end;

VAR
  globalVariableReport: P_variableTreeEntryCategoryNode=nil;
  localVariableReport : P_variableTreeEntryCategoryNode=nil;
  inlineVariableReport: P_variableTreeEntryCategoryNode=nil;

PROCEDURE ensureDebuggerVarForm;
IMPLEMENTATION

PROCEDURE ensureDebuggerVarForm;
  begin
    if not(hasFormOfType(icDebuggerVariables)) then dockNewForm(TDebuggerVarForm.create(Application));
  end;

{$R *.lfm}

{ TDebuggerVarForm }

FUNCTION TDebuggerVarForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icDebuggerVariables;
  end;

PROCEDURE TDebuggerVarForm.performSlowUpdate;
begin

end;

PROCEDURE TDebuggerVarForm.performFastUpdate;
begin

end;

end.

