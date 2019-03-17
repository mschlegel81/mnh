UNIT breakpointsForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids,ideLayoutUtil,editorMeta,basicTypes;

TYPE

  { TBreakpointsForm }

  TBreakpointsForm = class(T_mnhComponentForm)
    breakpointsGrid: TStringGrid;
    PROCEDURE breakpointsGridDblClick(Sender: TObject);
    PROCEDURE breakpointsGridKeyUp(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private

  public

  end;

PROCEDURE ensureBreakpointsForm;
IMPLEMENTATION

PROCEDURE ensureBreakpointsForm;
begin
  if not(hasFormOfType(icDebuggerBreakpoints))
  then dockNewForm(TBreakpointsForm.create(Application));
end;

{$R *.lfm}
FUNCTION TBreakpointsForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icDebuggerBreakpoints;
  end;

PROCEDURE TBreakpointsForm.breakpointsGridKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    //TODO: Implement jump
    //TODO: Implement delete
  end;

PROCEDURE TBreakpointsForm.breakpointsGridDblClick(Sender: TObject);
  begin
    //TODO: Implement jump
  end;

PROCEDURE TBreakpointsForm.performSlowUpdate;
  VAR breakpoints:T_searchTokenLocations;
      k:longint;
  begin
    breakpoints:=workspace.getAllBreakpoints;
    breakpointsGrid.RowCount:=length(breakpoints)+1;
    for k:=0 to length(breakpoints)-1 do begin
      breakpointsGrid.Cells[0,k+1]:=breakpoints[k].fileName;
      breakpointsGrid.Cells[1,k+1]:=intToStr(breakpoints[k].line);
    end;
    setLength(breakpoints,0);
  end;

PROCEDURE TBreakpointsForm.performFastUpdate;
  begin
  end;

end.

