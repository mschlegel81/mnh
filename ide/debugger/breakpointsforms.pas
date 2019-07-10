UNIT breakpointsForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ideLayoutUtil, editorMeta, basicTypes, mnh_settings;

TYPE
  TBreakpointsForm = class(T_mnhComponentForm)
    BreakpointsListBox: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    PROCEDURE BreakpointsListBoxDblClick(Sender: TObject);
    PROCEDURE BreakpointsListBoxKeyDown(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
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

PROCEDURE TBreakpointsForm.BreakpointsListBoxDblClick(Sender: TObject);
  begin
    with BreakpointsListBox do
    if (ItemIndex>=0) then workspace.openLocation(guessLocationFromString(items[ItemIndex],false));
  end;

PROCEDURE TBreakpointsForm.BreakpointsListBoxKeyDown(Sender: TObject;
  VAR key: word; Shift: TShiftState);
  begin
    if key=46 //delete
    then begin
      with BreakpointsListBox do if (ItemIndex>=0) then workspace.removeBreakpoint(guessLocationFromString(items[ItemIndex],false));
    end else if key=13
    then begin
      BreakpointsListBoxDblClick(Sender);
    end;
  end;

PROCEDURE TBreakpointsForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(BreakpointsListBox,ctGeneral);
    initDockMenuItems(MainMenu1,MenuItem1);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
  end;

PROCEDURE TBreakpointsForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(BreakpointsListBox);
  end;

PROCEDURE TBreakpointsForm.performSlowUpdate(CONST isEvaluationRunning:boolean);
  VAR oldSelected:longint=-1;
      loc:T_searchTokenLocation;
  begin
    oldSelected:=BreakpointsListBox.ItemIndex;
    BreakpointsListBox.clear;
    for loc in workspace.getAllBreakpoints do
      BreakpointsListBox.items.add(loc);
    BreakpointsListBox.ItemIndex:=oldSelected;
  end;

PROCEDURE TBreakpointsForm.performFastUpdate;
  begin
  end;

PROCEDURE TBreakpointsForm.dockChanged;
  begin
  end;

end.

