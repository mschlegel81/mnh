UNIT debuggerVarForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Grids, Menus,ideLayoutUtil,debuggingVar,debugging, treeUtil;

TYPE
  TDebuggerVarForm = class(T_mnhComponentForm)
    MainMenu1: TMainMenu;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    StackGrid: TStringGrid;
    VariablesTree: TTreeView;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE StackGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE dockChanged; override;
  private
    variablesTreeViewModel:T_treeModel;
  public

  end;

VAR
  currentSnapshot       : P_debuggingSnapshot=nil;
  debuggerVarFormIsDirty: boolean=true;

PROCEDURE ensureDebuggerVarForm;
IMPLEMENTATION
USES mnh_settings,basicTypes;

PROCEDURE ensureDebuggerVarForm;
  begin
    if not(hasFormOfType(icDebuggerVariables,true)) then dockNewForm(TDebuggerVarForm.create(Application));
  end;

{$R *.lfm}
FUNCTION TDebuggerVarForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icDebuggerVariables;
  end;

PROCEDURE TDebuggerVarForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(StackGrid,ctTable);
    registerFontControl(VariablesTree,ctGeneral);
    variablesTreeViewModel.create(VariablesTree);
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
  end;

PROCEDURE TDebuggerVarForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(StackGrid);
    unregisterFontControl(VariablesTree);
    variablesTreeViewModel.destroy;
  end;

PROCEDURE TDebuggerVarForm.performSlowUpdate;
  begin

  end;

PROCEDURE TDebuggerVarForm.performFastUpdate;
  PROCEDURE cleanup;
    begin
      VariablesTree.items.clear;
      StackGrid.RowCount:=1;
    end;

  PROCEDURE updateWithCurrentSnapshot;
    VAR i:longint;
        j:longint=1;
        firstRow:TGridRect;
    begin
      StackGrid.RowCount:=1+currentSnapshot^.callStack^.size;
      for i:=currentSnapshot^.callStack^.size-1 downto 0 do begin
        StackGrid.Cells[0,j]:=currentSnapshot^.callStack^[i].callerLocation;
        StackGrid.Cells[1,j]:=currentSnapshot^.callStack^[i].calleeId;
        inc(j);
      end;
      VariablesTree.items.clear;
      firstRow.top:=1;
      firstRow.Bottom:=1;
      firstRow.Left:=0;
      firstRow.Right:=1;
      if StackGrid.RowCount>1 then StackGrid.selection:=firstRow;
      StackGridSelection(nil,0,1);
    end;

  begin
    if debuggerVarFormIsDirty and (currentSnapshot<>nil) then begin
      updateWithCurrentSnapshot;
      debuggerVarFormIsDirty:=false;
    end else if (currentSnapshot=nil) and (VariablesTree.items.count>0) then
      cleanup;
  end;

PROCEDURE TDebuggerVarForm.StackGridSelection(Sender: TObject; aCol,
  aRow: integer);
  VAR stackIdx:longint;
  PROCEDURE addCategoryNode(CONST r:P_variableTreeEntryCategoryNode; CONST expand:boolean=true);
    VAR newNode:TTreeNode;
    begin
      if (r=nil) or not(r^.canExpand) then exit;
      newNode:=VariablesTree.items.add(nil,r^.toString);
      newNode.data:=r;
      variablesTreeViewModel.addChildren(newNode);
      if expand then newNode.expand(false);
    end;

  begin
    if currentSnapshot=nil then exit;
    stackIdx:=currentSnapshot^.callStack^.size-aRow;
    VariablesTree.items.clear;
    if (stackIdx<0) or (stackIdx>=currentSnapshot^.callStack^.size) then exit;

    if currentSnapshot^.globalVariableReport<>nil
    then addCategoryNode(currentSnapshot^.globalVariableReport,false);
         addCategoryNode(currentSnapshot^.callStack^[stackIdx].parameters);
    if currentSnapshot^.localVariableReport<>nil
    then addCategoryNode(currentSnapshot^.localVariableReport);
  end;

PROCEDURE TDebuggerVarForm.dockChanged;
  begin
  end;

end.

