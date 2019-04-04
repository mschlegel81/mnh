UNIT debuggerVarForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Grids,ideLayoutUtil,debuggingVar,debugging, treeUtil,mnh_settings,basicTypes;

TYPE
  TDebuggerVarForm = class(T_mnhComponentForm)
    Splitter1: TSplitter;
    StackGrid: TStringGrid;
    VariablesTree: TTreeView;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE StackGridSelection(Sender: TObject; aCol, aRow: integer);
  private
    variablesTreeViewModel:T_treeModel;
  public

  end;

VAR
  currentSnapshot       : P_debuggingSnapshot=nil;
  debuggerVarFormIsDirty: boolean=true;

PROCEDURE ensureDebuggerVarForm;
IMPLEMENTATION

PROCEDURE ensureDebuggerVarForm;
  begin
    if not(hasFormOfType(icDebuggerVariables,true)) then dockNewForm(TDebuggerVarForm.create(Application));
  end;

{$R *.lfm}

{ TDebuggerVarForm }

FUNCTION TDebuggerVarForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icDebuggerVariables;
  end;

PROCEDURE TDebuggerVarForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(StackGrid,ctTable);
    registerFontControl(VariablesTree,ctGeneral);
    variablesTreeViewModel.create(VariablesTree);
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
        StackGrid.Cells[0,j]:=currentSnapshot^.callStack^[i].callLocation;
        StackGrid.Cells[1,j]:=currentSnapshot^.callStack^[i].calleeId;
        inc(j);
      end;
      VariablesTree.items.clear;
      firstRow.top:=1;
      firstRow.Bottom:=1;
      firstRow.Left:=0;
      firstRow.Right:=1;
      if StackGrid.RowCount>1 then StackGrid.selection:=firstRow;
    end;

  begin
    if debuggerVarFormIsDirty and (currentSnapshot<>nil) then begin
      updateWithCurrentSnapshot;
      debuggerVarFormIsDirty:=false;
    end else if (currentSnapshot=nil) and (VariablesTree.items.count>0) then
      cleanup;
  end;

PROCEDURE TDebuggerVarForm.StackGridSelection(Sender: TObject; aCol, aRow: integer);
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
    stackIdx:=currentSnapshot^.callStack^.size-1-aRow;

    VariablesTree.items.clear;

    //TODO: This only has to be done once!
    if currentSnapshot^.globalVariableReport<>nil
    then addCategoryNode(currentSnapshot^.globalVariableReport,false);
         addCategoryNode(currentSnapshot^.callStack^[stackIdx].parameters);
    //TODO: This only has to be done once!
    if currentSnapshot^.localVariableReport<>nil
    then addCategoryNode(currentSnapshot^.localVariableReport);
    //updateExpressionMemo;
    //TODO: This only has to be done when the expression memo changes...
    //TODO: Is this the right place for inline variables? Corresponds more to the current expression than to the Variables view.
    //if currentSnapshot^.inlineVariableReport<>nil
    //then addCategoryNode(currentSnapshot^.inlineVariableReport);
  end;

end.

