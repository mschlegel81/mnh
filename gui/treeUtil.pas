UNIT treeUtil;
INTERFACE
USES ComCtrls;
TYPE
  P_treeEntry=^I_treeEntry;
  T_treeEntries=array of P_treeEntry;
  I_treeEntry=object
    FUNCTION canExpand:boolean; virtual; abstract;
    FUNCTION toString:string; virtual; abstract;
    FUNCTION getChildren:T_treeEntries; virtual; abstract;
    DESTRUCTOR destroy; virtual; abstract;
  end;

  { T_treeModel }

  T_treeModel=object
    private
      view:TTreeView;
    public
      CONSTRUCTOR create(CONST tree:TTreeView);
      DESTRUCTOR destroy;
      PROCEDURE addChildren(CONST node:TTreeNode);
      PROCEDURE variablesTreeViewExpanding(Sender: TObject; node: TTreeNode; VAR AllowExpansion: boolean);
  end;

IMPLEMENTATION
CONSTRUCTOR T_treeModel.create(CONST tree: TTreeView);
  begin
    view:=tree;
    view.OnExpanding:=@variablesTreeViewExpanding;
  end;

DESTRUCTOR T_treeModel.destroy;
  begin
  end;

PROCEDURE T_treeModel.addChildren(CONST node: TTreeNode);
  VAR rootValue:P_treeEntry;
      children:T_treeEntries;
      child:P_treeEntry;
  begin
    rootValue:=P_treeEntry(node.data);
    children:=rootValue^.getChildren;
    for child in children do view.items.AddChild(node,child^.toString).data:=child;
  end;

PROCEDURE T_treeModel.variablesTreeViewExpanding(Sender: TObject; node: TTreeNode; VAR AllowExpansion: boolean);
  VAR i:longint;
  begin
    AllowExpansion:=P_treeEntry(node.data)^.canExpand;
    if not(AllowExpansion) then exit;
    for i:=0 to node.count-1 do
    if not(node.items[i].HasChildren) and (P_treeEntry(node.items[i].data)^.canExpand) then addChildren(node.items[i]);
  end;

end.
