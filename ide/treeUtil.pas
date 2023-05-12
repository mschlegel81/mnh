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
      FUNCTION variablesTreeViewHasChildren(Sender: TCustomTreeView;
        ANode: TTreeNode): boolean;
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
    view.OnHasChildren:=@variablesTreeViewHasChildren;
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
    for child in children do view.items.addChild(node,child^.toString).data:=child;
  end;

PROCEDURE T_treeModel.variablesTreeViewExpanding(Sender: TObject; node: TTreeNode; VAR AllowExpansion: boolean);
  begin
    AllowExpansion:=P_treeEntry(node.data)^.canExpand;
    if AllowExpansion and (node.GetFirstChild=nil) then addChildren(node);
  end;

FUNCTION T_treeModel.variablesTreeViewHasChildren(Sender: TCustomTreeView; ANode: TTreeNode): boolean;
  begin
    result:=P_treeEntry(anode.data)^.canExpand;
  end;

end.
