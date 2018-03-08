UNIT outlines;
INTERFACE
USES Classes,
     StdCtrls,
     ComCtrls,
     mnh_litVar,
     mnh_constants,mnh_basicTypes,
     mnh_subrules,mnh_rule,mnh_packages;
TYPE
  T_openLocationCallback=FUNCTION (CONST location:T_searchTokenLocation):boolean of object;

  P_outlineTreeModel=^T_outlineTreeModel;
  P_outlineNode=^T_outlineNode;
  T_outlineTreeModel=object
    private
      cs:TRTLCriticalSection;
      view:TTreeView;
      openLocation:T_openLocationCallback;

      showPrivateCheckbox,
      showImportedCheckbox:TCheckBox;
      packageNodes:array of P_outlineNode;
    public
      CONSTRUCTOR create(CONST tree:TTreeView; CONST showPrivateCB,showImportedCB:TCheckBox; CONST openLocationCallback:T_openLocationCallback);
      DESTRUCTOR destroy;
      PROCEDURE refresh;
      PROCEDURE update(CONST assistant:P_codeAssistanceData);
      PROCEDURE checkboxClick(Sender: TObject);
      PROCEDURE treeViewKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  end;

  T_outlineNode=object
    private
      containingModel:P_outlineTreeModel;
      associatedNode:TTreeNode;

      isLocal :boolean;
      isPublic:boolean;
      location:T_searchTokenLocation;
      children:array of P_outlineNode;

      PROCEDURE updateWithRule(CONST rule:P_rule; CONST inMainPackage:boolean);
      PROCEDURE updateWithSubrule(CONST subRule:P_subruleExpression; CONST ruleIdAndModifiers:string; CONST inMainPackage:boolean);
      PROCEDURE updateWithPackage(CONST package:P_package; CONST mainPackage:boolean);
      CONSTRUCTOR createBlank(CONST inModel:P_outlineTreeModel; CONST node:TTreeNode);
      DESTRUCTOR destroy;
      PROCEDURE refresh;
  end;

IMPLEMENTATION

FUNCTION privatePrefix(CONST isPrivate:boolean):string;
  begin
    if isPrivate then result:=PRIVATE_TEXT+' ' else result:='';
  end;

PROCEDURE T_outlineNode.updateWithSubrule(CONST subRule: P_subruleExpression; CONST ruleIdAndModifiers: string; CONST inMainPackage:boolean);
  begin
    isLocal:=inMainPackage;
    isPublic:=subRule^.isPublic;
    location:=subRule^.getLocation;
    associatedNode.text:=privatePrefix(not(isPublic))+ruleIdAndModifiers+subRule^.patternString;
  end;

PROCEDURE T_outlineNode.updateWithRule(CONST rule: P_rule; CONST inMainPackage:boolean);
  VAR idAndModifiers:string;
      subrules:T_subruleArray;
      subRule:P_subruleExpression;
      relevantSubruleCount:longint=0;
      childIdx:longint=0;
      keepCount:longint;
  begin
    isLocal:=inMainPackage;
    isPublic:=rule^.hasPublicSubrule;
    location:=rule^.getLocation;

    idAndModifiers:=C_ruleTypeText[rule^.getRuleType]+rule^.getId;
    if rule^.getRuleType in C_mutableRuleTypes then begin
      associatedNode.text:=privatePrefix(not(isPublic))+idAndModifiers;
      for childIdx:=0 to length(children)-1 do dispose(children[childIdx],destroy);
      setLength(children,0);
    end else begin
      associatedNode.text:=idAndModifiers;
      subrules:=P_ruleWithSubrules(rule)^.getSubrules;
      for subRule in subrules do if inMainPackage or (subRule^.isPublic) then inc(relevantSubruleCount);
      if relevantSubruleCount<=1 then begin
         for subRule in P_ruleWithSubrules(rule)^.getSubrules do if inMainPackage or (subRule^.isPublic) then begin
           isLocal:=inMainPackage;
           isPublic:=subRule^.isPublic;
           location:=subRule^.getLocation;
           associatedNode.text:=privatePrefix(not(isPublic))+idAndModifiers+subRule^.patternString;
         end;
      end else for subRule in P_ruleWithSubrules(rule)^.getSubrules do if inMainPackage or (subRule^.isPublic) then begin
        if childIdx>=length(children) then begin
          setLength(children,childIdx+1);
          new(children[childIdx],createBlank(containingModel,containingModel^.view.items.AddChild(associatedNode,'')));
        end;
        children[childIdx]^.updateWithSubrule(subRule,idAndModifiers,inMainPackage);
        inc(childIdx);
      end;
      keepCount:=childIdx;
      while childIdx<length(children) do begin
        dispose(children[childIdx],destroy);
        inc(childIdx);
      end;
      setLength(children,keepCount);
    end;
  end;

PROCEDURE T_outlineNode.updateWithPackage(CONST package: P_package; CONST mainPackage:boolean);
  VAR rule:P_rule;
      childIdx:longint=0;
      keepCount:longint;
  begin
    associatedNode.text:=package^.getId;
    isLocal:=mainPackage;
    isPublic:=true;
    location:=packageTokenLocation(package);
    for rule in package^.declaredRules do if rule^.hasPublicSubrule or mainPackage then begin
      if childIdx>=length(children) then begin
        setLength(children,childIdx+1);
        new(children[childIdx],createBlank(containingModel,containingModel^.view.items.AddChild(associatedNode,'')));
      end;
      children[childIdx]^.updateWithRule(rule,mainPackage);
      inc(childIdx);
    end;
    keepCount:=childIdx;
    while childIdx<length(children) do begin
      dispose(children[childIdx],destroy);
      inc(childIdx);
    end;
    setLength(children,keepCount);
    refresh;
  end;

CONSTRUCTOR T_outlineNode.createBlank(CONST inModel: P_outlineTreeModel; CONST node:TTreeNode);
  begin
    containingModel:=inModel;
    associatedNode:=node;
    setLength(children,0);
    node.data:=@self;
  end;

DESTRUCTOR T_outlineNode.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(children)-1 do dispose(children[i],destroy);
    setLength(children,0);
    containingModel^.view.items.delete(associatedNode);
  end;

PROCEDURE T_outlineNode.refresh;
  VAR child:P_outlineNode;
  begin
    associatedNode.visible:=(isPublic or isLocal) and
                            (isPublic or containingModel^.showPrivateCheckbox.checked) and
                            (isLocal or containingModel^.showImportedCheckbox.checked);
    if associatedNode.visible then for child in children do child^.refresh;
  end;

CONSTRUCTOR T_outlineTreeModel.create(CONST tree: TTreeView; CONST showPrivateCB, showImportedCB: TCheckBox; CONST openLocationCallback:T_openLocationCallback);
  begin
    view:=tree;
    showPrivateCheckbox :=showPrivateCB;
    showImportedCheckbox:=showImportedCB;
    showPrivateCB .OnClick:=@checkboxClick;
    showImportedCB.OnClick:=@checkboxClick;
    tree.OnKeyDown:=@treeViewKeyDown;
    openLocation:=openLocationCallback;
    initCriticalSection(cs);
    setLength(packageNodes,0);
  end;

DESTRUCTOR T_outlineTreeModel.destroy;
  VAR i:longint;
  begin
    enterCriticalSection(cs);
    for i:=0 to length(packageNodes)-1 do dispose(packageNodes[i],destroy);
    setLength(packageNodes,0);
    leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

PROCEDURE T_outlineTreeModel.refresh;
  VAR node:P_outlineNode;
  begin
    enterCriticalSection(cs);
    for node in packageNodes do node^.refresh;
    if not(showImportedCheckbox.checked) then packageNodes[0]^.associatedNode.expand(false);
    leaveCriticalSection(cs);
  end;

PROCEDURE T_outlineTreeModel.update(CONST assistant:P_codeAssistanceData);
  VAR mainPackage:P_package;
      imported:T_packageList;
      i:longint;
  begin
    enterCriticalSection(cs);
    mainPackage:=assistant^.getPackageLocking;
    if mainPackage=nil then begin
      for i:=0 to length(packageNodes)-1 do dispose(packageNodes[i],destroy);
      setLength(packageNodes,0);
    end else begin
      imported:=mainPackage^.usedPackages;
      //ensure correct node count
      if length(packageNodes)>length(imported)+1 then begin
        for i:=length(imported)+1 to length(packageNodes)-1 do dispose(packageNodes[i],destroy);
        setLength(packageNodes,length(imported)+1);
      end else if length(packageNodes)<length(imported)+1 then begin
        i:=length(packageNodes);
        setLength(packageNodes,length(imported)+1);
        while i<length(packageNodes) do begin
          new(packageNodes[i],createBlank(@self,view.items.add(nil,'')));
          inc(i);
        end;
      end;
      //update nodes
      packageNodes[0]^.updateWithPackage(mainPackage,true);
      for i:=0 to length(imported)-1 do packageNodes[i+1]^.updateWithPackage(imported[i],false);
    end;
    assistant^.releaseLock;
    refresh;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_outlineTreeModel.checkboxClick(Sender: TObject);
  begin
    refresh;
  end;

PROCEDURE T_outlineTreeModel.treeViewKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    enterCriticalSection(cs);
    if key=13 then openLocation(P_outlineNode(view.Selected.data)^.location);
    leaveCriticalSection(cs);
  end;

end.
