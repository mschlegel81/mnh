UNIT outlineFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ideLayoutUtil,
  litVar,
  operators,
  mnh_settings,
  mnh_constants,basicTypes,
  subrules,rules,packages;

TYPE
  T_openLocationCallback=FUNCTION (CONST location:T_searchTokenLocation):boolean of object;
  P_outlineNode=^T_outlineNode;

  { TOutlineForm }

  TOutlineForm = class(T_mnhComponentForm)
    outlineGlyphs: TImageList;
    showPrivateCheckbox: TMenuItem;
    showImportedCheckbox: TMenuItem;
    MenuItem3: TMenuItem;
    sortByNameRadio: TMenuItem;
    sortByNameCaseRadio: TMenuItem;
    sortByLocationRadio: TMenuItem;
    OutlinePopupMenu: TPopupMenu;
    outlineTreeView: TTreeView;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE treeViewKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE checkboxClick(Sender: TObject);
    PROCEDURE treeViewDblClick(Sender: TObject);
  private
    cs:TRTLCriticalSection;
    currentMainPackage:P_package;

    packageNodes:array of P_outlineNode;
    PROCEDURE openSelectedLocation;
    PROCEDURE refresh;
    FUNCTION ruleSorting: T_ruleSorting;
    PROCEDURE update(CONST mainPackage: P_package);

  public
    FUNCTION getIdeComponentType:T_ideComponent; virtual;
  end;

  T_outlineNode=object
    private
      containingModel:TOutlineForm;
      associatedNode:TTreeNode;

      isLocal :boolean;
      isPublic:boolean;
      location:T_searchTokenLocation;
      children:array of P_outlineNode;

      PROCEDURE updateWithRule(CONST rule:P_rule; CONST inMainPackage:boolean);
      PROCEDURE updateWithSubrule(CONST subRule:P_subruleExpression; CONST ruleId:string; CONST inMainPackage:boolean);
      PROCEDURE updateWithPackage(CONST package:P_package; CONST mainPackage:boolean);
      CONSTRUCTOR createBlank(CONST inModel:TOutlineForm; CONST node:TTreeNode);
      DESTRUCTOR destroy;
      PROCEDURE refresh;
  end;

VAR openLocation:T_openLocationCallback;
IMPLEMENTATION
VAR
  OutlineForm: TOutlineForm;

{$R *.lfm}

CONST outlineIconIndex:array[T_ruleType] of longint=(-1,
              {rt_memoized}        0,
              {rt_mutable}         1,
              {rt_datastore}       2,
              {rt_synchronized}    3,
              {rt_customTypeCheck} 4,
              {rt_duckTypeCheck}   4,
              {rt_customTypeCast}  5,
              {rt_customOperator}  6);

PROCEDURE T_outlineNode.updateWithSubrule(CONST subRule: P_subruleExpression; CONST ruleId: string; CONST inMainPackage:boolean);
  begin
    isLocal:=inMainPackage;
    isPublic:=subRule^.isPublic;
    location:=subRule^.getLocation;
    associatedNode.text:=ruleId+subRule^.patternString;
  end;

PROCEDURE T_outlineNode.updateWithRule(CONST rule: P_rule; CONST inMainPackage:boolean);
  VAR ruleId:string;
      subrules:T_subruleArray;
      subRule:P_subruleExpression;
      relevantSubruleCount:longint=0;
      childIdx:longint=0;
      keepCount:longint;
  FUNCTION opRuleName:string;
    VAR tt:T_tokenType;
    begin
      result:='';
      if rule^.getRuleType=rt_customOperator then begin
        for tt:=low(operatorName) to high(operatorName) do if
        operatorName[tt]=rule^.getId then exit(C_tokenInfo[tt].defaultId);
      end else result:=rule^.getId;
    end;

  begin
    isLocal:=inMainPackage;
    isPublic:=rule^.hasPublicSubrule;
    location:=rule^.getLocation;

    ruleId:=opRuleName;
    if rule^.getRuleType in C_mutableRuleTypes then begin
      associatedNode.text:=ruleId;
      associatedNode.ImageIndex:=outlineIconIndex[rule^.getRuleType];
      for childIdx:=0 to length(children)-1 do dispose(children[childIdx],destroy);
      setLength(children,0);
    end else begin
      associatedNode.text:=ruleId;
      associatedNode.ImageIndex:=outlineIconIndex[rule^.getRuleType];
      subrules:=P_ruleWithSubrules(rule)^.getSubrules;
      for subRule in subrules do if inMainPackage or (subRule^.isPublic) then inc(relevantSubruleCount);
      if relevantSubruleCount<=1 then begin
         for subRule in P_ruleWithSubrules(rule)^.getSubrules do if inMainPackage or (subRule^.isPublic) then begin
           isLocal:=inMainPackage;
           isPublic:=subRule^.isPublic;
           location:=subRule^.getLocation;
           associatedNode.text:=ruleId+subRule^.patternString;
         end;
      end else for subRule in P_ruleWithSubrules(rule)^.getSubrules do if inMainPackage or (subRule^.isPublic) then begin
        if childIdx>=length(children) then begin
          setLength(children,childIdx+1);
          new(children[childIdx],createBlank(containingModel,containingModel.outlineTreeView.items.addChild(associatedNode,'')));
        end;
        children[childIdx]^.updateWithSubrule(subRule,ruleId,inMainPackage);
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
    for rule in package^.declaredRules(containingModel.ruleSorting) do if rule^.hasPublicSubrule or mainPackage then begin
      if childIdx>=length(children) then begin
        setLength(children,childIdx+1);
        new(children[childIdx],createBlank(containingModel,containingModel.outlineTreeView.items.addChild(associatedNode,'')));
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

CONSTRUCTOR T_outlineNode.createBlank(CONST inModel: TOutlineForm; CONST node:TTreeNode);
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
    containingModel.outlineTreeView.items.delete(associatedNode);
  end;

PROCEDURE T_outlineNode.refresh;
  VAR child:P_outlineNode;
  begin
    associatedNode.visible:=(isPublic or isLocal) and
                            (isPublic or containingModel.showPrivateCheckbox.checked) and
                            (isLocal or containingModel.showImportedCheckbox.checked);
    if associatedNode.visible then for child in children do child^.refresh;
  end;

PROCEDURE TOutlineForm.FormCreate(Sender: TObject);
  begin
    initCriticalSection(cs);
    setLength(packageNodes,0);
  end;

PROCEDURE TOutlineForm.FormDestroy(Sender: TObject);
  VAR i:longint;
  begin
    enterCriticalSection(cs);
    for i:=0 to length(packageNodes)-1 do dispose(packageNodes[i],destroy);
    setLength(packageNodes,0);
    leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

PROCEDURE TOutlineForm.openSelectedLocation;
  begin
    enterCriticalSection(cs);
    if Assigned(outlineTreeView.Selected) and (outlineTreeView.Selected.data<>nil) then
    openLocation(P_outlineNode(outlineTreeView.Selected.data)^.location);
    leaveCriticalSection(cs);
  end;

PROCEDURE TOutlineForm.treeViewKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if key=13 then openSelectedLocation;
  end;

PROCEDURE TOutlineForm.treeViewDblClick(Sender: TObject);
  begin
    openSelectedLocation;
  end;

PROCEDURE TOutlineForm.refresh;
  begin
    enterCriticalSection(cs);
    update(currentMainPackage);
    leaveCriticalSection(cs);
  end;

PROCEDURE TOutlineForm.update(CONST mainPackage:P_package);
  VAR imported:T_packageList;
      i:longint;
  begin
    enterCriticalSection(cs);
    currentMainPackage:=mainPackage;
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
          new(packageNodes[i],createBlank(self,outlineTreeView.items.add(nil,'')));
          inc(i);
        end;
      end;
      //update nodes
      packageNodes[0]^.updateWithPackage(mainPackage,true);
      for i:=0 to length(imported)-1 do packageNodes[i+1]^.updateWithPackage(imported[i],false);
    end;
    if not(showImportedCheckbox.checked) then packageNodes[0]^.associatedNode.expand(false);
    leaveCriticalSection(cs);
  end;

PROCEDURE TOutlineForm.checkboxClick(Sender: TObject);
  begin
    settings.outline.showPrivate      :=showPrivateCheckbox .checked;
    settings.outline.showImported     :=showImportedCheckbox.checked;
    settings.outline.sortByNameCaseSen:=sortByNameCaseRadio .checked;
    settings.outline.sortByName       :=sortByNameRadio     .checked;
    settings.outline.sortByLoc        :=sortByLocationRadio .checked;
    refresh;
  end;

FUNCTION TOutlineForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icOutline;
  end;

FUNCTION TOutlineForm.ruleSorting:T_ruleSorting;
  begin
    result:=rs_none;
    if sortByNameCaseRadio.checked then exit(rs_byNameCaseSensitive);
    if sortByNameRadio    .checked then exit(rs_byNameCaseInsensitive);
    if sortByLocationRadio.checked then exit(rs_byLocation);
  end;

end.

