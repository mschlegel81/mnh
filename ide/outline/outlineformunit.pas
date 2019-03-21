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
  subrules,rules,packages,editorMeta,
  serializationUtil;

TYPE
  P_outlineNode=^T_outlineNode;
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

    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private
    currentMainPackage:P_package;

    packageNodes:array of P_outlineNode;
    PROCEDURE openSelectedLocation;
    PROCEDURE refresh;
    FUNCTION ruleSorting: T_ruleSorting;

    PROCEDURE updateOutlineTree(CONST mainPackage: P_package);
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

  T_outlineSettings=object(T_serializable)
    showPrivate,
    showImported:boolean;
    ruleSorting:T_ruleSorting;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

PROCEDURE ensureOutlineForm;
VAR outlineSettings:T_outlineSettings;
IMPLEMENTATION
USES codeAssistance;
PROCEDURE ensureOutlineForm;
  begin
    if not(hasFormOfType(icOutline,true)) then dockNewForm(TOutlineForm.create(Application));
  end;

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

CONSTRUCTOR T_outlineSettings.create;
  begin
    showPrivate:=true;
    showImported:=false;
    ruleSorting:=rs_byLocation;
  end;

DESTRUCTOR T_outlineSettings.destroy;
  begin
  end;

FUNCTION T_outlineSettings.getSerialVersion: dword;
  begin
    result:=1;
  end;

FUNCTION T_outlineSettings.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    try
      showPrivate:=stream.readBoolean;
      showImported:=stream.readBoolean;
      ruleSorting:=T_ruleSorting(stream.readByte);
      result:=stream.allOkay;
    except
      showPrivate:=true;
      showImported:=false;
      ruleSorting:=rs_byLocation;
      result:=false;
    end;
  end;

PROCEDURE T_outlineSettings.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    stream.writeBoolean(showPrivate);
    stream.writeBoolean(showImported);
    stream.writeByte(byte(ruleSorting));
  end;

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
    setLength(packageNodes,0);
    showPrivateCheckbox.checked:=outlineSettings.showPrivate;
    showImportedCheckbox.checked:=outlineSettings.showImported;
    case outlineSettings.ruleSorting of
      rs_byLocation             : sortByLocationRadio.checked:=true;
      rs_byNameCaseSensitive    : sortByNameCaseRadio.checked:=true;
      rs_byNameCaseInsensitive  : sortByNameRadio.checked:=true;
    end;
  end;

PROCEDURE TOutlineForm.FormDestroy(Sender: TObject);
  VAR i:longint;
  begin
    for i:=0 to length(packageNodes)-1 do dispose(packageNodes[i],destroy);
    setLength(packageNodes,0);
  end;

PROCEDURE TOutlineForm.openSelectedLocation;
  begin
    if Assigned(outlineTreeView.Selected) and (outlineTreeView.Selected.data<>nil) then
    workspace.openLocation(P_outlineNode(outlineTreeView.Selected.data)^.location);
  end;

PROCEDURE TOutlineForm.treeViewKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if key=13 then openSelectedLocation;
  end;

PROCEDURE TOutlineForm.treeViewDblClick(Sender: TObject);
  begin
    openSelectedLocation;
  end;

PROCEDURE TOutlineForm.refresh;
  begin
    updateOutlineTree(currentMainPackage);
  end;

PROCEDURE TOutlineForm.updateOutlineTree(CONST mainPackage: P_package);
  VAR imported:T_packageList;
      i:longint;
  begin
    currentMainPackage:=mainPackage;
    if mainPackage=nil then begin
      for i:=0 to length(packageNodes)-1 do dispose(packageNodes[i],destroy);
      setLength(packageNodes,0);
      visible:=false;
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
      visible:=true;
    end;
    if not(showImportedCheckbox.checked) and (length(packageNodes)>0) then packageNodes[0]^.associatedNode.expand(false);
  end;

PROCEDURE TOutlineForm.checkboxClick(Sender: TObject);
  begin
    outlineSettings.showPrivate      :=showPrivateCheckbox .checked;
    outlineSettings.showImported     :=showImportedCheckbox.checked;
    if sortByNameCaseRadio .checked then outlineSettings.ruleSorting:=rs_byNameCaseSensitive;
    if sortByNameRadio     .checked then outlineSettings.ruleSorting:=rs_byNameCaseInsensitive;
    if sortByLocationRadio .checked then outlineSettings.ruleSorting:=rs_byLocation;
    refresh;
  end;

FUNCTION TOutlineForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icOutline;
  end;

PROCEDURE TOutlineForm.performSlowUpdate;
  begin
    if workspace.assistanceResponseForUpdate<>nil
    then updateOutlineTree(workspace.assistanceResponseForUpdate^.package);
  end;

PROCEDURE TOutlineForm.performFastUpdate;
  begin

  end;

FUNCTION TOutlineForm.ruleSorting: T_ruleSorting;
  begin
    result:=rs_none;
    if sortByNameCaseRadio.checked then exit(rs_byNameCaseSensitive);
    if sortByNameRadio    .checked then exit(rs_byNameCaseInsensitive);
    if sortByLocationRadio.checked then exit(rs_byLocation);
  end;

end.

