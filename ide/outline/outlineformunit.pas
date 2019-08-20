UNIT outlineFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ideLayoutUtil,
  litVar,
  mnh_settings,
  mnh_constants,basicTypes,
  subrules,rules,packages,editorMeta,
  codeAssistance,
  serializationUtil;

TYPE
  P_outlineNode=^T_outlineNode;
  TOutlineForm = class(T_mnhComponentForm)
    MainMenu1: TMainMenu;
    miDockMain: TMenuItem;
    outlinePopupMenuRoot: TMenuItem;
    outlineMainMenuRoot: TMenuItem;
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
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
  private
    caResponse:P_codeAssistanceResponse;

    packageNodes:array of P_outlineNode;
    PROCEDURE openSelectedLocation;
    FUNCTION ruleSorting: T_ruleSorting;

    PROCEDURE updateOutlineTree;
  end;

  T_outlineNode=object
    private
      containingModel:TOutlineForm;
      associatedNode:TTreeNode;

      isLocal :boolean;
      isPublic:boolean;
      location:T_searchTokenLocation;
      children:array of P_outlineNode;

      PROCEDURE updateWithEntry(CONST rule:T_ruleMapEntry; CONST inMainPackage:boolean);
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
USES tokenArray;
PROCEDURE ensureOutlineForm;
  begin
    if not(hasFormOfType(icOutline,true)) then dockNewForm(TOutlineForm.create(Application));
  end;

{$R *.lfm}

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
    {$ifdef debugMode}
    writeln('Loading T_outlineSettings @',stream.streamPos);
    {$endif}
    try
      showPrivate:=stream.readBoolean;
      showImported:=stream.readBoolean;
      ruleSorting:=T_ruleSorting(stream.readByte);
      result:=stream.allOkay;
    except
      result:=false;
    end;
    if not(result) then begin
      showPrivate:=true;
      showImported:=false;
      ruleSorting:=rs_byLocation;
    end;
  end;

PROCEDURE T_outlineSettings.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    {$ifdef debugMode}
    writeln('Saving T_outlineSettings @',stream.streamPos);
    {$endif}
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

PROCEDURE T_outlineNode.updateWithEntry(CONST rule: T_ruleMapEntry; CONST inMainPackage:boolean);
  VAR subrules:T_subruleArray;
      subRule:P_subruleExpression;
      relevantSubruleCount:longint=0;
      childIdx:longint=0;
      keepCount:longint;

  FUNCTION canHaveChildren:boolean;
    begin
      result:=(rule.entryType=tt_userRule);
    end;

  FUNCTION getRuleId:string;
    VAR local:P_rule;
        tt:T_tokenType;
    begin
      result:=rule.value^.getId;
      if (rule.entryType=tt_userRule) then begin
        local:=P_rule(rule.value);
        if (local^.getRuleType=rt_delegate) then local:=P_delegatorRule(local)^.getLocalRule;
        if local=nil then exit(result);
        if local^.getRuleType=rt_customOperator
        then begin
          for tt:=low(operatorName) to high(operatorName) do if
          operatorName[tt]=local^.getId then exit(C_tokenDefaultId[tt]);
        end;
      end;
    end;

  FUNCTION outlineIconIndex(CONST rule:T_ruleMapEntry):longint;
    //ICONS:
    //  memoized         0
    //  mutable          1
    //  datastore        2
    //  synchronized     3
    //  customTypeCheck  4
    //  duckTypeCheck    4
    //  customTypeCast   5
    //  customOperator   6
    VAR localRule:P_rule;
    begin
      case rule.entryType of
        tt_customType    : result:=4;
        tt_globalVariable: if P_variable(rule.value)^.getVariableType=vt_mutable
                           then result:=1
                           else result:=2;
        tt_userRule: begin
          localRule:=P_rule(rule.value);
          if localRule^.getRuleType=rt_delegate then localRule:=P_delegatorRule(localRule)^.getLocalRule;
          if localRule=nil then result:=-1
          else case localRule^.getRuleType of
            rt_memoized      : result:= 0;
            rt_synchronized  : result:= 3;
            rt_customTypeCheck,
            rt_duckTypeCheck : result:= 4;
            rt_customTypeCast: result:= 5;
            rt_customOperator: result:= 6;
            else               result:=-1;
          end;
        end
        else result:=-1;
      end;
    end;

  FUNCTION extractSubrules:T_subruleArray;
    VAR local:P_rule;
    begin
      setLength(result,0);
      if (rule.entryType=tt_userRule) then begin
        local:=P_rule(rule.value);
        if (local^.getRuleType=rt_delegate) then local:=P_delegatorRule(local)^.getLocalRule;
        if local<>nil then result:=P_ruleWithSubrules(local)^.getSubrules;
      end;
    end;

  begin
    isLocal:=inMainPackage;
    isPublic:=rule.hasPublicSubrule;
    location:=rule.value^.getLocation;

    associatedNode.text:=getRuleId;
    associatedNode.ImageIndex:=outlineIconIndex(rule);
    if not(canHaveChildren) then begin
      for childIdx:=0 to length(children)-1 do dispose(children[childIdx],destroy);
      setLength(children,0);
    end else begin
      subrules:=extractSubrules;
      for subRule in subrules do if inMainPackage or (subRule^.isPublic) then inc(relevantSubruleCount);
      if relevantSubruleCount<=1 then begin
         for subRule in subrules do if inMainPackage or (subRule^.isPublic) then begin
           isLocal:=inMainPackage;
           isPublic:=subRule^.isPublic;
           location:=subRule^.getLocation;
           associatedNode.text:=getRuleId+subRule^.patternString;
         end;
      end else for subRule in subrules do if inMainPackage or (subRule^.isPublic) then begin
        if childIdx>=length(children) then begin
          setLength(children,childIdx+1);
          new(children[childIdx],createBlank(containingModel,containingModel.outlineTreeView.items.addChild(associatedNode,'')));
        end;
        children[childIdx]^.updateWithSubrule(subRule,getRuleId,inMainPackage);
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
  VAR entry:T_ruleMapEntry;
      childIdx:longint=0;
      keepCount:longint;
  begin
    associatedNode.text:=package^.getId;
    isLocal:=mainPackage;
    isPublic:=true;
    location:=packageTokenLocation(package);
    for entry in package^.declaredRules(containingModel.ruleSorting) do if (entry.hasPublicSubrule or mainPackage) and
    not(entry.isImportedOrDelegateWithoutLocal) then begin
      if childIdx>=length(children) then begin
        setLength(children,childIdx+1);
        new(children[childIdx],createBlank(containingModel,containingModel.outlineTreeView.items.addChild(associatedNode,'')));
      end;
      children[childIdx]^.updateWithEntry(entry,mainPackage);
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
    registerFontControl(outlineTreeView,ctGeneral);
    initDockMenuItems(MainMenu1,miDockMain);
    initDockMenuItems(OutlinePopupMenu,outlinePopupMenuRoot);
    caResponse:=nil;
  end;

PROCEDURE TOutlineForm.FormDestroy(Sender: TObject);
  VAR i:longint;
  begin
    for i:=0 to length(packageNodes)-1 do dispose(packageNodes[i],destroy);
    setLength(packageNodes,0);
    if caResponse<>nil then disposeCodeAssistanceResponse(caResponse);
    unregisterFontControl(outlineTreeView);
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

PROCEDURE TOutlineForm.updateOutlineTree;
  VAR imported:T_packageList;
      i:longint;
  begin
    if (caResponse=nil) or (caResponse^.package=nil) then begin
      for i:=0 to length(packageNodes)-1 do dispose(packageNodes[i],destroy);
      setLength(packageNodes,0);
      visible:=false;
    end else begin
      imported:=caResponse^.package^.usedPackages;
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
      packageNodes[0]^.updateWithPackage(caResponse^.package,true);
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
    updateOutlineTree;
  end;

FUNCTION TOutlineForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icOutline;
  end;

PROCEDURE TOutlineForm.performSlowUpdate(CONST isEvaluationRunning:boolean);
  begin
  end;

PROCEDURE TOutlineForm.performFastUpdate;
  VAR codeAssistanceResponse:P_codeAssistanceResponse;
  begin
    codeAssistanceResponse:=workspace.getCurrentAssistanceResponse;
    if (codeAssistanceResponse<>caResponse) then begin
      if caResponse<>nil then disposeCodeAssistanceResponse(caResponse);
      caResponse:=codeAssistanceResponse;
      updateOutlineTree;
    end else disposeCodeAssistanceResponse(codeAssistanceResponse);
  end;

PROCEDURE TOutlineForm.dockChanged;
  begin
    if (myComponentParent=cpNone)
    then moveAllItems(OutlinePopupMenu.items,outlineMainMenuRoot)
    else moveAllItems(outlineMainMenuRoot,OutlinePopupMenu.items);
  end;

FUNCTION TOutlineForm.ruleSorting: T_ruleSorting;
  begin
    result:=rs_none;
    if sortByNameCaseRadio.checked then exit(rs_byNameCaseSensitive);
    if sortByNameRadio    .checked then exit(rs_byNameCaseInsensitive);
    if sortByLocationRadio.checked then exit(rs_byLocation);
  end;

end.

