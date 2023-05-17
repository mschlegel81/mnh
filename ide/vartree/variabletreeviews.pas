UNIT variableTreeViews;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, mnh_constants, basicTypes, mnh_messages, debuggingVar, treeUtil,
  litVar, contexts, funcs, out_adapters, mnh_settings, ideLayoutUtil;

TYPE
  P_treeAdapter=^T_treeAdapter;

  { TVarTreeViewForm }

  TVarTreeViewForm = class(T_mnhComponentForm)
    MainMenu1: TMainMenu;
    PopupMenu1: TPopupMenu;
    VarTreeView: TTreeView;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);

    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
    FUNCTION getDefaultControl:TWinControl; override;
    PROCEDURE VarTreeViewEdited(Sender: TObject; node: TTreeNode; VAR S: string
      );
    PROCEDURE VarTreeViewKeyUp(Sender: TObject; VAR key: word;
      Shift: TShiftState);
  private
    rootNode:P_variableTreeEntryAnonymousValue;
    model:T_treeModel;
    adapter:P_treeAdapter;
  public
    PROCEDURE initWithLiteral(CONST L:P_literal; CONST newCaption:string; CONST adapter_:P_treeAdapter);
  end;

  P_treeDisplayRequest=^T_treeDisplayRequest;
  T_treeDisplayRequest=object(T_payloadMessage)
    private
      treeContent:P_literal;
      treeCaption:string;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create(CONST L:P_literal; CONST newCaption:string);
      DESTRUCTOR destroy; virtual;
  end;

  T_treeAdapterTemplate=specialize G_multiChildGuiOutAdapter<TVarTreeViewForm>;
  T_treeAdapter=object(T_treeAdapterTemplate)
    defaultCaption:string;
    CONSTRUCTOR create(CONST defaultCaption_:string);
    FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
  end;

IMPLEMENTATION
USES recyclers,myStringUtil;
{$R *.lfm}

FUNCTION showVariable_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): P_literal;
  VAR caption:string='';
      Post:P_treeDisplayRequest;
  begin
    if not(context^.checkSideEffects('showVariable',tokenLocation,[se_alterGuiState])) then exit(nil);
    if (gui_started=NO) then begin
      context^.messages^.logGuiNeeded;
      exit(nil);
    end;
    if (params<>nil) and
       (params^.size>0) and (params^.size<=2) then begin
      if (params^.size=2) then begin
        if params^.value[1]^.literalType=lt_string
        then caption:=P_stringLiteral(params^.value[1])^.value
        else exit(nil);
      end;
      new(Post,create(P_listLiteral(params^.value[0]),caption));
      context^.messages^.postCustomMessage(Post,true);
      if (gui_started<>NO) then result:=newVoidLiteral else result:=nil;
    end else result:=nil;
  end;

CONSTRUCTOR T_treeAdapter.create(CONST defaultCaption_: string);
  begin
    inherited create(at_treeView,[mt_startOfEvaluation,mt_displayVariableTree]);
    defaultCaption:=defaultCaption_;
  end;

FUNCTION T_treeAdapter.flushToGui(CONST forceFlush:boolean): T_messageTypeSet;
  VAR tree:TVarTreeViewForm;
      caption:string;
      i:longint;
      start:double;
  begin
    start:=now;
    result:=[];
    enterCriticalSection(adapterCs);
    try
      for i:=0 to collectedFill-1 do case collected[i]^.messageType of
        mt_displayVariableTree:
          begin
            include(result,collected[i]^.messageType);
            tree:=addChild(TVarTreeViewForm.create(nil));
            with P_treeDisplayRequest(collected[i])^ do begin
              if treeCaption=''
              then caption:=defaultCaption+' ('+intToStr(length(children))+')'
              else caption:=treeCaption;
              tree.initWithLiteral(treeContent,caption,@self);
            end;
            dockNewForm(tree);
            tree.showComponent(true);
          end;
        mt_startOfEvaluation:
          begin
            include(result,collected[i]^.messageType);
            destroyAllChildren;
          end;
      end;
      clear;
    finally
      leaveCriticalSection(adapterCs);
    end;
    if now-start>ONE_SECOND*0.1 then postIdeMessage('Flush of tree-view adapter form took a long time: '+myTimeToStr(now-start),true);
  end;

FUNCTION T_treeDisplayRequest.internalType: shortstring;
  begin
    result:='T_treeDisplayRequest';
  end;

CONSTRUCTOR T_treeDisplayRequest.create(CONST L: P_literal; CONST newCaption: string);
  begin
    inherited create(mt_displayVariableTree);
    treeCaption:=newCaption;
    treeContent:=L^.rereferenced;
  end;

DESTRUCTOR T_treeDisplayRequest.destroy;
  begin
    globalLiteralRecycler.disposeLiteral(treeContent);
    inherited destroy;
  end;

PROCEDURE TVarTreeViewForm.FormDestroy(Sender: TObject);
  begin
    if rootNode<>nil then dispose(rootNode,destroy);
    model.destroy;
    VarTreeView.items.clear;
    unregisterFontControl(VarTreeView);
    adapter^.childDestroyed(self);
  end;

FUNCTION TVarTreeViewForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icVariableView;
  end;

PROCEDURE TVarTreeViewForm.performSlowUpdate(CONST isEvaluationRunning:boolean); begin end;
PROCEDURE TVarTreeViewForm.performFastUpdate; begin end;

PROCEDURE TVarTreeViewForm.dockChanged;
  begin
  end;

FUNCTION TVarTreeViewForm.getDefaultControl: TWinControl;
  begin
    result:=VarTreeView;
  end;

PROCEDURE TVarTreeViewForm.VarTreeViewEdited(Sender: TObject; node: TTreeNode; VAR S: string);
  begin
    S:=P_treeEntry(node.data)^.toString;
  end;

PROCEDURE TVarTreeViewForm.VarTreeViewKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    tabNextKeyHandling(Sender,key,Shift);
  end;

PROCEDURE TVarTreeViewForm.FormCreate(Sender: TObject);
  begin
    model.create(VarTreeView);
    rootNode:=nil;
    registerFontControl(VarTreeView,ctGeneral);
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
  end;

PROCEDURE TVarTreeViewForm.FormClose(Sender: TObject;
  VAR CloseAction: TCloseAction);
  begin
    CloseAction:=caFree;
  end;

PROCEDURE TVarTreeViewForm.initWithLiteral(CONST L: P_literal; CONST newCaption: string; CONST adapter_: P_treeAdapter);
  VAR node:TTreeNode;
  begin
    adapter:=adapter_;
    if rootNode<>nil then begin
      dispose(rootNode,destroy);
      VarTreeView.items.clear;
    end;
    new(rootNode,create(L,false));
    node:=VarTreeView.items.add(nil,rootNode^.toString);
    node.data:=rootNode;
    caption:=newCaption;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(GUI_NAMESPACE,'showVariable',@showVariable_impl,ak_variadic_1,'showVarible(L);//Shows L in a tree view.#showVariable(L,caption:string);//Shows L in a table with given caption',[se_alterGuiState]);
end.

