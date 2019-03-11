UNIT variableTreeViews;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  mnh_constants,basicTypes,
  mnh_messages,recyclers,
  debuggingVar,treeUtil, litVar,contexts,funcs,out_adapters;

TYPE
  TVarTreeViewForm = class(TForm)
    VarTreeView: TTreeView;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
  private
    displayPending:boolean;
    rootNode:P_variableTreeEntryAnonymousValue;
    model:T_treeModel;
    vacant:boolean;
  public
    PROCEDURE initWithLiteral(CONST L:P_literal; CONST newCaption:string);
    PROCEDURE conditionalDoShow;
    PROCEDURE clearForm;
  end;

PROCEDURE resetTreeForms(CONST doDispose:boolean=false);
PROCEDURE conditionalShowVarTrees;
IMPLEMENTATION
VAR treeForms: array of TVarTreeViewForm;
    treeFormCs:TRTLCriticalSection;

PROCEDURE resetTreeForms(CONST doDispose:boolean=false);
  VAR i:longint;
  begin
    enterCriticalSection(treeFormCs);
    for i:=0 to length(treeForms)-1 do begin
      if doDispose then FreeAndNil(treeForms[i])
                   else treeForms[i].clearForm;
    end;
    if doDispose then setLength(treeForms,0);
    leaveCriticalSection(treeFormCs);
  end;

PROCEDURE conditionalShowVarTrees;
  VAR i:longint;
  begin
    enterCriticalSection(treeFormCs);
    for i:=0 to length(treeForms)-1 do treeForms[i].conditionalDoShow;
    leaveCriticalSection(treeFormCs);
  end;

FUNCTION newTreeForm:TVarTreeViewForm;
  VAR i:longint;
  begin
    enterCriticalSection(treeFormCs);
    for i:=0 to length(treeForms)-1 do if treeForms[i].vacant then begin
      leaveCriticalSection(treeFormCs);
      exit(treeForms[i]);
    end;
    result:=TVarTreeViewForm.create(nil);
    setLength(treeForms,length(treeForms)+1);
    treeForms[length(treeForms)-1]:=result;
    leaveCriticalSection(treeFormCs);
  end;

{$R *.lfm}

FUNCTION showVariable_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler): P_literal;
  VAR caption:string='MNH variable';
  begin
    if not(context.checkSideEffects('showVariable',tokenLocation,[se_output])) then exit(nil);
    if not(gui_started) then begin
      context.messages^.logGuiNeeded;
      exit(nil);
    end;
    if (params<>nil) and
       (params^.size>0) and (params^.size<=2) then begin
      if (params^.size=2) then begin
        if params^.value[1]^.literalType=lt_string
        then caption:=P_stringLiteral(params^.value[1])^.value
        else exit(nil);
      end;
      enterCriticalSection(treeFormCs);
      newTreeForm.initWithLiteral(P_listLiteral(params^.value[0]),caption);
      context.messages^.postSingal(mt_displayVariableTree,C_nilTokenLocation);
      leaveCriticalSection(treeFormCs);
      if gui_started then result:=newVoidLiteral else result:=nil;
    end else result:=nil;
  end;

PROCEDURE TVarTreeViewForm.FormDestroy(Sender: TObject);
  begin
    if rootNode<>nil then dispose(rootNode,destroy);
    model.destroy;
    VarTreeView.items.clear;
  end;

PROCEDURE TVarTreeViewForm.clearForm;
  begin
    if rootNode<>nil then dispose(rootNode,destroy);
    rootNode:=nil;
    VarTreeView.items.clear;
    vacant:=true;
    displayPending:=false;
    close;
  end;

PROCEDURE TVarTreeViewForm.FormCreate(Sender: TObject);
  begin
    displayPending:=false;
    model.create(VarTreeView);
    rootNode:=nil;
    //if not(anyFormShowing(ft_main)) then ShowInTaskBar:=stAlways;
    vacant:=true;
  end;

PROCEDURE TVarTreeViewForm.initWithLiteral(CONST L: P_literal; CONST newCaption: string);
  VAR node:TTreeNode;
  begin
    if rootNode<>nil then begin
      dispose(rootNode,destroy);
      VarTreeView.items.clear;
    end;
    new(rootNode,create(L,false));
    node:=VarTreeView.items.add(nil,rootNode^.toString);
    node.data:=rootNode;
    model.addChildren(node);
    displayPending:=true;
    caption:=newCaption;
    vacant:=false;
  end;

PROCEDURE TVarTreeViewForm.conditionalDoShow;
  begin
    if displayPending and not(vacant) then begin
      displayPending:=false;
      Show;
    end;
  end;

INITIALIZATION
  registerRule(GUI_NAMESPACE,'showVariable',@showVariable_impl,ak_variadic_1,'showVarible(L);//Shows L in a tree view.#showVariable(L,caption:string);//Shows L in a table with given caption');
  setLength(treeForms,0);
  initialize(treeFormCs);
  initCriticalSection(treeFormCs);
FINALIZATION
  resetTreeForms(true);
  doneCriticalSection(treeFormCs);
end.

