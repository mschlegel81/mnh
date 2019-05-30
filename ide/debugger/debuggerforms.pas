UNIT debuggerForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, ComCtrls, ExtCtrls, SynEdit,
  ideLayoutUtil, debugging, debuggingVar, debuggerVarForms, Classes, treeUtil,
  SynHighlighterMnh;

TYPE

  { TDebuggerForm }

  TDebuggerForm = class(T_mnhComponentForm)
    DebuggerIcons: TImageList;
    DebuggerToolbar: TToolBar;
    currentExpressionEdit: TSynEdit;
    Splitter1: TSplitter;
    tbRunContinue: TToolButton;
    tbStepIn: TToolButton;
    tbStep: TToolButton;
    tbMicroStep: TToolButton;
    tbStepOut: TToolButton;
    tbHalt: TToolButton;
    inlineVariablesTreeView: TTreeView;
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE tbHaltClick(Sender: TObject);
    PROCEDURE tbMicroStepClick(Sender: TObject);
    PROCEDURE tbRunContinueClick(Sender: TObject);
    PROCEDURE tbStepClick(Sender: TObject);
    PROCEDURE tbStepInClick(Sender: TObject);
    PROCEDURE tbStepOutClick(Sender: TObject);
  private
    parameterInfo,
    inlineVariableReport:P_variableTreeEntryCategoryNode;
    treeViewModel:T_treeModel;
    currentExpressionHighlighter:TMnhDebugSyn;

    PROCEDURE addCategoryNode(CONST r: P_variableTreeEntryCategoryNode;
      CONST expand: boolean=true);
    PROCEDURE updateWithCurrentSnapshot;
    PROCEDURE delegateDebuggerAction(CONST newState:T_debuggerState);
  public

  end;

PROCEDURE ensureDebuggerForm(CONST snapshot:P_debuggingSnapshot=nil);
IMPLEMENTATION
USES editorMeta,myGenerics,mnh_settings,mnh_messages;

PROCEDURE ensureDebuggerForm(CONST snapshot:P_debuggingSnapshot);
  PROCEDURE jumpToFile;
    VAR meta:P_editorMeta;
    begin
      runnerModel.markDebugLine(nil,-1);
      if currentSnapshot^.getLocation.fileName='?' then exit;
      meta:=workspace.addOrGetEditorMetaForFiles(currentSnapshot^.getLocation.fileName,false);
      if meta<>nil then begin
        runnerModel.markDebugLine(meta^.editor,currentSnapshot^.getLocation.line);
        meta^.editor.Repaint;
      end;
    end;

  VAR form:T_mnhComponentForm;
  begin
    if not(hasFormOfType(icDebugger,true)) then dockNewForm(TDebuggerForm.create(Application));
    if snapshot<>nil then begin
      currentSnapshot:=snapshot;
      debuggerVarFormIsDirty:=true;
      jumpToFile;
      form:=getFormOfType(icDebugger);
      form.showComponent(false);
      TDebuggerForm(form).updateWithCurrentSnapshot;
    end;
  end;

{$R *.lfm}

{ TDebuggerForm }

FUNCTION TDebuggerForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icDebugger;
  end;

PROCEDURE TDebuggerForm.FormCreate(Sender: TObject);
  begin
    parameterInfo:=nil;
    new(inlineVariableReport,create(dvc_inline));
    treeViewModel.create(inlineVariablesTreeView);
    registerFontControl(currentExpressionEdit,ctEditor);
    registerFontControl(inlineVariablesTreeView,ctGeneral);
    currentExpressionHighlighter:=TMnhDebugSyn.create(currentExpressionEdit);
    currentExpressionEdit.highlighter:=currentExpressionHighlighter;
  end;

PROCEDURE TDebuggerForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    CanClose:=not(runnerModel.anyRunning(false)) or
              not(runnerModel.debugMode) or
              (currentSnapshot=nil);
  end;

PROCEDURE TDebuggerForm.FormDestroy(Sender: TObject);
  begin
    dispose(inlineVariableReport,destroy);
    treeViewModel.destroy;
    unregisterFontControl(currentExpressionEdit);
    unregisterFontControl(inlineVariablesTreeView);
  end;

PROCEDURE TDebuggerForm.performSlowUpdate;
  begin
  end;

PROCEDURE TDebuggerForm.performFastUpdate;
  VAR running:boolean;
      halted:boolean;

 PROCEDURE handleButton(VAR button:TToolButton; CONST enabled:boolean; CONST enabledImageIndex:longint; CONST enableAlways:boolean=false);
   begin
     button.enabled:=enabled or enableAlways;
     if enabled then button.ImageIndex:=enabledImageIndex
                else button.ImageIndex:=enabledImageIndex+1;
   end;

 begin
   running  :=runnerModel.anyRunning(false);
   halted   :=runnerModel.isMainEvaluationPaused and (currentSnapshot<>nil);
   handleButton(tbHalt       ,halted or running, 2);
   handleButton(tbRunContinue,halted or runnerModel.canRun, 0,true);
   handleButton(tbStep       ,halted , 4);
   handleButton(tbStepIn     ,halted , 6);
   handleButton(tbStepOut    ,halted , 8);
   handleButton(tbMicroStep  ,halted ,10);

   if not(running) and not(halted) and (inlineVariablesTreeView.items.count>0) then updateWithCurrentSnapshot;
 end;

PROCEDURE TDebuggerForm.tbHaltClick(Sender: TObject);
  begin
    runnerModel.postHalt;
    disposeMessage(currentSnapshot);
    currentSnapshot:=nil;
    updateWithCurrentSnapshot;
  end;

PROCEDURE TDebuggerForm.addCategoryNode(CONST r:P_variableTreeEntryCategoryNode; CONST expand:boolean=true);
  VAR newNode:TTreeNode;
  begin
    if (r=nil) or not(r^.canExpand) then exit;
    newNode:=inlineVariablesTreeView.items.add(nil,r^.toString);
    newNode.data:=r;
    treeViewModel.addChildren(newNode);
    if expand then newNode.expand(false);
  end;

PROCEDURE TDebuggerForm.tbRunContinueClick(Sender: TObject);
  begin
    if runnerModel.canRun then begin
      runnerModel.debugMode:=true;
      mainForm.onDebuggerEvent;
      runnerModel.rerun();
    end
    else if runnerModel.isMainEvaluationPaused and (currentSnapshot<>nil)
    then delegateDebuggerAction(runUntilBreakpoint)
    else delegateDebuggerAction(breakSoonest);
  end;

PROCEDURE TDebuggerForm.tbMicroStepClick(Sender: TObject); begin if tbMicroStep.enabled then delegateDebuggerAction(breakSoonest     ); end;
PROCEDURE TDebuggerForm.tbStepClick     (Sender: TObject); begin if tbStep     .enabled then delegateDebuggerAction(breakOnLineChange); end;
PROCEDURE TDebuggerForm.tbStepInClick   (Sender: TObject); begin if tbStepIn   .enabled then delegateDebuggerAction(breakOnStepIn    ); end;
PROCEDURE TDebuggerForm.tbStepOutClick  (Sender: TObject); begin if tbStepOut  .enabled then delegateDebuggerAction(breakOnStepOut   ); end;

PROCEDURE TDebuggerForm.updateWithCurrentSnapshot;
  VAR lines,chars:longint;
      tokens:T_arrayOfString;
      txt:ansistring;
      k:longint=0;
      firstInLine:boolean;
  begin
    currentExpressionEdit.lines.clear;
    if currentSnapshot=nil then exit;

    lines:=currentExpressionEdit.LinesInWindow;
    chars:=currentExpressionEdit.charsInWindow;
    if (lines*chars<50) then begin
      lines:=1;
      chars:=50;
    end;
    parameterInfo:=currentSnapshot^.callStack^[currentSnapshot^.callStack^.size-1].parameters;

    tokens:=currentSnapshot^.tokenStack^
            .toDebuggerString(currentSnapshot^.first,
                              round(lines*chars*0.9),
                              parameterInfo,
                              currentSnapshot^.localVariableReport,
                              currentSnapshot^.globalVariableReport,
                              inlineVariableReport);

    addCategoryNode(parameterInfo,true);
    addCategoryNode(inlineVariableReport,true);

    while k<length(tokens) do begin
      txt:='';
      firstInLine:=true;
      while (k<length(tokens)) and (firstInLine or (length(txt)+length(tokens[k])<=chars)) do begin
        txt:=txt+tokens[k];
        inc(k);
        firstInLine:=false;
      end;
      currentExpressionEdit.lines.append(txt);
    end;
    setLength(tokens,0);
  end;

PROCEDURE TDebuggerForm.delegateDebuggerAction(CONST newState: T_debuggerState);
  begin
    inlineVariablesTreeView.items.clear;
    runnerModel.doDebuggerAction(newState);
    inlineVariableReport^.clear;
    parameterInfo:=nil;
    disposeMessage(currentSnapshot);
    currentSnapshot:=nil;
    updateWithCurrentSnapshot;
  end;

end.

