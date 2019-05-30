UNIT quickEvalForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, SynEdit, ideLayoutUtil, SynHighlighterMnh, editorMeta, editorMetaBase,
  mnh_settings,guiOutAdapters,evalThread,codeAssistance,basicTypes,synOutAdapter,mnh_messages;

TYPE
  TQuickEvalForm = class(T_mnhComponentForm)
    cbEvaluateInCurrentPackage: TCheckBox;
    MenuItem1: TMenuItem;
    miEchoDeclarations: TMenuItem;
    miEchoInput: TMenuItem;
    miEchoOutput: TMenuItem;
    miErrorL1: TMenuItem;
    miErrorL2: TMenuItem;
    miErrorL3: TMenuItem;
    miErrorL4: TMenuItem;
    miErrorUser: TMenuItem;
    miShowTiming: TMenuItem;
    miWrapEcho: TMenuItem;
    OutputPopupMenu: TPopupMenu;
    Splitter1: TSplitter;
    outputHighlighter:TMnhOutputSyn;
    quickInputSynEdit: TSynEdit;
    quickOutputSynEdit: TSynEdit;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE miEchoInputClick(Sender: TObject);
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE quickInputEditChange(Sender: TObject);

  private
    evaluatedFor:T_hashInt;
    inputMeta:T_quickEvalEditorMeta;
    quickEvaluation:T_quickEvaluation;
    quickOutput:T_synOutAdapter;
    FUNCTION stateHash:T_hashInt;
    PROCEDURE updateWordWrap;
  public

  end;

PROCEDURE ensureQuickEvalForm;
FUNCTION isQuickEvaluationRunning:boolean;
PROCEDURE stopQuickEvaluation;
IMPLEMENTATION
PROCEDURE ensureQuickEvalForm;
  begin
    if not(hasFormOfType(icQuickEval,true)) then dockNewForm(TQuickEvalForm.create(Application));
  end;

FUNCTION isQuickEvaluationRunning:boolean;
  VAR form:T_mnhComponentForm;
  begin
    form:=getFormOfType(icQuickEval);
    result:=(form<>nil) and TQuickEvalForm(form).quickEvaluation.isRunning;
  end;

PROCEDURE stopQuickEvaluation;
  VAR form:T_mnhComponentForm;
  begin
    form:=getFormOfType(icQuickEval);
    if (form<>nil) then TQuickEvalForm(form).quickEvaluation.haltEvaluation;
  end;

{$R *.lfm}
FUNCTION TQuickEvalForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icQuickEval;
  end;

PROCEDURE TQuickEvalForm.miEchoInputClick(Sender: TObject);
  begin
    with quickOutputBehavior do begin
      echo_declaration     :=miEchoDeclarations .checked;
      echo_input           :=miEchoInput        .checked;
      echo_output          :=miEchoOutput       .checked;
      echo_wrapping        :=miWrapEcho         .checked;
      show_timing          :=miShowTiming       .checked;
      show_all_userMessages:=miErrorUser .checked;
      if miErrorL1.checked then suppressWarningsUnderLevel:=1;
      if miErrorL2.checked then suppressWarningsUnderLevel:=2;
      if miErrorL3.checked then suppressWarningsUnderLevel:=3;
      if miErrorL4.checked then suppressWarningsUnderLevel:=4;
    end;
    quickOutput.outputBehavior:=outputBehavior;
    quickOutput.wrapEcho:=outputBehavior.echo_wrapping;
    updateWordWrap;
  end;

PROCEDURE TQuickEvalForm.FormCreate(Sender: TObject);
  begin
    inputMeta.createWithExistingEditor(quickInputSynEdit,nil);
    inputMeta.language:=LANG_MNH;
    inputMeta.editor.OnChange:=@quickInputEditChange;

    registerFontControl(quickOutputSynEdit,ctEditor);
    outputHighlighter:=TMnhOutputSyn.create(self);
    quickOutputSynEdit.highlighter:=outputHighlighter;
    quickOutput.create(self,quickOutputSynEdit,quickOutputBehavior);
    quickOutputSynEdit.OnKeyUp:=@workspace.keyUpForJumpToLocation;
    quickOutputSynEdit.OnMouseDown:=@workspace.mouseDownForJumpToLocation;
    quickEvaluation.create(@quickOutput);
    with quickOutputBehavior do begin
      miEchoDeclarations .checked:=echo_declaration     ;
      miEchoInput        .checked:=echo_input           ;
      miEchoOutput       .checked:=echo_output          ;
      miWrapEcho         .checked:=echo_wrapping        ;
      miShowTiming       .checked:=show_timing          ;
      miErrorUser        .checked:=show_all_userMessages;
      miErrorL1.checked:=suppressWarningsUnderLevel=1;
      miErrorL2.checked:=suppressWarningsUnderLevel=2;
      miErrorL3.checked:=suppressWarningsUnderLevel=3;
      miErrorL4.checked:=suppressWarningsUnderLevel=4;
    end;
    evaluatedFor:=0;
  end;

PROCEDURE TQuickEvalForm.FormDestroy(Sender: TObject);
  begin
    quickEvaluation.destroy;
    inputMeta.destroy;
    unregisterFontControl(quickOutputSynEdit);
    quickOutput.destroy;
  end;

PROCEDURE TQuickEvalForm.FormResize(Sender: TObject);
  begin
    updateWordWrap;
  end;

PROCEDURE TQuickEvalForm.performSlowUpdate;
  begin
  end;

PROCEDURE TQuickEvalForm.performFastUpdate;
  VAR meta:P_editorMeta;
      proxy:P_editorMetaProxy;
      ca:P_codeAssistanceResponse;
  begin
    meta:=workspace.currentEditor;
    cbEvaluateInCurrentPackage.enabled:=(meta<>nil) and (meta^.language=LANG_MNH);
    if (meta<>nil) and (cbEvaluateInCurrentPackage.enabled and cbEvaluateInCurrentPackage.checked)
    then begin
      ca:=meta^.getCodeAssistanceDataRereferenced;
      inputMeta.updateAssistanceResponse(ca);
      disposeCodeAssistanceResponse(ca);
    end
    else inputMeta.updateAssistanceResponse(nil);

    quickEvaluation.flushMessages;
    if evaluatedFor<>stateHash then begin
      if (meta<>nil) and cbEvaluateInCurrentPackage.enabled and cbEvaluateInCurrentPackage.checked
      then proxy:=newFixatedFileProxy(meta^.getPath)
      else proxy:=nil;
      quickEvaluation.postEvaluation(proxy,
                                      cbEvaluateInCurrentPackage.enabled and cbEvaluateInCurrentPackage.checked,
                                      inputMeta.getLines);
      evaluatedFor:=stateHash;
    end;
  end;

PROCEDURE TQuickEvalForm.quickInputEditChange(Sender: TObject);
  begin
  end;

FUNCTION TQuickEvalForm.stateHash: T_hashInt;
  VAR meta:P_editorMeta;
  begin
    meta:=workspace.currentEditor;
    result:=inputMeta.stateHash;
    if (meta<>nil) and (cbEvaluateInCurrentPackage.enabled and cbEvaluateInCurrentPackage.checked)
    then result:=result xor meta^.stateHash
    else result:=result xor 0;
  end;

PROCEDURE TQuickEvalForm.updateWordWrap;
  begin
    if quickOutput.parentMessages=nil then exit;
    if quickOutputBehavior.echo_wrapping
    then quickOutput.parentMessages^.preferredEchoLineLength:=quickOutputSynEdit.charsInWindow-6
    else quickOutput.parentMessages^.preferredEchoLineLength:=-1;
  end;

end.

