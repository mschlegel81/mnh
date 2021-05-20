UNIT quickEvalForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, SynEdit, ideLayoutUtil, SynHighlighterMnh, editorMeta, editorMetaBase,
  mnh_settings,
  evalThread,codeAssistance,basicTypes,synOutAdapter,mnh_messages;

TYPE

  { TQuickEvalForm }

  TQuickEvalForm = class(T_mnhComponentForm)
    cbEvaluateInCurrentPackage: TCheckBox;
    miDockMenuInMain: TMenuItem;
    miOutputMenuInMain: TMenuItem;
    OutputMainMenu: TMainMenu;
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
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE miEchoInputClick(Sender: TObject);
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE quickInputEditChange(Sender: TObject);
    PROCEDURE dockChanged; override;
    PROCEDURE quickOutputSynEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE quickOutputSynEditMouseDown(Sender: TObject;
      button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private
    evaluatedFor:T_hashInt;
    inputMeta:T_quickEvalEditorMeta;
    quickEvaluation:T_quickEvaluation;
    quickOutput:T_eagerInitializedOutAdapter;
    PROCEDURE openLocationForLine(CONST lineIndex:longint);
    FUNCTION stateHash:T_hashInt;
  public

  end;

PROCEDURE ensureQuickEvalForm;
FUNCTION isQuickEvaluationRunning:boolean;
PROCEDURE stopQuickEvaluation;
IMPLEMENTATION
USES mnh_constants,contexts,myStringUtil;
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
    with ideSettings.quickOutputBehavior do begin
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
    quickOutput.outputBehavior:=ideSettings.quickOutputBehavior;
    quickOutput.wrapEcho:=ideSettings.quickOutputBehavior.echo_wrapping;
  end;

PROCEDURE TQuickEvalForm.FormCreate(Sender: TObject);
  begin
    caption:=getCaption;
    inputMeta.createWithExistingEditor(quickInputSynEdit,nil);
    inputMeta.language:=LANG_MNH;
    inputMeta.editor.OnChange:=@quickInputEditChange;

    registerFontControl(quickOutputSynEdit,ctEditor);
    outputHighlighter:=TMnhOutputSyn.create(self);
    inputMeta.editor.OnKeyUp:=@tabNextKeyHandling;
    quickOutputSynEdit.highlighter:=outputHighlighter;
    quickOutput.create(quickOutputSynEdit,self,ideSettings.quickOutputBehavior);
    quickEvaluation.create(@quickOutput);
    with ideSettings.quickOutputBehavior do begin
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
    initDockMenuItems(OutputMainMenu,miDockMenuInMain);
    initDockMenuItems(OutputPopupMenu,nil);
    quickOutput.wrapEcho:=miWrapEcho.checked;
  end;

PROCEDURE TQuickEvalForm.FormDestroy(Sender: TObject);
  begin
    quickEvaluation.destroy;
    inputMeta.destroy;
    unregisterFontControl(quickOutputSynEdit);
    quickOutput.destroy;
  end;

PROCEDURE TQuickEvalForm.performSlowUpdate(CONST isEvaluationRunning: boolean);
  begin
  end;

PROCEDURE TQuickEvalForm.performFastUpdate;
  VAR meta:P_editorMeta;
      proxy:P_editorMetaProxy;
      assistanceData:P_codeAssistanceResponse;
      startTime:double;
  begin
    startTime:=now;
    meta:=workspace.currentEditor;
    cbEvaluateInCurrentPackage.enabled:=(meta<>nil) and (meta^.language=LANG_MNH);
    if (meta<>nil) and (cbEvaluateInCurrentPackage.enabled and cbEvaluateInCurrentPackage.checked)
    then begin
      assistanceData:=meta^.getAssistanceResponse;
      inputMeta.updateAssistanceResponse(assistanceData);
      disposeMessage(assistanceData);
    end else inputMeta.updateAssistanceResponse(nil);

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
    if now-startTime>ONE_SECOND then postIdeMessage('Update of quick-evaluation form took a long time: '+myTimeToStr(now-startTime),true);
  end;

PROCEDURE TQuickEvalForm.quickInputEditChange(Sender: TObject);
  begin
  end;

PROCEDURE TQuickEvalForm.dockChanged;
  begin
    if myComponentParent=cpNone
    then moveAllItems(OutputPopupMenu.items,miOutputMenuInMain)
    else moveAllItems(miOutputMenuInMain,OutputPopupMenu.items);
  end;

PROCEDURE TQuickEvalForm.quickOutputSynEditKeyUp(Sender: TObject;
  VAR key: word; Shift: TShiftState);
  begin
    tabNextKeyHandling(Sender,key,Shift);
    if (key=13) and (ssCtrl in Shift) then begin
      openLocationForLine(quickOutputSynEdit.CaretY-1);
      key:=0;
    end;
  end;

PROCEDURE TQuickEvalForm.quickOutputSynEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (ssCtrl in Shift) and (button=mbLeft) then begin
      openLocationForLine(quickOutputSynEdit.PixelsToRowColumn(point(x,y)).Y-1);
    end;
  end;

PROCEDURE TQuickEvalForm.openLocationForLine(CONST lineIndex: longint);
  VAR location: T_searchTokenLocation;
  begin
    location:=quickOutput.getLocationAtLine(lineIndex);
    if location.fileName=C_QuickEvalPseudoPackageName then begin
      quickInputSynEdit.SetFocus;
      inputMeta.setCaret(location);
    end else workspace.openLocation(location);
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

end.

