UNIT quickEvalForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, SynEdit, ideLayoutUtil, SynHighlighterMnh, editorMeta, editorMetaBase,
  mnh_settings,guiOutAdapters;

TYPE
//TODO: This form has it's own evaluator!
  { TQuickEvalForm }

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
    quickOutputSynEdit: TSynEdit;
    outputHighlighter:TSynMnhSyn;
    quickInputEdit: TSynEdit;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE miEchoInputClick(Sender: TObject);
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE quickInputEditChange(Sender: TObject);

  private
    inputMeta:T_basicEditorMeta;
  public

  end;

PROCEDURE ensureQuickEvalForm;
IMPLEMENTATION
PROCEDURE ensureQuickEvalForm;
  begin
    if not(hasFormOfType(icQuickEval,true)) then dockNewForm(TQuickEvalForm.create(Application));
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
    //quickA...;
    //guiOutAdapter.outputBehavior:=outputBehavior;
    //guiOutAdapter.wrapEcho:=outputBehavior.echo_wrapping;
    //TODO: updateWordWrap;
  end;

PROCEDURE TQuickEvalForm.FormCreate(Sender: TObject);
  begin
    //TODO: Use "nonbasic" editor meta ?
    inputMeta.createWithExistingEditor(quickInputEdit,nil);
    inputMeta.language:=LANG_MNH;

    registerFontControl(quickOutputSynEdit,ctEditor);
    outputHighlighter:=TSynMnhSyn.create(self,msf_output);
    quickOutputSynEdit.highlighter:=outputHighlighter;
  end;

PROCEDURE TQuickEvalForm.FormDestroy(Sender: TObject);
  begin
    inputMeta.destroy;
    unregisterFontControl(quickOutputSynEdit);
  end;

PROCEDURE TQuickEvalForm.performSlowUpdate;
  VAR meta:P_editorMeta;
  begin
    meta:=workspace.currentEditor;
    cbEvaluateInCurrentPackage.enabled:=(meta<>nil) and (meta^.language=LANG_MNH);

  end;

PROCEDURE TQuickEvalForm.performFastUpdate;
  begin

  end;

PROCEDURE TQuickEvalForm.quickInputEditChange(Sender: TObject);
  begin

  end;

end.

