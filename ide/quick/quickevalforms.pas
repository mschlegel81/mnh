UNIT quickEvalForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SynEdit, ideLayoutUtil, SynHighlighterMnh,editorMeta,editorMetaBase,mnh_settings;

TYPE

  { TQuickEvalForm }

  TQuickEvalForm = class(T_mnhComponentForm)
    cbEvaluateInCurrentPackage: TCheckBox;
    quickInputEdit: TSynEdit;
    Splitter1: TSplitter;
    quickOutputSynEdit: TSynEdit;
    outputHighlighter:TSynMnhSyn;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
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

{ TQuickEvalForm }

FUNCTION TQuickEvalForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icQuickEval;
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

