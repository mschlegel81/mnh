UNIT quickEvalForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SynEdit, ideLayoutUtil, SynHighlighterMnh,editorMeta;

TYPE

  { TQuickEvalForm }

  TQuickEvalForm = class(T_mnhComponentForm)
    cbEvaluateInCurrentPackage: TCheckBox;
    quickInputEdit: TSynEdit;
    Splitter1: TSplitter;
    quickOutputSynEdit: TSynEdit;
    inputHighlighter,outputHighlighter:TSynMnhSyn;
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;

  private

  public

  end;

VAR
  QuickEvalForm: TQuickEvalForm;

IMPLEMENTATION

{$R *.lfm}

{ TQuickEvalForm }

FUNCTION TQuickEvalForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icQuickEval;
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

end.

