UNIT assistanceFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, SynEdit,
  ideLayoutUtil,codeAssistance,SynHighlighterMnh,editorMeta;

TYPE

  { TAssistanceForm }

  TAssistanceForm = class(T_mnhComponentForm)
    AssistanceEdit: TSynEdit;
    assistanceHighlighter:TSynMnhSyn;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private

  public

  end;

PROCEDURE ensureAssistanceForm;
IMPLEMENTATION

{$R *.lfm}
PROCEDURE ensureAssistanceForm;
  begin
    if not(hasFormOfType(icAssistance)) then dockNewForm(TAssistanceForm.create(Application));
  end;

PROCEDURE TAssistanceForm.FormCreate(Sender: TObject);
  begin
    registerSynEdit(AssistanceEdit);
    assistanceHighlighter:=TSynMnhSyn.create(self,msf_output);
    AssistanceEdit.highlighter:=assistanceHighlighter;
  end;

PROCEDURE TAssistanceForm.FormDestroy(Sender: TObject);
  begin
    unregisterSynEdit(AssistanceEdit);
  end;

FUNCTION TAssistanceForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icAssistance;
  end;

PROCEDURE TAssistanceForm.performSlowUpdate;
  CONST conditionalCaption:array[false..true,false..true] of string=
       (('Assistance','Warnings'),
        ('Errors','Errors and Warnings'));

  VAR edit:P_editorMeta;
      assistantData:P_codeAssistanceResponse;
      hasErrors  :boolean=false;
      hasWarnings:boolean=false;
  begin
    edit:=workspace.currentEditor;
    if edit=nil
    then exit;
    assistantData:=edit^.getCodeAssistanceData;
    if assistantData<>nil
    then assistantData^.getErrorHints(AssistanceEdit,hasErrors,hasWarnings);
    caption:=conditionalCaption[hasErrors,hasWarnings];
    if parent<>nil then parent.caption:=caption;
  end;

PROCEDURE TAssistanceForm.performFastUpdate;
  begin

  end;

end.

