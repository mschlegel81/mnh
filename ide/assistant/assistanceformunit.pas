UNIT assistanceFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, SynEdit,
  ideLayoutUtil,codeAssistance,SynHighlighterMnh,editorMeta,mnh_settings,basicTypes;

TYPE
  TAssistanceForm = class(T_mnhComponentForm)
    AssistanceEdit: TSynEdit;
    assistanceHighlighter:TSynMnhSyn;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private
    paintedWithStateHash:T_hashInt;
  public

  end;

PROCEDURE ensureAssistanceForm;
IMPLEMENTATION

{$R *.lfm}
PROCEDURE ensureAssistanceForm;
  begin
    if not(hasFormOfType(icAssistance,true)) then dockNewForm(TAssistanceForm.create(Application));
  end;

PROCEDURE TAssistanceForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(AssistanceEdit,ctEditor);
    assistanceHighlighter:=TSynMnhSyn.create(self,msf_output);
    AssistanceEdit.highlighter:=assistanceHighlighter;
    AssistanceEdit.OnKeyUp:=@workspace.keyUpForJumpToLocation;
    AssistanceEdit.OnMouseDown:=@workspace.mouseDownForJumpToLocation;
    paintedWithStateHash:=0;
  end;

PROCEDURE TAssistanceForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(AssistanceEdit);
  end;

FUNCTION TAssistanceForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icAssistance;
  end;

PROCEDURE TAssistanceForm.performSlowUpdate;
  CONST conditionalCaption:array[false..true,false..true] of string=
       (('Assistance','Warnings'           ),
        ('Errors'    ,'Errors and Warnings'));

  VAR hasErrors  :boolean=false;
      hasWarnings:boolean=false;
      codeAssistanceResponse:P_codeAssistanceResponse;
  begin
    codeAssistanceResponse:=workspace.getCurrentAssistanceResponse;
    if (codeAssistanceResponse<>nil) and (codeAssistanceResponse^.stateHash<>paintedWithStateHash)
    then begin
      codeAssistanceResponse^.getErrorHints(AssistanceEdit,hasErrors,hasWarnings);
      caption:=conditionalCaption[hasErrors,hasWarnings];
      if parent<>nil then parent.caption:=conditionalCaption[hasErrors,hasWarnings];
      paintedWithStateHash:=codeAssistanceResponse^.stateHash;
    end;
    disposeCodeAssistanceResponse(codeAssistanceResponse);
  end;

PROCEDURE TAssistanceForm.performFastUpdate;
  begin

  end;

end.

