UNIT assistanceFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, SynEdit,
  ideLayoutUtil,codeAssistance,SynHighlighterMnh,editorMeta,mnh_settings,basicTypes;

TYPE
  TAssistanceForm = class(T_mnhComponentForm)
    AssistanceEdit: TSynEdit;
    assistanceHighlighter:TMnhOutputSyn;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private
    paintedWithStateHash:T_hashInt;
    paintedWithWidth:longint;
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
    assistanceHighlighter:=TMnhOutputSyn.create(self);
    AssistanceEdit.highlighter:=assistanceHighlighter;
    AssistanceEdit.OnKeyUp:=@workspace.keyUpForJumpToLocation;
    AssistanceEdit.OnMouseDown:=@workspace.mouseDownForJumpToLocation;
    paintedWithStateHash:=0;
    paintedWithWidth:=0;
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
    try
      if (codeAssistanceResponse<>nil) and ((codeAssistanceResponse^.stateHash<>paintedWithStateHash) or (AssistanceEdit.charsInWindow<>paintedWithWidth))
      then begin
        codeAssistanceResponse^.getErrorHints(AssistanceEdit,hasErrors,hasWarnings);
        caption:=conditionalCaption[hasErrors,hasWarnings];
        if parent<>nil then parent.caption:=conditionalCaption[hasErrors,hasWarnings];
        paintedWithStateHash:=codeAssistanceResponse^.stateHash;
        paintedWithWidth:=AssistanceEdit.charsInWindow;
      end;
    finally
      try disposeCodeAssistanceResponse(codeAssistanceResponse); except end;
    end;
  end;

PROCEDURE TAssistanceForm.performFastUpdate;
  begin

  end;

end.

