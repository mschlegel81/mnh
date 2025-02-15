UNIT assistanceFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Dialogs, Menus,
  SynEdit, ideLayoutUtil, codeAssistance, SynHighlighterMnh, editorMeta,
  mnh_settings, basicTypes, messageFormatting;

TYPE

  { TAssistanceForm }

  TAssistanceForm = class(T_mnhComponentForm)
    AssistanceEdit: TSynEdit;
    assistanceHighlighter:TMnhOutputSyn;
    MainMenu1: TMainMenu;
    PopupMenu1: TPopupMenu;
    PROCEDURE AssistanceEditKeyUp(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE AssistanceEditMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
  private
    paintedWithStateHash:T_hashInt;
    paintedWithWidth:longint;
    messagesAndLocations:T_messagesAndLocations;
    PROCEDURE openLocationForLine(CONST i:longint);
  public

  end;

PROCEDURE ensureAssistanceForm;
IMPLEMENTATION
USES mnh_messages;

{$R *.lfm}
PROCEDURE ensureAssistanceForm;
  begin
    if not(hasFormOfType(icAssistance,true)) then dockNewForm(TAssistanceForm.create(Application));
  end;

PROCEDURE TAssistanceForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(AssistanceEdit,ctEditor);
    messagesAndLocations.create(1000);
    assistanceHighlighter:=TMnhOutputSyn.create(self,@messagesAndLocations);
    AssistanceEdit.highlighter:=assistanceHighlighter;
    paintedWithStateHash:=0;
    paintedWithWidth:=0;
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
  end;

PROCEDURE TAssistanceForm.AssistanceEditKeyUp(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    tabNextKeyHandling(Sender,key,Shift);
    if (key=13) and (ssCtrl in Shift) then begin
      openLocationForLine(AssistanceEdit.CaretY-1);
      key:=0;
    end;
  end;

PROCEDURE TAssistanceForm.AssistanceEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (ssCtrl in Shift) and (button=mbLeft) then begin
      openLocationForLine(AssistanceEdit.PixelsToRowColumn(point(x,y)).Y-1);
    end;
  end;

PROCEDURE TAssistanceForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(AssistanceEdit);
  end;

FUNCTION TAssistanceForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icAssistance;
  end;

PROCEDURE TAssistanceForm.performSlowUpdate(CONST isEvaluationRunning: boolean);
  begin
  end;

PROCEDURE TAssistanceForm.performFastUpdate;
  CONST conditionalCaption:array[false..true,false..true] of string=
       (('Assistance','Warnings'           ),
        ('Errors'    ,'Errors and Warnings'));

  VAR hasErrors  :boolean=false;
      hasWarnings:boolean=false;
      codeAssistanceResponse:P_codeAssistanceResponse;
      i:longint;
  begin
    codeAssistanceResponse:=workspace.getCurrentAssistanceResponse;
    try
      if (codeAssistanceResponse<>nil) and ((codeAssistanceResponse^.stateHash<>paintedWithStateHash) or (AssistanceEdit.charsInWindow<>paintedWithWidth))
      then begin
        messagesAndLocations.clear;
        codeAssistanceResponse^.getErrorHints(hasErrors,hasWarnings,messagesAndLocations);
        caption                           :=conditionalCaption[hasErrors,hasWarnings]+' ('+COMPONENT_SHORTCUT[icAssistance]+')';
        if parent<>nil then parent.caption:=conditionalCaption[hasErrors,hasWarnings]+' ('+COMPONENT_SHORTCUT[icAssistance]+')';
        AssistanceEdit.clear;
        for i:=0 to messagesAndLocations.size-1 do
          AssistanceEdit.append(string(messagesAndLocations.location(i))+' '+messagesAndLocations.text(i));
        paintedWithStateHash:=codeAssistanceResponse^.stateHash;
        paintedWithWidth:=AssistanceEdit.charsInWindow;
      end;
    finally
      if codeAssistanceResponse<>nil then try disposeMessage(codeAssistanceResponse); except end;
    end;
  end;

PROCEDURE TAssistanceForm.dockChanged;
  begin
  end;

PROCEDURE TAssistanceForm.openLocationForLine(CONST i: longint);
  begin
    if (i>=0) and (i<messagesAndLocations.size) then workspace.openLocation(messagesAndLocations.location(i));
  end;

end.

