UNIT closeDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ButtonPanel;
TYPE
  T_closeDialogAnswer=(cda_noneOrInvalid,
                       cda_close,cda_reload,cda_ignoreChanges,cda_overwrite,
                       cda_cancel,cda_pickAnother,
                       cda_saveAndChange,cda_cancelAndStay,cda_discardChanges,
                       cda_saveAndClose,
                       cda_quitAfterEval,cda_dontQuit,cda_cancelEvalAndQuit);
CONST
  C_answerText:array[T_closeDialogAnswer] of string=('',
                                                     'Close','Reload','Ignore changes','Overwrite',
                                                     'Cancel','Pick another',
                                                     'Save and change file','Cancel (stay here)','Discard changes',
                                                     'Save and close file',
                                                     'Quit after end of evaluation','Don''t quit','Cancel evaluation and quit');

TYPE
  TcloseDialogForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    PROCEDURE FormShow(Sender: TObject);
  private
    FUNCTION showFor(CONST okAnswer,cancelAnswer,closeAnswer:T_closeDialogAnswer):T_closeDialogAnswer;
  public
    FUNCTION showOnOverwrite(CONST fileName:string): T_closeDialogAnswer;
    FUNCTION showOnLoad     (CONST fileName:string): T_closeDialogAnswer;
    FUNCTION showOnClose    (CONST fileName:string): T_closeDialogAnswer;
    {OK: close, Cancel: ignore, Close: overwrite}
    FUNCTION showOnDeleted  (CONST fileName:string): T_closeDialogAnswer;
    {OK: reload, Cancel: ignore, Close: overwrite}
    FUNCTION showOnOutOfSync(CONST fileName:string): T_closeDialogAnswer;
    FUNCTION showOnQuitWhileEvaluating:T_closeDialogAnswer;
  end;

FUNCTION closeDialogForm:TcloseDialogForm;
IMPLEMENTATION
USES FileUtil;
VAR
  myCloseDialogForm: TcloseDialogForm=nil;

FUNCTION closeDialogForm:TcloseDialogForm;
  begin
    if myCloseDialogForm=nil then myCloseDialogForm:=TcloseDialogForm.create(Application);
    result:=myCloseDialogForm;
  end;

{$R *.lfm}
PROCEDURE TcloseDialogForm.FormShow(Sender: TObject);
  begin
  end;

FUNCTION TcloseDialogForm.showFor(CONST okAnswer, cancelAnswer, closeAnswer: T_closeDialogAnswer): T_closeDialogAnswer;
  VAR mr:integer;
  begin
    ButtonPanel1.OKButton    .caption := C_answerText[okAnswer];
    ButtonPanel1.CancelButton.caption := C_answerText[cancelAnswer];
    ButtonPanel1.CloseButton .caption := C_answerText[closeAnswer];
    mr := ShowModal;
    if      mr=mrOk     then result:=okAnswer
    else if mr=mrCancel then result:=cancelAnswer
    else if mr=mrClose  then result:=closeAnswer
    else result:=cda_noneOrInvalid;
  end;

FUNCTION TcloseDialogForm.showOnOverwrite(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:=fileName+' already exists';
    result:=showFor(cda_overwrite,cda_pickAnother,cda_cancel);
  end;

FUNCTION TcloseDialogForm.showOnLoad(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:=fileName+' has been changed';
    result:=showFor(cda_saveAndChange,cda_cancelAndStay,cda_discardChanges);
  end;

FUNCTION TcloseDialogForm.showOnClose(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:=fileName+' has been changed';
    result:=showFor(cda_saveAndClose,cda_cancelAndStay,cda_discardChanges);
  end;

FUNCTION TcloseDialogForm.showOnOutOfSync(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:=fileName+' is out of sync';
    result:=showFor(cda_reload,cda_ignoreChanges,cda_overwrite);
  end;

FUNCTION TcloseDialogForm.showOnDeleted(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:=fileName+' is deleted';
    result:=showFor(cda_close,cda_ignoreChanges,cda_overwrite);
  end;

FUNCTION TcloseDialogForm.showOnQuitWhileEvaluating: T_closeDialogAnswer;
  begin
    caption:='Still evaluating...';
    result:=showFor(cda_quitAfterEval,cda_dontQuit,cda_cancelEvalAndQuit);
  end;

end.

