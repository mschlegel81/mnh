UNIT closeDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls;
TYPE
  T_closeDialogAnswer=(cda_noneOrInvalid,
                       cda_close,cda_reload,cda_ignoreChanges,cda_overwrite,
                       cda_cancel,cda_pickAnother,
                       cda_saveAndChange,cda_cancelAndStay,cda_discardChanges,
                       cda_saveAndClose,
                       cda_quitAfterEval,cda_dontQuit,cda_cancelEvalAndQuit,
                       cda_continueUninstall);
CONST
  C_answerText:array[T_closeDialogAnswer] of string=('',
                                                     'Close','Reload','Ignore changes','Overwrite',
                                                     'Cancel','Pick another',
                                                     'Save and change file','Cancel (stay here)','Discard changes',
                                                     'Save and close file',
                                                     'Quit after end of evaluation','Don''t quit','Cancel evaluation and quit',
                                                     'Yes, Uninstall');

TYPE
  TcloseDialogForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
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
    {OK: cda_continueUninstall, Canel:cda_cancel, Close:-}
    FUNCTION showOnUninstall:T_closeDialogAnswer;
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
    AdjustSize;
  end;

FUNCTION TcloseDialogForm.showFor(CONST okAnswer, cancelAnswer, closeAnswer: T_closeDialogAnswer): T_closeDialogAnswer;
  VAR mr:integer;
  begin
    ButtonPanel1.OKButton    .caption := C_answerText[okAnswer    ];
    ButtonPanel1.OKButton    .enabled:=(okAnswer    <>cda_noneOrInvalid);
    ButtonPanel1.OKButton    .visible:=(okAnswer    <>cda_noneOrInvalid);
    ButtonPanel1.CancelButton.caption := C_answerText[cancelAnswer];
    ButtonPanel1.CancelButton.enabled:=(cancelAnswer<>cda_noneOrInvalid);
    ButtonPanel1.CancelButton.visible:=(cancelAnswer<>cda_noneOrInvalid);
    ButtonPanel1.CloseButton .caption := C_answerText[closeAnswer ];
    ButtonPanel1.CloseButton .enabled:=(closeAnswer <>cda_noneOrInvalid);
    ButtonPanel1.CloseButton .visible:=(closeAnswer <>cda_noneOrInvalid);
    mr := ShowModal;
    if      mr=mrOk     then result:=okAnswer
    else if mr=mrCancel then result:=cancelAnswer
    else if mr=mrClose  then result:=closeAnswer
    else result:=cda_noneOrInvalid;
  end;

FUNCTION TcloseDialogForm.showOnOverwrite(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:='File already exists';
    memo1.text:=fileName+' already exists';
    result:=showFor(cda_overwrite,cda_pickAnother,cda_cancel);
  end;

FUNCTION TcloseDialogForm.showOnLoad(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:='File has been changed';
    memo1.text:=fileName+' has been changed';
    result:=showFor(cda_saveAndChange,cda_cancelAndStay,cda_discardChanges);
  end;

FUNCTION TcloseDialogForm.showOnClose(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:='File has been changed';
    memo1.text:=fileName+' has been changed';
    result:=showFor(cda_saveAndClose,cda_cancelAndStay,cda_discardChanges);
  end;

FUNCTION TcloseDialogForm.showOnOutOfSync(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:='File is ouf of sync';
    memo1.text:=fileName+' is out of sync';
    result:=showFor(cda_reload,cda_ignoreChanges,cda_overwrite);
  end;

FUNCTION TcloseDialogForm.showOnDeleted(CONST fileName: string): T_closeDialogAnswer;
  begin
    caption:='File is deleted';
    memo1.text:=fileName+' is deleted';
    result:=showFor(cda_close,cda_ignoreChanges,cda_overwrite);
  end;

FUNCTION TcloseDialogForm.showOnQuitWhileEvaluating: T_closeDialogAnswer;
  begin
    caption:='Still evaluating...';
    memo1.text:='An evaluation is still running.';
    result:=showFor(cda_quitAfterEval,cda_dontQuit,cda_cancelEvalAndQuit);
  end;

FUNCTION TcloseDialogForm.showOnUninstall:T_closeDialogAnswer;
  begin
    caption:='Do you really want to uninstall MNH?';
    memo1.text:='Do you really want to uninstall MNH?';
    result:=showFor(cda_continueUninstall,cda_cancel,cda_noneOrInvalid);
  end;

end.

