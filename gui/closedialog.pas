UNIT closeDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel;
TYPE

  { TcloseDialogForm }

  TcloseDialogForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    PROCEDURE FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FUNCTION showOnLoad     (CONST fileName:string): integer;
    FUNCTION showOnClose    (CONST fileName:string): integer;
    FUNCTION showOnDeleted  (CONST fileName:string): integer;
    FUNCTION showOnOutOfSync(CONST fileName:string): integer;
  end;

FUNCTION closeDialogForm:TcloseDialogForm;
IMPLEMENTATION
VAR
  myCloseDialogForm: TcloseDialogForm=nil;

FUNCTION closeDialogForm:TcloseDialogForm;
  begin
    if myCloseDialogForm=nil then myCloseDialogForm:=TcloseDialogForm.create(nil);
    result:=myCloseDialogForm;
  end;

{$R *.lfm}

{ TcloseDialogForm }

PROCEDURE TcloseDialogForm.FormShow(Sender: TObject);
  begin
  end;

FUNCTION TcloseDialogForm.showOnLoad(CONST fileName:string): integer;
  begin
    caption:=fileName+' has been changed';
    ButtonPanel1.OKButton.caption := 'Save and change file';
    ButtonPanel1.CancelButton.caption := 'Cancel (stay here)';
    ButtonPanel1.CloseButton.caption := 'Discard changes';
    result := ShowModal;
  end;

FUNCTION TcloseDialogForm.showOnClose(CONST fileName:string): integer;
  begin
    caption:=fileName+' has been changed';
    ButtonPanel1.OKButton.caption := 'Save and close file';
    ButtonPanel1.CancelButton.caption := 'Cancel (stay here)';
    ButtonPanel1.CloseButton.caption := 'Discard changes';
    result := ShowModal;
  end;

FUNCTION TcloseDialogForm.showOnOutOfSync(CONST fileName:string): integer;
  begin
    caption:=fileName+' is out of sync';
    ButtonPanel1.OKButton.caption := 'Reload';
    ButtonPanel1.CancelButton.caption := 'Ignore changes';
    ButtonPanel1.CloseButton.caption := 'Overwrite';
    result:=ShowModal;
  end;

FUNCTION TcloseDialogForm.showOnDeleted(CONST fileName:string): integer;
  begin
    caption:=fileName+' is deleted';
    ButtonPanel1.OKButton.caption := 'Close';
    ButtonPanel1.CancelButton.caption := 'Ignore changes';
    ButtonPanel1.CloseButton.caption := 'Overwrite';
    result:=ShowModal;
  end;

FINALIZATION
  if myCloseDialogForm<>nil then FreeAndNil(myCloseDialogForm);

end.


