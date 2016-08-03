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
    FUNCTION showOnQuit: integer;
    FUNCTION showOnLoad: integer;
    FUNCTION showOnDeleted(CONST fileName:string): integer;
    FUNCTION showOnOutOfSync(CONST fileName:string): integer;
    FUNCTION showOnUninstall: integer;
  end;

VAR
  closeDialogForm: TcloseDialogForm;

IMPLEMENTATION

{$R *.lfm}

{ TcloseDialogForm }

PROCEDURE TcloseDialogForm.FormShow(Sender: TObject);
  begin
  end;

FUNCTION TcloseDialogForm.showOnQuit: integer;
  begin
    Caption:='The current file has been changed';
    ButtonPanel1.OKButton.Caption := 'Save and quit';
    ButtonPanel1.CancelButton.Caption := 'Cancel';
    ButtonPanel1.CloseButton.Caption := 'Discard changes';
    result := ShowModal;
  end;

FUNCTION TcloseDialogForm.showOnLoad: integer;
  begin
    Caption:='The current file has been changed';
    ButtonPanel1.OKButton.Caption := 'Save and change file';
    ButtonPanel1.CancelButton.Caption := 'Cancel (stay here)';
    ButtonPanel1.CloseButton.Caption := 'Discard changes';
    result := ShowModal;
  end;

FUNCTION TcloseDialogForm.showOnOutOfSync(CONST fileName:string): integer;
  begin
    Caption:=fileName+' is out of sync';
    ButtonPanel1.OKButton.Caption := 'Reload';
    ButtonPanel1.CancelButton.Caption := 'Ignore changes';
    ButtonPanel1.CloseButton.Caption := 'Overwrite';
    result:=ShowModal;
  end;

FUNCTION TcloseDialogForm.showOnDeleted(CONST fileName:string): integer;
  begin
    Caption:=fileName+' is deleted';
    ButtonPanel1.OKButton.Caption := 'Close';
    ButtonPanel1.CancelButton.Caption := 'Ignore changes';
    ButtonPanel1.CloseButton.Caption := 'Overwrite';
    result:=ShowModal;
  end;

FUNCTION TcloseDialogForm.showOnUninstall: integer;
  begin
    Caption:='Do you really want to uninstall MNH5?';
    ButtonPanel1.OKButton.Caption := 'Uninstall and quit';
    ButtonPanel1.CancelButton.Caption := 'Cancel';
    ButtonPanel1.CloseButton.Caption := 'Quit without uninstalling';
    result := ShowModal;
  end;

end.


