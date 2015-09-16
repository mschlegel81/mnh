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
    FUNCTION showOnOutOfSync: integer;
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

FUNCTION TcloseDialogForm.showOnOutOfSync: integer;
  begin
    Caption:='The current file is out of sync';
    ButtonPanel1.OKButton.Caption := 'Reload';
    ButtonPanel1.CancelButton.Caption := 'Overwrite';
    ButtonPanel1.CloseButton.Caption := 'Ignore changes';
    result:=ShowModal;
  end;

end.


