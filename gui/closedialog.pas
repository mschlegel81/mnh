UNIT closeDialog;

{$mode objfpc}{$H+}

INTERFACE

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel;
type

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
    ButtonPanel1.OKButton.Caption := 'Save and quit';
    ButtonPanel1.CancelButton.Caption := 'Cancel';
    ButtonPanel1.CloseButton.Caption := 'Discard changes';
    result := ShowModal;
  end;

FUNCTION TcloseDialogForm.showOnLoad: integer;
  begin
    ButtonPanel1.OKButton.Caption := 'Save and change file';
    ButtonPanel1.CancelButton.Caption := 'Cancel';
    ButtonPanel1.CloseButton.Caption := 'Discard changes';
    result := ShowModal;
  end;

end.


