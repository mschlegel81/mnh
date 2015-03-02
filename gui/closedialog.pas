unit closeDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel;

type

  { TcloseDialogForm }

  TcloseDialogForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FUNCTION showOnQuit:integer;
    FUNCTION showOnLoad:integer;
  end;

var
  closeDialogForm: TcloseDialogForm;

implementation

{$R *.lfm}

{ TcloseDialogForm }

procedure TcloseDialogForm.FormShow(Sender: TObject);
begin
end;

function TcloseDialogForm.showOnQuit: integer;
begin
  ButtonPanel1.OKButton.Caption:='Save and quit';
  ButtonPanel1.CancelButton.Caption:='Cancel';
  ButtonPanel1.CloseButton.Caption:='Discard changes';
  result:=ShowModal
end;

function TcloseDialogForm.showOnLoad: integer;
begin
  ButtonPanel1.OKButton.Caption:='Save and change file';
  ButtonPanel1.CancelButton.Caption:='Cancel';
  ButtonPanel1.CloseButton.Caption:='Discard changes';
  result:=ShowModal;
end;

end.

