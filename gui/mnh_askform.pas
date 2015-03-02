unit mnh_askForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel;

type

  { TAskForm }

  TAskForm = class(TForm)
    ButtonPanel: TButtonPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    FUNCTION showModalOnFileChange():integer;
    { public declarations }
  end;

var
  AskForm: TAskForm;

implementation

{$R *.lfm}

{ TAskForm }

procedure TAskForm.FormCreate(Sender: TObject);
begin
  ButtonPanel.HelpButton.Visible:=false;
  ButtonPanel.OKButton.ModalResult:=mrYes;
  ButtonPanel.CancelButton.ModalResult:=mrCancel;
  ButtonPanel.CloseButton.ModalResult:=mrNo;
  ButtonPanel.CancelButton.Caption:='Cancel';
end;

function TAskForm.showModalOnFileChange(): integer;
begin
  Caption:='Save before closing?';
  ButtonPanel.OKButton.Caption:='Save';
  ButtonPanel.CloseButton.Caption:='Don''t save';
  result:=ShowModal;
end;

end.

