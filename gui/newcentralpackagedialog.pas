UNIT newCentralPackageDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, myStringUtil,mnh_constants;

TYPE

  { TnewCentralPackageForm }

  TnewCentralPackageForm = class(TForm)
    Label1: TLabel;
    packageNameEdit: TEdit;
    Label2: TLabel;
    fileNameEdit: TEdit;
    complaintLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE packageNameEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

VAR
  newCentralPackageForm: TnewCentralPackageForm;

IMPLEMENTATION

{$R *.lfm}

{ TnewCentralPackageForm }

PROCEDURE TnewCentralPackageForm.FormShow(Sender: TObject);
  begin
    packageNameEdit.text:='';
    fileNameEdit.text:='';
    packageNameEditChange(Sender);
  end;

PROCEDURE TnewCentralPackageForm.packageNameEditChange(Sender: TObject);
  CONST FILE_EXISTS_COMPLAINT='File already exists';
        PACKAGE_NAME_COMPLAINT='Package name must be a valid identifier';
  begin
    fileNameEdit.text:=configDir+'packages'+DirectorySeparator+packageNameEdit.text+'.mnh';
    if not(isIdentifier(packageNameEdit.text,false)) then begin
      complaintLabel.Caption:=PACKAGE_NAME_COMPLAINT;
      complaintLabel.visible:=true;
      OKButton.Enabled:=false;
      exit;
    end;
    if fileExists(fileNameEdit.text) then begin
      complaintLabel.Caption:=FILE_EXISTS_COMPLAINT;
      complaintLabel.visible:=true;
      OKButton.Enabled:=false;
      exit;
    end;
    complaintLabel.visible:=false;
    OKButton.Enabled:=true;
  end;

end.

