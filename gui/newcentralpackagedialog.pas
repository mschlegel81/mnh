UNIT newCentralPackageDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, myStringUtil, mnh_constants, mnh_fileWrappers, myGenerics, LazFileUtils, mnh_doc;

TYPE

  { TnewCentralPackageForm }

  TnewCentralPackageForm = class(TForm)
    restoreButton: TButton;
    remButton: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ListBox: TListBox;
    packageNameEdit: TEdit;
    Label2: TLabel;
    fileNameEdit: TEdit;
    complaintLabel: TLabel;
    OKButton: TButton;
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ListBoxClick(Sender: TObject);
    PROCEDURE ListBoxDblClick(Sender: TObject);
    PROCEDURE ListBoxKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE packageNameEditChange(Sender: TObject);
    PROCEDURE remButtonClick(Sender: TObject);
    PROCEDURE restoreButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    PROCEDURE updatePackageList;
    { public declarations }
  end;

FUNCTION newCentralPackageForm: TnewCentralPackageForm;
IMPLEMENTATION
VAR myNewCentralPackageForm: TnewCentralPackageForm=nil;
FUNCTION newCentralPackageForm: TnewCentralPackageForm;
  begin
    if myNewCentralPackageForm=nil then myNewCentralPackageForm:=TnewCentralPackageForm.create(nil);
    result:=myNewCentralPackageForm;
  end;

{$R *.lfm}
FUNCTION packagesFolder:string;
  begin
    result:=configDir+'packages'+DirectorySeparator;
  end;

PROCEDURE TnewCentralPackageForm.FormShow(Sender: TObject);
  begin
    updatePackageList;
    packageNameEdit.text:='';
    fileNameEdit.text:='';
    packageNameEditChange(Sender);
  end;

PROCEDURE TnewCentralPackageForm.ListBoxClick(Sender: TObject);
  begin
    remButton.enabled:=true;
  end;

PROCEDURE TnewCentralPackageForm.ListBoxDblClick(Sender: TObject);
  begin
    fileNameEdit.text:=packagesFolder+ListBox.items[ListBox.ItemIndex]+'.mnh';
    ModalResult:=mrOk;
  end;

PROCEDURE TnewCentralPackageForm.ListBoxKeyPress(Sender: TObject; VAR key: char);
  begin
    if key=#13 then ListBoxDblClick(Sender);
  end;

PROCEDURE TnewCentralPackageForm.packageNameEditChange(Sender: TObject);
  CONST FILE_EXISTS_COMPLAINT='File already exists';
        PACKAGE_NAME_COMPLAINT='Name must be a valid identifier';
  begin
    fileNameEdit.text:=packagesFolder+packageNameEdit.text+'.mnh';
    if not(isIdentifier(packageNameEdit.text,false)) then begin
      complaintLabel.caption:=PACKAGE_NAME_COMPLAINT;
      complaintLabel.visible:=true;
      OKButton.enabled:=false;
      exit;
    end;
    if fileExists(fileNameEdit.text) then begin
      complaintLabel.caption:=FILE_EXISTS_COMPLAINT;
      complaintLabel.visible:=true;
      OKButton.enabled:=false;
      exit;
    end;
    complaintLabel.visible:=false;
    OKButton.enabled:=true;
  end;

PROCEDURE TnewCentralPackageForm.remButtonClick(Sender: TObject);
  begin
    DeleteFile(packagesFolder+ListBox.items[ListBox.ItemIndex]+'.mnh');
    updatePackageList;
  end;

PROCEDURE TnewCentralPackageForm.restoreButtonClick(Sender: TObject);
  begin
    ensureDemos;
    updatePackageList;
  end;

PROCEDURE TnewCentralPackageForm.updatePackageList;
  VAR files:T_arrayOfString;
      i:longint;
  begin
    files:=find(packagesFolder+'*.mnh',true,false);
    ListBox.items.clear;
    for i:=0 to length(files)-1 do ListBox.items.add(ExtractFileNameOnly(files[i]));
    ListBox.ItemIndex:=-1;
    remButton.enabled:=false;
  end;

FINALIZATION
  if myNewCentralPackageForm<>nil then FreeAndNil(myNewCentralPackageForm);

end.

