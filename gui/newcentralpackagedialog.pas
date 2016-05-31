// MIT License
//
// Copyright (c) 2016 Martin Schlegel
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

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

