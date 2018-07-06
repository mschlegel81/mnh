UNIT renameDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  mnh_constants;

TYPE
  TrenameForm = class(TForm)
    CancelButton: TButton;
    checkAllEditorsCheckBox: TCheckBox;
    OKButton: TButton;
    OriginalNameEdit: TEdit;
    NewNameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PROCEDURE NewNameEditChange(Sender: TObject);
    PROCEDURE NewNameEditKeyPress(Sender: TObject; VAR key: char);
  private

  public
    FUNCTION showModalFor(CONST id:string; CONST tokenType:T_tokenType; CONST enableScanOther:boolean):integer;
    FUNCTION newId:string;
  end;

FUNCTION renameForm: TrenameForm;
IMPLEMENTATION
VAR
  myRenameForm: TrenameForm=nil;

FUNCTION renameForm: TrenameForm;
  begin
    if myRenameForm=nil then myRenameForm:=TrenameForm.create(nil);
    result:=myRenameForm;
  end;

{$R *.lfm}
PROCEDURE TrenameForm.NewNameEditKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key in ['A'..'Z','a'..'z',#8]) or (length(NewNameEdit.text)>0) and (key in ['0'..'9','_'])
    then exit
    else if key=#13 then ModalResult:=mrOk
    else key:=#0;
  end;

PROCEDURE TrenameForm.NewNameEditChange(Sender: TObject);
  begin
    OKButton.enabled:=NewNameEdit.text<>OriginalNameEdit.text;
  end;

FUNCTION TrenameForm.showModalFor(CONST id:string; CONST tokenType:T_tokenType; CONST enableScanOther:boolean):integer;
  begin
    caption:='Rename '+C_tokenInfo[tokenType].helpText;
    OriginalNameEdit.text:=id;
    NewNameEdit.text:=id;
    checkAllEditorsCheckBox.enabled:=enableScanOther;
    checkAllEditorsCheckBox.checked:=checkAllEditorsCheckBox.checked and enableScanOther;
    result:=ShowModal;
  end;

FUNCTION TrenameForm.newId: string;
  begin
    result:=NewNameEdit.text;
  end;

FINALIZATION
  {$ifdef debugMode}writeln(stderr,'finalizing renameDialog');{$endif}
  if myRenameForm<>nil then FreeAndNil(myRenameForm);
end.

