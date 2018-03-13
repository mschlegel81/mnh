UNIT renameDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  mnhFormHandler;

TYPE

  { TrenameForm }

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
    FUNCTION showModalFor(CONST id,typeString:string):integer;
    FUNCTION newId:string;
  end;

FUNCTION renameForm: TrenameForm;
IMPLEMENTATION
VAR
  myRenameForm: TrenameForm=nil;

FUNCTION renameForm: TrenameForm;
  begin
    if myRenameForm=nil then begin
      myRenameForm:=TrenameForm.create(nil);
      registerForm(myRenameForm,false,false);
    end;
    result:=myRenameForm;
  end;

{$R *.lfm}
PROCEDURE TrenameForm.NewNameEditKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key in ['A'..'Z','a'..'z',#8]) or (length(NewNameEdit.text)>0) and (key in ['0'..'9','_'])
    then exit
    else key:=#0;
  end;

PROCEDURE TrenameForm.NewNameEditChange(Sender: TObject);
  begin
    OKButton.enabled:=NewNameEdit.text<>OriginalNameEdit.text;
  end;

FUNCTION TrenameForm.showModalFor(CONST id, typeString: string): integer;
  begin
    caption:='Rename '+typeString;
    OriginalNameEdit.text:=id;
    NewNameEdit.text:=id;
    result:=ShowModal;
  end;

FUNCTION TrenameForm.newId: string;
  begin
    result:=NewNameEdit.text;
  end;

FINALIZATION
  if myRenameForm<>nil then FreeAndNil(myRenameForm);
end.

