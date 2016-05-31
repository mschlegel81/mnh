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

FUNCTION TcloseDialogForm.showOnUninstall: integer;
  begin
    Caption:='Do you really want to uninstall MNH5?';
    ButtonPanel1.OKButton.Caption := 'Uninstall and quit';
    ButtonPanel1.CancelButton.Caption := 'Cancel';
    ButtonPanel1.CloseButton.Caption := 'Quit without uninstalling';
    result := ShowModal;
  end;

end.


