UNIT openDemoDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  mnh_constants,mnh_fileWrappers,myGenerics,LazFileUtils,mnh_doc;

TYPE
  TopenDemoDialogForm = class(TForm)
    ListBox: TListBox;
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ListBoxDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    selectedFile:string;
  end;

VAR
  openDemoDialogForm: TopenDemoDialogForm;

IMPLEMENTATION

{$R *.lfm}

FUNCTION demosFolder:string;
  begin
    result:=configDir+'demos'+DirectorySeparator;
  end;

PROCEDURE TopenDemoDialogForm.ListBoxDblClick(Sender: TObject);
  begin
    selectedFile:=demosFolder+ListBox.Items[ListBox.ItemIndex]+'.mnh';
    ModalResult:=mrOk;
  end;

PROCEDURE TopenDemoDialogForm.FormShow(Sender: TObject);
  VAR files:T_arrayOfString;
      i:longint;
  begin
    ensureDemos;
    files:=find(demosFolder+'*.mnh',true,false);
    ListBox.Items.clear;
    for i:=0 to length(files)-1 do ListBox.Items.add(ExtractFileNameOnly(files[i]));
    ListBox.ItemIndex:=-1;
  end;

end.

