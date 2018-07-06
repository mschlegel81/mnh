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
    PROCEDURE ListBoxKeyPress(Sender: TObject; VAR key: char);
  private
    { private declarations }
  public
    { public declarations }
    selectedFile:string;
  end;

FUNCTION openDemoDialogForm: TopenDemoDialogForm;
IMPLEMENTATION
VAR myOpenDemoDialogForm: TopenDemoDialogForm=nil;
FUNCTION openDemoDialogForm: TopenDemoDialogForm;
  begin
    if myOpenDemoDialogForm=nil then myOpenDemoDialogForm:=TopenDemoDialogForm.create(nil);
    result:=myOpenDemoDialogForm;
  end;

{$R *.lfm}

FUNCTION demosFolder:string;
  begin
    result:=configDir+'demos'+DirectorySeparator;
  end;

PROCEDURE TopenDemoDialogForm.ListBoxDblClick(Sender: TObject);
  begin
    selectedFile:=demosFolder+ListBox.items[ListBox.ItemIndex]+'.mnh';
    ModalResult:=mrOk;
  end;

PROCEDURE TopenDemoDialogForm.ListBoxKeyPress(Sender: TObject; VAR key: char);
  begin
    if key=#13 then ListBoxDblClick(Sender);
  end;

PROCEDURE TopenDemoDialogForm.FormShow(Sender: TObject);
  VAR files:T_arrayOfString;
      i:longint;
  begin
    ensureDemos;
    files:=find(demosFolder+'*.mnh',true,false);
    ListBox.items.clear;
    for i:=0 to length(files)-1 do ListBox.items.add(ExtractFileNameOnly(files[i]));
    ListBox.ItemIndex:=-1;
  end;

FINALIZATION
  {$ifdef debugMode}writeln(stdErr,'finalizing openDemoDialog');{$endif}
  if myOpenDemoDialogForm<>nil then FreeAndNil(myOpenDemoDialogForm);

end.

