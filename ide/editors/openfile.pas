UNIT openFile;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  myStringUtil,
  myGenerics,
  mnh_constants,
  editorMeta;

TYPE
  TopenFileDialog = class(TForm)
    CancelButton: TButton;
    OpenDialog1: TOpenDialog;
    openViaDialogButton: TButton;
    searchEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    searchResultsListBox: TListBox;
    Panel1: TPanel;
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE openViaDialogButtonClick(Sender: TObject);
    PROCEDURE searchEditChange(Sender: TObject);
    PROCEDURE searchEditKeyDown(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE searchEditKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE searchResultsListBoxKeyPress(Sender: TObject; VAR key: char);
  private
    selectedFile:string;
    fileList:T_arrayOfString;
  public
    PROPERTY getSelectedFile:string read selectedFile;
    FUNCTION showForRoot(CONST rootPath:string):longint;
    FUNCTION showClassicDialog:longint;
  end;

FUNCTION openFileDialog:TopenFileDialog;
IMPLEMENTATION
VAR myOpenFileDialog: TopenFileDialog=nil;
FUNCTION openFileDialog:TopenFileDialog;
  begin
    if myOpenFileDialog=nil then
      myOpenFileDialog:=TopenFileDialog.create(nil);
    result:=myOpenFileDialog;
  end;

{$R *.lfm}

PROCEDURE TopenFileDialog.openViaDialogButtonClick(Sender: TObject);
  begin
    if OpenDialog1.execute then begin
      selectedFile:=OpenDialog1.fileName;
      ModalResult:=mrOk;
    end else ModalResult:=mrCancel;
  end;

PROCEDURE TopenFileDialog.FormShow(Sender: TObject);
  begin
    searchEdit.SetFocus;
  end;

PROCEDURE TopenFileDialog.searchEditChange(Sender: TObject);
  VAR newList:T_arrayOfString;
      s:string;
  begin
    newList:=getListOfSimilarWords(searchEdit.text,fileList,100,true);
    searchResultsListBox.items.clear;
    for s in newList do searchResultsListBox.items.add(s);
  end;

PROCEDURE TopenFileDialog.searchEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if key=40 then begin
      searchResultsListBox.SetFocus;
      key:=0;
    end;
  end;

PROCEDURE TopenFileDialog.searchEditKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key=#13) and (searchResultsListBox.items.count=1) then begin
      selectedFile:=expandMnhDir(searchResultsListBox.items[0]);
      ModalResult:=mrOk;
    end;
  end;

PROCEDURE TopenFileDialog.searchResultsListBoxKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key=#13) and (searchResultsListBox.ItemIndex>=0) and (searchResultsListBox.ItemIndex<searchResultsListBox.items.count)
    then begin
      selectedFile:=expandMnhDir(searchResultsListBox.items[searchResultsListBox.ItemIndex]);
      ModalResult:=mrOk;
    end;
  end;

FUNCTION TopenFileDialog.showForRoot(CONST rootPath: string): longint;
  VAR k:longint;
  begin
    searchEdit.text:='';
    fileList:=folderHistory.findFiles(rootPath);
    append(fileList,fileHistory.items);
    sortUnique(fileList);
    for k:=0 to length(fileList)-1 do fileList[k]:=collapseMnhDir(fileList[k]);
    searchResultsListBox.clear;
    selectedFile:='';
    result:=ShowModal;
  end;

FUNCTION TopenFileDialog.showClassicDialog:longint;
  begin
    if OpenDialog1.execute then begin
      selectedFile:=OpenDialog1.fileName;
      result:=mrOk;
    end else result:=mrCancel;
  end;

FINALIZATION
  if myOpenFileDialog<>nil then FreeAndNil(myOpenFileDialog);

end.

