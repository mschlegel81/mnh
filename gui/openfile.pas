UNIT openFile;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  myStringUtil,
  myGenerics,
  editorMeta;

TYPE

  { TopenFileDialog }

  TopenFileDialog = class(TForm)
    OpenDialog1: TOpenDialog;
    openViaDialogButton: TButton;
    searchEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    searchResultsListBox: TListBox;
    Panel1: TPanel;
    PROCEDURE openViaDialogButtonClick(Sender: TObject);
    PROCEDURE searchEditChange(Sender: TObject);
    PROCEDURE searchEditKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE searchResultsListBoxKeyPress(Sender: TObject; VAR key: char);
  private
    selectedFile:string;
    fileList:T_arrayOfString;
  public
    PROPERTY getSelectedFile:string read selectedFile;
    FUNCTION showForRoot(CONST rootPath:string):longint;
  end;

VAR
  openFileDialog: TopenFileDialog;

IMPLEMENTATION

{$R *.lfm}

PROCEDURE TopenFileDialog.openViaDialogButtonClick(Sender: TObject);
  begin
    if OpenDialog1.execute then begin
      selectedFile:=OpenDialog1.fileName;
      ModalResult:=mrOk;
    end else ModalResult:=mrCancel;
  end;

PROCEDURE TopenFileDialog.searchEditChange(Sender: TObject);
  VAR newList:T_arrayOfString;
      s:string;
  begin
    newList:=getListOfSimilarWords(searchEdit.text,fileList,100);
    searchResultsListBox.items.clear;
    for s in newList do searchResultsListBox.items.add(s);
  end;

PROCEDURE TopenFileDialog.searchEditKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key=#13) and (searchResultsListBox.items.count=1) then begin
      selectedFile:=searchResultsListBox.items[0];
      ModalResult:=mrOk;
    end;
  end;

PROCEDURE TopenFileDialog.searchResultsListBoxKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key=#13) and (searchResultsListBox.ItemIndex>=0) and (searchResultsListBox.ItemIndex<searchResultsListBox.items.count)
    then begin
      selectedFile:=searchResultsListBox.items[searchResultsListBox.ItemIndex];
      ModalResult:=mrOk;
    end;
  end;

FUNCTION TopenFileDialog.showForRoot(CONST rootPath: string): longint;
  begin
    searchEdit.text:='';
    fileList:=fileHistory.findFiles(rootPath);
    searchResultsListBox.clear;
    selectedFile:='';
    result:=ShowModal;
  end;

end.

