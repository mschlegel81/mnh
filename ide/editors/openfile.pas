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
  { TopenFileDialog }

  TopenFileDialog = class(TForm)
    CancelButton: TButton;
    OpenDialog1: TOpenDialog;
    openViaDialogButton: TButton;
    searchEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    searchResultsListBox: TListBox;
    Panel1: TPanel;
    Timer1: TTimer;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE openViaDialogButtonClick(Sender: TObject);
    PROCEDURE searchEditChange(Sender: TObject);
    PROCEDURE searchEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE searchEditKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE searchResultsListBoxKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE Timer1Timer(Sender: TObject);
  private
    selectedFile:T_arrayOfString;
  public
    PROPERTY getSelectedFile:T_arrayOfString read selectedFile;
    FUNCTION showForRoot(CONST rootPath:string):longint;
    FUNCTION showClassicDialog:longint;
  end;

FUNCTION openFileDialog:TopenFileDialog;
IMPLEMENTATION
USES fileWrappers;
VAR myOpenFileDialog: TopenFileDialog=nil;
FUNCTION openFileDialog:TopenFileDialog;
  begin
    if myOpenFileDialog=nil then
      myOpenFileDialog:=TopenFileDialog.create(Application);
    result:=myOpenFileDialog;
  end;

{$R *.lfm}

PROCEDURE TopenFileDialog.openViaDialogButtonClick(Sender: TObject);
  VAR s:string;
  begin
    if OpenDialog1.execute then begin
      setLength(selectedFile,0);
      for s in OpenDialog1.Files do append(selectedFile,s);
      ModalResult:=mrOk;
    end else ModalResult:=mrCancel;
  end;

PROCEDURE TopenFileDialog.FormShow(Sender: TObject);
  begin
    searchEdit.SetFocus;
    fileCache.scanInBackground;
  end;

PROCEDURE TopenFileDialog.FormCreate(Sender: TObject);
  begin
  end;

PROCEDURE TopenFileDialog.FormDestroy(Sender: TObject);
  begin
  end;

PROCEDURE TopenFileDialog.searchEditChange(Sender: TObject);
  VAR oldItemIndex:longint;
      oldItem:string;
      fileList: T_arrayOfString;
      s:string;
  begin
    searchResultsListBox.items.BeginUpdate;
    oldItemIndex:=searchResultsListBox.ItemIndex;
    if oldItemIndex>=0
    then oldItem:=searchResultsListBox.items[oldItemIndex]
    else oldItem:='';

    fileList:=fileCache.listFilesMatching(searchEdit.text);
    searchResultsListBox.items.clear;
    for s in fileList do searchResultsListBox.items.add(s);

    if oldItem<>'' then searchResultsListBox.ItemIndex:=searchResultsListBox.items.IndexOf(oldItem);
    searchResultsListBox.items.EndUpdate;
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

PROCEDURE TopenFileDialog.Timer1Timer(Sender: TObject);
  begin
    if fileCache.currentlyScanning then searchEditChange(Sender);
  end;

FUNCTION TopenFileDialog.showForRoot(CONST rootPath: string): longint;
  begin
    searchEdit.text:='';
    searchResultsListBox.clear;
    selectedFile:='';
    result:=ShowModal;
  end;

FUNCTION TopenFileDialog.showClassicDialog:longint;
  VAR s:string;
  begin
    if OpenDialog1.execute then begin
      setLength(selectedFile,0);
      for s in OpenDialog1.Files do append(selectedFile,s);
      result:=mrOk;
    end else result:=mrCancel;
  end;

end.

