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
  T_BackgroundFileSearch=class(TThread)
    searchCS:TRTLCriticalSection;
    foldersToScan,
    foldersScanned,
    found:T_arrayOfString;

    CONSTRUCTOR create(CONST rootPath:string);
    DESTRUCTOR destroy; override;
    PROCEDURE execute; override;
    PROCEDURE getFilteredWords(CONST filter:string; CONST items:TStrings);
  end;

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
    timer1: TTimer;
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
    fileSearch:T_BackgroundFileSearch;
  public
    PROPERTY getSelectedFile:T_arrayOfString read selectedFile;
    FUNCTION showForRoot(CONST rootPath:string):longint;
    FUNCTION showClassicDialog:longint;
  end;

FUNCTION openFileDialog:TopenFileDialog;
IMPLEMENTATION
VAR myOpenFileDialog: TopenFileDialog=nil;
FUNCTION openFileDialog:TopenFileDialog;
  begin
    if myOpenFileDialog=nil then
      myOpenFileDialog:=TopenFileDialog.create(Application);
    result:=myOpenFileDialog;
  end;

{$R *.lfm}

{ T_BackgroundFileSearch }

CONSTRUCTOR T_BackgroundFileSearch.create(CONST rootPath: string);
  VAR i:longint;
  begin
    initCriticalSection(searchCS);
    workspace.fileHistory.getClonedFileItems(found);
    for i:=0 to length(found)-1 do found[i]:=collapseMnhDir(found[i]);

    setLength(foldersScanned,0);
    foldersToScan:=rootPath;
    append(foldersToScan,configDir);
    append(foldersToScan,extractFilePath(paramStr(0)));
    append(foldersToScan,workspace.fileHistory.folderItems);
    inherited create(false);
    FreeOnTerminate := false;
    Priority:=tpNormal;
  end;

DESTRUCTOR T_BackgroundFileSearch.destroy;
  begin
    doneCriticalSection(searchCS);
    inherited destroy;
  end;

PROCEDURE T_BackgroundFileSearch.execute;
  PROCEDURE scanPath(CONST path:ansistring);
    VAR info: TSearchRec;
        scriptsFound:T_arrayOfString;
    begin
      if arrContains(foldersScanned,path)
      then exit
      else append(foldersScanned,path);

      setLength(scriptsFound,0);

      if (findFirst(path+'*'+SCRIPT_EXTENSION, faAnyFile and not(faDirectory), info) = 0)
      then begin
        repeat
          if ((info.Attr and faDirectory)<>faDirectory) then
          append(scriptsFound,collapseMnhDir(path+info.name));
        until (findNext(info)<>0) ;
      end;
      sysutils.findClose(info);

      enterCriticalSection(searchCS);
      append(found,scriptsFound);
      leaveCriticalSection(searchCS);

      setLength(scriptsFound,0);

      if findFirst(path+'*', faAnyFile, info) = 0
      then repeat
        if ((info.Attr and faDirectory)=faDirectory) and (info.name<>'.') and (info.name<>'..')
        then append(foldersToScan,path+info.name+DirectorySeparator);
      until (findNext(info)<>0);
      sysutils.findClose(info);
    end;

  VAR i:longint=0;
  begin
    while (i<length(foldersToScan)) and not(Terminated) do begin
      scanPath(foldersToScan[i]);
      inc(i);
    end;
    setLength(foldersScanned,0);
    setLength(foldersToScan,0);
    enterCriticalSection(searchCS);
    sortUnique(found);
    leaveCriticalSection(searchCS);
    Terminate;
  end;

PROCEDURE T_BackgroundFileSearch.getFilteredWords(CONST filter: string; CONST items: TStrings);
  VAR tempList:T_arrayOfString;
      s:string;
      counter:longint=0;
  begin
    enterCriticalSection(searchCS);
    sortUnique(found);
    tempList:=getListOfSimilarWords(filter,found,100,true);
    leaveCriticalSection(searchCS);

    items.clear;
    for s in tempList do if counter<100 then begin
      items.add(s);
      inc(counter);
    end;
  end;

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
  end;

PROCEDURE TopenFileDialog.FormCreate(Sender: TObject);
  begin
    fileSearch:=nil;
  end;

PROCEDURE TopenFileDialog.FormDestroy(Sender: TObject);
  begin
    if Assigned(fileSearch) then FreeAndNil(fileSearch);
  end;

PROCEDURE TopenFileDialog.searchEditChange(Sender: TObject);
  VAR oldItemIndex:longint;
  begin
    searchResultsListBox.items.BeginUpdate;
    oldItemIndex:=searchResultsListBox.ItemIndex;
    fileSearch.getFilteredWords(searchEdit.text,searchResultsListBox.items);
    if oldItemIndex<searchResultsListBox.items.count then searchResultsListBox.ItemIndex:=oldItemIndex;
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
      FreeAndNil(fileSearch);
      ModalResult:=mrOk;
    end;
  end;

PROCEDURE TopenFileDialog.searchResultsListBoxKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key=#13) and (searchResultsListBox.ItemIndex>=0) and (searchResultsListBox.ItemIndex<searchResultsListBox.items.count)
    then begin
      selectedFile:=expandMnhDir(searchResultsListBox.items[searchResultsListBox.ItemIndex]);
      FreeAndNil(fileSearch);
      ModalResult:=mrOk;
    end;
  end;

PROCEDURE TopenFileDialog.Timer1Timer(Sender: TObject);
  begin
    if Assigned(fileSearch) then searchEditChange(Sender);
  end;

FUNCTION TopenFileDialog.showForRoot(CONST rootPath: string): longint;
  begin
    searchEdit.text:='';
    if fileSearch<>nil then FreeAndNil(fileSearch);
    fileSearch:=T_BackgroundFileSearch.create(rootPath);

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

