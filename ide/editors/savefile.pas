UNIT saveFile;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  mnh_constants,
  editorMeta,editorMetaBase,closeDialog;

CONST
  CAPTION_IF_FILE_EXISTS:array[false..true] of string=('Save File','Overwrite File');

TYPE
  TSaveFileDialog = class(TForm)
    Button1: TButton;
    CancelButton: TButton;
    saveToPackagesCb: TCheckBox;
    extEdit: TComboBox;
    dirComboBox: TComboBox;
    nameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    PROCEDURE Button1Click(Sender: TObject);
    PROCEDURE dirComboBoxChange(Sender: TObject);
    PROCEDURE dirComboBoxKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE extEditChange(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE nameEditChange(Sender: TObject);
    PROCEDURE saveToPackagesCbChange(Sender: TObject);
  private
    selectedFile:string;
    PROCEDURE setSelectedFile(CONST f:string);
  public
    PROPERTY getSelectedFile:string read selectedFile;
    FUNCTION showForRoot(CONST rootPath,fname,ext: string):string;
  end;

FUNCTION saveFileDialog:TSaveFileDialog;
IMPLEMENTATION
USES fileWrappers;
VAR mySaveFileDialog: TSaveFileDialog=nil;
FUNCTION saveFileDialog:TSaveFileDialog;
  begin
    if mySaveFileDialog=nil then
      mySaveFileDialog:=TSaveFileDialog.create(Application);
    result:=mySaveFileDialog;
  end;

{$R *.lfm}
PROCEDURE TSaveFileDialog.Button1Click(Sender: TObject);
  begin
    if SaveDialog1.execute then begin
      selectedFile:=SaveDialog1.fileName;
      ModalResult:=mrOk;
    end else ModalResult:=mrCancel;
  end;

PROCEDURE TSaveFileDialog.dirComboBoxChange(Sender: TObject);
  begin
    setSelectedFile(expandMnhDir(dirComboBox.text)+DirectorySeparator+nameEdit.text+extEdit.text);
  end;

PROCEDURE TSaveFileDialog.dirComboBoxKeyPress(Sender: TObject; VAR key: char);
  begin
    if key=#13 then begin
      setSelectedFile(expandMnhDir(dirComboBox.text)+DirectorySeparator+nameEdit.text+extEdit.text);
      if fileExists(selectedFile)
      then case closeDialogForm.showOnOverwrite(selectedFile) of
        cda_overwrite  : ModalResult:=mrOk;
        cda_cancel     : ModalResult:=mrCancel;
      end else ModalResult:=mrOk;
    end;
  end;

PROCEDURE TSaveFileDialog.extEditChange(Sender: TObject);
  begin
    setSelectedFile(expandMnhDir(dirComboBox.text)+DirectorySeparator+nameEdit.text+extEdit.text);
  end;

PROCEDURE TSaveFileDialog.FormShow(Sender: TObject);
  begin
    nameEdit.SetFocus;
    AdjustSize;
  end;

PROCEDURE TSaveFileDialog.nameEditChange(Sender: TObject);
  begin
    setSelectedFile(expandMnhDir(dirComboBox.text)+DirectorySeparator+nameEdit.text+extEdit.text);
  end;

PROCEDURE TSaveFileDialog.saveToPackagesCbChange(Sender: TObject);
  begin
    if saveToPackagesCb.checked then begin
      dirComboBox.enabled:=false;
      dirComboBox.text:=collapseMnhDir(configDir+'packages');
      setSelectedFile(configDir+'packages'+DirectorySeparator+nameEdit.text+extEdit.text);
    end else begin
      dirComboBox.enabled:=true;
    end;
  end;

PROCEDURE TSaveFileDialog.setSelectedFile(CONST f: string);
  begin
    selectedFile:=f;
    caption:=CAPTION_IF_FILE_EXISTS[fileExists(selectedFile)];
  end;

FUNCTION TSaveFileDialog.showForRoot(CONST rootPath, fname, ext: string): string;
  PROCEDURE ensureExtensions;
    VAR lang:T_language;
        s:string;
    begin
      if extEdit.items.count=0 then for lang in T_language do with fileTypeMeta[lang] do begin
        for s in extensions do extEdit.items.add('.'+lowercase(s));
      end;
    end;

  PROCEDURE addItemIfNew(CONST s:string);
    VAR toAdd:string;
    begin
      toAdd:=collapseMnhDir(s);
      if (dirComboBox.items.IndexOf(toAdd)<0) then dirComboBox.items.add(toAdd);
    end;

  VAR s:string;
      i:longint;
  begin
    nameEdit.caption:=fname;
    //extension
    ensureExtensions;
    extEdit.text:=ext;
    //directory
    dirComboBox.items.clear;
    for s in fileCache.allKnownFolders do addItemIfNew(s);
    addItemIfNew(rootPath            );
    addItemIfNew(configDir+'packages');
    addItemIfNew(configDir+'demos'   );
    //sub dialog
    if uppercase(ext)='.MNH' then begin
      SaveDialog1.FilterIndex:=0;
      SaveDialog1.options:=SaveDialog1.options+[ofExtensionDifferent];
    end else if uppercase(ext)='.HTML' then begin
      SaveDialog1.FilterIndex:=2;
      SaveDialog1.options:=SaveDialog1.options+[ofExtensionDifferent];
    end else begin
      SaveDialog1.FilterIndex:=1;
      SaveDialog1.options:=SaveDialog1.options-[ofExtensionDifferent];
    end;
    dirComboBox.text:=collapseMnhDir(rootPath);
    i:=dirComboBox.items.IndexOf(rootPath);
    if i>=0 then dirComboBox.ItemIndex:=i;
    setSelectedFile(rootPath+DirectorySeparator+fname+ext);
    if ShowModal=mrOk then result:=selectedFile else result:='';
  end;

FUNCTION saveFile(CONST rootPath,fname,ext: string): string;
  begin
    result:=saveFileDialog.showForRoot(rootPath,fname,ext);
    if result<>'' then fileCache.Invalidate(ExtractFileDir(result));
  end;

INITIALIZATION
  editorMeta.safeCallback:=@saveFile;
end.

