UNIT saveFile;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  mnh_constants,
  editorMeta,editorMetaBase;

TYPE

  { TSaveFileDialog }

  TSaveFileDialog = class(TForm)
    Button1: TButton;
    CancelButton: TButton;
    extEdit: TComboBox;
    dirComboBox: TComboBox;
    nameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveDialog1: TSaveDialog;
    PROCEDURE Button1Click(Sender: TObject);
    PROCEDURE dirComboBoxKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE FormShow(Sender: TObject);
  private
    selectedFile:string;
  public
    PROPERTY getSelectedFile:string read selectedFile;
    FUNCTION showForRoot(CONST rootPath,fname,ext: string):string;
  end;

FUNCTION saveFileDialog:TSaveFileDialog;
IMPLEMENTATION
VAR mySaveFileDialog: TSaveFileDialog=nil;
FUNCTION saveFileDialog:TSaveFileDialog;
  begin
    if mySaveFileDialog=nil then
      mySaveFileDialog:=TSaveFileDialog.create(nil);
    result:=mySaveFileDialog;
  end;

{$R *.lfm}
PROCEDURE TSaveFileDialog.Button1Click(Sender: TObject);
  begin
    if SaveDialog1.execute then begin
      ModalResult:=mrOk;
      selectedFile:=SaveDialog1.fileName;
    end else ModalResult:=mrCancel;
  end;

PROCEDURE TSaveFileDialog.dirComboBoxKeyPress(Sender: TObject; VAR key: char);
  begin
    if key=#13 then begin
      selectedFile:=dirComboBox.text+DirectorySeparator+nameEdit.text+extEdit.text;
      ModalResult:=mrOk;
    end;
  end;

PROCEDURE TSaveFileDialog.FormShow(Sender: TObject);
  begin
    nameEdit.SetFocus;
  end;

FUNCTION TSaveFileDialog.showForRoot(CONST rootPath,fname,ext: string): string;
  PROCEDURE ensureExtensions;
    VAR lang:T_language;
        s:string;
    begin
      if extEdit.items.count=0 then for lang in T_language do with fileTypeMeta[lang] do begin
        for s in extensions do extEdit.items.add('.'+lowercase(s));
      end;
    end;

  VAR s:string;
  begin
    nameEdit.caption:=fname;
    //extension
    ensureExtensions;
    extEdit.text:=ext;
    //directory
    dirComboBox.items.clear;
    dirComboBox.items.add(rootPath);
    dirComboBox.items.add(configDir+'packages');
    dirComboBox.items.add(configDir+'demos');
    for s in fileHistory.recentFolders do dirComboBox.items.add(s);
    dirComboBox.ItemIndex:=0;
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
    selectedFile:='';
    if ShowModal=mrOk then result:=selectedFile else result:='';
  end;

FUNCTION saveFile(CONST rootPath,fname,ext: string): string;
  begin
    writeln('Calling saveFile ',rootPath,'/',fname,'/',ext,'; initialized=',mySaveFileDialog<>nil);
    result:=saveFileDialog.showForRoot(rootPath,fname,ext);
  end;

INITIALIZATION
  editorMeta.safeCallback:=@saveFile;
FINALIZATION
  if mySaveFileDialog<>nil then FreeAndNil(mySaveFileDialog);
end.

