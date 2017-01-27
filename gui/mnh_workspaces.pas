UNIT mnh_workspaces;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, mnh_settings;

TYPE

  { TworkspacesForm }

  TworkspacesForm = class(TForm)
    deleteButton: TButton;
    renameButton: TButton;
    exportWorkspaceButton: TButton;
    GroupBox1: TGroupBox;
    newWorkspaceButton: TButton;
    importWorkspaceButton: TButton;
    OpenDialog: TOpenDialog;
    Panel2: TPanel;
    SaveDialog: TSaveDialog;
    workspaceNameEdit: TEdit;
    workspaceListBox: TListBox;
    Panel1: TPanel;
    PROCEDURE deleteButtonClick(Sender: TObject);
    PROCEDURE exportWorkspaceButtonClick(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE importWorkspaceButtonClick(Sender: TObject);
    PROCEDURE newWorkspaceButtonClick(Sender: TObject);
    PROCEDURE renameButtonClick(Sender: TObject);
    PROCEDURE workspaceListBoxDblClick(Sender: TObject);
  private
    available:T_workspaceMetaArray;
    currentWorkspaceIndex:longint;
    implicitWorkspaceChange:boolean;
    FUNCTION niceName(CONST index:longint):string;
    { private declarations }
  public
    { public declarations }
  end;

FUNCTION switchWorkspace:boolean;
IMPLEMENTATION
VAR
  workspacesForm: TworkspacesForm=nil;

FUNCTION switchWorkspace:boolean;
  begin
    if workspacesForm=nil then workspacesForm:=TworkspacesForm.create(nil);
    workspacesForm.implicitWorkspaceChange:=false;
    result:=(workspacesForm.ShowModal=mrOk) or workspacesForm.implicitWorkspaceChange;
  end;

{$R *.lfm}

PROCEDURE TworkspacesForm.FormShow(Sender: TObject);
  VAR i:longint;
  begin
    currentWorkspaceIndex:=-1;
    available:=avaliableWorkspaces;
    workspaceListBox.items.clear;
    for i:=0 to length(available)-1 do
    if available[i].fileName=settings.value^.currentWorkspaceFilename then begin
      currentWorkspaceIndex:=i;
      workspaceListBox.items.add('[ '+niceName(i)+' ]')
    end else workspaceListBox.items.add(niceName(i));
    exportWorkspaceButton.caption:='export current';
    importWorkspaceButton.caption:='import to current';
    workspaceListBox.ItemIndex:=currentWorkspaceIndex;
    workspaceNameEdit.text:='';
  end;

PROCEDURE TworkspacesForm.exportWorkspaceButtonClick(Sender: TObject);
  begin
    if SaveDialog.execute then settings.value^.exportWorkspace(SaveDialog.fileName);
  end;

PROCEDURE TworkspacesForm.deleteButtonClick(Sender: TObject);
  begin
    if (workspaceListBox.ItemIndex>=0) and (workspaceListBox.ItemIndex<=length(available)) then begin
      implicitWorkspaceChange:=settings.value^.deleteWorkspace(available[workspaceListBox.ItemIndex]);
      FormShow(Sender);
    end;
  end;

PROCEDURE TworkspacesForm.importWorkspaceButtonClick(Sender: TObject);
  begin
    if OpenDialog.execute and settings.value^.importWorkspace(OpenDialog.fileName) then ModalResult:=mrOk;
  end;

PROCEDURE TworkspacesForm.newWorkspaceButtonClick(Sender: TObject);
  begin
    settings.value^.createNewWorkspace;
    settings.value^.workspace.name:=workspaceNameEdit.text;
    ModalResult:=mrOk;
  end;

FUNCTION TworkspacesForm.niceName(CONST index:longint):string;
  begin
    result:=StringReplace(extractFileExt(available[index].fileName),'.','',[])+': '+available[index].name;
  end;

PROCEDURE TworkspacesForm.renameButtonClick(Sender: TObject);
  begin
    settings.value^.workspace.name:=workspaceNameEdit.text;
    available[currentWorkspaceIndex].name:=workspaceNameEdit.text;
    workspaceListBox.items[currentWorkspaceIndex]:=('[ '+niceName(currentWorkspaceIndex)+' ]');
  end;

PROCEDURE TworkspacesForm.workspaceListBoxDblClick(Sender: TObject);
  begin
    if workspaceListBox.ItemIndex=currentWorkspaceIndex then ModalResult:=mrCancel
    else begin
      settings.value^.switchWorkspace(available[workspaceListBox.ItemIndex].fileName);
      ModalResult:=mrOk;
    end;
  end;

FINALIZATION
  if workspacesForm<>nil then FreeAndNil(workspacesForm);

end.

