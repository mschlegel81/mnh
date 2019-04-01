UNIT gotoLineDialogs;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  editorMeta,SynEdit;

TYPE

  { TGotoLineDialog }

  TGotoLineDialog = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    lineNumberEdit: TEdit;
    PROCEDURE lineNumberEditKeyPress(Sender: TObject; VAR key: char);
  private

  public

  end;

PROCEDURE gotoLineByDialog(CONST editor:TSynEdit);
IMPLEMENTATION
VAR gotoLineDialog:TGotoLineDialog=nil;

PROCEDURE gotoLineByDialog(CONST editor:TSynEdit);
  VAR LineNumber:longint;
  begin
    if editor=nil then exit;
    if gotoLineDialog=nil then gotoLineDialog:=TGotoLineDialog.create(Application);
    gotoLineDialog.lineNumberEdit.text:='';
    gotoLineDialog.lineNumberEdit.maxLength:=length(intToStr(editor.lines.count));
    if gotoLineDialog.ShowModal=mrOk then begin
      LineNumber:=strToIntDef(gotoLineDialog.lineNumberEdit.text,-1);
      if (LineNumber>0) and (LineNumber<=editor.lines.count)
      then begin
        editor.CaretX:=1;
        editor.CaretY:=LineNumber;
      end;
    end;
  end;

{$R *.lfm}

{ TGotoLineDialog }

PROCEDURE TGotoLineDialog.lineNumberEditKeyPress(Sender: TObject; VAR key: char);
begin
  if key=#13 then ModalResult:=mrOk;
end;

end.

