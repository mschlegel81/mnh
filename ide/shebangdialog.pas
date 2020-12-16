UNIT shebangDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  editorMeta, cmdLineFrames;

TYPE

  { TShebangWizard }

  TShebangWizard = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CmdLineParametersFrame: TCmdLineParametersFrame;
    Label3: TLabel;
    considerErrorsLabel: TLabel;
    notExecutableHint: TPanel;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

PROCEDURE showShebangWizard(CONST meta:P_editorMeta);
IMPLEMENTATION
USES mnh_settings,commandLineParameters,funcs,codeAssistance,out_adapters,editorMetaBase,mnh_constants;
{$R *.lfm}
VAR ShebangWizard:TShebangWizard=nil;

PROCEDURE showShebangWizard(CONST meta:P_editorMeta);
  VAR hadShebang,isExecutable:boolean;
  begin
    if meta^.language<>LANG_MNH then exit;
    if ShebangWizard=nil then ShebangWizard:=TShebangWizard.create(Application);
    with ShebangWizard do begin
      CmdLineParametersFrame.initFromExecOptions(meta^.getParametersFromShebang(hadShebang,isExecutable));
      notExecutableHint.visible:=not(isExecutable);
      if ShowModal=mrOk then begin
        if hadShebang
        then meta^.editor.SetTextBetweenPoints(point(1,1),point(1,2),CmdLineParametersFrame.execOptions.getShebang+LineEnding)
        else meta^.editor.SetTextBetweenPoints(point(1,1),point(1,1),CmdLineParametersFrame.execOptions.getShebang+LineEnding);
      end;
    end;
  end;

{ TShebangWizard }

procedure TShebangWizard.FormCreate(Sender: TObject);
  begin
    CmdLineParametersFrame:=TCmdLineParametersFrame.Create(self);
  end;


end.

