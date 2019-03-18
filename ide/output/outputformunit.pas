UNIT outputFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, SynEdit,
  SynHighlighterMnh,ideLayoutUtil,guiOutAdapters;

TYPE

  { TOutputForm }

  TOutputForm = class(T_mnhComponentForm)
    OutputSynEdit: TSynEdit;
    outputHighlighter:TSynMnhSyn;
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private

  public
  end;

PROCEDURE ensureOutputForm;
IMPLEMENTATION

PROCEDURE ensureOutputForm;
  begin
    if not(hasFormOfType(icOutput,true))
    then dockNewForm(TOutputForm.create(Application));
  end;

{$R *.lfm}

PROCEDURE TOutputForm.FormCreate(Sender: TObject);
  begin
    registerSynEdit(OutputSynEdit);
    outputHighlighter:=TSynMnhSyn.create(self,msf_output);
    OutputSynEdit.highlighter:=outputHighlighter;
  end;

PROCEDURE TOutputForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    CanClose:=false;
  end;

FUNCTION TOutputForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icOutput;
  end;

PROCEDURE TOutputForm.performSlowUpdate;
  begin

  end;

PROCEDURE TOutputForm.performFastUpdate;
  begin
    guiOutAdapter.flushToGui(true);
  end;

end.

