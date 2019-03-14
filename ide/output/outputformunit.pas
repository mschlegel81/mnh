UNIT outputFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, SynEdit,
  SynHighlighterMnh,ideLayoutUtil;

TYPE
  TOutputForm = class(T_mnhComponentForm)
    OutputSynEdit: TSynEdit;
    outputHighlighter:TSynMnhSyn;
    PROCEDURE FormCreate(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
  private

  public
  end;

IMPLEMENTATION

{$R *.lfm}

PROCEDURE TOutputForm.FormCreate(Sender: TObject);
  begin
    registerSynEdit(OutputSynEdit);
    outputHighlighter:=TSynMnhSyn.create(self,msf_output);
    OutputSynEdit.highlighter:=outputHighlighter;
  end;

FUNCTION TOutputForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icOutput;
  end;

end.

