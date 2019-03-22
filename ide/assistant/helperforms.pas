UNIT helperForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, SynEdit,ideLayoutUtil,SynHighlighterMnh,
  editorMeta,mnh_settings;

TYPE

  { THelpForm }

  THelpForm = class(T_mnhComponentForm)
    SynEdit1: TSynEdit;
    highlighter:TSynMnhSyn;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private

  public

  end;

PROCEDURE ensureHelpForm;
IMPLEMENTATION

PROCEDURE ensureHelpForm;
  begin
    if not(hasFormOfType(icHelp,true)) then dockNewForm(THelpForm.create(Application));
  end;

{$R *.lfm}

PROCEDURE THelpForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(SynEdit1,ctEditor);
    highlighter.create(self,msf_help);
    SynEdit1.highlighter:=highlighter;
  end;

PROCEDURE THelpForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(SynEdit1);
  end;

FUNCTION THelpForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icHelp;
  end;

PROCEDURE THelpForm.performSlowUpdate;
  begin
    SynEdit1.text:=getHelpText;
  end;

PROCEDURE THelpForm.performFastUpdate;
  begin

  end;

end.

