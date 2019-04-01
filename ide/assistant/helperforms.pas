UNIT helperForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Dialogs, SynEdit,ideLayoutUtil,SynHighlighterMnh,
  editorMeta, mnh_settings;

TYPE

  { THelpForm }

  THelpForm = class(T_mnhComponentForm)
    SynEdit1: TSynEdit;
    helpHighlighter:TSynMnhSyn;
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
USES editorMetaBase;

PROCEDURE ensureHelpForm;
  begin
    if not(hasFormOfType(icHelp,true)) then dockNewForm(THelpForm.create(Application));
  end;

{$R *.lfm}

PROCEDURE THelpForm.FormCreate(Sender: TObject);
  begin
    //TODO: Can we add a link to the html documentation ?
    //Example: openUrl('file:///C:/Users/Martin%20Schlegel/AppData/Local/MNH/doc/builtin.html#math.argMin');
    registerFontControl(SynEdit1,ctEditor);
    helpHighlighter:=TSynMnhSyn.create(self,msf_help);
    SynEdit1.highlighter:=helpHighlighter;
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
  VAR meta:P_editorMeta;
  begin
    if not(showing) then exit;
    meta:=workspace.currentEditor;
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit;
    meta^.setUnderCursor(false,true);
    SynEdit1.text:=getHelpText;
  end;

PROCEDURE THelpForm.performFastUpdate;
  begin

  end;

end.

