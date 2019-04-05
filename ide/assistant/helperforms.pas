UNIT helperForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Dialogs, StdCtrls, SynEdit, ideLayoutUtil,
  SynHighlighterMnh, editorMeta, mnh_settings, Classes,mnh_doc;

TYPE
  THelpForm = class(T_mnhComponentForm)
    openHtmlButton: TButton;
    SynEdit1: TSynEdit;
    helpHighlighter:TSynMnhSyn;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE openHtmlButtonClick(Sender: TObject);
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private
    currentLink:string;

  public

  end;

PROCEDURE ensureHelpForm;
IMPLEMENTATION
USES editorMetaBase, lclintf;

PROCEDURE ensureHelpForm;
  begin
    if not(hasFormOfType(icHelp,true)) then dockNewForm(THelpForm.create(Application));
  end;

{$R *.lfm}

PROCEDURE THelpForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(SynEdit1,ctEditor);
    helpHighlighter:=TSynMnhSyn.create(self,msf_help);
    SynEdit1.highlighter:=helpHighlighter;
    SynEdit1.OnKeyUp:=@workspace.keyUpForJumpToLocation;
    SynEdit1.OnMouseDown:=@workspace.mouseDownForJumpToLocation;
    currentLink:=getDocIndexLinkForBrowser;
  end;

PROCEDURE THelpForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(SynEdit1);
  end;

FUNCTION THelpForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icHelp;
  end;

PROCEDURE THelpForm.openHtmlButtonClick(Sender: TObject);
  begin
    OpenURL(currentLink);
  end;

PROCEDURE THelpForm.performSlowUpdate;
  VAR meta:P_editorMeta;
  begin
    if not(showing) then exit;
    meta:=workspace.currentEditor;
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit;
    meta^.setUnderCursor(false,true);
    SynEdit1.text:=getHelpText(currentLink);
  end;

PROCEDURE THelpForm.performFastUpdate;
  begin

  end;

end.

