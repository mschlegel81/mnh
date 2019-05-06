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
    UpdateToggleBox: TToggleBox;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE openHtmlButtonClick(Sender: TObject);
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE UpdateToggleBoxChange(Sender: TObject);
  private
    currentLink:string;
    PROCEDURE toggleUpdate(CONST force:boolean=false; CONST enable:boolean=false);
  public

  end;

PROCEDURE ensureHelpForm;
IMPLEMENTATION
USES editorMetaBase, lclintf,ComCtrls,Graphics;

PROCEDURE ensureHelpForm;
  VAR helperForm:T_mnhComponentForm;
      page:TTabSheet;
      PageControl:TPageControl;
  begin
    helperForm:=getFormOfType(icHelp);
    if helperForm=nil then dockNewForm(THelpForm.create(Application))
    else begin
      helperForm.getParents(page,PageControl);
      if (PageControl= nil) and (helperForm.Focused) or
         (PageControl<>nil) and (PageControl.activePage=page)
      then THelpForm(helperForm).toggleUpdate()
      else PageControl.activePage:=page;
    end;
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
  begin

  end;

PROCEDURE THelpForm.performFastUpdate;
  VAR meta:P_editorMeta;
  begin
    if not(showing and UpdateToggleBox.checked) then exit;
    meta:=workspace.currentEditor;
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit;
    meta^.setUnderCursor(false,true);
    SynEdit1.text:=getHelpText(currentLink);
  end;

PROCEDURE THelpForm.UpdateToggleBoxChange(Sender: TObject);
  begin
    toggleUpdate(true,UpdateToggleBox.checked);
  end;

PROCEDURE THelpForm.toggleUpdate(CONST force: boolean; CONST enable: boolean);
  begin
    if force
    then UpdateToggleBox.checked:=enable
    else UpdateToggleBox.checked:=not(UpdateToggleBox.checked);
    if UpdateToggleBox.checked
    then SynEdit1.color:=clWhite
    else SynEdit1.color:=CL_INACTIVE_GREY;
  end;

end.

