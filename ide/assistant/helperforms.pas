UNIT helperForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Dialogs, StdCtrls, Menus, SynEdit, ideLayoutUtil,
  SynHighlighterMnh, editorMeta, mnh_settings, Classes,mnh_doc;

TYPE
  THelpForm = class(T_mnhComponentForm)
    MainMenu1: TMainMenu;
    openHtmlButton: TButton;
    PopupMenu1: TPopupMenu;
    SynEdit1: TSynEdit;
    helpHighlighter:TMnhOutputSyn;
    UpdateToggleBox: TToggleBox;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE openHtmlButtonClick(Sender: TObject);
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE SynEdit1KeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE UpdateToggleBoxChange(Sender: TObject);
    PROCEDURE dockChanged; override;
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
      previouslyActiveControl:TWinControl;
  begin
    previouslyActiveControl:=mainForm.ActiveControl;
    helperForm:=getFormOfType(icHelp);
    if helperForm=nil then begin
      dockNewForm(THelpForm.create(Application));
      mainForm.ActiveControl:=previouslyActiveControl;
    end else begin
      helperForm.getParents(page,PageControl);
      if (PageControl= nil) and (helperForm.Focused) or
         (PageControl<>nil) and (PageControl.activePage=page)
      then THelpForm(helperForm).toggleUpdate()
      else begin
        if PageControl<>nil
        then PageControl.activePage:=page
        else helperForm.showComponent(true);
      end;
    end;
  end;

{$R *.lfm}

PROCEDURE THelpForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(SynEdit1,ctEditor);
    helpHighlighter:=TMnhOutputSyn.create(self);
    SynEdit1.highlighter:=helpHighlighter;
    SynEdit1.OnMouseDown:=@workspace.mouseDownForJumpToLocation;
    currentLink:=getDocIndexLinkForBrowser;
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
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

PROCEDURE THelpForm.performSlowUpdate(CONST isEvaluationRunning:boolean);
  begin

  end;

PROCEDURE THelpForm.performFastUpdate;
  VAR meta:P_editorMeta;
  begin
    if not(showing and UpdateToggleBox.checked) or SynEdit1.Focused then exit;
    meta:=workspace.currentEditor;
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit;
    if meta^.setUnderCursor(false,true)
    then SynEdit1.text:=getHelpText(currentLink);
  end;

PROCEDURE THelpForm.SynEdit1KeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    workspace.keyUpForJumpToLocation(Sender,key,Shift);
    tabNextKeyHandling(Sender,key,Shift);
  end;

PROCEDURE THelpForm.UpdateToggleBoxChange(Sender: TObject);
  begin
    toggleUpdate(true,UpdateToggleBox.checked);
  end;

PROCEDURE THelpForm.dockChanged;
  begin
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

