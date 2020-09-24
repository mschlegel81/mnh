UNIT helperForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Dialogs, StdCtrls, Menus, ExtCtrls, SynEdit,
  ideLayoutUtil, SynHighlighterMnh, editorMeta, mnh_settings, Classes, mnh_doc,
  tokenArray,basicTypes;

TYPE
  THelpForm = class(T_mnhComponentForm)
    builtinGroupBox: TGroupBox;
    examplesGroupBox: TGroupBox;
    referenceListBox: TListBox;
    Panel1: TPanel;
    examplesSynEdit: TSynEdit;
    ScrollBox1: TScrollBox;
    usedAtGroupBox: TGroupBox;
    subrulesGroupBox: TGroupBox;
    shortInfoLabel: TLabel;
    shortInfoGroupBox: TGroupBox;
    tokenLabel: TLabel;
    MainMenu1: TMainMenu;
    openHtmlButton: TButton;
    PopupMenu1: TPopupMenu;
    helpHighlighter:TMnhOutputSyn;
    UpdateToggleBox: TToggleBox;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE openHtmlButtonClick(Sender: TObject);
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE referenceListBoxKeyDown(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE SynEdit1KeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE UpdateToggleBoxChange(Sender: TObject);
    PROCEDURE dockChanged; override;
    PROCEDURE locationLabelDblClick(Sender: TObject);
  private
    temporaryComponents:array of TControl;
    currentLink:string;
    tempLocations:array of T_searchTokenLocation;
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
      try
      mainForm.ActiveControl:=previouslyActiveControl;
      except
      end;
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
    caption:=getCaption;
    registerFontControl(examplesSynEdit,ctEditor);
    registerFontControl(self,ctGeneral);
    helpHighlighter:=TMnhOutputSyn.create(self);
    examplesSynEdit.highlighter:=helpHighlighter;
    currentLink:=getDocIndexLinkForBrowser;
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
    setLength(temporaryComponents,0);
  end;

PROCEDURE THelpForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(examplesSynEdit);
    unregisterFontControl(self);
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
      info:T_tokenInfo;
      ruleInfo:T_structuredRuleInfo;
      textLine:string;
      i:longint;

  PROCEDURE clearTemporaryComponents;
    VAR c:TComponent;
    begin
      for c in temporaryComponents do c.free;
      setLength(temporaryComponents,0);
      setLength(tempLocations,0);
    end;

  FUNCTION getNewLabel(CONST italic:boolean; CONST parent:TWinControl):TLabel;
    VAR c       :TControl;
        neighbor:TControl=nil;
    begin
      for c in temporaryComponents do if c.parent=parent then neighbor:=c;
      result:=TLabel.create(self);
      setLength(temporaryComponents,length(temporaryComponents)+1);
      temporaryComponents[length(temporaryComponents)-1]:=result;
      if italic then begin
        result.Font:=Font;
        result.Font.style:=[fsItalic];
      end;
      result.parent:=parent;
      if neighbor<>nil then begin
        result.Align:=alCustom;
        result.AnchorToNeighbour(akTop ,0,neighbor);
        result.AnchorParallel   (akLeft,0,neighbor);
      end else result.Align:=alTop;
    end;

  PROCEDURE addRuleInfo(CONST r:T_structuredRuleInfo; CONST box:TWinControl);
    VAR lab:TLabel;
    begin
      if length(r.comment)>0 then getNewLabel(true,box).caption:=r.comment;
      if length(r.idAndSignature)>0 then begin
        lab:=getNewLabel(false,box);
        if length(r.body)>0
        then lab.caption:=r.idAndSignature+'->'+r.body
        else lab.caption:=r.idAndSignature;

        if r.location<>C_nilTokenLocation then begin
          lab.OnClick:=@locationLabelDblClick;
          lab.Tag:=length(tempLocations);
          setLength(tempLocations,length(tempLocations)+1);
          tempLocations[length(tempLocations)-1]:=r.location;
        end;
      end;
    end;

  begin
    if not(showing and UpdateToggleBox.checked) or examplesSynEdit.Focused then exit;
    meta:=workspace.currentEditor;
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit;
    if meta^.setUnderCursor(false,true)
    then begin
      BeginFormUpdate;
      clearTemporaryComponents;
      info :=getCurrentTokenInfo;
      tokenLabel.caption:='Token: '+info.tokenText;
      shortInfoGroupBox.visible:=info.shortInfo<>'';
      shortInfoLabel.caption:=info.shortInfo;

      subrulesGroupBox.visible:=length(info.userDefRuleInfo)>0;
      for ruleInfo in info.userDefRuleInfo do addRuleInfo(ruleInfo,subrulesGroupBox);

      builtinGroupBox.visible:=length(info.builtinRuleInfo)>0;
      for ruleInfo in info.builtinRuleInfo do addRuleInfo(ruleInfo,builtinGroupBox);
      builtinGroupBox.Constraints.MaxHeight:=ClientHeight;

      if (length(info.userDefRuleInfo)>0) and (length(info.builtinRuleInfo)>0)
      then builtinGroupBox.caption:='Overloads builtin function'
      else builtinGroupBox.caption:='Builtin function';

      examplesGroupBox.visible:=length(info.exampleText)>0;
      examplesSynEdit.lines.clear;
      for textline in info.exampleText do examplesSynEdit.lines.append(textLine);

      usedAtGroupBox.visible:=length(info.referencedAt)>0;
      referenceListBox.items.clear;
      for i:=0 to length(info.referencedAt)-1 do referenceListBox.items.append(info.referencedAt[i]);
      EndFormUpdate;
    end;
  end;

PROCEDURE THelpForm.referenceListBoxKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if key=13 then begin
      workspace.openLocation(guessLocationFromString(referenceListBox.items[referenceListBox.ItemIndex],false));
    end;
  end;

PROCEDURE THelpForm.locationLabelDblClick(Sender: TObject);
  begin
    if (TComponent(Sender).Tag>=0) and (TComponent(Sender).Tag<length(tempLocations))
    then workspace.openLocation(tempLocations[TComponent(Sender).Tag]);
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
  end;

end.

