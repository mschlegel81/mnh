UNIT helperForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Dialogs, StdCtrls, Menus, ExtCtrls, SynEdit,
  ideLayoutUtil, SynHighlighterMnh, editorMeta, mnh_settings, Classes, mnh_doc,
  tokenArray,basicTypes;

TYPE
  THelpForm = class(T_mnhComponentForm)
    openHtmlButton: TButton;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    MainMenu1: TMainMenu;
    PopupMenu1: TPopupMenu;
    helpHighlighter:TMnhOutputSyn;
    shortInfoGroupBox: TGroupBox;
    shortInfoLabel: TLabel;
    SynEdit1: TSynEdit;
    tokenLabel: TLabel;
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
USES editorMetaBase, lclintf,ComCtrls,Graphics,mnh_messages,myStringUtil,myGenerics;
CONST H_LINE=#226#148#128;

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
    registerFontControl(SynEdit1,ctEditor);
    registerFontControl(self,ctGeneral);

    helpHighlighter:=TMnhOutputSyn.create(self);
    SynEdit1.highlighter:=helpHighlighter;
    currentLink:=getDocIndexLinkForBrowser;
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
  end;

PROCEDURE THelpForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(SynEdit1);
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

  FUNCTION isActiveInTabSheet:boolean;
    VAR
      PageControl: TPageControl;
      page: TTabSheet;
    begin
      getParents(page,PageControl);
      if (PageControl=nil) or (page=nil) then exit(showing);
      result:=PageControl.activePage=page;
    end;

  VAR separatorLine:string='';
  PROCEDURE writeSectionHeader(CONST headerText:ansistring);
    VAR i:longint;
    begin
      if separatorLine='' then begin
        for i:=1 to SynEdit1.charsInWindow-1 do separatorLine+=H_LINE;
      end;
      SynEdit1.append(separatorLine);
      SynEdit1.append(SECTION_MARKER+headerText);
      SynEdit1.append(separatorLine);
    end;

  PROCEDURE addSubrulesSection;
    VAR ruleInfo:T_structuredRuleInfo;
        line:string;
    begin
      if length(info.userDefRuleInfo)<=0 then exit;
      //Section header:
      writeSectionHeader('Subrules:');

      for ruleInfo in info.userDefRuleInfo do begin
        for line in split(ruleInfo.comment,C_lineBreakChar) do SynEdit1.append(line);
        SynEdit1.append(ruleInfo.location);
        SynEdit1.append(ECHO_MARKER+ruleInfo.idAndSignature+'->'+ruleInfo.body);
      end;
    end;

  PROCEDURE addBuiltinSection;
    VAR ruleInfo:T_structuredRuleInfo;
        line:string;

    begin
     if length(info.builtinRuleInfo)<=0 then exit;
     if (length(info.userDefRuleInfo)>0)
     then writeSectionHeader('Overloads builtin function')
     else writeSectionHeader('Builtin function');

     for ruleInfo in info.builtinRuleInfo do begin
       for line in split(ruleInfo.comment,C_lineBreakChar) do SynEdit1.append(ECHO_MARKER+'//'+line);
       SynEdit1.append(ECHO_MARKER+ruleInfo.idAndSignature);
     end;
    end;

  PROCEDURE addReferencedSection;
    VAR loc:T_searchTokenLocation;
    begin
      if length(info.referencedAt)=0 then exit;
      writeSectionHeader('References:');
      for loc in info.referencedAt do begin
        SynEdit1.append(string(loc));
        SynEdit1.append(ECHO_MARKER+workspace.getSourceLine(loc));
      end;
    end;

  PROCEDURE addExamplesSection;
    VAR line:string;
    begin
      if length(info.exampleText)=0 then exit;
      writeSectionHeader('Examples:');
      for line in info.exampleText do SynEdit1.append(line);
    end;

  begin
    if not(isActiveInTabSheet and UpdateToggleBox.checked) or SynEdit1.Focused then exit;
    meta:=workspace.currentEditor;
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit;
    if meta^.setUnderCursor(false,true)
    then begin
      BeginFormUpdate;
      SynEdit1.clearAll;
      info :=getCurrentTokenInfo;
      currentLink:=info.linkToHelp;
      tokenLabel.caption:='Token: '+info.tokenText;
      shortInfoGroupBox.visible:=info.shortInfo<>'';
      shortInfoLabel.caption:=info.shortInfo;

      addSubrulesSection;
      addBuiltinSection;
      addReferencedSection;
      addExamplesSection;
      EndFormUpdate;
    end;
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

