UNIT helperForms;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Dialogs, StdCtrls, Menus, ExtCtrls, SynEdit,
  ideLayoutUtil, SynHighlighterMnh, editorMeta, mnh_settings, Classes, mnh_doc,
  tokenArray,basicTypes;

TYPE

  { THelpForm }

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
    PROCEDURE SynEdit1MouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE UpdateToggleBoxChange(Sender: TObject);
    PROCEDURE dockChanged; override;
  private
    currentLink:string;
    lineLocations:T_searchTokenLocations;
    PROCEDURE toggleUpdate(CONST force:boolean=false; CONST enable:boolean=false);
    PROCEDURE openLocationForLine(CONST lineIndex:longint);
  public

  end;

PROCEDURE ensureHelpForm;
IMPLEMENTATION
USES editorMetaBase, lclintf,ComCtrls,Graphics,mnh_messages,myStringUtil,myGenerics,messageFormatting;
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

PROCEDURE THelpForm.performSlowUpdate(CONST isEvaluationRunning: boolean);
  begin

  end;

PROCEDURE THelpForm.performFastUpdate;
  CONST noLocation:T_searchTokenLocation=(fileName: ''; line:-1; column: -1);
  VAR meta:P_editorMeta;
      info:T_tokenInfo;

  PROCEDURE appendLAL(CONST line:string; CONST location:T_searchTokenLocation);
    begin
      SynEdit1.append(line);
      setLength(lineLocations,SynEdit1.lines.count);
      lineLocations[length(lineLocations)-1]:=location;
    end;

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
      appendLAL(separatorLine,noLocation);
      appendLAL(SECTION_MARKER+headerText,noLocation);
      appendLAL(separatorLine,noLocation);
    end;

  PROCEDURE addSubrulesSection;
    VAR ruleInfo:T_structuredRuleInfo;
        line:string;
    begin
      if length(info.userDefRuleInfo)<=0 then exit;
      writeSectionHeader('Subrules:');

      for ruleInfo in info.userDefRuleInfo do begin
        for line in split(ruleInfo.comment,C_lineBreakChar) do appendLAL(line,ruleInfo.location);
        appendLAL(ECHO_MARKER+copy(ruleInfo.idAndSignature+'->'+ruleInfo.body,1,SynEdit1.charsInWindow-2),ruleInfo.location);
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
       for line in split(ruleInfo.comment,C_lineBreakChar) do appendLAL(ECHO_MARKER+'//'+line,noLocation);
       appendLAL(ECHO_MARKER+ruleInfo.idAndSignature,noLocation);
     end;
    end;

  PROCEDURE addReferencedSection;
    VAR loc:T_searchTokenLocation;
        first:boolean=true;
        source:T_messagesAndLocations;
        i:longint;
    begin
      if length(info.referencedAt)=0 then exit;
      writeSectionHeader('References:');
      for loc in info.referencedAt do begin
        source:=workspace.getSourceLine(loc,1,2);
        if source.size>0 then begin
          if first then first:=false else appendLAL('',noLocation);
          appendLAL(loc,loc);
          for i:=0 to source.size do appendLAL(ECHO_MARKER+source.text(i),source.location(i));
        end;
        source.destroy;
      end;
    end;

  PROCEDURE addExamplesSection;
    VAR line:string;
    begin
      if length(info.exampleText)=0 then exit;
      writeSectionHeader('Examples:');
      for line in info.exampleText do appendLAL(line,noLocation);
    end;

  begin
    if not(isActiveInTabSheet and UpdateToggleBox.checked) or SynEdit1.Focused then exit;
    meta:=workspace.currentEditor;
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit;
    if meta^.setUnderCursor(false,true)
    then begin
      BeginFormUpdate;
      SynEdit1.clearAll;
      setLength(lineLocations,0);
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

PROCEDURE THelpForm.SynEdit1KeyUp(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    tabNextKeyHandling(Sender,key,Shift);
    if (key=13) and (ssCtrl in Shift) then begin
      openLocationForLine(SynEdit1.CaretY-1);
      key:=0;
    end;
  end;

PROCEDURE THelpForm.SynEdit1MouseDown(Sender: TObject; button: TMouseButton;Shift: TShiftState; X, Y: integer);
  begin
    if (ssCtrl in Shift) and (button=mbLeft) then begin
      openLocationForLine(SynEdit1.PixelsToRowColumn(point(x,y)).Y-1);
    end;
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

PROCEDURE THelpForm.openLocationForLine(CONST lineIndex: longint);
  begin
    if (lineIndex<0) or (lineIndex>=length(lineLocations)) then exit;
    workspace.openLocation(lineLocations[lineIndex]);
  end;

end.

