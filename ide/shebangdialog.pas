UNIT shebangDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  editorMeta;

TYPE
  TShebangWizard = class(TForm)
    Button1: TButton;
    Button2: TButton;
    sideEffectComboBox: TComboBox;
    doLogCheckbox: TCheckBox;
    restrictionsGroupBox: TGroupBox;
    Label3: TLabel;
    considerErrorsLabel: TLabel;
    logNameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    logAppendCb: TCheckBox;
    GroupBox4: TGroupBox;
    notExecutableHint: TPanel;
    pauseFlagCb: TCheckBox;
    pauseOnErrorCb: TCheckBox;
    verbosityCombo: TComboBox;
    GroupBox3: TGroupBox;
    quietFlagCb: TCheckBox;
    silentFlagCb: TCheckBox;
    headlessFlagCb: TCheckBox;
    guiFlagCb: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lightVersionRb: TRadioButton;
    fullVersionRb: TRadioButton;
    verbosityCombo1: TComboBox;
    PROCEDURE doLogCheckboxClick(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE guiFlagCbClick(Sender: TObject);
    PROCEDURE lightVersionRbClick(Sender: TObject);
    PROCEDURE pauseFlagCbClick(Sender: TObject);
  private
  public
  end;

PROCEDURE showShebangWizard(CONST meta:P_editorMeta);
IMPLEMENTATION
USES mnh_settings,commandLineParameters,funcs,codeAssistance,out_adapters,editorMetaBase,mnh_constants;
{$R *.lfm}
VAR ShebangWizard:TShebangWizard=nil;

PROCEDURE showShebangWizard(CONST meta:P_editorMeta);
  VAR clp:T_mnhExecutionOptions;
      hadShebang,isExecutable:boolean;
  begin
    if meta^.language<>LANG_MNH then exit;
    clp:=meta^.getParametersFromShebang(hadShebang,isExecutable);

    if ShebangWizard=nil then ShebangWizard:=TShebangWizard.create(Application);
    with ShebangWizard do begin
      notExecutableHint.visible:=not(isExecutable);

      lightVersionRb.checked:=(clp.executor=settings.lightFlavourLocation);
      fullVersionRb .checked:=not(lightVersionRb.checked);

      guiFlagCb     .checked:=clf_GUI in clp.flags;
      silentFlagCb  .checked:=clf_SILENT in clp.flags;
      quietFlagCb   .checked:=clf_QUIET in clp.flags;
      headlessFlagCb.checked:=clf_HEADLESS in clp.flags;
      pauseFlagCb   .checked:=clf_PAUSE_ALWAYS in clp.flags;
      pauseOnErrorCb.checked:=clf_PAUSE_ON_ERR in clp.flags;

      verbosityCombo.text:=clp.verbosityString;
      sideEffectComboBox.ItemIndex:=clp.sideEffectProfile;

      if length(clp.deferredAdapterCreations)>0 then begin
        doLogCheckbox.checked:=true;
        logAppendCb.enabled:=true;
        logAppendCb.checked:=not(clp.deferredAdapterCreations[0].forceNewFile);
        logNameEdit.enabled:=true;
        logNameEdit.text:=clp.deferredAdapterCreations[0].getFilename;
        verbosityCombo1.enabled:=true;
        verbosityCombo1.text:=clp.deferredAdapterCreations[0].getVerbosityPart;
      end else begin
        doLogCheckbox.checked:=false;
        logAppendCb.enabled:=false;
        logNameEdit.enabled:=false;
        verbosityCombo1.enabled:=false;
        //defaults:
        logNameEdit.caption:='?.log';
        verbosityCombo1.caption:='';
        logAppendCb.checked:=false;
      end;
      if ShowModal=mrOk then begin
        if lightVersionRb.checked
        then clp.executor:=settings.lightFlavourLocation
        else clp.executor:=settings.fullFlavourLocation;
        if guiFlagCb     .checked then include(clp.flags,clf_GUI) else Exclude(clp.flags,clf_GUI);
        if silentFlagCb  .checked then include(clp.flags,clf_SILENT) else Exclude(clp.flags,clf_SILENT);
        if quietFlagCb   .checked then include(clp.flags,clf_QUIET) else Exclude(clp.flags,clf_QUIET);
        if headlessFlagCb.checked then include(clp.flags,clf_HEADLESS) else Exclude(clp.flags,clf_HEADLESS);
        if pauseFlagCb   .checked then include(clp.flags,clf_PAUSE_ALWAYS) else Exclude(clp.flags,clf_PAUSE_ALWAYS);
        if pauseOnErrorCb.checked then include(clp.flags,clf_PAUSE_ON_ERR) else Exclude(clp.flags,clf_PAUSE_ON_ERR);
        clp.sideEffectProfile:=sideEffectComboBox.ItemIndex;
        clp.verbosityString:=verbosityCombo.text;
        if doLogCheckbox.checked then begin
          setLength(clp.deferredAdapterCreations,1);
          clp.deferredAdapterCreations[0].forceNewFile :=not(logAppendCb.checked);
          clp.deferredAdapterCreations[0].fileName     :=logNameEdit.text;
          clp.deferredAdapterCreations[0].setVerbosityPart(verbosityCombo1.text,C_defaultOutputBehavior);
        end else setLength(clp.deferredAdapterCreations,0);
        if hadShebang
        then meta^.editor.SetTextBetweenPoints(point(1,1),point(1,2),clp.getShebang+LineEnding)
        else meta^.editor.SetTextBetweenPoints(point(1,1),point(1,1),clp.getShebang+LineEnding);
      end;
    end;
  end;

PROCEDURE TShebangWizard.FormShow(Sender: TObject);
  begin
    lightVersionRb.caption:=settings.lightFlavourLocation;
    fullVersionRb.caption:=settings.fullFlavourLocation;
  end;

PROCEDURE TShebangWizard.guiFlagCbClick(Sender: TObject);
  begin
    if guiFlagCb.checked then fullVersionRb.checked:=true;
  end;

PROCEDURE TShebangWizard.lightVersionRbClick(Sender: TObject);
  begin
    if lightVersionRb.checked then guiFlagCb.checked:=false;
  end;

PROCEDURE TShebangWizard.pauseFlagCbClick(Sender: TObject);
  begin
    if pauseFlagCb.checked then pauseOnErrorCb.checked:=false;
  end;

PROCEDURE TShebangWizard.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    guiFlagCb.caption:=FLAG_GUI;
    quietFlagCb.caption:=FLAG_QUIET;
    silentFlagCb.caption:=FLAG_SILENT;
    pauseFlagCb.caption:=FLAG_PAUSE_ALWAYS;
    headlessFlagCb.caption:=FLAG_HEADLESS;
    pauseOnErrorCb.caption:=FLAG_PAUSE_ON_ERR;
    sideEffectComboBox.items.clear;
    for i:=0 to length(C_sideEffectProfile)-1 do sideEffectComboBox.items.add(C_sideEffectProfile[i].name);
  end;

PROCEDURE TShebangWizard.doLogCheckboxClick(Sender: TObject);
  begin
    logAppendCb    .enabled:=doLogCheckbox.checked;
    logNameEdit    .enabled:=doLogCheckbox.checked;
    verbosityCombo1.enabled:=doLogCheckbox.checked;
  end;

end.

