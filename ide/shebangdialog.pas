UNIT shebangDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, editorMeta;

TYPE
  TShebangWizard = class(TForm)
    Button1: TButton;
    Button2: TButton;
    doLogCheckbox: TCheckBox;
    logNameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    logAppendCb: TCheckBox;
    GroupBox4: TGroupBox;
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
USES mnh_settings,commandLineParameters,funcs,codeAssistance,out_adapters,editorMetaBase;
{$R *.lfm}
VAR ShebangWizard:TShebangWizard=nil;

PROCEDURE showShebangWizard(CONST meta:P_editorMeta);
  VAR clp:T_commandLineParameters;
      requires:T_specialFunctionRequirements;
      hadShebang:boolean=false;
      assistanceData:P_codeAssistanceResponse;
      fileName,options:string;
  begin
    if meta^.language<>LANG_MNH then exit;
    if ShebangWizard=nil then ShebangWizard:=TShebangWizard.create(Application);
    assistanceData:=meta^.getCodeAssistanceDataRereferenced;
    requires:=assistanceData^.getBuiltinRestrictions;
    disposeCodeAssistanceResponse(assistanceData);

    if (meta^.editor.lines.count>0) then begin
      clp.initFromShebang(meta^.editor.lines[0],requires);
      hadShebang:=true;
    end else clp.clear;

    with ShebangWizard do begin
      lightVersionRb.checked:=(clp.executor=settings.lightFlavourLocation);
      fullVersionRb .checked:=not(lightVersionRb.checked);

      guiFlagCb.checked:=clp.reEvaluationWithGUIrequired;
      silentFlagCb.checked:=clp.suppressBeep;
      quietFlagCb.checked:=not(clp.wantConsoleAdapter);
      headlessFlagCb.checked:=clp.headless;
      pauseFlagCb.checked:=clp.pauseAtEnd;
      pauseOnErrorCb.checked:=clp.pauseOnError;

      verbosityCombo.text:=clp.verbosityString;

      if length(clp.deferredAdapterCreations)>1 then begin
        doLogCheckbox.checked:=true;
        logAppendCb.enabled:=true;
        logAppendCb.checked:=clp.deferredAdapterCreations[0].appending;
        logNameEdit.enabled:=true;
        splitIntoLogNameAndOption(clp.deferredAdapterCreations[0].nameAndOption,fileName,options);
        logNameEdit.text:=fileName;
        verbosityCombo1.enabled:=true;
        verbosityCombo1.text:=options;
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
        clp.reEvaluationWithGUIrequired:=guiFlagCb.checked;
        clp.suppressBeep:=silentFlagCb.checked;
        clp.wantConsoleAdapter:=not(quietFlagCb.checked);
        clp.headless:=headlessFlagCb.checked;
        clp.pauseAtEnd:=pauseFlagCb.checked;
        clp.pauseOnError:=pauseOnErrorCb.checked;
        clp.verbosityString:=verbosityCombo.text;
        if doLogCheckbox.checked then begin
          setLength(clp.deferredAdapterCreations,1);
          clp.deferredAdapterCreations[0].appending:=logAppendCb.checked;
          clp.deferredAdapterCreations[0].nameAndOption:=logNameEdit.text;
          if verbosityCombo1.text<>''
          then clp.deferredAdapterCreations[0].nameAndOption+='('+verbosityCombo1.text+')';
        end else setLength(clp.deferredAdapterCreations,0);

        if hadShebang
        then meta^.editor.lines[0]:=clp.getShebang
        else meta^.editor.lines.Insert(0,clp.getShebang);
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
  begin
    guiFlagCb.caption:=FLAG_GUI;
    quietFlagCb.caption:=FLAG_QUIET;
    silentFlagCb.caption:=FLAG_SILENT;
    pauseFlagCb.caption:=FLAG_PAUSE_ALWAYS;
    headlessFlagCb.caption:=FLAG_HEADLESS;
    pauseOnErrorCb.caption:=FLAG_PAUSE_ON_ERR;
  end;

PROCEDURE TShebangWizard.doLogCheckboxClick(Sender: TObject);
  begin
    logAppendCb    .enabled:=doLogCheckbox.checked;
    logNameEdit    .enabled:=doLogCheckbox.checked;
    verbosityCombo1.enabled:=doLogCheckbox.checked;
  end;

end.

