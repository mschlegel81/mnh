UNIT customRunDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn,mnh_settings;

TYPE
  TCustomRunForm = class(TForm)
    Button1: TButton;
    RunButton: TButton;
    guiFlagCb: TCheckBox;
    quietFlagCb: TCheckBox;
    silentFlagCb: TCheckBox;
    headlessFlagCb: TCheckBox;
    profileFlagCb: TCheckBox;
    DirectoryEdit: TDirectoryEdit;
    verbosityEdit: TEdit;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    fullVersionRb: TRadioButton;
    lightVersionRb: TRadioButton;
    scriptLocRb: TRadioButton;
    customLocRb: TRadioButton;
    scriptParamEdit: TEdit;
    GroupBox1: TGroupBox;
    PROCEDURE customLocRbChange(Sender: TObject);
    PROCEDURE DirectoryEditChange(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE fullVersionRbChange(Sender: TObject);
    PROCEDURE guiFlagCbChange(Sender: TObject);
    PROCEDURE headlessFlagCbChange(Sender: TObject);
    PROCEDURE profileFlagCbChange(Sender: TObject);
    PROCEDURE quietFlagCbChange(Sender: TObject);
    PROCEDURE scriptParamEditKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE silentFlagCbChange(Sender: TObject);
    PROCEDURE verbosityEditChange(Sender: TObject);
  private

  public

  end;

FUNCTION customRunForm(CONST externalRun:boolean):TCustomRunForm;

IMPLEMENTATION
VAR myCustomRunForm:TCustomRunForm=nil;

FUNCTION customRunForm(CONST externalRun:boolean):TCustomRunForm;
  begin
    if myCustomRunForm=nil then begin
      myCustomRunForm:=TCustomRunForm.create(nil);
    end;
    result:=myCustomRunForm;
    result.GroupBox2.enabled:=externalRun;
    result.GroupBox3.enabled:=externalRun;
    result.GroupBox4.enabled:=externalRun;
  end;

{$R *.lfm}
PROCEDURE TCustomRunForm.FormCreate(Sender: TObject);
  begin
    with settings.externalRunOptions do begin
      if not(fileExists(settings.lightFlavourLocation)) then begin
        callLightFlavour:=false;
        lightVersionRb.enabled:=false;
      end;
      if callLightFlavour
      then lightVersionRb.checked:=true
      else fullVersionRb.checked :=true;
      guiFlagCb     .checked:=clf_GUI in flags;
      quietFlagCb   .checked:=clf_QUIET in flags;
      silentFlagCb  .checked:=clf_SILENT in flags;
      headlessFlagCb.checked:=clf_HEADLESS in flags;
      profileFlagCb .checked:=clf_PROFILE in flags;
      verbosityEdit.text:=verbosity;
      if customFolder='' then begin
        scriptLocRb.checked:=true;
        DirectoryEdit.enabled:=false;
      end else begin
        customLocRb.checked:=true;
        DirectoryEdit.enabled:=true;
        DirectoryEdit.caption:=customFolder;
      end;
    end;
  end;

PROCEDURE TCustomRunForm.customLocRbChange(Sender: TObject);
  begin
    DirectoryEdit.enabled:=customLocRb.checked;
    if not(customLocRb.checked) then settings.externalRunOptions.customFolder:='';
  end;

PROCEDURE TCustomRunForm.DirectoryEditChange(Sender: TObject);
  begin
    settings.externalRunOptions.customFolder:=DirectoryEdit.text;
  end;

PROCEDURE TCustomRunForm.fullVersionRbChange(Sender: TObject);
  begin
    settings.externalRunOptions.callLightFlavour:=lightVersionRb.checked;
    if settings.externalRunOptions.callLightFlavour then begin
      guiFlagCb.checked:=false;
      profileFlagCb.checked:=false;
      settings.externalRunOptions.flags:=settings.externalRunOptions.flags-[clf_GUI,clf_PROFILE];
    end;
  end;

PROCEDURE TCustomRunForm.guiFlagCbChange(Sender: TObject);
  begin
    if guiFlagCb.checked then begin
      include(settings.externalRunOptions.flags,clf_GUI);
      settings.externalRunOptions.callLightFlavour:=false;
      fullVersionRb.checked:=true;
    end else exclude(settings.externalRunOptions.flags,clf_GUI);
  end;

PROCEDURE TCustomRunForm.profileFlagCbChange(Sender: TObject);
  begin
    if profileFlagCb.checked then begin
      include(settings.externalRunOptions.flags,clf_PROFILE);
      exclude(settings.externalRunOptions.flags,clf_QUIET);
      quietFlagCb.checked:=false;
      settings.externalRunOptions.callLightFlavour:=false;
      fullVersionRb.checked:=true;
    end else exclude(settings.externalRunOptions.flags,clf_PROFILE);
  end;

PROCEDURE TCustomRunForm.headlessFlagCbChange(Sender: TObject);
  begin
    if headlessFlagCb.checked
    then include(settings.externalRunOptions.flags,clf_HEADLESS)
    else exclude(settings.externalRunOptions.flags,clf_HEADLESS);
  end;

PROCEDURE TCustomRunForm.quietFlagCbChange(Sender: TObject);
  begin
    if headlessFlagCb.checked
    then begin
      include(settings.externalRunOptions.flags,clf_QUIET);
      exclude(settings.externalRunOptions.flags,clf_PROFILE);
      profileFlagCb.checked:=false;
    end else exclude(settings.externalRunOptions.flags,clf_QUIET);
  end;

PROCEDURE TCustomRunForm.scriptParamEditKeyPress(Sender: TObject; VAR key: char);
  begin
    if key=#13 then ModalResult:=mrOk;
  end;

PROCEDURE TCustomRunForm.silentFlagCbChange(Sender: TObject);
  begin
    if headlessFlagCb.checked
    then include(settings.externalRunOptions.flags,clf_SILENT)
    else exclude(settings.externalRunOptions.flags,clf_SILENT);
  end;

PROCEDURE TCustomRunForm.verbosityEditChange(Sender: TObject);
  begin
    settings.externalRunOptions.verbosity:=verbosityEdit.text;
  end;

FINALIZATION
  if myCustomRunForm<>nil then FreeAndNil(myCustomRunForm);

end.

