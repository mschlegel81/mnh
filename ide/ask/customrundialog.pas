UNIT customRunDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn,mnh_settings, editorMeta,commandLineParameters;

TYPE

  { TCustomRunForm }

  TCustomRunForm = class(TForm)
    Button1: TButton;
    flagsByShebangCb: TCheckBox;
    restrictionsGroupBox: TGroupBox;
    scriptParamEdit: TComboBox;
    RunButton: TButton;
    guiFlagCb: TCheckBox;
    quietFlagCb: TCheckBox;
    sideEffectComboBox: TComboBox;
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
    GroupBox1: TGroupBox;
    PROCEDURE customLocRbChange(Sender: TObject);
    PROCEDURE DirectoryEditChange(Sender: TObject);
    PROCEDURE flagsByShebangCbChange(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE fullVersionRbChange(Sender: TObject);
    PROCEDURE guiFlagCbChange(Sender: TObject);
    PROCEDURE headlessFlagCbChange(Sender: TObject);
    PROCEDURE profileFlagCbChange(Sender: TObject);
    PROCEDURE quietFlagCbChange(Sender: TObject);
    PROCEDURE scriptParamEditKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE silentFlagCbChange(Sender: TObject);
    PROCEDURE verbosityEditChange(Sender: TObject);
  private
    parametersFromShebang:T_mnhExecutionOptions;
    PROCEDURE reinitialize;
  public

  end;

FUNCTION showCustomRunForm(CONST externalRun:boolean):boolean;

IMPLEMENTATION
USES editorMetaBase,mnh_constants;
VAR myCustomRunForm:TCustomRunForm=nil;

FUNCTION showCustomRunForm(CONST externalRun:boolean):boolean;
  //TODO: Use restriction profile settings
  CONST formCaption:array[false..true] of string=('Run','Run externally');
  VAR k:longint;
      meta:P_editorMeta;
      executable:boolean;
      fileHadShebang:boolean=false;
      scriptName:string;
  begin
    meta:=workspace.currentEditor;
    scriptName:=meta^.pseudoName();
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit(false);
    if myCustomRunForm=nil then myCustomRunForm:=TCustomRunForm.create(Application);
    myCustomRunForm.caption:=formCaption[externalRun];
    if externalRun then begin
      myCustomRunForm.parametersFromShebang:=meta^.getParametersFromShebang(fileHadShebang,executable);
    end;
    myCustomRunForm.flagsByShebangCb.visible:=externalRun;
    myCustomRunForm.flagsByShebangCb.enabled:=fileHadShebang;
    myCustomRunForm.flagsByShebangCb.checked:=runnerModel.persistentRunOptions.preferShebang;
    myCustomRunForm.sideEffectComboBox.ItemIndex:=runnerModel.persistentRunOptions.mnhExecutionOptions.sideEffectProfile;
    myCustomRunForm.GroupBox2.enabled:=externalRun;
    myCustomRunForm.GroupBox4.enabled:=externalRun;
    myCustomRunForm.restrictionsGroupBox.enabled:=externalRun;
    myCustomRunForm.GroupBox2.visible:=externalRun;
    myCustomRunForm.GroupBox4.visible:=externalRun;
    myCustomRunForm.restrictionsGroupBox.visible:=externalRun;
    if externalRun then myCustomRunForm.reinitialize;
    if myCustomRunForm.ShowModal=mrOk then begin
      result:=true;
      runnerModel.externalRun.mnhExecutionOptions.sideEffectProfile:=myCustomRunForm.sideEffectComboBox.ItemIndex;
      runnerModel.externalRun.initFromIde(scriptName,myCustomRunForm.scriptParamEdit.text);
      with myCustomRunForm.scriptParamEdit do if text<>'' then begin
        k:=items.IndexOf(text);
        if k>=0 then items.delete(k);
        items.Insert(0,text);
        text:='';
      end;
      if not(myCustomRunForm.flagsByShebangCb.enabled and myCustomRunForm.flagsByShebangCb.checked) then
        runnerModel.persistentRunOptions.mnhExecutionOptions.copyFrom(runnerModel.externalRun.mnhExecutionOptions);
    end else begin
      result:=false;
      myCustomRunForm.scriptParamEdit.text:='';
    end;
  end;

{$R *.lfm}

PROCEDURE TCustomRunForm.reinitialize;
  VAR useParametersFromShebang:boolean;
  begin
    runnerModel.externalRun.clear;
    useParametersFromShebang:=flagsByShebangCb.enabled and flagsByShebangCb.checked;
    if useParametersFromShebang
    then runnerModel.externalRun.mnhExecutionOptions.copyFrom(parametersFromShebang)
    else runnerModel.externalRun.mnhExecutionOptions.copyFrom(runnerModel.persistentRunOptions.mnhExecutionOptions);

    with runnerModel.externalRun.mnhExecutionOptions do begin
      include(flags,clf_PAUSE_ALWAYS);
      guiFlagCb     .checked:=clf_GUI      in flags;
      quietFlagCb   .checked:=clf_QUIET    in flags;
      silentFlagCb  .checked:=clf_SILENT   in flags;
      headlessFlagCb.checked:=clf_HEADLESS in flags;
      profileFlagCb .checked:=clf_PROFILE  in flags;
      verbosityEdit .text:=verbosityString;
      sideEffectComboBox.ItemIndex:=sideEffectProfile;
    end;
    guiFlagCb         .enabled:=not(useParametersFromShebang);
    quietFlagCb       .enabled:=not(useParametersFromShebang);
    silentFlagCb      .enabled:=not(useParametersFromShebang);
    headlessFlagCb    .enabled:=not(useParametersFromShebang);
    profileFlagCb     .enabled:=not(useParametersFromShebang);
    verbosityEdit     .enabled:=not(useParametersFromShebang);
    sideEffectComboBox.enabled:=not(useParametersFromShebang);
    if not(fileExists(settings.lightFlavourLocation)) then begin
      runnerModel.externalRun         .mnhExecutionOptions.callLightFlavour:=false;
      runnerModel.persistentRunOptions.mnhExecutionOptions.callLightFlavour:=false;
      lightVersionRb.enabled:=false;
    end;
    if runnerModel.externalRun.mnhExecutionOptions.callLightFlavour
    then lightVersionRb.checked:=true
    else fullVersionRb.checked :=true;

    lightVersionRb.enabled:=lightVersionRb.enabled and not(useParametersFromShebang);
    fullVersionRb .enabled:=                           not(useParametersFromShebang);
  end;

PROCEDURE TCustomRunForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    with runnerModel.persistentRunOptions do begin
      if customFolder='' then begin
        scriptLocRb.checked:=true;
        DirectoryEdit.enabled:=false;
      end else begin
        customLocRb.checked:=true;
        DirectoryEdit.enabled:=true;
        DirectoryEdit.caption:=customFolder;
      end;
    end;
    sideEffectComboBox.items.clear;
    for i:=0 to length(C_sideEffectProfile)-1 do sideEffectComboBox.items.add(C_sideEffectProfile[i].name);
  end;

PROCEDURE TCustomRunForm.FormShow(Sender: TObject);
  begin
    reinitialize;
  end;

PROCEDURE TCustomRunForm.customLocRbChange(Sender: TObject);
  begin
    DirectoryEdit.enabled:=customLocRb.checked;
    if not(customLocRb.checked) then runnerModel.persistentRunOptions.customFolder:='';
  end;

PROCEDURE TCustomRunForm.DirectoryEditChange(Sender: TObject);
  begin
    runnerModel.persistentRunOptions.customFolder:=DirectoryEdit.text;
  end;

PROCEDURE TCustomRunForm.flagsByShebangCbChange(Sender: TObject);
  begin
    runnerModel.persistentRunOptions.preferShebang:=flagsByShebangCb.checked;
    reinitialize;
  end;

PROCEDURE TCustomRunForm.fullVersionRbChange(Sender: TObject);
  begin
    runnerModel.externalRun.mnhExecutionOptions.callLightFlavour:=lightVersionRb.checked;
    if runnerModel.externalRun.mnhExecutionOptions.callLightFlavour then begin
      guiFlagCb.checked:=false;
      profileFlagCb.checked:=false;
      runnerModel.externalRun.mnhExecutionOptions.flags:=runnerModel.externalRun.mnhExecutionOptions.flags-[clf_GUI,clf_PROFILE];
    end;
  end;

PROCEDURE TCustomRunForm.guiFlagCbChange(Sender: TObject);
  begin
    if guiFlagCb.checked then begin
      include(runnerModel.externalRun.mnhExecutionOptions.flags,clf_GUI);
      runnerModel.externalRun.mnhExecutionOptions.callLightFlavour:=false;
      fullVersionRb.checked:=true;
    end else Exclude(runnerModel.externalRun.mnhExecutionOptions.flags,clf_GUI);
  end;

PROCEDURE TCustomRunForm.profileFlagCbChange(Sender: TObject);
  begin
    if profileFlagCb.checked then begin
      include(runnerModel.externalRun.mnhExecutionOptions.flags,clf_PROFILE);
      Exclude(runnerModel.externalRun.mnhExecutionOptions.flags,clf_QUIET);
      quietFlagCb.checked:=false;
      runnerModel.externalRun.mnhExecutionOptions.callLightFlavour:=false;
      fullVersionRb.checked:=true;
    end else Exclude(runnerModel.externalRun.mnhExecutionOptions.flags,clf_PROFILE);
  end;

PROCEDURE TCustomRunForm.headlessFlagCbChange(Sender: TObject);
  begin
    if headlessFlagCb.checked
    then include(runnerModel.externalRun.mnhExecutionOptions.flags,clf_HEADLESS)
    else Exclude(runnerModel.externalRun.mnhExecutionOptions.flags,clf_HEADLESS);
  end;

PROCEDURE TCustomRunForm.quietFlagCbChange(Sender: TObject);
  begin
    if headlessFlagCb.checked
    then begin
      include(runnerModel.externalRun.mnhExecutionOptions.flags,clf_QUIET);
      Exclude(runnerModel.externalRun.mnhExecutionOptions.flags,clf_PROFILE);
      profileFlagCb.checked:=false;
    end else Exclude(runnerModel.externalRun.mnhExecutionOptions.flags,clf_QUIET);
  end;

PROCEDURE TCustomRunForm.scriptParamEditKeyPress(Sender: TObject; VAR key: char
  );
  begin
    if key=#13 then ModalResult:=mrOk;
  end;

PROCEDURE TCustomRunForm.silentFlagCbChange(Sender: TObject);
  begin
    if headlessFlagCb.checked
    then include(runnerModel.externalRun.mnhExecutionOptions.flags,clf_SILENT)
    else Exclude(runnerModel.externalRun.mnhExecutionOptions.flags,clf_SILENT);
  end;

PROCEDURE TCustomRunForm.verbosityEditChange(Sender: TObject);
  begin
    runnerModel.externalRun.mnhExecutionOptions.verbosityString:=verbosityEdit.text;
  end;

end.

