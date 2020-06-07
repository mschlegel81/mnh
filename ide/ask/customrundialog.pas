UNIT customRunDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn,mnh_settings, editorMeta,commandLineParameters,
  serializationUtil,myGenerics;

TYPE
  T_runParameterHistory=object(T_serializable)
    historyPerScript:array of record
      scriptName:string;
      parameterHistory:T_arrayOfString;
    end;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    FUNCTION getParameterHistory(CONST scriptName:string):T_arrayOfString;
    PROCEDURE storeUsedParameter(CONST scriptName,parameters:string);

    //From T_serializable:
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

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
VAR runParameterHistory:T_runParameterHistory;
IMPLEMENTATION
USES editorMetaBase,mnh_constants;
VAR myCustomRunForm:TCustomRunForm=nil;

FUNCTION showCustomRunForm(CONST externalRun:boolean):boolean;
  CONST formCaption:array[false..true] of string=('Run','Run externally');
  VAR meta:P_editorMeta;
      executable:boolean;
      fileHadShebang:boolean=false;
      scriptName:string;
      previousParameters:string;
  begin
    meta:=workspace.currentEditor;
    scriptName:=meta^.pseudoName();
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit(false);
    if myCustomRunForm=nil then myCustomRunForm:=TCustomRunForm.create(Application);
    with myCustomRunForm.scriptParamEdit do begin
      items.clear;
      for previousParameters in runParameterHistory.getParameterHistory(scriptName) do items.add(previousParameters);
      text:='';
    end;

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
      runParameterHistory.storeUsedParameter(scriptName,myCustomRunForm.scriptParamEdit.text);
      if not(myCustomRunForm.flagsByShebangCb.enabled and myCustomRunForm.flagsByShebangCb.checked) then
        runnerModel.persistentRunOptions.mnhExecutionOptions.copyFrom(runnerModel.externalRun.mnhExecutionOptions);
    end else begin
      result:=false;
      myCustomRunForm.scriptParamEdit.text:='';
    end;
  end;

{$R *.lfm}

{ T_runParameterHistory }

CONSTRUCTOR T_runParameterHistory.create;
  begin
    setLength(historyPerScript,0);
  end;

DESTRUCTOR T_runParameterHistory.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(historyPerScript)-1 do begin
      setLength(historyPerScript[i].parameterHistory,0);
      historyPerScript[i].scriptName:='';
    end;
    setLength(historyPerScript,0);
  end;

FUNCTION T_runParameterHistory.getParameterHistory(CONST scriptName: string): T_arrayOfString;
  VAR i:longint;
      p:string;
  begin
    setLength(result,0);
    for i:=0 to length(historyPerScript)-1 do
      if historyPerScript[i].scriptName=scriptName
      then append(result,historyPerScript[i].parameterHistory);
    //Also append all parameters from other scripts...
    for i:=0 to length(historyPerScript)-1 do
      if historyPerScript[i].scriptName<>scriptName
      then for p in historyPerScript[i].parameterHistory do appendIfNew(result,p);
  end;

PROCEDURE T_runParameterHistory.storeUsedParameter(CONST scriptName, parameters: string);
  VAR i:longint=0;
  begin
    while (i<length(historyPerScript)) and (historyPerScript[i].scriptName<>scriptName) do inc(i);
    if i=length(historyPerScript) then begin
      setLength(historyPerScript,i+1);
      historyPerScript[i].scriptName:=scriptName;
      historyPerScript[i].parameterHistory:=parameters;
    end else with historyPerScript[i] do begin
      dropValues(parameterHistory,parameters);
      prepend   (parameterHistory,parameters);
    end;
  end;

FUNCTION T_runParameterHistory.getSerialVersion: dword;
  begin
    result:=482;
  end;

FUNCTION T_runParameterHistory.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i,j:longint;
  begin
    result:=inherited loadFromStream(stream);
    if result then begin
      setLength(historyPerScript,stream.readNaturalNumber);
      for i:=0 to length(historyPerScript)-1 do with historyPerScript[i] do begin
        scriptName:=stream.readAnsiString;
        setLength(parameterHistory,stream.readNaturalNumber);
        for j:=0 to length(parameterHistory)-1 do parameterHistory[j]:=stream.readAnsiString;
      end;
      result:=stream.allOkay;
    end;
    if not(result) then begin
      for i:=0 to length(historyPerScript)-1 do begin
        setLength(historyPerScript[i].parameterHistory,0);
        historyPerScript[i].scriptName:='';
      end;
      setLength(historyPerScript,0);
    end;
  end;

PROCEDURE T_runParameterHistory.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
      s:string;
      toPersist:T_arrayOfLongint;
  begin
    setLength(toPersist,0);
    //Save only parameters associated with existent files
    for i:=0 to length(historyPerScript)-1 do if fileExists(historyPerScript[i].scriptName) then append(toPersist,i);

    inherited saveToStream(stream);
    stream.writeNaturalNumber(length(toPersist));
    for i in toPersist do with historyPerScript[i] do begin
      stream.writeAnsiString(scriptName);
      stream.writeNaturalNumber(length(parameterHistory));
      for s in parameterHistory do stream.writeAnsiString(s);
    end;
  end;

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

