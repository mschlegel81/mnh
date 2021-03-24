UNIT customRunDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ExtCtrls,mnh_settings, editorMeta,commandLineParameters,
  serializationUtil,myGenerics,cmdLineFrames;

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
    bottomPanel: TPanel;
    topPanel: TPanel;
    scriptParamEdit: TComboBox;
    RunButton: TButton;
    DirectoryEdit: TDirectoryEdit;
    GroupBox3: TGroupBox;
    scriptLocRb: TRadioButton;
    customLocRb: TRadioButton;
    GroupBox1: TGroupBox;
    PROCEDURE customLocRbChange(Sender: TObject);
    PROCEDURE DirectoryEditChange(Sender: TObject);
    PROCEDURE flagsByShebangCbChange(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE scriptParamEditKeyPress(Sender: TObject; VAR key: char);
  private
    parametersFromShebang:T_mnhExecutionOptions;
  public

  end;

FUNCTION showCustomRunForm(CONST externalRun:boolean):boolean;
VAR runParameterHistory:T_runParameterHistory;
IMPLEMENTATION
USES editorMetaBase,mnh_constants,closeDialog,mnh_messages,codeAssistance;
VAR myCustomRunForm:TCustomRunForm=nil;

FUNCTION showCustomRunForm(CONST externalRun:boolean):boolean;
  CONST formCaption:array[false..true] of string=('Run','Run externally');
  VAR meta:P_editorMeta;
      executable:boolean;
      fileHadShebang:boolean=false;
      scriptName:string;
      previousParameters:string;
      assistanceData:P_codeAssistanceResponse;
  begin
    meta:=workspace.currentEditor;
    scriptName:=meta^.pseudoName();

    assistanceData:=meta^.getAssistanceResponse;
    if (meta=nil) or (meta^.language<>LANG_MNH) or (assistanceData=nil) or not(assistanceData^.isExecutablePackage) then begin
      closeDialogForm.showOnExecute(scriptName,externalRun,'it is not an executable package.');
      disposeMessage(assistanceData);
      exit(false);
    end else disposeMessage(assistanceData);

    if meta^.isPseudoFile and externalRun then begin
      closeDialogForm.showOnExecute(scriptName,true,'has never been saved.');
      exit(false);
    end;

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

    if externalRun then begin
      myCustomRunForm.AutoSize:=false;
      if fileHadShebang and runnerModel.persistentRunOptions.preferShebang
      then runnerModel.externalRun.mnhExecutionOptions.copyFrom(myCustomRunForm.parametersFromShebang)
      else runnerModel.externalRun.mnhExecutionOptions.copyFrom(runnerModel.persistentRunOptions.mnhExecutionOptions);
      getCmdLineParametersFrameInstance(myCustomRunForm,false,scriptName,@runnerModel.externalRun);
      myCustomRunForm.bottomPanel.Align:=alBottom;
      if myCustomRunForm.height<500 then myCustomRunForm.height:=500;
    end else begin
      detachCmdLineParametersFrameInstance;
      myCustomRunForm.bottomPanel.Align:=alClient;
      myCustomRunForm.AutoSize:=true;
    end;
    if myCustomRunForm.ShowModal=mrOk then begin
      result:=true;
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
    initialize(result);
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
    if trim(parameters)='' then exit;
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
      toPersist:T_arrayOfLongint=();
  begin
    //Save only parameters associated with existent files
    for i:=0 to length(historyPerScript)-1 do if fileExists(historyPerScript[i].scriptName) then append(toPersist,i);

    inherited saveToStream(stream);
    stream.writeNaturalNumber(length(toPersist));
    for i in toPersist do with historyPerScript[i] do begin
      stream.writeAnsiString(scriptName);
      dropValues(parameterHistory,'');
      stream.writeNaturalNumber(length(parameterHistory));
      for s in parameterHistory do stream.writeAnsiString(s);
    end;
  end;

PROCEDURE TCustomRunForm.FormCreate(Sender: TObject);
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
  end;

PROCEDURE TCustomRunForm.FormShow(Sender: TObject);
  begin
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
    if flagsByShebangCb.enabled and flagsByShebangCb.checked
    then runnerModel.externalRun.mnhExecutionOptions.copyFrom(parametersFromShebang)
    else runnerModel.externalRun.mnhExecutionOptions.copyFrom(runnerModel.persistentRunOptions.mnhExecutionOptions);
    getCmdLineParametersFrameInstance(myCustomRunForm,false,workspace.currentEditor^.pseudoName(),@runnerModel.externalRun);
  end;

PROCEDURE TCustomRunForm.scriptParamEditKeyPress(Sender: TObject; VAR key: char);
  begin
    if key=#13 then ModalResult:=mrOk;
  end;

end.

