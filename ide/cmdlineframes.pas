unit cmdLineFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  commandLineParameters, mnh_settings,out_adapters,messageFormatting,mnh_constants;

type

  { TCmdLineParametersFrame }

  TCmdLineParametersFrame = class(TFrame)
    addOutFile: TButton;
    cbConsoleLikeLog: TCheckBox;
    cbConvertPrintToLog: TCheckBox;
    cbLogAppend: TCheckBox;
    forceStdErrCb: TCheckBox;
    forceStdOutCb: TCheckBox;
    outputFileComboBox: TComboBox;
    profileFlagCb: TCheckBox;
    Label3: TLabel;
    logLocationLengthEdit: TEdit;
    formatPreviewMemo: TMemo;
    timeFormatEdit: TEdit;
    GroupBox6: TGroupBox;
    Label2: TLabel;
    outFileVerbosityEdit: TEdit;
    verbosityGroupBox: TGroupBox;
    logFilenameEdit: TEdit;
    GroupBox5: TGroupBox;
    rbOutputToFile: TRadioButton;
    rbOutputToStdout: TRadioButton;
    rbOutputToStderr: TRadioButton;
    removeOutFile: TButton;
    sideEffectsComboBox: TComboBox;
    consoleVerbosityEdit: TEdit;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    quietFlagCb: TCheckBox;
    silentFlagCb: TCheckBox;
    GroupBox3: TGroupBox;
    pauseFlagCb: TCheckBox;
    pauseOnErrorFlagCb: TCheckBox;
    guiFlagCb: TCheckBox;
    headlessFlagCb: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    PageControl1: TPageControl;
    lightVersionRb: TRadioButton;
    fullVersionRb: TRadioButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure addOutFileClick(Sender: TObject);
    procedure cbConsoleLikeLogClick(Sender: TObject);
    procedure cbConvertPrintToLogClick(Sender: TObject);
    procedure cbLogAppendClick(Sender: TObject);
    procedure forceStdErrCbClick(Sender: TObject);
    procedure forceStdOutCbClick(Sender: TObject);
    procedure guiFlagCbClick(Sender: TObject);
    procedure headlessFlagCbClick(Sender: TObject);
    procedure anyPage1Change(Sender: TObject);
    procedure lightVersionRbClick(Sender: TObject);
    procedure logFilenameEditEditingDone(Sender: TObject);
    procedure logLocationLengthEditEditingDone(Sender: TObject);
    procedure outFileVerbosityEditEditingDone(Sender: TObject);
    procedure profileFlagCbClick(Sender: TObject);
    procedure rbOutputToFileClick(Sender: TObject);
    procedure rbOutputToStderrClick(Sender: TObject);
    procedure rbOutputToStdoutClick(Sender: TObject);
    procedure removeOutFileClick(Sender: TObject);
  private
    execOptions:T_mnhExecutionOptions;
    procedure updateLogPreview;
    procedure updateLogSection;
    procedure initLabels;
    function currentAdapterSpecification:P_textFileAdapterSpecification;
  public
    procedure initFromShebang(CONST s:String; CONST requires: T_sideEffects);
    procedure initFromExecOptions(CONST opt:T_mnhExecutionOptions);

  end;

implementation
USES mnh_messages,myGenerics,litVar,basicTypes,myStringUtil;
{$R *.lfm}

{ TCmdLineParametersFrame }

procedure TCmdLineParametersFrame.lightVersionRbClick(Sender: TObject);
  begin
    guiFlagCb.Checked:=false;
  end;

procedure TCmdLineParametersFrame.logFilenameEditEditingDone(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a=nil then exit;
    a^.fileName:=logFilenameEdit.Caption;
  end;

procedure TCmdLineParametersFrame.logLocationLengthEditEditingDone(Sender: TObject);
  VAR i:longint;
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a=nil then exit;
    i:=StrToIntDef(logLocationLengthEdit.Text,maxlongint);
    if i<0 then i:=MaxLongint;
    logLocationLengthEdit.Text:=IntToStr(i);
    a^.logLocationLen:=i;
    updateLogPreview;
  end;

procedure TCmdLineParametersFrame.outFileVerbosityEditEditingDone(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a<>nil then begin
      a^.setVerbosityPart(outputFileComboBox.Text,stringToMessageTypeSet(execOptions.verbosityString));
      updateLogPreview;
    end;
  end;

procedure TCmdLineParametersFrame.profileFlagCbClick(Sender: TObject);
  begin
    if profileFlagCb.Checked then fullVersionRb.Checked:=True;
  end;

procedure TCmdLineParametersFrame.rbOutputToFileClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    logFilenameEdit.Enabled:=true;
    a:=currentAdapterSpecification;
    if a<>nil then a^.textFileCase:=tfc_file;
  end;

procedure TCmdLineParametersFrame.rbOutputToStderrClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    logFilenameEdit.Enabled:=false;
    a:=currentAdapterSpecification;
    if a<>nil then a^.textFileCase:=tfc_stderr;
  end;

procedure TCmdLineParametersFrame.rbOutputToStdoutClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    logFilenameEdit.Enabled:=false;
    a:=currentAdapterSpecification;
    if a<>nil then a^.textFileCase:=tfc_stdout;
  end;

procedure TCmdLineParametersFrame.removeOutFileClick(Sender: TObject);
  begin

  end;

procedure TCmdLineParametersFrame.updateLogPreview;
  VAR specification:P_textFileAdapterSpecification;
      formatter:P_messageFormatProvider;
  FUNCTION formattedLinesFor(CONST messageType:T_messageType; CONST lineIndex:longint;
                             CONST txt1:string):T_arrayOfString;
    VAR message:T_storedMessageWithText;
        location:T_searchTokenLocation;
    begin
      location.fileName:='/home/user/scripts/myTestScript.mnh';
      location.line:=lineIndex;
      location.column:=1;
      message.create(messageType,location,split(txt1,'#'));
      result:=formatter^.formatMessage(@message);
      message.unreference;
      message.destroy;
    end;

  FUNCTION formattedLinesForOutput:T_arrayOfString;
    VAR message:T_echoOutMessage;
        location:T_searchTokenLocation;
    begin
      location.fileName:='/home/user/scripts/myTestScript.mnh';
      location.line:=2;
      location.column:=1;
      message.create(newVoidLiteral,location);
      result:=formatter^.formatMessage(@message);
      message.unreference;
      message.destroy;
    end;

  VAR s:string;
  begin
    specification:=currentAdapterSpecification;
    formatPreviewMemo.Clear;
    if specification=nil then exit;

    formatter:=getFormatterFor(specification^);

    if mt_echo_declaration in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_echo_declaration,1,'f(x)->2*x+3;') do formatPreviewMemo.Append(s);

    if mt_echo_input in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_echo_input,2,'print("f(7)=",f(7));') do formatPreviewMemo.Append(s);

    if mt_printline in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_printline,2,'f(7)=17') do formatPreviewMemo.Append(s);

    if mt_echo_output in specification^.getMessageTypes then
    for s in formattedLinesForOutput do formatPreviewMemo.Append(s);

    if mt_log in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_log,20,'This is a log message.') do formatPreviewMemo.Append(s);

    if mt_el1_userNote in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_el1_userNote,21,'This is a (user) note.') do formatPreviewMemo.Append(s);

    if mt_el1_note in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_el1_note,22,'This is a builtin note.#Like user notes it may contain multiple lines...') do formatPreviewMemo.Append(s);

    if mt_el2_userWarning in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_el2_userWarning,23,'This is a (user) warning message.') do formatPreviewMemo.Append(s);

    if mt_el2_warning in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_el2_warning,24,'This is a builtin warning message.') do formatPreviewMemo.Append(s);

    if mt_el3_evalError in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_el3_evalError,25,'This is an error!!!#An erromessage may contain multiple lines.') do formatPreviewMemo.Append(s);

    if mt_timing_info in specification^.getMessageTypes then
    for s in formattedLinesFor(mt_timing_info,0,'Importing time      802.108ms#Tokenizing time       9.126ms#Declaration time     55.315ms#Interpretation time   0.000ms#Unaccounted for       1.044ms#-----------------------------#Total               867.593ms#') do formatPreviewMemo.Append(s);
    dispose(formatter,destroy);
  end;

procedure TCmdLineParametersFrame.updateLogSection;
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    GroupBox5        .Enabled:=(a<>nil);
    verbosityGroupBox.Enabled:=(a<>nil);
    GroupBox6        .Enabled:=(a<>nil);
    if a=nil then exit;

    rbOutputToFile  .Checked:=a^.textFileCase=tfc_file;
    rbOutputToStderr.Checked:=a^.textFileCase=tfc_stderr;
    rbOutputToStdout.Checked:=a^.textFileCase=tfc_stdout;
    logFilenameEdit.Text:=a^.fileName;

    outFileVerbosityEdit.Text:=a^.getVerbosityPart;

    cbConsoleLikeLog.Checked:=not(a^.useLogFormatter);
    cbConvertPrintToLog.Enabled:=a^.useLogFormatter;
    cbConvertPrintToLog.Checked:=a^.handlePrintAsLog;
    timeFormatEdit.Enabled:=a^.useLogFormatter;
    timeFormatEdit.Text   :=a^.logDateFormat;
    logLocationLengthEdit.Enabled:=a^.useLogFormatter;
    logLocationLengthEdit.Text   :=intTostr(a^.logLocationLen);
    updateLogPreview;
  end;

procedure TCmdLineParametersFrame.initLabels;
  VAR i:longint;
  begin
    lightVersionRb.Caption:=settings.lightFlavourLocation;
    lightVersionRb.Enabled:=FileExists(settings.lightFlavourLocation);
    fullVersionRb.Caption :=settings.fullFlavourLocation;

    guiFlagCb.Caption:=FLAG_GUI;
    headlessFlagCb.Caption:=FLAG_HEADLESS;
    pauseFlagCb.Caption:=FLAG_PAUSE_ALWAYS;
    pauseOnErrorFlagCb.Caption:=FLAG_PAUSE_ON_ERR;

    quietFlagCb.Caption:=FLAG_QUIET;
    silentFlagCb.Caption:=FLAG_SILENT;
    profileFlagCb.Caption:=FLAG_PROFILE;
    forceStdOutCb.Caption:=FLAG_STDOUT;
    forceStdErrCb.Caption:=FLAG_STDERR;

    sideEffectsComboBox.Items.Clear;
    for i:=0 to length(C_sideEffectProfile)-1 do sideEffectsComboBox.Items.Add(C_sideEffectProfile[i].name);
  end;

function TCmdLineParametersFrame.currentAdapterSpecification: P_textFileAdapterSpecification;
  begin
    if (outputFileComboBox.ItemIndex>=0) and (outputFileComboBox.ItemIndex<length(execOptions.deferredAdapterCreations))
    then result:=@(execOptions.deferredAdapterCreations[outputFileComboBox.ItemIndex])
    else result:=nil;
  end;

procedure TCmdLineParametersFrame.initFromShebang(const s: String; const requires: T_sideEffects);
  VAR tempOptions:T_mnhExecutionOptions;
  begin
    tempOptions.create;
    tempOptions.initFromShebang(s,requires);
    initFromExecOptions(tempOptions);
    tempOptions.destroy;
  end;

procedure TCmdLineParametersFrame.initFromExecOptions(const opt: T_mnhExecutionOptions);
  VAR i:longint;
  begin
    initLabels;
    execOptions.clear;
    execOptions.copyFrom(opt);

    lightVersionRb.Checked:=lightVersionRb.Enabled and opt.callLightFlavour;
    fullVersionRb.Checked:=not(lightVersionRb.Checked);

    guiFlagCb         .Checked:=clf_GUI          in opt.flags;
    headlessFlagCb    .Checked:=clf_HEADLESS     in opt.flags;
    pauseFlagCb       .Checked:=clf_PAUSE_ALWAYS in opt.flags;
    pauseOnErrorFlagCb.Checked:=clf_PAUSE_ON_ERR in opt.flags;
    quietFlagCb       .Checked:=clf_QUIET        in opt.flags;
    silentFlagCb      .Checked:=clf_SILENT       in opt.flags;
    profileFlagCb     .Checked:=clf_PROFILE      in opt.flags;
    forceStdErrCb     .Checked:=clf_FORCE_STDERR in opt.flags;
    forceStdOutCb     .Checked:=clf_FORCE_STDOUT in opt.flags;
    consoleVerbosityEdit.Caption:=opt.verbosityString;
    sideEffectsComboBox.ItemIndex:=opt.sideEffectProfile;

    outputFileComboBox.Items.Clear;
    setLength(execOptions.deferredAdapterCreations,length(opt.deferredAdapterCreations));
    for i:=0 to length(opt.deferredAdapterCreations)-1 do begin
      execOptions.deferredAdapterCreations[i].copy(opt.deferredAdapterCreations[i]);
      outputFileComboBox.Items.Add(IntToStr(i+1));
    end;
    if length(opt.deferredAdapterCreations)>0
    then outputFileComboBox.ItemIndex:= 0
    else outputFileComboBox.ItemIndex:=-1;
    updateLogSection;
    updateLogPreview;
  end;

procedure TCmdLineParametersFrame.guiFlagCbClick(Sender: TObject);
  begin
    if guiFlagCb.checked then fullVersionRb.checked:=true;
  end;

procedure TCmdLineParametersFrame.cbConsoleLikeLogClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    if cbConsoleLikeLog.Checked then begin
      cbConvertPrintToLog  .Enabled:=false;
      timeFormatEdit       .Enabled:=false;
      logLocationLengthEdit.Enabled:=False;
    end else begin
      cbConvertPrintToLog  .Enabled:=true;
      timeFormatEdit       .Enabled:=true;
      logLocationLengthEdit.Enabled:=true;
    end;
    a:=currentAdapterSpecification;
    if a<>nil then begin
      a^.useLogFormatter:=not(cbConsoleLikeLog.Checked);
      updateLogPreview;
    end;
  end;

procedure TCmdLineParametersFrame.addOutFileClick(Sender: TObject);
  VAR newLogIndex:longint;
  begin
    with execOptions do begin
      newLogIndex:=length(deferredAdapterCreations);
      setLength(deferredAdapterCreations,newLogIndex+1);
      deferredAdapterCreations[newLogIndex].setFilenameAndOptions('?.log(1)',stringToMessageTypeSet(verbosityString));
    end;
    outputFileComboBox.Items.Add(IntToStr(newLogIndex+1));
    outputFileComboBox.ItemIndex:=newLogIndex;
    updateLogSection;
  end;

procedure TCmdLineParametersFrame.cbConvertPrintToLogClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a=nil then exit;
    a^.handlePrintAsLog:=cbConvertPrintToLog.Checked;
    a^.logDateFormat   :=timeFormatEdit.Caption;
    updateLogPreview;
  end;

procedure TCmdLineParametersFrame.cbLogAppendClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a=nil then exit;
    a^.forceNewFile:=not(cbLogAppend.Checked);
  end;

procedure TCmdLineParametersFrame.forceStdErrCbClick(Sender: TObject);
  begin
    if forceStdErrCb.Checked then forceStdOutCb.Checked:=false;
  end;

procedure TCmdLineParametersFrame.forceStdOutCbClick(Sender: TObject);
  begin
    if forceStdOutCb.Checked then forceStdErrCb.Checked:=False;
  end;

procedure TCmdLineParametersFrame.headlessFlagCbClick(Sender: TObject);
  begin
    if headlessFlagCb.Checked then begin
      pauseFlagCb.Checked:=false;
      pauseOnErrorFlagCb.Checked:=false;
    end;
  end;

procedure TCmdLineParametersFrame.anyPage1Change(Sender: TObject);
  begin
    execOptions.setCallLightFlavour(lightVersionRb.Checked);
    execOptions.flags:=[];
    if guiFlagCb         .Checked then include(execOptions.flags,clf_GUI);
    if headlessFlagCb    .Checked then include(execOptions.flags,clf_HEADLESS);
    if pauseFlagCb       .Checked then include(execOptions.flags,clf_PAUSE_ALWAYS);
    if pauseOnErrorFlagCb.Checked then include(execOptions.flags,clf_PAUSE_ON_ERR);
    if profileFlagCb     .Checked then include(execOptions.flags,clf_PROFILE);
    if quietFlagCb       .Checked then include(execOptions.flags,clf_QUIET);
    if silentFlagCb      .Checked then include(execOptions.flags,clf_SILENT);
    if forceStdErrCb     .Checked then include(execOptions.flags,clf_FORCE_STDERR);
    if forceStdOutCb     .Checked then include(execOptions.flags,clf_FORCE_STDOUT);
    execOptions.sideEffectProfile:=sideEffectsComboBox.ItemIndex;
  end;

end.

