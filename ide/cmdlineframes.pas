UNIT cmdLineFrames;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  commandLineParameters, mnh_settings,out_adapters,messageFormatting,mnh_constants;

TYPE

  { TCmdLineParametersFrame }

  TCmdLineParametersFrame = class(TFrame)
    addOutfile: TButton;
    cbConsoleLikeLog: TCheckBox;
    cbConvertPrintToLog: TCheckBox;
    cbLogAppend: TCheckBox;
    cmdLinePreviewEdit: TEdit;
    forceStdErrCb: TCheckBox;
    forceStdOutCb: TCheckBox;
    Label4: TLabel;
    outputFileComboBox: TComboBox;
    Panel1: TPanel;
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
    PROCEDURE addOutFileClick(Sender: TObject);
    PROCEDURE cbConsoleLikeLogClick(Sender: TObject);
    PROCEDURE cbConvertPrintToLogClick(Sender: TObject);
    PROCEDURE cbLogAppendClick(Sender: TObject);
    PROCEDURE forceStdErrCbClick(Sender: TObject);
    PROCEDURE forceStdOutCbClick(Sender: TObject);
    PROCEDURE guiFlagCbClick(Sender: TObject);
    PROCEDURE headlessFlagCbClick(Sender: TObject);
    PROCEDURE anyPage1Change(Sender: TObject);
    PROCEDURE lightVersionRbClick(Sender: TObject);
    PROCEDURE logFilenameEditEditingDone(Sender: TObject);
    PROCEDURE logLocationLengthEditEditingDone(Sender: TObject);
    PROCEDURE outFileVerbosityEditEditingDone(Sender: TObject);
    PROCEDURE outputFileComboBoxSelect(Sender: TObject);
    PROCEDURE pauseFlagCbClick(Sender: TObject);
    PROCEDURE pauseOnErrorFlagCbClick(Sender: TObject);
    PROCEDURE profileFlagCbClick(Sender: TObject);
    PROCEDURE rbOutputToFileClick(Sender: TObject);
    PROCEDURE rbOutputToStderrClick(Sender: TObject);
    PROCEDURE rbOutputToStdoutClick(Sender: TObject);
    PROCEDURE removeOutFileClick(Sender: TObject);
  private
    optionsToUpdate:P_mnhExecutionOptions;
    initializing:boolean;
    shebangEditor:boolean;
    scriptFilePath:string;
    PROCEDURE updateLogPreview;
    PROCEDURE updateLogSection;
    PROCEDURE initLabels;
    FUNCTION currentAdapterSpecification:P_textFileAdapterSpecification;
    PROCEDURE updateLogComboBox(CONST preferredItemIndex:longint);
    PROCEDURE updateShebangPreview;
  public
    CONSTRUCTOR create(TheOwner: TComponent); override;
    PROCEDURE initFromExecOptions(CONST opt:P_mnhExecutionOptions);
  end;

PROCEDURE detachCmdLineParametersFrameInstance;
FUNCTION getCmdLineParametersFrameInstance(CONST parent:TWinControl;
                                           CONST shebangEditor:boolean;
                                           CONST scriptFilePath:string;
                                           CONST optionsToUpdate:P_mnhExecutionOptions):TCmdLineParametersFrame;
IMPLEMENTATION
USES mnh_messages,myGenerics,litVar,basicTypes,myStringUtil;
{$R *.lfm}
VAR myFrame:TCmdLineParametersFrame=nil;

PROCEDURE detachCmdLineParametersFrameInstance;
  begin
    if myFrame<>nil then myFrame.parent:=nil;
  end;

FUNCTION getCmdLineParametersFrameInstance(CONST parent:TWinControl;
                                           CONST shebangEditor:boolean;
                                           CONST scriptFilePath:string;
                                           CONST optionsToUpdate:P_mnhExecutionOptions):TCmdLineParametersFrame;
  begin
    if myFrame=nil then myFrame:=TCmdLineParametersFrame.create(Application);
    myFrame.parent:=parent;
    myFrame.shebangEditor:=shebangEditor;
    myFrame.scriptFilePath:=scriptFilePath;
    myFrame.initFromExecOptions(optionsToUpdate);
    result:=myFrame;
  end;

{ TCmdLineParametersFrame }

PROCEDURE TCmdLineParametersFrame.lightVersionRbClick(Sender: TObject);
  begin
    guiFlagCb.checked    :=false;
    profileFlagCb.checked:=false;
  end;

PROCEDURE TCmdLineParametersFrame.logFilenameEditEditingDone(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a=nil then exit;
    a^.fileName:=logFilenameEdit.caption;
    updateLogComboBox(outputFileComboBox.ItemIndex);
  end;

PROCEDURE TCmdLineParametersFrame.logLocationLengthEditEditingDone(
  Sender: TObject);
  VAR i:longint;
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a=nil then exit;
    i:=strToIntDef(logLocationLengthEdit.text,maxLongint);
    if i<0 then i:=maxLongint;
    logLocationLengthEdit.text:=intToStr(i);
    a^.logLocationLen:=i;
    updateLogPreview;
    updateShebangPreview;
  end;

PROCEDURE TCmdLineParametersFrame.outFileVerbosityEditEditingDone(
  Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a<>nil then begin
      a^.setVerbosityPart(outFileVerbosityEdit.text,stringToMessageTypeSet(optionsToUpdate^.verbosityString));
      updateLogPreview;
      updateShebangPreview;
    end;
  end;

PROCEDURE TCmdLineParametersFrame.outputFileComboBoxSelect(Sender: TObject);
  begin
    updateLogSection;
  end;

PROCEDURE TCmdLineParametersFrame.pauseFlagCbClick(Sender: TObject);
  begin
    if pauseFlagCb.checked then headlessFlagCb.checked:=false;
  end;

PROCEDURE TCmdLineParametersFrame.pauseOnErrorFlagCbClick(Sender: TObject);
  begin
    if pauseOnErrorFlagCb.checked then headlessFlagCb.checked:=false;
  end;

PROCEDURE TCmdLineParametersFrame.profileFlagCbClick(Sender: TObject);
  begin
    if profileFlagCb.checked then fullVersionRb.checked:=true;
  end;

PROCEDURE TCmdLineParametersFrame.rbOutputToFileClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    logFilenameEdit.enabled:=true;
    a:=currentAdapterSpecification;
    if a<>nil then a^.textFileCase:=tfc_file;
    updateLogComboBox(outputFileComboBox.ItemIndex);
    updateShebangPreview;
  end;

PROCEDURE TCmdLineParametersFrame.rbOutputToStderrClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    logFilenameEdit.enabled:=false;
    a:=currentAdapterSpecification;
    if a<>nil then a^.textFileCase:=tfc_stderr;
    updateLogComboBox(outputFileComboBox.ItemIndex);
    updateShebangPreview;
  end;

PROCEDURE TCmdLineParametersFrame.rbOutputToStdoutClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    logFilenameEdit.enabled:=false;
    a:=currentAdapterSpecification;
    if a<>nil then a^.textFileCase:=tfc_stdout;
    updateLogComboBox(outputFileComboBox.ItemIndex);
    updateShebangPreview;
  end;

PROCEDURE TCmdLineParametersFrame.removeOutFileClick(Sender: TObject);
  VAR logIdxToDrop:longint;
      i:longint;
  begin
    logIdxToDrop:=outputFileComboBox.ItemIndex;
    with optionsToUpdate^ do if (logIdxToDrop>=0) and (logIdxToDrop<length(deferredAdapterCreations)) then begin
      for i:=logIdxToDrop to length(deferredAdapterCreations)-2 do deferredAdapterCreations[i]:=deferredAdapterCreations[i+1];
      setLength(deferredAdapterCreations,length(deferredAdapterCreations)-1);
      updateLogComboBox(logIdxToDrop);
      updateLogSection;
    end;
  end;

PROCEDURE TCmdLineParametersFrame.updateLogPreview;
  VAR specification:P_textFileAdapterSpecification;
      formatter:P_messageFormatProvider;
  PROCEDURE formattedLinesFor(CONST messageType:T_messageType; CONST lineIndex:longint;
                             CONST txt1:string);
    VAR message:T_storedMessageWithText;
        location:T_searchTokenLocation;
        toAppend:T_arrayOfString;
        line:string;
    begin

      location.fileName:=scriptFilePath;
      location.line:=lineIndex;
      location.column:=1;
      message.create(messageType,location,split(txt1,'#'));
      toAppend:=formatter^.formatMessage(@message);
      for line in toAppend do formatPreviewMemo.append(line);
      message.unreference;
      message.destroy;
    end;

  PROCEDURE formattedLinesForOutput;
    VAR message:T_echoOutMessage;
        location:T_searchTokenLocation;
        toAppend:T_arrayOfString;
        line:string;
    begin
      location.fileName:=scriptFilePath;
      location.line:=2;
      location.column:=1;
      message.create(newVoidLiteral,location);
      toAppend:=formatter^.formatMessage(@message);
      for line in toAppend do formatPreviewMemo.append(line);
      message.unreference;
      message.destroy;
    end;

  VAR messagesToInclude:T_messageTypeSet;
  begin
    specification:=currentAdapterSpecification;
    formatPreviewMemo.clear;
    if specification=nil then exit;

    formatter:=getFormatterFor(specification^);
    messagesToInclude:=specification^.getMessageTypes;

    if mt_echo_declaration in messagesToInclude
    then formattedLinesFor(mt_echo_declaration,1,'f(x)->2*x+3;');

    if mt_echo_input in messagesToInclude
    then formattedLinesFor(mt_echo_input,2,'print("f(7)=",f(7));');

    if mt_printline in messagesToInclude
    then formattedLinesFor(mt_printline,2,'f(7)=17');

    if mt_echo_output in messagesToInclude
    then formattedLinesForOutput;

    if mt_log in messagesToInclude
    then formattedLinesFor(mt_log,20,'This is a log message.');

    if mt_el1_userNote in messagesToInclude
    then formattedLinesFor(mt_el1_userNote,21,'This is a (user) note.');

    if mt_el1_note in messagesToInclude
    then formattedLinesFor(mt_el1_note,22,'This is a builtin note.#Like user notes it may contain multiple lines...');

    if mt_el2_userWarning in messagesToInclude
    then formattedLinesFor(mt_el2_userWarning,23,'This is a (user) warning message.');

    if mt_el2_warning in messagesToInclude
    then formattedLinesFor(mt_el2_warning,24,'This is a builtin warning message.');

    if mt_el3_evalError in messagesToInclude
    then formattedLinesFor(mt_el3_evalError,25,'This is an error!!!#An erromessage may contain multiple lines.');

    if mt_timing_info in messagesToInclude
    then formattedLinesFor(mt_timing_info,0,'Importing time      802.108ms#Tokenizing time       9.126ms#Declaration time     55.315ms#Interpretation time   0.000ms#Unaccounted for       1.044ms#-----------------------------#Total               867.593ms#');
    dispose(formatter,destroy);
  end;

PROCEDURE TCmdLineParametersFrame.updateLogSection;
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    GroupBox5        .enabled:=(a<>nil);
    verbosityGroupBox.enabled:=(a<>nil);
    GroupBox6        .enabled:=(a<>nil);
    if a=nil then exit;

    rbOutputToFile  .checked:=a^.textFileCase=tfc_file;
    rbOutputToStderr.checked:=a^.textFileCase=tfc_stderr;
    rbOutputToStdout.checked:=a^.textFileCase=tfc_stdout;
    logFilenameEdit.text:=a^.fileName;

    outFileVerbosityEdit.text:=a^.getVerbosityPart;

    cbConsoleLikeLog.checked:=not(a^.useLogFormatter);
    cbConvertPrintToLog.enabled:=a^.useLogFormatter;
    cbConvertPrintToLog.checked:=a^.handlePrintAsLog;
    timeFormatEdit.enabled:=a^.useLogFormatter;
    timeFormatEdit.text   :=a^.logDateFormat;
    logLocationLengthEdit.enabled:=a^.useLogFormatter;
    logLocationLengthEdit.text   :=intToStr(a^.logLocationLen);
    updateLogPreview;
    updateShebangPreview;
  end;

PROCEDURE TCmdLineParametersFrame.initLabels;
  VAR i:longint;
  begin
    lightVersionRb.caption:=settings.lightFlavourLocation;
    lightVersionRb.enabled:=fileExists(settings.lightFlavourLocation);
    fullVersionRb.caption :=settings.fullFlavourLocation;

    guiFlagCb.caption:=FLAG_GUI;
    headlessFlagCb.caption:=FLAG_HEADLESS;
    pauseFlagCb.caption:=FLAG_PAUSE_ALWAYS;
    pauseOnErrorFlagCb.caption:=FLAG_PAUSE_ON_ERR;

    quietFlagCb.caption:=FLAG_QUIET;
    silentFlagCb.caption:=FLAG_SILENT;
    profileFlagCb.caption:=FLAG_PROFILE;
    forceStdOutCb.caption:=FLAG_STDOUT;
    forceStdErrCb.caption:=FLAG_STDERR;

    sideEffectsComboBox.items.clear;
    for i:=0 to length(C_sideEffectProfile)-1 do sideEffectsComboBox.items.add(C_sideEffectProfile[i].name);
    sideEffectsComboBox.ItemIndex:=0;
  end;

FUNCTION TCmdLineParametersFrame.currentAdapterSpecification: P_textFileAdapterSpecification;
  begin
    if (outputFileComboBox.ItemIndex>=0) and (outputFileComboBox.ItemIndex<length(optionsToUpdate^.deferredAdapterCreations))
    then result:=@(optionsToUpdate^.deferredAdapterCreations[outputFileComboBox.ItemIndex])
    else result:=nil;
  end;

PROCEDURE TCmdLineParametersFrame.updateLogComboBox(CONST preferredItemIndex: longint);
  VAR i:longint;
  begin
    outputFileComboBox.items.clear;
    for i:=0 to length(optionsToUpdate^.deferredAdapterCreations)-1 do begin
      case optionsToUpdate^.deferredAdapterCreations[i].textFileCase of
        tfc_stdout: outputFileComboBox.items.add(intToStr(i+1)+': STDOUT');
        tfc_stderr: outputFileComboBox.items.add(intToStr(i+1)+': STDERR');
        else        outputFileComboBox.items.add(intToStr(i+1)+': '+optionsToUpdate^.deferredAdapterCreations[i].fileName);
      end;
    end;
    if length(optionsToUpdate^.deferredAdapterCreations)>0
    then begin
      if (preferredItemIndex>=0) and (preferredItemIndex<outputFileComboBox.DropDownCount)
      then outputFileComboBox.ItemIndex:= preferredItemIndex
      else outputFileComboBox.ItemIndex:= 0;
    end
    else outputFileComboBox.ItemIndex:=-1;
  end;

PROCEDURE TCmdLineParametersFrame.updateShebangPreview;
  VAR newTxt:string;
  begin
    newTxt:=optionsToUpdate^.getShebang;
    if not(shebangEditor)
    then newTxt:=copy(newTxt,3,length(newTxt)-2)+' '+scriptFilePath;
    cmdLinePreviewEdit.text:=newTxt;
  end;

CONSTRUCTOR TCmdLineParametersFrame.create(TheOwner: TComponent);
  begin
    inherited create(TheOwner);
    optionsToUpdate:=nil;
    initLabels;
  end;

PROCEDURE TCmdLineParametersFrame.initFromExecOptions(
  CONST opt: P_mnhExecutionOptions);
  VAR i:longint;
  begin
    initializing:=true;
    initLabels;
    optionsToUpdate:=opt;

    lightVersionRb.checked:=lightVersionRb.enabled and optionsToUpdate^.callLightFlavour;
    fullVersionRb.checked:=not(lightVersionRb.checked);

    if not(optionsToUpdate^.callLightFlavour) then optionsToUpdate^.executor:=settings.fullFlavourLocation;

    guiFlagCb         .checked:=clf_GUI          in optionsToUpdate^.flags;
    headlessFlagCb    .checked:=clf_HEADLESS     in optionsToUpdate^.flags;
    pauseFlagCb       .checked:=clf_PAUSE_ALWAYS in optionsToUpdate^.flags;
    pauseOnErrorFlagCb.checked:=clf_PAUSE_ON_ERR in optionsToUpdate^.flags;
    quietFlagCb       .checked:=clf_QUIET        in optionsToUpdate^.flags;
    silentFlagCb      .checked:=clf_SILENT       in optionsToUpdate^.flags;
    profileFlagCb     .checked:=clf_PROFILE      in optionsToUpdate^.flags;
    forceStdErrCb     .checked:=clf_FORCE_STDERR in optionsToUpdate^.flags;
    forceStdOutCb     .checked:=clf_FORCE_STDOUT in optionsToUpdate^.flags;
    consoleVerbosityEdit.text:=optionsToUpdate^.verbosityString;
    sideEffectsComboBox.ItemIndex:=optionsToUpdate^.sideEffectProfile;

    setLength(optionsToUpdate^.deferredAdapterCreations,length(optionsToUpdate^.deferredAdapterCreations));
    for i:=0 to length(optionsToUpdate^.deferredAdapterCreations)-1 do
      optionsToUpdate^.deferredAdapterCreations[i].copy(optionsToUpdate^.deferredAdapterCreations[i]);

    updateLogComboBox(0);
    updateLogSection;
    updateLogPreview;
    initializing:=false;
  end;

PROCEDURE TCmdLineParametersFrame.guiFlagCbClick(Sender: TObject);
  begin
    if guiFlagCb.checked then begin
      fullVersionRb.checked:=true;
      headlessFlagCb.checked:=false;
    end;
  end;

PROCEDURE TCmdLineParametersFrame.cbConsoleLikeLogClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    if cbConsoleLikeLog.checked then begin
      cbConvertPrintToLog  .enabled:=false;
      timeFormatEdit       .enabled:=false;
      logLocationLengthEdit.enabled:=false;
    end else begin
      cbConvertPrintToLog  .enabled:=true;
      timeFormatEdit       .enabled:=true;
      logLocationLengthEdit.enabled:=true;
    end;
    a:=currentAdapterSpecification;
    if a<>nil then begin
      a^.useLogFormatter:=not(cbConsoleLikeLog.checked);
      updateLogPreview;
      updateShebangPreview;
    end;
  end;

PROCEDURE TCmdLineParametersFrame.addOutFileClick(Sender: TObject);
  VAR newLogIndex:longint;
  begin
    with optionsToUpdate^ do begin
      newLogIndex:=length(deferredAdapterCreations);
      setLength(deferredAdapterCreations,newLogIndex+1);
      deferredAdapterCreations[newLogIndex].setFilenameAndOptions('?.log(1)',stringToMessageTypeSet(verbosityString));
    end;
    updateLogComboBox(newLogIndex);
    updateLogSection;
  end;

PROCEDURE TCmdLineParametersFrame.cbConvertPrintToLogClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a=nil then exit;
    a^.handlePrintAsLog:=cbConvertPrintToLog.checked;
    a^.logDateFormat   :=timeFormatEdit.text;
    updateLogPreview;
    updateShebangPreview;
  end;

PROCEDURE TCmdLineParametersFrame.cbLogAppendClick(Sender: TObject);
  VAR a:P_textFileAdapterSpecification;
  begin
    a:=currentAdapterSpecification;
    if a=nil then exit;
    a^.forceNewFile:=not(cbLogAppend.checked);
    updateShebangPreview;
  end;

PROCEDURE TCmdLineParametersFrame.forceStdErrCbClick(Sender: TObject);
  begin
    if forceStdErrCb.checked then forceStdOutCb.checked:=false;
  end;

PROCEDURE TCmdLineParametersFrame.forceStdOutCbClick(Sender: TObject);
  begin
    if forceStdOutCb.checked then forceStdErrCb.checked:=false;
  end;

PROCEDURE TCmdLineParametersFrame.headlessFlagCbClick(Sender: TObject);
  begin
    if headlessFlagCb.checked then begin
      guiFlagCb.checked:=false;
      pauseFlagCb.checked:=false;
      pauseOnErrorFlagCb.checked:=false;
    end;
  end;

PROCEDURE TCmdLineParametersFrame.anyPage1Change(Sender: TObject);
  begin
    if initializing then exit;
    optionsToUpdate^.setCallLightFlavour(lightVersionRb.checked);
    optionsToUpdate^.flags:=[];
    optionsToUpdate^.verbosityString:=consoleVerbosityEdit.text;
    if guiFlagCb         .checked then include(optionsToUpdate^.flags,clf_GUI);
    if headlessFlagCb    .checked then include(optionsToUpdate^.flags,clf_HEADLESS);
    if pauseFlagCb       .checked then include(optionsToUpdate^.flags,clf_PAUSE_ALWAYS);
    if pauseOnErrorFlagCb.checked then include(optionsToUpdate^.flags,clf_PAUSE_ON_ERR);
    if profileFlagCb     .checked then include(optionsToUpdate^.flags,clf_PROFILE);
    if quietFlagCb       .checked then include(optionsToUpdate^.flags,clf_QUIET);
    if silentFlagCb      .checked then include(optionsToUpdate^.flags,clf_SILENT);
    if forceStdErrCb     .checked then include(optionsToUpdate^.flags,clf_FORCE_STDERR);
    if forceStdOutCb     .checked then include(optionsToUpdate^.flags,clf_FORCE_STDOUT);
    optionsToUpdate^.sideEffectProfile:=sideEffectsComboBox.ItemIndex;
    updateShebangPreview;
  end;

end.

