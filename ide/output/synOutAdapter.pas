UNIT synOutAdapter;
INTERFACE
USES
  sysutils, SynEdit, Forms,
  SynEditKeyCmds,
  myStringUtil,
  mnh_constants,basicTypes,
  mnh_messages,
  mnh_settings,
  myGenerics,
  out_adapters,
  messageFormatting;
CONST
  C_synOutDefaultMessageTypes:T_messageTypeSet=[mt_clearConsole,
                                              mt_printline,
                                              mt_printdirect,
                                              mt_el1_note,
                                              mt_el1_userNote,
                                              mt_el2_warning,
                                              mt_el2_userWarning,
                                              mt_el3_evalError,
                                              mt_el3_noMatchingMain,
                                              mt_el3_userDefined,
                                              mt_el4_systemError,
                                              mt_timing_info];
TYPE
  P_abstractSynOutAdapter=^T_abstractSynOutAdapter;
  T_abstractSynOutAdapter=object(T_abstractGuiOutAdapter)
    private
      currentlyFlushing:boolean;
      messageFormatter:T_guiFormatter;
    protected
      state:T_messagesAndLocations;
      FUNCTION getSynEdit:TSynEdit; virtual; abstract;
      FUNCTION getOwnerForm:TForm;  virtual; abstract;
    public

      jumpToEnd:boolean;
      autoflush:boolean;
      CONSTRUCTOR create(CONST messageTypesToInc:T_messageTypeSet);
      FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
      PROCEDURE flushClear;
      FUNCTION isDoneFlushing:boolean; virtual;
      PROPERTY wrapEcho:boolean read messageFormatter.wrapEcho write messageFormatter.wrapEcho;
      PROPERTY preferredLineLength:longint read messageFormatter.preferredLineLength write messageFormatter.preferredLineLength;
      FUNCTION getLocationAtLine(CONST lineIndex:longint):T_searchTokenLocation;
  end;

  P_eagerInitializedOutAdapter=^T_eagerInitializedOutAdapter;
  T_eagerInitializedOutAdapter=object(T_abstractSynOutAdapter)
    private
      SynEdit:TSynEdit;
      ownerForm:TForm;
    protected
      FUNCTION getSynEdit:TSynEdit; virtual;
      FUNCTION getOwnerForm:TForm;  virtual;
    public
      CONSTRUCTOR create(CONST synEdit_:TSynEdit;
                         CONST ownerForm_:TForm;
                         CONST messageTypesToInc:T_messageTypeSet=[mt_clearConsole,
                                                                   mt_printline,
                                                                   mt_printdirect,
                                                                   mt_log,
                                                                   mt_el1_note,
                                                                   mt_el1_userNote,
                                                                   mt_el2_warning,
                                                                   mt_el2_userWarning,
                                                                   mt_echo_input,
                                                                   mt_echo_output,
                                                                   mt_echo_declaration,
                                                                   mt_startOfEvaluation,
                                                                   mt_endOfEvaluation]);
  end;

  P_redirectionAwareConsoleOutAdapter=^T_redirectionAwareConsoleOutAdapter;
  T_redirectionAwareConsoleOutAdapter=object(T_consoleOutAdapter)
    CONSTRUCTOR create(CONST messageTypesToInclude_:T_messageTypeSet; CONST consoleMode:T_consoleOutMode; CONST formatProvider:P_messageFormatProvider);
    FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
  end;

PROCEDURE registerRedirector(CONST syn:P_eagerInitializedOutAdapter);
PROCEDURE unregisterRedirector(CONST syn:P_eagerInitializedOutAdapter);
IMPLEMENTATION
USES ideLayoutUtil,contexts;
VAR redirectors:array of P_eagerInitializedOutAdapter;
    redirected:T_messageTypeSet=[];

PROCEDURE registerRedirector(CONST syn:P_eagerInitializedOutAdapter);
  VAR k:longint;
  begin
    k:=0;
    while (k<length(redirectors)) and (redirectors[k]<>syn) do inc(k);
    if k=length(redirectors) then setLength(redirectors,k+1);
    redirectors[k]:=syn;

    redirected:=[];
    for k:=0 to length(redirectors)-1 do redirected+=redirectors[k]^.outputBehavior;
  end;

PROCEDURE unregisterRedirector(CONST syn:P_eagerInitializedOutAdapter);
  VAR k:longint;
  begin
    k:=0;
    while (k<length(redirectors)) and (redirectors[k]<>syn) do inc(k);
    if k<length(redirectors) then begin
      redirectors[k]:=redirectors[length(redirectors)-1];
      setLength(redirectors,length(redirectors)-1);
      redirected:=[];
      for k:=0 to length(redirectors)-1 do redirected+=redirectors[k]^.outputBehavior;
    end;
  end;

FUNCTION redirectedMessages:T_messageTypeSet;
  begin
    result:=redirected;
  end;

FUNCTION T_eagerInitializedOutAdapter.getSynEdit: TSynEdit;
  begin result:=SynEdit; end;

FUNCTION T_eagerInitializedOutAdapter.getOwnerForm: TForm;
  begin result:=ownerForm; end;

CONSTRUCTOR T_eagerInitializedOutAdapter.create(CONST synEdit_: TSynEdit; CONST ownerForm_: TForm; CONST messageTypesToInc: T_messageTypeSet);
  begin
    inherited create(messageTypesToInc);
    SynEdit:=synEdit_;
    ownerForm:=ownerForm_;
  end;

CONSTRUCTOR T_redirectionAwareConsoleOutAdapter.create(CONST messageTypesToInclude_: T_messageTypeSet; CONST consoleMode:T_consoleOutMode; CONST formatProvider:P_messageFormatProvider);
  begin
    inherited create(messageTypesToInclude_,consoleMode,formatProvider);
  end;

FUNCTION T_redirectionAwareConsoleOutAdapter.append(CONST message: P_storedMessage): boolean;
  begin
    if message^.messageType in redirected
    then result:=false
    else result:=inherited append(message);
  end;

CONSTRUCTOR T_abstractSynOutAdapter.create(CONST messageTypesToInc: T_messageTypeSet);
  begin
    inherited create(at_guiSynOutput,messageTypesToInc);
    currentlyFlushing:=false;
    autoflush:=true;
    jumpToEnd:=true;
    messageFormatter.create(false);
    messageFormatter.wrapEcho:=false;
    state.create(maxLongint);
  end;

FUNCTION T_abstractSynOutAdapter.flushToGui(CONST forceFlush: boolean): T_messageTypeSet;
  VAR SynEdit:TSynEdit=nil;
      synOwnerForm:TForm;
      toProcessInThisRun:T_storedMessages;
      i,i0: longint;
      fullUpdateRequired:boolean=false;
      start:double;

  PROCEDURE processMessage(CONST message: P_storedMessage);
    VAR j:longint;
        s:string;
    begin
      case message^.messageType of
        mt_startOfEvaluation,
        mt_clearConsole: begin
          state.clear;
          fullUpdateRequired:=true;
        end;
        mt_printline:
          begin
            if (length(P_storedMessageWithText(message)^.txt)>0) and (P_storedMessageWithText(message)^.txt[0]=C_formFeedChar) then begin
              state.clear;
              fullUpdateRequired:=true;
              for j:=1 to length(P_storedMessageWithText(message)^.txt)-1 do state.append(P_storedMessageWithText(message)^.txt[j]);
            end else for s in P_storedMessageWithText(message)^.txt do state.append(s);
          end;
        mt_printdirect:
          begin
            fullUpdateRequired:=true;
            for s in P_storedMessageWithText(message)^.txt do state.processDirectPrint(s);
          end;
        mt_log,
        mt_el1_note,
        mt_el1_userNote,
        mt_el2_warning,
        mt_el2_userWarning,
        mt_el3_evalError,
        mt_el3_noMatchingMain,
        mt_el3_userDefined,
        mt_el4_systemError,
        mt_timing_info,
        mt_echo_input,
        mt_echo_declaration,
        mt_echo_output: begin
          messageFormatter.formatMessageAndLocation(message,state);
        end;
        mt_endOfEvaluation: state.cleanup;
      end;
    end;

  begin
    start:=now;
    if not(forceFlush or autoflush) then exit([]);

    enterCriticalSection(adapterCs);
    currentlyFlushing:=true;
    removeDuplicateStoredMessages([mt_el2_warning,mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined,mt_el4_systemError]);
    setLength(toProcessInThisRun,collectedFill);
    for i:=0 to collectedFill-1 do toProcessInThisRun[i]:=collected[i];
    result:=[];
    collectedFill:=0;
    leaveCriticalSection(adapterCs);

    if length(toProcessInThisRun)>0 then begin
      if state.size=0 then fullUpdateRequired:=true;

      SynEdit:=getSynEdit;
      messageFormatter.preferredLineLength:=SynEdit.charsInWindow;
      state.outputLinesLimit:=ideSettings.outputLinesLimit;

      i0:=SynEdit.lines.count;

      result:=[];
      for i:=0 to length(toProcessInThisRun)-1 do begin
        include(result,toProcessInThisRun[i]^.messageType);
        processMessage(toProcessInThisRun[i]);
        disposeMessage(toProcessInThisRun[i]);
      end;
      fullUpdateRequired:=fullUpdateRequired or (state.size=state.outputLinesLimit);

      SynEdit.BeginUpdate();
      if fullUpdateRequired then SynEdit.lines.SetStrings(state.text)
      else for i:=i0 to state.size-1 do SynEdit.lines.add(state.text(i));
      SynEdit.EndUpdate;

      synOwnerForm:=getOwnerForm;
      if not(synOwnerForm.showing) or not(synOwnerForm.visible) then begin
        synOwnerForm.Show;
        synOwnerForm.visible:=true;
      end;
      if jumpToEnd then begin
        SynEdit.executeCommand(ecEditorBottom,' ',nil);
        SynEdit.executeCommand(ecLineStart,' ',nil);
      end;
    end else result:=[];
    currentlyFlushing:=false;
    if now-start>ONE_SECOND*0.1 then postIdeMessage('Flush of output adapter form took a long time: '+myTimeToStr(now-start),true);
  end;

PROCEDURE T_abstractSynOutAdapter.flushClear;
  VAR clearMessage:P_storedMessage;
  begin
    system.enterCriticalSection(adapterCs);
    try
      clear;
      new(clearMessage,create(mt_clearConsole,C_nilSearchTokenLocation));
      append(clearMessage);
      disposeMessage(clearMessage);
    finally
      system.leaveCriticalSection(adapterCs);
    end;
  end;

FUNCTION T_abstractSynOutAdapter.isDoneFlushing: boolean;
  begin
    result:=not(currentlyFlushing) and (collectedFill<=0);
  end;

FUNCTION T_abstractSynOutAdapter.getLocationAtLine(CONST lineIndex:longint):T_searchTokenLocation;
  CONST noLocation:T_searchTokenLocation=(fileName:'';line:-1;column:-1);
  begin
    if (lineIndex>=0) and (lineIndex<state.size)
    then result:=state.location(lineIndex)
    else result:=noLocation;
    if (result.fileName='') or (result.fileName='?')
    then result:=guessLocationFromString(getSynEdit.lines[lineIndex],false);
  end;

INITIALIZATION
  setLength(redirectors,0);

end.
