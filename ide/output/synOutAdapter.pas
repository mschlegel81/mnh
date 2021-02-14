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
  out_adapters;
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
      lastWasDirectPrint:boolean;
      currentlyFlushing:boolean;
    protected
      outputLinesLimit:longint;
      FUNCTION getSynEdit:TSynEdit; virtual; abstract;
      FUNCTION getOwnerForm:TForm;  virtual; abstract;
    public

      wrapEcho:boolean;
      jumpToEnd:boolean;
      autoflush:boolean;
      CONSTRUCTOR create(CONST messageTypesToInc:T_messageTypeSet);
      FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
      PROPERTY directPrintFlag:boolean read lastWasDirectPrint;
      PROCEDURE flushClear;
      FUNCTION isDoneFlushing:boolean; virtual;
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
USES ideLayoutUtil,messageFormatting;
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

CONSTRUCTOR T_abstractSynOutAdapter.create(CONST messageTypesToInc:T_messageTypeSet);
  begin
    inherited create(at_guiSynOutput,messageTypesToInc);
    currentlyFlushing:=false;
    autoflush:=true;
    jumpToEnd:=true;
    wrapEcho:=false;
    //----------------------
    outputLinesLimit:=1000;
    lastWasDirectPrint:=false;
  end;

FUNCTION T_abstractSynOutAdapter.flushToGui(CONST forceFlush:boolean):T_messageTypeSet;
  VAR linesToWrite:T_arrayOfString;
      bufferOffset:longint;
      hadDirectPrint:boolean;
      wroteToSyn:boolean;
      SynEdit:TSynEdit=nil;
      messageFormatter:T_guiFormatter;

  PROCEDURE startOutput;
    begin
      currentlyFlushing:=true;
      setLength(linesToWrite,0);
      outputLinesLimit:=ideSettings.outputLinesLimit;
      bufferOffset:=0;
      hadDirectPrint:=false;
      wroteToSyn:=false;
    end;

  FUNCTION ensureSynEdit:TSynEdit; inline;
    begin
      if SynEdit=nil then begin
        SynEdit:=getSynEdit;
        SynEdit.BeginUpdate();
      end;
      result:=SynEdit;
    end;

  PROCEDURE flushBuffer;
    VAR i:longint;
        edit:TSynEdit;
    begin
      edit:=ensureSynEdit;
      if length(linesToWrite)>=outputLinesLimit then edit.lines.clear
      else while edit.lines.count+length(linesToWrite)>outputLinesLimit do edit.lines.delete(0);
      for i:=bufferOffset to length(linesToWrite)-1+bufferOffset do edit.lines.append(linesToWrite[i mod outputLinesLimit]);
      linesToWrite:=C_EMPTY_STRING_ARRAY;
      bufferOffset:=0;
    end;

  PROCEDURE appendInternal(CONST s:string);
    begin
      if length(linesToWrite)>=outputLinesLimit then begin
        linesToWrite[bufferOffset]:=s;
        inc(bufferOffset);
        if bufferOffset>=outputLinesLimit then bufferOffset:=0;
      end else myGenerics.append(linesToWrite,s);
      wroteToSyn:=true;
    end;

  FUNCTION singleMessageOut(CONST m: P_storedMessage):boolean;
    PROCEDURE clearSynAndBuffer;
      begin
        linesToWrite:=C_EMPTY_STRING_ARRAY;
        bufferOffset:=0;
        ensureSynEdit.lines.clear;
        wroteToSyn:=true;
      end;

    PROCEDURE processDirectPrint(CONST s:string);
      VAR c:char;
      begin
        ensureSynEdit.readonly:=false;
        for c in s do case c of
          #8 : SynEdit.executeCommand(ecDeleteLastChar,c,nil);
          #13: begin
                 SynEdit.executeCommand(ecLineStart,c,nil);
                 SynEdit.executeCommand(ecOverwriteMode,c,nil);
               end
          else SynEdit.executeCommand(ecChar,c,nil);
        end;
        SynEdit.readonly:=true;
        hadDirectPrint:=true;
      end;

    VAR j:longint;
        s:string;
    begin
      result:=true;
      case m^.messageType of
        mt_startOfEvaluation,
        mt_clearConsole: clearSynAndBuffer;
        mt_printline:
          begin
            if (length(P_storedMessageWithText(m)^.txt)>0) and (P_storedMessageWithText(m)^.txt[0]=C_formFeedChar) then begin
              clearSynAndBuffer;
              for j:=1 to length(P_storedMessageWithText(m)^.txt)-1 do appendInternal(P_storedMessageWithText(m)^.txt[j]);
            end else if lastWasDirectPrint then begin
              if length(P_storedMessageWithText(m)^.txt)>0 then begin
                processDirectPrint(P_storedMessageWithText(m)^.txt[0]);
              end;
              for j:=1 to length(P_storedMessageWithText(m)^.txt)-1 do appendInternal(P_storedMessageWithText(m)^.txt[j]);
            end else for s in P_storedMessageWithText(m)^.txt do appendInternal(s);
          end;
        mt_printdirect:
          begin
            if wroteToSyn then begin
              flushBuffer;
              wroteToSyn:=false;
              SynEdit.executeCommand(ecEditorBottom,' ',nil);
              SynEdit.executeCommand(ecLineStart,' ',nil);
            end;
            if not(lastWasDirectPrint) then begin
              ensureSynEdit.append('');
              SynEdit.executeCommand(ecEditorBottom,' ',nil);
              SynEdit.executeCommand(ecLineStart,' ',nil);
            end;
            for s in P_storedMessageWithText(m)^.txt do processDirectPrint(s);
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
        mt_echo_output: for s in messageFormatter.formatMessage(m) do appendInternal(s);
        mt_endOfEvaluation: ensureSynEdit.enabled:=true;
        else result:=false;
      end;
      if result then lastWasDirectPrint:=m^.messageType=mt_printdirect;
    end;

  PROCEDURE doneOutput(CONST jumpDown:boolean);
    VAR synOwnerForm:TForm;
    begin
      currentlyFlushing:=false;
      if wroteToSyn then begin
        flushBuffer;
        synOwnerForm:=getOwnerForm;
        if not(synOwnerForm.showing) or not(synOwnerForm.visible) then begin
          synOwnerForm.Show;
          synOwnerForm.visible:=true;
        end;
        if SynEdit<>nil then begin;
          SynEdit.EndUpdate;
          if jumpDown then begin
            SynEdit.executeCommand(ecEditorBottom,' ',nil);
            SynEdit.executeCommand(ecLineStart,' ',nil);
          end;
        end;
      end else begin
        if hadDirectPrint then begin
          synOwnerForm:=getOwnerForm;
          if (not(synOwnerForm.showing) or not(synOwnerForm.visible)) then begin
            synOwnerForm.Show;
            synOwnerForm.visible:=true;
          end;
        end;
        if SynEdit<>nil then SynEdit.EndUpdate;
      end;
      if SynEdit<>nil then SynEdit.enabled:=not(lastWasDirectPrint);
    end;

  VAR i:longint;
      toProcessInThisRun:T_storedMessages;
  begin
    if not(forceFlush or autoflush) then exit;
    enterCriticalSection(adapterCs);
    removeDuplicateStoredMessages([mt_el2_warning,mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined,mt_el4_systemError]);
    setLength(toProcessInThisRun,collectedFill);
    for i:=0 to collectedFill-1 do toProcessInThisRun[i]:=collected[i];
    result:=[];
    startOutput;
    collectedFill:=0;
    clear;
    leaveCriticalSection(adapterCs);
    messageFormatter.create(false);

    if length(toProcessInThisRun)>0 then begin
      messageFormatter.preferredLineLength:=getSynEdit.charsInWindow;
      messageFormatter.wrapEcho:=wrapEcho;
    end;

    for i:=0 to length(toProcessInThisRun)-1 do begin
      include(result,toProcessInThisRun[i]^.messageType);
      singleMessageOut(toProcessInThisRun[i]);
      disposeMessage(toProcessInThisRun[i]);
    end;
    if (mt_endOfEvaluation in result) and hadDirectPrint then appendInternal('');
    doneOutput(jumpToEnd);
    messageFormatter.destroy;
  end;

PROCEDURE T_abstractSynOutAdapter.flushClear;
  VAR clearMessage:P_storedMessage;
  begin
    system.enterCriticalSection(adapterCs);
    try
      lastWasDirectPrint:=false;
      clear;
      new(clearMessage,create(mt_clearConsole,C_nilSearchTokenLocation));
      append(clearMessage);
      disposeMessage(clearMessage);
    finally
      system.leaveCriticalSection(adapterCs);
    end;
  end;

FUNCTION T_abstractSynOutAdapter.isDoneFlushing:boolean;
  begin
    result:=not(currentlyFlushing) and (collectedFill<=0);
  end;

INITIALIZATION
  setLength(redirectors,0);

end.
