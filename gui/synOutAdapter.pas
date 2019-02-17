UNIT synOutAdapter;
INTERFACE
USES
  sysutils, SynEdit, Forms,
  SynEditKeyCmds,
  myStringUtil,
  mnh_constants,basicTypes,
  mnh_messages,
  mnh_settings,
  myGenerics,out_adapters;
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
  P_synOutAdapter=^T_synOutAdapter;
  T_synOutAdapter=object(T_collectingOutAdapter)
    private
      id:longint;

      synOwnerForm:TForm;
      linesToWrite:T_arrayOfString;
      bufferOffset:longint;

      lastWasDirectPrint:boolean;
      hadDirectPrint:boolean;
      wroteToSyn:boolean;
      PROCEDURE flushBuffer;
    protected
      syn:TSynEdit;
      outputLinesLimit:longint;
      PROCEDURE appendInternal(CONST s:string);
      PROCEDURE startOutput;
      FUNCTION singleMessageOut(CONST m:P_storedMessage):boolean;
      PROCEDURE doneOutput(CONST jumpDown:boolean);
    public
      wrapEcho:boolean;
      CONSTRUCTOR create(CONST owner:TForm; CONST outputEdit:TSynEdit;
                         CONST messageTypesToInclude:T_messageTypeSet=[mt_clearConsole,
                                                                       mt_printline,
                                                                       mt_printdirect,
                                                                       mt_el1_note,
                                                                       mt_el1_userNote,
                                                                       mt_el2_warning,
                                                                       mt_el2_userWarning,
                                                                       mt_echo_input,
                                                                       mt_echo_output,
                                                                       mt_echo_declaration,
                                                                       mt_echo_continued,
                                                                       mt_endOfEvaluation]);
      FUNCTION flushToGui(CONST jumpToEnd:boolean):T_messageTypeSet;
      PROPERTY directPrintFlag:boolean read lastWasDirectPrint;
      PROPERTY ownerForm:TForm read synOwnerForm;
      PROCEDURE flushClear;
  end;

  T_redirectionAwareConsoleOutAdapter=object(T_consoleOutAdapter)
    CONSTRUCTOR create(CONST messageTypesToInclude_:T_messageTypeSet);
    FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
  end;

PROCEDURE registerRedirector(CONST syn:P_synOutAdapter);
PROCEDURE unregisterRedirector(CONST syn:P_synOutAdapter);
IMPLEMENTATION
VAR lastSynOutId:longint=0;
    redirectors:array of P_synOutAdapter;
    redirected:T_messageTypeSet=[];

PROCEDURE registerRedirector(CONST syn:P_synOutAdapter);
  VAR k:longint;
  begin
    k:=0;
    while (k<length(redirectors)) and (redirectors[k]^.id<>syn^.id) do inc(k);
    if k=length(redirectors) then setLength(redirectors,k+1);
    redirectors[k]:=syn;

    redirected:=[];
    for k:=0 to length(redirectors)-1 do redirected+=redirectors[k]^.outputBehavior;
  end;

PROCEDURE unregisterRedirector(CONST syn:P_synOutAdapter);
  VAR k:longint;
  begin
    k:=0;
    while (k<length(redirectors)) and (redirectors[k]^.id<>syn^.id) do inc(k);
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

CONSTRUCTOR T_redirectionAwareConsoleOutAdapter.create(CONST messageTypesToInclude_: T_messageTypeSet);
  begin
    inherited create(messageTypesToInclude_);
  end;

FUNCTION T_redirectionAwareConsoleOutAdapter.append(CONST message: P_storedMessage): boolean;
  begin
    if message^.messageType in redirected
    then result:=false
    else result:=inherited append(message);
  end;

PROCEDURE T_synOutAdapter.startOutput;
  begin
    setLength(linesToWrite,0);
    outputLinesLimit:=settings.outputLinesLimit;
    bufferOffset:=0;
    hadDirectPrint:=false;
    wroteToSyn:=false;
    syn.BeginUpdate(false);
  end;

PROCEDURE T_synOutAdapter.flushBuffer;
  VAR i:longint;
  begin
    if length(linesToWrite)>=outputLinesLimit then syn.lines.clear
    else while syn.lines.count+length(linesToWrite)>outputLinesLimit do syn.lines.delete(0);
    for i:=bufferOffset to length(linesToWrite)-1+bufferOffset do syn.lines.append(linesToWrite[i mod outputLinesLimit]);
    linesToWrite:=C_EMPTY_STRING_ARRAY;
    bufferOffset:=0;
  end;

PROCEDURE T_synOutAdapter.appendInternal(CONST s:string);
  begin
    if length(linesToWrite)>=outputLinesLimit then begin
      linesToWrite[bufferOffset]:=s;
      inc(bufferOffset);
      if bufferOffset>=outputLinesLimit then bufferOffset:=0;
    end else myGenerics.append(linesToWrite,s);
    wroteToSyn:=true;
  end;

FUNCTION T_synOutAdapter.singleMessageOut(CONST m: P_storedMessage):boolean;
  PROCEDURE writeWrapped(CONST messageType:T_messageType; CONST messageList:T_arrayOfString);
    {$MACRO ON}
    {$define marker:=C_messageTypeMeta[messageType].guiMarker}
    VAR txt:string;
        tokens:T_arrayOfString;
        k:longint=0;
        first:boolean=true;
        firstInLine:boolean=true;
        message:string;
    begin
      if length(messageList)<>1 then for message in messageList do begin
        if first
        then appendInternal(marker+getPrefix(messageType      )+' '+message)
        else appendInternal(marker+getPrefix(mt_echo_continued)+' '+message);
        first:=false;
      end else begin
        message:=messageList[0];
        if wrapEcho and (syn.charsInWindow-5<length(message)) then begin
          if length(messageList)=1
          then tokens:=tokenSplit(message)
          else tokens:=message;
          while k<length(tokens) do begin
            txt:='';
            firstInLine:=true;
            while (k<length(tokens)) and (firstInLine or (length(txt)+length(tokens[k])<=syn.charsInWindow-5)) do begin
              txt:=txt+tokens[k];
              inc(k);
              firstInLine:=false;
            end;
            if first
            then appendInternal(marker+getPrefix(messageType      )+' '+txt)
            else appendInternal(marker+getPrefix(mt_echo_continued)+' '+txt);
            first:=false;
          end;
        end else begin
          if first
          then appendInternal(marker+getPrefix(messageType      )+' '+message)
          else appendInternal(marker+getPrefix(mt_echo_continued)+' '+message);
          first:=false;
        end;
      end;
    end;

  PROCEDURE clearSynAndBuffer;
    begin
      linesToWrite:=C_EMPTY_STRING_ARRAY;
      bufferOffset:=0;
      syn.lines.clear;
      wroteToSyn:=true;
    end;

  PROCEDURE processDirectPrint(CONST s:string);
    VAR c:char;
    begin
      syn.readonly:=false;
      for c in s do case c of
        #8 : syn.executeCommand(ecDeleteLastChar,c,nil);
        #13: begin
               syn.executeCommand(ecLineStart,c,nil);
               syn.executeCommand(ecOverwriteMode,c,nil);
             end
        else syn.executeCommand(ecChar,c,nil);
      end;
      syn.readonly:=true;
      hadDirectPrint:=true;
    end;

  VAR j:longint;
      s:string;
  begin
    result:=true;
    case m^.messageType of
      mt_clearConsole: clearSynAndBuffer;
      mt_printline:
        begin
          if (length(m^.messageText)>0) and (m^.messageText[0]=C_formFeedChar) then begin
            clearSynAndBuffer;
            for j:=1 to length(m^.messageText)-1 do appendInternal(m^.messageText[j]);
          end else if lastWasDirectPrint then begin
            if length(m^.messageText)>0 then begin
              processDirectPrint(m^.messageText[0]);
            end;
            for j:=1 to length(m^.messageText)-1 do appendInternal(m^.messageText[j]);
          end else for s in m^.messageText do appendInternal(s);
        end;
      mt_printdirect:
        begin
          if wroteToSyn then begin
            flushBuffer;
            wroteToSyn:=false;
            syn.executeCommand(ecEditorBottom,' ',nil);
            syn.executeCommand(ecLineStart,' ',nil);
          end;
          if not(lastWasDirectPrint) then begin
            syn.append('');
            syn.executeCommand(ecEditorBottom,' ',nil);
            syn.executeCommand(ecLineStart,' ',nil);
          end;
          for s in m^.messageText do processDirectPrint(s);
        end;
      mt_el1_note,
      mt_el1_userNote,
      mt_el2_warning,
      mt_el2_userWarning,
      mt_el3_evalError,
      mt_el3_noMatchingMain,
      mt_el3_userDefined,
      mt_el4_systemError,
      mt_profile_call_info,
      mt_timing_info: for s in m^.toString(true) do appendInternal(s);
      mt_echo_input,
      mt_echo_declaration,
      mt_echo_output: writeWrapped(m^.messageType,m^.messageText);
      else result:=false;
    end;
    if result then lastWasDirectPrint:=m^.messageType=mt_printdirect;
  end;

PROCEDURE T_synOutAdapter.doneOutput(CONST jumpDown:boolean);
  begin
    if wroteToSyn then begin
      flushBuffer;
      if not(synOwnerForm.showing) or not(synOwnerForm.visible) then begin
        synOwnerForm.Show;
        synOwnerForm.visible:=true;
      end;
      syn.EndUpdate;
      if jumpDown then begin
        syn.executeCommand(ecEditorBottom,' ',nil);
        syn.executeCommand(ecLineStart,' ',nil);
      end;
    end else begin
      if hadDirectPrint and (not(synOwnerForm.showing) or not(synOwnerForm.visible)) then begin
        synOwnerForm.Show;
        synOwnerForm.visible:=true;
      end;
      syn.EndUpdate;
    end;
    syn.enabled:=not(lastWasDirectPrint);
  end;

CONSTRUCTOR T_synOutAdapter.create(CONST owner: TForm; CONST outputEdit: TSynEdit; CONST messageTypesToInclude:T_messageTypeSet);
  begin
    inherited create(at_guiSynOutput,messageTypesToInclude);
    wrapEcho:=false;
    id:=interLockedIncrement(lastSynOutId);
    synOwnerForm:=owner;
    syn         :=outputEdit;
    //----------------------
    outputLinesLimit:=1000;
    lastWasDirectPrint:=false;
  end;

FUNCTION T_synOutAdapter.flushToGui(CONST jumpToEnd:boolean):T_messageTypeSet;
  VAR m:P_storedMessage;
  begin
    system.enterCriticalSection(cs);
    result:=[];
    startOutput;
    try
      for m in storedMessages do begin
        include(result,m^.messageType);
        singleMessageOut(m);
      end;
      clear;
    finally
      if (mt_endOfEvaluation in result) and hadDirectPrint then appendInternal('');
      doneOutput(jumpToEnd);
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_synOutAdapter.flushClear;
  VAR clearMessage:P_storedMessage;
  begin
    system.enterCriticalSection(cs);
    lastWasDirectPrint:=false;
    clear;
    new(clearMessage,create(mt_clearConsole,C_nilTokenLocation));
    append(clearMessage);
    disposeMessage(clearMessage);
    system.leaveCriticalSection(cs);
  end;

INITIALIZATION
  setLength(redirectors,0);

end.
