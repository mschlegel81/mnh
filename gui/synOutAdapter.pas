UNIT synOutAdapter;
INTERFACE
USES
  sysutils, SynEdit, Forms,
  SynEditKeyCmds,
  myStringUtil,
  mnh_constants,
  myGenerics,mnh_out_adapters;
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
                                              mt_el3_stackTrace,
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
      FUNCTION singleMessageOut(CONST m:T_storedMessage):boolean;
      PROCEDURE doneOutput;
    public
      CONSTRUCTOR create(CONST owner:TForm; CONST outputEdit:TSynEdit;
                         CONST messageTypesToInclude:T_messageTypeSet=[mt_clearConsole,
                                                                       mt_printline,
                                                                       mt_printdirect,
                                                                       mt_el1_note,
                                                                       mt_el1_userNote,
                                                                       mt_el2_warning,
                                                                       mt_el2_userWarning]);
      FUNCTION flushToGui:T_messageTypeSet; virtual;
      PROPERTY directPrintFlag:boolean read lastWasDirectPrint;
      PROCEDURE flushClear;
  end;

PROCEDURE registerRedirector(CONST syn:P_synOutAdapter);
PROCEDURE unregisterRedirector(CONST syn:P_synOutAdapter);
FUNCTION redirectedMessages:T_messageTypeSet;
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

PROCEDURE T_synOutAdapter.startOutput;
  begin
    setLength(linesToWrite,0);
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

FUNCTION T_synOutAdapter.singleMessageOut(CONST m: T_storedMessage):boolean;
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
        #8 : syn.ExecuteCommand(ecDeleteLastChar,c,nil);
        #13: begin
               syn.ExecuteCommand(ecLineStart,c,nil);
               syn.ExecuteCommand(ecOverwriteMode,c,nil);
             end
        else syn.ExecuteCommand(ecChar,c,nil);
      end;
      syn.readonly:=true;
      hadDirectPrint:=true;
    end;

  VAR j:longint;
      s:string;
  begin
    result:=true;
    case m.messageType of
      mt_clearConsole: clearSynAndBuffer;
      mt_printline:
        begin
          if (length(m.messageText)>0) and (m.messageText[0]=C_formFeedChar) then begin
            clearSynAndBuffer;
            for j:=1 to length(m.messageText)-1 do appendInternal(m.messageText[j]);
          end else if lastWasDirectPrint then begin
            if length(m.messageText)>0 then begin
              processDirectPrint(m.messageText[0]);
            end;
            for j:=1 to length(m.messageText)-1 do appendInternal(m.messageText[j]);
          end else for j:=0 to length(m.messageText)-1 do appendInternal(m.messageText[j]);
        end;
      mt_printdirect:
        begin
          if wroteToSyn then begin
            flushBuffer;
            wroteToSyn:=false;
            syn.ExecuteCommand(ecEditorBottom,' ',nil);
            syn.ExecuteCommand(ecLineStart,' ',nil);
          end;
          if not(lastWasDirectPrint) then begin
            syn.append('');
            syn.ExecuteCommand(ecEditorBottom,' ',nil);
            syn.ExecuteCommand(ecLineStart,' ',nil);
          end;
          for j:=0 to length(m.messageText)-1 do processDirectPrint(m.messageText[j]);
        end;
      mt_el1_note,
      mt_el1_userNote,
      mt_el2_warning,
      mt_el2_userWarning,
      mt_el3_evalError,
      mt_el3_noMatchingMain,
      mt_el3_stackTrace,
      mt_el3_userDefined,
      mt_el4_systemError,
      mt_timing_info: for s in defaultFormatting(m,true) do appendInternal(s);
      else result:=false;
    end;
    if result then lastWasDirectPrint:=m.messageType=mt_printdirect;
  end;

PROCEDURE T_synOutAdapter.doneOutput;
  begin
    if wroteToSyn then begin
      flushBuffer;
      if not(synOwnerForm.showing) or not(synOwnerForm.visible) then begin
        synOwnerForm.Show;
        synOwnerForm.visible:=true;
      end;
      syn.EndUpdate;
      syn.ExecuteCommand(ecEditorBottom,' ',nil);
      syn.ExecuteCommand(ecLineStart,' ',nil);
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
    inherited create(at_gui,messageTypesToInclude);
    id:=interLockedIncrement(lastSynOutId);
    synOwnerForm:=owner;
    syn         :=outputEdit;
    //----------------------
    outputLinesLimit:=1000;
    lastWasDirectPrint:=false;
  end;

FUNCTION T_synOutAdapter.flushToGui:T_messageTypeSet;
  VAR m:T_storedMessage;
  begin
    system.enterCriticalSection(cs);
    result:=[];
    startOutput;
    try
      for m in storedMessages do begin
        include(result,m.messageType);
        singleMessageOut(m);
      end;
      clear;
    finally
      doneOutput;
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_synOutAdapter.flushClear;
  begin
    system.enterCriticalSection(cs);
    lastWasDirectPrint:=false;
    clear;
    append(clearConsoleMessage);
    system.leaveCriticalSection(cs);
  end;

INITIALIZATION
  setLength(redirectors,0);

end.
