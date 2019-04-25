UNIT guiOutAdapters;
INTERFACE
USES SynEdit,SynEditKeyCmds,Forms,
     debugging,
     out_adapters,
     mnh_constants,basicTypes,
     mnh_settings,
     mnh_plotForm,
     mnh_messages,
     mnhCustomForm, askDialog,
     serializationUtil,
     ideLayoutUtil,
     editScripts;

TYPE
  P_guiEventsAdapter=^T_guiEventsAdapter;
  T_guiEventsAdapter=object(T_abstractGuiOutAdapter)
    form:T_mnhIdeForm;
    CONSTRUCTOR create(CONST guiForm:T_mnhIdeForm);
    FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
  end;

VAR outputBehavior,
    quickOutputBehavior: T_ideMessageConfig;
    outputLinesLimit:longint;

FUNCTION  loadOutputSettings(VAR stream:T_bufferedInputStreamWrapper):boolean;
PROCEDURE saveOutputSettings(VAR stream:T_bufferedOutputStreamWrapper);

IMPLEMENTATION
FUNCTION loadOutputSettings(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    {$ifdef debugMode}
    writeln('Loading output settings @',stream.streamPos);
    {$endif}
    stream.read(outputBehavior,sizeOf(outputBehavior));
    stream.read(quickOutputBehavior,sizeOf(quickOutputBehavior));
    outputLinesLimit:=stream.readLongint;
    result:=stream.allOkay and (outputLinesLimit>=0);

    if not(result) then begin
      outputBehavior     :=C_defaultIdeMessageConfig;
      quickOutputBehavior:=C_defaultIdeMessageConfig;
      outputLinesLimit:=maxLongint;
    end;
  end;

PROCEDURE saveOutputSettings(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    {$ifdef debugMode}
    writeln('Saving output settings @',stream.streamPos);
    {$endif}
    stream.write(outputBehavior,sizeOf(outputBehavior));
    stream.write(quickOutputBehavior,sizeOf(quickOutputBehavior));
    stream.writeLongint(outputLinesLimit);
  end;

CONSTRUCTOR T_guiEventsAdapter.create(CONST guiForm: T_mnhIdeForm);
  begin
    inherited create(at_guiEventsCollector,
                    [mt_endOfEvaluation,
                     mt_guiEdit_done,
                     mt_guiEditScriptsLoaded,
                     mt_debugger_breakpoint]);
    form:=guiForm;
  end;

FUNCTION T_guiEventsAdapter.flushToGui(CONST forceFlush:boolean): T_messageTypeSet;
  VAR message:P_storedMessage;
  begin
    system.enterCriticalSection(cs);
    try
      result:=[];
      for message in storedMessages do begin
        include(result,message^.messageType);
        case message^.messageType of
          mt_guiEditScriptsLoaded,
          mt_guiEdit_done:        form.onEditFinished(message);
          mt_debugger_breakpoint: form.onBreakpoint(P_debuggingSnapshot(message));
          mt_endOfEvaluation    : form.onEndOfEvaluation;
        end;
      end;
      if length(storedMessages)>0 then clear;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

INITIALIZATION
  outputBehavior:=C_defaultIdeMessageConfig;
  quickOutputBehavior:=C_defaultIdeMessageConfig;
  outputLinesLimit:=maxLongint;

end.
