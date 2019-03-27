UNIT guiOutAdapters;
INTERFACE
USES SynEdit,SynEditKeyCmds,Forms,
     debugging,
     out_adapters,
     mnh_constants,basicTypes,
     mnh_settings,
     mnh_plotForm,
     mnh_messages,
     evalThread,
     synOutAdapter,
     mnhCustomForm, askDialog,
     serializationUtil;

TYPE
  T_guiEventsAdapter=object(T_abstractGuiOutAdapter)
    form:T_abstractMnhForm;
    CONSTRUCTOR create(CONST guiForm:T_abstractMnhForm);
    FUNCTION flushToGui:T_messageTypeSet; virtual;
  end;

//VAR guiOutAdapter:    T_synOutAdapter;
//    guiEventsAdapter: T_guiEventsAdapter;
//    guiAdapters:      T_messagesDistributor;

VAR outputBehavior,
    quickOutputBehavior: T_ideMessageConfig;
    outputLinesLimit:longint;

//PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST outputEdit:TSynEdit);
//FUNCTION createSecondaryAdapters(CONST outputEdit:TSynEdit):P_messagesDistributor;

FUNCTION  loadOutputSettings(VAR stream:T_bufferedInputStreamWrapper):boolean;
PROCEDURE saveOutputSettings(VAR stream:T_bufferedOutputStreamWrapper);

IMPLEMENTATION
//VAR unitIsInitialized:boolean=false;
//PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST outputEdit:TSynEdit);
//  VAR logoMessage:P_storedMessageWithText;
//  begin
//    if unitIsInitialized then exit;
//    guiOutAdapter.create(parent,outputEdit);
//    guiEventsAdapter.create(parent);
//    guiAdapters.createDistributor();
//    if outputEdit<>nil then begin
//      new(logoMessage,create(mt_printline,C_nilTokenLocation,LOGO));
//      guiOutAdapter.append(logoMessage);
//      disposeMessage(logoMessage);
//      guiAdapters.addOutAdapter(@guiOutAdapter,false);
//    end;
//    guiAdapters.addOutAdapter(@plotSystem   ,false);
//    guiAdapters.addOutAdapter(@guiEventsAdapter,false);
//    unitIsInitialized:=true;
//    out_adapters.gui_started:=true;
//    initializePlotForm(parent);
//  end;
//
//FUNCTION createSecondaryAdapters(CONST outputEdit:TSynEdit):P_messagesDistributor;
//  VAR guiAd:P_synOutAdapter;
//  begin
//    result:=nil;
//    if not(unitIsInitialized) then exit;
//    new(guiAd,create(guiOutAdapter.ownerForm,outputEdit));
//    new(result,createDistributor);
//    result^.addOutAdapter(guiAd,true);
//  end;

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

CONSTRUCTOR T_guiEventsAdapter.create(CONST guiForm: T_abstractMnhForm);
  begin
    inherited create(at_guiEventsCollector,
                    [mt_displayVariableTree,
                     mt_displayCustomForm,
                     mt_endOfEvaluation,
                     mt_guiEdit_done,
                     mt_debugger_breakpoint]);
    form:=guiForm;
  end;

FUNCTION T_guiEventsAdapter.flushToGui: T_messageTypeSet;
  VAR message:P_storedMessage;
  begin
    system.enterCriticalSection(cs);
    try
      result:=[];
      for message in storedMessages do begin
        include(result,message^.messageType);
        case message^.messageType of
          mt_guiEdit_done:        form.onEditFinished(P_editScriptTask(message));
          mt_debugger_breakpoint: form.onBreakpoint(P_debuggingSnapshot(message));
        end;
      end;
      if length(storedMessages)>0 then clear;
    finally
      system.leaveCriticalSection(cs);
      if showFormsAfter then conditionalShowCustomForms(@guiAdapters);
      if evaluationEnded then begin
        freeScriptedForms;
        form.onEndOfEvaluation;
      end;
    end;
    if plotSystem.processPendingMessages then include(result,mt_plot_addRow);
  end;

INITIALIZATION
  outputBehavior:=C_defaultIdeMessageConfig;
  quickOutputBehavior:=C_defaultIdeMessageConfig;
  outputLinesLimit:=maxLongint;

//FINALIZATION
//  if unitIsInitialized then begin
//    guiAdapters.destroy;
//    guiOutAdapter.destroy;
//    guiEventsAdapter.destroy;
//  end;
end.
