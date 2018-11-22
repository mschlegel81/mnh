UNIT guiOutAdapters;
INTERFACE
USES SynEdit,SynEditKeyCmds,Forms,
     mnh_debugging,
     mnh_out_adapters,mnh_constants,mnh_settings,mnh_basicTypes,
     mnh_plotForm, mnh_tables,
     mnh_imig_form,
     mnh_messages,
     mnh_evalThread,
     synOutAdapter,
     variableTreeViews,mnhCustomForm, askDialog;

TYPE
  T_abstractMnhForm=class(TForm)
    public
      PROCEDURE onEditFinished(CONST data:P_editScriptTask   ); virtual; abstract;
      PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); virtual; abstract;
      PROCEDURE onDebuggerEvent;                                virtual; abstract;
      PROCEDURE onEndOfEvaluation;                              virtual; abstract;
      PROCEDURE triggerFastPolling;                             virtual; abstract;
      PROCEDURE activeFileChanged(CONST newCaption:string; CONST isMnhFile:boolean; CONST isPseudoFile:boolean); virtual; abstract;
  end;

  T_guiEventsAdapter=object(T_collectingOutAdapter)
    form:T_abstractMnhForm;
    CONSTRUCTOR create(CONST guiForm:T_abstractMnhForm);
    FUNCTION flushToGui:T_messageTypeSet;
  end;

VAR guiOutAdapter:    T_synOutAdapter;
    guiEventsAdapter: T_guiEventsAdapter;
    guiAdapters:      T_messagesDistributor;

PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST outputEdit:TSynEdit);
FUNCTION createSecondaryAdapters(CONST outputEdit:TSynEdit):P_messagesDistributor;
IMPLEMENTATION
VAR unitIsInitialized:boolean=false;
PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST outputEdit:TSynEdit);
  VAR logoMessage:P_storedMessageWithText;
  begin
    if unitIsInitialized then exit;
    guiOutAdapter.create(parent,outputEdit);
    guiEventsAdapter.create(parent);
    guiAdapters.createDistributor();
    guiAdapters.addOutAdapter(@imigSystem,false);
    if outputEdit<>nil then begin
      new(logoMessage,create(mt_printline,C_nilTokenLocation,LOGO));
      guiOutAdapter.append(logoMessage);
      disposeMessage(logoMessage);
      guiAdapters.addOutAdapter(@guiOutAdapter,false);
    end;
    guiAdapters.addOutAdapter(@plotSystem   ,false);
    guiAdapters.addOutAdapter(@guiEventsAdapter,false);
    unitIsInitialized:=true;
    mnh_out_adapters.gui_started:=true;
    initializePlotForm;
  end;

FUNCTION createSecondaryAdapters(CONST outputEdit:TSynEdit):P_messagesDistributor;
  VAR guiAd:P_synOutAdapter;
  begin
    result:=nil;
    if not(unitIsInitialized) then exit;
    new(guiAd,create(guiOutAdapter.ownerForm,outputEdit));
    new(result,createDistributor);
    result^.addOutAdapter(guiAd,true);
  end;

CONSTRUCTOR T_guiEventsAdapter.create(CONST guiForm: T_abstractMnhForm);
  begin
    inherited create(at_gui,[mt_displayTable,
                             mt_displayVariableTree,
                             mt_displayCustomForm,
                             mt_endOfEvaluation,
                             mt_guiEdit_done,
                             mt_debugger_breakpoint]);
    form:=guiForm;
  end;

FUNCTION T_guiEventsAdapter.flushToGui: T_messageTypeSet;
  VAR showFormsAfter:boolean=false;
      evaluationEnded:boolean=false;
      message:P_storedMessage;
  begin
    system.enterCriticalSection(cs);
    try
      result:=[];
      for message in storedMessages do begin
        include(result,message^.messageType);
        case message^.messageType of
          mt_displayTable:        conditionalShowTables;
          mt_displayVariableTree: conditionalShowVarTrees;
          mt_displayCustomForm:   showFormsAfter:=true;
          mt_endOfEvaluation:     evaluationEnded:=true;
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
    if plotSystem.requiresFastPolling    then form.triggerFastPolling;
    imigSystem.processPendingMessages;
  end;

FINALIZATION
  if unitIsInitialized then begin
    guiAdapters.destroy;
    guiOutAdapter.destroy;
    guiEventsAdapter.destroy;
  end;
end.
