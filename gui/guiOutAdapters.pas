UNIT guiOutAdapters;
INTERFACE
USES SynEdit,SynEditKeyCmds,Forms,
     myStringUtil,myGenerics,
     mnh_debugging,
     mnh_out_adapters,mnh_constants,mnh_settings,mnh_basicTypes,
     mnh_plotForm, mnh_tables,
     {$ifdef imig}mnh_imig_form,{$endif}
     mnh_messages,
     mnh_evalThread,
     synOutAdapter,
     variableTreeViews,mnhCustomForm, askDialog;

{$ifdef debugMode}
  {define debug_guiOutAdapters}
{$endif}

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

  P_guiOutAdapter=^T_guiOutAdapter;
  T_guiOutAdapter=object(T_synOutAdapter)
    flushing:boolean;
    parentForm:T_abstractMnhForm;
    CONSTRUCTOR create(CONST owner:T_abstractMnhForm; CONST outputEdit:TSynEdit; CONST displayLogo:boolean);
    DESTRUCTOR destroy; virtual;
    FUNCTION flushToGui:T_messageTypeSet; virtual;
  end;

VAR guiOutAdapter: T_guiOutAdapter;
    guiAdapters: T_messageConnector;

PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST outputEdit:TSynEdit; CONST displayLogo:boolean);
FUNCTION createSecondaryAdapters(CONST outputEdit:TSynEdit):P_messageConnector;
IMPLEMENTATION
VAR unitIsInitialized:boolean=false;
PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST outputEdit:TSynEdit; CONST displayLogo:boolean);
  begin
    if unitIsInitialized then exit;
    guiOutAdapter.create(parent,outputEdit,displayLogo);
    guiAdapters.create;
    {$ifdef imig}
    guiAdapters.addOutAdapter(@imigSystem,false);
    {$endif}
    guiAdapters.addOutAdapter(@guiOutAdapter,false);
    guiAdapters.addOutAdapter(@plotSystem   ,false);
    unitIsInitialized:=true;
    mnh_out_adapters.gui_started:=true;
    initializePlotForm;
  end;

FUNCTION createSecondaryAdapters(CONST outputEdit:TSynEdit):P_messageConnector;
  VAR guiAd:P_guiOutAdapter;
  begin
    result:=nil;
    if not(unitIsInitialized) then exit;
    new(guiAd,create(guiOutAdapter.parentForm,outputEdit,false));
    new(result,create);
    result^.addOutAdapter(guiAd,true);
  end;

CONSTRUCTOR T_guiOutAdapter.create(CONST owner:T_abstractMnhForm; CONST outputEdit:TSynEdit; CONST displayLogo:boolean);
  VAR m:P_storedMessageWithText;
  FUNCTION allTypesApartFromPlotting:T_messageTypeSet;
    VAR typ:T_messageType;
    begin
      result:=[];
      for typ in T_messageType do
        if (C_messageTypeMeta[typ].mClass<>mc_plot)
           {$ifdef imig} and
           (C_messageTypeMeta[typ].mClass<>mc_image)
           {$endif}
        then include(result,typ);
    end;

  begin
    inherited create(owner,outputEdit,allTypesApartFromPlotting);
    parentForm:=owner;
    flushing:=false;
    if not(displayLogo) then exit;
    new(m,create(mt_printline,C_nilTokenLocation,LOGO));
    append(m);
    disposeMessage(m);
  end;

DESTRUCTOR T_guiOutAdapter.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_guiOutAdapter.flushToGui: T_messageTypeSet;

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
        if settings.wordWrapEcho and (syn.charsInWindow-5<length(message)) then begin
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

  PROCEDURE appendPrintNothing;
    VAR m:P_storedMessageWithText;
    begin
      new(m,create(mt_printline,C_nilTokenLocation,''));
      append(m);
    end;

  VAR showFormsAfter:boolean=false;
      evaluationEnded:boolean=false;
      s:string;
      message:P_storedMessage;
  begin
    system.enterCriticalSection(cs);
    if flushing then begin
      {$ifdef debug_guiOutAdapters}writeln(stdErr,'        DEBUG: Already flushing!');{$endif}
      system.leaveCriticalSection(cs);
      exit([]);
    end;
    try
      flushing:=true;
      result:=[];
      if length(storedMessages)>0 then outputLinesLimit:=settings.outputLinesLimit;
      startOutput;
      for message in storedMessages do
      if not(message^.messageType in redirectedMessages) then begin
        {$ifdef debug_guiOutAdapters}writeln(stdErr,'        DEBUG: GUI adapter processes message type ',message^.messageType);{$endif}
        include(result,message^.messageType);
        case message^.messageType of
          mt_displayTable: conditionalShowTables;
          mt_displayVariableTree: conditionalShowVarTrees;
          mt_displayCustomForm: showFormsAfter:=true;
          mt_endOfEvaluation: evaluationEnded:=true;
          mt_guiEdit_done: parentForm.onEditFinished(P_editScriptTask(message));
          mt_debugger_breakpoint: parentForm.onBreakpoint(P_debuggingSnapshot(message));
          mt_echo_input,
          mt_echo_declaration,
          mt_echo_output: writeWrapped(message^.messageType,message^.messageText);
          else if not(singleMessageOut(message))
          then for s in message^.toString(true) do appendInternal(s);
        end;
      end;
      if length(storedMessages)>0 then clear;
    finally
      doneOutput;
      flushing:=false;
      system.leaveCriticalSection(cs);
      if showFormsAfter then conditionalShowCustomForms(guiAdapters);
      if evaluationEnded then begin
        freeScriptedForms;
        if directPrintFlag then appendPrintNothing;
        syn.enabled:=true;
        parentForm.onEndOfEvaluation;
      end;
    end;
    if plotSystem.processPendingMessages then include(result,mt_plot_addRow);
    if plotSystem.requiresFastPolling    then parentForm.triggerFastPolling;
    {$ifdef imig}
    imigSystem.processPendingMessages;
    {$endif}
  end;

FINALIZATION
  if unitIsInitialized then begin
    guiAdapters.destroy;
    guiOutAdapter.destroy;
  end;
end.
