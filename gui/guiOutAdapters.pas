UNIT guiOutAdapters;
INTERFACE
USES SynEdit,SynEditKeyCmds,Forms,
     myStringUtil,myGenerics,
     mnh_out_adapters,mnh_constants,mnh_settings,mnh_basicTypes,
     mnh_plotForm, mnh_tables,
     synOutAdapter,
     variableTreeViews,mnhCustomForm;
{$ifdef debugMode}
  {$define debug_guiOutAdapters}
{$endif}

TYPE
  T_abstractMnhForm=class(TForm)
    public
      PROCEDURE onEditFinished(CONST data:pointer; CONST successful:boolean); virtual; abstract;
      PROCEDURE onBreakpoint  (CONST data:pointer);                           virtual; abstract;
      PROCEDURE onDebuggerEvent;                                              virtual; abstract;
      PROCEDURE onEndOfEvaluation;                                            virtual; abstract;
      PROCEDURE triggerFastPolling;                                           virtual; abstract;
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
    guiAdapters: T_adapters;

PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST outputEdit:TSynEdit; CONST displayLogo:boolean);
FUNCTION createSecondaryAdapters(CONST outputEdit:TSynEdit):P_adapters;
IMPLEMENTATION
VAR unitIsInitialized:boolean=false;
PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST outputEdit:TSynEdit; CONST displayLogo:boolean);
  begin
    if unitIsInitialized then exit;
    guiOutAdapter.create(parent,outputEdit,displayLogo);
    guiAdapters.create;
    mnh_plotForm.guiAdapters:=@guiAdapters;
    guiAdapters.addOutAdapter(@guiOutAdapter,false);
    unitIsInitialized:=true;
    mnh_out_adapters.gui_started:=true;
    initializePlotForm;
  end;

FUNCTION createSecondaryAdapters(CONST outputEdit:TSynEdit):P_adapters;
  VAR guiAd:P_guiOutAdapter;
  begin
    result:=nil;
    if not(unitIsInitialized) then exit;
    new(guiAd,create(guiOutAdapter.parentForm,outputEdit,false));
    new(result,create);
    result^.addOutAdapter(guiAd,true);
  end;

CONSTRUCTOR T_guiOutAdapter.create(CONST owner:T_abstractMnhForm; CONST outputEdit:TSynEdit; CONST displayLogo:boolean);
  VAR i:longint;
      m:T_storedMessage;
  begin
    inherited create(owner,outputEdit,C_collectAllOutputBehavior);
    parentForm:=owner;
    flushing:=false;
    if not(displayLogo) then exit;
    with m do begin
      messageType:=mt_printline;
      location:=C_nilTokenLocation;
      setLength(messageText,8);
      for i:=0 to 7 do messageText[i]:=LOGO[i];
      data:=nil;
    end;
    append(m);
  end;

DESTRUCTOR T_guiOutAdapter.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_guiOutAdapter.flushToGui: T_messageTypeSet;

  PROCEDURE writeWrapped(CONST messageType:T_messageType; CONST messageList:T_arrayOfString);
    {$MACRO ON}
    {$define marker:=C_messageClassMeta[C_messageTypeMeta[messageType].mClass].guiMarker}
    VAR txt:string;
        tokens:T_arrayOfString;
        k:longint=0;
        first:boolean=true;
        firstInLine:boolean=true;
        message:string;
    begin
      if length(messageList)<>1 then for message in messageList do begin
        if first
        then appendInternal(marker+C_messageTypeMeta[messageType]      .prefix+' '+message)
        else appendInternal(marker+C_messageTypeMeta[mt_echo_continued].prefix+' '+message);
        first:=false;
      end else begin
        message:=messageList[0];
        if settings.value^.wordWrapEcho and (syn.charsInWindow-5<length(message)) then begin
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
            then appendInternal(marker+C_messageTypeMeta[messageType]      .prefix+' '+txt)
            else appendInternal(marker+C_messageTypeMeta[mt_echo_continued].prefix+' '+txt);
            first:=false;
          end;
        end else begin
          if first
          then appendInternal(marker+C_messageTypeMeta[messageType]      .prefix+' '+message)
          else appendInternal(marker+C_messageTypeMeta[mt_echo_continued].prefix+' '+message);
          first:=false;
        end;
      end;
    end;

  VAR i:longint;
      showFormsAfter:boolean=false;
      evaluationEnded:boolean=false;
      s:string;
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
      if length(storedMessages)>0 then outputLinesLimit:=settings.value^.outputLinesLimit;
      startOutput;
      for i:=0 to length(storedMessages)-1 do with storedMessages[i] do
      if not(storedMessages[i].messageType in redirectedMessages) then begin
        {$ifdef debug_guiOutAdapters}writeln(stdErr,'        DEBUG: GUI adapter processes message type ',storedMessages[i].messageType);{$endif}
        include(result,messageType);
        case messageType of
          mt_plotSettingsChanged: plotForm.pullPlotSettingsToGui;
          mt_plotCreatedWithInstantDisplay: begin
            plotForm.doPlot(true);
            parentForm.triggerFastPolling;
          end;
          mt_displayTable: conditionalShowTables;
          mt_displayTreeView: conditionalShowVarTrees;
          mt_displayCustomDialog: showFormsAfter:=true;
          mt_plotCreatedWithDeferredDisplay: begin end;
          mt_endOfEvaluation: evaluationEnded:=true;
          mt_gui_editScriptSucceeded  : parentForm.onEditFinished(data,true);
          mt_gui_editScriptFailed     : parentForm.onEditFinished(data,false);
          mt_gui_breakpointEncountered: parentForm.onBreakpoint  (data);
          mt_echo_input,
          mt_echo_declaration,
          mt_echo_output: writeWrapped(messageType,messageText);
          else if not(singleMessageOut(storedMessages[i]))
          then for s in defaultFormatting(storedMessages[i],true) do appendInternal(s);
        end;
      end;
      if length(storedMessages)>0 then clear;
    finally
      doneOutput;
      flushing:=false;
      system.leaveCriticalSection(cs);
      if showFormsAfter then conditionalShowCustomForms(guiAdapters);
      if evaluationEnded then begin
        if plotFormIsInitialized and plotForm.AnimationGroupBox.visible or guiAdapters.isDeferredPlotLogged then plotForm.doPlot();
        freeScriptedForms;
        if directPrintFlag then append(message(mt_printline,'',C_nilTokenLocation));
        syn.enabled:=true;
        parentForm.onEndOfEvaluation;
      end;
    end;
  end;

FINALIZATION
  if unitIsInitialized then begin
    guiAdapters.destroy;
    guiOutAdapter.destroy;
  end;
end.
