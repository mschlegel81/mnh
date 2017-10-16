UNIT guiOutAdapters;
INTERFACE
USES SynEdit,SynEditKeyCmds,Forms,
     myStringUtil,myGenerics,
     mnh_out_adapters,mnh_constants,mnh_settings,mnh_basicTypes,
     mnh_plotForm, mnh_tables, dynamicPlotting;
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
  T_guiOutAdapter=object(T_collectingOutAdapter)
    flushing:boolean;
    parentForm:T_abstractMnhForm;
    lastWasDirectPrint:boolean;
    CONSTRUCTOR create(CONST owner:T_abstractMnhForm; CONST displayLogo:boolean);
    DESTRUCTOR destroy; virtual;
    FUNCTION flushToGui(VAR syn:TSynEdit):boolean;
    PROCEDURE flushClear;
  end;

VAR guiOutAdapter: T_guiOutAdapter;
    guiAdapters: T_adapters;

PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST displayLogo:boolean);
FUNCTION createSecondaryAdapters:P_adapters;
IMPLEMENTATION
VAR unitIsInitialized:boolean=false;
PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST displayLogo:boolean);
  begin
    if unitIsInitialized then exit;
    guiOutAdapter.create(parent,displayLogo);
    guiAdapters.create;
    mnh_plotForm.guiAdapters:=@guiAdapters;
    guiAdapters.addOutAdapter(@guiOutAdapter,false);
    unitIsInitialized:=true;
    mnh_out_adapters.gui_started:=true;
    initializeDynamicPlotting;
    initializePlotForm;
  end;

FUNCTION createSecondaryAdapters:P_adapters;
  VAR guiAd:P_guiOutAdapter;
  begin
    if not(unitIsInitialized) then exit;
    new(guiAd,create(guiOutAdapter.parentForm,false));
    new(result,create);
    result^.addOutAdapter(guiAd,true);
  end;

CONSTRUCTOR T_guiOutAdapter.create(CONST owner:T_abstractMnhForm; CONST displayLogo:boolean);
  VAR i:longint;
      m:T_storedMessage;
  begin
    inherited create(at_gui,C_collectAllOutputBehavior);
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

FUNCTION T_guiOutAdapter.flushToGui(VAR syn: TSynEdit): boolean;
  VAR i,j:longint;
      outputLinesLimit:longint=0;
      wroteToSyn:boolean=false;
      s:string;

      linesToWrite:T_arrayOfString;
      bufferOffset:longint=0;

  PROCEDURE appendInternal(CONST s:string);
    begin
      if length(linesToWrite)>=outputLinesLimit then begin
        linesToWrite[bufferOffset]:=s;
        inc(bufferOffset);
        if bufferOffset>=outputLinesLimit then bufferOffset:=0;
      end else myGenerics.append(linesToWrite,s);
      wroteToSyn:=true;
    end;

  PROCEDURE flushBuffer;
    VAR i:longint;
    begin
      if length(linesToWrite)>=outputLinesLimit then syn.lines.clear
      else while syn.lines.count+length(linesToWrite)>outputLinesLimit do syn.lines.delete(0);
      for i:=bufferOffset to length(linesToWrite)-1+bufferOffset do syn.lines.append(linesToWrite[i mod outputLinesLimit]);
      linesToWrite:=C_EMPTY_STRING_ARRAY;
      bufferOffset:=0;
    end;

  PROCEDURE clearSynAndBuffer;
    begin
      linesToWrite:=C_EMPTY_STRING_ARRAY;
      bufferOffset:=0;
      syn.lines.clear;
      wroteToSyn:=true;
    end;

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
    end;

  begin
    system.enterCriticalSection(cs);
    if flushing then begin
      {$ifdef debugMode}writeln(stdErr,'        DEBUG: Already flushing!');{$endif}
      system.leaveCriticalSection(cs);
      exit(false);
    end;
    flushing:=true;
    result:=length(storedMessages)>0;
    if result then outputLinesLimit:=settings.value^.outputLinesLimit;
    linesToWrite:=C_EMPTY_STRING_ARRAY;
    for i:=0 to length(storedMessages)-1 do with storedMessages[i] do begin
      case messageType of
        mt_clearConsole: clearSynAndBuffer;
        mt_plotSettingsChanged: plotForm.pullPlotSettingsToGui;
        mt_plotCreatedWithInstantDisplay: begin
          plotForm.doPlot(true);
          parentForm.triggerFastPolling;
        end;
        mt_displayTable: conditionalShowTables;
        mt_plotCreatedWithDeferredDisplay: begin end;
        mt_printline:
          begin
            if (length(messageText)>0) and (messageText[0]=C_formFeedChar) then begin
              clearSynAndBuffer;
              for j:=1 to length(messageText)-1 do appendInternal(messageText[j]);
            end else if lastWasDirectPrint then begin
              if length(messageText)>0 then begin
                processDirectPrint(messageText[0]);
              end;
              for j:=1 to length(messageText)-1 do appendInternal(messageText[j]);
            end else for j:=0 to length(messageText)-1 do appendInternal(messageText[j]);
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
            for j:=0 to length(messageText)-1 do processDirectPrint(messageText[j]);
          end;
        mt_endOfEvaluation: begin
          if plotForm.InteractionPanel.visible or guiAdapters.isDeferredPlotLogged then plotForm.doPlot();
          parentForm.onEndOfEvaluation;
        end;
        mt_gui_editScriptSucceeded  : parentForm.onEditFinished(data,true);
        mt_gui_editScriptFailed     : parentForm.onEditFinished(data,false);
        mt_gui_breakpointEncountered: parentForm.onBreakpoint  (data);
        mt_echo_input,
        mt_echo_declaration,
        mt_echo_output: writeWrapped(messageType,messageText);
        else for s in defaultFormatting(storedMessages[i],true) do appendInternal(s);
      end;
      if messageType in [mt_clearConsole..mt_timing_info] then lastWasDirectPrint:=messageType=mt_printdirect;
    end;
    if result then clear;
    if wroteToSyn then begin
      flushBuffer;
      if not(parentForm.showing) or not(parentForm.visible) then begin
        parentForm.Show;
        parentForm.visible:=true;
        {$ifdef debugMode}
        writeln(stdErr,'        DEBUG: mnh form show triggered');
        {$endif}
      end;
      syn.ExecuteCommand(ecEditorBottom,' ',nil);
      syn.ExecuteCommand(ecLineStart,' ',nil);
    end;
    flushing:=false;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_guiOutAdapter.flushClear;
  begin
    system.enterCriticalSection(cs);
    lastWasDirectPrint:=false;
    clear;
    append(clearConsoleMessage);
    system.leaveCriticalSection(cs);
  end;

FINALIZATION
  if unitIsInitialized then begin
    guiAdapters.destroy;
    guiOutAdapter.destroy;
  end;
end.
