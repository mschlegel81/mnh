UNIT guiOutAdapters;
INTERFACE
USES SynEdit,SynEditKeyCmds,Forms,
     myStringUtil,myGenerics,
     mnh_out_adapters,mnh_constants,mnh_settings,mnh_basicTypes,
     mnh_plotForm, mnh_tables{$ifdef imig},mnh_imig_form{$endif};

TYPE
  T_abstractMnhForm=class(TForm)
    PROCEDURE onEndOfEvaluation; virtual; abstract;
  end;

  T_guiOutAdapter=object(T_collectingOutAdapter)
    flushing:boolean;
    parentForm:T_abstractMnhForm;
    CONSTRUCTOR create(CONST owner:T_abstractMnhForm; CONST displayLogo:boolean);
    DESTRUCTOR destroy; virtual;
    FUNCTION flushToGui(VAR syn:TSynEdit):boolean;
    PROCEDURE flushClear;
  end;

VAR guiOutAdapter: T_guiOutAdapter;
    guiAdapters: T_adapters;

PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST displayLogo:boolean);
IMPLEMENTATION
VAR unitIsInitialized:boolean=false;
PROCEDURE initGuiOutAdapters(CONST parent:T_abstractMnhForm; CONST displayLogo:boolean);
  begin
    if unitIsInitialized then exit;
    guiOutAdapter.create(parent,displayLogo);
    guiAdapters.create;
    mnh_plotForm.guiAdapters:=@guiAdapters;
    {$ifdef imig}
    mnh_imig_form.guiAdapters:=@guiAdapters;
    {$endif}
    guiAdapters.addOutAdapter(@guiOutAdapter,false);
    unitIsInitialized:=true;
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
      data:='';
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
    VAR txt:string;
        tokens:T_arrayOfString;
        k:longint=0;
        first:boolean=true;
        firstInLine:boolean=true;
        message:string;
    begin
      for message in messageList do begin
        if settings.value^.wordWrapEcho and (syn.charsInWindow-5<length(message)) then begin
          tokens:=tokenSplit(message);
          while k<length(tokens) do begin
            txt:='';
            firstInLine:=true;
            while (k<length(tokens)) and (firstInLine or (length(txt)+length(tokens[k])<=syn.charsInWindow-5)) do begin
              txt:=txt+tokens[k];
              inc(k);
              firstInLine:=false;
            end;
            if first
            then appendInternal(C_messageClassMeta[C_messageTypeMeta[messageType].mClass].guiMarker+C_messageTypeMeta[messageType]      .prefix+' '+txt)
            else appendInternal(C_messageClassMeta[C_messageTypeMeta[messageType].mClass].guiMarker+C_messageTypeMeta[mt_echo_continued].prefix+' '+txt);
            first:=false;
          end;
        end else begin
          if first
          then appendInternal(C_messageClassMeta[C_messageTypeMeta[messageType].mClass].guiMarker+C_messageTypeMeta[messageType]      .prefix+' '+message)
          else appendInternal(C_messageClassMeta[C_messageTypeMeta[messageType].mClass].guiMarker+C_messageTypeMeta[mt_echo_continued].prefix+' '+message);
          first:=false;
        end;
      end;
    end;

  begin
    system.enterCriticalSection(cs);
    if flushing then begin
      {$ifdef debugMode}writeln(stdErr,'Already flushing!');{$endif}
      system.leaveCriticalSection(cs);
      exit(false);
    end;
    flushing:=true;
    result:=length(storedMessages)>0;
    if result then outputLinesLimit:=settings.value^.outputLinesLimit;
    linesToWrite:=C_EMPTY_STRING_ARRAY;
    for i:=0 to length(storedMessages)-1 do with storedMessages[i] do begin
      {$ifdef debugMode}
      writeln(stdErr,'guiOutAdapter: Processing message ',i,'/',length(storedMessages),': ',messageType);
      {$endif}
      case messageType of
        mt_clearConsole: clearSynAndBuffer;
        mt_plotSettingsChanged: plotForm.pullPlotSettingsToGui;
        mt_plotCreatedWithInstantDisplay: plotForm.doPlot();
        mt_displayTable: conditionalShowTables;
        {$ifdef imig}
        mt_displayImage: DisplayImageForm.displayCurrentImage;
        {$endif}
        mt_plotCreatedWithDeferredDisplay: begin end;
        mt_printline:
          begin
            if (length(messageText)>0) and (messageText[0]=C_formFeedChar) then begin
              clearSynAndBuffer;
              for j:=1 to length(messageText)-1 do appendInternal(messageText[j]);
            end else for j:=0 to length(messageText)-1 do appendInternal(messageText[j]);
          end;
        mt_endOfEvaluation: parentForm.onEndOfEvaluation;
        mt_echo_input,
        mt_echo_declaration,
        mt_echo_output: writeWrapped(messageType,messageText);
        else for s in defaultFormatting(storedMessages[i],true) do appendInternal(s);
      end;
    end;
    if result then clear;
    if wroteToSyn then begin
      flushBuffer;
      if not(parentForm.showing) or not(parentForm.visible) then begin
        parentForm.Show;
        parentForm.visible:=true;
        {$ifdef debugMode}
        writeln(stdErr,'mnh form show triggered');
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
