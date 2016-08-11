{$ifdef includeInterface}
T_guiOutAdapter=object(T_collectingOutAdapter)
  lineIndexPerEvaluatedStatement:array of longint;
  CONSTRUCTOR create;
  DESTRUCTOR destroy; virtual;
  FUNCTION flushToGui(VAR syn:TSynEdit):boolean;
  PROCEDURE flushClear;
end;
{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_guiOutAdapter.create;
  begin
    inherited create(at_gui,true);
    setLength(lineIndexPerEvaluatedStatement,0);
  end;

DESTRUCTOR T_guiOutAdapter.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_guiOutAdapter.flushToGui(VAR syn: TSynEdit): boolean;
  VAR i,j,lineIndex,k:longint;
      instantPlotRequested:boolean=false;
  PROCEDURE writeWrapped(CONST messageType:T_messageType; CONST message:ansistring);
    VAR txt:string;
        tokens:T_arrayOfString;
        k:longint=0;
        first:boolean=true;
        firstInLine:boolean=true;
    begin
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
          then syn.lines.append(UTF8_ZERO_WIDTH_SPACE+C_messageTypeMeta[messageType]      .prefix+' '+txt)
          else syn.lines.append(UTF8_ZERO_WIDTH_SPACE+C_messageTypeMeta[mt_echo_continued].prefix+' '+txt);
          first:=false;
        end;
      end else syn.lines.append(UTF8_ZERO_WIDTH_SPACE+C_messageTypeMeta[messageType].prefix+' '+message);
    end;

  begin
    system.enterCriticalSection(cs);
    result:=length(storedMessages)>0;
    for i:=0 to length(storedMessages)-1 do with storedMessages[i] do begin
      case messageType of
        mt_evaluatedStatementInInteractiveMode: begin
          j:=strToIntDef(simpleMessage,-1);
          if (j<0) then syn.lines.clear
          else if j<length(lineIndexPerEvaluatedStatement) then begin
            lineIndex:=lineIndexPerEvaluatedStatement[j];
            for k:=syn.lines.count-1 downto lineIndex do syn.lines.delete(k);
            setLength(lineIndexPerEvaluatedStatement,j+1);
            lineIndexPerEvaluatedStatement[j]:=syn.lines.count;
          end else begin
            if j=0 then syn.lines.clear;
            setLength(lineIndexPerEvaluatedStatement,j+1);
            lineIndexPerEvaluatedStatement[j]:=syn.lines.count;
          end;
        end;
        mt_clearConsole: syn.lines.clear;
        mt_plotSettingsChanged: plotForm.pullPlotSettingsToGui;
        mt_plotCreatedWithInstantDisplay: instantPlotRequested:=true;
        mt_displayTable,mt_plotCreatedWithDeferredDisplay: begin end;
        mt_printline:
          begin
            if (length(multiMessage)>0) and (multiMessage[0]=C_formFeedChar) then begin
              syn.lines.clear;
              for j:=1 to length(multiMessage)-1 do  syn.lines.append(multiMessage[j]);
            end else for j:=0 to length(multiMessage)-1 do syn.lines.append(multiMessage[j]);
          end;
        mt_endOfEvaluation: begin
          for j:=0 to length(MnhForm.editorMeta)-1 do MnhForm.editorMeta[j].doneDebugging;
          MnhForm.updateDebugParts;
        end;
        mt_reloadRequired:  for j:=0 to length(MnhForm.editorMeta)-1 do MnhForm.editorMeta[j].reloadFile(simpleMessage);
        mt_echo_input,
        mt_echo_declaration,
        mt_echo_output: writeWrapped(messageType,simpleMessage);
        mt_timing_info:      syn.lines.append(UTF8_ZERO_WIDTH_SPACE+C_messageTypeMeta[messageType].prefix+                         ' '+(simpleMessage));
        else begin           syn.lines.append(UTF8_ZERO_WIDTH_SPACE+C_messageTypeMeta[messageType].prefix+' '+ansistring(location)+' '+(simpleMessage));
        end;
      end;
    end;
    clearMessages;
    if result then begin
      syn.ExecuteCommand(ecEditorBottom,' ',nil);
      syn.ExecuteCommand(ecLineStart,' ',nil);
    end;
    if instantPlotRequested then begin
      plotForm.doPlot();
      guiAdapters.hasMessageOfType[mt_plotCreatedWithDeferredDisplay]:=false;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_guiOutAdapter.flushClear;
  begin
    system.enterCriticalSection(cs);
    clearMessages;
    clearConsole;
    setLength(lineIndexPerEvaluatedStatement,0);
    system.leaveCriticalSection(cs);
  end;
{$endif}
