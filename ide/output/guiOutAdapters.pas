UNIT guiOutAdapters;
INTERFACE
USES SynEdit,SynEditKeyCmds,Forms,
     debugging,
     out_adapters,
     mnh_constants,basicTypes,
     mnh_settings,
     mnh_messages,
     mnhCustomForm, askDialog,
     ideLayoutUtil,
     editScripts;

TYPE
  P_guiEventsAdapter=^T_guiEventsAdapter;
  T_guiEventsAdapter=object(T_abstractGuiOutAdapter)
    form:T_mnhIdeForm;
    CONSTRUCTOR create(CONST guiForm:T_mnhIdeForm);
    FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
  end;

IMPLEMENTATION
USES sysutils,contexts,myStringUtil;
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
  VAR i:longint;
      start:double;
  begin
    start:=now;
    system.enterCriticalSection(adapterCs);
    try
      result:=[];
      for i:=0 to collectedFill-1 do begin
        include(result,collected[i]^.messageType);
        case collected[i]^.messageType of
          mt_guiEditScriptsLoaded,
          mt_guiEdit_done:        form.onEditFinished(collected[i]);
          mt_debugger_breakpoint: form.onBreakpoint(P_debuggingSnapshot(collected[i]));
          mt_endOfEvaluation    : form.onEndOfEvaluation;
        end;
      end;
      if collectedFill>0 then clear;
    finally
      system.leaveCriticalSection(adapterCs);
    end;
    if now-start>ONE_SECOND*0.1 then postIdeMessage('Flush of IDE adapter form took a long time: '+myTimeToStr(now-start),true);
  end;

end.
