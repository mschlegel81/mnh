// MIT License
//
// Copyright (c) 2016 Martin Schlegel
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

UNIT mnh_evalThread;
INTERFACE
USES FileUtil,sysutils,myGenerics,mnh_packages,mnh_out_adapters,Classes,mnh_constants,mnh_tokLoc,mnh_funcs,mnh_litVar,
     myStringUtil,mnh_tokens,mnh_contexts,mnh_doc,mnh_cmdLineInterpretation, LazUTF8, mnh_fileWrappers;
TYPE
  T_evalRequest    =(er_none,er_evaluate,er_evaluateInteractive,er_callMain,er_reEvaluateWithGUI,er_die);
  T_evaluationState=(es_dead,es_idle,es_running);

  T_tokenInfo=record
    tokenText, tokenExplanation:ansistring;
    location,
    startLoc,endLoc:T_searchTokenLocation;
  end;

  P_evaluator=^T_evaluator;

  { T_evaluator }

  T_evaluator=object
    private
      cs:TRTLCriticalSection;
      request:T_evalRequest;
      state:T_evaluationState;
      package:T_package;
      thread:TThreadFunc;
      adapter:P_adapters;
      endOfEvaluationText:ansistring;
      mainParameters:T_arrayOfString;
      startOfEvaluation:double;
      firstErrorLocation:T_searchTokenLocation;
      PROCEDURE ensureThread;
      PROCEDURE threadStarted;
      PROCEDURE threadStopped;
      PROCEDURE preEval;
      PROCEDURE postEval(CONST includeLists:boolean=true);
      FUNCTION parametersForMainCall:T_arrayOfString;
      FUNCTION pendingRequest:T_evalRequest;
    public
      CONSTRUCTOR create(CONST adapters:P_adapters; threadFunc:TThreadFunc);
      DESTRUCTOR destroy;
      PROCEDURE haltEvaluation;

      PROCEDURE reEvaluateWithGUI;
      PROCEDURE evaluate(CONST path:ansistring; CONST L: TStrings; CONST interactive:boolean);
      PROCEDURE callMain(CONST path:ansistring; CONST L: TStrings; params: ansistring);
      FUNCTION evaluationRunning: boolean;
      FUNCTION getCodeProvider:P_codeProvider;
      PROCEDURE explainIdentifier(CONST fullLine:ansistring; CONST CaretY,CaretX:longint; VAR info:T_tokenInfo);
      PROCEDURE reportVariables(VAR report:T_variableReport);
      FUNCTION getEndOfEvaluationText:ansistring;
      FUNCTION getFirstErrorLocation:T_searchTokenLocation;
  end;

VAR userRules,
    completionList:T_listOfString;
    runEvaluator,
    docEvaluator:T_evaluator;

PROCEDURE initIntrinsicRuleList;
PROCEDURE initUnit(CONST guiAdapters:P_adapters);

IMPLEMENTATION
VAR unitIsInitialized:boolean=false;
    silentAdapters:P_adapters;
    intrinsicRulesForCompletion:T_listOfString;

{$WARN 5024 OFF}
FUNCTION main(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=250;
  VAR sleepTime:longint=0;
      mainEvaluationContext:T_evaluationContext;

  begin with P_evaluator(p)^ do begin
    mainEvaluationContext.createNormalContext(adapter);
    result:=0;
    threadStarted;
    repeat
      case pendingRequest of
        er_evaluate: begin
          preEval;
          sleepTime:=0;
          package.load(lu_forDirectExecution,mainEvaluationContext,C_EMPTY_STRING_ARRAY);
          postEval(false);
        end;
        er_evaluateInteractive: begin
          preEval;
          sleepTime:=0;
          package.load(lu_interactiveMode,mainEvaluationContext,C_EMPTY_STRING_ARRAY);
          postEval(false);
        end;
        er_callMain: begin
          preEval;
          sleepTime:=0;
          package.load(lu_forCallingMain,mainEvaluationContext,parametersForMainCall);
          postEval(false);
        end;
        er_reEvaluateWithGUI: begin
          preEval;
          sleepTime:=0;
          package.setSourcePath(getFileOrCommandToInterpretFromCommandLine);
          setupOutputBehaviour(mainEvaluationContext.adapters^);
          package.load(lu_forCallingMain,mainEvaluationContext,parametersForMainCall);
          postEval(false);
        end;
        else begin
          if sleepTime<MAX_SLEEP_TIME then inc(sleepTime);
          sleep(sleepTime);
        end;
      end;
    until (pendingRequest=er_die);
    mainEvaluationContext.destroy;
    threadStopped;
  end; end;

FUNCTION docMain(p:pointer):ptrint;
  CONST MAX_SLEEP_TIME=250;
  VAR mainEvaluationContext:T_evaluationContext;
      i:longint;

  begin with P_evaluator(p)^ do begin
    mainEvaluationContext.createNormalContext(adapter);
    result:=0;
    threadStarted;
    repeat
      if not(currentlyDebugging) and (request in [er_evaluate,er_evaluateInteractive,er_callMain,er_reEvaluateWithGUI]) then begin
        preEval;
        package.load(lu_forDocGeneration,mainEvaluationContext,C_EMPTY_STRING_ARRAY);
        firstErrorLocation.column:=-1;
        firstErrorLocation.line:=-1;
        for i:=length(P_collectingOutAdapter(adapter^.getAdapter(0))^.storedMessages)-1 downto 0 do
        with P_collectingOutAdapter(adapter^.getAdapter(0))^.storedMessages[i] do
        if (location.fileName=package.getPath) and (C_errorLevelForMessageType[messageType]>=2) then begin
          firstErrorLocation:=location;
        end;
        writeln('First error ',ansistring(firstErrorLocation));
        postEval;
      end;
      ThreadSwitch;
      sleep(MAX_SLEEP_TIME);
    until (pendingRequest=er_die);
    mainEvaluationContext.destroy;
    threadStopped;
  end; end;

PROCEDURE initIntrinsicRuleList;
  VAR ids:T_arrayOfString;
      i:longint;
      tt:T_tokenType;
  begin
    ids:=mnh_funcs.intrinsicRuleMap.keySet;
    intrinsicRulesForCompletion.create;
    for i:=0 to length(ids)-1 do begin
      if pos(C_ID_QUALIFY_CHARACTER,ids[i])<=0 then begin
        intrinsicRulesForCompletion.add(ids[i]);
        intrinsicRulesForCompletion.add(C_ID_QUALIFY_CHARACTER+ids[i]);
      end else begin
        intrinsicRulesForCompletion.add(ids[i]);
        intrinsicRulesForCompletion.add(split(ids[i],C_ID_QUALIFY_CHARACTER)[0]);
        intrinsicRulesForCompletion.add(split(ids[i],C_ID_QUALIFY_CHARACTER)[1]);
        intrinsicRulesForCompletion.add(C_ID_QUALIFY_CHARACTER+split(ids[i],C_ID_QUALIFY_CHARACTER)[0]);
        intrinsicRulesForCompletion.add(C_ID_QUALIFY_CHARACTER+split(ids[i],C_ID_QUALIFY_CHARACTER)[1]);
      end;
    end;
    for tt:=low(T_tokenType) to high(T_tokenType) do if isIdentifier(C_tokenInfo[tt].defaultId,true) then
      intrinsicRulesForCompletion.add(replaceAll(C_tokenInfo[tt].defaultId,'.',''));
    for i:=low(C_specialWordInfo) to high(C_specialWordInfo) do
      intrinsicRulesForCompletion.add(C_specialWordInfo[i].txt);
    intrinsicRulesForCompletion.unique;
  end;

PROCEDURE T_evaluator.ensureThread;
  begin
    if state=es_dead then beginThread(thread,@self);
  end;

CONSTRUCTOR T_evaluator.create(CONST adapters: P_adapters; threadFunc: TThreadFunc);
  begin
    system.initCriticalSection(cs);
    request:=er_none;
    state:=es_dead;
    thread:=threadFunc;
    endOfEvaluationText:='compiled on: '+{$I %DATE%}+' at: '+{$I %TIME%}+' with FPC'+{$I %FPCVERSION%}+' for '+{$I %FPCTARGET%};
    package.create(nil);
    adapter:=adapters;
    firstErrorLocation.line:=-1;
  end;

DESTRUCTOR T_evaluator.destroy;
  begin
    enterCriticalSection(cs);
    if state=es_running then adapter^.haltEvaluation;
    repeat
      request:=er_die;
      leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      enterCriticalSection(cs);
    until state=es_dead;
    package.destroy;
    leaveCriticalSection(cs);
    system.doneCriticalSection(cs);
  end;

PROCEDURE T_evaluator.haltEvaluation;
  begin
    enterCriticalSection(cs);
    if state=es_running then stepper.doStop;
    while not(adapter^.hasMessageOfType[mt_el5_haltMessageReceived]) do begin
      adapter^.haltEvaluation;
      leaveCriticalSection(cs);
      ThreadSwitch;
      sleep(1);
      enterCriticalSection(cs);
    end;
    request:=er_none;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.reEvaluateWithGUI;
  begin
    enterCriticalSection(cs);
    if state=es_running then begin
      leaveCriticalSection(cs);
      exit;
    end;
    request:=er_reEvaluateWithGUI;
    ensureThread;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.evaluate(CONST path: ansistring; CONST L: TStrings; CONST interactive: boolean);
  begin
    enterCriticalSection(cs);
    ensureThread;
    if state=es_running then begin
      leaveCriticalSection(cs);
      exit;
    end;

    package.setSourceUTF8AndPath(L,path);
    if interactive
    then request:=er_evaluateInteractive
    else request:=er_evaluate;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.callMain(CONST path: ansistring; CONST L: TStrings; params: ansistring);
  VAR sp:longint;
  begin
    enterCriticalSection(cs);
    if state=es_running then begin
      leaveCriticalSection(cs);
      exit;
    end;

    setLength(mainParameters,0);
    params:=trim(UTF8ToSys(params));
    while params<>'' do begin
      sp:=pos(' ',params);
      if sp<=0 then begin
        append(mainParameters,params);
        params:='';
      end else begin
        append(mainParameters,copy(params,1,sp-1));
        params:=trim(copy(params,sp+1,length(params)));
      end;
    end;
    package.setSourceUTF8AndPath(L,path);
    request:=er_callMain;
    ensureThread;
    leaveCriticalSection(cs);
  end;

FUNCTION T_evaluator.evaluationRunning: boolean;
  begin
    enterCriticalSection(cs);
    result:=state=es_running;
    leaveCriticalSection(cs);
  end;

FUNCTION T_evaluator.getCodeProvider: P_codeProvider;
  begin
    enterCriticalSection(cs);
    result:=package.getCodeProvider;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.explainIdentifier(CONST fullLine:ansistring; CONST CaretY,CaretX:longint; VAR info: T_tokenInfo);
  PROCEDURE appendBuiltinRuleInfo(CONST prefix:string='');
    VAR doc:P_intrinsicFunctionDocumentation;
    begin
      ensureBuiltinDocExamples;
      if (length(info.tokenText)>1) and (info.tokenText[1]='.')
      then doc:=functionDocMap.get(copy(info.tokenText,2,length(info.tokenText)-1))
      else doc:= functionDocMap.get(info.tokenText);
      if doc=nil then exit;
      info.tokenExplanation:=info.tokenExplanation+prefix+'Builtin rule'+C_lineBreakChar+doc^.getPlainText(C_lineBreakChar)+';';
    end;

  VAR tokens:T_tokenArray;
      tokenToExplain:T_token;
      loc:T_tokenLocation;
      i:longint;
      lastComment:ansistring='';
  begin
    if (CaretY=info.startLoc.line) and (CaretX>=info.startLoc.column) and (CaretX<info.endLoc.column) then exit;

    tokens.create;
    loc.line:=1;
    loc.column:=1;
    loc.package:=@package;
    adapter^.ClearAll;
    tokens.tokenizeAll(fullLine,loc,@package,adapter^,false);
    tokens.step(@package,lastComment,adapter^);

    while not(tokens.atEnd) and (tokens.current.location.column<CaretX) do tokens.step(@package,lastComment,adapter^);
    if tokens.atEnd then begin
      tokenToExplain:=tokens.lastToken;
      info.startLoc:=tokenToExplain.location;
      info.endLoc:=info.startLoc;
      info.endLoc.column:=maxLongint;
    end else begin
      tokenToExplain:=tokens.current;
      info.startLoc:=tokenToExplain.location;
      info.endLoc:=loc;
    end;
    info.location:=info.startLoc;
    info.tokenText:=safeTokenToString(@tokenToExplain);
    tokens.getRawTokensUndefining;
    tokens.destroy;

    if tokenToExplain.tokType in [tt_identifier,tt_identifier_pon] then package.resolveRuleId(tokenToExplain,nil);

    info.tokenExplanation:=replaceAll(C_tokenInfo[tokenToExplain.tokType].helpText,'#',C_lineBreakChar);
    for i:=0 to length(C_specialWordInfo)-1 do
      if C_specialWordInfo[i].txt=info.tokenText then
      info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar+replaceAll(C_specialWordInfo[i].helpText,'#',C_lineBreakChar);

    case tokenToExplain.tokType of
      tt_intrinsicRule,tt_intrinsicRule_pon: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        appendBuiltinRuleInfo;
      end;
      tt_importedUserRule,tt_importedUserRule_pon: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Imported rule'+C_lineBreakChar+replaceAll(P_rule(tokenToExplain.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(tokenToExplain.data)^.getLocationOfDeclaration;
        if intrinsicRuleMap.containsKey(tokenToExplain.txt) then appendBuiltinRuleInfo('hides ');
      end;
      tt_localUserRule,tt_localUserRule_pon: begin
        if info.tokenExplanation<>'' then info.tokenExplanation:=info.tokenExplanation+C_lineBreakChar;
        info.tokenExplanation:=info.tokenExplanation+'Local rule'+C_lineBreakChar+replaceAll(P_rule(tokenToExplain.data)^.getDocTxt,C_tabChar,' ');
        info.location:=P_rule(tokenToExplain.data)^.getLocationOfDeclaration;
        if intrinsicRuleMap.containsKey(tokenToExplain.txt) then appendBuiltinRuleInfo('hides ');
      end;
    end;
  end;

PROCEDURE T_evaluator.reportVariables(VAR report: T_variableReport);
  begin
    enterCriticalSection(cs);
    package.reportVariables(report);
    leaveCriticalSection(cs);
  end;

FUNCTION T_evaluator.getEndOfEvaluationText: ansistring;
  begin
    enterCriticalSection(cs);
    result:=endOfEvaluationText;
    leaveCriticalSection(cs);
  end;

FUNCTION T_evaluator.getFirstErrorLocation:T_searchTokenLocation;
  begin
    result:=firstErrorLocation;
  end;

FUNCTION T_evaluator.pendingRequest: T_evalRequest;
  begin
    enterCriticalSection(cs);
    if state=es_idle
    then result:=request
    else result:=er_none;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.threadStarted;
  begin
    enterCriticalSection(cs);
    state:=es_idle;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.threadStopped;
  begin
    enterCriticalSection(cs);
    state:=es_dead;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.preEval;
  begin
    enterCriticalSection(cs);
    request:=er_none;
    state:=es_running;
    startOfEvaluation:=now;
    adapter^.ClearAll;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_evaluator.postEval(CONST includeLists: boolean);
  PROCEDURE updateCompletionList;
    begin
      completionList.clear;
      completionList.addAll(intrinsicRulesForCompletion.elementArray);
      package.updateLists(userRules);
      completionList.addAll(userRules.elementArray);
      completionList.unique;
    end;

  begin
    enterCriticalSection(cs);
    if includeLists then begin
      package.updateLists(userRules);
      updateCompletionList;
    end;
    state:=es_idle;
    if adapter^.hasMessageOfType[mt_el5_haltMessageReceived]
    then endOfEvaluationText:='Aborted after '+myTimeToStr(now-startOfEvaluation)
    else endOfEvaluationText:='Done in '+myTimeToStr(now-startOfEvaluation);
    while not(adapter^.hasMessageOfType[mt_endOfEvaluation])
    do adapter^.logEndOfEvaluation;
    leaveCriticalSection(cs);
  end;

FUNCTION T_evaluator.parametersForMainCall: T_arrayOfString;
  begin
    enterCriticalSection(cs);
    result:=mainParameters;
    leaveCriticalSection(cs);
  end;

PROCEDURE initUnit(CONST guiAdapters:P_adapters);
  VAR collector:P_collectingOutAdapter;
  begin
    runEvaluator.create(guiAdapters,@main);
    new(silentAdapters,create);
    new(collector,create(at_unknown));
    silentAdapters^.minErrorLevel:=2;
    silentAdapters^.addOutAdapter(collector,true);
    docEvaluator.create(silentAdapters,@docMain);
    initIntrinsicRuleList;
    userRules.create;
    completionList.create;
    unitIsInitialized:=true;
  end;

FINALIZATION
  if unitIsInitialized then begin
    runEvaluator.destroy;
    docEvaluator.destroy;
    dispose(silentAdapters,destroy);
    userRules.destroy;
    completionList.destroy;
  end;
end.
