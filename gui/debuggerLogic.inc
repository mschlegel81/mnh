{$ifdef includeInterface}
PROCEDURE editScriptRootClick(Sender: TObject);
PROCEDURE miUtilityScriptRootClick(Sender: TObject);
PROCEDURE tbRunClick(Sender: TObject);
PROCEDURE tbStepInClick(Sender: TObject);
PROCEDURE tbStepClick(Sender: TObject);
PROCEDURE tbStepOutClick(Sender: TObject);
PROCEDURE tbStopClick(Sender: TObject);
PROCEDURE tbMicroStepClick(Sender: TObject);
PROCEDURE variablesTreeViewExpanding(Sender: TObject; node: TTreeNode; VAR AllowExpansion: boolean);
PROCEDURE callStackListSelectionChange(Sender: TObject; User: boolean);
PROCEDURE handleBreak;
PROCEDURE updateDebugParts;
PROCEDURE updateExpressionMemo;
PROCEDURE miDebugCancelClick(Sender: TObject);
PROCEDURE miDebugClick(Sender: TObject);
{$endif}
{$ifdef includeImplementation}
PROCEDURE TMnhForm.tbRunClick(Sender: TObject);
  begin
    if not(runEvaluator.evaluationRunning) then begin
      doStartEvaluation;
      if lastStart.mainCall then begin
        with editorMeta[inputPageControl.activePageIndex] do runEvaluator.callMain(pseudoName,editor.lines,lastStart.parameters,ct_debugging);
      end else begin
        with editorMeta[inputPageControl.activePageIndex] do runEvaluator.evaluate(pseudoName,editor.lines,ct_debugging);
      end;
    end else if runEvaluator.context.paused then runEvaluator.context.doContinue
                                            else runEvaluator.context.doMicrostep;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.editScriptRootClick(Sender: TObject);
  begin
    updateEditScriptMenu;
  end;

PROCEDURE TMnhForm.miUtilityScriptRootClick(Sender: TObject);
  begin
    updateUtilityScriptMenu;
  end;

PROCEDURE TMnhForm.tbStepInClick(Sender: TObject);
  begin
    runEvaluator.context.doStepInto;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.tbStepClick(Sender: TObject);
  begin
    runEvaluator.context.doStep;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.tbStepOutClick(Sender: TObject);
  begin
    runEvaluator.context.doStepOut;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.tbStopClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

PROCEDURE TMnhForm.tbMicroStepClick(Sender: TObject);
  begin
    runEvaluator.context.doMicrostep;
    updateDebugParts;
    breakPointHandlingPending:=true;
    lastReportedRunnerInfo.state:=es_dead;
  end;

FUNCTION treeLit_canExpand(L:P_literal):boolean;
  begin
    result:=(L<>nil) and (L^.literalType in C_allListTypes) and (length(L^.toString(100))>=100);
  end;

FUNCTION treeLit_toString(L:P_literal):string;
  begin
    if L=nil then result:=' ' else
    if treeLit_canExpand(L) then result:=L^.typeString
                            else result:=L^.toString();
  end;

PROCEDURE TMnhForm.variablesTreeViewExpanding(Sender: TObject; node: TTreeNode; VAR AllowExpansion: boolean);
  PROCEDURE addChildren(node2:TTreeNode);
    VAR k:longint;
        L:P_literal;
    begin
      for k:=0 to P_listLiteral(node2.data)^.size-1 do begin
        L:=P_listLiteral(node2.data)^.value(k);
        variablesTreeView.items.AddChild(node2,treeLit_toString(L)).data:=L;
      end;
    end;

  VAR i:longint;
  begin
    AllowExpansion:=treeLit_canExpand(node.data);
    if not(AllowExpansion) then exit;
    for i:=0 to node.count-1 do
    if not(node.items[i].HasChildren) and (treeLit_canExpand(node.items[i].data)) then addChildren(node.items[i]);
  end;

PROCEDURE TMnhForm.callStackListSelectionChange(Sender: TObject; User: boolean);
  VAR snapshot:T_debuggingSnapshot;
      stackIdx:longint;
      report:T_variableReport;
      i:longint;
  PROCEDURE addVariable(CONST id:string; CONST value:P_literal);
    VAR newNode:TTreeNode;
        i:longint;
    begin
      newNode:=variablesTreeView.items.add(nil,id+'='+treeLit_toString(value));
      newNode.data:=value;
      if treeLit_canExpand(value) then for i:=0 to P_listLiteral(value)^.size-1 do
        variablesTreeView.items.AddChild(newNode,treeLit_toString(P_listLiteral(value)^.value(i))).data:=P_listLiteral(value)^.value(i);
    end;

  begin
    if not(runEvaluator.context.paused and runEvaluator.evaluationRunning) then exit;
    snapshot:=runEvaluator.context.getDebuggingSnapshot;
    stackIdx:=length(snapshot.callStack)-1-callStackList.ItemIndex;
    variablesTreeView.items.clear;

    if (stackIdx>=0) and (stackIdx<length(snapshot.callStack)) then begin
      callStackInfoStringGrid.Cells[1,0]:=ansistring(snapshot.callStack[stackIdx].callerLocation);
      if snapshot.callStack[stackIdx].calleeLiteral=nil
      then callStackInfoStringGrid.Cells[1,1]:=snapshot.callStack[stackIdx].callee^.getId
      else callStackInfoStringGrid.Cells[1,1]:=snapshot.callStack[stackIdx].calleeLiteral^.toString(200);
      callStackInfoStringGrid.Cells[1,2]:=ansistring(snapshot.callStack[stackIdx].callee^.getLocation);
      addVariable('$params',snapshot.callStack[stackIdx].callParameters);
    end;

    if (callStackList.ItemIndex<=0) then begin
      report.create;
      runEvaluator.reportVariables(report);
      runEvaluator.context.reportVariables(report);
      for i:=length(report.dat)-1 downto 0 do
        addVariable(report.dat[i].id+' ['+report.dat[i].location+']',report.dat[i].value);
      report.destroy;
    end;
  end;

PROCEDURE TMnhForm.handleBreak;
  VAR snapshot:T_debuggingSnapshot;

  PROCEDURE jumpToFile;
    VAR pageIdx:longint;
        newCaret:TPoint;
    begin
      debugLine.line:=-1;
      if (snapshot.location.package=nil) then exit;
      pageIdx:=addOrGetEditorMetaForFile(snapshot.location.package^.getPath);
      if pageIdx>=0 then begin
        inputPageControl.activePageIndex:=pageIdx;
        newCaret.x:=snapshot.location.column;
        newCaret.y:=snapshot.location.line;
        editorMeta[pageIdx].editor.CaretXY:=newCaret;
        debugLine.editor:=editorMeta[pageIdx].editor;
        debugLine.line:=newCaret.y;
        editorMeta[pageIdx].editor.Repaint;
      end;
    end;

  VAR i:longint;
  begin
    if not(runEvaluator.context.paused) then exit;
    updateDebugParts;

    snapshot:=runEvaluator.context.getDebuggingSnapshot;

    breakPointHandlingPending:=false;
    jumpToFile;

    variablesTreeView.items.clear;
    for i:=0 to callStackInfoStringGrid.RowCount-1 do callStackInfoStringGrid.Cells[1,i]:='';

    callStackList.items.clear;
    for i:=length(snapshot.callStack)-1 downto 0 do
    callStackList.items.add(snapshot.callStack[i].callee^.getId);
    callStackList.ItemIndex:=0;
    updateExpressionMemo;

    outputPageControl.activePage:=debugTabSheet;
  end;

PROCEDURE TMnhForm.updateExpressionMemo;
  VAR lines,chars:longint;
      snapshot:T_debuggingSnapshot;
      tokens:T_arrayOfString;
      txt:ansistring;
      k:longint=0;
      firstInLine:boolean;
  begin
    if not(runEvaluator.context.paused and runEvaluator.evaluationRunning) then exit;
    snapshot:=runEvaluator.context.getDebuggingSnapshot;
    lines:=currentExpressionMemo.LinesInWindow;
    chars:=currentExpressionMemo.charsInWindow;
    if (lines*chars<50) then begin
      lines:=1;
      chars:=50;
    end;

    currentExpressionMemo.lines.clear;
    tokens:=tokenSplit(tokenStackToString(snapshot.tokenStack,snapshot.first,round(lines*chars*0.9)));

    while k<length(tokens) do begin
      txt:='';
      firstInLine:=true;
      while (k<length(tokens)) and (firstInLine or (length(txt)+length(tokens[k])<=chars)) do begin
        txt:=txt+tokens[k];
        inc(k);
        firstInLine:=false;
      end;
      currentExpressionMemo.lines.append(txt);
    end;
    setLength(tokens,0);
  end;

PROCEDURE TMnhForm.updateDebugParts;
  PROCEDURE handleButton(VAR button:TToolButton; CONST enabled:boolean; CONST enabledImageIndex:longint; CONST enableAlways:boolean=false);
    begin
      button.enabled:=enabled or enableAlways;
      if enabled then button.ImageIndex:=enabledImageIndex
                 else button.ImageIndex:=enabledImageIndex+1;
    end;

  VAR i:longint;
      isPaused:boolean;
      isRunning:boolean;
  begin
    if miDebug.Checked then begin
      debugTabSheet.visible:=true;
      debugTabSheet.tabVisible:=true;
      for i:=0 to length(editorMeta)-1 do with editorMeta[i] do if language=LANG_MNH then editor.Gutter.MarksPart.visible:=true;
      DebugToolbar.visible:=true;
      DebugToolbar.enabled:=true;
      DebugToolbar.top:=0;
      isPaused:=runEvaluator.context.paused;
      isRunning:=runEvaluator.evaluationRunning;
      handleButton(tbStop     ,    isRunning             ,2);
      handleButton(tbRun      ,not(isRunning) or isPaused,0,true);
      handleButton(tbStep     ,    isRunning and isPaused,4);
      handleButton(tbStepIn   ,    isRunning and isPaused,6);
      handleButton(tbStepOut  ,    isRunning and isPaused,8);
      handleButton(tbMicroStep,    isRunning and isPaused,10);
    end else begin
      debugTabSheet.visible:=false;
      debugTabSheet.tabVisible:=false;
      for i:=0 to length(editorMeta)-1 do editorMeta[i].editor.Gutter.MarksPart.visible:=false;
      outputPageControl.activePage:=outputTabSheet;
      DebugToolbar.visible:=false;
      DebugToolbar.enabled:=false;
    end;
  end;

PROCEDURE TMnhForm.miDebugCancelClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
  end;

PROCEDURE TMnhForm.miDebugClick(Sender: TObject);
  begin
    miDebug.Checked:=not(miDebug.Checked);
    if miDebug.Checked then miProfile.Checked:=true;
    updateDebugParts;
  end;

{$endif}
