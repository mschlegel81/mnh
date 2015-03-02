{$ifdef include_interface}
  T_workerThreadState=(wts_free,wts_evaluating,wts_resultReady);

  T_subruleList=array of P_subrule;
  T_literalList=array of P_literal;
  P_literalList=^T_literalList;

  P_workerThread=^T_workerThread;
  T_workerThread=object
    recyclerIndex:longint;
    rules:T_subruleList;
    param:P_listLiteral;
    mod4Rest:byte;
    resultContainer:P_literalList;

    state:T_workerThreadState;

    CONSTRUCTOR create(CONST threadRecyclerIdx:longint);
    DESTRUCTOR destroy;
    FUNCTION putRequest(CONST rulesToEvaluate:T_subruleList; CONST itList:P_listLiteral; CONST filterByte:byte; CONST results:P_literalList):boolean;
    PROCEDURE waitForEvaluationThenFree;
    PROCEDURE evaluate(CONST callDepth:word);
  end;

PROCEDURE evaluatePEachChunk(CONST rulesToEvaluate:T_subruleList; CONST itList:P_listLiteral; CONST filterByte:byte; VAR results:T_literalList; CONST callDepth:word); inline;
{$endif}
{$ifdef include_implementation}
PROCEDURE initWorkerThreads;
  VAR i:longint;
  begin
    for i:=0 to length(workerThread)-1 do workerThread[i].create(i+1);
  end;
  
PROCEDURE finalizeWorkerThreads;
  VAR i:longint;
  begin
    for i:=0 to length(workerThread)-1 do workerThread[i].destroy;
  end;  

FUNCTION workerThreadThread(p:pointer):ptrint;
  begin
    P_workerThread(p)^.evaluate(STACK_DEPTH_LIMIT+1);
  end;
  
CONSTRUCTOR T_workerThread.create(CONST threadRecyclerIdx:longint);
  begin
    recyclerIndex:=threadRecyclerIdx;
    state:=wts_free;
  end;

DESTRUCTOR T_workerThread.destroy;
  begin
    waitForEvaluationThenFree;
  end;

FUNCTION T_workerThread.putRequest(CONST rulesToEvaluate:T_subruleList; CONST itList:P_listLiteral; CONST filterByte:byte; CONST results:P_literalList):boolean;
  VAR i:longint;
  begin
    if (state=wts_free) then begin
      state:=wts_evaluating;
      setLength(rules,length(rulesToEvaluate));
      for i:=0 to length(rules)-1 do rules[i]:=rulesToEvaluate[i];
      param:=itList;
      resultContainer:=results;
      mod4Rest:=filterByte;
      beginThread(@workerThreadThread,@self);
      result:=true;
    end else result:=false;
  end;

PROCEDURE T_workerThread.waitForEvaluationThenFree;
  begin
    while state=wts_evaluating do sleep(1);
    state:=wts_free;
  end;

PROCEDURE evaluatePEachChunk(CONST rulesToEvaluate:T_subruleList; CONST itList:P_listLiteral; CONST filterByte:byte; VAR results:T_literalList; CONST callDepth:word); inline;
  VAR i,j,k:longint;
  begin
    k:=0;
    for i:=0 to itList^.size-1 do for j:=0 to length(rulesToEvaluate)-1 do if errorLevel<el3_evalError then begin
      if (k and 7)=filterByte then
        results[k]:=rulesToEvaluate[j]^.directEvaluateUnary(itList^.value(i),callDepth);
      inc(k);
    end;
  end;

PROCEDURE T_workerThread.evaluate(CONST callDepth:word);
  begin
    evaluatePEachChunk(rules,param,mod4Rest,resultContainer^,callDepth);
    repeat state:=wts_resultReady until state=wts_resultReady;
  end;

{$endif}
