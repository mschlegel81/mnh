UNIT mnh_out_adapters;
INTERFACE
USES mnh_stringutil, mnh_constants, mnh_tokLoc;
CONST HALT_MESSAGE='Evaluation haltet (most probably by user).';

TYPE
  T_storedError=record
                  errorLevel:T_errorLevel;
                  errorMessage:ansistring;
                  errorLocation:T_tokenLocation
                end;
  T_writeCallback=PROCEDURE(CONST s:ansistring);
  T_writeErrorCallback=PROCEDURE(CONST error:T_storedError);

VAR inputDeclEcho,
    inputExprEcho,
    exprOut ,    
    printOut,
    tablePrintOut:T_writeCallback;
    errorOut     :T_writeErrorCallback;
    maxErrorLevel:T_errorLevel;

PROCEDURE writeDeclEcho(CONST s:ansistring);
PROCEDURE writeExprEcho(CONST s:ansistring);
PROCEDURE writeExprOut (CONST s:ansistring);
PROCEDURE writePrint   (CONST s:ansistring);
PROCEDURE clearErrors;
PROCEDURE raiseError(CONST thisErrorLevel:T_errorLevel; CONST errorMessage:ansistring; CONST errorLocation:T_tokenLocation);
FUNCTION errorLevel:T_errorLevel;

PROCEDURE plainConsoleOut(CONST s:ansistring);
PROCEDURE plainStdErrOut(CONST error:T_storedError);

FUNCTION isMemoryFree(CONST usage:string):boolean;

PROCEDURE haltEvaluation;

VAR hasHaltMessage:boolean=false;
IMPLEMENTATION
PROCEDURE writeDeclEcho(CONST s:ansistring); begin if inputDeclEcho<>nil then inputDeclEcho(s); end;
PROCEDURE writeExprEcho(CONST s:ansistring); begin if inputExprEcho<>nil then inputExprEcho(s); end;
PROCEDURE writeExprOut (CONST s:ansistring); begin if exprOut<>nil       then exprOut(s); end;
PROCEDURE writePrint   (CONST s:ansistring);
  VAR i:longint;
      tmp:ansistring;
  begin
    if pos(C_tabChar,s)>0 then begin
      if tablePrintOut<>nil then tablePrintOut(s)
      else if printOut<>nil then writePrint(formatTabs(s));
    end else if printOut<>nil then begin
      tmp:=s;
      i:=pos(C_lineBreakChar,tmp);
      while i>0 do begin
        if (i>1) and (tmp[i-1]=C_carriageReturnChar)
        then printOut(copy(tmp,1,i-2))
        else printOut(copy(tmp,1,i-1));
        tmp:=copy(tmp,i+1,length(tmp));
        i:=pos(C_lineBreakChar,tmp);
      end;
      printOut(tmp);
    end;
  end;

PROCEDURE clearErrors;
  begin
    maxErrorLevel:=el0_allOkay;
    hasHaltMessage:=false;
  end;

PROCEDURE raiseError(CONST thisErrorLevel:T_errorLevel; CONST errorMessage:ansistring; CONST errorLocation:T_tokenLocation);
  VAR newError:T_storedError;
  begin
    newError.errorLevel   :=thisErrorLevel;
    newError.errorMessage :=errorMessage;
    newError.errorLocation:=errorLocation;
    if thisErrorLevel>maxErrorLevel then maxErrorLevel:=thisErrorLevel;
    if errorMessage=HALT_MESSAGE then hasHaltMessage:=true;
    if errorOut<>nil then errorOut(newError);
  end;

FUNCTION errorLevel:T_errorLevel;
  begin
    result:=maxErrorLevel;
  end;

PROCEDURE plainConsoleOut(CONST s:ansistring);
  begin
    writeln(s);
  end;

PROCEDURE plainStdErrOut(CONST error:T_storedError);
  begin
    with error do writeln(stdErr,C_errorLevelTxt[errorLevel],errorMessage,' @',ansistring(errorLocation));
  end;

VAR MEMORY_MANAGER:TMemoryManager;

FUNCTION isMemoryFree(CONST usage:string):boolean;
  CONST MAX_MEMORY_THRESHOLD=1500*1024*1024; //=1500 MB
  begin
    result:=(MEMORY_MANAGER.GetFPCHeapStatus().CurrHeapUsed<MAX_MEMORY_THRESHOLD);
    if not(result) and (maxErrorLevel<el5_systemError) then raiseError(el5_systemError,'Out of memory! ('+usage+')',C_nilTokenLocation);
  end;

PROCEDURE haltEvaluation;
  begin
    raiseError(el5_systemError,HALT_MESSAGE,C_nilTokenLocation);
  end;

INITIALIZATION
  GetMemoryManager(MEMORY_MANAGER);
  inputDeclEcho:=nil;
  inputExprEcho:=nil;
  exprOut      :=nil;
  errorOut     :=@plainStdErrOut;
  printOut     :=@plainConsoleOut;
  tablePrintOut:=nil;
  maxErrorLevel:=el0_allOkay;


end.
