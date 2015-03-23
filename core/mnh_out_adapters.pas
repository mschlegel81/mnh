UNIT mnh_out_adapters;

INTERFACE

USES myStringutil, mnh_constants, mnh_tokLoc, myGenerics;

CONST
  HALT_MESSAGE = 'Evaluation haltet (most probably by user).';

TYPE
  T_storedError = record
    errorLevel: T_errorLevel;
    errorMessage: ansistring;
    errorLocation: T_tokenLocation
  end;

  T_writeCallback = PROCEDURE(CONST s: ansistring);
  T_writeMultiCallback = PROCEDURE(CONST s:T_arrayOfString);
  T_writeErrorCallback = PROCEDURE(CONST error: T_storedError);

VAR
  inputDeclEcho, inputExprEcho, exprOut:T_writeCallback;
  printOut: T_writeMultiCallback;
  errorOut: T_writeErrorCallback;
  maxErrorLevel: T_errorLevel;

PROCEDURE writeDeclEcho(CONST s: ansistring);
PROCEDURE writeExprEcho(CONST s: ansistring);
PROCEDURE writeExprOut(CONST s: ansistring);
PROCEDURE writePrint(CONST s: T_arrayOfString);
PROCEDURE clearErrors;
PROCEDURE raiseError(CONST thisErrorLevel: T_errorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
FUNCTION errorLevel: T_errorLevel;

PROCEDURE plainConsoleOut(CONST s: T_arrayOfString);
PROCEDURE plainStdErrOut(CONST error: T_storedError);

PROCEDURE haltEvaluation;

VAR
  hasHaltMessage: boolean = false;

IMPLEMENTATION

PROCEDURE writeDeclEcho(CONST s: ansistring);
  begin
    if inputDeclEcho <> nil then
      inputDeclEcho(s);
  end;

PROCEDURE writeExprEcho(CONST s: ansistring);
  begin
    if inputExprEcho <> nil then
      inputExprEcho(s);
  end;

PROCEDURE writeExprOut(CONST s: ansistring);
  begin
    if exprOut <> nil then
      exprOut(s);
  end;

PROCEDURE writePrint(CONST s: T_arrayOfString);
  begin
    if printOut <> nil then printOut(formatTabs(s));
  end;

VAR errorCount:longint=0;

PROCEDURE clearErrors;
  begin
    maxErrorLevel := el0_allOkay;
    hasHaltMessage := false;
    errorCount:=0;
  end;

PROCEDURE raiseError(CONST thisErrorLevel: T_errorLevel;
  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR
    newError: T_storedError;
  begin
    InterLockedIncrement(errorCount); if (errorCount>20) and (thisErrorLevel<=maxErrorLevel) then exit;
    newError.errorLevel := thisErrorLevel;
    newError.errorMessage := errorMessage;
    newError.errorLocation := errorLocation;
    if (thisErrorLevel > maxErrorLevel) or
       (thisErrorLevel = elX_stateInfo) then begin
      maxErrorLevel := thisErrorLevel;
      errorCount:=0;
    end;
    if errorMessage = HALT_MESSAGE then hasHaltMessage := true;
    if errorOut <> nil then try
      errorOut(newError);
    except
      plainStdErrOut(newError);
    end;
  end;

FUNCTION errorLevel: T_errorLevel;
  begin
    if hasHaltMessage then result:=el5_systemError else result := maxErrorLevel;
  end;

PROCEDURE plainConsoleOut(CONST s: T_arrayOfString);
  VAR i:longint;
  begin
    for i:=0 to length(s)-1 do writeln(s[i]);
  end;

PROCEDURE plainStdErrOut(CONST error: T_storedError);
  begin
    WITH error do
      writeln(stdErr, C_errorLevelTxt[errorLevel], errorMessage, ' @',
        ansistring(errorLocation));
  end;

PROCEDURE haltEvaluation;
  begin
    raiseError(el5_systemError, HALT_MESSAGE, C_nilTokenLocation);
  end;

INITIALIZATION
  inputDeclEcho := nil;
  inputExprEcho := nil;
  exprOut := nil;
  errorOut := @plainStdErrOut;
  printOut := @plainConsoleOut;
  maxErrorLevel := el0_allOkay;

end.
