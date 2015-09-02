UNIT mnh_out_adapters;

INTERFACE

USES myStringutil, mnh_constants, mnh_tokLoc, myGenerics,mySys;

CONST
  HALT_MESSAGE = 'Evaluation haltet (most probably by user).';

TYPE
  P_storedMessage = ^T_storedMessage;
  T_storedMessage = record
    messageType : T_messageTypeOrErrorLevel;
    simpleMessage: ansistring;
    multiMessage: T_arrayOfString;
    location: T_tokenLocation;
    next,prev: P_storedMessage;
  end;

  P_abstractOutAdapter = ^T_abstractOutAdapter;
  T_abstractOutAdapter = object
    PROCEDURE clearConsole; virtual; abstract;
    PROCEDURE writeEcho(CONST mt:T_messageTypeOrErrorLevel; CONST s: ansistring); virtual; abstract;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual; abstract;
    PROCEDURE errorOut(CONST level:T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual; abstract;
  end;

  P_consoleOutAdapter = ^T_consoleOutAdapter;

  { T_consoleOutAdapter }

  T_consoleOutAdapter = object(T_abstractOutAdapter)
    echoOn:boolean;
    minErrorLevel:T_messageTypeOrErrorLevel;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE writeEcho(CONST mt:T_messageTypeOrErrorLevel; CONST s: ansistring); virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST level:T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
  end;


VAR
  outAdapter:P_abstractOutAdapter;
  consoleOutAdapter:T_consoleOutAdapter;
  maxErrorLevel: T_messageTypeOrErrorLevel;

PROCEDURE clearErrors;
PROCEDURE raiseError(CONST thisErrorLevel: T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
FUNCTION errorLevel: T_messageTypeOrErrorLevel;
PROCEDURE haltEvaluation;
PROCEDURE setDefaultCallbacks;

PROCEDURE haltWithAdaptedSystemErrorLevel;
VAR
  hasHaltMessage: boolean = false;
  systemErrorlevel: specialize G_safeVar<longint>;


IMPLEMENTATION
VAR errorCount:longint=0;

PROCEDURE clearErrors;
  begin
    maxErrorLevel := el0_allOkay;
    hasHaltMessage := false;
    errorCount:=0;
  end;

PROCEDURE raiseError(CONST thisErrorLevel: T_messageTypeOrErrorLevel;
  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    InterLockedIncrement(errorCount); if (errorCount>20) and (thisErrorLevel<=maxErrorLevel) then exit;
    if (thisErrorLevel > maxErrorLevel) or
       (thisErrorLevel = elX_stateInfo) then begin
      maxErrorLevel := thisErrorLevel;
      errorCount:=0;
    end;
    if errorMessage = HALT_MESSAGE then hasHaltMessage := true;
    outAdapter^.errorOut(thisErrorLevel,errorMessage,errorLocation);
  end;

FUNCTION errorLevel: T_messageTypeOrErrorLevel;
  begin
    if hasHaltMessage then result:=el5_systemError else result := maxErrorLevel;
  end;

//PROCEDURE plainConsoleOut(CONST s: T_arrayOfString);
//  VAR i:longint;
//  begin
//    for i:=0 to length(s)-1 do writeln(s[i]);
//  end;

//PROCEDURE plainStdErrOut(CONST error: T_storedError);
//  begin
//    with error do
//      writeln(stdErr, C_errorLevelTxt[errorLevel], errorMessage, ' @',
//        ansistring(errorLocation));
//  end;

PROCEDURE haltEvaluation;
  begin
    raiseError(el5_systemError, HALT_MESSAGE, C_nilTokenLocation);
  end;

PROCEDURE setDefaultCallbacks;
  begin
    outAdapter:=@consoleOutAdapter;
  end;

PROCEDURE haltWithAdaptedSystemErrorLevel;
  VAR L:longint=0;
  begin
    if errorLevel>=el3_evalError then begin
      L:=103;
      if errorLevel>=el4_parsingError then begin
        L:=104;
        if errorLevel>=el5_systemError then L:=105;
      end;
    end;
    if L>systemErrorlevel.value then halt(L)
                                else halt(systemErrorlevel.value);
  end;

{ T_consoleOutAdapter }

CONSTRUCTOR T_consoleOutAdapter.create;
  begin
    echoOn:=true;
    minErrorLevel:=el2_warning;
  end;

DESTRUCTOR T_consoleOutAdapter.destroy;
  begin
  end;

PROCEDURE T_consoleOutAdapter.clearConsole;
  begin
    mySys.clearConsole;
  end;

PROCEDURE T_consoleOutAdapter.writeEcho(CONST mt:T_messageTypeOrErrorLevel; CONST s: ansistring);
  begin
    if not(echoOn) then exit;
    writeln(C_errorLevelTxt[mt],s);
  end;

PROCEDURE T_consoleOutAdapter.printOut(CONST s: T_arrayOfString);
  VAR i:longint;
  begin
    for i:=0 to length(s)-1 do writeln(s[i]);
  end;

PROCEDURE T_consoleOutAdapter.errorOut(CONST level: T_messageTypeOrErrorLevel;
  CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  begin
    if level<minErrorLevel then exit;
    writeln(stdErr, C_errorLevelTxt[errorLevel], errorMessage, ' @',
            ansistring(errorLocation));

  end;

INITIALIZATION
  consoleOutAdapter.create;
  setDefaultCallbacks;
  maxErrorLevel := el0_allOkay;
  systemErrorlevel.create(0);

end.
