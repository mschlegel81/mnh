UNIT mnh_out_adapters;

INTERFACE

USES mnh_constants, mnh_tokLoc, myGenerics,mySys,sysutils;

CONST
  HALT_MESSAGE = 'Evaluation haltet (most probably by user).';
  NO_PARAMETERLESS_MAIN_MESSAGE = 'Cannot apply user defined rule main to parameter list ()';

TYPE
  T_storedMessage = record
    messageType : T_messageTypeOrErrorLevel;
    simpleMessage: ansistring;
    multiMessage: T_arrayOfString;
    location: T_tokenLocation;
  end;

  { T_rolloverState }

  T_rolloverState = object
    txt:array[0..31] of ansistring;
    offset:byte;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clear;
    PROCEDURE addLine(CONST lineText:ansistring);
    PROCEDURE raiseStateMessage;
  end;

  P_abstractOutAdapter = ^T_abstractOutAdapter;

  { T_abstractOutAdapter }

  T_abstractOutAdapter = object
    CONSTRUCTOR create;
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

  T_outputBehaviour= record
    doEchoInput: boolean;
    doEchoDeclaration: boolean;
    doShowExpressionOut: boolean;
  end;

  T_collectingOutAdapter=object(T_abstractOutAdapter)
    outputBehaviour:T_outputBehaviour;
    storedMessages:array of T_storedMessage;
    cs:TRTLCriticalSection;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clearConsole; virtual;
    PROCEDURE writeEcho(CONST mt:T_messageTypeOrErrorLevel; CONST s: ansistring); virtual;
    PROCEDURE printOut(CONST s:T_arrayOfString); virtual;
    PROCEDURE errorOut(CONST level:T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation); virtual;
    PROCEDURE appendSingleMessage(CONST message:T_storedMessage);
    PROCEDURE clearMessages;
  end;



VAR
  outAdapter:P_abstractOutAdapter;
  consoleOutAdapter:T_consoleOutAdapter;
  maxErrorLevel: T_messageTypeOrErrorLevel;
  wantStateHistory: boolean=false;

PROCEDURE clearErrors;
PROCEDURE raiseError(CONST thisErrorLevel: T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
FUNCTION errorLevel: T_messageTypeOrErrorLevel;
PROCEDURE haltEvaluation;
PROCEDURE setDefaultCallbacks;

PROCEDURE haltWithAdaptedSystemErrorLevel;
VAR
  hasHaltMessage: boolean = false;
  hasNoParameterlessMainMessage: boolean = false;
  systemErrorlevel: specialize G_safeVar<longint>;


IMPLEMENTATION
VAR errorCount:longint=0;

PROCEDURE clearErrors;
  begin
    maxErrorLevel := el0_allOkay;
    hasHaltMessage := false;
    hasNoParameterlessMainMessage := false;
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
    if errorMessage = NO_PARAMETERLESS_MAIN_MESSAGE then hasNoParameterlessMainMessage := true;
    outAdapter^.errorOut(thisErrorLevel,errorMessage,errorLocation);
  end;

FUNCTION errorLevel: T_messageTypeOrErrorLevel;
  begin
    if hasHaltMessage then result:=el5_systemError else result := maxErrorLevel;
  end;

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

{ T_rolloverState }

CONSTRUCTOR T_rolloverState.create;
  begin
    clear;
  end;

DESTRUCTOR T_rolloverState.destroy;
  begin
    clear;
  end;

PROCEDURE T_rolloverState.clear;
  VAR i:longint;
  begin
    for i:=0 to length(txt)-1 do txt[i]:='';
    offset:=0;
  end;

PROCEDURE T_rolloverState.addLine(CONST lineText: ansistring);
  begin
    if (lineText='') or (lineText=txt[(offset+31) and 31]) then exit;
    txt[offset]:=lineText;
    offset:=(offset+1) mod length(txt);
  end;

PROCEDURE T_rolloverState.raiseStateMessage;
  VAR i,j,j1,commonTailLength,lengthDiff:longint;
      someChanges:boolean=true;
  begin
    while someChanges do begin
      someChanges:=false;
      for i:=0 to length(txt)-2 do begin
        j :=(offset+i  ) and 31;
        j1:=(offset+i+1) and 31;
        if (length(txt[j])>160) or (length(txt[j1])>160) or (length(txt[j1])=0) then Continue;
        commonTailLength:=0;
        while (commonTailLength<length(txt[j ])) and
              (commonTailLength<length(txt[j1])) and
              (txt[j ][length(txt[j ])-commonTailLength]=
               txt[j1][length(txt[j1])-commonTailLength]) do inc(commonTailLength);
        lengthDiff:=length(txt[j1])-length(txt[j]);
        if lengthDiff>0 then begin
          txt[j]:=copy(txt[j],1,length(txt[j])-commonTailLength)+
                  Space(lengthDiff)+
                  copy(txt[j],length(txt[j])-commonTailLength+1,commonTailLength);
          someChanges:=true;
        end else if lengthDiff<0 then begin
          txt[j1]:=copy(txt[j1],1,length(txt[j1])-commonTailLength)+
                  Space(-lengthDiff)+
                  copy(txt[j1],length(txt[j1])-commonTailLength+1,commonTailLength);
          someChanges:=true;
        end;
      end;
    end;

    raiseError(elX_stateInfo,'The last 32 steps were:',C_nilTokenLocation);
    for i:=0 to length(txt)-1 do begin
      j:=(offset+i) and 31;
      txt[j]:=trim(txt[j]);
      if txt[j]<>'' then raiseError(elX_stateInfo,txt[j],C_nilTokenLocation);
    end;
  end;

{ T_abstractOutAdapter }

CONSTRUCTOR T_abstractOutAdapter.create;
  begin
    wantStateHistory:=false;
  end;

{ T_consoleOutAdapter }

CONSTRUCTOR T_consoleOutAdapter.create;
  begin
    echoOn:=false;
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
    writeln(stdErr, C_errorLevelTxt[errorLevel], errorMessage, ' ',
            ansistring(errorLocation));
  end;

CONSTRUCTOR T_collectingOutAdapter.create;
  begin
    system.InitCriticalSection(cs);
    setLength(storedMessages,0);
  end;

DESTRUCTOR T_collectingOutAdapter.destroy;
  begin
    system.DoneCriticalsection(cs);
    clearMessages;
  end;

PROCEDURE T_collectingOutAdapter.clearConsole;
  VAR msg:T_storedMessage;
  begin
    system.enterCriticalSection(cs);
    clearMessages;
    msg.messageType:=elc_clearConsole;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.writeEcho(CONST mt: T_messageTypeOrErrorLevel; CONST s: ansistring);
  VAR msg:T_storedMessage;
  begin
    case mt of
      elo_echoOutput     : if not(outputBehaviour.doShowExpressionOut) then exit;
      eld_echoDeclaration: if not(outputBehaviour.doEchoDeclaration) then exit;
      ele_echoInput      : if not(outputBehaviour.doEchoInput) then exit;
    end;
    system.enterCriticalSection(cs);
    msg.messageType:=mt;
    msg.simpleMessage:=s;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.printOut(CONST s: T_arrayOfString);
  VAR msg:T_storedMessage;
  begin
    system.enterCriticalSection(cs);
    msg.messageType:=elp_printline;
    msg.multiMessage:=s;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.errorOut(CONST level: T_messageTypeOrErrorLevel; CONST errorMessage: ansistring; CONST errorLocation: T_tokenLocation);
  VAR msg:T_storedMessage;
  begin
    {$ifdef debugMode}
    consoleOutAdapter.errorOut(level,errorMessage,errorLocation);
    {$endif}
    if level<el2_warning then exit;
    system.enterCriticalSection(cs);
    msg.messageType:=level;
    msg.simpleMessage:=errorMessage;
    msg.location:=errorLocation;
    appendSingleMessage(msg);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_collectingOutAdapter.appendSingleMessage(CONST message: T_storedMessage);
  begin
    setLength(storedMessages,length(storedMessages)+1);
    storedMessages[length(storedMessages)-1]:=message;
  end;

PROCEDURE T_collectingOutAdapter.clearMessages;
  begin
    setLength(storedMessages,0);
  end;


INITIALIZATION
  consoleOutAdapter.create;
  setDefaultCallbacks;
  maxErrorLevel := el0_allOkay;
  systemErrorlevel.create(0);

end.
