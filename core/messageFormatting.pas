UNIT messageFormatting;
INTERFACE
USES myGenerics,
     mnh_messages,
     basicTypes,
     litVar;
TYPE
  P_echoOutMessage=^T_echoOutMessage;
  T_echoOutMessage=object(T_payloadMessage)
    private
      literal:P_literal;
    public
      CONSTRUCTOR create(CONST value:P_literal; CONST loc:T_searchTokenLocation);
      DESTRUCTOR destroy; virtual;
      PROPERTY getLiteral:P_literal read literal;
  end;

  P_defaultConsoleFormatter=^T_defaultConsoleFormatter;
  T_defaultConsoleFormatter=object(T_messageFormatProvider)
    CONSTRUCTOR create;
    FUNCTION getClonedInstance:P_messageFormatProvider; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION formatMessage(CONST message:P_storedMessage):T_arrayOfString; virtual;
  end;

  P_logFormatter=^T_logFormatter;
  T_logFormatter=object(T_messageFormatProvider)
    maxLocationLength:longint;
    timeFormat:string;
    handlePrintAsLog:boolean;
    CONSTRUCTOR create;
    FUNCTION getClonedInstance:P_messageFormatProvider; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION formatMessage(CONST message:P_storedMessage):T_arrayOfString; virtual;
  end;

  P_guiFormatter=^T_guiFormatter;
  T_guiFormatter=object(T_messageFormatProvider)
    private
      formatterForDemos:boolean;
    public
      wrapEcho:boolean;
      preferredLineLength:longint;
      CONSTRUCTOR create(CONST forDemos:boolean);
      FUNCTION getClonedInstance:P_messageFormatProvider; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION formatMessage(CONST message:P_storedMessage):T_arrayOfString; virtual;
  end;

VAR defaultConsoleFormatter:T_defaultConsoleFormatter;
FUNCTION newEchoMessage(CONST value: P_literal; CONST loc: T_searchTokenLocation):P_echoOutMessage;
IMPLEMENTATION
USES sysutils,LazFileUtils;
FUNCTION newEchoMessage(CONST value: P_literal; CONST loc: T_searchTokenLocation):P_echoOutMessage;
  begin
    new(result,create(value,loc));
  end;

CONSTRUCTOR T_echoOutMessage.create(CONST value: P_literal; CONST loc: T_searchTokenLocation);
  begin
    inherited create(mt_echo_output);
    location:=loc;
    literal:=value^.rereferenced;
  end;

DESTRUCTOR T_echoOutMessage.destroy;
  VAR literalRecycler:T_literalRecycler;
  begin
    literalRecycler.initRecycler;
    enterCriticalSection(messageCs);
    literalRecycler.disposeLiteral(literal);
    leaveCriticalSection(messageCs);
    literalRecycler.cleanup;
    inherited;
  end;

//------------------------------------------------------------------------------
CONSTRUCTOR T_defaultConsoleFormatter.create; begin inherited; end;
DESTRUCTOR T_defaultConsoleFormatter.destroy; begin inherited; end;
FUNCTION T_defaultConsoleFormatter.getClonedInstance: P_messageFormatProvider;
  begin
    new(P_defaultConsoleFormatter(result),create);
  end;
//------------------------------------------------------------------------------
CONSTRUCTOR T_logFormatter.create;
  begin
    inherited;
    timeFormat:='hh:nn:ss.zzz';
    maxLocationLength:=maxLongint;
    handlePrintAsLog:=false;
  end;

DESTRUCTOR T_logFormatter.destroy; begin inherited; end;
FUNCTION T_logFormatter.getClonedInstance: P_messageFormatProvider;
  begin
    new(P_logFormatter(result),create);
    P_logFormatter(result)^.handlePrintAsLog:=handlePrintAsLog;
    P_logFormatter(result)^.timeFormat:=timeFormat;
    P_logFormatter(result)^.maxLocationLength:=maxLocationLength;
  end;

FUNCTION T_logFormatter.formatMessage(CONST message: P_storedMessage): T_arrayOfString;
  FUNCTION getTimePart:string;
    begin
      if timeFormat<>'' then try
        result:=FormatDateTime(timeFormat,P_storedMessageWithText(message)^.getTime)+' ';
      except
        result:='--erroneous time format--';
      end else result:='';
    end;

  VAR fullLoc:boolean=false;
  FUNCTION getLocationPart(CONST loc:T_searchTokenLocation):string;
    VAR s  :string;
    begin
      if (maxLocationLength<=1) or (maxLocationLength>1000) then begin
        fullLoc:=true;
        result:=loc;
      end else begin
        fullLoc:=false;
        if loc.column<0
        then s:=':'+intToStr(loc.line)+',1'
        else s:=':'+intToStr(loc.line)+','+intToStr(loc.column);

        result:=message^.getLocation.fileName;
        if 1+length(result)+length(s)<=maxLocationLength
        then result:='@'+result+s
        else begin
          result:=ExtractFileNameOnly(message^.getLocation.fileName);
          if 1+length(result)+length(s)<=maxLocationLength
          then result:='@'+     result                                 +s
          else result:='@'+copy(result,1,maxLocationLength-length(s)-1)+s;
        end;
        result+=StringOfChar(' ',maxLocationLength-length(result));
      end;
    end;

  VAR mc:T_messageClass;
  FUNCTION getLevelPart:string;
    begin
      case message^.messageClass of
        mc_echo   :
          case message^.messageType of
            mt_echo_input      : result:='In    ';
            mt_echo_declaration: result:='Decl. ';
            mt_echo_output     : result:='Out   ';
            else                 result:='?echo?';
          end;
        mc_timing : result:='Time  ';
        mc_print  : result:='Print ';
        mc_log    : result:='Log   ';
        mc_note   : result:='Note  ';
        mc_warning: result:='Warn  ';
        mc_error  : result:='Error ';
        mc_fatal  : result:='FATAL ';
        else        result:='?     ';
      end;
    end;

  VAR locationPart:string;
      timePart    :string;
      levelPart   :string;

  VAR nextLine    :string='';
      s           :string;
      i           :longint;
  begin
    if (message=nil) or (not(message^.isTextMessage) and (message^.messageType<>mt_echo_output))  then exit(C_EMPTY_STRING_ARRAY);

    mc:=message^.messageClass;
    if mc=mc_print then begin
      if handlePrintAsLog
      then mc:=mc_log
      else exit(P_storedMessageWithText(message)^.txt);
    end;
    setLength(result,0);

    if mc=mc_echo then begin
      if message^.messageType=mt_echo_output then begin
        result:=serializeToStringList(P_echoOutMessage(message)^.literal,C_nilSearchTokenLocation,nil);
      end else begin
        for s in P_storedMessageWithText(message)^.txt do begin
          if (length(nextLine)>10) and (length(nextLine)+length(s)>100)
          then begin
            append(result,trimRight(nextLine));
            nextLine:=trimLeft(s);
          end else nextLine+=s;
        end;
        append(result,trimRight(nextLine));
      end;
    end else append(result,P_storedMessageWithText(message)^.txt);
    if length(result)=0 then append(result,'');

    locationPart:=getLocationPart(message^.getLocation);
    timePart    :=getTimePart;
    levelPart   :=getLevelPart;

    if (length(result)>1) and fullLoc then begin
      prepend(result,timePart+levelPart+locationPart);
      s:=StringOfChar(' ',length(timePart)+length(levelPart));
      for i:=1 to length(result)-1 do result[i]:=s+result[i];
    end else begin
      result[0]:=timePart+levelPart+locationPart+' '+result[0];
      s:=StringOfChar(' ',length(timePart)+length(levelPart)+length(locationPart)+1);
      for i:=1 to length(result)-1 do result[i]:=s+result[i];
    end;
    if (message^.internalType='T_errorMessage') then with P_errorMessage(message)^ do begin
      s:=StringOfChar(' ',length(timePart)+length(levelPart));
      for i:=0 to length(stacktrace)-1 do
        append(result,s+getLocationPart(stacktrace[i].location)+' call '+stacktrace[i].callee+' with '+stacktrace[i].parameters);
    end;
  end;
//------------------------------------------------------------------------------
CONSTRUCTOR T_guiFormatter.create(CONST forDemos: boolean);
  begin
    inherited create;
    formatterForDemos:=forDemos;
    preferredLineLength:=maxLongint;
    wrapEcho:=false;
  end;

DESTRUCTOR T_guiFormatter.destroy; begin inherited; end;

FUNCTION T_guiFormatter.getClonedInstance: P_messageFormatProvider;
  begin
    new(P_guiFormatter(result),create(formatterForDemos));
    P_guiFormatter(result)^.preferredLineLength:=preferredLineLength;
    P_guiFormatter(result)^.wrapEcho           :=wrapEcho;
  end;

FUNCTION T_guiFormatter.formatMessage(CONST message: P_storedMessage): T_arrayOfString;
  VAR locationPart:string='';
      marker      :string='';
      nextLine    :string='';
      s           :string;
      i           :longint;
  begin
    if (message=nil) or (not(message^.isTextMessage) and (message^.messageType<>mt_echo_output))  then exit(C_EMPTY_STRING_ARRAY);
    if not(formatterForDemos)
    then locationPart:=string(message^.getLocation)+' ';

    marker:=C_messageClassMeta[message^.messageClass].guiMarker;
    setLength(result,0);

    case message^.messageClass of
      mc_echo: case message^.messageType of
        mt_echo_input,
        mt_echo_declaration: begin
          if message^.messageType=mt_echo_input
          then nextLine:=C_echoInInfix
          else nextLine:=C_echoDeclInfix;
          for s in P_storedMessageWithText(message)^.txt do begin
            if wrapEcho and (length(nextLine)>10) and (length(nextLine)+length(s)>preferredLineLength)
            then begin
              append(result,marker+trimRight(nextLine));
              nextLine:=C_echoContdInfix+trimLeft(s);
            end else nextLine+=s;
          end;
          append(result,marker+trimRight(nextLine));
        end;
        mt_echo_output: begin
          if wrapEcho
          then result:=serializeToStringList(P_echoOutMessage(message)^.literal,C_nilSearchTokenLocation,nil,preferredLineLength-C_echoPrefixLength)
          else result:=P_echoOutMessage(message)^.literal^.toString();

          result[  0]:=marker+C_echoOutInfix+result[0];
          for i:=1 to length(result)-1 do
            result[i]:=marker+C_echoContdInfix+result[i];
        end;
      end;
      mc_timing: for s in P_storedMessageWithText(message)^.txt do append(result,marker+s);
      mc_log    ,
      mc_note   ,
      mc_warning,
      mc_error  ,
      mc_fatal  : begin
        if length(P_storedMessageWithText(message)^.txt)=1 then begin
          if length(C_messageClassMeta[message^.messageClass].levelTxt)+2+length(locationPart)+length(P_storedMessageWithText(message)^.txt[0])<preferredLineLength
          then result:=marker+C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart+P_storedMessageWithText(message)^.txt[0]
          else begin
            result:=marker+C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart;
            append(result,marker+StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1)+P_storedMessageWithText(message)^.txt[0]);
          end;
        end else begin
          result:=marker+C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart;
          for s in P_storedMessageWithText(message)^.txt do
            append(result,marker+StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1)+s);
        end;
        if (message^.internalType='T_errorMessage') then with P_errorMessage(message)^ do begin
          marker+=StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1);
          for i:=0 to length(stacktrace)-1 do
            append(result,marker+string(stacktrace[i].location)+' call '+stacktrace[i].callee+' with '+stacktrace[i].parameters);
        end;
      end
      else for s in P_storedMessageWithText(message)^.txt do append(result,''+s);
    end;
  end;

FUNCTION T_defaultConsoleFormatter.formatMessage(CONST message: P_storedMessage): T_arrayOfString;
  CONST CONSOLE_OUT_WIDTH=100;
  VAR locationPart:string='';
      nextLine    :string='';
      s           :string;
      i           :longint;

  FUNCTION shortLocationString(CONST x:T_searchTokenLocation):string;
    begin
      if (x.fileName='?') and (x.line=0) and (x.column=0) then exit('');
      if x.column<0
      then result:='@'+ExtractFileNameOnly(x.fileName)+':'+intToStr(x.line)+',1'
      else result:='@'+ExtractFileNameOnly(x.fileName)+':'+intToStr(x.line)+','+intToStr(x.column);
    end;

  begin
    if (message=nil) or (not(message^.isTextMessage) and (message^.messageType<>mt_echo_output))  then exit(C_EMPTY_STRING_ARRAY);
    locationPart:=shortLocationString(message^.getLocation)+' ';

    setLength(result,0);

    case message^.messageClass of
      mc_echo: case message^.messageType of
        mt_echo_input,
        mt_echo_declaration: begin
          if message^.messageType=mt_echo_input
          then nextLine:=C_echoInInfix
          else nextLine:=C_echoDeclInfix;
          for s in P_storedMessageWithText(message)^.txt do begin
            if (length(nextLine)>10) and (length(nextLine)+length(s)>CONSOLE_OUT_WIDTH)
            then begin
              append(result,trimRight(nextLine));
              nextLine:=C_echoContdInfix+trimLeft(s);
            end else nextLine+=s;
          end;
          append(result,trimRight(nextLine));
        end;
        mt_echo_output: begin
          result:=serializeToStringList(P_echoOutMessage(message)^.literal,C_nilSearchTokenLocation,nil,CONSOLE_OUT_WIDTH-C_echoPrefixLength);
          result[  0]:=C_echoOutInfix+result[0];
          for i:=1 to length(result)-1 do
            result[i]:=C_echoContdInfix+result[i];
        end;
      end;
      mc_timing: for s in P_storedMessageWithText(message)^.txt do append(result,s);
      mc_log    ,
      mc_note   ,
      mc_warning,
      mc_error  ,
      mc_fatal  : begin
        if length(P_storedMessageWithText(message)^.txt)=1 then begin
          result:=C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart+P_storedMessageWithText(message)^.txt[0];
        end else begin
          result:=C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart;
          for s in P_storedMessageWithText(message)^.txt do
            append(result,StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1)+s);
        end;
        if (message^.internalType='T_errorMessage') then with P_errorMessage(message)^ do begin
          s:=StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1);
          for i:=0 to length(stacktrace)-1 do
            append(result,s+shortLocationString(stacktrace[i].location)+' call '+stacktrace[i].callee+' with '+stacktrace[i].parameters);
        end;
      end
      else result:=P_storedMessageWithText(message)^.txt;
    end;
  end;

INITIALIZATION
  defaultConsoleFormatter.create;

FINALIZATION
  defaultConsoleFormatter.destroy;

end.
