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
    FUNCTION formatLocation(CONST location:T_searchTokenLocation):string; virtual;
  end;

  P_logFormatter=^T_logFormatter;
  T_logFormatter=object(T_messageFormatProvider)
    maxLocationLength:longint;
    timeFormat:string;
    handlePrintAsLog:boolean;
    fullLoc:boolean;
    allowColoring:boolean;

    CONSTRUCTOR create(CONST targetIsConsole:boolean);
    FUNCTION getClonedInstance:P_messageFormatProvider; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION formatMessage(CONST message:P_storedMessage):T_arrayOfString; virtual;
    FUNCTION formatLocation(CONST location:T_searchTokenLocation):string; virtual;
  end;

  T_messagesAndLocations=object
    private
      dat:array of record message:string; location:T_searchTokenLocation; end;
      fill:longint;
      offset:longint;
      maxSize:longint;
      directPrinting:longint;
      PROCEDURE setMaxSize(CONST newValue:longint);
    public
      CONSTRUCTOR create(CONST maxSize_:longint);
      PROPERTY outputLinesLimit:longint read maxSize write setMaxSize;
      DESTRUCTOR destroy;
      PROCEDURE append(CONST message:string);
      PROCEDURE append(CONST message:string; CONST location:T_searchTokenLocation);
      PROCEDURE append(CONST message: T_arrayOfString; CONST location: T_searchTokenLocation);
      PROCEDURE clear;
      PROPERTY size:longint read fill;
      FUNCTION text(CONST i:longint):string;
      FUNCTION text:T_arrayOfString;
      FUNCTION location(CONST i:longint):T_searchTokenLocation;
      FUNCTION locations:T_searchTokenLocations;
      PROCEDURE appendPrint(CONST message:string);
      PROCEDURE processDirectPrint(CONST chars:string);
      PROCEDURE cleanup;
  end;

  P_guiFormatter=^T_guiFormatter;
  T_guiFormatter=object(T_messageFormatProvider)
    private
      formatterForDemos:boolean;
    public
      wrapEcho,forceFullLiteralOutput:boolean;
      maxLinesPerLiteral:longint;
      CONSTRUCTOR create(CONST forDemos:boolean);
      FUNCTION getClonedInstance:P_messageFormatProvider; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION formatMessage(CONST message:P_storedMessage):T_arrayOfString; virtual;
      FUNCTION formatLocation(CONST location:T_searchTokenLocation):string; virtual;
      PROCEDURE formatMessageAndLocation(CONST message:P_storedMessage; VAR messagesAndLocations:T_messagesAndLocations);
  end;

  {$ifdef fullVersion}
  T_profilingInfo=record
    timeSpent_inclusive,
    timeSpent_exclusive:double;
    callCount:longint;
  end;

  T_callerListEntry=record
    id:string;
    location:T_searchTokenLocation;
    time:T_profilingInfo;
  end;
  T_callerList=array of T_callerListEntry;

  T_profilingListEntry=record
    id:T_idString;
    calleeLocation:T_searchTokenLocation;
    callers,
    callees:T_callerList;
    //aggregated values
    aggTime:T_profilingInfo;
  end;

  T_profilingList=array of T_profilingListEntry;

  P_profileMessage=^T_profileMessage;
  T_profileMessage=object(T_payloadMessage)
    public
      content:T_profilingList;
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      FUNCTION toString(CONST formatter:P_messageFormatProvider): T_arrayOfString;
  end;

  PROCEDURE sortCallerList(VAR list:T_callerList; CONST sortIndex:byte);
  PROCEDURE sortProfilingList(VAR list:T_profilingList; CONST sortIndex:byte);

  VAR mnhSysPseudopackagePrefix :string='';
  {$endif}
VAR defaultConsoleFormatter:T_defaultConsoleFormatter;
FUNCTION newEchoMessage(CONST value: P_literal; CONST loc: T_searchTokenLocation):P_echoOutMessage;
IMPLEMENTATION
USES sysutils,LazFileUtils{$ifdef fullVersion},myStringUtil{$endif},recyclers;

FUNCTION newEchoMessage(CONST value: P_literal; CONST loc: T_searchTokenLocation):P_echoOutMessage;
  begin
    new(result,create(value,loc));
  end;

PROCEDURE T_messagesAndLocations.cleanup;
  VAR tempLoc:T_searchTokenLocations;
      tempTxt:T_arrayOfString;
      i:longint;
  begin
    setLength(tempLoc,fill);
    setLength(tempTxt,fill);
    for i:=0 to length(tempTxt)-1 do tempTxt[i]:=text(i);
    for i:=0 to length(tempLoc)-1 do tempLoc[i]:=location(i);
    offset:=0;
    for i:=0 to length(tempTxt)-1 do begin
      dat[i].message :=tempTxt[i];
      dat[i].location:=tempLoc[i];
    end;
    setLength(dat,fill);
  end;

PROCEDURE T_messagesAndLocations.setMaxSize(CONST newValue: longint);
  begin
    if maxSize=newValue then exit;
    if (offset>0) and (fill>0) then cleanup;
    maxSize:=newValue;
    if length(dat)>maxSize then setLength(dat,maxSize);
    if fill       >maxSize then fill:=maxSize;
  end;

CONSTRUCTOR T_messagesAndLocations.create(CONST maxSize_: longint);
  begin
    maxSize:=maxSize_;
    fill:=0;
    offset:=0;
    setLength(dat,0);
    directPrinting:=-1;
  end;

PROCEDURE T_messagesAndLocations.clear;
  begin
    fill:=0;
    offset:=0;
    directPrinting:=-1;
  end;

FUNCTION T_messagesAndLocations.text(CONST i: longint): string;
  begin
    result:=dat[(i+offset+maxSize) mod maxSize].message;
  end;

FUNCTION T_messagesAndLocations.text: T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,fill);
    for i:=0 to fill-1 do result[i]:=dat[longint((int64(i)+offset+maxSize) mod maxSize)].message;
  end;

FUNCTION T_messagesAndLocations.location(CONST i: longint): T_searchTokenLocation;
  begin
    result:=dat[(i+offset+maxSize) mod maxSize].location;
  end;

FUNCTION T_messagesAndLocations.locations:T_searchTokenLocations;
  VAR i:longint;
  begin
    setLength(result,fill);
    for i:=0 to fill-1 do result[i]:=dat[longint((int64(i)+offset+maxSize) mod maxSize)].location;
  end;

PROCEDURE T_messagesAndLocations.processDirectPrint(CONST chars: string);
  VAR c:char;
      lineIndex:longint;
      state:(initial,read27,readingAnsiEscape)=initial;
  begin
    if directPrinting<0 then begin
      append('');
      directPrinting:=0;
    end;
    if fill>=maxSize
    then lineIndex:=(offset+maxSize-1) mod maxSize
    else lineIndex:=fill-1;
    for c in chars do case state of
      initial: case c of
        #27: state:=read27;
        #8 ://backspace
          if directPrinting>=1 then begin
            dat[lineIndex].message:=copy(dat[lineIndex].message,1,directPrinting-1)
                                   +copy(dat[lineIndex].message,directPrinting+1,length(dat[lineIndex].message));
            //e.g.: directPrinting=2
            //123456789
            // ^
            //13456789

            //Finally: move cursor left
            dec(directPrinting);
          end;
        #13: //carriage-return
          directPrinting:=0;
        #10: //new line
          begin
            append('');
            directPrinting:=0;
            if fill>=maxSize
            then lineIndex:=(offset+maxSize-1) mod maxSize
            else lineIndex:=fill-1;
          end
        else begin
          if directPrinting>=length(dat[lineIndex].message)
          then dat[lineIndex].message+=c
          else dat[lineIndex].message[directPrinting+1]:=c;
          inc(directPrinting);
        end;
      end;
      read27: case c of
        '[': state:=readingAnsiEscape;
        else state:=initial;
      end;
      readingAnsiEscape: if c=';' then state:=initial;
    end;
  end;

DESTRUCTOR T_messagesAndLocations.destroy;
  begin
    setLength(dat,0);
  end;

PROCEDURE T_messagesAndLocations.appendPrint(CONST message:string);
  CONST noLocation:T_searchTokenLocation=(fileName:'';line:-1; column:-1);
  begin
    //TODO: Process and remove ansi escapes
    // Ansi escape: \e[...m , e.g. \e[0m Reset colors
    // 0: Reset
    // 1: Bold
    // 3: Italic
    // 4: Underline
    // Foreground Background
    // 30 	40 	Black     12, 12, 12
    // 31 	41 	Red 	  197, 15, 31
    // 32 	42 	Green 	  0, 166, 0
    // 33 	43 	Yellow 	  193, 156, 0
    // 34 	44 	Blue      0, 55, 218
    // 35 	45 	Magenta   136, 23, 152
    // 36 	46 	Cyan      58, 150, 221
    // 37 	47 	White 	  191, 191, 191
    // 90 	100 	Bright Black (Gray) 	102, 102, 102
    // 91 	101 	Bright Red 	231, 72, 86
    // 92 	102 	Bright Green 	22, 198, 12
    // 93 	103 	Bright Yellow 	249, 241, 165
    // 94 	104 	Bright Blue 	59, 142, 234
    // 95 	105 	Bright Magenta  190,0,190
    // 96 	106 	Bright Cyan     97,214,214
    // 97 	107 	Bright White    242,242,242
    //              \e[...[A..H,J,K,S,T,f,m,i,n]
    //  A: cursor n up
    //  B: cursor n down
    //  C: cursor forward
    //  D: cursor back
    //  E: cursor next line
    //  F: cursor previous line
    //  H: curosr position (n;m)
    //  K: Erase in line

    append(message,noLocation);
  end;

PROCEDURE T_messagesAndLocations.append(CONST message: string);
  CONST noLocation:T_searchTokenLocation=(fileName:'';line:-1; column:-1);
  begin
    append(message,noLocation);
  end;

PROCEDURE T_messagesAndLocations.append(CONST message: string;
  CONST location: T_searchTokenLocation);
  VAR i:longint;
  begin
    if fill>=length(dat) then begin
      {$Q-}{$R-}
      i:=(length(dat) shl 1);
      if i=0 then i:=32;
      if (i<0) or (i>maxSize) then i:=maxSize;
      {$Q+}{$R+}
      setLength(dat,i);
    end;
    directPrinting:=-1;
    if fill>=maxSize then begin
      dat[offset].message :=message;
      dat[offset].location:=location;
      inc(offset);
      if offset>=maxSize then offset:=0;
    end else begin
      dat[fill].message :=message;
      dat[fill].location:=location;
      inc(fill);
    end;
  end;

PROCEDURE T_messagesAndLocations.append(CONST message: T_arrayOfString;
  CONST location: T_searchTokenLocation);
  VAR s:string;
  begin
    if fill+length(message)>length(dat) then setLength(dat,fill+length(message));
    for s in message do append(s,location);
  end;

CONSTRUCTOR T_echoOutMessage.create(CONST value: P_literal; CONST loc: T_searchTokenLocation);
  begin
    inherited create(mt_echo_output);
    location:=loc;
    literal:=value^.rereferenced;
  end;

DESTRUCTOR T_echoOutMessage.destroy;
  begin
    enterCriticalSection(messageCs);
    globalLiteralRecycler.disposeLiteral(literal);
    leaveCriticalSection(messageCs);
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
CONSTRUCTOR T_logFormatter.create(CONST targetIsConsole:boolean);
  begin
    inherited create;
    timeFormat:='hh:nn:ss.zzz';
    maxLocationLength:=maxLongint;
    handlePrintAsLog:=false;
    fullLoc:=false;
    allowColoring:=targetIsConsole;
  end;

DESTRUCTOR T_logFormatter.destroy; begin inherited; end;
FUNCTION T_logFormatter.getClonedInstance: P_messageFormatProvider;
  begin
    new(P_logFormatter(result),create(allowColoring));
    P_logFormatter(result)^.handlePrintAsLog:=handlePrintAsLog;
    P_logFormatter(result)^.timeFormat:=timeFormat;
    P_logFormatter(result)^.maxLocationLength:=maxLocationLength;
    P_logFormatter(result)^.fullLoc:=fullLoc;
  end;

FUNCTION T_logFormatter.formatLocation(CONST location:T_searchTokenLocation):string;
  VAR s  :string;
  begin
    if maxLocationLength=0 then exit('');
    if (maxLocationLength<=1) or (maxLocationLength>1000) then begin
      fullLoc:=true;
      result:=location;
    end else begin
      fullLoc:=false;
      if location.column<0
      then s:=':'+intToStr(location.line)+',1'
      else s:=':'+intToStr(location.line)+','+intToStr(location.column);

      result:=location.fileName;
      if 1+length(result)+length(s)<=maxLocationLength
      then result:='@'+result+s
      else begin
        result:=ExtractFileNameOnly(location.fileName);
        if 1+length(result)+length(s)<=maxLocationLength
        then result:='@'+     result                                 +s
        else result:='@'+copy(result,1,maxLocationLength-length(s)-1)+s;
      end;
      result+=StringOfChar(' ',maxLocationLength-length(result));
    end;
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

  FUNCTION optionalColorOff:string;
    begin
      if allowColoring then result:=C_ANSI_CODE_RESET else result:='';
    end;

  FUNCTION optionalColor(CONST mc:T_messageClass):string;
    begin
      if allowColoring then result:=C_messageClassMeta[mc].levelColor else result:='';
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
    if (message=nil) or (not(message^.isTextMessage) and not(message^.messageType in [mt_echo_output{$ifdef fullVersion},mt_profile_call_info{$endif}]))  then exit(C_EMPTY_STRING_ARRAY);
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
      if allowColoring then for i:=0 to length(result)-1 do result[i]:=C_messageClassMeta[mc_echo].levelColor+result[i]+C_ANSI_CODE_RESET;
    end
    {$ifdef fullVersion}
    else if (message^.messageType=mt_profile_call_info) then exit(P_profileMessage(message)^.toString(@self))
    {$endif}
    else append(result,P_storedMessageWithText(message)^.txt);
    if length(result)=0 then append(result,'');

    locationPart:=formatLocation(message^.getLocation);
    timePart    :=getTimePart;
    levelPart   :=getLevelPart;

    if (length(result)>1) and fullLoc then begin
      prepend(result,timePart+levelPart+locationPart);
      s:=StringOfChar(' ',length(timePart)+length(levelPart));
      for i:=1 to length(result)-1 do result[i]:=s+optionalColor(mc) + result[i] + optionalColorOff;
    end else begin
      result[0]:=timePart+levelPart+locationPart+' '+optionalColor(mc)+result[0] + optionalColorOff;
      s:=StringOfChar(' ',length(timePart)+length(levelPart)+length(locationPart)+1);
      for i:=1 to length(result)-1 do result[i]:=s+optionalColor(mc) + result[i] + optionalColorOff;
    end;
    if (message^.internalType='T_errorMessage') then with P_errorMessage(message)^ do begin
      s:=StringOfChar(' ',length(timePart)+length(levelPart));
      for i:=0 to length(stacktrace)-1 do
        append(result,s+optionalColor(mc) +  formatLocation(stacktrace[i].location)+' call '+stacktrace[i].callee+' with '+stacktrace[i].parameters + optionalColorOff);
    end;
  end;
//------------------------------------------------------------------------------
CONSTRUCTOR T_guiFormatter.create(CONST forDemos: boolean);
  begin
    inherited create;
    formatterForDemos:=forDemos;
    preferredLineLength:=maxLongint;
    wrapEcho:=false;
    forceFullLiteralOutput:=false;
    maxLinesPerLiteral:=16;
  end;

DESTRUCTOR T_guiFormatter.destroy; begin inherited; end;

FUNCTION T_guiFormatter.getClonedInstance: P_messageFormatProvider;
  begin
    new(P_guiFormatter(result),create(formatterForDemos));
    P_guiFormatter(result)^.preferredLineLength:=preferredLineLength;
    P_guiFormatter(result)^.wrapEcho           :=wrapEcho;
  end;

FUNCTION T_guiFormatter.formatLocation(CONST location: T_searchTokenLocation): string;
  begin
    if formatterForDemos then result:='' else begin
      if (location.fileName='?') and (location.line=0) and (location.column=0) then exit('');
      if location.column<0
      then result:='@'+extractFileName(location.fileName)+':'+intToStr(location.line)+',1'
      else result:='@'+extractFileName(location.fileName)+':'+intToStr(location.line)+','+intToStr(location.column);
    end;
  end;

PROCEDURE T_guiFormatter.formatMessageAndLocation(CONST message: P_storedMessage; VAR messagesAndLocations: T_messagesAndLocations);
  VAR locationPart:string='';
      marker      :string='';
      nextLine    :string='';
      s           :string;
      i           :longint=0;
      messageLoc  :T_searchTokenLocation;
      echo        :T_arrayOfString;
      limitPerLiteral: int64;
  begin
    if (message=nil) or (not(message^.isTextMessage) and (message^.messageType<>mt_echo_output))  then exit;
    messageLoc:=message^.getLocation;
    if not(formatterForDemos)
    then locationPart:=formatLocation(messageLoc)+' ';

    marker:=C_messageClassMeta[message^.messageClass].guiMarker;

    case message^.messageClass of
      mc_echo: case message^.messageType of
        mt_echo_input,
        mt_echo_declaration: begin
          if message^.messageType=mt_echo_input
          then nextLine:=C_echoInInfix
          else nextLine:=C_echoDeclInfix;
          for s in P_storedMessageWithText(message)^.txt do begin
            if (formatterForDemos and (i>0)) or wrapEcho and (length(nextLine)+length(s)>preferredLineLength)
            then begin
              messagesAndLocations.append(marker+trimRight(nextLine),messageLoc);
              if formatterForDemos
              then nextLine:=C_echoContdInfix+         s
              else nextLine:=C_echoContdInfix+trimLeft(s);
            end else nextLine+=s;
            inc(i);
          end;
          messagesAndLocations.append(marker+trimRight(nextLine),messageLoc);
        end;
        mt_echo_output: begin
          limitPerLiteral:=int64(maxLinesPerLiteral)*int64(preferredLineLength);
          if (limitPerLiteral>maxLongint) or forceFullLiteralOutput then limitPerLiteral:=maxLongint;
          if wrapEcho
          then echo:=serializeToStringList(P_echoOutMessage(message)^.literal,
                                           C_nilSearchTokenLocation,nil,
                                           forceFullLiteralOutput,
                                           preferredLineLength-C_echoPrefixLength,
                                           limitPerLiteral)
          else echo:=P_echoOutMessage(message)^.literal^.toString();

          if length(echo) >0 then echo[  0]:=marker+C_echoOutInfix+echo[0];
          for i:=1 to length(echo)-1 do
            echo[i]:=marker+C_echoContdInfix+echo[i];
          messagesAndLocations.append(echo,messageLoc);
        end;
      end;
      mc_timing: for s in P_storedMessageWithText(message)^.txt do messagesAndLocations.append(marker+s,messageLoc);
      mc_log    ,
      mc_note   ,
      mc_warning,
      mc_error  ,
      mc_trace  ,
      mc_fatal  : begin
        if length(P_storedMessageWithText(message)^.txt)=1 then begin
          if length(C_messageClassMeta[message^.messageClass].levelTxt)+2+length(locationPart)+length(P_storedMessageWithText(message)^.txt[0])<preferredLineLength
          then messagesAndLocations.append(marker+C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart+P_storedMessageWithText(message)^.txt[0],messageLoc)
          else begin
            messagesAndLocations.append(marker+C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart,messageLoc);
            messagesAndLocations.append(marker+StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1)+P_storedMessageWithText(message)^.txt[0],messageLoc);
          end;
        end else begin
          messagesAndLocations.append(marker+C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart,messageLoc);
          for s in P_storedMessageWithText(message)^.txt do
            messagesAndLocations.append(marker+StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1)+s,messageLoc);
        end;
        if (message^.internalType='T_errorMessage') then with P_errorMessage(message)^ do begin
          marker+=StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1);
          for i:=0 to length(stacktrace)-1 do
            messagesAndLocations.append(marker+formatLocation(stacktrace[i].location)+' call '+stacktrace[i].callee+' with '+stacktrace[i].parameters,stacktrace[i].location);
        end;
      end;
      mc_print: for s in P_storedMessageWithText(message)^.txt do messagesAndLocations.appendPrint(s);
      else begin
        for s in P_storedMessageWithText(message)^.txt do messagesAndLocations.append(s);
      end;
    end;
  end;

FUNCTION T_guiFormatter.formatMessage(CONST message: P_storedMessage): T_arrayOfString;
  VAR locationPart:string='';
      marker      :string='';
      nextLine    :string='';
      s           :string;
      i           :longint=0;
      limitPerLiteral:int64;
  begin
    if (message=nil) or (not(message^.isTextMessage) and (message^.messageType<>mt_echo_output))  then exit(C_EMPTY_STRING_ARRAY);
    if not(formatterForDemos)
    then locationPart:=formatLocation(message^.getLocation)+' ';

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
            if (formatterForDemos and (message^.messageType=mt_echo_input) and (i>0)) or wrapEcho and (length(nextLine)>10) and (length(nextLine)+length(s)>preferredLineLength)
            then begin
              append(result,marker+trimRight(nextLine));
              if formatterForDemos and (message^.messageType=mt_echo_input)
              then nextLine:=C_echoContdInfix+         s
              else nextLine:=C_echoContdInfix+trimLeft(s);
            end else nextLine+=s;
            inc(i);
          end;
          append(result,marker+trimRight(nextLine));
        end;
        mt_echo_output: begin
          limitPerLiteral:=int64(maxLinesPerLiteral)*int64(preferredLineLength);
          if (limitPerLiteral>maxLongint) or forceFullLiteralOutput then limitPerLiteral:=maxLongint;
          if wrapEcho
          then result:=serializeToStringList(P_echoOutMessage(message)^.literal,
                                             C_nilSearchTokenLocation,nil,
                                             forceFullLiteralOutput,
                                             preferredLineLength-C_echoPrefixLength,
                                             limitPerLiteral)
          else result:=P_echoOutMessage(message)^.literal^.toString();
          if length(result)>0 then result[0]:=marker+C_echoOutInfix+result[0];
          for i:=1 to length(result)-1 do
            result[i]:=marker+C_echoContdInfix+result[i];
        end;
      end;
      mc_timing: for s in P_storedMessageWithText(message)^.txt do append(result,marker+s);
      mc_log    ,
      mc_note   ,
      mc_warning,
      mc_error  ,
      mc_trace  ,
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

FUNCTION T_defaultConsoleFormatter.formatLocation(CONST location:T_searchTokenLocation):string;
  begin
    if (location.fileName='?') and (location.line=0) and (location.column=0) then exit('');
    if location.column<0
    then result:='@'+ExtractFileNameOnly(location.fileName)+':'+intToStr(location.line)+',1'
    else result:='@'+ExtractFileNameOnly(location.fileName)+':'+intToStr(location.line)+','+intToStr(location.column);

  end;

FUNCTION T_defaultConsoleFormatter.formatMessage(CONST message: P_storedMessage): T_arrayOfString;
  VAR locationPart:string='';
      nextLine    :string='';
      s           :string;
      i           :longint;
      limitPerLiteral: int64;

  begin
    if (message=nil) or (not(message^.isTextMessage) and not(message^.messageType in [mt_echo_output{$ifdef fullVersion},mt_profile_call_info{$endif}]))  then exit(C_EMPTY_STRING_ARRAY);
    locationPart:=formatLocation(message^.getLocation)+' ';

    setLength(result,0);

    case message^.messageClass of
      {$ifdef fullVersion}
      mc_gui: if message^.messageType=mt_profile_call_info
              then result:=P_profileMessage(message)^.toString(@self);
      {$endif}
      mc_echo: case message^.messageType of
        mt_echo_input,
        mt_echo_declaration: begin
          if message^.messageType=mt_echo_input
          then nextLine:=C_echoInInfix  +C_messageClassMeta[mc_echo].levelColor
          else nextLine:=C_echoDeclInfix+C_messageClassMeta[mc_echo].levelColor;
          for s in P_storedMessageWithText(message)^.txt do begin
            if (length(nextLine)>10) and (length(nextLine)+length(s)>preferredLineLength)
            then begin
              append(result,trimRight(nextLine)+C_ANSI_CODE_RESET);
              nextLine:=C_echoContdInfix+C_messageClassMeta[mc_echo].levelColor+trimLeft(s);
            end else nextLine+=s;
          end;
          append(result,trimRight(nextLine)+C_ANSI_CODE_RESET);
        end;
        mt_echo_output: begin
          limitPerLiteral:=200*int64(preferredLineLength);
          if (limitPerLiteral>maxLongint) then limitPerLiteral:=maxLongint;

          if P_echoOutMessage(message)^.literal=nil
          then result:=''
          else result:=serializeToStringList(P_echoOutMessage(message)^.literal,
                                             C_nilSearchTokenLocation,nil,
                                             false,
                                             preferredLineLength-C_echoPrefixLength,
                                             limitPerLiteral);
          if length(result)>0 then result[  0]:=C_echoOutInfix+C_messageClassMeta[mc_echo].levelColor+result[0]+C_ANSI_CODE_RESET;
          for i:=1 to length(result)-1 do
            result[i]:=C_echoContdInfix+C_messageClassMeta[mc_echo].levelColor+result[i]+C_ANSI_CODE_RESET;
        end;
      end;
      mc_timing: for s in P_storedMessageWithText(message)^.txt do append(result,C_messageClassMeta[message^.messageClass].levelColor+ s + C_ANSI_CODE_RESET);
      mc_log    ,
      mc_note   ,
      mc_warning,
      mc_error  ,
      mc_trace  ,
      mc_fatal  : begin
        if length(P_storedMessageWithText(message)^.txt)=1 then begin
          result:=C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart+C_messageClassMeta[message^.messageClass].levelColor + P_storedMessageWithText(message)^.txt[0] + C_ANSI_CODE_RESET;
        end else begin
          result:=C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart;
          for s in P_storedMessageWithText(message)^.txt do
            append(result,StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1)+C_messageClassMeta[message^.messageClass].levelColor + s + C_ANSI_CODE_RESET);
        end;
        if (message^.internalType='T_errorMessage') then with P_errorMessage(message)^ do begin
          s:=StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt)+1);
          for i:=0 to length(stacktrace)-1 do
            append(result,s+formatLocation(stacktrace[i].location)+' call '+stacktrace[i].callee+' with '+stacktrace[i].parameters);
        end;
      end
      else result:=P_storedMessageWithText(message)^.txt;
    end;
  end;

{$ifdef fullVersion}
PROCEDURE sortProfilingList(VAR list:T_profilingList; CONST sortIndex:byte);
  FUNCTION lesser(CONST a,b:T_profilingListEntry):boolean;
    begin
      result:=false;
      case sortIndex of
        0: result:=a.id<b.id;
        1: result:=a.id>b.id;
        2: result:=a.calleeLocation<b.calleeLocation;
        3: result:=b.calleeLocation<a.calleeLocation;
        4: result:=a.aggTime.callCount<b.aggTime.callCount;
        5: result:=a.aggTime.callCount>b.aggTime.callCount;
        6: result:=a.aggTime.timeSpent_inclusive<b.aggTime.timeSpent_inclusive;
        7: result:=a.aggTime.timeSpent_inclusive>b.aggTime.timeSpent_inclusive;
        8: result:=a.aggTime.timeSpent_exclusive<b.aggTime.timeSpent_exclusive;
        9: result:=a.aggTime.timeSpent_exclusive>b.aggTime.timeSpent_exclusive;
      end;
    end;

  VAR i,j:longint;
      tmp:T_profilingListEntry;
  begin
    for i:=1 to length(list)-1 do
    for j:=0 to i-1 do if lesser(list[i],list[j]) then begin
      tmp:=list[i]; list[i]:=list[j]; list[j]:=tmp;
    end;
  end;

PROCEDURE sortCallerList(VAR list:T_callerList; CONST sortIndex:byte);
  FUNCTION lesser(CONST a,b:T_callerListEntry):boolean;
    begin
      result:=false;
      case sortIndex of
        0: result:=a.id<b.id;
        1: result:=a.id>b.id;
        2: result:=a.location<b.location;
        3: result:=b.location<a.location;
        4: result:=a.time.callCount          <b.time.callCount;
        5: result:=a.time.callCount          >b.time.callCount;
        6: result:=a.time.timeSpent_inclusive<b.time.timeSpent_inclusive;
        7: result:=a.time.timeSpent_inclusive>b.time.timeSpent_inclusive;
        8: result:=a.time.timeSpent_exclusive<b.time.timeSpent_exclusive;
        9: result:=a.time.timeSpent_exclusive>b.time.timeSpent_exclusive;
      end;
    end;

  VAR i,j:longint;
      tmp:T_callerListEntry;
  begin
    for i:=1 to length(list)-1 do
    for j:=0 to i-1 do if lesser(list[i],list[j]) then begin
      tmp:=list[i]; list[i]:=list[j]; list[j]:=tmp;
    end;
  end;

FUNCTION T_profileMessage.internalType: shortstring;
begin result:='T_profileMessage'; end;

CONSTRUCTOR T_profileMessage.create;
  begin
    inherited create(mt_profile_call_info);
  end;

DESTRUCTOR T_profileMessage.destroy;
  begin
    setLength(content,0);
  end;

FUNCTION T_profileMessage.toString(CONST formatter:P_messageFormatProvider): T_arrayOfString;
  FUNCTION nicestTime(CONST seconds:double):string;
    begin
       result:=formatFloat('0.000',seconds*1E3);
    end;

  FUNCTION profiledLocation(CONST location:T_searchTokenLocation):string;
    begin
      if startsWith(location,mnhSysPseudopackagePrefix)
      then result:='(builtin)'
      else result:=formatter^.formatLocation(location);
    end;

  VAR j,k:longint;
      shortId:string;
  begin
    for k:=0 to length(content)-1 do sortCallerList(content[k].callers,4);
    sortProfilingList(content,6);
    result:='id'          +C_tabChar+
            'location'    +C_tabChar+
            'count'       +C_tabChar+
            'inclusive ms'+C_tabChar+
            'exclusive ms';
    for k:=0 to length(content)-1 do with content[k] do begin
      if length(id)>50 then shortId:=copy(id,1,47)+'...' else shortId:=id;
      append(result,shortId                         +C_tabChar+
                    profiledLocation(calleeLocation)+C_tabChar+
                    intToStr  (aggTime.callCount)           +C_tabChar+
                    nicestTime(aggTime.timeSpent_inclusive) +C_tabChar+
                    nicestTime(aggTime.timeSpent_exclusive));
      for j:=0 to length(callers)-1 do begin
        append(result,BoolToStr(j=0,C_shiftInChar+'called at',' ')+C_tabChar+
                  profiledLocation(callers[j].location)           +C_tabChar+
                  intToStr  (callers[j].time.callCount)           +C_tabChar+
                  nicestTime(callers[j].time.timeSpent_inclusive) +C_tabChar+
                  nicestTime(callers[j].time.timeSpent_exclusive));
      end;
    end;
    formatTabs(result);
  end;
{$endif}

INITIALIZATION
  defaultConsoleFormatter.create;

FINALIZATION
  defaultConsoleFormatter.destroy;

end.
