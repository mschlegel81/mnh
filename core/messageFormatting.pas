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
    private
      maxLocationLength:longint;
    public
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
USES sysutils;
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
  begin
    enterCriticalSection(messageCs);
    disposeLiteral(literal);
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
  FUNCTION getLocationPart:string;
    VAR loc:T_searchTokenLocation;
        s  :string;
    begin
      loc:=message^.getLocation;
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
          result:=extractFileName(message^.getLocation.fileName);
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
    if (message=nil) or not(message^.isTextMessage) then exit(C_EMPTY_STRING_ARRAY);

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

    locationPart:=getLocationPart;
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
      msgTxt      :T_arrayOfString;
  begin
    if (message=nil) or (not(message^.isTextMessage) and (message^.messageType<>mt_echo_output))  then exit(C_EMPTY_STRING_ARRAY);
    if not(formatterForDemos)
    then locationPart:=string(message^.getLocation)+' ';

    marker:=C_messageClassMeta[message^.messageClass].guiMarker;
    setLength(result,0);

    case message^.messageClass of
      mc_echo   : begin
        case message^.messageType of
          mt_echo_input      : begin nextLine:='  in> '; msgTxt:=P_storedMessageWithText(message)^.txt; end;
          mt_echo_declaration: begin nextLine:='decl> '; msgTxt:=P_storedMessageWithText(message)^.txt; end;
          mt_echo_output     : begin
            nextLine:=' out> ';
            if wrapEcho
            then msgTxt:=serializeToStringList(P_echoOutMessage(message)^.literal,C_nilSearchTokenLocation,nil,preferredLineLength)
            else msgTxt:=P_echoOutMessage(message)^.literal^.toString();
          end;
        end;
        for s in msgTxt do begin
          if (length(nextLine)>10) and (length(nextLine)+length(s)>preferredLineLength)
          then begin
            append(result,marker+trimRight(nextLine));
            nextLine:=' ...> '+trimLeft(s);
          end else nextLine+=s;
        end;
        append(result,marker+trimRight(nextLine));
      end;
      mc_timing: for s in P_storedMessageWithText(message)^.txt do append(result,marker+s);
      mc_log    ,
      mc_note   ,
      mc_warning,
      mc_error  ,
      mc_fatal  : if length(P_storedMessageWithText(message)^.txt)=1 then begin
        result:=marker+C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart+P_storedMessageWithText(message)^.txt[0];
      end else begin
        result:=marker+C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart;
        for s in P_storedMessageWithText(message)^.txt do
          append(result,marker+StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt))+' '+s);
      end
      else for s in P_storedMessageWithText(message)^.txt do append(result,''+s);
    end;
  end;

FUNCTION T_defaultConsoleFormatter.formatMessage(CONST message: P_storedMessage): T_arrayOfString;
  VAR locationPart:string='';
      nextLine    :string='';
      s           :string;
      msgTxt      :T_arrayOfString;

  FUNCTION shortLocationString(CONST x:T_searchTokenLocation):string;
    begin
      if (x.fileName='?') and (x.line=0) and (x.column=0) then exit('');
      if x.column<0
      then result:='@'+extractFileName(x.fileName)+':'+intToStr(x.line)+',1'
      else result:='@'+extractFileName(x.fileName)+':'+intToStr(x.line)+','+intToStr(x.column);
    end;

  begin
    if (message=nil) or not(message^.isTextMessage) then exit(C_EMPTY_STRING_ARRAY);
    locationPart:=shortLocationString(message^.getLocation)+' ';

    setLength(result,0);

    case message^.messageClass of
      mc_echo   : begin
        case message^.messageType of
          mt_echo_input      : begin nextLine:='  in> '; msgTxt:=P_storedMessageWithText(message)^.txt; end;
          mt_echo_declaration: begin nextLine:='decl> '; msgTxt:=P_storedMessageWithText(message)^.txt; end;
          mt_echo_output     : begin
            nextLine:=' out> ';
            msgTxt:=serializeToStringList(P_echoOutMessage(message)^.literal,C_nilSearchTokenLocation,nil)
          end;
        end;
        for s in msgTxt do begin
          if (length(nextLine)>10) and (length(nextLine)+length(s)>100)
          then begin
            append(result,trimRight(nextLine));
            nextLine:=' ...> '+trimLeft(s);
          end else nextLine+=s;
        end;
        append(result,trimRight(nextLine));
      end;
      mc_timing: for s in P_storedMessageWithText(message)^.txt do append(result,s);
      mc_log    ,
      mc_note   ,
      mc_warning,
      mc_error  ,
      mc_fatal  : if length(P_storedMessageWithText(message)^.txt)=1 then begin
        result:=C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart+P_storedMessageWithText(message)^.txt[0];
      end else begin
        result:=C_messageClassMeta[message^.messageClass].levelTxt+' '+locationPart;
        for s in P_storedMessageWithText(message)^.txt do
          append(result,StringOfChar(' ',length(C_messageClassMeta[message^.messageClass].levelTxt))+' '+s);
      end
      else result:=P_storedMessageWithText(message)^.txt;
    end;
  end;

//CONSTRUCTOR T_messageFormatProvider.clone(CONST original: P_messageFormatProvider);
//  VAR mt:T_formattableMessageClass;
//      b :boolean;
//  begin
//    create;
//    for mt in T_formattableMessageClass do for b in boolean do setMessageFormat(mt,b,original^.getMessageFormat(mt,b));
//  end;

//PROCEDURE T_messageFormatProvider.setDefaults(CONST forGui: boolean);
//  VAR mc:T_formattableMessageClass;
//      first:boolean;
//  PROCEDURE append(CONST t:T_messagePatternElementType; CONST literal:string='');
//    begin
//      setLength(formatStrings[mc,first],length(formatStrings[mc,first])+1);
//      with formatStrings[mc,first,length(formatStrings[mc,first])-1] do begin
//        elementType:=t;
//        litValue:=literal;
//      end;
//    end;
//
//  begin
//    for mc in T_formattableMessageClass do for first:=false to true do setLength(formatStrings[mc,first],0);
//    setMessageFormat(fm_print,true ,'%message%');
//    setMessageFormat(fm_print,false,'%message%');
//    if forGui then begin
//      setMessageFormat(fm_echoIn    ,true ,ECHO_MARKER+' in> %message%');
//      setMessageFormat(fm_echoIn    ,false,ECHO_MARKER+'...> %message%');
//      setMessageFormat(fm_echoOut   ,true ,ECHO_MARKER+'out> %message%');
//      setMessageFormat(fm_echoOut   ,false,ECHO_MARKER+'...> %message%');
//      setMessageFormat(fm_note      ,true ,NOTE_MARKER+'Note %location%%t%%message%');
//      setMessageFormat(fm_note      ,false,NOTE_MARKER+'%t%%message%');
//      setMessageFormat(fm_warn      ,true ,WARNING_MARKER+'Warning %location%%t%%message%');
//      setMessageFormat(fm_warn      ,false,WARNING_MARKER+'%t%%message%');
//      setMessageFormat(fm_error     ,true ,ERROR_MARKER+'Error %location%%t%%message%');
//      setMessageFormat(fm_error     ,false,ERROR_MARKER+'%t%%message%');
//      setMessageFormat(fm_stacktrace,true ,ERROR_MARKER+'%location%%t%%message%');
//    end else begin
//      setMessageFormat(fm_echoIn    ,true ,' in> %message%');
//      setMessageFormat(fm_echoIn    ,false,'...> %message%');
//      setMessageFormat(fm_echoOut   ,true ,'out> %message%');
//      setMessageFormat(fm_echoOut   ,false,'...> %message%');
//      setMessageFormat(fm_note      ,true ,'Note %shortLoc%%t%%message%');
//      setMessageFormat(fm_note      ,false,'%t%%message%');
//      setMessageFormat(fm_warn      ,true ,'Warning %shortLoc%%t%%message%');
//      setMessageFormat(fm_warn      ,false,'%t%%message%');
//      setMessageFormat(fm_error     ,true ,'Error %shortLoc%%t%%message%');
//      setMessageFormat(fm_error     ,false,'%t%%message%');
//      setMessageFormat(fm_stacktrace,true ,'%shortLoc%%t%%message%');
//    end;
//  end;

//FUNCTION T_messageFormatProvider.formatMessage(CONST message: T_arrayOfString;
//  CONST msgTime: double; CONST loc: T_SearchTokenLocation;
//  CONST formattableMessageClass: T_formattableMessageClass): T_arrayOfString;
//  FUNCTION getPart(CONST el:T_messagePatternElement; CONST messageLineIndex:longint):string;
//    begin
//      case el.elementType of
//        et_literal :
//          result:=el.litValue;
//        et_lineBreak:
//          result:=C_lineBreakChar;
//        et_tab:
//          result:=C_tabChar;
//        et_message :
//          result:=message[messageLineIndex];
//        et_shortLoc:
//          begin
//            if (loc.fileName='?') and (loc.line=0) and (loc.column=0)
//            then exit('@?')
//            else result:='@'+extractFileName(loc.fileName)+':';
//            if loc.column<0
//            then result+=intToStr(loc.line)+',1'
//            else result+=intToStr(loc.line)+','+intToStr(loc.column);
//          end;
//        et_fullLoc :
//          result:=string(loc);
//        et_time    :
//          result:=FormatDateTime('hh:nn:ss.zzz',msgTime);
//        et_datetime:
//          result:=FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',msgTime);
//        else raise Exception.create('Missing implementation for element type! '+getEnumName(TypeInfo(T_messagePatternElementType),integer(el.elementType)));
//      end;
//    end;
//
//  VAR messageLineIndex,i:longint;
//      resultLine:string;
//  begin
//    if (formattableMessageClass=fm_NOT_FORMATTABLE) or (length(formatStrings[formattableMessageClass,true])=0) then exit(message);
//
//    setLength(result,0);
//    for messageLineIndex:=0 to length(message)-1 do begin
//      resultLine:='';
//      for i:=0 to length(formatStrings[formattableMessageClass])-1 do
//        resultLine+=getPart(formatStrings[formattableMessageClass,messageLineIndex=0,i],messageLineIndex);
//      append(result,resultLine);
//    end;
//    result:=formatTabs(result);
//  end;

//FUNCTION T_messageFormatProvider.getMessageFormat(CONST messageType: T_formattableMessageClass; CONST firstLine: boolean): string;
//  FUNCTION getPart(CONST el:T_messagePatternElement):string;
//    begin
//      case el.elementType of
//        et_literal :
//          result:=el.litValue;
//        else
//          result:=C_messagePatternElementTypePlaceholder[el.elementType];
//      end;
//    end;
//
//  VAR i:longint;
//  begin
//    result:='';
//    for i:=0 to length(formatStrings[messageType,firstLine])-1 do
//      result+=getPart(formatStrings[messageType,firstLine,i]);
//  end;
//
//PROCEDURE T_messageFormatProvider.setMessageFormat(CONST messageType: T_formattableMessageClass; CONST firstLine: boolean; CONST format: string);
//  VAR newParts:array of T_messagePatternElement;
//      rest:string;
//      placeholder:T_messagePatternElementType;
//      k:longint;
//      literalValue:boolean;
//
//  FUNCTION appendElement(CONST typ:T_messagePatternElementType):longint;
//    begin
//      result:=length(newParts);
//      setLength(newParts,result+1);
//      newParts[result].elementType:=typ;
//      newParts[result].litValue:='';
//    end;
//
//  begin
//    setLength(newParts,0);
//    rest:=format;
//    while length(rest)>0 do begin
//      literalValue:=true;
//      for placeholder := low(C_messagePatternElementTypePlaceholder) to high(C_messagePatternElementTypePlaceholder)
//      do if startsWith(rest,C_messagePatternElementTypePlaceholder[placeholder]) then begin
//        appendElement(placeholder);
//        rest:=copy(rest,1+length(C_messagePatternElementTypePlaceholder[placeholder]),length(rest));
//        literalValue:=false;
//      end;
//      if literalValue then begin
//        if rest[1]=C_lineBreakChar then begin
//          appendElement(et_lineBreak);
//          rest:=copy(rest,2,length(rest)-1);
//        end else if rest[1]=C_tabChar then begin
//          appendElement(et_tab);
//          rest:=copy(rest,2,length(rest)-1);
//        end else begin
//          k:=length(newParts);
//          if (k>0) and (newParts[k-1].elementType=et_literal)
//          then dec(k)
//          else k:=appendElement(et_literal);
//          newParts[k].litValue+=copy(rest,1,1);
//          rest:=copy(rest,2,length(rest)-1);
//        end;
//      end;
//    end;
//    formatStrings[messageType,firstLine]:=newParts;
//  end;
//
//FUNCTION T_messageFormatProvider.getSerialVersion: dword;
//  begin
//    result:=193864203;
//  end;
//
//FUNCTION T_messageFormatProvider.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
//  VAR fmc:T_formattableMessageClass;
//      b  :boolean;
//      i  :longint;
//      len:longint;
//  begin
//    for fmc in T_formattableMessageClass do
//    for b in boolean do begin
//      len:=stream.readNaturalNumber;
//      setLength(formatStrings[fmc,b],len);
//      for i:=0 to length(formatStrings[fmc,b])-1 do begin
//        formatStrings[fmc,b,i].elementType:=T_messagePatternElementType(stream.readByte([byte(low(T_messagePatternElementType))..byte(high(T_messagePatternElementType))]));
//        if not(stream.allOkay) then exit(false);
//        if formatStrings[fmc,b,i].elementType=et_literal
//        then formatStrings[fmc,b,i].litValue:=stream.readAnsiString;
//      end;
//    end;
//    result:=stream.allOkay;
//  end;
//
//PROCEDURE T_messageFormatProvider.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
//  VAR fmc:T_formattableMessageClass;
//      b  :boolean;
//      i  :longint;
//  begin
//    for fmc in T_formattableMessageClass do
//    for b in boolean do begin
//      stream.writeNaturalNumber(length(formatStrings[fmc,b]));
//      for i:=0 to length(formatStrings[fmc,b])-1 do begin
//        stream.writeByte(byte(formatStrings[fmc,b,i].elementType));
//        if formatStrings[fmc,b,i].elementType=et_literal
//        then stream.writeAnsiString(formatStrings[fmc,b,i].litValue);
//      end;
//    end;
//  end;

INITIALIZATION
  defaultConsoleFormatter.create;

FINALIZATION
  defaultConsoleFormatter.destroy;

end.
