UNIT mnh_messages;
INTERFACE
USES sysutils,
     typinfo,
     myGenerics,
     basicTypes,
     serializationUtil;

TYPE
  T_stateFlag=(FlagQuietHalt,
               FlagError,
               FlagFatalError,
               FlagGUINeeded);
  T_stateFlags=set of T_stateFlag;

  T_messageClass=(mc_echo   ,
                  mc_print  ,
                  mc_timing ,
                  mc_note   ,
                  mc_warning,
                  mc_error  ,
                  mc_fatal
                  {$ifdef fullVersion},
                  mc_plot   ,
                  mc_gui
                  {$endif});
CONST
  //UTF-8 zero width and invisible characters
  ECHO_MARKER   =#226#128#139;
  NOTE_MARKER   =#226#128#140;
  ERROR_MARKER  =#226#129#162;
  WARNING_MARKER=#226#129#163;
  TIMING_MARKER =#226#129#164;
  TIMING_MARKER2=#226#128#141;

  C_messageClassMeta:array[T_messageClass] of record htmlSpan:string; includeLocation:boolean; triggeredFlags:T_stateFlags; end=
    {mc_echo   }((htmlSpan:''     ; includeLocation:false; triggeredFlags:[]),
    {mc_print  } (htmlSpan:''     ; includeLocation:false; triggeredFlags:[]),
    {mc_timing } (htmlSpan:''     ; includeLocation:false; triggeredFlags:[]),
    {mc_note   } (htmlSpan:''     ; includeLocation:true;  triggeredFlags:[]),
    {mc_warning} (htmlSpan:''     ; includeLocation:true;  triggeredFlags:[]),
    {mc_error  } (htmlSpan:'error'; includeLocation:true;  triggeredFlags:[FlagError]),
    {mc_fatal  } (htmlSpan:'error'; includeLocation:true;  triggeredFlags:[FlagFatalError])
    {$ifdef fullVersion},
    {mc_plot   } (htmlSpan:''     ; includeLocation:false; triggeredFlags:[]),
    {mc_gui}     (htmlSpan:''     ; includeLocation:false; triggeredFlags:[])
    {$endif});

TYPE
  T_messageType = (
    mt_clearConsole,
    mt_printline,
    mt_printdirect,
    mt_echo_input,
    mt_echo_declaration,
    mt_echo_output,
    mt_echo_continued,
    mt_el1_note,
    mt_el1_userNote,
    mt_el2_warning,
    mt_el2_userWarning,
    mt_el3_evalError,
    mt_el3_noMatchingMain,
    mt_el3_userDefined,
    mt_el4_systemError,
    mt_endOfEvaluation,
    mt_timing_info
    {$ifdef fullVersion},
    mt_profile_call_info,
    mt_startOfEvaluation,
    mt_debugger_breakpoint,
    mt_displayTable,
    mt_plot_addText,
    mt_plot_addRow,
    mt_plot_dropRow,
    mt_plot_renderRequest,
    mt_plot_retrieveOptions,
    mt_plot_setOptions,
    mt_plot_queryClosedByUser,
    mt_plot_clear,
    mt_plot_clearAnimation,
    mt_plot_clearAnimationVolatile,
    mt_plot_addAnimationFrame,
    mt_plot_postDisplay,
    mt_guiEdit_done,
    mt_guiEditScriptsLoaded,
    mt_displayVariableTree,
    mt_displayCustomForm
    {$endif});

  { T_ideMessageConfig }

  T_ideMessageConfig=object(T_serializable)
    echo_input,echo_declaration,echo_output,echo_wrapping,
    show_timing,
    show_all_userMessages:boolean;
    suppressWarningsUnderLevel:byte;

    CONSTRUCTOR create;
    PROCEDURE reset;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

  T_messageTypeSet=set of T_messageType;

CONST
  C_textMessages:T_messageTypeSet=[mt_clearConsole..mt_el4_systemError,mt_timing_info];
  C_errorsAndWarnings:T_messageTypeSet=[mt_el2_warning,mt_el2_userWarning,mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined,mt_el4_systemError];
  C_messagesSuppressedOnQuietHalt:T_messageTypeSet=[mt_el3_evalError,
                                                    mt_el3_noMatchingMain,
                                                    mt_el3_userDefined];
  C_messageTypeMeta:array[T_messageType] of record
    guiMarker:string[3];
    level:shortint;
    mClass:T_messageClass;
    systemErrorLevel:byte;
  end = (
{mt_clearConsole      }  (guiMarker: ''            ; level:-2; mClass:mc_print;   systemErrorLevel:0),
{mt_printline         }  (guiMarker: ''            ; level:-2; mClass:mc_print;   systemErrorLevel:0),
{mt_print             }  (guiMarker: ''            ; level:-2; mClass:mc_print;   systemErrorLevel:0),
{mt_echo_input        }  (guiMarker: ECHO_MARKER   ; level:-1; mClass:mc_echo;    systemErrorLevel:0),
{mt_echo_declaration  }  (guiMarker: ECHO_MARKER   ; level:-1; mClass:mc_echo;    systemErrorLevel:0),
{mt_echo_output       }  (guiMarker: ECHO_MARKER   ; level:-1; mClass:mc_echo;    systemErrorLevel:0),
{mt_echo_continued    }  (guiMarker: ECHO_MARKER   ; level:-1; mClass:mc_echo;    systemErrorLevel:0),
{mt_el1_note          }  (guiMarker: NOTE_MARKER   ; level: 1; mClass:mc_note;    systemErrorLevel:0),
{mt_el1_userNote      }  (guiMarker: NOTE_MARKER   ; level: 1; mClass:mc_note;    systemErrorLevel:0),
{mt_el2_warning       }  (guiMarker: WARNING_MARKER; level: 2; mClass:mc_warning; systemErrorLevel:0),
{mt_el2_userWarning   }  (guiMarker: WARNING_MARKER; level: 2; mClass:mc_warning; systemErrorLevel:0),
{mt_el3_evalError     }  (guiMarker: ERROR_MARKER  ; level: 3; mClass:mc_error;   systemErrorLevel:3),
{mt_el3_noMatchingMain}  (guiMarker: ERROR_MARKER  ; level: 3; mClass:mc_error;   systemErrorLevel:1),
{mt_el3_userDefined   }  (guiMarker: ERROR_MARKER  ; level: 3; mClass:mc_error;   systemErrorLevel:2),
{mt_el4_systemError   }  (guiMarker: ERROR_MARKER  ; level: 4; mClass:mc_fatal;   systemErrorLevel:5),
{mt_endOfEvaluation   }  (guiMarker: NOTE_MARKER   ; level:-1; mClass:mc_note;    systemErrorLevel:0),
{mt_timing_info       }  (guiMarker: TIMING_MARKER ; level:-1; mClass:mc_timing;  systemErrorLevel:0)
{$ifdef fullVersion},
{mt_profile_call_info}   (guiMarker: ''            ; level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_startOfEvaluation}   (guiMarker: ''            ; level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_debugger_breakpoint} (guiMarker: ''            ; level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_displayTable}        (guiMarker: ''            ; level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_plot_addText}        (guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_addRow}         (guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_dropRow}        (guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_renderRequest}  (guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_retrieveOptions}(guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_setOptions}     (guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_queryClosedB...}(guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_clear}          (guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_clearAnimation} (guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_clearAnimati...}(guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_addAnimation...}(guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_postDisplay}    (guiMarker: ''            ; level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_guiEdit_done}        (guiMarker: ''            ; level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_guiEditScriptsLoaded}(guiMarker: ''            ; level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_displayVariableTree} (guiMarker: ''            ; level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_displayCustomForm}   (guiMarker: ''            ; level:-1; mClass:mc_gui;     systemErrorLevel:0)
{$endif});

  C_errorMessageTypes:array[1..4] of T_messageTypeSet=(
    [mt_el1_note,mt_el1_userNote],
    [mt_el2_warning,mt_el2_userWarning],
    [mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined],
    [mt_el4_systemError]);

TYPE
  P_storedMessage=^T_storedMessage;
  T_storedMessages=array of P_storedMessage;
  T_storedMessage=object
    protected
      refCount:longint;
      location:T_searchTokenLocation;
      kind:T_messageType;
      FUNCTION internalType:shortstring; virtual;
    public
      FUNCTION prefix:shortstring;
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation);
      FUNCTION toString({$WARN 5024 OFF}{$ifdef fullVersion}CONST forGui:boolean{$endif}):T_arrayOfString; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION equals(CONST other:P_storedMessage):boolean; virtual;

      FUNCTION unreference:boolean;
      FUNCTION rereferenced:P_storedMessage;
      FUNCTION messageClass:T_messageClass;
      PROPERTY messageType:T_messageType read kind;
      FUNCTION messageText:T_arrayOfString; virtual;
      FUNCTION getMessageTypeName:string;
      PROPERTY getLocation:T_searchTokenLocation read location;
      PROPERTY getRefCount:longint read refCount;
  end;

  P_storedMessageWithText=^T_storedMessageWithText;
  T_storedMessageWithText=object(T_storedMessage)
    protected
      txt:T_arrayOfString;
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
      FUNCTION toString({$ifdef fullVersion}CONST forGui:boolean{$endif}):T_arrayOfString; virtual;
      FUNCTION equals(CONST other:P_storedMessage):boolean; virtual;
      FUNCTION messageText:T_arrayOfString; virtual;
      DESTRUCTOR destroy; virtual;
  end;

  P_errorMessage=^T_errorMessage;
  T_errorMessage=object(T_storedMessageWithText)
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      stacktrace:array of record location:T_searchTokenLocation; callee:T_idString; parameters:string; end;
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
      FUNCTION toString({$ifdef fullVersion}CONST forGui:boolean{$endif}):T_arrayOfString; virtual;
      FUNCTION messageText:T_arrayOfString; virtual;
      DESTRUCTOR destroy; virtual;
  end;

  P_payloadMessage=^T_payloadMessage;
  T_payloadMessage=object(T_storedMessage)
    protected
      messageCs:TRTLCriticalSection;
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create(CONST messageType_:T_messageType);
      FUNCTION equals(CONST other:P_storedMessage):boolean; virtual;
      DESTRUCTOR destroy; virtual;
  end;

OPERATOR :=(CONST x:T_ideMessageConfig):T_messageTypeSet;
PROCEDURE disposeMessage(VAR message:P_storedMessage);
PROCEDURE disposeMessage_(message:P_storedMessage);
FUNCTION getPrefix(CONST messageType:T_messageType):shortstring;
FUNCTION messageTypeName(CONST m:T_messageType):string;
IMPLEMENTATION
OPERATOR :=(CONST x:T_ideMessageConfig):T_messageTypeSet;
  begin
    result:=[mt_clearConsole,mt_printline,mt_printdirect,mt_endOfEvaluation{$ifdef fullVersion},mt_startOfEvaluation{$endif},mt_el4_systemError,mt_el3_noMatchingMain];
    if x.echo_input       then result+=[mt_echo_input      ,mt_echo_continued];
    if x.echo_output      then result+=[mt_echo_output     ,mt_echo_continued];
    if x.echo_declaration then result+=[mt_echo_declaration,mt_echo_continued];
    if x.show_timing      then result+=[mt_timing_info];
    if x.show_all_userMessages then result+=[mt_el1_userNote,mt_el2_userWarning,mt_el3_userDefined];
    case x.suppressWarningsUnderLevel of
        4: begin end;
        3: result+=[mt_el3_evalError,mt_el3_userDefined];
        2: result+=[mt_el3_evalError,mt_el3_userDefined,mt_el2_warning,mt_el2_userWarning];
      else result+=[mt_el3_evalError,mt_el3_userDefined,mt_el2_warning,mt_el2_userWarning,mt_el1_note,mt_el1_userNote];
    end;
  end;

PROCEDURE disposeMessage(VAR message:P_storedMessage);
  begin
    if message^.unreference then dispose(message,destroy);
    message:=nil;
  end;

PROCEDURE disposeMessage_(message:P_storedMessage);
  begin
    if message^.unreference then dispose(message,destroy);
    message:=nil;
  end;

CONSTRUCTOR T_ideMessageConfig.create;
  begin
    reset;
  end;

PROCEDURE T_ideMessageConfig.reset;
  begin
    echo_input           :=true;
    echo_declaration     :=true;
    echo_output          :=true;
    echo_wrapping        :=false;
    show_timing          :=false;
    show_all_userMessages:=false;
    suppressWarningsUnderLevel:=3;
  end;

FUNCTION T_ideMessageConfig.getSerialVersion: dword;
  begin
    result:=232325;
  end;

FUNCTION T_ideMessageConfig.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR b:byte;
  begin
    b:=stream.readByte([0..63]);
    echo_input           :=(b and  1)>0;
    echo_declaration     :=(b and  2)>0;
    echo_output          :=(b and  4)>0;
    echo_wrapping        :=(b and  8)>0;
    show_timing          :=(b and 16)>0;
    show_all_userMessages:=(b and 32)>0;
    suppressWarningsUnderLevel:=stream.readByte([0..4]);
    if not(stream.allOkay) then begin
      reset;
      result:=false;
    end else result:=true;
  end;

PROCEDURE T_ideMessageConfig.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR b:byte;
  begin
    b:=0;
    if echo_input            then b+= 1;
    if echo_declaration      then b+= 2;
    if echo_output           then b+= 4;
    if echo_wrapping         then b+= 8;
    if show_timing           then b+=16;
    if show_all_userMessages then b+=32;
    stream.writeByte(b);
    stream.writeByte(suppressWarningsUnderLevel);
  end;

FUNCTION T_storedMessage.internalType: shortstring; begin result:='T_storedMessage'; end;
FUNCTION T_storedMessageWithText.internalType: shortstring; begin result:='T_storedMessageWithText'; end;
FUNCTION T_payloadMessage.internalType: shortstring; begin result:='T_payloadMessage'; end;
FUNCTION T_errorMessage.internalType: shortstring; begin result:='T_errorMessage'; end;

FUNCTION getPrefix(CONST messageType:T_messageType):shortstring;
  begin
    case messageType of
      mt_echo_input,
      mt_echo_declaration:result:=' in>';
      mt_echo_output     :result:='out>';
      mt_echo_continued  :result:='...>';
      mt_el1_note,
      mt_el1_userNote    :result:='Note ';
      mt_el2_warning,
      mt_el2_userWarning :result:='Warning ';
      mt_el3_evalError,
      mt_el3_userDefined :result:='Error ';
      mt_el4_systemError :result:='Fatal ';
      else result:='';
    end;
  end;

FUNCTION T_storedMessage.prefix: shortstring;
  begin result:=getPrefix(kind); end;

CONSTRUCTOR T_storedMessageWithText.create(CONST messageType_: T_messageType; CONST loc: T_searchTokenLocation; CONST message: T_arrayOfString);
  begin
    inherited create(messageType_,loc);
    setLength(txt,0);
    append(txt,message);
  end;

CONSTRUCTOR T_errorMessage.create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
  begin
    inherited create(messageType_,loc,message);
    setLength(stacktrace,0);
  end;

CONSTRUCTOR T_storedMessage.create(CONST messageType_: T_messageType; CONST loc: T_searchTokenLocation);
  begin
    location:=loc;
    kind:=messageType_;
    refCount:=1;
  end;

CONSTRUCTOR T_payloadMessage.create(CONST messageType_: T_messageType);
  begin
    inherited create(messageType_,C_nilTokenLocation);
    initCriticalSection(messageCs);
  end;

FUNCTION T_storedMessage.toString({$ifdef fullVersion}CONST forGui:boolean{$endif}): T_arrayOfString;
  begin
    result:=C_EMPTY_STRING_ARRAY;
  end;

FUNCTION T_storedMessageWithText.toString({$ifdef fullVersion}CONST forGui:boolean{$endif}): T_arrayOfString;
  VAR i:longint;
      loc:string='';
      marker:string;
  begin
    if kind in [mt_printline,mt_printdirect] then exit(txt);
    {$ifdef fullVersion}if forGui then marker:=C_messageTypeMeta[kind].guiMarker else {$endif} marker:='';
    setLength(result,length(txt));
    with C_messageTypeMeta[kind] do begin
      if C_messageClassMeta[mClass].includeLocation then loc:=ansistring(location)+' ';
      for i:=0 to length(result)-1 do begin
        result[i]:=marker+prefix+loc+txt[i];
        if i=0 then loc:=StringOfChar(' ',length(loc));
      end;
    end;
  end;

FUNCTION T_errorMessage.toString({$ifdef fullVersion}CONST forGui:boolean{$endif}): T_arrayOfString;
  VAR i,i0:longint;
      marker:string;
  begin
    result:=inherited toString({$ifdef fullVersion}forGui{$endif});
    {$ifdef fullVersion}if forGui then marker:=C_messageTypeMeta[kind].guiMarker else {$endif}marker:='';
    i0:=length(result);
    setLength(result,i0+length(stacktrace));
    for i:=0 to length(stacktrace)-1 do begin
      result[i+i0]:=marker+string(stacktrace[i].location)+' call '+stacktrace[i].callee+' with '+stacktrace[i].parameters;
    end;
  end;

DESTRUCTOR T_storedMessage.destroy;
  begin
    if refCount<>0 then raise Exception.create('Disposing message with refCount='+intToStr(refCount));
  end;

DESTRUCTOR T_storedMessageWithText.destroy;
  VAR i:longint;
  begin
    inherited destroy;
    for i:=0 to length(txt)-1 do txt[i]:='';
    setLength(txt,0);
  end;

DESTRUCTOR T_errorMessage.destroy;
  begin
    inherited destroy;
    setLength(stacktrace,0);
  end;

DESTRUCTOR T_payloadMessage.destroy;
  begin
    inherited destroy;
    doneCriticalSection(messageCs);
  end;

FUNCTION T_storedMessage.equals(CONST other: P_storedMessage): boolean;
  begin
    result:=(other=@self) or
            (other^.kind=kind) and
            (other^.internalType=internalType) and
            (other^.location=location);
  end;

FUNCTION T_storedMessageWithText.equals(CONST other: P_storedMessage): boolean;
  begin
    result:=inherited equals(other) and arrEquals(txt,P_storedMessageWithText(other)^.txt);
  end;

FUNCTION T_payloadMessage.equals(CONST other: P_storedMessage): boolean;
  begin
    result:=other=@self;
  end;

FUNCTION T_storedMessage.unreference: boolean;
  begin
    result:=interlockedDecrement(refCount)<=0;
  end;

FUNCTION T_storedMessage.rereferenced: P_storedMessage;
  begin
    interLockedIncrement(refCount);
    result:=@self;
  end;

FUNCTION T_storedMessage.messageClass: T_messageClass;
  begin
    result:=C_messageTypeMeta[kind].mClass;
  end;

FUNCTION T_storedMessage.messageText: T_arrayOfString;
  begin
    result:=C_EMPTY_STRING_ARRAY;
  end;

VAR messageTypeNames:array[T_messageType] of string;

FUNCTION messageTypeName(CONST m:T_messageType):string;
  VAR mt:T_messageType;
  begin
    if messageTypeNames[mt_printline]='' then begin
      for mt in T_messageType do messageTypeNames[mt]:=copy(getEnumName(TypeInfo(mt),ord(mt)),4,1000);
    end;
    result:=messageTypeNames[m];
  end;

FUNCTION T_storedMessage.getMessageTypeName:string;
  begin
    result:= copy(getEnumName(TypeInfo(kind),ord(kind)),4,1000);
  end;

FUNCTION T_storedMessageWithText.messageText: T_arrayOfString;
  begin
    result:=txt;
  end;

FUNCTION T_errorMessage.messageText: T_arrayOfString;
  begin
    result:=txt;
  end;

INITIALIZATION
  messageTypeNames[mt_printline]:='';

end.
