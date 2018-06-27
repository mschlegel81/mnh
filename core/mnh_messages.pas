UNIT mnh_messages;
INTERFACE
USES sysutils,
     myGenerics,
     myStringUtil,
     mnh_constants,mnh_basicTypes;

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
                  mc_fatal  ,
                  mc_plot   ,
                  mc_gui);
CONST
  C_messageClassMeta:array[T_messageClass] of record guiMarker:string; htmlSpan:string; includeLocation:boolean; triggeredFlags:T_stateFlags; end=
    {mc_echo   }((guiMarker: ECHO_MARKER   ; htmlSpan:''     ; includeLocation:false; triggeredFlags:[]),
    {mc_print  } (guiMarker: ''            ; htmlSpan:''     ; includeLocation:false; triggeredFlags:[]),
    {mc_timing } (guiMarker: TIMING_MARKER ; htmlSpan:''     ; includeLocation:false; triggeredFlags:[]),
    {mc_note   } (guiMarker: NOTE_MARKER   ; htmlSpan:''     ; includeLocation:true;  triggeredFlags:[]),
    {mc_warning} (guiMarker: WARNING_MARKER; htmlSpan:''     ; includeLocation:true;  triggeredFlags:[]),
    {mc_error  } (guiMarker: ERROR_MARKER  ; htmlSpan:'error'; includeLocation:true;  triggeredFlags:[FlagQuietHalt,FlagError]),
    {mc_fatal  } (guiMarker: ERROR_MARKER  ; htmlSpan:'error'; includeLocation:false; triggeredFlags:[FlagFatalError,FlagQuietHalt]),
    {mc_plot   } (guiMarker: ''            ; htmlSpan:''     ; includeLocation:false; triggeredFlags:[]),
    {mc_gui}     (guiMarker: ''            ; htmlSpan:''     ; includeLocation:false; triggeredFlags:[]));

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
    mt_timing_info,
    mt_debugger_breakpoint,
    mt_displayTable,
    mt_plot_addText,
    mt_plot_dropRow,
    mt_plot_renderRequest,
    mt_plot_retrieveOptions,
    mt_plot_setOptions,
    mt_plot_clear);

  T_messageTypeSet=set of T_messageType;

CONST
  C_messageTypeMeta:array[T_messageType] of record
    level:shortint;
    mClass:T_messageClass;
    ignoredBySandbox,triggersGuiStartup:boolean;
    systemErrorLevel:byte;
  end = (
{mt_clearConsole      }  (level:-2; mClass:mc_print;   ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_printline         }  (level:-2; mClass:mc_print;   ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_print             }  (level:-2; mClass:mc_print;   ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_echo_input        }  (level:-1; mClass:mc_echo;    ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_echo_declaration  }  (level:-1; mClass:mc_echo;    ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_echo_output       }  (level:-1; mClass:mc_echo;    ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_echo_continued    }  (level:-1; mClass:mc_echo;    ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el1_note          }  (level: 1; mClass:mc_note;    ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el1_userNote      }  (level: 1; mClass:mc_note;    ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el2_warning       }  (level: 2; mClass:mc_warning; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el2_userWarning   }  (level: 2; mClass:mc_warning; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el3_evalError     }  (level: 3; mClass:mc_error;   ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:3),
{mt_el3_noMatchingMain}  (level: 3; mClass:mc_error;   ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:1),
{mt_el3_userDefined   }  (level: 3; mClass:mc_error;   ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:2),
{mt_el4_systemError   }  (level: 4; mClass:mc_error;   ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:5),
{mt_endOfEvaluation   }  (level:-1; mClass:mc_note;    ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_timing_info       }  (level:-1; mClass:mc_timing;  ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_debugger_breakpoint} (level:-1; mClass:mc_gui;     ignoredBySandbox:  true; triggersGuiStartup: true; systemErrorLevel:0),
{mt_displayTable}        (level:-1; mClass:mc_gui;     ignoredBySandbox:  true; triggersGuiStartup: true; systemErrorLevel:0),
{mt_plot_addText}        (level:-1; mClass:mc_plot;    ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_plot_dropRow}        (level:-1; mClass:mc_plot;    ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_plot_renderRequest}  (level:-1; mClass:mc_plot;    ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_plot_retrieveOptions}(level:-1; mClass:mc_plot;    ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_plot_setOptions}     (level:-1; mClass:mc_plot;    ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_plot_clear}          (level:-1; mClass:mc_plot;    ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0));

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
      FUNCTION toString(CONST forGui:boolean):T_arrayOfString; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION equals(CONST other:P_storedMessage):boolean; virtual;

      FUNCTION unreference:boolean;
      FUNCTION rereferenced:P_storedMessage;
      FUNCTION messageClass:T_messageClass;
      PROPERTY messageType:T_messageType read kind;
      FUNCTION messageText:T_arrayOfString; virtual;
      PROPERTY getLocation:T_searchTokenLocation read location;
  end;

  P_storedMessageWithText=^T_storedMessageWithText;
  T_storedMessageWithText=object(T_storedMessage)
    protected
      txt:T_arrayOfString;
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
      FUNCTION toString(CONST forGui:boolean):T_arrayOfString; virtual;
      FUNCTION equals(CONST other:P_storedMessage):boolean; virtual;
      FUNCTION messageText:T_arrayOfString; virtual;
  end;

  P_errorMessage=^T_errorMessage;
  T_errorMessage=object(T_storedMessageWithText)
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      stacktrace:array of record location:T_searchTokenLocation; callee:T_idString; end;
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
      FUNCTION toString(CONST forGui:boolean):T_arrayOfString; virtual;
      FUNCTION messageText:T_arrayOfString; virtual;
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

OPERATOR :=(CONST x:T_messageTypeSet):qword;
OPERATOR :=(x:qword):T_messageTypeSet;
PROCEDURE disposeMessage(message:P_storedMessage);
FUNCTION getPrefix(CONST messageType:T_messageType):shortstring;
IMPLEMENTATION
OPERATOR :=(CONST x:T_messageTypeSet):qword;
  VAR mt:T_messageType;
      mask:bitpacked array [0..sizeOf(qword)*8-1] of boolean;
      i:longint;
  begin
    for i:=0 to length(mask)-1 do mask[i]:=false;
    i:=length(mask)-1;
    for mt:=low(T_messageType) to high(T_messageType) do if i>0 then begin
      mask[i]:=mt in x;
      dec(i);
    end;
    result:=0;
    move(mask,result,sizeOf(qword));
  end;

OPERATOR :=(x:qword):T_messageTypeSet;
  VAR mt:T_messageType;
      mask:bitpacked array [0..sizeOf(qword)*8-1] of boolean;
      i:longint;
  begin
    initialize(mask);
    move(x,mask,sizeOf(qword));
    i:=length(mask)-1;
    result:=[];
    for mt:=low(T_messageType) to high(T_messageType) do begin
      if (i>0) and mask[i] then include(result,mt);
      dec(i);
    end;
  end;

PROCEDURE disposeMessage(message:P_storedMessage);
  begin
    if message^.unreference then dispose(message,destroy);
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
    txt:=message;
  end;

CONSTRUCTOR T_errorMessage.create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
  begin
    inherited create(messageType_,loc,message);
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

FUNCTION T_storedMessage.toString(CONST forGui: boolean): T_arrayOfString;
  begin
    result:=C_EMPTY_STRING_ARRAY;
  end;

FUNCTION T_storedMessageWithText.toString(CONST forGui: boolean): T_arrayOfString;
  VAR i:longint;
      loc:string='';
      marker:string;
  begin
    if kind in [mt_printline,mt_printdirect] then exit(txt);
    if forGui then marker:=C_messageClassMeta[C_messageTypeMeta[kind].mClass].guiMarker else marker:='';
    setLength(result,length(txt));
    with C_messageTypeMeta[kind] do begin
      if C_messageClassMeta[mClass].includeLocation then loc:=ansistring(location)+' ';
      for i:=0 to length(result)-1 do begin
        result[i]:=marker+prefix+loc+txt[i];
        if i=0 then loc:=StringOfChar(' ',length(loc));
      end;
    end;
  end;

FUNCTION T_errorMessage.toString(CONST forGui: boolean): T_arrayOfString;
  VAR i,i0:longint;
      marker:string;
  begin
    result:=inherited toString(forGui);
    if forGui then marker:=C_messageClassMeta[C_messageTypeMeta[kind].mClass].guiMarker else marker:='';
    i0:=length(result);
    setLength(result,i0+length(stacktrace));
    for i:=0 to length(stacktrace)-1 do begin
      result[i+i0]:=marker+string(stacktrace[i].location)+' call '+stacktrace[i].callee;
    end;
  end;

DESTRUCTOR T_storedMessage.destroy;
  begin
    if refCount<>0 then raise Exception.create('Disposing message with refCount='+intToStr(refCount));
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

FUNCTION T_storedMessageWithText.messageText: T_arrayOfString;
  begin
    result:=txt;
  end;

FUNCTION T_errorMessage.messageText: T_arrayOfString;
  begin
    result:=txt;
  end;

end.
