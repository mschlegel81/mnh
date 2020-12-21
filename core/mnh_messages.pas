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
                  mc_log    ,
                  mc_timing ,
                  mc_note   ,
                  mc_warning,
                  mc_error  ,
                  mc_fatal
                  {$ifdef fullVersion},
                  mc_plot   ,
                  mc_image,
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

  C_echoPrefixLength=6;

  C_messageClassMeta:array[T_messageClass] of record htmlSpan:string; guiMarker:string; levelTxt:string; triggeredFlags:T_stateFlags; end=
    {mc_echo   }((htmlSpan:''     ; guiMarker:ECHO_MARKER;    levelTxt:'';      triggeredFlags:[]),
    {mc_print  } (htmlSpan:''     ; guiMarker:'';             levelTxt:'';      triggeredFlags:[]),
    {mc_log    } (htmlSpan:''     ; guiMarker:'';             levelTxt:'log';   triggeredFlags:[]),
    {mc_timing } (htmlSpan:''     ; guiMarker:TIMING_MARKER;  levelTxt:'';      triggeredFlags:[]),
    {mc_note   } (htmlSpan:''     ; guiMarker:NOTE_MARKER;    levelTxt:'Note';  triggeredFlags:[]),
    {mc_warning} (htmlSpan:''     ; guiMarker:WARNING_MARKER; levelTxt:'Warn';  triggeredFlags:[]),
    {mc_error  } (htmlSpan:'error'; guiMarker:ERROR_MARKER;   levelTxt:'Error'; triggeredFlags:[FlagError]),
    {mc_fatal  } (htmlSpan:'error'; guiMarker:ERROR_MARKER;   levelTxt:'Fatal'; triggeredFlags:[FlagFatalError])
    {$ifdef fullVersion},
    {mc_plot   } (htmlSpan:''     ; guiMarker:''; levelTxt:''; triggeredFlags:[]),
                 (htmlSpan:''     ; guiMarker:''; levelTxt:''; triggeredFlags:[]),
    {mc_gui}     (htmlSpan:''     ; guiMarker:''; levelTxt:''; triggeredFlags:[])
    {$endif});

TYPE
  T_messageType = (
    mt_clearConsole,
    mt_printline,
    mt_printdirect,
    mt_log,
    mt_echo_input,
    mt_echo_declaration,
    mt_echo_output,
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
    mt_image_postDisplay,     //signal
    mt_image_load,            //dedicated type
    mt_image_replaceImage,    //dedicated type
    mt_image_close,           //signal
    mt_image_obtainImageData, //dedicated type
    mt_image_obtainDimensions,//dedicated type
    mt_displayVariableTree,
    mt_displayCustomForm
    {$endif});

  T_messageTypeSet=set of T_messageType;

  T_ideMessageConfig=object(T_serializable)
    echo_input,echo_declaration,echo_output,
      echo_wrapping, //TODO: Echo_wrapping might be obsolete by now...
    show_timing,
    show_all_userMessages:boolean;
    suppressWarningsUnderLevel:byte;

    CONSTRUCTOR create;
    PROCEDURE reset;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

CONST
  C_textMessages:T_messageTypeSet=[mt_clearConsole..mt_el4_systemError,mt_timing_info];
  C_errorsAndWarnings:T_messageTypeSet=[mt_el2_warning,mt_el2_userWarning,mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined,mt_el4_systemError];
  C_messagesSuppressedOnQuietHalt:T_messageTypeSet=[mt_el3_evalError,
                                                    mt_el3_noMatchingMain,
                                                    mt_el3_userDefined];
  C_messageTypeMeta:array[T_messageType] of record
    level:shortint;
    mClass:T_messageClass;
    systemErrorLevel:byte;
  end = (

{mt_clearConsole      }  (level:-2; mClass:mc_print;   systemErrorLevel:0),
{mt_printline         }  (level:-2; mClass:mc_print;   systemErrorLevel:0),
{mt_print             }  (level:-2; mClass:mc_print;   systemErrorLevel:0),
{mt_log               }  (level:-2; mClass:mc_log;     systemErrorLevel:0),
{mt_echo_input        }  (level:-1; mClass:mc_echo;    systemErrorLevel:0),
{mt_echo_declaration  }  (level:-1; mClass:mc_echo;    systemErrorLevel:0),
{mt_echo_continued    }  (level:-1; mClass:mc_echo;    systemErrorLevel:0),
{mt_el1_note          }  (level: 1; mClass:mc_note;    systemErrorLevel:0),
{mt_el1_userNote      }  (level: 1; mClass:mc_note;    systemErrorLevel:0),
{mt_el2_warning       }  (level: 2; mClass:mc_warning; systemErrorLevel:0),
{mt_el2_userWarning   }  (level: 2; mClass:mc_warning; systemErrorLevel:0),
{mt_el3_evalError     }  (level: 3; mClass:mc_error;   systemErrorLevel:3),
{mt_el3_noMatchingMain}  (level: 3; mClass:mc_error;   systemErrorLevel:1),
{mt_el3_userDefined   }  (level: 3; mClass:mc_error;   systemErrorLevel:2),
{mt_el4_systemError   }  (level: 4; mClass:mc_fatal;   systemErrorLevel:5),
{mt_endOfEvaluation   }  (level:-1; mClass:mc_note;    systemErrorLevel:0),
{mt_timing_info       }  (level:-1; mClass:mc_timing;  systemErrorLevel:0)
{$ifdef fullVersion},
{mt_profile_call_info}   (level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_startOfEvaluation}   (level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_debugger_breakpoint} (level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_displayTable}        (level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_plot_addText}        (level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_addRow}         (level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_dropRow}        (level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_renderRequest}  (level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_retrieveOptions}(level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_setOptions}     (level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_queryClosedB...}(level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_clear}          (level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_clearAnimation} (level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_clearAnimati...}(level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_addAnimation...}(level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_plot_postDisplay}    (level:-1; mClass:mc_plot;    systemErrorLevel:0),
{mt_guiEdit_done}        (level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_guiEditScriptsLoaded}(level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_image_postDisplay}   (level:-1; mClass:mc_image;   systemErrorLevel:0),
{mt_image_load}          (level:-1; mClass:mc_image;   systemErrorLevel:0),
{mt_image_replaceImage}  (level:-1; mClass:mc_image;   systemErrorLevel:0),
{mt_image_close}         (level:-1; mClass:mc_image;   systemErrorLevel:0),
{mt_image_obtainImage...}(level:-1; mClass:mc_image;   systemErrorLevel:0),
{mt_image_obtainDim...}  (level:-1; mClass:mc_image;   systemErrorLevel:0),
{mt_displayVariableTree} (level:-1; mClass:mc_gui;     systemErrorLevel:0),
{mt_displayCustomForm}   (level:-1; mClass:mc_gui;     systemErrorLevel:0)
{$endif});

  C_errorMessageTypes:array[1..4] of T_messageTypeSet=(
    [mt_el1_note,mt_el1_userNote],
    [mt_el2_warning,mt_el2_userWarning],
    [mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined],
    [mt_el4_systemError]);

TYPE
  P_storedMessage=^T_storedMessage;
  T_storedMessages=array of P_storedMessage;

  { T_storedMessage }

  T_storedMessage=object
    protected
      refCount:longint;
      location:T_searchTokenLocation;
      createdAt:double;
      kind:T_messageType;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation);
      DESTRUCTOR destroy; virtual;
      FUNCTION equals(CONST other:P_storedMessage):boolean; virtual;

      FUNCTION unreference:boolean;
      FUNCTION rereferenced:P_storedMessage;
      FUNCTION messageClass:T_messageClass;
      PROPERTY messageType:T_messageType read kind;
      FUNCTION getMessageTypeName:string;
      PROPERTY getLocation:T_searchTokenLocation read location;
      PROPERTY getRefCount:longint read refCount;
      FUNCTION isTextMessage:boolean; virtual;
      PROPERTY getTime:double read createdAt;
  end;

  P_messageFormatProvider=^T_messageFormatProvider;
  T_messageFormatProvider=object
    CONSTRUCTOR create;
    FUNCTION getClonedInstance:P_messageFormatProvider; virtual; abstract;
    DESTRUCTOR destroy; virtual;
    FUNCTION formatMessage(CONST message:P_storedMessage):T_arrayOfString; virtual;
  end;

  P_storedMessageWithText=^T_storedMessageWithText;
  T_storedMessageWithText=object(T_storedMessage)
    public
      txt:T_arrayOfString;
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
      FUNCTION equals(CONST other:P_storedMessage):boolean; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION isTextMessage:boolean; virtual;
  end;

  P_errorMessage=^T_errorMessage;
  T_errorMessage=object(T_storedMessageWithText)
    public
      stacktrace:array of record location:T_searchTokenLocation; callee:T_idString; parameters:string; end;
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
      DESTRUCTOR destroy; virtual;
  end;

  P_payloadMessage=^T_payloadMessage;
  T_payloadMessage=object(T_storedMessage)
    protected
      messageCs:TRTLCriticalSection;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create(CONST messageType_:T_messageType);
      FUNCTION equals(CONST other:P_storedMessage):boolean; virtual;
      DESTRUCTOR destroy; virtual;
  end;

OPERATOR :=(CONST x:T_ideMessageConfig):T_messageTypeSet;
PROCEDURE disposeMessage(VAR message:P_storedMessage);
PROCEDURE disposeMessage_(message:P_storedMessage);
FUNCTION messageTypeName(CONST m:T_messageType):string;
IMPLEMENTATION
OPERATOR :=(CONST x:T_ideMessageConfig):T_messageTypeSet;
  begin
    result:=[mt_clearConsole,mt_printline,mt_log,mt_printdirect,mt_endOfEvaluation{$ifdef fullVersion},mt_startOfEvaluation{$endif},mt_el4_systemError,mt_el3_noMatchingMain];
    if x.echo_input       then result+=[mt_echo_input      ];
    if x.echo_output      then result+=[mt_echo_output     ];
    if x.echo_declaration then result+=[mt_echo_declaration];
    if x.show_timing      then result+=[mt_timing_info];
    if x.show_all_userMessages then result+=[mt_log,mt_el1_userNote,mt_el2_userWarning,mt_el3_userDefined];
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

//------------------------------------------------------------------------------
CONSTRUCTOR T_messageFormatProvider.create; begin end;
DESTRUCTOR T_messageFormatProvider.destroy; begin end;
FUNCTION T_messageFormatProvider.formatMessage(CONST message: P_storedMessage): T_arrayOfString;
  begin
    if (message=nil) or not(message^.isTextMessage)
    then result:=C_EMPTY_STRING_ARRAY
    else result:=P_storedMessageWithText(message)^.txt;
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

CONSTRUCTOR T_storedMessageWithText.create(CONST messageType_: T_messageType;
  CONST loc: T_searchTokenLocation; CONST message: T_arrayOfString);
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

CONSTRUCTOR T_storedMessage.create(CONST messageType_: T_messageType;
  CONST loc: T_searchTokenLocation);
  begin
    location:=loc;
    kind:=messageType_;
    refCount:=1;
    createdAt:=now;
  end;

CONSTRUCTOR T_payloadMessage.create(CONST messageType_: T_messageType);
  begin
    inherited create(messageType_,C_nilSearchTokenLocation);
    initCriticalSection(messageCs);
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

FUNCTION T_storedMessageWithText.isTextMessage: boolean;
  begin
    result:=true;
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

VAR messageTypeNames:array[T_messageType] of string;

FUNCTION messageTypeName(CONST m:T_messageType):string;
  VAR mt:T_messageType;
  begin
    if messageTypeNames[mt_printline]='' then begin
      for mt in T_messageType do messageTypeNames[mt]:=copy(getEnumName(TypeInfo(mt),ord(mt)),4,1000);
    end;
    result:=messageTypeNames[m];
  end;

FUNCTION T_storedMessage.getMessageTypeName: string;
  begin
    result:= copy(getEnumName(TypeInfo(kind),ord(kind)),4,1000);
  end;

FUNCTION T_storedMessage.isTextMessage: boolean;
  begin
    result:=false;
  end;

INITIALIZATION
  messageTypeNames[mt_printline]:='';

end.
