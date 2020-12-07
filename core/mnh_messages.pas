UNIT mnh_messages;
INTERFACE
USES sysutils,
     typinfo,
     myGenerics,
     basicTypes,
     serializationUtil,
     myStringUtil;

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

  C_messageClassMeta:array[T_messageClass] of record htmlSpan:string; triggeredFlags:T_stateFlags; end=
    {mc_echo   }((htmlSpan:''     ; triggeredFlags:[]),
    {mc_print  } (htmlSpan:''     ; triggeredFlags:[]),
    {mc_timing } (htmlSpan:''     ; triggeredFlags:[]),
    {mc_note   } (htmlSpan:''     ; triggeredFlags:[]),
    {mc_warning} (htmlSpan:''     ; triggeredFlags:[]),
    {mc_error  } (htmlSpan:'error'; triggeredFlags:[FlagError]),
    {mc_fatal  } (htmlSpan:'error'; triggeredFlags:[FlagFatalError])
    {$ifdef fullVersion},
    {mc_plot   } (htmlSpan:''     ; triggeredFlags:[]),
    {mc_gui}     (htmlSpan:''     ; triggeredFlags:[])
    {$endif});

//TODO: Declare pattern doc as constant array[0..?] of string

TYPE
  T_messagePatternElementType=(et_literal,
                               et_lineBreak,  //%n%
                               et_tab,        //%t%
                               et_message,    //%message%
                               et_shortLoc,   //%shortLoc%
                               et_fullLoc,    //%location%
                               et_time,       //%time%
                               et_datetime);  //%datetime%

CONST
  C_messagePatternElementTypePlaceholder:array[et_lineBreak..high(T_messagePatternElementType)] of string=
   ('%n%'       ,
    '%t%'       ,
    '%message%' ,
    '%shortLoc%',
    '%location%',
    '%time%'    ,
    '%datetime%');

TYPE
  T_formattableMessageClass=(fm_NOT_FORMATTABLE,
                             fm_print,
                             fm_echoIn,
                             fm_echoOut,
                             fm_note,
                             fm_warn,
                             fm_error,
                             fm_stacktrace);

  T_messageType = (
    mt_clearConsole,
    mt_printline,
    mt_printdirect,
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
    mt_displayVariableTree,
    mt_displayCustomForm
    {$endif});

  T_messageTypeSet=set of T_messageType;

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

CONST
  C_textMessages:T_messageTypeSet=[mt_clearConsole..mt_el4_systemError,mt_timing_info];
  GUI_MARKER:Array[T_formattableMessageClass] of string=(
   {fm_NOT_FORMATTABLE}'',
   {fm_print,         }'',
   {fm_echoIn,        }ECHO_MARKER,
   {fm_echoOut,       }ECHO_MARKER,
   {fm_note,          }NOTE_MARKER,
   {fm_warn,          }WARNING_MARKER,
   {fm_error,         }ERROR_MARKER,
   {fm_stacktrace     }ERROR_MARKER);

  C_errorsAndWarnings:T_messageTypeSet=[mt_el2_warning,mt_el2_userWarning,mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined,mt_el4_systemError];
  C_messagesSuppressedOnQuietHalt:T_messageTypeSet=[mt_el3_evalError,
                                                    mt_el3_noMatchingMain,
                                                    mt_el3_userDefined];
  C_messageTypeMeta:array[T_messageType] of record
    level:shortint;
    mClass:T_messageClass;
    fClass:T_formattableMessageClass;
    systemErrorLevel:byte;
  end = (

{mt_clearConsole      }  (level:-2; mClass:mc_print;   fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_printline         }  (level:-2; mClass:mc_print;   fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_print             }  (level:-2; mClass:mc_print;   fClass:fm_print;           systemErrorLevel:0),
{mt_echo_input        }  (level:-1; mClass:mc_echo;    fClass:fm_echoIn;          systemErrorLevel:0),
{mt_echo_declaration  }  (level:-1; mClass:mc_echo;    fClass:fm_echoIn;          systemErrorLevel:0),
{mt_echo_continued    }  (level:-1; mClass:mc_echo;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_el1_note          }  (level: 1; mClass:mc_note;    fClass:fm_note;            systemErrorLevel:0),
{mt_el1_userNote      }  (level: 1; mClass:mc_note;    fClass:fm_note;            systemErrorLevel:0),
{mt_el2_warning       }  (level: 2; mClass:mc_warning; fClass:fm_warn;            systemErrorLevel:0),
{mt_el2_userWarning   }  (level: 2; mClass:mc_warning; fClass:fm_warn;            systemErrorLevel:0),
{mt_el3_evalError     }  (level: 3; mClass:mc_error;   fClass:fm_error;           systemErrorLevel:3),
{mt_el3_noMatchingMain}  (level: 3; mClass:mc_error;   fClass:fm_error;           systemErrorLevel:1),
{mt_el3_userDefined   }  (level: 3; mClass:mc_error;   fClass:fm_error;           systemErrorLevel:2),
{mt_el4_systemError   }  (level: 4; mClass:mc_fatal;   fClass:fm_error;           systemErrorLevel:5),
{mt_endOfEvaluation   }  (level:-1; mClass:mc_note;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_timing_info       }  (level:-1; mClass:mc_timing;  fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0)
{$ifdef fullVersion},
{mt_profile_call_info}   (level:-1; mClass:mc_gui;     fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_startOfEvaluation}   (level:-1; mClass:mc_gui;     fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_debugger_breakpoint} (level:-1; mClass:mc_gui;     fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_displayTable}        (level:-1; mClass:mc_gui;     fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_addText}        (level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_addRow}         (level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_dropRow}        (level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_renderRequest}  (level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_retrieveOptions}(level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_setOptions}     (level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_queryClosedB...}(level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_clear}          (level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_clearAnimation} (level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_clearAnimati...}(level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_addAnimation...}(level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_plot_postDisplay}    (level:-1; mClass:mc_plot;    fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_guiEdit_done}        (level:-1; mClass:mc_gui;     fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_guiEditScriptsLoaded}(level:-1; mClass:mc_gui;     fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_displayVariableTree} (level:-1; mClass:mc_gui;     fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0),
{mt_displayCustomForm}   (level:-1; mClass:mc_gui;     fClass:fm_NOT_FORMATTABLE; systemErrorLevel:0)
{$endif});

  C_errorMessageTypes:array[1..4] of T_messageTypeSet=(
    [mt_el1_note,mt_el1_userNote],
    [mt_el2_warning,mt_el2_userWarning],
    [mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_userDefined],
    [mt_el4_systemError]);

  ECHO_CONTINUED_PREFIX='...>';

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
      FUNCTION internalType:shortstring; virtual;
    public
      FUNCTION prefix:shortstring;
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation);
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
      FUNCTION isTextMessage:boolean; virtual;
  end;

  T_messagePatternElement=record
    elementType:T_messagePatternElementType;
    litValue:string;
  end;

  P_messageFormatProvider=^T_messageFormatProvider;

  T_messageFormatProvider=object
    CONSTRUCTOR create;
    FUNCTION getClonedInstance:P_messageFormatProvider; virtual; abstract;
    DESTRUCTOR destroy; virtual;
    FUNCTION formatMessage(CONST message:P_storedMessage):T_arrayOfString; virtual;
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
      fullLocations:boolean;
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
      CONSTRUCTOR create(CONST forDemos:boolean);
      FUNCTION getClonedInstance:P_messageFormatProvider; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION formatMessage(CONST message:P_storedMessage):T_arrayOfString; virtual;
  end;



  //P_messageFormatProvider=^T_messageFormatProvider;
  //T_messageFormatProvider=object
  //  private
  //    formatStrings:array[T_formattableMessageClass,boolean] of array of T_messagePatternElement;
  //  public
  //    CONSTRUCTOR create;
  //    CONSTRUCTOR clone(CONST original:P_messageFormatProvider);
  //    DESTRUCTOR destroy;
  //    PROCEDURE setDefaults(CONST forGui:boolean);
  //    FUNCTION formatMessage(CONST message:P_storedMessage):T_arrayOfString;
  //
  //    FUNCTION getMessageFormat(CONST messageType:T_formattableMessageClass; CONST firstLine:boolean):string;
  //    PROCEDURE setMessageFormat(CONST messageType:T_formattableMessageClass; CONST firstLine:boolean; CONST format:string);
  //
  //    //FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
  //    //PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  //end;



  P_storedMessageWithText=^T_storedMessageWithText;

  { T_storedMessageWithText }

  T_storedMessageWithText=object(T_storedMessage)
    protected
      txt:T_arrayOfString;
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
      FUNCTION equals(CONST other:P_storedMessage):boolean; virtual;
      FUNCTION messageText:T_arrayOfString; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION isTextMessage:boolean; virtual;
  end;

  P_errorMessage=^T_errorMessage;
  T_errorMessage=object(T_storedMessageWithText)
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      stacktrace:array of record location:T_searchTokenLocation; callee:T_idString; parameters:string; end;
      CONSTRUCTOR create(CONST messageType_:T_messageType; CONST loc:T_searchTokenLocation;  CONST message:T_arrayOfString);
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
    if x.echo_input       then result+=[mt_echo_input      ];
    if x.echo_output      then result+=[mt_echo_output     ];
    if x.echo_declaration then result+=[mt_echo_declaration];
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

//------------------------------------------------------------------------------
constructor T_messageFormatProvider.create; begin end;
destructor T_messageFormatProvider.destroy; begin end;
function T_messageFormatProvider.formatMessage(const message: P_storedMessage): T_arrayOfString;
  begin
    if (message=nil) or not(message^.isTextMessage)
    then result:=C_EMPTY_STRING_ARRAY
    else result:=P_storedMessageWithText(message)^.txt;
  end;
//------------------------------------------------------------------------------
constructor T_defaultConsoleFormatter.create; begin inherited; end;
destructor T_defaultConsoleFormatter.destroy; begin inherited; end;
function T_defaultConsoleFormatter.getClonedInstance: P_messageFormatProvider;
  begin
    new(P_defaultConsoleFormatter(result),create);
  end;

function T_defaultConsoleFormatter.formatMessage(const message: P_storedMessage): T_arrayOfString;
  begin
    //TODO: Reimplement this!
    assert(false);
    if (message=nil) or not(message^.isTextMessage)
    then result:=C_EMPTY_STRING_ARRAY
    else result:=P_storedMessageWithText(message)^.txt;
  end;
//------------------------------------------------------------------------------
constructor T_logFormatter.create;
  begin
    inherited;
    timeFormat:='hh:nn:ss.zzz';
    fullLocations:=false;
    maxLocationLength:=0;
  end;

destructor T_logFormatter.destroy; begin inherited; end;
function T_logFormatter.getClonedInstance: P_messageFormatProvider;
  begin
    new(P_logFormatter(result),create);
    P_logFormatter(result)^.timeFormat:=timeFormat;
    P_logFormatter(result)^.fullLocations:=fullLocations;
  end;

function T_logFormatter.formatMessage(const message: P_storedMessage): T_arrayOfString;
  begin
    //TODO: Reimplement this!
    assert(false);
    if (message=nil) or not(message^.isTextMessage)
    then result:=C_EMPTY_STRING_ARRAY
    else result:=P_storedMessageWithText(message)^.txt;
  end;
//------------------------------------------------------------------------------
constructor T_guiFormatter.create(const forDemos: boolean);
  begin
    inherited create;
    formatterForDemos:=forDemos;
  end;
destructor T_guiFormatter.destroy; begin inherited; end;

function T_guiFormatter.getClonedInstance: P_messageFormatProvider;
  begin
    new(P_guiFormatter(result),create(formatterForDemos));
  end;

function T_guiFormatter.formatMessage(const message: P_storedMessage): T_arrayOfString;
  begin
    //TODO: Reimplement this!
    assert(false);
    if (message=nil) or not(message^.isTextMessage)
    then result:=C_EMPTY_STRING_ARRAY
    else result:=P_storedMessageWithText(message)^.txt;
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
//  CONST msgTime: double; CONST loc: T_searchTokenLocation;
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

function T_storedMessage.internalType: shortstring; begin result:='T_storedMessage'; end;
function T_storedMessageWithText.internalType: shortstring; begin result:='T_storedMessageWithText'; end;
FUNCTION T_payloadMessage.internalType: shortstring; begin result:='T_payloadMessage'; end;
FUNCTION T_errorMessage.internalType: shortstring; begin result:='T_errorMessage'; end;

FUNCTION getPrefix(CONST messageType:T_messageType):shortstring;
  begin
    case messageType of
      mt_echo_input,
      mt_echo_declaration:result:=' in>';
      mt_echo_output     :result:='out>';
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

function T_storedMessage.prefix: shortstring;
  begin result:=getPrefix(kind); end;

constructor T_storedMessageWithText.create(const messageType_: T_messageType;
  const loc: T_searchTokenLocation; const message: T_arrayOfString);
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

constructor T_storedMessage.create(const messageType_: T_messageType;
  const loc: T_searchTokenLocation);
  begin
    location:=loc;
    kind:=messageType_;
    refCount:=1;
    createdAt:=now;
  end;

CONSTRUCTOR T_payloadMessage.create(CONST messageType_: T_messageType);
  begin
    inherited create(messageType_,C_nilTokenLocation);
    initCriticalSection(messageCs);
  end;

destructor T_storedMessage.destroy;
  begin
    if refCount<>0 then raise Exception.create('Disposing message with refCount='+intToStr(refCount));
  end;

destructor T_storedMessageWithText.destroy;
  VAR i:longint;
  begin
    inherited destroy;
    for i:=0 to length(txt)-1 do txt[i]:='';
    setLength(txt,0);
  end;

function T_storedMessageWithText.isTextMessage: boolean;
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

function T_storedMessage.equals(const other: P_storedMessage): boolean;
  begin
    result:=(other=@self) or
            (other^.kind=kind) and
            (other^.internalType=internalType) and
            (other^.location=location);
  end;

function T_storedMessageWithText.equals(const other: P_storedMessage): boolean;
  begin
    result:=inherited equals(other) and arrEquals(txt,P_storedMessageWithText(other)^.txt);
  end;

FUNCTION T_payloadMessage.equals(CONST other: P_storedMessage): boolean;
  begin
    result:=other=@self;
  end;

function T_storedMessage.unreference: boolean;
  begin
    result:=interlockedDecrement(refCount)<=0;
  end;

function T_storedMessage.rereferenced: P_storedMessage;
  begin
    interLockedIncrement(refCount);
    result:=@self;
  end;

function T_storedMessage.messageClass: T_messageClass;
  begin
    result:=C_messageTypeMeta[kind].mClass;
  end;

function T_storedMessage.messageText: T_arrayOfString;
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

function T_storedMessage.getMessageTypeName: string;
  begin
    result:= copy(getEnumName(TypeInfo(kind),ord(kind)),4,1000);
  end;

function T_storedMessage.isTextMessage: boolean;
  begin
    result:=false;
  end;

function T_storedMessageWithText.messageText: T_arrayOfString;
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
