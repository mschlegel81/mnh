UNIT tokenArray;
INTERFACE
USES myGenerics,myStringUtil,
     basicTypes,mnh_constants,
     fileWrappers,
     litVar,
     funcs,
     funcs_mnh,
     recyclers,
     {$ifdef fullVersion}
     mnh_html,
     mnh_doc,
     {$endif}
     tokens,
     mnh_messages,
     contexts,
     out_adapters;
CONST operatorName:array[tt_comparatorEq..tt_unaryOpMinus] of string=
      ('COMPARATOR_EQ',
       'COMPARATOR_NEQ',
       'COMPARATOR_LEQ',
       'COMPARATOR_GEQ',
       'COMPARATOR_LSS',
       'COMPARATOR_GRT',
       'COMPARATOR_LISTEQ',
       'OPERATOR_IN',
       'OPERATOR_NOT_IN',
       'OPERATOR_AND',
       'OPERATOR_OR',
       'OPERATOR_XOR',
       'OPERATOR_LAZYAND',
       'OPERATOR_LAZYOR',
       'OPERATOR_PLUS',
       'OPERATOR_MINUS',
       'OPERATOR_MULT',
       'OPERATOR_DIVREAL',
       'OPERATOR_DIVINT',
       'OPERATOR_MOD',
       'OPERATOR_POT',
       'OPERATOR_STRCONCAT',
       'OPERATOR_ORELSE',
       'OPERATOR_CONCAT',
       'OPERATOR_CONCATALT',
       'OPERATOR_NEGATE_LOGICAL',
       'OPERATOR_UNARY_PLUS',
       'OPERATOR_NEGATE_ARITHMETIC');

TYPE
  T_customOperatorArray=array[tt_comparatorEq..tt_unaryOpMinus] of P_abstractRule;
  {$ifdef fullVersion}
  P_callAndIdInfos=^T_callAndIdInfos;
  {$endif}

  P_abstractPackage=^T_abstractPackage;
  T_abstractPackage=object(T_objectWithPath)
    private
      codeProvider:P_codeProvider;
      readyForCodeState:T_hashInt;
    protected
      customOperatorRules:T_customOperatorArray;
      PROCEDURE logReady(CONST stateHashAtLoad:T_hashInt);
      PROCEDURE clearCustomOperators;
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider);
      DESTRUCTOR destroy; virtual;
      FUNCTION replaceCodeProvider(CONST newProvider:P_codeProvider):boolean;
      FUNCTION codeChanged:boolean;
      FUNCTION getId:T_idString; virtual;
      FUNCTION getPath:ansistring; virtual;
      PROPERTY getCodeProvider:P_codeProvider read codeProvider;
      PROPERTY getCodeState:T_hashInt read readyForCodeState;
      PROPERTY customOperatorRule:T_customOperatorArray read customOperatorRules;
      FUNCTION resolveId(VAR token:T_token; CONST adaptersOrNil:P_messages):boolean; virtual;
      FUNCTION resolveLocationForStackTrace(CONST location:T_tokenLocation):string; virtual;
      FUNCTION getTypeMap:T_typeMap; virtual;
      FUNCTION literalToString(CONST L:P_literal; {$WARN 5024 OFF}CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_recycler):string; virtual;
      {$ifdef fullVersion}
      FUNCTION getImport({$WARN 5024 OFF}CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION getExtended(CONST idOrPath:string):P_abstractPackage; virtual;
      {$endif}
      FUNCTION inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; CONST functionCallInfos:P_callAndIdInfos{$endif}):P_mapLiteral; virtual;
      FUNCTION isMain:boolean; virtual;
  end;

  P_extendedPackage=^T_extendedPackage;
  T_extendedPackage=object(T_abstractPackage)
    private
      extender:P_abstractPackage;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider; CONST extender_:P_abstractPackage);
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
      FUNCTION resolveId(VAR token:T_token; CONST adaptersOrNil:P_messages):boolean; virtual;
      FUNCTION inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; CONST functionCallInfos:P_callAndIdInfos{$endif}):P_mapLiteral; virtual;
  end;

  P_mnhSystemPseudoPackage=^T_mnhSystemPseudoPackage;

  { T_mnhSystemPseudoPackage }

  T_mnhSystemPseudoPackage=object(T_abstractPackage)
    CONSTRUCTOR create;
    FUNCTION getId:T_idString; virtual;
    FUNCTION getPath:ansistring; virtual;
  end;

  T_enhancedStatement=record
    comments,
    attributes:T_arrayOfString;
    token:T_tokenRange;
    assignmentToken:P_token;
  end;

  T_simpleTokenRange=record x,y,width:longint; end;
  T_simpleTokenRanges=array of T_simpleTokenRange;

  {$ifdef fullVersion}
  T_usageInfo=record
    targetLocation,
    referencedAt:T_searchTokenLocation;
  end;

  T_localIdInfo=record
                  name:string;
                  validFrom,validUntil:T_tokenLocation;
                  tokenType:T_tokenType;
                  used:boolean;
                end;

  T_relatedTokens=record
    count:byte;
    position: array[0..63] of T_simpleTokenRange;
  end;

  T_callAndIdInfos=object
    private
      usageInfoFill:longint;
      usedBuiltins :T_setOfPointer;
      usageInfos:array of T_usageInfo;
      relatedTokens:array of T_relatedTokens;
      localIdInfos: array of T_localIdInfo;
      blobLocations:array of record
        closer:char;
        startLine,startCol,endLine,endCol:longint;
      end;
      formatStrings:array of T_simpleTokenRange;
      FUNCTION getBlobCloserOrZero(CONST lineIndex:longint):char;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE add(CONST token:P_token);
      PROCEDURE addAll(CONST token:P_token);
      PROCEDURE cleanup;
      FUNCTION  calledBuiltinFunctions:T_builtinFunctionMetaDatas;
      FUNCTION  getBuiltinSideEffects:T_sideEffects;
      FUNCTION  whoReferencesLocation(CONST loc:T_searchTokenLocation):T_searchTokenLocations;
      FUNCTION  isLocationReferenced(CONST loc:T_searchTokenLocation):boolean;
      FUNCTION  isParameterReferenced(CONST loc:T_searchTokenLocation):boolean;
      FUNCTION  isPackageReferenced(CONST packagePath:string):boolean;
      FUNCTION  isEmpty:boolean;
      PROCEDURE includeUsages(CONST other:P_callAndIdInfos);
      PROCEDURE addTokenRelation(CONST token:P_token; CONST other:T_tokenLocation);
      FUNCTION  getIndexOfRelated(CONST CaretX,CaretY:longint; CONST exactMatchOnly:boolean=false):longint;
      FUNCTION  getRelated(CONST CaretX,CaretY:longint):T_relatedTokens;
      FUNCTION localTypeOf(CONST id:T_idString; CONST line,col:longint; OUT declaredAt:T_tokenLocation):T_tokenType;
      FUNCTION allLocalIdsAt(CONST line,col:longint):T_arrayOfString;
      PROCEDURE addLocalIdInfo(CONST id:T_idString; CONST validFrom,validUntil:T_tokenLocation; CONST typ:T_tokenType; CONST used:boolean);
      PROCEDURE markBlobLine(CONST startLine_,startCol_,endLine_,endCol_:longint; CONST closer_:char);
      FUNCTION getEndOfBlob(CONST lineIndex,colIndex:longint):longint;
      PROCEDURE addFormatStrings(CONST ranges:T_simpleTokenRanges);
      FUNCTION isFormatString(CONST lineIndex, tokenStart:longint; OUT tokenEnd:longint):boolean;
      PROCEDURE copyFrom(CONST original:P_callAndIdInfos);
  end;

  T_tokenInfo=record
    //position info
    fullLine    :ansistring;
    CaretX      :longint;
    startLoc,
    endLoc,
    location    :T_searchTokenLocation;
    referencedAt:T_searchTokenLocations;

    //basic info
    tokenText,
    shortInfo,
    linkToHelp:string;
    tokenType :T_tokenType;

    //rule info
    builtinRuleInfo,
    userDefRuleInfo:T_structuredRuleInfoList;
    exampleText    :T_arrayOfString;

    //Renaming related:
    canRename,
    mightBeUsedInOtherPackages:boolean;
    idWithoutIsPrefix         :string;
  end;

  T_enhancedToken=object
    private
      token:P_token;
      originalType:T_tokenType;
      references:T_tokenLocation;
      linksTo:(nothing,packageUse,packageInclude);
      endsAtColumn:longint;
      FUNCTION renameInLine(VAR line:string; CONST referencedLocation:T_searchTokenLocation; CONST oldName,newName:string):boolean;
    public
      CONSTRUCTOR create(CONST tok:P_token; CONST callAndIdInfos:P_callAndIdInfos; CONST package:P_abstractPackage);
      PROCEDURE cleanup(CONST recycler:P_recycler);
      DESTRUCTOR destroy;
      FUNCTION toInfo:T_tokenInfo;
  end;

  T_enhancedTokens=object
    private
      dat:array of T_enhancedToken;
      PROCEDURE add(CONST tok:P_token; CONST callAndIdInfos:P_callAndIdInfos; CONST package:P_abstractPackage);
      PROCEDURE addLineEnder(CONST lineLength:longint);
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION getTokenAtIndex(CONST rowIndex:longint):T_enhancedToken;
      FUNCTION renameInLine(VAR line:string; CONST referencedLocation:T_searchTokenLocation; CONST oldName,newName:string):boolean;
  end;
  {$endif}

  T_lexingStyle=(ls_onlyInterpretable,ls_retainAll,ls_retainComments);
  T_lexingUsecase=(lxu_interpretation, lxu_assistance, lxu_assistanceImport);

  { T_abstractLexer }

  T_abstractLexer=object
    private
      nextStatement:T_enhancedStatement;
      tokenQueue:specialize G_queue<P_token>;
      PROCEDURE resetTemp;
    protected
      blob:record
        closer:char;
        lines:T_arrayOfString;
        start:T_tokenLocation;
      end;
      associatedPackage:P_abstractPackage;
      {$ifdef fullVersion}
      usecase:T_lexingUsecase;
      callAndIdInfos:P_callAndIdInfos;
      {$endif}
      FUNCTION getToken(CONST line: ansistring; VAR inputLocation: T_tokenLocation; CONST messages:P_messages; CONST recycler:P_recycler): P_token;
      FUNCTION fetchNext(                                                           CONST messages:P_messages; CONST recycler:P_recycler):boolean;
      FUNCTION fetch(CONST messages:P_messages; CONST recycler:P_recycler):P_token; virtual; abstract;
    public
      CONSTRUCTOR create(CONST package:P_abstractPackage {$ifdef fullVersion};CONST usecase_:T_lexingUsecase; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;
      FUNCTION getNextStatement(CONST context:P_context; CONST recycler:P_recycler):T_enhancedStatement;
  end;

  { T_singleStringLexer }

  T_singleStringLexer=object(T_abstractLexer)
    protected
      text:string;
      inputLocation:T_tokenLocation;
      columnOffset:longint;
      FUNCTION fetch(CONST messages:P_messages; CONST recycler:P_recycler):P_token; virtual;
    public
      CONSTRUCTOR create(CONST inputString:string; CONST parseLocation:T_tokenLocation;
                         CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;
      {$ifdef fullVersion}
      FUNCTION getEnhancedTokens(CONST idInfos:P_callAndIdInfos):T_enhancedTokens;
      {$endif}
  end;

  T_locatedString=record txt:string; loc:T_tokenLocation; end;
  T_locatedStrings=array of T_locatedString;

  { T_locatedStringLexer }

  T_locatedStringLexer = object(T_abstractLexer)
    protected
      parts:T_locatedStrings;
      partIdx:longint;
      inputLocation:T_tokenLocation;
      columnOffset:longint;
      FUNCTION fetch(CONST messages:P_messages; CONST recycler:P_recycler):P_token; virtual;
    public
      CONSTRUCTOR create(CONST strings:T_locatedStrings; CONST package:P_abstractPackage {$ifdef fullVersion}; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;
  end;

  { T_variableLexer }

  T_variableLexer=object(T_abstractLexer)
    private
      parseLocation:T_tokenLocation;
      inputLocation:T_tokenLocation;
      data:T_arrayOfLiteral;
      dataIdx:longint;
      textToParse:string;
    protected
      FUNCTION fetch(CONST messages:P_messages; CONST recycler:P_recycler):P_token; virtual;
    public
      CONSTRUCTOR create(CONST input:T_arrayOfLiteral; CONST parseLocation_:T_tokenLocation;
                         CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;
  end;

  { T_linesLexer }

  T_linesLexer=object(T_abstractLexer)
    private
      inputLocation:T_tokenLocation;
      input:T_arrayOfString;
      inputIndex:longint;
    protected
      FUNCTION fetch(CONST messages:P_messages; CONST recycler:P_recycler):P_token; virtual;
    public
      CONSTRUCTOR create(CONST input_:T_arrayOfString; CONST parseLocation_:T_tokenLocation;
                         CONST package:P_abstractPackage {$ifdef fullVersion};CONST usecase_:T_lexingUsecase; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      CONSTRUCTOR createForExtendedPackage(CONST importWrapper:P_extendedPackage;
                         CONST package:P_abstractPackage {$ifdef fullVersion};CONST usecase_:T_lexingUsecase; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      CONSTRUCTOR createForPackageParsing(CONST package:P_abstractPackage {$ifdef fullVersion}; CONST usecase_:T_lexingUsecase; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;
  end;

{$ifdef fullVersion}
OPERATOR =(CONST A,B:T_relatedTokens):boolean;
{$endif}
FUNCTION isOperatorName(CONST id:T_idString):boolean;
CONST
  C_assistanceUseCases:set of T_lexingUsecase=[lxu_assistance, lxu_assistanceImport];
VAR
    {$ifdef fullVersion}
    getFormatTokens: FUNCTION (CONST formatString:ansistring; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler; OUT formatStringRanges:T_simpleTokenRanges):P_token;
    {$endif}
    BLANK_ABSTRACT_PACKAGE:T_abstractPackage;
    MNH_PSEUDO_PACKAGE:T_mnhSystemPseudoPackage;
    BUILTIN_WRITE_DATA_STORES,
    BUILTIN_WRITE_ALL_DATA_STORES,
    BUILTIN_INSPECT:P_intFuncCallback;
IMPLEMENTATION
USES sysutils,{$ifdef fullVersion}strutils,messageFormatting,{$endif}math,subrules,profiling,typinfo,patterns,funcs_ipc,rules;

TYPE
T_scopeType=(sc_block,        // begin ... end
             sc_each,         // .each(id,... ) | .pEach(id,...) | for id in ... do ... ; | for id in ... do ... aggregator
             sc_bracketOnly,  // (...) | [...] | {...}
             sc_inlineIfThen, // ? ... :
             sc_inlineIfElse, // ? : ...
             sc_if,           // if ... then
             sc_then,         // if then ... else | if then ... ;
             sc_else,         // if then else ...
             sc_lambdaBody,   // ()->... ;
             sc_repeat,       // repeat ... until
             sc_until,        // repeat until ... ;
             sc_while,        // while ... do
             sc_whileDo       // while do ... ;
             );

T_addIdResult=(air_ok,air_reintroduce,air_notInBlock);

T_idStack=object
  scope:array of record
    scopeType:T_scopeType;
    scopeStartToken:P_token;
    ids:array of record name:T_idString; used:boolean; location:T_tokenLocation; idType:T_tokenType; end;
    delayedErrorMessages:T_storedMessages;
  end;
  lexingUsecase:T_lexingUsecase;
  doSuppressAllUnusedWarnings:boolean;
  suppressUnusedWarningInLines:T_arrayOfLongint;
  {$ifdef fullVersion}
  localIdInfos:P_callAndIdInfos;
  {$endif}

  CONSTRUCTOR create({$ifdef fullVersion}CONST info:P_callAndIdInfos{$endif});
  DESTRUCTOR destroy;
  PROCEDURE clear;
  PROCEDURE suppressUnusedWarningInLine(CONST lineIndex:longint);
  PROCEDURE suppressAllUnusedWarnings;

  PROCEDURE scopePush(CONST openToken:P_token; CONST scopeType: T_scopeType);
  PROCEDURE scopePop(CONST context:P_context; CONST location:T_tokenLocation; CONST recycler:P_recycler; VAR workingIn:T_enhancedStatement; CONST closeToken:P_token; CONST forcePop:boolean=false);
  {$ifdef fullVersion}
  PROCEDURE popRemaining(CONST lastLocation: T_tokenLocation);
  {$endif}
  FUNCTION oneAboveBottom:boolean;
  FUNCTION scopeBottom:boolean;
  FUNCTION hasBeginItem:boolean;
  FUNCTION addId(CONST id:T_idString; CONST location:T_tokenLocation; CONST idType:T_tokenType):T_addIdResult;
  FUNCTION hasId(CONST id:T_idString; OUT idType:T_tokenType; OUT idLoc:T_tokenLocation):boolean;
end;

FUNCTION T_linesLexer.fetch(CONST messages: P_messages; CONST recycler:P_recycler): P_token;
  begin
    result:=nil;
    while (result=nil) and (inputIndex<length(input)) do begin
      result:=getToken(input[inputIndex],inputLocation,messages,recycler);
      if (result=nil) then begin
        inc(inputIndex);
        inc(inputLocation.line);
        inputLocation.column:=1;
      end;
    end;
  end;

CONSTRUCTOR T_linesLexer.create(CONST input_: T_arrayOfString; CONST parseLocation_: T_tokenLocation;
                                CONST package:P_abstractPackage {$ifdef fullVersion}; CONST usecase_:T_lexingUsecase; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    inherited create(package{$ifdef fullVersion},usecase_,callAndIdInfos_{$endif});
    input:=input_;
    inputIndex:=0;
    inputLocation:=parseLocation_;
    inputLocation.column:=1;
  end;

CONSTRUCTOR T_linesLexer.createForExtendedPackage(CONST importWrapper: P_extendedPackage;
                                                  CONST package:P_abstractPackage {$ifdef fullVersion}; CONST usecase_:T_lexingUsecase; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    create(importWrapper^.getCodeProvider^.getLines,packageTokenLocation(importWrapper),package{$ifdef fullVersion},usecase_,callAndIdInfos_{$endif});
  end;

CONSTRUCTOR T_linesLexer.createForPackageParsing(CONST package:P_abstractPackage {$ifdef fullVersion}; CONST usecase_:T_lexingUsecase; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    create(package^.getCodeProvider^.getLines,packageTokenLocation(package),package{$ifdef fullVersion},usecase_,callAndIdInfos_{$endif});
  end;

DESTRUCTOR T_linesLexer.destroy;
  begin
    inherited;
    setLength(input,0);
  end;

{ T_variableLexer }

FUNCTION T_variableLexer.fetch(CONST messages: P_messages; CONST recycler:P_recycler): P_token;
  begin
    result:=nil;
    while (result=nil) do begin
      result:=getToken(textToParse,inputLocation,messages,recycler);
      if result=nil then begin
        inc(dataIdx);
        if dataIdx>=length(data) then exit(nil);
        if data[dataIdx]^.literalType=lt_string then begin
          textToParse:=P_stringLiteral(data[dataIdx])^.value;
          inputLocation.column:=1;
        end else begin
          result:=recycler^.newToken(parseLocation,'',tt_literal,data[dataIdx]^.rereferenced);
        end;
      end;
    end;
    if result<>nil then result^.location:=parseLocation;
  end;

CONSTRUCTOR T_variableLexer.create(CONST input: T_arrayOfLiteral; CONST parseLocation_: T_tokenLocation; CONST package: P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    inherited create(package{$ifdef fullVersion},lxu_interpretation,callAndIdInfos_{$endif});
    data:=input;
    dataIdx:=-1;
    parseLocation:=parseLocation_;
    inputLocation:=parseLocation_;
    textToParse:='';
  end;

DESTRUCTOR T_variableLexer.destroy;
  VAR l:P_literal;
  begin
    inherited;
    for l in data do l^.unreference;
  end;

{ T_singleStringLexer }

FUNCTION T_singleStringLexer.fetch(CONST messages: P_messages; CONST recycler:P_recycler): P_token;
  begin
    result:=nil;
    while (result=nil) and (inputLocation.column<=length(text)) do begin
      result:=getToken(text,inputLocation,messages,recycler);
    end;
    if result<>nil then inc(result^.location.column,columnOffset);
  end;

CONSTRUCTOR T_singleStringLexer.create(CONST inputString: string; CONST parseLocation: T_tokenLocation;
                                       CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    inherited create(package{$ifdef fullVersion},lxu_interpretation,callAndIdInfos_{$endif});
    text:=inputString;
    inputLocation:=parseLocation;
    inputLocation.column:=1;
    columnOffset:=parseLocation.column-inputLocation.column;
  end;

DESTRUCTOR T_singleStringLexer.destroy;
  begin
    inherited;
    text:='';
  end;

{ T_locatedStringLexer }

FUNCTION T_locatedStringLexer.fetch(CONST messages: P_messages; CONST recycler: P_recycler): P_token;
  begin
    result:=nil;
    while (result=nil) and (partIdx<length(parts)) do begin
      result:=getToken(parts[partIdx].txt,inputLocation,messages,recycler);
      if (result=nil) then begin
        inc(partIdx);
        if length(parts)>partIdx then begin
          columnOffset:=parts[partIdx].loc.column+1;
          inputLocation:=parts[partIdx].loc;
        end;
        inputLocation.column:=1;
      end;
    end;
    if result<>nil then inc(result^.location.column,columnOffset);
  end;

CONSTRUCTOR T_locatedStringLexer.create(CONST strings: T_locatedStrings; CONST package: P_abstractPackage {$ifdef fullVersion}; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  VAR k:longint;
  begin
    inherited create(package{$ifdef fullVersion},lxu_interpretation,callAndIdInfos_{$endif});
    partIdx:=0;
    setLength(parts,length(strings));
    for k:=0 to length(parts)-1 do parts[k]:=strings[k];
    if length(parts)>partIdx then begin
      columnOffset:=parts[partIdx].loc.column+1;
      inputLocation:=parts[partIdx].loc;
    end;
    inputLocation.column:=1;
  end;

DESTRUCTOR T_locatedStringLexer.destroy;
  begin
  end;

{$ifdef fullVersion}
FUNCTION T_singleStringLexer.getEnhancedTokens(CONST idInfos: P_callAndIdInfos): T_enhancedTokens;
  VAR adapters:T_messagesDummy;
      recycler:P_recycler;
      tokenToProcess:P_token;
  FUNCTION informedFetchNext:boolean;
    VAR
      fStringEnd: longint;
      fStringToken:P_token;
    begin
      if idInfos^.isFormatString(inputLocation.line-1,inputLocation.column-1,fStringEnd)
      then begin
        fStringToken:=recycler^.newToken(inputLocation,'',tt_formatString,
          recycler^.newStringLiteral(copy(text,inputLocation.column,fStringEnd-inputLocation.column+1)));
        tokenQueue.append(fStringToken);
        inputLocation.column:=fStringEnd+1;
        result:=true;
      end else result:=fetchNext(@adapters,recycler);
    end;

  begin
    recycler:=newRecycler;
    blob.closer:=idInfos^.getBlobCloserOrZero(inputLocation.line);

    adapters.createDummy;
    while informedFetchNext do begin end;
    dec(inputLocation.line);
    inputLocation.column:=length(text);
    adapters.destroy;
    result.create;
    while tokenQueue.hasNext do begin
      tokenToProcess:=tokenQueue.next;
      associatedPackage^.resolveId(tokenToProcess^,nil);
      result.add(tokenToProcess,idInfos,associatedPackage);
    end;
    result.addLineEnder(inputLocation.column);
    resetTemp;
    freeRecycler(recycler);
  end;

FUNCTION tokenizeAllReturningRawTokens(CONST inputString:ansistring):T_rawTokenArray;
  VAR lexer:T_singleStringLexer;
      location:T_tokenLocation;
      adapters:T_messagesDummy;
      recycler:P_recycler;
      t:P_token;
  begin
    recycler:=newRecycler;
    location.package:=@BLANK_ABSTRACT_PACKAGE;
    location.line:=0;
    location.column:=1;
    lexer.create(inputString,location,@BLANK_ABSTRACT_PACKAGE);
    adapters.createDummy;
    repeat until not(lexer.fetchNext(@adapters,recycler));
    adapters.destroy;
    setLength(result,0);
    while lexer.tokenQueue.hasNext do begin
      t:=lexer.tokenQueue.next;
      setLength(result,length(result)+1);
      BLANK_ABSTRACT_PACKAGE.resolveId(t^,nil);
      result[length(result)-1]:=t^.getRawToken;
      recycler^.disposeToken(t);
    end;
    lexer.resetTemp;
    lexer.destroy;
    freeRecycler(recycler);
  end;

FUNCTION T_abstractPackage.getImport(CONST idOrPath:string):P_abstractPackage; begin result:=nil; end;
FUNCTION T_abstractPackage.getExtended(CONST idOrPath:string):P_abstractPackage; begin result:=nil; end;
{$endif}

{ T_abstractLexer }

CONSTRUCTOR T_abstractLexer.create(CONST package: P_abstractPackage {$ifdef fullVersion};CONST usecase_:T_lexingUsecase; CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    associatedPackage:=package;

    {$ifdef fullVersion}
    callAndIdInfos:=callAndIdInfos_;
    usecase:=usecase_;
    {$endif}

    setLength(blob.lines,0);
    blob.closer:=#0;
    tokenQueue.create;
    resetTemp;
  end;

DESTRUCTOR T_abstractLexer.destroy;
  VAR t:P_token;
  begin
    while tokenQueue.hasNext do begin
      t:=tokenQueue.next;
      dispose(t,destroy);
    end;
    tokenQueue.destroy;
    while nextStatement.token.first<>nil do begin
      t:=nextStatement.token.first;
      nextStatement.token.first:=nextStatement.token.first^.next;
      dispose(t,destroy);
    end;
  end;

{$ifdef fullVersion}
OPERATOR=(CONST A, B: T_relatedTokens): boolean;
  VAR i:longint;
  begin
    if A.count<>B.count then exit(false);
    for i:=0 to A.count-1 do
      if (A.position[i].y    <>B.position[i].y) or
         (A.position[i].x    <>B.position[i].x) or
         (A.position[i].width<>B.position[i].width) then exit(false);
    result:=true;
  end;
{$endif}

FUNCTION T_abstractLexer.getNextStatement(CONST context:P_context; CONST recycler:P_recycler): T_enhancedStatement;
  VAR localIdStack:T_idStack;
      earlierSuppressedUnusedAttribute:boolean=false;
      attributeSectionBeforeBody:boolean=true;

  FUNCTION hasSuppressedUnusedAttribute:boolean;
    VAR s:string;
    begin
      if earlierSuppressedUnusedAttribute then exit(true);
      for s in nextStatement.attributes do earlierSuppressedUnusedAttribute:=earlierSuppressedUnusedAttribute or startsWith(s,SUPPRESS_UNUSED_WARNING_ATTRIBUTE);
      result:=earlierSuppressedUnusedAttribute;
    end;

  {$ifdef fullVersion}
  PROCEDURE validateFormatString(CONST tok:P_token);
    VAR ft:P_token;
      idType: T_tokenType;
      idLoc: T_tokenLocation;
      formatStringRanges: T_simpleTokenRanges;
      addHighlightingInfo:boolean;
    begin
      ft:=getFormatTokens(P_stringLiteral(tok^.data)^.getEscapedOriginal,tok^.location,context,recycler,formatStringRanges);
      addHighlightingInfo:=(usecase=lxu_assistance) and (localIdStack.localIdInfos<>nil);
      if addHighlightingInfo then localIdStack.localIdInfos^.addFormatStrings(formatStringRanges);
      while (ft<>nil) do begin
        if localIdStack.hasId(ft^.txt,idType,idLoc)
        then begin
          ft^.tokType:=idType;
          if addHighlightingInfo then callAndIdInfos^.addTokenRelation(ft,idLoc);
        end else begin
          associatedPackage^.resolveId(ft^,nil);
          if ft^.tokType in [tt_userRule,tt_globalVariable] then begin
            idLoc:=P_abstractRule(ft^.data)^.getLocation;
            if addHighlightingInfo and (idLoc.package=P_objectWithPath(associatedPackage)) then callAndIdInfos^.addTokenRelation(ft,idLoc);
          end;
        end;
        if ft^.tokType=tt_identifier
        then context^.raiseError('Unresolvable identifier in format string: "'+ft^.txt+'"',ft^.location)
        else if localIdStack.localIdInfos<>nil then localIdStack.localIdInfos^.add(ft);
        ft:=recycler^.disposeToken(ft);
      end;
    end;
  {$endif}

  FUNCTION appendTokenToResultYieldsClosingSemicolon(tok:P_token):boolean;
    VAR
      air: T_addIdResult;
      idType: T_tokenType;
      idLoc: T_tokenLocation;
      beforeLast:P_token=nil;
      id:T_idString;
    FUNCTION ensureBeforeLast:P_token;
      begin
        beforeLast:=nextStatement.token.first;
        if (beforeLast<>nil) and (beforeLast^.next<>nil) then begin
          while beforeLast^.next^.next<>nil do beforeLast:=beforeLast^.next;
        end else beforeLast:=nil;
        result:=beforeLast;
      end;

    begin
      result:=false;
      case tok^.tokType of
        tt_attributeComment: begin
          if (tok^.txt<>'') and attributeSectionBeforeBody then myGenerics.append(nextStatement.attributes,tok^.txt);
          if startsWith(tok^.txt,SUPPRESS_UNUSED_WARNING_ATTRIBUTE) then begin
            localIdStack.suppressUnusedWarningInLine(tok^.location.line+1);
          end;
          recycler^.disposeToken(tok);
          exit(false);
        end;
        tt_docComment: begin
          myGenerics.append(nextStatement.comments ,tok^.txt);
          recycler^.disposeToken(tok);
          exit(false);
        end;
        tt_blank, tt_EOL: begin
          recycler^.disposeToken(tok);
          exit(false);
        end;
      end;
      attributeSectionBeforeBody:=false;

      if (nextStatement.token.last<>nil) then begin
        if (nextStatement.token.last^.tokType in [tt_beginExpression,tt_each,tt_parallelEach,tt_agg,tt_list_constructor,tt_expBraceOpen,tt_iifCheck]) and
           (tok                     ^.tokType in [tt_endExpression                                                     ,tt_expBraceClose,tt_iifElse])
        then context^.messages^.raiseSimpleError('Empty '+nextStatement.token.last^.singleTokenToString+'-'+tok^.singleTokenToString+' block',tok^.location);
        if (nextStatement.token.last^.tokType in [tt_separatorCnt,tt_separatorComma]) and (tok^.tokType in C_closingBrackets)
        then context^.messages^.raiseSimpleError('Missing element in '+nextStatement.token.last^.singleTokenToString+'-separated list',tok^.location);
        if (nextStatement.token.last^.tokType<>tt_semicolon) and (tok^.tokType in [tt_endBlock,tt_endRule])
        then context^.messages^.raiseSimpleError('Missing ";" before "end"',tok^.location);
        if (nextStatement.token.last^.tokType=tt_return) and (tok^.tokType=tt_semicolon)
        then context^.messages^.raiseSimpleError('Empty return statement',nextStatement.token.last^.location);
      end;

      case tok^.tokType of
        tt_beginBlock:                                  localIdStack.scopePush(tok,sc_block);
        tt_braceOpen,tt_expBraceOpen,tt_listBraceOpen:  localIdStack.scopePush(tok,sc_bracketOnly);
        tt_iifCheck:                                    localIdStack.scopePush(tok,sc_inlineIfThen);
        tt_while:                                       localIdStack.scopePush(tok,sc_while);
        tt_repeat:                                      localIdStack.scopePush(tok,sc_repeat);
        tt_if:                                          localIdStack.scopePush(tok,sc_if);
        tt_each,tt_parallelEach,tt_for:begin
          localIdStack.scopePush(tok,sc_each);
          localIdStack.addId(EACH_INDEX_IDENTIFIER,tok^.location,tt_eachIndex);
          for id in split(tok^.txt,',') do localIdStack.addId(id,tok^.location,tt_eachParameter);
        end;
        tt_endBlock,tt_separatorComma,
        tt_braceClose,tt_expBraceClose,tt_listBraceClose,tt_iifElse,tt_do,tt_until,tt_then,tt_else,
        tt_endOfPatternDeclare,tt_endOfPatternAssign,tt_aggregatorConstructor:
          localIdStack.scopePop(context,tok^.location,recycler,nextStatement,tok);
        tt_semicolon:
          begin
            localIdStack.scopePop(context,tok^.location,recycler,nextStatement,tok);
            if localIdStack.scopeBottom then exit(true);
          end;
        tt_assign: begin
          if (nextStatement.token.last<>nil) and (nextStatement.token.last^.tokType=tt_identifier) and localIdStack.hasBeginItem
          then begin
            nextStatement.token.last^.tokType:=tt_assignNewBlockLocal;
            localIdStack.addId(nextStatement.token.last^.txt,nextStatement.token.last^.location,tt_blockLocalVariable);
            recycler^.disposeToken(tok);
            exit(false);
          end;
          if (nextStatement.token.last<>nil) and (nextStatement.token.last^.tokType=tt_blockLocalVariable)
          then begin
            if (ensureBeforeLast<>nil) and (beforeLast^.tokType=tt_modifier) and (beforeLast^.getModifier=modifier_local)
            then begin
              beforeLast^.tokType:=tt_assignNewBlockLocal;
              beforeLast^.txt    :=nextStatement.token.last^.txt;
              recycler^.disposeToken(beforeLast^.next);
              recycler^.disposeToken(tok);
              nextStatement.token.last:=beforeLast;
              exit(false);
            end else begin
              nextStatement.token.last^.tokType:=tt_assignExistingBlockLocal;
              recycler^.disposeToken(tok);
              exit(false);
            end;
          end;
          if (nextStatement.token.last<>nil) and (nextStatement.token.last^.tokType=tt_globalVariable)
          then begin
            nextStatement.token.last^.tokType:=tt_mutate;
            recycler^.disposeToken(tok);
            exit(false);
          end;
          if (nextStatement.token.last=nil)
          then context^.raiseError('Invalid assignment',tok^.location)
          else if not (nextStatement.token.last^.tokType in [tt_listBraceClose,tt_identifier])
               and not((nextStatement.token.last^.tokType in [tt_userRule,tt_intrinsicRule]) and localIdStack.scopeBottom)
          then context^.raiseError('Invalid assignment; Left hand side: '+tokenTypeName(nextStatement.token.last^.tokType),tok^.location)
          else if localIdStack.scopeBottom then nextStatement.assignmentToken:=tok;
        end;
        tt_declare: if localIdStack.scopeBottom then nextStatement.assignmentToken:=tok;
        tt_mut_nested_assign..tt_mut_nestedDrop: begin
          if (nextStatement.token.last<>nil) and (nextStatement.token.last^.tokType=tt_blockLocalVariable) then begin
            nextStatement.token.last^.tokType:=tok^.tokType;
            nextStatement.token.last^.data:=nil;
            recycler^.disposeToken(tok);
            exit(false);
          end;
          if (nextStatement.token.last<>nil) and (nextStatement.token.last^.tokType=tt_globalVariable) then begin
            nextStatement.token.last^.tokType:=tok^.tokType;
            recycler^.disposeToken(tok);
            exit(false);
          end;
          if (nextStatement.token.last=nil)
          then context^.raiseError('Invalid assignment',tok^.location)
          else if not (nextStatement.token.last^.tokType in [tt_listBraceClose,tt_blockLocalVariable,tt_globalVariable])
               and not((nextStatement.token.last^.tokType in [tt_userRule,tt_intrinsicRule]) and localIdStack.scopeBottom)
          then context^.raiseError('Invalid assignment; Left hand side: '+tokenTypeName(nextStatement.token.last^.tokType),tok^.location);
        end;
        tt_identifier,tt_userRule,tt_intrinsicRule,tt_globalVariable,tt_customType:begin
          if (nextStatement.token.last<>nil) and (nextStatement.token.last^.tokType=tt_modifier) and (nextStatement.token.last^.getModifier=modifier_local) then begin
            {$ifdef fullVersion}
            if (tok^.tokType=tt_identifier) and not(localIdStack.hasId(tok^.txt,idType,idLoc)) and (usecase in C_assistanceUseCases)
            then context^.messages^.postTextMessage(mt_el1_note,nextStatement.token.last^.location,'Obsolete local modifier.');
            {$endif}
            tok^.tokType:=tt_blockLocalVariable;
            air:=localIdStack.addId(tok^.txt,tok^.location,tt_blockLocalVariable);
            if context^.messages<>nil then case air of
              air_reintroduce: context^.messages^.raiseSimpleError('Invalid re-introduction of local variable "'+tok^.txt+'"',tok^.location);
              air_notInBlock : context^.messages^.raiseSimpleError('You can only declare local variables in begin-end-blocks',tok^.location);
            end;
          end else if localIdStack.hasId(tok^.txt,idType,idLoc) then begin
            tok^.tokType:=idType;
            {$ifdef fullVersion}
            if (callAndIdInfos<>nil) and (usecase=lxu_assistance) and (idLoc.package=P_objectWithPath(associatedPackage)) and (tok^.location.package=P_objectWithPath(associatedPackage)) then callAndIdInfos^.addTokenRelation(tok,idLoc);
            {$endif}
            if idType=tt_eachIndex then tok^.location:=idLoc;
          end else begin
            associatedPackage^.resolveId(tok^,nil);
            {$ifdef fullVersion}
            if (callAndIdInfos<>nil) and (tok^.location.package=P_objectWithPath(associatedPackage)) and (tok^.tokType in [tt_userRule,tt_globalVariable]) then begin
              idLoc:=P_abstractRule(tok^.data)^.getLocation;
              if (idLoc.package=P_objectWithPath(associatedPackage)) and (usecase=lxu_assistance) then callAndIdInfos^.addTokenRelation(tok,idLoc);
            end;
            {$endif}
          end;
        end;
        {$ifdef fullVersion}
        tt_formatString: validateFormatString(tok);
        tt_literal:
          begin
            if (P_literal(tok^.data)^.literalType=lt_string) and (nextStatement.token.last<>nil) and (nextStatement.token.last^.tokType=tt_braceOpen) and (ensureBeforeLast<>nil) and (beforeLast^.tokType=tt_intrinsicRule) and ((beforeLast^.txt='format') or (beforeLast^.txt='printf'))
            then validateFormatString(tok);
          end;
        {$endif}
      end;

      if tok^.tokType in [tt_blank,tt_EOL]
      then recycler^.disposeToken(tok)
      else begin
        if nextStatement.token.first=nil
        then nextStatement.token.first     :=tok
        else nextStatement.token.last^.next:=tok;
        nextStatement.token.last :=tok;
      end;
    end;

  VAR nextToProcess:P_token=nil;
      lastLocation:T_tokenLocation;
  begin
    localIdStack.create({$ifdef fullVersion}callAndIdInfos{$endif});
    {$ifdef fullVersion}
    localIdStack.lexingUsecase:=usecase;
    {$endif}
    while tokenQueue.hasNext or (fetchNext(context^.messages,recycler) and tokenQueue.hasNext) do begin
      nextToProcess:=tokenQueue.next;
      lastLocation:=nextToProcess^.location;
      if nextToProcess^.tokType=tt_semicolon then begin
        if localIdStack.scopeBottom or appendTokenToResultYieldsClosingSemicolon(nextToProcess) then begin
          if nextStatement.token.first=nil then context^.messages^.raiseSimpleError('Empty statement!',nextToProcess^.location);
          recycler^.disposeToken(nextToProcess);
          result:=nextStatement;
          {$ifdef fullVersion}
          if (callAndIdInfos<>nil) and (usecase in C_assistanceUseCases) then begin
            if result.assignmentToken<>nil
            then callAndIdInfos^.addAll(result.assignmentToken)
            else callAndIdInfos^.addAll(result.token.first);
          end;
          {$endif}

          if not(context^.continueEvaluation) then recycler^.cascadeDisposeToken(result.token.first);
          if result.token.first=nil
          then result.token.last:=nil
          else result.token.last:=result.token.first^.last;
          resetTemp;
          localIdStack.destroy;
          exit;
        end;
      end else appendTokenToResultYieldsClosingSemicolon(nextToProcess);
    end;
    //Add trailing semicolon(s) if necessary;
    if hasSuppressedUnusedAttribute then localIdStack.suppressAllUnusedWarnings;
    while not(localIdStack.scopeBottom)
          do localIdStack.scopePop(context,lastLocation,recycler,nextStatement,nil,true);
    result:=nextStatement;
    {$ifdef fullVersion}
    if (callAndIdInfos<>nil) and (usecase in C_assistanceUseCases) then begin
      if result.assignmentToken<>nil
      then callAndIdInfos^.addAll(result.assignmentToken)
      else callAndIdInfos^.addAll(result.token.first);
    end;
    {$endif}
    if result.token.first=nil
    then result.token.last:=nil
    else result.token.last:=result.token.first^.last;
    if not(context^.continueEvaluation) then begin
      {$ifdef fullVersion}
      localIdStack.popRemaining(lastLocation);
      {$endif}
      recycler^.cascadeDisposeToken(result.token.first);
    end;
    resetTemp;
    while not(localIdStack.scopeBottom) do localIdStack.scopePop(context,lastLocation,recycler,nextStatement,nil,true);
    localIdStack.destroy;
  end;

CONSTRUCTOR T_idStack.create({$ifdef fullVersion}CONST info:P_callAndIdInfos{$endif});
  begin
    setLength(scope,0);
    {$ifdef fullVersion}
    localIdInfos:=info;
    {$endif}
    clear;
  end;

DESTRUCTOR T_idStack.destroy;
  begin
    clear;
  end;

PROCEDURE T_idStack.clear;
  VAR i:longint;
  begin
    for i:=0 to length(scope)-1 do setLength(scope[i].ids,0);
    setLength(scope,0);
    setLength(suppressUnusedWarningInLines,0);
    doSuppressAllUnusedWarnings:=false;
  end;

PROCEDURE T_idStack.suppressUnusedWarningInLine(CONST lineIndex:longint);
  begin
    append(suppressUnusedWarningInLines,lineIndex);
  end;

PROCEDURE T_idStack.suppressAllUnusedWarnings;
  begin
    doSuppressAllUnusedWarnings:=true;
  end;

PROCEDURE T_idStack.scopePush(CONST openToken:P_token; CONST scopeType:T_scopeType);
  VAR newTopIdx:longint;
  begin
    newTopIdx:=length(scope);
    setLength(scope,newTopIdx+1);
    setLength(scope[newTopIdx].ids,0);
    setLength(scope[newTopIdx].delayedErrorMessages,0);
    scope[newTopIdx].scopeType:=scopeType;
    scope[newTopIdx].scopeStartToken:=openToken;
  end;

PROCEDURE T_idStack.scopePop(CONST context:P_context; CONST location:T_tokenLocation; CONST recycler:P_recycler; VAR workingIn:T_enhancedStatement; CONST closeToken:P_token; CONST forcePop:boolean=false);
  VAR topIdx:longint;
  PROCEDURE postDelayedMismatchError;
    VAR message: string;
        i: longint;
    begin
      if context=nil then exit;
      message:='Mismatch; '+scope[topIdx].scopeStartToken^.singleTokenToString+' closed by '+closeToken^.singleTokenToString;
      with scope[topIdx] do begin
        i:=length(delayedErrorMessages);
        setLength(delayedErrorMessages,i+2);
        new(P_storedMessageWithText(delayedErrorMessages[i  ]),create(mt_el3_evalError,scopeStartToken^.location,message));
        new(P_storedMessageWithText(delayedErrorMessages[i+1]),create(mt_el3_evalError,closeToken     ^.location,message));
      end;
    end;

  PROCEDURE raiseMismatchError;
    VAR message:string;
    begin
      if context=nil then exit;
      message:='Mismatch; '+scope[topIdx].scopeStartToken^.singleTokenToString+' closed by '+closeToken^.singleTokenToString;
      context^.raiseError(message,scope[topIdx].scopeStartToken^.location);
      context^.raiseError(message,                               location);
    end;

  PROCEDURE performPop;
    VAR {$ifdef fullVersion}
        i:longint;
        {$endif}
        msg: P_storedMessage;
    begin
      {$ifdef fullVersion}
      if (localIdInfos<>nil) and (lexingUsecase in C_assistanceUseCases) then
      with scope[topIdx] do for i:=0 to length(ids)-1 do begin
        if not(ids[i].used) and (context<>nil) and not(arrContains(suppressUnusedWarningInLines,ids[i].location.line)) and (ids[i].idType=tt_blockLocalVariable)
        then context^.messages^.postTextMessage(mt_el2_warning,ids[i].location,'Unused local variable '+ids[i].name);
        if lexingUsecase = lxu_assistance then localIdInfos^.addLocalIdInfo(ids[i].name,ids[i].location,location,ids[i].idType,ids[i].used);
      end;
      {$endif}
      setLength(scope[topIdx].ids,0);
      for msg in scope[topIdx].delayedErrorMessages do context^.messages^.postCustomMessage(msg,true);
      setLength(scope,topIdx);
      dec(topIdx);
    end;

  PROCEDURE popSpecialIfPresent;
    VAR t:P_token;
    begin
      topIdx:=length(scope)-1;
      while (topIdx>=0) and (scope[topIdx].scopeStartToken^.tokType in [tt_while,tt_for,tt_then,tt_functionPattern]) do begin
        if scope[topIdx].scopeStartToken^.tokType = tt_then then begin
          scope[topIdx].scopeStartToken^.tokType:=tt_iifCheck;
          t:=workingIn.token.last;
          t^.next:=recycler^.newToken(location,C_tokenDefaultId[tt_iifElse],tt_iifElse);
          t:=t^.next;
          t^.next:=recycler^.newToken(location,LITERAL_TEXT_VOID,tt_literal,newVoidLiteral);
          workingIn.token.last:=t^.next;
        end else if scope[topIdx].scopeStartToken^.tokType = tt_for then begin
          t:=workingIn.token.last;
          t^.next:=recycler^.newToken(location,C_tokenDefaultId[tt_end_for],tt_end_for);
          workingIn.token.last:=t^.next;
        end;
        performPop;
      end;
    end;

  FUNCTION stackIsEmpty:boolean;
    begin
      result:=topIdx<0;
      if result and (context<>nil)
      then context^.raiseError('Missing opening bracket for closing bracket: '+safeTokenToString(closeToken),location);
    end;

  VAR pre:P_token;
      pattern:P_pattern;
      namedParamter: T_patternElementLocation;
      i: longint;
      multi_assign:boolean=false;
      idType: T_tokenType;
      idLoc: T_tokenLocation;
  begin
    if (closeToken<>nil) and (closeToken^.tokType in [tt_semicolon,tt_separatorComma]) or forcePop then begin
      popSpecialIfPresent;
      if (not forcePop) or (forcePop and (topIdx<0)) then exit;
    end;
    if (closeToken=nil) and not forcePop then exit;
    topIdx:=length(scope)-1;
    if stackIsEmpty then exit;
    if closeToken<>nil then case closeToken^.tokType of
      tt_braceClose: begin
        popSpecialIfPresent; if stackIsEmpty then exit;
        if not(scope[topIdx].scopeStartToken^.tokType in [tt_each,tt_parallelEach,tt_braceOpen])
        then raiseMismatchError;
      end;
      tt_listBraceClose: begin
        popSpecialIfPresent; if stackIsEmpty then exit;
        if scope[topIdx].scopeStartToken^.tokType<>tt_listBraceOpen
        then raiseMismatchError;
      end;
      tt_endOfPatternDeclare,tt_endOfPatternAssign:
        if   scope[topIdx].scopeStartToken^.tokType =tt_braceOpen
        then begin
          multi_assign:=(topIdx>0) and (closeToken^.tokType=tt_endOfPatternAssign);
          with scope[topIdx] do begin
            for i:=0 to length(delayedErrorMessages)-1 do disposeMessage(delayedErrorMessages[i]);
            setLength(delayedErrorMessages,0);
          end;
          scope[topIdx].scopeStartToken^.tokType:=tt_startOfPattern;
          //The closing token is not part of the expression yet: add a clone now:
          workingIn.token.last^.next:=recycler^.newToken(closeToken);
          workingIn.token.last      :=workingIn.token.last^.next;
          new(pattern,create);
          if  pattern^.parse(scope[topIdx].scopeStartToken,
                             scope[topIdx].scopeStartToken^.location,
                             context,
                             recycler,
                             multi_assign{$ifdef fullVersion},localIdInfos{$endif})
          then begin

            //expression changed; need to collect new last token
            workingIn.token.last:=scope[topIdx].scopeStartToken^.last;

            case closeToken^.tokType of
              tt_endOfPatternDeclare: closeToken^.tokType:=tt_declare;
              tt_endOfPatternAssign : closeToken^.tokType:=tt_assign;
            end;
            if (topIdx=0) and (workingIn.assignmentToken=nil) then workingIn.assignmentToken:=closeToken;
            if multi_assign then begin
              if pattern^.isVariadic then context^.raiseError('Invalid token "..." in this context.',scope[topIdx].scopeStartToken^.location);
              for namedParamter in pattern^.getNamedParameters do
              if not hasId(namedParamter.id,idType,idLoc) or (idType<>tt_blockLocalVariable)
              then begin
                addId(namedParamter.id,
                      namedParamter.location,
                      tt_blockLocalVariable);
              end;
            end else begin
              for namedParamter in pattern^.getNamedParameters do
                addId(namedParamter.id,
                      namedParamter.location,
                      tt_parameterIdentifier);
              //We exit early, because we do not want to pop the scope we just modified.
              exit;
            end;
          end else dispose(pattern,destroy);
        end else raiseMismatchError;
      tt_expBraceClose: begin
        popSpecialIfPresent; if stackIsEmpty then exit;
        if scope[topIdx].scopeStartToken^.tokType<>tt_expBraceOpen
        then raiseMismatchError;
      end;
      tt_then:
        if scope[topIdx].scopeStartToken^.tokType = tt_if
        then begin
          {$ifdef fullVersion}
          if (localIdInfos<>nil) and (lexingUsecase=lxu_assistance) then localIdInfos^.addTokenRelation(scope[topIdx].scopeStartToken,closeToken^.location);
          {$endif}
          pre:=workingIn.token.first;
          if pre = scope[topIdx].scopeStartToken
          then workingIn.token.first:=recycler^.disposeToken(workingIn.token.first)
          else begin
            while (pre<>nil) and (pre^.next<>nil) and (pre^.next<>scope[topIdx].scopeStartToken) do pre:=pre^.next;
            pre^.next:=recycler^.disposeToken(scope[topIdx].scopeStartToken);
          end;
          scope[topIdx].scopeStartToken:=closeToken;
          exit;
        end
        else raiseMismatchError;
      tt_else:
        if scope[topIdx].scopeStartToken^.tokType=tt_then
        then begin
          {$ifdef fullVersion}
          if (localIdInfos<>nil) and (lexingUsecase=lxu_assistance) then localIdInfos^.addTokenRelation(scope[topIdx].scopeStartToken,closeToken^.location);
          {$endif}
          scope[topIdx].scopeStartToken^.tokType:=tt_iifCheck;
          closeToken                   ^.tokType:=tt_iifElse;
        end else raiseMismatchError;
      tt_iifElse: begin
        popSpecialIfPresent; if stackIsEmpty then exit;
        if scope[topIdx].scopeStartToken^.tokType=tt_iifCheck then begin
          {$ifdef fullVersion}
          if (localIdInfos<>nil) and (lexingUsecase=lxu_assistance) then localIdInfos^.addTokenRelation(scope[topIdx].scopeStartToken,closeToken^.location);
          {$endif}
        end else begin
          postDelayedMismatchError;
          exit;
        end;
      end;
      tt_endBlock: begin
        popSpecialIfPresent; if stackIsEmpty then exit;
        if scope[topIdx].scopeStartToken^.tokType=tt_beginBlock then begin
          {$ifdef fullVersion}
          if (localIdInfos<>nil) and (lexingUsecase=lxu_assistance) then localIdInfos^.addTokenRelation(scope[topIdx].scopeStartToken,closeToken^.location);
          {$endif}
        end else raiseMismatchError;
      end;
      tt_do:
        if scope[topIdx].scopeStartToken^.tokType=tt_for then begin
          {$ifdef fullVersion}
          if (localIdInfos<>nil) and (lexingUsecase=lxu_assistance) then localIdInfos^.addTokenRelation(scope[topIdx].scopeStartToken,closeToken^.location);
          {$endif}
          if closeToken^.getDoType=dt_unknown then closeToken^.setDoType(dt_for_related_do);
          exit; //This is not a pop...
        end else if scope[topIdx].scopeStartToken^.tokType=tt_while then begin
          {$ifdef fullVersion}
          if (localIdInfos<>nil) and (lexingUsecase=lxu_assistance) then localIdInfos^.addTokenRelation(scope[topIdx].scopeStartToken,closeToken^.location);
          {$endif}
          if closeToken^.getDoType=dt_unknown then closeToken^.setDoType(dt_while_related_do);
        end else raiseMismatchError;
      tt_aggregatorConstructor: begin
        {$ifdef fullVersion}
        if (scope[topIdx].scopeStartToken^.tokType in [tt_for,tt_each,tt_parallelEach]) and (localIdInfos<>nil) and (lexingUsecase=lxu_assistance) then localIdInfos^.addTokenRelation(scope[topIdx].scopeStartToken,closeToken^.location);
        {$endif}
        exit;
      end;
      tt_until: begin
        if scope[topIdx].scopeStartToken^.tokType=tt_repeat then begin
          {$ifdef fullVersion}
          if (localIdInfos<>nil) and (lexingUsecase=lxu_assistance) then localIdInfos^.addTokenRelation(scope[topIdx].scopeStartToken,closeToken^.location);
          {$endif}
        end else raiseMismatchError;
      end;
      else raise Exception.create('Unexpected closing token '+tokenTypeName(closeToken^.tokType));
    end;
    performPop;
  end;

{$ifdef fullVersion}
PROCEDURE T_idStack.popRemaining(CONST lastLocation: T_tokenLocation);
  VAR topIdx:longint;
      i:longint;
  begin
    topIdx:=length(scope)-1;
    while topIdx>=0 do begin
      with scope[topIdx] do for i:=0 to length(ids)-1 do begin
        if localIdInfos<>nil then localIdInfos^.addLocalIdInfo(ids[i].name,ids[i].location,lastLocation,ids[i].idType,ids[i].used);
      end;
      setLength(scope[topIdx].ids,0);
      topIdx-=1;
    end;
    setLength(scope,0);
  end;
{$endif}

FUNCTION T_idStack.oneAboveBottom:boolean;
  begin
    result:=length(scope)=1;
  end;

FUNCTION T_idStack.scopeBottom:boolean;
  begin
    result:=length(scope)=0;
  end;

FUNCTION T_idStack.hasBeginItem:boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=0 to length(scope)-1 do if scope[i].scopeType in [sc_block,sc_repeat] then exit(true);
  end;

FUNCTION T_idStack.addId(CONST id:T_idString; CONST location:T_tokenLocation; CONST idType:T_tokenType):T_addIdResult;
  VAR i,j:longint;
  begin
    i:=length(scope)-1;
    if idType=tt_blockLocalVariable
    then while (i>=0) and not(scope[i].scopeType in [sc_block,sc_repeat]) do dec(i);
    if i<0 then exit(air_notInBlock);
    with scope[i] do begin
      for j:=0 to length(ids)-1 do if ids[j].name=id then exit(air_reintroduce);
      j:=length(ids);
      setLength(ids,j+1);
      ids[j].name:=id;
      ids[j].used:=not(idType in [tt_blockLocalVariable,tt_parameterIdentifier]);
      ids[j].location:=location;
      ids[j].idType:=idType;
      result:=air_ok;
    end;
  end;

FUNCTION T_idStack.hasId(CONST id:T_idString; OUT idType:T_tokenType; OUT idLoc:T_tokenLocation):boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=length(scope)-1 downto 0 do with scope[i] do
    for j:=0 to length(ids)-1 do if ids[j].name=id then begin
      ids[j].used:=true;
      idType:=ids[j].idType;
      idLoc:=ids[j].location;
      exit(true);
    end;
  end;

FUNCTION isOperatorName(CONST id:T_idString):boolean;
  VAR s:string;
  begin
    for s in operatorName do if id=s then exit(true);
    result:=false;
  end;

{$ifdef fullVersion}
CONSTRUCTOR T_callAndIdInfos.create;
  begin
    usedBuiltins.create;
    clear;
  end;

DESTRUCTOR T_callAndIdInfos.destroy;
  begin
    clear;
    usedBuiltins.destroy;
  end;

PROCEDURE T_callAndIdInfos.clear;
  begin
    setLength(usageInfos,0);
    usedBuiltins.clear;
    usageInfoFill:=0;
    setLength(localIdInfos,0);
    setLength(blobLocations,0);
    setLength(relatedTokens,0);
    setLength(formatStrings,0);
  end;

PROCEDURE T_callAndIdInfos.add(CONST token: P_token);
  begin
    if (token=nil) then exit;
    if      token^.tokType in [tt_comparatorEq..tt_operatorConcatAlt] then usedBuiltins.put(intFuncForOperator[token^.tokType])
    else if token^.tokType=tt_intrinsicRule                           then usedBuiltins.put(token^.data)
    else if token^.tokType in [tt_userRule,tt_customType,tt_globalVariable,tt_customTypeCheck] then begin
      if usageInfoFill>=length(usageInfos) then setLength(usageInfos,round(length(usageInfos)*1.1)+1);
      usageInfos[usageInfoFill].referencedAt:=token^.location;
      usageInfos[usageInfoFill].targetLocation:=P_objectWithIdAndLocation(token^.data)^.getLocation;
      inc(usageInfoFill);
    end;
  end;

PROCEDURE T_callAndIdInfos.addAll(CONST token:P_token);
  VAR t:P_token;
  begin
    t:=token;
    while t<>nil do begin
      add(t);
      t:=t^.next;
    end;
  end;

PROCEDURE T_callAndIdInfos.cleanup;
  VAR temporary:specialize G_stringKeyMap<T_usageInfo>;
      info:T_usageInfo;
      newDat:temporary.VALUE_TYPE_ARRAY;
      k:longint;
  begin
    setLength(usageInfos,usageInfoFill);
    temporary.create();
    for info in usageInfos do temporary.put(string(info.referencedAt),info);
    newDat:=temporary.valueSet;
    usageInfoFill:=length(newDat);
    setLength(usageInfos,usageInfoFill);
    for k:=0 to length(usageInfos)-1 do usageInfos[k]:=newDat[k];
    setLength(newDat,0);
    temporary.destroy;
  end;

FUNCTION T_callAndIdInfos.calledBuiltinFunctions: T_builtinFunctionMetaDatas;
  VAR func:pointer;
      k:longint=0;
  begin
    setLength(result,usedBuiltins.size);
    for func in usedBuiltins.values do begin
      result[k]:=builtinFunctionMap.getMeta(P_intFuncCallback(func));
      inc(k);
    end;
  end;

FUNCTION T_callAndIdInfos.getBuiltinSideEffects:T_sideEffects;
  VAR meta:P_builtinFunctionMetaData;
  begin
    result:=[];
    for meta in calledBuiltinFunctions do result+=meta^.sideEffects;
  end;

FUNCTION T_callAndIdInfos.whoReferencesLocation(CONST loc: T_searchTokenLocation): T_searchTokenLocations;
  VAR info:T_usageInfo;
  begin
    //TODO: Can import-overloads be taken into account?
    //This would require modification of the rule map
    if usageInfoFill<>length(usageInfos) then cleanup;
    setLength(result,0);
    for info in usageInfos do if info.targetLocation=loc then begin
      setLength(result,length(result)+1);
      result[length(result)-1]:=info.referencedAt;
    end;
    sortLocations(result);
  end;

FUNCTION T_callAndIdInfos.isLocationReferenced(CONST loc:T_searchTokenLocation):boolean;
  VAR info:T_usageInfo;
  begin
    if usageInfoFill<>length(usageInfos) then cleanup;
    for info in usageInfos do if info.targetLocation=loc then exit(true);
    result:=false;
  end;

FUNCTION T_callAndIdInfos.isParameterReferenced(CONST loc:T_searchTokenLocation):boolean;
  VAR
    info: T_localIdInfo;
  begin
    for info in localIdInfos do if (info.tokenType=tt_parameterIdentifier) and (info.validFrom=loc) then exit(info.used);
    result:=false;
  end;

FUNCTION T_callAndIdInfos.isPackageReferenced(CONST packagePath:string):boolean;
  VAR info:T_usageInfo;
  begin
    if usageInfoFill<>length(usageInfos) then cleanup;
    for info in usageInfos do if (info.referencedAt.fileName<>packagePath) and (info.targetLocation.fileName=packagePath) then exit(true);
    result:=false;
  end;

FUNCTION T_callAndIdInfos.isEmpty:boolean;
  begin
    result:=(usageInfoFill=0) and (length(localIdInfos)=0) and (length(blobLocations)=0);
  end;

PROCEDURE T_callAndIdInfos.includeUsages(CONST other:P_callAndIdInfos);
  VAR k :longint;
  begin
    other^.cleanup;
    setLength(usageInfos,usageInfoFill+other^.usageInfoFill);
    for k:=0 to other^.usageInfoFill-1 do begin
      usageInfos[usageInfoFill]:=other^.usageInfos[k];
      inc(usageInfoFill);
    end;
    usedBuiltins.put(other^.usedBuiltins.values);
  end;

PROCEDURE T_callAndIdInfos.addTokenRelation(CONST token:P_token; CONST other:T_tokenLocation);
  PROCEDURE assign(VAR target:T_simpleTokenRange; CONST source:T_tokenLocation);
    begin
      target.width:=length(token^.txt);
      if target.width<=0 then target.width:=length(C_tokenDefaultId[token^.tokType]);
      if target.width<=0 then target.width:=1;
      inc(target.width);
      target.x:=source.column;
      target.y:=source.line;
    end;

  VAR i,j:longint;
  begin
    i:=getIndexOfRelated(token^.location.column,token^.location.line,true);
    j:=getIndexOfRelated(other          .column,other          .line,true);
    if i>=0 then begin
      if j>=0 then exit;
      //other is new
      with relatedTokens[i] do begin
        if count>=length(position) then exit;
        assign(position[count],other);
        inc(count);
      end;
    end else begin
      if j>=0 then begin
        //token is new
        with relatedTokens[j] do begin
          if count>=length(position) then exit;
          assign(position[count],token^.location);
          inc(count);
        end;
      end else begin
        //both are new
        i:=length(relatedTokens);
        setLength(relatedTokens,i+1);
        with relatedTokens[i] do begin
          count:=2;
          assign(position[0],token^.location);
          assign(position[1],other);
        end;
      end;
    end;
  end;

FUNCTION T_callAndIdInfos.getIndexOfRelated(CONST CaretX,CaretY:longint; CONST exactMatchOnly:boolean=false):longint;
  VAR i,j:longint;
  begin
    for i:=0 to length(relatedTokens)-1 do begin
      for j:=0 to relatedTokens[i].count-1 do
        if (CaretY =  relatedTokens[i].position[j].y) and
          ((CaretX =  relatedTokens[i].position[j].x) or not exactMatchOnly and
           (CaretX >= relatedTokens[i].position[j].x) and
           (CaretX <  relatedTokens[i].position[j].x+relatedTokens[i].position[j].width))
        then exit(i);
    end;
    result:=-1;
  end;

FUNCTION T_callAndIdInfos.getRelated(CONST CaretX,CaretY:longint):T_relatedTokens;
  VAR i:longint;
  begin
    i:=getIndexOfRelated(CaretX,CaretY);
    if i<0 then result.count:=0 else result:=relatedTokens[i];
  end;

PROCEDURE T_callAndIdInfos.copyFrom(CONST original:P_callAndIdInfos);
  VAR k:longint;
  begin
    clear;
    if original=nil then exit;
    setLength(localIdInfos,length(original^.localIdInfos));
    for k:=0 to length(localIdInfos)-1 do localIdInfos[k]:=original^.localIdInfos[k];
    setLength(blobLocations,length(original^.blobLocations));
    for k:=0 to length(blobLocations)-1 do blobLocations[k]:=original^.blobLocations[k];
    setLength(relatedTokens,length(original^.relatedTokens));
    for k:=0 to length(relatedTokens)-1 do relatedTokens[k]:=original^.relatedTokens[k];
    setLength(formatStrings,length(original^.formatStrings));
    for k:=0 to length(formatStrings)-1 do formatStrings[k]:=original^.formatStrings[k];
  end;

FUNCTION T_callAndIdInfos.localTypeOf(CONST id: T_idString; CONST line, col: longint; OUT declaredAt: T_tokenLocation): T_tokenType;
  VAR entry:T_localIdInfo;
  begin
    result:=tt_literal;
    for entry in localIdInfos do
      if (entry.name=id) and
         not(positionIsBeforeLocation    (line,col,entry.validFrom)) and
             positionIsBeforeOrAtLocation(line,col,entry.validUntil) then begin
        declaredAt:=entry.validFrom;
        exit(entry.tokenType);
      end;
  end;

FUNCTION T_callAndIdInfos.allLocalIdsAt(CONST line,col:longint):T_arrayOfString;
  VAR entry:T_localIdInfo;
  begin
    setLength(result,0);
    for entry in localIdInfos do
      if not(positionIsBeforeLocation    (line,col,entry.validFrom)) and
             positionIsBeforeOrAtLocation(line,col,entry.validUntil) then begin
        append(result,entry.name);
      end;
  end;

PROCEDURE T_callAndIdInfos.addLocalIdInfo(CONST id:T_idString; CONST validFrom,validUntil:T_tokenLocation; CONST typ:T_tokenType; CONST used:boolean);
  VAR i:longint;
  begin
    i:=length(localIdInfos);
    setLength(localIdInfos,i+1);
    localIdInfos[i].name      :=id;
    localIdInfos[i].validFrom :=validFrom;
    localIdInfos[i].validUntil:=validUntil;
    localIdInfos[i].tokenType :=typ;
    localIdInfos[i].used      :=used;
  end;

PROCEDURE T_callAndIdInfos.markBlobLine(CONST startLine_,startCol_,endLine_,endCol_:longint; CONST closer_:char);
  VAR k:longint;
  begin
    k:=length(blobLocations);
    setLength(blobLocations,k+1);
    with blobLocations[k] do begin
      closer   :=closer_;
      startLine:=startLine_;
      startCol :=startCol_ ;
      endLine  :=endLine_  ;
      endCol   :=endCol_   ;
    end;
  end;

PROCEDURE T_callAndIdInfos.addFormatStrings(CONST ranges:T_simpleTokenRanges);
  VAR i,i0:longint;
      firstLine:longint=-1;
      lastLine :longint=-1;
  begin
    i0:=length(formatStrings);
    setLength(formatStrings,i0+length(ranges));
    for i:=0 to length(ranges)-1 do begin
      formatStrings[i0+i]:=ranges[i];
      if firstLine<0 then firstLine:=ranges[i].y;
      lastLine:=ranges[i].y;
    end;

    if firstLine<0 then exit;

    //i:=0;
    //for i0:=0 to length(blobLines)-1 do if (blobLines[i0].lineIndex<firstLine) or (blobLines[i0].lineIndex>lastLine) then begin
    //  if i<>i0 then blobLines[i]:=blobLines[i0];
    //  inc(i);
    //end;
    //if i<>length(blobLines) then setLength(blobLines,i);
  end;

FUNCTION T_callAndIdInfos.isFormatString(CONST lineIndex, tokenStart:longint; OUT tokenEnd:longint):boolean;
  VAR r:T_simpleTokenRange;
  begin
    result:=false;
    for r in formatStrings do if (r.y=lineIndex+1) and (r.x=tokenStart) then begin
      tokenEnd:=r.x+r.width;
      exit(r.width>0);
    end;
  end;

FUNCTION T_callAndIdInfos.getEndOfBlob(CONST lineIndex,colIndex:longint):longint;
  VAR k:longint;
  begin
    result:=-1;
    for k:=0 to length(blobLocations)-1 do with blobLocations[k] do begin
      if (lineIndex>startLine) or (lineIndex=startLine) and (colIndex>=startCol) then begin
        if (lineIndex<endLine) then exit(maxLongint)
        else if lineIndex=endLine then exit(endCol);
      end;
    end;
  end;

FUNCTION T_callAndIdInfos.getBlobCloserOrZero(CONST lineIndex:longint):char;
  VAR k:longint;
  begin
    result:=#0;
    for k:=0 to length(blobLocations)-1 do if (lineIndex>blobLocations[k].startLine) and (lineIndex<=blobLocations[k].endLine) then exit(blobLocations[k].closer);
  end;

{$endif}

CONSTRUCTOR T_mnhSystemPseudoPackage.create;
  begin
    inherited create(newVirtualFileCodeProvider('',C_EMPTY_STRING_ARRAY));
  end;

FUNCTION T_mnhSystemPseudoPackage.getId: T_idString;
  begin
    result:='[MNH]';
  end;

FUNCTION T_mnhSystemPseudoPackage.getPath: ansistring;
  begin
    result:='[MNH]';
  end;

{$ifdef fullVersion}
PROCEDURE T_enhancedTokens.add(CONST tok: P_token; CONST callAndIdInfos:P_callAndIdInfos; CONST package: P_abstractPackage);
  VAR i:longint;
  begin
    i:=length(dat);
    setLength(dat,i+1);
    dat[i].create(tok,callAndIdInfos,package);
    if (i>0) and (tok<>nil) then begin
      dat[i-1].endsAtColumn:=tok^.location.column-1;
    end;
  end;

PROCEDURE T_enhancedTokens.addLineEnder(CONST lineLength:longint);
  VAR last:longint;
  begin
    last:=length(dat)-1;
    add(nil,nil,nil);
    if last>0 then dat[last].endsAtColumn:=lineLength;
  end;

CONSTRUCTOR T_enhancedTokens.create;
  begin
    setLength(dat,0);
  end;

DESTRUCTOR T_enhancedTokens.destroy;
  VAR i:longint;
      recycler:P_recycler;
  begin
    recycler:=newRecycler;
    for i:=0 to length(dat)-1 do begin
      dat[i].cleanup(recycler);
      dat[i].destroy;
    end;
    setLength(dat,0);
    freeRecycler(recycler);
  end;

FUNCTION T_enhancedTokens.getTokenAtIndex(CONST rowIndex: longint): T_enhancedToken;
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do if (dat[i].token<>nil) and (rowIndex>=dat[i].token^.location.column) and (rowIndex<=dat[i].endsAtColumn) then exit(dat[i]);
    //Fallback 1
    for i:=1 to length(dat)-1 do if (dat[i].token<>nil) and (dat[i].token^.location.column>rowIndex) then exit(dat[i-1]);
    //Fallback 2
    result:=dat[length(dat)-1];
  end;

FUNCTION T_enhancedTokens.renameInLine(VAR line:string; CONST referencedLocation:T_searchTokenLocation; CONST oldName,newName:string):boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=length(dat)-1 downto 0 do if dat[i].token<>nil then begin
      if dat[i].renameInLine(line,referencedLocation,oldName,newName) then result:=true;
    end;
  end;

CONSTRUCTOR T_enhancedToken.create(CONST tok: P_token; CONST callAndIdInfos:P_callAndIdInfos; CONST package:P_abstractPackage);
  VAR tokenText:string;
  begin
    linksTo:=nothing;
    endsAtColumn:=maxLongint;
    if tok=nil then begin
      token:=nil;
      originalType:=tt_EOL;
      references.package:=package;
      references.column:=0;
      references.line:=0;
      exit;
    end;
    token:=tok;
    originalType:=token^.tokType;
    references:=token^.location; //default: references itself

    tokenText:=safeTokenToString(token);
    if (token^.tokType in [tt_userRule,tt_customTypeCheck,tt_identifier,tt_globalVariable,tt_customType]) then
    case callAndIdInfos^.localTypeOf(tokenText,token^.location.line,token^.location.column,references) of
      tt_eachParameter: begin
        token^.tokType:=tt_eachParameter;
        references.package:=package;
      end;
      tt_eachIndex: begin
        token^.tokType:=tt_eachIndex;
        references.package:=package;
      end;
      tt_blockLocalVariable: begin
        token^.tokType:=tt_blockLocalVariable;
        references.package:=package;
      end;
      tt_parameterIdentifier: begin
        token^.tokType:=tt_parameterIdentifier;
        references.package:=package;
      end;
      tt_use: begin
        references.package:=package^.getImport(tokenText);
        if references.package=nil
        then references.package:=package
        else begin
          linksTo:=packageUse;
          references.line:=1;
          references.column:=1;
        end;
      end;
      tt_include:begin
        references.package:=package^.getExtended(tokenText);
        if references.package=nil
        then references.package:=package
        else begin
          linksTo:=packageInclude;
          references.line:=1;
          references.column:=1;
        end;
      end;
    end;
    if (linksTo=nothing) then case token^.tokType of
      tt_userRule,tt_globalVariable   : references:=P_abstractRule(token^.data)^.getLocation;
      tt_customType,tt_customTypeCheck: references:=P_typedef(token^.data)^.getLocation;
    end;
  end;

PROCEDURE T_enhancedToken.cleanup(CONST recycler:P_recycler);
  begin
    if token<>nil then recycler^.disposeToken(token);
    token:=nil;
  end;

DESTRUCTOR T_enhancedToken.destroy;
  begin
    assert(token=nil);
  end;

FUNCTION T_enhancedToken.renameInLine(VAR line: string; CONST referencedLocation: T_searchTokenLocation; CONST oldName,newName: string): boolean;
  VAR partBefore, partBetween, partAfter: string;
      idParts: T_arrayOfString;
      i:longint;
  begin
    case token^.tokType of
      tt_identifier         ,
      tt_parameterIdentifier,
      tt_userRule      ,
      tt_globalVariable,
      tt_customType,
      tt_blockLocalVariable ,
      tt_customTypeCheck    ,
      tt_eachParameter,
      tt_each,tt_parallelEach,tt_for: if references<>referencedLocation then exit(false);
      else exit(false);
    end;
    {$WARN 5092 OFF}
    result:=true;
    if endsAtColumn>length(line) then endsAtColumn:=length(line);
    if line[endsAtColumn]=' ' then dec(endsAtColumn);
    partBefore :=copy(line,1,token^.location.column-1);
    partBetween:=copy(line,token^.location.column,endsAtColumn-token^.location.column+1);
    partAfter  :=copy(line,endsAtColumn+1,length(line));
    if token^.tokType in [tt_each,tt_parallelEach]
    then begin
      idParts:=split(token^.txt,',');
      if length(idParts)>1 then begin
        for i:=0 to length(idParts)-1 do if idParts[i]=oldName then idParts[i]:=newName;
        partBetween:=C_tokenDefaultId[token^.tokType]+'(('+join(idParts,',')+'),';
      end else partBetween:=C_tokenDefaultId[token^.tokType]+'('+newName+',';
    end
    else if partBetween='is'+oldName then partBetween:='is'+newName
    else if partBetween='to'+oldName then partBetween:='to'+newName
    else    partBetween:=replaceOne(partBetween,oldName,newName);
    line:=partBefore+partBetween+partAfter;
  end;

FUNCTION T_enhancedToken.toInfo:T_tokenInfo;
  VAR i:longint;
  PROCEDURE getBuiltinRuleInfo(VAR link:string);
    VAR doc:P_intrinsicFunctionDocumentation=nil;
    begin
      if not(functionDocMap.containsKey(result.tokenText,doc)) then begin
        if not(functionDocMap.containsKey(unqualifiedId(result.tokenText),doc))
        then doc:=nil;
      end;
      if doc<>nil then begin
        result.builtinRuleInfo:=doc^.getStructuredInfo(result.exampleText);
        link:=doc^.getHtmlLink;
      end;
    end;
  begin
    result.linkToHelp:=getDocIndexLinkForBrowser;
    result.location:=C_nilSearchTokenLocation;
    result.startLoc:=C_nilSearchTokenLocation;
    result.endLoc  :=C_nilSearchTokenLocation;
    result.canRename:=false;
    result.mightBeUsedInOtherPackages:=false;
    result.idWithoutIsPrefix:='';
    result.tokenType:=tt_EOL;
    setLength(result.exampleText    ,0);
    setLength(result.builtinRuleInfo,0);
    setLength(result.userDefRuleInfo,0);

    if token=nil then exit;
    result.tokenType    :=token^.tokType;
    if C_tokenDoc[result.tokenType].helpLink<>''
    then result.linkToHelp:=getDocIndexLinkForBrowser(C_tokenDoc[result.tokenType].helpLink);

    result.location     :=references;
    result.startLoc     :=token^.location;
    result.endLoc       :=token^.location;
    result.endLoc.column:=endsAtColumn;
    result.canRename:=(token^.tokType in [tt_parameterIdentifier,tt_userRule,tt_globalVariable,tt_customType,tt_blockLocalVariable,tt_customTypeCheck]) or
                      (token^.tokType in [tt_eachParameter,tt_each,tt_parallelEach,tt_for]) and isIdentifier(token^.txt,false);
    result.tokenText:=safeTokenToString(token);
    if result.canRename then begin
      case token^.tokType of
        tt_each,tt_parallelEach, tt_for:   result.idWithoutIsPrefix:=token^.txt;
        tt_customType,tt_customTypeCheck:  result.idWithoutIsPrefix:=P_typedef(token^.data)^.getId;
        tt_userRule,tt_globalVariable:     result.idWithoutIsPrefix:=P_abstractRule(token^.data)^.getRootId;
        else                               result.idWithoutIsPrefix:=result.tokenText;
      end;
      result.mightBeUsedInOtherPackages:=(token^.tokType in [tt_userRule,tt_globalVariable]) and (P_abstractRule(token^.data)^.hasPublicSubrule) or (token^.tokType in [tt_customTypeCheck,tt_customType]);
    end;
    result.shortInfo:='';
    case linksTo of
      packageUse: begin
        result.shortInfo:=C_lineBreakChar+'Used package: '+ansistring(references);
        exit;
      end;
      packageInclude: begin
        result.shortInfo:=C_lineBreakChar+'Included package: '+ansistring(references);
        exit;
      end;
    end;
    result.shortInfo:=ansiReplaceStr(C_tokenDoc[token^.tokType].helpText,'#',C_lineBreakChar);
    for i:=0 to length(C_specialWordInfo)-1 do
      if C_specialWordInfo[i].txt=result.tokenText then
      result.shortInfo:=C_lineBreakChar+ansiReplaceStr(C_specialWordInfo[i].helpText,'#',C_lineBreakChar);

    case token^.tokType of
      tt_intrinsicRule:
        getBuiltinRuleInfo(result.linkToHelp);
      tt_blockLocalVariable, tt_parameterIdentifier, tt_eachParameter, tt_eachIndex:
        begin
          result.shortInfo+=' declared at '+string(references);
        end;
      tt_comparatorEq..tt_unaryOpMinus: begin
        result.tokenText:=operatorName[token^.tokType];
        getBuiltinRuleInfo(result.linkToHelp);
      end;
      tt_attributeComment: begin
        if result.tokenText=ATTRIBUTE_PREFIX+EXECUTE_AFTER_ATTRIBUTE
        then result.shortInfo:='marks a nullary subrule for execution after the script is finished without raising an error';
        if startsWith(result.tokenText,ATTRIBUTE_PREFIX+SUPPRESS_UNUSED_WARNING_ATTRIBUTE)
        then result.shortInfo:='suppresses warnings about unused rules';
        if result.tokenText=ATTRIBUTE_PREFIX+SUPPRESS_UNUSED_PARAMETER_WARNING_ATTRIBUTE
        then result.shortInfo:='suppresses warnings about unused parameters';
        if startsWith(result.tokenText,ATTRIBUTE_PREFIX+MAX_AGE_ATTRIBUTE)
        then result.shortInfo:='Sets the maximum age for a cached element in seconds';
      end;
      tt_userRule, tt_globalVariable: begin
        result.userDefRuleInfo:=P_abstractRule(token^.data)^.getStructuredInfo;
        if builtinFunctionMap.containsKey(result.tokenText) then getBuiltinRuleInfo(result.linkToHelp);
      end;
      tt_customType, tt_customTypeCheck: begin
        if P_typedef(token^.data)^.isDucktyping
        then result.shortInfo:=C_ruleTypeText[rt_duckTypeCheck  ]
        else result.shortInfo:=C_ruleTypeText[rt_customTypeCheck];
        setLength(result.userDefRuleInfo,1);
        result.userDefRuleInfo[0]:=P_subruleExpression(P_typedef(token^.data)^.getDuckTypeRule)^.getStructuredInfo;
      end;
      tt_identifier:
        if arrContains(validStringTypes,token^.txt) then result.shortInfo:='builtin type';
      tt_type,tt_typeCheck:
        result.shortInfo:=ansiReplaceStr(C_typeCheckInfo[token^.getTypeCheck].helpText,'#',C_lineBreakChar);
      tt_modifier:
        result.shortInfo:=ansiReplaceStr(C_modifierInfo[token^.getModifier].helpText,'#',C_lineBreakChar);
      else if tokenDocumentation[token^.tokType]<>nil then tokenDocumentation[token^.tokType]^.getStructuredInfo(result.exampleText);
    end;
  end;
{$endif}

FUNCTION T_abstractLexer.getToken(CONST line: ansistring; VAR inputLocation:T_tokenLocation; CONST messages:P_messages; CONST recycler:P_recycler): P_token;
  VAR parsedLength:longint=0;

  PROCEDURE fail(message:ansistring);
    begin
      messages^.raiseSimpleError(message,inputLocation);
    end;

  FUNCTION isFormatString:boolean;
    begin
      result:=(line[inputLocation.column]='f') and (inputLocation.column+1<length(line)) and
              (line[inputLocation.column+1] in ['"','''']);
    end;

  FUNCTION leadingId:T_idString;
    VAR i:longint;
        tt:T_tokenType;
        match:boolean;
    begin
      i:=inputLocation.column;
      while (i<length(line)) and (line[i+1] in ['a'..'z','A'..'Z','0'..'9','_']) do inc(i);
      parsedLength:=i-inputLocation.column+1;
      for tt:=low(T_tokenType) to high(T_tokenType) do if length(C_tokenDefaultId[tt])=parsedLength then begin
        match:=true;
        for i:=0 to parsedLength-1 do match:=match and (line[inputLocation.column+i]=C_tokenDefaultId[tt][i+1]);
        if match then exit(C_tokenDefaultId[tt]);
      end;
      result:=copy(line,inputLocation.column,parsedLength);
    end;

  FUNCTION startsWith(CONST c:char):boolean; inline;
    begin result:=line[inputLocation.column]=c; end;
  FUNCTION startsWith(CONST prefix:string):boolean;  inline;
    begin result:=copy(line,inputLocation.column,length(prefix))=prefix; end;
  FUNCTION startsWith(CONST t:T_tokenType):boolean; inline;
    begin result:=copy(line,inputLocation.column,length(C_tokenDefaultId[t]))=C_tokenDefaultId[t]; end;
  PROCEDURE apply(CONST t:T_tokenType); inline;
    begin
      result^.tokType:=t;
      parsedLength:=length(C_tokenDefaultId[t]);
    end;
  PROCEDURE apply(CONST len:longint; CONST t:T_tokenType); inline;
    begin
      result^.tokType:=t;
      if t=tt_do then result^.setDoType(dt_unknown);
      parsedLength:=len;
    end;

  PROCEDURE handleComment(CONST commentText:ansistring; CONST commentOpener:string);
    begin
      result^.tokType:=tt_blank;
      if commentOpener=ATTRIBUTE_PREFIX then begin
        result^.txt:=trimRight(copy(commentText,length(ATTRIBUTE_PREFIX)+1,length(commentText)));
        result^.tokType:=tt_attributeComment;
      end else if copy(commentText,1,length(DOC_COMMENT_INFIX))=DOC_COMMENT_INFIX then begin
        result^.txt:=trimRight(copy(commentText,length(DOC_COMMENT_INFIX)+1,length(commentText)));
        result^.tokType:=tt_docComment;
      end else if (commentOpener<>'#') and (copy(commentText,1,length(SPECIAL_COMMENT_BLOB_BEGIN_INFIX))=SPECIAL_COMMENT_BLOB_BEGIN_INFIX) then begin
        result^.txt:=SPECIAL_COMMENT_BLOB_BEGIN_INFIX;
        blob.start:=inputLocation;
        if length(commentText)>=1+length(SPECIAL_COMMENT_BLOB_BEGIN_INFIX)
        then begin
          blob.closer:=commentText[1+length(SPECIAL_COMMENT_BLOB_BEGIN_INFIX)];
          parsedLength:=length(commentOpener+SPECIAL_COMMENT_BLOB_BEGIN_INFIX)+1;
        end else {if commentOpener='#' then blob.closer:='#' else} blob.closer:='''';
      end {$ifdef fullVersion} else if (pos('TODO',commentText)>0) and (usecase in C_assistanceUseCases) then messages^.postTextMessage(mt_el1_note,inputLocation,commentText) {$endif};
    end;

  {$MACRO ON}
  {$define exitFailing:=
      begin
        fail('Cannot parse: '+copy(line,inputLocation.column,20)+' (first char is "'+line[inputLocation.column]+'"=#'+intToStr(ord(line[inputLocation.column]))+')');
        inputLocation.column:=length(line)+1;
        recycler^.disposeToken(result);
        exit(nil);
      end}

  VAR id:ansistring='';
      stringValue:ansistring='';
      tt:T_tokenType;
      tc:T_typeCheck;
      md:T_modifier;
      firstInLine:boolean;
  begin
    firstInLine:=inputLocation.column=1;
    result:=recycler^.newToken(inputLocation,'',tt_EOL);
    with blob do if closer<>#0 then begin
      //id now is rest of line
      id:=copy(line,inputLocation.column,length(line));
      if pos(closer,id)<=0 then begin
        parsedLength:=length(id);
        inc(inputLocation.column,parsedLength);
        append(lines,id);
      end else begin
        parsedLength:=pos(closer,id)+length(closer)-1;
        inc(inputLocation.column,parsedLength);
        {$ifdef fullVersion}
        if (usecase=lxu_assistance) and (callAndIdInfos<>nil) then callAndIdInfos^.markBlobLine(blob.start.line,blob.start.column,inputLocation.line,inputLocation.column-1,closer);
        {$endif}
        append(lines,copy(id,1,pos(closer,id)-1));
        result^.location:=start;
        result^.tokType:=tt_literal;
        result^.data:=recycler^.newStringLiteral(join(lines,C_lineBreakChar),'//!'+closer+join(lines,C_lineBreakChar)+closer);
        setLength(lines,0);
        closer:=#0;
        exit(result);
      end;
    end;
    while (inputLocation.column<=length(line)) and
          (line[inputLocation.column] in [' ',C_lineBreakChar,C_tabChar,C_carriageReturnChar]) do inc(inputLocation.column);
    result^.location:=inputLocation;
    if length(line)<inputLocation.column then begin
      recycler^.disposeToken(result);
      exit(nil);
    end;
    case line[inputLocation.column] of
      '0'..'9': begin
        result^.data:=parseNumber(line,inputLocation.column, false,recycler, parsedLength);
        if parsedLength<=0 then begin
                                  fail('Cannot parse numeric literal '+line);
                                  recycler^.disposeToken(result);
                                  exit(nil);
                                end
                           else result^.tokType:=tt_literal;
      end;
      #194: if (length(line)>inputLocation.column) and (line[inputLocation.column+1] in [#178,#179]) then begin
        if line[inputLocation.column+1]=#178
        then apply(tt_pow2)
        else apply(tt_pow3);
      end else exitFailing;
      '"','''','#': begin
        stringValue:=unescapeString(line,inputLocation.column,parsedLength);
        if parsedLength=0 then begin
          parsedLength:=1;
          while (parsedLength+inputLocation.column<=length(line)) and not(line[parsedLength+inputLocation.column] in [C_lineBreakChar,C_carriageReturnChar]) do inc(parsedLength);
          id:=copy(line,inputLocation.column+length(BLOCK_COMMENT_DELIMITER),parsedLength-1);
          if (length(line)>=parsedLength+inputLocation.column) and (line[parsedLength+inputLocation.column]='#') then inc(parsedLength);
          handleComment(id,BLOCK_COMMENT_DELIMITER);
        end else begin
          result^.tokType:=tt_literal;
          result^.data:=recycler^.newStringLiteral(stringValue,copy(line,inputLocation.column,parsedLength));
        end;
        stringValue:='';
      end;
      '$': begin
        result^.txt:=leadingId;
        result^.tokType:=tt_parameterIdentifier;
      end;
      'a'..'z','A'..'Z','_':
      if isFormatString then begin
        stringValue:=unescapeString(line,inputLocation.column+1,parsedLength);
        inc(parsedLength); //...because we added one more character before the string
        result^.tokType:=tt_formatString;
        result^.data:=recycler^.newStringLiteral(stringValue,copy(line,inputLocation.column,parsedLength));
      end else begin
        result^.txt:=leadingId;
        result^.tokType:=tt_identifier;
        for tt:=low(T_tokenType) to high(T_tokenType) do
        if result^.txt=C_tokenDefaultId[tt] then result^.tokType:=tt;
        if result^.tokType=tt_identifier then begin
          if      result^.txt=LITERAL_BOOL_TEXT[true]  then begin result^.tokType:=tt_literal; result^.data:=newBoolLiteral(true);     end
          else if result^.txt=LITERAL_BOOL_TEXT[false] then begin result^.tokType:=tt_literal; result^.data:=newBoolLiteral(false);    end
          else if result^.txt=LITERAL_NAN_TEXT         then begin result^.tokType:=tt_literal; result^.data:=recycler^.newRealLiteral(Nan);      end
          else if result^.txt=LITERAL_INF_TEXT         then begin result^.tokType:=tt_literal; result^.data:=recycler^.newRealLiteral(infinity); end
          else if result^.txt=LITERAL_TEXT_VOID        then begin result^.tokType:=tt_literal; result^.data:=newVoidLiteral;                 end
          else if result^.txt=LITERAL_TEXT_END_OF_GENERATOR then begin result^.tokType:=tt_literal; result^.data:=newGeneratorClosedLiteral; end
          else if result^.txt=ALTERNATIVE_NOT_TEXT          then result^.tokType:=tt_unaryOpNegate
          else begin
            result^.data:=associatedPackage;
            for tc in T_typeCheck do if result^.txt=C_typeCheckInfo[tc].name then begin
              result^.tokType:=tt_type;
              result^.setTypeCheck(tc);
            end;
            if result^.tokType=tt_identifier then for md in T_modifier do if result^.txt=C_modifierInfo[md].name then result^.setModifier(md);
          end;
        end;
      end;
      '/': if startsWith(COMMENT_PREFIX) then begin //comments
        parsedLength:=2;
        while (parsedLength+inputLocation.column<=length(line)) and not(line[parsedLength+inputLocation.column] in [C_lineBreakChar,C_carriageReturnChar]) do inc(parsedLength);
        handleComment(copy(line,inputLocation.column+length(COMMENT_PREFIX),parsedLength),COMMENT_PREFIX);
      end else if startsWith(tt_mut_assignDiv) then apply(tt_mut_assignDiv)
                                               else apply(tt_operatorDivReal);
      ':': if startsWith(tt_assign)            then apply(tt_assign)
      else if startsWith(tt_pseudoFuncPointer) then apply(tt_pseudoFuncPointer)
      else apply(tt_iifElse);
      '.': if startsWith(tt_each)                then apply(tt_each) else
           if startsWith(tt_parallelEach)        then apply(tt_parallelEach) else
           if startsWith(tt_agg)                 then apply(tt_agg) else
           if startsWith(tt_optionalParameters)  then apply(tt_optionalParameters) else
           if startsWith(tt_separatorCnt)        then apply(tt_separatorCnt)
                                                 else apply(tt_ponFlipper);
      ';':                                            apply(tt_semicolon);
      '}':                                            apply(tt_expBraceClose);
      '{':                                            apply(tt_expBraceOpen);
      '^':                                            apply(tt_operatorPot);
      ']':                                            apply(tt_listBraceClose);
      '[':                                            apply(tt_listBraceOpen);
      '?':                                            apply(tt_iifCheck);
      ',':                                            apply(tt_separatorComma);
      '@': if firstInLine then begin
             parsedLength:=1;
             while (parsedLength+inputLocation.column<=length(line)) and not(line[parsedLength+inputLocation.column] in [C_lineBreakChar,C_carriageReturnChar]) do inc(parsedLength);
             handleComment(copy(line,inputLocation.column,parsedLength),'@');
          end else apply(tt_listToParameterList);
      ')':                                            apply(tt_braceClose);
      '(':                                            apply(tt_braceOpen);
      '|': if startsWith(tt_mut_assignAppend)    then apply(tt_mut_assignAppend) else
           if startsWith(tt_mut_assignAppendAlt) then apply(tt_mut_assignAppendAlt) else
           if startsWith(tt_operatorConcatAlt)   then apply(tt_operatorConcatAlt)
                                                 else apply(tt_operatorConcat);
      '+': if startsWith(tt_mut_assignPlus)      then apply(tt_mut_assignPlus)
                                                 else apply(tt_operatorPlus);
      '&': if startsWith(tt_mut_assignStrConcat) then apply(tt_mut_assignStrConcat)
                                                 else apply(tt_operatorStrConcat);
      '-': if startsWith(tt_declare)             then apply(tt_declare) else
           if startsWith(tt_mut_assignMinus)     then apply(tt_mut_assignMinus)
                                                 else apply(tt_operatorMinus);
      '*': if startsWith('**')                   then apply(2,tt_operatorPot) else
           if startsWith(tt_mut_assignMult)      then apply(tt_mut_assignMult)
                                                 else apply(tt_operatorMult);
      '>': if startsWith(tt_comparatorGeq)       then apply(tt_comparatorGeq) else
           if startsWith(tt_mut_assignDrop)      then apply(tt_mut_assignDrop)
                                                 else apply(tt_comparatorGrt);
      '=': if startsWith(tt_comparatorListEq)    then apply(tt_comparatorListEq) else
           if startsWith(tt_separatorMapItem)    then apply(tt_separatorMapItem)
                                                 else apply(tt_comparatorEq);
      '<': if startsWith(tt_comparatorNeq)       then apply(tt_comparatorNeq) else
           if startsWith(tt_comparatorLeq)       then apply(tt_comparatorLeq)
                                                 else apply(tt_comparatorLss);
      '!': if startsWith('!=')                   then apply(2,tt_comparatorNeq)
                                                 else apply(tt_unaryOpNegate);
      else exitFailing;
    end;
    if parsedLength>0 then inc(inputLocation.column,parsedLength);
  end;

FUNCTION T_abstractLexer.fetchNext(CONST messages:P_messages; CONST recycler:P_recycler): boolean;
  PROCEDURE appendToken(CONST tok:P_token); inline;
    begin
      if tok=nil then exit;
      if (tok^.tokType=tt_intrinsicRule) and
         ((tok^.data=pointer(BUILTIN_MYPATH)) or
          (tok^.data=pointer(BUILTIN_ASSERTUNIQUEINSTANCE)) or
          (tok^.data=pointer(BUILTIN_INSPECT)) or
          (tok^.data=pointer(BUILTIN_WRITE_ALL_DATA_STORES)) or
          (tok^.data=pointer(BUILTIN_WRITE_DATA_STORES)))
      then tok^.location.package:=associatedPackage;
      tokenQueue.append(tok);
    end;

  VAR nextToken:P_token;
      n:array[1..3] of P_token;
      id:string;
      inFound:boolean;
  begin
    nextToken:=fetch(messages,recycler);
    if nextToken=nil then exit(false);
    while nextToken<>nil do case nextToken^.tokType of
      tt_do: begin // do | parallel -> do parallel
        n[1]:=fetch(messages,recycler);
        if (n[1]<>nil) and (n[1]^.tokType = tt_identifier) and (n[1]^.txt='parallel') then begin
          nextToken^.setDoType(dt_for_related_do_parallel);
          recycler^.disposeToken(n[1]);
          n[1]:=nil;
        end;
        appendToken(nextToken);
        nextToken:=n[1];
      end;
      tt_unaryOpNegate: begin // not | in -> not in
        n[1]:=fetch(messages,recycler);
        if (n[1]<>nil) and (n[1]^.tokType=tt_operatorIn) then begin
          nextToken^.tokType:=tt_operatorNotIn;
          recycler^.disposeToken(n[1]);
          n[1]:=nil;
        end;
        appendToken(nextToken);
        nextToken:=n[1];
      end;
      tt_iifElse: begin
        // : may be part of # ? # : # or part of a pattern like (x:Type)
        n[1]:=fetch(messages,recycler);
        if n[1]<>nil then begin
          if (n[1]^.tokType=tt_identifier) and (associatedPackage<>nil)
          then associatedPackage^.resolveId(n[1]^,nil);

          if (nextToken^.tokType=tt_iifElse) and (n[1]<>nil) and (n[1]^.tokType in [tt_type,tt_customType]) then begin
            if n[1]^.tokType=tt_type
            then nextToken^.tokType:=tt_typeCheck
            else nextToken^.tokType:=tt_customTypeCheck;
            nextToken^.txt    :=n[1]^.txt;
            nextToken^.data   :=n[1]^.data;
            recycler^.disposeToken(n[1]);
          end else begin
            appendToken(nextToken);
            nextToken:=n[1]; //=> repeat case distinction
          end;
        end else begin
          appendToken(nextToken);
          nextToken:=nil;
        end;
      end;
      tt_identifier: if (associatedPackage<>nil) then begin
        if (associatedPackage^.isImportedOrBuiltinPackage(nextToken^.txt)) then begin
          n[1]:=fetch(messages,recycler);
          if (n[1]<>nil) and (n[1]^.tokType=tt_ponFlipper) then begin
            n[2]:=fetch(messages,recycler);
            if (n[2]<>nil) and (n[2]^.tokType=tt_identifier) then begin
              id:=nextToken^.txt;
              nextToken^.txt:=nextToken^.txt+ID_QUALIFY_CHARACTER+n[2]^.txt;
              if associatedPackage^.resolveId(nextToken^,nil) then begin
                recycler^.disposeToken(n[1]);
                recycler^.disposeToken(n[2]);
                appendToken(nextToken);
                nextToken:=nil;
              end else begin
                //Could not interpret A.B as qualified identifier - interpreting as "PON" instead
                nextToken^.txt:=id;
                associatedPackage^.resolveId(nextToken^,nil);
                appendToken(nextToken);
                appendToken(n[1]);
                nextToken:=n[2];
                //=> repeat case distinction
              end;
            end else begin
              associatedPackage^.resolveId(nextToken^,nil);
              appendToken(nextToken);
              appendToken(n[1]);
              nextToken:=n[2];
              //=> repeat case distinction
            end;
          end else begin
            associatedPackage^.resolveId(nextToken^,nil);
            appendToken(nextToken);
            nextToken:=n[1];
            //=> repeat case distinction
          end;
        end else begin
          associatedPackage^.resolveId(nextToken^,nil);
          appendToken(nextToken);
          nextToken:=nil;
        end;
      end else begin
        appendToken(nextToken);
        nextToken:=nil;
      end;
      tt_each,tt_parallelEach: begin
        n[1]:=fetch(messages,recycler);
        n[2]:=fetch(messages,recycler);
        n[3]:=fetch(messages,recycler);
        if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) and
           (n[2]<>nil) and (n[2]^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule]) and
           (n[3]<>nil) and (n[3]^.tokType=tt_separatorComma) then begin
          nextToken^.txt:=n[2]^.txt;
          nextToken^.data:=nil;
          recycler^.disposeToken(n[1]);
          recycler^.disposeToken(n[2]);
          recycler^.disposeToken(n[3]);
          appendToken(nextToken);
          nextToken:=nil;
          inFound:=true;
        end else if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) and
          (n[2]<>nil) and (n[2]^.tokType=tt_braceOpen) and
          (n[3]<>nil) and (n[3]^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule])
        then begin
          recycler^.disposeToken(n[1]);
          recycler^.disposeToken(n[2]);
          n[1]:=n[3];
          n[2]:=fetch(messages,recycler);
          n[3]:=fetch(messages,recycler);
          inFound:=false;
          nextToken^.txt:='';
          while not(inFound) and (n[1]<>nil) and (n[2]<>nil) and (n[3]<>nil) and
             (n[1]^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule]) and
             ((n[2]^.tokType=tt_braceClose) and (n[3]^.tokType=tt_separatorComma) or
              (n[2]^.tokType=tt_separatorComma) and (n[3]^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule]))
          do begin
            if n[2]^.tokType=tt_braceClose then inFound:=true;
            if nextToken^.txt=''
            then nextToken^.txt:=    n[1]^.txt
            else nextToken^.txt+=','+n[1]^.txt;
            recycler^.disposeToken(n[1]);
            recycler^.disposeToken(n[2]);
            n[1]:=n[3];
            if inFound then begin
              nextToken^.data:=nil;
              appendToken(nextToken);
              nextToken:=nil;
              recycler^.disposeToken(n[1]);
            end else begin
              n[2]:=fetch(messages,recycler);
              n[3]:=fetch(messages,recycler);
            end;
          end;
        end else inFound:=false;
        if not inFound then begin
          messages^.raiseSimpleError('Invalid (p)Each construct. First argument must be an identifier. At least two arguments must be given.',nextToken^.location);
          appendToken(nextToken);
          if n[1]<>nil then appendToken(n[1]);
          if n[2]<>nil then appendToken(n[2]);
          if n[3]<>nil then appendToken(n[3]);
          nextToken:=nil;
        end;
      end;
      tt_for:begin
        inFound:=false;
        n[1]:=fetch(messages,recycler);
        n[2]:=fetch(messages,recycler);
        while not(inFound) and
           (n[1]<>nil) and (n[1]^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule]) and
           (n[2]<>nil) and (n[2]^.tokType in [tt_operatorIn,tt_separatorComma])
        do begin
          if (n[2]^.tokType=tt_operatorIn) then begin
            inFound:=true;
            {$ifdef fullVersion}
            if (callAndIdInfos<>nil) and (associatedPackage^.isMain) and (usecase=lxu_assistance) then begin
              callAndIdInfos^.addTokenRelation(nextToken,n[2]^.location);
            end;
            {$endif}
          end;
          if nextToken^.txt='for'
          then nextToken^.txt:=    n[1]^.txt
          else nextToken^.txt+=','+n[1]^.txt;

          recycler^.disposeToken(n[1]);
          recycler^.disposeToken(n[2]);

          if inFound then begin
            nextToken^.data:=nil;
            appendToken(nextToken);
            nextToken:=nil;
          end else begin
            n[1]:=fetch(messages,recycler);
            n[2]:=fetch(messages,recycler);
          end;
        end;
        if not inFound then begin
          messages^.raiseSimpleError('Invalid for construct. Syntax is: for <id> in <iterable> do [parallel] ...',nextToken^.location);
          appendToken(nextToken);
          appendToken(n[1]);
          nextToken:=n[2];
        end;
      end;
      tt_agg: begin
        n[1]:=fetch(messages,recycler);
        if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) then begin
          nextToken^.tokType:=tt_each;
          nextToken^.txt:='';
          nextToken^.data:=nil;
        end else messages^.raiseSimpleError('Invalid agg construct.',nextToken^.location);
        recycler^.disposeToken(n[1]);
        appendToken(nextToken);
        nextToken:=nil;
      end;
      tt_braceClose: begin
        n[1]:=fetch(messages,recycler);
        if (n[1]<>nil) then begin
          if (n[1]^.tokType=tt_declare) then begin
            nextToken^.tokType:=tt_endOfPatternDeclare;
            recycler^.disposeToken(n[1]);
            appendToken(nextToken);
            nextToken:=nil;
          end else if (n[1]^.tokType=tt_assign) then begin
            nextToken^.tokType:=tt_endOfPatternAssign;
            recycler^.disposeToken(n[1]);
            appendToken(nextToken);
            nextToken:=nil;
          end else begin
            appendToken(nextToken);
            nextToken:=n[1]; //Repeat case distinction
          end;
        end else begin
          appendToken(nextToken);
          nextToken:=nil;
        end;
      end;
      else begin
        appendToken(nextToken);
        nextToken:=nil;
      end;
    end;
    result:=true;
  end;

PROCEDURE T_abstractLexer.resetTemp;
  begin
    nextStatement.attributes:=C_EMPTY_STRING_ARRAY;
    nextStatement.comments  :=C_EMPTY_STRING_ARRAY;
    nextStatement.token     :=EMPTY_TOKEN_RANGE;
    nextStatement.assignmentToken:=nil;
  end;

PROCEDURE T_abstractPackage.clearCustomOperators;
  VAR op:T_tokenType;
  begin
    for op:=low(T_customOperatorArray) to high(T_customOperatorArray) do customOperatorRules[op]:=nil;
  end;

CONSTRUCTOR T_abstractPackage.create(CONST provider: P_codeProvider);
  begin
    codeProvider:=provider;
    readyForCodeState:=0;
    clearCustomOperators;
  end;

CONSTRUCTOR T_extendedPackage.create(CONST provider: P_codeProvider;
  CONST extender_: P_abstractPackage);
  begin
    inherited create(provider);
    extender:=extender_;
  end;

DESTRUCTOR T_abstractPackage.destroy;
  begin
    try
      if codeProvider^.disposeOnPackageDestruction then dispose(codeProvider,destroy);
    except end;
    codeProvider:=nil;
  end;

FUNCTION T_abstractPackage.isImportedOrBuiltinPackage(CONST id: string): boolean;
  VAR ns:T_namespace;
  begin
    for ns in T_namespace do if C_namespaceString[ns]=id then exit(true);
    result:=false;
  end;

FUNCTION T_extendedPackage.isImportedOrBuiltinPackage(CONST id: string): boolean;
  begin
    result:=extender^.isImportedOrBuiltinPackage(id);
  end;

FUNCTION T_abstractPackage.resolveId(VAR token: T_token; CONST adaptersOrNil: P_messages):boolean;
  VAR intrinsicFuncPtr:P_intFuncCallback;
      ruleId:T_idString;
  begin
    ruleId   :=token.txt;
    if builtinFunctionMap.containsFunctionForId(ruleId,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRule;
      token.data:=intrinsicFuncPtr;
      exit(true);
    end;
    if adaptersOrNil<>nil then adaptersOrNil^.raiseSimpleError('Cannot resolve ID "'+token.txt+'"',token.location);
    result:=false;
  end;

FUNCTION T_abstractPackage.resolveLocationForStackTrace(CONST location:T_tokenLocation):string;
  VAR lines: T_arrayOfString;
  begin
    if location.package=@self then begin
      lines:=codeProvider^.getLines;
      if (location.line>=1) and (location.line<=length(lines))
      then begin
        result:=lines[location.line-1];
        if (location.column>=1) and (location.column<length(result)) then begin
          result:=copy(result,location.column,maxLongint);
          if location.column>=5 then result:='... '+result;
        end;
      end else exit('(?)');
    end else result:='(?)';
  end;

FUNCTION T_extendedPackage.resolveId(VAR token: T_token; CONST adaptersOrNil: P_messages):boolean;
  begin
    result:=extender^.resolveId(token,adaptersOrNil);
  end;

FUNCTION T_abstractPackage.inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; CONST functionCallInfos:P_callAndIdInfos{$endif}):P_mapLiteral;
  begin
    result:=recycler^.newMapLiteral(0);
  end;

FUNCTION T_abstractPackage.isMain:boolean;
  begin
    result:=true;
  end;

FUNCTION T_extendedPackage.inspect(CONST includeRulePointer: boolean; CONST context: P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; CONST functionCallInfos:P_callAndIdInfos{$endif}): P_mapLiteral;
  begin
    result:=extender^.inspect(includeRulePointer,context,recycler{$ifdef fullVersion},functionCallInfos{$endif});
  end;

FUNCTION T_abstractPackage.replaceCodeProvider(CONST newProvider: P_codeProvider):boolean;
  begin
    if (codeProvider=newProvider) then exit(false);
    if (codeProvider<>nil) and (codeProvider^.disposeOnPackageDestruction) then dispose(codeProvider,destroy);
    codeProvider:=newProvider;
    readyForCodeState:=0;
    if (codeProvider=nil) then new(P_blankCodeProvider(codeProvider),create);
    result:=true;
  end;

FUNCTION T_abstractPackage.codeChanged: boolean;                       begin result:=readyForCodeState<>codeProvider^.stateHash; end;
PROCEDURE T_abstractPackage.logReady(CONST stateHashAtLoad:T_hashInt); begin readyForCodeState:=stateHashAtLoad;                 end;
FUNCTION T_abstractPackage.getId: T_idString;                          begin result:=codeProvider^.id;                           end;
FUNCTION T_abstractPackage.getPath: ansistring;                        begin result:=codeProvider^.getPath;                      end;

FUNCTION T_abstractPackage.literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_recycler):string;
  begin
    if (L^.literalType=lt_string)
    then result:=P_stringLiteral(L)^.value
    else result:=L^.toString();
  end;

FUNCTION T_abstractPackage.getTypeMap:T_typeMap;
  begin
    result.create();
  end;

INITIALIZATION
  BLANK_ABSTRACT_PACKAGE.create(newVirtualFileCodeProvider('',C_EMPTY_STRING_ARRAY));
  MNH_PSEUDO_PACKAGE.create();
  {$ifdef fullVersion}
  messageFormatting.mnhSysPseudopackagePrefix:=MNH_PSEUDO_PACKAGE.getPath;
  rawTokenizeCallback:=@tokenizeAllReturningRawTokens;
  {$endif}

FINALIZATION
  BLANK_ABSTRACT_PACKAGE.destroy;
  MNH_PSEUDO_PACKAGE.destroy;

end.
