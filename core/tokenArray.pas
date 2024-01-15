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
      PROCEDURE resolveId(VAR token:T_token; CONST adaptersOrNil:P_messages); virtual;
      FUNCTION getTypeMap:T_typeMap; virtual;
      FUNCTION literalToString(CONST L:P_literal; {$WARN 5024 OFF}CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_recycler):string; virtual;
      {$ifdef fullVersion}
      FUNCTION getImport({$WARN 5024 OFF}CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION getExtended(CONST idOrPath:string):P_abstractPackage; virtual;
      {$endif}
      FUNCTION inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_callAndIdInfos{$endif}):P_mapLiteral; virtual;
  end;

  P_extendedPackage=^T_extendedPackage;
  T_extendedPackage=object(T_abstractPackage)
    private
      extender:P_abstractPackage;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider; CONST extender_:P_abstractPackage);
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
      PROCEDURE resolveId(VAR token:T_token; CONST adaptersOrNil:P_messages); virtual;
      FUNCTION inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_callAndIdInfos{$endif}):P_mapLiteral; virtual;
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
  end;

  {$ifdef fullVersion}
  T_usageInfo=record
    targetLocation,
    referencedAt:T_searchTokenLocation;
  end;

  T_localIdInfo=record
                  name:string;
                  validFrom,validUntil:T_tokenLocation;
                  tokenType:T_tokenType;
                end;

  T_callAndIdInfos=object
    private
      usageInfoFill:longint;
      usedBuiltins :T_setOfPointer;
      usageInfos:array of T_usageInfo;

      localIdInfos: array of T_localIdInfo;
      blobLines:array of record
        lineIndex:longint;
        blobCloser:char;
      end;
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
      FUNCTION  isPackageReferenced(CONST packagePath:string):boolean;
      FUNCTION  isEmpty:boolean;
      PROCEDURE includeUsages(CONST other:P_callAndIdInfos);

      FUNCTION localTypeOf(CONST id:T_idString; CONST line,col:longint; OUT declaredAt:T_tokenLocation):T_tokenType;
      FUNCTION allLocalIdsAt(CONST line,col:longint):T_arrayOfString;
      PROCEDURE addLocalIdInfo(CONST id:T_idString; CONST validFrom,validUntil:T_tokenLocation; CONST typ:T_tokenType);
      PROCEDURE markBlobLine(CONST lineIndex:longint; CONST closer:char);
      FUNCTION getBlobCloserOrZero(CONST lineIndex:longint):char;
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
      FUNCTION renameInLine(VAR line:string; CONST referencedLocation:T_searchTokenLocation; CONST oldName:string; newName:string):boolean;
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
      callAndIdInfos:P_callAndIdInfos;
      {$endif}
      FUNCTION getToken(CONST line: ansistring; VAR inputLocation: T_tokenLocation; CONST messages:P_messages; CONST recycler:P_recycler; CONST lexingStyle:T_lexingStyle): P_token;
      FUNCTION fetchNext(                                                           CONST messages:P_messages; CONST recycler:P_recycler; CONST lexingStyle:T_lexingStyle):boolean;
      FUNCTION fetch(CONST messages:P_messages; CONST recycler:P_recycler; CONST lexingStyle:T_lexingStyle):P_token; virtual; abstract;
    public
      CONSTRUCTOR create(CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;
      FUNCTION getNextStatement(CONST messages:P_messages; CONST recycler:P_recycler):T_enhancedStatement;
  end;

  { T_singleStringLexer }

  T_singleStringLexer=object(T_abstractLexer)
    protected
      text:string;
      inputLocation:T_tokenLocation;
      columnOffset:longint;
      FUNCTION fetch(CONST messages:P_messages; CONST recycler:P_recycler; CONST lexingStyle:T_lexingStyle):P_token; virtual;
    public
      CONSTRUCTOR create(CONST inputString:string; CONST parseLocation:T_tokenLocation;
                         CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;
      {$ifdef fullVersion}
      FUNCTION getEnhancedTokens(CONST idInfos:P_callAndIdInfos):T_enhancedTokens;
      {$endif}
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
      FUNCTION fetch(CONST messages:P_messages; CONST recycler:P_recycler; CONST lexingStyle:T_lexingStyle):P_token; virtual;
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
      FUNCTION fetch(CONST messages:P_messages; CONST recycler:P_recycler; CONST lexingStyle:T_lexingStyle):P_token; virtual;
    public
      CONSTRUCTOR create(CONST input_:T_arrayOfString; CONST parseLocation_:T_tokenLocation;
                         CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      CONSTRUCTOR createForExtendedPackage(CONST importWrapper:P_extendedPackage;
                         CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      CONSTRUCTOR createForPackageParsing(CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
      DESTRUCTOR destroy; virtual;
  end;

PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_abstractPackage; CONST context:P_context; CONST recycler:P_recycler{$ifdef fullVersion};CONST callAndIdInfos:P_callAndIdInfos=nil{$endif});
FUNCTION isOperatorName(CONST id:T_idString):boolean;
VAR BLANK_ABSTRACT_PACKAGE:T_abstractPackage;
    MNH_PSEUDO_PACKAGE:T_mnhSystemPseudoPackage;
    BUILTIN_WRITE_DATA_STORES,
    BUILTIN_WRITE_ALL_DATA_STORES,
    BUILTIN_INSPECT:P_intFuncCallback;
IMPLEMENTATION
USES sysutils,{$ifdef fullVersion}strutils,messageFormatting,{$endif}math,subrules,profiling,typinfo,patterns,funcs_ipc;

TYPE
T_scopeType=(sc_block,sc_each,sc_bracketOnly);
T_addIdResult=(air_ok,air_reintroduce,air_notInBlock);

T_idStack=object
  scope:array of record
    scopeType:T_scopeType;
    scopeStartToken:P_token;
    ids:array of record name:T_idString; used:boolean; location:T_tokenLocation; idType:T_tokenType; end;
  end;
  bracketLevel:longint;
  {$ifdef fullVersion}
  localIdInfos:P_callAndIdInfos;
  {$endif}
  lastWasLocalModifier:boolean;
  prevToken:P_token;

  CONSTRUCTOR create({$ifdef fullVersion}CONST info:P_callAndIdInfos{$endif});
  DESTRUCTOR destroy;
  PROCEDURE clear;
  PROCEDURE applyToken(CONST token:P_token; CONST messages:P_messages);

  PROCEDURE scopePop(CONST adapters:P_messages; CONST location:T_tokenLocation; CONST closeToken:P_token; CONST warnAboutUnused:boolean);
  {$ifdef fullVersion}
  PROCEDURE popRemaining;
  {$endif}
  FUNCTION oneAboveBottom:boolean;
  FUNCTION scopeBottom:boolean;
  FUNCTION addId(CONST id:T_idString; CONST location:T_tokenLocation; CONST idType:T_tokenType):T_addIdResult;
  FUNCTION hasId(CONST id:T_idString; OUT idType:T_tokenType; OUT idLoc:T_tokenLocation):boolean;
end;

FUNCTION T_linesLexer.fetch(CONST messages: P_messages; CONST recycler:P_recycler; CONST lexingStyle: T_lexingStyle): P_token;
  begin
    result:=nil;
    while (result=nil) and (inputIndex<length(input)) do begin
      result:=getToken(input[inputIndex],inputLocation,messages,recycler,lexingStyle);
      if (result=nil) then begin
        inc(inputIndex);
        inc(inputLocation.line);
        inputLocation.column:=1;
      end else if lexingStyle<>ls_retainAll then case result^.tokType of
        tt_EOL: begin
          recycler^.disposeToken(result);
          result:=nil;
        end;
        tt_docComment: if lexingStyle<>ls_retainComments then begin
          myGenerics.append(nextStatement.comments ,result^.txt);
          recycler^.disposeToken(result);
          result:=nil;
        end;
        tt_attributeComment: if lexingStyle<>ls_retainComments then begin
          if (result^.txt<>'') then myGenerics.append(nextStatement.attributes,result^.txt);
          recycler^.disposeToken(result);
          result:=nil;
        end;
      end;
    end;
  end;

CONSTRUCTOR T_linesLexer.create(CONST input_: T_arrayOfString; CONST parseLocation_: T_tokenLocation;
                                CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    inherited create(package{$ifdef fullVersion},callAndIdInfos_{$endif});
    input:=input_;
    inputIndex:=0;
    inputLocation:=parseLocation_;
    inputLocation.column:=1;
  end;

CONSTRUCTOR T_linesLexer.createForExtendedPackage(CONST importWrapper: P_extendedPackage;
                                                  CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    create(importWrapper^.getCodeProvider^.getLines,packageTokenLocation(importWrapper),package{$ifdef fullVersion},callAndIdInfos_{$endif});
  end;

CONSTRUCTOR T_linesLexer.createForPackageParsing(CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    create(package^.getCodeProvider^.getLines,packageTokenLocation(package),package{$ifdef fullVersion},callAndIdInfos_{$endif});
  end;

DESTRUCTOR T_linesLexer.destroy;
  begin
    inherited;
    setLength(input,0);
  end;

{ T_variableLexer }

FUNCTION T_variableLexer.fetch(CONST messages: P_messages; CONST recycler:P_recycler; CONST lexingStyle: T_lexingStyle): P_token;
  begin
    result:=nil;
    while (result=nil) do begin
      result:=getToken(textToParse,inputLocation,messages,recycler,lexingStyle);
      if result=nil then begin
        inc(dataIdx);
        if dataIdx>=length(data) then exit(nil);
        if data[dataIdx]^.literalType=lt_string then begin
          textToParse:=P_stringLiteral(data[dataIdx])^.value;
          inputLocation.column:=1;
        end else begin
          result:=recycler^.newToken(parseLocation,'',tt_literal,data[dataIdx]^.rereferenced);
        end;
      end else if (lexingStyle<>ls_retainAll) then case result^.tokType of
        tt_EOL: begin
          recycler^.disposeToken(result);
          result:=nil;
        end;
        tt_docComment: if lexingStyle<>ls_retainComments then begin
          myGenerics.append(nextStatement.comments ,result^.txt);
          recycler^.disposeToken(result);
          result:=nil;
        end;
        tt_attributeComment: if lexingStyle<>ls_retainComments then begin
          if (result^.txt<>'') then myGenerics.append(nextStatement.attributes,result^.txt);
          recycler^.disposeToken(result);
          result:=nil;
        end;
      end;
    end;
    if result<>nil then result^.location:=parseLocation;
  end;

CONSTRUCTOR T_variableLexer.create(CONST input: T_arrayOfLiteral; CONST parseLocation_: T_tokenLocation; CONST package: P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    inherited create(package{$ifdef fullVersion},callAndIdInfos_{$endif});
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

FUNCTION T_singleStringLexer.fetch(CONST messages: P_messages; CONST recycler:P_recycler; CONST lexingStyle: T_lexingStyle): P_token;
  begin
    result:=nil;
    while (result=nil) and (inputLocation.column<=length(text)) do begin
      result:=getToken(text,inputLocation,messages,recycler,lexingStyle);
      if (result<>nil) and (lexingStyle<>ls_retainAll) then case result^.tokType of
        tt_EOL: begin
          recycler^.disposeToken(result);
          result:=nil;
        end;
        tt_docComment: if lexingStyle<>ls_retainComments then begin
          myGenerics.append(nextStatement.comments ,result^.txt);
          recycler^.disposeToken(result);
          result:=nil;
        end;
        tt_attributeComment: if lexingStyle<>ls_retainComments then begin
          if (result^.txt<>'') then myGenerics.append(nextStatement.attributes,result^.txt);
          recycler^.disposeToken(result);
          result:=nil;
        end;
      end;
    end;
    if result<>nil then inc(result^.location.column,columnOffset);
  end;

CONSTRUCTOR T_singleStringLexer.create(CONST inputString: string; CONST parseLocation: T_tokenLocation;
                                       CONST package:P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    inherited create(package{$ifdef fullVersion},callAndIdInfos_{$endif});
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

{$ifdef fullVersion}
FUNCTION T_singleStringLexer.getEnhancedTokens(CONST idInfos: P_callAndIdInfos): T_enhancedTokens;
  VAR adapters:T_messagesDummy;
      recycler:P_recycler;
      tokenToProcess:P_token;
  begin
    recycler:=newRecycler;
    blob.closer:=idInfos^.getBlobCloserOrZero(inputLocation.line);

    adapters.createDummy;
    while fetchNext(@adapters,recycler,ls_retainComments) do begin end;
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
    repeat until not(lexer.fetchNext(@adapters,recycler,ls_retainAll));
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

CONSTRUCTOR T_abstractLexer.create(CONST package: P_abstractPackage {$ifdef fullVersion};CONST callAndIdInfos_:P_callAndIdInfos=nil{$endif});
  begin
    associatedPackage:=package;

    {$ifdef fullVersion}
    callAndIdInfos:=callAndIdInfos_;
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

FUNCTION T_abstractLexer.getNextStatement(CONST messages: P_messages; CONST recycler:P_recycler): T_enhancedStatement;
  VAR localIdStack:T_idStack;
      earlierSuppressedUnusedAttribute:boolean=false;
  FUNCTION hasSuppressedUnusedAttribute:boolean;
    VAR s:string;
    begin
      if earlierSuppressedUnusedAttribute then exit(true);
      for s in nextStatement.attributes do earlierSuppressedUnusedAttribute:=earlierSuppressedUnusedAttribute or startsWith(s,SUPPRESS_UNUSED_WARNING_ATTRIBUTE);
      result:=earlierSuppressedUnusedAttribute;
    end;

  PROCEDURE appendTokenToResult(CONST tok:P_token);
    begin
      if nextStatement.token.first=nil
      then nextStatement.token.first     :=tok
      else nextStatement.token.last^.next:=tok;
      nextStatement.token.last :=tok;
      localIdStack.applyToken(tok,messages);
    end;

  VAR nextToProcess:P_token=nil;
  begin
    localIdStack.create({$ifdef fullVersion}callAndIdInfos{$endif});
    while tokenQueue.hasNext or (fetchNext(messages,recycler,ls_onlyInterpretable) and tokenQueue.hasNext) do begin
      nextToProcess:=tokenQueue.next;
      if nextToProcess^.tokType=tt_semicolon then begin
        if localIdStack.scopeBottom then begin
          if nextStatement.token.first=nil then messages^.raiseSimpleError('Empty statement!',nextToProcess^.location);
          recycler^.disposeToken(nextToProcess);
          result:=nextStatement;
          if not(messages^.continueEvaluation) then recycler^.cascadeDisposeToken(result.token.first);
          resetTemp;
          localIdStack.destroy;
          exit;
        end else appendTokenToResult(nextToProcess)
      end else appendTokenToResult(nextToProcess);
    end;
    result:=nextStatement;
    if not(messages^.continueEvaluation) then begin
      {$ifdef fullVersion}
      localIdStack.popRemaining;
      {$endif}
      recycler^.cascadeDisposeToken(result.token.first);
    end;
    resetTemp;
    while not(localIdStack.scopeBottom) do localIdStack.scopePop(messages,localIdStack.prevToken^.location,nil,not(hasSuppressedUnusedAttribute));
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
    lastWasLocalModifier:=false;
    for i:=0 to length(scope)-1 do setLength(scope[i].ids,0);
    setLength(scope,0);
    bracketLevel:=0;
    prevToken:=nil;
  end;

PROCEDURE T_idStack.applyToken(CONST token:P_token; CONST messages:P_messages);
  PROCEDURE scopePush(CONST scopeType:T_scopeType);
    VAR newTopIdx:longint;
    begin
      newTopIdx:=length(scope);
      setLength(scope,newTopIdx+1);
      setLength(scope[newTopIdx].ids,0);
      scope[newTopIdx].scopeType:=scopeType;
      scope[newTopIdx].scopeStartToken:=token;
    end;

  VAR idType:T_tokenType;
      idLoc:T_tokenLocation;
  begin
    if (messages<>nil) and (prevToken<>nil) then begin
      if (prevToken^.tokType in [tt_beginBlock,tt_beginRule,tt_beginExpression,tt_each,tt_parallelEach,tt_agg,tt_list_constructor,tt_expBraceOpen,tt_iifCheck]) and
         (token    ^.tokType in [tt_endBlock  ,tt_endRule  ,tt_endExpression                                                     ,tt_expBraceClose,tt_iifElse]) then begin
        messages^.raiseSimpleError('Empty '+prevToken^.singleTokenToString+'-'+token^.singleTokenToString+' block',token^.location);
      end;
      if (prevToken^.tokType in [tt_separatorCnt,tt_separatorComma]) and (token^.tokType in C_closingBrackets) then begin
        messages^.raiseSimpleError('Missing element in '+prevToken^.singleTokenToString+'-separated list',token^.location);
      end;
    end;
    prevToken:=token;
    case token^.tokType of
      tt_beginBlock:
        scopePush(sc_block);
      tt_endBlock:
        scopePop(messages,token^.location,token,true);

      tt_braceOpen,tt_expBraceOpen,tt_listBraceOpen,tt_iifCheck:
        scopePush(sc_bracketOnly);
      tt_braceClose,tt_expBraceClose,tt_listBraceClose,tt_iifElse,
      tt_endOfPatternDeclare,tt_endOfPatternAssign:
        scopePop(messages,token^.location,token,true);

      tt_each,tt_parallelEach:begin
        scopePush(sc_each);
        addId(EACH_INDEX_IDENTIFIER,token^.location,tt_eachIndex);
        addId(token^.txt           ,token^.location,tt_eachParameter);
      end;
      tt_identifier,tt_userRule,tt_intrinsicRule,tt_globalVariable,tt_customType,tt_customTypeCheck:begin
        if lastWasLocalModifier then begin
          token^.tokType:=tt_blockLocalVariable;
          if messages<>nil then case addId(token^.txt,token^.location,tt_blockLocalVariable) of
            air_reintroduce: messages^.raiseSimpleError('Invalid re-introduction of local variable "'+token^.txt+'"',token^.location);
            air_notInBlock : messages^.raiseSimpleError('You can only declare local variables in begin-end-blocks',token^.location);
          end;
        end else if (hasId(token^.txt,idType,idLoc)) then begin
          token^.tokType:=idType;
          if idType=tt_eachIndex then token^.location:=idLoc;
        end;
      end;
    end;
    lastWasLocalModifier:=(token^.tokType=tt_modifier) and (token^.getModifier=modifier_local);
  end;

PROCEDURE T_idStack.scopePop(CONST adapters:P_messages; CONST location:T_tokenLocation; CONST closeToken:P_token; CONST warnAboutUnused:boolean);
  VAR topIdx:longint;
      i:longint;
  PROCEDURE raiseMismatchError;
    VAR message:string;
    begin
      if adapters=nil then exit;
      message:='Mismatch; '+scope[topIdx].scopeStartToken^.singleTokenToString+' closed by '+closeToken^.singleTokenToString;
      adapters^.raiseSimpleError(message,scope[topIdx].scopeStartToken^.location);
      adapters^.raiseSimpleError(message,                               location);
    end;

  begin
    topIdx:=length(scope)-1;
    if topIdx<0 then begin
      if adapters<>nil then adapters^.raiseSimpleError('Missing opening bracket for closing bracket',location);
      exit;
    end;
    if closeToken<>nil then case closeToken^.tokType of
      tt_braceClose: begin
        if not(scope[topIdx].scopeStartToken^.tokType in [tt_each,tt_parallelEach,tt_braceOpen])
        then raiseMismatchError;
      end;
      tt_listBraceClose: begin
        if scope[topIdx].scopeStartToken^.tokType<>tt_listBraceOpen
        then raiseMismatchError;
      end;
      tt_endOfPatternDeclare,tt_endOfPatternAssign: begin
        if   scope[topIdx].scopeStartToken^.tokType =tt_braceOpen
        then scope[topIdx].scopeStartToken^.tokType:=tt_startOfPattern
        else raiseMismatchError;
      end;
      tt_expBraceClose: begin
        if scope[topIdx].scopeStartToken^.tokType<>tt_expBraceOpen
        then raiseMismatchError;
      end;
      tt_iifElse: begin
        if scope[topIdx].scopeStartToken^.tokType<>tt_iifCheck
        then raiseMismatchError;
      end;
      tt_endBlock: begin
        if scope[topIdx].scopeStartToken^.tokType<>tt_beginBlock
        then raiseMismatchError;
      end;
      else raise Exception.create('Unexpected closing token '+getEnumName(TypeInfo(closeToken^.tokType),ord(closeToken^.tokType)));
    end;
    with scope[topIdx] do for i:=0 to length(ids)-1 do begin
      if warnAboutUnused and not(ids[i].used) and (adapters<>nil) then adapters^.postTextMessage(mt_el2_warning,ids[i].location,'Unused local variable '+ids[i].name);
      {$ifdef fullVersion}
      if localIdInfos<>nil then localIdInfos^.addLocalIdInfo(ids[i].name,ids[i].location,location,ids[i].idType);
      {$endif}
    end;
    setLength(scope[topIdx].ids,0);
    setLength(scope,topIdx);
  end;

{$ifdef fullVersion}
PROCEDURE T_idStack.popRemaining;
  VAR topIdx:longint;
      i:longint;
      lastLocation: T_tokenLocation;
  begin
    if prevToken=nil then exit;
    lastLocation:=prevToken^.location;
    lastLocation.line:=maxLongint;
    lastLocation.column:=maxLongint;
    topIdx:=length(scope)-1;
    while topIdx>=0 do begin
      with scope[topIdx] do for i:=0 to length(ids)-1 do begin
        if localIdInfos<>nil then localIdInfos^.addLocalIdInfo(ids[i].name,ids[i].location,lastLocation,ids[i].idType);
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

FUNCTION T_idStack.addId(CONST id:T_idString; CONST location:T_tokenLocation; CONST idType:T_tokenType):T_addIdResult;
  VAR i,j:longint;
  begin
    i:=length(scope)-1;
    if idType=tt_blockLocalVariable
    then while (i>=0) and (scope[i].scopeType<>sc_block) do dec(i);
    if i<0 then exit(air_notInBlock);
    with scope[i] do begin
      for j:=0 to length(ids)-1 do if ids[j].name=id then exit(air_reintroduce);
      j:=length(ids);
      setLength(ids,j+1);
      ids[j].name:=id;
      ids[j].used:=idType<>tt_blockLocalVariable;
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

PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_abstractPackage; CONST context:P_context; CONST recycler:P_recycler{$ifdef fullVersion};CONST callAndIdInfos:P_callAndIdInfos=nil{$endif});
  VAR t:P_token;
      rule:P_abstractRule;
      pattern:P_pattern;
  PROCEDURE prepareLambdaBody;
    VAR parameterId: T_patternElementLocation;
        uniqueIds: T_arrayOfString=();
        tokenInLambda:P_token;
        bracketLevel:longint=0;
        parameterIds: T_patternElementLocations;
        {$ifdef fullVersion}
        lastLocation:T_tokenLocation;
        {$endif}
    begin
      parameterIds:=pattern^.getNamedParameters;
      for parameterId in parameterIds do appendIfNew(uniqueIds,parameterId.id);

      tokenInLambda:=t;
      {$ifdef fullVersion}
      lastLocation:=tokenInLambda^.location;
      {$endif}
      while (tokenInLambda<>nil) and (bracketLevel>=0) do begin
        if      tokenInLambda^.tokType in C_openingBrackets then inc(bracketLevel)
        else if tokenInLambda^.tokType in C_closingBrackets then dec(bracketLevel)
        else if (tokenInLambda^.tokType in [tt_separatorComma,tt_semicolon]) and (bracketLevel=0) then dec(bracketLevel);
        if (tokenInLambda^.tokType in [tt_identifier, tt_eachParameter, tt_userRule, tt_globalVariable, tt_customType, tt_parameterIdentifier, tt_intrinsicRule]) and
           arrContains(uniqueIds,tokenInLambda^.txt)
        then tokenInLambda^.tokType:=tt_parameterIdentifier;
        {$ifdef fullVersion}
        lastLocation:=tokenInLambda^.location;
        {$endif}
        tokenInLambda:=tokenInLambda^.next;
      end;
      {$ifdef fullVersion}
      if tokenInLambda<>nil then lastLocation:=tokenInLambda^.location;
      if callAndIdInfos<>nil then
      for parameterId in parameterIds do callAndIdInfos^.addLocalIdInfo(parameterId.id,parameterId.location,lastLocation,tt_parameterIdentifier);
      {$endif}
      setLength(parameterIds,0);
      setLength(uniqueIds,0)
    end;

  begin
    t:=first;
    while t<>nil do begin
      case t^.tokType of
        tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach,tt_expBraceOpen:
          if (t^.next<>nil) and (t^.next^.next<>nil) and (t^.next^.tokType in [tt_literal,tt_globalVariable,tt_blockLocalVariable]) then case t^.next^.tokType of
            tt_operatorMinus: t^.next^.tokType:=tt_unaryOpMinus;
            tt_operatorPlus : t^.next:=recycler^.disposeToken(t^.next);
          end;
        tt_unaryOpMinus:
          if (t^.next<>nil) and (t^.next^.tokType=tt_unaryOpMinus) then begin
            t:=recycler^.disposeToken(t);
            t:=recycler^.disposeToken(t);
          end;
        tt_startOfPattern:begin
          new(pattern,create);
          pattern^.parse(t,t^.location,context,recycler,{$ifdef fullVersion}callAndIdInfos,{$endif}true);
          if context^.continueEvaluation
          then prepareLambdaBody
          else begin
            dispose(pattern,destroy);
            t^.tokType:=tt_EOL;
          end;
        end;
        tt_identifier,tt_globalVariable: if inPackage<>nil then begin
          if t^.data=nil then t^.data:=inPackage;
          if t^.tokType=tt_identifier
          then inPackage^.resolveId(t^,nil);
          {$ifdef fullVersion}
          if callAndIdInfos<>nil then callAndIdInfos^.add(t)
          {$endif};
          if (t^.next<>nil) and (t^.next^.tokType in [tt_assign,tt_mut_nested_assign..tt_mut_nestedDrop]) then begin
            if t^.tokType<>tt_identifier then begin
              if (t^.tokType = tt_globalVariable) then begin
                rule:=t^.data;
                t^.data:=rule;
                t^.tokType:=t^.next^.tokType;
                if t^.tokType=tt_assign then t^.tokType:=tt_mutate;
                t^.txt:=t^.txt;
                t^.next:=recycler^.disposeToken(t^.next);
              end else context^.raiseError('You can only modify variables! '+t^.txt+' is not a variable.',t^.next^.location);
            end else context^.raiseError('Cannot resolve identifier "'+t^.txt+'".',t^.location);
          end;
        end;
        tt_userRule: begin
          {$ifdef fullVersion}
          if callAndIdInfos<>nil then callAndIdInfos^.add(t);
          {$endif}
        end;
        tt_modifier: if t^.getModifier<>modifier_local then context^.raiseError('Modifier '+safeTokenToString(t)+' is not allowed here',t^.location)
        else if (t^.next<>nil) and (t^.next^.tokType=tt_blockLocalVariable) and (t^.next^.next<>nil) and (t^.next^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignNewBlockLocal;
          t^.data:=nil;
          t^.txt:=t^.next^.txt;
          t^.next:=recycler^.disposeToken(t^.next);
          t^.next:=recycler^.disposeToken(t^.next);
        end;
        tt_blockLocalVariable: if (t^.next<>nil) and (t^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignExistingBlockLocal;
          t^.data:=nil;
          t^.next:=recycler^.disposeToken(t^.next);
        end else if (t^.next<>nil) and (t^.next^.tokType in [tt_mut_nested_assign..tt_mut_nestedDrop]) then begin
          t^.tokType:=t^.next^.tokType;
          t^.data:=nil;
          t^.next:=recycler^.disposeToken(t^.next);
        end;
        {$ifdef fullVersion}
        else if callAndIdInfos<>nil then callAndIdInfos^.add(t);
        {$endif}
      end;
      t:=t^.next;
    end;
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
    setLength(blobLines,0);
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
  end;

FUNCTION T_callAndIdInfos.isLocationReferenced(CONST loc:T_searchTokenLocation):boolean;
  VAR info:T_usageInfo;
  begin
    if usageInfoFill<>length(usageInfos) then cleanup;
    for info in usageInfos do if info.targetLocation=loc then exit(true);
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
    result:=(usageInfoFill=0) and (length(localIdInfos)=0) and (length(blobLines)=0);
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

PROCEDURE T_callAndIdInfos.copyFrom(CONST original:P_callAndIdInfos);
  VAR k:longint;
  begin
    clear;
    if original=nil then exit;
    setLength(localIdInfos,length(original^.localIdInfos));
    for k:=0 to length(localIdInfos)-1 do localIdInfos[k]:=original^.localIdInfos[k];
    setLength(blobLines,length(original^.blobLines));
    for k:=0 to length(blobLines)-1 do blobLines[k]:=original^.blobLines[k];
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

PROCEDURE T_callAndIdInfos.addLocalIdInfo(CONST id:T_idString; CONST validFrom,validUntil:T_tokenLocation; CONST typ:T_tokenType);
  VAR i:longint;
  begin
    i:=length(localIdInfos);
    setLength(localIdInfos,i+1);
    localIdInfos[i].name      :=id;
    localIdInfos[i].validFrom :=validFrom;
    localIdInfos[i].validUntil:=validUntil;
    localIdInfos[i].tokenType :=typ;
  end;

PROCEDURE T_callAndIdInfos.markBlobLine(CONST lineIndex:longint; CONST closer:char);
  VAR k:longint;
  begin
    k:=length(blobLines);
    setLength(blobLines,k+1);
    blobLines[k].lineIndex :=lineIndex;
    blobLines[k].blobCloser:=closer;
  end;

FUNCTION T_callAndIdInfos.getBlobCloserOrZero(CONST lineIndex:longint):char;
  VAR k:longint;
  begin
    result:=#0;
    for k:=0 to length(blobLines)-1 do if blobLines[k].lineIndex=lineIndex then exit(blobLines[k].blobCloser);
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
    if (token^.tokType in [tt_userRule,tt_customTypeCheck,tt_identifier,tt_literal,tt_globalVariable,tt_customType]) then
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

FUNCTION T_enhancedToken.renameInLine(VAR line: string; CONST referencedLocation: T_searchTokenLocation; CONST oldName:string; newName: string): boolean;
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
      tt_each,tt_parallelEach: if references<>referencedLocation then exit(false);
      else exit(false);
    end;
    {$WARN 5092 OFF}
    case token^.tokType of
      tt_each,tt_parallelEach: newName:=C_tokenDefaultId[token^.tokType]+'('+newName+',';
      else newName:=replaceOne(token^.singleTokenToString,oldName,newName);
    end;
    result:=true;
    if endsAtColumn>length(line) then endsAtColumn:=length(line);
    if line[endsAtColumn]=' ' then dec(endsAtColumn);
    line:=copy(line,1,token^.location.column-1)+newName+copy(line,endsAtColumn+1,length(line));
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
    result.canRename:=token^.tokType in [tt_parameterIdentifier,tt_userRule,tt_globalVariable,tt_customType,tt_blockLocalVariable,tt_customTypeCheck,tt_eachParameter,tt_each,tt_parallelEach];
    result.tokenText:=safeTokenToString(token);
    if result.canRename then begin
      case token^.tokType of
        tt_each,tt_parallelEach:           result.idWithoutIsPrefix:=token^.txt;
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
        begin end;
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
        if result.tokenText=ATTRIBUTE_PREFIX+MAX_AGE_ATTRIBUTE
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
      tt_type,tt_typeCheck:
        result.shortInfo:=ansiReplaceStr(C_typeCheckInfo[token^.getTypeCheck].helpText,'#',C_lineBreakChar);
      tt_modifier:
        result.shortInfo:=ansiReplaceStr(C_modifierInfo[token^.getModifier].helpText,'#',C_lineBreakChar);
    end;
  end;
{$endif}

FUNCTION T_abstractLexer.getToken(CONST line: ansistring; VAR inputLocation:T_tokenLocation; CONST messages:P_messages; CONST recycler:P_recycler; CONST lexingStyle:T_lexingStyle): P_token;
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
      parsedLength:=len;
    end;

  PROCEDURE handleComment(CONST commentText:ansistring; CONST commentOpener:string);
    begin
      result^.tokType:=tt_EOL;
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
        end else if commentOpener='#' then blob.closer:='#' else blob.closer:='''';
      end else if pos('TODO',commentText)>0 then messages^.postTextMessage(mt_el1_note,inputLocation,commentText);
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
      {$ifdef fullVersion}
      if callAndIdInfos<>nil then callAndIdInfos^.markBlobLine(inputLocation.line,closer);
      {$endif}
      //id now is rest of line
      id:=copy(line,inputLocation.column,length(line));
      if pos(closer,id)<=0 then begin
        parsedLength:=length(id);
        inc(inputLocation.column,parsedLength);
        append(lines,id);
      end else begin
        parsedLength:=pos(closer,id)+length(closer)-1;
        inc(inputLocation.column,parsedLength);
        append(lines,copy(id,1,pos(closer,id)-1));
        result^.txt:=closer;
        closer:=#0;
        exit(result);
      end;
    end else if length(lines)>0 then begin
      result^.location:=start;
      result^.tokType:=tt_literal;
      result^.data:=recycler^.newStringLiteral(join(lines,C_lineBreakChar));
      setLength(lines,0);
      exit(result);
    end;
    if lexingStyle=ls_retainAll then begin
      while (inputLocation.column<=length(line)) and
            (line[inputLocation.column] in [' ',C_lineBreakChar,C_tabChar,C_carriageReturnChar]) do begin
        result^.txt:=result^.txt+line[inputLocation.column];
        inc(inputLocation.column);
      end;
      if result^.txt<>'' then begin
        result^.tokType:=tt_blank;
        exit(result);
      end;
    end else begin
      while (inputLocation.column<=length(line)) and
            (line[inputLocation.column] in [' ',C_lineBreakChar,C_tabChar,C_carriageReturnChar]) do inc(inputLocation.column);
      result^.location:=inputLocation;
    end;
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
          while (parsedLength+inputLocation.column<=length(line)) and not(line[parsedLength+inputLocation.column] in [C_lineBreakChar,C_carriageReturnChar,'#']) do inc(parsedLength);
          id:=copy(line,inputLocation.column+length(BLOCK_COMMENT_DELIMITER),parsedLength-1);
          if (length(line)>=parsedLength+inputLocation.column) and (line[parsedLength+inputLocation.column]='#') then inc(parsedLength);
          if lexingStyle in [ls_retainAll,ls_retainComments] then begin
            result^.tokType:=tt_blank;
            result^.txt:=copy(line,inputLocation.column,length(line));
          end else begin
            handleComment(id,BLOCK_COMMENT_DELIMITER);
          end;
        end else begin
          result^.tokType:=tt_literal;
          result^.data:=recycler^.newStringLiteral(stringValue);
        end;
        stringValue:='';
      end;
      '$': begin
        result^.txt:=leadingId;
        result^.tokType:=tt_parameterIdentifier;
      end;
      'a'..'z','A'..'Z','_':
      if copy(line,inputLocation.column,length(C_tokenDefaultId[tt_operatorNotIn]))=C_tokenDefaultId[tt_operatorNotIn] then apply(tt_operatorNotIn)
      else if isFormatString then begin
        stringValue:=unescapeString(line,inputLocation.column+1,parsedLength);
        inc(parsedLength); //...because we added one more character before the string
        result^.tokType:=tt_formatString;
        result^.data:=recycler^.newStringLiteral(stringValue);
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
        if lexingStyle in [ls_retainAll,ls_retainComments] then begin
          result^.tokType:=tt_blank;
          result^.txt:=copy(line,inputLocation.column,length(line));
        end else begin
          handleComment(copy(line,inputLocation.column+length(COMMENT_PREFIX),parsedLength),COMMENT_PREFIX);
        end;
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

FUNCTION T_abstractLexer.fetchNext(CONST messages:P_messages; CONST recycler:P_recycler; CONST lexingStyle:T_lexingStyle): boolean;
  PROCEDURE appendToken(CONST tok:P_token); inline;
    begin
      if (tok<>nil) and
         (tok^.tokType=tt_intrinsicRule) and
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
  begin
    nextToken:=fetch(messages,recycler,lexingStyle);
    if nextToken=nil then exit(false);
    while nextToken<>nil do case nextToken^.tokType of
      tt_iifElse: begin
        n[1]:=fetch(messages,recycler,lexingStyle);
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
          n[1]:=fetch(messages,recycler,lexingStyle);
          if (n[1]<>nil) and (n[1]^.tokType=tt_ponFlipper) then begin
            n[2]:=fetch(messages,recycler,lexingStyle);
            if (n[2]<>nil) and (n[2]^.tokType=tt_identifier) then begin
              nextToken^.txt:=nextToken^.txt+ID_QUALIFY_CHARACTER+n[2]^.txt;
              associatedPackage^.resolveId(nextToken^,nil);
              recycler^.disposeToken(n[1]);
              recycler^.disposeToken(n[2]);
              appendToken(nextToken);
              nextToken:=nil;
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
        n[1]:=fetch(messages,recycler,lexingStyle);
        n[2]:=fetch(messages,recycler,lexingStyle);
        n[3]:=fetch(messages,recycler,lexingStyle);
        if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) and
           (n[2]<>nil) and (n[2]^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule]) and
           (n[3]<>nil) and (n[3]^.tokType=tt_separatorComma) then begin
          nextToken^.txt:=n[2]^.txt;
          nextToken^.data:=nil;
        end else messages^.raiseSimpleError('Invalid (p)Each construct. First argument must be an identifier. At least two arguments must be given.',nextToken^.location);
        recycler^.disposeToken(n[1]);
        recycler^.disposeToken(n[2]);
        recycler^.disposeToken(n[3]);
        appendToken(nextToken);
        nextToken:=nil;
      end;
      tt_agg: begin
        n[1]:=fetch(messages,recycler,lexingStyle);
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
        n[1]:=fetch(messages,recycler,lexingStyle);
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

PROCEDURE T_abstractPackage.resolveId(VAR token: T_token; CONST adaptersOrNil: P_messages);
  VAR intrinsicFuncPtr:P_intFuncCallback;
      ruleId:T_idString;
  begin
    ruleId   :=token.txt;
    if builtinFunctionMap.containsFunctionForId(ruleId,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRule;
      token.data:=intrinsicFuncPtr;
      exit;
    end;
    if adaptersOrNil<>nil then adaptersOrNil^.raiseSimpleError('Cannot resolve ID "'+token.txt+'"',token.location);
  end;

PROCEDURE T_extendedPackage.resolveId(VAR token: T_token;
  CONST adaptersOrNil: P_messages);
  begin
    extender^.resolveId(token,adaptersOrNil);
  end;

FUNCTION T_abstractPackage.inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_callAndIdInfos{$endif}):P_mapLiteral;
  begin
    {$ifdef fullVersion}
    if functionCallInfos<>nil then new(functionCallInfos,create);
    {$endif}
    result:=newMapLiteral(0);
  end;

FUNCTION T_extendedPackage.inspect(CONST includeRulePointer: boolean; CONST context: P_abstractContext; CONST recycler:P_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_callAndIdInfos{$endif}): P_mapLiteral;
  begin
    {$ifdef fullVersion}
    if functionCallInfos=nil then new(functionCallInfos,create);
    {$endif}
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
