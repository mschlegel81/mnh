UNIT tokenArray;
INTERFACE
USES myGenerics,myStringUtil,
     basicTypes,mnh_constants,
     fileWrappers,
     litVar,
     funcs,
     funcs_mnh,
     recyclers,
     tokenStack,
     {$ifdef fullVersion}
     mnh_html,
     mnh_doc,
     {$endif}
     tokens,
     mnh_messages,
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
  P_functionCallInfos=^T_functionCallInfos;
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
      FUNCTION literalToString(CONST L:P_literal; {$WARN 5024 OFF}CONST location:T_tokenLocation; CONST context:P_abstractContext; VAR recycler:T_recycler):string; virtual;
      {$ifdef fullVersion}
      FUNCTION getImport({$WARN 5024 OFF}CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION getExtended(CONST idOrPath:string):P_abstractPackage; virtual;
      {$endif}
      FUNCTION inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; VAR recycler:T_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_functionCallInfos{$endif}):P_mapLiteral; virtual;
  end;

  P_extendedPackage=^T_extendedPackage;
    T_extendedPackage=object(T_abstractPackage)
    private
      extender:P_abstractPackage;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider; CONST extender_:P_abstractPackage);
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
      PROCEDURE resolveId(VAR token:T_token; CONST adaptersOrNil:P_messages); virtual;
      FUNCTION inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; VAR recycler:T_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_functionCallInfos{$endif}):P_mapLiteral; virtual;
  end;

  P_mnhSystemPseudoPackage=^T_mnhSystemPseudoPackage;
  T_mnhSystemPseudoPackage=object(T_abstractPackage)
    CONSTRUCTOR create;
    FUNCTION getId:T_idString; virtual;
    FUNCTION getPath:ansistring; virtual;
  end;

  T_enhancedStatement=record
    comments,
    attributes:T_arrayOfString;
    firstToken:P_token;
  end;

  {$ifdef fullVersion}
  T_usageInfo=record
    targetLocation,
    referencedAt:T_searchTokenLocation;
  end;

  T_functionCallInfos=object
    private
      fill:longint;
      usedBuiltins :T_setOfPointer;
      dat:array of T_usageInfo;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE add(CONST rulePointer:pointer; CONST token:P_token);
      PROCEDURE cleanup;
      FUNCTION  calledBuiltinFunctions:T_builtinFunctionMetaDatas;
      FUNCTION  getBuiltinRestrictions:T_specialFunctionRequirements;
      FUNCTION  whoReferencesLocation(CONST loc:T_searchTokenLocation):T_searchTokenLocations;
      FUNCTION  isLocationReferenced(CONST loc:T_searchTokenLocation):boolean;
      FUNCTION  isPackageReferenced(CONST packagePath:string):boolean;
      FUNCTION  isEmpty:boolean;
      PROCEDURE includeUsages(CONST other:P_functionCallInfos);
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
      CONSTRUCTOR create(CONST tok:P_token; CONST localIdInfos:P_localIdInfos; CONST package:P_abstractPackage);
      DESTRUCTOR destroy;
      FUNCTION toInfo:T_tokenInfo;
  end;

  T_enhancedTokens=object
    private
      dat:array of T_enhancedToken;
      PROCEDURE add(CONST tok:P_token; CONST localIdInfos:P_localIdInfos; CONST package:P_abstractPackage);
      PROCEDURE addLineEnder(CONST lineLength:longint);
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION getTokenAtIndex(CONST rowIndex:longint):T_enhancedToken;
      FUNCTION renameInLine(VAR line:string; CONST referencedLocation:T_searchTokenLocation; CONST oldName,newName:string):boolean;
  end;
  {$endif}

  T_lexingStyle=(ls_onlyInterpretable,ls_retainAll,ls_retainComments);

  T_lexer=object
    private
      blob:record
        closer:char;
        lines:T_arrayOfString;
        start:T_tokenLocation;
      end;
      input:T_arrayOfString;
      inputIndex:longint;
      inputLocation:T_tokenLocation;
      inputColumnOffset:longint;
      associatedPackage:P_abstractPackage;
      nextStatement:T_enhancedStatement;
      beforeLastTokenized,
      lastTokenized:P_token;
      FUNCTION getToken(CONST line:ansistring; CONST messages:P_messages; VAR recycler:T_recycler; {$ifdef fullVersion} CONST localIdInfos:P_localIdInfos;{$endif} CONST lexingStyle:T_lexingStyle=ls_onlyInterpretable):P_token;
      FUNCTION fetchNext(                      CONST messages:P_messages; VAR recycler:T_recycler; {$ifdef fullVersion} CONST localIdInfos:P_localIdInfos;{$endif} CONST lexingStyle:T_lexingStyle=ls_onlyInterpretable):boolean;
      PROCEDURE resetTemp;
    public
      CONSTRUCTOR create(CONST input_:T_arrayOfString; CONST location:T_tokenLocation; CONST inPackage:P_abstractPackage);
      CONSTRUCTOR create(CONST sourcePackage:P_abstractPackage; CONST inPackage:P_abstractPackage);
      CONSTRUCTOR create(CONST package:P_abstractPackage);
      DESTRUCTOR destroy;
      FUNCTION getNextStatement(CONST messages:P_messages; VAR recycler:T_recycler{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos{$endif}):T_enhancedStatement;
      {$ifdef fullVersion}
      FUNCTION getEnhancedTokens(CONST localIdInfos:P_localIdInfos):T_enhancedTokens;
      {$endif}
      PROCEDURE rawTokenize(CONST inputTxt:string;   CONST location:T_tokenLocation; VAR firstToken,lastToken:P_token; CONST messages:P_messages; VAR recycler:T_recycler);
      PROCEDURE rawTokenize(CONST literal:P_literal; CONST location:T_tokenLocation; VAR firstToken,lastToken:P_token; VAR recycler:T_recycler);
  end;

PROCEDURE preprocessStatement(CONST token:P_token; CONST messages:P_messages{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos{$endif});
PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_abstractPackage; CONST messages:P_messages; VAR recycler:T_recycler{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif});
FUNCTION isOperatorName(CONST id:T_idString):boolean;
VAR BLANK_ABSTRACT_PACKAGE:T_abstractPackage;
    MNH_PSEUDO_PACKAGE:T_mnhSystemPseudoPackage;
IMPLEMENTATION
USES sysutils,strutils,math,subrules,profiling;
FUNCTION isOperatorName(CONST id:T_idString):boolean;
  VAR s:string;
  begin
    for s in operatorName do if id=s then exit(true);
    result:=false;
  end;

PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_abstractPackage; CONST messages:P_messages; VAR recycler:T_recycler{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif});
  VAR t:P_token;
      rule:P_abstractRule;
  begin
    t:=first;
    while t<>nil do begin
      case t^.tokType of
        tt_identifier,tt_globalVariable: if inPackage<>nil then begin
          if t^.data=nil then t^.data:=inPackage;
          if t^.tokType=tt_identifier
          then inPackage^.resolveId(t^,nil);
          {$ifdef fullVersion}
          if functionCallInfos<>nil then functionCallInfos^.add(nil,t)
          {$endif};
          if (t^.next<>nil) and (t^.next^.tokType in [tt_assign,tt_mut_nested_assign..tt_mut_nestedDrop]) then begin
            if t^.tokType<>tt_identifier then begin
              if (t^.tokType = tt_globalVariable) then begin
                rule:=t^.data;
                t^.data:=rule;
                t^.tokType:=t^.next^.tokType;
                if t^.tokType=tt_assign then t^.tokType:=tt_mutate;
                t^.txt:=t^.txt;
                t^.next:=recycler.disposeToken(t^.next);
              end else messages^.raiseSimpleError('You can only modify variables! '+t^.txt+' is not a variable.',t^.next^.location);
            end else messages^.raiseSimpleError('Cannot resolve identifier "'+t^.txt+'".',t^.location);
          end;
        end;
        tt_userRule: begin
          {$ifdef fullVersion}
          if functionCallInfos<>nil then functionCallInfos^.add(nil,t);
          {$endif}
        end;
        tt_modifier: if t^.getModifier<>modifier_local then messages^.raiseSimpleError('Modifier '+safeTokenToString(t)+' is not allowed here',t^.location)
        else if (t^.next<>nil) and (t^.next^.tokType=tt_blockLocalVariable) and (t^.next^.next<>nil) and (t^.next^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignNewBlockLocal;
          t^.data:=nil;
          t^.txt:=t^.next^.txt;
          t^.next:=recycler.disposeToken(t^.next);
          t^.next:=recycler.disposeToken(t^.next);
        end;
        tt_blockLocalVariable: if (t^.next<>nil) and (t^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignExistingBlockLocal;
          t^.data:=nil;
          t^.next:=recycler.disposeToken(t^.next);
        end else if (t^.next<>nil) and (t^.next^.tokType in [tt_mut_nested_assign..tt_mut_nestedDrop]) then begin
          t^.tokType:=t^.next^.tokType;
          t^.data:=nil;
          t^.next:=recycler.disposeToken(t^.next);
        end;
        {$ifdef fullVersion}
        else if functionCallInfos<>nil then functionCallInfos^.add(nil,t);
        {$endif}
      end;
      t:=t^.next;
    end;
  end;

{$ifdef fullVersion}
CONSTRUCTOR T_functionCallInfos.create;
  begin
    usedBuiltins.create;
    clear;
  end;

DESTRUCTOR T_functionCallInfos.destroy;
  begin
    clear;
    usedBuiltins.destroy;
  end;

PROCEDURE T_functionCallInfos.clear;
  begin
    setLength(dat,0);
    usedBuiltins.clear;
    fill:=0;
  end;

PROCEDURE T_functionCallInfos.add(CONST rulePointer:pointer; CONST token: P_token);
  begin
    if (token=nil) then exit;
    if      token^.tokType in [tt_comparatorEq..tt_operatorConcatAlt] then usedBuiltins.put(intFuncForOperator[token^.tokType])
    else if token^.tokType=tt_intrinsicRule                           then usedBuiltins.put(token^.data)
    else if token^.tokType in [tt_userRule,tt_customType,tt_globalVariable,tt_customTypeCheck] then begin
      if fill>=length(dat) then setLength(dat,round(length(dat)*1.1)+1);
      dat[fill].referencedAt:=token^.location;
      dat[fill].targetLocation:=P_objectWithIdAndLocation(token^.data)^.getLocation;
      inc(fill);
    end;
  end;

PROCEDURE T_functionCallInfos.cleanup;
  VAR temporary:specialize G_stringKeyMap<T_usageInfo>;
      info:T_usageInfo;
      newDat:temporary.VALUE_TYPE_ARRAY;
      k:longint;
  begin
    setLength(dat,fill);
    temporary.create();
    for info in dat do temporary.put(string(info.referencedAt),info);
    newDat:=temporary.valueSet;
    fill:=length(newDat);
    setLength(dat,fill);
    for k:=0 to length(dat)-1 do dat[k]:=newDat[k];
    temporary.destroy;
  end;

FUNCTION T_functionCallInfos.calledBuiltinFunctions: T_builtinFunctionMetaDatas;
  VAR func:pointer;
      k:longint=0;
  begin
    setLength(result,usedBuiltins.size);
    for func in usedBuiltins.values do begin
      result[k]:=getMeta(func);
      inc(k);
    end;
  end;

FUNCTION T_functionCallInfos.getBuiltinRestrictions:T_specialFunctionRequirements;
  VAR meta:T_builtinFunctionMetaData;
  begin
    result:=[];
    for meta in calledBuiltinFunctions do include(result,meta.specialRequirement);
  end;

FUNCTION T_functionCallInfos.whoReferencesLocation(CONST loc: T_searchTokenLocation): T_searchTokenLocations;
  VAR info:T_usageInfo;
  begin
    //TODO: Can import-overloads be taken into account?
    //This would require modification of the rule map
    if fill<>length(dat) then cleanup;
    setLength(result,0);
    for info in dat do if info.targetLocation=loc then begin
      setLength(result,length(result)+1);
      result[length(result)-1]:=info.referencedAt;
    end;
  end;

FUNCTION T_functionCallInfos.isLocationReferenced(CONST loc:T_searchTokenLocation):boolean;
  VAR info:T_usageInfo;
  begin
    if fill<>length(dat) then cleanup;
    for info in dat do if info.targetLocation=loc then exit(true);
    result:=false;
  end;

FUNCTION T_functionCallInfos.isPackageReferenced(CONST packagePath:string):boolean;
  VAR info:T_usageInfo;
  begin
    if fill<>length(dat) then cleanup;
    for info in dat do if (info.referencedAt.fileName<>packagePath) and (info.targetLocation.fileName=packagePath) then exit(true);
    result:=false;
  end;

FUNCTION T_functionCallInfos.isEmpty:boolean;
  begin
    result:=fill=0;
  end;

PROCEDURE T_functionCallInfos.includeUsages(CONST other:P_functionCallInfos);
  VAR k :longint;
  begin
    other^.cleanup;
    setLength(dat,fill+other^.fill);
    for k:=0 to other^.fill-1 do begin
      dat[fill]:=other^.dat[k];
      inc(fill);
    end;
    usedBuiltins.put(other^.usedBuiltins.values);
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
PROCEDURE T_enhancedTokens.add(CONST tok: P_token; CONST localIdInfos: P_localIdInfos; CONST package: P_abstractPackage);
  VAR i:longint;
  begin
    i:=length(dat);
    setLength(dat,i+1);
    dat[i].create(tok,localIdInfos,package);
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
  begin
    for i:=0 to length(dat)-1 do dat[i].destroy;
    setLength(dat,0);
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

CONSTRUCTOR T_enhancedToken.create(CONST tok: P_token; CONST localIdInfos: P_localIdInfos; CONST package:P_abstractPackage);
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
    case localIdInfos^.localTypeOf(tokenText,token^.location.line,token^.location.column,references) of
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

DESTRUCTOR T_enhancedToken.destroy;
  begin
    if token<>nil then dispose(token,destroy);
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
    if line[endsAtColumn]=' ' then dec(endsAtColumn);
    line:=copy(line,1,token^.location.column-1)+newName+copy(line,endsAtColumn+1,length(line));
  end;

FUNCTION T_enhancedToken.toInfo:T_tokenInfo;
  VAR i:longint;
  PROCEDURE getBuiltinRuleInfo(OUT link:string);
    VAR doc:P_intrinsicFunctionDocumentation;
    begin
      if isQualified(result.tokenText)
      then doc:=functionDocMap.get(copy(result.tokenText,2,length(result.tokenText)-1))
      else doc:=functionDocMap.get(     result.tokenText                              );
      if doc<>nil then begin
        result.builtinRuleInfo:=doc^.getStructuredInfo(result.exampleText);
        link:=doc^.getHtmlLink;
      end;
    end;
  begin
    result.linkToHelp:=getDocIndexLinkForBrowser;
    result.location:=C_nilTokenLocation;
    result.startLoc:=C_nilTokenLocation;
    result.endLoc  :=C_nilTokenLocation;
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
      end;
      tt_userRule, tt_globalVariable: begin
        result.userDefRuleInfo:=P_abstractRule(token^.data)^.getStructuredInfo;
        if builtinRuleMap.containsKey(result.tokenText) then getBuiltinRuleInfo(result.linkToHelp);
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

FUNCTION T_lexer.getToken(CONST line: ansistring; CONST messages:P_messages; VAR recycler:T_recycler;
  {$ifdef fullVersion} CONST localIdInfos:P_localIdInfos;{$endif} CONST lexingStyle:T_lexingStyle=ls_onlyInterpretable): P_token;
  VAR parsedLength:longint=0;

  PROCEDURE fail(message:ansistring);
    begin
      messages^.raiseSimpleError(message,inputLocation);
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
        recycler.disposeToken(result);
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
    result:=recycler.newToken(inputLocation,'',tt_EOL);
    with blob do if closer<>#0 then begin
      {$ifdef fullVersion}
      if localIdInfos<>nil then localIdInfos^.markBlobLine(inputLocation.line,closer);
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
      result^.data:=newStringLiteral(join(lines,C_lineBreakChar));
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
      recycler.disposeToken(result);
      exit(nil);
    end;
    case line[inputLocation.column] of
      '0'..'9': begin
        result^.data:=parseNumber(line,inputLocation.column, false,parsedLength);
        if parsedLength<=0 then begin
                                  fail('Cannot parse numeric literal '+line);
                                  recycler.disposeToken(result);
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
          result^.data:=newStringLiteral(stringValue);
        end;
        stringValue:='';
      end;
      '$': begin
        result^.txt:=leadingId;
        result^.tokType:=tt_parameterIdentifier;
      end;
      'a'..'z','A'..'Z': begin
        result^.txt:=leadingId;
        result^.tokType:=tt_identifier;
        for tt:=low(T_tokenType) to high(T_tokenType) do
        if result^.txt=C_tokenDefaultId[tt] then result^.tokType:=tt;
        if result^.tokType=tt_identifier then begin
          if      result^.txt=LITERAL_BOOL_TEXT[true]  then begin result^.tokType:=tt_literal; result^.data:=newBoolLiteral(true);     end
          else if result^.txt=LITERAL_BOOL_TEXT[false] then begin result^.tokType:=tt_literal; result^.data:=newBoolLiteral(false);    end
          else if result^.txt=LITERAL_NAN_TEXT         then begin result^.tokType:=tt_literal; result^.data:=newRealLiteral(Nan);      end
          else if result^.txt=LITERAL_INF_TEXT         then begin result^.tokType:=tt_literal; result^.data:=newRealLiteral(infinity); end
          else if result^.txt=LITERAL_TEXT_VOID        then begin result^.tokType:=tt_literal; result^.data:=newVoidLiteral;           end
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

FUNCTION T_lexer.fetchNext(CONST messages:P_messages; VAR recycler:T_recycler;
  {$ifdef fullVersion} CONST localIdInfos:P_localIdInfos;{$endif} CONST lexingStyle:T_lexingStyle=ls_onlyInterpretable): boolean;
  FUNCTION fetch:P_token;
    begin
      result:=nil;
      while (result=nil) and (messages^.continueEvaluation) and (inputIndex<length(input)) do begin
        result:=getToken(input[inputIndex],messages,recycler{$ifdef fullVersion},localIdInfos{$endif},lexingStyle);
        if (result=nil) then begin
          inc(inputIndex);
          inc(inputLocation.line);
          inputLocation.column:=1;
        end else if lexingStyle<>ls_retainAll then case result^.tokType of
          tt_EOL: begin
            recycler.disposeToken(result);
            result:=nil;
          end;
          tt_docComment: if lexingStyle<>ls_retainComments then begin
            myGenerics.append(nextStatement.comments ,result^.txt);
            recycler.disposeToken(result);
            result:=nil;
          end;
          tt_attributeComment: if lexingStyle<>ls_retainComments then begin
            if (result^.txt<>'') then myGenerics.append(nextStatement.attributes,result^.txt);
            recycler.disposeToken(result);
            result:=nil;
          end;
        end;
      end;
      if result<>nil then inc(result^.location.column,inputColumnOffset);
    end;

  PROCEDURE appendToken(CONST tok:P_token); inline;
    begin
      if nextStatement.firstToken=nil
      then nextStatement.firstToken:=tok
      else lastTokenized^.next     :=tok;
      beforeLastTokenized          :=lastTokenized;
      lastTokenized                :=tok;
    end;

  VAR nextToken:P_token;
      n:array[1..3] of P_token;
  begin
    nextToken:=fetch;
    if nextToken=nil then exit(false);
    while nextToken<>nil do case nextToken^.tokType of
      tt_literal: if (beforeLastTokenized<>nil) and (beforeLastTokenized^.tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach,tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus])
                       and (lastTokenized<>nil) and (lastTokenized^.tokType in [tt_operatorMinus,tt_operatorPlus]) then begin
        if lastTokenized^.tokType=tt_operatorMinus
        then lastTokenized^.tokType:=tt_unaryOpMinus
        else begin
          recycler.disposeToken(lastTokenized);
          lastTokenized:=beforeLastTokenized;
        end;
        appendToken(nextToken);
        nextToken:=nil;
      end else begin
        appendToken(nextToken);
        nextToken:=nil;
      end;
      tt_identifier: if (associatedPackage<>nil) then begin
        if (associatedPackage^.isImportedOrBuiltinPackage(nextToken^.txt)) then begin
          n[1]:=fetch;
          if (n[1]<>nil) and (n[1]^.tokType=tt_ponFlipper) then begin
            n[2]:=fetch;
            if (n[2]<>nil) and (n[2]^.tokType=tt_identifier) then begin
              nextToken^.txt:=nextToken^.txt+ID_QUALIFY_CHARACTER+n[2]^.txt;
              associatedPackage^.resolveId(nextToken^,nil);
              recycler.disposeToken(n[1]);
              recycler.disposeToken(n[2]);
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
        //This is a hack to ensure that "myPath" behaves nicely when including
        if (lastTokenized<>nil) and
           (lastTokenized^.tokType=tt_intrinsicRule) and
           (lastTokenized^.data=pointer(BUILTIN_MYPATH))
        then lastTokenized^.location.package:=associatedPackage;
      end else begin
        appendToken(nextToken);
        nextToken:=nil;
      end;
      tt_each,tt_parallelEach: begin
        n[1]:=fetch; n[2]:=fetch; n[3]:=fetch;
        if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) and
           (n[2]<>nil) and (n[2]^.tokType in [tt_identifier,tt_userRule,tt_intrinsicRule]) and
           (n[3]<>nil) and (n[3]^.tokType=tt_separatorComma) then begin
          nextToken^.txt:=n[2]^.txt;
          nextToken^.data:=nil;
        end else messages^.raiseSimpleError('Invalid (p)Each construct. First argument must be an identifier. At least two arguments must be given.',nextToken^.location);
        recycler.disposeToken(n[1]);
        recycler.disposeToken(n[2]);
        recycler.disposeToken(n[3]);
        appendToken(nextToken);
        nextToken:=nil;
      end;
      tt_agg: begin
        n[1]:=fetch;
        if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) then begin
          nextToken^.tokType:=tt_each;
          nextToken^.txt:='';
          nextToken^.data:=nil;
        end else messages^.raiseSimpleError('Invalid agg construct.',nextToken^.location);
        recycler.disposeToken(n[1]);
        appendToken(nextToken);
        nextToken:=nil;
      end;
      else begin
        appendToken(nextToken);
        nextToken:=nil;
      end;
    end;
    result:=true;

  end;

CONSTRUCTOR T_lexer.create(CONST input_: T_arrayOfString; CONST location: T_tokenLocation; CONST inPackage: P_abstractPackage);
  begin
    input:=input_;
    inputIndex:=0;
    inputLocation:=location;
    inputLocation.column:=1;
    inputColumnOffset:=location.column-inputLocation.column;
    associatedPackage:=inPackage;

    setLength(blob.lines,0);
    blob.closer:=#0;
    resetTemp;
  end;

CONSTRUCTOR T_lexer.create(CONST sourcePackage:P_abstractPackage; CONST inPackage:P_abstractPackage);
  begin
    input:=sourcePackage^.getCodeProvider^.getLines;
    inputIndex:=0;
    inputLocation.package:=sourcePackage;
    inputLocation.column:=1;
    inputLocation.line:=1;
    inputColumnOffset:=0;
    associatedPackage:=inPackage;
    setLength(blob.lines,0);
    blob.closer:=#0;
    resetTemp;
  end;

CONSTRUCTOR T_lexer.create(CONST package: P_abstractPackage);
  begin
    create(package,package);
  end;

PROCEDURE T_lexer.resetTemp;
  begin
    nextStatement.attributes:=C_EMPTY_STRING_ARRAY;
    nextStatement.comments  :=C_EMPTY_STRING_ARRAY;
    nextStatement.firstToken:=nil;
    beforeLastTokenized:=nil;
    lastTokenized:=nil;
  end;

DESTRUCTOR T_lexer.destroy;
  begin
    while nextStatement.firstToken<>nil do begin
      lastTokenized:=nextStatement.firstToken;
      nextStatement.firstToken:=nextStatement.firstToken^.next;
      dispose(lastTokenized,destroy);
    end;
  end;

PROCEDURE preprocessStatement(CONST token:P_token; CONST messages:P_messages{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos{$endif});
  VAR t:P_token;
      localIdStack:T_idStack;
      lastWasLocalModifier:boolean=false;
      idType:T_tokenType;
      lastLocation:T_tokenLocation;
      idLoc:T_tokenLocation;
  begin
    localIdStack.create({$ifdef fullVersion}localIdInfos{$endif});
    t:=token;
    while (t<>nil) do begin
      lastLocation:=t^.location;
      case t^.tokType of
        tt_beginBlock:
          localIdStack.scopePush(sc_block,lastLocation);
        tt_endBlock:
          localIdStack.scopePop(messages,lastLocation,false,true);
        tt_braceOpen:
          localIdStack.scopePush(sc_bracketOnly,lastLocation);
        tt_braceClose:
          localIdStack.scopePop(messages,lastLocation,true,true);
        tt_each,tt_parallelEach:begin
          localIdStack.scopePush(sc_each,lastLocation);
          localIdStack.addId(EACH_INDEX_IDENTIFIER,lastLocation,tt_eachIndex);
          localIdStack.addId(t^.txt               ,lastLocation,tt_eachParameter);
        end;
        tt_identifier,tt_userRule,tt_intrinsicRule,tt_globalVariable,tt_customType,tt_customTypeCheck:begin
          if lastWasLocalModifier then begin
            t^.tokType:=tt_blockLocalVariable;
            case localIdStack.addId(t^.txt,lastLocation,tt_blockLocalVariable) of
              air_reintroduce: messages^.raiseSimpleError('Invalid re-introduction of local variable "'+t^.txt+'"',lastLocation);
              air_notInBlock : messages^.raiseSimpleError('You can only declare local variables in begin-end-blocks',lastLocation);
            end;
          end else if (localIdStack.hasId(t^.txt,idType,idLoc)) then begin
            t^.tokType:=idType;
            if idType=tt_eachIndex then t^.location:=idLoc;
          end;
        end;
      end;
      lastWasLocalModifier:=(t^.tokType=tt_modifier) and (t^.getModifier=modifier_local);
      t:=t^.next;
    end;
    while not(localIdStack.scopeBottom) do localIdStack.scopePop(messages,lastLocation,false,true);
    localIdStack.destroy;
  end;

FUNCTION T_lexer.getNextStatement(CONST messages:P_messages; VAR recycler:T_recycler{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos{$endif}): T_enhancedStatement;
  VAR localIdStack:T_idStack;
      lastWasLocalModifier:boolean=false;
      idType:T_tokenType;
      lastLocation:T_tokenLocation;
      idLoc:T_tokenLocation;
      earlierSuppressedUnusedAttribute:boolean=false;
  FUNCTION hasSuppressedUnusedAttribute:boolean;
    VAR s:string;
    begin
      if earlierSuppressedUnusedAttribute then exit(true);
      for s in nextStatement.attributes do earlierSuppressedUnusedAttribute:=earlierSuppressedUnusedAttribute or startsWith(s,SUPPRESS_UNUSED_WARNING_ATTRIBUTE);
      result:=earlierSuppressedUnusedAttribute;
    end;

  begin
    localIdStack.create({$ifdef fullVersion}localIdInfos{$endif});
    while fetchNext(messages,recycler{$ifdef fullVersion},localIdInfos{$endif}) and (lastTokenized<>nil) do begin
      lastLocation:=lastTokenized^.location;
      case lastTokenized^.tokType of
        tt_beginBlock:
          localIdStack.scopePush(sc_block,lastLocation);
        tt_endBlock:
          localIdStack.scopePop(messages,lastLocation,false,not(hasSuppressedUnusedAttribute));
        tt_braceOpen:
          localIdStack.scopePush(sc_bracketOnly,lastLocation);
        tt_braceClose:
          localIdStack.scopePop(messages,lastLocation,true,not(hasSuppressedUnusedAttribute));
        tt_each,tt_parallelEach:begin
          localIdStack.scopePush(sc_each,lastLocation);
          localIdStack.addId(EACH_INDEX_IDENTIFIER,lastLocation,tt_eachIndex);
          localIdStack.addId(lastTokenized^.txt   ,lastLocation,tt_eachParameter);
        end;
        tt_identifier,tt_userRule,tt_intrinsicRule,tt_globalVariable,tt_customType,tt_customTypeCheck:
          if lastWasLocalModifier then begin
            lastTokenized^.tokType:=tt_blockLocalVariable;
            case localIdStack.addId(lastTokenized^.txt,lastLocation,tt_blockLocalVariable) of
              air_reintroduce: messages^.raiseSimpleError('Invalid re-introduction of local variable "'+lastTokenized^.txt+'"',lastLocation);
              air_notInBlock : messages^.raiseSimpleError('You can only declare local variables in begin-end-blocks',lastLocation);
            end;
          end else if (localIdStack.hasId(lastTokenized^.txt,idType,idLoc)) then begin
            lastTokenized^.tokType:=idType;
            if idType=tt_eachIndex then lastTokenized^.location:=idLoc;
          end;
        tt_semicolon: if localIdStack.scopeBottom then begin
          if beforeLastTokenized<>nil then begin;
            beforeLastTokenized^.next:=nil;
            recycler.disposeToken(lastTokenized);
          end;
          result:=nextStatement;
          if not(messages^.continueEvaluation) then recycler.cascadeDisposeToken(result.firstToken);
          resetTemp;
          localIdStack.destroy;
          exit;
        end;
      end;
      lastWasLocalModifier:=(lastTokenized<>nil) and (lastTokenized^.tokType=tt_modifier) and (lastTokenized^.getModifier=modifier_local);
    end;
    result:=nextStatement;
    if not(messages^.continueEvaluation) then begin
      {$ifdef fullVersion}
      localIdStack.popRemaining(lastLocation);
      {$endif}
      recycler.cascadeDisposeToken(result.firstToken);
    end;
    resetTemp;
    while not(localIdStack.scopeBottom) do localIdStack.scopePop(messages,lastLocation,false,not(hasSuppressedUnusedAttribute));
    localIdStack.destroy;
  end;

PROCEDURE safeAppend(VAR first,last:P_token; CONST appendix:P_token);
  begin
    if appendix=nil then exit;
    if first=nil
    then first     :=appendix
    else last^.next:=appendix;
    last :=appendix^.last;
  end;

PROCEDURE T_lexer.rawTokenize(CONST inputTxt:string; CONST location:T_tokenLocation; VAR firstToken,lastToken:P_token; CONST messages:P_messages; VAR recycler:T_recycler);
  begin
    input:=inputTxt;
    inputIndex:=0;
    inputLocation:=location;
    inputLocation.column:=1;
    inputColumnOffset:=location.column-inputLocation.column;
    setLength(blob.lines,0);
    blob.closer:=#0;
    while fetchNext(messages,recycler{$ifdef fullVersion},nil{$endif}) do begin end;
    safeAppend(firstToken,lastToken,nextStatement.firstToken);
    resetTemp;
  end;

PROCEDURE T_lexer.rawTokenize(CONST literal:P_literal; CONST location:T_tokenLocation; VAR firstToken,lastToken:P_token; VAR recycler:T_recycler);
  begin
    safeAppend(firstToken,lastToken,recycler.newToken(location,'',tt_literal,literal^.rereferenced));
  end;

{$ifdef fullVersion}
FUNCTION T_lexer.getEnhancedTokens(CONST localIdInfos:P_localIdInfos):T_enhancedTokens;
  VAR adapters:T_messagesDummy;
      t:P_token;
      recycler:T_recycler;
  begin
    recycler.initRecycler;
    blob.closer:=localIdInfos^.getBlobCloserOrZero(inputLocation.line);

    adapters.createDummy;
    while fetchNext(@adapters,recycler,nil,ls_retainComments) do begin end;
    dec(inputLocation.line);
    inputLocation.column:=length(input[length(input)-1]);

    adapters.destroy;

    result.create;
    t:=nextStatement.firstToken;
    while t<>nil do begin
      associatedPackage^.resolveId(t^,nil);
      result.add(t,localIdInfos,associatedPackage);
      t:=t^.next;
    end;
    result.addLineEnder(inputLocation.column);
    resetTemp;
    recycler.cleanup;
  end;

{$endif}

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

CONSTRUCTOR T_extendedPackage.create(CONST provider:P_codeProvider; CONST extender_:P_abstractPackage);
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

FUNCTION T_extendedPackage.isImportedOrBuiltinPackage(CONST id:string):boolean;
  begin
    result:=extender^.isImportedOrBuiltinPackage(id);
  end;

PROCEDURE T_abstractPackage.resolveId(VAR token: T_token; CONST adaptersOrNil: P_messages);
  VAR intrinsicFuncPtr:P_intFuncCallback;
      ruleId:T_idString;
  begin
    ruleId   :=token.txt;
    if builtinRuleMap.containsKey(ruleId,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRule;
      token.data:=intrinsicFuncPtr;
      exit;
    end;
    if adaptersOrNil<>nil then adaptersOrNil^.raiseSimpleError('Cannot resolve ID "'+token.txt+'"',token.location);
  end;

PROCEDURE T_extendedPackage.resolveId(VAR token:T_token; CONST adaptersOrNil:P_messages);
  begin
    extender^.resolveId(token,adaptersOrNil);
  end;

FUNCTION T_abstractPackage.inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; VAR recycler:T_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_functionCallInfos{$endif}):P_mapLiteral;
  begin
    {$ifdef fullVersion}
    if functionCallInfos<>nil then new(functionCallInfos,create);
    {$endif}
    result:=newMapLiteral(0);
  end;

FUNCTION T_extendedPackage.inspect(CONST includeRulePointer:boolean; CONST context:P_abstractContext; VAR recycler:T_recycler{$ifdef fullVersion}; VAR functionCallInfos:P_functionCallInfos{$endif}):P_mapLiteral;
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

FUNCTION T_abstractPackage.literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:P_abstractContext; VAR recycler:T_recycler):string;
  begin
    if (L^.literalType=lt_string)
    then result:=P_stringLiteral(L)^.value
    else result:=L^.toString();
  end;

FUNCTION T_abstractPackage.getTypeMap:T_typeMap;
  begin
    result.create();
  end;

{$ifdef fullVersion}

FUNCTION T_abstractPackage.getImport(CONST idOrPath:string):P_abstractPackage; begin result:=nil; end;
FUNCTION T_abstractPackage.getExtended(CONST idOrPath:string):P_abstractPackage; begin result:=nil; end;

FUNCTION tokenizeAllReturningRawTokens(CONST inputString:ansistring):T_rawTokenArray;
  VAR lexer:T_lexer;
      location:T_tokenLocation;
      adapters:T_messagesDummy;
      recycler:T_recycler;
      t:P_token;
  begin
    recycler.initRecycler;
    location.package:=@BLANK_ABSTRACT_PACKAGE;
    location.line:=0;
    location.column:=1;
    lexer.create(inputString,location,@BLANK_ABSTRACT_PACKAGE);
    adapters.createDummy;
    repeat until not(lexer.fetchNext(@adapters,recycler{$ifdef fullVersion},nil{$endif},ls_retainAll));
    adapters.destroy;
    t:=lexer.nextStatement.firstToken;
    lexer.resetTemp;
    lexer.destroy;
    setLength(result,0);
    while t<>nil do begin
      setLength(result,length(result)+1);
      BLANK_ABSTRACT_PACKAGE.resolveId(t^,nil);
      result[length(result)-1]:=t^.getRawToken;
      t:=recycler.disposeToken(t);
    end;
    recycler.cleanup;
  end;
{$endif}

INITIALIZATION
  BLANK_ABSTRACT_PACKAGE.create(newVirtualFileCodeProvider('',C_EMPTY_STRING_ARRAY));
  MNH_PSEUDO_PACKAGE.create();
  profiling.mnhSysPseudopackagePrefix:=MNH_PSEUDO_PACKAGE.getPath;
  {$ifdef fullVersion}
  rawTokenizeCallback:=@tokenizeAllReturningRawTokens;
  {$endif}

FINALIZATION
  BLANK_ABSTRACT_PACKAGE.destroy;
  MNH_PSEUDO_PACKAGE.destroy;

end.
