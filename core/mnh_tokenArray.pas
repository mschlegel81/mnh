UNIT mnh_tokenArray;
INTERFACE
USES sysutils,math,
     myGenerics,myStringUtil,
     mnh_basicTypes,mnh_constants,
     mnh_fileWrappers,
     mnh_litVar,
     mnh_funcs,
     mnh_funcs_mnh,
     tokenStack,
     {$ifdef fullVersion}
     mnh_html,
     mnh_doc,
     {$endif}
     mnh_tokens,
     mnh_messages,
     mnh_out_adapters;
TYPE
  T_customOperatorArray=array[tt_comparatorEq..tt_unaryOpMinus] of P_abstractRule;
  P_abstractPackage=^T_abstractPackage;
  T_abstractPackage=object(T_objectWithPath)
    private
      codeProvider:P_codeProvider;
      readyForCodeState:T_hashInt;
    protected
      customOperatorRules:T_customOperatorArray;
      PROCEDURE logReady(CONST stateHashAtLoad:T_hashInt);
      PROCEDURE clearCustomOperators;
      FUNCTION mergeCustomOps(CONST importedPackage:P_abstractPackage; CONST connector:P_messages):boolean;
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
      PROCEDURE resolveId(VAR token:T_token; CONST adaptersOrNil:P_messages{$ifdef fullVersion};CONST markAsUsed:boolean=true{$endif}); virtual;
      FUNCTION getTypeMap:T_typeMap; virtual;
      FUNCTION literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:pointer):string; virtual;
      {$ifdef fullVersion}
      FUNCTION getImport({$WARN 5024 OFF}CONST idOrPath:string):P_abstractPackage; virtual;
      FUNCTION getExtended(CONST idOrPath:string):P_abstractPackage; virtual;
      {$endif}
  end;

  P_extendedPackage=^T_extendedPackage;
  T_extendedPackage=object(T_abstractPackage)
    private
      extender:P_abstractPackage;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider; CONST extender_:P_abstractPackage);
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual;
      PROCEDURE resolveId(VAR token:T_token; CONST adaptersOrNil:P_messages{$ifdef fullVersion};CONST markAsUsed:boolean=true{$endif}); virtual;
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
  T_tokenInfo=record
    infoText:ansistring;
    location,startLoc,endLoc:T_searchTokenLocation;
    canRename,mightBeUsedInOtherPackages:boolean;
    tokenType:T_tokenType;
    idWithoutIsPrefix:string;
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

  T_lexer=object
    private
      blob:record
        closer:char;
        text:string;
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
      FUNCTION getToken(CONST line:ansistring; CONST messages:P_messages; {$ifdef fullVersion} CONST localIdInfos:P_localIdInfos;{$endif} CONST retainBlanks:boolean=false):P_token;
      FUNCTION fetchNext(                      CONST messages:P_messages; {$ifdef fullVersion} CONST localIdInfos:P_localIdInfos;{$endif} CONST retainBlanks:boolean=false):boolean;
      PROCEDURE resetTemp;
    public
      CONSTRUCTOR create(CONST input_:T_arrayOfString; CONST location:T_tokenLocation; CONST inPackage:P_abstractPackage);
      CONSTRUCTOR create(CONST sourcePackage:P_abstractPackage; CONST inPackage:P_abstractPackage);
      CONSTRUCTOR create(CONST package:P_abstractPackage);
      DESTRUCTOR destroy;
      FUNCTION getNextStatement(CONST messages:P_messages{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos{$endif}):T_enhancedStatement;
      {$ifdef fullVersion}
      FUNCTION getEnhancedTokens(CONST localIdInfos:P_localIdInfos):T_enhancedTokens;
      {$endif}
      PROCEDURE rawTokenize(CONST inputTxt:string;   CONST location:T_tokenLocation; VAR firstToken,lastToken:P_token; CONST messages:P_messages);
      PROCEDURE rawTokenize(CONST literal:P_literal; CONST location:T_tokenLocation; VAR firstToken,lastToken:P_token);
  end;

PROCEDURE preprocessStatement(CONST token:P_token; CONST messages:P_messages{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos{$endif});
PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_abstractPackage; CONST messages:P_messages);
VAR BLANK_ABSTRACT_PACKAGE:T_abstractPackage;
    MNH_PSEUDO_PACKAGE:T_mnhSystemPseudoPackage;
IMPLEMENTATION
PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_abstractPackage; CONST messages:P_messages);
  VAR t:P_token;
      rule:P_abstractRule;
  begin
    t:=first;
    while t<>nil do begin
      case t^.tokType of
        tt_identifier,tt_localUserRule,tt_importedUserRule,tt_customTypeRule: if inPackage<>nil then begin
          if t^.data=nil then t^.data:=inPackage;
          if t^.tokType=tt_identifier
          then inPackage^.resolveId(t^,nil)
          {$ifdef fullVersion}
          else P_abstractRule(t^.data)^.setIdResolved
          {$endif};
          if (t^.next<>nil) and (t^.next^.tokType in [tt_assign,tt_mut_nested_assign..tt_mut_nestedDrop]) then begin
            if t^.tokType<>tt_identifier then begin
              if (t^.tokType in [tt_localUserRule,tt_importedUserRule]) then begin
                rule:=t^.data;
                if rule^.getRuleType in C_mutableRuleTypes then begin
                  t^.data:=rule;
                  t^.tokType:=t^.next^.tokType;
                  if t^.tokType=tt_assign then t^.tokType:=tt_mutate;
                  t^.txt:=t^.txt;
                  t^.next:=disposeToken(t^.next);
                end else messages^.raiseSimpleError('You can only mutate mutable rules! Rule '+rule^.getId+' is not mutable',t^.next^.location);
              end else messages^.raiseSimpleError('You can only mutate mutable rules! Rule '+t^.txt+' is a '+C_ruleTypeString[t^.tokType],t^.next^.location);
            end else messages^.raiseSimpleError('Cannot resolve identifier "'+t^.txt+'".',t^.location);
          end;
        end;
        tt_modifier:
        if t^.getModifier<>modifier_local then messages^.raiseSimpleError('Modifier '+safeTokenToString(t)+' is not allowed here',t^.location)
        else
        if (t^.next<>nil) and (t^.next^.tokType=tt_blockLocalVariable) and (t^.next^.next<>nil) and (t^.next^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignNewBlockLocal;
          t^.data:=nil;
          t^.txt:=t^.next^.txt;
          t^.next:=disposeToken(t^.next);
          t^.next:=disposeToken(t^.next);
        end;
        tt_blockLocalVariable: if (t^.next<>nil) and (t^.next^.tokType=tt_assign) then begin
          t^.tokType:=tt_assignExistingBlockLocal;
          t^.data:=nil;
          t^.next:=disposeToken(t^.next);
        end else if (t^.next<>nil) and (t^.next^.tokType in [tt_mut_nested_assign..tt_mut_nestedDrop]) then begin
          t^.tokType:=t^.next^.tokType;
          t^.data:=nil;
          t^.next:=disposeToken(t^.next);
        end;
        end;
      t:=t^.next;
    end;
  end;

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
    if (token^.tokType in [tt_importedUserRule,tt_localUserRule,tt_customTypeRule, tt_customTypeCheck,tt_identifier,tt_literal]) then
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
    if (linksTo=nothing) and (token^.tokType in [tt_importedUserRule,tt_localUserRule,tt_customTypeRule, tt_customTypeCheck]) then begin
      references:=P_abstractRule(token^.data)^.getLocation;
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
      tt_localUserRule      ,
      tt_importedUserRule   ,
      tt_blockLocalVariable ,
      tt_customTypeCheck    ,
      tt_customTypeRule     ,
      tt_eachParameter,
      tt_each,tt_parallelEach: if references<>referencedLocation then exit(false);
      else exit(false);
    end;
    case token^.tokType of
      tt_each,tt_parallelEach: newName:=C_tokenInfo[token^.tokType].defaultId+'('+newName+',';
      else newName:=replaceOne(token^.singleTokenToString,oldName,newName);
    end;
    result:=true;
    line:=copy(line,1,token^.location.column-1)+newName+copy(line,endsAtColumn+1,length(line));
  end;

FUNCTION T_enhancedToken.toInfo:T_tokenInfo;
  VAR i:longint;
      tokenText:string;
  FUNCTION getBuiltinRuleInfo:string;
    VAR doc:P_intrinsicFunctionDocumentation;
    begin
      ensureBuiltinDocExamples;
      if (length(tokenText)>1) and (tokenText[1]='.')
      then doc:=functionDocMap.get(copy(tokenText,2,length(tokenText)-1))
      else doc:=functionDocMap.get(tokenText);
      if doc=nil then exit('');
      result:='builtin rule'+C_lineBreakChar+doc^.getPlainText(C_lineBreakChar)+';';
    end;
  begin
    result.infoText:='(eol)';
    result.location:=C_nilTokenLocation;
    result.startLoc:=C_nilTokenLocation;
    result.endLoc  :=C_nilTokenLocation;
    result.canRename:=false;
    result.mightBeUsedInOtherPackages:=false;
    result.idWithoutIsPrefix:='';
    result.tokenType:=tt_EOL;
    if token=nil then exit;
    result.tokenType    :=token^.tokType;
    result.location     :=references;
    result.startLoc     :=token^.location;
    result.endLoc       :=token^.location;
    result.endLoc.column:=endsAtColumn;
    result.canRename:=token^.tokType in [tt_parameterIdentifier,tt_importedUserRule,tt_localUserRule,tt_blockLocalVariable,tt_customTypeCheck,tt_customTypeRule,tt_eachParameter,tt_each,tt_parallelEach];
    tokenText:=safeTokenToString(token);
    if result.canRename then begin
      case token^.tokType of
        tt_each,tt_parallelEach: result.idWithoutIsPrefix:=token^.txt;
        tt_customTypeCheck,tt_customTypeRule,tt_localUserRule,tt_importedUserRule:
                                 result.idWithoutIsPrefix:=P_abstractRule(token^.data)^.getRootId;
        else                     result.idWithoutIsPrefix:=tokenText;
      end;
      result.mightBeUsedInOtherPackages:=(token^.tokType=tt_importedUserRule) or
                                         (token^.tokType in [tt_importedUserRule,tt_localUserRule,tt_customTypeCheck,tt_customTypeRule]) and (P_abstractRule(token^.data)^.hasPublicSubrule);
    end;
    result.infoText:=ECHO_MARKER+tokenText;
    case linksTo of
      packageUse: begin
        result.infoText+=C_lineBreakChar+'Used package: '+ansistring(references);
        exit;
      end;
      packageInclude: begin
        result.infoText+=C_lineBreakChar+'Included package: '+ansistring(references);
        exit;
      end;
    end;
    result.infoText+=C_lineBreakChar
                    +replaceAll(C_tokenInfo[token^.tokType].helpText,'#',C_lineBreakChar);
    for i:=0 to length(C_specialWordInfo)-1 do
      if C_specialWordInfo[i].txt=tokenText then
      result.infoText+=C_lineBreakChar+replaceAll(C_specialWordInfo[i].helpText,'#',C_lineBreakChar);

    case token^.tokType of
      tt_intrinsicRule:
        result.infoText+=C_lineBreakChar+getBuiltinRuleInfo;
      tt_blockLocalVariable, tt_parameterIdentifier, tt_eachParameter, tt_eachIndex:
        result.infoText+=C_lineBreakChar+'Declared '+ansistring(references);
      tt_importedUserRule,tt_localUserRule,tt_customTypeRule, tt_customTypeCheck: begin
        result.infoText+=C_lineBreakChar
                        +replaceAll(P_abstractRule(token^.data)^.getDocTxt,C_tabChar,' ');
        if intrinsicRuleMap.containsKey(tokenText) then
         result.infoText+=C_lineBreakChar+'overloads '+getBuiltinRuleInfo;
      end;
      tt_type,tt_typeCheck:
        result.infoText+=C_lineBreakChar+replaceAll(C_typeCheckInfo[token^.getTypeCheck].helpText,'#',C_lineBreakChar);
      tt_modifier:
        result.infoText+=C_lineBreakChar+replaceAll(C_modifierInfo[token^.getModifier].helpText,'#',C_lineBreakChar);
    end;
  end;
{$endif}

FUNCTION T_lexer.getToken(CONST line: ansistring; CONST messages:P_messages;
  {$ifdef fullVersion} CONST localIdInfos:P_localIdInfos;{$endif} CONST retainBlanks: boolean): P_token;
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
      for tt:=low(T_tokenType) to high(T_tokenType) do if length(C_tokenInfo[tt].defaultId)=parsedLength then begin
        match:=true;
        for i:=0 to parsedLength-1 do match:=match and (line[inputLocation.column+i]=C_tokenInfo[tt].defaultId[i+1]);
        if match then exit(C_tokenInfo[tt].defaultId);
      end;
      result:=copy(line,inputLocation.column,parsedLength);
    end;

  FUNCTION startsWith(CONST c:char):boolean; inline;
    begin result:=line[inputLocation.column]=c; end;
  FUNCTION startsWith(CONST prefix:string):boolean;  inline;
    begin result:=copy(line,inputLocation.column,length(prefix))=prefix; end;
  FUNCTION startsWith(CONST t:T_tokenType):boolean; inline;
    begin result:=copy(line,inputLocation.column,length(C_tokenInfo[t].defaultId))=C_tokenInfo[t].defaultId; end;
  PROCEDURE apply(CONST t:T_tokenType); inline;
    begin
      result^.tokType:=t;
      parsedLength:=length(C_tokenInfo[t].defaultId);
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
      end else if pos('TODO',commentText)>0 then messages^.postTextMessage(mt_el2_warning,inputLocation,commentText);
    end;

  VAR id:ansistring='';
      stringValue:ansistring='';
      tt:T_tokenType;
      tc:T_typeCheck;
      md:T_modifier;
      firstInLine:boolean;
  begin
    firstInLine:=inputLocation.column=1;
    result:=newToken(inputLocation,'',tt_EOL);
    with blob do if closer<>#0 then begin
      {$ifdef fullVersion}
      if localIdInfos<>nil then localIdInfos^.markBlobLine(inputLocation.line,closer);
      {$endif}
      //id now is rest of line
      id:=copy(line,inputLocation.column,length(line));
      if pos(closer,id)<=0 then begin
        parsedLength:=length(id);
        inc(inputLocation.column,parsedLength);
        if text='' then text:=id
                   else text:=text+C_lineBreakChar+id;
      end else begin
        parsedLength:=pos(closer,id)+length(closer)-1;
        inc(inputLocation.column,parsedLength);
        if text='' then text:=copy(id,1,pos(closer,id)-1)
                   else text:=text+C_lineBreakChar+copy(id,1,pos(closer,id)-1);
        result^.txt:=closer;
        closer:=#0;
        exit(result);
      end;
    end else if text<>'' then begin
      result^.location:=start;
      result^.tokType:=tt_literal;
      result^.data:=newStringLiteral(text);
      text:='';
      exit(result);
    end;
    if retainBlanks then begin
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
      disposeToken(result);
      exit(nil);
    end;
    case line[inputLocation.column] of
      '0'..'9': begin
        result^.data:=parseNumber(line,inputLocation.column, false,parsedLength);
        if parsedLength<=0 then begin
                                  fail('Cannot parse numeric literal '+line);
                                  disposeToken(result);
                                  exit(nil);
                                end
                           else result^.tokType:=tt_literal;
      end;
      '"','''','#': begin
        stringValue:=unescapeString(line,inputLocation.column,parsedLength);
        if parsedLength=0 then begin
          parsedLength:=1;
          while (parsedLength+inputLocation.column<=length(line)) and not(line[parsedLength+inputLocation.column] in [C_lineBreakChar,C_carriageReturnChar,'#']) do inc(parsedLength);
          id:=copy(line,inputLocation.column+length(BLOCK_COMMENT_DELIMITER),parsedLength-1);
          if (length(line)>=parsedLength+inputLocation.column) and (line[parsedLength+inputLocation.column]='#') then inc(parsedLength);
          if retainBlanks then begin
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
        if result^.txt=C_tokenInfo[tt].defaultId then result^.tokType:=tt;
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
        if retainBlanks then begin
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
      '=': if startsWith(tt_comparatorListEq)    then apply(tt_comparatorListEq)
                                                 else apply(tt_comparatorEq);
      '<': if startsWith(tt_comparatorNeq)       then apply(tt_comparatorNeq) else
           if startsWith(tt_comparatorLeq)       then apply(tt_comparatorLeq)
                                                 else apply(tt_comparatorLss);
      '!': if startsWith('!=')                   then apply(2,tt_comparatorNeq)
                                                 else apply(tt_unaryOpNegate);
      else begin
        fail('Cannot parse: '+copy(line,inputLocation.column,20)+' (first char is "'+line[inputLocation.column]+'"=#'+intToStr(ord(line[inputLocation.column]))+')');
        inputLocation.column:=length(line)+1;
        disposeToken(result);
        exit(nil);
      end;
    end;
    if parsedLength>0 then inc(inputLocation.column,parsedLength);
  end;

FUNCTION T_lexer.fetchNext(CONST messages:P_messages;
  {$ifdef fullVersion} CONST localIdInfos:P_localIdInfos;{$endif} CONST retainBlanks: boolean): boolean;
  FUNCTION fetch:P_token;
    begin
      result:=nil;
      while (result=nil) and (messages^.continueEvaluation) and (inputIndex<length(input)) do begin
        result:=getToken(input[inputIndex],messages{$ifdef fullVersion},localIdInfos{$endif},retainBlanks);
        if (result=nil) then begin
          inc(inputIndex);
          inc(inputLocation.line);
          inputLocation.column:=1;
        end else if not(retainBlanks) then case result^.tokType of
          tt_EOL: begin
            disposeToken(result);
            result:=nil;
          end;
          tt_docComment: begin
            myGenerics.append(nextStatement.comments ,result^.txt);
            disposeToken(result);
            result:=nil;
          end;
          tt_attributeComment: begin
            if (result^.txt<>'') then myGenerics.append(nextStatement.attributes,result^.txt);
            disposeToken(result);
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
    case nextToken^.tokType of
      tt_literal: if (beforeLastTokenized<>nil) and (beforeLastTokenized^.tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach,tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus])
                       and (lastTokenized<>nil) and (lastTokenized^.tokType in [tt_operatorMinus,tt_operatorPlus]) then begin
        if lastTokenized^.tokType=tt_operatorMinus
        then lastTokenized^.tokType:=tt_unaryOpMinus
        else begin
          disposeToken(lastTokenized);
          lastTokenized:=beforeLastTokenized;
        end;
      end;
      tt_identifier: if (associatedPackage<>nil) then begin
        if (associatedPackage^.isImportedOrBuiltinPackage(nextToken^.txt)) then begin
          n[1]:=fetch;
          if (n[1]<>nil) and (n[1]^.tokType=tt_ponFlipper) then begin
            n[2]:=fetch;
            if (n[2]<>nil) and (n[2]^.tokType=tt_identifier) then begin
              nextToken^.txt:=nextToken^.txt+ID_QUALIFY_CHARACTER+n[2]^.txt;
              associatedPackage^.resolveId(nextToken^,nil);
              disposeToken(n[1]);
              disposeToken(n[2]);
            end else begin
              associatedPackage^.resolveId(nextToken^,nil{$ifdef fullVersion},false{$endif});
              appendToken(nextToken);
              appendToken(n[1]);
              nextToken:=n[2];
            end;
          end else begin
            associatedPackage^.resolveId(nextToken^,nil{$ifdef fullVersion},false{$endif});
            appendToken(nextToken);
            nextToken:=n[1];
          end;
        end else associatedPackage^.resolveId(nextToken^,nil{$ifdef fullVersion},false{$endif});
        //This is a hack to ensure that "myPath" behaves nicely when including
        if (nextToken<>nil) and (nextToken^.tokType=tt_intrinsicRule) and (nextToken^.data=pointer(BUILTIN_MYPATH)) then nextToken^.location.package:=associatedPackage;
      end;
      tt_each,tt_parallelEach: begin
        n[1]:=fetch; n[2]:=fetch; n[3]:=fetch;
        if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) and
           (n[2]<>nil) and (n[2]^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_customTypeRule,tt_intrinsicRule]) and
           (n[3]<>nil) and (n[3]^.tokType=tt_separatorComma) then begin
          nextToken^.txt:=n[2]^.txt;
          nextToken^.data:=nil;
        end else messages^.raiseSimpleError('Invalid (p)Each construct. First argument must be an identifier. At least two arguments must be given.',nextToken^.location);
        disposeToken(n[1]);
        disposeToken(n[2]);
        disposeToken(n[3]);
      end;
      tt_agg: begin
        n[1]:=fetch;
        if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) then begin
          nextToken^.tokType:=tt_each;
          nextToken^.txt:='';
          nextToken^.data:=nil;
        end else messages^.raiseSimpleError('Invalid agg construct.',nextToken^.location);
        disposeToken(n[1]);
      end;
    end;
    result:=true;
    appendToken(nextToken);
  end;

CONSTRUCTOR T_lexer.create(CONST input_: T_arrayOfString; CONST location: T_tokenLocation; CONST inPackage: P_abstractPackage);
  begin
    input:=input_;
    inputIndex:=0;
    inputLocation:=location;
    inputLocation.column:=1;
    inputColumnOffset:=location.column-inputLocation.column;
    associatedPackage:=inPackage;

    blob.text:='';
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
    blob.text:='';
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
          localIdStack.scopePush(sc_block);
        tt_endBlock:
          localIdStack.scopePop(messages,t^.location,false);
        tt_braceOpen:
          localIdStack.scopePush(sc_bracketOnly);
        tt_braceClose:
          localIdStack.scopePop(messages,t^.location,true);
        tt_each,tt_parallelEach:begin
          localIdStack.scopePush(sc_each);
          localIdStack.addId(EACH_INDEX_IDENTIFIER,t^.location,tt_eachIndex);
          localIdStack.addId(t^.txt               ,t^.location,tt_eachParameter);
        end;
        tt_identifier, tt_importedUserRule,tt_localUserRule,tt_intrinsicRule:
          if lastWasLocalModifier then begin
            t^.tokType:=tt_blockLocalVariable;
            if not(localIdStack.addId(t^.txt,t^.location,tt_blockLocalVariable)) then messages^.raiseSimpleError('Invalid re-introduction of local variable "'+t^.txt+'"',t^.location);
          end else if (localIdStack.hasId(t^.txt,idType,idLoc)) then begin
            t^.tokType:=idType;
            if idType=tt_eachIndex then t^.location:=idLoc;
          end;
      end;
      lastWasLocalModifier:=(t^.tokType=tt_modifier) and (t^.getModifier=modifier_local);
      t:=t^.next;
    end;
    while not(localIdStack.scopeBottom) do localIdStack.scopePop(messages,lastLocation,false);
    localIdStack.destroy;
  end;

FUNCTION T_lexer.getNextStatement(CONST messages:P_messages{$ifdef fullVersion}; CONST localIdInfos:P_localIdInfos{$endif}): T_enhancedStatement;
  VAR localIdStack:T_idStack;
      lastWasLocalModifier:boolean=false;
      idType:T_tokenType;
      lastLocation:T_tokenLocation;
      idLoc:T_tokenLocation;
  begin
    localIdStack.create({$ifdef fullVersion}localIdInfos{$endif});
    while fetchNext(messages{$ifdef fullVersion},localIdInfos{$endif}) and (lastTokenized<>nil) do begin
      lastLocation:=lastTokenized^.location;
      case lastTokenized^.tokType of
        tt_beginBlock:
          localIdStack.scopePush(sc_block);
        tt_endBlock:
          localIdStack.scopePop(messages,lastTokenized^.location,false);
        tt_braceOpen:
          localIdStack.scopePush(sc_bracketOnly);
        tt_braceClose:
          localIdStack.scopePop(messages,lastTokenized^.location,true);
        tt_each,tt_parallelEach:begin
          localIdStack.scopePush(sc_each);
          localIdStack.addId(EACH_INDEX_IDENTIFIER,lastTokenized^.location,tt_eachIndex);
          localIdStack.addId(lastTokenized^.txt   ,lastTokenized^.location,tt_eachParameter);
        end;
        tt_identifier, tt_importedUserRule,tt_localUserRule,tt_intrinsicRule:
          if lastWasLocalModifier then begin
            lastTokenized^.tokType:=tt_blockLocalVariable;
            if not(localIdStack.addId(lastTokenized^.txt,lastTokenized^.location,tt_blockLocalVariable))
            then messages^.raiseSimpleError('Invalid re-introduction of local variable "'+lastTokenized^.txt+'"',lastTokenized^.location);
          end else if (localIdStack.hasId(lastTokenized^.txt,idType,idLoc)) then begin
            lastTokenized^.tokType:=idType;
            if idType=tt_eachIndex then lastTokenized^.location:=idLoc;
          end;
        tt_semicolon: if localIdStack.scopeBottom then begin
          if beforeLastTokenized<>nil then begin;
            beforeLastTokenized^.next:=nil;
            disposeToken(lastTokenized);
          end;
          result:=nextStatement;
          if not(messages^.continueEvaluation) then cascadeDisposeToken(result.firstToken);
          resetTemp;
          localIdStack.destroy;
          exit;
        end;
      end;
      lastWasLocalModifier:=(lastTokenized<>nil) and (lastTokenized^.tokType=tt_modifier) and (lastTokenized^.getModifier=modifier_local);
    end;
    result:=nextStatement;
    if not(messages^.continueEvaluation) then cascadeDisposeToken(result.firstToken);
    resetTemp;
    while not(localIdStack.scopeBottom) do localIdStack.scopePop(messages,lastLocation,false);
    localIdStack.destroy;
  end;

PROCEDURE safeAppend(VAR first,last:P_token; CONST appendix:P_token);
  begin
    if first=nil
    then first     :=appendix
    else last^.next:=appendix;
    last :=appendix^.last;
  end;

PROCEDURE T_lexer.rawTokenize(CONST inputTxt:string; CONST location:T_tokenLocation; VAR firstToken,lastToken:P_token; CONST messages:P_messages);
  begin
    input:=inputTxt;
    inputIndex:=0;
    inputLocation:=location;
    inputLocation.column:=1;
    inputColumnOffset:=location.column-inputLocation.column;
    blob.text:='';
    blob.closer:=#0;
    while fetchNext(messages{$ifdef fullVersion},nil{$endif}) do begin end;
    safeAppend(firstToken,lastToken,nextStatement.firstToken);
    resetTemp;
  end;

PROCEDURE T_lexer.rawTokenize(CONST literal:P_literal; CONST location:T_tokenLocation; VAR firstToken,lastToken:P_token);
  begin
    safeAppend(firstToken,lastToken,newToken(location,'',tt_literal,literal^.rereferenced));
  end;

{$ifdef fullVersion}
FUNCTION T_lexer.getEnhancedTokens(CONST localIdInfos:P_localIdInfos):T_enhancedTokens;
  VAR adapters:T_messagesDummy;
      t:P_token;
  begin
    blob.closer:=localIdInfos^.getBlobCloserOrZero(inputLocation.line);

    adapters.createDummy;
    while fetchNext(@adapters,nil,false) do begin end;
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
  end;

{$endif}

PROCEDURE T_abstractPackage.clearCustomOperators;
  VAR op:T_tokenType;
  begin
    for op:=low(T_customOperatorArray) to high(T_customOperatorArray) do customOperatorRules[op]:=nil;
  end;

FUNCTION T_abstractPackage.mergeCustomOps(CONST importedPackage:P_abstractPackage; CONST connector:P_messages):boolean;
  VAR op:T_tokenType;
  begin
    result:=false;
    for op:=low(T_customOperatorArray) to high(T_customOperatorArray) do
    if customOperatorRules[op] =nil then begin
      customOperatorRules[op]:=importedPackage^.customOperatorRules[op];
      result:=result or (customOperatorRules[op]<>nil);
    end else if (importedPackage^.customOperatorRules[op]<>nil) and (importedPackage^.customOperatorRules[op]<>customOperatorRules[op]) then begin
      connector^.postTextMessage(mt_el2_warning,customOperatorRules[op]^.getLocation,
        'Custom operator '+C_tokenInfo[op].defaultId+' hides operator defined '
        + ansistring(importedPackage^.customOperatorRules[op]^.getLocation));
    end;
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

PROCEDURE T_abstractPackage.resolveId(VAR token: T_token; CONST adaptersOrNil: P_messages{$ifdef fullVersion};CONST markAsUsed:boolean=true{$endif});
  VAR intrinsicFuncPtr:P_intFuncCallback;
      ruleId:T_idString;
  begin
    ruleId   :=token.txt;
    if intrinsicRuleMap.containsKey(ruleId,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRule;
      token.data:=intrinsicFuncPtr;
      exit;
    end;
    if adaptersOrNil<>nil then adaptersOrNil^.raiseSimpleError('Cannot resolve ID "'+token.txt+'"',token.location);
  end;

PROCEDURE T_extendedPackage.resolveId(VAR token:T_token; CONST adaptersOrNil:P_messages{$ifdef fullVersion};CONST markAsUsed:boolean=true{$endif});
  begin
    extender^.resolveId(token,adaptersOrNil{$ifdef fullVersion},markAsUsed{$endif});
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

FUNCTION T_abstractPackage.literalToString(CONST L:P_literal; CONST location:T_tokenLocation; CONST context:pointer):string;
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
      t:P_token;
  begin
    location.package:=@BLANK_ABSTRACT_PACKAGE;
    location.line:=0;
    location.column:=1;
    lexer.create(inputString,location,@BLANK_ABSTRACT_PACKAGE);
    adapters.createDummy;
    repeat until not(lexer.fetchNext(@adapters{$ifdef fullVersion},nil{$endif},true));
    adapters.destroy;
    t:=lexer.nextStatement.firstToken;
    lexer.resetTemp;
    lexer.destroy;
    setLength(result,0);
    while t<>nil do begin
      setLength(result,length(result)+1);
      BLANK_ABSTRACT_PACKAGE.resolveId(t^,nil);
      result[length(result)-1]:=t^.getRawToken;
      t:=disposeToken(t);
    end;
  end;
{$endif}

INITIALIZATION
  BLANK_ABSTRACT_PACKAGE.create(newVirtualFileCodeProvider('',C_EMPTY_STRING_ARRAY));
  MNH_PSEUDO_PACKAGE.create();
  {$ifdef fullVersion}
  rawTokenizeCallback:=@tokenizeAllReturningRawTokens;
  {$endif}

FINALIZATION
  BLANK_ABSTRACT_PACKAGE.destroy;
  MNH_PSEUDO_PACKAGE.destroy;

end.
