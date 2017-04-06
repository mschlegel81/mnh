UNIT mnh_tokenArray;
INTERFACE
USES sysutils,math,
     myGenerics,myStringUtil,
     mnh_basicTypes,mnh_constants,
     mnh_fileWrappers,
     mnh_litVar,
     mnh_funcs,
     tokenStack,
     {$ifdef fullVersion}
     mnh_html,
     {$endif}
     mnh_tokens,mnh_out_adapters;
TYPE
  P_abstractPackage=^T_abstractPackage;
  T_abstractPackage=object(T_objectWithPath)
    private
      codeProvider:P_codeProvider;
      readyForCodeState:T_hashInt;
    protected
      PROCEDURE logReady;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider);
      DESTRUCTOR destroy; virtual;
      FUNCTION isImportedOrBuiltinPackage(CONST id:string):boolean; virtual; abstract;
      PROCEDURE replaceCodeProvider(CONST newProvider:P_codeProvider);
      FUNCTION codeChanged:boolean;
      FUNCTION getId:T_idString; virtual;
      FUNCTION getPath:ansistring; virtual;
      PROPERTY getCodeProvider:P_codeProvider read codeProvider;
  end;

  T_enhancedStatement=record
    comments,
    attributes:T_arrayOfString;
    firstToken:P_token;
  end;

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
      FUNCTION getToken(CONST line:ansistring; VAR recycler:T_tokenRecycler; VAR adapters:T_adapters; CONST retainBlanks:boolean=false):P_token;
      FUNCTION fetchNext(VAR recycler:T_tokenRecycler; VAR adapters:T_adapters; CONST retainBlanks:boolean=false):boolean;
      PROCEDURE resetTemp;
    public
      CONSTRUCTOR create(CONST input_:T_arrayOfString; CONST location:T_tokenLocation; CONST inPackage:P_abstractPackage);
      CONSTRUCTOR create(CONST package:P_abstractPackage);
      DESTRUCTOR destroy; virtual;
      FUNCTION getNextStatement(VAR recycler:T_tokenRecycler; VAR adapters:T_adapters):T_enhancedStatement; virtual;
      FUNCTION getTokenAtColumnOrNil(CONST startColumnIndex:longint; OUT endColumnIndex:longint):P_token;
  end;

IMPLEMENTATION

FUNCTION T_lexer.getToken(CONST line: ansistring;
  VAR recycler: T_tokenRecycler; VAR adapters: T_adapters;
  CONST retainBlanks: boolean): P_token;
  VAR parsedLength:longint=0;

  PROCEDURE fail(message:ansistring);
    begin
      adapters.raiseError(message,inputLocation);
    end;

  FUNCTION leadingId:ansistring;
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

  VAR id:ansistring='';
      stringValue:ansistring='';
      tt:T_tokenType;
  begin
    result:=recycler.newToken(inputLocation,'',tt_EOL);
    with blob do if closer<>#0 then begin
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
      '"','''','#': begin
        stringValue:=unescapeString(line,inputLocation.column,parsedLength);
        if parsedLength=0 then begin
          fail('Cannot parse string literal '+line);
          recycler.disposeToken(result);
          exit(nil);
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
          result^.tokType:=tt_EOL;
          if startsWith(DOC_COMMENT_PREFIX) then begin
            result^.txt:=trimRight(copy(line,length(DOC_COMMENT_PREFIX)+inputLocation.column,length(line)-length(DOC_COMMENT_PREFIX)+1-inputLocation.column));
            result^.tokType:=tt_docComment;
          end else if startsWith(ATTRIBUTE_COMMENT_PREFIX) then begin
            result^.txt:=trim(copy(line,length(ATTRIBUTE_COMMENT_PREFIX)+inputLocation.column,length(line)-length(ATTRIBUTE_COMMENT_PREFIX)+1-inputLocation.column));
            result^.tokType:=tt_attributeComment;
          end else if startsWith(SPECIAL_COMMENT_BLOB_BEGIN) then begin
            result^.txt:=SPECIAL_COMMENT_BLOB_BEGIN;
            blob.start:=inputLocation;
            if length(line)>=inputLocation.column+length(SPECIAL_COMMENT_BLOB_BEGIN)
            then begin
              blob.closer:=line[inputLocation.column+length(SPECIAL_COMMENT_BLOB_BEGIN)];
              parsedLength:=length(SPECIAL_COMMENT_BLOB_BEGIN)+1;
            end else blob.closer:='''';
          end;
        end;
      end else if startsWith(tt_cso_assignDiv) then apply(tt_cso_assignDiv)
                                               else apply(tt_operatorDivReal);
      ':': if startsWith(tt_assign)            then apply(tt_assign)
      else if startsWith(tt_pseudoFuncPointer) then apply(tt_pseudoFuncPointer)
      else if (length(line)>=inputLocation.column+3) and (line[inputLocation.column+1] in ['b','c','e','i','l','n','s','r','k','f','m']) then begin
        id:=leadingId;
        result^.tokType:=tt_iifElse;
        for tt in C_typeChecks do if id=C_tokenInfo[tt].defaultId then result^.tokType:=tt;
        if result^.tokType=tt_iifElse then parsedLength:=1;
      end else apply(tt_iifElse);
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
      '@':                                            apply(tt_listToParameterList);
      ')':                                            apply(tt_braceClose);
      '(':                                            apply(tt_braceOpen);
      '|': if startsWith(tt_cso_assignAppend)    then apply(tt_cso_assignAppend)
                                                 else apply(tt_operatorConcat);
      '+': if startsWith(tt_cso_assignPlus)      then apply(tt_cso_assignPlus)
                                                 else apply(tt_operatorPlus);
      '&': if startsWith(tt_cso_assignStrConcat) then apply(tt_cso_assignStrConcat)
                                                 else apply(tt_operatorStrConcat);
      '-': if startsWith(tt_declare)             then apply(tt_declare) else
           if startsWith(tt_cso_assignMinus)     then apply(tt_cso_assignMinus)
                                                 else apply(tt_operatorMinus);
      '*': if startsWith('**')                   then apply(2,tt_operatorPot) else
           if startsWith(tt_cso_assignMult)      then apply(tt_cso_assignMult)
                                                 else apply(tt_operatorMult);
      '>': if startsWith(tt_comparatorGeq)       then apply(tt_comparatorGeq) else
           if startsWith(tt_cso_mapDrop)         then apply(tt_cso_mapDrop)
                                                 else apply(tt_comparatorGrt);
      '=': if startsWith(tt_comparatorListEq)    then apply(tt_comparatorListEq)
                                                 else apply(tt_comparatorEq);
      '<': if startsWith(tt_comparatorNeq)       then apply(tt_comparatorNeq) else
           if startsWith(tt_cso_mapPut)          then apply(tt_cso_mapPut) else
           if startsWith(tt_comparatorLeq)       then apply(tt_comparatorLeq)
                                                 else apply(tt_comparatorLss);
      '!': if startsWith('!=')                   then apply(2,tt_comparatorNeq)
           else begin
             fail('Cannot parse: '+copy(line,inputLocation.column,20)+' (first char is "'+line[inputLocation.column]+'"=#'+intToStr(ord(line[inputLocation.column]))+')');
             inputLocation.column:=length(line)+1;
             recycler.disposeToken(result);
             exit(nil);
           end;
      else begin
        fail('Cannot parse: '+copy(line,inputLocation.column,20)+' (first char is "'+line[inputLocation.column]+'"=#'+intToStr(ord(line[inputLocation.column]))+')');
        inputLocation.column:=length(line)+1;
        recycler.disposeToken(result);
        exit(nil);
      end;
    end;
    if parsedLength>0 then inc(inputLocation.column,parsedLength);
  end;

FUNCTION T_lexer.fetchNext(VAR recycler: T_tokenRecycler;
  VAR adapters: T_adapters; CONST retainBlanks: boolean): boolean;
  FUNCTION fetch:P_token;
    begin
      result:=nil;
      while (result=nil) and (adapters.noErrors) and (inputIndex<length(input)) do begin
        result:=getToken(input[inputIndex],recycler,adapters,retainBlanks);
        if (result=nil) then begin
          inc(inputIndex);
          inc(inputLocation.line);
          inputLocation.column:=1;
        end else if not(retainBlanks) then case result^.tokType of
          tt_EOL: begin
            recycler.disposeToken(result);
            result:=nil;
          end;
          tt_docComment: begin
            myGenerics.append(nextStatement.comments ,result^.txt);
            recycler.disposeToken(result);
            result:=nil;
          end;
          tt_attributeComment: begin
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
    if not(retainBlanks) then case nextToken^.tokType of
      tt_literal: if (beforeLastTokenized<>nil) and (beforeLastTokenized^.tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach,tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus])
                       and (lastTokenized<>nil) and (lastTokenized^.tokType in [tt_operatorMinus,tt_operatorPlus]) then begin
        if lastTokenized^.tokType=tt_operatorMinus
        then lastTokenized^.tokType:=tt_unaryOpMinus
        else begin
          recycler.disposeToken(lastTokenized);
          lastTokenized:=beforeLastTokenized;
        end;
      end;

{        tt_operatorMinus,tt_operatorPlus: if (stepIndex<tokenFill-1) and
         ((stepIndex=0) or (token[stepIndex-1].tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach,tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus])) and
         (token[stepIndex+1].tokType in [tt_literal,tt_identifier,tt_braceOpen,tt_listBraceOpen,tt_expBraceOpen,tt_localUserRule,tt_importedUserRule,tt_customTypeRule,tt_intrinsicRule,tt_parameterIdentifier]) then begin
          if token[stepIndex].tokType=tt_operatorMinus
          then token[stepIndex].tokType:=tt_unaryOpMinus //unary - is special token
          else inc(stepIndex);                       //unary + is skipped
        end;}
      tt_identifier: if (associatedPackage<>nil) then begin
        if (associatedPackage^.isImportedOrBuiltinPackage(nextToken^.txt)) then begin
          n[1]:=fetch;
          if (n[1]<>nil) and (n[1]^.tokType=tt_ponFlipper) then begin
            n[2]:=fetch;
            if (n[2]<>nil) and (n[2]^.tokType=tt_identifier) then begin
              nextToken^.txt:=nextToken^.txt+ID_QUALIFY_CHARACTER+n[2]^.txt;
              nextToken^.resolveRuleId(associatedPackage,nil);
              recycler.disposeToken(n[1]);
              recycler.disposeToken(n[2]);
            end else begin
              nextToken^.resolveRuleId(associatedPackage,nil);
              appendToken(nextToken);
              appendToken(n[1]);
              nextToken:=n[2];
            end;
          end else begin
            nextToken^.resolveRuleId(associatedPackage,nil);
            appendToken(nextToken);
            nextToken:=n[1];
          end;
        end else nextToken^.resolveRuleId(associatedPackage,nil);
      end;
      tt_each,tt_parallelEach: begin
        n[1]:=fetch; n[2]:=fetch; n[3]:=fetch;
        if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) and
           (n[2]<>nil) and (n[2]^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_customTypeRule,tt_intrinsicRule]) and
           (n[3]<>nil) and (n[3]^.tokType=tt_separatorComma) then begin
          nextToken^.txt:=n[2]^.txt;
          nextToken^.data:=nil;
        end else adapters.raiseError('Invalid (p)Each construct. First argument must be an identifier. At least two arguments must be given.',nextToken^.location);
        recycler.disposeToken(n[1]);
        recycler.disposeToken(n[2]);
        recycler.disposeToken(n[3]);
      end;
      tt_agg: begin
        n[1]:=fetch;
        if (n[1]<>nil) and (n[1]^.tokType=tt_braceOpen) then begin
          nextToken^.tokType:=tt_each;
          nextToken^.txt:='';
          nextToken^.data:=nil;
        end else adapters.raiseError('Invalid agg construct.',nextToken^.location);
        recycler.disposeToken(n[1]);
      end;
    end;
    result:=true;
    appendToken(nextToken);
  end;

CONSTRUCTOR T_lexer.create(CONST input_: T_arrayOfString;
  CONST location: T_tokenLocation; CONST inPackage: P_abstractPackage);
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

CONSTRUCTOR T_lexer.create(CONST package: P_abstractPackage);
  begin
    input:=package^.getCodeProvider^.getLines;
    inputIndex:=0;
    inputLocation.package:=package;
    inputLocation.column:=1;
    inputLocation.line:=1;
    inputColumnOffset:=0;
    associatedPackage:=package;
    blob.text:='';
    blob.closer:=#0;
    resetTemp;
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

FUNCTION T_lexer.getNextStatement(VAR recycler: T_tokenRecycler;
  VAR adapters: T_adapters): T_enhancedStatement;
  VAR localIdStack:T_idStack;
      lastWasLocalModifier:boolean=false;
  begin
    localIdStack.create;
    while fetchNext(recycler,adapters) and (lastTokenized<>nil) do case lastTokenized^.tokType of
      tt_beginBlock: begin
        localIdStack.clear;
        localIdStack.scopePush;
        lastWasLocalModifier:=false;
        while fetchNext(recycler,adapters) and not((lastTokenized^.tokType=tt_endBlock) and (localIdStack.oneAboveBottom)) do begin
          case lastTokenized^.tokType of
            tt_beginBlock    : localIdStack.scopePush;
            tt_endBlock      : localIdStack.scopePop;
            tt_identifier, tt_importedUserRule,tt_localUserRule,tt_intrinsicRule:
              if lastWasLocalModifier then begin
                lastTokenized^.tokType:=tt_blockLocalVariable;
                localIdStack.addId(lastTokenized^.txt);
              end else if (localIdStack.hasId(lastTokenized^.txt)) then
                lastTokenized^.tokType:=tt_blockLocalVariable;
          end;
          lastWasLocalModifier:=lastTokenized^.tokType=tt_modifier_local;
        end;
      end;
      tt_semicolon: begin
        if beforeLastTokenized<>nil then begin;
          beforeLastTokenized^.next:=nil;
          recycler.disposeToken(lastTokenized);
        end;
        result:=nextStatement;
        resetTemp;
        localIdStack.destroy;
        exit;
      end;
    end;
    result:=nextStatement;
    resetTemp;
    localIdStack.destroy;
  end;

FUNCTION T_lexer.getTokenAtColumnOrNil(CONST startColumnIndex:longint; OUT endColumnIndex:longint): P_token;
  VAR recycler:T_tokenRecycler;
      adapters:T_adapters;
  begin
    recycler.create;
    adapters.create;
    while (inputLocation.column<=startColumnIndex) and (fetchNext(recycler,adapters,false)) do begin end;
    result:=nextStatement.firstToken;
    while (result<>nil) and (result^.location.column<startColumnIndex) do result:=result^.next;
    if result=nil then begin
      result:=lastTokenized;
      if result=nil then exit;
    end;
    if result^.next=nil then endColumnIndex:=inputLocation.column
                        else endColumnIndex:=result^.next^.location.column;
    recycler.destroy;
    adapters.destroy;
  end;

CONSTRUCTOR T_abstractPackage.create(CONST provider: P_codeProvider);
  begin
    codeProvider:=provider;
    readyForCodeState:=0;
  end;

DESTRUCTOR T_abstractPackage.destroy;
begin
  if codeProvider^.disposeOnPackageDestruction then dispose(codeProvider,destroy);
  codeProvider:=nil;
end;

PROCEDURE T_abstractPackage.replaceCodeProvider(CONST newProvider: P_codeProvider);
  begin
    if (codeProvider<>nil) and (codeProvider^.disposeOnPackageDestruction) then dispose(codeProvider,destroy);
    codeProvider:=newProvider;
    readyForCodeState:=0;
  end;

FUNCTION T_abstractPackage.codeChanged: boolean;  begin result:=readyForCodeState<>codeProvider^.stateHash; end;
PROCEDURE T_abstractPackage.logReady;             begin         readyForCodeState:=codeProvider^.stateHash; end;
FUNCTION T_abstractPackage.getId: T_idString;     begin result:=codeProvider^.id;                           end;
FUNCTION T_abstractPackage.getPath: ansistring;   begin result:=codeProvider^.getPath;                      end;

{$ifdef fullVersion}
FUNCTION tokenizeAllReturningRawTokens(CONST inputString:ansistring):T_rawTokenArray;
  VAR lexer:T_lexer;
      location:T_tokenLocation;
      adapters:T_adapters;
      recycler:T_tokenRecycler;
      t:P_token;
  begin
    location.package:=nil;
    location.line:=0;
    location.column:=1;
    lexer.create(inputString,location,nil);
    adapters.create;
    recycler.create;
    repeat until not(lexer.fetchNext(recycler,adapters,true));
    adapters.destroy;
    t:=lexer.nextStatement.firstToken;
    lexer.resetTemp;
    lexer.destroy;
    setLength(result,0);
    while t<>nil do begin
      setLength(result,length(result)+1);
      result[length(result)-1]:=t^.getRawToken;
      t:=recycler.disposeToken(t);
    end;
    recycler.destroy;
  end;
{$endif}

INITIALIZATION
  {$ifdef fullVersion}
  rawTokenizeCallback:=@tokenizeAllReturningRawTokens;
  {$endif}

end.
