UNIT tokens;
INTERFACE
USES //basic classes
     sysutils,
     //my utilities
     myGenerics,
     serializationUtil,
     //MNH:
     basicTypes,
     mnh_constants,
     mnh_messages,
     out_adapters,
     litVar;
TYPE
  {$ifdef fullVersion}
  T_rawToken=record txt:string; tokType:T_tokenType; end;
  T_rawTokenArray=array of T_rawToken;
  {$endif}

  P_token=^T_token;
  T_tokenRange=record
    first,last:P_token;
  end;
  PP_token=^P_token;
  T_token=object
    next    :P_token;
    location:T_tokenLocation;
    txt     :T_idString;
    tokType :T_tokenType;
    data    :pointer;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; CONST tokenText:T_idString; CONST tokenType:T_tokenType; CONST ptr:pointer=nil);
    PROCEDURE define(CONST original:T_token; VAR literalRecycler:T_literalRecycler); {$ifndef debugMode}{$ifndef profilingFlavour}inline;{$endif}{$endif}
    PROCEDURE undefine(VAR literalRecycler:T_literalRecycler); {$ifndef debugMode}{$ifndef profilingFlavour}inline;{$endif}{$endif}
    FUNCTION last:P_token;
    FUNCTION getCount:longint;
    FUNCTION toString(CONST lastWasIdLike:boolean; OUT idLike:boolean; CONST limit:longint=maxLongint):ansistring;
    FUNCTION hash:T_hashInt;
    FUNCTION equals(CONST other:T_token):boolean;
    FUNCTION singleTokenToString:ansistring;
    FUNCTION getTokenOnBracketLevel(CONST types:T_tokenTypeSet; CONST onLevel:longint; CONST initialLevel:longint=0):P_token;
    {$ifdef fullVersion}
    FUNCTION getRawToken:T_rawToken;
    {$endif}
    PROCEDURE setSingleLocationForExpression(CONST loc:T_tokenLocation);
    PROCEDURE injectAfter(CONST newToken:P_token);
    PROCEDURE injectAfter(CONST range:T_tokenRange);

    FUNCTION getTypeCheck:T_typeCheck;
    PROCEDURE setTypeCheck(CONST check:T_typeCheck);
    FUNCTION getModifier:T_modifier;
    PROCEDURE setModifier(CONST modifier:T_modifier);

    FUNCTION serializeSingleToken(VAR literalRecycler:T_literalRecycler; CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream:P_outputStreamWrapper):boolean;
    PROCEDURE deserializeSingleToken(VAR literalRecycler:T_literalRecycler; CONST locationOfDeserializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream: P_inputStreamWrapper; VAR typeMap:T_typeMap);
  end;

  T_bodyParts=array of T_tokenRange;

CONST EMPTY_TOKEN_RANGE:T_tokenRange=(first:nil;last:nil);
VAR patternToString:FUNCTION (CONST pattern:pointer):ansistring=nil;
    disposePattern :PROCEDURE(VAR pattern:pointer; VAR literalRecycler: T_literalRecycler)=nil;
    clonePattern   :FUNCTION (CONST pattern:pointer):pointer     =nil;
FUNCTION tokensToString(CONST first:P_token; CONST limit:longint=maxLongint):ansistring;
FUNCTION tokensToStrings(CONST first:P_token; CONST limit:longint=maxLongint):T_arrayOfString;
FUNCTION tokensToEcho(CONST first:P_token):T_arrayOfString;
FUNCTION safeTokenToString(CONST t:P_token):ansistring;
FUNCTION getBodyParts(CONST first:P_token; CONST initialBracketLevel:longint; CONST context:P_abstractContext; OUT closingBracket:P_token; CONST acceptedClosers:T_tokenTypeSet=[tt_braceClose]):T_bodyParts;
IMPLEMENTATION
USES typinfo,myStringUtil;
FUNCTION tokensToString(CONST first:P_token; CONST limit:longint):ansistring;
  begin
    result:=join(tokensToStrings(first,limit),'');
  end;

FUNCTION tokensToEcho(CONST first:P_token):T_arrayOfString;
  begin
    result:=tokensToStrings(first);
    append(result,';');
  end;

FUNCTION tokensToStrings(CONST first:P_token; CONST limit:longint=maxLongint):T_arrayOfString;
  VAR p:P_token;
      idLike,prevIdLike:boolean;
      remainingLength:longint;
      resultFill:longint=0;
  begin
    setLength(result,100);
    prevIdLike:=false;
    remainingLength:=limit;
    p:=first;
    while (p<>nil) and (remainingLength>0) do begin
      if resultFill>=length(result) then setLength(result,round(length(result)*1.1));
      result[resultFill]:=p^.toString(prevIdLike,idLike,remainingLength);
      remainingLength-=length(result[resultFill]);
      inc(resultFill);
      prevIdLike:=idLike;
      p:=p^.next;
    end;
    if (p<>nil) or (remainingLength<=0) then begin
      if resultFill>=length(result) then setLength(result,resultFill+1);
      result[resultFill]:=' ...';
    end;
    setLength(result,resultFill);
  end;

FUNCTION safeTokenToString(CONST t:P_token):ansistring;
  begin
    if t=nil then result:='<EOL>'
    else result:=t^.singleTokenToString;
  end;

FUNCTION getBodyParts(CONST first:P_token; CONST initialBracketLevel:longint; CONST context:P_abstractContext; OUT closingBracket:P_token; CONST acceptedClosers:T_tokenTypeSet=[tt_braceClose]):T_bodyParts;
  VAR t,p:P_token;
      bracketLevel,i:longint;
      partLength:longint=-1;
  begin
    closingBracket:=nil;
    bracketLevel:=initialBracketLevel;
    t:=first; p:=nil;
    if (first^.next<>nil) and (first^.next^.tokType<>tt_separatorComma) then begin
      setLength(result,1);
      result[0].first:=first^.next;
    end else begin
      context^.raiseError('Invalid special construct; Cannot find closing bracket.',first^.location);
      setLength(result,0);
      exit;
    end;
    while (t<>nil) and not((t^.tokType in acceptedClosers) and (bracketLevel=1)) do begin
      if t^.tokType in C_openingBrackets then inc(bracketLevel)
      else if t^.tokType in C_closingBrackets then dec(bracketLevel)
      else if (t^.tokType=tt_separatorComma) and (bracketLevel=1) then begin
        if partLength=0 then context^.raiseError('Empty body part.',result[length(result)-1].first^.location);
        result[length(result)-1].last:=p; //end of body part is token before comma
        setLength(result,length(result)+1);
        result[length(result)-1].first:=t^.next; //start of next body part is token after comma
        if (t^.next^.tokType in C_closingBrackets) and (bracketLevel=1) then context^.raiseError('Empty body part.',result[length(result)-1].first^.location);
        partLength:=-1; //excluding delimiting separators
      end;
      p:=t; t:=t^.next; inc(partLength);
    end;
    if not(context^.continueEvaluation) then begin
      setLength(result,0);
      exit;
    end;
    result[length(result)-1].last:=p; //end of body part is token before comma
    if (p=nil) or (t=nil) or not(t^.tokType in acceptedClosers) or (bracketLevel<>1) then begin
      context^.raiseError('Invalid special construct; Cannot find closing bracket...',first^.location);
      setLength(result,0);
      exit;
    end;
    closingBracket:=t;
    for i:=0 to length(result)-1 do begin
      if result[i].last^.next<>closingBracket then dispose(result[i].last^.next,destroy);
      result[i].last^.next:=nil;
    end;
  end;

CONSTRUCTOR T_token.create;
  begin
  end;

DESTRUCTOR T_token.destroy;
  begin
    assert(data=nil,'Call undefine before destroy!');
  end;

PROCEDURE T_token.define(CONST tokenLocation: T_tokenLocation; CONST tokenText: T_idString; CONST tokenType: T_tokenType; CONST ptr: pointer);
  {$ifdef debugMode}VAR idLikeDummy:boolean;{$endif}
  begin
    location:=tokenLocation;
    if (tokenText='') and (C_tokenDefaultId[tokenType]<>'')
      then txt:=C_tokenDefaultId[tokenType]
      else txt:=tokenText;
    tokType:=tokenType;
    data:=ptr;
    {$ifdef debugMode}
    if (ptr=nil) and (tokenType=tt_literal) then raise Exception.create('Creating literal token without data in location @'+intToStr(tokenLocation.line)+':'+intToStr(tokenLocation.column)+'; Text is: '+toString(false,idLikeDummy));
    if (tokenLocation.package=nil) then raise Exception.create('Creating token without package in location @'+intToStr(tokenLocation.line)+':'+intToStr(tokenLocation.column)+'; Text is: '+toString(false,idLikeDummy));
    {$endif}
  end;

PROCEDURE T_token.define(CONST original: T_token; VAR literalRecycler:T_literalRecycler);
  {$ifdef debugMode}VAR idLikeDummy:boolean;{$endif}
  begin
    location:=original.location;
    txt     :=original.txt;
    tokType :=original.tokType;
    data    :=original.data;
    case tokType of
      tt_literal,tt_aggregatorExpressionLiteral,tt_parList: P_literal(data)^.rereference;
      tt_each,tt_parallelEach: if data<>nil then P_literal(data)^.rereference;
      tt_list_constructor,tt_parList_constructor: if data=nil then data:=literalRecycler.newListLiteral else data:=P_listLiteral(original.data)^.clone(@literalRecycler);
      tt_functionPattern: data:=clonePattern(original.data);
    end;
    {$ifdef debugMode}
    if (data=nil) and (tokType=tt_literal) then raise Exception.create('Creating literal token without data in location @'+intToStr(location.line)+':'+intToStr(location.column)+'; Text is: '+toString(false,idLikeDummy));
    if (location.package=nil)then raise Exception.create('Creating token without package in location @'+intToStr(location.line)+':'+intToStr(location.column)+'; Text is: '+toString(false,idLikeDummy));
    {$endif}
  end;

PROCEDURE T_token.undefine(VAR literalRecycler:T_literalRecycler);
  begin
    case tokType of
      tt_literal,tt_aggregatorExpressionLiteral,tt_list_constructor,tt_parList_constructor,tt_parList: literalRecycler.disposeLiteral(data);
      tt_each,tt_parallelEach: if data<>nil then literalRecycler.disposeLiteral(data);
      tt_functionPattern: disposePattern(data,literalRecycler);
    end;
    data:=nil;
    tokType:=tt_EOL;
    location.package:=nil;
    location.column:=0;
    location.line:=0;
  end;

FUNCTION T_token.last: P_token;
  begin
    result:=@self;
    while result^.next<>nil do result:=result^.next;
  end;

FUNCTION T_token.getCount: longint;
  VAR p:P_token;
  begin
    p:=@self;
    result:=0;
    while p<>nil do begin
      p:=p^.next;
      inc(result);
    end;
  end;

FUNCTION T_token.toString(CONST lastWasIdLike: boolean; OUT idLike: boolean; CONST limit:longint=maxLongint): ansistring;
  begin
    idLike:=false;
    case tokType of
      tt_each, tt_parallelEach: begin
        result:=C_tokenDefaultId[tokType];
        if txt<>'' then result:=result+'('+txt+','
                   else result:=C_tokenDefaultId[tt_agg]+'(';
        if data<>nil then result:=result+P_literal(data)^.toString(limit-6)+',';
      end;
      tt_customTypeCheck: result:=':'+txt;
      tt_aggregatorExpressionLiteral,
      tt_literal            : result:=P_literal    (data)^.toString(limit);
      tt_parList_constructor: result:=toParameterListString(P_listLiteral(data),false,limit);
      tt_parList            : result:=toParameterListString(P_listLiteral(data),true ,limit);
      tt_list_constructor   : result:=P_listLiteral(data)^.listConstructorToString(limit);
      tt_assignNewBlockLocal: result:=C_modifierInfo[modifier_local].name+' '+txt+C_tokenDefaultId[tokType];
      tt_beginRule,tt_beginExpression:result:=C_tokenDefaultId[tt_beginBlock]+'* ';
      tt_endRule  ,tt_endExpression  :result:=C_tokenDefaultId[tt_endBlock  ]+'* ';
      tt_mutate, tt_assignExistingBlockLocal..tt_mut_nestedDrop: result:=txt+C_tokenDefaultId[tokType];
      tt_type:      result:=    C_typeCheckInfo[getTypeCheck].name;
      tt_typeCheck: result:=':'+C_typeCheckInfo[getTypeCheck].name;
      tt_modifier : result:=C_modifierInfo[getModifier].name;
      tt_identifier,
      tt_userRule,
      tt_customType,
      tt_globalVariable,
      tt_intrinsicRule,
      tt_rulePutCacheValue,
      tt_parameterIdentifier,
      tt_blockLocalVariable,
      tt_eachIndex,
      tt_eachParameter,
      tt_blank: result:=txt;
      tt_attributeComment:result:=ATTRIBUTE_PREFIX+txt;
      tt_docComment      :result:=COMMENT_PREFIX+txt;
      tt_functionPattern :result:=patternToString(data);
      else result:=C_tokenDefaultId[tokType];
    end;
    if length(result)<1 then begin
      idLike:=false; exit(result);
    end;
    if lastWasIdLike and (result[1] in ['$','a'..'z','A'..'Z','?',':','0'..'9'])
      or (tokType in [tt_operatorAnd,tt_operatorDivInt,tt_operatorIn,tt_operatorNotIn,tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorMod,tt_operatorOr,tt_operatorXor,tt_operatorOrElse,tt_iifCheck,tt_iifElse])
    then result:=' '+result;
    idLike:=(result[length(result)] in ['a'..'z','A'..'Z','?',':','_']) or (tokType in [tt_separatorComma,tt_semicolon]);
  end;

FUNCTION T_token.hash: T_hashInt;
  begin
    result:=T_hashInt(tokType);
    if tokType in [tt_intrinsicRule,tt_userRule,tt_rulePutCacheValue,tt_typeCheck] then result:=result*31+T_hashInt(data);
    if (tokType in [tt_each,tt_parallelEach,tt_customTypeCheck,tt_assignNewBlockLocal,
                    tt_mutate,tt_assignExistingBlockLocal..tt_mut_nestedDrop,
                    tt_identifier,tt_parameterIdentifier,tt_blockLocalVariable,tt_blank])
    then result:=result*37+hashOfAnsiString(txt);
    if (tokType in [tt_each, tt_parallelEach,tt_aggregatorExpressionLiteral,tt_literal,
                    tt_parList_constructor,tt_parList,tt_list_constructor])
    then begin
      result:=result*41;
      if data<>nil then result:=result+P_literal(data)^.hash;
    end;
  end;

FUNCTION T_token.equals(CONST other: T_token): boolean;
  FUNCTION literalEquals(CONST x,y:P_literal):boolean; inline;
    begin
      result:=(x=nil) and (y=nil) or
              (x<>nil) and (y<>nil) and (x^.equals(y));
    end;

  begin
    if tokType<>other.tokType then exit(false);
    if tokType in [tt_intrinsicRule,tt_userRule,tt_rulePutCacheValue,tt_typeCheck] then exit(data=other.data);
    if (tokType in [tt_each,tt_parallelEach,tt_customTypeCheck,tt_assignNewBlockLocal,
                    tt_mutate,tt_assignExistingBlockLocal..tt_mut_nestedDrop,
                    tt_identifier,tt_parameterIdentifier,tt_blockLocalVariable,tt_blank])
       and (txt<>other.txt) then exit(false);
    if (tokType in [tt_each, tt_parallelEach,tt_aggregatorExpressionLiteral,tt_literal,
                    tt_parList_constructor,tt_parList,tt_list_constructor])
       and not(literalEquals(data,other.data)) then exit(false);
    result:=true;
  end;

FUNCTION T_token.singleTokenToString: ansistring;
  VAR dummy:boolean;
  begin
    result:=toString(false,dummy);
    if tokType in [tt_operatorAnd,tt_operatorDivInt,tt_operatorIn,tt_operatorNotIn,tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorMod,tt_operatorOr,tt_operatorXor,tt_iifCheck,tt_iifElse,tt_operatorOrElse]
    then result:=trim(result);
  end;

FUNCTION T_token.getTokenOnBracketLevel(CONST types: T_tokenTypeSet; CONST onLevel: longint; CONST initialLevel: longint): P_token;
  VAR level:longint=0;
      t:P_token;
  begin
    level:=initialLevel;
    t:=@self;
    while (t<>nil) do begin
      if t^.tokType      in C_openingBrackets then inc(level)
      else if t^.tokType in C_closingBrackets then dec(level);
      if (level=onLevel) and (t^.tokType in types) then exit(t);
      t:=t^.next;
    end;
    result:=nil;
  end;

{$ifdef fullVersion}
FUNCTION T_token.getRawToken: T_rawToken;
  begin
    result.tokType:=tokType;
    result.txt:=singleTokenToString;
  end;
{$endif}

PROCEDURE T_token.setSingleLocationForExpression(CONST loc: T_tokenLocation);
  VAR t:P_token;
  begin
    t:=@self;
    while t<>nil do begin
      t^.location:=loc;
      t:=t^.next;
    end;
  end;

PROCEDURE T_token.injectAfter(CONST newToken: P_token);
  begin
    newToken^.next:=next;
    next:=newToken;
  end;

PROCEDURE T_token.injectAfter(CONST range: T_tokenRange);
  begin
    assert(range.first^.last=range.last);
    range.last^.next:=next;
    next:=range.first;
  end;

FUNCTION T_token.getTypeCheck: T_typeCheck;
  begin
    {$ifdef debugMode}
    if not(tokType in [tt_type,tt_typeCheck]) then raise Exception.create('Call to getTypeCheck is invalid by tokenType');
    {$endif}
    result:=T_typeCheck(PtrUInt(data));
  end;

PROCEDURE T_token.setTypeCheck(CONST check: T_typeCheck);
  begin
    {$ifdef debugMode}
    if not(tokType in [tt_type,tt_typeCheck]) then raise Exception.create('Call to setTypeCheck is invalid by tokenType');
    {$endif}
    data:=pointer(PtrUInt(check));
  end;

FUNCTION T_token.getModifier: T_modifier;
  begin
    {$ifdef debugMode}
    if (tokType<>tt_modifier) then raise Exception.create('Call to getModifier is invalid by tokenType');
    {$endif}
    result:=T_modifier(PtrUInt(data));
  end;

PROCEDURE T_token.setModifier(CONST modifier: T_modifier);
  begin
    data:=pointer(PtrUInt(modifier));
    tokType:=tt_modifier;
  end;

FUNCTION T_token.serializeSingleToken(VAR literalRecycler:T_literalRecycler; CONST locationOfSerializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream: P_outputStreamWrapper): boolean;
  PROCEDURE writeType; begin stream^.writeByte(byte(tokType)); end;
  PROCEDURE writeTxt;  begin stream^.writeAnsiString(txt); end;

  begin
    case tokType of
      tt_literal:
        try
          writeType;
          writeLiteralToStream(literalRecycler,P_literal(data),stream,locationOfSerializeCall,adapters);
          result:=stream^.allOkay;
        except
          result:=false
        end;
      tt_identifier,
      tt_parameterIdentifier,
      tt_intrinsicRule,
      tt_blockLocalVariable,
      tt_eachParameter,
      tt_each,
      tt_parallelEach,
      tt_assignNewBlockLocal,
      tt_assignExistingBlockLocal,
      tt_declare,
      tt_assign,
      tt_mutate,
      tt_mut_assignPlus,
      tt_mut_assignMinus,
      tt_mut_assignMult,
      tt_mut_assignDiv,
      tt_mut_assignStrConcat,
      tt_mut_assignAppend,
      tt_mut_assignAppendAlt,
      tt_mut_assignDrop:
        begin
          writeType;
          writeTxt;
          result:=stream^.allOkay;
        end;
      tt_eachIndex,
      tt_ponFlipper,
      tt_agg,
      tt_while,
      tt_beginBlock, tt_beginRule, tt_beginExpression, tt_endBlock, tt_endRule, tt_endExpression,
      tt_save, tt_return,
      tt_pseudoFuncPointer,
      tt_pow2,tt_pow3,
      tt_braceOpen, tt_braceClose,
      tt_listBraceOpen, tt_listBraceClose,
      tt_expBraceOpen, tt_expBraceClose,
      tt_iifCheck, tt_iifElse,
      tt_separatorComma, tt_separatorCnt, tt_separatorMapItem,
      tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq,
      tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq,
      tt_operatorIn, tt_operatorNotIn,
      tt_operatorAnd, tt_operatorOr, tt_operatorXor,
      tt_operatorLazyAnd, tt_operatorLazyOr,
      tt_operatorPlus, tt_operatorMinus, tt_operatorMult,
      tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
      tt_operatorStrConcat, tt_operatorOrElse,
      tt_operatorConcat, tt_operatorConcatAlt,
      tt_unaryOpNegate,
      tt_unaryOpPlus, tt_unaryOpMinus,
      tt_listToParameterList,
      tt_semicolon,
      tt_optionalParameters,
      tt_nameOf:
        begin
          writeType;
          result:=stream^.allOkay;
        end;
      tt_modifier:
        begin
          writeType;
          stream^.writeByte(byte(getModifier));
          result:=stream^.allOkay;
        end;
      //To ignore:
      tt_EOL,
      tt_docComment,
      tt_attributeComment,
      tt_blank:
        begin
        end;
      //Not deserializable: tt_userRule
      //                    tt_customType,
      //                    tt_globalVariable,
      //                    tt_customTypeCheck
      //                    tt_typeCheck
      //                    tt_type
      //                    tt_use
      //                    tt_include
      //Only defined during evaluation: tt_aggregatorExpressionLiteral
      //                                tt_rulePutCacheValue
      //                                tt_aggregatorConstructor
      //                                tt_parList_constructor,
      //                                tt_parList,
      //                                tt_list_constructor,
      //                                tt_list_constructor_ranging
      //                                tt_mut_nested_assign,
      //                                tt_mut_nestedPlus,
      //                                tt_mut_nestedMinus,
      //                                tt_mut_nestedMult,
      //                                tt_mut_nestedDiv,
      //                                tt_mut_nestedStrConcat,
      //                                tt_mut_nestedAppend,
      //                                tt_mut_nestedAppendAlt,
      //                                tt_mut_nestedDrop,
      else begin
        if adapters<>nil
        then adapters^.raiseSimpleError('Cannot serialize token of type '+getEnumName(TypeInfo(tokType),ord(tokType)),locationOfSerializeCall)
        else raise Exception.create    ('Cannot serialize token of type '+getEnumName(TypeInfo(tokType),ord(tokType)));
        stream^.logWrongTypeError;
        result:=false;
      end;
    end;
  end;

PROCEDURE T_token.deserializeSingleToken(VAR literalRecycler:T_literalRecycler; CONST locationOfDeserializeCall:T_tokenLocation; CONST adapters:P_messages; CONST stream: P_inputStreamWrapper; VAR typeMap:T_typeMap);
  begin
    location:=locationOfDeserializeCall;
    tokType:=T_tokenType(stream^.readByte([byte(low(T_tokenType))..byte(high(T_tokenType))]));
    if stream^.allOkay then case tokType of
      tt_literal:
        data:=newLiteralFromStream(literalRecycler,stream,locationOfDeserializeCall,adapters,typeMap);
      tt_intrinsicRule:
        begin
          //TODO: This is a workaround; it would be nicer if the intrinsic rule map could be used
          tokType:=tt_identifier;
          txt:=stream^.readAnsiString;
        end;
      tt_identifier,
      tt_parameterIdentifier,
      tt_blockLocalVariable,
      tt_eachParameter,
      tt_each,
      tt_parallelEach,
      tt_assignNewBlockLocal,
      tt_assignExistingBlockLocal,
      tt_declare,
      tt_assign,
      tt_mutate,
      tt_mut_assignPlus,
      tt_mut_assignMinus,
      tt_mut_assignMult,
      tt_mut_assignDiv,
      tt_mut_assignStrConcat,
      tt_mut_assignAppend,
      tt_mut_assignAppendAlt,
      tt_mut_assignDrop:
        txt:=stream^.readAnsiString;
      tt_eachIndex,
      tt_ponFlipper,
      tt_agg,
      tt_while,
      tt_beginBlock, tt_beginRule, tt_beginExpression, tt_endBlock, tt_endRule, tt_endExpression,
      tt_save, tt_return,
      tt_pseudoFuncPointer,
      tt_pow2,tt_pow3,
      tt_braceOpen, tt_braceClose,
      tt_listBraceOpen, tt_listBraceClose,
      tt_expBraceOpen, tt_expBraceClose,
      tt_iifCheck, tt_iifElse,
      tt_separatorComma, tt_separatorCnt, tt_separatorMapItem,
      tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq,
      tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq,
      tt_operatorIn, tt_operatorNotIn,
      tt_operatorAnd, tt_operatorOr, tt_operatorXor,
      tt_operatorLazyAnd, tt_operatorLazyOr,
      tt_operatorPlus, tt_operatorMinus, tt_operatorMult,
      tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
      tt_operatorStrConcat, tt_operatorOrElse,
      tt_operatorConcat, tt_operatorConcatAlt,
      tt_unaryOpNegate,
      tt_unaryOpPlus, tt_unaryOpMinus,
      tt_listToParameterList,
      tt_semicolon,
      tt_optionalParameters,
      tt_nameOf:
          txt:=C_tokenDefaultId[tokType];
      tt_modifier:
        begin
          setModifier(T_modifier(stream^.readByte([byte(low(T_modifier))..byte(high(T_modifier))])));
          txt:=C_modifierInfo[getModifier].name;
        end;
      //To ignore:
      tt_EOL,
      tt_docComment,
      tt_attributeComment,
      tt_blank:
        begin
        end;
      //Not deserializable: tt_userRule
      //                    tt_customType,
      //                    tt_globalVariable,
      //                    tt_customTypeCheck
      //                    tt_typeCheck
      //                    tt_type
      //                    tt_use
      //                    tt_include
      //Only defined during evaluation: tt_aggregatorExpressionLiteral
      //                                tt_rulePutCacheValue
      //                                tt_aggregatorConstructor
      //                                tt_parList_constructor,
      //                                tt_parList,
      //                                tt_list_constructor,
      //                                tt_list_constructor_ranging
      //                                tt_mut_nested_assign,
      //                                tt_mut_nestedPlus,
      //                                tt_mut_nestedMinus,
      //                                tt_mut_nestedMult,
      //                                tt_mut_nestedDiv,
      //                                tt_mut_nestedStrConcat,
      //                                tt_mut_nestedAppend,
      //                                tt_mut_nestedAppendAlt,
      //                                tt_mut_nestedDrop,
      else begin
        if adapters<>nil
        then adapters^.raiseSimpleError('Cannot deserialize token of type '+getEnumName(TypeInfo(tokType),ord(tokType)),locationOfDeserializeCall)
        else raise Exception.create    ('Cannot deserialize token of type '+getEnumName(TypeInfo(tokType),ord(tokType)));
        stream^.logWrongTypeError;
      end;
    end;
  end;

end.
