UNIT mnh_tokens;
INTERFACE
USES //basic classes
     sysutils,
     //my utilities
     myGenerics, myStringUtil,
     //MNH:
     mnh_basicTypes,mnh_constants,
     mnh_out_adapters,
     mnh_litVar;
TYPE
  {$ifdef fullVersion}
  T_rawToken=record txt:string; tokType:T_tokenType; end;
  T_rawTokenArray=array of T_rawToken;
  {$endif}

  P_token=^T_token;
  PP_token=^P_token;
  P_abstractRule=^T_abstractRule;
  P_tokenRecycler=^T_tokenRecycler;
  T_abstractRule=object(T_objectWithIdAndLocation)
    private
      {$ifdef fullVersion}
      idResolved:boolean;
      {$endif}
      id:T_idString;
      declarationStart:T_tokenLocation;
      ruleType:T_ruleType;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType);
      DESTRUCTOR destroy; virtual;
      FUNCTION getId:T_idString; virtual;
      FUNCTION getLocation:T_tokenLocation; virtual;
      PROPERTY getRuleType:T_ruleType read ruleType;

      FUNCTION hasPublicSubrule:boolean; virtual; abstract;

      FUNCTION getCmdLineHelpText:T_arrayOfString; virtual;
      {$ifdef fullVersion}
      PROCEDURE setIdResolved;
      PROPERTY isIdResolved:boolean read idResolved;
      FUNCTION complainAboutUnused(VAR adapters:T_adapters):boolean;
      FUNCTION getDocTxt:string; virtual; abstract;
      {$endif}
      PROCEDURE clearCache; virtual;
      PROCEDURE resolveIds(CONST adapters:P_adapters); virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual; abstract;
      FUNCTION replaces(CONST ruleTokenType:T_tokenType; CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token;CONST threadContextPointer:pointer):boolean; virtual; abstract;
      FUNCTION evaluateToBoolean(CONST ruleTokenType:T_tokenType; CONST callLocation:T_tokenLocation; CONST singleParameter:P_literal; CONST threadContextPointer:pointer; CONST recycler:P_tokenRecycler):boolean;
      FUNCTION getTypedef:P_typedef; virtual;
  end;

  T_token=object
    next    :P_token;
    location:T_tokenLocation;
    txt     :T_idString;
    tokType :T_tokenType;
    data    :pointer;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; CONST tokenText:T_idString; CONST tokenType:T_tokenType; CONST ptr:pointer=nil);
    PROCEDURE define(CONST original:T_token); inline;
    PROCEDURE undefine; inline;
    FUNCTION last:P_token;
    FUNCTION toString(CONST lastWasIdLike:boolean; OUT idLike:boolean; CONST limit:longint=maxLongint):ansistring;
    FUNCTION hash:T_hashInt;
    FUNCTION equals(CONST other:T_token):boolean;
    FUNCTION singleTokenToString:ansistring;
    FUNCTION areBracketsPlausible(VAR adaptersForComplaints:T_adapters):boolean;
    FUNCTION getTokenOnBracketLevel(CONST types:T_tokenTypeSet; CONST onLevel:longint; CONST initialLevel:longint=0):P_token;
    {$ifdef fullVersion}
    FUNCTION getRawToken:T_rawToken;
    {$endif}
    PROCEDURE setSingleLocationForExpression(CONST loc:T_tokenLocation);
    PROCEDURE injectAfter(CONST newToken:P_token);

    FUNCTION getTypeCheck:T_typeCheck;
    PROCEDURE setTypeCheck(CONST check:T_typeCheck);
    FUNCTION getModifier:T_modifier;
    PROCEDURE setModifier(CONST modifier:T_modifier);
  end;

  T_tokenRecycler=object
    private
      dat:array[0..2047] of P_token;
      fill:longint;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;

      FUNCTION disposeToken(p:P_token):P_token; inline;
      PROCEDURE cascadeDisposeToken(VAR p:P_token);
      FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer=nil):P_token; inline;
      FUNCTION newToken(CONST original:T_token):P_token; inline;
      FUNCTION newToken(CONST original:P_token):P_token; inline;
  end;

  T_bodyParts=array of record first,last:P_token; end;

FUNCTION tokensToString(CONST first:P_token; CONST limit:longint=maxLongint):ansistring;
FUNCTION safeTokenToString(CONST t:P_token):ansistring;
FUNCTION getBodyParts(CONST first:P_token; CONST initialBracketLevel:longint; VAR recycler:T_tokenRecycler; CONST adapters:P_adapters; OUT closingBracket:P_token):T_bodyParts;
IMPLEMENTATION
FUNCTION tokensToString(CONST first:P_token; CONST limit:longint):ansistring;
  VAR p:P_token;
      idLike,prevIdLike:boolean;
      remainingLength:longint;
  begin
    prevIdLike:=false;
    result:='';
    remainingLength:=limit;
    p:=first;
    while (p<>nil) and (remainingLength>0) do begin
      result:=result+p^.toString(prevIdLike,idLike,remainingLength);
      remainingLength:=limit-length(result);
      prevIdLike:=idLike;
      p:=p^.next;
    end;
    if (p<>nil) or (remainingLength<=0) then result:=result+' ...';
  end;

FUNCTION safeTokenToString(CONST t:P_token):ansistring;
  begin
    if t=nil then result:='<EOL>'
    else result:=t^.singleTokenToString;
  end;

FUNCTION getBodyParts(CONST first:P_token; CONST initialBracketLevel:longint; VAR recycler:T_tokenRecycler; CONST adapters:P_adapters; OUT closingBracket:P_token):T_bodyParts;
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
      adapters^.raiseError('Invalid special construct; Cannot find closing bracket.',first^.location);
      setLength(result,0);
      exit;
    end;
    while (t<>nil) and not((t^.tokType=tt_braceClose) and (bracketLevel=1)) do begin
      if t^.tokType in C_openingBrackets then inc(bracketLevel)
      else if t^.tokType in C_closingBrackets then dec(bracketLevel)
      else if (t^.tokType=tt_separatorComma) and (bracketLevel=1) then begin
        if partLength=0 then adapters^.raiseError('Empty body part.',result[length(result)-1].first^.location);
        result[length(result)-1].last:=p; //end of body part is token before comma
        setLength(result,length(result)+1);
        result[length(result)-1].first:=t^.next; //start of next body part is token after comma
        partLength:=-1; //excluding delimiting separators
      end;
      p:=t; t:=t^.next; inc(partLength);
    end;
    result[length(result)-1].last:=p; //end of body part is token before comma
    if (t=nil) or (t^.tokType<>tt_braceClose) or (bracketLevel<>1) then begin
      adapters^.raiseError('Invalid special construct; Cannot find closing bracket.',first^.location);
      setLength(result,0);
      exit;
    end;
    closingBracket:=t;
    for i:=0 to length(result)-1 do begin
      if result[i].last^.next<>closingBracket then recycler.disposeToken(result[i].last^.next);
      result[i].last^.next:=nil;
    end;
  end;

CONSTRUCTOR T_abstractRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    {$ifdef fullVersion}
    idResolved:=false;
    {$endif}
    id              :=ruleId;
    declarationStart:=startAt;
    ruleType        :=ruleTyp;
  end;

DESTRUCTOR T_abstractRule.destroy;                    begin id:='';                   end;
FUNCTION T_abstractRule.getId: T_idString;            begin result:=id;               end;
FUNCTION T_abstractRule.getLocation: T_tokenLocation; begin result:=declarationStart; end;

FUNCTION T_abstractRule.getCmdLineHelpText: T_arrayOfString;
  begin
    result:=(C_ruleTypeText[getRuleType]+'rule '+getId+C_lineBreakChar+'in '+getLocation.package^.getPath);
  end;

{$ifdef fullVersion}
PROCEDURE T_abstractRule.setIdResolved;
  begin
    idResolved:=true;
  end;

FUNCTION T_abstractRule.complainAboutUnused(VAR adapters: T_adapters): boolean;
  begin
    result:=(id<>MAIN_RULE_ID) and not(idResolved);
    if result then adapters.raiseWarning('Unused rule '+id+
    '; you can suppress this warning with '+
    ATTRIBUTE_PREFIX+SUPPRESS_UNUSED_WARNING_ATTRIBUTE,
    lineLocation(declarationStart));
  end;
{$endif}

PROCEDURE T_abstractRule.clearCache; begin end;
PROCEDURE T_abstractRule.resolveIds(CONST adapters: P_adapters); begin end;
FUNCTION T_abstractRule.evaluateToBoolean(CONST ruleTokenType:T_tokenType; CONST callLocation:T_tokenLocation; CONST singleParameter:P_literal; CONST threadContextPointer:pointer; CONST recycler:P_tokenRecycler):boolean;
  VAR parList:P_listLiteral;
      firstRep,lastRep:P_token;
  begin
    new(parList,create(1));
    parList^.append(singleParameter,true);
    if replaces(ruleTokenType,callLocation,parList,firstRep,lastRep,threadContextPointer)
    then begin
      result:=(firstRep<>     nil          ) and
              (firstRep^.next=nil          ) and
              (firstRep^.tokType=tt_literal) and
              (firstRep^.data=@boolLit[true]);
      recycler^.cascadeDisposeToken(firstRep);
    end else result:=false;
    disposeLiteral(parList);
  end;

FUNCTION T_abstractRule.getTypedef:P_typedef;
  begin
    raise Exception.create('getTypeDef is not implemented for this rule type');
    result:=nil;
  end;

CONSTRUCTOR T_token.create;
  begin
  end;

DESTRUCTOR T_token.destroy;
  begin
    undefine;
  end;

PROCEDURE T_token.define(CONST tokenLocation: T_tokenLocation; CONST tokenText: T_idString; CONST tokenType: T_tokenType; CONST ptr: pointer);
  {$ifdef debugMode}VAR idLikeDummy:boolean;{$endif}
  begin
    location:=tokenLocation;
    if (tokenText='') and (C_tokenInfo[tokenType].defaultId<>'')
      then txt:=C_tokenInfo[tokenType].defaultId
      else txt:=tokenText;
    tokType:=tokenType;
    data:=ptr;
    {$ifdef debugMode}
    if (ptr=nil) and (tokenType=tt_literal) then raise Exception.create('Creating literal token without data in location @'+intToStr(tokenLocation.line)+':'+intToStr(tokenLocation.column)+'; Text is: '+toString(false,idLikeDummy));
    if (tokenLocation.package=nil) then raise Exception.create('Creating token without package in location @'+intToStr(tokenLocation.line)+':'+intToStr(tokenLocation.column)+'; Text is: '+toString(false,idLikeDummy));
    {$endif}
  end;

PROCEDURE T_token.define(CONST original: T_token);
  {$ifdef debugMode}VAR idLikeDummy:boolean;{$endif}
  begin
    location:=original.location;
    txt     :=original.txt;
    tokType :=original.tokType;
    data    :=original.data;
    case tokType of
      tt_literal,tt_aggregatorExpressionLiteral,tt_parList: P_literal(data)^.rereference;
      tt_each,tt_parallelEach: if data<>nil then P_literal(data)^.rereference;
      tt_list_constructor,tt_parList_constructor: if data=nil then data:=newListLiteral else data:=P_listLiteral(original.data)^.clone;
    end;
    {$ifdef debugMode}
    if (data=nil) and (tokType=tt_literal) then raise Exception.create('Creating literal token without data in location @'+intToStr(location.line)+':'+intToStr(location.column)+'; Text is: '+toString(false,idLikeDummy));
    if (location.package=nil)then raise Exception.create('Creating token without package in location @'+intToStr(location.line)+':'+intToStr(location.column)+'; Text is: '+toString(false,idLikeDummy));
    {$endif}
  end;

PROCEDURE T_token.undefine;
  begin
    case tokType of
      tt_literal,tt_aggregatorExpressionLiteral,tt_list_constructor,tt_parList_constructor,tt_parList: disposeLiteral(data);
      tt_each,tt_parallelEach: if data<>nil then disposeLiteral(data);
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

FUNCTION T_token.toString(CONST lastWasIdLike: boolean; OUT idLike: boolean; CONST limit:longint=maxLongint): ansistring;
  begin
    idLike:=false;
    case tokType of
      tt_each, tt_parallelEach: begin
        result:=C_tokenInfo[tokType].defaultId;
        if txt<>'' then result:=result+'('+txt+','
                   else result:=C_tokenInfo[tt_agg].defaultId+'(';
        if data<>nil then result:=result+P_literal(data)^.toString(limit-6)+',';
      end;
      tt_customTypeCheck: result:=':'+txt;
      tt_aggregatorExpressionLiteral,
      tt_literal            : result:=P_literal    (data)^.toString(limit);
      tt_parList_constructor: result:=toParameterListString(P_listLiteral(data),false,limit);
      tt_parList            : result:=toParameterListString(P_listLiteral(data),true ,limit);
      tt_list_constructor   : result:=P_listLiteral(data)^.listConstructorToString(limit);
      tt_assignNewBlockLocal: result:=C_modifierInfo[modifier_local].name+' '+txt+C_tokenInfo[tokType].defaultId;
      tt_beginRule,tt_beginExpression:result:=C_tokenInfo[tt_beginBlock].defaultId+'* ';
      tt_endRule  ,tt_endExpression  :result:=C_tokenInfo[tt_endBlock  ].defaultId+'* ';
      tt_mutate, tt_assignExistingBlockLocal..tt_mut_nestedDrop: result:=txt+C_tokenInfo[tokType].defaultId;
      tt_type:      result:=    C_typeCheckInfo[getTypeCheck].name;
      tt_typeCheck: result:=':'+C_typeCheckInfo[getTypeCheck].name;
      tt_modifier : result:=C_modifierInfo[getModifier].name;
      tt_identifier,
      tt_localUserRule,
      tt_importedUserRule,
      tt_intrinsicRule,
      tt_rulePutCacheValue,
      tt_customTypeRule,
      tt_parameterIdentifier,
      tt_blockLocalVariable,
      tt_blank: result:=txt;
      else result:=C_tokenInfo[tokType].defaultId;
    end;
    if length(result)<1 then begin
      idLike:=false; exit(result);
    end;
    if lastWasIdLike and (result[1] in ['a'..'z','A'..'Z','?',':','0'..'9'])
      or (tokType in [tt_operatorAnd,tt_operatorDivInt,tt_operatorIn,tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorMod,tt_operatorOr,tt_operatorXor,tt_operatorOrElse,tt_iifCheck,tt_iifElse])
    then result:=' '+result;
    idLike:=(result[length(result)] in ['a'..'z','A'..'Z','?',':','_']) or (tokType in [tt_separatorComma,tt_semicolon]);
  end;

FUNCTION T_token.hash:T_hashInt;
  begin
    result:=T_hashInt(tokType);
    if tokType in [tt_intrinsicRule,tt_localUserRule,tt_importedUserRule,tt_rulePutCacheValue,tt_customTypeRule,tt_typeCheck] then result:=result*31+T_hashInt(data);
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

FUNCTION T_token.equals(CONST other:T_token):boolean;
  FUNCTION literalEquals(CONST x,y:P_literal):boolean; inline;
    begin
      result:=(x=nil) and (y=nil) or
              (x<>nil) and (y<>nil) and (x^.equals(y));
    end;

  begin
    if tokType<>other.tokType then exit(false);
    if tokType in [tt_intrinsicRule,tt_localUserRule,tt_importedUserRule,tt_rulePutCacheValue,tt_customTypeRule,tt_typeCheck] then exit(data=other.data);
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
    if tokType in [tt_operatorAnd,tt_operatorDivInt,tt_operatorIn,tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorMod,tt_operatorOr,tt_operatorXor,tt_iifCheck,tt_iifElse,tt_operatorOrElse]
    then result:=trim(result);
  end;

FUNCTION T_token.areBracketsPlausible(VAR adaptersForComplaints: T_adapters): boolean;
  VAR bracketStack:array of P_token;
  PROCEDURE push(CONST token:P_token);
    begin
      setLength(bracketStack,length(bracketStack)+1);
      bracketStack[length(bracketStack)-1]:=token;
    end;

  FUNCTION popPlausible(CONST token:P_token):boolean;
    begin
      if length(bracketStack)<=0 then begin
        adaptersForComplaints.raiseError('Missing opening bracket for closing '+safeTokenToString(token),token^.location);
        exit(false);
      end;
      if token^.tokType<>C_matchingClosingBracket[bracketStack[length(bracketStack)-1]^.tokType] then begin
        adaptersForComplaints.raiseError('Bracket mismatch; opening '+safeTokenToString(bracketStack[length(bracketStack)-1])+' (matches with "'+C_tokenInfo[C_matchingClosingBracket[bracketStack[length(bracketStack)-1]^.tokType]].defaultId+'")',bracketStack[length(bracketStack)-1]^.location);
        adaptersForComplaints.raiseError('Bracket mismatch; closing with '+safeTokenToString(token) ,token^.location);
        exit(false);
      end;
      setLength(bracketStack,length(bracketStack)-1);
      result:=true;
    end;

  FUNCTION stackIsEmpty:boolean;
    begin
      if length(bracketStack)<=0 then exit(true);
      adaptersForComplaints.raiseError('Missing closing bracket.',bracketStack[length(bracketStack)-1]^.location);
      result:=false;
    end;

  VAR t:P_token;
  begin
    setLength(bracketStack,0);
    t:=@self;
    result:=true;
    while result and (t<>nil) do begin
      if t^.tokType in C_forbiddenTokenTypes then begin
        adaptersForComplaints.raiseError('Invalid symbol '+safeTokenToString(t),t^.location);
        result:=false;
      end;
      if      t^.tokType in C_openingBrackets then push(t)
      else if t^.tokType in C_closingBrackets then result:=result and popPlausible(t);
      t:=t^.next;
    end;
    result:=result and stackIsEmpty;
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

PROCEDURE T_token.setSingleLocationForExpression(CONST loc:T_tokenLocation);
  VAR t:P_token;
  begin
    t:=@self;
    while t<>nil do begin
      t^.location:=loc;
      t:=t^.next;
    end;
  end;

PROCEDURE T_token.injectAfter(CONST newToken:P_token);
  begin
    newToken^.next:=next;
    next:=newToken;
  end;

FUNCTION T_token.getTypeCheck: T_typeCheck;
  begin
    {$ifdef debugMode}
    if not(tokType in [tt_type,tt_typeCheck]) then raise Exception.create('Call to getTypeCheck is invalid by tokenType');
    {$endif}
    result:=T_typeCheck(PtrUInt(data));
  end;

PROCEDURE T_token.setTypeCheck(CONST check:T_typeCheck);
  begin
    {$ifdef debugMode}
    if not(tokType in [tt_type,tt_typeCheck]) then raise Exception.create('Call to setTypeCheck is invalid by tokenType');
    {$endif}
    data:=pointer(PtrUInt(check));
  end;

FUNCTION T_token.getModifier:T_modifier;
  begin
    {$ifdef debugMode}
    if (tokType<>tt_modifier) then raise Exception.create('Call to getModifier is invalid by tokenType');
    {$endif}
    result:=T_modifier(PtrUInt(data));
  end;

PROCEDURE T_token.setModifier(CONST modifier:T_modifier);
  begin
    data:=pointer(PtrUInt(modifier));
    tokType:=tt_modifier;
  end;

CONSTRUCTOR T_tokenRecycler.create;
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
  end;

DESTRUCTOR T_tokenRecycler.destroy;
  begin
    while fill>0 do begin
      dec(fill);
      try
        dispose(dat[fill],destroy);
      except
        dat[fill]:=nil;
      end;
    end;
  end;

FUNCTION T_tokenRecycler.disposeToken(p: P_token): P_token;
  begin
    if p=nil then result:=nil
    else begin
      result:=p^.next;
      if (fill>=length(dat))
      then dispose(p,destroy)
      else begin
        p^.undefine;
        dat[fill]:=p;
        inc(fill);
      end;
    end;
  end;

PROCEDURE T_tokenRecycler.cascadeDisposeToken(VAR p: P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_tokenRecycler.newToken(CONST tokenLocation: T_tokenLocation; CONST tokenText: ansistring; CONST tokenType: T_tokenType; CONST ptr: pointer): P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST original: T_token): P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST original: P_token): P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original^);
    result^.next:=nil;
  end;

end.
