UNIT mnh_tokens;
INTERFACE
USES sysutils,mnh_litVar,mnh_basicTypes,mnh_constants,mnh_out_adapters,myGenerics;
TYPE
  T_rawToken=record txt:string; tokType:T_tokenType; end;
  T_rawTokenArray=array of T_rawToken;

  P_token=^T_token;
  T_token=object
    next    :P_token;
    location:T_tokenLocation;
    txt     :T_idString;
    tokType :T_tokenType;
    data    :pointer;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; CONST tokenText:T_idString; CONST tokenType:T_tokenType; CONST ptr:pointer=nil);
    PROCEDURE define(CONST original:T_token);
    PROCEDURE undefine;
    FUNCTION last:P_token;
    FUNCTION toString(CONST lastWasIdLike:boolean; OUT idLike:boolean; CONST limit:longint=maxLongint):ansistring;
    FUNCTION hash:T_hashInt;
    FUNCTION equals(CONST other:T_token):boolean;
    FUNCTION singleTokenToString:ansistring;
    FUNCTION areBracketsPlausible(VAR adaptersForComplaints:T_adapters):boolean;
    FUNCTION getTokenOnBracketLevel(CONST types:T_tokenTypeSet; CONST onLevel:longint; CONST initialLevel:longint=0):P_token;
    FUNCTION getDeclarationOrAssignmentToken:P_token;
    FUNCTION getRawToken:T_rawToken;
    PROCEDURE resolveRuleId(CONST packageOrNil:pointer; CONST adaptersOrNil:P_adapters);

  end;

  T_bodyParts=array of record first,last:P_token; end;

  T_resolveIDsCallback=PROCEDURE(VAR token:T_token; CONST package:pointer; CONST adaptersOrNil:P_adapters);

FUNCTION tokensToString(CONST first:P_token; CONST limit:longint=maxLongint):ansistring;
FUNCTION safeTokenToString(CONST t:P_token):ansistring;
VAR resolveIDsCallback:T_resolveIDsCallback;
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
    if tokenLocation.package=nil then raise Exception.create('Creating token without package in location @'+intToStr(tokenLocation.line)+':'+intToStr(tokenLocation.column)+'; Text is: '+toString(false,idLikeDummy));
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
    if location.package=nil then raise Exception.create('Creating token without package in location @'+intToStr(location.line)+':'+intToStr(location.column)+'; Text is: '+toString(false,idLikeDummy));
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
      tt_assignNewBlockLocal: result:=C_tokenInfo[tt_modifier_local].defaultId+' '+txt+C_tokenInfo[tokType].defaultId;
      tt_beginRule,tt_beginExpression:result:=C_tokenInfo[tt_beginBlock].defaultId+'* ';
      tt_endRule  ,tt_endExpression  :result:=C_tokenInfo[tt_endBlock  ].defaultId+'* ';
      tt_mutate, tt_assignExistingBlockLocal, tt_cso_assignPlus..tt_cso_assignAppend: result:=txt+C_tokenInfo[tokType].defaultId;
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
    if tokType in [tt_intrinsicRule,tt_localUserRule,tt_importedUserRule,tt_rulePutCacheValue,tt_customTypeRule] then result:=result*31+T_hashInt(data);
    if (tokType in [tt_each,tt_parallelEach,tt_customTypeCheck,tt_assignNewBlockLocal,
                    tt_mutate,tt_assignExistingBlockLocal, tt_cso_assignPlus..tt_cso_assignAppend,
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
    if tokType in [tt_intrinsicRule,tt_localUserRule,tt_importedUserRule,tt_rulePutCacheValue,tt_customTypeRule] then exit(data=other.data);
    if (tokType in [tt_each,tt_parallelEach,tt_customTypeCheck,tt_assignNewBlockLocal,
                    tt_mutate,tt_assignExistingBlockLocal, tt_cso_assignPlus..tt_cso_assignAppend,
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

FUNCTION T_token.getDeclarationOrAssignmentToken: P_token;
  VAR level:longint=0;
      t,newNext:P_token;

  begin
    t:=@self;
    while (t<>nil) do begin
      if (t^.tokType=tt_iifElse) and (t^.next<>nil) and (t^.next^.tokType=tt_customTypeRule) then begin
        newNext:=t^.next^.next;
        t^.tokType:=tt_customTypeCheck;
        t^.txt    :=t^.next^.txt;
        t^.data   :=t^.next^.data;
        dispose(t^.next,destroy);
        t^.next:=newNext;
      end;
      if t^.tokType      in C_openingBrackets then inc(level)
      else if t^.tokType in C_closingBrackets then dec(level);
      if (level=0) and (t^.tokType=tt_assign) or (t^.tokType=tt_declare) then exit(t);
      t:=t^.next;
    end;
    result:=nil;
  end;

FUNCTION T_token.getRawToken: T_rawToken;
  begin
    result.tokType:=tokType;
    result.txt:=singleTokenToString;
  end;

PROCEDURE T_token.resolveRuleId(CONST packageOrNil:pointer; CONST adaptersOrNil:P_adapters);
  VAR package:pointer;
  begin
    if packageOrNil<>nil then package:=packageOrNil
                         else package:=location.package;
    resolveIDsCallback(self,package,adaptersOrNil);
  end;

end.
