UNIT mnh_tokens;
INTERFACE
USES sysutils,mnh_litVar,mnh_tokLoc,mnh_constants,mnh_out_adapters;
TYPE
  T_rawToken=record txt:string; tokType:T_tokenType; end;
  T_rawTokenArray=array of T_rawToken;

  P_token=^T_token;
  T_token=object
    next    :P_token;
    location:T_tokenLocation;
    txt     :idString;
    tokType :T_tokenType;
    data    :pointer;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer=nil);
    PROCEDURE define(CONST original:T_token);
    PROCEDURE undefine;
    FUNCTION last:P_token;
    FUNCTION toString(CONST lastWasIdLike:boolean; OUT idLike:boolean; CONST limit:longint=maxLongint):ansistring;
    FUNCTION singleTokenToString:ansistring;
    FUNCTION areBracketsPlausible(VAR adaptersForComplaints:T_adapters):boolean;
    FUNCTION getTokenOnBracketLevel(CONST types:T_tokenTypeSet; CONST onLevel:longint; CONST initialLevel:longint=0):P_token;
    FUNCTION getDeclarationOrAssignmentToken:P_token;
    FUNCTION getRawToken:T_rawToken;
    FUNCTION hash:T_hashInt;
    {$ifdef DEBUGMODE}
    PROCEDURE validate(CONST recurse:boolean=false);
    {$endif}
  end;

  T_bodyParts=array of record first,last:P_token; end;

FUNCTION tokensToString(CONST first:P_token; CONST limit:longint=maxLongint):ansistring;
FUNCTION safeTokenToString(CONST t:P_token):ansistring;
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

PROCEDURE T_token.define(CONST tokenLocation: T_tokenLocation; CONST tokenText: ansistring; CONST tokenType: T_tokenType; CONST ptr: pointer);
  {$ifdef DEBUGMODE}VAR idLikeDummy:boolean;{$endif}
  begin
    location:=tokenLocation;
    if (tokenText='') and (C_tokenInfo[tokenType].defaultId<>'')
      then txt:=C_tokenInfo[tokenType].defaultId
      else txt:=tokenText;
    tokType:=tokenType;
    data:=ptr;
    {$ifdef DEBUGMODE}
    if (ptr=nil) and (tokenType=tt_literal) then raise Exception.create('Creating literal token without data in location @'+intToStr(tokenLocation.line)+':'+intToStr(tokenLocation.column)+'; Text is: '+toString(false,idLikeDummy));
    if tokenLocation.package=nil then raise Exception.create('Creating token without package in location @'+intToStr(tokenLocation.line)+':'+intToStr(tokenLocation.column)+'; Text is: '+toString(false,idLikeDummy));
    {$endif}
  end;

PROCEDURE T_token.define(CONST original: T_token);
  {$ifdef DEBUGMODE}VAR idLikeDummy:boolean;{$endif}
  begin
    location:=original.location;
    txt     :=original.txt;
    tokType :=original.tokType;
    data    :=original.data;
    case tokType of
      tt_literal,tt_aggregatorExpressionLiteral,tt_parList: P_literal(data)^.rereference;
      tt_each,tt_parallelEach,tt_forcedParallelEach: if data<>nil then P_literal(data)^.rereference;
      tt_list_constructor,tt_parList_constructor: if data=nil then data:=newListLiteral else data:=P_listLiteral(original.data)^.clone;
    end;
    {$ifdef DEBUGMODE}
    if (data=nil) and (tokType=tt_literal) then raise Exception.create('Creating literal token without data in location @'+intToStr(location.line)+':'+intToStr(location.column)+'; Text is: '+toString(false,idLikeDummy));
    if location.package=nil then raise Exception.create('Creating token without package in location @'+intToStr(location.line)+':'+intToStr(location.column)+'; Text is: '+toString(false,idLikeDummy));
    {$endif}
  end;

PROCEDURE T_token.undefine;
  begin
    case tokType of
      tt_literal,tt_aggregatorExpressionLiteral,tt_list_constructor,tt_parList_constructor,tt_parList: disposeLiteral(data);
      tt_each,tt_parallelEach,tt_forcedParallelEach: if data<>nil then disposeLiteral(data);
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
      tt_each, tt_parallelEach, tt_forcedParallelEach: begin
        result:=C_tokenInfo[tokType].defaultId;
        if txt<>'' then result:=result+'('+txt+','
                   else result:=C_tokenInfo[tt_agg].defaultId+'(';
        if data<>nil then result:=result+P_literal(data)^.toString(limit-6)+',';
      end;
      tt_aggregatorExpressionLiteral,
      tt_literal            : result:=P_literal    (data)^.toString(limit);
      tt_parList_constructor: result:=P_listLiteral(data)^.toParameterListString(false,limit);
      tt_parList            : result:=P_listLiteral(data)^.toParameterListString(true ,limit);
      tt_list_constructor   : result:=P_listLiteral(data)^.listConstructorToString(limit);
      tt_assignNewBlockLocal: result:=C_tokenInfo[tt_modifier_local].defaultId+' '+txt+C_tokenInfo[tokType].defaultId;
      tt_beginFunc:result:=C_tokenInfo[tt_beginBlock].defaultId+'* ';
      tt_endFunc  :result:=C_tokenInfo[tt_endBlock  ].defaultId+'* ';
      tt_mutate, tt_assignExistingBlockLocal, tt_cso_assignPlus..tt_cso_assignAppend: result:=txt+C_tokenInfo[tokType].defaultId;
      tt_identifier,
      tt_localUserRule,
      tt_importedUserRule,
      tt_intrinsicRule,
      tt_rulePutCacheValue,
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
        adaptersForComplaints.raiseCustomMessage(mt_el4_parsingError,'Missing opening bracket.',token^.location);
        exit(false);
      end;
      if token^.tokType<>C_matchingClosingBracket[bracketStack[length(bracketStack)-1]^.tokType] then begin
        adaptersForComplaints.raiseCustomMessage(mt_el4_parsingError,'Bracket mismatch; open matches with "'+C_tokenInfo[C_matchingClosingBracket[bracketStack[length(bracketStack)-1]^.tokType]].defaultId+'")',bracketStack[length(bracketStack)-1]^.location);
        adaptersForComplaints.raiseCustomMessage(mt_el4_parsingError,'Bracket mismatch; close',token                               ^.location);
        exit(false);
      end;
      setLength(bracketStack,length(bracketStack)-1);
      result:=true;
    end;

  FUNCTION stackIsEmpty:boolean;
    begin
      if length(bracketStack)<=0 then exit(true);
      adaptersForComplaints.raiseCustomMessage(mt_el4_parsingError,'Missing closing bracket.',bracketStack[length(bracketStack)-1]^.location);
      result:=false;
    end;

  VAR t:P_token;
  begin
    setLength(bracketStack,0);
    t:=@self;
    result:=true;
    while result and (t<>nil) do begin
      if t^.tokType in C_openingBrackets then push(t)
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
  begin
    result:=getTokenOnBracketLevel([tt_declare,tt_assign],0);
  end;

FUNCTION T_token.getRawToken: T_rawToken;
  begin
    result.tokType:=tokType;
    result.txt:=singleTokenToString;
  end;

FUNCTION T_token.hash:T_hashInt;
  VAR i:longint;
      pt:P_token;
  begin
    result:=0;
    pt:=@self;
    {$Q-}{$R-}
    while pt<>nil do begin
      with pt^ do begin
        case tokType of
          tt_identifier,    tt_localUserRule    ,tt_importedUserRule    ,tt_intrinsicRule    : result:=result*31+longint(tt_identifier);
          else result:=result*31+longint(tokType);
        end;
        result:=result*31+length(txt);
        for i:=1 to length(txt) do result:=result*31+ord(txt[i]);
        case tokType of
          tt_literal,tt_aggregatorExpressionLiteral,tt_list_constructor,tt_parList_constructor,tt_parList: result:=result*31+P_literal(data)^.hash;
          tt_each,tt_parallelEach,tt_forcedParallelEach: if data<>nil then result:=result*31+P_literal(data)^.hash else result:=result*31;
        end;
      end;
      pt:=pt^.next;
    end;
    {$Q+}{$R+}
    {$ifdef DEBUGMODE}
    validate;
    {$endif}
  end;

{$ifdef DEBUGMODE}
PROCEDURE T_token.validate(CONST recurse:boolean=false);
  VAR p:P_token;
  begin
    if (data=nil) and (tokType=tt_literal) then raise Exception.create('Encountered literal token without data in location @'+intToStr(location.line)+':'+intToStr(location.column));
    p:=next;
    while recurse and (p<>nil) do begin
      p^.validate;
      p:=p^.next;
    end;
  end;
{$endif}

end.
