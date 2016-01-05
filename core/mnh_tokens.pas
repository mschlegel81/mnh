UNIT mnh_tokens;
INTERFACE
USES sysutils,mnh_litVar,mnh_tokLoc,mnh_constants;
TYPE
  T_rawToken=record txt:string; tokType:T_tokenType; end;
  T_rawTokenArray=array of T_rawToken;

  P_token=^T_token;
  T_token=object
    next    :P_token;
    location:T_tokenLocation;
    txt     :shortString;
    tokType :T_tokenType;
    data    :pointer;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer);
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType);
    PROCEDURE define(CONST original:T_token);
    PROCEDURE undefine;
    FUNCTION last:P_token;
    FUNCTION toString(CONST lastWasIdLike:boolean; OUT idLike:boolean):ansistring;
    FUNCTION singleTokenToString:ansistring;
    FUNCTION getDeclarationOrAssignmentToken:P_token;
    FUNCTION getRawToken:T_rawToken;
  end;

FUNCTION tokensToString(CONST first:P_token; CONST limit:longint):ansistring;
FUNCTION safeTokenToString(CONST t:P_token):ansistring;
IMPLEMENTATION
FUNCTION tokensToString(CONST first:P_token; CONST limit:longint):ansistring;
  VAR p:P_token;
      idLike,prevIdLike:boolean;
      count:longint=0;
  begin
    prevIdLike:=false;
    result:='';
    p:=first;
    while (p<>nil) and (count<limit) do begin
      result:=result+p^.toString(prevIdLike,idLike);
      prevIdLike:=idLike;
      p:=p^.next;
    end;
    if p<>nil then result:=result+' ...';
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

PROCEDURE T_token.define(CONST tokenLocation:T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer);
  begin
    location:=tokenLocation;
    if (tokenText='') or (C_tokenString[tokenType]<>'')
      then txt:=C_tokenString[tokenType]
      else txt:=tokenText;
    tokType:=tokenType;
    data:=ptr;
  end;

PROCEDURE T_token.define(CONST tokenLocation:T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType);
  begin
    location:=tokenLocation;
    if tokenText='' then txt:=C_tokenString[tokenType]
                    else txt:=tokenText;
    tokType:=tokenType;
    data:=nil;
  end;

PROCEDURE T_token.define(CONST original:T_token);
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
  end;

PROCEDURE T_token.undefine;
  begin
    case tokType of
      tt_literal,tt_aggregatorExpressionLiteral,tt_list_constructor,tt_parList_constructor,tt_parList: disposeLiteral(data);
      tt_each,tt_parallelEach: if data<>nil then disposeLiteral(data);
      else data:=nil;
    end;
    tokType:=tt_EOL;
    location:=C_nilTokenLocation;
  end;

FUNCTION T_token.last:P_token;
  begin
    result:=@self;
    while result^.next<>nil do result:=result^.next;
  end;

FUNCTION T_token.toString(CONST lastWasIdLike:boolean; OUT idLike:boolean):ansistring;
  begin
    idLike:=false;
    case tokType of
      tt_each, tt_parallelEach: begin
        result:=C_tokenString[tokType];
        if txt<>'' then result:=result+'('+txt+',';
        if data<>nil then result:=result+P_literal(data)^.toString+',';
      end;
      tt_aggregatorExpressionLiteral,
      tt_literal            : result:=P_literal(data)^.toString;
      tt_parList_constructor: result:=P_listLiteral(data)^.toParameterListString(false);
      tt_parList            : result:=P_listLiteral(data)^.toParameterListString(true);
      tt_list_constructor   : result:=P_listLiteral(data)^.listConstructorToString;
      tt_assignNewBlockLocal: result:=C_tokenString[tt_modifier_local]+' '+txt+C_tokenString[tokType];
      tt_mutate, tt_assignExistingBlockLocal, tt_cso_assignPlus..tt_cso_assignAppend: result:=txt+C_tokenString[tokType];
      tt_identifier_pon,
      tt_localUserRule_pon,
      tt_importedUserRule_pon,
      tt_intrinsicRule_pon: result:='.'+txt;
      tt_identifier,
      tt_localUserRule,
      tt_importedUserRule,
      tt_intrinsicRule,
      tt_rulePutCacheValue,
      tt_parameterIdentifier,
      tt_blockLocalVariable,
      tt_blank: result:=txt;
      else result:=C_tokenString[tokType];
    end;
    if length(result)<1 then begin
      idLike:=false; exit(result);
    end;
    if lastWasIdLike and (result[1] in ['a'..'z','A'..'Z','?',':','0'..'9'])
      or (tokType in [tt_operatorAnd,tt_operatorDivInt,tt_operatorIn,tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorMod,tt_operatorOr,tt_operatorXor,tt_iifCheck,tt_iifElse])
    then result:=' '+result;
    idLike:=(result[length(result)] in ['a'..'z','A'..'Z','?',':','_']) or (tokType in [tt_separatorComma,tt_semicolon]);
  end;

FUNCTION T_token.singleTokenToString:ansistring;
  VAR dummy:boolean;
  begin
    result:=toString(false,dummy);
    if tokType in [tt_operatorAnd,tt_operatorDivInt,tt_operatorIn,tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorMod,tt_operatorOr,tt_operatorXor,tt_iifCheck,tt_iifElse]
    then result:=trim(result);
  end;

FUNCTION T_token.getDeclarationOrAssignmentToken:P_token;
  VAR scopeLevel:longint=0;
      t:P_token;
  begin
    result:=nil;
    t:=@self;
    while t<>nil do begin
      case t^.tokType of
        tt_declare,tt_assign: if scopeLevel=0 then exit(t);
        tt_begin,tt_braceOpen ,tt_expBraceOpen ,tt_listBraceOpen:  inc(scopeLevel);
        tt_end  ,tt_braceClose,tt_expBraceClose,tt_listBraceClose: dec(scopeLevel);
      end;
      t:=t^.next;
    end;
  end;

FUNCTION T_token.getRawToken:T_rawToken;
  begin
    result.tokType:=tokType;
    result.txt:=singleTokenToString;
  end;

end.
