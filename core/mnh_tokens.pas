UNIT mnh_tokens;
INTERFACE
USES sysutils,mnh_litVar,mnh_tokLoc,mnh_constants,mnh_out_adapters;
TYPE
  T_rawToken=record txt:string; tokType:T_tokenType; end;
  T_rawTokenArray=array of T_rawToken;

  P_token=^T_token;

  { T_token }

  T_token=object
    private
      tokType :T_tokenType;
      data    :P_tokenPayload;

      PROCEDURE setTokenType(CONST newType:T_tokenType);
      FUNCTION getComment:T_commentString;
      PROCEDURE setCommment(CONST comment:T_commentString);
      FUNCTION getId:T_idString;
      PROCEDURE setId(CONST id:T_idString);
      FUNCTION getLiteral:P_literal;
      PROCEDURE setLiteral(CONST lit:P_literal);
    public
      location:T_tokenLocation;
      next    :P_token;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; CONST tokenType:T_tokenType; CONST ptr:pointer=nil);
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

    PROPERTY tokenType:T_tokenType read tokType write setTokenType;
    PROPERTY comment:T_commentString read getComment write setCommment;
    PROPERTY id:T_idString read getId write setId;
    PROPERTY literal:P_literal read getLiteral write setLiteral;
    PROPERTY rawData:P_tokenPayload read data;
    FUNCTION getIdWithPointer:T_idWithPointerPayload;
    PROCEDURE setIdWithPointer(CONST id_:T_idString; CONST ptr:pointer);
    PROCEDURE disposeLiteral;
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

constructor T_token.create;
  begin
  end;

destructor T_token.destroy;
  begin
    undefine;
  end;

procedure T_token.define(const tokenLocation: T_tokenLocation;
  const tokenType: T_tokenType; const ptr: pointer);
  {$ifdef DEBUGMODE}VAR idLikeDummy:boolean;{$endif}
  begin
    location:=tokenLocation;
    tokType:=tokenType;
    data:=ptr;
    {$ifdef DEBUGMODE}
    if (ptr=nil) and (C_tokenInfo[tokenType].payloadType<>tpt_none) then raise Exception.create('Creating token without payload in location @'+intToStr(tokenLocation.line)+':'+intToStr(tokenLocation.column)+'; Text is: '+toString(false,idLikeDummy));
    if tokenLocation.package=nil then raise Exception.create('Creating token without package in location @'+intToStr(tokenLocation.line)+':'+intToStr(tokenLocation.column)+'; Text is: '+toString(false,idLikeDummy));
    {$endif}
  end;

procedure T_token.define(const original: T_token);
  {$ifdef DEBUGMODE}VAR idLikeDummy:boolean;{$endif}
  begin
    location:=original.location;
    tokType:=original.tokType;
    if C_tokenInfo[tokType].payloadType<>tpt_none
    then data:=original.data^.cloneOrCopy(tokType)
    else data:=nil;
    {$ifdef DEBUGMODE}
    if (data=nil) and (C_tokenInfo[tokType].payloadType<>tpt_none) then raise Exception.create('Creating token without payload in location @'+intToStr(location.line)+':'+intToStr(location.column)+'; Text is: '+toString(false,idLikeDummy));
    if location.package=nil then raise Exception.create('Creating token without package in location @'+intToStr(location.line)+':'+intToStr(location.column)+'; Text is: '+toString(false,idLikeDummy));
    {$endif}
  end;

procedure T_token.undefine;
  begin
    if data<>nil then case C_tokenInfo[tokType].payloadType of
      tpt_literal: mnh_litVar.disposeLiteral(P_literal(data));
      tpt_identifier,
      tpt_comment,
      tpt_idWithPointerPayload,
      tpt_eachPayload: dispose(data,destroy);
    end;
    data:=nil;
    tokType:=tt_EOL;
    location.package:=nil;
    location.column:=0;
    location.line:=0;
  end;

function T_token.last: P_token;
  begin
    result:=@self;
    while result^.next<>nil do result:=result^.next;
  end;

function T_token.toString(const lastWasIdLike: boolean; out idLike: boolean;
  const limit: longint): ansistring;
  begin
    idLike:=false;
    if data=nil then result:=C_tokenInfo[tokType].defaultId
                 else result:=data^.toString(tokType,limit);
    if length(result)<1 then begin
      idLike:=false; exit(result);
    end;
    if lastWasIdLike and (result[1] in ['a'..'z','A'..'Z','?',':','0'..'9'])
      or (tokType in [tt_operatorAnd,tt_operatorDivInt,tt_operatorIn,tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorMod,tt_operatorOr,tt_operatorXor,tt_operatorOrElse,tt_iifCheck,tt_iifElse])
    then result:=' '+result;
    idLike:=(result[length(result)] in ['a'..'z','A'..'Z','?',':','_']) or (tokType in [tt_separatorComma,tt_semicolon]);
  end;

function T_token.singleTokenToString: ansistring;
  VAR dummy:boolean;
  begin
    result:=toString(false,dummy);
    if tokType in [tt_operatorAnd,tt_operatorDivInt,tt_operatorIn,tt_operatorLazyAnd,tt_operatorLazyOr,tt_operatorMod,tt_operatorOr,tt_operatorXor,tt_iifCheck,tt_iifElse,tt_operatorOrElse]
    then result:=trim(result);
  end;

function T_token.areBracketsPlausible(var adaptersForComplaints: T_adapters
  ): boolean;
  VAR bracketStack:array of P_token;
  PROCEDURE push(CONST token:P_token);
    begin
      setLength(bracketStack,length(bracketStack)+1);
      bracketStack[length(bracketStack)-1]:=token;
    end;

  FUNCTION popPlausible(CONST token:P_token):boolean;
    begin
      if length(bracketStack)<=0 then begin
        adaptersForComplaints.raiseCustomMessage(mt_el4_parsingError,'Missing opening bracket for closing '+safeTokenToString(token),token^.location);
        exit(false);
      end;
      if token^.tokType<>C_matchingClosingBracket[bracketStack[length(bracketStack)-1]^.tokType] then begin
        adaptersForComplaints.raiseCustomMessage(mt_el4_parsingError,'Bracket mismatch; opening '+safeTokenToString(bracketStack[length(bracketStack)-1])+' (matches with "'+C_tokenInfo[C_matchingClosingBracket[bracketStack[length(bracketStack)-1]^.tokType]].defaultId+'")',bracketStack[length(bracketStack)-1]^.location);
        adaptersForComplaints.raiseCustomMessage(mt_el4_parsingError,'Bracket mismatch; closing with '+safeTokenToString(token) ,token^.location);
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
      if t^.tokType in C_forbiddenTokenTypes then begin
        adaptersForComplaints.raiseCustomMessage(mt_el4_parsingError,'Invalid symbol '+safeTokenToString(t),t^.location);
        result:=false;
      end;
      if      t^.tokType in C_openingBrackets then push(t)
      else if t^.tokType in C_closingBrackets then result:=result and popPlausible(t);
      t:=t^.next;
    end;
    result:=result and stackIsEmpty;
  end;

function T_token.getTokenOnBracketLevel(const types: T_tokenTypeSet;
  const onLevel: longint; const initialLevel: longint): P_token;
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

function T_token.getDeclarationOrAssignmentToken: P_token;
  VAR level:longint=0;
      t,newNext:P_token;
  begin
    t:=@self;
    while (t<>nil) do begin
      if (t^.tokType=tt_iifElse) and (t^.next<>nil) and (t^.next^.tokType=tt_customTypeRule) then begin
        newNext:=t^.next^.next;
        t^.tokType:=tt_customTypeCheck;
        t^.data   :=t^.next^.data;
        dispose(t^.next,destroy);
        t^.next:=newNext;
      end;
      if t^.tokType      in C_openingBrackets then inc(level)
      else if t^.tokType in C_closingBrackets then dec(level);
      if (level=0) and (t^.tokType in [tt_declare,tt_assign]) then exit(t);
      t:=t^.next;
    end;
    result:=nil;
  end;

function T_token.getRawToken: T_rawToken;
  begin
    result.tokType:=tokType;
    result.txt:=singleTokenToString;
  end;

function T_token.hash: T_hashInt;
  VAR pt:P_token;
  begin
    result:=0;
    pt:=@self;
    {$Q-}{$R-}
    while pt<>nil do begin
      with pt^ do begin
        case tokType of
          tt_identifier, tt_localUserRule,tt_importedUserRule,tt_intrinsicRule:
               result:=result*31+longint(tt_identifier);
          else result:=result*31+longint(tokType);
        end;
        if C_tokenInfo[tokType].payloadType<>tpt_none
        then result:=result*31+data^.hash;
      end;
      pt:=pt^.next;
    end;
    {$Q+}{$R+}
  end;

function T_token.getIdWithPointer: T_idWithPointerPayload;
  begin
    if (C_tokenInfo[tokType].payloadType in [tpt_idWithPointerPayload,tpt_builtinRule]) and (data<>nil)
    then result:=P_idWithPointerPayload(data)^
    else raise Exception.create('Invalid operation');
  end;

procedure T_token.setIdWithPointer(const id_: T_idString; const ptr: pointer);
  begin
    if (C_tokenInfo[tokType].payloadType=tpt_idWithPointerPayload) then raise Exception.create('Invalid operation');
    if data=nil then new(P_idWithPointerPayload(data),create(id_,ptr)) else begin
      P_idWithPointerPayload(data)^.id :=id_;
      P_idWithPointerPayload(data)^.ptr:=ptr;
    end;
  end;

procedure T_token.setTokenType(const newType: T_tokenType);
  begin
    if (data<>nil) and (C_tokenInfo[newType].payloadType=C_tokenInfo[tokType].payloadType) or
       (data=nil)  and (C_tokenInfo[newType].payloadType=tpt_none) then begin
      tokType:=newType;
      exit;
    end;
    if data<>nil then case C_tokenInfo[tokType].payloadType of
      tpt_literal: mnh_litVar.disposeLiteral(P_literal(data));
      tpt_identifier,
      tpt_comment,
      tpt_idWithPointerPayload,
      tpt_eachPayload: dispose(data,destroy);
    end;
    tokType:=newType;
  end;

function T_token.getComment: T_commentString;
  begin
    if (C_tokenInfo[tokType].payloadType=tpt_comment) and (data<>nil)
    then result:=P_commentPayload(data)^.txt
    else result:='';
  end;

procedure T_token.setCommment(const comment: T_commentString);
  begin
    if (C_tokenInfo[tokType].payloadType<>tpt_comment) then raise Exception.create('Invalid operation');
    if data=nil then new(P_commentPayload(data),create(comment))
                else P_commentPayload(data)^.txt:=comment;
  end;

function T_token.getId: T_idString;
  begin
    if (C_tokenInfo[tokenType].payloadType in [tpt_identifier,tpt_builtinRule,tpt_userRule,tpt_eachPayload]) and (data<>nil)
    then result:=P_idPayload(data)^.id
    else result:='';
  end;

procedure T_token.setId(const id: T_idString);
  begin
    if not(C_tokenInfo[tokType].payloadType in [tpt_identifier,tpt_eachPayload]) then raise Exception.create('Invalid operation');
    case C_tokenInfo[tokType].payloadType of
      tpt_identifier : if data=nil then new(P_idPayload  (data),create(id    )) else P_idPayload  (data)^.id:=id;
      tpt_eachPayload: if data=nil then new(P_eachPayload(data),create(id,nil)) else P_eachPayload(data)^.id:=id;
    end;
  end;

function T_token.getLiteral: P_literal;
  begin
    if C_tokenInfo[tokType].payloadType=tpt_literal
    then result:=P_literal(data)
    else result:=nil;
  end;

procedure T_token.setLiteral(const lit: P_literal);
  begin
    if (C_tokenInfo[tokType].payloadType<>tpt_literal) then raise Exception.create('Invalid operation');
    data:=lit;
  end;

procedure T_token.disposeLiteral;
  begin
    if C_tokenInfo[tokType].payloadType=tpt_literal
    then mnh_litVar.disposeLiteral(P_literal(data));
  end;


end.
