{$ifdef include_interface}
  P_pattern=^T_pattern;
  T_pattern=object
    sig:array of record
          id               :ansistring;
          restrictionType  :T_tokenType;
          restrictionScalar:P_scalarLiteral;
          restrictionIdx   :longint;
          restrictionId    :ansistring;
        end;
    containsSelfPointer,
    hasOptionals:boolean;

    CONSTRUCTOR create;
    CONSTRUCTOR clone(original:T_pattern);
    PROCEDURE clear;
    DESTRUCTOR destroy;
    FUNCTION appendFreeId    (CONST parId:ansistring):longint;
    FUNCTION appendTypeCheck (CONST parId:ansistring; CONST typeRest:T_tokenType):longint;
    FUNCTION appendComparison(CONST parId:ansistring; CONST comparator:T_tokenType; CONST value:P_scalarLiteral):longint;
    FUNCTION appendComparison(CONST parId:ansistring; CONST comparator:T_tokenType; CONST otherId:ansistring):longint;
    PROCEDURE appendOptional;
    FUNCTION indexOfId(CONST id:ansistring; CONST location:T_tokenLocation; VAR adapters:T_adapters):longint;
    FUNCTION indexOfIdForInline(CONST id:ansistring):longint;
    PROCEDURE finalizeRefs;
    FUNCTION matches(VAR par:T_listLiteral; CONST selfPointerProvided:boolean; VAR adapters:T_adapters):boolean;
    FUNCTION matchesNilPattern:boolean;
    FUNCTION toString:ansistring;
    FUNCTION isEquivalent(CONST p:T_pattern):boolean;
    PROCEDURE toParameterIds(CONST tok:P_token; VAR adapters:T_adapters);
  end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_pattern.create;
  begin clear; end;

CONSTRUCTOR T_pattern.clone(original:T_pattern);
  VAR i:longint;
  begin
    containsSelfPointer:=original.containsSelfPointer;
    hasOptionals:=original.hasOptionals;
    setLength(sig,length(original.sig));
    for i:=0 to length(sig)-1 do begin
      sig[i]:=original.sig[i];
      if sig[i].restrictionScalar<>nil then sig[i].restrictionScalar^.rereference;
    end;
  end;

PROCEDURE T_pattern.clear;
  begin
    setLength(sig,0);
    containsSelfPointer:=false;
    hasOptionals:=false;
  end;

DESTRUCTOR T_pattern.destroy;
  begin clear; end;

FUNCTION T_pattern.appendFreeId    (CONST parId:ansistring):longint;
  begin
    result:=length(sig);
    setLength(sig,result+1);
    with sig[result] do begin
      id             :=parId;
      restrictionType:=tt_literal;
      restrictionIdx :=-1;
      restrictionId  :='';
      restrictionScalar:=nil;
    end;
  end;

FUNCTION T_pattern.appendTypeCheck (CONST parId:ansistring; CONST typeRest:T_tokenType):longint;
  begin
    result:=length(sig);
    setLength(sig,result+1);
    with sig[result] do begin
      id             :=parId;
      restrictionType:=typeRest;
      restrictionIdx :=-1;
      restrictionId  :='';
      restrictionScalar:=nil;
    end;
  end;

FUNCTION T_pattern.appendComparison(CONST parId:ansistring; CONST comparator:T_tokenType; CONST value:P_scalarLiteral):longint;
  begin
    result:=length(sig);
    setLength(sig,result+1);
    with sig[result] do begin
      id             :=parId;
      restrictionType:=comparator;
      if restrictionType=tt_comparatorListEq then restrictionType:=tt_comparatorEq;
      restrictionScalar:=value;
      value^.rereference;
      restrictionIdx :=-1;
      restrictionId  :='';
    end;
  end;

FUNCTION T_pattern.appendComparison(CONST parId:ansistring; CONST comparator:T_tokenType; CONST otherId:ansistring):longint;
  begin
    result:=length(sig);
    setLength(sig,result+1);
    with sig[result] do begin
      id             :=parId;
      restrictionType:=comparator;
      restrictionIdx :=-1;
      restrictionId  :=otherId;
      restrictionScalar:=nil;
    end;
  end;

PROCEDURE T_pattern.appendOptional;
  begin
    hasOptionals:=true;
  end;

FUNCTION T_pattern.indexOfId(CONST id:ansistring; CONST location:T_tokenLocation; VAR adapters:T_adapters):longint;
  VAR i:longint;
  begin
    if id=SELF_TOKEN_TEXT           then exit(SELF_TOKEN_PAR_IDX);
    if id=ALL_PARAMETERS_TOKEN_TEXT then exit(ALL_PARAMETERS_PAR_IDX);
    result:=strToIntDef(copy(id,2,length(id)-1),-1);
    if (copy(id,1,1)='$') and (result>=0) then begin
      if result<length(sig) then exit(result);
      adapters.raiseError('Trying to access paramete '+id+'; Maximum allowed is: $'+intToStr(length(sig)-1),location);
    end;
    for i:=0 to length(sig)-1 do if sig[i].id=id then exit(i);
    result:=-1;
  end;

FUNCTION T_pattern.indexOfIdForInline(CONST id:ansistring):longint;
  VAR i:longint;
  begin
    if id=SELF_TOKEN_TEXT           then begin containsSelfPointer:=true; exit(SELF_TOKEN_PAR_IDX); end;
    if id=ALL_PARAMETERS_TOKEN_TEXT then begin hasOptionals       :=true; exit(ALL_PARAMETERS_PAR_IDX); end;
    result:=strToIntDef(copy(id,2,length(id)-1),-1);
    if (copy(id,1,1)='$') and (result>=0) then begin
      while length(sig)<result+1 do appendFreeId('');
      exit(result);
    end;
    for i:=0 to length(sig)-1 do
    if sig[i].id=id then                      exit(i) else
    if sig[i].id='' then begin sig[i].id:=id; exit(i); end;
    result:=appendFreeId(id);
  end;


PROCEDURE T_pattern.finalizeRefs;
  VAR i,j:longint;
  begin
    for i:=1 to length(sig)-1 do if (sig[i].restrictionType=tt_literal) then begin
      j:=0;
      while (j<i) and (sig[j].id<>sig[i].id) do inc(j);
      if j<i then with sig[i] do begin
        sig[i].restrictionType:=tt_comparatorListEq;
        sig[i].restrictionIdx:=j;
        sig[i].restrictionId:=sig[j].id;
      end;
    end;
    for i:=0 to length(sig)-1 do
      if (sig[i].restrictionIdx<0) and
         (sig[i].restrictionId<>'') and
         (sig[i].restrictionType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
      then for j:=length(sig)-1 downto 0 do
        if (j<>i) and (sig[i].restrictionId=sig[j].id)
        then sig[i].restrictionIdx:=j;
  end;

FUNCTION T_pattern.matchesNilPattern:boolean;
  begin
    result:=(length(sig)=0);
  end;

FUNCTION T_pattern.matches(VAR par:T_listLiteral; CONST selfPointerProvided:boolean; VAR adapters:T_adapters):boolean;
  VAR i:longint;
      L:P_literal;
  begin
    if (par.size<length(sig)) or ((par.size>length(sig)) and not(hasOptionals)) then exit(false);
    for i:=0 to length(sig)-1 do begin
      L:=par.value(i);
      with sig[i] do case restrictionType of
        tt_typeCheckScalar..tt_typeCheckKeyValueList: if not(L^.literalType in C_matchingTypes[restrictionType]) then exit(false);
        tt_comparatorEq..tt_comparatorGrt:
          if L^.literalType in [lt_boolean,lt_int,lt_real,lt_string,lt_expression] then begin
            if restrictionIdx<0 then begin
              if not(P_scalarLiteral(L)^.isInRelationTo(restrictionType,restrictionScalar  ))
              then exit(false);
            end else begin
              if not((par.value(restrictionIdx)^.literalType in [lt_boolean,lt_int,lt_real,lt_string,lt_expression])
                 and P_scalarLiteral(L)^.isInRelationTo(restrictionType,P_scalarLiteral(par.value(restrictionIdx))))
              then exit(false);
            end;
          end else exit(false);
        tt_comparatorListEq:
          if restrictionIdx>=0 then begin
            if (par.value(restrictionIdx)^.literalType <> L^.literalType) then exit(false);
            if L^.literalType in [lt_boolean,lt_int,lt_real,lt_string,lt_expression] then begin
              if not(par.value(restrictionIdx)^.literalType in [lt_boolean,lt_int,lt_real,lt_string,lt_expression])
              or not(P_scalarLiteral(L)^.isInRelationTo(restrictionType,P_scalarLiteral(par.value(restrictionIdx)) ))
              then exit(false);
            end else begin
              L:=resolveOperator(L,tt_comparatorListEq,par.value(restrictionIdx),C_nilTokenLocation,adapters);
              if not(L^.literalType=lt_boolean) or not(P_boolLiteral(L)^.value) then begin
                disposeLiteral(L);
                exit(false);
              end else disposeLiteral(L);
            end;
          end else exit(false);
      end;
    end;
    result:=selfPointerProvided or not(containsSelfPointer);
  end;

FUNCTION T_pattern.toString:ansistring;
  FUNCTION trimLeadingEquals(CONST s:ansistring):ansistring;
    begin
      if (length(s)>1) and (s[1]='=') then begin
        result:=copy(s,2,length(s)-1);
        if (length(result)>1) and (result[1]='=')
        then result:=copy(result,2,length(result)-1);
      end else result:=s;
    end;

  VAR i:longint;
  begin
    if (length(sig)=0) and not(hasOptionals) then exit('');
    result:='(';
    for i:=0 to length(sig)-1 do with sig[i] do begin
      if i>0 then result:=result+', ';
      case restrictionType of
        tt_literal: result:=result+id;
        tt_typeCheckScalar    ,
        tt_typeCheckList      ,
        tt_typeCheckBoolean   ,
        tt_typeCheckBoolList  ,
        tt_typeCheckInt       ,
        tt_typeCheckIntList   ,
        tt_typeCheckReal      ,
        tt_typeCheckRealList  ,
        tt_typeCheckString    ,
        tt_typeCheckStringList,
        tt_typeCheckNumeric   ,
        tt_typeCheckNumList   ,
        tt_typeCheckExpression  ,
        tt_typeCheckNonemptyList,
        tt_typeCheckEmptyList,
        tt_typeCheckKeyValueList: result:=result+trimLeadingEquals(id+C_tokenString[restrictionType]);
        tt_comparatorEq,
        tt_comparatorNeq,
        tt_comparatorLeq,
        tt_comparatorGeq,
        tt_comparatorLss,
        tt_comparatorGrt,
        tt_comparatorListEq: if restrictionId='' then result:=result+trimLeadingEquals(id+C_tokenString[restrictionType]+restrictionScalar^.toString)
                                                 else result:=result+trimLeadingEquals(id+C_tokenString[restrictionType]+restrictionId);
        else result:=result+id;
      end;
    end;
    if hasOptionals then begin
      if  length(sig)>0 then result:=result+',';
      result:=result+'...';
    end;
    result:=result+')';
  end;

FUNCTION T_pattern.isEquivalent(CONST p:T_pattern):boolean;
  VAR i:longint;
  begin
    result:=(length(sig)=length(p.sig)) and
            (containsSelfPointer=p.containsSelfPointer) and
            (hasOptionals=p.hasOptionals);
    for i:=0 to length(sig)-1 do
      result:=result and (sig[i].restrictionType = p.sig[i].restrictionType)
                     and (sig[i].restrictionIdx  = p.sig[i].restrictionIdx )
                     and ((sig[i].restrictionScalar =nil) and (p.sig[i].restrictionScalar =nil)
                       or (sig[i].restrictionScalar<>nil) and (p.sig[i].restrictionScalar<>nil)
                       and sig[i].restrictionScalar^.isInRelationTo(tt_comparatorEq,p.sig[i].restrictionScalar));
  end;

PROCEDURE T_pattern.toParameterIds(CONST tok:P_token; VAR adapters:T_adapters);
  VAR t:P_token;
  begin
    t:=tok;
    while t<>nil do begin
      if (t^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_parameterIdentifier, tt_intrinsicRule])
      and (indexOfId(t^.txt,t^.location,adapters)>=0) then t^.tokType:=tt_parameterIdentifier;
      t:=t^.next;
    end;
  end;

{$endif}
