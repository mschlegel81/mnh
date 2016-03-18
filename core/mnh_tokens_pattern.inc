{$ifdef include_interface}

  { T_patternElement }

  T_patternElement=object
    private
      id               :ansistring;
      restrictionType  :T_tokenType;
      restrictionValue :P_literal;
      restrictionIdx   :longint;
      typeWhitelist    :set of T_literalType;
      restrictionId    :ansistring;
    public
    CONSTRUCTOR createAnonymous;
    CONSTRUCTOR create(CONST parameterId:ansistring);
    FUNCTION accept(VAR parameterList:T_listLiteral; CONST ownIndex:longint):boolean;
    FUNCTION toString:ansistring;
    FUNCTION isEquivalent(CONST pe:T_patternElement):boolean;
    PROCEDURE lateRHSResolution(VAR context:T_evaluationContext; CONST package:P_package);
    PROCEDURE thinOutWhitelist;
    DESTRUCTOR destroy;
  end;

  P_pattern=^T_pattern;
  T_pattern=object
    sig:array of T_patternElement;
    containsSelfPointer,
    hasOptionals:boolean;

    CONSTRUCTOR create;
    CONSTRUCTOR clone(original:T_pattern);
    PROCEDURE clear;
    DESTRUCTOR destroy;
    FUNCTION appendFreeId    (CONST parId:ansistring):longint;
    PROCEDURE append(CONST el:T_patternElement);
    PROCEDURE appendOptional;
    FUNCTION indexOfId(CONST id:ansistring):longint;
    FUNCTION indexOfIdForInline(CONST id:ansistring):longint;
    PROCEDURE finalizeRefs(VAR context:T_evaluationContext; CONST package:P_package);
    FUNCTION matches(VAR par:T_listLiteral; CONST selfPointerProvided:boolean):boolean;
    FUNCTION matchesNilPattern:boolean;
    FUNCTION toString:ansistring;
    FUNCTION isEquivalent(CONST p:T_pattern):boolean;
    PROCEDURE toParameterIds(CONST tok:P_token);
  end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_patternElement.createAnonymous;
  begin
    id:='';
    restrictionType :=tt_literal;
    restrictionValue:=nil;
    restrictionIdx  :=-1;
    restrictionId   :='';
    typeWhitelist   :=C_validNonVoidTypes;
  end;

CONSTRUCTOR T_patternElement.create(CONST parameterId: ansistring);
  begin
    createAnonymous;
    id:=parameterId;
  end;

FUNCTION T_patternElement.accept(VAR parameterList:T_listLiteral; CONST ownIndex:longint):boolean;
  VAR L:P_literal;
  begin
    L:=parameterList.value(ownIndex);
    if not(L^.literalType in typeWhitelist) then exit(false);
    if (restrictionIdx>=0) and (restrictionType=tt_typeCheckExpression) and (P_subrule(P_expressionLiteral(L)^.value)^.arity<>restrictionIdx) then exit(false);
    if (restrictionIdx>=0) and (restrictionType in [tt_typeCheckList,tt_typeCheckBoolList,tt_typeCheckIntList,tt_typeCheckRealList,tt_typeCheckStringList,tt_typeCheckNumList,tt_typeCheckKeyValueList])
      and (P_listLiteral(L)^.size<>restrictionIdx) then exit(false);
    result:=true;
    if restrictionType in [tt_comparatorEq..tt_comparatorListEq,tt_operatorIn] then begin
      if restrictionIdx>=0 then result:=L^.isInRelationTo(restrictionType, parameterList.value(restrictionIdx))
                           else result:=L^.isInRelationTo(restrictionType, restrictionValue                   );
    end;
  end;

FUNCTION T_patternElement.toString: ansistring;
  FUNCTION trimLeadingEquals(CONST s:ansistring):ansistring;
    begin
      if (length(s)>1) and (s[1]='=') then begin
        result:=copy(s,2,length(s)-1);
        if (length(result)>1) and (result[1]='=')
        then result:=copy(result,2,length(result)-1);
      end else result:=s;
    end;

  begin
    result:='';
    case restrictionType of
      tt_literal: result:=id;
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
      tt_typeCheckKeyValueList: result:=trimLeadingEquals(id+C_tokenString[restrictionType]);
      tt_comparatorEq,
      tt_comparatorNeq,
      tt_comparatorLeq,
      tt_comparatorGeq,
      tt_comparatorLss,
      tt_comparatorGrt,
      tt_comparatorListEq: if restrictionId='' then result:=trimLeadingEquals(id+C_tokenString[restrictionType]+restrictionValue^.toString)
                                               else result:=trimLeadingEquals(id+C_tokenString[restrictionType]+restrictionId);
      tt_operatorIn: if restrictionId='' then result:=trimLeadingEquals(id+' '+C_tokenString[restrictionType]+' '+restrictionValue^.toString)
                                         else result:=trimLeadingEquals(id+' '+C_tokenString[restrictionType]+' '+restrictionId);
      else result:=result+id;
    end;
  end;

FUNCTION T_patternElement.isEquivalent(CONST pe: T_patternElement): boolean;
  begin
    result:=(restrictionType = pe.restrictionType)
        and (restrictionIdx  = pe.restrictionIdx )
        and ((restrictionValue =nil) and (pe.restrictionValue =nil)
          or (restrictionValue<>nil) and (pe.restrictionValue<>nil)
          and restrictionValue^.isInRelationTo(tt_comparatorListEq,pe.restrictionValue));
  end;

PROCEDURE T_patternElement.lateRHSResolution(VAR context:T_evaluationContext; CONST package:P_package);
  VAR tok:P_token;
  begin
    if (restrictionId<>'') and (restrictionIdx<0) then begin
      tok:=context.newToken(C_nilTokenLocation,restrictionId,tt_identifier,nil);
      reduceExpression(tok,0,context);
      if (tok<>nil) and (tok^.next=nil) and (tok^.tokType=tt_literal) then begin
        restrictionId:='';
        restrictionValue:=tok^.data;
        restrictionValue^.rereference;
      end else context.adapters^.raiseError('Invalid pattern; cannot resolve ID "'+restrictionId+'"',C_nilTokenLocation);
      context.cascadeDisposeToken(tok);
    end;
  end;

PROCEDURE T_patternElement.thinOutWhitelist;
  begin
    if restrictionType in C_typeChecks then typeWhitelist:=C_matchingTypes[restrictionType];
    if (restrictionType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
       and (restrictionValue<>nil)
    then begin
      case restrictionValue^.literalType of
        lt_int : typeWhitelist:=[lt_int];
        lt_real: typeWhitelist:=[lt_int,lt_real];
        lt_string: typeWhitelist:=[lt_string];
        lt_boolean: typeWhitelist:=[lt_boolean];
        lt_emptyList: if restrictionType in [tt_comparatorEq,tt_comparatorListEq]
          then typeWhitelist:=C_matchingTypes[tt_typeCheckEmptyList]
          else typeWhitelist:=C_matchingTypes[tt_typeCheckNonemptyList];
      end;
    end;
  end;

DESTRUCTOR T_patternElement.destroy;
  begin
    if restrictionValue<>nil then disposeLiteral(restrictionValue);
  end;

CONSTRUCTOR T_pattern.create;
  begin clear; end;

CONSTRUCTOR T_pattern.clone(original: T_pattern);
  VAR i:longint;
  begin
    containsSelfPointer:=original.containsSelfPointer;
    hasOptionals:=original.hasOptionals;
    setLength(sig,length(original.sig));
    for i:=0 to length(sig)-1 do begin
      sig[i]:=original.sig[i];
      if sig[i].restrictionValue<>nil then sig[i].restrictionValue^.rereference;
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

FUNCTION T_pattern.appendFreeId(CONST parId: ansistring): longint;
  begin
    result:=length(sig);
    setLength(sig,result+1);
    sig[result].create(parId);
  end;

PROCEDURE T_pattern.append(CONST el: T_patternElement);
  begin
    setLength(sig,length(sig)+1);
    sig[length(sig)-1]:=el;
  end;

PROCEDURE T_pattern.appendOptional;
  begin
    hasOptionals:=true;
  end;

FUNCTION T_pattern.indexOfId(CONST id: ansistring): longint;
  VAR i:longint;
  begin
    if id=SELF_TOKEN_TEXT           then exit(SELF_TOKEN_PAR_IDX);
    if id=ALL_PARAMETERS_TOKEN_TEXT then exit(ALL_PARAMETERS_PAR_IDX);
    for i:=0 to length(sig)-1 do if sig[i].id=id then exit(i);
    result:=-1;
  end;

FUNCTION T_pattern.indexOfIdForInline(CONST id: ansistring): longint;
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

PROCEDURE T_pattern.finalizeRefs(VAR context:T_evaluationContext; CONST package:P_package);
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
         (sig[i].restrictionType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq, tt_operatorIn])
      then for j:=length(sig)-1 downto 0 do
        if (j<>i) and (sig[i].restrictionId=sig[j].id)
        then sig[i].restrictionIdx:=j;

    for i:=0 to length(sig)-1 do begin
      sig[i].lateRHSResolution(context,package);
      sig[i].thinOutWhitelist;
    end;
  end;

FUNCTION T_pattern.matchesNilPattern: boolean;
  begin
    result:=(length(sig)=0);
  end;

FUNCTION T_pattern.matches(VAR par: T_listLiteral; CONST selfPointerProvided: boolean): boolean;
  VAR i:longint;
  begin
    if (par.size<length(sig)) or ((par.size>length(sig)) and not(hasOptionals)) then exit(false);
    result:=true;
    for i:=0 to length(sig)-1 do if not(sig[i].accept(par,i)) then exit(false);
    result:=selfPointerProvided or not(containsSelfPointer);
  end;

FUNCTION T_pattern.toString: ansistring;
  VAR i:longint;
  begin
    if (length(sig)=0) and not(hasOptionals) then exit('');
    result:='(';
    for i:=0 to length(sig)-1 do begin
      if i>0 then result:=result+', ';
      result:=result+sig[i].toString;
    end;
    if hasOptionals then begin
      if  length(sig)>0 then result:=result+',';
      result:=result+'...';
    end;
    result:=result+')';
  end;

FUNCTION T_pattern.isEquivalent(CONST p: T_pattern): boolean;
  VAR i:longint;
  begin
    result:=(length(sig)=length(p.sig)) and
            (containsSelfPointer=p.containsSelfPointer) and
            (hasOptionals=p.hasOptionals);
    for i:=0 to length(sig)-1 do result:=result and sig[i].isEquivalent(p.sig[i]);
  end;

PROCEDURE T_pattern.toParameterIds(CONST tok: P_token);
  VAR t:P_token;
  begin
    t:=tok;
    while t<>nil do begin
      if (t^.tokType in [tt_identifier, tt_localUserRule, tt_importedUserRule, tt_parameterIdentifier, tt_intrinsicRule])
      and (indexOfId(t^.txt)>=0) then t^.tokType:=tt_parameterIdentifier;
      t:=t^.next;
    end;
  end;

{$endif}
