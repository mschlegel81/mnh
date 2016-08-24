{$ifdef include_interface}
  T_patternElement=object
    private
      id               :ansistring;
      restrictionType  :T_tokenType;
      restrictionValue :P_literal;
      restrictionIdx   :longint;
      typeWhitelist    :T_literalTypeSet;
      restrictionId    :ansistring;
      customTypeCheck  :P_subrule;
    public
    CONSTRUCTOR createAnonymous;
    CONSTRUCTOR create(CONST parameterId:ansistring);
    FUNCTION accept(VAR parameterList:T_listLiteral; CONST ownIndex:longint; VAR context:T_evaluationContext):boolean;
    FUNCTION toString:ansistring;
    FUNCTION toCmdLineHelpStringString:ansistring;
    FUNCTION isEquivalent(CONST pe:T_patternElement):boolean;
    PROCEDURE lateRHSResolution(CONST location:T_tokenLocation; VAR context:T_evaluationContext);
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
    CONSTRUCTOR combineForInline(CONST LHSPattern,RHSPattern:T_pattern);
    PROCEDURE clear;
    DESTRUCTOR destroy;
    FUNCTION appendFreeId(CONST parId:ansistring):longint;
    PROCEDURE append(CONST el:T_patternElement);
    PROCEDURE appendOptional;
    FUNCTION indexOfId(CONST id:ansistring):longint;
    FUNCTION indexOfIdForInline(CONST id:ansistring):longint;
    FUNCTION idForIndexInline(CONST index:longint):ansistring;
    PROCEDURE finalizeRefs(CONST location:T_tokenLocation; VAR context:T_evaluationContext);
    FUNCTION matches(VAR par:T_listLiteral; CONST selfPointerProvided:boolean; VAR context:T_evaluationContext):boolean;
    FUNCTION matchesForFallback(VAR par:T_listLiteral; CONST selfPointerProvided:boolean; VAR context:T_evaluationContext):boolean;
    FUNCTION matchesNilPattern:boolean;
    FUNCTION toString:ansistring;
    FUNCTION toCmdLineHelpStringString:ansistring;
    FUNCTION isEquivalent(CONST p:T_pattern):boolean;
    FUNCTION isEquivalentIgnoringRestrictions(CONST p:T_pattern):boolean;
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

FUNCTION T_patternElement.accept(VAR parameterList:T_listLiteral; CONST ownIndex:longint; VAR context:T_evaluationContext):boolean;
  VAR L:P_literal;
  begin
    L:=parameterList.value(ownIndex);
    if not(L^.literalType in typeWhitelist) then exit(false);
    if restrictionType=tt_customTypeCheck then exit(customTypeCheck^.accept(L,context));
    if (restrictionIdx>=0) and (restrictionType=tt_typeCheckExpression) and (P_expressionLiteral(L)^.arity<>restrictionIdx) then exit(false);
    if (restrictionIdx>=0) and (restrictionType in [tt_typeCheckList,tt_typeCheckBoolList,tt_typeCheckIntList,tt_typeCheckRealList,tt_typeCheckStringList,tt_typeCheckNumList,tt_typeCheckKeyValueList])
      and (P_listLiteral(L)^.size<>restrictionIdx) then exit(false);
    result:=true;
    if restrictionType in [tt_comparatorEq..tt_comparatorListEq,tt_operatorIn] then begin
      if restrictionIdx>=0 then result:=(parameterList.size>restrictionIdx) and
                                        L^.isInRelationTo(restrictionType, parameterList.value(restrictionIdx))
                           else result:=L^.isInRelationTo(restrictionType, restrictionValue                   );
    end;
  end;

FUNCTION T_patternElement.toString: ansistring;
  begin
    result:='';
    case restrictionType of
      tt_literal: result:=id;
      tt_customTypeCheck    : result:=id+':'+customTypeCheck^.parent^.id;
      tt_typeCheckScalar    ,
      tt_typeCheckBoolean   ,
      tt_typeCheckInt       ,
      tt_typeCheckReal      ,
      tt_typeCheckString    ,
      tt_typeCheckNumeric   : result:=(id+C_tokenInfo[restrictionType].defaultId);
      tt_typeCheckList      ,
      tt_typeCheckBoolList  ,
      tt_typeCheckIntList   ,
      tt_typeCheckRealList  ,
      tt_typeCheckStringList,
      tt_typeCheckNumList   ,
      tt_typeCheckKeyValueList,
      tt_typeCheckExpression  : if (restrictionIdx>=0) then result:=(id+C_tokenInfo[restrictionType].defaultId+'('+intToStr(restrictionIdx)+')')
                                                       else result:=(id+C_tokenInfo[restrictionType].defaultId);
      tt_comparatorEq,
      tt_comparatorNeq,
      tt_comparatorLeq,
      tt_comparatorGeq,
      tt_comparatorLss,
      tt_comparatorGrt: if restrictionId='' then result:=(id+C_tokenInfo[restrictionType].defaultId+restrictionValue^.toString)
                                            else result:=(id+C_tokenInfo[restrictionType].defaultId+restrictionId);
      tt_comparatorListEq: if restrictionId='' then result:=restrictionValue^.toString
                                               else result:=(id+C_tokenInfo[restrictionType].defaultId+restrictionId);
      tt_operatorIn: if restrictionId='' then result:=(id+' '+C_tokenInfo[restrictionType].defaultId+' '+restrictionValue^.toString)
                                         else result:=(id+' '+C_tokenInfo[restrictionType].defaultId+' '+restrictionId);
      else result:=result+id;
    end;
  end;

FUNCTION T_patternElement.toCmdLineHelpStringString:ansistring;
  begin
    case restrictionType of
      tt_comparatorListEq: if restrictionId='' then begin
        if restrictionValue^.literalType=lt_string then result:=P_stringLiteral(restrictionValue)^.value
                                                   else result:=restrictionValue^.toString
      end else result:='<'+id+'>';
      else result:='<'+id+'>';
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

PROCEDURE T_patternElement.lateRHSResolution(CONST location:T_tokenLocation; VAR context:T_evaluationContext);
  VAR tok:P_token;
  begin
    if (restrictionId<>'') and (restrictionIdx<0) then begin
      tok:=context.newToken(location,restrictionId,tt_identifier,nil);
      reduceExpression(tok,0,context);
      if (tok<>nil) and (tok^.next=nil) and (tok^.tokType=tt_literal) then begin
        restrictionId:='';
        restrictionValue:=tok^.data;
        restrictionValue^.rereference;
      end else context.adapters^.raiseError('Invalid pattern; cannot resolve ID "'+restrictionId+'"',location);
      context.cascadeDisposeToken(tok);
    end;
  end;

PROCEDURE T_patternElement.thinOutWhitelist;
  begin
    if restrictionType = tt_modifier_customType then exit;
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
          then typeWhitelist:=[lt_emptyList]
          else typeWhitelist:=[lt_list..lt_stringList, lt_keyValueList, lt_flatList];
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

CONSTRUCTOR T_pattern.combineForInline(CONST LHSPattern,RHSPattern:T_pattern);
  VAR i:longint;
  begin
    containsSelfPointer:=LHSPattern.containsSelfPointer
                      or RHSPattern.containsSelfPointer;
    hasOptionals:=LHSPattern.hasOptionals
               or RHSPattern.hasOptionals;
    setLength(sig,max(length(LHSPattern.sig),length(RHSPattern.sig)));
    for i:=0 to length(sig)-1 do begin
      if      (i<length(LHSPattern.sig)) and (LHSPattern.sig[i].id<>'') then sig[i]:=LHSPattern.sig[i]
      else if (i<length(RHSPattern.sig)) and (RHSPattern.sig[i].id<>'') then sig[i]:=RHSPattern.sig[i]
      else sig[i].createAnonymous;
    end;
  end;

PROCEDURE T_pattern.clear;
  VAR i:longint;
  begin
    for i:=0 to length(sig)-1 do sig[i].destroy;
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

FUNCTION T_pattern.idForIndexInline(CONST index:longint):ansistring;
  begin
    if index=SELF_TOKEN_PAR_IDX     then exit(SELF_TOKEN_TEXT);
    if index=ALL_PARAMETERS_PAR_IDX then exit(ALL_PARAMETERS_TOKEN_TEXT);
    result:=sig[index].id;
    if result='' then result:='$'+intToStr(index);
  end;

PROCEDURE T_pattern.finalizeRefs(CONST location:T_tokenLocation; VAR context:T_evaluationContext);
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
      sig[i].lateRHSResolution(location,context);
      sig[i].thinOutWhitelist;
    end;
  end;

FUNCTION T_pattern.matchesNilPattern: boolean;
  begin
    result:=(length(sig)=0);
  end;

FUNCTION T_pattern.matches(VAR par: T_listLiteral; CONST selfPointerProvided: boolean; VAR context:T_evaluationContext): boolean;
  begin
    result:=((par.size<=length(sig)) or hasOptionals) and matchesForFallback(par,selfPointerProvided,context);
  end;

FUNCTION T_pattern.matchesForFallback(VAR par:T_listLiteral; CONST selfPointerProvided:boolean; VAR context:T_evaluationContext):boolean;
  VAR i:longint;
  begin
    if (par.size<length(sig)) then exit(false);
    result:=true;
    for i:=0 to length(sig)-1 do if not(sig[i].accept(par,i,context)) then exit(false);
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

FUNCTION T_pattern.toCmdLineHelpStringString:ansistring;
  VAR i:longint;
  begin
    if (length(sig)=0) and not(hasOptionals) then exit('< no parameters >');
    result:='';
    for i:=0 to length(sig)-1 do begin
      if i>0 then result:=result+' ';
      result:=result+sig[i].toCmdLineHelpStringString;
    end;
    if hasOptionals then begin
      if  length(sig)>0 then result:=result+',';
      result:=result+'...';
    end;
  end;

FUNCTION T_pattern.isEquivalent(CONST p: T_pattern): boolean;
  VAR i:longint;
  begin
    result:=(length(sig)=length(p.sig)) and
            (containsSelfPointer=p.containsSelfPointer) and
            (hasOptionals=p.hasOptionals);
    for i:=0 to length(sig)-1 do result:=result and sig[i].isEquivalent(p.sig[i]);
  end;

FUNCTION T_pattern.isEquivalentIgnoringRestrictions(CONST p:T_pattern):boolean;
  VAR i:longint;
  begin
    result:=(length(sig)=length(p.sig)) and
            (containsSelfPointer=p.containsSelfPointer) and
            (hasOptionals=p.hasOptionals);
    for i:=0 to length(sig)-1 do result:=result and (sig[i].typeWhitelist=p.sig[i].typeWhitelist);
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
