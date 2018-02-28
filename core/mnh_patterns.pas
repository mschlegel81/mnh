UNIT mnh_patterns;
INTERFACE
USES sysutils,
     math,
     myGenerics,
     mnh_basicTypes,mnh_constants,
     mnh_litVar,
     mnh_tokens,
     mnh_contexts;
TYPE
  T_patternElement=object
    private
      elementLocation  :T_tokenLocation;
      id               :T_idString;
      restrictionType  :T_tokenType;
      restrictionValue :P_literal;
      restrictionIdx   :longint;
      typeWhitelist    :T_literalTypeSet;
      restrictionId    :T_idString;
      builtinTypeCheck :T_typeCheck;
      customTypeCheck  :P_abstractRule;
      FUNCTION accept(VAR parameterList:T_listLiteral; CONST ownIndex:longint; CONST location:T_tokenLocation; VAR context:T_threadContext):boolean;
      FUNCTION toString:ansistring;
      FUNCTION toCmdLineHelpStringString:ansistring;
      FUNCTION isEquivalent(CONST pe:T_patternElement):boolean;
      PROCEDURE lateRHSResolution(CONST location:T_tokenLocation; VAR context:T_threadContext);
      PROCEDURE thinOutWhitelist;
      FUNCTION hides(CONST e:T_patternElement):boolean;
      PROPERTY getId:T_idString read id;
    public
      CONSTRUCTOR createAnonymous(CONST loc:T_tokenLocation);
      CONSTRUCTOR create(CONST parameterId:T_idString; CONST loc:T_tokenLocation);
      DESTRUCTOR destroy;
  end;

  T_patternElementLocation=object
    id:T_idString;
    location:T_tokenLocation;
  end;
  T_patternElementLocations=array of T_patternElementLocation;

  P_pattern=^T_pattern;
  T_pattern=object
    private
      sig:array of T_patternElement;
      hasOptionals:boolean;
      PROCEDURE append(CONST el:T_patternElement);
      PROCEDURE finalizeRefs(CONST location:T_tokenLocation; VAR context:T_threadContext);
    public
      CONSTRUCTOR create;
      CONSTRUCTOR clone(original:T_pattern);
      CONSTRUCTOR combineForInline(CONST LHSPattern,RHSPattern:T_pattern; CONST fallbackLocation:T_tokenLocation);
      DESTRUCTOR destroy;
      PROCEDURE clear;
      FUNCTION appendFreeId(CONST parId:T_idString; CONST location:T_tokenLocation):longint;
      FUNCTION indexOfId(CONST id:T_idString):longint;
      FUNCTION indexOfIdForInline(CONST id:T_idString; CONST location:T_tokenLocation):longint;
      PROCEDURE appendOptional;
      FUNCTION arity:longint;
      FUNCTION canApplyToNumberOfParameters(CONST parCount:longint):boolean;
      PROPERTY isVariadic:boolean read hasOptionals;
      FUNCTION isValidMainPattern:boolean;
      FUNCTION isValidCustomTypeCheckPattern:boolean;
      FUNCTION isValidMutablePattern:boolean;
      FUNCTION acceptsSingleLiteral(CONST literalTypeToAccept:T_literalType):boolean;
      FUNCTION isEquivalent(CONST p:T_pattern):boolean;
      FUNCTION hides(CONST p:T_pattern):boolean;
      FUNCTION getParameterNames:P_listLiteral;
      FUNCTION getNamedParameters:T_patternElementLocations;
      FUNCTION matchesNilPattern:boolean;
      FUNCTION matches(VAR par:T_listLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext):boolean;
      FUNCTION matchesForFallback(VAR par:T_listLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext):boolean;
      FUNCTION idForIndexInline(CONST index:longint):T_idString;
      PROCEDURE parse(VAR first:P_token; CONST ruleDeclarationStart:T_tokenLocation; VAR context:T_threadContext);
      FUNCTION toString:ansistring;
      FUNCTION toCmdLineHelpStringString:ansistring;
      PROCEDURE toParameterIds(CONST tok:P_token);
      {$ifdef fullVersion}
      PROCEDURE complainAboutUnusedParameters(CONST usedIds:T_arrayOfLongint; VAR context:T_threadContext; CONST subruleLocation:T_tokenLocation);
      {$endif}
      FUNCTION getFirstParameterTypeWhitelist:T_literalTypeSet;
  end;

FUNCTION typeCheckAccept(CONST valueToCheck:P_literal; CONST check:T_typeCheck; CONST modifier:longint=-1):boolean; inline;

IMPLEMENTATION
FUNCTION typeCheckAccept(CONST valueToCheck:P_literal; CONST check:T_typeCheck; CONST modifier:longint=-1):boolean;
  begin
    if not(valueToCheck^.literalType in C_typeCheckInfo[check].matching) then exit(false);
    if modifier<0 then case check of
      tc_typeCheckStatelessExpression : result:=not(P_expressionLiteral(valueToCheck)^.typ in C_statefulExpressionTypes);
      tc_typeCheckStatefulExpression  : result:=   (P_expressionLiteral(valueToCheck)^.typ in C_statefulExpressionTypes);
      tc_typeCheckIteratableExpression: result:=    P_expressionLiteral(valueToCheck)^.typ in C_iteratableExpressionTypes;
      tc_typeCheckIteratable          : result:=   (valueToCheck^.literalType<>lt_expression) or
                                                   (P_expressionLiteral(valueToCheck)^.typ in C_iteratableExpressionTypes);
      else result:=true;
    end else case check of
      tc_typeCheckList,       tc_typeCheckSet,       tc_typeCheckCollection,
      tc_typeCheckBoolList,   tc_typeCheckBoolSet,   tc_typeCheckBoolCollection,
      tc_typeCheckIntList,    tc_typeCheckIntSet,    tc_typeCheckIntCollection,
      tc_typeCheckRealList,   tc_typeCheckRealSet,   tc_typeCheckRealCollection,
      tc_typeCheckStringList, tc_typeCheckStringSet, tc_typeCheckStringCollection,
      tc_typeCheckNumList,    tc_typeCheckNumSet,    tc_typeCheckNumCollection,
      tc_typeCheckMap:                 result:=P_compoundLiteral  (valueToCheck)^.size =                       modifier ;
      tc_typeCheckExpression:          result:=P_expressionLiteral(valueToCheck)^.canApplyToNumberOfParameters(modifier);
      tc_typeCheckStatelessExpression: result:=not(P_expressionLiteral(valueToCheck)^.typ in C_statefulExpressionTypes) and
                                                   P_expressionLiteral(valueToCheck)^.canApplyToNumberOfParameters(modifier);
      tc_typeCheckStatefulExpression : result:=   (P_expressionLiteral(valueToCheck)^.typ in C_statefulExpressionTypes) and
                                                   P_expressionLiteral(valueToCheck)^.canApplyToNumberOfParameters(modifier);
      else result:=false;
    end;
  end;
CONSTRUCTOR T_patternElement.createAnonymous(CONST loc:T_tokenLocation);
  begin
    id:='';
    restrictionType :=tt_literal;
    restrictionValue:=nil;
    restrictionIdx  :=-1;
    restrictionId   :='';
    builtinTypeCheck:=tc_typeCheckScalar;
    customTypeCheck :=nil;
    typeWhitelist   :=[lt_boolean..lt_emptyMap];
    elementLocation:=loc;
  end;

CONSTRUCTOR T_patternElement.create(CONST parameterId: T_idString; CONST loc:T_tokenLocation);
  begin
    createAnonymous(loc);
    id:=parameterId;
  end;

FUNCTION T_patternElement.accept(VAR parameterList:T_listLiteral; CONST ownIndex:longint; CONST location:T_tokenLocation; VAR context:T_threadContext):boolean;
  VAR L:P_literal;
  begin
    L:=parameterList.value[ownIndex];
    if not(L^.literalType in typeWhitelist) then exit(false);
    result:=true;
    case restrictionType of
      tt_customTypeCheck:    exit(customTypeCheck^.evaluateToBoolean(L,location,true,@context,@context.recycler));
      tt_typeCheck:          exit(typeCheckAccept(L,builtinTypeCheck,restrictionIdx));
      tt_comparatorEq..tt_comparatorListEq,tt_operatorIn:begin
        if restrictionIdx>=0 then result:=(parameterList.size>restrictionIdx) and
                                          L^.isInRelationTo(restrictionType, parameterList.value[restrictionIdx])
                             else result:=L^.isInRelationTo(restrictionType, restrictionValue             );
      end;
    end;
  end;

FUNCTION T_patternElement.toString: ansistring;
  begin
    result:='';
    case restrictionType of
      tt_literal: result:=id;
      tt_customTypeCheck    : result:=id+':'+customTypeCheck^.getId;
      tt_typeCheck: if C_typeCheckInfo[builtinTypeCheck].modifiable and (restrictionIdx>=0)
                    then result:=id+':'+C_typeCheckInfo[builtinTypeCheck].name+'('+intToStr(restrictionIdx)+')'
                    else result:=id+':'+C_typeCheckInfo[builtinTypeCheck].name;
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
  VAR iter:T_arrayOfLiteral;
      l:P_literal;
  begin
    case restrictionType of
      tt_comparatorListEq, tt_comparatorEq: if restrictionId='' then begin
        if restrictionValue^.literalType=lt_string then result:=P_stringLiteral(restrictionValue)^.value
                                                   else result:=restrictionValue^.toString
      end else result:='<'+id+'>';
      tt_operatorIn: if (restrictionValue<>nil) and (restrictionValue^.literalType in [lt_stringList,lt_stringSet]) then begin
        iter:=P_compoundLiteral(restrictionValue)^.iteratableList;
        result:='';
        for l in iter do begin
          if result<>'' then result:=result+'|';
          result:=result+P_stringLiteral(l)^.value;
        end;
        result:='['+result+']';
        disposeLiteral(iter);
        if id<>'' then result:='<'+id+'> in '+result;
      end else result:='<'+id+'>';
      else result:='<'+id+'>';
    end;
  end;

FUNCTION T_patternElement.isEquivalent(CONST pe: T_patternElement): boolean;
  begin
    result:=(restrictionType = pe.restrictionType)
        and (typeWhitelist   = pe.typeWhitelist  )
        and (restrictionIdx  = pe.restrictionIdx )
        and (builtinTypeCheck= pe.builtinTypeCheck)
        and (customTypeCheck = pe.customTypeCheck)
        and ((restrictionValue =nil) and (pe.restrictionValue =nil)
          or (restrictionValue<>nil) and (pe.restrictionValue<>nil)
          and restrictionValue^.isInRelationTo(tt_comparatorListEq,pe.restrictionValue));
  end;

PROCEDURE T_patternElement.lateRHSResolution(CONST location:T_tokenLocation; VAR context:T_threadContext);
  VAR tok:P_token;
  begin
    if (restrictionId<>'') and (restrictionIdx<0) then begin
      tok:=context.recycler.newToken(location,restrictionId,tt_identifier,nil);
      context.reduceExpression(tok);
      if (tok<>nil) and (tok^.next=nil) and (tok^.tokType=tt_literal) then begin
        restrictionId:='';
        restrictionValue:=P_literal(tok^.data)^.rereferenced;
      end else context.adapters^.raiseError('Invalid pattern; cannot resolve ID "'+restrictionId+'"',location);
      context.recycler.cascadeDisposeToken(tok);
    end;
  end;

PROCEDURE T_patternElement.thinOutWhitelist;
  begin
    if restrictionType = tt_customTypeCheck then begin
      typeWhitelist:=customTypeCheck^.getFirstParameterTypeWhitelist;
      exit;
    end;
    if restrictionType = tt_typeCheck then typeWhitelist:=C_typeCheckInfo[builtinTypeCheck].matching;
    if (restrictionType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
       and (restrictionValue<>nil)
    then begin
      case restrictionValue^.literalType of
        lt_int      : typeWhitelist:=[lt_int];
        lt_real     : typeWhitelist:=[lt_int,lt_real];
        lt_string   : typeWhitelist:=[lt_string];
        lt_boolean  : typeWhitelist:=[lt_boolean];
        lt_emptyList: if restrictionType in [tt_comparatorEq,tt_comparatorListEq]
                 then typeWhitelist:=[lt_emptyList]
                 else typeWhitelist:=C_listTypes-[lt_emptyList];
        lt_emptySet:  if restrictionType in [tt_comparatorEq,tt_comparatorListEq]
                 then typeWhitelist:=[lt_emptySet]
                 else typeWhitelist:=C_setTypes-[lt_emptySet];
        lt_emptyMap:  if restrictionType in [tt_comparatorEq,tt_comparatorListEq]
                 then typeWhitelist:=[lt_emptyMap]
                 else typeWhitelist:=[lt_map];
      end;
    end;
  end;

FUNCTION T_patternElement.hides(CONST e:T_patternElement):boolean;
  TYPE T_valueRelation=(vr_unknown,vr_equal,vr_lesser,vr_greater);

  FUNCTION getValueRelation:T_valueRelation;
    begin
      if (restrictionIdx>=0) and (e.restrictionIdx=restrictionIdx) then exit(vr_equal);
      if (restrictionIdx<0) and (e.restrictionIdx<0) and (e.restrictionType<>tt_typeCheck) then begin
        if      restrictionValue^.isInRelationTo(tt_comparatorListEq,e.restrictionValue) then exit(vr_equal)
        else if restrictionValue^.isInRelationTo(tt_comparatorLss   ,e.restrictionValue) then exit(vr_lesser)
        else if restrictionValue^.isInRelationTo(tt_comparatorGrt   ,e.restrictionValue) then exit(vr_greater);
      end;
      result:=vr_unknown;
    end;

  begin
    result:=false;
    if e.typeWhitelist-typeWhitelist<>[] then exit(false); //e accepts types not accepted by self
    case restrictionType of
      tt_customTypeCheck:
        exit((e.restrictionType=tt_customTypeCheck) and (customTypeCheck=e.customTypeCheck));
      tt_type,tt_typeCheck:
        exit((e.restrictionIdx=restrictionIdx) or (restrictionIdx<0) and (e.restrictionIdx>=0));
      tt_comparatorListEq,
      tt_comparatorEq    : exit((e.restrictionType in [tt_comparatorEq,tt_comparatorListEq]) and (getValueRelation=vr_equal));
      tt_comparatorNeq   : exit((e.restrictionType = tt_comparatorNeq) and (getValueRelation=vr_equal));
      tt_comparatorLeq   :
        case getValueRelation of
          vr_unknown,vr_lesser: exit(false);
          vr_equal, vr_greater: exit(e.restrictionType in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorLss,tt_comparatorLeq]);
        end;
      tt_comparatorGeq   :
        case getValueRelation of
          vr_unknown,vr_greater: exit(false);
          vr_equal,vr_lesser: exit(e.restrictionType in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorGrt,tt_comparatorGeq]);
        end;
      tt_comparatorLss:
        case getValueRelation of
          vr_unknown,vr_lesser: exit(false);
          vr_equal  : exit(e.restrictionType in [tt_comparatorNeq,tt_comparatorLss]);
          vr_greater: exit(e.restrictionType in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorLss,tt_comparatorLeq]);
        end;
      tt_comparatorGrt:
        case getValueRelation of
          vr_unknown,vr_greater: exit(false);
          vr_equal : exit(e.restrictionType in [tt_comparatorNeq,tt_comparatorGrt]);
          vr_lesser: exit(e.restrictionType in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorGrt,tt_comparatorGeq]);
        end;
      tt_operatorIn: exit(false); //TODO: this is too naive
      else exit(true);
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
    hasOptionals:=original.hasOptionals;
    setLength(sig,length(original.sig));
    for i:=0 to length(sig)-1 do begin
      sig[i]:=original.sig[i];
      if sig[i].restrictionValue<>nil then sig[i].restrictionValue^.rereference;
    end;
  end;

CONSTRUCTOR T_pattern.combineForInline(CONST LHSPattern,RHSPattern:T_pattern; CONST fallbackLocation:T_tokenLocation);
  VAR i:longint;
  begin
    hasOptionals:=LHSPattern.hasOptionals
               or RHSPattern.hasOptionals;
    setLength(sig,max(length(LHSPattern.sig),length(RHSPattern.sig)));
    for i:=0 to length(sig)-1 do begin
      if      (i<length(LHSPattern.sig)) and (LHSPattern.sig[i].id<>'') then sig[i]:=LHSPattern.sig[i]
      else if (i<length(RHSPattern.sig)) and (RHSPattern.sig[i].id<>'') then sig[i]:=RHSPattern.sig[i]
      else sig[i].createAnonymous(fallbackLocation);
    end;
  end;

PROCEDURE T_pattern.clear;
  VAR i:longint;
  begin
    for i:=0 to length(sig)-1 do sig[i].destroy;
    setLength(sig,0);
    hasOptionals:=false;
  end;

DESTRUCTOR T_pattern.destroy;
  begin clear; end;

FUNCTION T_pattern.appendFreeId(CONST parId: T_idString; CONST location:T_tokenLocation): longint;
  begin
    result:=length(sig);
    setLength(sig,result+1);
    sig[result].create(parId,location);
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

FUNCTION T_pattern.arity:longint;
  begin
    result:=length(sig);
  end;

FUNCTION T_pattern.canApplyToNumberOfParameters(CONST parCount:longint):boolean;
  begin
    result:=(length(sig)<=parCount) and hasOptionals or (length(sig)=parCount);
  end;

FUNCTION T_pattern.indexOfId(CONST id: T_idString): longint;
  VAR i:longint;
  begin
    if id=ALL_PARAMETERS_TOKEN_TEXT then exit(ALL_PARAMETERS_PAR_IDX);
    for i:=0 to length(sig)-1 do if sig[i].id=id then exit(i);
    result:=-1;
  end;

FUNCTION T_pattern.indexOfIdForInline(CONST id: T_idString; CONST location:T_tokenLocation): longint;
  VAR i:longint;
  begin
    if id=ALL_PARAMETERS_TOKEN_TEXT then begin hasOptionals:=true; exit(ALL_PARAMETERS_PAR_IDX); end;
    result:=strToIntDef(copy(id,2,length(id)-1),-1);
    if (copy(id,1,1)='$') and (result>=0) then begin
      while length(sig)<result+1 do appendFreeId('',location);
      exit(result);
    end;
    for i:=0 to length(sig)-1 do
    if sig[i].id=id then                      exit(i) else
    if sig[i].id='' then begin sig[i].id:=id; exit(i); end;
    result:=appendFreeId(id,location);
  end;

FUNCTION T_pattern.idForIndexInline(CONST index:longint):T_idString;
  begin
    if index=ALL_PARAMETERS_PAR_IDX then exit(ALL_PARAMETERS_TOKEN_TEXT);
    if (index>=0) and (index<length(sig))
    then result:=sig[index].id
    else result:='';
    if result='' then result:='$'+intToStr(index);
  end;

PROCEDURE T_pattern.finalizeRefs(CONST location:T_tokenLocation; VAR context:T_threadContext);
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

FUNCTION T_pattern.matches(VAR par: T_listLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext): boolean;
  begin
    result:=((par.size<=length(sig)) or hasOptionals) and matchesForFallback(par,location,context);
  end;

FUNCTION T_pattern.matchesForFallback(VAR par:T_listLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext):boolean;
  VAR i:longint;
  begin
    if (par.size<length(sig)) then exit(false);
    for i:=0 to length(sig)-1 do if not(sig[i].accept(par,i,location,context)) then exit(false);
    result:=true;
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
      if  length(sig)>0 then result:=result+' ';
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
            (hasOptionals=p.hasOptionals);
    for i:=0 to length(sig)-1 do result:=result and sig[i].isEquivalent(p.sig[i]);
  end;

FUNCTION T_pattern.hides(CONST p:T_pattern):boolean;
  VAR i:longint;
  begin
    if not(hasOptionals) then begin
      if p.hasOptionals               then exit(false); //p accepts more parameters
      if (length(sig)<>length(p.sig)) then exit(false); //p accepts a different number of parameters
    end else begin
      if length(sig)>length(p.sig) then exit(false); //p accepts fewer parameters
    end;
    for i:=0 to length(sig)-1 do if not(sig[i].hides(p.sig[i])) then exit(false);
    result:=true;
  end;

FUNCTION T_pattern.isValidMainPattern:boolean;
  VAR i:longint;
  begin
    result:=true;
    for i:=0 to length(sig)-1 do result:=result and (lt_string in sig[i].typeWhitelist);
  end;

FUNCTION T_pattern.isValidCustomTypeCheckPattern:boolean; begin result:=(length(sig)=1) and not(hasOptionals); end;
FUNCTION T_pattern.isValidMutablePattern        :boolean; begin result:=(length(sig)=0) and not(hasOptionals); end;
FUNCTION T_pattern.acceptsSingleLiteral(CONST literalTypeToAccept:T_literalType):boolean; begin result:=(length(sig)=1) and (literalTypeToAccept in sig[0].typeWhitelist); end;
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

FUNCTION T_pattern.getParameterNames:P_listLiteral;
  VAR el:T_patternElement;
  begin
    result:=newListLiteral(length(sig));
    for el in sig do result^.appendString(el.getId);
  end;

FUNCTION T_pattern.getNamedParameters:T_patternElementLocations;
  VAR el:T_patternElement;
      i:longint=0;
  begin
    setLength(result,length(sig));
    for el in sig do if el.id<>'' then begin
      result[i].location:=el.elementLocation;
      result[i].id      :=el.id;
      inc(i);
    end;
    setLength(result,i);
  end;

PROCEDURE T_pattern.parse(VAR first:P_token; CONST ruleDeclarationStart:T_tokenLocation; VAR context:T_threadContext);
  CONST MSG_INVALID_OPTIONAL='Optional parameters are allowed only as last entry in a function head declaration.';
  VAR parts:T_bodyParts;
      closingBracket:P_token;
      i:longint;
      partLocation:T_tokenLocation;
      rulePatternElement:T_patternElement;

  PROCEDURE fail(VAR firstOfPart:P_token);
    begin
      if firstOfPart=nil
      then context.adapters^.raiseError('Invalid declaration pattern element.',partLocation)
      else context.adapters^.raiseError('Invalid declaration pattern element: '+tokensToString(firstOfPart,20),firstOfPart^.location);
      context.recycler.cascadeDisposeToken(firstOfPart);
    end;

  PROCEDURE assertNil(VAR firstOfPart:P_token);
    begin
      if firstOfPart<>nil then fail(firstOfPart);
    end;

  begin
    if (first^.next<>nil) and (first^.next^.tokType=tt_braceClose) then begin
      setLength(parts,0);
      closingBracket:=first^.next;
    end else begin
      parts:=getBodyParts(first,0,context.recycler,context.adapters,closingBracket);
      if closingBracket=nil then begin
        context.recycler.cascadeDisposeToken(first);
        exit;
      end;
      if (closingBracket^.next<>nil) and not(closingBracket^.next^.tokType in [tt_assign,tt_declare]) then begin
        context.adapters^.raiseError('Invalid pattern suffix '+tokensToString(closingBracket^.next),closingBracket^.next^.location);
        context.recycler.cascadeDisposeToken(closingBracket^.next);
      end;

      for i:=0 to length(parts)-1 do begin
        partLocation:=parts[i].first^.location;
        if (parts[i].first^.tokType=tt_optionalParameters) and (parts[i].first^.next=nil) then begin
          if i<>length(parts)-1 then context.adapters^.raiseError(MSG_INVALID_OPTIONAL,parts[i].first^.location);
          //Optionals: f(...)->
          appendOptional;
          parts[i].first:=context.recycler.disposeToken(parts[i].first);
          assertNil(parts[i].first);
        end else if (parts[i].first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule]) then begin
          //Identified parameter: f(x)->
          rulePatternElement.create(parts[i].first^.txt,parts[i].first^.location);
          parts[i].first:=context.recycler.disposeToken(parts[i].first);
          if (parts[i].first<>nil) then begin
            if (parts[i].first^.tokType=tt_typeCheck) then begin
              rulePatternElement.restrictionType:=parts[i].first^.tokType;
              rulePatternElement.builtinTypeCheck:=parts[i].first^.getTypeCheck;
              parts[i].first:=context.recycler.disposeToken(parts[i].first);

              if C_typeCheckInfo[rulePatternElement.builtinTypeCheck].modifiable then begin
                if (parts[i].first=nil) then begin end else
                if (parts[i].first^.tokType=tt_braceOpen) then begin
                  if (parts[i].first^.next<>nil) and
                     (parts[i].first^.next^.tokType=tt_literal) and
                     (P_literal   (parts[i].first^.next^.data)^.literalType=lt_int) and
                     (P_intLiteral(parts[i].first^.next^.data)^.value.toInt>=0) and
                     (parts[i].first^.next^.next<>nil) and
                     (parts[i].first^.next^.next^.tokType=tt_braceClose) and
                     (parts[i].first^.next^.next^.next=nil) then begin
                      rulePatternElement.restrictionIdx:=P_intLiteral(parts[i].first^.next^.data)^.value.toInt;
                      context.recycler.cascadeDisposeToken(parts[i].first);
                  end else begin
                    context.reduceExpression(parts[i].first);
                    if (context.adapters^.noErrors) and
                       (parts[i].first<>nil) and
                       (parts[i].first^.tokType=tt_literal) and
                       (P_literal   (parts[i].first^.data)^.literalType=lt_int) and
                       (P_intLiteral(parts[i].first^.data)^.value.toInt>=0)
                    then begin
                      rulePatternElement.restrictionIdx:=P_intLiteral(parts[i].first^.data)^.value.toInt;
                      context.recycler.cascadeDisposeToken(parts[i].first);
                    end else fail(parts[i].first);
                  end;
                end else fail(parts[i].first);
              end else assertNil(parts[i].first);

            end else if (parts[i].first^.tokType in C_comparators) then begin
              rulePatternElement.restrictionType:=parts[i].first^.tokType;
              parts[i].first:=context.recycler.disposeToken(parts[i].first);

              if (parts[i].first=nil) then fail(parts[i].first) else
              if parts[i].first^.tokType in [tt_identifier,tt_localUserRule,tt_importedUserRule,tt_intrinsicRule] then begin
                rulePatternElement.restrictionId:=parts[i].first^.txt;
                parts[i].first:=context.recycler.disposeToken(parts[i].first);
                assertNil(parts[i].first);
              end else begin
                context.reduceExpression(parts[i].first);
                if (parts[i].first<>nil) and (parts[i].first^.tokType=tt_literal) then begin
                  rulePatternElement.restrictionValue:=parts[i].first^.data;
                  rulePatternElement.restrictionValue^.rereference;
                  parts[i].first:=context.recycler.disposeToken(parts[i].first);
                  assertNil(parts[i].first);
                end else fail(parts[i].first);
              end;

            end else if (parts[i].first^.tokType=tt_customTypeCheck) then begin
              rulePatternElement.restrictionType:=parts[i].first^.tokType;
              rulePatternElement.customTypeCheck:=parts[i].first^.data;
              parts[i].first:=context.recycler.disposeToken(parts[i].first);

              assertNil(parts[i].first);
            end else fail(parts[i].first);
          end;
          append(rulePatternElement);
        end else begin
          //Anonymous equals: f(1)->
          context.reduceExpression(parts[i].first);
          if (parts[i].first<>nil) and (parts[i].first^.tokType=tt_literal) then begin
            rulePatternElement.createAnonymous(parts[i].first^.location);
            rulePatternElement.restrictionType:=tt_comparatorListEq;
            rulePatternElement.restrictionValue:=parts[i].first^.data;
            rulePatternElement.restrictionValue^.rereference;
            parts[i].first:=context.recycler.disposeToken(parts[i].first);
            assertNil(parts[i].first);
            append(rulePatternElement);
          end else fail(parts[i].first);
        end;
      end;
      parts:=nil;
    end;
    finalizeRefs(ruleDeclarationStart,context);
    context.recycler.disposeToken(first);
    first:=context.recycler.disposeToken(closingBracket);
  end;

{$ifdef fullVersion}
PROCEDURE T_pattern.complainAboutUnusedParameters(CONST usedIds:T_arrayOfLongint; VAR context:T_threadContext; CONST subruleLocation:T_tokenLocation);
  VAR i:longint;
      unusedIds:T_arrayOfLongint;
      allUsed:boolean=false;
      optUsed:boolean=false;

  FUNCTION warnText(CONST s:string):T_arrayOfString;
    begin
      setLength(result,2);
      result[0]:=s;
      result[1]:='You can suppress this warning with '+COMMENT_PREFIX+ATTRIBUTE_COMMENT_INFIX+SUPPRESS_UNUSED_PARAMETER_WARNING_ATTRIBUTE;
    end;

  begin
    setLength(unusedIds,length(sig));
    for i:=0 to length(unusedIds)-1 do unusedIds[i]:=i;
    for i in usedIds do begin
      dropValues(unusedIds,i);
      allUsed:=allUsed or (i=ALL_PARAMETERS_PAR_IDX);
      optUsed:=optUsed or (i=REMAINING_PARAMETERS_IDX);
    end;
    for i:=0 to length(sig)-1 do dropValues(unusedIds,sig[i].restrictionIdx);

    if allUsed then exit;
    for i in unusedIds do if sig[i].restrictionType in [tt_typeCheck,tt_customTypeCheck,tt_literal] then
      context.adapters^.raiseWarning(warnText('Parameter '+sig[i].toString+' not used'),sig[i].elementLocation);
    if hasOptionals and not(optUsed) then context.adapters^.raiseNote(warnText('Optional ... not used'),subruleLocation);
  end;
{$endif}

FUNCTION T_pattern.getFirstParameterTypeWhitelist:T_literalTypeSet;
  begin
    if length(sig)>=1 then result:=sig[0].typeWhitelist else result:=[];
  end;

end.
