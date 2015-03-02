{$ifdef include_interface}
  P_pattern=^T_pattern;
  T_pattern=object
    containsSelfPointer:boolean;
    sig:array of record
          id               :ansistring;
          restrictionType  :T_tokenType;
          restrictionScalar:P_scalarLiteral;
          restrictionIdx   :longint;
          restrictionId    :ansistring;
        end;

    CONSTRUCTOR create;
    PROCEDURE clear;
    DESTRUCTOR destroy;
    FUNCTION appendFreeId    (CONST parId:ansistring):longint;
    FUNCTION appendTypeCheck (CONST parId:ansistring; CONST typeRest:T_tokenType):longint;
    FUNCTION appendComparison(CONST parId:ansistring; CONST comparator:T_tokenType; CONST value:P_scalarLiteral):longint;
    FUNCTION appendComparison(CONST parId:ansistring; CONST comparator:T_tokenType; CONST otherId:ansistring):longint;
    FUNCTION indexOfId(CONST id:ansistring):longint;
    PROCEDURE finalizeRefs;
    FUNCTION matches(VAR par:T_listLiteral):boolean;
    FUNCTION matchesNilPattern:boolean;
    FUNCTION toString:ansistring;
    FUNCTION isEquivalent(CONST p:T_pattern):boolean;
  end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_pattern.create;
  begin clear; end;

PROCEDURE T_pattern.clear;
  begin
    setLength(sig,0);
    containsSelfPointer:=false;
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
    end;
  end;

FUNCTION T_pattern.indexOfId(CONST id:ansistring):longint;
  VAR i:longint;
  begin
    for i:=0 to length(sig)-1 do if sig[i].id=id then exit(i);
    result:=-1;
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
  
FUNCTION T_pattern.matches(VAR par:T_listLiteral):boolean;
  VAR i:longint;
      L:P_literal;
  begin

    //writeln('trying to match ',toString,' with ',par.toParameterListString(true));
    if (par.size<>length(sig)) then exit(false);
    for i:=0 to length(sig)-1 do begin
      //writeln('trying to match parameter #',i,' index=',sig[i].restrictionIdx);
      L:=par.value(i);
      with sig[i] do case restrictionType of
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
        tt_typeCheckExpression  : if not(L^.literalType in C_matchingTypes[restrictionType]) then exit(false);
        tt_typeCheckNonemptyList: if not(L^.literalType in C_matchingTypes[restrictionType]) or (P_listLiteral(L)^.size<=0) then exit(false);
        tt_typeCheckEmptyList   : if not(L^.literalType in C_matchingTypes[restrictionType]) or (P_listLiteral(L)^.size> 0) then exit(false);
        tt_comparatorEq,
        tt_comparatorNeq,
        tt_comparatorLeq,
        tt_comparatorGeq,
        tt_comparatorLss,
        tt_comparatorGrt:
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
              if not(P_scalarLiteral(L)^.isInRelationTo(restrictionType,restrictionScalar  ))
              then exit(false);
            end else begin
              L:=resolveOperator(L,tt_comparatorListEq,par.value(restrictionIdx),C_nilTokenLocation);
              if not(L^.literalType=lt_boolean) or not(P_boolLiteral(L)^.value) then begin
                disposeLiteral(L);
                exit(false);
              end else disposeLiteral(L);
            end;
          end else exit(false);
      end;
    end;
    result:=true;
  end;

FUNCTION T_pattern.toString:ansistring;
  FUNCTION trimLeadingEquals(CONST s:ansistring):ansistring;
    begin
      if (length(s)>1) and (s[1]='=') then result:=copy(s,2,length(s)-1) else result:=s;
    end;

  VAR i:longint;
  begin
    if length(sig)=0 then exit('');
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
        tt_typeCheckEmptyList   : result:=result+trimLeadingEquals(id+C_tokenString[restrictionType]);
        tt_comparatorEq,
        tt_comparatorNeq,
        tt_comparatorLeq,
        tt_comparatorGeq,
        tt_comparatorLss,
        tt_comparatorGrt,
        tt_comparatorListEq: if restrictionId='' then result:=result+id+C_tokenString[restrictionType]+restrictionScalar^.toString
                                                 else result:=result+id+C_tokenString[restrictionType]+restrictionId;
        else result:=result+id;
      end;
    end;
    result:=result+')';
  end;

FUNCTION T_pattern.isEquivalent(CONST p:T_pattern):boolean;
  VAR i:longint;
  begin
    result:=length(sig)=length(p.sig);
    for i:=0 to length(sig)-1 do
      result:=result and (sig[i].restrictionType = p.sig[i].restrictionType)
                     and (sig[i].restrictionIdx  = p.sig[i].restrictionIdx )
                     and ((sig[i].restrictionScalar =nil) and (p.sig[i].restrictionScalar =nil)
                       or (sig[i].restrictionScalar<>nil) and (p.sig[i].restrictionScalar<>nil)
                       and sig[i].restrictionScalar^.isInRelationTo(tt_comparatorEq,p.sig[i].restrictionScalar));
  end;

{$endif}
