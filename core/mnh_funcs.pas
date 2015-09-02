UNIT mnh_funcs;
INTERFACE
USES sysutils,mygenerics,mnh_constants,mnh_litvar,math,mnh_out_adapters,mnh_tokloc,mnh_fileWrappers,myStringutil,classes,process,mySys,fphttpclient,FileUtil,windows;
TYPE
  T_intFuncCallback=FUNCTION(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;

VAR
  intrinsicRuleMap           :specialize G_stringKeyMap<T_intFuncCallback>;
  intrinsicRuleExplanationMap:specialize G_stringKeyMap<ansistring>;

PROCEDURE registerRule(CONST namespace,name:ansistring; CONST ptr:T_intFuncCallback; CONST explanation:ansistring);
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation);
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST typ:T_literalType; CONST messageTail:ansistring; CONST tokenLocation:T_tokenLocation);

IMPLEMENTATION
//Critical sections:------------------------------------------------------------
VAR print_cs:system.TRTLCriticalSection;
    file_cs :system.TRTLCriticalSection;
//------------------------------------------------------------:Critical sections
PROCEDURE registerRule(CONST namespace,name:ansistring; CONST ptr:T_intFuncCallback; CONST explanation:ansistring);
  PROCEDURE registerImp(CONST regName:ansistring);
    VAR oldExplanation:ansistring;
    begin
      intrinsicRuleMap.put(regName,ptr);
      if (explanation<>'') or not(intrinsicRuleExplanationMap.containsKey(regName,oldExplanation))
                             then intrinsicRuleExplanationMap.put        (regName,explanation);
    end;

  begin
    registerImp(name);
    registerImp(namespace+C_ID_QUALIFY_CHARACTER+name);
  end;

PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation);
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function ['+functionName+'] cannot be applied to parameters ';
    if params=nil then complaintText:=complaintText+'()'
                  else complaintText:=complaintText+params^.toParameterListString(true);
    raiseError(el3_evalError,complaintText,tokenLocation);
  end;

PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST typ:T_literalType; CONST messageTail:ansistring; CONST tokenLocation:T_tokenLocation);
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function ['+functionName+'] cannot be applied to type '+C_typeString[typ]+messageTail;
    raiseError(el3_evalError,complaintText,tokenLocation);
  end;

FUNCTION clearPrint_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    system.EnterCriticalSection(print_cs);
    mnh_out_adapters.clearConsole();
    system.LeaveCriticalsection(print_cs);
    result:=newVoidLiteral;
  end;

FUNCTION print_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR stringToPrint:ansistring='';
      i:longint;
  begin
    if params<>nil then for i:=0 to params^.size-1 do case params^.value(i)^.literalType of
      lt_boolean,
      lt_int,
      lt_real,
      lt_string,
      lt_expression: stringToPrint:=stringToPrint + P_scalarLiteral(params^.value(i))^.stringForm;
      lt_list..lt_listWithError: stringToPrint:=stringToPrint + params^.value(i)^.toString;
    end;
    system.EnterCriticalSection(print_cs);
    writePrint(split(stringToPrint));
    system.LeaveCriticalsection(print_cs);
    result:=newVoidLiteral;
  end;

{$MACRO ON}
{$define SUB_LIST_IMPL:=
begin
  result:=nil;
  if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType in C_validListTypes) then begin
    if      (params^.size=1) then result:=P_listLiteral(params^.value(0))^.CALL_MACRO
    else if (params^.size=2) and (params^.value(1)^.literalType=lt_int) then result:=P_listLiteral(params^.value(0))^.CALL_MACRO(P_intLiteral(params^.value(1))^.value)
    else raiseNotApplicableError(ID_MACRO,params,tokenLocation);
  end else raiseNotApplicableError(ID_MACRO,params,tokenLocation);
end}

FUNCTION head_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=head}
{$define ID_MACRO:='head'}
SUB_LIST_IMPL;

FUNCTION tail_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=tail}
{$define ID_MACRO:='tail'}
SUB_LIST_IMPL;

FUNCTION leading_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=leading}
{$define ID_MACRO:='leading'}
SUB_LIST_IMPL;

FUNCTION trailing_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=trailing}
{$define ID_MACRO:='trailing'}
SUB_LIST_IMPL;

{$undef SUB_LIST_IMPL}

FUNCTION sort_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in C_validListTypes) then begin
      result:=P_listLiteral(params^.value(0))^.clone;
      P_listLiteral(result)^.sort;
    end else if (params<>nil) and (params^.size=2)
            and (params^.value(0)^.literalType in C_validListTypes)
            and (params^.value(1)^.literalType=lt_expression) then begin
      result:=P_listLiteral(params^.value(0))^.clone;
      P_listLiteral(result)^.customSort(P_expressionLiteral(params^.value(1)));
    end else raiseNotApplicableError('sort',params,tokenLocation);
  end;

FUNCTION sortPerm_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in C_validListTypes)
    then result:=P_listLiteral(params^.value(0))^.sortPerm
    else raiseNotApplicableError('sortPerm',params,tokenLocation);
  end;

FUNCTION unique_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (params^.value(0)^.literalType in C_validListTypes) then begin
        result:=P_listLiteral(params^.value(0))^.clone;
        P_listLiteral(result)^.unique;
      end;
    end else raiseNotApplicableError('unique',params,tokenLocation);
  end;

FUNCTION flatten_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  PROCEDURE recurse_flatten(CONST L:P_listLiteral);
    VAR i:longint;
    begin
      for i:=0 to L^.size-1 do
      if L^.value(i)^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression]
      then P_listLiteral(result)^.append(L^.value(i),true)
      else recurse_flatten(P_listLiteral(L^.value(i)));
    end;

  begin
    if params<>nil then begin
      result:=newListLiteral;
      recurse_flatten(params);
    end else begin
      result:=nil;
      raiseNotApplicableError('FLATTEN',params,tokenLocation);
    end;
  end;

FUNCTION random_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR i,count:longint;
  begin
    if (params=nil) or (params^.size=0) then exit(newRealLiteral(random))
    else if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_int) then begin
      count:=P_intLiteral(params^.value(0))^.value;
      if count>0 then begin
        result:=newListLiteral;
        for i:=1 to count do P_listLiteral(result)^.append(newRealLiteral(random),false);
        exit(result);
      end;
    end;
    result:=nil;
    raiseNotApplicableError('random',params,tokenLocation);
  end;


FUNCTION max_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR x:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then x:=params^.value(0)
    else x:=params;
    if (x<>nil) and (x^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
      result:=P_listLiteral(x)^.value(0);
      for i:=1 to P_listLiteral(x)^.size-1 do if P_scalarLiteral(P_listLiteral(x)^.value(i))^.isInRelationTo(tt_comparatorGrt,P_scalarLiteral(result)) then result:=P_listLiteral(x)^.value(i);
      result^.rereference;
    end else raiseNotApplicableError('max',params,tokenLocation);
  end;

FUNCTION argMax_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR x,xMin:P_scalarLiteral;
      L:P_listLiteral;
      i,iMin:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)<>nil) and (params^.value(0)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
      L:=P_listLiteral(params^.value(0));
      iMin:=0;
      xMin:=P_scalarLiteral(L^.value(0));
      for i:=1 to L^.size-1 do begin
        x:=P_scalarLiteral(L^.value(i));
        if x^.isInRelationTo(tt_comparatorGrt,xMin) then begin
          iMin:=i;
          xMin:=x;
        end;
      end;
      result:=newIntLiteral(iMin);
    end else raiseNotApplicableError('argMax',params,tokenLocation);
  end;

FUNCTION min_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR x:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then x:=params^.value(0)
    else x:=params;
    if (x<>nil) and (x^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
      result:=P_listLiteral(x)^.value(0);
      for i:=1 to P_listLiteral(x)^.size-1 do if P_scalarLiteral(P_listLiteral(x)^.value(i))^.isInRelationTo(tt_comparatorLss,P_scalarLiteral(result)) then result:=P_listLiteral(x)^.value(i);
      result^.rereference;
    end else raiseNotApplicableError('min',params,tokenLocation);
  end;

FUNCTION argMin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR x,xMin:P_scalarLiteral;
      L:P_listLiteral;
      i,iMin:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)<>nil) and (params^.value(0)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
      L:=P_listLiteral(params^.value(0));
      iMin:=0;
      xMin:=P_scalarLiteral(L^.value(0));
      for i:=1 to L^.size-1 do begin
        x:=P_scalarLiteral(L^.value(i));
        if x^.isInRelationTo(tt_comparatorLss,xMin) then begin
          iMin:=i;
          xMin:=x;
        end;
      end;
      result:=newIntLiteral(iMin);
    end else raiseNotApplicableError('argMin',params,tokenLocation);
  end;


FUNCTION size_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case params^.value(0)^.literalType of
        lt_error..  lt_expression: result:=newIntLiteral(1);
        lt_list..lt_listWithError: result:=newIntLiteral(P_listLiteral(params^.value(0))^.size);
      end;
    end else raiseNotApplicableError('size',params,tokenLocation);
  end;

FUNCTION length_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case params^.value(0)^.literalType of
        lt_string: result:=newIntLiteral(length(P_stringLiteral(params^.value(0))^.value));
        lt_stringList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
            P_listLiteral(result)^.append(newIntLiteral(length(P_stringLiteral(P_listLiteral(params^.value(0))^.value(i))^.value)),false);
        end;
        else raiseNotApplicableError('length',params,tokenLocation);
      end;
    end else raiseNotApplicableError('length',params,tokenLocation);
  end;

FUNCTION pos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION posInt(x,y:P_literal):P_intLiteral;
    begin
      result:=newIntLiteral(pos(P_stringLiteral(x)^.value,
                                P_stringLiteral(y)^.value)-1);
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       (params^.value(0)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(1)^.literalType in [lt_string,lt_stringList]) then begin
      if params^.value(0)^.literalType=lt_string then begin
        if params^.value(1)^.literalType=lt_string then begin
          result:=posInt(params^.value(0),
                         params^.value(1));
        end else begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(params^.value(1))^.size-1 do
            P_listLiteral(result)^.append(posInt(              params^.value(0),
                                                 P_listLiteral(params^.value(1))^.value(i)),false);
        end;
      end else begin
        if params^.value(1)^.literalType=lt_string then begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
            P_listLiteral(result)^.append(posInt(P_listLiteral(params^.value(0))^.value(i),
                                                               params^.value(1)           ),false);
        end else begin
          if P_listLiteral(params^.value(0))^.size=P_listLiteral(params^.value(1))^.size then begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
              P_listLiteral(result)^.append(posInt(P_listLiteral(params^.value(0))^.value(i),
                                                   P_listLiteral(params^.value(1))^.value(i)),false);
          end else raiseError(el3_evalError,'Incompatible list lengths for function pos.',tokenLocation)
        end;
      end;
    end else raiseNotApplicableError('pos',params,tokenLocation);
  end;

FUNCTION copy_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR anyList:boolean=false;
      allOkay:boolean=true;
      i1,i:longint;

  PROCEDURE checkLength(L:P_literal);
    VAR s:longint;
    begin
      s:=P_listLiteral(L)^.size;
      if not(anyList) then begin i1:=s; anyList:=true; end
                      else if    i1<>s then allOkay:=false;
    end;

  FUNCTION safeString(index:longint):ansistring;
    begin
      if params^.value(0)^.literalType=lt_string
        then result:=P_stringLiteral(              params^.value(0)               )^.value
        else result:=P_stringLiteral(P_listLiteral(params^.value(0))^.value(index))^.value;
    end;

  FUNCTION safeStart(index:longint):longint;
    begin
      if params^.value(1)^.literalType=lt_int
        then result:=P_intLiteral(              params^.value(1)               )^.value
        else result:=P_intLiteral(P_listLiteral(params^.value(1))^.value(index))^.value;
      inc(result);
    end;

  FUNCTION safeLen(index:longint):longint;
    begin
      if params^.value(2)^.literalType=lt_int
        then result:=P_intLiteral(              params^.value(2)               )^.value
        else result:=P_intLiteral(P_listLiteral(params^.value(2))^.value(index))^.value;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (params^.value(0)^.literalType in [lt_string,lt_stringList])
                                          and (params^.value(1)^.literalType in [lt_int   ,lt_intList])
                                          and (params^.value(2)^.literalType in [lt_int   ,lt_intList]) then begin
      anyList:=false;
      if params^.value(0)^.literalType=lt_stringList then checkLength(params^.value(0));
      if params^.value(1)^.literalType=lt_intList    then checkLength(params^.value(1));
      if params^.value(2)^.literalType=lt_intList    then checkLength(params^.value(2));
      if not(allOkay) then raiseNotApplicableError('copy',params,tokenLocation)
      else if not(anyList) then
        result:=newStringLiteral(copy(P_stringLiteral(params^.value(0))^.value,
                                      P_intLiteral   (params^.value(1))^.value+1,
                                      P_intLiteral   (params^.value(2))^.value))
      else begin
        result:=newListLiteral;
        for i:=0 to i1-1 do
          P_listLiteral(result)^.append(
            newStringLiteral(
              copy(safeString(i),
                   safeStart(i),
                   safeLen(i))),
            false);
      end;
    end else raiseNotApplicableError('copy',params,tokenLocation);
  end;

FUNCTION split_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR splitters:T_arrayOfString;
  PROCEDURE initSplitters;
    VAR i:longint;
    begin
      if params^.value(1)^.literalType=lt_string then begin
        setLength(splitters,1);
        splitters[0]:=P_stringLiteral(params^.value(1))^.value;
      end else begin
        setLength(splitters,P_listLiteral(params^.value(1))^.size);
        for i:=0 to length(splitters)-1 do
          splitters[i]:=P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value;
      end;
    end;

  FUNCTION splitOneString(CONST s:P_stringLiteral):P_listLiteral;
    PROCEDURE firstSplitterPos(CONST s:ansistring; OUT splitterStart,splitterEnd:longint);
      VAR i,p:longint;
      begin
        splitterStart:=0;
        for i:=0 to length(splitters)-1 do begin
          p:=pos(splitters[i],s);
          if (p>0) and ((splitterStart=0) or (p<splitterStart)) then begin
            splitterStart:=p;
            splitterEnd:=p+length(splitters[i]);
          end;
        end;
      end;

    VAR sp0,sp1:longint;
        rest:ansistring;
    begin
      firstSplitterPos(s^.value,sp0,sp1);
      if sp0<0 then exit(newOneElementListLiteral(s,true));
      result:=newListLiteral;
      rest:=s^.value;
      while sp0>0 do begin
        result^.append(newStringLiteral(copy(rest,1,sp0-1)),false);
        rest:=copy(rest,sp1,length(rest));
        firstSplitterPos(rest,sp0,sp1);
      end;
      result^.append(newStringLiteral(rest),false);
    end;

  FUNCTION splitRecurse(CONST p:P_literal):P_Literal;
    VAR i:longint;
    begin
      case p^.literalType of
        lt_string: result:=splitOneString(P_stringLiteral(p));
        lt_list,lt_stringList,lt_emptyList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(p)^.size-1 do if errorLevel<el3_evalError then
            P_listLiteral(result)^.append(splitRecurse(P_listLiteral(p)^.value(i)),false);
        end
       else result:=newErrorLiteralRaising('Cannot split non-string varables ',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2)
      and (params^.value(0)^.literalType in [lt_string,lt_stringList,lt_list,lt_emptyList])
      and (params^.value(1)^.literalType in [lt_string,lt_stringList,lt_emptyList]) then begin
      initSplitters;
      result:=splitRecurse(params^.value(0));
    end else raiseNotApplicableError('split',params,tokenLocation);
  end;

FUNCTION softCast_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION softCastRecurse(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.softCast;
        lt_list..lt_listWithError: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do
            P_listLiteral(result)^.append(softCastRecurse(P_listLiteral(x)^.value(i)),false);
        end;
        else begin
          x^.rereference;
          result:=x;
        end;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=softCastRecurse(params^.value(0))
    else raiseNotApplicableError('softcast',params,tokenLocation);
  end;

{$define STRINGLITERAL_ROUTINE:=
FUNCTION recurse(CONST x:P_literal):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    case x^.literalType of
      lt_string: result:=P_stringLiteral(x)^.CALL_MACRO;
      lt_list,lt_stringList,lt_emptyList:  begin
        result:=newListLiteral;
        for i:=0 to P_listLiteral(x)^.size-1 do if errorLevel<el3_evalError then
          P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false);
      end;
      else result:=newErrorLiteralRaising('Cannot apply '+ID_MACRO+' to literal of type '+C_typeString[x^.literalType],tokenLocation);
    end;
  end;

begin
  result:=nil;
  if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_string,lt_emptyList])
  then result:=recurse(params^.value(0))
  else raiseNotApplicableError(ID_MACRO,params,tokenLocation);
end}


FUNCTION trim_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=trim}
{$define ID_MACRO:='trim'}
STRINGLITERAL_ROUTINE;

FUNCTION trimLeft_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=trimLeft}
{$define ID_MACRO:='trimLeft'}
STRINGLITERAL_ROUTINE;

FUNCTION trimRight_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=trimRight}
{$define ID_MACRO:='trimRight'}
STRINGLITERAL_ROUTINE;

FUNCTION upper_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=upper}
{$define ID_MACRO:='upper'}
STRINGLITERAL_ROUTINE;

FUNCTION lower_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=lower}
{$define ID_MACRO:='lower'}
STRINGLITERAL_ROUTINE;

FUNCTION unbrace_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=unbrace}
{$define ID_MACRO:='unbrace'}
STRINGLITERAL_ROUTINE;

FUNCTION escape_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define CALL_MACRO:=escape}
{$define ID_MACRO:='escape'}
STRINGLITERAL_ROUTINE;
{$undef STRINGLITERAL_ROUTINE}


FUNCTION string_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if params^.value(0)^.literalType=lt_string then begin
        result:=params^.value(0);
        result^.rereference;
      end else result:=newStringLiteral(params^.value(0)^.toString);
    end else raiseNotApplicableError('string',params,tokenLocation);
  end;

FUNCTION filesOrDirs_impl(CONST pathOrPathList:P_literal; CONST filesAndNotFolders:boolean):P_listLiteral;
  VAR i,j:longint;
      found:T_arrayOfString;
  begin
    result:=newListLiteral;
    if pathOrPathList^.literalType=lt_string then begin
      found:=find(P_stringLiteral(pathOrPathList)^.value,filesAndNotFolders);
      for i:=0 to length(found)-1 do result^.append(newStringLiteral(UTF8Encode(found[i])),false);
    end else if pathOrPathList^.literalType=lt_stringList then begin
      for j:=0 to P_listLiteral(pathOrPathList)^.size-1 do begin
        found:=find(P_stringLiteral(P_listLiteral(pathOrPathList)^.value(j))^.value,filesAndNotFolders);
        for i:=0 to length(found)-1 do result^.append(newStringLiteral(UTF8Encode(found[i])),false);
      end;
    end;
  end;

FUNCTION files_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_string, lt_stringList]) then begin
      result:=filesOrDirs_impl(params^.value(0),true);
    end else raiseNotApplicableError('files',params,tokenLocation);
  end;

FUNCTION folders_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_string, lt_stringList]) then begin
      result:=filesOrDirs_impl(params^.value(0),false);
    end else raiseNotApplicableError('folders',params,tokenLocation);
  end;

FUNCTION fileExists_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newBoolLiteral(FileExists(UTF8Decode(P_stringLiteral(params^.value(0))^.value)));
    end else raiseNotApplicableError('fileExists',params,tokenLocation);
  end;

FUNCTION fileContents_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR accessed:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      system.EnterCriticalSection(file_cs);
      result:=newStringLiteral(fileContent(P_stringLiteral(params^.value(0))^.value,accessed));
      system.LeaveCriticalsection(file_cs);
      if not(accessed) then raiseError(el2_warning,'File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('fileContents',params,tokenLocation);
  end;

FUNCTION fileLines_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR accessed:boolean;
      L:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      system.EnterCriticalSection(file_cs);
      L:=fileLines(P_stringLiteral(params^.value(0))^.value,accessed);
      system.LeaveCriticalsection(file_cs);
      result:=newListLiteral;
      for i:=0 to length(L)-1 do P_listLiteral(result)^.append(newStringLiteral(L[i]),false);
      if not(accessed) then raiseError(el2_warning,'File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else if (params<>nil) and (params^.size=3) and
                (params^.value(0)^.literalType=lt_string) and
                (params^.value(1)^.literalType=lt_int) and
                (params^.value(2)^.literalType=lt_int) then begin
      system.EnterCriticalSection(file_cs);
      L:=fileLines(P_stringLiteral(params^.value(0))^.value,
                   P_intLiteral   (params^.value(1))^.value,
                   P_intLiteral   (params^.value(2))^.value,accessed);
      system.LeaveCriticalsection(file_cs);
      result:=newListLiteral;
      for i:=0 to length(L)-1 do P_listLiteral(result)^.append(newStringLiteral(L[i]),false);
      if not(accessed) then raiseError(el2_warning,'File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('fileLines',params,tokenLocation);
  end;

FUNCTION writeFile_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR ok:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string)
                                          and (params^.value(1)^.literalType=lt_string) then begin
      system.EnterCriticalSection(file_cs);
      ok:=mnh_fileWrappers.writeFile(P_stringLiteral(params^.value(0))^.value,
                                     P_stringLiteral(params^.value(1))^.value);
      system.LeaveCriticalsection(file_cs);
      result:=newBoolLiteral(ok);
      if not(ok) then raiseError(el2_warning,'File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('writeFile',params,tokenLocation);
  end;

FUNCTION writeFileLines_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR ok:boolean;
      L:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string)
                                          and (params^.value(1)^.literalType in [lt_stringList,lt_emptyList]) then begin
      setLength(L,P_listLiteral(params^.value(1))^.size);
      for i:=0 to length(L)-1 do L[i]:=P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value;
      system.EnterCriticalSection(file_cs);
      ok:=writeFileLines(P_stringLiteral(params^.value(0))^.value,L);
      system.LeaveCriticalsection(file_cs);
      result:=newBoolLiteral(ok);
      if not(ok) then raiseError(el2_warning,'File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('writeFileLines',params,tokenLocation);
  end;

FUNCTION replace_one_or_all(CONST params:P_listLiteral; CONST all:boolean):P_literal;
  VAR lookFor,replaceBy:T_arrayOfString;
      i:longint;

  PROCEDURE initArrays;
    VAR L:P_literal;
        i:longint;
    PROCEDURE elongate(VAR list:T_arrayOfString);
      begin
        if length(list)=0 then begin
          setLength(list,1);
          list[0]:='';
        end else begin
          setLength(list,length(list)+1);
          list[length(list)-1]:=list[length(list)-2];
        end;
      end;

    begin
      L:=params^.value(1);
      if L^.literalType=lt_string then begin
        setLength(lookFor,1);
        lookFor[0]:=P_stringLiteral(L)^.value;
      end else begin
        setLength(lookFor,P_listLiteral(L)^.size);
        for i:=0 to length(lookFor)-1 do
          lookFor[i]:=P_stringLiteral(P_listLiteral(L)^.value(i))^.value;
      end;
      L:=params^.value(2);
      if L^.literalType=lt_string then begin
        setLength(replaceBy,1);
        replaceBy[0]:=P_stringLiteral(L)^.value;
      end else begin
        setLength(replaceBy,P_listLiteral(L)^.size);
        for i:=0 to length(replaceBy)-1 do
          replaceBy[i]:=P_stringLiteral(P_listLiteral(L)^.value(i))^.value;
      end;
      while length(replaceBy)<length(lookFor) do elongate(replaceBy);
      while length(lookFor)<length(replaceBy) do elongate(lookFor);
    end;

  FUNCTION modify(CONST original:ansistring):ansistring;
    VAR i:longint;
        dummy:boolean;
    begin
      result:=original;
      if all then for i:=0 to length(lookFor)-1 do result:=replaceRecursively(result,lookFor[i],replaceBy[i],dummy)
             else for i:=0 to length(lookFor)-1 do result:=replaceOne        (result,lookFor[i],replaceBy[i]);
    end;

  begin
    initArrays;
    if params^.value(0)^.literalType=lt_string then result:=newStringLiteral(modify(P_stringLiteral(params^.value(0))^.value))
    else begin
      result:=newListLiteral;
      for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
        P_listLiteral(result)^.append(
          newStringLiteral(modify(P_stringLiteral(P_listLiteral(params^.value(0))^.value(i))^.value)),
          false
        );
    end;
    setLength(lookFor,0);
    setLength(replaceBy,0);
  end;

FUNCTION replaceOne_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (params^.value(0)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(1)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(2)^.literalType in [lt_string,lt_stringList]) then begin
      result:=replace_one_or_all(params,false);
    end else raiseNotApplicableError('replaceOne',params,tokenLocation);
  end;

FUNCTION replace_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (params^.value(0)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(1)^.literalType in [lt_string,lt_stringList]) and
       (params^.value(2)^.literalType in [lt_string,lt_stringList]) then begin
      result:=replace_one_or_all(params,true);
    end else raiseNotApplicableError('replace',params,tokenLocation);
  end;

FUNCTION repeat_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR sub,res:ansistring;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
       (params^.value(0)^.literalType = lt_string) and
       (params^.value(1)^.literalType = lt_int) then begin
      res:='';
      sub:=P_stringLiteral(params^.value(0))^.value;
      for i:=1 to P_intLiteral(params^.value(1))^.value do res:=res+sub;
      result:=newStringLiteral(res);
    end else raiseNotApplicableError('repeat',params,tokenLocation);
  end;

FUNCTION execSync_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION runCommand(CONST executable: ansistring; CONST parameters: T_arrayOfString; OUT output: TStringList): boolean;
    CONST
      READ_BYTES = 2048;
    VAR
      memStream: TMemoryStream;
      tempProcess: TProcess;
      n: longint;
      BytesRead: longint;
    begin
      memStream := TMemoryStream.create;
      BytesRead := 0;
      tempProcess := TProcess.create(nil);
      tempProcess.Executable := executable;
      for n := 0 to length(parameters)-1 do
        tempProcess.Parameters.Add(parameters [n]);
      tempProcess.Options := [poUsePipes, poStderrToOutPut];
      tempProcess.ShowWindow := swoHIDE;
      try
        tempProcess.Execute;
        tempProcess.CloseInput;
        while tempProcess.Running and (errorLevel<el3_evalError) do begin
          memStream.SetSize(BytesRead+READ_BYTES);
          n := tempProcess.Output.Read((memStream.Memory+BytesRead)^, READ_BYTES);
          if n>0 then Inc(BytesRead, n)
                 else Sleep(10);
        end;
        if tempProcess.Running then tempProcess.Terminate(999);
        repeat
          memStream.SetSize(BytesRead+READ_BYTES);
          n := tempProcess.Output.Read((memStream.Memory+BytesRead)^, READ_BYTES);
          if n>0 then Inc(BytesRead, n);
        until n<=0;
        result := (tempProcess.ExitStatus = 0);
      except
        result := false;
      end;
      tempProcess.Free;
      memStream.SetSize(BytesRead);
      output := TStringList.create;
      output.LoadFromStream(memStream);
      memStream.Free;
    end;

  VAR executable:ansistring;
      cmdLinePar:T_arrayOfString;
      output:TStringList;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string)
      and ((params^.size=1) or (params^.size=2) and (params^.value(1)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_flatList])) then begin
      setLength(cmdLinePar,0);
      executable:=P_stringLiteral(params^.value(0))^.value;
      if params^.size=2 then begin
        setLength(cmdLinePar,P_listLiteral(params^.value(1))^.size);
        for i:=0 to P_listLiteral(params^.value(1))^.size-1 do
          cmdLinePar[i]:=P_scalarLiteral(P_listLiteral(params^.value(1))^.value(i))^.stringForm;
      end;
      runCommand(executable,
                 cmdLinePar,
                 output);
      result:=newListLiteral;
      for i:=0 to output.Count-1 do P_listLiteral(result)^.append(newStringLiteral(output[i]),false);
      output.Free;
    end else raiseNotApplicableError('exec',params,tokenLocation);
  end;

FUNCTION execAsync_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR executable:ansistring;
      cmdLinePar:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string)
      and ((params^.size=1) or (params^.size=2) and (params^.value(1)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_flatList])) then begin
      setLength(cmdLinePar,0);
      executable:=P_stringLiteral(params^.value(0))^.value;
      if params^.size=2 then begin
        setLength(cmdLinePar,P_listLiteral(params^.value(1))^.size);
        for i:=0 to P_listLiteral(params^.value(1))^.size-1 do
          cmdLinePar[i]:=P_scalarLiteral(P_listLiteral(params^.value(1))^.value(i))^.stringForm;
      end;
      runCommandAsync(executable,
                      cmdLinePar);
      result:=newBoolLiteral(true);
    end else raiseNotApplicableError('execAsync',params,tokenLocation);
  end;

FUNCTION tokenSplit_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR stringToSplit:ansistring;
      i0,i1:longint;

  PROCEDURE stepToken;
    begin
      P_listLiteral(result)^.append(newStringLiteral(copy(stringToSplit,i0,i1-i0)),false);
      i0:=i1;
    end;

  VAR doubleQuoteString:boolean=false;
      singleQuoteString:boolean=false;
      escapeStringDelimiter:boolean=false;
      curlyBracketsDelimitOneToken:boolean=false;
      cStyleComments:boolean=false;
      dollarVariables:boolean=false;
      t:T_tokenType;

  PROCEDURE setLanguage(name:string);
    begin
      if trim(UpperCase(name))='MNH' then begin
        doubleQuoteString:=true;
        singleQuoteString:=true;
        escapeStringDelimiter:=true;
        curlyBracketsDelimitOneToken:=true;
        cStyleComments:=true;
        dollarVariables:=true;
      end else if trim(UpperCase(name))='JAVA' then begin
        doubleQuoteString:=true;
        singleQuoteString:=true;
        escapeStringDelimiter:=true;
        curlyBracketsDelimitOneToken:=false;
        cStyleComments:=true;
      end else if trim(UpperCase(name))='PASCAL' then begin
        doubleQuoteString:=false;
        singleQuoteString:=true;
        escapeStringDelimiter:=false;
        curlyBracketsDelimitOneToken:=true;
        cStyleComments:=true;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then begin
      if (params^.value(1)^.literalType=lt_string)
      then setLanguage(P_stringLiteral(params^.value(1))^.value)
      else begin
        raiseNotApplicableError('tokenSplit',params,tokenLocation);
        exit(nil);
      end;
    end;

    if (params<>nil) and (params^.size>=1) and
       (params^.value(0)^.literalType=lt_string) then begin
      stringToSplit:=P_stringLiteral(params^.value(0))^.value;

      result:=newListLiteral;
      i0:=1;
      while i0<=length(stringToSplit) do begin
        if stringToSplit[i0] in [' ',C_lineBreakChar,C_carriageReturnChar,C_tabChar] then begin //whitespace
          i1:=i0;
          while (i1<=length(stringToSplit)) and (stringToSplit[i1] in [' ',C_lineBreakChar,C_carriageReturnChar,C_tabChar]) do inc(i1);
        end else if (stringToSplit[i0]='''') and singleQuoteString or
                    (stringToSplit[i0]='"') and doubleQuoteString then begin
          if escapeStringDelimiter then begin
            unescapeString(copy(stringToSplit,i0,length(stringToSplit)-i0+1),i1);
            if i1<=0 then i1:=i0+1
                     else i1:=i0+i1;
          end else begin
            i1:=i0+1;
            while (i1<=length(stringToSplit)) and (stringToSplit[i1]<>stringToSplit[i0]) do inc(i1);
            inc(i1);
          end;
        end else if (stringToSplit[i0]='{') and curlyBracketsDelimitOneToken then begin
          i1:=i0+1;
          while (i1<=length(stringToSplit)) and (stringToSplit[i1]<>'}') do inc(i1);
          inc(i1);
        end else if (copy(stringToSplit,i0,2)='//') and cStyleComments then begin
          i1:=i0+1;
          while (i1<=length(stringToSplit)) and not(stringToSplit[i1] in [C_lineBreakChar,C_carriageReturnChar]) do inc(i1);
        end else if (stringToSplit[i0] in ['a'..'z','A'..'Z']) or (stringToSplit[i0]='$') and dollarVariables then begin
          i1:=i0+1;
          while (i1<=length(stringToSplit)) and (stringToSplit[i1] in ['a'..'z','A'..'Z','_','0'..'9']) do inc(i1);
        end else if stringToSplit[i0] in ['0'..'9'] then begin //numbers
          parseNumber(copy(stringToSplit,i0,length(stringToSplit)-i0+1),false,i1);
          if i1<=0 then i1:=i0
                   else i1:=i0+i1;
        end else begin
          //symbols, etc.
          t:=tt_literal;
          i1:=i0;
          for t:=tt_literal to tt_eol do begin
            if (C_tokenString[t]<>'') and
               (copy(stringToSplit,i0,length(C_tokenString[t]))=C_tokenString[t]) and
               (i0+length(C_tokenString[t])>i1)
            then i1:=i0+length(C_tokenString[t]);
          end;
          if i1=i0 then i1:=i0+1;
        end;
        stepToken;
      end;

    end else raiseNotApplicableError('tokenSplit',params,tokenLocation);
  end;

FUNCTION myPath_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    if (tokenLocation.filename='?') or
       (tokenLocation.filename='') then result:=newStringLiteral('<Unknown>')
                                   else result:=newStringLiteral(tokenLocation.filename);
  end;

FUNCTION executor_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=newStringLiteral(ParamStr(0));
  end;

FUNCTION trueCount_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR B:P_literal;
      i,c:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      B:=params^.value(0);
      case B^.literalType of
        lt_boolean: if P_boolLiteral(B)^.value then exit(newIntLiteral(1)) else exit(newIntLiteral(0));
        lt_booleanList: begin
          c:=0;
          for i:=0 to P_listLiteral(B)^.size-1 do if P_boolLiteral(P_listLiteral(B)^.value(i))^.value then inc(c);
          exit(newIntLiteral(c));
        end;
        lt_list, lt_flatList: if P_listLiteral(B)^.size=0 then exit(newIntLiteral(0));
      end;
    end;
    raiseNotApplicableError('trueCount',params,tokenLocation);
  end;

FUNCTION isNan_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and
       (params^.value(0)^.literalType in [lt_real,lt_int,lt_realList,lt_intList,lt_numList,lt_emptyList]) then begin
       case params^.value(0)^.literalType of
         lt_real: exit(newBoolLiteral(IsNan(P_realLiteral(params^.value(0))^.value)));
         lt_int:  exit(newBoolLiteral(false));
         lt_intList: begin
           result:=newListLiteral;
           for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
             P_listLiteral(result)^.append(newBoolLiteral(false),false);
         end;
         else begin
           result:=newListLiteral;
           for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
             P_listLiteral(result)^.append(newBoolLiteral((P_listLiteral(params^.value(0))^.literalType=lt_real) and IsNan(P_realLiteral(P_listLiteral(params^.value(0)))^.value)),false);
         end;
       end;
    end else raiseNotApplicableError('isNan',params,tokenLocation);
  end;

FUNCTION isInfinite_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and
       (params^.value(0)^.literalType in [lt_real,lt_int,lt_realList,lt_intList,lt_numList,lt_emptyList]) then begin
      case params^.value(0)^.literalType of
        lt_real: exit(newBoolLiteral(IsInfinite(P_realLiteral(params^.value(0))^.value)));
        lt_int:  exit(newBoolLiteral(false));
        lt_intList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
            P_listLiteral(result)^.append(newBoolLiteral(false),false);
        end;
        else begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
            P_listLiteral(result)^.append(newBoolLiteral((P_listLiteral(params^.value(0))^.literalType=lt_real) and IsInfinite(P_realLiteral(P_listLiteral(params^.value(0)))^.value)),false);
        end;
      end;
    end else raiseNotApplicableError('isInfinite',params,tokenLocation);
  end;

FUNCTION isInRange_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR r0,r1:T_myFloat;
  FUNCTION inRange(CONST L:P_literal):boolean; inline;
    VAR i:int64;
        r:T_myFloat;
    begin
      if l^.literalType=lt_real then begin
        r:=P_realLiteral(l)^.value;
        result:=not(IsNan(r)) and not(IsInfinite(r)) and (r0<=r) and (r<=r1);
      end else begin
        i:=P_intLiteral(l)^.value;
        result:=(r0<=i) and (i<=r1);
      end;
    end;

  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (params^.value(0)^.literalType in [lt_real,lt_int,lt_realList,lt_intList,lt_numList,lt_emptyList]) and
       (params^.value(1)^.literalType in [lt_real,lt_int]) and
       (params^.value(2)^.literalType in [lt_real,lt_int]) then begin
      if params^.value(1)^.literalType=lt_real
        then r0:=P_realLiteral(params^.value(1))^.value
        else r0:=P_intLiteral (params^.value(1))^.value;
      if params^.value(2)^.literalType=lt_real
        then r1:=P_realLiteral(params^.value(2))^.value
        else r1:=P_intLiteral (params^.value(2))^.value;
      if params^.value(0)^.literalType in [lt_real,lt_int] then exit(newBoolLiteral(inRange(params^.value(0))))
      else begin
        result:=newListLiteral;
        for i:=0 to P_listLiteral(params^.value(0))^.size-1 do
          P_listLiteral(result)^.append(newBoolLiteral(inRange(P_listLiteral(params^.value(0))^.value(i))),false);
      end;
    end else raiseNotApplicableError('isInRange',params,tokenLocation);
  end;

FUNCTION splitFileName_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  PROCEDURE appendPair(VAR result:P_literal; CONST el0:string; CONST el1:string);
    VAR aid:P_listLiteral;
    begin
      aid:=newListLiteral;
      aid^.append(newStringLiteral(el0),false);
      aid^.append(newStringLiteral(el1),false);
      P_listLiteral(result)^.append(aid,false);
    end;

  VAR name:string;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newListLiteral;
      name:=P_stringLiteral(params^.value(0))^.value;
      appendPair(result,'input',name);
      appendPair(result,'expanded',ExpandFileName(name));
      appendPair(result,'relative',ExtractRelativepath(ExpandFileName(''),name));
      appendPair(result,'directory',ExtractFileDir(name));
      appendPair(result,'filename',ExtractFileName(name));
      appendPair(result,'extension',ExtractFileExt(name));
    end else raiseNotApplicableError('splitFileName',params,tokenLocation);
  end;

FUNCTION systime_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then exit(newRealLiteral(now))
    else raiseNotApplicableError('systime',params,tokenLocation);
  end;

FUNCTION ord_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION recurse(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_boolean: if P_boolLiteral(x)^.value
                    then exit(newIntLiteral(1))
                    else exit(newIntLiteral(0));
        lt_int: begin x^.rereference; exit(x); end;
        lt_string : if length(P_stringLiteral(x)^.value)=1
                    then exit(newIntLiteral(ord(P_stringLiteral(x)^.value[1])))
                    else exit(newIntLiteral(-1));
        lt_error,lt_void, lt_real,lt_expression: exit(newErrorLiteralRaising('ord can only be applied to booleans, ints and strings',tokenLocation));
        else begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if errorLevel<el3_evalError then
            P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false);
        end;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=recurse(params^.value(0))
    else raiseNotApplicableError('ord',params,tokenLocation);
  end;

FUNCTION format_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
{$define INNER_FORMATTING:=
  VAR resultString:T_arrayOfString;
      resultIsList:boolean=false;
      literalIterator:longint=1;

  FUNCTION nextLiteral:P_literal;
    begin
      if literalIterator<params^.size then begin
        result:=params^.value(literalIterator);
        inc(literalIterator);
      end else result:=nil;
    end;

  PROCEDURE decomposeFormatString(CONST s:ansistring);
    VAR i0,i1,j:longint;
        L,X:P_literal;
        fmtString:ansistring;
    PROCEDURE appendToAll(CONST suffix:ansistring);
      VAR k:longint;
      begin
        for k:=0 to length(resultString)-1 do resultString[k]:=resultString[k]+suffix;
      end;

    PROCEDURE appendTo(CONST index:longint; CONST suffix:ansistring);
      VAR k:longint;
      begin
        resultIsList:=true;
        if index>=length(resultString) then begin
          k:=length(resultString);
          setLength(resultString,index+1);
          while k<length(resultString) do begin
            resultString[k]:=resultString[k-1];
            inc(k);
          end;
        end;
        resultString[index]:=resultString[index]+suffix;
      end;

    begin
      setLength(resultString,1);
      if pos('%',s)<=0 then begin
        resultString[0]:=s;
        exit;
      end else resultString[0]:='';
      i0:=1;
      while i0<=length(s) do begin
        i1:=i0+1; while (i1<=length(s)) and (s[i1]<>'%') do inc(i1);
        if s[i0]='%' then begin
          if (i1<=length(s)) and (i1=i0+1) then appendToAll('%')
          else begin
            fmtString:=copy(s,i0+1,i1-i0-1);
            L:=nextLiteral;
            if L=nil then appendToAll('%'+fmtString+'%') else
            case L^.literalType of
              lt_int   : appendToAll(myFormat(fmtString,P_intLiteral   (L)^.value));
              lt_real  : appendToAll(myFormat(fmtString,P_realLiteral  (L)^.value));
              lt_string: appendToAll(myFormat(fmtString,P_stringLiteral(L)^.value));
              lt_list..lt_flatList: begin
                for j:=P_listLiteral(L)^.size-1 downto 0 do begin
                  X:=P_listLiteral(L)^.value(j);
                  case X^.literalType of
                    lt_int   : appendTo(j,myFormat(fmtString,P_intLiteral   (X)^.value));
                    lt_real  : appendTo(j,myFormat(fmtString,P_realLiteral  (X)^.value));
                    lt_string: appendTo(j,myFormat(fmtString,P_stringLiteral(X)^.value));
                    else       appendTo(j,myFormat(fmtString,              X^.toString));
                  end;
                end;
                for j:=P_listLiteral(L)^.size to length(resultString)-1 do appendTo(j,'%'+fmtString+'%');
              end;
              else appendToAll(myFormat(fmtString,L^.toString));
            end;
          end;
          i0:=i1+1;
        end else begin
          appendToAll(copy(s,i0,i1-i0));
          i0:=i1;
        end;
      end;
    end}
  INNER_FORMATTING;
  VAR k:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string) then begin
      decomposeFormatString(P_stringLiteral(params^.value(0))^.value);
      if resultIsList or (length(resultString)<>1) then begin
        result:=newListLiteral;
        for k:=0 to length(resultString)-1 do P_listLiteral(result)^.append(newStringLiteral(resultString[k]),false);
      end else result:=newStringLiteral(join(formatTabs(split(resultString[0])),C_lineBreakChar));
    end else raiseNotApplicableError('format',params,tokenLocation);
  end;

FUNCTION printf_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  INNER_FORMATTING;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string) then begin
      decomposeFormatString(P_stringLiteral(params^.value(0))^.value);
      system.EnterCriticalSection(print_cs);
      writePrint(reSplit(resultString));
      system.LeaveCriticalsection(print_cs);
      result:=newVoidLiteral;
    end else raiseNotApplicableError('printf',params,tokenLocation);
  end;
{$undef INNER_FORMATTING}

FUNCTION deleteFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newBoolLiteral(DeleteFileUTF8(P_stringLiteral(params^.value(0))^.value));
    end else raiseNotApplicableError('deleteFile',params,tokenLocation);
  end;

FUNCTION deleteDir_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newBoolLiteral(DeleteDirectory(P_stringLiteral(params^.value(0))^.value,false));
    end else raiseNotApplicableError('deleteDir',params,tokenLocation);
  end;

FUNCTION copyFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) and (params^.value(1)^.literalType=lt_string)  then begin
      ensurePath(P_stringLiteral(params^.value(1))^.value);
      result:=newBoolLiteral(
      FileUtil.CopyFile(P_stringLiteral(params^.value(0))^.value,
                        P_stringLiteral(params^.value(1))^.value,true));
    end else raiseNotApplicableError('copyFile',params,tokenLocation);
  end;

FUNCTION moveFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) and (params^.value(1)^.literalType=lt_string)  then begin
      ensurePath(P_stringLiteral(params^.value(1))^.value);
      result:=newBoolLiteral(
      FileUtil.RenameFileUTF8(P_stringLiteral(params^.value(0))^.value,
                              P_stringLiteral(params^.value(1))^.value));
    end else raiseNotApplicableError('moveFile',params,tokenLocation);
  end;

FUNCTION fileInfo_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR time:double;
      size:int64;
      isExistent,
      isArchive,
      isDirectory,
      isReadOnly,
      isSystem,
      isHidden:boolean;
      //-------------------------
      resultAsList:P_listLiteral;
      attributeList:P_listLiteral;

  PROCEDURE appendKeyValuePair(CONST key:string; CONST value:P_literal);
    VAR subList:P_listLiteral;
    begin
      subList:=newListLiteral;
      subList^.append(newStringLiteral(key),false);
      subList^.append(value,false);
      resultAsList^.append(subList,false);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      getFileInfo(P_stringLiteral(params^.value(0))^.value,time,size,isExistent,isArchive,isDirectory,isReadOnly,isSystem,isHidden);
      resultAsList:=newListLiteral;
      appendKeyValuePair('exists',newBoolLiteral(isExistent));
      if isExistent then begin
        if size>=0 then appendKeyValuePair('size',newIntLiteral(size));
        if time<>-1 then appendKeyValuePair('time',newRealLiteral(time));
        attributeList:=newListLiteral;
        if isArchive   then attributeList^.append(newStringLiteral('archive'  ),false);
        if isDirectory then attributeList^.append(newStringLiteral('directory'),false);
        if isReadOnly  then attributeList^.append(newStringLiteral('readonly' ),false);
        if isSystem    then attributeList^.append(newStringLiteral('system'   ),false);
        if isHidden    then attributeList^.append(newStringLiteral('hidden'   ),false);
        appendKeyValuePair('attributes',attributeList);
      end;
      result:=resultAsList;
    end else raiseNotApplicableError('fileInfo',params,tokenLocation);
  end;

FUNCTION httpGet_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR resultText:ansistring;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      try
        resultText:=TFPCustomHTTPClient.SimpleGet(P_stringLiteral(params^.value(0))^.value);
      except
        On E : Exception do begin
          resultText:='';
          raiseError(el5_systemError,'httpGet failed with:'+E.Message,tokenLocation);
        end;
      end;
      result:=newStringLiteral(resultText);
    end else raiseNotApplicableError('httpGet',params,tokenLocation);
  end;

FUNCTION setErrorlevel_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_int) then begin
      mnh_out_adapters.systemErrorlevel.value:=P_intLiteral(params^.value(0))^.value;
      result:=newVoidLiteral;
    end else raiseNotApplicableError('setErrorlevel',params,tokenLocation);
  end;

FUNCTION hash_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=newIntLiteral(params^.value(0)^.hash)
    else raiseNotApplicableError('hash',params,tokenLocation);
  end;

FUNCTION listBuiltin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR keys:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      keys:=intrinsicRuleExplanationMap.keySet;
      result:=newListLiteral;
      for i:=0 to length(keys)-1 do P_listLiteral(result)^.append(newStringLiteral(keys[i]),false);
      setLength(keys,0);
    end else raiseNotApplicableError('listBuiltin',params,tokenLocation);
  end;

FUNCTION driveInfo_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION infoForLetter(CONST drive:char):P_literal;
    VAR DriveLetter: string;
        driveType:longint;
        NotUsed:     DWORD;
        VolumeFlags: DWORD;
        VolumeInfo:  array[0..MAX_PATH] of Char;
        VolumeSerialNumber: DWORD;
        Buf: array [0..MAX_PATH] of Char;
        infoPair:P_listLiteral;
    begin
      DriveLetter := Drive + ':\';
      driveType:=GetDriveType(PChar(DriveLetter));
      if driveType in [DRIVE_REMOVABLE,DRIVE_FIXED,DRIVE_REMOTE,DRIVE_CDROM,DRIVE_RAMDISK] then begin
        result:=newListLiteral;
      end else exit(newVoidLiteral);

      infoPair:=newListLiteral;
      infoPair^.append(newStringLiteral('drive'),false);
      infoPair^.append(newStringLiteral( drive ),false);
      P_listLiteral(result)^.append(infoPair,false);

      infoPair:=newListLiteral;
      infoPair^.append(newStringLiteral('type'),false);
      case driveType of
        DRIVE_REMOVABLE: infoPair^.append(newStringLiteral('removable'),false);
        DRIVE_FIXED:     infoPair^.append(newStringLiteral('fixed'    ),false);
        DRIVE_REMOTE:    infoPair^.append(newStringLiteral('network'  ),false);
        DRIVE_CDROM:     infoPair^.append(newStringLiteral('CD_ROM'   ),false);
        DRIVE_RAMDISK:   infoPair^.append(newStringLiteral('RAM_disk' ),false);
      end;
      P_listLiteral(result)^.append(infoPair,false);

      GetVolumeInformation(PChar(DriveLetter),
        Buf, SizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
        VolumeFlags, nil, 0);
      SetString(DriveLetter, Buf, StrLen(Buf));

      infoPair:=newListLiteral;
      infoPair^.append(newStringLiteral('serial'),false);
      infoPair^.append(newIntLiteral(VolumeSerialNumber),false);
      P_listLiteral(result)^.append(infoPair,false);

      infoPair:=newListLiteral;
      infoPair^.append(newStringLiteral('label'),false);
      infoPair^.append(newStringLiteral(DriveLetter),false);
      P_listLiteral(result)^.append(infoPair,false);
    end;

  VAR c:char;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      result:=newListLiteral;
      for c:='A' to 'Z' do P_listLiteral(result)^.append(infoForLetter(c),false);
    end else raiseNotApplicableError('driveInfo',params,tokenLocation);
  end;

INITIALIZATION
  //Critical sections:------------------------------------------------------------
  system.InitCriticalSection(print_cs);
  system.InitCriticalSection(file_cs);
  //------------------------------------------------------------:Critical sections
  intrinsicRuleMap.create;
  intrinsicRuleExplanationMap.create;

  registerRule(SYSTEM_BUILTIN_NAMESPACE,'clearPrint'    ,@clearPrint_imp,'clearPrint(...);#Clears the output and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'print'         ,@print_imp     ,'print(...);#Prints out the given parameters and returns void#if tabs and line breaks are part of the output, a default pretty-printing is used');
  //Functions on lists:
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'head'          ,@head_imp      ,'head(L);#Returns the first element of list L or [] if L is empty#head(L,k);#Returns the first min(k,size(L)) elements of L or [] if L is empty');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'tail'          ,@tail_imp      ,'tail(L);#Returns list L without the first element#tail(L,k);#Returns L without the first k elements');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'leading'       ,@leading_imp   ,'leading(L);#Returns L without the last element or [] if L is empty#leading(L,k);#Returns L without the last k elements or [] if L is empty');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'trailing'      ,@trailing_imp  ,'trailing(L);#Returns the last element of L#trailing(L,k);#Returns the last k elements of L');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sort'          ,@sort_imp      ,'sort(L);#Returns list L sorted ascending (using fallbacks for uncomparable types)#sort(L,leqExpression:expression);#Returns L sorted using the custom binary expression, interpreted as "is lesser or equal"');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'sortPerm'      ,@sortPerm_imp  ,'sortPerm(L);#Returns indexes I so that L%I==sort(L)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'unique'        ,@unique_imp    ,'unique(L);#Returns list L sorted ascending and without duplicates');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'flatten'       ,@flatten_imp   ,'flatten(L,...);#Returns all parameters as a flat list.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'random'        ,@random_imp    ,'random;#Returns a random value in range [0,1]#random(n);Returns a list of n random values in range [0,1]');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'max'           ,@max_imp       ,'max(L);#Returns the greatest element out of list L#max(x,y,...);#Returns the greatest element out of the given parameters');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'argMax'        ,@argMax_imp    ,'argMax(L);#Returns the index of the greatest element out of list L (or the first index if ambiguous)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'min'           ,@min_imp       ,'min(L);#Returns the smallest element out of list L#min(x,y,...);#Returns the smallest element out of the given parameters');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'argMin'        ,@argMin_imp    ,'argMin(L);#Returns the index of the smallest element out of list L (or the first index if ambiguous)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'size'          ,@size_imp      ,'size(L);#Returns the number of elements in list L');
  //Functions on Strings:
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'length'        ,@length_imp    ,'length(S:string);#Returns the number of characters in string S');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'pos'           ,@pos_imp       ,'pos(subString,searchInString);#Returns the index of the first occurence of subString in searchInString or -1 if there is none');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'copy'          ,@copy_imp      ,'copy(S,start,length):#Returns the substring of S starting at index start and having specified length');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'split'         ,@split_imp     ,'split(S:string;splitter:string);#Returns a list of strings obtained by splitting S at the specified splitters#The splitters themselves are not contained in the result');

  registerRule(DEFAULT_BUILTIN_NAMESPACE,'softCast'      ,@softCast_imp  ,'softCast(X);#Returns a simplified version of X, trying to parse integers, real values and booleans');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'trim'          ,@trim_imp      ,'trim(S:string);#Returns string S without leading or trailing spaces');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'trimLeft'      ,@trimLeft_imp  ,'trimLeft(S:string);#Returns string S without leading spaces');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'trimRight'     ,@trimRight_imp ,'trimRight(S:string);#Returns string S without trailing spaces');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'upper'         ,@upper_imp     ,'upper(S:string);#Returns an uppercase representation of S');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'lower'         ,@lower_imp     ,'lower(S:string);#Returns an lowercase representation of S');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'unbrace'       ,@unbrace_imp   ,'unbrace(S:string);#Returns an unbraced representation of S');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'escape'        ,@escape_imp    ,'escape(S:string);#Returns an escaped representation of S');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'string'        ,@string_imp    ,'string(X);#Returns a string-representation of X');

  registerRule(SYSTEM_BUILTIN_NAMESPACE,'files'         ,@files_impl       ,'files(searchPattern:string);#Returns a list of files matching the given search pattern');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'folders'       ,@folders_impl     ,'folders(searchPattern:string);#Returns a list of folders matching the given search pattern');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileExists'    ,@fileExists_impl  ,'fileExists(filename:string);#Returns true if the specified file exists and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileContents'  ,@fileContents_impl,'fileContents(filename:string);#Returns the contents of the specified file as one string');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileLines'     ,@fileLines_impl   ,'fileLines(filename:string);#Returns the contents of the specified file as a list of strings#Information on the line breaks is lost#'+
                                                         'fileLines(filename:string,firstIdx:int,lastIdx:int);#Returns the specified range of lines or the empty list if no line was found in the range. Indexes are inclusive and start with 0.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeFile'     ,@writeFile_impl,'writeFile(filename:string, content:string);#Writes the specified content to the specified file and returns true');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'writeFileLines',@writeFileLines_impl,'writeFileLines(filename:string, content:stringList);#Writes the specified content to the specified file (using system-default line breaks) and returns true');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'replaceOne'    ,@replaceOne_impl,'replaceOne(source:string,lookFor,replaceBy);#Replaces the first occurences of lookFor in source by replaceBy#lookFor and replaceBy may be of type string or stringList');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'replace'       ,@replace_impl,'replace(source:string,lookFor,replaceBy);#Recursively replaces all occurences of lookFor in source by replaceBy#lookFor and replaceBy may be of type string or stringList');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'repeat'        ,@repeat_impl,'repeat(s:string,k:int);#Returns a string containing s repeated k times');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'exec'          ,@execSync_impl,'exec(programPath:string,parameters ...);#Executes the specified program and returns the text output');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'execAsync'     ,@execAsync_impl,'execAsync(programPath:string,parameters ...);#Starts the specified program and returns true');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'tokenSplit'    ,@tokenSplit_impl,'tokenSplit(S:string);#tokenSplit(S:string,language:string);#Returns a list of strings from S for a given language#Languages: <code>MNH, Pascal, Java</code>');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath'        ,@myPath_impl,'myPath;#returns the path to the current package');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor',@executor_impl,'executor;#returns the path to the currently executing instance of MNH');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'trueCount'     ,@trueCount_impl,'trueCount(B:booleanList);#Returns the number of true values in B');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isNan'         ,@isNan_impl,'isNan(n);#Returns true if n is a number representing the value Not-A-Number');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isInfinite'    ,@isInfinite_impl,'isInfinite(n);#Returns true if n is a number representing an infinite value');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'isInRange'     ,@isInRange_impl,'isInRange(x,x0,x1);#Returns true, if x0<=x<=x1 and x is neither Not-A-Number nor infinite');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'splitFileName' ,@splitFileName_imp,'splitFilename(name:string);#Returns various representations and parts of the given name');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'systime'       ,@systime_imp,'sytime;#Returns the current time as a real number');

  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord'           ,@ord_imp           ,'ord(x);#Returns the ordinal value of x');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'format'        ,@format_imp        ,'format(formatString:string,...);#Returns a formatted version of the given 0..n parameters');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printf'        ,@printf_imp        ,'fprint(formatString:string,...);#Prints a formatted version of the given 0..n parameters and returns void');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deleteFile',@deleteFile_imp,'deleteFile(filename:string);#Deletes the given file, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'deleteDir',@deleteDir_imp,'deleteDir(directoryname:string);#Deletes the given directory, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'copyFile',@copyFile_imp,'copyFile(source:string,dest:string);#Copies a file from source to dest, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'moveFile',@moveFile_imp,'moveFile(source:string,dest:string);#Moves a file from source to dest, returning true on success and false otherwise');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'fileInfo',@fileInfo_imp,'fileInfo(filename:string);#Retuns file info as a key-value-list');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'httpGet',@httpGet_imp,'httpGet(URL:string);#Retrieves the contents of the given URL and returns them as a string');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'setErrorlevel',@setErrorlevel_imp,'setErrorlevel(level:int);#Sets the errorlevel returned by the interpreter');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash',@hash_imp,'hash(x);#Returns the builtin hash for the given literal');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listBuiltin',@listBuiltin_imp,'listBuiltin;#Returns a list of all built-in functions (qualified and non-qualified)');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'driveInfo',@driveInfo_imp,'driveInfo;#Returns info on the computer''''s drives/volumes.');

FINALIZATION
  {$ifdef debugMode}
  writeln(stdErr,'Finalizing mnh_funcs');
  {$endif}
  intrinsicRuleMap.destroy;
  intrinsicRuleExplanationMap.destroy;
  //Critical sections:------------------------------------------------------------
  system.DoneCriticalsection(print_cs);
  system.DoneCriticalsection(file_cs);
  //------------------------------------------------------------:Critical sections
  {$ifdef debugMode}
  writeln(stdErr,'mnh_funcs finalized');
  {$endif}
end.
