UNIT mnh_funcs;
INTERFACE
USES sysutils,mygenerics,mnh_constants,mnh_litvar,math,mnh_out_adapters,mnh_tokloc,mnh_fileWrappers,myStringutil,classes,process;
TYPE
  T_intFuncCallback=FUNCTION(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;

VAR
  intrinsicRuleMap           :specialize G_stringKeyMap<T_intFuncCallback>;
  intrinsicRuleExplanationMap:specialize G_stringKeyMap<ansistring>;
  pureIntrinsicFunctions     :specialize G_list<pointer>;

PROCEDURE registerRule(CONST name:ansistring; CONST ptr:T_intFuncCallback; CONST isPure:boolean; CONST explanation:ansistring);
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation);
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST typ:T_literalType; CONST messageTail:ansistring; CONST tokenLocation:T_tokenLocation);

IMPLEMENTATION
//Critical sections:------------------------------------------------------------
VAR print_cs:system.TRTLCriticalSection;
    file_cs :system.TRTLCriticalSection;
//------------------------------------------------------------:Critical sections
PROCEDURE registerRule(CONST name:ansistring; CONST ptr:T_intFuncCallback; CONST isPure:boolean; CONST explanation:ansistring);
  VAR oldExplanation:ansistring;
  begin
    intrinsicRuleMap.put(name,ptr);
    if (explanation<>'') or not(intrinsicRuleExplanationMap.containsKey(name,oldExplanation))
      then intrinsicRuleExplanationMap.put(name,explanation);
    if isPure then pureIntrinsicFunctions.add(ptr);
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

FUNCTION head_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION headOf(CONST x:P_literal):P_literal;
    begin
      result:=nil;
      if x^.literalType in C_validListTypes then begin
        if P_listLiteral(x)^.size>0 then begin
          result:=P_listLiteral(x)^.value(0);
          result^.rereference;
        end else result:=newListLiteral;
      end else raiseNotApplicableError('head',x^.literalType,'',tokenLocation);
    end;

  FUNCTION headOf2(CONST x,y:P_literal):P_listLiteral;
    VAR i,i0:longint;
    begin
      result:=nil;
      if x^.literalType in C_validListTypes then begin
        case y^.literalType of
          lt_int: begin
            result:=newListLiteral;
            i0:=P_intLiteral(y)^.value;
            if i0>P_listLiteral(x)^.size then i0:=P_listLiteral(x)^.size;
            for i:=0 to i0-1 do result^.append(P_listLiteral(x)^.value(i),true);
          end;
          lt_intList: begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(y)^.size-1 do result^.append(headOf2(x,P_listLiteral(y)^.value(i)),false);
          end;
          else raiseNotApplicableError('head',y^.literalType,' (second parameter)',tokenLocation);
        end;
      end else raiseNotApplicableError('head',x^.literalType,' (first parameter)',tokenLocation);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=headOf(params^.value(0)) else
    if (params<>nil) and (params^.size=2) then result:=headOf2(params^.value(0),params^.value(1))
    else raiseNotApplicableError('head',params,tokenLocation);
  end;

FUNCTION tail_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION tailOf(CONST x:P_literal):P_listLiteral;
    VAR i:longint;
    begin
      result:=nil;
      if x^.literalType in C_validListTypes then begin
        result:=newListLiteral;
        for i:=1 to P_listLiteral(x)^.size-1 do result^.append(P_listLiteral(x)^.value(i),true);
      end else raiseError(el3_evalError,'FUNCTION tail cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
    end;

  FUNCTION tailOf2(CONST x,y:P_literal):P_listLiteral;
    VAR i,i0:longint;
    begin
      if x^.literalType in C_validListTypes then begin
        case y^.literalType of
          lt_int: begin
            result:=newListLiteral;
            i0:=P_intLiteral(y)^.value;
            if i0<0 then i0:=0;
            if i0>P_listLiteral(x)^.size then i0:=P_listLiteral(x)^.size;
            for i:=i0 to P_listLiteral(x)^.size-1 do result^.append(P_listLiteral(x)^.value(i),true);
          end;
          lt_intList: begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(y)^.size-1 do result^.append(tailOf2(x,P_listLiteral(y)^.value(i)),false);
          end;
          else raiseError(el3_evalError,'FUNCTION tail cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
        end;
      end else raiseError(el3_evalError,'FUNCTION tail cannot be applied to type '+C_typeString[x^.literalType]+' (first parameter)',tokenLocation);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=tailOf(params^.value(0)) else
    if (params<>nil) and (params^.size=2) then result:=tailOf2(params^.value(0),params^.value(1))
    else raiseNotApplicableError('tail',params,tokenLocation);
  end;

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
      ok:=writeFile(P_stringLiteral(params^.value(0))^.value,
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
        while tempProcess.Running and (errorLevel<el3_evalError) do begin
          memStream.SetSize(BytesRead+READ_BYTES);
          n := tempProcess.Output.Read((memStream.Memory+BytesRead)^, READ_BYTES);
          if n>0 then Inc(BytesRead, n)
                 else Sleep(10);
        end;
        if errorLevel>=el3_evalError then begin
          tempProcess.CloseInput;
          tempProcess.CloseOutput;
          tempProcess.CloseStderr;
        end;
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
    if (tokenLocation.provider=nil) or
       (tokenLocation.provider^.getPath='') then result:=newStringLiteral('<Unknown>')
                                            else result:=newStringLiteral(tokenLocation.provider^.getPath);
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
      end else result:=newStringLiteral(resultString[0]);
    end else raiseNotApplicableError('format',params,tokenLocation);
  end;

FUNCTION printf_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  INNER_FORMATTING;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string) then begin
      decomposeFormatString(P_stringLiteral(params^.value(0))^.value);
      system.EnterCriticalSection(print_cs);
      writePrint(resultString);
      system.LeaveCriticalsection(print_cs);
      result:=newVoidLiteral;
    end else raiseNotApplicableError('printf',params,tokenLocation);
  end;
{$undef INNER_FORMATTING}

FUNCTION deleteFile_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newBoolLiteral(DeleteFile(P_stringLiteral(params^.value(0))^.value));
    end else raiseNotApplicableError('deleteFile',params,tokenLocation);
  end;

INITIALIZATION
  //Critical sections:------------------------------------------------------------
  system.InitCriticalSection(print_cs);
  system.InitCriticalSection(file_cs);
  //------------------------------------------------------------:Critical sections
  intrinsicRuleMap.create;
  intrinsicRuleExplanationMap.create;
  pureIntrinsicFunctions.create;
  registerRule('clearPrint'    ,@clearPrint_imp,false,'clearPrint(...);#Clears the output and returns void.');
  registerRule('print'         ,@print_imp     ,false,'print(...);#Prints out the given parameters and returns void#if tabs and line breaks are part of the output, a default pretty-printing is used');
  //Functions on lists:
  registerRule('head'          ,@head_imp      ,true,'head(L);#Returns the first element of list L or [] if L is empty#head(L,k);#Returns the first min(k,size(L)) elements of L or [] if L is empty');
  registerRule('tail'          ,@tail_imp      ,true,'tail(L);#Returns list L without the first element#tail(L,k);#Returns L without the first k elements');
  registerRule('sort'          ,@sort_imp      ,true,'sort(L);#Returns list L sorted ascending (using fallbacks for uncomparable types)#sort(L,leqExpression:expression);#Returns L sorted using the custom binary expression, interpreted as "is lesser or equal"');
  registerRule('sortPerm'      ,@sortPerm_imp  ,true,'sortPerm(L);#Returns indexes I so that L%I==sort(L)');
  registerRule('unique'        ,@unique_imp    ,true,'unique(L);#Returns list L sorted ascending and without duplicates');
  registerRule('flatten'       ,@flatten_imp   ,true,'flatten(L,...);#Returns all parameters as a flat list.');
  registerRule('random'        ,@random_imp    ,false,'random;#Returns a random value in range [0,1]#random(n);Returns a list of n random values in range [0,1]');
  registerRule('max'           ,@max_imp       ,true,'max(L);#Returns the greatest element out of list L#max(x,y,...);#Returns the greatest element out of the given parameters');
  registerRule('argMax'        ,@argMax_imp    ,true,'argMax(L);#Returns the index of the greatest element out of list L (or the first index if ambiguous)');
  registerRule('min'           ,@min_imp       ,true,'min(L);#Returns the smallest element out of list L#min(x,y,...);#Returns the smallest element out of the given parameters');
  registerRule('argMin'        ,@argMin_imp    ,true,'argMin(L);#Returns the index of the smallest element out of list L (or the first index if ambiguous)');
  registerRule('size'          ,@size_imp      ,true,'size(L);#Returns the number of elements in list L');
  //Functions on Strings:
  registerRule('length'        ,@length_imp    ,true,'length(S:string);#Returns the number of characters in string S');
  registerRule('pos'           ,@pos_imp       ,true,'pos(subString,searchInString);#Returns the index of the first occurence of subString in searchInString or -1 if there is none');
  registerRule('copy'          ,@copy_imp      ,true,'copy(S,start,length):#Returns the substring of S starting at index start and having specified length');
  registerRule('split'         ,@split_imp     ,true,'split(S:string;splitter:string);#Returns a list of strings obtained by splitting S at the specified splitters#The splitters themselves are not contained in the result');

  registerRule('softCast'      ,@softCast_imp  ,true,'softCast(X);#Returns a simplified version of X, trying to parse integers, real values and booleans');
  registerRule('trim'          ,@trim_imp      ,true,'trim(S:string);#Returns string S without leading or trailing spaces');
  registerRule('trimLeft'      ,@trimLeft_imp  ,true,'trimLeft(S:string);#Returns string S without leading spaces');
  registerRule('trimRight'     ,@trimRight_imp ,true,'trimRight(S:string);#Returns string S without trailing spaces');
  registerRule('upper'         ,@upper_imp     ,true,'upper(S:string);#Returns an uppercase representation of S');
  registerRule('lower'         ,@lower_imp     ,true,'lower(S:string);#Returns an lowercase representation of S');
  registerRule('unbrace'       ,@unbrace_imp   ,true,'unbrace(S:string);#Returns an unbraced representation of S');
  registerRule('string'        ,@string_imp    ,true,'string(X);#Returns a string-representation of X');

  registerRule('files'         ,@files_impl       ,false,'files(searchPattern:string);#Returns a list of files matching the given search pattern');
  registerRule('folders'       ,@folders_impl     ,false,'folders(searchPattern:string);#Returns a list of folders matching the given search pattern');
  registerRule('fileExists'    ,@fileExists_impl  ,false,'fileExists(filename:string);#Returns true if the specified file exists and false otherwise');
  registerRule('fileContents'  ,@fileContents_impl,false,'fileContents(filename:string);#Returns the contents of the specified file as one string');
  registerRule('fileLines'     ,@fileLines_impl   ,false,'fileLines(filename:string);#Returns the contents of the specified file as a list of strings#Information on the line breaks is lost#'+
                                                         'fileLines(filename:string,firstIdx:int,lastIdx:int);#Returns the specified range of lines or the empty list if no line was found in the range. Indexes are inclusive and start with 0.');
  registerRule('writeFile'     ,@writeFile_impl,false,'writeFile(filename:string, content:string);#Writes the specified content to the specified file and returns true');
  registerRule('writeFileLines',@writeFileLines_impl,false,'writeFileLines(filename:string, content:stringList);#Writes the specified content to the specified file (using system-default line breaks) and returns true');
  registerRule('replaceOne'    ,@replaceOne_impl,true,'replaceOne(source:string,lookFor,replaceBy);#Replaces the first occurences of lookFor in source by replaceBy#lookFor and replaceBy may be of type string or stringList');
  registerRule('replace'       ,@replace_impl,true,'replace(source:string,lookFor,replaceBy);#Recursively replaces all occurences of lookFor in source by replaceBy#lookFor and replaceBy may be of type string or stringList');
  registerRule('exec'          ,@execSync_impl,false,'exec(programPath:string,parameters ...);#Executes the specified program and returns the text output');
  registerRule('execAsync'     ,@execAsync_impl,false,'execAsync(programPath:string,parameters ...);#Starts the specified program and returns true');
  registerRule('tokenSplit'    ,@tokenSplit_impl,true,'tokenSplit(S:string);#tokenSplit(S:string,language:string);#Returns a list of strings from S for a given language#Languages: <code>MNH, Pascal, Java</code>');
  registerRule('myPath'        ,@myPath_impl,true,'myPath;#returns the path to the current package');
  registerRule('executor',@executor_impl,true,'executor;#returns the path to the currently executing instance of MNH');
  registerRule('trueCount'     ,@trueCount_impl,true,'trueCount(B:booleanList);#Returns the number of true values in B');
  registerRule('isNan'         ,@isNan_impl,true,'isNan(n);#Returns true if n is a number representing the value Not-A-Number');
  registerRule('isInfinite'    ,@isInfinite_impl,true,'isInfinite(n);#Returns true if n is a number representing an infinite value');
  registerRule('isInRange'     ,@isInRange_impl,true,'isInRange(x,x0,x1);#Returns true, if x0<=x<=x1 and x is neither Not-A-Number nor infinite');
  registerRule('splitFileName' ,@splitFileName_imp,true,'splitFilename(name:string);#Returns various representations and parts of the given name');
  registerRule('systime'       ,@systime_imp,false,'sytime;#Returns the current time as a real number');

  registerRule('ord'           ,@ord_imp           ,true,'ord(x);#Returns the ordinal value of x');
  registerRule('format'        ,@format_imp        ,true,'format(formatString:string,...);#Returns a formatted version of the given 0..n parameters');
  registerRule('printf'        ,@printf_imp        ,false,'fprint(formatString:string,...);#Prints a formatted version of the given 0..n parameters and returns void');
  registerRule('deleteFile',@deleteFile_imp,false,'deleteFile(filename:string);#Deletes the given file, returning true on success and false otherwise');

FINALIZATION
  {$ifdef debugMode}
  writeln(stdErr,'Finalizing mnh_funcs');
  {$endif}
  intrinsicRuleMap.destroy;
  intrinsicRuleExplanationMap.destroy;
  pureIntrinsicFunctions.destroy;
  //Critical sections:------------------------------------------------------------
  system.DoneCriticalsection(print_cs);
  system.DoneCriticalsection(file_cs);
  //------------------------------------------------------------:Critical sections
  {$ifdef debugMode}
  writeln(stdErr,'mnh_funcs finalized');
  {$endif}
end.
