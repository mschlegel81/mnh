UNIT mnh_funcs_list;
INTERFACE
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,mnh_out_adapters;
IMPLEMENTATION

FUNCTION add_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType in C_validListTypes) then begin
      if params^.value(0)^.getReferenceCount=1 then begin
        result:=params^.value(0);
        result^.rereference;
      end else result:=P_listLiteral(params^.value(0))^.clone;
      P_listLiteral(result)^.append(params^.value(1),true,adapters);
    end;
  end;

{$MACRO ON}
{$define SUB_LIST_IMPL:=
begin
  result:=nil;
  if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType in C_validListTypes) then begin
    if      (params^.size=1) then result:=P_listLiteral(params^.value(0))^.CALL_MACRO
    else if (params^.size=2) and (params^.value(1)^.literalType=lt_int) then result:=P_listLiteral(params^.value(0))^.CALL_MACRO(P_intLiteral(params^.value(1))^.value);
  end;
end}

FUNCTION head_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=head}
{$define ID_MACRO:='head'}
SUB_LIST_IMPL;

FUNCTION tail_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=tail}
{$define ID_MACRO:='tail'}
SUB_LIST_IMPL;

FUNCTION leading_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=leading}
{$define ID_MACRO:='leading'}
SUB_LIST_IMPL;

FUNCTION trailing_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
{$define CALL_MACRO:=trailing}
{$define ID_MACRO:='trailing'}
SUB_LIST_IMPL;

{$undef SUB_LIST_IMPL}

FUNCTION sort_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in C_validListTypes) then begin
      if (params^.value(0)^.getReferenceCount=1) then begin
        result:=params^.value(0);
        result^.rereference;
      end else result:=P_listLiteral(params^.value(0))^.clone;
      P_listLiteral(result)^.sort;
    end else if (params<>nil) and (params^.size=2)
            and (params^.value(0)^.literalType in C_validListTypes)
            and (params^.value(1)^.literalType=lt_expression) then begin
      if (params^.value(0)^.getReferenceCount=1) then begin
        result:=params^.value(0);
        result^.rereference;
      end else result:=P_listLiteral(params^.value(0))^.clone;
      P_listLiteral(result)^.customSort(P_expressionLiteral(params^.value(1)),adapters);
    end;
  end;

FUNCTION sortPerm_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in C_validListTypes)
    then result:=P_listLiteral(params^.value(0))^.sortPerm;
  end;

FUNCTION unique_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in C_validListTypes) then begin
      if (params^.value(0)^.getReferenceCount=1) then begin
        result:=params^.value(0);
        result^.rereference;
      end else result:=P_listLiteral(params^.value(0))^.clone;
      P_listLiteral(result)^.unique;
    end;
  end;

FUNCTION flatten_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  PROCEDURE recurse_flatten(CONST L:P_listLiteral);
    VAR i:longint;
    begin
      for i:=0 to L^.size-1 do
      if L^.value(i)^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression]
      then P_listLiteral(result)^.append(L^.value(i),true,adapters)
      else recurse_flatten(P_listLiteral(L^.value(i)));
    end;

  begin
    if params<>nil then begin
      result:=newListLiteral;
      recurse_flatten(params);
    end else result:=nil;
  end;

FUNCTION size_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case params^.value(0)^.literalType of
        lt_error..  lt_expression: result:=newIntLiteral(1);
        lt_list..lt_listWithError: result:=newIntLiteral(P_listLiteral(params^.value(0))^.size);
      end;
    end;
  end;

FUNCTION trueCount_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
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
        lt_emptyList: exit(newIntLiteral(0));
      end;
    end;
  end;

FUNCTION listAnd_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR B:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      B:=params^.value(0);
      case B^.literalType of
        lt_boolean: begin result:=B; result^.rereference; end;
        lt_booleanList: begin
          for i:=0 to P_listLiteral(B)^.size-1 do if not(P_boolLiteral(P_listLiteral(B)^.value(i))^.value) then exit(newBoolLiteral(false));
          exit(newBoolLiteral(true));
        end;
        lt_emptyList: exit(newBoolLiteral(true));
      end;
    end;
  end;

FUNCTION listOr_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR B:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      B:=params^.value(0);
      case B^.literalType of
        lt_boolean: begin result:=B; result^.rereference; end;
        lt_booleanList: begin
          for i:=0 to P_listLiteral(B)^.size-1 do if P_boolLiteral(P_listLiteral(B)^.value(i))^.value then exit(newBoolLiteral(true));
          exit(newBoolLiteral(false));
        end;
        lt_emptyList: exit(newBoolLiteral(false));
      end;
    end;
  end;

FUNCTION reverseList_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in C_validListTypes) then begin
      result:=newListLiteral;
      for i:=P_listLiteral(params^.value(0))^.size-1 downto 0 do
        P_listLiteral(result)^.append(P_listLiteral(params^.value(0))^.value(i),true,adapters);
    end;
  end;

FUNCTION indexOf_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  VAR i:longint;
      R:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_booleanList,lt_emptyList]) then begin
      R:=newListLiteral;
      with P_listLiteral(params^.value(0))^ do for i:=0 to size-1 do if P_boolLiteral(value(i))^.value
        then R^.appendInt(i);
      result:=R;
    end;
  end;

INITIALIZATION
  //Functions on lists:
  registerRule(LIST_NAMESPACE,'add',@add_imp,'add(L,e);#Returns L with the new element e appended');
  registerRule(LIST_NAMESPACE,'head',@head_imp,'head(L);#Returns the first element of list L or [] if L is empty#head(L,k);#Returns the first min(k,size(L)) elements of L or [] if L is empty');
  registerRule(LIST_NAMESPACE,'tail',@tail_imp,'tail(L);#Returns list L without the first element#tail(L,k);#Returns L without the first k elements');
  registerRule(LIST_NAMESPACE,'leading',@leading_imp,'leading(L);#Returns L without the last element or [] if L is empty#leading(L,k);#Returns L without the last k elements or [] if L is empty');
  registerRule(LIST_NAMESPACE,'trailing',@trailing_imp,'trailing(L);#Returns the last element of L#trailing(L,k);#Returns the last k elements of L');
  registerRule(LIST_NAMESPACE,'sort',@sort_imp,'sort(L);#Returns list L sorted ascending (using fallbacks for uncomparable types)#sort(L,leqExpression:expression);#Returns L sorted using the custom binary expression, interpreted as "is lesser or equal"');
  registerRule(LIST_NAMESPACE,'sortPerm',@sortPerm_imp,'sortPerm(L);#Returns indexes I so that L%I==sort(L)');
  registerRule(LIST_NAMESPACE,'unique',@unique_imp,'unique(L);#Returns list L sorted ascending and without duplicates');
  registerRule(LIST_NAMESPACE,'elementFrequency',@getElementFreqency,'elementFrequency(L);#Returns a list of pairs [count,e] containing distinct elements e of L and their respective frequencies');
  registerRule(LIST_NAMESPACE,'union',@setUnion,'union(A,...);#Returns a union of all given parameters. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'intersect',@setIntersect,'intersect(A,...);#Returns an intersection of all given parameters. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'minus',@setMinus,'minus(A,B);#Returns the asymmetric set difference of A and B. All parameters must be lists.');
  registerRule(LIST_NAMESPACE,'flatten',@flatten_imp,'flatten(L,...);#Returns all parameters as a flat list.');
  registerRule(LIST_NAMESPACE,'size',@size_imp,'size(L);#Returns the number of elements in list L');
  registerRule(LIST_NAMESPACE,'trueCount',@trueCount_impl,'trueCount(B:booleanList);#Returns the number of true values in B');
  registerRule(LIST_NAMESPACE,'listAnd',@listAnd_impl,'listAnd(B:booleanList);#Returns true if all values in B are true, false otherwise');
  registerRule(LIST_NAMESPACE,'listOr',@listOr_impl,'listOr(B:booleanList);#Returns true if any value in B is true, false otherwise');
  registerRule(LIST_NAMESPACE,'reverseList',@reverseList_impl,'reverse(L:list);#Returns L reversed');
  registerRule(LIST_NAMESPACE,'put',@mapPut,'put(L:keyValueList,key:string,value);#Returns L with an additional or modified key-value-pair [key,value].');
  registerRule(LIST_NAMESPACE,'get',@mapGet,'get(L:keyValueList,key:string);#Returns the element with matching key or the empty list if no such element was found.#'+
                                            'get(L:keyValueList,key:string,fallback);#Returns the element with matching key or fallback if no such element was found.');
  registerRule(LIST_NAMESPACE,'indexOf',@indexOf_impl,'indexOf(B:booleanList);#Returns the indexes for which B is true.');
end.
