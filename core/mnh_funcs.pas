UNIT mnh_funcs;
INTERFACE
USES sysutils,mygenerics,mnh_constants,mnh_litvar,math,mnh_out_adapters;
TYPE
  T_intFuncCallback=FUNCTION(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
VAR
  intrinsicRuleMap    :specialize G_stringKeyMap<T_intFuncCallback>;
  intrinsicRuleAliases:specialize G_stringKeyMap<T_intFuncCallback>;

PROCEDURE registerRule(CONST name:string; CONST ptr:T_intFuncCallback);

//Callbacks:--------------------------------
TYPE T_resolveNullaryCallback=FUNCTION (CONST subrulePointer:pointer):P_literal;
VAR resolveNullaryCallback:T_resolveNullaryCallback;
//--------------------------------:Callbacks
IMPLEMENTATION
PROCEDURE raiseNotApplicableError(CONST functionName:string; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation);
  VAR complaintText:ansistring;
  begin
    complaintText:='Function '+functionName+' cannot be applied to parameters ';
    if params=nil then complaintText:=complaintText+'()'
                  else complaintText:=complaintText+params^.toParameterListString(true);
    raiseError(el3_evalError,complaintText,tokenLocation);
  end;

FUNCTION print_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR stringToPrint:ansistring='';
      i:longint;    
  begin
    for i:=0 to params^.size-1 do case params^.value(i)^.literalType of
      lt_error,
      lt_boolean,
      lt_int,
      lt_real,
      lt_string,
      lt_expression: stringToPrint:=stringToPrint + P_scalarLiteral(params^.value(i))^.stringForm;
      lt_list,
      lt_booleanList,
      lt_intList,
      lt_realList,
      lt_numList,
      lt_stringList,
      lt_uncheckedList,
      lt_listWithError: stringToPrint:=stringToPrint + params^.value(i)^.toString;        
    end;
    writePrint(stringToPrint);
    result:=newBoolLiteral(true);
  end;

FUNCTION sqr_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION sqr_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newIntLiteral (sqr(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(sqr(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(sqr_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function SQR cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=sqr_rec(params^.value(0))
    else raiseNotApplicableError('SQR',params,tokenLocation);
  end;
  
FUNCTION sqrt_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION sqrt_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(sqrt(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(sqrt(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(sqrt_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function SQRT cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=sqrt_rec(params^.value(0))
    else raiseNotApplicableError('SQRT',params,tokenLocation);
  end;

FUNCTION sin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION sin_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(sin(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(sin(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(sin_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function SIN cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=sin_rec(params^.value(0))
    else raiseNotApplicableError('SIN',params,tokenLocation);
  end;
  
FUNCTION arcsin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION arcsin_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(arcsin(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(arcsin(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(arcsin_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function ARCSIN cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=arcsin_rec(params^.value(0))
    else raiseNotApplicableError('ARCSIN',params,tokenLocation);
  end;

FUNCTION cos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION cos_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(cos(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(cos(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(cos_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function COS cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=cos_rec(params^.value(0))
    else raiseNotApplicableError('COS',params,tokenLocation);
  end;

FUNCTION arccos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION arccos_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(arccos(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(arccos(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(arccos_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function ARCCOS cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=arccos_rec(params^.value(0))
    else raiseNotApplicableError('ARCCOS',params,tokenLocation);
  end;

FUNCTION tan_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION tan_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(tan(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(tan(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(tan_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function TAN cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=tan_rec(params^.value(0))
    else raiseNotApplicableError('TAN',params,tokenLocation);
  end;

FUNCTION arctan_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION arctan_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(arctan(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(arctan(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(arctan_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function ARCTAN cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=arctan_rec(params^.value(0))
    else raiseNotApplicableError('ARCTAN',params,tokenLocation);
  end;

FUNCTION exp_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION exp_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(exp(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(exp(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(exp_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function EXP cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=exp_rec(params^.value(0))
    else raiseNotApplicableError('EXP',params,tokenLocation);
  end;

FUNCTION ln_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION ln_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(ln(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(ln(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(ln_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function LN cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=ln_rec(params^.value(0))
    else raiseNotApplicableError('LN',params,tokenLocation);
  end;

FUNCTION round_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION round_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : begin result:=x; x^.rereference; end;
        lt_real: result:=newIntLiteral(round(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(round_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function ROUND cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;
    
  FUNCTION round_rec2(CONST x,y:P_literal):P_literal;
    FUNCTION myRound(CONST x:extended; CONST y:int64):P_literal; inline;
      VAR pot:extended;
          i:int64;
      begin
        pot:=1;
        i:=0;
        while i<y do begin pot:=pot*10;  inc(i); end;
        while i>y do begin pot:=pot*0.1; dec(i); end;
        result:=newRealLiteral(round(x*pot)/pot);
      end;
    VAR i:longint;  
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : begin result:=x; x^.rereference; end;
        lt_real: case y^.literalType of
          lt_error,lt_listWithError: begin result:=y; result^.rereference; end;
          lt_int: result:=myRound(P_realLiteral(x)^.value,P_intLiteral(y)^.value);
          lt_list,lt_intList: begin 
            result:=newListLiteral;
            for i:=0 to P_listLiteral(y)^.size-1 do P_listLiteral(result)^.append(round_rec2(x,P_listLiteral(y)^.value(i)),false);
          end;
          else raiseError(el3_evalError,'Function ROUND cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
        end;
        lt_list,lt_intList,lt_realList,lt_numList: case y^.literalType of
          lt_error,lt_listWithError: begin result:=y; result^.rereference; end;
          lt_int: begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(round_rec2(P_listLiteral(x)^.value(i),y),false);
          end;
          lt_list,lt_intList: if P_listLiteral(x)^.size=P_listLiteral(y)^.size then begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(round_rec2(P_listLiteral(x)^.value(i),P_listLiteral(y)^.value(i)),false);
          end else raiseError(el3_evalError,'Incompatible list lengths given for function ROUND',tokenLocation);
          else raiseError(el3_evalError,'Function ROUND cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
        end;
        else raiseError(el3_evalError,'Function ROUND cannot be applied to type '+C_typeString[x^.literalType]+' (first parameter)',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=round_rec(params^.value(0)) else
    if (params<>nil) and (params^.size=2) then result:=round_rec2(params^.value(0),params^.value(1))
    else raiseNotApplicableError('ROUND',params,tokenLocation);
  end;

FUNCTION ceil_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION ceil_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : begin result:=x; x^.rereference; end;
        lt_real: result:=newIntLiteral(ceil(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(ceil_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function CEIL cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=ceil_rec(params^.value(0))
    else raiseNotApplicableError('CEIL',params,tokenLocation);
  end;

FUNCTION floor_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION floor_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : begin result:=x; x^.rereference; end;
        lt_real: result:=newIntLiteral(floor(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin 
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(floor_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseError(el3_evalError,'Function FLOOR cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) 
    then result:=floor_rec(params^.value(0))
    else raiseNotApplicableError('FLOOR',params,tokenLocation);
  end;

FUNCTION head_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;    
  FUNCTION headOf(CONST x:P_literal):P_literal;
    begin
      result:=nil;
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList] then begin
        result:=P_listLiteral(x)^.value(0);
        result^.rereference;
      end else raiseError(el3_evalError,'Function HEAD cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
    end;
    
  FUNCTION headOf2(CONST x,y:P_literal):P_listLiteral;
    VAR i,i0:longint;  
    begin
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList] then begin
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
          else raiseError(el3_evalError,'Function HEAD cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
        end;
      end else raiseError(el3_evalError,'Function HEAD cannot be applied to type '+C_typeString[x^.literalType]+' (first parameter)',tokenLocation);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=headOf(params^.value(0)) else
    if (params<>nil) and (params^.size=2) then result:=headOf2(params^.value(0),params^.value(1))
    else raiseNotApplicableError('HEAD',params,tokenLocation);
  end;

FUNCTION tail_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;    
  FUNCTION tailOf(CONST x:P_literal):P_listLiteral;
    VAR i:longint;
    begin
      result:=nil;
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList] then begin
        result:=newListLiteral;
        for i:=1 to P_listLiteral(x)^.size-1 do result^.append(P_listLiteral(x)^.value(i),true);
      end else raiseError(el3_evalError,'Function TAIL cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
    end;
    
  FUNCTION tailOf2(CONST x,y:P_literal):P_listLiteral;
    VAR i,i0:longint;  
    begin
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList] then begin
        case y^.literalType of
          lt_int: begin
            result:=newListLiteral;
            i0:=P_intLiteral(y)^.value;
            if i0>P_listLiteral(x)^.size then i0:=P_listLiteral(x)^.size;
            for i:=i0 to P_listLiteral(x)^.size-1 do result^.append(P_listLiteral(x)^.value(i),true);
          end;
          lt_intList: begin
            result:=newListLiteral;
            for i:=0 to P_listLiteral(y)^.size-1 do result^.append(tailOf2(x,P_listLiteral(y)^.value(i)),false);
          end;
          else raiseError(el3_evalError,'Function TAIL cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
        end;
      end else raiseError(el3_evalError,'Function TAIL cannot be applied to type '+C_typeString[x^.literalType]+' (first parameter)',tokenLocation);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=tailOf(params^.value(0)) else
    if (params<>nil) and (params^.size=2) then result:=tailOf2(params^.value(0),params^.value(1))
    else raiseNotApplicableError('TAIL',params,tokenLocation);
  end;
  
FUNCTION sort_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (params^.value(0)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then begin
        result:=newListLiteral;
        P_listLiteral(result)^.appendAll(P_listLiteral(params^.value(0)));
        P_listLiteral(result)^.sort;
      end else if (params^.value(0)^.literalType=lt_list) and (P_listLiteral(params^.value(0))^.size=0) then begin
        result:=params^.value(0);
        result^.rereference;
      end else raiseError(el3_evalError,'Function SORT can only be applied to flat lists containing comparable types',tokenLocation);
    end else raiseNotApplicableError('SORT',params,tokenLocation);
  end;
  
FUNCTION sortPerm_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (params^.value(0)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]) then
        result:=P_listLiteral(params^.value(0))^.sortPerm
      else if (params^.value(0)^.literalType=lt_list) and (P_listLiteral(params^.value(0))^.size=0) then begin
        result:=params^.value(0);
        result^.rereference;
      end else raiseError(el3_evalError,'Function SORTPERM can only be applied to flat lists containing comparable types',tokenLocation);
    end else raiseNotApplicableError('SORTPERM',params,tokenLocation);
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
    raiseNotApplicableError('RANDOM',params,tokenLocation);
  end;

PROCEDURE registerRule(CONST name:string; CONST ptr:T_intFuncCallback);
  begin
    intrinsicRuleAliases.put(name,ptr);
    intrinsicRuleMap.put(uppercase(name),ptr);
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
    end else raiseNotApplicableError('MAX',params,tokenLocation);
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
    end else raiseNotApplicableError('MIN',params,tokenLocation);
  end;
  
FUNCTION size_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case params^.value(0)^.literalType of
        lt_error..  lt_expression: result:=newIntLiteral(1);
        lt_list..lt_listWithError: result:=newIntLiteral(P_listLiteral(params^.value(0))^.size);
      end;   
    end else raiseNotApplicableError('SIZE',params,tokenLocation);
  end;
  
FUNCTION time_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR res:P_literal;
      time:double;  
      aid:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_expression) then begin
      time:=now;
      res:=resolveNullaryCallback(P_expressionLiteral(params^.value(0))^.value);      
      time:=now-time;
      if res<>nil then begin
        result:=newListLiteral;
        aid:=newListLiteral;
        aid^.append(newStringLiteral('result'),false);
        aid^.append(res,false);        
        P_listLiteral(result)^.append(aid,false);
        aid:=newListLiteral;
        aid^.append(newStringLiteral('time'),false);
        aid^.append(newRealLiteral(time/(24*60*60)),false);
        P_listLiteral(result)^.append(aid,false);
      end;      
    end else raiseNotApplicableError('TIME',params,tokenLocation);
  end;
  
FUNCTION split_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR splitters:array of ansistring;
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
        lt_list,lt_stringList: begin
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
      and (params^.value(0)^.literalType in [lt_string,lt_stringList,lt_list])
      and (params^.value(1)^.literalType in [lt_string,lt_stringList]) then begin
      initSplitters;
      result:=splitRecurse(params^.value(0));
    end else raiseNotApplicableError('SPLIT',params,tokenLocation);
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
    else raiseNotApplicableError('SOFTCAST',params,tokenLocation);
  end;
  
FUNCTION trim_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION trim_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.trim;
        lt_list,lt_stringList:  begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if errorLevel<el3_evalError then
            P_listLiteral(result)^.append(trim_rec(P_listLiteral(x)^.value(i)),false);
        end; 
        else result:=newErrorLiteralRaising('Cannot apply TRIM to literal of type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_string]) then result:=trim_rec(params^.value(0))
    else raiseNotApplicableError('TRIM',params,tokenLocation);
  end;

FUNCTION upper_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION upper_rec(CONST x:P_literal):P_literal;
    VAR i:longint; 
    begin
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.upper;
        lt_list,lt_stringList:  begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if errorLevel<el3_evalError then
            P_listLiteral(result)^.append(upper_rec(P_listLiteral(x)^.value(i)),false);
        end; 
        else result:=newErrorLiteralRaising('Cannot apply UPPER to literal of type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_string]) then result:=upper_rec(params^.value(0))
    else raiseNotApplicableError('UPPER',params,tokenLocation);
  end;
  
FUNCTION lower_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION lower_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.lower;
        lt_list,lt_stringList:  begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if errorLevel<el3_evalError then
            P_listLiteral(result)^.append(lower_rec(P_listLiteral(x)^.value(i)),false);
        end; 
        else result:=newErrorLiteralRaising('Cannot apply LOWER to literal of type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_string]) then result:=lower_rec(params^.value(0))
    else raiseNotApplicableError('LOWER',params,tokenLocation);
  end;
  
{$WARNING TODO: fileExists(filename)}
{$WARNING TODO: readFile(filename)}
{$WARNING TODO: writeFile(filename,strings)}
{$WARNING TODO: fileTree(filename)}
{$WARNING TODO: replace(full,original,subst)}
{$WARNING TODO: replaceAll(full,original,subst)}
{$WARNING TODO: callSync(executablepath)}
{$WARNING TODO: callAsync(executablepath)}
  
INITIALIZATION
  intrinsicRuleMap.create;
  intrinsicRuleAliases.create;
  registerRule('print'   ,@print_imp   );
  registerRule('sqr'     ,@sqr_imp     );
  registerRule('sqrt'    ,@sqrt_imp    );
  registerRule('sin'     ,@sin_imp     );
  registerRule('arcsin'  ,@arcsin_imp  );
  registerRule('cos'     ,@cos_imp     );
  registerRule('arccos'  ,@arccos_imp  );
  registerRule('tan'     ,@tan_imp     );
  registerRule('arctan'  ,@arctan_imp  );
  registerRule('exp'     ,@exp_imp     );
  registerRule('ln'      ,@ln_imp      );
  registerRule('round'   ,@round_imp   );
  registerRule('ceil'    ,@ceil_imp    );
  registerRule('floor'   ,@floor_imp   );
  registerRule('head'    ,@head_imp    );
  registerRule('tail'    ,@tail_imp    );
  registerRule('sort'    ,@sort_imp    );
  registerRule('sortPerm',@sortPerm_imp);
  registerRule('flatten' ,@flatten_imp );
  registerRule('random'  ,@random_imp  );
  registerRule('max'     ,@max_imp     );
  registerRule('min'     ,@min_imp     );
  registerRule('size'    ,@size_imp    );
  registerRule('time'    ,@time_imp    );
  registerRule('split'   ,@split_imp   );
  registerRule('softCast',@softCast_imp);
  registerRule('trim'    ,@trim_imp    );
  registerRule('upper'   ,@upper_imp   );
  registerRule('lower'   ,@lower_imp   );
  
FINALIZATION
  intrinsicRuleMap.destroy;
  intrinsicRuleAliases.destroy;
end.