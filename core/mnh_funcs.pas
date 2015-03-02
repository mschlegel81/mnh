UNIT mnh_funcs;
INTERFACE
USES sysutils,mygenerics,mnh_constants,mnh_litvar,math,mnh_out_adapters,mnh_tokloc,mnh_fileWrappers,mnh_stringutil,classes;
TYPE
  T_intFuncCallback=FUNCTION(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;

VAR
  intrinsicRuleMap           :specialize G_stringKeyMap<T_intFuncCallback>;
  intrinsicRuleExplanationMap:specialize G_stringKeyMap<ansistring>;
  mnh_console_executable:ansistring;
PROCEDURE registerRule(CONST name:string; CONST ptr:T_intFuncCallback; CONST explanation:ansistring);

//Callbacks:--------------------------------
TYPE T_resolveNullaryCallback=FUNCTION (CONST subrulePointer:pointer; CONST callDepth:word):P_literal;
VAR resolveNullaryCallback:T_resolveNullaryCallback;

TYPE T_stringToExprCallback=FUNCTION(s:ansistring; CONST location:T_tokenLocation):P_scalarLiteral;
VAR stringToExprCallback:T_stringToExprCallback;
//--------------------------------:Callbacks
IMPLEMENTATION
PROCEDURE raiseNotApplicableError(CONST functionName:string; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation);
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function ['+functionName+'] cannot be applied to parameters ';
    if params=nil then complaintText:=complaintText+'()'
                  else complaintText:=complaintText+params^.toParameterListString(true);
    raiseError(el3_evalError,complaintText,tokenLocation);
  end;

PROCEDURE raiseNotApplicableError(CONST functionName:string; CONST typ:T_literalType; CONST messageTail:string; CONST tokenLocation:T_tokenLocation);
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function ['+functionName+'] cannot be applied to type '+C_typeString[typ]+messageTail;
    raiseError(el3_evalError,complaintText,tokenLocation);
  end;

FUNCTION not_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION not_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_boolean: result:=newBoolLiteral(not(P_boolLiteral(x)^.value));
        lt_int:     result:=newIntLiteral (not(P_intLiteral (x)^.value));
        lt_list,lt_booleanList, lt_intList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do
            P_listLiteral(result)^.append(not_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('not',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=not_rec(params^.value(0))
    else raiseNotApplicableError('not',params,tokenLocation);
  end;

FUNCTION print_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR stringToPrint:ansistring='';
      i:longint;
  begin
    if params<>nil then for i:=0 to params^.size-1 do case params^.value(i)^.literalType of
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

FUNCTION abs_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION abs_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newIntLiteral (abs(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(abs(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(abs_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('abs',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=abs_rec(params^.value(0))
    else raiseNotApplicableError('abs',params,tokenLocation);
  end;

FUNCTION sign_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION sign_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newIntLiteral(sign(P_intLiteral (x)^.value));
        lt_real: result:=newIntLiteral(sign(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(sign_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('sign',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=sign_rec(params^.value(0))
    else raiseNotApplicableError('sign',params,tokenLocation);
  end;

FUNCTION sqr_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION sqr_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newIntLiteral (sqr(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(sqr(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(sqr_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('sqr',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=sqr_rec(params^.value(0))
    else raiseNotApplicableError('sqr',params,tokenLocation);
  end;

FUNCTION sqrt_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION sqrt_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(sqrt(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(sqrt(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(sqrt_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('sqrt',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=sqrt_rec(params^.value(0))
    else raiseNotApplicableError('sqrt',params,tokenLocation);
  end;

FUNCTION sin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION sin_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(sin(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(sin(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(sin_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('sin',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=sin_rec(params^.value(0))
    else raiseNotApplicableError('sin',params,tokenLocation);
  end;

FUNCTION arcsin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION arcsin_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(arcsin(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(arcsin(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(arcsin_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('arcsin',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=arcsin_rec(params^.value(0))
    else raiseNotApplicableError('arcsin',params,tokenLocation);
  end;

FUNCTION cos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION cos_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(cos(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(cos(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(cos_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('cos',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=cos_rec(params^.value(0))
    else raiseNotApplicableError('cos',params,tokenLocation);
  end;

FUNCTION arccos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION arccos_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(arccos(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(arccos(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(arccos_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('arccos',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=arccos_rec(params^.value(0))
    else raiseNotApplicableError('arccos',params,tokenLocation);
  end;

FUNCTION tan_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION tan_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(tan(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(tan(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(tan_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('tan',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=tan_rec(params^.value(0))
    else raiseNotApplicableError('tan',params,tokenLocation);
  end;

FUNCTION arctan_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION arctan_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(arctan(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(arctan(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(arctan_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('arctan',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=arctan_rec(params^.value(0))
    else raiseNotApplicableError('arctan',params,tokenLocation);
  end;

FUNCTION exp_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION exp_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(exp(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(exp(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(exp_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('exp',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=exp_rec(params^.value(0))
    else raiseNotApplicableError('exp',params,tokenLocation);
  end;

FUNCTION ln_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION ln_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : result:=newRealLiteral(ln(P_intLiteral (x)^.value));
        lt_real: result:=newRealLiteral(ln(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(ln_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('ln',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=ln_rec(params^.value(0))
    else raiseNotApplicableError('ln',params,tokenLocation);
  end;

FUNCTION round_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION round_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : begin result:=x; x^.rereference; end;
        lt_real: result:=newIntLiteral(round(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(round_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('round',x^.literalType,'',tokenLocation);
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
      result:=nil;
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
          else raiseNotApplicableError('round',y^.literalType,' (second parameter)',tokenLocation);
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
          end else raiseError(el3_evalError,'Incompatible list lengths given for built in function [round]',tokenLocation);
          else raiseNotApplicableError('round',y^.literalType,' (second parameter)',tokenLocation);
        end;
        else raiseNotApplicableError('round',x^.literalType,' (first parameter)',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=round_rec(params^.value(0)) else
    if (params<>nil) and (params^.size=2) then result:=round_rec2(params^.value(0),params^.value(1))
    else raiseNotApplicableError('round',params,tokenLocation);
  end;

FUNCTION ceil_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION ceil_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : begin result:=x; x^.rereference; end;
        lt_real: result:=newIntLiteral(ceil(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(ceil_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('ceil',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=ceil_rec(params^.value(0))
    else raiseNotApplicableError('ceil',params,tokenLocation);
  end;

FUNCTION floor_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION floor_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_error,lt_listWithError: begin result:=x; result^.rereference; end;
        lt_int : begin result:=x; x^.rereference; end;
        lt_real: result:=newIntLiteral(floor(P_realLiteral(x)^.value));
        lt_list,lt_intList,lt_realList,lt_numList: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do P_listLiteral(result)^.append(floor_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else raiseNotApplicableError('floor',x^.literalType,'',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=floor_rec(params^.value(0))
    else raiseNotApplicableError('floor',params,tokenLocation);
  end;

FUNCTION head_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION headOf(CONST x:P_literal):P_literal;
    begin
      result:=nil;
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList,lt_flatList] then begin
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
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList,lt_flatList] then begin
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

FUNCTION tail_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION tailOf(CONST x:P_literal):P_listLiteral;
    VAR i:longint;
    begin
      result:=nil;
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList,lt_flatList] then begin
        result:=newListLiteral;
        for i:=1 to P_listLiteral(x)^.size-1 do result^.append(P_listLiteral(x)^.value(i),true);
      end else raiseError(el3_evalError,'Function tail cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
    end;

  FUNCTION tailOf2(CONST x,y:P_literal):P_listLiteral;
    VAR i,i0:longint;
    begin
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList,lt_flatList] then begin
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
          else raiseError(el3_evalError,'Function tail cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
        end;
      end else raiseError(el3_evalError,'Function tail cannot be applied to type '+C_typeString[x^.literalType]+' (first parameter)',tokenLocation);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=tailOf(params^.value(0)) else
    if (params<>nil) and (params^.size=2) then result:=tailOf2(params^.value(0),params^.value(1))
    else raiseNotApplicableError('tail',params,tokenLocation);
  end;

FUNCTION sort_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (params^.value(0)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList,lt_flatList]) then begin
        result:=newListLiteral;
        P_listLiteral(result)^.appendAll(P_listLiteral(params^.value(0)));
        P_listLiteral(result)^.sort;
      end else if (params^.value(0)^.literalType=lt_list) and (P_listLiteral(params^.value(0))^.size=0) then begin
        result:=params^.value(0);
        result^.rereference;
      end else raiseError(el3_evalError,'Function sort can only be applied to flat lists containing comparable types',tokenLocation);
    end else raiseNotApplicableError('sort',params,tokenLocation);
  end;

FUNCTION sortPerm_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (params^.value(0)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList,lt_flatList]) then
        result:=P_listLiteral(params^.value(0))^.sortPerm
      else if (params^.value(0)^.literalType=lt_list) and (P_listLiteral(params^.value(0))^.size=0) then begin
        result:=params^.value(0);
        result^.rereference;
      end else raiseError(el3_evalError,'Function sortPerm can only be applied to flat lists containing comparable types',tokenLocation);
    end else raiseNotApplicableError('sortPerm',params,tokenLocation);
  end;

FUNCTION unique_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if (params^.value(0)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList,lt_flatList]) then begin
        result:=newListLiteral;
        P_listLiteral(result)^.appendAll(P_listLiteral(params^.value(0)));
        P_listLiteral(result)^.unique;
      end else if (params^.value(0)^.literalType=lt_list) and (P_listLiteral(params^.value(0))^.size=0) then begin
        result:=params^.value(0);
        result^.rereference;
      end else raiseError(el3_evalError,'Function unique can only be applied to flat lists containing comparable types',tokenLocation);
    end else raiseNotApplicableError('unique',params,tokenLocation);
  end;

FUNCTION flatten_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
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

FUNCTION random_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
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

PROCEDURE registerRule(CONST name:string; CONST ptr:T_intFuncCallback; CONST explanation:ansistring);
  begin
    intrinsicRuleMap.put(name,ptr);
    intrinsicRuleExplanationMap.put(name,explanation);
  end;

FUNCTION max_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
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

FUNCTION argMax_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
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

FUNCTION min_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
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

FUNCTION argMin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
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


FUNCTION size_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      case params^.value(0)^.literalType of
        lt_error..  lt_expression: result:=newIntLiteral(1);
        lt_list..lt_listWithError: result:=newIntLiteral(P_listLiteral(params^.value(0))^.size);
      end;
    end else raiseNotApplicableError('size',params,tokenLocation);
  end;

FUNCTION length_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
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

FUNCTION pos_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) and (params^.value(1)^.literalType=lt_string) then begin
      result:=newIntLiteral(pos(P_stringLiteral(params^.value(0))^.value,
                                P_stringLiteral(params^.value(1))^.value));
    end else raiseNotApplicableError('pos',params,tokenLocation);
  end;

FUNCTION copy_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (params^.value(0)^.literalType=lt_string)
                                          and (params^.value(1)^.literalType=lt_int)
                                          and (params^.value(2)^.literalType=lt_int) then begin
      result:=newStringLiteral(copy(P_stringLiteral(params^.value(0))^.value,
                                    P_intLiteral   (params^.value(1))^.value,
                                    P_intLiteral   (params^.value(2))^.value));
    end else raiseNotApplicableError('copy',params,tokenLocation);
  end;


FUNCTION time_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR res:P_literal;
      startTime,time:double;
      runCount:longint=1;

  PROCEDURE appendPair(VAR result:P_literal; CONST el0:string; CONST el1:P_literal);
    VAR aid:P_listLiteral;
    begin
      aid:=newListLiteral;
      aid^.append(newStringLiteral(el0),false);
      aid^.append(el1,false);
      P_listLiteral(result)^.append(aid,false);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_expression) then begin
      startTime:=now;
      res:=resolveNullaryCallback(P_expressionLiteral(params^.value(0))^.value,callDepth+1);
      time:=now-startTime;
      if res<>nil then begin
        while (time*24*60*60<0.5) do begin //measure at least half a second
          disposeLiteral(res);
          res:=resolveNullaryCallback(P_expressionLiteral(params^.value(0))^.value,callDepth+1);
          time:=now-startTime;
          inc(runCount);
        end;
        result:=newListLiteral;
        appendPair(result,'expression',newStringLiteral(params^.value(0)^.toString));
        appendPair(result,'result'    ,res);
        appendPair(result,'time'      ,newRealLiteral(round(time/runCount*(24*60*60*100000))*1E-5));
        appendPair(result,'samples'   ,newIntLiteral(runCount));
      end;
    end else raiseNotApplicableError('time',params,tokenLocation);
  end;

FUNCTION split_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
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

FUNCTION softCast_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
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

FUNCTION trim_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION trim_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
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

FUNCTION upper_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION upper_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.upper;
        lt_list,lt_stringList:  begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if errorLevel<el3_evalError then
            P_listLiteral(result)^.append(upper_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else result:=newErrorLiteralRaising('Cannot apply "upper" to literal of type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_string]) then result:=upper_rec(params^.value(0))
    else raiseNotApplicableError('upper',params,tokenLocation);
  end;

FUNCTION lower_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION lower_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.lower;
        lt_list,lt_stringList:  begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if errorLevel<el3_evalError then
            P_listLiteral(result)^.append(lower_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else result:=newErrorLiteralRaising('Cannot apply "lower" to literal of type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_string]) then result:=lower_rec(params^.value(0))
    else raiseNotApplicableError('lower',params,tokenLocation);
  end;

FUNCTION string_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if params^.value(0)^.literalType=lt_string then begin
        result:=params^.value(0);
        result^.rereference;
      end else result:=newStringLiteral(params^.value(0)^.toString);
    end else raiseNotApplicableError('string',params,tokenLocation);
  end;

FUNCTION expression_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  FUNCTION expression_rec(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      result:=nil;
      case x^.literalType of
        lt_string: result:=stringToExprCallback(P_stringLiteral(x)^.value,tokenLocation);
        lt_list,lt_stringList:  begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if errorLevel<el3_evalError then
            P_listLiteral(result)^.append(expression_rec(P_listLiteral(x)^.value(i)),false);
        end;
        else result:=newErrorLiteralRaising('Cannot apply "expression" to literal of type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_list,lt_stringList,lt_string]) then result:=expression_rec(params^.value(0))
    else raiseNotApplicableError('expression',params,tokenLocation);
  end;

FUNCTION filesOrDirs_impl(CONST pathOrPathList:P_literal; CONST filesAndNotFolders:boolean):P_listLiteral;
  VAR i,j:longint;
      found:T_stringList;
  begin
    result:=newListLiteral;
    if pathOrPathList^.literalType=lt_string then begin
      found:=find(P_stringLiteral(pathOrPathList)^.value,filesAndNotFolders);
      for i:=0 to length(found)-1 do result^.append(newStringLiteral(found[i]),false);
    end else if pathOrPathList^.literalType=lt_stringList then begin
      for j:=0 to P_listLiteral(pathOrPathList)^.size-1 do begin
        found:=find(P_stringLiteral(P_listLiteral(pathOrPathList)^.value(j))^.value,filesAndNotFolders);
        for i:=0 to length(found)-1 do result^.append(newStringLiteral(found[i]),false);
      end;

    end;
  end;

FUNCTION files_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_string, lt_stringList]) then begin
      result:=filesOrDirs_impl(params^.value(0),true);
    end else raiseNotApplicableError('files',params,tokenLocation);
  end;

FUNCTION folders_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_string, lt_stringList]) then begin
      result:=filesOrDirs_impl(params^.value(0),false);
    end else raiseNotApplicableError('folders',params,tokenLocation);
  end;

FUNCTION fileExists_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newBoolLiteral(FileExists(P_stringLiteral(params^.value(0))^.value));
    end else raiseNotApplicableError('fileExists',params,tokenLocation);
  end;

FUNCTION fileContents_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR accessed:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newStringLiteral(fileContent(P_stringLiteral(params^.value(0))^.value,accessed));
      if not(accessed) then raiseError(el2_warning,'File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('fileContents',params,tokenLocation);
  end;

FUNCTION fileLines_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR accessed:boolean;
      L:T_stringList;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      L:=fileLines(P_stringLiteral(params^.value(0))^.value,accessed);
      result:=newListLiteral;
      for i:=0 to length(L)-1 do P_listLiteral(result)^.append(newStringLiteral(L[i]),false);
      if not(accessed) then raiseError(el2_warning,'File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('fileLines',params,tokenLocation);
  end;

FUNCTION writeFile_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR ok:boolean;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string)
                                          and (params^.value(1)^.literalType=lt_string) then begin
      ok:=writeFile(P_stringLiteral(params^.value(0))^.value,
                    P_stringLiteral(params^.value(1))^.value);
      result:=newBoolLiteral(ok);
      if not(ok) then raiseError(el2_warning,'File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('writeFile',params,tokenLocation);
  end;

FUNCTION writeFileLines_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR ok:boolean;
      L:T_stringList;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string)
                                          and (params^.value(1)^.literalType=lt_stringList) then begin
      setLength(L,P_listLiteral(params^.value(1))^.size);
      for i:=0 to length(L)-1 do L[i]:=P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value;
      ok:=writeFileLines(P_stringLiteral(params^.value(0))^.value,L);
      result:=newBoolLiteral(ok);
      if not(ok) then raiseError(el2_warning,'File "'+P_stringLiteral(params^.value(0))^.value+'" cannot be accessed',tokenLocation);
    end else raiseNotApplicableError('writeFileLines',params,tokenLocation);
  end;

FUNCTION replaceOne_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR originalAndResult:ansistring;
      lookFor,replaceBy:P_literal;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (params^.value(0)^.literalType=lt_string) then begin
      originalAndResult :=P_stringLiteral(params^.value(0))^.value;
      lookFor  :=params^.value(1);
      replaceBy:=params^.value(2);
      if lookFor^.literalType=lt_string then begin
        if replaceBy^.literalType=lt_string then begin
          originalAndResult:=replaceOne(originalAndResult,
                                        P_stringLiteral(lookFor)^.value,
                                        P_stringLiteral(replaceBy)^.value);
        end else if replaceBy^.literalType=lt_stringList then begin
          for i:=0 to P_listLiteral(replaceBy)^.size-1 do
          originalAndResult:=replaceOne(originalAndResult,
                                        P_stringLiteral(lookFor)^.value,
                                        P_stringLiteral(P_listLiteral(replaceBy)^.value(i))^.value);
        end else raiseNotApplicableError('replaceOne',params,tokenLocation);
      end else if lookFor^.literalType=lt_stringList then begin
        if replaceBy^.literalType=lt_string then begin
          for i:=0 to P_listLiteral(lookFor)^.size-1 do
          originalAndResult:=replaceOne(originalAndResult,
                                        P_stringLiteral(P_listLiteral(lookFor)^.value(i))^.value,
                                        P_stringLiteral(replaceBy)^.value);
        end else if (replaceBy^.literalType=lt_stringList) and (P_listLiteral(lookFor)^.size=P_listLiteral(replaceBy)^.size) then begin
          for i:=0 to P_listLiteral(lookFor)^.size-1 do
          originalAndResult:=replaceOne(originalAndResult,
                                        P_stringLiteral(P_listLiteral(lookFor  )^.value(i))^.value,
                                        P_stringLiteral(P_listLiteral(replaceBy)^.value(i))^.value);
        end else raiseNotApplicableError('replaceOne',params,tokenLocation);
      end else raiseNotApplicableError('replaceOne',params,tokenLocation);
      result:=newStringLiteral(originalAndResult);
    end else raiseNotApplicableError('replaceOne',params,tokenLocation);
  end;

FUNCTION replace_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR originalAndResult:ansistring;
      lookFor,replaceBy:P_literal;
      i:longint;
      ok:boolean=true;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and (params^.value(0)^.literalType=lt_string) then begin
      originalAndResult :=P_stringLiteral(params^.value(0))^.value;
      lookFor  :=params^.value(1);
      replaceBy:=params^.value(2);
      if lookFor^.literalType=lt_string then begin
        if replaceBy^.literalType=lt_string then begin
          originalAndResult:=replaceRecursively(originalAndResult,
                                                P_stringLiteral(lookFor)^.value,
                                                P_stringLiteral(replaceBy)^.value,
                                                ok);
        end else if replaceBy^.literalType=lt_stringList then begin
          for i:=0 to P_listLiteral(replaceBy)^.size-1 do
          originalAndResult:=replaceRecursively(originalAndResult,
                                                P_stringLiteral(lookFor)^.value,
                                                P_stringLiteral(P_listLiteral(replaceBy)^.value(i))^.value,
                                                ok);
        end else raiseNotApplicableError('replace',params,tokenLocation);
      end else if lookFor^.literalType=lt_stringList then begin
        if replaceBy^.literalType=lt_string then begin
          for i:=0 to P_listLiteral(lookFor)^.size-1 do
          originalAndResult:=replaceRecursively(originalAndResult,
                                                P_stringLiteral(P_listLiteral(lookFor)^.value(i))^.value,
                                                P_stringLiteral(replaceBy)^.value,
                                                ok);
        end else if (replaceBy^.literalType=lt_stringList) and (P_listLiteral(lookFor)^.size=P_listLiteral(replaceBy)^.size) then begin
          for i:=0 to P_listLiteral(lookFor)^.size-1 do
          originalAndResult:=replaceRecursively(originalAndResult,
                                                P_stringLiteral(P_listLiteral(lookFor  )^.value(i))^.value,
                                                P_stringLiteral(P_listLiteral(replaceBy)^.value(i))^.value,
                                                ok);
        end else raiseNotApplicableError('replace',params,tokenLocation);
      end else raiseNotApplicableError('replace',params,tokenLocation);
      result:=newStringLiteral(originalAndResult);
    end else raiseNotApplicableError('replace',params,tokenLocation);
  end;

FUNCTION interpret_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR fileByScriptName,filename:ansistring;
      cmdLinePar:T_stringList;
      i:longint;
      output:TStringList;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string)
      and ((params^.size=1) or (params^.size=2) and (params^.value(1)^.literalType in [lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_flatList])) then begin
      filename        :=             P_stringLiteral(params^.value(0))^.value;
      fileByScriptName:=locateSource(P_stringLiteral(params^.value(0))^.value);
      if (fileByScriptName      <>'') and FileExists(fileByScriptName) then filename:=fileByScriptName;
      if (filename              <>'') and FileExists(filename) and
         (mnh_console_executable<>'') and FileExists(mnh_console_executable) then begin
        setLength(cmdLinePar,1);
        cmdLinePar[0]:=filename;
        if params^.size=2 then begin
          setLength(cmdLinePar,1+P_listLiteral(params^.value(1))^.size);
          for i:=0 to P_listLiteral(params^.value(1))^.size-1 do
            cmdLinePar[i+1]:=P_scalarLiteral(P_listLiteral(params^.value(1))^.value(i))^.stringForm;
        end;
        runCommand(mnh_console_executable,
                   cmdLinePar,
                   output);
        result:=newListLiteral;
        for i:=0 to output.Count-1 do P_listLiteral(result)^.append(newStringLiteral(output[i]),false);
        output.Free;
      end else begin
        raiseError(el1_note,'File or script "'+P_stringLiteral(params^.value(0))^.value+'" cannot be found.',tokenLocation);
        result:=newListLiteral;
      end;
    end else raiseNotApplicableError('interpret',params,tokenLocation);
  end;

FUNCTION execSync_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR executable:ansistring;
      cmdLinePar:T_stringList;
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
    end else raiseNotApplicableError('execSync',params,tokenLocation);
  end;

FUNCTION execAsync_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR executable:ansistring;
      cmdLinePar:T_stringList;
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

FUNCTION tokenSplit_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR stringToSplit:AnsiString;
      i0,i1:longint;

  PROCEDURE stepToken;
    begin
      P_listLiteral(result)^.append(newStringLiteral(copy(stringToSplit,i0,i1-i0)),false);
      i0:=i1;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      stringToSplit:=P_stringLiteral(params^.value(0))^.value;
      result:=newListLiteral;
      i0:=1;
      while i0<=length(stringToSplit) do begin
        case stringToSplit[i0] of
          'a'..'z','A'..'Z': begin //identifiers, keywords, etc.
            i1:=i0;
            while (i1<=length(stringToSplit)) and (stringToSplit[i1] in ['a'..'z','A'..'Z','_','0'..'9']) do inc(i1);
          end;
          '''','"': begin //strings
            unescapeString(copy(stringToSplit,i0,length(stringToSplit)-i0+1),i1);
            if i1<=0 then i1:=i0
                     else i1:=i0+i1;
          end;
          '0'..'9': begin //numbers
            parseNumber(copy(stringToSplit,i0,length(stringToSplit)-i0+1),false,i1);
            if i1<=0 then i1:=i0
                     else i1:=i0+i1;
          end;
          ' ',C_lineBreakChar,C_carriageReturnChar,C_tabChar: begin //whitespace
            i1:=i0;
            while (i1<=length(stringToSplit)) and (stringToSplit[i1] in [' ',C_lineBreakChar,C_carriageReturnChar,C_tabChar]) do inc(i1);
          end;
          else i1:=i0+1; //symbols, etc.
        end;
        stepToken;
      end;

    end else raiseNotApplicableError('tokenSplit',params,tokenLocation);
  end;

FUNCTION false_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=newBoolLiteral(false);
  end;

FUNCTION myPath_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    if (tokenLocation.provider=nil) or
       (tokenLocation.provider^.getPath='') then result:=newStringLiteral('<Unknown>')
                                            else result:=newStringLiteral(tokenLocation.provider^.getPath);
  end;


INITIALIZATION
  intrinsicRuleMap.create;
  intrinsicRuleExplanationMap.create;
  registerRule('not'           ,@not_imp       ,'not(b:boolean);#not(b:booleanList);#Returns the negated value of b#not(i:int);#not(i:intList);#Returns the bitwise negated value of i');
  registerRule('print'         ,@print_imp     ,'print(...);#Prints OUT the given parameters and returns true#if tabs and line breaks are part of the output, a default pretty-printing is used');
  registerRule('abs'           ,@abs_imp       ,'abs(n);#Returns the absolute value of numeric parameter n');
  registerRule('sign'          ,@sign_imp      ,'sign(n);#Returns the sign of numeric parameter n');
  registerRule('sqr'           ,@sqr_imp       ,'sqr(n);#Returns the square of numeric parameter n');
  registerRule('sqrt'          ,@sqrt_imp      ,'sqrt(n);#Returns the square root of numeric parameter n');
  registerRule('sin'           ,@sin_imp       ,'sin(n);#Returns the sine of numeric parameter n');
  registerRule('arcsin'        ,@arcsin_imp    ,'arcsin(n);#Returns the arcsine of numeric parameter n');
  registerRule('cos'           ,@cos_imp       ,'cos(n);#Returns the cosine of numeric parameter n');
  registerRule('arccos'        ,@arccos_imp    ,'arccos(n);#Returns the arccosine of numeric parameter n');
  registerRule('tan'           ,@tan_imp       ,'tan(n);#Returns the tangent of numeric parameter n');
  registerRule('arctan'        ,@arctan_imp    ,'tan(n);#Returns the arctangent of numeric parameter n');
  registerRule('exp'           ,@exp_imp       ,'exp(n);#Returns the exponential of numeric parameter n');
  registerRule('ln'            ,@ln_imp        ,'ln(n);#Returns the natural logarithm of numeric parameter n');
  registerRule('round'         ,@round_imp     ,'round(x);#Returns the value of x, rounded to the nearest integer#round(x,k);#Returns the value of x rounded to k-digits precision');
  registerRule('ceil'          ,@ceil_imp      ,'ceil(x);#Returns the smallest integer >=x');
  registerRule('floor'         ,@floor_imp     ,'floor(x);#Returns the largest integer <=x');
  registerRule('head'          ,@head_imp      ,'head(L);#Returns the first element of list L or [] if L is empty#head(L,k);#Returns the first min(k,size(L)) elements of L or [] if L is empty');
  registerRule('tail'          ,@tail_imp      ,'tail(L);#Returns list L without the first element#tail(L,k);#Returns L without the first k elements');
  registerRule('sort'          ,@sort_imp      ,'sort(L);#Returns list L sorted ascending (using fallbacks for uncomparable types)');
  registerRule('sortPerm'      ,@sortPerm_imp  ,'sortPerm(L);#Returns indexes I so that L%I==sort(L)');
  registerRule('unique'        ,@unique_imp    ,'unique(L);#Returns list L sorted ascending and without duplicates');
  registerRule('flatten'       ,@flatten_imp   ,'flatten(L,...);#Returns all parameters as a flat list.');
  registerRule('random'        ,@random_imp    ,'random;#Returns a random value in range [0,1]#random(n);Returns a list of n random values in range [0,1]');
  registerRule('max'           ,@max_imp       ,'max(L);#Returns the greatest element OUT of list L#max(x,y,...);#Returns the greatest element OUT of the given parameters');
  registerRule('argMax'        ,@argMax_imp    ,'argMax(L);#Returns the index of the greatest element OUT of list L (or the first index if ambiguous)');
  registerRule('min'           ,@min_imp       ,'min(L);#Returns the smallest element OUT of list L#min(x,y,...);#Returns the smallest element OUT of the given parameters');
  registerRule('argMin'        ,@argMin_imp    ,'argMin(L);#Returns the index of the smallest element OUT of list L (or the first index if ambiguous)');
  registerRule('size'          ,@size_imp      ,'size(L);#Returns the number of elements in list L');
  registerRule('length'        ,@length_imp    ,'length(S:string);#Returns the number of characters in string S');
  registerRule('pos'           ,@pos_imp       ,'pos(subString,searchInString);#Returns the index of the first occurence of subString in searchInString or 0 if there is none#Note: Character indexes start with 1!');
  registerRule('copy'          ,@copy_imp      ,'copy(S,start,length):#Returns the substring of S starting at index start and having specified length#Note: Character indexes start with 1!');
  registerRule('time'          ,@time_imp      ,'time(E:expression);#Evaluates E (without parameters) and returns a nested List with evaluation details.');
  registerRule('split'         ,@split_imp     ,'split(S:string;splitter:string);#Returns a list of strings obtained by splitting S at the specified splitters#The splitters themselves are not contained in the result');
  registerRule('softCast'      ,@softCast_imp  ,'softCast(X);#Returns a simplified version of X, trying to parse integers, real values and booleans');
  registerRule('trim'          ,@trim_imp      ,'trim(S:string);#Returns string S without leading or trailing spaces');
  registerRule('upper'         ,@upper_imp     ,'upper(S:string);#Returns an uppercase representation of S');
  registerRule('lower'         ,@lower_imp     ,'lower(S:string);#Returns an lowercase representation of S');
  registerRule('string'        ,@string_imp    ,'string(X);#Returns a string-representation of X');
  registerRule('expression'    ,@expression_imp   ,'expression(S:string);#Returns an expression parsed from S');
  registerRule('files'         ,@files_impl       ,'files(searchPattern:string);#Returns a list of files matching the given search pattern');
  registerRule('folders'       ,@folders_impl     ,'folders(searchPattern:string);#Returns a list of folders matching the given search pattern');
  registerRule('fileExists'    ,@fileExists_impl  ,'fileExists(filename:string);#Returns true if the specified file exists and false otherwise');
  registerRule('fileContents'  ,@fileContents_impl,'fileContents(filename:string);#Returns the contents of the specified file as one string');
  registerRule('fileLines'     ,@fileLines_impl   ,'fileLines(filename:string);#Returns the contents of the specified file as a list of strings#Information on the line breaks is lost');
  registerRule('writeFile'     ,@writeFile_impl,'writeFile(filename:string, content:string);#Writes the specified content to the specified file and returns true');
  registerRule('writeFileLines',@writeFileLines_impl,'writeFileLines(filename:string, content:stringList);#Writes the specified content to the specified file (using system-default line breaks) and returns true');
  registerRule('replaceOne'    ,@replaceOne_impl,'replaceOne(source:string,lookFor,replaceBy);#Replaces the first occurences of lookFor in source by replaceBy#lookFor and replaceBy may be of type string or stringList');
  registerRule('replace'       ,@replace_impl,'replace(source:string,lookFor,replaceBy);#Recursively replaces all occurences of lookFor in source by replaceBy#lookFor and replaceBy may be of type string or stringList');
  registerRule('interpret'     ,@interpret_impl,'interpret(filenameOrPackageId:string);#Evaluates the specified file and returns the text output');
  registerRule('execSync'      ,@execSync_impl,'execSync(programPath:string,parameters ...);#Executes the specified program and returns the text output');
  registerRule('execAsync'     ,@execAsync_impl,'execAsync(programPath:string,parameters ...);#Starts the specified program and returns true');
  registerRule('tokenSplit'    ,@tokenSplit_impl,'tokenSplit(S:string);#Returns a list of strings from S');
  registerRule('plotAvailable' ,@false_impl,'returns false (because plotting is not available)');
  registerRule('myPath',@myPath_impl,'returns the path to the current package');

FINALIZATION
  intrinsicRuleMap.destroy;
  intrinsicRuleExplanationMap.destroy;

end.
