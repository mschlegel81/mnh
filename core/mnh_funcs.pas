UNIT mnh_funcs;
INTERFACE
USES sysutils,mygenerics,mnh_constants,mnh_litvar,math,mnh_out_adapters;
TYPE
  T_intFuncCallback=FUNCTION(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
VAR
  intrinsicRuleMap    :specialize G_stringKeyMap<T_intFuncCallback>;
  intrinsicRuleAliases:specialize G_stringKeyMap<T_intFuncCallback>;
IMPLEMENTATION
FUNCTION print_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
    outAdapter.writePrint(stringToPrint);
    result:=newBoolLiteral(true);
  end;

FUNCTION sqr_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function sqr cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=sqr_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function sqr cannot be applied to parameters '+params^.toString,tokenLocation);
  end;
  
FUNCTION sqrt_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function sqrt cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=sqrt_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function sqrt cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION sin_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function sin cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=sin_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function sin cannot be applied to parameters '+params^.toString,tokenLocation);
  end;
  
FUNCTION arcsin_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function arcsin cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=arcsin_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function arcsin cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION cos_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function cos cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=cos_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function cos cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION arccos_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function arccos cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=arccos_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function arccos cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION tan_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function tan cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=tan_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function tan cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION arctan_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function arctan cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=arctan_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function arctan cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION exp_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function exp cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=exp_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function exp cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION ln_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function ln cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=ln_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function ln cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION round_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function round cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
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
          else outAdapter.raiseError(el3_evalError,'Function round cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
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
          end else outAdapter.raiseError(el3_evalError,'Incompatible list lengths given for function round',tokenLocation);
          else outAdapter.raiseError(el3_evalError,'Function round cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
        end;
        else outAdapter.raiseError(el3_evalError,'Function round cannot be applied to type '+C_typeString[x^.literalType]+' (first parameter)',tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) then result:=round_rec(params^.value(0)) else
    if (params^.size=2) then result:=round_rec2(params^.value(0),params^.value(1))
    else outAdapter.raiseError(el3_evalError,'Function round cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION ceil_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function ceil cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=ceil_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function ceil cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION floor_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;
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
        else outAdapter.raiseError(el3_evalError,'Function floor cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
      end;
    end;

  begin
    result:=nil;
    if (params^.size=1) 
    then result:=floor_rec(params^.value(0))
    else outAdapter.raiseError(el3_evalError,'Function floor cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION head_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;    
  FUNCTION headOf(CONST x:P_literal):P_literal;
    begin
      result:=nil;
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList] then begin
        result:=P_listLiteral(x)^.value(0);
        result^.rereference;
      end else outAdapter.raiseError(el3_evalError,'Function head cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
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
          else outAdapter.raiseError(el3_evalError,'Function head cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
        end;
      end else outAdapter.raiseError(el3_evalError,'Function head cannot be applied to type '+C_typeString[x^.literalType]+' (first parameter)',tokenLocation);
    end;

  begin
    result:=nil;
    if (params^.size=1) then result:=headOf(params^.value(0)) else
    if (params^.size=2) then result:=headOf2(params^.value(0),params^.value(1))
    else outAdapter.raiseError(el3_evalError,'Function head cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

FUNCTION tail_imp(CONST params:P_listLiteral; CONST tokenLocation:ansistring):P_literal;    
  FUNCTION tailOf(CONST x:P_literal):P_listLiteral;
    VAR i:longint;
    begin
      result:=nil;
      if x^.literalType in [lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList] then begin
        result:=newListLiteral;
        for i:=1 to P_listLiteral(x)^.size-1 do result^.append(P_listLiteral(x)^.value(i),true);
      end else outAdapter.raiseError(el3_evalError,'Function tail cannot be applied to type '+C_typeString[x^.literalType],tokenLocation);
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
          else outAdapter.raiseError(el3_evalError,'Function tail cannot be applied to type '+C_typeString[y^.literalType]+' (second parameter)',tokenLocation);
        end;
      end else outAdapter.raiseError(el3_evalError,'Function tail cannot be applied to type '+C_typeString[x^.literalType]+' (first parameter)',tokenLocation);
    end;

  begin
    result:=nil;
    if (params^.size=1) then result:=tailOf(params^.value(0)) else
    if (params^.size=2) then result:=tailOf2(params^.value(0),params^.value(1))
    else outAdapter.raiseError(el3_evalError,'Function tail cannot be applied to parameters '+params^.toString,tokenLocation);
  end;

PROCEDURE regRule(CONST name:string; CONST ptr:T_intFuncCallback);
  begin
    intrinsicRuleAliases.put(name,ptr);
    intrinsicRuleMap.put(uppercase(name),ptr);
  end;
  
INITIALIZATION
  intrinsicRuleMap.create;
  intrinsicRuleAliases.create;
  regRule('print' ,@print_imp );
  regRule('sqr'   ,@sqr_imp   );
  regRule('sqrt'  ,@sqrt_imp  );
  regRule('sin'   ,@sin_imp   );
  regRule('arcsin',@arcsin_imp);
  regRule('cos'   ,@cos_imp   );
  regRule('arccos',@arccos_imp);
  regRule('tan'   ,@tan_imp   );
  regRule('arctan',@arctan_imp);
  regRule('exp'   ,@exp_imp   );
  regRule('ln'    ,@ln_imp    );
  regRule('round' ,@round_imp );
  regRule('ceil'  ,@ceil_imp  );
  regRule('floor' ,@floor_imp );
  regRule('head'  ,@head_imp  );
  regRule('tail'  ,@tail_imp  );
  
FINALIZATION
  intrinsicRuleMap.destroy;
  intrinsicRuleAliases.destroy;
end.