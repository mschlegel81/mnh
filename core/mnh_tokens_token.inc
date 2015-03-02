{$ifdef include_interface}
  P_token=^T_token;
  T_token=object
    location:ansistring;
    txt     :ansistring;
    tokType :T_tokenType;
    data    :pointer;
    next    :P_Token;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE define(CONST tokenLocation,tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer);
    PROCEDURE define(CONST tokenLocation,tokenText:ansistring; CONST tokenType:T_tokenType);
    PROCEDURE define(CONST original:P_token);
    PROCEDURE undefine;
    FUNCTION last:P_token;        
    FUNCTION toString:ansistring;
    FUNCTION getDeclarationOrAssignmentToken:P_token;
  end;
{$endif}
{$ifdef include_implementation}
VAR tokenRecycling:record
      dat:array[0..1023] of P_token;
      fill:longint;
    end;
    
FUNCTION disposeToken(p:P_token):P_token;
  begin
    if p=nil then exit(nil);
    result:=p^.next;
    p^.undefine;
    with tokenRecycling do 
    if fill>=length(dat) then dispose(p,destroy) 
                         else begin
                                dat[fill]:=p; 
                                inc(fill);
                              end;
  end;
  
PROCEDURE cascadeDisposeToken(VAR p:P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;
   
FUNCTION newToken(CONST tokenLocation,tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer):P_token;
  begin
    with tokenRecycling do if fill>0 then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION newToken(CONST tokenLocation,tokenText:ansistring; CONST tokenType:T_tokenType):P_token;
  begin
    with tokenRecycling do if fill>0 then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);    
    result^.define(tokenLocation,tokenText,tokenType);
    result^.next:=nil;
  end;

FUNCTION newToken(CONST original:P_token):P_token;
  begin
    with tokenRecycling do if fill>0 then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end;
  
PROCEDURE finalizeTokens;
  begin
    with tokenRecycling do while fill>0 do begin
      dec(fill);
      dispose(dat[fill],destroy);      
    end;
  end;
  
FUNCTION tokensToString(CONST first:P_token):ansistring;
  VAR p:P_token;
  begin
    result:='';
    p:=first;
    while p<>nil do begin
      result:=result+p^.toString;
      p:=p^.next;
    end;
  end;
  
CONSTRUCTOR T_token.create;
  begin
  end;

DESTRUCTOR T_token.destroy;
  begin
    undefine;
  end;

PROCEDURE T_token.define(CONST tokenLocation,tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer);
  begin
    location:=tokenLocation;    
    if (tokenText='') or (C_tokenString[tokenType]<>'') 
      then txt:=C_tokenString[tokenType]
      else txt:=tokenText;    
    tokType:=tokenType;
    data:=ptr;
  end;

PROCEDURE T_token.define(CONST tokenLocation,tokenText:ansistring; CONST tokenType:T_tokenType);
  begin
    location:=tokenLocation;
    if tokenText='' then txt:=C_tokenString[tokenType]
                    else txt:=tokenText;    
    tokType:=tokenType;
    data:=nil;
  end;

PROCEDURE T_token.define(CONST original:P_token);
  begin
    location:=original^.location;
    txt    :=original^.txt;
    tokType:=original^.tokType;
    data   :=original^.data;
    if (tokType=tt_literal) or (tokType=tt_each) and (data<>nil) then P_literal(data)^.rereference;
  end;
  
PROCEDURE T_Token.undefine;
  begin
    case tokType of
      tt_literal,tt_list_constructor,tt_parList_constructor,tt_parList: disposeLiteral(data);
      tt_each: if data<>nil then disposeLiteral(data);
      else data:=nil;
    end;
    tokType:=tt_eol;  
  end;

  
FUNCTION T_token.last:P_token;        
  begin
    result:=@self;
    while result^.next<>nil do result:=result^.next;
  end;
  
FUNCTION T_token.toString:ansistring;  
  begin
    case tokType of
      tt_each: begin
        result:='each';
        if txt<>'' then result:=result+'('+txt+',';
        if data<>nil then result:=result+P_literal(data)^.toString+',';
      end;
      tt_set: begin
        result:='set';
        if data<>nil then result:=result+'('+P_rule(data)^.id+',';
      end;      
      tt_braceOpen    ,tt_braceClose    ,
      tt_listBraceOpen,tt_listBraceClose,
      tt_expBraceOpen ,tt_expBraceClose,
      tt_separatorComma, tt_separatorCnt,
      tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt,
      tt_operatorAnd, tt_operatorOr, tt_operatorXor,
      tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
      tt_operatorStrConcat,
      tt_operatorExtractL0, tt_operatorExtractL1, tt_operatorExtractL2, tt_operatorExtractL3,
      tt_operatorConcat, tt_operatorIn,
      tt_iifCheck, tt_iifElse,
      tt_declare, tt_assign,
      tt_typeCheckScalar,  tt_typeCheckList,
      tt_typeCheckBoolean, tt_typeCheckBoolList,
      tt_typeCheckInt,     tt_typeCheckIntList,
      tt_typeCheckReal,    tt_typeCheckRealList,
      tt_typeCheckString,  tt_typeCheckStringList,
      tt_typeCheckNumeric, tt_typeCheckNumList,
      tt_typeCheckExpression,tt_typeCheckNonemptyList,tt_typeCheckEmptyList: result:=C_tokenString[tokType];
      tt_identifier, tt_userRulePointer, tt_intrinsicRulePointer, tt_parameterIdentifier:result:=txt;
      tt_literal            : result:=P_literal(data)^.toString;
      tt_parList_constructor: result:=P_listLiteral(data)^.toParameterListString(false);
      tt_parList            : result:=P_listLiteral(data)^.toParameterListString(true);
      tt_list_constructor   : result:=P_literal(data)^.toString;
    end;    
  end;
  
FUNCTION T_token.getDeclarationOrAssignmentToken:P_token;
  begin
    if tokType in [tt_declare,tt_assign] then result:=@self
    else if next<>nil then result:=next^.getDeclarationOrAssignmentToken
    else result:=nil;
  end;



{$endif}
