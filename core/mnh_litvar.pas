UNIT mnh_litvar;
INTERFACE
USES mnh_constants,mnh_out_adapters,sysutils,math,mnh_stringUtil, mnh_tokloc;

CONST
  C_boolText:array[false..true] of string=('false','true');

TYPE
  PP_literal=^P_literal;
  P_literal=^T_literal;
  T_literal=object
    private
      numberOfReferences:longint;
    public
      CONSTRUCTOR init;
      DESTRUCTOR destroy; virtual;
      PROCEDURE rereference;
      FUNCTION unreference:longint;
      FUNCTION literalType:T_literalType; virtual;
      FUNCTION toString:ansistring;  virtual;
      FUNCTION negate(CONST minusLocation:T_tokenLocation):P_literal; virtual;
      FUNCTION hash:longint; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
  end;

  P_scalarLiteral=^T_scalarLiteral;

  { T_scalarLiteral }

  T_scalarLiteral=object(T_literal)
    FUNCTION literalType:T_literalType; virtual;
    FUNCTION toString:ansistring;  virtual;
    FUNCTION stringForm:ansistring; virtual;
    FUNCTION isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;  virtual;
    FUNCTION leqForSorting(CONST other:P_scalarLiteral):boolean; virtual;
    FUNCTION operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; virtual;
    FUNCTION negate(CONST minusLocation:T_tokenLocation):P_literal; virtual;
    FUNCTION hash:longint; virtual;
    FUNCTION equals(CONST other:P_literal):boolean; virtual;
  end;

  P_boolLiteral=^T_boolLiteral;
  T_boolLiteral=object(T_scalarLiteral)
    private
      val:boolean;
    public
      CONSTRUCTOR create(CONST value:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION literalType:T_literalType; virtual;
      FUNCTION toString:ansistring;  virtual;
      FUNCTION stringForm:ansistring; virtual;
      FUNCTION isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;  virtual;
      FUNCTION leqForSorting(CONST other:P_scalarLiteral):boolean; virtual;
      FUNCTION operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; virtual;
      FUNCTION value:boolean;
      FUNCTION negate(CONST minusLocation:T_tokenLocation):P_literal; virtual;
      FUNCTION hash:longint; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
  end;

  P_intLiteral=^T_intLiteral;
  T_intLiteral=object(T_scalarLiteral)
    private
      val:int64;
    public
      CONSTRUCTOR create(CONST value:int64);
      DESTRUCTOR destroy; virtual;
      FUNCTION literalType:T_literalType; virtual;
      FUNCTION toString:ansistring;  virtual;
      FUNCTION stringForm:ansistring; virtual;
      FUNCTION isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;  virtual;
      FUNCTION leqForSorting(CONST other:P_scalarLiteral):boolean; virtual;
      FUNCTION operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; virtual;
      FUNCTION value:int64;
      FUNCTION negate(CONST minusLocation:T_tokenLocation):P_literal; virtual;
      FUNCTION hash:longint; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
  end;

  P_realLiteral=^T_realLiteral;
  T_realLiteral=object(T_scalarLiteral)
    private
      val:extended;
    public
      CONSTRUCTOR create(CONST value:extended);
      DESTRUCTOR destroy; virtual;
      FUNCTION literalType:T_literalType; virtual;
      FUNCTION toString:ansistring;  virtual;
      FUNCTION stringForm:ansistring; virtual;
      FUNCTION isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;  virtual;
      FUNCTION leqForSorting(CONST other:P_scalarLiteral):boolean; virtual;
      FUNCTION operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; virtual;
      FUNCTION value:extended;
      FUNCTION negate(CONST minusLocation:T_tokenLocation):P_literal; virtual;
      FUNCTION hash:longint; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
  end;

  P_stringLiteral=^T_stringLiteral;
  T_stringLiteral=object(T_scalarLiteral)
    private
      val:ansistring;
    public
      CONSTRUCTOR create(CONST value:ansistring);
      DESTRUCTOR destroy; virtual;
      FUNCTION literalType:T_literalType; virtual;
      FUNCTION toString:ansistring;  virtual;
      FUNCTION stringForm:ansistring; virtual;
      FUNCTION isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;  virtual;
      FUNCTION leqForSorting(CONST other:P_scalarLiteral):boolean; virtual;
      FUNCTION operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; virtual;
      FUNCTION value:ansistring;
      FUNCTION negate(CONST minusLocation:T_tokenLocation):P_literal; virtual;
      FUNCTION softCast:P_scalarLiteral;
      FUNCTION trim:P_stringLiteral;
      FUNCTION upper:P_stringLiteral;
      FUNCTION lower:P_stringLiteral;
      FUNCTION hash:longint; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
  end;

  P_expressionLiteral=^T_expressionLiteral;
  T_expressionLiteral=object(T_scalarLiteral)
    private
      val:pointer;
    public
      CONSTRUCTOR create(CONST value:pointer);
      DESTRUCTOR destroy; virtual;
      FUNCTION literalType:T_literalType; virtual;
      FUNCTION toString:ansistring;  virtual;
      FUNCTION stringForm:ansistring; virtual;
      FUNCTION isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;  virtual;
      FUNCTION leqForSorting(CONST other:P_scalarLiteral):boolean; virtual;
      FUNCTION operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; virtual;
      FUNCTION value:pointer;
      FUNCTION negate(CONST minusLocation:T_tokenLocation):P_literal; virtual;
      FUNCTION hash:longint; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
  end;

  P_listLiteral=^T_listLiteral;
  T_listLiteral=object(T_literal)
    private
      strictType:T_literalType;
      element:array of P_literal;
      nextAppendIsRange:boolean;
    public
      CONSTRUCTOR create;
      PROCEDURE destroyChildren;
      DESTRUCTOR destroy; virtual;
      FUNCTION literalType:T_literalType; virtual;
      FUNCTION toString:ansistring;  virtual;
      FUNCTION toParameterListString(CONST isFinalized:boolean):ansistring;
      PROCEDURE append(CONST L:P_literal; CONST incRefs:boolean);
      PROCEDURE appendAll(CONST L:P_listLiteral);
      PROCEDURE appendConstructing(CONST L:P_literal; CONST tokenLocation:T_tokenLocation);
      PROCEDURE setRangeAppend;
      FUNCTION size:longint;
      FUNCTION value(index:longint):P_literal;
      FUNCTION negate(CONST minusLocation:T_tokenLocation):P_literal; virtual;
      PROCEDURE sort;
      FUNCTION sortPerm:P_listLiteral;
      PROCEDURE unique;
      FUNCTION hash:longint; virtual;
      FUNCTION equals(CONST other:P_literal):boolean; virtual;
  end;

TYPE
  T_disposeSubruleCallback =PROCEDURE(VAR p:pointer);  
  T_subruleApplyOpCallback =FUNCTION(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation):pointer;
  T_pointerToStringCallback=FUNCTION(CONST p:pointer):string;

VAR
  disposeSubruleCallback :T_disposeSubruleCallback;
  subruleToStringCallback:T_pointerToStringCallback;
  subruleApplyOpCallback :T_subruleApplyOpCallback;

PROCEDURE disposeLiteral     (VAR l:P_literal); inline;
FUNCTION newBoolLiteral      (CONST value:boolean   ):P_boolLiteral; inline;
FUNCTION newIntLiteral       (CONST value:int64     ):P_intLiteral; inline;
FUNCTION newRealLiteral      (CONST value:extended  ):P_realLiteral; inline;
FUNCTION newStringLiteral    (CONST value:ansistring):P_stringLiteral; inline;
FUNCTION newExpressionLiteral(CONST value:pointer   ):P_expressionLiteral; inline;
FUNCTION newListLiteral:P_listLiteral; inline;
FUNCTION newOneElementListLiteral(CONST value:P_literal; CONST incRefs:boolean):P_listLiteral; inline;
FUNCTION newErrorLiteral:P_scalarLiteral; inline;
FUNCTION newErrorLiteralRaising(CONST errorMessage:ansistring; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; inline;
FUNCTION newErrorLiteralRaising(CONST x,y:T_literalType; CONST op:T_tokenType; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; inline;
FUNCTION resolveOperator(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation):P_literal; inline;
FUNCTION parseNumber(CONST input:ansistring; CONST suppressOutput:boolean; OUT parsedLength:longint):P_scalarLiteral; inline;
IMPLEMENTATION

VAR boolLit:array[false..true] of T_boolLiteral;
    intLit :array[0..127] of P_intLiteral;
    errLit :T_scalarLiteral;

PROCEDURE disposeLiteral(VAR l:P_literal);
  begin
    if l=nil then begin
      writeln(stderr,'disposing NIL literal ?!?');
    end;
    if l^.unreference<=0 then
        dispose(l,destroy);
    l:=nil;
  end;

FUNCTION newBoolLiteral(CONST value:boolean   ):P_boolLiteral; 
  begin
    result:=@boolLit[value];
    result^.rereference;
  end;

FUNCTION newIntLiteral(CONST value:int64     ):P_intLiteral; 
  begin
    if (value>=0) and (value<length(intLit)) then begin
      if intLit[value]=nil then new(intLit[value],create(value));
      result:=intLit[value];
      result^.rereference;
    end else begin
      new(result,create(value));
      isMemoryFree('allocating new integer literal');
    end;
  end;

FUNCTION newRealLiteral(CONST value:extended  ):P_realLiteral; 
  begin
    new(result,create(value));
    isMemoryFree('allocating new real literal');
  end;

FUNCTION newStringLiteral    (CONST value:ansistring):P_stringLiteral; 
  begin
    new(result,create(value));
    isMemoryFree('allocating new string literal');
  end;

FUNCTION newExpressionLiteral(CONST value:pointer   ):P_expressionLiteral; 
  begin
    new(result,create(value));
    isMemoryFree('allocating new expression literal');
  end;

FUNCTION newListLiteral:P_listLiteral; 
  begin
    new(result,create);
    isMemoryFree('allocating new list literal');
  end;

FUNCTION newOneElementListLiteral(CONST value:P_literal; CONST incRefs:boolean):P_listLiteral; 
  begin
    result:=newListLiteral;
    result^.append(value,incRefs);
    isMemoryFree('allocating new one-element-list literal');
  end;

FUNCTION newErrorLiteral:P_scalarLiteral;
  begin
    result:=@errLit;
    errLit.rereference;
  end;

FUNCTION newErrorLiteralRaising(CONST errorMessage:ansistring; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; 
  begin
    result:=@errLit;
    errLit.rereference;
    raiseError(el2_warning,errorMessage,tokenLocation);
  end;

FUNCTION newErrorLiteralRaising(CONST x,y:T_literalType; CONST op:T_tokenType; CONST tokenLocation:T_tokenLocation):P_scalarLiteral; 
  begin
    result:=@errLit;
    errLit.rereference;
    raiseError(el2_warning,'Operator '+C_tokenString[op]+' is not supported for types '+C_typeString[x]+' and '+C_typeString[y],tokenLocation);
  end;

FUNCTION myFloatToStr(CONST x:extended):string;
  begin
    result:=FloatToStr(x);
    if (pos('E',UpperCase(result))<=0) and //occurs in exponents
       (pos('N',UpperCase(result))<=0) and //occurs in "Nan or Inf"
       (pos('.',          result )<=0) then result:=result+'.0';
  end;

FUNCTION parseNumber(CONST input:ansistring; CONST suppressOutput:boolean; OUT parsedLength:longint):P_scalarLiteral;
  VAR i:longint;
  begin
    result:=nil;
    parsedLength:=0;
    if (length(input)>=1) and (input[1] in ['0'..'9','-','+']) then begin
      i:=1;
      while (i<length(input)) and (input[i+1] in ['0'..'9']) do inc(i);
      parsedLength:=i;
      //Only digits on indexes [1..i]; accept decimal point and following digts
      if (i<length(input)) and (input[i+1]='.') then begin
        inc(i);
        if (i<length(input)) and (input[i+1]='.') then dec(i);
      end;
      while (i<length(input)) and (input[i+1] in ['0'..'9']) do inc(i);
      //Accept exponent marker and following exponent
      if (i<length(input)) and (input[i+1] in ['e','E']) then begin
        inc(i);
        if (i<length(input)) and (input[i+1] in ['+','-']) then inc(i);
      end;
      while (i<length(input)) and (input[i+1] in ['0'..'9']) do inc(i);
      if i>parsedLength then begin
        parsedLength:=i;
        if suppressOutput then exit(nil);
        result:=newRealLiteral(StrToFloatDef(copy(input,1,parsedLength),NAN));
      end else begin
        if suppressOutput then exit(nil);
        result:=newIntLiteral(StrToInt64Def(copy(input,1,parsedLength),0));
      end;
    end;
  end;

//=====================================================================================================================
CONSTRUCTOR T_literal.init;                                  begin numberOfReferences:=1; end;
CONSTRUCTOR T_boolLiteral.create(CONST value:boolean);       begin inherited init; val:=value; end;
CONSTRUCTOR T_intLiteral.create(CONST value:int64);          begin inherited init; val:=value; end;
CONSTRUCTOR T_realLiteral.create(CONST value:extended);      begin inherited init; val:=value; end;
CONSTRUCTOR T_stringLiteral.create(CONST value:ansistring);  begin inherited init; val:=value; end;
CONSTRUCTOR T_expressionLiteral.create(CONST value:pointer); begin inherited init; val:=value; end;
CONSTRUCTOR T_listLiteral.create;                            begin inherited init; setLength(element,0); strictType:=lt_uncheckedList; nextAppendIsRange:=false; end;

DESTRUCTOR T_literal.destroy;            begin end;
DESTRUCTOR T_boolLiteral.destroy;        begin end;
DESTRUCTOR T_intLiteral.destroy;         begin end;
DESTRUCTOR T_realLiteral.destroy;        begin end;
DESTRUCTOR T_stringLiteral.destroy;      begin end;
DESTRUCTOR T_expressionLiteral.destroy;  begin disposeSubruleCallback(val); end;
PROCEDURE T_listLiteral.destroyChildren;
  VAR i:longint;
  begin
    for i:=0 to length(element)-1 do if element[i]<>nil then disposeLiteral(element[i]);
    setLength(element,0);
    strictType:=lt_uncheckedList;
    nextAppendIsRange:=false;
  end;

DESTRUCTOR T_listLiteral.destroy;
  begin
    destroyChildren;
  end;

PROCEDURE T_literal.rereference;         begin InterLockedIncrement(numberOfReferences); end;
FUNCTION T_literal.unreference:longint;  begin InterLockedDecrement(numberOfReferences); result:=numberOfReferences; end;

FUNCTION T_literal.literalType:T_literalType;           begin result:=lt_error;      end;
FUNCTION T_scalarLiteral.literalType: T_literalType;     begin result:=lt_error;      end;
FUNCTION T_boolLiteral.literalType:T_literalType;       begin result:=lt_boolean;    end;
FUNCTION T_intLiteral.literalType:T_literalType;        begin result:=lt_int;        end;
FUNCTION T_realLiteral.literalType:T_literalType;       begin result:=lt_real;       end;
FUNCTION T_stringLiteral.literalType:T_literalType;     begin result:=lt_string;     end;
FUNCTION T_expressionLiteral.literalType:T_literalType; begin result:=lt_expression; end;
FUNCTION T_listLiteral.literalType:T_literalType;
  VAR i:longint;
      containsError:boolean=false;
      allInt :boolean=true;
      allReal:boolean=true;
      allNum :boolean=true;
      allBool:boolean=true;
      allStr :boolean=true;
      allScalar:boolean=true;
  begin
    if strictType<>lt_uncheckedList then exit(strictType);
    if length(element)>0 then begin
      for i:=0 to length(element)-1 do if element[i]=nil
      then containsError:=true
      else begin
        case element[i]^.literalType of
          lt_error, lt_listWithError: containsError:=true;
          lt_boolean          : begin                   allInt:=false; allReal:=false; allNum:=false;                 allStr:=false; end;
          lt_int              : begin                                  allReal:=false;                allBool:=false; allStr:=false; end;
          lt_real             : begin                   allInt:=false;                                allBool:=false; allStr:=false; end;
          lt_string           : begin                   allInt:=false; allReal:=false; allNum:=false; allBool:=false;                end;
          lt_list..lt_flatList: begin allScalar:=false; allInt:=false; allReal:=false; allNum:=false; allBool:=false; allStr:=false; end;
          else                  begin                   allInt:=false; allReal:=false; allNum:=false; allBool:=false; allStr:=false; end;
        end;
      end;
      if containsError  then strictType:=lt_listWithError
      else if allInt    then strictType:=lt_intList
      else if allReal   then strictType:=lt_realList
      else if allNum    then strictType:=lt_numList
      else if allBool   then strictType:=lt_booleanList
      else if allStr    then strictType:=lt_stringList
      else if allScalar then strictType:=lt_flatList
      else                   strictType:=lt_list;
    end else begin
      strictType:=lt_list;
    end;
    result:=strictType;
  end;

FUNCTION T_intLiteral.value:int64;          begin result:=val; end;
FUNCTION T_realLiteral.value:extended;      begin result:=val; end;
FUNCTION T_stringLiteral.value:ansistring;  begin result:=val; end;
FUNCTION T_boolLiteral.value:boolean;       begin result:=val; end;
FUNCTION T_expressionLiteral.value:pointer; begin result:=val; end;
FUNCTION T_listLiteral.size:longint;        begin result:=length(element); end;
FUNCTION T_listLiteral.value(index:longint):P_literal;    begin result:=element[index]; end;


FUNCTION T_literal.toString:ansistring;           begin result:='<ERR>'; end;
FUNCTION T_scalarLiteral.toString: ansistring;     begin result:='<ERR>'; end;
FUNCTION T_boolLiteral.toString:ansistring;       begin result:=C_boolText[val]; end;
FUNCTION T_intLiteral.toString:ansistring;        begin result:=intToStr(val); end;
FUNCTION T_realLiteral.toString:ansistring;       begin result:=myFloatToStr(val); end;
FUNCTION T_stringLiteral.toString:ansistring;     begin result:=escapeString(val); end;
FUNCTION T_expressionLiteral.toString:ansistring; begin result:=subruleToStringCallback(val); end;
FUNCTION T_listLiteral.toString:ansistring;
  VAR i:longint;
  begin
    if length(element)=0 then result:='[]'
    else begin
      result:='['+element[0]^.toString;
      for i:=1 to length(element)-1 do result:=result+','+element[i]^.toString;
      result:=result+']';
    end;
  end;

FUNCTION T_listLiteral.toParameterListString(CONST isFinalized:boolean):ansistring;
  VAR i:longint;
  begin
    if length(element)=0 then begin
      if isFinalized then result:='()'
                     else result:='(  ';
    end else begin
      result:='('+element[0]^.toString;
      for i:=1 to length(element)-1 do result:=result+','+element[i]^.toString;
      if isFinalized then result:=result+')'
                     else result:=result+',  ';
    end;
  end;

FUNCTION T_scalarLiteral.stringForm: ansistring;     begin result:=toString; end;
FUNCTION T_boolLiteral.stringForm:ansistring;       begin result:=toString; end;
FUNCTION T_intLiteral.stringForm:ansistring;        begin result:=toString; end;
FUNCTION T_realLiteral.stringForm:ansistring;       begin result:=toString; end;
FUNCTION T_stringLiteral.stringForm:ansistring;     begin result:=val;      end;
FUNCTION T_expressionLiteral.stringForm:ansistring; begin result:=toString; end;

FUNCTION T_scalarLiteral.isInRelationTo(CONST relation: T_tokenType; CONST other: P_scalarLiteral): boolean;
  begin result:=false; end;
FUNCTION T_scalarLiteral.leqForSorting(CONST other:P_scalarLiteral):boolean;
  begin
    result:=literalType<=other^.literalType;
  end;

FUNCTION T_boolLiteral.isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;
  VAR ovl:boolean;
  begin
    if other^.literalType<>lt_boolean then exit(false);
    ovl:=P_boolLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorLeq, tt_comparatorGeq]) or
            (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
            (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_boolLiteral.leqForSorting(CONST other:P_scalarLiteral):boolean;
  begin
    if other^.literalType=lt_boolean
    then result:=isInRelationTo(tt_comparatorLeq,other)
    else result:=(literalType<=other^.literalType);
  end;

FUNCTION T_intLiteral.isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;
  VAR ovi:int64;
      ovr:extended;
  begin
    case other^.literalType of
      lt_int: begin
        ovi:=P_intLiteral(other)^.val;
        result:=(val=ovi) and (relation in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorLeq, tt_comparatorGeq]) or
                (val<ovi) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
                (val>ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        ovr:=P_realLiteral(other)^.val;
        result:=(val=ovr) and (relation in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorLeq, tt_comparatorGeq]) or
                (val<ovr) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
                (val>ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result:=false;
    end;
  end;

FUNCTION T_intLiteral.leqForSorting(CONST other:P_scalarLiteral):boolean;
  begin
    if (other^.literalType in [lt_int,lt_real])
    then result:=isInRelationTo(tt_comparatorLeq,other)
    else result:=(literalType<=other^.literalType);
  end;

FUNCTION T_realLiteral.isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;
  VAR ovi:int64;
      ovr:extended;
  begin
    case other^.literalType of
      lt_int: begin
        ovi:=P_intLiteral(other)^.val;
        result:=(val=ovi) and (relation in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorLeq, tt_comparatorGeq]) or
                (val<ovi) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
                (val>ovi) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      lt_real: begin
        ovr:=P_realLiteral(other)^.val;
        result:=(val=ovr) and (relation in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorLeq, tt_comparatorGeq]) or
                (val<ovr) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
                (val>ovr) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
      end;
      else result:=false;
    end;
  end;

FUNCTION T_realLiteral.leqForSorting(CONST other:P_scalarLiteral):boolean;
  begin
    if (other^.literalType in [lt_int,lt_real])
    then result:=isInRelationTo(tt_comparatorLeq,other)
    else result:=(literalType<=other^.literalType);
  end;

FUNCTION T_stringLiteral.isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;
  VAR ovl:ansistring;
  begin
    if other^.literalType<>lt_string then exit(false);
    ovl:=P_stringLiteral(other)^.val;
    result:=(val=ovl) and (relation in [tt_comparatorEq,tt_comparatorListEq,tt_comparatorLeq, tt_comparatorGeq]) or
            (val<ovl) and (relation in [tt_comparatorNeq, tt_comparatorLeq, tt_comparatorLss]) or
            (val>ovl) and (relation in [tt_comparatorNeq, tt_comparatorGeq, tt_comparatorGrt]);
  end;

FUNCTION T_stringLiteral.leqForSorting(CONST other:P_scalarLiteral):boolean;
  begin
    if (other^.literalType=lt_string)
    then result:=isInRelationTo(tt_comparatorLeq,other)
    else result:=(literalType<=other^.literalType);
  end;

FUNCTION T_stringLiteral.softCast:P_scalarLiteral;
  VAR len:longint;
      otherVal:ansistring;
  begin
    if lowercase(val)=C_boolText[false] then exit(newBoolLiteral(false));
    if lowercase(val)=C_boolText[true ] then exit(newBoolLiteral(true ));
    result:=parseNumber(val,false,len);
    if (result<>nil) then begin
      if (len=length(val))
      then exit(result)
      else disposeLiteral(result);
    end;
    otherVal:=unescapeString(sysutils.trim(value),len);
    if len=length(sysutils.trim(value)) then exit(newStringLiteral(otherVal));
    result:=@self;
    rereference;
  end;

FUNCTION T_stringLiteral.trim:P_stringLiteral;
  VAR rs:ansistring;
  begin
    rs:=sysutils.trim(val);
    if rs=val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.upper:P_stringLiteral;
  VAR rs:string;
  begin
    rs:=uppercase(val);
    if rs=val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_stringLiteral.lower:P_stringLiteral;
  VAR rs:string;
  begin
    rs:=lowercase(val);
    if rs=val then begin
      result:=@self;
      rereference;
    end else result:=newStringLiteral(rs);
  end;

FUNCTION T_expressionLiteral.isInRelationTo(CONST relation:T_tokenType; CONST other:P_scalarLiteral):boolean;
  begin
    result:=false;
  end;

FUNCTION T_expressionLiteral.leqForSorting(CONST other:P_scalarLiteral):boolean;
  begin
    result:=literalType<=other^.literalType;
  end;

FUNCTION T_scalarLiteral.operate(CONST op: T_tokenType; CONST other: P_scalarLiteral; CONST tokenLocation: T_tokenLocation): P_scalarLiteral;
  begin
    result:=@errLit;
    errLit.rereference;
  end;

FUNCTION T_boolLiteral.operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral;
  begin
    if other^.literalType=lt_expression then
      result:=newExpressionLiteral(subruleApplyOpCallback(@self,op,other,tokenLocation))
    else case op of
      tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq:
        result:=newBoolLiteral(isInRelationTo(op,other));
      tt_operatorAnd:
        if other^.literalType=lt_boolean then result:=newBoolLiteral(val and P_boolLiteral(other)^.val)
                                                 else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
      tt_operatorOr:
        if other^.literalType=lt_boolean then result:=newBoolLiteral(val or P_boolLiteral(other)^.val)
                                                 else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
      tt_operatorXor:
        if other^.literalType=lt_boolean then result:=newBoolLiteral(val xor P_boolLiteral(other)^.val)
                                                 else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
      tt_operatorStrConcat: result:=newStringLiteral(stringForm+other^.stringForm);
      else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
    end;
  end;

FUNCTION T_intLiteral.operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral;
  FUNCTION pot_int_int(x,y:int64):P_scalarLiteral;
    VAR temp:int64;
        tx,rx:extended;
    begin
      if y>=0 then begin
        temp:=1;
        while y>0 do begin
          if odd(y) then temp:=temp*x;
          x:=int64(x)*int64(x);
          y:=y shr 1;
        end;
        result:=newIntLiteral(temp);
      end else begin
        rx:=1/x; tx:=1; y:=-y;
        while y>0 do begin
          if odd(y) then tx:=tx*rx;
          rx:=rx*rx;
          y:=y shr 1;
        end;
        result:=newRealLiteral(tx);
      end;
    end;

  begin
    if other^.literalType=lt_expression then
      result:=newExpressionLiteral(subruleApplyOpCallback(@self,op,other,tokenLocation))
    else case op of
      tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq:
        result:=newBoolLiteral(isInRelationTo(op,other));
      tt_operatorAnd:
        if other^.literalType=lt_int then result:=newIntLiteral(val and P_intLiteral(other)^.val)
                                             else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
      tt_operatorOr:
        if other^.literalType=lt_int then result:=newIntLiteral(val or P_intLiteral(other)^.val)
                                             else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
      tt_operatorXor:
        if other^.literalType=lt_int then result:=newIntLiteral(val xor P_intLiteral(other)^.val)
                                             else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
      tt_operatorPlus: case other^.literalType of
          lt_int : result:=newIntLiteral (val + P_intLiteral (other)^.val);
          lt_real: result:=newRealLiteral(val + P_realLiteral(other)^.val);
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorMinus: case other^.literalType of
          lt_int : result:=newIntLiteral (val - P_intLiteral (other)^.val);
          lt_real: result:=newRealLiteral(val - P_realLiteral(other)^.val);
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorMult: case other^.literalType of
          lt_int : result:=newIntLiteral (val * P_intLiteral (other)^.val);
          lt_real: result:=newRealLiteral(val * P_realLiteral(other)^.val);
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorDivReal: case other^.literalType of
          lt_int : result:=newRealLiteral(val / P_intLiteral (other)^.val);
          lt_real: result:=newRealLiteral(val / P_realLiteral(other)^.val);
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorDivInt:
        if other^.literalType=lt_int then try
                                                    result:=newIntLiteral(val div P_intLiteral(other)^.val);
                                                  except
                                                    raiseError(el1_note,'WARN: Integer division by zero; returning Nan',tokenLocation);
                                                    result:=newRealLiteral(Nan);
                                                  end
                                             else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
      tt_operatorMod:
        if other^.literalType=lt_int then try
                                                    result:=newIntLiteral(val mod P_intLiteral(other)^.val)
                                                  except
                                                    raiseError(el1_note,'WARN: Integer division by zero; returning Nan',tokenLocation);
                                                    result:=newRealLiteral(Nan);
                                                  end
                                             else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
      tt_operatorPot: case other^.literalType of
          lt_int : result:=pot_int_int(val,P_intLiteral(other)^.val);
          lt_real: result:=newRealLiteral(exp(ln(val)*P_realLiteral(other)^.val));
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorStrConcat: result:=newStringLiteral(stringForm+other^.stringForm);
      else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
    end;
  end;

FUNCTION T_realLiteral.operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral;
  FUNCTION pot_real_int(x:extended; y:longint):extended;
    begin
      if y<0 then begin
        y:=-y;
        x:=1/x;
      end;
      result:=1;
      while y>0 do begin
        if odd(y) then result:=result*x;
        x:=x*x;
        y:=y shr 1;
      end;
    end;

  begin
    if other^.literalType=lt_expression then
      result:=newExpressionLiteral(subruleApplyOpCallback(@self,op,other,tokenLocation))
    else case op of
      tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq:
        result:=newBoolLiteral(isInRelationTo(op,other));
      tt_operatorPlus: case other^.literalType of
          lt_int : result:=newRealLiteral(val + P_intLiteral (other)^.val);
          lt_real: result:=newRealLiteral(val + P_realLiteral(other)^.val);
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorMinus: case other^.literalType of
          lt_int : result:=newRealLiteral(val - P_intLiteral (other)^.val);
          lt_real: result:=newRealLiteral(val - P_realLiteral(other)^.val);
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorMult: case other^.literalType of
          lt_int : result:=newRealLiteral(val * P_intLiteral (other)^.val);
          lt_real: result:=newRealLiteral(val * P_realLiteral(other)^.val);
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorDivReal: case other^.literalType of
          lt_int : result:=newRealLiteral(val / P_intLiteral (other)^.val);
          lt_real: result:=newRealLiteral(val / P_realLiteral(other)^.val);
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorPot: case other^.literalType of
          lt_int : result:=newRealLiteral(pot_real_int(val,P_intLiteral(other)^.val));
          lt_real: result:=newRealLiteral(exp(ln(val)*P_realLiteral(other)^.val));
          else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
        end;
      tt_operatorStrConcat: result:=newStringLiteral(stringForm+other^.stringForm);
      else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
    end;
  end;

FUNCTION T_stringLiteral.operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral;
  begin
    if other^.literalType=lt_expression then
      result:=newExpressionLiteral(subruleApplyOpCallback(@self,op,other,tokenLocation))
    else case op of
      tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq:
        result:=newBoolLiteral(isInRelationTo(op,other));
      tt_operatorPlus:
        if other^.literalType=lt_string
        then result:=newStringLiteral(val+P_stringLiteral(other)^.val)
        else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
      tt_operatorStrConcat: result:=newStringLiteral(stringForm+other^.stringForm);
      else result:=newErrorLiteralRaising(literalType,other^.literalType,op,tokenLocation);
    end;
  end;

FUNCTION T_expressionLiteral.operate(CONST op:T_tokenType; CONST other:P_scalarLiteral; CONST tokenLocation:T_tokenLocation):P_scalarLiteral;
  begin
    result:=newExpressionLiteral(subruleApplyOpCallback(@self,op,other,tokenLocation));
  end;

PROCEDURE T_listLiteral.append(CONST L:P_literal; CONST incRefs:boolean);
  begin
    if L=nil then begin
      raiseError(el3_evalError,'Trying to append NIL literal to list',C_nilTokenLocation);
      exit;
    end;
    setLength(element,length(element)+1);
    element[length(element)-1]:=L;
    if incRefs then L^.rereference;
    strictType:=lt_uncheckedList;
  end;

PROCEDURE T_listLiteral.appendAll(CONST L:P_listLiteral);
  VAR i:longint;
  begin
    for i:=0 to length(L^.element)-1 do append(L^.element[i],true);
  end;

PROCEDURE T_listLiteral.appendConstructing(CONST L:P_literal; CONST tokenLocation:T_tokenLocation);
  VAR last:P_literal;
      i0,i1:int64;
      c0,c1:char;
  begin
    if not(nextAppendIsRange) then begin
      append(L,true);
      exit;
    end;
    nextAppendIsRange:=false;

    if length(element)=0 then begin
      strictType:=lt_listWithError;
      raiseError(el3_evalError,'Cannot append range to empty list',tokenLocation);
      exit;
    end;
    last:=element[length(element)-1];
    if (last^.literalType=lt_int) and (L^.literalType=lt_int) then begin
      i0:=P_intLiteral(last)^.val;
      i1:=P_intLiteral(L)^.val;
      while (i0<i1) and (errorLevel<el3_evalError) do begin
        inc(i0);
        append(newIntLiteral(i0),false);
      end;
      while (i0>i1) and (errorLevel<el3_evalError) do begin
        dec(i0);
        append(newIntLiteral(i0),false);
      end;
    end else if (last^.literalType=lt_string) and (length(P_stringLiteral(last)^.val)=1) and
                (   L^.literalType=lt_string) and (length(P_stringLiteral(L)^.val)=1) then begin
      c0:=P_stringLiteral(last)^.val[1];
      c1:=P_stringLiteral(L)^.val[1];
      while c0<c1 do begin
        inc(c0);
        append(newStringLiteral(c0),false);
      end;
      while c0>c1 do begin
        dec(c0);
        append(newStringLiteral(c0),false);
      end;
    end else begin
      strictType:=lt_listWithError;
      raiseError(el3_evalError,'Invalid range expression '+last^.toString+'..'+L^.toString,tokenLocation);
    end;
  end;

PROCEDURE T_listLiteral.setRangeAppend;
  begin
    nextAppendIsRange:=true;
  end;

PROCEDURE T_listLiteral.sort;
  VAR temp:array of P_scalarLiteral;
      scale    :longint;
      i,j0,j1,k:longint;
  begin
    scale:=1;
    setLength(temp,length(element));
    while scale<length(element) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<length(element) do begin
        j0:=i;
        j1:=i+scale;
        k :=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do begin
          if P_scalarLiteral(element[j0])^.leqForSorting(P_scalarLiteral(element[j1]))
            then begin temp[k]:=P_scalarLiteral(element[j0]); inc(k); inc(j0); end
            else begin temp[k]:=P_scalarLiteral(element[j1]); inc(k); inc(j1); end;
        end;
        while (j0<i+scale)       and (j0<length(element)) do begin temp[k]:=P_scalarLiteral(element[j0]); inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(element)) do begin temp[k]:=P_scalarLiteral(element[j1]); inc(k); inc(j1); end;
        inc(i,scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale,scale);
      if (scale<length(element)) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //While making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while i<length(element) do begin
          j0:=i;
          j1:=i+scale;
          k :=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(element)) do begin
            if temp[j0]^.leqForSorting(temp[j1])
              then begin element[k]:=temp[j0]; inc(k); inc(j0); end
              else begin element[k]:=temp[j1]; inc(k); inc(j1); end;
          end;
          while (j0<i+scale)       and (j0<length(element)) do begin element[k]:=temp[j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(element)) do begin element[k]:=temp[j1]; inc(k); inc(j1); end;
          inc(i,scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale,scale);
      end else for k:=0 to length(element)-1 do element[k]:=temp[k];
    end;
    setLength(temp,0);
  end;

FUNCTION T_listLiteral.sortPerm:P_listLiteral;
  VAR temp1,temp2:array of record
        v:P_scalarLiteral;
        index:longint;
      end;
      scale    :longint;
      i,j0,j1,k:longint;

  begin
    setLength(temp1,length(element));
    setLength(temp2,length(element));
    for i:=0 to length(element)-1 do with temp1[i] do begin
      v:=P_scalarLiteral(element[i]);
      index:=i;
    end;
    scale:=1;
    while scale<length(temp1) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<length(temp1) do begin
        j0:=i;
        j1:=i+scale;
        k :=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do begin
          if temp1[j0].v^.leqForSorting(temp1[j1].v)
            then begin temp2[k]:=temp1[j0]; inc(k); inc(j0); end
            else begin temp2[k]:=temp1[j1]; inc(k); inc(j1); end;
        end;
        while (j0<i+scale)       and (j0<length(temp1)) do begin temp2[k]:=temp1[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(temp1)) do begin temp2[k]:=temp1[j1]; inc(k); inc(j1); end;
        inc(i,scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale,scale);
      if (scale<length(temp1)) then begin
        i:=0;
        while i<length(temp1) do begin
          j0:=i;
          j1:=i+scale;
          k :=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp1)) do begin
            if temp2[j0].v^.leqForSorting(temp2[j1].v)
              then begin temp1[k]:=temp2[j0]; inc(k); inc(j0); end
              else begin temp1[k]:=temp2[j1]; inc(k); inc(j1); end;
          end;
          while (j0<i+scale)       and (j0<length(temp1)) do begin temp1[k]:=temp2[j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(temp1)) do begin temp1[k]:=temp2[j1]; inc(k); inc(j1); end;
          inc(i,scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale,scale);
      end else for k:=0 to length(temp1)-1 do temp1[k]:=temp2[k];
    end;
    setLength(temp2,0);
    result:=newListLiteral;
    for i:=0 to length(temp1)-1 do result^.append(newIntLiteral(temp1[i].index),false);
    setLength(temp1,0);
  end;

PROCEDURE T_listLiteral.unique;
  VAR i,j:longint;
  begin
    if literalType in [lt_list, lt_listWithError, lt_uncheckedList] then exit;
    sort;
    i:=0;
    for j:=1 to length(element)-1 do if (P_scalarLiteral(element[i])^.isInRelationTo(tt_comparatorEq,P_scalarLiteral(element[j]))) then begin
      disposeLiteral(element[j]);
    end else begin
      inc(i); element[i]:=element[j];
    end;
    setLength(element,i+1);
  end;

FUNCTION T_literal.negate(CONST minusLocation:T_tokenLocation):P_literal; begin result:=@self; rereference; end;
FUNCTION T_scalarLiteral.negate(CONST minusLocation: T_tokenLocation): P_literal; begin result:=@self; rereference; end;

FUNCTION T_stringLiteral.negate(CONST minusLocation:T_tokenLocation):P_literal; begin result:=newErrorLiteralRaising('Cannot negate string.',minusLocation); end;
FUNCTION T_boolLiteral.negate(CONST minusLocation:T_tokenLocation):P_literal; begin result:=newErrorLiteralRaising('Cannot negate boolean.',minusLocation); end;
FUNCTION T_intLiteral.negate(CONST minusLocation:T_tokenLocation):P_literal; begin result:=newIntLiteral(-value); end;
FUNCTION T_realLiteral.negate(CONST minusLocation:T_tokenLocation):P_literal; begin result:=newRealLiteral(-value); end;
FUNCTION T_expressionLiteral.negate(CONST minusLocation:T_tokenLocation):P_literal; begin result:=newErrorLiteralRaising('Cannot negate expression. Please use "-1*..." instead.',minusLocation); end;
FUNCTION T_listLiteral.negate(CONST minusLocation:T_tokenLocation):P_literal;
  VAR res:P_listLiteral;
      i:longint;
  begin
    res:=newListLiteral;
    for i:=0 to length(element)-1 do res^.append(element[i]^.negate(minusLocation),false);
    result:=res;
  end;

FUNCTION resolveOperator(CONST LHS:P_literal; CONST op:T_tokenType; CONST RHS:P_literal; CONST tokenLocation:T_tokenLocation):P_literal;
  FUNCTION equals(CONST LHS,RHS:P_literal):boolean;
    VAR i:longint;
    begin
      if LHS=RHS then exit(true);
      case LHS^.literalType of
        lt_int,lt_real,lt_boolean,lt_string,lt_expression:
          if RHS^.literalType in [lt_int,lt_real,lt_boolean,lt_string,lt_expression]
          then exit(P_scalarLiteral(LHS)^.isInRelationTo(tt_comparatorEq,P_scalarLiteral(RHS)))
          else exit(false);
        lt_list..lt_flatList:
          if (RHS^.literalType in [lt_list..lt_flatList])
          and (length(P_listLiteral(LHS)^.element)=length(P_listLiteral(RHS)^.element)) then begin
            result:=true;
            i:=0;
            while result and (i<length(P_listLiteral(LHS)^.element)) do begin
              result:=result and equals(P_listLiteral(LHS)^.element[i],P_listLiteral(RHS)^.element[i]);
              inc(i);
            end;
          end else exit(false);
        else exit(false);
      end;
    end;

  FUNCTION isContained(CONST LHS,RHS:P_literal):boolean;
    VAR i:longint;
    begin
      result:=false;
      if RHS^.literalType in [lt_list..lt_flatList] then begin
        i:=0;
        while (i<length(P_listLiteral(RHS)^.element)) and not(result) do begin
          result:=result or equals(LHS,P_listLiteral(RHS)^.element[i]);
          inc(i);
        end;
      end;
    end;

  VAR i,i1,j:longint;
  begin
    //writeln('resolving operator ',op);
    //writeln('             LHS = @',ptrint(LHS));
    //writeln('             RHS = @',ptrint(RHS));
    //HANDLE ERROR LITERALS:---------------------------------------------------
    if (LHS^.literalType=lt_error) then begin LHS^.rereference; exit(LHS); end;
    if (RHS^.literalType=lt_error) then begin RHS^.rereference; exit(RHS); end;
    //---------------------------------------------------:HANDLE ERROR LITERALS
    //HANDLE EXPRESSION LITERALS:----------------------------------------------
    if (LHS^.literalType=lt_expression) or (RHS^.literalType=lt_expression)
    then exit(newExpressionLiteral(subruleApplyOpCallback(LHS,op,RHS,tokenLocation)));
    //----------------------------------------------:HANDLE EXPRESSION LITERALS
    //HANDLE S x S -> S OPERATORS:---------------------------------------------
    if (op in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt,
               tt_operatorAnd, tt_operatorOr, tt_operatorXor,
               tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
               tt_operatorStrConcat]) then begin
      case LHS^.literalType of
        lt_boolean, lt_int, lt_real, lt_string, lt_expression: begin
          case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string, lt_expression: begin
              //scalar X scalar
              exit(P_scalarLiteral(LHS)^.operate(op,P_scalarLiteral(RHS),tokenLocation));
            end;
            lt_list: begin
              //scalar X nested list
              result:=newListLiteral;
              for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(
                resolveOperator(LHS,op,P_listLiteral(RHS)^.element[i],tokenLocation),
                false);
              exit(result);
            end;
            lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_flatList: begin
              //scalar X flat list
              result:=newListLiteral;
              for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(
                P_scalarLiteral(LHS)^.operate(op,P_scalarLiteral(P_listLiteral(RHS)^.element[i]),tokenLocation),
                false);
              exit(result);
            end;
            else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
          end;
        end;
        lt_list: begin
          case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string, lt_expression: begin
              //nested list X scalar
              result:=newListLiteral;
              for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(
                resolveOperator(P_listLiteral(LHS)^.element[i],op,RHS,tokenLocation),
                false);
              exit(result);
            end;
            lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_flatList: begin
              //nested list X flat/nested list
              i :=length(P_listLiteral(LHS)^.element);
              i1:=length(P_listLiteral(RHS)^.element);
              if i=i1 then begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.element[i],op,
                    P_listLiteral(RHS)^.element[i],tokenLocation),false);
                exit(result);
              end else exit(newErrorLiteralRaising('Invalid list lengths '+intToStr(i)+' and '+intToStr(i1)+' given for operator '+C_tokenString[op],tokenLocation));
            end;
            else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
          end;
        end;
        lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_flatList:
          case RHS^.literalType of
            lt_boolean, lt_int, lt_real, lt_string, lt_expression: begin
              //flat list X scalar
              result:=newListLiteral;
              for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(
                P_scalarLiteral(P_listLiteral(LHS)^.element[i])^.operate(op,P_scalarLiteral(RHS),tokenLocation),
                false);
              exit(result);
            end;
            lt_list: begin
              //flat list X nested list
              i :=length(P_listLiteral(LHS)^.element);
              i1:=length(P_listLiteral(RHS)^.element);
              if i=i1 then begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  P_listLiteral(result)^.append(resolveOperator(
                    P_listLiteral(LHS)^.element[i],op,
                    P_listLiteral(RHS)^.element[i],tokenLocation),false);
                exit(result);
              end else exit(newErrorLiteralRaising('Invalid list lengths '+intToStr(i)+' and '+intToStr(i1)+' given for operator '+C_tokenString[op],tokenLocation));
            end;
            lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_flatList: begin
              //flat list X flat list
              i :=length(P_listLiteral(LHS)^.element);
              i1:=length(P_listLiteral(RHS)^.element);
              if i=i1 then begin
                result:=newListLiteral;
                for i:=0 to i1-1 do
                  P_listLiteral(result)^.append(
                    P_scalarLiteral(P_listLiteral(LHS)^.element[i])^.operate(op,
                    P_scalarLiteral(P_listLiteral(RHS)^.element[i]),tokenLocation),false);
                exit(result);
              end else exit(newErrorLiteralRaising('Invalid list lengths '+intToStr(i)+' and '+intToStr(i1)+' given for operator '+C_tokenString[op],tokenLocation));
            end;
            else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
          end;
          else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
        end;
      end;
    //  if (LHS^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression]) then begin
    //    if (RHS^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression]) then begin
    //      exit(P_scalarLiteral(LHS)^.operate(op,P_scalarLiteral(RHS),tokenLocation));
    //    end else begin
    //      result:=newListLiteral;
    //      case RHS^.literalType of
    //        lt_listWithError: exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
    //        lt_list: for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(resolveOperator(LHS,          op,                P_listLiteral(RHS)^.element[i] ,tokenLocation),false);
    //        else     for i:=0 to length(P_listLiteral(RHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(LHS)^.operate(op,P_scalarLiteral(P_listLiteral(RHS)^.element[i]),tokenLocation),false);
    //      end;
    //      exit(result);
    //    end;
    //  end else begin
    //    if (RHS^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression]) then begin
    //      result:=newListLiteral;
    //      case LHS^.literalType of
    //        lt_listWithError: exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
    //        lt_list: for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(resolveOperator(P_listLiteral(LHS)^.element[i]          ,op,                RHS ,tokenLocation),false);
    //        else     for i:=0 to length(P_listLiteral(LHS)^.element)-1 do P_listLiteral(result)^.append(P_scalarLiteral(P_listLiteral(LHS)^.element[i])^.operate(op,P_scalarLiteral(RHS),tokenLocation),false);
    //      end;
    //      exit(result);
    //    end else begin
    //      i :=length(P_listLiteral(LHS)^.element);
    //      i1:=length(P_listLiteral(RHS)^.element);
    //      if i=i1 then begin
    //        result:=newListLiteral;
    //        for i:=0 to i1-1 do
    //          P_listLiteral(result)^.append(resolveOperator(
    //            P_listLiteral(LHS)^.element[i],op,
    //            P_listLiteral(RHS)^.element[i],tokenLocation),false);
    //        exit(result);
    //      end else exit(newErrorLiteralRaising('Invalid list lengths '+intToStr(i)+' and '+intToStr(i1)+' given for operator '+C_tokenString[op],tokenLocation));
    //    end;
    //  end;
    //end;
    //---------------------------------------------:HANDLE S x S -> S OPERATORS
    case op of
      tt_operatorConcat: begin
        result:=newListLiteral;
        if (LHS^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression])
        then begin P_listLiteral(result)^.append(LHS,true); end
        else P_listLiteral(result)^.appendAll(P_listLiteral(LHS));
        if (RHS^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression])
        then begin P_listLiteral(result)^.append(RHS,true); end
        else P_listLiteral(result)^.appendAll(P_listLiteral(RHS));
        exit(result);
      end;
      tt_comparatorListEq: exit(newBoolLiteral(equals(LHS,RHS)));
      tt_operatorIn: exit(newBoolLiteral(isContained(LHS,RHS)));
      tt_operatorExtractL0: if LHS^.literalType in [lt_list..lt_flatList] then case RHS^.literalType of
        lt_int: begin
          i1:=length(P_listLiteral(LHS)^.element);
          i :=P_intLiteral(RHS)^.val;
          if (i>=0) and (i<i1) then begin
            result:=P_listLiteral(LHS)^.element[i];
            result^.rereference;
            exit(result);
          end else exit(newListLiteral);
        end;
        lt_intList: begin
          result:=newListLiteral;
          i1:=length(P_listLiteral(LHS)^.element);
          for j:=0 to length(P_listLiteral(RHS)^.element)-1 do begin
            i:=P_intLiteral(P_listLiteral(RHS)^.element[j])^.val;
            if (i>=0) and (i<i1) then P_listLiteral(result)^.append(P_listLiteral(LHS)^.element[i],true);
          end;
          exit(result);
        end;
        lt_booleanList: begin
          result:=newListLiteral;
          i1:=length(P_listLiteral(LHS)^.element);
          if i1=length(P_listLiteral(RHS)^.element) then for i:=0 to length(P_listLiteral(RHS)^.element)-1 do
            if P_boolLiteral(P_listLiteral(RHS)^.element[i])^.val
            then P_listLiteral(result)^.append(P_listLiteral(LHS)^.element[i],true);
          exit(result);
        end;
        lt_list,lt_flatList:
          if P_listLiteral(RHS)^.size=0 then exit(newListLiteral)
                                        else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
        else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
      end else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
      tt_operatorExtractL1: if LHS^.literalType in [lt_list..lt_flatList] then begin
        result:=newListLiteral;
        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do
          P_listLiteral(result)^.append(resolveOperator(P_listLiteral(LHS)^.element[i],tt_operatorExtractL0,RHS,tokenLocation),false);
        exit(result);
      end else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
      tt_operatorExtractL2: if LHS^.literalType in [lt_list..lt_flatList] then begin
        result:=newListLiteral;
        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do
          P_listLiteral(result)^.append(resolveOperator(P_listLiteral(LHS)^.element[i],tt_operatorExtractL1,RHS,tokenLocation),false);
        exit(result);
      end else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
      tt_operatorExtractL3: if LHS^.literalType in [lt_list..lt_flatList] then begin
        result:=newListLiteral;
        for i:=0 to length(P_listLiteral(LHS)^.element)-1 do
          P_listLiteral(result)^.append(resolveOperator(P_listLiteral(LHS)^.element[i],tt_operatorExtractL2,RHS,tokenLocation),false);
        exit(result);
      end else exit(newErrorLiteralRaising(LHS^.literalType,RHS^.literalType,op,tokenLocation));
    end;
  end;
  
FUNCTION T_literal.hash:longint;       begin result:=-1; end;
FUNCTION T_scalarLiteral.hash:longint; begin result:=longint(lt_error); end;
FUNCTION T_boolLiteral.hash:longint;   begin result:=longint(lt_boolean); if val then inc(result); end;
FUNCTION T_intLiteral.hash:longint;    begin result:=longint(lt_int) xor longint(val); end;
FUNCTION T_realLiteral.hash:longint;   begin {$Q-} move(val,result,4); result:=result xor longint(lt_real); {$Q+} end; 
FUNCTION T_stringLiteral.hash:longint; 
  VAR i:longint;
  begin
    {$Q-}
    result:=longint(lt_string)+length(val);
    for i:=1 to length(val) do result:=result*31+ord(val[i]);
    {$Q+}
  end;

FUNCTION T_expressionLiteral.hash:LongInt;
  begin
    {$Q-}
    result:=longint(lt_expression)+longint(val);
    {$Q+}
  end;
  
FUNCTION T_listLiteral.hash:longint;
  VAR i:longint;
  begin
    {$Q-}
    result:=longint(lt_list)+length(element);
    for i:=0 to length(element)-1 do result:=result*31+element[i]^.hash;
    {$Q+}
  end;
  
FUNCTION T_literal.equals(CONST other:P_literal):Boolean;           begin result:=(@self=other); end;
FUNCTION T_scalarLiteral.equals(CONST other:P_literal):Boolean;     begin result:=(@self=other); end; //this can only be the error literal, a singleton
FUNCTION T_boolLiteral.equals(CONST other:P_literal):Boolean;       begin result:=(@self=other); end; //this can only be true or false, both singleons
FUNCTION T_intLiteral.equals(CONST other:P_literal):Boolean;        begin result:=(@self=other) or (other^.literalType=lt_int) and (P_intLiteral(other)^.value=val); end;
FUNCTION T_realLiteral.equals(CONST other:P_literal):Boolean;       begin result:=(@self=other) or (other^.literalType=lt_real) and (P_realLiteral(other)^.value=val); {$WARNING TODO Does this cover Nan and Inf?} end;
FUNCTION T_stringLiteral.equals(CONST other:P_literal):Boolean;     begin result:=(@self=other) or (other^.literalType=lt_string) and (P_stringLiteral(other)^.value=val); end;
FUNCTION T_expressionLiteral.equals(CONST other:P_literal):Boolean; begin result:=(@self=other) or (other^.literalType=lt_expression) and (P_expressionLiteral(other)^.value=val); end;
FUNCTION T_listLiteral.equals(CONST other:P_literal):Boolean;       
  VAR i:longint;
  begin 
    if (@self=other) then exit(true);
    if (other^.literalType<>literalType) or (P_listLiteral(other)^.size<>size) then exit(false);
    for i:=0 to length(element)-1 do
      if not(element[i]^.equals(P_listLiteral(other)^.element[i])) then exit(false);
    result:=true;
  end;
  
VAR i:longint;
INITIALIZATION
  boolLit[false].create(false);
  boolLit[true] .create(true );
  errLit.init;
  for i:=0 to length(intLit)-1 do intLit[i]:=nil;
  DefaultFormatSettings.DecimalSeparator:='.';
  SetExceptionMask([ exInvalidOp,  exDenormalized,  exZeroDivide,  exOverflow,  exUnderflow,  exPrecision]);
  randomize;

FINALIZATION
  boolLit[false].destroy;
  boolLit[true] .destroy;
  errLit.destroy;
  for i:=0 to length(intLit)-1 do if intLit[i]<>nil then dispose(intLit[i],destroy);
end.
