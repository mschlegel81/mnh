{$ifdef include_interface}
  P_token=^T_token;
  T_token=packed object
    next    :P_Token;
    location:T_tokenLocation;
    txt     :shortstring;
    tokType :T_tokenType;
    data    :pointer;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer);
    PROCEDURE define(CONST tokenLocation: T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType);
    PROCEDURE define(CONST original:T_token);
    PROCEDURE undefine;
    FUNCTION last:P_token;
    FUNCTION toString:ansistring;
    FUNCTION getDeclarationOrAssignmentToken:P_token;
  end;

  T_tokenRecycler=object
    dat:array[0..1023] of P_token;
    fill:longint;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION disposeToken(p:P_token):P_token; inline;
    PROCEDURE cascadeDisposeToken(VAR p:P_token);
    FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer):P_token; inline;
    FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType):P_token; inline;
    FUNCTION newToken(CONST original:T_token):P_token; inline;
    FUNCTION newToken(CONST original:P_token):P_token; inline;
  end;

{$endif}
{$ifdef include_implementation}

FUNCTION isQualified(CONST s:string):boolean;
  begin
    result:=pos(C_id_qualify_character,s)>0;
  end;

CONSTRUCTOR T_tokenRecycler.create;
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
  end;

DESTRUCTOR T_tokenRecycler.destroy;
  begin
    while fill>0 do begin
      dec(fill);
      try
        dispose(dat[fill],destroy);
      except
        dat[fill]:=nil;
      end;
    end;
  end;

FUNCTION T_tokenRecycler.disposeToken(p:P_token):P_token;
  begin
    if p=nil then exit(nil);
    result:=p^.next;
    if (fill>=length(dat))
    then dispose(p,destroy)
    else begin
      p^.undefine;
      dat[fill]:=p;
      inc(fill);
    end;
  end;

PROCEDURE T_tokenRecycler.cascadeDisposeToken(VAR p:P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_tokenRecycler.newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST original:T_token):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end;

FUNCTION T_tokenRecycler.newToken(CONST original:P_token):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original^);
    result^.next:=nil;
  end;

PROCEDURE polishExpressionString(VAR s:ansistring);
  CONST specialChar:array [0..16] of char=(',','+','-','*','/','?',':','^','%','.','&','(',')','[',']','{','}');
  VAR i:longint;
  begin
    s:=replaceAll(trim(s),'  ',' ');
    for i:=0 to length(specialChar)-1 do s:=replaceAll(s,' '+specialChar[i],specialChar[i]);
    for i:=1 to length(specialChar)-1 do s:=replaceAll(s,specialChar[i]+' ',specialChar[i]);
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
    polishExpressionString(result);
  end;

FUNCTION safeTokenToString(CONST t:P_token):ansistring;
  begin
    if t=nil then result:='<EOL>'
    else result:=t^.toString;
  end;

CONSTRUCTOR T_token.create;
  begin
  end;

DESTRUCTOR T_token.destroy;
  begin
    undefine;
  end;

PROCEDURE T_token.define(CONST tokenLocation:T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer);
  begin
    location:=tokenLocation;
    if (tokenText='') or (C_tokenString[tokenType]<>'')
      then txt:=C_tokenString[tokenType]
      else txt:=tokenText;
    tokType:=tokenType;
    data:=ptr;
  end;

PROCEDURE T_token.define(CONST tokenLocation:T_tokenLocation; tokenText:ansistring; CONST tokenType:T_tokenType);
  begin
    location:=tokenLocation;
    if tokenText='' then txt:=C_tokenString[tokenType]
                    else txt:=tokenText;
    tokType:=tokenType;
    data:=nil;
  end;

PROCEDURE T_token.define(CONST original:T_token);
  begin
    location:=original.location;
    txt     :=original.txt;
    tokType :=original.tokType;
    data    :=original.data;
    if (tokType in [tt_literal,tt_parList]) or (tokType in [tt_each,tt_parallelEach]) and (data<>nil) then P_literal(data)^.rereference
    else if (tokType in [tt_list_constructor,tt_parList_constructor]) then data:=P_listLiteral(original.data)^.clone;
  end;

PROCEDURE T_Token.undefine;
  begin
    case tokType of
      tt_literal,tt_list_constructor,tt_parList_constructor,tt_parList: disposeLiteral(data);
      tt_each,tt_parallelEach: if data<>nil then disposeLiteral(data);
      else data:=nil;
    end;
    tokType:=tt_eol;
    location:=C_nilTokenLocation;
  end;

FUNCTION T_token.last:P_token;
  begin
    result:=@self;
    while result^.next<>nil do result:=result^.next;
  end;

FUNCTION T_token.toString:ansistring;
  begin
    case tokType of
      tt_each, tt_parallelEach: begin
        result:=C_tokenString[tokType];
        if txt<>'' then result:=result+'('+txt+',';
        if data<>nil then result:=result+P_literal(data)^.toString+',';
      end;
      tt_mutate: result:=txt;
      tt_identifier,
      tt_localUserRulePointer,
      tt_importedUserRulePointer,
      tt_intrinsicRulePointer,
      tt_parameterIdentifier: result:=' '+txt+' ';
      tt_literal            : result:=' '+P_literal(data)^.toString+' ';
      tt_parList_constructor: result:=P_listLiteral(data)^.toParameterListString(false);
      tt_parList            : result:=P_listLiteral(data)^.toParameterListString(true);
      tt_list_constructor   : result:=P_literal(data)^.toString;
      tt_operatorAnd,tt_operatorOr,tt_operatorXor,tt_operatorIn,tt_operatorDivInt,tt_operatorMod: result:=' '+C_tokenString[tokType]+' ';
      else result:=C_tokenString[tokType];
    end;
  end;

FUNCTION T_token.getDeclarationOrAssignmentToken:P_token;
  begin
    if tokType in [tt_declare,tt_assign] then result:=@self
    else if next<>nil then result:=next^.getDeclarationOrAssignmentToken
    else result:=nil;
  end;

FUNCTION firstToken(VAR line:ansistring; VAR lineLocation:T_tokenLocation; CONST inPackage:P_package; CONST inEvaluationLoop:boolean):T_token;
  VAR parsedLength:longint=0;

  PROCEDURE fail(CONST message:ansistring);
    begin
      if inEvaluationLoop then begin
        raiseError(el4_parsingError,message,lineLocation);
      end else parsedLength:=length(line);
    end;

  FUNCTION leadingId(CONST allowQualified:boolean):ansistring;
    VAR i:longint;
        dotFound:boolean;
    begin
      i:=1;
      dotFound:=false;
      while (i<length(line)) and (line[i+1] in ['a'..'z','A'..'Z','0'..'9','_',C_id_qualify_character]) do begin
        inc(i);
        if line[i]=C_id_qualify_character then begin
          if (i<length(line)) and (line[i+1]=C_id_qualify_character) then begin
            parsedLength:=i-1;
            exit(copy(line,1,i-1));
          end else if not(allowQualified) then begin
            fail('No qualified is identifier accepted here. '+line);
            parsedLength:=i-1;
            exit(copy(line,1,i-1));
          end else if dotFound then begin
            fail('Over-qualified identifier! Only one level is allowed. '+line);
            parsedLength:=i-1;
            exit(copy(line,1,i-1));
          end else dotFound:=true;
        end;
      end;
      parsedLength:=i;
      result:=copy(line,1,i);
    end;

  PROCEDURE removeLeadingBlanks();
    VAR i:longint;
    begin
      i:=1;
      while (i<=length(line)) and (line[i] in [' ',C_lineBreakChar,C_tabChar,C_carriageReturnChar]) do inc(i);
      if i>1 then line:=copy(line,i,length(line)+1-i);
      inc(lineLocation.column,i-1);
    end;

  VAR id:ansistring;

  begin
    {$ifdef PROFILING}
    profiler.Clear;
    profiler.Start;
    {$endif}

    removeLeadingBlanks;
    result.location:=lineLocation;
    result.txt:='';
    result.tokType:=tt_eol;
    result.data:=nil;
    result.next:=nil;
    if length(line)<1 then exit(result);
    case line[1] of
      '0'..'9': begin
        result.data:=parseNumber(line,not(inEvaluationLoop),parsedLength);
        if parsedLength<=0 then fail('Cannot parse numeric literal '+line)
                           else result.tokType:=tt_literal;
      end;
      '"','''': begin
        id:=unescapeString(line,parsedLength);
        if parsedLength=0 then fail('Cannot parse string literal '+line)
        else begin
          result.tokType:=tt_literal;
          if inEvaluationLoop then result.data:=newStringLiteral(id);
        end;
      end;
      '$': begin
        result.txt:=leadingId(false);
        parsedLength:=length(result.txt);
        result.tokType:=tt_parameterIdentifier;
      end;
      'a'..'z','A'..'Z': begin
        id:=leadingId(true);
        parsedLength:=length(id);
        if      id=C_tokenString[tt_operatorXor]       then result.tokType:=tt_operatorXor
        else if id=C_tokenString[tt_operatorOr]        then result.tokType:=tt_operatorOr
        else if id=C_tokenString[tt_operatorMod]       then result.tokType:=tt_operatorMod
        else if id=C_tokenString[tt_operatorIn]        then result.tokType:=tt_operatorIn
        else if id=C_tokenString[tt_operatorDivInt]    then result.tokType:=tt_operatorDivInt
        else if id=C_tokenString[tt_operatorAnd]       then result.tokType:=tt_operatorAnd
        else if id=C_tokenString[tt_modifier_private]  then result.tokType:=tt_modifier_private
        else if id=C_tokenString[tt_modifier_memoized] then result.tokType:=tt_modifier_memoized
        else if id=C_tokenString[tt_modifier_mutable]  then result.tokType:=tt_modifier_mutable
        else if id=C_tokenString[tt_modifier_synchronized]  then result.tokType:=tt_modifier_synchronized
        else if id=C_tokenString[tt_each]         then begin result.tokType:=tt_each;         result.data:=nil; result.txt:=''; end
        else if id=C_tokenString[tt_parallelEach] then begin result.tokType:=tt_parallelEach; result.data:=nil; result.txt:=''; end
        else if id=C_boolText[true]  then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newBoolLiteral(true); end
        else if id=C_boolText[false] then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newBoolLiteral(false); end
        else if id='Nan'             then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newRealLiteral(Nan); end
        else if id='Inf'             then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newRealLiteral(Infinity); end
        else if id='void'            then begin result.tokType:=tt_literal; if inEvaluationLoop then result.data:=newVoidLiteral; end
        else begin
          result.txt:=id;
          result.tokType:=tt_identifier;
          result.data:=inPackage;
          if not(inEvaluationLoop) then inPackage^.resolveRuleId(result,true);
        end;
      end;
      ';': begin parsedLength:=1; result.tokType:=tt_semicolon; end;
      '}': begin parsedLength:=1; result.tokType:=tt_expBraceClose; end;
      '|': begin parsedLength:=1; result.tokType:=tt_operatorConcat; end;
      '{': begin parsedLength:=1; result.tokType:=tt_expBraceOpen; end;
      '^': begin parsedLength:=1; result.tokType:=tt_operatorPot; end;
      ']': begin parsedLength:=1; result.tokType:=tt_listBraceClose; end;
      '[': begin parsedLength:=1; result.tokType:=tt_listBraceOpen; end;
      '?': begin parsedLength:=1; result.tokType:=tt_iifCheck; end;
      ',': begin parsedLength:=1; result.tokType:=tt_separatorComma; end;
      '+': begin parsedLength:=1; result.tokType:=tt_operatorPlus; end;
      ')': begin parsedLength:=1; result.tokType:=tt_braceClose; end;
      '(': begin parsedLength:=1; result.tokType:=tt_braceOpen; end;
      '&': begin parsedLength:=1; result.tokType:=tt_operatorStrConcat; end;
      '-': if startsWith(line,'->') then begin
             parsedLength:=2;
             result.tokType:=tt_declare;
           end else begin
             parsedLength:=1;
             result.tokType:=tt_operatorMinus;
           end;
      '*': if startsWith(line,'**') then begin
             parsedLength:=2;
             result.tokType:=tt_operatorPot;
           end else begin
             parsedLength:=1;
             result.tokType:=tt_operatorMult;
           end;
      '>': if startsWith(line,'>=') then begin
             parsedLength:=2;
             result.tokType:=tt_comparatorGeq;
           end else begin
             parsedLength:=1;
             result.tokType:=tt_comparatorGrt;
           end;
      '!': if startsWith(line,'!=') then begin
             parsedLength:=2;
             result.tokType:=tt_comparatorNeq;
           end else begin
             fail('Cannot parse: '+line+' (first char is "'+line[1]+'"=#'+intToStr(ord(line[1]))+')');
             line:='';
           end;
      '=': if startsWith(line,'=[]') then begin
             parsedLength:=3;
             result.tokType:=tt_typeCheckEmptyList;
           end else if startsWith(line,'==') then begin
             parsedLength:=2;
             result.tokType:=tt_comparatorListEq;
           end else begin
             parsedLength:=1;
             result.tokType:=tt_comparatorEq;
           end;
      '<': if startsWith(line,'<>[]') then begin
             parsedLength:=4;
             result.tokType:=tt_typeCheckNonemptyList;
           end else if startsWith(line,'<>') then begin
             parsedLength:=2;
             result.tokType:=tt_comparatorNeq;
           end else if startsWith(line,'<=') then begin
             parsedLength:=2;
             result.tokType:=tt_comparatorLeq;
           end else begin
             parsedLength:=1;
             result.tokType:=tt_comparatorLss;
           end;
      '/': if startsWith(line,'//') then begin //comments
             parsedLength:=length(line);
             result.tokType:=tt_eol;
             if startsWith(line,'//*') then result.txt:=trim(copy(line,4,length(line)-3));
           end else begin
             parsedLength:=1;
             result.tokType:=tt_operatorDivReal;
           end;
      '%': if startsWith(line,'%%%%') then begin
             parsedLength:=4;
             result.tokType:=tt_operatorExtractL3;
           end else if startsWith(line,'%%%') then begin
             parsedLength:=3;
             result.tokType:=tt_operatorExtractL2;
           end else if startsWith(line,'%%') then begin
             parsedLength:=2;
             result.tokType:=tt_operatorExtractL1;
           end else begin
             parsedLength:=1;
             result.tokType:=tt_operatorExtractL0;
           end;
      '.': if startsWith(line,'..') then begin
             parsedLength:=2;
             result.tokType:=tt_separatorCnt;
           end else fail('Cannot parse: '+line);
      ':': if startsWith(line,':=') then begin
             parsedLength:=2;
             result.tokType:=tt_assign;
           end else if (length(line)>=4) and (line[2] in ['b','e','i','l','n','s','r']) then begin
             id:=leadingId(false);
             parsedLength:=length(id);
             if      id=':booleanList' then result.tokType:=tt_typeCheckBoolList
             else if id=':boolean'     then result.tokType:=tt_typeCheckBoolean
             else if id=':expression'  then result.tokType:=tt_typeCheckExpression
             else if id=':intList'     then result.tokType:=tt_typeCheckIntList
             else if id=':int'         then result.tokType:=tt_typeCheckInt
             else if id=':list'        then result.tokType:=tt_typeCheckList
             else if id=':numericList' then result.tokType:=tt_typeCheckNumList
             else if id=':numeric'     then result.tokType:=tt_typeCheckNumeric
             else if id=':stringList'  then result.tokType:=tt_typeCheckStringList
             else if id=':scalar'      then result.tokType:=tt_typeCheckScalar
             else if id=':string'      then result.tokType:=tt_typeCheckString
             else if id=':realList'    then result.tokType:=tt_typeCheckRealList
             else if id=':real'        then result.tokType:=tt_typeCheckReal
             else begin
               parsedLength:=1;
               result.tokType:=tt_iifElse;
             end;
           end else begin
             parsedLength:=1;
             result.tokType:=tt_iifElse;
           end;
      else begin
        fail('Cannot parse: '+line+' (first char is "'+line[1]+'"=#'+intToStr(ord(line[1]))+')');
        line:='';
      end;
    end;
    if parsedLength>0 then begin
      if not(inEvaluationLoop) then result.txt:=copy(line,1,parsedLength);
      line:=copy(line,parsedLength+1,length(line));
      inc(lineLocation.column,parsedLength);
    end;
    {$ifdef PROFILING}
    writeln(stderr,'PROFILING;firstToken;' + FloatToStr(profiler.Elapsed));
    {$endif}
  end;

PROCEDURE predigestBeforeDeclarationParsing(VAR first:P_token; VAR recycler:T_tokenRecycler);
  VAR this,next,prev:P_token;
  begin
    this:=first; prev:=nil;
    while this<>nil do begin
      next:=this^.next;
      if (this^.tokType in [tt_operatorMinus,tt_operatorPlus]) and
         ((prev=nil) or (prev^.tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_each,tt_parallelEach, tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus]))
         and (this^.next<>nil) and (this^.next^.tokType in [tt_literal,tt_identifier,tt_braceOpen,tt_listBraceOpen,tt_expBraceOpen,tt_localUserRulePointer,tt_importedUserRulePointer,tt_intrinsicRulePointer,tt_parameterIdentifier]) then begin
        if this^.tokType=tt_operatorMinus then begin
          if (next<>nil) and (next^.tokType = tt_literal) then begin
            this^.tokType:=tt_literal;
            this^.data:=P_literal(next^.data)^.negate(this^.location);
            this^.next:=recycler.disposeToken(next);
          end else this^.tokType:=tt_unaryOpMinus;
        end else begin
          if prev=nil then begin
            first:=recycler.disposeToken(this);
            this:=first;
          end else begin
            prev^.next:=recycler.disposeToken(this);
            this:=next;
          end;
        end;
      end else if (this^.tokType=tt_listBraceOpen) and (this^.next<>nil) and (this^.next^.tokType=tt_listBraceClose) then begin
        this^.tokType:=tt_literal;
        this^.data:=newListLiteral;
        this^.next:=recycler.disposeToken(this^.next);
      end;
      prev:=this;
      this:=this^.next;
    end;
  end;

PROCEDURE predigest(VAR first:P_token; CONST inPackage:P_package; VAR recycler:T_tokenRecycler);
  VAR t:P_token;
      rule:P_rule;
  begin
    t:=first;
    while t<>nil do begin
      case t^.tokType of
        tt_identifier: begin
          if t^.data=nil then t^.data:=inPackage;
          if (t^.next<>nil) and (t^.next^.tokType=tt_assign) then begin
            inPackage^.resolveRuleId(t^,false);
            if errorLevel<el3_evalError then begin
              if t^.tokType=tt_localUserRulePointer then begin
                rule:=t^.data;
                if rule^.ruleType=rt_mutable then begin
                  t^.data:=rule;
                  t^.tokType:=tt_mutate;
                  t^.txt:=t^.txt+C_tokenString[tt_mutate];
                  t^.next:=recycler.disposeToken(t^.next);
                end else raiseError(el4_parsingError,'You can only mutate local mutable rules! Rule '+t^.txt+' is not mutable',t^.next^.location);
              end else raiseError(el4_parsingError,'You can only mutate local mutable rules! Rule '+t^.txt+' is a '+C_tokenInfoString[t^.tokType],t^.next^.location);
            end else raiseError(el4_parsingError,'You can only mutate local mutable rules! Identifier '+t^.txt+' cannot be resolved.',t^.next^.location);
          end;
        end;

        tt_each,tt_parallelEach:
          if  (t^.data=nil)
          and (t^.next            <>nil) and (t^.next^            .tokType=tt_braceOpen)
          and (t^.next^.next      <>nil) and (t^.next^.next^      .tokType=tt_identifier)
          and (t^.next^.next^.next<>nil) and (t^.next^.next^.next^.tokType=tt_separatorComma) then begin
            t^.txt:=t^.next^.next^.txt;
            t^.data:=nil;
            t^.next:=recycler.disposeToken(recycler.disposeToken(recycler.disposeToken(t^.next))); //dispose ( , <id> and ","
          end else if t^.txt='' then begin
            raiseError(el4_parsingError,'Invalid each-expression; expected to start with "each(<id>,", starting with "'+tokensToString(t)+'"' ,t^.location);
            exit;
          end;
        end;
      t:=t^.next;
    end;
  end;

PROCEDURE digestInlineExpression(VAR rep:P_token; VAR recycler:T_tokenRecycler);
  VAR t,prev,inlineRule:P_token;
      bracketLevel:longint=0;
      inlineSubRule:P_subrule;
  begin
    predigestBeforeDeclarationParsing(rep,recycler);
    predigest(rep,nil,recycler);
    if (rep^.tokType<>tt_expBraceOpen) then begin
      raiseError(el4_parsingError,'Error creating subrule from inline; expression does not start with "{"',rep^.location);
      exit;
    end;
    t:=rep^.next; prev:=rep;
    inlineRule:=t;
    while (t<>nil) and ((t^.tokType<>tt_expBraceClose) or (bracketLevel>0)) do begin
      case t^.tokType of
        tt_expBraceOpen: begin
          digestInlineExpression(t,recycler);
          if t^.tokType=tt_expBraceOpen then inc(bracketLevel);
        end;
      end;
      prev:=t;
      t:=t^.next;
    end;
    if (t=nil) or (t^.tokType<>tt_expBraceClose) then begin
      raiseError(el4_parsingError,'Error creating subrule from inline; expression does not end with an }',rep^.location);
      exit;
    end;

    rep^.next:=t^.next; //remove expression from parent expression
    prev^.next:=nil; //unlink closing curly bracket
    recycler.disposeToken(t); //dispose closing curly bracket
    reduceExpression(inlineRule,true,0,recycler);
    if   errorLevel<=el2_warning then begin
      new(inlineSubRule,createFromInline(inlineRule,recycler));
      if   errorLevel<=el2_warning then begin
        rep^.tokType:=tt_literal;
        rep^.data:=newExpressionLiteral(inlineSubRule);
      end else dispose(inlineSubRule,destroy);
    end;
  end;

PROCEDURE reduceExpression(VAR first:P_token; CONST pureOnly:boolean; CONST callDepth:word; VAR recycler:T_tokenRecycler);
  VAR stack:array of P_token;
      newLit:P_literal;
      didSubstitution:boolean;
      tt:T_tokenType;

  FUNCTION cTokType(CONST index:longint):T_tokenType; inline;
    VAR p:P_token;
        i:longint;
    begin
      if index>=0 then begin
        p:=first;
        for i:=1 to index do if p<>nil then p:=p^.next;
        if p=nil then result:=tt_eol
                 else result:=p^.tokType;
      end else if index>=-length(stack) then begin
        result:=stack[length(stack)+index]^.tokType;
      end else result:=tt_eol;
    end;

  PROCEDURE stack_popDestroy; inline;
    begin
      recycler.disposeToken(stack[length(stack)-1]);
      setLength(stack,length(stack)-1);
      didSubstitution:=true;
    end;

  PROCEDURE stack_popLink; inline;
    VAR ti:longint;
    begin
      ti:=length(stack)-1;
      stack[ti]^.next:=first;
      first:=stack[ti];
      setLength(stack,ti);
      didSubstitution:=true;
    end;

  PROCEDURE stack_push; inline;
    begin
      setLength(stack,length(stack)+1);
      stack[length(stack)-1]:=first;
      first:=first^.next;
      didSubstitution:=true;
    end;

  PROCEDURE resolveEach(CONST inParallel:boolean);
    VAR t,bracketClosingEach:P_token;
        tokensBetween:array of P_token;
        bodyRule:array of P_subrule;
        bodyRuleLiteral:array of P_expressionLiteral;
        aggregatorPresent:boolean;
        aggregatorIsPrimitive:boolean;

    FUNCTION parseBodyOk:boolean;
      VAR bracketLevel,i:longint;
          p:P_token;
          bodyPart:array of array[0..1] of P_token;
      begin
        {$ifdef PROFILING}
        profiler.Clear;
        profiler.Start;
        {$endif}

        //first token is <each>-Token
        //find closing bracket and body parts
        bracketLevel:=0; //note: first token will raise bracket Level to 1
        t:=first; p:=nil;
        if (first^.next<>nil) and (first^.next^.tokType<>tt_separatorComma) then begin
          setLength(bodyPart,1);
          bodyPart[0,0]:=first^.next;
        end else begin
          raiseError(el4_parsingError,'Invalid each-construct; Cannot find closing bracket.',first^.location);
          exit(false);
        end;
        while (t<>nil) and not((t^.tokType=tt_braceClose) and (bracketLevel=1)) do begin
          if      t^.tokType in [tt_braceOpen,tt_each,tt_parallelEach,tt_listBraceOpen ,tt_expBraceOpen]  then inc(bracketLevel)
          else if t^.tokType in [tt_braceClose                       ,tt_listBraceClose,tt_expBraceClose] then dec(bracketLevel)
          else if (t^.tokType=tt_separatorComma) and (bracketLevel=1) then begin
            bodyPart[length(bodyPart)-1,1]:=p; //end of body part is token before comma
            setLength(bodyPart,length(bodyPart)+1);
            bodyPart[length(bodyPart)-1,0]:=t^.next; //start of next body part is token after comma
          end else if pureOnly and (t^.tokType in [tt_identifier,tt_localUserRulePointer,tt_importedUserRulePointer,tt_mutate]) then exit(false);
          p:=t; t:=t^.next;
        end;
        bodyPart[length(bodyPart)-1,1]:=p; //end of body part is token before comma
        if (t=nil) or (t^.tokType<>tt_braceClose) or (bracketLevel<>1) then begin
          raiseError(el4_parsingError,'Invalid each-construct; Cannot find closing bracket.',first^.location);
          exit(false);
        end;
        bracketClosingEach:=t;
        for i:=0 to length(bodyPart)-1 do begin
          if bodyPart[i,1]^.next<>bracketClosingEach then recycler.disposeToken(bodyPart[i,1]^.next);
          bodyPart[i,1]^.next:=nil;
        end;

        //process aggregator part (if any)----------------------------------------------
        if (bodyPart[0,0]^.tokType in [tt_comparatorEq..tt_operatorIn]) and
           (bodyPart[0,1]^.tokType in [tt_comparatorEq..tt_operatorIn]) then begin
          setLength(tokensBetween,0);
          t:=bodyPart[0,0];
          while t<>nil do begin
            setLength(tokensBetween,length(tokensBetween)+1);
            tokensBetween[length(tokensBetween)-1]:=t;
            t:=t^.next;
          end;
          for i:=0 to length(bodyPart)-2 do bodyPart[i]:=bodyPart[i+1];
          setLength(bodyPart,length(bodyPart)-1);

          first^.tokType:=tt_braceOpen;
          bracketClosingEach^.tokType:=tt_braceClose;
          aggregatorPresent:=true;
          aggregatorIsPrimitive:=length(tokensBetween)=1;
        end else begin
          setLength(tokensBetween,1);
          tokensBetween[0]:=recycler.newToken(first^.location,'',tt_separatorComma);

          first^.tokType:=tt_listBraceOpen;
          bracketClosingEach^.tokType:=tt_listBraceClose;
          aggregatorPresent    :=false;
          aggregatorIsPrimitive:=false;
        end;
        //----------------------------------------------process aggregator part (if any)
        //process other body parts (if any)---------------------------------------------
        setLength(bodyRule       ,length(bodyPart));
        setLength(bodyRuleLiteral,length(bodyPart));
        for i:=0 to length(bodyPart)-1 do begin
          new(bodyRule[i],createForEachBody(first^.txt,bodyPart[i,0],recycler));
          bodyRuleLiteral[i]:=newExpressionLiteral(bodyRule[i]);
        end;
        //---------------------------------------------process other body parts (if any)
        result:=true;
        {$ifdef PROFILING}
        writeln(stderr,'PROFILING;reduceExpression.resolveEach.parseBodyOk;' + FloatToStr(profiler.Elapsed));
        {$endif}
      end;

    VAR itList:P_listLiteral;
        i,j:longint;
        eachLocation:T_tokenLocation;
        resultLiteral:P_literal;

    PROCEDURE inlineTokensBetween; inline;
      VAR k:longint;
      begin
        for k:=0 to length(tokensBetween)-1 do begin
          t^.next:=recycler.newToken(tokensBetween[k]);
          t:=t^.next;
        end;
      end;

    VAR future:array of P_futureTask;
    PROCEDURE initFutures;
      VAR i,j,k:longint;
      begin
        k:=0;
        setLength(future,length(bodyRule)*itList^.size);
        for i:=0 to itList^.size-1 do
        for j:=0 to length(bodyRule)-1 do begin
          if errorLevel<el3_evalError then begin
            future[k]:=pendingTasks.enqueue(bodyRule[j],itList^.value(i),recycler);
            if future[k]^.state=fts_errorOnInitialization then raiseError(el3_evalError,'Error initializing future.',eachLocation);
          end else future[k]:=nil;
          inc(k);
        end;
      end;

    PROCEDURE addToAggregation(L:P_literal; CONST doDispose:boolean); inline;
      VAR newLit:P_literal;
      begin
        if L=nil then begin
          if resultLiteral=nil then resultLiteral:=newErrorLiteral;
          raiseError(el3_evalError,'Error performing aggregation in each-construct - NIL literal encountered',eachLocation);
          exit;
        end else if L^.literalType=lt_void then exit;
        if aggregatorPresent then begin
          if (tokensBetween[0]^.tokType=tt_operatorConcat) then begin
            if resultLiteral=nil then resultLiteral:=newListLiteral;
            if (L^.literalType in [lt_error, lt_boolean, lt_int, lt_real, lt_string, lt_expression])
            then P_listLiteral(resultLiteral)^.append   (L, true)
            else P_listLiteral(resultLiteral)^.appendAll(P_listLiteral(L));
          end else begin
            if resultLiteral=nil then begin
              resultLiteral:=L; L^.rereference;
            end else begin
              newLit:=resolveOperator(resultLiteral,
                                      tokensBetween[0]^.tokType,
                                      L,
                                      eachLocation);
              if newLit<>nil then begin
                disposeLiteral(resultLiteral);
                resultLiteral:=newLit;
              end else raiseError(el3_evalError,'Error performing aggregation in each-construct with aggregator ('+tokensBetween[0]^.toString+')',eachLocation);
            end;
          end;
        end else begin
          if resultLiteral=nil then resultLiteral:=newListLiteral;
          P_listLiteral(resultLiteral)^.append(L,true);
        end;
        if doDispose then disposeLiteral(L);
      end;

    PROCEDURE finalizeAggregation; inline;
      begin
        first^.tokType:=tt_literal;
        if resultLiteral=nil then begin
          if aggregatorPresent then begin
            first^.data:=newErrorLiteral;
            raiseError(el3_evalError,'Error performing aggregation in each-construct - NIL literal encountered',eachLocation);
          end else first^.data:=newListLiteral;
        end else first^.data:=resultLiteral;
        first^.txt:='';
        first^.next:=recycler.disposeToken(bracketClosingEach);
      end;

    PROCEDURE aggregateFutures;
      VAR k:longint;
      begin
        if (aggregatorIsPrimitive or not(aggregatorPresent)) then begin
          for k:=0 to length(future)-1 do if future[k]<>nil then begin
            while (future[k]^.state in [fts_pending,fts_evaluating]) do pendingTasks.activeDeqeue(callDepth+1,recycler);
            addToAggregation(future[k]^.getResultAsLiteral,true);
            future[k]^.free(recycler);
            dispose(future[k],proFormaDestroy);
          end;
          finalizeAggregation;
        end else begin
          for k:=0 to length(future)-1 do if future[k]<>nil then begin
            if k>0 then inlineTokensBetween;
            while (future[k]^.state in [fts_pending,fts_evaluating]) do pendingTasks.activeDeqeue(callDepth+1,recycler);
            t^.next:=recycler.newToken(eachLocation,'',tt_literal,future[k]^.getResultAsLiteral);
            future[k]^.free(recycler);
            dispose(future[k],proFormaDestroy);
            t:=t^.next;
          end;
          t^.next:=bracketClosingEach;
        end;
        setLength(future,0);
      end;

    begin
      eachLocation:=first^.location;
      if not(parseBodyOk) then exit;

      if (P_literal(first^.data)^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression])
      then itList:=newOneElementListLiteral(first^.data,false)
      else itList:=first^.data;

      resultLiteral:=nil;

      first^.data:=nil;
      first^.txt:='';
      //iterate over itList----------------------------------------------------------
      t:=first;
      if aggregatorPresent and (itList^.size=0) then
        raiseError(el3_evalError,'Each construct with aggregator is invalid for empty lists to iterate',tokensBetween[0]^.location)
      else if length(bodyRule)>0 then begin
        if inParallel then begin
          initFutures;
          aggregateFutures;
        end else if (aggregatorIsPrimitive or not(aggregatorPresent)) and (callDepth<STACK_DEPTH_LIMIT) then begin
          for i:=0 to itList^.size-1 do for j:=0 to length(bodyRule)-1 do if errorLevel<=el2_warning then
            addToAggregation(bodyRule[j]^.directEvaluateUnary(itList^.value(i),nil,callDepth+1,recycler),true);
          finalizeAggregation;
        end else begin
          for i:=0 to itList^.size-1 do if errorLevel<=el2_warning then
          for j:=0 to length(bodyRule)-1 do begin
            if (i<>0) or (j<>0) then inlineTokensBetween;
            t^.next:=recycler.newToken(eachLocation,'',tt_literal,bodyRuleLiteral[j]);
            bodyRuleLiteral[j]^.rereference;
            t:=t^.next;
            t^.next:=recycler.newToken(eachLocation,'',tt_parList,newOneElementListLiteral(itList^.value(i),true));
            t:=t^.next;
          end;
          t^.next:=bracketClosingEach;
        end;
      end else begin
        if not(aggregatorPresent) then begin
          resultLiteral:=itList;
          resultLiteral^.rereference;
          finalizeAggregation;
        end else if aggregatorIsPrimitive then begin
          for i:=0 to itList^.size-1 do if errorLevel<=el2_warning then
            addToAggregation(itList^.value(i),false);
          finalizeAggregation;
        end else begin
          for i:=0 to itList^.size-1 do if errorLevel<=el2_warning then begin
            if (i<>0) then inlineTokensBetween;
            t^.next:=recycler.newToken(eachLocation,'',tt_literal,itList^.value(i));
            itList^.value(i)^.rereference;
            t:=t^.next;
          end;
          t^.next:=bracketClosingEach;
        end;
      end;
      //----------------------------------------------------------iterate over itList
      //cleanup----------------------------------------------------------------------
      for i:=0 to length(bodyRule)-1 do disposeLiteral(bodyRuleLiteral[i]);
      for i:=0 to length(tokensBetween)-1 do recycler.disposeToken(tokensBetween[i]);
      disposeLiteral(itList);
      //----------------------------------------------------------------------cleanup
      didSubstitution:=true;
    end;

  PROCEDURE applyRule(CONST parameterListToken:P_token; CONST firstTokenAfterCall:P_token);
    VAR firstReplace,lastReplace:P_token;
        newLiteral:P_literal;
        parameterListLiteral:P_listLiteral;
        inlineRule:P_subrule;
    begin
      if parameterListToken=nil then parameterListLiteral:=nil
                                else parameterListLiteral:=parameterListToken^.data;
      if (first^.tokType in [tt_localUserRulePointer,tt_importedUserRulePointer]) and not(pureOnly) then begin
        if not(P_rule(first^.data)^.replaces(parameterListLiteral,firstReplace,lastReplace,callDepth,first^.tokType=tt_localUserRulePointer,recycler)) then begin
          if parameterListLiteral=nil
          then raiseError(el3_evalError,'Cannot apply user defined rule '+P_rule(first^.data)^.id+' to empty parameter list',first^.location)
          else raiseError(el3_evalError,'Cannot apply user defined rule '+P_rule(first^.data)^.id+' to parameter list '+parameterListLiteral^.toParameterListString(true),first^.location);
          exit;
        end;
      end else if (first^.tokType=tt_intrinsicRulePointer) and (not(pureOnly) or (pureIntrinsicFunctions.contains(first^.data)))  then begin
        {$ifdef PROFILING}
        profiler.Clear;
        profiler.Start;
        {$endif}
        newLiteral:=T_intFuncCallback(first^.data)(parameterListLiteral,first^.location);
        {$ifdef PROFILING}
        writeln(stderr,'PROFILING;applying intrinsic rule;' + FloatToStr(profiler.Elapsed));
        {$endif}
        if newLiteral<>nil then begin
          firstReplace:=recycler.newToken(first^.location,'',tt_literal,newLiteral);
          lastReplace:=firstReplace;
        end else begin
          if parameterListLiteral=nil
          then raiseError(el3_evalError,'Cannot apply intrinsic rule '+first^.txt+' to empty parameter list',first^.location)
          else raiseError(el3_evalError,'Cannot apply intrinsic rule '+first^.txt+' to parameter list '+parameterListLiteral^.toParameterListString(true),first^.location);
          exit;
        end;
      end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_expression) and not(pureOnly) then begin
        inlineRule:=P_expressionLiteral(first^.data)^.value;
        //failing "replaces" for inline rules will raise evaluation error.
        if not(inlineRule^.replaces(parameterListLiteral,P_expressionLiteral(first^.data),firstReplace,lastReplace,recycler)) then exit;
      end else begin
        if not(pureOnly) then raiseError(el3_evalError,'Trying to apply a rule which is no rule!',first^.location);
        exit;
      end;
      recycler.disposeToken(first);
      if parameterListToken<>nil then recycler.disposeToken(parameterListToken);
      first:=firstReplace;
      lastReplace^.next:=firstTokenAfterCall;
      didSubstitution:=true;
    end;

  PROCEDURE resolveInlineIf(CONST conditionLit:boolean);
    VAR p,prev,tokenBeforeElse,lastThen:P_token;
        bracketLevel:longint=0;
    begin
      {$ifdef PROFILING}
      profiler.Clear;
      profiler.Start;
      {$endif}
      prev:=first;
      stack_push; //push "?"
      p:=first;
      while (p<>nil) and not((p^.tokType=tt_iifElse) and (bracketLevel=0)) do begin
        case p^.tokType of
          tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck: inc(bracketLevel);
          tt_braceClose,                         tt_listBraceClose ,tt_iifElse : dec(bracketLevel);
        end;
        prev:=p; p:=p^.next;
      end;
      if not((p<>nil) and (p^.tokType=tt_iifElse) and (bracketLevel=0)) then begin
        stack_popLink;
        raiseError(el3_evalError,'Cannot evaluate inline-if; cannot locate then-marker',first^.location);
        exit;
      end;
      tokenBeforeElse:=prev;
      while (p<>nil) and not((p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma]) and (bracketLevel=-1)) do begin
        case p^.tokType of
          tt_braceOpen ,tt_each,tt_parallelEach, tt_listBraceOpen  ,tt_iifCheck: inc(bracketLevel);
          tt_braceClose,                         tt_listBraceClose ,tt_iifElse : dec(bracketLevel);
        end;
        prev:=p; p:=p^.next;
      end;
      if  not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma]) and (bracketLevel=-1)) then begin
        stack_popLink;
        raiseError(el3_evalError,'Cannot evaluate inline-if; cannot locate end of then-expression',first^.location);
        exit;
      end;
      lastThen:=prev;
      if conditionLit then begin
        //take then-subexpression -> drop else-subexpression
        p:=tokenBeforeElse^.next;              //store tt_iifElse-token
        tokenBeforeElse^.next:=lastThen^.next; //unlink else-expression (head)
        lastThen^.next:=nil;                   //unlink else-expression (tail);
        recycler.cascadeDisposeToken(p);       //dispose else-expression
      end else begin
        //take else-subexpression -> drop then-subexpression
        p:=first;
        first:=tokenBeforeElse^.next^.next;
        tokenBeforeElse^.next^.next:=nil;
        recycler.cascadeDisposeToken(p);
      end;
      stack_popDestroy; //pop "?"
      stack_popDestroy; //pop condition literal
      {$ifdef PROFILING}
      writeln(stderr,'PROFILING;resolve IIF;' + FloatToStr(profiler.Elapsed));
      {$endif}
    end;

  PROCEDURE applyCheck;
    VAR checkResult:boolean;
    begin
      checkResult:=P_literal(first^.data)^.literalType in C_matchingTypes[first^.next^.tokType];
      first^.next:=recycler.disposeToken(first^.next);
      disposeLiteral(first^.data);
      first^.data:=newBoolLiteral(checkResult);
      didSubstitution:=true;
    end;

  PROCEDURE applyMutation;
    VAR newValue:P_literal;
    begin
      newValue:=first^.next^.data;
      P_rule(first^.data)^.setMutableValue(newValue);
      first:=recycler.disposeToken(first);
    end;

  PROCEDURE startOrPushParameterList; inline;
    begin
      setLength(stack,length(stack)+2);
      stack[length(stack)-2]:=first;
      first:=first^.next;
      if first^.tokType=tt_braceOpen then begin
        first^.tokType:=tt_parList_constructor;
        first^.data:=newListLiteral;
      end;
      stack[length(stack)-1]:=first;
      first:=first^.next;
      didSubstitution:=true;
    end;

  begin
    setLength(stack,0);
    try
    repeat
      didSubstitution:=false;
      //writeln(tokensToString(first));
      //writeln('                                           ',cTokType(-1),' ] ',cTokType(0),' | ',cTokType(1));
      case cTokType(0) of
        tt_literal: case cTokType(-1) of
          tt_unaryOpPlus: stack_popDestroy;
          tt_unaryOpMinus: begin
            newLit:=P_literal(first^.data)^.negate(stack[length(stack)-1]^.location);
            disposeLiteral(first^.data);
            first^.data:=newLit;
            stack_popDestroy;
          end;
          tt_comparatorEq..tt_comparatorListEq: begin //operators with special cascading
            tt:=cTokType(1);
            if (tt in [tt_comparatorEq..tt_comparatorListEq]) then begin
              // x < y < z -> [x < y] and y < z
              {$ifdef PROFILING}
              profiler.Clear;
              profiler.Start;
              {$endif}
              newLit:=resolveOperator(stack[length(stack)-2]^.data,
                                      stack[length(stack)-1]^.tokType,
                                      first^.data,
                                      stack[length(stack)-1]^.location);
              //LHS literal is now result of first comparison (still a literal)
              disposeLiteral(stack[length(stack)-2]^.data);
              stack[length(stack)-2]^.data:=newLit;
              //applied comparator is replaced by operator 'and'
              stack[length(stack)-1]^.tokType:=tt_operatorAnd;
              didSubstitution:=true;
              {$ifdef PROFILING}
              writeln(stderr,'PROFILING;resolveOperator (A);' + FloatToStr(profiler.Elapsed));
              {$endif}
            end else if (tt in [tt_comparatorEq..tt_operatorIn]) and (C_opPrecedence[tt]>=C_opPrecedence[cTokType(-1)]) or
                        (tt in [tt_braceClose,tt_listBraceClose,tt_EOL,tt_separatorComma,tt_separatorCnt, tt_iifCheck, tt_iifElse]) then begin
              {$ifdef PROFILING}
              profiler.Clear;
              profiler.Start;
              {$endif}
              newLit:=resolveOperator(stack[length(stack)-2]^.data,
                                      stack[length(stack)-1]^.tokType,
                                      first^.data,
                                      stack[length(stack)-1]^.location);
              disposeLiteral(first^.data);
              first^.data:=newLit; //store new literal in head
              stack_popDestroy; //pop operator from stack
              stack_popDestroy; //pop LHS-Literal from stack
              {$ifdef PROFILING}
              writeln(stderr,'PROFILING;resolveOperator (B);' + FloatToStr(profiler.Elapsed));
              {$endif}
            end else if tt=tt_parList then applyRule(first^.next,first^.next^.next)
            else if tt in [tt_typeCheckScalar..tt_typeCheckEmptyList] then applyCheck
            else begin
              stack_push;
              stack_push;
            end;
          end;
          tt_operatorAnd, tt_operatorOr, //operators with lazy evaluation ?
          tt_operatorXor, tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt,
          tt_operatorMod, tt_operatorPot, tt_operatorStrConcat, tt_operatorExtractL0, tt_operatorExtractL1,
          tt_operatorExtractL2, tt_operatorExtractL3, tt_operatorConcat, tt_operatorIn: begin
            tt:=cTokType(1);
            if (tt in [tt_comparatorEq..tt_operatorIn]) and (C_opPrecedence[tt]>=C_opPrecedence[cTokType(-1)]) or
               (tt in [tt_braceClose,tt_listBraceClose,tt_EOL,tt_separatorComma,tt_separatorCnt, tt_iifCheck, tt_iifElse]) then begin
              {$ifdef PROFILING}
              profiler.Clear;
              profiler.Start;
              {$endif}
              newLit:=resolveOperator(stack[length(stack)-2]^.data,
                                      stack[length(stack)-1]^.tokType,
                                      first^.data,
                                      stack[length(stack)-1]^.location);
              disposeLiteral(first^.data);
              first^.data:=newLit; //store new literal in head
              stack_popDestroy; //pop operator from stack
              stack_popDestroy; //pop LHS-Literal from stack
              {$ifdef PROFILING}
              writeln(stderr,'PROFILING;resolveOperator (C);' + FloatToStr(profiler.Elapsed));
              {$endif}
            end else if tt=tt_parList then applyRule(first^.next,first^.next^.next)
            else if tt in [tt_parList_constructor,tt_braceOpen] then startOrPushParameterList
            else if tt in [tt_typeCheckScalar..tt_typeCheckEmptyList] then applyCheck
            else begin
              stack_push;
              stack_push;
            end;
          end;
          tt_braceOpen : case cTokType(1) of // ( | <Lit>
            tt_braceClose: begin  // ( | <Lit> )
              stack_popDestroy;
              first^.next:=recycler.disposeToken(first^.next);
              didSubstitution:=true;
            end;
            tt_braceOpen, tt_parList_constructor: startOrPushParameterList;
            tt_parList:  applyRule(first^.next,first^.next^.next);
            tt_comparatorEq..tt_operatorIn: begin stack_push; stack_push; end;
            tt_iifCheck: stack_push;
            tt_typeCheckScalar..tt_typeCheckEmptyList: applyCheck;
            else raiseError(el3_evalError,'Unable to resolve paranthesis!',stack[length(stack)-1]^.location);
          end;
          tt_list_constructor: case cTokType(1) of
            tt_separatorComma: begin // [ | <Lit> ,
              P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              didSubstitution:=true;
            end;
            tt_separatorCnt: begin // [ | <Lit> ,
              P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location);
              P_listLiteral(stack[length(stack)-1]^.data)^.setRangeAppend;
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              didSubstitution:=true;
            end;
            tt_listBraceClose: begin // [ | <Lit> ]
              P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              stack_popLink;
              first^.tokType:=tt_literal;
              didSubstitution:=true;
            end;
            tt_braceOpen, tt_parList_constructor: startOrPushParameterList;
            tt_parList:  applyRule(first^.next,first^.next^.next);
            tt_comparatorEq..tt_operatorIn: begin stack_push; stack_push; end;
            tt_iifCheck: stack_push;
            tt_typeCheckScalar..tt_typeCheckEmptyList: applyCheck;
          end;
          tt_parList_constructor: case cTokType(1) of
            tt_braceClose: begin // <F> <par(> | <Lit> ) -> <F> <par>
              P_listLiteral(stack[length(stack)-1]^.data)^.append(first^.data,true);
              stack[length(stack)-1]^.tokType:=tt_parList; //mutate <tt_parList_constructor> -> <tt_parList>
              first:=recycler.disposeToken(first); //dispose literal
              first:=recycler.disposeToken(first); //dispose closing bracket
              stack_popLink; //pop parameter list
              stack_popLink; //pop FUNCTION
            end;
            tt_separatorComma: begin // <F> <par(> | <Lit> , -> <F> <par(> |
              P_listLiteral(stack[length(stack)-1]^.data)^.append(first^.data,true);
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              didSubstitution:=true;
            end;
            tt_braceOpen, tt_parList_constructor: startOrPushParameterList;
            tt_parList:  applyRule(first^.next,first^.next^.next);
            tt_comparatorEq..tt_operatorIn: begin stack_push; stack_push; end;
            tt_iifCheck: stack_push;
            tt_typeCheckScalar..tt_typeCheckEmptyList: applyCheck;
          end;
          tt_each,tt_parallelEach: if (stack[length(stack)-1]^.data=nil) then case cTokType(1) of
            tt_separatorComma: if cTokType(2)<>tt_eol then begin
              stack[length(stack)-1]^.data:=first^.data; //store literal list in each-token
              P_literal(first^.data)^.reReference; //rereference literal to prevent destruction
              first:=recycler.disposeToken(first);
              first:=recycler.disposeToken(first);
              stack_popLink;
            end else raiseError(el3_evalError,'Invalid syntax for each construct - EOL comes to early!',stack[length(stack)-1]^.location);
            tt_braceOpen, tt_parList_constructor: startOrPushParameterList;
            tt_parList:  applyRule(first^.next,first^.next^.next);
            tt_comparatorEq..tt_operatorIn: begin stack_push; stack_push; end;
            tt_iifCheck: stack_push;
            tt_typeCheckScalar..tt_typeCheckEmptyList: applyCheck;
            else raiseError(el3_evalError,'Invalid syntax for each construct - token "'+safeTokenToString(first^.next)+'" cannot be interpreted after each!',stack[length(stack)-1]^.location);
          end else raiseError(el3_evalError,'Invalid syntax for each construct - each already has a literal!',stack[length(stack)-1]^.location);
          tt_mutate: begin
            case cTokType(1) of
              tt_comparatorEq..tt_operatorIn: begin
                stack_push;
                stack_push;
              end;
              tt_braceClose,tt_separatorCnt,tt_separatorComma,tt_eol,tt_expBraceClose,tt_listBraceClose: if not(pureOnly) then begin
                stack_popLink;
                applyMutation;
              end;
            end;
          end;
          else case cTokType(1) of
            tt_braceOpen, tt_parList_constructor: startOrPushParameterList;
            tt_parList:  applyRule(first^.next,first^.next^.next);
            tt_comparatorEq..tt_operatorIn: begin stack_push; stack_push; end;
            tt_iifCheck: stack_push;
            tt_typeCheckScalar..tt_typeCheckEmptyList: applyCheck;
          end;
        end;
        tt_operatorPlus: begin first^.tokType:=tt_unaryOpPlus; stack_push; end;
        tt_operatorMinus: begin first^.tokType:=tt_unaryOpMinus; stack_push; end;
        tt_unaryOpPlus, tt_unaryOpMinus: stack_push;
        tt_comparatorEq..tt_operatorXor,tt_operatorMult..tt_operatorPot,tt_operatorStrConcat..tt_operatorIn:
          raiseError(el3_evalError,'Undefined prefix operator '+first^.toString,first^.location);
        tt_braceOpen: stack_push;
        tt_expBraceOpen: if not(pureOnly) then begin
          digestInlineExpression(first,recycler);
          didSubstitution:=true;
        end;
        tt_braceClose: if cTokType(-1)=tt_parList_constructor then begin
          first:=recycler.disposeToken(first);
          stack_popLink;
          first^.tokType:=tt_parList;
          stack_popLink;
          didSubstitution:=true;
        end;
        tt_listBraceOpen: if cTokType(1)=tt_listBraceClose then begin
          //empty list
          first^.data:=newListLiteral;
          first^.tokType:=tt_literal;
          first^.next:=recycler.disposeToken(first^.next);
          didSubstitution:=true;
        end else begin
          first^.data:=newListLiteral;
          first^.tokType:=tt_list_constructor;
          stack_push;
        end;
        tt_list_constructor: stack_push;
        tt_identifier: begin
          if first^.data=nil then first^.data:=guessPackageForToken(first^);
          P_package(first^.data)^.resolveRuleId(first^,pureOnly);
          if first^.tokType<>tt_identifier then didSubstitution:=true;
        end;
        tt_each,tt_parallelEach: if (first^.data=nil) then stack_push else resolveEach((first^.tokType=tt_parallelEach) and (callDepth<STACK_DEPTH_LIMIT));
        tt_mutate: stack_push;
        tt_localUserRulePointer, tt_importedUserRulePointer, tt_intrinsicRulePointer : case cTokType(1) of
          tt_braceOpen, tt_parList_constructor: startOrPushParameterList;
          tt_parList: applyRule(first^.next,first^.next^.next);
          tt_braceClose,tt_listBraceClose,tt_comparatorEq..tt_operatorIn,tt_EOL,tt_iifCheck,tt_iifElse,tt_separatorCnt,tt_separatorComma: applyRule(nil,first^.next);
        end;
        tt_iifCheck: if (cTokType(-1)=tt_literal) and (P_literal(stack[length(stack)-1]^.data)^.literalType=lt_boolean) then begin
          resolveInlineIf(P_boolLiteral(stack[length(stack)-1]^.data)^.value);
        end else raiseError(el3_evalError,'Invalid syntax for inline-if; first operand is expected to be a boolean.',first^.location);
      end;
      //printState;
    until not(didSubstitution) or (errorLevel>=el3_evalError);
    except
      on EOutOfMemory do begin
        raiseError(el5_systemError,'An unhandled EOutOfMemory exception was caught in reduceExpression on callDepth='+IntToStr(callDepth),C_nilTokenLocation);
        while (length(stack)>0) do stack_popDestroy;
      end else begin
        raiseError(el5_systemError,'An unhandled, unknown exception was caught in reduceExpression on callDepth='+IntToStr(callDepth),C_nilTokenLocation);
        while (length(stack)>0) do stack_popDestroy;
      end;
    end;
    if callDepth=0 then begin
      if first=nil then raiseError(el0_allOkay,'Reduce expression done',C_nilTokenLocation)
                   else raiseError(el0_allOkay,'Reduce expression done',first^.location);
    end;
    if (errorLevel>=el5_systemError) and (callDepth=0) then begin
      raiseError(el1_note,'Cleaning up partial result after system error',C_nilTokenLocation);
      while (length(stack)>0) do stack_popDestroy;
      recycler.cascadeDisposeToken(first);
    end else while (length(stack)>0) do stack_popLink;
  end;

{$endif}
