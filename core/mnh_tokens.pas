UNIT mnh_tokens;
INTERFACE
USES myGenerics,mnh_constants,math,mnh_litvar,sysutils,fileWrappers,mnh_stringUtil,mnh_funcs,mnh_out_adapters;
{$define include_interface}
TYPE
  {$i mnh_tokens_token}
  {$i mnh_tokens_pattern}    
  P_package=^T_package;
  {$i mnh_tokens_subrule}    
  {$i mnh_tokens_rule}    
    
  T_package=object
    private 
      publicRules:specialize G_stringKeyMap<P_rule>;
      localRules:specialize G_stringKeyMap<P_rule>;
      packageUses:array of record
        id:string;
        pack:P_package;
      end;
      ready:boolean;
      codeProvider:P_codeProvider;
    public
      CONSTRUCTOR create(CONST provider:P_codeProvider);
      PROCEDURE load;
      PROCEDURE clear;
      DESTRUCTOR destroy;
      PROCEDURE resolveRuleId(VAR token:T_token);
      FUNCTION ensureLocalRuleId(CONST ruleId:ansistring):P_rule;
      FUNCTION ensurePublicRuleId(CONST ruleId:ansistring):P_rule;
  end;
  
FUNCTION isReloadOfAllPackagesIndicated:boolean;
FUNCTION isReloadOfMainPackageIndicated:boolean;
PROCEDURE reloadAllPackages;
PROCEDURE reloadMainPackage;
PROCEDURE clearAllPackages;
PROCEDURE initMainPackage(CONST filename:ansistring);
PROCEDURE initMainPackage(CONST directInputWrapper:P_directInputWrapper);
{$undef include_interface}
IMPLEMENTATION
{$define include_implementation}
{$i mnh_tokens_token}
{$i mnh_tokens_pattern}
{$i mnh_tokens_subrule}
{$i mnh_tokens_rule}
VAR packages:array of P_package;
FUNCTION isReloadOfAllPackagesIndicated:boolean;
  VAR i:longint;
  begin
    for i:=1 to length(packages)-1 do if packages[i]^.codeProvider^.fileChanged then exit(true);
    result:=false;
  end;

FUNCTION isReloadOfMainPackageIndicated:boolean;
  begin
    result:=(length(packages)>0) and packages[0]^.codeProvider^.fileChanged;
  end;

PROCEDURE reloadAllPackages;
  VAR i:longint;
  begin
    if length(packages)<=0 then exit;
    for i:=length(packages)-1 downto 1 do dispose(packages[i],destroy);      
    setLength(packages,1);
    reloadMainPackage;  
  end;
  
PROCEDURE reloadMainPackage;
  begin
    outAdapter.clearErrors;    
    if length(packages)>0 then packages[0]^.load;
  end;

PROCEDURE clearAllPackages;
  VAR i:longint;
  begin
    outAdapter.clearErrors;
    clearSourceScanPaths;
    for i:=length(packages)-1 downto 0 do dispose(packages[i],destroy);      
    setLength(packages,0);  
  end;

PROCEDURE initMainPackage(CONST filename:ansistring);
  VAR fileWrapper:P_fileWrapper;
  begin
    if length(packages)>0 then exit;
    outAdapter.clearErrors;    
    new(fileWrapper,create(filename));
    if fileWrapper^.exists then begin
      addSourceScanPath(extractFilePath(filename));
      setLength(packages,1);
      new(packages[0],create(fileWrapper));          
    end else dispose(fileWrapper,destroy);
  end;

PROCEDURE initMainPackage(CONST directInputWrapper:P_directInputWrapper);
  begin
    if length(packages)>0 then exit;
    outAdapter.clearErrors;    
    setLength(packages,1);
    new(packages[0],create(directInputWrapper));    
  end;

FUNCTION loadPackage(CONST packageId,tokenLocation:ansistring):P_package;
  VAR i:longint;      
      newSource:P_fileWrapper;      
  begin
    for i:=0 to length(packages)-1 do if packages[i]^.codeProvider^.fileIdentifier = packageId then begin
      if packages[i]^.ready then exit(packages[i])
                            else begin
                              outAdapter.raiseError(el4_parsingError,'Cyclic package dependencies encountered; already loading "'+packageId+'"',tokenLocation);
                              exit(nil);
                            end;
    end;
    newSource:=locateSource(packageId);
    if newSource<>nil then begin
      setLength(packages,length(packages)+1);
      new(packages[length(packages)-1],create(newSource));
    end else begin
      outAdapter.raiseError(el4_parsingError,'Cannot locate package for id "'+packageId+'"',tokenLocation);
      result:=nil;
    end;
  end;


PROCEDURE T_package.load;
  FUNCTION firstToken(VAR line:ansistring; VAR colCounter:longint; CONST lineLocation:ansistring):P_token;
    VAR parsedLength:longint=0;
    FUNCTION resultFrom(CONST len:longint; CONST tt:T_tokenType):P_token;
      begin
        if tt in [tt_each,tt_literal,tt_set] 
        then result:=newToken(lineLocation,''              ,tt)
        else result:=newToken(lineLocation,copy(line,1,len),tt);        
        parsedLength:=len;
      end;
      
    FUNCTION leadingId:ansistring;
      VAR i:longint;
      begin
        i:=1;
        while (i<length(line)) and (line[i+1] in ['a'..'z','A'..'Z','0'..'9','_']) do inc(i);
        parsedLength:=i;
        result:=copy(line,1,i);
      end;
      
    PROCEDURE removeLeadingBlanks();
      VAR i:longint;
      begin
        i:=1;
        while (i<=length(line)) and (line[i] in [' ',C_lineBreakChar,C_tabChar]) do inc(i);
        if i>1 then line:=copy(line,i,length(line)+1-i);
        inc(colCounter,i-1);
      end;
      
    VAR id:ansistring;
        intrinsicFuncPtr:T_intFuncCallback;
        literal:P_literal;
    begin
      removeLeadingBlanks;
      result:=nil;
      case line[1] of
        '0'..'9': begin
          literal:=parseNumber(line,parsedLength);
          if literal=nil then outAdapter.raiseError(el4_parsingError,'Cannot parse numeric literal '+line,lineLocation)
          else begin
            result:=resultFrom(parsedLength,tt_literal);
            result^.data:=literal;
          end;
        end;
        '"': begin
          id:=unescapeString(line,parsedLength);
          if parsedLength=0 then outAdapter.raiseError(el4_parsingError,'Cannot parse string literal '+line,lineLocation)
          else begin
            result:=resultFrom(parsedLength,tt_literal);
            result^.data:=newStringLiteral(id);
          end;        
        end;
        '$': begin
          id:=leadingId;
          result:=resultFrom(length(id),tt_parameterIdentifier);
        end;
        'a'..'z','A'..'Z': begin
          id:=leadingId;
          if      id='xor'   then result:=resultFrom(3,tt_operatorXor   )
          else if id='set'   then result:=resultFrom(3,tt_set           )
          else if id='or'    then result:=resultFrom(2,tt_operatorOr    )
          else if id='mod'   then result:=resultFrom(3,tt_operatorMod   )
          else if id='in'    then result:=resultFrom(2,tt_operatorIn    )
          else if id='each'  then result:=resultFrom(4,tt_each          )
          else if id='div'   then result:=resultFrom(3,tt_operatorDivInt)
          else if id='and'   then result:=resultFrom(3,tt_operatorAnd   )
          else if id='true'  then begin result:=resultFrom(4,tt_literal); result^.data:=newBoolLiteral(true); end
          else if id='false' then begin result:=resultFrom(5,tt_literal); result^.data:=newBoolLiteral(false); end
          else if id='Nan'   then begin result:=resultFrom(3,tt_literal); result^.data:=newRealLiteral(Nan); end
          else if id='Inf'   then begin result:=resultFrom(3,tt_literal); result^.data:=newRealLiteral(Infinity); end
          else if intrinsicRuleMap.containsKey(id,intrinsicFuncPtr) then begin
            result:=resultFrom(length(id),tt_intrinsicRulePointer);
            result^.data:=intrinsicFuncPtr;
          end else begin
            result:=resultFrom(length(id),tt_identifier);
          end;
        end;
        ';': result:=resultFrom(1,tt_eol);
        '}': result:=resultFrom(1,tt_expBraceClose);   
        '|': result:=resultFrom(1,tt_operatorConcat);   
        '{': result:=resultFrom(1,tt_expBraceOpen);    
        '^': result:=resultFrom(1,tt_operatorPot);      
        ']': result:=resultFrom(1,tt_listBraceClose);     
        '[': result:=resultFrom(1,tt_listBraceOpen); 
        '?': result:=resultFrom(1,tt_iifCheck);         
        ',': result:=resultFrom(1,tt_separatorComma);
        '+': result:=resultFrom(1,tt_operatorPlus);
        ')': result:=resultFrom(1,tt_braceClose);
        '(': result:=resultFrom(1,tt_braceOpen);
        '&': result:=resultFrom(1,tt_operatorStrConcat);
        '-': if startsWith(line,'->')        then result:=resultFrom(2,tt_declare)
                                             else result:=resultFrom(1,tt_operatorMinus);
        '*': if startsWith(line,'**')        then result:=resultFrom(2,tt_operatorPot) 
                                             else result:=resultFrom(1,tt_operatorMult);
        '>': if startsWith(line,'>=')        then result:=resultFrom(2,tt_comparatorGeq)
                                             else result:=resultFrom(1,tt_comparatorGrt); 
        '=': if      startsWith(line,'=[]')  then result:=resultFrom(3,tt_typeCheckEmptyList)
             else if startsWith(line,'==')   then result:=resultFrom(2,tt_comparatorListEq)
             else                                 result:=resultFrom(1,tt_comparatorEq); 
        '<': if      startsWith(line,'<>[]') then result:=resultFrom(4,tt_typeCheckNonemptyList)
             else if startsWith(line,'<>'  ) then result:=resultFrom(2,tt_comparatorNeq)
             else if startsWith(line,'<='  ) then result:=resultFrom(2,tt_comparatorLeq)
             else                                 result:=resultFrom(1,tt_comparatorLss);
        '/': if      startsWith(line,'//')   then begin result:=nil; parsedLength:=length(line); end //comments
                                             else result:=resultFrom(1,tt_operatorDivReal);
        '%': if      startsWith(line,'%%%%') then result:=resultFrom(4,tt_operatorExtractL3)
             else if startsWith(line,'%%%')  then result:=resultFrom(3,tt_operatorExtractL2)
             else if startsWith(line,'%%')   then result:=resultFrom(2,tt_operatorExtractL1)
             else                                 result:=resultFrom(1,tt_operatorExtractL0);
        '.': if startsWith(line,'..')        then result:=resultFrom(2,tt_separatorCnt)
                                             else outAdapter.raiseError(el4_parsingError,'Cannot parse: '+line,lineLocation);
        ':': if startsWith(line,':=') then result:=resultFrom(2,tt_assign)
             else if (length(line)>4) and (line[2] in ['b','e','i','l','n','s','r']) then begin
               id:=leadingId;
               if      id=':booleanList' then result:=newToken(lineLocation,'',tt_typeCheckBoolList  )
               else if id=':boolean'     then result:=newToken(lineLocation,'',tt_typeCheckBoolean   )
               else if id=':expression'  then result:=newToken(lineLocation,'',tt_typeCheckExpression)
               else if id=':intList'     then result:=newToken(lineLocation,'',tt_typeCheckIntList   )
               else if id=':int'         then result:=newToken(lineLocation,'',tt_typeCheckInt       )
               else if id=':list'        then result:=newToken(lineLocation,'',tt_typeCheckList      )
               else if id=':numericList' then result:=newToken(lineLocation,'',tt_typeCheckNumList   )
               else if id=':numeric'     then result:=newToken(lineLocation,'',tt_typeCheckNumeric   )
               else if id=':stringList'  then result:=newToken(lineLocation,'',tt_typeCheckStringList)
               else if id=':scalar'      then result:=newToken(lineLocation,'',tt_typeCheckScalar    )
               else if id=':string'      then result:=newToken(lineLocation,'',tt_typeCheckString    )
               else if id=':realList'    then result:=newToken(lineLocation,'',tt_typeCheckRealList  )
               else if id=':real'        then result:=newToken(lineLocation,'',tt_typeCheckReal      )
               else outAdapter.raiseError(el4_parsingError,'Cannot parse: '+line+C_lineBreakChar+'expected type check token (one of :scalar, :list, :boolean, :booleanList, :int, :intList, :real, :realList, :string, :stringList, :numeric, :numericList, :expression)',lineLocation);
             end else result:=resultFrom(1,tt_iifElse);
        else begin
          outAdapter.raiseError(el4_parsingError,'Cannot parse: '+line,lineLocation);
          line:='';
        end;
      end;
      if parsedLength>0 then begin
        line:=copy(line,parsedLength+1,length(line));
        inc(colCounter,parsedLength);
      end;      
    end;

  PROCEDURE predigestBeforeDeclarationParsing(VAR first:P_token);
    VAR this,next,prev:P_token;
    begin
      this:=first; prev:=nil;
      while this<>nil do begin
        next:=this^.next;
        if (this^.tokType in [tt_operatorMinus,tt_operatorPlus]) and 
           ((prev=nil) or (prev^.tokType in [tt_braceOpen,tt_listBraceOpen,tt_separatorCnt,tt_separatorComma,tt_set,tt_each,tt_expBraceOpen,tt_unaryOpMinus,tt_unaryOpPlus])) 
           and (this^.next<>nil) and (this^.next^.tokType in [tt_literal,tt_identifier,tt_braceOpen,tt_listBraceOpen,tt_expBraceOpen,tt_userRulePointer,tt_intrinsicRulePointer,tt_parameterIdentifier]) then begin
          if this^.tokType=tt_operatorMinus then begin
            if (next<>nil) and (next^.tokType = tt_literal) then begin            
              this^.tokType:=tt_literal;
              this^.data:=P_literal(next^.data)^.negate(this^.location);
              this^.next:=disposeToken(next);
            end else this^.tokType:=tt_unaryOpMinus;
          end else begin
            if prev=nil then begin
              first:=disposeToken(this);
              this:=first;
            end else begin
              prev^.next:=disposeToken(this);
              this:=next;
            end;
          end;
        end else if (this^.tokType=tt_listBraceOpen) and (this^.next<>nil) and (this^.next^.tokType=tt_listBraceClose) then begin
          this^.tokType:=tt_literal;
          this^.data:=newListLiteral;
          this^.next:=disposeToken(this^.next);
        end;
        prev:=this;
        this:=this^.next;
      end;
    end;
    
  PROCEDURE predigest(VAR first:P_token; CONST forceResolveIds:boolean);
    PROCEDURE digestInlineExpression(VAR rep:P_token);
      VAR t,prev,inlineRule:P_token;
          bracketLevel:longint=0;
          inlineSubRule:P_subrule;
      begin
        if (rep^.tokType<>tt_expBraceOpen) then begin      
          outAdapter.raiseError(el4_parsingError,'Error creating subrule from inline; expression does not start with "{"',rep^.location);
          exit;
        end;          
        t:=rep^.next; prev:=rep;  
        inlineRule:=t;
        while (t<>nil) and ((t^.tokType<>tt_expBraceClose) or (bracketLevel>0)) do begin
          case t^.tokType of
            tt_expBraceOpen: inc(bracketLevel);
            tt_expBraceClose: dec(bracketLevel);
          end;
          prev:=t;
          t:=t^.next;
        end;        
        if (t=nil) or (t^.tokType<>tt_expBraceClose) then begin
          outAdapter.raiseError(el4_parsingError,'Error creating subrule from inline; expression does not end with an }',rep^.location); 
          exit;
        end;     
        rep^.next:=t^.next; //remove expression from parent expression
        prev^.next:=nil; //unlink closing curly bracket
        disposeToken(t); //dispose closing curly bracket
        predigest(inlineRule,forceResolveIds);
        if outAdapter.errorLevel<=el2_warning then begin
          new(inlineSubRule,createFromInline(inlineRule,@self));
          if outAdapter.errorLevel<=el2_warning then begin
            rep^.tokType:=tt_literal;
            rep^.data:=newExpressionLiteral(inlineSubRule);
          end else dispose(inlineSubRule,destroy);
        end;
      end;
  
    VAR t:P_token;                
    begin
      t:=first; 
      while t^.next<>nil do begin
        case t^.tokType of
          tt_expBraceOpen: digestInlineExpression(t);
          tt_identifier: t^.data:=@self;
          tt_set: if  (t^.next            <>nil) and (t^.next^            .tokType=tt_braceOpen)
                  and (t^.next^.next      <>nil) and (t^.next^.next^      .tokType=tt_identifier)
                  and (t^.next^.next^.next<>nil) and (t^.next^.next^.next^.tokType=tt_separatorComma) then begin
            t^.data:=ensureLocalRuleId(t^.next^.next^.txt); 
            t^.next:=disposeToken(t^.next); //dispose (
            t^.next:=disposeToken(t^.next); //dispose <id>
            t^.next:=disposeToken(t^.next); //dispose ,
          end else begin
            outAdapter.raiseError(el4_parsingError,'Invalid set-expression; expected to start with "set(<id>,"',t^.location);
            exit;
          end;
          tt_each:if  (t^.next            <>nil) and (t^.next^            .tokType=tt_braceOpen)
                  and (t^.next^.next      <>nil) and (t^.next^.next^      .tokType=tt_identifier)
                  and (t^.next^.next^.next<>nil) and (t^.next^.next^.next^.tokType=tt_separatorComma) then begin
            t^.txt:=t^.next^.next^.txt;
            t^.data:=nil;
            t^.next:=disposeToken(disposeToken(disposeToken(t^.next))); //dispose ( , <id> and ","
          end else begin
            outAdapter.raiseError(el4_parsingError,'Invalid each-expression; expected to start with "each(<id>,"',t^.location);
            exit;
          end;
        end;
        t:=t^.next;
      end;
    end;
    
  VAR isFirstLine:boolean=true;
  PROCEDURE interpret(VAR first:P_token);    
    PROCEDURE interpretUseClause;
      VAR temp:P_token;
          i,j:longint;
          locationForErrorFeedback:ansistring;            
          newId:string;
      begin
        locationForErrorFeedback:=first^.location;
        temp:=first; first:=disposeToken(temp);
        while first<>nil do begin
          if first^.tokType=tt_identifier then begin
            newId:=first^.txt;
            //no duplicates are created; packages are always added at the end
            i:=0;
            while (i<length(packageUses)) and (packageUses[i].id<>newId) do inc(i);
            if i<length(packageUses) then for j:=i to length(packageUses)-2 do packageUses[j]:=packageUses[j+1]
                                     else setLength(packageUses,length(packageUses)+1);
            with packageUses[length(packageUses)-1] do begin
              id:=first^.txt;
              pack:=nil;
            end;            
          end else if first^.tokType<>tt_separatorComma then begin
            outAdapter.raiseError(el4_parsingError,'Cannot interpret use clause containing '+first^.toString,first^.location);
            exit;
          end;
          temp:=first; first:=disposeToken(temp);
        end;
        for i:=0 to length(packageUses)-1 do with packageUses[i] do pack:=loadPackage(id,locationForErrorFeedback);
      end;
      
    PROCEDURE reduceExpression(VAR first:P_token);
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
          disposeToken(stack[length(stack)-1]);
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
        
      PROCEDURE resolveEach;
        VAR bracketLevel,i,j,k:longint;
            t,p,bracketClosingEach:P_token;
            tokensBetween:array of P_token;
            bodyRule:array of P_expressionLiteral;
            bodyPart:array of array[0..1] of P_token;
            tempRule:P_subrule;
            itList:P_listLiteral;
            aggregatorPresent:boolean;
        begin 
          //first token is <each>-Token
          //find closing bracket and body parts 
          bracketLevel:=0; //note: first token will raise bracket Level to 1
          t:=first; p:=nil;
          if (first^.next<>nil) and (first^.next^.tokType<>tt_separatorComma) then begin
            setLength(bodyPart,1);
            bodyPart[0,0]:=first^.next;
          end else begin
            outAdapter.raiseError(el4_parsingError,'Invalid each-construct; Cannot find closing bracket.',first^.location);
            exit;          
          end;          
          while (t<>nil) and not((t^.tokType=tt_braceClose) and (bracketLevel=1)) do begin            
            if      t^.tokType in [tt_braceOpen,tt_set,tt_each]  then inc(bracketLevel)
            else if t^.tokType=tt_braceClose then dec(bracketLevel)
            else if (t^.tokType=tt_separatorComma) and (bracketLevel=1) then begin              
              bodyPart[length(bodyPart)-1,1]:=p; //end of body part is token before comma
              setLength(bodyPart,length(bodyPart)+1);
              bodyPart[length(bodyPart)-1,0]:=t^.next; //start of next body part is token after comma
            end;
            p:=t; t:=t^.next; 
          end;
          bodyPart[length(bodyPart)-1,1]:=p; //end of body part is token before comma          
          if (t=nil) or (t^.tokType<>tt_braceClose) or (bracketLevel<>1) then begin
            outAdapter.raiseError(el4_parsingError,'Invalid each-construct; Cannot find closing bracket.',first^.location);
            exit;
          end;
          bracketClosingEach:=t;
          for i:=0 to length(bodyPart)-1 do begin
            if bodyPart[i,1]^.next<>bracketClosingEach then disposeToken(bodyPart[i,1]^.next);
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
          end else begin            
            setLength(tokensBetween,1);
            tokensBetween[0]:=newToken(first^.location,'',tt_separatorComma);
            
            first^.tokType:=tt_listBraceOpen;
            bracketClosingEach^.tokType:=tt_listBraceClose;
            aggregatorPresent:=false;
          end;
          //----------------------------------------------process aggregator part (if any)
          //process other body parts (if any)---------------------------------------------
          setLength(bodyRule,length(bodyPart));
          for i:=0 to length(bodyPart)-1 do begin
            new(tempRule,createForEachBody(first^.txt,bodyPart[i,0]));
            bodyRule[i]:=newExpressionLiteral(tempRule);
          end;
          //---------------------------------------------process other body parts (if any)          
          if (P_literal(first^.data)^.literalType in [lt_error,lt_boolean,lt_int,lt_real,lt_string,lt_expression]) 
          then itList:=newOneElementListLiteral(first^.data,false)
          else itList:=first^.data;

          first^.data:=nil;
          first^.txt:='';
          //iterate over itList----------------------------------------------------------
          t:=first;
          if aggregatorPresent and (itList^.size=0) then 
            outAdapter.raiseError(el3_evalError,'Each construct with aggregator is invalid for empty lists to iterate',tokensBetween[0]^.location)
          else if length(bodyRule)>0 then for i:=0 to itList^.size-1 do
          for j:=0 to length(bodyRule)-1 do begin
            if (i<>0) or (j<>0) then for k:=0 to length(tokensBetween)-1 do begin
              t^.next:=newToken(tokensBetween[k]);
              t:=t^.next;
            end;
            t^.next:=newToken('','',tt_literal,bodyRule[j]);
            bodyRule[j]^.rereference;
            t:=t^.next;
            t^.next:=newToken('','',tt_parList,newOneElementListLiteral(itList^.value(i),true));
            t:=t^.next;
          end else for i:=0 to itList^.size-1 do begin
            if (i<>0) then for k:=0 to length(tokensBetween)-1 do begin
              t^.next:=newToken(tokensBetween[k]);
              t:=t^.next;
            end;
            t^.next:=newToken('','',tt_literal,itList^.value(i));
            itList^.value(i)^.rereference;
            t:=t^.next;
          end;
          //----------------------------------------------------------iterate over itList          
          t^.next:=bracketClosingEach;
          //cleanup----------------------------------------------------------------------
          for i:=0 to length(bodyRule)-1 do disposeLiteral(bodyRule[i]);
          for i:=0 to length(tokensBetween)-1 do disposeToken(tokensBetween[i]);
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
          if first^.tokType=tt_userRulePointer then begin
            if not(P_rule(first^.data)^.replaces(parameterListLiteral,firstReplace,lastReplace)) then begin
              if parameterListLiteral=nil then outAdapter.raiseError(el3_evalError,'Cannot apply user defined rule '+P_rule(first^.data)^.id+' to empty parameter list',first^.location)
                                          else outAdapter.raiseError(el3_evalError,'Cannot apply user defined rule '+P_rule(first^.data)^.id+' to parameter list '+parameterListLiteral^.toString,first^.location);
              exit;
            end;
          end else if first^.tokType=tt_intrinsicRulePointer then begin
            newLiteral:=T_intFuncCallback(first^.data)(parameterListLiteral,first^.location);
            if newLiteral<>nil then begin
              firstReplace:=newToken('','',tt_literal,newLiteral);
              lastReplace:=firstReplace;
            end else begin
              if parameterListLiteral=nil then outAdapter.raiseError(el3_evalError,'Cannot apply intrinsic rule '+first^.txt+' to empty parameter list',first^.location)
                                   else outAdapter.raiseError(el3_evalError,'Cannot apply intrinsic rule '+first^.txt+' to parameter list '+parameterListLiteral^.toString,first^.location);
              exit;
            end;
          end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_expression) then begin
            inlineRule:=P_expressionLiteral(first^.data)^.value;
            //failing "replaces" for inline rules will raise evaluation error.
            if not(inlineRule^.replaces(parameterListLiteral,firstReplace,lastReplace)) then exit;
          end else begin
            outAdapter.raiseError(el3_evalError,'Trying to apply a rule which is no rule!',first^.location);
            exit;
          end;
          disposeToken(first);
          if parameterListToken<>nil then disposeToken(parameterListToken);
          first:=firstReplace;
          lastReplace^.next:=firstTokenAfterCall;
          didSubstitution:=true;          
        end;
        
      PROCEDURE resolveInlineIf(CONST conditionLit:boolean);
        VAR p,prev,tokenBeforeThen,lastThen:P_token;
        begin
          p:=first; prev:=nil;
          //while p<>nil and 
          {$WARNING unimplemented}
//          tt_questionMark: if (c[-1].tt=tt_literal) and (P_literal(c[-1].tok^.data)^.typeByte=type_boolScalar) then begin
//            {$ifdef verbose}
//            logSubst('Resolve inline-if');
//            {$endif}
//            stack.pushUnlink(first); //push question mark
//            first:=c[1].tok; iterator:=first;
//            if iterator<>nil then begin
//              //locate subexpressions===============================================
//              while (iterator<>nil) and not((iterator^.ttype=tt_thenMarker) and (stack.top=c[0].tok))  do begin
//                case iterator^.ttype of
//                  tt_braceOpen,tt_listBraceOpen: stack.push(iterator);
//                  tt_braceClose: if stack.top^.ttype=tt_braceOpen
//                                 then stack.pop
//                                 else evaluationEnvironment.raiseError('Unbalanced paranthesis!');
//                  tt_listBraceClose: if stack.top^.ttype=tt_listBraceOpen
//                                 then stack.pop
//                                 else evaluationEnvironment.raiseError('Unbalanced paranthesis!');
//                end;
//                c[2].tok:=iterator;
//                iterator:=iterator^.next;
//              end;
//              //===============================================locate subexpressions
//              if (iterator<>nil) and (iterator^.ttype=tt_thenMarker) then begin
//                iterator:=iterator^.next;
//                c[3].tok:=nil;
//                while (iterator<>nil) and not((iterator^.ttype in [tt_braceClose,tt_listBraceClose,tt_separator]) and (stack.top=c[0].tok)) do begin
//                  case iterator^.ttype of
//                    tt_braceOpen,tt_listBraceOpen: stack.push(iterator);
//                    tt_braceClose: if stack.top^.ttype=tt_braceOpen
//                                   then stack.pop
//                                   else evaluationEnvironment.raiseError('Unbalanced paranthesis!');
//                    tt_listBraceClose: if stack.top^.ttype=tt_listBraceOpen
//                                   then stack.pop
//                                   else evaluationEnvironment.raiseError('Unbalanced paranthesis!');
//                  end;
//                  c[3].tok:=iterator;
//                  iterator:=iterator^.next;
//                end;
//                if (stack.top^.ttype=tt_questionMark) and (c[3].tok<>nil) and ((iterator=nil) or (iterator^.ttype in [tt_braceClose,tt_listBraceClose,tt_separator,tt_questionMark])) then begin
//                  //if iterator<>nil then begin
//                  if P_literal(c[-1].tok^.data)^.bVal[0] then begin
//                    //THEN-case -> drop else-subexpression
//                    c[3].tok^.next:=nil;
//                    c[2].tok^.destroySuccessors();
//                    c[2].tok^.next:=iterator;
//                  end else begin
//                    //ELSE-case -> drop then-subexpression
//                    iterator:=c[2].tok^.next^.next;
//                    c[2].tok^.next^.next:=nil;
//                    first^.destroySuccessors();
//                    recycler.disposeOrPutToHeap(first);
//                    first:=iterator;
//                  end;
//                  stack.popDestroy();
//                  stack.popDestroy();
//                  didSubstitution:=true; reloadForwardChunk; reloadStackChunk;
//                end else begin
//                  evaluationEnvironment.raiseError('Invalid syntax for inline if!');
//                  while stack.top^.ttype<>tt_questionMark do stack.pop;
//                  stack.pop; //pop question mark
//                end;
//              end else begin
//                evaluationEnvironment.raiseError('Invalid syntax for inline if!');
//                while stack.top^.ttype<>tt_questionMark do stack.pop;
//                stack.pop; //pop question mark
//              end;
//              with evaluationEnvironment.debug do if Level=C_debugLevel_functionCallsAndOperators then
//                evaluationEnvironment.addStep(stack.toString+first^.toString(true,literalLengthLimit,expressionLengthLimit));
//            end;
//  
//          end else evaluationEnvironment.raiseError('Invalid syntax for inline-if!');
          
        
        end;
        
      begin
        setLength(stack,0);
        repeat
          didSubstitution:=false;
          case cTokType(0) of
            tt_literal: case cTokType(-1) of
              tt_unaryOpPlus: stack_popDestroy;
              tt_unaryOpMinus: begin
                newLit:=P_literal(first^.data)^.negate(stack[length(stack)-1]^.location);
                disposeToken(first^.data);
                first^.data:=newLit;
                stack_popDestroy;                
              end;
              tt_comparatorEq..tt_comparatorListEq: begin //operators with special cascading
                tt:=cTokType(1);
                if (tt in [tt_comparatorEq..tt_comparatorListEq]) then begin
                  // x < y < z -> [x < y] and y < z
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
                end else if (tt in [tt_comparatorEq..tt_operatorIn]) and (C_opPrecedence[tt]>=C_opPrecedence[cTokType(-1)]) or
                            (tt in [tt_braceClose,tt_listBraceClose,tt_EOL,tt_separatorComma,tt_separatorCnt, tt_iifCheck, tt_iifElse]) then begin
                  newLit:=resolveOperator(stack[length(stack)-2]^.data,
                                          stack[length(stack)-1]^.tokType,
                                          first^.data,
                                          stack[length(stack)-1]^.location);
                  disposeLiteral(first^.data);
                  first^.data:=newLit; //store new literal in head
                  stack_popDestroy; //pop operator from stack
                  stack_popDestroy; //pop LHS-Literal from stack
                end else if tt=tt_parList then applyRule(first^.next,first^.next^.next)
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
                  newLit:=resolveOperator(stack[length(stack)-2]^.data,
                                          stack[length(stack)-1]^.tokType,
                                          first^.data,
                                          stack[length(stack)-1]^.location);
                  disposeLiteral(first^.data);
                  first^.data:=newLit; //store new literal in head
                  stack_popDestroy; //pop operator from stack
                  stack_popDestroy; //pop LHS-Literal from stack
                end else if tt=tt_parList then applyRule(first^.next,first^.next^.next)
                else begin
                  stack_push;
                  stack_push;
                end;              
              end;
              tt_braceOpen: case cTokType(1) of // ( | <Lit>
                tt_braceClose: begin  // ( | <Lit> )
                  stack_popDestroy;
                  first^.next:=disposeToken(first^.next);
                  didSubstitution:=true;
                end;
                tt_comparatorEq..tt_operatorIn: begin // ( | <Lit> op
                  stack_push;
                  stack_push;
                end;
                tt_iifCheck: stack_push;
                tt_parList:  applyRule(first^.next,first^.next^.next);
                //tt_braceOpen: begin
                //  stack_push;
                //  first^.tokType:=tt_parList_constructor;
                //end;
                else outAdapter.raiseError(el3_evalError,'Unable to resolve paranthesis!',stack[length(stack)-1]^.location);
              end;
              tt_list_constructor: case cTokType(1) of
                tt_iifCheck: stack_push;  // [ | <Lit> ?
                tt_separatorComma: begin // [ | <Lit> ,                  
                  P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location);
                  first:=disposeToken(first);
                  first:=disposeToken(first);
                  didSubstitution:=true;
                end;
                tt_separatorCnt: begin // [ | <Lit> ,                  
                  P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location);
                  P_listLiteral(stack[length(stack)-1]^.data)^.setRangeAppend;
                  first:=disposeToken(first);
                  first:=disposeToken(first);
                  didSubstitution:=true;
                end;
                tt_listBraceClose: begin // [ | <Lit> ]
                  P_listLiteral(stack[length(stack)-1]^.data)^.appendConstructing(first^.data,first^.next^.location);
                  first:=disposeToken(first);
                  first:=disposeToken(first);
                  stack_popLink;
                  first^.tokType:=tt_literal;
                  didSubstitution:=true;
                end;
                tt_comparatorEq..tt_operatorIn: begin // [ | <Lit> <op>
                  stack_push;
                  stack_push;
                end;
                tt_parList: applyRule(first^.next,first^.next^.next);
              end;
              tt_parList_constructor: case cTokType(1) of
                tt_iifCheck: stack_push; // ( | <Lit> ? -> ( <Lit> | ?
                tt_braceClose: begin // <F> <par(> | <Lit> ) -> <F> <par>
                  P_listLiteral(stack[length(stack)-1]^.data)^.append(first^.data,true);
                  stack[length(stack)-1]^.tokType:=tt_parList; //mutate <tt_parList_constructor> -> <tt_parList>
                  first:=disposeToken(first); //dispose literal
                  first:=disposeToken(first); //dispose closing bracket
                  stack_popLink; //pop parameter list
                  stack_popLink; //pop function 
                end;
                tt_separatorComma: begin // <F> <par(> | <Lit> , -> <F> <par(> | 
                  P_listLiteral(stack[length(stack)-1]^.data)^.append(first^.data,true);
                  first:=disposeToken(first);
                  first:=disposeToken(first);
                end;
                tt_comparatorEq..tt_operatorIn: begin stack_push; stack_push; end;
              end;
              tt_each: if (stack[length(stack)-1]^.data=nil) then case cTokType(1) of
                tt_comparatorEq..tt_operatorIn: begin stack_push; stack_push; end;
                tt_iifCheck: stack_push;
                tt_separatorComma: if cTokType(2)<>tt_eol then begin
                  stack[length(stack)-1]^.data:=first^.data; //store literal list in each-token
                  P_literal(first^.data)^.reReference; //rereference literal to prevent destruction
                  first:=disposeToken(first);
                  first:=disposeToken(first);                
                  stack_popLink;
                end else outAdapter.raiseError(el3_evalError,'Invalid syntax for each construct - EOL comes to early!',stack[length(stack)-1]^.location);
                else outAdapter.raiseError(el3_evalError,'Invalid syntax for each construct - token "'+first^.next^.toString+'" cannot be interpreted after each!',stack[length(stack)-1]^.location);
              end else outAdapter.raiseError(el3_evalError,'Invalid syntax for each construct - each already has a literal!',stack[length(stack)-1]^.location);
              else case cTokType(1) of
                tt_braceOpen: begin
                  stack_push;
                  first^.tokType:=tt_parList_constructor;
                  first^.data:=newListLiteral;
                  stack_push;
                end;
                tt_parList: applyRule(first^.next,first^.next^.next);
                tt_comparatorEq..tt_operatorIn: begin stack_push; stack_push; end;
                tt_iifCheck: stack_push; 
              end;
            end;
            tt_operatorPlus: begin first^.tokType:=tt_unaryOpPlus; stack_push; end;
            tt_operatorMinus: begin first^.tokType:=tt_unaryOpMinus; stack_push; end;
            tt_comparatorEq..tt_operatorXor,tt_operatorMult..tt_operatorIn: 
              outAdapter.raiseError(el3_evalError,'Undefined prefix operator '+first^.toString,first^.location);
            tt_braceOpen: stack_push;
            tt_listBraceOpen: if cTokType(1)=tt_listBraceClose then begin
              //empty list
              first^.data:=newListLiteral;
              first^.tokType:=tt_literal;
              first^.next:=disposeToken(first^.next);
              didSubstitution:=true;
            end else begin
              first^.data:=newListLiteral;
              first^.tokType:=tt_list_constructor;
              stack_push;
            end;
            tt_identifier: begin              
              P_package(first^.data)^.resolveRuleId(first^);
              if first^.tokType<>tt_identifier then didSubstitution:=true;
            end;
            tt_each: if (first^.data=nil) then stack_push
                                          else resolveEach;
            tt_set: if (cTokType(1)=tt_literal)
                    and (cTokType(2)=tt_braceClose) then begin
              P_rule(first^.data)^.performSet(first^.next^.data);
              first:=disposeToken(first); //dispose set
              first^.next:=disposeToken(first^.next); //dispose closing bracket              
              //result is set literal
              didSubstitution:=true;
            end else begin
              stack_push; //push set
            end;
            tt_userRulePointer, tt_intrinsicRulePointer : case cTokType(1) of
              tt_braceOpen: begin
                stack_push; //push rule pointer
                first^.tokType:=tt_parList_constructor; //mutate ( -> tt_parList_constructor
                first^.data:=newListLiteral;
                stack_push; //push parameter list constructor
              end;
              tt_braceClose,tt_listBraceClose,tt_comparatorEq..tt_operatorIn,tt_EOL,tt_iifCheck,tt_iifElse,tt_separatorCnt,tt_separatorComma: applyRule(nil,first^.next);
              tt_parList: applyRule(first^.next,first^.next^.next);
            end;
            tt_iifCheck: if (cTokType(-1)=tt_literal) and (P_literal(stack[length(stack)-1]^.data)^.literalType=lt_boolean) then begin
              resolveInlineIf(P_boolLiteral(stack[length(stack)-1]^.data)^.value);
            end else outAdapter.raiseError(el3_evalError,'Invalid syntax for inline-if; first operand is expected to be a boolean.',first^.location);
          end;
        until not(didSubstitution) or (outAdapter.errorLevel>=el3_evalError);
        while (length(stack)>0) do stack_popLink;
      end;
      
    VAR assignmentToken:P_token;

    PROCEDURE parseRule;      
      VAR p,n,nn,nnn:P_token;
          ruleIsPrivate:boolean=false;
          ruleId:string;
          evaluateBody:boolean;
          rulePattern:T_pattern;
          ruleBody:P_token;
          ruleDeclarationStart:ansistring;
          subRule:P_subrule;
      begin        
        ruleDeclarationStart:=first^.location;
        evaluateBody:=(assignmentToken^.tokType=tt_assign);
        ruleBody:=assignmentToken^.next;
        //plausis:
        if (ruleBody=nil) then begin 
          outAdapter.raiseError(el4_parsingError,'Missing function body after assignment/declaration token.',assignmentToken^.location); 
          cascadeDisposeToken(first);
          exit; 
        end;        
        p:=ruleBody^.getDeclarationOrAssignmentToken;
        if (p<>nil) then begin
          outAdapter.raiseError(el4_parsingError,'Function body contains unplausible assignment/declaration token.',p^.location);  
          cascadeDisposeToken(first);
          exit;
        end;
        if not(first^.tokType in [tt_identifier, tt_userRulePointer, tt_intrinsicRulePointer]) then begin
          outAdapter.raiseError(el4_parsingError,'Declaration does not start with an identifier.',first^.location);  
          cascadeDisposeToken(first);
          exit;
        end;        
        //:plausis
        if (first^.next^.tokType=tt_identifier) and (first^.tokType=tt_identifier) then begin
          if trim(first^.txt)='private' then ruleIsPrivate:=true
          else if trim(first^.txt)<>'public' then begin
            outAdapter.raiseError(el4_parsingError,'Invalid declaration head.',first^.location);  
            cascadeDisposeToken(first);
            exit;
          end;
          p:=first; first:=disposeToken(p);
        end;
        ruleId:=trim(first^.txt);
        p:=first;
        first:=disposeToken(p);
        if not(first^.tokType in [tt_braceOpen,tt_assign,tt_declare])  then begin
          outAdapter.raiseError(el4_parsingError,'Invalid declaration head.',first^.location);  
          cascadeDisposeToken(first);
          exit;
        end;
        rulePattern.create;
        if first^.tokType=tt_braceOpen then begin
          first:=disposeToken(first);
          while not((first=nil) or (first^.tokType in [tt_assign,tt_declare])) do begin
            n  :=first^.next;
            nn :=n    ^.next;
            nnn:=nn   ^.next;
            if (first^.tokType=tt_identifier) 
            and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendFreeId(first^.txt);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType in [lt_boolean, lt_int, lt_real, lt_string])
                     and (n^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison('',tt_comparatorEq,P_scalarLiteral(first^.data));
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_literal) and (P_literal(first^.data)^.literalType=lt_list) and (P_listLiteral(first^.data)^.size=0) then begin
              rulePattern.appendTypeCheck('',tt_typeCheckEmptyList);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_identifier) 
                     and (n^.tokType in [tt_typeCheckScalar, tt_typeCheckList, tt_typeCheckBoolean, tt_typeCheckBoolList, tt_typeCheckInt, tt_typeCheckIntList, tt_typeCheckReal,tt_typeCheckRealList, tt_typeCheckString,tt_typeCheckStringList, tt_typeCheckNumeric, tt_typeCheckNumList, tt_typeCheckExpression, tt_typeCheckNonemptyList, tt_typeCheckEmptyList])
                     and (nn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendTypeCheck(first^.txt,n^.tokType);
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_identifier) 
                     and (n^.tokType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
                     and (nn^.tokType=tt_literal) and (P_literal(nn^.data)^.literalType in [lt_boolean, lt_int, lt_real, lt_string])
                     and (nnn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison(first^.txt,n^.tokType,P_scalarLiteral(nn^.data));
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else if (first^.tokType=tt_identifier) 
                     and (n^.tokType in [tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq])
                     and (nn^.tokType=tt_identifier) 
                     and (nnn^.tokType in [tt_separatorComma,tt_braceClose]) then begin
              rulePattern.appendComparison(first^.txt,n^.tokType,nn^.txt);
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
              first:=disposeToken(first);
            end else begin
              outAdapter.raiseError(el4_parsingError,'Invalid declaration pattern element.',first^.location);
              cascadeDisposeToken(first);
              exit;
            end;
          end;
          if first<>nil then begin
            first:=disposeToken(first);
          end else begin
            outAdapter.raiseError(el4_parsingError,'Invalid declaration.',ruleDeclarationStart);
            exit;
          end;
        end;
        if evaluateBody then reduceExpression(ruleBody);
        if outAdapter.errorLevel<el3_evalError then begin
          new(subrule,create(rulePattern,ruleBody,ruleDeclarationStart,@self));
          ensureLocalRuleId(ruleId)^.addOrReplaceSubRule(subrule);
          if not(ruleIsPrivate) then 
          ensurePublicRuleId(ruleId)^.addOrSetSubRule(subrule);
          first:=nil;
        end else begin
          cascadeDisposeToken(first);
        end;
      end;
      
    begin
      if first=nil then exit;
      if isFirstLine then begin      
        isFirstLine:=false;
        if (          first^.tokType=tt_identifier) and 
           (uppercase(first^.txt)='USE') and 
           (          first^.next<>nil) and 
           (          first^.next^.tokType=tt_identifier) 
        then begin
          interpretUseClause;
          exit;
        end;
      end;
      predigestBeforeDeclarationParsing(first);
      assignmentToken:=first^.getDeclarationOrAssignmentToken;
      if assignmentToken=nil then predigest(first,true)
                             else predigest(assignmentToken,false);
      if assignmentToken<>nil then begin
        outAdapter.writeDeclEcho(tokensToString(first));
        parseRule;
      end else begin
        outAdapter.writeExprEcho(tokensToString(first));
        reduceExpression(first);
        outAdapter.writeExprOut(tokensToString(first));
      end;
      cascadeDisposeToken(first);
    end;

  VAR codeLines:T_stringList;
      i,lineColCounter:longint;
      first,next,last:P_token;
  begin   
    clear;    
    codeLines:=codeProvider^.fileLines;
    codeProvider^.logCheck;
    
    first:=nil;
    next :=nil;     
    last :=nil;
    for i:=0 to length(codeLines)-1 do begin
      lineColCounter:=1;
      while codeLines[i]<>'' do begin
        next:=firstToken(codeLines[i],lineColCounter,codeProvider^.fileName+':'+intToStr(i+1)+','+intToStr(lineColCounter));
          
        if (next<>nil) and (next^.tokType=tt_eol) then begin //EOL (end of input or semicolon)
          disposeToken(next);
          next:=nil;
          last:=nil;
        end;
        
        if next=nil then begin
          if first<>nil then interpret(first);
          last:=nil;
          first:=nil;        
        end else begin
          if first=nil then begin
            first:=next;
            last :=next;
          end else begin
            last^.next:=next;
            last      :=next;
          end;
        end;
      end;
    end;
    if first<>nil then interpret(first);
  end;
  
CONSTRUCTOR T_package.create(CONST provider:P_codeProvider);
  begin
    setLength(packageUses,0);
    codeProvider:=provider;      
    publicRules.create;
    localRules.create;
    load;
  end;
  
PROCEDURE T_package.clear;
  VAR keySet:T_arrayOfString;
      i:longint;
      rule:P_rule;
  begin
    keySet:=publicRules.keySet;
    for i:=0 to length(keySet)-1 do begin
      rule:=publicRules.get(keySet[i]);
      setLength(rule^.subRules,0);
      dispose(rule,destroy);
    end;
    publicRules.clear;
    
    keySet:=localRules.keySet;
    for i:=0 to length(keySet)-1 do begin
      rule:=localRules.get(keySet[i]);
      dispose(rule,destroy);
    end;
    localRules.clear;
    
    
    ready:=false;
  end;
  
DESTRUCTOR T_package.destroy;
  begin
    clear;  
    publicRules.destroy;
    localRules.destroy;
    setLength(packageUses,0);    
  end;
  
PROCEDURE T_package.resolveRuleId(VAR token:T_token);
  VAR i:longint;
      userRule:P_rule;
      intrinsicFuncPtr:T_intFuncCallback;
  begin
    if localRules.containsKey(token.txt,userRule) then begin
      token.tokType:=tt_userRulePointer;
      token.data:=userRule;
      exit;
    end;
    for i:=length(packageUses)-1 downto 0 do 
    if packageUses[i].pack^.publicRules.containsKey(token.txt,userRule) then begin
      token.tokType:=tt_userRulePointer;
      token.data:=userRule;
      exit;
    end;
    if intrinsicRuleAliases.containsKey(token.txt,intrinsicFuncPtr) then begin
      token.tokType:=tt_intrinsicRulePointer;
      token.data:=intrinsicFuncPtr;
      exit;
    end;
    outAdapter.raiseError(el4_parsingError,'Cannot resolve ID "'+token.txt+'"',token.location);
  end;
  
FUNCTION T_package.ensureLocalRuleId(CONST ruleId:ansistring):P_rule;
  begin
    if not(localRules.containsKey(ruleId,result)) then begin
      new(result,create(ruleId));
      localRules.put(ruleId,result);
    end;
  end;
  
FUNCTION T_package.ensurePublicRuleId(CONST ruleId:ansistring):P_rule;
  begin
    if not(publicRules.containsKey(ruleId,result)) then begin
      new(result,create(ruleId));
      publicRules.put(ruleId,result);
    end;
  end;
  
INITIALIZATION
  setLength(packages,0);
  tokenRecycling.fill:=0;
  disposeSubruleCallback :=@disposeSubruleImpl;
  subruleToStringCallback:=@subruleToStringImpl;
  subruleApplyOpCallback :=@subruleApplyOpImpl;
FINALIZATION
  clearAllPackages;
  finalizeTokens;
end.