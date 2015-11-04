UNIT mnh_funcs;
INTERFACE
USES sysutils,myGenerics,mnh_constants,mnh_litVar,mnh_out_adapters,mnh_tokLoc,
     myStringUtil,Classes,mySys;
TYPE
  T_intFuncCallback=FUNCTION(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;

VAR
  intrinsicRuleMap           :specialize G_stringKeyMap<T_intFuncCallback>;
  intrinsicRuleExplanationMap:specialize G_stringKeyMap<ansistring>;

PROCEDURE registerRule(CONST namespace:T_namespace; CONST name:ansistring; CONST ptr:T_intFuncCallback; CONST explanation:ansistring; CONST fullNameOnly:boolean=false);
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST typ:T_literalType; CONST messageTail:ansistring; CONST tokenLocation:T_tokenLocation);

IMPLEMENTATION
VAR print_cs:system.TRTLCriticalSection;

PROCEDURE registerRule(CONST namespace:T_namespace; CONST name:ansistring; CONST ptr:T_intFuncCallback; CONST explanation:ansistring; CONST fullNameOnly:boolean=false);
  PROCEDURE registerImp(CONST regName:ansistring);
    VAR oldExplanation:ansistring;
    begin
      intrinsicRuleMap.put(regName,ptr);
      if (explanation<>'') or not(intrinsicRuleExplanationMap.containsKey(regName,oldExplanation))
                             then intrinsicRuleExplanationMap.put        (regName,explanation);
    end;

  begin
    if not(fullNameOnly) then registerImp(name);
    registerImp(C_namespaceString[namespace]+C_ID_QUALIFY_CHARACTER+name);
  end;

PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST typ:T_literalType; CONST messageTail:ansistring; CONST tokenLocation:T_tokenLocation);
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function '+functionName+' cannot be applied to type '+C_typeString[typ]+messageTail;
    raiseError(complaintText,tokenLocation);
  end;

FUNCTION format_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  {$MACRO ON}
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
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string) then begin
      decomposeFormatString(P_stringLiteral(params^.value(0))^.value);
      result:=newStringLiteral(join(formatTabs(reSplit(resultString)),C_lineBreakChar));
    end;
  end;

FUNCTION printf_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  INNER_FORMATTING;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (params^.value(0)^.literalType=lt_string) then begin
      decomposeFormatString(P_stringLiteral(params^.value(0))^.value);
      system.enterCriticalSection(print_cs);
      printOut(formatTabs(reSplit(resultString)));
      system.leaveCriticalSection(print_cs);
      result:=newVoidLiteral;
    end;
  end;
{$undef INNER_FORMATTING}

FUNCTION clearPrint_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    system.enterCriticalSection(print_cs);
    clearPrint();
    system.leaveCriticalSection(print_cs);
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
    system.enterCriticalSection(print_cs);
    printOut(formatTabs(split(stringToPrint)));
    system.leaveCriticalSection(print_cs);
    result:=newVoidLiteral;
  end;


INITIALIZATION
  intrinsicRuleMap.create;
  intrinsicRuleExplanationMap.create;
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'printf',@printf_imp,'fprint(formatString:string,...);#Prints a formatted version of the given 0..n parameters and returns void');
  registerRule(STRINGS_NAMESPACE,'format',@format_imp,'format(formatString:string,...);#Returns a formatted version of the given 0..n parameters');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'clearPrint',@clearPrint_imp,'clearPrint(...);#Clears the output and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'print',@print_imp,'print(...);#Prints out the given parameters and returns void#if tabs and line breaks are part of the output, a default pretty-printing is used');
  system.initCriticalSection(print_cs);
FINALIZATION
  intrinsicRuleMap.destroy;
  intrinsicRuleExplanationMap.destroy;
  system.doneCriticalSection(print_cs);
end.
