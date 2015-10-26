UNIT mnh_funcs_mnh;
INTERFACE
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,sysutils,myGenerics,mnh_out_adapters,myStringUtil;
IMPLEMENTATION
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

FUNCTION myPath_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      if (tokenLocation.fileName='?') or
         (tokenLocation.fileName='') then result:=newStringLiteral('<Unknown>')
                                     else result:=newStringLiteral(tokenLocation.fileName);
    end else raiseNotApplicableError('myPath',params,tokenLocation);
  end;

FUNCTION executor_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then result:=newStringLiteral(paramStr(0))
    else raiseNotApplicableError('executor',params,tokenLocation);
  end;

FUNCTION splitFileName_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  PROCEDURE appendPair(VAR result:P_literal; CONST el0:string; CONST el1:string);
    begin
      P_listLiteral(result)^.append(
        newListLiteral^.
        appendString(el0)^.
        appendString(el1),false);
    end;
  VAR name:string;
      i:longint;
      tmpParam:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newListLiteral;
      name:=P_stringLiteral(params^.value(0))^.value;
      appendPair(result,'input',name);
      appendPair(result,'expanded',replaceAll(expandFileName(name),'\','/'));
      appendPair(result,'relative',replaceAll(extractRelativePath(expandFileName(''),name),'\','/'));
      if ExtractFileDir(name)=''
      then appendPair(result,'directory','.')
      else appendPair(result,'directory',replaceAll(ExtractFileDir(name),'\','/'));
      appendPair(result,'filename',replaceAll(extractFileName(name),'\','/'));
      appendPair(result,'extension',replaceAll(extractFileExt(name),'\','/'));
    end else if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_stringList,lt_emptyList]) then begin
      result:=newListLiteral;
      for i:=0 to P_listLiteral(params^.value(0))^.size-1 do begin
        tmpParam:=newOneElementListLiteral(P_listLiteral(params^.value(0))^.value(i),true);
        P_listLiteral(result)^.append(splitFileName_imp(tmpParam,tokenLocation),false);
        disposeLiteral(tmpParam);
      end;
    end else raiseNotApplicableError('splitFileName',params,tokenLocation);
  end;

FUNCTION hash_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=newIntLiteral(params^.value(0)^.hash)
    else raiseNotApplicableError('hash',params,tokenLocation);
  end;

FUNCTION listBuiltin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  VAR keys:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      keys:=intrinsicRuleExplanationMap.keySet;
      result:=newListLiteral;
      for i:=0 to length(keys)-1 do P_listLiteral(result)^.appendString(keys[i]);
      setLength(keys,0);
    end else raiseNotApplicableError('listBuiltin',params,tokenLocation);
  end;

FUNCTION fail_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    if (params=nil) or (params^.size=0) then raiseError(el3_evalError,'Fail.',tokenLocation)
    else if (params<>nil) and (params^.size=1) then raiseError(el3_evalError,params^.value(0)^.toString,tokenLocation)
    else raiseNotApplicableError('fail',params,tokenLocation);
    result:=nil;
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
          for i:=0 to P_listLiteral(x)^.size-1 do if noErrors then
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

INITIALIZATION
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'softCast',@softCast_imp,'softCast(X);#Returns a simplified version of X, trying to parse integers, real values and booleans');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'string',@string_imp,'string(X);#Returns a string-representation of X');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath',@myPath_impl,'myPath;#returns the path to the current package');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor',@executor_impl,'executor;#returns the path to the currently executing instance of MNH');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'splitFileName',@splitFileName_imp,'splitFilename(name:string);#Returns various representations and parts of the given name');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash',@hash_imp,'hash(x);#Returns the builtin hash for the given literal');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listBuiltin',@listBuiltin_imp,'listBuiltin;#Returns a list of all built-in functions (qualified and non-qualified)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'fail',@fail_impl,'fail;#Raises an exception without a message#fail(message);#Raises an exception with the given message');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord',@ord_imp,'ord(x);#Returns the ordinal value of x');
end.
