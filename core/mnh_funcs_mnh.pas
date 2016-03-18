UNIT mnh_funcs_mnh;
INTERFACE
USES mnh_tokLoc,mnh_litVar,mnh_constants, mnh_funcs,sysutils,myGenerics,mnh_out_adapters,myStringUtil,mnh_html,mnh_contexts;
IMPLEMENTATION
{$MACRO ON}
{$define str0:=P_stringLiteral(params^.value(0))}
{$define str1:=P_stringLiteral(params^.value(1))}
{$define list0:=P_listLiteral(params^.value(0))}
{$define list1:=P_listLiteral(params^.value(1))}
{$define arg0:=params^.value(0)}
{$define arg1:=params^.value(1)}

FUNCTION softCast_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  FUNCTION softCastRecurse(CONST x:P_literal):P_literal;
    VAR i:longint;
    begin
      case x^.literalType of
        lt_string: result:=P_stringLiteral(x)^.softCast;
        lt_list..lt_listWithError: begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do
            P_listLiteral(result)^.append(softCastRecurse(P_listLiteral(x)^.value(i)),false,context.adapters^);
        end;
        else begin
          x^.rereference;
          result:=x;
        end;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then result:=softCastRecurse(arg0);
  end;

FUNCTION string_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) then begin
      if arg0^.literalType=lt_string then begin
        result:=arg0;
        result^.rereference;
      end else result:=newStringLiteral(arg0^.toString);
    end;
  end;

FUNCTION myPath_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      if (tokenLocation.fileName='?') or
         (tokenLocation.fileName='') then result:=newStringLiteral('<Unknown>')
                                     else result:=newStringLiteral(tokenLocation.fileName);
    end;
  end;

FUNCTION executor_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0)
    then result:=newStringLiteral(paramStr(0));
  end;

FUNCTION splitFileName_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  PROCEDURE appendPair(VAR result:P_literal; CONST el0,el1:string);
    begin
      P_listLiteral(result)^.append(
        newListLiteral^.
        appendString(el0)^.
        appendString(el1),false,context.adapters^);
    end;
  VAR name:string;
      i:longint;
      tmpParam:P_listLiteral;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      result:=newListLiteral;
      name:=str0^.value;
      appendPair(result,'input',name);
      appendPair(result,'expanded',replaceAll(expandFileName(name),'\','/'));
      appendPair(result,'relative',replaceAll(extractRelativePath(expandFileName(''),name),'\','/'));
      if ExtractFileDir(name)=''
      then appendPair(result,'directory','.')
      else appendPair(result,'directory',replaceAll(ExtractFileDir(name),'\','/'));
      appendPair(result,'filename',replaceAll(extractFileName(name),'\','/'));
      appendPair(result,'extension',replaceAll(extractFileExt(name),'\','/'));
      appendPair(result,'drive',ExtractFileDrive(expandFileName(name)));
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_stringList,lt_emptyList]) then begin
      result:=newListLiteral;
      for i:=0 to list0^.size-1 do begin
        tmpParam:=newOneElementListLiteral(list0^.value(i),true,context.adapters^);
        P_listLiteral(result)^.append(splitFileName_imp(tmpParam,tokenLocation,context),false,context.adapters^);
        disposeLiteral(tmpParam);
      end;
    end;
  end;

FUNCTION relativeFilename_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) then case arg0^.literalType of
      lt_string: case arg1^.literalType of
        lt_string: exit(newStringLiteral(
            replaceAll(
            extractRelativePath(str0^.value+'/',
                                str1^.value),
            '\','/')));
        lt_stringList,lt_emptyList: begin
          result:=newListLiteral;
          for i:=0 to list1^.size-1 do
            P_listLiteral(result)^.appendString(
            replaceAll(
            extractRelativePath(str0^.value+'/',
                                P_stringLiteral(list1^.value(i))^.value),
            '\','/'));
        end;
      end;
      lt_stringList,lt_emptyList: case arg1^.literalType of
        lt_string: begin
          result:=newListLiteral;
          for i:=0 to list1^.size-1 do
            P_listLiteral(result)^.appendString(
            replaceAll(
            extractRelativePath(P_stringLiteral(list0^.value(i))^.value+'/',
                                str1^.value),
            '\','/'));
        end;
        lt_stringList,lt_emptyList: if  list0^.size= list1^.size then begin
          result:=newListLiteral;
          for i:=0 to list1^.size-1 do
            P_listLiteral(result)^.appendString(
            replaceAll(
            extractRelativePath(P_stringLiteral(list0^.value(i))^.value+'/',
                                P_stringLiteral(list1^.value(i))^.value),
            '\','/'));
        end;
      end;
    end;
  end;

FUNCTION hash_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=newIntLiteral(arg0^.hash);
  end;

FUNCTION listBuiltin_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR keys:T_arrayOfString;
      i:longint;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      keys:=intrinsicRuleMap.keySet;
      result:=newListLiteral;
      for i:=0 to length(keys)-1 do P_listLiteral(result)^.appendString(keys[i]);
      setLength(keys,0);
    end;
  end;

FUNCTION fail_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params=nil) or (params^.size=0) then context.adapters^.raiseError('Fail.',tokenLocation)
    else if (params<>nil) and (params^.size=1) then context.adapters^.raiseError(arg0^.toString,tokenLocation);
    result:=nil;
  end;

FUNCTION ord_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
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
        lt_error,lt_void, lt_real,lt_expression: exit(newErrorLiteralRaising('ord can only be applied to booleans, ints and strings',tokenLocation,context.adapters^));
        else begin
          result:=newListLiteral;
          for i:=0 to P_listLiteral(x)^.size-1 do if context.adapters^.noErrors then
            P_listLiteral(result)^.append(recurse(P_listLiteral(x)^.value(i)),false,context.adapters^);
        end;
      end;
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then result:=recurse(arg0);
  end;

FUNCTION addPrintAdapter_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR printAdapter:P_textFilePrintOnlyAdapter;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      new(printAdapter,create(str0^.value));
      context.adapters^.addOutAdapter(printAdapter,true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION addFullAdapter_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      addOutfile(context.adapters^,str0^.value,true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION removeAdapter_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      context.adapters^.removeOutAdapter(str0^.value);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

INITIALIZATION
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'softCast',@softCast_imp,'softCast(X);#Returns a simplified version of X, trying to parse integers, real values and booleans');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'string',@string_imp,'string(X);#Returns a string-representation of X');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'myPath',@myPath_impl,'myPath;#returns the path to the current package');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'executor',@executor_impl,'executor;#returns the path to the currently executing instance of MNH');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'splitFileName',@splitFileName_imp,'splitFilename(name:string);#Returns various representations and parts of the given name');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'relativeFilename',@relativeFilename_impl,'relativeFilename(reference,file);#Returns the path of file relative to reference');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'hash',@hash_imp,'hash(x);#Returns the builtin hash for the given literal');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'listBuiltin',@listBuiltin_imp,'listBuiltin;#Returns a list of all built-in functions (qualified and non-qualified)');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'fail',@fail_impl,'fail;#Raises an exception without a message#fail(message);#Raises an exception with the given message');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'ord',@ord_imp,'ord(x);#Returns the ordinal value of x');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'addPrintAdapter',@addPrintAdapter_imp,'addPrintAdapter(filename:string);#Adds a file recieving the print-output in addition to console outputs etc.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'addFullAdapter',@addFullAdapter_imp,'addFullAdapter(filename:string);#Adds a file receiving all outputs going to the adapter.');
  registerRule(DEFAULT_BUILTIN_NAMESPACE,'removeAdapter',@removeAdapter_imp,'removeAdapter(filename:string);#Removes an adapter if present.');

end.
