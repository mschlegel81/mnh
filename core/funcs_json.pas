UNIT funcs_json;
INTERFACE
USES JSONParser,fpjson,jsonscanner,
     myGenerics,
     basicTypes,mnh_constants,
     litVar,
     funcs,
     mnh_messages,
     recyclers,
     contexts,
     myStringUtil;

IMPLEMENTATION
USES sysutils;
{$i func_defines.inc}

FUNCTION parseJson_impl intFuncSignature;
  VAR insteadOfNull:P_literal=nil;
  FUNCTION jsonToLiteral(CONST json:TJSONData):P_literal;
    VAR childLit :P_literal;
        i:longint;
        key:string;
    begin
      result:=nil;
      case json.JSONType of
        jtString  : result:=recycler^.newStringLiteral(json.AsString);
        jtBoolean : result:=newBoolLiteral(json.AsBoolean);
        jtNumber  : if json.ClassType=TJSONFloatNumber.ClassType
                    then result:=recycler^.newRealLiteral(json.AsFloat)
                    else result:=recycler^.newIntLiteral(json.AsInt64);
        jtArray   : begin
                      result:=recycler^.newListLiteral(TJSONArray(json).count);
                      for i:=0 to TJSONArray(json).count-1 do begin
                        childLit:=jsonToLiteral(TJSONArray(json)[i]);
                        if childLit<>nil then P_listLiteral(result)^.append(recycler,childLit,false);
                      end;
                    end;
        jtObject  : begin
                      result:=recycler^.newMapLiteral(TJSONObject(json).count);
                      for i:=0 to TJSONObject(json).count-1 do begin
                        key     :=TJSONObject(json).names[i];
                        childLit:=jsonToLiteral(TJSONObject(json).elements[key]);
                        if childLit<>nil then P_mapLiteral(result)^.put(recycler,key,childLit,false);
                      end;
                    end;
        jtNull    : if insteadOfNull<>nil then result:=insteadOfNull^.rereferenced;
        jtUnknown : context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'Unparseable JSON part: '+json.toString);
      end;
    end;

  VAR JSONParser:TJSONParser;
      jsonData: TJSONData;
  begin
    if (params<>nil) and (params^.size>=1) and (params^.size<=2) and (arg0^.literalType=lt_string) then begin
      if params^.size>1 then insteadOfNull:=arg1;
      JSONParser:=TJSONParser.create(str0^.value,[joUTF8,joIgnoreTrailingComma]);
      try
        jsonData:=JSONParser.parse;
      except
        on e:Exception do begin
          context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,e.message);
          JSONParser.free;
          exit(newVoidLiteral);
        end;
      end;
      if (jsonData=nil) then begin
        result:=newVoidLiteral;
      end else begin
        result:=jsonToLiteral(jsonData);
      end;
      jsonData.free;
      JSONParser.free;
    end else if (params=nil) or (params^.size=0)
    then result:=newVoidLiteral
    else result:=nil;
  end;

FUNCTION formatJson_impl intFuncSignature;
  FUNCTION literalToJson(CONST literal:P_literal):string;
    VAR nonescapableFound: boolean;
        parts:T_arrayOfString;
        i:longint;
        entryList: T_arrayOfKeyValuePair;
        iter: T_arrayOfLiteral;
    begin
      case literal^.literalType of
        lt_expression,
        lt_boolean, lt_smallint, lt_bigint, lt_real: result:=literal^.toString();
        lt_string: result:=escapeString(P_stringLiteral(literal)^.value,es_javaStyle,P_stringLiteral(literal)^.getEncoding,nonescapableFound);
        lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_emptyList,
        lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet,  lt_emptySet: begin
          iter:=P_collectionLiteral(literal)^.tempIterableList;
          setLength(parts,length(iter));
          for i:=0 to length(iter)-1 do parts[i]:=literalToJson(iter[i]);
          result:='['+join(parts,',')+']';
        end;
        lt_map,lt_emptyMap: begin
          entryList:=P_mapLiteral(literal)^.entryList;
          setLength(parts,length(entryList));
          for i:=0 to length(entryList)-1 do begin
            case entryList[i].key^.literalType of
              lt_string: parts[i]:=literalToJson(entryList[i].key)
              else       parts[i]:=escapeString(literalToJson(entryList[i].key),es_javaStyle,P_stringLiteral(literal)^.getEncoding,nonescapableFound);
            end;
            parts[i]+=':'+literalToJson(entryList[i].value);
            entryList[i].key^.unreference;
            entryList[i].value^.unreference;
          end;
          result:='{'+join(parts,',')+'}';
        end
        else result:='';
      end;
    end;

  begin
    if (params<>nil) and (params^.size=1)
    then result:=recycler^.newStringLiteral(literalToJson(arg0),'',true)
    else result:=nil;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'parseJson',@parseJson_impl,ak_variadic_1);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'formatJson',@formatJson_impl,ak_unary);

end.
