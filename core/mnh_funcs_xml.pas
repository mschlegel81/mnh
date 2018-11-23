UNIT mnh_funcs_xml;
INTERFACE
USES sysutils,XMLRead,dom,Classes,LazUTF8,
     myGenerics,
     mnh_basicTypes,mnh_constants,
     mnh_litVar,
     mnh_funcs,
     mnh_messages,
     recyclers,
     mnh_contexts;

IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION obtainXmlData(VAR FDoc: TXMLDocument):P_literal;
  VAR resultList:P_listLiteral;
  PROCEDURE readAttributes(CONST owner:P_listLiteral; CONST node:TDOMNode);
    VAR j:longint;
    begin
      if not Assigned(node) then exit;
      if Assigned(node.attributes) then begin
        for j:=0 to node.attributes.length-1 do
          owner^.append(newListLiteral(2)^
                       .appendString(UTF16ToUTF8(node.attributes[j].NodeName))^
                       .appendString(UTF16ToUTF8(node.attributes[j].NodeValue)),false);
      end;
    end;

  PROCEDURE readChildren(CONST owner:P_listLiteral; CONST node:TDOMNode);
    VAR i: integer;
        subTree   :P_listLiteral;
    begin
      if not Assigned(node) then exit;
      for i:=0 to node.ChildNodes.count-1 do begin
        subTree:=newListLiteral(3);
        subTree^.appendString(UTF16ToUTF8(node.ChildNodes[i].NodeName))
               ^.append(newListLiteral(),false)
               ^.append(newListLiteral(),false);
        readAttributes(P_listLiteral(subTree^.value[1]),node.ChildNodes[i]);
        readChildren  (P_listLiteral(subTree^.value[2]),node.ChildNodes[i]);
        owner^.append(subTree,false);
      end;
    end;

  begin
    if Assigned(FDoc) then begin
      resultList:=newListLiteral;
      readChildren(resultList,FDoc);
      FreeAndNil(FDoc);
      exit(resultList);
    end else result:=nil;
  end;

FUNCTION readXmlFile_impl intFuncSignature;
  VAR FDoc: TXMLDocument;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context.checkSideEffects('readXmlFile',tokenLocation,[se_readFile]) then begin
      if fileExists(str0^.value)
      then try
        ReadXMLFile(FDoc, str0^.value)
      except
        on e:Exception do begin
          context.raiseError('Error parsing XML file '+str0^.value+': '+e.message,tokenLocation);
        end;
      end else begin
        context.messages^.postTextMessage(mt_el2_warning,tokenLocation,'XML File '+str0^.value+' does not exist');
        exit(newVoidLiteral);
      end;
      result:=obtainXmlData(FDoc);
      if result=nil then begin
        context.messages^.postTextMessage(mt_el2_warning,tokenLocation,'Error parsing XML file '+str0^.value);
        result:=newVoidLiteral;
      end;
    end;
  end;

FUNCTION readXml_impl intFuncSignature;
  VAR FDoc: TXMLDocument;
      input: TStringStream;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      input:=TStringStream.create(str0^.value);
      input.position:=0;
      try
        ReadXMLFile(FDoc,input);
      except
        on e:Exception do begin
          context.raiseError('Error parsing XML input: '+e.message,tokenLocation);
        end;
      end;
      FreeAndNil(input);
      result:=obtainXmlData(FDoc);
      if result=nil then begin
        context.raiseError('Error parsing XML input.',tokenLocation);
        result:=newVoidLiteral;
      end;
    end;
  end;

INITIALIZATION
  registerRule(FILES_BUILTIN_NAMESPACE,'readXmlFile',@readXmlFile_impl,ak_unary,'readXmlFile(filename:string);//Reads contents of an XML file and returns the contents as a list');
  registerRule(FILES_BUILTIN_NAMESPACE,'readXml'    ,@readXml_impl    ,ak_unary,'readXml(input:string);//Parses input as XML and returns the contents as a list');

end.
