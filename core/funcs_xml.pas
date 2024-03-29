UNIT funcs_xml;
INTERFACE
USES sysutils,XMLRead,dom,Classes,LazUTF8,
     myGenerics,
     basicTypes,mnh_constants,
     litVar,
     funcs,
     mnh_messages,
     recyclers,
     contexts;

IMPLEMENTATION
{$i func_defines.inc}

FUNCTION obtainXmlData(CONST literalRecycler:P_literalRecycler; VAR FDoc: TXMLDocument):P_listLiteral;
  FUNCTION readNode(CONST node:TDOMNode):P_literal;
    VAR j:longint;
        attributesMap:P_mapLiteral;
        childList:P_listLiteral;

    begin
      if (node.NodeType=TEXT_NODE) or
         (node.NodeType=CDATA_SECTION_NODE) then exit(literalRecycler^.newStringLiteral(UTF16ToUTF8(node.NodeValue)));
      result:=literalRecycler^.newMapLiteral(3);
      P_mapLiteral(result)^.put(literalRecycler,'name',UTF16ToUTF8(node.NodeName));
      if node.NodeType<>ELEMENT_NODE then P_mapLiteral(result)^.put(literalRecycler,'type',node.NodeType);
      if node.NodeType<>ELEMENT_NODE then P_mapLiteral(result)^.put(literalRecycler,'value',UTF16ToUTF8(node.NodeValue));
      if Assigned(node.attributes) and (node.attributes.length>0) then begin
        attributesMap:=literalRecycler^.newMapLiteral(node.attributes.length);
        for j:=0 to node.attributes.length-1 do
         attributesMap^.put(literalRecycler,
                            UTF16ToUTF8(node.attributes[j].NodeName),
                            UTF16ToUTF8(node.attributes[j].NodeValue));
        P_mapLiteral(result)^.put(literalRecycler,'attributes',attributesMap,false);
      end;
      if Assigned(node.ChildNodes) and (node.ChildNodes.length>0) then begin
        childList:=literalRecycler^.newListLiteral(node.ChildNodes.count);
        for j:=0 to node.ChildNodes.count-1 do childList^.append(literalRecycler,readNode(node.ChildNodes[j]),false);
        P_mapLiteral(result)^.put(literalRecycler,'children',childList,false);
      end;
    end;

  VAR i:longint;
  begin
    if Assigned(FDoc) then begin
      result:=literalRecycler^.newListLiteral();
      for i:=0 to FDoc.ChildNodes.count-1 do result^.append(literalRecycler,readNode(FDoc.ChildNodes[i]),false);
      FreeAndNil(FDoc);
    end else result:=nil;
  end;

FUNCTION readXmlFile_impl intFuncSignature;
  VAR FDoc: TXMLDocument;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context^.checkSideEffects('readXmlFile',tokenLocation,[se_readFile]) then begin
      if fileExists(str0^.value)
      then try
        ReadXMLFile(FDoc, str0^.value)
      except
        on e:Exception do begin
          context^.raiseError('Error parsing XML file '+str0^.value+': '+e.message,tokenLocation);
        end;
      end else begin
        context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'XML File '+str0^.value+' does not exist');
        exit(newVoidLiteral);
      end;
      result:=obtainXmlData(recycler,FDoc);
      if result=nil then begin
        context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'Error parsing XML file '+str0^.value);
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
          context^.raiseError('Error parsing XML input: '+e.message,tokenLocation);
        end;
      end;
      FreeAndNil(input);
      result:=obtainXmlData(recycler,FDoc);
      if result=nil then begin
        context^.raiseError('Error parsing XML input.',tokenLocation);
        result:=newVoidLiteral;
      end;
    end;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'readXmlFile',@readXmlFile_impl,ak_unary,[se_readFile]);
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'readXml'    ,@readXml_impl    ,ak_unary);

end.
