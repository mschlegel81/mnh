UNIT mnh_funcs_xml;
INTERFACE
USES sysutils, mnh_basicTypes,mnh_litVar,mnh_constants,mnh_funcs,mnh_contexts,XMLRead,dom;

IMPLEMENTATION
{$i mnh_func_defines.inc}

FUNCTION obtainXmlData intFuncSignature;
  VAR resultList:P_listLiteral;
  PROCEDURE readAttributes(CONST owner:P_listLiteral; CONST node:TDOMNode);
    VAR j:longint;
    begin
      if not Assigned(node) then exit;
      if Assigned(node.attributes) then begin
        for j:=0 to node.attributes.length-1 do
          owner^.append(newListLiteral(2)^
                       .appendString(node.attributes[j].NodeName)^
                       .appendString(node.attributes[j].NodeValue),false);
      end;
    end;

  PROCEDURE readChildren(CONST owner:P_listLiteral; CONST node:TDOMNode);
    VAR i,j: integer;
        subTree   :P_listLiteral;
    begin
      if not Assigned(node) then exit;
      for i:=0 to node.ChildNodes.count-1 do begin
        {$ifdef debugMode}
        writeln(stdErr,'Reading XML Node: ',node.ChildNodes[i].NodeName);
        {$endif}
        subTree:=newListLiteral(3)^.appendString(node.ChildNodes[i].NodeName)
                                  ^.append(newListLiteral(),false)
                                  ^.append(newListLiteral(),false);
        readAttributes(P_listLiteral(subtree^.value(1)),node.ChildNodes[i]);
        readChildren  (P_listLiteral(subTree^.value(2)),node.ChildNodes[i]);
        owner^.append(subTree,false);
      end;
    end;

  VAR FDoc: TXMLDocument;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      if fileExists(str0^.value)
      then ReadXMLFile(FDoc, str0^.value)
      else begin
        context.adapters^.raiseWarning('XML File '+str0^.value+' does not exist',tokenLocation);
        exit(newVoidLiteral);
      end;

      if Assigned(FDoc) then begin
        resultList:=newListLiteral^.appendString(FDoc.NodeName)^.append(newListLiteral(),false)^.append(newListLiteral(),false);
        readAttributes(P_listLiteral(resultList^.value(1)),FDoc);
        readChildren  (P_listLiteral(resultList^.value(2)),FDoc);
        FreeAndNil(FDoc);
        exit(resultList);
      end;
      context.adapters^.raiseWarning('Error parsing XML file '+str0^.value,tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

INITIALIZATION
  registerRule(FILES_BUILTIN_NAMESPACE,'readXml',@obtainXmlData,'readXml(filename:string);//Reads contents of an XML file and returns the contents as a list');

end.
