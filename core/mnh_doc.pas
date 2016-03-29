UNIT mnh_doc;
INTERFACE
USES sysutils, myStringUtil, myGenerics, mnh_constants, mnh_litVar, mnh_html,mnh_fileWrappers;
TYPE
  T_demoCodeToHtmlCallback=FUNCTION(CONST input:T_arrayOfString):T_arrayOfString;
  T_registerDocProcedure=PROCEDURE(CONST qualifiedId,explanation:ansistring);
VAR demoCodeToHtmlCallback:T_demoCodeToHtmlCallback;
TYPE
  P_intrinsicFunctionDocumentation = ^T_intrinsicFunctionDocumentation;
  T_intrinsicFunctionDocumentation = object
    id, unqualifiedId, description: ansistring;
    unqualifiedAccess:boolean;
    example: T_arrayOfString;
    CONSTRUCTOR create(CONST funcName: ansistring);
    DESTRUCTOR destroy;
    FUNCTION getHtml:ansistring;
    FUNCTION getPlainText(CONST lineSplitter:string):ansistring;
    PROCEDURE addExampleIfRelevant(CONST exampleSource,exampleHtml:T_arrayOfString);
  end;

  P_userPackageDocumentation = ^T_userPackageDocumentation;
  T_userPackageDocumentation = object
    uid: ansistring;
    id: ansistring;
    docFileName: ansistring;
    rawUses: T_arrayOfString;
    usesPackage, usedByPackage: array of P_userPackageDocumentation;
    rulesDoc: T_arrayOfString;
    isExecutable:boolean;
    sourceCode: T_arrayOfString;
    CONSTRUCTOR create(CONST path, name: ansistring; CONST code:T_arrayOfString);
    DESTRUCTOR destroy;
    PROCEDURE addUses(OtherUid: ansistring);
    PROCEDURE addUsed(other: P_userPackageDocumentation);
    PROCEDURE resolveUses;
    PROCEDURE writePackageDoc;
    PROCEDURE addRuleDoc(CONST htmlDoc:ansistring);
    FUNCTION getHref(CONST fromHtmlRoot:boolean): ansistring;
  end;


PROCEDURE addPackageDoc(CONST doc:P_userPackageDocumentation);
PROCEDURE makeHtmlFromTemplate;
PROCEDURE registerDoc(CONST qualifiedId,explanation:ansistring; CONST qualifiedOnly:boolean);
PROCEDURE ensureBuiltinDocExamples;
FUNCTION getHtmlRoot:ansistring;
VAR functionDocMap:specialize G_stringKeyMap<P_intrinsicFunctionDocumentation>;
IMPLEMENTATION
VAR packages: array of P_userPackageDocumentation;
    functionDocExamplesReady:boolean=false;
    htmlRoot:ansistring;
CONST PACKAGE_DOC_SUBFOLDER='package_doc';

FUNCTION getHtmlRoot:ansistring; begin result:=htmlRoot; end;

PROCEDURE registerDoc(CONST qualifiedId,explanation:ansistring; CONST qualifiedOnly:boolean);
  VAR newDoc:P_intrinsicFunctionDocumentation;
      oldDoc:P_intrinsicFunctionDocumentation;
  begin
    new(newDoc,create(qualifiedId));
    newDoc^.description:=explanation;
    newDoc^.unqualifiedAccess:=not(qualifiedOnly);
    functionDocMap.put(qualifiedId,newDoc);
    if not(qualifiedOnly) then begin
      if functionDocMap.containsKey(newDoc^.unqualifiedId,oldDoc) then oldDoc^.unqualifiedAccess:=false;
      functionDocMap.put(newDoc^.unqualifiedId,newDoc);
    end;
  end;

PROCEDURE ensureBuiltinDocExamples;
  {$include res_examples.inc}
  VAR code:T_arrayOfString;
      i:longint;
      keys:T_arrayOfString;
      allDocs:array of P_intrinsicFunctionDocumentation;

  PROCEDURE processExample;
    VAR html:T_arrayOfString;
        i:longint;
    begin
      if (length(code)<=0) then exit;
      html:=demoCodeToHtmlCallback(code);
      for i:=0 to length(allDocs)-1 do allDocs[i]^.addExampleIfRelevant(code,html);
      setLength(code,0);
      setLength(html,0);
    end;

  begin
    if functionDocExamplesReady then exit;
    keys:=functionDocMap.keySet;
    setLength(allDocs,0);
    for i:=0 to length(keys)-1 do if isQualified(keys[i]) then begin
      setLength(allDocs,length(allDocs)+1);
      allDocs[length(allDocs)-1]:=functionDocMap.get(keys[i]);
    end;

    //Read examples:---------------------------------------------------------------------
    setLength(code,0);
    for i:=0 to length(doc_examples_txt)-1 do
    if trim(doc_examples_txt[i])='' then processExample
                                    else append(code,doc_examples_txt[i]);
    processExample;
    //---------------------------------------------------------------------:Read examples
    functionDocExamplesReady:=true;
  end;

PROCEDURE addPackageDoc(CONST doc:P_userPackageDocumentation);
  begin
    setLength(packages,length(packages)+1);
    packages[length(packages)-1]:=doc;
  end;

CONSTRUCTOR T_userPackageDocumentation.create(CONST path, name: ansistring; CONST code:T_arrayOfString);
  begin
    isExecutable:=false;
    id:=name;
    uid:=expandFileName(path);
    docFileName:=replaceAll(replaceAll(replaceAll(replaceAll(replaceAll(uid, ':', '_'), '\', '_'),
                    '/', '_'), '.', '_'), '__','_')+'.html';
    sourceCode:=code;
    setLength(rawUses, 0);
    setLength(usedByPackage, 0);
    setLength(usedByPackage, 0);
    setLength(rulesDoc, 0);
  end;

DESTRUCTOR T_userPackageDocumentation.destroy;
  begin
    setLength(rawUses, 0);
    setLength(usedByPackage, 0);
    setLength(usedByPackage, 0);
    setLength(rulesDoc, 0);
  end;

PROCEDURE T_userPackageDocumentation.addUses(OtherUid: ansistring);
  begin
    setLength(rawUses, length(rawUses)+1);
    rawUses[length(rawUses)-1]:=OtherUid;
  end;

PROCEDURE T_userPackageDocumentation.addUsed(other: P_userPackageDocumentation);
  begin
    setLength(usedByPackage, length(usedByPackage)+1);
    usedByPackage[length(usedByPackage)-1]:=other;
  end;

PROCEDURE T_userPackageDocumentation.resolveUses;
  FUNCTION packageByPath(CONST path: ansistring): P_userPackageDocumentation;
    VAR i: longint;
    begin
      for i:=0 to length(packages)-1 do if packages [i]^.uid = path then exit(packages [i]);
      result:=nil;
    end;

  VAR other: P_userPackageDocumentation;
    i: longint;
  begin
    setLength(usesPackage, 0);
    for i:=0 to length(rawUses)-1 do
      begin
      other:=packageByPath(rawUses [i]);
      if other<>nil then
        begin
        setLength(usesPackage, length(usesPackage)+1);
        usesPackage[length(usesPackage)-1]:=other;
        other^.addUsed(@self);
        end;
      end;
    setLength(rawUses, 0);
  end;

PROCEDURE T_userPackageDocumentation.writePackageDoc;
  FUNCTION getUses: ansistring;
    VAR i: longint;
    begin
      if length(usesPackage) = 0 then exit('none');
      result:='';
      for i:=0 to length(usesPackage)-1 do
        begin
        if result<>'' then result:=result+', ';
        result:=result+usesPackage [i]^.getHref(false);
        end;
    end;

  FUNCTION getUsed: ansistring;
    VAR i: longint;
    begin
      if length(usedByPackage) = 0 then exit('none');
      result:='';
      for i:=0 to length(usedByPackage)-1 do
        begin
        if result<>'' then result:=result+', ';
        result:=result+usedByPackage [i]^.getHref(false);
        end;
    end;
  VAR handle:text;
      i:longint;
      blobLevel:longint=0;
  begin
    assign(handle,htmlRoot+DirectorySeparator+PACKAGE_DOC_SUBFOLDER+DirectorySeparator+docFileName);
    rewrite(handle);
    writeln(handle,'<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/transitional.dtd">');
    writeln(handle,'<html><head><link rel="stylesheet" href="../style.css" type="text/css"><META http-equiv="Content-Type" content="text/html; charset=ASCII">');
    writeln(handle,'<title>',id,'</title></head><body>');
    writeln(handle,'<h4>'+id+'</h4><table class="oben">');
    writeln(handle,'<tr class="oben"><td>Path: </td><td><a href="file:///'+replaceAll(uid,'\','/')+'"><code>'+uid+'</code></a></td></tr>');
    writeln(handle,'<tr class="oben"><td>Uses: </td><td>'+getUses+'</td></tr>');
    writeln(handle,'<tr class="oben"><td>Publishes: </td><td>');
    for i:=0 to length(rulesDoc)-1 do writeln(handle,rulesDoc[i]);
    writeln(handle, '</td></tr>'+'<tr class="oben"><td></ul>Used by: </td><td>'+getUsed+'</td></tr></table>');
    writeln(handle,'<div align="right"> <hr> </div><code>');
    for i:=0 to length(sourceCode)-1 do writeln(handle,toHtmlCode(sourceCode[i],blobLevel));
    writeln(handle,'</code><div align="right"> <hr> </div> </body> </html>');
    close(handle);
  end;

PROCEDURE T_userPackageDocumentation.addRuleDoc(CONST htmlDoc:ansistring);
  begin
    setLength(rulesDoc,length(rulesDoc)+1);
    rulesDoc[length(rulesDoc)-1]:=htmlDoc;
  end;

FUNCTION T_userPackageDocumentation.getHref(CONST fromHtmlRoot:boolean): ansistring;
  begin
    if fromHtmlRoot
    then result:='<a href="'+PACKAGE_DOC_SUBFOLDER+'/'+docFileName+'">'+id+'</a>'
    else result:='<a href="'+                          docFileName+'">'+id+'</a>';
  end;

FUNCTION namespace(CONST id:ansistring):T_namespace;
  VAR useId:ansistring;
      n:T_namespace;
  begin
    if isQualified(id) then useId:=split(id,C_ID_QUALIFY_CHARACTER)[0] else useId:=id;
    for n:=low(T_namespace) to high(T_namespace) do if C_namespaceString[n]=useId then exit(n);
  end;

FUNCTION shortName(CONST id:ansistring):ansistring;
  begin
    if isQualified(id) then result:=split(id,'.')[1] else result:=id;
  end;


CONSTRUCTOR T_intrinsicFunctionDocumentation.create(CONST funcName: ansistring);
  begin
    id:=funcName;
    unqualifiedId:=shortName(funcName);
    setLength(example,0);
  end;

DESTRUCTOR T_intrinsicFunctionDocumentation.destroy;
  begin
    id:='';
    description:='';
    setLength(example,0);
  end;

FUNCTION T_intrinsicFunctionDocumentation.getHtml:ansistring;
  FUNCTION prettyHtml(s: ansistring): ansistring;
    VAR lines: T_arrayOfString;
        i: longint;
        blobLevel:longint=0;
    begin
      setLength(lines, 0);
      while pos('#', s)>0 do begin
        append(lines, copy(s, 1, pos('#', s)-1));
        s:=copy(s, pos('#', s)+1, length(s));
      end;
      append(lines,s);

      result:='';
      for i:=0 to length(lines)-1 do
        begin
        if i>0 then result:=result+'<br>';
        if pos(';', lines [i])>0 then result:=result+'<code>'+toHtmlCode(lines [i],blobLevel)+'</code>'
        else result:=result+lines [i];
      end;
    end;
  VAR i:longint;
  begin
    result:='<h4><a name="'+id+'">'+id+'</a></h4>'+prettyHtml(description);
    if length(example)>0 then begin
      result:=result+'<br>Examples:<code>';
      for i:=0 to length(example)-1 do result:=result+LineEnding+example[i];
      result:=result+'</code>';
    end;
    for i:=0 to length(example)-1 do writeln(StripHTML(example[i]));
  end;

FUNCTION T_intrinsicFunctionDocumentation.getPlainText(CONST lineSplitter:string):ansistring;
  VAR i:longint;
  begin
    result:=id+lineSplitter+replaceAll(description,'#',lineSplitter);
    if length(example)>0 then result:=result+lineSplitter+'Examples:';
    for i:=0 to length(example)-1 do result:=result+lineSplitter+StripHTML(example[i]);
  end;

PROCEDURE T_intrinsicFunctionDocumentation.addExampleIfRelevant(CONST exampleSource,exampleHtml:T_arrayOfString);
  VAR isRelevant:boolean=false;
      i,j:longint;
      words:T_arrayOfString;
  begin
    for i:=0 to length(exampleSource)-1 do if not(isRelevant) and (copy(trim(exampleSource[i]),1,2)<>'//') then begin
      words:=split(replaceAll(cleanString(exampleSource[i],IDENTIFIER_CHARS,'?'),'??','?'),'?');
      for j:=0 to length(words)-1 do isRelevant:=isRelevant or
        (words[j]=id) or
        (unqualifiedAccess) and (words[j]=unqualifiedId) or (words[j]='.'+unqualifiedId);
      setLength(words,0);
    end;
    if isRelevant then append(example,exampleHtml);
  end;

PROCEDURE makeHtmlFromTemplate();
  VAR builtInDoc: array[T_namespace] of array of P_intrinsicFunctionDocumentation;

  PROCEDURE prepareBuiltInDocs;
    VAR ids: T_arrayOfString;
        i,j: longint;
        n: T_namespace;
        swapTmp: P_intrinsicFunctionDocumentation;
    begin
      ensureBuiltinDocExamples;
      //Prepare and sort data:-------------------------------------------------------------
      for n:=low(T_namespace) to high(T_namespace) do setLength(builtInDoc[n],0);
      ids:=functionDocMap.keySet;
      for i:=0 to length(ids)-1 do if isQualified(ids[i]) then begin
        n:=namespace(ids[i]);
        j:=length(builtInDoc[n]);
        setLength(builtInDoc[n],j+1);
        builtInDoc[n][j]:=functionDocMap.get(ids[i]);
      end;
      setLength(ids,0);

      for n:=low(T_namespace) to high(T_namespace) do
      for i:=1 to length(builtInDoc[n])-1 do for j:=0 to i-1 do
      if builtInDoc[n][i]^.id < builtInDoc[n][j]^.id then begin
        swapTmp:=builtInDoc[n][i]; builtInDoc[n][i]:=builtInDoc[n][j]; builtInDoc[n][j]:=swapTmp;
      end;
      //-------------------------------------------------------------:Prepare and sort data
    end;

  PROCEDURE writeUserPackageDocumentations(VAR outFile:text);
    PROCEDURE clearPackageSubFolder;
      VAR files:T_arrayOfString;
          i:longint;
      begin
        CreateDir(htmlRoot+DirectorySeparator+PACKAGE_DOC_SUBFOLDER);
        files:=find(htmlRoot+DirectorySeparator+PACKAGE_DOC_SUBFOLDER+DirectorySeparator+'*',true,false);
        for i:=0 to length(files)-1 do DeleteFile(files[i]);
      end;

    VAR i: longint;
    begin
      clearPackageSubFolder;
      for i:=0 to length(packages)-1 do packages[i]^.resolveUses;
      write(outFile, '<table>');
      for i:=0 to length(packages)-1 do begin
        if odd(i) then write(outFile, '<tr>')
        else write(outFile, '<tr class="ruleHead">');
        write(outFile, '<td>', packages [i]^.getHref(true), '</td><td><a href="file:///'+replaceAll(packages [i]^.uid,'\','/')+'"><code>'+packages [i]^.uid+'</code></a></td>');
        if packages [i]^.isExecutable then
          write(outFile, '<td>executable</td>') else write(outFile, '<td>&nbsp;</td>');
        writeln(outFile, '</tr>');
      end;
      write(outFile, '</table>');
      for i:=0 to length(packages)-1 do packages[i]^.writePackageDoc;
      for i:=0 to length(packages)-1 do dispose(packages [i], destroy);
      setLength(packages, 0);
    end;

  PROCEDURE documentBuiltIns(VAR outFile:text);
    VAR i: longint;
        n: T_namespace;
    begin
      writeln(outFile, '<div align="right"><hr></div><br><div>');
      for n:=low(T_namespace) to high(T_namespace) do for i:=0 to length(builtInDoc[n])-1 do
        writeln(outFile, '<a href="#', builtInDoc[n][i]^.id, '">', builtInDoc[n][i]^.id, '</a> &nbsp; ');
      writeln(outFile, '</div><br><div align="right"><hr></div>');
      for n:=low(T_namespace) to high(T_namespace) do
        writeln(outFile,'<h4><a href="#'+C_namespaceString[n]+'">'+C_namespaceString[n]+'</a></h4>');

      for n:=low(T_namespace) to high(T_namespace) do begin
        writeln(outFile,'<div align="right"><hr></div><h3><a name="'+C_namespaceString[n]+'">'+C_namespaceString[n]+'<a></h3>');
        for i:=0 to length(builtInDoc[n])-1 do writeln(outFile, '<a href="#', builtInDoc[n][i]^.id, '">', builtInDoc[n][i]^.id, '</a> &nbsp; ');
        for i:=0 to length(builtInDoc[n])-1 do write(outFile,builtInDoc[n][i]^.getHtml);
      end;
    end;

  TYPE T_include=record
         includeTag:ansistring;
         content:T_arrayOfString;
       end;

  VAR outFile:record
        handle:text;
        isOpen:boolean;
      end;
      includes:array of T_include;
      context:record
        mode:(none,beautifying,definingInclude);
        include:T_include;
      end;

  FUNCTION contextEnds(CONST txt:ansistring) :boolean;
    CONST END_CONTEXT_CMD='<!--end-->';
    begin
      if trim(txt)=END_CONTEXT_CMD then begin
        if context.mode = definingInclude then begin
          setLength(includes,length(includes)+1);
          includes[length(includes)-1]:=context.include;
        end;
        context.mode:=none;
        exit(true);
        result:=true;
      end else result:=false;
    end;

  CONST BUILTIN_FILE_NAME='builtin.html';
  VAR blobLevel:longint=0;

  FUNCTION builtInReady:boolean;
    begin
      result:=fileExists(htmlRoot+DirectorySeparator+BUILTIN_FILE_NAME);
    end;

  FUNCTION handleCommand(cmd:ansistring):boolean;
    FUNCTION commandParameter(CONST txt:ansistring):ansistring;
      VAR i:longint;
      begin
        result:='';
        i:=1;
        while (i<=length(txt)) and (txt[i]<>' ') do inc(i);
        while (i<=length(txt)) and (txt[i]<>'-') do begin
          result:=result+txt[i];
          inc(i);
        end;
        result:=trim(result);
      end;

    CONST FILE_SWITCH_PREFIX  ='<!--file ';
          START_INCLUDE_PREFIX='<!--begin ';
          START_BEAUTIFY_CMD  ='<!--mnh-->';
          BUILTIN_DOC_CMD     ='<!--BUILTIN-->';
          PACKAGE_DOC_CMD     ='<!--USERPKG-->';

    VAR i,j:longint;
        cmdParam:ansistring;
    begin
      cmd:=trim(cmd);
      if not(startsWith(cmd,'<!--')) then exit(false);
      cmdParam:=commandParameter(cmd);
      if startsWith(cmd,FILE_SWITCH_PREFIX) then begin
        with outFile do begin
          if isOpen then close(handle);
          if not((cmdParam=BUILTIN_FILE_NAME) and builtInReady) then begin
            assign(handle,htmlRoot+'\'+cmdParam);
            rewrite(handle);
            isOpen:=true;
          end else isOpen:=false;

        end;
        exit(true);
      end;
      if cmd=BUILTIN_DOC_CMD then begin
        if outFile.isOpen then documentBuiltIns(outFile.handle);
        exit(true);
      end;
      if cmd=PACKAGE_DOC_CMD then begin
        if outFile.isOpen then writeUserPackageDocumentations(outFile.handle);
        exit(true);
      end;
      if cmd=START_BEAUTIFY_CMD then begin
        context.mode:=beautifying;
        blobLevel:=0;
        exit(true);
      end;
      if startsWith(cmd,START_INCLUDE_PREFIX) then begin
        context.mode:=definingInclude;
        context.include.includeTag:='<!--'+cmdParam+'-->';
        setLength(context.include.content,0);
        exit(true);
      end;
      for i:=0 to length(includes)-1 do if includes[i].includeTag=cmd then begin
        with outFile do if isOpen then for j:=0 to length(includes[i].content)-1 do writeln(handle,includes[i].content[j]);
        exit(true);
      end;
      result:=false;
    end;
  {$include res_html_template.inc}
  VAR i:longint;
  begin
    if not(builtInReady) then prepareBuiltInDocs;
    outFile.isOpen:=false;
    setLength(includes,0);
    context.mode:=none;

    for i:=0 to length(doc_html_template_txt)-1 do begin
      case context.mode of
        none:            if not(handleCommand(doc_html_template_txt[i])) and outFile.isOpen then writeln(outFile.handle,doc_html_template_txt[i]);
        beautifying:     if not(contextEnds(doc_html_template_txt[i]))   and outFile.isOpen then writeln(outFile.handle,toHtmlCode(doc_html_template_txt[i],blobLevel));
        definingInclude: if not(contextEnds(doc_html_template_txt[i]))   then append(context.include.content,doc_html_template_txt[i]);
      end;
    end;
    with outFile do if isOpen then close(handle);
  end;

PROCEDURE disposeFunctionDocMap;
  VAR keys:T_arrayOfString;
      doc:P_intrinsicFunctionDocumentation;
      i:longint;
  begin
    keys:=functionDocMap.keySet;
    for i:=0 to length(keys)-1 do if isQualified(keys[i]) then begin
      doc:=functionDocMap.get(keys[i]);
      dispose(doc,destroy);
    end;
    functionDocMap.destroy;
  end;

INITIALIZATION
  htmlRoot:=GetAppConfigDir(true)+'doc';
  if not(DirectoryExists(htmlRoot)) then CreateDir(htmlRoot);
  functionDocMap.create();
FINALIZATION
  functionDocMap.destroy;

end.
