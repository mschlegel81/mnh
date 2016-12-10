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
    PROCEDURE addExample(CONST exampleHtml:T_arrayOfString; CONST skipFirstLine:boolean=false);
  end;

PROCEDURE makeHtmlFromTemplate();
PROCEDURE registerDoc(CONST qualifiedId,explanation:ansistring; CONST qualifiedOnly:boolean);
PROCEDURE ensureBuiltinDocExamples;
FUNCTION getHtmlRoot:ansistring;
PROCEDURE ensureDemos;
VAR functionDocMap:specialize G_stringKeyMap<P_intrinsicFunctionDocumentation>;
IMPLEMENTATION
VAR functionDocExamplesReady:boolean=false;
    htmlRoot:ansistring;
CONST PACKAGE_DOC_SUBFOLDER='package_doc';

PROCEDURE ensureDemos;
  {$i res_ensurePackages.inc}
  VAR code:T_arrayOfString;
      i:longint;
  begin
    setLength(code,length(ensurePackages_mnh));
    for i:=0 to length(code)-1 do code[i]:=ensurePackages_mnh[i];
    append(code,'('+escapeString(configDir,es_dontCare)+')');
    demoCodeToHtmlCallback(code);
  end;

FUNCTION getHtmlRoot:ansistring; begin result:=htmlRoot; end;

PROCEDURE registerDoc(CONST qualifiedId,explanation:ansistring; CONST qualifiedOnly:boolean);
  VAR newDoc:P_intrinsicFunctionDocumentation;
      oldDoc:P_intrinsicFunctionDocumentation;
      outdatedDoc:P_intrinsicFunctionDocumentation;
      replaceQualified:boolean=false;
      replaceUnqualified:boolean=false;
  begin
    new(newDoc,create(qualifiedId));
    newDoc^.description:=explanation;
    newDoc^.unqualifiedAccess:=not(qualifiedOnly);
    replaceQualified:=functionDocMap.containsKey(qualifiedId,outdatedDoc);
    functionDocMap.put(qualifiedId,newDoc);
    if not(qualifiedOnly) then begin
      if functionDocMap.containsKey(newDoc^.unqualifiedId,oldDoc) then begin
        oldDoc^.unqualifiedAccess:=false;
        replaceUnqualified:=true;
      end;
      functionDocMap.put(newDoc^.unqualifiedId,newDoc);
    end;
    if replaceQualified and replaceUnqualified then dispose(outdatedDoc,destroy);
  end;

PROCEDURE ensureBuiltinDocExamples;
  {$include res_examples.inc}
  CONST EXAMPLES_CACHE_FILE= '/examples.dat';
        httpServerExample:array[0..7] of string=
('//#startHttpServer',
 ' in> startHttpServer("127.0.0.1:60000",{print("Parameters: "&$p) orElse wrapTextInHttp($p)},1);',
 ' in> httpGet("http://127.0.0.1:60000/index.html?x=0&y=3%2Ax");',
 'Note @<new 6>:2,1 Microserver started. 127.0.0.1:60000',
 'out> void',
 'Parameters: /index.html?x=0&y=3%2Ax',
 'out> "/index.html?x=0&y=3%2Ax"',
 'Note @<new 6>:2,1 Microserver stopped. 127.0.0.1:60000');

  VAR code:T_arrayOfString;
      i:longint;
      keys:T_arrayOfString;
      allDocs:array of P_intrinsicFunctionDocumentation;
      examplesCache:text;

  PROCEDURE addExample(CONST exampleSource,html:T_arrayOfString);
    VAR ids:T_listOfString;
        words:T_arrayOfString;
        i:longint;
        leadingIdLine:boolean=false;
        doc:P_intrinsicFunctionDocumentation;
        {$ifdef debugMode} first:boolean=true; j:longint; {$endif}
    begin
      ids.create;
      if copy(exampleSource[0],1,3)='//#' then begin
        ids.add(trim(copy(exampleSource[0],4,length(exampleSource[0])-3)));
        leadingIdLine:=true;
      end else for i:=0 to length(exampleSource)-1 do if (copy(trim(exampleSource[i]),1,2)<>COMMENT_PREFIX) then begin
        words:=split(replaceAll(cleanString(exampleSource[i],IDENTIFIER_CHARS,'?'),'??','?'),'?');
        ids.addAll(words);
        ids.addAll(split(join(words,'.'),'.'));
      end;
      ids.unique;
      for i:=0 to ids.size-1 do if functionDocMap.containsKey(ids[i],doc) then begin
        {$ifdef debugMode}
        if first then first:=false else begin
          write('The following example is not uniquely assignable. IDs: ');
          for j:=0 to ids.size-1 do write(ids[j],' ');
          writeln;
          for j:=0 to length(exampleSource)-1 do writeln(exampleSource[j]);
        end;
        {$endif}
        doc^.addExample(html,leadingIdLine);
      end;
      ids.destroy
    end;

  PROCEDURE processExample;
    VAR html:T_arrayOfString;
        i:longint;
    begin
      if (length(code)<=0) then exit;
      html:=demoCodeToHtmlCallback(code);
      addExample(code,html);
      for i:=0 to length(html)-1 do writeln(examplesCache,html[i]);
      writeln(examplesCache,'');
      setLength(code,0);
      setLength(html,0);
    end;

  PROCEDURE restoreExample;
    VAR html:T_arrayOfString;
        htmlLine:ansistring;
    begin
      if (length(code)<=0) then exit;
      setLength(html,0);
      repeat
        readln(examplesCache,htmlLine);
        if htmlLine<>'' then append(html,htmlLine);
      until (htmlLine='') or eof(examplesCache);
      addExample(code,html);
      setLength(code,0);
      setLength(html,0);
    end;

  begin
    if functionDocExamplesReady then exit;
    keys:=functionDocMap.keySet;
    setLength(allDocs,0);

    setLength(code,length(httpServerExample));
    for i:=0 to length(code)-1 do if byte(i) in [3,5,7] then code[i]:=httpServerExample[i] else code[i]:=toHtmlCode(httpServerExample[i]);

    for i:=0 to length(keys)-1 do if isQualified(keys[i]) then begin
      setLength(allDocs,length(allDocs)+1);
      allDocs[length(allDocs)-1]:=functionDocMap.get(keys[i]);
    end;
    addExample(httpServerExample,code);
    if fileExists(htmlRoot+EXAMPLES_CACHE_FILE) then begin
      assign(examplesCache,htmlRoot+EXAMPLES_CACHE_FILE);
      reset(examplesCache);
      //Read examples:---------------------------------------------------------------------
      setLength(code,0);
      for i:=0 to length(examples_txt)-1 do
      if trim(examples_txt[i])='' then restoreExample
                                      else append(code,examples_txt[i]);
      restoreExample;
      //---------------------------------------------------------------------:Read examples
      close(examplesCache);
    end else begin
      assign(examplesCache,htmlRoot+EXAMPLES_CACHE_FILE);
      rewrite(examplesCache);
      //Read examples:---------------------------------------------------------------------
      setLength(code,0);
      for i:=0 to length(examples_txt)-1 do
      if trim(examples_txt[i])='' then processExample
                                      else append(code,examples_txt[i]);
      processExample;
      //---------------------------------------------------------------------:Read examples
      close(examplesCache);
    end;
    functionDocExamplesReady:=true;
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
    {$ifdef debugMode}
    if (length(example)=0) and functionDocExamplesReady then writeln(stdErr,'               undocumented builtin function: ',id);
    {$endif}
    id:='';
    description:='';
    setLength(example,0);
  end;

FUNCTION T_intrinsicFunctionDocumentation.getHtml:ansistring;
  FUNCTION prettyHtml(CONST s: ansistring): ansistring;
    VAR lines: T_arrayOfString;
        i: longint;
    begin
      result:=s;
      setLength(lines, 0);
      while pos('#', result)>0 do begin
        append(lines, copy(result, 1, pos('#', result)-1));
        result:=copy(result, pos('#', result)+1, length(result));
      end;
      append(lines,result);

      result:='';
      for i:=0 to length(lines)-1 do
        begin
        if i>0 then result:=result+'<br>';
        if pos(';', lines [i])>0 then result:=result+'<code>'+toHtmlCode(lines [i])+'</code>'
        else result:=result+lines [i];
      end;
    end;
  VAR i:longint;
  begin
    result:='<h4><br><a name="'+id+'">'+id+'</a></h4>'+prettyHtml(description);
    if length(example)>0 then begin
      result:=result+'<br>Examples:<code>';
      for i:=0 to length(example)-1 do result:=result+LineEnding+example[i];
      result:=result+'</code>';
    end;
  end;

FUNCTION T_intrinsicFunctionDocumentation.getPlainText(CONST lineSplitter:string):ansistring;
  VAR i:longint;
  begin
    result:=id+lineSplitter+replaceAll(description,'#',lineSplitter);
    if length(example)>0 then result:=result+lineSplitter+'Examples:';
    for i:=0 to length(example)-1 do result:=result+lineSplitter+StripHTML(example[i]);
  end;

PROCEDURE T_intrinsicFunctionDocumentation.addExample(CONST exampleHtml:T_arrayOfString; CONST skipFirstLine:boolean=false);
  VAR i:longint;
  begin
    if skipFirstLine then for i:=1 to length(exampleHtml)-1 do append(example,exampleHtml[i])
                     else append(example,exampleHtml);
  end;

PROCEDURE makeHtmlFromTemplate();
  VAR builtInDoc: array[T_namespace] of array of P_intrinsicFunctionDocumentation;

  PROCEDURE prepareBuiltInDocs;
    FUNCTION namespace(CONST id:ansistring):T_namespace;
      VAR useId:ansistring;
          n:T_namespace;
      begin
        if isQualified(id) then useId:=split(id,ID_QUALIFY_CHARACTER)[0] else useId:=id;
        for n:=low(T_namespace) to high(T_namespace) do if C_namespaceString[n]=useId then exit(n);
        result:=DEFAULT_BUILTIN_NAMESPACE;
      end;

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

  PROCEDURE documentBuiltIns(VAR outFile:text);
    CONST prelude='<div class="navContent"><ul><li><h3><a href="index.html">Quick start</a></h4></li><li><h3><a href="types.html">Types</a></h4></li><li><h3><a href="operators.html">Operators</a></h4></li><li><h3><a href="functions.html">Functions</a></h4></li><li><h3><a href="specials.html">Special constructs</a></h4></li><li><h3><a href="builtin.html">Built-in Functions</a></h4></li><ul>';
          interlude='</ul><li><h3><a href="formatStrings.html">Format Strings</a></h4></li><li><h3><a href="packages.html">User packages</a></h4></li></ul></div><div class="docContent"><table border="0" align="center">';
          coda='</table></div>';
    VAR i: longint;
        n: T_namespace;
    begin
      writeln(outFile,prelude);
      for n:=low(T_namespace) to high(T_namespace) do writeln(outFile,'<li><h4><a href="#'+C_namespaceString[n]+'">'+C_namespaceString[n]+'</a></h4></li>');
      writeln(outFile,interlude);
      for n:=low(T_namespace) to high(T_namespace) do for i:=0 to length(builtInDoc[n])-1 do
        writeln(outFile, '<a href="#', builtInDoc[n][i]^.id, '">', builtInDoc[n][i]^.id, '</a> &nbsp; ');
      writeln(outFile, '</div><br>');

      for n:=low(T_namespace) to high(T_namespace) do begin
        writeln(outFile,'<div align="right"><hr></div><h3><br><a name="'+C_namespaceString[n]+'">'+C_namespaceString[n]+'<a></h3>');
        for i:=0 to length(builtInDoc[n])-1 do writeln(outFile, '<a href="#', builtInDoc[n][i]^.id, '">', builtInDoc[n][i]^.id, '</a> &nbsp; ');
        for i:=0 to length(builtInDoc[n])-1 do write(outFile,builtInDoc[n][i]^.getHtml);
      end;
      writeln(outFile,coda);
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

  FUNCTION handleCommand(CONST cmd:ansistring):boolean;
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

    VAR i,j:longint;
        tCmd,
        cmdParam:ansistring;
    begin
      tCmd:=trim(cmd);
      if not(startsWith(tCmd,'<!--')) then exit(false);
      cmdParam:=commandParameter(tCmd);
      if startsWith(tCmd,FILE_SWITCH_PREFIX) then begin
        with outFile do begin
          if isOpen then close(handle);
          assign(handle,htmlRoot+DirectorySeparator+cmdParam);
          rewrite(handle);
          isOpen:=true;
        end;
        exit(true);
      end;
      if tCmd=BUILTIN_DOC_CMD then begin
        if outFile.isOpen then documentBuiltIns(outFile.handle);
        exit(true);
      end;
      if tCmd=START_BEAUTIFY_CMD then begin
        context.mode:=beautifying;
        exit(true);
      end;
      if startsWith(tCmd,START_INCLUDE_PREFIX) then begin
        context.mode:=definingInclude;
        context.include.includeTag:='<!--'+cmdParam+'-->';
        setLength(context.include.content,0);
        exit(true);
      end;
      for i:=0 to length(includes)-1 do if includes[i].includeTag=tCmd then begin
        with outFile do if isOpen then for j:=0 to length(includes[i].content)-1 do writeln(handle,includes[i].content[j]);
        exit(true);
      end;
      result:=false;
    end;
  {$include res_html_template.inc}
  VAR i:longint;
  begin
    prepareBuiltInDocs;
    outFile.isOpen:=false;
    setLength(includes,0);
    context.mode:=none;

    for i:=0 to length(html_template_txt)-1 do begin
      case context.mode of
        none:            if not(handleCommand(html_template_txt[i])) and outFile.isOpen then writeln(outFile.handle,html_template_txt[i]);
        beautifying:     if not(contextEnds(html_template_txt[i]))   and outFile.isOpen then writeln(outFile.handle,toHtmlCode(html_template_txt[i]));
        definingInclude: if not(contextEnds(html_template_txt[i]))   then append(context.include.content,html_template_txt[i]);
      end;
    end;
    with outFile do if isOpen then close(handle);
  end;

PROCEDURE finalizeFunctionDocMap;
  VAR entries:functionDocMap.KEY_VALUE_LIST;
      values:specialize G_list<P_intrinsicFunctionDocumentation>;
      i:longint;
  begin
    entries:=functionDocMap.entrySet;
    values.create;
    for i:=0 to length(entries)-1 do values.add(entries[i].value);
    values.unique;
    for i:=0 to values.size-1 do dispose(values[i],destroy);
    functionDocMap.destroy;
  end;

INITIALIZATION
  htmlRoot:=configDir+'doc';
  if not(DirectoryExists(htmlRoot)) then CreateDir(htmlRoot);
  functionDocMap.create();
FINALIZATION
  finalizeFunctionDocMap;

end.
