UNIT mnh_doc;
INTERFACE
USES sysutils,
     myStringUtil, myGenerics, serializationUtil,
     fileWrappers,
     mnh_messages,
     basicTypes, mnh_constants,recyclers,
     Forms,ComCtrls,
     litVar, mnh_html;
TYPE
  T_demoCodeToHtmlCallback=PROCEDURE(CONST input:T_arrayOfString; OUT textOut,htmlOut,usedBuiltinIDs:T_arrayOfString; CONST recycler:P_recycler);
VAR demoCodeToHtmlCallback:T_demoCodeToHtmlCallback;
TYPE
  P_intrinsicFunctionDocumentation = ^T_intrinsicFunctionDocumentation;
  T_intrinsicFunctionDocumentation = object
    id, unqualifiedId: T_idString;
    description: ansistring;
    unqualifiedAccess:boolean;
    txtExample,htmlExample: T_arrayOfString;
    relatedDemos:T_arrayOfLongint;
    CONSTRUCTOR create(CONST funcName: ansistring);
    DESTRUCTOR destroy;
    FUNCTION getHtml:ansistring;
    FUNCTION getStructuredInfo(OUT examples:T_arrayOfString):T_structuredRuleInfoList;
    PROCEDURE addExample(CONST html,txt:T_arrayOfString; CONST skipFirstLine:boolean=false);
    FUNCTION getHtmlLink:string;
  end;

FUNCTION addDemoFile(CONST name,content:string):longint;
PROCEDURE linkDemoToDoc(CONST demoIndex:longint; CONST qualifiedId:string);
PROCEDURE makeHtmlFromTemplate(Application:Tapplication; bar:TProgressBar);
PROCEDURE registerDoc(CONST qualifiedId,explanation:ansistring; CONST qualifiedOnly:boolean);
FUNCTION getDocIndexLinkForBrowser(CONST suffix:string=''):ansistring;
FUNCTION getHtmlRoot:ansistring;
FUNCTION getDemosRoot:ansistring;
FUNCTION getPackagesRoot:ansistring;
VAR functionDocMap:specialize G_stringKeyMap<P_intrinsicFunctionDocumentation>;
    htmlDocGeneratedForCodeHash:string;

PROCEDURE finalizeFunctionDocMap;
IMPLEMENTATION
USES strutils;
VAR functionDocExamplesReady:boolean=false;
    demoFiles:array of record
      sourceName,htmlName:string;
    end;

FUNCTION getHtmlRoot:ansistring; begin result:=configDir+'doc'; end;
FUNCTION getDemosRoot:ansistring; begin result:=configDir+'demos'; end;
FUNCTION getPackagesRoot:ansistring; begin result:=configDir+'packages'; end;

FUNCTION addDemoFile(CONST name,content:string):longint;
  begin
    result:=length(demoFiles);
    setLength(demoFiles,result+1);
    demoFiles[result].sourceName:=name;
    demoFiles[result].htmlName:=intToStr(result)+ChangeFileExt(demoFiles[length(demoFiles)-1].sourceName,'.html');
    writeFile(getHtmlRoot+DirectorySeparator+demoFiles[result].htmlName,content);
  end;

PROCEDURE linkDemoToDoc(CONST demoIndex:longint; CONST qualifiedId:string);
  VAR doc:P_intrinsicFunctionDocumentation;
  begin
    if not(functionDocMap.containsKey(qualifiedId,doc)) then exit;
    append(doc^.relatedDemos,demoIndex);
  end;

FUNCTION getDocIndexLinkForBrowser(CONST suffix:string=''):ansistring;
  begin
    if suffix=''
    then result:='file:///'+ansiReplaceStr(expandFileName(getHtmlRoot+'/index.html'),'\','/')
    else result:='file:///'+ansiReplaceStr(expandFileName(getHtmlRoot              ),'\','/')+suffix;
  end;

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

PROCEDURE ensureBuiltinDocExamples(Application:Tapplication; bar:TProgressBar);
  {$include res_examples.inc}
  VAR code:T_arrayOfString=();
      i:longint;
      keys:T_arrayOfString=();
      allDocs:array of P_intrinsicFunctionDocumentation=();

  PROCEDURE addExample(CONST exampleSource,html,txt,idList:T_arrayOfString);
    VAR ids:T_arrayOfString;
        i:longint;
        leadingIdLine:boolean=false;
        doc:P_intrinsicFunctionDocumentation;
        {$ifdef debugMode} first:boolean=true; j:longint; {$endif}
    begin
      if length(exampleSource)<=0 then exit;
      if copy(exampleSource[0],1,3)='//#' then begin
        ids:=split(trim(copy(exampleSource[0],4,length(exampleSource[0])-3)),' ');
        leadingIdLine:=true;
      end else ids:=idList;
      sortUnique(ids);
      for i:=0 to length(ids)-1 do if functionDocMap.containsKey(ids[i],doc) then begin
        {$ifdef debugMode}
        if first then first:=false else begin
          write('The following example is not uniquely assignable. IDs: ');
          for j:=0 to length(ids)-1 do write(ids[j],' ');
          writeln;
          for j:=0 to length(exampleSource)-1 do writeln(exampleSource[j]);
        end;
        {$endif}
        doc^.addExample(html,txt,leadingIdLine);
      end;
      {$ifdef debugMode}
      if first then begin
        write('The following example is not assignable. IDs: ');
        for j:=0 to length(ids)-1 do write(ids[j],' ');
        writeln;
        for j:=0 to length(exampleSource)-1 do writeln(exampleSource[j]);
      end;
      {$endif}
      setLength(ids,0);
    end;

  CONST EXAMPLES_CACHE_FILE= '/examples.dat';
  VAR examplesToStore:array of array[0..3] of T_arrayOfString;
      recycler:P_recycler;

  PROCEDURE processExample;
    VAR html,txt,ids:T_arrayOfString;
    begin
      if (length(code)<=0) then exit;
      demoCodeToHtmlCallback(code,txt,html,ids,recycler);
      addExample(code,html,txt,ids);
      setLength(examplesToStore,length(examplesToStore)+1);
      examplesToStore[length(examplesToStore)-1,0]:=code;
      examplesToStore[length(examplesToStore)-1,1]:=txt;
      examplesToStore[length(examplesToStore)-1,2]:=html;
      examplesToStore[length(examplesToStore)-1,3]:=ids;
      code:=C_EMPTY_STRING_ARRAY;
    end;

  PROCEDURE storeExamples;
    VAR wrapper:T_bufferedOutputStreamWrapper;
        i,j:longint;
    PROCEDURE writeArrayOfString(CONST a:T_arrayOfString);
      VAR s:string;
      begin
        wrapper.writeNaturalNumber(length(a));
        for s in a do wrapper.writeAnsiString(s);
      end;

    begin
      wrapper.createToWriteToFile(getHtmlRoot+EXAMPLES_CACHE_FILE);
      wrapper.writeAnsiString(CODE_HASH);
      wrapper.writeNaturalNumber(length(examplesToStore));
      for i:=0 to length(examplesToStore)-1 do for j:=0 to 3 do writeArrayOfString(examplesToStore[i,j]);
      wrapper.destroy;
    end;

  FUNCTION canRestoreExamples:boolean;
    VAR wrapper:T_bufferedInputStreamWrapper;
        exampleCount,ei:longint;

    FUNCTION readArrayOfString:T_arrayOfString;
      VAR i:longint;
      begin
        //NOTE: initialize(result); leads to memory leak
        setLength(result,wrapper.readNaturalNumber);
        for i:=0 to length(result)-1 do result[i]:=wrapper.readAnsiString;
      end;

    VAR code,txt,html,ids:T_arrayOfString;
    begin
      if not(fileExists(getHtmlRoot+EXAMPLES_CACHE_FILE)) then exit(false);
      wrapper.createToReadFromFile(getHtmlRoot+EXAMPLES_CACHE_FILE);
      if not(wrapper.allOkay) then begin
        wrapper.destroy;
        exit(false);
      end;
      result:=(wrapper.readAnsiString=CODE_HASH);
      exampleCount:=wrapper.readNaturalNumber;
      result:=result and wrapper.allOkay;
      if result then begin
        {$ifdef debugMode} writeln(stdErr,'        DEBUG: restoring built-in documentation');{$endif}
        for ei:=0 to exampleCount-1 do begin
          code:=readArrayOfString;
          txt :=readArrayOfString;
          html:=readArrayOfString;
          ids :=readArrayOfString;
          addExample(code,html,txt ,ids);
        end;
      end;
      result:=result and wrapper.allOkay;
      wrapper.destroy;
    end;

  VAR decompressed_examples:T_arrayOfString;
  begin
    if functionDocExamplesReady then exit;
    decompressed_examples:=split(decompressString(examples_txt),C_lineBreakChar);
    if bar<>nil then begin
      bar.caption:='Executing example code for help/doc';
      Application.ProcessMessages;
    end;
    recycler:=newRecycler;
    keys:=functionDocMap.keySet;
    for i:=0 to length(keys)-1 do if isQualified(keys[i]) then begin
      setLength(allDocs,length(allDocs)+1);
      allDocs[length(allDocs)-1]:=functionDocMap.get(keys[i]);
    end;
    if not(canRestoreExamples) then begin
      {$ifdef debugMode} writeln(stdErr,'        DEBUG: preparing built-in documentation');{$endif}
      setLength(examplesToStore,0);
      //Read examples:---------------------------------------------------------------------
      setLength(code,0);
      for i:=0 to length(decompressed_examples)-1 do begin
        if trim(decompressed_examples[i])='' then processExample
                                             else append(code,decompressed_examples[i]);
        if bar<>nil then begin
          bar.position:=bar.position+1;
          Application.ProcessMessages;
        end;
      end;
      processExample;
      //---------------------------------------------------------------------:Read examples
      storeExamples;
      setLength(examplesToStore,0);
    end else bar.position:=bar.position+length(decompressed_examples);
    functionDocExamplesReady:=true;
    freeRecycler(recycler);
  end;

FUNCTION shortName(CONST id:T_idString):T_idString;
  begin
    if isQualified(id) then result:=split(id,'.')[1] else result:=id;
  end;

CONSTRUCTOR T_intrinsicFunctionDocumentation.create(CONST funcName: ansistring);
  begin
    id:=funcName;
    unqualifiedId:=shortName(funcName);
    setLength(txtExample,0);
    setLength(htmlExample,0);
    setLength(relatedDemos,0);
  end;

DESTRUCTOR T_intrinsicFunctionDocumentation.destroy;
  begin
    id:='';
    description:='';
    setLength(txtExample,0);
    setLength(htmlExample,0);
  end;

FUNCTION T_intrinsicFunctionDocumentation.getHtml:ansistring;
  FUNCTION prettyHtml(CONST s: ansistring): ansistring;
    CONST splitters:array[0..1] of string=('#','//');
    VAR lines: T_arrayOfString;
        i: longint;
    begin
      result:=s;
      lines:=split(s,splitters);
      result:='';
      for i:=0 to length(lines)-1 do if lines[i]<>'' then begin
        if i>0 then result:=result+'<br>';
        if pos(';', lines [i])>0 then result:=result+'<code>'+toHtmlCode(lines [i])+'</code>'
        else result:=result+lines [i];
      end;
    end;
  VAR i:longint;
  begin
    result:='<h4><br><a name="'+id+'">'+id+'</a></h4>'+prettyHtml(description);
    if length(htmlExample)>0 then begin
      result:=result+'<br>Examples:<code>';
      for i:=0 to length(htmlExample)-1 do result:=result+LineEnding+htmlExample[i];
      result:=result+'</code>';
    end;
    if length(relatedDemos)>0 then begin
      result+='<br>Related demos:<ul>';
      for i in relatedDemos do result+='<li><a href='+demoFiles[i].htmlName+'>'+demoFiles[i].sourceName+'</a></li>';
      result+='</ul>';
    end;
  end;

FUNCTION T_intrinsicFunctionDocumentation.getHtmlLink:string;
  begin
    result:='file:///'+ansiReplaceStr(expandFileName(getHtmlRoot+'/builtin.html#'),'\','/')+id;
  end;

FUNCTION T_intrinsicFunctionDocumentation.getStructuredInfo(OUT examples:T_arrayOfString):T_structuredRuleInfoList;
  VAR i:longint;
      parts:T_arrayOfString;
      sigAndComment:T_arrayOfString;
  begin
    parts:=split(description,'#');
    initialize(result);
    setLength(result,length(parts));
    for i:=0 to length(result)-1 do begin
      result[i].location:=C_nilSearchTokenLocation;
      sigAndComment:=split(parts[i],'//');
      result[i].idAndSignature:=sigAndComment[0];
      if length(sigAndComment)>1
      then result[i].comment:=sigAndComment[1]
      else begin
        if startsWith(parts[i],'//') then begin
          result[i].comment:=result[i].idAndSignature;
          result[i].idAndSignature:='';
        end else result[i].comment:='';
      end;
    end;
    examples:=txtExample;
  end;

PROCEDURE T_intrinsicFunctionDocumentation.addExample(CONST html,txt:T_arrayOfString; CONST skipFirstLine:boolean=false);
  VAR i:longint;
  begin
    if skipFirstLine then begin
      for i:=1 to length(txt )-1 do append(txtExample ,txt [i]);
      for i:=1 to length(html)-1 do append(htmlExample,html[i]);
      exit;
    end;
    append(txtExample ,txt);
    append(htmlExample,html);
  end;

PROCEDURE makeHtmlFromTemplate(Application:Tapplication; bar:TProgressBar);
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
      if not(DirectoryExists(getHtmlRoot)) then CreateDir(getHtmlRoot);
      ensureBuiltinDocExamples(Application,bar);
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
          cmdParam:=getHtmlRoot+DirectorySeparator+cmdParam;
          if not(fileExists(cmdParam)) or (CODE_HASH<>htmlDocGeneratedForCodeHash) then begin
            {$ifdef debugMode} writeln(stdErr,'        DEBUG: creating file ',cmdParam);{$endif}
            ensurePath(cmdParam);
            assign(handle,cmdParam);
            rewrite(handle);
            isOpen:=true;
          end else begin
            isOpen:=false;
            {$ifdef debugMode} writeln(stdErr,'        DEBUG: not(!) creating file ',cmdParam);{$endif}
          end;
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
  VAR templateLine:string;
      templateLineCount:longint=0;
      decompressedTemplate:T_arrayOfString;
  begin
    prepareBuiltInDocs;
    outFile.isOpen:=false;
    setLength(includes,0);
    context.mode:=none;
    decompressedTemplate:=split(decompressString(html_template_txt),C_lineBreakChar);
    if bar<>nil then begin
      bar.caption:='Creating html documentation';
      bar.position:=bar.position+1;
      Application.ProcessMessages;
    end;
    for templateLine in decompressedTemplate do begin
      case context.mode of
        none:            if not(handleCommand(templateLine)) and outFile.isOpen then writeln(outFile.handle,templateLine);
        beautifying:     if not(contextEnds(templateLine))   and outFile.isOpen then writeln(outFile.handle,toHtmlCode(templateLine));
        definingInclude: if not(contextEnds(templateLine))   then append(context.include.content,templateLine);
      end;
      inc(templateLineCount);
    end;
    htmlDocGeneratedForCodeHash:=CODE_HASH;
    with outFile do if isOpen then close(handle);
    if bar<>nil then begin
      bar.caption:='Creating html documentation';
      bar.position:=bar.position+1;
      Application.ProcessMessages;
    end;
    {$ifdef debugMode} writeln(stdErr,'        DEBUG: documentation is ready; ',templateLineCount,' lines processed');{$endif}
    setLength(demoFiles,0);
  end;

VAR docMapIsFinalized:boolean=false;
PROCEDURE finalizeFunctionDocMap;
  VAR entries:functionDocMap.KEY_VALUE_LIST=();
      values:T_arrayOfPointer=();
      i:longint;
  begin
    if docMapIsFinalized then exit;
    docMapIsFinalized:=true;
    entries:=functionDocMap.entrySet;
    setLength(values,0);
    for i:=0 to length(entries)-1 do appendIfNew(values,entries[i].value);
    for i:=0 to length(values)-1 do dispose(P_intrinsicFunctionDocumentation(values[i]),destroy);
    functionDocMap.destroy;
  end;

INITIALIZATION
  functionDocMap.create();
FINALIZATION
  finalizeFunctionDocMap;

end.
