UNIT mnh_doc;
INTERFACE
USES sysutils,
     myStringUtil, myGenerics,
     fileWrappers,
     mnh_messages,
     basicTypes, mnh_constants,recyclers,
     Forms,ComCtrls,
     litVar, mnh_html;

VAR lineToHtml:FUNCTION(CONST s:string; CONST forceSyntaxHighlighting:boolean):string=nil;
TYPE
  P_intrinsicFunctionDocumentation = ^T_intrinsicFunctionDocumentation;
  T_intrinsicFunctionDocumentation = object
    id, unqualifiedId: T_idString;
    unqualifiedAccess:boolean;
    docTxt      : T_arrayOfString;
    CONSTRUCTOR create(CONST funcName: ansistring);
    DESTRUCTOR destroy;
    FUNCTION getHtml:ansistring;
    FUNCTION getStructuredInfo(OUT examples:T_arrayOfString):T_structuredRuleInfoList;
    PROCEDURE addExample(CONST txt:T_arrayOfString);
    FUNCTION getHtmlLink:string;
  end;
  F_simpleCallback = PROCEDURE of object;

PROCEDURE makeHtmlFromTemplate;
PROCEDURE registerDoc(CONST qualifiedId:ansistring; CONST qualifiedOnly:boolean);
FUNCTION getDocIndexLinkForBrowser(CONST suffix:string=''):ansistring;
FUNCTION getHtmlRoot:ansistring;
FUNCTION getDemosRoot:ansistring;
FUNCTION getPackagesRoot:ansistring;
VAR functionDocMap:specialize G_stringKeyMap<P_intrinsicFunctionDocumentation>;
    tokenDocumentation:array[T_tokenType] of P_intrinsicFunctionDocumentation;
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

FUNCTION getDocIndexLinkForBrowser(CONST suffix:string=''):ansistring;
  begin
    if suffix=''
    then result:='file:///'+ansiReplaceStr(expandFileName(getHtmlRoot+'/index.html'),'\','/')
    else result:='file:///'+ansiReplaceStr(expandFileName(getHtmlRoot              ),'\','/')+suffix;
  end;

PROCEDURE registerDoc(CONST qualifiedId:ansistring; CONST qualifiedOnly:boolean);
  VAR newDoc:P_intrinsicFunctionDocumentation;
      oldDoc:P_intrinsicFunctionDocumentation;
      outdatedDoc:P_intrinsicFunctionDocumentation;
      replaceQualified:boolean=false;
      replaceUnqualified:boolean=false;
  begin
    new(newDoc,create(qualifiedId));
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
  VAR code:T_arrayOfString=();
      i:longint;
      keys:T_arrayOfString=();
      allDocs:array of P_intrinsicFunctionDocumentation=();

  FUNCTION string_to_tokenType(CONST s:string):T_tokenType;
    VAR t:T_tokenType;
    begin
      for t in T_tokenType do if s=tokenTypeName(t) then exit(t);
      result:=tt_EOL;
    end;

  PROCEDURE processExample;
    VAR ids:T_arrayOfString;
        i:longint;
        doc:P_intrinsicFunctionDocumentation;
        tokenType: T_tokenType;
    begin
      if (length(code)<=0) then exit;
      ids:=split(trim(copy(code[0],4,length(code[0])-3)),' ');
      sortUnique(ids);
      dropFirst(code,1);

      for i:=0 to length(ids)-1 do
      if functionDocMap.containsKey(ids[i],doc)
      then doc^.addExample(code)
      else begin
        tokenType:=string_to_tokenType(ids[i]);
        if tokenType<>tt_EOL then begin
          if tokenDocumentation[tokenType]=nil then new(tokenDocumentation[tokenType],create(tokenTypeName(tokenType)));
          tokenDocumentation[tokenType]^.addExample(code);
        end;
      end;

      code:=C_EMPTY_STRING_ARRAY;
    end;

  VAR decompressed_examples:T_arrayOfString;
  begin
    if functionDocExamplesReady then exit;
    decompressed_examples:=split(decompressString(examples_txt),C_lineBreakChar);
    keys:=functionDocMap.keySet;
    for i:=0 to length(keys)-1 do if isQualified(keys[i]) then begin
      setLength(allDocs,length(allDocs)+1);
      allDocs[length(allDocs)-1]:=functionDocMap.get(keys[i]);
    end;

    {$ifdef debugMode} writeln(stdErr,'        DEBUG: preparing built-in documentation');{$endif}
    //Read examples:---------------------------------------------------------------------
    setLength(code,0);
    for i:=0 to length(decompressed_examples)-1 do begin
      if trim(decompressed_examples[i])=''
      then processExample
      else append(code,decompressed_examples[i]);
    end;
    processExample;
    //---------------------------------------------------------------------:Read examples

    functionDocExamplesReady:=true;
  end;

FUNCTION shortName(CONST id:T_idString):T_idString;
  begin
    if isQualified(id) then result:=split(id,'.')[1] else result:=id;
  end;

CONSTRUCTOR T_intrinsicFunctionDocumentation.create(CONST funcName: ansistring);
  begin
    id:=funcName;
    unqualifiedId:=shortName(funcName);
    setLength(docTxt,0);
  end;

DESTRUCTOR T_intrinsicFunctionDocumentation.destroy;
  begin
    id:='';
    setLength(docTxt,0);
  end;

FUNCTION T_intrinsicFunctionDocumentation.getHtml:ansistring;
  VAR docTxtLine: string;
      lastWasCode:boolean=false;
  FUNCTION br:string;
    begin if lastWasCode then result:='' else result:='<br>'; end;
  begin
    result:='<h4><br><a name="'+id+'">'+id+'</a></h4>';
    for docTxtLine in docTxt do begin
      if startsWith(docTxtLine,'#S ') then begin result+=LineEnding+lineToHtml(copy(docTxtLine,4,maxLongint),true); lastWasCode:=true;  end else
      if startsWith(docTxtLine,'#C ') then begin result+=LineEnding+br+ escapeHtml(copy(docTxtLine,4,maxLongint) ); lastWasCode:=false; end else
      if startsWith(docTxtLine,'#H ') then begin result+=LineEnding+br+            copy(docTxtLine,4,maxLongint)  ; lastWasCode:=false; end else
                                           begin result+=LineEnding+lineToHtml(docTxtLine,false);                   lastWasCode:=true;  end;
    end;
  end;

FUNCTION T_intrinsicFunctionDocumentation.getHtmlLink:string;
  begin
    result:='file:///'+ansiReplaceStr(expandFileName(getHtmlRoot+'/builtin.html#'),'\','/')+id;
  end;

FUNCTION T_intrinsicFunctionDocumentation.getStructuredInfo(OUT examples:T_arrayOfString):T_structuredRuleInfoList;
  VAR info:T_structuredRuleInfo;
      docTxtLine:string;
      first:boolean=true;
  begin
    info.body:='';
    info.comment:='';
    info.idAndSignature:='';
    info.location:=C_nilSearchTokenLocation;
    setLength(result,0);
    setLength(examples,0);
    for docTxtLine in docTxt do begin
      if docTxtLine.startsWith('#S ') then begin
        if not(first) then begin
          setLength(result,length(result)+1);
          result[length(result)-1]:=info;
          info.idAndSignature:='';
          info.comment       :=''
        end;
        info.idAndSignature:=copy(docTxtLine,4,maxLongint);
        first:=false;
      end else if docTxtLine.startsWith('#C ') then begin
        if info.comment=''
        then info.comment:=                copy(docTxtLine,4,maxLongint)
        else info.comment+=C_lineBreakChar+copy(docTxtLine,4,maxLongint);
      end
      else if not docTxtLine.startsWith('#H ') then
      append(examples,docTxtLine);
    end;
    if (info.idAndSignature<>'') or (info.comment<>'') then begin
      setLength(result,length(result)+1);
      result[length(result)-1]:=info;
    end;
  end;

PROCEDURE T_intrinsicFunctionDocumentation.addExample(CONST txt:T_arrayOfString);
  begin
    append(docTxt ,txt);
  end;

PROCEDURE makeHtmlFromTemplate;
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
    try
      prepareBuiltInDocs;
      outFile.isOpen:=false;
      setLength(includes,0);
      context.mode:=none;
      decompressedTemplate:=split(decompressString(html_template_txt),C_lineBreakChar);
      for templateLine in decompressedTemplate do begin
        case context.mode of
          none:            if not(handleCommand(templateLine)) and outFile.isOpen then writeln(outFile.handle,templateLine);
          beautifying:     if not(contextEnds(templateLine))   and outFile.isOpen then writeln(outFile.handle,lineToHtml(templateLine,true));
          definingInclude: if not(contextEnds(templateLine))   then append(context.include.content,templateLine);
        end;
        inc(templateLineCount);
      end;
      htmlDocGeneratedForCodeHash:=CODE_HASH;
      with outFile do if isOpen then close(handle);
      {$ifdef debugMode} writeln(stdErr,'        DEBUG: documentation is ready; ',templateLineCount,' lines processed');{$endif}
      setLength(demoFiles,0);
    except
      //Ignore all exceptions. We have another go at the next program start.
    end;
  end;

VAR docMapIsFinalized:boolean=false;
PROCEDURE finalizeFunctionDocMap;
  VAR entries:functionDocMap.KEY_VALUE_LIST=();
      values:T_arrayOfPointer=();
      i:longint;
      t:T_tokenType;
  begin
    if docMapIsFinalized then exit;
    docMapIsFinalized:=true;
    entries:=functionDocMap.entrySet;
    setLength(values,0);
    for i:=0 to length(entries)-1 do appendIfNew(values,entries[i].value);
    for i:=0 to length(values)-1 do dispose(P_intrinsicFunctionDocumentation(values[i]),destroy);
    functionDocMap.destroy;
    for t in T_tokenType do if tokenDocumentation[t]<>nil then dispose(tokenDocumentation[t],destroy);
  end;

PROCEDURE initTokenDoc;
  VAR t:T_tokenType;
  begin
    for t in T_tokenType do tokenDocumentation[t]:=nil;
  end;

INITIALIZATION
  functionDocMap.create();
  initTokenDoc;
FINALIZATION
  finalizeFunctionDocMap;

end.
