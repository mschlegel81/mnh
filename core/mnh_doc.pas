UNIT mnh_doc;
INTERFACE
USES sysutils, mnh_funcs, myStringUtil, myGenerics, mnh_constants, mnh_litVar, mnh_html;
VAR htmlRoot: string;
TYPE

  { T_intrinsicFunctionDocumentation }

  T_intrinsicFunctionDocumentation = object
    id, description: ansistring;
    example:T_arrayOfString;
    CONSTRUCTOR create(CONST funcName: ansistring);
    DESTRUCTOR destroy;
    FUNCTION getHtml:ansistring;
  end;

  P_userPackageDocumentation = ^T_userPackageDocumentation;

  { T_userPackageDocumentation }

  T_userPackageDocumentation = object
    uid: ansistring;
    id: ansistring;
    docFileName: ansistring;
    rawUses: T_arrayOfString;
    usesPackage, usedByPackage: array of P_userPackageDocumentation;
    rulesDoc: array of ansistring;
    isExecutable:boolean;
    CONSTRUCTOR create(CONST path, name: ansistring);
    DESTRUCTOR destroy;
    PROCEDURE addUses(OtherUid: ansistring);
    PROCEDURE addUsed(other: P_userPackageDocumentation);
    PROCEDURE resolveUses;
    FUNCTION toHtml: ansistring;
    PROCEDURE addRuleDoc(CONST htmlDoc:ansistring);
    FUNCTION getHref: ansistring;
  end;


PROCEDURE addPackageDoc(CONST doc:P_userPackageDocumentation);
PROCEDURE makeHtmlFromTemplate(CONST prepareOnly:boolean);
PROCEDURE writeBuiltInDoc(CONST id:ansistring);
FUNCTION getBuiltInDocTxt(CONST id:ansistring):ansistring;

IMPLEMENTATION
VAR packages: array of P_userPackageDocumentation;

PROCEDURE addPackageDoc(CONST doc:P_userPackageDocumentation);
  begin
    setLength(packages,length(packages)+1);
    packages[length(packages)-1]:=doc;
  end;

{ T_userPackageDocumentation }

CONSTRUCTOR T_userPackageDocumentation.create(CONST path, name: ansistring);
  begin
    isExecutable:=false;
    id:=name;
    uid:=expandFileName(path);
    docFileName:=replaceAll(replaceAll(replaceAll(replaceAll(uid, ':', '_'), '\', '_'),
      '/', '_'), '.', '_');
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

FUNCTION T_userPackageDocumentation.toHtml: ansistring;
  FUNCTION getUses: ansistring;
    VAR i: longint;
    begin
      if length(usesPackage) = 0 then exit('none');
      result:='';
      for i:=0 to length(usesPackage)-1 do
        begin
        if result<>'' then result:=result+', ';
        result:=result+usesPackage [i]^.getHref;
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
        result:=result+usedByPackage [i]^.getHref;
        end;
    end;

  VAR i: longint;

  begin
    result:='<h4><a name="'+docFileName+'">'+id+'</a></h4><table class="oben">'+
      '<tr class="oben"><td>Path: </td><td><a href="file:///'+replaceAll(uid,'\','/')+'"><code>'+uid+'</code></a></td></tr>'+
      '<tr class="oben"><td>Uses: </td><td>'+getUses+'</td></tr>'+
      '<tr class="oben"><td>Publishes: </td><td>';
    for i:=0 to length(rulesDoc)-1 do result:=result+rulesDoc[i];
    result:=result+'</td></tr>'+'<tr class="oben"><td></ul>Used by: </td><td>'+
      getUsed+'</td></tr></table>';
  end;

PROCEDURE T_userPackageDocumentation.addRuleDoc(CONST htmlDoc:ansistring);
  begin
    setLength(rulesDoc,length(rulesDoc)+1);
    rulesDoc[length(rulesDoc)-1]:=htmlDoc;
  end;

FUNCTION T_userPackageDocumentation.getHref: ansistring;
  begin
    result:='<a href="#'+docFileName+'"><code>'+id+'</code></a>';
  end;

CONSTRUCTOR T_intrinsicFunctionDocumentation.create(CONST funcName: ansistring);
  begin
    id:=funcName;
    description:=intrinsicRuleExplanationMap.get(id);
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
        if pos(';', lines [i])>0 then result:=result+'<code>'+toHtmlCode(lines [i])+'</code>'
        else result:=result+lines [i];
      end;
    end;
  VAR i:longint;
  begin
    result:='<h4><a name="'+id+'">'+id+'</a></h4>'+prettyHtml(description);
    if length(example)>0 then begin
      result:=result+'<br>Examples:';
      for i:=0 to length(example)-1 do result:=result+'<br><code>'+example[i]+'</code>';
    end;

  end;

VAR builtInDoc: array[T_namespace] of array of T_intrinsicFunctionDocumentation;
    builtInDocsReady:boolean=false;
    builtInDocsEnhanced:boolean=false;

FUNCTION namespace(CONST id:ansistring):T_namespace;
  VAR useId:ansistring;
      n:T_namespace;
  begin
    if isQualified(id) then useId:=split(id,'.')[0] else useId:=id;
    for n:=low(T_namespace) to high(T_namespace) do if C_namespaceString[n]=useId then exit(n);
  end;

FUNCTION shortName(CONST id:ansistring):ansistring;
  begin
    if isQualified(id) then result:=split(id,'.')[1] else result:=id;
  end;

PROCEDURE prepareBuiltInDocs;
  VAR ids: T_arrayOfString;
      i,j: longint;
      n: T_namespace;
      swapTmp: T_intrinsicFunctionDocumentation;

  begin
    if builtInDocsReady then exit;
    //Prepare and sort data:-------------------------------------------------------------
    for n:=low(T_namespace) to high(T_namespace) do setLength(builtInDoc[n],0);
    ids:=intrinsicRuleExplanationMap.keySet;
    for i:=0 to length(ids)-1 do if isQualified(ids[i]) then begin
      n:=namespace(ids[i]);
      j:=length(builtInDoc[n]);
      setLength(builtInDoc[n],j+1);
      builtInDoc[n][j].create(ids[i]);
    end;
    setLength(ids,0);

    for n:=low(T_namespace) to high(T_namespace) do
    for i:=1 to length(builtInDoc[n])-1 do for j:=0 to i-1 do
    if builtInDoc[n][i].id < builtInDoc[n][j].id then begin
      swapTmp:=builtInDoc[n][i]; builtInDoc[n][i]:=builtInDoc[n][j]; builtInDoc[n][j]:=swapTmp;
    end;
    //-------------------------------------------------------------:Prepare and sort data
    builtInDocsReady:=true;
  end;

FUNCTION getBuiltInDocTxt(CONST id:ansistring):ansistring;
  VAR n:T_namespace;
      i:longint;
  begin
    prepareBuiltInDocs;
    if not(builtInDocsEnhanced) then makeHtmlFromTemplate(true);
    result:=HTML_FILE_START;
    if isQualified(id) then begin
      n:=namespace(id);
      for i:=0 to length(builtInDoc[n])-1 do if builtInDoc[n][i].id=id then result:=result+builtInDoc[n][i].getHtml;
    end else for n:=low(T_namespace) to high(T_namespace) do
    for i:=0 to length(builtInDoc[n])-1 do
    if shortName(builtInDoc[n][i].id)=id then result:=result+builtInDoc[n][i].getHtml;
    result:=result+LineEnding+HTML_FILE_END;
  end;

PROCEDURE writeBuiltInDoc(CONST id:ansistring);
  VAR outFile:text;
  begin

    assign(outFile,htmlRoot+'\_tmp_doc_.html');
    rewrite(outFile);
    close(outFile);
  end;

PROCEDURE finalizeBuiltInDocs;
  VAR i:longint;
      n: T_namespace;
  begin
    for n:=low(T_namespace) to high(T_namespace) do begin
      for i:=0 to length(builtInDoc[n])-1 do builtInDoc[n][i].destroy;
      setLength(builtInDoc[n],0);
    end;
    builtInDocsReady:=false;
  end;

PROCEDURE makeHtmlFromTemplate(CONST prepareOnly:boolean);
  PROCEDURE writeUserPackageDocumentations(VAR outFile:text);
    VAR i: longint;
    begin
      for i:=0 to length(packages)-1 do packages[i]^.resolveUses;
      write(outFile, '<table>');
      for i:=0 to length(packages)-1 do
        begin
        if odd(i) then write(outFile, '<tr>')
        else write(outFile, '<tr class="ruleHead">');
        write(outFile, '<td>', packages [i]^.getHref, '</td><td><a href="file:///'+replaceAll(packages [i]^.uid,'\','/')+'"><code>'+packages [i]^.uid+'</code></a></td>');
        if packages [i]^.isExecutable then
          write(outFile, '<td>executable</td>') else write(outFile, '<td>&nbsp;</td>');
        writeln(outFile, '</tr>');
        end;
      write(outFile, '</table>');

      for i:=0 to length(packages)-1 do writeln(outFile, packages [i]^.toHtml);
      for i:=0 to length(packages)-1 do dispose(packages [i], destroy);
      setLength(packages, 0);
    end;

  PROCEDURE documentBuiltIns(VAR outFile:text);
    VAR i: longint;
        n: T_namespace;
    begin
      writeln(outFile, '<div align="right"><hr></div><br><div>');
      for n:=low(T_namespace) to high(T_namespace) do for i:=0 to length(builtInDoc[n])-1 do
        writeln(outFile, '<a href="#', builtInDoc[n][i].id, '">', builtInDoc[n][i].id, '</a> &nbsp; ');
      writeln(outFile, '</div><br><div align="right"><hr></div>');
      for n:=low(T_namespace) to high(T_namespace) do
        writeln(outFile,'<h4><a href="#'+C_namespaceString[n]+'">'+C_namespaceString[n]+'</a></h4>');

      for n:=low(T_namespace) to high(T_namespace) do begin
        writeln(outFile,'<div align="right"><hr></div><h3><a name="'+C_namespaceString[n]+'">'+C_namespaceString[n]+'<a></h3>');
        for i:=0 to length(builtInDoc[n])-1 do write(outFile,builtInDoc[n][i].getHtml);
      end;
    end;

  CONST TEMPLATE_NAME_SUFFIX='\html_template.txt';
  TYPE T_include=record
         includeTag:ansistring;
         content:T_arrayOfString;
       end;

  VAR templateFile:text;
      txt:ansistring;
      outFile:record
        handle:text;
        isOpen:boolean;
      end;
      includes:array of T_include;
      context:record
        mode:(none,beautifying,definingInclude,definingExample);
        include:T_include;
      end;

  FUNCTION contextEnds(CONST txt:ansistring) :boolean;
    CONST END_CONTEXT_CMD='<!--end-->';
    VAR ns:T_namespace;
        i:longint;
    begin
      if trim(txt)=END_CONTEXT_CMD then begin
        if context.mode = definingInclude then begin
          setLength(includes,length(includes)+1);
          includes[length(includes)-1]:=context.include;
        end else if context.mode = definingExample then begin
          for ns:=low(T_namespace) to high(T_namespace) do
          for i:=0 to length(builtInDoc[ns])-1 do
            if   builtInDoc[ns][i].id      =context.include.includeTag
            then builtInDoc[ns][i].example:=context.include.content;
        end;
        context.mode:=none;
        exit(true);
        result:=true;
      end else result:=false;
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
          START_EXAMPLE_PREFIX='<!--example ';
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
          assign(handle,htmlRoot+'\'+cmdParam);
          if not(prepareOnly) then begin
            rewrite(handle);
            isOpen:=true;
          end;
        end;
        exit(true);
      end;
      if cmd=BUILTIN_DOC_CMD then begin
        if not(prepareOnly) then documentBuiltIns(outFile.handle);
        exit(true);
      end;
      if cmd=PACKAGE_DOC_CMD then begin
        if not(prepareOnly) then writeUserPackageDocumentations(outFile.handle);
        exit(true);
      end;
      if cmd=START_BEAUTIFY_CMD then begin
        context.mode:=beautifying;
        exit(true);
      end;
      if startsWith(cmd,START_INCLUDE_PREFIX) then begin
        context.mode:=definingInclude;
        context.include.includeTag:='<!--'+cmdParam+'-->';
        setLength(context.include.content,0);
        exit(true);
      end;
      if startsWith(cmd,START_EXAMPLE_PREFIX) then begin
        context.mode:=definingExample;
        context.include.includeTag:=cmdParam;
        setLength(context.include.content,0);
        exit(true);
      end;
      for i:=0 to length(includes)-1 do if includes[i].includeTag=cmd then begin
        with outFile do if isOpen and not(prepareOnly) then for j:=0 to length(includes[i].content)-1 do writeln(handle,includes[i].content[j]);
        exit(true);
      end;
      result:=false;
    end;

  begin
    prepareBuiltInDocs;
    outFile.isOpen:=false;
    setLength(includes,0);
    context.mode:=none;

    assign(templateFile, htmlRoot+TEMPLATE_NAME_SUFFIX);
    reset(templateFile);
    while not(eof(templateFile)) do begin
      readln(templateFile, txt);
      case context.mode of
        none:            if not(handleCommand(txt)) and outFile.isOpen then writeln(outFile.handle,txt);
        beautifying:     if not(contextEnds(txt))   and outFile.isOpen then writeln(outFile.handle,toHtmlCode(txt));
        definingInclude: if not(contextEnds(txt))   then append(context.include.content,txt);
        definingExample: if not(contextEnds(txt))   then append(context.include.content,toHtmlCode(txt))
      end;
    end;
    close(templateFile);
    with outFile do if isOpen then close(handle);
    builtInDocsEnhanced:=true;
  end;


PROCEDURE locateHtml;
  CONST primary = 'doc';
    VAR secondary:string;
  begin
    secondary:=extractFilePath(paramStr(0))+DirectorySeparator+primary;
    if DirectoryExists(primary) then htmlRoot:=primary
    else if DirectoryExists(secondary) then htmlRoot:=secondary
    else htmlRoot:='';
  end;

INITIALIZATION
  locateHtml;
FINALIZATION
  finalizeBuiltInDocs;
end.
