UNIT mnh_doc;
INTERFACE
USES SysUtils, mnh_funcs, myStringutil, myGenerics, mnh_constants;
VAR htmlRoot: string;
TYPE

  { T_intrinsicFunctionDocumentation }

  T_intrinsicFunctionDocumentation = object
    id, description: ansistring;
    CONSTRUCTOR create(CONST funcName: ansistring);
    DESTRUCTOR destroy;
    FUNCTION toHtml: string;
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
    CONSTRUCTOR create(path, Name: ansistring);
    DESTRUCTOR destroy;
    PROCEDURE addUses(OtherUid: ansistring);
    PROCEDURE addUsed(other: P_userPackageDocumentation);
    PROCEDURE resolveUses;
    FUNCTION toHtml: ansistring;
    PROCEDURE addRuleDoc(CONST htmlDoc:ansistring);
    FUNCTION getHref: ansistring;
  end;

PROCEDURE writeUserPackageDocumentations;
PROCEDURE documentBuiltIns;
PROCEDURE addPackageDoc(CONST doc:P_userPackageDocumentation);
IMPLEMENTATION
VAR packages: array of P_userPackageDocumentation;

PROCEDURE addPackageDoc(CONST doc:P_userPackageDocumentation);
  begin
    setLength(packages,length(packages)+1);
    packages[length(packages)-1]:=doc;
  end;

PROCEDURE locateHtml;
  CONST primary = 'doc';
    VAR secondary:string;
  begin
    secondary:=ExtractFilePath(paramstr(0))+DirectorySeparator+primary;
    if DirectoryExists(primary) then htmlRoot:=primary
    else if DirectoryExists(secondary) then htmlRoot:=secondary
    else htmlRoot:='';
  end;

FUNCTION prettyHtml(s: ansistring): ansistring;
  VAR Lines: T_arrayOfString;
    i: longint;
  begin
    setLength(Lines, 0);
    while pos('#', s)>0 do begin
      append(Lines, copy(s, 1, pos('#', s)-1));
      s:=copy(s, pos('#', s)+1, length(s));
    end;
    append(Lines,s);

    result:='';
    for i:=0 to length(Lines)-1 do
      begin
      if i>0 then result:=result+'<br>';
      if pos(';', Lines [i])>0 then result:=result+'<code>'+Lines [i]+'</code>'
      else result:=result+Lines [i];
      end;
  end;

{ T_userPackageDocumentation }

CONSTRUCTOR T_userPackageDocumentation.create(path, Name: ansistring);
  begin
    isExecutable:=false;
    id:=Name;
    uid:=ExpandFileName(path);
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
  end;

DESTRUCTOR T_intrinsicFunctionDocumentation.destroy;
  begin
    id:='';
    description:='';
  end;

FUNCTION T_intrinsicFunctionDocumentation.toHtml: string;
  begin
    result:='<h4><a name="'+id+'">'+id+'</a></h4>'+prettyHtml(description);
  end;

PROCEDURE writeUserPackageDocumentations;
  CONST htmlHead = '\packages.head';
    htmlFoot = '\packages.foot';
    htmlResult = '\packages.html';

  VAR i: longint;
    tmp: ansistring;
    inFile, outFile: Text;
  begin
    for i:=0 to length(packages)-1 do packages[i]^.resolveUses;
    assign(outFile, htmlRoot+htmlResult); rewrite(outfile);

    assign(inFile, htmlRoot+htmlHead); reset(inFile);
    while not(EOF(inFile)) do
      begin
      readln(inFile, tmp);
      writeln(outFile, tmp);
      end;
    close(inFile);

    Write(outfile, '<table>');
    for i:=0 to length(packages)-1 do
      begin
      if odd(i) then Write(outfile, '<tr>')
      else Write(outfile, '<tr class="ruleHead">');
      Write(outfile, '<td>', packages [i]^.getHref, '</td><td><a href="file:///'+replaceAll(packages [i]^.uid,'\','/')+'"><code>'+packages [i]^.uid+'</code></a></td>');
      if packages [i]^.isExecutable then
        Write(outfile, '<td>executable</td>') else Write(outfile, '<td>&nbsp;</td>');
      writeln(outfile, '</tr>');
      end;
    Write(outfile, '</table>');

    for i:=0 to length(packages)-1 do writeln(outFile, packages [i]^.toHtml);
    for i:=0 to length(packages)-1 do dispose(packages [i], destroy);
    setLength(packages, 0);

    assign(inFile, htmlRoot+htmlFoot); reset(inFile);
    while not(EOF(inFile)) do
      begin
      readln(inFile, tmp);
      writeln(outFile, tmp);
      end;
    close(inFile);
    close(outFile);

  end;

PROCEDURE documentBuiltIns;
  CONST htmlHead = '\builtin.head';
    htmlFoot = '\builtin.foot';
    htmlResult = '\builtin.html';

  VAR ids: T_arrayOfString;
      i,j: longint;
      n: T_namespace;
      tmp: ansistring;
      swapTmp: T_intrinsicFunctionDocumentation;

      doc: array[T_namespace] of array of T_intrinsicFunctionDocumentation;
      inFile, outFile: Text;

  FUNCTION namespace(CONST id:ansistring):T_namespace;
    VAR useId:ansistring;
        n:T_namespace;
    begin
      if isQualified(id) then useId:=split(id,'.')[0] else useId:=id;
      for n:=low(T_namespace) to high(T_namespace) do if C_namespaceString[n]=useId then exit(n);
    end;

  begin
    if not(FileExists(htmlRoot+htmlHead)) or not(FileExists(htmlRoot+htmlFoot)) then exit;
    //Prepare and sort data:-------------------------------------------------------------
    for n:=Low(T_namespace) to High(T_namespace) do setLength(doc[n],0);
    ids:=intrinsicRuleExplanationMap.keySet;
    for i:=0 to length(ids)-1 do if isQualified(ids[i]) then begin
      n:=namespace(ids[i]);
      j:=length(doc[n]);
      setLength(doc[n],j+1);
      doc[n][j].create(ids[i]);
    end;
    setLength(ids,0);

    for n:=Low(T_namespace) to High(T_namespace) do
    for i:=1 to length(doc[n])-1 do for j:=0 to i-1 do
    if doc[n][i].id < doc[n][j].id then begin
      swapTmp:=doc[n][i]; doc[n][i]:=doc[n][j]; doc[n][j]:=swapTmp;
    end;
    //-------------------------------------------------------------:Prepare and sort data
    //Write Header:----------------------------------------------------------------------
    assign(outFile, htmlRoot+htmlResult); rewrite(outfile);
    assign(inFile, htmlRoot+htmlHead); reset(inFile);
    while not(EOF(inFile)) do
      begin
      readln(inFile, tmp);
      writeln(outFile, tmp);
      end;
    close(inFile);
    //----------------------------------------------------------------------:Write Header
    writeln(outFile, '<div align="right"><hr></div><br><div>');
    for n:=Low(T_namespace) to High(T_namespace) do for i:=0 to length(doc[n])-1 do
      writeln(outFile, '<a href="#', doc[n][i].id, '">', doc[n][i].id, '</a> &nbsp; ');
    writeln(outFile, '</div><br><div align="right"><hr></div>');
    for n:=Low(T_namespace) to High(T_namespace) do
      writeln(outFile,'<h4><a href="#'+C_namespaceString[n]+'">'+C_namespaceString[n]+'</a></h4>');


    for n:=Low(T_namespace) to High(T_namespace) do begin
      writeln(outFile,'<div align="right"><hr></div><h3><a name="'+C_namespaceString[n]+'">'+C_namespaceString[n]+'<a></h3>');
      for i:=0 to length(doc[n])-1 do begin
        writeln(outFile, doc[n][i].toHtml);
        doc[n][i].destroy;
      end;
      setLength(doc[n],0);

    end;
    //Write Footer:----------------------------------------------------------------------
    assign(inFile, htmlRoot+htmlFoot); reset(inFile);
    while not(EOF(inFile)) do
      begin
      readln(inFile, tmp);
      writeln(outFile, tmp);
      end;
    close(inFile);
    close(outFile);
    //----------------------------------------------------------------------:Write Footer
  end;

INITIALIZATION
  locateHtml;

end.
