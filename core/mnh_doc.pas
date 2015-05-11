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

  { T_userFunctionDocumentation }
  T_userFunctionDocumentation = object
    isMemoized: boolean;
    id: ansistring;
    subrules: array of record
      isPrivate,isSynchronized,isMutable: boolean;
      pattern, comment: ansistring;
    end;
    CONSTRUCTOR create(ruleId: ansistring);
    DESTRUCTOR destroy;
    PROCEDURE addSubRule(privateSubrule,sync,mut: boolean; pat, comment: ansistring);
    FUNCTION toHtml: ansistring;
    FUNCTION printHelpText(sourceName:ansistring):boolean;
  end;

  P_userPackageDocumentation = ^T_userPackageDocumentation;

  { T_userPackageDocumentation }

  T_userPackageDocumentation = object
    lastComment: ansistring;

    uid: ansistring;
    id: ansistring;
    docFileName: ansistring;
    rawUses: T_arrayOfString;
    usesPackage, usedByPackage: array of P_userPackageDocumentation;
    rules: array of T_userFunctionDocumentation;
    CONSTRUCTOR create(path, Name: ansistring);
    DESTRUCTOR destroy;
    PROCEDURE addUses(OtherUid: ansistring);
    PROCEDURE addUsed(other: P_userPackageDocumentation);
    PROCEDURE resolveUses;
    FUNCTION toHtml: ansistring;
    PROCEDURE addComment(CONST s: ansistring);
    PROCEDURE addSubRule(CONST subruleId, pattern: ansistring; CONST isMemoized, isPrivate, isSynchronized, isMutable: boolean);
    FUNCTION getHref: ansistring;
    FUNCTION isExecutable: boolean;
    PROCEDURE printHelpText;
  end;

PROCEDURE writeUserPackageDocumentations;
PROCEDURE writeUserPackageHelpText;
PROCEDURE documentBuiltIns;
IMPLEMENTATION
VAR packages: array of P_userPackageDocumentation;

PROCEDURE locateHtml;
  CONST primary = 'doc';
    secondary = '..'+DirectorySeparator+'doc';
  begin
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
    lastComment:='';
    id:=Name;
    uid:=ExpandFileName(path);
    docFileName:=replaceAll(replaceAll(replaceAll(replaceAll(uid, ':', '_'), '\', '_'),
      '/', '_'), '.', '_');
    setLength(rawUses, 0);
    setLength(usedByPackage, 0);
    setLength(usedByPackage, 0);
    setLength(rules, 0);
    setLength(packages, length(packages)+1);
    packages[length(packages)-1]:=@self;
  end;

DESTRUCTOR T_userPackageDocumentation.destroy;
  VAR i: longint;
  begin
    setLength(rawUses, 0);
    setLength(usedByPackage, 0);
    setLength(usedByPackage, 0);
    for i:=0 to length(rules)-1 do rules[i].destroy;
    setLength(rules, 0);
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
    for i:=0 to length(rules)-1 do result:=result+rules [i].toHtml;
    result:=result+'</td></tr>'+'<tr class="oben"><td></ul>Used by: </td><td>'+
      getUsed+'</td></tr></table>';
  end;

PROCEDURE T_userPackageDocumentation.addComment(CONST s: ansistring);
  begin
    if lastComment = '' then lastComment:=s
    else lastComment:=lastComment+' '+s;
  end;

PROCEDURE T_userPackageDocumentation.addSubRule(CONST subruleId, pattern: ansistring; CONST isMemoized, isPrivate, isSynchronized, isMutable:boolean);
  VAR ruleIdx: longint;
  begin
    if isPrivate then begin
      lastComment:='';
      exit;
    end;
    ruleIdx:=0;
    while (ruleIdx<length(rules)) and (rules [ruleIdx].id<>subruleId) do Inc(ruleIdx);
    if ruleIdx>=length(rules) then begin
      setLength(rules, ruleIdx+1);
      rules[ruleIdx].create(subruleId);
    end;
    if isMemoized then rules[ruleIdx].isMemoized:=true;
    rules[ruleIdx].addSubRule(isPrivate or isMutable,isSynchronized, isMutable, pattern, lastComment);
    lastComment:='';
  end;

FUNCTION T_userPackageDocumentation.getHref: ansistring;
  begin
    result:='<a href="#'+docFileName+'"><code>'+id+'</code></a>';
  end;

FUNCTION T_userPackageDocumentation.isExecutable: boolean;
  VAR i: longint;
  begin
    result:=false;
    for i:=0 to length(rules)-1 do if rules [i].id = 'main' then exit(true);
  end;

PROCEDURE T_userPackageDocumentation.printHelpText;
  VAR i:longint;
  begin
    for i:=0 to length(rules)-1 do rules[i].printHelpText(uid);
  end;

{ T_userFunctionDocumentation }

CONSTRUCTOR T_userFunctionDocumentation.create(ruleId: ansistring);
  begin
    id:=ruleId;
    setLength(subrules, 0);
  end;

DESTRUCTOR T_userFunctionDocumentation.destroy;
  begin
    setLength(subrules, 0);
  end;

PROCEDURE T_userFunctionDocumentation.addSubRule(privateSubrule,sync,mut: boolean; pat, comment: ansistring);
  begin
    setLength(subrules, length(subrules)+1);
    subrules[length(subrules)-1].isPrivate:=privateSubrule;
    subrules[length(subrules)-1].isSynchronized:=sync;
    subrules[length(subrules)-1].isMutable:=mut;
    subrules[length(subrules)-1].pattern:=pat;
    subrules[length(subrules)-1].comment:=comment;
  end;

FUNCTION T_userFunctionDocumentation.toHtml: ansistring;
  VAR i: longint;
      anyPublic:boolean=false;
  begin
    result:='<table><tr class="ruleHead"><td>';
    if isMemoized then result:=result+'<i>memoized </i>';
    if id = 'main' then result:=result+'<b>'+id+'</b>'
    else result:=result+id;
    result:=result+'</td></tr>';
    for i:=0 to length(subrules)-1 do with subrules [i] do begin
      if not(isPrivate) then anyPublic:=true;
      result:=result+'<tr><td>';
      if comment<>'' then result:=result+'<i>'+comment+'</i><br>';
      result:=result+'<code>';
      if isPrivate then result:=result+'<i>private </i>';
      if isMutable then result:=result+'<i>mutable </i>';
      if isSynchronized then result:=result+'<i>synchronized </i>';
      result:=result+id+pattern+'</code></td></tr>';
    end;
    result:=result+'</table>';
    if not(anyPublic) then result:='';
  end;

FUNCTION T_userFunctionDocumentation.printHelpText(sourceName:ansistring):boolean;
  FUNCTION noHtml(s:string):string;
    FUNCTION removeTag(s:string; tag:string):string;
      begin result:=replaceAll(replaceAll(s,'<'+tag+'>',''),'</'+tag+'>',''); end;
    begin
      result:=removeTag(s,'code');
      result:=removeTag(result,'i');
      result:=removeTag(result,'b');
      result:=removeTag(result,'u');
      result:=replaceAll(result,'</a>','');
      result:=replaceAll(result,'<a href="','');
      result:=replaceAll(result,'">',' ');
    end;
  VAR i:longint;
  begin
    if id<>'main' then exit(false);
    writeln('main functions in ',sourceName);
    for i:=0 to length(subrules)-1 do with subrules[i] do begin
      if comment<>'' then writeln('  ',noHtml(comment),':');
      writeln('    ',id,pattern);
    end;
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

PROCEDURE writeUserPackageHelpText;
  VAR i:longint;
  begin
    writeln;
    for i:=0 to length(packages)-1 do begin
      packages[i]^.printHelpText;
      dispose(packages[i],destroy);
    end;
    setLength(packages,0);
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
    i, j: longint;
    tmp: ansistring;
    doc: T_intrinsicFunctionDocumentation;

    inFile, outFile: Text;
  begin
    if not(FileExists(htmlRoot+htmlHead)) or not(FileExists(htmlRoot+htmlFoot)) then
      exit;

    ids:=intrinsicRuleExplanationMap.keySet;
    j:=0;
    for i:=0 to length(ids)-1 do if isQualified(ids[i]) then begin
      ids[j]:=ids[i]; inc(j);
    end;
    setLength(ids,j);

    for i:=1 to length(ids)-1 do for j:=0 to i-1 do
    if ids [i]<ids [j] then begin
      tmp:=ids [i]; ids[i]:=ids [j]; ids[j]:=tmp;
    end;

    assign(outFile, htmlRoot+htmlResult); rewrite(outfile);

    assign(inFile, htmlRoot+htmlHead); reset(inFile);
    while not(EOF(inFile)) do
      begin
      readln(inFile, tmp);
      writeln(outFile, tmp);
      end;
    close(inFile);

    writeln(outFile, '<br><div>');
    for i:=0 to length(ids)-1 do
      writeln(outFile, '<a href="#', ids [i], '">', ids [i], '</a> &nbsp; ');
    writeln(outFile, '</div>');

    for i:=0 to length(ids)-1 do
      begin
      doc.create(ids [i]);
      writeln(outFile, doc.toHtml);
      doc.destroy;
      end;

    assign(inFile, htmlRoot+htmlFoot); reset(inFile);
    while not(EOF(inFile)) do
      begin
      readln(inFile, tmp);
      writeln(outFile, tmp);
      end;
    close(inFile);
    close(outFile);

  end;

INITIALIZATION
  locateHtml;

end.
