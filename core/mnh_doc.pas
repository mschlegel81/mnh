UNIT mnh_doc;
INTERFACE
USES sysutils, mnh_funcs, mnh_stringutil,myGenerics;
VAR htmlRoot:string;
TYPE

  { T_intrinsicFunctionDocumentation }

  T_intrinsicFunctionDocumentation=object
    id, description:ansistring;
    CONSTRUCTOR create(CONST funcName:ansistring);
    DESTRUCTOR destroy;
    FUNCTION toHtml:string;
  end;

  { T_userFunctionDocumentation }
  T_userFunctionDocumentation=object
    isPure:boolean;
    id:ansistring;
    subrules:array of record
      isPrivate:boolean;
      pattern,comment:ansistring;
    end;
    CONSTRUCTOR create(ruleId:ansistring);
    DESTRUCTOR destroy;
    PROCEDURE addSubRule(privateSubrule:boolean; pat,comment:ansistring);
    FUNCTION toHtml:ansistring;    
  end;

  P_userPackageDocumentation=^T_userPackageDocumentation;

  { T_userPackageDocumentation }

  T_userPackageDocumentation=object
    lastComment:ansistring;

    uid:ansistring;
    id:ansistring;
    docFileName:ansistring;
    rawUses:array of ansistring;
    usesPackage,usedByPackage:array of P_userPackageDocumentation;
    rules:array of T_userFunctionDocumentation;
    CONSTRUCTOR create(path,name:ansistring);
    DESTRUCTOR destroy;
    PROCEDURE addUses(OtherUid:ansistring);
    PROCEDURE addUsed(other:P_userPackageDocumentation);
    PROCEDURE resolveUses;
    FUNCTION toHtml:ansistring;
    PROCEDURE addComment(CONST s:ansistring);
    PROCEDURE addSubRule(CONST subruleId,pattern:ansistring; CONST isPure,isPrivate:boolean);
    FUNCTION getHref:ansistring;
    FUNCTION isExecutable:boolean;
  end;

PROCEDURE writeUserPackageDocumentations;
PROCEDURE documentBuiltIns;
IMPLEMENTATION
VAR packages:array of P_userPackageDocumentation;

PROCEDURE locateHtml;
  CONST primary='doc';
        secondary='..'+DirectorySeparator+'doc';
  begin
    if DirectoryExists(primary) then htmlRoot:=primary
    else if DirectoryExists(secondary) then htmlRoot:=secondary
    else htmlRoot:='';
  end;

FUNCTION prettyHtml(s:ansistring):ansistring;
  VAR lines:array of ansistring;  
      i:longint;
  begin
    setLength(lines,0);
    while pos('#',s)>0 do begin
      setLength(lines,length(lines)+1);
      lines[length(lines)-1]:=copy(s,1,pos('#',s)-1);
      s:=copy(s,pos('#',s)+1,length(s));
    end;
    setLength(lines,length(lines)+1);
    lines[length(lines)-1]:=s;
  
    result:='';
    for i:=0 to length(lines)-1 do begin
      if i>0 then result:=result+'<br>';
      if pos(';',lines[i])>0 then result:=result+'<code>'+lines[i]+'</code>' 
                             else result:=result+         lines[i];      
    end;
  end;

{ T_userPackageDocumentation }

constructor T_userPackageDocumentation.create(path, name: ansistring);
begin
  lastComment:='';
  id:=name;
  uid:=ExpandFileName(path);
  docFileName:=replaceAll(replaceAll(replaceAll(replaceAll(uid,':','_'),'\','_'),'/','_'),'.','_');
  setLength(rawUses,0);
  setLength(usedByPackage,0);
  SetLength(usedByPackage,0);
  SetLength(rules,0);
  setLength(packages,length(packages)+1);
  packages[length(packages)-1]:=@self;
end;

destructor T_userPackageDocumentation.destroy;
VAR i:longint;
begin
  setLength(rawUses,0);
  setLength(usedByPackage,0);
  SetLength(usedByPackage,0);
  for i:=0 to length(rules)-1 do rules[i].destroy;
  setLength(rules,0);
end;

procedure T_userPackageDocumentation.addUses(OtherUid: ansistring);
begin
  setLength(rawUses,length(rawUses)+1);
  rawUses[length(rawUses)-1]:=OtherUid;
end;

procedure T_userPackageDocumentation.addUsed(other:P_userPackageDocumentation);
  begin
    setLength(usedByPackage,length(usedByPackage)+1);
    usedByPackage[length(usedByPackage)-1]:=other;
  end;

procedure T_userPackageDocumentation.resolveUses;
  FUNCTION packageByPath(CONST path:ansistring):P_userPackageDocumentation;
    VAR i:longint;
    begin
      for i:=0 to length(packages)-1 do if packages[i]^.uid=path then exit(packages[i]);
      result:=nil;
    end;

  VAR other:P_userPackageDocumentation;
      i:longint;
  begin
    setLength(usesPackage,0);
    for i:=0 to length(rawUses)-1 do begin
      other:=packageByPath(rawUses[i]);
      if other<>nil then begin
        setLength(usesPackage,length(usesPackage)+1);
        usesPackage[length(usesPackage)-1]:=other;
        other^.addUsed(@self);
      end;
    end;
    setLength(rawUses,0);
  end;

function T_userPackageDocumentation.toHtml: ansistring;
  FUNCTION getUses:ansistring;
    VAR i:longint;
    begin
      if length(usesPackage)=0 then exit('none');
      result:='';
      for i:=0 to length(usesPackage)-1 do begin
        if result<>'' then result:=result+', ';
        result:=result+usesPackage[i]^.getHref;
      end;
    end;

  FUNCTION getUsed:ansistring;
    VAR i:longint;
    begin
      if length(usedByPackage)=0 then exit('none');
      result:='';
      for i:=0 to length(usedByPackage)-1 do begin
        if result<>'' then result:=result+', ';
        result:=result+usedByPackage[i]^.getHref;
      end;
    end;

  VAR i:longint;

  begin
    result:='<h4><a name="'+docFileName+'">'+id+'</a></h4><table class="oben">'+
    '<tr class="oben"><td>Path: </td><td><code>'+uid+'</code></td></tr>'+
    '<tr class="oben"><td>Uses: </td><td>'+getUses+'</td></tr>'+
    '<tr class="oben"><td>Publishes: </td><td>';
    for i:=0 to length(rules)-1 do result:=result+rules[i].toHtml;
    result:=result+'</td></tr>'+
      '<tr class="oben"><td></ul>Used by: </td><td>'+getUsed+'</td></tr></table>';
  end;

procedure T_userPackageDocumentation.addComment(const s: ansistring);
begin
  if lastComment=''
  then lastComment:=s
  else lastComment:=lastComment+' '+s;
end;

procedure T_userPackageDocumentation.addSubRule(const subruleId, pattern: ansistring; const isPure, isPrivate: boolean);
  VAR ruleIdx:longint;
  begin
    if isPrivate then begin
      lastComment:='';
      exit;
    end;
    ruleIdx:=0;
    while (ruleIdx<length(rules)) and (rules[ruleIdx].id<>subruleId) do inc(ruleIdx);
    if ruleIdx>=length(rules) then begin
      setLength(rules,ruleIdx+1);
      rules[ruleIdx].create(subruleId);
    end;
    if isPure then rules[ruleIdx].isPure:=true;
    rules[ruleIdx].addSubRule(isPrivate,pattern,lastComment);
    lastComment:='';
  end;

function T_userPackageDocumentation.getHref: ansistring;
begin
  result:='<a href="#'+docFileName+'"><code>'+id+'</code></a>';
end;

function T_userPackageDocumentation.isExecutable: boolean;
VAR i:longint;
begin
  result:=false;
  for i:=0 to length(rules)-1 do if rules[i].id='main' then exit(true);
end;

{ T_userFunctionDocumentation }

constructor T_userFunctionDocumentation.create(ruleId: ansistring);
begin
  id:=ruleId;
  setLength(subrules,0);
end;

destructor T_userFunctionDocumentation.destroy;
begin
  setLength(subrules,0);
end;

procedure T_userFunctionDocumentation.addSubRule(privateSubrule: boolean; pat,comment: ansistring);
begin
  setLength(subrules,length(subrules)+1);
  subrules[length(subrules)-1].isPrivate:=privateSubrule;
  subrules[length(subrules)-1].pattern:=pat;
  subrules[length(subrules)-1].comment:=comment;
end;

function T_userFunctionDocumentation.toHtml: ansistring;
VAR i:longint;
    anyPublic:boolean=false;
begin


  result:='<table><tr class="ruleHead"><td>';
  if isPure then result:=result+'<div class="red">';
  if id='main' then result:=result+'<b>'+id+'</b>'
               else result:=result+      id;
  if isPure then result:=result+' (pure)</div>';

  result:=result+'</td></tr>';
  for i:=0 to length(subrules)-1 do with subrules[i] do if not(isPrivate) then begin
    result:=result+'<tr><td>';
    if comment<>'' then result:=result+'<i>'+ comment+'</i><br>';
    result:=result+'<code>'+id+pattern+'</code></td></tr>';
    anyPublic:=true;
  end;
  result:=result+'</table>';

end;

constructor T_intrinsicFunctionDocumentation.create(const funcName: ansistring);
  begin
    id:=funcName;
    description:=intrinsicRuleExplanationMap.get(id);
  end;

destructor T_intrinsicFunctionDocumentation.destroy;
  begin
    id:='';
    description:='';
  end;

function T_intrinsicFunctionDocumentation.toHtml: string;
  begin  
    result:='<h4><a name="'+id+'">'+id+'</a></h4>'+prettyHtml(description);
  end;

procedure writeUserPackageDocumentations;
  CONST htmlHead='\packages.head';
        htmlFoot='\packages.foot';
        htmlResult='\packages.html';

  VAR i:longint;
      tmp:ansistring;
      inFile,outFile:text;
begin
  for i:=0 to length(packages)-1 do packages[i]^.resolveUses;
  assign(outFile,htmlRoot+htmlResult); rewrite(outfile);

  assign(inFile,htmlRoot+htmlHead); reset(inFile);
  while not(eof(inFile)) do begin
    readln(inFile,tmp);
    writeln(outFile,tmp);
  end;
  Close(inFile);

  write(outfile,'<table>');
  for i:=0 to length(packages)-1 do begin
    if odd(i) then write(outfile,'<tr>')
              else write(outfile,'<tr class="ruleHead">');
    write(outfile,'<td>',packages[i]^.getHref,'</td><td>',packages[i]^.uid,'</td>');
    if packages[i]^.isExecutable then write(outfile,'<td>executable</td>') else write(outfile,'<td>&nbsp;</td>');
    writeln(outfile,'</tr>');
  end;
    write(outfile,'</table>');

  for i:=0 to length(packages)-1 do writeln(outFile,packages[i]^.toHtml);
  for i:=0 to length(packages)-1 do dispose(packages[i],destroy);
  setLength(packages,0);

  assign(inFile,htmlRoot+htmlFoot); reset(inFile);
  while not(eof(inFile)) do begin
    readln(inFile,tmp);
    writeln(outFile,tmp);
  end;
  Close(inFile);
  close(outFile);

end;

PROCEDURE documentBuiltIns;
  CONST htmlHead='\builtin.head';
        htmlFoot='\builtin.foot';
        htmlResult='\builtin.html';

  VAR ids:T_arrayOfString;
      i,j:longint;
      tmp:ansistring;
      doc:T_intrinsicFunctionDocumentation;

      inFile,outFile:text;
  begin
    if not(FileExists(htmlRoot+htmlHead)) or not(FileExists(htmlRoot+htmlFoot)) then exit;

    ids:=intrinsicRuleExplanationMap.keySet;
    for i:=1 to length(ids)-1 do for j:=0 to i-1 do if ids[i]<ids[j] then begin
      tmp:=ids[i]; ids[i]:=ids[j]; ids[j]:=tmp;
    end;

    assign(outFile,htmlRoot+htmlResult); rewrite(outfile);

    assign(inFile,htmlRoot+htmlHead); reset(inFile);
    while not(eof(inFile)) do begin
      readln(inFile,tmp);
      writeln(outFile,tmp);
    end;
    Close(inFile);

    writeln(outFile,'<br><div>');
    for i:=0 to length(ids)-1 do begin
      writeln(outFile,'<a href="#',ids[i],'">',ids[i],'</a> &nbsp; ');
    end;
    writeln(outFile,'</div>');
    
    for i:=0 to length(ids)-1 do begin
      doc.create(ids[i]);
      writeln(outFile,doc.toHtml);
      doc.destroy;
    end;

    assign(inFile,htmlRoot+htmlFoot); reset(inFile);
    while not(eof(inFile)) do begin
      readln(inFile,tmp);
      writeln(outFile,tmp);
    end;
    Close(inFile);
    close(outFile);
  end;

INITIALIZATION
  locateHtml;


end.
