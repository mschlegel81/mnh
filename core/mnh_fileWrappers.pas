UNIT mnh_fileWrappers;
INTERFACE
USES sysutils,Classes,process,myGenerics,mnh_constants,myStringUtil;
TYPE
  P_codeProvider = ^T_codeProvider;

  { T_codeProvider }

  T_codeProvider = object
  private
    filePath: ansistring;
    lineData: T_arrayOfString;
    fileContentsEnforced,
    outOfSync:boolean;
    fileAccessedAtFileAge:double;
  public
    CONSTRUCTOR create;
    CONSTRUCTOR create(CONST path: ansistring);
    DESTRUCTOR destroy;

    FUNCTION getLines: T_arrayOfString;
    PROCEDURE getLines(CONST value: TStrings);
    PROCEDURE setLines(CONST value: T_arrayOfString);
    PROCEDURE setLines(CONST value: TStrings);
    PROCEDURE setLines(CONST value: ansistring);
    PROCEDURE appendLine(CONST value: ansistring);


    PROCEDURE replaceCode(CONST line0, col0:longint; line1:longint; CONST col1: longint; CONST newText: ansistring);

    PROCEDURE setPath(CONST path: ansistring);
    FUNCTION getPath: ansistring;
    PROCEDURE load;
    PROCEDURE save;
    FUNCTION fileName: ansistring;
    FUNCTION fileHasChanged: boolean;
    FUNCTION fileIsOutOfSync: boolean;

    FUNCTION id: ansistring;
    PROCEDURE clear;
  end;

FUNCTION fileContent(CONST name: ansistring; OUT accessed: boolean): ansistring;
FUNCTION fileLines(CONST name: ansistring; OUT accessed: boolean): T_arrayOfString;
FUNCTION fileLines(CONST name: ansistring; CONST firstIdx,lastIdx:longint; OUT accessed: boolean): T_arrayOfString;
FUNCTION writeFile(CONST name, textToWrite: ansistring): boolean;
FUNCTION writeFileLines(CONST name: ansistring; CONST textToWrite: T_arrayOfString; CONST lineSeparator:string): boolean;
FUNCTION find(CONST pattern: ansistring; CONST filesAndNotFolders,recurseSubDirs: boolean): T_arrayOfString;
FUNCTION filenameToPackageId(CONST filenameOrPath:ansistring):ansistring;

FUNCTION locateSource(CONST rootPath, id: ansistring): ansistring;
FUNCTION locateSources: T_arrayOfString;

FUNCTION runCommandAsyncOrPipeless(CONST executable: ansistring; CONST parameters: T_arrayOfString; CONST asynch:boolean): boolean;
PROCEDURE ensurePath(path:ansistring);

IMPLEMENTATION
PROCEDURE ensurePath(path:ansistring);
  VAR newDir:string;
      ensuredDir:string;
      p,p2:longint;
  begin
    ensuredDir:='';
    path:=extractFilePath(expandFileName(path));
    if path[length(path)] in ['/','\'] then path:=copy(path,1,length(path)-1);
    while path<>'' do begin
      p:=pos('/',path);
      p2:=pos('\',path);
      if (p<=0) or (p2>0) and (p2<p) then p:=p2;
      if p<=0 then begin
        newDir:=path;
        path:='';
      end else begin
        newDir:=copy(path,1,p-1);
        path:=copy(path,p+1,length(path)-p);
      end;
      if ensuredDir='' then ensuredDir:=newDir
                       else ensuredDir:=ensuredDir+DirectorySeparator+newDir;
      CreateDir(ensuredDir);
    end;
  end;

FUNCTION locateSource(CONST rootPath, id: ansistring): ansistring;
  FUNCTION nameToId(CONST fname: ansistring): ansistring;
    begin
      if uppercase(extractFileExt(fname)) = SCRIPT_EXTENSION then
        begin
        result := extractFileName(fname);
        result := copy(result, 1, length(result)-length(SCRIPT_EXTENSION));
        end
      else
        result := '';
      if isReservedNamespace( lowercase(result)) then
        result := '';
    end;

  PROCEDURE recursePath(CONST path: ansistring);
    VAR
      info: TSearchRec;
    begin
      if findFirst(path+'*', faAnyFile, info) = 0 then
        repeat
          if (info.Attr and faDirectory) = faDirectory then
            begin
            if (info.name<>'.') and (info.name<>'..') then
              recursePath(path+info.name+DirectorySeparator);
            end
          else if nameToId(info.name) = id then
            result := path+info.name;
        until (findNext(info)<>0) or (result<>'');
      sysutils.findClose(info);
    end;

  begin
    result := '';
    recursePath(extractRelativePath(expandFileName(''),extractFilePath(rootPath)));
    if result = '' then recursePath(extractRelativePath(expandFileName(''),extractFilePath(paramStr(0))));
    if result = '' then recursePath(extractRelativePath(expandFileName(''),''));
  end;

FUNCTION locateSources: T_arrayOfString;
  PROCEDURE recursePath(CONST path: ansistring);
    VAR info: TSearchRec;
    begin
      if findFirst(path+'*', faAnyFile, info) = 0 then repeat
        if (info.Attr and faDirectory) = faDirectory then begin
          if (info.name<>'.') and (info.name<>'..') then
            recursePath(path+info.name+DirectorySeparator);
        end else if uppercase(extractFileExt(info.name)) = SCRIPT_EXTENSION then
          appendIfNew(result,extractRelativePath(expandFileName(''),path+info.name));
      until (findNext(info)<>0);
      sysutils.findClose(info);
    end;

  begin
    setLength(result, 0);
    recursePath('');
    recursePath(extractFilePath(paramStr(0)));
  end;


FUNCTION fileContent(CONST name: ansistring; OUT accessed: boolean): ansistring;
  VAR handle: file of char;
      block: array[0..1023] of char;
      actuallyRead, i: longint;
  begin
    if trim(name) = '' then begin
      accessed := false;
      exit;
    end;
    try
      accessed := true;
      assign(handle, name);
      reset(handle);
      result := '';
      repeat
        blockread(handle, block, length(block), actuallyRead);
        for i := 0 to actuallyRead-1 do
          result := result+block [i];
      until actuallyRead<length(block);
      close(handle);
    except
      accessed := false;
      result := '';
    end;
  end;

FUNCTION fileLines(CONST name: ansistring; OUT accessed: boolean): T_arrayOfString;
  begin
    result:=fileLines(name,0,maxLongint,accessed);
  end;

FUNCTION fileLines(CONST name: ansistring; CONST firstIdx,lastIdx:longint; OUT accessed: boolean): T_arrayOfString;
  VAR handle: textFile;
      lineIndex:longint=0;
      tempLine:ansistring;
  begin
    setLength(result, 0);
    if trim(name) = '' then begin
      accessed := false;
      exit;
    end;
    try
      accessed := true;
      assign(handle, name);
      reset(handle);
      while not (eof(handle)) and (lineIndex<=lastIdx) do begin
        readln(handle,tempLine);
        if (lineIndex>=firstIdx) and (lineIndex<=lastIdx) then begin
          setLength(result, length(result)+1);
          result [length(result)-1]:=tempLine;
        end;
        inc(lineIndex);
      end;
      close(handle);
    except
      accessed := false;
      setLength(result, 0);
    end;
  end;

FUNCTION writeFile(CONST name, textToWrite: ansistring): boolean;
  VAR
    handle: file of char;
    block: array[0..1023] of char;
    i, j: longint;
  begin
    if trim(name) = '' then exit(false);
    try
      ensurePath(name);
      result := true;
      assign(handle, name);
      rewrite(handle);
      i := 1;
      while i<=length(textToWrite) do begin
        j := 0;
        while (i<=length(textToWrite)) and (j<length(block)) do
          begin
          block[j] := textToWrite [i];
          inc(i);
          inc(j);
          end;
        blockwrite(handle, block, j);
      end;
      close(handle);
    except
      result := false;
    end;
  end;

FUNCTION writeFileLines(CONST name: ansistring; CONST textToWrite: T_arrayOfString; CONST lineSeparator:string): boolean;
  VAR handle: textFile;
      i: longint;
      textLineEnding:string;

  PROCEDURE findTextLineEnding;
    FUNCTION currentTextLineEnding(): string;
      VAR chandle: file of char;
          block: array[0..1023] of char;
          actuallyRead, i: longint;
          content:ansistring='';
          pr,pn:longint;
      begin
        try
          assign(chandle, name);
          reset(chandle);
          content := '';
          result :='';
          repeat
            blockread(chandle, block, length(block), actuallyRead);
            for i := 0 to actuallyRead-1 do content := content+block [i];
            pr:=pos(C_carriageReturnChar,content);
            pn:=pos(C_lineBreakChar,content);
            if (pn>0) and (pn<length(content)) or
               (pr>0) and (pr<length(content)) then begin
              if (pr<=0) and (pn>0) then result:=C_lineBreakChar
              else if (pn<=0) and (pr>0) then result:=C_carriageReturnChar
              else if (pr>0) and (pn=pr+1) then result:=C_carriageReturnChar+C_lineBreakChar
              else if (pn>0) and (pr=pn+1) then result:=C_lineBreakChar+C_carriageReturnChar;
           end;
          until (actuallyRead<length(block)) or (result<>'');
          close(chandle);
        except
          result :=LineEnding;
        end;
        content := '';
        if result='' then result:=LineEnding;
      end;

    begin
      if     lineSeparator<>'' then textLineEnding:=lineSeparator
      else if fileExists(name) then textLineEnding:=currentTextLineEnding
      else textLineEnding:=LineEnding;
    end;

  begin
    if trim(name) = '' then exit(false);
    try
      ensurePath(name);
      findTextLineEnding;
      assign(handle, name);
      SetTextLineEnding(handle,textLineEnding);
      rewrite(handle);
      for i := 0 to length(textToWrite)-1 do
        writeln(handle, textToWrite [i]);
      close(handle);
      result := true;
    except
      result := false;
    end;
  end;

FUNCTION find(CONST pattern: ansistring; CONST filesAndNotFolders,recurseSubDirs: boolean): T_arrayOfString;
  VAR info: TSearchRec;
    path: ansistring;
  begin
    path := extractFilePath(pattern);
    setLength(result, 0);
    if findFirst(pattern, faAnyFile, info) = 0 then repeat
      if (info.name<>'.') and
         (info.name<>'..') and
        (((info.Attr and faDirectory) =faDirectory) and not(filesAndNotFolders) or
         ((info.Attr and faDirectory)<>faDirectory) and     filesAndNotFolders) then begin
        setLength(result, length(result)+1);
        result[length(result)-1] := path+info.name;
        if recurseSubDirs and not(filesAndNotFolders) then append(result,find(path+info.name+DirectorySeparator+'*',false,true));
      end;
    until (findNext(info)<>0);
    sysutils.findClose(info);
  end;

FUNCTION runCommandAsyncOrPipeless(CONST executable: ansistring; CONST parameters: T_arrayOfString; CONST asynch:boolean): boolean;
  VAR tempProcess: TProcess;
      i: longint;
  begin
    result := true;
    try
      tempProcess := TProcess.create(nil);
      tempProcess.executable := executable;
      if not asynch then tempProcess.options:=tempProcess.options + [poWaitOnExit];
      for i := 0 to length(parameters)-1 do tempProcess.parameters.add(parameters [i]);
      tempProcess.execute;
      tempProcess.free;
    except
      result := false;
    end;
  end;

FUNCTION filenameToPackageId(CONST filenameOrPath:ansistring):ansistring;
  VAR i:longint;
  begin
    result:=extractFileName(filenameOrPath);
    i := 1;
    while (i<=length(result)) and (result [i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do inc(i);
    result := copy(result, 1, i-1);
  end;

{ T_codeProvider }

FUNCTION T_codeProvider.getLines: T_arrayOfString;
  VAR i: longint;
  begin
    setLength(result, length(lineData));
    for i := 0 to length(lineData)-1 do begin
      result[i] := lineData [i];
    end;
  end;

PROCEDURE T_codeProvider.getLines(CONST value: TStrings);
  VAR i:longint;
  begin
    value.clear;
    for i:=0 to length(lineData)-1 do value.append(lineData[i]);
  end;

PROCEDURE T_codeProvider.setLines(CONST value: T_arrayOfString);
  VAR i: longint;
  begin
    outOfSync:=length(lineData)<>length(value);
    setLength(lineData, length(value));
    for i := 0 to length(value)-1 do begin
      outOfSync:=outOfSync or (lineData[i]<>value[i]);
      lineData[i] := value [i];
    end;
  end;

PROCEDURE T_codeProvider.setLines(CONST value: TStrings);
  VAR i: longint;
    cleanCount: longint;
  begin
    cleanCount := value.count;
    while (cleanCount>0) and (trim(value [cleanCount-1]) = '') do dec(cleanCount);
    if length(lineData)<>cleanCount then begin
      setLength(lineData, cleanCount);
      outOfSync:=true;
    end;
    for i := 0 to cleanCount-1 do begin
      outOfSync := outOfSync or (trim(lineData [i])<>trim(value [i]));
      lineData[i] := value [i];
    end;
    fileContentsEnforced:=true;
  end;

PROCEDURE T_codeProvider.setLines(CONST value: ansistring);
  begin
    outOfSync:=(length(lineData)<>1) or (lineData[0]<>value);
    setLength(lineData, 1);
    lineData[0] := value;
  end;

PROCEDURE T_codeProvider.appendLine(CONST value: ansistring);
  begin
    setLength(lineData, length(lineData)+1);
    lineData[length(lineData)-1] := value;
    outOfSync:=true;
  end;

PROCEDURE T_codeProvider.replaceCode(CONST line0, col0:longint; line1:longint; CONST col1: longint; CONST newText: ansistring);
  VAR i:longint;
      partAfter,
      partBefore:ansistring;
  begin
    //Remember partBefore and part after for later reference
    if (line1>=length(lineData)) or (line1<0) or (col1<=0) then begin
      partAfter:='';
      line1:=length(lineData)+1;
    end else begin
      partAfter:=copy(lineData[line1],col1+1,length(lineData[line1]));
    end;
    partBefore:=copy(lineData[line0],1,col0-1);
    //clear all related lines
    for i:=line0 to line1-1 do lineData[i]:='';
    //rewrite lines
    lineData[line0]:=partBefore+newText;
    if line1=line0 then lineData[line0]:=lineData[line0]+partAfter;
    outOfSync:=true;
  end;

CONSTRUCTOR T_codeProvider.create;
  begin
    clear;
    filePath:='';
    outOfSync:=false;
    fileContentsEnforced:=false;
  end;

CONSTRUCTOR T_codeProvider.create(CONST path: ansistring);
  begin
    clear;
    filePath := path;
    if fileExists(path) then load;
    fileContentsEnforced:=false;
  end;

DESTRUCTOR T_codeProvider.destroy;
  begin
    setLength(lineData, 0);
  end;

PROCEDURE T_codeProvider.setPath(CONST path: ansistring);
  begin
    filePath := path;
  end;

FUNCTION T_codeProvider.getPath: ansistring;
  begin
    result := extractRelativePath(expandFileName(''),filePath);
  end;

PROCEDURE T_codeProvider.load;
  VAR accessed: boolean;
      L: T_arrayOfString;
      i: longint;
  begin
    L := fileLines(filePath, accessed);
    if accessed then begin
      setLength(lineData, length(L));
      for i := 0 to length(L)-1 do lineData[i] := L [i];
      i := length(lineData);
      while (i>0) and (trim(lineData [i-1]) = '') do begin
        setLength(lineData, i-1);
        dec(i);
      end;
      fileAge(filePath,fileAccessedAtFileAge);
      outOfSync:=false;
    end;
  end;

PROCEDURE T_codeProvider.save;
  begin
    if (filePath<>'') and writeFileLines(filePath, lineData,'') then begin
      fileAge(filePath,fileAccessedAtFileAge);
      outOfSync:=false;
    end;
  end;

FUNCTION T_codeProvider.fileName: ansistring;
  begin
    result := extractFileName(filePath);
  end;

FUNCTION T_codeProvider.fileHasChanged: boolean;
  VAR currentFileAge: double;
  begin
    if (filePath<>'') and fileExists(filePath) and not(fileContentsEnforced) then begin
      fileAge(filePath, currentFileAge);
      result:=currentFileAge>fileAccessedAtFileAge;
    end else result := false;
  end;

FUNCTION T_codeProvider.fileIsOutOfSync: boolean;
  begin
    result:=(filePath<>'') and outOfSync;
  end;

FUNCTION T_codeProvider.id: ansistring;
  begin
    result:=filenameToPackageId(filePath);
  end;

PROCEDURE T_codeProvider.clear;
  begin
    filePath := '';
    setLength(lineData, 0);

  end;

end.
