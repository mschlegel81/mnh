// MIT License
//
// Copyright (c) 2016 Martin Schlegel
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

UNIT mnh_fileWrappers;
INTERFACE
USES FileUtil,sysutils,Classes,Process,myGenerics,mnh_constants,myStringUtil,LazUTF8;
TYPE
  P_codeProvider = ^T_codeProvider;
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
    PROCEDURE getLinesUTF8(CONST value: TStrings);
    PROCEDURE setLines(CONST value: T_arrayOfString);
    PROCEDURE setLinesUTF8(CONST value: TStrings);
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
PROCEDURE fileStats(CONST name:ansistring; OUT lineCount,wordCount,byteCount:longint; OUT hash:T_hashInt);
FUNCTION fileLines(CONST name: ansistring; OUT accessed: boolean): T_arrayOfString;
FUNCTION writeFile(CONST name, textToWrite: ansistring): boolean;
FUNCTION writeFileLines(CONST name: ansistring; CONST textToWrite: T_arrayOfString; CONST lineSeparator:string): boolean;
FUNCTION find(CONST pattern: ansistring; CONST filesAndNotFolders,recurseSubDirs: boolean): T_arrayOfString;
FUNCTION filenameToPackageId(CONST filenameOrPath:ansistring):ansistring;

FUNCTION locateSource(CONST rootPath, id: ansistring): ansistring;
FUNCTION locateSources: T_arrayOfString;

FUNCTION runCommandAsyncOrPipeless(CONST executable: ansistring; CONST parameters: T_arrayOfString; CONST asynch:boolean): boolean;
PROCEDURE ensurePath(CONST path:ansistring);

IMPLEMENTATION
PROCEDURE ensurePath(CONST path:ansistring);
  begin
    ForceDirectories(extractFilePath(expandFileName(path)));
  end;

FUNCTION locateSource(CONST rootPath, id: ansistring): ansistring;
  PROCEDURE recursePath(CONST path: ansistring);
    VAR info: TSearchRec;
    begin
      if (FindFirst(path+id+SCRIPT_EXTENSION, faAnyFile and not(faDirectory), info) = 0) and
         ((info.Attr and faDirectory)<>faDirectory)
      then result:=expandFileName(path+info.name);
      sysutils.FindClose(info);
      if result<>'' then exit;

      if FindFirst(path+'*', faAnyFile, info) = 0 then repeat
        if ((info.Attr and faDirectory)=faDirectory) and (info.name<>'.') and (info.name<>'..') then recursePath(path+info.name+DirectorySeparator);
      until (findNext(info)<>0) or (result<>'');
      sysutils.FindClose(info);
    end;

  begin
    {$ifdef DEBUGMODE}
    writeln('Root path is "',rootPath,'"');
    writeln('Search folders: ',rootPath);
    writeln('                ',configDir);
    writeln('                ',extractFilePath(paramStr(0)));
    {$endIf}
    result := '';
    recursePath(rootPath);
    if result = '' then recursePath(configDir);
    if result = '' then recursePath(extractFilePath(paramStr(0)));
    {$ifdef DEBUGMODE}
    writeln('Found: ',result);
    {$endif}
  end;

FUNCTION locateSources: T_arrayOfString;
  PROCEDURE recursePath(CONST path: ansistring);
    VAR info: TSearchRec;
    begin
      if (FindFirst(path+'*'+SCRIPT_EXTENSION, faAnyFile and not(faDirectory), info) = 0) then repeat
        appendIfNew(result,path+info.name);
      until (findNext(info)<>0);
      sysutils.FindClose(info);

      if FindFirst(path+'*', faAnyFile, info) = 0 then repeat
        if (info.name<>'.') and (info.name<>'..') then recursePath(path+info.name+DirectorySeparator);
      until (findNext(info)<>0);
      sysutils.FindClose(info);
    end;

  begin
    setLength(result, 0);
    recursePath(configDir);
    recursePath(extractRelativePath(expandFileName(''),extractFilePath(paramStr(0))));
    recursePath(extractFilePath(paramStr(0)));
    recursePath('');
  end;

FUNCTION fileContent(CONST name: ansistring; OUT accessed: boolean): ansistring;
  VAR stream:TMemoryStream;
      size,i:longint;
  begin
    stream:=TMemoryStream.create;
    accessed:=false;
    try
      stream.loadFromFile(name);
      accessed:=true;
      size:=stream.size;
      stream.Seek(0,soFromBeginning);
      setLength(result,size);
      for i:=1 to size do result[i]:=chr(stream.readByte);
    except
      accessed:=false;
    end;
    stream.destroy;
  end;

PROCEDURE fileStats(CONST name:ansistring; OUT lineCount,wordCount,byteCount:longint; OUT hash:T_hashInt);
  VAR stream:TMemoryStream;
      i:longint;
      b:byte;
      space:boolean=true;
  begin
    stream:=TMemoryStream.create;
    lineCount:=0;
    wordCount:=0;
    byteCount:=0;
    hash:=0;
    if trim(name) = '' then exit;
    try
      stream.loadFromFile(name);
      stream.Seek(0,soFromBeginning);
      byteCount:=stream.size;
      hash:=T_hashInt(lt_string)+T_hashInt(byteCount);
      for i:=1 to byteCount do begin
        b:=stream.readByte;
        case b of
        ord('a')..ord('z'),ord('A')..ord('Z'): begin
          if space then inc(wordCount);
          space:=false;
        end;
        ord(C_lineBreakChar): begin
          inc(lineCount);
          space:=true;
        end;
        else space:=true;
        end;
        {$Q-}{$R-}
        hash:=hash*31+b;
        {$Q+}{$R+}
      end;
    except
      lineCount:=0;
      wordCount:=0;
      byteCount:=0;
      hash:=0;
    end;
    stream.destroy;
  end;

FUNCTION fileLines(CONST name: ansistring; OUT accessed: boolean): T_arrayOfString;
  VAR strings:TStringList;
      i:longint;
  begin
    strings:=TStringList.create;
    accessed:=false;
    try
      strings.loadFromFile(name);
      accessed:=true;
      setLength(result,strings.count);
      for i:=0 to length(result)-1 do result[i]:=strings[i];
    except
      accessed:=false;
    end;
    strings.destroy;
  end;

FUNCTION writeFile(CONST name, textToWrite: ansistring): boolean;
  VAR stream:TFileStream;
      i:longint;
  begin
    stream:=TFileStream.create(name,fmOpenWrite);
    try
      stream.Seek(0,soFromBeginning);
      for i:=1 to length(textToWrite) do stream.writeByte(ord(textToWrite[i]));
      result:=true;
    except
      result:=false;
    end;
    stream.destroy;
  end;

FUNCTION writeFileLines(CONST name: ansistring; CONST textToWrite: T_arrayOfString; CONST lineSeparator:string): boolean;
  VAR i,j: longint;
      textLineEnding:string;
      stream:TFileStream;

  PROCEDURE findTextLineEnding;
    FUNCTION currentTextLineEnding(): string;
      VAR stream:TFileStream;
          i:longint;
          c:array[0..1] of char=(#0,#0);
      begin
        result:='';
        stream:=TFileStream.create(name,fmOpenRead);
        try
          stream.Seek(0,soFromBeginning);
          i:=stream.size;
          while (i>0) and (result='') do begin
            c[0]:=c[1]; c[1]:=chr(stream.readByte);
            dec(i);
            if (c[0]=C_carriageReturnChar) and (c[1]=C_lineBreakChar) or
               (c[0]=C_lineBreakChar) and (c[1]=C_carriageReturnChar)
            then result:=c[0]+c[1]
            else if c[0]=C_lineBreakChar then result:=c[0];
          end;
        except
        end;
        stream.destroy;
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
      stream:=TFileStream.create(name,fmCreate);
      for i:=0 to length(textToWrite)-1 do begin
        for j:=1 to length(textToWrite[i]) do stream.writeByte(ord(textToWrite[i][j]));
        for j:=1 to length(textLineEnding) do stream.writeByte(ord(textLineEnding[j]));
      end;
      stream.destroy;
      result := true;
    except
      on e:Exception do begin
        result := false;
        writeln(stdErr,'Erron in writeFile Lines: ',e.message,'; ',e.ClassName);
      end;
    end;
  end;

FUNCTION find(CONST pattern: ansistring; CONST filesAndNotFolders,recurseSubDirs: boolean): T_arrayOfString;
  VAR info: TSearchRec;
      path: ansistring;
  begin
    path := extractFilePath(pattern);
    setLength(result, 0);
    if FindFirst(pattern, faAnyFile, info) = 0 then repeat
      if (info.name<>'.') and
         (info.name<>'..') and
        (((info.Attr and faDirectory) =faDirectory) and not(filesAndNotFolders) or
         ((info.Attr and faDirectory)<>faDirectory) and     filesAndNotFolders) then begin
        setLength(result, length(result)+1);
        result[length(result)-1] := path+info.name;
        if recurseSubDirs and not(filesAndNotFolders) then append(result,find(path+info.name+DirectorySeparator+'*',false,true));
      end;
    until (findNext(info)<>0);
    sysutils.FindClose(info);
  end;

FUNCTION runCommandAsyncOrPipeless(CONST executable: ansistring; CONST parameters: T_arrayOfString; CONST asynch:boolean): boolean;
  VAR tempProcess: TProcess;
      i: longint;
  begin
    result := true;
    try
      tempProcess := TProcess.create(nil);
      tempProcess.executable := executable;
      if not asynch then tempProcess.options:=tempProcess.options +[poWaitOnExit];
      for i := 0 to length(parameters)-1 do tempProcess.parameters.add(parameters[i]);
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
    while (i<=length(result)) and (result[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do inc(i);
    result := copy(result, 1, i-1);
  end;

{ T_codeProvider }

FUNCTION T_codeProvider.getLines: T_arrayOfString;
  VAR i: longint;
  begin
    setLength(result, length(lineData));
    for i := 0 to length(lineData)-1 do begin
      result[i] := lineData[i];
    end;
  end;

PROCEDURE T_codeProvider.getLinesUTF8(CONST value: TStrings);
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
      lineData[i] := value[i];
    end;
  end;

PROCEDURE T_codeProvider.setLinesUTF8(CONST value: TStrings);
  VAR i: longint;
    cleanCount: longint;
  begin
    cleanCount := value.count;
    while (cleanCount>0) and (trim(value[cleanCount-1]) = '') do dec(cleanCount);
    if length(lineData)<>cleanCount then begin
      setLength(lineData, cleanCount);
      outOfSync:=true;
    end;
    for i := 0 to cleanCount-1 do begin
      outOfSync := outOfSync or (trim(lineData[i])<>trim(value[i]));
      lineData[i] := value[i];
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
    result := filePath;
  end;

PROCEDURE T_codeProvider.load;
  VAR accessed: boolean;
      L: T_arrayOfString;
      i: longint;
  begin
    L := fileLines(filePath, accessed);
    if accessed then begin
      setLength(lineData, length(L));
      for i := 0 to length(L)-1 do lineData[i] := L[i];
      i := length(lineData);
      while (i>0) and (trim(lineData[i-1]) = '') do begin
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
