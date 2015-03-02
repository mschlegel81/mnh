UNIT mnh_fileWrappers;
{$WARNING TODO: Cleanup unit.}
INTERFACE
USES sysutils;
TYPE
  T_stringList=array of ansistring;

  P_codeProvider=^T_codeProvider;
  T_codeProvider=object
    CONSTRUCTOR forSakeOfCompleteness;
    FUNCTION fileName:ansistring; virtual; abstract;
    FUNCTION filePath:ansistring; virtual; abstract;
    FUNCTION fileIdentifier:ansistring; virtual; abstract;
    FUNCTION fileLines:T_stringList; virtual; abstract;
    FUNCTION fileChanged:boolean; virtual; abstract;
    PROCEDURE logCheck; virtual; abstract;
    PROCEDURE markAsDirty; virtual; abstract;
    DESTRUCTOR destroy; virtual; abstract;
  end;

  P_directInputWrapper=^T_directInputWrapper;

  { T_directInputWrapper }

  T_directInputWrapper=object(T_codeProvider)
    private
      lines:T_stringList;
      changedSinceCheck:boolean;
    public
     CONSTRUCTOR create;
     DESTRUCTOR destroy; virtual;
     FUNCTION fileName:ansistring; virtual;
     FUNCTION filePath:ansistring; virtual; 
     FUNCTION fileIdentifier:ansistring; virtual;
     FUNCTION fileLines:T_stringList; virtual;
     FUNCTION fileChanged:boolean; virtual;
     PROCEDURE logCheck; virtual;
     PROCEDURE markAsDirty; virtual;
     PROCEDURE setInput(CONST L:T_stringList);
     PROCEDURE setInput(CONST s:ansistring);
  end;

  P_fileWrapper=^T_fileWrapper;

  { T_fileWrapper }

  T_fileWrapper=object(T_codeProvider)
    private
      fpath:ansistring;
      checkedAtAge:longint;
    public
      CONSTRUCTOR create(CONST filepath_:ansistring);
      DESTRUCTOR destroy; virtual;
      FUNCTION fileName:ansistring; virtual;
      FUNCTION filePath:ansistring; virtual; 
      FUNCTION fileIdentifier:ansistring; virtual;
      FUNCTION getCheckedAtAge:longint;
      FUNCTION getHash:QWord;
      FUNCTION fileChanged:boolean; virtual;
      PROCEDURE logCheck; virtual;
      PROCEDURE markAsDirty; virtual;
      FUNCTION fileLines:T_stringList; virtual;
      FUNCTION exists:boolean;
      FUNCTION hasExtension(ext:ansistring):boolean;
      FUNCTION name:ansistring;
  end;

FUNCTION fileContent(CONST name:ansistring; OUT accessed:boolean):ansistring;
FUNCTION fileLines  (CONST name:ansistring; OUT accessed:boolean):T_stringList;
FUNCTION writeFile     (CONST name,textToWrite:ansistring):boolean;
{$WARNING TODO: FUNCTION writeFileLines(CONST name:ansistring; CONST textToWrite:T_stringList):boolean;}
FUNCTION find(CONST pattern:ansistring; CONST filesAndNotFolders:boolean):T_stringList;

PROCEDURE clearSourceScanPaths;
PROCEDURE addSourceScanPath(CONST path:ansistring);
FUNCTION locateSource(CONST id:ansistring):P_fileWrapper;
IMPLEMENTATION
VAR sourceScanPath:T_stringList;
PROCEDURE clearSourceScanPaths;
  begin
    setLength(sourceScanPath,0);
    addSourceScanPath(extractFilePath(paramstr(0)));
    addSourceScanPath('');
  end;

PROCEDURE addSourceScanPath(CONST path:ansistring);
  VAR expandedPath:ansistring;
      i:longint;
  begin
    expandedPath:=expandFileName(path);
    for i:=0 to length(sourceScanPath)-1 do if sourceScanPath[i]=expandedPath then exit;
    setLength(sourceScanPath,length(sourceScanPath)+1);
    sourceScanPath[length(sourceScanPath)-1]:=expandedPath;
  end;

FUNCTION locateSource(CONST id:ansistring):P_fileWrapper;
  CONST sourceExt='.MNH';

  FUNCTION nameToId(CONST fname:ansistring):ansistring;
    begin
      if uppercase(extractFileExt(fname))=sourceExt then begin
        result:=extractFileName(fname);
        result:=copy(result,1,length(result)-length(sourceExt));
      end else result:='';
    end;

  PROCEDURE recursePath(CONST path:ansistring);
    VAR info   :TSearchRec;
    begin
      if findFirst(path+'*',faAnyFile,info)=0 then repeat        
        if (info.attr and faDirectory)=faDirectory then begin
          if (info.name<>'.') and (info.name<>'..')
          then recursePath(path+info.name+DirectorySeparator);
        end else if nameToId(info.name)=id then
          new(result,create(path+info.name));
      until (findNext(info)<>0) or (result<>nil);
      sysutils.findClose(info);
    end;

  VAR i:longint;
  begin
    result:=nil;
    for i:=0 to length(sourceScanPath)-1 do
    if result=nil then recursePath(sourceScanPath[i]);
  end;
  
CONSTRUCTOR T_codeProvider.forSakeOfCompleteness;  begin end;

constructor T_directInputWrapper.create;
  begin setLength(lines,0); changedSinceCheck:=true; end;

destructor T_directInputWrapper.destroy;
  begin setLength(lines,0); end;

function T_directInputWrapper.fileName: ansistring;
  begin
    result:='<direct input>';
  end;
  
function T_directInputWrapper.filePath: ansistring;
  begin
    result:='';
  end;

function T_directInputWrapper.fileIdentifier: ansistring;
  begin
    result:='main';
  end;

function T_directInputWrapper.fileLines: T_stringList;
  VAR i:longint;
  begin
    setLength(result,length(lines));
    for i:=0 to length(lines)-1 do result[i]:=lines[i];
  end;

function T_directInputWrapper.fileChanged: boolean;
  begin
    result:=changedSinceCheck;
  end;

procedure T_directInputWrapper.logCheck;
  begin
    changedSinceCheck:=false;
  end;

procedure T_directInputWrapper.markAsDirty;
  begin
    changedSinceCheck:=true;
  end;

procedure T_directInputWrapper.setInput(const L: T_stringList);
  VAR i:longint;
  begin
    changedSinceCheck:=true;
    setLength(lines,length(L));
    for i:=0 to length(l)-1 do lines[i]:=L[i];
  end;

procedure T_directInputWrapper.setInput(const s: ansistring);
  begin
    changedSinceCheck:=true;
    setLength(lines,1);
    lines[0]:=s;
  end;

constructor T_fileWrapper.create(const filepath_: ansistring);
  begin
    fpath:=filepath_;
    logCheck;
  end;

destructor T_fileWrapper.destroy;
  begin
  end;

function T_fileWrapper.fileName: ansistring;
  begin
    result:=extractFileName(fpath);
  end;
  
function T_fileWrapper.filePath: ansistring;
  begin
    result:=fpath;
  end;  

function T_fileWrapper.fileIdentifier: ansistring;
  VAR i:longint;
  begin
    result:=fileName;
    i:=1;
    while (i<=length(result)) and (result[i] in ['A'..'Z','a'..'z','0'..'9','_']) do inc(i);
    result:=copy(result,1,i-1);
  end;

function T_fileWrapper.getCheckedAtAge: longint;
  begin
    result:=checkedAtAge;
  end;

function T_fileWrapper.getHash: QWord;
  CONST prime:array[0..3] of dword=(977,983,991,997);
  VAR handle:file of longword;
      block:array[0..2047] of dword;
      actuallyRead:longword;
      i:longword;
      wordRes:array[0..3] of dword;
  begin
    wordRes[0]:=0;
    wordRes[1]:=0;
    wordRes[2]:=0;
    wordRes[3]:=0;
    assign(handle,fpath);
    try
      reset(handle);
      while not(eof(handle)) do begin
        blockread(handle,block,length(block),actuallyRead);
        if actuallyRead>0 then for i:=0 to actuallyRead-1 do begin
          wordRes[0]:=wordRes[0]*prime[0]+block[i];
          wordRes[1]:=wordRes[1]*prime[1]+block[i];
          wordRes[2]:=wordRes[2]*prime[2]+block[i];
          wordRes[3]:=wordRes[3]*prime[3]+block[i];
        end;
      end;
      close(handle);
    except
      exit(0);
    end;
    result:=(QWord(wordRes[0]) or (QWord(wordRes[1]) shl 4)) xor
            (QWord(wordRes[2]) or (QWord(wordRes[3]) shl 4));
  end;

function T_fileWrapper.fileChanged: boolean;
  begin
    result:=fileage(fpath)<>checkedAtAge;
  end;

procedure T_fileWrapper.logCheck;
  begin
    checkedAtAge:=fileage(fpath);
  end;

procedure T_fileWrapper.markAsDirty;
  begin
    checkedAtAge:=checkedAtAge-1;
  end;

function T_fileWrapper.fileLines: T_stringList;
  VAR handle:textFile;
      i:longint;
  begin
    setLength(result,0); i:=0;
    try
      assign(handle,fpath);
      reset(handle);
      while not(eof(handle)) do begin
        setLength(result,i+1);
        readln(handle,result[i]);
        inc(i);
      end;
      close(handle);
    except
      setLength(result,0);
    end;
  end;

function T_fileWrapper.exists: boolean;
  begin
    result:=fileExists(fPath);
  end;

function T_fileWrapper.hasExtension(ext: ansistring): boolean;
  begin
    if (length(ext)<1) or (ext[1]<>'.') then ext:='.'+ext;
    result:=uppercase(extractFileExt(fpath))=uppercase(ext);
  end;

function T_fileWrapper.name: ansistring;
  begin
    result:=fPath;
  end;

//CONSTRUCTOR T_fileWrapperList.create;
//  begin
//    setLength(f,0);
//  end;
//
//DESTRUCTOR T_fileWrapperList.destroy;
//  VAR i:longint;
//  begin
//    for i:=0 to length(f)-1 do dispose(f[i],destroy);
//    setLength(f,0);
//  end;
//
//PROCEDURE T_fileWrapperList.addFiles(CONST pathOrPattern:ansistring);
//  VAR info :TSearchRec;
//      fpath:ansistring;
//  begin
//    if findFirst(pathOrPattern,faAnyFile,info)=0 then repeat
//      if ((info.attr and faDirectory)<>faDirectory) then begin
//        fpath:=ExtractFilePath(pathOrPattern)+info.name;
//        if not(hasFile(fpath)) then begin
//          setLength(f,length(f)+1);
//          new(f[length(f)-1],create(fpath));
//        end;
//      end;
//    until findNext(info)<>0;
//    sysutils.findClose(info);
//  end;
//
//PROCEDURE T_fileWrapperList.addFilesRecursively(CONST pathOrPattern:ansistring);
//  VAR info :TSearchRec;
//      fpath:ansistring;
//  begin
//    if findFirst(pathOrPattern,faAnyFile,info)=0 then repeat
//      if ((info.attr and faDirectory)<>faDirectory) then begin
//        fpath:=ExtractFilePath(pathOrPattern)+info.name;
//        if not(hasFile(fpath)) then begin
//          setLength(f,length(f)+1);
//          new(f[length(f)-1],create(fpath));
//        end;
//      end else if (info.name<>'.') and (info.name<>'..') then begin
//        addFilesRecursively(ExtractFilePath(pathOrPattern)+info.name+'\*');
//      end;
//    until findNext(info)<>0;
//    sysutils.findClose(info);
//  end;
//
//PROCEDURE T_fileWrapperList.dropNonexistentFiles;
//  VAR i,j:longint;
//  begin
//    j:=0;
//    for i:=0 to length(f)-1 do if f[i]^.exists then begin
//      f[j]:=f[i];
//      inc(j);
//    end else dispose(f[i],destroy);
//    setLength(f,j);
//  end;
//
//FUNCTION T_fileWrapperList.anyChanged:boolean;
//  VAR i:longint;
//  begin
//    for i:=0 to length(f)-1 do if f[i]^.fileChanged then exit(true);
//    result:=false;
//  end;
//
//FUNCTION T_fileWrapperList.size:longint;
//  begin
//    result:=length(f);
//  end;
//
//FUNCTION T_fileWrapperList.get(CONST index:longint):P_fileWrapper;
//  begin
//    if (index>=0) and (index<length(f))
//      then result:=f[index]
//      else result:=nil;
//  end;
//
//FUNCTION T_fileWrapperList.hasFile(CONST name:ansistring):boolean;
//  VAR i:longint;
//  begin
//    for i:=0 to length(f)-1 do if trim(uppercase(f[i]^.name))=trim(uppercase(name)) then exit(true);
//    result:=false;
//  end;
//
//PROCEDURE T_fileWrapperList.dropFile(CONST index:longint);
//  VAR i:longint;
//  begin
//    if (index>=0) and (index<length(f)) then begin
//      dispose(f[index],destroy);
//      for i:=index to length(f)-2 do f[i]:=f[i+1];
//      setLength(f,length(f)-1);
//    end;
//  end;

FUNCTION fileContent(CONST name:ansistring; OUT accessed:boolean):ansistring;
  VAR handle:file of char;
      block:array[0..1023] of char;
      actuallyRead,i:longint;
  begin
    try
      accessed:=true;
      assign(handle,name);
      reset(handle);
      result:='';
      repeat
        blockread(handle,block,length(block),actuallyRead);
        for i:=0 to actuallyRead-1 do result:=result+block[i];
      until actuallyRead<length(block);
      close(handle);
    except
      accessed:=false;
      result:='';
    end;
  end;
  
FUNCTION fileLines  (CONST name:ansistring; OUT accessed:boolean):T_stringList;
  VAR handle:textFile;
  begin
    setLength(result,0);
    try
      accessed:=true;
      assign(handle,name);
      reset(handle);
      while not(eof(handle)) do begin
        setLength(result,length(result)+1);
        readln(handle,result[length(result)-1]);
      end;
      close(handle);
    except
      accessed:=false;
      setLength(result,0);
    end;
  end;
  
FUNCTION writeFile(CONST name,textToWrite:ansistring):boolean;
  VAR handle:file of char;
      block:array[0..1023] of char;
      i,j:longint;
  begin
    try
      result:=true;
      assign(handle,name);
      rewrite(handle);
      i:=1;
      while i<=length(textToWrite) do begin
        j:=0;
        while (i<=length(textToWrite)) and (j<length(block)) do begin
          block[j]:=textToWrite[i];
          inc(i);
          inc(j);
        end;
        blockwrite(handle,block,j);
      end;
      close(handle);
    except
      result:=false;
    end;
  end;

FUNCTION find(CONST pattern:ansistring; CONST filesAndNotFolders:boolean):T_stringList;
  VAR info   :TSearchRec;
      path:ansistring;
  begin
    path:=ExtractFilePath(pattern);
    setLength(result,0);
    if findFirst(pattern,faAnyFile,info)=0 then repeat
      if (info.name<>'.') and (info.name<>'..') and
         (((info.attr and faDirectory)= faDirectory) and not(filesAndNotFolders) or
          ((info.attr and faDirectory)<>faDirectory) and     filesAndNotFolders ) then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=path+info.name;
      end;
    until (findNext(info)<>0);
    sysutils.findClose(info);
  end;

INITIALIZATION
  clearSourceScanPaths;
end.
