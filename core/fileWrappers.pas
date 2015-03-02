UNIT fileWrappers;
INTERFACE
USES sysutils;
TYPE
  T_stringList=array of ansistring;

  P_codeProvider=^T_codeProvider;
  T_codeProvider=object
    FUNCTION fileName:string; virtual; abstract;
    FUNCTION fileIdentifier:string; virtual; abstract;
    FUNCTION fileLines:T_stringList; virtual; abstract;
    FUNCTION fileChanged:boolean; virtual; abstract;
    PROCEDURE logCheck; virtual; abstract;
  end;
  
  P_directInputWrapper=^T_directInputWrapper;
  T_directInputWrapper=object(T_codeProvider)
    private
      lines:T_stringList;
      changedSinceCheck:boolean;
    public
     CONSTRUCTOR create;
     DESTRUCTOR destroy;
     FUNCTION fileName:string; virtual; 
     FUNCTION fileIdentifier:string; virtual; 
     FUNCTION fileLines:T_stringList; virtual; 
     FUNCTION fileChanged:boolean; virtual; 
     PROCEDURE logCheck; virtual;
     PROCEDURE setInput(CONST L:T_stringList);
     PROCEDURE setInput(CONST s:ansistring);
  end;
  
  P_fileWrapper=^T_fileWrapper;
  T_fileWrapper=object(T_codeProvider)
    private
      fpath:ansistring;
      checkedAtAge:longint;
    public
      CONSTRUCTOR create(CONST filepath:ansistring);
      DESTRUCTOR destroy;
      FUNCTION fileName:string; virtual; 
      FUNCTION fileIdentifier:string; virtual; 
      FUNCTION getCheckedAtAge:longint;
      FUNCTION getAge:longint;
      FUNCTION getHash:QWord;
      FUNCTION fileChanged:boolean; virtual;
      PROCEDURE logCheck; virtual;
      FUNCTION fileLines:T_stringList; virtual;   
      FUNCTION exists:boolean;
      FUNCTION hasExtension(ext:ansistring):boolean;
      FUNCTION name:ansistring;
  end;
  
  T_fileWrapperList=object
    private
      f:array of P_fileWrapper;
    public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addFiles(CONST pathOrPattern:ansistring);
    PROCEDURE addFilesRecursively(CONST pathOrPattern:ansistring);
    PROCEDURE dropNonexistentFiles;
    FUNCTION anyChanged:boolean;
    
    FUNCTION size:longint;
    FUNCTION get(CONST index:longint):P_fileWrapper;
    PROPERTY fileAt[index:longint]:P_fileWrapper read get; default;    
    FUNCTION hasFile(CONST name:ansistring):boolean;
    PROCEDURE dropFile(CONST index:longint);
  end;

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
  CONST sourceExt='.PAS';
  
  FUNCTION nameToId(CONST fname:ansistring):ansistring;
    begin
      if uppercase(extractFileExt(fname))=sourceExt then begin
        result:=extractFileName(fname);
        result:=copy(result,1,length(result)-length(sourceExt));
      end else result:=#13;
    end;
    
  PROCEDURE recursePath(CONST path:ansistring);
    VAR info   :TSearchRec;
    begin 
      if findFirst(path+'*',faAnyFile,info)=0 then repeat
        if (info.attr and faDirectory)=faDirectory then begin
          if (info.name<>'.') and (info.name<>'..') then recursePath(path+info.name+DirectorySeparator);
        end else if nameToId(info.name)=id then new(result,create(path+info.name));
      until (findNext(info)<>0) or (result<>nil);
      sysutils.findClose(info);
    end;
  VAR i:longint;
  begin
    result:=nil;
    for i:=0 to length(sourceScanPath)-1 do if result=nil then recursePath(sourceScanPath[i]);
  end;

CONSTRUCTOR T_directInputWrapper.create;
  begin setLength(lines,0); changedSinceCheck:=true; end;
  
DESTRUCTOR T_directInputWrapper.destroy;
  begin setLength(lines,0); end;

FUNCTION T_directInputWrapper.fileName:string; 
  begin
    result:='<direct input>';
  end;

FUNCTION T_directInputWrapper.fileIdentifier:string; 
  begin
    result:='main';
  end;

FUNCTION T_directInputWrapper.fileLines:T_stringList; 
  VAR i:longint;
  begin
    setLength(result,length(lines));
    for i:=0 to length(lines)-1 do result[i]:=lines[i];
  end;

FUNCTION T_directInputWrapper.fileChanged:boolean;  
  begin
    result:=changedSinceCheck;  
  end;

PROCEDURE T_directInputWrapper.logCheck;
  begin
    changedSinceCheck:=false;
  end;

PROCEDURE T_directInputWrapper.setInput(CONST L:T_stringList);
  VAR i:longint;
  begin
    changedSinceCheck:=true;
    setLength(lines,length(L));
    for i:=0 to length(l)-1 do lines[i]:=L[i];
  end;
  
PROCEDURE T_directInputWrapper.setInput(CONST s:ansistring);
  begin
    changedSinceCheck:=true;
    setLength(lines,1);
    lines[0]:=s;
  end;

CONSTRUCTOR T_fileWrapper.create(CONST filepath:ansistring);
  begin
    fpath:=filepath;
    logCheck;
  end;

DESTRUCTOR T_fileWrapper.destroy;
  begin
  end;
  
FUNCTION T_fileWrapper.fileName:string; 
  begin
    result:=extractFileName(fpath); 
  end;

FUNCTION T_fileWrapper.fileIdentifier:string; 
  VAR i:longint;
  begin
    result:=fileName;
    i:=1;
    while (i<=length(result)) and (result[i] in ['A'..'Z','a'..'z','0'..'9','_']) do inc(i);
    result:=copy(result,1,i-1);
  end;
  
FUNCTION T_fileWrapper.getCheckedAtAge:longint;
  begin
    result:=checkedAtAge;
  end;
  
FUNCTION T_fileWrapper.getAge:longint;
  begin
    result:=fileage(fpath);
  end;

FUNCTION T_fileWrapper.getHash:QWord;
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
  
FUNCTION T_fileWrapper.fileChanged:boolean;
  begin
    result:=getAge<>checkedAtAge;
  end;
  
PROCEDURE T_fileWrapper.logCheck;
  begin
    checkedAtAge:=getAge;
  end;
  
FUNCTION T_fileWrapper.fileLines:T_stringList;  
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
  
FUNCTION T_fileWrapper.exists:boolean;  
  begin
    result:=fileExists(fPath);
  end;
  
FUNCTION T_fileWrapper.hasExtension(ext:ansistring):boolean;
  begin
    if (length(ext)<1) or (ext[1]<>'.') then ext:='.'+ext;
    result:=uppercase(extractFileExt(fpath))=uppercase(ext);
  end;  
  
FUNCTION T_fileWrapper.name:ansistring;
  begin
    result:=fPath;
  end;

CONSTRUCTOR T_fileWrapperList.create;
  begin
    setLength(f,0);
  end;
  
DESTRUCTOR T_fileWrapperList.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(f)-1 do dispose(f[i],destroy);
    setLength(f,0);  
  end;
  
PROCEDURE T_fileWrapperList.addFiles(CONST pathOrPattern:ansistring);
  VAR info :TSearchRec;
      fpath:ansistring;
  begin
    if findFirst(pathOrPattern,faAnyFile,info)=0 then repeat
      if ((info.attr and faDirectory)<>faDirectory) then begin
        fpath:=ExtractFilePath(pathOrPattern)+info.name;
        if not(hasFile(fpath)) then begin
          setLength(f,length(f)+1);
          new(f[length(f)-1],create(fpath));
        end;      
      end;
    until findNext(info)<>0;
    sysutils.findClose(info);    
  end;

PROCEDURE T_fileWrapperList.addFilesRecursively(CONST pathOrPattern:ansistring);
  VAR info :TSearchRec;
      fpath:ansistring;
  begin
    if findFirst(pathOrPattern,faAnyFile,info)=0 then repeat
      if ((info.attr and faDirectory)<>faDirectory) then begin
        fpath:=ExtractFilePath(pathOrPattern)+info.name;
        if not(hasFile(fpath)) then begin
          setLength(f,length(f)+1);
          new(f[length(f)-1],create(fpath));
        end;      
      end else if (info.name<>'.') and (info.name<>'..') then begin          
        addFilesRecursively(ExtractFilePath(pathOrPattern)+info.name+'\*');
      end;
    until findNext(info)<>0;
    sysutils.findClose(info);    
  end;
  
PROCEDURE T_fileWrapperList.dropNonexistentFiles;
  VAR i,j:longint;
  begin
    j:=0;
    for i:=0 to length(f)-1 do if f[i]^.exists then begin
      f[j]:=f[i];
      inc(j);
    end else dispose(f[i],destroy);
    setLength(f,j);
  end;

FUNCTION T_fileWrapperList.anyChanged:boolean;    
  VAR i:longint;
  begin
    for i:=0 to length(f)-1 do if f[i]^.fileChanged then exit(true);
    result:=false;  
  end;
  
FUNCTION T_fileWrapperList.size:longint;
  begin
    result:=length(f);
  end;
  
FUNCTION T_fileWrapperList.get(CONST index:longint):P_fileWrapper;
  begin
    if (index>=0) and (index<length(f)) 
      then result:=f[index] 
      else result:=nil;
  end;
  
FUNCTION T_fileWrapperList.hasFile(CONST name:ansistring):boolean;
  VAR i:longint;  
  begin
    for i:=0 to length(f)-1 do if trim(uppercase(f[i]^.name))=trim(uppercase(name)) then exit(true);    
    result:=false;
  end;

PROCEDURE T_fileWrapperList.dropFile(CONST index:longint);
  VAR i:longint;
  begin
    if (index>=0) and (index<length(f)) then begin
      dispose(f[index],destroy);
      for i:=index to length(f)-2 do f[i]:=f[i+1];
      setLength(f,length(f)-1);
    end; 
  end;
  
INITIALIZATION
  clearSourceScanPaths;
end.