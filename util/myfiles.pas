UNIT myFiles;
INTERFACE
USES {$ifdef UNIX}cmem,cthreads,{$endif}sysutils,process;
{$MACRO ON}
CONST bufferSize=1000;
TYPE

  T_file=object
    private
      readMode,stateOkay:boolean;
      handle:file of byte;
      buffer:array[0..bufferSize-1] of byte;
      bufFill:longint;
      PROCEDURE flushBuffer;
      PROCEDURE readBuffer;
    public

    CONSTRUCTOR createToRead (filename:string);
    CONSTRUCTOR createToWrite(filename:string);
    DESTRUCTOR  destroy;
    FUNCTION    allOkay:boolean;

    PROCEDURE writeByte(x:byte);
    FUNCTION   readByte  :byte;
    PROCEDURE writeBoolean(x:boolean);
    FUNCTION   readBoolean:boolean;
    PROCEDURE writeWord(x:word);
    FUNCTION   readWord  :word;
    PROCEDURE writeDWord(x:dword);
    FUNCTION   readDWord  :dword;
    PROCEDURE writeQWord(x:qword);
    FUNCTION   readQWord  :qword;
    PROCEDURE writeLongint(x:longint);
    FUNCTION   readLongint  :longint;
    PROCEDURE writeShortint(x:shortint);
    FUNCTION   readShortint  :shortint;
    PROCEDURE writeChar(x:char);
    FUNCTION   readChar  :char;
    PROCEDURE writeSingle(x:single);
    FUNCTION   readSingle  :single;
    PROCEDURE writeDouble(x:double);
    FUNCTION   readDouble  :double;
    PROCEDURE writeExtended(x:extended);
    FUNCTION   readExtended  :extended;    

    PROCEDURE writeShortstring(x:shortstring);
    FUNCTION   readShortstring  :shortstring;
    PROCEDURE writeAnsiString (x:ansistring);
    FUNCTION   readAnsiString   :ansistring;
    PROCEDURE writeBuf(p:pbyte; psize:longint);
    PROCEDURE readBuf (p:pbyte; psize:longint);
  end;

  T_serializable=object
    CONSTRUCTOR notReallyAConstructor; 
    FUNCTION  loadFromFile(filename:string):boolean;                 overload; //liest die Inhalte des Objektes aus der Datei mit dem übergebenen Namen und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(filename:string);                           overload; //schreibt die Inhalte des Objektes in die Datei mit dem übergebenen Namen
    FUNCTION  loadFromFile(VAR F:T_File):boolean; virtual; abstract; overload; //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(VAR F:T_File);           virtual; abstract; overload; //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei

  end;


PROCEDURE pack(filename:ansistring);
PROCEDURE pack(fileList,outfile:ansistring);
PROCEDURE unpack(packedFile:ansistring);
FUNCTION execute(call:ansistring):longint;

IMPLEMENTATION
FUNCTION execute(call:ansistring):longint;
  VAR tempProcess:TProcess;
  begin
    try
    tempProcess :=TProcess.create(nil);
    tempProcess.CommandLine :=call;
    tempProcess.options:=tempProcess.options+[powaitonexit];
    tempProcess.execute;
    result:=tempProcess.exitStatus;
    tempProcess.Free;
    except result:=-1001 end;
  end;

PROCEDURE pack(fileList,outfile:ansistring);
  begin
    if fileExists('7z.exe') then
      execute('7z.exe a '+outfile+' -mx=9 '+filelist);

  end;

PROCEDURE pack(filename:ansistring);
  begin
    pack(filename,filename+'.7z');
  end;

PROCEDURE unpack(packedFile:ansistring);
  begin
    if fileExists('7z.exe') then begin
      execute('7z.exe e '+packedFile+' -y');
    end;
  end;

PROCEDURE T_file.flushBuffer;
  begin
    if not(readMode) and stateOkay then begin
      blockwrite(handle,buffer,bufFill);
      bufFill:=0;
    end;
  end;

PROCEDURE T_file.readBuffer;
  VAR actuallyRead:longint;
  begin
    if readmode and stateOkay then begin
      blockread(handle,buffer[bufFill],buffersize-bufFill,actuallyRead);
      bufFill:=bufFill+actuallyRead;
    end;
  end;

CONSTRUCTOR T_file.createToRead (filename:string);
  begin
    readMode:=true;
    bufFill:=0;
    if fileExists(filename) then begin
      assign(handle,filename);
      reset(handle);
      readBuffer;
      stateOkay:=true;
    end else stateOkay:=false;
  end;

CONSTRUCTOR T_file.createToWrite(filename:string);
  begin
    try
      readMode:=false;
      assign(handle,filename);
      rewrite(handle);
      bufFill:=0;
      stateOkay:=true;
    except stateOkay:=false; end;
  end;
  
DESTRUCTOR  T_file.destroy;
  begin
    if not(readMode) then flushBuffer;
    close(handle);
  end;
  
FUNCTION    T_file.allOkay:boolean;  
  begin
    result:=stateOkay;
  end;

{$define macro_genericWrite:=
  begin
    if bufFill+sizeOf(x)>buffersize then flushBuffer;
    if stateOkay then move(x,buffer[bufFill],sizeOf(x));
    inc(bufFill,sizeOf(x));
  end}
{$define macro_genericRead:=
  begin
    if stateOkay then begin
      if bufFill<sizeOf(result) then readBuffer;
      stateOkay:=stateOkay and (bufFill>=sizeOf(result));
      move(buffer[0],result,sizeOf(result));
      move(buffer[sizeOf(result)],buffer[0],bufFill-sizeOf(result));
      dec(bufFill,sizeOf(result));    
    end;
  end}

PROCEDURE T_file.writeByte    (x:byte    ); macro_genericWrite;
FUNCTION  T_file. readByte      :byte     ; macro_genericRead;
PROCEDURE T_file.writeWord    (x:word    ); macro_genericWrite;
FUNCTION  T_file. readWord      :word     ; macro_genericRead;
PROCEDURE T_file.writeDWord   (x:dword   ); macro_genericWrite;
FUNCTION  T_file. readDWord     :dword    ; macro_genericRead;
PROCEDURE T_file.writeQWord   (x:qword   ); macro_genericWrite;
FUNCTION  T_file. readQWord     :qword    ; macro_genericRead;
PROCEDURE T_file.writeLongint (x:longint ); macro_genericWrite;
FUNCTION  T_file. readLongint   :longint  ; macro_genericRead;
PROCEDURE T_file.writeShortint(x:shortint); macro_genericWrite;
FUNCTION  T_file. readShortint  :shortint ; macro_genericRead;
PROCEDURE T_file.writeChar    (x:char    ); macro_genericWrite;
FUNCTION  T_file. readChar      :char     ; macro_genericRead;
PROCEDURE T_file.writeSingle  (x:single  ); macro_genericWrite;
FUNCTION  T_file. readSingle    :single   ; macro_genericRead;
PROCEDURE T_file.writeDouble  (x:double  ); macro_genericWrite;
FUNCTION  T_file. readDouble    :double   ; macro_genericRead;
PROCEDURE T_file.writeExtended(x:extended); macro_genericWrite;
FUNCTION  T_file. readExtended  :extended ; macro_genericRead;

PROCEDURE T_file.writeShortstring(x:shortstring);
  VAR i:longint;
  begin
    writeByte(length(x));
    for i:=1 to length(x) do writeChar(x[i]);
  end;

FUNCTION  T_file. readShortstring  :shortstring;
  VAR i:longint;
      charsToRead:byte;
  begin
    charsToRead:=readByte;
    result:='';
    for i:=1 to charsToRead do result:=result+readChar;
  end;

PROCEDURE T_file.writeAnsiString (x:ansistring);
  VAR i:longint;
  begin
    writeWord(length(x));
    for i:=1 to length(x) do writeChar(x[i]);
  end;

FUNCTION  T_file. readAnsiString   :ansistring;
  VAR i:longint;
      charsToRead:word;
  begin
    charsToRead:=readWord;
    result:='';
    for i:=1 to charsToRead do result:=result+readChar;
  end;

PROCEDURE T_file.writeBoolean(x:boolean);
  begin
    if x then writeByte(255) else writeByte(0);
  end;

FUNCTION  T_file.readBoolean:boolean;
  VAR b:byte;
  begin
    b:=readByte;
    result:=(b=255);
    stateOkay:=stateOkay and (b in [0,255]);
  end;

PROCEDURE T_file.writeBuf(p:PByte; psize:longint);
  begin
    if not(readMode) then begin
      flushBuffer;                 //write all that is stored in buffer
      blockwrite(handle,p^,psize); //write bytes from pointer
    end;
  end;

PROCEDURE T_file.readBuf (p:PByte; psize:longint);
  VAR actuallyRead:longint;
  begin
    if readmode then begin
      if bufFill>=pSize then begin //if buffer contains enough data...
        move(buffer[0],p^,pSize);                    //move data from buffer to pointer
        move(buffer[psize],buffer[0],bufFill-psize); //buffer shift
        dec(bufFill,psize);                          //decrement buffer-fill
      end else begin               //if buffer contains less than necessary...
        move(buffer[0],p^,bufFill);                                //read first bytes from buffer
        blockread(handle,(p+bufFill)^,pSize-bufFill,actuallyRead); //read remaining bytes from file
        stateOkay:=stateOkay and (actuallyRead=pSize-bufFill);     //check the number of sucessfully read bytes
        bufFill:=0;                                                //buffer is empty now
      end;
    end;
  end;

CONSTRUCTOR T_serializable.notReallyAConstructor; begin end;
FUNCTION    T_serializable.loadFromFile(filename:string):boolean;
  VAR ff:T_File;
  begin
    if fileExists(filename) then begin
      ff.createToRead(filename);
      result:=loadFromFile(ff);
      ff.destroy;
    end else result:=false;
  end;

PROCEDURE T_serializable.saveToFile(filename:string);
  VAR ff:T_File;
  begin
    ff.createToWrite(filename);
    saveToFile(ff);
    ff.destroy;
  end;


end.
