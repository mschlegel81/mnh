UNIT datastores;
INTERFACE
USES sysutils,
     serializationUtil, myGenerics, myStringUtil,
     basicTypes,
     out_adapters,
     fileWrappers,
     recyclers,
     litVar,
     tokenArray,contexts;
TYPE
  P_datastoreMeta=^T_datastoreMeta;
  T_datastoreMeta=object
    private
      fileHasBinaryFormat:boolean;
      packagePath,ruleId,
      fileName:string;
      fileReadAt:double;
      FUNCTION newStoreName:string;
      {Returns true if new name is returned}
      FUNCTION tryObtainName(CONST createIfMissing:boolean):boolean;
    public
      CONSTRUCTOR create(CONST packagePath_,ruleId_:string);
      DESTRUCTOR destroy;
      FUNCTION fileChangedSinceRead:boolean;
      FUNCTION fileExists:boolean;
      FUNCTION readValue(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal;
      PROCEDURE writeValue(CONST L: P_literal; CONST location: T_tokenLocation; CONST threadLocalMessages: P_messages; CONST writePlainText:boolean);
  end;

FUNCTION isBinaryDatastore(CONST fileName:string; OUT dataAsStringList:T_arrayOfString):boolean;
IMPLEMENTATION
USES FileUtil;
VAR globalDatastoreCs:TRTLCriticalSection;

FUNCTION isBinaryDatastore(CONST fileName:string; OUT dataAsStringList:T_arrayOfString):boolean;
  VAR wrapper:T_bufferedInputStreamWrapper;
      id:string;
      literal:P_literal=nil;
      dummyLocation:T_tokenLocation;
      dummyTypeMap:T_typeMap;
  begin

    wrapper.createToReadFromFile(fileName);
    id:=wrapper.readAnsiString;
    result:=wrapper.allOkay;
    if result then begin
      try
        initialize(dummyLocation);
        dummyTypeMap.create();
        literal:=newLiteralFromStream(@wrapper,dummyLocation,nil,dummyTypeMap);
        dummyTypeMap.destroy;
        if wrapper.allOkay and (literal<>nil) then begin
          dataAsStringList:=id+':=';
          append(dataAsStringList,serializeToStringList(literal,dummyLocation,nil));
        end else result:=false;
      except
        result:=false;
      end;
      if literal<>nil then disposeLiteral(literal);
    end else dataAsStringList:=C_EMPTY_STRING_ARRAY;
    wrapper.destroy;
  end;

FUNCTION T_datastoreMeta.newStoreName:string;
  VAR i:longint;
  begin
    i:=0;
    repeat
      result:=ChangeFileExt(packagePath,'.datastore'+intToStr(i));
      inc(i);
    until not(sysutils.fileExists(result));
  end;

FUNCTION T_datastoreMeta.tryObtainName(CONST createIfMissing: boolean):boolean;
  VAR allStores:T_arrayOfString=();
      i:longint;
      wrapper:T_bufferedInputStreamWrapper;

  FUNCTION firstLineEquals(CONST fileName:string; CONST expectedFirstLine:string):boolean;
    VAR fileHandle:text;
        firstLine:string='';
    begin
      assign(fileHandle,fileName);
      try
        reset(fileHandle);
        readln(fileHandle,firstLine);
        close(fileHandle);
      except
        exit(false);
      end;
      result:=expectedFirstLine=firstLine;
    end;

  begin
    if fileName<>'' then exit(false);
    result:=false;
    enterCriticalSection(globalDatastoreCs);
    allStores:=find(ChangeFileExt(packagePath,'.datastore*'),true,false);
    for i:=0 to length(allStores)-1 do if fileName='' then begin
      wrapper.createToReadFromFile(allStores[i]);
      if ((wrapper.readAnsiString=ruleId) and wrapper.allOkay) then begin
        fileName:=allStores[i];
        fileHasBinaryFormat:=true;
      end;
      wrapper.destroy;
      if (fileName='') and firstLineEquals(allStores[i],ruleId+':=') then begin
        fileName:=allStores[i];
        fileHasBinaryFormat:=false;
      end;
    end;
    setLength(allStores,0);
    if (fileName='') and createIfMissing then begin
      fileName:=newStoreName;
      result:=true;
    end;
    leaveCriticalSection(globalDatastoreCs);
  end;

CONSTRUCTOR T_datastoreMeta.create(CONST packagePath_, ruleId_: string);
  begin
    packagePath:=packagePath_;
    ruleId:=ruleId_;
    fileName:='';
    fileReadAt:=0;
  end;

DESTRUCTOR T_datastoreMeta.destroy;
  begin
  end;

FUNCTION T_datastoreMeta.fileChangedSinceRead: boolean;
  VAR currentAge:double;
  begin
    tryObtainName(false);
    if fileName='' then exit(false);
    if fileReadAt=0 then exit(true);
    fileAge(fileName,currentAge);
    result:=currentAge<>fileReadAt;
  end;

FUNCTION T_datastoreMeta.fileExists:boolean;
  begin
    tryObtainName(false);
    result:=fileName<>'';
  end;

FUNCTION T_datastoreMeta.readValue(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler): P_literal;
  VAR wrapper:T_bufferedInputStreamWrapper;
      lexer:T_linesLexer;
      fileLines:T_arrayOfString;
      accessed:boolean;
      stmt:T_enhancedStatement;
      typeMap:T_typeMap;
  begin
    tryObtainName(false);
    if fileName='' then exit(nil);
    enterCriticalSection(globalDatastoreCs);
    if fileHasBinaryFormat then begin
      wrapper.createToReadFromFile(fileName);
      wrapper.readAnsiString;
      result:=nil;
      typeMap:=P_abstractPackage(location.package)^.getTypeMap;
      if wrapper.allOkay then result:=newLiteralFromStream(@wrapper,location,context.messages,typeMap);
      typeMap.destroy;
      if not(wrapper.allOkay) then begin
        if result<>nil then disposeLiteral(result);
        result:=nil;
      end;
      wrapper.destroy;
    end else begin
      fileLines:=fileWrappers.fileLines(fileName,accessed);
      if not(accessed) then result:=nil
      else begin
        dropFirst(fileLines,1);
        lexer.create(fileLines,location,P_abstractPackage(location.package));
        stmt:=lexer.getNextStatement(context.messages,recycler);
        stmt.token.first^.setSingleLocationForExpression(location);
        lexer.destroy;
        result:=context.reduceToLiteral(stmt.token.first,recycler).literal;
      end;
    end;
    fileAge(fileName,fileReadAt);
    leaveCriticalSection(globalDatastoreCs)
  end;

PROCEDURE T_datastoreMeta.writeValue(CONST L: P_literal; CONST location:T_tokenLocation; CONST threadLocalMessages: P_messages; CONST writePlainText:boolean);
  VAR wrapper:T_bufferedOutputStreamWrapper;
      plainText:T_arrayOfString;
      tempFileName:string;
      useTempFile:boolean;
  begin
    useTempFile:=not(tryObtainName(true));
    if useTempFile
    then tempFileName:=newStoreName
    else tempFileName:=fileName;

    enterCriticalSection(globalDatastoreCs);
    if writePlainText then begin
      plainText:=ruleId+':=';
      append(plainText,serializeToStringList(L,location,threadLocalMessages));
      writeFileLines(tempFileName,plainText,C_lineBreakChar,false);
    end else begin
      wrapper.createToWriteToFile(tempFileName);
      wrapper.writeAnsiString(ruleId);
      writeLiteralToStream(L,@wrapper,location,threadLocalMessages);
      wrapper.destroy;
    end;
    leaveCriticalSection(globalDatastoreCs);

    if useTempFile then begin
      if not(RenameFile(tempFileName,fileName))
      then begin
        if CopyFile(tempFileName,fileName,[cffOverwriteFile,cffPreserveTime],false)
        then DeleteFile(tempFileName);
      end;
    end;
  end;

INITIALIZATION
  initialize(globalDatastoreCs);
  system.initCriticalSection(globalDatastoreCs);

FINALIZATION
  system.doneCriticalSection(globalDatastoreCs);

end.

