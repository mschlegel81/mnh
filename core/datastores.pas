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
  T_writeTempRequest=record
    meta:P_datastoreMeta;
    L: P_literal;
    location: T_tokenLocation;
    writePlainText:boolean;
  end;

  T_writeTempResult=record
    useTempFile,finishedOk:boolean;
    tempFileName,fileName:string;
  end;

  { T_datastoreFlush }

  T_datastoreFlush=object
    private
      pendingWrites:array of T_writeTempRequest;
    public
      CONSTRUCTOR create;
      PROCEDURE addStoreToFlush(CONST meta:P_datastoreMeta; CONST L: P_literal; CONST location: T_tokenLocation; CONST writePlainText:boolean);
      PROCEDURE finalize(CONST threadLocalMessages: P_messages; CONST recycler:P_literalRecycler);
      DESTRUCTOR destroy;
  end;

  T_datastoreMeta=object
    private
      fileHasBinaryFormat:boolean;
      packagePath,ruleId,
      fileName:string;
      fileReadAt:double;
      FUNCTION newStoreName:string;
      {Returns true if new name is returned}
      FUNCTION tryObtainName(CONST createIfMissing:boolean; CONST enforcedName:string=''):boolean;
      FUNCTION writeValue(CONST L: P_literal; CONST location: T_tokenLocation; CONST threadLocalMessages: P_messages; CONST writePlainText:boolean; CONST recycler:P_literalRecycler):T_writeTempResult;
    public
      CONSTRUCTOR create(CONST packagePath_,ruleId_:string);
      DESTRUCTOR destroy;
      FUNCTION fileChangedSinceRead:boolean;
      FUNCTION fileExists:boolean;
      FUNCTION readFromSpecificFileIncludingId(CONST fname:string; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
      FUNCTION readValue(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  end;

{$ifdef fullVersion}
FUNCTION isBinaryDatastore(CONST fileName:string; OUT dataAsStringList:T_arrayOfString):boolean;
{$endif}
IMPLEMENTATION
USES FileUtil;
VAR globalDatastoreCs:TRTLCriticalSection;
CONST TEMP_FILE_SUFFIX='_tmp';
{$ifdef fullVersion}
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
      if literal<>nil then globalLiteralRecycler.disposeLiteral(literal);
    end else dataAsStringList:=C_EMPTY_STRING_ARRAY;
    wrapper.destroy;
  end;
{$endif}

{ T_datastoreFlush }

CONSTRUCTOR T_datastoreFlush.create;
  begin
    setLength(pendingWrites,0);
  end;

PROCEDURE T_datastoreFlush.addStoreToFlush(CONST meta: P_datastoreMeta; CONST L: P_literal; CONST location: T_tokenLocation; CONST writePlainText: boolean);
  begin
    setLength(pendingWrites,length(pendingWrites)+1);
    pendingWrites[length(pendingWrites)-1].meta:=meta;
    pendingWrites[length(pendingWrites)-1].L:=L;
    pendingWrites[length(pendingWrites)-1].location:=location;
    pendingWrites[length(pendingWrites)-1].writePlainText:=writePlainText;
  end;

PROCEDURE moveSafely(CONST source,dest:string);
  begin
    if not(RenameFile(source,dest))
    then begin
      if CopyFile(source,dest,[cffOverwriteFile,cffPreserveTime],false)
      then DeleteFile(source);
    end;
  end;

PROCEDURE T_datastoreFlush.finalize(CONST threadLocalMessages: P_messages;
  CONST recycler: P_literalRecycler);
  VAR i:longint;
      rename:T_writeTempResult;
      pendingRenames:array of T_writeTempResult;
      allOk:boolean=true;
  begin
    enterCriticalSection(globalDatastoreCs);
    try
      setLength(pendingRenames,length(pendingWrites));
      for i:=0 to length(pendingWrites)-1 do with pendingWrites[i] do begin
        if threadLocalMessages^.continueEvaluation
        then begin
          pendingRenames[i]:=meta^.writeValue(L,location,threadLocalMessages,writePlainText,recycler);
          allOk:=allOk and pendingRenames[i].finishedOk;
        end
        else pendingRenames[i].tempFileName:='';
        recycler^.disposeLiteral(L);
      end;
      if allOk then begin
        for rename in pendingRenames do if rename.useTempFile then moveSafely(rename.tempFileName,rename.fileName);
      end else begin
        for rename in pendingRenames do if rename.tempFileName<>'' then DeleteFile(rename.tempFileName);
      end;
    finally
      leaveCriticalSection(globalDatastoreCs);
    end;
  end;

DESTRUCTOR T_datastoreFlush.destroy;
  begin
    setLength(pendingWrites,0);
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

FUNCTION T_datastoreMeta.tryObtainName(CONST createIfMissing: boolean; CONST enforcedName:string):boolean;
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
      if enforcedName=''
      then result:=expectedFirstLine=firstLine
      else begin
        ruleId:=copy(firstLine,1,length(firstLine)-2);
        result:=(firstLine=ruleId+':=') and isIdentifier(ruleId,false);
      end;
    end;

  begin
    if fileName<>'' then exit(false);
    result:=false;
    enterCriticalSection(globalDatastoreCs);
    if enforcedName='' then begin
      allStores:=find(ChangeFileExt(packagePath,'.datastore*'),true,false);
      sort(allStores);
    end else allStores:=enforcedName;
    for i:=0 to length(allStores)-1 do if fileName='' then begin
      wrapper.createToReadFromFile(allStores[i]);
      if enforcedName='' then begin
        if (wrapper.readAnsiString<>ruleId)
        then wrapper.logWrongTypeError;
      end else begin
        ruleId:=wrapper.readAnsiString;
      end;
      if (wrapper.allOkay) then begin
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
    if enforcedName='' then begin
      if (fileName='') and createIfMissing then begin
        fileName:=newStoreName;
        result:=true;
      end else if (fileName<>'') and (copy(fileName,length(fileName)-length(TEMP_FILE_SUFFIX),length(TEMP_FILE_SUFFIX))=TEMP_FILE_SUFFIX) then begin
        moveSafely(fileName,copy(fileName,1,length(fileName)-length(TEMP_FILE_SUFFIX)));
        fileName:=copy(fileName,1,length(fileName)-length(TEMP_FILE_SUFFIX));
      end;
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
    if fileName='' then exit(false); //file is not known -> neither read nor written
    if fileReadAt=0 then exit(true); //file was never read
    fileAge(fileName,currentAge);
    result:=currentAge<>fileReadAt;
  end;

FUNCTION T_datastoreMeta.fileExists:boolean;
  begin
    tryObtainName(false);
    result:=fileName<>'';
  end;

FUNCTION T_datastoreMeta.readValue(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): P_literal;
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
      if wrapper.allOkay then result:=newLiteralFromStream(@wrapper,location,context^.messages,typeMap);
      typeMap.destroy;
      if not(wrapper.allOkay) then begin
        if result<>nil then recycler^.disposeLiteral(result);
        result:=nil;
      end;
      wrapper.destroy;
    end else begin
      fileLines:=fileWrappers.fileLines(fileName,accessed);
      if not(accessed) then result:=nil
      else begin
        dropFirst(fileLines,1);
        lexer.create(fileLines,location,P_abstractPackage(location.package));
        stmt:=lexer.getNextStatement(context^.messages,recycler);
        stmt.token.first^.setSingleLocationForExpression(location);
        lexer.destroy;
        result:=context^.reduceToLiteral(stmt.token.first,recycler).literal;
      end;
    end;
    fileAge(fileName,fileReadAt);
    leaveCriticalSection(globalDatastoreCs)
  end;

FUNCTION T_datastoreMeta.readFromSpecificFileIncludingId(CONST fname:string; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  VAR contentLiteral:P_literal=nil;
  begin
    if sysutils.fileExists(fname) then begin
      tryObtainName(false,fname);
      if fileName='' then exit(newVoidLiteral);
      contentLiteral:=readValue(location,context,recycler);
      if contentLiteral=nil then exit(newVoidLiteral);
      result:=newMapLiteral(2)
        ^.put(recycler,'id',ruleId)
        ^.put(recycler,'content',contentLiteral,false);
    end else result:=newVoidLiteral;
  end;

FUNCTION T_datastoreMeta.writeValue(CONST L: P_literal; CONST location:T_tokenLocation; CONST threadLocalMessages: P_messages; CONST writePlainText:boolean; CONST recycler:P_literalRecycler):T_writeTempResult;
  VAR wrapper:T_bufferedOutputStreamWrapper;
      plainText:T_arrayOfString;
  begin
    result.useTempFile:=not(tryObtainName(true));
    if result.useTempFile
    then result.tempFileName:=fileName+TEMP_FILE_SUFFIX
    else result.tempFileName:=fileName;
    result.fileName:=fileName;

    if writePlainText then begin
      plainText:=ruleId+':=';
      append(plainText,serializeToStringList(L,location,threadLocalMessages));
      writeFileLines(result.tempFileName,plainText,C_lineBreakChar,false);
      result.finishedOk:=threadLocalMessages^.continueEvaluation;
    end else begin
      wrapper.createToWriteToFile(result.tempFileName);
      wrapper.writeAnsiString(ruleId);
      writeLiteralToStream(L,location,threadLocalMessages,true,false,@wrapper);
      result.finishedOk:=threadLocalMessages^.continueEvaluation and wrapper.allOkay;
      wrapper.destroy;
    end;
  end;

INITIALIZATION
  initialize(globalDatastoreCs);
  system.initCriticalSection(globalDatastoreCs);

FINALIZATION
  system.doneCriticalSection(globalDatastoreCs);

end.

