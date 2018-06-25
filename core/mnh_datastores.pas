UNIT mnh_datastores;
INTERFACE
USES sysutils,
     serializationUtil, myGenerics, myStringUtil,
     mnh_basicTypes,
     mnh_out_adapters, mnh_fileWrappers,
     mnh_litVar,
     mnh_tokenArray,mnh_contexts;
TYPE
  P_datastoreMeta=^T_datastoreMeta;
  T_datastoreMeta=object
    private
      fileHasBinaryFormat:boolean;
      packagePath,ruleId,
      fileName:string;
      fileReadAt:double;
      PROCEDURE tryObtainName(CONST createIfMissing:boolean);
    public
      CONSTRUCTOR create(CONST packagePath_,ruleId_:string);
      DESTRUCTOR destroy;
      FUNCTION fileChangedSinceRead:boolean;
      FUNCTION readValue(CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal;
      PROCEDURE writeValue(CONST L: P_literal; CONST location: T_tokenLocation; CONST adapters: P_messageConnector; CONST writePlainText:boolean);
  end;

FUNCTION isBinaryDatastore(CONST fileName:string; OUT dataAsStringList:T_arrayOfString):boolean;
IMPLEMENTATION

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

PROCEDURE T_datastoreMeta.tryObtainName(CONST createIfMissing: boolean);
  VAR allStores:T_arrayOfString;
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
    if fileName<>'' then exit;
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
    if (fileName='') and createIfMissing then begin
      i:=0;
      repeat
        fileName:=ChangeFileExt(packagePath,'.datastore'+intToStr(i));
        inc(i);
      until not(fileExists(fileName));
    end;
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

FUNCTION T_datastoreMeta.readValue(CONST location:T_tokenLocation; VAR context:T_threadContext): P_literal;
  VAR wrapper:T_bufferedInputStreamWrapper;
      lexer:T_lexer;
      fileLines:T_arrayOfString;
      accessed:boolean;
      stmt:T_enhancedStatement;
      typeMap:T_typeMap;
  begin
    tryObtainName(false);
    if fileName='' then exit(nil);
    if fileHasBinaryFormat then begin
      wrapper.createToReadFromFile(fileName);
      wrapper.readAnsiString;
      result:=nil;
      typeMap:=P_abstractPackage(location.package)^.getTypeMap;
      if wrapper.allOkay then result:=newLiteralFromStream(@wrapper,location,context.adapters,typeMap);
      typeMap.destroy;
      if not(wrapper.allOkay) then begin
        if result<>nil then disposeLiteral(result);
        result:=nil;
      end;
      wrapper.destroy;
    end else begin
      result:=newVoidLiteral;
      fileLines:=mnh_fileWrappers.fileLines(fileName,accessed);
      if not(accessed) then exit(newVoidLiteral);
      dropFirst(fileLines,1);
      lexer.create(fileLines,location,P_abstractPackage(location.package));
      stmt:=lexer.getNextStatement(context.recycler,context.adapters^{$ifdef fullVersion},nil{$endif});
      stmt.firstToken^.setSingleLocationForExpression(location);
      lexer.destroy;
      result:=context.reduceToLiteral(stmt.firstToken).literal;
    end;
    fileAge(fileName,fileReadAt);
  end;

PROCEDURE T_datastoreMeta.writeValue(CONST L: P_literal; CONST location:T_tokenLocation; CONST adapters:P_messageConnector; CONST writePlainText:boolean);
  VAR wrapper:T_bufferedOutputStreamWrapper;
      plainText:T_arrayOfString;
  begin
    tryObtainName(true);
    if writePlainText then begin
      plainText:=ruleId+':=';
      append(plainText,serializeToStringList(L,location,adapters));
      writeFileLines(fileName,plainText,C_lineBreakChar,false);
    end else begin
      wrapper.createToWriteToFile(fileName);
      wrapper.writeAnsiString(ruleId);
      writeLiteralToStream(L,@wrapper,location,adapters);
      wrapper.destroy;
    end;
  end;

end.

