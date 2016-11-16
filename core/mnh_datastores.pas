UNIT mnh_datastores;
INTERFACE
USES mnh_litVar,mnh_basicTypes,mnh_out_adapters, serializationUtil,myGenerics,mnh_fileWrappers,sysutils;
TYPE
  P_datastoreMeta=^T_datastoreMeta;
  T_datastoreMeta=object
    private
      packagePath,ruleId,
      fileName:string;
      fileReadAt:double;
      PROCEDURE tryObtainName(CONST createIfMissing:boolean);
    public
      CONSTRUCTOR create(CONST packagePath_,ruleId_:string);
      DESTRUCTOR destroy;
      FUNCTION fileChangedSinceRead:boolean;
      FUNCTION readValue(CONST location:T_tokenLocation; CONST adapters:P_adapters):P_literal;
      PROCEDURE writeValue(CONST L: P_literal; CONST location: T_tokenLocation;
        CONST adapters: P_adapters);
  end;

IMPLEMENTATION

PROCEDURE T_datastoreMeta.tryObtainName(CONST createIfMissing: boolean);
  VAR allStores:T_arrayOfString;
      i:longint;
      wrapper:T_streamWrapper;
  begin
    if fileName<>'' then exit;
    allStores:=find(ChangeFileExt(packagePath,'.datastore*'),true,false);
    for i:=0 to length(allStores)-1 do if fileName='' then begin
      wrapper.createToReadFromFile(allStores[i]);
      if (wrapper.readAnsiString=ruleId) and wrapper.allOkay then fileName:=allStores[i];
      wrapper.destroy;
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

FUNCTION T_datastoreMeta.readValue(CONST location:T_tokenLocation; CONST adapters:P_adapters): P_literal;
  VAR wrapper:T_streamWrapper;
  begin
    tryObtainName(false);
    {$ifdef debugMode} writeln(stdErr,'Reading datastore ',fileName,' for rule ',ruleId); {$endif}
    if fileName='' then exit(newVoidLiteral);
    wrapper.createToReadFromFile(fileName);
    wrapper.readAnsiString;
    result:=newLiteralFromStream(wrapper,location,adapters);
    if not(wrapper.allOkay) then begin
      if result<>nil then disposeLiteral(result);
      result:=nil;
    end;
    wrapper.destroy;
    fileAge(fileName,fileReadAt);
  end;

PROCEDURE T_datastoreMeta.writeValue(CONST L: P_literal; CONST location:T_tokenLocation; CONST adapters:P_adapters);
  VAR wrapper:T_streamWrapper;
  begin
    tryObtainName(true);
    {$ifdef debugMode} writeln(stdErr,'Writing datastore ',fileName,' for rule ',ruleId); {$endif}
    wrapper.createToWriteToFile(fileName);
    wrapper.writeAnsiString(ruleId);
    writeLiteralToStream(L,wrapper,location,adapters);
    wrapper.destroy;
  end;

end.

