{$ifdef include_interface}
T_literalSerializer=object
  private
    useZStream,useReusableMap:boolean;
    location:T_tokenLocation;
    adapters:P_messages;

    streamWrapper:P_outputStreamWrapper;
    reusableMap:specialize G_literalKeyMap<longint>;
    stream:TStringStream;
  public
    CONSTRUCTOR create(CONST deflate,reuse:boolean; CONST location_:T_tokenLocation; CONST adapters_:P_messages);
    DESTRUCTOR destroy;

    FUNCTION variantByte:byte;
    PROCEDURE writeLiteral(CONST l:P_literal);
    FUNCTION getData:ansistring;
    PROCEDURE writeSerializedToStream(CONST outputStream: P_outputStreamWrapper);
    PROCEDURE raiseError(CONST message:string);
    PROPERTY wrappedRaw:P_outputStreamWrapper read streamWrapper;
end;

T_literalDeserializer=object
  private
    useZStream,useReusableMap:boolean;
    location:T_tokenLocation;
    adapters:P_messages;
    reusableLiterals:PP_literal;
    typeMap:T_typeMap;
    reusableFill:longint;
    streamWrapper:P_inputStreamWrapper;
  public
    CONSTRUCTOR create(CONST typeMap_:T_typeMap; CONST location_:T_tokenLocation; CONST adapters_:P_messages; CONST inputStream:P_inputStreamWrapper);
    DESTRUCTOR destroy;
    FUNCTION getLiteral:P_literal;
    PROCEDURE raiseError(CONST message:string);
    PROPERTY wrappedRaw:P_inputStreamWrapper read streamWrapper;
    FUNCTION getTypeCheck(OUT skipCustomCheck:boolean):P_typedef;
    PROPERTY getLocation:T_tokenLocation read location;
end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_literalDeserializer.create(CONST typeMap_: T_typeMap; CONST location_: T_tokenLocation; CONST adapters_: P_messages; CONST inputStream: P_inputStreamWrapper);
  VAR variant: byte;
      sourceStream: TStringStream;
      decompressor: TDecompressionStream;
      decompressed: TMemoryStream;
  begin
    typeMap:=typeMap_;
    location:=location_;
    adapters:=adapters_;
    variant:=inputStream^.readByte;
    case variant of
      248: begin useZStream:=false; useReusableMap:=false; end;
      249: begin useZStream:=true;  useReusableMap:=true;  end;
      250: begin useZStream:=true;  useReusableMap:=false; end;
      251: begin useZStream:=false; useReusableMap:=true;  end;
      else begin
        raiseError('Invalid literal encoding: '+intToStr(variant));
        inputStream^.logWrongTypeError;
        //Even if this fails, a graceful error handling must be possible afterwards.
        useZStream:=false;
        streamWrapper:=inputStream;
        exit;
      end;
    end;
    if useZStream then begin
      sourceStream:=TStringStream.create(inputStream^.readAnsiString);
      sourceStream.position:=0;
      decompressor:=TDecompressionStream.create(sourceStream,false);
      decompressed:=TMemoryStream.create;
      decompressed.copyFrom(decompressor,0);
      decompressed.position:=0;
      new(streamWrapper,create(decompressed));
      decompressor.free;
      sourceStream.free;
    end else streamWrapper:=inputStream;
    if useReusableMap then getMem(reusableLiterals,sizeOf(P_literal)*2097151);
    reusableFill:=0;
  end;

DESTRUCTOR T_literalDeserializer.destroy;
  begin
    if useZStream then dispose(streamWrapper,destroy);
    if useReusableMap then freeMem(reusableLiterals,sizeOf(P_literal)*2097151);
  end;

FUNCTION T_literalDeserializer.getLiteral: P_literal;
  VAR literalType:T_literalType;
      customTypeName:T_idString='';
      customType:P_typedef=nil;
      reusableIndex:longint=-1;
      listSize:longint;
      i:longint;
      mapKey,mapValue:P_literal;

  FUNCTION byteToType(CONST b:byte):T_literalType;
    begin
      result:=T_literalType(b shr 1);
    end;

  FUNCTION nextIntFromStream:P_abstractIntLiteral;
    VAR markerByte:byte;
        big:T_bigInt;
        small:longint;
    begin
      markerByte:=streamWrapper^.readByte;
      if markerByte>=253 then begin
        big.readFromStream(markerByte,streamWrapper);
        new(P_bigIntLiteral(result),create(big));
      end else begin
        small:=readLongintFromStream(markerByte,streamWrapper);
        result:=globalLiteralRecycler.newIntLiteral(small);
      end;
    end;

  FUNCTION typeStringOrNone(CONST t:T_literalType):string;
    begin
      if (t>=low(T_literalType)) and (t<=high(T_literalType)) then result:=C_typeInfo[t].name else result:='';
    end;

  begin
    if not(streamWrapper^.allOkay) then exit(newVoidLiteral);
    reusableIndex:=streamWrapper^.readNaturalNumber;
    if reusableIndex<=47 then begin
      literalType:=byteToType(reusableIndex);
      if odd(reusableIndex)
      then customTypeName:=streamWrapper^.readAnsiString
      else customTypeName:='';
    end
    else begin
      dec(reusableIndex,2*byte(high(T_literalType))+2);
      if (reusableIndex<reusableFill) then begin
        result:=reusableLiterals[reusableIndex];
        result^.rereference;
      end else begin
        result:=newVoidLiteral;
        streamWrapper^.logWrongTypeError;
        raiseError('Read invalid reuse index '+intToStr(reusableIndex)+'! Abort.');
      end;
      exit(result);
    end;
    case literalType of
      lt_boolean  : result:=boolLit[streamWrapper^.readBoolean].rereferenced;
      lt_smallint,lt_bigint: result:=nextIntFromStream;
      lt_real     : result:=globalLiteralRecycler.newRealLiteral  (streamWrapper^.readDouble    );
      lt_string   : result:=globalLiteralRecycler.newStringLiteral(streamWrapper^.readAnsiString,customTypeName<>'');
      lt_emptyList: result:=globalLiteralRecycler.newListLiteral;
      lt_emptySet : result:=globalLiteralRecycler.newSetLiteral(0);
      lt_emptyMap : result:=globalLiteralRecycler.newMapLiteral(0);
      lt_void     : result:=newVoidLiteral;
      lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList,
      lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet: begin
        listSize:=streamWrapper^.readNaturalNumber;
        if literalType in C_setTypes
        then result:=globalLiteralRecycler.newSetLiteral(listSize)
        else result:=globalLiteralRecycler.newListLiteral(listSize);
        case literalType of
          lt_booleanList,lt_booleanSet:
            for i:=0 to listSize-1 do if streamWrapper^.allOkay then P_collectionLiteral(result)^.appendBool(@globalLiteralRecycler,streamWrapper^.readBoolean);
          lt_intList,lt_intSet:
            for i:=0 to listSize-1 do if streamWrapper^.allOkay then P_collectionLiteral(result)^.append(@globalLiteralRecycler,nextIntFromStream,false);
          lt_realList,lt_realSet:
            for i:=0 to listSize-1 do if streamWrapper^.allOkay then P_collectionLiteral(result)^.appendReal(@globalLiteralRecycler,streamWrapper^.readDouble);
          lt_stringList,lt_stringSet:
            for i:=0 to listSize-1 do if streamWrapper^.allOkay then P_collectionLiteral(result)^.appendString(@globalLiteralRecycler,streamWrapper^.readAnsiString);
          else
            for i:=0 to listSize-1 do if streamWrapper^.allOkay then P_collectionLiteral(result)^.append(@globalLiteralRecycler,getLiteral(),false);
        end;
        if P_collectionLiteral(result)^.size<>listSize then raiseError('Invalid collection. Expected size of '+intToStr(listSize)+' but got '+result^.typeString);
      end;
      lt_map: begin
        result:=globalLiteralRecycler.newMapLiteral(0);
        listSize:=streamWrapper^.readNaturalNumber;
        for i:=0 to listSize-1 do if streamWrapper^.allOkay then begin
          mapKey  :=getLiteral();
          mapValue:=getLiteral();
          P_mapLiteral(result)^.dat.put(mapKey,mapValue);
        end;
        result^.literalType:=lt_map;
      end;
      else begin
        raiseError('Read invalid literal type '+typeStringOrNone(literalType)+' ('+intToStr(reusableIndex)+') ! Abort.');
        streamWrapper^.logWrongTypeError;
        exit(newVoidLiteral);
      end;
    end;
    if (result^.literalType<>literalType) then raiseError('Deserializaion result has other type ('+typeStringOrNone(result^.literalType)+') than expected ('+typeStringOrNone(literalType)+').');
    if customTypeName<>'' then begin
      if literalType in C_typables then begin
        if typeMap.containsKey(customTypeName,customType) then begin
          P_compoundLiteral(result)^.customType:=customType;
        end else raiseError('Read unknown custom type '+customTypeName);
      end else raiseError('Read invalid custom for literal of type '+C_typeInfo[literalType].name);
    end;

    if not(streamWrapper^.allOkay) then raiseError('Unknown error during deserialization.');

    if useReusableMap and (reusableFill<2097151) and ((literalType in [lt_string]) or (literalType in C_typables)) then begin
      reusableLiterals[reusableFill]:=result;
      inc(reusableFill);
    end;
  end;

PROCEDURE T_literalDeserializer.raiseError(CONST message: string);
  begin
    if adapters<>nil
    then adapters^.raiseSimpleError(message,location)
    else raise Exception.create    (message);
  end;

FUNCTION T_literalDeserializer.getTypeCheck(OUT skipCustomCheck:boolean): P_typedef;
  VAR typeName: string;
  begin
    typeName:=streamWrapper^.readAnsiString;
    if typeName='' then exit(nil);
    if typeMap.containsKey(typeName,result) then begin
      skipCustomCheck :=result^.isDucktyping and
                        result^.isAlwaysTrue;
    end else begin
      raiseError('Unknown custom type: '+typeName);
      streamWrapper^.logWrongTypeError;
    end;
  end;

CONSTRUCTOR T_literalSerializer.create(CONST deflate, reuse: boolean; CONST location_: T_tokenLocation; CONST adapters_: P_messages);
  begin
    useZStream:=deflate;;
    useReusableMap:=reuse;
    location:=location_;
    adapters:=adapters_;
    if useReusableMap then reusableMap.create();
    stream:=TStringStream.create();
    new(streamWrapper,create(stream));
  end;

FUNCTION T_literalSerializer.getData: ansistring;
  VAR outputStream: T_outputStreamWrapper;
      vDest: TStringStream;
  begin
    vDest:= TStringStream.create('');
    outputStream.create(vDest);
    writeSerializedToStream(@outputStream);
    vDest.position:=0;
    result:=vDest.DataString;
    outputStream.destroy;
  end;

PROCEDURE T_literalSerializer.writeSerializedToStream(CONST outputStream: P_outputStreamWrapper);
  VAR vCompressor: TCompressionStream;
      vDest: TStringStream;
  begin
    outputStream^.writeByte(variantByte);
    if useZStream then begin
      vDest   := TStringStream.create('');
      vCompressor := TCompressionStream.create(clMax, vDest,false);
      vCompressor.copyFrom(stream,0);
      vCompressor.free;
      outputStream^.writeAnsiString(vDest.DataString);
      vDest.free;
    end else outputStream^.write(stream.memory^,stream.size);
  end;

PROCEDURE T_literalSerializer.raiseError(CONST message: string);
  begin
    if adapters<>nil
    then adapters^.raiseSimpleError(message,location)
    else raise Exception.create    (message);
  end;

DESTRUCTOR T_literalSerializer.destroy;
  begin
    if useReusableMap then reusableMap.destroy;
    dispose(streamWrapper,destroy);
  end;

FUNCTION T_literalSerializer.variantByte: byte;
  begin
    if useReusableMap then begin
      if useZStream
      then result:=249
      else result:=251;
    end else begin
      if useZStream
      then result:=250
      else result:=248;
    end;
  end;

PROCEDURE T_literalSerializer.writeLiteral(CONST l: P_literal);
  VAR reusableIndex:longint;
      x:P_literal;
      iter:T_arrayOfLiteral;
      mapEntry: T_literalKeyLiteralValueMap.CACHE_ENTRY;
  FUNCTION typeByte:byte;
    begin
      result:=byte(L^.literalType)*2;
      if (L^.literalType in C_typables) and (P_typableLiteral(L)^.customType<>nil) then inc(result);
    end;

  begin
    if useReusableMap then begin
      reusableIndex:=reusableMap.get(L,2097151);
      if reusableIndex<2097151 then begin
        inc(reusableIndex,2*byte(high(T_literalType))+2);
        streamWrapper^.writeNaturalNumber(reusableIndex);
        exit;
      end;
    end;
    streamWrapper^.writeNaturalNumber(typeByte);
    if (L^.literalType in C_typables) and (P_typableLiteral(L)^.customType<>nil) then streamWrapper^.writeAnsiString(P_typableLiteral(L)^.customType^.name);
    case L^.literalType of
      lt_boolean : streamWrapper^.writeBoolean   (P_boolLiteral  (L)^.val);
      lt_smallint,lt_bigint  : P_abstractIntLiteral(L)^.writeToStream(streamWrapper);
      lt_real:   streamWrapper^.writeDouble    (P_realLiteral  (L)^.val);
      lt_string: streamWrapper^.writeAnsiString(P_stringLiteral(L)^.val);
      lt_booleanList,lt_booleanSet:begin
        streamWrapper^.writeNaturalNumber(P_compoundLiteral(L)^.size);
        iter:=P_collectionLiteral(L)^.tempIterableList;
        for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then streamWrapper^.writeBoolean(P_boolLiteral(x)^.val);
      end;
      lt_intList,lt_intSet:begin
        streamWrapper^.writeNaturalNumber(P_compoundLiteral(L)^.size);
        iter:=P_collectionLiteral(L)^.tempIterableList;
        for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then P_abstractIntLiteral(x)^.writeToStream(streamWrapper);
      end;
      lt_realList,lt_realSet:begin
        streamWrapper^.writeNaturalNumber(P_compoundLiteral(L)^.size);
        iter:=P_collectionLiteral(L)^.tempIterableList;
        for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then streamWrapper^.writeDouble(P_realLiteral(x)^.val);
      end;
      lt_stringList,lt_stringSet:begin
        streamWrapper^.writeNaturalNumber(P_compoundLiteral(L)^.size);
        iter:=P_collectionLiteral(L)^.tempIterableList;
        for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then streamWrapper^.writeAnsiString(P_stringLiteral(x)^.val);
      end;
      lt_void, lt_emptyList,lt_emptySet,lt_emptyMap: begin end; //completely defined by type
      lt_list,lt_set,
      lt_numList,lt_numSet:begin
        streamWrapper^.writeNaturalNumber(P_compoundLiteral(L)^.size);
        iter:=P_collectionLiteral(L)^.tempIterableList;
        for x in iter do if (adapters=nil) or (adapters^.continueEvaluation) then writeLiteral(x);
      end;
      lt_map: begin
        streamWrapper^.writeNaturalNumber(P_mapLiteral(L)^.size);
        for mapEntry in P_mapLiteral(L)^.dat.keyValueList do begin
          writeLiteral(mapEntry.key);
          writeLiteral(mapEntry.value);
        end;
      end;
      else begin
        if adapters<>nil then adapters^.raiseSimpleError('Cannot represent '+L^.typeString+' literal in binary form!',location)
                         else raise Exception.create    ('Cannot represent '+L^.typeString+' literal in binary form!');
      end;
    end;
    if useReusableMap and (reusableMap.fill<2097151) and ((L^.literalType in [lt_string]) or (L^.literalType in C_typables)) then
      reusableMap.putNew(L,reusableMap.fill,reusableIndex);
  end;
{$endif}
