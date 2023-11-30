UNIT funcs_fileStreams;

{$mode objfpc}{$H+}

INTERFACE

IMPLEMENTATION
USES subrules,basicTypes,contexts,litVar,out_adapters, Classes, fileWrappers, sysutils, mnh_constants, recyclers, funcs;
CONST FILE_BUFFER_SIZE = 1 shl 20;
TYPE

P_fileOutputStream=^T_fileOutputStream;

{ T_fileOutputStream }

T_fileOutputStream=object(T_expression)
    FUNCTION applyBuiltinFunction(CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; CONST threadContext: P_abstractContext; CONST recycler: pointer): P_expressionLiteral; virtual;
    FUNCTION arity: T_arityInfo; virtual;
    FUNCTION canApplyToNumberOfParameters(CONST parCount: longint): boolean; virtual;
    FUNCTION referencesAnyUserPackage: boolean; virtual;
    FUNCTION writeToStream(VAR serializer: T_literalSerializer): boolean; virtual;
  private
    streamCs    :TRTLCriticalSection;
    fileStream  : TFileStream;
    separator_: string;
    buffer:PByte;
    bufferFill:longint;

    PROCEDURE writeBytes(CONST Buf; CONST byteCount:longint);
    PROCEDURE flushBuffer;
  protected
    FUNCTION getParameterNames(CONST literalRecycler: P_literalRecycler): P_listLiteral; virtual;
  public
    CONSTRUCTOR create(CONST location:T_tokenLocation; CONST doAppend:boolean; CONST fileName,Separator:string);
    PROCEDURE cleanup(CONST literalRecycler: P_literalRecycler); virtual;
    DESTRUCTOR destroy; virtual;

    FUNCTION evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: pointer;  CONST parameters: P_listLiteral): T_evaluationResult; virtual;

  end;

{ T_fileOutputStream }

FUNCTION T_fileOutputStream.applyBuiltinFunction(CONST intrinsicRuleId: string; CONST funcLocation: T_tokenLocation; CONST threadContext: P_abstractContext; CONST recycler: pointer): P_expressionLiteral;
  begin
    threadContext^.raiseError('Cannot apply builtin function to file output stream.',funcLocation);
    result:=nil;
  end;

FUNCTION T_fileOutputStream.arity: T_arityInfo;
  begin
    result.minPatternLength:=1;
    result.maxPatternLength:=1;
  end;

FUNCTION T_fileOutputStream.canApplyToNumberOfParameters(CONST parCount: longint): boolean;
  begin
    result:=(parCount=1) or (parCount=0);
  end;

FUNCTION T_fileOutputStream.referencesAnyUserPackage: boolean;
  begin
    result:=false;
  end;

FUNCTION T_fileOutputStream.writeToStream(VAR serializer: T_literalSerializer): boolean;
  begin
    result:=false;
  end;

PROCEDURE T_fileOutputStream.writeBytes(CONST Buf; CONST byteCount: longint);
  begin
    if byteCount<=0 then exit;
    if bufferFill+byteCount>FILE_BUFFER_SIZE then flushBuffer;
    if byteCount>=FILE_BUFFER_SIZE then begin
      fileStream.write(Buf,byteCount);
    end else begin
      move(Buf,buffer[bufferFill],byteCount);
      bufferFill+=byteCount;
    end;
  end;

PROCEDURE T_fileOutputStream.flushBuffer;
  begin
    if bufferFill=0 then exit;
    fileStream.write(buffer,bufferFill);
    bufferFill:=0;
  end;

FUNCTION T_fileOutputStream.getParameterNames(CONST literalRecycler: P_literalRecycler): P_listLiteral;
  begin
    result:=P_listLiteral(literalRecycler^.newListLiteral(1)^.appendString(literalRecycler,'x'));
  end;

CONSTRUCTOR T_fileOutputStream.create(CONST location: T_tokenLocation; CONST doAppend: boolean; CONST fileName,Separator:string);
  begin
    inherited create(et_builtinStateful,location);
    initCriticalSection(streamCs);
    separator_:=Separator;
    ensurePath(fileName);
    if doAppend and fileExists(fileName)
    then begin
      fileStream:=TFileStream.create(fileName,fmOpenReadWrite or fmShareDenyWrite);
      fileStream.Seek(0, soEnd);
    end else fileStream:=TFileStream.create(fileName,fmCreate);
    getMem(buffer,FILE_BUFFER_SIZE);
    bufferFill:=0;
   end;

PROCEDURE T_fileOutputStream.cleanup(CONST literalRecycler: P_literalRecycler);
  begin
    if fileStream=nil then exit;
    enterCriticalSection(streamCs);
    try
      flushBuffer;
      freeMem(buffer,FILE_BUFFER_SIZE);
      fileStream.destroy;
    except end;
    fileStream:=nil;
    leaveCriticalSection(streamCs);
  end;

DESTRUCTOR T_fileOutputStream.destroy;
  begin
    doneCriticalSection(streamCs);
  end;

FUNCTION T_fileOutputStream.evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: pointer; CONST parameters: P_listLiteral): T_evaluationResult;
  VAR i, k:longint;
      dbl:double;
      L: P_literal;
  begin
    enterCriticalSection(streamCs);
    result.reasonForStop:=rr_ok;
    result.literal :=newVoidLiteral;
    try
      if (parameters=nil) then begin
        if separator_<>'' then writeBytes(separator_[1],length(separator_));
      end else for i:=0 to parameters^.size-1 do begin
        L:=parameters^.value[i];
        case L^.literalType of
          lt_boolean : writeBytes(P_boolLiteral(L)^.value,1);
          lt_smallint: begin
            k:=P_smallIntLiteral(L)^.value;
            if (k>=0) and (k<=255)
            then writeBytes(k,1)
            else context^.raiseError('Integer '+L^.toString+' cannot be interpreted as a byte - must be a value between 0 and 255',location);
          end;
          lt_real    : begin
            dbl:=P_realLiteral(L)^.value;
            writeBytes(dbl,sizeOf(dbl));
          end;
          lt_string  : writeBytes(P_stringLiteral(L)^.value[1],length(P_stringLiteral(L)^.value));
          else context^.raiseError('Cannot write literals of type '+L^.typeString+' to a file stream',location);
        end;
        if separator_<>'' then writeBytes(separator_[1],length(separator_));
      end;
    except
      on e:Exception do begin
        context^.raiseError('A fatal error ocurred trying to write to file: '+e.message,location);
        result.reasonForStop:=rr_fail;
      end;
    end;
    leaveCriticalSection(streamCs);
  end;

{$i func_defines.inc}
FUNCTION new_outputFileStream intFuncSignature;
  VAR outputStream:P_fileOutputStream;
      fileName:string;
      append:boolean=false;
      Separator:string='';
      i: integer;
  begin
    if (params=nil) or (params^.size<1) or (params^.size>3) or (arg0^.literalType<>lt_string) then exit(nil);
    fileName:=str0^.value;
    for i:=1 to params^.size-1 do begin
      case params^.value[i]^.literalType of
        lt_boolean: append   :=P_boolLiteral(params^.value[i])^.value;
        lt_string : Separator:=P_stringLiteral(params^.value[i])^.value;
        else exit(nil);
      end;
    end;
    result:=nil;
    try
      new(outputStream,create(tokenLocation,append,fileName,Separator));
      result:=outputStream;
    except
      on e:Exception do begin
        context^.raiseError('A fatal error ocurred trying to write to file: '+e.message,tokenLocation);
        result:=nil;
      end;
    end;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(FILES_BUILTIN_NAMESPACE,'newFileOutputStream',@new_outputFileStream,ak_variadic_1);
end.

