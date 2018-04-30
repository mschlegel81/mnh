UNIT tokenStack;
INTERFACE
USES sysutils,
     myGenerics,
     //MNH:
     mnh_constants,
     mnh_basicTypes,
     mnh_out_adapters,
     mnh_litVar,
     mnh_tokens
     {$ifdef fullVersion},
     mnh_profiling,
     mnh_debuggingVar
     {$endif};
TYPE
  P_tokenStack=^T_TokenStack;
  T_TokenStack=object
    alloc,
    topIndex:longint;
    dat:PP_token;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE popDestroy(VAR recycler:T_tokenRecycler);
    PROCEDURE popLink(VAR first:P_token);
    PROCEDURE push(VAR first:P_token);
    PROCEDURE quietPush(CONST first:P_token);
    PROCEDURE quietPop;
    FUNCTION topType:T_tokenType;
    FUNCTION hasTokenTypeAnywhere(CONST t:T_tokenType):boolean;
    FUNCTION toString(CONST first:P_token; CONST lengthLimit:longint=maxLongint):ansistring;
    {$ifdef fullVersion}
    FUNCTION toDebuggerString(CONST first:P_token; CONST lengthLimit:longint;
                              CONST parameterVar,localVar,globalVar,inlineVar:P_variableTreeEntryCategoryNode):T_arrayOfString;
    {$endif}
  end;

  {$ifdef fullVersion}
  T_callStackEntry=record
    callLocation:T_tokenLocation;
    calleeId:ansistring;
    calleeLocation:T_tokenLocation;
    timeForProfiling_inclusive,
    timeForProfiling_exclusive:double;
    parameters:P_variableTreeEntryCategoryNode;
  end;

  P_callStack=^T_callStack;
  T_callStack=object
    private
      dat:array of T_callStackEntry;
      fill:longint;
      FUNCTION getEntry(CONST index:longint):T_callStackEntry;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROPERTY size:longint read fill;
      PROCEDURE clear;
      PROCEDURE push(CONST wallclockTime:double;
                     CONST parameters:P_variableTreeEntryCategoryNode;
                     CONST callerLocation: T_tokenLocation;
                     CONST callee: P_objectWithIdAndLocation);
      FUNCTION pop(CONST wallclockTime:double;CONST profiler:P_profiler):T_tokenLocation;
      PROCEDURE print(VAR adapters:T_adapters);
      PROPERTY entry[index:longint]:T_callStackEntry read getEntry; default;
  end;

  T_localIdInfo=record
                  name:string;
                  validFrom,validUntil:T_tokenLocation;
                  tokenType:T_tokenType;
                end;

  P_localIdInfos=^T_localIdInfos;
  T_localIdInfos=object
    private
      infos: array of T_localIdInfo;
      blobLines:array of record
        lineIndex:longint;
        blobCloser:char;
      end;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION localTypeOf(CONST id:T_idString; CONST line,col:longint; OUT declaredAt:T_tokenLocation):T_tokenType;
      FUNCTION allLocalIdsAt(CONST line,col:longint):T_arrayOfString;
      PROCEDURE add(CONST id:T_idString; CONST validFrom,validUntil:T_tokenLocation; CONST typ:T_tokenType);
      PROCEDURE markBlobLine(CONST lineIndex:longint; CONST closer:char);
      FUNCTION getBlobCloserOrZero(CONST lineIndex:longint):char;
      PROCEDURE clear;
  end;
  {$endif}

  T_idStack=object
    ids:array of array of record name:T_idString; used:boolean; location:T_tokenLocation end;
    {$ifdef fullVersion}
    localIdInfos:P_localIdInfos;
    {$endif}

    CONSTRUCTOR create({$ifdef fullVersion}CONST info:P_localIdInfos{$endif});
    DESTRUCTOR destroy;
    PROCEDURE clear;
    PROCEDURE scopePush;
    PROCEDURE scopePop(VAR adapters:T_adapters; CONST location:T_tokenLocation);
    FUNCTION oneAboveBottom:boolean;
    FUNCTION scopeBottom:boolean;
    FUNCTION addId(CONST id:T_idString; CONST location:T_tokenLocation):boolean;
    FUNCTION hasId(CONST id:T_idString):boolean;
  end;

IMPLEMENTATION
{$ifdef fullVersion}
CONSTRUCTOR T_localIdInfos.create;
  begin clear; end;

DESTRUCTOR T_localIdInfos.destroy;
  begin clear; end;

FUNCTION T_localIdInfos.localTypeOf(CONST id: T_idString; CONST line, col: longint; OUT declaredAt: T_tokenLocation): T_tokenType;
  VAR entry:T_localIdInfo;
  begin
    result:=tt_literal;
    for entry in infos do
      if (entry.name=id) and
         not(positionIsBeforeLocation    (line,col,entry.validFrom)) and
             positionIsBeforeOrAtLocation(line,col,entry.validUntil) then begin
        declaredAt:=entry.validFrom;
        exit(entry.tokenType);
      end;
  end;

FUNCTION T_localIdInfos.allLocalIdsAt(CONST line,col:longint):T_arrayOfString;
  VAR entry:T_localIdInfo;
  begin
    setLength(result,0);
    for entry in infos do
      if not(positionIsBeforeLocation    (line,col,entry.validFrom)) and
             positionIsBeforeOrAtLocation(line,col,entry.validUntil) then begin
        append(result,entry.name);
      end;
  end;

PROCEDURE T_localIdInfos.add(CONST id:T_idString; CONST validFrom,validUntil:T_tokenLocation; CONST typ:T_tokenType);
  VAR i:longint;
  begin
    i:=length(infos);
    setLength(infos,i+1);
    infos[i].name      :=id;
    infos[i].validFrom :=validFrom;
    infos[i].validUntil:=validUntil;
    infos[i].tokenType :=typ;
  end;

PROCEDURE T_localIdInfos.markBlobLine(CONST lineIndex:longint; CONST closer:char);
  VAR k:longint;
  begin
    k:=length(blobLines);
    setLength(blobLines,k+1);
    blobLines[k].lineIndex :=lineIndex;
    blobLines[k].blobCloser:=closer;
  end;

FUNCTION T_localIdInfos.getBlobCloserOrZero(CONST lineIndex:longint):char;
  VAR k:longint;
  begin
    result:=#0;
    for k:=0 to length(blobLines)-1 do if blobLines[k].lineIndex=lineIndex then exit(blobLines[k].blobCloser);
  end;

PROCEDURE T_localIdInfos.clear;
  begin
    setLength(infos,0);
    setLength(blobLines,0);
  end;

CONSTRUCTOR T_callStack.create;
  begin
    setLength(dat,0);
    fill:=0;
  end;

DESTRUCTOR T_callStack.destroy;
  begin

  end;

PROCEDURE T_callStack.clear;
  begin
    while fill>0 do pop(0,nil);
  end;

PROCEDURE T_callStack.push(CONST wallclockTime:double; CONST parameters:P_variableTreeEntryCategoryNode;
  CONST callerLocation: T_tokenLocation;
  CONST callee: P_objectWithIdAndLocation);
  begin
    if length(dat)<=fill then setLength(dat,length(dat)+16);
    dat[fill].callLocation:=callerLocation;
    dat[fill].calleeId:=callee^.getId;
    dat[fill].calleeLocation:=callee^.getLocation;

    if fill>0 then with dat[fill-1] do timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
    dat[fill].timeForProfiling_exclusive:=wallclockTime;
    dat[fill].timeForProfiling_inclusive:=wallclockTime;
    dat[fill].parameters:=parameters;

    inc(fill);
  end;

FUNCTION T_callStack.pop(CONST wallclockTime: double;CONST profiler:P_profiler):T_tokenLocation;
  begin
    initialize(result);
    if fill<1 then begin
      result.package:=nil;
      result.column:=-1;
      result.line:=-1;
      exit(result);
    end;
    if profiler<>nil then begin
      with dat[fill-1] do begin
        timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
        timeForProfiling_inclusive:=wallclockTime-timeForProfiling_inclusive;
        profiler^.add(calleeId,calleeLocation,timeForProfiling_inclusive, timeForProfiling_exclusive);
      end;
      if fill>1 then with dat[fill-2] do timeForProfiling_exclusive:=wallclockTime-timeForProfiling_exclusive;
    end;
    if dat[fill-1].parameters<>nil then begin
      dispose(dat[fill-1].parameters,destroy);
      dat[fill-1].parameters:=nil;
      result:=dat[fill-1].callLocation;
    end;
    dec(fill);
  end;

PROCEDURE T_callStack.print(VAR adapters:T_adapters);
  VAR i:longint;
  begin
    for i:=fill-1 downto 0 do with dat[i] do
    adapters.logCallStackInfo(calleeId,callLocation);
  end;

FUNCTION T_callStack.getEntry(CONST index:longint):T_callStackEntry;
  begin
    result:=dat[index];
  end;
{$endif}

CONSTRUCTOR T_TokenStack.create;
  begin
    alloc:=8;
    getMem(dat,alloc*sizeOf(P_token));
    topIndex:=-1;
  end;

DESTRUCTOR T_TokenStack.destroy;
  begin
    freeMem(dat,alloc*sizeOf(P_token));
  end;

PROCEDURE T_TokenStack.popDestroy(VAR recycler:T_tokenRecycler);
  begin
    recycler.disposeToken(dat[topIndex]);
    dec(topIndex);
  end;

PROCEDURE T_TokenStack.popLink(VAR first: P_token);
  begin
    dat[topIndex]^.next:=first;
    first:=dat[topIndex];
    dec(topIndex);
  end;

PROCEDURE T_TokenStack.push(VAR first: P_token);
  begin
    inc(topIndex);
    if topIndex>=alloc then begin
      inc(alloc,alloc);
      ReAllocMem(dat,alloc*sizeOf(P_token));
    end;
    dat[topIndex]:=first;
    first:=first^.next;
  end;

PROCEDURE T_TokenStack.quietPush(CONST first:P_token);
  begin
    inc(topIndex);
    if topIndex>=alloc then begin
      inc(alloc,alloc);
      ReAllocMem(dat,alloc*sizeOf(P_token));
    end;
    dat[topIndex]:=first;
  end;

PROCEDURE T_TokenStack.quietPop;
  begin
    if topIndex>=0 then dec(topIndex);
  end;

FUNCTION T_TokenStack.topType:T_tokenType;
  begin
    if topIndex>=0 then result:=dat[topIndex]^.tokType
                   else result:=tt_EOL;
  end;

FUNCTION T_TokenStack.hasTokenTypeAnywhere(CONST t:T_tokenType):boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=topIndex downto 0 do if dat[i]^.tokType=t then exit(true);
  end;

FUNCTION T_TokenStack.toString(CONST first: P_token; CONST lengthLimit: longint): ansistring;
  VAR i0,i:longint;
      prevWasIdLike:boolean=false;
  begin
    if topIndex>=0 then begin
      i0:=topIndex;
      result:='';
      while (i0>0) and (length(result)<lengthLimit) do begin
        result:=dat[i0]^.toString(prevWasIdLike,prevWasIdLike,lengthLimit-length(result))+result;
        dec(i0);
      end;
      if i0>0 then result:='... '
              else result:='';
      prevWasIdLike:=false;
      for i:=i0 to topIndex do result:=result+dat[i]^.toString(prevWasIdLike,prevWasIdLike);
    end else result:='';
    result:=result+' § '+tokensToString(first,lengthLimit);
  end;

{$ifdef fullVersion}
FUNCTION T_TokenStack.toDebuggerString(CONST first:P_token; CONST lengthLimit:longint;
                                       CONST parameterVar,localVar,globalVar,inlineVar:P_variableTreeEntryCategoryNode):T_arrayOfString;
  VAR prevWasIdLike:boolean=false;
  FUNCTION toShorterString(CONST t:P_token):ansistring;
    VAR alternativeNode:P_variableTreeEntryNamedValue=nil;
    begin
      result:=t^.toString(prevWasIdLike,prevWasIdLike);
      if t^.tokType=tt_literal then begin
        if (parameterVar<>nil)                       then alternativeNode:=parameterVar^.findEntryForValue(t^.data);
        if (alternativeNode=nil) and (localVar<>nil) then alternativeNode:=localVar    ^.findEntryForValue(t^.data);
        if alternativeNode=nil                       then alternativeNode:=globalVar   ^.findEntryForValue(t^.data);
        if alternativeNode=nil                       then alternativeNode:=inlineVar   ^.findEntryForValue(t^.data);
        if (alternativeNode<>nil) then begin
          if (length(alternativeNode^.getIdOnly)<length(result)) then begin
            result:=alternativeNode^.getIdOnly;
            prevWasIdLike:=true;
          end;
        end else if (length(result)>20) then begin
          alternativeNode:=inlineVar^.findEntryForValue(t^.data,true);
          result:=alternativeNode^.getIdOnly;
          prevWasIdLike:=true;
        end;
      end;
    end;

  VAR i0,i:longint;
      remainingLength:longint;
      part:string;
      pt:P_token;
  begin
    result:=C_EMPTY_STRING_ARRAY;
    inlineVar^.clear;
    if topIndex>=0 then begin
      i0:=topIndex;
      remainingLength:=lengthLimit div 2;
      while (i0>0) and (remainingLength>0) do begin
        dec(remainingLength,length(toShorterString(dat[i0])));
        dec(i0);
      end;
      remainingLength:=lengthLimit-3;
      inlineVar^.clear;
      if i0>0 then result:='... ';
      prevWasIdLike:=false;
      for i:=i0 to topIndex do append(result,toShorterString(dat[i]));
    end else remainingLength:=lengthLimit-3;
    append(result,' § ');
    pt:=first;
    i:=length(result);
    while (pt<>nil) and (remainingLength>0) do begin
      part:=toShorterString(pt);
      dec(remainingLength,length(part));
      append(result,part);
      pt:=pt^.next;
    end;
    if pt<>nil then append(result,' ...');
  end;
{$endif}

CONSTRUCTOR T_idStack.create({$ifdef fullVersion}CONST info:P_localIdInfos{$endif});
  begin
    setLength(ids,0);
    {$ifdef fullVersion}
    localIdInfos:=info;
    {$endif}
  end;

DESTRUCTOR T_idStack.destroy;
  begin
    clear;
  end;

PROCEDURE T_idStack.clear;
  VAR i:longint;
  begin
    for i:=0 to length(ids)-1 do setLength(ids[i],0);
    setLength(ids,0);
  end;

PROCEDURE T_idStack.scopePush;
  begin
    setLength(ids,length(ids)+1);
  end;

PROCEDURE T_idStack.scopePop(VAR adapters:T_adapters; CONST location:T_tokenLocation);
  VAR topIdx:longint;
      i:longint;
  begin
    topIdx:=length(ids)-1;
    for i:=0 to length(ids[topIdx])-1 do begin
      if not(ids[topIdx,i].used) then adapters.raiseWarning('Unused local variable '+ids[topIdx,i].name,ids[topIdx,i].location);
      {$ifdef fullVersion}
      if localIdInfos<>nil then localIdInfos^.add(ids[topIdx,i].name,ids[topIdx,i].location,location,tt_blockLocalVariable);
      {$endif}
    end;
    setLength(ids,topIdx);
  end;

FUNCTION T_idStack.oneAboveBottom:boolean;
  begin
    result:=length(ids)=1;
  end;

FUNCTION T_idStack.scopeBottom:boolean;
  begin
    result:=length(ids)=0;
  end;

FUNCTION T_idStack.addId(CONST id:T_idString; CONST location:T_tokenLocation):boolean;
  VAR i,j:longint;
  begin
    i:=length(ids)-1;
    for j:=0 to length(ids[i])-1 do if ids[i,j].name=id then exit(false);
    j:=length(ids[i]);
    setLength(ids[i],j+1);
    ids[i,j].name:=id;
    ids[i,j].used:=false;
    ids[i,j].location:=location;
    result:=true;
  end;

FUNCTION T_idStack.hasId(CONST id:T_idString):boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=length(ids)-1 downto 0 do for j:=0 to length(ids[i])-1 do if ids[i,j].name=id then begin
      ids[i,j].used:=true;
      exit(true);
    end;
  end;

end.
