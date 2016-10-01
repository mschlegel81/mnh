UNIT mnh_contexts;
INTERFACE
USES sysutils,mnh_constants,mnh_tokens,mnh_basicTypes, mnh_out_adapters,mnh_litVar;
TYPE
  T_valueStoreMarker=(vsm_none,vsm_nonBlockingVoid,vsm_blockingVoid,vsm_nonBlockingFirst,vsm_blockingFirst);
CONST
  C_voidOfBlocking:array[false..true] of T_valueStoreMarker=(vsm_nonBlockingVoid,vsm_blockingVoid);
  C_firstOfVoid   :array[vsm_nonBlockingVoid..vsm_blockingVoid] of T_valueStoreMarker=(vsm_nonBlockingFirst,vsm_blockingFirst);
TYPE
  T_valueStore=object
    cs:TRTLCriticalSection;
    data:array of record
      marker:T_valueStoreMarker;
      v:P_namedVariable;
    end;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE scopePush(CONST blocking:boolean);
    PROCEDURE scopePop;
    FUNCTION getVariable(CONST id:ansistring):P_namedVariable;
    PROCEDURE createVariable(CONST id:ansistring; CONST value:P_literal; CONST readonly:boolean);
    {$ifdef FULLVERSION}
    //For debugging:
    PROCEDURE reportVariables(VAR variableReport:T_variableReport);
    {$endif}
  end;

  P_evaluationContext=^T_evaluationContext;
  T_evaluationContext=object
    dat:array[0..2047] of P_token;
    fill:longint;
    parentContext:P_evaluationContext;
    valueStore:T_valueStore;
    adapters:P_adapters;
    allowDelegation:boolean;
    {$ifdef FULLVERSION}
    callStack:array of record
      callerLocation:T_tokenLocation;
      callee:P_objectWithIdAndLocation;
      callParameters:P_listLiteral;
    end;
    {$endif}
    CONSTRUCTOR createNormalContext(CONST outAdapters:P_adapters);
    CONSTRUCTOR createSanboxContext(CONST outAdapters:P_adapters);
    PROCEDURE adopt(CONST parent:P_evaluationContext);
    DESTRUCTOR destroy;
    //Recycler routines:
    FUNCTION disposeToken(p:P_token):P_token; inline;
    PROCEDURE cascadeDisposeToken(VAR p:P_token);
    FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer=nil):P_token; inline;
    FUNCTION newToken(CONST original:T_token):P_token; inline;
    FUNCTION newToken(CONST original:P_token):P_token; inline;
    //Local scope routines:
    FUNCTION getVariable(CONST id:ansistring):P_namedVariable;
    FUNCTION getVariableValue(CONST id:ansistring):P_literal;
    {$ifdef FULLVERSION}
    //For debugging:
    PROCEDURE callStackPush(CONST callerLocation:T_tokenLocation; CONST callee:P_objectWithIdAndLocation; CONST callParameters:P_listLiteral);
    //PROCEDURE callStackPushInEach(CONST calledFuncLoc:T_tokenLocation; CONST listElementValue:P_literal; CONST listElementName:ansistring);
    PROCEDURE callStackPop();
    PROCEDURE reportVariables(VAR variableReport:T_variableReport);
    {$endif}
  end;

IMPLEMENTATION
CONSTRUCTOR T_valueStore.create;
  begin
    system.initCriticalSection(cs);
    setLength(data,0);
  end;

DESTRUCTOR T_valueStore.destroy;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(data)-1 do if data[i].v<>nil then dispose(data[i].v,destroy);
    setLength(data,0);
    system.leaveCriticalSection(cs);
    system.doneCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePush(CONST blocking:boolean);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(data);
    setLength(data,i+1);
    with data[i] do begin
      v:=nil;
      marker:=C_voidOfBlocking[blocking];
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.scopePop;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(data);
    repeat
      dec(i);
      with data[i] do if v<>nil then begin
        dispose(v,destroy);
        v:=nil;
      end;
    until data[i].marker<>vsm_none;
    setLength(data,i);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_valueStore.getVariable(CONST id:ansistring):P_namedVariable;
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    result:=nil;
    for i:=length(data)-1 downto 0 do with data[i] do
    if (v<>nil) and (v^.getId=id) then begin
      system.leaveCriticalSection(cs);
      exit(v);
    end else if marker in [vsm_blockingFirst,vsm_blockingVoid] then begin
      system.leaveCriticalSection(cs);
      exit(nil);
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_valueStore.createVariable(CONST id:ansistring; CONST value:P_literal; CONST readonly:boolean);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    i:=length(data);
    with data[i-1] do if marker in [vsm_blockingVoid,vsm_nonBlockingVoid] then begin
      marker:=C_firstOfVoid[marker];
      new(v,create(id,value,readonly));
      system.leaveCriticalSection(cs);
      exit;
    end;
    setLength(data,i+1);
    with data[i] do begin
      marker:=vsm_none;
      new(v,create(id,value,readonly));
    end;
    system.leaveCriticalSection(cs);
  end;

{$ifdef FULLVERSION}
PROCEDURE T_valueStore.reportVariables(VAR variableReport:T_variableReport);
  VAR i :longint;
      i0:longint=0;
      up:longint=0;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(data)-1 do case data[i].marker of
      vsm_nonBlockingVoid,vsm_nonBlockingFirst: inc(up);
      vsm_blockingVoid,vsm_blockingFirst: begin up:=1; i0:=i; end;
    end;
    for i:=i0 to length(data)-1 do begin
      if data[i].marker<>vsm_none then dec(up);
      with data[i] do if v<>nil then begin
        if up=0 then variableReport.addVariable(v,'local')
                else variableReport.addVariable(v,'local (+'+intToStr(up)+')');
      end;
    end;
    system.leaveCriticalSection(cs);
  end;
{$endif}

PROCEDURE T_evaluationContext.adopt(CONST parent:P_evaluationContext);
  begin
    parentContext:=parent;
    if parent<>nil then adapters:=parent^.adapters;
  end;

CONSTRUCTOR T_evaluationContext.createNormalContext(CONST outAdapters:P_adapters);
  VAR i:longint;
  begin
    parentContext:=nil;
    adapters:=outAdapters;
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
    valueStore.create;
    allowDelegation:=true;
  end;

CONSTRUCTOR T_evaluationContext.createSanboxContext(CONST outAdapters:P_adapters);
  VAR i:longint;
  begin
    parentContext:=nil;
    adapters:=outAdapters;
    for i:=0 to length(dat)-1 do dat[i]:=nil;
    fill:=0;
    valueStore.create;
    allowDelegation:=false;
  end;


DESTRUCTOR T_evaluationContext.destroy;
  begin
    while fill>0 do begin
      dec(fill);
      try
        dispose(dat[fill],destroy);
      except
        dat[fill]:=nil;
      end;
    end;
    valueStore.destroy;
  end;

FUNCTION T_evaluationContext.disposeToken(p:P_token):P_token;
  begin
    if p=nil then exit(nil);
    result:=p^.next;
    if (fill>=length(dat))
    then dispose(p,destroy)
    else begin
      p^.undefine;
      dat[fill]:=p;
      inc(fill);
    end;
  end;

PROCEDURE T_evaluationContext.cascadeDisposeToken(VAR p:P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_evaluationContext.newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION T_evaluationContext.newToken(CONST original:T_token):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end;

FUNCTION T_evaluationContext.newToken(CONST original:P_token):P_token;
  begin
    if (fill>0) then begin
      dec(fill);
      result:=dat[fill];
    end else new(result,create);
    result^.define(original^);
    result^.next:=nil;
  end;

FUNCTION T_evaluationContext.getVariable(CONST id:ansistring):P_namedVariable;
  begin
    result:=valueStore.getVariable(id);
    if (result=nil) and (parentContext<>nil) then result:=parentContext^.getVariable(id);
  end;

FUNCTION T_evaluationContext.getVariableValue(CONST id:ansistring):P_literal;
  VAR named:P_namedVariable;
  begin
    named:=getVariable(id);
    if named=nil then result:=nil
                 else result:=named^.getValue;
  end;

{$ifdef FULLVERSION}
PROCEDURE T_evaluationContext.callStackPush(CONST callerLocation:T_tokenLocation; CONST callee:P_objectWithIdAndLocation; CONST callParameters:P_listLiteral);
  VAR i:longint;
  begin
    i:=length(callStack);
    setLength(callStack,i+1);
    callStack[i].callerLocation:=callerLocation;
    callStack[i].callee        :=callee;
    callStack[i].callParameters:=callParameters;
    callParameters^.rereference;
    stepper.steppingIn(callee^.getLocation,callee^.getId);
  end;

//PROCEDURE T_evaluationContext.callStackPushInEach(CONST calledFuncLoc:T_tokenLocation; CONST listElementValue:P_literal; CONST listElementName:ansistring);
//  VAR i:longint;
//  begin
//    i:=length(callStack);
//    setLength(callStack,i+1);
//    callStack[i].calledFuncLoc   :=calledFuncLoc;
//    callStack[i].calledFunctionId:='(p)each body';
//    callStack[i].parameters.create;
//    callStack[i].parameters.addVariable(listElementName,listElementValue,'');
//    listElementValue^.rereference;
//    stepper.steppingIn(calledFuncLoc,'(p)each body');
//  end;

PROCEDURE T_evaluationContext.callStackPop();
  begin
    if length(callStack)<=0 then exit;
    with callStack[length(callStack)-1] do begin
      stepper.steppingOut(callee^.getLocation);
      disposeLiteral(callParameters);
    end;
    setLength(callStack,length(callStack)-1);
  end;

PROCEDURE T_evaluationContext.reportVariables(VAR variableReport:T_variableReport);
  begin
    if parentContext<>nil then parentContext^.reportVariables(variableReport);
    valueStore.reportVariables(variableReport);
  end;

{$endif}
end.
