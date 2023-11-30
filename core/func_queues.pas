UNIT func_queues;

{$mode objfpc}{$H+}

INTERFACE
USES sysutils,Classes,
     mySys,myCrypto,bigint,myGenerics,
     myStringUtil,
     mnh_constants,
     basicTypes,
     out_adapters,
     litVar,
     funcs,contexts,
     listProcessing,
     recyclers,
     subrules;

IMPLEMENTATION
{$i func_defines.inc}
TYPE
  P_queueEntry=^T_queueEntry;
  T_queueEntry=record
    value:P_literal;
    next:P_queueEntry;
  end;

  P_queue=^T_queue;

  { T_queue }

  T_queue=object(T_builtinGeneratorExpression)
    first,last:P_queueEntry;
    closed:boolean;
    queueCs:TRTLCriticalSection;
    CONSTRUCTOR create(CONST location: T_tokenLocation);
    FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
    FUNCTION evaluateToLiteral({$WARN 5024 OFF}CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:pointer; CONST a:P_literal=nil; CONST b:P_literal=nil):T_evaluationResult; virtual;
    PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION writeToStream(VAR serializer:T_literalSerializer):boolean; virtual;
    FUNCTION getBultinGeneratorType:T_builtinGeneratorType; virtual;
  end;

CONSTRUCTOR T_queue.create(CONST location: T_tokenLocation);
  begin
    inherited create(location,et_builtinIteratable);
    first:=nil;
    last:=nil;
    closed:=false;
    initCriticalSection(queueCs);
  end;

FUNCTION T_queue.toString(CONST lengthLimit: longint): string;
  begin
    result:='queue';
  end;

FUNCTION T_queue.evaluateToLiteral(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: pointer; CONST a: P_literal; CONST b: P_literal): T_evaluationResult;
  VAR entry:P_queueEntry;
  begin
    //dequeue...
    enterCriticalSection(queueCs);
    while (first=nil) and not(closed) and (context^.continueEvaluation) do begin
      leaveCriticalSection(queueCs);
      sleep(10);
      enterCriticalSection(queueCs);
    end;
    if first=nil then begin
      result.reasonForStop:=rr_ok;
      result.literal:=newVoidLiteral;
    end else begin
      result.literal:=first^.value;
      result.reasonForStop:=rr_ok;
      entry:=first^.next;
      freeMem(first,sizeOf(T_queueEntry));
      first:=entry;
      if first=nil then last:=nil;
    end;
    leaveCriticalSection(queueCs);
  end;

FUNCTION queue_put intFuncSignature;
  VAR queue: P_queue;
      i:longint;
      entry:P_queueEntry;
  begin
    if (params<>nil) and (params^.size>=2)
    and (params^.value[0]^.literalType=lt_expression)
    and (P_expressionLiteral(params^.value[0])^.typ=et_builtinIteratable)
    and (P_builtinGeneratorExpression(params^.value[0])^.getBultinGeneratorType=bgt_queue) then begin
      queue:=P_queue(params^.value[0]);
      with queue^ do begin
        enterCriticalSection(queueCs);
        if closed
        then begin
          context^.raiseError('Cannot append to a closed queue.',tokenLocation);
          result:=nil;
        end else begin
          for i:=1 to params^.size-1 do begin
            getMem(entry,sizeOf(T_queueEntry));
            entry^.value:=params^.value[i]^.rereferenced;
            entry^.next:=nil;
            if first=nil
            then first     :=entry
            else last^.next:=entry;
            last:=entry;
          end;
          result:=newVoidLiteral;
        end;
        leaveCriticalSection(queueCs);
      end;
    end else result:=nil;
  end;

FUNCTION queue_close intFuncSignature;
  VAR queue: P_queue;
  begin
    if (params<>nil) and (params^.size=1)
    and (params^.value[0]^.literalType=lt_expression)
    and (P_expressionLiteral(params^.value[0])^.typ=et_builtinIteratable)
    and (P_builtinGeneratorExpression(params^.value[0])^.getBultinGeneratorType=bgt_queue) then begin
      queue:=P_queue(params^.value[0]);
      with queue^ do begin
        enterCriticalSection(queueCs);
        result:=newBoolLiteral(not(closed));
        closed:=true;
        leaveCriticalSection(queueCs);
      end;
    end else result:=nil;
  end;

FUNCTION new_queue intFuncSignature;
  VAR queue: P_queue;
  begin
    if (params=nil) or (params^.size=0)
    then begin
      new(queue,create(tokenLocation));
      result:=queue;
    end else result:=nil;
  end;

PROCEDURE T_queue.cleanup(CONST literalRecycler: P_literalRecycler);
  VAR entry:P_queueEntry;
  begin
    enterCriticalSection(queueCs);
    while first<>nil do begin
      entry:=first^.next;
      literalRecycler^.disposeLiteral(first^.value);
      freeMem(first,sizeOf(T_queueEntry));
      first:=entry;
    end;
    last:=nil;
    leaveCriticalSection(queueCs);
  end;

DESTRUCTOR T_queue.destroy;
  begin
    doneCriticalSection(queueCs);
  end;

FUNCTION T_queue.writeToStream(VAR serializer: T_literalSerializer): boolean;
  begin
    //Not serializable
    result:=false;
  end;

FUNCTION T_queue.getBultinGeneratorType: T_builtinGeneratorType;
  begin
    result:=bgt_queue;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'newQueue',@new_queue    ,ak_nullary);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'put',@queue_put         ,ak_variadic_1);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'closeQueue',@queue_close,ak_nullary);

end.

