UNIT func_queues;

{$mode objfpc}{$H+}

INTERFACE
USES sysutils,Classes,
     myGenerics,
     mnh_constants,
     basicTypes,
     out_adapters,
     litVar,
     funcs,contexts,
     recyclers,
     subrules;
CONST QUEUE_TYPE_NAME='Queue';
TYPE
  P_queueEntry=^T_queueEntry;
  T_queueEntry=record
    value:P_literal;
    next:P_queueEntry;
  end;

  P_queue=^T_queue;

  { T_queue }

  T_queue=object(T_builtinGeneratorExpression)
    private
      first,last:P_queueEntry;
      closed:boolean;
      queueCs:TRTLCriticalSection;
      queuedCount:longint;
    public
      CONSTRUCTOR create(CONST location: T_tokenLocation);
      FUNCTION toString(CONST lengthLimit:longint=maxLongint):string; virtual;
      FUNCTION typeString: string; virtual;
      FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_abstractContext; CONST recycler:P_literalRecycler; CONST parameters:P_listLiteral=nil):T_evaluationResult; virtual;
      PROCEDURE cleanup(CONST literalRecycler:P_literalRecycler); virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION getBultinGeneratorType:T_builtinGeneratorType; virtual;
      PROPERTY getQueuedCount:longint read queuedCount;
  end;

IMPLEMENTATION
{$i func_defines.inc}

CONSTRUCTOR T_queue.create(CONST location: T_tokenLocation);
  begin
    inherited create(location,et_builtinIterable);
    first:=nil;
    last:=nil;
    closed:=false;
    queuedCount:=0;
    initCriticalSection(queueCs);
  end;

FUNCTION T_queue.toString(CONST lengthLimit: longint): string;
  begin
    result:='queue';
  end;

FUNCTION T_queue.typeString: string;
  begin
    result:=QUEUE_TYPE_NAME;
  end;

FUNCTION T_queue.evaluate(CONST location: T_tokenLocation; CONST context: P_abstractContext; CONST recycler: P_literalRecycler; CONST parameters: P_listLiteral): T_evaluationResult;
  VAR entry:P_queueEntry;
  begin
    //dequeue...
    enterCriticalSection(queueCs);
    while (first=nil) and not(closed) and (context^.continueEvaluation) do begin
      leaveCriticalSection(queueCs);
      sleep(2);
      enterCriticalSection(queueCs);
    end;
    if first=nil then begin
      if closed then result:=GENERATOR_END_EVAL_RESULT
      else begin
        result.reasonForStop:=rr_ok; //TODO: Timeout return code?
        result.literal:=newVoidLiteral;
      end;
    end else begin
      result.literal:=first^.value;
      result.reasonForStop:=rr_ok;
      dec(queuedCount);
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
    and (P_expressionLiteral(params^.value[0])^.typ=et_builtinIterable)
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
            inc(queuedCount);
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
    and (P_expressionLiteral(params^.value[0])^.typ=et_builtinIterable)
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

FUNCTION T_queue.getBultinGeneratorType: T_builtinGeneratorType;
  begin
    result:=bgt_queue;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'newQueue',@new_queue    ,ak_nullary);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'put',@queue_put         ,ak_variadic_1);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'closeQueue',@queue_close,ak_nullary);

end.

